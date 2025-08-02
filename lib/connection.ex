defmodule MCP.Connection do
  @moduledoc false
  # Internal state management for SSE connections

  use GenServer
  require Logger

  import Plug.Conn

  # 30 seconds for initialization
  @init_timeout 30_000
  # 30 minutes in milliseconds
  @inactivity_timeout 30 * 60 * 1000
  @sse_keepalive_timeout_ms 15_000

  @protocol_version "2024-11-05"
  @vsn Mix.Project.config()[:version]

  @error_codes %{
    not_initialized: -32601,
    invalid_protocol: -32602,
    request_not_found: -32603
  }

  @impl GenServer
  def init({session_id, conn, opts}) do
    Logger.metadata(session_id: session_id, mcp: true)
    # Start initialization timeout
    Process.send_after(self(), :init_timeout, @init_timeout)
    # Start inactivity timeout
    timeout_ref = Process.send_after(self(), :inactivity_timeout, @inactivity_timeout)

    :gen_server.enter_loop(__MODULE__, [], %{
      session_id: session_id,
      conn: conn,
      state: :connected,
      init_received: false,
      initialized_received: false,
      last_activity: System.monotonic_time(:millisecond),
      sse_keepalive_timeout: @sse_keepalive_timeout_ms,
      # Add reference to the timeout timer
      timeout_ref: timeout_ref,
      requests: %{},
      init_callback: opts[:init_callback]
    })
  end

  def ready?(pid) do
    GenServer.call(pid, :ready?)
  end

  def handle_message(pid, message) do
    GenServer.cast(pid, {:handle_message, message})
  end

  @impl GenServer
  def handle_continue({:handle_initialize, id, params}, state) do
    with :ok <- validate_protocol_version(params["protocolVersion"]),
         {:ok, %{tools: tools, server_info: server_info}} <- state.init_callback.(state.session_id, params) do
      
      # Build dispatch table from tools with callbacks
      {tool_schemas, dispatch_table} = build_dispatch_table(tools)
      
      # Update state with dispatch table
      state = Map.put(state, :dispatch_table, dispatch_table)
      
      %{
        protocolVersion: @protocol_version,
        capabilities: %{
          tools: %{
            listChanged: false
          }
        },
        serverInfo: server_info,
        tools: tool_schemas
      }
      |> format_sse_response(id)
      |> handle_sse_response(state)
    else
      {:error, reason} ->
        handle_sse_error(reason, @error_codes.invalid_protocol, state, id)
    end
  end

  def handle_continue({:handle_tools_list, id}, state) do
    # Get tools from the initial callback result
    case state.init_callback.(state.session_id, %{}) do
      {:ok, %{tools: tools}} ->
        {tool_schemas, _dispatch_table} = build_dispatch_table(tools)
        
        %{tools: tool_schemas}
        |> format_sse_response(id)
        |> handle_sse_response(state)
        
      {:error, reason} ->
        handle_sse_error(reason, @error_codes.invalid_protocol, state, id)
    end
  end

  def handle_continue({:handle_tools_call, id, params}, state) do
    tool_name = params["name"]
    arguments = params["arguments"] || %{}
    
    case Map.get(state.dispatch_table, tool_name) do
      nil ->
        handle_sse_error("Tool not found: #{tool_name}", -32601, state, id)
        
      callback ->
        try do
          case callback.(arguments) do
            {:ok, result} ->
              result
              |> format_sse_response(id)
              |> handle_sse_response(state)
              
            {:error, reason} ->
              # Tool errors should be returned as successful responses with isError: true
              # per MCP specification
              result = %{
                content: [
                  %{
                    type: "text",
                    text: reason
                  }
                ],
                isError: true
              }
              
              result
              |> format_sse_response(id)
              |> handle_sse_response(state)
          end
        rescue
          error ->
            # Handle exceptions as tool errors
            result = %{
              content: [
                %{
                  type: "text",
                  text: "Tool execution failed: #{Exception.message(error)}"
                }
              ],
              isError: true
            }
            
            result
            |> format_sse_response(id)
            |> handle_sse_response(state)
        end
    end
  end

  @impl GenServer
  def handle_call(:ready?, _from, state) do
    {:reply, state.state == :ready, state}
  end

  @impl GenServer
  def handle_call({:dispatch, callback, args}, _from, state) do
    # tools that change the state are dispatched inside the Connection server
    # in order to synchronize state changes
    try do
      case callback.(args, state.assigns) do
        {:ok, result, new_assigns} ->
          {:reply, {:ok, result}, %{state | assigns: new_assigns}}

        {:ok, result, new_assigns, metadata} ->
          {:reply, {:ok, result, metadata}, %{state | assigns: new_assigns}}

        {:error, reason, new_assigns} ->
          {:reply, {:error, reason}, %{state | assigns: new_assigns}}

        other ->
          {:reply, other, state}
      end
    catch
      kind, reason ->
        {:error, "Failed to call tool: #{Exception.format(kind, reason, __STACKTRACE__)}"}
    end
  end

  @impl GenServer
  def handle_cast(
        {:handle_message, %{"method" => "initialize", "params" => params, "id" => id} = _msg},
        state
      ) do
    state =
      state
      |> record_activity()
      |> put_in([:init_received], true)

    {:noreply, state, {:continue, {:handle_initialize, id, params}}}
  end

  def handle_cast(
        {:handle_message, %{"method" => "notifications/initialized"}},
        state = %{init_received: true}
      ) do
    state =
      state
      |> record_activity()
      |> put_in([:initialized_received], true)
      |> put_in([:state], :ready)
      |> schedule_next_ping()

    {:noreply, state}
  end

  def handle_cast(
        {:handle_message, %{"method" => "notifications/initialized"}},
        state
      ) do
    handle_sse_error("Server is not initialized", @error_codes.not_initialized, state)
  end

  def handle_cast(
        {:handle_message, %{"method" => "notifications/cancelled"}},
        state
      ) do
    # Do we have to do something?
    {:noreply, record_activity(state)}
  end

  def handle_cast(
        {:handle_message, %{"method" => "ping", "id" => id}},
        state
      ) do
    %{}
    |> format_sse_response(id)
    |> handle_sse_response(state)
  end

  def handle_cast(
        {:handle_message, %{"method" => "tools/list", "id" => id}},
        state
      ) do
    {:noreply, state, {:continue, {:handle_tools_list, id}}}
  end

  def handle_cast(
        {:handle_message, %{"method" => "tools/call", "id" => id, "params" => params}},
        state
      ) do
    {:noreply, state, {:continue, {:handle_tools_call, id, params}}}
  end

  def handle_cast(
        {:handle_message, %{"id" => id, "result" => _result}},
        state = %{state: :ready}
      ) do
    # This is a ping response.
    {val, state} =
      state
      |> record_activity()
      |> pop_in([:requests, id])

    if val == true do
      {:noreply, state}
    else
      handle_sse_error("Request not found", @error_codes.request_not_found, state, id)
    end
  end

  @impl GenServer
  def handle_info(:init_timeout, %{state: :ready} = state) do
    {:noreply, state}
  end

  def handle_info(:init_timeout, %{session_id: _session_id} = state) do
    handle_close_connection(state, "Initialization timeout")
  end

  def handle_info(:inactivity_timeout, state) do
    handle_close_connection(state, "Inactivity timeout")
  end

  def handle_info(:send_ping, %{state: :ready} = state) do
    case handle_ping(state) do
      {:noreply, new_state} ->
        schedule_next_ping(new_state)
        {:noreply, new_state}

      {:stop, reason, new_state} ->
        {:stop, reason, new_state}
    end
  end

  def handle_info(:send_ping, state) do
    schedule_next_ping(state)
    {:noreply, state}
  end

  def handle_info(_message, state), do: {:noreply, state}

  defp schedule_next_ping(%{sse_keepalive_timeout: timeout}) do
    Process.send_after(self(), :send_ping, timeout)
  end

  defp record_activity(state) do
    # Cancel existing timeout
    if state.timeout_ref, do: Process.cancel_timer(state.timeout_ref)
    # Schedule new timeout
    timeout_ref = Process.send_after(self(), :inactivity_timeout, @inactivity_timeout)

    state
    |> put_in([:last_activity], System.monotonic_time(:millisecond))
    |> put_in([:timeout_ref], timeout_ref)
  end

  defp validate_protocol_version(client_version) do
    cond do
      is_nil(client_version) ->
        {:error, "Protocol version is required"}

      client_version < unquote(@protocol_version) ->
        {:error,
         "Unsupported protocol version. Server supports #{unquote(@protocol_version)} or later"}

      true ->
        :ok
    end
  end

  defp handle_ping(state) do
    id = generate_id()
    state = put_in(state, [:requests, id], true)

    id
    |> format_sse_response(id)
    |> handle_sse_response(state, "ping")
  end

  defp handle_close_connection(state, reason) do
    Logger.info("Closing SSE connection: #{inspect(reason)}")

    resp =
      %{reason: inspect(reason)}
      |> format_sse_response()
      |> handle_sse_response(state, "close")

    case resp do
      {:noreply, state} ->
        halt(state.conn)
        {:stop, reason, state}
        {:stop, reason, state}
        {:stop, reason, state}
    end
  end

  defp handle_sse_error(reason, code, state, id \\ nil, _data \\ %{}) do
    reason
    |> format_sse_error(code)
    |> format_sse_error_response(id)
    |> handle_sse_response(state)
  end

  defp handle_sse_response(message, state, event \\ "message") do
    sse_message = ["event: #{event}\ndata: ", JSON.encode_to_iodata!(message), "\n\n"]

    case chunk(state.conn, sse_message) do
      {:ok, conn} -> {:noreply, %{state | conn: conn}}
      {:error, reason} -> {:stop, {:shutdown, reason}, state}
    end
  end

  defp format_sse_response(id), do: %{jsonrpc: "2.0", id: id}

  defp format_sse_response(result, id), do: %{jsonrpc: "2.0", id: id, result: result}

  defp format_sse_error(reason, code, data \\ %{}), do: %{code: code, message: reason, data: data}

  defp format_sse_error_response(error, id)

  defp format_sse_error_response(error, nil), do: %{jsonrpc: "2.0", error: error}

  defp format_sse_error_response(error, id), do: %{jsonrpc: "2.0", id: id, error: error}

  defp generate_id(length \\ 5) do
    :crypto.strong_rand_bytes(length)
    |> Base.url_encode64(padding: false)
    |> binary_part(0, length)
  end

  defp build_dispatch_table(tools) do
    Enum.reduce(tools, {[], %{}}, fn tool, {schemas, dispatch} ->
      # Extract callback from tool if present
      {callback, tool_schema} = Map.pop(tool, :callback)
      
      # Add to schemas (without callback)
      new_schemas = [tool_schema | schemas]
      
      # Add to dispatch table if callback exists
      new_dispatch = if callback do
        Map.put(dispatch, tool_schema.name, callback)
      else
        dispatch
      end
      
      {new_schemas, new_dispatch}
    end)
    |> then(fn {schemas, dispatch} -> {Enum.reverse(schemas), dispatch} end)
  end

  # defp safe_call_tool(request_id, params, state_pid) do
  #     handle_call_tool(request_id, params, state_pid)
  #   catch
  #     kind, reason ->
  #       # tool exceptions should be treated as successful response with isError: true
  #       # https://spec.modelcontextprotocol.io/specification/2024-11-05/server/tools/#error-handling
  #       {:ok,
  #        %{
  #          jsonrpc: "2.0",
  #          id: request_id,
  #          result: %{
  #            content: [
  #              %{
  #                type: "text",
  #                text: "Failed to call tool: #{Exception.format(kind, reason, __STACKTRACE__)}"
  #              }
  #            ],
  #            isError: true
  #          }
  #        }}
  #   end
  # defp result_or_error(request_id, {:error, message}) when is_binary(message) do
  #   # tool errors should be treated as successful response with isError: true
  #   # https://spec.modelcontextprotocol.io/specification/2024-11-05/server/tools/#error-handling
  #   result_or_error(
  #     request_id,
  #     {:ok, %{content: [%{type: "text", text: message}], isError: true}}
  #   )
  # end
end
