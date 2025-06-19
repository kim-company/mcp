defmodule MCP.Server do
  @moduledoc false

  require Logger

  alias MCP.Connection

  use GenServer

  @protocol_version "2024-11-05"
  @vsn Mix.Project.config()[:version]
  @name __MODULE__

  def start_link(opts) do
    opts = Keyword.validate!(opts, tools: [])
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  def register(tool_or_tools) do
    tools = List.wrap(tool_or_tools)
    GenServer.cast(@name, {:register, tools})
  end

  def handle_ping(request_id) do
    {:ok,
     %{
       jsonrpc: "2.0",
       id: request_id,
       result: %{}
     }}
  end

  def handle_initialize(request_id, params, state_pid) do
    case validate_protocol_version(params["protocolVersion"]) do
      :ok ->
        {:ok,
         %{
           jsonrpc: "2.0",
           id: request_id,
           result: %{
             protocolVersion: @protocol_version,
             capabilities: %{
               tools: %{
                 listChanged: false
               }
             },
             serverInfo: %{
               name: "Tourism MCP Server",
               version: @vsn
             },
             tools: tools(Connection.connect_params_and_assigns(state_pid))
           }
         }}

      {:error, reason} ->
        {:error,
         %{
           jsonrpc: "2.0",
           id: request_id,
           error: %{
             code: -32602,
             message: reason
           }
         }}
    end
  end

  def handle_list_tools(request_id, _params, state_pid) do
    result_or_error(
      request_id,
      {:ok, %{tools: tools(Connection.connect_params_and_assigns(state_pid))}}
    )
  end

  def handle_call_tool(request_id, %{"name" => name} = params, state_pid) do
    args = Map.get(params, "arguments", %{})
    result_or_error(request_id, dispatch(name, args, state_pid))
  end

  ## handle_message function for SSE plug

  # Built-in message routing
  def handle_message(%{"method" => "notifications/initialized"} = message, _state_pid) do
    Logger.info("Received initialized notification")
    Logger.debug("Full message: #{inspect(message, pretty: true)}")
    {:ok, nil}
  end

  def handle_message(%{"method" => method, "id" => id} = message, state_pid) do
    Logger.info("Routing MCP message - Method: #{method}, ID: #{id}")
    Logger.debug("Full message: #{inspect(message, pretty: true)}")

    case method do
      "ping" ->
        Logger.debug("Handling ping request")
        handle_ping(id)

      "initialize" ->
        Logger.info("Handling initialize request with params: #{inspect(message["params"], pretty: true)}")

        handle_initialize(id, message["params"], state_pid)

      "tools/list" ->
        Logger.debug("Handling tools list request")
        handle_list_tools(id, message["params"], state_pid)

      "tools/call" ->
        Logger.debug("Handling tool call request with params: #{inspect(message["params"], pretty: true)}")

        safe_call_tool(id, message["params"], state_pid)

      other ->
        Logger.warning("Received unsupported method: #{other}")

        {:error,
         %{
           jsonrpc: "2.0",
           id: id,
           error: %{
             code: -32601,
             message: "Method not found",
             data: %{
               name: other
             }
           }
         }}
    end
  end

  def handle_message_async(%{"method" => method, "id" => id} = message, state_pid) do
    Logger.info("Routing MCP message async - Method: #{method}, ID: #{id}")
    Logger.debug("Full message: #{inspect(message, pretty: true)}")

    case method do
      "ping" ->
        Logger.debug("Handling ping request")
        response = handle_ping(id)
        send_async_response(response, state_pid)

      "initialize" ->
        Logger.info("Handling initialize request with params: #{inspect(message["params"], pretty: true)}")
        response = handle_initialize(id, message["params"], state_pid)
        send_async_response(response, state_pid)

      "tools/list" ->
        Logger.debug("Handling tools list request")
        response = handle_list_tools(id, message["params"], state_pid)
        send_async_response(response, state_pid)

      "tools/call" ->
        Logger.debug("Handling tool call request async with params: #{inspect(message["params"], pretty: true)}")
        
        Task.start(fn ->
          response = safe_call_tool(id, message["params"], state_pid)
          send_async_response(response, state_pid)
        end)

      other ->
        Logger.warning("Received unsupported method: #{other}")

        error_response = {:error,
         %{
           jsonrpc: "2.0",
           id: id,
           error: %{
             code: -32601,
             message: "Method not found",
             data: %{
               name: other
             }
           }
         }}
        send_async_response(error_response, state_pid)
    end
    
    :ok
  end

  @impl true
  def init(opts) do
    initial_tools = Keyword.get(opts, :tools)
    state = %{tools: [], dispatch_map: %{}}

    {:ok, state, {:continue, {:register, initial_tools}}}
  end

  @impl true
  def handle_continue({:register, initial_tools}, state) do
    {:noreply, do_register(initial_tools, state)}
  end

  @impl true
  def handle_cast({:register, tools}, state) do
    {:noreply, do_register(tools, state)}
  end

  @impl true
  def handle_call(:tools, _from, state) do
    {:reply, state.tools, state}
  end

  def handle_call(:dispatch_map, _from, state) do
    {:reply, state.dispatch_map, state}
  end

  # Private

  defp do_register(tools, state) do
    dispatch_map = Map.new(tools, fn tool -> {tool.name, tool.callback} end)

    state
    |> update_in([:dispatch_map], fn old ->
      Map.merge(old, dispatch_map)
    end)
    |> update_in([:tools], fn x ->
      x
      |> Enum.concat(tools)
      |> Enum.uniq_by(fn x -> x.name end)
    end)
  end

  @doc false
  defp tools({connect_params, _assigns}) do
    tools = GenServer.call(@name, :tools)

    listable? = fn
      %{listable: listable} when is_function(listable, 1) ->
        listable.(connect_params)

      _tool ->
        true
    end

    for tool <- tools,
        listable?.(tool) do
      tool
      |> Map.put(:description, String.trim(tool.description))
      |> Map.drop([:callback, :listable])
    end
  end

  # A callback must return either
  #
  #   * `{:ok, result}` if the callback does not receive state
  #   * `{:ok, result, new_state}` if the callback receives state (i.e. if it is of arity 2)
  #   * `{:ok, result, metadata}` if the callback is of arity 1 and returns metadata (returned as `_meta`)
  #   * `{:ok, result, new_state, metadata}` if the callback is of arity 2 and returns metadata (returned as `_meta`)
  #   * `{:error, reason}` for any error
  #   * `{:error, reason, new_state}` for any error that should also update the state
  #
  defp dispatch(name, args, state_pid) do
    dispatch = GenServer.call(@name, :dispatch_map)

    case dispatch do
      %{^name => callback} when is_function(callback, 2) ->
        MCP.Connection.dispatch(state_pid, callback, args)

      %{^name => callback} when is_function(callback, 1) ->
        callback.(args)

      _ ->
        {:error,
         %{
           code: -32601,
           message: "Method not found",
           data: %{
             name: name
           }
         }}
    end
  end

  defp result_or_error(request_id, {:ok, text, metadata})
       when is_binary(text) and is_map(metadata) do
    result_or_error(request_id, {:ok, %{content: [%{type: "text", text: text}], _meta: metadata}})
  end

  defp result_or_error(request_id, {:ok, text}) when is_binary(text) do
    result_or_error(request_id, {:ok, %{content: [%{type: "text", text: text}]}})
  end

  defp result_or_error(request_id, {:ok, result}) when is_map(result) do
    {:ok,
     %{
       jsonrpc: "2.0",
       id: request_id,
       result: result
     }}
  end

  defp result_or_error(request_id, {:error, :invalid_arguments}) do
    {:error,
     %{
       jsonrpc: "2.0",
       id: request_id,
       error: %{code: -32602, message: "Invalid arguments for tool"}
     }}
  end

  defp result_or_error(request_id, {:error, message}) when is_binary(message) do
    # tool errors should be treated as successful response with isError: true
    # https://spec.modelcontextprotocol.io/specification/2024-11-05/server/tools/#error-handling
    result_or_error(
      request_id,
      {:ok, %{content: [%{type: "text", text: message}], isError: true}}
    )
  end

  defp result_or_error(request_id, {:error, error}) when is_map(error) do
    {:error,
     %{
       jsonrpc: "2.0",
       id: request_id,
       error: error
     }}
  end

  defp validate_protocol_version(client_version) do
    cond do
      is_nil(client_version) ->
        {:error, "Protocol version is required"}

      client_version < unquote(@protocol_version) ->
        {:error, "Unsupported protocol version. Server supports #{unquote(@protocol_version)} or later"}

      true ->
        :ok
    end
  end

  defp send_async_response({:ok, response}, state_pid) do
    MCP.Connection.send_sse_message(state_pid, response)
  end

  defp send_async_response({:error, error_response}, state_pid) do
    MCP.Connection.send_sse_message(state_pid, error_response)
  end

  defp safe_call_tool(request_id, params, state_pid) do
    handle_call_tool(request_id, params, state_pid)
  catch
    kind, reason ->
      # tool exceptions should be treated as successful response with isError: true
      # https://spec.modelcontextprotocol.io/specification/2024-11-05/server/tools/#error-handling
      {:ok,
       %{
         jsonrpc: "2.0",
         id: request_id,
         result: %{
           content: [
             %{
               type: "text",
               text: "Failed to call tool: #{Exception.format(kind, reason, __STACKTRACE__)}"
             }
           ],
           isError: true
         }
       }}
  end
end
