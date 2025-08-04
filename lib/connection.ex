defmodule MCP.Connection do
  @moduledoc false
  # Internal state management for SSE connections

  use GenServer
  require Logger

  alias MCP.Connection.ToolParseException

  @type init_callback_result ::
          {:ok, %{tools: list(tool_spec()), server_info: map()}} | {:error, String.t()}

  @type tool_spec :: %{
          spec: map(),
          callback: (map() -> {:ok, any()} | {:error, String.t()}) | nil
        }

  import Plug.Conn

  # 30 seconds for initialization
  @init_timeout 30_000
  # 30 minutes in milliseconds
  @inactivity_timeout 30 * 60 * 1000
  @sse_keepalive_timeout_ms 15_000

  @protocol_version "2024-11-05"
  @vsn Mix.Project.config()[:version]

  @error_codes %{
    parse_error: -32700,
    invalid_request: -32600,
    method_not_found: -32601,
    invalid_params: -32602,
    internal_error: -32603,
    not_initialized: -32001,
    invalid_protocol: -32002,
    request_not_found: -32003
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
      init_callback: opts[:init_callback],
      # Track pending async tool tasks: %{task_ref => request_id}
      pending_tasks: %{}
    })
  end

  @doc """
  Checks if a connection is ready to receive MCP requests.

  A connection is considered ready after the client has sent both the `initialize` 
  request and the `notifications/initialized` notification.

  ## Parameters

  * `pid` - The GenServer process ID of the connection

  ## Returns

  * `true` - If the connection is ready for tool calls
  * `false` - If the connection is still initializing
  """
  @spec ready?(pid()) :: boolean()
  def ready?(pid) do
    GenServer.call(pid, :ready?)
  end

  @doc """
  Sends a JSON-RPC message to a connection for processing.

  This function is used internally by the SSE handler to route incoming
  messages to the appropriate connection process.

  ## Parameters

  * `pid` - The GenServer process ID of the connection
  * `message` - A JSON-RPC message map conforming to the MCP specification

  ## Returns

  * `:ok` - The message was successfully queued for processing
  """
  @spec handle_message(pid(), map()) :: :ok
  def handle_message(pid, message) do
    GenServer.cast(pid, {:handle_message, message})
  end

  @impl GenServer
  def handle_continue({:handle_initialize, id, params}, state) do
    with :ok <- validate_protocol_version(params["protocolVersion"]),
         {:ok, %{tools: tools, server_info: server_info}} <-
           state.init_callback.(state.session_id, params),
         {:ok, {tool_schemas, dispatch_table}} <- build_dispatch_table(tools) do
      # Update state with dispatch table and tool schemas
      state =
        state
        |> Map.put(:dispatch_table, dispatch_table)
        |> Map.put(:tool_schemas, tool_schemas)

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
        handle_sse_error(reason, @error_codes.invalid_params, state, id)
    end
  end

  def handle_continue({:handle_tools_list, id}, state) do
    # Use the tool schemas from the dispatch table built during initialization
    case Map.get(state, :tool_schemas) do
      nil ->
        # If no tool schemas in state, server not properly initialized
        handle_sse_error("Server not initialized", @error_codes.not_initialized, state, id)

      tool_schemas ->
        %{tools: tool_schemas}
        |> format_sse_response(id)
        |> handle_sse_response(state)
    end
  end

  def handle_continue({:handle_tools_call, id, params}, state) do
    tool_name = params["name"]
    arguments = params["arguments"] || %{}

    dispatch_table = Map.get(state, :dispatch_table, %{})

    case Map.get(dispatch_table, tool_name) do
      nil ->
        handle_sse_error("Tool not found: #{tool_name}", @error_codes.method_not_found, state, id)

      callback ->
        # Start async task for tool execution
        task =
          Task.Supervisor.async_nolink(MCP.ToolCallSupervisor, fn ->
            callback.(arguments)
          end)

        # Track the task reference with its request ID
        new_pending_tasks = Map.put(state.pending_tasks, task.ref, id)
        {:noreply, %{state | pending_tasks: new_pending_tasks}}
    end
  end

  @impl GenServer
  def handle_call(:ready?, _from, state) do
    {:reply, state.state == :ready, state}
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

  # Handle unknown methods - must come after all specific method handlers
  def handle_cast(
        {:handle_message, %{"method" => _method, "id" => id} = _msg},
        state
      ) do
    handle_sse_error("Method not found", @error_codes.method_not_found, state, id)
  end

  # Handle unknown methods without ID (notifications)
  def handle_cast(
        {:handle_message, %{"method" => _method} = _msg},
        state
      ) do
    # Notifications don't get responses, just ignore unknown ones
    {:noreply, record_activity(state)}
  end

  # Handle invalid JSON-RPC messages (missing method, malformed, etc.)
  def handle_cast({:handle_message, _msg}, state) do
    handle_sse_error("Could not parse message", @error_codes.invalid_request, state)
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

  # Handle successful task completion
  def handle_info({ref, result}, state) when is_reference(ref) do
    Process.demonitor(ref, [:flush])
    handle_task_completion(ref, result, state)
  end

  # Handle task failure
  def handle_info({:DOWN, ref, :process, _pid, reason}, state) when is_reference(ref) do
    error_reason = "Tool execution failed: #{inspect(reason)}"
    handle_task_completion(ref, {:error, error_reason}, state)
  end

  def handle_info(_message, state), do: {:noreply, state}

  # Helper function to handle task completion by reference
  defp handle_task_completion(ref, result, %{pending_tasks: pending_tasks} = state) do
    case Map.pop(pending_tasks, ref) do
      {nil, _} ->
        # Task reference not found, ignore
        {:noreply, state}

      {request_id, remaining_tasks} ->
        handle_task_result(result, request_id, %{state | pending_tasks: remaining_tasks})
    end
  end

  # Helper function to handle task results
  defp handle_task_result(result, request_id, state) do
    case result do
      {:ok, tool_result} ->
        tool_result
        |> format_sse_response(request_id)
        |> handle_sse_response(state)

      {:error, reason} ->
        # Tool errors should be returned as successful responses with isError: true
        # per MCP specification
        error_result = %{
          content: [
            %{
              type: "text",
              text: reason
            }
          ],
          isError: true
        }

        error_result
        |> format_sse_response(request_id)
        |> handle_sse_response(state)
    end
  end

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

      not valid_version_format?(client_version) ->
        {:error, "Invalid protocol version format. Expected YYYY-MM-DD format"}

      client_version < unquote(@protocol_version) ->
        {:error,
         "Unsupported protocol version. Server supports #{unquote(@protocol_version)} or later"}

      true ->
        :ok
    end
  end

  defp valid_version_format?(version) when is_binary(version) do
    case Regex.match?(~r/^\d{4}-\d{2}-\d{2}$/, version) do
      true -> true
      false -> false
    end
  end

  defp valid_version_format?(_), do: false

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

  @doc """
  Validates a tool specification to ensure it conforms to the expected internal format.

  This function performs comprehensive validation of tool specifications, checking:

  - Tool spec has required :spec and :callback fields
  - MCP spec has required "name" and "inputSchema" fields (string keys)
  - Field types are correct (name must be non-empty string)
  - Input schema is a valid JSON Schema map with "type" field
  - Callback is either a function/1 or nil

  ## Parameters

  * `tool_spec` - A map containing the tool specification with :spec and :callback fields

  ## Returns

  * `:ok` - If the tool specification is valid
  * `{:error, reason}` - If validation fails, with a descriptive error message

  ## Examples

      iex> MCP.Connection.validate_tool_spec(%{
      ...>   spec: %{
      ...>     "name" => "test_tool",
      ...>     "description" => "A test tool",
      ...>     "inputSchema" => %{"type" => "object"}
      ...>   },
      ...>   callback: fn _args -> {:ok, %{}} end
      ...> })
      :ok

      iex> MCP.Connection.validate_tool_spec(%{spec: %{"name" => ""}})
      {:error, "Tool name must be a non-empty string"}
  """
  @spec validate_tool_spec(map()) :: :ok | {:error, String.t()}
  def validate_tool_spec(tool_spec) when is_map(tool_spec) do
    try do
      %{}
      |> parse_tool_spec_structure(tool_spec)
      |> parse_mcp_spec_name(tool_spec.spec)
      |> parse_mcp_spec_input_schema(tool_spec.spec)
      |> parse_tool_callback(tool_spec)

      :ok
    rescue
      e in ToolParseException ->
        {:error, Exception.message(e)}
    end
  end

  def validate_tool_spec(_), do: {:error, "Tool specification must be a map"}

  defp parse_tool_spec_structure(parsed_tool, tool_spec) do
    cond do
      not Map.has_key?(tool_spec, :spec) ->
        raise ToolParseException, "Tool specification must include a :spec field"

      not is_map(tool_spec.spec) ->
        raise ToolParseException, "Tool :spec field must be a map"

      true ->
        parsed_tool
    end
  end

  defp parse_mcp_spec_name(parsed_tool, mcp_spec) do
    name = mcp_spec["name"]

    cond do
      is_nil(name) ->
        raise ToolParseException, "MCP tool specification must include a 'name' field"

      not is_binary(name) ->
        raise ToolParseException, "Tool name must be a non-empty string"

      name == "" ->
        raise ToolParseException, "Tool name must be a non-empty string"

      true ->
        parsed_tool
    end
  end

  defp parse_mcp_spec_input_schema(parsed_tool, mcp_spec) do
    schema = mcp_spec["inputSchema"]

    cond do
      is_nil(schema) ->
        raise ToolParseException, "MCP tool specification must include an 'inputSchema' field"

      not is_map(schema) ->
        raise ToolParseException, "Tool inputSchema must be a map"

      true ->
        case validate_mcp_input_schema(schema) do
          :ok ->
            parsed_tool

          {:error, reason} ->
            raise ToolParseException, "Invalid inputSchema: #{reason}"
        end
    end
  end

  defp parse_tool_callback(parsed_tool, tool_spec) do
    callback = tool_spec[:callback]

    cond do
      is_nil(callback) ->
        parsed_tool

      is_function(callback, 1) ->
        parsed_tool

      true ->
        raise ToolParseException, "Tool callback must be a function/1 or nil"
    end
  end

  defp validate_mcp_input_schema(schema) when is_map(schema) do
    # Validate MCP input schema - must have "type" field
    case Map.get(schema, "type") do
      nil ->
        {:error, "inputSchema must have a 'type' field"}

      type when is_binary(type) ->
        # Additional validation for object type with properties
        case type do
          "object" ->
            validate_object_schema_properties(schema)

          _ ->
            :ok
        end

      _ ->
        {:error, "inputSchema 'type' must be a string"}
    end
  end

  defp validate_mcp_input_schema(_), do: {:error, "inputSchema must be a map"}

  defp validate_object_schema_properties(schema) do
    case Map.get(schema, "properties") do
      nil ->
        :ok

      props when is_map(props) ->
        :ok

      _ ->
        {:error, "inputSchema 'properties' must be a map"}
    end
  end

  @doc """
  Converts an internal tool specification to the MCP protocol format.

  Simply extracts the spec field since it already contains the MCP-compliant format.
  """
  @spec marshal_tool_spec(tool_spec()) :: map()
  def marshal_tool_spec(tool_spec) do
    tool_spec.spec
  end

  defp build_dispatch_table(tools) do
    validate_and_build_tools(tools, {[], %{}})
  end

  defp validate_and_build_tools([], {schemas, dispatch}) do
    {:ok, {Enum.reverse(schemas), dispatch}}
  end

  defp validate_and_build_tools([tool | rest], {schemas, dispatch}) do
    case validate_tool_spec(tool) do
      :ok ->
        # Extract callback from tool
        callback = tool[:callback]

        # Marshal tool to MCP format (without callback)
        tool_schema = marshal_tool_spec(tool)

        # Add to schemas
        new_schemas = [tool_schema | schemas]

        # Add to dispatch table if callback exists
        new_dispatch =
          if callback do
            Map.put(dispatch, tool.spec["name"], callback)
          else
            dispatch
          end

        validate_and_build_tools(rest, {new_schemas, new_dispatch})

      {:error, reason} ->
        {:error, "Invalid tool specification: #{reason}"}
    end
  end
end
