defmodule MCP.ServerTest do
  use ExUnit.Case, async: false
  
  alias MCP.Server
  
  setup do
    # Start a fresh server for each test
    if Process.whereis(MCP.Server) do
      GenServer.stop(MCP.Server, :normal, 1000)
      :timer.sleep(10)  # Give it time to terminate
    end
    
    {:ok, server_pid} = Server.start_link(tools: [])
    
    on_exit(fn ->
      if Process.alive?(server_pid) do
        GenServer.stop(server_pid, :normal, 1000)
      end
    end)
    
    {:ok, server_pid: server_pid}
  end

  describe "server lifecycle" do
    test "starts with empty tools list", %{server_pid: _pid} do
      tools = GenServer.call(MCP.Server, :tools)
      assert tools == []
    end

    test "can register tools after startup", %{server_pid: _pid} do
      test_tool = create_test_tool("test_tool", "A test tool")
      
      Server.register(test_tool)
      
      tools = GenServer.call(MCP.Server, :tools)
      assert length(tools) == 1
      assert hd(tools).name == "test_tool"
    end

    test "can register multiple tools at once", %{server_pid: _pid} do
      tools = [
        create_test_tool("tool1", "First tool"),
        create_test_tool("tool2", "Second tool")
      ]
      
      Server.register(tools)
      
      registered_tools = GenServer.call(MCP.Server, :tools)
      assert length(registered_tools) == 2
      
      names = Enum.map(registered_tools, & &1.name)
      assert "tool1" in names
      assert "tool2" in names
    end

    test "prevents duplicate tool registration", %{server_pid: _pid} do
      tool = create_test_tool("duplicate", "Duplicate tool")
      
      Server.register(tool)
      Server.register(tool)  # Register again
      
      tools = GenServer.call(MCP.Server, :tools)
      assert length(tools) == 1
    end
  end

  describe "JSON-RPC 2.0 compliance" do
    test "ping returns proper JSON-RPC 2.0 response" do
      {:ok, response} = Server.handle_ping(123)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 123
      assert response.result == %{}
    end

    test "initialize returns proper JSON-RPC 2.0 response" do
      params = %{"protocolVersion" => "2024-11-05"}
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_initialize(456, params, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 456
      assert Map.has_key?(response, :result)
      assert response.result.protocolVersion == "2024-11-05"
    end

    test "error responses follow JSON-RPC 2.0 format" do
      message = %{"method" => "unsupported_method", "id" => 789}
      mock_state_pid = create_mock_connection()
      
      {:error, response} = Server.handle_message(message, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 789
      assert Map.has_key?(response, :error)
      assert response.error.code == -32601
      assert response.error.message == "Method not found"
    end
  end

  describe "MCP protocol compliance" do  
    test "initialize requires protocol version" do
      params = %{}
      mock_state_pid = create_mock_connection()
      
      {:error, reason} = Server.handle_initialize(1, params, mock_state_pid)
      assert reason == "Protocol version is required"
    end

    test "initialize validates protocol version" do
      params = %{"protocolVersion" => "2024-01-01"}  # Old version
      mock_state_pid = create_mock_connection()
      
      {:error, reason} = Server.handle_initialize(1, params, mock_state_pid)
      assert reason =~ "Unsupported protocol version"
    end

    test "initialize accepts current protocol version" do
      params = %{"protocolVersion" => "2024-11-05"}
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_initialize(1, params, mock_state_pid)
      assert response.result.protocolVersion == "2024-11-05"
    end

    test "initialize includes server capabilities" do
      params = %{"protocolVersion" => "2024-11-05"}
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_initialize(1, params, mock_state_pid)
      
      assert Map.has_key?(response.result, :capabilities)
      assert Map.has_key?(response.result.capabilities, :tools)
      assert response.result.capabilities.tools.listChanged == false
    end

    test "initialize includes server info" do
      params = %{"protocolVersion" => "2024-11-05"}
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_initialize(1, params, mock_state_pid)
      
      assert Map.has_key?(response.result, :serverInfo)
      assert response.result.serverInfo.name == "Tourism MCP Server"
      assert is_binary(response.result.serverInfo.version)
    end
  end

  describe "tool listing" do
    test "tools/list returns empty list when no tools registered", %{server_pid: _pid} do
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_list_tools(1, %{}, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert response.result.tools == []
    end

    test "tools/list returns registered tools", %{server_pid: _pid} do
      tool = create_test_tool("list_test", "Tool for listing test")
      Server.register(tool)
      
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_list_tools(1, %{}, mock_state_pid)
      
      assert length(response.result.tools) == 1
      tool_info = hd(response.result.tools)
      assert tool_info.name == "list_test"
      assert tool_info.description == "Tool for listing test"
      assert Map.has_key?(tool_info, :inputSchema)
      refute Map.has_key?(tool_info, :callback)  # Should not expose callback
    end
  end

  describe "tool calling" do
    test "tools/call executes tool with arity 1", %{server_pid: _pid} do
      tool = %{
        name: "echo_tool",
        description: "Echoes the input",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn args -> {:ok, "Echo: #{inspect(args)}"} end
      }
      Server.register(tool)
      
      mock_state_pid = create_mock_connection()
      params = %{"name" => "echo_tool", "arguments" => %{"message" => "hello"}}
      
      {:ok, response} = Server.handle_call_tool(1, params, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert response.result.content == [%{type: "text", text: "Echo: %{\"message\" => \"hello\"}"}]
    end

    test "tools/call handles tool errors gracefully", %{server_pid: _pid} do
      tool = %{
        name: "error_tool",
        description: "Always fails",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args -> {:error, "Tool failed"} end
      }
      Server.register(tool)
      
      mock_state_pid = create_mock_connection()
      params = %{"name" => "error_tool", "arguments" => %{}}
      
      {:ok, response} = Server.handle_call_tool(1, params, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert response.result.isError == true
      assert response.result.content == [%{type: "text", text: "Tool failed"}]
    end

    test "tools/call returns error for unknown tool", %{server_pid: _pid} do
      mock_state_pid = create_mock_connection()
      params = %{"name" => "unknown_tool", "arguments" => %{}}
      
      {:error, response} = Server.handle_call_tool(1, params, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert response.error.code == -32601
      assert response.error.message == "Method not found"
    end

    test "tools/call handles exceptions in tool execution", %{server_pid: _pid} do
      tool = %{
        name: "crash_tool",
        description: "Crashes during execution",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args -> raise "Intentional crash" end
      }
      Server.register(tool)
      
      mock_state_pid = create_mock_connection()
      message = %{
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "crash_tool", "arguments" => %{}}
      }
      
      # Test exception handling through the message router
      {:ok, response} = Server.handle_message(message, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert response.result.isError == true
      assert response.result.content |> hd() |> Map.get(:text) =~ "Failed to call tool"
    end
  end

  describe "message routing" do
    test "handles notifications/initialized" do
      message = %{"method" => "notifications/initialized"}
      mock_state_pid = create_mock_connection()
      
      {:ok, result} = Server.handle_message(message, mock_state_pid)
      assert result == nil
    end

    test "routes ping messages correctly" do
      message = %{"method" => "ping", "id" => 42}
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_message(message, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 42
      assert response.result == %{}
    end

    test "routes initialize messages correctly" do
      message = %{
        "method" => "initialize", 
        "id" => 1, 
        "params" => %{"protocolVersion" => "2024-11-05"}
      }
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_message(message, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert response.result.protocolVersion == "2024-11-05"
    end

    test "routes tools/list messages correctly" do
      message = %{"method" => "tools/list", "id" => 1}
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_message(message, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert Map.has_key?(response.result, :tools)
    end

    test "routes tools/call messages correctly", %{server_pid: _pid} do
      tool = create_test_tool("route_test", "Tool for routing test")
      Server.register(tool)
      
      message = %{
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "route_test", "arguments" => %{}}
      }
      mock_state_pid = create_mock_connection()
      
      {:ok, response} = Server.handle_message(message, mock_state_pid)
      
      assert response.jsonrpc == "2.0"
      assert response.id == 1
      assert Map.has_key?(response.result, :content)
    end
  end

  describe "async message handling" do
    test "handle_message_async returns :ok immediately" do
      message = %{"method" => "ping", "id" => 1}
      mock_state_pid = create_mock_connection()
      
      result = Server.handle_message_async(message, mock_state_pid)
      assert result == :ok
    end

    test "async tool calls are executed in separate task", %{server_pid: _pid} do
      parent = self()
      
      tool = %{
        name: "async_test",
        description: "Async test tool",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args -> 
          send(parent, {:tool_executed, self()})
          {:ok, "async result"}
        end
      }
      Server.register(tool)
      
      # Create a mock connection that forwards SSE messages to us
      mock_state_pid = spawn(fn ->
        receive do
          {:"$gen_cast", {:send_sse_message, response}} ->
            send(parent, {:sse_message_sent, response})
        end
      end)
      
      message = %{
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "async_test", "arguments" => %{}}
      }
      
      result = Server.handle_message_async(message, mock_state_pid)
      assert result == :ok
      
      # Wait for the async task to execute and send SSE response
      assert_receive {:tool_executed, task_pid}, 1000
      assert task_pid != self()  # Executed in different process
      
      assert_receive {:sse_message_sent, response}, 1000
      assert response.jsonrpc == "2.0"
      assert response.id == 1
    end
  end

  # Helper functions
  
  defp create_test_tool(name, description) do
    %{
      name: name,
      description: description,
      inputSchema: %{
        type: "object",
        properties: %{}
      },
      callback: fn _args -> {:ok, "Test result"} end
    }
  end

  defp create_mock_connection() do
    {:ok, pid} = GenServer.start_link(__MODULE__.MockConnection, {%{}, %{}})
    
    # Schedule cleanup - this will be called when the test process exits
    test_pid = self()
    spawn(fn ->
      ref = Process.monitor(test_pid)
      receive do
        {:DOWN, ^ref, :process, ^test_pid, _reason} ->
          if Process.alive?(pid) do
            GenServer.stop(pid, :normal, 1000)  
          end
      end
    end)
    
    pid
  end

  defmodule MockConnection do
    use GenServer

    def init({connect_params, assigns}) do
      Process.flag(:trap_exit, true)
      {:ok, {connect_params, assigns}}
    end

    def handle_call(:connect_params_and_assigns, _from, state) do
      {:reply, state, state}
    end

    def handle_call({:dispatch, callback, args}, _from, state) do
      try do
        result = callback.(args)
        {:reply, result, state}
      rescue
        e -> 
          {:reply, {:error, Exception.message(e)}, state}
      end
    end

    def handle_cast({:send_sse_message, _message}, state) do
      {:noreply, state}
    end

    def handle_info(_msg, state) do
      {:noreply, state}
    end

    def terminate(_reason, _state) do
      :ok
    end
  end
end