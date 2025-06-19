defmodule MCP.RouterTest do
  use ExUnit.Case, async: false
  import Plug.Test
  import Plug.Conn

  @opts MCP.Router.init([])
  
  setup do
    # Start a fresh server for each test (Registry is managed by supervisor)
    if Process.whereis(MCP.Server) do
      GenServer.stop(MCP.Server, :normal, 1000)
      :timer.sleep(10)
    end
    
    {:ok, server_pid} = MCP.Server.start_link(tools: [])
    
    # Register test tools
    register_test_tools()
    
    on_exit(fn ->
      if Process.alive?(server_pid) do
        GenServer.stop(server_pid, :normal, 1000)
      end
    end)
    
    {:ok, server_pid: server_pid}
  end

  describe "IP address validation" do
    test "rejects non-localhost connections for GET /" do
      conn = 
        conn(:get, "/")
        |> put_remote_ip({192, 168, 1, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 403
      assert conn.resp_body == "Forbidden"
      assert conn.halted
    end

    test "rejects non-localhost connections for POST /message" do
      message = %{"jsonrpc" => "2.0", "id" => 1, "method" => "ping"}
      
      conn = 
        conn(:post, "/message", Jason.encode!(message))
        |> put_req_header("content-type", "application/json")
        |> put_remote_ip({192, 168, 1, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 403
      assert conn.resp_body == "Forbidden"
      assert conn.halted
    end

    test "rejects non-localhost IPv4 addresses" do
      non_localhost_ips = [
        {192, 168, 1, 1},
        {10, 0, 0, 1},
        {172, 16, 0, 1},
        {8, 8, 8, 8}
      ]

      for ip <- non_localhost_ips do
        conn = 
          conn(:get, "/")
          |> put_remote_ip(ip)
          |> MCP.Router.call(@opts)

        assert conn.status == 403
        assert conn.resp_body == "Forbidden"
        assert conn.halted
      end
    end
  end

  describe "HTTP method handling" do
    test "POST to root returns 405 Method Not Allowed" do
      conn = 
        conn(:post, "/")
        |> put_remote_ip({127, 0, 0, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 405
      assert conn.resp_body == "Method not allowed"
      assert conn.halted
    end

    test "POST to root still checks IP first" do
      conn = 
        conn(:post, "/")
        |> put_remote_ip({192, 168, 1, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 403
      assert conn.resp_body == "Forbidden"
      assert conn.halted
    end
  end

  describe "JSON parsing" do  
    test "malformed JSON results in parsing error" do
      conn = 
        conn(:post, "/message", "{invalid json}")
        |> put_req_header("content-type", "application/json")
        |> put_remote_ip({127, 0, 0, 1})

      # Malformed JSON should result in a parsing error
      assert_raise Plug.Conn.WrapperError, fn ->
        MCP.Router.call(conn, @opts)
      end
    end
  end

  describe "IP validation logic" do
    test "is_local? function works correctly" do
      # Test via private function access
      localhost_ips = [
        {127, 0, 0, 1},
        {127, 0, 0, 255},
        {0, 0, 0, 0, 0, 0, 0, 1},  # IPv6 localhost
        {0, 0, 0, 0, 0, 65535, 32512, 1}  # IPv4-mapped IPv6 localhost
      ]

      non_localhost_ips = [
        {192, 168, 1, 1},
        {10, 0, 0, 1},
        {172, 16, 0, 1},
        {8, 8, 8, 8},
        {0, 0, 0, 0, 0, 0, 0, 2}  # Different IPv6
      ]

      # We can test this indirectly by checking if localhost IPs pass through
      # and non-localhost IPs get blocked
      for ip <- localhost_ips do
        conn = 
          conn(:post, "/")
          |> put_remote_ip(ip)
          |> MCP.Router.call(@opts)

        # Localhost should pass IP check and get 405 for POST /
        assert conn.status == 405
        assert conn.resp_body == "Method not allowed"
      end

      for ip <- non_localhost_ips do
        conn = 
          conn(:post, "/")
          |> put_remote_ip(ip)
          |> MCP.Router.call(@opts)

        # Non-localhost should be blocked with 403
        assert conn.status == 403
        assert conn.resp_body == "Forbidden"
      end
    end
  end

  describe "MCP Protocol via HTTP" do
    test "ping request returns 202 and proper response" do
      message = %{"jsonrpc" => "2.0", "method" => "ping", "id" => 42}
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "initialize request with valid protocol version" do
      message = %{
        "jsonrpc" => "2.0", 
        "method" => "initialize", 
        "id" => 1,
        "params" => %{"protocolVersion" => "2024-11-05"}
      }
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "initialize request with invalid protocol version" do
      message = %{
        "jsonrpc" => "2.0", 
        "method" => "initialize", 
        "id" => 1,
        "params" => %{"protocolVersion" => "2020-01-01"}
      }
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "initialize request without protocol version" do
      message = %{
        "jsonrpc" => "2.0", 
        "method" => "initialize", 
        "id" => 1,
        "params" => %{}
      }
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "tools/list request returns 202" do
      message = %{"jsonrpc" => "2.0", "method" => "tools/list", "id" => 1}
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "unsupported method returns 202 (error sent via SSE)" do
      message = %{"jsonrpc" => "2.0", "method" => "unsupported", "id" => 1}
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "notifications/initialized request" do
      message = %{"jsonrpc" => "2.0", "method" => "notifications/initialized"}
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end
  end

  describe "Tool Execution via HTTP" do
    test "valid tool call returns 202", %{server_pid: _pid} do
      message = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "test_tool", "arguments" => %{}}
      }
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "tool call with arguments returns 202", %{server_pid: _pid} do
      message = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{
          "name" => "echo_tool", 
          "arguments" => %{"message" => "hello world"}
        }
      }
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "unknown tool call returns 202 (error sent via SSE)", %{server_pid: _pid} do
      message = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "unknown_tool", "arguments" => %{}}
      }
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end

    test "tool that raises exception returns 202", %{server_pid: _pid} do
      message = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "crash_tool", "arguments" => %{}}
      }
      
      conn = post_mcp_message(message)
      
      assert conn.status == 202
      assert conn.halted
    end
  end

  describe "Async Behavior via HTTP" do
    test "slow tool returns 202 immediately", %{server_pid: _pid} do
      message = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "slow_tool", "arguments" => %{}}
      }
      
      start_time = System.monotonic_time(:millisecond)
      conn = post_mcp_message(message)
      end_time = System.monotonic_time(:millisecond)
      
      # Should return immediately (< 100ms), not wait for slow tool (1000ms)
      assert (end_time - start_time) < 100
      assert conn.status == 202
      assert conn.halted
    end

    test "concurrent tool calls both return 202 immediately", %{server_pid: _pid} do
      message1 = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "slow_tool", "arguments" => %{}}
      }
      
      message2 = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 2,
        "params" => %{"name" => "slow_tool", "arguments" => %{}}
      }
      
      start_time = System.monotonic_time(:millisecond)
      
      conn1 = post_mcp_message(message1)
      conn2 = post_mcp_message(message2)
      
      end_time = System.monotonic_time(:millisecond)
      
      # Both should return quickly
      assert (end_time - start_time) < 200
      assert conn1.status == 202
      assert conn2.status == 202
      assert conn1.halted
      assert conn2.halted
    end

    test "fast tool after slow tool returns 202 quickly", %{server_pid: _pid} do
      slow_message = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "slow_tool", "arguments" => %{}}
      }
      
      fast_message = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 2,
        "params" => %{"name" => "test_tool", "arguments" => %{}}
      }
      
      # Start slow tool
      conn1 = post_mcp_message(slow_message)
      assert conn1.status == 202
      
      # Immediately start fast tool
      start_time = System.monotonic_time(:millisecond)
      conn2 = post_mcp_message(fast_message)
      end_time = System.monotonic_time(:millisecond)
      
      # Fast tool should return quickly even though slow tool is running
      assert (end_time - start_time) < 50
      assert conn2.status == 202
      assert conn2.halted
    end
  end

  describe "SSE Connection Handling" do
    test "GET / from localhost passes IP validation" do
      # In test mode, we can't actually establish SSE connections,
      # but we can verify the request passes IP validation
      conn = 
        conn(:get, "/")
        |> put_remote_ip({127, 0, 0, 1})

      # Should not be blocked by IP validation but will exit in SSE setup
      # We catch the expected exit to verify IP validation passed
      result = try do
        MCP.Router.call(conn, @opts)
      catch
        :exit, :process_was_not_started_by_proc_lib ->
          # This is expected in test mode - means IP validation passed
          :ip_validation_passed
      end
      
      assert result == :ip_validation_passed
    end

    test "GET / rejects non-localhost IP addresses" do
      conn = 
        conn(:get, "/")
        |> put_remote_ip({192, 168, 1, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 403
      assert conn.resp_body == "Forbidden"
      assert conn.halted
    end
  end

  describe "Error Handling via HTTP" do
    test "malformed JSON-RPC message returns 200 with invalid message error" do
      # Missing required fields
      message = %{"method" => "ping"}  # Missing jsonrpc and id
      
      conn = post_mcp_message(message)
      
      # Should return 200 with error (invalid JSON-RPC format)
      assert conn.status == 200
      assert conn.halted
    end

    test "empty request body returns 200 with error" do
      session_id = create_test_session()
      
      conn = 
        conn(:post, "/message?sessionId=#{session_id}", "")
        |> put_req_header("content-type", "application/json")
        |> put_remote_ip({127, 0, 0, 1})
        |> MCP.Router.call(@opts)

      # Should return 200 with error for empty body
      assert conn.status == 200
      assert conn.halted
    end

    test "missing content-type header returns 200 with error" do
      session_id = create_test_session()
      message = %{"jsonrpc" => "2.0", "method" => "ping", "id" => 1}
      
      conn = 
        conn(:post, "/message?sessionId=#{session_id}", Jason.encode!(message))
        |> put_remote_ip({127, 0, 0, 1})
        |> MCP.Router.call(@opts)

      # Should return 200 with error without proper content type
      assert conn.status == 200
      assert conn.halted
    end
  end

  # Helper functions
  
  defp register_test_tools do
    tools = [
      %{
        name: "test_tool",
        description: "A simple test tool",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args -> {:ok, "Test result"} end
      },
      %{
        name: "echo_tool",
        description: "Echoes the input message",
        inputSchema: %{
          type: "object",
          properties: %{message: %{type: "string"}}
        },
        callback: fn args -> {:ok, "Echo: #{args["message"]}"} end
      },
      %{
        name: "slow_tool",
        description: "A tool that takes time to execute",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args ->
          :timer.sleep(1000)  # 1 second delay
          {:ok, "Slow tool completed"}
        end
      },
      %{
        name: "crash_tool",
        description: "A tool that always crashes",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args -> raise "Intentional crash" end
      },
      %{
        name: "error_tool",
        description: "A tool that returns errors",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args -> {:error, "Tool error"} end
      }
    ]
    
    MCP.Server.register(tools)
  end

  defp create_test_session() do
    # Generate unique session ID for this test
    session_id = "test-session-#{:rand.uniform(1000000)}"
    
    # Create a mock connection process and tell it to register itself
    {:ok, mock_conn_pid} = GenServer.start_link(__MODULE__.MockConnection, %{})
    GenServer.call(mock_conn_pid, {:register_session, session_id})
    
    # Store for cleanup
    Process.put(:test_session_id, session_id)
    Process.put(:mock_conn_pid, mock_conn_pid)
    
    session_id
  end

  defp cleanup_test_session() do
    if mock_conn_pid = Process.get(:mock_conn_pid) do
      if Process.alive?(mock_conn_pid) do
        GenServer.stop(mock_conn_pid, :normal, 1000)
      end
    end
  end

  defp post_mcp_message(message) do
    session_id = create_test_session()
    
    on_exit(fn -> cleanup_test_session() end)
    
    post_mcp_message_with_session(message, session_id)
  end

  defp post_mcp_message_with_session(message, session_id) do
    conn(:post, "/message?sessionId=#{session_id}", Jason.encode!(message))
    |> put_req_header("content-type", "application/json")
    |> put_remote_ip({127, 0, 0, 1})
    |> MCP.Router.call(@opts)
  end

  defp put_remote_ip(conn, ip) do
    %{conn | remote_ip: ip}
  end

  # Mock Connection for testing
  defmodule MockConnection do
    use GenServer

    def init(state) do
      {:ok, state}
    end

    def handle_call({:register_session, session_id}, _from, state) do
      Registry.register(MCP.Registry, session_id, [])
      {:reply, :ok, Map.put(state, :session_id, session_id)}
    end

    def handle_call(:connect_params_and_assigns, _from, state) do
      {:reply, {%{}, %{}}, state}
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

    def handle_call(:record_activity, _from, state) do
      {:reply, :ok, state}
    end

    def handle_call(:handle_initialize, _from, state) do
      {:reply, :ok, state}
    end

    def handle_call(:handle_initialized, _from, state) do
      {:reply, :ok, state}
    end

    def handle_cast({:send_sse_message, _message}, state) do
      {:noreply, state}
    end

    def handle_info(_msg, state) do
      {:noreply, state}
    end

    def terminate(_reason, %{session_id: session_id}) do
      Registry.unregister(MCP.Registry, session_id)
      :ok
    end

    def terminate(_reason, _state), do: :ok
  end
end