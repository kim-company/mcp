defmodule MCP.AsyncTest do
  use ExUnit.Case, async: false
  
  alias MCP.Server
  
  setup do
    # Start a fresh server for each test
    if Process.whereis(MCP.Server) do
      GenServer.stop(MCP.Server, :normal, 1000)
      :timer.sleep(10)
    end
    
    {:ok, server_pid} = Server.start_link(tools: [])
    
    on_exit(fn ->
      if Process.alive?(server_pid) do
        GenServer.stop(server_pid, :normal, 1000)
      end
    end)
    
    {:ok, server_pid: server_pid}
  end

  describe "async tool execution" do
    test "slow tool should not block HTTP responses", %{server_pid: _pid} do
      parent = self()
      
      # Register slow and fast tools
      slow_tool = %{
        name: "slow_tool",
        description: "A tool that takes time to execute",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args ->
          send(parent, {:slow_tool_started, self()})
          :timer.sleep(500)  # 500ms delay
          send(parent, {:slow_tool_finished, self()})
          {:ok, "Slow tool completed!"}
        end
      }
      
      fast_tool = %{
        name: "fast_tool", 
        description: "A tool that executes immediately",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args ->
          send(parent, {:fast_tool_executed, self()})
          {:ok, "Fast tool completed instantly!"}
        end
      }
      
      Server.register([slow_tool, fast_tool])
      
      # Create mock connection that captures SSE messages
      responses = []
      mock_state_pid = spawn(fn ->
        collect_responses(parent, responses)
      end)
      
      # Start slow tool execution async
      slow_message = %{
        "method" => "tools/call",
        "id" => 1,
        "params" => %{"name" => "slow_tool", "arguments" => %{}}
      }
      
      result1 = Server.handle_message_async(slow_message, mock_state_pid)
      assert result1 == :ok
      
      # Verify slow tool started but not finished yet
      assert_receive {:slow_tool_started, _pid}, 100
      
      # Immediately call fast tool while slow tool is running
      fast_message = %{
        "method" => "tools/call",
        "id" => 2,
        "params" => %{"name" => "fast_tool", "arguments" => %{}}
      }
      
      result2 = Server.handle_message_async(fast_message, mock_state_pid)
      assert result2 == :ok
      
      # Fast tool should complete before slow tool
      assert_receive {:fast_tool_executed, _pid}, 100
      assert_receive {:slow_tool_finished, _pid}, 600
      
      # Both should send SSE responses
      assert_receive {:sse_response, response1}, 100
      assert_receive {:sse_response, response2}, 600
      
      # Verify responses have correct IDs
      response_ids = [response1.id, response2.id]
      assert 1 in response_ids
      assert 2 in response_ids
    end

    test "multiple tools can execute concurrently", %{server_pid: _pid} do
      parent = self()
      
      # Register multiple concurrent tools
      tools = for i <- 1..3 do
        %{
          name: "concurrent_tool_#{i}",
          description: "Concurrent tool #{i}",
          inputSchema: %{type: "object", properties: %{}},
          callback: fn _args ->
            send(parent, {:tool_started, i, self()})
            :timer.sleep(200)  # Small delay
            send(parent, {:tool_finished, i, self()})
            {:ok, "Tool #{i} completed"}
          end
        }
      end
      
      Server.register(tools)
      
      mock_state_pid = spawn(fn ->
        collect_responses(parent, [])
      end)
      
      # Start all tools concurrently
      start_time = System.monotonic_time(:millisecond)
      
      for i <- 1..3 do
        message = %{
          "method" => "tools/call",
          "id" => i,
          "params" => %{"name" => "concurrent_tool_#{i}", "arguments" => %{}}
        }
        result = Server.handle_message_async(message, mock_state_pid)
        assert result == :ok
      end
      
      # All tools should start quickly
      for i <- 1..3 do
        assert_receive {:tool_started, ^i, _pid}, 100
      end
      
      # All tools should finish around the same time
      for i <- 1..3 do
        assert_receive {:tool_finished, ^i, _pid}, 300
      end
      
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time
      
      # Should complete in ~200ms (not 600ms if sequential)
      assert total_time < 400
      
      # All should send responses
      for _i <- 1..3 do
        assert_receive {:sse_response, _response}, 100
      end
    end

    test "async message handling maintains JSON-RPC compliance", %{server_pid: _pid} do
      parent = self()
      
      test_tool = %{
        name: "compliance_tool",
        description: "Tool for testing compliance",
        inputSchema: %{
          type: "object",
          properties: %{message: %{type: "string"}}
        },
        callback: fn args ->
          {:ok, "Received: #{args["message"]}"}
        end
      }
      
      Server.register(test_tool)
      
      mock_state_pid = spawn(fn ->
        receive do
          {:"$gen_cast", {:send_sse_message, response}} ->
            send(parent, {:sse_response, response})
        end
      end)
      
      message = %{
        "method" => "tools/call",
        "id" => 42,
        "params" => %{
          "name" => "compliance_tool", 
          "arguments" => %{"message" => "hello world"}
        }
      }
      
      result = Server.handle_message_async(message, mock_state_pid)
      assert result == :ok
      
      assert_receive {:sse_response, response}, 1000
      
      # Verify JSON-RPC 2.0 compliance
      assert response.jsonrpc == "2.0"
      assert response.id == 42
      assert Map.has_key?(response, :result)
      assert response.result.content == [%{type: "text", text: "Received: hello world"}]
    end

    test "async error handling works correctly", %{server_pid: _pid} do
      parent = self()
      
      error_tool = %{
        name: "error_tool",
        description: "Tool that always fails",
        inputSchema: %{type: "object", properties: %{}},
        callback: fn _args ->
          {:error, "This tool always fails"}
        end
      }
      
      Server.register(error_tool)
      
      mock_state_pid = spawn(fn ->
        receive do
          {:"$gen_cast", {:send_sse_message, response}} ->
            send(parent, {:sse_response, response})
        end
      end)
      
      message = %{
        "method" => "tools/call",
        "id" => 99,
        "params" => %{"name" => "error_tool", "arguments" => %{}}
      }
      
      result = Server.handle_message_async(message, mock_state_pid)
      assert result == :ok
      
      assert_receive {:sse_response, response}, 1000
      
      # Error should be returned as successful response with isError: true
      assert response.jsonrpc == "2.0"
      assert response.id == 99
      assert response.result.isError == true
      assert response.result.content == [%{type: "text", text: "This tool always fails"}]
    end
  end

  # Helper function to collect SSE responses
  defp collect_responses(parent, acc) do
    receive do
      {:"$gen_cast", {:send_sse_message, response}} ->
        send(parent, {:sse_response, response})
        collect_responses(parent, [response | acc])
      after 1000 ->
        acc
    end
  end
end