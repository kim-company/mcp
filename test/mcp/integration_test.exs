defmodule MCP.IntegrationTest do
  use ExUnit.Case, async: true

  @protocol_version "2024-11-05"

  setup_all do
    # Start the dispatcher
    start_link_supervised!(MCP.Test.Dispatcher)
    
    # Create init_callback that uses the dispatcher
    init_callback = fn session_id, init_params ->
      MCP.Test.Dispatcher.dispatch_callback(session_id, init_params)
    end
    
    # Start the server once with the dispatcher-based callback
    pid = start_link_supervised!({Bandit, [plug: {MCP.Router, [init_callback: init_callback]}, port: 0]})
    {:ok, {_ip, port}} = ThousandIsland.listener_info(pid)
    req = Req.new(retry: false, base_url: "http://localhost:#{port}")
    %{sse_req: req}
  end

  def init_session_with_tools(req, tools) do
    init_session_with_config(req, tools, %{name: "test-server"})
  end

  def init_session_with_config(req, tools, server_info) do
    # Start the session to get the session_id
    {session_req, sse_resp} = init_session(req)
    
    # Extract session_id from the session_req URL
    session_id = session_req.url
    |> to_string()
    |> URI.parse()
    |> Map.get(:query)
    |> URI.decode_query()
    |> Map.get("sessionId")
    
    # Register the callback with tools and server_info for this session_id
    callback_fn = fn _session_id, _init_params ->
      {:ok, %{server_info: server_info, tools: tools}}
    end
    MCP.Test.Dispatcher.register_callback(session_id, callback_fn)
    
    {session_req, sse_resp}
  end


  test "405 on invalid request", %{sse_req: req} do
    resp = Req.post!(req, url: "/")
    assert resp.status == 405
  end

  test "session is correctly initialized", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session(sse_req)
    assert sse_resp.status == 200
    url = to_string(session_req.url)
    assert String.starts_with?(url, "http")
    assert String.contains?(url, "message?sessionId")
  end

  test "fails with invalid jsonrpc messages", %{sse_req: sse_req} do
    {session_req, _sse_resp} = init_session(sse_req)
    resp = Req.post!(session_req, json: %{event: :invalid})

    assert resp.status == 200

    assert resp.body == %{
             "error" => %{"code" => -32600, "message" => "Could not parse message"},
             "id" => nil,
             "jsonrpc" => "2.0"
           }
  end

  test "mcp initialization", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session(sse_req)
    resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "initialize",
          jsonrpc: "2.0",
          params: %{
            protocolVersion: @protocol_version
          }
        }
      )

    assert resp.status == 202
    {:ok, %{event: "message", data: data}} = receive_response_event(sse_resp)
    data = JSON.decode!(data)
    assert data["result"]["protocolVersion"] == @protocol_version
  end

  test "mcp initialization with custom tools", %{sse_req: sse_req} do
    # Define some test tools
    tools = [
      %{
        name: "list_activities",
        description: "Returns all activities from categories",
        inputSchema: %{
          type: "object",
          required: ["is_today"],
          properties: %{
            is_today: %{
              type: "boolean",
              default: true,
              description: "Whether the activity is scheduled for today"
            }
          }
        }
      }
    ]

    # Use the new helper function to initialize session with tools
    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)
    
    resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "initialize",
          jsonrpc: "2.0",
          params: %{
            protocolVersion: @protocol_version
          }
        }
      )

    assert resp.status == 202
    {:ok, %{event: "message", data: data}} = receive_response_event(sse_resp)
    data = JSON.decode!(data)
    
    # Verify the tools are included in the response
    assert data["result"]["protocolVersion"] == @protocol_version
    assert length(data["result"]["tools"]) == 1
    assert hd(data["result"]["tools"])["name"] == "list_activities"
  end

  test "mcp initialization with custom server info and tools", %{sse_req: sse_req} do
    # Define custom server info and tools
    server_info = %{
      name: "custom-activity-server",
      version: "1.2.3"
    }

    tools = [%{name: "get_activity", description: "Returns activity details"}]

    # Use the new helper function to initialize session with custom config
    {session_req, sse_resp} = init_session_with_config(sse_req, tools, server_info)
    
    resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "initialize",
          jsonrpc: "2.0",
          params: %{
            protocolVersion: @protocol_version
          }
        }
      )

    assert resp.status == 202
    {:ok, %{event: "message", data: data}} = receive_response_event(sse_resp)
    data = JSON.decode!(data)
    
    # Verify the response contains our custom server info and tools
    assert data["result"]["serverInfo"] == %{"name" => "custom-activity-server", "version" => "1.2.3"}
    assert data["result"]["tools"] == [%{"name" => "get_activity", "description" => "Returns activity details"}]
  end

  test "complete tool calling flow: initialization, tools/list, and tools/call", %{sse_req: sse_req} do
    # Define test tools with detailed schemas and callbacks
    tools = [
      %{
        name: "echo_message",
        description: "Echoes back the provided message with a prefix",
        inputSchema: %{
          type: "object",
          required: ["message"],
          properties: %{
            message: %{
              type: "string",
              description: "The message to echo back"
            },
            prefix: %{
              type: "string",
              description: "Optional prefix to add to the message",
              default: "Echo:"
            }
          }
        },
        callback: fn arguments ->
          message = arguments["message"] || ""
          prefix = arguments["prefix"] || "Echo:"
          
          {:ok, %{
            content: [
              %{
                type: "text",
                text: "#{prefix} #{message}"
              }
            ]
          }}
        end
      },
      %{
        name: "calculate_sum",
        description: "Calculates the sum of two numbers",
        inputSchema: %{
          type: "object",
          required: ["a", "b"],
          properties: %{
            a: %{type: "number", description: "First number"},
            b: %{type: "number", description: "Second number"}
          }
        },
        callback: fn arguments ->
          a = arguments["a"] || 0
          b = arguments["b"] || 0
          sum = a + b
          
          {:ok, %{
            content: [
              %{
                type: "text", 
                text: "The sum of #{a} and #{b} is #{sum}"
              }
            ]
          }}
        end
      }
    ]

    # Initialize session with tools
    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)
    
    # Step 1: Initialize the MCP connection
    init_resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "initialize",
          jsonrpc: "2.0",
          params: %{
            protocolVersion: @protocol_version
          }
        }
      )

    assert init_resp.status == 202
    {:ok, %{event: "message", data: init_data}} = receive_response_event(sse_resp)
    init_data = JSON.decode!(init_data)
    
    # Verify initialization response contains tools
    assert init_data["result"]["protocolVersion"] == @protocol_version
    assert length(init_data["result"]["tools"]) == 2
    tool_names = Enum.map(init_data["result"]["tools"], & &1["name"])
    assert "echo_message" in tool_names
    assert "calculate_sum" in tool_names

    # Step 2: List available tools using tools/list
    list_resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "tools/list",
          jsonrpc: "2.0",
          params: %{}
        }
      )

    assert list_resp.status == 202
    {:ok, %{event: "message", data: list_data}} = receive_response_event(sse_resp)
    list_data = JSON.decode!(list_data)
    
    # Verify tools/list response
    assert is_list(list_data["result"]["tools"])
    assert length(list_data["result"]["tools"]) == 2
    
    # Find the echo_message tool and verify its schema
    echo_tool = Enum.find(list_data["result"]["tools"], & &1["name"] == "echo_message")
    assert echo_tool["description"] == "Echoes back the provided message with a prefix"
    assert echo_tool["inputSchema"]["type"] == "object"
    assert echo_tool["inputSchema"]["required"] == ["message"]

    # Step 3: Call the echo_message tool
    call_resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "tools/call",
          jsonrpc: "2.0",
          params: %{
            name: "echo_message",
            arguments: %{
              message: "Hello, MCP!",
              prefix: "Test:"
            }
          }
        }
      )

    assert call_resp.status == 202
    {:ok, %{event: "message", data: call_data}} = receive_response_event(sse_resp)
    call_data = JSON.decode!(call_data)
    
    # Verify tools/call response
    assert call_data["result"]["content"] != nil
    assert is_list(call_data["result"]["content"])
    assert length(call_data["result"]["content"]) == 1
    
    content = hd(call_data["result"]["content"])
    assert content["type"] == "text"
    assert content["text"] == "Test: Hello, MCP!"
    
    # Step 4: Call the calculate_sum tool
    calc_resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "tools/call",
          jsonrpc: "2.0",
          params: %{
            name: "calculate_sum",
            arguments: %{
              a: 10,
              b: 5
            }
          }
        }
      )

    assert calc_resp.status == 202
    {:ok, %{event: "message", data: calc_data}} = receive_response_event(sse_resp)
    calc_data = JSON.decode!(calc_data)
    
    # Verify tools/call response for calculation
    assert calc_data["result"]["content"] != nil
    assert is_list(calc_data["result"]["content"])
    assert length(calc_data["result"]["content"]) == 1
    
    calc_content = hd(calc_data["result"]["content"])
    assert calc_content["type"] == "text"
    assert calc_content["text"] == "The sum of 10 and 5 is 15"
    
    # Step 5: Test tool not found error
    error_resp =
      Req.post!(session_req,
        json: %{
          id: id(),
          method: "tools/call",
          jsonrpc: "2.0",
          params: %{
            name: "nonexistent_tool",
            arguments: %{}
          }
        }
      )

    assert error_resp.status == 202
    {:ok, %{event: "message", data: error_data}} = receive_response_event(sse_resp)
    error_data = JSON.decode!(error_data)
    
    # Verify error response
    assert error_data["error"] != nil
    assert error_data["error"]["code"] == -32601
    assert String.contains?(error_data["error"]["message"], "Tool not found: nonexistent_tool")
  end

  defp init_session(req) do
    resp = Req.get!(req, url: "/", into: :self)
    {:ok, %{event: "endpoint", data: uri}} = receive_response_event(resp)

    {Req.new(url: uri, retry: false), resp}
  end

  defp receive_response_event(resp) do
    resp =
      Req.parse_message(
        resp,
        receive do
          message -> message
        end
      )

    case resp do
      {:ok, [data: data]} ->
        %{"event" => event, "data" => data} =
          Regex.named_captures(~r/event: (?<event>\w+)\ndata: (?<data>.+)\n\n/, data)

        {:ok, %{event: event, data: data}}

      other ->
        other
    end
  end

  defp id(length \\ 5) do
    :crypto.strong_rand_bytes(length)
    |> Base.url_encode64(padding: false)
    |> binary_part(0, length)
  end
end
