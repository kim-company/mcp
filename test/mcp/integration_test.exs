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
    pid =
      start_link_supervised!(
        {Bandit, [plug: {MCP.Router, [init_callback: init_callback]}, port: 0]}
      )

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
    session_id =
      session_req.url
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
        spec: %{
          "name" => "list_activities",
          "description" => "Returns all activities from categories",
          "inputSchema" => %{
            "type" => "object",
            "required" => ["is_today"],
            "properties" => %{
              "is_today" => %{
                "type" => "boolean",
                "default" => true,
                "description" => "Whether the activity is scheduled for today"
              }
            }
          }
        },
        callback: nil
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

    tools = [
      %{
        spec: %{
          "name" => "get_activity",
          "description" => "Returns activity details",
          "inputSchema" => %{"type" => "object"}
        },
        callback: nil
      }
    ]

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
    assert data["result"]["serverInfo"] == %{
             "name" => "custom-activity-server",
             "version" => "1.2.3"
           }

    assert data["result"]["tools"] == [
             %{
               "name" => "get_activity",
               "description" => "Returns activity details",
               "inputSchema" => %{"type" => "object"}
             }
           ]
  end

  test "complete tool calling flow: initialization, tools/list, and tools/call", %{
    sse_req: sse_req
  } do
    # Define test tools with detailed schemas and callbacks
    tools = [
      %{
        spec: %{
          "name" => "echo_message",
          "description" => "Echoes back the provided message with a prefix",
          "inputSchema" => %{
            "type" => "object",
            "required" => ["message"],
            "properties" => %{
              "message" => %{
                "type" => "string",
                "description" => "The message to echo back"
              },
              "prefix" => %{
                "type" => "string",
                "description" => "Optional prefix to add to the message",
                "default" => "Echo:"
              }
            }
          }
        },
        callback: fn arguments ->
          message = arguments["message"] || ""
          prefix = arguments["prefix"] || "Echo:"

          {:ok,
           %{
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
        spec: %{
          "name" => "calculate_sum",
          "description" => "Calculates the sum of two numbers",
          "inputSchema" => %{
            "type" => "object",
            "required" => ["a", "b"],
            "properties" => %{
              "a" => %{"type" => "number", "description" => "First number"},
              "b" => %{"type" => "number", "description" => "Second number"}
            }
          }
        },
        callback: fn arguments ->
          a = arguments["a"] || 0
          b = arguments["b"] || 0
          sum = a + b

          {:ok,
           %{
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
    echo_tool = Enum.find(list_data["result"]["tools"], &(&1["name"] == "echo_message"))
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

  test "initialize request with missing protocol version", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session_with_tools(sse_req, [])

    response =
      send_rpc_request(session_req, "initialize", %{
        clientInfo: %{name: "test-client", version: "1.0.0"},
        capabilities: %{}
      })

    assert response.status == 202
    expect_rpc_error(sse_resp, -32602)
  end

  test "initialize request with invalid protocol version", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session_with_tools(sse_req, [])

    response =
      send_rpc_request(session_req, "initialize", %{
        protocolVersion: "invalid-version",
        clientInfo: %{name: "test-client", version: "1.0.0"},
        capabilities: %{}
      })

    assert response.status == 202
    expect_rpc_error(sse_resp, -32602)
  end

  test "initialize request with client capabilities", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session_with_tools(sse_req, [])

    response =
      send_rpc_request(session_req, "initialize", %{
        protocolVersion: @protocol_version,
        clientInfo: %{name: "advanced-client", version: "2.1.0"},
        capabilities: %{
          experimental: %{},
          sampling: %{}
        }
      })

    assert response.status == 202
    data = expect_rpc_response(sse_resp)

    assert data["result"]["protocolVersion"] == @protocol_version
    assert data["result"]["serverInfo"]["name"] == "test-server"
    assert data["result"]["capabilities"] != nil
  end

  test "tools/list with no tools available", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session_with_tools(sse_req, [])

    # Initialize first
    send_initialize_request(session_req)
    expect_rpc_response(sse_resp)

    # List tools
    response = send_tools_list_request(session_req)
    assert response.status == 202

    data = expect_rpc_response(sse_resp)
    assert data["result"]["tools"] == []
    assert data["result"]["nextCursor"] == nil
  end

  test "tools/list with pagination (cursor)", %{sse_req: sse_req} do
    tools =
      Enum.map(1..5, fn i ->
        %{
          spec: %{
            "name" => "tool_#{i}",
            "description" => "Tool number #{i}",
            "inputSchema" => %{"type" => "object", "properties" => %{}}
          },
          callback: nil
        }
      end)

    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)

    # Initialize first
    send_initialize_request(session_req)
    expect_rpc_response(sse_resp)

    # List tools with cursor
    response = send_tools_list_request(session_req, "some-cursor")
    assert response.status == 202

    data = expect_rpc_response(sse_resp)
    assert is_list(data["result"]["tools"])
    assert length(data["result"]["tools"]) == 5
  end

  test "tools/call with invalid tool name", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session_with_tools(sse_req, [])

    # Initialize first
    send_initialize_request(session_req)
    expect_rpc_response(sse_resp)

    # Call non-existent tool
    response = send_tools_call_request(session_req, "nonexistent_tool", %{})
    assert response.status == 202

    expect_rpc_error(sse_resp, -32601)
  end

  test "tools/call with missing required arguments", %{sse_req: sse_req} do
    tools = [
      %{
        spec: %{
          "name" => "require_args_tool",
          "description" => "Tool that requires arguments",
          "inputSchema" => %{
            "type" => "object",
            "required" => ["required_arg"],
            "properties" => %{
              "required_arg" => %{"type" => "string", "description" => "Required argument"}
            }
          }
        },
        callback: fn _arguments ->
          {:ok, %{content: [%{type: "text", text: "Success"}]}}
        end
      }
    ]

    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)

    # Initialize first
    send_initialize_request(session_req)
    expect_rpc_response(sse_resp)

    # Call tool without required arguments
    response = send_tools_call_request(session_req, "require_args_tool", %{})
    assert response.status == 202

    data = expect_rpc_response(sse_resp)
    # Should succeed even without validation (server implementation dependent)
    assert data["result"] != nil
  end

  test "tools/call with tool returning error", %{sse_req: sse_req} do
    tools = [
      %{
        spec: %{
          "name" => "error_tool",
          "description" => "Tool that returns an error",
          "inputSchema" => %{"type" => "object", "properties" => %{}}
        },
        callback: fn _arguments ->
          {:ok,
           %{
             content: [%{type: "text", text: "Tool execution failed"}],
             isError: true
           }}
        end
      }
    ]

    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)

    # Initialize first
    send_initialize_request(session_req)
    expect_rpc_response(sse_resp)

    # Call error tool
    response = send_tools_call_request(session_req, "error_tool", %{})
    assert response.status == 202

    data = expect_rpc_response(sse_resp)
    assert data["result"]["content"] != nil
    assert data["result"]["isError"] == true
  end

  test "JSON-RPC 2.0 compliance - request without id (notification)", %{sse_req: sse_req} do
    {session_req, _sse_resp} = init_session_with_tools(sse_req, [])

    # Send notification (no id field)
    response = send_rpc_notification(session_req, "notifications/initialized", %{})
    assert response.status == 202

    # Notifications should not generate responses
    # We can't easily test this without timing, so we'll just ensure no error
  end

  test "JSON-RPC 2.0 compliance - request with null id", %{sse_req: sse_req} do
    {session_req, _sse_resp} = init_session(sse_req)

    # Manually create the request with null ID
    json_payload = %{
      jsonrpc: "2.0",
      id: nil,
      method: "initialize",
      params: %{
        protocolVersion: @protocol_version,
        clientInfo: %{name: "test-client", version: "1.0.0"},
        capabilities: %{}
      }
    }

    response = Req.post!(session_req, json: json_payload)
    assert response.status == 200

    # Should get invalid request error in response body
    assert response.body["error"]["code"] == -32600
  end

  test "JSON-RPC 2.0 compliance - request with string id", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session_with_tools(sse_req, [])

    string_id = "test-request-id"

    response =
      send_rpc_request(
        session_req,
        "initialize",
        %{
          protocolVersion: @protocol_version,
          clientInfo: %{name: "test-client", version: "1.0.0"},
          capabilities: %{}
        },
        string_id
      )

    assert response.status == 202
    data = expect_rpc_response(sse_resp, string_id)
    assert data["id"] == string_id
  end

  test "JSON-RPC 2.0 compliance - request with numeric id", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session_with_tools(sse_req, [])

    numeric_id = 12345

    response =
      send_rpc_request(
        session_req,
        "initialize",
        %{
          protocolVersion: @protocol_version,
          clientInfo: %{name: "test-client", version: "1.0.0"},
          capabilities: %{}
        },
        numeric_id
      )

    assert response.status == 202
    data = expect_rpc_response(sse_resp, numeric_id)
    assert data["id"] == numeric_id
  end

  test "method not found error", %{sse_req: sse_req} do
    {session_req, sse_resp} = init_session(sse_req)

    response = send_rpc_request(session_req, "unknown/method", %{})
    assert response.status == 202

    expect_rpc_error(sse_resp, -32601)
  end

  test "initialize before calling tools", %{sse_req: sse_req} do
    tools = [
      %{
        spec: %{
          "name" => "test_tool",
          "description" => "Test tool",
          "inputSchema" => %{"type" => "object", "properties" => %{}}
        },
        callback: fn _arguments ->
          {:ok, %{content: [%{type: "text", text: "Tool result"}]}}
        end
      }
    ]

    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)

    # Try to call tool before initializing - dispatch_table hasn't been set up yet
    response = send_tools_call_request(session_req, "test_tool", %{})
    assert response.status == 202

    # Should get tool not found error since dispatch_table is empty
    expect_rpc_error(sse_resp, -32601)
  end

  test "multiple tool calls in sequence", %{sse_req: sse_req} do
    tools = [
      %{
        spec: %{
          "name" => "counter",
          "description" => "Returns incrementing numbers",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "start" => %{"type" => "number", "default" => 0}
            }
          }
        },
        callback: fn arguments ->
          start = arguments["start"] || 0
          {:ok, %{content: [%{type: "text", text: "Count: #{start + 1}"}]}}
        end
      }
    ]

    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)

    # Initialize first
    send_initialize_request(session_req)
    expect_rpc_response(sse_resp)

    # Call tool multiple times
    for i <- 1..3 do
      response = send_tools_call_request(session_req, "counter", %{start: i})
      assert response.status == 202

      data = expect_rpc_response(sse_resp)
      content = hd(data["result"]["content"])
      assert content["text"] == "Count: #{i + 1}"
    end
  end

  test "tools with complex input schemas", %{sse_req: sse_req} do
    tools = [
      %{
        spec: %{
          "name" => "complex_tool",
          "description" => "Tool with complex input schema",
          "inputSchema" => %{
            "type" => "object",
            "required" => ["name"],
            "properties" => %{
              "name" => %{"type" => "string", "minLength" => 1},
              "age" => %{"type" => "integer", "minimum" => 0, "maximum" => 150},
              "preferences" => %{
                "type" => "object",
                "properties" => %{
                  "color" => %{"type" => "string", "enum" => ["red", "green", "blue"]},
                  "numbers" => %{"type" => "array", "items" => %{"type" => "number"}}
                }
              }
            }
          }
        },
        callback: fn arguments ->
          name = arguments["name"]
          age = arguments["age"] || "unknown"
          {:ok, %{content: [%{type: "text", text: "Hello #{name}, age: #{age}"}]}}
        end
      }
    ]

    {session_req, sse_resp} = init_session_with_tools(sse_req, tools)

    # Initialize first
    send_initialize_request(session_req)
    expect_rpc_response(sse_resp)

    # List tools to verify schema
    response = send_tools_list_request(session_req)
    assert response.status == 202

    data = expect_rpc_response(sse_resp)
    tool = hd(data["result"]["tools"])
    assert tool["inputSchema"]["required"] == ["name"]
    assert tool["inputSchema"]["properties"]["name"]["type"] == "string"

    # Call tool with valid arguments
    response =
      send_tools_call_request(session_req, "complex_tool", %{
        name: "Alice",
        age: 30,
        preferences: %{
          color: "blue",
          numbers: [1, 2, 3]
        }
      })

    assert response.status == 202
    data = expect_rpc_response(sse_resp)
    content = hd(data["result"]["content"])
    assert content["text"] == "Hello Alice, age: 30"
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

  # RPC Helper Functions
  defp send_rpc_request(session_req, method, params, request_id \\ nil) do
    id = request_id || id()

    json_payload = %{
      jsonrpc: "2.0",
      id: id,
      method: method,
      params: params
    }

    Req.post!(session_req, json: json_payload)
  end

  defp send_rpc_notification(session_req, method, params) do
    json_payload = %{
      jsonrpc: "2.0",
      method: method,
      params: params
    }

    Req.post!(session_req, json: json_payload)
  end

  defp expect_rpc_response(sse_resp, expected_id \\ nil) do
    {:ok, %{event: "message", data: data}} = receive_response_event(sse_resp)
    response = JSON.decode!(data)

    if expected_id do
      assert response["id"] == expected_id
    end

    response
  end

  defp expect_rpc_error(sse_resp, expected_code, expected_id \\ nil) do
    response = expect_rpc_response(sse_resp, expected_id)

    assert response["error"] != nil

    if expected_code do
      assert response["error"]["code"] == expected_code
    end

    response
  end

  defp send_initialize_request(
         session_req,
         client_info \\ %{name: "test-client", version: "1.0.0"}
       ) do
    send_rpc_request(session_req, "initialize", %{
      protocolVersion: @protocol_version,
      clientInfo: client_info,
      capabilities: %{}
    })
  end

  defp send_tools_list_request(session_req, cursor \\ nil) do
    params = if cursor, do: %{cursor: cursor}, else: %{}
    send_rpc_request(session_req, "tools/list", params)
  end

  defp send_tools_call_request(session_req, tool_name, arguments) do
    send_rpc_request(session_req, "tools/call", %{
      name: tool_name,
      arguments: arguments
    })
  end

  # Integration test that verifies validation happens during tool registration
  test "tool validation during initialization with invalid tool", %{sse_req: sse_req} do
    # Define invalid tools (missing required fields)
    invalid_tools = [
      %{
        # Missing spec field - this will trigger the :spec validation error
        callback: nil
      }
    ]

    # Register the invalid tools
    {session_req, sse_resp} = init_session_with_tools(sse_req, invalid_tools)

    # Send initialize request - this should return an error response
    response = send_initialize_request(session_req)
    assert response.status == 202

    # Expect an SSE error event about invalid tool specification
    # invalid_params error code
    error_data = expect_rpc_error(sse_resp, -32602)
    assert error_data["error"]["message"] =~ "Invalid tool specification"
    assert error_data["error"]["message"] =~ ":spec field"
  end

  test "tool validation during initialization with valid tools", %{sse_req: sse_req} do
    # Define valid tools
    valid_tools = [
      %{
        spec: %{
          "name" => "valid_tool",
          "description" => "A valid tool",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "param" => %{"type" => "string"}
            }
          }
        },
        callback: fn _args -> {:ok, %{content: [%{type: "text", text: "success"}]}} end
      }
    ]

    # This should work without errors
    {session_req, sse_resp} = init_session_with_tools(sse_req, valid_tools)

    # Initialize and verify the tool is properly marshaled
    send_initialize_request(session_req)
    data = expect_rpc_response(sse_resp)

    # Verify the tool appears in MCP format
    tools = data["result"]["tools"]
    assert length(tools) == 1

    tool = hd(tools)
    assert tool["name"] == "valid_tool"
    assert tool["description"] == "A valid tool"
    assert tool["inputSchema"]["type"] == "object"
    assert tool["inputSchema"]["properties"]["param"]["type"] == "string"

    # Verify callback field is not present in the marshaled output
    refute Map.has_key?(tool, "callback")
    refute Map.has_key?(tool, :callback)
  end

  defp id(length \\ 5) do
    :crypto.strong_rand_bytes(length)
    |> Base.url_encode64(padding: false)
    |> binary_part(0, length)
  end
end
