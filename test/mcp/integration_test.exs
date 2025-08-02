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
