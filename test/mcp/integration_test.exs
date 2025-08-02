defmodule MCP.IntegrationTest do
  use ExUnit.Case, async: true

  @protocol_version "2024-11-05"

  setup_all do
    # Start the server once.
    pid = start_link_supervised!({Bandit, [plug: MCP.Router, port: 0]})
    {:ok, {_ip, port}} = ThousandIsland.listener_info(pid)
    req = Req.new(retry: false, base_url: "http://localhost:#{port}")
    %{sse_req: req}
  end

  setup %{sse_req: sse_req} do
    # Start a session for each request.
    {session_req, sse_resp} = init_session(sse_req)
    %{sse_req: sse_req, sse_resp: sse_resp, session_req: session_req}
  end

  test "405 on invalid request", %{sse_req: req} do
    resp = Req.post!(req, url: "/")
    assert resp.status == 405
  end

  test "session is correctly initialized", %{session_req: req, sse_resp: resp} do
    assert resp.status == 200
    url = to_string(req.url)
    assert String.starts_with?(url, "http")
    assert String.contains?(url, "message?sessionId")
  end

  test "fails with invalid jsonrpc messages", %{session_req: req} do
    resp = Req.post!(req, json: %{event: :invalid})

    assert resp.status == 200

    assert resp.body == %{
             "error" => %{"code" => -32600, "message" => "Could not parse message"},
             "id" => nil,
             "jsonrpc" => "2.0"
           }
  end

  test "mcp initialization", %{session_req: req, sse_resp: sse_resp} do
    resp =
      Req.post!(req,
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
