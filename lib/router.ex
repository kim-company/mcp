defmodule MCP.Router do
  use Plug.Router

  import Plug.Conn

  plug :match
  plug :check_remote_ip
  plug :dispatch

  get "/" do
    conn
    |> MCP.SSE.handle_sse()
    |> halt()
  end

  post "/" do
    conn
    |> send_resp(405, "Method not allowed")
    |> halt()
  end

  post "/message" do
    opts =
      Plug.Parsers.init(
        parsers: [:json],
        pass: [],
        json_decoder: JSON
      )

    conn
    |> Plug.Parsers.call(opts)
    |> MCP.SSE.handle_message()
    |> halt()
  end

  defp is_local?({127, 0, 0, _}), do: true
  defp is_local?({0, 0, 0, 0, 0, 0, 0, 1}), do: true
  # ipv4 mapped ipv6 address ::ffff:127.0.0.1
  defp is_local?({0, 0, 0, 0, 0, 65535, 32512, 1}), do: true
  defp is_local?(_), do: false

  defp check_remote_ip(conn, _opts) do
    cond do
      is_local?(conn.remote_ip) ->
        conn

      true ->
        conn
        |> send_resp(403, "Forbidden")
        |> halt()
    end
  end
end
