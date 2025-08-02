defmodule MCP.Router do
  use Plug.Router

  import Plug.Conn

  plug(:match)
  plug(:dispatch)

  def init(opts) do
    Keyword.validate!(opts,
      init_callback: fn _init_params ->
        {:ok, %{server_info: %{}, tools: []}}
      end
    )
  end

  def call(conn, opts) do
    conn
    |> assign(:init_callback, opts[:init_callback])
    |> super(opts)
  end

  get "/" do
    conn
    |> MCP.SSE.handle_sse(init_callback: conn.assigns[:init_callback])
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

  match _ do
    conn
    |> send_resp(404, "Route not found")
    |> halt()
  end
end
