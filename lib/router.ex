defmodule MCP.Router do
  @moduledoc """
  HTTP router for the MCP (Model Context Protocol) server.

  This module provides a Plug-based router that handles Server-Sent Events (SSE) connections
  and JSON-RPC message routing for MCP clients. It implements the MCP specification over HTTP transport.

  ## Usage

  To use the MCP Router in your application, configure it as a Plug in your endpoint or router:

      plug MCP.Router, init_callback: &MyApp.Tools.init_callback/2

  The router requires an `init_callback` function that will be called when clients initialize
  their MCP connection. This callback is responsible for providing the tools and server
  information that will be available to the client.

  ## Routes

  - `GET /` - Establishes an SSE connection for real-time communication
  - `POST /message` - Receives JSON-RPC messages from the client
  - All other routes return 404
  """

  use Plug.Router

  import Plug.Conn

  plug(:match)
  plug(:dispatch)

  @doc """
  Initializes the router with the provided options.

  ## Options

  * `:init_callback` - Required. A function that will be called when a client initializes
    their MCP connection. The function must have arity 2 and conform to the specification
    documented below.

  ## init_callback Function

  The `init_callback` function is called during the MCP initialization handshake and must
  return the tools and server information that will be available to the client.

  ### Function Signature

      init_callback(session_id, init_params) -> result

  ### Parameters

  * `session_id` - A unique string identifier for the client session
  * `init_params` - A map containing the initialization parameters sent by the client,
    including the protocol version and any client capabilities

  ### Return Value

  The function must return one of:

  * `{:ok, %{server_info: map(), tools: [tool_spec()]}}` - Success with server info and tools
  * `{:error, String.t()}` - Error with a descriptive message

  ### Tool Specification

  Each tool in the tools list must be a map with the following structure:

  * `:spec` - Map. The MCP tool specification with string keys as defined in the MCP protocol
  * `:callback` - Function/1 or nil. The function to call when the tool is executed

  The `:spec` field must contain a map conforming to the MCP tool specification:

  * `"name"` - String. The name of the tool (must be unique)
  * `"inputSchema"` - Map. A JSON Schema defining the tool's input parameters (must have "type" field)
  * `"description"` - String (optional). A description of what the tool does
  * `"title"` - String (optional). A human-readable title for the tool
  * `"outputSchema"` - Map (optional). A JSON Schema defining the tool's output format
  * `"annotations"` - Map (optional). Hints about tool behavior

  The callback function, if provided, should accept a map of arguments and return
  `{:ok, result}` or `{:error, reason}`.

  ### Example

      def my_init_callback(session_id, _init_params) do
        tools = [
          %{
            spec: %{
              "name" => "echo",
              "description" => "Echoes back the input text",
              "inputSchema" => %{
                "type" => "object",
                "properties" => %{
                  "text" => %{"type" => "string", "description" => "Text to echo"}
                },
                "required" => ["text"]
              }
            },
            callback: fn %{"text" => text} ->
              {:ok, %{content: [%{type: "text", text: text}]}}
            end
          }
        ]

        {:ok, %{
          server_info: %{
            name: "My MCP Server",
            version: "1.0.0"
          },
          tools: tools
        }}
      end
  """
  def init(opts) do
    Keyword.validate!(opts,
      init_callback: fn _session_id, _init_params ->
        {:ok, %{server_info: %{}, tools: []}}
      end
    )
  end

  @doc """
  Processes incoming HTTP requests through the MCP router.

  This function is automatically called by the Plug pipeline and handles:

  - Setting up the init_callback in the connection assigns
  - Delegating to the parent Plug.Router implementation

  You typically don't need to call this function directly.
  """
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
