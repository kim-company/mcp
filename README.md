# MCP - Model Context Protocol Server

An Elixir implementation of the Model Context Protocol (MCP) server that communicates via Server-Sent Events over HTTP.

## Installation

Add `mcp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:mcp, "~> 0.1.0"}
  ]
end
```

## Phoenix Integration

Mount the MCP router in your Phoenix application:

```elixir
# In your Phoenix router (lib/my_app_web/router.ex)
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  # Your existing routes...
  
  # Mount MCP router under /mcp prefix
  forward "/mcp", MCP.Router, init_callback: &MyApp.MCP.Tools.init_callback/2
end
```

Define your tools:

```elixir
# lib/my_app/mcp/tools.ex
defmodule MyApp.MCP.Tools do
  def init_callback(_session_id, _init_params) do
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
          {:ok, %{content: [%{type: "text", text: "Echo: #{text}"}]}}
        end
      }
    ]

    {:ok, %{
      server_info: %{name: "My App MCP Server", version: "1.0.0"},
      tools: tools
    }}
  end
end
```

Your MCP server will be available at:
- SSE endpoint: `GET /mcp/`
- JSON-RPC messages: `POST /mcp/message`

## Protocol Implementation

This implementation follows the MCP specification version **2024-11-05** with:

**Core Features:**
- Server-Sent Events (SSE) transport
- JSON-RPC 2.0 message protocol  
- Session management with automatic cleanup
- Tool discovery via `tools/list`
- Tool execution via `tools/call`
- Protocol version negotiation

**Tool Support:**
- MCP-compliant tool specifications
- JSON Schema validation for input parameters
- Text content responses
- Error handling with `isError` flag
- Tool callback execution

**Not Implemented:**
- Image/resource content types
- `listChanged` notifications
- Advanced pagination features

## License

MIT License - see LICENSE file for details.