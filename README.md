# MCP - Model Context Protocol Server

An Elixir implementation of the [Model Context Protocol (MCP)](https://spec.modelcontextprotocol.io/) server that communicates via Server-Sent Events (SSE) over HTTP.

## Overview

This library provides a complete MCP server implementation built on OTP principles using GenServers, supervision trees, and fault tolerance. It enables you to create custom tools that AI models can discover and execute through a standardized protocol.

## Features

- **Standards Compliant**: Implements MCP specification 2024-11-05
- **SSE Transport**: Real-time communication via Server-Sent Events over HTTP
- **JSON-RPC 2.0**: Full support for request/response/notification patterns
- **Fault Tolerant**: Built on OTP with proper supervision and error handling
- **Session Management**: UUID-based session tracking with automatic cleanup
- **Async Tool Execution**: Non-blocking tool execution with proper timeout handling
- **Clean Tool Definition**: MCP-compliant tool specifications with proper type validation

## Protocol Implementation Status

This section tracks implementation of the MCP 2024-11-05 specification for tools:

### ✅ Core Protocol
- [x] Server-Sent Events (SSE) transport layer
- [x] JSON-RPC 2.0 message protocol
- [x] Session management with unique session IDs
- [x] Connection lifecycle management
- [x] Protocol version negotiation (2024-11-05)

### ✅ Initialization
- [x] `initialize` method with protocol version validation
- [x] Server capabilities declaration
- [x] Client capabilities handling
- [x] Server info response (name, version)
- [x] `notifications/initialized` completion

### ✅ Tools Protocol
- [x] Tools capability declaration in server capabilities
- [x] Tool definition with name, description, input schema
- [x] Tool validation during initialization
- [x] `tools/list` method for tool discovery
- [x] `tools/call` method for tool execution
- [x] Pagination support with cursor parameter
- [x] Tool callback execution with proper response format
- [x] Error handling for unknown tools
- [x] Tool execution error reporting with `isError` flag

### ✅ Content Types
- [x] Text content support (`type: "text"`)
- [x] Content array responses
- [x] Error content with `isError` flag

### ✅ Error Handling
- [x] JSON-RPC 2.0 error responses
- [x] Protocol-level error codes (-32600, -32601, -32602)
- [x] Tool execution error handling
- [x] Invalid tool specification validation
- [x] Missing required parameter handling

### ✅ Security & Validation
- [x] Input validation for tool schemas
- [x] Tool specification validation (required fields)
- [x] JSON Schema compliance for input schemas
- [x] Argument validation during tool calls

### ❌ Not Implemented
- [ ] `listChanged` notification capability
- [ ] Image content support (`type: "image"`)
- [ ] Resource content support (`type: "resource"`)
- [ ] Advanced pagination features
- [ ] Rate limiting for tool invocations
- [ ] Access control mechanisms
- [ ] Tool execution timeouts

### 🔄 Partial Implementation
- [ ] **Tool input validation**: Basic validation implemented, but full JSON Schema validation against tool input schemas not enforced during `tools/call`

## Installation

Add `mcp` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:mcp, "~> 0.1.0"}
  ]
end
```

## Quick Start

### 1. Define Your Tools

Create an initialization callback that defines the tools available to MCP clients:

```elixir
defmodule MyApp.MCPTools do
  def init_callback(_session_id, _init_params) do
    tools = [
      %{
        spec: %{
          "name" => "echo",
          "description" => "Echoes back the input text",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "text" => %{
                "type" => "string", 
                "description" => "Text to echo back"
              }
            },
            "required" => ["text"]
          }
        },
        callback: fn %{"text" => text} ->
          {:ok, %{
            content: [
              %{type: "text", text: "Echo: #{text}"}
            ]
          }}
        end
      },
      %{
        spec: %{
          "name" => "get_time",
          "description" => "Returns the current time",
          "inputSchema" => %{"type" => "object", "properties" => %{}}
        },
        callback: fn _args ->
          time = DateTime.utc_now() |> DateTime.to_string()
          {:ok, %{
            content: [
              %{type: "text", text: "Current time: #{time}"}
            ]
          }}
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
end
```

### 2. Set Up the Router

Configure the MCP router in your Phoenix endpoint or Plug pipeline:

#### Option 1: Using forward 

Use the `MCP.Router` module with `forward` to mount the MCP router at a specific path:

```elixir
# In your config.exs
config :mime, :types, %{
  "text/event-stream" => ["sse"]
}

# In your Phoenix router
defmodule MyAppWeb.Router do
  use MyAppWeb, :router

  pipeline :mcp_sse do
    plug accepts: ["sse"]
  end

  # Forward MCP requests to the MCP.Router
  scope "/mcp/sse" do
    pipe_through :mcp_sse
    
    forward "/", MCP.Router, init_callback: &MyApp.MCPTools.init_callback/2
  end
  
  # Your other routes...
end
```

This makes the MCP router available at:
- `GET /mcp/sse` - Establishes an SSE connection
- `POST /mcp/sse/message` - Receives JSON-RPC messages

#### Option 2: Direct plug usage

```elixir
# In your Phoenix endpoint
defmodule MyAppWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :my_app

  # Add the MCP router directly
  plug MCP.Router, init_callback: &MyApp.MCPTools.init_callback/2
  
  # Your other plugs...
end
```

#### Option 3: Standalone with Bandit

```elixir
# In your application.ex
def start(_type, _args) do
  children = [
    # Start your app supervision tree
    MCP.Application,
    
    # Start the HTTP server
    {Bandit, 
     plug: {MCP.Router, init_callback: &MyApp.MCPTools.init_callback/2},
     scheme: :http,
     port: 4000}
  ]

  opts = [strategy: :one_for_one, name: MyApp.Supervisor]
  Supervisor.start_link(children, opts)
end
```

### 3. Start Your Server

```bash
iex -S mix
```

Your MCP server will be available at `http://localhost:4000`.

## Usage

### Client Connection

Clients connect to your server by:

1. **Establishing SSE Connection**: `GET /` to start receiving server events
2. **Sending Messages**: `POST /message?sessionId=<session_id>` to send JSON-RPC messages

### Tool Specification Format

Tools must follow this structure:

```elixir
%{
  spec: %{                              # MCP-compliant tool specification
    "name" => "tool_name",              # Unique identifier (string)
    "description" => "What the tool does", # Human-readable description (optional)
    "inputSchema" => %{                 # JSON Schema for parameters
      "type" => "object",
      "properties" => %{
        "param1" => %{"type" => "string"}
      },
      "required" => ["param1"]
    }
  },
  callback: fn args ->                  # Function to execute (optional)
    # Tool logic here
    {:ok, result} | {:error, reason}
  end
}
```

**Key Requirements:**
- `spec` field contains the MCP tool specification with string keys
- `spec["name"]` and `spec["inputSchema"]` are required
- `spec["inputSchema"]` must have a `"type"` field
- `callback` is optional and should be a function/1 or nil

**Benefits of This Structure:**
- **Protocol Compliant**: Tool specs are stored exactly as defined in the MCP specification
- **No Key Conversion**: Eliminates the need to convert between atom and string keys
- **Type Safe**: Clear separation between MCP specification and internal callback logic
- **Validation Friendly**: Direct validation against MCP schema requirements

### Tool Callback Return Format

Tool callbacks should return results in MCP format:

```elixir
# Success response
{:ok, %{
  content: [
    %{type: "text", text: "Result text"},
    %{type: "image", data: "base64_image", mimeType: "image/png"}
  ]
}}

# Error response  
{:error, "Error description"}
```

## Architecture

### Core Components

- **`MCP.Application`**: OTP application entry point and supervision
- **`MCP.Router`**: HTTP routing with Plug, handles SSE and JSON-RPC endpoints
- **`MCP.Connection`**: GenServer managing individual client connections and protocol state
- **`MCP.SSE`**: Server-Sent Events transport layer implementation

### Message Flow

1. Client establishes SSE connection via `GET /`
2. Server responds with session endpoint URL
3. Client sends `initialize` request with protocol version
4. Server validates and responds with capabilities and tools
5. Client sends `notifications/initialized` to complete handshake
6. Client can now call tools via `tools/call` requests

### Session Management

- Each connection gets a unique session ID
- Sessions are tracked in an OTP Registry
- Automatic cleanup on disconnection or timeout
- 30-second initialization timeout
- 30-minute inactivity timeout

## Configuration

### Environment Variables

Set these in your application configuration:

```elixir
# config/config.exs
config :mcp,
  port: 4000,
  host: "localhost"
```

## Error Handling

The server handles errors gracefully:

- **Protocol Errors**: Invalid JSON-RPC messages return appropriate error codes
- **Tool Errors**: Tool execution failures are returned as successful responses with `isError: true`
- **Timeouts**: Connections that don't initialize or remain inactive are automatically closed
- **Process Crashes**: Supervision tree ensures failed processes are restarted


## Advanced Usage

### Custom Validation

Implement custom tool validation by extending the built-in validation:

```elixir
def init_callback(session_id, init_params) do
  # Custom logic based on session or client capabilities
  if authorized?(session_id) do
    {:ok, %{tools: get_tools_for_user(session_id), server_info: %{}}}
  else
    {:error, "Unauthorized"}
  end
end
```

### Dynamic Tool Loading

Tools can be loaded dynamically based on client needs:

```elixir
def init_callback(_session_id, %{"clientInfo" => client_info}) do
  tools = case client_info["name"] do
    "development-client" -> development_tools()
    "production-client" -> production_tools()
    _ -> standard_tools()
  end
  
  {:ok, %{tools: tools, server_info: %{}}}
end
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Run `mix test` and `mix format`
6. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Links

- [Model Context Protocol Specification](https://spec.modelcontextprotocol.io/)
- [MCP Official Documentation](https://modelcontextprotocol.io/)
- [Elixir Documentation](https://hexdocs.pm/elixir/)
