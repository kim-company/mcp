# MCP Server (Elixir Implementation)

An Elixir implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) server that communicates via Server-Sent Events (SSE) over HTTP.

## Overview

This server implements the MCP 2024-11-05 specification, providing a bridge between AI applications and external data sources and tools. It features asynchronous tool execution, robust connection management, and full JSON-RPC 2.0 compliance.

## Features

### Core Architecture
- **GenServer-based**: Built on Elixir's OTP principles for fault tolerance and concurrency
- **SSE Transport**: Real-time communication via Server-Sent Events over HTTP
- **Async Tool Execution**: Non-blocking tool calls with concurrent execution support
- **Connection Management**: Session-based connections with timeout handling and lifecycle management
- **JSON-RPC 2.0 Compliant**: Full compatibility with the JSON-RPC 2.0 specification

### Security
- **Localhost Only**: HTTP server restricted to localhost access for security
- **Session Management**: Secure session-based connections with unique identifiers
- **Timeout Protection**: Automatic connection cleanup after inactivity periods

## Installation

Since this project is not yet published on Hex.pm, install directly from Git:

```bash
# Clone the repository
git clone <repository-url>
cd mcp

# Install dependencies
mix deps.get

# Compile the project
mix compile
```

## Usage

### Starting the Server

```elixir
# Start the application
mix run --no-halt

# Or in IEx
iex -S mix
```

The server will start on `http://localhost:4000` by default.

### Registering Tools

Tools can be registered with the server using the tool registration API:

```elixir
# Define a tool
tool = %{
  name: "example_tool",
  description: "An example tool that demonstrates MCP functionality",
  inputSchema: %{
    type: "object",
    properties: %{
      message: %{type: "string", description: "A message to process"}
    },
    required: ["message"]
  },
  callback: fn args -> 
    {:ok, "Processed: #{args["message"]}"} 
  end
}

# Register the tool
MCP.Server.register(tool)
```

### Client Connection Flow

1. **Establish SSE Connection**: `GET /` to establish Server-Sent Events connection
2. **Initialize**: Send `initialize` message via `POST /message`
3. **Get Tools**: Call `tools/list` to retrieve available tools
4. **Execute Tools**: Call `tools/call` to execute registered tools
5. **Maintain Connection**: Handle periodic pings and activity timeouts

## MCP Protocol Compliance

### ✅ Implemented Features

| Feature Category | Feature | Status | Notes |
|-----------------|---------|---------|-------|
| **Base Protocol** | JSON-RPC 2.0 | ✅ | Full compliance with message structure |
| | Protocol Version Negotiation | ✅ | Supports MCP 2024-11-05 |
| | Error Handling | ✅ | Proper error codes and messages |
| **Lifecycle Management** | Connection Initialization | ✅ | SSE-based connection establishment |
| | Initialize/Initialized Handshake | ✅ | Full initialization sequence |
| | Ping/Pong | ✅ | Connection keepalive mechanism |
| | Graceful Shutdown | ✅ | Proper connection cleanup |
| **Server Features - Tools** | Tool Registration | ✅ | Dynamic tool registration |
| | Tool Listing (`tools/list`) | ✅ | Enumerate available tools |
| | Tool Execution (`tools/call`) | ✅ | Synchronous and asynchronous execution |
| | Tool Input Validation | ✅ | JSON Schema-based validation |
| | Tool Error Handling | ✅ | Proper error responses with `isError` flag |
| | Concurrent Tool Execution | ✅ | Multiple tools can run simultaneously |
| **Transport** | Server-Sent Events (SSE) | ✅ | Real-time bidirectional communication |
| | HTTP POST for Messages | ✅ | JSON-RPC messages via HTTP |
| | Session Management | ✅ | UUID-based session tracking |
| **Connection Management** | Inactivity Timeouts | ✅ | 30-minute inactivity timeout |
| | Initialization Timeouts | ✅ | 30-second initialization timeout |
| | Connection State Tracking | ✅ | State machine: connected → initialized → ready |

### ❌ Not Yet Implemented

| Feature Category | Feature | Status | Priority |
|-----------------|---------|---------|----------|
| **Server Features - Resources** | Resource Registration | ❌ | Medium |
| | Resource Listing (`resources/list`) | ❌ | Medium |
| | Resource Reading (`resources/read`) | ❌ | Medium |
| | Resource Templates | ❌ | Low |
| | Resource Subscriptions | ❌ | Low |
| **Server Features - Prompts** | Prompt Registration | ❌ | Medium |
| | Prompt Listing (`prompts/list`) | ❌ | Medium |
| | Prompt Retrieval (`prompts/get`) | ❌ | Medium |
| **Client Features** | Sampling Support | ❌ | Low |
| | Root Directory Lists | ❌ | Low |
| **Advanced Features** | Tool Input Completion | ❌ | Low |
| | Progress Notifications | ❌ | Medium |
| | Cancellation Support | ❌ | Medium |
| **Security** | Authentication/Authorization | ❌ | High |
| | Access Control Lists | ❌ | High |
| **Transport Alternatives** | WebSocket Transport | ❌ | Low |
| | Unix Socket Transport | ❌ | Low |

### Protocol Version Support

- **Target Version**: MCP 2024-11-05
- **Backwards Compatibility**: None (first implementation)
- **Forward Compatibility**: Version negotiation supports newer clients

## Development

### Running Tests

```bash
# Run the test suite
mix test

# Test async behavior specifically
elixir test_async.exs
```

### Project Structure

```
lib/
├── mcp/
│   └── application.ex      # OTP Application
├── connection.ex           # Connection state management
├── mcp.ex                 # Main module
├── router.ex              # HTTP routing
├── server.ex              # MCP protocol implementation
├── sse.ex                 # SSE transport layer
└── supervisor.ex          # Process supervision
```

### Key Components

- **`MCP.Server`**: Core GenServer handling MCP protocol messages with both sync and async processing
- **`MCP.SSE`**: Manages SSE connections and HTTP message routing with async tool execution
- **`MCP.Connection`**: Handles individual client connections, timeouts, and state management
- **`MCP.Router`**: HTTP routing with localhost-only security restrictions

## Configuration

The server can be configured through application environment:

```elixir
# config/config.exs
config :mcp,
  port: 4000,
  host: "localhost"
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `mix test`
6. Submit a pull request

## Protocol Flow Example

```
Client                    Server
  |                         |
  |-- GET / (SSE) --------->|
  |<-- session_id, endpoint |
  |                         |
  |-- POST /message ------->|
  |   (initialize)          |
  |<-- 202 Accepted --------|
  |<-- SSE: capabilities ---|
  |                         |
  |-- POST /message ------->|
  |   (tools/list)          |
  |<-- 202 Accepted --------|
  |<-- SSE: tools list ----|
  |                         |
  |-- POST /message ------->|
  |   (tools/call)          |
  |<-- 202 Accepted --------|
  |<-- SSE: tool result ---|
```

## License

[Add your license information here]

## Acknowledgments

This implementation is based on the [tidewave](https://github.com/tidewave-ai/tidewave_phoenix) project's MCP implementation, used under the MIT License.