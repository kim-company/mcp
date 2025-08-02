# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

This is an Elixir project using Mix. Common commands:

```bash
# Install dependencies
mix deps.get

# Compile the project
mix compile

# Run the application (starts HTTP server on localhost:4000)
mix run --no-halt

# Interactive shell with the application loaded
iex -S mix

# Run tests
mix test

# Run async behavior tests specifically
elixir test_async.exs

# Check code formatting
mix format --check-formatted

# Format code
mix format
```

## Project Architecture

This is an Elixir implementation of the Model Context Protocol (MCP) server that communicates via Server-Sent Events (SSE) over HTTP.

### Core Components

- **`MCP.Application`** (`lib/mcp/application.ex`): OTP Application entry point, sets up Registry for session management
- **`MCP.Connection`** (`lib/connection.ex`): GenServer handling individual client connections, protocol state management, timeouts, and JSON-RPC message processing
- **`MCP.SSE`** (`lib/sse.ex`): Server-Sent Events transport layer for real-time communication
- **`MCP.Router`** (`lib/router.ex`): HTTP routing with Plug, handles GET for SSE connections and POST for JSON-RPC messages
- **`MCP.Supervisor`** (`lib/supervisor.ex`): Process supervision tree

### Architecture Flow

1. **Connection Establishment**: Client connects via GET / to establish SSE connection
2. **Protocol Handshake**: Client sends `initialize` message, server responds with capabilities
3. **Tool Operations**: Client can list tools (`tools/list`) and execute them (`tools/call`)
4. **State Management**: Connection state machine: connected → initialized → ready

### Key Features

- **GenServer-based**: Built on OTP principles for fault tolerance
- **Session Management**: UUID-based sessions tracked via Registry
- **Timeout Handling**: 30-second initialization timeout, 30-minute inactivity timeout
- **JSON-RPC 2.0**: Full compliance with request/response/notification patterns
- **Protocol Version**: Implements MCP 2024-11-05 specification

### Configuration

The server runs on localhost:4000 by default. Environment variables can be set via application config:

```elixir
# config/config.exs
config :mcp,
  port: 4000,
  host: "localhost"
```

### Development Environment

- **Elixir**: 1.18.3-otp-27 (specified in mise.toml)
- **Erlang**: 27.2.1
- **Dependencies**: Plug for HTTP, Bandit/Req for testing

### Test Structure

- `test/mcp/integration_test.exs`: Integration tests for MCP protocol
- `test_async.exs`: Async behavior tests (run separately with `elixir test_async.exs`)

The codebase follows standard Elixir/OTP patterns with GenServers for stateful processes, Plug for HTTP handling, and supervision trees for fault tolerance.