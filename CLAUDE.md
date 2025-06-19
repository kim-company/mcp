# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Development
- `mix compile` - Compile the project
- `mix test` - Run tests
- `mix deps.get` - Install dependencies

### Testing
- `elixir test_async.exs` - Test async tool execution behavior

## Architecture

This is an Elixir implementation of the Model Context Protocol (MCP) server that communicates via Server-Sent Events (SSE) over HTTP.

### Core Components

- **MCP.Server**: Core GenServer that handles MCP protocol messages (initialize, ping, tools/list, tools/call). Supports both synchronous (`handle_message/2`) and asynchronous (`handle_message_async/2`) message processing.

- **MCP.SSE**: Handles SSE connections and HTTP message routing. The `/message` endpoint now processes tool calls asynchronously - it validates requests synchronously then spawns async tasks for tool execution.

- **MCP.Connection**: Manages individual client connection state, tool dispatch, and SSE message sending. Handles connection lifecycle, timeouts, and state management.

- **MCP.Router**: HTTP routing with localhost-only access restriction. GET / establishes SSE connection, POST /message accepts JSON-RPC messages.

- **MCP.Supervisor**: Supervises the server and session registry.

### Protocol Flow

1. Client connects via GET / (establishes SSE connection with session ID)
2. Client sends initialize message via POST /message
3. Server responds with capabilities and available tools
4. Client can call registered tools via tools/call messages
5. Tool execution happens asynchronously - HTTP responds immediately with 202, tool results sent via SSE
6. Connection maintained with periodic pings and inactivity timeouts

### Async Tool Execution

The server now supports non-blocking tool execution:
- POST /message validates requests and returns HTTP 202 immediately
- Tool execution happens in separate Tasks
- Results are sent asynchronously via SSE connection
- Multiple tools can execute concurrently
- Maintains JSON-RPC 2.0 and MCP protocol compliance

### Key Files

- `lib/server.ex` - MCP protocol implementation with async support
- `lib/sse.ex` - SSE transport and HTTP message handling
- `lib/connection.ex` - Connection state management
- `lib/router.ex` - HTTP routing
- `test_async.exs` - Async behavior validation script

### Dependencies

- `plug` - HTTP server functionality
- `jason` - JSON encoding/decoding
- Built-in Elixir GenServer, Registry, Task for concurrency