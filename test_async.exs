#!/usr/bin/env elixir

# Simple test script to validate async behavior
Mix.install([
  {:plug, "~> 1.15"},
  {:jason, "~> 1.4"}
])

defmodule AsyncTest do
  def test_slow_tool do
    %{
      name: "slow_tool",
      description: "A tool that takes 5 seconds to execute",
      inputSchema: %{
        type: "object",
        properties: %{}
      },
      callback: fn _args ->
        IO.puts("Starting slow tool execution...")
        :timer.sleep(5000)
        {:ok, "Slow tool completed after 5 seconds!"}
      end
    }
  end

  def test_fast_tool do
    %{
      name: "fast_tool", 
      description: "A tool that executes immediately",
      inputSchema: %{
        type: "object",
        properties: %{}
      },
      callback: fn _args ->
        {:ok, "Fast tool completed instantly!"}
      end
    }
  end

  def run_test do
    IO.puts("Testing async tool execution...")
    IO.puts("1. Slow tool should not block HTTP responses")
    IO.puts("2. Multiple tools can execute concurrently")
    IO.puts("3. HTTP POST /message returns 202 immediately")
    IO.puts("\nTo test manually:")
    IO.puts("1. Start MCP server with these tools")
    IO.puts("2. Call slow_tool via POST /message")
    IO.puts("3. Immediately call fast_tool")
    IO.puts("4. Verify fast_tool returns before slow_tool completes")
  end
end

AsyncTest.run_test()