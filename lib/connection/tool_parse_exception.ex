defmodule MCP.Connection.ToolParseException do
  @moduledoc """
  Exception raised when tool specification parsing fails.

  This exception is used internally during tool specification validation
  to enable early termination and clear error reporting.
  """

  defexception [:message]
end
