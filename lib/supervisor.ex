defmodule MCP.Supervisor do
  @moduledoc false

  use Supervisor

  def start_link(opts) do
    opts = Keyword.validate!(opts, load_tools: fn -> [] end)
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      # For registering client sessions.
      {Registry, name: MCP.Registry, keys: :unique}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
