defmodule MCP.Test.Dispatcher do
  @moduledoc """
  A GenServer that acts as a dispatcher for test init_callback functions.
  
  Tests can register callback functions before calling init_session,
  and the dispatcher will look up and execute the appropriate callback
  when the MCP initialization happens.
  """
  
  use GenServer

  ## Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, Keyword.put_new(opts, :name, __MODULE__))
  end

  @doc """
  Register a callback function for a specific session_id.
  The callback will be called during MCP initialization for that session.
  """
  def register_callback(session_id, callback_fn, server \\ __MODULE__) do
    GenServer.call(server, {:register, session_id, callback_fn})
  end

  @doc """
  Lookup and execute the callback function for the given session_id.
  This is called by the MCP router during initialization.
  """
  def dispatch_callback(session_id, init_params, server \\ __MODULE__) do
    GenServer.call(server, {:dispatch, session_id, init_params})
  end

  @doc """
  Clear the callback for a specific session_id.
  Useful for cleanup between tests.
  """
  def clear_callback(session_id, server \\ __MODULE__) do
    GenServer.call(server, {:clear, session_id})
  end

  ## Server Callbacks

  @impl true
  def init(:ok) do
    # Simple callbacks map keyed by session_id
    {:ok, %{callbacks: %{}}}  
  end

  @impl true
  def handle_call({:register, session_id, callback_fn}, _from, state) do
    new_state = %{
      state |
      callbacks: Map.put(state.callbacks, session_id, callback_fn)
    }
    
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:dispatch, session_id, init_params}, _from, state) do
    case Map.get(state.callbacks, session_id) do
      nil ->
        # Default callback if no specific one is registered
        default_result = {:ok, %{server_info: %{name: "test-server"}, tools: []}}
        {:reply, default_result, state}
      
      callback_fn ->
        result = callback_fn.(session_id, init_params)
        {:reply, result, state}
    end
  end

  @impl true
  def handle_call({:clear, session_id}, _from, state) do
    new_state = %{
      state |
      callbacks: Map.delete(state.callbacks, session_id)
    }
    
    {:reply, :ok, new_state}
  end
end