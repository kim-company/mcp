defmodule MCP.SSE do
  @moduledoc false

  require Logger

  import Plug.Conn
  alias MCP.Connection

  def handle_sse(conn, opts) do
    session_id = generate_session_id()
    conn = fetch_query_params(conn)

    conn
    |> setup_sse_connection()
    |> send_initial_message(session_id)
    |> enter_loop(session_id, opts)
  end

  def handle_message(conn) do
    params = conn.body_params
    Logger.debug("SSE message received: #{inspect(params, pretty: true)}")

    with {:ok, session_id} <- get_session_id(conn),
         {:ok, connection_pid} <- lookup_session(session_id),
         {:ok, message} <- validate_jsonrpc_message(params) do
      :ok = Connection.handle_message(connection_pid, message)

      conn
      |> put_status(202)
      |> send_json(%{status: "ok"})
    else
      {:error, :missing_session} ->
        Logger.warning("Missing session ID in request")
        send_error(conn, 400, "session_id is required")

      {:error, :invalid_session} ->
        Logger.warning("Invalid session ID provided")
        send_error(conn, 400, "Invalid session ID")

      {:error, :session_not_found} ->
        Logger.warning("Session not found: #{conn.query_params["sessionId"]}")
        send_error(conn, 404, "Could not find session")

      {:error, :invalid_jsonrpc} ->
        Logger.warning("Invalid JSON-RPC message format")
        send_jsonrpc_error(conn, -32600, "Could not parse message")
    end
  end

  defp send_json(conn, data) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(conn.status || 200, JSON.encode!(data))
  end

  defp send_error(conn, status, message) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, JSON.encode!(%{error: message}))
  end

  defp send_jsonrpc_error(conn, code, message) do
    error_response = %{
      jsonrpc: "2.0",
      id: nil,
      error: %{
        code: code,
        message: message
      }
    }
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, JSON.encode!(error_response))
  end

  defp setup_sse_connection(conn) do
    conn
    |> put_resp_header("cache-control", "no-cache")
    |> put_resp_header("connection", "keep-alive")
    |> put_resp_header("content-type", "text/event-stream; charset=utf-8")
    |> send_chunked(200)
  end

  defp send_initial_message(conn, session_id) do
    path = Path.join(conn.path_info ++ ["message"])

    endpoint =
      "#{conn.scheme}://#{conn.host}:#{conn.port}/#{path}?sessionId=#{session_id}"

    case chunk(conn, "event: endpoint\ndata: #{endpoint}\n\n") do
      {:ok, conn} -> conn
      {:error, _reason} -> conn
    end
  end

  defp enter_loop(conn, session_id, opts) do
    try do
      Registry.register(MCP.Registry, session_id, [])
      Connection.init({session_id, conn, opts})
    catch
      :exit, :normal -> conn
      :exit, :shutdown -> conn
      :exit, {:shutdown, _} -> conn
    after
      # Bandit re-uses the same process for new requests,
      # therefore we need to unregister manually and clear
      # any pending messages from the inbox
      Registry.unregister(MCP.Registry, session_id)
      clear_inbox()
      send(self(), {:plug_conn, :sent})
    end
  end

  defp clear_inbox do
    receive do
      _ -> clear_inbox()
    after
      0 -> :ok
    end
  end

  defp get_session_id(conn) do
    case conn.query_params do
      %{"sessionId" => ""} -> {:error, :invalid_session}
      %{"sessionId" => session_id} -> {:ok, session_id}
      _ -> {:error, :missing_session}
    end
  end

  defp lookup_session(session_id) do
    case Registry.lookup(MCP.Registry, session_id) do
      [{connection_pid, _}] -> {:ok, connection_pid}
      [] -> {:error, :session_not_found}
    end
  end

  defp validate_jsonrpc_message(%{"jsonrpc" => "2.0"} = message) do
    cond do
      # Request must have method and id (string or number)
      Map.has_key?(message, "id") and Map.has_key?(message, "method") ->
        case message["id"] do
          id when is_binary(id) or is_number(id) -> {:ok, message}
          nil -> {:error, :invalid_jsonrpc}
          _ -> {:error, :invalid_jsonrpc}
        end

      # Notification must have method but no id
      not Map.has_key?(message, "id") and Map.has_key?(message, "method") ->
        {:ok, message}

      # reply (e.g. to ping) with ID + result
      Map.has_key?(message, "id") and Map.has_key?(message, "result") ->
        {:ok, message}

      true ->
        {:error, :invalid_jsonrpc}
    end
  end

  defp validate_jsonrpc_message(_), do: {:error, :invalid_jsonrpc}

  defp generate_session_id do
    <<i1::32, i2::32, i3::32>> = :crypto.strong_rand_bytes(12)

    :io_lib.format("~8.16.0b-~8.16.0b-~8.16.0b", [i1, i2, i3])
    |> List.to_string()
  end
end
