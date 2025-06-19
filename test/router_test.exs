defmodule MCP.RouterTest do
  use ExUnit.Case, async: true
  import Plug.Test
  import Plug.Conn

  @opts MCP.Router.init([])

  describe "IP address validation" do
    test "rejects non-localhost connections for GET /" do
      conn = 
        conn(:get, "/")
        |> put_remote_ip({192, 168, 1, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 403
      assert conn.resp_body == "Forbidden"
      assert conn.halted
    end

    test "rejects non-localhost connections for POST /message" do
      message = %{"jsonrpc" => "2.0", "id" => 1, "method" => "ping"}
      
      conn = 
        conn(:post, "/message", Jason.encode!(message))
        |> put_req_header("content-type", "application/json")
        |> put_remote_ip({192, 168, 1, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 403
      assert conn.resp_body == "Forbidden"
      assert conn.halted
    end

    test "rejects non-localhost IPv4 addresses" do
      non_localhost_ips = [
        {192, 168, 1, 1},
        {10, 0, 0, 1},
        {172, 16, 0, 1},
        {8, 8, 8, 8}
      ]

      for ip <- non_localhost_ips do
        conn = 
          conn(:get, "/")
          |> put_remote_ip(ip)
          |> MCP.Router.call(@opts)

        assert conn.status == 403
        assert conn.resp_body == "Forbidden"
        assert conn.halted
      end
    end
  end

  describe "HTTP method handling" do
    test "POST to root returns 405 Method Not Allowed" do
      conn = 
        conn(:post, "/")
        |> put_remote_ip({127, 0, 0, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 405
      assert conn.resp_body == "Method not allowed"
      assert conn.halted
    end

    test "POST to root still checks IP first" do
      conn = 
        conn(:post, "/")
        |> put_remote_ip({192, 168, 1, 1})
        |> MCP.Router.call(@opts)

      assert conn.status == 403
      assert conn.resp_body == "Forbidden"
      assert conn.halted
    end
  end

  describe "JSON parsing" do  
    test "malformed JSON results in parsing error" do
      conn = 
        conn(:post, "/message", "{invalid json}")
        |> put_req_header("content-type", "application/json")
        |> put_remote_ip({127, 0, 0, 1})

      # Malformed JSON should result in a parsing error
      assert_raise Plug.Conn.WrapperError, fn ->
        MCP.Router.call(conn, @opts)
      end
    end
  end

  describe "IP validation logic" do
    test "is_local? function works correctly" do
      # Test via private function access
      localhost_ips = [
        {127, 0, 0, 1},
        {127, 0, 0, 255},
        {0, 0, 0, 0, 0, 0, 0, 1},  # IPv6 localhost
        {0, 0, 0, 0, 0, 65535, 32512, 1}  # IPv4-mapped IPv6 localhost
      ]

      non_localhost_ips = [
        {192, 168, 1, 1},
        {10, 0, 0, 1},
        {172, 16, 0, 1},
        {8, 8, 8, 8},
        {0, 0, 0, 0, 0, 0, 0, 2}  # Different IPv6
      ]

      # We can test this indirectly by checking if localhost IPs pass through
      # and non-localhost IPs get blocked
      for ip <- localhost_ips do
        conn = 
          conn(:post, "/")
          |> put_remote_ip(ip)
          |> MCP.Router.call(@opts)

        # Localhost should pass IP check and get 405 for POST /
        assert conn.status == 405
        assert conn.resp_body == "Method not allowed"
      end

      for ip <- non_localhost_ips do
        conn = 
          conn(:post, "/")
          |> put_remote_ip(ip)
          |> MCP.Router.call(@opts)

        # Non-localhost should be blocked with 403
        assert conn.status == 403
        assert conn.resp_body == "Forbidden"
      end
    end
  end

  defp put_remote_ip(conn, ip) do
    %{conn | remote_ip: ip}
  end
end