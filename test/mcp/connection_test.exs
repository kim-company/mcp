defmodule MCP.ConnectionTest do
  use ExUnit.Case, async: true

  describe "validate_tool_spec/1" do
    test "validates tool with all required fields" do
      valid_tool = %{
        spec: %{
          "name" => "test_tool",
          "description" => "A test tool",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "param1" => %{"type" => "string"}
            }
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "validates tool with minimal required fields" do
      valid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{"type" => "object"}
        },
        callback: nil
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "rejects tool with missing spec field" do
      invalid_tool = %{
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ ":spec field"
    end

    test "rejects tool with non-map spec field" do
      invalid_tool = %{
        spec: "not a map",
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ ":spec field must be a map"
    end

    test "rejects tool with missing name in spec" do
      invalid_tool = %{
        spec: %{
          "description" => "A test tool",
          "inputSchema" => %{"type" => "object"}
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "'name' field"
    end

    test "rejects tool with empty name" do
      invalid_tool = %{
        spec: %{
          "name" => "",
          "inputSchema" => %{"type" => "object"}
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "non-empty string"
    end

    test "rejects tool with non-string name" do
      invalid_tool = %{
        spec: %{
          "name" => 123,
          "inputSchema" => %{"type" => "object"}
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "non-empty string"
    end

    test "rejects tool with missing inputSchema" do
      invalid_tool = %{
        spec: %{
          "name" => "test_tool",
          "description" => "A test tool"
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "'inputSchema' field"
    end

    test "rejects tool with non-map inputSchema" do
      invalid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => "not a map"
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "inputSchema must be a map"
    end

    test "rejects tool with inputSchema missing type field" do
      invalid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{
            "properties" => %{
              "param1" => %{"type" => "string"}
            }
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "'type' field"
    end

    test "rejects tool with non-string type in inputSchema" do
      invalid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{
            "type" => 123
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "'type' must be a string"
    end

    test "rejects tool with non-map properties in inputSchema" do
      invalid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{
            "type" => "object",
            "properties" => "not a map"
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "'properties' must be a map"
    end

    test "accepts tool with valid inputSchema - object with properties" do
      valid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "param1" => %{"type" => "string"},
              "param2" => %{"type" => "number"}
            },
            "required" => ["param1"]
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "accepts tool with non-object type inputSchema" do
      valid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{
            "type" => "string"
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "rejects tool with invalid callback type" do
      invalid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{"type" => "object"}
        },
        callback: "not a function"
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "function/1"
    end

    test "rejects tool with wrong arity callback" do
      invalid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{"type" => "object"}
        },
        # Wrong arity
        callback: fn -> :ok end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "function/1"
    end

    test "accepts tool with nil callback" do
      valid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{"type" => "object"}
        },
        callback: nil
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "accepts tool with no callback field" do
      valid_tool = %{
        spec: %{
          "name" => "test_tool",
          "inputSchema" => %{"type" => "object"}
        }
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "accepts tool with optional MCP fields" do
      valid_tool = %{
        spec: %{
          "name" => "test_tool",
          "description" => "A test tool",
          "title" => "Test Tool",
          "inputSchema" => %{"type" => "object"},
          "outputSchema" => %{"type" => "object"},
          "annotations" => %{
            "destructiveHint" => true,
            "idempotentHint" => false
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "rejects non-map input" do
      assert {:error, reason} = MCP.Connection.validate_tool_spec("not a map")
      assert reason =~ "must be a map"
    end

    test "rejects nil input" do
      assert {:error, reason} = MCP.Connection.validate_tool_spec(nil)
      assert reason =~ "must be a map"
    end
  end

  describe "marshal_tool_spec/1" do
    test "extracts spec from tool specification" do
      tool = %{
        spec: %{
          "name" => "test_tool",
          "description" => "A test tool",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "param1" => %{"type" => "string"}
            }
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      marshaled = MCP.Connection.marshal_tool_spec(tool)

      expected = %{
        "name" => "test_tool",
        "description" => "A test tool",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "param1" => %{"type" => "string"}
          }
        }
      }

      assert marshaled == expected
      refute Map.has_key?(marshaled, "callback")
      refute Map.has_key?(marshaled, :callback)
    end

    test "preserves all MCP spec fields" do
      tool = %{
        spec: %{
          "name" => "complex_tool",
          "description" => "Complex tool",
          "title" => "Complex Tool Title",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "param1" => %{"type" => "string"}
            }
          },
          "outputSchema" => %{
            "type" => "object",
            "properties" => %{
              "result" => %{"type" => "string"}
            }
          },
          "annotations" => %{
            "destructiveHint" => true,
            "readOnlyHint" => false
          }
        },
        callback: fn _args -> {:ok, %{}} end
      }

      marshaled = MCP.Connection.marshal_tool_spec(tool)

      expected = %{
        "name" => "complex_tool",
        "description" => "Complex tool",
        "title" => "Complex Tool Title",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "param1" => %{"type" => "string"}
          }
        },
        "outputSchema" => %{
          "type" => "object",
          "properties" => %{
            "result" => %{"type" => "string"}
          }
        },
        "annotations" => %{
          "destructiveHint" => true,
          "readOnlyHint" => false
        }
      }

      assert marshaled == expected
    end

    test "handles minimal spec" do
      tool = %{
        spec: %{
          "name" => "minimal_tool",
          "inputSchema" => %{"type" => "object"}
        },
        callback: nil
      }

      marshaled = MCP.Connection.marshal_tool_spec(tool)

      expected = %{
        "name" => "minimal_tool",
        "inputSchema" => %{"type" => "object"}
      }

      assert marshaled == expected
    end
  end
end
