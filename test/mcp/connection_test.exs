defmodule MCP.ConnectionTest do
  use ExUnit.Case, async: true

  describe "validate_tool_spec/1" do
    test "validates tool with all required fields" do
      valid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{
          "type" => "object",
          "properties" => %{
            "param1" => %{"type" => "string"}
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "rejects tool with missing name" do
      invalid_tool = %{
        description: "A test tool",
        input_schema: %{"type" => "object"},
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "name"
    end

    test "rejects tool with empty name" do
      invalid_tool = %{
        name: "",
        description: "A test tool",
        input_schema: %{"type" => "object"},
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "non-empty string"
    end

    test "rejects tool with non-string name" do
      invalid_tool = %{
        name: 123,
        description: "A test tool",
        input_schema: %{"type" => "object"},
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "non-empty string"
    end

    test "rejects tool with missing description" do
      invalid_tool = %{
        name: "test_tool",
        input_schema: %{"type" => "object"},
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "description"
    end

    test "rejects tool with empty description" do
      invalid_tool = %{
        name: "test_tool",
        description: "",
        input_schema: %{"type" => "object"},
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "non-empty string"
    end

    test "rejects tool with non-string description" do
      invalid_tool = %{
        name: "test_tool",
        description: 123,
        input_schema: %{"type" => "object"},
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "non-empty string"
    end

    test "rejects tool with missing input_schema" do
      invalid_tool = %{
        name: "test_tool",
        description: "A test tool",
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "input_schema"
    end

    test "rejects tool with invalid input_schema type" do
      invalid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: "not a map",
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "input_schema must be a map"
    end

    test "rejects tool with invalid JSON schema structure - properties without object type" do
      invalid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{
          "properties" => %{
            "param1" => %{"type" => "string"}
          }
          # Missing type: "object"
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "type 'object'"
    end

    test "rejects tool with properties and wrong type" do
      invalid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{
          "type" => "string",
          "properties" => %{
            "param1" => %{"type" => "string"}
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "type 'object'"
    end

    test "accepts tool with valid JSON schema - no properties" do
      valid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{
          "type" => "string"
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "accepts tool with valid JSON schema - object with properties" do
      valid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{
          "type" => "object",
          "properties" => %{
            "param1" => %{"type" => "string"},
            "param2" => %{"type" => "number"}
          },
          "required" => ["param1"]
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "rejects tool with invalid callback type" do
      invalid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{"type" => "object"},
        callback: "not a function"
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "function/1"
    end

    test "rejects tool with wrong arity callback" do
      invalid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{"type" => "object"},
        # Wrong arity
        callback: fn -> :ok end
      }

      assert {:error, reason} = MCP.Connection.validate_tool_spec(invalid_tool)
      assert reason =~ "function/1"
    end

    test "accepts tool with nil callback" do
      valid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{"type" => "object"},
        callback: nil
      }

      assert MCP.Connection.validate_tool_spec(valid_tool) == :ok
    end

    test "accepts tool with no callback field" do
      valid_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{"type" => "object"}
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
    test "converts internal format to MCP format" do
      internal_tool = %{
        name: "test_tool",
        description: "A test tool",
        input_schema: %{
          "type" => "object",
          "properties" => %{
            "param1" => %{"type" => "string"}
          }
        },
        callback: fn _args -> {:ok, %{content: []}} end
      }

      marshaled = MCP.Connection.marshal_tool_spec(internal_tool)

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

    test "handles atom keys correctly" do
      internal_tool = %{
        :name => "test_tool",
        :description => "A test tool",
        :input_schema => %{"type" => "object"},
        :callback => fn _args -> {:ok, %{content: []}} end,
        :extra_field => "extra_value"
      }

      marshaled = MCP.Connection.marshal_tool_spec(internal_tool)

      expected = %{
        "name" => "test_tool",
        "description" => "A test tool",
        "inputSchema" => %{"type" => "object"},
        "extra_field" => "extra_value"
      }

      assert marshaled == expected
      refute Map.has_key?(marshaled, "callback")
    end

    test "preserves string keys" do
      internal_tool = %{
        "name" => "test_tool",
        "description" => "A test tool",
        "input_schema" => %{"type" => "object"},
        "callback" => fn _args -> {:ok, %{content: []}} end
      }

      marshaled = MCP.Connection.marshal_tool_spec(internal_tool)

      expected = %{
        "name" => "test_tool",
        "description" => "A test tool",
        "inputSchema" => %{"type" => "object"}
      }

      assert marshaled == expected
      refute Map.has_key?(marshaled, "callback")
    end

    test "handles mixed atom and string keys" do
      internal_tool = %{
        :name => "test_tool",
        "description" => "A test tool",
        :input_schema => %{"type" => "object"},
        "existing_string_key" => "value",
        :callback => fn _args -> {:ok, %{content: []}} end
      }

      marshaled = MCP.Connection.marshal_tool_spec(internal_tool)

      expected = %{
        "name" => "test_tool",
        "description" => "A test tool",
        "inputSchema" => %{"type" => "object"},
        "existing_string_key" => "value"
      }

      assert marshaled == expected
      refute Map.has_key?(marshaled, "callback")
    end

    test "removes callback field regardless of key type" do
      # Test with atom key
      tool_with_atom_callback = %{
        name: "test",
        description: "test",
        input_schema: %{},
        callback: fn _args -> :ok end
      }

      marshaled_atom = MCP.Connection.marshal_tool_spec(tool_with_atom_callback)
      refute Map.has_key?(marshaled_atom, :callback)
      refute Map.has_key?(marshaled_atom, "callback")

      # Test with string key
      tool_with_string_callback = %{
        "name" => "test",
        "description" => "test",
        "input_schema" => %{},
        "callback" => fn _args -> :ok end
      }

      marshaled_string = MCP.Connection.marshal_tool_spec(tool_with_string_callback)
      refute Map.has_key?(marshaled_string, :callback)
      refute Map.has_key?(marshaled_string, "callback")
    end

    test "handles empty tool spec" do
      empty_tool = %{}
      marshaled = MCP.Connection.marshal_tool_spec(empty_tool)
      assert marshaled == %{}
    end

    test "preserves complex nested structures" do
      complex_tool = %{
        name: "complex_tool",
        description: "Complex tool",
        input_schema: %{
          "type" => "object",
          "properties" => %{
            "nested" => %{
              "type" => "object",
              "properties" => %{
                "deep" => %{"type" => "string"}
              }
            },
            "array" => %{
              "type" => "array",
              "items" => %{"type" => "number"}
            }
          },
          "required" => ["nested"]
        },
        callback: fn _args -> {:ok, %{}} end
      }

      marshaled = MCP.Connection.marshal_tool_spec(complex_tool)

      expected = %{
        "name" => "complex_tool",
        "description" => "Complex tool",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "nested" => %{
              "type" => "object",
              "properties" => %{
                "deep" => %{"type" => "string"}
              }
            },
            "array" => %{
              "type" => "array",
              "items" => %{"type" => "number"}
            }
          },
          "required" => ["nested"]
        }
      }

      assert marshaled == expected
      refute Map.has_key?(marshaled, "callback")
    end
  end
end
