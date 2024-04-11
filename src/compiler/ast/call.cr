require "./expr"

module Compiler
  class CallExpr < Expr
    property function : String
    property arguments : Array(Expr)

    def initialize(@function, @arguments)
    end

    def to_s(io : IO)
      io << "#{function}(#{arguments.join(", ")})"
    end

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      if @function == "printf"
        puts arguments.map { |e| e.codegen(variables, functions) }
        return 0.0
      end
      functions[@function].call(arguments.map { |argument| argument.codegen(variables, functions) }, variables, functions)
    end
  end

  class Compiler::CodeGenerator
    private def perform_implicit_cast(builder, value : LLVM::Value, expected_type : LLVM::Type) : LLVM::Value
      return case {value.type.kind, expected_type.kind}
      in {LLVM::Type::Kind::Integer, LLVM::Type::Kind::Double}
        builder.si2fp value, expected_type, "casttmp"
      in {_, _}
        value
      end
    end

    def generate(builder, expr : CallExpr)
      function_type = @function_types[expr.function]
      function_param_types = function_type.params_types
      builder.call function_type, @mod.functions[expr.function], expr.arguments.map_with_index { |arg, index|
        generated_arg = generate builder, arg
        if index < function_param_types.size
          perform_implicit_cast builder, generated_arg, function_param_types[index]
        else
          generated_arg
        end
      }, "calltmp"
    end
  end
end
