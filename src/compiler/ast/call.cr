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
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : CallExpr) : LLVM::Value
      function_type = @function_types[expr.function]
      function_param_types = function_type.params_types
      builder.call function_type, @mod.functions[expr.function], expr.arguments.map { |arg|
        generate builder, basic_block, arg
      }, "calltmp"
    end
  end
end
