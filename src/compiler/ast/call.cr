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

    def codegen(variables : Hash(String, Float64), functions : Hash(String, Proc(Float64, Float64)))
      functions[@function].call(arguments[0].codegen(variables, functions))
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : CallExpr)
      function_type = @function_types[expr.function]
      builder.call function_type, @mod.functions[expr.function], expr.arguments.map { |arg| generate(builder, arg) }, "calltmp"
    end
  end
end