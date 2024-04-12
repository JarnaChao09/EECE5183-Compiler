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
    def generate(builder, expr : CallExpr)
      function_type = @function_types[expr.function]
      function_param_types = function_type.params_types
      builder.call function_type, @mod.functions[expr.function], expr.arguments.map { |arg|
        generate builder, arg
      }, "calltmp"
    end
  end
end
