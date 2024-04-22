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
    def generate(builder, basic_block, expr : CallExpr) : {LLVM::Value, LLVM::BasicBlock, LLVM::Type}
      mangled_function_name = @function_names[expr.function]? || @global_function_names[expr.function]

      function_type = @function_types[mangled_function_name]? || @global_function_types[mangled_function_name]

      function_param_types = function_type.params_types

      ret_block = basic_block

      return {builder.call(function_type, @mod.functions[mangled_function_name], expr.arguments.map { |arg|
        call_arg, ret_block = generate builder, basic_block, arg
        call_arg
      }, "calltmp"), ret_block, function_type.return_type}
    end
  end
end
