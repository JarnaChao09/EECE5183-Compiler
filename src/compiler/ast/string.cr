require "./expr"

module Compiler
  class StringExpr < Expr
    property value : String

    def initialize(@value)
    end

    def codegen(variables : Hash(String, Float64), functions : Hash(String, Function))
      0.0
    end

    def to_s(io : IO)
      io << value
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : StringExpr) : LLVM::Value
      builder.global_string_pointer(expr.value)
    end
  end
end
