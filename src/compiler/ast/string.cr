require "./expr"

module Compiler
  class StringExpr < Expr
    property value : String

    def initialize(@value)
    end

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      value
    end

    def to_s(io : IO)
      io << value
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : StringExpr) : LLVM::Value
      builder.global_string_pointer(expr.value)
    end
  end
end
