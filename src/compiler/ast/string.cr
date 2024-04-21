require "./expr"

module Compiler
  class StringExpr < Expr
    property value : String

    def initialize(@value)
    end

    def to_s(io : IO)
      io << value
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : StringExpr) : {LLVM::Value, LLVM::BasicBlock, LLVM::Type}
      return {builder.global_string_pointer(expr.value), basic_block, @ctx.int8.pointer}
    end
  end
end
