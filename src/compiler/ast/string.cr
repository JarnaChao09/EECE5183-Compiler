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
    def generate(builder, basic_block, expr : StringExpr) : {LLVM::Value, LLVM::BasicBlock, Type}
      return {builder.global_string_pointer(expr.value), basic_block, Type.new(TypeType::String)}
    end
  end
end
