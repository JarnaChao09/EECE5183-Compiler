require "./expr"

module Compiler
  class BooleanExpr < Expr
    property value : Bool

    def initialize(@value)
    end

    def to_s(io : IO)
      io << value
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : BooleanExpr) : {LLVM::Value, LLVM::BasicBlock}
      return {@ctx.int1.const_int(expr.value ? 1 : 0), basic_block}
    end
  end
end