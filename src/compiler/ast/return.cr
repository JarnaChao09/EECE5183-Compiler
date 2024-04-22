require "./stmt"

module Compiler
  class ReturnStmt < Stmt
    # property token
    property expression : Expr

    def initialize(@expression)
    end

    def to_s(io : IO)
      io << "return " << @expression
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, return_stmt : ReturnStmt) : LLVM::BasicBlock
      builder.position_at_end basic_block

      val, block = generate builder, basic_block, return_stmt.expression

      builder.position_at_end block

      builder.ret val

      block
    end
  end
end
