require "./stmt"
require "./expr"

module Compiler
  class ExpressionStmt < Stmt
    property expression : Expr

    def initialize(@expression)
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : ExpressionStmt) : LLVM::BasicBlock
      _, ret = generate(builder, basic_block, expr.expression)
      ret
    end
  end
end
