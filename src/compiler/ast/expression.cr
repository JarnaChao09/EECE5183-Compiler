require "./stmt"
require "./expr"

module Compiler
  class ExpressionStmt < Stmt
    property expression : Expr

    def initialize(@expression)
    end

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      expression.codegen(variables, functions)
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : ExpressionStmt) : LLVM::Value
      generate(builder, expr.expression)
    end
  end
end
