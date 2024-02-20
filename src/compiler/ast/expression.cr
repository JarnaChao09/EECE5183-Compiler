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
end
