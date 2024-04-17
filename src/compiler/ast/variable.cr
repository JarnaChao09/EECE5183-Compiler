require "./expr"

module Compiler
  class VariableExpr < Expr
    property name : String

    def initialize(@name)
    end

    def to_s(io : IO)
      io << "(#{@name})"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : VariableExpr) : {LLVM::Value, LLVM::BasicBlock}
      variable, variable_type = @variables[expr.name]
      return {builder.load(variable_type, variable, expr.name), basic_block}
    end
  end
end
