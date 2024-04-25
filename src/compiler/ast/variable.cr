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
    def generate(builder, basic_block, expr : VariableExpr) : {LLVM::Value, LLVM::BasicBlock, Type}
      variable, variable_type = @variables[expr.name]? || @global_variables[expr.name]
      if variable_type.array_size
        return {variable, basic_block, variable_type}
      else
        return {builder.load(variable_type.type.to_llvm_type(@ctx), variable, expr.name), basic_block, variable_type}
      end
    end
  end
end
