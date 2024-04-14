require "./expr"

module Compiler
  class VariableExpr < Expr
    property name : String

    def initialize(@name)
    end

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      variables[@name]
    end

    def to_s(io : IO)
      io << "(#{@name})"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : VariableExpr) : LLVM::Value
      variable, variable_type = @variables[expr.name]
      builder.load variable_type, variable, expr.name
    end
  end
end
