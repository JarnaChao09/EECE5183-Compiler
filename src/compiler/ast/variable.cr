require "./expr"

module Compiler
  class VariableExpr < Expr
    property name : String

    def initialize(@name)
    end

    def codegen(variables : Hash(String, Float64), functions : Hash(String, Proc(Float64, Float64)))
      variables[@name]
    end

    def to_s(io : IO)
      io << "(#{@name})"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : VariableExpr) : LLVM::Value
      builder.load @ctx.double, @variables[expr.name]
    end
  end
end
