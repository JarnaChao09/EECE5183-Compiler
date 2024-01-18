require "./expr"

module Compiler
  class AssignmentExpr < Expr
    property variable : String
    property initializer : Expr

    def initialize(@variable, @initializer)
    end

    def codegen(variables : Hash(String, Float64), functions : Hash(String, Function))
      value = @initializer.codegen variables, functions
      variables[@variable] = value
      value
    end

    def to_s(io : IO)
      io << "#{variable} = #{initializer}"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : AssignmentExpr) : LLVM::Value
      alloca_location = builder.alloca @ctx.double, expr.variable
      @variables[expr.variable] = alloca_location
      start_value = generate(builder, expr.initializer)
      builder.store start_value, alloca_location
    end
  end
end
