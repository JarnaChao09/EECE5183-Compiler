require "./expr"
require "./function"

module Compiler
  class FunctionDefinitionExpr < Expr
    property name : String
    property function : Function

    def initialize(@name, @function)
    end

    def codegen(variables : Hash(String, Float64), functions : Hash(String, Function))
      functions[@name] = @function
      0.0
    end

    def to_s(io : IO)
      io << @name << @function
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : FunctionDefinitionExpr) : LLVM::Value
      @ctx.int32.const_int(-1)
    end
  end
end
