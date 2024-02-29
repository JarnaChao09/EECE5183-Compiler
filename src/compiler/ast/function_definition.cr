require "./stmt"
require "./function"

module Compiler
  class FunctionDefinitionStmt < Stmt
    property name : String
    property function : Function

    def initialize(@name, @function)
    end

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      functions[@name] = @function
    end

    def to_s(io : IO)
      io << @name << @function
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : FunctionDefinitionStmt) : LLVM::Value
      @ctx.double.const_double(0.0)
      # generate(builder, expr.expression)
    end
  end
end
