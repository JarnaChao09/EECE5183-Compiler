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
    def generate(builder, basic_block, expr : FunctionDefinitionStmt) : LLVM::BasicBlock
      # generate(builder, expr.expression)
      basic_block
    end
  end
end
