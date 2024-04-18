require "./decl"
require "./function"

module Compiler
  class FunctionDeclaration < Decl
    property name : String
    property function : Function
    property is_global : Bool

    def initialize(@name, @function, @is_global)
    end

    def is_global?
      @is_global
    end

    def to_s(io : IO)
      io << @name << @function
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : FunctionDeclaration) : LLVM::BasicBlock
      raise "todo"
      # generate(builder, expr.expression)
      # basic_block
    end
  end
end
