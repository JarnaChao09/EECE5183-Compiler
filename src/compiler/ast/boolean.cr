require "./expr"

module Compiler
  class BooleanExpr < Expr
    property value : Bool

    def initialize(@value)
    end

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      value
    end

    def to_s(io : IO)
      io << value
    end
  end

  # class Compiler::CodeGenerator
  #   def generate(builder, expr : StringExpr) : LLVM::Value
  #   end
  # end
end
