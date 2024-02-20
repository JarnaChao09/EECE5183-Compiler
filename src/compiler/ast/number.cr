require "./expr"

module Compiler
  class NumberExpr < Expr
    property value : Float64

    def initialize(@value)
    end

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      @value
    end

    def to_s(io : IO)
      io << "(#{@value})"
    end

    class Compiler::CodeGenerator
      def generate(builder, expr : NumberExpr) : LLVM::Value
        @ctx.double.const_double(expr.value)
      end
    end
  end
end
