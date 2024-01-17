require "./expr"

module Compiler
  class BinaryExpr < Expr
    property lhs : Expr
    property operation : Operation
    property rhs : Expr
    enum Operation
      Addition
      Subtraction
      Multiplication
      Division

      def to_s(io : IO)
        io << case self
        in .addition?
          "+"
        in .subtraction?
          "-"
        in .multiplication?
          "*"
        in .division?
          "/"
        end
      end
    end

    def initialize(@lhs, @operation, @rhs)
    end

    def codegen(variables : Hash(String, Float64), functions : Hash(String, Proc(Float64, Float64)))
      l, r = @lhs.codegen(variables, functions), @rhs.codegen(variables, functions)
      case @operation
      in .addition?
        l + r
      in .subtraction?
        l - r
      in .multiplication?
        l * r
      in .division?
        l / r
      end
    end

    def to_s(io : IO)
      io << "(#{@lhs} #{@operation} #{@rhs})"
    end

    class Compiler::CodeGenerator
      def generate(builder, expr : BinaryExpr) : LLVM::Value
        l, r = generate(builder, expr.lhs), generate(builder, expr.rhs)
        case expr.operation
        in .addition?
          builder.fadd l, r
        in .subtraction?
          builder.fsub l, r
        in .multiplication?
          builder.fmul l, r
        in .division?
          builder.fdiv l, r
        end
      end
    end
  end
end
