require "../codegen/*"

module Compiler
  abstract class Expr
    abstract def codegen
  end

  class NumberExpr < Expr
    property value : Float64

    def initialize(@value)
    end

    def codegen
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

    def codegen
      l, r = @lhs.codegen, @rhs.codegen
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
