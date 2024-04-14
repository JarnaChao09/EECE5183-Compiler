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

      # TODO: add logical operators

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

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
      l, r = @lhs.codegen(variables, functions), @rhs.codegen(variables, functions)
      case @operation
      in .addition?
        case {l, r}
        when {String, String}
          l + r
        when {Float64, Float64}
          l + r
        else
          raise "Type Mismatch between #{l} and #{r}"
        end
      in .subtraction?
        case {l, r}
        when {Float64, Float64}
          l - r
        else
          raise "Type Mismatch between #{l} and #{r}"
        end
      in .multiplication?
        case {l, r}
        when {Float64, Float64}
          l * r
        else
          raise "Type Mismatch between #{l} and #{r}"
        end
      in .division?
        case {l, r}
        when {Float64, Float64}
          l / r
        else
          raise "Type Mismatch between #{l} and #{r}"
        end
      end
    end

    def to_s(io : IO)
      io << "(#{@lhs} #{@operation} #{@rhs})"
    end

    class Compiler::CodeGenerator
      private def build_int_operation(builder, operation : BinaryExpr::Operation, l : LLVM::Value, r : LLVM::Value)
        case operation
        in .addition?
          builder.add l, r, "faddtmp"
        in .subtraction?
          builder.sub l, r, "fsubtmp"
        in .multiplication?
          builder.mul l, r, "fmultmp"
        in .division?
          builder.sdiv l, r, "fdivtmp"
        end
      end

      private def build_fp_operation(builder, operation : BinaryExpr::Operation, l : LLVM::Value, r : LLVM::Value)
        case operation
        in .addition?
          builder.fadd l, r, "faddtmp"
        in .subtraction?
          builder.fsub l, r, "fsubtmp"
        in .multiplication?
          builder.fmul l, r, "fmultmp"
        in .division?
          builder.fdiv l, r, "fdivtmp"
        end
      end

      def generate(builder, basic_block, expr : BinaryExpr) : LLVM::Value
        l, r = generate(builder, basic_block, expr.lhs), generate(builder, basic_block, expr.rhs)
        case {l.type.kind, r.type.kind}
        when {LLVM::Type::Kind::Integer, LLVM::Type::Kind::Integer}
          build_int_operation builder, expr.operation, l, r
        when {LLVM::Type::Kind::Double, LLVM::Type::Kind::Double}
          build_fp_operation builder, expr.operation, l, r
        else
          raise "unreachable"
        end
      end
    end
  end
end
