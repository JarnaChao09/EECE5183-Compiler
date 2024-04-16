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

      LessThan
      LessEqual
      GreaterThan
      GreaterEqual
      Equal
      NotEqual

      # TODO: add bitwise operators

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
        in .less_than?
          "<"
        in .less_equal?
          "<="
        in .greater_than?
          ">"
        in .greater_equal?
          ">="
        in .equal?
          "=="
        in .not_equal?
          "!="
        end
      end
    end

    def initialize(@lhs, @operation, @rhs)
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
        in .less_than?
          builder.icmp LLVM::IntPredicate::SLT, l, r, "lttmp"
        in .less_equal?
          builder.icmp LLVM::IntPredicate::SLE, l, r, "letmp"
        in .greater_than?
          builder.icmp LLVM::IntPredicate::SGT, l, r, "gttmp"
        in .greater_equal?
          builder.icmp LLVM::IntPredicate::SGE, l, r, "getmp"
        in .equal?
          builder.icmp LLVM::IntPredicate::EQ, l, r, "eqtmp"
        in .not_equal?
          builder.icmp LLVM::IntPredicate::NE, l, r, "netmp"
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
        in .less_than?
          builder.fcmp LLVM::RealPredicate::OLT, l, r, "lttmp"
        in .less_equal?
          builder.fcmp LLVM::RealPredicate::OLE, l, r, "letmp"
        in .greater_than?
          builder.fcmp LLVM::RealPredicate::OGT, l, r, "gttmp"
        in .greater_equal?
          builder.fcmp LLVM::RealPredicate::OGE, l, r, "getmp"
        in .equal?
          builder.fcmp LLVM::RealPredicate::OEQ, l, r, "eqtmp"
        in .not_equal?
          builder.fcmp LLVM::RealPredicate::ONE, l, r, "netmp"
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
