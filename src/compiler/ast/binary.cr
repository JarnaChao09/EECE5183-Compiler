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

      BitwiseAnd
      BitwiseOr

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
        in .bitwise_and?
          "&"
        in .bitwise_or?
          "|"
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
        in .bitwise_and?
          builder.and l, r, "andtmp"
        in .bitwise_or?
          builder.or l, r, "ortmp"
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
        in .bitwise_and?, .bitwise_or?
          raise "unreachable"
        end
      end

      private def build_bool_operation(builder, current_block, operation : BinaryExpr::Operation, l : LLVM::Value, expr_rhs : Expr) : {LLVM::Value, LLVM::BasicBlock}
        case operation
        in .addition?,
           .subtraction?,
           .multiplication?,
           .division?,
           .less_than?,
           .less_equal?,
           .greater_than?,
           .greater_equal?,
           .equal?,
           .not_equal?
          raise "unreachable"
        in .bitwise_and?
          rhs = @function.basic_blocks.append "and_rhs"
          and = @function.basic_blocks.append "and_end"

          builder.cond l, rhs, and

          builder.position_at_end rhs
          value, block = generate builder, rhs, expr_rhs

          builder.position_at_end block
          builder.br and

          builder.position_at_end and
          phi_and = builder.phi @ctx.int1, [current_block, rhs], [@ctx.int1.const_int(0), value]
          {phi_and, and}
        in .bitwise_or?
          rhs = @function.basic_blocks.append "or_rhs"
          or = @function.basic_blocks.append "or_end"

          builder.cond l, or, rhs

          builder.position_at_end rhs
          value, block = generate builder, rhs, expr_rhs

          builder.position_at_end block
          builder.br or

          builder.position_at_end or
          phi_or = builder.phi @ctx.int1, [current_block, rhs], [@ctx.int1.const_int(1), value]
          {phi_or, or}
        end
      end

      def generate(builder, basic_block, expr : BinaryExpr) : {LLVM::Value, LLVM::BasicBlock}
        l, _ = generate builder, basic_block, expr.lhs

        if (expr.operation.bitwise_and? || expr.operation.bitwise_or?) && l.type.kind == LLVM::Type::Kind::Integer && l.type.int_width == 1
          return build_bool_operation builder, basic_block, expr.operation, l, expr.rhs
        end

        r, _ = generate builder, basic_block, expr.rhs

        case {l.type.kind, r.type.kind}
        when {LLVM::Type::Kind::Integer, LLVM::Type::Kind::Integer}
          ret = build_int_operation builder, expr.operation, l, r
          {ret, basic_block}
        when {LLVM::Type::Kind::Double, LLVM::Type::Kind::Double}
          ret = build_fp_operation builder, expr.operation, l, r
          {ret, basic_block}
        else
          raise "unreachable"
        end
      end
    end
  end
end
