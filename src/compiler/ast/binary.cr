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
         .division?
        raise "unreachable"
      in .less_than?,
         .less_equal?,
         .greater_than?,
         .greater_equal?,
         .equal?,
         .not_equal?
        raise "todo"
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

    private def build_string_operation(builder, operation : BinaryExpr::Operation, l : LLVM::Value, r : LLVM::Value) : LLVM::Value
      case operation
      in .addition?,
         .subtraction?,
         .multiplication?,
         .division?,
         .less_than?,
         .less_equal?,
         .greater_than?,
         .greater_equal?,
         .bitwise_and?,
         .bitwise_or?
        raise "unreachable"
      in .equal?
        strcmp_function, strcmp_function_type = get_function "strcmp"
        cmp = builder.call strcmp_function_type, strcmp_function, [l, r], "cmptmp"
        builder.icmp LLVM::IntPredicate::EQ, cmp, @ctx.int32.const_int(0), "netmp"
      in .not_equal?
        strcmp_function, strcmp_function_type = get_function "strcmp"
        cmp = builder.call strcmp_function_type, strcmp_function, [l, r], "cmptmp"
        builder.icmp LLVM::IntPredicate::NE, cmp, @ctx.int32.const_int(0), "netmp"
      end
    end

    private def build_builder_operation(builder, operation : BinaryExpr::Operation, resulting_type : LLVM::Type::Kind) : Proc(LLVM::Value, LLVM::Value, LLVM::Value)
      case resulting_type
      when LLVM::Type::Kind::Integer
        case operation
        in .addition?
          ->builder.add(LLVM::Value, LLVM::Value)
        in .subtraction?
          ->builder.sub(LLVM::Value, LLVM::Value)
        in .multiplication?
          ->builder.mul(LLVM::Value, LLVM::Value)
        in .division?
          ->builder.sdiv(LLVM::Value, LLVM::Value)
        in .less_than?
          (->builder.icmp(LLVM::IntPredicate, LLVM::Value, LLVM::Value)).partial LLVM::IntPredicate::SLT
        in .less_equal?
          (->builder.icmp(LLVM::IntPredicate, LLVM::Value, LLVM::Value)).partial LLVM::IntPredicate::SLE
        in .greater_than?
          (->builder.icmp(LLVM::IntPredicate, LLVM::Value, LLVM::Value)).partial LLVM::IntPredicate::SGT
        in .greater_equal?
          (->builder.icmp(LLVM::IntPredicate, LLVM::Value, LLVM::Value)).partial LLVM::IntPredicate::SGE
        in .equal?
          (->builder.icmp(LLVM::IntPredicate, LLVM::Value, LLVM::Value)).partial LLVM::IntPredicate::EQ
        in .not_equal?
          (->builder.icmp(LLVM::IntPredicate, LLVM::Value, LLVM::Value)).partial LLVM::IntPredicate::NE
        in .bitwise_and?
          ->builder.and(LLVM::Value, LLVM::Value)
        in .bitwise_or?
          ->builder.or(LLVM::Value, LLVM::Value)
        end
      when LLVM::Type::Kind::Double
        case operation
        in .addition?
          ->builder.fadd(LLVM::Value, LLVM::Value)
        in .subtraction?
          ->builder.fsub(LLVM::Value, LLVM::Value)
        in .multiplication?
          ->builder.fmul(LLVM::Value, LLVM::Value)
        in .division?
          ->builder.fdiv(LLVM::Value, LLVM::Value)
        in .less_than?
          (->builder.fcmp(LLVM::RealPredicate, LLVM::Value, LLVM::Value)).partial LLVM::RealPredicate::OLT
        in .less_equal?
          (->builder.fcmp(LLVM::RealPredicate, LLVM::Value, LLVM::Value)).partial LLVM::RealPredicate::OLE
        in .greater_than?
          (->builder.fcmp(LLVM::RealPredicate, LLVM::Value, LLVM::Value)).partial LLVM::RealPredicate::OGT
        in .greater_equal?
          (->builder.fcmp(LLVM::RealPredicate, LLVM::Value, LLVM::Value)).partial LLVM::RealPredicate::OGE
        in .equal?
          (->builder.fcmp(LLVM::RealPredicate, LLVM::Value, LLVM::Value)).partial LLVM::RealPredicate::OEQ
        in .not_equal?
          (->builder.fcmp(LLVM::RealPredicate, LLVM::Value, LLVM::Value)).partial LLVM::RealPredicate::ONE
        in .bitwise_and?, .bitwise_or?
          raise "unreachable"
        end
        # when LLVM::Type::Kind::Boolean
        #   case operation
        #   in .addition?, .subtraction?, .multiplication?, .division?
        #     raise "unreachable"
        #   in .less_than?
        #     raise "todo"
        #   in .less_equal?
        #     raise "todo"
        #   in .greater_than?
        #     raise "todo"
        #   in .greater_equal?
        #     raise "todo"
        #   in .equal?
        #     raise "todo"
        #   in .not_equal?
        #     raise "todo"
        #   in .bitwise_and?
        #     raise "handled prior"
        #   in .bitwise_or?
        #     raise "handled prior"
        #   end
      when LLVM::Type::Kind::Pointer
        case operation
        in .addition?,
           .subtraction?,
           .multiplication?,
           .division?,
           .less_than?,
           .less_equal?,
           .greater_than?,
           .greater_equal?,
           .bitwise_and?,
           .bitwise_or?
          raise "unreachable"
        in .equal?
          ->(l : LLVM::Value, r : LLVM::Value) : LLVM::Value {
            strcmp_function = @mod.functions["strcmp"]
            strcmp_function_type = @function_types["strcmp"]
            cmp = builder.call strcmp_function_type, strcmp_function, [l, r], "cmptmp"
            builder.icmp LLVM::IntPredicate::EQ, cmp, @ctx.int32.const_int(0), "netmp"
          }
        in .not_equal?
          ->(l : LLVM::Value, r : LLVM::Value) : LLVM::Value {
            strcmp_function = @mod.functions["strcmp"]
            strcmp_function_type = @function_types["strcmp"]
            cmp = builder.call strcmp_function_type, strcmp_function, [l, r], "cmptmp"
            builder.icmp LLVM::IntPredicate::NE, cmp, @ctx.int32.const_int(0), "netmp"
          }
        end
      else
        raise "unreachable"
      end
    end

    private def build_array_operation(
      builder,
      current_block,
      operation : BinaryExpr::Operation,
      l : LLVM::Value,
      l_type : Type,
      r : LLVM::Value,
      r_type : Type
    ) : {LLVM::Value, LLVM::BasicBlock, Type}
      llvm_l_type = l_type.to_llvm_type @ctx
      llvm_r_type = r_type.to_llvm_type @ctx
      if llvm_l_type.kind == LLVM::Type::Kind::Array && llvm_r_type.kind == LLVM::Type::Kind::Array
        builder_operation = build_builder_operation builder, operation, llvm_l_type.element_type.kind
        if llvm_l_type.array_size != llvm_l_type.array_size
          raise "array sizes must match (should be unreachable)"
        end

        # TODO: should casting be moved here? should cast nodes handle arrays?
        if llvm_l_type.element_type.kind != llvm_r_type.element_type.kind
          raise "arrays must be of the same type (should be unreachable)"
        end

        holder_type = case operation
                      in .less_than?,
                         .less_equal?,
                         .greater_than?,
                         .greater_equal?,
                         .equal?,
                         .not_equal?
                        Type.new TypeType::Boolean, false, llvm_l_type.array_size.to_u32
                      in .addition?,
                         .subtraction?,
                         .multiplication?,
                         .division?,
                         .bitwise_and?,
                         .bitwise_or?
                        l_type
                      end

        llvm_holder_type = holder_type.to_llvm_type @ctx
        holder = builder.alloca llvm_holder_type, "tmp"
        index = builder.alloca @ctx.int64

        builder.store @ctx.int64.const_int(0), index

        cond_block = @function.basic_blocks.append "arr_cond"
        body_block = @function.basic_blocks.append "arr_body"
        end_block = @function.basic_blocks.append "arr_end"

        builder.br cond_block

        builder.position_at_end cond_block

        index_check = builder.load @ctx.int64, index

        cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_l_type.array_size)

        builder.cond cond, body_block, end_block

        builder.position_at_end body_block

        lp_i = builder.gep llvm_l_type, l, @ctx.int64.const_int(0), index_check, "gep1"
        rp_i = builder.gep llvm_r_type, r, @ctx.int64.const_int(0), index_check, "gep2"

        l_i = builder.load llvm_l_type.element_type, lp_i, "li"
        r_i = builder.load llvm_r_type.element_type, rp_i, "ri"

        tmp = builder_operation.call l_i, r_i

        h_i = builder.gep llvm_holder_type.element_type, holder, index_check

        s = builder.store tmp, h_i

        ind = builder.load @ctx.int32, index

        ind1 = builder.add ind, @ctx.int32.const_int(1)

        builder.store ind1, index

        builder.br cond_block

        {holder, end_block, holder_type}
      elsif llvm_l_type.kind == LLVM::Type::Kind::Array && llvm_r_type.kind != LLVM::Type::Kind::Array
        builder_operation = build_builder_operation builder, operation, llvm_l_type.element_type.kind

        # TODO: should casting be moved here? should cast nodes handle arrays?
        if llvm_l_type.element_type.kind != llvm_r_type.kind
          raise "arrays must be of the same type (should be unreachable)"
        end

        holder_type = case operation
                      in .less_than?,
                         .less_equal?,
                         .greater_than?,
                         .greater_equal?,
                         .equal?,
                         .not_equal?
                        Type.new TypeType::Boolean, false, llvm_l_type.array_size.to_u32
                      in .addition?,
                         .subtraction?,
                         .multiplication?,
                         .division?,
                         .bitwise_and?,
                         .bitwise_or?
                        l_type
                      end

        llvm_holder_type = holder_type.to_llvm_type @ctx
        holder = builder.alloca llvm_holder_type, "tmp"
        index = builder.alloca @ctx.int64

        builder.store @ctx.int64.const_int(0), index

        cond_block = @function.basic_blocks.append "arr_cond"
        body_block = @function.basic_blocks.append "arr_body"
        end_block = @function.basic_blocks.append "arr_end"

        builder.br cond_block

        builder.position_at_end cond_block

        index_check = builder.load @ctx.int64, index

        cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_l_type.array_size)

        builder.cond cond, body_block, end_block

        builder.position_at_end body_block

        lp_i = builder.gep llvm_l_type, l, @ctx.int64.const_int(0), index_check, "gep1"

        l_i = builder.load llvm_l_type.element_type, lp_i, "li"
        r_i = r

        tmp = builder_operation.call l_i, r_i

        h_i = builder.gep llvm_holder_type.element_type, holder, index_check

        s = builder.store tmp, h_i

        ind = builder.load @ctx.int32, index

        ind1 = builder.add ind, @ctx.int32.const_int(1)

        builder.store ind1, index

        builder.br cond_block

        {holder, end_block, holder_type}
      elsif llvm_l_type.kind != LLVM::Type::Kind::Array && llvm_r_type.kind == LLVM::Type::Kind::Array
        builder_operation = build_builder_operation builder, operation, llvm_r_type.element_type.kind

        # TODO: should casting be moved here? should cast nodes handle arrays?
        if llvm_l_type.kind != llvm_r_type.element_type.kind
          raise "arrays must be of the same type (should be unreachable)"
        end

        holder_type = case operation
                      in .less_than?,
                         .less_equal?,
                         .greater_than?,
                         .greater_equal?,
                         .equal?,
                         .not_equal?
                        Type.new TypeType::Boolean, false, llvm_r_type.array_size.to_u32
                      in .addition?,
                         .subtraction?,
                         .multiplication?,
                         .division?,
                         .bitwise_and?,
                         .bitwise_or?
                        r_type
                      end

        llvm_holder_type = holder_type.to_llvm_type @ctx
        holder = builder.alloca llvm_holder_type, "tmp"
        index = builder.alloca @ctx.int64

        builder.store @ctx.int64.const_int(0), index

        cond_block = @function.basic_blocks.append "arr_cond"
        body_block = @function.basic_blocks.append "arr_body"
        end_block = @function.basic_blocks.append "arr_end"

        builder.br cond_block

        builder.position_at_end cond_block

        index_check = builder.load @ctx.int64, index

        cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_r_type.array_size)

        builder.cond cond, body_block, end_block

        builder.position_at_end body_block

        rp_i = builder.gep llvm_r_type, r, @ctx.int64.const_int(0), index_check, "gep2"

        l_i = l
        r_i = builder.load llvm_r_type.element_type, rp_i, "ri"

        tmp = builder_operation.call l_i, r_i

        h_i = builder.gep llvm_holder_type.element_type, holder, index_check

        s = builder.store tmp, h_i

        ind = builder.load @ctx.int32, index

        ind1 = builder.add ind, @ctx.int32.const_int(1)

        builder.store ind1, index

        builder.br cond_block

        {holder, end_block, holder_type}
      else
        raise "unreachable"
      end
    end

    def generate(builder, basic_block, expr : BinaryExpr) : {LLVM::Value, LLVM::BasicBlock, Type}
      l, block, l_type = generate builder, basic_block, expr.lhs

      builder.position_at_end block

      if (expr.operation.bitwise_and? || expr.operation.bitwise_or?) && l.type.kind == LLVM::Type::Kind::Integer && l.type.int_width == 1
        value, block = build_bool_operation builder, block, expr.operation, l, expr.rhs
        return {value, block, Type.new(TypeType::Boolean)}
      end

      r, block, r_type = generate builder, block, expr.rhs

      builder.position_at_end block

      case {l.type.kind, r.type.kind}
      when {LLVM::Type::Kind::Integer, LLVM::Type::Kind::Integer}
        ret = build_int_operation builder, expr.operation, l, r
        {ret, block, l_type}
      when {LLVM::Type::Kind::Double, LLVM::Type::Kind::Double}
        ret = build_fp_operation builder, expr.operation, l, r
        {ret, block, l_type}
      when {LLVM::Type::Kind::Pointer, LLVM::Type::Kind::Pointer}
        # assuming pointers are just strings
        ret = build_string_operation builder, expr.operation, l, r
        {ret, block, l_type}
      when {LLVM::Type::Kind::Array, _}, {_, LLVM::Type::Kind::Array}
        build_array_operation builder, block, expr.operation, l, l_type, r, r_type
      else
        raise "unreachable"
      end
    end
  end
end
