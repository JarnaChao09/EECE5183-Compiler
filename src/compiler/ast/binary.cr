require "./expr"

module Compiler
  class BinaryExpr < Expr
    property lhs : Expr
    property operation : Operation
    property rhs : Expr
    property type : Type
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

    def initialize(@lhs, @operation, @rhs, @type)
    end

    def to_s(io : IO)
      io << "(#{@lhs} #{@operation} #{@rhs})"
    end
  end

  class Compiler::CodeGenerator
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
            strcmp_function, strcmp_function_type = get_function "strcmp"
            cmp = builder.call strcmp_function_type, strcmp_function, [l, r], "cmptmp"
            builder.icmp LLVM::IntPredicate::EQ, cmp, @ctx.int32.const_int(0), "netmp"
          }
        in .not_equal?
          ->(l : LLVM::Value, r : LLVM::Value) : LLVM::Value {
            strcmp_function, strcmp_function_type = get_function "strcmp"
            cmp = builder.call strcmp_function_type, strcmp_function, [l, r], "cmptmp"
            builder.icmp LLVM::IntPredicate::NE, cmp, @ctx.int32.const_int(0), "netmp"
          }
        end
      else
        raise "unreachable"
      end
    end

    private def cast(builder, value : LLVM::Value, from_type : Type, to_type : Type) : LLVM::Value
      llvm_casted_type = to_type.type.to_llvm_type @ctx

      case {from_type.type, to_type.type}
      in {TypeType::Integer, TypeType::Double}
        builder.si2fp value, llvm_casted_type
      in {TypeType::Double, TypeType::Integer}
        builder.fp2si value, llvm_casted_type
      in {TypeType::Boolean, TypeType::Integer}
        builder.zext value, llvm_casted_type
      in {TypeType::Integer, TypeType::Boolean}
        builder.icmp LLVM::IntPredicate::NE, value, @ctx.int64.const_int(0)
      in {_, _}
        value
      end
    end

    private def find_dominating_type(type1 : Type, type2 : Type) : Type
      return case {type1.type, type2.type}
      when {TypeType::Boolean, TypeType::Integer}
        type2
      when {TypeType::Integer, TypeType::Boolean}
        type1
      when {TypeType::Integer, TypeType::Double}
        type2
      when {TypeType::Double, TypeType::Integer}
        type1
      else
        if type1.type == type2.type
          type1
        else
          raise "incompatible types #{type1} and #{type2}"
        end
      end
    end

    private def build_array_operation(
      builder,
      current_block,
      expression : BinaryExpr
    ) : {LLVM::Value, LLVM::BasicBlock}
      operation = expression.operation
      lhs = expression.lhs
      rhs = expression.rhs
      resulting_type = expression.type

      l, current_block, l_type = generate builder, current_block, lhs
      r, current_block, r_type = generate builder, current_block, rhs

      llvm_l_arrtype = l_type.to_llvm_type @ctx
      llvm_r_arrtype = r_type.to_llvm_type @ctx

      llvm_ret_arrtype = resulting_type.to_llvm_type @ctx
      llvm_ret_elemtype = llvm_ret_arrtype.element_type

      is_l_array = l_type.array_size != nil
      is_r_array = r_type.array_size != nil

      if is_l_array && is_r_array
        llvm_l_elemtype = llvm_l_arrtype.element_type
        llvm_r_elemtype = llvm_r_arrtype.element_type

        cast_type = find_dominating_type l_type.element_type, r_type.element_type
        cast_type_llvm = cast_type.to_llvm_type @ctx

        builder_operation = build_builder_operation builder, operation, cast_type_llvm.kind

        if llvm_l_arrtype.array_size != llvm_r_arrtype.array_size
          raise "array sizes must match (should be unreachable)"
        end

        if llvm_l_arrtype.array_size != llvm_ret_arrtype.array_size
          raise "array sizes must match resulting array size (should be unreachable)"
        end

        llvm_holder_type = llvm_ret_arrtype
        holder = builder.alloca llvm_holder_type, "tmp"
        index = builder.alloca @ctx.int64

        builder.store @ctx.int64.const_int(0), index

        cond_block = @function.basic_blocks.append "arr_cond"
        body_block = @function.basic_blocks.append "arr_body"
        end_block = @function.basic_blocks.append "arr_end"

        builder.br cond_block

        builder.position_at_end cond_block

        index_check = builder.load @ctx.int64, index

        cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_holder_type.array_size)

        builder.cond cond, body_block, end_block

        builder.position_at_end body_block

        lp_i = if l_type.global
                 builder.gep llvm_l_arrtype, l, @ctx.int64.const_int(0), index_check
               else
                 loaded_location = builder.load llvm_l_elemtype.pointer, l
                 builder.gep llvm_l_elemtype, loaded_location, index_check
               end
        rp_i = if r_type.global
                 builder.gep llvm_r_arrtype, r, @ctx.int64.const_int(0), index_check
               else
                 loaded_location = builder.load llvm_r_elemtype.pointer, r
                 builder.gep llvm_r_elemtype, loaded_location, index_check
               end

        l_i = builder.load llvm_l_elemtype, lp_i, "li"
        r_i = builder.load llvm_r_elemtype, rp_i, "ri"

        l_i = cast builder, l_i, l_type.element_type, cast_type
        r_i = cast builder, r_i, r_type.element_type, cast_type

        tmp = builder_operation.call l_i, r_i

        h_i = builder.gep llvm_holder_type.element_type, holder, index_check

        s = builder.store tmp, h_i

        ind = builder.load @ctx.int64, index

        ind1 = builder.add ind, @ctx.int64.const_int(1)

        builder.store ind1, index

        builder.br cond_block

        builder.position_at_end end_block

        stack_location = builder.alloca llvm_holder_type.element_type.pointer

        builder.store holder, stack_location

        {stack_location, end_block}
      elsif is_l_array && !is_r_array
        llvm_l_elemtype = llvm_l_arrtype.element_type

        cast_type = find_dominating_type l_type.element_type, r_type
        cast_type_llvm = cast_type.to_llvm_type @ctx

        builder_operation = build_builder_operation builder, operation, cast_type_llvm.kind

        if llvm_l_arrtype.array_size != llvm_ret_arrtype.array_size
          raise "array size must match resulting array size (should be unreachable)"
        end

        llvm_holder_type = llvm_ret_arrtype
        holder = builder.alloca llvm_holder_type, "tmp"
        index = builder.alloca @ctx.int64

        builder.store @ctx.int64.const_int(0), index

        cond_block = @function.basic_blocks.append "arr_cond"
        body_block = @function.basic_blocks.append "arr_body"
        end_block = @function.basic_blocks.append "arr_end"

        builder.br cond_block

        builder.position_at_end cond_block

        index_check = builder.load @ctx.int64, index

        cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_holder_type.array_size)

        builder.cond cond, body_block, end_block

        builder.position_at_end body_block

        lp_i = if l_type.global
                 builder.gep llvm_l_arrtype, l, @ctx.int64.const_int(0), index_check
               else
                 loaded_location = builder.load llvm_l_elemtype.pointer, l
                 builder.gep llvm_l_elemtype, loaded_location, index_check
               end

        l_i = builder.load llvm_l_elemtype, lp_i, "li"
        r_i = r

        l_i = cast builder, l_i, l_type.element_type, cast_type
        r_i = cast builder, r_i, r_type, cast_type

        tmp = builder_operation.call l_i, r_i

        h_i = builder.gep llvm_holder_type.element_type, holder, index_check

        s = builder.store tmp, h_i

        ind = builder.load @ctx.int64, index

        ind1 = builder.add ind, @ctx.int64.const_int(1)

        builder.store ind1, index

        builder.br cond_block

        builder.position_at_end end_block

        stack_location = builder.alloca llvm_holder_type.element_type.pointer

        builder.store holder, stack_location

        {stack_location, end_block}
      elsif !is_l_array && is_r_array
        llvm_r_elemtype = llvm_r_arrtype.element_type
        cast_type = find_dominating_type l_type, r_type.element_type
        cast_type_llvm = cast_type.to_llvm_type @ctx

        builder_operation = build_builder_operation builder, operation, cast_type_llvm.kind

        if llvm_r_arrtype.array_size != llvm_ret_arrtype.array_size
          raise "array size must match resulting array size (should be unreachable)"
        end

        llvm_holder_type = llvm_ret_arrtype
        holder = builder.alloca llvm_holder_type, "tmp"
        index = builder.alloca @ctx.int64

        builder.store @ctx.int64.const_int(0), index

        cond_block = @function.basic_blocks.append "arr_cond"
        body_block = @function.basic_blocks.append "arr_body"
        end_block = @function.basic_blocks.append "arr_end"

        builder.br cond_block

        builder.position_at_end cond_block

        index_check = builder.load @ctx.int64, index

        cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_holder_type.array_size)

        builder.cond cond, body_block, end_block

        builder.position_at_end body_block

        # rp_i = builder.gep llvm_r_arrtype, r, @ctx.int64.const_int(0), index_check, "gep2"

        rp_i = if r_type.global
                 builder.gep llvm_r_arrtype, r, @ctx.int64.const_int(0), index_check
               else
                 loaded_location = builder.load llvm_r_elemtype.pointer, r
                 builder.gep llvm_r_elemtype, loaded_location, index_check
               end

        l_i = l
        r_i = builder.load llvm_r_elemtype, rp_i, "ri"

        l_i = cast builder, l_i, l_type, cast_type
        r_i = cast builder, r_i, r_type.element_type, cast_type

        tmp = builder_operation.call l_i, r_i

        h_i = builder.gep llvm_holder_type.element_type, holder, index_check

        s = builder.store tmp, h_i

        ind = builder.load @ctx.int64, index

        ind1 = builder.add ind, @ctx.int64.const_int(1)

        builder.store ind1, index

        builder.br cond_block

        builder.position_at_end end_block

        stack_location = builder.alloca llvm_holder_type.element_type.pointer

        builder.store holder, stack_location

        {stack_location, end_block}
      else
        raise "unreachable"
      end
    end

    private def build_bool_sc_operations(builder, basic_block, operation : BinaryExpr::Operation) : Proc(Expr, Expr, {LLVM::Value, LLVM::BasicBlock})
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
        ->(expr_lhs : Expr, expr_rhs : Expr) : {LLVM::Value, LLVM::BasicBlock} {
          l, block, l_type = generate builder, basic_block, expr_lhs

          builder.position_at_end block

          rhs = @function.basic_blocks.append "and_rhs"
          and = @function.basic_blocks.append "and_end"

          builder.cond l, rhs, and

          builder.position_at_end rhs
          value, block, rhs_type = generate builder, rhs, expr_rhs

          builder.position_at_end block
          builder.br and

          builder.position_at_end and
          phi_and = builder.phi @ctx.int1, [basic_block, rhs], [@ctx.int1.const_int(0), value]

          {phi_and, and}
        }
      in .bitwise_or?
        ->(expr_lhs : Expr, expr_rhs : Expr) : {LLVM::Value, LLVM::BasicBlock} {
          l, block, l_type = generate builder, basic_block, expr_lhs

          builder.position_at_end block

          rhs = @function.basic_blocks.append "or_rhs"
          or = @function.basic_blocks.append "or_end"

          builder.cond l, or, rhs

          builder.position_at_end rhs
          value, block, rhs_type = generate builder, rhs, expr_rhs

          builder.position_at_end block
          builder.br or

          builder.position_at_end or
          phi_or = builder.phi @ctx.int1, [basic_block, rhs], [@ctx.int1.const_int(1), value]

          {phi_or, or}
        }
      end
    end

    private def build_operations(builder, basic_block, operation : BinaryExpr::Operation, lhs : Expr, rhs : Expr) : {LLVM::Value, LLVM::BasicBlock}
      l, basic_block, l_type = generate builder, basic_block, lhs
      r, basic_block, r_type = generate builder, basic_block, rhs

      value = case {l_type.type, r_type.type}
              when {TypeType::Boolean, TypeType::Boolean}
                case operation
                in .addition?,
                   .subtraction?,
                   .multiplication?,
                   .division?
                  raise "unreachable"
                in .less_than?
                  builder.icmp LLVM::IntPredicate::ULT, l, r
                in .less_equal?
                  builder.icmp LLVM::IntPredicate::ULE, l, r
                in .greater_than?
                  builder.icmp LLVM::IntPredicate::UGT, l, r
                in .greater_equal?
                  builder.icmp LLVM::IntPredicate::UGE, l, r
                in .equal?
                  builder.icmp LLVM::IntPredicate::EQ, l, r
                in .not_equal?
                  builder.icmp LLVM::IntPredicate::NE, l, r
                in .bitwise_and?,
                   .bitwise_or?
                  raise "unreachable"
                end
              when {TypeType::Integer, TypeType::Integer}
                case operation
                in .addition?
                  builder.add l, r
                in .subtraction?
                  builder.sub l, r
                in .multiplication?
                  builder.mul l, r
                in .division?
                  builder.sdiv l, r
                in .less_than?
                  builder.icmp LLVM::IntPredicate::SLT, l, r
                in .less_equal?
                  builder.icmp LLVM::IntPredicate::SLE, l, r
                in .greater_than?
                  builder.icmp LLVM::IntPredicate::SGT, l, r
                in .greater_equal?
                  builder.icmp LLVM::IntPredicate::SGE, l, r
                in .equal?
                  builder.icmp LLVM::IntPredicate::EQ, l, r
                in .not_equal?
                  builder.icmp LLVM::IntPredicate::NE, l, r
                in .bitwise_and?
                  builder.and l, r
                in .bitwise_or?
                  builder.or l, r
                end
              when {TypeType::Double, TypeType::Double}
                case operation
                in .addition?
                  builder.fadd l, r
                in .subtraction?
                  builder.fsub l, r
                in .multiplication?
                  builder.fmul l, r
                in .division?
                  builder.fdiv l, r
                in .less_than?
                  builder.fcmp LLVM::RealPredicate::OLT, l, r
                in .less_equal?
                  builder.fcmp LLVM::RealPredicate::OLE, l, r
                in .greater_than?
                  builder.fcmp LLVM::RealPredicate::OGT, l, r
                in .greater_equal?
                  builder.fcmp LLVM::RealPredicate::OGE, l, r
                in .equal?
                  builder.fcmp LLVM::RealPredicate::OEQ, l, r
                in .not_equal?
                  builder.fcmp LLVM::RealPredicate::ONE, l, r
                in .bitwise_and?,
                   .bitwise_or?
                  raise "unreachable"
                end
              when {TypeType::String, TypeType::String}
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
                  cmp = builder.call strcmp_function_type, strcmp_function, [l, r]
                  builder.icmp LLVM::IntPredicate::EQ, cmp, @ctx.int32.const_int(0)
                in .not_equal?
                  strcmp_function, strcmp_function_type = get_function "strcmp"
                  cmp = builder.call strcmp_function_type, strcmp_function, [l, r]
                  builder.icmp LLVM::IntPredicate::NE, cmp, @ctx.int32.const_int(0)
                end
              else
                raise "unreachable"
              end

      return {value, basic_block}
    end

    def generate(builder, basic_block, expr : BinaryExpr) : {LLVM::Value, LLVM::BasicBlock, Type}
      expected_type = expr.type

      ret, basic_block = if expected_type.array_size
                           build_array_operation builder, basic_block, expr
                         else
                           if expected_type.type == TypeType::Boolean && (expr.operation.bitwise_and? || expr.operation.bitwise_or?)
                             # short circuiting
                             op = build_bool_sc_operations builder, basic_block, expr.operation
                             op.call expr.lhs, expr.rhs
                           elsif expected_type.type == TypeType::Boolean
                             # relation operations
                             build_operations builder, basic_block, expr.operation, expr.lhs, expr.rhs
                           elsif expected_type.type == TypeType::Integer
                             # integer operations
                             build_operations builder, basic_block, expr.operation, expr.lhs, expr.rhs
                           elsif expected_type.type == TypeType::Double
                             # floating point operations
                             build_operations builder, basic_block, expr.operation, expr.lhs, expr.rhs
                           elsif expected_type.type == TypeType::String
                             raise "No binary operation results in strings"
                           else
                             raise "unreachable"
                           end
                         end

      return {ret, basic_block, expected_type.copy_with(global: false)}
    end
  end
end
