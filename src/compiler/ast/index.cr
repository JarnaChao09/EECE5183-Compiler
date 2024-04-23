require "./expr"
require "./stmt"

module Compiler
  class IndexGetExpr < Expr
    property variable : String
    property index : Expr

    def initialize(@variable, @index)
    end

    def to_s(io : IO)
      io << @variable << "[" << @index << "]"
    end
  end

  class IndexSetStmt < Stmt
    property variable : String
    property index : Expr
    property initializer : Expr

    def initialize(@variable, @index, @initializer)
    end

    def to_s(io : IO)
      io << @variable << "[" << @index << "]"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, index_get : IndexGetExpr) : {LLVM::Value, LLVM::BasicBlock, LLVM::Type}
      index, block, _ = generate builder, basic_block, index_get.index

      builder.position_at_end block

      alloca_location, var_type = @variables[index_get.variable]? || @global_variables[index_get.variable]

      zero = @ctx.int64.const_int 0
      max = @ctx.int64.const_int var_type.array_size

      rhs = @function.basic_blocks.append "bc_rhs"
      and = @function.basic_blocks.append "bc_end"

      bc_lhs = builder.icmp LLVM::IntPredicate::SLE, zero, index

      builder.cond bc_lhs, rhs, and

      builder.position_at_end rhs
      bc_rhs = builder.icmp LLVM::IntPredicate::SLT, index, max
      builder.br and

      builder.position_at_end and
      phi_and = builder.phi @ctx.int1, [basic_block, rhs], [@ctx.int1.const_int(0), bc_rhs]

      fail_block = @function.basic_blocks.append "fail"
      index_block = @function.basic_blocks.append "index"

      builder.cond phi_and, index_block, fail_block

      builder.position_at_end fail_block

      oobe_function, oobe_function_type = get_function "oobe"

      builder.call oobe_function_type, oobe_function, [index, max]

      builder.br index_block

      builder.position_at_end index_block

      array_location = builder.gep var_type, alloca_location, @ctx.int64.const_int(0), index
      ret = builder.load var_type.element_type, array_location

      return {ret, index_block, var_type.element_type}
    end

    def generate(builder, basic_block, index_set : IndexSetStmt) : LLVM::BasicBlock
      init, block = generate builder, basic_block, index_set.initializer

      builder.position_at_end block

      index, block = generate builder, block, index_set.index

      builder.position_at_end block

      alloca_location, var_type = @variables[index_set.variable]? || @global_variables[index_set.variable]

      zero = @ctx.int64.const_int 0
      max = @ctx.int64.const_int var_type.array_size

      rhs = @function.basic_blocks.append "bc_rhs"
      and = @function.basic_blocks.append "bc_end"

      bc_lhs = builder.icmp LLVM::IntPredicate::SLE, zero, index

      builder.cond bc_lhs, rhs, and

      builder.position_at_end rhs
      bc_rhs = builder.icmp LLVM::IntPredicate::SLT, index, max
      builder.br and

      builder.position_at_end and
      phi_and = builder.phi @ctx.int1, [basic_block, rhs], [@ctx.int1.const_int(0), bc_rhs]

      fail_block = @function.basic_blocks.append "fail"
      index_block = @function.basic_blocks.append "index"

      builder.cond phi_and, index_block, fail_block

      builder.position_at_end fail_block

      oobe_function, oobe_function_type = get_function "oobe"

      builder.call oobe_function_type, oobe_function, [index, max]

      builder.br index_block

      builder.position_at_end index_block

      array_location = builder.gep var_type, alloca_location, @ctx.int64.const_int(0), index

      builder.store init, array_location

      return index_block
    end
  end
end
