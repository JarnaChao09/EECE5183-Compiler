require "./stmt"

module Compiler
  class AssignmentStmt < Stmt
    property variable : String
    property initializer : Expr

    def initialize(@variable, @initializer)
    end

    def to_s(io : IO)
      io << "#{variable} = #{initializer}"
    end
  end

  class Compiler::CodeGenerator
    private def array_copy(builder, basic_block, dest, dest_type, call_arg, call_arg_type) : LLVM::BasicBlock
      llvm_call_type = call_arg_type.to_llvm_type @ctx
      llvm_dest_type = dest_type.to_llvm_type @ctx

      builder.position_at_end basic_block

      index = builder.alloca @ctx.int64, "index"

      builder.store @ctx.int64.const_int(0), index

      cond_block = @function.basic_blocks.append "copy_cond"
      body_block = @function.basic_blocks.append "copy_body"
      end_block = @function.basic_blocks.append "copy_end"

      builder.br cond_block

      builder.position_at_end cond_block

      index_check = builder.load @ctx.int64, index

      cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_call_type.array_size)

      builder.cond cond, body_block, end_block

      builder.position_at_end body_block

      sp_i = if call_arg_type.global
               builder.gep llvm_call_type, call_arg, @ctx.int64.const_int(0), index_check
             else
               loaded_location = builder.load llvm_call_type.element_type.pointer, call_arg
               builder.gep llvm_call_type.element_type, loaded_location, index_check
             end

      s_i = builder.load llvm_call_type.element_type, sp_i

      a_i = if dest_type.global
              builder.gep llvm_dest_type, dest, @ctx.int64.const_int(0), index_check
            else
              loaded_location = builder.load llvm_dest_type.element_type.pointer, dest
              builder.gep llvm_dest_type.element_type, loaded_location, index_check
            end

      builder.store s_i, a_i

      ind = builder.load @ctx.int64, index

      ind1 = builder.add ind, @ctx.int64.const_int(1)

      builder.store ind1, index

      builder.br cond_block

      builder.position_at_end end_block

      return end_block
    end

    def generate(builder, basic_block, expr : AssignmentStmt) : LLVM::BasicBlock
      start_value, block, start_type = generate builder, basic_block, expr.initializer
      dest, dest_type = @variables[expr.variable]? || @global_variables[expr.variable]

      builder.position_at_end block

      llvm_start_type = start_type.to_llvm_type @ctx

      if llvm_start_type.kind == LLVM::Type::Kind::Array
        array_copy builder, block, dest, dest_type, start_value, start_type
      else
        builder.store start_value, dest

        return block
      end
    end
  end
end
