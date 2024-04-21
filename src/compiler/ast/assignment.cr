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
    def generate(builder, basic_block, expr : AssignmentStmt) : LLVM::BasicBlock
      # todo: fix to not always create a new variable on creation
      # todo: this is variable declaration codegen, not variable assignment
      start_value, block, start_type = generate(builder, basic_block, expr.initializer)
      alloca_location, _ = @variables[expr.variable]

      # alloca_location = builder.alloca start_value.type, expr.variable
      # case start_value.type.kind
      # when LLVM::Type::Kind::Double
      #   puts "its a double"
      # when LLVM::Type::Kind::Integer
      #   puts "its an integer #{start_value.type.int_width}"
      # else
      #   puts start_value.type.kind
      # end
      # @variables[expr.variable] = {alloca_location, start_value.type}

      builder.position_at_end block
      if start_type.kind == LLVM::Type::Kind::Array
        index = builder.alloca @ctx.int64

        builder.store @ctx.int64.const_int(0), index

        cond_block = @function.basic_blocks.append "arr_cond"
        body_block = @function.basic_blocks.append "arr_body"
        end_block = @function.basic_blocks.append "arr_end"

        builder.br cond_block

        builder.position_at_end cond_block

        index_check = builder.load @ctx.int64, index

        cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(start_type.array_size)

        builder.cond cond, body_block, end_block

        builder.position_at_end body_block

        sp_i = builder.gep start_type, start_value, @ctx.int64.const_int(0), index_check, "gep1"

        s_i = builder.load start_type.element_type, sp_i, "si"

        a_i = builder.gep start_type.element_type, alloca_location, index_check

        builder.store s_i, a_i

        ind = builder.load @ctx.int32, index

        ind1 = builder.add ind, @ctx.int32.const_int(1)

        builder.store ind1, index

        builder.br cond_block

        return end_block
      else
        builder.store start_value, alloca_location

        return block
      end
    end
  end
end
