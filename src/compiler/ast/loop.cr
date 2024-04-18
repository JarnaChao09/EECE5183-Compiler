require "./assignment"
require "./stmt"
require "./expr"

module Compiler
  class LoopStmt < Stmt
    property initializer : AssignmentStmt
    property condition : Expr
    property body : Array(Stmt)

    def initialize(@initializer, @condition, @body)
    end

    def to_s(io : IO)
      io << "for(" << @initializer << "; " << @condition << ")"

      body.each do |stmt|
        io << "    " << stmt << "\n"
      end

      io << "end for"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, loop_stmt : LoopStmt) : LLVM::BasicBlock
      assignment_block = generate builder, basic_block, loop_stmt.initializer

      cond_block = @function.basic_blocks.append "for_cond"
      body_block = @function.basic_blocks.append "for_body"
      end_block = @function.basic_blocks.append "for_end"

      builder.br cond_block

      builder.position_at_end cond_block

      cond, cond_block_2 = generate builder, cond_block, loop_stmt.condition

      builder.position_at_end cond_block_2

      builder.cond cond, body_block, end_block

      loop_stmt.body.each do |stmt|
        builder.position_at_end body_block

        body_block = generate builder, body_block, stmt
      end

      builder.position_at_end body_block
      builder.br cond_block

      end_block
    end
  end
end
