require "./stmt"

module Compiler
  class IfStmt < Stmt
    property condition : Expr
    property then_branch : Array(Stmt)
    property else_branch : Array(Stmt)

    def initialize(@condition, @then_branch, @else_branch)
    end

    def to_s(io : IO)
      io << "if (#{@condition}) then"
      then_branch.each do |branch_statement|
        io << "    #{branch_statement}"
      end
      if else_branch.size != 0
        io << "else"
        else_branch.each do |branch_statement|
          io << "    #{branch_statement}"
        end
      end

      io << "end if"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, stmt : IfStmt) : LLVM::BasicBlock
      cond = generate builder, basic_block, stmt.condition

      builder.position_at_end basic_block

      then_basic_block = @function.basic_blocks.append "if_then"
      else_basic_block = @function.basic_blocks.append "if_else"
      end_basic_block = @function.basic_blocks.append "if_end"

      builder.cond cond, then_basic_block, else_basic_block

      stmt.then_branch.each do |then_stmt|
        builder.position_at_end then_basic_block

        then_basic_block = generate builder, then_basic_block, then_stmt
      end

      builder.position_at_end then_basic_block
      builder.br end_basic_block

      stmt.else_branch.each do |else_stmt|
        builder.position_at_end else_basic_block

        else_basic_block = generate builder, else_basic_block, else_stmt
      end

      builder.position_at_end else_basic_block
      builder.br end_basic_block

      end_basic_block
    end
  end
end
