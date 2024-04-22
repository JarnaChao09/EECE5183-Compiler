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
      cond, cond_block = generate builder, basic_block, stmt.condition

      builder.position_at_end cond_block

      then_branch_ret = false
      else_branch_ret = false

      then_basic_block = @function.basic_blocks.append "if_then"
      else_basic_block = @function.basic_blocks.append "if_else"

      builder.cond cond, then_basic_block, else_basic_block

      stmt.then_branch.each do |then_stmt|
        builder.position_at_end then_basic_block

        if then_stmt.is_a? Compiler::ReturnStmt
          then_branch_ret = true
        end

        then_basic_block = generate builder, then_basic_block, then_stmt
      end

      stmt.else_branch.each do |else_stmt|
        builder.position_at_end else_basic_block

        if else_stmt.is_a? Compiler::ReturnStmt
          else_branch_ret = true
        end

        else_basic_block = generate builder, else_basic_block, else_stmt
      end

      if then_branch_ret && else_branch_ret
        return else_basic_block
      elsif then_branch_ret && stmt.else_branch.empty?
        return else_basic_block
      else
        end_basic_block = @function.basic_blocks.append "if_end"

        if !then_branch_ret
          builder.position_at_end then_basic_block
          builder.br end_basic_block
        end

        if !else_branch_ret
          builder.position_at_end else_basic_block
          builder.br end_basic_block
        end

        return end_basic_block
      end
    end
  end
end
