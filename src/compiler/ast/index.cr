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
      # TODO: add bounds check

      index, block = generate builder, basic_block, index_get.index

      builder.position_at_end block

      alloca_location, var_type = @variables[index_get.variable]? || @global_variables[index_get.variable]

      array_location = builder.gep var_type, alloca_location, @ctx.int64.const_int(0), index
      ret = builder.load var_type.element_type, array_location

      return {ret, block, var_type.element_type}
    end

    def generate(builder, basic_block, index_set : IndexSetStmt) : LLVM::BasicBlock
      # TODO: add bounds check

      init, block = generate builder, basic_block, index_set.initializer

      builder.position_at_end block

      index, block = generate builder, block, index_set.index

      builder.position_at_end block

      alloca_location, var_type = @variables[index_set.variable]? || @global_variables[index_set.variable]

      array_location = builder.gep var_type, alloca_location, @ctx.int64.const_int(0), index

      builder.store init, array_location

      return block
    end
  end
end
