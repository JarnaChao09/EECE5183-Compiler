require "./expr"

module Compiler
  class AssignmentExpr < Expr
    property variable : String
    property initializer : Expr

    def initialize(@variable, @initializer)
    end

    def to_s(io : IO)
      io << "#{variable} = #{initializer}"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : AssignmentExpr) : {LLVM::Value, LLVM::BasicBlock}
      # todo: fix to not always create a new variable on creation
      # todo: this is variable declaration codegen, not variable assignment
      start_value, _ = generate(builder, basic_block, expr.initializer)
      alloca_location = builder.alloca start_value.type, expr.variable
      # case start_value.type.kind
      # when LLVM::Type::Kind::Double
      #   puts "its a double"
      # when LLVM::Type::Kind::Integer
      #   puts "its an integer #{start_value.type.int_width}"
      # else
      #   puts start_value.type.kind
      # end
      @variables[expr.variable] = {alloca_location, start_value.type}
      return {builder.store(start_value, alloca_location), basic_block}
    end
  end
end
