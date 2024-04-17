require "./expr"

module Compiler
  class CastExpr < Expr
    property expression : Expr
    property casted_type : Type

    def initialize(@expression, @casted_type)
    end

    def to_s(io : IO)
      io << "cast<#{@casted_type.kind}>(#{@expression})"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : CastExpr) : {LLVM::Value, LLVM::BasicBlock}
      value, basic_block = generate builder, basic_block, expr.expression
      llvm_casted_type = expr.casted_type.to_llvm_type @ctx
      return {case {value.type.kind, llvm_casted_type.kind}
      in {LLVM::Type::Kind::Integer, LLVM::Type::Kind::Double}
        builder.si2fp value, llvm_casted_type, "casttmp"
      in {LLVM::Type::Kind::Double, LLVM::Type::Kind::Integer}
        builder.fp2si value, llvm_casted_type, "casttmp"
      in {_, _}
        value
      end, basic_block}
    end
  end
end
