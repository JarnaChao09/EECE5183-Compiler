require "./expr"

module Compiler
  class CastExpr < Expr
    property expression : Expr
    property to_type : Type

    def initialize(@expression, @to_type)
    end

    def to_s(io : IO)
      io << "cast<#{@to_type.type}>(#{@expression})"
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, expr : CastExpr) : {LLVM::Value, LLVM::BasicBlock, Type}
      value, basic_block, value_type = generate builder, basic_block, expr.expression
      builder.position_at_end basic_block

      llvm_casted_type = expr.to_type.type.to_llvm_type @ctx

      return case {value_type.type, expr.to_type.type}
      in {TypeType::Integer, TypeType::Double}
        {builder.si2fp(value, llvm_casted_type, "casttmp"), basic_block, expr.to_type}
      in {TypeType::Double, TypeType::Integer}
        {builder.fp2si(value, llvm_casted_type, "casttmp"), basic_block, expr.to_type}
      in {TypeType::Boolean, TypeType::Integer}
        {builder.zext(value, llvm_casted_type, "casttmp"), basic_block, expr.to_type}
      in {TypeType::Integer, TypeType::Boolean}
        {builder.icmp(LLVM::IntPredicate::NE, value, @ctx.int64.const_int(0), "casttmp"), basic_block, expr.to_type}
      in {_, _}
        {value, basic_block, value_type}
      end
    end
  end
end
