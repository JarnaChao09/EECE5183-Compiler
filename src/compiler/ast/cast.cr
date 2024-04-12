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

    def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, expr : CastExpr) : LLVM::Value
      value = generate(builder, expr.expression)
      llvm_casted_type = expr.casted_type.to_llvm_type @ctx
      return case {value.type.kind, llvm_casted_type.kind}
      in {LLVM::Type::Kind::Integer, LLVM::Type::Kind::Double}
        builder.si2fp value, llvm_casted_type, "casttmp"
      in {LLVM::Type::Kind::Double, LLVM::Type::Kind::Integer}
        builder.fp2si value, llvm_casted_type, "casttmp"
      in {_, _}
        value
      end
    end
  end
end
