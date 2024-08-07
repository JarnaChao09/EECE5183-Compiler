require "./expr"

module Compiler
  private macro define_number_type(number_type_name, number_type, codegen_code, type_kind)
    class {{ number_type_name }}Expr < Expr
      property value : {{ number_type }}

      def initialize(@value)
      end
  
      def to_s(io : IO)
        io << "(#{@value})"
      end
    end

    class Compiler::CodeGenerator
      def generate(builder, basic_block, expr : {{ number_type_name }}Expr) : {LLVM::Value, LLVM::BasicBlock, Type}
        return { {{ codegen_code }} , basic_block, {{ type_kind }} }
      end
    end
  end

  define_number_type Float, Float64, @ctx.double.const_double(expr.value), Type.new(TypeType::Double)
  define_number_type Integer, Int64, @ctx.int64.const_int(expr.value), Type.new(TypeType::Integer)
end
