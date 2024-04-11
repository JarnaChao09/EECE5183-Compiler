require "./expr"

module Compiler
  private macro define_number_type(number_type_name, number_type, codegen_code)
    class {{ number_type_name }}Expr < Expr
      property value : {{ number_type }}

      def initialize(@value)
      end

      def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
        @value
      end
  
      def to_s(io : IO)
        io << "(#{@value})"
      end

      class Compiler::CodeGenerator
        def generate(builder, expr : {{ number_type_name }}Expr) : LLVM::Value
          {{ codegen_code }}
        end
      end
    end
  end

  define_number_type Float, Float64, @ctx.double.const_double(expr.value)
  define_number_type Integer, Int64, @ctx.int64.const_int(expr.value)
end
