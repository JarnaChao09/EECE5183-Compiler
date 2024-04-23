require "./expr"

module Compiler
  class UnaryExpr < Expr
    property operation : Operation
    property expression : Expr
    enum Operation
      Negation
      BitwiseNot

      def to_s(io : IO)
        io << case self
        in .negation?
          "-"
        in .bit_not?
          "!"
        end
      end
    end

    def initialize(@operation, @expression)
    end

    def to_s(io : IO)
      io << "(#{@operation} #{@expression})"
    end
  end

  class Compiler::CodeGenerator
    private def build_unary_operation(builder, operation : UnaryExpr::Operation, input_type : LLVM::Type) : {Proc(LLVM::Value, LLVM::Value), LLVM::Type}
      case operation
      in .negation?
        case input_type.kind
        when LLVM::Type::Kind::Integer
          if input_type.int_width == 64
            {->builder.neg(LLVM::Value), input_type}
          else
            raise "invalid operation (unreachable)"
          end
        when LLVM::Type::Kind::Double
          {->builder.fneg(LLVM::Value), input_type}
        when LLVM::Type::Kind::Pointer
          raise "invalid operation (unreachable)"
        when LLVM::Type::Kind::Array
          raise "should be unreachable"
        else
          raise "unreachable"
        end
      in .bitwise_not?
        case input_type.kind
        when LLVM::Type::Kind::Integer
          {->builder.not(LLVM::Value), input_type}
        when LLVM::Type::Kind::Double
          raise "invalid operation (unreachable)"
        when LLVM::Type::Kind::Pointer
          raise "invalid operation (unreachable)"
        when LLVM::Type::Kind::Array
          raise "should be unreachable"
        else
          raise "unreachable"
        end
      end
    end

    def generate(builder, basic_block, unary_expr : UnaryExpr) : {LLVM::Value, LLVM::BasicBlock, LLVM::Type}
      expr, block, expr_type = generate builder, basic_block, unary_expr.expression

      builder.position_at_end block

      ret, block = case expr_type.kind
                   when LLVM::Type::Kind::Integer, LLVM::Type::Kind::Double, LLVM::Type::Kind::Pointer
                     builder_operation, ret_type = build_unary_operation builder, unary_expr.operation, expr_type

                     {builder_operation.call(expr), block}
                   when LLVM::Type::Kind::Array
                     builder_operation, holder_element_type = build_unary_operation builder, unary_expr.operation, expr_type.element_type

                     holder_type = holder_element_type.array expr_type.array_size

                     holder = builder.alloca holder_type, "tmp"
                     index = builder.alloca @ctx.int64

                     builder.store @ctx.int64.const_int(0), index

                     cond_block = @function.basic_blocks.append "arr_cond"
                     body_block = @function.basic_blocks.append "arr_body"
                     end_block = @function.basic_blocks.append "arr_end"

                     builder.br cond_block

                     builder.position_at_end cond_block

                     index_check = builder.load @ctx.int64, index

                     cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(expr_type.array_size)

                     builder.cond cond, body_block, end_block

                     builder.position_at_end body_block

                     ep_i = builder.gep expr_type, expr, @ctx.int64.const_int(0), index_check, "gep1"

                     e_i = builder.load expr_type.element_type, ep_i, "ei"

                     tmp = builder_operation.call e_i

                     h_i = builder.gep holder_element_type, holder, index_check

                     s = builder.store tmp, h_i

                     ind = builder.load @ctx.int32, index

                     ind1 = builder.add ind, @ctx.int32.const_int(1)

                     builder.store ind1, index

                     builder.br cond_block

                     {holder, end_block}
                   else
                     raise "unreachable"
                   end

      return {ret, block, expr_type}
    end
  end
end

lib LibLLVM
  fun build_neg = LLVMBuildNeg(BuilderRef, val : ValueRef, name : Char*) : ValueRef
  fun build_fneg = LLVMBuildFNeg(BuilderRef, val : ValueRef, name : Char*) : ValueRef
end

class LLVM::Builder
  def neg(value, name = "")
    Value.new LibLLVM.build_neg(self, value, name)
  end

  def fneg(value, name = "")
    Value.new LibLLVM.build_fneg(self, value, name)
  end
end
