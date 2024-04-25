require "./expr"

module Compiler
  class CallExpr < Expr
    property function : String
    property arguments : Array(Expr)

    def initialize(@function, @arguments)
    end

    def to_s(io : IO)
      io << "#{function}(#{arguments.join(", ")})"
    end
  end

  class Compiler::CodeGenerator
    private def return_type_to_type(return_type : LLVM::Type) : Type
      case return_type.kind
      when LLVM::Type::Kind::Integer
        if return_type.int_width == 1
          Type.new TypeType::Boolean
        else
          Type.new TypeType::Integer
        end
      when LLVM::Type::Kind::Double
        Type.new TypeType::Double
      when LLVM::Type::Kind::Pointer
        Type.new TypeType::String
      else
        raise "unreachable return type #{return_type}"
      end
    end

    private def array_copy(builder, basic_block, call_arg, call_arg_type) : {LLVM::Value, LLVM::BasicBlock}
      llvm_call_type = call_arg_type.to_llvm_type @ctx

      builder.position_at_end basic_block

      tmp = builder.alloca llvm_call_type, "tmp"
      index = builder.alloca @ctx.int64, "index"

      builder.store @ctx.int64.const_int(0), index

      cond_block = @function.basic_blocks.append "copy_cond"
      body_block = @function.basic_blocks.append "copy_body"
      end_block = @function.basic_blocks.append "copy_end"

      builder.br cond_block

      builder.position_at_end cond_block

      index_check = builder.load @ctx.int64, index

      cond = builder.icmp LLVM::IntPredicate::SLT, index_check, @ctx.int64.const_int(llvm_call_type.array_size)

      builder.cond cond, body_block, end_block

      builder.position_at_end body_block

      sp_i = builder.gep llvm_call_type, call_arg, @ctx.int64.const_int(0), index_check

      s_i = builder.load llvm_call_type.element_type, sp_i

      a_i = builder.gep llvm_call_type.element_type, tmp, index_check

      builder.store s_i, a_i

      ind = builder.load @ctx.int64, index

      ind1 = builder.add ind, @ctx.int64.const_int(1)

      builder.store ind1, index

      builder.br cond_block

      builder.position_at_end end_block

      ret = builder.gep llvm_call_type, tmp, @ctx.int64.const_int(0), @ctx.int64.const_int(0), "decay"

      return {ret, end_block}
    end

    def generate(builder, basic_block, expr : CallExpr) : {LLVM::Value, LLVM::BasicBlock, Type}
      mangled_function_name = @function_names[expr.function]? || @global_function_names[expr.function]

      function_type = @function_types[mangled_function_name]? || @global_function_types[mangled_function_name]

      function_param_types = function_type.params_types

      ret_block = basic_block

      args = expr.arguments.map do |arg|
        builder.position_at_end ret_block
        call_arg, ret_block, call_type = generate builder, ret_block, arg
        if array_size = call_type.array_size
          decay, ret_block = array_copy builder, ret_block, call_arg, call_type
          decay
        else
          call_arg
        end
      end

      builder.position_at_end ret_block

      return_type = return_type_to_type function_type.return_type

      return {builder.call(function_type, @mod.functions[mangled_function_name], args, "calltmp"), ret_block, return_type}
    end
  end
end
