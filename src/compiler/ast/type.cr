module Compiler
  enum TypeType
    Boolean
    Integer
    Double
    String

    def to_s(io : IO)
      io << case self
      in .boolean?
        "boolean"
      in .integer?
        "integer"
      in .double?
        "double"
      in .string?
        "string"
      end
    end

    def to_llvm_type(ctx) : LLVM::Type
      case self
      in .boolean?
        ctx.int1
      in .integer?
        ctx.int64
      in .double?
        ctx.double
      in .string?
        ctx.int8.pointer
      end
    end

    def to_llvm_default_value(ctx) : LLVM::Value
      case self
      in .boolean?
        ctx.int1.const_int 0
      in .integer?
        ctx.int64.const_int 0
      in .double?
        ctx.double.const_double 0.0
      in .string?
        ctx.pointer.null_pointer
      end
    end
  end

  # NOTE: global member on the following record is currently only for use when trying to load
  # arrays. GEP instructions will change depending on if the load is to a local array (ptr ptr)
  # or a global array (ptr).
  # global is put onto the variable's type so that it may propogate up the codegeneration stack
  # this is currently only used in assignment.cr
  #
  # index.cr could be refactored to utilize this information instead of calculating local on the fly
  # from table lookups
  record Type, type : TypeType, global : Bool = false, array_size : UInt32 | Nil = nil do
    def element_type : Type
      if array_size
        return Type.new type
      else
        raise "Not an Array Type"
      end
    end

    def to_llvm_type(ctx) : LLVM::Type
      ret = type.to_llvm_type ctx

      if arr_size = array_size
        ret = ret.array arr_size
      end

      return ret
    end

    def to_s(io : IO)
      io << type
      if array_size
        io << "[" << array_size << "]"
      end
    end
  end

  record FunctionType, parameter_types : Array(Type), return_type : Type, vararg : Bool = false do
    def to_llvm_function_type(ctx) : LLVM::Type
      LLVM::Type.function parameter_types.map { |e| e.to_llvm_type ctx }, return_type.to_llvm_type(ctx), vararg
    end
  end
end
