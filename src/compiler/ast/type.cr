module Compiler
  enum Type
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
        ctx.pointer
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
end
