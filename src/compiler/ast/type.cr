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

    def to_llvm_type(ctx)
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
  end
end
