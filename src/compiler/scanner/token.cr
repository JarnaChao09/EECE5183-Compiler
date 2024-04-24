require "./token_type"

module Compiler
  record Token, token_type : TokenType, lexeme : String, line : UInt64, column : UInt64 do
    def to_s(io : IO)
      io << lexeme << " @ " << line << ":" << column
    end
  end
end
