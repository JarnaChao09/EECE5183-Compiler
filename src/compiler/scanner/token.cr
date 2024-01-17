require "./token_type"

module Compiler
  record Token, token_type : TokenType, lexeme : String, line : UInt64, column : UInt64
end
