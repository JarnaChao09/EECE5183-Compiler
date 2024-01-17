module Compiler
  enum TokenType
    Program
    Is
    Procedure

    Begin
    End

    Variable
    Global

    Integer
    Float
    String
    Boolean

    If
    Then
    Else
    For

    Return

    Assign

    And
    Or
    Not
    Plus
    Minus
    Star
    Slash

    LT
    LE
    GT
    GE
    EQ
    NE

    LeftParen
    RightParen
    LeftBracket
    RightBracket
    Comma
    Colon
    SemiColon
    Period

    Identifier
    IntegerLiteral
    FloatLiteral
    StringLiteral
    True
    False

    EOF
  end
end
