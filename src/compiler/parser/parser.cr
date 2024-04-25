require "../scanner/*"
require "../ast/*"

module Compiler
  class Parser
    property tokens : Array(Token)
    property current : Int32
    property type_table : Hash(String, TypeType)
    property variables : Hash(String, Type)
    property global_variables : Hash(String, Type)
    property functions : Hash(String, FunctionType)
    property global_functions : Hash(String, FunctionType)
    property binary_operators : Hash(TokenType, Array({Type, Type}))
    property unary_operators : Hash(TokenType, Array(Type))
    property current_return_type : Type | Nil

    def initialize(@tokens)
      @current = 0
      @type_table = {
        "integer" => TypeType::Integer,
        "float"   => TypeType::Double,
        "string"  => TypeType::String,
        "bool"    => TypeType::Boolean,
      }

      @boolean_type = Type.new(TypeType::Boolean)
      @integer_type = Type.new(TypeType::Integer)
      @double_type = Type.new(TypeType::Double)
      @string_type = Type.new(TypeType::String)

      @variables = {} of String => Type
      @global_variables = {} of String => Type
      @functions = {} of String => FunctionType
      @global_functions = {
        "getbool"    => FunctionType.new([] of Type, @boolean_type),
        "getinteger" => FunctionType.new([] of Type, @integer_type),
        "getfloat"   => FunctionType.new([] of Type, @double_type),
        "getstring"  => FunctionType.new([] of Type, @string_type),
        "putbool"    => FunctionType.new([@boolean_type], @boolean_type),
        "putinteger" => FunctionType.new([@integer_type], @boolean_type),
        "putfloat"   => FunctionType.new([@double_type], @boolean_type),
        "putstring"  => FunctionType.new([@string_type], @boolean_type),
        "sqrt"       => FunctionType.new([@integer_type], @double_type),
      }

      @binary_operators = {
        TokenType::Plus  => [{@double_type, @double_type}, {@integer_type, @integer_type}],
        TokenType::Minus => [{@double_type, @double_type}, {@integer_type, @integer_type}],
        TokenType::Star  => [{@double_type, @double_type}, {@integer_type, @integer_type}],
        TokenType::Slash => [{@double_type, @double_type}, {@integer_type, @integer_type}],

        TokenType::And => [{@integer_type, @integer_type}, {@boolean_type, @boolean_type}],
        TokenType::Or  => [{@integer_type, @integer_type}, {@boolean_type, @boolean_type}],

        TokenType::LT => [{@double_type, @double_type}, {@integer_type, @integer_type}, {@boolean_type, @boolean_type}],
        TokenType::LE => [{@double_type, @double_type}, {@integer_type, @integer_type}, {@boolean_type, @boolean_type}],
        TokenType::GT => [{@double_type, @double_type}, {@integer_type, @integer_type}, {@boolean_type, @boolean_type}],
        TokenType::GE => [{@double_type, @double_type}, {@integer_type, @integer_type}, {@boolean_type, @boolean_type}],
        TokenType::EQ => [{@double_type, @double_type}, {@integer_type, @integer_type}, {@boolean_type, @boolean_type}],
        TokenType::NE => [{@double_type, @double_type}, {@integer_type, @integer_type}, {@boolean_type, @boolean_type}],
      }

      @unary_operators = {
        TokenType::Not   => [@integer_type, @boolean_type],
        TokenType::Minus => [@integer_type, @double_type],
      }

      @current_return_type = nil
    end

    def parse
      return program
    end

    private def program
      prog_header = program_header
      program_body_decls, program_body_stmts = program_body

      return Compiler::Program.new(
        prog_header,
        program_body_decls,
        program_body_stmts,
      )
    end

    private def program_header : String
      expect TokenType::Program, "Expected program keyword at the beginning of the program"
      program_id = expect TokenType::Identifier, "Expected program identifier after program keyword"
      expect TokenType::Is, "Expected is keyword after the program identifier"

      return program_id.lexeme
    end

    private def program_body : {Array(Decl), Array(Stmt)}
      decls = [] of Decl
      while !check_current TokenType::Begin
        if decl = declaration true
          decls << decl
        end
      end

      expect TokenType::Begin, "Expected 'begin' to denote the beginning of program body"

      stmts = [] of Stmt
      while !check_current TokenType::End
        stmts << statement
      end

      expect TokenType::End, "Expected 'end' after list of statements"
      expect TokenType::Program, "Expected 'end program' to denote the end the program body"

      # expect TokenType::Period, "Expected 'end program.' to denote the end of the program"

      return {decls, stmts}
    end

    private def declaration(force_global : Bool) : Decl
      is_global = match([TokenType::Global]) || force_global

      ret = case
            when match [TokenType::Variable]
              variable_declaration is_global
            when match [TokenType::Procedure]
              procedure_declaration is_global
            else
              raise "Invalid declaration"
            end

      expect TokenType::SemiColon, "Expected ';' at end of declaration"

      return ret
    end

    private def variable_declaration(is_global : Bool) : Decl
      variable_identifier = expect TokenType::Identifier, "Expected a variable name"

      expect TokenType::Colon, "Expected a ':' after variable name"

      type_identifier = type_mark

      array_size = if match [TokenType::LeftBracket]
                     size = expect TokenType::IntegerLiteral, "Expected a integer literal for array size"
                     expect TokenType::RightBracket, "Expected a ']' after array size"
                     size.lexeme.to_u32
                   end

      complete_type_identifier = Type.new type_identifier, is_global, array_size

      var_holder = if is_global
                     @global_variables
                   else
                     @variables
                   end

      var_holder[variable_identifier.lexeme] = complete_type_identifier

      return VariableDeclaration.new variable_identifier.lexeme, complete_type_identifier, is_global
    end

    private def procedure_declaration(is_global : Bool) : Decl
      procedure_identifier = expect TokenType::Identifier, "Expected a procedure name"

      expect TokenType::Colon, "Expected a ':' after procedure name"

      return_type_identifier = Type.new type_mark

      parameters = [] of VariableDeclaration

      expect TokenType::LeftParen, "Expected a '(' to start the parameter list"

      if !check_current TokenType::RightParen
        loop do
          expect TokenType::Variable, "Expect variable keyword for new parameter"

          parameters << variable_declaration false

          break unless match [TokenType::Comma]
        end
      end

      expect TokenType::RightParen, "Expected ')' after end of parameter list"

      function_decls = [] of Decl

      while !check_current TokenType::Begin
        if decl = declaration false
          function_decls << decl
        end
      end

      expect TokenType::Begin, "Expected 'begin' after declarations"

      stmts = [] of Stmt
      while !check_current TokenType::End
        stmts << statement
      end

      expect TokenType::End, "Expected 'end' after list of statements"
      expect TokenType::Procedure, "Expected 'end procedure' to denote the end of a procedure body"

      func_holder = if is_global
                      @global_functions
                    else
                      @functions
                    end

      func_holder[procedure_identifier.lexeme] = FunctionType.new parameters.map &.variable_type, return_type_identifier

      return FunctionDeclaration.new(
        procedure_identifier.lexeme,
        Function.new(
          parameters,
          return_type_identifier,
          function_decls,
          stmts,
        ),
        is_global
      )
    end

    private def type_mark : TypeType
      type_id = expect TokenType::Identifier, "Expected a valid type identifier"

      if type_identifier = @type_table[type_id.lexeme]
        type_identifier
      else
        raise "#{type_id.lexeme} is an invalid type"
      end
    end

    private def statement : Stmt
      ret = case
            when match [TokenType::If]
              if_statement
            when match [TokenType::For]
              for_statement
            when match [TokenType::Return]
              return_statement
            else
              assignment_statement
            end

      expect TokenType::SemiColon, "Expected ';' after statement"

      return ret
    end

    private def if_statement : Stmt
      expect TokenType::LeftParen, "Expected '(' before conditional expression in if statement"

      condition, condition_type = expression

      expect TokenType::RightParen, "Expected ')' after conditional expression in if statement"
      expect TokenType::Then, "Expected 'then' after conditional expression in if statement"

      then_branch_stmts = [] of Stmt

      while !check_current(TokenType::End) && !check_current(TokenType::Else)
        then_branch_stmts << statement
      end

      else_branch_stmts = [] of Stmt

      if match [TokenType::Else]
        while !match [TokenType::End]
          else_branch_stmts << statement
        end
      else
        expect TokenType::End, "Expected 'end' at the end of an if statement"
      end

      expect TokenType::If, "Expected 'end if' at the end of an if statement"

      return IfStmt.new(
        condition,
        then_branch_stmts,
        else_branch_stmts,
      )
    end

    private def for_statement : Stmt
      expect TokenType::LeftParen, "Expected '(' before initializer in a for loop"

      assignment = assignment_statement

      expect TokenType::SemiColon, "Expected a ';' after initializer in a for loop"

      condition, condition_type = expression

      expect TokenType::RightParen, "Expected a ')' after conditional expression in a for loop"

      for_body = [] of Stmt

      while !match [TokenType::End]
        for_body << statement
      end

      expect TokenType::For, "Expected 'end for' at the end of a for loop"

      return LoopStmt.new(
        assignment,
        condition,
        for_body,
      )
    end

    private def return_statement : Stmt
      return_value, return_type = expression

      return ReturnStmt.new return_value
    end

    private def assignment_statement : Stmt
      dest = expect TokenType::Identifier, "Expected an identifier as assignment destination"

      array_index = if match [TokenType::LeftBracket]
                      index, index_type = expression
                      expect TokenType::RightBracket, "Expected a ']' after array size"
                      index
                    end

      expect TokenType::Assign, "Expected a := after assignment destination"

      value, value_type = expression

      if array_index
        IndexSetStmt.new(
          dest.lexeme,
          array_index,
          value
        )
      else
        AssignmentStmt.new(
          dest.lexeme,
          value
        )
      end
    end

    private def expression : {Expr, Type}
      expression_not
    end

    private def expression_not : {Expr, Type}
      if match [TokenType::Not]
        t = previous.token_type

        operation = UnaryExpr::Operation::BitwiseNot

        ret, type = arithmetic
        return {UnaryExpr.new(operation, ret), type}
      end

      return expression_and_or
    end

    private def expression_and_or : {Expr, Type}
      expr, expr_type = arithmetic

      while match [TokenType::And, TokenType::Or]
        t = previous.token_type
        operation = case t
                    when TokenType::And
                      BinaryExpr::Operation::BitwiseAnd
                    when TokenType::Or
                      BinaryExpr::Operation::BitwiseOr
                    else
                      raise "should be unreachable"
                    end

        right, type = arithmetic
        expr = BinaryExpr.new(expr, operation, right)
      end

      return {expr, expr_type}
    end

    private def arithmetic : {Expr, Type}
      expr, expr_type = relation

      while match [TokenType::Plus, TokenType::Minus]
        t = previous.token_type
        operation = case t
                    when TokenType::Plus
                      BinaryExpr::Operation::Addition
                    when TokenType::Minus
                      BinaryExpr::Operation::Subtraction
                    else
                      raise "should be unreachable"
                    end

        right, type = relation
        expr = BinaryExpr.new(expr, operation, right)
      end

      return {expr, expr_type}
    end

    private def relation : {Expr, Type}
      expr, expr_type = term

      while match [TokenType::LT, TokenType::LE, TokenType::GT, TokenType::GE, TokenType::EQ, TokenType::NE]
        t = previous.token_type

        operation = case t
                    when TokenType::LT
                      BinaryExpr::Operation::LessThan
                    when TokenType::LE
                      BinaryExpr::Operation::LessEqual
                    when TokenType::GT
                      BinaryExpr::Operation::GreaterThan
                    when TokenType::GE
                      BinaryExpr::Operation::GreaterEqual
                    when TokenType::EQ
                      BinaryExpr::Operation::Equal
                    when TokenType::NE
                      BinaryExpr::Operation::NotEqual
                    else
                      raise "should be unreachable"
                    end

        right, type = term
        expr = BinaryExpr.new(expr, operation, right)
      end

      return {expr, expr_type}
    end

    private def term : {Expr, Type}
      expr, expr_type = factor

      while match [TokenType::Star, TokenType::Slash]
        t = previous.token_type
        operation = case t
                    when TokenType::Star
                      BinaryExpr::Operation::Multiplication
                    when TokenType::Slash
                      BinaryExpr::Operation::Division
                    else
                      raise "should be unreachable"
                    end

        right, type = factor
        expr = BinaryExpr.new(expr, operation, right)
      end

      return {expr, expr_type}
    end

    private def factor : {Expr, Type}
      if match [TokenType::Minus]
        t = previous.token_type
        operation = UnaryExpr::Operation::Negation
        ret, type = case
                    when match [TokenType::IntegerLiteral]
                      {IntegerExpr.new(previous.lexeme.to_i64), @integer_type}
                    when match [TokenType::FloatLiteral]
                      {FloatExpr.new(previous.lexeme.to_f64), @double_type}
                    when match [TokenType::Identifier]
                      name
                    else
                      raise "error"
                    end

        return {UnaryExpr.new(operation, ret), type}
      end

      return atom
    end

    private def atom : {Expr, Type}
      return case
      when match [TokenType::True]
        {BooleanExpr.new(true), @boolean_type}
      when match [TokenType::False]
        {BooleanExpr.new(false), @boolean_type}
      when match [TokenType::IntegerLiteral]
        {IntegerExpr.new(previous.lexeme.to_i64), @integer_type}
      when match [TokenType::FloatLiteral]
        {FloatExpr.new(previous.lexeme.to_f64), @double_type}
      when match [TokenType::StringLiteral]
        {StringExpr.new(previous.lexeme[1..-2]), @string_type}
      when match [TokenType::Identifier]
        call_or_name
      when match [TokenType::LeftParen]
        ret, ret_type = expression
        expect TokenType::RightParen, "Expected ')' after expression"
        {ret, ret_type}
      else
        raise "error"
      end
    end

    private def name : {Expr, Type}
      id = previous

      type = @variables[id.lexeme]? || @global_variables[id.lexeme]

      array_index = if match [TokenType::LeftBracket]
                      index, index_type = expression
                      expect TokenType::RightBracket, "Expected a ']' after array size"
                      index
                    end

      return {if array_index
        IndexGetExpr.new(
          id.lexeme,
          array_index
        )
      else
        VariableExpr.new(id.lexeme)
      end, type}
    end

    private def call_or_name : {Expr, Type}
      id = previous

      array_index = if match [TokenType::LeftBracket]
                      index, index_type = expression
                      expect TokenType::RightBracket, "Expected a ']' after array size"
                      index
                    end

      call_args = if match [TokenType::LeftParen]
                    args = [] of Expr
                    if !check_current TokenType::RightParen
                      loop do
                        arg, arg_type = expression

                        args << arg

                        break unless match [TokenType::Comma]
                      end
                    end

                    expect TokenType::RightParen, "Expected a ')' after procedure call argument list"

                    args
                  end

      if array_index
        return {IndexGetExpr.new(
          id.lexeme,
          array_index
        ), @boolean_type}
      end

      if call_args
        return {CallExpr.new(
          id.lexeme,
          call_args
        ), @boolean_type}
      end

      return {VariableExpr.new(id.lexeme), @boolean_type}
    end

    private def is_at_end? : Bool
      return peek.token_type == TokenType::EOF
    end

    private def advance : Token
      if !is_at_end?
        @current += 1
      end

      return previous
    end

    private def peek : Token
      return @tokens[@current]
    end

    private def previous : Token
      return @tokens[@current - 1]
    end

    private def match(ttypes : Array(TokenType)) : Bool
      ttypes.each do |ttype|
        if check_current ttype
          advance
          return true
        end
      end

      return false
    end

    private def check_current(ttype : TokenType) : Bool
      return !is_at_end? && peek.token_type == ttype
    end

    private def expect(ttype : TokenType, message : String) : Token
      return (if check_current ttype
        advance
      else
        raise message
      end)
    end
  end
end
