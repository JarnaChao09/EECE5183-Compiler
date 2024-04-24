require "../scanner/*"
require "../ast/*"

module Compiler
  class Parser
    property tokens : Array(Token)
    property current : Int32
    property type_table : Hash(String, Type)

    def initialize(@tokens)
      @current = 0
      @type_table = {
        "integer" => Type::Integer,
        "float"   => Type::Double,
        "string"  => Type::String,
        "bool"    => Type::Boolean,
      }
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

      return VariableDeclaration.new(variable_identifier.lexeme, type_identifier, is_global, array_size)
    end

    private def procedure_declaration(is_global : Bool) : Decl
      procedure_identifier = expect TokenType::Identifier, "Expected a procedure name"

      expect TokenType::Colon, "Expected a ':' after procedure name"

      return_type_identifier = type_mark

      parameter_names = [] of String
      parameter_types = [] of Type

      expect TokenType::LeftParen, "Expected a '(' to start the parameter list"

      if !check_current TokenType::RightParen
        loop do
          expect TokenType::Variable, "Expect variable keyword for new parameter"

          variable_decl = variable_declaration false

          parameter_names << variable_decl.variable
          parameter_types << variable_decl.variable_type

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

      return FunctionDeclaration.new(
        procedure_identifier.lexeme,
        Function.new(
          parameter_names,
          parameter_types,
          return_type_identifier,
          function_decls,
          stmts,
        ),
        is_global
      )
    end

    private def type_mark : Type
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

      condition = expression

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

      condition = expression

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
      return_value = expression

      return ReturnStmt.new return_value
    end

    private def assignment_statement : Stmt
      dest = expect TokenType::Identifier, "Expected an identifier as assignment destination"

      array_index = if match [TokenType::LeftBracket]
                      index = expression
                      expect TokenType::RightBracket, "Expected a ']' after array size"
                      index
                    end

      expect TokenType::Assign, "Expected a := after assignment destination"

      value = expression

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

    private def expression : Expr
      expression_not
    end

    private def expression_not : Expr
      if match [TokenType::Not]
        operation = UnaryExpr::Operation::BitwiseNot
        return UnaryExpr.new(operation, arithmetic)
      end

      return expression_and_or
    end

    private def expression_and_or : Expr
      expr = arithmetic

      while match [TokenType::And, TokenType::Or]
        operation = case previous.token_type
                    when TokenType::And
                      BinaryExpr::Operation::BitwiseAnd
                    when TokenType::Or
                      BinaryExpr::Operation::BitwiseOr
                    else
                      raise "should be unreachable"
                    end

        expr = BinaryExpr.new(expr, operation, arithmetic)
      end

      return expr
    end

    private def arithmetic : Expr
      expr = relation

      while match [TokenType::Plus, TokenType::Minus]
        operation = case previous.token_type
                    when TokenType::Plus
                      BinaryExpr::Operation::Addition
                    when TokenType::Minus
                      BinaryExpr::Operation::Subtraction
                    else
                      raise "should be unreachable"
                    end

        expr = BinaryExpr.new(expr, operation, relation)
      end

      return expr
    end

    private def relation : Expr
      expr = term

      while match [TokenType::LT, TokenType::LE, TokenType::GT, TokenType::GE, TokenType::EQ, TokenType::NE]
        operation = case previous.token_type
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

        expr = BinaryExpr.new(expr, operation, term)
      end

      return expr
    end

    private def term : Expr
      expr = factor

      while match [TokenType::Star, TokenType::Slash]
        operation = case previous.token_type
                    when TokenType::Star
                      BinaryExpr::Operation::Multiplication
                    when TokenType::Slash
                      BinaryExpr::Operation::Division
                    else
                      raise "should be unreachable"
                    end

        expr = BinaryExpr.new(expr, operation, factor)
      end

      return expr
    end

    private def factor : Expr
      if match [TokenType::Minus]
        operation = UnaryExpr::Operation::Negation
        ret = case
              when match [TokenType::IntegerLiteral]
                IntegerExpr.new previous.lexeme.to_i64
              when match [TokenType::FloatLiteral]
                FloatExpr.new previous.lexeme.to_f64
              when match [TokenType::Identifier]
                name
              else
                raise "error"
              end

        return UnaryExpr.new operation, ret
      end

      return atom
    end

    private def atom : Expr
      return case
      when match [TokenType::True]
        BooleanExpr.new true
      when match [TokenType::False]
        BooleanExpr.new false
      when match [TokenType::IntegerLiteral]
        IntegerExpr.new previous.lexeme.to_i64
      when match [TokenType::FloatLiteral]
        FloatExpr.new previous.lexeme.to_f64
      when match [TokenType::StringLiteral]
        StringExpr.new previous.lexeme[1..-2]
      when match [TokenType::Identifier]
        call_or_name
      when match [TokenType::LeftParen]
        ret = expression
        expect TokenType::RightParen, "Expected ')' after expression"
        ret
      else
        raise "error"
      end
    end

    private def name : Expr
      id = previous

      array_index = if match [TokenType::LeftBracket]
                      index = expression
                      expect TokenType::RightBracket, "Expected a ']' after array size"
                      index
                    end

      if array_index
        IndexGetExpr.new(
          id.lexeme,
          array_index
        )
      else
        VariableExpr.new(id.lexeme)
      end
    end

    private def call_or_name : Expr
      id = previous

      array_index = if match [TokenType::LeftBracket]
                      index = expression
                      expect TokenType::RightBracket, "Expected a ']' after array size"
                      index
                    end

      call_args = if match [TokenType::LeftParen]
                    args = [] of Expr
                    if !check_current TokenType::RightParen
                      loop do
                        args << expression

                        break unless match [TokenType::Comma]
                      end
                    end

                    expect TokenType::RightParen, "Expected a ')' after procedure call argument list"

                    args
                  end

      if array_index
        return IndexGetExpr.new(
          id.lexeme,
          array_index
        )
      end

      if call_args
        return CallExpr.new(
          id.lexeme,
          call_args
        )
      end

      return VariableExpr.new(id.lexeme)
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
