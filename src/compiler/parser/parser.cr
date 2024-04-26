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
    property binary_operators : Hash(TokenType, Array({TypeType, TypeType}))
    property unary_operators : Hash(TokenType, Array(TypeType))
    property current_return_type : Type | Nil
    property errors : Array(Exception)
    property warnings : Array(String)

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
        TokenType::Plus  => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}],
        TokenType::Minus => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}],
        TokenType::Star  => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}],
        TokenType::Slash => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}],

        TokenType::And => [{TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}],
        TokenType::Or  => [{TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}],

        TokenType::LT => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}],
        TokenType::LE => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}],
        TokenType::GT => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}],
        TokenType::GE => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}],
        TokenType::EQ => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}, {TypeType::String, TypeType::String}],
        TokenType::NE => [{TypeType::Double, TypeType::Double}, {TypeType::Integer, TypeType::Integer}, {TypeType::Boolean, TypeType::Boolean}, {TypeType::String, TypeType::String}],
      }

      @unary_operators = {
        TokenType::Not   => [TypeType::Integer, TypeType::Boolean],
        TokenType::Minus => [TypeType::Integer, TypeType::Double],
      }

      @current_return_type = nil

      @errors = [] of Exception
      @warnings = [] of String
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
      decls = declarations true

      expect TokenType::Begin, "Expected 'begin' to denote the beginning of program body"

      stmts = statements

      expect TokenType::End, "Expected 'end' after list of statements"
      expect TokenType::Program, "Expected 'end program' to denote the end the program body"

      if match [TokenType::Period]
      else
        @warnings << "WARNING[PARSER]: Expected 'end program.' to denote the end of the program, missing a '.'"
      end

      return {decls, stmts}
    end

    private def declarations(force_global : Bool) : Array(Decl)
      decls = [] of Decl
      while !is_at_end? && !check_current TokenType::Begin
        begin
          decls << declaration force_global
        rescue ex
          errors << ex

          resync [TokenType::Begin]
        end
      end

      return decls
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

    private def statements : Array(Stmt)
      stmts = [] of Stmt
      while !is_at_end? && !check_current TokenType::End
        begin
          stmts << statement
        rescue ex
          errors << ex

          resync [TokenType::End]
        end
      end

      return stmts
    end

    private def resync(toks : Array(TokenType)) : Nil
      advance

      while !is_at_end?
        if previous.token_type == TokenType::SemiColon
          return
        end

        if toks.includes? peek.token_type
          return
        end

        advance
      end
    end

    private def variable_declaration(is_global : Bool) : Decl
      variable_identifier = expect TokenType::Identifier, "Expected a variable name"

      var_holder = if is_global
                     @global_variables
                   else
                     @variables
                   end

      if var_holder.has_key? variable_identifier.lexeme
        raise "Variable #{variable_identifier.lexeme} has already been declared inside the #{is_global ? "global" : "current"} scope"
      end

      expect TokenType::Colon, "Expected a ':' after variable name"

      type_identifier = type_mark

      array_size = if match [TokenType::LeftBracket]
                     size = expect TokenType::IntegerLiteral, "Expected a integer literal for array size"
                     expect TokenType::RightBracket, "Expected a ']' after array size"
                     size.lexeme.to_u32
                   end

      complete_type_identifier = Type.new type_identifier, is_global, array_size

      var_holder[variable_identifier.lexeme] = complete_type_identifier

      return VariableDeclaration.new variable_identifier.lexeme, complete_type_identifier, is_global
    end

    private def procedure_declaration(is_global : Bool) : Decl
      procedure_identifier = expect TokenType::Identifier, "Expected a procedure name"

      func_holder = if is_global
                      @global_functions
                    else
                      @functions
                    end

      if func_holder.has_key? procedure_identifier.lexeme
        raise "Procedure #{procedure_identifier.lexeme} has already been declared inside the #{is_global ? "global" : "current"} scope"
      end

      expect TokenType::Colon, "Expected a ':' after procedure name"

      return_type_identifier = Type.new type_mark

      prev_return_type = @current_return_type
      @current_return_type = return_type_identifier

      prev_variables = @variables
      @variables = {} of String => Type

      parameters = [] of VariableDeclaration

      expect TokenType::LeftParen, "Expected a '(' to start the parameter list"

      if !check_current TokenType::RightParen
        loop do
          expect TokenType::Variable, "Expect variable keyword for new parameter"

          parameters << variable_declaration false

          break unless match [TokenType::Comma]
        end
      end

      func_type = FunctionType.new parameters.map &.variable_type, return_type_identifier

      func_holder[procedure_identifier.lexeme] = func_type

      prev_functions = @functions
      @functions = {
        procedure_identifier.lexeme => func_type,
      }

      expect TokenType::RightParen, "Expected ')' after end of parameter list"

      function_decls = declarations false

      expect TokenType::Begin, "Expected 'begin' after declarations"

      stmts = statements

      expect TokenType::End, "Expected 'end' after list of statements"
      expect TokenType::Procedure, "Expected 'end procedure' to denote the end of a procedure body"

      @current_return_type = prev_return_type
      @variables = prev_variables
      @functions = prev_functions

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

      eq = check_types condition_type, @boolean_type
      castable = check_types_castable condition_type, @boolean_type

      if !eq && !castable
        raise "Expected condition of if statement to resolve to a boolean compatible type"
      end

      if castable
        condition = create_cast condition, @boolean_type
      end

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

      eq = check_types condition_type, @boolean_type
      castable = check_types_castable condition_type, @boolean_type

      if !eq && !castable
        raise "Expected condition of if statement to resolve to a boolean compatible type"
      end

      if castable
        condition = create_cast condition, @boolean_type
      end

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

      if curr_ret_type = @current_return_type
        eq = check_types return_type, curr_ret_type
        castable = check_types_castable return_type, curr_ret_type

        if !eq && !castable
          raise "Return types do not match"
        end

        if castable
          return_value = create_cast return_value, curr_ret_type
        end
      else
        raise "Return not allowed in top level of the program"
      end

      return ReturnStmt.new return_value
    end

    private def assignment_statement : Stmt
      dest = expect TokenType::Identifier, "Expected an identifier as assignment destination"

      array_index = if match [TokenType::LeftBracket]
                      index, index_type = expression
                      expect TokenType::RightBracket, "Expected a ']' after array size"

                      is_integer_type = check_types index_type, @integer_type

                      if !is_integer_type
                        raise "Array index must be of type integer"
                      end

                      index
                    end

      expect TokenType::Assign, "Expected a := after assignment destination"

      value, value_type = expression

      var_type = @variables[dest.lexeme]? || @global_variables[dest.lexeme]?

      if var_type
        eq = if array_index
               check_types value_type, var_type.copy_with(array_size: nil)
             else
               check_types value_type, var_type
             end
        castable = check_types_castable value_type, var_type

        if !eq && !castable
          raise "Variable #{dest.lexeme} with cannot be assigned to a value of #{value_type}"
        end

        if castable && !array_index
          # array casts are moved to codegen
          value = create_cast value, var_type
        end
      else
        raise "Variable #{dest.lexeme} is not found in scope"
      end

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
        op_types = @unary_operators[t]

        operation = UnaryExpr::Operation::BitwiseNot

        ret, type = arithmetic

        type = if op_types.includes? type.type
                 type
               else
                 raise "Not is only defined on #{op_types.join ", "}"
               end

        return {UnaryExpr.new(operation, ret), type}
      end

      return expression_and_or
    end

    private def expression_and_or : {Expr, Type}
      expr, expr_type = arithmetic

      while match [TokenType::And, TokenType::Or]
        t = previous.token_type
        op_types = @binary_operators[t]

        operation = case t
                    when TokenType::And
                      BinaryExpr::Operation::BitwiseAnd
                    when TokenType::Or
                      BinaryExpr::Operation::BitwiseOr
                    else
                      raise "should be unreachable"
                    end

        right, type = arithmetic

        tmp_type, expr, right = find_type_for_operation operation, op_types, expr, expr_type, right, type
        expr_type = tmp_type

        expr = BinaryExpr.new(expr, operation, right, tmp_type)
      end

      return {expr, expr_type}
    end

    private def arithmetic : {Expr, Type}
      expr, expr_type = relation

      while match [TokenType::Plus, TokenType::Minus]
        t = previous.token_type
        op_types = @binary_operators[t]

        operation = case t
                    when TokenType::Plus
                      BinaryExpr::Operation::Addition
                    when TokenType::Minus
                      BinaryExpr::Operation::Subtraction
                    else
                      raise "should be unreachable"
                    end

        right, type = relation

        tmp_type, expr, right = find_type_for_operation operation, op_types, expr, expr_type, right, type
        expr_type = tmp_type

        expr = BinaryExpr.new(expr, operation, right, expr_type)
      end

      return {expr, expr_type}
    end

    private def relation : {Expr, Type}
      expr, expr_type = term

      while match [TokenType::LT, TokenType::LE, TokenType::GT, TokenType::GE, TokenType::EQ, TokenType::NE]
        t = previous.token_type
        op_types = @binary_operators[t]

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

        tmp_type, expr, right = find_type_for_operation operation, op_types, expr, expr_type, right, type

        array_size = expr_type.array_size || type.array_size
        expr_type = Type.new TypeType::Boolean, false, array_size

        expr = BinaryExpr.new(expr, operation, right, expr_type)
      end

      return {expr, expr_type}
    end

    private def term : {Expr, Type}
      expr, expr_type = factor

      while match [TokenType::Star, TokenType::Slash]
        t = previous.token_type
        op_types = @binary_operators[t]

        operation = case t
                    when TokenType::Star
                      BinaryExpr::Operation::Multiplication
                    when TokenType::Slash
                      BinaryExpr::Operation::Division
                    else
                      raise "should be unreachable"
                    end

        right, type = factor

        tmp_type, expr, right = find_type_for_operation operation, op_types, expr, expr_type, right, type
        expr_type = tmp_type

        expr = BinaryExpr.new(expr, operation, right, expr_type)
      end

      return {expr, expr_type}
    end

    private def factor : {Expr, Type}
      if match [TokenType::Minus]
        t = previous.token_type
        op_types = @unary_operators[t]

        operation = UnaryExpr::Operation::Negation

        ret, type = case
                    when match [TokenType::IntegerLiteral]
                      {IntegerExpr.new(previous.lexeme.to_i64), @integer_type}
                    when match [TokenType::FloatLiteral]
                      {FloatExpr.new(previous.lexeme.to_f64), @double_type}
                    when match [TokenType::Identifier]
                      tmp, tmp_type = name
                      if op_types.includes? tmp_type.type
                        {tmp, tmp_type}
                      else
                        raise "Negation is only defined on #{op_types.join ", "}"
                      end
                    else
                      raise "Invalid use of negation"
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
        {StringExpr.new(previous.lexeme[1...-1]), @string_type}
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

      var_type = @variables[id.lexeme]? || @global_variables[id.lexeme]?

      if !var_type
        raise "Variable #{id.lexeme} is not found in scope"
      end

      return case
      when match [TokenType::LeftBracket]
        if !var_type.array_size
          raise "Variable #{id.lexeme} is not an array and cannot be indexed using the subscript operation"
        end

        index, index_type = expression
        expect TokenType::RightBracket, "Expected a ']' after array size"

        is_integer_type = check_types index_type, @integer_type

        if !is_integer_type
          raise "Array index must be of type integer"
        end

        {IndexGetExpr.new(id.lexeme, index), var_type.copy_with(array_size: nil)}
      else
        {VariableExpr.new(id.lexeme), var_type}
      end
    end

    private def call_or_name : {Expr, Type}
      id = previous

      return case
      when match [TokenType::LeftBracket]
        var_type = @variables[id.lexeme]? || @global_variables[id.lexeme]?

        if !var_type
          raise "Variable #{id.lexeme} is not found in scope"
        end

        if !var_type.array_size
          raise "Variable #{id.lexeme} is not an array and cannot be indexed using the subscript operation"
        end

        index, index_type = expression
        expect TokenType::RightBracket, "Expected a ']' after array size"

        is_integer_type = check_types index_type, @integer_type

        if !is_integer_type
          raise "Array index must be of type integer"
        end

        {IndexGetExpr.new(id.lexeme, index), var_type.copy_with(array_size: nil)}
      when match [TokenType::LeftParen]
        function_type = @functions[id.lexeme]? || @global_functions[id.lexeme]?

        if !function_type
          raise "Procedure #{id.lexeme} is not found in scope"
        end

        args = [] of Expr
        if !check_current TokenType::RightParen
          i = 0
          error = false
          loop do
            arg, arg_type = expression

            if i >= function_type.parameter_types.size
              # while we should check for varargs, no stdlib or custom procedure can be vararg
              # only the hidden underlying C functions can be
            else
              current_param_type = function_type.parameter_types[i]

              are_equal = check_types current_param_type, arg_type

              are_castable = check_types_castable current_param_type, arg_type

              if !are_equal && !are_castable
                raise "#{id.lexeme}: #{arg_type} does not match expected type of #{function_type.parameter_types[i]}"
              end

              if are_castable && !arg_type.array_size
                arg = create_cast arg, current_param_type
              end
            end

            args << arg

            i += 1
            break unless match [TokenType::Comma]
          end
        end

        if args.size != function_type.parameter_types.size
          raise "Invalid number of arguments to #{id.lexeme}, expected #{function_type.parameter_types.size} but got #{args.size}"
        end

        expect TokenType::RightParen, "Expected a ')' after procedure call argument list"

        {CallExpr.new(id.lexeme, args, function_type.parameter_types), function_type.return_type}
      else
        var_type = @variables[id.lexeme]? || @global_variables[id.lexeme]?

        if !var_type
          raise "Variable #{id.lexeme} is not found in scope"
        end

        {VariableExpr.new(id.lexeme), var_type}
      end
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

    private def check_types(type1 : Type, type2 : Type) : Bool
      return type1.type == type2.type && type1.array_size == type2.array_size
    end

    private def check_types_castable(from_type : Type, to_type : Type) : Bool
      if from_type.type == to_type.type
        return false
      end

      return case {from_type.type, to_type.type}
      when {TypeType::Boolean, TypeType::Integer}
        true
      when {TypeType::Integer, TypeType::Boolean}
        true
      when {TypeType::Integer, TypeType::Double}
        true
      when {TypeType::Double, TypeType::Integer}
        true
      else
        false
      end
    end

    private def find_dominating_type(type1 : Type, type2 : Type) : Type
      return case {type1.type, type2.type}
      when {TypeType::Boolean, TypeType::Integer}
        type2
      when {TypeType::Integer, TypeType::Boolean}
        type1
      when {TypeType::Integer, TypeType::Double}
        type2
      when {TypeType::Double, TypeType::Integer}
        type1
      else
        raise "incompatible types #{type1} and #{type2}"
      end
    end

    private def create_cast(expression : Expr, to_type : Type) : CastExpr
      return CastExpr.new expression, to_type
    end

    private def find_type_for_operation(operation, operation_types, lhs : Expr, lhs_type : Type, rhs : Expr, rhs_type : Type) : {Type, Expr, Expr}
      ret_type = if operation_types.includes?({lhs_type.type, rhs_type.type})
                   if lhs_type.array_size && rhs_type.array_size
                     if lhs_type.array_size != rhs_type.array_size
                       raise "Invalid array sizes between #{lhs_type} and #{rhs_type}"
                     end
                     lhs_type
                   elsif lhs_type.array_size && !rhs_type.array_size
                     lhs_type
                   elsif !lhs_type.array_size && rhs_type.array_size
                     rhs_type
                   else
                     lhs_type
                   end
                 else
                   valid_types = operation_types.map &.[0]
                   if !valid_types.includes?(lhs_type.type)
                     raise "Invalid left hand side type #{lhs_type} for #{operation}"
                   end
                   if !valid_types.includes?(rhs_type.type)
                     raise "Invalid right hand side type #{rhs_type} for #{operation}"
                   end

                   dom_type = find_dominating_type lhs_type, rhs_type

                   if lhs_type.array_size && rhs_type.array_size
                     if lhs_type.array_size != rhs_type.array_size
                       raise "Invalid array sizes between #{lhs_type} and #{rhs_type}"
                     end
                     dom_type.copy_with(array_size: lhs_type.array_size)
                   elsif lhs_type.array_size && !rhs_type.array_size
                     dom_type.copy_with(array_size: lhs_type.array_size)
                   elsif !lhs_type.array_size && rhs_type.array_size
                     dom_type.copy_with(array_size: rhs_type.array_size)
                   else
                     if !check_types(lhs_type, dom_type) && check_types_castable(lhs_type, dom_type)
                       lhs = create_cast lhs, dom_type
                     end

                     if !check_types(rhs_type, dom_type) && check_types_castable(rhs_type, dom_type)
                       rhs = create_cast rhs, dom_type
                     end
                     dom_type
                   end
                 end
      return {ret_type, lhs, rhs}
    end
  end
end
