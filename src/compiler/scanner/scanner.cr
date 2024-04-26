require "./token"
require "./token_type"

module Compiler
  class Scanner
    @source : String
    @line_count : UInt64
    @column_count : UInt64
    @start : UInt64
    @current : UInt64
    @tokens : Array(Token)

    @keywords : Hash(String, TokenType)

    property errors : Array(Exception)

    def initialize(@source)
      @line_count = 1
      @column_count = 1
      @start = 0
      @current = 0

      @tokens = [] of Token

      @keywords = {
        "program"   => TokenType::Program,
        "is"        => TokenType::Is,
        "procedure" => TokenType::Procedure,
        "begin"     => TokenType::Begin,
        "end"       => TokenType::End,
        "variable"  => TokenType::Variable,
        "global"    => TokenType::Global,
        "integer"   => TokenType::Identifier,
        "float"     => TokenType::Identifier,
        "string"    => TokenType::Identifier,
        "bool"      => TokenType::Identifier,
        "if"        => TokenType::If,
        "then"      => TokenType::Then,
        "else"      => TokenType::Else,
        "for"       => TokenType::For,
        "return"    => TokenType::Return,
        "not"       => TokenType::Not,
        "true"      => TokenType::True,
        "false"     => TokenType::False,
      }

      @errors = [] of Exception
    end

    def tokens
      if @tokens.empty?
        while is_not_at_end?
          begin
            @tokens << scan_token
          rescue ex
            @errors << ex
          end
        end
      end

      @tokens
    end

    private def is_at_end?(dist = 0)
      (@current + dist) >= @source.size
    end

    private def is_not_at_end?(dist = 0)
      !is_at_end?(dist)
    end

    private def scan_token
      skip_whitespace

      if is_at_end?
        return Token.new TokenType::EOF, "", @line_count, @column_count
      end

      @start = @current

      token_type = case {advance, peek}
                   when {'(', _}
                     TokenType::LeftParen
                   when {')', _}
                     TokenType::RightParen
                   when {'[', _}
                     TokenType::LeftBracket
                   when {']', _}
                     TokenType::RightBracket
                   when {',', _}
                     TokenType::Comma
                   when {':', '='}
                     advance
                     TokenType::Assign
                   when {':', _}
                     TokenType::Colon
                   when {';', _}
                     TokenType::SemiColon
                   when {'.', _}
                     TokenType::Period
                   when {'&', _}
                     TokenType::And
                   when {'|', _}
                     TokenType::Or
                   when {'+', _}
                     TokenType::Plus
                   when {'-', _}
                     TokenType::Minus
                   when {'*', _}
                     TokenType::Star
                   when {'/', _}
                     TokenType::Slash
                   when {'<', '='}
                     advance
                     TokenType::LE
                   when {'<', _}
                     TokenType::LT
                   when {'>', '='}
                     advance
                     TokenType::GE
                   when {'>', _}
                     TokenType::GT
                   when {'=', '='}
                     advance
                     TokenType::EQ
                   when {'!', '='}
                     advance
                     TokenType::NE
                   when {.number?, _}
                     number
                   when {.letter?, _}
                     identifier
                   when {'"', _}
                     string
                   else
                     raise "Unknown Token #{peek -1} @ #{@line_count}:#{@column_count}"
                   end

      Token.new token_type, token_type.string_literal? ? @source[@start...@current] : @source[@start...@current].downcase, @line_count, @column_count
    end

    private def peek(dist = 0) : Char
      unless is_at_end?(dist)
        @source[@current + dist]
      else
        '\u0000'
      end
    end

    private def advance(dist = 1) : Char
      peek(dist - 1).tap {
        @column_count += dist
        @current += dist
      }
    end

    private def skip_whitespace
      while true
        case peek
        when ' ', '\r', '\t'
          advance
        when '\n'
          @line_count += 1
          @column_count = 0
          advance
        when '/'
          case peek 1
          when '/'
            advance 2
            while peek != '\n' && is_not_at_end?
              advance
            end
          when '*'
            multiline = 1
            advance 2
            while true
              case {peek, peek 1}
              when {'*', '/'}
                multiline -= 1
                if multiline == 0
                  advance 2
                  break
                end
                advance
              when {'\n', _}
                @line_count += 1
                @column_count = 0
              when {'/', '*'}
                multiline += 1
              end
              advance
            end
          else
            return
          end
        else
          return
        end
      end
    end

    private def string
      while peek != '"' && is_not_at_end?
        if peek == '\n'
          @line_count += 1
          @column_count = 0
        end
        advance
      end

      if is_at_end?
        raise "Unterminated String" # TODO better error reporting
      end

      advance

      TokenType::StringLiteral
    end

    private def number
      number_type = TokenType::IntegerLiteral
      while peek.number?
        advance
      end

      if peek == '.' && peek(1).number?
        advance

        while peek.number?
          advance
        end
        number_type = TokenType::FloatLiteral
      end

      number_type
    end

    private def identifier
      while is_alpha_numeric peek
        advance
      end

      if keyword = @keywords[@source[@start...@current].downcase]?
        keyword
      else
        TokenType::Identifier
      end
    end

    private def is_alpha_numeric(char : Char)
      char.letter? || char.number? || char == '_'
    end
  end
end
