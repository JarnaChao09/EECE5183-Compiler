require "./value"
require "./type"
require "../codegen/*"

module Compiler
  abstract class Expr
    abstract def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
  end
end
