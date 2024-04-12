require "./value"
require "./type"
require "../codegen/*"

module Compiler
  abstract class Stmt
    abstract def codegen(variables : Hash(String, Value), functions : Hash(String, Function))
  end
end
