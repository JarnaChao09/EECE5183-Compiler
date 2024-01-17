require "../codegen/*"

module Compiler
  abstract class Expr
    abstract def codegen(variables : Hash(String, Float64), functions : Hash(String, Proc(Float64, Float64)))
  end
end
