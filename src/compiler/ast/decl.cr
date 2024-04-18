require "./type"
require "../codegen/*"

module Compiler
  abstract class Decl
    abstract def is_global?
  end
end
