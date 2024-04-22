require "./stmt"
require "./type"

module Compiler
  class Function
    property parameters : Array(String)
    property parameter_types : Array(Type)
    property return_type : Type
    property declarations : Array(Decl)
    property body : Array(Stmt)

    def initialize(@parameters, @parameter_types, @return_type, @declarations, @body)
    end

    def to_s(io : IO)
      io << "("
      parameters.each do |parameter|
        io << parameter
      end
      io << ") " << @body
    end
  end
end
