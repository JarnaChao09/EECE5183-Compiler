require "./stmt"
require "./type"

module Compiler
  class Function
    property parameters : Array(VariableDeclaration)
    property return_type : Type
    property declarations : Array(Decl)
    property body : Array(Stmt)

    def initialize(@parameters, @return_type, @declarations, @body)
    end

    def to_s(io : IO)
      io << "("
      parameters.each do |parameter|
        io << parameter
      end
      io << ")\n"

      body.each do |stmt|
        io << "    " << stmt << "\n"
      end
    end
  end
end
