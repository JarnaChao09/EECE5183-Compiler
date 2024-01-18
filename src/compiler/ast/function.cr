require "./expr"

module Compiler
  class Function
    property parameters : Array(String) # TODO add type checking to function prototypes
    # property return_type : # TODO
    property body : Expr # TDOO expand to multiline function bodies

    def initialize(@parameters, @body)
    end

    def call(arguments : Array(Float64), variables : Hash(String, Float64), functions : Hash(String, Function))
      unless arguments.size == @parameters.size
        raise "Incorrect number of arguments"
      end

      @body.codegen(variables.merge(Hash.zip(parameters, arguments)), functions)
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
