require "./decl"
require "./type"

module Compiler
  class VariableDeclaration < Decl
    property variable : String
    property variable_type : Type
    property is_global : Bool
    property array_length : Int32 | Nil

    def initialize(@variable, @variable_type, @is_global, @array_length = nil)
    end

    def is_global?
      @is_global
    end

    def to_s(io : IO)
      if is_global
        io << "global "
      end

      io << "variable " << @variable << " : " << @variable_type
      if @array_length
        io << "[" << @array_length << "]"
      end
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, variable_declaration : VariableDeclaration) : LLVM::BasicBlock
      if variable_declaration.is_global
        global_value = @mod.globals.add variable_declaration.variable_type.to_llvm_type(@ctx), variable_declaration.variable
        global_value.initializer = variable_declaration.variable_type.to_llvm_default_value(@ctx)

        @variables[variable_declaration.variable] = {global_value, variable_declaration.variable_type.to_llvm_type(@ctx)}
      else
        builder.position_at_end basic_block
        alloca_location = builder.alloca variable_declaration.variable_type.to_llvm_type(@ctx), variable_declaration.variable

        @variables[variable_declaration.variable] = {alloca_location, variable_declaration.variable_type.to_llvm_type(@ctx)}
      end
      basic_block
    end
  end
end
