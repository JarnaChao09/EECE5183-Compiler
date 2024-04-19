require "./decl"
require "./type"

module Compiler
  class VariableDeclaration < Decl
    property variable : String
    property variable_type : Type
    property is_global : Bool
    property array_length : UInt32 | Nil

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
        array_length = variable_declaration.array_length
        if array_length
          var_type = variable_declaration.variable_type
          llvm_type = var_type.to_llvm_type @ctx
          llvm_array_type = llvm_type.array array_length

          init_array = Array.new array_length do |i|
            var_type.to_llvm_default_value @ctx
          end

          global_value = @mod.globals.add llvm_array_type, variable_declaration.variable
          global_value.initializer = llvm_type.const_array init_array

          @variables[variable_declaration.variable] = {global_value, llvm_array_type}
        else
          global_value = @mod.globals.add variable_declaration.variable_type.to_llvm_type(@ctx), variable_declaration.variable
          global_value.initializer = variable_declaration.variable_type.to_llvm_default_value(@ctx)

          if variable_declaration.variable_type.string?
            global_value.alignment = 8
          end

          @variables[variable_declaration.variable] = {global_value, variable_declaration.variable_type.to_llvm_type(@ctx)}
        end
      else
        builder.position_at_end basic_block

        array_length = variable_declaration.array_length
        if array_length
          var_type = variable_declaration.variable_type
          llvm_type = var_type.to_llvm_type @ctx
          llvm_array_type = llvm_type.array array_length

          init_array = Array.new array_length do |i|
            var_type.to_llvm_default_value @ctx
          end

          alloca_location = builder.alloca llvm_array_type, variable_declaration.variable

          @variables[variable_declaration.variable] = {alloca_location, llvm_array_type}
        else
          builder.position_at_end basic_block
          alloca_location = builder.alloca variable_declaration.variable_type.to_llvm_type(@ctx), variable_declaration.variable

          @variables[variable_declaration.variable] = {alloca_location, variable_declaration.variable_type.to_llvm_type(@ctx)}
        end
      end
      basic_block
    end
  end
end
