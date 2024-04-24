require "./decl"
require "./function"

module Compiler
  class FunctionDeclaration < Decl
    property name : String
    property function : Function
    property is_global : Bool

    def initialize(@name, @function, @is_global)
    end

    def is_global?
      @is_global
    end

    def to_s(io : IO)
      if is_global?
        io << "global "
      end

      io << @name << @function
    end
  end

  class Compiler::CodeGenerator
    def generate(builder, basic_block, func_decl : FunctionDeclaration) : LLVM::BasicBlock
      func_name = "#{@function_name}_#{func_decl.name}"

      param_types = func_decl.function.parameter_types.map { |e| e.to_llvm_type @ctx }
      return_type = func_decl.function.return_type.to_llvm_type @ctx
      func_type = LLVM::Type.function param_types, return_type

      if func_decl.is_global
        @global_function_names[func_decl.name] = func_name
        @global_function_types[func_name] = func_type
      else
        @function_names[func_decl.name] = func_name
        @function_types[func_name] = func_type
      end

      prev_function_name = @function_name
      @function_name = func_name

      prev_function_names = @function_names
      @function_names = {
        func_decl.name => func_name,
      }

      prev_variables = @variables
      @variables = {} of String => {LLVM::Value, LLVM::Type}

      prev_function_types = @function_types
      @function_types = {
        func_name => func_type,
      }

      prev_function = @function
      @function = @mod.functions.add func_name, param_types, return_type

      current_basic_block = @function.basic_blocks.append

      @function.params.each_with_index do |param, i|
        builder.position_at_end current_basic_block

        param_alloca = builder.alloca param.type, func_decl.function.parameters[i]
        builder.store param, param_alloca

        @variables[func_decl.function.parameters[i]] = {param_alloca, param.type}
      end

      func_decl.function.declarations.each do |decl|
        builder.position_at_end current_basic_block

        current_basic_block = generate builder, current_basic_block, decl
      end

      func_decl.function.body.each do |stmt|
        builder.position_at_end current_basic_block

        current_basic_block = generate builder, current_basic_block, stmt
      end

      @function_name = prev_function_name
      @function_names = prev_function_names
      @variables = prev_variables
      @function_types = prev_function_types
      @function = prev_function

      basic_block
    end
  end
end
