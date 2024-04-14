module Compiler
  class CodeGenerator
    property ctx : LLVM::Context
    property mod : LLVM::Module
    property function : LLVM::Function
    property variables : Hash(String, {LLVM::Value, LLVM::Type})
    property function_types : Hash(String, LLVM::Type)
    property target_machine : LLVM::TargetMachine

    def initialize(mod_name : String = "")
      {% if host_flag?(:aarch64) %}
        LLVM.init_aarch64
      {% elsif host_flag?(:x86_64) %}
        LLVM.init_x86
      {% end %}
      @ctx = LLVM::Context.new
      @mod = @ctx.new_module(mod_name)
      @function = @mod.functions.add "main", [] of LLVM::Type, @ctx.int32
      @variables = {} of String => {LLVM::Value, LLVM::Type}
      @function_types = {} of String => LLVM::Type
      @target_machine = LLVM::Target.first.create_target_machine(LLVM.default_target_triple)
    end

    def generate(statements : Array(Compiler::Stmt))
      current_builder = @ctx.new_builder
      current_basic_block = @function.basic_blocks.append

      statements.each do |statement|
        current_builder.position_at_end current_basic_block

        current_basic_block = generate(current_builder, current_basic_block, statement)
      end

      current_builder.position_at_end current_basic_block
      current_builder.ret @ctx.int32.const_int 0
    end

    def define_native_function(name : String, types : Array(LLVM::Type), return_type : LLVM::Type, &)
      function_type = LLVM::Type.function(types, return_type)
      @function_types[name] = function_type
      @mod.functions.add name, function_type do |function|
        function.basic_blocks.append do |builder|
          yield self, builder, function
        end
      end
    end

    def optimize(level : String = "default<O3>")
      LLVM::PassBuilderOptions.new do |options|
        LLVM.run_passes(@mod, level, @target_machine, options)
      end
    end
  end
end
