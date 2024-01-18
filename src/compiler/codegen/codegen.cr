module Compiler
  class CodeGenerator
    property ctx : LLVM::Context
    property mod : LLVM::Module
    property function : LLVM::Function
    property variables : Hash(String, LLVM::Value)
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
      @variables = {} of String => LLVM::Value
      @function_types = {} of String => LLVM::Type
      @target_machine = LLVM::Target.first.create_target_machine(LLVM.default_target_triple)
    end

    def generate(expressions : Array(Compiler::Expr))
      @function.basic_blocks.append do |builder|
        expressions.each do |expression|
          generated = generate(builder, expression)
          unless expression.is_a?(AssignmentExpr)
            function_type = @function_types["printf"]
            builder.call function_type, @mod.functions["printf"], [builder.global_string_pointer("%.16f\n"), generated], "calltmp"
          end
        end
        builder.ret @ctx.int32.const_int(0)
      end
    end

    def optimize(level : String = "default<O3>")
      LLVM::PassBuilderOptions.new do |options|
        LLVM.run_passes(@mod, level, @target_machine, options)
      end
    end
  end
end
