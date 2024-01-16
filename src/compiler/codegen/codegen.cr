require "../ast/*"

module Compiler
  class CodeGenerator
    private property function : LLVM::Function

    def initialize(mod_name : String = "")
      {% if host_flag?(:aarch64) %}
        LLVM.init_aarch64
      {% elsif host_flag?(:x86_64) %}
        LLVM.init_x86
      {% end %}
      @ctx = LLVM::Context.new
      @mod = @ctx.new_module(mod_name)
      @function = @mod.functions.add "main", [] of LLVM::Type, @ctx.void
    end

    def generate(expr : Compiler::Expr)
      @function.basic_blocks.append do |builder|
        builder.ret generate(builder, expr)
      end
    end

    def optimize(level : String = "default<O3>")
      target_machine = LLVM::Target.first.create_target_machine(LLVM.default_target_triple)
      LLVM::PassBuilderOptions.new do |options|
        LLVM.run_passes(mod, level, target_machine, options)
      end
    end

    def mod
      @mod
    end
  end
end
