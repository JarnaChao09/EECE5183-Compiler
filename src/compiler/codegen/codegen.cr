module Compiler
  class CodeGenerator
    property ctx : LLVM::Context
    property mod : LLVM::Module
    property function : LLVM::Function
    property function_name : String
    property function_names : Hash(String, String)
    property global_function_names : Hash(String, String)
    property variables : Hash(String, {LLVM::Value, LLVM::Type})
    property global_variables : Hash(String, {LLVM::Value, LLVM::Type})
    property function_types : Hash(String, LLVM::Type)
    property global_function_types : Hash(String, LLVM::Type)
    property target_machine : LLVM::TargetMachine
    property _stdin : LLVM::Value

    def initialize(mod_name : String = "")
      {% if host_flag?(:aarch64) %}
        LLVM.init_aarch64
      {% elsif host_flag?(:x86_64) %}
        LLVM.init_x86
      {% end %}
      @ctx = LLVM::Context.new

      @mod = @ctx.new_module(mod_name)

      @function = @mod.functions.add "main", [] of LLVM::Type, @ctx.int32
      @function_name = ""
      @function_names = {} of String => String
      @global_function_names = {} of String => String

      @variables = {} of String => {LLVM::Value, LLVM::Type}
      @global_variables = {} of String => {LLVM::Value, LLVM::Type}

      @function_types = {} of String => LLVM::Type
      @global_function_types = {} of String => LLVM::Type

      @target_machine = LLVM::Target.first.create_target_machine(LLVM.default_target_triple)

      # TODO: MacOS external pointer is "__stdinp", linux is "stdin"
      # could be moved to file descriptor impl (0 = stdin, 1 = stdout, 2 = stderr)
      stdin_string = {% if host_flag?(:darwin) %}
                       "__stdinp"
                     {% else %}
                       "stdin"
                     {% end %}

      @_stdin = mod.globals.add @ctx.void_pointer, stdin_string
      @_stdin.alignment = 8
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

    def generate(program : Compiler::Program)
      @function_name = program.program_name

      current_builder = @ctx.new_builder
      null_block = LLVM::BasicBlock.null

      program.declarations.each do |decl|
        if decl.is_global?
          generate current_builder, null_block, decl
        else
          raise "all program declarations must be global"
        end
      end

      generate program.body
    end

    def declare_native_function(name : String, types : Array(LLVM::Type), return_type : LLVM::Type, varargs : Bool = false) : Nil
      function_type = LLVM::Type.function(types, return_type, varargs)
      @global_function_types[name] = function_type
      @global_function_names[name] = name
      @mod.functions.add name, function_type
    end

    def define_native_function(name : String, types : Array(LLVM::Type), return_type : LLVM::Type, varargs : Bool = false, &)
      function_type = LLVM::Type.function(types, return_type, varargs)
      @global_function_types[name] = function_type
      @global_function_names[name] = name
      @mod.functions.add name, function_type do |function|
        function.basic_blocks.append do |builder|
          yield self, builder, function
        end
      end
    end

    def get_function(name : String) : {LLVM::Function, LLVM::Type}
      return {@mod.functions[name], @function_types[name]? || @global_function_types[name]}
    end

    def optimize(level : String = "default<O3>")
      LLVM::PassBuilderOptions.new do |options|
        LLVM.run_passes(@mod, level, @target_machine, options)
      end
    end
  end
end
