require "./compiler/requires"
require "./compiler/*"

require "math"
require "option_parser"

emit_llvm_ir = false
dump_llvm_ir = false
compile = false
run = false
output_path = ""
rel_file_path = ""

errors = [] of String
error_parser = nil

options_parser = OptionParser.parse do |parser|
  parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler [subcommand] [options] [program-file]"

  parser.on "compile", "subcommand: Compile a file" do
    parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler compile [options] [program-file]"
    compile = true

    parser.on "-o FILE_NAME", "--output FILE_NAME", "Specifies the name of the executable in the output directory" do |file_name|
      output_path = file_name
    end
  end

  parser.on "run", "subcommand: Compile and Execute a file using LLVM JIT" do
    parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler run [options] [program-file]"
    run = true
  end

  parser.on "-emit-ir", "--emit-llvm-ir", "Emits generated LLVM IR" do
    emit_llvm_ir = true
  end

  parser.on "-dump-ir", "--dump-llvm-ir", "Dumps generated LLVM IR" do
    dump_llvm_ir = true
  end

  parser.on "-v", "--version", "Show Version" do
    puts "version 0.1"
    exit
  end

  parser.on "-h", "--help", "Show Help" do
    puts parser
    exit
  end

  parser.invalid_option do |flag|
    errors << "ERROR: #{flag} is not a valid option."
  end

  parser.unknown_args do |before, after|
    files = [] of String
    unknown_args = [] of String
    before.each do |e|
      if (!e.starts_with?("-") && e.ends_with?(".src")) || File.file?(e)
        files << e
      else
        unknown_args << e if !e.starts_with?("-")
      end
    end
    if !unknown_args.empty?
      unknown_args.each do |uarg|
        errors << "ERROR: unexpected argument #{uarg}"
      end
    end
    case files.size
    when 0
      errors << "ERROR: expected a file"
    when 1
      if File.file? before[0]
        rel_file_path = before[0]
      else
        errors << "ERROR: unable to find file #{before[0]}"
      end
    else
      errors << "ERROR: expected only one file, found #{files.join ", "}"
    end
  end
end

if !compile && !run
  puts "ERROR: expected a subcommand"
  puts options_parser
  exit 1
end

if !errors.empty?
  errors.each do |err|
    puts err
  end
  options_parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler compile [options] [program-file]" if compile
  options_parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler run [options] [program-file]" if run
  puts options_parser

  exit 1
end

file = File.read(rel_file_path)

scanner = Compiler::Scanner.new file

tokens = scanner.tokens

if !scanner.errors.empty?
  scanner.errors.map do |err|
    puts "ERROR[SCANNER]: #{err}"
  end

  exit 1
end

parser = Compiler::Parser.new tokens

program = parser.parse

if !parser.warnings.empty?
  parser.warnings.map do |err|
    puts err
  end
end

if !parser.errors.empty?
  parser.errors.map do |err|
    puts "ERROR[PARSER]: #{err}"
  end

  exit 1
end

generator = Compiler::CodeGenerator.new "main"

generator.declare_native_function "sqrt", [generator.ctx.double], generator.ctx.double

generator.declare_native_function "printf", [generator.ctx.int8.pointer], generator.ctx.int32, varargs = true

scanf_name = {% if host_flag?(:darwin) %}
               "scanf"
             {% else %}
               "__isoc99_scanf"
             {% end %}

generator.declare_native_function scanf_name, [generator.ctx.int8.pointer], generator.ctx.int32, varargs = true
generator.global_function_names["scanf"] = scanf_name

generator.declare_native_function "getline", [generator.ctx.int8.pointer.pointer, generator.ctx.int64.pointer, generator.ctx.pointer], generator.ctx.int64

generator.declare_native_function "strcmp", [generator.ctx.int8.pointer, generator.ctx.int8.pointer], generator.ctx.int32

generator.declare_native_function "exit", [generator.ctx.int32], generator.ctx.void

generator.declare_native_function "getchar", [] of LLVM::Type, generator.ctx.int32

generator.define_native_function "oobe", [generator.ctx.int64, generator.ctx.int64], generator.ctx.void do |generator, builder, function|
  input_index, input_size = function.params

  output_string = builder.global_string_pointer "ERROR: index %ld out of bounds for array size of %ld with index range of [0, %ld)\n"
  printf_function, printf_function_type = generator.get_function "printf"

  builder.call printf_function_type, printf_function, [output_string, input_index, input_size, input_size]

  exit_function, exit_function_type = generator.get_function "exit"

  builder.call exit_function_type, exit_function, [generator.ctx.int32.const_int(1)]

  builder.unreachable
end

generator.define_native_function "flush", [] of LLVM::Type, generator.ctx.void do |generator, builder, function|
  c = builder.alloca generator.ctx.int32

  while_cond = function.basic_blocks.append
  and_rhs = function.basic_blocks.append
  and_end = function.basic_blocks.append
  while_body = function.basic_blocks.append
  while_end = function.basic_blocks.append

  builder.br while_cond

  builder.position_at_end while_cond

  getchar_function, getchar_function_type = generator.get_function "getchar"

  char = builder.call getchar_function_type, getchar_function, [] of LLVM::Value

  builder.store char, c

  cmp1 = builder.icmp LLVM::IntPredicate::NE, char, generator.ctx.int32.const_int(10)

  builder.cond cmp1, and_rhs, and_end

  builder.position_at_end and_rhs

  tmp = builder.load generator.ctx.int32, c

  cmp2 = builder.icmp LLVM::IntPredicate::NE, char, generator.ctx.int32.const_int(-1)

  builder.br and_end

  builder.position_at_end and_end

  tmp1 = builder.phi generator.ctx.int1, [while_cond, and_rhs], [generator.ctx.int1.const_int(0), cmp2]

  builder.cond tmp1, while_body, while_end

  builder.position_at_end while_body

  builder.br while_cond

  builder.position_at_end while_end

  builder.ret
end

generator.define_native_function "_sqrt", [generator.ctx.int64], generator.ctx.double do |generator, builder, function|
  input_int = function.params[0]

  sqrt_function, sqrt_function_type = generator.get_function "sqrt"

  input_double = builder.si2fp input_int, generator.ctx.double

  ret = builder.call sqrt_function_type, sqrt_function, [input_double]

  builder.ret ret
end
# overriding sqrt in the global function names
# lose access to double sqrt(double)
generator.global_function_names["sqrt"] = "_sqrt"

generator.define_native_function "getinteger", [] of LLVM::Type, generator.ctx.int64 do |generator, builder, function|
  integer_type = generator.ctx.int64

  integer_var = builder.alloca integer_type, "getInttmp"

  input_string = builder.global_string_pointer "%lld"

  scanf_function, scanf_function_type = generator.get_function "scanf"

  builder.call scanf_function_type, scanf_function, [input_string, integer_var], "scanftmp"

  flush_function, flush_function_type = generator.get_function "flush"

  builder.call flush_function_type, flush_function, [] of LLVM::Value

  ret = builder.load integer_type, integer_var

  builder.ret ret
end

generator.define_native_function "getfloat", [] of LLVM::Type, generator.ctx.double do |generator, builder, function|
  float_type = generator.ctx.double

  float_var = builder.alloca float_type, "getFloattmp"

  input_string = builder.global_string_pointer "%lf"

  scanf_function, scanf_function_type = generator.get_function "scanf"

  builder.call scanf_function_type, scanf_function, [input_string, float_var], "scanftmp"

  flush_function, flush_function_type = generator.get_function "flush"

  builder.call flush_function_type, flush_function, [] of LLVM::Value

  ret = builder.load float_type, float_var

  builder.ret ret
end

generator.define_native_function "getbool", [] of LLVM::Type, generator.ctx.int1 do |generator, builder, function|
  boolean_type = generator.ctx.int1

  getInt_function, getInt_function_type = generator.get_function "getinteger"

  input = builder.call getInt_function_type, getInt_function, [] of LLVM::Value, "getInttmp"

  ret = builder.icmp LLVM::IntPredicate::NE, input, generator.ctx.int64.const_int(0), "netmp"

  builder.ret ret
end

generator.define_native_function "getstring", [] of LLVM::Type, generator.ctx.int8.pointer do |generator, builder, function|
  string_type = generator.ctx.int8.pointer

  line = builder.alloca string_type         # , "line"
  len = builder.alloca generator.ctx.int64  # , "len"
  read = builder.alloca generator.ctx.int64 # , "read"

  builder.store generator.ctx.pointer.null_pointer, line
  store = builder.store generator.ctx.int64.const_int(0), len

  # store.alignment = 8

  _stdin = builder.load generator._stdin.type, generator._stdin

  getline_function, getline_function_type = generator.get_function "getline"

  read_value = builder.call getline_function_type, getline_function, [line, len, _stdin] # , "getlinetmp"

  store = builder.store read_value, read

  # store.alignment = 8

  line_access = builder.load string_type, line
  read_value = builder.load generator.ctx.int64, read

  # read_value.alignment = 8

  read1 = builder.sub read_value, generator.ctx.int64.const_int(1)

  newline_char = builder.gep generator.ctx.int8, line_access, read1

  builder.store generator.ctx.int8.const_int(0), newline_char

  ret = builder.load string_type, line

  builder.ret ret
end

generator.define_native_function "putfloat", [generator.ctx.double], generator.ctx.int1 do |generator, builder, function|
  input_float = function.params[0]

  output_string = builder.global_string_pointer "%.16f\n"

  printf_function, printf_function_type = generator.get_function "printf"

  printf_value = builder.call printf_function_type, printf_function, [output_string, input_float], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.define_native_function "putinteger", [generator.ctx.int64], generator.ctx.int1 do |generator, builder, function|
  input_int = function.params[0]

  output_string = builder.global_string_pointer "%ld\n"

  printf_function, printf_function_type = generator.get_function "printf"

  printf_value = builder.call printf_function_type, printf_function, [output_string, input_int], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.define_native_function "putbool", [generator.ctx.int1], generator.ctx.int1 do |generator, builder, function|
  input_bool = function.params[0]

  printf_function, printf_function_type = generator.get_function "printf"

  print_value = builder.select input_bool, builder.global_string_pointer("true\n"), builder.global_string_pointer("false\n")

  printf_value = builder.call printf_function_type, printf_function, [print_value], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.define_native_function "putstring", [generator.ctx.pointer], generator.ctx.int1 do |generator, builder, function|
  input_string = function.params[0]

  output_string = builder.global_string_pointer "%s\n"

  printf_function, printf_function_type = generator.get_function "printf"

  printf_value = builder.call printf_function_type, printf_function, [output_string, input_string], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.generate program

generator.optimize

if dump_llvm_ir
  generator.mod.dump

  puts "========"
end

generator.mod.verify

if compile
  if !Dir.exists? "output"
    Dir.mkdir "output"
  end

  name = if output_path != ""
           output_path
         else
           "compiler_tmp.out"
         end

  if emit_llvm_ir
    File.open("output/#{name}.ll", "w") do |file|
      generator.mod.to_s file
    end
  end

  generator.emit_to_obj_file "output/#{name}.o"

  clang_process = Process.run("clang", ["-lm", "output/#{name}.o", "-o", "output/#{name}"])

  puts "compiled with status code #{$?}"
elsif run
  LLVM::JITCompiler.new generator.mod do |jit|
    func_ptr = jit.get_pointer_to_global(generator.function)
    func_proc = Proc(Int32).new(func_ptr, Pointer(Void).null)
    puts "exited with #{func_proc.call}"
  end
end
