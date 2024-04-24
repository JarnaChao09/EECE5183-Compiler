require "./compiler/requires"
require "./compiler/*"

require "math"
require "option_parser"

debug_llvm_ir = false
compile = false
run = false
rel_file_path = ""

errors = [] of String
error_parser = nil

options_parser = OptionParser.parse do |parser|
  parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler [subcommand] [options] [program-file]"

  parser.on "compile", "Compile a file" do
    parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler compile [options] [program-file]"
    compile = true
  end

  parser.on "run", "Compiler and Execute a file using LLVM JIT" do
    parser.banner = "Welcome to the compiler for EECE 5183\nUsage: compiler run [options] [program-file]"
    run = true
  end

  parser.on "-llvm_ir", "--emit-llvm-ir", "Emits generated LLVM IR" do
    debug_llvm_ir = true
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

scanner.tokens.each do |token|
  puts token
end

parser = Compiler::Parser.new scanner.tokens

program = parser.parse

puts program

# exit

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

generator.define_native_function "test", [] of LLVM::Type, generator.ctx.int1 do |generator, builder, function|
  printf_function, printf_function_type = generator.get_function "printf"

  builder.call printf_function_type, printf_function, [builder.global_string_pointer("didn't short circuit\n")], "printftmp"

  ret = generator.ctx.int1.const_int 1

  builder.ret ret
end

# generator.define_native_function "ile", [generator.ctx.int64, generator.ctx.int64], generator.ctx.int1 do |generator, builder, function|
#   l, r = function.params

#   ret = builder.icmp LLVM::IntPredicate::SLE, l, r, "iletmp"

#   builder.ret ret
# end

# id_type = LLVM::Type.function([generator.ctx.double, generator.ctx.double], generator.ctx.double)

# func = generator.mod.functions.add "add2", id_type

# generator.function_types["add2"] = id_type

# program = Compiler::Program.new(
#   "RecursiveFib",
#   [
#     Compiler::VariableDeclaration.new("x", Compiler::Type::Integer, true),
#     Compiler::VariableDeclaration.new("i", Compiler::Type::Integer, true),
#     Compiler::VariableDeclaration.new("max", Compiler::Type::Integer, true),
#     Compiler::VariableDeclaration.new("out", Compiler::Type::Boolean, true),

#     Compiler::FunctionDeclaration.new(
#       "Fib",
#       Compiler::Function.new(
#         [
#           "val",
#         ],
#         [
#           Compiler::Type::Integer,
#         ],
#         Compiler::Type::Integer,
#         [
#           Compiler::FunctionDeclaration.new(
#             "Sub",
#             Compiler::Function.new(
#               [
#                 "val1",
#               ],
#               [
#                 Compiler::Type::Integer,
#               ],
#               Compiler::Type::Integer,
#               [] of Compiler::Decl,
#               [
#                 Compiler::ReturnStmt.new(
#                   Compiler::BinaryExpr.new(
#                     Compiler::VariableExpr.new("val1"),
#                     Compiler::BinaryExpr::Operation::Subtraction,
#                     Compiler::IntegerExpr.new(1),
#                   )
#                 ),
#               ] of Compiler::Stmt
#             ),
#             false
#           ),
#         ] of Compiler::Decl,
#         [
#           Compiler::IfStmt.new(
#             Compiler::BinaryExpr.new(
#               Compiler::VariableExpr.new("val"),
#               Compiler::BinaryExpr::Operation::Equal,
#               Compiler::IntegerExpr.new(0)
#             ),
#             [
#               Compiler::ReturnStmt.new(
#                 Compiler::IntegerExpr.new(0)
#               ),
#             ] of Compiler::Stmt,
#             [] of Compiler::Stmt
#           ),
#           Compiler::IfStmt.new(
#             Compiler::BinaryExpr.new(
#               Compiler::VariableExpr.new("val"),
#               Compiler::BinaryExpr::Operation::Equal,
#               Compiler::IntegerExpr.new(1)
#             ),
#             [
#               Compiler::ReturnStmt.new(
#                 Compiler::IntegerExpr.new(1)
#               ),
#             ] of Compiler::Stmt,
#             [] of Compiler::Stmt
#           ),
#           Compiler::ReturnStmt.new(
#             Compiler::BinaryExpr.new(
#               Compiler::VariableExpr.new("val"),
#               Compiler::BinaryExpr::Operation::Addition,
#               Compiler::CallExpr.new(
#                 "Fib",
#                 [
#                   Compiler::CallExpr.new(
#                     "Sub",
#                     [
#                       Compiler::VariableExpr.new("val"),
#                     ] of Compiler::Expr
#                   ),
#                 ] of Compiler::Expr
#               )
#             )
#           ),
#         ] of Compiler::Stmt,
#       ),
#       true
#     ),
#   ] of Compiler::Decl,
#   [
#     Compiler::AssignmentStmt.new(
#       "max",
#       Compiler::CallExpr.new(
#         "getInteger",
#         [] of Compiler::Expr
#       )
#     ),
#     Compiler::LoopStmt.new(
#       Compiler::AssignmentStmt.new(
#         "i",
#         Compiler::IntegerExpr.new(0)
#       ),
#       Compiler::BinaryExpr.new(
#         Compiler::VariableExpr.new("i"),
#         Compiler::BinaryExpr::Operation::LessThan,
#         Compiler::VariableExpr.new("max"),
#       ),
#       [
#         Compiler::AssignmentStmt.new(
#           "x",
#           Compiler::CallExpr.new(
#             "Fib",
#             [Compiler::VariableExpr.new("i")] of Compiler::Expr
#           )
#         ),
#         Compiler::AssignmentStmt.new(
#           "out",
#           Compiler::CallExpr.new(
#             "putInteger",
#             [Compiler::VariableExpr.new("x")] of Compiler::Expr
#           )
#         ),
#         Compiler::AssignmentStmt.new(
#           "i",
#           Compiler::BinaryExpr.new(
#             Compiler::VariableExpr.new("i"),
#             Compiler::BinaryExpr::Operation::Addition,
#             Compiler::IntegerExpr.new(1)
#           )
#         ),
#       ] of Compiler::Stmt
#     ),
#   ] of Compiler::Stmt
# )

# program = Compiler::Program.new(
#   "testprogram",
#   [
#     # Compiler::VariableDeclaration.new("test1", Compiler::Type::String, true),
#     # Compiler::VariableDeclaration.new("test2", Compiler::Type::String, true),

#     # Compiler::VariableDeclaration.new("index", Compiler::Type::Integer, true),
#     # Compiler::VariableDeclaration.new("value", Compiler::Type::Integer, true),
#     # Compiler::VariableDeclaration.new("test", Compiler::Type::Integer, true, 4),

#     # Compiler::VariableDeclaration.new("i", Compiler::Type::Integer, true),
#     # Compiler::VariableDeclaration.new("n", Compiler::Type::Integer, true),

#     # Compiler::VariableDeclaration.new("arr1", Compiler::Type::Integer, true, 2),
#     # Compiler::VariableDeclaration.new("arr2", Compiler::Type::Integer, true, 2),
#     # Compiler::VariableDeclaration.new("arr3", Compiler::Type::Integer, true, 2),
#     # Compiler::VariableDeclaration.new("arr4", Compiler::Type::Integer, true, 2),
#     Compiler::VariableDeclaration.new("arr5", Compiler::Type::Double, true, 2),
#     # Compiler::VariableDeclaration.new("arr6", Compiler::Type::Double, true, 2),
#     # Compiler::VariableDeclaration.new("arrb", Compiler::Type::Boolean, true, 2),
#     # Compiler::VariableDeclaration.new("arrB", Compiler::Type::Boolean, true, 2),

#     # Compiler::VariableDeclaration.new("test_integer", Compiler::Type::Integer, true),
#     # Compiler::VariableDeclaration.new("test_boolean", Compiler::Type::Boolean, true),
#     # Compiler::VariableDeclaration.new("test_double", Compiler::Type::Double, true),

#     # Compiler::FunctionDeclaration.new(
#     #   "fib",
#     #   Compiler::Function.new(
#     #     ["n"],
#     #     [Compiler::Type::Integer],
#     #     Compiler::Type::Integer,
#     #     [] of Compiler::Decl,
#     #     [
#     #       Compiler::IfStmt.new(
#     #         Compiler::BinaryExpr.new(
#     #           Compiler::BinaryExpr.new(
#     #             Compiler::VariableExpr.new("n"),
#     #             Compiler::BinaryExpr::Operation::Equal,
#     #             Compiler::IntegerExpr.new(0),
#     #           ),
#     #           Compiler::BinaryExpr::Operation::BitwiseOr,
#     #           Compiler::BinaryExpr.new(
#     #             Compiler::VariableExpr.new("n"),
#     #             Compiler::BinaryExpr::Operation::Equal,
#     #             Compiler::IntegerExpr.new(1),
#     #           ),
#     #         ),
#     #         [
#     #           Compiler::ReturnStmt.new(
#     #             Compiler::VariableExpr.new("n")
#     #           ),
#     #         ] of Compiler::Stmt,
#     #         [
#     #           Compiler::ReturnStmt.new(
#     #             Compiler::BinaryExpr.new(
#     #               Compiler::CallExpr.new(
#     #                 "fib",
#     #                 [
#     #                   Compiler::BinaryExpr.new(
#     #                     Compiler::VariableExpr.new("n"),
#     #                     Compiler::BinaryExpr::Operation::Subtraction,
#     #                     Compiler::IntegerExpr.new(1),
#     #                   ),
#     #                 ] of Compiler::Expr
#     #               ),
#     #               Compiler::BinaryExpr::Operation::Addition,
#     #               Compiler::CallExpr.new(
#     #                 "fib",
#     #                 [
#     #                   Compiler::BinaryExpr.new(
#     #                     Compiler::VariableExpr.new("n"),
#     #                     Compiler::BinaryExpr::Operation::Subtraction,
#     #                     Compiler::IntegerExpr.new(2),
#     #                   ),
#     #                 ] of Compiler::Expr
#     #               ),
#     #             )
#     #           ),
#     #         ] of Compiler::Stmt
#     #       ),
#     #     ] of Compiler::Stmt,
#     #   ),
#     #   true
#     # ),
#   ] of Compiler::Decl,
#   [
#     # Compiler::AssignmentStmt.new(
#     #   "test_integer", Compiler::UnaryExpr.new(Compiler::UnaryExpr::Operation::BitwiseNot, Compiler::CallExpr.new("getInteger", [] of Compiler::Expr))
#     # ),
#     # Compiler::AssignmentStmt.new(
#     #   "test_boolean", Compiler::UnaryExpr.new(Compiler::UnaryExpr::Operation::BitwiseNot, Compiler::CallExpr.new("getBool", [] of Compiler::Expr))
#     # ),
#     # Compiler::AssignmentStmt.new(
#     #   "test_double", Compiler::UnaryExpr.new(Compiler::UnaryExpr::Operation::Negation, Compiler::CallExpr.new("getFloat", [] of Compiler::Expr))
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::VariableExpr.new("test_integer")] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putBool", [Compiler::VariableExpr.new("test_boolean")] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putFloat", [Compiler::VariableExpr.new("test_double")] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInt",
#     #     [
#     #       Compiler::CallExpr.new(
#     #         "fib",
#     #         [
#     #           Compiler::IntegerExpr.new(20),
#     #         ] of Compiler::Expr
#     #       ),
#     #     ] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::IndexSetStmt.new(
#     #   "arr1", Compiler::IntegerExpr.new(0), Compiler::CallExpr.new("getInteger", [] of Compiler::Expr)
#     # ),
#     # Compiler::IndexSetStmt.new(
#     #   "arr1", Compiler::IntegerExpr.new(1), Compiler::CallExpr.new("getInteger", [] of Compiler::Expr)
#     # ),
#     # Compiler::IndexSetStmt.new(
#     #   "arr3", Compiler::IntegerExpr.new(0), Compiler::CallExpr.new("getInteger", [] of Compiler::Expr)
#     # ),
#     # Compiler::IndexSetStmt.new(
#     #   "arr3", Compiler::IntegerExpr.new(1), Compiler::CallExpr.new("getInteger", [] of Compiler::Expr)
#     # ),
#     Compiler::IndexSetStmt.new(
#       "arr5", Compiler::IntegerExpr.new(0), Compiler::CallExpr.new("getFloat", [] of Compiler::Expr)
#     ),
#     Compiler::IndexSetStmt.new(
#       "arr5", Compiler::IntegerExpr.new(1), Compiler::CallExpr.new("getFloat", [] of Compiler::Expr)
#     ),
#     Compiler::IndexSetStmt.new(
#       "arr5", Compiler::CallExpr.new("getInteger", [] of Compiler::Expr), Compiler::CallExpr.new("getFloat", [] of Compiler::Expr)
#     ),
#     # Compiler::IndexSetStmt.new(
#     #   "arrb", Compiler::IntegerExpr.new(0), Compiler::CallExpr.new("getBool", [] of Compiler::Expr)
#     # ),
#     # Compiler::IndexSetStmt.new(
#     #   "arrb", Compiler::IntegerExpr.new(1), Compiler::CallExpr.new("getBool", [] of Compiler::Expr)
#     # ),

#     # Compiler::AssignmentStmt.new(
#     #   "arr2",
#     #   Compiler::UnaryExpr.new(
#     #     Compiler::UnaryExpr::Operation::BitwiseNot,
#     #     Compiler::VariableExpr.new("arr1"),
#     #   )
#     # ),

#     # Compiler::AssignmentStmt.new(
#     #   "arr3",
#     #   Compiler::BinaryExpr.new(
#     #     Compiler::VariableExpr.new("arr3"),
#     #     Compiler::BinaryExpr::Operation::Multiplication,
#     #     Compiler::IntegerExpr.new(2)
#     #   )
#     # ),

#     # Compiler::AssignmentStmt.new(
#     #   "arr4",
#     #   Compiler::UnaryExpr.new(
#     #     Compiler::UnaryExpr::Operation::Negation,
#     #     Compiler::VariableExpr.new("arr3"),
#     #   )
#     # ),

#     Compiler::AssignmentStmt.new(
#       "arr5",
#       Compiler::BinaryExpr.new(
#         Compiler::BinaryExpr.new(
#           Compiler::FloatExpr.new(2.0),
#           Compiler::BinaryExpr::Operation::Multiplication,
#           Compiler::VariableExpr.new("arr5")
#         ),
#         Compiler::BinaryExpr::Operation::Subtraction,
#         Compiler::FloatExpr.new(1.0)
#       )
#     ),

#     # Compiler::AssignmentStmt.new(
#     #   "arr6",
#     #   Compiler::UnaryExpr.new(
#     #     Compiler::UnaryExpr::Operation::Negation,
#     #     Compiler::VariableExpr.new("arr5"),
#     #   )
#     # ),

#     # Compiler::AssignmentStmt.new(
#     #   "arrB",
#     #   Compiler::UnaryExpr.new(
#     #     Compiler::UnaryExpr::Operation::BitwiseNot,
#     #     Compiler::VariableExpr.new("arrb"),
#     #   )
#     # ),

#     Compiler::ExpressionStmt.new(
#       Compiler::CallExpr.new(
#         "putString", [Compiler::StringExpr.new("")] of Compiler::Expr
#       )
#     ),
#     Compiler::ExpressionStmt.new(
#       Compiler::CallExpr.new(
#         "putFloat", [Compiler::IndexGetExpr.new("arr5", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#       )
#     ),
#     Compiler::ExpressionStmt.new(
#       Compiler::CallExpr.new(
#         "putFloat", [Compiler::IndexGetExpr.new("arr5", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#       )
#     ),
#     Compiler::ExpressionStmt.new(
#       Compiler::CallExpr.new(
#         "putString", [Compiler::StringExpr.new("")] of Compiler::Expr
#       )
#     ),
#     Compiler::ExpressionStmt.new(
#       Compiler::CallExpr.new(
#         "putFloat",
#         [
#           Compiler::IndexGetExpr.new(
#             "arr5",
#             Compiler::CallExpr.new(
#               "getInteger",
#               [] of Compiler::Expr
#             )
#           ),
#         ] of Compiler::Expr
#       )
#     ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr1", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr1", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr3", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr3", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putFloat", [Compiler::IndexGetExpr.new("arr5", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putFloat", [Compiler::IndexGetExpr.new("arr5", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putBool", [Compiler::IndexGetExpr.new("arrb", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putBool", [Compiler::IndexGetExpr.new("arrb", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),

#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putString", [Compiler::StringExpr.new("")] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr2", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr2", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr4", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr4", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putFloat", [Compiler::IndexGetExpr.new("arr6", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putFloat", [Compiler::IndexGetExpr.new("arr6", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putBool", [Compiler::IndexGetExpr.new("arrB", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putBool", [Compiler::IndexGetExpr.new("arrB", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putFloat", [Compiler::CallExpr.new("sqrt", [Compiler::CallExpr.new("getInteger", [] of Compiler::Expr)] of Compiler::Expr)] of Compiler::Expr
#     #   )
#     # ),

#     # Compiler::AssignmentStmt.new(
#     #   "arr3",
#     #   Compiler::BinaryExpr.new(
#     #     Compiler::VariableExpr.new("arr1"),
#     #     Compiler::BinaryExpr::Operation::Multiplication,
#     #     Compiler::VariableExpr.new("arr2"),
#     #   )
#     # ),

#     # Compiler::LoopStmt.new(
#     #   Compiler::AssignmentStmt.new(
#     #     "i", Compiler::IntegerExpr.new(0)
#     #   ),
#     #   Compiler::BinaryExpr.new(
#     #     Compiler::VariableExpr.new("i"),
#     #     Compiler::BinaryExpr::Operation::LessThan,
#     #     Compiler::IntegerExpr.new(2),
#     #   ),
#     #   [
#     #     Compiler::IndexSetStmt.new(
#     #       "arr4",
#     #       Compiler::VariableExpr.new("i"),
#     #       Compiler::BinaryExpr.new(
#     #         Compiler::IndexGetExpr.new(
#     #           "arr1",
#     #           Compiler::VariableExpr.new("i")
#     #         ),
#     #         Compiler::BinaryExpr::Operation::Division,
#     #         Compiler::IndexGetExpr.new(
#     #           "arr2",
#     #           Compiler::VariableExpr.new("i")
#     #         ),
#     #       )
#     #     ),
#     #     Compiler::AssignmentStmt.new(
#     #       "i",
#     #       Compiler::BinaryExpr.new(
#     #         Compiler::VariableExpr.new("i"),
#     #         Compiler::BinaryExpr::Operation::Addition,
#     #         Compiler::IntegerExpr.new(1),
#     #       )
#     #     ),
#     #   ]
#     # ),

#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putString", [Compiler::StringExpr.new("")] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr1", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr1", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr2", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr2", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr3", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr3", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr4", Compiler::IntegerExpr.new(0))] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInteger", [Compiler::IndexGetExpr.new("arr4", Compiler::IntegerExpr.new(1))] of Compiler::Expr
#     #   )
#     # ),

#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putFloat",
#     #     [
#     #       Compiler::CallExpr.new(
#     #         "_sqrt",
#     #         [
#     #           Compiler::IntegerExpr.new(100),
#     #         ] of Compiler::Expr
#     #       ),
#     #     ] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::AssignmentStmt.new(
#     #   "test1", Compiler::CallExpr.new("getString", [] of Compiler::Expr)
#     # ),
#     # Compiler::AssignmentStmt.new(
#     #   "test2", Compiler::CallExpr.new("getString", [] of Compiler::Expr)
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "printf", [
#     #     Compiler::StringExpr.new("%s cmp %s = %d\n"),
#     #     Compiler::VariableExpr.new("test1"),
#     #     Compiler::VariableExpr.new("test2"),
#     #     Compiler::CallExpr.new(
#     #       "strcmp", [
#     #       Compiler::VariableExpr.new("test1"),
#     #       Compiler::VariableExpr.new("test2"),
#     #     ] of Compiler::Expr
#     #     ),
#     #   ] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putBool", [Compiler::BinaryExpr.new(
#     #     Compiler::VariableExpr.new("test1"),
#     #     Compiler::BinaryExpr::Operation::Equal,
#     #     Compiler::VariableExpr.new("test2")
#     #   )] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putBool", [Compiler::BinaryExpr.new(
#     #     Compiler::VariableExpr.new("test1"),
#     #     Compiler::BinaryExpr::Operation::NotEqual,
#     #     Compiler::VariableExpr.new("test2")
#     #   )] of Compiler::Expr
#     #   )
#     # ),

#     # Compiler::AssignmentStmt.new(
#     #   "index", Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
#     # ),
#     # Compiler::AssignmentStmt.new(
#     #   "value", Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
#     # ),
#     # Compiler::IndexSetStmt.new(
#     #   "test", Compiler::VariableExpr.new("index"), Compiler::VariableExpr.new("value")
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(0))] of Compiler::Expr,
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(1))] of Compiler::Expr,
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(2))] of Compiler::Expr,
#     #   )
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(3))] of Compiler::Expr,
#     #   )
#     # ),

#     # Compiler::AssignmentStmt.new(
#     #   "test", Compiler::StringExpr.new("hello world")
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putString", [Compiler::VariableExpr.new("test")] of Compiler::Expr,
#     #   )
#     # ),
#     # Compiler::AssignmentStmt.new(
#     #   "test", Compiler::IntegerExpr.new(10)
#     # ),
#     # Compiler::ExpressionStmt.new(
#     #   Compiler::CallExpr.new(
#     #     "putInt", [Compiler::VariableExpr.new("test")] of Compiler::Expr
#     #   )
#     # ),
#     # Compiler::LoopStmt.new(
#     #   Compiler::AssignmentStmt.new(
#     #     "test", Compiler::IntegerExpr.new(0),
#     #   ),
#     #   Compiler::BinaryExpr.new(
#     #     Compiler::VariableExpr.new("test"),
#     #     Compiler::BinaryExpr::Operation::LessThan,
#     #     Compiler::IntegerExpr.new(3),
#     #   ),
#     #   [
#     #     Compiler::ExpressionStmt.new(
#     #       Compiler::CallExpr.new(
#     #         "putInt", [Compiler::VariableExpr.new("test")] of Compiler::Expr,
#     #       )
#     #     ),
#     #     Compiler::AssignmentStmt.new(
#     #       "test", Compiler::BinaryExpr.new(
#     #       Compiler::VariableExpr.new("test"),
#     #       Compiler::BinaryExpr::Operation::Addition,
#     #       Compiler::IntegerExpr.new(1),
#     #     )
#     #     ),
#     #   ] of Compiler::Stmt
#     # ),
#   ] of Compiler::Stmt,
# )

generator.generate program

generator.mod.dump

puts "========"

# generator.optimize

# generator.mod.dump

# puts "========"

generator.mod.verify

LLVM::JITCompiler.new generator.mod do |jit|
  func_ptr = jit.get_pointer_to_global(generator.function)
  func_proc = Proc(Int32).new(func_ptr, Pointer(Void).null)
  puts "exited with #{func_proc.call}"
end

# LLVM.init_aarch64

# ctx = LLVM::Context.new

# mod = ctx.new_module("mod")

# builder1 = nil

# func1 = mod.functions.add("foo", [ctx.int32, ctx.int32], ctx.int32)

# func1.basic_blocks.append do |b|
#   builder1 = b
# end

# builder2 = nil

# func2 = mod.functions.add("bar", [ctx.int32, ctx.int32], ctx.int32)

# func2.basic_blocks.append do |b|
#   builder2 = b
# end

# # ## Bar
# l, r = func2.params

# lr = builder2.not_nil!.sub l, r

# builder2.not_nil!.ret lr

# # ## Foo
# l, r = func1.params

# lr = builder1.not_nil!.call(LLVM::Type.function([ctx.int32, ctx.int32], ctx.int32), func2, [l, r])

# builder1.not_nil!.ret lr

# mod.verify

# mod.dump

# LLVM::JITCompiler.new mod do |jit|
#   func_ptr = jit.get_pointer_to_global(func1)
#   func_proc = Proc(Int32, Int32, Int32).new(func_ptr, Pointer(Void).null)
#   puts "exited with #{func_proc.call(20, 15)}"
# end

# statements = [
# Compiler::FunctionDefinitionStmt.new(
#   "add2",
#   Compiler::Function.new(
#     ["x", "y"],
#     Compiler::BinaryExpr.new(
#       Compiler::VariableExpr.new("x"),
#       Compiler::BinaryExpr::Operation::Addition,
#       Compiler::VariableExpr.new("y")
#     )
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     # "x", Compiler::FloatExpr.new(5.5)
#     "x", Compiler::IntegerExpr.new(15)
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     # "y", Compiler::IntegerExpr.new(4616189618054758400) # FP rep of 4.0
#     "y", Compiler::IntegerExpr.new(40)
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("x : int = ")] of Compiler::Expr
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "x", Compiler::CallExpr.new("getInt", [] of Compiler::Expr),
#   )
# ),
# Compiler::AssignmentStmt.new(
#   "x", Compiler::CallExpr.new(
#   "getBool", [] of Compiler::Expr
# ),
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("y : int = ")] of Compiler::Expr
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "y", Compiler::CallExpr.new("getInt", [] of Compiler::Expr),
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "y", Compiler::BooleanExpr.new(true),
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "putBool", [Compiler::BinaryExpr.new(
#     Compiler::VariableExpr.new("x"),
#     Compiler::BinaryExpr::Operation::BitwiseAnd,
#     Compiler::CallExpr.new(
#       "test",
#       [] of Compiler::Expr
#     )
#   )] of Compiler::Expr
#   )
# ),
# Compiler::IfStmt.new(
#   Compiler::BinaryExpr.new(
#     Compiler::VariableExpr.new("x"),
#     Compiler::BinaryExpr::Operation::LessThan,
#     Compiler::IntegerExpr.new(10),
#   ),
#   [
#     Compiler::ExpressionStmt.new(
#       Compiler::CallExpr.new(
#         "putInt", [Compiler::VariableExpr.new("x")] of Compiler::Expr
#       )
#     ),
#   ] of Compiler::Stmt,
#   [
#     Compiler::IfStmt.new(
#       Compiler::BinaryExpr.new(
#         Compiler::VariableExpr.new("x"),
#         Compiler::BinaryExpr::Operation::LessThan,
#         Compiler::IntegerExpr.new(20),
#       ),
#       [
#         Compiler::ExpressionStmt.new(
#           Compiler::CallExpr.new(
#             "putInt", [Compiler::BinaryExpr.new(Compiler::VariableExpr.new("x"), Compiler::BinaryExpr::Operation::Multiplication, Compiler::IntegerExpr.new(2))] of Compiler::Expr
#           )
#         ),
#       ] of Compiler::Stmt,
#       [
#         Compiler::ExpressionStmt.new(
#           Compiler::CallExpr.new(
#             "putInt", [Compiler::BinaryExpr.new(Compiler::VariableExpr.new("x"), Compiler::BinaryExpr::Operation::Multiplication, Compiler::IntegerExpr.new(3))] of Compiler::Expr
#           )
#         ),
#       ] of Compiler::Stmt,
#     ),
#   ] of Compiler::Stmt,
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "putInt", [Compiler::BinaryExpr.new(
#     Compiler::VariableExpr.new("x"),
#     Compiler::BinaryExpr::Operation::BitwiseAnd,
#     Compiler::VariableExpr.new("y"),
#   )] of Compiler::Expr,
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("y : double = ")] of Compiler::Expr
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "y", Compiler::CallExpr.new("getFloat", [] of Compiler::Expr),
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "result",
#     Compiler::BinaryExpr.new(
#       Compiler::CastExpr.new(
#         Compiler::VariableExpr.new("x"),
#         Compiler::Type::Double,
#       ),
#       Compiler::BinaryExpr::Operation::Addition,
#       Compiler::VariableExpr.new("y"),
#     )
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "result_add",
#     Compiler::CallExpr.new(
#       "add2",
#       [
#         Compiler::CastExpr.new(
#           Compiler::VariableExpr.new("x"),
#           Compiler::Type::Double,
#         ),
#         Compiler::VariableExpr.new("y"),
#       ] of Compiler::Expr
#     )
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("x + y = %.16f\n"), Compiler::VariableExpr.new("result")]
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("x + y = %.16f\n"), Compiler::VariableExpr.new("result_add")]
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "putFloat", [Compiler::VariableExpr.new("x")] of Compiler::Expr
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "test_bool", Compiler::BooleanExpr.new(false)
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "test_str", Compiler::StringExpr.new("Hello world")
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new("z",
#     Compiler::BinaryExpr.new(
#       Compiler::BinaryExpr.new(
#         Compiler::VariableExpr.new("x"),
#         Compiler::BinaryExpr::Operation::Multiplication,
#         Compiler::VariableExpr.new("x"),
#       ),
#       Compiler::BinaryExpr::Operation::Division,
#       Compiler::VariableExpr.new("y"),
#     )
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new("z",
#     Compiler::BinaryExpr.new(
#       Compiler::VariableExpr.new("x"),
#       Compiler::BinaryExpr::Operation::Addition,
#       Compiler::VariableExpr.new("y"),
#     )
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new("w",
#     Compiler::CallExpr.new(
#       "add2", [Compiler::VariableExpr.new("x"), Compiler::VariableExpr.new("y")] of Compiler::Expr
#     )
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new("w",
#     Compiler::BinaryExpr.new(
#       Compiler::VariableExpr.new("x"),
#       Compiler::BinaryExpr::Operation::Multiplication,
#       Compiler::BinaryExpr.new(
#         Compiler::VariableExpr.new("x"),
#         Compiler::BinaryExpr::Operation::Division,
#         Compiler::VariableExpr.new("y"),
#       ),
#     )
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "x", Compiler::NumberExpr.new(Math::PI / 2.0)
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::AssignmentExpr.new(
#     "doublex", Compiler::CallExpr.new("add2", [Compiler::VariableExpr.new("x"), Compiler::VariableExpr.new("x")] of Compiler::Expr)
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("%.16f %d\n"), Compiler::VariableExpr.new("doublex"), Compiler::BooleanExpr.new(true)] of Compiler::Expr
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("%.16f\n"), Compiler::VariableExpr.new("z")] of Compiler::Expr
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("%.16f\n"), Compiler::VariableExpr.new("w")] of Compiler::Expr
#   )
# ),
# Compiler::ExpressionStmt.new(
#   Compiler::CallExpr.new(
#     "printf", [Compiler::StringExpr.new("%i\n"), Compiler::CallExpr.new("getInt", [] of Compiler::Expr)] of Compiler::Expr
#   )
# ),
# ] of Compiler::Stmt
