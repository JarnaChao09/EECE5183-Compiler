require "./compiler/requires"
require "./compiler/*"

require "math"

# file = File.read("./spec/programs/correct/test1.src")

# scanner = Compiler::Scanner.new file

# scanner.tokens.map { |token| token.lexeme }.each do |lexeme|
#   puts lexeme
# end

generator = Compiler::CodeGenerator.new "main"

sin_type = LLVM::Type.function([generator.ctx.double], generator.ctx.double)

func = generator.mod.functions.add "sin", sin_type

generator.function_types["sin"] = sin_type

printf_type = LLVM::Type.function([generator.ctx.int8.pointer], generator.ctx.int32, varargs = true)

func = generator.mod.functions.add "printf", printf_type

generator.function_types["printf"] = printf_type

scanf_type = LLVM::Type.function([generator.ctx.int8.pointer], generator.ctx.int32, varargs = true)

func = generator.mod.functions.add "scanf", scanf_type

generator.function_types["scanf"] = scanf_type

getline_type = LLVM::Type.function([generator.ctx.int8.pointer.pointer, generator.ctx.int64.pointer, generator.ctx.pointer], generator.ctx.int64)

func = generator.mod.functions.add "getline", getline_type

generator.function_types["getline"] = getline_type

strcmp_type = LLVM::Type.function([generator.ctx.int8.pointer, generator.ctx.int8.pointer], generator.ctx.int32)

func = generator.mod.functions.add "strcmp", strcmp_type

generator.function_types["strcmp"] = strcmp_type

# malloc_type = LLVM::Type.function([],)

# func = generator.mod.functions.add "malloc",

generator.define_native_function "getInt", [] of LLVM::Type, generator.ctx.int64 do |generator, builder, function|
  integer_type = generator.ctx.int64

  integer_var = builder.alloca integer_type, "getInttmp"

  input_string = builder.global_string_pointer "%ld"

  scanf_function = generator.mod.functions["scanf"]
  scanf_function_type = generator.function_types["scanf"]

  builder.call scanf_function_type, scanf_function, [input_string, integer_var], "scanftmp"

  ret = builder.load integer_type, integer_var

  builder.ret ret
end

generator.define_native_function "getFloat", [] of LLVM::Type, generator.ctx.double do |generator, builder, function|
  float_type = generator.ctx.double

  float_var = builder.alloca float_type, "getFloattmp"

  input_string = builder.global_string_pointer "%lf"

  scanf_function = generator.mod.functions["scanf"]
  scanf_function_type = generator.function_types["scanf"]

  builder.call scanf_function_type, scanf_function, [input_string, float_var], "scanftmp"

  ret = builder.load float_type, float_var

  builder.ret ret
end

generator.define_native_function "getBool", [] of LLVM::Type, generator.ctx.int1 do |generator, builder, function|
  boolean_type = generator.ctx.int1

  getInt_function = generator.mod.functions["getInt"]
  getInt_function_type = generator.function_types["getInt"]

  input = builder.call getInt_function_type, getInt_function, [] of LLVM::Value, "getInttmp"

  ret = builder.icmp LLVM::IntPredicate::NE, input, generator.ctx.int64.const_int(0), "netmp"

  builder.ret ret
end

generator.define_native_function "getString", [] of LLVM::Type, generator.ctx.int8.pointer do |generator, builder, function|
  string_type = generator.ctx.int8.pointer

  line = builder.alloca string_type         # , "line"
  len = builder.alloca generator.ctx.int64  # , "len"
  read = builder.alloca generator.ctx.int64 # , "read"

  builder.store generator.ctx.pointer.null_pointer, line
  store = builder.store generator.ctx.int64.const_int(0), len

  # store.alignment = 8

  _stdin = builder.load generator._stdin.type, generator._stdin

  getline_function = generator.mod.functions["getline"]
  getline_function_type = generator.function_types["getline"]

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

generator.define_native_function "putFloat", [generator.ctx.double], generator.ctx.int1 do |generator, builder, function|
  input_float = function.params[0]

  output_string = builder.global_string_pointer "%.16f\n"

  printf_function = generator.mod.functions["printf"]
  printf_function_type = generator.function_types["printf"]

  printf_value = builder.call printf_function_type, printf_function, [output_string, input_float], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.define_native_function "putInt", [generator.ctx.int64], generator.ctx.int1 do |generator, builder, function|
  input_int = function.params[0]

  output_string = builder.global_string_pointer "%ld\n"

  printf_function = generator.mod.functions["printf"]
  printf_function_type = generator.function_types["printf"]

  printf_value = builder.call printf_function_type, printf_function, [output_string, input_int], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.define_native_function "putBool", [generator.ctx.int1], generator.ctx.int1 do |generator, builder, function|
  input_bool = function.params[0]

  printf_function = generator.mod.functions["printf"]
  printf_function_type = generator.function_types["printf"]

  print_value = builder.select input_bool, builder.global_string_pointer("true\n"), builder.global_string_pointer("false\n")

  printf_value = builder.call printf_function_type, printf_function, [print_value], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.define_native_function "putString", [generator.ctx.pointer], generator.ctx.int1 do |generator, builder, function|
  input_string = function.params[0]

  output_string = builder.global_string_pointer "%s\n"

  printf_function = generator.mod.functions["printf"]
  printf_function_type = generator.function_types["printf"]

  printf_value = builder.call printf_function_type, printf_function, [output_string, input_string], "printftmp"

  ret = builder.icmp LLVM::IntPredicate::SGE, printf_value, generator.ctx.int32.const_int(0), "sgetmp"

  builder.ret ret
end

generator.define_native_function "test", [] of LLVM::Type, generator.ctx.int1 do |generator, builder, function|
  printf_function = generator.mod.functions["printf"]
  printf_function_type = generator.function_types["printf"]

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

program = Compiler::Program.new(
  "testprogram",
  [
    # Compiler::VariableDeclaration.new("test1", Compiler::Type::String, true),
    # Compiler::VariableDeclaration.new("test2", Compiler::Type::String, true),

    # Compiler::VariableDeclaration.new("index", Compiler::Type::Integer, true),
    # Compiler::VariableDeclaration.new("value", Compiler::Type::Integer, true),
    # Compiler::VariableDeclaration.new("test", Compiler::Type::Integer, true, 4),

    Compiler::VariableDeclaration.new("i", Compiler::Type::Integer, true),

    Compiler::VariableDeclaration.new("arr1", Compiler::Type::Integer, true, 2),
    Compiler::VariableDeclaration.new("arr2", Compiler::Type::Integer, true, 2),
    Compiler::VariableDeclaration.new("arr3", Compiler::Type::Integer, true, 2),
    Compiler::VariableDeclaration.new("arr4", Compiler::Type::Integer, true, 2),
  ] of Compiler::Decl,
  [
    Compiler::IndexSetStmt.new(
      "arr1", Compiler::IntegerExpr.new(0), Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
    ),
    Compiler::IndexSetStmt.new(
      "arr1", Compiler::IntegerExpr.new(1), Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
    ),
    Compiler::IndexSetStmt.new(
      "arr2", Compiler::IntegerExpr.new(0), Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
    ),
    Compiler::IndexSetStmt.new(
      "arr2", Compiler::IntegerExpr.new(1), Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
    ),

    Compiler::AssignmentStmt.new(
      "arr3", Compiler::BinaryExpr.new(
      Compiler::VariableExpr.new("arr1"),
      Compiler::BinaryExpr::Operation::Addition,
      Compiler::VariableExpr.new("arr2"),
    )
    ),

    Compiler::LoopStmt.new(
      Compiler::AssignmentStmt.new(
        "i", Compiler::IntegerExpr.new(0)
      ),
      Compiler::BinaryExpr.new(
        Compiler::VariableExpr.new("i"),
        Compiler::BinaryExpr::Operation::LessThan,
        Compiler::IntegerExpr.new(2),
      ),
      [
        Compiler::IndexSetStmt.new(
          "arr4",
          Compiler::VariableExpr.new("i"),
          Compiler::BinaryExpr.new(
            Compiler::IndexGetExpr.new(
              "arr1",
              Compiler::VariableExpr.new("i")
            ),
            Compiler::BinaryExpr::Operation::Addition,
            Compiler::IndexGetExpr.new(
              "arr2",
              Compiler::VariableExpr.new("i")
            ),
          )
        ),
        Compiler::AssignmentStmt.new(
          "i", Compiler::BinaryExpr.new(
          Compiler::VariableExpr.new("i"),
          Compiler::BinaryExpr::Operation::Addition,
          Compiler::IntegerExpr.new(1),
        )
        ),
      ]
    ),

    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "printf", [Compiler::StringExpr.new("\n")] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr1", Compiler::IntegerExpr.new(0))] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr1", Compiler::IntegerExpr.new(1))] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr2", Compiler::IntegerExpr.new(0))] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr2", Compiler::IntegerExpr.new(1))] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr3", Compiler::IntegerExpr.new(0))] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr3", Compiler::IntegerExpr.new(1))] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr4", Compiler::IntegerExpr.new(0))] of Compiler::Expr
      )
    ),
    Compiler::ExpressionStmt.new(
      Compiler::CallExpr.new(
        "putInt", [Compiler::IndexGetExpr.new("arr4", Compiler::IntegerExpr.new(1))] of Compiler::Expr
      )
    ),

    # Compiler::AssignmentStmt.new(
    #   "test1", Compiler::CallExpr.new("getString", [] of Compiler::Expr)
    # ),
    # Compiler::AssignmentStmt.new(
    #   "test2", Compiler::CallExpr.new("getString", [] of Compiler::Expr)
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "printf", [
    #     Compiler::StringExpr.new("%s cmp %s = %d\n"),
    #     Compiler::VariableExpr.new("test1"),
    #     Compiler::VariableExpr.new("test2"),
    #     Compiler::CallExpr.new(
    #       "strcmp", [
    #       Compiler::VariableExpr.new("test1"),
    #       Compiler::VariableExpr.new("test2"),
    #     ] of Compiler::Expr
    #     ),
    #   ] of Compiler::Expr
    #   )
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putBool", [Compiler::BinaryExpr.new(
    #     Compiler::VariableExpr.new("test1"),
    #     Compiler::BinaryExpr::Operation::Equal,
    #     Compiler::VariableExpr.new("test2")
    #   )] of Compiler::Expr
    #   )
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putBool", [Compiler::BinaryExpr.new(
    #     Compiler::VariableExpr.new("test1"),
    #     Compiler::BinaryExpr::Operation::NotEqual,
    #     Compiler::VariableExpr.new("test2")
    #   )] of Compiler::Expr
    #   )
    # ),

    # Compiler::AssignmentStmt.new(
    #   "index", Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
    # ),
    # Compiler::AssignmentStmt.new(
    #   "value", Compiler::CallExpr.new("getInt", [] of Compiler::Expr)
    # ),
    # Compiler::IndexSetStmt.new(
    #   "test", Compiler::VariableExpr.new("index"), Compiler::VariableExpr.new("value")
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(0))] of Compiler::Expr,
    #   )
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(1))] of Compiler::Expr,
    #   )
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(2))] of Compiler::Expr,
    #   )
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putInt", [Compiler::IndexGetExpr.new("test", Compiler::IntegerExpr.new(3))] of Compiler::Expr,
    #   )
    # ),

    # Compiler::AssignmentStmt.new(
    #   "test", Compiler::StringExpr.new("hello world")
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putString", [Compiler::VariableExpr.new("test")] of Compiler::Expr,
    #   )
    # ),
    # Compiler::AssignmentStmt.new(
    #   "test", Compiler::IntegerExpr.new(10)
    # ),
    # Compiler::ExpressionStmt.new(
    #   Compiler::CallExpr.new(
    #     "putInt", [Compiler::VariableExpr.new("test")] of Compiler::Expr
    #   )
    # ),
    # Compiler::LoopStmt.new(
    #   Compiler::AssignmentStmt.new(
    #     "test", Compiler::IntegerExpr.new(0),
    #   ),
    #   Compiler::BinaryExpr.new(
    #     Compiler::VariableExpr.new("test"),
    #     Compiler::BinaryExpr::Operation::LessThan,
    #     Compiler::IntegerExpr.new(3),
    #   ),
    #   [
    #     Compiler::ExpressionStmt.new(
    #       Compiler::CallExpr.new(
    #         "putInt", [Compiler::VariableExpr.new("test")] of Compiler::Expr,
    #       )
    #     ),
    #     Compiler::AssignmentStmt.new(
    #       "test", Compiler::BinaryExpr.new(
    #       Compiler::VariableExpr.new("test"),
    #       Compiler::BinaryExpr::Operation::Addition,
    #       Compiler::IntegerExpr.new(1),
    #     )
    #     ),
    #   ] of Compiler::Stmt
    # ),
  ] of Compiler::Stmt,
)

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
