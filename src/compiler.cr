require "./compiler/requires"
require "./compiler/*"

require "math"

# file = File.read("./spec/programs/correct/test1.src")

# scanner = Compiler::Scanner.new file

# scanner.tokens.map { |token| token.lexeme }.each do |lexeme|
#   puts lexeme
# end

statements = [
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
  #     "x", Compiler::IntegerExpr.new(5)
  #   )
  # ),
  # Compiler::ExpressionStmt.new(
  #   Compiler::AssignmentExpr.new(
  #     # "y", Compiler::IntegerExpr.new(4616189618054758400) # FP rep of 4.0
  #     "y", Compiler::FloatExpr.new(4.0)
  #   )
  # ),
  Compiler::ExpressionStmt.new(
    Compiler::CallExpr.new(
      "printf", [Compiler::StringExpr.new("x : int = ")] of Compiler::Expr
    )
  ),
  Compiler::ExpressionStmt.new(
    Compiler::AssignmentExpr.new(
      "x", Compiler::CallExpr.new("getInt", [] of Compiler::Expr),
    )
  ),
  Compiler::IfStmt.new(
    Compiler::BinaryExpr.new(
      Compiler::VariableExpr.new("x"),
      Compiler::BinaryExpr::Operation::LessThan,
      Compiler::IntegerExpr.new(10),
    ),
    [
      Compiler::ExpressionStmt.new(
        Compiler::CallExpr.new(
          "putInt", [Compiler::VariableExpr.new("x")] of Compiler::Expr
        )
      ),
    ] of Compiler::Stmt,
    [
      Compiler::IfStmt.new(
        Compiler::BinaryExpr.new(
          Compiler::VariableExpr.new("x"),
          Compiler::BinaryExpr::Operation::LessThan,
          Compiler::IntegerExpr.new(20),
        ),
        [
          Compiler::ExpressionStmt.new(
            Compiler::CallExpr.new(
              "putInt", [Compiler::BinaryExpr.new(Compiler::VariableExpr.new("x"), Compiler::BinaryExpr::Operation::Multiplication, Compiler::IntegerExpr.new(2))] of Compiler::Expr
            )
          ),
        ] of Compiler::Stmt,
        [
          Compiler::ExpressionStmt.new(
            Compiler::CallExpr.new(
              "putInt", [Compiler::BinaryExpr.new(Compiler::VariableExpr.new("x"), Compiler::BinaryExpr::Operation::Multiplication, Compiler::IntegerExpr.new(3))] of Compiler::Expr
            )
          ),
        ] of Compiler::Stmt,
      ),
    ] of Compiler::Stmt,
  ),
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
] of Compiler::Stmt

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

generator.define_native_function "putFloat", [generator.ctx.double], generator.ctx.int32 do |generator, builder, function|
  input_float = function.params[0]

  output_string = builder.global_string_pointer "%.16f\n"

  printf_function = generator.mod.functions["printf"]
  printf_function_type = generator.function_types["printf"]

  ret = builder.call printf_function_type, printf_function, [output_string, input_float], "printftmp"

  builder.ret ret
end

generator.define_native_function "putInt", [generator.ctx.int64], generator.ctx.int32 do |generator, builder, function|
  input_int = function.params[0]

  output_string = builder.global_string_pointer "%ld\n"

  printf_function = generator.mod.functions["printf"]
  printf_function_type = generator.function_types["printf"]

  ret = builder.call printf_function_type, printf_function, [output_string, input_int], "printftmp"

  builder.ret ret
end

# generator.define_native_function "ile", [generator.ctx.int64, generator.ctx.int64], generator.ctx.int1 do |generator, builder, function|
#   l, r = function.params

#   ret = builder.icmp LLVM::IntPredicate::SLE, l, r, "iletmp"

#   builder.ret ret
# end

id_type = LLVM::Type.function([generator.ctx.double, generator.ctx.double], generator.ctx.double)

func = generator.mod.functions.add "add2", id_type

generator.function_types["add2"] = id_type

generator.generate statements

generator.mod.dump

puts "========"

generator.optimize

generator.mod.dump

puts "========"

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
