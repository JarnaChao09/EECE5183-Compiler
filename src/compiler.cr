require "./compiler/requires"
require "./compiler/*"

require "math"

file = File.read("./spec/programs/correct/test1.src")

scanner = Compiler::Scanner.new file

scanner.tokens.map { |token| token.lexeme }.each do |lexeme|
  puts lexeme
end

expressions = [
  Compiler::AssignmentExpr.new(
    "x", Compiler::NumberExpr.new(Math::PI / 2.0)
  ),
  Compiler::AssignmentExpr.new(
    "y", Compiler::NumberExpr.new(Math::PI / 2.0)
  ),
  # Compiler::AssignmentExpr.new(
  #   "z",
  #   Compiler::BinaryExpr.new(
  #     Compiler::BinaryExpr.new(
  #       Compiler::BinaryExpr.new(
  #         Compiler::NumberExpr.new(4.0),
  #         Compiler::BinaryExpr::Operation::Multiplication,
  #         Compiler::NumberExpr.new(5.0),
  #       ),
  #       Compiler::BinaryExpr::Operation::Addition,
  #       Compiler::CallExpr.new(
  #         "sin", [Compiler::VariableExpr.new("x")] of Compiler::Expr
  #       ),
  #     ),
  #     Compiler::BinaryExpr::Operation::Division,
  #     Compiler::NumberExpr.new(1.0),
  #   )
  # ),
  Compiler::AssignmentExpr.new(
    "xy", Compiler::CallExpr.new("add2", [Compiler::VariableExpr.new("x"), Compiler::VariableExpr.new("y")] of Compiler::Expr),
  ),
  # Compiler::CallExpr.new("printf", [Compiler::StringExpr.new("%.16f\n"), Compiler::VariableExpr.new("z")] of Compiler::Expr),
  Compiler::CallExpr.new("printf", [Compiler::StringExpr.new("%.16f\n"), Compiler::VariableExpr.new("xy")] of Compiler::Expr),
] of Compiler::Expr

variables = {} of String => Float64
functions = {
  "add2" => Compiler::Function.new(["x", "y"], Compiler::BinaryExpr.new(Compiler::VariableExpr.new("x"), Compiler::BinaryExpr::Operation::Addition, Compiler::VariableExpr.new("y"))),
} of String => Compiler::Function

expressions.each do |expression|
  expression.codegen variables, functions
  # if expression.is_a?(Compiler::AssignmentExpr)
  #   expression.codegen variables, functions
  #   pp "#{expression}"
  # else
  #   pp "#{expression} = #{expression.codegen variables, functions}"
  # end
end

generator = Compiler::CodeGenerator.new "main"

sin_type = LLVM::Type.function([generator.ctx.double], generator.ctx.double)

func = generator.mod.functions.add "sin", sin_type

generator.function_types["sin"] = sin_type

printf_type = LLVM::Type.function([generator.ctx.int8.pointer], generator.ctx.int32, varargs = true)

func = generator.mod.functions.add "printf", printf_type

generator.function_types["printf"] = printf_type

id_type = LLVM::Type.function([generator.ctx.double, generator.ctx.double], generator.ctx.double)

func = generator.mod.functions.add "add2", id_type

generator.function_types["add2"] = id_type

generator.generate expressions

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
