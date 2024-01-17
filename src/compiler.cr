require "./compiler/requires"
require "./compiler/*"

require "math"

expressions = [
  Compiler::AssignmentExpr.new(
    "x", Compiler::NumberExpr.new(Math::PI / 2.0)
  ),
  Compiler::AssignmentExpr.new(
    "y", Compiler::NumberExpr.new(Math::PI / 2.0)
  ),
  Compiler::BinaryExpr.new(
    Compiler::BinaryExpr.new(
      Compiler::BinaryExpr.new(
        Compiler::NumberExpr.new(4.0),
        Compiler::BinaryExpr::Operation::Multiplication,
        Compiler::NumberExpr.new(5.0),
      ),
      Compiler::BinaryExpr::Operation::Addition,
      Compiler::CallExpr.new("sin", [Compiler::VariableExpr.new("x")] of Compiler::Expr),
    ),
    Compiler::BinaryExpr::Operation::Division,
    Compiler::NumberExpr.new(1.0),
  ),
  Compiler::BinaryExpr.new(
    Compiler::VariableExpr.new("x"),
    Compiler::BinaryExpr::Operation::Addition,
    Compiler::VariableExpr.new("y"),
  ),
] of Compiler::Expr

variables = {} of String => Float64
functions = {"sin" => ->Math.sin(Float64)} of String => Proc(Float64, Float64)

expressions.each do |expression|
  if expression.is_a?(Compiler::AssignmentExpr)
    expression.codegen variables, functions
    pp "#{expression}"
  else
    pp "#{expression} = #{expression.codegen variables, functions}"
  end
end

generator = Compiler::CodeGenerator.new "main"

# generator.variables["x"] = generator.ctx.double.const_double Math::PI / 2

sin_type = LLVM::Type.function([generator.ctx.double], generator.ctx.double)

func = generator.mod.functions.add "sin", sin_type

generator.function_types["sin"] = sin_type

printf_type = LLVM::Type.function([generator.ctx.int8.pointer], generator.ctx.int32, varargs = true)

func = generator.mod.functions.add "printf", printf_type

generator.function_types["printf"] = printf_type

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
  pp func_proc.call
end
