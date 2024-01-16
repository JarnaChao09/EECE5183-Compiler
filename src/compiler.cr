require "./compiler/requires"
require "./compiler/*"

expression = Compiler::BinaryExpr.new(
  Compiler::BinaryExpr.new(
    Compiler::BinaryExpr.new(
      Compiler::NumberExpr.new(4.0),
      Compiler::BinaryExpr::Operation::Multiplication,
      Compiler::NumberExpr.new(5.0),
    ),
    Compiler::BinaryExpr::Operation::Addition,
    Compiler::NumberExpr.new(2.0),
  ),
  Compiler::BinaryExpr::Operation::Division,
  Compiler::NumberExpr.new(12.0),
) # (4 * 5 + 2) / 11 == 2

puts "#{expression} = #{expression.codegen}"

generator = Compiler::CodeGenerator.new "main"

generator.generate expression

generator.mod.dump

generator.optimize

generator.mod.dump

jit = LLVM::JITCompiler.new(generator.mod)
func_ptr = jit.get_pointer_to_global(generator.function)
func_proc = Proc(Float64).new(func_ptr, Pointer(Void).null)
pp func_proc.call

# {% if host_flag?(:aarch64) %}
#   LLVM.init_aarch64
# {% elsif host_flag?(:x86_64) %}
#   LLVM.init_x86
# {% end %}

# ctx = LLVM::Context.new

# mod = ctx.new_module("main_mod")

# func = mod.functions.add "sum", [ctx.int32, ctx.int32], ctx.int32 do |function|
#   function.basic_blocks.append do |builder|
#     l, r = function.params

#     lr = builder.add l, r, "lr"

#     builder.ret lr
#   end
# end

# func2 = mod.functions.add "addThree", [ctx.int32, ctx.int32], ctx.int32 do |function|
#   function.basic_blocks.append do |builder|
#     p1, p2 = function.params

#     p1p2 = builder.call LLVM::Type.function([ctx.int32, ctx.int32], ctx.int32), mod.functions["sum"], [p1, p2], "p1p2"

#     p3 = builder.add ctx.int32.const_int(6), ctx.int32.const_int(7), "p3"

#     p1p2p3 = builder.call LLVM::Type.function([ctx.int32, ctx.int32], ctx.int32), mod.functions["sum"], [p1p2, p3], "p1p2p3"

#     builder.ret p1p2p3
#   end
# end

# puts "before passes"
# mod.dump

# target_machine = LLVM::Target.first.create_target_machine(LLVM.default_target_triple)
# puts "target_machine = #{target_machine.triple}"

# LLVM::PassBuilderOptions.new do |options|
#   LLVM.run_passes(mod, "default<O3>", target_machine, options)
# end
# puts "after passes"
# mod.dump

# jit = LLVM::JITCompiler.new(mod)
# func_ptr = jit.get_pointer_to_global(func2)
# func_proc = Proc(Int32, Int32, Int32).new(func_ptr, Pointer(Void).null)
# pp func_proc.call(4, 5)
