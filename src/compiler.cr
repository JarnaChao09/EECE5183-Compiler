require "./compiler/requires"
require "./compiler/*"

puts Compiler::TokenType::Plus
puts Compiler::Token.new 1, 1

LLVM.init_aarch64

ctx = LLVM::Context.new

mod = ctx.new_module("main_mod")

func = mod.functions.add "sum", [ctx.int32, ctx.int32], ctx.int32
func.basic_blocks.append("entrypoint") do |b|
  l = func.params[0]
  r = func.params[1]

  lr = b.add(l, r, "lr")

  b.ret(lr)
end

func2 = mod.functions.add("sumCaller", [ctx.int32, ctx.int32], ctx.int32)
func2.basic_blocks.append("entrypoint1") do |builder|
  a = func2.params[1]
  b = func2.params[0]

  ba = builder.call(LLVM::Type.function([ctx.int32, ctx.int32], ctx.int32), func, [a, b], "callSumTmp")

  builder.ret(ba)
end

mod.dump

jit = LLVM::JITCompiler.new(mod)
func_ptr = jit.get_pointer_to_global(func2)
func_proc = Proc(Int32, Int32, Int32).new(func_ptr, Pointer(Void).null)
pp func_proc.call(4, 5)
