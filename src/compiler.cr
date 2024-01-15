require "./compiler/requires"
require "./compiler/*"

puts Compiler::TokenType::Plus
puts Compiler::Token.new 1, 1

LLVM.init_aarch64

ctx = LLVM::Context.new

mod = ctx.new_module("main_mod")

func = mod.functions.add "sum", [ctx.int32, ctx.int32], ctx.int32 do |function|
  function.basic_blocks.append do |builder|
    l, r = function.params

    lr = builder.add l, r

    builder.ret lr
  end
end

func2 = mod.functions.add "addThree", [ctx.int32, ctx.int32, ctx.int32], ctx.int32 do |function|
  function.basic_blocks.append do |builder|
    p1, p2, p3 = function.params

    p1p2 = builder.call LLVM::Type.function([ctx.int32, ctx.int32], ctx.int32), mod.functions["sum"], [p1, p2]

    p1p2p3 = builder.call LLVM::Type.function([ctx.int32, ctx.int32], ctx.int32), mod.functions["sum"], [p1p2, p3]

    builder.ret p1p2p3
  end
end

mod.dump

jit = LLVM::JITCompiler.new(mod)
func_ptr = jit.get_pointer_to_global(func2)
func_proc = Proc(Int32, Int32, Int32, Int32).new(func_ptr, Pointer(Void).null)
pp func_proc.call(4, 5, 6)
