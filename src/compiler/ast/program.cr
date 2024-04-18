require "./decl"
require "./stmt"

module Compiler
  class Program
    property program_name : String
    property declarations : Array(Decl)
    property body : Array(Stmt)

    def initialize(@program_name, @declarations, @body)
    end

    def to_s(io : IO)
      io << "program " << program_name << " is\n"
      declarations.each do |decl|
        io << decl
        io << "\n"
      end

      body.each do |stmt|
        io << stmt
        io << "\n"
      end
    end
  end

  # class Compiler::CodeGenerator
  #   def generate(builder, basic_block, program : Program) : LLVM::BasicBlock
  #     basic_block
  #   end
  # end
end
