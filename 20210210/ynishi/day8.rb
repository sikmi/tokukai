require 'set'

def pipe?(stream = $stdin)
  File.pipe?(stream)
end

def input_stdin_or(file)
  if pipe?
    yield $stdin
  else
    open(file) do |f|
      yield f
    end
  end
end

class Vm
  def initialize(instructions)
    # p [:instructions, instructions]
    @instructions = instructions
  end

  def dump_state(pc, acc)

    lines = ["acc: #{acc}"] + @instructions.each_with_index.map do |(op, arg), index|
      if index == pc
        "#{op} #{arg} <="
      else
        "#{op} #{arg}"
      end
    end

    lines.join("\n")
  end

  def run
    executed_line = Set.new
    acc = 0
    pc = 0
    while pc < @instructions.size && !executed_line.include?(pc)
      # puts "-------------------------"
      # puts dump_state(pc, acc)
      executed_line << pc
      op, arg = @instructions[pc]

      case op
      when :nop
        # noop
        pc += 1
      when :acc
        acc += arg
        pc += 1
      when :jmp
        pc += arg
      else
        raise "invalid op: #{op}"
      end

    end
    acc
  end
end

def load_instructions
  input_stdin_or("./day8.dat") do |f|
    f.readlines.map(&:chomp).map do |line|
      op_str, arg_str = line.split(/ /)
      [op_str.to_sym, arg_str.to_i]
    end
  end
end

def main
  vm = Vm.new(load_instructions)
  p vm.run
end

main