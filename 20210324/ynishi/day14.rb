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

class Mask
  FULL_BIT = 0b111111111111111111111111111111111111
  def initialize(mask_str)
    @mask = mask_str.reverse.chars.each_with_index.inject(Hash.new) do |acc, (ch, i)|
      case ch
      when '0'
        acc[i] = 0
      when '1'
        acc[i] = 1
      when 'X'
        # noop
      else
        raise "invalid ch: #{ch}"
      end
      acc
    end
  end

  def apply(value)
    @mask.inject(value) do |v, (bit, val)|
      b = 1 << bit
      case val
      when 1
        v | b
      when 0
        v & (FULL_BIT ^ b)
      end
    end
  end
end

class Store
  attr_reader :addr, :value
  def initialize(addr, value)
    @addr = addr
    @value = value
  end
end

def read_data
  input_stdin_or('./day14.dat') do |f|
    f.readlines.map(&:chomp).map do |line|
      case line
      when /^mask = (.+)$/
        Mask.new(Regexp.last_match(1))
      when /^mem\[(\d+)\] = (\d+)$/
        addr = Regexp.last_match(1).to_i
        value = Regexp.last_match(2).to_i
        Store.new(addr, value)
      else
        raise "invalid line: #{line}"
      end
    end
  end
end

def main
  q = read_data

  mem = Hash.new(0)
  m = nil
  q.each do |op|
    case op
    when Mask
      m = op
    else
      mem[op.addr] = m.apply(op.value)
    end
  end

  p mem.values.sum
end

main
