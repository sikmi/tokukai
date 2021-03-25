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
        acc[i] = :floating
      else
        raise "invalid ch: #{ch}"
      end
      acc
    end
  end

  def calc_base_mask(value)
    # 1の部分だけ適用してfloating未対応のmask生成
    @mask.inject(value) do |v, (bit, val)|
      b = 1 << bit
      case val
      when 1
        v | b
      when 0
        v
      when :floating
        v
      end
    end
  end

  def all_addresses(value)
    base_mask = calc_base_mask(value)
    all_mask_patterns(base_mask)
  end

  def all_mask_patterns(base_mask)
    floating_index_list = @mask.select {|k, v| v == :floating}.map {|k, v| k}

    all_mask_patterns_helper(0, floating_index_list, base_mask)
  end

  def all_mask_patterns_helper(i, floating_index_list, acc)
    # p [:hoge, i, floating_index_list, acc.to_s(2)]
    if i >= floating_index_list.size
      # p [:acc, acc, acc.to_s(2)]
      [acc]
    else
      [0, 1].flat_map do |v|
        b = 1 << floating_index_list[i]
        case v
        when 1
          all_mask_patterns_helper(i + 1, floating_index_list, acc | b)
        when 0
          all_mask_patterns_helper(i + 1, floating_index_list, acc & (FULL_BIT ^ b))
        end
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
      m.all_addresses(op.addr).each do |real_addr|
        mem[real_addr] = op.value
      end
    end
  end

  p mem.values.sum
end

main
