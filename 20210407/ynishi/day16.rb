require 'pry-byebug'
require 'set'

def pipe?(stream = $stdin)
  File.pipe?(stream)
end

def input_stdin_or(file)
  if pipe?
    yield $stdin
  else
    open(file) do |f| yield f end
  end
end

def split_blank(ary)
  [].tap do |result|
    buf = []
    ary.each do |e|
      if e == ""
        result << buf
        buf = []
      else
        buf << e
      end
    end
    result << buf
  end
end

def read_data
  input_stdin_or('./day16.dat') do |f|
    split_blank(f.readlines.map(&:chomp))
  end
end

def parse_valid_numbers(block)
  block.inject([]) do |checkers, line|
    checkers.tap do
      raise "invalid format line: #{line}" unless /^[^:]+: (\d+-\d+) or (\d+-\d+)/ =~ line
      ranges = [
        Regexp.last_match(1),
        Regexp.last_match(2),
      ]

      ranges.each do |range|
        raise "innvalid format range: #{range}" unless /(\d+)-(\d+)/ =~ range
        s = Regexp.last_match(1).to_i
        e = Regexp.last_match(2).to_i
        checkers << ->(x) { s <= x && x <= e }
      end
    end
  end
end

def parse_numbers(block)
  # 1行目は nearby tickets: の行なので捨てる
  block.drop(1).flat_map do |line|
    line.split(/,/).map(&:to_i)
  end
end

def extract_invalid_numbers(numbers, checkers)
  numbers.find_all do |n|
    !checkers.any? do |checker|
      checker.call(n)
    end
  end
end

def main
  valid_num_block, ticket_block, nearby_block = read_data
  valid_number_checkers = parse_valid_numbers(valid_num_block)
  numbers = parse_numbers(nearby_block)
  invalid_numbers = extract_invalid_numbers(numbers, valid_number_checkers)
  p invalid_numbers.sum
end

main