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

def read_data
  nums = input_stdin_or("day10.dat") do |f|
    f.readlines.map(&:chomp).map(&:to_i)
  end
  nums
end

def check_diffs(current, val)
  (1 .. 3).each do |d|
    if current + d == val
      return d
    end
  end
  # 1-3いずれかでマッチするはずなので、ここにきたらエラー
  raise "invalid state"
end

# 0 1 2 3 6
# n(6) = n(3)
# n(3) = n(2) + n(1) + n(0)
# n(2) = n(0) + n(1)
# n(0) = 1
# n(1) = n(0)

def count_patterns(val, s, memo)
  if val == 0
    1
  elsif memo[val]
    memo[val]
  else
    count = 0
    (1 .. 3).each do |d|
      if s.include?(val - d)
        count += count_patterns(val - d, s, memo)
      end
    end
    memo[val] = count
  end
end

def main
  adapters = read_data
  max = adapters.max

  all_adapters = adapters + [0, max + 3]
  s = Set.new(all_adapters)

  p count_patterns(all_adapters.max, s, {})
end

main
