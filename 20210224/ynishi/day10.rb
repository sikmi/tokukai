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

def main
  # 全部使うので小さい順に並べる
  adapters = read_data
  # p [:adapters, adapters]

  sorted = adapters.sort
  jolt = 0
  result = sorted.each_with_object(Hash.new(0)) do |val, diffs|
    d = check_diffs(jolt, val)
    diffs[d] += 1
    # p [:val, val, d, jolt]
    jolt += d
  end

  # 最後に+3が1回発生するらしい
  result[3] += 1

  # p [:result, result]
  p result[3] * result[1]
end

main