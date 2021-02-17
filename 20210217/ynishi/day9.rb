
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
  nums = input_stdin_or("day9_sample.dat") do |f|
    f.readlines.map(&:chomp).map(&:to_i)
  end
  nums
end

def valid?(preamble, val)
  valid_numbers = preamble.combination(2).map {|a, b| a + b}.uniq
  valid_numbers.include? val
end

def main
  values = read_data
  preamble_size = 25
  start_index = preamble_size

  # p [:values, values]
  i = start_index
  while i < values.size
    preamble = values[(i - preamble_size) .. (i - 1)]
    # p [:preamble, preamble, values[i]]
    unless valid?(preamble, values[i])
      puts values[i]
      break
    end
    i += 1
  end
end

main
