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
  nums = input_stdin_or("day9.dat") do |f|
    f.readlines.map(&:chomp).map(&:to_i)
  end
  nums
end

def valid?(preamble, val)
  valid_numbers = preamble.combination(2).map {|a, b| a + b}.uniq
  valid_numbers.include? val
end

def invalid_num_index(values)
  preamble_size = 25
  start_index = preamble_size

  # p [:values, values]
  i = start_index
  while i < values.size
    preamble = values[(i - preamble_size) .. (i - 1)]
    # p [:preamble, preamble, values[i]]
    unless valid?(preamble, values[i])
      return i
    end
    i += 1
  end

  raise 'no invalid data'
end

def main
  values = read_data
  invalid_index = invalid_num_index(values)
  invalid_val = values[invalid_index]

  i = 0;
  buf = []
  while i < invalid_index
    buf << values[i]
    j = i + 1
    while j < invalid_index
      buf << values[j]
      s = buf.sum
      # p [:buf, buf]
      case
      when s == invalid_val
        p [:solved, buf]
        p buf.min + buf.max
        return
      when s > invalid_val
        break
      else
        j += 1
      end
    end
    buf = []
    i += 1
  end
  raise 'no answer'
end

main
