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
  input_stdin_or('./day13.dat') do |f|
    time = f.readline.chomp.to_i
    buses = f.readline.chomp.split(/,/).select{ _1 != 'x'}.map(&:to_i)
    [time, buses]
  end
end

def recent(current, bus_id)
  bus_id - current % bus_id
end

def main
  time, buses = read_data

  min = buses.map {|id| [id, recent(time, id)] }.min_by {|e| e[1]}
  p min[0] * min[1]
end

main