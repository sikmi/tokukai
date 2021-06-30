require 'pry-byebug'

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

def read_data
  input_stdin_or('day25.dat') do |f|
    f.read.lines(chomp: true).map(&:to_i)
  end
end

def secret_loop_size(subject_number, key)
  c = 0
  val = 1
  while val != key
    val = (subject_number * val) % 20201227
    c += 1
  end
  c
end

def encription_key(subject_number, count)
  val = 1
  count.times do
    val = (subject_number * val) % 20201227
  end
  val
end

def main
  public1, public2 = read_data
  p [public1, public2]

  secret_loop_size1 = secret_loop_size(7, public1)
  secret_loop_size2 = secret_loop_size(7, public2)

  p encription_key(public1, secret_loop_size2)
  p encription_key(public2, secret_loop_size1)
end

main
