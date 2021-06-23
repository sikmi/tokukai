require 'strscan'
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
  dirs_regexp = /e|se|sw|w|nw|ne/
  input_stdin_or("./day24.dat") do |f|
    f.readlines(chomp: true).map do |line|
      dirs = []
      ss = StringScanner.new(line)
      until ss.eos?
        dirs << ss.scan(dirs_regexp)
      end
      dirs.map(&:to_sym)
    end
  end
end

def move(moves)
  moves.inject([0, 0]) do |(r, c), dir|
    case dir
    when :ne
      [r + 1, c]
    when :se
      [r, c + 1]
    when :sw
      [r - 1, c]
    when :nw
      [r, c - 1]
    when :e
      [r + 1, c + 1]
    when :w
      [r - 1, c - 1]
    else
      raise "invalid dir: #{dir}"
    end
  end
end

def main
  move_list = read_data

  puts move_list.map {|moves|
    move(moves)
  }.group_by {|e|
    e
  }.map {|_, values|
    # valuesには、同じ場所にくるパターンが個数分入ってるので、これが奇数なら黒の数
    values.size.odd? ? 1 : 0
  }.sum
end

main
