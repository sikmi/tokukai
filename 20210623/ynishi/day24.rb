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


$reduce_mappings = {
  %i[ne se] => [:e],
  %i[ne w] => [:nw],
  %i[e nw] => [:ne],
  %i[e sw] => [:se],
  %i[se ne] => [:e],
  %i[se w] => [:sw],
  %i[sw e] => [:se],
  %i[sw nw] => [:w],
  %i[w se] => [:sw],
  %i[w ne] => [:nw],
  %i[nw sw] => [:w],
  %i[nw e] => [:ne],
}

def reduce_move(moves)
  $reduce_mappings[moves]
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

def normalize(moves)
  return [] if moves == [nil]
  # p [:moves, moves]
  first, second, *rest = moves
  result = reduce_move([first, second])
  if result
    normalize(result + rest)
  else
    [first] + normalize([second] + rest)
  end
end

def normalize_list(move_list)
  move_list.map do |moves|
    tmp = moves
    loop do
      tmp2 = normalize(tmp)
      break if tmp2 == tmp
      tmp = tmp2
    end
    tmp
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
