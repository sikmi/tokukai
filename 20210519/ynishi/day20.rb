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

def deep_dup(ary)
  ary.map do |e|
    case e
    when Array
      deep_dup(e)
    else
      e.dup
    end
  end
end

class Tile
  attr_reader :no, :tiles
  private :tiles

  def initialize(no, tile_data)
    @no = no
    @tiles = tile_data.map do |line|
      line.chars.to_a
    end
  end

  def to_s
    <<~EOS
    Tile #{no}:
    #{tiles.map {|line| line.join }.join("\n")}
    EOS
  end

  def all_patterns
    @_all_patterns ||= [*raw_rotates, *mirror_rotates]
  end

  def raw
    deep_dup(tiles)
  end

  def mirror
    raw.map(&:reverse)
  end

  def raw_rotates
    rotates(raw)
  end

  def mirror_rotates
    rotates(mirror)
  end

  def rotates(orig)
    r1 = deep_dup(rotate(orig))
    r2 = deep_dup(rotate(r1))
    r3 = deep_dup(rotate(r2))

    [orig, r1, r2, r3]
  end

  # 1 2 3
  # 4 5 6
  # 7 8 9

  # 左90回転
  # 3 6 9
  # 2 5 8
  # 1 4 7

  # 0 0 -> 2, 0
  # 0 1 -> 1, 0
  # 0 2 -> 0, 0

  # 1 0 -> 2, 1
  # 1 1 -> 1, 1
  # 1 2 -> 0, 1

  # 2 0 -> 2, 2
  # 2 1 -> 1, 2
  # 2 2 -> 0, 2
  def rotate(orig)
    rotated = Array.new(orig.size) do
      Array.new(orig.size, nil)
    end

    (0 ... orig.size).each do |r|
      (0 ... orig.size).each do |c|
        rotated[orig.size - c - 1][r] = orig[r][c]
      end
    end

    rotated
  end

  class << self
    def tiles_to_s(tiles)
      tiles.map(&:join).join("\n")
    end
  end
end

def read_tiles
  input_stdin_or('day20.dat') do |f|
    tile_data_base = f.read.split(/\n\n/)
    tile_data_base.map do |td|
      header, *data = td.lines(chomp: true)
      no = header.gsub(/^.* /, '').to_i
      Tile.new(no, data)
    end
  end
end

def solve(tiles)
  size = Math.sqrt(tiles.size).to_i

  acc = Array.new(size) do
    Array.new(size, nil)
  end

  used = Array.new(tiles.size, false)
  solve_helper(0, 0, size, tiles, used, acc)
end

def solve_helper(r, c, size, tiles, used, acc)
  # p [r, c]
  if r == size
    # p :solved
    return acc
  else
    tiles.each_with_index do |t, i|
      unless used[i]
        t.all_patterns.each_with_index do |pat, j|
          # binding.pry if [r, c, i, j] == [0, 1, 0, 6] && acc[0][0]&.at(1) == [1, 6]
          if can_put?(r, c, pat, acc)
            acc[r][c] = [pat, [i, j]]
            used[i] = true
            nr, nc = inc_rc(r, c, size)

            result = solve_helper(nr, nc, size, tiles, used, acc)
            return result if result

            acc[r][c] = nil
            used[i] = false
          end
        end
      end
    end
    false
  end
end

def can_put?(r, c, pat, board)
  if r > 0
    upper = board[r - 1][c][0]
    return false unless upper[upper.size - 1] == pat[0]
  end

  if c > 0
    left = board[r][c - 1][0]
    return false unless left.transpose.last == pat.transpose[0]
  end

  true
end

def inc_rc(r, c, size)
  if c < size - 1
    return [r, c + 1]
  end

  [r + 1, 0]
end

def main
  tiles = read_tiles
  result = solve(tiles)
  if result
    size = result.size
    answer =
      tiles[result[0][0][1].first].no *
      tiles[result[0][size - 1][1].first].no *
      tiles[result[size - 1][0][1].first].no *
      tiles[result[size - 1][size - 1][1].first].no
    p answer
  end
end

main