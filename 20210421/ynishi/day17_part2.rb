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


module DeepDup
  refine Array do
    def deep_dup
      map do |v|
        case v
        when Array
          v.deep_dup
        else
          v.dup
        end
      end
    end
  end
end
using DeepDup

def load_initial_state
  input_stdin_or('./day17.dat') do |f|
    f.readlines.map do |line|
      line.chomp.chars.map do |c|
        case c
        when '#'
          :active
        when '.'
          :inactive
        else
          raise "invalid char: #{c}"
        end
      end
    end
  end
end

def board_to_s(board)
  board.map { |row|
    row.map { |s|
      case s
      when :active
        '#'
      when :inactive
        '.'
      else
        raise "invalid state: #{s}"
      end
    }.join
  }.join("\n")
end

class World
  attr_reader :cubes
  def initialize(size)
    @size = size
    @cubes =
      Array.new(size) do
        Array.new(size) do
          Array.new(size) do
            Array.new(size, :inactive)
          end
        end
      end
  end

  def step
    current = cubes.deep_dup

    min_x, min_y, min_z, min_w = search_min
    max_x, max_y, max_z, max_w = search_max

    # このとき隣接するセルもあるので、一つ広く見る
    (min_x - 1 .. max_x + 1).each do |x|
      (min_y - 1 .. max_y + 1).each do |y|
        (min_z - 1 .. max_z + 1).each do |z|
          (min_w - 1 .. max_w + 1).each do |w|
            case current[x][y][z][w]
            when :active
              c = around_cubes(x, y, z, w).count {|dx, dy, dz, dw| current[x + dx][y + dy][z + dz][w + dw] == :active}
              unless c == 2 || c == 3
                cubes[x][y][z][w] = :inactive
              end
            when :inactive
              c = around_cubes(x, y, z, w).count {|dx, dy, dz, dw| current[x + dx][y + dy][z + dz][w + dw] == :active}
              if c == 3
                cubes[x][y][z][w] = :active
              end
            end
          end
        end
      end
    end
  end

  def count_active
    count = 0
    (0 ... @size).each do |x|
      (0 ... @size).each do |y|
        (0 ... @size).each do |z|
          (0 ... @size).each do |w|
            if cubes[x][y][z][w] == :active
              count += 1
            end
          end
        end
      end
    end
    count
  end

  def zero
    center = @size / 2
    [center, center, center, center]
  end

  def map_from(row_col_board)
    x0, y0, z0, w0 = zero

    (0...row_col_board.size).each do |x|
      y_len = row_col_board[x].size
      (0...y_len).each do |y|
        cubes[x0 + x][y0 + y][z0][w0] = row_col_board[x][y]
      end
    end
  end


  def dump_by_z(z)
    min_x, min_y, _ = search_min
    max_x, max_y, _ = search_max

    # p [:min, min_x, min_y]
    # p [:max, max_x, max_y]

    slice = (min_x .. max_x).map do |x|
      (min_y .. max_y).map do |y|
        cubes[x][y][z]
      end
    end
    board_to_s(slice)
  end

  def search_min
    xx, yy, zz, ww = @size, @size, @size, @size
    (0 ... @size).each do |x|
      (0 ... @size).each do |y|
        (0 ... @size).each do |z|
          (0 ... @size).each do |w|
            if cubes[x][y][z][w] == :active
              xx = x if x < xx
              yy = y if y < yy
              zz = z if z < zz
              ww = w if w < ww
            end
          end
        end
      end
    end
    [xx, yy, zz, ww]
  end

  def search_max
    xx, yy, zz, ww = -1, -1, -1, -1
    (0 ... @size).each do |x|
      (0 ... @size).each do |y|
        (0 ... @size).each do |z|
          (0 ... @size).each do |w|
            if cubes[x][y][z][w] == :active
              xx = x if x > xx
              yy = y if y > yy
              zz = z if z > zz
              ww = w if w > ww
            end
          end
        end
      end
    end
    [xx, yy, zz, ww]
  end

  def around_cubes(x, y, z, w)
    return enum_for(:around_cubes, x, y, z, w) unless block_given?

    [-1, 0, 1].flat_map do |xd|
      [-1, 0, 1].flat_map do |yd|
        [-1, 0, 1].flat_map do |zd|
          [-1, 0, 1].map do |wd|
            unless xd == 0 && yd == 0 && zd == 0 && wd == 0
              yield [xd, yd, zd, wd]
            end
          end
        end
      end
    end
  end

  def inspect
    "dummy"
  end
end

def main
  initial_state = load_initial_state
  w = World.new(40)

  w.map_from(initial_state)

  6.times { |i|
    w.step
  }

  p w.count_active
end

main
