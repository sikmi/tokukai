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

class Position
  attr_accessor :east, :north, :dir
  def initialize(east, north, dir)
    @east = east
    @north = north
    @dir = dir
  end

  def move(east:, north:, dir:)
    Position.new(self.east + east, self.north + north, dir)
  end

  def manhattan_distance
    @east.abs + @north.abs
  end
end

class Action
  attr_reader :value
  def initialize(value)
    @value = value
  end

  def next(pos)
    raise NotImplementedError
  end
end

class Forward < Action
  def next(pos)
    case pos.dir
    when :north
      pos.move(
        east: 0,
        north: value,
        dir: pos.dir
      )
    when :south
      pos.move(
        east: 0,
        north: -value,
        dir: pos.dir
      )
    when :east
      pos.move(
        east: value,
        north: 0,
        dir: pos.dir
      )
    when :west
      pos.move(
        east: -value,
        north: 0,
        dir: pos.dir
      )
    else
      raise "invalid dir: #{pos.dir}"
    end
  end
end

class North < Action
  def next(pos)
    pos.move(
      east: 0,
      north: value,
      dir: pos.dir
    )
  end
end

class South < Action
  def next(pos)
    pos.move(
      east: 0,
      north: -value,
      dir: pos.dir
    )
  end
end

class West < Action
  def next(pos)
    pos.move(
      east: -value,
      north: 0,
      dir: pos.dir
    )
  end
end

class East < Action
  def next(pos)
    pos.move(
      east: value,
      north: 0,
      dir: pos.dir
    )
  end
end

class Rotation < Action
  LEFT_ROTATE = {
    east: 0,
    north: 1,
    west: 2,
    south: 3,
  }

  def rotate(current_dir)
    LEFT_ROTATE.invert[(LEFT_ROTATE[current_dir] + rotate_count + 4) % 4]
  end

  def rotate_count
    (value / 90) % 4
  end

  def next(pos)
    pos.move(
      east: 0,
      north: 0,
      dir: rotate(pos.dir)
    )
  end
end

class Left < Rotation
  def rotate_count
    super
  end
end

class Right < Rotation
  def rotate_count
    -super
  end
end

def read_data
  input_stdin_or("day12.dat") do |f|
    f.readlines.map(&:chomp).map do |line|
      type, value_s = line[0], line[1..-1]
      value = value_s.to_i
      case type
      when 'N'
        North.new(value)
      when 'S'
        South.new(value)
      when 'E'
        East.new(value)
      when 'W'
        West.new(value)
      when 'L'
        Left.new(value)
      when 'R'
        Right.new(value)
      when 'F'
        Forward.new(value)
      end
    end
  end
end

def main
  actions = read_data
  goal = actions.inject(Position.new(0, 0, :east)) do |pos, action|
    action.next(pos)
  end
  p goal.manhattan_distance
end

main