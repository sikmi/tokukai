class Point
  attr_reader :x, :y, :direction

  def initialize(x, y, direction)
    @x = x
    @y = y
    @direction = direction
  end

  COMMAND_REGEX = Regexp.new('(N|E|S|W|F|L|R)(\d+)')
  DIRECTIONS = ['E', 'S', 'W', 'N']

  def next(input)
    input.match(COMMAND_REGEX)

    case $1
    when 'N', 'E', 'S', 'W'
      go_ahead($1, $2.to_i)
    when 'F'
      go_ahead(direction, $2.to_i)
    when 'L', 'R'
      turn($1, $2.to_i)
    else
      raise "invalid char!!!"
    end
  end

  def manhattan_distance
    x.abs + y.abs
  end

  private

  def turn(rotation_direction, degree = 0)
    degree_step = degree / 90

    case rotation_direction
    when 'R'
      Point.new(x, y, rotate_directions[degree_step])
    when 'L'
      Point.new(x, y, rotate_directions[-degree_step])
    end
  end

  def rotate_directions
    DIRECTIONS.rotate(DIRECTIONS.index(direction))
  end

  def go_ahead(command, steps)
    case command
    when 'N'
      Point.new(x, y + steps, direction)
    when 'E'
      Point.new(x + steps, y, direction)
    when 'S'
      Point.new(x, y - steps, direction)
    when 'W'
      Point.new(x - steps, y, direction)
    end
  end
end

def main
  point = Point.new(0, 0, 'E')
  inputs = IO.readlines('input.txt', chomp: true)

  inputs.each do |input|
    point = point.next(input)
  end

  pp point.x
  point.y,
  point.manhattan_distance
end

main()