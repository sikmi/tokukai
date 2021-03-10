class WayPoint
  COMMAND_REGEX = Regexp.new('(N|E|S|W|F|L|R)(\d+)')
  DIRECTIONS = ['N', 'E', 'S', 'W']

  attr_reader :left_degree, :right_degree, :way_point_direction, :point

  def initialize(left_degree, right_degree, way_point_direction, point)
    @left_degree = left_degree
    @right_degree = right_degree
    @way_point_direction = way_point_direction
    @point = point
  end

  def next(input)
    input.match(COMMAND_REGEX)

    case $1
    when 'N', 'E', 'S', 'W'
      change_degree($1, $2.to_i)
    when 'F'
      move_point($2.to_i)
    when 'L', 'R'
      turn($1, $2.to_i)
    else
      raise "invalid char!!!"
    end
  end

  private

  def change_degree(direction, count)
    case direction
    when 'N'
      case way_point_direction
      when 'N'
        WayPoint.new(left_degree + count, right_degree, way_point_direction, point)
      when 'E'
        WayPoint.new(left_degree, right_degree - count, way_point_direction, point)
      when 'S'
        WayPoint.new(left_degree - count, right_degree, way_point_direction, point)
      when 'W'
        WayPoint.new(left_degree, right_degree + count, way_point_direction, point)
      end
    when 'E'
      case way_point_direction
      when 'N'
        WayPoint.new(left_degree, right_degree + count, way_point_direction, point)
      when 'E'
        WayPoint.new(left_degree + count, right_degree, way_point_direction, point)
      when 'S'
        WayPoint.new(left_degree, right_degree - count, way_point_direction, point)
      when 'W'
        WayPoint.new(left_degree - count, right_degree, way_point_direction, point)
      end
    when 'S'
      case way_point_direction
      when 'N'
        WayPoint.new(left_degree - count, right_degree, way_point_direction, point)
      when 'E'
        WayPoint.new(left_degree, right_degree + count, way_point_direction, point)
      when 'S'
        WayPoint.new(left_degree + count, right_degree, way_point_direction, point)
      when 'W'
        WayPoint.new(left_degree, right_degree - count, way_point_direction, point)
      end
    when 'W'
      case way_point_direction
      when 'N'
        WayPoint.new(left_degree, right_degree - count, way_point_direction, point)
      when 'E'
        WayPoint.new(left_degree - count, right_degree, way_point_direction, point)
      when 'S'
        WayPoint.new(left_degree, right_degree + count, way_point_direction, point)
      when 'W'
        WayPoint.new(left_degree + count, right_degree, way_point_direction, point)
      end
    end
  end

  def turn(rotation_direction, degree = 0)
    degree_step = degree / 90

    case rotation_direction
    when 'R'
      WayPoint.new(left_degree, right_degree, rotate_directions[degree_step], point)
    when 'L'
      WayPoint.new(left_degree, right_degree, rotate_directions[-degree_step], point)
    end
  end

  def move_point(step = 0)
    x = point.x
    y = point.y

    case left_direction
    when 'N'
      new_point = Point.new(x + step * right_degree, y + step * left_degree)
    when 'E'
      new_point = Point.new(x + step * left_degree,  y - step * right_degree)
    when 'S'
      new_point = Point.new(x - step * right_degree, y - step * left_degree)
    when 'W'
      new_point = Point.new(x - step * left_degree, y + step * right_degree)
    end
    WayPoint.new(left_degree, right_degree, way_point_direction, new_point)
  end

  def rotate_directions
    DIRECTIONS.rotate(DIRECTIONS.index(way_point_direction))
  end

  def left_direction
    rotate_directions[0]
  end

  def right_direction
    rotate_directions[1]
  end
end

class Point
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def manhattan_distance
    x.abs + y.abs
  end
end

def main
  point = Point.new(0, 0)
  way_point = WayPoint.new(1, 10, 'N', point)

  inputs = IO.readlines('input.txt', chomp: true)

  inputs.each do |input|
    way_point = way_point.next(input)
  end

  pp way_point.point.x
  pp way_point.point.y
  pp way_point.point.manhattan_distance
end

main()