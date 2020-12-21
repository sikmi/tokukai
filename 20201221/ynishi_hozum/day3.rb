require 'pp'

def debug_dump_board(m)
  m.map { |row|
    row.map { |v|
      case v
      when :tree
        '#'
      when :space
        '.'
      end
    }.join
  }.join("\n")
end

def load_map
  readlines.map(&:chomp).map {|line|
    line.each_char.map {|c|
      case c
      when '#'
        :tree
      when '.'
        :space
      else
        raise 'invalid char'
      end
    }
  }
end

def move(row, col, toboggan_map)
  row_size = toboggan_map.size
  col_size = toboggan_map.first.size

  count = 0
  routes = (row_size - 1).times.map {|r|
    row = row + 1
    col = col + 3
    # p [:r_c, row, col]
    case toboggan_map[row][col % col_size]
    when :tree
      count += 1
      [row, col]
    when :space
      [row, col]
    end
  }
  [routes, count]
end

def dump_route(max_loop, routes, toboggan_map)
  row_size = toboggan_map.size
  original_col_size = toboggan_map.first.size
  col_size = max_loop * toboggan_map.first.size

  routes_hash = routes.inject({}) {|acc, (r, c)|
    acc[r] ||= {}
    acc[r][c] = :visited
    acc
  }

  # p [:row_size, row_size]
  # p [:col_size, col_size]
  row_size.times.map {|r|
    col_size.times.map {|c|
      routes_hash[r] ||= {}
      case toboggan_map[r][c % original_col_size]
      when :tree
        if routes_hash[r][c] == :visited
          'X'
        else
          '#'
        end
      when :space
        if routes_hash[r][c] == :visited
          'O'
        else
          '.'
        end
      end
    }.join
  }.join("\n")
end

def main
  toboggan_map = load_map
  # puts debug_dump_board(toboggan_map)

  routes, trees = move(0, 0, toboggan_map)

  max_col = routes.last[1]
  max_loop = (max_col.to_f / toboggan_map.first.size).ceil

  #puts dump_route(max_loop, routes, toboggan_map)
  puts trees
end

main
