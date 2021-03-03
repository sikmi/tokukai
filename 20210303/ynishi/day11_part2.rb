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

def board_to_s(board)
  board.map { |row|
    row.map { |s|
      case s
      when :occupied
        '#'
      when :empty
        'L'
      when :floor
        '.'
      else
        raise "invalid value: #{s}"
      end
    }.join
  }.join("\n")
end

def dump_board(board)
  puts board_to_s(board)
end

def build_data(f)
  f.readlines.map(&:chomp).map do |line|
    line.chars.map do |c|
      case c
      when '#'
        :occupied
      when 'L'
        :empty
      when '.'
        :floor
      else
        raise "invalid pattern: #{c}"
      end
    end
  end
end

def read_data
  input_stdin_or("day11.dat") do |f|
    build_data(f)
  end
end

def copy_board(board)
  board.map do |row|
    row.dup
  end
end

def create_counts_array(board)
  Array.new(board.size) do
    Array.new(board[0].size, 0)
  end
end

def max_row_col(board)
  max_row = board.size - 1
  max_col = board[0].size - 1
  [max_row, max_col]
end

def count_occupied_left_to_right(board, counts)
  max_row, max_col = max_row_col(board)

  (0 .. max_row).each do |r|
    base = 0
    (0 .. max_col).each do |c|
      if board[r][c] == :occupied
        (base ... c).each do |i|
          counts[r][i] += 1
        end
        base = c
      end
    end
  end
end

def count_occupied_right_to_left(board, counts)
  max_row, max_col = max_row_col(board)

  (0 .. max_row).each do |r|
    base = max_col
    (0 .. max_col).reverse_each do |c|
      if board[r][c] == :occupied
        (c + 1 .. base).each do |i|
          counts[r][i] += 1
        end
        base = c
      end
    end
  end
end


def count_occupied_up_to_down(board, counts)
  max_row, max_col = max_row_col(board)

  (0 .. max_col).each do |c|
    base = 0
    (0 .. max_row).each do |r|
      if board[r][c] == :occupied
        (base ... r).each do |i|
          counts[i][c] += 1
        end
        base = r
      end
    end
  end
end

def count_occupied_down_to_up(board, counts)
  max_row, max_col = max_row_col(board)

  (0 .. max_col).each do |c|
    base = max_row
    (0 .. max_row).reverse_each do |r|
      if board[r][c] == :occupied
        (r + 1 .. base).each do |i|
          counts[i][c] += 1
        end
        base = r
      end
    end
  end
end

def count_occupied_right_up(board, counts)
  max_row, max_col = max_row_col(board)

  dir = [-1, 1]
 
  left = -max_row + 1
  right = max_col - 1

  (left .. right).each do |c|
    i = [max_row, c]
    base = i
    while i[0] >= 0
      r, c = i
      if 0 <= c && c <= max_col
        if board[r][c] == :occupied
          j = base
          while j != i
            r2, c2 = j
            if 0 <= c2 && c2 <= max_col
              counts[r2][c2] += 1
            end
            j = j.zip(dir).map(&:sum)
          end
          base = [r, c]
        end
      end
      i = i.zip(dir).map(&:sum)
    end
  end
end

def count_occupied_left_down(board, counts)
  max_row, max_col = max_row_col(board)

  dir = [1, -1]

  left = 1
  right = max_col + max_row - 1

  (left .. right).each do |c|
    i = [0, c]
    base = i
    while i[0] <= max_row
      r, c = i
      if 0 <= c && c <= max_col
        if board[r][c] == :occupied
          j = base
          while j != i
            r2, c2 = j
            if 0 <= c2 && c2 <= max_col
              counts[r2][c2] += 1
            end
            j = j.zip(dir).map(&:sum)
          end
          base = [r, c]
        end
      end
      i = i.zip(dir).map(&:sum)
    end
  end
end

def count_occupied_right_down(board, counts)
  max_row, max_col = max_row_col(board)

  dir = [1, 1]
 
  left = -max_row + 1
  right = max_col - 1

  (left .. right).each do |c|
    i = [0, c]
    base = i
    while i[0] <= max_row
      r, c = i
      if 0 <= c && c <= max_col
        if board[r][c] == :occupied
          j = base
          while j != i
            r2, c2 = j
            if 0 <= c2 && c2 <= max_col
              counts[r2][c2] += 1
            end
            j = j.zip(dir).map(&:sum)
          end
          base = [r, c]
        end
      end
      i = i.zip(dir).map(&:sum)
    end
  end
end

def count_occupied_left_up(board, counts)
  max_row, max_col = max_row_col(board)

  dir = [-1, -1]

  left = 1
  right = max_col + max_row - 1

  (left .. right).each do |c|
    i = [max_row, c]
    base = i
    while i[0] >= 0
      r, c = i
      if 0 <= c && c <= max_col
        if board[r][c] == :occupied
          j = base
          while j != i
            r2, c2 = j
            if 0 <= c2 && c2 <= max_col
              counts[r2][c2] += 1
            end
            j = j.zip(dir).map(&:sum)
          end
          base = [r, c]
        end
      end
      i = i.zip(dir).map(&:sum)
    end
  end
end

def main
  board = read_data
end

main
