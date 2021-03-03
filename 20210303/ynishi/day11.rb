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

def read_data
  input_stdin_or("day11.dat") do |f|
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
end

def copy_board(board)
  board.map do |row|
    row.dup
  end
end

def max_row_col(board)
  max_row = board.size - 1
  max_col = board[0].size - 1
  [max_row, max_col]
end

# 全部の座標をyieldする
def each_rc(board)
  return enum_for(:each_rc, board) unless block_given?

  max_row, max_col = max_row_col(board)
  (0 .. max_row).each do |r|
    (0 .. max_col).each do |c|
      yield r, c
    end
  end
end

# (r, c) の八方をyieldする
def around(board, r, c)
  return enum_for(:around, board, r, c) unless block_given?

  max_row, max_col = max_row_col(board)
  # p [:max_row_col, max_row, max_col]
  (r - 1 .. r + 1).each do |rr|
    (c - 1 .. c + 1).each do |cc|
      # p [:hoge, rr, cc]
      next unless rr >= 0 && rr <= max_row
      next unless cc >= 0 && cc <= max_col
      next if rr == r && cc == c # 自分の場所は除く
      # p [:foo, rr, cc]

      yield rr, cc
    end
  end
end

# boardの状態から各座標をルールに従って変換した次の世代のboardを返す
def inc_generation(board)
  next_generation = copy_board(board)

  each_rc(board) do |r, c|
    case board[r][c]
    when :empty
      # ルール1 今emptyのとき、周りが全部occupiedならoccupiedになる
      if around(board, r, c).all? {|r, c| board[r][c] != :occupied}
        next_generation[r][c] = :occupied
      end
    when :occupied
      # ルール2 今occupiedのとき、周りが4つ以上occupiedならemptyになる
      if around(board, r, c).count{|r, c| board[r][c] == :occupied } >= 4
        next_generation[r][c] = :empty
      end
    end
  end
  next_generation
end

def count_state(board, state)
  each_rc(board).count { |r, c| board[r][c] == state }
end

def count_occupied(board)
  count_state(board, :occupied)
end

def main
  board = read_data

  prev = nil
  generation = 0
  while prev != board
    prev = copy_board(board)
    board = inc_generation(board)
    generation += 1
  end

  puts count_occupied(board)
end

main
