def load_board
  readlines.map(&:chomp).map { _1.split(' ') }
end

def rotate(board)
  size = board.size
  (0 ... board.size).map do |r|
    (0 ... board[r].size).map do |c|
      board[size - r - 1][size - c - 1]
    end
  end
end

def dump_board(board)
  board.map {|row| row.join(" ")}.join("\n")
end

def main
  board = load_board

  # puts dump_board(board)
  puts dump_board(rotate(board))
end

main