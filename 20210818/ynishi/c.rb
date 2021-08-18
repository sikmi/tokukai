def load_maze
  gets
  start = gets.chomp.split(' ').map(&:to_i)
  goal = gets.chomp.split(' ').map(&:to_i)

  board = $stdin.readlines(chomp: true).map do |line|
    line.chars
  end

  [start, goal, board]
end

class State
  attr_reader :pos, :prev
  def initialize(pos, prev)
    @pos = pos
    @prev = prev
  end

  def count_step
    if prev == nil
      0
    else
      1 + prev.count_step
    end
  end
end

def dump_board(pos, board)
  b = board.map(&:dup)
  r, c = pos
  b[r - 1][c - 1] = '@'
  b.map { |row|
    row.join
  }.join("\n")
end

def generate_next_pos_list(pos)
  r, c = pos
  [
    [ 1,  0], # 下
    [-1,  0], # 上
    [ 0,  1], # 右
    [ 0, -1], # 右
  ].map do |dc, dr|
    [r + dr, c + dc]
  end
end

def solve(start, goal, board)
  queue = [State.new(start, nil)]
  seen = {
    start => true
  }

  until queue.empty?
    s = queue.shift

    if s.pos == goal
      return s
    else
      generate_next_pos_list(s.pos).each do |r, c|
        pos = [r, c]
        if board[r - 1][c - 1] == '.' && !seen[pos]
          seen[pos] = true
          queue.push(State.new([r, c], s))
        end
      end
    end
  end

  # 絶対解けるらしいのでここにはこない
  raise 'no answer'
end

def extract_pos_list(state)
  if state.prev
    [state.pos] + extract_pos_list(state.prev)
  else
    [state.pos]
  end
end

def dump_route(board, state)
  pos_list = extract_pos_list(state)
  b = board.map(&:dup)

  pos_list.each_with_index do |(r, c), i|
    mark = case i
           when 0
             'G'
           when pos_list.size - 1
             'S'
           else
             '@'
           end
    b[r - 1][c - 1] = mark
  end

  b.map { |row|
    row.join
  }.join("\n")
end

def main
  start, goal, board = load_maze
  # p [:start, start]
  # p [:goal, goal]
  # p [:board, board]

  # puts "--------------------------"
  # puts dump_board(start, board)

  # puts "--------------------------"
  # puts dump_board(goal, board)
  # puts 
  s = solve(start, goal, board)
  # puts dump_route(board, s)
  p s.count_step
end

main