require 'set'

def read_data
  [
    *gets.split(/ /).map(&:to_i),
    *gets.split(/ /).map(&:to_i)
  ]
end

# 座標 P(r, c) に関して、
# \ 方向の効き筋を r - c
# / 方向の機器筋を r + cとして
# (r - c, r + c)
# としたものを斜め方向の相対位置X(r, c)と呼ぶ
#
# または２つの座標P1, P2があったときにP1からみてP2がざっくりどっちにあるかを下記のように判断する
# P1 = P(a, b)で、その周りにP2 = P(c, d)があるとする
# このときに、(a - c, b - d) をチェックしたときに、
#
# (2, 2)          (2, 4)
#         (3, 3)
# (4, 2)          (4, 4)
#
#
# (a - c, b - d)
#
# ( 1, 1)          ( 1, -1)
#          (0, 0)
# (-1, 1)          (-1, -1)
#
# なので、
#
# a - c <= 0  下方向
# a - c > 0   上方向
# 
# b - d <= 0  右方向
# b - d > 0   左方向
#
# みたいにざっくり、どっちかわかる。

def move(r, c, dir, delta: 1)
  # p [:move, r, c]
  case dir
  when :up
    [r - delta, c]
  when :down
    [r + delta, c]
  when :left
    [r, c - delta]
  when :right
    [r, c + delta]
  else
    raise "invalid dir: #{dir}"
  end
end

# a - c <= 0  下方向
# a - c > 0   上方向
# 
# b - d <= 0  右方向
# b - d > 0   左方向
def dir_rel(r1, c1, r2, c2)
  r_dir = if r1 - r2 <= 0
            :down
          else
            :up
          end
  c_dir = if c1 - c2 <= 0
            :right
          else
            :left
          end

  [r_dir, c_dir]
end

def x(a, b)
  [a - b, a + b]
end

def distance(r1, c1, r2, c2)
  (r1 - r2).abs + (c1 - c2).abs
end

def area3(r1, c1, d = 0, acc = Set.new)
  if d >= 3
    acc
  else
    result = Set.new
    result += area3(r1 + 1, c1    , d + 1, acc + Set.new([ [r1 + 1, c1] ])) unless acc.include?([r1 + 1, c1])
    result += area3(r1    , c1 + 1, d + 1, acc + Set.new([ [r1, c1 + 1] ])) unless acc.include?([r1, c1 + 1])
    result += area3(r1 - 1, c1    , d + 1, acc + Set.new([ [r1 - 1, c1] ])) unless acc.include?([r1 - 1, c1])
    result += area3(r1    , c1 - 1, d + 1, acc + Set.new([ [r1, c1 - 1] ])) unless acc.include?([r1, c1 - 1])
    result
  end
end

def solve(r1, c1, r2, c2)
  step = []

  rr1 = r1
  cc1 = c1

  loop do
    step << [rr1, cc1]
    if (rr1 == r2) && (cc1 == c2)
      break
    end

    if ((rr1 + cc1) == (r2 + c2)) || ((rr1 - cc1) == (r2 - c2)) || distance(rr1, cc1, r2, c2) <= 3
      step << [r2, c2]
      break
    end

    intersection = area3(rr1, cc1) & area3(r2, c2)

    if intersection.empty?
      r_dir, c_dir = dir_rel(rr1, cc1, r2, c2)

      # binding.pry
      x1 = x(rr1, cc1)
      x2 = x(r2, c2)

      case [r_dir, c_dir]
      when [:down, :left]  # 左下
        delta = (x2[0] - x1[0]).abs / 2
        tmp = move(rr1, cc1, :down, delta: delta)
        rr1, cc1 = move(*tmp, :left, delta: delta)
      when [:down, :right] # 右下
        delta = (x2[1] - x1[1]).abs / 2
        tmp = move(rr1, cc1, :down, delta: delta)
        rr1, cc1 = move(*tmp, :right, delta: delta)
      when [:up, :left]    # 左上
        delta = (x2[1] - x1[1]).abs / 2
        tmp = move(rr1, cc1, :up, delta: delta)
        rr1, cc1 = move(*tmp, :left, delta: delta)
      when [:up, :right]   # 右上
        delta = (x2[0] - x1[0]).abs / 2
        tmp = move(rr1, cc1, :up, delta: delta)
        rr1, cc1 = move(*tmp, :right, delta: delta)
      end
    else
      rr1, cc1 = intersection.first
    end
  end
  step
end

def main
  r1, c1, r2, c2 = read_data

  step = solve(r1, c1, r2, c2)
  ustep = solve(1, 1, 1, 1)

  # p [:step, step]
  p step.size - 1
end

main
