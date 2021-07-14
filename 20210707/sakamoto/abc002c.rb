# 二次元配列が勝手に生成されてる様ないけてる感じにほんとはしたい
a, b, c, d, e, f = gets.chomp.split(" ").map(&:to_i)
triangle = [[a, b], [c, d], [e, f]]

x_distance = a - 0
y_distance = b - 0


triangle.each_with_index do |(x, y), i|
  triangle[i][0] = x - x_distance
  triangle[i][1] = y - y_distance
end

ans = ((triangle[1][0] * triangle[2][1]) - (triangle[1][1] * triangle[2][0])).abs / 2.to_f

p ans