# 実行方法
# cat day1.data | ruby day1.rb
nums = readlines.map(&:to_i)

# part1
puts nums.combination(2).find_all {|x, y|
  x + y == 2020
}.map{|x, y|
  x * y
}.join("\n")


# part2
puts nums.combination(3).find_all {|x, y, z|
  x + y + z == 2020
}.map{|x, y, z|
  x * y * z
}.join("\n")