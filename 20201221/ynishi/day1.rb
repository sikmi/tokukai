# 実行方法
# cat day1.data | ruby day1.rb
nums = readlines.map(&:to_i)

def day1(nums, size)
  nums.combination(size).find_all {|ary|
    ary.sum == 2020
  }.map{|ary|
    ary.inject(:*)
  }.join("\n")
end

# part1
puts day1(nums, 2)

# part2
puts day1(nums, 3)