# cat day1.data | ruby day1_2.rb

def my_combination_helper(ary, num, i, acc)
  if acc.size >= num
    [acc]
  else
    ret = []
    if i < ary.size
      # i番目を使う場合
      ret += my_combination_helper(ary, num, i + 1, acc + [ary[i]])
      # i番目を使わない場合
      ret += my_combination_helper(ary, num, i + 1, acc)
    end
    ret
  end
end

def my_combination(ary, num)
  my_combination_helper(ary, num, 0, [])
end

nums = readlines.map(&:to_i)

# part1
puts my_combination(nums, 2).find_all {|x, y|
  x + y == 2020
}.map{|x, y|
  x * y
}.join("\n")

# part2
puts my_combination(nums, 3).find_all {|x, y, z|
  x + y + z == 2020
}.map{|x, y, z|
  x * y * z
}.join("\n")
