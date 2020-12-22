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

def day1(nums, size)
  my_combination(nums, size).find_all {|ary|
    ary.sum == 2020
  }.map{|ary|
    ary.inject(:*)
  }.join("\n")
end

# part1
puts day1(nums, 2)

# part2
puts day1(nums, 3)
