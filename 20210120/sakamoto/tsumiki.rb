numbers = gets.chomp.split("").map(&:to_i)
puddle = 0
p numbers



numbers.each_with_index do |num ,i|
  if i == 0
    max = num
  elsif i == 10
  else
    if num > max
      max = num
    elsif num < max
      middle = num
    elsif num == 0
      
    end
  end
end
# ary = []
# p ary.max < 2

# numbers.each do |num|
#   ary = []
  
# end
# ary2 = []
# numbers.each do |num|
#   ary = [].fill(0,0,10)
#   num.times do |i|
#     ary[i] = 1
#   end
#   ary2 << ary
# end

# ary2.each do |a|
#   p a
# end