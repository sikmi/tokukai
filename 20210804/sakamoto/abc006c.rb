# n, m = gets.chomp.split(" ").map(&:to_i)

# adult = 2
# old = 3
# child = 4

# ave = (m / n).to_f
# if ave > 4
#   puts "-1 -1 -1"
#   return
# elsif ave < 2
#   puts "-1 -1 -1"
#   return
# elsif ave >= 2 && ave < 3
#   (n+1).times do |a|
#     (n+1).times do |o|
#       next if n - (a + o) < 0
#       if (child * a + old * o + adult * (n - a - o) == m)
#         puts "#{a} #{o} #{n - a - o}"
#         return
#       end
#     end
#   end
# elsif ave >= 3 && ave <= 4
#   (n+1).times do |a|
#     (n+1).times do |o|
#       next if n - (a + o) < 0
#       if (adult * a + old * o + child * (n - a - o) == m)
#         puts "#{a} #{o} #{n - a - o}"
#         return
#       end
#     end
#   end
# end



# puts "-1 -1 -1"


n, m = gets.chomp.split(" ").map(&:to_i)

adult = 2
old = 3
child = 4

if child * n < m
  puts "-1 -1 -1"
  return
end

if adult * n > m
  puts "-1 -1 -1"
  return
end

(n+1).times do |a|
  (n+1).times do |o|
    break if n - (a + o) < 0
    if (adult * a + old * o + child * (n - a - o) == m)
      puts "#{a} #{o} #{n - a - o}"
      return
    end
  end
end

puts "-1 -1 -1"