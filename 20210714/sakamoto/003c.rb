n, k = gets.chomp.split(" ").map(&:to_i)
movies = gets.chomp.split(" ").map(&:to_i).sort!.reverse.slice(0..(k-1)).reverse

ans = 0
movies.each do |m|
  ans = (ans + m) / 2.to_f
end

puts ans