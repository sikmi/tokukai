tribonatch = [0, 0, 1]

n = gets.chomp.to_i

if n <= 3
  puts tribonatch[n-1]
  return
end

while tribonatch.length != n do
  tribonatch.push(tribonatch.last(3).sum % 10007)
end

puts tribonatch.last
