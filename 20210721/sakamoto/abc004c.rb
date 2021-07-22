n = gets.chomp.to_i
card = [1, 2, 3, 4, 5, 6]

a = n / 5
b = n % 5

rotate_num = a % 6

anscard = card.rotate(rotate_num)

b.times do |i|
  first = anscard[i]
  second = anscard[i+1]
  anscard[i] = second
  anscard[i+1] = first
end

puts anscard.join