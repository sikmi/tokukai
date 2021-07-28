n = gets.chomp.to_i

takoyaki = []

n.times do
  takoyaki << gets.chomp.to_i
end

puts takoyaki.sort[0]