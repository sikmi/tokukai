wait_limit = gets.chomp.to_i
takoyaki_quantity = gets.chomp.to_i
takoyakies = gets.chomp.split(" ").map(&:to_i)
people_quantity = gets.chomp.to_i
peoples = gets.chomp.split(" ").map(&:to_i)

if people_quantity > takoyaki_quantity
  puts "no"
  return
end


peoples.each do |people|
  sellables = takoyakies.select { |t|  people - wait_limit <= t && t <= people }
  if sellables.length == 0
    puts "no"
    return
  end
  takoyakies.delete_at(takoyakies.find_index(sellables[0]))
end

puts "yes"
