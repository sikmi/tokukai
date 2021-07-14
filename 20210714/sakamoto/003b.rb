first = gets.chomp.split("")
second = gets.chomp.split("")

at_list = ['a', 't', 'c', 'o', 'd', 'e', 'r']

length = first.length

length.times do |i|
  if first[i] == '@' && second[i] == '@'
    next
  elsif first[i] == '@' || second[i] == '@'
    if at_list.include?(first[i]) || at_list.include?(second[i])
      next
    else
      puts "You will lose"
      return
    end
  elsif first[i] != second[i]
    puts  "You will lose"
    return
  end
end

puts "You can win"