words = gets.chomp.split("").delete_if{ |a| ["a", "i", "u", "e", "o"].include? a }

puts words.join