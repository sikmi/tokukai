x,y = gets.chomp.split(" ").map(&:to_i)
if x > y
  p x
else
  p y
end