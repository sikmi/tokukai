def check(s1, s2)
  s = s2.size
  (0 ... s).all? do |i|
    case
    when s1[i] == s2[i]
      true
    when s1[i] == '@' && %w(a t c o d e r @).include?(s2[i])
      true
    when s2[i] == '@' && %w(a t c o d e r @).include?(s1[i])
      true
    else
      false
    end
  end
end

def main
  ss = gets.chomp
  tt = gets.chomp

  if check(ss, tt)
    puts "You can win"
  else
    puts "You will lose"
  end
end

main