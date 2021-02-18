inputs = []

File.readlines(File.dirname(__FILE__) + '/day9.txt').each do |line|
  inputs << line.chomp!.to_i
end

inputs[25..-1].each.with_index(26) do |input,i|
  preamble = inputs.slice((i-26)..(i-2))
  sumcheck = preamble.combination(2).to_a
  check = 0
  sumcheck.each do |s|
    if s.sum == input
      check += 1
      break
    end 
  end
  if check == 0
    puts input
    return
  end
end