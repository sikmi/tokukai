lines = []

File.readlines(File.dirname(__FILE__) + '/day8.txt').each do |line|
  lineary = line.chomp!.split(' ')
  lineary.push(0)
  lineary[1] = lineary[1].to_i
  lines << lineary
end

ans = 0
indexnum = 0

while lines[indexnum][2] == 0 do
  if lines[indexnum][0] == "acc"
    ans += lines[indexnum][1]
    lines[indexnum][2] = 1
    indexnum += 1
  elsif lines[indexnum][0] == "nop"
    lines[indexnum][2] = 1
    indexnum += 1
  elsif lines[indexnum][0] == "jmp"
    lines[indexnum][2] = 1
    indexnum += lines[indexnum][1]
  end
end

p ans