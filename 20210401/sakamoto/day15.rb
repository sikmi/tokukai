# memo:配列でただ回してると処理量でpart2が死ぬ。

input = [2, 0, 1, 9, 5, 19]

2014.times do |i|
  if input.count(input.last) == 1
    input.push(0)
    next
  end

  input.reverse_each.with_index(0) do |num, diff|
    next if diff == 0
    if input.last == num
      input.push(diff)
      break
    end
  end
end

p input[2019]