inputs = []

File.readlines(File.dirname(__FILE__) + '/day10.txt').each do |line|
  inputs << line.chomp!.to_i
end


# 昇順にして、先頭に0(スタート地点)、最後に末尾の要素＋3(ゴール地点)を追加
inputs.sort!
inputs[inputs.length] = inputs[-1] + 3
inputs.unshift(0)


difference1 = 0
difference3 = 0

# 次の数字との差が1のもの、3のものをカウントする。
inputs.first(inputs.length - 1).each_with_index do |input, i|
  if (inputs[i+1] - input) == 1
    difference1 += 1
  elsif (inputs[i+1] - input) == 3
    difference3 += 1
  end
end

# カウントした値の掛け算が答え
p difference1 * difference3