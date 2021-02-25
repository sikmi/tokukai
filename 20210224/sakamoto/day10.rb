inputs = []

File.readlines(File.dirname(__FILE__) + '/day10.txt').each do |line|
  inputs << line.chomp!.to_i
end


# 昇順にして、先頭に0(スタート地点)、最後に末尾の要素＋3(ゴール地点)を追加
inputs.sort!
inputs.push(inputs[-1] + 3)
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

def part2(inputs)
  offsets = []
  # 3離れてる所で配列をsplitするための準備
  inputs.first(inputs.length - 1).each_with_index do |input, i|
    if (inputs[i+1] - input) == 3
      offsets << i
    end
  end
  # このやりかただと頭と最後のデータが飛ぶので無理やり補完している・・・（末尾はどうせ3飛びなので無視)
  splits = ([0] + offsets + [0]).each_cons(2).map{|i, j| inputs[i+1 .. j]}
  splits[0].unshift(0)
  ans = 1

  # 要素の数に応じて場合分けが何通りか決まるので、それを掛け合わせていく。
  # ただし通り数を法則化できておらず、データからlengthが5以上にならないことを確認した上で作ってるので答えは出たがちゃんとした回答とは言い難い
  splits.each_with_index do |split, i|
    if split.length <= 2
      next
    elsif split.length == 3
      ans *= 2
    elsif split.length == 4
      ans *= 4
    elsif split.length == 5
      ans *= 7
    end
    
  end
  p ans
end

part2(inputs)
