# strの前から見ていて、aより大きい値を見つけたら、その文字より辞書順で一つ前の文字に変換する。
# 変換するものがなかったらnil
def find_answer(str)
  ret = str.dup
  (0 ... ret.size).each do |i|
    if ret[i] > 'a'
      ret[i] = (ret[i].codepoints[0] - 1).chr
      return ret
    end
  end

  if ret.size > 1
    return ret[0 .. -2]
  end

  return nil
end

def main
  aa = gets.chomp
  ansswer = find_answer(aa)

  if ansswer
    puts ansswer
  else
    puts '-1'
  end
end

main