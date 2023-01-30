def check(string)
  reg = /^[A-Z][1-9]\d{5}[A-Z]$/.freeze

  if string.match?(reg)
    'Yes'
  else
    'No'
  end
end

def main
  test(1, 'A111111B', 'Yes')
  test(4, 'A100000B', 'Yes')
  test(5, 'A999999B', 'Yes')
  test(2, 'A011111B', 'No') # 100000〜999999に入らない
  test(3, 'AB111111B', 'No') # 前2つNG
  test(6, 'A11111B', 'No') # 5桁NG
  test(7, 'A111111BC', 'No') # 後ろ2つNG
  test(8, 'A1111111BC', 'No') # 後ろ2つ + 7桁 NG
  test(9, 'X900000', 'No') # 後ろのアルファベットがない NG
  test(7, 'A1111111B', 'No') # 7桁NG
end

def test(no, string, expected)
  puts "# #{no}"
  actual = check(string)
  puts actual
  puts "------------------------------------------------------------------------------------------------------------"
  unless actual == expected
    puts "string: #{string}, expected: #{expected}, but #{actual}"
  end
end

main
