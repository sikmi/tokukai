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
  test(2, 'A011111B', 'No')
  test(3, 'AB111111B', 'No')
  test(6, 'A11111B', 'No')
  test(7, 'A111111BC', 'No')
  test(8, 'A1111111BC', 'No')
  test(9, 'X900000', 'No')
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
