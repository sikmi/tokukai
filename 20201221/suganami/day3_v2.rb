TARGET_CHAR = '#'
EACH_RIGHT_STEPS = 3

def text_match_char?(text: '', target_char: '', overflow_index: 0)
  index = overflow_index % text.length
  text[index] == target_char
end

def main()
  file_lines_array = IO.readlines('day3.txt', chomp: true)

  file_lines_array.filter_map.with_index do |line, i|
    text_match_char?(
      text: line,
      target_char: TARGET_CHAR,
      overflow_index: EACH_RIGHT_STEPS * i
    )
  end.count
end

pp "#{TARGET_CHAR} count is #{main()}"