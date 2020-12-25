def text_match_char?(text: '', target_char: '', overflow_index: 0)
  index = overflow_index % text.length
  text[index] == target_char
end

def async_main()
  pipe = Ractor.new do
    loop do
      Ractor.yield Ractor.recv
    end
  end

  file_lines_array = IO.readlines('day3.txt', chomp: true)
  
  workers = (1..10).map do
    Ractor.new pipe do |pipe|
      while array = pipe.take
        Ractor.yield text_match_char?(text: array[0], target_char: '#', overflow_index: 3 * array[1])
      end
    end
  end

  file_lines_array.each_with_index do |line, i|
    pipe << [line, i]
  end

  count = 0
  file_lines_array.count.times do |_|
    count += 1 if Ractor.select(*workers)[1]
  end
  pp count
end

TARGET_CHAR = '#'
EACH_RIGHT_STEPS = 3

def text_match_char?(text: '', target_char: '', overflow_index: 0)
  index = overflow_index % text.length
  text[index] == target_char
end

async_main()