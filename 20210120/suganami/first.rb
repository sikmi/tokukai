def water_count_without_zero(input)
  water_array = Array.new(input.length, 0)
  number_arrays = input.split('').map(&:to_i)

  prev = max_input = number_arrays[0]
  start_index = 1

  (1...number_arrays.length).each do |i|
    current = number_arrays[i]
    prev = number_arrays[i - 1]

    if prev < current
      (start_index...i).each do |index|
        water_array[index] = [[max_input, current].min - number_arrays[index], water_array[index]].max
      end
      
      if max_input <= current
        max_input = current
        start_index = i
      end
    end
  end

  # pp water_array
  water_array.sum
end

def result(input)
  number_arrays = input.split('0')
  number_arrays.map{ |input| water_count_without_zero(input) }.sum
end

def test(no, q, expected)
  puts "# #{no}"
  actual = result(q).to_s
  puts actual
  puts "------------------------------------------------------------------------------------------------------------"
  unless actual == expected
    puts "q: #{q}, expected: #{expected}, but #{actual}"
  end
end

def main
  test(0, "83141310145169154671122", "24" );
  test(1, "923111128", "45" );
  test(2, "923101128", "1" );
  test(3, "903111128", "9" );
  test(4, "3", "0" );
  test(5, "31", "0" );
  test(6, "412", "1" );
  test(7, "3124", "3" );
  test(8, "11111", "0" );
  test(9, "222111", "0" );
  test(10, "335544", "0" );
  test(11, "1223455321", "0" );
  test(12, "000", "0" );
  test(13, "000100020003121", "1" );
  test(14, "1213141516171819181716151413121", "56" );
  test(15, "712131415161718191817161514131216", "117" );
  test(16, "712131405161718191817161514031216", "64" );
  test(17, "03205301204342100", "1" );
  test(18, "0912830485711120342", "18" );
  test(19, "1113241120998943327631001", "20" );
  test(20, "7688167781598943035023813337019904732", "41" );
  test(21, "2032075902729233234129146823006063388", "79" );
  test(22, "8323636570846582397534533", "44" );
  test(23, "2142555257761672319599209190604843", "41" );
  test(24, "06424633785085474133925235", "51" );
  test(25, "503144400846933212134", "21" );
  test(26, "1204706243676306476295999864", "21" );
  test(27, "050527640248767717738306306596466224", "29" );
  test(28, "5926294098216193922825", "65" );
  test(29, "655589141599534035", "29" );
  test(30, "7411279689677738", "34" );
  test(31, "268131111165754619136819109839402", "102" );
end

main