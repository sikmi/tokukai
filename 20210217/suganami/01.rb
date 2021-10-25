
inputs = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]

def check_invalid_number(input_array = [], count = 5)
  preanble = input_array.first(count)
  check_numbers = input_array[count..]

  check_numbers.each do |input|
    sum_array = preanble.combination(2).map(&:sum).uniq
    return input unless sum_array.include?(input)

    preanble = preanble.push(input)[-count..]
  end

  return 'No value'
end

pp check_invalid_number(inputs, 5)

number_array = IO.readlines('input.txt', chomp: true).map(&:to_i)

invalid_number = check_invalid_number(number_array, 25)

def check_multiple(inputs, invalid_number)
  (0..input_length).each do |num|
    inputs[num]
  end
end