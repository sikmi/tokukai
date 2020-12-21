file_lines_array = IO.readlines('day3.txt', chomp: true)

depth = file_lines_array.length

@tree_count = 0
@blank_count = 0

file_lines_array.each_with_index do |line, i|
  x = 3 * i

  @n = 1
  while (line.length * @n) <= x
    @n += 1
  end

  n_line_string = line * @n
  @tree_count += 1 if n_line_string[@n] == '#'
  @blank_count += 1 if n_line_string[@n] == '.'
end

pp "total count: #{depth}"
pp "tree count: #{@tree_count}"
pp "brank count: #{@blank_count}"