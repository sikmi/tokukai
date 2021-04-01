def main
  starting_numbers = [12, 20, 0, 6, 1, 17, 7]
  # starting_numbers = [0, 3, 6]
  counts = Hash.new(0)
  spoken_turns = Hash.new do |h, k|
    h[k] = []
  end

  prev = nil
  30000000.times do |i|
    turn = i + 1
    if turn <= starting_numbers.size
      num = starting_numbers[i]
      counts[num] += 1
      spoken_turns[num] << turn
      prev = num
    else
      prev =
        if counts[prev] <= 1
          current = 0
          counts[current] += 1
          spoken_turns[current] << turn

          current
        else
          x, y = spoken_turns[prev].last(2)
          current = y - x

          counts[current] += 1
          spoken_turns[current] << turn

          current
        end
    end
    # p [:prev, turn, prev]
  end
  puts prev
end

main