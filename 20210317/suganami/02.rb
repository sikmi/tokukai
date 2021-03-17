inputs = IO.readlines('input.txt', chomp: true)
CURRENT_TIME = inputs[0].to_i
times = inputs[1]

offset = 0
BUSES_TIME_FREQUENCY_WITH_OFFSET = times.split(',').reduce([]) do |result_array, value|
  if value.match(/\d/)
    result_array.push({
      frequency: value.to_i,
      offset: offset
    })
  end
  offset += 1
  result_array
end

def main
  timestamp = 0
  lcm = 1

  BUSES_TIME_FREQUENCY_WITH_OFFSET.each do |bus|
    while (timestamp + bus[:offset]) % bus[:frequency] != 0
      timestamp += lcm
    end
    lcm *= bus[:frequency]
  end
  p timestamp
end

main