inputs = IO.readlines('input.txt', chomp: true)

CURRENT_TIME = inputs[0].to_i
BUSES_TIME_FREQUENCY = inputs[1].split(',').delete_if{ _1 == 'x' }.map(&:to_i)

# ex:) [14, 49, 25]

def calc_min_time(current, frequency)
  frequency * (current / frequency + 1)
end

def main
  min = BUSES_TIME_FREQUENCY.map do |frequency|
    { frequency: frequency, min_time: calc_min_time(CURRENT_TIME, frequency)}
  end.min_by{ |time| time[:min_time] }
  
  pp (min[:min_time] - CURRENT_TIME) * min[:frequency]
end

main