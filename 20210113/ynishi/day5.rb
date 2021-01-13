require_relative 'day5_lib'

def main
  addresses = input_stdin_or('./day5.dat') do |f|
    load_addresses(f)
  end

  positions = calc_positions(addresses)
  p positions.map { |r, c|
    r * 8 + c
  }.max
end

main