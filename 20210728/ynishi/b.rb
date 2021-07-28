def main
  puts readlines.drop(1).map(&:chomp).map(&:to_i).min
end

main