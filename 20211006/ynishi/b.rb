def main
  gets
  puts readlines(chomp: true).group_by(&:itself).map {|k, v|
    [k, v.size]
  }.sort_by {|_name, count|
    -count
  }.first[0]
end

main