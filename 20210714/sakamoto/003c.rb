n, k = gets.chomp.split(" ").map(&:to_i)
movies = gets.chomp.split(" ").map(&:to_i).sort!.reverse.slice(0..(k-1)).reverse

puts movies.inject(0) {|my_rate, movie_rate| (my_rate + movie_rate) / 2.to_f}

