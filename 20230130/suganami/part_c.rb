def circlar_playlist
  n, target_second = gets.split(' ').map(&:to_i)
  song_seconds_array = gets.split(' ').map(&:to_i)

  calc(song_seconds_array, target_second)
end

def calc(song_seconds_array, target_second)
  total_second = 0
  (0..).each do |i|
    song_counts = song_seconds_array.count
    this_song_length = song_seconds_array[i % song_counts]
    this_song_finished_seconds = total_second + this_song_length

    if target_second < this_song_finished_seconds
      break (i + 1), target_second - total_second
    end
 
    total_second += this_song_length
  end
end

def main
  pp circlar_playlist
end

main

# 出力結果
# $ ruby part_c.rb                                                                                                                                                        [main]
# 3 281
# 94 94 94
# [3, 93]

# $ ruby part_c.rb                                                                                                                                                        [main]
# 10 5678912340
# 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000
# [6, 678912340]

