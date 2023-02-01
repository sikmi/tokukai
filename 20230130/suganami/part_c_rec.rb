def check_song_finished(songs, index, start, target)
  current = songs[index]
  finish = start + current

  if finish > target
    return index + 1, target - start
  end

  check_song_finished(songs, (index + 1) % songs.length, finish, target)
end

def main
  n, target = gets.split(' ').map(&:to_i)
  songs = gets.split(' ').map(&:to_i)

  check_song_finished(songs, 0, 0, target)
end

puts main.join(" ")

# 出力結果
# $ ruby part_c_rec.rb                                                                                                                           +[main]
# 3 600
# 180 240 120
# [1, 60]

# $ ruby part_c_rec.rb                                                                                                                                                        [main]
# 3 281
# 94 94 94
# [3, 93]

# $ ruby part_c_rec.rb                                                                                                                                                        [main]
# 10 5678912340
# 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000
# [6, 678912340]

