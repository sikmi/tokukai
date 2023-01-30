def check_song_finished(songs, index, start, target)
  current = songs.rotate(index).first
  finish = start + current

  if finish > target
    return index + 1, target - start
  end

  check_song_finished(songs, index +1, finish, target)
end

def main
  n, target = gets.split(' ').map(&:to_i)
  songs = gets.split(' ').map(&:to_i)

  check_song_finished(songs, 0, 0, target)
end

print main

# 出力結果
# $ ruby part_c.rb                                                                                                                                                        [main]
# 3 281
# 94 94 94
# [3, 93]

# $ ruby part_c.rb                                                                                                                                                        [main]
# 10 5678912340
# 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000
# [6, 678912340]

