use proconio::input;

fn main() {
    input! {
      n: u64,
      target: u64,
      songs: [u64;n],
    }

    let sum: u64 = songs.iter().sum();
    let real_target: u64 = if target > sum { target % sum } else { target };
    let result = check_song_finished(&songs, 0, 0, real_target);
    println!("{} {}", result.0, result.1);
}

fn check_song_finished(songs: &Vec<u64>, index: usize, start: u64, target: u64) -> (usize, u64) {
    let current = songs[index];
    let finish = start + current;

    if finish > target {
        return (index + 1, target - start);
    }

    let next = index + 1 % songs.len();

    check_song_finished(songs, next, finish, target)
}
