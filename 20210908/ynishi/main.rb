# 各レールパネルの各線路から次にいける線路へのマッピング情報
$rails = {
  1 => {
    0 => [0, 1],
    1 => [1, 2],
    2 => [2]
  },
  2 => {
    0 => [0, 2],
    1 => [1],
    2 => [1, 2]
  },
  3 => {
    0 => [0, 2],
    1 => [0, 1],
    2 => [2]
  },
  4 => {
    0 => [0],
    1 => [0, 1],
    2 => [1, 2]
  },
  5 => {
    0 => [0],
    1 => [1, 2],
    2 => [0, 2]
  },
  6 => {
    0 => [0, 1],
    1 => [1],
    2 => [0, 2]
  },
  7 => {
    0 => [0],
    1 => [],
    2 => [2]
  },
  8 => {
    0 => [],
    1 => [1],
    2 => [2]
  },
  9 => {
    0 => [0],
    1 => [1],
    2 => []
  },
}

def try_checks
  test( "1728398", "bc" );
  test( "789", "-" );
  test( "274", "ac" );
  test( "185", "abc" );
  test( "396", "ab" );
  test( "1278", "abc" );
  test( "7659832", "a" );
  test( "178", "bc" );
  test( "189", "ab" );
  test( "197", "a" );
  test( "278", "ac" );
  test( "289", "bc" );
  test( "297", "a" );
  test( "378", "ac" );
  test( "389", "b" );
  test( "397", "ab" );
  test( "478", "c" );
  test( "489", "bc" );
  test( "497", "ab" );
  test( "578", "bc" );
  test( "589", "b" );
  test( "597", "ac" );
  test( "678", "c" );
  test( "689", "ab" );
  test( "697", "ac" );
  test( "899", "b" );
  test( "7172", "ac" );
  test( "54787", "bc" );
  test( "83713", "bc" );
  test( "149978", "-" );
  test( "159735", "abc" );
  test( "1449467", "abc" );
  test( "9862916", "b" );
  test( "96112873", "ab" );
  test( "311536789", "-" );
  test( "281787212994", "abc" );
  test( "697535114542", "ac" );
end

# 実際にいけるかどうかチェックする
def solve_helper(cur, rest, pos)
  # p [:args, cur, rest, pos, rest.empty?]
  if rest.empty? && !$rails[cur][pos].empty?
    # 次にパネルがなく、かつ、そのパネルの立ち位置から先にいける状態ならOK
    return true
  else
    new_cur, *new_rest = rest
    # p [:candidates, $rails[cur][pos]]
    # 候補を順にためしていけるだけいく
    $rails[cur][pos].each do |next_pos|
      # p [:step, "rail: #{cur}, #{pos} -> #{next_pos}"]
      if solve_helper(new_cur, new_rest, next_pos)
        # もこのルートでいけたならOKなのでtrueで返す
        return true
      end
    end
  end
  false
end

def solve(route)
  ret = ('a' .. 'c').each_with_index.filter_map { |char, pos|
    # p [:check_start, char, pos]
    current, *rest = route
    result = solve_helper(current, rest, pos)
    # p [:result, result]
    if result
      char
    end
  }.join

  if ret == ""
    "-"
  else
    ret
  end
end

def test(route, expected)
  # p [:route, route, route.chars.map(&:to_i)]
  # p [:expected, expected]

  actual = solve(route.chars.map(&:to_i))

  puts "#{sprintf("%-15s", route)} expected: #{sprintf("%3s", expected)} actual: #{sprintf("%3s", actual)} #{expected == actual ? 'OK' : 'NG'}"
end

def main
  try_checks
end

main