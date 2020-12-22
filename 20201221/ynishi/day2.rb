# 実行方法
# cat day2.data | ruby day2.rb

def load_passwords
  readlines.map(&:chomp).map { |line|
    # ["1-3 a", "abcde"]
    restriction, password = line.split(/:/).map(&:strip)

    # ["1-3", "a"]
    condition, char = restriction.split

    # [1, 3]
    min, max = condition.split(/-/).map(&:to_i)

    [min, max, char, password]
  }
end

def valid_password?(min, max, char, password)
  c = password.each_char.count { |ch|
    ch == char
  }

  min <= c && c <= max
end

valids = load_passwords.find_all {|min, max, char, password|
  valid_password?(min, max, char, password)
}

p valids.size

