def load_passports(f=$stdin)
  # 一気に全部読んで空白行(連続した\n\n)で分けたものを加工していく
  val = f.read.split(/\n\n/).map {
    # パスポート単位に分けたものの中で改行をスペースに変換
    _1.gsub("\n", ' ')
  }.map { |passport|
    # パスポートを空白または改行で項目単位に分けて,
    # その分けたものを : で分割して key value に分ける
    passport.split.map { |kv|
      kv.split(":")
    }
  }.map { |passport|
    # [[k, v]...]
    # に分けた内容をhashに変換
    passport.inject({}) do |acc, (k, v)|
      acc.merge(k => v)
    end
  }
end

def digit?(val)
  case
  when val.size == 1
    /^[0-9]$/ =~ val
  else
    /^[1-9][0-9]+$/ =~ val
  end
end

VALIDATORS = {
  byr: -> (x) {
    digit?(x) && x.size == 4 && '1920' <= x && x <= '2002'
  },
  iyr: -> (x) {
    digit?(x) && x.size == 4 && '2010' <= x && x <= '2020'
  },
  eyr: -> (x) {
    digit?(x) && x.size == 4 && '2020' <= x && x <= '2030'
  },
  hgt: -> (x) {
    case
    when x.end_with?('cm')
      v = x[0 ... -2]
      vi = v.to_i
      digit?(v) && 150 <= vi && vi <= 193
    when x.end_with?('in')
      v = x[0 ... -2]
      vi = v.to_i
      digit?(v) && 59 <= vi && vi <= 76
    else
      false
    end
  },
  hcl: -> (x) {
    return false unless x[0] == "#"
    color = x[1 .. -1]
    /[a-f0-9]{6}/ =~ color
  },
  ecl: -> (x) {
    %w[amb blu brn gry grn hzl oth].include?(x)
  },
  pid: -> (x) {
    # leading zero がありなので digit? は使わない
    /^[0-9]{9}$/ =~ x
  },
}

def good?(passport)
  VALIDATORS.all? do |k, validator|
    val = passport[k.to_s]
    val && val.size > 0 && validator.call(val)
  end
end

def good_passports(passports)
  # validなパスポートのみ抽出
  passports.find_all do |passport|
    good?(passport)
  end
end

def pipe?
  File.pipe?($stdin)
end

def input_stdin_or(file)
  if pipe?
    yield $stdin
  else
    open(file) do |f|
      yield f
    end
  end
end

def main
  passports = input_stdin_or('./data') do |f|
    load_passports(f)
  end

  good_items = good_passports(passports)
  p good_items.size
end

main