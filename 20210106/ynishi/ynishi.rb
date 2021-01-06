require 'pry-byebug'

def load_passports(f=$stdin)
  val = f.read.split(/\n\n/).map {
    # 空白行ごとに分ける
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

def good?(passport)
  required_keys = %w[
    byr
    iyr
    eyr
    hgt
    hcl
    ecl
    pid
  ]

  # 必須のキーが全部あるhashならok
  required_keys.all? do |k|
    passport[k]
  end
end

def good_passports(passports)
  # validなパスポートのみ抽出
  passports.filter_map do |passport|
    good?(passport)
  end
end

def main
  passports = open('./data') do |f|
    load_passports(f)
  end

  good_items = good_passports(passports)

  p good_items.size
end

main
