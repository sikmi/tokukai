require 'pry-byebug'

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

REQUIRED_KEYS = %w[
  byr
  iyr
  eyr
  hgt
  hcl
  ecl
  pid
]
def good?(passport)
  # 必須のキーが全部あるhashならok
  REQUIRED_KEYS.all? do |k|
    passport[k]
  end
end

def pipe?()
  if File.pipe?(STDIN) || File.select([STDIN], [], [], 0) != nil then
    return true
  end
  false
end

def input
  if pipe?
    yield $stdin
  else
    open('./data') do |f|
      yield f
    end
  end
end

def good_passports(passports)
  # validなパスポートのみ抽出
  passports.find_all do |passport|
    good?(passport)
  end
end

def main
  passports = input do |f|
    load_passports(f)
  end

  good_items = good_passports(passports)

  p good_items.size
end

main
