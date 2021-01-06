require 'pry-byebug'

def load_passports(f=$stdin)
  val = f.read.split(/\n\n/).map {
    _1.gsub("\n", ' ')
  }.map { |passports|
    passports.split.map { |kv|
      kv.split(":")
    }
  }.map { |passport|
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

  required_keys.all? do |k|
    passport[k]
  end
end

def good_passports(passports)
  passports.filter_map do |passport|
    good?(passport)
  end
end

passports = load_passports(open('./data'))
good_items = good_passports(passports)

p good_items.size