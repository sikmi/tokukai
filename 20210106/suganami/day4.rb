require 'set'

TARGET_SET = Set.new(%w[byr iyr eyr hgt hcl ecl pid])

def valid_passpord_keys?(passport_string)
  Set.new(passport_string.scan(/(\w+):/).flatten) & TARGET_SET == TARGET_SET
end

def main
  file_lines_array = IO.readlines('day4.txt', chomp: true)
  passport_array = []

  f = File.open("day4.txt", mode = "rt"){|f|
    f.each_line(rs=""){|line|
      passport_array << line.chomp.gsub("\n", ' ').strip
    }
  }
  passport_array.filter { |passport| valid_passpord_keys?(passport) }.count
end

p main
