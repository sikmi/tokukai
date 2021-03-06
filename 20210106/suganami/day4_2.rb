
def valid_passpord_keys?(passport_string)
  passport_string.scan(/(\S+):(\S+)/).to_h.transform_keys(&:to_sym) in { byr:, iyr:, eyr:, hgt:, hcl:, ecl:, pid: }
end

def main
  file_lines_array = IO.readlines('day4.txt', chomp: true)
  passport_array = []

  File.open('day4.txt', mode = 'r') do |f|
    f.each_line(rs="\n\n") do |line|
      passport_array << line.gsub("\n", ' ').chomp
    end
  end
  passport_array.filter { |passport| valid_passpord_keys?(passport) }.count
end

p main
