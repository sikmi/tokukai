valid = 0
passport = {}
valid_keys = %w[byr iyr eyr hgt hcl ecl pid]
File.readlines(File.dirname(__FILE__) + '/day4.txt').each do |line|
  if line == "\n"
    valid += 1 if valid_keys.all? { |key| passport.keys.include?(key) }
    passport = {}
  else
    line.split(' ').each do |kv|
      kv2 = kv.split(':')
      passport[kv2[0]] = kv[1]
    end
  end
end
valid += 1 if valid_keys.all? { |key| passport.keys.include?(key) }

puts valid
