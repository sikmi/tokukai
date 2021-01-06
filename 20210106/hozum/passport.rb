CHECK_COLS = %w[byr iyr eyr hgt hcl ecl pid]
passport_cols=[]
cnt = 0

readlines.each do |line|
  if line == "\n"
    cnt += 1 if CHECK_COLS.all?{|check_col| passport_cols.include?(check_col)}
    passport_cols =[]
  else
    passport_cols += line.split(" ").map {|col| col.split(":")[0]}
  end
end

# 最後に改行がないのでもう一度
cnt += 1 if CHECK_COLS.all?{|check_col| passport_cols.include?(check_col)}

puts cnt
