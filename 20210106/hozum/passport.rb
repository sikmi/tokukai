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

# $B:G8e$K2~9T$,$J$$$N$G$b$&0lEY(B
cnt += 1 if CHECK_COLS.all?{|check_col| passport_cols.include?(check_col)}

puts cnt
