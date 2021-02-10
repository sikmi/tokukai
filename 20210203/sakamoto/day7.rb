lines = []
mybag = "shiny gold"
ans = 0
trials = []

File.readlines(File.dirname(__FILE__) + '/day7.txt').each do |line|
  lines << line.chomp
end

def bags_check(mybag,lines,ans,trials)
  checks = lines.select{ |line| line.include? mybag}
  p checks

  checks.each do |check|
    next if check.start_with?(mybag)
    check.slice!(check.index("bag")-1..check.length)
    p check
    p trials
    unless trials.include?(check)
      ans += 1
    end
    trials << check
    p ans
  end
end

bags_check("shiny gold",lines,ans,trials)

# trials.each do |trial|
#   checks = lines.select{ |line| line.include? trial}
#   p checks
#   checks.each do |check|
#     next if check.start_with?(mybag)
#     check.slice!(check.index("bag")-1..check.length)
#     unless trials.include?(check)
#       ans += 1
#     end
#     trials << check
#   end
# end
p trials
p ans
# message = "one red bags contains two blue bags"

# p message.index("bag")
# message.slice!(7..message.length)
# p message