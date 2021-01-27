
def part1
ary = []
text = ""
  
  File.readlines(File.dirname(__FILE__) + '/day6.txt').each do |line|
    if line == "\n"
      text.delete!("\n")
      ary << text
      text = ""
    else
      text += line
    end
  end
  text.delete!("\n")
  ary << text

  # message = "message"
  # p message.split("")

  # p ary

  ans = 0


  ary.each do |a|
    ans += a.split("").uniq.length
  end

  p ary[0].split("")
  p ary[0].split("").uniq
  p ary[0].split("").uniq.length

  p ans
end

def part2
  
end

