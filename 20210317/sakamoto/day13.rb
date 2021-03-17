
lines = File.read(File.dirname(__FILE__) + '/day13.txt')
ary = lines.chomp.split(/\n/)

timestamp = ary[0].to_i
busses = ary[1].scan(/(\d+)[^\d]*/).flatten.map(&:to_i)

count = 0
(timestamp..Float::INFINITY).each do |nearest|
  count += 1
  
  busses.each do |bus|
    if nearest % bus == 0
      puts (nearest - timestamp) * bus
      return
    end
  end

  # 無限ループしちゃった時用
  if count == 10000
    p "error"
    return
  end
end






# NOTE:質問用
# p ary[1]
# check = ary[1].scan(/(\d+)[^\d]*/).flatten.map(&:to_i)
# p check
# check = /(\d+)[^\d]*/.match(ary[1])