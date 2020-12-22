class SolveDay1
  def initialize
  end

  def part1
    File.open(File.dirname(__FILE__) + '/day1.txt') do |f|
      lines = f.readlines.map(&:to_i)
      # 1010が何個含まれてるかで場合分け
      if lines.count(1010) == 2
        puts 1010 ** 2
        return
      end

      if lines.include?(1010)
        lines.delete(1010)
      end

      lines.each do |l|
        next unless lines.include?(2020 - l)
        puts l * (2020 - l)
        return
      end
    end
  end

  def part2
    File.open(File.dirname(__FILE__) + '/day1.txt') do |f|
      lines = f.readlines.map(&:to_i)
      lines.each_with_index do |l,i|
        key = 2020 - l
        # 同じ要素が2回選ばれない様にする。
        nextlines = lines
        nextlines.delete_at(i)
        nextlines.each do |n|
          next unless nextlines.include?(key - n)
          puts l * n * (2020 - l - n)
          return
        end
      end
    end
  end
end

# 取得する要素が4個、5個、、となっていった場合にも使いまわせる仕様になってないのがなんだかなあと思っている。
SolveDay1.new.part1
SolveDay1.new.part2
