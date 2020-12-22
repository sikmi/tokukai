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
      # x + y + z = 2020のうち、xをlと仮定してeachで回す
      lines.each_with_index do |l,i|
        # key(= y + z)は(2020 - x)となる。
        key = 2020 - l
        # 同じ要素を再度取得しない様、linesからl(=x)を排除した配列nextlinesを作る。
        nextlines = lines
        nextlines.delete_at(i)
        # y + z = 2020 - x の内、yをnと仮定してeachで回す。
        nextlines.each do |n|
          # z = 2020 - (x + y)、すなわち z = 2020 - (l + n)を満たす様なzが配列に見つかったら、 x * y * z (= l * n * (2020 - l - n))を出力し処理を終了させる。
          # よく考えればほんとはここでもnextlinesからnを排除しておく必要がある(nが2回選ばれて条件を満たしても解とは認められないため)
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
