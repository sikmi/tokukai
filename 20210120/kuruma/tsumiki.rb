## 考え方
# 壁に挟まれている空間に水が貯まるので、
# 3Dプリンターのように横に見ていって、壁に挟まれている空間を出す
# 0があるといったん切れているので、0で分断してそれぞれ
#
# Area: 全エリア
# Plate: 0で切れているのでその切れた1つずつ
#
class Area
  def initialize(seed)
    @plates = seed.split("0").map { |s| Plate.new(s) }
  end

  def water_count
    @plates.map(&:water_count).sum
  end
end

class Plate
  def initialize(seed)
    @data = seed.split(//).map(&:to_i)
  end

  # 壁を1 空間を0にしてmatrixつくる
  def matrix
    max_row = @data.max
    matrix_data = @data.map do |col|
      (1..max_row).map{|row| row <= col ? 1 : 0 }
    end
  end

  # 横にひっくり返して数えていく準備
  def yoko_matrix
    matrix.transpose
  end

  # 00100100100
  # 1に挟まれている0を数えたい
  def water_count
    yoko_matrix.map do |row|
      # puts row.inspect
      all_zero = row.select { |i| i == 0 }.count
      left_zero = row.index { |i| i == 1 }
      right_zero = row.reverse.index { |i| i == 1 }
      zero = all_zero - left_zero - right_zero
      # puts "row_water_count: #{zero}"
      zero
    end.sum
  end
end

seeds = %w(
  83141310145169154671122
  923111128
  923101128
  903111128
  3
  31
  412
  3124
  11111
  222111
  335544
  1223455321
  000
  000100020003121
  1213141516171819181716151413121
  712131415161718191817161514131216
  712131405161718191817161514031216
  03205301204342100
  0912830485711120342
  1113241120998943327631001
  7688167781598943035023813337019904732
  2032075902729233234129146823006063388
  8323636570846582397534533
  2142555257761672319599209190604843
  06424633785085474133925235
  503144400846933212134
  1204706243676306476295999864
  050527640248767717738306306596466224
  5926294098216193922825
  655589141599534035
  7411279689677738
  268131111165754619136819109839402
)

kai = %w(
  24
  45
  1
  9
  0
  0
  1
  3
  0
  0
  0
  0
  0
  1
  56
  117
  64
  1
  18
  20
  41
  79
  44
  41
  51
  21
  21
  29
  65
  29
  34
  102
).map(&:to_i)

seeds.zip(kai).each do |seed, kai|
  answer = Area.new(seed).water_count
  puts "#{seed}\t#{answer}\t#{kai == answer}"
end
