
rows = [*0..127]
cols = [*0..7]
ids = []

File.open(File.dirname(__FILE__) + '/day5.txt') do |f|
  # １行毎を要素として配列化
  lines = f.readlines.map {|l| l.chop}
  lines.each do|line|
    # 初期値のセット
    thisrows = rows
    thiscols = cols
    # 1文字ずつに分割
    seats = line.split("")
    #各文字の値に応じてrow,colを分割し、最終的な座席を特定
    seats.each do |seat|
      case seat
      when "F" then
        thisrows = thisrows.slice(0..thisrows.size/2 - 1)
      when "B" then
        thisrows = thisrows.slice((thisrows.size/2)..thisrows.size)
      when "L" then
        thiscols = thiscols.slice(0..thiscols.size/2 - 1)
      when "R" then
        thiscols = thiscols.slice((thiscols.size/2)..thiscols.size)
      end
      # 特定した座席からIDを計算し、代入
      ids << thisrows[0] * 8 + thiscols[0]
    end
  end
  # 最大値を表示
  p ids.max
end
