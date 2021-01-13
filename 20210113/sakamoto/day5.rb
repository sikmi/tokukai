
rows = [*0..127]
cols = [*0..7]
ids = []

File.open(File.dirname(__FILE__) + '/day5.txt') do |f|
  lines = f.readlines.map {|l| l.chop}
  lines.each do|line|
    thisrows = rows
    thiscols = cols
    seats = line.split("")
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
      ids << thisrows[0] * 8 + thiscols[0]
    end
  end
  p ids.max
end
