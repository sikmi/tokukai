require_relative 'day5_lib'

def main
  addresses = input_stdin_or('./day5.dat') do |f|
    load_addresses(f)
  end

  candidates = {}
  (1..126).each do |r|
    (0 .. 7).each do |c|
      candidates[r * 8 + c] = true
    end
  end
  positions = calc_positions(addresses)
  positions.map { |r, c|
    candidates[r * 8 + c] = false
  }

  #候補(candidates)のうち、前後(idの+1, -1)がfalse(== リストにいた)ものを探す
  result = candidates.each_cons(3).find_all do |(id1, flag1), (id2, flag2), (id3, flag3)|
    !flag1 && flag2 && !flag3
  end

  # 候補マップを表示(xが候補)
  # board = (0 .. 127).map do |r|
  #   (0 .. 7).map do |c|
  #     val = case candidates[r * 8 + c]
  #     when true
  #       "x "
  #     else
  #       ". "
  #     end
  #     if c == 0
  #       val = "#{sprintf("%03d", r)}: #{val}"
  #     end
  #     val
  #   end
  # end
  # puts board.map {|r| r.join(" ")}.join("\n")
  p result.first[1][0]
end

main
