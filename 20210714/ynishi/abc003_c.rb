def main
  nn, kk = gets.split(' ').map(&:to_i)
  rr = gets.split(' ').map(&:to_f)

  rs = rr.sort.last(kk)
  val = rs.inject(0) do |acc, rate|
    (acc + rate) / 2
  end

  p val
end

main