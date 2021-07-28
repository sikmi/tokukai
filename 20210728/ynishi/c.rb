def ok?(max_wait, takoyaki, okyaku)
  okyaku.each_with_index do |k, i|
    ok = false
    while t = takoyaki.shift
      # p [k, t, k - max_wait <= t, k >= t]
      if (k - max_wait <= t) && k >= t
        ok = true
        break
      end
    end
    return false if !ok || (i != okyaku.size - 1 && takoyaki.size == 0)
  end
  true
end

def main
  tt = gets.chomp.to_i
  gets
  takoyaki_list = gets.chomp.split(' ').map(&:to_i)

  gets
  kyaku_list = gets.chomp.split(' ').map(&:to_i)

  puts ok?(tt, takoyaki_list, kyaku_list) ? 'yes' : 'no'
end

main
