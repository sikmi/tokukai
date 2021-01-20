def dump_waters(waters)
  waters.join(" ")
end

def load_aquarium(str)
  str.chars.map(&:to_i)
end

def dump_aquarium(aquarium, waters)
  max = aquarium.max
  graph = aquarium.zip(waters).map { |jimen, mizu|
    (["*"] * jimen) + (["."] * mizu) + ([" "] * (max - (jimen + mizu)))
  }

  graph.map(&:reverse).transpose.map {|r| r.join(" ")}.join("\n")
end

def solve(str)
  aquarium = load_aquarium(str)
  if aquarium.size < 3
    return 0
  end

  # 各列に溜まっている水のマスの個数
  waters = Array.new(aquarium.size, 0)

  bottom = 0
  prev = top = aquarium[0]
  (1 ... aquarium.size).each do |c|
    cur = aquarium[c]
    if cur < prev
      # 底を見つけたので記録
      bottom = cur
      if bottom == 0
        # 底抜けてたらたまらないので、topもリセット
        top = 0
      end
    elsif prev < cur
      # p [:fill_water]
      if bottom > 0
        # 底抜けてなかったら水確保

        # 今見つけた高さかそれまでの一番高かったところかいずれか低い方が上限
        upper_bound = top > cur ? cur : top
        i = c - 1
        while upper_bound > aquarium[i]
          waters[i] = upper_bound - aquarium[i] # 上限との差分分水を貯める
          i -= 1
        end
      end
      if top < cur
        top = cur
      end
    end
    # prev == curの場合は無視

    # p [:c, c]
    # p [:bottom, bottom, :top, top]
    # p [:waters, dump_waters(waters)]
    prev = cur
  end

  puts dump_aquarium(aquarium, waters)
  puts "------------------------------------------------------------------------------------------------------------"

  waters.sum
end

def test(q, expected)
  actual = solve(q).to_s
  puts actual
  unless actual == expected
    puts "q: #{q}, expected: #{expected}, but #{actual}"
  end
end

def main
  test( "83141310145169154671122", "24" );
  test( "923111128", "45" );
  test( "923101128", "1" );
  test( "903111128", "9" );
  test( "3", "0" );
  test( "31", "0" );
  test( "412", "1" );
  test( "3124", "3" );
  test( "11111", "0" );
  test( "222111", "0" );
  test( "335544", "0" );
  test( "1223455321", "0" );
  test( "000", "0" );
  test( "000100020003121", "1" );
  test( "1213141516171819181716151413121", "56" );
  test( "712131415161718191817161514131216", "117" );
  test( "712131405161718191817161514031216", "64" );
  test( "03205301204342100", "1" );
  test( "0912830485711120342", "18" );
  test( "1113241120998943327631001", "20" );
  test( "7688167781598943035023813337019904732", "41" );
  test( "2032075902729233234129146823006063388", "79" );
  test( "8323636570846582397534533", "44" );
  test( "2142555257761672319599209190604843", "41" );
  test( "06424633785085474133925235", "51" );
  test( "503144400846933212134", "21" );
  test( "1204706243676306476295999864", "21" );
  test( "050527640248767717738306306596466224", "29" );
  test( "5926294098216193922825", "65" );
  test( "655589141599534035", "29" );
  test( "7411279689677738", "34" );
  test( "268131111165754619136819109839402", "102" );
end

main