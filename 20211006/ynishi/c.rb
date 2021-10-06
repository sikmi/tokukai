def main
 nn = gets.chomp.to_i
 cs = readlines(chomp: true).map(&:to_i)

 patterns = cs.permutation

 # p [:nn, nn]
 # p [:cs, cs]

 result = patterns.flat_map do |list|
   # 全部表(true)で初期化
   state = [true] * list.size
   list.each_with_index do |val, i|
     (i + 1 ... state.size).each do |j|
       if list[j] % val == 0
         state[j] = !state[j]
       end
     end
   end
   # p [list, state]
   state
 end

 # p [:pattern_num, patterns.size]
 p result.select(&:itself).size.to_f / patterns.size
end

main
