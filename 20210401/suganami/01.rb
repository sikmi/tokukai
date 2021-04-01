@inputs = IO.readlines('input.txt')[0].split(',').map(&:to_i)

# @memory = Hash.new(0)

# def main
#   @inputs.each.with_index(1) do |value, turn|
#     @memory[value] = turn
#   end

#   start = @inputs.length + 1

#   (start..2020).each do |turn|
#     last = @inputs.last

#     @inputs << @memory[last]  

#     last = @inputs.last
#   end 
# end

# main

def main
  ((@inputs.length + 1)..30000000).each do |turn|
    preview = @inputs.last
    preview_array = @inputs[0..-2]

    unless preview_array.include?(preview)
      @inputs << 0
      next
    end

    pre_turn = turn - 1
    prepre_turn = preview_array.rindex(preview) + 1
  
    @inputs << (pre_turn - prepre_turn)
  end

  pp @inputs
  pp @inputs.last
end

main