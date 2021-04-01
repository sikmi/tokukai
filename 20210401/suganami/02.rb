@inputs = IO.readlines('input.txt')[0].split(',').map(&:to_i)

# key => number, value => 最後のturn
@memory = Hash.new(0)

def main
  @inputs[0..-2].each.with_index(1) do |value, turn|
    @memory[value] = turn
  end

  start = @inputs.length + 1

  (start..30000000).each do |turn|
    last = @inputs.last    
    @inputs << (@memory[last].positive? ? (turn - 1) - @memory[last] : 0)
    @memory[last] = turn - 1
  end
  pp @inputs.last
end

main
