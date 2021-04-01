
require 'benchmark'

@inputs = IO.readlines('input.txt')[0].split(',').map(&:to_i)

# key => number, value => 最後のturn
@memory = Hash.new(0)

def main
  # 最後の要素を残してメモ化
  @inputs[0..-2].each.with_index(1) do |value, turn|
    @memory[value] = turn
  end

  start = @inputs.length + 1

  Benchmark.bm 10 do |r|
    r.report '30000000th' do
      (start..30000000).each do |turn|
        last = @inputs.last
        @inputs << (@memory[last].positive? ? (turn - 1) - @memory[last] : 0)
        @memory[last] = turn - 1
      end
    end
  end
  pp @inputs.last
end

main

# M1 macでのレポート
#  user     system      total        real
#  30000000th   5.816685   0.090467   5.907152 (  5.912440)
