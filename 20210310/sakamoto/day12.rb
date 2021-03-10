inputs = []

File.readlines(File.dirname(__FILE__) + '/day12.txt').each do |line|
  line.chomp!
  inputs << [line[0].to_sym,line[1..-1].to_i]
end

shipmove = {N: 0, S: 0, W: 0, E:0, shiphead: :E}

directionary = [:N, :E , :S , :W, :N, :E, :S, :W]

inputs.each do |direction,move|
  if direction == :R
    moving = (move / 90) % 4
    shipmove[:shiphead] =  directionary[directionary.index(shipmove[:shiphead]) + moving]
  elsif direction == :L
    moving = 4 - ((move / 90) % 4)
    shipmove[:shiphead] =  directionary[directionary.index(shipmove[:shiphead]) + moving]
  elsif direction == :F
    shipmove[shipmove[:shiphead]] += move
  else
    shipmove[direction] += move
  end
end

p (shipmove[:N] - shipmove[:S]).abs + (shipmove[:E] - shipmove[:W]).abs

