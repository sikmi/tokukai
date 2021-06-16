input = "394618527"
inputs = input.split("").map(&:to_i)

100.times do |i|
  pickup = [inputs[1], inputs[2], inputs[3]]
  inputs.slice!(1,3)

  next_cup = inputs[1]
  destination = inputs[0] - 1
  destination = 9 if destination == 0

  until inputs.include?(destination) do
    destination -= 1

    p inputs.include?(destination)
    destination = 9 if destination == 0

  end
  p "destination is #{destination}"
  destination_index = inputs.index(destination)
  inputs.insert(destination_index+1, pickup).flatten!
  inputs.rotate!(inputs.index(next_cup))
  p "after #{inputs}"
  p i
end


inputs.rotate!(inputs.index(1))
puts inputs.join("").delete_prefix("1")
