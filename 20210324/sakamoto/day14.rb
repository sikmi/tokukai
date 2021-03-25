inputs = []
File.readlines(File.dirname(__FILE__) + '/day14.txt').each do |line|
  inputs << line.chomp.gsub(/ /, "").split("=")
end

mask = ""
memories = {}

inputs.each do |target, number|
  if target == "mask"
    mask = number
  else
    binary_number = number.to_i.to_s(2)
    binary_number = ("0" * (mask.length - binary_number.length) ) + binary_number 
    separate_binary_number = binary_number.split ("")
    separate_mask = mask.split("")

    masked_number = []
    separate_mask.length.times do |i|
      if separate_mask[i] == "X"
        masked_number << separate_binary_number[i]
      else
        masked_number << separate_mask[i]
      end
    end
    memories[target] = masked_number.join.to_i(2)
  end
end

p memories.values.sum
