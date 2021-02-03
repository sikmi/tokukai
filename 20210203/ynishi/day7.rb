require 'pry-byebug'

def pipe?(stream = $stdin)
  File.pipe?(stream)
end

def input_stdin_or(file)
  if pipe?
    yield $stdin
  else
    open(file) do |f|
      yield f
    end
  end
end

# bag_a => [[1, bag_b], [2, bag_c]]
# bag_b => [[2, bag_d]]
# bag_c => [[2, bag_e]]
def load_bag_info
  input_stdin_or("./day7.dat") do |f|
    f.readlines.map(&:chomp).map do |line|
      parent, children_statement = line.split(' bags contain ')
      children = case
                 when children_statement == 'no other bags.'
                   []
                 else
                   children_statement.split(', ').map do |bag_info|
                     bag_info.gsub(/ bag(s|\.)?/, '').split(' ').then do |count_str, *color_strs|
                       [count_str.to_i, color_strs.join(' ').gsub('.', '')]
                     end
                   end
                 end
      [parent, children]
    end
  end
end

def count_parents(bag, relations)
  relations[bag] + relations[bag].flat_map do |b|
    count_parents(b, relations)
  end
end

def main
  child_to_parents = Hash.new do |h, k|
    h[k] = []
  end
  load_bag_info.each do |parent, children|
    # p [parent, children]
    children.each do |_, child|
      child_to_parents[child] << parent
    end
  end

  # p count_parents('shiny gold', child_to_parents)
  require 'pp'
  # pp child_to_parents
  # p count_parents('shiny gold', child_to_parents)
  # p count_parents('shiny gold', child_to_parents).size
  p count_parents('shiny gold', child_to_parents).uniq.size
end

main
