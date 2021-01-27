require_relative 'day6_lib'

def main
  groups = input_stdin_or('./day6.dat') do |f|
    load_groups(f)
  end

  p groups.map {|g|
    Group.new(g).yes_questions.count
  }.sum
end

main
