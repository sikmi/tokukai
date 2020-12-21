class SolveDay3
  def initialize
  end

  def main
    File.open(File.dirname(__FILE__) + '/day3.txt') do |f|
      count = 0
      lines = f.readlines
      width = lines[0].length - 1
      x = 0
      lines.each do |line|
        count += 1 if move_to_tree_cell?(x, line, width)
        x += 3
      end
      puts count
    end
  end

  def move_to_tree_cell?(x, line, width)
    line[x % width] == "#"
  end  
end

SolveDay3.new.main

