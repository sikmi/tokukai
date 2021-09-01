require 'set'
require 'pry-byebug'

class Node
  attr_reader :id, :parent, :children
  def initialize(id:, parent:)
    @id = id
    @parent = parent
    @children = []
  end

  def next_nodes
    ret = []
    ret << [:up, parent] if parent

    ret + children.compact.map{|e| [:down, e]}
  end

  def inspect
    "(#{id}, (parent #{parent&.id}) (children #{children.map(&:inspect).join(' ')}))"
  end

  class << self
    def nodes
      @nodes ||= {}
    end

    def build(id = 1, parent = nil)
      return if id > 40
      (nodes[id] ||= Node.new(id: id, parent: parent)).tap do |n|
        3.times do |i|
          n.children << build(id * 3 - 1 + i, n)
        end
      end
    end
  end
end

class State
  attr_reader :id, :dir, :prev
  def initialize(id, dir, prev)
    @id = id
    @dir = dir
    @prev = prev
  end

  def route
    if prev
      [dir] + prev.route
    else
      []
    end
  end
end

def solve(start, goal)
  queue = [State.new(start, nil, nil)]
  seen = {
    start => true
  }

  until queue.empty?
    s = queue.shift
    if s.id == goal
      return s
    else
      Node.nodes[s.id].next_nodes.each do |(dir, n)|
        if !seen[n.id]
          seen[n.id] = true
          queue.push(State.new(n.id, dir, s))
        end
      end
    end
  end

  return false
end

def each_sample
  readlines.each do |line|
    yield line.chomp.split("\t")
  end
end

$zoku = {
  [] => 'me',
  [:up] => 'mo',
  [:down] => 'da',
  [:up, :down].sort => 'si',
  [:up, :up, :down].sort => 'au',
  [:up, :down, :down].sort => 'ni',
  [:up, :up, :down, :down].sort => 'co',
}

def main
  Node.build

  each_sample do |no, input, expected|
    start, goal = input.split("->").map(&:to_i)
    s = solve(start, goal)
    actual = $zoku[s.route] || '-'
    puts "#{sprintf("%-7s", input)} expected: #{sprintf("%2s", expected)}, actual: #{sprintf("%2s", actual)}, #{expected == actual ? 'ok' : 'ng'}"
  end
end

main
