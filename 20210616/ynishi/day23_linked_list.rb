require 'pry-byebug'
require 'set'

class Node
  attr_reader :value
  attr_accessor :next_node

  def initialize(value: nil, next_node: nil)
    @value = value
    @next_node = next_node
  end
end

class LinkedList
  attr_reader :head
  def initialize
    @head = Node.new
    @tail = @head
    @tail.next_node = @head
  end

  def add(val)
    Node.new(value: val).tap do |n|
      @tail.next_node = n
      @tail = n
      @tail.next_node = @head.next_node
    end
  end

  def each_one_cycle
    each do |cur|
      yield cur
      return if cur == @tail
    end
  end

  def each
    cur = @head
    while cur.next_node
      yield cur.next_node
      cur = cur.next_node
    end
  end
end

class Game
  def initialize(nums)
    list = LinkedList.new
    nums.each do |n|
      list.add(n)
    end
    @max_value = nums.max;
    @cur = list.head.next_node
  end

  def run
    100.times do |i|
      p [:move, i + 1, each_one_cycle(@cur).map {|c| c.value}]
      move
    end

    each_one_cycle(@cur).map {|e| e.value}
  end

  def move
    cur = @cur
    picked = pick(cur, 3)
    picked_nums = partial_nums(picked)
    p [:picked_nums, picked_nums]
    dest = search_dest(cur.next_node, cur.value - 1, picked_nums)
    # p [:dest, dest]
    add_picked_after_dest(dest, picked)

    # これでリストの繋ぎ変えは終わってるので、単純に次のノードが注目ノードになる
    @cur = @cur.next_node
  end

  def add_picked_after_dest(dest, picked)
    after = dest.next_node
    dest.next_node = picked

    # pickedの最後にafterをつなげる
    c = picked
    while c.next_node
      c = c.next_node
    end
    # この時点でcがpickedの最終要素なので、その次にafterをつなげる
    c.next_node = after
  end

  def pick(node, i)
    partial_top = node.next_node

    c = partial_top
    (i - 1).times do
      c = c.next_node
    end

    # ここでpick puするノードを飛ばして今の注文ノードからpick upした後のノードにつなぎ直す
    node.next_node = c.next_node

    # で一旦pick upしたi個の要素を孤立させる(これはいらんかも)
    # これで、partial_topからnext_nodeがnilになるまでのリストがpick upしたリスト
    c.next_node = nil

    partial_top
  end

  def search_dest(node, val, picked_nums)
    # p [:search_dest]
    last = node
    loop do
      # p [:search, val]
      c = node
      loop do
        # p [:search_checkx, c.value, c.value == val && !picked_nums.include?(val)]
        return c if c.value == val && !picked_nums.include?(val)
        break if c.next_node == last
        c = c.next_node
      end
      val -= 1
      val = val <= 0 ? @max_value : val
    end
  end

  def partial_nums(partial_top)
    Set.new.tap do |s|
      each_elem(partial_top) do |n|
        s << n.value
      end
    end
  end

  def each_elem(top)
    c = top
    loop do
      return unless c
      yield c
      c = c.next_node
    end
  end

  def each_one_cycle(top)
    return enum_for(:each_one_cycle, top) unless block_given?

    yield(top)
    c = top.next_node
    while c != top
      yield c
      c = c.next_node
    end
  end
end

def pipe?(stream = $stdin)
  File.pipe?(stream)
end

def input_stdin_or(file)
  if pipe?
    yield $stdin
  else
    open(file) do |f| yield f end
  end
end

def read_data
  input_stdin_or('day23.dat') do |f|
    f.read.chomp.chars.map(&:to_i)
  end
end

def main
  g = Game.new(read_data)
  last_state = g.run

  _, index1 = last_state.each_with_index.find{|e, i| e == 1}
  puts last_state.rotate(index1).drop(1).join
end

main
