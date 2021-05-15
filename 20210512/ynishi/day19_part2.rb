require 'pry-byebug'

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

class Node
  def match?(ctx, chars)
    raise NotImplementation
  end
end

class Ref < Node
  def initialize(key)
    @key = key
  end

  def inspect
    "(Ref #{@key})"
  end

  def match?(ctx, chars)
    ctx[@key].match?(ctx, chars)&.filter_map do |ok, rest|
      if ok
        [ok, rest]
      end
    end
  end
end

class Char < Node
  def initialize(ch)
    @ch = ch
  end

  def match?(ctx, chars)
    c, *rest = chars

    # puts "#{self.inspect} #{chars} #{c == @ch} #{rest}"
    if @ch == c
      [[true, rest]]
    end
  end

  def inspect
    "(Char #{@ch})"
  end
end

class Seq < Node
  def initialize(x, y)
    @x, @y = x, y
  end

  def match?(ctx, chars)
    ret = @x.match?(ctx, chars)&.flat_map do |ok, rest|
      if ok
        @y.match?(ctx, rest)
      end
    end&.compact

    if ret&.empty?
      nil
    else
      ret
    end
  end

  def inspect
    "(Seq #{@x.inspect} #{@y.inspect})"
  end
end

class Branch < Node
  def initialize(x, y)
    @x, @y = x, y
  end

  def match?(ctx, chars)
    x_list = @x.match?(ctx, chars)&.filter_map do |ok, rest|
      [ok, rest] if ok
    end

    y_list = @y.match?(ctx, chars)&.filter_map do |ok, rest|
      [ok, rest] if ok
    end

    x_list ||= []
    y_list ||= []

    x_list + y_list
  end

  def inspect
    "(Branch #{@x.inspect} #{@y.inspect})"
  end
end

class RootRule
  def initialize(rules)
    @rules = rules
  end

  def match?(str)
    @rules[0].match?(@rules, str.chars)&.any? do |_ok, rest|
      rest.empty?
    end
  end
end

def parse_rule(rule_content)
  node, result = parse_branch(rule_content)
  node
end

def parse_branch(rule_content)
  node, rest = parse_seq(rule_content)
  while !rest.empty? && rest[0] == '|'
    rest = rest[1..-1] # | を消費
    n, rest = parse_seq(rest)
    node = Branch.new(node, n)
  end
  [node, rest]
end

def parse_seq(rule_content)
  node, rest = parse_primary(rule_content)
  while !rest.empty? && rest[0] != '|'
    n, rest = parse_primary(rest)
    node = Seq.new(node, n)
  end
  [node, rest]
end

def parse_primary(rule_content)
  c, *rest = rule_content
  case
  when c.start_with?('"')
    [Char.new(c[1]), rest]
  when /^\d+$/ =~ c
    [Ref.new(c.to_i), rest]
  else
    raise "invalid token: #{c}"
  end
end

def parse_rules(rule_str)
  rules = rule_str.lines(chomp: true).inject({}) do |acc, line|
    rule_name, rule_content = line.split(":").map(&:strip)
    acc.merge({
      rule_name.to_i => parse_rule(rule_content.split(' '))
    })
  end

  # part2用に8と11のルール書き換え
  rules[8] = Branch.new(
    Ref.new(42),
    Seq.new(
      Ref.new(42),
      Ref.new(8),
    )
  )

  rules[11] = Branch.new(
    Seq.new(
      Ref.new(42),
      Ref.new(31),
    ),
    Seq.new(
      Seq.new(
        Ref.new(42),
        Ref.new(11),
      ),
      Ref.new(31),
    )
  )

  RootRule.new(rules)
end

def read_rules_and_data
  input_stdin_or('./day19.dat') do |f|
    f.read.split("\n\n")
  end
end

def main
  rule_str, data = read_rules_and_data
  rule = parse_rules(rule_str)

  c = data.lines(chomp: true).count do |line|
    val = rule.match?(line)
    p [:val, line, val]
    val
  end

  p c
end

main
