require 'pry-byebug'
require 'set'

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

def split_blank(ary)
  [].tap do |result|
    buf = []
    ary.each do |e|
      if e == ""
        result << buf
        buf = []
      else
        buf << e
      end
    end
    result << buf
  end
end

def read_data
  input_stdin_or('./day16.dat') do |f|
    split_blank(f.readlines.map(&:chomp))
  end
end

class Checker
  attr_reader :name, :checkers
  def initialize(name, checkers)
    @name = name
    @checkers = checkers
  end

  def check(x)
    checkers.any? do |c|
      c.call(x)
    end
  end
end

def parse_valid_numbers(block)
  block.inject([]) do |checkers, line|
    checkers.tap do
      raise "invalid format line: #{line}" unless /^([^:]+): (\d+-\d+) or (\d+-\d+)/ =~ line
      name = Regexp.last_match(1)
      ranges = [
        Regexp.last_match(2),
        Regexp.last_match(3),
      ]

      chs = ranges.map do |range|
        raise "innvalid format range: #{range}" unless /(\d+)-(\d+)/ =~ range
        s = Regexp.last_match(1).to_i
        e = Regexp.last_match(2).to_i
        ->(x) { s <= x && x <= e }
      end

      checkers << Checker.new(name, chs)
    end
  end
end

def parse_numbers(block)
  # 1行目は nearby tickets: の行なので捨てる
  block.drop(1).map do |line|
    line.split(/,/).map(&:to_i)
  end
end

def extract_valid_tickets(tickets, checkers)
  tickets.find_all do |ticket|
    ticket.all? do |n|
      checkers.any? do |checker|
        checker.check(n)
      end
    end
  end
end

def parse_my_ticket(ticket)
  ticket[1].split(/,/).map(&:to_i)
end

def remove_checker(candidate, name)
  candidate.find_all { |c| c[0].name != name }
end

def determine_unique_name(candidates)
  done = Array.new(candidates.size, false)

  while !done.all?
    # 候補が一意に決まるもののうち、未処理のものを確定していく
    list = candidates.each_with_index.find_all {|e, i| e.size == 1}
    list.each do |candidate, i|
      unless done[i]
        candidates = candidates.map do |c|
          if c.size == 1
            c
          else
            remove_checker(c, candidate.first.first.name)
          end
        end
        done[i] = true
      end
    end
    # p [:done, done]
  end

  candidates
end

def main
  valid_num_block, ticket_block, nearby_block = read_data
  valid_number_checkers = parse_valid_numbers(valid_num_block)
  numbers = parse_numbers(nearby_block)
  valid_tickets = extract_valid_tickets(numbers, valid_number_checkers)
  transposed = valid_tickets.transpose

  checker_name_candidates = transposed.map do |nums|
    valid_number_checkers.each_with_index.find_all {|ch, i|
      nums.all? do |n|
        ch.check(n)
      end
    }
  end

  # 唯一に決まるものベースで列の種類を特定していく
  checker_mappings = determine_unique_name(checker_name_candidates).flatten(1)

  my_ticket = parse_my_ticket(ticket_block)

  result = 1
  checker_mappings.each_with_index do |(checker, _), i|
    if /^departure/ =~ checker.name
      result *= my_ticket[i]
    end
  end

  p result
end

main