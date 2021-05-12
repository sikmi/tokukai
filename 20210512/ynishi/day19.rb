require 'parslet'
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

def seq_rule(seq_str)
  seq_str.strip.split(' ').map { |r|
    "r#{r}"
  }.join(" >> ")
end

def indent_rule(rule_code)
  rule_code.lines.map { "  " + _1 }.join
end

def parslet_parser_generator(rule_str)
  rules = rule_str.lines.map(&:chomp).map do |line|
    rule, rule_body = line.split(":").map(&:strip)
    if rule_body.start_with?('"')
      ch = rule_body.gsub('"', '')
      <<~EOS
        rule(:r#{rule}) {
          str('#{ch}')
        }
      EOS
    else
      if rule_body.include?('|')
        left_str, right_str = rule_body.split('|')
        <<~EOS
          rule(:r#{rule}) {
            (#{seq_rule(left_str)}) | (#{seq_rule(right_str)})
          }
        EOS
      else
        <<~EOS
          rule(:r#{rule}) {
            #{seq_rule(rule_body)}
          }
        EOS
      end
    end
  end

  <<~EOS
    class MyParser < Parslet::Parser
      root(:r0)

    #{rules.map {|code| indent_rule(code) }.join}
    end
  EOS
end

def read_rules_and_data
  input_stdin_or('./day19.dat') do |f|
    f.read.split("\n\n")
  end
end

def parse_ok?(str)
  MyParser.new.parse(str)
rescue Parslet::ParseFailed
  nil
end

def main
  rule_str, data = read_rules_and_data
  parser_class_ruby_code = parslet_parser_generator(rule_str)
  puts parser_class_ruby_code
  eval(parser_class_ruby_code)
  p data.lines.count { parse_ok?(_1.chomp) }
end

main
