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

# 普通の計算とちがって +-*/の優先度がないので注意
#
# <expr>   ::= term
# <term>   ::= <primary> [ ('+'|'-'|'*'|'/') <primary> ]*
# <primary> ::= <number> | '(' <expr> ')'
class CalcParser < Parslet::Parser
  root(:expr)

  rule(:expr) {
    # 今回は全部同じ優先度なのでtermなくす(カッコの処理で面倒なのでそのままtermを呼ぶ
    # (term.as(:left) >> (expr_op >> term.as(:right)).repeat).as(:expr)
    term
  }

  rule(:term) {
    (primary.as(:left) >> (term_op >> primary.as(:right)).repeat).as(:term)
  }

  rule(:primary) {
    paren_expr | number
  }

  rule(:paren_expr) {
    lparen >> expr >> rparen
  }

  rule(:number) {
    (sign.maybe >> integer >> decimal.maybe).as(:number) >> space?
  }

  # rule(:expr_op) { match('[+-]').as(:op) >> space? }
  # 演算子も全部termにまとめる
  rule(:term_op) { match('[*/+-]').as(:op) >> space? }

  rule(:sign) { match('[-+]') }

  rule(:integer) {
    (match('[1-9]') >> match('[0-9]').repeat) |
    match('[0-9]')
  }

  rule(:decimal) {
    str('.') >> match('[0-9]').repeat(1)
  }

  rule(:lparen) { str('(') >> space? }
  rule(:rparen) { str(')') >> space? }

  rule(:space) { match('\s').repeat(1) }
  rule(:space?) { space.maybe }

end

# 数値用クラス
NumericNode = Struct.new(:value) do
  def eval
    value
  end
end

# 二項演算用クラス
BinOpNode = Struct.new(:left, :op, :right, keyword_init: true) do
  def eval
    l = left.eval
    r = right.eval
    case op
    when '-'
      l - r
    when '+'
      l + r
    when '*'
      l * r
    when '/'
      l / r
    else
      raise "予期しない演算子です. #{op}"
    end
  end
end

class CalcTransform < Parslet::Transform
  # パーサは浮動小数点も大丈夫だが、day18は整数だけなので実際の計算時は整数で
  rule(number: simple(:x)) { NumericNode.new(x.to_i) }
  rule(left: simple(:x)) { x }
  rule(term: simple(:x)) { x }
  # rule(expr: simple(:x)) { x }

  # rule(expr: subtree(:tree)) {
  #   case tree
  #   when Array
  #     tree.inject do |left, right|
  #       BinOpNode.new(
  #         left: left,
  #         op: right[:op].to_s,
  #         right: right[:right],
  #       )
  #     end
  #   else
  #     tree
  #   end
  # }

  rule(term: subtree(:tree)) {
    case tree
    when Array
      tree.inject do |left, right|
        BinOpNode.new(
          left: left,
          op: right[:op].to_s,
          right: right[:right],
        )
      end
    else
      tree
    end
  }
end

def calc(expression)
  parsed = CalcParser.new.parse(expression)
  ast = CalcTransform.new.apply(parsed)
  ast.eval
end

def load_exprs
  input_stdin_or('./day18.dat') do |f|
    f.readlines.map(&:chomp)
  end
end

def main
  exprs = load_exprs
  results = exprs.map do |expr|
    v = calc(expr)
    puts "#{expr} = #{v}"
    v
  end

  p results.sum
end

main