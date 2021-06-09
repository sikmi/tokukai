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

class Deck
  attr_reader :name, :cards
  def initialize(name:, cards:)
    @name = name
    @cards = cards
  end

  def next_card
    @cards.shift
  end

  def add_cards(cards)
    @cards += cards
  end

  def has_cards?
    !@cards.empty?
  end

  def calc_points
    cards.reverse.each.with_index(1).inject(0) do |acc, (val, index)|
      acc += val * index
    end
  end

  def recursive_battle?(val)
    cards.size >= val
  end

  def copy_deck(val)
    Deck.new(
      name: name,
      cards: cards.take(val).dup
    )
  end
end

class Combat
  def initialize(decks)
    @decks = decks
  end

  def do_round
    p self
    while has_next_round?
      next_round
    end

    winner
  end

  def check_winner(cards)
    if recursive_battle?(cards)
      winner = Combat.new(@decks.each_with_index.map { |d, i| d.copy_deck(cards.to_a[i][0])}).do_round
      _, winner_index = @decks.each_with_index.find {|d, _i| d.name == winner.name}
      cards.find {|_val, index| index == winner_index}
    else
      cards.max_by {|val, _index| val }
    end
  end

  def recursive_battle?(cards)
    @decks.zip(cards).all? do |d, (val, _index)|
      d.recursive_battle?(val)
    end

  end

  def next_round
    cards = @decks.map(&:next_card).each_with_index

    winner_val, winner_index = check_winner(cards)

    # 二人なので、とりあえず自分が先頭になるようにソートする
    additional_cards = cards.sort_by {|_val, index| index == winner_index ? 0 : 1}.map {|val, _index| val}

    @decks[winner_index].add_cards(additional_cards)
  end

  def has_next_round?
    @decks.all?(&:has_cards?)
  end

  def winner
    @decks.find {|d| d.has_cards?}
  end
end

def read_data
  input_stdin_or("./day22_sample.dat") do |f|
    decks = f.read.split(/\n\n/).map do |deck_base|
      name, *cards = deck_base.lines(chomp: true)
      Deck.new(
        name: name,
        cards: cards.map(&:to_i)
      )
    end

    Combat.new(decks)
  end
end

def main
  combat = read_data

  combat.do_round

  p combat
  p combat.winner.calc_points
end

main
