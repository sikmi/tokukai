def main
  cards = (1..6).to_a
  n = gets.chomp.to_i

  (0 ... (n % 30)).each do |i|
    from = i % 5 + 1 - 1
    to = i % 5 + 2 - 1

    tmp = cards[from]
    cards[from] = cards[to]
    cards[to] = tmp
    # p [i, from, to, cards.join]
  end

  puts cards.join
end

main