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


class Rule
  attr_reader :ingredients, :allergens
  def initialize(ingredients, allergens)
    @ingredients = ingredients
    @allergens = allergens
  end
end

def read_data
  input_stdin_or("./day21.dat") do |f|
    f.read.lines.map do |line|
      ingredients_base, allergens_base = line.chomp.split("(")

      allergens = allergens_base.gsub(")", '').gsub("contains ", '').gsub(",", '').split(/\s+/)
      ingredients = ingredients_base.strip.split(/\s+/)

      Rule.new(ingredients, allergens)
    end
  end
end

def ok?(rule, allergen_to_rules, ingredient_to_allergen, ing, a)
  # すでにどれかの材料に含まれているアレルゲンならNG
  if ingredient_to_allergen.values.include?(a)
    return false
  end

  # このアレルゲンが含まれるruleすべてにこのingがあるかどうかチェック
  allergen_to_rules[a].each do |r|
    return false unless r.ingredients.include?(ing)
  end

  true
end

def search(rules, allergen_to_rules, ingredient_to_allergen, allergens_to_ingredients)
  if allergens_to_ingredients.size == 0
    # 全部のアレルゲンについて調べたらその時点の 材料 => アレルゲンの情報で確定させる
    return ingredient_to_allergen
  else
    (a, ingredients), *rest = allergens_to_ingredients

    #このアレルゲンaを持つ材料の候補ingredientsを順に調べて矛盾がでたらやり直す
    ingredients.each do |ing|
      unless ingredient_to_allergen[ing]
        if ok?(rules, allergen_to_rules, ingredient_to_allergen, ing, a)
          ingredient_to_allergen[ing] = a
          ret = search(rules, allergen_to_rules, ingredient_to_allergen, rest)
          if ret
            return ret
          end
          ingredient_to_allergen[ing] = nil
        end
      end
    end
    false
  end
end

def main
  rules = read_data

  h = Hash.new do |h, k|
    h[k] = []
  end

  # アレルゲンからそのアレルゲンが含まれているルールへのマップ
  allergen_to_rules = rules.inject(h) { |h, r|
    r.allergens.each do |a|
      h[a] << r
    end
    h
  }


  h2 = Hash.new do |h, k|
    h[k] = []
  end

  # アレルゲンから、そのアレルゲンを持つ可能性のある材料候補へのマップ
  allergens_to_ingredients = rules.inject(h2) { |h, r|
    h.tap do |h|
      r.allergens.each do |a|
        r.ingredients.each do |u|
          h[a] << u
          h[a].uniq!
        end
      end
    end
  }.to_a

  # p allergens_to_ingredients

  # 材料からアレルゲンのマップ(これを組み立てていく)
  ingredient_to_allergen = rules.flat_map {|r| r.ingredients}.uniq.inject({}) {|h, u| h.merge(u => nil)}
  # p ingredient_to_allergen


  result = search(rules, allergen_to_rules, ingredient_to_allergen, allergens_to_ingredients)
  p [:result, result]
  p [:result_fixed, result.select {|k, v| v}]

  count_base = rules.flat_map do |r|
    r.ingredients
  end

  targets = result.select {|k, v| !v}.map(&:first)
  # p [:targets, targets]
  # p [:count_base, count_base]
  p count_base.count { |ing| targets.include?(ing) }
end

main