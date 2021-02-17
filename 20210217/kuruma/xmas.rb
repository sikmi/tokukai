class Xmas

  def initialize(seed, preamble_length = 5, start_number = 0)
    @seed = seed
    # 枠長
    @preamble_length = preamble_length
    # サーチの開始位置
    @start_number = start_number
    # 今回の検査対象数値
    @target_number = @seed[@preamble_length + @start_number]
    # 検査対象の配列
    @target_seed = @seed[@start_number..( @start_number + @preamble_length - 1)]
  end

  # 全体チェック
  def all_check
    if valid?
      return nil
    elsif one_check
      puts "#{@target_number}: OK"
      return Xmas.new(@seed, @preamble_length, @start_number + 1).all_check
    else
      return @target_number
    end
  end

  private

    # preamble指定がseedと同じ時は検証済としてvaildに
    def valid?
      @seed.length <= @start_number
    end

    # 最初の1つをチェックする
    def one_check
      return true if valid?
      # 長さ+1をチェックするのでターゲットを決定
      # a + b = target_number になるかの確認
      @target_seed.each do |a|
        @target_seed.each do |b|
          if a != b && a + b == @target_number
            puts "a: #{a}, b: #{b}, a + b: #{ a + b }, target_number: #{@target_number} "
            return true
          end
        end
      end
      return false
    end

end

# 実行系
seed = %w(35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576).map(&:to_i)
preamble_length = 5

puts "seed: #{seed.inspect}"
puts "preamble_length #{preamble_length}"
puts "#===========> #{Xmas.new(seed, preamble_length).all_check}"
