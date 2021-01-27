def pipe?(stream = $stdin)
  File.pipe?(stream)
end

def input_stdin_or(file)
  if pipe?
    yield $stdin
  else
    open(file) do |f|
      yield f
    end
  end
end

def load_groups(f)
  f.read.split("\n\n").map do |block|
    block
  end
end

class Group
  def initialize(src)
    @result = src.split("\n").map(&:chars).map(&:to_a)
  end

  def yes_questions
    @result.flatten.uniq
  end

  def all_yes_counts
    yes_counts.count {|k, v| v == @result.size}
  end

  def yes_counts
    @result.flatten.tally
  end
end

