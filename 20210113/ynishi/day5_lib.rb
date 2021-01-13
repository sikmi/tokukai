def pipe?(stream = $stdin)
  File.pipe?(stream)
end

def input_stdin_or(file)
  if pipe?
    yield $stdin
  else
    open(file) do |f|
      yield f end
  end
end

def load_addresses(f)
  f.readlines.map do |line|
    line.chomp.chars.map(&:to_sym)
  end
end

def to_row(parts)
  bin_search(parts, 0, 127)
end

def to_col(parts)
  bin_search(parts, 0, 7)
end

def bin_search(parts, from, to)
  # p [:from_to, from, to]
  parts.each do |dir|
    case dir
    when :low
      to  -= (to - from) / 2 + 1
    when :high
      from += (to - from) / 2 + 1
    else
      raise "invalid pattern: #{dir}"
    end
    # p [:from_to_dir, dir, from, to]
  end
  from
end

def normalize_row_parts(parts)
  parts.map do |c|
    case c
    when :F
      :low
    when :B
      :high
    else
      raise "invalid pattern: #{c}"
    end
  end
end

def normalize_col_parts(parts)
  parts.map do |c|
    case c
    when :L
      :low
    when :R
      :high
    else
      raise "invalid pattern: #{c}"
    end
  end
end

def calc_positions(addresses)
  # p [:addresses, addresses]
  addresses.map do |addr|
    row_parts = normalize_row_parts(addr[0 ... 7])
    col_parts = normalize_col_parts(addr[7 .. -1])

    # p [:addr, addr]
    # p [:row, row_parts]
    # p [:col, col_parts]
    # p [:r_c, to_row(row_parts), to_col(col_parts)]
    [to_row(row_parts), to_col(col_parts)]
  end
end

