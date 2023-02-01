def check(string)
  reg = /^[A-Z][1-9]\d{5}[A-Z]$/.freeze

  if string.match?(reg)
    'Yes'
  else
    'No'
  end
end

def main
  check(gets)
end

puts main
