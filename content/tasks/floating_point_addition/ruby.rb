require "bigdecimal"

# Default Float (IEEE 754 binary64)
puts 0.1 + 0.2              # 0.30000000000000004
puts (0.1 + 0.2 == 0.3)     # false

# Rational literals are exact
puts 1/10r + 2/10r          # 3/10
puts (1/10r + 2/10r == 3/10r)   # true

# BigDecimal for exact decimal arithmetic
a = BigDecimal("0.1")
b = BigDecimal("0.2")
puts (a + b).to_s("F")      # 0.3
puts (a + b == BigDecimal("0.3"))   # true
