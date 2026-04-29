from decimal import Decimal
from fractions import Fraction

# Default float (IEEE 754 binary64)
print(0.1 + 0.2)              # 0.30000000000000004
print(0.1 + 0.2 == 0.3)       # False

# Exact decimal arithmetic
print(Decimal("0.1") + Decimal("0.2"))                    # 0.3
print(Decimal("0.1") + Decimal("0.2") == Decimal("0.3"))  # True

# Exact rational arithmetic
print(Fraction(1, 10) + Fraction(2, 10))           # 3/10
print(Fraction(1, 10) + Fraction(2, 10) == Fraction(3, 10))  # True
