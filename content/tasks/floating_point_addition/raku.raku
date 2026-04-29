# Decimal literals in Raku are Rat (rationals) by default, so arithmetic is exact.
say 0.1 + 0.2;             # 0.3
say 0.1 + 0.2 == 0.3;      # True
say (0.1 + 0.2).WHAT;      # (Rat)

# Force IEEE 754 doubles via the Num type to see the rounding error.
say 0.1.Num + 0.2.Num;     # 0.30000000000000004
say 0.1.Num + 0.2.Num == 0.3.Num;   # False
