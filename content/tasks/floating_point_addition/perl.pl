use strict;
use warnings;

my $sum = 0.1 + 0.2;
printf "%.17f\n", $sum;                # 0.30000000000000004
print $sum == 0.3 ? "equal\n" : "not equal\n";

# Exact arithmetic via Math::BigRat
use Math::BigRat;
my $r = Math::BigRat->new("1/10") + Math::BigRat->new("2/10");
print "$r\n";                          # 3/10
