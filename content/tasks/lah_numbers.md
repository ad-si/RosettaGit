+++
title = "Lah numbers"
description = ""
date = 2019-10-10T00:30:07Z
aliases = []
[extra]
id = 22467
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "d",
  "factor",
  "go",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "rexx",
  "sidef",
  "zkl",
]
+++

Lah numbers, sometimes referred to as Stirling numbers of the third kind, are coefficients of polynomial expansions expressing rising factorials in terms of falling factorials.

Unsigned Lah numbers count the number of ways a set of '''n''' elements can be partitioned into '''k''' non-empty linearly ordered subsets.

Lah numbers are closely related to Stirling numbers of the first & second kinds, and may be derived from them.

Lah numbers obey the identities and relations:

    L(n, 0), L(0, k) = 0 # for n, k > 0
    L(n, n) = 1
    L(n, 1) = n!
    L(n, k) =           ( n! * (n - 1)! ) / ( k! * (k - 1)! ) / (n - k)!      # For unsigned Lah numbers
      ''or''
    L(n, k) = (-1)**n * ( n! * (n - 1)! ) / ( k! * (k - 1)! ) / (n - k)!      # For signed Lah numbers


## Task

:* Write a routine (function, procedure, whatever) to find '''unsigned Lah numbers'''. There are several methods to generate unsigned Lah numbers. You are free to choose the most appropriate for your language. If your language has a built-in, or easily, publicly available library implementation, it is acceptable to use that.

:* Using the routine, generate and show here, on this page, a table (or triangle) showing the unsigned Lah numbers, '''L(n, k)''', up to '''L(12, 12)'''. it is optional to show the row / column for n == 0 and k == 0. It is optional to show places where L(n, k) == 0 (when k > n).

:* If your language supports large integers, find and show here, on this page, the maximum value of '''L(n, k)''' where '''n == 100'''.


## See also

:* '''[[wp:Lah_number|Wikipedia - Lah number]]'''
:* '''[[oeis:A105278|OEIS:A105278 - Unsigned Lah numbers]]'''
:* '''[[oeis:A008297|OEIS:A008297 - Signed Lah numbers]]'''


;Related Tasks:

:* '''[[Stirling_numbers_of_the_first_kind|Stirling numbers of the first kind]]'''
:* '''[[Stirling_numbers_of_the_second_kind|Stirling numbers of the second kind]]'''
:* '''[[Bell_numbers|Bell numbers]]'''






## D

```d
import std.algorithm : map;
import std.bigint;
import std.range;
import std.stdio;

BigInt factorial(BigInt n) {
    if (n == 0) return BigInt(1);
    BigInt res = 1;
    while (n > 0) {
        res *= n--;
    }
    return res;
}

BigInt lah(BigInt n, BigInt k) {
    if (k == 1) return factorial(n);
    if (k == n) return BigInt(1);
    if (k > n) return BigInt(0);
    if (k < 1 || n < 1) return BigInt(0);
    return (factorial(n) * factorial(n - 1)) / (factorial(k) * factorial(k - 1)) / factorial(n - k);
}

auto max(R)(R r) if (isInputRange!R) {
    alias T = ElementType!R;
    T v = T.init;

    while (!r.empty) {
        if (v < r.front) {
            v = r.front;
        }
        r.popFront;
    }

    return v;
}

void main() {
    writeln("Unsigned Lah numbers: L(n, k):");
    write("n/k ");
    foreach (i; 0..13) {
        writef("%10d ", i);
    }
    writeln();
    foreach (row; 0..13) {
        writef("%-3d", row);
        foreach (i; 0..row+1) {
            auto l = lah(BigInt(row), BigInt(i));
            writef("%11d", l);
        }
        writeln();
    }
    writeln("\nMaximum value from the L(100, *) row:");
    auto lambda = (int a) => lah(BigInt(100), BigInt(a));
    writeln(iota(0, 100).map!lambda.max);
}
```

```txt
Unsigned Lah numbers: L(n, k):
n/k          0          1          2          3          4          5          6          7          8          9         10         11         12
0            1
1            0          1
2            0          2          1
3            0          6          6          1
4            0         24         36         12          1
5            0        120        240        120         20          1
6            0        720       1800       1200        300         30          1
7            0       5040      15120      12600       4200        630         42          1
8            0      40320     141120     141120      58800      11760       1176         56          1
9            0     362880    1451520    1693440     846720     211680      28224       2016         72          1
10           0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1
11           0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1
12           0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132          1

Maximum value from the L(100, *) row:
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000
```



## Factor

```factor
USING: combinators combinators.short-circuit formatting infix io
kernel locals math math.factorials math.ranges prettyprint
sequences ;
IN: rosetta-code.lah-numbers

! Yes, Factor can do infix arithmetic with local variables!
! This is a good use case for it.

INFIX:: (lah) ( n k -- m )
    ( factorial(n) * factorial(n-1) ) /
    ( factorial(k) * factorial(k-1) ) / factorial(n-k) ;

:: lah ( n k -- m )
    {
        { [ k 1 = ] [ n factorial ] }
        { [ k n = ] [ 1 ] }
        { [ k n > ] [ 0 ] }
        { [ k 1 < n 1 < or ] [ 0 ] }
        [ n k (lah) ]
    } cond ;

"Unsigned Lah numbers: n k lah:" print
"n\\k" write 13 dup [ "%11d" printf ] each-integer nl

<iota> [
    dup dup "%-2d " printf [0,b] [
        lah "%11d" printf
    ] with each nl
] each nl

"Maximum value from the 100 _ lah row:" print
100 [0,b] [ 100 swap lah ] map supremum .
```

```txt

Unsigned Lah numbers: n k lah:
n\k          0          1          2          3          4          5          6          7          8          9         10         11         12
0            1
1            0          1
2            0          2          1
3            0          6          6          1
4            0         24         36         12          1
5            0        120        240        120         20          1
6            0        720       1800       1200        300         30          1
7            0       5040      15120      12600       4200        630         42          1
8            0      40320     141120     141120      58800      11760       1176         56          1
9            0     362880    1451520    1693440     846720     211680      28224       2016         72          1
10           0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1
11           0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1
12           0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132          1

Maximum value from the 100 _ lah row:
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Lah_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    limit := 100
    last := 12
    unsigned := true
    l := make([][]*big.Int, limit+1)
    for n := 0; n <= limit; n++ {
        l[n] = make([]*big.Int, limit+1)
        for k := 0; k <= limit; k++ {
            l[n][k] = new(big.Int)
        }
        l[n][n].SetInt64(int64(1))
        if n != 1 {
            l[n][1].MulRange(int64(2), int64(n))
        }
    }
    var t big.Int
    for n := 1; n <= limit; n++ {
        for k := 1; k <= n; k++ {
            t.Mul(l[n][1], l[n-1][1])
            t.Quo(&t, l[k][1])
            t.Quo(&t, l[k-1][1])
            t.Quo(&t, l[n-k][1])
            l[n][k].Set(&t)
            if !unsigned && (n%2 == 1) {
                l[n][k].Neg(l[n][k])
            }
        }
    }
    fmt.Println("Unsigned Lah numbers: l(n, k):")
    fmt.Printf("n/k")
    for i := 0; i <= last; i++ {
        fmt.Printf("%10d ", i)
    }
    fmt.Printf("\n--")
    for i := 0; i <= last; i++ {
        fmt.Printf("-----------")
    }
    fmt.Println()
    for n := 0; n <= last; n++ {
        fmt.Printf("%2d ", n)
        for k := 0; k <= n; k++ {
            fmt.Printf("%10d ", l[n][k])
        }
        fmt.Println()
    }
    fmt.Println("\nMaximum value from the l(100, *) row:")
    max := new(big.Int).Set(l[limit][0])
    for k := 1; k <= limit; k++ {
        if l[limit][k].Cmp(max) > 0 {
            max.Set(l[limit][k])
        }
    }
    fmt.Println(max)
    fmt.Printf("which has %d digits.\n", len(max.String()))
}
```


```txt

Unsigned Lah numbers: l(n, k):
n/k         0          1          2          3          4          5          6          7          8          9         10         11         12 
-------------------------------------------------------------------------------------------------------------------------------------------------
 0          1 
 1          0          1 
 2          0          2          1 
 3          0          6          6          1 
 4          0         24         36         12          1 
 5          0        120        240        120         20          1 
 6          0        720       1800       1200        300         30          1 
 7          0       5040      15120      12600       4200        630         42          1 
 8          0      40320     141120     141120      58800      11760       1176         56          1 
 9          0     362880    1451520    1693440     846720     211680      28224       2016         72          1 
10          0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1 
11          0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1 
12          0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132          1 

Maximum value from the l(100, *) row:
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000
which has 164 digits.

```



## Julia


```julia
using Combinatorics

function lah(n::Integer, k::Integer, signed=false)
    if n == 0 || k == 0 || k > n
        return zero(n)
    elseif n == k
        return one(n)
    elseif k == 1
        return factorial(n)
    else
        unsignedvalue = binomial(n, k) * binomial(n - 1, k - 1) * factorial(n - k)
        if signed && isodd(n)
            return -unsignedvalue
        else
            return unsignedvalue
        end
    end
end

function printlahtable(kmax)
    println("  ", mapreduce(i -> lpad(i, 12), *, 0:kmax))

    sstring(n, k) = begin i = lah(n, k); lpad(k > n && i == 0 ? "" : i, 12) end

    for n in 0:kmax
        println(rpad(n, 2) * mapreduce(k -> sstring(n, k), *, 0:kmax))
    end
end

printlahtable(12)

println("\nThe maxiumum of lah(100, _) is: ", maximum(k -> lah(BigInt(100), BigInt(k)), 1:100))

```
```txt

             0           1           2           3           4           5           6           7           8           9          10          11          12
0            0
1            0           1
2            0           2           1
3            0           6           6           1
4            0          24          36          12           1
5            0         120         240         120          20           1
6            0         720        1800        1200         300          30           1
7            0        5040       15120       12600        4200         630          42           1
8            0       40320      141120      141120       58800       11760        1176          56           1
9            0      362880     1451520     1693440      846720      211680       28224        2016          72           1
10           0     3628800    16329600    21772800    12700800     3810240      635040       60480        3240          90           1
11           0    39916800   199584000   299376000   199584000    69854400    13970880     1663200      118800        4950         110           1
12           0   479001600  2634508800  4390848000  3293136000  1317254400   307359360    43908480     3920400      217800        7260         132           1

The maxiumum of lah(100, _) is: 44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000

```



## Kotlin

```scala
import java.math.BigInteger

fun factorial(n: BigInteger): BigInteger {
    if (n == BigInteger.ZERO) return BigInteger.ONE
    if (n == BigInteger.ONE) return BigInteger.ONE
    var prod = BigInteger.ONE
    var num = n
    while (num > BigInteger.ONE) {
        prod *= num
        num--
    }
    return prod
}

fun lah(n: BigInteger, k: BigInteger): BigInteger {
    if (k == BigInteger.ONE) return factorial(n)
    if (k == n) return BigInteger.ONE
    if (k > n) return BigInteger.ZERO
    if (k < BigInteger.ONE || n < BigInteger.ONE) return BigInteger.ZERO
    return (factorial(n) * factorial(n - BigInteger.ONE)) / (factorial(k) * factorial(k - BigInteger.ONE)) / factorial(n - k)
}

fun main() {
    println("Unsigned Lah numbers: L(n, k):")
    print("n/k ")
    for (i in 0..12) {
        print("%10d ".format(i))
    }
    println()
    for (row in 0..12) {
        print("%-3d".format(row))
        for (i in 0..row) {
            val l = lah(BigInteger.valueOf(row.toLong()), BigInteger.valueOf(i.toLong()))
            print("%11d".format(l))
        }
        println()
    }
    println("\nMaximum value from the L(100, *) row:")
    println((0..100).map { lah(BigInteger.valueOf(100.toLong()), BigInteger.valueOf(it.toLong())) }.max())
}
```

```txt
Unsigned Lah numbers: L(n, k):
n/k          0          1          2          3          4          5          6          7          8          9         10         11         12 
0            1
1            0          1
2            0          2          1
3            0          6          6          1
4            0         24         36         12          1
5            0        120        240        120         20          1
6            0        720       1800       1200        300         30          1
7            0       5040      15120      12600       4200        630         42          1
8            0      40320     141120     141120      58800      11760       1176         56          1
9            0     362880    1451520    1693440     846720     211680      28224       2016         72          1
10           0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1
11           0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1
12           0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132          1

Maximum value from the L(100, *) row:
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000
```



## Perl

```perl
use strict;
use warnings;
use feature 'say';
use ntheory qw(factorial);
use List::Util qw(max);

sub Lah {
    my($n, $k) = @_;
    return factorial($n) if $k == 1;
    return 1 if $k == $n;
    return 0 if $k > $n;
    return 0 if $k < 1 or $n < 1;
    (factorial($n) * factorial($n - 1)) / (factorial($k) * factorial($k - 1)) / factorial($n - $k)
}

my $upto = 12;
my $mx   = 1 + length max map { Lah(12,$_) } 0..$upto;

say 'Unsigned Lah numbers:  L(n, k):';
print 'n\k' . sprintf "%${mx}s"x(1+$upto)."\n", 0..1+$upto;

for my $row (0..$upto) {
    printf '%-3d', $row;
    map { printf "%${mx}d", Lah($row, $_) } 0..$row;
    print "\n";
}

say "\nMaximum value from the L(100, *) row:";
say max map { Lah(100,$_) } 0..100;
```

```txt
Unsigned Lah numbers:  L(n, k):
n\k          0          1          2          3          4          5          6          7          8          9         10         11
0            1
1            0          1
2            0          2          1
3            0          6          6          1
4            0         24         36         12          1
5            0        120        240        120         20          1
6            0        720       1800       1200        300         30          1
7            0       5040      15120      12600       4200        630         42          1
8            0      40320     141120     141120      58800      11760       1176         56          1
9            0     362880    1451520    1693440     846720     211680      28224       2016         72          1
10           0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1
11           0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1
12           0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132

Maximum value from the L(100, *) row:
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000
```



## Perl 6

```perl6
constant @factorial = 1, |[\*] 1..*;

sub Lah (Int \n, Int \k) {
    return @factorial[n] if k == 1;
    return 1 if k == n;
    return 0 if k > n;
    return 0 if k < 1 or n < 1;
    (@factorial[n] * @factorial[n - 1]) / (@factorial[k] * @factorial[k - 1]) / @factorial[n - k]
}

my $upto = 12;

my $mx = (1..$upto).map( { Lah($upto, $_) } ).max.chars;

put 'Unsigned Lah numbers:  L(n, k):';
put 'n\k', (0..$upto)».fmt: "%{$mx}d";

for 0..$upto -> $row {
    $row.fmt('%-3d').print;
    put (0..$row).map( { Lah($row, $_) } )».fmt: "%{$mx}d";
}

say "\nMaximum value from the L(100, *) row:";
say (^100).map( { Lah 100, $_ } ).max;
```

```txt
Unsigned Lah numbers:  L(n, k):
n\k         0          1          2          3          4          5          6          7          8          9         10         11         12
0           1
1           0          1
2           0          2          1
3           0          6          6          1
4           0         24         36         12          1
5           0        120        240        120         20          1
6           0        720       1800       1200        300         30          1
7           0       5040      15120      12600       4200        630         42          1
8           0      40320     141120     141120      58800      11760       1176         56          1
9           0     362880    1451520    1693440     846720     211680      28224       2016         72          1
10          0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1
11          0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1
12          0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132          1

Maximum value from the L(100, *) row:
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000
```



## Phix

```Phix
include mpfr.e
 
constant lim = 100,
         lim1 = lim+1,
         last = 12
sequence l = repeat(0,lim1)
for n=1 to lim1 do
    l[n] = mpz_inits(lim1)
    mpz_set_si(l[n][n],1)
    if n!=2 then
        mpz_fac_ui(l[n][2],n-1)
    end if
end for
mpz {t, m100} = mpz_inits(2)
for n=1 to lim do
    for k=1 to n do
        mpz_mul(t,l[n+1][2],l[n][2])
        mpz_fdiv_q(t, t, l[k+1][2])
        mpz_fdiv_q(t, t, l[k][2])
        mpz_fdiv_q(l[n+1][k+1], t, l[n-k+1][2])
    end for
end for
printf(1,"Unsigned Lah numbers: l(n, k):\n n   k:")
for i=0 to last do
    printf(1,"%6d     ", i)
end for
printf(1,"\n---    %s\n",repeat('-',last*11+6))
for n=0 to last do
    printf(1,"%2d ", n)
    for k=1 to n+1 do
        mpfr_printf(1,"%10Zd ", l[n+1][k])
    end for
    printf(1,"\n")
end for
for k=1 to lim1 do
    mpz l100k = l[lim1][k] 
    if mpz_cmp(l100k,m100) > 0 then
        mpz_set(m100,l100k)
    end if
end for
printf(1,"\nThe maximum l(100,k): %s\n",shorten(mpz_get_str(m100)))
```

```txt

Unsigned Lah numbers: l(n, k):
 n   k:     0          1          2          3          4          5          6          7          8          9         10         11         12
---    ------------------------------------------------------------------------------------------------------------------------------------------
 0          1
 1          0          1
 2          0          2          1
 3          0          6          6          1
 4          0         24         36         12          1
 5          0        120        240        120         20          1
 6          0        720       1800       1200        300         30          1
 7          0       5040      15120      12600       4200        630         42          1
 8          0      40320     141120     141120      58800      11760       1176         56          1
 9          0     362880    1451520    1693440     846720     211680      28224       2016         72          1
10          0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1
11          0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1
12          0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132          1

The maximum l(100,k): 4451900544899314481...0000000000000000000 (164 digits)

```



## REXX

Some extra code was added to minimize the column widths in the displaying of the numbers. 

Also, code was added to use memoization of the factorial calculations. 

```rexx
/*REXX pgm computes & display (unsigned) Stirling numbers of the 3rd kind (Lah numbers).*/
parse arg lim .                                  /*obtain optional argument from the CL.*/
if lim=='' | lim==","  then lim= 12              /*Not specified?  Then use the default.*/
olim= lim                                        /*save     the original value of  LIM. */
lim= abs(lim)                                    /*only use the absolute value of  LIM. */
numeric digits max(9, 4*lim)                     /*(over) specify maximum number in grid*/
max#.= 0
!.=.
@.=                                              /* [↓]  calculate values for the grid. */
        do   n=0  to  lim;   nm= n - 1
          do k=0  to  lim;   km= k - 1
          if k==1               then do;  @.n.k= !(n); call maxer; iterate;  end
          if k==n               then do;  @.n.k= 1   ;             iterate;  end
          if k>n | k==0 | n==0  then do;  @.n.k= 0   ;             iterate;  end
          @.n.k = (!(n) * !(nm)) % (!(k) * !(km)) % !(n-k)  /*calculate a # in the grid.*/
          call maxer                                        /*find    max #  "  "    "  */
          end   /*k*/
        end     /*n*/

        do k=0  for lim+1                        /*find max column width for each column*/
        max#.a= max#.a + length(max#.k)
        end   /*k*/
                                                 /* [↓]  only show the maximum value ?  */
w= length(max#.b)                                /*calculate max width of all numbers.  */
if olim<0  then do;  say 'The maximum value  (which has '      w      " decimal digits):"
                     say max#.b                  /*display maximum number in the grid.  */
                     exit                        /*stick a fork in it,  we're all done. */
                end                              /* [↑]  the 100th row is when LIM is 99*/
wi= max(3, length(lim+1) )                       /*the maximum width of the grid's index*/
say 'row'  center('columns', max(9, max#.a + lim), '═')    /*display header of the grid.*/

        do r=0  for lim+1;   $=                  /* [↓]  display the grid to the term.  */
          do c=0  for lim+1  until c>=r          /*build a row of grid, 1 col at a time.*/
          $= $  right(@.r.c, length(max#.c) )    /*append a column to a row of the grid.*/
          end   /*c*/
        say right(r,wi)  strip(substr($,2), 'T') /*display a single row of the grid.    */
        end     /*r*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!: parse arg z; if !.z\==. then return !.z; !=1; do f=2  to z; !=!*f; end; !.z=!; return !
maxer:   max#.k= max(max#.k, @.n.k);        max#.b= max(max#.b, @.n.k);           return
```

```txt

row ══════════════════════════════════════════════columns═══════════════════════════════════════════════
  0 1
  1 0         1
  2 0         2          1
  3 0         6          6          1
  4 0        24         36         12          1
  5 0       120        240        120         20          1
  6 0       720       1800       1200        300         30         1
  7 0      5040      15120      12600       4200        630        42        1
  8 0     40320     141120     141120      58800      11760      1176       56       1
  9 0    362880    1451520    1693440     846720     211680     28224     2016      72      1
 10 0   3628800   16329600   21772800   12700800    3810240    635040    60480    3240     90    1
 11 0  39916800  199584000  299376000  199584000   69854400  13970880  1663200  118800   4950  110   1
 12 0 479001600 2634508800 4390848000 3293136000 1317254400 307359360 43908480 3920400 217800 7260 132 1

```

```txt

The maximum value  (which has  164  decimal digits):
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000

```



## Sidef


```ruby
func lah(n, k) {
    stirling3(n, k)
    #binomial(n-1, k-1) * n!/k!     # alternative formula
}

const r = (0..12)

var triangle = r.map {|n| 0..n -> map {|k| lah(n, k) } }
var widths   = r.map {|n| r.map {|k| (triangle[k][n] \\ 0).len }.max }

say ('n\k ', r.map {|n| "%*s" % (widths[n], n) }.join(' '))

r.each {|n|
    var str = ('%-3s ' % n)
    str += triangle[n].map_kv {|k,v| "%*s" % (widths[k], v) }.join(' ')
    say str
}

with (100) {|n|
    say "\nMaximum value from the L(#{n}, *) row:"
    say { lah(n, _) }.map(^n).max
}
```

```txt

n\k 0         1          2          3          4          5         6        7       8      9   10  11 12
0   1
1   0         1
2   0         2          1
3   0         6          6          1
4   0        24         36         12          1
5   0       120        240        120         20          1
6   0       720       1800       1200        300         30         1
7   0      5040      15120      12600       4200        630        42        1
8   0     40320     141120     141120      58800      11760      1176       56       1
9   0    362880    1451520    1693440     846720     211680     28224     2016      72      1
10  0   3628800   16329600   21772800   12700800    3810240    635040    60480    3240     90    1
11  0  39916800  199584000  299376000  199584000   69854400  13970880  1663200  118800   4950  110   1
12  0 479001600 2634508800 4390848000 3293136000 1317254400 307359360 43908480 3920400 217800 7260 132 1

Maximum value from the L(100, *) row:
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000

```



## zkl


```zkl
fcn lah(n,k,fact=fcn(n){ [1..n].reduce('*,1) }){
   if(n==k)	  return(1);
   if(k==1)	  return(fact(n));
   if(n<1 or k<1) return(0);  
   (fact(n)*fact(n - 1)) /(fact(k)*fact(k - 1)) /fact(n - k)
}
```


```zkl
// calculate entire table (quick), find max, find num digits in max
N,mx := 12, [1..N].apply(fcn(n){ [1..n].apply(lah.fp(n)) }).flatten() : (0).max(_);
fmt:="%%%dd".fmt("%d".fmt(mx.numDigits + 1)).fmt;  // "%9d".fmt
println("Unsigned Lah numbers: L(n,k):");
println("n\\k",[0..N].pump(String,fmt));
foreach row in ([0..N]){
   println("%3d".fmt(row), [0..row].pump(String, lah.fp(row), fmt));
}
```

<pre style="font-size:83%">
Unsigned Lah numbers: L(n,k):
n\k          0          1          2          3          4          5          6          7          8          9         10         11         12
  0          0
  1          0          1
  2          0          2          1
  3          0          6          6          1
  4          0         24         36         12          1
  5          0        120        240        120         20          1
  6          0        720       1800       1200        300         30          1
  7          0       5040      15120      12600       4200        630         42          1
  8          0      40320     141120     141120      58800      11760       1176         56          1
  9          0     362880    1451520    1693440     846720     211680      28224       2016         72          1
 10          0    3628800   16329600   21772800   12700800    3810240     635040      60480       3240         90          1
 11          0   39916800  199584000  299376000  199584000   69854400   13970880    1663200     118800       4950        110          1
 12          0  479001600 2634508800 4390848000 3293136000 1317254400  307359360   43908480    3920400     217800       7260        132          1

```

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
N=100; 
L100:=[1..N].apply(lah.fpM("101",BI(N),fcn(n){ BI(n).factorial() }))
       .reduce(fcn(m,n){ m.max(n) });
println("Maximum value from the L(%d, *) row (%d digits):".fmt(N,L100.numDigits));
println(L100);
```

<pre style="font-size:83%">
Maximum value from the L(100, *) row (164 digits):
44519005448993144810881324947684737529186447692709328597242209638906324913313742508392928375354932241404408343800007105650554669129521241784320000000000000000000000

```

