+++
title = "Stirling numbers of the second kind"
description = ""
date = 2019-09-20T16:14:59Z
aliases = []
[extra]
id = 22466
[taxonomies]
categories = ["task"]
tags = []
+++

Stirling numbers of the second kind, or Stirling partition numbers, are the
number of ways to partition a set of n objects into k non-empty subsets. They are
closely related to [[Bell numbers]], and may be derived from them.


Stirling numbers of the second kind obey the recurrence relation:

    S2(n, 0) and S2(0, k) = 0 # for n, k > 0
    S2(n, n) = 1
    S2(n + 1, k) = k * S2(n, k) + S2(n, k - 1)



## Task

:* Write a routine (function, procedure, whatever) to find '''Stirling numbers of the second kind'''. There are several methods to generate Stirling numbers of the second kind. You are free to choose the most appropriate for your language. If your language has a built-in, or easily, publicly available library implementation, it is acceptable to use that.

:* Using the routine, generate and show here, on this page, a table (or triangle) showing the Stirling numbers of the second kind, '''S2(n, k)''', up to '''S2(12, 12)'''. it is optional to show the row / column for n == 0 and k == 0. It is optional to show places where S2(n, k) == 0 (when k > n).

:* If your language supports large integers, find and show here, on this page, the maximum value of '''S2(n, k)''' where '''n == 100'''.


## See also

:* '''[[wp:Stirling_numbers_of_the_second_kind|Wikipedia - Stirling numbers of the second kind]]'''
:* '''[[oeis:A008277|OEIS:A008277 - Stirling numbers of the second kind]]'''


;Related Tasks:

:* '''[[Stirling_numbers_of_the_first_kind|Stirling numbers of the first kind]]'''
:* '''[[Bell_numbers|Bell numbers]]'''
:* '''[[Lah_numbers|Lah numbers]]'''






## ALGOL 68

Uses the LONG LONG INT mode of Algol 68g which allows large precision integers. As the default precision of LONG LONG INT is too small, the precision is specified via a pragmatic comment.

```algol68
BEGIN
    # show some Stirling numbers of the second kind                          #

    # specify the precision of LONG LONG INT, somewhat under 160 digits are  #
    # needed for Stirling numbers of the second kind with n, k = 100         #
    PR precision 160 PR
    MODE SINT = LONG LONG INT;

    # returns a triangular matrix of Stirling numbers up to max n, max n #
    PROC make s2 = ( INT max n )REF[,]SINT:
    BEGIN
        REF[,]SINT s2 := HEAP[ 0 : max n, 0 : max n ]SINT;
        FOR n FROM 0 TO max n DO FOR k FROM 0 TO max n DO s2[ n, k ] := 0 OD OD;
        FOR n FROM 0 TO max n DO s2[ n, n ] := 1 OD;
        FOR n FROM 0 TO max n - 1 DO
            FOR k FROM 1 TO n DO
                s2[ n + 1, k ] := k * s2[ n, k ] + s2[ n, k - 1 ];
            OD
        OD;
        s2
    END # make s2 # ;
    # task requirements:                                                     #
    # print Stirling numbers up to n, k = 12                                 #
    BEGIN
        INT max stirling = 12;
        REF[,]SINT s2 = make s2( max stirling );
        print( ( "Stirling numbers of the second kind:", newline ) );
        print( ( " k" ) );
        FOR k FROM 0 TO max stirling DO print( ( whole( k, -10 ) ) ) OD;
        print( ( newline, " n", newline ) );
        FOR n FROM 0 TO max stirling DO
            print( ( whole( n, -2 ) ) );
            FOR k FROM 0 TO n DO
                print( ( whole( s2[ n, k ], -10 ) ) )
            OD;
            print( ( newline ) )
        OD
    END;
    # find the maximum Stirling number with n = 100                          #
    BEGIN
        INT max stirling = 100;
        REF[,]SINT s2 = make s2( max stirling );
        SINT max 100 := 0;
        FOR k FROM 0 TO max stirling DO
            IF s2[ max stirling, k ] > max 100 THEN max 100 := s2[ max stirling, k ] FI
        OD;
        print( ( "Maximum Stirling number of the second kind with n = 100:", newline ) );
        print( ( whole( max 100, 0 ), newline ) )
    END
END
```

```txt

Stirling numbers of the second kind:
 k         0         1         2         3         4         5         6         7         8         9        10        11        12
 n
 0         1
 1         0         1
 2         0         1         1
 3         0         1         3         1
 4         0         1         7         6         1
 5         0         1        15        25        10         1
 6         0         1        31        90        65        15         1
 7         0         1        63       301       350       140        21         1
 8         0         1       127       966      1701      1050       266        28         1
 9         0         1       255      3025      7770      6951      2646       462        36         1
10         0         1       511      9330     34105     42525     22827      5880       750        45         1
11         0         1      1023     28501    145750    246730    179487     63987     11880      1155        55         1
12         0         1      2047     86526    611501   1379400   1323652    627396    159027     22275      1705        66         1
Maximum Stirling number of the second kind with n = 100:
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674

```



## Factor

```factor
USING: combinators.short-circuit formatting io kernel math
math.extras prettyprint sequences ;
RENAME: stirling math.extras => (stirling)
IN: rosetta-code.stirling-second

! Tweak Factor's in-built stirling function for k=0
: stirling ( n k -- m )
    2dup { [ = not ] [ nip zero? ] } 2&&
    [ 2drop 0 ] [ (stirling) ] if ;

"Stirling numbers of the second kind: n k stirling:" print
"n\\k" write 13 dup [ "%8d" printf ] each-integer nl

<iota> [
    dup dup "%-2d " printf [0,b] [
        stirling "%8d" printf
    ] with each nl
] each nl

"Maximum value from the 100 _ stirling row:" print
100 <iota> [ 100 swap stirling ] map supremum .
```

```txt

Stirling numbers of the second kind: n k stirling:
n\k       0       1       2       3       4       5       6       7       8       9      10      11      12
0         1
1         0       1
2         0       1       1
3         0       1       3       1
4         0       1       7       6       1
5         0       1      15      25      10       1
6         0       1      31      90      65      15       1
7         0       1      63     301     350     140      21       1
8         0       1     127     966    1701    1050     266      28       1
9         0       1     255    3025    7770    6951    2646     462      36       1
10        0       1     511    9330   34105   42525   22827    5880     750      45       1
11        0       1    1023   28501  145750  246730  179487   63987   11880    1155      55       1
12        0       1    2047   86526  611501 1379400 1323652  627396  159027   22275    1705      66       1

Maximum value from the 100 _ stirling row:
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Stirling_numbers_of_the_second_kind this] page you can see the solution of this task.

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
    s2 := make([][]*big.Int, limit+1)
    for n := 0; n <= limit; n++ {
        s2[n] = make([]*big.Int, limit+1)
        for k := 0; k <= limit; k++ {
            s2[n][k] = new(big.Int)
        }
        s2[n][n].SetInt64(int64(1))
    }
    var t big.Int
    for n := 1; n <= limit; n++ {
        for k := 1; k <= n; k++ {
            t.SetInt64(int64(k))
            t.Mul(&t, s2[n-1][k])
            s2[n][k].Add(&t, s2[n-1][k-1])
        }
    }
    fmt.Println("Stirling numbers of the second kind: S2(n, k):")
    fmt.Printf("n/k")
    for i := 0; i <= last; i++ {
        fmt.Printf("%9d ", i)
    }
    fmt.Printf("\n--")
    for i := 0; i <= last; i++ {
        fmt.Printf("----------")
    }
    fmt.Println()
    for n := 0; n <= last; n++ {
        fmt.Printf("%2d ", n)
        for k := 0; k <= n; k++ {
            fmt.Printf("%9d ", s2[n][k])
        }
        fmt.Println()
    }
    fmt.Println("\nMaximum value from the S2(100, *) row:")
    max := new(big.Int).Set(s2[limit][0])
    for k := 1; k <= limit; k++ {
        if s2[limit][k].Cmp(max) > 0 {
            max.Set(s2[limit][k])
        }
    }
    fmt.Println(max)
    fmt.Printf("which has %d digits.\n", len(max.String()))
}
```


```txt

Stirling numbers of the second kind: S2(n, k):
n/k        0         1         2         3         4         5         6         7         8         9        10        11        12 
------------------------------------------------------------------------------------------------------------------------------------
 0         1 
 1         0         1 
 2         0         1         1 
 3         0         1         3         1 
 4         0         1         7         6         1 
 5         0         1        15        25        10         1 
 6         0         1        31        90        65        15         1 
 7         0         1        63       301       350       140        21         1 
 8         0         1       127       966      1701      1050       266        28         1 
 9         0         1       255      3025      7770      6951      2646       462        36         1 
10         0         1       511      9330     34105     42525     22827      5880       750        45         1 
11         0         1      1023     28501    145750    246730    179487     63987     11880      1155        55         1 
12         0         1      2047     86526    611501   1379400   1323652    627396    159027     22275      1705        66         1 

Maximum value from the S2(100, *) row:
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674
which has 115 digits.

```



## Julia


```julia
using Combinatorics

const s2cache = Dict()

function stirlings2(n, k)
    if haskey(s2cache, Pair(n, k))
        return s2cache[Pair(n, k)]
    elseif n < 0
        throw(DomainError(n, "n must be nonnegative"))
    elseif n == k == 0
        return one(n)
    elseif n == 0 || k == 0
        return zero(n)
    elseif k == n - 1
        return binomial(n, 2)
    elseif k == 2
        return 2^(n-1) - 1
    end

    ret = k * stirlings2(n - 1, k) + stirlings2(n - 1, k - 1)
    s2cache[Pair(n, k)] = ret
    return ret

end

function printstirling2table(kmax)
    println("  ", mapreduce(i -> lpad(i, 10), *, 0:kmax))

    sstring(n, k) = begin i = stirlings2(n, k); lpad(k > n && i == 0 ? "" : i, 10) end

    for n in 0:kmax
        println(rpad(n, 2) * mapreduce(k -> sstring(n, k), *, 0:kmax))
    end
end

printstirling2table(12)
println("\nThe maximum for stirling2(100, _) is: ", maximum(k-> stirlings2(BigInt(100), BigInt(k)), 1:100))


```
```txt

           0         1         2         3         4         5         6         7         8         9        10        11        12
0          1
1          0         1
2          0         1         1
3          0         1         3         1
4          0         1         7         6         1
5          0         1        15        25        10         1
6          0         1        31        90        65        15         1
7          0         1        63       301       350       140        21         1
8          0         1       127       966      1701      1050       266        28         1
9          0         1       255      3025      7770      6951      2646       462        36         1
10         0         1       511      9330     34105     42525     22827      5880       750        45         1
11         0         1      1023     28501    145750    246730    179487     63987     11880      1155        55         1
12         0         1      2047     86526    611501   1379400   1323652    627396    159027     22275      1705        66         1

The maximum for stirling2(100, _) is:
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674

```



## Perl


```perl
use strict;
use warnings;
use bigint;
use feature 'say';
use feature 'state';
no warnings 'recursion';
use List::Util qw(max);

sub Stirling2 {
    my($n, $k) = @_;
    my $n1 = $n - 1;
    return 1 if     $n1 == $k;
    return 0 unless $n1 && $k;
    state %seen;
    return ($seen{"{$n1}|{$k}"  } //= Stirling2($n1,$k  ) * $k) +
           ($seen{"{$n1}|{$k-1}"} //= Stirling2($n1,$k-1))
}

my $upto  = 12;
my $width = 1 + length max map { Stirling2($upto+1,$_) } 0..$upto+1;

say 'Unsigned Stirling2 numbers of the second kind: S2(n, k):';
print 'n\k' . sprintf "%${width}s"x(1+$upto)."\n", 0..$upto;

for my $row (1..$upto+1) {
    printf '%-3d', $row-1;
    printf "%${width}d", Stirling2($row, $_) for 0..$row-1;
    print "\n";
}

say "\nMaximum value from the S2(100, *) row:";
say max map { Stirling2(101,$_) } 0..100;
```

```txt
Unsigned Stirling2 numbers of the second kind: S2(n, k):
n\k       0       1       2       3       4       5       6       7       8       9      10      11      12
0         1
1         0       1
2         0       1       1
3         0       1       3       1
4         0       1       7       6       1
5         0       1      15      25      10       1
6         0       1      31      90      65      15       1
7         0       1      63     301     350     140      21       1
8         0       1     127     966    1701    1050     266      28       1
9         0       1     255    3025    7770    6951    2646     462      36       1
10        0       1     511    9330   34105   42525   22827    5880     750      45       1
11        0       1    1023   28501  145750  246730  179487   63987   11880    1155      55       1
12        0       1    2047   86526  611501 1379400 1323652  627396  159027   22275    1705      66       1

Maximum value from the S2(100, *) row:
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674
```



## Perl 6

```perl6
sub Stirling2 (Int \n, Int \k) {
    ((1,), { (0, |@^last) »+« (|(@^last »*« @^last.keys), 0) } … *)[n;k]
}

my $upto = 12;

my $mx = (1..^$upto).map( { Stirling2($upto, $_) } ).max.chars;

put 'Stirling numbers of the second kind: S2(n, k):';
put 'n\k', (0..$upto)».fmt: "%{$mx}d";

for 0..$upto -> $row {
    $row.fmt('%-3d').print;
    put (0..$row).map( { Stirling2($row, $_) } )».fmt: "%{$mx}d";
}

say "\nMaximum value from the S2(100, *) row:";
say (^100).map( { Stirling2 100, $_ } ).max;
```

```txt
Stirling numbers of the second kind: S2(n, k):
n\k      0       1       2       3       4       5       6       7       8       9      10      11      12
0        1
1        0       1
2        0       1       1
3        0       1       3       1
4        0       1       7       6       1
5        0       1      15      25      10       1
6        0       1      31      90      65      15       1
7        0       1      63     301     350     140      21       1
8        0       1     127     966    1701    1050     266      28       1
9        0       1     255    3025    7770    6951    2646     462      36       1
10       0       1     511    9330   34105   42525   22827    5880     750      45       1
11       0       1    1023   28501  145750  246730  179487   63987   11880    1155      55       1
12       0       1    2047   86526  611501 1379400 1323652  627396  159027   22275    1705      66       1

Maximum value from the S2(100, *) row:
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674
```



## Phix

```Phix
include mpfr.e
 
constant lim = 100,
         lim1 = lim+1,
         last = 12
sequence s2 = repeat(0,lim1)
for n=1 to lim1 do
    s2[n] = mpz_inits(lim1)
    mpz_set_si(s2[n][n],1)
end for
mpz {t, m100} = mpz_inits(2)
for n=1 to lim do
    for k=1 to n do
        mpz_set_si(t,k)
        mpz_mul(t,t,s2[n][k+1])
        mpz_add(s2[n+1][k+1],t,s2[n][k])
    end for
end for
printf(1,"Stirling numbers of the second kind: S2(n, k):\n n   k:")
for i=0 to last do
    printf(1,"%5d     ", i)
end for
printf(1,"\n---    %s\n",repeat('-',last*10+5))
for n=0 to last do
    printf(1,"%2d ", n)
    for k=1 to n+1 do
        mpfr_printf(1,"%9Zd ", s2[n+1][k])
    end for
    printf(1,"\n")
end for
for k=1 to lim1 do
    mpz s100k = s2[lim1][k] 
    if mpz_cmp(s100k,m100) > 0 then
        mpz_set(m100,s100k)
    end if
end for
printf(1,"\nThe maximum S2(100,k): %s\n",shorten(mpz_get_str(m100)))
```

```txt

Stirling numbers of the second kind: S2(n, k):
 n  k:     0         1         2         3         4         5         6         7         8         9        10        11        12
---   ------------------------------------------------------------------------------------------------------------------------------
 0         1
 1         0         1
 2         0         1         1
 3         0         1         3         1
 4         0         1         7         6         1
 5         0         1        15        25        10         1
 6         0         1        31        90        65        15         1
 7         0         1        63       301       350       140        21         1
 8         0         1       127       966      1701      1050       266        28         1
 9         0         1       255      3025      7770      6951      2646       462        36         1
10         0         1       511      9330     34105     42525     22827      5880       750        45         1
11         0         1      1023     28501    145750    246730    179487     63987     11880      1155        55         1
12         0         1      2047     86526    611501   1379400   1323652    627396    159027     22275      1705        66         1

Maximum value from the S2(100, *) row: 7769730053598745155...3545178960761551674 (115 digits)

```



## REXX

Some extra code was added to minimize the displaying of the column widths.
<lang>/*REXX program to compute and display   Stirling numbers   of the second kind.          */
parse arg lim .                                  /*obtain optional argument from the CL.*/
if lim=='' | lim==","  then lim= 12              /*Not specified?  Then use the default.*/
olim= lim                                        /*save     the original value of  LIM. */
lim= abs(lim)                                    /*only use the absolute value of  LIM. */
numeric digits max(9, 2*lim)                     /*(over) specify maximum number in grid*/
@.=
        do j=0  for lim+1
        @.j.j = 1;  if j==0  then iterate        /*define the right descending diagonal.*/
        @.0.j = 0;  @.j.0 = 0                    /*   "    "  zero  values.             */
        end   /*j*/
max#.= 0                                         /* [↓]  calculate values for the grid. */
        do   n=0  for lim+1;         np= n + 1
          do k=1  for lim;           km= k - 1
          @.np.k = k * @.n.k  +  @.n.km          /*calculate a number in the grid.      */
          max#.k= max(max#.k, @.n.k)             /*find the maximum value for a column. */
          max#.b= max(max#.b, @.n.k)             /*find the maximum value for all rows. */
          end   /*k*/
        end     /*n*/
                                                 /* [↓]  only show the maximum value ?  */
        do k=0  for lim+1                        /*find max column width for each column*/
        max#.a= max#.a + length(max#.k)
        end   /*k*/

w= length(max#.b)                                /*calculate max width of all numbers.  */
if olim<0  then do;  say 'The maximum value  (which has '       w      " decimal digits):"
                     say max#.b                  /*display maximum number in the grid.  */
                     exit                        /*stick a fork in it,  we're all done. */
                end
wi= max(3, length(lim+1) )                       /*the maximum width of the grid's index*/
say 'row'  center('columns', max(9, max#.a + lim), '═')    /*display header of the grid.*/

        do   r=0  for lim+1; $=                  /* [↓]  display the grid to the term.  */
          do c=0  for lim+1  until c>=r          /*build a row of grid, 1 col at a time.*/
          $= $  right(@.r.c, length(max#.c) )    /*append a column to a row of the grid.*/
          end   /*c*/
        say right(r,wi)  strip(substr($,2), 'T') /*display a single row of the grid.    */
        end     /*r*/                            /*stick a fork in it,  we're all done. */
```

```txt

row ══════════════════════════════columns══════════════════════════════
  0 1
  1 0 1
  2 0 1    1
  3 0 1    3     1
  4 0 1    7     6      1
  5 0 1   15    25     10       1
  6 0 1   31    90     65      15       1
  7 0 1   63   301    350     140      21      1
  8 0 1  127   966   1701    1050     266     28      1
  9 0 1  255  3025   7770    6951    2646    462     36     1
 10 0 1  511  9330  34105   42525   22827   5880    750    45    1
 11 0 1 1023 28501 145750  246730  179487  63987  11880  1155   55  1
 12 0 1 2047 86526 611501 1379400 1323652 627396 159027 22275 1705 66 1

```

```txt

The maximum value  (which has  115  decimal digits):
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674

```



## Sidef


```ruby
func S2(n, k) {     # Stirling numbers of the second kind
    stirling2(n, k)
}

const r = (0..12)

var triangle = r.map {|n| 0..n -> map {|k| S2(n, k) } }
var widths   = r.map {|n| r.map {|k| (triangle[k][n] \\ 0).len }.max }

say ('n\k ', r.map {|n| "%*s" % (widths[n], n) }.join(' '))

r.each {|n|
    var str = ('%-3s ' % n)
    str += triangle[n].map_kv {|k,v| "%*s" % (widths[k], v) }.join(' ')
    say str
}

with (100) {|n|
    say "\nMaximum value from the S2(#{n}, *) row:"
    say { S2(n, _) }.map(^n).max
}
```

```txt

n\k 0 1    2     3      4       5       6      7      8     9   10 11 12
0   1
1   0 1
2   0 1    1
3   0 1    3     1
4   0 1    7     6      1
5   0 1   15    25     10       1
6   0 1   31    90     65      15       1
7   0 1   63   301    350     140      21      1
8   0 1  127   966   1701    1050     266     28      1
9   0 1  255  3025   7770    6951    2646    462     36     1
10  0 1  511  9330  34105   42525   22827   5880    750    45    1
11  0 1 1023 28501 145750  246730  179487  63987  11880  1155   55  1
12  0 1 2047 86526 611501 1379400 1323652 627396 159027 22275 1705 66 1

Maximum value from the S2(100, *) row:
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674

```


Alternatively, the '''S2(n,k)''' function can be defined as:

```ruby
func S2((0), (0)) { 1 }
func S2(_, (0))   { 0 }
func S2((0), _)   { 0 }
func S2(n, k) is cached { S2(n-1, k)*k + S2(n-1, k-1) }
```



## zkl


```zkl
fcn stirling2(n,k){
   var seen=Dictionary();	// cache for recursion
   if(n==k)       return(1);	// (0.0)==1
   if(n<1 or k<1) return(0);
   z1,z2 := "%d,%d".fmt(n-1,k), "%d,%d".fmt(n-1,k-1);
   if(Void==(s1 := seen.find(z1))){ s1 = seen[z1] = stirling2(n-1,k)   }
   if(Void==(s2 := seen.find(z2))){ s2 = seen[z2] = stirling2(n-1,k-1) }
   k*s1 + s2;   // k is first to cast to BigInt (if using BigInts)
}
```


```zkl
// calculate entire table (cached), find max, find num digits in max
N,mx := 12, [1..N].apply(fcn(n){ [1..n].apply(stirling2.fp(n)) }).flatten() : (0).max(_);
fmt:="%%%dd".fmt("%d".fmt(mx.numDigits + 1)).fmt;  // "%9d".fmt
println("Stirling numbers of the second kind: S2(n,k):");
println("n\\k",[0..N].pump(String,fmt));
foreach row in ([0..N]){
   println("%3d".fmt(row), [0..row].pump(String, stirling2.fp(row), fmt));
}
```

<pre style="font-size:83%">
Stirling numbers of the second kind: S2(n,k):
n\k       0       1       2       3       4       5       6       7       8       9      10      11      12
  0       1
  1       0       1
  2       0       1       1
  3       0       1       3       1
  4       0       1       7       6       1
  5       0       1      15      25      10       1
  6       0       1      31      90      65      15       1
  7       0       1      63     301     350     140      21       1
  8       0       1     127     966    1701    1050     266      28       1
  9       0       1     255    3025    7770    6951    2646     462      36       1
 10       0       1     511    9330   34105   42525   22827    5880     750      45       1
 11       0       1    1023   28501  145750  246730  179487   63987   11880    1155      55       1
 12       0       1    2047   86526  611501 1379400 1323652  627396  159027   22275    1705      66       1

```

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
N=100; 
S2100:=[BI(2)..N].apply(stirling2.fp(BI(N))).reduce(fcn(m,n){ m.max(n) });
println("Maximum value from the S2(%d,*) row (%d digits):".fmt(N,S2100.numDigits));
println(S2100);
```

<pre style="font-size:83%">
Maximum value from the S2(100,*) row (115 digits):
7769730053598745155212806612787584787397878128370115840974992570102386086289805848025074822404843545178960761551674

```

