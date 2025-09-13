+++
title = "Stirling numbers of the first kind"
description = ""
date = 2019-09-20T16:13:23Z
aliases = []
[extra]
id = 22465
[taxonomies]
categories = ["task"]
tags = []
+++

Stirling numbers of the first kind, or Stirling cycle numbers, count permutations according to their number
of cycles (counting fixed points as cycles of length one).

They may be defined directly to be the number of permutations of '''n'''
elements with '''k''' disjoint cycles.

Stirling numbers of the first kind express coefficients of polynomial expansions of falling or rising factorials.

Depending on the application, Stirling numbers of the first kind may be "signed"
or "unsigned". Signed Stirling numbers of the first kind arise when the
polynomial expansion is expressed in terms of falling factorials; unsigned when
expressed in terms of rising factorials. The only substantial difference is that,
for signed Stirling numbers of the first kind, values of S1(n, k) are negative
when n + k is odd.

Stirling numbers of the first kind follow the simple identities:

    S1(0, 0) = 1
    S1(n, 0) = 0 if n > 0
    S1(n, k) = 0 if k > n
    S1(n, k) = S1(n - 1, k - 1) + (n - 1) * S1(n - 1, k) # For unsigned
      ''or''
    S1(n, k) = S1(n - 1, k - 1) - (n - 1) * S1(n - 1, k) # For signed


## Task

:* Write a routine (function, procedure, whatever) to find '''Stirling numbers of the first kind'''. There are several methods to generate Stirling numbers of the first kind. You are free to choose the most appropriate for your language. If your language has a built-in, or easily, publicly available library implementation, it is acceptable to use that.

:* Using the routine, generate and show here, on this page, a table (or triangle) showing the Stirling numbers of the first kind, '''S1(n, k)''', up to '''S1(12, 12)'''. it is optional to show the row / column for n == 0 and k == 0. It is optional to show places where S1(n, k) == 0 (when k > n). You may choose to show signed or unsigned Stirling numbers of the first kind, just make a note of which was chosen.

:* If your language supports large integers, find and show here, on this page, the maximum value of '''S1(n, k)''' where '''n == 100'''.


## See also

:* '''[[wp:Stirling_numbers_of_the_first_kind|Wikipedia - Stirling numbers of the first kind]]'''
:* '''[[oeis:A008275|OEIS:A008275 - Signed Stirling numbers of the first kind]]'''
:* '''[[oeis:A130534|OEIS:A130534 - Unsigned Stirling numbers of the first kind]]'''


;Related Tasks:

:* '''[[Stirling_numbers_of_the_second_kind|Stirling numbers of the second kind]]'''
:* '''[[Lah_numbers|Lah numbers]]'''






## ALGOL 68

Uses the Algol 68G LONG LONG INT mode which provides large-precision integers. As the default number of digits is insufficient for the task, the maximum nunber of digits is specified by a pragmatic comment. 

```algol68
BEGIN
    # show some (unsigned) Stirling numbers of the first kind            #

    # specify the precision of LONG LONG INT, we need about 160 digits   #
    # for Stirling numbers of the first kind with n, k = 100             #
    PR precision 160 PR
    MODE SINT = LONG LONG INT;

    # returns a triangular matrix of Stirling numbers up to max n, max n #
    # the numbers are signed if signed is TRUE, unsigned otherwise       #
    PROC make s1 = ( INT max n, BOOL signed )REF[,]SINT:
    BEGIN
        REF[,]SINT s1 := HEAP[ 0 : max n, 0 : max n ]SINT;
        FOR n FROM 0 TO max n DO FOR k FROM 0 TO max n DO s1[ n, k ] := 0 OD OD;
        s1[ 0, 0 ] := 1;
        FOR n FROM 1 TO max n DO s1[ n, 0 ] := 0 OD;
        FOR n FROM 1 TO max n DO
            FOR k FROM 1 TO n DO
                SINT s1 term = ( ( n - 1 ) * s1[ n - 1, k ] );
                s1[ n, k ] := s1[ n - 1, k - 1 ] + IF signed THEN - s1 term ELSE s1 term FI
            OD
        OD;
        s1
    END # make s1 # ;
    # task requirements:                                                #
    # print Stirling numbers up to n, k = 12                            #
    BEGIN
        INT max stirling = 12;
        REF[,]SINT s1 = make s1( max stirling, FALSE );
        print( ( "Unsigned Stirling numbers of the first kind:", newline ) );
        print( ( " k" ) );
        FOR k FROM 0 TO max stirling DO print( ( whole( k, -10 ) ) ) OD;
        print( ( newline, " n", newline ) );
        FOR n FROM 0 TO max stirling DO
            print( ( whole( n, -2 ) ) );
            FOR k FROM 0 TO n DO
                print( ( whole( s1[ n, k ], -10 ) ) )
            OD;
            print( ( newline ) )
        OD
    END;
    # find the maximum Stirling number with n = 100                     #
    BEGIN
        INT max stirling = 100;
        REF[,]SINT s1 = make s1( max stirling, FALSE );
        SINT max 100 := 0;
        FOR k FROM 0 TO max stirling DO
            IF s1[ max stirling, k ] > max 100 THEN max 100 := s1[ max stirling, k ] FI
        OD;
        print( ( "Maximum Stirling number of the first kind with n = 100:", newline ) );
        print( ( whole( max 100, 0 ), newline ) )
    END
END
```

```txt

Unsigned Stirling numbers of the first kind:
 k         0         1         2         3         4         5         6         7         8         9        10        11        12
 n
 0         1
 1         0         1
 2         0         1         1
 3         0         2         3         1
 4         0         6        11         6         1
 5         0        24        50        35        10         1
 6         0       120       274       225        85        15         1
 7         0       720      1764      1624       735       175        21         1
 8         0      5040     13068     13132      6769      1960       322        28         1
 9         0     40320    109584    118124     67284     22449      4536       546        36         1
10         0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1
11         0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1
12         0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1
Maximum Stirling number of the first kind with n = 100:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000

```



## Factor

Here we calculate a row at a time as the coefficients of the falling factorial <tt>x(x-1)(x-2)...(x-n+1)</tt> using Factor's built-in polynomial arithmetic.

For example, <tt>x(x-1)(x-2) = x<sup>3</sup> - 3x<sup>2</sup> + 2x</tt>. Taking the absolute values of the coefficients, the third row is <tt>(0) 2 3 1</tt>.
```factor
USING: arrays assocs formatting io kernel math math.polynomials
math.ranges prettyprint sequences ;
IN: rosetta-code.stirling-first

: stirling-row ( n -- seq )
    [ { 1 } ] [
        [ -1 ] dip neg [a,b) dup length 1 <array> zip
        { 0 1 } [ p* ] reduce [ abs ] map
    ] if-zero ;

"Unsigned Stirling numbers of the first kind:" print
"n\\k" write 13 dup [ "%10d" printf ] each-integer nl

[ dup "%-2d " printf stirling-row [ "%10d" printf ] each nl ]
each-integer nl

"Maximum value from 100th stirling row:" print
100 stirling-row supremum .
```

```txt

Unsigned Stirling numbers of the first kind:
n\k         0         1         2         3         4         5         6         7         8         9        10        11        12
0           1
1           0         1
2           0         1         1
3           0         2         3         1
4           0         6        11         6         1
5           0        24        50        35        10         1
6           0       120       274       225        85        15         1
7           0       720      1764      1624       735       175        21         1
8           0      5040     13068     13132      6769      1960       322        28         1
9           0     40320    109584    118124     67284     22449      4536       546        36         1
10          0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1
11          0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1
12          0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1

Maximum value from 100th stirling row:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Stirling_numbers_of_the_first_kind this] page you can see the solution of this task.

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
    s1 := make([][]*big.Int, limit+1)
    for n := 0; n <= limit; n++ {
        s1[n] = make([]*big.Int, limit+1)
        for k := 0; k <= limit; k++ {
            s1[n][k] = new(big.Int)
        }
    }
    s1[0][0].SetInt64(int64(1))
    var t big.Int
    for n := 1; n <= limit; n++ {
        for k := 1; k <= n; k++ {
            t.SetInt64(int64(n - 1))
            t.Mul(&t, s1[n-1][k])            
            if unsigned {
                s1[n][k].Add(s1[n-1][k-1], &t)
            } else {
                s1[n][k].Sub(s1[n-1][k-1], &t)
            }           
        }
    }
    fmt.Println("Unsigned Stirling numbers of the first kind: S1(n, k):")
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
            fmt.Printf("%9d ", s1[n][k])
        }
        fmt.Println()
    }
    fmt.Println("\nMaximum value from the S1(100, *) row:")
    max := new(big.Int).Set(s1[limit][0])
    for k := 1; k <= limit; k++ {
        if s1[limit][k].Cmp(max) > 0 {
            max.Set(s1[limit][k])
        }
    }
    fmt.Println(max)
    fmt.Printf("which has %d digits.\n", len(max.String()))
}
```


```txt

Unsigned Stirling numbers of the first kind: S1(n, k):
n/k        0         1         2         3         4         5         6         7         8         9        10        11        12 
------------------------------------------------------------------------------------------------------------------------------------
 0         1 
 1         0         1 
 2         0         1         1 
 3         0         2         3         1 
 4         0         6        11         6         1 
 5         0        24        50        35        10         1 
 6         0       120       274       225        85        15         1 
 7         0       720      1764      1624       735       175        21         1 
 8         0      5040     13068     13132      6769      1960       322        28         1 
 9         0     40320    109584    118124     67284     22449      4536       546        36         1 
10         0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1 
11         0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1 
12         0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1 

Maximum value from the S1(100, *) row:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000
which has 158 digits.

```



## Julia


```julia
using Combinatorics

const s1cache = Dict()

function stirlings1(n, k, signed::Bool=false)
    if signed == true && isodd(n - k)
        return -stirlings1(n, k)
    elseif haskey(s1cache, Pair(n, k))
        return s1cache[Pair(n, k)]
    elseif n < 0
        throw(DomainError(n, "n must be nonnegative"))
    elseif n == k == 0
        return one(n)
    elseif n == 0 || k == 0
        return zero(n)
    elseif n == k
        return one(n)
    elseif k == 1
        return factorial(n-1)
    elseif k == n - 1
        return binomial(n, 2)
    elseif k == n - 2
        return div((3 * n - 1) * binomial(n, 3), 4)
    elseif k == n - 3
        return binomial(n, 2) * binomial(n, 4)
    end

    ret = (n - 1) * stirlings1(n - 1, k) + stirlings1(n - 1, k - 1)
    s1cache[Pair(n, k)] = ret
    return ret
end

function printstirling1table(kmax)
    println("  ", mapreduce(i -> lpad(i, 10), *, 0:kmax))

    sstring(n, k) = begin i = stirlings1(n, k); lpad(k > n && i == 0 ? "" : i, 10) end

    for n in 0:kmax
        println(rpad(n, 2) * mapreduce(k -> sstring(n, k), *, 0:kmax))
    end
end

printstirling1table(12)
println("\nThe maximum for stirling1(100, _) is:\n", maximum(k-> stirlings1(BigInt(100), BigInt(k)), 1:100))

```
```txt

           0         1         2         3         4         5         6         7         8         9        10        11        12
0          1
1          0         1
2          0         1         1
3          0         2         3         1
4          0         6        11         6         1
5          0        24        50        35        10         1
6          0       120       274       225        85        15         1
7          0       720      1764      1624       735       175        21         1
8          0      5040     13068     13132      6769      1960       322        28         1
9          0     40320    109584    118124     67284     22449      4536       546        36         1
10         0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1
11         0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1
12         0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1

The maximum for stirling1(100, _) is:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000

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

sub Stirling1 {
    my($n, $k) = @_;
    return 1 unless $n || $k;
    return 0 unless $n && $k;
    state %seen;
    return ($seen{"{$n-1}|{$k-1}"} //= Stirling1($n-1, $k-1)) +
           ($seen{"{$n-1}|{$k}"  } //= Stirling1($n-1, $k  )) * ($n-1)
}

my $upto  = 12;
my $width = 1 + length max map { Stirling1($upto,$_) } 0..$upto;

say 'Unsigned Stirling1 numbers of the first kind: S1(n, k):';
print 'n\k' . sprintf "%${width}s"x(1+$upto)."\n", 0..$upto;

for my $row (0..$upto) {
    printf '%-3d', $row;
    printf "%${width}d", Stirling1($row, $_) for 0..$row;
    print "\n";
}

say "\nMaximum value from the S1(100, *) row:";
say max map { Stirling1(100,$_) } 0..100;
```

```txt
Unsigned Stirling1 numbers of the first kind: S1(n, k):
n\k         0         1         2         3         4         5         6         7         8         9        10        11        12
0           1
1           0         1
2           0         1         1
3           0         2         3         1
4           0         6        11         6         1
5           0        24        50        35        10         1
6           0       120       274       225        85        15         1
7           0       720      1764      1624       735       175        21         1
8           0      5040     13068     13132      6769      1960       322        28         1
9           0     40320    109584    118124     67284     22449      4536       546        36         1
10          0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1
11          0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1
12          0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1

Maximum value from the S1(100, *) row:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000
```



## Perl 6

```perl6
sub Stirling1 (Int \n, Int \k) {
    return 1 unless n || k;
    return 0 unless n && k;
    state %seen;
    (%seen{"{n - 1}|{k - 1}"} //= Stirling1(n - 1, k - 1)) +
    (n - 1) * (%seen{"{n - 1}|{k}"} //= Stirling1(n - 1, k))
}

my $upto = 12;

my $mx = (1..^$upto).map( { Stirling1($upto, $_) } ).max.chars;

put 'Unsigned Stirling numbers of the first kind: S1(n, k):';
put 'n\k', (0..$upto)».fmt: "%{$mx}d";

for 0..$upto -> $row {
    $row.fmt('%-3d').print;
    put (0..$row).map( { Stirling1($row, $_) } )».fmt: "%{$mx}d";
}

say "\nMaximum value from the S1(100, *) row:";
say (^100).map( { Stirling1 100, $_ } ).max;
```

```txt
Unsigned Stirling numbers of the first kind: S1(n, k):
n\k        0         1         2         3         4         5         6         7         8         9        10        11        12
0          1
1          0         1
2          0         1         1
3          0         2         3         1
4          0         6        11         6         1
5          0        24        50        35        10         1
6          0       120       274       225        85        15         1
7          0       720      1764      1624       735       175        21         1
8          0      5040     13068     13132      6769      1960       322        28         1
9          0     40320    109584    118124     67284     22449      4536       546        36         1
10         0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1
11         0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1
12         0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1

Maximum value from the S1(100, *) row:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000
```



## Phix

```Phix
include mpfr.e
 
constant lim = 100,
         lim1 = lim+1,
         last = 12
bool unsigned = true
sequence s1 = repeat(0,lim1)
for n=1 to lim1 do
    s1[n] = mpz_inits(lim1)
end for
mpz_set_si(s1[1][1],1)
mpz {t, m100} = mpz_inits(2)
for n=1 to lim do
    for k=1 to n do
        mpz_set_si(t,n-1)
        mpz_mul(t,t,s1[n][k+1])
        if unsigned then
            mpz_add(s1[n+1][k+1],s1[n][k],t)
        else
            mpz_sub(s1[n+1][k+1],s1[n][k],t)
        end if
    end for
end for
string s = iff(unsigned?"Uns":"S")
printf(1,"%signed Stirling numbers of the first kind: S1(n, k):\n n   k:",s)
for i=0 to last do
    printf(1,"%5d     ", i)
end for
printf(1,"\n---    %s\n",repeat('-',last*10+5))
for n=0 to last do
    printf(1,"%2d ", n)
    for k=1 to n+1 do
        mpfr_printf(1,"%9Zd ", s1[n+1][k])
    end for
    printf(1,"\n")
end for
for k=1 to lim1 do
    mpz s100k = s1[lim1][k] 
    if mpz_cmp(s100k,m100) > 0 then
        mpz_set(m100,s100k)
    end if
end for
printf(1,"\nThe maximum S1(100,k): %s\n",shorten(mpz_get_str(m100)))
```

```txt

Unsigned Stirling numbers of the first kind: S1(n, k):
 n   k:    0         1         2         3         4         5         6         7         8         9        10        11        12
---    -----------------------------------------------------------------------------------------------------------------------------
 0         1
 1         0         1
 2         0         1         1
 3         0         2         3         1
 4         0         6        11         6         1
 5         0        24        50        35        10         1
 6         0       120       274       225        85        15         1
 7         0       720      1764      1624       735       175        21         1
 8         0      5040     13068     13132      6769      1960       322        28         1
 9         0     40320    109584    118124     67284     22449      4536       546        36         1
10         0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1
11         0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1
12         0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1

The maximum S1(100,k): 1971090874705526110...6000000000000000000 (158 digits)

```



## REXX

Some extra code was added to minimize the displaying of the column widths.

```rexx
/*REXX program to compute and display  (unsigned)  Stirling numbers   of the first kind.*/
parse arg lim .                                  /*obtain optional argument from the CL.*/
if lim=='' | lim==","  then lim= 12              /*Not specified?  Then use the default.*/
olim= lim                                        /*save     the original value of  LIM. */
lim= abs(lim)                                    /*only use the absolute value of  LIM. */
numeric digits max(9, 2*lim)                     /*(over) specify maximum number in grid*/
@.=;                                 @.0.0= 1    /*define the   (0, 0)th  value  in grid*/
        do n=0  for lim+1                        /* [↓]  initialize some  values  "   " */
        if n>0  then @.n.0 = 0                   /*assign value if  N > zero.           */
          do k=n+1  to lim
          @.n.k = 0                              /*zero some values in grid  if  K > N. */
          end   /*k*/
        end     /*n*/
max#.= 0                                         /* [↓]  calculate values for the grid. */
        do   n=1  for lim;           nm= n - 1
          do k=1  for lim;           km= k - 1
          @.n.k = @.nm.km  +  nm * @.nm.k        /*calculate an unsigned number in grid.*/
          max#.k= max(max#.k, @.n.k)             /*find the      maximum value   "   "  */
          max#.b= max(max#.b, @.n.k)             /*find the maximum value for all rows. */
          end   /*k*/
        end     /*n*/

        do k=1  for lim                          /*find max column width for each column*/
        max#.a= max#.a + length(max#.k)
        end   /*k*/
                                                 /* [↓]  only show the maximum value ?  */
w= length(max#.b)                                /*calculate max width of all numbers.  */
if olim<0  then do;  say 'The maximum value  (which has '      w      " decimal digits):"
                     say max#.b                  /*display maximum number in the grid.  */
                     exit                        /*stick a fork in it,  we're all done. */
                end
wi= max(3, length(lim+1) )                       /*the maximum width of the grid's index*/
say 'row'  center('columns', max(9, max#.a + lim), '═')    /*display header of the grid.*/

        do r=0  for lim+1;   $=                  /* [↓]  display the grid to the term.  */
          do c=0  for lim+1  until c>=r          /*build a row of grid, 1 col at a time.*/
          $= $  right(@.r.c, length(max#.c) )    /*append a column to a row of the grid.*/
          end   /*c*/
        say right(r,wi)  strip(substr($,2), 'T') /*display a single row of the grid.    */
        end     /*r*/                            /*stick a fork in it,  we're all done. */
```

```txt

row ════════════════════════════════════════columns═════════════════════════════════════════
  0 1
  1 0        1
  2 0        1         1
  3 0        2         3         1
  4 0        6        11         6         1
  5 0       24        50        35        10        1
  6 0      120       274       225        85       15        1
  7 0      720      1764      1624       735      175       21       1
  8 0     5040     13068     13132      6769     1960      322      28      1
  9 0    40320    109584    118124     67284    22449     4536     546     36     1
 10 0   362880   1026576   1172700    723680   269325    63273    9450    870    45    1
 11 0  3628800  10628640  12753576   8409500  3416930   902055  157773  18150  1320   55  1
 12 0 39916800 120543840 150917976 105258076 45995730 13339535 2637558 357423 32670 1925 66 1

```

```txt

The maximum value  (which has  158  decimal digits):
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000

```



## Sidef


```ruby
func S1(n, k) {     # unsigned Stirling numbers of the first kind
    stirling(n, k).abs
}

const r = (0..12)

var triangle = r.map {|n| 0..n -> map {|k| S1(n, k) } }
var widths   = r.map {|n| r.map {|k| (triangle[k][n] \\ 0).len }.max }

say ('n\k ', r.map {|n| "%*s" % (widths[n], n) }.join(' '))

r.each {|n|
    var str = ('%-3s ' % n)
    str += triangle[n].map_kv {|k,v| "%*s" % (widths[k], v) }.join(' ')
    say str
}

with (100) {|n|
    say "\nMaximum value from the S1(#{n}, *) row:"
    say { S1(n, _) }.map(^n).max
}
```

```txt

n\k 0        1         2         3         4        5        6       7      8     9   10 11 12
0   1
1   0        1
2   0        1         1
3   0        2         3         1
4   0        6        11         6         1
5   0       24        50        35        10        1
6   0      120       274       225        85       15        1
7   0      720      1764      1624       735      175       21       1
8   0     5040     13068     13132      6769     1960      322      28      1
9   0    40320    109584    118124     67284    22449     4536     546     36     1
10  0   362880   1026576   1172700    723680   269325    63273    9450    870    45    1
11  0  3628800  10628640  12753576   8409500  3416930   902055  157773  18150  1320   55  1
12  0 39916800 120543840 150917976 105258076 45995730 13339535 2637558 357423 32670 1925 66 1

Maximum value from the S1(100, *) row:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000

```


Alternatively, the '''S1(n,k)''' function can be defined as:

```ruby
func S1((0), (0)) { 1 }
func S1(_, (0))   { 0 }
func S1((0), _)   { 0 }
func S1(n, k) is cached { S1(n-1, k-1) + (n-1)*S1(n-1, k) }
```



## zkl

```zkl
fcn stirling1(n,k){
   var seen=Dictionary();	// cache for recursion
   if(n==k==0)      return(1);
   if(n==0 or k==0) return(0);
   
   z1,z2 := "%d,%d".fmt(n-1,k-1), "%d,%d".fmt(n-1,k);
   if(Void==(s1 := seen.find(z1))){ s1 = seen[z1] = stirling1(n - 1, k - 1) }
   if(Void==(s2 := seen.find(z2))){ s2 = seen[z2] = stirling1(n - 1, k)     }
   (n - 1)*s2 + s1;   // n is first to cast to BigInt (if using BigInts)
}
```


```zkl
// calculate entire table (cached), find max, find num digits in max
N,mx := 12, [1..N].apply(fcn(n){ [1..n].apply(stirling1.fp(n)) : (0).max(_) }) : (0).max(_);
fmt:="%%%dd".fmt("%d".fmt(mx.numDigits + 1)).fmt;  // "%9d".fmt
println("Unsigned Stirling numbers of the first kind: S1(n, k):");
println("n\\k",[0..N].pump(String,fmt));
foreach row in ([0..N]){
   println("%3d".fmt(row), [0..row].pump(String, stirling1.fp(row), fmt));
}
```

<pre style="font-size:83%">
Unsigned Stirling numbers of the first kind: S1(n, k):
n\k         0         1         2         3         4         5         6         7         8         9        10        11        12
  0         1
  1         0         1
  2         0         1         1
  3         0         2         3         1
  4         0         6        11         6         1
  5         0        24        50        35        10         1
  6         0       120       274       225        85        15         1
  7         0       720      1764      1624       735       175        21         1
  8         0      5040     13068     13132      6769      1960       322        28         1
  9         0     40320    109584    118124     67284     22449      4536       546        36         1
 10         0    362880   1026576   1172700    723680    269325     63273      9450       870        45         1
 11         0   3628800  10628640  12753576   8409500   3416930    902055    157773     18150      1320        55         1
 12         0  39916800 120543840 150917976 105258076  45995730  13339535   2637558    357423     32670      1925        66         1

```

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
N=100; 
println("Maximum value from the S1(%d, *) row:".fmt(N));
[1..N].apply(stirling1.fp(BI(N)))
  .reduce(fcn(m,n){ m.max(n) }).println();
```

<pre style="font-size:83%">
Maximum value from the S1(100, *) row:
19710908747055261109287881673376044669240511161402863823515728791076863288440277983854056472903481625299174865860036734731122707870406148096000000000000000000

```

