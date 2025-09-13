+++
title = "Factorions"
description = ""
date = 2019-09-10T09:12:35Z
aliases = []
[extra]
id = 22459
[taxonomies]
categories = ["task"]
tags = []
+++

;Definition:
A factorion is a natural number that equals the sum of the factorials of its digits.


;Example:
'''145'''   is a factorion in base '''10''' because:
<b>
          <big> 1! + 4! + 5!   =   1 + 24 + 120   =   145 </big>
</b>


It can be shown (see the Wikipedia article below) that no factorion in base '''10''' can exceed   '''1,499,999'''.


## Task

Write a program in your language to demonstrate, by calculating and printing out the factorions, that shows:
:*   There are   '''3'''   factorions in base   '''9'''
:*   There are   '''4'''   factorions in base '''10'''
:*   There are   '''5'''   factorions in base '''11'''
:*   There are   '''2'''   factorions in base '''12'''     (up to the same upper bound as for base '''10''')


## See also

:* '''[[wp:Factorion|Wikipedia article]]'''
:* '''[[OEIS:A014080|OEIS:A014080 - Factorions in base 10]]'''
:* '''[[OEIS:A193163|OEIS:A193163 - Factorions in base n]]'''





## ALGOL 68

```algol68
BEGIN
    # cache factorials from 0 to 11 #
    [ 0 : 11 ]INT fact;
    fact[0] := 1;
    FOR n TO 11 DO
        fact[n] := fact[n-1] * n
    OD;
    FOR b FROM 9 TO 12 DO
        print( ( "The factorions for base ", whole( b, 0 ), " are:", newline ) );
        FOR i TO 1500000 - 1 DO
            INT sum := 0;
            INT j := i;
            WHILE j > 0 DO
                sum +:= fact[ j MOD b ];
                j OVERAB b
            OD;
            IF sum = i THEN print( ( whole( i, 0 ), " " ) ) FI
        OD;
        print( ( newline ) )
    OD
END
```

```txt

The factorions for base 9 are:
1 2 41282
The factorions for base 10 are:
1 2 145 40585
The factorions for base 11 are:
1 2 26 48 40472
The factorions for base 12 are:
1 2
```


## AWK


```AWK

# syntax: GAWK -f FACTORIONS.AWK
# converted from C
BEGIN {
    fact[0] = 1 # cache factorials from 0 to 11
    for (n=1; n<12; ++n) {
      fact[n] = fact[n-1] * n
    }
    for (b=9; b<=12; ++b) {
      printf("base %d factorions:",b)
      for (i=1; i<1500000; ++i) {
        sum = 0
        j = i
        while (j > 0) {
          d = j % b
          sum += fact[d]
          j = int(j/b)
        }
        if (sum == i) {
          printf(" %d",i)
        }
      }
      printf("\n")
    }
    exit(0)
}

```

```txt

base 9 factorions: 1 2 41282
base 10 factorions: 1 2 145 40585
base 11 factorions: 1 2 26 48 40472
base 12 factorions: 1 2

```



## C

```c
#include <stdio.h>

int main() {
    int n, b, d;
    unsigned long long i, j, sum, fact[12];
    // cache factorials from 0 to 11
    fact[0] = 1;
    for (n = 1; n < 12; ++n) {
        fact[n] = fact[n-1] * n;
    }

    for (b = 9; b <= 12; ++b) {
        printf("The factorions for base %d are:\n", b);
        for (i = 1; i < 1500000; ++i) {
            sum = 0;
            j = i;
            while (j > 0) {
                d = j % b;
                sum += fact[d];
                j /= b;
            }
            if (sum == i) printf("%llu ", i);
        }
        printf("\n\n");
    }
    return 0;
}
```


```txt

The factorions for base 9 are:
1 2 41282

The factorions for base 10 are:
1 2 145 40585

The factorions for base 11 are:
1 2 26 48 40472

The factorions for base 12 are:
1 2

```



## Factor


```factor
USING: formatting io kernel math math.parser math.ranges memoize
prettyprint sequences ;
IN: rosetta-code.factorions

! Memoize factorial function
MEMO: factorial ( n -- n! ) [ 1 ] [ [1,b] product ] if-zero ;

: factorion? ( n base -- ? )
    dupd >base string>digits [ factorial ] map-sum = ;

: show-factorions ( limit base -- )
    dup "The factorions for base %d are:\n" printf
    [ [1,b) ] dip [ dupd factorion? [ pprint bl ] [ drop ] if ]
    curry each nl ;

1,500,000 9 12 [a,b] [ show-factorions nl ] with each
```

```txt

The factorions for base 9 are:
1 2 41282

The factorions for base 10 are:
1 2 145 40585

The factorions for base 11 are:
1 2 26 48 40472

The factorions for base 12 are:
1 2

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Factorions this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go


```go
package main

import (
    "fmt"
    "strconv"
)

func main() {
    // cache factorials from 0 to 11
    var fact [12]uint64
    fact[0] = 1
    for n := uint64(1); n < 12; n++ {
        fact[n] = fact[n-1] * n
    }

    for b := 9; b <= 12; b++ {
        fmt.Printf("The factorions for base %d are:\n", b)
        for i := uint64(1); i < 1500000; i++ {
            digits := strconv.FormatUint(i, b)
            sum := uint64(0)
            for _, digit := range digits {
                if digit < 'a' {
                    sum += fact[digit-'0']
                } else {
                    sum += fact[digit+10-'a']
                }
            }
            if sum == i {
                fmt.Printf("%d ", i)
            }
        }
        fmt.Println("\n")
    }
}
```


```txt

The factorions for base 9 are:
1 2 41282

The factorions for base 10 are:
1 2 145 40585

The factorions for base 11 are:
1 2 26 48 40472

The factorions for base 12 are:
1 2

```



## Julia


```julia
isfactorian(n, base) = mapreduce(factorial, +, map(c -> parse(Int, c, base=16), split(string(n, base=base), ""))) == n

printallfactorian(base) = println("Factorians for base $base: ", [n for n in 1:100000 if isfactorian(n, base)])

foreach(printallfactorian, 9:12)

```
```txt

Factorians for base 9: [1, 2, 41282]
Factorians for base 10: [1, 2, 145, 40585]
Factorians for base 11: [1, 2, 26, 48, 40472]
Factorians for base 12: [1, 2]

```



## OCaml

```ocaml
let () =
  (* cache factorials from 0 to 11 *)
  let fact = Array.make 12 0 in
  fact.(0) <- 1;
  for n = 1 to pred 12 do
    fact.(n) <- fact.(n-1) * n;
  done;

  for b = 9 to 12 do
    Printf.printf "The factorions for base %d are:\n" b;
    for i = 1 to pred 1_500_000 do
      let sum = ref 0 in
      let j = ref i in
      while !j > 0 do
        let d = !j mod b in
        sum := !sum + fact.(d);
        j := !j / b;
      done;
      if !sum = i then (print_int i; print_string " ")
    done;
    print_string "\n\n";
  done
```



## Perl

```perl
use strict;
use warnings;
use ntheory qw/factorial todigits/;

my $limit = 1500000;

for my $b (9 .. 12) {
    print "Factorions in base $b:\n";
    $_ == factorial($_) and print "$_ " for 0..$b-1;

    for my $i (1 .. int $limit/$b) {
        my $sum;
        my $prod = $i * $b;

        for (reverse todigits($i, $b)) {
            $sum += factorial($_);
            $sum = 0 && last if $sum > $prod;
        }

        next if $sum == 0;
        ($sum + factorial($_) == $prod + $_) and print $prod+$_ . ' ' for 0..$b-1;
    }
    print "\n\n";
}
```

```txt
Factorions in base 9:
1 2 41282

Factorions in base 10:
1 2 145 40585

Factorions in base 11:
1 2 26 48 40472

Factorions in base 12:
1 2
```


Alternatively, a more efficient approach:
```perl
use 5.020;
use ntheory qw(:all);
use experimental qw(signatures);
use Algorithm::Combinatorics qw(combinations_with_repetition);

sub max_power ($base = 10) {
    my $m = 1;
    my $f = factorial($base - 1);
    while ($m * $f >= $base**($m-1)) {
        $m += 1;
    }
    return $m-1;
}

sub factorions ($base = 10) {

    my @result;
    my @digits    = (0 .. $base-1);
    my @factorial = map { factorial($_) } @digits;

    foreach my $k (1 .. max_power($base)) {
        my $iter = combinations_with_repetition(\@digits, $k);
        while (my $comb = $iter->next) {
            my $n = vecsum(map { $factorial[$_] } @$comb);
            if (join(' ', sort { $a <=> $b } todigits($n, $base)) eq join(' ', @$comb)) {
                push @result, $n;
            }
        }
    }

    return @result;
}

foreach my $base (2 .. 14) {
    my @r = factorions($base);
    say "Factorions in base $base are (@r)";
}
```

```txt

Factorions in base 2 are (1 2)
Factorions in base 3 are (1 2)
Factorions in base 4 are (1 2 7)
Factorions in base 5 are (1 2 49)
Factorions in base 6 are (1 2 25 26)
Factorions in base 7 are (1 2)
Factorions in base 8 are (1 2)
Factorions in base 9 are (1 2 41282)
Factorions in base 10 are (1 2 145 40585)
Factorions in base 11 are (1 2 26 48 40472)
Factorions in base 12 are (1 2)
Factorions in base 13 are (1 2 519326767)
Factorions in base 14 are (1 2 12973363226)

```



## Perl 6

```perl6
constant @factorial = 1, |[\*] 1..*;

constant $limit = 1500000;

constant $bases = 9 .. 12;

my @result;

$bases.race(:1batch).map: -> $base {

    @result[$base] = "\nFactorions in base $base:\n1 2";

    sink (1 .. $limit div $base).map: -> $i {
        my $product = $i * $base;
        my $partial;

        for $i.polymod($base xx *) {
            $partial += @factorial[$_];
            last if $partial > $product
        }

        next if $partial > $product;

        my $sum;

        for ^$base {
            last if ($sum = $partial + @factorial[$_]) > $product + $_;
            @result[$base] ~= " $sum" and last if $sum == $product + $_
        }
    }
}

.say for @result[$bases];
```

```txt
Factorions in base 9:
1 2 41282

Factorions in base 10:
1 2 145 40585

Factorions in base 11:
1 2 26 48 40472

Factorions in base 12:
1 2
```



## Phix

```Phix
-- cache factorials from 0 to 11
sequence fact = repeat(1,12)
for n=2 to length(fact) do
    fact[n] = fact[n-1]*(n-1)
end for

for b=9 to 12 do
    printf(1,"The factorions for base %d are:\n", b)
    for i=1 to 1499999 do
        atom total = 0, j = i, d
        while j>0 and total<=i do
            d = remainder(j,b)
            total += fact[d+1]
            j = floor(j/b)
        end while
        if total==i then printf(1,"%d ", i) end if
    end for
    printf(1,"\n\n")
end for
```

```txt

The factorions for base 9 are:
1 2 41282

The factorions for base 10 are:
1 2 145 40585

The factorions for base 11 are:
1 2 26 48 40472

The factorions for base 12 are:
1 2

```



## Racket


```racket
#lang racket

(define fact
  (curry list-ref (for/fold ([result (list 1)] #:result (reverse result))
                            ([x (in-range 1 20)])
                    (cons (* x (first result)) result))))

(for ([b (in-range 9 13)])
  (printf "The factorions for base ~a are:\n" b)
  (for ([i (in-range 1 1500000)])
    (let loop ([sum 0] [n i])
      (cond
        [(positive? n) (loop (+ sum (fact (modulo n b))) (quotient n b))]
        [(= sum i) (printf "~a " i)])))
  (newline))
```


```txt

The factorions for base 9 are:
1 2 41282
The factorions for base 10 are:
1 2 145 40585
The factorions for base 11 are:
1 2 26 48 40472
The factorions for base 12 are:
1 2

```



## REXX

```rexx
/*REXX program calculates and displays   factorions   in  bases  nine ───► twelve.      */
parse arg LOb HIb lim .                          /*obtain optional arguments from the CL*/
if LOb=='' | LOb==","  then LOb=       9         /*Not specified?  Then use the default.*/
if HIb=='' | HIb==","  then HIb=      12         /* "      "         "   "   "      "   */
if lim=='' | lim==","  then lim= 1500000  -  1   /* "      "         "   "   "      "   */

  do fact=0  to HIb;   !.fact= !(fact)           /*use memoization for factorials.      */
  end   /*fact*/

  do base=LOb  to  HIb                           /*process all the required bases.      */
  @= 1 2                                         /*initialize the list  (@)  to null.   */
          do j=3  for lim-2;  $= 0               /*initialize the sum   ($)  to zero.   */
                                          t= j   /*define the target  (for the sum !'s).*/
                                 do until t==0;   d= t // base      /*obtain a "digit". */
                                                  $= $ + !.d        /*add  !(d) to sum. */
                                                  t= t % base       /*get a new target. */
                                 end   /*until*/
          if $==j  then @= @ j                   /*Good factorial sum? Then add to list.*/
          end   /*i*/
  say
  say 'The factorions for base '      right( base, length(HIb) )        " are: "         @
  end   /*base*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!: procedure; arg x;  !=1;    do j=2  to x;  !=!*j;  end;   return !  /*calc factorials.*/
```

```txt

The factorions for base   9  are:  1 2 41282

The factorions for base  10  are:  1 2 145 40585

The factorions for base  11  are:  1 2 26 48 40472

The factorions for base  12  are:  1 2

```



## Sidef


```ruby
func max_power(b = 10) {
    var m = 1
    var f = (b-1)!
    while (m*f >= b**(m-1)) {
        m += 1
    }
    return m-1
}

func factorions(b = 10) {

    var result = []
    var digits = @^b
    var fact = digits.map { _! }

    for k in (1 .. max_power(b)) {
        digits.combinations_with_repetition(k, {|*comb|
            var n = comb.sum_by { fact[_] }
            if (n.digits(b).sort == comb) {
                result << n
            }
        })
    }

    return result
}

for b in (2..12) {
    var r = factorions(b)
    say "Base #{'%2d' % b} factorions: #{r}"
}
```

```txt

Base  2 factorions: [1, 2]
Base  3 factorions: [1, 2]
Base  4 factorions: [1, 2, 7]
Base  5 factorions: [1, 2, 49]
Base  6 factorions: [1, 2, 25, 26]
Base  7 factorions: [1, 2]
Base  8 factorions: [1, 2]
Base  9 factorions: [1, 2, 41282]
Base 10 factorions: [1, 2, 145, 40585]
Base 11 factorions: [1, 2, 26, 48, 40472]
Base 12 factorions: [1, 2]

```



## zkl

```zkl
var facts=[0..12].pump(List,fcn(n){ (1).reduce(n,fcn(N,n){ N*n },1) }); #(1,1,2,6....)
fcn factorions(base){
   fs:=List();
   foreach n in ([1..1_499_999]){
      sum,j := 0,n;
      while(j){
	 sum+=facts[j%base];
	 j/=base;
      }
      if(sum==n) fs.append(n);
   }
   fs
}
```


```zkl
foreach n in ([9..12]){
   println("The factorions for base %2d are: ".fmt(n),factorions(n).concat("  "));
}
```

```txt

The factorions for base  9 are: 1  2  41282
The factorions for base 10 are: 1  2  145  40585
The factorions for base 11 are: 1  2  26  48  40472
The factorions for base 12 are: 1  2

```

