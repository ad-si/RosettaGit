+++
title = "Super-d numbers"
description = ""
date = 2019-10-12T20:49:17Z
aliases = []
[extra]
id = 22580
[taxonomies]
categories = ["task"]
tags = []
+++

A super-d number is a positive, decimal (base ten) integer '''n''' such that '''d × n^d''' has at least '''d''' consecutive digits '''d'''
where
    <big>2 ≤ d ≤ 9</big>
For instance, 753 is a super-3 number because 3 × 753^3 = 128087<u>333</u>1.


'''Super-d'''   numbers are also shown on   '''MathWorld'''&trade;   as   '''super-''d'' '''   or   '''super-<sup>''d''<sup>'''.


## Task

:* Write a function/procedure/routine to find super-d numbers.

:* For   '''d=2'''   through   '''d=6''',   use the routine to show the first   '''10'''   super-d numbers.


;Extra credit

:* Show the first   '''10'''   super-7, super-8, and/or super-9 numbers.   (Optional)


## See also

:* '''[http://mathworld.wolfram.com/Super-dNumber.html Wolfram MathWorld - Super-d Number]'''
:* '''[http://oeis.org/A014569 OEIS: A014569 - Super-3 Numbers]'''


=={{header|F_Sharp|F#}}==
;The Function

```fsharp

// Generate Super-N numbers. Nigel Galloway: October 12th., 2019
let superD N=
  let      I=bigint(pown 10 N)
  let      G=bigint N
  let      E=G*(111111111I%I)
  let rec fL n=match (E-n%I).IsZero with true->true |_->if (E*10I)<n then false else fL (n/10I)
  seq{1I..999999999999999999I}|>Seq.choose(fun n->if fL (G*n**N) then Some n else None)

```

;The Task

```fsharp

superD 2 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""
superD 3 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""
superD 4 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""
superD 5 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""
superD 6 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""
superD 7 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""
superD 8 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""
superD 9 |> Seq.take 10 |> Seq.iter(printf "%A "); printfn ""

```

```txt

19 31 69 81 105 106 107 119 127 131
261 462 471 481 558 753 1036 1046 1471 1645
1168 4972 7423 7752 8431 10267 11317 11487 11549 11680
4602 5517 7539 12955 14555 20137 20379 26629 32767 35689
27257 272570 302693 323576 364509 502785 513675 537771 676657 678146
140997 490996 1184321 1259609 1409970 1783166 1886654 1977538 2457756 2714763
185423 641519 1551728 1854230 6415190 12043464 12147605 15517280 16561735 18542300
17546133 32613656 93568867 107225764 109255734 113315082 121251742 175461330 180917907 182557181

```


## Factor


```factor
USING: arrays formatting io kernel lists lists.lazy math
math.functions math.ranges math.text.utils prettyprint sequences
;
IN: rosetta-code.super-d

: super-d? ( seq n d -- ? ) tuck ^ * 1 digit-groups subseq? ;

: super-d ( d -- list )
    [ dup <array> ] [ drop 1 lfrom ] [ ] tri [ super-d? ] curry
    with lfilter ;

: super-d-demo ( -- )
    10 2 6 [a,b] [
        dup "First 10 super-%d numbers:\n" printf
        super-d ltake list>array [ pprint bl ] each nl nl
    ] with each ;

MAIN: super-d-demo
```

```txt

First 10 super-2 numbers:
19 31 69 81 105 106 107 119 127 131 

First 10 super-3 numbers:
261 462 471 481 558 753 1036 1046 1471 1645 

First 10 super-4 numbers:
1168 4972 7423 7752 8431 10267 11317 11487 11549 11680 

First 10 super-5 numbers:
4602 5517 7539 12955 14555 20137 20379 26629 32767 35689 

First 10 super-6 numbers:
27257 272570 302693 323576 364509 502785 513675 537771 676657 678146 

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Super-d_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

Simple brute force approach and so not particularly quick - about 2.25 minutes on a Core i7.

```go
package main

import (
    "fmt"
    "math/big"
    "strings"
    "time"
)

func main() {
    start := time.Now()
    rd := []string{"22", "333", "4444", "55555", "666666", "7777777", "88888888", "999999999"}
    one := big.NewInt(1)
    nine := big.NewInt(9)
    for i := big.NewInt(2); i.Cmp(nine) <= 0; i.Add(i, one) {
        fmt.Printf("First 10 super-%d numbers:\n", i)
        ii := i.Uint64()
        k := new(big.Int)
        count := 0
    inner:
        for j := big.NewInt(3); ; j.Add(j, one) {
            k.Exp(j, i, nil)
            k.Mul(i, k)
            ix := strings.Index(k.String(), rd[ii-2])
            if ix >= 0 {
                count++
                fmt.Printf("%d ", j)
                if count == 10 {
                    fmt.Printf("\nfound in %d ms\n\n", time.Since(start).Milliseconds())
                    break inner
                }
            }
        }
    }
}
```


```txt

First 10 super-2 numbers:
19 31 69 81 105 106 107 119 127 131 
found in 0 ms

First 10 super-3 numbers:
261 462 471 481 558 753 1036 1046 1471 1645 
found in 1 ms

First 10 super-4 numbers:
1168 4972 7423 7752 8431 10267 11317 11487 11549 11680 
found in 7 ms

First 10 super-5 numbers:
4602 5517 7539 12955 14555 20137 20379 26629 32767 35689 
found in 28 ms

First 10 super-6 numbers:
27257 272570 302693 323576 364509 502785 513675 537771 676657 678146 
found in 285 ms

First 10 super-7 numbers:
140997 490996 1184321 1259609 1409970 1783166 1886654 1977538 2457756 2714763 
found in 1517 ms

First 10 super-8 numbers:
185423 641519 1551728 1854230 6415190 12043464 12147605 15517280 16561735 18542300 
found in 11117 ms

First 10 super-9 numbers:
17546133 32613656 93568867 107225764 109255734 113315082 121251742 175461330 180917907 182557181 
found in 135616 ms

```



## Perl


```perl
use strict;
use warnings;
use bigint;
use feature 'say';

sub super {
    my $d = shift;
    my $run = $d x $d;
    my @super;
    my $i = 0;
    my $n = 0;
    while ( $i < 10 ) {
        if (index($n ** $d * $d, $run) > -1) {
            push @super, $n;
            ++$i;
        }
        ++$n;
    }
    @super;
}
 
say "\nFirst 10 super-$_ numbers:\n", join ' ', super($_) for 2..6;
```



```txt

First 10 super-2 numbers:
19 31 69 81 105 106 107 119 127 131

First 10 super-3 numbers:
261 462 471 481 558 753 1036 1046 1471 1645

First 10 super-4 numbers:
1168 4972 7423 7752 8431 10267 11317 11487 11549 11680

First 10 super-5 numbers:
4602 5517 7539 12955 14555 20137 20379 26629 32767 35689

First 10 super-6 numbers:
27257 272570 302693 323576 364509 502785 513675 537771 676657 678146
```



## Perl 6

===Single-threaded===
2 - 6 takes a few seconds, 7 & 8 take a few minutes; I got tired of waiting for 9.


```perl6
sub super ($d) {
    my $run = $d x $d;
    "First 10 super-{$d} numbers:\n{ (^∞ .grep: ($d * * ** $d).Str.contains($run) )[^10]}\n"
}

put .&super for 2 .. 8
```



```txt
First 10 super-2 numbers:
19 31 69 81 105 106 107 119 127 131

First 10 super-3 numbers:
261 462 471 481 558 753 1036 1046 1471 1645

First 10 super-4 numbers:
1168 4972 7423 7752 8431 10267 11317 11487 11549 11680

First 10 super-5 numbers:
4602 5517 7539 12955 14555 20137 20379 26629 32767 35689

First 10 super-6 numbers:
27257 272570 302693 323576 364509 502785 513675 537771 676657 678146

First 10 super-7 numbers:
140997 490996 1184321 1259609 1409970 1783166 1886654 1977538 2457756 2714763

First 10 super-8 numbers:
185423 641519 1551728 1854230 6415190 12043464 12147605 15517280 16561735 18542300
```


### Concurrent

Since waiting can be tiresome, make things faster with the <code>race</code> concurrency function. The only problem is that we're not sure what the stopping point is ahead of time, so when it seems safe to quit (calculate a few more result than strictly needed, since the parallel workers don't coordinate), break out of the concurrent block by raising an exception. The usual concurrency caveats apply: <code>@super</code> must be sized ahead of time, and only modified with an atomic operation (⚛++).

Concurrency makes little difference for d < 6, but the benefits accrue rapidly after that (greater than 10-fold speed-up for d = 8, with an 8-core CPU). However, in the end, you'll still have to wait a quite bit or the super-9 values.

```perl6
sub super-d ($d,$max) {
    my $max-plus = $max + floor 2*$max/$d;
    my @super[2*$max-plus];
    {
        my $digits = $d x $d;
        my $chunk  = 250 * $d;
        my atomicint $found = 0;
        (0, 1*$chunk, 2*$chunk ... *).race(:1batch).map: -> $i {
             @super[$found⚛++] = $_ if ($_ ** $d  *  $d).contains($digits) for 1+$i .. 1+$i+$chunk;
             X::AdHoc.new.throw if $found ≥ $max-plus;
        }
        CATCH { when X::AdHoc { return @super.grep(so *).sort.head($max) } }
    }
}

say "$_: " ~ join ' ', super-d($_,10) for 2..9;
```

```txt
2: 19 31 69 81 105 106 107 119 127 131
3: 261 462 471 481 558 753 1036 1046 1471 1645
4: 1168 4972 7423 7752 8431 10267 11317 11487 11549 11680
5: 4602 5517 7539 12955 14555 20137 20379 26629 32767 35689
6: 27257 272570 302693 323576 364509 502785 513675 537771 676657 678146
7: 140997 490996 1184321 1259609 1409970 1783166 1886654 1977538 2457756 2714763
8: 185423 641519 1551728 1854230 6415190 12043464 12147605 15517280 16561735 18542300
9: 17546133 32613656 93568867 107225764 109255734 113315082 121251742 175461330 180917907 182557181
```



## REXX


```rexx
/*REXX program computes and displays the first  N  super-d  numbers for D from LO to HI.*/
numeric digits 100                               /*ensure enough decimal digs for calc. */
parse arg n LO HI .                              /*obtain optional arguments from the CL*/
if  n=='' |  n==","  then  n= 10                 /*the number of super-d numbers to calc*/
if LO=='' | LO==","  then LO=  2                 /*low  end of  D  for the super-d nums.*/
if HI=='' | HI==","  then HI=  9                 /*high  "   "  "   "   "     "      "  */

     do d=LO  for HI-1                           /*process the  D  from  LO  through  HI*/
     #= 0                                        /*count of the super-d numbers (so far)*/
     $=                                          /* list  "  "     "       "      "  "  */
     z= copies(d, d)                             /*the string that is being searched for*/
         do j=2  until #==n                      /*search for super-d numbers 'til found*/
         if pos(z, d * j**d)==0  then iterate    /*does product have the required reps? */
         #= # + 1;        $= $ j                 /*bump counter;  add the number to list*/
         end   /*j*/
     say
     say center(' the first '     n     " super-"d 'numbers ',  digits(),  "═")
     say $
     end   /*d*/                                 /*stick a fork in it,  we're all done. */
```

```txt

══════════════════════════════════ the first  10  super-2 numbers ══════════════════════════════════
 19 31 69 81 105 106 107 119 127 131

══════════════════════════════════ the first  10  super-3 numbers ══════════════════════════════════
 261 462 471 481 558 753 1036 1046 1471 1645

══════════════════════════════════ the first  10  super-4 numbers ══════════════════════════════════
 1168 4972 7423 7752 8431 10267 11317 11487 11549 11680

══════════════════════════════════ the first  10  super-5 numbers ══════════════════════════════════
 4602 5517 7539 12955 14555 20137 20379 26629 32767 35689

══════════════════════════════════ the first  10  super-6 numbers ══════════════════════════════════
 27257 272570 302693 323576 364509 502785 513675 537771 676657 678146

══════════════════════════════════ the first  10  super-7 numbers ══════════════════════════════════
 140997 490996 1184321 1259609 1409970 1783166 1886654 1977538 2457756 2714763

══════════════════════════════════ the first  10  super-8 numbers ══════════════════════════════════
 185423 641519 1551728 1854230 6415190 12043464 12147605 15517280 16561735 18542300

══════════════════════════════════ the first  10  super-9 numbers ══════════════════════════════════
 17546133 32613656 93568867 107225764 109255734 113315082 121251742 175461330 180917907 182557181

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library

```zkl
var [const] BI=Import("zklBigNum");  // libGMP

fcn superDW(d){
   digits:=d.toString()*d;
   [2..].tweak('wrap(n)
      { BI(n).pow(d).mul(d).toString().holds(digits) and n or Void.Skip });
}
foreach d in ([2..8]){ println(d," : ",superDW(d).walk(10).concat(" ")) }
```

```txt

2 : 19 31 69 81 105 106 107 119 127 131
3 : 261 462 471 481 558 753 1036 1046 1471 1645
4 : 1168 4972 7423 7752 8431 10267 11317 11487 11549 11680
5 : 4602 5517 7539 12955 14555 20137 20379 26629 32767 35689
6 : 27257 272570 302693 323576 364509 502785 513675 537771 676657 678146
7 : 140997 490996 1184321 1259609 1409970 1783166 1886654 1977538 2457756 2714763
8 : 185423 641519 1551728 1854230 6415190 12043464 12147605 15517280 16561735 18542300

```

