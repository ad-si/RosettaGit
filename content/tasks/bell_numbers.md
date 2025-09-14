+++
title = "Bell numbers"
description = ""
date = 2019-10-01T23:59:06Z
aliases = []
[extra]
id = 22417
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "csharp",
  "d",
  "fsharp",
  "factor",
  "go",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "rexx",
  "sidef",
  "visual_basic_net",
  "zkl",
]
+++

## Task

[[wp:Bell number|Bell or exponential numbers]] are enumerations of the number of different ways to partition a set that has exactly '''n''' elements. Each element of the sequence '''B<sub>n</sub>''' is the number of partitions of a set of size '''n''' where order of the elements and order of the partitions are non-significant. E.G.: '''{a b}''' is the same as '''{b a}''' and '''{a} {b}''' is the same as '''{b} {a}'''.


;So:

:'''B<sub>0</sub> = 1''' trivially. There is only one way to partition a set with zero elements. '''{ }'''

:'''B<sub>1</sub> = 1''' There is only one way to partition a set with one element. '''{a}'''

:'''B<sub>2</sub> = 2''' Two elements may be partitioned in two ways. '''{a} {b}, {a b}'''

:'''B<sub>3</sub> = 5''' Three elements may be partitioned in five ways '''{a} {b} {c}, {a b} {c}, {a} {b c}, {a c} {b}, {a b c}'''

: and so on.


A simple way to find the Bell numbers is construct a '''[[wp:Bell_triangle|Bell triangle]]''', also known as an '''Aitken's array''' or '''Peirce triangle''', and read off the numbers in the first column of each row. There are other generating algorithms though, and you are free to choose the best / most appropriate for your case.


;Task:

Write a routine (function, generator, whatever) to generate the Bell number sequence and call the routine to show here, on this page at least the '''first 15''' and (if your language supports big Integers) '''50th''' elements of the sequence.

If you ''do'' use the Bell triangle method to generate the numbers, also show the '''first ten rows''' of the Bell triangle.


;See also:

:* '''[[oeis:A000110|OEIS:A000110 Bell or exponential numbers]]'''
:* '''[[oeis:A011971|OEIS:A011971 Aitken's array]]'''




## C

{{trans|D}}

```c
#include <stdio.h>
#include <stdlib.h>

// row starts with 1; col < row
size_t bellIndex(int row, int col) {
    return row * (row - 1) / 2 + col;
}

int getBell(int *bellTri, int row, int col) {
    size_t index = bellIndex(row, col);
    return bellTri[index];
}

void setBell(int *bellTri, int row, int col, int value) {
    size_t index = bellIndex(row, col);
    bellTri[index] = value;
}

int *bellTriangle(int n) {
    size_t length = n * (n + 1) / 2;
    int *tri = calloc(length, sizeof(int));
    int i, j;

    setBell(tri, 1, 0, 1);
    for (i = 2; i <= n; ++i) {
        setBell(tri, i, 0, getBell(tri, i - 1, i - 2));
        for (j = 1; j < i; ++j) {
            int value = getBell(tri, i, j - 1) + getBell(tri, i - 1, j - 1);
            setBell(tri, i, j, value);
        }
    }

    return tri;
}

int main() {
    const int rows = 15;
    int *bt = bellTriangle(rows);
    int i, j;

    printf("First fifteen Bell numbers:\n");
    for (i = 1; i <= rows; ++i) {
        printf("%2d: %d\n", i, getBell(bt, i, 0));
    }

    printf("\nThe first ten rows of Bell's triangle:\n");
    for (i = 1; i <= 10; ++i) {
        printf("%d", getBell(bt, i, 0));
        for (j = 1; j < i; ++j) {
            printf(", %d", getBell(bt, i, j));
        }
        printf("\n");
    }

    free(bt);
    return 0;
}
```


{{out}}

```txt
First fifteen Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322

The first ten rows of Bell's triangle:
1
1, 2
2, 3, 5
5, 7, 10, 15
15, 20, 27, 37, 52
52, 67, 87, 114, 151, 203
203, 255, 322, 409, 523, 674, 877
877, 1080, 1335, 1657, 2066, 2589, 3263, 4140
4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147
21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975
```


=={{header|C#|C_sharp}}==
{{trans|D}}

```c#
using System;
using System.Numerics;

namespace BellNumbers {
    public static class Utility {
        public static void Init<T>(this T[] array, T value) {
            if (null == array) return;
            for (int i = 0; i < array.Length; ++i) {
                array[i] = value;
            }
        }
    }

    class Program {
        static BigInteger[][] BellTriangle(int n) {
            BigInteger[][] tri = new BigInteger[n][];
            for (int i = 0; i < n; ++i) {
                tri[i] = new BigInteger[i];
                tri[i].Init(BigInteger.Zero);
            }
            tri[1][0] = 1;
            for (int i = 2; i < n; ++i) {
                tri[i][0] = tri[i - 1][i - 2];
                for (int j = 1; j < i; ++j) {
                    tri[i][j] = tri[i][j - 1] + tri[i - 1][j - 1];
                }
            }
            return tri;
        }

        static void Main(string[] args) {
            var bt = BellTriangle(51);
            Console.WriteLine("First fifteen and fiftieth Bell numbers:");
            for (int i = 1; i < 16; ++i) {
                Console.WriteLine("{0,2}: {1}", i, bt[i][0]);
            }
            Console.WriteLine("50: {0}", bt[50][0]);
            Console.WriteLine();
            Console.WriteLine("The first ten rows of Bell's triangle:");
            for (int i = 1; i < 11; ++i) {
                //Console.WriteLine(bt[i]);
                var it = bt[i].GetEnumerator();
                Console.Write("[");
                if (it.MoveNext()) {
                    Console.Write(it.Current);
                }
                while (it.MoveNext()) {
                    Console.Write(", ");
                    Console.Write(it.Current);
                }
                Console.WriteLine("]");
            }
        }
    }
}
```

{{out}}

```txt
First fifteen and fiftieth Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
50: 10726137154573358400342215518590002633917247281

The first ten rows of Bell's triangle:
[1]
[1, 2]
[2, 3, 5]
[5, 7, 10, 15]
[15, 20, 27, 37, 52]
[52, 67, 87, 114, 151, 203]
[203, 255, 322, 409, 523, 674, 877]
[877, 1080, 1335, 1657, 2066, 2589, 3263, 4140]
[4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147]
[21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975]
```



## D

{{trans|Go}}

```d
import std.array : uninitializedArray;
import std.bigint;
import std.stdio : writeln, writefln;

auto bellTriangle(int n) {
    auto tri = uninitializedArray!(BigInt[][])(n);
    foreach (i; 0..n) {
        tri[i] = uninitializedArray!(BigInt[])(i);
        tri[i][] = BigInt(0);
    }
    tri[1][0] = 1;
    foreach (i; 2..n) {
        tri[i][0] = tri[i - 1][i - 2];
        foreach (j; 1..i) {
            tri[i][j] = tri[i][j - 1] + tri[i - 1][j - 1];
        }
    }
    return tri;
}

void main() {
    auto bt = bellTriangle(51);
    writeln("First fifteen and fiftieth Bell numbers:");
    foreach (i; 1..16) {
        writefln("%2d: %d", i, bt[i][0]);
    }
    writeln("50: ", bt[50][0]);
    writeln;
    writeln("The first ten rows of Bell's triangle:");
    foreach (i; 1..11) {
        writeln(bt[i]);
    }
}
```

{{out}}

```txt
First fifteen and fiftieth Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
50: 10726137154573358400342215518590002633917247281

The first ten rows of Bell's triangle:
[1]
[1, 2]
[2, 3, 5]
[5, 7, 10, 15]
[15, 20, 27, 37, 52]
[52, 67, 87, 114, 151, 203]
[203, 255, 322, 409, 523, 674, 877]
[877, 1080, 1335, 1657, 2066, 2589, 3263, 4140]
[4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147]
[21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975]
```


=={{header|F_Sharp|F#}}==

### The function


```fsharp

// Generate bell triangle. Nigel Galloway: July 6th., 2019
let bell=Seq.unfold(fun g->Some(g,List.scan(+) (List.last g) g))[1I]

```


### The Task


```fsharp

bell|>Seq.take 10|>Seq.iter(printfn "%A")

```

{{out}}

```txt

[1]
[1; 2]
[2; 3; 5]
[5; 7; 10; 15]
[15; 20; 27; 37; 52]
[52; 67; 87; 114; 151; 203]
[203; 255; 322; 409; 523; 674; 877]
[877; 1080; 1335; 1657; 2066; 2589; 3263; 4140]
[4140; 5017; 6097; 7432; 9089; 11155; 13744; 17007; 21147]
[21147; 25287; 30304; 36401; 43833; 52922; 64077; 77821; 94828; 115975]

```


```fsharp

bell|>Seq.take 15|>Seq.iter(fun n->printf "%A " (List.head n));printfn ""

```

{{out}}

```txt

1 1 2 5 15 52 203 877 4140 21147 115975 678570 4213597 27644437 190899322

```


```fsharp

printfn "%A" (Seq.head (Seq.item 49 bell))

```

{{out}}

```txt

10726137154573358400342215518590002633917247281

```



## Factor

===via Aitken's array===
{{works with|Factor|0.98}}

```factor
USING: formatting io kernel math math.matrices sequences vectors ;

: next-row ( prev -- next )
    [ 1 1vector ]
    [ dup last [ + ] accumulate swap suffix! ] if-empty ;

: aitken ( n -- seq )
    V{ } clone swap [ next-row dup ] replicate nip ;

0 50 aitken col [ 15 head ] [ last ] bi
"First 15 Bell numbers:\n%[%d, %]\n\n50th: %d\n\n" printf
"First 10 rows of the Bell triangle:" print
10 aitken [ "%[%d, %]\n" printf ] each
```

{{out}}

```txt

First 15 Bell numbers:
{ 1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 27644437, 190899322 }

50th: 10726137154573358400342215518590002633917247281

First 10 rows of the Bell triangle:
{ 1 }
{ 1, 2 }
{ 2, 3, 5 }
{ 5, 7, 10, 15 }
{ 15, 20, 27, 37, 52 }
{ 52, 67, 87, 114, 151, 203 }
{ 203, 255, 322, 409, 523, 674, 877 }
{ 877, 1080, 1335, 1657, 2066, 2589, 3263, 4140 }
{ 4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147 }
{ 21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975 }

```


### via recurrence relation

This solution makes use of a [https://en.wikipedia.org/wiki/Bell_number#Summation_formulas recurrence relation] involving binomial coefficients.
{{works with|Factor|0.98}}

```factor
USING: formatting kernel math math.combinatorics sequences ;

: next-bell ( seq -- n )
    dup length 1 - [ swap nCk * ] curry map-index sum ;

: bells ( n -- seq )
    V{ 1 } clone swap 1 - [ dup next-bell suffix! ] times ;

50 bells [ 15 head ] [ last ] bi
"First 15 Bell numbers:\n%[%d, %]\n\n50th: %d\n" printf
```

{{out}}

```txt

First 15 Bell numbers:
{ 1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 27644437, 190899322 }

50th: 10726137154573358400342215518590002633917247281

```


### via Stirling sums

This solution defines Bell numbers in terms of [https://en.wikipedia.org/wiki/Bell_number#Summation_formulas sums of Stirling numbers of the second kind].
{{works with|Factor|0.99 development release 2019-07-10}}

```factor
USING: formatting kernel math math.extras math.ranges sequences ;

: bell ( m -- n )
    [ 1 ] [ dup [1,b] [ stirling ] with map-sum ] if-zero ;

50 [ bell ] { } map-integers [ 15 head ] [ last ] bi
"First 15 Bell numbers:\n%[%d, %]\n\n50th: %d\n" printf
```

{{out}}
As above.


## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func bellTriangle(n int) [][]*big.Int {
    tri := make([][]*big.Int, n)
    for i := 0; i < n; i++ {
        tri[i] = make([]*big.Int, i)
        for j := 0; j < i; j++ {
            tri[i][j] = new(big.Int)
        }
    }
    tri[1][0].SetUint64(1)
    for i := 2; i < n; i++ {
        tri[i][0].Set(tri[i-1][i-2])
        for j := 1; j < i; j++ {
            tri[i][j].Add(tri[i][j-1], tri[i-1][j-1])
        }
    }
    return tri
}

func main() {
    bt := bellTriangle(51)
    fmt.Println("First fifteen and fiftieth Bell numbers:")
    for i := 1; i <= 15; i++ {
        fmt.Printf("%2d: %d\n", i, bt[i][0])
    }
    fmt.Println("50:", bt[50][0])
    fmt.Println("\nThe first ten rows of Bell's triangle:")
    for i := 1; i <= 10; i++ {
        fmt.Println(bt[i])
    }
}
```


{{out}}

```txt

First fifteen and fiftieth Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
50: 10726137154573358400342215518590002633917247281

First ten rows of Bell's triangle:
[1]
[1 2]
[2 3 5]
[5 7 10 15]
[15 20 27 37 52]
[52 67 87 114 151 203]
[203 255 322 409 523 674 877]
[877 1080 1335 1657 2066 2589 3263 4140]
[4140 5017 6097 7432 9089 11155 13744 17007 21147]
[21147 25287 30304 36401 43833 52922 64077 77821 94828 115975]

```




## Julia

Source: Combinatorics at https://github.com/JuliaMath/Combinatorics.jl/blob/master/src/numbers.jl

```julia
"""
    bellnum(n)
Compute the ``n``th Bell number.
"""
function bellnum(n::Integer)
    if n < 0
        throw(DomainError(n))
    elseif n < 2
        return 1
    end
    list = Vector{BigInt}(undef, n)
    list[1] = 1
    for i = 2:n
        for j = 1:i - 2
            list[i - j - 1] += list[i - j]
        end
        list[i] = list[1] + list[i - 1]
    end
    return list[n]
end

for i in 1:50
    println(bellnum(i))
end

```
{{out}}

```txt

1
2
5
15
52
203
877
4140
21147
115975
678570
4213597
27644437
190899322
1382958545
10480142147
82864869804
682076806159
5832742205057
51724158235372
474869816156751
4506715738447323
44152005855084346
445958869294805289
4638590332229999353
49631246523618756274
545717047936059989389
6160539404599934652455
71339801938860275191172
846749014511809332450147
10293358946226376485095653
128064670049908713818925644
1629595892846007606764728147
21195039388640360462388656799
281600203019560266563340426570
3819714729894818339975525681317
52868366208550447901945575624941
746289892095625330523099540639146
10738823330774692832768857986425209
157450588391204931289324344702531067
2351152507740617628200694077243788988
35742549198872617291353508656626642567
552950118797165484321714693280737767385
8701963427387055089023600531855797148876
139258505266263669602347053993654079693415
2265418219334494002928484444705392276158355
37450059502461511196505342096431510120174682
628919796303118415420210454071849537746015761
10726137154573358400342215518590002633917247281
185724268771078270438257767181908917499221852770

```



## Kotlin

{{trans|C}}

```scala
class BellTriangle(n: Int) {
    private val arr: Array<Int>

    init {
        val length = n * (n + 1) / 2
        arr = Array(length) { 0 }

        set(1, 0, 1)
        for (i in 2..n) {
            set(i, 0, get(i - 1, i - 2))
            for (j in 1 until i) {
                val value = get(i, j - 1) + get(i - 1, j - 1)
                set(i, j, value)
            }
        }
    }

    private fun index(row: Int, col: Int): Int {
        require(row > 0)
        require(col >= 0)
        require(col < row)
        return row * (row - 1) / 2 + col
    }

    operator fun get(row: Int, col: Int): Int {
        val i = index(row, col)
        return arr[i]
    }

    private operator fun set(row: Int, col: Int, value: Int) {
        val i = index(row, col)
        arr[i] = value
    }
}

fun main() {
    val rows = 15
    val bt = BellTriangle(rows)

    println("First fifteen Bell numbers:")
    for (i in 1..rows) {
        println("%2d: %d".format(i, bt[i, 0]))
    }

    for (i in 1..10) {
        print("${bt[i, 0]}")
        for (j in 1 until i) {
            print(", ${bt[i, j]}")
        }
        println()
    }
}
```

{{out}}

```txt
First fifteen Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
1
1, 2
2, 3, 5
5, 7, 10, 15
15, 20, 27, 37, 52
52, 67, 87, 114, 151, 203
203, 255, 322, 409, 523, 674, 877
877, 1080, 1335, 1657, 2066, 2589, 3263, 4140
4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147
21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975
```



## Perl

{{trans|Perl 6}}

```perl
use strict 'vars';
use warnings;
use feature 'say';
use bigint;

my @b = 1;
my @Aitkens = [1];

push @Aitkens, do {
    my @c = $b[-1];
    push @c, $b[$_] + $c[$_] for 0..$#b;
    @b = @c;
    [@c]
} until (@Aitkens == 50);

my @Bell_numbers = map { @$_[0] } @Aitkens;

say 'First fifteen and fiftieth Bell numbers:';
printf "%2d: %s\n", 1+$_, $Bell_numbers[$_] for 0..14, 49;

say "\nFirst ten rows of Aitken's array:";
printf '%-7d'x@{$Aitkens[$_]}."\n", @{$Aitkens[$_]} for 0..9;
```

{{out}}

```txt
First fifteen and fiftieth Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
50: 10726137154573358400342215518590002633917247281

First ten rows of Aitken's array:
1
1      2
2      3      5
5      7      10     15
15     20     27     37     52
52     67     87     114    151    203
203    255    322    409    523    674    877
877    1080   1335   1657   2066   2589   3263   4140
4140   5017   6097   7432   9089   11155  13744  17007  21147
21147  25287  30304  36401  43833  52922  64077  77821  94828  115975

```



## Perl 6

===via Aitken's array===
{{works with|Rakudo|2019.03}}


```perl6
 my @Aitkens-array = lazy [1], -> @b {
     my @c = @b.tail;
     @c.push: @b[$_] + @c[$_] for ^@b;
     @c
 } ... *;

 my @Bell-numbers = @Aitkens-array.map: { .head };

say "First fifteen and fiftieth Bell numbers:";
printf "%2d: %s\n", 1+$_, @Bell-numbers[$_] for flat ^15, 49;

say "\nFirst ten rows of Aitken's array:";
.say for @Aitkens-array[^10];
```

{{out}}

```txt
First fifteen and fiftieth Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
50: 10726137154573358400342215518590002633917247281

First ten rows of Aitken's array:
[1]
[1 2]
[2 3 5]
[5 7 10 15]
[15 20 27 37 52]
[52 67 87 114 151 203]
[203 255 322 409 523 674 877]
[877 1080 1335 1657 2066 2589 3263 4140]
[4140 5017 6097 7432 9089 11155 13744 17007 21147]
[21147 25287 30304 36401 43833 52922 64077 77821 94828 115975]
```



### via Recurrence relation

{{works with|Rakudo|2019.03}}


```perl6
sub binomial { [*] ($^n … 0) Z/ 1 .. $^p }

my @bell = 1, -> *@s { [+] @s »*« @s.keys.map: { binomial(@s-1, $_) }  } … *;

.say for @bell[^15], @bell[50 - 1];
```

{{out}}

```txt
(1 1 2 5 15 52 203 877 4140 21147 115975 678570 4213597 27644437 190899322)
10726137154573358400342215518590002633917247281
```



### via Stirling sums

{{works with|Rakudo|2019.03}}


```perl6
my @Stirling_numbers_of_the_second_kind =
    (1,),
    { (0, |@^last) »+« (|(@^last »*« @^last.keys), 0) } … *
;
my @bell = @Stirling_numbers_of_the_second_kind.map: *.sum;

.say for @bell.head(15), @bell[50 - 1];
```

{{out}}

```txt
(1 1 2 5 15 52 203 877 4140 21147 115975 678570 4213597 27644437 190899322)
10726137154573358400342215518590002633917247281
```



## Phix

{{libheader|mpfr}}
Started out as a translation of Go, but the main routine has now been completely replaced.

```Phix
function bellTriangle(integer n)
-- nb: returns strings to simplify output
    mpz z = mpz_init(1)
    string sz = "1"
    sequence tri = {}, line = {}
    for i=1 to n do
        line = prepend(line,mpz_init_set(z))
        tri = append(tri,{sz})
        for j=2 to length(line) do
            mpz_add(z,z,line[j])
            mpz_set(line[j],z)
            sz = mpz_get_str(z)
            tri[$] = append(tri[$],sz)
        end for
    end for
    line = mpz_free(line)
    z = mpz_free(z)
    return tri
end function

sequence bt = bellTriangle(50)
printf(1,"First fifteen and fiftieth Bell numbers:\n%s\n50:%s\n\n",
         {join(vslice(bt[1..15],1)),bt[50][1]})
printf(1,"The first ten rows of Bell's triangle:\n")
for i=1 to 10 do
    printf(1,"%s\n",{join(bt[i])})
end for
```

{{out}}

```txt

First fifteen and fiftieth Bell numbers:
1 1 2 5 15 52 203 877 4140 21147 115975 678570 4213597 27644437 190899322
50:10726137154573358400342215518590002633917247281

The first ten rows of Bell's triangle:
1
1 2
2 3 5
5 7 10 15
15 20 27 37 52
52 67 87 114 151 203
203 255 322 409 523 674 877
877 1080 1335 1657 2066 2589 3263 4140
4140 5017 6097 7432 9089 11155 13744 17007 21147
21147 25287 30304 36401 43833 52922 64077 77821 94828 115975

```



## Python

{{trans|D}}

```python
def bellTriangle(n):
    tri = [None] * n
    for i in xrange(n):
        tri[i] = [0] * i
    tri[1][0] = 1
    for i in xrange(2, n):
        tri[i][0] = tri[i - 1][i - 2]
        for j in xrange(1, i):
            tri[i][j] = tri[i][j - 1] + tri[i - 1][j - 1]
    return tri

def main():
    bt = bellTriangle(51)
    print "First fifteen and fiftieth Bell numbers:"
    for i in xrange(1, 16):
        print "%2d: %d" % (i, bt[i][0])
    print "50:", bt[50][0]
    print
    print "The first ten rows of Bell's triangle:"
    for i in xrange(1, 11):
        print bt[i]

main()
```

{{out}}

```txt
First fifteen and fiftieth Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
50: 10726137154573358400342215518590002633917247281

The first ten rows of Bell's triangle:
[1]
[1, 2]
[2, 3, 5]
[5, 7, 10, 15]
[15, 20, 27, 37, 52]
[52, 67, 87, 114, 151, 203]
[203, 255, 322, 409, 523, 674, 877]
[877, 1080, 1335, 1657, 2066, 2589, 3263, 4140]
[4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147]
[21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975]
```



## REXX

Bell numbers are the number of ways of placing   '''n'''   labeled balls
into   '''n'''   indistinguishable boxes.   Bell(0)   is defined as   '''1'''.

This REXX version uses an   ''index''   of the Bell number   (which starts a zero).

A little optimization was added in calculating the factorial of a number by using memoization.

Also, see this task's   ''discussion''   to view how the sizes of Bell numbers increase in relation to its index.

```rexx
/*REXX program calculates and displays a range of  Bell numbers  (index starts at zero).*/
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' & HI==""   then do; LO=0; HI=14;  end  /*Not specified?  Then use the default.*/
if LO=='' | LO==","  then LO=  0                 /* "      "         "   "   "     "    */
if HI=='' | HI==","  then HI= 15                 /* "      "         "   "   "     "    */
numeric digits max(9, HI*2)                      /*crudely calculate the # decimal digs.*/
!.=;                       @.= 1                 /*the  FACT  function uses memoization.*/
     do j=0  for  HI+1;    $= (j==0);    jm= j-1 /*JM  is used for a shortcut  (below). */
            do k=0  for j;            _= jm-k    /* [↓]  calculate a Bell # the easy way*/
            $= $ + comb(jm,k) * @._              /*COMB≡combination or binomial function*/
            end   /*k*/
     @.j= $                                      /*assign the Jth Bell number to @ array*/
     if j>=LO  &  j<=HI  then say '    bell('right(j, length(HI) )") = "      $
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
comb: procedure expose !.; parse arg x,y;  if x==y  then return 1;   if y>x  then return 0
      if x-y<y  then y= x - y
      _= 1;          do j=x-y+1  to x;  _=_*j;  end;          return _ / fact(y)
/*──────────────────────────────────────────────────────────────────────────────────────*/
fact: procedure expose !.; parse arg x;     if !.x\==''  then return !.x
      !=1;  do f=2  to x;  != !*f;  end;    !.x=!;            return !
```

{{out|output|text=  when using the internal default inputs of:     <tt> 0   14 </tt>}}

```txt

    Bell( 0) =  1
    Bell( 1) =  1
    Bell( 2) =  2
    Bell( 3) =  5
    Bell( 4) =  15
    Bell( 5) =  52
    Bell( 6) =  203
    Bell( 7) =  877
    Bell( 8) =  4140
    Bell( 9) =  21147
    Bell(10) =  115975
    Bell(11) =  678570
    Bell(12) =  4213597
    Bell(13) =  27644437
    Bell(14) =  190899322

```

{{out|output|text=  when using the inputs of:     <tt> 49   49 </tt>}}

```txt

    Bell(49) =  10726137154573358400342215518590002633917247281

```



## Sidef

Built-in:

```ruby
say 15.of { .bell }
```


Formula as a sum of Stirling numbers of the second kind:

```ruby
func bell(n) { sum(0..n, {|k| stirling2(n, k) }) }
```


Via Aitken's array (optimized for space):

```ruby
func bell_numbers (n) {

    var acc = []
    var bell = [1]

    (n-1).times {
        acc.unshift(bell[-1])
        acc.accumulate!
        bell.push(acc[-1])
    }

    bell
}

var B = bell_numbers(50)
say "The first 15 Bell numbers: #{B.first(15).join(', ')}"
say "The fiftieth Bell number : #{B[50-1]}"
```

{{out}}

```txt

The first 15 Bell numbers: 1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597, 27644437, 190899322
The fiftieth Bell number : 10726137154573358400342215518590002633917247281

```


Aitken's array:

```ruby
func aitken_array (n) {

    var A = [1]

    [[1]] + (n-1).of {
        A = [A[-1], A...].accumulate
    }
}

aitken_array(10).each { .say }
```

{{out}}

```txt

[1]
[1, 2]
[2, 3, 5]
[5, 7, 10, 15]
[15, 20, 27, 37, 52]
[52, 67, 87, 114, 151, 203]
[203, 255, 322, 409, 523, 674, 877]
[877, 1080, 1335, 1657, 2066, 2589, 3263, 4140]
[4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147]
[21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975]

```


Aitken's array (recursive definition):

```ruby
func A((0), (0))       { 1 }
func A(n, (0))         { A(n-1, n-1) }
func A(n, k) is cached { A(n, k-1) + A(n-1, k-1) }

for n in (^10) {
    say (0..n -> map{|k| A(n, k) })
}
```


(same output as above)


## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Numerics
Imports System.Runtime.CompilerServices

Module Module1

    <Extension()>
    Sub Init(Of T)(array As T(), value As T)
        If IsNothing(array) Then Return
        For i = 0 To array.Length - 1
            array(i) = value
        Next
    End Sub

    Function BellTriangle(n As Integer) As BigInteger()()
        Dim tri(n - 1)() As BigInteger
        For i = 0 To n - 1
            Dim temp(i - 1) As BigInteger
            tri(i) = temp
            tri(i).Init(0)
        Next
        tri(1)(0) = 1
        For i = 2 To n - 1
            tri(i)(0) = tri(i - 1)(i - 2)
            For j = 1 To i - 1
                tri(i)(j) = tri(i)(j - 1) + tri(i - 1)(j - 1)
            Next
        Next
        Return tri
    End Function

    Sub Main()
        Dim bt = BellTriangle(51)
        Console.WriteLine("First fifteen Bell numbers:")
        For i = 1 To 15
            Console.WriteLine("{0,2}: {1}", i, bt(i)(0))
        Next
        Console.WriteLine("50: {0}", bt(50)(0))
        Console.WriteLine()
        Console.WriteLine("The first ten rows of Bell's triangle:")
        For i = 1 To 10
            Dim it = bt(i).GetEnumerator()
            Console.Write("[")
            If it.MoveNext() Then
                Console.Write(it.Current)
            End If
            While it.MoveNext()
                Console.Write(", ")
                Console.Write(it.Current)
            End While
            Console.WriteLine("]")
        Next
    End Sub

End Module
```

{{out}}

```txt
First fifteen Bell numbers:
 1: 1
 2: 1
 3: 2
 4: 5
 5: 15
 6: 52
 7: 203
 8: 877
 9: 4140
10: 21147
11: 115975
12: 678570
13: 4213597
14: 27644437
15: 190899322
50: 10726137154573358400342215518590002633917247281

The first ten rows of Bell's triangle:
[1]
[1, 2]
[2, 3, 5]
[5, 7, 10, 15]
[15, 20, 27, 37, 52]
[52, 67, 87, 114, 151, 203]
[203, 255, 322, 409, 523, 674, 877]
[877, 1080, 1335, 1657, 2066, 2589, 3263, 4140]
[4140, 5017, 6097, 7432, 9089, 11155, 13744, 17007, 21147]
[21147, 25287, 30304, 36401, 43833, 52922, 64077, 77821, 94828, 115975]
```



## zkl


```zkl
fcn bellTriangleW(start=1,wantRow=False){	// --> iterator
   Walker.zero().tweak('wrap(row){
      row.insert(0,row[-1]);
      foreach i in ([1..row.len()-1]){ row[i]+=row[i-1] }
      wantRow and row or row[-1]
   }.fp(List(start))).push(start,start);
}
```


```zkl
println("First fifteen Bell numbers:");
bellTriangleW().walk(15).println();
```

{{out}}

```txt

First fifteen Bell numbers:
L(1,1,2,5,15,52,203,877,4140,21147,115975,678570,4213597,27644437,190899322)

```


```zkl
println("Rows of the Bell Triangle:");
bt:=bellTriangleW(1,True); do(11){ println(bt.next()) }
```

{{out}}

```txt

Rows of the Bell Triangle:
1
1
L(1,2)
L(2,3,5)
L(5,7,10,15)
L(15,20,27,37,52)
L(52,67,87,114,151,203)
L(203,255,322,409,523,674,877)
L(877,1080,1335,1657,2066,2589,3263,4140)
L(4140,5017,6097,7432,9089,11155,13744,17007,21147)
L(21147,25287,30304,36401,43833,52922,64077,77821,94828,115975)

```

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library

```zkl
print("The fiftieth Bell number: ");
var [const] BI=Import("zklBigNum");  // libGMP
bellTriangleW(BI(1)).drop(50).value.println();
```

{{out}}

```txt

The fiftieth Bell number: 10726137154573358400342215518590002633917247281

```
