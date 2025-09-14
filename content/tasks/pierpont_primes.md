+++
title = "Pierpont primes"
description = ""
date = 2019-10-10T23:31:12Z
aliases = []
[extra]
id = 22476
[taxonomies]
categories = ["task"]
tags = []
languages = [
    "factor",
    "go",
    "julia",
    "kotlin",
    "perl",
    "perl6",
    "phix",
    "python",
    "rexx",
    "sidef",
    "zkl",
]
+++

A Pierpont prime is a prime number of the form: <big>'''2<sup>''u''</sup>3<sup>''v''</sup> + 1'''</big> for some non-negative integers <big>''' ''u'' '''</big> and <big>''' ''v'' '''</big>.


A Pierpont prime of the second kind is a prime number of the form: <big>'''2<sup>''u''</sup>3<sup>''v''</sup> - 1'''</big> for some non-negative integers <big>''' ''u'' '''</big> and <big>''' ''v'' '''</big>.


''The term "Pierpont primes" is generally understood to mean the first definition, but will be called "Pierpont primes of the first kind" on this page to distinguish them.


## Task

:* Write a routine (function, procedure, whatever) to find Pierpont primes of the first & second kinds.

:* Use the routine to find and display here, on this page, the first '''50 Pierpont primes of the first kind'''.

:* Use the routine to find and display here, on this page, the first '''50 Pierpont primes of the second kind'''

:* If your language supports large integers, find and display here, on this page, the '''250<sup>th</sup> Pierpont prime of the first kind''' and the '''250<sup>th</sup> Pierpont prime of the second kind'''.


## See also

:* '''[[wp:Pierpont_prime|Wikipedia - Pierpont primes]]'''
:* '''[[oeis:A005109|OEIS:A005109 - Class 1 -, or Pierpont primes]]'''
:* '''[[oeis:A005105|OEIS:A005105 - Class 1 +, or Pierpont primes of the second kind]]'''





## Factor


```factor
USING: fry grouping io kernel locals make math math.functions
math.primes prettyprint sequences sorting ;

: pierpont ( ulim vlim quot -- seq )
    '[
        _ <iota> _ <iota> [
            [ 2 ] [ 3 ] bi* [ swap ^ ] 2bi@ * 1 @
            dup prime? [ , ] [ drop ] if
        ] cartesian-each
    ] { } make natural-sort ; inline

: .fifty ( seq -- ) 50 head 10 group simple-table. nl ;

[let
    [ + ] [ - ] [ [ 120 80 ] dip pierpont ] bi@
    :> ( first second )

    "First 50 Pierpont primes of the first kind:" print
    first .fifty

    "First 50 Pierpont primes of the second kind:" print
    second .fifty

    "250th Pierpont prime of the first kind: " write
    249 first nth . nl

    "250th Pierpont prime of the second kind: " write
    249 second nth .
]
```

```txt

First 50 Pierpont primes of the first kind:
2      3      5       7       13      17      19      37      73      97
109    163    193     257     433     487     577     769     1153    1297
1459   2593   2917    3457    3889    10369   12289   17497   18433   39367
52489  65537  139969  147457  209953  331777  472393  629857  746497  786433
839809 995329 1179649 1492993 1769473 1990657 2654209 5038849 5308417 8503057

First 50 Pierpont primes of the second kind:
2      3      5      7       11      17       23       31       47       53
71     107    127    191     383     431      647      863      971      1151
2591   4373   6143   6911    8191    8747     13121    15551    23327    27647
62207  73727  131071 139967  165887  294911   314927   442367   472391   497663
524287 786431 995327 1062881 2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the first kind: 62518864539857068333550694039553

250th Pierpont prime of the second kind: 4111131172000956525894875083702271

```



## Go

### Brute force approach


Despite being inherently inefficient, this still works very quickly (less than 0.4 seconds on my machine).

A GMP wrapper, rather than Go's math/big package, has been used for extra speed (about 3.5 times quicker).

However, in order to be sure that the first 250 Pierpont primes will be generated, it is necessary to choose the loop sizes to produce somewhat more than this and then sort them into order.

```go
package main

import (
    "fmt"
    big "github.com/ncw/gmp"
    "sort"
)

var (
    one   = new(big.Int).SetUint64(1)
    two   = new(big.Int).SetUint64(2)
    three = new(big.Int).SetUint64(3)
)

func pierpont(ulim, vlim int, first bool) []*big.Int {
    p := new(big.Int)
    p2 := new(big.Int).Set(one)
    p3 := new(big.Int).Set(one)
    var pp []*big.Int
    for v := 0; v < vlim; v++ {
        for u := 0; u < ulim; u++ {
            p.Mul(p2, p3)
            if first {
                p.Add(p, one)
            } else {
                p.Sub(p, one)
            }
            if p.ProbablyPrime(10) {
                q := new(big.Int)
                q.Set(p)
                pp = append(pp, q)
            }
            p2.Mul(p2, two)
        }
        p3.Mul(p3, three)
        p2.Set(one)
    }
    sort.Slice(pp, func(i, j int) bool {
        return pp[i].Cmp(pp[j]) < 0
    })
    return pp
}
func main() {
    fmt.Println("First 50 Pierpont primes of the first kind:")
    pp := pierpont(120, 80, true)
    for i := 0; i < 50; i++ {
        fmt.Printf("%8d ", pp[i])
        if (i-9)%10 == 0 {
            fmt.Println()
        }
    }
    fmt.Println("\nFirst 50 Pierpont primes of the second kind:")
    pp2 := pierpont(120, 80, false)
    for i := 0; i < 50; i++ {
        fmt.Printf("%8d ", pp2[i])
        if (i-9)%10 == 0 {
            fmt.Println()
        }
    }
    fmt.Println("\n250th Pierpont prime of the first kind:", pp[249])
    fmt.Println("\n250th Pierpont prime of the second kind:", pp2[249])
}
```


```txt

First 50 Pierpont primes of the first kind:
       2        3        5        7       13       17       19       37       73       97
     109      163      193      257      433      487      577      769     1153     1297
    1459     2593     2917     3457     3889    10369    12289    17497    18433    39367
   52489    65537   139969   147457   209953   331777   472393   629857   746497   786433
  839809   995329  1179649  1492993  1769473  1990657  2654209  5038849  5308417  8503057

First 50 Pierpont primes of the second kind:
       2        3        5        7       11       17       23       31       47       53
      71      107      127      191      383      431      647      863      971     1151
    2591     4373     6143     6911     8191     8747    13121    15551    23327    27647
   62207    73727   131071   139967   165887   294911   314927   442367   472391   497663
  524287   786431   995327  1062881  2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the first kind: 62518864539857068333550694039553

250th Pierpont prime of the second kind: 4111131172000956525894875083702271

```


===3-Smooth approach===
The strategy here is to generate successive [https://en.wikipedia.org/wiki/Smooth_number 3-smooth numbers], add (or subtract) one, check if prime and, if so, append to a slice of 'big' integers until the required number is reached.

The Pierpoint primes of the first and second kind are generated at the same time so there is no real need for parallel processing.

This approach is considerably faster than the first version. Although not shown below, it produces the first 250 Pierpont primes (of both kinds) in under 0.2 seconds and the first 1000 in about 7.4 seconds. However, the first 2000 takes around 100 seconds.

These timings are for my Celeron @1.6GHz and should therefore be much faster on a more modern machine.


```go
package main

import (
    "fmt"
    big "github.com/ncw/gmp"
    "time"
)

var (
    one   = new(big.Int).SetUint64(1)
    two   = new(big.Int).SetUint64(2)
    three = new(big.Int).SetUint64(3)
)

func min(i, j int) int {
    if i < j {
        return i
    }
    return j
}

func pierpont(n int, first bool) (p [2][]*big.Int) {
    p[0] = make([]*big.Int, n)
    p[1] = make([]*big.Int, n)
    for i := 0; i < n; i++ {
        p[0][i] = new(big.Int)
        p[1][i] = new(big.Int)
    }
    p[0][0].Set(two)
    count, count1, count2 := 0, 1, 0
    var s []*big.Int
    s = append(s, new(big.Int).Set(one))
    i2, i3, k := 0, 0, 1
    n2, n3, t := new(big.Int), new(big.Int), new(big.Int)
    for count < n {
        n2.Mul(s[i2], two)
        n3.Mul(s[i3], three)
        if n2.Cmp(n3) < 0 {
            t.Set(n2)
            i2++
        } else {
            t.Set(n3)
            i3++
        }
        if t.Cmp(s[k-1]) > 0 {
            s = append(s, new(big.Int).Set(t))
            k++
            t.Add(t, one)
            if count1 < n && t.ProbablyPrime(10) {
                p[0][count1].Set(t)
                count1++
            }
            t.Sub(t, two)
            if count2 < n && t.ProbablyPrime(10) {
                p[1][count2].Set(t)
                count2++
            }
            count = min(count1, count2)
        }
    }
    return p
}

func main() {
    start := time.Now()
    p := pierpont(2000, true)
    fmt.Println("First 50 Pierpont primes of the first kind:")
    for i := 0; i < 50; i++ {
        fmt.Printf("%8d ", p[0][i])
        if (i-9)%10 == 0 {
            fmt.Println()
        }
    }
    fmt.Println("\nFirst 50 Pierpont primes of the second kind:")
    for i := 0; i < 50; i++ {
        fmt.Printf("%8d ", p[1][i])
        if (i-9)%10 == 0 {
            fmt.Println()
        }
    }

    fmt.Println("\n250th Pierpont prime of the first kind:", p[0][249])
    fmt.Println("\n250th Pierpont prime of the second kind:", p[1][249])

    fmt.Println("\n1000th Pierpont prime of the first kind:", p[0][999])
    fmt.Println("\n1000th Pierpont prime of the second kind:", p[1][999])

    fmt.Println("\n2000th Pierpont prime of the first kind:", p[0][1999])
    fmt.Println("\n2000th Pierpont prime of the second kind:", p[1][1999])

    elapsed := time.Now().Sub(start)
    fmt.Printf("\nTook %s\n", elapsed)
}
```


```txt

First 50 Pierpont primes of the first kind:
       2        3        5        7       13       17       19       37       73       97
     109      163      193      257      433      487      577      769     1153     1297
    1459     2593     2917     3457     3889    10369    12289    17497    18433    39367
   52489    65537   139969   147457   209953   331777   472393   629857   746497   786433
  839809   995329  1179649  1492993  1769473  1990657  2654209  5038849  5308417  8503057

First 50 Pierpont primes of the second kind:
       2        3        5        7       11       17       23       31       47       53
      71      107      127      191      383      431      647      863      971     1151
    2591     4373     6143     6911     8191     8747    13121    15551    23327    27647
   62207    73727   131071   139967   165887   294911   314927   442367   472391   497663
  524287   786431   995327  1062881  2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the first kind: 62518864539857068333550694039553

250th Pierpont prime of the second kind: 4111131172000956525894875083702271

1000th Pierpont prime of the first kind: 69269314716439690250482558089997110961545818230232043107188537422260188701607997086273960899938499201024414931399264696270849

1000th Pierpont prime of the second kind: 1308088756227965581249669045506775407896673213729433892383353027814827286537163695213418982500477392209371001259166465228280492460735463423

2000th Pierpont prime of the first kind: 23647056334818750458979408107288138983957799805326855934519920502493109431728722178351835778368596067773810122477389192659352731519830867553659739507195398662712180250483714053474639899675114018023738461139103130959712720686117399642823861502738433

2000th Pierpont prime of the second kind: 1702224134662426018061116932011222570937093650174807121918750428723338890211147039320296240754205680537318845776107057915956535566573559841027244444877454493022783449689509569107393738917120492483994302725479684822283929715327187974256253064796234576415398735760543848603844607

Took 1m40.781726122s

```



## Julia

The generator method is very fast but does not guarantee the primes are generated in order. Therefore we generate two times the primes needed and then sort and return the lower half.

```julia
using Primes

function pierponts(N, firstkind = true)
    ret, incdec = BigInt[], firstkind ? 1 : -1
    for k2 in 0:10000, k3 in 0:k2, switch in false:true
        i, j = switch ? (k3, k2) : (k2, k3)
        n = BigInt(2)^i * BigInt(3)^j + incdec
        if isprime(n) && !(n in ret)
            push!(ret, n)
            if length(ret) == N * 2
                return sort(ret)[1:N]
            end
        end
    end
    throw("Failed to find $(N * 2) primes")
end

println("The first 50 Pierpont primes (first kind) are: ", pierponts(50))

println("\nThe first 50 Pierpont primes (second kind) are: ", pierponts(50, false))

println("\nThe 250th Pierpont prime (first kind) is: ", pierponts(250)[250])

println("\nThe 250th Pierpont prime (second kind) is: ", pierponts(250, false)[250])

println("\nThe 1000th Pierpont prime (first kind) is: ", pierponts(1000)[1000])

println("\nThe 1000th Pierpont prime (second kind) is: ", pierponts(1000, false)[1000])

println("\nThe 2000th Pierpont prime (first kind) is: ", pierponts(2000)[2000])

println("\nThe 2000th Pierpont prime (second kind) is: ", pierponts(2000, false)[2000])

```
```txt

The first 50 Pierpont primes (first kind) are: BigInt[2, 3, 5, 7, 13, 17, 19, 37, 73, 97, 109, 163, 193, 257, 433, 487, 577, 769, 1153, 1297, 1459, 2593, 2917, 3457, 3889, 10369, 12289, 17497, 18433, 39367, 52489, 65537, 139969, 147457, 209953, 331777, 472393, 629857, 746497, 786433, 839809, 995329, 1179649, 1492993, 1769473, 1990657, 2654209, 5038849, 5308417, 8503057]

The first 50 Pierpont primes (second kind) are: BigInt[2, 3, 5, 7, 11, 17, 23, 31, 47, 53, 71, 107, 127, 191, 383, 431, 647, 863, 971, 1151, 2591, 4373, 6143, 6911, 8191, 8747, 13121, 15551, 23327, 27647, 62207, 73727, 131071, 139967, 165887, 294911, 314927, 442367, 472391, 497663, 524287, 786431, 995327, 1062881, 2519423, 10616831, 17915903, 18874367, 25509167, 30233087]

The 250th Pierpont prime (first kind) is: 62518864539857068333550694039553

The 250th Pierpont prime (second kind) is: 4111131172000956525894875083702271

The 1000th Pierpont prime (first kind) is: 69269314716439690250482558089997110961545818230232043107188537422260188701607997086273960899938499201024414931399264696270849

The 1000th Pierpont prime (second kind) is: 1308088756227965581249669045506775407896673213729433892383353027814827286537163695213418982500477392209371001259166465228280492460735463423

The 2000th Pierpont prime (first kind) is: 23647056334818750458979408107288138983957799805326855934519920502493109431728722178351835778368596067773810122477389192659352731519830867553659739507195398662712180250483714053474639899675114018023738461139103130959712720686117399642823861502738433

The 2000th Pierpont prime (second kind) is: 1702224134662426018061116932011222570937093650174807121918750428723338890211147039320296240754205680537318845776107057915956535566573559841027244444877454493022783449689509569107393738917120492483994302725479684822283929715327187974256253064796234576415398735760543848603844607

```



## Kotlin

```scala
import java.math.BigInteger
import kotlin.math.min

val one: BigInteger = BigInteger.ONE
val two: BigInteger = BigInteger.valueOf(2)
val three: BigInteger = BigInteger.valueOf(3)

fun pierpont(n: Int): List<List<BigInteger>> {
    val p = List(2) { MutableList(n) { BigInteger.ZERO } }
    p[0][0] = two
    var count = 0
    var count1 = 1
    var count2 = 0
    val s = mutableListOf<BigInteger>()
    s.add(one)
    var i2 = 0
    var i3 = 0
    var k = 1
    var n2: BigInteger
    var n3: BigInteger
    var t: BigInteger
    while (count < n) {
        n2 = s[i2] * two
        n3 = s[i3] * three
        if (n2 < n3) {
            t = n2
            i2++
        } else {
            t = n3
            i3++
        }
        if (t > s[k - 1]) {
            s.add(t)
            k++
            t += one
            if (count1 < n && t.isProbablePrime(10)) {
                p[0][count1] = t
                count1++
            }
            t -= two
            if (count2 < n && t.isProbablePrime(10)) {
                p[1][count2] = t
                count2++
            }
            count = min(count1, count2)
        }
    }
    return p
}

fun main() {
    val p = pierpont(2000)

    println("First 50 Pierpont primes of the first kind:")
    for (i in 0 until 50) {
        print("%8d ".format(p[0][i]))
        if ((i - 9) % 10 == 0) {
            println()
        }
    }

    println("\nFirst 50 Pierpont primes of the second kind:")
    for (i in 0 until 50) {
        print("%8d ".format(p[1][i]))
        if ((i - 9) % 10 == 0) {
            println()
        }
    }

    println("\n250th Pierpont prime of the first kind: ${p[0][249]}")
    println("\n250th Pierpont prime of the first kind: ${p[1][249]}")

    println("\n1000th Pierpont prime of the first kind: ${p[0][999]}")
    println("\n1000th Pierpont prime of the first kind: ${p[1][999]}")

    println("\n2000th Pierpont prime of the first kind: ${p[0][1999]}")
    println("\n2000th Pierpont prime of the first kind: ${p[1][1999]}")
}
```

```txt
First 50 Pierpont primes of the first kind:
       2        3        5        7       13       17       19       37       73       97
     109      163      193      257      433      487      577      769     1153     1297
    1459     2593     2917     3457     3889    10369    12289    17497    18433    39367
   52489    65537   139969   147457   209953   331777   472393   629857   746497   786433
  839809   995329  1179649  1492993  1769473  1990657  2654209  5038849  5308417  8503057

First 50 Pierpont primes of the second kind:
       2        3        5        7       11       17       23       31       47       53
      71      107      127      191      383      431      647      863      971     1151
    2591     4373     6143     6911     8191     8747    13121    15551    23327    27647
   62207    73727   131071   139967   165887   294911   314927   442367   472391   497663
  524287   786431   995327  1062881  2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the first kind: 62518864539857068333550694039553

250th Pierpont prime of the first kind: 4111131172000956525894875083702271

1000th Pierpont prime of the first kind: 69269314716439690250482558089997110961545818230232043107188537422260188701607997086273960899938499201024414931399264696270849

1000th Pierpont prime of the first kind: 1308088756227965581249669045506775407896673213729433892383353027814827286537163695213418982500477392209371001259166465228280492460735463423

2000th Pierpont prime of the first kind: 23647056334818750458979408107288138983957799805326855934519920502493109431728722178351835778368596067773810122477389192659352731519830867553659739507195398662712180250483714053474639899675114018023738461139103130959712720686117399642823861502738433

2000th Pierpont prime of the first kind: 1702224134662426018061116932011222570937093650174807121918750428723338890211147039320296240754205680537318845776107057915956535566573559841027244444877454493022783449689509569107393738917120492483994302725479684822283929715327187974256253064796234576415398735760543848603844607
```



## Perl

```perl
use strict;
use warnings;
use feature 'say';
use bigint try=>"GMP";
use ntheory qw<is_prime>;

# index of mininum value in list
sub min_index { my $b = $_[my $i = 0]; $_[$_] < $b && ($b = $_[$i = $_]) for 0..$#_; $i }

sub iter1 { my $m = shift; my $e = 0; return sub { $m ** $e++;    } }
sub iter2 { my $m = shift; my $e = 1; return sub { $m * ($e *= 2) } }

sub pierpont {
    my($max ) = shift || die 'Must specify count of primes to generate.';
    my($kind) = @_ ? shift : 1;
    die "Unknown type: $kind. Must be one of 1 (default) or 2" unless $kind == 1 || $kind == 2;
    $kind = -1 if $kind == 2;

    my $po3     = 3;
    my $add_one = 3;
    my @iterators;
    push @iterators, iter1(2);
    push @iterators, iter1(3); $iterators[1]->();
    my @head = ($iterators[0]->(), $iterators[1]->());

    my @pierpont;
    do {
        my $key = min_index(@head);
        my $min = $head[$key];
        push @pierpont, $min + $kind if is_prime($min + $kind);

        $head[$key] = $iterators[$key]->();

        if ($min >= $add_one) {
            push @iterators, iter2($po3);
            $add_one = $head[$#iterators] = $iterators[$#iterators]->();
            $po3 *= 3;
        }
    } until @pierpont == $max;
    @pierpont;
}

my @pierpont_1st = pierpont(250,1);
my @pierpont_2nd = pierpont(250,2);

say "First 50 Pierpont primes of the first kind:";
my $fmt = "%9d"x10 . "\n";
for my $row (0..4) { printf $fmt, map { $pierpont_1st[10*$row + $_] } 0..9 }
say "\nFirst 50 Pierpont primes of the second kind:";
for my $row (0..4) { printf $fmt, map { $pierpont_2nd[10*$row + $_] } 0..9 }

say "\n250th Pierpont prime of the first kind:    " . $pierpont_1st[249];
say "\n250th Pierpont prime of the second kind: "   . $pierpont_2nd[249];
```

```txt
First 50 Pierpont primes of the first kind:
        2        3        5        7       13       17       19       37       73       97
      109      163      193      257      433      487      577      769     1153     1297
     1459     2593     2917     3457     3889    10369    12289    17497    18433    39367
    52489    65537   139969   147457   209953   331777   472393   629857   746497   786433
   839809   995329  1179649  1492993  1769473  1990657  2654209  5038849  5308417  8503057

First 50 Pierpont primes of the second kind:
        2        3        5        7       11       17       23       31       47       53
       71      107      127      191      383      431      647      863      971     1151
     2591     4373     6143     6911     8191     8747    13121    15551    23327    27647
    62207    73727   131071   139967   165887   294911   314927   442367   472391   497663
   524287   786431   995327  1062881  2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the first kind:    62518864539857068333550694039553

250th Pierpont prime of the second kind: 4111131172000956525894875083702271
```



## Perl 6

### Finesse version


This finesse version never produces more Pierpont numbers than it needs to
fulfill the requested number of primes. It uses a series of parallel iterators
with additional iterators added as required. No need to speculatively generate
an overabundance. No need to rely on magic numbers. No need to sort them. It
produces exactly what is needed, in order, on demand.


```perl6>use ntheory:from<Perl5
 <is_prime>;

sub pierpont ($kind is copy = 1) {
    fail "Unknown type: $kind. Must be one of 1 (default) or 2" if $kind !== 1|2;
    $kind = -1 if $kind == 2;
    my $po3 = 3;
    my $add-one = 3;
    my @iterators = [1,2,4,8 … *].iterator, [3,9,27 … *].iterator;
    my @head = @iterators».pull-one;

    gather {
        loop {
            my $key = @head.pairs.min( *.value ).key;
            my $min = @head[$key];
            @head[$key] = @iterators[$key].pull-one;

            take $min + $kind if "{$min + $kind}".&is_prime;

            if $min >= $add-one {
                @iterators.push: ([|((2,4,8).map: * * $po3) … *]).iterator;
                $add-one = @head[+@iterators - 1] = @iterators[+@iterators - 1].pull-one;
                $po3 *= 3;
            }
        }
    }
}

say "First 50 Pierpont primes of the first kind:\n" ~ pierpont[^50].rotor(10)».fmt('%8d').join: "\n";

say "\nFirst 50 Pierpont primes of the second kind:\n" ~ pierpont(2)[^50].rotor(10)».fmt('%8d').join: "\n";

say "\n250th Pierpont prime of the first kind: " ~ pierpont[249];

say "\n250th Pierpont prime of the second kind: " ~ pierpont(2)[249];
```


```txt
First 50 Pierpont primes of the first kind:
       2        3        5        7       13       17       19       37       73       97
     109      163      193      257      433      487      577      769     1153     1297
    1459     2593     2917     3457     3889    10369    12289    17497    18433    39367
   52489    65537   139969   147457   209953   331777   472393   629857   746497   786433
  839809   995329  1179649  1492993  1769473  1990657  2654209  5038849  5308417  8503057

First 50 Pierpont primes of the second kind:
       2        3        5        7       11       17       23       31       47       53
      71      107      127      191      383      431      647      863      971     1151
    2591     4373     6143     6911     8191     8747    13121    15551    23327    27647
   62207    73727   131071   139967   165887   294911   314927   442367   472391   497663
  524287   786431   995327  1062881  2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the first kind: 62518864539857068333550694039553

250th Pierpont prime of the second kind: 4111131172000956525894875083702271
```



### Generalized Pierpont iterator

Alternately, a version that will generate [[wp:Pierpont_prime#Generalization|generalized Pierpont numbers]] for any set of prime integers where at least one of the primes is 2.

(Cut down output as it is exactly the same as the first version for {2,3} +1 and {2,3} -1; leaves room to demo some other options.)


```perl6
sub smooth-numbers (*@list) {
    cache my \Smooth := gather {
        my %i = (flat @list) Z=> (Smooth.iterator for ^@list);
        my %n = (flat @list) Z=> 1 xx *;

        loop {
            take my $n := %n{*}.min;

            for @list -> \k {
                %n{k} = %i{k}.pull-one * k if %n{k} == $n;
            }
        }
    }
}

# Testing various smooth numbers

for   'OEIS: A092506 - 2 + Fermat primes:',        (2),        1,  6,
    "\nOEIS: A000668 - Mersenne primes:",          (2),       -1, 10,
    "\nOEIS: A005109 - Pierpont primes 1st:",      (2,3),      1, 20,
    "\nOEIS: A005105 - Pierpont primes 2nd:",      (2,3),     -1, 20,
    "\nOEIS: A077497:",                            (2,5),      1, 20,
    "\nOEIS: A077313:",                            (2,5),     -1, 20,
    "\nOEIS: A002200 - (\"Hamming\" primes 1st):", (2,3,5),    1, 20,
    "\nOEIS: A293194 - (\"Hamming\" primes 2nd):", (2,3,5),   -1, 20,
    "\nOEIS: A077498:",                            (2,7),      1, 20,
    "\nOEIS: A077314:",                            (2,7),     -1, 20,
    "\nOEIS: A174144 - (\"Humble\" primes 1st):",  (2,3,5,7),  1, 20,
    "\nOEIS: A299171 - (\"Humble\" primes 2nd):",  (2,3,5,7), -1, 20,
    "\nOEIS: A077499:",                            (2,11),     1, 20,
    "\nOEIS: A077315:",                            (2,11),    -1, 20,
    "\nOEIS: A173236:",                            (2,13),     1, 20,
    "\nOEIS: A173062:",                            (2,13),    -1, 20

  -> $title, $primes, $add, $count {

      say "$title smooth \{$primes\} {$add > 0 ?? '+' !! '-'} 1 ";
      put smooth-numbers(|$primes).map( * + $add ).grep( *.is-prime )[^$count]
}
```


```txt
OEIS: A092506 - 2 + Fermat primes: smooth {2} + 1
2 3 5 17 257 65537

OEIS: A000668 - Mersenne primes: smooth {2} - 1
3 7 31 127 8191 131071 524287 2147483647 2305843009213693951 618970019642690137449562111

OEIS: A005109 - Pierpont primes 1st: smooth {2 3} + 1
2 3 5 7 13 17 19 37 73 97 109 163 193 257 433 487 577 769 1153 1297

OEIS: A005105 - Pierpont primes 2nd: smooth {2 3} - 1
2 3 5 7 11 17 23 31 47 53 71 107 127 191 383 431 647 863 971 1151

OEIS: A077497: smooth {2 5} + 1
2 3 5 11 17 41 101 251 257 401 641 1601 4001 16001 25601 40961 62501 65537 160001 163841

OEIS: A077313: smooth {2 5} - 1
3 7 19 31 79 127 199 499 1249 1279 1999 4999 5119 8191 12799 20479 31249 49999 51199 79999

OEIS: A002200 - ("Hamming" primes 1st): smooth {2 3 5} + 1
2 3 5 7 11 13 17 19 31 37 41 61 73 97 101 109 151 163 181 193

OEIS: A293194 - ("Hamming" primes 2nd): smooth {2 3 5} - 1
2 3 5 7 11 17 19 23 29 31 47 53 59 71 79 89 107 127 149 179

OEIS: A077498: smooth {2 7} + 1
2 3 5 17 29 113 197 257 449 1373 3137 50177 65537 114689 268913 470597 614657 1075649 3294173 7340033

OEIS: A077314: smooth {2 7} - 1
3 7 13 31 97 127 223 1567 3583 4801 6271 8191 19207 25087 33613 76831 131071 401407 524287 917503

OEIS: A174144 - ("Humble" primes 1st): smooth {2 3 5 7} + 1
2 3 5 7 11 13 17 19 29 31 37 41 43 61 71 73 97 101 109 113

OEIS: A299171 - ("Humble" primes 2nd): smooth {2 3 5 7} - 1
2 3 5 7 11 13 17 19 23 29 31 41 47 53 59 71 79 83 89 97

OEIS: A077499: smooth {2 11} + 1
2 3 5 17 23 89 257 353 1409 2663 30977 65537 170369 495617 5767169 23068673 59969537 82458113 453519617 3429742097

OEIS: A077315: smooth {2 11} - 1
3 7 31 43 127 241 967 5323 8191 117127 131071 524287 7496191 10307263 77948683 253755391 428717761 738197503 1714871047 2147483647

OEIS: A173236: smooth {2 13} + 1
2 3 5 17 53 257 677 3329 13313 35153 65537 2768897 13631489 2303721473 3489660929 4942652417 11341398017 10859007357953 1594691292233729 31403151600910337

OEIS: A173062: smooth {2 13} - 1
3 7 31 103 127 337 1663 5407 8191 131071 346111 524287 2970343 3655807 22151167 109051903 617831551 1631461441 2007952543 2147483647
```



## Phix

I also tried using a priority queue, popping the smallest (and any duplicates of it) and pushing *2 and *3 on every iteration, which worked pretty well but ended up about 20% slower, with about 1500 untested candidates left in the queue by the time it found the 2000th second kind.

```Phix
-- demo/rosetta/Pierpont_primes.exw
include mpfr.e

function pierpont(integer n)
    sequence p = {{mpz_init(2)},{}},
             s = {mpz_init(1)}
    integer i2 = 1, i3 = 1
    mpz {n2, n3, t} = mpz_inits(3)
    randstate state = gmp_randinit_mt()
    atom t1 = time()+1
    while min(length(p[1]),length(p[2])) < n do
        mpz_mul_si(n2,s[i2],2)
        mpz_mul_si(n3,s[i3],3)
        if mpz_cmp(n2,n3)<0 then
            mpz_set(t,n2)
            i2 += 1
        else
            mpz_set(t,n3)
            i3 += 1
        end if
        if mpz_cmp(t,s[$]) > 0 then
            s = append(s, mpz_init_set(t))
            mpz_add_ui(t,t,1)
            if length(p[1]) < n and mpz_probable_prime_p(t,state) then
                p[1] = append(p[1],mpz_init_set(t))
            end if
            mpz_sub_ui(t,t,2)
            if length(p[2]) < n and mpz_probable_prime_p(t,state) then
                p[2] = append(p[2],mpz_init_set(t))
            end if
        end if
        if time()>t1 then
            printf(1,"Found: 1st:%d/%d, 2nd:%d/%d\r",
                     {length(p[1]),n,length(p[2]),n})
            t1 = time()+1
        end if
    end while
    return p
end function

constant limit = 2000           -- 2 mins
--constant limit = 1000         -- 8.1s
--constant limit = 250          -- 0.1s
atom t0 = time()
sequence p = pierpont(limit)
constant fs = {"first","second"}

for i=1 to length(fs) do
    printf(1,"First 50 Pierpont primes of the %s kind:\n",{fs[i]})
    for j=1 to 50 do
        mpfr_printf(1,"%8Zd ", p[i][j])
        if mod(j,10)=0 then printf(1,"\n") end if
    end for
    printf(1,"\n")
end for

constant t = {250,1000,2000}
for i=1 to length(t) do
    integer ti = t[i]
    if ti>limit then exit end if
    for j=1 to length(fs) do
        string zs = shorten(mpz_get_str(p[j][ti]))
        printf(1,"%dth Pierpont prime of the %s kind: %s\n",{ti,fs[j],zs})
    end for
    printf(1,"\n")
end for
printf(1,"Took %s\n", elapsed(time()-t0))
```

```txt

First 50 Pierpont primes of the first kind:
       2        3        5        7       13       17       19       37       73       97
     109      163      193      257      433      487      577      769     1153     1297
    1459     2593     2917     3457     3889    10369    12289    17497    18433    39367
   52489    65537   139969   147457   209953   331777   472393   629857   746497   786433
  839809   995329  1179649  1492993  1769473  1990657  2654209  5038849  5308417  8503057

First 50 Pierpont primes of the second kind:
       2        3        5        7       11       17       23       31       47       53
      71      107      127      191      383      431      647      863      971     1151
    2591     4373     6143     6911     8191     8747    13121    15551    23327    27647
   62207    73727   131071   139967   165887   294911   314927   442367   472391   497663
  524287   786431   995327  1062881  2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the first kind: 62518864539857068333550694039553
250th Pierpont prime of the second kind: 4111131172000956525894875083702271

1000th Pierpont prime of the first kind: 6926931471643969025...4931399264696270849 (125 digits)
1000th Pierpont prime of the second kind: 1308088756227965581...8280492460735463423 (139 digits)

2000th Pierpont prime of the first kind: 2364705633481875045...9642823861502738433 (248 digits)
2000th Pierpont prime of the second kind: 1702224134662426018...5760543848603844607 (277 digits)

Took 2 minutes and 01s

```

Note that shorten() has recently been added as a builtin, in honour of this and the several other dozen rc
tasks that previously all re-implemented some variation of it. Should you not yet have it, just use this:

```Phix
function shorten(string s, what="digits", integer ml=20)
    integer l = length(s)
    string ls = sprintf(" (%d %s)",{l,what})
    if l>ml*2+3+length(ls) then
        s[ml..-ml] = "..."
        s &= ls
    end if
    return s
end function
```



## Python

```python
import random

# Copied from https://rosettacode.org/wiki/Miller-Rabin_primality_test#Python
def is_Prime(n):
    """
    Miller-Rabin primality test.

    A return value of False means n is certainly not prime. A return value of
    True means n is very likely a prime.
    """
    if n!=int(n):
        return False
    n=int(n)
    #Miller-Rabin test for prime
    if n==0 or n==1 or n==4 or n==6 or n==8 or n==9:
        return False

    if n==2 or n==3 or n==5 or n==7:
        return True
    s = 0
    d = n-1
    while d%2==0:
        d>>=1
        s+=1
    assert(2**s * d == n-1)

    def trial_composite(a):
        if pow(a, d, n) == 1:
            return False
        for i in range(s):
            if pow(a, 2**i * d, n) == n-1:
                return False
        return True

    for i in range(8):#number of trials
        a = random.randrange(2, n)
        if trial_composite(a):
            return False

    return True

def pierpont(ulim, vlim, first):
    p = 0
    p2 = 1
    p3 = 1
    pp = []
    for v in xrange(vlim):
        for u in xrange(ulim):
            p = p2 * p3
            if first:
                p = p + 1
            else:
                p = p - 1
            if is_Prime(p):
                pp.append(p)
            p2 = p2 * 2
        p3 = p3 * 3
        p2 = 1
    pp.sort()
    return pp

def main():
    print "First 50 Pierpont primes of the first kind:"
    pp = pierpont(120, 80, True)
    for i in xrange(50):
        print "%8d " % pp[i],
        if (i - 9) % 10 == 0:
            print
    print "First 50 Pierpont primes of the second kind:"
    pp2 = pierpont(120, 80, False)
    for i in xrange(50):
        print "%8d " % pp2[i],
        if (i - 9) % 10 == 0:
            print
    print "250th Pierpont prime of the first kind:", pp[249]
    print "250th Pierpont prime of the second kind:", pp2[249]

main()
```

```txt
First 50 Pierpont primes of the first kind:
       2         3         5         7        13        17        19        37        73        97
     109       163       193       257       433       487       577       769      1153      1297
    1459      2593      2917      3457      3889     10369     12289     17497     18433     39367
   52489     65537    139969    147457    209953    331777    472393    629857    746497    786433
  839809    995329   1179649   1492993   1769473   1990657   2654209   5038849   5308417   8503057
First 50 Pierpont primes of the second kind:
       2         3         5         7        11        17        23        31        47        53
      71       107       127       191       383       431       647       863       971      1151
    2591      4373      6143      6911      8191      8747     13121     15551     23327     27647
   62207     73727    131071    139967    165887    294911    314927    442367    472391    497663
  524287    786431    995327   1062881   2519423  10616831  17915903  18874367  25509167  30233087
250th Pierpont prime of the first kind: 62518864539857068333550694039553
250th Pierpont prime of the second kind: 4111131172000956525894875083702271
```



## REXX

The REXX language has a "big num" capability to handle almost any amount of decimal digits,   but

it lacks a robust   '''isPrime'''   function.   Without that, verifying very large primes is problematic.

```rexx
/*REXX program finds and displays  Pierpont primes  of the  first  and  second  kinds.  */
parse arg n .                                    /*obtain optional argument from the CL.*/
if n=='' | n==","  then n= 50                    /*Not specified?  Then use the default.*/
numeric digits n                                 /*ensure enough decimal digs (bit int).*/
big= copies(9, digits() )                        /*BIG:  used as a max number (a limit).*/
       do t=1  to -1  by -2;  usum= 0;   vsum= 0;    s= 0
       #= 0                                      /*number of Pierpont primes  (so far). */
       w= 0                                      /*the max width of a Pierpont prime.   */
       $=;    do j=0  until #>=n                 /*$: the list of the Pierpont primes.  */
              if usum<=s  then usum= get(2, 3);    if vsum<=s  then vsum= get(3, 2)
              s= min(vsum, usum);  if \isPrime(s)  then iterate  /*get min;  is prime?  */
              #= # + 1;            $= $ s                        /*bump counter; append.*/
              w= max(w, length(s) )                              /*find max prime width.*/
              end   /*j*/
       say
       if t==1  then @= '1st'                                    /*choose word for type.*/
                else @= '2nd'                                    /*   "     "   "    "  */
       say center(n   " Pierpont primes of the "   @   ' kind', max(10*(w+1) -1, 79), "═")
       call show $                                               /*display the primes.  */
       end   /*type*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:    do j=1  by 10  to words($);       _=
           do k=j  for 10;                 _= _ right( word($, k), w)
           end   /*k*/
         if _\==''  then say substr( strip(_, 'T'), 2)
         end     /*j*/;                                        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg x;  if x<2  then return 0                  /*not  a prime.*/
         if wordpos(x, '2 3 5 7')\==0     then return 1                  /*it's a prime.*/
         if x//2==0  then return 0;       if x//3==0  then return 0      /*not  a prime.*/
           do j=5  by 6  until j*j>x
           if x//j==0  then return 0;     if x//(j+2)==0  then return 0  /*not  a prime.*/
           end   /*j*/;                                        return 1  /*it's a prime.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
get: parse arg c1,c2; m=big;  do   ju=0;  pu= c1**ju;  if pu+t>s  then return min(m, pu+t)
                                do jv=0;  pv= c2**jv;  if pv  >s  then iterate ju
                                _= pu*pv + t;          if _   >s  then m= min(_, m)
                                end   /*jv*/
                              end     /*ju*/           /*see the    RETURN    (above).  */
```

```txt

═════════════════════50  Pierpont primes of the  1st  kind═════════════════════
      2       3       5       7      13      17      19      37      73      97
    109     163     193     257     433     487     577     769    1153    1297
   1459    2593    2917    3457    3889   10369   12289   17497   18433   39367
  52489   65537  139969  147457  209953  331777  472393  629857  746497  786433
 839809  995329 1179649 1492993 1769473 1990657 2654209 5038849 5308417 8503057

══════════════════════════50  Pierpont primes of the  2nd  kind══════════════════════════
       2        3        5        7       11       17       23       31       47       53
      71      107      127      191      383      431      647      863      971     1151
    2591     4373     6143     6911     8191     8747    13121    15551    23327    27647
   62207    73727   131071   139967   165887   294911   314927   442367   472391   497663
  524287   786431   995327  1062881  2519423 10616831 17915903 18874367 25509167 30233087

```



## Sidef


```ruby
func smooth_generator(primes) {
    var s = primes.len.of { [1] }
    {
        var n = s.map { .first }.min
        { |i|
            s[i].shift if (s[i][0] == n)
            s[i] << (n * primes[i])
        } * primes.len
        n
    }
}

func pierpont_primes(n, k = 1) {
    var g = smooth_generator([2,3])
    1..Inf -> lazy.map { g.run + k }.grep { .is_prime }.first(n)
}

say "First 50 Pierpont primes of the 1st kind: "
say pierpont_primes(50, +1).join(' ')

say "\nFirst 50 Pierpont primes of the 2nd kind: "
say pierpont_primes(50, -1).join(' ')

for n in (250, 500, 1000) {
    var p = pierpont_primes(n, +1).last
    var q = pierpont_primes(n, -1).last
    say "\n#{n}th Pierpont prime of the 1st kind: #{p}"
    say "#{n}th Pierpont prime of the 2nd kind: #{q}"
}
```

```txt

First 50 Pierpont primes of the 1st kind:
2 3 5 7 13 17 19 37 73 97 109 163 193 257 433 487 577 769 1153 1297 1459 2593 2917 3457 3889 10369 12289 17497 18433 39367 52489 65537 139969 147457 209953 331777 472393 629857 746497 786433 839809 995329 1179649 1492993 1769473 1990657 2654209 5038849 5308417 8503057

First 50 Pierpont primes of the 2nd kind:
2 3 5 7 11 17 23 31 47 53 71 107 127 191 383 431 647 863 971 1151 2591 4373 6143 6911 8191 8747 13121 15551 23327 27647 62207 73727 131071 139967 165887 294911 314927 442367 472391 497663 524287 786431 995327 1062881 2519423 10616831 17915903 18874367 25509167 30233087

250th Pierpont prime of the 1st kind: 62518864539857068333550694039553
250th Pierpont prime of the 2nd kind: 4111131172000956525894875083702271

500th Pierpont prime of the 1st kind: 2228588214163334773718162801501181906563609505773852212825423873
500th Pierpont prime of the 2nd kind: 1582451786724712011220565860035647126064921139018525742951994237124607

1000th Pierpont prime of the 1st kind: 69269314716439690250482558089997110961545818230232043107188537422260188701607997086273960899938499201024414931399264696270849
1000th Pierpont prime of the 2nd kind: 1308088756227965581249669045506775407896673213729433892383353027814827286537163695213418982500477392209371001259166465228280492460735463423

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
Using GMP's probabilistic primes makes it is easy and fast to test for primeness.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
var [const] one=BI(1), two=BI(2), three=BI(3);

fcn pierPonts(n){  //-->((bigInt first kind primes) (bigInt second))
   pps1,pps2 := List(BI(2)), List();
   count1, count2, s := 1, 0, List(BI(1));  // n==2_000, s-->266_379 elements
   i2,i3,k := 0, 0, 1;
   n2,n3,t := BI(0),BI(0),BI(0);
   while(count1.min(count2) < n){
      n2.set(s[i2]).mul(two);	// .mul, .add, .sub are in-place
      n3.set(s[i3]).mul(three);
      if(n2<n3){ t.set(n2); i2+=1; }
      else     { t.set(n3); i3+=1; }
      if(t > s[k-1]){
	 s.append(t.copy());
	 k+=1;
	 t.add(one);
	 if(count1<n and t.probablyPrime()){
	    pps1.append(t.copy());
	    count1+=1;
	 }
	 if(count2<n and t.sub(two).probablyPrime()){
	    pps2.append(t.copy());
	    count2+=1;
	 }
      }
   }
   return(pps1,pps2)
}
```


```zkl
pps1,pps2 := pierPonts(2_000);

println("The first 50 Pierpont primes (first kind):");
foreach r in (5){ pps1[r*10,10].apply("%10d".fmt).concat().println() }

println("\nThe first 50 Pierpont primes (second kind):");
foreach r in (5){ pps2[r*10,10].apply("%10d".fmt).concat().println() }

foreach n in (T(250, 1_000, 2_000)){
   println("\n%4dth Pierpont prime, first kind: ".fmt(n), pps1[n-1]);
   println( "                      second kind: ",        pps2[n-1]);
}
```

<pre style="font-size:83%">
The first 50 Pierpont primes (first kind):
         2         3         5         7        13        17        19        37        73        97
       109       163       193       257       433       487       577       769      1153      1297
      1459      2593      2917      3457      3889     10369     12289     17497     18433     39367
     52489     65537    139969    147457    209953    331777    472393    629857    746497    786433
    839809    995329   1179649   1492993   1769473   1990657   2654209   5038849   5308417   8503057

The first 50 Pierpont primes (second kind):
         2         3         5         7        11        17        23        31        47        53
        71       107       127       191       383       431       647       863       971      1151
      2591      4373      6143      6911      8191      8747     13121     15551     23327     27647
     62207     73727    131071    139967    165887    294911    314927    442367    472391    497663
    524287    786431    995327   1062881   2519423  10616831  17915903  18874367  25509167  30233087

 250th Pierpont prime, first kind: 62518864539857068333550694039553
                      second kind: 4111131172000956525894875083702271

1000th Pierpont prime, first kind: 69269314716439690250482558089997110961545818230232043107188537422260188701607997086273960899938499201024414931399264696270849
                      second kind: 1308088756227965581249669045506775407896673213729433892383353027814827286537163695213418982500477392209371001259166465228280492460735463423

2000th Pierpont prime, first kind: 23647056334818750458979408107288138983957799805326855934519920502493109431728722178351835778368596067773810122477389192659352731519830867553659739507195398662712180250483714053474639899675114018023738461139103130959712720686117399642823861502738433
                      second kind: 1702224134662426018061116932011222570937093650174807121918750428723338890211147039320296240754205680537318845776107057915956535566573559841027244444877454493022783449689509569107393738917120492483994302725479684822283929715327187974256253064796234576415398735760543848603844607

```

