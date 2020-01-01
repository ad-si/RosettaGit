+++
title = "Humble numbers"
description = ""
date = 2019-10-08T23:59:48Z
aliases = []
[extra]
id = 22492
[taxonomies]
categories = []
tags = []
+++

{{task|Prime numbers}}

Humble numbers are positive integers which have   no   prime factors <big>&gt;</big> '''7'''.


Humble numbers are also called   ''7-smooth numbers'',   and sometimes called ''highly composite'',

although this conflicts with another meaning of   ''highly composite numbers''.



Another way to express the above is:

  <big><big> humble  =  2<sup>i</sup> &times; 3<sup>j</sup> &times; 5<sup>k</sup> &times; 7<sup>m</sup> </big></big>

            where   <big>  i, j, k, m <big>&ge;</big> 0 </big>


;Task:
:*   show the first   '''50'''   humble numbers   (in a horizontal list)
:*   show the number of humble numbers that have   '''x'''   decimal digits for all   '''x's'''   up to   '''n'''   (inclusive).
:*   show   (as many as feasible or reasonable for above)   on separate lines
:*   show all output here on this page


;Related tasks:
:*   [[Hamming numbers]]


;References:
:*   [https://en.wikipedia.org/wiki/Smooth_number#Definition smooth numbers, see the 2<sup>nd</sup> paragraph]
:*   [http://oeis.org/A002473 OEIS A002473   humble numbers]
:*   [http://www.informatik.uni-ulm.de/acm/Locals/1996/number.sol University of Ulm, The first 5842 terms of humble numbers]





## C++

{{trans|Kotlin}}

```cpp
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>

bool isHumble(int i) {
    if (i <= 1) return true;
    if (i % 2 == 0) return isHumble(i / 2);
    if (i % 3 == 0) return isHumble(i / 3);
    if (i % 5 == 0) return isHumble(i / 5);
    if (i % 7 == 0) return isHumble(i / 7);
    return false;
}

auto toString(int n) {
    std::stringstream ss;
    ss << n;
    return ss.str();
}

int main() {
    auto limit = SHRT_MAX;
    std::map<int, int> humble;
    auto count = 0;
    auto num = 1;

    while (count < limit) {
        if (isHumble(num)) {
            auto str = toString(num);
            auto len = str.length();
            auto it = humble.find(len);

            if (it != humble.end()) {
                it->second++;
            } else {
                humble[len] = 1;
            }

            if (count < 50) std::cout << num << ' ';
            count++;
        }
        num++;
    }
    std::cout << "\n\n";

    std::cout << "Of the first " << count << " humble numbers:\n";
    num = 1;
    while (num < humble.size() - 1) {
        auto it = humble.find(num);
        if (it != humble.end()) {
            auto c = *it;
            std::cout << std::setw(5) << c.second << " have " << std::setw(2) << num << " digits\n";
            num++;
        } else {
            break;
        }
    }

    return 0;
}
```

{{out}}

```txt
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

Of the first 32767 humble numbers:
    9 have  1 digits
   36 have  2 digits
   95 have  3 digits
  197 have  4 digits
  356 have  5 digits
  579 have  6 digits
  882 have  7 digits
 1272 have  8 digits
 1767 have  9 digits
```



## Factor


```factor
USING: accessors assocs combinators deques dlists formatting fry
generalizations io kernel make math math.functions math.order
prettyprint sequences tools.memory.private ;
IN: rosetta-code.humble-numbers

TUPLE: humble-iterator 2s 3s 5s 7s digits
    { #digits initial: 1 } { target initial: 10 } ;

: <humble-iterator> ( -- humble-iterator )
    humble-iterator new
    1 1dlist >>2s
    1 1dlist >>3s
    1 1dlist >>5s
    1 1dlist >>7s
    H{ } clone >>digits ;

: enqueue ( n humble-iterator -- )
    {
        [ [ 2 * ] [ 2s>> ] ]
        [ [ 3 * ] [ 3s>> ] ]
        [ [ 5 * ] [ 5s>> ] ]
        [ [ 7 * ] [ 7s>> ] ]
    } [ bi* push-back ] map-compose 2cleave ;

: count-digits ( humble-iterator n -- )
    [ over target>> >=
    [ [ 1 + ] change-#digits [ 10 * ] change-target ] when ]
    [ drop 1 swap [ #digits>> ] [ digits>> ] bi at+ ] bi ;

: ?pop ( 2s 3s 5s 7s n -- )
    '[ dup peek-front _ = [ pop-front* ] [ drop ] if ] 4 napply ;

: next ( humble-iterator -- n )
    dup dup { [ 2s>> ] [ 3s>> ] [ 5s>> ] [ 7s>> ] } cleave
    4 ndup [ peek-front ] 4 napply min min min
    { [ ?pop ] [ swap enqueue ] [ count-digits ] [ ] } cleave ;

: upto-n-digits ( humble-iterator n -- seq )
    1 + swap [ [ 2dup digits>> key? ] [ dup next , ] until ] { }
    make [ digits>> delete-at ] dip but-last-slice ;

: .first50 ( seq -- )
    "First 50 humble numbers:" print 50 head [ pprint bl ] each
    nl ;

: .digit-breakdown ( humble-iterator -- )
    "The digit counts of humble numbers:" print digits>> [
        commas swap dup 1 = "" "s" ? "%9s have %2d digit%s\n"
        printf
    ] assoc-each ;

: humble-numbers ( -- )
    [ <humble-iterator> dup 95 upto-n-digits
    [ .first50 nl ] [ drop .digit-breakdown nl ] [
        "Total number of humble numbers found: " write length
        commas print
    ] tri ] time ;

MAIN: humble-numbers
```

{{out}}

```txt

First 50 humble numbers:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

The digit counts of humble numbers:
        9 have  1 digit
       36 have  2 digits
       95 have  3 digits
      197 have  4 digits
      356 have  5 digits
      579 have  6 digits
      882 have  7 digits
    1,272 have  8 digits
    1,767 have  9 digits
    2,381 have 10 digits
    3,113 have 11 digits
    3,984 have 12 digits
    5,002 have 13 digits
    6,187 have 14 digits
    7,545 have 15 digits
    9,081 have 16 digits
   10,815 have 17 digits
   12,759 have 18 digits
   14,927 have 19 digits
   17,323 have 20 digits
   19,960 have 21 digits
   22,853 have 22 digits
   26,015 have 23 digits
   29,458 have 24 digits
   33,188 have 25 digits
   37,222 have 26 digits
   41,568 have 27 digits
   46,245 have 28 digits
   51,254 have 29 digits
   56,618 have 30 digits
   62,338 have 31 digits
   68,437 have 32 digits
   74,917 have 33 digits
   81,793 have 34 digits
   89,083 have 35 digits
   96,786 have 36 digits
  104,926 have 37 digits
  113,511 have 38 digits
  122,546 have 39 digits
  132,054 have 40 digits
  142,038 have 41 digits
  152,515 have 42 digits
  163,497 have 43 digits
  174,986 have 44 digits
  187,004 have 45 digits
  199,565 have 46 digits
  212,675 have 47 digits
  226,346 have 48 digits
  240,590 have 49 digits
  255,415 have 50 digits
  270,843 have 51 digits
  286,880 have 52 digits
  303,533 have 53 digits
  320,821 have 54 digits
  338,750 have 55 digits
  357,343 have 56 digits
  376,599 have 57 digits
  396,533 have 58 digits
  417,160 have 59 digits
  438,492 have 60 digits
  460,533 have 61 digits
  483,307 have 62 digits
  506,820 have 63 digits
  531,076 have 64 digits
  556,104 have 65 digits
  581,902 have 66 digits
  608,483 have 67 digits
  635,864 have 68 digits
  664,053 have 69 digits
  693,065 have 70 digits
  722,911 have 71 digits
  753,593 have 72 digits
  785,141 have 73 digits
  817,554 have 74 digits
  850,847 have 75 digits
  885,037 have 76 digits
  920,120 have 77 digits
  956,120 have 78 digits
  993,058 have 79 digits
1,030,928 have 80 digits
1,069,748 have 81 digits
1,109,528 have 82 digits
1,150,287 have 83 digits
1,192,035 have 84 digits
1,234,774 have 85 digits
1,278,527 have 86 digits
1,323,301 have 87 digits
1,369,106 have 88 digits
1,415,956 have 89 digits
1,463,862 have 90 digits
1,512,840 have 91 digits
1,562,897 have 92 digits
1,614,050 have 93 digits
1,666,302 have 94 digits
1,719,669 have 95 digits

Total number of humble numbers found: 41,990,065
Running time: 335.1803624581294 seconds

```



## Go

Not particularly fast and uses a lot of memory but easier to understand than the 'log' based methods for generating 7-smooth numbers.

```go
package main

import (
    "fmt"
    "math/big"
)

var (
    one   = new(big.Int).SetUint64(1)
    two   = new(big.Int).SetUint64(2)
    three = new(big.Int).SetUint64(3)
    five  = new(big.Int).SetUint64(5)
    seven = new(big.Int).SetUint64(7)
    ten   = new(big.Int).SetUint64(10)
)

func min(a, b *big.Int) *big.Int {
    if a.Cmp(b) < 0 {
        return a
    }
    return b
}

func humble(n int) []*big.Int {
    h := make([]*big.Int, n)
    h[0] = new(big.Int).Set(one)
    next2, next3 := new(big.Int).Set(two), new(big.Int).Set(three)
    next5, next7 := new(big.Int).Set(five), new(big.Int).Set(seven)
    var i, j, k, l int
    for m := 1; m < len(h); m++ {
        h[m] = new(big.Int).Set(min(next2, min(next3, min(next5, next7))))
        if h[m].Cmp(next2) == 0 {
            i++
            next2.Mul(two, h[i])
        }
        if h[m].Cmp(next3) == 0 {
            j++
            next3.Mul(three, h[j])
        }
        if h[m].Cmp(next5) == 0 {
            k++
            next5.Mul(five, h[k])
        }
        if h[m].Cmp(next7) == 0 {
            l++
            next7.Mul(seven, h[l])
        }
    }
    return h
}

func commatize(n int) string {
    s := fmt.Sprintf("%d", n)
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    return s
}

func main() {
    const n = 13 * 1e6  // calculate the first 13 million humble numbers, say
    h := humble(n)
    fmt.Println("The first 50 humble numbers are:")
    fmt.Println(h[0:50])

    maxDigits := len(h[len(h)-1].String()) - 1
    counts := make([]int, maxDigits+1)
    var maxUsed int
    digits := 1
    pow10 := new(big.Int).Set(ten)
    for i := 0; i < len(h); i++ {
        for {
            if h[i].Cmp(pow10) >= 0 {
                pow10.Mul(pow10, ten)
                digits++
            } else {
                break
            }
        }
        if digits > maxDigits {
            maxUsed = i
            break
        }
        counts[digits]++
    }
    fmt.Printf("\nOf the first %s humble numbers:\n", commatize(maxUsed))
    for i := 1; i <= maxDigits; i++ {
        s := "s"
        if i == 1 {
            s = ""
        }
        fmt.Printf("%9s have %2d digit%s\n", commatize(counts[i]), i, s)
    }
}
```


{{out}}

```txt

The first 50 humble numbers are:
[1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120]

Of the first 12,591,874 humble numbers:
        9 have  1 digit
       36 have  2 digits
       95 have  3 digits
      197 have  4 digits
      356 have  5 digits
      579 have  6 digits
      882 have  7 digits
    1,272 have  8 digits
    1,767 have  9 digits
    2,381 have 10 digits
    3,113 have 11 digits
    3,984 have 12 digits
    5,002 have 13 digits
    6,187 have 14 digits
    7,545 have 15 digits
    9,081 have 16 digits
   10,815 have 17 digits
   12,759 have 18 digits
   14,927 have 19 digits
   17,323 have 20 digits
   19,960 have 21 digits
   22,853 have 22 digits
   26,015 have 23 digits
   29,458 have 24 digits
   33,188 have 25 digits
   37,222 have 26 digits
   41,568 have 27 digits
   46,245 have 28 digits
   51,254 have 29 digits
   56,618 have 30 digits
   62,338 have 31 digits
   68,437 have 32 digits
   74,917 have 33 digits
   81,793 have 34 digits
   89,083 have 35 digits
   96,786 have 36 digits
  104,926 have 37 digits
  113,511 have 38 digits
  122,546 have 39 digits
  132,054 have 40 digits
  142,038 have 41 digits
  152,515 have 42 digits
  163,497 have 43 digits
  174,986 have 44 digits
  187,004 have 45 digits
  199,565 have 46 digits
  212,675 have 47 digits
  226,346 have 48 digits
  240,590 have 49 digits
  255,415 have 50 digits
  270,843 have 51 digits
  286,880 have 52 digits
  303,533 have 53 digits
  320,821 have 54 digits
  338,750 have 55 digits
  357,343 have 56 digits
  376,599 have 57 digits
  396,533 have 58 digits
  417,160 have 59 digits
  438,492 have 60 digits
  460,533 have 61 digits
  483,307 have 62 digits
  506,820 have 63 digits
  531,076 have 64 digits
  556,104 have 65 digits
  581,902 have 66 digits
  608,483 have 67 digits
  635,864 have 68 digits
  664,053 have 69 digits
  693,065 have 70 digits

```



## Julia

To spare heap memory, keeps only the last 2 million values found for use in the generation of further values.

```julia

function counthumbledigits(maxdigits, returnsequencelength=50)
    n, count, adjustindex, maxdiff = BigInt(1), 0, BigInt(0), 0
    humble, savesequence = Vector{BigInt}([1]), Vector{BigInt}()
    base2, base3, base5, base7 = 1, 1, 1, 1
    next2, next3, next5, next7 = BigInt(2), BigInt(3), BigInt(5), BigInt(7)
    digitcounts= Dict{Int, Int}(1 => 1)
    while n < BigInt(10)^(maxdigits+1)
        n = min(next2, next3, next5, next7)
        push!(humble, n)
        count += 1
        if count == returnsequencelength
            savesequence = deepcopy(humble[1:returnsequencelength])
        elseif count > 2000000
            popfirst!(humble)
            adjustindex += 1
        end
        placesbase10 = length(string(n))
        if haskey(digitcounts, placesbase10)
            digitcounts[placesbase10] += 1
        else
            digitcounts[placesbase10] = 1
        end
        maxdiff = max(maxdiff, count - base2, count - base3, count - base5, count - base7)
        (next2 <= n) && (next2 = 2 * humble[(base2 += 1) - adjustindex])
        (next3 <= n) && (next3 = 3 * humble[(base3 += 1) - adjustindex])
        (next5 <= n) && (next5 = 5 * humble[(base5 += 1) - adjustindex])
        (next7 <= n) && (next7 = 7 * humble[(base7 += 1) - adjustindex])
    end
    savesequence, digitcounts, count, maxdiff
end

counthumbledigits(3)

@time first120, digitcounts, count, maxdiff = counthumbledigits(99)

println("\nTotal humble numbers counted: $count")
println("Maximum depth between top of array and a multiplier: $maxdiff\n")

println("The first 50 humble numbers are: $first120\n\nDigit counts of humble numbers:")
for ndigits in sort(collect(keys(digitcounts)))[1:end-1]
    println(lpad(digitcounts[ndigits], 10), " have ", lpad(ndigits, 3), " digits.")
end

```
{{out}}

```txt

828.693164 seconds (3.61 G allocations: 64.351 GiB, 51.37% gc time)

Total humble numbers counted: 51428827
Maximum depth between top of array and a multiplier: 1697189

The first 50 humble numbers are: BigInt[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 24, 25, 27, 28, 30, 32, 35, 36, 40, 42, 45, 48, 49, 50, 54, 56, 60, 63, 64, 70, 72, 75, 80, 81, 84, 90, 96, 98, 100, 105, 108, 112, 120]

Digit counts of humble numbers:
         9 have   1 digits.
        36 have   2 digits.
        95 have   3 digits.
       197 have   4 digits.
       356 have   5 digits.
       579 have   6 digits.
       882 have   7 digits.
      1272 have   8 digits.
      1767 have   9 digits.
      2381 have  10 digits.
      3113 have  11 digits.
      3984 have  12 digits.
      5002 have  13 digits.
      6187 have  14 digits.
      7545 have  15 digits.
      9081 have  16 digits.
     10815 have  17 digits.
     12759 have  18 digits.
     14927 have  19 digits.
     17323 have  20 digits.
     19960 have  21 digits.
     22853 have  22 digits.
     26015 have  23 digits.
     29458 have  24 digits.
     33188 have  25 digits.
     37222 have  26 digits.
     41568 have  27 digits.
     46245 have  28 digits.
     51254 have  29 digits.
     56618 have  30 digits.
     62338 have  31 digits.
     68437 have  32 digits.
     74917 have  33 digits.
     81793 have  34 digits.
     89083 have  35 digits.
     96786 have  36 digits.
    104926 have  37 digits.
    113511 have  38 digits.
    122546 have  39 digits.
    132054 have  40 digits.
    142038 have  41 digits.
    152515 have  42 digits.
    163497 have  43 digits.
    174986 have  44 digits.
    187004 have  45 digits.
    199565 have  46 digits.
    212675 have  47 digits.
    226346 have  48 digits.
    240590 have  49 digits.
    255415 have  50 digits.
    270843 have  51 digits.
    286880 have  52 digits.
    303533 have  53 digits.
    320821 have  54 digits.
    338750 have  55 digits.
    357343 have  56 digits.
    376599 have  57 digits.
    396533 have  58 digits.
    417160 have  59 digits.
    438492 have  60 digits.
    460533 have  61 digits.
    483307 have  62 digits.
    506820 have  63 digits.
    531076 have  64 digits.
    556104 have  65 digits.
    581902 have  66 digits.
    608483 have  67 digits.
    635864 have  68 digits.
    664053 have  69 digits.
    693065 have  70 digits.
    722911 have  71 digits.
    753593 have  72 digits.
    785141 have  73 digits.
    817554 have  74 digits.
    850847 have  75 digits.
    885037 have  76 digits.
    920120 have  77 digits.
    956120 have  78 digits.
    993058 have  79 digits.
   1030928 have  80 digits.
   1069748 have  81 digits.
   1109528 have  82 digits.
   1150287 have  83 digits.
   1192035 have  84 digits.
   1234774 have  85 digits.
   1278527 have  86 digits.
   1323301 have  87 digits.
   1369106 have  88 digits.
   1415956 have  89 digits.
   1463862 have  90 digits.
   1512840 have  91 digits.
   1562897 have  92 digits.
   1614050 have  93 digits.
   1666302 have  94 digits.
   1719669 have  95 digits.
   1774166 have  96 digits.
   1829805 have  97 digits.
   1886590 have  98 digits.
   1944540 have  99 digits.
   2003661 have 100 digits.

```



## Kotlin


```scala
fun isHumble(i: Int): Boolean {
    if (i <= 1) return true
    if (i % 2 == 0) return isHumble(i / 2)
    if (i % 3 == 0) return isHumble(i / 3)
    if (i % 5 == 0) return isHumble(i / 5)
    if (i % 7 == 0) return isHumble(i / 7)
    return false
}

fun main() {
    val limit: Int = Short.MAX_VALUE.toInt()
    val humble = mutableMapOf<Int, Int>()
    var count = 0
    var num = 1

    while (count < limit) {
        if (isHumble(num)) {
            val str = num.toString()
            val len = str.length
            humble.merge(len, 1) { a, b -> a + b }

            if (count < 50) print("$num ")
            count++
        }
        num++
    }
    println("\n")

    println("Of the first $count humble numbers:")
    num = 1
    while (num < humble.size - 1) {
        if (humble.containsKey(num)) {
            val c = humble[num]
            println("%5d have %2d digits".format(c, num))
            num++
        } else {
            break
        }
    }
}
```

{{out}}

```txt
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

Of the first 32767 humble numbers:
    9 have  1 digits
   36 have  2 digits
   95 have  3 digits
  197 have  4 digits
  356 have  5 digits
  579 have  6 digits
  882 have  7 digits
 1272 have  8 digits
 1767 have  9 digits
```



## Perl


```perl
use strict;
use warnings;
use List::Util 'min';

#use bigint     # works, but slow
use Math::GMPz; # this module gives roughly 16x speed-up

sub humble_gen {
    my @s = ([1], [1], [1], [1]);
    my @m = (2, 3, 5, 7);
    @m = map { Math::GMPz->new($_) } @m; # comment out to NOT use Math::GMPz

    return sub {
    my $n = min $s[0][0], $s[1][0], $s[2][0], $s[3][0];
    for (0..3) {
            shift @{$s[$_]} if $s[$_][0] == $n;
            push @{$s[$_]}, $n * $m[$_]
        }
        return $n
    }
}

my $h = humble_gen;
my $i = 0;
my $upto = 50;

my $list;
++$i, $list .= $h->(). " " until $i == $upto;
print "$list\n";

$h = humble_gen; # from the top...
my $count  = 0;
my $digits = 1;

while ($digits <= $upto) {
    ++$count and next if $digits == length $h->();
    printf "Digits: %2d - Count: %s\n", $digits++, $count;
    $count = 1;
}
```

{{out}}
<pre style="height:20ex">1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

Digits:  1 - Count: 9
Digits:  2 - Count: 36
Digits:  3 - Count: 95
Digits:  4 - Count: 197
Digits:  5 - Count: 356
Digits:  6 - Count: 579
Digits:  7 - Count: 882
Digits:  8 - Count: 1272
Digits:  9 - Count: 1767
Digits: 10 - Count: 2381
Digits: 11 - Count: 3113
Digits: 12 - Count: 3984
Digits: 13 - Count: 5002
Digits: 14 - Count: 6187
Digits: 15 - Count: 7545
Digits: 16 - Count: 9081
Digits: 17 - Count: 10815
Digits: 18 - Count: 12759
Digits: 19 - Count: 14927
Digits: 20 - Count: 17323
Digits: 21 - Count: 19960
Digits: 22 - Count: 22853
Digits: 23 - Count: 26015
Digits: 24 - Count: 29458
Digits: 25 - Count: 33188
Digits: 26 - Count: 37222
Digits: 27 - Count: 41568
Digits: 28 - Count: 46245
Digits: 29 - Count: 51254
Digits: 30 - Count: 56618
Digits: 31 - Count: 62338
Digits: 32 - Count: 68437
Digits: 33 - Count: 74917
Digits: 34 - Count: 81793
Digits: 35 - Count: 89083
Digits: 36 - Count: 96786
Digits: 37 - Count: 104926
Digits: 38 - Count: 113511
Digits: 39 - Count: 122546
Digits: 40 - Count: 132054
Digits: 41 - Count: 142038
Digits: 42 - Count: 152515
Digits: 43 - Count: 163497
Digits: 44 - Count: 174986
Digits: 45 - Count: 187004
Digits: 46 - Count: 199565
Digits: 47 - Count: 212675
Digits: 48 - Count: 226346
Digits: 49 - Count: 240590
Digits: 50 - Count: 255415
```



## Perl 6

{{works with|Rakudo|2019.07.1}}


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

my $humble := smooth-numbers(2,3,5,7);

put $humble[^50];
say '';

my $upto = 50;
my $digits = 1;
my $count;

$humble.map: -> \h {
    ++$count and next if h.chars == $digits;
    printf "Digits: %2d - Count: %s\n", $digits++, $count;
    $count = 1;
    last if $digits > $upto;
}
```

{{out}}

```txt
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

Digits:  1 - Count: 9
Digits:  2 - Count: 36
Digits:  3 - Count: 95
Digits:  4 - Count: 197
Digits:  5 - Count: 356
Digits:  6 - Count: 579
Digits:  7 - Count: 882
Digits:  8 - Count: 1272
Digits:  9 - Count: 1767
Digits: 10 - Count: 2381
Digits: 11 - Count: 3113
Digits: 12 - Count: 3984
Digits: 13 - Count: 5002
Digits: 14 - Count: 6187
Digits: 15 - Count: 7545
Digits: 16 - Count: 9081
Digits: 17 - Count: 10815
Digits: 18 - Count: 12759
Digits: 19 - Count: 14927
Digits: 20 - Count: 17323
Digits: 21 - Count: 19960
Digits: 22 - Count: 22853
Digits: 23 - Count: 26015
Digits: 24 - Count: 29458
Digits: 25 - Count: 33188
Digits: 26 - Count: 37222
Digits: 27 - Count: 41568
Digits: 28 - Count: 46245
Digits: 29 - Count: 51254
Digits: 30 - Count: 56618
Digits: 31 - Count: 62338
Digits: 32 - Count: 68437
Digits: 33 - Count: 74917
Digits: 34 - Count: 81793
Digits: 35 - Count: 89083
Digits: 36 - Count: 96786
Digits: 37 - Count: 104926
Digits: 38 - Count: 113511
Digits: 39 - Count: 122546
Digits: 40 - Count: 132054
Digits: 41 - Count: 142038
Digits: 42 - Count: 152515
Digits: 43 - Count: 163497
Digits: 44 - Count: 174986
Digits: 45 - Count: 187004
Digits: 46 - Count: 199565
Digits: 47 - Count: 212675
Digits: 48 - Count: 226346
Digits: 49 - Count: 240590
Digits: 50 - Count: 255415
```



## Phix

{{libheader|mpfr}}
{{trans|Julia}}
I felt pretty good about the performance of this, until I ran the Go version - humbled indeed!

It will go all the way to 100 digits if you give it time (18 mins, on 64bit - 32bit runs out of memory after printing the 99th line)

I also tried a log version (similar to [[Hamming_numbers#A_much_faster_logarithmic_version|Hamming_numbers]]) but inaccuracies with floor(h[n][LOG]) crept in quite early, at just 10 digits.

```Phix
-- demo/rosetta/humble.exw
include mpfr.e

procedure humble(integer n, bool countdigits=false)
-- if countdigits is false: show first n humble numbers,
-- if countdigits is true: count them up to n digits.
    sequence humble = {mpz_init(1)},
             nexts = {2,3,5,7},
             indices = repeat(1,4)
    for i=1 to 4 do nexts[i] = mpz_init(nexts[i]) end for
    integer digits = 1,
            count = 1,
            dead = 1,
            tc = 0
    atom t0 = time()
    mpz p10 = mpz_init(10)
    while ((not countdigits) and length(humble)<n)
       or (countdigits and digits<=n) do
        mpz x = mpz_init_set(mpz_min(nexts))
        humble = append(humble,x)
        if countdigits then
            if mpz_cmp(x,p10)>=0 then
                mpz_mul_si(p10,p10,10)
                integer d = min(indices)
                for k=dead to d-1 do
                    humble[k] = mpz_free(humble[k])
                end for
                dead = d
                string s = iff(digits=1?"":"s"),
                       e = elapsed(time()-t0)
                tc += count
--              e &= sprintf(", %,d dead",{dead-1})
                e &= sprintf(", total:%,d",{tc})
                printf(1,"%,12d humble numbers have %d digit%s (%s)\n",{count,digits,s,e})
                digits += 1
                count = 1
            else
                count += 1
            end if
        end if
        for j=1 to 4 do
            if mpz_cmp(nexts[j],x)<=0 then
                indices[j] += 1
                mpz_mul_si(nexts[j],humble[indices[j]],get_prime(j))
            end if
        end for
    end while
    if not countdigits then
        for i=1 to length(humble) do
            humble[i] = shorten(mpz_get_str(humble[i]),ml:=10)
        end for
        printf(1,"First %d humble numbers: %s\n\n",{n,join(humble," ")})
    end if
end procedure

humble(50)
humble(42,true)
```

{{out}}

```txt

First 50 humble numbers: 1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

           9 humble numbers have 1 digit (0s, total:9)
          36 humble numbers have 2 digits (0s, total:45)
          95 humble numbers have 3 digits (0s, total:140)
         197 humble numbers have 4 digits (0s, total:337)
         356 humble numbers have 5 digits (0s, total:693)
         579 humble numbers have 6 digits (0.0s, total:1,272)
         882 humble numbers have 7 digits (0.0s, total:2,154)
       1,272 humble numbers have 8 digits (0.0s, total:3,426)
       1,767 humble numbers have 9 digits (0.1s, total:5,193)
       2,381 humble numbers have 10 digits (0.1s, total:7,574)
       3,113 humble numbers have 11 digits (0.2s, total:10,687)
       3,984 humble numbers have 12 digits (0.2s, total:14,671)
       5,002 humble numbers have 13 digits (0.3s, total:19,673)
       6,187 humble numbers have 14 digits (0.4s, total:25,860)
       7,545 humble numbers have 15 digits (0.5s, total:33,405)
       9,081 humble numbers have 16 digits (0.6s, total:42,486)
      10,815 humble numbers have 17 digits (0.8s, total:53,301)
      12,759 humble numbers have 18 digits (0.9s, total:66,060)
      14,927 humble numbers have 19 digits (1.2s, total:80,987)
      17,323 humble numbers have 20 digits (1.4s, total:98,310)
      19,960 humble numbers have 21 digits (1.7s, total:118,270)
      22,853 humble numbers have 22 digits (2.0s, total:141,123)
      26,015 humble numbers have 23 digits (2.3s, total:167,138)
      29,458 humble numbers have 24 digits (2.7s, total:196,596)
      33,188 humble numbers have 25 digits (3.2s, total:229,784)
      37,222 humble numbers have 26 digits (3.7s, total:267,006)
      41,568 humble numbers have 27 digits (4.3s, total:308,574)
      46,245 humble numbers have 28 digits (5.0s, total:354,819)
      51,254 humble numbers have 29 digits (5.7s, total:406,073)
      56,618 humble numbers have 30 digits (6.5s, total:462,691)
      62,338 humble numbers have 31 digits (7.3s, total:525,029)
      68,437 humble numbers have 32 digits (8.3s, total:593,466)
      74,917 humble numbers have 33 digits (9.4s, total:668,383)
      81,793 humble numbers have 34 digits (10.5s, total:750,176)
      89,083 humble numbers have 35 digits (11.7s, total:839,259)
      96,786 humble numbers have 36 digits (13.1s, total:936,045)
     104,926 humble numbers have 37 digits (14.6s, total:1,040,971)
     113,511 humble numbers have 38 digits (16.2s, total:1,154,482)
     122,546 humble numbers have 39 digits (17.9s, total:1,277,028)
     132,054 humble numbers have 40 digits (19.7s, total:1,409,082)
     142,038 humble numbers have 41 digits (21.7s, total:1,551,120)
     152,515 humble numbers have 42 digits (23.9s, total:1,703,635)

```



## Racket


{{trans|Go}}


```racket
#lang racket

(define (gen-humble-numbers N (kons #f) (k0 (void)))
  (define rv (make-vector N 1))

  (define (loop n 2-idx 3-idx 5-idx 7-idx next-2 next-3 next-5 next-7 k)
    (if (= n N)
        rv
        (let ((mn (min next-2 next-3 next-5 next-7)))
          (vector-set! rv n mn)
          (define (add-1-if-min n x) (if (= mn n) (add1 x) x))
          (define (*vr.i-if-min n m i) (if (= mn n) (* m (vector-ref rv i)) n))
          (let* ((2-idx  (add-1-if-min next-2 2-idx))
                 (next-2 (*vr.i-if-min next-2 2 2-idx))
                 (3-idx  (add-1-if-min next-3 3-idx))
                 (next-3 (*vr.i-if-min next-3 3 3-idx))
                 (5-idx  (add-1-if-min next-5 5-idx))
                 (next-5 (*vr.i-if-min next-5 5 5-idx))
                 (7-idx  (add-1-if-min next-7 7-idx))
                 (next-7 (*vr.i-if-min next-7 7 7-idx))
                 (k (and kons (kons mn k))))
            (loop (add1 n) 2-idx 3-idx 5-idx 7-idx next-2 next-3 next-5 next-7 k)))))
  (loop 1 0 0 0 0 2 3 5 7 (and kons (kons 1 k0))))

(define ((digit-tracker breaker) h last-ten.count)
  (let ((last-ten (car last-ten.count)))
    (if (< h last-ten)
        (cons last-ten (add1 (cdr last-ten.count)))
        (begin
          (printf "~a humble numbers with ~a digits~%" (cdr last-ten.count) (order-of-magnitude last-ten))
          (cons (breaker (* 10 last-ten)) 1)))))

(define (Humble-numbers)
  (displayln (gen-humble-numbers 50))
  (time
   (let/ec break
     (void (gen-humble-numbers
            100000000
            (digit-tracker (λ (o) (if (> (order-of-magnitude o) 100) (break) o)))
            '(10 . 0))))))

(module+ main
  (Humble-numbers))

```


{{out}}
output has been elided manually, to avoid repetition with the numbers you've already seen elsewhere:

```txt
#(1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120)
9 humble numbers with 1 digits
36 humble numbers with 2 digits
95 humble numbers with 3 digits
197 humble numbers with 4 digits
356 humble numbers with 5 digits
579 humble numbers with 6 digits
882 humble numbers with 7 digits
1272 humble numbers with 8 digits
1767 humble numbers with 9 digits
2381 humble numbers with 10 digits
...
17323 humble numbers with 20 digits
...
56618 humble numbers with 30 digits
...
132054 humble numbers with 40 digits
...
255415 humble numbers with 50 digits
...
438492 humble numbers with 60 digits
...
693065 humble numbers with 70 digits
...
1030928 humble numbers with 80 digits
...
1463862 humble numbers with 90 digits
...
2003661 humble numbers with 100 digits
cpu time: 234970 real time: 235489 gc time: 189187

```



## REXX


```rexx
/*REXX program computes and displays humble numbers,  also will display counts of sizes.*/
parse arg n m .                                  /*obtain optional arguments from the CL*/
if n=='' | n==","  then n= 50                    /*Not specified?  Then use the default.*/
if m=='' | m==","  then m= 60                    /* "      "         "   "   "     "    */
numeric digits 1 + max(20, m)                    /*be able to handle some big numbers.  */
$.= 0                                            /*a count array for  X  digit humble #s*/
call humble n;                    list=          /*call HUMBLE sub; initialize the list.*/
                  do j=1  for n;  list= list @.j /*append a  humble  number to the list.*/
                  end   /*j*/

if list\=''  then do;    say "A list of the first "    n    ' humble numbers are:'
                         say strip(list)         /*elide the leading blank in the list. */
                  end
say
call humble -m                                   /*invoke subroutine for counting nums. */
if $.1==0  then exit                             /*if no counts, then we're all finished*/
total= 0                                         /*initialize count of humble numbers.  */
$.1= $.1 + 1                                     /*adjust count for absent 1st humble #.*/
say '                    The digit counts of humble numbers:'
say '                 ═════════════════════════════════════════'
        do c=1  while $.c>0;  s= left('s', length($.c)>1)   /*count needs pluralization?*/
        say right( commas($.c), 30)         ' have '         right(c, 2)         " digit"s
        total= total + $.c                       /* ◄─────────────────────────────────┐ */
        end   /*k*/                              /*bump humble number count (so far)──┘ */
/*REXX program computes and displays humble numbers, also will display a count of sizes.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure; arg _;  do i=length(_)-3 to 1 by -3; _=insert(',', _, i); end; return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
humble: procedure expose @. $.;   parse arg x;         if x==0  then return
        y= abs(x);   a= y;        noCount= x>0;        if x<0   then y= 999999999
        #2= 1;    #3= 1;    #5= 1;     #7= 1     /*define the initial humble constants. */
                  $.= 0;    @.= 0;    @.1= 1     /*initialize counts and humble numbers.*/
          do h=2  for y-1
          @.h= min(2*@.#2,3*@.#3,5*@.#5,7*@.#7)  /*pick the minimum of 4 humble numbers.*/
          m= @.h                                 /*M:    "     "     " "    "      "    */
          if 2*@.#2 == m   then #2 = #2 + 1      /*Is number already defined? Use next #*/
          if 3*@.#3 == m   then #3 = #3 + 1      /* "    "      "       "      "    "  "*/
          if 5*@.#5 == m   then #5 = #5 + 1      /* "    "      "       "      "    "  "*/
          if 7*@.#7 == m   then #7 = #7 + 1      /* "    "      "       "      "    "  "*/
          if noCount       then iterate          /*Not counting digits?   Then iterate. */
          L= length(m);    if L>a  then leave    /*Are we done with counting?  Then quit*/
          $.L= $.L + 1                           /*bump the digit count for this number.*/
          end   /*h*/                            /*the humble numbers are in the @ array*/
        return                                   /* "  count  results  "   "  "  $   "  */
```

{{out|output|text=  when using the default inputs:}}

```txt

A list of the first  50  humble numbers are:
1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

                    The digit counts of humble numbers:
                 ═════════════════════════════════════════
                             9  have   1  digit
                            36  have   2  digits
                            95  have   3  digits
                           197  have   4  digits
                           356  have   5  digits
                           579  have   6  digits
                           882  have   7  digits
                         1,272  have   8  digits
                         1,767  have   9  digits
                         2,381  have  10  digits
                         3,113  have  11  digits
                         3,984  have  12  digits
                         5,002  have  13  digits
                         6,187  have  14  digits
                         7,545  have  15  digits
                         9,081  have  16  digits
                        10,815  have  17  digits
                        12,759  have  18  digits
                        14,927  have  19  digits
                        17,323  have  20  digits
                        19,960  have  21  digits
                        22,853  have  22  digits
                        26,015  have  23  digits
                        29,458  have  24  digits
                        33,188  have  25  digits
                        37,222  have  26  digits
                        41,568  have  27  digits
                        46,245  have  28  digits
                        51,254  have  29  digits
                        56,618  have  30  digits
                        62,338  have  31  digits
                        68,437  have  32  digits
                        74,917  have  33  digits
                        81,793  have  34  digits
                        89,083  have  35  digits
                        96,786  have  36  digits
                       104,926  have  37  digits
                       113,511  have  38  digits
                       122,546  have  39  digits
                       132,054  have  40  digits
                       142,038  have  41  digits
                       152,515  have  42  digits
                       163,497  have  43  digits
                       174,986  have  44  digits
                       187,004  have  45  digits
                       199,565  have  46  digits
                       212,675  have  47  digits
                       226,346  have  48  digits
                       240,590  have  49  digits
                       255,415  have  50  digits
                       270,843  have  51  digits
                       286,880  have  52  digits
                       303,533  have  53  digits
                       320,821  have  54  digits
                       338,750  have  55  digits
                       357,343  have  56  digits
                       376,599  have  57  digits
                       396,533  have  58  digits
                       417,160  have  59  digits
                       438,492  have  60  digits

total number of humble numbers found:  6,870,667

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

with (smooth_generator([2,3,5,7])) {|g|
    say 50.of { g.run }.join(' ')
}

say "\nThe digit counts of humble numbers"
say '═'*35

with (smooth_generator([2,3,5,7])) {|g|
    for (var(d=1,c=0); d <= 20; ++c) {
        var n = g.run
        n.len > d || next
        say "#{'%10s'%c.commify}  have  #{'%2d'%d}  digit#{[:s,''][d==1]}"
        (c, d) = (0, n.len)
    }
}
```

{{out}}

```txt

1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

The digit counts of humble numbers
═══════════════════════════════════
         9  have   1  digit
        36  have   2  digits
        95  have   3  digits
       197  have   4  digits
       356  have   5  digits
       579  have   6  digits
       882  have   7  digits
     1,272  have   8  digits
     1,767  have   9  digits
     2,381  have  10  digits
     3,113  have  11  digits
     3,984  have  12  digits
     5,002  have  13  digits
     6,187  have  14  digits
     7,545  have  15  digits
     9,081  have  16  digits
    10,815  have  17  digits
    12,759  have  18  digits
    14,927  have  19  digits
    17,323  have  20  digits

```



## zkl

{{trans|Go}}
{{libheader|GMP}} GNU Multiple Precision Arithmetic Library

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
var one   = BI(1), two   = BI(2), three = BI(3),
    five  = BI(5), seven = BI(7);

fcn humble(n){	// --> List of BigInt Humble numbers
   h:=List.createLong(n);  h.append(one);
   next2,next3 := two.copy(),  three.copy();
   next5,next7 := five.copy(), seven.copy();
   reg i=0,j=0,k=0,l=0;
   do(n-1){
      h.append( hm:=BI(next2.min(next3.min(next5.min(next7)))) );
      if(hm==next2) next2.set(two)  .mul(h[i+=1]);
      if(hm==next3) next3.set(three).mul(h[j+=1]);
      if(hm==next5) next5.set(five) .mul(h[k+=1]);
      if(hm==next7) next7.set(seven).mul(h[l+=1]);
   }
   h
}
```


```zkl
fcn __main__{
   const N = 5 * 1e6;  // calculate the first 1 million humble numbers, say
   h:=humble(N);
   println("The first 50 humble numbers are:\n  ",h[0,50].concat(" "));

   counts:=Dictionary();	// tally the number of digits in each number
   h.apply2('wrap(n){ counts.incV(n.numDigits) });

   println("\nOf the first %,d humble numbers:".fmt(h.len()));
   println("Digits   Count");
   foreach n in (counts.keys.apply("toInt").sort()){
      println("%2d  %,9d".fmt(n,counts[n], n));
   }
}
```

{{out}}
<pre style="height:45ex">
The first 50 humble numbers are:
  1 2 3 4 5 6 7 8 9 10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42 45 48 49 50 54 56 60 63 64 70 72 75 80 81 84 90 96 98 100 105 108 112 120

Of the first 5,000,000 humble numbers:
Digits   Count
 1          9
 2         36
 3         95
 4        197
 5        356
 6        579
 7        882
 8      1,272
 9      1,767
10      2,381
11      3,113
12      3,984
13      5,002
14      6,187
15      7,545
16      9,081
17     10,815
18     12,759
19     14,927
20     17,323
21     19,960
22     22,853
23     26,015
24     29,458
25     33,188
26     37,222
27     41,568
28     46,245
29     51,254
30     56,618
31     62,338
32     68,437
33     74,917
34     81,793
35     89,083
36     96,786
37    104,926
38    113,511
39    122,546
40    132,054
41    142,038
42    152,515
43    163,497
44    174,986
45    187,004
46    199,565
47    212,675
48    226,346
49    240,590
50    255,415
51    270,843
52    286,880
53    303,533
54    320,821
55    338,750
56    115,460

```

