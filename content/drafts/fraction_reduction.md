+++
title = "Fraction reduction"
description = ""
date = 2019-10-08T00:03:29Z
aliases = []
[extra]
id = 22506
[taxonomies]
categories = []
tags = []
+++

{{draft task|Puzzles}}

               ''There is a fine line between numerator and denominator.''       ''─── anonymous''



A method to   "reduce"   some reducible fractions is to   ''cross out''   <u>a</u> digit from the
numerator and the denominator.   An example is:
        <big>16</big>                                                  <big>1<b><strike>6</strike></b></big>
       ────     and then (simply) cross─out the sixes:      ────
        <big>64</big>                                                  <big><b><strike>6</strike></b>4</big>
resulting in:
         <big>1</big>
        ───
         <big>4</big>


Naturally,   this "method" of reduction must reduce to the proper value   (shown as a fraction).

This "method" is also known as   ''anomalous cancellation''   and also   ''accidental cancellation''.


(Of course,   this "method" shouldn't be taught to impressionable or gullible minds.)       <big><big><big> 😇 </big></big></big>



;Task:
Find and show some fractions that can be reduced by the above "method".
:*   show 2-digit fractions found   (like the example shown above)
:*   show 3-digit fractions
:*   show 4-digit fractions
:*   show 5-digit fractions   (and higher)       ''(optional)''
:*   show each (above) n-digit fractions separately from other different n-sized fractions, don't mix different "sizes" together
:*   for each "size" fraction,   only show a dozen examples   (the 1<sup>st</sup> twelve found)
:*   (it's recognized that not every programming solution will have the same generation algorithm)
:*   for each "size" fraction:
:::*   show a count of how many reducible fractions were found.   The example (above) is size '''2'''
:::*   show a count of which digits were crossed out   (one line for each different digit)
:*   for each "size" fraction,   show a count of how many were found.   The example (above) is size '''2'''
:*   show each n-digit example   (to be shown on one line):
:::*   show each n-digit fraction
:::*   show each reduced n-digit fraction
:::*   show what digit was crossed out for the numerator and the denominator



;Task requirements/restrictions:
:*   only proper fractions and their reductions   (the result)   are to be used   (no vulgar fractions)
:*   only positive fractions are to be used   (no negative signs anywhere)
:*   only base ten integers are to be used for the numerator and denominator
:*   no zeros   (decimal digit)   can be used within the numerator or the denominator
:*   the numerator and denominator should be composed of the same number of digits
:*   no digit can be repeated in the numerator
:*   no digit can be repeated in the denominator
:*   (naturally)   there should be a shared decimal digit in the numerator   ''and''   the denominator
:*   fractions can be shown as   16/64   (for example)


Show all output here, on this page.


;Somewhat related task:
:*   [https://rosettacode.org/wiki/Farey_sequence Farey sequence]       (It concerns fractions.)



;References:
:*   Wikipedia entry:   [https://en.wikipedia.org/wiki/Fraction_(mathematics)#Proper_and_improper_fractions proper and improper fractions].
:*   Wikipedia entry:   [https://en.wikipedia.org/wiki/Anomalous_cancellation anomalous cancellation and/or accidental cancellation].




## C#
{{trans|Kotlin}}

```c#
using System;

namespace FractionReduction {
    class Program {
        static int IndexOf(int n, int[] s) {
            for (int i = 0; i < s.Length; i++) {
                if (s[i] == n) {
                    return i;
                }
            }
            return -1;
        }

        static bool GetDigits(int n, int le, int[] digits) {
            while (n > 0) {
                var r = n % 10;
                if (r == 0 || IndexOf(r, digits) >= 0) {
                    return false;
                }
                le--;
                digits[le] = r;
                n /= 10;
            }
            return true;
        }

        static int RemoveDigit(int[] digits, int le, int idx) {
            int[] pows = { 1, 10, 100, 1000, 10000 };

            var sum = 0;
            var pow = pows[le - 2];
            for (int i = 0; i < le; i++) {
                if (i == idx) continue;
                sum += digits[i] * pow;
                pow /= 10;

            }
            return sum;
        }

        static void Main() {
            var lims = new int[,] { { 12, 97 }, { 123, 986 }, { 1234, 9875 }, { 12345, 98764 } };
            var count = new int[5];
            var omitted = new int[5, 10];
            var upperBound = lims.GetLength(0);
            for (int i = 0; i < upperBound; i++) {
                var nDigits = new int[i + 2];
                var dDigits = new int[i + 2];
                var blank = new int[i + 2];
                for (int n = lims[i, 0]; n <= lims[i, 1]; n++) {
                    blank.CopyTo(nDigits, 0);
                    var nOk = GetDigits(n, i + 2, nDigits);
                    if (!nOk) {
                        continue;
                    }
                    for (int d = n + 1; d <= lims[i, 1] + 1; d++) {
                        blank.CopyTo(dDigits, 0);
                        var dOk = GetDigits(d, i + 2, dDigits);
                        if (!dOk) {
                            continue;
                        }
                        for (int nix = 0; nix < nDigits.Length; nix++) {
                            var digit = nDigits[nix];
                            var dix = IndexOf(digit, dDigits);
                            if (dix >= 0) {
                                var rn = RemoveDigit(nDigits, i + 2, nix);
                                var rd = RemoveDigit(dDigits, i + 2, dix);
                                if ((double)n / d == (double)rn / rd) {
                                    count[i]++;
                                    omitted[i, digit]++;
                                    if (count[i] <= 12) {
                                        Console.WriteLine("{0}/{1} = {2}/{3} by omitting {4}'s", n, d, rn, rd, digit);
                                    }
                                }
                            }
                        }
                    }
                }
                Console.WriteLine();
            }

            for (int i = 2; i <= 5; i++) {
                Console.WriteLine("There are {0} {1}-digit fractions of which:", count[i - 2], i);
                for (int j = 1; j <= 9; j++) {
                    if (omitted[i - 2, j] == 0) {
                        continue;
                    }
                    Console.WriteLine("{0,6} have {1}'s omitted", omitted[i - 2, j], j);
                }
                Console.WriteLine();
            }
        }
    }
}
```

{{out}}

```txt
16/64 = 1/4 by omitting 6's
19/95 = 1/5 by omitting 9's
26/65 = 2/5 by omitting 6's
49/98 = 4/8 by omitting 9's

132/231 = 12/21 by omitting 3's
134/536 = 14/56 by omitting 3's
134/938 = 14/98 by omitting 3's
136/238 = 16/28 by omitting 3's
138/345 = 18/45 by omitting 3's
139/695 = 13/65 by omitting 9's
143/341 = 13/31 by omitting 4's
146/365 = 14/35 by omitting 6's
149/298 = 14/28 by omitting 9's
149/596 = 14/56 by omitting 9's
149/894 = 14/84 by omitting 9's
154/253 = 14/23 by omitting 5's

1234/4936 = 124/496 by omitting 3's
1239/6195 = 123/615 by omitting 9's
1246/3649 = 126/369 by omitting 4's
1249/2498 = 124/248 by omitting 9's
1259/6295 = 125/625 by omitting 9's
1279/6395 = 127/635 by omitting 9's
1283/5132 = 128/512 by omitting 3's
1297/2594 = 127/254 by omitting 9's
1297/3891 = 127/381 by omitting 9's
1298/2596 = 128/256 by omitting 9's
1298/3894 = 128/384 by omitting 9's
1298/5192 = 128/512 by omitting 9's

12349/24698 = 1234/2468 by omitting 9's
12356/67958 = 1236/6798 by omitting 5's
12358/14362 = 1258/1462 by omitting 3's
12358/15364 = 1258/1564 by omitting 3's
12358/17368 = 1258/1768 by omitting 3's
12358/19372 = 1258/1972 by omitting 3's
12358/21376 = 1258/2176 by omitting 3's
12358/25384 = 1258/2584 by omitting 3's
12359/61795 = 1235/6175 by omitting 9's
12364/32596 = 1364/3596 by omitting 2's
12379/61895 = 1237/6185 by omitting 9's
12386/32654 = 1386/3654 by omitting 2's

There are 4 2-digit fractions of which:
     2 have 6's omitted
     2 have 9's omitted

There are 122 3-digit fractions of which:
     9 have 3's omitted
     1 have 4's omitted
     6 have 5's omitted
    15 have 6's omitted
    16 have 7's omitted
    15 have 8's omitted
    60 have 9's omitted

There are 660 4-digit fractions of which:
    14 have 1's omitted
    25 have 2's omitted
    92 have 3's omitted
    14 have 4's omitted
    29 have 5's omitted
    63 have 6's omitted
    16 have 7's omitted
    17 have 8's omitted
   390 have 9's omitted

There are 5087 5-digit fractions of which:
    75 have 1's omitted
    40 have 2's omitted
   376 have 3's omitted
    78 have 4's omitted
   209 have 5's omitted
   379 have 6's omitted
   591 have 7's omitted
   351 have 8's omitted
  2988 have 9's omitted
```



## Go


### Version 1

This produces the stats for 5-digit fractions in less than 25 seconds but takes a much longer 15.5 minutes to process the 6-digit case. Timings are for an Intel Core i7-8565U machine.

```go
package main

import (
    "fmt"
    "time"
)

func indexOf(n int, s []int) int {
    for i, j := range s {
        if n == j {
            return i
        }
    }
    return -1
}

func getDigits(n, le int, digits []int) bool {
    for n > 0 {
        r := n % 10
        if r == 0 || indexOf(r, digits) >= 0 {
            return false
        }
        le--
        digits[le] = r
        n /= 10
    }
    return true
}

var pows = [5]int{1, 10, 100, 1000, 10000}

func removeDigit(digits []int, le, idx int) int {
    sum := 0
    pow := pows[le-2]
    for i := 0; i < le; i++ {
        if i == idx {
            continue
        }
        sum += digits[i] * pow
        pow /= 10
    }
    return sum
}

func main() {
    start := time.Now()
    lims := [5][2]int{
        {12, 97},
        {123, 986},
        {1234, 9875},
        {12345, 98764},
        {123456, 987653},
    }
    var count [5]int
    var omitted [5][10]int
    for i, lim := range lims {
        nDigits := make([]int, i+2)
        dDigits := make([]int, i+2)
        blank := make([]int, i+2)
        for n := lim[0]; n <= lim[1]; n++ {
            copy(nDigits, blank)
            nOk := getDigits(n, i+2, nDigits)
            if !nOk {
                continue
            }
            for d := n + 1; d <= lim[1]+1; d++ {
                copy(dDigits, blank)
                dOk := getDigits(d, i+2, dDigits)
                if !dOk {
                    continue
                }
                for nix, digit := range nDigits {
                    if dix := indexOf(digit, dDigits); dix >= 0 {
                        rn := removeDigit(nDigits, i+2, nix)
                        rd := removeDigit(dDigits, i+2, dix)
                        if float64(n)/float64(d) == float64(rn)/float64(rd) {
                            count[i]++
                            omitted[i][digit]++
                            if count[i] <= 12 {
                                fmt.Printf("%d/%d = %d/%d by omitting %d's\n", n, d, rn, rd, digit)
                            }
                        }
                    }
                }
            }
        }
        fmt.Println()
    }

    for i := 2; i <= 6; i++ {
        fmt.Printf("There are %d %d-digit fractions of which:\n", count[i-2], i)
        for j := 1; j <= 9; j++ {
            if omitted[i-2][j] == 0 {
                continue
            }
            fmt.Printf("%6d have %d's omitted\n", omitted[i-2][j], j)
        }
        fmt.Println()
    }
    fmt.Printf("Took %s\n", time.Since(start))
}
```


{{out}}

```txt

16/64 = 1/4 by omitting 6's
19/95 = 1/5 by omitting 9's
26/65 = 2/5 by omitting 6's
49/98 = 4/8 by omitting 9's

132/231 = 12/21 by omitting 3's
134/536 = 14/56 by omitting 3's
134/938 = 14/98 by omitting 3's
136/238 = 16/28 by omitting 3's
138/345 = 18/45 by omitting 3's
139/695 = 13/65 by omitting 9's
143/341 = 13/31 by omitting 4's
146/365 = 14/35 by omitting 6's
149/298 = 14/28 by omitting 9's
149/596 = 14/56 by omitting 9's
149/894 = 14/84 by omitting 9's
154/253 = 14/23 by omitting 5's

1234/4936 = 124/496 by omitting 3's
1239/6195 = 123/615 by omitting 9's
1246/3649 = 126/369 by omitting 4's
1249/2498 = 124/248 by omitting 9's
1259/6295 = 125/625 by omitting 9's
1279/6395 = 127/635 by omitting 9's
1283/5132 = 128/512 by omitting 3's
1297/2594 = 127/254 by omitting 9's
1297/3891 = 127/381 by omitting 9's
1298/2596 = 128/256 by omitting 9's
1298/3894 = 128/384 by omitting 9's
1298/5192 = 128/512 by omitting 9's

12349/24698 = 1234/2468 by omitting 9's
12356/67958 = 1236/6798 by omitting 5's
12358/14362 = 1258/1462 by omitting 3's
12358/15364 = 1258/1564 by omitting 3's
12358/17368 = 1258/1768 by omitting 3's
12358/19372 = 1258/1972 by omitting 3's
12358/21376 = 1258/2176 by omitting 3's
12358/25384 = 1258/2584 by omitting 3's
12359/61795 = 1235/6175 by omitting 9's
12364/32596 = 1364/3596 by omitting 2's
12379/61895 = 1237/6185 by omitting 9's
12386/32654 = 1386/3654 by omitting 2's

123459/617295 = 12345/61725 by omitting 9's
123468/493872 = 12468/49872 by omitting 3's
123469/173524 = 12469/17524 by omitting 3's
123469/193546 = 12469/19546 by omitting 3's
123469/213568 = 12469/21568 by omitting 3's
123469/283645 = 12469/28645 by omitting 3's
123469/493876 = 12469/49876 by omitting 3's
123469/573964 = 12469/57964 by omitting 3's
123479/617395 = 12347/61735 by omitting 9's
123495/172893 = 12345/17283 by omitting 9's
123548/679514 = 12348/67914 by omitting 5's
123574/325786 = 13574/35786 by omitting 2's

There are 4 2-digit fractions of which:
     2 have 6's omitted
     2 have 9's omitted

There are 122 3-digit fractions of which:
     9 have 3's omitted
     1 have 4's omitted
     6 have 5's omitted
    15 have 6's omitted
    16 have 7's omitted
    15 have 8's omitted
    60 have 9's omitted

There are 660 4-digit fractions of which:
    14 have 1's omitted
    25 have 2's omitted
    92 have 3's omitted
    14 have 4's omitted
    29 have 5's omitted
    63 have 6's omitted
    16 have 7's omitted
    17 have 8's omitted
   390 have 9's omitted

There are 5087 5-digit fractions of which:
    75 have 1's omitted
    40 have 2's omitted
   376 have 3's omitted
    78 have 4's omitted
   209 have 5's omitted
   379 have 6's omitted
   591 have 7's omitted
   351 have 8's omitted
  2988 have 9's omitted

There are 9778 6-digit fractions of which:
   230 have 1's omitted
   256 have 2's omitted
   921 have 3's omitted
   186 have 4's omitted
   317 have 5's omitted
   751 have 6's omitted
   262 have 7's omitted
   205 have 8's omitted
  6650 have 9's omitted

Took 15m38.231915709s

```



### Version 2

{{trans|Phix}}
Rather than iterate through all numbers in the n-digit range and check if they contain unique non-zero digits, this generates all such numbers to start with which turns out to be a much more efficient approach - more than 20 times faster than before.

```go
package main

import (
    "fmt"
    "time"
)

type result struct {
    n    int
    nine [9]int
}

func indexOf(n int, s []int) int {
    for i, j := range s {
        if n == j {
            return i
        }
    }
    return -1
}

func bIndexOf(b bool, s []bool) int {
    for i, j := range s {
        if b == j {
            return i
        }
    }
    return -1
}

func toNumber(digits []int, removeDigit int) int {
    digits2 := digits
    if removeDigit != 0 {
        digits2 = make([]int, len(digits))
        copy(digits2, digits)
        d := indexOf(removeDigit, digits2)
        copy(digits2[d:], digits2[d+1:])
        digits2[len(digits2)-1] = 0
        digits2 = digits2[:len(digits2)-1]
    }
    res := digits2[0]
    for i := 1; i < len(digits2); i++ {
        res = res*10 + digits2[i]
    }
    return res
}

func nDigits(n int) []result {
    var res []result
    digits := make([]int, n)
    var used [9]bool
    for i := 0; i < n; i++ {
        digits[i] = i + 1
        used[i] = true
    }
    for {
        var nine [9]int
        for i := 0; i < len(used); i++ {
            if used[i] {
                nine[i] = toNumber(digits, i+1)
            }
        }
        res = append(res, result{toNumber(digits, 0), nine})
        found := false
        for i := n - 1; i >= 0; i-- {
            d := digits[i]
            if !used[d-1] {
                panic("something went wrong with 'used' array")
            }
            used[d-1] = false
            for j := d; j < 9; j++ {
                if !used[j] {
                    used[j] = true
                    digits[i] = j + 1
                    for k := i + 1; k < n; k++ {
                        digits[k] = bIndexOf(false, used[:]) + 1
                        used[digits[k]-1] = true
                    }
                    found = true
                    break
                }
            }
            if found {
                break
            }
        }
        if !found {
            break
        }
    }
    return res
}

func main() {
    start := time.Now()
    for n := 2; n <= 5; n++ {
        rs := nDigits(n)
        count := 0
        var omitted [9]int
        for i := 0; i < len(rs)-1; i++ {
            xn, rn := rs[i].n, rs[i].nine
            for j := i + 1; j < len(rs); j++ {
                xd, rd := rs[j].n, rs[j].nine
                for k := 0; k < 9; k++ {
                    yn, yd := rn[k], rd[k]
                    if yn != 0 && yd != 0 &&
                        float64(xn)/float64(xd) == float64(yn)/float64(yd) {
                        count++
                        omitted[k]++
                        if count <= 12 {
                            fmt.Printf("%d/%d => %d/%d (removed %d)\n", xn, xd, yn, yd, k+1)
                        }
                    }
                }
            }
        }
        fmt.Printf("%d-digit fractions found:%d, omitted %v\n\n", n, count, omitted)
    }
    fmt.Printf("Took %s\n", time.Since(start))
}
```


{{out}}

```txt

16/64 => 1/4 (removed 6)
19/95 => 1/5 (removed 9)
26/65 => 2/5 (removed 6)
49/98 => 4/8 (removed 9)
2-digit fractions found:4, omitted [0 0 0 0 0 2 0 0 2]

132/231 => 12/21 (removed 3)
134/536 => 14/56 (removed 3)
134/938 => 14/98 (removed 3)
136/238 => 16/28 (removed 3)
138/345 => 18/45 (removed 3)
139/695 => 13/65 (removed 9)
143/341 => 13/31 (removed 4)
146/365 => 14/35 (removed 6)
149/298 => 14/28 (removed 9)
149/596 => 14/56 (removed 9)
149/894 => 14/84 (removed 9)
154/253 => 14/23 (removed 5)
3-digit fractions found:122, omitted [0 0 9 1 6 15 16 15 60]

1234/4936 => 124/496 (removed 3)
1239/6195 => 123/615 (removed 9)
1246/3649 => 126/369 (removed 4)
1249/2498 => 124/248 (removed 9)
1259/6295 => 125/625 (removed 9)
1279/6395 => 127/635 (removed 9)
1283/5132 => 128/512 (removed 3)
1297/2594 => 127/254 (removed 9)
1297/3891 => 127/381 (removed 9)
1298/2596 => 128/256 (removed 9)
1298/3894 => 128/384 (removed 9)
1298/5192 => 128/512 (removed 9)
4-digit fractions found:660, omitted [14 25 92 14 29 63 16 17 390]

12349/24698 => 1234/2468 (removed 9)
12356/67958 => 1236/6798 (removed 5)
12358/14362 => 1258/1462 (removed 3)
12358/15364 => 1258/1564 (removed 3)
12358/17368 => 1258/1768 (removed 3)
12358/19372 => 1258/1972 (removed 3)
12358/21376 => 1258/2176 (removed 3)
12358/25384 => 1258/2584 (removed 3)
12359/61795 => 1235/6175 (removed 9)
12364/32596 => 1364/3596 (removed 2)
12379/61895 => 1237/6185 (removed 9)
12386/32654 => 1386/3654 (removed 2)
5-digit fractions found:5087, omitted [75 40 376 78 209 379 591 351 2988]

123459/617295 => 12345/61725 (removed 9)
123468/493872 => 12468/49872 (removed 3)
123469/173524 => 12469/17524 (removed 3)
123469/193546 => 12469/19546 (removed 3)
123469/213568 => 12469/21568 (removed 3)
123469/283645 => 12469/28645 (removed 3)
123469/493876 => 12469/49876 (removed 3)
123469/573964 => 12469/57964 (removed 3)
123479/617395 => 12347/61735 (removed 9)
123495/172893 => 12345/17283 (removed 9)
123548/679514 => 12348/67914 (removed 5)
123574/325786 => 13574/35786 (removed 2)
6-digit fractions found:9778, omitted [230 256 921 186 317 751 262 205 6650]

Took 42.251172302s

```



## Julia


```julia
using Combinatorics

toi(set) = parse(Int, join(set, ""))
drop1(c, set) = toi(filter(x -> x != c, set))

function anomalouscancellingfractions(numdigits)
    ret = Vector{Tuple{Int, Int, Int, Int, Int}}()
    for nset in permutations(1:9, numdigits), dset in permutations(1:9, numdigits)
        if nset < dset # only proper fractions
            for c in nset
                if c in dset # a common digit exists
                    n, d, nn, dd = toi(nset), toi(dset), drop1(c, nset), drop1(c, dset)
                    if n // d == nn // dd # anomalous cancellation
                        push!(ret, (n, d, nn, dd, c))
                    end
                end
            end
        end
    end
    ret
end

function testfractionreduction(maxdigits=5)
    for i in 2:maxdigits
        results = anomalouscancellingfractions(i)
        println("\nFor $i digits, there were ", length(results),
            " fractions with anomalous cancellation.")
        numcounts = zeros(Int, 9)
        for r in results
            numcounts[r[5]] += 1
        end
        for (j, count) in enumerate(numcounts)
            count > 0 && println("The digit $j was crossed out $count times.")
        end
        println("Examples:")
        for j in 1:min(length(results), 12)
            r = results[j]
            println(r[1], "/", r[2], " = ", r[3], "/", r[4], "   ($(r[5]) crossed out)")
        end
    end
end

testfractionreduction()

```
{{out}}

```txt

For 2 digits, there were 4 fractions with anomalous cancellation.
The digit 6 was crossed out 2 times.
The digit 9 was crossed out 2 times.
Examples:
16/64 = 1/4   (6 crossed out)
19/95 = 1/5   (9 crossed out)
26/65 = 2/5   (6 crossed out)
49/98 = 4/8   (9 crossed out)

For 3 digits, there were 122 fractions with anomalous cancellation.
The digit 3 was crossed out 9 times.
The digit 4 was crossed out 1 times.
The digit 5 was crossed out 6 times.
The digit 6 was crossed out 15 times.
The digit 7 was crossed out 16 times.
The digit 8 was crossed out 15 times.
The digit 9 was crossed out 60 times.
Examples:
132/231 = 12/21   (3 crossed out)
134/536 = 14/56   (3 crossed out)
134/938 = 14/98   (3 crossed out)
136/238 = 16/28   (3 crossed out)
138/345 = 18/45   (3 crossed out)
139/695 = 13/65   (9 crossed out)
143/341 = 13/31   (4 crossed out)
146/365 = 14/35   (6 crossed out)
149/298 = 14/28   (9 crossed out)
149/596 = 14/56   (9 crossed out)
149/894 = 14/84   (9 crossed out)
154/253 = 14/23   (5 crossed out)

For 4 digits, there were 660 fractions with anomalous cancellation.
The digit 1 was crossed out 14 times.
The digit 2 was crossed out 25 times.
The digit 3 was crossed out 92 times.
The digit 4 was crossed out 14 times.
The digit 5 was crossed out 29 times.
The digit 6 was crossed out 63 times.
The digit 7 was crossed out 16 times.
The digit 8 was crossed out 17 times.
The digit 9 was crossed out 390 times.
Examples:
1234/4936 = 124/496   (3 crossed out)
1239/6195 = 123/615   (9 crossed out)
1246/3649 = 126/369   (4 crossed out)
1249/2498 = 124/248   (9 crossed out)
1259/6295 = 125/625   (9 crossed out)
1279/6395 = 127/635   (9 crossed out)
1283/5132 = 128/512   (3 crossed out)
1297/2594 = 127/254   (9 crossed out)
1297/3891 = 127/381   (9 crossed out)
1298/2596 = 128/256   (9 crossed out)
1298/3894 = 128/384   (9 crossed out)
1298/5192 = 128/512   (9 crossed out)

For 5 digits, there were 5087 fractions with anomalous cancellation.
The digit 1 was crossed out 75 times.
The digit 2 was crossed out 40 times.
The digit 3 was crossed out 376 times.
The digit 4 was crossed out 78 times.
The digit 5 was crossed out 209 times.
The digit 6 was crossed out 379 times.
The digit 7 was crossed out 591 times.
The digit 8 was crossed out 351 times.
The digit 9 was crossed out 2988 times.
Examples:
12349/24698 = 1234/2468   (9 crossed out)
12356/67958 = 1236/6798   (5 crossed out)
12358/14362 = 1258/1462   (3 crossed out)
12358/15364 = 1258/1564   (3 crossed out)
12358/17368 = 1258/1768   (3 crossed out)
12358/19372 = 1258/1972   (3 crossed out)
12358/21376 = 1258/2176   (3 crossed out)
12358/25384 = 1258/2584   (3 crossed out)
12359/61795 = 1235/6175   (9 crossed out)
12364/32596 = 1364/3596   (2 crossed out)
12379/61895 = 1237/6185   (9 crossed out)
12386/32654 = 1386/3654   (2 crossed out)

```


## Kotlin

{{trans|Go}}

```scala
fun indexOf(n: Int, s: IntArray): Int {
    for (i_j in s.withIndex()) {
        if (n == i_j.value) {
            return i_j.index
        }
    }
    return -1
}

fun getDigits(n: Int, le: Int, digits: IntArray): Boolean {
    var mn = n
    var mle = le
    while (mn > 0) {
        val r = mn % 10
        if (r == 0 || indexOf(r, digits) >= 0) {
            return false
        }
        mle--
        digits[mle] = r
        mn /= 10
    }
    return true
}

val pows = intArrayOf(1, 10, 100, 1_000, 10_000)

fun removeDigit(digits: IntArray, le: Int, idx: Int): Int {
    var sum = 0
    var pow = pows[le - 2]
    for (i in 0 until le) {
        if (i == idx) {
            continue
        }
        sum += digits[i] * pow
        pow /= 10
    }
    return sum
}

fun main() {
    val lims = listOf(
        Pair(12, 97),
        Pair(123, 986),
        Pair(1234, 9875),
        Pair(12345, 98764)
    )
    val count = IntArray(5)
    var omitted = arrayOf<Array<Int>>()
    for (i in 0 until 5) {
        var array = arrayOf<Int>()
        for (j in 0 until 10) {
            array += 0
        }
        omitted += array
    }
    for (i_lim in lims.withIndex()) {
        val i = i_lim.index
        val lim = i_lim.value

        val nDigits = IntArray(i + 2)
        val dDigits = IntArray(i + 2)
        val blank = IntArray(i + 2) { 0 }
        for (n in lim.first..lim.second) {
            blank.copyInto(nDigits)
            val nOk = getDigits(n, i + 2, nDigits)
            if (!nOk) {
                continue
            }
            for (d in n + 1..lim.second + 1) {
                blank.copyInto(dDigits)
                val dOk = getDigits(d, i + 2, dDigits)
                if (!dOk) {
                    continue
                }
                for (nix_digit in nDigits.withIndex()) {
                    val dix = indexOf(nix_digit.value, dDigits)
                    if (dix >= 0) {
                        val rn = removeDigit(nDigits, i + 2, nix_digit.index)
                        val rd = removeDigit(dDigits, i + 2, dix)
                        if (n.toDouble() / d.toDouble() == rn.toDouble() / rd.toDouble()) {
                            count[i]++
                            omitted[i][nix_digit.value]++
                            if (count[i] <= 12) {
                                println("$n/$d = $rn/$rd by omitting ${nix_digit.value}'s")
                            }
                        }
                    }
                }
            }
        }
        println()
    }

    for (i in 2..5) {
        println("There are ${count[i - 2]} $i-digit fractions of which:")
        for (j in 1..9) {
            if (omitted[i - 2][j] == 0) {
                continue
            }
            println("%6d have %d's omitted".format(omitted[i - 2][j], j))
        }
        println()
    }
}
```

{{out}}

```txt
16/64 = 1/4 by omitting 6's
19/95 = 1/5 by omitting 9's
26/65 = 2/5 by omitting 6's
49/98 = 4/8 by omitting 9's

132/231 = 12/21 by omitting 3's
134/536 = 14/56 by omitting 3's
134/938 = 14/98 by omitting 3's
136/238 = 16/28 by omitting 3's
138/345 = 18/45 by omitting 3's
139/695 = 13/65 by omitting 9's
143/341 = 13/31 by omitting 4's
146/365 = 14/35 by omitting 6's
149/298 = 14/28 by omitting 9's
149/596 = 14/56 by omitting 9's
149/894 = 14/84 by omitting 9's
154/253 = 14/23 by omitting 5's

1234/4936 = 124/496 by omitting 3's
1239/6195 = 123/615 by omitting 9's
1246/3649 = 126/369 by omitting 4's
1249/2498 = 124/248 by omitting 9's
1259/6295 = 125/625 by omitting 9's
1279/6395 = 127/635 by omitting 9's
1283/5132 = 128/512 by omitting 3's
1297/2594 = 127/254 by omitting 9's
1297/3891 = 127/381 by omitting 9's
1298/2596 = 128/256 by omitting 9's
1298/3894 = 128/384 by omitting 9's
1298/5192 = 128/512 by omitting 9's

12349/24698 = 1234/2468 by omitting 9's
12356/67958 = 1236/6798 by omitting 5's
12358/14362 = 1258/1462 by omitting 3's
12358/15364 = 1258/1564 by omitting 3's
12358/17368 = 1258/1768 by omitting 3's
12358/19372 = 1258/1972 by omitting 3's
12358/21376 = 1258/2176 by omitting 3's
12358/25384 = 1258/2584 by omitting 3's
12359/61795 = 1235/6175 by omitting 9's
12364/32596 = 1364/3596 by omitting 2's
12379/61895 = 1237/6185 by omitting 9's
12386/32654 = 1386/3654 by omitting 2's

There are 4 2-digit fractions of which:
     2 have 6's omitted
     2 have 9's omitted

There are 122 3-digit fractions of which:
     9 have 3's omitted
     1 have 4's omitted
     6 have 5's omitted
    15 have 6's omitted
    16 have 7's omitted
    15 have 8's omitted
    60 have 9's omitted

There are 660 4-digit fractions of which:
    14 have 1's omitted
    25 have 2's omitted
    92 have 3's omitted
    14 have 4's omitted
    29 have 5's omitted
    63 have 6's omitted
    16 have 7's omitted
    17 have 8's omitted
   390 have 9's omitted

There are 5087 5-digit fractions of which:
    75 have 1's omitted
    40 have 2's omitted
   376 have 3's omitted
    78 have 4's omitted
   209 have 5's omitted
   379 have 6's omitted
   591 have 7's omitted
   351 have 8's omitted
  2988 have 9's omitted
```



## Pascal

{{works with|Free Pascal}}
Using a permutation k out of n with k <= n<BR>
Inserting a record with this number and all numbers with one digit removed of that number.So only once calculated.Trade off is big size and no cache friendly local access.

```pascal

program FracRedu;
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON,ALL}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  SysUtils;

type
  tdigit = 0..9;
const
  cMaskDgt: array [tdigit] of Uint32 = (1, 2, 4, 8, 16, 32, 64, 128, 256, 512
    {,1024,2048,4096,8193,16384,32768});
  cMaxDigits = High(tdigit);
type
  tPermfield = array[tdigit] of uint32;
  tpPermfield = ^tPermfield;

  tDigitCnt = array[tdigit] of Uint32;

  tErg = record
           numUsedDigits : Uint32;
           numUnusedDigit : array[tdigit] of Uint32;
           numNormal : Uint64;// so sqr of number stays in Uint64
           dummy : array[0..7] of byte;//-> sizeof(tErg) = 64
         end;
  tpErg = ^tErg;
var
  Erg: array of tErg;
  pf_x, pf_y: tPermfield;
  DigitCnt :tDigitCnt;
  permcnt, UsedDigits,Anzahl: NativeUint;

  function Fakultaet(i: integer): integer;
  begin
    Result := 1;
    while i > 1 do
    begin
      Result := Result * i;
      Dec(i);
    end;
  end;

  procedure OutErg(dgt: Uint32;pi,pJ:tpErg);
  begin
    writeln(dgt:3,'  ', pi^.numUnusedDigit[dgt],'/',pj^.numUnusedDigit[dgt]
            ,' = ',pi^.numNormal,'/',pj^.numNormal);
  end;

  function Check(pI,pJ : tpErg;Nud :Word):integer;
  var
    dgt: NativeInt;
  Begin
    result := 0;
    dgt := 1;
    NUD := NUD SHR 1;
    repeat
      IF NUD AND 1 <> 0 then
      Begin
        If  pI^.numNormal*pJ^.numUnusedDigit[dgt] = pJ^.numNormal*pI^.numUnusedDigit[dgt] then
        Begin
          inc(result);
          inc(DigitCnt[dgt]);
          IF Anzahl < 110 then
            OutErg(dgt,pI,pJ);
        end;
      end;
      inc(dgt);
      NUD := NUD SHR 1;
    until NUD = 0;
  end;

  procedure CheckWithOne(pI : tpErg;j,Nud:Uint32);
  var
    pJ : tpErg;
    l : NativeUInt;
  Begin
    pJ := pI;
    if UsedDigits <5 then
    Begin
      for j := j+1 to permcnt do
      begin
        inc(pJ);
        //digits used by both numbers
        l := NUD AND pJ^.numUsedDigits;
        IF l <> 0 then
          inc(Anzahl,Check(pI,pJ,l));
      end;
    end
    else
    Begin
      for j := j+1 to permcnt do
      begin
        inc(pJ);
        l := NUD AND pJ^.numUsedDigits;
        inc(Anzahl,Check(pI,pJ,l));
      end;
    end;
  end;

  procedure SearchMultiple;
  var
    pI : tpErg;
    i : NativeUInt;
  begin
    pI := @Erg[0];
    for i := 0 to permcnt do
    Begin
      CheckWithOne(pI,i,pI^.numUsedDigits);
      inc(pI);
    end;
  end;

  function BinomCoeff(n, k: byte): longint;
  var
    i: longint;
  begin
    {n ueber k  = n ueber (n-k) , also kuerzere Version waehlen}
    if k > n div 2 then
      k := n - k;
    Result := 1;
    if k <= n then
      for i := 1 to k do
        Result := Result * (n - i + 1) div i;{geht immer  ohne Rest }
  end;

  procedure InsertToErg(var E: tErg; const x: tPermfield);
  var
    n : Uint64;
    k,i,j,dgt,nud: NativeInt;
  begin
    // k of PermKoutofN is reduced by one for 9 digits
    k := UsedDigits;
    n := 0;
    nud := 0;
    for i := 1 to k do
    begin
      dgt := x[i];
      nud := nud or cMaskDgt[dgt];
      n := n * 10 + dgt;
    end;
    with E do
    begin
      numUsedDigits := nud;
      numNormal := n;
    end;
    //calc all numbers with one removed digit
    For J := k downto 1 do
    Begin
      n := 0;
      for i := 1 to j-1 do
        n := n * 10 + x[i];
      for i := j+1 to k do
        n := n * 10 + x[i];
      E.numUnusedDigit[x[j]] := n;
    end;
  end;

  procedure PermKoutofN(k, n: nativeInt);
  var
    x, y: tpPermfield;
    i, yi, tmp: NativeInt;
  begin
    //initialise
    x := @pf_x;
    y := @pf_y;
    permcnt := 0;
    if k > n then
      k := n;
    if k = n then
      k := k - 1;
    for i := 1 to n do
      x^[i] := i;
    for i := 1 to k do
      y^[i] := i;

    InserttoErg(Erg[permcnt], x^);
    i := k;
    repeat
      yi := y^[i];
      if yi < n then
      begin
        Inc(permcnt);
        Inc(yi);
        y^[i] := yi;
        tmp := x^[i];
        x^[i] := x^[yi];
        x^[yi] := tmp;
        i := k;
        InserttoErg(Erg[permcnt], x^);
      end
      else
      begin
        repeat
          tmp := x^[i];
          x^[i] := x^[yi];
          x^[yi] := tmp;
          Dec(yi);
        until yi <= i;
        y^[i] := yi;
        Dec(i);
      end;
    until (i = 0);
  end;

  procedure OutDigitCount;
   var
     i : tDigit;
  Begin
    writeln('omitted digits 1 to 9');
    For i := 1 to 9do
      write(DigitCnt[i]:UsedDigits);
    writeln;
  end;

  procedure ClearDigitCount;
   var
     i : tDigit;
  Begin
    For i := low(DigitCnt) to high(DigitCnt) do
      DigitCnt[i] := 0;
  end;

var
  t1, t0: TDateTime;
begin
  For UsedDigits := 8 to 9 do
  Begin
    writeln('Used digits ',UsedDigits);
    T0 := now;
    ClearDigitCount;
    setlength(Erg, Fakultaet(UsedDigits) * BinomCoeff(cMaxDigits, UsedDigits));
    Anzahl := 0;
    permcnt := 0;
    PermKoutOfN(UsedDigits, cMaxDigits);
    SearchMultiple;
    T1 := now;
    writeln('Found solutions ',Anzahl);
    OutDigitCount;
    writeln('time taken ',FormatDateTime('HH:NN:SS.zzz', T1 - T0));
    setlength(Erg, 0);
    writeln;
  end;
end.
```

{{out}}

```txt

{ /* inserted by hand                            / solutions
Used digits 2 count of different numbers 72      /     4
Used digits 3 count of different numbers 504     /   122
Used digits 4 count of different numbers 3024    /   660
Used digits 5 count of different numbers 15120   /  5087
Used digits 6 count of different numbers 60480   /  9778
Used digits 7 count of different numbers 181440  / 40163
Used digits 8 count of different numbers 362880  / 17722
Used digits 9 count of different numbers 362880  / 92413
*/ }

Used digits 2
  6  1/4 = 16/64
  9  1/5 = 19/95
  6  2/5 = 26/65
  9  4/8 = 49/98
Found solutions 4
omitted digits 1 to 9
 0 0 0 0 0 2 0 0 2
time taken 00:00:00.000

Used digits 3
  3  12/21 = 132/231
  3  14/56 = 134/536
  3  14/98 = 134/938
  3  16/28 = 136/238
  3  18/45 = 138/345
  9  13/65 = 139/695
  4  13/31 = 143/341
  6  14/35 = 146/365
  9  14/28 = 149/298
  9  14/56 = 149/596
  9  14/84 = 149/894
  5  14/23 = 154/253
Found solutions 122
omitted digits 1 to 9
  0  0  9  1  6 15 16 15 60
time taken 00:00:00.004

Used digits 4
  3  124/496 = 1234/4936
  9  123/615 = 1239/6195
  4  126/369 = 1246/3649
  9  124/248 = 1249/2498
  9  125/625 = 1259/6295
  9  127/635 = 1279/6395
  3  128/512 = 1283/5132
  9  127/254 = 1297/2594
  9  127/381 = 1297/3891
  9  128/256 = 1298/2596
  9  128/384 = 1298/3894
  9  128/512 = 1298/5192
Found solutions 660
omitted digits 1 to 9
  14  25  92  14  29  63  16  17 390
time taken 00:00:00.060

Used digits 5
  9  1234/2468 = 12349/24698
  5  1236/6798 = 12356/67958
  3  1258/1462 = 12358/14362
  3  1258/1564 = 12358/15364
  3  1258/1768 = 12358/17368
  3  1258/1972 = 12358/19372
  3  1258/2176 = 12358/21376
  3  1258/2584 = 12358/25384
  9  1235/6175 = 12359/61795
  2  1364/3596 = 12364/32596
  9  1237/6185 = 12379/61895
  2  1386/3654 = 12386/32654
Found solutions 5087
omitted digits 1 to 9
   75   40  376   78  209  379  591  351 2988
time taken 00:00:01.787

Used digits 6
  9  12345/61725 = 123459/617295
  3  12468/49872 = 123468/493872
  3  12469/17524 = 123469/173524
  3  12469/19546 = 123469/193546
  3  12469/21568 = 123469/213568
  3  12469/28645 = 123469/283645
  3  12469/49876 = 123469/493876
  3  12469/57964 = 123469/573964
  9  12347/61735 = 123479/617395
  9  12345/17283 = 123495/172893
  5  12348/67914 = 123548/679514
  2  13574/35786 = 123574/325786
Found solutions 9778
omitted digits 1 to 9
   230   256   921   186   317   751   262   205  6650
time taken 00:00:31.858

Used digits 7
  3  124569/498276 = 1234569/4938276
  3  124579/195286 = 1234579/1935286
  3  124579/245791 = 1234579/2435791
  3  124579/286195 = 1234579/2836195
  3  124579/457912 = 1234579/4537912
  3  124579/528619 = 1234579/5238619
  3  124579/579124 = 1234579/5739124
  3  124579/619528 = 1234579/6139528
  9  123457/617285 = 1234579/6172895
  9  123457/617285 = 1234597/6172985
  9  123465/617325 = 1234659/6173295
  3  124678/498712 = 1234678/4938712
Found solutions 40163
omitted digits 1 to 9
    333    191   1368    278    498   1094   3657   1434  31310
time taken 00:04:54.703

Used digits 8
  3  1245679/2457691 = 12345679/24357691
  6  1234579/2435791 = 12345679/24357691
  3  1245679/4982716 = 12345679/49382716
  3  1245679/6194728 = 12345679/61394728
  9  1234567/6172835 = 12345679/61728395
  3  1245689/4982756 = 12345689/49382756
  9  1234567/6172835 = 12345967/61729835
  9  1234657/6173285 = 12346579/61732895
  9  1234657/6173285 = 12346597/61732985
  3  1246789/4987156 = 12346789/49387156
  9  1234685/6173425 = 12346859/61734295
  3  1246879/4987516 = 12346879/49387516
Found solutions 17233
omitted digits 1 to 9
     247     233     888     288     355     710     425     193   13894
time taken 00:18:58.784

Used digits 9
  3  12456789/49827156 = 123456789/493827156
  3  12456879/49827516 = 123456879/493827516
  9  12345687/61728435 = 123456879/617284395
  9  12345687/61728435 = 123456987/617284935
  9  12345687/61728435 = 123459687/617298435
  9  12346857/61734285 = 123468579/617342895
  9  12346857/61734285 = 123468597/617342985
  9  12346857/61734285 = 123469857/617349285
  9  12347685/61738425 = 123476859/617384295
  9  12347685/61738425 = 123476985/617384925
  5  12347896/67913428 = 123478956/679134258
  9  12347685/61738425 = 123479685/617398425
Found solutions 92413
omitted digits 1 to 9
      266      110     1008      131      324      737      300      159    89378
time taken 00:13:04.511

/*
go version go1.10.3 gccgo (Debian 8.3.0-6) 8.3.0 linux/amd64
6-digit fractions found:9778, omitted [230 256 921 186 317 751 262 205 6650]

Took 1m38.85577279s
*/
```



## MiniZinc


### The Model


```MiniZinc

%Latin Squares in Reduced Form. Nigel Galloway, September 5th., 2019
include "alldifferent.mzn"; include "member.mzn";
int: S;
array [1..9] of int: Pn=[1,10,100,1000,10000,100000,1000000,10000000,100000000];
array [1..S] of var 1..9: Nz; constraint alldifferent(Nz);
array [1..S] of var 1..9: Gz; constraint alldifferent(Gz);
var  int: n; constraint n=sum(n in 1..S)(Nz[n]*Pn[n]);
var  int: i; constraint i=sum(n in 1..S)(Gz[n]*Pn[n]); constraint n<i; constraint n*g=i*e;
var  int: g; constraint g=sum(n in 1..S)(if n=a then 0 elseif n>a then Gz[n]*Pn[n-1] else Gz[n]*Pn[n] endif);
var  int: e; constraint e=sum(n in 1..S)(if n=l then 0 elseif n>l then Nz[n]*Pn[n-1] else Nz[n]*Pn[n] endif);
var 1..S: l; constraint Nz[l]=w;
var 1..S: a; constraint Gz[a]=w;
var 1..9: w; constraint member(Nz,w) /\ member(Gz,w);

output [show(n)++"/"++show(i)++" becomes "++show(e)++"/"++show(g)++" when "++show(w)++" is omitted"]

```


### The Tasks

;Displaying 12 solutions
;minizinc --num-solutions 12 -DS=2
{{out}}

```txt

16/64 becomes 1/4 when 6 is omitted
----------
26/65 becomes 2/5 when 6 is omitted
----------
19/95 becomes 1/5 when 9 is omitted
----------
49/98 becomes 4/8 when 9 is omitted
----------

### ====


```

;minizinc --num-solutions 12 -DS=3
{{out}}

```txt

132/231 becomes 12/21 when 3 is omitted
----------
134/536 becomes 14/56 when 3 is omitted
----------
134/938 becomes 14/98 when 3 is omitted
----------
136/238 becomes 16/28 when 3 is omitted
----------
138/345 becomes 18/45 when 3 is omitted
----------
139/695 becomes 13/65 when 9 is omitted
----------
143/341 becomes 13/31 when 4 is omitted
----------
146/365 becomes 14/35 when 6 is omitted
----------
149/298 becomes 14/28 when 9 is omitted
----------
149/596 becomes 14/56 when 9 is omitted
----------
149/894 becomes 14/84 when 9 is omitted
----------
154/253 becomes 14/23 when 5 is omitted
----------

```

;minizinc --num-solutions 12 -DS=4
{{out}}

```txt

2147/3164 becomes 247/364 when 1 is omitted
----------
2314/3916 becomes 234/396 when 1 is omitted
----------
2147/5198 becomes 247/598 when 1 is omitted
----------
3164/5198 becomes 364/598 when 1 is omitted
----------
2314/6319 becomes 234/639 when 1 is omitted
----------
3916/6319 becomes 396/639 when 1 is omitted
----------
5129/7136 becomes 529/736 when 1 is omitted
----------
3129/7152 becomes 329/752 when 1 is omitted
----------
4913/7514 becomes 493/754 when 1 is omitted
----------
7168/8176 becomes 768/876 when 1 is omitted
----------
5129/9143 becomes 529/943 when 1 is omitted
----------
7136/9143 becomes 736/943 when 1 is omitted
----------

```

;minizinc --num-solutions 12 -DS=5
{{out}}

```txt

21356/31472 becomes 2356/3472 when 1 is omitted
----------
21394/31528 becomes 2394/3528 when 1 is omitted
----------
21546/31752 becomes 2546/3752 when 1 is omitted
----------
21679/31948 becomes 2679/3948 when 1 is omitted
----------
21698/31976 becomes 2698/3976 when 1 is omitted
----------
25714/34615 becomes 2574/3465 when 1 is omitted
----------
27615/34716 becomes 2765/3476 when 1 is omitted
----------
25917/34719 becomes 2597/3479 when 1 is omitted
----------
25916/36518 becomes 2596/3658 when 1 is omitted
----------
31276/41329 becomes 3276/4329 when 1 is omitted
----------
21375/41625 becomes 2375/4625 when 1 is omitted
----------
31584/41736 becomes 3584/4736 when 1 is omitted
----------

```

;minizinc --num-solutions 12 -DS=6
{{out}}

```txt

123495/172893 becomes 12345/17283 when 9 is omitted
----------
123594/164792 becomes 12354/16472 when 9 is omitted
----------
123654/163758 becomes 12654/16758 when 3 is omitted
----------
124678/135679 becomes 12478/13579 when 6 is omitted
----------
124768/164872 becomes 12768/16872 when 4 is omitted
----------
125349/149352 becomes 12549/14952 when 3 is omitted
----------
125394/146293 becomes 12534/14623 when 9 is omitted
----------
125937/127936 becomes 12537/12736 when 9 is omitted
----------
125694/167592 becomes 12564/16752 when 9 is omitted
----------
125769/135786 becomes 12769/13786 when 5 is omitted
----------
125769/165837 becomes 12769/16837 when 5 is omitted
----------
125934/146923 becomes 12534/14623 when 9 is omitted
----------

```

;Count number of solutions
;minizinc --all-solutions -s -DS=3
{{out}}

```txt

%%%mzn-stat: nSolutions=122

```

;minizinc --all-solutions -s -DS=4
{{out}}

```txt

%%%mzn-stat: nSolutions=660

```

;minizinc --all-solutions -s -DS=5
{{out}}

```txt

%%%mzn-stat: nSolutions=5087

```


## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';
use List::Util qw<sum uniq uniqnum head tail>;

for my $exp (map { $_ - 1 } <2 3 4>) {
    my %reduced;
    my $start = sum map { 10 ** $_ * ($exp - $_ + 1) } 0..$exp;
    my $end   = 10**($exp+1) - -1 + sum map { 10 ** $_ * ($exp - $_) } 0..$exp-1;

    for my $den ($start .. $end-1) {
        next if $den =~ /0/ or (uniqnum split '', $den) <= $exp;
        for my $num ($start .. $den-1) {
            next if $num =~ /0/ or (uniqnum split '', $num) <= $exp;
            my %i;
            map { $i{$_}++ } (uniq head -1, split '',$den), uniq tail -1, split '',$num;
            my @set = grep { $_ if $i{$_} > 1 } keys %i;
            next if @set < 1;
            for (@set) {
                (my $ne = $num) =~ s/$_//;
                (my $de = $den) =~ s/$_//;
                if ($ne/$de == $num/$den) {
                    $reduced{"$num/$den:$_"} = "$ne/$de";
                }
            }
        }
    }
    my $digit = $exp + 1;
    say "\n" . +%reduced . " $digit-digit reducible fractions:";
    for my $n (1..9) {
        my $cnt = scalar grep { /:$n/ } keys %reduced;
        say "$cnt with removed $n" if $cnt;
    }
    say "\n  12 (or all, if less) $digit-digit reducible fractions:";
    for my $f (head 12, sort keys %reduced) {
        printf "    %s => %s removed %s\n", substr($f,0,$digit*2+1), $reduced{$f}, substr($f,-1)
    }
}
```

{{out}}

```txt
4 2-digit reducible fractions:
  2 with removed 6
  2 with removed 9

  12 (or all, if less) 2-digit reducible fractions:
    16/64 => 1/4 removed 6
    19/95 => 1/5 removed 9
    26/65 => 2/5 removed 6
    49/98 => 4/8 removed 9

122 3-digit reducible fractions:
  9 with removed 3
  1 with removed 4
  6 with removed 5
  15 with removed 6
  16 with removed 7
  15 with removed 8
  60 with removed 9

  12 (or all, if less) 3-digit reducible fractions:
    132/231 => 12/21 removed 3
    134/536 => 14/56 removed 3
    134/938 => 14/98 removed 3
    136/238 => 16/28 removed 3
    138/345 => 18/45 removed 3
    139/695 => 13/65 removed 9
    143/341 => 13/31 removed 4
    146/365 => 14/35 removed 6
    149/298 => 14/28 removed 9
    149/596 => 14/56 removed 9
    149/894 => 14/84 removed 9
    154/253 => 14/23 removed 5

660 4-digit reducible fractions:
  14 with removed 1
  25 with removed 2
  92 with removed 3
  14 with removed 4
  29 with removed 5
  63 with removed 6
  16 with removed 7
  17 with removed 8
  390 with removed 9

  12 (or all, if less) 4-digit reducible fractions:
    1234/4936 => 124/496 removed 3
    1239/6195 => 123/615 removed 9
    1246/3649 => 126/369 removed 4
    1249/2498 => 124/248 removed 9
    1259/6295 => 125/625 removed 9
    1279/6395 => 127/635 removed 9
    1283/5132 => 128/512 removed 3
    1297/2594 => 127/254 removed 9
    1297/3891 => 127/381 removed 9
    1298/2596 => 128/256 removed 9
    1298/3894 => 128/384 removed 9
    1298/5192 => 128/512 removed 9
```



## Perl 6

{{works with|Rakudo|2019.07.1}}
;[[wp:Anomalous cancellation|Anomalous Cancellation]]

```perl6
my %reduced;
my $digits = 2..4;

for $digits.map: * - 1 -> $exp {
    my $start = sum (0..$exp).map( { 10 ** $_ * ($exp - $_ + 1) });
    my $end   = 10**($exp+1) - sum (^$exp).map( { 10 ** $_ * ($exp - $_) } ) - 1;

    ($start ..^ $end).race(:8degree, :3batch).map: -> $den {
        next if $den.contains: '0';
        next if $den.comb.unique <= $exp;

        for $start ..^ $den -> $num {
            next if $num.contains: '0';
            next if $num.comb.unique <= $exp;

            my $set = ($den.comb.head(* - 1).Set ∩ $num.comb.skip(1).Set);
            next if $set.elems < 1;

            for $set.keys {
                my $ne = $num.trans: $_ => '', :delete;
                my $de = $den.trans: $_ => '', :delete;
                if $ne / $de == $num / $den {
                    print "\b" x 40, "$num/$den:$_ => $ne/$de";
                    %reduced{"$num/$den:$_"} = "$ne/$de";
                }
            }
        }
    }


    print "\b" x 40, ' ' x 40, "\b" x 40;

    my $digit = $exp +1;
    my %d = %reduced.pairs.grep: { .key.chars == ($digit * 2 + 3) };
    say "\n({+%d}) $digit digit reduceable fractions:";
    for 1..9 {
        my $cnt = +%d.pairs.grep( *.key.contains: ":$_" );
        next unless $cnt;
        say "  $cnt with removed $_";
    }
    say "\n  12 Random (or all, if less) $digit digit reduceable fractions:";
    say "    {.key.substr(0, $digit * 2 + 1)} => {.value} removed {.key.substr(* - 1)}"
      for %d.pairs.pick(12).sort;
}
```

{{out|Sample output}}

```txt
(4) 2 digit reduceable fractions:
  2 with removed 6
  2 with removed 9

  12 Random (or all, if less) 2 digit reduceable fractions:
    16/64 => 1/4 removed 6
    19/95 => 1/5 removed 9
    26/65 => 2/5 removed 6
    49/98 => 4/8 removed 9

(122) 3 digit reduceable fractions:
  9 with removed 3
  1 with removed 4
  6 with removed 5
  15 with removed 6
  16 with removed 7
  15 with removed 8
  60 with removed 9

  12 Random (or all, if less) 3 digit reduceable fractions:
    149/298 => 14/28 removed 9
    154/352 => 14/32 removed 5
    165/264 => 15/24 removed 6
    176/275 => 16/25 removed 7
    187/286 => 17/26 removed 8
    194/291 => 14/21 removed 9
    286/385 => 26/35 removed 8
    286/682 => 26/62 removed 8
    374/572 => 34/52 removed 7
    473/572 => 43/52 removed 7
    492/984 => 42/84 removed 9
    594/693 => 54/63 removed 9

(660) 4 digit reduceable fractions:
  14 with removed 1
  25 with removed 2
  92 with removed 3
  14 with removed 4
  29 with removed 5
  63 with removed 6
  16 with removed 7
  17 with removed 8
  390 with removed 9

  12 Random (or all, if less) 4 digit reduceable fractions:
    1348/4381 => 148/481 removed 3
    1598/3196 => 158/316 removed 9
    1783/7132 => 178/712 removed 3
    1978/5934 => 178/534 removed 9
    2971/5942 => 271/542 removed 9
    2974/5948 => 274/548 removed 9
    3584/4592 => 384/492 removed 5
    3791/5798 => 391/598 removed 7
    3968/7936 => 368/736 removed 9
    4329/9324 => 429/924 removed 3
    4936/9872 => 436/872 removed 9
    6327/8325 => 627/825 removed 3
```



## Phix


```Phix
function to_n(sequence digits, integer remove_digit=0)
    if remove_digit!=0 then
        integer d = find(remove_digit,digits)
        digits[d..d] = {}
    end if
    integer res = digits[1]
    for i=2 to length(digits) do
        res = res*10+digits[i]
    end for
    return res
end function

function ndigits(integer n)
-- generate numbers with unique digits efficiently
-- and store them in an array for multiple re-use,
-- along with an array of the removed-digit values.
    sequence res = {},
             digits = tagset(n),
             used = repeat(1,n)&repeat(0,9-n)
    while true do
        sequence nine = repeat(0,9)
        for i=1 to length(used) do
            if used[i] then
                nine[i] = to_n(digits,i)
            end if
        end for
        res = append(res,{to_n(digits),nine})
        bool found = false
        for i=n to 1 by -1 do
            integer d = digits[i]
            if not used[d] then ?9/0 end if
            used[d] = 0
            for j=d+1 to 9 do
                if not used[j] then
                    used[j] = 1
                    digits[i] = j
                    for k=i+1 to n do
                        digits[k] = find(0,used)
                        used[digits[k]] = 1
                    end for
                    found = true
                    exit
                end if
            end for
            if found then exit end if
        end for
        if not found then exit end if
    end while
    return res
end function

atom t0 = time(),
     t1 = time()+1
for n=2 to 6 do
    sequence d = ndigits(n)
    integer count = 0
    sequence omitted = repeat(0,9)
    for i=1 to length(d)-1 do
        {integer xn, sequence rn} = d[i]
        for j=i+1 to length(d) do
            {integer xd, sequence rd} = d[j]
            for k=1 to 9 do
                integer yn = rn[k], yd = rd[k]
                if yn!=0 and yd!=0 and xn/xd = yn/yd then
                    count += 1
                    omitted[k] += 1
                    if count<=12 then
                        printf(1,"%d/%d => %d/%d (removed %d)\n",{xn,xd,yn,yd,k})
                    elsif time()>t1 then
                        printf(1,"working (%d/%d)...\r",{i,length(d)})
                        t1 = time()+1
                    end if
                end if
            end for
        end for
    end for
    printf(1,"%d-digit fractions found:%d, omitted %v\n\n",{n,count,omitted})
end for
?elapsed(time()-t0)
```

{{out}}

```txt

16/64 => 1/4 (removed 6)
19/95 => 1/5 (removed 9)
26/65 => 2/5 (removed 6)
49/98 => 4/8 (removed 9)
2-digit fractions found:4, omitted {0,0,0,0,0,2,0,0,2}

132/231 => 12/21 (removed 3)
134/536 => 14/56 (removed 3)
134/938 => 14/98 (removed 3)
136/238 => 16/28 (removed 3)
138/345 => 18/45 (removed 3)
139/695 => 13/65 (removed 9)
143/341 => 13/31 (removed 4)
146/365 => 14/35 (removed 6)
149/298 => 14/28 (removed 9)
149/596 => 14/56 (removed 9)
149/894 => 14/84 (removed 9)
154/253 => 14/23 (removed 5)
3-digit fractions found:122, omitted {0,0,9,1,6,15,16,15,60}

1234/4936 => 124/496 (removed 3)
1239/6195 => 123/615 (removed 9)
1246/3649 => 126/369 (removed 4)
1249/2498 => 124/248 (removed 9)
1259/6295 => 125/625 (removed 9)
1279/6395 => 127/635 (removed 9)
1283/5132 => 128/512 (removed 3)
1297/2594 => 127/254 (removed 9)
1297/3891 => 127/381 (removed 9)
1298/2596 => 128/256 (removed 9)
1298/3894 => 128/384 (removed 9)
1298/5192 => 128/512 (removed 9)
4-digit fractions found:660, omitted {14,25,92,14,29,63,16,17,390}

12349/24698 => 1234/2468 (removed 9)
12356/67958 => 1236/6798 (removed 5)
12358/14362 => 1258/1462 (removed 3)
12358/15364 => 1258/1564 (removed 3)
12358/17368 => 1258/1768 (removed 3)
12358/19372 => 1258/1972 (removed 3)
12358/21376 => 1258/2176 (removed 3)
12358/25384 => 1258/2584 (removed 3)
12359/61795 => 1235/6175 (removed 9)
12364/32596 => 1364/3596 (removed 2)
12379/61895 => 1237/6185 (removed 9)
12386/32654 => 1386/3654 (removed 2)
5-digit fractions found:5087, omitted {75,40,376,78,209,379,591,351,2988}

123459/617295 => 12345/61725 (removed 9)
123468/493872 => 12468/49872 (removed 3)
123469/173524 => 12469/17524 (removed 3)
123469/193546 => 12469/19546 (removed 3)
123469/213568 => 12469/21568 (removed 3)
123469/283645 => 12469/28645 (removed 3)
123469/493876 => 12469/49876 (removed 3)
123469/573964 => 12469/57964 (removed 3)
123479/617395 => 12347/61735 (removed 9)
123495/172893 => 12345/17283 (removed 9)
123548/679514 => 12348/67914 (removed 5)
123574/325786 => 13574/35786 (removed 2)
6-digit fractions found:9778, omitted {230,256,921,186,317,751,262,205,6650}

"10 minutes and 13s"

```



## Racket


Racket's generator is horribly slow, so I roll my own more efficient generator. Pretty much using continuation-passing style, but then using macro to make it appear that we are writing in the direct style.


```racket
#lang racket

(require racket/generator
         syntax/parse/define)

(define-syntax-parser for**
  [(_ [x:id {~datum <-} (e ...)] rst ...) #'(e ... (λ (x) (for** rst ...)))]
  [(_ e ...) #'(begin e ...)])

(define (permutations xs n yield #:lower [lower #f])
  (let loop ([xs xs] [n n] [acc '()] [lower lower])
    (cond
      [(= n 0) (yield (reverse acc))]
      [else (for ([x (in-list xs)] #:when (or (not lower) (>= x (first lower))))
              (loop (remove x xs)
                    (sub1 n)
                    (cons x acc)
                    (and lower (= x (first lower)) (rest lower))))])))

(define (list->number xs) (foldl (λ (e acc) (+ (* 10 acc) e)) 0 xs))

(define (calc n)
  (define rng (range 1 10))
  (in-generator
   (for** [numer <- (permutations rng n)]
          [denom <- (permutations rng n #:lower numer)]
          (for* (#:when (not (equal? numer denom))
                 [crossed (in-list numer)]
                 #:when (member crossed denom)
                 [numer* (in-value (list->number (remove crossed numer)))]
                 [denom* (in-value (list->number (remove crossed denom)))]
                 [numer** (in-value (list->number numer))]
                 [denom** (in-value (list->number denom))]
                 #:when (= (* numer** denom*) (* numer* denom**)))
            (yield (list numer** denom** numer* denom* crossed))))))

(define (enumerate n)
  (for ([x (calc n)] [i (in-range 12)])
    (apply printf "~a/~a = ~a/~a (~a crossed out)\n" x))
  (newline))

(define (stats n)
  (define digits (make-hash))
  (for ([x (calc n)]) (hash-update! digits (last x) add1 0))
  (printf "There are ~a ~a-digit fractions of which:\n" (for/sum ([(k v) (in-hash digits)]) v) n)
  (for ([digit (in-list (sort (hash->list digits) < #:key car))])
    (printf "  The digit ~a was crossed out ~a times\n" (car digit) (cdr digit)))
  (newline))

(define (main)
  (enumerate 2)
  (enumerate 3)
  (enumerate 4)
  (enumerate 5)
  (stats 2)
  (stats 3)
  (stats 4)
  (stats 5))

(main)
```


{{out}}

```txt

16/64 = 1/4 (6 crossed out)
19/95 = 1/5 (9 crossed out)
26/65 = 2/5 (6 crossed out)
49/98 = 4/8 (9 crossed out)

132/231 = 12/21 (3 crossed out)
134/536 = 14/56 (3 crossed out)
134/938 = 14/98 (3 crossed out)
136/238 = 16/28 (3 crossed out)
138/345 = 18/45 (3 crossed out)
139/695 = 13/65 (9 crossed out)
143/341 = 13/31 (4 crossed out)
146/365 = 14/35 (6 crossed out)
149/298 = 14/28 (9 crossed out)
149/596 = 14/56 (9 crossed out)
149/894 = 14/84 (9 crossed out)
154/253 = 14/23 (5 crossed out)

1234/4936 = 124/496 (3 crossed out)
1239/6195 = 123/615 (9 crossed out)
1246/3649 = 126/369 (4 crossed out)
1249/2498 = 124/248 (9 crossed out)
1259/6295 = 125/625 (9 crossed out)
1279/6395 = 127/635 (9 crossed out)
1283/5132 = 128/512 (3 crossed out)
1297/2594 = 127/254 (9 crossed out)
1297/3891 = 127/381 (9 crossed out)
1298/2596 = 128/256 (9 crossed out)
1298/3894 = 128/384 (9 crossed out)
1298/5192 = 128/512 (9 crossed out)

12349/24698 = 1234/2468 (9 crossed out)
12356/67958 = 1236/6798 (5 crossed out)
12358/14362 = 1258/1462 (3 crossed out)
12358/15364 = 1258/1564 (3 crossed out)
12358/17368 = 1258/1768 (3 crossed out)
12358/19372 = 1258/1972 (3 crossed out)
12358/21376 = 1258/2176 (3 crossed out)
12358/25384 = 1258/2584 (3 crossed out)
12359/61795 = 1235/6175 (9 crossed out)
12364/32596 = 1364/3596 (2 crossed out)
12379/61895 = 1237/6185 (9 crossed out)
12386/32654 = 1386/3654 (2 crossed out)

There are 4 2-digit fractions of which:
  The digit 6 was crossed out 2 times
  The digit 9 was crossed out 2 times

There are 122 3-digit fractions of which:
  The digit 3 was crossed out 9 times
  The digit 4 was crossed out 1 times
  The digit 5 was crossed out 6 times
  The digit 6 was crossed out 15 times
  The digit 7 was crossed out 16 times
  The digit 8 was crossed out 15 times
  The digit 9 was crossed out 60 times

There are 660 4-digit fractions of which:
  The digit 1 was crossed out 14 times
  The digit 2 was crossed out 25 times
  The digit 3 was crossed out 92 times
  The digit 4 was crossed out 14 times
  The digit 5 was crossed out 29 times
  The digit 6 was crossed out 63 times
  The digit 7 was crossed out 16 times
  The digit 8 was crossed out 17 times
  The digit 9 was crossed out 390 times

There are 5087 5-digit fractions of which:
  The digit 1 was crossed out 75 times
  The digit 2 was crossed out 40 times
  The digit 3 was crossed out 376 times
  The digit 4 was crossed out 78 times
  The digit 5 was crossed out 209 times
  The digit 6 was crossed out 379 times
  The digit 7 was crossed out 591 times
  The digit 8 was crossed out 351 times
  The digit 9 was crossed out 2988 times

```



## REXX


```rexx
/*REXX pgm reduces fractions by "crossing out" matching digits in nominator&denominator.*/
parse arg high show .                            /*obtain optional arguments from the CL*/
if high=='' | high==","  then high=  4           /*Not specified?  Then use the default.*/
if show=='' | show==","  then show= 12           /* "      "         "   "   "     "    */
say center(' some samples of reduced fractions by crossing out digits ', 79, "═")
$.=0                                             /*placeholder array for counts; init. 0*/
      do L=2  to high;      say                  /*do 2-dig fractions to HIGH-dig fract.*/
                            lim= 10**L - 1       /*calculate the upper limit just once. */
         do n=10**(L-1)  to lim                  /*generate some  N  digit  fractions.  */
         if pos(0, n) \==0  then iterate         /*Does  it  have a zero?  Then skip it.*/
         if hasDup(n)       then iterate         /*  "    "    "  " dup?     "    "   " */

            do d=n+1     to lim                           /*only process like-sized #'s */
            if pos(0, d)\==0         then iterate         /*Have a zero? Then skip it.  */
            if verify(d, n, 'M')==0  then iterate         /*No digs in common?  Skip it.*/
            if hasDup(d)             then iterate         /*Any digs are dups?    "   " */
            q= n/d                                        /*compute quotient just once. */
                  do e=1  for L;     xo= substr(n, e, 1)  /*try crossing out each digit.*/
                  nn= space( translate(n, , xo), 0)       /*elide from the numerator.   */
                  dd= space( translate(d, , xo), 0)       /*  "     "   "  denominator. */
                  if nn/dd \== q  then iterate            /*Not the same quotient? Skip.*/
                  $.L=    $.L    + 1                      /*Eureka!   We found one.     */
                  $.L.xo= $.L.xo + 1                      /*count the silly reduction.  */
                  if $.L>show  then iterate               /*Too many found?  Don't show.*/
                  say center(n'/'d   " = "   nn'/'dd  "  by crossing out the" xo"'s.", 79)
                  end   /*e*/
            end         /*d*/
         end            /*n*/
      end               /*L*/
say;                  @with= ' with crossed-out' /* [↓]  show counts for any reductions.*/
      do k=1  for 9                              /*traipse through each cross─out digit.*/
      if $.k==0  then iterate                    /*Is this a zero count?  Then skip it. */
      say;    say center('There are '     $.k     " "k'-digit fractions.', 79, "═")
                          @for= '          For ' /*literal for SAY indentation (below). */
         do #=1  for 9;   if $.k.#==0  then iterate
         say @for    k"-digit fractions, there are "    right($.k.#, k-1)   @with   #"'s."
         end   /*#*/
      end      /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hasDup: parse arg x;          /* if L<2  then return 0 */           /*L will never be 1.*/
           do i=1  for L-1;      if pos(substr(x,i,1), substr(x,i+1)) \== 0  then return 1
           end   /*i*/;                                                           return 0
```

{{out|output|text=  when using the input of:     <tt> 5   12 </tt>}}

```txt

══════════ some samples of reduced fractions by crossing out digits ═══════════

                   16/64  =  1/4   by crossing out the 6's.
                   19/95  =  1/5   by crossing out the 9's.
                   26/65  =  2/5   by crossing out the 6's.
                   49/98  =  4/8   by crossing out the 9's.

                 132/231  =  12/21   by crossing out the 3's.
                 134/536  =  14/56   by crossing out the 3's.
                 134/938  =  14/98   by crossing out the 3's.
                 136/238  =  16/28   by crossing out the 3's.
                 138/345  =  18/45   by crossing out the 3's.
                 139/695  =  13/65   by crossing out the 9's.
                 143/341  =  13/31   by crossing out the 4's.
                 146/365  =  14/35   by crossing out the 6's.
                 149/298  =  14/28   by crossing out the 9's.
                 149/596  =  14/56   by crossing out the 9's.
                 149/894  =  14/84   by crossing out the 9's.
                 154/253  =  14/23   by crossing out the 5's.

               1234/4936  =  124/496   by crossing out the 3's.
               1239/6195  =  123/615   by crossing out the 9's.
               1246/3649  =  126/369   by crossing out the 4's.
               1249/2498  =  124/248   by crossing out the 9's.
               1259/6295  =  125/625   by crossing out the 9's.
               1279/6395  =  127/635   by crossing out the 9's.
               1283/5132  =  128/512   by crossing out the 3's.
               1297/2594  =  127/254   by crossing out the 9's.
               1297/3891  =  127/381   by crossing out the 9's.
               1298/2596  =  128/256   by crossing out the 9's.
               1298/3894  =  128/384   by crossing out the 9's.
               1298/5192  =  128/512   by crossing out the 9's.

             12349/24698  =  1234/2468   by crossing out the 9's.
             12356/67958  =  1236/6798   by crossing out the 5's.
             12358/14362  =  1258/1462   by crossing out the 3's.
             12358/15364  =  1258/1564   by crossing out the 3's.
             12358/17368  =  1258/1768   by crossing out the 3's.
             12358/19372  =  1258/1972   by crossing out the 3's.
             12358/21376  =  1258/2176   by crossing out the 3's.
             12358/25384  =  1258/2584   by crossing out the 3's.
             12359/61795  =  1235/6175   by crossing out the 9's.
             12364/32596  =  1364/3596   by crossing out the 2's.
             12379/61895  =  1237/6185   by crossing out the 9's.
             12386/32654  =  1386/3654   by crossing out the 2's.


═══════════════════════There are  4  2-digit fractions.════════════════════════
          For  2-digit fractions, there are  2  with crossed-out 6's.
          For  2-digit fractions, there are  2  with crossed-out 9's.

══════════════════════There are  122  3-digit fractions.═══════════════════════
          For  3-digit fractions, there are   9  with crossed-out 3's.
          For  3-digit fractions, there are   1  with crossed-out 4's.
          For  3-digit fractions, there are   6  with crossed-out 5's.
          For  3-digit fractions, there are  15  with crossed-out 6's.
          For  3-digit fractions, there are  16  with crossed-out 7's.
          For  3-digit fractions, there are  15  with crossed-out 8's.
          For  3-digit fractions, there are  60  with crossed-out 9's.

══════════════════════There are  660  4-digit fractions.═══════════════════════
          For  4-digit fractions, there are   14  with crossed-out 1's.
          For  4-digit fractions, there are   25  with crossed-out 2's.
          For  4-digit fractions, there are   92  with crossed-out 3's.
          For  4-digit fractions, there are   14  with crossed-out 4's.
          For  4-digit fractions, there are   29  with crossed-out 5's.
          For  4-digit fractions, there are   63  with crossed-out 6's.
          For  4-digit fractions, there are   16  with crossed-out 7's.
          For  4-digit fractions, there are   17  with crossed-out 8's.
          For  4-digit fractions, there are  390  with crossed-out 9's.

══════════════════════There are  5087  5-digit fractions.══════════════════════
          For  5-digit fractions, there are    75  with crossed-out 1's.
          For  5-digit fractions, there are    40  with crossed-out 2's.
          For  5-digit fractions, there are   376  with crossed-out 3's.
          For  5-digit fractions, there are    78  with crossed-out 4's.
          For  5-digit fractions, there are   209  with crossed-out 5's.
          For  5-digit fractions, there are   379  with crossed-out 6's.
          For  5-digit fractions, there are   591  with crossed-out 7's.
          For  5-digit fractions, there are   351  with crossed-out 8's.
          For  5-digit fractions, there are  2988  with crossed-out 9's.

```

