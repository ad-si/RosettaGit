+++
title = "Combinations with repetitions/Square Digit Chain"
description = ""
date = 2019-06-04T21:48:56Z
aliases = []
[extra]
id = 17934
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
[[Iterated digits squaring]] introduces RC the Project Euler Task #92. [[Combinations with repetitions]] introduce RC to the concept of generating all the combinations with repetitions of n types of things taken k at a time.

The purpose of this task is to combine these tasks as follows:
:The collections of k items will be taken from [0,1,4,9,16,25,36,49,64,81] and must be obtained using code from [[Combinations with repetitions]]. The collection of k zeroes is excluded.
:For each collection of k items determine if it translates to 1 using the rules from [[Iterated digits squaring]]
:For each collection which translates to 1 determine the number of different ways, c say, in which the k items can be uniquely ordered.
:Keep a running total of all the values of c obtained
:Answer the Project Euler Task #92 question (k=7).
:Answer the equivalent question for k=8,11,14.
:Optionally answer the question for k=17. These numbers will be larger than the basic integer type for many languages, if it is not easy to use larger numbers it is not necessary for this task.


## D

{{improve|D|See talk page}}

```d

// Count how many number chains for Natural Numbers < 10**K end with a value of 1.
//
import std.stdio, std.range;
 
const struct CombRep {
    immutable uint nt, nc;
    private const ulong[] combVal;
 
    this(in uint numType, in uint numChoice) pure nothrow @safe
    in {
        assert(0 < numType && numType + numChoice <= 64,
               "Valid only for nt + nc <= 64 (ulong bit size)");
    } body {
        nt = numType;
        nc = numChoice;
        if (nc == 0)
            return;
        ulong v  = (1UL << (nt - 1)) - 1;
 
        // Init to smallest number that has nt-1 bit set
        // a set bit is metaphored as a _type_ seperator.
        immutable limit = v << nc;
 
        ulong[] localCombVal;
        // Limit is the largest nt-1 bit set number that has nc
        // zero-bit a zero-bit means a _choice_ between _type_
        // seperators.
        while (v <= limit) {
            localCombVal ~= v;
            if (v == 0)
                break;
            // Get next nt-1 bit number.
            immutable t = (v | (v - 1)) + 1;
            v = t | ((((t & -t) / (v & -v)) >> 1) - 1);
        }
        this.combVal = localCombVal;
    }
 
    uint length() @property const pure nothrow @safe {
        return combVal.length;
    }
 
    uint[] opIndex(in uint idx) const pure nothrow @safe {
        return val2set(combVal[idx]);
    }
 
    int opApply(immutable int delegate(in ref uint[]) pure nothrow @safe dg)
    pure nothrow @safe {
        foreach (immutable v; combVal) {
            auto set = val2set(v);
            if (dg(set))
                break;
        }
        return 1;
    }
 
    private uint[] val2set(in ulong v) const pure nothrow @safe {
        // Convert bit pattern to selection set
        immutable uint bitLimit = nt + nc - 1;
        uint typeIdx = 0;
        uint[] set;
        foreach (immutable bitNum; 0 .. bitLimit)
            if (v & (1 << (bitLimit - bitNum - 1)))
                typeIdx++;
            else
                set ~= typeIdx;
        return set;
    }
}
 
// For finite Random Access Range.
auto combRep(R)(R types, in uint numChoice) /*pure*/ nothrow @safe
if (hasLength!R && isRandomAccessRange!R) {
    ElementType!R[][] result;
 
    foreach (const s; CombRep(types.length, numChoice)) {
        ElementType!R[] r;
        foreach (immutable i; s)
            r ~= types[i];
        result ~= r;
    }
 
    return result;
}
 
void main() {
    int K = 17;
    ulong[] F = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600, 6227020800, 87178291200, 1307674368000, 20922789888000, 355687428096000];
    int[] N = [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0];

    ulong z = 0;
    foreach (const e; combRep([0,1,4,9,16,25,36,49,64,81], K)) {
      int s = 0;
      foreach (const g; e) s += g;
      if (N[s] == 0) continue;
      int [int] n;
      foreach (const g; e) n[g] += 1;
	ulong gn = F[K];
      foreach (const g; n.byValue()) gn /= F[g];
	z += gn;
      }
      writefln ("\n(k=%d) In the range 1 to %d\n%d translate to 1 and %d translate to 89\n", K, (cast (ulong) (10))^^K-1,z,(cast (ulong) (10))^^K-1-z);
}

```

{{out}}

```txt

//(k=7) In the range 1 to 9999999
//1418853 translate to 1 and 8581146 translate to 89

//(k=8) In the range 1 to 99999999
//14255666 translate to 1 and 85744333 translate to 89

//(k=11) In the range 1 to 99999999999
//15091199356 translate to 1 and 84908800643 translate to 89

//(k=14) In the range 1 to 99999999999999
//13770853279684 translate to 1 and 86229146720315 translate to 89

//(k=17) In the range 1 to 99999999999999999
//12024696404768024 translate to 1 and 87975303595231975 translate to 89

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "math"
)

func endsWithOne(n int) bool {
    sum := 0
    for {
        for n > 0 {
            digit := n % 10
            sum += digit * digit
            n /= 10
        }
        if sum == 1 {
            return true
        }
        if sum == 89 {
            return false
        }
        n = sum
        sum = 0
    }
}

func main() {
    ks := [...]int{7, 8, 11, 14, 17}
    for _, k := range ks {
        sums := make([]int64, k*81+1)
        sums[0] = 1
        sums[1] = 0
        for n := 1; n <= k; n++ {
            for i := n * 81; i > 0; i-- {
                for j := 1; j < 10; j++ {
                    s := j * j
                    if s > i {
                        break
                    }
                    sums[i] += sums[i-s]
                }
            }
        }
        count1 := int64(0)
        for i := 1; i <= k*81; i++ {
            if endsWithOne(i) {
                count1 += sums[i]
            }
        }
        limit := int64(math.Pow10(k)) - 1
        fmt.Println("For k =", k, "in the range 1 to", limit)
        fmt.Println(count1, "numbers produce 1 and", limit-count1, "numbers produce 89\n")
    }
}
```


{{out}}

```txt

For k = 7 in the range 1 to 9999999
1418853 numbers produce 1 and 8581146 numbers produce 89

For k = 8 in the range 1 to 99999999
14255666 numbers produce 1 and 85744333 numbers produce 89

For k = 11 in the range 1 to 99999999999
15091199356 numbers produce 1 and 84908800643 numbers produce 89

For k = 14 in the range 1 to 99999999999999
13770853279684 numbers produce 1 and 86229146720315 numbers produce 89

For k = 17 in the range 1 to 99999999999999999
12024696404768024 numbers produce 1 and 87975303595231975 numbers produce 89

```



## Julia


```julia
using Combinatorics

function iterate(m::Integer)
    while m != 1 && m != 89
        s = 0
        while m > 0 # compute sum of squares of digits
            m, d = divrem(m, 10)
            s += d ^ 2
        end
        m = s
    end
    return m
end

function testitersquares(numdigits)
    items =  [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
    onecount, eightyninecount = 0, 0
    for combo in with_replacement_combinations(items, numdigits)
        if any(x -> x != 0, combo)
            pcount = Int(factorial(length(combo)) / 
                prod(y -> factorial(sum(x -> x == y, combo)), unique(combo)))
            if iterate(sum(combo)) == 89
                eightyninecount += pcount
            else
                onecount += pcount
            end
        end
    end
    println("For k = $numdigits, in the range 1 to $("9" ^ numdigits),\n" *
        "$onecount numbers produce 1 and $eightyninecount numbers produce 89.\n")
end

for i in [7, 8, 11, 14, 17]
    testitersquares(i)
end

```
{{out}}

```txt

For k = 2, in the range 1 to 99,
19 numbers produce 1 and 80 numbers produce 89.

For k = 7, in the range 1 to 9999999,
1418853 numbers produce 1 and 8581146 numbers produce 89.

For k = 8, in the range 1 to 99999999,
14255666 numbers produce 1 and 85744333 numbers produce 89.

For k = 11, in the range 1 to 99999999999,
15091199356 numbers produce 1 and 84908800643 numbers produce 89.

For k = 14, in the range 1 to 99999999999999,
13770853279684 numbers produce 1 and 86229146720315 numbers produce 89.

For k = 17, in the range 1 to 99999999999999999,
12024696404768024 numbers produce 1 and 87975303595231975 numbers produce 89.

```



## Kotlin

To achieve reasonable performance, the Kotlin entry for the [[Iterated digits squaring]] task already used a similar approach to that required by this task for k = 8.

So the following generalizes that code to deal with values of k up to 17 (which requires 64 bit integers) and to count numbers where the squared digits sum sequence eventually ends in 1 rather than 89, albeit the sum of both must of course be 10 ^ k - 1.

```scala
// version 1.1.51

fun endsWithOne(n: Int): Boolean {
    var digit: Int
    var sum = 0
    var nn = n
    while (true) {
        while (nn > 0) {
            digit = nn % 10
            sum += digit * digit
            nn /= 10
        }
        if (sum == 1) return true
        if (sum == 89) return false
        nn = sum
        sum  = 0
    }
}

fun main(args: Array<String>) {
    val ks = intArrayOf(7, 8, 11, 14, 17)
    for (k in ks) {
        val sums = LongArray(k * 81 + 1)
        sums[0] = 1
        sums[1] = 0
        var s: Int
        for (n in 1 .. k) {
            for (i in n * 81 downTo 1) {
                for (j in 1 .. 9) {
                    s = j * j
                    if (s > i) break
                    sums[i] += sums[i - s]
                }
            }
        }
        var count1 = 0L
        for (i in 1 .. k * 81) if (endsWithOne(i)) count1 += sums[i]
        val limit = Math.pow(10.0, k.toDouble()).toLong() - 1
        println("For k = $k in the range 1 to $limit")
        println("$count1 numbers produce 1 and ${limit - count1} numbers produce 89\n")
    }
}
```


{{out}}

```txt

For k = 7 in the range 1 to 9999999
1418853 numbers produce 1 and 8581146 numbers produce 89

For k = 8 in the range 1 to 99999999
14255666 numbers produce 1 and 85744333 numbers produce 89

For k = 11 in the range 1 to 99999999999
15091199356 numbers produce 1 and 84908800643 numbers produce 89

For k = 14 in the range 1 to 99999999999999
13770853279684 numbers produce 1 and 86229146720315 numbers produce 89

For k = 17 in the range 1 to 99999999999999999
12024696404768024 numbers produce 1 and 87975303595231975 numbers produce 89

```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use feature 'say';

#use bigint; # un-comment to support the k = 17 case 

sub comma { reverse ((reverse shift) =~ s/(.{3})/$1,/gr) =~ s/^,//r }

sub endsWithOne {
    my($n) = @_;
    my $digit;
    my $sum = 0;
    my $nn  = $n;
    while () {
        while ($nn > 0) {
            $digit = $nn % 10;
            $sum  += $digit**2;
            $nn    = int $nn / 10;
        }
        return 1 if $sum ==  1;
        return 0 if $sum == 89;
        $nn = $sum;
        $sum = 0;
    }
}

my @ks = <7 8 11 14>;

for my $k (@ks) {
    my @sums = <1 0>;
    my $s;
    for my $n (1 .. $k) {
        for my $i (reverse 1 .. $n*81) {
            for my $j (1 .. 9) {
                last if ($s = $j**2) > $i;
                $sums[$i] += $sums[$i-$s];
            }
        }
   }
   my $count1 = 0;
   for my $i (1 .. $k*81) { $count1 += $sums[$i] if endsWithOne($i) }
   my $limit = 10**$k - 1;
   say "For k = $k in the range 1 to " . comma $limit;
   say comma($count1) . ' numbers produce 1 and ' . comma($limit-$count1) . " numbers produce 89\n";
}
```

{{out}}

```txt
For k = 7 in the range 1 to 9,999,999
1,418,853 numbers produce 1 and 8,581,146 numbers produce 89

For k = 8 in the range 1 to 99,999,999
14,255,666 numbers produce 1 and 85,744,333 numbers produce 89

For k = 11 in the range 1 to 99,999,999,999
15,091,199,356 numbers produce 1 and 84,908,800,643 numbers produce 89

For k = 14 in the range 1 to 99,999,999,999,999
13,770,853,279,684 numbers produce 1 and 86,229,146,720,315 numbers produce 89
```



## Perl 6

{{trans|Kotlin}}

```perl6
#!/usr/bin/env perl6

use v6;

sub endsWithOne($n --> Bool) {
   my $digit;
   my $sum = 0;
   my $nn = $n;
   loop {
      while ($nn > 0) {
         $digit = $nn % 10;
         $sum += $digit²;
         $nn = $nn div 10;
      }
      ($sum == 1) and return True;
      ($sum == 89) and return False;
      $nn = $sum;
      $sum = 0;
   }
}

my @ks = (7, 8, 11, 14, 17);

for @ks -> $k {
   my @sums is default(0) = 1,0;
   my $s;
   for (1 .. $k) -> $n {
      for ($n*81 ... 1) -> $i {
         for (1 .. 9) -> $j {
            $s = $j²;
            if ($s > $i) { last };
            @sums[$i] += @sums[$i-$s];
         }
      }
   }
   my $count1 = 0;
   for (1 .. $k*81) -> $i { if (endsWithOne($i)) {$count1 += @sums[$i]} }
   my $limit = 10**$k - 1;
   say "For k = $k in the range 1 to $limit";
   say "$count1 numbers produce 1 and ",$limit-$count1," numbers produce 89";
}
```


{{out}}

```txt
For k = 7 in the range 1 to 9999999
1418853 numbers produce 1 and 8581146 numbers produce 89
For k = 8 in the range 1 to 99999999
14255666 numbers produce 1 and 85744333 numbers produce 89
For k = 11 in the range 1 to 99999999999
15091199356 numbers produce 1 and 84908800643 numbers produce 89
For k = 14 in the range 1 to 99999999999999
13770853279684 numbers produce 1 and 86229146720315 numbers produce 89
For k = 17 in the range 1 to 99999999999999999
12024696404768024 numbers produce 1 and 87975303595231975 numbers produce 89
```



## Phix

There is a solution to this on the [[Iterated_digits_squaring#Combinatorics_version|Iterated_digits_squaring]] page


## Ruby


```ruby

# Count how many number chains for Natural Numbers < 10**K end with a value of 1.
#
#  Nigel_Galloway
#  August 26th., 2014.
K = 17
F = Array.new(K+1){|n| n==0?1:(1..n).inject(:*)}   #Some small factorials
g = -> n, gn=[n,0], res=0 { while gn[0]>0
                              gn = gn[0].divmod(10)
                              res += gn[1]**2
                            end
                            return res==89?0:res
                           }
#An array: N[n]==1 means that n translates to 1, 0 means that it does not.
N = (G=Array.new(K*81+1){|n| n==0? 0:(i=g.call(n))==89 ? 0:i}).collect{|n| while n>1 do n = G[n] end; n }
z = 0   #Running count of numbers translating to 1
(0..9).collect{|n| n**2}.repeated_combination(K).each{|n|   #Iterate over unique digit combinations
    next if N[n.inject(:+)] == 0                            #Count only ones
    nn = Hash.new{0}                                        #Determine how many numbers this digit combination corresponds to
    n.each{|n| nn[n] += 1}                                  #and
    z += nn.values.inject(F[K]){|gn,n| gn/F[n]}             #Add to the count of numbers terminating in 1
}
puts "\nk=(#{K}) in the range 1 to #{10**K-1}\n#{z} numbers produce 1 and #{10**K-1-z} numbers produce 89"

```

{{out}}

```txt

#(k=7) in the range 1 to 9999999
#1418853 numbers produce 1 and 8581146 numbers produce 89

#(k=8) in the range 1 to 99999999
#14255666 numbers produce 1 and 85744333 numbers produce 89

#(k=11) in the range 1 to 99999999999
#15091199356 numbers produce 1 and 84908800643 numbers produce 89

#(k=14) in the range 1 to 99999999999999
#13770853279684 numbers produce 1 and 86229146720315 numbers produce 89

#(k=17) in the range 1 to 99999999999999999
#12024696404768024 numbers produce 1 and 87975303595231975 numbers produce 89

```




## zkl

{{trans|Ruby}}

```zkl
fcn countNumberChains(K){
   F:=(K+1).pump(List,fcn(n){ (1).reduce(n,'*,1) });   #Some small factorials
   g:=fcn(n){
      gn,res:=L(n,0),0;
      while(gn[0]>0){ gn=gn[0].divr(10); res+=gn[1].pow(2); }
      if(res==89) 0 else res
   };
   #An array: N[n]==1 means that n translates to 1, 0 means that it does not.
   n,G:=K*81+1,n.pump(List,g);
   N:=n.pump(List,'wrap(n){ n=g(n); while(n>1){ n=G[n] } n });
   z:=([0..9].pump(List,fcn(n){ n*n }):Utils.Helpers.combosKW(K,_)) #combos of (0,1,4,9,16,25,36,49,64,81)
   .reduce('wrap(z,ds){				#Iterate over unique digit combinations
       if(N[ds.sum(0)]==0) return(z);		#Count only ones
       nn:=Dictionary();                        #Determine how many numbers this digit combination corresponds to
       ds.pump(Void,nn.incV);                   #and (eg (0,0,0,0,0,1,9)-->(0:5, 1:1, 9:1)
       z + nn.values.reduce( 			#Add to the count of numbers terminating in 1
	   'wrap(gn,n){ gn/F[n] },F[K]);
   },0);
   println("\nk=(%d) in the range 1 to %,d".fmt(K,(10).pow(K)-1));
   println("%,d numbers produce 1 and %,d numbers produce 89".fmt(z,(10).pow(K)-1-z));
   z
}
```

combosKW(k,sequence) is lazy, which, in this case, is quite a bit faster than the non-lazy version.

```zkl
foreach K in (T(7,8,11,14,17)){ countNumberChains(K) }
```

{{out}}

```txt

k=(7) in the range 1 to 9,999,999
1,418,853 numbers produce 1 and 8,581,146 numbers produce 89

k=(8) in the range 1 to 99,999,999
14,255,666 numbers produce 1 and 85,744,333 numbers produce 89

k=(11) in the range 1 to 99,999,999,999
15,091,199,356 numbers produce 1 and 84,908,800,643 numbers produce 89

k=(14) in the range 1 to 99,999,999,999,999
13,770,853,279,684 numbers produce 1 and 86,229,146,720,315 numbers produce 89

k=(17) in the range 1 to 99,999,999,999,999,999
12,024,696,404,768,024 numbers produce 1 and 87,975,303,595,231,975 numbers produce 89

```

