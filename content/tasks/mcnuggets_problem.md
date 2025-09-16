+++
title = "McNuggets Problem"
description = ""
date = 2019-10-18T16:02:58Z
aliases = []
[extra]
id = 22042
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "awk",
  "c",
  "clojure",
  "dart",
  "dyalect",
  "elixir",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "locomotive_basic",
  "mathematica",
  "minizinc",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "swift",
  "tailspin",
  "unix_shell",
  "zkl",
]
+++

{{task}} From [https://en.wikipedia.org/wiki/Coin_problem#McNugget_numbers Wikipedia]:

 The McNuggets version of the coin problem was introduced by Henri Picciotto,
 who included it in his algebra textbook co-authored with Anita Wah. Picciotto
 thought of the application in the 1980s while dining with his son at
 McDonald's, working the problem out on a napkin. A McNugget number is
 the total number of McDonald's Chicken McNuggets in any number of boxes.
 In the United Kingdom, the original boxes (prior to the introduction of
 the Happy Meal-sized nugget boxes) were of 6, 9, and 20 nuggets.

## Task

Calculate (from 0 up to a limit of 100) the largest non-McNuggets
number (a number ''n'' which cannot be expressed with ''6x + 9y + 20z = n''
where ''x'', ''y'' and ''z'' are natural numbers).


## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure McNugget is
   Limit : constant                      := 100;
   List  : array (0 .. Limit) of Boolean := (others => False);
   N     : Integer;
begin
   for A in 0 .. Limit / 6 loop
      for B in 0 .. Limit / 9 loop
         for C in 0 .. Limit / 20 loop
            N := A * 6 + B * 9 + C * 20;
            if N <= 100 then
               List (N) := True;
            end if;
         end loop;
      end loop;
   end loop;
   for N in reverse 1 .. Limit loop
      if not List (N) then
         Put_Line ("The largest non McNugget number is:" & Integer'Image (N));
         exit;
      end if;
   end loop;
end McNugget;
```

```txt

The largest non McNugget number is: 43

```



## ALGOL 68


```algol68
BEGIN
    # Solve the McNuggets problem: find the largest n <= 100 for which there #
    # are no non-negative integers x, y, z such that 6x + 9y + 20z = n       #
    INT max nuggets = 100;
    [ 0 : max nuggets ]BOOL sum;
    FOR i FROM LWB sum TO UPB sum DO sum[ i ] := FALSE OD;
    FOR x FROM 0 BY 6 TO max nuggets DO
        FOR y FROM 0 BY 9 TO max nuggets DO
            FOR z FROM 0 BY 20 TO max nuggets DO
                INT nuggets = x + y + z;
                IF nuggets <= max nuggets THEN sum[ nuggets ] := TRUE FI
            OD # z #
        OD # y #
    OD # x # ;
    # show the highest number that cannot be formed                          #
    INT largest := -1;
    FOR i FROM UPB sum BY -1 TO LWB sum WHILE largest := i; sum[ i ] DO SKIP OD;
    print( ( "The largest non McNugget number is: "
           , whole( largest, 0 )
           , newline
           )
         )
END
```

```txt

The largest non McNugget number is: 43

```



## AppleScript

Generalised for other set sizes, and for other triples of natural numbers.
Uses NSMutableSet, through the AppleScript ObjC interface:

```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions


on run
    set setNuggets to mcNuggetSet(100, 6, 9, 20)

    script isMcNugget
        on |λ|(x)
            setMember(x, setNuggets)
        end |λ|
    end script
    set xs to dropWhile(isMcNugget, enumFromThenTo(100, 99, 1))

    set setNuggets to missing value -- Clear ObjC pointer value
    if 0 < length of xs then
        item 1 of xs
    else
        "No unreachable quantities in this range"
    end if
end run

-- mcNuggetSet :: Int -> Int -> Int -> Int -> ObjC Set
on mcNuggetSet(n, mcx, mcy, mcz)
    set upTo to enumFromTo(0)
    script fx
        on |λ|(x)
            script fy
                on |λ|(y)
                    script fz
                        on |λ|(z)
                            set v to sum({mcx * x, mcy * y, mcz * z})
                            if 101 > v then
                                {v}
                            else
                                {}
                            end if
                        end |λ|
                    end script
                    concatMap(fz, upTo's |λ|(n div mcz))
                end |λ|
            end script
            concatMap(fy, upTo's |λ|(n div mcy))
        end |λ|
    end script
    setFromList(concatMap(fx, upTo's |λ|(n div mcx)))
end mcNuggetSet


-- GENERIC FUNCTIONS ----------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap


-- drop :: Int -> [a] -> [a]
-- drop :: Int -> String -> String
on drop(n, xs)
    set c to class of xs
    if c is not script then
        if c is not string then
            if n < length of xs then
                items (1 + n) thru -1 of xs
            else
                {}
            end if
        else
            if n < length of xs then
                text (1 + n) thru -1 of xs
            else
                ""
            end if
        end if
    else
        take(n, xs) -- consumed
        return xs
    end if
end drop

-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile :: (Char -> Bool) -> String -> String
on dropWhile(p, xs)
    set lng to length of xs
    set i to 1
    tell mReturn(p)
        repeat while i ≤ lng and |λ|(item i of xs)
            set i to i + 1
        end repeat
    end tell
    drop(i - 1, xs)
end dropWhile

-- enumFromThenTo :: Int -> Int -> Int -> [Int]
on enumFromThenTo(x1, x2, y)
    set xs to {}
    repeat with i from x1 to y by (x2 - x1)
        set end of xs to i
    end repeat
    return xs
end enumFromThenTo

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m)
    script
        on |λ|(n)
            if m ≤ n then
                set lst to {}
                repeat with i from m to n
                    set end of lst to i
                end repeat
                return lst
            else
                return {}
            end if
        end |λ|
    end script
end enumFromTo

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- sum :: [Num] -> Num
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldl(add, 0, xs)
end sum

-- NB All names of NSMutableSets should be set to *missing value*
-- before the script exits.
-- ( scpt files can not be saved if they contain ObjC pointer values )
-- setFromList :: Ord a => [a] -> Set a
on setFromList(xs)
    set ca to current application
    ca's NSMutableSet's ¬
        setWithArray:(ca's NSArray's arrayWithArray:(xs))
end setFromList

-- setMember :: Ord a => a -> Set a -> Bool
on setMember(x, objcSet)
    missing value is not (objcSet's member:(x))
end setMember
```

```txt
43
```


## AWK


```AWK

# syntax: GAWK -f MCNUGGETS_PROBLEM.AWK
# converted from Go
BEGIN {
    limit = 100
    for (a=0; a<=limit; a+=6) {
      for (b=a; b<=limit; b+=9) {
        for (c=b; c<=limit; c+=20) {
          arr[c] = 1
        }
      }
    }
    for (i=limit; i>=0; i--) {
      if (!arr[i]+0) {
        printf("%d\n",i)
        break
      }
    }
    exit(0)
}

```

```txt

43

```



## C



```c
#include <stdio.h>

int
main() {
    int max = 0, i = 0, sixes, nines, twenties;

loopstart: while (i < 100) {
        for (sixes = 0; sixes*6 < i; sixes++) {
            if (sixes*6 == i) {
                i++;
                goto loopstart;
            }

            for (nines = 0; nines*9 < i; nines++) {
                if (sixes*6 + nines*9 == i) {
                    i++;
                    goto loopstart;
                }

                for (twenties = 0; twenties*20 < i; twenties++) {
                    if (sixes*6 + nines*9 + twenties*20 == i) {
                        i++;
                        goto loopstart;
                    }
                }
            }
        }
        max = i;
        i++;
    }

    printf("Maximum non-McNuggets number is %d\n", max);

    return 0;
}
```


```txt

Maximum non-McNuggets number is 43

```


## Clojure


```clojure
(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [more (cart (rest colls))
          x (first colls)]
      (cons x more))))

(defn nuggets [[n6 n9 n20]] (+ (* 6 n6) (* 9 n9) (* 20 n20)))

(let [possible (distinct (map nuggets (cart (map range [18 13 6]))))
      mcmax (apply max (filter (fn [x] (not-any? #{x} possible)) (range 101)))]
  (printf "Maximum non-McNuggets number is %d\n" mcmax))
```

```txt
Maximum non-McNuggets number is 43
```



## Dart


```dart
import 'dart:math';
main() {
  var nuggets = List<int>.generate(101, (int index) => index);
  for (int small in List<int>.generate((100 ~/ (6 + 1)), (int index) => index)) {
    for (int medium in List<int>.generate((100 ~/ (9 + 1)), (int index) => index)) {
      for (int large in List<int>.generate((100 ~/ (20 + 1)), (int index) => index)) {
        nuggets.removeWhere((element) => element == 6 * small + 9 * medium + 20 * large);
      }
    }
  }
  print('Largest non-McNuggets number: ${nuggets.reduce(max).toString() ?? 'none'}.');
}
```


```txt
Largest non-McNuggets number: 43.
```



## Dyalect


```dyalect
func mcnugget(limit) {
    var sv = Array.empty(limit + 1, false)
    var s = 0
    while s <= limit {
        var n = s
        while n <= limit {
            var t = n
            while t <= limit {
                sv[t] = true
                t += 20
            }
            n += 9
        }
        s += 6
    }
    for i in limit..0 {
        if !sv[i] {
            print("Maximum non-McNuggets number is \(i)")
            return
        }
    }
}

mcnugget(100)
```


```txt
Maximum non-McNuggets number is 43
```



## Elixir


Uses MapSet and Comprehension


```Elixir
defmodule Mcnugget do
  def solve(limit) do
    0..limit
    |> MapSet.new()
    |> MapSet.difference(
      for(
        x <- 0..limit,
        y <- 0..limit,
        z <- 0..limit,
        Integer.mod(x, 6) == 0,
        Integer.mod(y, 9) == 0,
        Integer.mod(z, 20) == 0,
        x + y + z <= limit,
        into: MapSet.new(),
        do: x + y + z
      )
    )
    |> Enum.max()
  end
end

Mcnugget.solve(100) |> IO.puts

```


```txt
43
```


=={{header|F_Sharp|F#}}==

```fsharp

// McNuggets. Nigel Galloway: October 28th., 2018
let fN n g = Seq.initInfinite(fun ng->ng*n+g)|>Seq.takeWhile(fun n->n<=100)
printfn "%d" (Set.maxElement(Set.difference (set[1..100]) (fN 20 0|>Seq.collect(fun n->fN 9 n)|>Seq.collect(fun n->fN 6 n)|>Set.ofSeq)))

```

```txt

43

```



## Factor


```factor
USING: backtrack kernel math.ranges prettyprint sequences sets ;
101 <iota> [ 0 6 9 20 [ 100 swap <range> amb-lazy ] tri@ ] bag-of diff last .
```

```txt

43

```



## FreeBASIC


```freebasic

Dim As Integer l(100), a, b, c, n
For a = 0 To 100/6
    For b =  0 To 100/9
        For c = 0 To 100/20
            n = a*6 + b*9 + c*20
            If n <= 100 Then l(n) = true
        Next c
    Next b
Next a
For n = 100 To 1 Step -1
    If l(n) = false Then Print "El mayor número que no sea McNugget es:"; n: Exit For
Next n
End

```

```txt

El mayor número que no sea McNugget es: 43

```



## Go


```go
package main

import "fmt"

func mcnugget(limit int) {
    sv := make([]bool, limit+1) // all false by default
    for s := 0; s <= limit; s += 6 {
        for n := s; n <= limit; n += 9 {
            for t := n; t <= limit; t += 20 {
                sv[t] = true
            }
        }
    }
    for i := limit; i >= 0; i-- {
        if !sv[i] {
            fmt.Println("Maximum non-McNuggets number is", i)
            return
        }
    }
}

func main() {
    mcnugget(100)
}
```


```txt

Maximum non-McNuggets number is 43

```



## Haskell


```haskell
import Data.Set (Set, fromList, member)

gaps :: [Int]
gaps = dropWhile (`member` mcNuggets) [100,99 .. 1]

mcNuggets :: Set Int
mcNuggets =
  let size = enumFromTo 0 . quot 100
  in fromList $
     size 6 >>=
     \x ->
        size 9 >>=
        \y ->
           size 20 >>=
           \z ->
              let v = sum [6 * x, 9 * y, 20 * z]
              in [ v
                 | 101 > v ]

main :: IO ()
main =
  print $
  case gaps of
    x:_ -> show x
    []  -> "No unreachable quantities found ..."
```


Or equivalently, making use of the list comprehension notation:

```haskell
import Data.Set (Set, fromList, member)

gaps :: [Int]
gaps = dropWhile (`member` mcNuggets) [100,99 .. 1]

mcNuggets :: Set Int
mcNuggets =
  let size n = [0 .. quot 100 n]
  in fromList
       [ v
       | x <- size 6
       , y <- size 9
       , z <- size 20
       , let v = sum [6 * x, 9 * y, 20 * z]
       , 101 > v ]

main :: IO ()
main =
  print $
  case gaps of
    x:_ -> show x
    []  -> "No unreachable quantities found ..."
```


```txt
43
```



## Java


```Java
public class McNuggets {

    public static void main(String... args) {
        int[] SIZES = new int[] { 6, 9, 20 };
        int MAX_TOTAL = 100;
        // Works like Sieve of Eratosthenes
        int numSizes = SIZES.length;
        int[] counts = new int[numSizes];
        int maxFound = MAX_TOTAL + 1;
        boolean[] found = new boolean[maxFound];
        int numFound = 0;
        int total = 0;
        boolean advancedState = false;
        do {
            if (!found[total]) {
                found[total] = true;
                numFound++;
            }

            // Advance state
            advancedState = false;
            for (int i = 0; i < numSizes; i++) {
                int curSize = SIZES[i];
                if ((total + curSize) > MAX_TOTAL) {
                    // Reset to zero and go to the next box size
                    total -= counts[i] * curSize;
                    counts[i] = 0;
                }
                else {
                    // Adding a box of this size still keeps the total at or below the maximum
                    counts[i]++;
                    total += curSize;
                    advancedState = true;
                    break;
                }
            }

        } while ((numFound < maxFound) && advancedState);

        if (numFound < maxFound) {
            // Did not find all counts within the search space
            for (int i = MAX_TOTAL; i >= 0; i--) {
                if (!found[i]) {
                    System.out.println("Largest non-McNugget number in the search space is " + i);
                    break;
                }
            }
        }
        else {
            System.out.println("All numbers in the search space are McNugget numbers");
        }

        return;
    }
}
```

```txt
Largest non-McNugget number in the search space is 43
```



## JavaScript


```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {
        const
            size = n => enumFromTo(0)(
                quot(100, n)
            ),
            nuggets = new Set(
                size(6).flatMap(
                    x => size(9).flatMap(
                        y => size(20).flatMap(
                            z => {
                                const v = sum([6 * x, 9 * y, 20 * z]);
                                return 101 > v ? (
                                    [v]
                                ) : [];
                            }
                        ),
                    )
                )
            ),
            xs = dropWhile(
                x => nuggets.has(x),
                enumFromThenTo(100, 99, 1)
            );

        return 0 < xs.length ? (
            xs[0]
        ) : 'No unreachable quantities found in this range';
    };


    // GENERIC FUNCTIONS ----------------------------------

    // dropWhile :: (a -> Bool) -> [a] -> [a]
    const dropWhile = (p, xs) => {
        const lng = xs.length;
        return 0 < lng ? xs.slice(
            until(
                i => i === lng || !p(xs[i]),
                i => 1 + i,
                0
            )
        ) : [];
    };

    // enumFromThenTo :: Int -> Int -> Int -> [Int]
    const enumFromThenTo = (x1, x2, y) => {
        const d = x2 - x1;
        return Array.from({
            length: Math.floor(y - x2) / d + 2
        }, (_, i) => x1 + (d * i));
    };

    // ft :: Int -> Int -> [Int]
    const enumFromTo = m => n =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return console.log(
        main()
    );
})();
```

```txt
43
```



## J


Brute force solution: calculate all pure (just one kind of box) McNugget numbers which do not exceed 100, then compute all possible sums, and then remove those from the list of numbers up to 100 (which is obviously a McNugget number), then find the largest number remaining:


```j
>
./(i.100)-.,+/&>{(* i.@>.@%~&101)&.>6 9 20
43
```


Technically, we could have used 100 in place of 101 when we were finding how many pure McNugget numbers were in each series (because 100 is obviously a McNugget number), but it's not like that's a problem, either.

## jq

```jq
[
 [range(18) as $n6  |
  range(13) as $n9  |
  range(6)  as $n20 |
  ($n6 * 6 + $n9 * 9 + $n20 * 20)] |
 unique |
 . as $possible |
 range(101) |
 . as $n |
 select($possible|contains([$n])|not)
] |
max
```

```txt
43
```



## Julia

Simple brute force solution, though the BitSet would save memory considerably with larger max numbers.

```julia
function mcnuggets(max)
    b = BitSet(1:max)
    for i in 0:6:max, j in 0:9:max, k in 0:20:max
        delete!(b, i + j + k)
    end
    maximum(b)
end

println(mcnuggets(100))

```
 {{output}}
```txt

43

```



## Kotlin

```scala
// Version 1.2.71

fun mcnugget(limit: Int) {
    val sv = BooleanArray(limit + 1)  // all false by default
    for (s in 0..limit step 6)
        for (n in s..limit step 9)
            for (t in n..limit step 20) sv[t] = true

    for (i in limit downTo 0) {
        if (!sv[i]) {
            println("Maximum non-McNuggets number is $i")
            return
        }
    }
}

fun main(args: Array<String>) {
    mcnugget(100)
}
```


```txt

Maximum non-McNuggets number is 43

```



## Locomotive Basic


```locobasic
100 CLEAR
110 DIM a(100)
120 FOR a=0 TO 100/6
130   FOR b=0 TO 100/9
140     FOR c=0 TO 100/20
150       n=a*6+b*9+c*20
160       IF n<=100 THEN a(n)=1
170     NEXT c
180   NEXT b
190 NEXT a
200 FOR n=0 TO 100
210   IF a(n)=0 THEN l=n
220 NEXT n
230 PRINT"The Largest non McNugget number is:";l
240 END
```


```txt
The largest non McNugget number is: 43
```




## Mathematica


```mathematica
Complement[Range[100],
  Select[6 #[[1]] + 9 #[[2]] + 20 #[[3]] & /@
    Tuples[Range[0, 17], 3], # < 101 &]][[-1]]
```

```txt

43

```



## MiniZinc


```MiniZinc

%McNuggets. Nigel Galloway, August 27th., 2019
var 0..99: n;
constraint forall(x in 0..16,y in 0..11,z in 0..5)(6*x + 9*y + 20*z!=n);
solve maximize n;
output [show(n)]

```

```txt

43
----------

### ====


```


## Perl

```perl
use ntheory qw/forperm gcd vecmin/;

sub Mcnugget_number {
    my $counts = shift;

    return 'No maximum' if 1 < gcd @$counts;

    my $min = vecmin @$counts;
    my @meals;
    my @min;

    my $a = -1;
    while (1) {
        $a++;
        for my $b (0..$a) {
            for my $c (0..$b) {
                my @s = ($a, $b, $c);
                forperm {
                    $meals[
                        $s[$_[0]] * $counts->[0]
                      + $s[$_[1]] * $counts->[1]
                      + $s[$_[2]] * $counts->[2]
                    ] = 1;
                } @s;
            }
        }
        for my $i (0..$#meals) {
            next unless $meals[$i];
            if ($min[-1] and $i == ($min[-1] + 1)) {
                push @min, $i;
                last if $min == @min
            } else {
                @min = $i;
            }
        }
        last if $min == @min
    }
    $min[0] ? $min[0] - 1 : 0
}

for my $counts ([6,9,20], [6,7,20], [1,3,20], [10,5,18], [5,17,44], [2,4,6], [3,6,15]) {
    print 'Maximum non-Mcnugget number using ' . join(', ', @$counts) . ' is: ' . Mcnugget_number($counts) . "\n"
}
```

```txt
Maximum non-Mcnugget number using 6, 9, 20 is: 43
Maximum non-Mcnugget number using 6, 7, 20 is: 29
Maximum non-Mcnugget number using 1, 3, 20 is: 0
Maximum non-Mcnugget number using 10, 5, 18 is: 67
Maximum non-Mcnugget number using 5, 17, 44 is: 131
Maximum non-Mcnugget number using 2, 4, 6 is: No maximum
Maximum non-Mcnugget number using 3, 6, 15 is: No maximum
```



### Perl using Regex


```Perl
use strict;
use warnings;

$_ = 1 . 0 x 100;
1 while s/ (?=1) (?:.{6}|.{9}|.{20}) \K 0 /1/x;
/01*$/ and print "Maximum non-Mcnugget number is: $-[0]\n";
```

```txt
Maximum non-Mcnugget number is: 43
```



## Perl 6

No hard coded limits, no hard coded values. General purpose 3 value solver. Count values may be any 3 different positive integers, in any order, that are relatively prime.

Finds the smallest count value, then looks for the first run of consecutive count totals able to be generated, that is at least the length of the smallest count size. From then on, every number can be generated by simply adding multiples of the minimum count to each of the totals in that run.


```perl6
sub Mcnugget-number (*@counts) {

    return '∞' if 1 < [gcd] @counts;

    my $min = min @counts;
    my @meals;
    my @min;

    for ^Inf -> $a {
        for 0..$a -> $b {
            for 0..$b -> $c {
                ($a, $b, $c).permutations.map: { @meals[ sum $_ Z* @counts ] = True }
            }
        }
        for @meals.grep: so *, :k {
            if @min.tail and @min.tail + 1 == $_ {
                @min.push: $_;
                last if $min == +@min
            } else {
                @min = $_;
            }
        }
        last if $min == +@min
    }
    @min[0] ?? @min[0] - 1 !! 0
}

for (6,9,20), (6,7,20), (1,3,20), (10,5,18), (5,17,44), (2,4,6), (3,6,15) -> $counts {
    put "Maximum non-Mcnugget number using {$counts.join: ', '} is: ",
        Mcnugget-number(|$counts)
}
```

```txt
Maximum non-Mcnugget number using 6, 9, 20 is: 43
Maximum non-Mcnugget number using 6, 7, 20 is: 29
Maximum non-Mcnugget number using 1, 3, 20 is: 0
Maximum non-Mcnugget number using 10, 5, 18 is: 67
Maximum non-Mcnugget number using 5, 17, 44 is: 131
Maximum non-Mcnugget number using 2, 4, 6 is: ∞
Maximum non-Mcnugget number using 3, 6, 15 is: ∞
```



## Phix

```Phix
constant limit=100
sequence nuggets = repeat(false,limit+1)
for sixes=0 to limit by 6 do
    for nines=sixes to limit by 9 do
        for twenties=nines to limit by 20 do
            nuggets[twenties+1] = true
        end for
    end for
end for
printf(1,"Maximum non-McNuggets number is %d\n", rfind(false,nuggets)-1)
```

```txt

Maximum non-McNuggets number is 43

```

Also, since it is a bit more interesting, a
```Phix
function Mcnugget_number(sequence counts)

    if gcd(counts)>1 then return "No maximum" end if

    atom cmin = min(counts)
    sequence meals = {}
    sequence smin = {}

    integer a = -1
    while true do
        a += 1
        for b=0 to a do
            for c=0 to b do
                sequence s = {a, b, c}
                for i=1 to factorial(3) do
                    sequence p = permute(i,s)
                    integer k = sum(sq_mul(p,counts))+1
                    if k>length(meals) then meals &= repeat(0,k-length(meals)) end if
                    meals[k] = 1
                end for
            end for
        end for
        for i=1 to length(meals) do
            if meals[i] then
                if length(smin) and smin[$]+1=i-1 then
                    smin = append(smin,i-1)
                    if length(smin)=cmin then exit end if
                else
                    smin = {i-1}
                end if
            end if
        end for
        if length(smin)=cmin then exit end if
    end while
    return sprintf("%d",iff(smin[1]?smin[1]-1:0))
end function

constant tests = {{6,9,20}, {6,7,20}, {1,3,20}, {10,5,18}, {5,17,44}, {2,4,6}, {3,6,15}}
for i=1 to length(tests) do
    sequence ti = tests[i]
    printf(1,"Maximum non-Mcnugget number using %s is: %s\n",{sprint(ti),Mcnugget_number(ti)})
end for
```

```txt

Maximum non-Mcnugget number using {6,9,20} is: 43
Maximum non-Mcnugget number using {6,7,20} is: 29
Maximum non-Mcnugget number using {1,3,20} is: 0
Maximum non-Mcnugget number using {10,5,18} is: 67
Maximum non-Mcnugget number using {5,17,44} is: 131
Maximum non-Mcnugget number using {2,4,6} is: No maximum
Maximum non-Mcnugget number using {3,6,15} is: No maximum

```


## PowerShell

```powershell
$possible = @{}
For ($i=0; $i -lt 18; $i++) {
  For ($j=0; $j -lt 13; $j++) {
    For ( $k=0; $k -lt 6; $k++ ) {
      $possible[ $i*6 + $j*9 + $k*20 ] = $true
    }
  }
}

For ( $n=100; $n -gt 0; $n-- ) {
  If ($possible[$n]) {
    Continue
  }
  Else {
    Break
  }
}
Write-Host "Maximum non-McNuggets number is $n"
```

```txt
Maximum non-McNuggets number is 43
```



## PicoLisp


```PicoLisp
(de nuggets1 (M)
   (let Lst (range 0 M)
      (for A (range 0 M 6)
         (for B (range A M 9)
            (for C (range B M 20)
               (set (nth Lst (inc C))) ) ) )
      (apply max Lst) ) )
```

Generator from fiber:

```PicoLisp
(de nugg (M)
   (co 'nugget
      (for A (range 0 M 6)
         (for B (range A M 9)
            (for C (range B M 20)
               (yield (inc C)) ) ) ) ) )
(de nuggets2 (M)
   (let Lst (range 0 M)
      (while (nugg 100)
         (set (nth Lst @)) )
      (apply max Lst) ) )
```

Test versions against each other:

```PicoLis
(test
   T
   (=
      43
      (nuggets1 100)
      (nuggets2 100) ) )
```



## Python


### Python: REPL

It's a simple solution done on the command line:

```python
>>>
 from itertools import product
>>> nuggets = set(range(101))
>>> for s, n, t in product(range(100//6+1), range(100//9+1), range(100//20+1)):
	nuggets.discard(6*s + 9*n + 20*t)


>>> max(nuggets)
43
>>>
```


Single expression version (expect to be slower, however no noticeable difference on a Celeron B820 and haven't benchmarked):

```python
>>>
 from itertools import product
>>> max(x for x in range(100+1) if x not in
...   (6*s + 9*n + 20*t for s, n, t in
...     product(range(100//6+1), range(100//9+1), range(100//20+1))))
43
>>>
```



### Using Set Comprehension

```python

#Wherein I observe that Set Comprehension is not intrinsically dysfunctional. Nigel Galloway: October 28th., 2018
n = {n for x in range(0,101,20) for y in range(x,101,9) for n in range(y,101,6)}
g = {n for n in range(101)}
print(max(g.difference(n)))

```

```txt

43

```



### List monad

A composition of pure functions, including dropwhile, which shows a more verbose and unwieldy (de-sugared) route to list comprehension, and reveals the underlying mechanics of what the (compact and elegant) built-in syntax expresses. May help to build intuition for confident use of the latter.

Note that the innermost function wraps its results in a (potentially empty) list. The resulting list of lists, some empty, is then flattened by the concatenation component of '''bind'''.

```python
'''mcNuggets list monad'''

from itertools import (chain, dropwhile)


# mcNuggetsByListMonad :: Int -> Set Int
def mcNuggetsByListMonad(limit):
    '''McNugget numbers up to limit.'''

    box = size(limit)
    return set(bind(box(6))(
        lambda x:

        bind(box(9))(
            lambda y:

            bind(box(20))(
                lambda z: (

                    lambda v=sum([x, y, z]): (
                        [] if v > limit else [v]
                    )
                )()))))


# Which, for comparison, is equivalent to:

# mcNuggetsByComprehension :: Int -> Set Int
def mcNuggetsByComprehension(limit):
    '''McNuggets numbers up to limit'''
    box = size(limit)
    return {
        v for v in (
            sum([x, y, z])
            for x in box(6)
            for y in box(9)
            for z in box(20)
        ) if v <= limit
    }


# size :: Int -> Int -> [Int]
def size(limit):
    '''Multiples of n up to limit.'''
    return lambda n: enumFromThenTo(0)(n)(limit)


# TEST -----------------------------------------------------------
def main():
    '''List monad and set comprehension - parallel routes'''

    def test(limit):
        def go(nuggets):
            ys = list(dropwhile(
                lambda x: x in nuggets,
                enumFromThenTo(limit)(limit - 1)(1)
            ))
            return str(ys[0]) if ys else (
                'No unreachable targets in this range.'
            )
        return lambda nuggets: go(nuggets)

    def fName(f):
        return f.__name__

    limit = 100
    print(
        fTable(main.__doc__ + ':\n')(fName)(test(limit))(
            lambda f: f(limit)
        )([mcNuggetsByListMonad, mcNuggetsByComprehension])
    )


# GENERIC ABSTRACTIONS ------------------------------------

# bind (>>=) :: [a] -> (a -> [b]) -> [b]
def bind(xs):
    '''List monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.
    '''
    return lambda f: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# enumFromThenTo :: Int -> Int -> Int -> [Int]
def enumFromThenTo(m):
    '''Integer values enumerated from m to n
       with a step defined by nxt - m.
    '''
    def go(nxt, n):
        d = nxt - m
        return range(m, n - 1 if d < 0 else 1 + n, d)
    return lambda nxt: lambda n: list(go(nxt, n))


# FORMATTING ----------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt

List monad and set comprehension - parallel routes:

    mcNuggetsByListMonad -> 43
mcNuggetsByComprehension -> 43
```



## Racket

{{trans|Python}} (one of them)


```racket
#lang racket
(apply max (set->list (for*/fold ((s (list->set (range 1 101))))
                                 ((x (in-range 0 101 20))
                                  (y (in-range x 101 9))
                                  (n (in-range y 101 6)))
                        (set-remove s n))))
```



## REXX

This REXX version generalizes the problem (does not depend on fixed meal sizes),   and also checks for:
:*   a meal that doesn't include McNuggets    (in other words, zero nuggets)
:*   a meal size that includes a double order of nuggets
:*   a meal size that includes a single nugget   (which means, no largest McNugget number)
:*   excludes meals that have a multiple order of nuggets
:*   automatically computes the '''high''' value algebraically instead of using   '''100'''.

```rexx
/*REXX pgm solves the  McNuggets problem:  the largest McNugget number for given meals. */
parse arg y                                      /*obtain optional arguments from the CL*/
if y='' | y=","  then y= 6 9 20                  /*Not specified?  Then use the defaults*/
say 'The number of McNuggets in the serving sizes of: '    space(y)
$=
#= 0                                             /*the Y list must be in ascending order*/
z=.
       do j=1  for words(y);      _= word(y, j)  /*examine  Y  list for dups, neg, zeros*/
       if _==1               then signal done    /*Value ≡ 1?  Then all values possible.*/
       if _<1                then iterate        /*ignore zero and negative # of nuggets*/
       if wordpos(_, $)\==0  then iterate        /*search for duplicate values.         */
            do k=1  for #                        /*   "    "  multiple     "            */
            if _//word($,k)==0  then iterate j   /*a multiple of a previous value, skip.*/
            end   /*k*/
       $= $ _;      #= # + 1;     $.#= _         /*add─►list; bump counter; assign value*/
       end        /*j*/
if #<2                     then signal done      /*not possible, go and tell bad news.  */
_= gcd($)        if _\==1  then signal done      /* "     "       "  "   "    "    "    */
if #==2   then z= $.1 * $.2  -  $.1  -  $.2      /*special case, construct the result.  */
if z\==.  then signal done
h= 0                                             /*construct a theoretical high limit H.*/
       do j=2  for #-1;  _= j-1;       _= $._;       h= max(h, _ * $.j  -  _  -  $.j)
       end   /*j*/
@.=0
       do j=1  for #;    _= $.j                  /*populate the  Jth + Kth   summand.   */
         do a=_  by _  to h;           @.a= 1    /*populate every multiple as possible. */
         end   /*s*/

         do k=1  for h;  if \@.k  then iterate
         s= k + _;       @.s= 1                  /*add two #s;   mark as being possible.*/
         end   /*k*/
       end     /*j*/

       do z=h  by -1  for h  until \@.z          /*find largest integer not summed.     */
       end     /*z*/
say
done:  if z==.  then say 'The largest McNuggets number not possible.'
                else say 'The largest McNuggets number is: '          z
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gcd: procedure; $=;    do j=1  for arg();  $=$ arg(j);  end;  $= space($)
     parse var $ x $;     x= abs(x);
       do  while $\=='';  parse var $ y $;  y= abs(y);  if y==0  then iterate
         do  until y==0;  parse value  x//y  y   with   y  x;  end
       end;              return x
```

```txt

The number of McNuggets in the serving sizes of:  6 9 20

The largest McNuggets number is:  43

```



## Ruby

```ruby
def mcnugget(limit)
  sv = (0..limit).to_a

  (0..limit).step(6) do |s|
    (0..limit).step(9) do |n|
      (0..limit).step(20) do |t|
        sv.delete(s + n + t)
      end
    end
  end

  sv.max
end

puts(mcnugget 100)
```

```txt

43

```

Generic solution, allowing for more or less then 3 portion-sizes:

```ruby
limit = 100
nugget_portions = [6, 9, 20]

arrs = nugget_portions.map{|n| 0.step(limit, n).to_a }
hits = arrs.pop.product(*arrs).map(&:sum)
p ((0..limit).to_a - hits).max # => 43
```



## Rust

No hard limits.
Generalization of Rødseth’s Algorithm explained in [https://parramining.blogspot.com/2019/09/generalization-of-rdseths-algorithm-for.html post].
Working code: [https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1424a910a196fb3d0e964c754fbf325c Rust playground].

```rust
fn main() {
    let test_cases = vec![
        [6, 9, 20],
        [12, 14, 17],
        [12, 13, 34],
        [5, 9, 21],
        [10, 18, 21],
        [71, 98, 99],
        [7_074_047, 8_214_596, 9_098_139],
        [582_795_988, 1_753_241_221, 6_814_151_015],
        [4, 30, 16],
        [12, 12, 13],
        [6, 15, 1],
    ];
    for case in &test_cases {
        print!("g({}, {}, {}) = ", case[0], case[1], case[2]);
        println!(
            "{}",
            match frobenius(case.to_vec()) {
                Ok(g) => format!("{}", g),
                Err(e) => e,
            }
        );
    }
}

fn frobenius(unsorted_a: Vec<i64>) -> Result<i64, String> {
    let mut a = unsorted_a;
    a.sort();
    assert!(a[0] >= 1);
    if gcd(gcd(a[0], a[1]), a[2]) > 1 {
        return Err("Undefined".to_string());
    }
    let d12 = gcd(a[0], a[1]);
    let d13 = gcd(a[0] / d12, a[2]);
    let d23 = gcd(a[1] / d12, a[2] / d13);
    let mut a_prime = vec![a[0] / d12 / d13, a[1] / d12 / d23, a[2] / d13 / d23];
    a_prime.sort();
    let rod = if a_prime[0] == 1 {
        -1
    } else {
        // Rødseth’s Algorithm
        let mut a1 = a_prime[0];
        let mut s0 = congruence(a_prime[1], a_prime[2], a_prime[0]);
        let mut s = vec![a1];
        let mut q: Vec<i64> = vec![];
        while s0 != 0 {
            s.push(s0);
            let s1 = if s0 == 1 { 0 } else { s0 - (a1 % s0) };
            let q1 = (a1 + s1) / s0;
            q.push(q1);
            a1 = s0;
            s0 = s1;
        }
        let mut p = vec![0, 1];
        let mut r = (s[1] * a_prime[1] - p[1] * a_prime[2]) / a_prime[0];
        let mut i = 1;
        while r > 0 {
            let p_next = q[i - 1] * p[i] - p[i - 1];
            p.push(p_next);
            r = (s[i + 1] * a_prime[1] - p_next * a_prime[2]) / a_prime[0];
            i += 1;
        }
        let v = i - 1;
        -a_prime[0] + a_prime[1] * (s[v] - 1) + a_prime[2] * (p[v + 1] - 1)
            - (a_prime[1] * s[v + 1]).min(a_prime[2] * p[v])
    };
    Ok(rod * d12 * d13 * d23 + a[0] * (d23 - 1) + a[1] * (d13 - 1) + a[2] * (d12 - 1))
}

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn congruence(a: i64, c: i64, m: i64) -> i64 {
    // Solves ax ≡ c mod m
    let aa = a % m;
    let cc = (c + a * m) % m;
    if aa == 1 {
        cc
    } else {
        let y = congruence(m, -cc, aa);
        (m * y + cc) / aa
    }
}
```

```txt

g(6, 9, 20) = 43
g(12, 14, 17) = 61
g(12, 13, 34) = 79
g(5, 9, 21) = 22
g(10, 18, 21) = 65
g(71, 98, 99) = 1307
g(7074047, 8214596, 9098139) = 48494282357
g(582795988, 1753241221, 6814151015) = 173685179295403
g(4, 30, 16) = Undefined
g(12, 12, 13) = 131
g(6, 15, 1) = -1

```



## Swift



```swift
func maxNugget(limit: Int) -> Int {
  var (max, sixes, nines, twenties, i) = (0, 0, 0, 0, 0)

  mainLoop: while i < limit {
    sixes = 0

    while sixes * 6 < i {
      if sixes * 6 == i {
        i += 1
        continue mainLoop
      }

      nines = 0

      while nines * 9 < i {
        if sixes * 6 + nines * 9 == i {
          i += 1
          continue mainLoop
        }

        twenties = 0

        while twenties * 20 < i {
          if sixes * 6 + nines * 9 + twenties * 20 == i {
            i += 1
            continue mainLoop
          }

          twenties += 1
        }

        nines += 1
      }

      sixes += 1
    }

    max = i
    i += 1
  }

  return max
}

print(maxNugget(limit: 100))
```


```txt
43
```



## Tailspin


```tailspin

templates largestNonMcNuggetNumber
  @: { largest: 0, mcNuggetNumbers: [1..$+20 -> 0] };
  @.mcNuggetNumbers([6,9,20]): 1..3 -> 1;
  1..$ -> #
  $@.largest !
  <?($@.mcNuggetNumbers($) <0>)> @.largest: $;
  <> @.mcNuggetNumbers([$ + 6, $ + 9, $ + 20]): 1..3 -> 1;
end largestNonMcNuggetNumber

100 -> largestNonMcNuggetNumber -> !OUT::write

```

```txt

43

```


## UNIX Shell

```bash
possible=()
for (( i=0; i<18; ++i )); do
  for (( j=0; j<13; ++j )); do
    for (( k=0; k<6; ++k )); do
      (( n = i*6 + j*9 + k*20 ))
      if (( n )); then
        possible[n]=1
      fi
    done
  done
done

for (( n=100; n; n-- )); do
  if [[ -n ${possible[n]} ]; then
    continue
  fi
  break
done

printf 'Maximum non-McNuggets number is %d\n' $n
```

```txt
Maximum non-McNuggets number is 43
```

```bash
possible=
i=0
while [ $i -lt 18 ]; do
  j=0
  while [ $j -lt 13 ]; do
    k=0
    while [ $k -lt 6 ]; do
      possible="${possible+$possible }"`expr $i \* 6 + $j \* 9 + $k \* 20`
      k=`expr $k + 1`
    done
    j=`expr $j + 1`
  done
  i=`expr $i + 1`
done

n=100
while [ $n -gt 0 ]; do
  if echo "$possible" | tr ' ' '\n' | fgrep -qx $n; then
    n=`expr $n - 1`
    continue
  fi
  break
done
echo "Maximum non-McNuggets number is $n"
```

```txt
Maximum non-McNuggets number is 43
```



## zkl

```zkl
nuggets:=[0..101].pump(List());	// (0,1,2,3..101), mutable
foreach s,n,t in ([0..100/6],[0..100/9],[0..100/20])
   { nuggets[(6*s + 9*n + 20*t).min(101)]=0 }
println((0).max(nuggets));
```

```txt

43

```

