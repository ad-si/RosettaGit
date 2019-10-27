+++
title = "Weird numbers"
description = ""
date = 2019-10-14T23:45:12Z
aliases = []
[extra]
id = 22228
[taxonomies]
categories = []
tags = []
+++

{{task}}
In number theory, a [[wp:weird number|weird number]] is a natural number that is abundant but not semiperfect.

In other words, the sum of the proper divisors (divisors including 1 but not itself) of the number is greater than the number, but no subset of those divisors sums to the number itself.

E.G. '''12''' is ''not'' a weird number. It is abundant; the proper divisors '''1, 2, 3, 4 <small>&</small> 6''' sum to '''16''', but it is semiperfect, '''6 + 4 + 2 == 12'''.

'''70''' ''is'' a weird number. It is abundant; the proper divisors '''1, 2, 5, 7, 10, 14 <small>&</small> 35''' sum to '''74''', but there is no subset of proper divisors that sum to '''70'''. 

;Task:

Find and display, here on this page, the first '''25''' weird numbers.

;See also:

:* [[oeis:A006037|OEIS:A006037 Weird numbers]] 
:* [[Abundant,_deficient_and_perfect_number_classifications|RosettaCode: Abundant, deficient and perfect number classifications]]
:* [[Proper_divisors|RosettaCode: Proper divisors]]



## AppleScript


Applescript is not the recommended apparatus for this kind of experiment.

(Though after about 6 seconds (on this system) it does yield the first 25, and intermediates can be logged in the Messages channel of macOS Script Editor).


```applescript
on run
    take(25, weirds())
    -- Gets there, but takes about 6 seconds on this system,
    -- (logging intermediates through the Messages channel, for the impatient :-)
end run


-- weirds :: Gen [Int]
on weirds()
    script
        property x : 1
        property v : 0
        on |λ|()
            repeat until isWeird(x)
                set x to 1 + x
            end repeat
            set v to x
            log v
            set x to 1 + x
            return v
        end |λ|
    end script
end weirds

-- isWeird :: Int -> Bool
on isWeird(n)
    set ds to descProperDivisors(n)
    set d to sum(ds) - n
    0 < d and not hasSum(d, ds)
end isWeird

-- hasSum :: Int -> [Int] -> Bool
on hasSum(n, xs)
    if {} ≠ xs then
        set h to item 1 of xs
        set t to rest of xs
        if n < h then
            hasSum(n, t)
        else
            n = h or hasSum(n - h, t) or hasSum(n, t)
        end if
    else
        false
    end if
end hasSum

-- GENERIC ------------------------------------------------

-- descProperDivisors :: Int -> [Int]
on descProperDivisors(n)
    if n = 1 then
        {1}
    else
        set realRoot to n ^ (1 / 2)
        set intRoot to realRoot as integer
        set blnPerfect to intRoot = realRoot
        
        -- isFactor :: Int -> Bool 
        script isFactor
            on |λ|(x)
                n mod x = 0
            end |λ|
        end script
        
        -- Factors up to square root of n,
        set lows to filter(isFactor, enumFromTo(1, intRoot))
        
        -- and cofactors of these beyond the square root,
        
        -- integerQuotient :: Int -> Int
        script integerQuotient
            on |λ|(x)
                (n / x) as integer
            end |λ|
        end script
        
        set t to rest of lows
        if blnPerfect then
            set xs to t
        else
            set xs to lows
        end if
        map(integerQuotient, t) & (reverse of xs)
    end if
end descProperDivisors

-- enumFromTo :: (Int, Int) -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromTo

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

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

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- sum :: [Num] -> Num
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script
    
    foldl(add, 0, xs)
end sum

-- take :: Int -> Gen [a] -> [a]
on take(n, xs)
    set ys to {}
    repeat with i from 1 to n
        set v to xs's |λ|()
        if missing value is v then
            return ys
        else
            set end of ys to v
        end if
    end repeat
    return ys
end take

-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if script is class of f then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

{{Out}}

```txt
{70, 836, 4030, 5830, 7192, 7912, 9272, 10430, 10570, 10792, 10990, 11410, 11690, 12110, 12530, 12670, 13370, 13510, 13790, 13930, 14770, 15610, 15890, 16030, 16310}
```



## C

{{trans|D}}

```c
#include "stdio.h"
#include "stdlib.h"
#include "stdbool.h"
#include "string.h"

struct int_a {
    int *ptr;
    size_t size;
};

struct int_a divisors(int n) {
    int *divs, *divs2, *out;
    int i, j, c1 = 0, c2 = 0;
    struct int_a array;

    divs = malloc(n * sizeof(int) / 2);
    divs2 = malloc(n * sizeof(int) / 2);
    divs[c1++] = 1;

    for (i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            j = n / i;
            divs[c1++] = i;
            if (i != j) {
                divs2[c2++] = j;
            }
        }
    }

    out = malloc((c1 + c2) * sizeof(int));
    for (int i = 0; i < c2; i++) {
        out[i] = divs2[i];
    }
    for (int i = 0; i < c1; i++) {
        out[c2 + i] = divs[c1 - i - 1];
    }
    array.ptr = out;
    array.size = c1 + c2;

    free(divs);
    free(divs2);
    return array;
}

bool abundant(int n, struct int_a divs) {
    int sum = 0;
    int i;
    for (i = 0; i < divs.size; i++) {
        sum += divs.ptr[i];
    }
    return sum > n;
}

bool semiperfect(int n, struct int_a divs) {
    if (divs.size > 0) {
        int h = *divs.ptr;
        int *t = divs.ptr + 1;

        struct int_a ta;
        ta.ptr = t;
        ta.size = divs.size - 1;

        if (n < h) {
            return semiperfect(n, ta);
        } else {
            return n == h
                || semiperfect(n - h, ta)
                || semiperfect(n, ta);
        }
    } else {
        return false;
    }
}

bool *sieve(int limit) {
    bool *w = calloc(limit, sizeof(bool));
    struct int_a divs;
    int i, j;

    for (i = 2; i < limit; i += 2) {
        if (w[i]) continue;
        divs = divisors(i);
        if (!abundant(i, divs)) {
            w[i] = true;
        } else if (semiperfect(i, divs)) {
            for (j = i; j < limit; j += i) {
                w[j] = true;
            }
        }
    }

    free(divs.ptr);
    return w;
}

int main() {
    bool *w = sieve(17000);
    int count = 0;
    int max = 25;
    int n;

    printf("The first 25 weird numbers:\n");
    for (n = 2; count < max; n += 2) {
        if (!w[n]) {
            printf("%d ", n);
            count++;
        }
    }
    printf("\n");

    free(w);
    return 0;
}
```

{{out}}

```txt
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310
```



## C++

{{trans|D}}

```cpp>#include <algorithm

#include <iostream>
#include <numeric>
#include <vector>

std::vector<int> divisors(int n) {
    std::vector<int> divs = { 1 };
    std::vector<int> divs2;

    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            int j = n / i;
            divs.push_back(i);
            if (i != j) {
                divs2.push_back(j);
            }
        }
    }

    std::copy(divs.cbegin(), divs.cend(), std::back_inserter(divs2));
    return divs2;
}

bool abundant(int n, const std::vector<int> &divs) {
    return std::accumulate(divs.cbegin(), divs.cend(), 0) > n;
}

template<typename IT>
bool semiperfect(int n, const IT &it, const IT &end) {
    if (it != end) {
        auto h = *it;
        auto t = std::next(it);
        if (n < h) {
            return semiperfect(n, t, end);
        } else {
            return n == h
                || semiperfect(n - h, t, end)
                || semiperfect(n, t, end);
        }
    } else {
        return false;
    }
}

template<typename C>
bool semiperfect(int n, const C &c) {
    return semiperfect(n, std::cbegin(c), std::cend(c));
}

std::vector<bool> sieve(int limit) {
    // false denotes abundant and not semi-perfect.
    // Only interested in even numbers >= 2
    std::vector<bool> w(limit);
    for (int i = 2; i < limit; i += 2) {
        if (w[i]) continue;
        auto divs = divisors(i);
        if (!abundant(i, divs)) {
            w[i] = true;
        } else if (semiperfect(i, divs)) {
            for (int j = i; j < limit; j += i) {
                w[j] = true;
            }
        }
    }
    return w;
}

int main() {
    auto w = sieve(17000);
    int count = 0;
    int max = 25;
    std::cout << "The first 25 weird numbers:";
    for (int n = 2; count < max; n += 2) {
        if (!w[n]) {
            std::cout << n << ' ';
            count++;
        }
    }
    std::cout << '\n';
    return 0;
}
```

{{out}}

```txt
The first 25 weird numbers:70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310
```



## D

{{trans|Kotlin}}

```d
import std.algorithm;
import std.array;
import std.stdio;

int[] divisors(int n) {
    int[] divs = [1];
    int[] divs2;
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            int j = n / i;
            divs ~= i;
            if (i != j) {
                divs2 ~= j;
            }
        }
    }
    divs2 ~= divs.reverse;
    return divs2;
}

bool abundant(int n, int[] divs) {
    return divs.sum() > n;
}

bool semiperfect(int n, int[] divs) {
    if (divs.length > 0) {
        auto h = divs[0];
        auto t = divs[1..$];
        if (n < h) {
            return semiperfect(n, t);
        } else {
            return n == h
                || semiperfect(n - h, t)
                || semiperfect(n, t);
        }
    } else {
        return false;
    }
}

bool[] sieve(int limit) {
    // false denotes abundant and not semi-perfect.
    // Only interested in even numbers >= 2
    auto w = uninitializedArray!(bool[])(limit);
    w[] = false;
    for (int i = 2; i < limit; i += 2) {
        if (w[i]) continue;
        auto divs = divisors(i);
        if (!abundant(i, divs)) {
            w[i] = true;
        } else if (semiperfect(i, divs)) {
            for (int j = i; j < limit; j += i) {
                w[j] = true;
            }
        }
    }
    return w;
}

void main() {
    auto w = sieve(17_000);
    int count = 0;
    int max = 25;
    writeln("The first 25 weird numbers:");
    for (int n = 2; count < max; n += 2) {
        if (!w[n]) {
            write(n, ' ');
            count++;
        }
    }
    writeln;
}
```

{{out}}

```txt
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310
```


=={{header|F#|F sharp}}==
{{trans|Kotlin}}

```fsharp
let divisors n = [1..n/2] |> List.filter (fun x->n % x = 0)

let abundant (n:int) divs = Seq.sum(divs) > n

let rec semiperfect (n:int) (divs:List<int>) =
    if divs.Length > 0 then
        let h = divs.Head
        let t = divs.Tail
        if n < h then
            semiperfect n t
        else
            n = h || (semiperfect (n - h) t) || (semiperfect n t)
    else false

let weird n =
    let d = divisors n
    if abundant n d then
        not(semiperfect n d)
    else
        false

[<EntryPoint>]
let main _ =
    let mutable i = 1
    let mutable count = 0
    while (count < 25) do
        if (weird i) then
            count <- count + 1
            printf "%d -> %d\n" count i
        i <- i + 1

    0 // return an integer exit code
```

{{out}}

```txt
1 -> 70
2 -> 836
3 -> 4030
4 -> 5830
5 -> 7192
6 -> 7912
7 -> 9272
8 -> 10430
9 -> 10570
10 -> 10792
11 -> 10990
12 -> 11410
13 -> 11690
14 -> 12110
15 -> 12530
16 -> 12670
17 -> 13370
18 -> 13510
19 -> 13790
20 -> 13930
21 -> 14770
22 -> 15610
23 -> 15890
24 -> 16030
25 -> 16310
```



## Factor

The <code>has-sum?</code> word is a translation of the Haskell function.

```factor
USING: combinators.short-circuit io kernel lists lists.lazy
locals math math.primes.factors prettyprint sequences ;
IN: rosetta-code.weird-numbers

:: has-sum? ( n seq -- ? )
    seq [ f ] [
        unclip-slice :> ( xs x )
        n x < [ n xs has-sum? ] [
            {
                [ n x = ]
                [ n x - xs has-sum? ]
                [ n xs has-sum? ]
            } 0||
        ] if
    ] if-empty ;

: weird? ( n -- ? )
    dup divisors but-last reverse
    { [ sum < ] [ has-sum? not ] } 2&& ;

: weirds ( -- list ) 1 lfrom [ weird? ] lfilter ;

: weird-numbers-demo ( -- )
    "First 25 weird numbers:" print
    25 weirds ltake list>array . ;

MAIN: weird-numbers-demo
```

{{out}}

```txt

First 25 weird numbers:
{
    70
    836
    4030
    5830
    7192
    7912
    9272
    10430
    10570
    10792
    10990
    11410
    11690
    12110
    12530
    12670
    13370
    13510
    13790
    13930
    14770
    15610
    15890
    16030
    16310
}

```



## Go


### Version 1

This takes advantage of Hout's analysis (see talk page) when testing for primitive semi-perfect numbers.

It also uses a sieve so we can make use of the fact that all multiples of a semi-perfect number are themselves semi-perfect.

Runs in less than 10 ms on an Intel Core i7-8565U machine. The first fifty (with a sieve size of 27000) takes roughly double that.

When run on the same machine, the 'tweaked' version (linked to below), which was supplied by Enter your username, is almost 3 times faster than this. 

```go
package main

import "fmt"

func divisors(n int) []int {
    divs := []int{1}
    divs2 := []int{}
    for i := 2; i*i <= n; i++ {
        if n%i == 0 {
            j := n / i
            divs = append(divs, i)
            if i != j {
                divs2 = append(divs2, j)
            }
        }
    }
    for i := len(divs) - 1; i >= 0; i-- {
        divs2 = append(divs2, divs[i])
    }
    return divs2
}

func abundant(n int, divs []int) bool {
    sum := 0
    for _, div := range divs {
        sum += div
    }
    return sum > n
}

func semiperfect(n int, divs []int) bool {
    le := len(divs)
    if le > 0 {
        h := divs[0]
        t := divs[1:]
        if n < h {
            return semiperfect(n, t)
        } else {
            return n == h || semiperfect(n-h, t) || semiperfect(n, t)
        }
    } else {
        return false
    }
} 

func sieve(limit int) []bool {
    // false denotes abundant and not semi-perfect.
    // Only interested in even numbers >= 2
    w := make([]bool, limit)
    for i := 2; i < limit; i += 2 {
        if w[i] {
            continue
        }
        divs := divisors(i)
        if !abundant(i, divs) {
            w[i] = true
        } else if semiperfect(i, divs) {
            for j := i; j < limit; j += i {
                w[j] = true
            }
        }
    }
    return w
}

func main() {
    w := sieve(17000)
    count := 0
    const max = 25
    fmt.Println("The first 25 weird numbers are:")
    for n := 2; count < max; n += 2 {
        if !w[n] {
            fmt.Printf("%d ", n)
            count++
        }
    }
    fmt.Println()
}
```


{{out}}

```txt

The first 25 weird numbers are:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310 

```

===Version 2 (Tweaked)===
[https://tio.run/##fVbbbts4EH33V0wNFCvBl9jxNkB9ydM@7g3o7pMRFIxF20wkUiWpKEHqb8@eIWXZitsVglDiDM/czgy9M29vpdg8ip2kQijdo54qSmM9Jf1t4fsL6ntVSF4L4ff9tNe7uiIra2u8JL9XjryhjZUCn5l6ciSsFS@kNAlySu9ySaVwbsjHyHlhvWOh38OeyjKIhc6oNvbR0b3xe5I6c@PettIbxlPOWJdoHPEpre@w0Cv1CE8wNl/FzddD2MtVwVvYSNjb8Zdv1ifb3Ah/82ui0zQNWltjSbEe1Bd4u6UpltGIXoOYH7Ul/VHRakWTs11@HvigpitSne3gzYpEWcL/hL@GpNKOCjAVfVgBoYv4s@OTNIiQNyTLSZrBXy05e06iPg8XIBtTvoSz6@n8bhgw15P5XRpBhMfJTD4jIrMNwh96sZ7cwZEu@KHXfYv/rfSV1V2fp2nvEBiyl3kpLXEZvTI6kMQUZQWWgAQ6E7nReK2KWGm8/FsGjFjPlEKlgx3IOOeTtnZfQ2y8Z4XeNbQ75ZT1ByvevXSWDWKjMSoLBSe3cuMjw2LOjh7cG5M3sChdLnXwL21IcUTcihy1ORzVNIuPeWyVvK2gw1XITGwaKWz@MqR7iYAkbSrv0StUlWc99EPL0wvLEVX/4mln0EsvyLkFYSzVe6lJG09abqRzwkZEz4k7suTk9vLS605@hoSUHKiTzE7@RkeAoPn9@@VxpkZMvJJPMkH3KX9s7LNkI54YWibhPQgv7isQBnzgScEBMfKogR4fz/ylcx47XlrpvMx4yMAKMlAV99I6ul3RddCtOQGFeJRJNDuk4MlpNoQWv1lgXUYRv4JRN0hNvX7g/jgVVELDUVHlXpU5nEVvzbpT5ppnTIukGOm6O2rqteK0b4wGCyp51m/HIddOwrOZ0vTFqXXSTjPDN9aw8lulLPLB/mj57MnXUjyem2e1JaZmCE@14R1Rkjb/bTdrQ@jfnWREmcns5NQBCUHpGPWs/GxhRCr2Vwo7jXPYIzQDboo9rhx0AZkngD5Uzr@br21d1GVd1Pu6/N@4qk/9z/fd10RFCvItN/6tsiIEGMuDgsCRQjwD@frTglzOHkxvZtPZgkNkyS0ksA8RdG4@zWZ0WIRxW2HkOZkj/Ogs6MyDktscBnA0DrfQjsH4n6ZO0kWkZ@wQl@NbvtMYf6nuE9fQNdwpGAufP58xCvf2@G@LqHKd9P/BRbtV1vn@MPg77NdS2axtC2HlvH@q38ZUOhicLCJjGgbH/SVDLLAZOczmP9RrzeRtjW6T/seMYE2nzbHBgHOCv3PH0p@4@1KqjcibyyIWg2OnWjhgyibuTkmlbycLlzQBwXpPwnLmulVF7SbNExTE0@5CI0o22ttwBfFG6OT5ChlRy@sJL4jo1ccxEiiESDkXfgmTr9KvwExgD3hlpMGA4@91Av1DaVWgBdj@nEJoQ15@NyPl9jguLf8ma8XAu@r4mjAyftEc3t7@Aw Link to a tweaked version at Try it Online!]<br/><br/>Runs in under 6.0ms on the Tio server. The first fifty (with a sieve size of 26,533) takes under 12.0ms. Comments added where tweaks were applied


## Haskell

{{Trans|Python}}

```haskell
weirds :: [Int]
weirds = filter abundantNotSemiperfect [1 ..]

abundantNotSemiperfect :: Int -> Bool
abundantNotSemiperfect n =
  let ds = descProperDivisors n
      d = sum ds - n
  in 0 < d && not (hasSum d ds)

hasSum :: Int -> [Int] -> Bool
hasSum _ [] = False
hasSum n (x:xs)
  | n < x = hasSum n xs
  | otherwise = (n == x) || hasSum (n - x) xs || hasSum n xs

descProperDivisors
  :: Integral a
  => a -> [a]
descProperDivisors n =
  let root = (floor . sqrt) (fromIntegral n :: Double)
      lows = filter ((0 ==) . rem n) [root,root - 1 .. 1]
      factors
        | n == root ^ 2 = tail lows 
        | otherwise = lows
  in tail $ reverse (quot n <$> lows) ++ factors

main :: IO ()
main =
  (putStrLn . unlines) $
  zipWith (\i x -> show i ++ (" -> " ++ show x)) [1 ..] (take 25 weirds)
```

{{Out}}

```txt
1 -> 70
2 -> 836
3 -> 4030
4 -> 5830
5 -> 7192
6 -> 7912
7 -> 9272
8 -> 10430
9 -> 10570
10 -> 10792
11 -> 10990
12 -> 11410
13 -> 11690
14 -> 12110
15 -> 12530
16 -> 12670
17 -> 13370
18 -> 13510
19 -> 13790
20 -> 13930
21 -> 14770
22 -> 15610
23 -> 15890
24 -> 16030
25 -> 16310
```



## JavaScript


### ES6

{{Trans|Python}}
{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // main :: IO ()
    const main = () =>
        take(25, weirds());


    // weirds :: Gen [Int]
    function* weirds() {
        let
            x = 1,
            i = 1;
        while (true) {
            x = until(isWeird, succ, x)
            console.log(i.toString() + ' -> ' + x)
            yield x;
            x = 1 + x;
            i = 1 + i;
        }
    }


    // isWeird :: Int -> Bool
    const isWeird = n => {
        const
            ds = descProperDivisors(n),
            d = sum(ds) - n;
        return 0 < d && !hasSum(d, ds)
    };

    // hasSum :: Int -> [Int] -> Bool
    const hasSum = (n, xs) => {
        const go = (n, xs) =>
            0 < xs.length ? (() => {
                const
                    h = xs[0],
                    t = xs.slice(1);
                return n < h ? (
                    go(n, t)
                ) : (
                    n == h || hasSum(n - h, t) || hasSum(n, t)
                );
            })() : false;
        return go(n, xs);
    };


    // descProperDivisors :: Int -> [Int]
    const descProperDivisors = n => {
        const
            rRoot = Math.sqrt(n),
            intRoot = Math.floor(rRoot),
            blnPerfect = rRoot === intRoot,
            lows = enumFromThenTo(intRoot, intRoot - 1, 1)
            .filter(x => (n % x) === 0);
        return (
            reverse(lows)
            .slice(1)
            .map(x => n / x)
        ).concat((blnPerfect ? tail : id)(lows))
    };


    // GENERIC FUNCTIONS ----------------------------


    // enumFromThenTo :: Int -> Int -> Int -> [Int]
    const enumFromThenTo = (x1, x2, y) => {
        const d = x2 - x1;
        return Array.from({
            length: Math.floor(y - x2) / d + 2
        }, (_, i) => x1 + (d * i));
    };

    // id :: a -> a
    const id = x => x;

    // reverse :: [a] -> [a]
    const reverse = xs =>
        'string' !== typeof xs ? (
            xs.slice(0).reverse()
        ) : xs.split('').reverse().join('');

    // succ :: Enum a => a -> a
    const succ = x => 1 + x;

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
1 -> 70
2 -> 836
3 -> 4030
4 -> 5830
5 -> 7192
6 -> 7912
7 -> 9272
8 -> 10430
9 -> 10570
10 -> 10792
11 -> 10990
12 -> 11410
13 -> 11690
14 -> 12110
15 -> 12530
16 -> 12670
17 -> 13370
18 -> 13510
19 -> 13790
20 -> 13930
21 -> 14770
22 -> 15610
23 -> 15890
24 -> 16030
25 -> 16310
```



## Julia


```Julia
using Primes

function nosuchsum(revsorted, num)
    if sum(revsorted) < num
        return true
    end
    for (i, n) in enumerate(revsorted)
        if n > num
            continue
        elseif n == num
            return false
        elseif !nosuchsum(revsorted[i+1:end], num - n)
            return false
        end
    end
    true
end

function isweird(n)
    if n < 70 || isodd(n)
        return false
    else
        f = [one(n)]
        for (p, x) in factor(n)
            f = reduce(vcat, [f*p^i for i in 1:x], init=f)
        end
        pop!(f)
        return sum(f) > n && nosuchsum(sort(f, rev=true), n)
    end
end

function testweird(N)
    println("The first $N weird numbers are: ")
    count, n = 0, 69
    while count < N
        if isweird(n)
            count += 1
            print("$n ")
        end
        n += 1
    end
end

testweird(25)

```
{{out}}

```txt

The first 25 weird numbers are:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310

```



## Kotlin

{{trans|Go}}
```scala
// Version 1.3.21

fun divisors(n: Int): List<Int> {
    val divs = mutableListOf(1)
    val divs2 = mutableListOf<Int>()
    var i = 2
    while (i * i <= n) {
        if (n % i == 0) {
            val j = n / i
            divs.add(i)
            if (i != j) divs2.add(j)
        }
        i++
    }
    divs2.addAll(divs.asReversed())
    return divs2
}

fun abundant(n: Int, divs: List<Int>) = divs.sum() > n

fun semiperfect(n: Int, divs: List<Int>): Boolean {
    if (divs.size > 0) {
        val h = divs[0]
        val t = divs.subList(1, divs.size)
        if (n < h) {
            return semiperfect(n, t)
        } else {
            return n == h || semiperfect(n-h, t) || semiperfect(n, t)
        }
    } else {
        return false
    }
}

fun sieve(limit: Int): BooleanArray {
    // false denotes abundant and not semi-perfect.
    // Only interested in even numbers >= 2
    val w = BooleanArray(limit)
    for (i in 2 until limit step 2) {
        if (w[i]) continue
        val divs = divisors(i)
        if (!abundant(i, divs)) {
            w[i] = true
        } else if (semiperfect(i, divs)) {
            for (j in i until limit step i) w[j] = true
        }
    }
    return w
}

fun main() {
    val w = sieve(17000)
    var count = 0
    val max = 25
    println("The first 25 weird numbers are:")
    var n = 2
    while (count < max) {
        if (!w[n]) {
            print("$n ")
            count++
        }
        n += 2
    }
    println()
}
```


{{output}}

```txt

The first 25 weird numbers are:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310 

```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use strict;
use feature 'say';

use List::Util 'sum';
use POSIX 'floor';
use Algorithm::Combinatorics 'subsets';
use ntheory <is_prime divisors>;

sub abundant {
    my($x) = @_;
    my $s = sum( my @l = is_prime($x) ? 1 : grep { $x != $_ } divisors($x) );
    $s > $x ? ($s, sort { $b <=> $a } @l) : ();
}

my(@weird,$n);
while () {
    $n++;
    my ($sum, @div) = abundant($n);
    next unless $sum;        # Weird number must be abundant, skip it if it isn't.
    next if $sum / $n > 1.1; # There aren't any weird numbers with a sum:number ratio greater than 1.08 or so.

    if ($n >= 10430 and (! int $n%70) and is_prime(int $n/70)) {
        # It's weird. All numbers of the form 70 * (a prime 149 or larger) are weird
    } else {
        my $next;
        my $l = shift @div;
        my $iter = subsets(\@div);
        while (my $s = $iter->next) {
            ++$next and last if sum(@$s) == $n - $l;
        }
        next if $next;
    }
    push @weird, $n;
    last if @weird == 25;
}

say "The first 25 weird numbers:\n" . join ' ', @weird;
```

{{out}}

```txt
The first 25 weird numbers:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310
```


Simpler and faster solution:
{{trans|Sidef}}
{{libheader|ntheory}}

```perl
use 5.010;
use strict;
use ntheory qw(vecsum divisors divisor_sum);

sub is_pseudoperfect {
    my ($n, $d, $s, $m) = @_;

    $d //= do { my @d = divisors($n); pop(@d); \@d };
    $s //= vecsum(@$d);
    $m //= $#$d;

    return 0 if $m < 0;

    while ($d->[$m] > $n) {
        $s -= $d->[$m--];
    }

    return 1 if ($n == $s or $d->[$m] == $n);

    is_pseudoperfect($n-$d->[$m], $d, $s-$d->[$m], $m - 1) ||
    is_pseudoperfect($n,          $d, $s-$d->[$m], $m - 1);
}

sub is_weird {
    my ($n) = @_;
    divisor_sum($n) > 2*$n and not is_pseudoperfect($n);
}

my @weird;
for (my $k = 1 ; @weird < 25 ; ++$k) {
    push(@weird, $k) if is_weird($k);
}

say "The first 25 weird numbers:\n@weird";
```

{{out}}

```txt

The first 25 weird numbers:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310

```



## Perl 6


```perl6
sub abundant (\x) {
    my @l = x.is-prime ?? 1 !! flat
    1, (2 .. x.sqrt.floor).map: -> \d {
         my \y = x div d;
         next if y * d !== x;
         d !== y ?? (d, y) !! d
    };
    (my $s = @l.sum) > x ?? ($s, |@l.sort(-*)) !! ();
}

my @weird = (2, 4, {|($_ + 4, $_ + 6)} ... *).map: -> $n {
    my ($sum, @div) = $n.&abundant;
    next unless $sum;        # Weird number must be abundant, skip it if it isn't.
    next if $sum / $n > 1.1; # There aren't any weird numbers with a sum:number ratio greater than 1.08 or so.
    if $n >= 10430 and ($n %% 70) and ($n div 70).is-prime {
        # It's weird. All numbers of the form 70 * (a prime 149 or larger) are weird
    } else {
        my $next;
        my $l = @div.shift;
        ++$next and last if $_.sum == $n - $l for @div.combinations;
        next if $next;
    }
    $n
}

put "The first 25 weird numbers:\n", @weird[^25];
```

{{out}}

```txt
The first 25 weird numbers:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310
```



## Phix

{{trans|Go}}
Sufficiently fast that I un-optimised it a bit to make it easier to follow.

```Phix
function abundant(integer n, sequence divs)
    return sum(divs) > n
end function

function semiperfect(integer n, sequence divs)
    if length(divs)=0 then return false end if
    integer h = divs[1]; divs = divs[2..$]
    return n=h
       or (n>h and semiperfect(n-h, divs))
       or          semiperfect(n, divs)
end function
 
function sieve(integer limit)
-- true denotes abundant and not semi-perfect.
-- only interested in even numbers >= 2
    sequence wierd := repeat(true,limit)
    for j=6 to limit by 6 do
        -- eliminate multiples of 3
        wierd[j] = false
    end for
    for i=2 to limit by 2 do
        if wierd[i] then
            sequence divs := factors(i,-1)
            if not abundant(i,divs) then
                wierd[i] = false
            elsif semiperfect(i,divs) then
                for j=i to limit by i do wierd[j] = false end for
            end if
        end if
    end for
    return wierd
end function

--constant MAX = 25, sieve_limit = 16313 
constant MAX = 50, sieve_limit = 26533 

sequence wierd := sieve(sieve_limit), res = {}
for i=2 to sieve_limit by 2 do
    if wierd[i] then
        res &= i
        if length(res)=MAX then exit end if
    end if
end for
printf(1,"The first %d weird numbers are: %v\n",{MAX,res})
```

{{out}}
<pre style="font-size: 11px">
The first 50 weird numbers are: {70,836,4030,5830,7192,7912,9272,10430,10570,10792,10990,11410,11690,12110,12530,12670,13370,13510,13790,13930,14770,15610,15890,16030,16310,
                       16730,16870,17272,17570,17990,18410,18830,18970,19390,19670,19810,20510,21490,21770,21910,22190,23170,23590,24290,24430,24710,25130,25690,26110,26530}

```



## Python



### Functional

The first 50 seem to take c. 300 ms
{{Works with|Python|3}}

```python
'''Weird numbers'''

from itertools import islice, repeat
from functools import reduce
from math import sqrt
from time import time


# weirds :: Gen [Int]
def weirds():
    '''Generator for weird numbers.
       (Abundant, but not semi-perfect)'''
    x = 1
    while True:
        x = until(isWeird)(succ)(x)
        yield x
        x = 1 + x


# isWeird :: Int -> Bool
def isWeird(n):
    '''Predicate :: abundant and not semi-perfect ?'''
    ds = descPropDivs(n)
    d = sum(ds) - n
    return 0 < d and not hasSum(d, ds)


# hasSum :: Int -> [Int] -> Bool
def hasSum(n, xs):
    '''Does any subset of xs sum to n ?
       (Assuming xs to be sorted in descending
       order of magnitude)'''
    def go(n, xs):
        if xs:
            h, t = xs[0], xs[1:]
            if n < h:  # Head too big. Forget it. Tail ?
                return go(n, t)
            else:
                # The head IS the target ?
                # Or the tail contains a sum for the
                # DIFFERENCE between the head and the target ?
                # Or the tail contains some OTHER sum for the target ?
                return n == h or go(n - h, t) or go(n, t)
        else:
            return False
    return go(n, xs)


# descPropDivs :: Int -> [Int]
def descPropDivs(n):
    '''Descending positive divisors of n,
       excluding n itself.'''
    root = sqrt(n)
    intRoot = int(root)
    blnSqr = root == intRoot
    lows = [x for x in range(1, 1 + intRoot) if 0 == n % x]
    return [
        n // x for x in (
            lows[1:-1] if blnSqr else lows[1:]
        )
    ] + list(reversed(lows))


# TEST ----------------------------------------------------


# main :: IO ()
def main():
    '''Test'''

    start = time()
    n = 50
    xs = take(n)(weirds())

    print(
        (tabulated('First ' + str(n) + ' weird numbers:\n')(
            lambda i: str(1 + i)
        )(str)(5)(
            index(xs)
        )(range(0, n)))
    )
    print(
        '\nApprox computation time: ' +
        str(int(1000 * (time() - start))) + ' ms'
    )


# GENERIC -------------------------------------------------


# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n,
       subdividing the contents of xs.
       Where the length of xs is not evenly divible,
       the final list will be shorter than n.'''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# index (!!) :: [a] -> Int -> a
def index(xs):
    '''Item at given (zero-based) index.'''
    return lambda n: None if 0 > n else (
        xs[n] if (
            hasattr(xs, "__getitem__")
        ) else next(islice(xs, n, None))
    )


# paddedMatrix :: a -> [[a]] -> [[a]]
def paddedMatrix(v):
    ''''A list of rows padded to equal length
        (where needed) with instances of the value v.'''
    def go(rows):
        return paddedRows(
            len(max(rows, key=len))
        )(v)(rows)
    return lambda rows: go(rows) if rows else []


# paddedRows :: Int -> a -> [[a]] -[[a]]
def paddedRows(n):
    '''A list of rows padded (but never truncated)
       to length n with copies of value v.'''
    def go(v, xs):
        def pad(x):
            d = n - len(x)
            return (x + list(repeat(v, d))) if 0 < d else x
        return list(map(pad, xs))
    return lambda v: lambda xs: go(v, xs) if xs else []


# showColumns :: Int -> [String] -> String
def showColumns(n):
    '''A column-wrapped string
       derived from a list of rows.'''
    def go(xs):
        def fit(col):
            w = len(max(col, key=len))

            def pad(x):
                return x.ljust(4 + w, ' ')
            return ''.join(map(pad, col))

        q, r = divmod(len(xs), n)
        return unlines(map(
            fit,
            transpose(paddedMatrix('')(
                chunksOf(q + int(bool(r)))(
                    xs
                )
            ))
        ))
    return lambda xs: go(xs)


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value. For numeric types, (1 +).'''
    return 1 + x if isinstance(x, int) else (
        chr(1 + ord(x))
    )


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        Int ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
          number of columns -> f -> value list -> tabular string.'''
    def go(xShow, fxShow, intCols, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + showColumns(intCols)([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: lambda nCols: (
        lambda f: lambda xs: go(
            xShow, fxShow, nCols, f, xs
        )
    )


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# transpose :: Matrix a -> Matrix a
def transpose(m):
    '''The rows and columns of the argument transposed.
       (The matrix containers and rows can be lists or tuples).'''
    if m:
        inner = type(m[0])
        z = zip(*m)
        return (type(m))(
            map(inner, z) if tuple != inner else z
        )
    else:
        return m


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# MAIN ----------------------------------------------------
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
First 50 weird numbers:

 1 -> 70       11 -> 10990    21 -> 14770    31 -> 18410    41 -> 22190    
 2 -> 836      12 -> 11410    22 -> 15610    32 -> 18830    42 -> 23170    
 3 -> 4030     13 -> 11690    23 -> 15890    33 -> 18970    43 -> 23590    
 4 -> 5830     14 -> 12110    24 -> 16030    34 -> 19390    44 -> 24290    
 5 -> 7192     15 -> 12530    25 -> 16310    35 -> 19670    45 -> 24430    
 6 -> 7912     16 -> 12670    26 -> 16730    36 -> 19810    46 -> 24710    
 7 -> 9272     17 -> 13370    27 -> 16870    37 -> 20510    47 -> 25130    
 8 -> 10430    18 -> 13510    28 -> 17272    38 -> 21490    48 -> 25690    
 9 -> 10570    19 -> 13790    29 -> 17570    39 -> 21770    49 -> 26110    
10 -> 10792    20 -> 13930    30 -> 17990    40 -> 21910    50 -> 26530    

Approx computation time: 278 ms
```



## REXX

This REXX program could be optimized by finding and using   ''primitive weird numbers''   and multiplying them by 

prime numbers <big>≥</big> '''149''' to find higher weird numbers,   but it would've added complexity to the REXX program.

```rexx
/*REXX program  finds and displays  N   weird numbers in a vertical format (with index).*/
parse arg n .                                    /*obtain optional arguments from the CL*/
if n=='' | n==","  then n= 25                    /*Not specified?  Then use the default.*/
#= 0                                             /*the count of weird numbers  (so far).*/
     do j=2  by 2  until #==n                    /*examine even integers 'til have 'nuff*/
     if \weird(j)  then iterate                  /*Not a  weird  number?  Then skip it. */
     #= # + 1                                    /*bump the count of  weird   numbers.  */
     say right(th(#), 30)   ' weird number is:' right(commas(j), 9)   /*display weird #.*/
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do ic=length(_)-3  to 1  by -3; _=insert(',', _, ic); end;  return _
th:     parse arg th;return th||word('th st nd rd',1+(th//10)*(th//100%10\==1)*(th//10<4))
/*──────────────────────────────────────────────────────────────────────────────────────*/
DaS:  procedure; parse arg x 1 z 1,b;       a= 1 /*get X,Z,B (the 1st arg); init A list.*/
      r= 0;         q= 1                         /* [↓] ══integer square root══     ___ */
           do while q<=z; q=q*4; end             /*R:  an integer which will be    √ X  */
           do while q>1;  q=q%4; _= z-r-q;  r= r%2;  if _>=0  then do;  z=_;  r= r+q;  end
           end   /*while q>1*/                   /* [↑]  compute the integer sqrt of  X.*/
      sig= a                                     /*initialize the sigma so far.     ___ */
          do j=2  to r - (r*r==x)                /*divide by some integers up to   √ X  */
          if x//j==0  then do;  a=a j;  b= x%j b /*if ÷, add both divisors to  α and ß. */
                                sig= sig +j +x%j /*bump the sigma (the sum of divisors).*/
                           end
          end   /*j*/                            /* [↑]  %  is the REXX integer division*/
                                                 /* [↓]  adjust for a square.        ___*/
      if j*j==x  then  return sig+j   a j b      /*Was  X  a square?    If so, add  √ X */
                       return sig     a   b      /*return the divisors  (both lists).   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
weird: procedure; parse arg x .                  /*obtain a # to be tested for weirdness*/
       if x<70 | x//3==0   then return 0         /*test if X is too low or multiple of 3*/
       parse value  DaS(x)  with  sigma divs     /*obtain sigma and the proper divisors.*/
       if sigma<=x  then  return 0               /*X  isn't abundant  (sigma too small).*/
       #= words(divs)                            /*count the number of divisors for  X. */
       if #<3   then return 0                    /*Not enough divisors?    "      "     */
       if #>15  then return 0                    /*number of divs > 15?  It's not weird.*/
       a.=                                       /*initialize the    A.   stemmed array.*/
           do i=1  for #;     _= word(divs, i)   /*obtain one of the divisors of  X.    */
           @.i= _;          a._= .               /*assign proper divs──►@ array; also id*/
           end   /*i*/
       df= sigma - x                             /*calculate difference between Σ and X.*/
       if a.df==.  then return 0                 /*Any divisor is equal to DF? Not weird*/
       c=0                                       /*zero combo counter; calc. power of 2.*/
           do p=1  for 2**#-2;         c= c + 1  /*convert P──►binary with leading zeros*/
           yy.c= strip( x2b( d2x(p) ),  'L', 0)  /*store this particular combination.   */
           end   /*p*/
                                                 /* [↓]  decreasing partitions is faster*/
           do part=c  by -1  for c;      s= 0    /*test of a partition add to the arg X.*/
           _= yy.part;           L= length(_)    /*obtain one method of partitioning.   */
             do cp=L  by -1  for L               /*obtain a sum of  a  partition.       */
             if substr(_,cp,1)  then do;  s= s + @.cp            /*1 bit?  Then add ──►S*/
                                          if s==x  then return 0 /*Sum equal?  Not weird*/
                                          if s==df then return 0 /*Sum = DF?    "    "  */
                                          if s>x   then iterate  /*Sum too big? Try next*/
                                     end
             end   /*cp*/
           end   /*part*/;           return 1    /*no sum equal to  X,  so  X  is weird.*/
```

{{out|output|text=  when using the input of:     <tt> 50 </tt>}}

```txt

                           1st  weird number is:        70
                           2nd  weird number is:       836
                           3rd  weird number is:     4,030
                           4th  weird number is:     5,830
                           5th  weird number is:     7,192
                           6th  weird number is:     7,912
                           7th  weird number is:     9,272
                           8th  weird number is:    10,430
                           9th  weird number is:    10,570
                          10th  weird number is:    10,792
                          11th  weird number is:    10,990
                          12th  weird number is:    11,410
                          13th  weird number is:    11,690
                          14th  weird number is:    12,110
                          15th  weird number is:    12,530
                          16th  weird number is:    12,670
                          17th  weird number is:    13,370
                          18th  weird number is:    13,510
                          19th  weird number is:    13,790
                          20th  weird number is:    13,930
                          21st  weird number is:    14,770
                          22nd  weird number is:    15,610
                          23rd  weird number is:    15,890
                          24th  weird number is:    16,030
                          25th  weird number is:    16,310
                          26th  weird number is:    16,730
                          27th  weird number is:    16,870
                          28th  weird number is:    17,272
                          29th  weird number is:    17,570
                          30th  weird number is:    17,990
                          31st  weird number is:    18,410
                          32nd  weird number is:    18,830
                          33rd  weird number is:    18,970
                          34th  weird number is:    19,390
                          35th  weird number is:    19,670
                          36th  weird number is:    19,810
                          37th  weird number is:    20,510
                          38th  weird number is:    21,490
                          39th  weird number is:    21,770
                          40th  weird number is:    21,910
                          41st  weird number is:    22,190
                          42nd  weird number is:    23,170
                          43rd  weird number is:    23,590
                          44th  weird number is:    24,290
                          45th  weird number is:    24,430
                          46th  weird number is:    24,710
                          47th  weird number is:    25,130
                          48th  weird number is:    25,690
                          49th  weird number is:    26,110
                          50th  weird number is:    26,530

```



## Sidef


```ruby
func is_pseudoperfect(n, d = n.divisors.slice(0, -2), s = d.sum, m = d.end) {

    return false if (m < 0)

    while (d[m] > n) {
        s -= d[m--]
    }

    return true if (n == s)
    return true if (d[m] == n)

    __FUNC__(n-d[m], d, s-d[m], m-1) || __FUNC__(n, d, s-d[m], m-1)
}

func is_weird(n) {
    (n.sigma > 2*n) && !is_pseudoperfect(n)
}

var w = (1..Inf -> lazy.grep(is_weird).first(25))
say "The first 25 weird numbers:\n#{w.join(' ')}"
```

{{out}}

```txt

The first 25 weird numbers:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310

```



## Visual Basic .NET

Performance is now on par with the python version, (but not quite up the Go version's performance), I applied what I could after reading the comments made by '''Hout''' on the discussion page.<br/>This program is similar to the structure of the '''Go''' example.  I found a couple of tweaks here and there to help with performance.  For example, the divisors list is built on a single array instead of joining two, and it calculates the sum while creating the divisors list.  The divisors list is headed by the difference between "n" and the sum of the divisors. The semiperfect() function checks for equality first (rather than chopping the head from the tail list first) to save a little more time.  And of course, the parallel execution.<br/><br/>A new feature is that one can calculate weird numbers up to any reasonable number, just enter a command line parameter of more than zero.  Another new feature is calculating weird numbers continuously until a key is pressed (like the spigot algorithm from the [[Pi]] task) - to do so, enter a command line parameter of less than 1.<br/>This has no sieve cache, as one must "know" beforehand what number to cache up to, (for best results).  Since there is no cache (runs slower), I added the parallel execution to make it run faster.<br/>I haven't let it run long enough to see how high it can get before crashing, I suspect it should happen once the weird number being tested is around Int32.MaxValue (2,147,483,647).  But long before that it will slow down quite a bit.  It takes around 17 minutes to get to the 10,732nd weird number, which is the first over 7 million (7,000,210).

```vbnet
Module Module1

    Dim resu As New List(Of Integer)

    Function TestAbundant(n As Integer, ByRef divs As List(Of Integer)) As Boolean
        divs = New List(Of Integer)
        Dim sum As Integer = -n : For i As Integer = Math.Sqrt(n) To 1 Step -1
            If n Mod i = 0 Then divs.Add(i) : Dim j As Integer = n / i : divs.Insert(0, j) : sum += i + j
        Next : divs(0) = sum - divs(0) : Return divs(0) > 0
    End Function

    Function subList(src As List(Of Integer), Optional first As Integer = Integer.MinValue) As List(Of Integer)
        subList = src.ToList : subList.RemoveAt(1)
    End Function

    Function semiperfect(divs As List(Of Integer)) As Boolean
        If divs.Count < 2 Then Return False
        Select Case divs.First.CompareTo(divs(1))
            Case 0 : Return True
            Case -1 : Return semiperfect(subList(divs))
            Case 1 : Dim t As List(Of Integer) = subList(divs) : t(0) -= divs(1)
                If semiperfect(t) Then Return True Else t(0) = divs.First : Return semiperfect(t)
        End Select : Return False ' execution can't get here, just for compiler warning
    End Function

    Function Since(et As TimeSpan) As String ' big ugly routine to prettify the elasped time
        If et > New TimeSpan(2000000) Then
            Dim s As String = " " & et.ToString(), p As Integer = s.IndexOf(":"), q As Integer = s.IndexOf(".")
            If q < p Then s = s.Insert(q, "Days") : s = s.Replace("Days.", "Days, ")
            p = s.IndexOf(":") : s = s.Insert(p, "h") : s = s.Replace("h:", "h ")
            p = s.IndexOf(":") : s = s.Insert(p, "m") : s = s.Replace("m:", "m ")
            s = s.Replace(" 0", " ").Replace(" 0h", " ").Replace(" 0m", " ") & "s"
            Return s.TrimStart()
        Else
            If et > New TimeSpan(1500) Then
                Return et.TotalMilliseconds.ToString() & "ms"
            Else
                If et > New TimeSpan(15) Then
                    Return (et.TotalMilliseconds * 1000.0).ToString() & "µs"
                Else
                    Return (et.TotalMilliseconds * 1000000.0).ToString() & "ns"
                End If
            End If
        End If
    End Function

    Sub Main(args As String())
        Dim sw As New Stopwatch, st As Integer = 2, stp As Integer = 1020, count As Integer = 0
        Dim max As Integer = 25, halted As Boolean = False
        If args.Length > 0 Then _
            Dim t As Integer = Integer.MaxValue : If Integer.TryParse(args(0), t) Then max = If(t > 0, t, Integer.MaxValue)
        If max = Integer.MaxValue Then
            Console.WriteLine("Calculating weird numbers, press a key to halt.")
            stp *= 10
        Else
            Console.WriteLine("The first {0} weird numbers:", max)
        End If
        If max < 25 Then stp = 140
        sw.Start()
        Do : Parallel.ForEach(Enumerable.Range(st, stp),
            Sub(n)
                Dim divs As List(Of Integer) = Nothing
                If TestAbundant(n, divs) AndAlso Not semiperfect(divs) Then
                    SyncLock resu : resu.Add(n) : End SyncLock
                End If
            End Sub)
            If resu.Count > 0 Then
                resu.Sort()
                If count + resu.Count > max Then
                    resu = resu.Take(max - count).ToList
                End If
                Console.Write(String.Join(" ", resu) & " ")
                count += resu.Count : resu.Clear()
            End If
            If Console.KeyAvailable Then Console.ReadKey() : halted = True : Exit Do
            st += stp
        Loop Until count >= max
        sw.Stop()
        If max < Integer.MaxValue Then
            Console.WriteLine(vbLf & "Computation time was {0}.", Since(sw.Elapsed))
            If halted Then Console.WriteLine("Halted at number {0}.", count)
        Else
            Console.WriteLine(vbLf & "Computation time was {0} for the first {1} weird numbers.", Since(sw.Elapsed), count)
        End If
    End Sub
End Module
```

{{out}}
Without any command line parameters:

```txt
The first 25 weird numbers:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310
Computation time was 37.4931ms.
```

With command line arguments = 50

```txt
The first 50 weird numbers:
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310 16730 16870 17272 17570 17990 18410 18830 18970 19390 19670 19810 20510 21490 21770 21910 22190 23170 23590 24290 24430 24710 25130 25690 26110 26530
Computation time was 47.6589ms.
```

With command line arguments = 0

```txt
Calculating weird numbers, press a key to halt.
70 836 4030 5830 7192 7912 9272 10430 10570 10792 10990 11410 11690 12110 12530 12670 13370 13510 13790 13930 14770 15610 15890 16030 16310 16730 16870 17272 17570 17990 18410 18830 18970 19390 19670 19810 20510 21490 21770 21910 22190 23170 23590 24290 24430 24710 25130 25690 26110 26530 26810 27230 27790 28070 28630 29330 29470 30170 30310 30730 31010 31430 31990 32270 32410 32690 33530 34090 34370 34930 35210 35630 36470 36610 37870 38290 38990 39410 39830 39970 40390 41090 41510 41930 42070 42490 42910 43190 43330 44170 44870 45010 45290 45356 45710 46130 46270 47110 47390 47810 48370 49070 49630 50330 50890 51310 51730 52010 52570 52990 53270 53830 54110 55090 55790 56630 56770 57470 57610 57890 58030 58730 59710 59990 60130 60410 61390 61670 61810 62090 63490 63770 64330 65030 65590 65870 66290 66710 67690 67970 68390 68810 69370 69790 70630 70910 71330 71470 72170 72310 72730 73430 73570 73616 74270 74410 74830 76090 76370 76510 76790 77210 77630 78190 78610 79030 80570 80710 81410 81970 82670 83090 83312 83510 84070 84910 85190 85610 86030 86170 86590 87430 88130 89390 89530 89810 90230 90370 90790 91070 91210 91388 91490 92330 92470 92890 95270 95690 96110 96670 97930 98630 99610 99890 100030 100310 100730 101290 101570 101710 102130 102970 103670 103810 104090 104230 104510 104930 105770 106610 107170 108010 108430 108710 109130 109690 109970 110530 110810 111790 112070 112490 112630 112910 113072 113330 113470 113890 114590 115990 116410 116690 116830 118510 118790 118930 119630 120470 120610 121310 121870 122290 122710 123130 124390 124810 125090 125230 126070 126770 127610 128170 129290 130270 130690 130970 131110 131390 131530 132230 133070 133490 133910 135170 135310 136430 136570 138110 138530 139090 139510 139790 139930 140210 140770
Computation time was 153.3649ms for the first 285 weird numbers.
```

Tail-end of a longer session:

```txt
6981310 6983108 6983270 6983690 6985090 6985510 6986630 6987190 6987610 6988030 6988310 6988730 6990130 6990970 6991390 6991468 6991670 6992930 6993070 6993490 6994610 6995030 6996484 6997270 6997970 6998110 6999230 6999370 7000210 7001330 7003010 7003172 7003430 7003990 7004830 7007210 7007630 7008890 7009030
Computation time was 17m 9.0062776s for the first 10742 weird numbers.
```

