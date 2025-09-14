+++
title = "Partition an integer X into N primes"
description = ""
date = 2019-08-22T14:31:33Z
aliases = []
[extra]
id = 21327
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "cpp",
  "csharp",
  "d",
  "factor",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "lingo",
  "mathematica",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "vbscript",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

Partition a positive integer   '''X'''   into   '''N'''   distinct primes.


Or, to put it in another way:

Find   '''N'''   unique primes such that they add up to   '''X'''.


Show in the output section the sum   '''X'''   and the   '''N'''   primes in ascending order separated by plus ('''+''') signs:
 partition  '''99809'''  with   1 prime.
 partition    '''18'''   with   2 primes.
 partition    '''19'''   with   3 primes.
 partition    '''20'''   with   4 primes.
 partition   '''2017'''  with  24 primes.
 partition  '''22699'''  with   1,  2,  3,  <u>and</u>  4  primes.
 partition  '''40355'''  with   3 primes.

The output could/should be shown in a format such as:

 Partitioned 19 with 3 primes: 3+5+11

::*   Use any spacing that may be appropriate for the display.
::*   You need not validate the input(s).
::*   Use the lowest primes possible;   use '''18 = 5+13''',   not '''18 = 7+11'''.
::*   You only need to show one solution.

This task is similar to factoring an integer.


## Related tasks

:*   [[Count in factors]]
:*   [[Prime decomposition]]
:*   [[Factors of an integer]]
:*   [[Sieve of Eratosthenes]]
:*   [[Primality by trial division]]
:*   [[Factors of a Mersenne number]]
:*   [[Factors of a Mersenne number]]
:*   [[Sequence of primes by trial division]]





## C++

```cpp
#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>

std::vector<int> primes;

struct Seq {
public:
    bool empty() {
        return p < 0;
    }

    int front() {
        return p;
    }

    void popFront() {
        if (p == 2) {
            p++;
        } else {
            p += 2;
            while (!empty() && !isPrime(p)) {
                p += 2;
            }
        }
    }

private:
    int p = 2;

    bool isPrime(int n) {
        if (n < 2) return false;
        if (n % 2 == 0) return n == 2;
        if (n % 3 == 0) return n == 3;

        int d = 5;
        while (d * d <= n) {
            if (n % d == 0) return false;
            d += 2;
            if (n % d == 0) return false;
            d += 4;
        }
        return true;
    }
};

// generate the first 50,000 primes and call it good
void init() {
    Seq seq;

    while (!seq.empty() && primes.size() < 50000) {
        primes.push_back(seq.front());
        seq.popFront();
    }
}

bool findCombo(int k, int x, int m, int n, std::vector<int>& combo) {
    if (k >= m) {
        int sum = 0;
        for (int idx : combo) {
            sum += primes[idx];
        }

        if (sum == x) {
            auto word = (m > 1) ? "primes" : "prime";
            printf("Partitioned %5d with %2d %s ", x, m, word);
            for (int idx = 0; idx < m; ++idx) {
                std::cout << primes[combo[idx]];
                if (idx < m - 1) {
                    std::cout << '+';
                } else {
                    std::cout << '\n';
                }
            }
            return true;
        }
    } else {
        for (int j = 0; j < n; j++) {
            if (k == 0 || j > combo[k - 1]) {
                combo[k] = j;
                bool foundCombo = findCombo(k + 1, x, m, n, combo);
                if (foundCombo) {
                    return true;
                }
            }
        }
    }

    return false;
}

void partition(int x, int m) {
    if (x < 2 || m < 1 || m >= x) {
        throw std::runtime_error("Invalid parameters");
    }

    std::vector<int> filteredPrimes;
    std::copy_if(
        primes.cbegin(), primes.cend(),
        std::back_inserter(filteredPrimes),
        [x](int a) { return a <= x; }
    );

    int n = filteredPrimes.size();
    if (n < m) {
        throw std::runtime_error("Not enough primes");
    }

    std::vector<int> combo;
    combo.resize(m);
    if (!findCombo(0, x, m, n, combo)) {
        auto word = (m > 1) ? "primes" : "prime";
        printf("Partitioned %5d with %2d %s: (not possible)\n", x, m, word);
    }
}

int main() {
    init();

    std::vector<std::pair<int, int>> a{
        {99809,  1},
        {   18,  2},
        {   19,  3},
        {   20,  4},
        { 2017, 24},
        {22699,  1},
        {22699,  2},
        {22699,  3},
        {22699,  4},
        {40355,  3}
    };

    for (auto& p : a) {
        partition(p.first, p.second);
    }

    return 0;
}
```

```txt
Partitioned 99809 with  1 prime 99809
Partitioned    18 with  2 primes 5+13
Partitioned    19 with  3 primes 3+5+11
Partitioned    20 with  4 primes: (not possible)
Partitioned  2017 with 24 primes 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partitioned 22699 with  1 prime 22699
Partitioned 22699 with  2 primes 2+22697
Partitioned 22699 with  3 primes 3+5+22691
Partitioned 22699 with  4 primes 2+3+43+22651
Partitioned 40355 with  3 primes 3+139+40213
```



## C#

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using static System.Linq.Enumerable;

public static class Rosetta
{
    static void Main()
    {
        foreach ((int x, int n) in new [] {
            (99809, 1),
            (18, 2),
            (19, 3),
            (20, 4),
            (2017, 24),
            (22699, 1),
            (22699, 2),
            (22699, 3),
            (22699, 4),
            (40355, 3)
        }) {
            Console.WriteLine(Partition(x, n));
        }
    }

    public static string Partition(int x, int n) {
        if (x < 1 || n < 1) throw new ArgumentOutOfRangeException("Parameters must be positive.");
        string header = $"{x} with {n} {(n == 1 ? "prime" : "primes")}: ";
        int[] primes = SievePrimes(x).ToArray();
        if (primes.Length < n) return header + "not enough primes";
        int[] solution = CombinationsOf(n, primes).FirstOrDefault(c => c.Sum() == x);
        return header + (solution == null ? "not possible" : string.Join("+", solution);
    }

    static IEnumerable<int> SievePrimes(int bound) {
        if (bound < 2) yield break;
        yield return 2;

        BitArray composite = new BitArray((bound - 1) / 2);
        int limit = ((int)(Math.Sqrt(bound)) - 1) / 2;
        for (int i = 0; i < limit; i++) {
            if (composite[i]) continue;
            int prime = 2 * i + 3;
            yield return prime;
            for (int j = (prime * prime - 2) / 2; j < composite.Count; j += prime) composite[j] = true;
        }
        for (int i = limit; i < composite.Count; i++) {
            if (!composite[i]) yield return 2 * i + 3;
        }
    }

    static IEnumerable<int[]> CombinationsOf(int count, int[] input) {
        T[] result = new T[count];
        foreach (int[] indices in Combinations(input.Length, count)) {
            for (int i = 0; i < count; i++) result[i] = input[indices[i]];
            yield return result;
        }
    }

    static IEnumerable<int[]> Combinations(int n, int k) {
        var result = new int[k];
        var stack = new Stack<int>();
        stack.Push(0);
        while (stack.Count > 0) {
            int index = stack.Count - 1;
            int value = stack.Pop();
            while (value < n) {
                result[index++] = value++;
                stack.Push(value);
                if (index == k) {
                    yield return result;
                    break;
                }
            }
        }
    }

}
```

```txt

99809 with 1 prime: 99809
18 with 2 primes: 5+13
19 with 3 primes: 3+5+11
20 with 4 primes: not possible
2017 with 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
22699 with 1 prime: 22699
22699 with 2 primes: 2+22697
22699 with 3 primes: 3+5+22691
22699 with 4 primes: 2+3+43+22651
40355 with 3 primes: 3+139+40213

```



## D

```D
import std.array : array;
import std.range : take;
import std.stdio;

bool isPrime(int n) {
    if (n < 2) return false;
    if (n % 2 == 0) return n == 2;
    if (n % 3 == 0) return n == 3;

    int d = 5;
    while (d*d <= n) {
        if (n % d == 0) return false;
        d += 2;
        if (n % d == 0) return false;
        d += 4;
    }
    return true;
}

auto generatePrimes() {
    struct Seq {
        int p = 2;

        bool empty() {
            return p < 0;
        }

        int front() {
            return p;
        }

        void popFront() {
            if (p==2) {
                p++;
            } else {
                p += 2;
                while (!empty && !p.isPrime) {
                    p += 2;
                }
            }
        }
    }

    return Seq();
}

bool findCombo(int k, int x, int m, int n, int[] combo) {
    import std.algorithm : map, sum;
    auto getPrime = function int(int idx) => primes[idx];

    if (k >= m) {
        if (combo.map!getPrime.sum == x) {
            auto word = (m > 1) ? "primes" : "prime";
            writef("Partitioned %5d with %2d %s ", x, m, word);
            foreach (i; 0..m) {
                write(primes[combo[i]]);
                if (i < m-1) {
                    write('+');
                } else {
                    writeln();
                }
            }
            return true;
        }
    } else {
        foreach (j; 0..n) {
            if (k==0 || j>combo[k-1]) {
                combo[k] = j;
                bool foundCombo = findCombo(k+1, x, m, n, combo);
                if (foundCombo) {
                    return true;
                }
            }
        }
    }
    return false;
}

void partition(int x, int m) {
    import std.exception : enforce;
    import std.algorithm : filter;
    enforce(x>=2 && m>=1 && m<x);

    auto lessThan = delegate int(int a) => a<=x;
    auto filteredPrimes = primes.filter!lessThan.array;
    auto n = filteredPrimes.length;
    enforce(n>=m, "Not enough primes");

    int[] combo = new int[m];
    if (!findCombo(0, x, m, n, combo)) {
        auto word = (m > 1) ? "primes" : "prime";
        writefln("Partitioned %5d with %2d %s: (not possible)", x, m, word);
    }
}

int[] primes;
void main() {
    // generate first 50,000 and call it good
    primes = generatePrimes().take(50_000).array;

    auto a = [
        [99809,  1],
        [   18,  2],
        [   19,  3],
        [   20,  4],
        [ 2017, 24],
        [22699,  1],
        [22699,  2],
        [22699,  3],
        [22699,  4],
        [40355,  3]
    ];

    foreach(p; a) {
        partition(p[0], p[1]);
    }
}
```


```txt
Partitioned 99809 with  1 prime 99809
Partitioned    18 with  2 primes 5+13
Partitioned    19 with  3 primes 3+5+11
Partitioned    20 with  4 primes: (not possible)
Partitioned  2017 with 24 primes 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partitioned 22699 with  1 prime 22699
Partitioned 22699 with  2 primes 2+22697
Partitioned 22699 with  3 primes 3+5+22691
Partitioned 22699 with  4 primes 2+3+43+22651
Partitioned 40355 with  3 primes 3+139+40213
```


=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Partition an integer as the sum of n primes. Nigel Galloway: November 27th., 2017
let rcTask n ng =
  let rec fN i g e l = seq{
    match i with
    |1 -> if isPrime g then yield Some (g::e) else yield None
    |_ -> yield! Seq.mapi (fun n a->fN (i-1) (g-a) (a::e) (Seq.skip (n+1) l)) (l|>Seq.takeWhile(fun n->(g-n)>n))|>Seq.concat}
  match fN n ng [] primes |> Seq.tryPick id with
    |Some n->printfn "%d is the sum of %A" ng n
    |_     ->printfn "No Solution"

```

```txt

rcTask 1 99089 -> 99089 is the sum of [99089]
rcTask 2 18    -> 18 is the sum of [13; 5]
rcTask 3 19    -> 19 is the sum of [11; 5; 3]
rcTask 4 20    -> No Solution
rcTask 24 2017 -> 2017 is the sum of [1129; 97; 79; 73; 71; 67; 61; 59; 53; 47; 43; 41; 37; 31; 29; 23; 19; 17; 13; 11; 7; 5; 3; 2]
rcTask 1 2269  -> 2269 is the sum of [2269]
rcTask 2 2269  -> 2269 is the sum of [2267; 2]
rcTask 3 2269  -> 2269 is the sum of [2243; 23; 3]
rcTask 4 2269  -> 2269 is the sum of [2251; 13; 3; 2]
rcTask 3 40355 -> 40355 is the sum of [40213; 139; 3]

```



## Factor


```factor
USING: formatting fry grouping kernel math.combinatorics
math.parser math.primes sequences ;

: partition ( x n -- str )
    over [ primes-upto ] 2dip '[ sum _ = ] find-combination
    [ number>string ] map "+" join ;

: print-partition ( x n seq -- )
    [ "no solution" ] when-empty
    "Partitioned %5d with %2d primes: %s\n" printf ;

{ 99809 1 18 2 19 3 20 4 2017 24 22699 1 22699 2 22699 3 22699
  4 40355 3 } 2 group
[ first2 2dup partition print-partition ] each
```

```txt

Partitioned 99809 with  1 primes: 99809
Partitioned    18 with  2 primes: 5+13
Partitioned    19 with  3 primes: 3+5+11
Partitioned    20 with  4 primes: no solution
Partitioned  2017 with 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partitioned 22699 with  1 primes: 22699
Partitioned 22699 with  2 primes: 2+22697
Partitioned 22699 with  3 primes: 3+5+22691
Partitioned 22699 with  4 primes: 2+3+43+22651
Partitioned 40355 with  3 primes: 3+139+40213

```



## Go

... though uses a sieve to generate the relevant primes.

```go
package main

import (
    "fmt"
    "log"
)

var (
    primes     = sieve(100000)
    foundCombo = false
)

func sieve(limit uint) []uint {
    primes := []uint{2}
    c := make([]bool, limit+1) // composite = true
    // no need to process even numbers > 2
    p := uint(3)
    for {
        p2 := p * p
        if p2 > limit {
            break
        }
        for i := p2; i <= limit; i += 2 * p {
            c[i] = true
        }
        for {
            p += 2
            if !c[p] {
                break
            }
        }
    }
    for i := uint(3); i <= limit; i += 2 {
        if !c[i] {
            primes = append(primes, i)
        }
    }
    return primes
}

func findCombo(k, x, m, n uint, combo []uint) {
    if k >= m {
        sum := uint(0)
        for _, c := range combo {
            sum += primes[c]
        }
        if sum == x {
            s := "s"
            if m == 1 {
                s = " "
            }
            fmt.Printf("Partitioned %5d with %2d prime%s: ", x, m, s)
            for i := uint(0); i < m; i++ {
                fmt.Print(primes[combo[i]])
                if i < m-1 {
                    fmt.Print("+")
                } else {
                    fmt.Println()
                }
            }
            foundCombo = true
        }
    } else {
        for j := uint(0); j < n; j++ {
            if k == 0 || j > combo[k-1] {
                combo[k] = j
                if !foundCombo {
                    findCombo(k+1, x, m, n, combo)
                }
            }
        }
    }
}

func partition(x, m uint) error {
    if !(x >= 2 && m >= 1 && m < x) {
        return fmt.Errorf("x must be at least 2 and m in [1, x)")
    }
    n := uint(0)
    for _, prime := range primes {
        if prime <= x {
            n++
        }
    }
    if n < m {
        return fmt.Errorf("not enough primes")
    }
    combo := make([]uint, m)
    foundCombo = false
    findCombo(0, x, m, n, combo)
    if !foundCombo {
        s := "s"
        if m == 1 {
            s = " "
        }
        fmt.Printf("Partitioned %5d with %2d prime%s: (impossible)\n", x, m, s)
    }
    return nil
}

func main() {
    a := [...][2]uint{
        {99809, 1}, {18, 2}, {19, 3}, {20, 4}, {2017, 24},
        {22699, 1}, {22699, 2}, {22699, 3}, {22699, 4}, {40355, 3},
    }
    for _, p := range a {
        err := partition(p[0], p[1])
        if err != nil {
            log.Println(err)
        }
    }
}
```


```txt

Partitioned 99809 with  1 prime : 99809
Partitioned    18 with  2 primes: 5+13
Partitioned    19 with  3 primes: 3+5+11
Partitioned    20 with  4 primes: (impossible)
Partitioned  2017 with 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partitioned 22699 with  1 prime : 22699
Partitioned 22699 with  2 primes: 2+22697
Partitioned 22699 with  3 primes: 3+5+22691
Partitioned 22699 with  4 primes: 2+3+43+22651
Partitioned 40355 with  3 primes: 3+139+40213

```



## Haskell


```haskell
{-# LANGUAGE TupleSections #-}

import Data.List (delete, intercalate)

-- PRIME PARTITIONS ----------------------------------------------------------
partition :: Int -> Int -> [Int]
partition x n
  | n <= 1 =
    [ x
    | last ps == x ]
  | otherwise = partition_ ps x n
  where
    ps = takeWhile (<= x) primes
    partition_ ps_ x 1 =
      [ x
      | x `elem` ps_ ]
    partition_ ps_ x n =
      let found = foldMap partitionsFound ps_
      in nullOr found [] (head found)
      where
        partitionsFound p =
          let r = x - p
              rs = partition_ (delete p (takeWhile (<= r) ps_)) r (n - 1)
          in nullOr rs [] [p : rs]

-- TEST ----------------------------------------------------------------------
main :: IO ()
main =
  mapM_ putStrLn $
  (\(x, n) ->
      (intercalate
         " -> "
         [ justifyLeft 9 ' ' (show (x, n))
         , let xs = partition x n
           in nullOr xs "(no solution)" (intercalate "+" (show <$> xs))
         ])) <$>
  concat
    [ [(99809, 1), (18, 2), (19, 3), (20, 4), (2017, 24)]
    , (22699, ) <$> [1 .. 4]
    , [(40355, 3)]
    ]

-- GENERIC --------------------------------------------------------------------
justifyLeft :: Int -> Char -> String -> String
justifyLeft n c s = take n (s ++ replicate n c)

nullOr
  :: Foldable t1
  => t1 a -> t -> t -> t
nullOr expression nullValue orValue =
  if null expression
    then nullValue
    else orValue

primes :: [Int]
primes =
  2 :
  pruned
    [3 ..]
    (listUnion
       [ (p *) <$> [p ..]
       | p <- primes ])
  where
    pruned :: [Int] -> [Int] -> [Int]
    pruned (x:xs) (y:ys)
      | x < y = x : pruned xs (y : ys)
      | x == y = pruned xs ys
      | x > y = pruned (x : xs) ys
    listUnion :: [[Int]] -> [Int]
    listUnion = foldr union []
      where
        union (x:xs) ys = x : union_ xs ys
        union_ (x:xs) (y:ys)
          | x < y = x : union_ xs (y : ys)
          | x == y = x : union_ xs ys
          | x > y = y : union_ (x : xs) ys
```

```txt
(99809,1) -> 99809
(18,2)    -> 5+13
(19,3)    -> 3+5+11
(20,4)    -> (no solution)
(2017,24) -> 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
(22699,1) -> 22699
(22699,2) -> 2+22697
(22699,3) -> 3+5+22691
(22699,4) -> 2+3+43+22651
(40355,3) -> 3+139+40213
```



## J


```j

load 'format/printf'

NB. I don't know of any way to easily make an idiomatic lazy exploration,
NB. except falling back on explicit imperative control strutures.
NB. However this is clearly not where J shines neither with speed nor elegance.

primes_up_to  =: monad def 'p: i. _1 p: 1 + y'
terms_as_text =: monad def '; }: , (": each y),.<'' + '''

search_next_terms =: dyad define
 acc=. x     NB. -> an accumulator that contains given beginning of the partition.
 p=.   >0{y  NB. -> number of elements wanted in the partition
 ns=.  >1{y  NB. -> candidate values to be included in the partition
 sum=. >2{y  NB. -> the integer to partition

 if. p=0 do.
    if. sum=+/acc do. acc return. end.
 else.
   for_m. i. (#ns)-(p-1) do.
     r =. (acc,m{ns) search_next_terms (p-1);((m+1)}.ns);sum
     if. #r do. r return. end.
   end.
 end.

 0$0   NB. Empty result if nothing found at the end of this path.
)


NB. Prints  a partition of y primes whose sum equals x.
partitioned_in =: dyad define
    terms =. (0$0) search_next_terms y;(primes_up_to x);x
    if. #terms do.
       'As the sum of %d primes, %d = %s' printf y;x; terms_as_text terms
    else.
       'Didn''t find a way to express %d as a sum of %d different primes.' printf x;y
    end.
)


tests=: (99809 1) ; (18 2) ; (19 3) ; (20 4) ; (2017 24) ; (22699 1) ; (22699 2) ; (22699 3) ; (22699 4)
(0&{ partitioned_in 1&{) each tests

```



```txt

As the sum of 1 primes, 99809 = 99809
As the sum of 2 primes, 18 = 5 + 13
As the sum of 3 primes, 19 = 3 + 5 + 11
Didn't find a way to express 20 as a sum of 4 different primes.
As the sum of 24 primes, 2017 = 2 + 3 + 5 + 7 + 11 + 13 + 17 + 19 + 23 + 29 + 31 + 37 + 41 + 43 + 47 + 53 + 59 + 61 + 67 + 71 + 73 + 79 + 97 + 1129
As the sum of 1 primes, 22699 = 22699
As the sum of 2 primes, 22699 = 2 + 22697
As the sum of 3 primes, 22699 = 3 + 5 + 22691
As the sum of 4 primes, 22699 = 2 + 3 + 43 + 22651
As the sum of 3 primes, 40355 = 3 + 139 + 40213

```



## Java

```Java
import java.util.Arrays;
import java.util.stream.IntStream;

public class PartitionInteger {
    private static final int[] primes = IntStream.concat(IntStream.of(2), IntStream.iterate(3, n -> n + 2))
        .filter(PartitionInteger::isPrime)
        .limit(50_000)
        .toArray();

    private static boolean isPrime(int n) {
        if (n < 2) return false;
        if (n % 2 == 0) return n == 2;
        if (n % 3 == 0) return n == 3;
        int d = 5;
        while (d * d <= n) {
            if (n % d == 0) return false;
            d += 2;
            if (n % d == 0) return false;
            d += 4;
        }
        return true;
    }

    private static boolean findCombo(int k, int x, int m, int n, int[] combo) {
        boolean foundCombo = false;
        if (k >= m) {
            if (Arrays.stream(combo).map(i -> primes[i]).sum() == x) {
                String s = m > 1 ? "s" : "";
                System.out.printf("Partitioned %5d with %2d prime%s: ", x, m, s);
                for (int i = 0; i < m; ++i) {
                    System.out.print(primes[combo[i]]);
                    if (i < m - 1) System.out.print('+');
                    else System.out.println();
                }
                foundCombo = true;
            }
        } else {
            for (int j = 0; j < n; ++j) {
                if (k == 0 || j > combo[k - 1]) {
                    combo[k] = j;
                    if (!foundCombo) {
                        foundCombo = findCombo(k + 1, x, m, n, combo);
                    }
                }
            }
        }
        return foundCombo;
    }

    private static void partition(int x, int m) {
        if (x < 2 || m < 1 || m >= x) {
            throw new IllegalArgumentException();
        }
        int[] filteredPrimes = Arrays.stream(primes).filter(it -> it <= x).toArray();
        int n = filteredPrimes.length;
        if (n < m) throw new IllegalArgumentException("Not enough primes");
        int[] combo = new int[m];
        boolean foundCombo = findCombo(0, x, m, n, combo);
        if (!foundCombo) {
            String s = m > 1 ? "s" : " ";
            System.out.printf("Partitioned %5d with %2d prime%s: (not possible)\n", x, m, s);
        }
    }

    public static void main(String[] args) {
        partition(99809, 1);
        partition(18, 2);
        partition(19, 3);
        partition(20, 4);
        partition(2017, 24);
        partition(22699, 1);
        partition(22699, 2);
        partition(22699, 3);
        partition(22699, 4);
        partition(40355, 3);
    }
}
```

```txt
Partitioned 99809 with  1 prime: 99809
Partitioned    18 with  2 primes: 5+13
Partitioned    19 with  3 primes: 3+5+11
Partitioned    20 with  4 primes: (not possible)
Partitioned  2017 with 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partitioned 22699 with  1 prime: 22699
Partitioned 22699 with  2 primes: 2+22697
Partitioned 22699 with  3 primes: 3+5+22691
Partitioned 22699 with  4 primes: 2+3+43+22651
Partitioned 40355 with  3 primes: 3+139+40213
```



## Julia

```julia
using Primes, Combinatorics

function primepartition(x::Int64, n::Int64)
    if n == oftype(n, 1)
        return isprime(x) ? [x] : Int64[]
    else
        for combo in combinations(primes(x), n)
            if sum(combo) == x
                return combo
            end
        end
    end
    return Int64[]
end

for (x, n) in [[   18, 2], [   19, 3], [   20,  4], [99807, 1], [99809, 1],
         [ 2017, 24],[22699, 1], [22699, 2], [22699,  3], [22699, 4] ,[40355, 3]]
    ans = primepartition(x, n)
    println("Partition of ", x, " into ", n, " primes: ",
        isempty(ans) ? "impossible" : join(ans, " + "))
end
```


```txt

Partition of 18 into 2 prime pieces: 5 + 13
Partition of 19 into 3 prime pieces: 3 + 5 + 11
Partition of 20 into 4 prime pieces: impossible
Partition of 99807 into 1 prime piece: impossible
Partition of 99809 into 1 prime piece: 99809
Partition of 2017 into 24 prime pieces: 2 + 3 + 5 + 7 + 11 + 13 + 17 + 19 + 23 + 29 + 31 + 37 + 41 + 43 + 47 + 53 + 59 + 61 + 67 + 71 + 73 + 79 + 97 + 1129
Partition of 22699 into 1 prime piece: 22699
Partition of 22699 into 2 prime pieces: 2 + 22697
Partition of 22699 into 3 prime pieces: 3 + 5 + 22691
Partition of 22699 into 4 prime pieces: 2 + 3 + 43 + 22651
Partition of 40355 into 3 prime pieces: 3 + 139 + 40213

```



## Kotlin


```scala
// version 1.1.2

// compiled with flag -Xcoroutines=enable to suppress 'experimental' warning

import kotlin.coroutines.experimental.*

val primes = generatePrimes().take(50_000).toList()  // generate first 50,000 say
var foundCombo = false

fun isPrime(n: Int) : Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun generatePrimes() =
    buildSequence {
        yield(2)
        var p = 3
        while (p <= Int.MAX_VALUE) {
           if (isPrime(p)) yield(p)
           p += 2
        }
    }

fun findCombo(k: Int, x: Int, m: Int, n: Int, combo: IntArray) {
    if (k >= m) {
        if (combo.sumBy { primes[it] } == x) {
           val s = if (m > 1) "s" else " "
           print("Partitioned ${"%5d".format(x)} with ${"%2d".format(m)} prime$s: ")
           for (i in 0 until m) {
               print(primes[combo[i]])
               if (i < m - 1) print("+") else println()
           }
           foundCombo = true
        }
    }
    else {
        for (j in 0 until n) {
            if (k == 0 || j > combo[k - 1]) {
                combo[k] = j
                if (!foundCombo) findCombo(k + 1, x, m, n, combo)
            }
        }
    }
}

fun partition(x: Int, m: Int) {
    require(x >= 2 && m >= 1 && m < x)
    val filteredPrimes = primes.filter { it <= x }
    val n = filteredPrimes.size
    if (n < m) throw IllegalArgumentException("Not enough primes")
    val combo = IntArray(m)
    foundCombo = false
    findCombo(0, x, m, n, combo)
    if (!foundCombo) {
        val s = if (m > 1) "s" else " "
        println("Partitioned ${"%5d".format(x)} with ${"%2d".format(m)} prime$s: (not possible)")
    }
}

fun main(args: Array<String>) {
    val a = arrayOf(
        99809 to 1,
        18 to 2,
        19 to 3,
        20 to 4,
        2017 to 24,
        22699 to 1,
        22699 to 2,
        22699 to 3,
        22699 to 4,
        40355 to 3
    )
    for (p in a) partition(p.first, p.second)
}
```


```txt

Partitioned 99809 with  1 prime : 99809
Partitioned    18 with  2 primes: 5+13
Partitioned    19 with  3 primes: 3+5+11
Partitioned    20 with  4 primes: (not possible)
Partitioned  2017 with 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partitioned 22699 with  1 prime : 22699
Partitioned 22699 with  2 primes: 2+22697
Partitioned 22699 with  3 primes: 3+5+22691
Partitioned 22699 with  4 primes: 2+3+43+22651
Partitioned 40355 with  3 primes: 3+139+40213

```



## Lingo

Using the prime generator class "sieve" from task [[Extensible prime generator#Lingo]].


```Lingo
----------------------------------------
-- returns a sorted list of the <cnt> smallest unique primes that add up to <n>,
-- or FALSE if there is no such partition of primes for <n>
----------------------------------------
on getPrimePartition (n, cnt,   primes, ptr, res)
    if voidP(primes) then
        primes = _global.sieve.getPrimesInRange(2, n)
        ptr = 1
        res = []
    end if
    if cnt=1 then
        if primes.getPos(n)>=ptr then
            res.addAt(1, n)
            if res.count=cnt+ptr-1 then
                return res
            end if
            return TRUE
        end if
    else
        repeat with i = ptr to primes.count
            p = primes[i]
            ok = getPrimePartition(n-p, cnt-1,   primes, i+1, res)
            if ok then
                res.addAt(1, p)
                if res.count=cnt+ptr-1 then
                    return res
                end if
                return TRUE
            end if
        end repeat
    end if
    return FALSE
end

----------------------------------------
-- gets partition, prints formatted result
----------------------------------------
on showPrimePartition (n, cnt)
    res = getPrimePartition(n, cnt)
    if res=FALSE then res = "not prossible"
    else res = implode("+", res)
    put "Partitioned "&n&" with "&cnt&" primes: " & res
end

----------------------------------------
-- implodes list into string
----------------------------------------
on implode (delim, tList)
    str = ""
    repeat with i=1 to tList.count
        put tList[i]&delim after str
    end repeat
    delete char (str.length+1-delim.length) to str.length of str
    return str
end
```



```Lingo
-- main
_global.sieve = script("sieve").new()

showPrimePartition(99809, 1)
showPrimePartition(18, 2)
showPrimePartition(19, 3)
showPrimePartition(20, 4)
showPrimePartition(2017, 24)
showPrimePartition(22699, 1)
showPrimePartition(22699, 2)
showPrimePartition(22699, 3)
showPrimePartition(22699, 4)
showPrimePartition(40355, 3)
```


```txt

-- "Partitioned 99809 with 1 primes: 99809"
-- "Partitioned 18 with 2 primes: 5+13"
-- "Partitioned 19 with 3 primes: 3+5+11"
-- "Partitioned 20 with 4 primes: not prossible"
-- "Partitioned 2017 with 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129"
-- "Partitioned 22699 with 1 primes: 22699"
-- "Partitioned 22699 with 2 primes: 2+22697"
-- "Partitioned 22699 with 3 primes: 3+5+22691"
-- "Partitioned 22699 with 4 primes: 2+3+43+22651"
-- "Partitioned 40355 with 3 primes: 3+139+40213"

```




## Mathematica


{{incorrect|Mathematica|

 the partitioning of   '''40,356'''   into three primes isn't the lowest primes that are possible,

the primes should be:

   <big> '''3''',   '''139''',   '''40213'''. </big>
}}


Just call the function F[X,N]

```Mathematica
F[x_, n_] :=
 Print["Partitioned ", x, " with ", n, " primes: ",
  StringRiffle[
   ToString /@
    Reverse[First@
      Sort[Select[IntegerPartitions[x, {n}, Prime@Range@PrimePi@x],
        Length@Union@# == n &], Last]], "+"]]

F[40355, 3]
```



```txt

Partitioned 40355 with 3 primes: 5+7+40343

```



## PARI/GP


```parigp
partDistinctPrimes(x,n,mn=2)=
{
  if(n==1, return(if(isprime(x) && mn<=x, [x], 0)));
  if((x-n)%2,
    if(mn>2, return(0));
    my(t=partDistinctPrimes(x-2,n-1,3));
    return(if(t, concat(2,t), 0))
  );
  if(n==2,
    forprime(p=mn,(x-1)\2,
      if(isprime(x-p), return([p,x-p]))
    );
    return(0)
  );
  if(n<1, return(if(n, 0, [])));

  \\ x is the sum of 3 or more odd primes
  my(t);
  forprime(p=mn,(x-1)\n,
    t=partDistinctPrimes(x-p,n-1,p+2);
    if(t, return(concat(p,t)))
  );
  0;
}
displayNicely(x,n)=
{
  printf("Partitioned%6d with%3d prime", x, n);
  if(n!=1, print1("s"));
  my(t=partDistinctPrimes(x,n));
  if(t===0, print(": (not possible)"); return);
  if(#t, print1(": "t[1]));
  for(i=2,#t, print1("+"t[i]));
  print();
}
V=[[99809,1], [18,2], [19,3], [20,4], [2017,24], [22699,1], [22699,2], [22699,3], [22699,4], [40355,3]];
for(i=1,#V, call(displayNicely, V[i]))
```

```txt
Partitioned 99809 with  1 prime: 99809
Partitioned    18 with  2 primes: 5+13
Partitioned    19 with  3 primes: 3+5+11
Partitioned    20 with  4 primes: (not possible)
Partitioned  2017 with 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partitioned 22699 with  1 prime: 22699
Partitioned 22699 with  2 primes: 2+22697
Partitioned 22699 with  3 primes: 3+5+22691
Partitioned 22699 with  4 primes: 2+3+43+22651
Partitioned 40355 with  3 primes: 3+139+40213
```



## Perl

It is tempting to use the partition iterator which takes a "isprime" flag, but this task calls for unique values.  Hence the combination iterator over an array of primes makes more sense.
```perl
use ntheory ":all";

sub prime_partition {
  my($num, $parts) = @_;
  return is_prime($num) ? [$num] : undef if $parts == 1;
  my @p = @{primes($num)};
  my $r;
  forcomb { lastfor, $r = [@p[@_]] if vecsum(@p[@_]) == $num; } @p, $parts;
  $r;
}

foreach my $test ([18,2], [19,3], [20,4], [99807,1], [99809,1], [2017,24], [22699,1], [22699,2], [22699,3], [22699,4], [40355,3]) {
  my $partar = prime_partition(@$test);
  printf "Partition %5d into %2d prime piece%s %s\n", $test->[0], $test->[1], ($test->[1] == 1) ? ": " : "s:", defined($partar) ? join("+",@$partar) : "not possible";
}
```

```txt

Partition    18 into  2 prime pieces: 5+13
Partition    19 into  3 prime pieces: 3+5+11
Partition    20 into  4 prime pieces: not possible
Partition 99807 into  1 prime piece:  not possible
Partition 99809 into  1 prime piece:  99809
Partition  2017 into 24 prime pieces: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partition 22699 into  1 prime piece:  22699
Partition 22699 into  2 prime pieces: 2+22697
Partition 22699 into  3 prime pieces: 3+5+22691
Partition 22699 into  4 prime pieces: 2+3+43+22651
Partition 40355 into  3 prime pieces: 3+139+40213

```



## Perl 6

```perl6
use Math::Primesieve;
my $sieve = Math::Primesieve.new;

# short circuit for '1' partition
multi partition ( Int $number, 1 ) { $number.is-prime ?? $number !! () }

multi partition ( Int $number, Int $parts where * > 0 = 2 ) {
    my @these = $sieve.primes($number);
    for @these.combinations($parts) { .return if .sum == $number };
    ()
}

# TESTING
(18,2, 19,3, 20,4, 99807,1, 99809,1, 2017,24, 22699,1, 22699,2, 22699,3, 22699,4, 40355,3)\
  .race(:1batch).map: -> $number, $parts {
    say (sprintf "Partition %5d into %2d prime piece", $number, $parts),
    $parts == 1 ?? ':  ' !! 's: ', join '+', partition($number, $parts) || 'not possible'
}
```

```txt
Partition    18 into  2 prime pieces: 5+13
Partition    19 into  3 prime pieces: 3+5+11
Partition    20 into  4 prime pieces: not possible
Partition 99807 into  1 prime piece:  not possible
Partition 99809 into  1 prime piece:  99809
Partition  2017 into 24 prime pieces: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partition 22699 into  1 prime piece:  22699
Partition 22699 into  2 prime pieces: 2+22697
Partition 22699 into  3 prime pieces: 3+5+22691
Partition 22699 into  4 prime pieces: 2+3+43+22651
Partition 40355 into  3 prime pieces: 3+139+40213
```



## Phix

Using is_prime(), primes[] and add_block() from [[Extensible_prime_generator#Phix]].

```Phix
function partition(integer v, n, idx=0)
    if n=1 then
        return iff(is_prime(v)?{v}:0)
    end if
    object res
    while 1 do
        idx += 1
        while length(primes)<idx do
            add_block()
        end while
        integer np = primes[idx]
        if np>=floor(v/2) then exit end if
        res = partition(v-np, n-1, idx)
        if sequence(res) then return np&res end if
    end while
    return 0
end function

constant tests = {{99809, 1},
                  {18, 2},
                  {19, 3},
                  {20, 4},
                  {2017, 24},
                  {22699, 1},
                  {22699, 2},
                  {22699, 3},
                  {22699, 4},
                  {40355, 3}}

for i=1 to length(tests) do
    integer {v,n} = tests[i]
    object res = partition(v,n)
    res = iff(res=0?"not possible":sprint(res))
    printf(1,"Partitioned %d into %d primes: %s\n",{v,n,res})
end for
```

```txt

Partitioned 99809 into 1 primes: {99809}
Partitioned 18 into 2 primes: {5,13}
Partitioned 19 into 3 primes: {3,5,11}
Partitioned 20 into 4 primes: not possible
Partitioned 2017 into 24 primes: {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,97,1129}
Partitioned 22699 into 1 primes: {22699}
Partitioned 22699 into 2 primes: {2,22697}
Partitioned 22699 into 3 primes: {3,5,22691}
Partitioned 22699 into 4 primes: {2,3,43,22651}
Partitioned 40355 into 3 primes: {3,139,40213}

```



## Python


```Python

from itertools import combinations as cmb

def isP(n):
    if n==2:
        return True
    if n%2==0:
        return False
    return all(n%x>0 for x in range(3, int(n**0.5)+1, 2))

def genP(n):
    p = [2]
    p.extend([x for x in range(3, n+1, 2) if isP(x)])
    return p

data = [(99809, 1), (18, 2), (19, 3), (20, 4), (2017, 24), (22699, 1), (22699, 2), (22699, 3), (22699, 4), (40355, 3)]

for n, cnt in data:
    ci = iter(cmb(genP(n), cnt))
    while True:
        try:
            c = next(ci)
            if sum(c)==n:
                print(n, ',', cnt , "->", '+'.join(str(s) for s in c))
                break
        except:
            print(n, ',', cnt, " -> Not possible")
            break

```

```txt

99809 , 1 -> 99809
18 , 2 -> 5+13
19 , 3 -> 3+5+11
20 , 4  -> Not possible
2017 , 24 -> 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
22699 , 1 -> 22699
22699 , 2 -> 2+22697
22699 , 3 -> 3+5+22691
22699 , 4 -> 2+3+43+22651
40355 , 3 -> 3+139+40213

```



## Racket


```racket
#lang racket
(require math/number-theory)

(define memoised-next-prime
  (let ((m# (make-hash)))
    (λ (P) (hash-ref! m# P (λ () (next-prime P))))))

(define (partition-X-into-N-primes X N)
  (define (partition-x-into-n-primes-starting-at-P x n P result)
    (cond ((= n x 0) result)
          ((or (= n 0) (= x 0) (> P x)) #f)
          (else
           (let ((P′ (memoised-next-prime P)))
             (or (partition-x-into-n-primes-starting-at-P (- x P) (- n 1) P′ (cons P result))
                 (partition-x-into-n-primes-starting-at-P x n P′ result))))))

  (reverse (or (partition-x-into-n-primes-starting-at-P X N 2 null) (list 'no-solution))))

(define (report-partition X N)
  (let ((result (partition-X-into-N-primes X N)))
    (printf "partition ~a\twith ~a\tprimes: ~a~%" X N (string-join (map ~a result) " + "))))

(module+ test
  (report-partition 99809 1)
  (report-partition 18 2)
  (report-partition 19 3)
  (report-partition 20 4)
  (report-partition 2017 24)
  (report-partition 22699 1)
  (report-partition 22699 2)
  (report-partition 22699 3)
  (report-partition 22699 4)
  (report-partition 40355 3))
```


```txt
partition 99809	with 1	primes: 99809
partition 18	with 2	primes: 5 + 13
partition 19	with 3	primes: 3 + 5 + 11
partition 20	with 4	primes: no-solution
partition 2017	with 24	primes: 2 + 3 + 5 + 7 + 11 + 13 + 17 + 19 + 23 + 29 + 31 + 37 + 41 + 43 + 47 + 53 + 59 + 61 + 67 + 71 + 73 + 79 + 97 + 1129
partition 22699	with 1	primes: 22699
partition 22699	with 2	primes: 2 + 22697
partition 22699	with 3	primes: 3 + 5 + 22691
partition 22699	with 4	primes: 2 + 3 + 43 + 22651
partition 40355	with 3	primes: 3 + 139 + 40213
```



## REXX

Usage note:   entering ranges of   '''X'''   and   '''N'''   numbers (arguments) from the command line:
:::::   <big> '''X-Y'''   '''N-M'''     <b> ∙∙∙ </b> </big>

which means:   partition all integers (inclusive) from   '''X''' ──► '''Y'''   with   '''N''' ──► '''M'''   primes.

The   ''to''   number   ('''Y'''   or   '''M''')   can be omitted.

```rexx
/*REXX program  partitions  integer(s)    (greater than unity)   into   N   primes.     */
parse arg what                                   /*obtain an optional list from the C.L.*/
  do  until what==''                             /*possibly process a series of integers*/
  parse var what x n what; parse var  x  x '-' y /*get possible range  and # partitions.*/
                           parse var  n  n '-' m /*get possible range  and # partitions.*/
  if x=='' | x==","   then x=19                  /*Not specified?  Then use the default.*/
  if y=='' | y==","   then y=x                   /* "      "         "   "   "     "    */
  if n=='' | n==","   then n= 3                  /* "      "         "   "   "     "    */
  if m=='' | m==","   then m=n                   /* "      "         "   "   "     "    */
  call genP y                                    /*generate   Y   number of primes.     */
     do g=x  to y                                /*partition  X ───► Y  into partitions.*/
       do q=n  to m;  call part;  end  /*q*/     /*partition  G   into    Q    primes.  */
     end   /*g*/
  end   /*until*/
exit 0                                           /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genP: arg high;     @.1=2;    @.2=3;     #=2     /*get highest prime, assign some vars. */
               do j=@.#+2  by 2  until @.#>high  /*only find odd primes from here on.   */
                  do k=2  while k*k<=j           /*divide by some known low odd primes. */
                  if j // @.k==0  then iterate j /*Is  J  divisible by P?  Then ¬ prime.*/
                  end   /*k*/                    /* [↓]  a prime  (J)  has been found.  */
               #=#+1;          @.#=j             /*bump prime count; assign prime to  @.*/
               end      /*j*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
getP: procedure expose i. p. @.;  parse arg z    /*bump the prime in the partition list.*/
      if i.z==0  then do;   _=z-1;    i.z=i._;   end
      i.z=i.z+1;   _=i.z;   p.z=@._;  return 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
list: _=p.1;   if $==g  then  do j=2  to q;  _=_ p.j;  end;     else _= '__(not_possible)'
      the= 'primes:';   if q==1  then the= 'prime: ';    return the translate(_,"+ ",' _')
/*──────────────────────────────────────────────────────────────────────────────────────*/
part: i.=0;                         do j=1  for q;  call getP j;   end  /*j*/
                do !=0  by 0;  $=0               /*!:  a DO variable for LEAVE & ITERATE*/
                    do s=1  for q;    $=$+p.s    /* [↓]  is sum of the primes too large?*/
                    if $>g  then do;  if s==1  then leave !      /*perform a quick exit?*/
                                           do k=s    to q;  i.k=0;        end  /*k*/
                                           do r=s-1  to q;  call getP r;  end  /*r*/
                                      iterate !
                                 end
                    end   /*s*/
                if $==g  then leave              /*is sum of the primes exactly right ? */
                if $ <g  then do; call getP q; iterate; end
                end   /*!*/                      /* [↑]   Is sum too low?  Bump a prime.*/
      say 'partitioned'     center(g,9)       "into"       center(q, 5)      list()
      return
```

```txt

partitioned   99809   into   1   prime:  99809
partitioned    18     into   2   primes: 5+13
partitioned    19     into   3   primes: 3+5+11
partitioned    20     into   4   primes:   (not possible)
partitioned   2017    into  24   primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
partitioned   22699   into   1   prime:  22699
partitioned   22699   into   2   primes: 2+22697
partitioned   22699   into   3   primes: 3+5+22691
partitioned   22699   into   4   primes: 2+3+43+22651
partitioned   40355   into   3   primes: 3+139+40213

```



## Ring


```ring

# Project : Partition an integer X into N primes

load "stdlib.ring"
nr = 0
num = 0
list = list(100000)
items = newlist(pow(2,len(list))-1,100000)
while true
          nr = nr + 1
          if isprime(nr)
             num = num + 1
             list[num] = nr
          ok
          if num = 100000
              exit
          ok
end

powerset(list,100000)
showarray(items,100000)
see nl

func showarray(items,ind)
        for p = 1 to 20
              if (p > 17 and p < 21) or p = 99809 or p = 2017  or p = 22699  or p = 40355
                  for n = 1 to len(items)
                       flag = 0
                       for m = 1 to ind
                             if items[n][m] = 0
                                exit
                             ok
                             flag = flag + items[n][m]
                       next
                       if flag = p
                          str = ""
                          for x = 1 to len(items[n])
                               if items[n][x] != 0
                                  str = str + items[n][x] + " "
                               ok
                          next
                          str = left(str, len(str) - 1)
                          str = str + "]"
                          if substr(str, " ") > 0
                             see "" + p + " = ["
                             see str + nl
                             exit
                          else
                             str = ""
                          ok
                       ok
                  next
              ok
        next

func powerset(list,ind)
        num = 0
        num2 = 0
        items = newlist(pow(2,len(list))-1,ind)
        for i = 2 to (2 << len(list)) - 1 step 2
             num2 = 0
             num = num + 1
             for j = 1 to len(list)
                  if i & (1 << j)
                      num2 = num2 + 1
                      if list[j] != 0
                        items[num][num2] = list[j]
                     ok
                  ok
             next
        next
        return items

```

Output:

```txt

99809 = [99809]
18 = [5 13]
19 = [3 5 11]
20 = []
2017 = [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 97 1129]
22699 = [22699]
22699 = [2 22697]
22699 = [3 5 22691]
22699 = [2 3 43 22651]
40355 = [3 139 40213]

```



## Ruby


```ruby
require "prime"

def prime_partition(x, n)
  Prime.each(x).to_a.combination(n).detect{|primes| primes.sum == x}
end

TESTCASES = [[99809, 1], [18, 2], [19, 3], [20, 4], [2017, 24],
             [22699, 1], [22699, 2], [22699, 3], [22699, 4], [40355, 3]]

TESTCASES.each do |prime, num|
  res = prime_partition(prime, num)
  str = res.nil? ? "no solution" : res.join(" + ")
  puts  "Partitioned #{prime} with #{num} primes: #{str}"
end

```

```txt
Partitioned 99809 with 1 primes: 99809
Partitioned 18 with 2 primes: 5 + 13
Partitioned 19 with 3 primes: 3 + 5 + 11
Partitioned 20 with 4 primes: no solution
Partitioned 2017 with 24 primes: 2 + 3 + 5 + 7 + 11 + 13 + 17 + 19 + 23 + 29 + 31 + 37 + 41 + 43 + 47 + 53 + 59 + 61 + 67 + 71 + 73 + 79 + 97 + 1129
Partitioned 22699 with 1 primes: 22699
Partitioned 22699 with 2 primes: 2 + 22697
Partitioned 22699 with 3 primes: 3 + 5 + 22691
Partitioned 22699 with 4 primes: 2 + 3 + 43 + 22651
Partitioned 40355 with 3 primes: 3 + 139 + 40213

```



## Scala


```Scala
object PartitionInteger {

  def sieve(nums: LazyList[Int]): LazyList[Int] =
    LazyList.cons(nums.head, sieve((nums.tail) filter (_ % nums.head != 0)))

  // An 'infinite' stream of primes, lazy evaluation and memo-ized
  private val oddPrimes = sieve(LazyList.from(3, 2))
  private val primes = sieve(2 #:: oddPrimes).take(3600 /*50_000*/)

  private def findCombo(k: Int, x: Int, m: Int, n: Int, combo: Array[Int]): Boolean = {
    var foundCombo = combo.map(i => primes(i)).sum == x
    if (k >= m) {
      if (foundCombo) {
        val s: String = if (m > 1) "s" else ""
        printf("Partitioned %5d with %2d prime%s: ", x, m, s)
        for (i <- 0 until m) {
          print(primes(combo(i)))
          if (i < m - 1) print('+') else println()
        }
      }
    } else for (j <- 0 until n if k == 0 || j > combo(k - 1)) {
      combo(k) = j
      if (!foundCombo) foundCombo = findCombo(k + 1, x, m, n, combo)
    }
    foundCombo
  }

  private def partition(x: Int, m: Int): Unit = {
    val n: Int = primes.count(it => it <= x)
    if (n < m) throw new IllegalArgumentException("Not enough primes")

    if (!findCombo(0, x, m, n, new Array[Int](m)))
      printf("Partitioned %5d with %2d prime%s: (not possible)\n", x, m, if (m > 1) "s" else " ")
  }

  def main(args: Array[String]): Unit = {
    partition(99809, 1)
    partition(18, 2)
    partition(19, 3)
    partition(20, 4)
    partition(2017, 24)
    partition(22699, 1)
    partition(22699, 2)
    partition(22699, 3)
    partition(22699, 4)
    partition(40355, 3)
  }

}
```


## Sidef

```ruby
func prime_partition(num, parts) {

    if (parts == 1) {
        return (num.is_prime ? [num] : [])
    }

    num.primes.combinations(parts, {|*c|
        return c if (c.sum == num)
    })

    return []
}

var tests = [
    [   18, 2], [   19, 3], [   20,  4],
    [99807, 1], [99809, 1], [ 2017, 24],
    [22699, 1], [22699, 2], [22699,  3],
    [22699, 4], [40355, 3],
]

for num,parts (tests) {
    say ("Partition %5d into %2d prime piece" % (num, parts),
    parts == 1 ? ':  ' : 's: ', prime_partition(num, parts).join('+') || 'not possible')
}
```

```txt
Partition    18 into  2 prime pieces: 5+13
Partition    19 into  3 prime pieces: 3+5+11
Partition    20 into  4 prime pieces: not possible
Partition 99807 into  1 prime piece:  not possible
Partition 99809 into  1 prime piece:  99809
Partition  2017 into 24 prime pieces: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partition 22699 into  1 prime piece:  22699
Partition 22699 into  2 prime pieces: 2+22697
Partition 22699 into  3 prime pieces: 3+5+22691
Partition 22699 into  4 prime pieces: 2+3+43+22651
Partition 40355 into  3 prime pieces: 3+139+40213
```



## VBScript

```vb
' Partition an integer X into N primes
    dim p(),a(32),b(32),v,g: redim p(64)
    what="99809 1  18 2  19 3  20 4  2017 24  22699 1-4  40355 3"
    t1=split(what,"  ")
    for j=0 to ubound(t1)
        t2=split(t1(j)): x=t2(0): n=t2(1)
        t3=split(x,"-"): x=clng(t3(0))
        if ubound(t3)=1 then y=clng(t3(1)) else y=x
        t3=split(n,"-"): n=clng(t3(0))
        if ubound(t3)=1 then m=clng(t3(1)) else m=n
        genp y 'generate primes in p
        for g=x to y
            for q=n to m: part: next 'q
        next 'g
    next 'j

sub genp(high)
    p(1)=2: p(2)=3: c=2: i=p(c)+2
    do 'i
        k=2: bk=false
        do while k*k<=i and not bk 'k
            if i mod p(k)=0 then bk=true
            k=k+1
        loop 'k
        if not bk then
            c=c+1: if c>ubound(p) then redim preserve p(ubound(p)+8)
            p(c)=i
        end if
        i=i+2
    loop until p(c)>high 'i
end sub 'genp

sub getp(z)
    if a(z)=0 then w=z-1: a(z)=a(w)
    a(z)=a(z)+1: w=a(z): b(z)=p(w)
end sub 'getp

function list()
    w=b(1)
    if v=g then for i=2 to q: w=w&"+"&b(i): next else w="(not possible)"
    list="primes: "&w
end function 'list

sub part()
    for i=lbound(a) to ubound(a): a(i)=0: next 'i
    for i=1 to q: call getp(i): next 'i
    do while true: v=0: bu=false
        for s=1 to q
            v=v+b(s)
            if v>g then
                if s=1 then exit do
                for k=s to q: a(k)=0: next 'k
                for r=s-1 to q: call getp(r): next 'r
                bu=true: exit for
            end if
        next 's
        if not bu then
            if v=g then exit do
            if v<g then call getp(q)
        end if
    loop
    wscript.echo "partition "&g&" into "&q&" "&list
end sub 'part

```

```txt

partition 99809 into 1 primes: 99809
partition 18 into 2 primes: 5+13
partition 19 into 3 primes: 3+5+11
partition 20 into 4 primes: (not possible)
partition 2017 into 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
partition 22699 into 1 primes: 22699
partition 22699 into 2 primes: 2+22697
partition 22699 into 3 primes: 3+5+22691
partition 22699 into 4 primes: 2+3+43+22651
partition 40355 into 3 primes: 3+139+40213

```



## Visual Basic .NET

```vbnet
' Partition an integer X into N primes - 29/03/2017
Option Explicit On

Module PartitionIntoPrimes
    Dim p(8), a(32), b(32), v, g, q As Long

    Sub Main()
        Dim what, t1(), t2(), t3(), xx, nn As String
        Dim x, y, n, m As Long
        what = "99809 1  18 2  19 3  20 4  2017 24  22699 1-4  40355 3"
        t1 = Split(what, "  ")
        For j = 0 To UBound(t1)
            t2 = Split(t1(j)) : xx = t2(0) : nn = t2(1)
            t3 = Split(xx, "-") : x = CLng(t3(0))
            If UBound(t3) = 1 Then y = CLng(t3(1)) Else y = x
            t3 = Split(nn, "-") : n = CLng(t3(0))
            If UBound(t3) = 1 Then m = CLng(t3(1)) Else m = n
            genp(y) 'generate primes in p
            For g = x To y
                For q = n To m : part() : Next 'q
            Next 'g
        Next 'j
    End Sub 'Main

    Sub genp(high As Long)
        Dim c, i, k As Long
        Dim bk As Boolean
        p(1) = 2 : p(2) = 3 : c = 2 : i = p(c) + 2
        Do 'i
            k = 2 : bk = False
            Do While k * k <= i And Not bk 'k
                If i Mod p(k) = 0 Then bk = True
                k = k + 1
            Loop 'k
            If Not bk Then
                c = c + 1 : If c > UBound(p) Then ReDim Preserve p(UBound(p) + 8)
                p(c) = i
            End If
            i = i + 2
        Loop Until p(c) > high 'i
    End Sub 'genp

    Sub getp(z As Long)
        Dim w As Long
        If a(z) = 0 Then w = z - 1 : a(z) = a(w)
        a(z) = a(z) + 1 : w = a(z) : b(z) = p(w)
    End Sub 'getp

    Function list()
        Dim w As String
        w = b(1)
        If v = g Then
            For i = 2 To q : w = w & "+" & b(i) : Next
        Else
            w = "(not possible)"
        End If
        Return "primes: " & w
    End Function 'list

    Sub part()
        For i = LBound(a) To UBound(a) : a(i) = 0 : Next 'i
        For i = 1 To q : Call getp(i) : Next 'i
        Do While True : v = 0
            For s = 1 To q
                v = v + b(s)
                If v > g Then
                    If s = 1 Then Exit Do
                    For k = s To q : a(k) = 0 : Next 'k
                    For r = s - 1 To q : Call getp(r) : Next 'r
                    Continue Do
                End If
            Next 's
            If v = g Then Exit Do
            If v < g Then Call getp(q)
        Loop
        Console.WriteLine("partition " & g & " into " & q & " " & list())
    End Sub 'part

End Module 'PartitionIntoPrimes

```

```txt

partition 99809 into 1 primes: 99809
partition 18 into 2 primes: 5+13
partition 19 into 3 primes: 3+5+11
partition 20 into 4 primes: (not possible)
partition 2017 into 24 primes: 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
partition 22699 into 1 primes: 22699
partition 22699 into 2 primes: 2+22697
partition 22699 into 3 primes: 3+5+22691
partition 22699 into 4 primes: 2+3+43+22651
partition 40355 into 3 primes: 3+139+40213

```




## zkl

Using the prime generator from task [[Extensible prime generator#zkl]].

```zkl
   // Partition integer N into M unique primes
fcn partition(N,M,idx=0,ps=List()){
   var [const] sieve=Utils.Generator(Import("sieve").postponed_sieve);
   var [const] primes=List();
   while(sieve.peek()<=N){ primes.append(sieve.next()) }
   if(M<2){
      z:=primes.find(N);
      return(if(Void!=z and z>=idx) ps.append(N) else Void);
   }
   foreach z in ([idx..primes.len()-1]){
      p:=primes[z];
      if(p<=N and self.fcn(N-p,M-1,z+1,ps)) return(ps.insert(0,p));
      if(p>N) break;
   }
   Void		// no solution
}
```


```zkl
foreach n,m in (T( T(18,2),T(19,3),T(99809,1),T(20,4),T(2017,24),
      T(22699,1),T(22699,2),T(22699,3),T(22699,4),T(40355,3), )){
   ps:=partition(n,m);
   if(ps) println("Partition %d with %d prime(s): %s".fmt(n,m,ps.concat("+")));
   else   println("Can not partition %d with %d prime(s)".fmt(n,m));
}
```

```txt

Partition 18 with 2 prime(s): 5+13
Partition 19 with 3 prime(s): 3+5+11
Partition 99809 with 1 prime(s): 99809
Can not partition 20 with 4 prime(s)
Partition 2017 with 24 prime(s): 2+3+5+7+11+13+17+19+23+29+31+37+41+43+47+53+59+61+67+71+73+79+97+1129
Partition 22699 with 1 prime(s): 22699
Partition 22699 with 2 prime(s): 2+22697
Partition 22699 with 3 prime(s): 3+5+22691
Partition 22699 with 4 prime(s): 2+3+43+22651
Partition 40355 with 3 prime(s): 3+139+40213

```

