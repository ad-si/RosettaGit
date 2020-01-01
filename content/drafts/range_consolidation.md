+++
title = "Range consolidation"
description = ""
date = 2019-08-22T11:11:50Z
aliases = []
[extra]
id = 22162
[taxonomies]
categories = []
tags = []
+++

{{task}}

Define a range of numbers <code>R</code>, with bounds <code>b0</code> and <code>b1</code> covering all numbers ''between and including both bounds''. That range can be shown as:

: <code>[b0, b1]</code>

or equally as:

: <code>[b1, b0]</code>.

Given two ranges, the act of consolidation between them compares the two ranges:
* If one range covers all of the other then the result is that encompassing range.
* If the ranges touch or intersect then the result is ''one'' new single range covering the overlapping ranges.
* Otherwise the act of consolidation is to return the two non-touching ranges.

Given N ranges where N>2 then the result is the same as repeatedly replacing all combinations of two ranges by their consolidation until no further consolidation between range pairs is possible. If N<2 then range consolidation has no strict meaning and the input can be returned.

;'''Example 1:'''
:Given the two ranges <tt>[1, 2.5]</tt> and <tt>[3, 4.2]</tt> then there is no
:common area between the ranges and the result is the same as the input.
;'''Example 2:'''
:Given the two ranges <tt>[1, 2.5]</tt> and <tt>[1.8, 4.7]</tt> then there is
:an overlap <tt>[2.5, 1.8]</tt> between the ranges and the result is the single
:range <tt>[1, 4.7]</tt>.  Note that order of bounds in a range is not, (yet), stated.
;'''Example 3:'''
:Given the two ranges <tt>[6.1, 7.2]</tt> and <tt>[7.2, 8.3]</tt> then they
:touch at <tt>7.2</tt> and the result is the single range <tt>[6.1, 8.3]</tt>.
;'''Example 4:'''
:Given the three ranges <tt>[1, 2]</tt> and <tt>[4, 8]</tt> and <tt>[2, 5]</tt>
:then there is no intersection of the ranges <tt>[1, 2]</tt> and <tt>[4, 8]</tt>
:but the ranges <tt>[1, 2]</tt> and <tt>[2, 5]</tt> overlap and consolidate to
:produce the range <tt>[1, 5]</tt>. This range, in turn, overlaps the other range
:<tt>[4, 8]</tt>, and so consolidates to the final output of the single range
:<tt>[1, 8]</tt>

;'''Task:'''
Let a normalized range display show the smaller bound to the left; and show the
range with the smaller lower bound to the left of other ranges when showing multiple
ranges.

Output the ''normalised'' result of applying consolidation to these five sets of ranges:

```txt

        [1.1, 2.2]
        [6.1, 7.2], [7.2, 8.3]
        [4, 3], [2, 1]
        [4, 3], [2, 1], [-1, -2], [3.9, 10]
        [1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]

```

Show output here.




'''See also'''
* [[Set consolidation]]
* [[Set of real numbers]]




## C#

{{works with|C sharp|7}}

```c#
using static System.Math;
using System.Linq;
using System;

public static class RangeConsolidation
{
    public static void Main() {
        foreach (var list in new [] {
            new[] { (1.1, 2.2) }.ToList(),
            new[] { (6.1, 7.2), (7.2, 8.3) }.ToList(),
            new[] { (4d, 3d), (2, 1) }.ToList(),
            new[] { (4d, 3d), (2, 1), (-1, 2), (3.9, 10) }.ToList(),
            new[] { (1d, 3d), (-6, -1), (-4, -5), (8, 2), (-6, -6) }.ToList()
        })
        {
            for (int z = list.Count-1; z >= 1; z--) {
                for (int y = z - 1; y >= 0; y--) {
                    if (Overlap(list[z], list[y])) {
                        list[y] = Consolidate(list[z], list[y]);
                        list.RemoveAt(z);
                        break;
                    }
                }
            }
            Console.WriteLine(string.Join(", ", list.Select(Normalize).OrderBy(range => range.s)));
        }
    }

    private static bool Overlap((double s, double e) left, (double s, double e) right) =>
        Max(left.s, left.e) > Max(right.s, right.e)
        ? Max(right.s, right.e) >= Min(left.s, left.e)
        : Max(left.s, left.e) >= Min(right.s, right.e);

    private static (double s, double e) Consolidate((double s, double e) left, (double s, double e) right) =>
        (Min(Min(left.s, left.e), Min(right.s, right.e)), Max(Max(left.s, left.e), Max(right.s, right.e)));

    private static (double s, double e) Normalize((double s, double e) range) =>
        (Min(range.s, range.e), Max(range.s, range.e));
}
```

{{out}}

```txt

(1.1, 2.2)
(6.1, 8.3)
(1, 2), (3, 4)
(-1, 2), (3, 10)
(-6, -1), (1, 8)
```



## Dyalect


{{trans|C#}}


```dyalect
func max(x, y) {
    if x > y {
        x
    } else {
        y
    }
}

func min(x, y) {
    if x < y {
        x
    } else {
        y
    }
}

func overlap(left, right) {
    if max(left.s, left.e) > max(right.s, right.e) {
        max(right.s, right.e) >= min(left.s, left.e)
    } else {
        max(left.s, left.e) >= min(right.s, right.e)
    }
}

func consolidate(left, right) {
    (s: min(min(left.s, left.e), min(right.s, right.e)), e: max(max(left.s, left.e), max(right.s, right.e)))
}

func normalize(range) {
    (s: min(range.s, range.e), e: max(range.s, range.e))
}

for list in [
    [ (s: 1.1, e: 2.2) ],
    [ (s: 6.1, e: 7.2), (s: 7.2, e: 8.3) ],
    [ (s: 4.0, e: 3.0), (s: 2, e: 1) ],
    [ (s: 4.0, e: 3.0), (s: 2, e: 1), (s: -1, e: 2), (s: 3.9, e: 10) ],
    [ (s: 1.0, e: 3.0), (s: -6, e: -1), (s: -4, e: -5), (s: 8, e: 2), (s: -6, e: -6) ]
] {
    var z = list.len()-1
    while z >= 1 {
        for y in (z - 1)..0 {
            if overlap(list[z], list[y]) {
                list[y] = consolidate(list[z], list[y])
                list.removeAt(z)
                break
            }
        }
        z -= 1
    }
    for i in list.indices() {
        list[i] = normalize(list[i])
    }
    list.sort((x,y) => x.s - y.s)
    print(list)
}

```


{{out}}


```txt
[(s: 1.1, e: 2.2)]
[(s: 6.1, e: 8.3)]
[(s: 1, e: 2), (s: 3, e: 4)]
[(s: -1, e: 2), (s: 3, e: 10)]
[(s: -6, e: -1), (s: 1, e: 8)]
```



## Go


```go
package main

import (
    "fmt"
    "math"
    "sort"
)

type Range struct{ Lower, Upper float64 }

func (r Range) Norm() Range {
    if r.Lower > r.Upper {
        return Range{r.Upper, r.Lower}
    }
    return r
}

func (r Range) String() string {
    return fmt.Sprintf("[%g, %g]", r.Lower, r.Upper)
}

func (r1 Range) Union(r2 Range) []Range {
    if r1.Upper < r2.Lower {
        return []Range{r1, r2}
    }
    r := Range{r1.Lower, math.Max(r1.Upper, r2.Upper)}
    return []Range{r}
}

func consolidate(rs []Range) []Range {
    for i := range rs {
        rs[i] = rs[i].Norm()
    }
    le := len(rs)
    if le < 2 {
        return rs
    }
    sort.Slice(rs, func(i, j int) bool {
        return rs[i].Lower < rs[j].Lower
    })
    if le == 2 {
        return rs[0].Union(rs[1])
    }
    for i := 0; i < le-1; i++ {
        for j := i + 1; j < le; j++ {
            ru := rs[i].Union(rs[j])
            if len(ru) == 1 {
                rs[i] = ru[0]
                copy(rs[j:], rs[j+1:])
                rs = rs[:le-1]
                le--
                i--
                break
            }
        }
    }
    return rs
}

func main() {
    rss := [][]Range{
        {{1.1, 2.2}},
        {{6.1, 7.2}, {7.2, 8.3}},
        {{4, 3}, {2, 1}},
        {{4, 3}, {2, 1}, {-1, -2}, {3.9, 10}},
        {{1, 3}, {-6, -1}, {-4, -5}, {8, 2}, {-6, -6}},
    }
    for _, rs := range rss {
        s := fmt.Sprintf("%v", rs)
        fmt.Printf("%40s => ", s[1:len(s)-1])
        rs2 := consolidate(rs)
        s = fmt.Sprintf("%v", rs2)
        fmt.Println(s[1 : len(s)-1])
    }
}
```


{{out}}

```txt

                              [1.1, 2.2] => [1.1, 2.2]
                   [6.1, 7.2] [7.2, 8.3] => [6.1, 8.3]
                           [4, 3] [2, 1] => [1, 2] [3, 4]
        [4, 3] [2, 1] [-1, -2] [3.9, 10] => [-2, -1] [1, 2] [3, 10]
[1, 3] [-6, -1] [-4, -5] [8, 2] [-6, -6] => [-6, -1] [1, 8]

```



## Haskell


```haskell
import Data.List (intercalate, maximumBy, sort)
import Data.Ord (comparing)

consolidated :: [(Float, Float)] -> [(Float, Float)]
consolidated xs =
  let go xy [] = [xy]
      go xy@(x, y) abetc@((a, b):etc)
        | y >= b = xy : etc
        | y >= a = (x, b) : etc
        | otherwise = xy : abetc
      ab (a, b)
        | a <= b = (a, b)
        | otherwise = (b, a)
  in foldr go [] (sort . fmap ab $ xs)


-- TEST ---------------------------------------------------
tests :: [[(Float, Float)]]
tests =
  [ []
  , [(1.1, 2.2)]
  , [(6.1, 7.2), (7.2, 8.3)]
  , [(4, 3), (2, 1)]
  , [(4, 3), (2, 1), (-1, -2), (3.9, 10)]
  , [(1, 3), (-6, -1), (-4, -5), (8, 2), (-6, -6)]
  ]

main :: IO ()
main =
  putStrLn $
  tabulated "Range consolidations:" showPairs showPairs consolidated tests


-- DISPLAY FORMATTING -------------------------------------

tabulated :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
tabulated s xShow fxShow f xs =
  let w = length $ maximumBy (comparing length) (xShow <$> xs)
      rjust n c s = drop (length s) (replicate n c ++ s)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs

showPairs :: [(Float, Float)] -> String
showPairs xs
  | null xs = "[]"
  | otherwise = '[' : intercalate ", " (showPair <$> xs) ++ "]"

showPair :: (Float, Float) -> String
showPair (a, b) = '(' : showNum a ++ ", " ++ showNum b ++ ")"

showNum :: Float -> String
showNum n
  | 0 == (n - fromIntegral (round n)) = show (round n)
  | otherwise = show n
```

{{Out}}

```txt
Range consolidations:
                                            [] -> []
                                  [(1.1, 2.2)] -> [(1.1, 2.2)]
                      [(6.1, 7.2), (7.2, 8.3)] -> [(6.1, 8.3)]
                              [(4, 3), (2, 1)] -> [(1, 2), (3, 4)]
         [(4, 3), (2, 1), (-1, -2), (3.9, 10)] -> [(-2, -1), (1, 2), (3, 10)]
[(1, 3), (-6, -1), (-4, -5), (8, 2), (-6, -6)] -> [(-6, -1), (1, 8)]
```



## J

'''Solution:'''

```j
ensure2D=: ,:^:(1 = #@$)                 NB. if list make 1 row table
normalise=: ([: /:~ /:~"1)@ensure2D      NB. normalises list of ranges
merge=: ,:`(<.&{. , >.&{:)@.(>:/&{: |.)  NB. merge ranges x and y
consolidate=: (}.@] ,~ (merge {.)) ensure2D
```

'''Required Examples:'''

```j
   tests=:  <@".;._2 noun define
1.1 2.2
6.1 7.2 ,: 7.2 8.3
4 3 ,: 2 1
4 3 , 2 1 , _1 _2 ,: 3.9 10
1 3 , _6 _1 , _4 _5 , 8 2 ,: _6 _6
)

   consolidate/@normalise&.> tests
+-------+-------+---+-----+-----+
|1.1 2.2|6.1 8.3|1 2|_2 _1|_6 _1|
|       |       |3 4| 1  2| 1  8|
|       |       |   | 3 10|     |
+-------+-------+---+-----+-----+
```



## JavaScript

{{Trans|Haskell}}
{{Trans|Python}}

```javascript
(() => {
    'use strict';

    const main = () => {

        // consolidated :: [(Float, Float)] -> [(Float, Float)]
        const consolidated = xs =>
            foldl((abetc, xy) =>
                0 < abetc.length ? (() => {
                    const
                        etc = abetc.slice(1),
                        [a, b] = abetc[0],
                        [x, y] = xy;

                    return y >= b ? (
                        cons(xy, etc)
                    ) : y >= a ? (
                        cons([x, b], etc)
                    ) : cons(xy, abetc);
                })() : [xy],
                [],
                sortBy(flip(comparing(fst)),
                    map(([a, b]) => a < b ? (
                            [a, b]
                        ) : [b, a],
                        xs
                    )
                )
            );

        // TEST -------------------------------------------
        console.log(
            tabulated(
                'Range consolidations:',
                JSON.stringify,
                JSON.stringify,
                consolidated,
                [
                    [
                        [1.1, 2.2]
                    ],
                    [
                        [6.1, 7.2],
                        [7.2, 8.3]
                    ],
                    [
                        [4, 3],
                        [2, 1]
                    ],
                    [
                        [4, 3],
                        [2, 1],
                        [-1, -2],
                        [3.9, 10]
                    ],
                    [
                        [1, 3],
                        [-6, -1],
                        [-4, -5],
                        [8, 2],
                        [-6, -6]
                    ]
                ]
            )
        );
    };

    // GENERIC FUNCTIONS ----------------------------

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        1 < f.length ? (
            (a, b) => f(b, a)
        ) : (x => y => f(y)(x));

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller, s) =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        0 < xs.length ? (
            xs.slice(1)
            .reduce((a, x) => 0 < f(x, a) ? x : a, xs[0])
        ) : undefined;

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) =>
        xs.slice()
        .sort(f);

    // tabulated :: String -> (a -> String) ->
    //                        (b -> String) ->
    //           (a -> b) -> [a] -> String
    const tabulated = (s, xShow, fxShow, f, xs) => {
        // Heading -> x display function ->
        //           fx display function ->
        //    f -> values -> tabular string
        const
            ys = map(xShow, xs),
            w = maximumBy(comparing(x => x.length), ys).length,
            rows = zipWith(
                (a, b) => justifyRight(w, ' ', a) + ' -> ' + b,
                ys,
                map(compose(fxShow, f), xs)
            );
        return s + '\n' + unlines(rows);
    };

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

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
Range consolidations:
                          [[1.1,2.2]] -> [[1.1,2.2]]
                [[6.1,7.2],[7.2,8.3]] -> [[6.1,8.3]]
                        [[4,3],[2,1]] -> [[1,2],[3,4]]
       [[4,3],[2,1],[-1,-2],[3.9,10]] -> [[-2,-1],[1,2],[3,10]]
[[1,3],[-6,-1],[-4,-5],[8,2],[-6,-6]] -> [[-6,-1],[1,8]]
```



## Julia

In Julia, a Range is a type of iterator, generally one over a specified interval.
The task as specified is orthogonal to the iteration purpose of a Julia Range,
since the task is about merging sets of numbers, not iterations. Therefore, a translation
of the Python code is done, rather than using a native Julia Range.
{{trans|Python}}

```julia
normalize(s) = sort([sort(bounds) for bounds in s])

function consolidate(ranges)
    norm = normalize(ranges)
    for (i, r1) in enumerate(norm)
        if !isempty(r1)
            for r2 in norm[i+1:end]
                if !isempty(r2) && r1[end] >= r2[1]     # intersect?
                    r1 .= [r1[1], max(r1[end], r2[end])]
                    empty!(r2)
                end
            end
        end
    end
    [r for r in norm if !isempty(r)]
end

function testranges()
    for s in [[[1.1, 2.2]], [[6.1, 7.2], [7.2, 8.3]], [[4, 3], [2, 1]],
              [[4, 3], [2, 1], [-1, -2], [3.9, 10]],
              [[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]]]
        println("$s => $(consolidate(s))")
    end
end

testranges()

```
{{output}}

```txt

Array{Float64,1}[[1.1, 2.2]] => Array{Float64,1}[[1.1, 2.2]]
Array{Float64,1}[[6.1, 7.2], [7.2, 8.3]] => Array{Float64,1}[[6.1, 8.3]]
Array{Float64,1}[[4.0, 3.0], [2.0, 1.0]] => Array{Float64,1}[[1.0, 2.0], [3.0, 4.0]]
Array{Float64,1}[[4.0, 3.0], [2.0, 1.0], [-1.0, -2.0], [3.9, 10.0]] => Array{Float64,1}[[-2.0, -1.0], [1.0, 2.0], [3.0, 10.0]]
Array{Float64,1}[[1.0, 3.0], [-6.0, -1.0], [-4.0, -5.0], [8.0, 2.0], [-6.0, -6.0]] => Array{Float64,1}[[-6.0, -1.0], [1.0, 8.0]]

```



## Perl


Note: the output is shown in the standard [https://perldoc.perl.org/perlop.html#Range-Operators Perl notation for Ranges].


```perl
use strict;
use warnings;

use List::Util qw(min max);

sub consolidate {
    our @arr; local *arr = shift;
    my @sorted = sort { @$a[0] <=> @$b[0] } map { [sort { $a <=> $b } @$_] } @arr;
    my @merge = shift @sorted;
    for my $i (@sorted) {
        if ($merge[-1][1] >= @$i[0]) {
            $merge[-1][0] = min($merge[-1][0], @$i[0]);
            $merge[-1][1] = max($merge[-1][1], @$i[1]);
        } else {
            push @merge, $i;
        }
    }
    return @merge;
}

for my $intervals (
    [[1.1, 2.2],],
    [[6.1, 7.2], [7.2, 8.3]],
    [[4, 3], [2, 1]],
    [[4, 3], [2, 1], [-1, -2], [3.9, 10]],
    [[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]]) {
        my($in,$out);
        $in   = join ', ', map { '[' . join(', ', @$_) . ']' } @$intervals;
        $out .= join('..', @$_). ' ' for consolidate($intervals);
        printf "%44s => %s\n", $in, $out;
}
```

{{out}}

```txt
                                  [1.1, 2.2] => 1.1..2.2
                      [6.1, 7.2], [7.2, 8.3] => 6.1..8.3
                              [4, 3], [2, 1] => 1..2 3..4
         [4, 3], [2, 1], [-1, -2], [3.9, 10] => -2..-1 1..2 3..10
[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6] => -6..-1 1..8
```



## Perl 6

{{works with|Rakudo|2018.12}}
In Perl 6, a Range is a first class object with its own specialized notation. Perl 6 Ranges allow for exclusion of the boundary numbers. This example doesn't since it isn't a requirement in this task. Much of the logic is lifted from the [[Set_of_real_numbers#Perl_6|Set_of_real_numbers]] task with simplified logic for the much simpler requirements.

Note: the output is in standard [https://docs.perl6.org/type/Range Perl 6 notation for Ranges].


```perl6
# Union
sub infix:<∪> (Range $a, Range $b) { Range.new($a.min,max($a.max,$b.max)) }

# Intersection
sub infix:<∩> (Range $a, Range $b) { so $a.max >= $b.min }

multi consolidate() { () }

multi consolidate($this is copy, **@those) {
    gather {
        for consolidate |@those -> $that {
            if $this ∩ $that { $this ∪= $that }
            else             { take $that }
        }
        take $this;
    }
}

for [[1.1, 2.2],],
    [[6.1, 7.2], [7.2, 8.3]],
    [[4, 3], [2, 1]],
    [[4, 3], [2, 1], [-1, -2], [3.9, 10]],
    [[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]]
-> @intervals {
    printf "%46s => ", @intervals.perl;
    say reverse consolidate |@intervals.grep(*.elems)».sort.sort({ [.[0], .[*-1]] }).map: { Range.new(.[0], .[*-1]) }
}
```

{{out}}

```txt
                                 [[1.1, 2.2],] => (1.1..2.2)
                      [[6.1, 7.2], [7.2, 8.3]] => (6.1..8.3)
                              [[4, 3], [2, 1]] => (1..2 3..4)
         [[4, 3], [2, 1], [-1, -2], [3.9, 10]] => (-2..-1 1..2 3..10)
[[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]] => (-6..-1 1..8)

```



## Phix


```Phix
function consolidate(sequence sets)
    for i=length(sets) to 1 by -1 do
        sets[i] = sort(sets[i])
        atom {is,ie} = sets[i]
        for j=length(sets) to i+1 by -1 do
            atom {js,je} = sets[j]
            bool overlap = iff(is<=js?js<=ie:is<=je)
            if overlap then
                sets[i] = {min(is,js),max(ie,je)}
                sets[j..j] = {}
            end if
        end for
    end for
    return sort(sets)
end function

procedure test(sequence set)
    printf(1,"%40v => %v\n",{set,consolidate(set)})
end procedure

test({{1.1,2.2}})
test({{6.1,7.2},{7.2,8.3}})
test({{4,3},{2,1}})
test({{4,3},{2,1},{-1,-2},{3.9,10}})
test({{1,3},{-6,-1},{-4,-5},{8,2},{-6,-6}})
```

{{out}}

```txt

                             {{1.1,2.2}} => {{1.1,2.2}}
                   {{6.1,7.2},{7.2,8.3}} => {{6.1,8.3}}
                           {{4,3},{2,1}} => {{1,2},{3,4}}
          {{4,3},{2,1},{-1,-2},{3.9,10}} => {{-2,-1},{1,2},{3,10}}
   {{1,3},{-6,-1},{-4,-5},{8,2},{-6,-6}} => {{-6,-1},{1,8}}

```



## Python


### Procedural


```python
def normalize(s):
    return sorted(sorted(bounds) for bounds in s if bounds)

def consolidate(ranges):
    norm = normalize(ranges)
    for i, r1 in enumerate(norm):
        if r1:
            for r2 in norm[i+1:]:
                if r2 and r1[-1] >= r2[0]:     # intersect?
                    r1[:] = [r1[0], max(r1[-1], r2[-1])]
                    r2.clear()
    return [rnge for rnge in norm if rnge]

if __name__ == '__main__':
    for s in [
            [[1.1, 2.2]],
            [[6.1, 7.2], [7.2, 8.3]],
            [[4, 3], [2, 1]],
            [[4, 3], [2, 1], [-1, -2], [3.9, 10]],
            [[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]],
            ]:
        print(f"{str(s)[1:-1]} => {str(consolidate(s))[1:-1]}")

```


{{out}}

```txt
[1.1, 2.2] => [1.1, 2.2]
[6.1, 7.2], [7.2, 8.3] => [6.1, 8.3]
[4, 3], [2, 1] => [1, 2], [3, 4]
[4, 3], [2, 1], [-1, -2], [3.9, 10] => [-2, -1], [1, 2], [3, 10]
[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6] => [-6, -1], [1, 8]
```




### Functional


Defining consolidation as a fold over a list of tuples:
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Range consolidation'''

from functools import reduce


# consolidated :: [(Float, Float)] -> [(Float, Float)]
def consolidated(xs):
    '''A consolidated list of
       [(Float, Float)] ranges.'''

    def go(abetc, xy):
        '''A copy of the accumulator abetc,
           with its head range ab either:
           1. replaced by or
           2. merged with
           the next range xy, or
           with xy simply prepended.'''
        if abetc:
            a, b = abetc[0]
            etc = abetc[1:]
            x, y = xy
            return [xy] + etc if y >= b else (   # ab replaced.
                [(x, b)] + etc if y >= a else (  # xy + ab merged.
                    [xy] + abetc                 # xy simply prepended.
                )
            )
        else:
            return [xy]

    def tupleSort(ab):
        a, b = ab
        return ab if a <= b else (b, a)

    return reduce(
        go,
        sorted(map(tupleSort, xs), reverse=True),
        []
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests'''

    print(
        tabulated('Consolidation of numeric ranges:')(str)(str)(
            consolidated
        )([
            [(1.1, 2.2)],
            [(6.1, 7.2), (7.2, 8.3)],
            [(4, 3), (2, 1)],
            [(4, 3), (2, 1), (-1, -2), (3.9, 10)],
            [(1, 3), (-6, -1), (-4, -5), (8, 2), (-6, -6)]
        ])
    )


# GENERIC FUNCTIONS FOR DISPLAY ---------------------------


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Consolidation of numeric ranges:
                                  [(1.1, 2.2)] -> [(1.1, 2.2)]
                      [(6.1, 7.2), (7.2, 8.3)] -> [(6.1, 8.3)]
                              [(4, 3), (2, 1)] -> [(1, 2), (3, 4)]
         [(4, 3), (2, 1), (-1, -2), (3.9, 10)] -> [(-2, -1), (1, 2), (3, 10)]
[(1, 3), (-6, -1), (-4, -5), (8, 2), (-6, -6)] -> [(-6, -1), (1, 8)]
```



## Racket



```racket
#lang racket

;; Racket's max and min allow inexact numbers to contaminate exact numbers
;; Use argmax and argmin instead, as they don't have this problem

(define (max . xs) (argmax identity xs))
(define (min . xs) (argmin identity xs))

;; a bag is a list of disjoint intervals

(define ((irrelevant? x y) item) (or (< (second item) x) (> (first item) y)))

(define (insert bag x y)
  (define-values (irrelevant relevant) (partition (irrelevant? x y) bag))
  (cons (list (apply min x (map first relevant))
              (apply max y (map second relevant))) irrelevant))

(define (solve xs)
  (sort (for/fold ([bag '()]) ([x (in-list xs)])
          (insert bag (apply min x) (apply max x))) < #:key first))

(define inputs '(([1.1 2.2])
                 ([6.1 7.2] [7.2 8.3])
                 ([4 3] [2 1])
                 ([4 3] [2 1] [-1 -2] [3.9 10])
                 ([1 3] [-6 -1] [-4 -5] [8 2] [-6 -6])))

(for ([xs (in-list inputs)]) (printf "~a => ~a\n" xs (solve xs)))
```


{{out}}

```txt

((1.1 2.2)) => ((1.1 2.2))
((6.1 7.2) (7.2 8.3)) => ((6.1 8.3))
((4 3) (2 1)) => ((1 2) (3 4))
((4 3) (2 1) (-1 -2) (3.9 10)) => ((-2 -1) (1 2) (3 10))
((1 3) (-6 -1) (-4 -5) (8 2) (-6 -6)) => ((-6 -1) (1 8))

```



## REXX

Most of the REXX code was testing (and rebuilding) the syntax (insuring blanks after commas), and handling of a null set.

The actual logic for the range consolidation is marked with the comments:     <big> <tt> /*■■■■►*/ </tt> </big>

```rexx
/*REXX program performs range consolidation (they can be [equal] ascending/descending). */
#.=                                              /*define the default for range sets.   */
parse arg #.1                                    /*obtain optional arguments from the CL*/
if #.1=''  then do                               /*Not specified?  Then use the defaults*/
                #.1= '[1.1, 2.2]'
                #.2= '[6.1, 7.2], [7.2, 8.3]'
                #.3= '[4, 3], [2, 1]'
                #.4= '[4, 3], [2, 1], [-1, -2], [3.9, 10]'
                #.5= '[1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]'
                #.6= '[]'
                end

       do j=1  while #.j\=='';   $= #.j          /*process each of the range sets.      */
       say copies('═', 75)                       /*display a fence between range sets.  */
       say '         original ranges:'     $     /*display the original range set.      */
       $= order($)                               /*order low and high ranges; normalize.*/
       call xSort  words($)                      /*sort the ranges using a simple sort. */
       $= merge($)                               /*consolidate the ranges.              */
       say '     consolidated ranges:'     $     /*display the consolidated range set.  */
       end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
merge: procedure expose @.; parse arg y
       if words(y)<2  then signal build          /*Null or only 1 range?  Skip merging. */

          do j=1  to @.0-1;         if @.j==''  then iterate      /*skip deleted ranges.*/
            do k=j+1  to  @.0;      if @.k==''  then iterate      /*  "     "       "   */
            parse var  @.j  a   b;  parse var  @.k  aa  bb        /*extract low and high*/
/*■■■■►*/   if a<=aa & b>=bb  then  do; @.k=;  iterate;            end  /*within a range*/
/*■■■■►*/   if a<=aa & b>=aa  then  do; @.j= a bb; @.k=; iterate;  end  /*abutted ranges*/
            end   /*k*/
          end     /*j*/
build: z=
             do r=1  for @.0;  z= z translate(@.r, ',', " ");  end   /*r*/   /*add comma*/
       f=;   do s=1  for words(z);   f= f '['word(z, s)"], ";  end   /*s*/   /*add [ ], */
       if f==''  then return '[]'                                            /*null set.*/
       return space( changestr(',',  strip( space(f), 'T', ","), ", ") )     /*add blank*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
order: procedure expose @.; parse arg y,,z;  @.= /*obtain arguments from the invocation.*/
       y= space(y, 0)                            /*elide superfluous blanks in the sets.*/
          do k=1  while y\==''  &  y\=='[]'      /*process ranges while range not blank.*/
          if left(y,1)==','  then y= substr(y,2) /*elide commas between sets of ranges. */
          parse var  y   '['  L  ","  H  ']'   y /*extract  the "low" and "high" values.*/
          if H<L  then parse value  L H with H L /*order     "    "    "     "      "   */
          L= L / 1;     H= H / 1                 /*normalize the  L  and the  H  values.*/
          @.k= L H;     z= z L','H               /*re─build the set w/o and with commas.*/
          end   /*k*/                            /* [↓]  at this point, K is one to big.*/
       @.0= k - 1                                /*keep track of the number of ranges.  */
       return strip(z)                           /*elide the extra leading blank in set.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
xSort: procedure expose @.; parse arg n          /*a simple sort for small set of ranges*/
          do j=1  to n-1;                        _= @.j
            do k=j+1  to n; if word(@.k,1)>=word(_,1)  then iterate; @.j=@.k; @.k=_; _=@.j
            end   /*k*/
```

{{out|output|text=  when using the default inputs:}}

```txt

═══════════════════════════════════════════════════════════════════════════
         original ranges: [1.1, 2.2]
     consolidated ranges: [1.1, 2.2]
═══════════════════════════════════════════════════════════════════════════
         original ranges: [6.1, 7.2], [7.2, 8.3]
     consolidated ranges: [6.1, 8.3]
═══════════════════════════════════════════════════════════════════════════
         original ranges: [4, 3], [2, 1]
     consolidated ranges: [1, 2], [3, 4]
═══════════════════════════════════════════════════════════════════════════
         original ranges: [4, 3], [2, 1], [-1, -2], [3.9, 10]
     consolidated ranges: [-2, -1], [1, 2], [3, 10]
═══════════════════════════════════════════════════════════════════════════
         original ranges: [1, 3], [-6, -1], [-4, -5], [8, 2], [-6, -6]
     consolidated ranges: [-6, -1], [1, 8]
═══════════════════════════════════════════════════════════════════════════
         original ranges: []
     consolidated ranges: []

```



## Yabasic


```Yabasic
sub sort(tabla())
    local items, i, t1, t2, s

    items = arraysize(tabla(), 1)

    repeat
        s = true
        for i = 1 to items-1
            if tabla(i, 1) > tabla(i+1, 1) then
                t1 = tabla(i, 1) : t2 = tabla(i, 2)
                tabla(i, 1) = tabla(i + 1, 1) : tabla(i, 2) = tabla(i + 1, 2)
                tabla(i + 1, 1) = t1 : tabla(i + 1, 2) = t2
                s = false
            end if
        next
    until(s)
end sub

sub normalize(tabla())
    local items, i, t

    items = arraysize(tabla(), 1)

    for i = 1 to items
        if tabla(i, 1) > tabla(i, 2) then
            t = tabla(i, 1)
            tabla(i, 1) = tabla(i, 2)
            tabla(i, 2) = t
        end if
    next

    sort(tabla())
end sub

sub consolidate(tabla())
    local items, i

    normalize(tabla())
    items = arraysize(tabla(), 1)

    for i = 1 to items - 1
        if tabla(i + 1, 1) <= tabla(i, 2) then
            tabla(i + 1, 1) = tabla(i, 1)
            if tabla(i + 1, 2) <= tabla(i, 2) then
                tabla(i + 1, 2) = tabla(i, 2)
            end if
            tabla(i, 1) = void : tabla(i, 2) = void
        end if
    next
end sub

// data 1, 1.1, 2.2
// data 2, 6.1, 7.2, 7.2, 8.3
// data 2, 4, 3, 2, 1
// data 4, 4, 3, 2, 1, -1, -2, 3.9, 10
 data 5, 1,3, -6,-1, -4,-5, 8,2, -6,-6

void = 10^30
read items

dim tabla(items,  2)

for i = 1 to items
    read tabla(i, 1), tabla(i, 2)
next

consolidate(tabla())

for i = 1 to items
    if tabla(i, 1) <> void print tabla(i, 1), "..", tabla(i, 2);
next
```



## zkl


```zkl
fcn consolidate(rs){
   (s:=List()).append(
      normalize(rs).reduce('wrap(ab,cd){
	 if(ab[1]>=cd[0]) L(ab[0],ab[1].max(cd[1])); // consolidate
	 else{ s.append(ab); cd }		     // no overlap
      }) )
}
fcn normalize(s){ s.apply("sort").sort(fcn(a,b){ a[0]<b[0] }) }
```


```zkl
foreach rs in (L(
   L(L(1.1, 2.2)),    L(L(6.1, 7.2), L(7.2, 8.3)),    L(L(4, 3), L(2, 1)),
   L(L(4.0, 3.0), L(2.0, 1.0), L(-1.0, -2.0), L(3.9, 10.0)),
   L(L(1, 3), L(-6, -1), L(-4, -5), L(8, 2), L(-6, -6)),
 )){ println(ppp(rs),"--> ",ppp(consolidate(rs))) }
fcn ppp(ll){ ll.pump(String,fcn(list){ list.concat(", ",  "[",  "] ") }) }
```

{{out}}

```txt

[1.1, 2.2] --> [1.1, 2.2]
[6.1, 7.2] [7.2, 8.3] --> [6.1, 8.3]
[4, 3] [2, 1] --> [1, 2] [3, 4]
[4, 3] [2, 1] [-1, -2] [3.9, 10] --> [-2, -1] [1, 2] [3, 10]
[1, 3] [-6, -1] [-4, -5] [8, 2] [-6, -6] --> [-6, -1] [1, 8]

```

