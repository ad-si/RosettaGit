+++
title = "Law of cosines - triples"
description = ""
date = 2019-08-03T21:40:20Z
aliases = []
[extra]
id = 22003
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "c",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "rexx",
  "ruby",
  "zkl",
]
+++

The [https://en.wikipedia.org/wiki/Law_of_cosines Law of cosines] states that for an angle γ, (gamma) of any triangle, if the sides adjacent to the angle are A and B and the side opposite is C; then the lengths of the sides are related by this formula:
          <big>  <code>A<sup>2</sup> + B<sup>2</sup> - 2ABcos(γ) = C<sup>2</sup></code> </big>

;Specific angles:
For an angle of of   '''90º'''   this becomes the more familiar "Pythagoras equation":
          <big>  <code>A<sup>2</sup> + B<sup>2</sup>  =  C<sup>2</sup></code>           </big>

For an angle of   '''60º'''   this becomes the less familiar  equation:
          <big>  <code>A<sup>2</sup> + B<sup>2</sup> - AB  =  C<sup>2</sup></code>       </big>

And finally for an angle of   '''120º'''   this becomes the equation:
          <big>  <code>A<sup>2</sup> + B<sup>2</sup> + AB  =  C<sup>2</sup></code>      </big>


## Task

*   Find all integer solutions (in order) to the three specific cases, distinguishing between each angle being considered.
*   Restrain all sides to the integers   '''1..13'''   inclusive.
*   Show how many results there are for each of the three angles mentioned above.
*   Display results on this page.


Note: Triangles with the same length sides but different order are to be treated as the same. 

;Optional Extra credit:
* How many 60° integer triples are there for sides in the range 1..10_000 ''where the sides are not all of the same length''.


;Related Task
* [[Pythagorean triples]]


## See also

* [https://youtu.be/p-0SOWbzUYI?t=12m11s Visualising Pythagoras: ultimate proofs and crazy contortions] Mathlogger Video





## ALGOL 68


```algol68
BEGIN
    # find all integer sided 90, 60 and 120 degree triangles by finding integer solutions for #
    #    a^2 + b^2 = c^2, a^2 + b^2 - ab = c^2, a^2 + b^2 + ab = c^2 where a, b, c in 1 .. 13 #
    INT max side   = 13;                  # max triangle side to consider                     #
    INT max square = max side * max side; # max triangle side squared to consider             #
    [ 1 : max square ]INT root;           # table of square roots                             #
    FOR s TO UPB root DO root[ s     ] := 0 OD;
    FOR s TO max side DO root[ s * s ] := s OD;
    INT tcount := 0;
    [ 1 : max square ]INT ta, tb, tc, tangle;
    # prints solutions for the specified angle                                                #
    PROC print triangles = ( INT angle )VOID:
    BEGIN
        INT scount := 0;
        FOR t TO tcount DO IF tangle[ t ] = angle THEN scount +:= 1 FI OD;
        print( ( whole( scount, -4 ), " ", whole( angle, -3 ), " degree triangles:", newline ) );
        FOR t TO tcount DO
            IF tangle[ t ] = angle THEN
                print( ( "    ", whole( ta[ t ], -3 ), whole( tb[ t ], -3 ), whole( tc[ t ], -3 ), newline ) )
            FI
        OD
    END # print triangles # ;
    # stores the triangle with sides a, b, root[ c2 ] and the specified angle,                #
    # if it is a solution                                                                     #
    PROC try triangle = ( INT a, b, c2, angle )VOID:
        IF  c2 <= max square THEN
            # the third side is small enough                                                  #
            INT c = root[ c2 ];
            IF  c /= 0 THEN
                # the third side is the square of an integer                                  #
                tcount +:= 1;
                ta[     tcount ] := a; tb[ tcount ] := b; tc[ tcount ] := root[ c2 ];
                tangle[ tcount ] := angle
            FI
        FI # try triangle # ;
    # find all triangles                                                                      #
    FOR a TO max side DO
        FOR b FROM a TO max side DO
            try triangle( a, b, ( a * a ) + ( b * b ) - ( a * b ),  60 );
            try triangle( a, b, ( a * a ) + ( b * b ),              90 );
            try triangle( a, b, ( a * a ) + ( b * b ) + ( a * b ), 120 )
        OD
    OD;
    # print the solutions                                                                     #    
    print triangles(  60 );
    print triangles(  90 );
    print triangles( 120 )
END
```

```txt

  15  60 degree triangles:
      1  1  1
      2  2  2
      3  3  3
      3  8  7
      4  4  4
      5  5  5
      5  8  7
      6  6  6
      7  7  7
      8  8  8
      9  9  9
     10 10 10
     11 11 11
     12 12 12
     13 13 13
   3  90 degree triangles:
      3  4  5
      5 12 13
      6  8 10
   2 120 degree triangles:
      3  5  7
      7  8 13

```



## C

=== A brute force algorithm, O(N^3) ===

```C
/*
 * RossetaCode: Law of cosines - triples
 *
 * An quick and dirty brute force solutions with O(N^3) cost.
 * Anyway it is possible set MAX_SIDE_LENGTH equal to 10000 
 * and use fast computer to obtain the "extra credit" badge.
 *
 * Obviously, there are better algorithms.
 */

#include <stdio.h>
#include <math.h>

#define MAX_SIDE_LENGTH 13
//#define DISPLAY_TRIANGLES 1

int main(void)
{
    static char description[3][80] = {
        "gamma =  90 degrees,  a*a + b*b       == c*c",
        "gamma =  60 degrees,  a*a + b*b - a*b == c*c",
        "gamma = 120 degrees,  a*a + b*b + a*b == c*c"
    };
    static int coeff[3] = { 0, 1, -1 };

    for (int k = 0; k < 3; k++)
    {
        int counter = 0;
        for (int a = 1; a <= MAX_SIDE_LENGTH; a++)
            for (int b = 1; b <= a; b++)
                for (int c = 1; c <= MAX_SIDE_LENGTH; c++)
                    if (a * a + b * b - coeff[k] * a * b == c * c)
                    {
                        counter++;
#ifdef DISPLAY_TRIANGLES
                        printf("  %d  %d  %d\n", a, b, c);
#endif
                    }
        printf("%s,  number of triangles = %d\n", description[k], counter);
    }

    return 0;
}

```

```txt

gamma =  90 degrees,  a*a + b*b       == c*c,  number of triangles = 3
gamma =  60 degrees,  a*a + b*b - a*b == c*c,  number of triangles = 15
gamma = 120 degrees,  a*a + b*b + a*b == c*c,  number of triangles = 2

```


=== An algorithm with O(N^2) cost ===

```C
/*
 * RossetaCode: Law of cosines - triples
 *
 * A solutions with O(N^2) cost.
 */

#include <stdio.h>
#include <math.h>

#define MAX_SIDE_LENGTH 10000
//#define DISPLAY_TRIANGLES 

int main(void)
{
    static char description[3][80] = {
        "gamma =  90 degrees,  a*a + b*b       == c*c",
        "gamma =  60 degrees,  a*a + b*b - a*b == c*c",
        "gamma = 120 degrees,  a*a + b*b + a*b == c*c"
    };
    static int coeff[3] = { 0, 1, -1 };

    printf("MAX SIDE LENGTH = %d\n\n", MAX_SIDE_LENGTH);

    for (int k = 0; k < 3; k++)
    {
        int counter = 0;
        for (int a = 1; a <= MAX_SIDE_LENGTH; a++)
            for (int b = 1; b <= a; b++)
            {
                int cc = a * a + b * b - coeff[k] * a * b;
                int c = (int)(sqrt(cc) + 0.5);
                if (c <= MAX_SIDE_LENGTH && c * c == cc)
                {
#ifdef DISPLAY_TRIANGLES
                    printf("%d %d %d\n", a, b, c);
#endif
                    counter++;
                }
            }
        printf("%s,  number of triangles = %d\n", description[k], counter);
    }

    return 0;
}

```

```txt

MAX SIDE LENGTH = 10000

gamma =  90 degrees,  a*a + b*b       == c*c,  number of triangles = 12471
gamma =  60 degrees,  a*a + b*b - a*b == c*c,  number of triangles = 28394
gamma = 120 degrees,  a*a + b*b + a*b == c*c,  number of triangles = 10374

```



## Factor


```factor
USING: backtrack formatting kernel locals math math.ranges
sequences sets sorting ;
IN: rosetta-code.law-of-cosines

:: triples ( quot -- seq )
    [
        V{ } clone :> seen        
        13 [1,b] dup dup [ amb-lazy ] tri@ :> ( a b c )
        a sq b sq + a b quot call( x x x -- x ) c sq =
        { b a c } seen member? not and
        must-be-true { a b c } dup seen push
    ] bag-of ;

: show-solutions ( quot angle -- )
    [ triples { } like dup length ] dip rot
    "%d solutions for %d degrees:\n%u\n\n" printf ;

[ * + ] 120
[ 2drop 0 - ] 90
[ * - ] 60 [ show-solutions ] 2tri@
```

```txt

2 solutions for 120 degrees:
{ { 3 5 7 } { 7 8 13 } }

3 solutions for 90 degrees:
{ { 3 4 5 } { 5 12 13 } { 6 8 10 } }

15 solutions for 60 degrees:
{
    { 1 1 1 }
    { 2 2 2 }
    { 3 3 3 }
    { 3 8 7 }
    { 4 4 4 }
    { 5 5 5 }
    { 5 8 7 }
    { 6 6 6 }
    { 7 7 7 }
    { 8 8 8 }
    { 9 9 9 }
    { 10 10 10 }
    { 11 11 11 }
    { 12 12 12 }
    { 13 13 13 }
}

```


## FreeBASIC


```freebasic
' version 03-03-2019
' compile with: fbc -s console

#Define max 13

#Define Format(_x)  Right("    " + Str(_x), 4)

Dim As UInteger a, b, c, a2, b2, c2, c60 , c90, c120
Dim As String s60, s90, s120

For a = 1 To max
    a2 = a * a
    For b = a To max
        b2 = b * b
        ' 60 degrees
        c2 = a2 + b * b - a * b
        c = Sqr(c2)
        If c * c = c2 AndAlso c <= max Then
            s60 += Format(a) + Format(b) + Format(c) + Chr(10, 13)
            c60 += 1
        End If
        ' 90 degrees
        c2 = a2 + b * b
        c = Sqr(c2)
        If c * c = c2 AndAlso c <= max Then
            s90 += Format(a) + Format(b) + Format(c) + Chr(10, 13)
            c90 += 1
        End If
        ' 120 degrees
        c2 = a2 + b * b + a * b
        c = Sqr(c2)
        If c * c = c2 AndAlso c <= max Then
            s120 += Format(a) + Format(b) + Format(c) + Chr(10, 13)
            c120 += 1
        End If
    Next
Next


Print Using "###: 60 degree triangles"; c60
Print s60
Print

Print Using "###: 90 degree triangles"; c90
Print s90
Print

Print Using "###: 120 degree triangles"; c120
Print s120
Print

#Undef max
#Define max 10000

c60 = 0
For a = 1 To max
    a2 = a * a
    For b = a +1 To max
        c2 = a2 + b * (b - a)
        c = Sqr(c2)
        If c * c = c2 AndAlso c <= max Then
            c60 += 1
        End If
    Next
Next

Print "For 60 degree triangles in the range [1, 10000]"
Print "There are "; c60; " triangles that have different length for a, b and c"

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
 15: 60 degree triangles
   1   1   1
   2   2   2
   3   3   3
   3   8   7
   4   4   4
   5   5   5
   5   8   7
   6   6   6
   7   7   7
   8   8   8
   9   9   9
  10  10  10
  11  11  11
  12  12  12
  13  13  13


  3: 90 degree triangles
   3   4   5
   5  12  13
   6   8  10


  2: 120 degree triangles
   3   5   7
   7   8  13


For 60 degree triangles in the range [1, 10000]
There are 18394 triangles that have different length for a, b and c
```



## Go


```go
package main

import "fmt"

type triple struct{ a, b, c int }

var squares13 = make(map[int]int, 13)
var squares10000 = make(map[int]int, 10000)

func init() {
    for i := 1; i <= 13; i++ {
        squares13[i*i] = i
    }
    for i := 1; i <= 10000; i++ {
        squares10000[i*i] = i
    }
}

func solve(angle, maxLen int, allowSame bool) []triple {
    var solutions []triple
    for a := 1; a <= maxLen; a++ {
        for b := a; b <= maxLen; b++ {
            lhs := a*a + b*b
            if angle != 90 {
                switch angle {
                case 60:
                    lhs -= a * b
                case 120:
                    lhs += a * b
                default:
                    panic("Angle must be 60, 90 or 120 degrees")
                }
            }
            switch maxLen {
            case 13:
                if c, ok := squares13[lhs]; ok {
                    if !allowSame && a == b && b == c {
                        continue
                    }
                    solutions = append(solutions, triple{a, b, c})
                }
            case 10000:
                if c, ok := squares10000[lhs]; ok {
                    if !allowSame && a == b && b == c {
                        continue
                    }
                    solutions = append(solutions, triple{a, b, c})
                }
            default:
                panic("Maximum length must be either 13 or 10000")
            }
        }
    }
    return solutions
}

func main() {
    fmt.Print("For sides in the range [1, 13] ")
    fmt.Println("where they can all be of the same length:-\n")
    angles := []int{90, 60, 120}
    var solutions []triple
    for _, angle := range angles {
        solutions = solve(angle, 13, true)
        fmt.Printf("  For an angle of %d degrees", angle)
        fmt.Println(" there are", len(solutions), "solutions, namely:")
        fmt.Printf("  %v\n", solutions)
        fmt.Println()
    }
    fmt.Print("For sides in the range [1, 10000] ")
    fmt.Println("where they cannot ALL be of the same length:-\n")
    solutions = solve(60, 10000, false)
    fmt.Print("  For an angle of 60 degrees")
    fmt.Println(" there are", len(solutions), "solutions.")
}
```


```txt

For sides in the range [1, 13] where they can all be of the same length:-

  For an angle of 90 degrees there are 3 solutions, namely:
  [{3 4 5} {5 12 13} {6 8 10}]

  For an angle of 60 degrees there are 15 solutions, namely:
  [{1 1 1} {2 2 2} {3 3 3} {3 8 7} {4 4 4} {5 5 5} {5 8 7} {6 6 6} {7 7 7} {8 8 8} {9 9 9} {10 10 10} {11 11 11} {12 12 12} {13 13 13}]

  For an angle of 120 degrees there are 2 solutions, namely:
  [{3 5 7} {7 8 13}]

For sides in the range [1, 10000] where they cannot ALL be of the same length:-

  For an angle of 60 degrees there are 18394 solutions.

```



## Haskell


```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Monoid ((<>))

triangles
  :: (Map.Map Int Int -> Int -> Int -> Int -> Int -> Maybe Int)
  -> Int
  -> [(Int, Int, Int)]
triangles f n =
  let mapRoots = Map.fromList $ ((,) =<< (^ 2)) <$> [1 .. n]
  in Set.elems $
     foldr
       (\(suma2b2, a, b) triSet ->
           (case f mapRoots suma2b2 (a * b) a b of
              Just c -> Set.insert (a, b, c) triSet
              _ -> triSet))
       (Set.fromList [])
       ([1 .. n] >>=
        (\a -> (flip (,,) a =<< (a * a +) . (>>= id) (*)) <$> [1 .. a]))


-- TESTS ------------------------------------------------------------------------

f90, f60, f60ne, f120 :: Map.Map Int Int -> Int -> Int -> Int -> Int -> Maybe Int
f90 dct x2 ab a b = Map.lookup x2 dct

f60 dct x2 ab a b = Map.lookup (x2 - ab) dct

f120 dct x2 ab a b = Map.lookup (x2 + ab) dct

f60ne dct x2 ab a b
  | a == b = Nothing
  | otherwise = Map.lookup (x2 - ab) dct

main :: IO ()
main = do
  putStrLn
    (unlines $
     "Triangles of maximum side 13\n" :
     zipWith
       (\f n ->
           let solns = triangles f 13
           in show (length solns) <> " solutions for " <> show n <>
              " degrees:\n" <>
              unlines (show <$> solns))
       [f120, f90, f60]
       [120, 90, 60])
  putStrLn "60 degrees - uneven triangles of maximum side 10000. Total:"
  print $ length $ triangles f60ne 10000
```

```txt
Triangles of maximum side 13

2 solutions for 120 degrees:
(5,3,7)
(8,7,13)

3 solutions for 90 degrees:
(4,3,5)
(8,6,10)
(12,5,13)

15 solutions for 60 degrees:
(1,1,1)
(2,2,2)
(3,3,3)
(4,4,4)
(5,5,5)
(6,6,6)
(7,7,7)
(8,3,7)
(8,5,7)
(8,8,8)
(9,9,9)
(10,10,10)
(11,11,11)
(12,12,12)
(13,13,13)


60 degrees - uneven triangles of maximum side 10000. Total:
18394
```



## J

'''Solution:'''

```j
load 'trig stats'
RHS=: *:                               NB. right-hand-side of Cosine Law
LHS=: +/@:*:@] - cos@rfd@[ * 2 * */@]  NB. Left-hand-side of Cosine Law

solve=: 4 :0
  adjsides=. >: 2 combrep y
  oppside=. >: i. y
  idx=. (RHS oppside) i. x LHS"1 adjsides
  adjsides ((#~ idx ~: #) ,. ({~ idx -. #)@]) oppside
)
```

'''Example:'''

```j
   60 90 120 solve&.> 13
+--------+-------+------+
| 1  1  1|3  4  5|3 5  7|
| 2  2  2|5 12 13|7 8 13|
| 3  3  3|6  8 10|      |
| 3  8  7|       |      |
| 4  4  4|       |      |
| 5  5  5|       |      |
| 5  8  7|       |      |
| 6  6  6|       |      |
| 7  7  7|       |      |
| 8  8  8|       |      |
| 9  9  9|       |      |
|10 10 10|       |      |
|11 11 11|       |      |
|12 12 12|       |      |
|13 13 13|       |      |
+--------+-------+------+
   60 #@(solve -. _3 ]\ 3 # >:@i.@]) 10000  NB. optional extra credit
18394
```


## JavaScript


```JavaScript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {

        const
            f90 = dct => x2 => dct[x2],
            f60 = dct => (x2, ab) => dct[x2 - ab],
            f120 = dct => (x2, ab) => dct[x2 + ab],
            f60unequal = dct => (x2, ab, a, b) =>
            (a !== b) ? (
                dct[x2 - ab]
            ) : undefined;


        // triangles :: Dict -> (Int -> Int -> Int -> Int -> Maybe Int)
        //                   -> [String]
        const triangles = (f, n) => {
            const
                xs = enumFromTo(1, n),
                fr = f(xs.reduce((a, x) => (a[x * x] = x, a), {})),
                gc = xs.reduce((a, _) => a, {}),
                setSoln = new Set();
            return (
                xs.forEach(
                    a => {
                        const a2 = a * a;
                        enumFromTo(1, 1 + a).forEach(
                            b => {
                                const
                                    suma2b2 = a2 + b * b,
                                    c = fr(suma2b2, a * b, a, b);
                                if (undefined !== c) {
                                    setSoln.add([a, b, c].sort())
                                };
                            }
                        );
                    }
                ),
                Array.from(setSoln.keys())
            );
        };

        const
            result = 'Triangles of maximum side 13:\n\n' +
            unlines(
                zipWith(
                    (s, f) => {
                        const ks = triangles(f, 13);
                        return ks.length.toString() + ' solutions for ' + s +
                            ' degrees:\n' + unlines(ks) + '\n';
                    },
                    ['120', '90', '60'],
                    [f120, f90, f60]
                )
            ) + '\nUneven triangles of maximum side 10000. Total:\n' +
            triangles(f60unequal, 10000).length

        return (
            //console.log(result),
            result
        );
    };


    // GENERIC FUNCTIONS ----------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // Returns Infinity over objects without finite length
    // this enables zip and zipWith to choose the shorter
    // argument when one non-finite like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs => xs.length || Infinity;

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        xs.constructor.constructor.name !== 'GeneratorFunction' ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

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

```txt
Triangles of maximum side 13:

2 solutions for 120 degrees:
3,5,7
13,7,8

3 solutions for 90 degrees:
3,4,5
10,6,8
12,13,5

15 solutions for 60 degrees:
1,1,1
2,2,2
3,3,3
4,4,4
5,5,5
6,6,6
7,7,7
3,7,8
5,7,8
8,8,8
9,9,9
10,10,10
11,11,11
12,12,12
13,13,13

Uneven triangles of maximum side 10000. Total:
18394
[Finished in 3.444s]
```



## Julia

```julia
sqdict(n) = Dict([(x*x, x) for x in 1:n])
numnotsame(arrarr) = sum(map(x -> !all(y -> y == x[1], x), arrarr))

function filtertriangles(N)
    sqd = sqdict(N)
    t60 = Vector{Vector{Int}}()
    t90 = Vector{Vector{Int}}()
    t120 = Vector{Vector{Int}}()
    for x in 1:N, y in 1:x
        xsq, ysq, xy = (x*x, y*y, x*y)
        if haskey(sqd, xsq + ysq - xy)
            push!(t60, sort([x, y, sqd[xsq + ysq - xy]]))
        elseif haskey(sqd, xsq + ysq)
            push!(t90, sort([x, y, sqd[xsq + ysq]]))
        elseif haskey(sqd, xsq + ysq + xy)
            push!(t120, sort([x, y, sqd[xsq + ysq + xy]]))
        end
    end
    t60, t90, t120
end

tri60, tri90, tri120 = filtertriangles(13)
println("Integer triples for 1 <= side length <= 13:\n")
println("Angle 60:"); for t in tri60 println(t) end
println("Angle 90:"); for t in tri90 println(t) end
println("Angle 120:"); for t in tri120 println(t) end
println("\nFor sizes N through 10000, there are $(numnotsame(filtertriangles(10000)[1])) 60 degree triples with nonequal sides.")

```
 {{output}} 
```txt


Integer triples for 1 <= side length <= 13:

Angle 60:
[1, 1, 1]
[2, 2, 2]
[3, 3, 3]
[4, 4, 4]
[5, 5, 5]
[6, 6, 6]
[7, 7, 7]
[3, 7, 8]
[5, 7, 8]
[8, 8, 8]
[9, 9, 9]
[10, 10, 10]
[11, 11, 11]
[12, 12, 12]
[13, 13, 13]
Angle 90:
[3, 4, 5]
[6, 8, 10]
[5, 12, 13]
Angle 120:
[3, 5, 7]
[7, 8, 13]

For sizes N through 10000, there are 18394 60 degree triples with nonequal sides.

```



## Kotlin

```scala
// Version 1.2.70

val squares13 = mutableMapOf<Int, Int>()
val squares10000 = mutableMapOf<Int, Int>()

class Trio(val a: Int, val b: Int, val c: Int) {
    override fun toString() = "($a $b $c)"
}

fun init() {
    for (i in 1..13) squares13.put(i * i, i)
    for (i in 1..10000) squares10000.put(i * i, i)
}

fun solve(angle :Int, maxLen: Int, allowSame: Boolean): List<Trio> {
    val solutions = mutableListOf<Trio>()
    for (a in 1..maxLen) {
        inner@ for (b in a..maxLen) {
            var lhs = a * a + b * b
            if (angle != 90) {
                when (angle) {
                    60   -> lhs -= a * b
                    120  -> lhs += a * b
                    else -> throw RuntimeException("Angle must be 60, 90 or 120 degrees")
                }
            }
            when (maxLen) {
                13 -> {
                    val c = squares13[lhs]
                    if (c != null) {
                        if (!allowSame && a == b && b == c) continue@inner
                        solutions.add(Trio(a, b, c))
                    }
                }

                10000 -> {
                    val c = squares10000[lhs]
                    if (c != null) {
                        if (!allowSame && a == b && b == c) continue@inner
                        solutions.add(Trio(a, b, c))
                    }
                }

                else -> throw RuntimeException("Maximum length must be either 13 or 10000")
            }
        }
    }
    return solutions
}

fun main(args: Array<String>) {
    init()
    print("For sides in the range [1, 13] ")
    println("where they can all be of the same length:-\n")
    val angles = intArrayOf(90, 60, 120)
    lateinit var solutions: List<Trio>
    for (angle in angles) {
        solutions = solve(angle, 13, true)
        print("  For an angle of ${angle} degrees")
        println(" there are ${solutions.size} solutions, namely:")
        println("  ${solutions.joinToString(" ", "[", "]")}\n")
    }
    print("For sides in the range [1, 10000] ")
    println("where they cannot ALL be of the same length:-\n")
    solutions = solve(60, 10000, false)
    print("  For an angle of 60 degrees")
    println(" there are ${solutions.size} solutions.")
}
```


```txt

For sides in the range [1, 13] where they can all be of the same length:-

  For an angle of 90 degrees there are 3 solutions, namely:
  [(3 4 5) (5 12 13) (6 8 10)]

  For an angle of 60 degrees there are 15 solutions, namely:
  [(1 1 1) (2 2 2) (3 3 3) (3 8 7) (4 4 4) (5 5 5) (5 8 7) (6 6 6) (7 7 7) (8 8 8) (9 9 9) (10 10 10) (11 11 11) (12 12 12) (13 13 13)]

  For an angle of 120 degrees there are 2 solutions, namely:
  [(3 5 7) (7 8 13)]

For sides in the range [1, 10000] where they cannot ALL be of the same length:-

  For an angle of 60 degrees there are 18394 solutions.

```



## Perl

```perl
use utf8;
binmode STDOUT, "utf8:";
use Sort::Naturally;

sub triples {
    my($n,$angle) = @_;
    my(@triples,%sq);
    $sq{$_**2}=$_ for 1..$n;
    for $a (1..$n-1) {
      for $b ($a+1..$n) {
        my $ab = $a*$a + $b*$b;
        my $cos = $angle == 60  ? $ab - $a * $b :
                  $angle == 120 ? $ab + $a * $b :
                                  $ab;
        if ($angle == 60) {
            push @triples, "$a $sq{$cos} $b" if exists $sq{$cos};
        } else {
            push @triples, "$a $b $sq{$cos}" if exists $sq{$cos};
        }
      }
    }
    @triples;
}

$n = 13;
print "Integer triangular triples for sides 1..$n:\n";
for my $angle (120, 90, 60) {
   my @itt = triples($n,$angle);
   if ($angle == 60) { push @itt, "$_ $_ $_" for 1..$n }
   printf "Angle %3d° has %2d solutions: %s\n", $angle, scalar @itt,
         join ', ', nsort @itt;
}

printf "Non-equilateral n=10000/60°: %d\n", scalar triples(10000,60);
```

```txt
Integer triangular triples for sides 1..13:
Angle 120° has  2 solutions: 3 5 7, 7 8 13
Angle  90° has  3 solutions: 3 4 5, 6 8 10, 5 12 13
Angle  60° has 15 solutions: 1 1 1, 2 2 2, 3 3 3, 3 7 8, 4 4 4, 5 5 5, 5 7 8, 6 6 6, 7 7 7, 8 8 8, 9 9 9, 10 10 10, 11 11 11, 12 12 12, 13 13 13
Non-equilateral n=10000/60°: 18394
```



## Perl 6

In each routine, <tt>race</tt> is used to allow concurrent operations, requiring the use of the atomic increment operator, <tt>⚛++</tt>, to safely update <tt>@triples</tt>, which must be declared fixed-sized, as an auto-resizing array is not thread-safe. At exit, default values in <tt>@triples</tt> are filtered out with the test <code>!eqv Any</code>.

```perl6
multi triples (60, $n) {
    my %sq = (1..$n).map: { .² => $_ };
    my atomicint $i = 0;
    my @triples[2*$n];
    (1..^$n).race(:8degree).map: -> $a {
        for $a^..$n -> $b {
            my $cos = $a * $a + $b * $b - $a * $b;
            @triples[$i⚛++] = $a, %sq{$cos}, $b if %sq{$cos}:exists;
        }
    }
    @triples.grep: so *;
}

multi triples (90, $n) {
    my %sq = (1..$n).map: { .² => $_ };
    my atomicint $i = 0;
    my @triples[2*$n];
    (1..^$n).race(:8degree).map: -> $a {
        for $a^..$n -> $b {
            my $cos = $a * $a + $b * $b;
            @triples[$i⚛++] = $a, $b, %sq{$cos} and last if %sq{$cos}:exists;
        }
    }
    @triples.grep: so *;
}

multi triples (120, $n) {
    my %sq = (1..$n).map: { .² => $_ };
    my atomicint $i = 0;
    my @triples[2*$n];
    (1..^$n).race(:8degree).map: -> $a {
        for $a^..$n -> $b {
            my $cos = $a * $a + $b * $b + $a * $b;
            @triples[$i⚛++] = $a, $b, %sq{$cos} and last if %sq{$cos}:exists;
        }
    }
    @triples.grep: so *;
}

use Sort::Naturally;

my $n = 13;
say "Integer triangular triples for sides 1..$n:";
for 120, 90, 60 -> $angle {
    my @itt = triples($angle, $n);
    if $angle == 60 { push @itt, "$_ $_ $_" for 1..$n }
    printf "Angle %3d° has %2d solutions: %s\n", $angle, +@itt, @itt.sort(*.&naturally).join(', ');
}

my ($angle, $count) = 60, 10_000;
say "\nExtra credit:";
say "$angle° integer triples in the range 1..$count where the sides are not all the same length: ", +triples($angle, $count);
```

```txt
Integer triangular triples for sides 1..13:
Angle 120° has  2 solutions: 3 5 7, 7 8 13
Angle  90° has  3 solutions: 3 4 5, 5 12 13, 6 8 10
Angle  60° has 15 solutions: 1 1 1, 2 2 2, 3 3 3, 3 7 8, 4 4 4, 5 5 5, 5 7 8, 6 6 6, 7 7 7, 8 8 8, 9 9 9, 10 10 10, 11 11 11, 12 12 12, 13 13 13

Extra credit:
60° integer triples in the range 1..10000 where the sides are not all the same length: 18394
```



## Phix

Using a simple flat sequence of 100 million elements (well within the language limits) proved significantly faster than a dictionary (5x or so).

```Phix
sequence squares = repeat(0,10000*10000)
for c=1 to 10000 do
    squares[c*c] = c
end for

function solve(integer angle, maxlen, bool samelen=true)
    sequence res = {}
    for a=1 to maxlen do
        integer a2 = a*a
        for b=a to maxlen do
            integer c2 = a2+b*b
            if angle!=90 then
                if    angle=60  then c2 -= a*b
                elsif angle=120 then c2 += a*b
                else crash("angle must be 60/90/120")
                end if  
            end if
            integer c = iff(c2>length(squares)?0:squares[c2])
            if c!=0 and c<=maxlen then
                if samelen or a!=b or b!=c then
                    res = append(res,{a,b,c})
                end if
            end if
        end for
    end for
    return res
end function

procedure show(string fmt,sequence res, bool full=true)
    printf(1,fmt,{length(res),iff(full?sprint(res):"")})
end procedure

puts(1,"Integer triangular triples for sides 1..13:\n")
show("Angle  60 has %2d solutions: %s\n",solve( 60,13))
show("Angle  90 has %2d solutions: %s\n",solve( 90,13))
show("Angle 120 has %2d solutions: %s\n",solve(120,13))
show("Non-equilateral angle 60 triangles for sides 1..10000: %d%s\n",solve(60,10000,false),false)
```

<pre style="font-size: 11px">
Integer triangular triples for sides 1..13:
Angle  60 has 15 solutions: {{1,1,1},{2,2,2},{3,3,3},{3,8,7},{4,4,4},{5,5,5},{5,8,7},{6,6,6},{7,7,7},{8,8,8},{9,9,9},{10,10,10},{11,11,11},{12,12,12},{13,13,13}}
Angle  90 has  3 solutions: {{3,4,5},{5,12,13},{6,8,10}}
Angle 120 has  2 solutions: {{3,5,7},{7,8,13}}
Non-equilateral angle 60 triangles for sides 1..10000: 18394

```



## Python


### Sets


```python
N = 13

def method1(N=N):
    squares = [x**2 for x in range(0, N+1)]
    sqrset = set(squares)
    tri90, tri60, tri120 = (set() for _ in range(3))
    for a in range(1, N+1):
        a2 = squares[a]
        for b in range(1, a + 1):
            b2 = squares[b]
            c2 = a2 + b2
            if c2 in sqrset:
                tri90.add(tuple(sorted((a, b, int(c2**0.5)))))
            ab = a * b
            c2 -= ab
            if c2 in sqrset:
                tri60.add(tuple(sorted((a, b, int(c2**0.5)))))
            c2 += 2 * ab
            if c2 in sqrset:
                tri120.add(tuple(sorted((a, b, int(c2**0.5)))))
    return  sorted(tri90), sorted(tri60), sorted(tri120)
#%%
if __name__ == '__main__':
    print(f'Integer triangular triples for sides 1..{N}:')
    for angle, triples in zip([90, 60, 120], method1(N)):
        print(f'  {angle:3}° has {len(triples)} solutions:\n    {triples}')
    _, t60, _ = method1(10_000)
    notsame = sum(1 for a, b, c in t60 if a != b or b != c)
    print('Extra credit:', notsame)
```


```txt
Integer triangular triples for sides 1..13:
   90° has 3 solutions:
    [(3, 4, 5), (5, 12, 13), (6, 8, 10)]
   60° has 15 solutions:
    [(1, 1, 1), (2, 2, 2), (3, 3, 3), (3, 7, 8), (4, 4, 4), (5, 5, 5), (5, 7, 8), (6, 6, 6), (7, 7, 7), (8, 8, 8), (9, 9, 9), (10, 10, 10), (11, 11, 11), (12, 12, 12), (13, 13, 13)]
  120° has 2 solutions:
    [(3, 5, 7), (7, 8, 13)]
Extra credit: 18394
```



### Dictionaries

A variant Python draft based on dictionaries. 
(Test functions are passed as parameters to the main function.)

```python
from itertools import (starmap)


def f90(dct):
    return lambda x2, ab, a, b: dct.get(x2, None)


def f60(dct):
    return lambda x2, ab, a, b: dct.get(x2 - ab, None)


def f120(dct):
    return lambda x2, ab, a, b: dct.get(x2 + ab, None)


def f60unequal(dct):
    return lambda x2, ab, a, b: (
        dct.get(x2 - ab, None) if a != b else None
    )


# triangles :: Dict -> (Int -> Int -> Int -> Int -> Maybe Int)
#                   -> [String]
def triangles(f, n):
    upto = enumFromTo(1)
    xs = upto(n)
    dctSquares = dict(zip(xs, [x**2 for x in xs]))
    dctRoots = {v: k for k, v in dctSquares.items()}
    fr = f(dctRoots)
    dct = {}
    for a in xs:
        a2 = dctSquares[a]
        for b in upto(a):
            suma2b2 = a2 + dctSquares[b]
            c = fr(suma2b2, a * b, a, b)
            if (c is not None):
                dct[str(sorted([a, b, c]))] = 1
    return list(dct.keys())


def main():
    print(
        'Triangles of maximum side 13\n\n' +
        unlines(
            zipWith(
                lambda f, n: (
                    lambda ks=triangles(f, 13): (
                        str(len(ks)) + ' solutions for ' +
                        str(n) + ' degrees:\n' +
                        unlines(ks) + '\n'
                    )
                )()
            )([f120, f90, f60])
             ([120, 90, 60])
        ) + '\n\n' +
        '60 degrees - uneven triangles of maximum side 10000. Total:\n' +
        str(len(triangles(f60unequal, 10000)))
    )


# GENERIC --------------------------------------------------------------

# enumFromTo :: Int -> Int -> [Int]
def enumFromTo(m):
    return lambda n: list(range(m, 1 + n))


# unlines :: [String] -> String
def unlines(xs):
    return '\n'.join(xs)


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    return lambda xs: lambda ys: (
        list(starmap(f, zip(xs, ys)))
    )


if __name__ == '__main__':
    main()
```

```txt
Triangles of maximum side 13

2 solutions for 120 degrees:
[3, 5, 7]
[7, 8, 13]

3 solutions for 90 degrees:
[3, 4, 5]
[6, 8, 10]
[5, 12, 13]

15 solutions for 60 degrees:
[1, 1, 1]
[2, 2, 2]
[3, 3, 3]
[4, 4, 4]
[5, 5, 5]
[6, 6, 6]
[7, 7, 7]
[3, 7, 8]
[5, 7, 8]
[8, 8, 8]
[9, 9, 9]
[10, 10, 10]
[11, 11, 11]
[12, 12, 12]
[13, 13, 13]


60 degrees - uneven triangles of maximum side 10000. Total:
18394
```



## REXX


### using some optimization

Instead of coding a general purpose subroutine (or function) to solve all of the
task's requirements,   it was decided to

write three very similar   '''do'''   loops (triple nested) to provide the
answers for the three requirements.

Three arguments   (from the command line)   can be specified which indicates the
maximum length of the triangle sides

(the default is   '''13''',   as per the task's requirement)   for each of the
three types of angles   ('''60º''', '''90º''', and '''120º''')   for

the triangles.   If the maximum length of the triangle's number of
sides is positive,   it indicates that the triangle sides are

displayed,   as well as a total number of triangles found.

If the maximum length of the triangle sides is negative,   only
the   ''number''   of triangles are
displayed   (using the

absolute value of the negative number).

```rexx
/*REXX pgm finds integer sided triangles that satisfy Law of cosines for 60º, 90º, 120º.*/
parse arg s1 s2 s3 .                             /*obtain optional arguments from the CL*/
if s1=='' | s1==","  then s1= 13                 /*Not specified?  Then use the default.*/
if s2=='' | s2==","  then s2= 13                 /* "      "         "   "   "     "    */
if s3=='' | s3==","  then s3= 13                 /* "      "         "   "   "     "    */
w= max( length(s1),  length(s2),  length(s3) )   /*W  is used to align the side lengths.*/

if s1>0  then do;  call head 120                        /*────120º:  a² + b² + ab  ≡ c² */
                                  do     a=1   for s1;  aa  = a*a
                                    do   b=a+1  to s1;  x= aa + b*b + a*b
                                      do c=b+1  to s1  until c*c>x
                                      if x==c*c  then do;  call show;  iterate b;  end
                                      end   /*c*/
                                    end     /*b*/
                                  end       /*a*/
                   call foot s1
              end

if s2>0  then do;  call head  90                        /*────90º:   a² + b²       ≡ c² */
                                  do     a=1   for s2;  aa  = a*a
                                    do   b=a+1  to s2;  x= aa + b*b
                                      do c=b+1  to s2     until c*c>x
                                      if x==c*c  then do;  call show;  iterate b;  end
                                      end   /*c*/
                                    end     /*b*/
                                  end       /*a*/
                   call foot s2
              end

if s3>0  then do;  call head  60                        /*────60º:   a² + b² ─ ab  ≡ c² */
                                  do     a=1   for s3;  aa  = a*a
                                    do   b=a    to s3;  x= aa + b*b - a*b
                                      do c=a    to s3  until c*c>x
                                      if x==c*c  then do;  call show;  iterate b;  end
                                      end   /*c*/
                                    end     /*b*/
                                  end       /*a*/
                   call foot s3
              end
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
foot: say right(#  ' solutions found for' angle "(sides up to" arg(1)')', 65); say; return
head: #= 0;  parse arg deg;   angle= ' 'deg"º ";   say center(angle, 65, '═');      return
show: #= # + 1;  say '     ('right(a, w)","   right(b, w)","   right(c, w)')';      return
```

```txt

═════════════════════════════ 120º ══════════════════════════════
     ( 3,  5,  7)
     ( 7,  8, 13)
                   2  solutions found for  120º  (sides up to 13)

══════════════════════════════ 90º ══════════════════════════════
     ( 3,  4,  5)
     ( 5, 12, 13)
     ( 6,  8, 10)
                    3  solutions found for  90º  (sides up to 13)

══════════════════════════════ 60º ══════════════════════════════
     ( 1,  1,  1)
     ( 2,  2,  2)
     ( 3,  3,  3)
     ( 3,  8,  7)
     ( 4,  4,  4)
     ( 5,  5,  5)
     ( 5,  8,  7)
     ( 6,  6,  6)
     ( 7,  7,  7)
     ( 8,  8,  8)
     ( 9,  9,  9)
     (10, 10, 10)
     (11, 11, 11)
     (12, 12, 12)
     (13, 13, 13)
                   15  solutions found for  60º  (sides up to 13)

```



### using memoization


```rexx
/*REXX pgm finds integer sided triangles that satisfy Law of cosines for 60º, 90º, 120º.*/
parse arg s1 s2 s3 s4 .                          /*obtain optional arguments from the CL*/
if s1=='' | s1==","  then s1=     13             /*Not specified?  Then use the default.*/
if s2=='' | s2==","  then s2=     13             /* "      "         "   "   "     "    */
if s3=='' | s3==","  then s3=     13             /* "      "         "   "   "     "    */
if s4=='' | s4==","  then s4= -10000             /* "      "         "   "   "     "    */
parse value s1 s2 s3 s4  with  os1 os2 os3 os4 . /*obtain the original values for sides.*/
s1=abs(s1);  s2=abs(s2); s3=abs(s3); s4=abs(s4)  /*use absolute values for the # sides. */
@.=
                      do j=1  for max(s1, s2, s3, s4);           @.j = j*j
                      end   /*j*/                /*build memoization array for squaring.*/

if s1>0  then do;  call head 120,,os1                     /*────120º: a² + b² + ab ≡ c² */
                                      do     a=1   for s1
                                        do   b=a+1  to s1;  x= @.a + @.b + a*b
                                        if x>z  then iterate a
                                          do c=b+1  to s1  until @.c>x
                                          if x==@.c  then do;  call show;  iterate b;  end
                                          end   /*c*/
                                        end     /*b*/
                                      end       /*a*/
                   call foot s1
              end

if s2>0  then do;  call head  90,, os2                    /*────90º:  a² + b²      ≡ c² */
                                      do     a=1   for s2
                                        do   b=a+1  to s2;  x= @.a + @.b
                                        if x>z  then iterate a
                                          do c=b+1  to s2  until @.c>x
                                          if x==@.c  then do;  call show;  iterate b;  end
                                          end   /*c*/
                                        end     /*b*/
                                      end       /*a*/
                   call foot s2
              end

if s3>0  then do;  call head  60,, os3                    /*────60º:  a² + b² ─ ab ≡ c² */
                                      do     a=1   for s3
                                        do   b=a    to s3;  x= @.a + @.b - a*b
                                        if x>z  then iterate a
                                          do c=a    to s3  until @.c>x
                                          if x==@.c  then do;  call show;  iterate b;  end
                                          end   /*c*/
                                        end     /*b*/
                                      end       /*a*/
                   call foot s3
              end

if s4>0  then do;  call head  60, 'unique', os4           /*────60º:  a² + b² ─ ab ≡ c² */
                                      do     a=1   for s4
                                        do   b=a    to s4;  x= @.a + @.b - a*b
                                        if x>z  then iterate a
                                          do c=a    to s4  until @.c>x
                                          if x==@.c  then do; if a==b&a==c  then iterate b
                                                              call show;         iterate b
                                                          end
                                          end   /*c*/
                                        end     /*b*/
                                      end       /*a*/
                   call foot s4
              end
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
foot: say right(#  ' solutions found for'  ang "(sides up to" arg(1)')', 65); say;  return
head: #=0; arg d,,s;z=s*s;w=length(s); ang=' 'd"º " arg(2); say center(ang,65,'═'); return
show: #= # + 1; if s>0  then say '     ('right(a,w)"," right(b,w)"," right(c,w)')'; return
```

Note that the first three computations are bypassed because of the three zero ('''0''') numbers,   the negative ten thousand indicates to find all the triangles with sides up to 10,000,   but not list the triangles, it just reports the   ''number''   of triangles found. 

```txt

══════════════════════════ 60º  unique═══════════════════════════
      18394  solutions found for  60º  unique (sides up to 10000)

```



## Ruby


```ruby
grouped =  (1..13).to_a.repeated_permutation(3).group_by do |a,b,c|
  sumaabb, ab = a*a + b*b, a*b
  case c*c
    when sumaabb      then 90
    when sumaabb - ab then 60
    when sumaabb + ab then 120
  end
end

grouped.delete(nil)
res = grouped.transform_values{|v| v.map(&:sort).uniq }

res.each do |k,v|
  puts "For an angle of #{k} there are #{v.size} solutions:"
  puts v.inspect, "\n"
end

```

```txt
For an angle of 60 there are 15 solutions:
[[1, 1, 1], [2, 2, 2], [3, 3, 3], [3, 7, 8], [4, 4, 4], [5, 5, 5], [5, 7, 8], [6, 6, 6], [7, 7, 7], [8, 8, 8], [9, 9, 9], [10, 10, 10], [11, 11, 11], [12, 12, 12], [13, 13, 13]]

For an angle of 90 there are 3 solutions:
[[3, 4, 5], [5, 12, 13], [6, 8, 10]]

For an angle of 120 there are 2 solutions:
[[3, 5, 7], [7, 8, 13]]

```

Extra credit:

```ruby
n  = 10_000
ar = (1..n).to_a
squares = {}
ar.each{|i| squares[i*i] = true }
count = ar.combination(2).count{|a,b| squares.key?(a*a + b*b - a*b)}

puts "There are #{count} 60° triangles with unequal sides of max size #{n}."

```

```txt
There are 18394 60° triangles with unequal sides of max size 10000.


```



## zkl


```zkl
fcn tritri(N=13){
   sqrset:=[0..N].pump(Dictionary().add.fp1(True),fcn(n){ n*n });
   tri90, tri60, tri120 := List(),List(),List();
   foreach a,b in ([1..N],[1..a]){
      aa,bb := a*a,b*b;
      ab,c  := a*b, aa + bb - ab;	// 60*
      if(sqrset.holds(c)){ tri60.append(abc(a,b,c)); continue; }

      c=aa + bb;			// 90*
      if(sqrset.holds(c)){ tri90.append(abc(a,b,c)); continue; }

      c=aa + bb + ab;			// 120*
      if(sqrset.holds(c))  tri120.append(abc(a,b,c));
   }
   List(tri60,tri90,tri120)
}
fcn abc(a,b,c){ List(a,b).sort().append(c.toFloat().sqrt().toInt()) }
fcn triToStr(tri){	// ((c,d,e),(a,b,c))-->"(a,b,c),(c,d,e)"
   tri.sort(fcn(t1,t2){ t1[0]<t2[0] })
      .apply("concat",",").apply("(%s)".fmt).concat(",")
}
```


```zkl
N:=13;
println("Integer triangular triples for sides 1..%d:".fmt(N));
foreach angle, triples in (T(60,90,120).zip(tritri(N))){
   println(" %3d\U00B0; has %d solutions:\n    %s"
           .fmt(angle,triples.len(),triToStr(triples)));
}
```

```txt

Integer triangular triples for sides 1..13:
  60° has 15 solutions:
    (1,1,1),(2,2,2),(3,8,7),(3,3,3),(4,4,4),(5,8,7),(5,5,5),(6,6,6),(7,7,7),(8,8,8),(9,9,9),(10,10,10),(11,11,11),(12,12,12),(13,13,13)
  90° has 3 solutions:
    (3,4,5),(5,12,13),(6,8,10)
 120° has 2 solutions:
    (3,5,7),(7,8,13)

```

Extra credit:

```zkl
fcn tri60(N){	// special case 60*
   sqrset:=[1..N].pump(Dictionary().add.fp1(True),fcn(n){ n*n });
   n60:=0;
   foreach a,b in ([1..N],[1..a]){
      c:=a*a + b*b - a*b;
      if(sqrset.holds(c) and a!=b!=c) n60+=1;
   }
   n60
}
```


```zkl
N:=10_000;
println(("60\U00b0; triangle where side lengths are unique,\n"
   "   side lengths 1..%,d, there are %,d solutions.").fmt(N,tri60(N)));
```

```txt

60° triangle where side lengths are unique,
   side lengths 1..10,000, there are 18,394 solutions.

```

