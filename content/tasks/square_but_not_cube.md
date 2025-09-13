+++
title = "Square but not cube"
description = ""
date = 2019-10-21T16:07:39Z
aliases = []
[extra]
id = 21947
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Show the first 30 positive integers which are squares but not cubes of such integers.

Optionally, show also the first 3 positive integers which are both squares and cubes - and mark them as such.





## ALGOL 68

Avoids computing cube roots.

```algol68
BEGIN
    # list the first 30 numbers that are squares but not cubes and also #
    # show the numbers that are both squares and cubes                  #
    INT count := 0;
    INT c     := 1;
    INT c3    := 1;
    FOR s WHILE count < 30 DO
        INT sq = s * s;
        WHILE c3 < sq DO
            c  +:= 1;
            c3  := c * c * c
        OD;
        print( ( whole( sq, -5 ) ) );
        IF c3 = sq THEN
            # the square is also a cube                                 #
            print( ( " is also the cube of ", whole( c, -5 ) ) )
        ELSE
            # square only                                               #
            count +:= 1
        FI;
        print( ( newline ) )
    OD
END
```

```txt

    1 is also the cube of     1
    4
    9
   16
   25
   36
   49
   64 is also the cube of     4
   81
  100
  121
  144
  169
  196
  225
  256
  289
  324
  361
  400
  441
  484
  529
  576
  625
  676
  729 is also the cube of     9
  784
  841
  900
  961
 1024
 1089

```



## AppleScript


```applescript
on run
    script listing
        on |λ|(x)
            set sqr to x * x
            set strSquare to sqr as text

            if isCube(sqr) then
                strSquare & " (also cube)"
            else
                strSquare
            end if
        end |λ|
    end script

    unlines(map(listing, ¬
        enumFromTo(1, 33)))
end run

-- isCube :: Int -> Bool
on isCube(x)
    x = (round (x ^ (1 / 3))) ^ 3
end isCube


-- GENERIC FUNCTIONS -------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
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

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines
```

```txt
1 (also cube)
4
9
16
25
36
49
64 (also cube)
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 (also cube)
784
841
900
961
1024
1089
```


## AWK


```AWK

# syntax: GAWK -f SQUARE_BUT_NOT_CUBE.AWK
BEGIN {
    while (n < 30) {
      sqpow = ++square ^ 2
      if (is_cube(sqpow) == 0) {
        n++
        printf("%4d\n",sqpow)
      }
      else {
        printf("%4d is square and cube\n",sqpow)
      }
    }
    exit(0)
}
function is_cube(x,  i) {
    for (i=1; i<=x; i++) {
      if (i ^ 3 == x) {
        return(1)
      }
    }
    return(0)
}

```

```txt

   1 is square and cube
   4
   9
  16
  25
  36
  49
  64 is square and cube
  81
 100
 121
 144
 169
 196
 225
 256
 289
 324
 361
 400
 441
 484
 529
 576
 625
 676
 729 is square and cube
 784
 841
 900
 961
1024
1089

```



## C


```c
#include <stdio.h>
#include <math.h>

int main() {
    int n = 1, count = 0, sq, cr;
    for ( ; count < 30; ++n) {
        sq = n * n;
        cr = (int)cbrt((double)sq);
        if (cr * cr * cr != sq) {
            count++;
            printf("%d\n", sq);
        }
        else {
            printf("%d is square and cube\n", sq);
        }
    }
    return 0;
}
```


```txt

Same as Ring example.

```



## C++

```cpp
#include <iostream>
#include <cmath>

int main() {
    int n = 1;
    int count = 0;
    int sq;
    int cr;

    for (; count < 30; ++n) {
        sq = n * n;
        cr = cbrt(sq);
        if (cr * cr * cr != sq) {
            count++;
            std::cout << sq << '\n';
        } else {
            std::cout << sq << " is square and cube\n";
        }
    }

    return 0;
}
```

```txt
1 is square and cube
4
9
16
25
36
49
64 is square and cube
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 is square and cube
784
841
900
961
1024
1089
```



## C#


```c#
using System;
using System.Collections.Generic;
using static System.Console;
using static System.Linq.Enumerable;

public static class SquareButNotCube
{
    public static void Main() {
        var squares = from i in Integers() select i * i;
        var cubes = from i in Integers() select i * i * i;

        foreach (var x in Merge().Take(33)) {
            WriteLine(x.isCube ? x.n + " (also cube)" : x.n + "");
        }

        IEnumerable<int> Integers() {
            for (int i = 1; ;i++) yield return i;
        }

        IEnumerable<(int n, bool isCube)> Merge() {
            using (var s = squares.GetEnumerator())
            using (var c = cubes.GetEnumerator()) {
                s.MoveNext();
                c.MoveNext();
                while (true) {
                    if (s.Current < c.Current) {
                        yield return (s.Current, false);
                        s.MoveNext();
                    } else if (s.Current == c.Current) {
                        yield return (s.Current, true);
                        s.MoveNext();
                        c.MoveNext();
                    } else {
                        c.MoveNext();
                    }
                }
            }
        }

    }
}
```

<pre style="height:30ex;overflow:scroll">
1 (also cube)
4
9
16
25
36
49
64 (also cube)
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 (also cube)
784
841
900
961
1024
1089
```



## D

```d
import std.algorithm;
import std.range;
import std.stdio;

auto squareGen() {
    struct Gen {
        private int add = 3;
        private int curr = 1;

        bool empty() {
            return curr < 0;
        }

        auto front() {
            return curr;
        }

        void popFront() {
            curr += add;
            add += 2;
        }
    }

    return Gen();
}

auto cubeGen() {
    struct Gen {
        private int add1 = 7;
        private int add2 = 12;
        private int curr = 1;

        bool empty() {
            return curr < 0;
        }

        auto front() {
            return curr;
        }

        void popFront() {
            curr += add1;
            add1 += add2;
            add2 += 6;
        }
    }

    return Gen();
}

auto merge() {
    struct Gen {
        private auto sg = squareGen();
        private auto cg = cubeGen();

        bool empty() {
            return sg.empty || cg.empty;
        }

        auto front() {
            import std.typecons;
            if (sg.front == cg.front) {
                return tuple!("num", "isCube")(sg.front, true);
            } else {
                return tuple!("num", "isCube")(sg.front, false);
            }
        }

        void popFront() {
            while (true) {
                if (sg.front < cg.front) {
                    sg.popFront();
                    return;
                } else if (sg.front == cg.front) {
                    sg.popFront();
                    cg.popFront();
                    return;
                } else {
                    cg.popFront();
                }
            }
        }
    }

    return Gen();
}

void main() {
    foreach (p; merge.take(33)) {
        if (p.isCube) {
            writeln(p.num, " (also cube)");
        } else {
            writeln(p.num);
        }
    }
}
```

```txt
1 (also cube)
4
9
16
25
36
49
64 (also cube)
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 (also cube)
784
841
900
961
1024
1089
```


=={{header|F_Sharp|F#}}==

```fsharp

let rec fN n g φ=if φ<31 then match compare(n*n)(g*g*g) with | -1->printfn "%d"(n*n);fN(n+1) g (φ+1)
                                                             |  0->printfn "%d cube and square"(n*n);fN(n+1)(g+1)φ
                                                             |  1->fN n (g+1) φ
fN 1 1 1

```

```txt

1 cube and square
4
9
16
25
36
49
64 cube and square
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 cube and square
784
841
900
961
1024
1089

```



## Factor

```factor
USING: combinators interpolate io kernel prettyprint math
math.functions math.order pair-rocket ;
IN: rosetta-code.square-but-not-cube

: fn ( s c n -- s' c' n' )
    dup 31 < [
        2over [ sq ] [ 3 ^ ] bi* <=> {
            +lt+ => [ [ dup sq . 1 + ] 2dip 1 + fn ]
            +eq+ => [ [ dup sq [I ${} cube and squareI] nl 1 + ] [ 1 + ] [ ] tri* fn ]
            +gt+ => [ [ 1 + ] dip fn ]
        } case
    ] when ;

1 1 1 fn 3drop
```

```txt

1 cube and square
4
9
16
25
36
49
64 cube and square
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 cube and square
784
841
900
961
1024
1089

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    for n, count := 1, 0; count < 30; n++ {
        sq := n * n
        cr := int(math.Cbrt(float64(sq)))
        if cr*cr*cr != sq {
            count++
            fmt.Println(sq)
        } else {
            fmt.Println(sq, "is square and cube")
        }
    }
}
```


```txt

1 is square and cube
4
9
16
25
36
49
64 is square and cube
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 is square and cube
784
841
900
961
1024
1089

```



## Haskell


```haskell
import Data.List (partition, sortBy)
import Control.Monad (join)
import Data.Ord (comparing)

isCube :: Int -> Bool
isCube n = n == round (fromIntegral n ** (1 / 3)) ^ 3

both, only :: [Int]
(both, only) = partition isCube $ join (*) <$> [1 ..]

-- TEST -----------------------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines) $
  uncurry ((++) . show) <$>
  sortBy
    (comparing fst)
    ((flip (,) " (also cube)" <$> take 3 both) ++ (flip (,) "" <$> take 30 only))
```


Or simply


```haskell
import Control.Monad (join)

cubeRoot :: Int -> Int
cubeRoot = round . (** (1 / 3)) . fromIntegral

isCube :: Int -> Bool
isCube = (==) <*> ((^ 3) . cubeRoot)

-- TEST ---------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines) $
  (\x ->
      show x ++
      if isCube x
        then concat [" (also cube of ", show (cubeRoot x), ")"]
        else []) <$>
  take 33 (join (*) <$> [1 ..])
```


Or, if we prefer a finite series to an infinite one

```haskell
isCube :: Int -> Bool
isCube = (==) <*> ((^ 3) . round . (** (1 / 3)) . fromIntegral)

squares :: Int -> Int -> [Int]
squares m n = (>>= id) (*) <$> [m .. n]

-- TEST ---------------------------------------------------
main :: IO ()
main = (putStrLn . unlines) $ (++) . show <*> label <$> squares 1 33

label :: Int -> String
label n
  | isCube n = " (also cube)"
  | otherwise = ""
```

```txt
1 (also cube)
4
9
16
25
36
49
64 (also cube)
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 (also cube)
784
841
900
961
1024
1089
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Square.bas"
110 LET SQNOTCB,SQANDCB,SQNUM,CBNUM,CBN,SQN,D1=0:LET SQD,D2=1
120 DO
130   LET SQN=SQN+1:LET SQNUM=SQNUM+SQD:LET SQD=SQD+2
140   IF SQNUM>CBNUM THEN
150     LET CBN=CBN+1:LET CBNUM=CBNUM+D2
160     LET D1=D1+6:LET D2=D2+D1
170   END IF
180   IF SQNUM<>CBNUM THEN
190     PRINT SQNUM:LET SQNOTCB=SQNOTCB+1
200   ELSE
210     PRINT SQNUM,SQN;"*";SQN;"=";CBN;"*";CBN;"*";CBN
220     LET SQANDCB=SQANDCB+1
230   END IF
240 LOOP UNTIL SQNOTCB>=30
250 PRINT SQANDCB;"where numbers are square and cube."
```



## J

'''Solution:'''

```j
isSqrNotCubeofInt=: (*. -.)/@(= <.)@(2 3 %:/ ])
getN_Indicies=: adverb def '[ ({. I.) [ (] , [: u (i.200) + #@])^:(> +/)^:_ u@]'
```


'''Example Use:'''

```j
   I. isSqrNotCubeofInt i.1090           NB. If we know the upper limit required to get first 30
4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841 900 961 1024 1089
   30 isSqrNotCubeofInt getN_Indicies 0  NB. otherwise iteratively build list until first 30 found
4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841 900 961 1024 1089
```


'''Alternative Solution:'''

Breaking up the solution above into smaller chunks with comments...

```j
isInt=: = <.                                     NB. are numbers integers?
sqrcube=: 2 3 %:/ ]                              NB. table of 2nd and 3rd roots of y
isSqrNotCubeofInt=: (*. -.)/@isInt@sqrcube       NB. is y the square but not cube of an integer?

getIdx=: {. I.                                   NB. get indicies of first x ones in boolean y

process_more=: adverb def '] , [: u (i.200) + #@]'  NB. process the next 200 indicies with u and append to y
notEnough=: > +/                                 NB. is left arg greater than sum of right arg
while=: conjunction def 'u^:v^:_'                NB. repeat u while v is true

process_until_enough=: adverb def 'u process_more while notEnough u'
```


'''Example Use:'''

```j
   30 ([ getIdx isSqrNotCubeofInt process_until_enough) 0
4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841
900 961 1024 1089

```



## Java


```java
public class SquaresCubes {
    public static boolean isPerfectCube(long n) {
        long c = (long)Math.cbrt((double)n);
        return ((c * c * c) == n);
    }

    public static void main(String... args) {
        long n = 1;
        int squareOnlyCount = 0;
        int squareCubeCount = 0;
        while ((squareOnlyCount < 30) || (squareCubeCount < 3)) {
            long sq = n * n;
            if (isPerfectCube(sq)) {
                squareCubeCount++;
                System.out.println("Square and cube: " + sq);
            }
            else {
                squareOnlyCount++;
                System.out.println("Square: " + sq);
            }
            n++;
        }
    }
}
```

```txt

Square and cube: 1
Square: 4
Square: 9
Square: 16
Square: 25
Square: 36
Square: 49
Square and cube: 64
Square: 81
Square: 100
Square: 121
Square: 144
Square: 169
Square: 196
Square: 225
Square: 256
Square: 289
Square: 324
Square: 361
Square: 400
Square: 441
Square: 484
Square: 529
Square: 576
Square: 625
Square: 676
Square and cube: 729
Square: 784
Square: 841
Square: 900
Square: 961
Square: 1024
Square: 1089
```



## JavaScript


```javascript
(() => {
    'use strict';

    const main = () =>
        unlines(map(
            x => x.toString() + (
                isCube(x) ? (
                    ` (cube of ${cubeRootInt(x)} and square of ${
                            Math.pow(x, 1/2)
                    })`
                ) : ''
            ),
            map(x => x * x, enumFromTo(1, 33))
        ));

    // isCube :: Int -> Bool
    const isCube = n =>
        n === Math.pow(cubeRootInt(n), 3);

    // cubeRootInt :: Int -> Int
    const cubeRootInt = n => Math.round(Math.pow(n, 1 / 3));


    // GENERIC FUNCTIONS ----------------------------------

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

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

```txt
1 (cube of 1 and square of 1)
4
9
16
25
36
49
64 (cube of 4 and square of 8)
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 (cube of 9 and square of 27)
784
841
900
961
1024
1089
```



## Julia



```Julia

iscube(n) = n == round(Int, cbrt(n))^3

println(collect(Iterators.take((n^2 for n in 1:10^6 if !iscube(n^2)), 30)))

```

[4, 9, 16, 25, 36, 49, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400, 441, 484, 529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]


## Kotlin


```scala
// Version 1.2.60

fun main(args: Array<String>) {
    var n = 1
    var count = 0
    while (count < 30) {
        val sq = n * n
        val cr = Math.cbrt(sq.toDouble()).toInt()
        if (cr * cr * cr != sq) {
            count++
            println(sq)
        }
        else {
            println("$sq is square and cube")
        }
        n++
    }
}
```


```txt

Same as Ring example.

```



## Lua

Calculating cube roots with x^(1/3) caused problems with floating-point 'dust' so the Newton-Raphson method is used instead.

```Lua
function nthroot (x, n)
  local r = 1
  for i = 1, 16 do
    r = (((n - 1) * r) + x / (r ^ (n - 1))) / n
  end
  return r
end

local i, count, sq, cbrt = 0, 0
while count < 30 do
  i = i + 1
  sq = i * i
  -- The next line should say nthroot(sq, 3), right? But this works. Maths, eh?
  cbrt = nthroot(i, 3)
  if cbrt == math.floor(cbrt) then
    print(sq .. " is square and cube")
  else
    print(sq)
    count = count + 1
  end
end
```

```txt
1 is square and cube
4
9
16
25
36
49
64 is square and cube
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 is square and cube
784
841
900
961
1024
1089
```


## Nim


```Nim
var count = 0
var n, c, c3 = 1

while count < 30:
  var sq = n * n
  while c3 < sq:
    inc c
    c3 = c * c * c
  if c3 == sq:
    echo $sq, " is square and cube"
  else:
    echo $sq
    inc count
  inc n
```


```txt

1 is square and cube
4
9
16
25
36
49
64 is square and cube
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 is square and cube
784
841
900
961
1024
1089

```



## OCaml

```ocaml
let rec fN n g phi =
  if phi < 31 then
    match compare (n*n) (g*g*g) with
    | -1 -> Printf.printf "%d\n" (n*n); fN (n+1) g (phi+1)
    |  0 -> Printf.printf "%d cube and square\n" (n*n); fN (n+1) (g+1) phi
    |  1 -> fN n (g+1) phi
    | _ -> assert false
;;

fN 1 1 1
```




## Pascal

Only using addition :-)

```pascal
program SquareButNotCube;
var
  sqN,
  sqDelta,
  SqNum,

  cbN,
  cbDelta1,
  cbDelta2,
  CbNum,

  CountSqNotCb,
  CountSqAndCb : NativeUint;

begin
  CountSqNotCb := 0;
  CountSqAndCb := 0;
  SqNum := 0;
  CbNum := 0;
  cbN := 0;
  sqN := 0;
  sqDelta := 1;
  cbDelta1 := 0;
  cbDelta2 := 1;
  repeat
    inc(sqN);
    inc(sqNum,sqDelta);
    inc(sqDelta,2);
    IF sqNum>cbNum then
    Begin
      inc(cbN);
      cbNum := cbNum+cbDelta2;
      inc(cbDelta1,6);// 0,6,12,18...
      inc(cbDelta2,cbDelta1);//1,7,19,35...
    end;
    IF sqNum <> cbNUm then
    Begin
      writeln(sqNum :25);
      inc(CountSqNotCb);
    end
    else
    Begin
      writeln(sqNum:25,sqN:10,'*',sqN,' = ',cbN,'*',cbN,'*',cbN);
      inc(CountSqANDCb);
    end;
  until CountSqNotCb >= 30;//sqrt(High(NativeUint));
  writeln(CountSqANDCb,' where numbers are square and cube ');
end.
```

```txt
                        1         1*1 = 1*1*1
                        4
                        9
                       16
                       25
                       36
                       49
                       64         8*8 = 4*4*4
                       81
                      100
                      121
                      144
                      169
                      196
                      225
                      256
                      289
                      324
                      361
                      400
                      441
                      484
                      529
                      576
                      625
                      676
                      729        27*27 = 9*9*9
                      784
                      841
                      900
                      961
                     1024
                     1089
3 where numbers are square and cube

// there are 1625 numbers which are square and cube < High(Uint64)
//18412815093994140625  4291015625*4291015625 = 2640625*2640625*2640625

```



## Perl


### = Hash =

Use a hash to track state (and avoid floating-point math).

```perl
while ($cnt < 30) {
    $n++;
    $h{$n**2}++;
    $h{$n**3}--;
    $cnt++ if $h{$n**2} > 0;
}

print "First 30 positive integers that are a square but not a cube:\n";
print "$_ " for sort { $a <=> $b } grep { $h{$_} == 1 } keys %h;

print "\n\nFirst 3 positive integers that are both a square and a cube:\n";
print "$_ " for sort { $a <=> $b } grep { $h{$_} == 0 } keys %h;
```

```txt
First 30 positive integers that are a square but not a cube:
4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841 900 961 1024 1089

First 3 positive integers that are both a square and a cube:
1 64 729
```



### = Generators =


A more general approach involving generators/closures to implement 'lazy' lists as in the Perl 6 example.
Using ideas and code from the very similar [[Generator_Exponential#Perl 6|Generator exponential]] task.
Output is the same as previous.


```perl
# return an anonymous subroutine that generates stream of specified powers
sub gen_pow {
    my $m = shift;
    my $e = 1;
    return sub { return $e++ ** $m; };
}

# return an anonymous subroutine generator that filters output from supplied generators g1 and g2
sub gen_filter {
    my($g1, $g2) = @_;
    my $v1;
    my $v2 = $g2->();
    return sub {
        while (1) {
            $v1 = $g1->();
            $v2 = $g2->() while $v1 > $v2;
            return $v1 unless $v1 == $v2;
        }
    };
}

my $pow2 = gen_pow(2);
my $pow3 = gen_pow(3);
my $squares_without_cubes = gen_filter($pow2, $pow3);
print "First 30 positive integers that are a square but not a cube:\n";
print $squares_without_cubes->() . ' ' for 1..30;

my $pow6 = gen_pow(6);
print "\n\nFirst 3 positive integers that are both a square and a cube:\n";
print $pow6->() . ' ' for 1..3;
```



## Perl 6


```perl6
my @square-and-cube = map { .⁶ }, 1..Inf;

my @square-but-not-cube = (1..Inf).map({ .² }).grep({ $_ ∉ @square-and-cube[^@square-and-cube.first: * > $_, :k]});

put "First 30 positive integers that are a square but not a cube: \n",  @square-but-not-cube[^30];

put "\nFirst 15 positive integers that are both a square and a cube: \n", @square-and-cube[^15];
```

```txt
First 30 positive integers that are a square but not a cube:
4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841 900 961 1024 1089

First 15 positive integers that are both a square and a cube:
1 64 729 4096 15625 46656 117649 262144 531441 1000000 1771561 2985984 4826809 7529536 11390625
```



## Phix


```Phix
integer square = 1, squared = 1*1,
        cube   = 1, cubed = 1*1*1,
        count  = 0

while count<30 do
    squared = square*square
    while squared>cubed do cube += 1; cubed = cube*cube*cube end while
    if squared=cubed then
        printf(1,"%d: %d == %d^3\n",{square,squared,cube})
    else
        count += 1
        printf(1,"%d: %d\n",{square,squared})
    end if
    square += 1
end while

printf(1,"\nThe first 15 positive integers that are both a square and a cube: \n")
?sq_power(tagset(15),6)
```

```txt

1: 1 == 1^3
2: 4
3: 9
4: 16
5: 25
6: 36
7: 49
8: 64 == 4^3
9: 81
10: 100
11: 121
12: 144
13: 169
14: 196
15: 225
16: 256
17: 289
18: 324
19: 361
20: 400
21: 441
22: 484
23: 529
24: 576
25: 625
26: 676
27: 729 == 9^3
28: 784
29: 841
30: 900
31: 961
32: 1024
33: 1089

The first 15 positive integers that are both a square and a cube:
{1,64,729,4096,15625,46656,117649,262144,531441,1000000,1771561,2985984,4826809,7529536,11390625}

```



## Python


```python
# nonCubeSquares :: Int -> [(Int, Bool)]
def nonCubeSquares(n):
    upto = enumFromTo(1)
    ns = upto(n)
    setCubes = set(x ** 3 for x in ns)
    ms = upto(n + len(set(x * x for x in ns).intersection(
        setCubes
    )))
    return list(tuple([x * x, x in setCubes]) for x in ms)


# squareListing :: [(Int, Bool)] -> [String]
def squareListing(xs):
    justifyIdx = justifyRight(len(str(1 + len(xs))))(' ')
    justifySqr = justifyRight(1 + len(str(xs[-1][0])))(' ')
    return list(
        '(' + str(1 + idx) + '^2 = ' + str(n) +
        ' = ' + str(round(n ** (1 / 3))) + '^3)' if bln else (
            justifyIdx(1 + idx) + ' ->' +
            justifySqr(n)
        )
        for idx, (n, bln) in enumerate(xs)
    )


def main():
    print(
        unlines(
            squareListing(
                nonCubeSquares(30)
            )
        )
    )


# GENERIC ------------------------------------------------------------------

# enumFromTo :: Int -> Int -> [Int]
def enumFromTo(m):
    return lambda n: list(range(m, 1 + n))


# justifyRight :: Int -> Char -> String -> String
def justifyRight(n):
    return lambda cFiller: lambda a: (
        ((n * cFiller) + str(a))[-n:]
    )


# unlines :: [String] -> String
def unlines(xs):
    return '\n'.join(xs)


main()
```

```txt
(1^2 = 1 = 1^3)
 2 ->    4
 3 ->    9
 4 ->   16
 5 ->   25
 6 ->   36
 7 ->   49
(8^2 = 64 = 4^3)
 9 ->   81
10 ->  100
11 ->  121
12 ->  144
13 ->  169
14 ->  196
15 ->  225
16 ->  256
17 ->  289
18 ->  324
19 ->  361
20 ->  400
21 ->  441
22 ->  484
23 ->  529
24 ->  576
25 ->  625
26 ->  676
(27^2 = 729 = 9^3)
28 ->  784
29 ->  841
30 ->  900
31 ->  961
32 -> 1024
33 -> 1089
```



## Racket


Using a generator it _is_ possible to print the cubes in-line, but I've chosen to show reusing the generator / for / sequence interaction:


```racket
#lang racket
(require racket/generator)

;; generates values:
;;  next square
;;  cube-root if cube, #f otherwise
(define (make-^2-but-not-^3-generator)
  (generator
   ()
   (let loop ((s 1) (c 1))
     (let ((s^2 (sqr s)) (c^3 (* c c c)))
       (yield s^2 (and (= s^2 c^3) c))
       (loop (add1 s) (+ c (if (>= s^2 c^3) 1 0)))))))

(for/list ((x (in-range 1 31))
           ((s^2 _) (sequence-filter (λ (_ c) (not c)) (in-producer (make-^2-but-not-^3-generator)))))
  s^2)

(for ((x (in-range 1 4))
      ((s^2 c) (sequence-filter (λ (s^2 c) c) (in-producer (make-^2-but-not-^3-generator)))))
  (printf "~a: ~a is also ~a^3~%" x s^2 c))
```


```txt
'(4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841 900 961 1024 1089)
1: 1 is also 1^3
2: 64 is also 4^3
3: 729 is also 9^3
```



## REXX

Programming note:   extra code was added to support an additional output format   (see the 2<sup>nd</sup> '''output''' section).

```rexx
/*REXX pgm shows N ints>0 that are squares and not cubes, & which are squares and cubes.*/
numeric digits 20                                /*be able to handle some large numbers.*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=30                     /*Not specified?  Then use the default.*/
sqcb= N<0                                        /*N negative? Then show squares & cubes*/
N = abs(N)                                       /*define  N  to be the absolute value. */
w= length(N) + 3                                 /*W:  used for aligning output columns.*/
say '   count   '                                /*display the  1st  line of the title. */
say '  ───────  '                                /*   "     "   2nd    "   "  "    "    */
@.= 0                                            /*@:  stemmed array for computed cubes.*/
                   #= 0;  ##= 0                  /*count (integer): squares & not cubes.*/
     do j=1  until #==N | ##==N                  /*loop 'til enough    "    "  "    "   */
     sq= j*j;        cube= sq*j;     @.cube= 1   /*compute the square of J and the cube.*/
     if @.sq  then do
                   ##= ## + 1                    /*bump the counter of squares and cubs.*/
                   if \sqcb  then counter=   left('', 12)     /*don't show this counter.*/
                             else counter= center(##, 12)     /*  do    "    "     "    */
                   say    counter     right(sq, 3*w)     'is a square and       a cube'
                   end
              else do
                   if sqcb  then  iterate
                   #= # + 1                      /*bump the counter of squares & ¬ cubes*/
                   say center(#, 12)  right(sq, 3*w)     'is a square and  not  a cube'
                   end
     end   /*j*/                                 /*stick a fork in it,  we're all done. */
```

```txt

   count
  ───────
                           1 is a square and       a cube
     1                     4 is a square and  not  a cube
     2                     9 is a square and  not  a cube
     3                    16 is a square and  not  a cube
     4                    25 is a square and  not  a cube
     5                    36 is a square and  not  a cube
     6                    49 is a square and  not  a cube
                          64 is a square and       a cube
     7                    81 is a square and  not  a cube
     8                   100 is a square and  not  a cube
     9                   121 is a square and  not  a cube
     10                  144 is a square and  not  a cube
     11                  169 is a square and  not  a cube
     12                  196 is a square and  not  a cube
     13                  225 is a square and  not  a cube
     14                  256 is a square and  not  a cube
     15                  289 is a square and  not  a cube
     16                  324 is a square and  not  a cube
     17                  361 is a square and  not  a cube
     18                  400 is a square and  not  a cube
     19                  441 is a square and  not  a cube
     20                  484 is a square and  not  a cube
     21                  529 is a square and  not  a cube
     22                  576 is a square and  not  a cube
     23                  625 is a square and  not  a cube
     24                  676 is a square and  not  a cube
                         729 is a square and       a cube
     25                  784 is a square and  not  a cube
     26                  841 is a square and  not  a cube
     27                  900 is a square and  not  a cube
     28                  961 is a square and  not  a cube
     29                 1024 is a square and  not  a cube
     30                 1089 is a square and  not  a cube

```

```txt

   count
  ───────
     1                     1 is a square and       a cube
     2                    64 is a square and       a cube
     3                   729 is a square and       a cube
     4                  4096 is a square and       a cube
     5                 15625 is a square and       a cube
     6                 46656 is a square and       a cube
     7                117649 is a square and       a cube
     8                262144 is a square and       a cube
     9                531441 is a square and       a cube
     10              1000000 is a square and       a cube
     11              1771561 is a square and       a cube
     12              2985984 is a square and       a cube
     13              4826809 is a square and       a cube
     14              7529536 is a square and       a cube
     15             11390625 is a square and       a cube
     16             16777216 is a square and       a cube
     17             24137569 is a square and       a cube
     18             34012224 is a square and       a cube
     19             47045881 is a square and       a cube
     20             64000000 is a square and       a cube
     21             85766121 is a square and       a cube
     22            113379904 is a square and       a cube
     23            148035889 is a square and       a cube
     24            191102976 is a square and       a cube
     25            244140625 is a square and       a cube
     26            308915776 is a square and       a cube
     27            387420489 is a square and       a cube
     28            481890304 is a square and       a cube
     29            594823321 is a square and       a cube
     30            729000000 is a square and       a cube
     31            887503681 is a square and       a cube
     32           1073741824 is a square and       a cube
     33           1291467969 is a square and       a cube
     34           1544804416 is a square and       a cube
     35           1838265625 is a square and       a cube
     36           2176782336 is a square and       a cube
     37           2565726409 is a square and       a cube
     38           3010936384 is a square and       a cube
     39           3518743761 is a square and       a cube
     40           4096000000 is a square and       a cube
     41           4750104241 is a square and       a cube
     42           5489031744 is a square and       a cube
     43           6321363049 is a square and       a cube
     44           7256313856 is a square and       a cube
     45           8303765625 is a square and       a cube
     46           9474296896 is a square and       a cube
     47          10779215329 is a square and       a cube
     48          12230590464 is a square and       a cube
     49          13841287201 is a square and       a cube
     50          15625000000 is a square and       a cube
     51          17596287801 is a square and       a cube
     52          19770609664 is a square and       a cube
     53          22164361129 is a square and       a cube
     54          24794911296 is a square and       a cube
     55          27680640625 is a square and       a cube

```



## Ring


```ring

# Project : Square but not cube

limit = 30
num = 0
sq = 0
while num < limit
      sq = sq + 1
      sqpow = pow(sq,2)
      flag = iscube(sqpow)
      if flag = 0
         num = num + 1
         see sqpow + nl
      else
         see "" + sqpow + " is square and cube" + nl
      ok
end

func iscube(cube)
     for n = 1 to cube
         if pow(n,3) = cube
            return 1
         ok
     next
     return 0

```

Output:

```txt

1 is square and cube
4
9
16
25
36
49
64 is square and cube
81
100
121
144
169
196
225
256
289
324
361
400
441
484
529
576
625
676
729 is square and cube
784
841
900
961
1024
1089

```



## Ruby


```ruby
#!/usr/bin/env ruby

class PowIt
	:next

	def initialize
		@next = 1;
	end
end

class SquareIt < PowIt
	def next
		result = @next ** 2
		@next += 1
		return result
	end
end

class CubeIt < PowIt
	def next
		result = @next ** 3
		@next += 1
		return result
	end
end

squares = []
hexponents = []

squit = SquareIt.new
cuit = CubeIt.new

s = squit.next
c = cuit.next

while (squares.length < 30 || hexponents.length < 3)
	if s < c
		squares.push(s) if squares.length < 30
		s = squit.next
	elsif s == c
		hexponents.push(s) if hexponents.length < 3
		s = squit.next
		c = cuit.next
	else
		c = cuit.next
	end
end

puts "Squares:"
puts squares.join(" ")

puts "Square-and-cubes:"
puts hexponents.join(" ")
```

```txt
Squares:
4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841 900 961 1024 1089
Square-and-cubes:
1 64 729
```



## Scala

This example uses Spire's SafeLongs for both removing any size limitation and making exponentiation/roots cleaner, at the expense of initializing lists with an iteration vs the simpler .from(n). Both the non-cube-squares and square-cubes are lazily evaluated lists, the former is constructed by making lists of square numbers between each pair of cubes and flattening them into one list, the latter is formed by filtering non-squares out of a list of cubes.


```scala
import spire.math.SafeLong
import spire.implicits._

def ncs: LazyList[SafeLong] = LazyList.iterate(SafeLong(1))(_ + 1).flatMap(n => Iterator.iterate(n.pow(3).sqrt + 1)(_ + 1).map(i => i*i).takeWhile(_ < (n + 1).pow(3)))
def scs: LazyList[SafeLong] = LazyList.iterate(SafeLong(1))(_ + 1).map(_.pow(3)).filter(n => n.sqrt.pow(2) == n)
```


```txt
scala> println(ncs.take(30).mkString(", "))
4, 9, 16, 25, 36, 49, 81, 100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400, 441, 484, 529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089

scala> println(scs.take(3).mkString(", "))
1, 64, 729
```



## Sidef

```ruby
var square_and_cube = Enumerator({|f|
    1..Inf -> each {|n| f(n**6) }
})

var square_but_not_cube = Enumerator({|f|
    1..Inf -> lazy.map {|n| n**2 }.grep {|n| !n.is_power(3) }.each {|n| f(n) }
})

say "First 30 positive integers that are a square but not a cube:"
say square_but_not_cube.first(30).join(' ')

say "First 15 positive integers that are both a square and a cube:"
say square_and_cube.first(15).join(' ')
```

```txt

First 30 positive integers that are a square but not a cube:
4 9 16 25 36 49 81 100 121 144 169 196 225 256 289 324 361 400 441 484 529 576 625 676 784 841 900 961 1024 1089
First 15 positive integers that are both a square and a cube:
1 64 729 4096 15625 46656 117649 262144 531441 1000000 1771561 2985984 4826809 7529536 11390625

```



## Visual Basic .NET

Inspired by the '''F#''' version, but no longer resembles it. Removed the recursion, multiplying (like the '''D''' and '''Pascal''' versions, only addition is used to calculate squares and cubes), '''match''' (Select Case) statement, and hard-coded limit.

```vbnet
Module Module1

  ' flag / mask explanation:
  '  bit 0 (1) = increment square
  '  bit 1 (2) = increment cube
  '  bit 2 (4) = has output

  ' Checks flag against mask, then advances mask.
  Function ChkFlg(flag As Integer, ByRef mask As Integer) As Boolean
    ChkFlg = (flag And mask) = mask : mask <<= 1
  End Function

  Sub SwoC(limit As Integer)
    Dim count, square, delta, cube, d1, d2, flag, mask As Integer, s as string = ""
    count = 1 : square = 1 : delta = 1 : cube = 1 : d1 = 1 : d2 = 0
    While count <= limit
      flag = {5, 7, 2}(1 + square.CompareTo(cube))
      If flag = 7 Then s = String. Format("   {0} (also cube)", square)
      If flag = 5 Then s = String.Format("{0,-2} {1}", count, square) : count += 1
      mask = 1 : If ChkFlg(flag, mask) Then delta += 2 : square += delta
      If ChkFlg(flag, mask) Then d2 += 6 : d1 += d2 : cube += d1
      If ChkFlg(flag, mask) Then Console.WriteLine(s)
    End While
  End Sub

  Sub Main()
    SwoC(30)
  End Sub

End Module
```

```txt
   1 (also cube)
1  4
2  9
3  16
4  25
5  36
6  49
   64 (also cube)
7  81
8  100
9  121
10 144
11 169
12 196
13 225
14 256
15 289
16 324
17 361
18 400
19 441
20 484
21 529
22 576
23 625
24 676
   729 (also cube)
25 784
26 841
27 900
28 961
29 1024
30 1089
```



## zkl


```zkl
println("First 30 positive integers that are a square but not a cube:");
squareButNotCube:=(1).walker(*).tweak(fcn(n){
   sq,cr := n*n, sq.toFloat().pow(1.0/3).round(); // cube root(64)<4
   if(sq==cr*cr*cr) Void.Skip else sq
});
squareButNotCube.walk(30).concat(",").println("\n");

println("First 15 positive integers that are both a square and a cube:");
println((1).walker(*).tweak((1).pow.unbind().fp1(6)).walk(15));
```

```txt

First 30 positive integers that are a square but not a cube:
4,9,16,25,36,49,81,100,121,144,169,196,225,256,289,324,361,400,441,484,529,576,625,676,784,841,900,961,1024,1089

First 15 positive integers that are both a square and a cube:
L(1,64,729,4096,15625,46656,117649,262144,531441,1000000,1771561,2985984,4826809,7529536,11390625

```

