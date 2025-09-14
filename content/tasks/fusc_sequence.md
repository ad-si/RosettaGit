+++
title = "Fusc sequence"
description = ""
date = 2019-09-20T00:03:05Z
aliases = []
[extra]
id = 22192
[taxonomies]
categories = ["Mathematics", "Sequences", "task"]
tags = []
languages = [
  "algol_68",
  "awk",
  "c",
  "csharp",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "sidef",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

### Definitions
The   '''fusc'''   integer sequence is defined as:
::*   fusc(0) = 0
::*   fusc(1) = 1
::*   for '''n'''>1,   the   '''n'''<sup>th</sup>   term is defined as:
::::*   if   '''n'''   is       even;     fusc(n) = fusc(n/2)
::::*   if   '''n'''   is   odd;     fusc(n) = fusc<big>(</big>(n-1)/2<big>)</big>   <big>+</big>   fusc<big>(</big>(n+1)/2<big>)</big>


Note that MathWorld's definition starts with unity, not zero.   This task will be using the OEIS' version   (above).



### An observation
:::::*   fusc(A) = fusc(B)

where   '''A'''   is some non-negative integer expressed in binary,   and
where   '''B'''   is the binary value of   '''A'''   reversed.



Fusc numbers are also known as:
::*   fusc function   (by Dijkstra, 1982)
::*   Stern's Diatomic series   (although it starts with unity, not zero)
::*   Stern-Brocot sequence   (although it starts with unity, not zero)



;Task:
::*   show the first   '''61'''   fusc numbers (starting at zero) in a horizontal format.
::*   show the fusc number (and its index) whose length is greater than any previous fusc number length.
::::*   (the length is the number of digits when the fusc number is expressed in decimal.)
::*   show all numbers with commas   (if appropriate).
::*   show all output here.


;Related task:
::*   [[Stern-Brocot_sequence|RosettaCode Stern-Brocot sequence]]
<!-- This is similar as  "generate primes by trial division",  and  "generate primes via a sieve".   Both Rosetta Code tasks have their uses and methods of generation.  !~-->


;Also see:
::*   the MathWorld entry:   [http://mathworld.wolfram.com/SternsDiatomicSeries.html Stern's Diatomic Series].
::*   the OEIS      entry:   [http://oeis.org/A2487 A2487].





## ALGOL 68


```algol68
BEGIN
    # calculate some members of the fusc sequence              #
    #    f0 = 0, f1 = 1, fn = f(n/2)                 if n even #
    #                       = f(n-1)/2) + f((n+1)/2) if n odd  #

    # constructs an array of the first n elements of the fusc sequence #
    PROC fusc sequence = ( INT n )[]INT:
         BEGIN
            [ 0 : n ]INT a;
            IF n > 0 THEN
                a[ 0 ] := 0;
                IF n > 1 THEN
                    a[ 1 ] := 1;
                    INT i2 := 1;
                    FOR i FROM 2 BY 2 TO n - 1 DO
                        a[ i     ] := a[ i2 ];
                        a[ i + 1 ] := a[ # j - i # i2 ] + a[ # ( j + 1 ) OVER 2 # i2 + 1 ];
                        i2 +:= 1
                    OD
                FI
            FI;
            a[ 0 : n - 1 AT 0 ]
         END ; # fusc #

    []INT f = fusc sequence( 800 000 );
    FOR i FROM 0 TO 60 DO print( ( " ", whole( f[ i ], 0 ) ) ) OD;
    print( ( newline ) );
    # find the lowest elements of the sequence that have 1, 2, 3, etc. digits #
    print( ( "Sequence elements where number of digits of the value increase:", newline ) );
    print( ( "       n    fusc(n)", newline ) );
    INT digit power := 0;
    FOR i FROM LWB f TO UPB f DO
        IF f[ i ] >= digit power THEN
            # found the first number with this many digits #
            print( ( whole( i, -8 ), " ", whole( f[ i ], -10 ), newline ) );
            IF digit power = 0 THEN digit power := 1 FI;
            digit power *:= 10
        FI
    OD
END
```

{{out}}

```txt

 0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4
Sequence elements where number of digits of the value increase:
       n    fusc(n)
       0          0
      37         11
    1173        108
   35499       1076
  699051      10946

```


## AWK


```AWK

# syntax: GAWK -f FUSC_SEQUENCE.AWK
# converted from C
BEGIN {
    for (i=0; i<61; i++) {
      printf("%d ",fusc(i))
    }
    printf("\n")
    print("fusc numbers whose length is greater than any previous fusc number length")
    printf("%9s %9s\n","fusc","index")
    for (i=0; i<=700000; i++) {
      f = fusc(i)
      leng = num_leng(f)
      if (leng > max_leng) {
        max_leng = leng
        printf("%9s %9s\n",commatize(f),commatize(i))
      }
    }
    exit(0)
}
function commatize(x,  num) {
    if (x < 0) {
      return "-" commatize(-x)
    }
    x = int(x)
    num = sprintf("%d.",x)
    while (num ~ /^[0-9][0-9][0-9][0-9]/) {
      sub(/[0-9][0-9][0-9][,.]/,",&",num)
    }
    sub(/\.$/,"",num)
    return(num)
}
function fusc(n) {
    if (n == 0 || n == 1) {
      return(n)
    }
    else if (n % 2 == 0) {
      return fusc(n/2)
    }
    else {
      return fusc((n-1)/2) + fusc((n+1)/2)
    }
}
function num_leng(n,  sum) {
    sum = 1
    while (n > 9) {
      n = int(n/10)
      sum++
    }
    return(sum)
}

```

{{out}}

```txt

0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4
fusc numbers whose length is greater than any previous fusc number length
     fusc     index
        0         0
       11        37
      108     1,173
    1,076    35,499
   10,946   699,051

```



## C


```C

#include<limits.h>
#include<stdio.h>

int fusc(int n){
        if(n==0||n==1)
                return n;
        else if(n%2==0)
                return fusc(n/2);
        else
                return fusc((n-1)/2) + fusc((n+1)/2);
}

int numLen(int n){
        int sum = 1;

        while(n>9){
                n = n/10;
                sum++;
        }

        return sum;
}

void printLargeFuscs(int limit){
        int i,f,len,maxLen = 1;

        printf("\n\nPrinting all largest Fusc numbers upto %d \nIndex-------Value",limit);

        for(i=0;i<=limit;i++){
                f = fusc(i);
                len = numLen(f);

                if(len>maxLen){
                        maxLen = len;
                        printf("\n%5d%12d",i,f);
                }
        }
}


int main()
{
        int i;

        printf("Index-------Value");
        for(i=0;i<61;i++)
                printf("\n%5d%12d",i,fusc(i));
        printLargeFuscs(INT_MAX);
        return 0;
}

```

Prints first 61 Fusc numbers followed by the largest numbers :

```txt

Index-------Value
    0           0
    1           1
    2           1
    3           2
    4           1
    5           3
    6           2
    7           3
    8           1
    9           4
   10           3
   11           5
   12           2
   13           5
   14           3
   15           4
   16           1
   17           5
   18           4
   19           7
   20           3
   21           8
   22           5
   23           7
   24           2
   25           7
   26           5
   27           8
   28           3
   29           7
   30           4
   31           5
   32           1
   33           6
   34           5
   35           9
   36           4
   37          11
   38           7
   39          10
   40           3
   41          11
   42           8
   43          13
   44           5
   45          12
   46           7
   47           9
   48           2
   49           9
   50           7
   51          12
   52           5
   53          13
   54           8
   55          11
   56           3
   57          10
   58           7
   59          11
   60           4

Printing all largest Fusc numbers upto 2147483647
Index-------Value
   37          11
 1173         108
35499        1076
699051      10946
103682   19573419
1010747  615164587

```

## C#

```c#
using System;
using System.Collections.Generic;

static class program
{
    static int n = 61;
    static List<int> l = new List<int>() { 0, 1 };

    static int fusc(int n)
    {
        if (n < l.Count) return l[n];
        int f = (n & 1) == 0 ? l[n >> 1] : l[(n - 1) >> 1] + l[(n + 1) >> 1];
        l.Add(f); return f;
    }

    static void Main(string[] args)
    {
        bool lst = true; int w = -1, c = 0, t;
        string fs = "{0,11:n0}  {1,-9:n0}", res = "";
        Console.WriteLine("First {0} numbers in the fusc sequence:", n);
        for (int i = 0; i < int.MaxValue; i++)
        {
            int f = fusc(i); if (lst)
            {
                if (i < 61) Console.Write("{0} ", f);
                else
                {
                    lst = false;
                    Console.WriteLine();
                    Console.WriteLine("Points in the sequence where an item has more digits than any previous items:");
                    Console.WriteLine(fs, "Index\\", "/Value"); Console.WriteLine(res); res = "";
                }
            }
            if ((t = f.ToString().Length) > w)
            {
                w = t; res += (res == "" ? "" : "\n") + string.Format(fs, i, f);
                if (!lst) { Console.WriteLine(res); res = ""; } if (++c > 5) break;
            }
        }
        l.Clear();
    }
}
```

{{out}}

```txt
First 61 numbers in the fusc sequence:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4
Points in the sequence where an item has more digits than any previous items:
     Index\  /Value
          0  0
         37  11
      1,173  108
     35,499  1,076
    699,051  10,946
 19,573,419  103,682
```


=={{header|F_Sharp|F#}}==

### The Function


```fsharp

// Generate the fusc sequence. Nigel Galloway: March 20th., 2019
let fG n=seq{for (n,g) in Seq.append n [1] |> Seq.pairwise do yield n; yield n+g}
let fusc=seq{yield 0; yield! Seq.unfold(fun n->Some(n,fG n))(seq[1])|>Seq.concat}|> Seq.mapi(fun n g->(n,g))

```


### The Tasks

;Print first 62 elements

```fsharp

fusc |> Seq.take 61 |> Seq.iter(fun(_,g)->printf "%d " g); printfn ""

```

{{out}}

```txt

0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

```

;Show the fusc number (and its index) whose length is greater than any previous fusc number length
The first 6 take only 10 secs so let me be more ambitious

```fsharp

let fN=let mutable n=0 in (fun (_,g)->if g>=n then n<-pown 10 (string g).Length; true else false)
fusc |> Seq.filter fN |> Seq.take 7 |> Seq.iter(fun(n,g)->printfn "fusc %d -> %d" n g)

```

{{out}}

```txt

fusc 0 -> 0
fusc 37 -> 11
fusc 1173 -> 108
fusc 35499 -> 1076
fusc 699051 -> 10946
fusc 19573419 -> 103682
fusc 615164587 -> 1010747
Real: 00:06:03.801, CPU: 00:06:03.140, GC gen0: 21336, gen1: 0

```


## Factor


```factor
USING: arrays assocs formatting io kernel make math math.parser
math.ranges namespaces prettyprint sequences
tools.memory.private ;
IN: rosetta-code.fusc

<PRIVATE

: (fusc) ( n -- seq )
    [ 2 ] dip [a,b) [
        0 , 1 , [
            [ building get ] dip dup even?
            [ 2/ swap nth ]
            [ [ 1 - 2/ ] [ 1 + 2/ ] 2bi [ swap nth ] 2bi@ + ]
            if ,
        ] each
    ] { } make ;

: increases ( seq -- assoc )
    [ 0 ] dip [
        [
            2array 2dup first number>string length <
            [ [ 1 + ] [ , ] bi* ] [ drop ] if
        ] each-index
    ] { } make nip ;

PRIVATE>

: fusc ( n -- seq )
    dup 3 < [ { 0 1 } swap head ] [ (fusc) ] if ;

: fusc-demo ( -- )
    "First 61 fusc numbers:" print 61 fusc [ pprint bl ] each
    nl nl
    "Fusc numbers with more digits than all previous ones:"
    print "Value   Index\n
### ===  ====
" print
    1,000,000 fusc increases
   [ [ commas ] bi@ "%-6s  %-7s\n" printf ] assoc-each ;

MAIN: fusc-demo
```

{{out}}

```txt

First 61 fusc numbers:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

Fusc numbers with more digits than all previous ones:
Value   Index

### ===  ====

0       0
11      37
108     1,173
1,076   35,499
10,946  699,051

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Fusc_sequence this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC


```freebasic
' version 01-03-2019
' compile with: fbc -s console

#Define max 20000000

Dim Shared As UInteger f(max)

Sub fusc

    f(0) = 0
    f(1) = 1

    For n As UInteger = 2 To max
        If n And 1 Then
            f(n) = f((n -1) \ 2) + f((n +1) \ 2)
        Else
            f(n) = f(n \ 2)
        End If
    Next

End Sub

' ------=< MAIN >=------

Dim As UInteger i, d
Dim As String fs

fusc

For i = 0 To 60
    Print f(i); " ";
Next

Print : Print
Print "       Index       Value"
For i = 0 To max
    If f(i) >= d Then
        Print Using "###########," ; i; f(i)
        If d = 0 Then d = 1
        d *= 10
    End If
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

       Index       Value
           0           0
          37          11
       1,173         108
      35,499       1,076
     699,051      10,946
  19,573,419     103,682
```



## Go


```go
package main

import (
    "fmt"
    "strconv"
)

func fusc(n int) []int {
    if n <= 0 {
        return []int{}
    }
    if n == 1 {
        return []int{0}
    }
    res := make([]int, n)
    res[0] = 0
    res[1] = 1
    for i := 2; i < n; i++ {
        if i%2 == 0 {
            res[i] = res[i/2]
        } else {
            res[i] = res[(i-1)/2] + res[(i+1)/2]
        }
    }
    return res
}

func fuscMaxLen(n int) [][2]int {
    maxLen := -1
    maxFusc := -1
    f := fusc(n)
    var res [][2]int
    for i := 0; i < n; i++ {
        if f[i] <= maxFusc {
            continue // avoid expensive strconv operation where possible
        }
        maxFusc = f[i]
        le := len(strconv.Itoa(f[i]))
        if le > maxLen {
            res = append(res, [2]int{i, f[i]})
            maxLen = le
        }
    }
    return res
}

func commatize(n int) string {
    s := fmt.Sprintf("%d", n)
    if n < 0 {
        s = s[1:]
    }
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    if n >= 0 {
        return s
    }
    return "-" + s
}

func main() {
    fmt.Println("The first 61 fusc numbers are:")
    fmt.Println(fusc(61))
    fmt.Println("\nThe fusc numbers whose length > any previous fusc number length are:")
    res := fuscMaxLen(20000000)  // examine first twenty million numbers say
    for i := 0; i < len(res); i++ {
        fmt.Printf("%7s (index %10s)\n", commatize(res[i][1]), commatize(res[i][0]))
    }
}
```


{{out}}

```txt

The first 61 fusc numbers are:
[0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4]

The fusc numbers whose length > any previous fusc number length are:
      0 (index          0)
     11 (index         37)
    108 (index      1,173)
  1,076 (index     35,499)
 10,946 (index    699,051)
103,682 (index 19,573,419)

```


## Haskell


```haskell
fusc :: Int -> Int
fusc i
  | 1 > i = 0
  | otherwise = fst $ go (pred i)
  where
    go n
      | 0 == n = (1, 0)
      | even n = (x + y, y)
      | otherwise = (x, x + y)
      where
        (x, y) = go (div n 2)

widths :: [(Int, Int)]
widths = (\(_, i, x) -> (i, x)) <$> iterate nxtWidth (2, 0, 0)

nxtWidth :: (Int, Int, Int) -> (Int, Int, Int)
nxtWidth (w, i, v) =
  let fi = (,) <*> fusc
      (j, x) = until ((w <=) . length . show . snd) (fi . succ . fst) (fi i)
  in (succ w, j, x)

main :: IO ()
main = do
  putStrLn "First 61 terms:"
  print $ fusc <$> [0 .. 60]
  putStrLn "\n(Index, Value):"
  mapM_ print $ take 5 widths
```

{{Out}}

```txt
First 61 terms:
[0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1,6,5,9,4,11,7,10,3,11,8,13,5,12,7,9,2,9,7,12,5,13,8,11,3,10,7,11,4]

(Index, Value):
(0,0)
(37,11)
(1173,108)
(35499,1076)
(699051,10946)
```



## J


```J

fusc_term =: ({~ -:@#)`([: +/ ({~ ([: -: _1 1 + #)))@.(2 | #)
fusc =: (, fusc_term)@:]^:[ 0 1"_

   NB. show the first 61 fusc numbers (starting at zero) in a horizontal format.
   61 {. fusc 70
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

   9!:17]2 2 NB. specify bottom right position in box

   FUSC =: fusc 99999
   DIGITS =: ; ([: # 10&#.inv)&.> FUSC

   (;: 'index value') ,. <"0(,: {&A) DIGITS i. 1 2 3 4
┌─────┬─┬──┬────┬─────┐
│index│0│37│1173│35499│
├─────┼─┼──┼────┼─────┤
│value│0│11│ 108│ 1076│
└─────┴─┴──┴────┴─────┘


```



## Javascript


### Functional

{{Trans|Python}}


A composition of pure generic functions:

```javascript
(() => {
    'use strict';

    const main = () => {

        // fusc :: Int -> Int
        const fusc = i => {
            const go = n =>
                0 === n ? (
                    [1, 0]
                ) : (() => {
                    const [x, y] = go(quot(n, 2));
                    return even(n) ? (
                        [x + y, y]
                    ) : [x, x + y];
                })();
            return 1 > i ? (
                0
            ) : fst(go(i - 1));
        };


        // firstWidths :: Int -> [(Int, Int)]
        const firstWidths = n => {
            const nxtWidth = xs => {
                const
                    fi = fanArrow(fusc, id),
                    [w, i, v] = head(xs),
                    [x, j] = Array.from(until(
                        v => w <= fst(v).toString().length,
                        v => fi(succ(snd(v))),
                        fi(i)
                    ));
                return cons(
                    [succ(w), j, x],
                    xs
                );
            };
            return until(
                x => n < fst(fst(x)),
                nxtWidth,
                [[2, 0, 0]]
            );
        };

        return unlines([
            'First 61 terms:',
            '[' + map(fusc, enumFromTo(0, 60)).join(',') + ']',
            '',
            '(Index, Value):',
            unlines(map(
                ([i, x]) => '(' + i + ', ' + x + ')',
                foldl(
                    (a, x) => cons(tail(x), a),
                    [],
                    firstWidths(5)
                )
            ))
        ]);
    };

    // GENERIC FUNCTIONS ----------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) =>
        Array.isArray(xs) ? (
            [x].concat(xs)
        ) : 'GeneratorFunction' !== xs.constructor.constructor.name ? (
            x + xs
        ) : ( // Existing generator wrapped with one additional element
            function*() {
                yield x;
                let nxt = xs.next()
                while (!nxt.done) {
                    yield nxt.value;
                    nxt = xs.next();
                }
            }
        )();

    // enumFromTo :: Enum a => a -> a -> [a]
    const enumFromTo = (m, n) => {
        const [x, y] = [m, n].map(fromEnum),
            b = x + ('number' !== typeof m ? 0 : m - x);
        return Array.from({
            length: 1 + (y - x)
        }, (_, i) => toEnum(m)(b + i));
    };

    // even :: Int -> Bool
    const even = n => 0 === n % 2;

    // Compose a function from a simple value to a tuple of
    // the separate outputs of two different functions

    // fanArrow (&&&) :: (a -> b) -> (a -> c) -> (a -> (b, c))
    const fanArrow = (f, g) => x => Tuple(f(x), g(x));

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // fromEnum :: Enum a => a -> Int
    const fromEnum = x =>
        typeof x !== 'string' ? (
            x.constructor === Object ? (
                x.value
            ) : parseInt(Number(x))
        ) : x.codePointAt(0);

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // id :: a -> a
    const id = x => x;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // succ :: Enum a => a -> a
    const succ = x => {
        const t = typeof x;
        return 'number' !== t ? (() => {
            const [i, mx] = [x, maxBound(x)].map(fromEnum);
            return i < mx ? (
                toEnum(x)(1 + i)
            ) : Error('succ :: enum out of range.')
        })() : x < Number.MAX_SAFE_INTEGER ? (
            1 + x
        ) : Error('succ :: Num out of range.')
    };

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // The first argument is a sample of the type
    // allowing the function to make the right mapping

    // toEnum :: a -> Int -> a
    const toEnum = e => x => {
        const
            m = e.enum,
            f = {
                'number': Number,
                'string': String.fromCodePoint,
                'boolean': Boolean
            } [typeof e];
        return f ? (
            f(x)
        ) : m[m[x]];
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

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
First 61 terms:
[0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1,6,5,9,4,11,7,10,3,11,8,13,5,12,7,9,2,9,7,12,5,13,8,11,3,10,7,11,4]

(Index, Value):
(0, 0)
(37, 11)
(1173, 108)
(35499, 1076)
(699051, 10946)
```



## Julia


```julia
using Memoize, Formatting

@memoize function sternbrocot(n)
    if n < 2
        return n
    elseif iseven(n)
        return sternbrocot(div(n, 2))
    else
        m = div(n - 1, 2)
        return sternbrocot(m) + sternbrocot(m + 1)
    end
end

function fusclengths(N=100000000)
    println("sequence number : fusc value")
    maxlen = 0
    for i in 0:N
        x = sternbrocot(i)
        if (len = length(string(x))) > maxlen
            println(lpad(format(i, commas=true), 15), " : ", format(x, commas=true))
            maxlen = len
        end
    end
end

println("The first 61 fusc numbers are: ", [sternbrocot(x) for x in 0:60])
fusclengths()

```
{{out}}

```txt

The first 61 fusc numbers are: [0, 1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1, 5, 4, 7, 3, 8, 5, 7, 2, 7, 5, 8, 3, 7, 4, 5, 1, 6,
 5, 9, 4, 11, 7, 10, 3, 11, 8, 13, 5, 12, 7, 9, 2, 9, 7, 12, 5, 13, 8, 11, 3, 10, 7, 11, 4]
sequence number : fusc value
              0 : 0
             37 : 11
          1,173 : 108
         35,499 : 1,076
        699,051 : 10,946
     19,573,419 : 103,682

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.3.21

fun fusc(n: Int): IntArray {
    if (n <= 0) return intArrayOf()
    if (n == 1) return intArrayOf(0)
    val res = IntArray(n)
    res[1] = 1
    for (i in 2 until n) {
        if (i % 2 == 0) {
            res[i] = res[i / 2]
        } else {
            res[i] = res[(i - 1) / 2] + res[(i + 1) / 2]
        }
    }
    return res
}

fun fuscMaxLen(n: Int): List<Pair<Int, Int>> {
    var maxLen = -1
    var maxFusc = -1
    val f = fusc(n)
    val res = mutableListOf<Pair<Int, Int>>()
    for (i in 0 until n) {
        if (f[i] <= maxFusc) continue // avoid string conversion
        maxFusc = f[i]
        val len = f[i].toString().length
        if (len > maxLen) {
            res.add(Pair(i, f[i]))
            maxLen = len
        }
    }
    return res
}

fun main() {
    println("The first 61 fusc numbers are:")
    println(fusc(61).asList())
    println("\nThe fusc numbers whose length > any previous fusc number length are:")
    val res = fuscMaxLen(20_000_000)  // examine first 20 million numbers say
    for (r in res) {
        System.out.printf("%,7d (index %,10d)\n", r.second, r.first)
    }
}
```


{{output}}

```txt

The first 61 fusc numbers are:
[0, 1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1, 5, 4, 7, 3, 8, 5, 7, 2, 7, 5, 8, 3, 7, 4, 5, 1, 6, 5, 9, 4, 11, 7, 10, 3, 11, 8, 13, 5, 12, 7, 9, 2, 9, 7, 12, 5, 13, 8, 11, 3, 10, 7, 11, 4]

The fusc numbers whose length > any previous fusc number length are:
      0 (index          0)
     11 (index         37)
    108 (index      1,173)
  1,076 (index     35,499)
 10,946 (index    699,051)
103,682 (index 19,573,419)

```



## Pascal

{{works with|Free Pascal}}
Using dynamic array.To speed things up using Pointer.
Found the indices of a specific base to oszillating.Tried power of phi with more success 11 ~ phi^5

```pascal
program fusc;
uses
  sysutils;
const
  MaxIdx =1253*1000*1000;//19573420; // must be even
type
  tFuscElem = LongWord;
  tFusc = array of tFuscElem;
var
  FuscField : tFusc;

function commatize(n:NativeUint):string;
var
  l,i : NativeUint;
begin
  str(n,result);
  l := length(result);
  //no commatize
  if l < 4 then
    exit;
  //new length
  i := l+ (l-1) DIV 3;
  setlength(result,i);
  //copy chars to the right place
  While i <> l do
  Begin
    result[i]:= result[l];result[i-1]:= result[l-1];
    result[i-2]:= result[l-2];result[i-3]:= ',';
    dec(i,4);dec(l,3);
  end;
end;

procedure OutFusc(StartIdx,EndIdx :NativeInt;const FF:tFusc);
Begin
  IF StartIdx < Low(FF) then StartIdx :=Low(FF);
  IF EndIdx > High(FF) then EndIdx := High(FF);
  For StartIdx := StartIdx to EndIdx do
    write(FF[StartIdx],' ');
  writeln;
end;

procedure FuscCalc(var FF:tFusc);
var
  pFFn,pFFi : ^tFuscElem;
  i,n,sum : NativeUint;
Begin
  FF[0]:= 0;
  FF[1]:= 1;
  n := 2;
  i := 1;
  pFFn := @FF[n];
  pFFi := @FF[i];
  sum := pFFi^;
  while n <= MaxIdx-2 do
  begin
    //even
    pFFn^ := sum;//FF[n] := FF[i];
    //odd
    inc(pFFi);//FF[i+1]
    inc(pFFn);//FF[n+1]
    sum := sum+pFFi^;
    pFFn^:= sum; //FF[n+1] := FF[i]+FF[i+1];
    sum := pFFi^;
    inc(pFFn);
    inc(n,2);
    //inc(i);
  end;
end;

procedure OutHeader(base:NativeInt);
begin
  writeln('Fusc numbers with more digits in base ',base,' than all previous ones:');
  writeln('Value':10,'Index':10,'  IndexNum/IndexNumBefore');
  writeln('======':10,'
### =
':14);
end;

procedure CheckFuscDigits(const FF:tFusc;Base:NativeUint);
var
  pFF : ^tFuscElem;
  Dig,
  i,lastIdx: NativeInt;
Begin
  OutHeader(base);
  Dig := -1;
  i := 0;
  lastIdx := 0;
  pFF := @FF[0];// aka FF[i]
  repeat
    //search in tight loop speeds up
    repeat
      inc(pFF);
      inc(i);
    until pFF^ >Dig;

    if i>= MaxIdx then
      BREAK;
    //output
    write(commatize(pFF^):10,commatize(i):14);//,DIG:10);
    IF lastIdx> 0 then
      write(i/lastIdx:12:7);
    writeln;
    lastIdx := i;
    IF Dig >0 then
      Dig := Dig*Base+Base-1
    else
     Dig := Base-1;
  until false;
  writeln;
end;

BEGIN
  setlength(FuscField,MaxIdx);
  FuscCalc(FuscField);
  writeln('First 61 fusc numbers:');
  OutFusc(0,60,FuscField);

  CheckFuscDigits(FuscField,10);
  CheckFuscDigits(FuscField,11); //11 ~phi^5  1.6180..^5 = 11,09
  setlength(FuscField,0);
  {$IFDEF WIN}readln;{$ENDIF}
END.
```

{{Out}}

```txt
First 61 fusc numbers:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4
Fusc numbers with more digits in base 10 than all previous ones:
     Value     Index  IndexNum/IndexNumBefore

### ===       ====

         1             1
        11            37  37.0000000
       108         1,173  31.7027027
     1,076        35,499  30.2634271
    10,946       699,051  19.6921322
   103,682    19,573,419  27.9999871
 1,010,747   615,164,587  31.4285709

Fusc numbers with more digits in base 11 than all previous ones:
     Value     Index  IndexNum/IndexNumBefore

### ===       ====

         1             1
        11            37  37.0000000
       123         1,195  32.2972973
     1,364        38,229  31.9907950
    15,127     1,223,339  32.0002877
   167,761    39,146,837  31.9999910
 1,860,498 1,252,698,795  32.0000003

real  0m1,968s  user  0m1,594s  sys 0m0,373s
```



## Perl

Borrowing from the [http://rosettacode.org/wiki/Stern-Brocot_sequence Stern-Brocot sequence] task.

```perl
use strict;
use warnings;
use feature 'say';

sub comma { reverse ((reverse shift) =~ s/(.{3})/$1,/gr) =~ s/^,//r }

sub stern_diatomic {
  my ($p,$q,$i) = (0,1,shift);
  while ($i) {
    if ($i & 1) { $p += $q; } else { $q += $p; }
    $i >>= 1;
  }
  $p;
}

say "First 61 terms of the Stern-Brocot sequence:\n" . join ' ', map { stern_diatomic($_) } 0..60;
say "\nIndex and value for first term longer than any previous:";

my $i =  0;
my $l = -1;
while ($l < 5) {
    my $v = stern_diatomic($i);
    printf("%15s : %s\n", comma($i), comma($v)) and $l = length $v if length $v > $l;
    $i++;
}
```

{{out}}

```txt
First 61 terms of the Stern-Brocot sequence:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

Index and value for first term longer than any previous:
              0 : 0
             37 : 11
          1,173 : 108
         35,499 : 1,076
        699,051 : 10,946
```


## Perl 6

{{works with|Rakudo|2018.12}}


```perl6
my @Stern-Brocot;
@Stern-Brocot = 0, 1, 1, { |(@Stern-Brocot[$_ - 1] + @Stern-Brocot[$_], @Stern-Brocot[$_]) given ++$+1 } ... *;

sub comma { $^i.flip.comb(3).join(',').flip }

put "First 61 terms of the Stern-Brocot sequence:\n{@Stern-Brocot[^61].gist}" ~
    "\n\nIndex and value for first term longer than any previous:";

for flat 'Index', 'Value', 0, 0, (1..4).map({
    my $l = 10**$_;
    @Stern-Brocot.first(* > $l, :kv).map: *.&comma
  }) -> $i, $v {
      printf "%15s : %s\n", $i, $v
}
```

{{out}}

```txt
First 61 terms of the Stern-Brocot sequence:
(0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4)

Index and value for first term longer than any previous:
          Index : Value
              0 : 0
             37 : 11
          1,173 : 108
         35,499 : 1,076
        699,051 : 10,946

```



## Phix

Note that phix is 1-indexed. While there are no commas in the first 61 entries, it felt more
in line with the task requirements to forego the standard comma-separated %v output.

```Phix
constant limit = 20_000_000
sequence fuscs = repeat(0,limit); -- NB 1-based indexing; fusc(0)===fuscs[1]
fuscs[2] = 1                                        -- ie fusc(1):=1
for n=3 to limit do
  fuscs[n] = iff(remainder(n-1,2)?fuscs[n/2]+fuscs[n/2+1]:fuscs[(n+1)/2])
end for
--printf(1,"First 61 terms of the Fusc sequence:\n%v\n",{fuscs[1..61]})
string s = ""
for n=1 to 61 do s&=sprintf("%,d ",fuscs[n]) end for
printf(1,"First 61 terms of the Fusc sequence:\n%s\n\n",{s})
printf(1,"Elements with more digits than any previous items:\n")
printf(1,"          Index : Value\n")
integer d = 0
for n=1 to length(fuscs) do
  if fuscs[n]>=d then
    printf(1,"%,15d : %,d\n",{n-1,fuscs[n]})
    d = iff(d=0?10:d*10)
  end if
end for
```

{{out}}

```txt

First 61 terms of the Fusc sequence:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

Elements with more digits than any previous items:
          Index : Value
              0 : 0
             37 : 11
          1,173 : 108
         35,499 : 1,076
        699,051 : 10,946
     19,573,419 : 103,682

```



## Python

By composition of pure functions, for better reliability, ease and speed of refactoring, and for higher levels of code reuse,

with type comments for the reader (not for the compiler).


```python
"""Fusc sequence and tests"""


# fusc :: Int -> Int
def fusc(i):
    '''Fusc sequence'''
    def go(n):
        if 0 == n:
            return (1, 0)
        else:
            x, y = go(n // 2)
            return (x + y, y) if 0 == n % 2 else (
                x, x + y
            )
    return 0 if 1 > i else (
        go(i - 1)[0]
    )


# main :: IO()
def main():
    '''Tests'''

    print('First 61 terms:')
    print(
        list(map(fusc, range(0, 61)))
    )
    print('\n(Index, Value):')

    # Up to five digits
    for tpl in firstWidths(5):
        print(tpl)


# firstWidths :: Int -> [(Int, Int)]
def firstWidths(n):
    '''First terms to have particular widths (digit counts) up to n'''

    # nxtFusc :: (Int, Int) -> (Int, Int)
    def nxtFusc(tpl):
        i = 1 + tpl[1]
        return (fusc(i), i)

    # nxtWidth :: [(Int, Int, Int)] -> [(Int, Int, Int)]
    def nxtWidth(xs):
        '''(width, index, value)'''
        w, i, _ = xs[0]

        def p(tpl):
            return w <= len(str(tpl[0]))
        x, j = until(p)(nxtFusc)(
            (fusc(i), i)
        )
        return [(1 + w, j, x)] + xs

    # go :: Int -> [(Int, Int, Int)]
    def go(n):
        def p(xs):
            return n <= len(xs)
        return until(p)(nxtWidth)(
            [(2, 0, 0)]
        )
    return list(map(tail, reversed(go(n))))


# GENERIC -------------------------------------------------

# tail :: [a] -> [a]
def tail(xs):
    '''The elements following the head of a (non-empty) list.'''
    return xs[1:]


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# TEST ----------------------------------------------------
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
First 61 terms:
[0, 1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1, 5, 4, 7, 3, 8, 5, 7, 2, 7, 5, 8, 3, 7, 4, 5, 1, 6, 5, 9, 4, 11, 7, 10, 3, 11, 8, 13, 5, 12, 7, 9, 2, 9, 7, 12, 5, 13, 8, 11, 3, 10, 7, 11, 4]

(Index, Value):
(0, 0)
(37, 11)
(1173, 108)
(35499, 1076)
(699051, 10946)
```



## Racket



```racket
#lang racket

(require racket/generator)

(define (memoize f)
  (define table (make-hash))
  (λ args (hash-ref! table args (thunk (apply f args)))))

(define fusc
  (memoize
   (λ (n)
     (cond
       [(<= n 1) n]
       [(even? n) (fusc (/ n 2))]
       [else (+ (fusc (/ (sub1 n) 2)) (fusc (/ (add1 n) 2)))]))))

(define (comma x)
  (string-join
   (reverse
    (for/list ([digit (in-list (reverse (string->list (~a x))))] [i (in-naturals)])
      (cond
        [(and (= 0 (modulo i 3)) (> i 0)) (string digit #\,)]
        [else (string digit)])))
   ""))

;; Task 1
(displayln (string-join (for/list ([i (in-range 61)]) (comma (fusc i))) " "))
(newline)

;; Task 2
(define gen
  (in-generator
   (let loop ([prev 0] [i 0])
     (define result (fusc i))
     (define len (string-length (~a result)))
     (cond
       [(> len prev)
        (yield (list i result))
        (loop len (add1 i))]
       [else (loop prev (add1 i))]))))

(for ([i (in-range 5)] [x gen])
  (match-define (list index result) x)
  (printf "~a: ~a\n" (comma index) (comma result)))
```


{{out}}

```txt

0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

0: 0
37: 11
1,173: 108
35,499: 1,076
699,051: 10,946

```



## REXX


```rexx
/*REXX program  calculates and displays the   fusc   (or  Stern's Diatomic)   sequence. */
parse arg LO HI xw .                             /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then LO=  0                 /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI= 61                 /* "      "         "   "   "     "    */
if xw=='' | xw==","  then xw=  0                 /* "      "         "   "   "     "    */
list= xw<1                                       /*boolean value:  LIST  to show numbers*/
@.=;        @.0= 0;       @.1= 1                 /*assign array default; assign low vals*/
mL= 0                                            /*the maximum length (digits)  so far. */
$=                                               /* "  list of  fusc  numbers    "  "   */
   do j=0  for HI+1                              /*process a bunch of integers from zero*/
   if j>1  then if j//2  then do;  _= (j-1) % 2;   p= (j+1) % 2;   @.j= @._ + @.p;   end
                         else do;  _= j % 2;                       @.j= @._;         end
   if list  then if j>=LO  then $= $ commas(@.j)                      /*add it to a list*/
                           else nop
            else do;   if length(@.j)<=mL  then iterate               /*still too small.*/
                       mL= length(@.j)                                /*found increase. */
                       if mL==1  then say '═══index═══   ═══fusc number═══'
                       say right( commas(j), 9)     right( commas(@.j), 14)
                       if mL==xw  then leave     /*Found max length?  Then stop looking.*/
                 end                             /* [↑]  display fusc #s of maximum len.*/
   end   /*j*/

if $\==''  then say strip($)                     /*display a horizontal list of fusc #s.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas:  parse arg _;  do c=length(_)-3  to 1  by -3; _=insert(',', _, c); end;   return _
```

{{out|output|text=  when using the default inputs:}}

```txt

0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4 9

```

{{out|output|text=  when using the inputs of:     <tt> 0   999999999   5 </tt>}}

```txt

═══index═══   ═══fusc number═══
        0              0
       37             11
    1,173            108
   35,499          1,076
  699,051         10,946

```



## Ring


```ring

# Project: Fusc sequence

max = 60
fusc = list(36000)
fusc[1] = 1
see "working..." + nl
see "wait for done..." + nl
see "The first 61 fusc numbers are:" + nl
fuscseq(max)
see "0"
for m = 1 to max
    see " " + fusc[m]
next

see nl
see "The fusc numbers whose length > any previous fusc number length are:" + nl
see "Index Value" + nl
see " 0     0" + nl
d = 10
for i = 1 to 36000
    if fusc[i] >= d
        see " " + i + "   " + fusc[i] + nl
        if d = 0
           d = 1
        ok
        d = d*10
    ok
next
see "done..." + nl

func fuscseq(max)
     for n = 2 to 36000
         if n%2 = 1
            fusc[n] = fusc[(n-1)/2] + fusc[(n+1)/2]
         but n%2 = 0
             fusc[n] = fusc[n/2]
         ok
     next

```

{{out}}

```txt

working...
wait for done...
The first 61 fusc numbers are:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4
The fusc numbers whose length > any previous fusc number length are:
Index Value
 0     0
 37    11
 1173  108
 35499 1076
done...

```



## Ruby

Using two Enumerators; the second making use of the first:

```ruby
fusc = Enumerator.new do |y|
  y << 0
  y << 1
  arr = [0,1]
  2.step do |n|
    res = n.even? ? arr[n/2] : arr[(n-1)/2] + arr[(n+1)/2]
    y   << res
    arr << res
  end
end

fusc_max_digits = Enumerator.new do |y|
   cur_max, cur_exp = 0, 0
   0.step do |i|
      f = fusc.next
      if f >= cur_max
        cur_exp += 1
        cur_max = 10**cur_exp
        y << [i, f]
      end
   end
end

puts fusc.take(61).join(" ")
fusc_max_digits.take(6).each{|pair| puts "%15s : %s" % pair }

```

{{out}}

```txt
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4
              0 : 0
             11 : 37
            108 : 1173
           1076 : 35499
          10946 : 699051
         103682 : 19573419

```



## Sidef


```ruby
func fusc(n) is cached {

    return 0 if n.is_zero
    return 1 if n.is_one

    n.is_even ? fusc(n/2) : (fusc((n-1)/2) + fusc(((n-1)/2)+1))
}

say ("First 61 terms of the Stern-Brocot sequence: ", 61.of(fusc).join(' '))

say "\nIndex and value for first term longer than any previous:"
printf("%15s : %s\n", "Index", "Value");

var (index=0, len=0)

5.times {
    index = (index..Inf -> first_by { fusc(_).len > len })
    len = fusc(index).len
    printf("%15s : %s\n", index.commify, fusc(index).commify)
}
```

{{out}}

```txt

First 61 terms of the Stern-Brocot sequence: 0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

Index and value for first term longer than any previous:
          Index : Value
              0 : 0
             37 : 11
          1,173 : 108
         35,499 : 1,076
        699,051 : 10,946

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Dim n As Integer = 61, l As List(Of Integer) = {0, 1}.ToList

    Function fusc(n As Integer) As Integer
        If n < l.Count Then Return l(n)
        fusc = If((n And 1) = 0, l(n >> 1), l((n - 1) >> 1) + l((n + 1) >> 1))
        l.Add(fusc)
    End Function

    Sub Main(args As String())
        Dim lst As Boolean = True, w As Integer = -1, c As Integer = 0,
            fs As String = "{0,11:n0}  {1,-9:n0}", res As String = ""
        Console.WriteLine("First {0} numbers in the fusc sequence:", n)
        For i As Integer = 0 To Integer.MaxValue
            Dim f As Integer = fusc(i)
            If lst Then
                If i < 61 Then
                    Console.Write("{0} ", f)
                Else
                    lst = False
                    Console.WriteLine()
                    Console.WriteLine("Points in the sequence where an item has more digits than any previous items:")
                    Console.WriteLine(fs, "Index\", "/Value") : Console.WriteLine(res) : res = ""
                End If
            End If
            Dim t As Integer = f.ToString.Length
            If t > w Then
                w = t
                res &= If(res = "", "", vbLf) & String.Format(fs, i, f)
                If Not lst Then Console.WriteLine(res) : res = ""
                c += 1 : If c > 5 Then Exit For
            End If
        Next : l.Clear()
    End Sub
End Module

```

{{out}}

```txt
First 61 numbers in the fusc sequence:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4
Points in the sequence where an item has more digits than any previous items:
     Index\  /Value
          0  0
         37  11
      1,173  108
     35,499  1,076
    699,051  10,946
 19,573,419  103,682

```


## zkl


```zkl
fuscs:=List.createLong(1_000_000, 0); fuscs[1]=1; // we'll just use a big count
foreach n in ([2..fuscs.len()-1]){		 // and generate
   fuscs[n]=( if(n.isEven()) fuscs[n/2] else fuscs[(n-1)/2] + fuscs[(n+1)/2] )
}

println("First 61 terms of the Stern-Brocot sequence:");
fuscs[0,61].concat(" ").println();

println("\nIndex and value for first term longer than any previous:");
println("          Index : Value");
prevMax:=-1;
foreach n in (fuscs.len()){
   f,fd := fuscs[n], f.numDigits;
   if(fd>prevMax){ println("%15,d : %,d".fmt(n,f)); prevMax=fd }
}
```

{{out}}

```txt

First 61 terms of the Stern-Brocot sequence:
0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4 5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9 7 12 5 13 8 11 3 10 7 11 4

Index and value for first term longer than any previous:
          Index : Value
              0 : 0
             37 : 11
          1,173 : 108
         35,499 : 1,076
        699,051 : 10,946

```

