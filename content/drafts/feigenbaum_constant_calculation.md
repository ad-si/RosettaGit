+++
title = "Feigenbaum constant calculation"
description = ""
date = 2019-09-15T17:22:03Z
aliases = []
[extra]
id = 21786
[taxonomies]
categories = []
tags = []
+++

{{draft task}}


;Task:
Calculate the Feigenbaum constant.


;See:
:*   Details in the Wikipedia article:   [https://en.wikipedia.org/wiki/Feigenbaum_constants Feigenbaum constant].





## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
{{Trans|Ring}}

```algol68
# Calculate the Feigenbaum constant #

print( ( "Feigenbaum constant calculation:", newline ) );
INT max it   = 13;
INT max it j = 10;
REAL a1 := 1.0;
REAL a2 := 0.0;
REAL d1 := 3.2;
print( ( "i  ", "d", newline ) );
FOR i FROM 2 TO max it DO
     REAL a := a1 + (a1 - a2) / d1;
     FOR j TO max it j DO
          REAL x := 0;
          REAL y := 0;
          FOR k TO 2 ^ i DO
               y := 1 - 2 * y * x;
               x := a - x * x
          OD;
          a := a - x / y
     OD;
     REAL d = (a1 - a2) / (a - a1);
     IF i < 10 THEN
        print( ( whole( i, 0 ), "  ", fixed( d, -10, 8 ), newline ) )
     ELSE
        print( ( whole( i, 0 ), " ",  fixed( d, -10, 8 ), newline ) )
     FI;
     d1 := d;
     a2 := a1;
     a1 := a
OD
```

{{out}}

```txt

Feigenbaum constant calculation:
i  d
2  3.21851142
3  4.38567760
4  4.60094928
5  4.65513050
6  4.66611195
7  4.66854858
8  4.66906066
9  4.66917155
10 4.66919515
11 4.66920026
12 4.66920098
13 4.66920537

```



## AWK


```AWK

# syntax: GAWK -f FEIGENBAUM_CONSTANT_CALCULATION.AWK
BEGIN {
    a1 = 1
    a2 = 0
    d1 = 3.2
    max_i = 13
    max_j = 10
    print(" i d")
    for (i=2; i<=max_i; i++) {
      a = a1 + (a1 - a2) / d1
      for (j=1; j<=max_j; j++) {
        x = y = 0
        for (k=1; k<=2^i; k++) {
          y = 1 - 2 * y * x
          x = a - x * x
        }
        a -= x / y
      }
      d = (a1 - a2) / (a - a1)
      printf("%2d %.8f\n",i,d)
      d1 = d
      a2 = a1
      a1 = a
    }
    exit(0)
}

```

{{out}}

```txt

 i d
 2 3.21851142
 3 4.38567760
 4 4.60094928
 5 4.65513050
 6 4.66611195
 7 4.66854858
 8 4.66906066
 9 4.66917155
10 4.66919515
11 4.66920026
12 4.66920098
13 4.66920537

```



## C

{{trans|Ring}}

```c
#include <stdio.h>

void feigenbaum() {
    int i, j, k, max_it = 13, max_it_j = 10;
    double a, x, y, d, a1 = 1.0, a2 = 0.0, d1 = 3.2;
    printf(" i       d\n");
    for (i = 2; i <= max_it; ++i) {
        a = a1 + (a1 - a2) / d1;
        for (j = 1; j <= max_it_j; ++j) {
            x = 0.0;
            y = 0.0;
            for (k = 1; k <= 1 << i; ++k) {
                 y = 1.0 - 2.0 * y * x;
                 x = a - x * x;
            }
            a -= x / y;
        }
        d = (a1 - a2) / (a - a1);
        printf("%2d    %.8f\n", i, d);
        d1 = d;
        a2 = a1;
        a1 = a;
    }
}

int main() {
    feigenbaum();
    return 0;
}
```


{{output}}

```txt

 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537

```



## C++

{{trans|C}}

```cpp
#include <iostream>

int main() {
    const int max_it = 13;
    const int max_it_j = 10;
    double a1 = 1.0, a2 = 0.0, d1 = 3.2;

    std::cout << " i       d\n";
    for (int i = 2; i <= max_it; ++i) {
        double a = a1 + (a1 - a2) / d1;
        for (int j = 1; j <= max_it_j; ++j) {
            double x = 0.0;
            double y = 0.0;
            for (int k = 1; k <= 1 << i; ++k) {
                y = 1.0 - 2.0*y*x;
                x = a - x * x;
            }
            a -= x / y;
        }
        double d = (a1 - a2) / (a - a1);
        printf("%2d    %.8f\n", i, d);
        d1 = d;
        a2 = a1;
        a1 = a;
    }

    return 0;
}
```

{{out}}

```txt
 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537
```


## C#
{{trans|Kotlin}}

```c#
using System;

namespace FeigenbaumConstant {
    class Program {
        static void Main(string[] args) {
            var maxIt = 13;
            var maxItJ = 10;
            var a1 = 1.0;
            var a2 = 0.0;
            var d1 = 3.2;
            Console.WriteLine(" i       d");
            for (int i = 2; i <= maxIt; i++) {
                var a = a1 + (a1 - a2) / d1;
                for (int j = 1; j <= maxItJ; j++) {
                    var x = 0.0;
                    var y = 0.0;
                    for (int k = 1; k <= 1<<i; k++) {
                        y = 1.0 - 2.0 * y * x;
                        x = a - x * x;
                    }
                    a -= x / y;
                }
                var d = (a1 - a2) / (a - a1);
                Console.WriteLine("{0,2:d}    {1:f8}", i, d);
                d1 = d;
                a2 = a1;
                a1 = a;
            }
        }
    }
}
```

{{out}}

```txt
 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537
```



## D


```d
import std.stdio;

void main() {
    int max_it = 13;
    int max_it_j = 10;
    double a1 = 1.0;
    double a2 = 0.0;
    double d1 = 3.2;
    double a;

    writeln(" i       d");
    for (int i=2; i<=max_it; i++) {
        a = a1 + (a1 - a2) / d1;
        for (int j=1; j<=max_it_j; j++) {
            double x = 0.0;
            double y = 0.0;
            for (int k=1; k <= 1<<i; k++) {
                y = 1.0 - 2.0 * y * x;
                x = a - x * x;
            }
            a -= x / y;
        }
        double d = (a1 - a2) / (a - a1);
        writefln("%2d    %.8f", i, d);
        d1 = d;
        a2 = a1;
        a1 = a;
    }
}
```

{{out}}

```txt
 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920028
12    4.66920099
13    4.66920555
```


=={{header|F#|F sharp}}==
{{trans|C#}}

```fsharp
open System

[<EntryPoint>]
let main _ =
    let maxIt = 13
    let maxItJ = 10
    let mutable a1 = 1.0
    let mutable a2 = 0.0
    let mutable d1 = 3.2
    Console.WriteLine(" i       d")
    for i in 2 .. maxIt do
        let mutable a = a1 + (a1 - a2) / d1
        for j in 1 .. maxItJ do
            let mutable x = 0.0
            let mutable y = 0.0
            for _ in 1 .. (1 <<< i) do
                y <- 1.0 - 2.0 * y * x
                x <- a - x * x
            a <- a - x / y
        let d = (a1 - a2) / (a - a1)
        Console.WriteLine("{0,2:d}    {1:f8}", i, d)
        d1 <- d
        a2 <- a1
        a1 <- a
    0 // return an integer exit code
```

{{out}}

```txt
 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537
```



## Fortran


```fortran
      program feigenbaum
      implicit none

      integer i, j, k
      real ( KIND = 16 ) x, y, a, b, a1, a2, d1

      print '(a4,a13)', 'i', 'd'

      a1 = 1.0;
      a2 = 0.0;
      d1 = 3.2;

      do i=2,20
         a = a1 + (a1 - a2) / d1;
         do j=1,10
            x = 0
            y = 0
            do k=1,2**i
                y = 1 - 2 * y * x;
                x = a - x**2;
            end do
            a = a - x / y;
         end do

         d1 = (a1 - a2) / (a - a1);
         a2 = a1;
         a1 = a;
         print '(i4,f13.10)', i, d1
     end do
     end
```

{{out}}

```txt
   i            d
   2 3.2185114220
   3 4.3856775986
   4 4.6009492765
   5 4.6551304954
   6 4.6661119478
   7 4.6685485814
   8 4.6690606606
   9 4.6691715554
  10 4.6691951560
  11 4.6692002291
  12 4.6692013133
  13 4.6692015458
  14 4.6692015955
  15 4.6692016062
  16 4.6692016085
  17 4.6692016090
  18 4.6692016091
  19 4.6692016091
  20 4.6692016091
```



## FreeBASIC


```freebasic
' version 25-0-2019
' compile with: fbc -s console

Dim As UInteger i, j, k, maxit = 13, maxitj = 13
Dim As Double x, y, a, a1 = 1, a2, d, d1 = 3.2

Print "Feigenbaum constant calculation:"
Print
Print "  i     d"
Print "
### =============
"

For i = 2 To maxIt
    a = a1 + (a1 - a2) / d1
    For j = 1 To maxItJ
        x = 0 : y = 0
        For k = 1 To 2 ^ i
            y = 1 - 2 * y * x
            x = a - x * x
        Next
        a = a - x / y
    Next
    d = (a1 - a2) / (a - a1)
    Print Using "###    ##.#########"; i; d
    d1 = d
    a2 = a1
    a1 = a
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Feigenbaum constant calculation:

  i     d

### =============

  2     3.218511422
  3     4.385677599
  4     4.600949277
  5     4.655130495
  6     4.666111948
  7     4.668548581
  8     4.669060660
  9     4.669171555
 10     4.669195148
 11     4.669200285
 12     4.669201301
 13     4.669198656
```



## Go

{{trans|Ring}}

```go
package main

import "fmt"

func feigenbaum() {
    maxIt, maxItJ := 13, 10
    a1, a2, d1 := 1.0, 0.0, 3.2
    fmt.Println(" i       d")
    for i := 2; i <= maxIt; i++ {
        a := a1 + (a1-a2)/d1
        for j := 1; j <= maxItJ; j++ {
            x, y := 0.0, 0.0
            for k := 1; k <= 1<<uint(i); k++ {
                y = 1.0 - 2.0*y*x
                x = a - x*x
            }
            a -= x / y
        }
        d := (a1 - a2) / (a - a1)
        fmt.Printf("%2d    %.8f\n", i, d)
        d1, a2, a1 = d, a1, a
    }
}

func main() {
    feigenbaum()
}
```


{{out}}

```txt

 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537

```



## Haskell


```haskell
import Data.List (mapAccumL)

feigenbaumApprox :: Int -> [Double]
feigenbaumApprox mx = snd $ mitch mx 10
  where
    mitch :: Int -> Int -> ((Double, Double, Double), [Double])
    mitch mx mxj =
      mapAccumL
        (\(a1, a2, d1) i ->
            let a =
                  iterate
                    (\a ->
                        let (x, y) =
                              iterate
                                (\(x, y) -> (a - (x * x), 1.0 - ((2.0 * x) * y)))
                                (0.0, 0.0) !!
                              (2 ^ i)
                        in a - (x / y))
                    (a1 + (a1 - a2) / d1) !!
                  mxj
                d = (a1 - a2) / (a - a1)
            in ((a, a1, d), d))
        (1.0, 0.0, 3.2)
        [2 .. (1 + mx)]

-- TEST ------------------------------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines) $
  zipWith
    (\i s -> justifyRight 2 ' ' (show i) ++ '\t' : s)
    [1 ..]
    (show <$> feigenbaumApprox 13)
  where
    justifyRight n c s = drop (length s) (replicate n c ++ s)
```

{{Out}}

```txt
 1    3.2185114220380866
 2    4.3856775985683365
 3    4.600949276538056
 4    4.6551304953919646
 5    4.666111947822846
 6    4.668548581451485
 7    4.66906066077106
 8    4.669171554514976
 9    4.669195154039278
10    4.669200256503637
11    4.669200975097843
12    4.669205372040318
13    4.669207514010413
```



## Java

{{trans|Kotlin}}

```java
public class Feigenbaum {
    public static void main(String[] args) {
        int max_it = 13;
        int max_it_j = 10;
        double a1 = 1.0;
        double a2 = 0.0;
        double d1 = 3.2;
        double a;

        System.out.println(" i       d");
        for (int i = 2; i <= max_it; i++) {
            a = a1 + (a1 - a2) / d1;
            for (int j = 0; j < max_it_j; j++) {
                double x = 0.0;
                double y = 0.0;
                for (int k = 0; k < 1 << i; k++) {
                    y = 1.0 - 2.0 * y * x;
                    x = a - x * x;
                }
                a -= x / y;
            }
            double d = (a1 - a2) / (a - a1);
            System.out.printf("%2d    %.8f\n", i, d);
            d1 = d;
            a2 = a1;
            a1 = a;
        }
    }
}
```

{{out}}

```txt
 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537
```




## Julia


```julia
# http://en.wikipedia.org/wiki/Feigenbaum_constant

function feigenbaum_delta(imax=23, jmax=20)
    a1, a2, d1 = BigFloat(1.0), BigFloat(0.0), BigFloat(3.2)
    println("Feigenbaum's delta constant incremental calculation:\ni   δ\n1   3.20")
    for i in 2:imax
        a = a1 + (a1 - a2) / d1
        for j in 1:jmax
            x, y = 0, 0
            for k in 1:2^i
                y = 1 - 2 * x * y
                x = a - x * x
            end
            a -= x / y
        end
        d = (a1 - a2) / (a - a1)
        println(rpad(i, 4), lpad(d, 4))
        d1, a2 = d, a1
        a1 = a
    end
end

feigenbaum_delta()

```
{{out}}

```txt

Feigenbaum's delta constant incremental calculation:
i   δ
1   3.20
2   3.218511422038087912270504530742813256028820377971082199141994437483271226037533
3   4.385677598568339085744948568775522346103216356576497808699630752612705940390646
4   4.600949276538075357811694698623834985023552496633543372295593454454329771521727
5   4.655130495391980136486254995856898819475460497385226078363311588165123307017281
6   4.66611194782857138833121369671177648071905897173694216397236891198998639455025
7   4.668548581446840948044543680148146265543287896654348757317309551400403337843036
8   4.66906066064826823913259982263027263779968209542149740052288679867743088942764
9   4.669171555379511388886004609897567088240676573170789783804375113804695091803033
10  4.669195156030017174021108801191492093392147908605756405516325961597435372704323
11  4.669200229086856497938353781004067217408888048906823830162962242800074595934665
12  4.669201313294204171164754941185571183728248888986548913352217226469150028661929
13  4.669201545780906707506058109930429736431564330452605295006142805341042630340361
14  4.669201595537493910292470639289646040074547412490596040512777985387237785978782
15  4.669201606198152157723831097078594524421336516011873717994000712976201143278191
16  4.669201608480804423294067945898622842792868381815074127672747764898152898198069
17  4.669201608969744700482485321938373343907385540992447405883605282416375303280911
18  4.669201609074452566227981520370886753946099646679618270214759101315481224820708
19  4.669201609096878794705135037864783677622666525741836726064298799595215295927305
20  4.66920160910168168118696016084580172992808889324407617097679098039831535247408
21  4.669201609102710327837210208629111857781724142614997392167298168695631199065625
22  4.669201609102930630539778141205517641783439121041016813735799961205502985593042
23  4.66920160910297781286849594159066394676896043144121209732784416240857379387701

```




## Kotlin

{{trans|Ring}}

```scala
// Version 1.2.40

fun feigenbaum() {
    val maxIt = 13
    val maxItJ = 10
    var a1 = 1.0
    var a2 = 0.0
    var d1 = 3.2
    println(" i       d")
    for (i in 2..maxIt) {
        var a = a1 + (a1 - a2) / d1
        for (j in 1..maxItJ) {
            var x = 0.0
            var y = 0.0
            for (k in 1..(1 shl i)) {
                 y = 1.0 - 2.0 * y * x
                 x = a - x * x
            }
            a -= x / y
        }
        val d = (a1 - a2) / (a - a1)
        println("%2d    %.8f".format(i,d))
        d1 = d
        a2 = a1
        a1 = a
    }
}

fun main(args: Array<String>) {
    feigenbaum()
}
```


{{output}}

```txt

 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537

```



## Lua


```lua
function leftShift(n,p)
    local r = n
    while p>0 do
        r = r * 2
        p = p - 1
    end
    return r
end

-- main

local MAX_IT = 13
local MAX_IT_J = 10
local a1 = 1.0
local a2 = 0.0
local d1 = 3.2

print(" i       d")
for i=2,MAX_IT do
    local a = a1 + (a1 - a2) / d1
    for j=1,MAX_IT_J do
        local x = 0.0
        local y = 0.0
        for k=1,leftShift(1,i) do
            y = 1.0 - 2.0 * y * x
            x = a - x * x
        end
        a = a - x / y
    end
    d = (a1 - a2) / (a - a1)
    print(string.format("%2d    %.8f", i, d))
    d1 = d
    a2 = a1
    a1 = a
end
```

{{out}}

```txt
 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537
```


=={{header|Modula-2}}==

```modula2
MODULE Feigenbaum;
FROM FormatString IMPORT FormatString;
FROM LongStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i,j,k,max_it,max_it_j : INTEGER;
    a,x,y,d,a1,a2,d1 : LONGREAL;
BEGIN
    max_it := 13;
    max_it_j := 10;

    a1 := 1.0;
    a2 := 0.0;
    d1 := 3.2;

    WriteString(" i       d");
    WriteLn;
    FOR i:=2 TO max_it DO
        a := a1 + (a1 - a2) / d1;
        FOR j:=1 TO max_it_j DO
            x := 0.0;
            y := 0.0;
            FOR k:=1 TO INT(1 SHL i) DO
                y := 1.0 - 2.0 * y * x;
                x := a - x * x
            END;
            a := a - x / y
        END;
        d := (a1 - a2) / (a - a1);
        FormatString("%2i    ", buf, i);
        WriteString(buf);
        RealToStr(d, buf);
        WriteString(buf);
        WriteLn;
        d1 := d;
        a2 := a1;
        a1 := a
    END;

    ReadChar
END Feigenbaum.
```



## Perl


```perl
use strict;
use warnings;
use Math::AnyNum 'sqr';

my $a1 = 1.0;
my $a2 = 0.0;
my $d1 = 3.2;

print " i         δ\n";

for my $i (2..13) {
    my $a = $a1 + ($a1 - $a2)/$d1;
    for (1..10) {
        my $x = 0;
        my $y = 0;
        for (1 .. 2**$i) {
            $y = 1 - 2 * $y * $x;
            $x = $a - sqr($x);
        }
        $a -= $x/$y;
    }

    $d1 = ($a1 - $a2) / ($a - $a1);
    ($a2, $a1) = ($a1, $a);
    printf "%2d %17.14f\n", $i, $d1;
}
```

{{out}}

```txt
 2  3.21851142203809
 3  4.38567759856834
 4  4.60094927653808
 5  4.65513049539198
 6  4.66611194782857
 7  4.66854858144684
 8  4.66906066064827
 9  4.66917155537951
10  4.66919515603002
11  4.66920022908686
12  4.66920131329420
13  4.66920154578091
```



## Perl 6

{{works with|Rakudo|2018.04.01}}
{{trans|Ring}}


```perl6
my $a1 = 1;
my $a2 = 0;
my $d = 3.2;

say ' i d';

for 2 .. 13 -> $exp {
    my $a = $a1 + ($a1 - $a2) / $d;
    do {
        my $x = 0;
        my $y = 0;
        for ^2 ** $exp {
            $y = 1 - 2 * $y * $x;
            $x = $a - $x²;
        }
        $a -= $x / $y;
    } xx 10;
     $d = ($a1 - $a2) / ($a - $a1);
     ($a2, $a1) = ($a1, $a);
     printf "%2d %.8f\n", $exp, $d;
}
```

{{out}}

```txt
 i d
 2 3.21851142
 3 4.38567760
 4 4.60094928
 5 4.65513050
 6 4.66611195
 7 4.66854858
 8 4.66906066
 9 4.66917155
10 4.66919515
11 4.66920026
12 4.66920098
13 4.66920537
```



## Phix

{{trans|Ring}}

```Phix
constant maxIt = 13,
        maxItJ = 10
atom a1 = 1.0,
     a2 = 0.0,
     d1 = 3.2
puts(1," i d\n")
for i=2 to maxIt do
     atom a = a1 + (a1 - a2) / d1
     for j=1 to maxItJ do
          atom x = 0, y = 0
          for k=1 to power(2,i) do
               y = 1 - 2*y*x
               x = a - x*x
          end for
          a = a - x/y
     end for
     atom d = (a1-a2)/(a-a1)
     printf(1,"%2d %.8f\n",{i,d})
     d1 = d
     a2 = a1
     a1 = a
end for
```

{{out}}

```txt

 i d
 2 3.21851142
 3 4.38567760
 4 4.60094928
 5 4.65513050
 6 4.66611195
 7 4.66854858
 8 4.66906066
 9 4.66917155
10 4.66919515
11 4.66920026
12 4.66920098
13 4.66920537

```



## Python

{{trans|D}}

```python
max_it = 13
max_it_j = 10
a1 = 1.0
a2 = 0.0
d1 = 3.2
a = 0.0

print " i       d"
for i in range(2, max_it + 1):
    a = a1 + (a1 - a2) / d1
    for j in range(1, max_it_j + 1):
        x = 0.0
        y = 0.0
        for k in range(1, (1 << i) + 1):
            y = 1.0 - 2.0 * y * x
            x = a - x * x
        a = a - x / y
    d = (a1 - a2) / (a - a1)
    print("{0:2d}    {1:.8f}".format(i, d))
    d1 = d
    a2 = a1
    a1 = a
```

{{out}}

```txt
 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537
```



## Racket

{{trans|C}}

```racket
#lang racket
(define (feigenbaum #:max-it (max-it 13) #:max-it-j (max-it-j 10))
  (displayln " i       d" (current-error-port))
  (define-values (_a _a1 d)
    (for/fold ((a 1) (a1 0) (d 3.2))
              ((i (in-range 2 (add1 max-it))))
      (let* ((a′ (for/fold ((a (+ a (/ (- a a1) d))))
                           ((j (in-range max-it-j)))
                   (let-values (([x y] (for/fold ((x 0) (y 0))
                                                 ((k (expt 2 i)))
                                         (values (- a (* x x))
                                                 (- 1 (* 2 y x))))))
                     (- a (/ x y)))))
             (d′ (/ (- a a1) (- a′ a))))
        (eprintf "~a   ~a\n" (~a i #:width 2) (real->decimal-string d′ 8))
        (values a′ a d′))))
  d)

(module+ main
  (feigenbaum))
```

{{out}}

```txt
 i       d
2    3.21851142
3    4.38567760
4    4.60094928
5    4.65513050
6    4.66611195
7    4.66854858
8    4.66906066
9    4.66917155
10   4.66919515
11   4.66920026
12   4.66920098
13   4.66920537
4.669205372040318
```



## REXX

{{trans|Sidef}}

```rexx
/*REXX pgm calculates the (Mitchell) Feigenbaum bifurcation velocity, #digs can be given*/
parse arg digs maxi maxj .                       /*obtain optional argument from the CL.*/
if digs=='' | digs==","  then digs= 30           /*Not specified?  Then use the default.*/
if maxi=='' | maxi==","  then maxi= 20           /* "      "         "   "   "     "    */
if maxJ=='' | maxJ==","  then maxJ= 10           /* "      "         "   "   "     "    */
#= 4.669201609102990671853203820466201617258185577475768632745651343004134330211314737138,
   || 68974402394801381716                       /*◄──Feigenbaum's constant, true value.*/
numeric digits digs                              /*use the specified # of decimal digits*/
    a1=  1
    a2=  0
    d1=  3.2
say 'Using '    maxJ      " iterations for  maxJ,  with "      digs     ' decimal digits:'
say
say copies(' ', 9)             center('correct', 11)              copies(' ', digs+1)
say center('i', 9, "─")        center('digits' , 11, '─')         center('d', digs+1, "─")

    do i=2  for maxi-1
    a= a1  +  (a1 - a2) / d1
                               do maxJ
                               x= 0;   y= 0
                                                   do 2**i;       y= 1  -  2 * x * y
                                                                  x= a  -  x*x
                                                   end   /*2**i*/
                               a= a  -  x / y
                               end   /*maxj*/
    d= (a1 - a2)  /  (a - a1)                    /*compute the delta (D) of the function*/
    t= max(0, compare(d, #)  - 2)                /*# true digs so far, ignore dec. point*/
    say center(i, 9)     center(t, 11)     d     /*display values for  I & D ──►terminal*/
    parse value  d  a1  a    with    d1  a2  a1  /*assign 3 variables with 3 new values.*/
    end   /*i*/
say                                              /*stick a fork in it,  we're all done. */
say '         true value= '    # / 1             /*true value of Feigenbaum's constant. */
```

{{out|output|text=  when using the default inputs:}}

```txt

Using  10  iterations for  maxJ,  with  30  decimal digits:

            correct
────i──── ──digits─── ───────────────d───────────────
    2          0      3.21851142203808791227050453077
    3          1      4.3856775985683390857449485682
    4          2      4.60094927653807535781169469969
    5          2      4.65513049539198013648625498649
    6          3      4.66611194782857138833121364654
    7          3      4.66854858144684094804454708811
    8          4      4.66906066064826823913257549468
    9          4      4.6691715553795113888859465442
   10          4      4.66919515603001717402161720542
   11          6      4.66920022908685649793393149233
   12          7      4.66920131329420417113719511412
   13          7      4.66920154578090670783369507315
   14          7      4.66920159553749390966169074155
   15          9      4.66920160619815215840788706632
   16          9      4.66920160848080435144581223484
   17          9      4.66920160896974538458267849027
   18         10      4.66920160907444981238909862845
   19         10      4.66920160909687888294310165196
   20         12      4.66920160910169069039564432665

         true value=  4.66920160910299067185320382047

```



## Ring


```ring
# Project : Feigenbaum constant calculation

decimals(8)
see "Feigenbaum constant calculation:" + nl
maxIt = 13
maxItJ = 10
a1 = 1.0
a2 = 0.0
d1 = 3.2
see "i     " + "d" + nl
for i = 2 to maxIt
     a = a1 + (a1 - a2) / d1
     for j = 1 to maxItJ
          x = 0
          y = 0
          for k = 1 to pow(2,i)
               y = 1 - 2 * y * x
               x = a - x * x
          next
          a = a - x / y
     next
     d = (a1 - a2) / (a - a1)
     if i < 10
        see "" + i + "    " + d + nl
     else
        see "" + i + "  " + d + nl
     ok
     d1 = d
     a2 = a1
     a1 = a
next
```

Output:

```txt
Feigenbaum constant calculation:
i  d
2  3.21851142
3  4.38567760
4  4.60094928
5  4.65513050
6  4.66611195
7  4.66854858
8  4.66906066
9  4.66917155
10 4.66919515
11 4.66920026
12 4.66920098
13 4.66920537
```


## Scala

===Imperative, ugly===

```Scala
object Feigenbaum1 extends App {
  val (max_it, max_it_j) = (13, 10)
  var (a1, a2, d1, a) = (1.0, 0.0, 3.2, 0.0)

  println(" i       d")
  var i: Int = 2
  while (i <= max_it) {
    a = a1 + (a1 - a2) / d1
    for (_ <- 0 until max_it_j) {
      var (x, y) = (0.0, 0.0)
      for (_ <- 0 until 1 << i) {
        y = 1.0 - 2.0 * y * x
        x = a - x * x
      }
      a -= x / y
    }
    val d: Double = (a1 - a2) / (a - a1)
    printf("%2d    %.8f\n", i, d)
    d1 = d
    a2 = a1
    a1 = a
    i += 1
  }

}
```

===Functional Style, Tail recursive===
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/OjA3sae/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/04eS3BfCShmrA7I8ZmQfJA Scastie (remote JVM)].

```Scala
object Feigenbaum2 extends App {
  private val (max_it, max_it_j) = (13, 10)

  private def result = {

    @scala.annotation.tailrec
    def outer(i: Int, d1: Double, a2: Double, a1: Double, acc: Seq[Double]): Seq[Double] = {
      @scala.annotation.tailrec
      def center(j: Int, a: Double): Double = {
        @scala.annotation.tailrec
        def inner(k: Int, end: Int, x: Double, y: Double): (Double, Double) =
          if (k < end) inner(k + 1, end, a - x * x, 1.0 - 2.0 * y * x) else (x, y)

        val (x, y) = inner(0, 1 << i, 0.0, 0.0)
        if (j < max_it_j) {
          center(j + 1, a - (x / y))
        } else a
      }

      if (i <= max_it) {
        val a = center(0, a1 + (a1 - a2) / d1)
        val d: Double = (a1 - a2) / (a - a1)

        outer(i + 1, d, a1, a, acc :+ d)
      } else acc
    }

    outer(2, 3.2, 0, 1.0, Seq[Double]()).zipWithIndex
  }

  println(" i     ≈ δ")
  result.foreach { case (δ, i) => println(f"${i + 2}%2d  $δ%.8f") }

}
```



## Sidef

{{trans|Perl 6}}

```ruby
var a1 = 1
var a2 = 0
var δ  = 3.2.float

say " i\tδ"

for i in (2..15) {
    var a0 = ((a1 - a2)/δ + a1)
    10.times {
        var (x, y) = (0, 0)
        2**i -> times {
            y = (1 - 2*x*y)
            x = (a0 - x²)
        }
        a0 -= x/y
    }
    δ = ((a1 - a2) / (a0 - a1))
    (a2, a1) = (a1, a0)
    printf("%2d %.8f\n", i, δ)
}
```

{{out}}

```txt

 i	δ
 2 3.21851142
 3 4.38567760
 4 4.60094928
 5 4.65513050
 6 4.66611195
 7 4.66854858
 8 4.66906066
 9 4.66917156
10 4.66919516
11 4.66920023
12 4.66920131
13 4.66920155
14 4.66920160
15 4.66920161

```



## zkl

{{trans|Kotlin}}

```zkl
fcn feigenbaum{
   maxIt,maxItJ,a1,a2,d1,a,d := 13, 10, 1.0, 0.0, 3.2, 0, 0;
   println(" i       d");
   foreach i in ([2..maxIt]){
      a=a1 + (a1 - a2)/d1;
      foreach j in ([1..maxItJ]){
         x,y := 0.0, 0.0;
	 foreach k in ([1..(1).shiftLeft(i)]){ y,x = 1.0 - 2.0*y*x, a - x*x; }
	 a-=x/y
      }
      d=(a1 - a2)/(a - a1);
      println("%2d    %.8f".fmt(i,d));
      d1,a2,a1 = d,a1,a;
   }
}();
```

{{out}}

```txt

 i       d
 2    3.21851142
 3    4.38567760
 4    4.60094928
 5    4.65513050
 6    4.66611195
 7    4.66854858
 8    4.66906066
 9    4.66917155
10    4.66919515
11    4.66920026
12    4.66920098
13    4.66920537

```

