+++
title = "Map range"
description = ""
date = 2019-02-28T11:25:25Z
aliases = []
[extra]
id = 8856
[taxonomies]
categories = ["Mathematics", "Interpolation", "task"]
tags = []
languages = [
  "acl2",
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "axiom",
  "basic",
  "bbc_basic",
  "bc",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "maxima",
  "nemerle",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "stata",
  "swift",
  "tcl",
  "ursala",
  "wdte",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

## Task

Given two [[wp:Interval (mathematics)|ranges]]:
:::*   <big><math>[a_1,a_2]</math></big>   and
:::*   <big><math>[b_1,b_2]</math></big>;
:::*   then a value   <big><math>s</math></big>   in range   <big><math>[a_1,a_2]</math></big>
:::*   is linearly mapped to a value   <big><math>t</math></big>   in range   <big><math>[b_1,b_2]</math>
</big>   where:



:::*   <big><big><math>t = b_1 + {(s - a_1)(b_2 - b_1) \over (a_2 - a_1)}</math></big></big>


### Requirements
Write a function/subroutine/... that takes two ranges and a real number, and returns the mapping of the real number from the first to the second range.

Use this function to map values from the range   <big><code> [0, 10] </code></big>   to the range   <big><code> [-1, 0]. </code></big>


;Extra credit:
Show additional idiomatic ways of performing the mapping, using tools available to the language.





## ACL2



```Lisp
(defun mapping (a1 a2 b1 b2 s)
   (+ b1 (/ (* (- s a1)
               (- b2 b1))
            (- a2 a1))))

(defun map-each (a1 a2 b1 b2 ss)
   (if (endp ss)
       nil
       (cons (mapping a1 a2 b1 b2 (first ss))
             (map-each a1 a2 b1 b2 (rest ss)))))

(map-each 0 10 -1 0 '(0 1 2 3 4 5 6 7 8 9 10))

;; (-1 -9/10 -4/5 -7/10 -3/5 -1/2 -2/5 -3/10 -1/5 -1/10 0)


```



## Ada



```Ada
with Ada.Text_IO;
procedure Map is
   type First_Range  is new Float range 0.0 .. 10.0;
   type Second_Range is new Float range -1.0 .. 0.0;
   function Translate (Value : First_Range) return Second_Range is
      B1 : Float := Float (Second_Range'First);
      B2 : Float := Float (Second_Range'Last);
      A1 : Float := Float (First_Range'First);
      A2 : Float := Float (First_Range'Last);
      Result : Float;
   begin
      Result := B1 + (Float (Value) - A1) * (B2 - B1) / (A2 - A1);
      return Second_Range (Result);
   end;
   function Translate (Value : Second_Range) return First_Range is
      B1 : Float := Float (First_Range'First);
      B2 : Float := Float (First_Range'Last);
      A1 : Float := Float (Second_Range'First);
      A2 : Float := Float (Second_Range'Last);
      Result : Float;
   begin
      Result := B1 + (Float (Value) - A1) * (B2 - B1) / (A2 - A1);
      return First_Range (Result);
   end;
   Test_Value : First_Range := First_Range'First;
begin
   loop
      Ada.Text_IO.Put_Line (First_Range'Image (Test_Value) & " maps to: "
                          & Second_Range'Image (Translate (Test_Value)));
      exit when Test_Value = First_Range'Last;
      Test_Value := Test_Value + 1.0;
   end loop;
end Map;
```


{{out}}

```txt
 0.00000E+00 maps to: -1.00000E+00
 1.00000E+00 maps to: -9.00000E-01
 2.00000E+00 maps to: -8.00000E-01
 3.00000E+00 maps to: -7.00000E-01
 4.00000E+00 maps to: -6.00000E-01
 5.00000E+00 maps to: -5.00000E-01
 6.00000E+00 maps to: -4.00000E-01
 7.00000E+00 maps to: -3.00000E-01
 8.00000E+00 maps to: -2.00000E-01
 9.00000E+00 maps to: -1.00000E-01
 1.00000E+01 maps to:  0.00000E+00
```



## ALGOL 68


```algol68
# maps a real s in the range [ a1, a2 ] to the range [ b1, b2 ]           #
# there are no checks that s is in the range or that the ranges are valid #
PROC map range = ( REAL s, a1, a2, b1, b2 )REAL:
    b1 + ( ( s - a1 ) * ( b2 - b1 ) ) / ( a2 - a1 );

# test the mapping #
FOR i FROM 0 TO 10 DO
    print( ( whole( i, -2 ), " maps to ", fixed( map range( i, 0, 10, -1, 0 ), -8, 2 ), newline ) )
OD
```

{{out}}

```txt

 0 maps to    -1.00
 1 maps to    -0.90
 2 maps to    -0.80
 3 maps to    -0.70
 4 maps to    -0.60
 5 maps to    -0.50
 6 maps to    -0.40
 7 maps to    -0.30
 8 maps to    -0.20
 9 maps to    -0.10
10 maps to     0.00

```



## AppleScript


```applescript
-- rangeMap :: (Num, Num) -> (Num, Num) -> Num -> Num
on rangeMap(a, b)
    script
        on |λ|(s)
            set {a1, a2} to a
            set {b1, b2} to b
            b1 + ((s - a1) * (b2 - b1)) / (a2 - a1)
        end |λ|
    end script
end rangeMap


-- TEST ---------------------------------------------------
on run
    set mapping to rangeMap({0, 10}, {-1, 0})

    set xs to enumFromTo(0, 10)
    set ys to map(mapping, xs)
    set zs to map(approxRatio(0), ys)

    unlines(zipWith3(formatted, xs, ys, zs))
end run


-- DISPLAY ------------------------------------------------

-- formatted :: Int -> Float -> Ratio -> String
on formatted(x, m, r)
    set fract to showRatio(r)
    set {n, d} to splitOn("/", fract)

    (justifyRight(2, space, x as string) & "   ->   " & ¬
        justifyRight(4, space, m as string)) & "   =   " & ¬
        justifyRight(2, space, n) & "/" & d
end formatted


-- GENERIC ABSTRACTIONS -----------------------------------

-- https://github.com/RobTrew/prelude-applescript

-- Absolute value.
-- abs :: Num -> Num
on abs(x)
    if 0 > x then
        -x
    else
        x
    end if
end abs

-- approxRatio :: Real -> Real -> Ratio
on approxRatio(epsilon)
    script
        on |λ|(n)
            if {real, integer} contains (class of epsilon) and 0 < epsilon then
                set e to epsilon
            else
                set e to 1 / 10000
            end if

            script gcde
                on |λ|(e, x, y)
                    script _gcd
                        on |λ|(a, b)
                            if b < e then
                                a
                            else
                                |λ|(b, a mod b)
                            end if
                        end |λ|
                    end script
                    |λ|(abs(x), abs(y)) of _gcd
                end |λ|
            end script

            set c to |λ|(e, 1, n) of gcde
            ratio((n div c), (1 div c))
        end |λ|
    end script
end approxRatio

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

-- gcd :: Int -> Int -> Int
on gcd(a, b)
    set x to abs(a)
    set y to abs(b)
    repeat until y = 0
        if x > y then
            set x to x - y
        else
            set y to y - x
        end if
    end repeat
    return x
end gcd

-- justifyLeft :: Int -> Char -> String -> String
on justifyLeft(n, cFiller, strText)
    if n > length of strText then
        text 1 thru n of (strText & replicate(n, cFiller))
    else
        strText
    end if
end justifyLeft

-- justifyRight :: Int -> Char -> String -> String
on justifyRight(n, cFiller, strText)
    if n > length of strText then
        text -n thru -1 of ((replicate(n, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight

-- length :: [a] -> Int
on |length|(xs)
    set c to class of xs
    if list is c or string is c then
        length of xs
    else
        (2 ^ 29 - 1) -- (maxInt - simple proxy for non-finite)
    end if
end |length|

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

-- minimum :: Ord a => [a] -> a
on minimum(xs)
    set lng to length of xs
    if lng < 1 then return missing value
    set m to item 1 of xs
    repeat with x in xs
        set v to contents of x
        if v < m then set m to v
    end repeat
    return m
end minimum

-- ratio :: Int -> Int -> Ratio Int
on ratio(x, y)
    script go
        on |λ|(x, y)
            if 0 ≠ y then
                if 0 ≠ x then
                    set d to gcd(x, y)
                    {type:"Ratio", n:(x div d), d:(y div d)}
                else
                    {type:"Ratio", n:0, d:1}
                end if
            else
                missing value
            end if
        end |λ|
    end script
    go's |λ|(x * (signum(y)), abs(y))
end ratio

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length
-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- showRatio :: Ratio -> String
on showRatio(r)
    (n of r as string) & "/" & (d of r as string)
end showRatio

-- signum :: Num -> Num
on signum(x)
    if x < 0 then
        -1
    else if x = 0 then
        0
    else
        1
    end if
end signum

-- splitOn :: String -> String -> [String]
on splitOn(pat, src)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, pat}
    set xs to text items of src
    set my text item delimiters to dlm
    return xs
end splitOn

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
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
    else
        missing value
    end if
end take

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
on zipWith3(f, xs, ys, zs)
    set lng to minimum({length of xs, length of ys, length of zs})
    if 1 > lng then return {}
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys, item i of zs)
        end repeat
        return lst
    end tell
end zipWith3
```

{{Out}}

```txt
 0   ->   -1.0   =   -1/1
 1   ->   -0.9   =   -9/10
 2   ->   -0.8   =   -4/5
 3   ->   -0.7   =   -7/10
 4   ->   -0.6   =   -3/5
 5   ->   -0.5   =   -1/2
 6   ->   -0.4   =   -2/5
 7   ->   -0.3   =   -3/10
 8   ->   -0.2   =   -1/5
 9   ->   -0.1   =   -1/10
10   ->    0.0   =    0/1
```



## AutoHotkey

{{trans|C}}

```AutoHotkey

mapRange(a1, a2, b1, b2, s)
{
	return b1 + (s-a1)*(b2-b1)/(a2-a1)
}

out := "Mapping [0,10] to [-1,0] at intervals of 1:`n"

Loop 11
	out .= "f(" A_Index-1 ") = " mapRange(0,10,-1,0,A_Index-1) "`n"
MsgBox % out

```



## Axiom

Axiom provides a Segment domain for intervals. The following uses a closure for a mapRange function over fields, which provides for some generality.

```Axiom
)abbrev package TESTP TestPackage
TestPackage(R:Field) : with
    mapRange: (Segment(R), Segment(R)) -> (R->R)
  == add
    mapRange(fromRange, toRange) ==
      (a1,a2,b1,b2) := (lo fromRange,hi fromRange,lo toRange,hi toRange)
      (x:R):R +-> b1+(x-a1)*(b2-b1)/(a2-a1)
```

Use:
```Axiom
f := mapRange(1..10,a..b)
[(xi,f xi) for xi in 1..10]
```

{{out}}

```txt
              b + 8a      2b + 7a      b + 2a      4b + 5a      5b + 4a
   [(1,a), (2,------), (3,-------), (4,------), (5,-------), (6,-------),
                 9           9            3           9            9
       2b + a      7b + 2a      8b + a
    (7,------), (8,-------), (9,------), (10,b)]
          3           9            9
                             Type: List(Tuple(Fraction(Polynomial(Integer))))
```



## AWK


```AWK

# syntax: GAWK -f MAP_RANGE.AWK
BEGIN {
    a1 = 0
    a2 = 10
    b1 = -1
    b2 = 0
    for (i=a1; i<=a2; i++) {
      printf("%g maps to %g\n",i,map_range(a1,a2,b1,b2,i))
    }
    exit(0)
}
function map_range(a1,a2,b1,b2,num) {
    return b1 + ((num-a1) * (b2-b1) / (a2-a1))
}

```

{{out}}

```txt

0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.3
8 maps to -0.2
9 maps to -0.1
10 maps to 0

```



## BASIC


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      @% = 5 : REM Column width
      DIM range{l, h}
      DIM A{} = range{}, B{} = range{}
      A.l = 0 : A.h = 10
      B.l = -1 : B.h = 0
      FOR n = 0 TO 10
        PRINT n " maps to " FNmaprange(A{}, B{}, n)
      NEXT
      END

      DEF FNmaprange(a{}, b{}, s)
      = b.l + (s - a.l) * (b.h - b.l) / (a.h - a.l)
```

{{out}}

```txt

    0 maps to    -1
    1 maps to  -0.9
    2 maps to  -0.8
    3 maps to  -0.7
    4 maps to  -0.6
    5 maps to  -0.5
    6 maps to  -0.4
    7 maps to  -0.3
    8 maps to  -0.2
    9 maps to  -0.1
   10 maps to     0

```


=
## Commodore BASIC
=

```commodorebasic
10 REM MAP RANGE
20 REM COMMODORE BASIC 2.0
30 REM
### ==========================

40 A1 = 0 : A2 = 10
50 B1 = -1 : B2 = 0
60 DEF FN MR(S)=B1+(S-A1)*(B2-B1)/(A2-A1)
70 FOR S=0 TO 10
80   PRINT S;"MAPS TO ";FN MR(S)
90 NEXT
```


{{out}}

```txt

 0 MAPS TO -1
 1 MAPS TO -.9
 2 MAPS TO -.8
 3 MAPS TO -.7
 4 MAPS TO -.6
 5 MAPS TO -.5
 6 MAPS TO -.4
 7 MAPS TO -.3
 8 MAPS TO -.2
 9 MAPS TO -.1
 10 MAPS TO  0

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "MapRange.bas"
110 LET A1=0:LET A2=10
120 LET B1=-1:LET B2=0
130 DEF MR(S)=B1+(S-A1)*(B2-B1)/(A2-A1)
140 FOR I=0 TO 10
150   PRINT I;"maps to ";MR(I)
160 NEXT
```



## bc


```bc
/* map s from [a, b] to [c, d] */
define m(a, b, c, d, s) {
	return (c + (s - a) * (d - c) / (b - a))
}

scale = 6  /* division to 6 decimal places */
"[0, 10] => [-1, 0]
"
for (i = 0; i <= 10; i += 2) {
	/*
         * If your bc(1) has a print statement, you can try
	 * print i, " => ", m(0, 10, -1, 0, i), "\n"
	 */
	i; "   => "; m(0, 10, -1, 0, i)
}
quit
```


{{out}}

```txt
[0, 10] => [-1, 0]
0
   => -1.000000
2
   => -.800000
4
   => -.600000
6
   => -.400000
8
   => -.200000
10
   => 0.000000
```



## Bracmat

{{trans|C}}

```bracmat
( ( mapRange
  =   a1,a2,b1,b2,s
    .   !arg:(?a1,?a2.?b1,?b2.?s)
      & !b1+(!s+-1*!a1)*(!b2+-1*!b1)*(!a2+-1*!a1)^-1
  )
& out$"Mapping [0,10] to [-1,0] at intervals of 1:"
& 0:?n
&   whl
  ' ( !n:~>10
    & out$("f(" !n ") = " flt$(mapRange$(0,10.-1,0.!n),2))
    & 1+!n:?n
    )
);
```

{{out}}

```txt
Mapping [0,10] to [-1,0] at intervals of 1:
f( 0 ) =  -1,00*10E0
f( 1 ) =  -9,00*10E-1
f( 2 ) =  -8,00*10E-1
f( 3 ) =  -7,00*10E-1
f( 4 ) =  -6,00*10E-1
f( 5 ) =  -5,00*10E-1
f( 6 ) =  -4,00*10E-1
f( 7 ) =  -3,00*10E-1
f( 8 ) =  -2,00*10E-1
f( 9 ) =  -1,00*10E-1
f( 10 ) =  0
```



## C


```c
#include <stdio.h>

double mapRange(double a1,double a2,double b1,double b2,double s)
{
	return b1 + (s-a1)*(b2-b1)/(a2-a1);
}

int main()
{
	int i;
	puts("Mapping [0,10] to [-1,0] at intervals of 1:");

	for(i=0;i<=10;i++)
	{
		printf("f(%d) = %g\n",i,mapRange(0,10,-1,0,i));
	}

	return 0;
}

```


{{out}}

```txt
Mapping [0,10] to [-1,0] at intervals of 1:
f(0) = -1
f(1) = -0.9
f(2) = -0.8
f(3) = -0.7
f(4) = -0.6
f(5) = -0.5
f(6) = -0.4
f(7) = -0.3
f(8) = -0.2
f(9) = -0.1
f(10) = 0
```



## C#


```c#
using System;
using System.Linq;

public class MapRange
{
    public static void Main() {
        foreach (int i in Enumerable.Range(0, 11))
            Console.WriteLine($"{i} maps to {Map(0, 10, -1, 0, i)}");
    }

    static double Map(double a1, double a2, double b1, double b2, double s) => b1 + (s - a1) * (b2 - b1) / (a2 - a1);
}
```

{{out}}

```txt

0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.3
8 maps to -0.2
9 maps to -0.1
10 maps to 0
```



## C++

This example defines a template function to handle the mapping, using two '''std::pair''' objects to define the source and destination ranges. It returns the provided value mapped into the target range.

It's not written efficiently; certainly, there can be fewer explicit temporary variables. The use of the template offers a choice in types for precision and accuracy considerations, though one area for improvement might be to allow a different type for intermediate calculations.


```cpp
#include <iostream>
#include <utility>

template<typename tVal>
tVal map_value(std::pair<tVal,tVal> a, std::pair<tVal, tVal> b, tVal inVal)
{
  tVal inValNorm = inVal - a.first;
  tVal aUpperNorm = a.second - a.first;
  tVal normPosition = inValNorm / aUpperNorm;

  tVal bUpperNorm = b.second - b.first;
  tVal bValNorm = normPosition * bUpperNorm;
  tVal outVal = b.first + bValNorm;

  return outVal;
}

int main()
{
  std::pair<float,float> a(0,10), b(-1,0);

  for(float value = 0.0; 10.0 >= value; ++value)
    std::cout << "map_value(" << value << ") = " << map_value(a, b, value) << std::endl;

  return 0;
}
```


{{out}}

```txt
map_value(0) = -1
map_value(1) = -0.9
map_value(2) = -0.8
map_value(3) = -0.7
map_value(4) = -0.6
map_value(5) = -0.5
map_value(6) = -0.4
map_value(7) = -0.3
map_value(8) = -0.2
map_value(9) = -0.1
map_value(10) = 0

```



## Clojure


{{trans|Python}}


```clojure

(defn maprange [[a1 a2] [b1 b2] s]
	(+ b1 (/ (* (- s a1) (- b2 b1)) (- a2 a1))))

> (doseq [s (range 11)]
       (printf "%2s maps to %s\n" s (maprange [0 10] [-1 0] s)))

 0 maps to -1
 1 maps to -9/10
 2 maps to -4/5
 3 maps to -7/10
 4 maps to -3/5
 5 maps to -1/2
 6 maps to -2/5
 7 maps to -3/10
 8 maps to -1/5
 9 maps to -1/10
10 maps to 0

```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. demo-map-range.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i                       USAGE FLOAT-LONG.

       01  mapped-num              USAGE FLOAT-LONG.

       01  a-begin                 USAGE FLOAT-LONG VALUE 0.
       01  a-end                   USAGE FLOAT-LONG VALUE 10.

       01  b-begin                 USAGE FLOAT-LONG VALUE -1.
       01  b-end                   USAGE FLOAT-LONG VALUE 0.

       01  i-display               PIC --9.9.
       01  mapped-display          PIC --9.9.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 0 BY 1 UNTIL i > 10
               CALL "map-range" USING CONTENT a-begin, a-end, b-begin,
                   b-end, i, REFERENCE mapped-num
               COMPUTE i-display ROUNDED = i
               COMPUTE mapped-display ROUNDED = mapped-num
               DISPLAY FUNCTION TRIM(i-display) " maps to "
                   FUNCTION TRIM(mapped-display)
           END-PERFORM
           .
       END PROGRAM demo-map-range.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. map-range.

       DATA DIVISION.
       LINKAGE SECTION.
       01  a-begin                 USAGE FLOAT-LONG.
       01  a-end                   USAGE FLOAT-LONG.

       01  b-begin                 USAGE FLOAT-LONG.
       01  b-end                   USAGE FLOAT-LONG.

       01  val-to-map              USAGE FLOAT-LONG.

       01  ret                     USAGE FLOAT-LONG.

       PROCEDURE DIVISION USING a-begin, a-end, b-begin, b-end,
               val-to-map, ret.
           COMPUTE ret =
               b-begin + ((val-to-map - a-begin) * (b-end - b-begin)
                   / (a-end - a-begin))
           .
       END PROGRAM map-range.
```


The output is identical to the output of the Common Lisp example.


## CoffeeScript



```CoffeeScript

mapRange = (a1,a2,b1,b2,s) ->
    t = b1 + ((s-a1)*(b2 - b1)/(a2-a1))

for s in [0..10]
    console.log("#{s} maps to #{mapRange(0,10,-1,0,s)}")

```

{{out}}

```txt

0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.30000000000000004
8 maps to -0.19999999999999996
9 maps to -0.09999999999999998
10 maps to 0

```



## Common Lisp


```lisp
(defun map-range (a1 a2 b1 b2 s)
  (+ b1
     (/ (* (- s a1)
	   (- b2 b1))
	(- a2 a1))))

(loop
   for i from 0 to 10
   do (format t "~F maps to ~F~C" i
	      (map-range 0 10 -1 0 i)
	      #\Newline))
```


{{out}}

```txt
0.0 maps to -1.0
1.0 maps to -0.9
2.0 maps to -0.8
3.0 maps to -0.7
4.0 maps to -0.6
5.0 maps to -0.5
6.0 maps to -0.4
7.0 maps to -0.3
8.0 maps to -0.2
9.0 maps to -0.1
10.0 maps to 0.0
```




## D


```d
double mapRange(in double[] a, in double[] b, in double s)
pure nothrow @nogc {
    return b[0] + ((s - a[0]) * (b[1] - b[0]) / (a[1] - a[0]));
}

void main() {
    import std.stdio;

    immutable r1 = [0.0, 10.0];
    immutable r2 = [-1.0, 0.0];
    foreach (immutable s; 0 .. 11)
        writefln("%2d maps to %5.2f", s, mapRange(r1, r2, s));
}
```

{{out}}

```txt
 0 maps to -1.00
 1 maps to -0.90
 2 maps to -0.80
 3 maps to -0.70
 4 maps to -0.60
 5 maps to -0.50
 6 maps to -0.40
 7 maps to -0.30
 8 maps to -0.20
 9 maps to -0.10
10 maps to  0.00
```



## EchoLisp

EchoLisp provides several native interpolation functions: smoothstep, s-curve, .. and '''linear''' which performs linear interpolation.

```scheme

(lib 'plot) ;; interpolation functions
(lib 'compile)

;; rational version
(define (q-map-range x xmin xmax ymin ymax) (+ ymin (/ ( * (- x xmin) (- ymax ymin)) (- xmax xmin))))

;; float version
(define (map-range x xmin xmax ymin ymax) (+ ymin (// ( * (- x xmin) (- ymax ymin)) (- xmax xmin))))
; accelerate it
(compile 'map-range "-vf")

(q-map-range 4 0 10 -1 0)
    → -3/5
(map-range 4 0 10 -1 0)
    → -0.6
(linear 4 0 10 -1 0) ;; native
    → -0.6

(for [(x (in-range 0 10))] (writeln x (q-map-range x 0 10 -1 0) (map-range x 0 10 -1 0)))

0     -1     -1
1     -9/10     -0.9
2     -4/5     -0.8
3     -7/10     -0.7
4     -3/5     -0.6
5     -1/2     -0.5
6     -2/5     -0.4
7     -3/10     -0.3
8     -1/5     -0.2
9     -1/10     -0.1

```



## Elixir


```elixir
defmodule RC do
  def map_range(a1 .. a2, b1 .. b2, s) do
    b1 + (s - a1) * (b2 - b1) / (a2 - a1)
  end
end

Enum.each(0..10, fn s ->
  :io.format "~2w map to ~7.3f~n", [s, RC.map_range(0..10, -1..0, s)]
end)
```


{{out}}

```txt

 0 map to  -1.000
 1 map to  -0.900
 2 map to  -0.800
 3 map to  -0.700
 4 map to  -0.600
 5 map to  -0.500
 6 map to  -0.400
 7 map to  -0.300
 8 map to  -0.200
 9 map to  -0.100
10 map to   0.000

```



## Emacs Lisp


```lisp
(defun maprange (a1 a2 b1 b2 s)
   (+ b1 (/ (* (- s a1) (- b2 b1)) (- a2 a1))))

(dotimes (i 10)
  (princ (maprange 0.0 10.0 -1.0 0.0 i))
  (terpri))
```



## Erlang


```erlang
-module(map_range).
-export([map_value/3]).

map_value({A1,A2},{B1,B2},S) ->
    B1 + (S - A1) * (B2 - B1) / (A2 - A1).

```



## ERRE


```ERRE
PROGRAM RANGE

BEGIN
      AL=0   AH=10
      BL=-1  BH=0
      FOR N=0 TO 10 DO
        RANGE=BL+(N-AL)*(BH-BL)/(AH-AL)
        WRITE("### maps to ##.##";N;RANGE)
!        PRINT(N;" maps to ";RANGE)
      END FOR
END PROGRAM

```

{{out}}

```txt
  0 maps to -1.00
  1 maps to -0.90
  2 maps to -0.80
  3 maps to -0.70
  4 maps to -0.60
  5 maps to -0.50
  6 maps to -0.40
  7 maps to -0.30
  8 maps to -0.20
  9 maps to -0.10
 10 maps to  0.00

```



## Euphoria


```euphoria
function map_range(sequence a, sequence b, atom s)
    return b[1]+(s-a[1])*(b[2]-b[1])/(a[2]-a[1])
end function

for i = 0 to 10 do
    printf(1, "%2g maps to %4g\n", {i, map_range({0,10},{-1,0},i)})
end for
```


{{out}}

```txt
 0 maps to   -1
 1 maps to -0.9
 2 maps to -0.8
 3 maps to -0.7
 4 maps to -0.6
 5 maps to -0.5
 6 maps to -0.4
 7 maps to -0.3
 8 maps to -0.2
 9 maps to -0.1
10 maps to    0

```


## Factor


```factor
USE: locals
:: map-range ( a1 a2 b1 b2 x -- y )
   x a1 - b2 b1 - * a2 a1 - / b1 + ;
```

Or:

```factor
USING: locals infix ;
:: map-range ( a1 a2 b1 b2 x -- y )
   [infix
     b1 + (x - a1) * (b2 - b1) / (a2 - a1)
   infix] ;
```

Test run:

```factor
10 iota [| x | 0 10 -1 0 x map-range ] map . ! { -1 -9/10 -4/5 -7/10 -3/5 -1/2 -2/5 -3/10 -1/5 -1/10 }
```



## Fantom



```fantom

class FRange
{
  const Float low
  const Float high
  // in constructing a range, ensure the low value is smaller than high
  new make (Float low, Float high)
  {
    this.low = ( low <= high ? low : high )
    this.high = ( low <= high ? high : low )
  }

  // return range as a string
  override Str toStr () { "[$low,$high]" }

  // return a point in given range interpolated into this range
  Float remap (Float point, FRange given)
  {
    this.low + (point - given.low) * (this.high - this.low) / (given.high - given.low)
  }
}

class Main
{
  public static Void main ()
  {
    range1 := FRange (0f, 10f)
    range2 := FRange (-1f, 0f)
    11.times |Int n|
    {
      m := range2.remap (n.toFloat, range1)
      echo ("Value $n in ${range1} maps to $m in ${range2}")
    }
  }
}

```


{{out}}

```txt

Value 0 in [0.0,10.0] maps to -1.0 in [-1.0,0.0]
Value 1 in [0.0,10.0] maps to -0.9 in [-1.0,0.0]
Value 2 in [0.0,10.0] maps to -0.8 in [-1.0,0.0]
Value 3 in [0.0,10.0] maps to -0.7 in [-1.0,0.0]
Value 4 in [0.0,10.0] maps to -0.6 in [-1.0,0.0]
Value 5 in [0.0,10.0] maps to -0.5 in [-1.0,0.0]
Value 6 in [0.0,10.0] maps to -0.4 in [-1.0,0.0]
Value 7 in [0.0,10.0] maps to -0.30000000000000004 in [-1.0,0.0]
Value 8 in [0.0,10.0] maps to -0.19999999999999996 in [-1.0,0.0]
Value 9 in [0.0,10.0] maps to -0.09999999999999998 in [-1.0,0.0]
Value 10 in [0.0,10.0] maps to 0.0 in [-1.0,0.0]

```



## Forth


```forth
\ linear interpolation

: lerp ( b2 b1 a2 a1 s -- t )
  fover f-
  frot frot f- f/
  frot frot fswap fover f- frot f*
  f+ ;

: test   11 0 do  0e -1e 10e 0e i s>f lerp f.  loop ;
```


There is less stack shuffling if you use origin and range instead of endpoints for intervals.  (o = a1, r = a2-a1)


```forth
: lerp ( o2 r2 r1 o1 s -- t ) fswap f-  fswap f/  f*  f+ ;

: test   11 0 do  -1e 1e 10e 0e i s>f lerp f.  loop ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Map
  implicit none

  real :: t
  integer :: i

  do i = 0, 10
    t = Maprange((/0.0, 10.0/), (/-1.0, 0.0/), real(i))
    write(*,*) i, " maps to ", t
  end do

contains

function Maprange(a, b, s)
  real :: Maprange
  real, intent(in) :: a(2), b(2), s

  Maprange = (s-a(1)) * (b(2)-b(1)) / (a(2)-a(1)) + b(1)

end function Maprange
end program Map
```


## Go

'''Basic task'''

```go
package main

import "fmt"

type rangeBounds struct {
    b1, b2 float64
}

func mapRange(x, y rangeBounds, n float64) float64 {
    return y.b1 + (n - x.b1) * (y.b2 - y.b1) / (x.b2 - x.b1)
}

func main() {
    r1 := rangeBounds{0, 10}
    r2 := rangeBounds{-1, 0}
    for n := float64(0); n <= 10; n += 2 {
        fmt.Println(n, "maps to", mapRange(r1, r2, n))
    }
}
```

{{out}}

```txt

0 maps to -1
2 maps to -0.8
4 maps to -0.6
6 maps to -0.4
8 maps to -0.19999999999999996
10 maps to 0

```

'''Extra credit'''

First, a function literal replaces the mapping function specified by the basic task.
This allows a simpler parameter signature and also allows things to be precomputed for efficiency.  newMapRange checks the direction of the first range and if it is decreasing, reverses both ranges.
This simplifies an out-of-range check in the function literal.
Also, the slope and intercept of the linear function are computed.
This allows the range mapping to use the slope intercept formula which is computationally more efficient that the two point formula.

Second, ", ok" is a Go idiom.  It takes advantage of Go's multiple return values and multiple assignment to return a success/failure disposition.
In the case of this task, the result t is undefined if the input s is out of range.

```go
package main

import "fmt"

type rangeBounds struct {
    b1, b2 float64
}

func newRangeMap(xr, yr rangeBounds) func(float64) (float64, bool) {
    // normalize direction of ranges so that out-of-range test works
    if xr.b1 > xr.b2 {
        xr.b1, xr.b2 = xr.b2, xr.b1
        yr.b1, yr.b2 = yr.b2, yr.b1
    }
    // compute slope, intercept
    m := (yr.b2 - yr.b1) / (xr.b2 - xr.b1)
    b := yr.b1 - m*xr.b1
    // return function literal
    return func(x float64) (y float64, ok bool) {
        if x < xr.b1 || x > xr.b2 {
            return 0, false // out of range
        }
        return m*x + b, true
    }
}

func main() {
    rm := newRangeMap(rangeBounds{0, 10}, rangeBounds{-1, 0})
    for s := float64(-2); s <= 12; s += 2 {
        t, ok := rm(s)
        if ok {
            fmt.Printf("s: %5.2f  t: %5.2f\n", s, t)
        } else {
            fmt.Printf("s: %5.2f  out of range\n", s)
        }
    }
}
```

{{out}}

```txt

s: -2.00  out of range
s:  0.00  t: -1.00
s:  2.00  t: -0.80
s:  4.00  t: -0.60
s:  6.00  t: -0.40
s:  8.00  t: -0.20
s: 10.00  t:  0.00
s: 12.00  out of range

```



## Groovy


```groovy

def mapRange(a1, a2, b1, b2, s) {
    b1 + ((s - a1) * (b2 - b1)) / (a2 - a1)
}

(0..10).each { s ->
    println(s + " in [0, 10] maps to " + mapRange(0, 10, -1, 0, s) + " in [-1, 0].")
}

```

{{out}}

```txt

0 in [0, 10] maps to -1 in [-1, 0].
1 in [0, 10] maps to -0.9 in [-1, 0].
2 in [0, 10] maps to -0.8 in [-1, 0].
3 in [0, 10] maps to -0.7 in [-1, 0].
4 in [0, 10] maps to -0.6 in [-1, 0].
5 in [0, 10] maps to -0.5 in [-1, 0].
6 in [0, 10] maps to -0.4 in [-1, 0].
7 in [0, 10] maps to -0.3 in [-1, 0].
8 in [0, 10] maps to -0.2 in [-1, 0].
9 in [0, 10] maps to -0.1 in [-1, 0].
10 in [0, 10] maps to 0 in [-1, 0].

```


## Haskell

Rather than handling only floating point numbers, the mapping function takes any number implementing the <tt>Fractional</tt> typeclass, which in our example also includes exact <tt>Rational</tt> numbers.

```haskell
import Data.Ratio
import Text.Printf (PrintfType, printf)

-- Map a value from the range [a1,a2] to the range [b1,b2].  We don't check
-- for empty ranges.
mapRange
  :: Fractional a
  => (a, a) -> (a, a) -> a -> a
mapRange (a1, a2) (b1, b2) s = b1 + (s - a1) * (b2 - b1) / (a2 - a1)

main :: IO ()
main
-- Perform the mapping over floating point numbers.
 = do
  putStrLn "---------- Floating point ----------"
  mapM_ (\n -> prtD n . mapRange (0, 10) (-1, 0) $ fromIntegral n) [0 .. 10]
  -- Perform the same mapping over exact rationals.
  putStrLn "---------- Rationals ----------"
  mapM_ (\n -> prtR n . mapRange (0, 10) (-1, 0) $ n % 1) [0 .. 10]
  where
    prtD
      :: PrintfType r
      => Integer -> Double -> r
    prtD = printf "%2d -> %6.3f\n"
    prtR
      :: PrintfType r
      => Integer -> Rational -> r
    prtR n x = printf "%2d -> %s\n" n (show x)
```

{{out}}

```txt
---------- Floating point ----------
 0 -> -1.000
 1 -> -0.900
 2 -> -0.800
 3 -> -0.700
 4 -> -0.600
 5 -> -0.500
 6 -> -0.400
 7 -> -0.300
 8 -> -0.200
 9 -> -0.100
10 ->  0.000
---------- Rationals ----------
 0 -> (-1) % 1
 1 -> (-9) % 10
 2 -> (-4) % 5
 3 -> (-7) % 10
 4 -> (-3) % 5
 5 -> (-1) % 2
 6 -> (-2) % 5
 7 -> (-3) % 10
 8 -> (-1) % 5
 9 -> (-1) % 10
10 -> 0 % 1
```


=={{header|Icon}} and {{header|Unicon}}==


```Unicon

record Range(a, b)

# note, we force 'n' to be real, which means recalculation will
# be using real numbers, not integers
procedure remap (range1, range2, n : real)
  if n < range2.a | n > range2.b then fail # n out of given range
  return range1.a + (n - range2.a) * (range1.b - range1.a) / (range2.b - range2.a)
end

procedure range_string (range)
  return "[" || range.a || ", " || range.b || "]"
end

procedure main ()
  range1 := Range (0, 10)
  range2 := Range (-1, 0)
  # if i is out of range1, then 'remap' fails, so only valid changes are written
  every i := -2 to 12 do {
    if m := remap (range2, range1, i)
      then write ("Value " || i || " in " || range_string (range1) ||
                  " maps to " || m || " in " || range_string (range2))
  }
end

```


Icon does not permit the type declaration, as Unicon does.  For Icon, replace 'remap' with:


```Icon

procedure remap (range1, range2, n)
  n *:= 1.0
  if n < range2.a | n > range2.b then fail # n out of given range
  return range1.a + (n - range2.a) * (range1.b - range1.a) / (range2.b - range2.a)
end

```


{{out}}

```txt

Value 0 in [0, 10] maps to -1.0 in [-1, 0]
Value 1 in [0, 10] maps to -0.9 in [-1, 0]
Value 2 in [0, 10] maps to -0.8 in [-1, 0]
Value 3 in [0, 10] maps to -0.7 in [-1, 0]
Value 4 in [0, 10] maps to -0.6 in [-1, 0]
Value 5 in [0, 10] maps to -0.5 in [-1, 0]
Value 6 in [0, 10] maps to -0.4 in [-1, 0]
Value 7 in [0, 10] maps to -0.3 in [-1, 0]
Value 8 in [0, 10] maps to -0.2 in [-1, 0]
Value 9 in [0, 10] maps to -0.1 in [-1, 0]
Value 10 in [0, 10] maps to 0.0 in [-1, 0]

```



## J



```j
maprange=:2 :0
  'a1 a2'=.m
  'b1 b2'=.n
  b1+((y-a1)*b2-b1)%a2-a1
)
NB. this version defers all calculations to runtime, but mirrors exactly the task formulation
```


Or


```j
maprange=:2 :0
  'a1 a2'=.m
  'b1 b2'=.n
  b1 + ((b2-b1)%a2-a1) * -&a1
)
NB. this version precomputes the scaling ratio
```


Example use:


```j
   2 4 maprange 5 11 (2.718282 3 3.141592)
7.15485 8 8.42478
```


or


```j
   adjust=:2 4 maprange 5 11 NB. save the derived function as a named entity
   adjust 2.718282 3 3.141592
7.15485 8 8.42478
```


Required example:


```j
   0 10 maprange _1 0 i.11
_1 _0.9 _0.8 _0.7 _0.6 _0.5 _0.4 _0.3 _0.2 _0.1 0
```


## Java


```java
public class Range {
	public static void main(String[] args){
		for(float s = 0;s <= 10; s++){
			System.out.println(s + " in [0, 10] maps to "+
					mapRange(0, 10, -1, 0, s)+" in [-1, 0].");
		}
	}

	public static double mapRange(double a1, double a2, double b1, double b2, double s){
		return b1 + ((s - a1)*(b2 - b1))/(a2 - a1);
	}
}
```

{{out}}

```txt
0.0 in [0, 10] maps to -1.0 in [-1, 0].
1.0 in [0, 10] maps to -0.9 in [-1, 0].
2.0 in [0, 10] maps to -0.8 in [-1, 0].
3.0 in [0, 10] maps to -0.7 in [-1, 0].
4.0 in [0, 10] maps to -0.6 in [-1, 0].
5.0 in [0, 10] maps to -0.5 in [-1, 0].
6.0 in [0, 10] maps to -0.4 in [-1, 0].
7.0 in [0, 10] maps to -0.30000000000000004 in [-1, 0].
8.0 in [0, 10] maps to -0.19999999999999996 in [-1, 0].
9.0 in [0, 10] maps to -0.09999999999999998 in [-1, 0].
10.0 in [0, 10] maps to 0.0 in [-1, 0].
```

The differences in 7, 8, and 9 come from double math. Similar issues show even when using float types.

## JavaScript


### ES5


```JavaScript
// Javascript doesn't have built-in support for ranges
// Insted we use arrays of two elements to represent ranges
var mapRange = function(from, to, s) {
  return to[0] + (s - from[0]) * (to[1] - to[0]) / (from[1] - from[0]);
};

var range = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
for (var i = 0; i < range.length; i++) {
  range[i] = mapRange([0, 10], [-1, 0], range[i]);
}

console.log(range);
```

{{out}}

```txt
[-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.30000000000000004, -0.19999999999999996, -0.09999999999999998, 0]
```



### =Extra credit=

Here we will use the [https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array/map ECMAScript 5 support for map] and the [http://underscorejs.org/#range _.range] function from Underscore.js.
{{libheader|Underscore.js}}

```JavaScript
var mapRange = function(from, to, s) {
  // mapRange expects ranges generated by _.range
  var a1 = from[0];
  var a2 = from[from.length - 1];
  var b1 = to[0];
  var b2 = to[to.length - 1];
  return b1 + (s - a1) * (b2 - b1) / (a2 - a1);
};

// The range function is exclusive
var fromRange = _.range(0, 11);
var toRange = _.range(-1, 1);

// .map constructs a new array
fromRange = fromRange.map(function(s) {
  return mapRange(fromRange, toRange, s);
});

console.log(fromRange);
```

{{out}}

```txt
[-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.30000000000000004, -0.19999999999999996, -0.09999999999999998, 0]
```



### ES6


Composing a solution from generic abstractions:

```javascript
(() => {
  'use strict';

  // main :: IO ()
  const main = () => {

    // rangeMap :: (Num, Num) -> (Num, Num) -> Num -> Num
    const rangeMap = (a, b) => s => {
      const [a1, a2] = a;
      const [b1, b2] = b;
      // Scaling up an order, and then down, to bypass a potential,
      // precision issue with negative numbers.
      return (((((b2 - b1) * (s - a1)) / (a2 - a1)) * 10) + (10 * b1)) / 10;
    };

    const
      mapping = rangeMap([0, 10], [-1, 0]),
      xs = enumFromTo(0, 10),
      ys = map(mapping, xs),
      zs = map(approxRatio(''), ys);


    const formatted = (x, m, r) => {
      const
        fract = showRatio(r),
        [n, d] = splitOn('/', fract);
      return justifyRight(2, ' ', x.toString()) + '  ->  ' +
        justifyRight(4, ' ', m.toString()) + '   =  ' +
        justifyRight(2, ' ', n.toString()) + '/' + d.toString();
    };

    console.log(
      unlines(zipWith3(formatted, xs, ys, zs))
    );
  };


  // GENERIC FUNCTIONS ----------------------------

  // abs :: Num -> Num
  const abs = Math.abs;

  // Epsilon - > Real - > Ratio
  // approxRatio :: Real -> Real -> Ratio
  const approxRatio = eps => n => {
    const
      gcde = (e, x, y) => {
        const _gcd = (a, b) => (b < e ? a : _gcd(b, a % b));
        return _gcd(abs(x), abs(y));
      },
      c = gcde(Boolean(eps) ? eps : (1 / 10000), 1, abs(n)),
      r = ratio(quot(abs(n), c), quot(1, c));
    return {
      type: 'Ratio',
      n: r.n * signum(n),
      d: r.d
    };
  };

  // enumFromTo :: Int -> Int -> [Int]
  const enumFromTo = (m, n) =>
    Array.from({
      length: 1 + n - m
    }, (_, i) => m + i)

  // gcd :: Int -> Int -> Int
  const gcd = (x, y) => {
    const
      _gcd = (a, b) => (0 === b ? a : _gcd(b, a % b)),
      abs = Math.abs;
    return _gcd(abs(x), abs(y));
  };

  // justifyRight :: Int -> Char -> String -> String
  const justifyRight = (n, cFiller, s) =>
    n > s.length ? (
      s.padStart(n, cFiller)
    ) : s;

  // Returns Infinity over objects without finite length
  // this enables zip and zipWith to choose the shorter
  // argument when one is non-finite, like cycle, repeat etc

  // length :: [a] -> Int
  const length = xs => Array.isArray(xs) ? xs.length : Infinity;

  // map :: (a -> b) -> [a] -> [b]
  const map = (f, xs) => xs.map(f);

  // quot :: Int -> Int -> Int
  const quot = (n, m) => Math.floor(n / m);

  // ratio :: Int -> Int -> Ratio Int
  const ratio = (x, y) => {
    const go = (x, y) =>
      0 !== y ? (() => {
        const d = gcd(x, y);
        return {
          type: 'Ratio',
          'n': quot(x, d), // numerator
          'd': quot(y, d) // denominator
        };
      })() : undefined;
    return go(x * signum(y), abs(y));
  };

  // showRatio :: Ratio -> String
  const showRatio = nd =>
    nd.n.toString() + '/' + nd.d.toString();

  // signum :: Num -> Num
  const signum = n => 0 > n ? -1 : (0 < n ? 1 : 0);

  // splitOn :: String -> String -> [String]
  const splitOn = (pat, src) =>
    src.split(pat);

  // unlines :: [String] -> String
  const unlines = xs => xs.join('\n');

  // zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
  const zipWith3 = (f, xs, ys, zs) =>
    Array.from({
      length: Math.min(length(xs), length(ys), length(zs))
    }, (_, i) => f(xs[i], ys[i], zs[i]));

  // MAIN ---
  return main();
})();
```

{{Out}}

```txt
 0  ->    -1   =  -1/1
 1  ->  -0.9   =  -9/10
 2  ->  -0.8   =  -4/5
 3  ->  -0.7   =  -7/10
 4  ->  -0.6   =  -3/5
 5  ->  -0.5   =  -1/2
 6  ->  -0.4   =  -2/5
 7  ->  -0.3   =  -3/10
 8  ->  -0.2   =  -1/5
 9  ->  -0.1   =  -1/10
10  ->     0   =   0/1
```



## jq

In jq, it is generally preferable to define functions as parameterized filters.  In the present case,
since the task calls for defining a map, the signature maprange(a;b), where a and b are the two ranges, is appropriate.

```jq
# The input is the value to be mapped.
# The ranges, a and b, should each be an array defining the
# left-most and right-most points of the range.
def maprange(a; b):
  b[0] + (((. - a[0]) * (b[1] - b[0])) / (a[1] - a[0])) ;
```

'''Example 1''': a single value
 6 | maprange([0,10]; [-1, 0])
produces:
  -0.4

'''Example 2''': a stream of values

```jq
range(0;11) | maprange([0,10]; [-1, 0])
```

produces:
 -1
 -0.9
 -0.8
 -0.7
 -0.6
 -0.5
 -0.4
 -0.30000000000000004
 -0.19999999999999996
 -0.09999999999999998
 0

### =Extra credit=

To avoid repeating the same arithmetic, we shall define a filter that handles an array of values all at once, using an inner function and map/1:

```jq
def maprange_array(a; b):
  def _helper(a0; b0; factor): b0 + (. - a0) * factor;

  a[0] as $a | b[0] as $b | ((b[1] - b[0]) / (a[1] - a[0])) as $factor
  | map(_helper( $a; $b; $factor) );
```

'''Example''':
 [range(0;11)] | maprange_array([0,10]; [-1, 0])


## Julia

{{works with|Julia|0.6}}


```julia
function maprange(s, a, b)
    a₁, a₂ = minimum(a), maximum(a)
    b₁, b₂ = minimum(b), maximum(b)
    return b₁ + (s - a₁) * (b₂ - b₁) / (a₂ - a₁)
end

@show maprange(6, 1:10, -1:0)
@show maprange(0:10, 0:10, -1:0)
```


{{out}}

```txt
maprange(6, 1:10, -1:0) = -0.4444444444444444
maprange(0:10, 0:10, -1:0) = -1.0:0.1:0.0
```



## K


```K
   f:{[a1;a2;b1;b2;s] b1+(s-a1)*(b2-b1)%(a2-a1)}

   +(a; f[0;10;-1;0]'a:!11)
((0;-1.0)
 (1;-0.9)
 (2;-0.8)
 (3;-0.7)
 (4;-0.6)
 (5;-0.5)
 (6;-0.4)
 (7;-0.3)
 (8;-0.2)
 (9;-0.1)
 (10;0.0))
```



## Kotlin


```scala
// version 1.0.6

class FloatRange(override val start: Float, override val endInclusive: Float) : ClosedRange<Float>

fun mapRange(range1: FloatRange, range2: FloatRange, value: Float): Float {
    if (value !in range1) throw IllegalArgumentException("value is not within the first range")
    if (range1.endInclusive == range1.start) throw IllegalArgumentException("first range cannot be single-valued")
    return range2.start + (value - range1.start) * (range2.endInclusive - range2.start) / (range1.endInclusive - range1.start)
}

fun main(args: Array<String>) {
    for (i in 0..10) {
        val mappedValue = mapRange(FloatRange(0.0f, 10.0f), FloatRange(-1.0f, 0.0f), i.toFloat())
        println(String.format("%2d  maps to %+4.2f", i, mappedValue))
    }
}
```


{{out}}

```txt

 0  maps to -1.00
 1  maps to -0.90
 2  maps to -0.80
 3  maps to -0.70
 4  maps to -0.60
 5  maps to -0.50
 6  maps to -0.40
 7  maps to -0.30
 8  maps to -0.20
 9  maps to -0.10
10  maps to +0.00

```



## Lasso


```Lasso
define map_range(
	a1,
	a2,
	b1,
	b2,
	number
) => (decimal(#b1) + (decimal(#number) - decimal(#a1)) * (decimal(#b2) - decimal(#b1)) / (decimal(#a2) - decimal(#a1))) -> asstring(-Precision = 1)

with number in generateSeries(1,10) do {^
	#number
	': '
	map_range( 0, 10, -1, 0, #number)
	'<br />'

^}'
```

{{out}}

```txt
0: -1.0
1: -0.9
2: -0.8
3: -0.7
4: -0.6
5: -0.5
6: -0.4
7: -0.3
8: -0.2
9: -0.1
10: 0.0
```



## Logo


```logo
to interpolate :s :a1 :a2 :b1 :b2
  output (:s-:a1) / (:a2-:a1) * (:b2-:b1) + :b1
end

for [i 0 10] [print interpolate :i 0 10 -1 0]
```



## Lua


```lua
function map_range( a1, a2, b1, b2, s )
    return b1 + (s-a1)*(b2-b1)/(a2-a1)
end

for i = 0, 10 do
    print( string.format( "f(%d) = %f", i, map_range( 0, 10, -1, 0, i ) ) )
end
```



## Maple


```Maple

Map:=proc(a1,a2,b1,b2,s);
	return (b1+((s-a1)*(b2-b1)/(a2-a1)));
end proc;

for i from 0 to 10 do
	printf("%a  maps to ",i);
	printf("%a\n",Map(0,10,-1,0,i));
end do;

```




## Mathematica

Such a function is already built in

```Mathematica

Rescale[#,{0,10},{-1,0}]&/@Range[0,10]

```


{{out}}
{-1., -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0.}


## Maxima


```maxima
maprange(a, b, c, d) := buildq([e: ratsimp(('x - a)*(d - c)/(b - a) + c)],
   lambda([x], e))$

f: maprange(0, 10, -1, 0);
```



## Nemerle


```Nemerle
using System;
using System.Console;

module Maprange
{
    Maprange(a : double * double, b : double * double, s : double) : double
    {
        def (a1, a2) = a; def (b1, b2) = b;

        b1 + (((s - a1) * (b2 - b1))/(a2 - a1))
    }

    Main() : void
    {
        foreach (i in [0 .. 10])
            WriteLine("{0, 2:f0} maps to {1:f1}", i, Maprange((0.0, 10.0), (-1.0, 0.0), i));
    }
}
```



## NetRexx


```netrexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

A    = [  0.0, 10.0 ]
B    = [ -1.0,  0.0 ]
incr = 1.0

say 'Mapping ['A[0]',' A[1]'] to ['B[0]',' B[1]'] in increments of' incr':'
loop sVal = A[0] to A[1] by incr
  say '  f('sVal.format(3, 3)') ='  mapRange(A, B, sVal).format(4, 3)
  end sVal

return

method mapRange(a = Rexx[], b = Rexx[], s_) public static
  return mapRange(a[0], a[1], b[0], b[1], s_)

method mapRange(a1, a2, b1, b2, s_) public static
  t_ = b1 + ((s_ - a1) * (b2 - b1) / (a2 - a1))
  return t_

```

{{out}}

```txt

Mapping [0.0, 10.0] to [-1.0, 0.0] in increments of 1.0:
  f(  0.000) =   -1.000
  f(  1.000) =   -0.900
  f(  2.000) =   -0.800
  f(  3.000) =   -0.700
  f(  4.000) =   -0.600
  f(  5.000) =   -0.500
  f(  6.000) =   -0.400
  f(  7.000) =   -0.300
  f(  8.000) =   -0.200
  f(  9.000) =   -0.100
  f( 10.000) =    0.000

```



## Nim


{{trans|Python}}


```nim
import strutils

type FloatRange = tuple[s,e: float]

proc mapRange(a,b: FloatRange, s): float =
  b.s + (s - a.s) * (b.e - b.s) / (a.e - a.s)

for i in 0..10:
  let m = mapRange((0.0,10.0), (-1.0, 0.0), float(i))
  echo i, " maps to ", formatFloat(m, precision = 0)
```


{{out}}

```txt
0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.3
8 maps to -0.2
9 maps to -0.1
10 maps to 0
```



## Objeck


```objeck

bundle Default {
  class Range {
    function : MapRange(a1:Float, a2:Float, b1:Float, b2:Float, s:Float) ~ Float {
      return b1 + (s-a1)*(b2-b1)/(a2-a1);
    }

    function : Main(args : String[]) ~ Nil {
      "Mapping [0,10] to [-1,0] at intervals of 1:"->PrintLine();
      for(i := 0.0; i <= 10.0; i += 1;) {
        IO.Console->Print("f(")->Print(i->As(Int))->Print(") = ")->PrintLine(MapRange(0.0, 10.0, -1.0, 0.0, i));
      };
    }
  }
}

```


{{out}}

```txt

Mapping [0,10] to [-1,0] at intervals of 1:
f(0) = -1
f(1) = -0.9
f(2) = -0.8
f(3) = -0.7
f(4) = -0.6
f(5) = -0.5
f(6) = -0.4
f(7) = -0.3
f(8) = -0.2
f(9) = -0.1
f(10) = 0

```



## OCaml



```ocaml
let map_range (a1, a2) (b1, b2) s =
  b1 +. ((s -. a1) *. (b2 -. b1) /. (a2 -. a1))

let () =
  print_endline "Mapping [0,10] to [-1,0] at intervals of 1:";
  for i = 0 to 10 do
    Printf.printf "f(%d) = %g\n" i (map_range (0.0, 10.0) (-1.0, 0.0) (float i))
  done
```


{{out}}

```txt
Mapping [0,10] to [-1,0] at intervals of 1:
f(0) = -1
f(1) = -0.9
f(2) = -0.8
f(3) = -0.7
f(4) = -0.6
f(5) = -0.5
f(6) = -0.4
f(7) = -0.3
f(8) = -0.2
f(9) = -0.1
f(10) = 0
```


If range mapping is used in a heavy computational task we can reduce the number of calculations made using partial application and [[currying]]:


```ocaml
let map_range (a1, a2) (b1, b2) =
  let v = (b2 -. b1) /. (a2 -. a1) in
  function s ->
    b1 +. ((s -. a1) *. v)

let () =
  print_endline "Mapping [0,10] to [-1,0] at intervals of 1:";
  let p = (map_range (0.0, 10.0) (-1.0, 0.0)) in
  for i = 0 to 10 do
    Printf.printf "f(%d) = %g\n" i (p (float i))
  done
```




## Oforth



```Oforth
: mapRange(p1, p2, s)
   s p1 first - p2 second p2 first - * p1 second p1 first - asFloat /
   p2 first + ;
```


{{out}}

```txt

Interval newFromToStep(0, 10, 0.5) map(#[ mapRange([0, 10], [ -1, 0 ])]) println
[-1, -0.95, -0.9, -0.85, -0.8, -0.75, -0.7, -0.65, -0.6, -0.55, -0.5, -0.45, -0.4, -0.35,
-0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0]

```



## PARI/GP

Usage (e.g.): map([1,10],[0,5],8.)

```parigp
map(r1,r2,x)=r2[1]+(x-r1[1])*(r2[2]-r2[1])/(r1[2]-r1[1])
```



## Pascal


```pascal
Program Map(output);

function MapRange(fromRange, toRange: array of real; value: real): real;
  begin
    MapRange := (value-fromRange[0]) * (toRange[1]-toRange[0]) / (fromRange[1]-fromRange[0]) + toRange[0];
  end;

var
  i: integer;
begin
  for i := 0 to 10 do
    writeln (i, ' maps to: ', MapRange([0.0, 10.0], [-1.0, 0.0], i):4:2);
end.
```

{{out}}

```txt
:> ./MapRange
0 maps to: -1.00
1 maps to: -0.90
2 maps to: -0.80
3 maps to: -0.70
4 maps to: -0.60
5 maps to: -0.50
6 maps to: -0.40
7 maps to: -0.30
8 maps to: -0.20
9 maps to: -0.10
10 maps to: 0.00

```


### improvement doing many calculations

Tested with freepascal_32  2.6.4 .Pushing all data over the stack takes quite a long time.
Precaltulating the scalefactor helps too.

Time relation doing 1E7 calculations

Org/ const / tMr

double  : 267/177/107 .. 25/16/10

extended: 363/193/123 .. 30/15/10

Output as above.

```pascal
Program Map(output);

type
  real = double;
  tRange = Array [0..1] of real;
  tMapRec = record
              mrFrom,
              mrTo : tRange;
              mrScale : real
            end;

function InitRange(rfrom,rTo:real):tRange;
begin
  InitRange[0] :=rfrom;
  InitRange[1] :=rTo;
end;

function InitMapRec(const fromRange, toRange: tRange):tMapRec;
begin
  With InitMapRec do
  Begin
    mrFrom := fromRange;
    mrTo   := toRange;
    mrScale := (toRange[1]-toRange[0]) / (fromRange[1]-fromRange[0]);
  end;
end;

function MapRecRange(const value: real;var MR :tMapRec): real;
begin
  with MR do
    MapRecRange := (value-mrFrom[0]) * mrScale + mrTo[0];
end;

function MapRange(const value: real;const fromRange, toRange: tRange): real;
begin
  MapRange := (value-fromRange[0]) * (toRange[1]-toRange[0]) / (fromRange[1]-fromRange[0]) + toRange[0];
end;

var
  value:real;
  rFrom,rTo : tRange;
  mr : tMapRec;
  i: LongInt;

begin
  rFrom:= InitRange(  0, 10);
  rTo  := InitRange( -1,  0);
  mr:= InitMapRec(rFrom,rTo);

  for i := 0 to 10 do
  Begin
    value := i;
    writeln (i:4, ' maps to: ', MapRange(value,rFrom, rTo):10:6,
                                  MapRecRange(value,mr):10:6);
  end;
end.
```



## Perl


```Perl
#!/usr/bin/perl -w
use strict ;

sub mapValue {
   my ( $range1 , $range2 , $number ) = @_ ;
   return ( $range2->[ 0 ] +
      (( $number - $range1->[ 0 ] ) * ( $range2->[ 1 ] - $range2->[ 0 ] ) ) / ( $range1->[ -1 ]
      - $range1->[ 0 ] ) ) ;
}
my @numbers = 0..10 ;
my @interval = ( -1 , 0 ) ;
print "The mapped value for $_ is " . mapValue( \@numbers , \@interval , $_ ) . " !\n" foreach @numbers ;

```

{{out}}
<PRE>The mapped value for 0 is -1 !
The mapped value for 1 is -0.9 !
The mapped value for 2 is -0.8 !
The mapped value for 3 is -0.7 !
The mapped value for 4 is -0.6 !
The mapped value for 5 is -0.5 !
The mapped value for 6 is -0.4 !
The mapped value for 7 is -0.3 !
The mapped value for 8 is -0.2 !
The mapped value for 9 is -0.1 !
The mapped value for 10 is 0 !
</PRE>


## Perl 6

{{works with|rakudo|2015-09-18}}

```perl6
sub the_function(Range $a, Range $b, $s) {
  my ($a1, $a2) = $a.bounds;
  my ($b1, $b2) = $b.bounds;
  return $b1 + (($s-$a1) * ($b2-$b1) / ($a2-$a1));
}

for ^11 -> $x { say "$x maps to {the_function(0..10, -1..0, $x)}" }
```



```txt
%perl6 map_range.p6
0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.3
8 maps to -0.2
9 maps to -0.1
10 maps to 0
```

A more idiomatic way would be to return a closure that does the mapping without have to supply the ranges every time:

```perl6
sub getmapper(Range $a, Range  $b) {
  my ($a1, $a2) = $a.bounds;
  my ($b1, $b2) = $b.bounds;
  return -> $s { $b1 + (($s-$a1) * ($b2-$b1) / ($a2-$a1)) }
}

my &mapper = getmapper(0 .. 10, -1 .. 0);
for ^11 -> $x {say "$x maps to &mapper($x)"}
```



## Phix


```Phix
function MapRange(atom s, a1, a2, b1, b2)
    return b1+(s-a1)*(b2-b1)/(a2-a1)
end function

for i=0 to 10 by 2 do
    printf(1,"%2d : %g\n",{i,MapRange(i,0,10,-1,0)})
end for
```

{{out}}

```txt

 0 : -1
 2 : -0.8
 4 : -0.6
 6 : -0.4
 8 : -0.2
10 : 0

```



## PicoLisp


```PicoLisp
(scl 1)

(de mapRange (Val A1 A2 B1 B2)
   (+ B1 (*/ (- Val A1) (- B2 B1) (- A2 A1))) )


(for Val (range 0 10.0 1.0)
   (prinl
      (format (mapRange Val 0 10.0 -1.0 0) *Scl) ) )
```

{{out}}

```txt
-1.0
-0.9
-0.8
-0.7
-0.6
-0.5
-0.4
-0.3
-0.2
-0.1
0.0
```



## PL/I


```pli

map: procedure options (main); /* 24/11/2011 */
   declare (a1, a2, b1, b2) float;
   declare d fixed decimal (3,1);

   do d = 0 to 10 by 0.9, 10;
      put skip edit ( d, ' maps to ', map(0, 10, -1, 0, d) ) (f(5,1), a, f(10,6));
   end;

map: procedure (a1, a2, b1, b2, s) returns (float);
   declare (a1, a2, b1, b2, s) float;
   return (b1 + (s - a1)*(b2 - b1) / (a2 - a1) );
end map;
end map;

```


{{out}}

```txt

  0.0 maps to  -1.000000
  0.9 maps to  -0.910000
  1.8 maps to  -0.820000
  2.7 maps to  -0.730000
  3.6 maps to  -0.640000
  4.5 maps to  -0.550000
  5.4 maps to  -0.460000
  6.3 maps to  -0.370000
  7.2 maps to  -0.280000
  8.1 maps to  -0.190000
  9.0 maps to  -0.100000
  9.9 maps to  -0.010000
 10.0 maps to   0.000000
```



## PowerShell


```PowerShell

function Group-Range
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   Position=0)]
        [ValidateCount(2,2)]
        [double[]]
        $Range1,

        [Parameter(Mandatory=$true,
                   Position=1)]
        [ValidateCount(2,2)]
        [double[]]
        $Range2,

        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   Position=2)]
        [double]
        $Map
    )

    Process
    {
        foreach ($number in $Map)
        {
            [PSCustomObject]@{
                Index   = $number
                Mapping = $Range2[0] + ($number - $Range1[0]) * ($Range2[0] - $Range2[1]) / ($Range1[0] - $Range1[1])
            }
        }
    }
}

```


```PowerShell

0..10 | Group-Range (0,10) (-1,0)

```

{{Out}}

```txt

Index Mapping
----- -------
    0      -1
    1    -0.9
    2    -0.8
    3    -0.7
    4    -0.6
    5    -0.5
    6    -0.4
    7    -0.3
    8    -0.2
    9    -0.1
   10       0

```



## PureBasic


```purebasic
Structure RR
  a.f
  b.f
EndStructure

Procedure.f MapRange(*a.RR, *b.RR, s)
  Protected.f a1, a2, b1, b2
  a1=*a\a:  a2=*a\b
  b1=*b\a:  b2=*b\b
  ProcedureReturn b1 + ((s - a1) * (b2 - b1) / (a2 - a1))
EndProcedure


;- Test the function
If OpenConsole()
  Define.RR Range1, Range2
  Range1\a=0: Range1\b=10
  Range2\a=-1:Range2\b=0
  ;
  For i=0 To 10
    PrintN(RSet(Str(i),2)+" maps to "+StrF(MapRange(@Range1, @Range2, i),1))
  Next
EndIf
```


```txt
 0 maps to -1.0
 1 maps to -0.9
 2 maps to -0.8
 3 maps to -0.7
 4 maps to -0.6
 5 maps to -0.5
 6 maps to -0.4
 7 maps to -0.3
 8 maps to -0.2
 9 maps to -0.1
10 maps to 0.0
```



## Python


```python
>>>
 def maprange( a, b, s):
	(a1, a2), (b1, b2) = a, b
	return  b1 + ((s - a1) * (b2 - b1) / (a2 - a1))

>>> for s in range(11):
	print("%2g maps to %g" % (s, maprange( (0, 10), (-1, 0), s)))


 0 maps to -1
 1 maps to -0.9
 2 maps to -0.8
 3 maps to -0.7
 4 maps to -0.6
 5 maps to -0.5
 6 maps to -0.4
 7 maps to -0.3
 8 maps to -0.2
 9 maps to -0.1
10 maps to 0
```


Because of Pythons  strict, dynamic, typing rules for numbers the same function can give answers as fractions:

```python
>>>
 from fractions import Fraction
>>> for s in range(11):
	print("%2g maps to %s" % (s, maprange( (0, 10), (-1, 0), Fraction(s))))


 0 maps to -1
 1 maps to -9/10
 2 maps to -4/5
 3 maps to -7/10
 4 maps to -3/5
 5 maps to -1/2
 6 maps to -2/5
 7 maps to -3/10
 8 maps to -1/5
 9 maps to -1/10
10 maps to 0
>>>
```



## Racket



```Racket

#lang racket

(define (make-range-map a1 a2 b1 b2)
  ;; returns a mapping function, doing computing the differences in
  ;; advance so it's fast
  (let ([a (- a2 a1)] [b (- b2 b1)])
    (λ(s) (exact->inexact (+ b1 (/ (* (- s a1) b) a))))))

(define map (make-range-map 0 10 -1 0))
(for ([i (in-range 0 11)]) (printf "~a --> ~a\n" i (map i)))

```


{{out}}

```txt

0 --> -1.0
1 --> -0.9
2 --> -0.8
3 --> -0.7
4 --> -0.6
5 --> -0.5
6 --> -0.4
7 --> -0.3
8 --> -0.2
9 --> -0.1
10 --> 0.0

```



## REXX

(The first three REXX versions don't differ idiomatically that much, but differ mostly just in style.)

The first three versions support different increments   (the   '''inc'''   variable)   and an   '''A'''   range that is decreasing in values

(that is, the 2nd number [usually the high] in the range is less than the first number in the range [usually the low]).   Also,

the   '''BY'''   (increment)   is automatically adjusted   (either   ''upwards''   or   ''downwards'').   Also,
both sets of numbers in the

output are aligned  (vertically).


### version 1


```rexx
/*REXX program maps and displays a  range of numbers from  one range  to  another range.*/
rangeA =   0   10                                      /*or:   rangeA =   '  0  10 '    */
rangeB =  -1    0                                      /*or:   rangeB =   " -1   0 "    */
parse  var   rangeA  L  H
inc= 1
          do j=L  to H  by inc * (1 - 2 * sign(H<L) )  /*BY:   either   +inc  or  -inc  */
          say right(j, 9)      ' maps to '      mapR(rangeA, rangeB, j)
          end   /*j*/
exit                                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
mapR: procedure; parse arg a1 a2,b1 b2,s;$=b1+(s-a1)*(b2-b1)/(a2-a1);return left('',$>=0)$
```

{{out|output}}

```txt

        0  maps to  -1
        1  maps to  -0.9
        2  maps to  -0.8
        3  maps to  -0.7
        4  maps to  -0.6
        5  maps to  -0.5
        6  maps to  -0.4
        7  maps to  -0.3
        8  maps to  -0.2
        9  maps to  -0.1
       10  maps to   0

```



### version 2

This version demonstrates an increment ('''inc''') of   '''1/2'''   instead of the usual unity.

Note that this REXX version also uses a different   '''rangeA'''   numbers   (they are reversed).

```rexx
/*REXX program maps and displays a  range of numbers from  one range  to  another range.*/
rangeA =  10   0                                       /*or:   rangeA =   '  0  10 '    */
rangeB =  -1   0                                       /*or:   rangeB =   " -1   0 "    */
parse  var   rangeA  L  H
inc= 1/2
          do j=L  to H  by inc * (1 - 2 * sign(H<L) )  /*BY:   either   +inc  or  -inc  */
          say right(j, 9)      ' maps to '      mapR(rangeA, rangeB, j)
          end   /*j*/
exit                                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
mapR: procedure; parse arg a1 a2,b1 b2,s;$=b1+(s-a1)*(b2-b1)/(a2-a1);return left('',$>=0)$
```

{{out|output}}

```txt

        0  maps to   0
      0.5  maps to  -0.05
      1.0  maps to  -0.1
      1.5  maps to  -0.15
      2.0  maps to  -0.2
      2.5  maps to  -0.25
      3.0  maps to  -0.3
      3.5  maps to  -0.35
      4.0  maps to  -0.4
      4.5  maps to  -0.45
      5.0  maps to  -0.5
      5.5  maps to  -0.55
      6.0  maps to  -0.6
      6.5  maps to  -0.65
      7.0  maps to  -0.7
      7.5  maps to  -0.75
      8.0  maps to  -0.8
      8.5  maps to  -0.85
      9.0  maps to  -0.9
      9.5  maps to  -0.95
     10.0  maps to  -1

```



### version 3

This REXX version used a function that calculates and also displays the range mapping.

```rexx
/*REXX program maps and displays a  range of numbers from  one range  to  another range.*/
rangeA =   0   10
rangeB =  -1    0
inc = 1
call mapR rangeA, rangeB, inc
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
mapR: procedure;  parse arg a1 a2, b1 b2, inc    /* [↓]  BY  is  either   +inc  or -inc.*/
                     do s=a1   to a2   by  inc  *  (1  -  2  *  sign(a2 < a1) )
                     t= b1 + (s-a1) * (b2-b1) / (a2-a1)
                     say right(s, 9)        ' maps to'          left('', t>=0)      t
                     end   /*s*/
      return                                     /* [↑]  LEFT··· aligns non─negative #'s*/
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




### Version 4


```rexx
/*REXX program maps a number from one range to another range.           */
/* 31.10.2013 Walter Pachl   */
/*                  'translated' from an older version 1 without using Procedure */
  do j=0  to 10
    say right(j,3)   ' maps to '   mapRange(0,10,-1,0,j)
    end
exit
/*──────────────────────────────────MAPRANGE subroutine─────────────────*/
mapRange: return arg(3)+(arg(5)-arg(1))*(arg(4)-arg(3))/(arg(2)-arg(1))
/* Arguments are arg a1,a2,b1,b2,x */
```

{{out}}

```txt

  0  maps to  -1
  1  maps to  -0.9
  2  maps to  -0.8
  3  maps to  -0.7
  4  maps to  -0.6
  5  maps to  -0.5
  6  maps to  -0.4
  7  maps to  -0.3
  8  maps to  -0.2
  9  maps to  -0.1
 10  maps to  0

```



## Ring


```ring

# Project : Map range

decimals(1)
al = 0
ah = 10
bl = -1
bh = 0
for n = 0 to 10
     see "" + n + " maps to " + maprange(al, bl, n) + nl
next

func maprange(al, bl, s)
       return bl + (s - al) * (bh - bl) / (ah - al)

```

Output:

```txt

0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.3
8 maps to -0.2
9 maps to -0.1
10 maps to 0

```



## Ruby


```ruby
def map_range(a, b, s)
  af, al, bf, bl = a.first, a.last, b.first, b.last
  bf + (s - af)*(bl - bf).quo(al - af)
end

(0..10).each{|s| puts "%s maps to %g" % [s, map_range(0..10, -1..0, s)]}
```


Numeric#quo does floating point division.
{{out}}

```txt

0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.3
8 maps to -0.2
9 maps to -0.1
10 maps to 0

```


To use rational arithmetic, delete <code>s *= 1.0</code> and either <code>require 'rational'</code>, or use Ruby 1.9 (which has Rational in the core library).

```ruby
(0..10).each do |s|
  puts "%s maps to %s" % [s, map_range(0..10, -1..0, s)]
end
```


{{out}} using rational arithmetic:

```txt

0 maps to -1/1
1 maps to -9/10
2 maps to -4/5
3 maps to -7/10
4 maps to -3/5
5 maps to -1/2
6 maps to -2/5
7 maps to -3/10
8 maps to -1/5
9 maps to -1/10
10 maps to 0/1

```



## Rust


```rust
use std::f64;

fn map_range(from_range: (f64, f64), to_range: (f64, f64), s: f64) -> f64 {
    to_range.0 + (s - from_range.0) * (to_range.1 - to_range.0) / (from_range.1 - from_range.0)
}

fn main() {
    let input: Vec<f64> = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
    let result = input.into_iter()
        .map(|x| map_range((0.0, 10.0), (-1.0, 0.0), x))
        .collect::<Vec<f64>>();
    print!("{:?}", result);
}
```

{{out}}

```txt

[-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.30000000000000004, -0.19999999999999996, -0.09999999999999998, 0]

```



## Scala


```scala
def mapRange(a1:Double, a2:Double, b1:Double, b2:Double, x:Double):Double=b1+(x-a1)*(b2-b1)/(a2-a1)

for(i <- 0 to 10)
  println("%2d in [0, 10] maps to %5.2f in [-1, 0]".format(i, mapRange(0,10, -1,0, i)))
```

{{out}}

```txt
 0 in [0, 10] maps to -1,00 in [-1, 0]
 1 in [0, 10] maps to -0,90 in [-1, 0]
 2 in [0, 10] maps to -0,80 in [-1, 0]
 3 in [0, 10] maps to -0,70 in [-1, 0]
 4 in [0, 10] maps to -0,60 in [-1, 0]
 5 in [0, 10] maps to -0,50 in [-1, 0]
 6 in [0, 10] maps to -0,40 in [-1, 0]
 7 in [0, 10] maps to -0,30 in [-1, 0]
 8 in [0, 10] maps to -0,20 in [-1, 0]
 9 in [0, 10] maps to -0,10 in [-1, 0]
10 in [0, 10] maps to  0,00 in [-1, 0]
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const func float: mapRange (in float: a1, in float: a2, in float: b1, in float: b2, ref float: s) is
    return b1 + (s-a1)*(b2-b1)/(a2-a1);

const proc: main is func
  local
    var integer: number is 0;
  begin
    writeln("Mapping [0,10] to [-1,0] at intervals of 1:");
    for number range 0 to 10 do
      writeln("f(" <& number <& ") = " <& mapRange(0.0, 10.0, -1.0, 0.0, flt(number)) digits 1);
    end for;
  end func;
```


{{out}}

```txt

Mapping [0,10] to [-1,0] at intervals of 1:
f(0) = -1.0
f(1) = -0.9
f(2) = -0.8
f(3) = -0.7
f(4) = -0.6
f(5) = -0.5
f(6) = -0.4
f(7) = -0.3
f(8) = -0.2
f(9) = -0.1
f(10) = 0.0

```



## Sidef


```ruby
func map_range(a, b, x) {
    var (a1, a2, b1, b2) = (a.bounds, b.bounds);
    x-a1 * b2-b1 / a2-a1 + b1;
}

var a = 0..10;
var b = -1..0;

for x in a {
    say "#{x} maps to #{map_range(a, b, x)}";
}
```

{{out}}

```txt
0 maps to -1
1 maps to -0.9
2 maps to -0.8
3 maps to -0.7
4 maps to -0.6
5 maps to -0.5
6 maps to -0.4
7 maps to -0.3
8 maps to -0.2
9 maps to -0.1
10 maps to 0
```



## Stata

The following program will map a variable to a new variable. It accepts '''if''' and '''in''' conditions.


```stata
program define maprange
	version 15.1
	syntax varname(numeric) [if] [in], ///
		from(numlist min=2 max=2) to(numlist min=2 max=2) ///
		GENerate(name) [REPLACE]
	tempname a b c d h
	sca `a'=`:word 1 of `from''
	sca `b'=`:word 2 of `from''
	sca `c'=`:word 1 of `to''
	sca `d'=`:word 2 of `to''
	sca `h'=(`d'-`c')/(`b'-`a')
	cap confirm variable `generate'
	if "`replace'"=="replace" & !_rc {
		qui replace `generate'=(`varlist'-`a')*`h'+`c' `if' `in'
	}
	else {
		if "`replace'"=="replace" {
			di in gr `"(note: variable `generate' not found)"'
		}
		qui gen `generate'=(`varlist'-`a')*`h'+`c' `if' `in'
	}
end
```


'''Example'''


```stata
clear
set obs 11
gen x=_n-1
maprange x if mod(x,2)==0, gen(y) from(0 10) to(-10 10)
maprange x if mod(x,2)!=0, gen(y) from(0 10) to(-100 100) replace
list
```


'''Output'''


```txt
     +----------+
     |  x     y |
     |----------|
  1. |  0   -10 |
  2. |  1   -80 |
  3. |  2    -6 |
  4. |  3   -40 |
  5. |  4    -2 |
     |----------|
  6. |  5     0 |
  7. |  6     2 |
  8. |  7    40 |
  9. |  8     6 |
 10. |  9    80 |
     |----------|
 11. | 10    10 |
     +----------+
```



## Swift



```Swift
import Foundation

func mapRanges(_ r1: ClosedRange<Double>, _ r2: ClosedRange<Double>, to: Double) -> Double {
  let num = (to - r1.lowerBound) * (r2.upperBound - r2.lowerBound)
  let denom = r1.upperBound - r1.lowerBound

  return r2.lowerBound + num / denom
}

for i in 0...10 {
  print(String(format: "%2d maps to %5.2f", i, mapRanges(0...10, -1...0, to: Double(i))))
}
```


{{out}}


```txt
 0 maps to -1.00
 1 maps to -0.90
 2 maps to -0.80
 3 maps to -0.70
 4 maps to -0.60
 5 maps to -0.50
 6 maps to -0.40
 7 maps to -0.30
 8 maps to -0.20
 9 maps to -0.10
10 maps to  0.00
```



## Tcl


```tcl
package require Tcl 8.5
proc rangemap {rangeA rangeB value} {
    lassign $rangeA a1 a2
    lassign $rangeB b1 b2
    expr {$b1 + ($value - $a1)*double($b2 - $b1)/($a2 - $a1)}
}
```

Demonstration (using a curried alias to bind the ranges mapped from and to):

```tcl
interp alias {} demomap {} rangemap {0 10} {-1 0}
for {set i 0} {$i <= 10} {incr i} {
    puts [format "%2d -> %5.2f" $i [demomap $i]]
}
```

{{out}}

```txt

 0 -> -1.00
 1 -> -0.90
 2 -> -0.80
 3 -> -0.70
 4 -> -0.60
 5 -> -0.50
 6 -> -0.40
 7 -> -0.30
 8 -> -0.20
 9 -> -0.10
10 ->  0.00

```



## Ursala

The function <code>f</code> is defined using pattern matching and substitution, taking a pair of pairs of interval endpoints and a number as parameters, and returning a number.

```Ursala
#import flo

f((("a1","a2"),("b1","b2")),"s") = plus("b1",div(minus("s","a1"),minus("a2","a1")))

#cast %eL

test = f* ((0.,10.),(-1.,0.))-* ari11/0. 10.
```

{{out}}

```txt
<
   -1.000000e+00,
   -9.000000e-01,
   -8.000000e-01,
   -7.000000e-01,
   -6.000000e-01,
   -5.000000e-01,
   -4.000000e-01,
   -3.000000e-01,
   -2.000000e-01,
   -1.000000e-01,
   0.000000e+00>
```

A more idiomatic way is to define f as a second order function

```Ursala
f(("a1","a2"),("b1","b2")) "s" = ...
```

with the same right hand side as above, so that it takes a pair of intervals and returns a function mapping numbers in one interval to numbers in the other.

An even more idiomatic way is to use the standard library function <code>plin</code>, which takes an arbitrarily long list of interval endpoints and returns a piecewise linear interpolation function.


## WDTE


```wdte
let mapRange r1 r2 s =>

    +
        (at r2 0)
        (/
            (*
                (-
                    s
                    (at r1 0)
                )
                (-
                    (at r2 1)
                    (at r2 0)
                )
            )
            (-
                (at r1 1)
                (at r1 0)
            )
        )
    ;

let s => import 'stream';
let str => import 'strings';

s.range 10
-> s.map (@ enum v => [v; mapRange [0; 10] [-1; 0] v])
-> s.map (@ print v => str.format '{} -> {}' (at v 0) (at v 1) -- io.writeln io.stdout)
-> s.drain
;
```


{{out}}

```txt
0 -> -1
1 -> -0.9
2 -> -0.8
3 -> -0.7
4 -> -0.6
5 -> -0.5
6 -> -0.4
7 -> -0.3
8 -> -0.2
9 -> -0.1
```



## XPL0


```XPL0
include c:\cxpl\codes;

func real Map(A1, A2, B1, B2, S);
real A1, A2, B1, B2, S;
return B1 + (S-A1)*(B2-B1)/(A2-A1);

int I;
[for I:= 0 to 10 do
    [if I<10 then ChOut(0, ^ );  IntOut(0, I);
    RlOut(0, Map(0., 10., -1., 0., float(I)));
    CrLf(0);
    ];
]
```


{{out}}

```txt

 0   -1.00000
 1   -0.90000
 2   -0.80000
 3   -0.70000
 4   -0.60000
 5   -0.50000
 6   -0.40000
 7   -0.30000
 8   -0.20000
 9   -0.10000
10    0.00000

```



## Yabasic


```Yabasic
sub MapRange(s, a1, a2, b1, b2)
    return b1+(s-a1)*(b2-b1)/(a2-a1)
end sub

for i = 0 to 10 step 2
    print i, " : ", MapRange(i,0,10,-1,0)
next
```



## zkl


```zkl
fcn mapRange([(a1,a2)], [(b1,b2)], s) // a1a2 is List(a1,a2)
   { b1 + ((s - a1) * (b2 - b1) / (a2 - a1)) }

r1:=T(0.0, 10.0); r2:=T(-1.0, 0.0);
foreach s in ([0.0 .. 10]){
   "%2d maps to %5.2f".fmt(s,mapRange(r1,r2, s)).println();
}
```

{{out}}

```txt

 0 maps to -1.00
 1 maps to -0.90
 2 maps to -0.80
 3 maps to -0.70
 4 maps to -0.60
 5 maps to -0.50
 6 maps to -0.40
 7 maps to -0.30
 8 maps to -0.20
 9 maps to -0.10
10 maps to  0.00

```

