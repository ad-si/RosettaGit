+++
title = "Maximum triangle path sum"
description = ""
date = 2019-10-18T13:01:01Z
aliases = []
[extra]
id = 17217
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "astro",
  "autohotkey",
  "bracmat",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elena",
  "elixir",
  "erre",
  "factor",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "nim",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "stata",
  "tcl",
  "vbscript",
  "zkl",
]
+++

Starting from the top of a pyramid of numbers like this, you can walk down going one step on the right or on the left, until you reach the bottom row:

```txt

                          55
                        94 48
                       95 30 96
                     77 71 26 67

```


One of such walks is 55 - 94 - 30 - 26.
You can compute the total of the numbers you have seen in such walk,
in this case it's 205.

Your problem is to find the maximum total among all possible paths from the top to the bottom row of the triangle. In the little example above it's 321.


## Task

Find the maximum total in the triangle below:

```txt

                          55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93

```


Such numbers can be included in the solution code, or read from a "triangle.txt" file.

This task is derived from the [http://projecteuler.net/problem=18 Euler Problem #18].





## Ada


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Max_Sum is

   Triangle : array (Positive range <>) of integer :=
                                     (55,
                                    94, 48,
                                  95, 30, 96,
                                77, 71, 26, 67,
                              97, 13, 76, 38, 45,
                            07, 36, 79, 16, 37, 68,
                          48, 07, 09, 18, 70, 26, 06,
                        18, 72, 79, 46, 59, 79, 29, 90,
                      20, 76, 87, 11, 32, 07, 07, 49, 18,
                    27, 83, 58, 35, 71, 11, 25, 57, 29, 85,
                  14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55,
                02, 90, 03, 60, 48, 49, 41, 46, 33, 36, 47, 23,
              92, 50, 48, 02, 36, 59, 42, 79, 72, 20, 82, 77, 42,
            56, 78, 38, 80, 39, 75, 02, 71, 66, 66, 01, 03, 55, 72,
          44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36,
        85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 01, 01, 99, 89, 52,
      06, 71, 28, 75, 94, 48, 37, 10, 23, 51, 06, 48, 53, 18, 74, 98, 15,
    27, 02, 92, 23, 08, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93);

   Last  : Integer := Triangle'Length;
   Tn    : Integer := 1;

begin
   while (Tn * (Tn + 1) / 2) < Last  loop
      Tn := Tn + 1;
   end loop;
   for N in reverse 2 .. Tn loop
      for I in 2 .. N loop
	 Triangle (Last - N) := Triangle (Last - N) +
	   Integer'Max(Triangle (Last - 1), Triangle (Last));
	 Last := Last - 1;
      end loop;
      Last := Last - 1;
   end loop;
   Put_Line(Integer'Image(Triangle(1)));
end Max_Sum;
```

```txt
 1320

```



## ALGOL 68

Basically the same algorithm as Ada and C++ but using a triangular matrix.

```algol68
# create a triangular array of the required values #

    [ 1]INT row  1 :=                           ( 55 );
    [ 2]INT row  2 :=                         ( 94, 48 );
    [ 3]INT row  3 :=                        ( 95, 30, 96 );
    [ 4]INT row  4 :=                      ( 77, 71, 26, 67 );
    [ 5]INT row  5 :=                     ( 97, 13, 76, 38, 45 );
    [ 6]INT row  6 :=                   ( 07, 36, 79, 16, 37, 68 );
    [ 7]INT row  7 :=                  ( 48, 07, 09, 18, 70, 26, 06 );
    [ 8]INT row  8 :=                ( 18, 72, 79, 46, 59, 79, 29, 90 );
    [ 9]INT row  9 :=               ( 20, 76, 87, 11, 32, 07, 07, 49, 18 );
    [10]INT row 10 :=             ( 27, 83, 58, 35, 71, 11, 25, 57, 29, 85 );
    [11]INT row 11 :=            ( 14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55 );
    [12]INT row 12 :=          ( 02, 90, 03, 60, 48, 49, 41, 46, 33, 36, 47, 23 );
    [13]INT row 13 :=         ( 92, 50, 48, 02, 36, 59, 42, 79, 72, 20, 82, 77, 42 );
    [14]INT row 14 :=       ( 56, 78, 38, 80, 39, 75, 02, 71, 66, 66, 01, 03, 55, 72 );
    [15]INT row 15 :=      ( 44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36 );
    [16]INT row 16 :=    ( 85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 01, 01, 99, 89, 52 );
    [17]INT row 17 :=   ( 06, 71, 28, 75, 94, 48, 37, 10, 23, 51, 06, 48, 53, 18, 74, 98, 15 );
    [18]INT row 18 := ( 27, 02, 92, 23, 08, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93 );

[18]REF[]INT triangle := ( row  1, row  2, row  3, row  4, row  5, row  6
                         , row  7, row  8, row  9, row 10, row 11, row 12
                         , row 13, row 14, row 15, row 16, row 17, row 18
                         );

PROC max = ( INT a, INT b )INT: IF a > b THEN a ELSE b FI;

# working backwards, we replace the elements of each row with the sum of that #
# element and the maximum of the two elements below it.                       #
# That destroys the triangle but leaves element [1][1] equal to the required  #
# maximum                                                                     #


FOR row FROM UPB triangle - 1 BY -1 TO 1
DO
    FOR element FROM 1 TO UPB triangle[row]
    DO
        # the elements "under" triangle[row][element] are                     #
        # triangle[row+1][element] and triangle[row+1][element+1]             #
        triangle[row][element]
            +:= max( triangle[row+1][element], triangle[row+1][element+1] )
    OD
OD;

print( ( triangle[1][1], newline ) )

```

```txt

      +1320

```



## AppleScript

```AppleScript
-- MAX PATH SUM ---------------------------------------------------------------

-- Working from the bottom of the triangle upwards,
-- summing each number with the larger of the two below
-- until the maximum emerges at the top.

-- maxPathSum :: [[Int]] -> Int
on maxPathSum(xss)

    -- With the last row as the initial accumulator,
    -- folding from the penultimate line,
    -- towards the top of the triangle:

    -- sumWithRowBelow :: [Int] -> [Int] -> [Int]
    script sumWithRowBelow
        on |λ|(row, accum)

            -- plusGreaterOfTwoBelow :: Int -> Int -> Int -> Int
            script plusGreaterOfTwoBelow
                on |λ|(x, intLeft, intRight)
                    x + max(intLeft, intRight)
                end |λ|
            end script

            -- The accumulator, zipped with the tail of the
            -- accumulator, yields pairs of adjacent sums so far.

            zipWith3(plusGreaterOfTwoBelow, row, accum, tail(accum))
        end |λ|
    end script

    -- A list of lists folded down to a list of just one remaining integer.
    -- Head returns that integer from the list.

    head(foldr1(sumWithRowBelow, xss))
end maxPathSum


-- TEST -----------------------------------------------------------------------
on run

    maxPathSum({¬
        {55}, ¬
        {94, 48}, ¬
        {95, 30, 96}, ¬
        {77, 71, 26, 67}, ¬
        {97, 13, 76, 38, 45}, ¬
        {7, 36, 79, 16, 37, 68}, ¬
        {48, 7, 9, 18, 70, 26, 6}, ¬
        {18, 72, 79, 46, 59, 79, 29, 90}, ¬
        {20, 76, 87, 11, 32, 7, 7, 49, 18}, ¬
        {27, 83, 58, 35, 71, 11, 25, 57, 29, 85}, ¬
        {14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55}, ¬
        {2, 90, 3, 60, 48, 49, 41, 46, 33, 36, 47, 23}, ¬
        {92, 50, 48, 2, 36, 59, 42, 79, 72, 20, 82, 77, 42}, ¬
        {56, 78, 38, 80, 39, 75, 2, 71, 66, 66, 1, 3, 55, 72}, ¬
        {44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36}, ¬
        {85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 1, 1, 99, 89, 52}, ¬
        {6, 71, 28, 75, 94, 48, 37, 10, 23, 51, 6, 48, 53, 18, 74, 98, 15}, ¬
        {27, 2, 92, 23, 8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93} ¬
            })

    --> 1320
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

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

-- foldr1 :: (a -> a -> a) -> [a] -> a
on foldr1(f, xs)
    if length of xs > 1 then
        tell mReturn(f)
            set v to item -1 of xs
            set lng to length of xs
            repeat with i from lng - 1 to 1 by -1
                set v to |λ|(item i of xs, v, i, xs)
            end repeat
            return v
        end tell
    else
        xs
    end if
end foldr1

-- head :: [a] -> a
on head(xs)
    if length of xs > 0 then
        item 1 of xs
    else
        missing value
    end if
end head

-- max :: Ord a => a -> a -> a
on max(x, y)
    if x > y then
        x
    else
        y
    end if
end max

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- minimum :: [a] -> a
on minimum(xs)
    script min
        on |λ|(a, x)
            if x < a or a is missing value then
                x
            else
                a
            end if
        end |λ|
    end script

    foldl(min, missing value, xs)
end minimum

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail

-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
on zipWith3(f, xs, ys, zs)
    set lng to minimum({length of xs, length of ys, length of zs})
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys, item i of zs)
        end repeat
        return lst
    end tell
end zipWith3
```

```txt
1320
```




## Astro


```python
fun maxpathsum(t): #: Array{Array{I}}
    let a = val t
    for i in a.length-1..-1..1, c in linearindices a[r]:
        a[r, c] += max(a[r+1, c], a[r=1, c+1])
    return a[1, 1]

let test = [
    [55],
    [94, 48],
    [95, 30, 96],
    [77, 71, 26, 67],
    [97, 13, 76, 38, 45],
    [07, 36, 79, 16, 37, 68],
    [48, 07, 09, 18, 70, 26, 06],
    [18, 72, 79, 46, 59, 79, 29, 90],
    [20, 76, 87, 11, 32, 07, 07, 49, 18],
    [27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
    [14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
    [02, 90, 03, 60, 48, 49, 41, 46, 33, 36, 47, 23],
    [92, 50, 48, 02, 36, 59, 42, 79, 72, 20, 82, 77, 42],
    [56, 78, 38, 80, 39, 75, 02, 71, 66, 66, 01, 03, 55, 72],
    [44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
    [85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 01, 01, 99, 89, 52],
    [06, 71, 28, 75, 94, 48, 37, 10, 23, 51, 06, 48, 53, 18, 74, 98, 15],
    [27, 02, 92, 23, 08, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]
]

@print maxpathsum test

```



## AutoHotkey


```AutoHotkey></lang

Examples:
```AutoHotkey
data :=[
(join ltrim
                 55,
                94,48,
               95,30,96,
              77,71,26,67,
             97,13,76,38,45,
            07,36,79,16,37,68,
           48,07,09,18,70,26,06,
          18,72,79,46,59,79,29,90,
         20,76,87,11,32,07,07,49,18,
        27,83,58,35,71,11,25,57,29,85,
       14,64,36,96,27,11,58,56,92,18,55,
      02,90,03,60,48,49,41,46,33,36,47,23,
     92,50,48,02,36,59,42,79,72,20,82,77,42,
    56,78,38,80,39,75,02,71,66,66,01,03,55,72,
   44,25,67,84,71,67,11,61,40,57,58,89,40,56,36,
  85,32,25,85,57,48,84,35,47,62,17,01,01,99,89,52,
 06,71,28,75,94,48,37,10,23,51,06,48,53,18,74,98,15,
27,02,92,23,08,71,76,84,15,52,92,63,81,10,44,10,69,93
)]

i	:= data.MaxIndex()
row	:= Ceil((Sqrt(8*i+1) - 1) / 2)
path:=[]

loop % row {
	path[i] := data[i]
	i--
}

while i {
	row := Ceil((Sqrt(8*i+1) - 1) / 2)
	path[i] := data[i] "+" (data[i+row] > data[i+row+1] ? path[i+row] : path[i+row+1])
	data[i] += data[i+row] > data[i+row+1] ? data[i+row] : data[i+row+1]
	i --
}

MsgBox % data[1] "`n" path[1]
```

Outputs:
```txt
1320
55+94+95+77+97+7+48+72+76+83+64+90+48+80+84+85+94+71
```



## Bracmat


```bracmat
(   "
                          55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93
"
  : ?triangle
& ( max
  =   a b
    . !arg:(?a.?b)&(!a:>!b|!b)
  )
& 0:?accumulator
&   whl
  ' ( @(!triangle:?row (\n|\r) ?triangle)
    & :?newaccumulator
    & 0:?first
    &   whl
      ' ( @(!row:? #%?n (" " ?row|:?row))
        & !accumulator:#%?second ?accumulator
        & !newaccumulator max$(!first.!second)+!n:?newaccumulator
        & !second:?first
        )
    & !newaccumulator 0:?accumulator
    )
& (   -1:?Max
    &   !accumulator
      : ? (%@:>!Max:?Max&~) ?
  | out$!Max
  )
)
```

```txt
1320
```



## C


```C

#include <stdio.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

int tri[] = {
        55,
	94, 48,
	95, 30, 96,
	77, 71, 26, 67,
	97, 13, 76, 38, 45,
	7, 36, 79, 16, 37, 68,
	48, 7, 9, 18, 70, 26, 6,
	18, 72, 79, 46, 59, 79, 29, 90,
	20, 76, 87, 11, 32, 7, 7, 49, 18,
	27, 83, 58, 35, 71, 11, 25, 57, 29, 85,
	14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55,
	2, 90, 3, 60, 48, 49, 41, 46, 33, 36, 47, 23,
	92, 50, 48, 2, 36, 59, 42, 79, 72, 20, 82, 77, 42,
	56, 78, 38, 80, 39, 75, 2, 71, 66, 66, 1, 3, 55, 72,
	44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36,
	85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 1, 1, 99, 89, 52,
	6, 71, 28, 75, 94, 48, 37, 10, 23, 51, 6, 48, 53, 18, 74, 98, 15,
	27, 2, 92, 23, 8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93
};

int main(void)
{
    const int len  = sizeof(tri) / sizeof(tri[0]);
    const int base = (sqrt(8*len + 1) - 1) / 2;
    int step       = base - 1;
    int stepc      = 0;

    int i;
    for (i = len - base - 1; i >= 0; --i) {
        tri[i] += max(tri[i + step], tri[i + step + 1]);
        if (++stepc == step) {
            step--;
            stepc = 0;
        }
    }

    printf("%d\n", tri[0]);
    return 0;
}

```

```txt

1320

```



## C++

```cpp

/* Algorithm complexity: n*log(n) */
#include <iostream>

int main( int argc, char* argv[] )
{
    int triangle[] =
    {
	55,
	94, 48,
	95, 30, 96,
	77, 71, 26, 67,
	97, 13, 76, 38, 45,
	7, 36, 79, 16, 37, 68,
	48, 7, 9, 18, 70, 26, 6,
	18, 72, 79, 46, 59, 79, 29, 90,
	20, 76, 87, 11, 32, 7, 7, 49, 18,
	27, 83, 58, 35, 71, 11, 25, 57, 29, 85,
	14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55,
	2, 90, 3, 60, 48, 49, 41, 46, 33, 36, 47, 23,
	92, 50, 48, 2, 36, 59, 42, 79, 72, 20, 82, 77, 42,
	56, 78, 38, 80, 39, 75, 2, 71, 66, 66, 1, 3, 55, 72,
	44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36,
	85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 1, 1, 99, 89, 52,
	6, 71, 28, 75, 94, 48, 37, 10, 23, 51, 6, 48, 53, 18, 74, 98, 15,
	27, 2, 92, 23, 8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93
    };

    const int size = sizeof( triangle ) / sizeof( int );
    const int tn = static_cast<int>(sqrt(2.0 * size));
    assert(tn * (tn + 1) == 2 * size);    // size should be a triangular number

    // walk backward by rows, replacing each element with max attainable therefrom
    for (int n = tn - 1; n > 0; --n)   // n is size of row, note we do not process last row
        for (int k = (n * (n-1)) / 2; k < (n * (n+1)) / 2; ++k) // from the start to the end of row
            triangle[k] += std::max(triangle[k + n], triangle[k + n + 1]);

    std::cout << "Maximum total: " << triangle[0] << "\n\n";
}

```

```txt
Maximum total: 1320
```


## C#


```c#

using System;

namespace RosetaCode
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			int[,] list = new int[18,19];
			string input = @"55
	                        94 48
	                       95 30 96
	                     77 71 26 67
	                    97 13 76 38 45
	                  07 36 79 16 37 68
	                 48 07 09 18 70 26 06
	               18 72 79 46 59 79 29 90
	              20 76 87 11 32 07 07 49 18
	            27 83 58 35 71 11 25 57 29 85
	           14 64 36 96 27 11 58 56 92 18 55
	         02 90 03 60 48 49 41 46 33 36 47 23
	        92 50 48 02 36 59 42 79 72 20 82 77 42
	      56 78 38 80 39 75 02 71 66 66 01 03 55 72
	     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
	   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
	  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
	27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93";
			var charArray = input.Split ('\n');

			for (int i=0; i < charArray.Length; i++) {
				var numArr = charArray[i].Trim().Split(' ');

				for (int j = 0; j<numArr.Length; j++)
				{
					int number = Convert.ToInt32 (numArr[j]);
					list [i, j] = number;
				}
			}

			for (int i = 16; i >= 0; i--) {
				for (int j = 0; j < 18; j++) {
					list[i,j] = Math.Max(list[i, j] + list[i+1, j], list[i,j] + list[i+1, j+1]);
				}
			}
			Console.WriteLine (string.Format("Maximum total: {0}", list [0, 0]));
		}
	}
}


```

```txt
Maximum total: 1320
```


=={{header|Clojure|ClojureScript}}==


```clojure

(ns clojure.examples.rosetta
	(:gen-class)
        (:require [clojure.string :as string]))

(def rosetta                     "55
	                        94 48
	                       95 30 96
	                     77 71 26 67
	                    97 13 76 38 45
	                  07 36 79 16 37 68
	                 48 07 09 18 70 26 06
	               18 72 79 46 59 79 29 90
	              20 76 87 11 32 07 07 49 18
	            27 83 58 35 71 11 25 57 29 85
	           14 64 36 96 27 11 58 56 92 18 55
	         02 90 03 60 48 49 41 46 33 36 47 23
	        92 50 48 02 36 59 42 79 72 20 82 77 42
	      56 78 38 80 39 75 02 71 66 66 01 03 55 72
	     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
	   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
	  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
	27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93")

;; The technique is described here in more detail http://mishadoff.com/blog/clojure-euler-problem-018/
;; Most of the code converts the string data to a nested array of integers.
;; The code to calculate the max sum is then only a  single line

;; First convert string data to nested list
;;  with each inner list containing one row of the triangle
;;   [[55] [94 48] [95 30 96] ... [...10 69 93]
(defn parse-int [s]
    " Convert digits to a number (finds digits when could be surrounded by non-digits"
  (Integer. (re-find #"\d+" s)))

(defn data-int-array [s]
  " Convert string to integer array"
  (map parse-int (string/split (string/trim s) #"\s+")))

(defn nested-triangle [s]
  " Convert triangle to nested vector, with each inner vector containing one triangle row"
  (loop [lst s n 1 newlist nil]
    (if (empty? lst) (reverse newlist)
                     (recur (drop n lst) (inc n) (cons (take n lst) newlist)))))
; Create nested list
(def nested-list (nested-triangle (data-int-array rosetta)))

;; Function to compute maximum path sum
(defn max-sum [s]
  " Compute maximum path sum using a technique described here: http://mishadoff.com/blog/clojure-euler-problem-018/"
  (reduce (fn [a b] (map + b (map max a (rest a)))) (reverse s)))

; Print result
(println (max-sum nested-list))


```

```txt
1320
```



## Common Lisp


```lisp
(defun find-max-path-sum (s)
  (let ((triangle (loop for line = (read-line s NIL NIL)
                        while line
                        collect (with-input-from-string (str line)
                                  (loop for n = (read str NIL NIL)
                                        while n
                                        collect n)))))
    (flet ((get-max-of-pairs (xs)
             (maplist (lambda (ys)
                        (and (cdr ys) (max (car ys) (cadr ys))))
                      xs)))
      (car (reduce (lambda (xs ys)
                     (mapcar #'+ (get-max-of-pairs xs) ys))
                   (reverse triangle))))))

(defparameter *small-triangle*
  "    55
     94 48
    95 30 96
  77 71 26 67")
(format T "~a~%" (with-input-from-string (s *small-triangle*)
                   (find-max-path-sum s)))
(format T "~a~%" (with-open-file (f "triangle.txt")
                   (find-max-path-sum f)))
```


```txt
321
1320
```



## D


```d
void main() {
    import std.stdio, std.algorithm, std.range, std.file, std.conv;

    "triangle.txt".File.byLine.map!split.map!(to!(int[])).array.retro
    .reduce!((x, y) => zip(y, x, x.dropOne)
                       .map!(t => t[0] + t[1 .. $].max)
                       .array)[0]
    .writeln;
}
```

```txt
1320
```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
import extensions'math;

string input =               "55
                            94 48
                          95 30 96
                        77 71 26 67
                       97 13 76 38 45
                     07 36 79 16 37 68
                    48 07 09 18 70 26 06
                  18 72 79 46 59 79 29 90
                 20 76 87 11 32 07 07 49 18
               27 83 58 35 71 11 25 57 29 85
              14 64 36 96 27 11 58 56 92 18 55
            02 90 03 60 48 49 41 46 33 36 47 23
           92 50 48 02 36 59 42 79 72 20 82 77 42
         56 78 38 80 39 75 02 71 66 66 01 03 55 72
        44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
      85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
     06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
   27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93";

public program()
{
    var list := new IntMatrix(18,19);

    int i := 0;
    int j := 0;
    input.split(forward newLine).forEach:(string line)
    {
        j := 0;
        line.trim().split(" ").forEach:(string num)
        {
            list[i][j] := num.toInt();

            j += 1
        };

        i += 1
    };

    for(int i := 16, i >= 0, i-=1)
    {
        for(int j := 0, j < 18, j += 1)
        {
            list[i][j] := max(list[i][j] + list[i+1][j], list[i][j] + list[i+1][j+1])
        }
    };

    console.printLine("Maximum total: ", list[0][0])
}
```

```txt

Maximum total: 1320

```



## Elixir


```elixir
defmodule Maximum do
  def triangle_path(text) do
    text
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split()
      |> Enum.map(&String.to_integer(&1))
    end)
    |> Enum.reduce([], fn x,total ->
         [0]++total++[0]
         |> Enum.chunk_every( 2, 1)
         |> Enum.map(&Enum.max(&1))
         |> Enum.zip(x)
         |> Enum.map(fn{a,b} -> a+b end)
       end)
    |> Enum.max()
  end
end

text = """
                          55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93
"""

IO.puts Maximum.triangle_path(text)

```


```txt

1320

```



## ERRE


```ERRE

PROGRAM TRIANGLE_PATH

CONST ROW=18

DIM TRI[200]

!
! for rosettacode,org
!

FUNCTION MAX(X,Y)
   MAX=-X*(X>=Y)-Y*(X<Y)
END FUNCTION

BEGIN

     DATA(55)
     DATA(94,48)
     DATA(95,30,96)
     DATA(77,71,26,67)
     DATA(97,13,76,38,45)
     DATA(7,36,79,16,37,68)
     DATA(48,7,9,18,70,26,6)
     DATA(18,72,79,46,59,79,29,90)
     DATA(20,76,87,11,32,7,7,49,18)
     DATA(27,83,58,35,71,11,25,57,29,85)
     DATA(14,64,36,96,27,11,58,56,92,18,55)
     DATA(2,90,3,60,48,49,41,46,33,36,47,23)
     DATA(92,50,48,2,36,59,42,79,72,20,82,77,42)
     DATA(56,78,38,80,39,75,2,71,66,66,1,3,55,72)
     DATA(44,25,67,84,71,67,11,61,40,57,58,89,40,56,36)
     DATA(85,32,25,85,57,48,84,35,47,62,17,1,1,99,89,52)
     DATA(6,71,28,75,94,48,37,10,23,51,6,48,53,18,74,98,15)
     DATA(27,2,92,23,8,71,76,84,15,52,92,63,81,10,44,10,69,93)

     PRINT(CHR$(12);) !CLS
     LUNG=ROW*(ROW+1)/2
     FOR I%=0 TO LUNG-1 DO
        READ(TRI[I%])
     END FOR

     BSE=(SQR(8*LUNG+1)-1)/2
     STP=BSE-1
     STEPC=0

     FOR I%=LUNG-BSE-1 TO 0 STEP -1 DO
        TRI[I%]=TRI[I%]+MAX(TRI[I%+STP],TRI[I%+STP+1])
        STEPC=STEPC+1
        IF STEPC=STP THEN
             STP=STP-1
             STEPC=0
        END IF
     END FOR

     PRINT(TRI[0])
END PROGRAM

```



## Factor


```factor
USING: grouping.extras io.encodings.utf8 io.files kernel
math.order math.parser math.vectors prettyprint sequences
splitting ;
IN: rosetta-code.maximum-triangle-path-sum

: parse-triangle ( path -- seq )
    utf8 file-lines [ " " split harvest ] map
    [ [ string>number ] map ] map ;

: max-triangle-path-sum ( seq -- n )
    <reversed> unclip-slice [ swap [ max ] 2clump-map v+ ]
    reduce first ;

"triangle.txt" parse-triangle max-triangle-path-sum .
```

```txt

1320

```


=={{Header|Fortran}}==
This being Fortran, why not a brute-force scan of all possible paths? This is eased by noting that from a given position, only two numbers are accessible, and always two numbers. Just like binary digits. So, for three levels, the choices would be 000, 001, 010, 011, 100, 101, 110, 111 or somesuch. Since however the pinnacle of the pyramid is always chosen, there is no choice there so the digits would be 100, 101, 110, 111.

A triangular array can be defined in some languages, and in some circumstances a square array is used with a lower triangle and upper triangle partition, but here, a simple linear array is in order, with some attention to subscript usage. The first layer has one number, the second has two, the third has three, ... easy enough. The more refined method that determines the maximum sum without ascertaining the path through working upwards from the base employs a FOR ALL statement in adding the maximum of the two possible descendants to each brick in the current layer, employing array BEST that starts off with all the values of the bottom layer. As each layer is one value shorter than the one below and the expression computes <code>BEST(i) = ... + MAX(BEST(i),BEST(i + 1))</code> the special feature of the FORALL statement, that ''all'' rhs expressions are evaluated before ''any'' results are placed on the lhs is not needed if a DO-loop were to be used instead.

For input, free-format is convenient. Bad input still is a problem, and can lead to puzzles. If say when N values are to be read but an input line is short of numbers, then additional lines will be read and confusion is likely. So, read the file's record into a text variable and then extract the expected N values from that. Should a problem arise, then the troublesome record can be shown.


```Fortran

      MODULE PYRAMIDS	!Produces a pyramid of numbers in 1-D array.
       INTEGER MANY		!The usual storage issues.
       PARAMETER (MANY = 666)	!This should suffice.
       INTEGER BRICK(MANY),IN,LAYERS	!Defines a pyramid.
       CONTAINS
        SUBROUTINE IMHOTEP(PLAN)!The architect.
Counting is from the apex down, the Erich von Daniken construction.
         CHARACTER*(*) PLAN	!The instruction file.
         INTEGER I,IT		!Steppers.
         CHARACTER*666 ALINE	!A scratchpad for input.
          IN = 0		!No bricks.
          LAYERS = 0		!In no courses.
          WRITE (6,*) "Reading from ",PLAN	!Here we go.
          OPEN(10,FILE=PLAN,FORM="FORMATTED",ACTION="READ",ERR=6)	!I hope.
          GO TO 10		!Why can't OPEN be a function?@*&%#^%!
    6     STOP "Can't grab the file!"
Chew into the plan.
   10     READ (10,11,END = 20) ALINE	!Get the whole line in one piece.
   11     FORMAT (A)			!As plain text.
          IF (ALINE .EQ. "") GO TO 10	!Ignoring any blank lines.
          IF (ALINE(1:1).EQ."%") GO TO 10	!A comment opportunity.
          LAYERS = LAYERS + 1		!Righto, this should be the next layer.
          IF (IN + LAYERS.GT.MANY) STOP "Too many bricks!"	!Perhaps not.
          READ (ALINE,*,END = 15,ERR = 15) BRICK(IN + 1:IN + LAYERS)	!Free format.
          IN = IN + LAYERS		!Insufficient numbers will provoke trouble.
          GO TO 10			!Extra numbers/stuff will be ignored.
Caught a crab? A bad number, or too few numbers on a line? No read-next-record antics, thanks.
   15     WRITE (6,16) LAYERS,ALINE	!Just complain.
   16     FORMAT ("Bad layer ",I0,": ",A)
Completed the plan.
   20     WRITE (6,21) IN,LAYERS	!Announce some details.
   21     FORMAT (I0," bricks in ",I0," layers.")
          CLOSE(10)			!Finished with input.
Cast forth the numbers in a nice pyramid.
   30     IT = 0		!For traversing the pyramid.
          DO I = 1,LAYERS	!Each course has one more number than the one before.
            WRITE (6,31) BRICK(IT + 1:IT + I)	!Sweep along the layer.
   31       FORMAT (<LAYERS*2 - 2*I>X,666I4)	!Leading spaces may be zero in number.
            IT = IT + I				!Thus finger the last of a layer.
          END DO		!On to the start of the next layer.
        END SUBROUTINE IMHOTEP	!The pyramid's plan is ready.

        SUBROUTINE TRAVERSE	!Clamber around the pyramid. Thoroughly.
C   The idea is that a pyramid of numbers is provided, and then, starting at the peak,
c work down to the base summing the numbers at each step to find the maximum value path.
c The constraint is that from a particular brick, only the two numbers below left and below right
c may be reached in stepping to that lower layer.
c   Since that is a 0/1 choice, recorded in MOVE, a base-two scan searches the possibilities.
         INTEGER MOVE(LAYERS)		!Choices are made at the various positions.
         INTEGER STEP(LAYERS),WALK(LAYERS)	!Thus determining the path.
         INTEGER I,L,IT		!Steppers.
         INTEGER PS,WS		!Scores.
          WRITE (6,1) LAYERS		!Announce the intention.
    1     FORMAT (//,"Find the highest score path across a pyramid of ",
     1     I0," layers."/)	!I'm not worrying over singular/plural.
          MOVE = 0	!All 0/1 values to zero.
          MOVE(1) = 1	!Except the first.
          STEP(1) = 1	!Every path starts here, without option.
          WS = -666	!The best score so far.
Commence a multi-level loop, using the values of MOVE as the digits, one digit per level.
   10       IT = 1		!All paths start with the first step.
            PS = BRICK(1)	!The starting score,.
c            write (6,8) "Move",MOVE,WS
            DO L = 2,LAYERS	!Deal with the subsequent layers.
              IT = IT + L - 1 + MOVE(L)	!Choose a brick.
              STEP(L) = IT		!Remember this step.
              PS = PS + BRICK(IT)	!Count its score.
c              WRITE (6,6) L,IT,BRICK(IT),PS
    6         FORMAT ("Layer ",I0,",Brick(",I0,")=",I0,",Sum=",I0)
            END DO		!Thus is the path determined.
            IF (PS .GT. WS) THEN	!An improvement?
              IF (WS.GT.0) WRITE (6,7) WS,PS	!Yes! Announce.
    7         FORMAT ("Improved path score: ",I0," to ",I0)
              WRITE (6,8) "Moves",MOVE		!Show the choices at each layer..
              WRITE (6,8) "Steps",STEP		!That resulted in this path.
              WRITE (6,8) "Score",BRICK(STEP)	!Whose steps were scored thus.
    8         FORMAT (A8,666I4)			!This should suffice.
              WS = PS				!Record the new best value.
              WALK = STEP			!And the path thereby.
            END IF			!So much for an improvement.
            DO L = LAYERS,1,-1		!Now add one to the number in MOVE.
              IF (MOVE(L).EQ.0) THEN	!By finding the lowest order zero.
                MOVE(L) = 1		!Making it one,
                MOVE(L + 1:LAYERS) = 0	!And setting still lower orders back to zero.
                GO TO 10		!And if we did, there's more to do!
              END IF		!But if that bit wasn't zero,
            END DO		!Perhaps the next one up will be.
          WRITE (6,*) WS," is the highest score."	!So much for that.
        END SUBROUTINE TRAVERSE	!All paths considered...

        SUBROUTINE REFINE	!Ascertain the highest score without searching.
         INTEGER BEST(LAYERS)	!A scratchpad.
         INTEGER I,L		!Steppers.
          L = LAYERS*(LAYERS - 1)/2 + 1	!Finger the first brick of the lowest layer.
          BEST = BRICK(L:L + LAYERS - 1)!Syncopation. Copy the lowest layer.
          DO L = LAYERS - 1,1,-1	!Work towards the peak.
            FORALL (I = 1:L) BEST(I) = BRICK(L*(L - 1)/2 + I)	!Add to each brick's value
     1                               + MAXVAL(BEST(I:I + 1))	!The better of its two possibles.
          END DO			!On to the next layer.
          WRITE (6,*) BEST(1)," is the highest score. By some path."
        END SUBROUTINE REFINE	!Who knows how we get there.
      END MODULE PYRAMIDS

      PROGRAM TRICKLE
      USE PYRAMIDS
c      CALL IMHOTEP("Sakkara.txt")
      CALL IMHOTEP("Cheops.txt")
      CALL TRAVERSE			!Do this the definite way.
      CALL REFINE			!Only the result by more cunning.
      END

```


Output:

```txt

 Reading from Cheops.txt
171 bricks in 18 layers.
                                    55
                                  94  48
                                95  30  96
                              77  71  26  67
                            97  13  76  38  45
                           7  36  79  16  37  68
                        48   7   9  18  70  26   6
                      18  72  79  46  59  79  29  90
                    20  76  87  11  32   7   7  49  18
                  27  83  58  35  71  11  25  57  29  85
                14  64  36  96  27  11  58  56  92  18  55
               2  90   3  60  48  49  41  46  33  36  47  23
            92  50  48   2  36  59  42  79  72  20  82  77  42
          56  78  38  80  39  75   2  71  66  66   1   3  55  72
        44  25  67  84  71  67  11  61  40  57  58  89  40  56  36
      85  32  25  85  57  48  84  35  47  62  17   1   1  99  89  52
     6  71  28  75  94  48  37  10  23  51   6  48  53  18  74  98  15
  27   2  92  23   8  71  76  84  15  52  92  63  81  10  44  10  69  93


Find the highest score path across a pyramid of 18 layers.

   Moves   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   Steps   1   2   4   7  11  16  22  29  37  46  56  67  79  92 106 121 137 154
   Score  55  94  95  77  97   7  48  18  20  27  14   2  92  56  44  85   6  27
Improved path score: 864 to 904
   Moves   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0
   Steps   1   2   4   7  11  16  22  29  37  46  56  67  79  92 106 121 138 155
   Score  55  94  95  77  97   7  48  18  20  27  14   2  92  56  44  85  71   2
Improved path score: 904 to 994
   Moves   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   1
   Steps   1   2   4   7  11  16  22  29  37  46  56  67  79  92 106 121 138 156
   Score  55  94  95  77  97   7  48  18  20  27  14   2  92  56  44  85  71  92
Improved path score: 994 to 1041
   Moves   1   0   0   0   0   0   0   0   0   0   0   0   0   1   1   1   1   1
   Steps   1   2   4   7  11  16  22  29  37  46  56  67  79  93 108 124 141 159
   Score  55  94  95  77  97   7  48  18  20  27  14   2  92  78  67  85  94  71
Improved path score: 1041 to 1087
   Moves   1   0   0   0   0   0   0   0   0   0   0   1   0   0   1   1   1   1
   Steps   1   2   4   7  11  16  22  29  37  46  56  68  80  93 108 124 141 159
   Score  55  94  95  77  97   7  48  18  20  27  14  90  50  78  67  85  94  71
Improved path score: 1087 to 1104
   Moves   1   0   0   0   0   0   0   0   0   0   0   1   1   1   0   0   1   1
   Steps   1   2   4   7  11  16  22  29  37  46  56  68  81  95 109 124 141 159
   Score  55  94  95  77  97   7  48  18  20  27  14  90  48  80  84  85  94  71
Improved path score: 1104 to 1137
   Moves   1   0   0   0   0   0   0   0   0   0   1   0   0   0   1   1   1   1
   Steps   1   2   4   7  11  16  22  29  37  46  57  68  80  93 108 124 141 159
   Score  55  94  95  77  97   7  48  18  20  27  64  90  50  78  67  85  94  71
Improved path score: 1137 to 1154
   Moves   1   0   0   0   0   0   0   0   0   0   1   0   1   1   0   0   1   1
   Steps   1   2   4   7  11  16  22  29  37  46  57  68  81  95 109 124 141 159
   Score  55  94  95  77  97   7  48  18  20  27  64  90  48  80  84  85  94  71
Improved path score: 1154 to 1193
   Moves   1   0   0   0   0   0   0   0   0   1   0   0   0   0   1   1   1   1
   Steps   1   2   4   7  11  16  22  29  37  47  57  68  80  93 108 124 141 159
   Score  55  94  95  77  97   7  48  18  20  83  64  90  50  78  67  85  94  71
Improved path score: 1193 to 1210
   Moves   1   0   0   0   0   0   0   0   0   1   0   0   1   1   0   0   1   1
   Steps   1   2   4   7  11  16  22  29  37  47  57  68  81  95 109 124 141 159
   Score  55  94  95  77  97   7  48  18  20  83  64  90  48  80  84  85  94  71
Improved path score: 1210 to 1249
   Moves   1   0   0   0   0   0   0   0   1   0   0   0   0   0   1   1   1   1
   Steps   1   2   4   7  11  16  22  29  38  47  57  68  80  93 108 124 141 159
   Score  55  94  95  77  97   7  48  18  76  83  64  90  50  78  67  85  94  71
Improved path score: 1249 to 1266
   Moves   1   0   0   0   0   0   0   0   1   0   0   0   1   1   0   0   1   1
   Steps   1   2   4   7  11  16  22  29  38  47  57  68  81  95 109 124 141 159
   Score  55  94  95  77  97   7  48  18  76  83  64  90  48  80  84  85  94  71
Improved path score: 1266 to 1303
   Moves   1   0   0   0   0   0   0   1   0   0   0   0   0   0   1   1   1   1
   Steps   1   2   4   7  11  16  22  30  38  47  57  68  80  93 108 124 141 159
   Score  55  94  95  77  97   7  48  72  76  83  64  90  50  78  67  85  94  71
Improved path score: 1303 to 1320
   Moves   1   0   0   0   0   0   0   1   0   0   0   0   1   1   0   0   1   1
   Steps   1   2   4   7  11  16  22  30  38  47  57  68  81  95 109 124 141 159
   Score  55  94  95  77  97   7  48  72  76  83  64  90  48  80  84  85  94  71
        1320  is the highest score.
        1320  is the highest score. By some path.

```


=={{Header|FreeBASIC}}==

```FreeBASIC
' version 21-06-2015
' compile with: fbc -s console

Data "                  55"
Data "                 94 48"
Data "                95 30 96"
Data "               77 71 26 67"
Data "              97 13 76 38 45"
Data "             07 36 79 16 37 68"
Data "            48 07 09 18 70 26 06"
Data "           18 72 79 46 59 79 29 90"
Data "          20 76 87 11 32 07 07 49 18"
Data "         27 83 58 35 71 11 25 57 29 85"
Data "        14 64 36 96 27 11 58 56 92 18 55"
Data "       02 90 03 60 48 49 41 46 33 36 47 23"
Data "      92 50 48 02 36 59 42 79 72 20 82 77 42"
Data "     56 78 38 80 39 75 02 71 66 66 01 03 55 72"
Data "    44 25 67 84 71 67 11 61 40 57 58 89 40 56 36"
Data "   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52"
Data "  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15"
Data " 27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93"
Data "END"   ' no more data

' ------=< MAIN >=------

Dim As String ln
Dim As Integer matrix(1 To 20, 1 To 20)
Dim As Integer x = 1, y, s1, s2, size

Do
    Read ln
    ln = Trim(ln)
    If ln = "END" Then Exit Do
    For y = 1 To x
        matrix(x, y) = Val(Left(ln, 2))
        ln = Mid(ln, 4)
    Next
    x += 1
    size += 1
Loop

For x = size - 1 To 1 Step - 1
    For y = 1 To x
        s1 = matrix(x + 1, y)
        s2 = matrix(x + 1, y + 1)
        If s1 > s2 Then
            matrix(x, y) += s1
        Else
            matrix(x, y) += s2
        End If
    Next
Next

Print
Print "  maximum triangle path sum ="; matrix(1, 1)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
  maximum triangle path sum = 1320
```


=={{Header|Go}}==

```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

const t = `               55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93`

func main() {
    lines := strings.Split(t, "\n")
    f := strings.Fields(lines[len(lines)-1])
    d := make([]int, len(f))
    var err error
    for i, s := range f {
        if d[i], err = strconv.Atoi(s); err != nil {
            panic(err)
        }
    }
    d1 := d[1:]
    var l, r, u int
    for row := len(lines) - 2; row >= 0; row-- {
        l = d[0]
        for i, s := range strings.Fields(lines[row]) {
            if u, err = strconv.Atoi(s); err != nil {
                panic(err)
            }
            if r = d1[i]; l > r {
                d[i] = u + l
            } else {
                d[i] = u + r
            }
            l = r
        }
    }
    fmt.Println(d[0])
}
```

```txt

1320

```



## Haskell


```haskell
parse = map (map read . words) . lines
f x y z = x + max y z
g xs ys = zipWith3 f xs ys $ tail ys
solve = head . foldr1 g
main = readFile "triangle.txt" >>= print . solve . parse
```

```txt
1320
```


Or, inlining the data for quick testing, and using an applicative expression:


```haskell
maxPathSum :: [[Int]] -> Int
maxPathSum = head . foldr1 ((<*> tail) . zipWith3 (\x y z -> x + max y z))

main :: IO ()
main =
  print $
  maxPathSum
    [ [55]
    , [94, 48]
    , [95, 30, 96]
    , [77, 71, 26, 67]
    , [97, 13, 76, 38, 45]
    , [07, 36, 79, 16, 37, 68]
    , [48, 07, 09, 18, 70, 26, 06]
    , [18, 72, 79, 46, 59, 79, 29, 90]
    , [20, 76, 87, 11, 32, 07, 07, 49, 18]
    , [27, 83, 58, 35, 71, 11, 25, 57, 29, 85]
    , [14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55]
    , [02, 90, 03, 60, 48, 49, 41, 46, 33, 36, 47, 23]
    , [92, 50, 48, 02, 36, 59, 42, 79, 72, 20, 82, 77, 42]
    , [56, 78, 38, 80, 39, 75, 02, 71, 66, 66, 01, 03, 55, 72]
    , [44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36]
    , [85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 01, 01, 99, 89, 52]
    , [06, 71, 28, 75, 94, 48, 37, 10, 23, 51, 06, 48, 53, 18, 74, 98, 15]
    , [27, 02, 92, 23, 08, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]
    ]
```


```txt
1320
```



## J


```j
padTri=: 0 ". ];._2                  NB. parse triangle and (implicitly) pad with zeros
maxSum=: [: {. (+ (0 ,~ 2 >./\ ]))/  NB. find max triangle path sum
```


'''Example Usage'''

```j
   maxSum padTri freads 'triangle.txt'
1320
```


Explanation:

First, we pad all short rows with trailing zeros so that all rows are the same length. This eliminates some ambiguity and simplifies the expression of both the data and the code.

Second, starting with the last row, for each pair of numbers we find the largest of the two (resulting in a list slightly shorter than before, so of course we pad it with a trailing zero) and add that row to the previous row. After repeating this through all the rows, the first value of the resulting row is the maximum we were looking for.

Instead of padding, we could instead trim the other argument to match the current reduced row length.


```J
maxsum=: ((] + #@] {. [)2 >./\ ])/
```


However, this turns out to be a slightly slower approach, because we are doing a little more work for each row.

(Note that the cost of padding every row to the same width averages out to an average 2x cost in space and time. So what we are saying here is that the interpreter overhead for changing the size of the memory region used in each operation with each row winds up being more than a 2x cost. You can probably beat that using compiled code, but of course the cost of compiling the program will itself be more than 2x - so not worth paying in a one-off experiment. You wind up with similar issues in any system involving one-off tests.)


## Java

```java
import java.nio.file.*;
import static java.util.Arrays.stream;

public class MaxPathSum {

    public static void main(String[] args) throws Exception {
        int[][] data = Files.lines(Paths.get("triangle.txt"))
                .map(s -> stream(s.trim().split("\\s+"))
                        .mapToInt(Integer::parseInt)
                        .toArray())
                .toArray(int[][]::new);

        for (int r = data.length - 1; r > 0; r--)
            for (int c = 0; c < data[r].length - 1; c++)
                data[r - 1][c] += Math.max(data[r][c], data[r][c + 1]);

        System.out.println(data[0][0]);
    }
}
```



```txt
1320
```



## Javascript


### ES5


### =Imperative=


```javascript

var arr = [
[55],
[94, 48],
[95, 30, 96],
[77, 71, 26, 67],
[97, 13, 76, 38, 45],
[07, 36, 79, 16, 37, 68],
[48, 07, 09, 18, 70, 26, 06],
[18, 72, 79, 46, 59, 79, 29, 90],
[20, 76, 87, 11, 32, 07, 07, 49, 18],
[27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
[14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
[02, 90, 03, 60, 48, 49, 41, 46, 33, 36, 47, 23],
[92, 50, 48, 02, 36, 59, 42, 79, 72, 20, 82, 77, 42],
[56, 78, 38, 80, 39, 75, 02, 71, 66, 66, 01, 03, 55, 72],
[44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
[85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 01, 01, 99, 89, 52],
[06, 71, 28, 75, 94, 48, 37, 10, 23, 51, 06, 48, 53, 18, 74, 98, 15],
[27, 02, 92, 23, 08, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]
];

while (arr.length !== 1) {
  var len = arr.length;
  var row = [];
  var current = arr[len-2];
  var currentLen = current.length - 1;
  var end = arr[len-1];

  for ( var i = 0; i <= currentLen; i++ ) {
    row.push(Math.max(current[i] + end[i] || 0, current[i] + end[i+1] || 0) )
  }

  arr.pop();
  arr.pop();

  arr.push(row);
}

console.log(arr);

```


```javascript

[ [ 1320 ] ]

```



### =Functional=

```JavaScript
(function () {

  // Right fold using final element as initial accumulator
  // (a -> a -> a) -> t a -> a
  function foldr1(f, lst) {
    return lst.length > 1 ? (
      f(lst[0], foldr1(f, lst.slice(1)))
    ) : lst[0];
  }

  // function of arity 3 mapped over nth items of each of 3 lists
  // (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
  function zipWith3(f, xs, ys, zs) {
    return zs.length ? [f(xs[0], ys[0], zs[0])].concat(
      zipWith3(f, xs.slice(1), ys.slice(1), zs.slice(1))) : [];
  }

  // Evaluating from bottom up (right fold)
  // and with recursion left to right (head and first item of tail at each stage)
  return foldr1(
    function (xs, ys) {
      return zipWith3(
        function (x, y, z) {
          return x + (y < z ? z : y);
        },
        xs, ys, ys.slice(1) // item above, and larger of two below
      );
    }, [
        [55],
        [94, 48],
        [95, 30, 96],
        [77, 71, 26, 67],
        [97, 13, 76, 38, 45],
        [07, 36, 79, 16, 37, 68],
        [48, 07, 09, 18, 70, 26, 06],
        [18, 72, 79, 46, 59, 79, 29, 90],
        [20, 76, 87, 11, 32, 07, 07, 49, 18],
        [27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
        [14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
        [02, 90, 03, 60, 48, 49, 41, 46, 33, 36, 47, 23],
        [92, 50, 48, 02, 36, 59, 42, 79, 72, 20, 82, 77, 42],
        [56, 78, 38, 80, 39, 75, 02, 71, 66, 66, 01, 03, 55, 72],
        [44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
        [85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 01, 01, 99, 89, 52],
        [06, 71, 28, 75, 94, 48, 37, 10, 23, 51, 06, 48, 53, 18, 74, 98, 15],
        [27, 02, 92, 23, 08, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]
    ]
  )[0];

})();
```

```JavaScript>1320</lang



### ES6


### =Imperative=


```javascript
function maximumTrianglePathSum(triangle) {

  function distilLastLine() {
    let lastLine = triangle.pop(),
        aboveLine = triangle.pop();
    for (let i = 0; i < aboveLine.length; i++)
      aboveLine[i] = Math.max(
        aboveLine[i] + lastLine[i],
        aboveLine[i] + lastLine[i + 1]
      );
    triangle.push(aboveLine);
  }

  do {
    distilLastLine();
  } while (triangle.length > 1);
  return triangle[0][0];
}

// testing
let theTriangle = [
[55],
[94, 48],
[95, 30, 96],
[77, 71, 26, 67],
[97, 13, 76, 38, 45],
[ 7, 36, 79, 16, 37, 68],
[48,  7,  9, 18, 70, 26,  6],
[18, 72, 79, 46, 59, 79, 29, 90],
[20, 76, 87, 11, 32,  7,  7, 49, 18],
[27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
[14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
[ 2, 90,  3, 60, 48, 49, 41, 46, 33, 36, 47, 23],
[92, 50, 48,  2, 36, 59, 42, 79, 72, 20, 82, 77, 42],
[56, 78, 38, 80, 39, 75,  2, 71, 66, 66,  1,  3, 55, 72],
[44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
[85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17,  1,  1, 99, 89, 52],
[ 6, 71, 28, 75, 94, 48, 37, 10, 23, 51,  6, 48, 53, 18, 74, 98, 15],
[27,  2, 92, 23,  8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]
];

console.log(maximumTrianglePathSum(theTriangle));
```

```javascript>1320</lang


### =Functional=

```JavaScript
(() => {
    'use strict';

    // MAX PATH SUM -----------------------------------------------------------

    // Working from the bottom of the triangle upwards,
    // summing each number with the larger of the two below
    // until the maximum emerges at the top.

    // maxPathSum ::[[Int]] -> Int
    const maxPathSum = xss =>

        // A list of lists folded down to a list of just one remaining integer.
        // The head function returns that integer from the list.

        head(
            foldr1(

                // The accumulator, zipped with the tail of the
                // accumulator, yields pairs of adjacent sums so far.
                (ys, xs) => zipWith3(

                    // Plus greater of two below
                    (a, b, c) => a + max(b, c),
                    xs, ys, tail(ys)
                ),
                xss
            )
        );


    // GENERIC FUNCTIONS ------------------------------------------------------

    // Right fold using final element as initial accumulator
    // foldr1 :: (a -> a -> a) -> [a] -> a
    const foldr1 = (f, xs) =>
        xs.length > 0 ? init(xs)
        .reduceRight(f, last(xs)) : [];

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // init :: [a] -> [a]
    const init = xs => xs.length ? xs.slice(0, -1) : undefined;

    // last :: [a] -> a
    const last = xs => xs.length ? xs.slice(-1)[0] : undefined;

    // max :: Ord a => a -> a -> a
    const max = (a, b) => b > a ? b : a;

    // minimum :: [a] -> a
    const minimum = xs =>
        xs.reduce((a, x) => (x < a || a === undefined ? x : a), undefined);

    // tail :: [a] -> [a]
    const tail = xs => xs.length ? xs.slice(1) : undefined;

    // Function of arity 3 mapped over nth items of each of 3 lists
    // zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
    const zipWith3 = (f, xs, ys, zs) =>
        Array.from({
            length: minimum([xs.length, ys.length, zs.length])
        }, (_, i) => f(xs[i], ys[i], zs[i]));


    // TEST -------------------------------------------------------------------
    return maxPathSum([
        [55],
        [94, 48],
        [95, 30, 96],
        [77, 71, 26, 67],
        [97, 13, 76, 38, 45],
        [7, 36, 79, 16, 37, 68],
        [48, 7, 9, 18, 70, 26, 6],
        [18, 72, 79, 46, 59, 79, 29, 90],
        [20, 76, 87, 11, 32, 7, 7, 49, 18],
        [27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
        [14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
        [2, 90, 3, 60, 48, 49, 41, 46, 33, 36, 47, 23],
        [92, 50, 48, 2, 36, 59, 42, 79, 72, 20, 82, 77, 42],
        [56, 78, 38, 80, 39, 75, 2, 71, 66, 66, 1, 3, 55, 72],
        [44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
        [85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 1, 1, 99, 89, 52],
        [6, 71, 28, 75, 94, 48, 37, 10, 23, 51, 6, 48, 53, 18, 74, 98, 15],
        [27, 2, 92, 23, 8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]
    ]);
})();
```

<lang>1320
```



## jq

The following implementation illustrates the use of an inner function as a helper function, which is used here mainly for clarity. The inner function in effect implements the inner loop;
the outer loop is implemented using <tt>reduce</tt>.

The input array is identical to that in the Javascript section and is therefore omitted here.
```jq
# Usage: TRIANGLE | solve
def solve:

  # update(next) updates the input row of maxima:
  def update(next):
    . as $maxima
    | [ range(0; next|length)
       | next[.] + ([$maxima[.], $maxima[. + 1]] | max) ];

  . as $in
  | reduce range(length -2; -1; -1) as $i
      ($in[-1];  update( $in[$i] ) ) ;
```



## Julia

```julia
# dynamic solution
function maxpathsum(t::Array{Array{I, 1}, 1}) where I
    T = deepcopy(t)
    for r in length(T)-1:-1:1
        for c in linearindices(T[r])
            T[r][c] += max(T[r+1][c], T[r+1][c+1])
        end
    end
    return T[1][1]
end

test = [[55],
        [94, 48],
        [95, 30, 96],
        [77, 71, 26, 67],
        [97, 13, 76, 38, 45],
        [07, 36, 79, 16, 37, 68],
        [48, 07, 09, 18, 70, 26, 06],
        [18, 72, 79, 46, 59, 79, 29, 90],
        [20, 76, 87, 11, 32, 07, 07, 49, 18],
        [27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
        [14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
        [02, 90, 03, 60, 48, 49, 41, 46, 33, 36, 47, 23],
        [92, 50, 48, 02, 36, 59, 42, 79, 72, 20, 82, 77, 42],
        [56, 78, 38, 80, 39, 75, 02, 71, 66, 66, 01, 03, 55, 72],
        [44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
        [85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 01, 01, 99, 89, 52],
        [06, 71, 28, 75, 94, 48, 37, 10, 23, 51, 06, 48, 53, 18, 74, 98, 15],
        [27, 02, 92, 23, 08, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]]

@show maxpathsum(test)
```


```txt
maxpathsum(test) = 1320
```



## Kotlin

```scala
// version 1.1.2

val tri = intArrayOf(
    55,
    94, 48,
    95, 30, 96,
    77, 71, 26, 67,
    97, 13, 76, 38, 45,
     7, 36, 79, 16, 37, 68,
    48,  7,  9, 18, 70, 26,  6,
    18, 72, 79, 46, 59, 79, 29, 90,
    20, 76, 87, 11, 32,  7,  7, 49, 18,
    27, 83, 58, 35, 71, 11, 25, 57, 29, 85,
    14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55,
     2, 90,  3, 60, 48, 49, 41, 46, 33, 36, 47, 23,
    92, 50, 48,  2, 36, 59, 42, 79, 72, 20, 82, 77, 42,
    56, 78, 38, 80, 39, 75,  2, 71, 66, 66,  1,  3, 55, 72,
    44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36,
    85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17,  1,  1, 99, 89, 52,
     6, 71, 28, 75, 94, 48, 37, 10, 23, 51,  6, 48, 53, 18, 74, 98, 15,
    27,  2, 92, 23,  8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93
)

fun main(args: Array<String>) {
    val triangles = arrayOf(tri.sliceArray(0..9), tri)
    for (triangle in triangles) {
        val size  = triangle.size
        val base  = ((Math.sqrt(8.0 * size + 1.0) - 1.0)/ 2.0).toInt()
        var step  = base - 1
        var stepc = 0
        for (i in (size - base - 1) downTo 0) {
            triangle[i] += maxOf(triangle[i + step], triangle[i + step + 1])
            if (++stepc == step) {
                step--
                stepc = 0
            }
        }
        println("Maximum total  = ${triangle[0]}")
    }
}
```


```txt

Maximum total  = 321
Maximum total  = 1320

```



## Lua


While the solutions here are clever, I found most of them to be hard to follow. In fact, none of them are very good for showing how the algorithm works. So I wrote this Lua version for maximum readability.


```lua
local triangleSmall = {
    { 55 },
    { 94, 48 },
    { 95, 30, 96 },
    { 77, 71, 26, 67 },
}

local triangleLarge = {
    { 55 },
    { 94, 48 },
    { 95, 30, 96 },
    { 77, 71, 26, 67 },
    { 97, 13, 76, 38, 45 },
    {  7, 36, 79, 16, 37, 68 },
    { 48,  7,  9, 18, 70, 26,  6 },
    { 18, 72, 79, 46, 59, 79, 29, 90 },
    { 20, 76, 87, 11, 32,  7,  7, 49, 18 },
    { 27, 83, 58, 35, 71, 11, 25, 57, 29, 85 },
    { 14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55 },
    {  2, 90,  3, 60, 48, 49, 41, 46, 33, 36, 47, 23 },
    { 92, 50, 48,  2, 36, 59, 42, 79, 72, 20, 82, 77, 42 },
    { 56, 78, 38, 80, 39, 75,  2, 71, 66, 66,  1,  3, 55, 72 },
    { 44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36 },
    { 85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17,  1,  1, 99, 89, 52 },
    {  6, 71, 28, 75, 94, 48, 37, 10, 23, 51,  6, 48, 53, 18, 74, 98, 15 },
    { 27,  2, 92, 23,  8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93 },
};

function solve(triangle)

    -- Get total number of rows in triangle.
    local nRows = table.getn(triangle)

    -- Start at 2nd-to-last row and work up to the top.
    for row = nRows-1, 1, -1 do

        -- For each value in row, add the max of the 2 children beneath it.
        for i = 1, row do
            local child1 = triangle[row+1][i]
            local child2 = triangle[row+1][i+1]
            triangle[row][i] = triangle[row][i] + math.max(child1, child2)
        end

    end

    -- The top of the triangle now holds the answer.
    return triangle[1][1];

end

print(solve(triangleSmall))
print(solve(triangleLarge))

```


```txt
321
1320
```




## Mathematica


```Mathematica
nums={{55},{94,48},{95,30,96},{77,71,26,67},{97,13,76,38,45},{7,36,79,16,37,68},{48,7,9,18,70,26,6},{18,72,79,46,59,79,29,90},{20,76,87,11,32,7,7,49,18},{27,83,58,35,71,11,25,57,29,85},{14,64,36,96,27,11,58,56,92,18,55},{2,90,3,60,48,49,41,46,33,36,47,23},{92,50,48,2,36,59,42,79,72,20,82,77,42},{56,78,38,80,39,75,2,71,66,66,1,3,55,72},{44,25,67,84,71,67,11,61,40,57,58,89,40,56,36},{85,32,25,85,57,48,84,35,47,62,17,1,1,99,89,52},{6,71,28,75,94,48,37,10,23,51,6,48,53,18,74,98,15},{27,2,92,23,8,71,76,84,15,52,92,63,81,10,44,10,69,93}};
ClearAll[DoStep,MaximumTrianglePathSum]
DoStep[lst1_List,lst2_List]:=lst2+Join[{First[lst1]},Max/@Partition[lst1,2,1],{Last[lst1]}]
MaximumTrianglePathSum[triangle_List]:=Max[Fold[DoStep,First[triangle],Rest[triangle]]]
```

```txt

MaximumTrianglePathSum[nums]
1320

```



## Nim

```nim
import strutils, future

proc solve(tri): int =
  var tri = tri
  while tri.len > 1:
    let t0 = tri.pop
    for i, t in tri[tri.high]: tri[tri.high][i] = max(t0[i], t0[i+1]) + t
  tri[0][0]

const data = """
                          55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93"""

echo solve data.splitLines.map((x: string) => x.split.map parseInt)
```

```txt
1320
```



## PARI/GP


```parigp
V=[[55],[94,48],[95,30,96],[77,71,26,67],[97,13,76,38,45],[07,36,79,16,37,68],[48,07,09,18,70,26,06],[18,72,79,46,59,79,29,90],[20,76,87,11,32,07,07,49,18],[27,83,58,35,71,11,25,57,29,85],[14,64,36,96,27,11,58,56,92,18,55],[02,90,03,60,48,49,41,46,33,36,47,23],[92,50,48,02,36,59,42,79,72,20,82,77,42],[56,78,38,80,39,75,02,71,66,66,01,03,55,72],[44,25,67,84,71,67,11,61,40,57,58,89,40,56,36],[85,32,25,85,57,48,84,35,47,62,17,01,01,99,89,52],[06,71,28,75,94,48,37,10,23,51,06,48,53,18,74,98,15],[27,02,92,23,08,71,76,84,15,52,92,63,81,10,44,10,69,93]];
forstep(i=#V,2,-1,V[i-1]+=vector(i-1,j,max(V[i][j],V[i][j+1]))); V[1][1]
```

```txt
%1 = 1320
```


## Pascal

testet with freepascal, should run under Turbo Pascal, therefore using static array and val,  and Delphi too.

```pascal
program TriSum;
{'triangle.txt'
* one element per line
55
94
48
95
30
96
...}
const
 cMaxTriHeight = 18;
 cMaxTriElemCnt = (cMaxTriHeight+1)*cMaxTriHeight DIV 2 +1;
type
  tElem = longint;
  tbaseRow =  array[0..cMaxTriHeight] of tElem;
  tmyTri   =  array[0..cMaxTriElemCnt] of tElem;

function ReadTri(     fname:string;
                  out     t:tmyTri):integer;
{read triangle values into t and returns height}
var
  f : text;
  s : string;
  i : integer;
  ValCode : word;
begin
  i := 0;
  fillchar(t,Sizeof(t),#0);

  Assign(f,fname);
  {$I-}
  reset(f);
  IF ioResult <> 0 then
  begin
    writeln('IO-Error ',ioResult);
    close(f);
    ReadTri := i;
    EXIT;
  end;
  {$I+}

  while NOT(EOF(f)) AND (i<cMaxTriElemCnt) do
  begin
    readln(f,s);
    val(s,t[i],ValCode);
    inc(i);
    IF ValCode <> 0 then
    begin
      writeln(ValCode,' conversion error at line ',i);
      fillchar(t,Sizeof(t),#0);
      i := 0;
      BREAK;
    end;
  end;
  close(f);
  ReadTri := round(sqrt(2*(i-1)));
end;

function TriMaxSum(var t: tmyTri;hei:integer):integer;
{sums up higher values bottom to top}
var
  i,r,h,tmpMax : integer;
  idxN : integer;
  sumrow : tbaseRow;
begin
  h := hei;
  idxN := (h*(h+1)) div 2 -1;
  {copy base row}
  move(t[idxN-h+1],sumrow[0],SizeOf(tElem)*h);
  dec(h);
{  for r := 0 to h do write(sumrow[r]:4);writeln;}
  idxN := idxN-h;
  while idxN >0 do
  begin
    i := idxN-h;
    r := 0;
    while r < h do
    begin
      tmpMax:= sumrow[r];
      IF tmpMax<sumrow[r+1] then
        tmpMax:=sumrow[r+1];
      sumrow[r]:= tmpMax+t[i];
      inc(i);
      inc(r);
    end;
    idxN := idxN-h;
    dec(h);
{  for r := 0 to h do write(sumrow[r]:4);writeln;}
  end;
  TriMaxSum := sumrow[0];
end;

var
  h : integer;
  triangle : tmyTri;
Begin
{  writeln(TriMaxSum(triangle,ReadTri('triangle.txt',triangle))); -> 1320}
  h := ReadTri('triangle.txt',triangle);
  writeln('height sum');
  while h > 0 do
  begin
    writeln(h:4,TriMaxSum(triangle,h):7);
    dec(h);
  end;
end.
```

```txt
height sum
  18   1320
  17   1249
....
   4    321
   3    244
   2    149
   1     55
```



## Perl


```perl
use 5.10.0;
use List::Util 'max';

my @sum;
while (<>) {
	my @x = split;
	@sum = ($x[0] + $sum[0],
		map($x[$_] + max(@sum[$_-1, $_]), 1 .. @x-2),
		$x[-1] + $sum[-1]);
}

say max(@sum);
```

```txt

% perl maxpath.pl triangle.txt
1320

```


## Perl 6

The <tt>Z+</tt> and <tt>Zmax</tt> are examples of the zipwith metaoperator. Note also we can use the <tt>Zmax</tt> metaoperator form because <tt>max</tt> is define as an infix in Perl 6.

```perl6
my $triangle = q|         55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93|;


my @rows = $triangle.lines.map: { [.words] }
while @rows > 1 {
    my @last := @rows.pop;
    @rows[*-1] = (@rows[*-1][] Z+ (@last Zmax @last[1..*])).List;
}
put @rows;


# Here's a more FPish version. We define our own operator and the use it in the reduction metaoperator form, [op], which turns any infix into a list operator.
sub infix:<op>(@a,@b) { (@a Zmax @a[1..*]) Z+ @b }
put [op] $triangle.lines.reverse.map: { [.words] }


# Or, instead of using reverse, one could also define the op as right-associative.
sub infix:<rop>(@a,@b) is assoc('right') { @a Z+ (@b Zmax @b[1..*]) }
put [rop] $triangle.lines.map: { [.words] }
```


```txt
1320
1320
1320
```



## Phix


```Phix
sequence tri = {{55},
                {94, 48},
                {95, 30, 96},
                {77, 71, 26, 67},
                {97, 13, 76, 38, 45},
                { 7, 36, 79, 16, 37, 68},
                {48,  7,  9, 18, 70, 26,  6},
                {18, 72, 79, 46, 59, 79, 29, 90},
                {20, 76, 87, 11, 32,  7,  7, 49, 18},
                {27, 83, 58, 35, 71, 11, 25, 57, 29, 85},
                {14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55},
                { 2, 90,  3, 60, 48, 49, 41, 46, 33, 36, 47, 23},
                {92, 50, 48,  2, 36, 59, 42, 79, 72, 20, 82, 77, 42},
                {56, 78, 38, 80, 39, 75,  2, 71, 66, 66,  1,  3, 55, 72},
                {44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36},
                {85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17,  1,  1, 99, 89, 52},
                { 6, 71, 28, 75, 94, 48, 37, 10, 23, 51,  6, 48, 53, 18, 74, 98, 15},
                {27,  2, 92, 23,  8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93}}

-- update each row from last but one upwards, with the larger
--  child, so the first step is to replace 6 with 6+27 or 6+2.
for r=length(tri)-1 to 1 by -1 do
    for c=1 to length(tri[r]) do
        tri[r][c] += max(tri[r+1][c..c+1])
    end for
end for
?tri[1][1]
```

```txt

1320

```



## PicoLisp

```PicoLisp
(de maxpath (Lst)
   (let (Lst (reverse Lst)  R (car Lst))
      (for I (cdr Lst)
         (setq R
            (mapcar
               +
               (maplist
                  '((L)
                     (and (cdr L) (max (car L) (cadr L))) )
                  R )
               I ) ) )
      (car R) ) )
```



## PL/I

```pli
*process source xref attributes or(!);
 triang: Proc Options(Main);
 Dcl nn(18,18)  Bin Fixed(31);
 Dcl (rows,i,j) Bin Fixed(31);
 Dcl (p,k,kn)   Bin Fixed(31);
 Call f_r(1 ,'                           55                         ');
 Call f_r(2 ,'                         94 48                        ');
 Call f_r(3 ,'                        95 30 96                      ');
 Call f_r(4 ,'                      77 71 26 67                     ');
 Call f_r(5 ,'                     97 13 76 38 45                   ');
 Call f_r(6 ,'                   07 36 79 16 37 68                  ');
 Call f_r(7 ,'                  48 07 09 18 70 26 06                ');
 Call f_r(8 ,'                18 72 79 46 59 79 29 90               ');
 Call f_r(9 ,'               20 76 87 11 32 07 07 49 18             ');
 Call f_r(10,'             27 83 58 35 71 11 25 57 29 85            ');
 Call f_r(11,'            14 64 36 96 27 11 58 56 92 18 55          ');
 Call f_r(12,'          02 90 03 60 48 49 41 46 33 36 47 23         ');
 Call f_r(13,'         92 50 48 02 36 59 42 79 72 20 82 77 42       ');
 Call f_r(14,'       56 78 38 80 39 75 02 71 66 66 01 03 55 72      ');
 Call f_r(15,'      44 25 67 84 71 67 11 61 40 57 58 89 40 56 36    ');
 Call f_r(16,'    85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52   ');
 Call f_r(17,'   06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15 ');
 Call f_r(18,' 27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93');
 rows=hbound(nn,1);

 do r=rows by -1 to 2;
   p=r-1;                           /*traipse through triangle rows. */
   do k=1 to p;
     kn=k+1;                        /*re-calculate the previous row. */
     nn(p,k)=max(nn(r,k),nn(r,kn))+nn(p,k);  /*replace previous nn   */
     end;
   end;
 Put Edit('maximum path sum:',nn(1,1))(Skip,a,f(5)); /*display result*/
 f_r: Proc(r,vl);
 /* fill row r with r values */
 Dcl r Bin Fixed(31);
 Dcl vl Char(*);
 Dcl vla Char(100) Var;
 vla=' '!!trim(vl);
 get string(vla) Edit((nn(r,j) Do j=1 To r))(f(3));
 End;
 End;
```

```txt
maximum path sum: 1320
```



## Prolog


```Prolog
max_path(N, V) :-
	data(N, T),
	path(0, T, V).

path(_N, [], 0) .
path(N, [H | T], V) :-
	nth0(N, H, V0),
	N1 is N+1,
	path(N, T, V1),
	path(N1, T, V2),
	V is V0 +  max(V1, V2).

data(1, P) :-
	P =
	[ [55],
	  [94, 48],
	  [95, 30, 96],
	  [77, 71, 26, 67]].


data(2, P) :-
	P =
	[ [55],
	  [94, 48],
	  [95, 30, 96],
	  [77, 71, 26, 67],
	  [97, 13, 76, 38, 45],
	  [7, 36, 79, 16, 37, 68],
	  [48, 7, 9, 18, 70, 26, 6],
	  [18, 72, 79, 46, 59, 79, 29, 90],
	  [20, 76, 87, 11, 32, 7, 7, 49, 18],
	  [27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
	  [14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
	  [2, 90, 3, 60, 48, 49, 41, 46, 33, 36, 47, 23],
	  [92, 50, 48, 2, 36, 59, 42, 79, 72, 20, 82, 77, 42],
	  [56, 78, 38, 80, 39, 75, 2, 71, 66, 66, 1, 3, 55, 72],
	  [44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
	  [85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 1, 1, 99, 89, 52],
	  [6, 71, 28, 75, 94, 48, 37, 10, 23, 51, 6, 48, 53, 18, 74, 98, 15],
	  [27, 2, 92, 23, 8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]].


```

```txt
 ?- max_path(1, V).
V = 321 .

 ?- max_path(2, V).
V = 1320 .


```



## Python

A simple mostly imperative solution:

```python
def solve(tri):
    while len(tri) > 1:
        t0 = tri.pop()
        t1 = tri.pop()
        tri.append([max(t0[i], t0[i+1]) + t for i,t in enumerate(t1)])
    return tri[0][0]


data = """                55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93"""

print solve([map(int, row.split()) for row in data.splitlines()])
```

```txt
1320
```


A more functional version, similar to the Haskell entry (same output):

```python
from itertools import imap

f = lambda x, y, z: x + max(y, z)
g = lambda xs, ys: list(imap(f, ys, xs, xs[1:]))
data = [map(int, row.split()) for row in open("triangle.txt")][::-1]
print reduce(g, data)[0]
```


And, updating a little for Python 3 (in which ''itertools'' no longer defines '''imap''', and '''reduce''' now has to be imported from ''functools''), while inlining the data for ease of testing:
```python
'''Maximum triangle path sum'''

from functools import (reduce)


# maxPathSum :: [[Int]] -> Int
def maxPathSum(rows):
    '''The maximum total among all possible
       paths from the top to the bottom row.
    '''
    return reduce(
        lambda xs, ys: [
            a + max(b, c) for (a, b, c) in zip(ys, xs, xs[1:])
        ],
        reversed(rows[:-1]), rows[-1]
    )[0]


# TEST ----------------------------------------------------
print(
    maxPathSum([
        [55],
        [94, 48],
        [95, 30, 96],
        [77, 71, 26, 67],
        [97, 13, 76, 38, 45],
        [7, 36, 79, 16, 37, 68],
        [48, 7, 9, 18, 70, 26, 6],
        [18, 72, 79, 46, 59, 79, 29, 90],
        [20, 76, 87, 11, 32, 7, 7, 49, 18],
        [27, 83, 58, 35, 71, 11, 25, 57, 29, 85],
        [14, 64, 36, 96, 27, 11, 58, 56, 92, 18, 55],
        [2, 90, 3, 60, 48, 49, 41, 46, 33, 36, 47, 23],
        [92, 50, 48, 2, 36, 59, 42, 79, 72, 20, 82, 77, 42],
        [56, 78, 38, 80, 39, 75, 2, 71, 66, 66, 1, 3, 55, 72],
        [44, 25, 67, 84, 71, 67, 11, 61, 40, 57, 58, 89, 40, 56, 36],
        [85, 32, 25, 85, 57, 48, 84, 35, 47, 62, 17, 1, 1, 99, 89, 52],
        [6, 71, 28, 75, 94, 48, 37, 10, 23, 51, 6, 48, 53, 18, 74, 98, 15],
        [27, 2, 92, 23, 8, 71, 76, 84, 15, 52, 92, 63, 81, 10, 44, 10, 69, 93]
    ])
)
```

```txt
1320
```



## Racket


```racket
#lang racket
(require math/number-theory)

(define (trinv n) ; OEIS A002024
  (exact-floor (/ (+ 1 (sqrt (* 1 (* 8 n)))) 2)))

(define (triangle-neighbour-bl n)
  (define row (trinv n))
  (+ n (- (triangle-number row) (triangle-number (- row 1)))))

(define (maximum-triangle-path-sum T)
  (define n-rows (trinv (vector-length T)))
  (define memo# (make-hash))
  (define (inner i)
    (hash-ref!
     memo# i
     (λ ()
       (+ (vector-ref T (sub1 i)) ; index is 1-based (so vector-refs need -1'ing)
          (cond [(= (trinv i) n-rows) 0]
                [else
                 (define bl (triangle-neighbour-bl i))
                 (max (inner bl) (inner (add1 bl)))])))))
  (inner 1))

(module+ main
  (maximum-triangle-path-sum
   #(55
     94 48
     95 30 96
     77 71 26 67
     97 13 76 38 45
     07 36 79 16 37 68
     48 07 09 18 70 26 06
     18 72 79 46 59 79 29 90
     20 76 87 11 32 07 07 49 18
     27 83 58 35 71 11 25 57 29 85
     14 64 36 96 27 11 58 56 92 18 55
     02 90 03 60 48 49 41 46 33 36 47 23
     92 50 48 02 36 59 42 79 72 20 82 77 42
     56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
     85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
     06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
     27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93)))

(module+ test
  (require rackunit)
  (check-equal? (for/list ((n (in-range 1 (add1 10)))) (trinv n)) '(1 2 2 3 3 3 4 4 4 4))
  ;    1
  ;   2 3
  ;  4 5 6
  ; 7 8 9 10
  (check-eq? (triangle-neighbour-bl 1) 2)
  (check-eq? (triangle-neighbour-bl 3) 5)
  (check-eq? (triangle-neighbour-bl 5) 8)
  (define test-triangle
    #(55   94 48   95 30 96   77 71 26 67))
  (check-equal? (maximum-triangle-path-sum test-triangle) 321)
  )
```

```txt
1320
```



## REXX

The method used is very efficient and performs very well for triangles that have thousands of rows (lines).

For an expanded discussion of the program method's efficiency, see the discussion page.

```rexx
/*REXX program finds the  maximum sum  of a  path of numbers  in a pyramid of numbers.  */
@.=.;                   @.1  =                            55
                        @.2  =                          94 48
                        @.3  =                         95 30 96
                        @.4  =                       77 71 26 67
                        @.5  =                      97 13 76 38 45
                        @.6  =                    07 36 79 16 37 68
                        @.7  =                   48 07 09 18 70 26 06
                        @.8  =                 18 72 79 46 59 79 29 90
                        @.9  =                20 76 87 11 32 07 07 49 18
                        @.10 =              27 83 58 35 71 11 25 57 29 85
                        @.11 =             14 64 36 96 27 11 58 56 92 18 55
                        @.12 =           02 90 03 60 48 49 41 46 33 36 47 23
                        @.13 =          92 50 48 02 36 59 42 79 72 20 82 77 42
                        @.14 =        56 78 38 80 39 75 02 71 66 66 01 03 55 72
                        @.15 =       44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
                        @.16 =     85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
                        @.17 =    06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
                        @.18 =  27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93
#.=0
      do    r=1  while  @.r\==.                  /*build another version of the pyramid.*/
         do k=1  for r;    #.r.k=word(@.r, k)    /*assign a number to an array number.  */
         end   /*k*/
      end      /*r*/

      do    r=r-1  by -1  to 2;         p=r-1    /*traipse through the pyramid rows.    */
         do k=1    for p;               _=k+1    /*re─calculate the previous pyramid row*/
         #.p.k=max(#.r.k, #.r._)    +   #.p.k    /*replace the previous number.         */
         end   /*k*/
      end      /*r*/
                                                 /*stick a fork in it,  we're all done. */
say 'maximum path sum: '       #.1.1             /*show the top (row 1) pyramid number. */
```

'''output'''   using the data within the REXX program:

```txt

maximum path sum:  1320

```



## Ring


```ring

# Project : Maximum triangle path sum

load "stdlib.ring"
ln = list(19)
ln[1] = "                   55"
ln[2] = "                  94 48"
ln[3] = "                95 30 96"
ln[4] = "               77 71 26 67"
ln[5] = "              97 13 76 38 45"
ln[6] = "             07 36 79 16 37 68"
ln[7] = "            48 07 09 18 70 26 06"
ln[8] = "           18 72 79 46 59 79 29 90"
ln[9] = "          20 76 87 11 32 07 07 49 18"
ln[10] = "         27 83 58 35 71 11 25 57 29 85"
ln[11] = "        14 64 36 96 27 11 58 56 92 18 55"
ln[12] = "       02 90 03 60 48 49 41 46 33 36 47 23"
ln[13] = "      92 50 48 02 36 59 42 79 72 20 82 77 42"
ln[14] = "     56 78 38 80 39 75 02 71 66 66 01 03 55 72"
ln[15] = "    44 25 67 84 71 67 11 61 40 57 58 89 40 56 36"
ln[16] = "   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52"
ln[17] = "  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15"
ln[18] = " 27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93"
ln[19] = "end"

matrix = newlist(20,20)
x = 1
size = 0

for n = 1 to len(ln) - 1
     ln2 = ln[n]
     ln2 = trim(ln2)
     for y = 1 to x
          matrix[x][y] = number(left(ln2,2))
          if len(ln2) > 4
              ln2 = substr(ln2,4,len(ln2)-4)
          ok
     next
     x = x + 1
     size = size + 1
next

for x = size - 1 to 1 step - 1
     for y = 1 to x
          s1 = matrix[x+1][y]
          s2 = matrix[x+1][y+1]
          if s1 > s2
             matrix[x][y] = matrix[x][y] + s1
          else
             matrix[x][y] = matrix[x][y] + s2
          ok
     next
next

see "maximum triangle path sum = " + matrix[1][1]

```

Output:

```txt

maximum triangle path sum = 1320

```



## Ruby


```ruby
triangle =
"                         55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93"

ar = triangle.each_line.map{|line| line.split.map(&:to_i)}
puts ar.inject([]){|res,x|
  maxes = [0, *res, 0].each_cons(2).map(&:max)
  x.zip(maxes).map{|a,b| a+b}
}.max
# => 1320
```



## Rust

```rust
use std::cmp::max;

fn max_path(vector: &mut Vec<Vec<u32>>) -> u32 {

    while vector.len() > 1 {

        let last = vector.pop().unwrap();
        let ante = vector.pop().unwrap();

        let mut new: Vec<u32> = Vec::new();

        for (i, value) in ante.iter().enumerate() {
            new.push(max(last[i], last[i+1]) + value);
        };

        vector.push(new);
    };

    vector[0][0]
}

fn main() {
    let mut data = "55
94 48
95 30 96
77 71 26 67
97 13 76 38 45
07 36 79 16 37 68
48 07 09 18 70 26 06
18 72 79 46 59 79 29 90
20 76 87 11 32 07 07 49 18
27 83 58 35 71 11 25 57 29 85
14 64 36 96 27 11 58 56 92 18 55
02 90 03 60 48 49 41 46 33 36 47 23
92 50 48 02 36 59 42 79 72 20 82 77 42
56 78 38 80 39 75 02 71 66 66 01 03 55 72
44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93";

    let mut vector = data.split("\n").map(|x| x.split(" ").map(|s: &str| s.parse::<u32>().unwrap())
        .collect::<Vec<u32>>()).collect::<Vec<Vec<u32>>>();

    let max_value = max_path(&mut vector);

    println!("{}", max_value);
    //=> 7273
}
```



## Scala


```Scala
object MaximumTrianglePathSum extends App {
    // Solution:
    def sum(triangle: Array[Array[Int]]) =
        triangle.reduceRight((upper, lower) =>
            upper zip (lower zip lower.tail)
            map {case (above, (left, right)) => above + Math.max(left, right)}
        ).head

    // Tests:
    def triangle = """
                          55
                        94 48
                       95 30 96
                     77 71 26 67
    """
    def parse(s: String) = s.trim.split("\\s+").map(_.toInt)
    def parseLines(s: String) = s.trim.split("\n").map(parse)
    def parseFile(f: String) = scala.io.Source.fromFile(f).getLines.map(parse).toArray
    println(sum(parseLines(triangle)))
    println(sum(parseFile("triangle.txt")))
}
```

```txt
321
1320
```



## Sidef

Iterative solution:

```ruby
var sum = [0]

ARGF.each {  |line|
    var x = line.words.map{.to_n}
    sum = [
            x.first + sum.first,
            1 ..^ x.end -> map{|i| x[i] + [sum[i-1, i]].max}...,
            x.last + sum.last,
          ]
}

say sum.max
```


Recursive solution:

```ruby
var triangle = ARGF.slurp.lines.map{.words.map{.to_n}}
 
func max_value(i=0, j=0) is cached {
    i == triangle.len && return 0
    triangle[i][j] + [max_value(i+1, j), max_value(i+1, j+1)].max
}
 
say max_value()
```

```txt
% sidef maxpath.sf triangle.txt
1320
```



## Stata


```stata
import delimited triangle.txt, delim(" ") clear
mata
a = st_data(.,.)
n = rows(a)
for (i=n-1; i>=1; i--) {
	for (j=1; j<=i; j++) {
		a[i,j] = a[i,j]+max((a[i+1,j],a[i+1,j+1]))
	}
}
a[1,1]
end
```


'''Output'''


```txt
1320
```



## Tcl

```tcl
package require Tcl 8.6

proc maxTrianglePathSum {definition} {
    # Parse the definition, stripping whitespace and leading zeroes.
    set lines [lmap line [split [string trim $definition] "\n"] {
	lmap val $line {scan $val %d}
    }]
    # Paths are bit strings (0 = go left, 1 = go right).
    # Enumerate the possible paths.
    set numPaths [expr {2 ** [llength $lines]}]
    for {set path 0; set max -inf} {$path < $numPaths} {incr path} {
	# Work out how much the current path costs.
	set sum [set idx [set row 0]]
	for {set bit 1} {$row < [llength $lines]} {incr row} {
	    incr sum [lindex $lines $row $idx]
	    if {$path & $bit} {incr idx}
	    set bit [expr {$bit << 1}]
	}
	# Remember the max so far.
	if {$sum > $max} {set max $sum}
    }
    return $max
}

puts [maxTrianglePathSum {
                          55
                        94 48
                       95 30 96
                     77 71 26 67
                    97 13 76 38 45
                  07 36 79 16 37 68
                 48 07 09 18 70 26 06
               18 72 79 46 59 79 29 90
              20 76 87 11 32 07 07 49 18
            27 83 58 35 71 11 25 57 29 85
           14 64 36 96 27 11 58 56 92 18 55
         02 90 03 60 48 49 41 46 33 36 47 23
        92 50 48 02 36 59 42 79 72 20 82 77 42
      56 78 38 80 39 75 02 71 66 66 01 03 55 72
     44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
   85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
  06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93
}]
# Reading from a file is left as an exercise…
```

```txt

1320

```



## VBScript


```vb

'Solution derived from http://stackoverflow.com/questions/8002252/euler-project-18-approach.

Set objfso = CreateObject("Scripting.FileSystemObject")
Set objinfile = objfso.OpenTextFile(objfso.GetParentFolderName(WScript.ScriptFullName) &_
	"\triangle.txt",1,False)

row = Split(objinfile.ReadAll,vbCrLf)

For i = UBound(row) To 0 Step -1
	row(i) = Split(row(i)," ")
	If i < UBound(row) Then
		For j = 0 To UBound(row(i))
			If (row(i)(j) + row(i+1)(j)) > (row(i)(j) + row(i+1)(j+1)) Then
				row(i)(j) = CInt(row(i)(j)) + CInt(row(i+1)(j))
			Else
				row(i)(j) = CInt(row(i)(j)) + CInt(row(i+1)(j+1))
			End If
		Next
	End If
Next

WScript.Echo row(0)(0)

objinfile.Close
Set objfso = Nothing

```


Input file

```txt

55
94 48
95 30 96
77 71 26 67
97 13 76 38 45
07 36 79 16 37 68
48 07 09 18 70 26 06
18 72 79 46 59 79 29 90
20 76 87 11 32 07 07 49 18
27 83 58 35 71 11 25 57 29 85
14 64 36 96 27 11 58 56 92 18 55
02 90 03 60 48 49 41 46 33 36 47 23
92 50 48 02 36 59 42 79 72 20 82 77 42
56 78 38 80 39 75 02 71 66 66 01 03 55 72
44 25 67 84 71 67 11 61 40 57 58 89 40 56 36
85 32 25 85 57 48 84 35 47 62 17 01 01 99 89 52
06 71 28 75 94 48 37 10 23 51 06 48 53 18 74 98 15
27 02 92 23 08 71 76 84 15 52 92 63 81 10 44 10 69 93

```


```txt
1320
```



## zkl

The two Python solutions:

```zkl
tri:=File("triangle.txt").pump(List,fcn(s){ s.strip().split(" ").apply("toInt") }).copy();
while(tri.len()>1){
   t0:=tri.pop();
   t1:=tri.pop();
   tri.append( [[(it); t1.enumerate();
	'wrap([(i,t)]){ t + t0[i].max(t0[i+1]) }]])
}
tri[0][0].println();
```


```zkl
data:=File("triangle.txt").pump(List,fcn(s){ s.strip().split(" ").apply("toInt") });
fcn f(x,y,z){ x + y.max(z) }
fcn g(xs,ys){ Utils.zipWith(f,ys,xs,xs[1,*]); }
data.reverse().reduce(g)[0].println();
```

```zkl
lines:=File("triangle.txt").pump(List,fcn(s){ s.strip().split(" ").apply("toInt") });
d:=lines[-1].copy();
foreach row in ([lines.len()-2..0,-1]){
   d1:=d[1,*];
   l :=d[0];
   foreach i,u in (lines[row].enumerate()){
       d[i] = u + l.max(r:=d1[i]);
       l    = r;
    }
}
println(d[0]);
```

```txt

1320
1320
1320

```

