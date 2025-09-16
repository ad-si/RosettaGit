+++
title = "Sequence of non-squares"
description = ""
date = 2019-04-05T05:26:52Z
aliases = []
[extra]
id = 3015
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "autohotkey",
  "awk",
  "basic",
  "bbc_basic",
  "bc",
  "burlesque",
  "c",
  "c_sharp",
  "c_plus_plus",
  "clojure",
  "coffee_script",
  "common_lisp",
  "d",
  "echolisp",
  "eiffel",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "idl",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "liberty_basic",
  "logo",
  "lua",
  "mathematica",
  "matlab",
  "maxima",
  "min",
  "mmix",
  "nim",
]
+++

## Task

Show that the following remarkable formula gives the [http://www.research.att.com/~njas/sequences/A000037 sequence] of non-square [[wp:Natural_number|natural numbers]]:
  n + floor(1/2 + sqrt(n))
* Print out the values for   <big> n </big>   in the range   '''1'''   to   '''22'''
* Show that no squares occur for   <big> n </big>   less than one million


This sequence is also known as   [http://oeis.org/A000037 A000037]   in the '''OEIS''' database.





## Ada


```ada
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Sequence_Of_Non_Squares_Test is
   use Ada.Numerics.Long_Elementary_Functions;

   function Non_Square (N : Positive) return Positive is
   begin
      return N + Positive (Long_Float'Rounding (Sqrt (Long_Float (N))));
   end Non_Square;

   I : Positive;
begin
   for N in 1..22 loop -- First 22 non-squares
      Put (Natural'Image (Non_Square (N)));
   end loop;
   New_Line;
   for N in 1..1_000_000 loop -- Check first million of
      I := Non_Square (N);
      if I = Positive (Sqrt (Long_Float (I)))**2 then
         Put_Line ("Found a square:" & Positive'Image (N));
      end if;
   end loop;
end Sequence_Of_Non_Squares_Test;
```

```txt

 2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

```



## ALGOL 68

```algol68
PROC non square = (INT n)INT: n + ENTIER(0.5 + sqrt(n));

main: (

    # first 22 values (as a list) has no squares: #
    FOR i TO 22 DO
        print((whole(non square(i),-3),space))
    OD;
    print(new line);

    # The following check shows no squares up to one million:  #
    FOR i TO 1 000 000 DO
        REAL j = sqrt(non square(i));
        IF j = ENTIER j THEN
            put(stand out, ("Error: number is a square:", j, new line));
            stop
        FI
    OD
)
```

  2   3   5   6   7   8  10  11  12  13  14  15  17  18  19  20  21  22  23  24  26  27


## ALGOL W


```algolw
begin
    % check values of the function: f(n) = n + floor(1/2 + sqrt(n))    %
    % are not squares                                                  %

    integer procedure f ( integer value n ) ;
        begin
            n + entier( 0.5 + sqrt( n ) )
        end f ;

    logical noSquares;

    % first 22 values of f                                             %
    for n := 1 until 22 do writeon( i_w := 1, f( n ) );

    % check f(n) does not produce a square for n in 1..1 000 000       %
    noSquares := true;
    for n := 1 until 1000000 do begin
        integer fn, rn;
        fn := f( n );
        rn := round( sqrt( fn ) );
        if ( rn * rn ) = fn then begin
            write( "Found square at: ", n );
            noSquares := false
        end if_fn_is_a_square
    end for_n ;

    if noSquares then write( "f(n) did not produce a square in 1 .. 1 000 000" )
                 else write( "f(n) produced a square" )

end.
```

```txt

2  3  5  6  7  8  10  11  12  13  14  15  17  18  19  20  21  22  23  24  26  27
f(n) did not produce a square in 1 .. 1 000 000

```



## APL

Generate the first 22 numbers:

```apl
      NONSQUARE←{(⍳⍵)+⌊0.5+(⍳⍵)*0.5}
      NONSQUARE 22
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
```

Show there are no squares in the first million:

```apl
      HOWMANYSQUARES←{+⌿⍵=(⌊⍵*0.5)*2}
      HOWMANYSQUARES NONSQUARE 1000000
0
```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/post-276683.html#276683 discussion]

```AutoHotkey
Loop 22
   t .= (A_Index + floor(0.5 + sqrt(A_Index))) "  "
MsgBox %t%

s := 0
Loop 1000000
   x := A_Index + floor(0.5 + sqrt(A_Index)), s += x = round(sqrt(x))**2
Msgbox Number of bad squares = %s% ; 0
```



## AWK


```awk
$ awk 'func f(n){return(n+int(.5+sqrt(n)))}BEGIN{for(i=1;i<=22;i++)print i,f(i)}'
1 2
2 3
3 5
4 6
5 7
6 8
7 10
8 11
9 12
10 13
11 14
12 15
13 17
14 18
15 19
16 20
17 21
18 22
19 23
20 24
21 26
22 27

$ awk 'func f(n){return(n+int(.5+sqrt(n)))}BEGIN{for(i=1;i<100000;i++){n=f(i);r=int(sqrt(n));if(r*r==n)print n"is square"}}'
$
```



## BASIC

```freebasic
DIM i      AS Integer
DIM j      AS Double
DIM found  AS Integer

FUNCTION nonsqr (n AS Integer) AS Integer
    nonsqr = n + INT(0.5 + SQR(n))
END FUNCTION

' Display first 22 values
FOR i = 1 TO 22
    PRINT nonsqr(i); " ";
NEXT i
PRINT

' Check for squares up to one million
found = 0
FOR i = 1 TO 1000000
     j = SQR(nonsqr(i))
     IF j = INT(j) THEN
	 found = 1
         PRINT "Found square: "; i
         EXIT FOR
     END IF
NEXT i
IF found=0 THEN PRINT "No squares found"
```



## BBC BASIC


```bbcbasic
      FOR N% = 1 TO 22
        S% = N% + SQR(N%) + 0.5
        PRINT S%
      NEXT

      PRINT '"Checking...."
      FOR N% = 1 TO 999999
        S% = N% + SQR(N%) + 0.5
        R% = SQR(S%)
        IF S%/R% = R% STOP
      NEXT
      PRINT "No squares occur for n < 1000000"
```

```txt

         2
         3
         5
         6
         7
         8
        10
        11
        12
        13
        14
        15
        17
        18
        19
        20
        21
        22
        23
        24
        26
        27

Checking....
No squares occur for n < 1000000

```



## Bc


Since BC is an arbitrary precision calculator, there are no issues in sqrt (it is enough to increase the scale variable upto the desired ''precision''), nor there are limits (but time) to how many non-squares we can compute.


```bc
#! /usr/bin/bc

scale = 20

define ceil(x) {
    auto intx
    intx=int(x)
    if (intx<x) intx+=1
    return intx
}

define floor(x) {
    return -ceil(-x)
}

define int(x) {
    auto old_scale, ret
    old_scale=scale
    scale=0
    ret=x/1
    scale=old_scale
    return ret
}

define round(x) {
    if (x<0) x-=.5 else x+=.5
    return int(x)
}


define nonsqr(n) {
  return n + round(sqrt(n))
}

for(i=1; i < 23; i++) {
   print nonsqr(i), "\n"
}

for(i=1; i < 1000000; i++) {
  j = sqrt(nonsqr(i))
  if ( j == floor(j) ) {
    print i, " square in the seq\n"
  }
}

quit
```


The functions int, round, floor, ceil are taken from [http://www.pixelbeat.org/scripts/bc here] (int is slightly modified) ([http://www.pixelbeat.org/scripts/ Here] he states the license is GPL).


## Burlesque



```burlesque

1 22r@{?s0.5?+av?+}[m

```



## C


```c
#include <math.h>
#include <stdio.h>
#include <assert.h>

int nonsqr(int n) {
    return n + (int)(0.5 + sqrt(n));
    /* return n + (int)round(sqrt(n)); in C99 */
}

int main() {
    int i;

    /* first 22 values (as a list) has no squares: */
    for (i = 1; i < 23; i++)
        printf("%d ", nonsqr(i));
    printf("\n");

    /* The following check shows no squares up to one million: */
    for (i = 1; i < 1000000; i++) {
        double j = sqrt(nonsqr(i));
        assert(j != floor(j));
    }
    return 0;
}
```


## C#

```c#
using System;
using System.Diagnostics;

namespace sons
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int i = 1; i < 23; i++)
                Console.WriteLine(nonsqr(i));

            for (int i = 1; i < 1000000; i++)
            {
                double j = Math.Sqrt(nonsqr(i));
                Debug.Assert(j != Math.Floor(j),"Square");
            }
        }

        static int nonsqr(int i)
        {
            return (int)(i + Math.Floor(0.5 + Math.Sqrt(i)));
        }
    }
}
```



## C++


```cpp
#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>
#include <boost/bind.hpp>
#include <iterator>

double nextNumber( double number ) {
   return number + floor( 0.5 + sqrt( number ) ) ;
}

int main( ) {
   std::vector<double> non_squares ;
   typedef std::vector<double>::iterator SVI ;
   non_squares.reserve( 1000000 ) ;
   //create a vector with a million sequence numbers
   for ( double i = 1.0 ; i < 100001.0 ; i += 1 )
      non_squares.push_back( nextNumber( i ) ) ;
   //copy the first numbers to standard out
   std::copy( non_squares.begin( ) , non_squares.begin( ) + 22 ,
	 std::ostream_iterator<double>(std::cout, " " ) ) ;
   std::cout << '\n' ;
   //find if floor of square root equals square root( i. e. it's a square number )
   SVI found = std::find_if ( non_squares.begin( ) , non_squares.end( ) ,
	 boost::bind( &floor, boost::bind( &sqrt, _1 ) ) == boost::bind( &sqrt, _1 ) ) ;
   if ( found != non_squares.end( ) ) {
      std::cout << "Found a square number in the sequence!\n" ;
      std::cout << "It is " << *found << " !\n" ;
   }
   else {
      std::cout << "Up to 1000000, found no square number in the sequence!\n" ;
   }
   return 0 ;
}
```

```txt

2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
Up to 1000000, found no square number in the sequence!

```



## Clojure



```clojure
;; provides floor and sqrt, but we use Java's sqrt as it's faster
;; (Clojure's is more exact)
(use 'clojure.contrib.math)


(defn nonsqr [#^Integer n] (+ n (floor (+ 0.5 (Math/sqrt n)))))
(defn square? [#^Double n]
  (let [r (floor (Math/sqrt n))]
    (= (* r r) n)))

(doseq [n (range 1 23)] (printf "%s -> %s\n" n (nonsqr n)))

(defn verify [] (not-any? square? (map nonsqr (range 1 1000000))) )
```



## CoffeeScript


```coffeescript

non_square = (n) -> n + Math.floor(1/2 + Math.sqrt(n))

is_square = (n) ->
  r = Math.floor(Math.sqrt(n))
  r * r is n

do ->
  first_22_non_squares = (non_square i for i in [1..22])
  console.log first_22_non_squares

  # test is_square has no false negatives:
  for i in [1..10000]
    throw Error("is_square broken") unless is_square i*i

  # test non_square is valid for first million values of n
  for i in [1..1000000]
    throw Error("non_square broken") if is_square non_square(i)

  console.log "success"

```


```txt

> coffee foo.coffee
[ 2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27 ]
success

```



## Common Lisp


```lisp
(defun non-square-sequence ()
  (flet ((non-square (n)
	   "Compute the N-th number of the non-square sequence"
	   (+ n (floor (+ 1/2 (sqrt n)))))
	 (squarep (n)
	   "Tests, whether N is a square"
	   (let ((r (floor (sqrt n))))
	     (= (* r r) n))))
    (loop
       :for n :upfrom 1 :to 22
       :do (format t "~2D -> ~D~%" n (non-square n)))
    (loop
       :for n :upfrom 1 :to 1000000
       :when (squarep (non-square n))
       :do (format t "Found a square: ~D -> ~D~%"
		   n (non-square n)))))
```



## D


```d
import std.stdio, std.math, std.algorithm, std.range;

int nonSquare(in int n) pure nothrow @safe @nogc {
    return n + cast(int)(0.5 + real(n).sqrt);
}

void main() {
    iota(1, 23).map!nonSquare.writeln;

    foreach (immutable i; 1 .. 1_000_000) {
        immutable ns = i.nonSquare;
        assert(ns != (cast(int)real(ns).sqrt) ^^ 2);
    }
}
```

```txt
[2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27]
```



## EchoLisp


```scheme

(lib 'sequences)

(define (a n) (+ n (floor (+ 0.5 (sqrt n)))))
(define A000037 (iterator/n a 1))

(take A000037 22)
    → (2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27)
(filter square? (take A000037 1000000))
    → null

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			sequence_of_non_squares (22)
			io.new_line
			sequence_of_non_squares (1000000)
		end

	sequence_of_non_squares (n: INTEGER)
                        -- Sequence of non-squares up to the n'th member.
		require
			n_positive: n >= 1
		local
			non_sq, part: REAL_64
			math: DOUBLE_MATH
			square: BOOLEAN
		do
			create math
			across
				1 |..| (n) as c
			loop
				part := (0.5 + math.sqrt (c.item.to_double))
				non_sq := c.item + part.floor
				io.put_string (non_sq.out + "%N")
				if math.sqrt (non_sq) - math.sqrt (non_sq).floor = 0 then
					square := True
				end
			end
			if square = True then
				io.put_string ("There are squares for n equal to " + n.out + ".")
			else
				io.put_string ("There are no squares for n equal to " + n.out + ".")
			end
		end

end


```

```txt

2
3
5
6
7
8
10
11
12
13
14
15
17
18
19
20
21
22
23
24
26
27
There are no squares for n equal to 22.

2
3
5
6 ...

1000999
1001000
There are no squares for n equal to 1000000.

```



## Elixir


```elixir
f = fn n -> n + trunc(0.5 + :math.sqrt(n)) end

IO.inspect for n <- 1..22, do: f.(n)

n = 1_000_000
non_squares = for i <- 1..n, do: f.(i)
m = :math.sqrt(f.(n)) |> Float.ceil |> trunc
squares = for  i <- 1..m, do: i*i
case Enum.find_value(squares, fn i -> i in non_squares end) do
  nil -> IO.puts "No squares found below #{n}"
  val -> IO.puts "Error: number is a square: #{val}"
end
```


```txt

[2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26,
 27]
No squares found below 1000000

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(non_squares).
-export([main/0]).

main() ->
		lists:foreach(fun(X) -> io:format("~p~n",[non_square(X)] ) end, lists:seq(1,22)),  % First 22 non-squares.
		lists:foreach(fun(X) -> io:format("~p~n",[non_square(X)] ) end, lists:seq(1,1000000)). % First 1 million non-squares.
non_square(N) ->
	N+trunc(1/2+ math:sqrt(N)).

```



## Euphoria

This is based on the [[BASIC]] and [[Go]] examples.

```Euphoria
function nonsqr( atom n)
    return n + floor( 0.5 + sqrt( n ) )
end function

puts( 1, "  n  r(n)\n" )
puts( 1, "---  ---\n" )
for i = 1 to 22 do
    printf( 1, "%3d  %3d\n", { i, nonsqr(i) } )
end for

atom j
atom found
found = 0
for i = 1 to 1000000 do
    j = sqrt(nonsqr(i))
    if integer(j) then
        found = 1
        printf( 1, "Found square: %d\n", i )
        exit
    end if
end for
if found = 0 then
    puts( 1, "No squares found\n" )
end if
```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let SequenceOfNonSquares =
    let nonsqr n = n+(int(0.5+Math.Sqrt(float (n))))
    let isqrt n = int(Math.Sqrt(float(n)))
    let IsSquare n = n = (isqrt n)*(isqrt n)
    {1 .. 999999}
    |> Seq.map(fun f -> (f, nonsqr f))
    |> Seq.filter(fun f -> IsSquare(snd f))
;;
```


Executing the code gives:
```fsharp

> SequenceOfNonSquares;;
val it : seq<int * int> = seq []
```



## Factor


```factor
USING: kernel math math.functions math.ranges prettyprint
sequences ;

: non-sq ( n -- m ) dup sqrt 1/2 + floor + >integer ;

: print-first22 ( -- ) 22 [1,b] [ non-sq ] map . ;

: check-for-sq ( -- ) 1,000,000 [1,b)
    [ non-sq sqrt dup floor = [ "Square found." throw ] when ]
    each ;

print-first22 check-for-sq
```

```txt

{ 2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27 }

```



## Fantom



```fantom

class Main
{
  static Float fn (Int n)
  {
    n + (0.5f + (n * 1.0f).sqrt).floor
  }

  static Bool isSquare (Float n)
  {
    n.sqrt.floor == n.sqrt
  }

  public static Void main ()
  {
    (1..22).each |n|
    {
      echo ("$n is ${fn(n)}")
    }
    echo ((1..1000000).toList.any |n| { isSquare (fn(n)) } )
  }
}

```



## Forth


```forth
: u>f  0 d>
f ;
: f>u  f>d drop ;

: fn ( n -- n ) dup u>f fsqrt fround f>u + ;
: test ( n -- ) 1 do i fn . loop ;
23 test    \ 2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27  ok

: square? ( n -- ? ) u>f fsqrt  fdup fround f-  f0= ;
: test ( n -- ) 1 do i fn square? if cr i . ." fn was square" then loop ;
1000000 test    \ ok
```



## Fortran

```fortran
PROGRAM NONSQUARES

  IMPLICIT NONE

  INTEGER :: m, n, nonsqr

  DO n = 1, 22
    nonsqr =  n + FLOOR(0.5 + SQRT(REAL(n)))  ! or could use NINT(SQRT(REAL(n)))
    WRITE(*,*) nonsqr
  END DO

  DO n = 1, 1000000
    nonsqr =  n + FLOOR(0.5 + SQRT(REAL(n)))
    m = INT(SQRT(REAL(nonsqr)))
    IF (m*m == nonsqr) THEN
      WRITE(*,*) "Square found, n=", n
    END IF
  END DO

END PROGRAM NONSQUARES
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function nonSquare (n As UInteger) As UInteger
  Return CUInt(n + Int(0.5 + Sqr(n)))
End Function

Function isSquare (n As UInteger) As Boolean
  Dim As UInteger r = CUInt(Sqr(n))
  Return n = r * r
End Function

Print "The first 22 numbers generated by the sequence are :"
For i As Integer = 1 To 22
  Print nonSquare(i); " ";
Next

Print : Print

' Test numbers generated for n less than a million to see if they're squares

For i As UInteger = 1 To 999999
  If isSquare(nonSquare(i)) Then
    Print "The number generated by the sequence for n ="; i; " is square!"
    Goto finish
  End If
Next

Print "None of the numbers generated by the sequence for n < 1000000 are square"

finish:
Print
Print "Press any key to quit"
Sleep
```


```txt

The first 22 numbers generated by the sequence are :
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

None of the numbers generated by the sequence for n < 1000000 are square

```



## GAP

<lang># Here we use generators : the given formula doesn't need one, but the alternate
# non-squares function is better done with a generator.

# The formula is implemented with exact floor(sqrt(n)), so we use
# a trick: multiply by 100 to get the first decimal digit of the
# square root of n, then add 5 (that's 1/2 multiplied by 10).
# Then just divide by 10 to get floor(1/2 + sqrt(n)) exactly.
# It looks weird, but unlike floating point, it will do the job
# for any n.
NonSquaresGen := function()
	local ns, n;
	n := 0;
	ns := function()
		n := n + 1;
		return n + QuoInt(5 + RootInt(100*n), 10);
	end;
	return ns;
end;

NonSquaresAlt := function()
	local ns, n, q, k;
	n := 1;
	q := 4;
	k := 3;
	ns := function()
		n := n + 1;
		if n = q then
			n := n + 1;
			k := k + 2;
			q := q + k;
		fi;
		return n;
	end;
	return ns;
end;

gen := NonSquaresGen();
List([1 .. 22] i -> gen());
# [ 2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27 ]

a := NonSquaresGen();
b := NonSquaresAlt();

ForAll([1 .. 1000000], i -> a() = b());
# true
```



## Go

I assume it's obvious that the function monotonically increases, thus it's enough to just watch for the next possible square.  If a square is found, the panic will cause an ugly stack trace.

```go
package main

import (
    "fmt"
    "math"
)

func remarkable(n int) int {
    return n + int(.5+math.Sqrt(float64(n)))
}

func main() {
    // task 1
    fmt.Println("  n  r(n)")
    fmt.Println("---  ---")
    for n := 1; n <= 22; n++ {
        fmt.Printf("%3d  %3d\n", n, remarkable(n))
    }

    // task 2
    const limit = 1e6
    fmt.Println("\nChecking for squares for n <", limit)
    next := 2
    nextSq := 4
    for n := 1; n < limit; n++ {
        r := remarkable(n)
        switch {
        case r == nextSq:
            panic(n)
        case r > nextSq:
            fmt.Println(nextSq, "didn't occur")
            next++
            nextSq = next * next
        }
    }
    fmt.Println("No squares occur for n <", limit)
}
```

```txt

  n  r(n)
---  ---
  1    2
  2    3
  3    5
  4    6
  5    7
  6    8
  7   10
  8   11
  9   12
 10   13
 11   14
 12   15
 13   17
 14   18
 15   19
 16   20
 17   21
 18   22
 19   23
 20   24
 21   26
 22   27

Checking for squares for n < 1e+06
4 didn't occur
9 didn't occur
16 didn't occur
...
996004 didn't occur
998001 didn't occur
1000000 didn't occur
No squares occur for n < 1e+06

```



## Groovy


Solution:

```groovy
 def nonSquare = { long n -> n + ((1/2 + n**0.5) as long) }
```


Test Program:

```groovy
(1..22).each { println nonSquare(it) }
(1..1000000).each { assert ((nonSquare(it)**0.5 as long)**2) != nonSquare(it) }
```


<pre style="height:30ex;overflow:scroll;">2
3
5
6
7
8
10
11
12
13
14
15
17
18
19
20
21
22
23
24
26
27
```



## Haskell


```haskell
nonsqr :: Integral a =>
 a -> a
nonsqr n = n + round (sqrt (fromIntegral n))
```


 > map nonsqr [1..22]
 [2,3,5,6,7,8,10,11,12,13,14,15,17,18,19,20,21,22,23,24,26,27]

 > any (\j -> j == fromIntegral (floor j)) $ map (sqrt . fromIntegral . nonsqr) [1..1000000]
 False


Or, in a point-free variation, defining a 'main' for the compiler (rather than interpreter)


```haskell
import Control.Monad (join)

root :: Int -> Float
root = sqrt . fromIntegral

nonSqr :: Int -> Int
nonSqr = (+) <*> (round . root)

notSquare :: Int -> Bool
notSquare = (/=) <*> (join (*) . floor . root)

main :: IO ()
main =
  mapM_
    putStrLn
    [ "First 22 members of the series:"
    , unwords $ (show . nonSqr) <$> [1 .. 22]
    , ""
    , "All first 10E6 members non square:"
    , show $ all (== True) $ (notSquare . nonSqr) <$> [1 .. 1000000]
    ]
```

```txt
First 22 members of the series:
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

All first 10E6 members non square:
True
```



## HicEst


```HicEst
REAL :: n=22, nonSqr(n)

nonSqr = $ + FLOOR(0.5 + $^0.5)
WRITE() nonSqr

squares_found = 0
DO i = 1, 1E6
   non2 = i + FLOOR(0.5 + i^0.5)
   root = FLOOR( non2^0.5 )
   squares_found =  squares_found + (non2 == root*root)
ENDDO
WRITE(Name) squares_found
END
```


```txt
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
squares_found=0;
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link numbers

procedure main()

every n := 1 to 22 do
  write("nsq(",n,") := ",nsq(n))

every x := sqrt(nsq(n := 1 to 1000000)) do
  if x  = floor(x)^2 then write("nsq(",n,") = ",x," is a square.")
write("finished.")
end

procedure nsq(n)   # return non-squares
return n + floor(0.5 + sqrt(n))
end
```

[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers provides floor]


## IDL


```IDL
n = lindgen(1000000)+1               ; Take a million numbers
f = n+floor(.5+sqrt(n))              ; Apply formula
print,f[0:21]                        ; Output first 22
print,where(sqrt(f) eq fix(sqrt(f))) ; Test for squares
```


```txt

        2        3        5        6        7        8       10       11       12
       13       14       15       17       18       19       20       21       22
       23       24       26       27

       -1

```



## J


```j
   rf=: + 0.5 <.@+ %:       NB.  Remarkable formula

   rf 1+i.22               NB.  Results from 1 to 22
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

   +/ (rf e. *:) 1+i.1e6   NB.  Number of square RFs <= 1e6
0
```



## Java


```java
public class SeqNonSquares {
    public static int nonsqr(int n) {
        return n + (int)Math.round(Math.sqrt(n));
    }

    public static void main(String[] args) {
        // first 22 values (as a list) has no squares:
        for (int i = 1; i < 23; i++)
            System.out.print(nonsqr(i) + " ");
        System.out.println();

        // The following check shows no squares up to one million:
        for (int i = 1; i < 1000000; i++) {
            double j = Math.sqrt(nonsqr(i));
            assert j != Math.floor(j);
        }
    }
}
```



## JavaScript



### ES5


Iterative


```javascript
var a = [];
for (var i = 1; i < 23; i++) a[i] = i + Math.floor(1/2 + Math.sqrt(i));
console.log(a);

for (i = 1; i < 1000000; i++) if (Number.isInteger(i + Math.floor(1/2 + Math.sqrt(i))) === false) {
    console.log("The ",i,"th element of the sequence is a square");
}
```



### ES6



By functional composition


```JavaScript
(() => {

    // nonSquare :: Int -> Int
    let nonSquare = n =>
        n + floor(1 / 2 + sqrt(n));



    // floor :: Num -> Int
    let floor = Math.floor,

        // sqrt :: Num -> Num
        sqrt = Math.sqrt,

        // isSquare :: Int -> Bool
        isSquare = n => {
            let root = sqrt(n);

            return root === floor(root);
        };


    // TEST
    return {
        first22: Array.from({
            length: 22
        }, (_, i) => nonSquare(i + 1)),

        firstMillionNotSquare: Array.from({
                length: 10E6
            }, (_, i) => nonSquare(i + 1))
            .filter(isSquare)
            .length === 0
    };

})();
```


```JavaScript
{
    "first22":[2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15,
               17, 18, 19, 20, 21, 22, 23, 24, 26, 27],
    "firstMillionNotSquare":true
}
```



## jq

```jq
def A000037: . + (0.5 + sqrt | floor);

def is_square: sqrt | . == floor;

"For n up to and including 22:",
 (range(1;23) | A000037),
"Check for squares for n up to 1e6:",
 (range(1;1e6+1) | A000037 | select( is_square ))
```

```sh
$ jq -n -r -f sequence_of_non-squares.jq
For n up to and including 22:
2
3
5
6
7
8
10
11
12
13
14
15
17
18
19
20
21
22
23
24
26
27
Check for squares for n up to 1e6:
$
```



## Julia



```julia
nonsquare(n::Real) = n + floor(typeof(n), 0.5 + sqrt(n))
@show nonsquare.(1:1_000_000) ∩ collect(1:1000) .^ 2
```

```txt
nonsquare.(1:1000000) ∩ collect(1:1000) .^ 2 = Int64[]
```

So the set of squares of integers between 1 and 1000 and the first 1000000 terms of the given sequence is empty. Note that the given sequence is increasing and that its last term has a square root slightly less than 1000.5.


## K


```k
   nonsquare:{x+_.5+%x}
   nonsquare[1_!23]
```

```txt
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
```


```k
   issquare:{(%x)=_%x}
   +/issquare[nonsquare[1_!1000001]]  / Number of squares in first million results
```

```txt
0
```



## Kotlin


```scala
// version 1.1

fun f(n: Int) = n + Math.floor(0.5 + Math.sqrt(n.toDouble())).toInt()

fun main(args: Array<String>) {
    println(" n   f")
    val squares = mutableListOf<Int>()
    for (n in 1 until 1000000) {
        val v1 = f(n)
        val v2 = Math.sqrt(v1.toDouble()).toInt()
        if (v1 == v2 * v2) squares.add(n)
        if (n < 23) println("${"%2d".format(n)} : $v1")
    }
    println()
    if (squares.size == 0) println("There are no squares for n less than one million")
    else println("Squares are generated for the following values of n: $squares")
}
```


```txt

 n   f
 1 : 2
 2 : 3
 3 : 5
 4 : 6
 5 : 7
 6 : 8
 7 : 10
 8 : 11
 9 : 12
10 : 13
11 : 14
12 : 15
13 : 17
14 : 18
15 : 19
16 : 20
17 : 21
18 : 22
19 : 23
20 : 24
21 : 26
22 : 27

There are no squares for n less than one million

```



## Liberty BASIC


```lb

for i = 1 to 22
    print nonsqr( i); " ";
next i
print

found = 0
for i = 1 to 1000000
     j = ( nonsqr( i))^0.5
     if j = int( j) then
        found = 1
        print "Found square: "; i
        exit for
     end if
next i
if found =0 then print "No squares found"

end

function nonsqr( n)
    nonsqr = n +int( 0.5 +n^0.5)
end function

```


```txt

2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
No squares found

```



## Logo


```logo
repeat 22 [print sum # round sqrt #]
```



## Lua


```lua
function nonSquare (n)
    return n + math.floor(1/2 + math.sqrt(n))
end

for n = 1, 22 do
    io.write(nonSquare(n) .. " ")
end
print()
local sr
for n = 1, 10^6 do
    sr = math.sqrt(nonSquare(n))
    if sr == math.floor(sr) then
        print("Result for n = " .. n .. " is square!")
        os.exit()
    end
end
print("No squares found")
```

```txt
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
No squares found
```



## Mathematica


```Mathematica
nonsq = (# + Floor[0.5 + Sqrt[#]]) &;
nonsq@Range[22]
If[! Or @@ (IntegerQ /@ Sqrt /@ nonsq@Range[10^6]),
 Print["No squares for n <= ", 10^6]
 ]
```

```txt
{2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27}
No squares for n <= 1000000
```



## MATLAB


```MATLAB
function nonSquares(i)

    for n = (1:i)

        generatedNumber = n + floor(1/2 + sqrt(n));

        if mod(sqrt(generatedNumber),1)==0 %Check to see if the sqrt of the generated number is an integer
            fprintf('\n%d generates a square number: %d\n', [n,generatedNumber]);
            return
        else %If it isn't then the generated number is a square number
            if n<=22
                fprintf('%d ',generatedNumber);
            end
        end
    end

    fprintf('\nNo square numbers were generated for n <= %d\n',i);

end
```

Solution:

```MATLAB
>>
 nonSquares(1000000)
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
No square numbers were generated for n <= 1000000
```



## Maxima


```maxima
nonsquare(n) := n + quotient(isqrt(100 * n) + 5, 10);
makelist(nonsquare(n), n, 1, 20);
[2,3,5,6,7,8,10,11,12,13,14,15,17,18,19,20,21,22,23,24]

not_square(n) := isqrt(n)^2 # n$

m: 10^6$
u: makelist(i, i, 1, m)$
is(sublist(u, not_square) = sublist(map(nonsquare, u), lambda([x], x <= m)));
true
```



## min

```min
(dup sqrt 0.5 + int +) :non-sq
(sqrt dup floor - 0 ==) :sq?
(:n =q 1 'dup q concat 'succ concat n times pop) :upto

(non-sq print! " " print!) 22 upto newline
"Squares for n below one million:" puts!
(non-sq 'sq? 'puts when pop) 999999 upto
```

```txt

2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
Squares for n below one million:

```


=={{header|MK-61/52}}==
<lang>1	П4	ИП4	0	,	5	ИП4	КвКор	+	[x]
+	С/П	КИП4	БП	02
```



## MMIX


```mmix
	LOC	Data_Segment
	GREG	@
buf	OCTA	0,0

	GREG	@
NL	BYTE	#a,0
errh	BYTE	"Sorry, number ",0
errt	BYTE	"is a quare.",0
prtOk	BYTE	"No squares found below 1000000.",0

i	IS	$1		% loop var.
x	IS	$2		% computations
y	IS	$3		%   ..
z	IS	$4		%   ..
t	IS	$5		% temp
Ja	IS	$127		% return address

	LOC	#100		% locate program
	GREG	@

// print integer of max. 7 digits to StdOut
// primarily used to show the first 22 non squares
// in advance the end of the buffer is filled with ' 0 '
// reg x contains int to be printed
bp	IS	$71
0H	GREG	#0000000000203020
prtInt	STO	0B,buf		% initialize buffer
	LDA	bp,buf+7	% points after LSD
				% REPEAT
1H	SUB	bp,bp,1		%  move buffer pointer
	DIV	x,x,10		%  divmod (x,10)
	GET	t,rR		%  get remainder
	INCL	t,'0'		%  make char digit
	STB	t,bp		%  store digit
	PBNZ	x,1B		% UNTIL no more digits
	LDA	$255,bp
	TRAP	0,Fputs,StdOut	% print integer
	GO	Ja,Ja,0		% 'return'

// function calculates non square
// x = RF ( i )
RF	FLOT	x,i		% convert i to float
	FSQRT	x,0,x		% x = floor ( 0.5 + sqrt i )
	FIX	x,x		% convert float to int
	ADD	x,x,i		% x = i + floor ( 0.5 + sqrt i )
	GO	Ja,Ja,0		% 'return'

				% main (argc, argv) {
// generate the first 22 non squares
Main	SET	i,1		%  for ( i=1; i<=22; i++){
1H	GO	Ja,RF		%   x =  RF (i)
	GO	Ja,prtInt	%   print non square
	INCL	i,1		%   i++
	CMP	t,i,22		%   i<=22 ?
	PBNP	t,1B		%  }
	LDA	$255,NL
	TRAP	0,Fputs,StdOut

// check if RF (i) is a square for 0 < i < 1000000
	SET	i,1000
	MUL	i,i,i
	SUB	i,i,1		% for ( i = 999999; i>0; i--)
3H	GO	Ja,RF		%  x = RF ( i )
// square test
	FLOT	y,x		%  convert int x to float
	FSQRT	z,3,y		%  z = floor ( sqrt ( int (x) ) )
	FIX	z,z		%  z = cint z
	MUL	z,z,z		%  z = z^2
	CMP	t,x,z		%  x != (int sqrt x)^2 ?
	PBNZ	t,2F		%  if yes then continue
// it should not happen, but if a square is found
	LDA	$255,errh	%  else print err-message
	TRAP	0,Fputs,StdOut
	GO	Ja,prtInt	%  show trespasser
	LDA	$255,errt
	TRAP	0,Fputs,StdOut
	LDA	$255,NL
	TRAP	0,Fputs,StdOut
	TRAP	0,Halt,0

2H	SUB	i,i,1		%  i--
	PBNZ	i,3B		%  i>0? }
	LDA	$255,prtOk	%
	TRAP	0,Fputs,StdOut
	LDA	$255,NL
	TRAP	0,Fputs,StdOut
	TRAP	0,Halt,0	% }
```

```txt
~/MIX/MMIX/Rosetta> mmix SoNS
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
No squares found below 1000000.
```


=={{header|Modula-3}}==

```modula3
MODULE NonSquare EXPORTS Main;

IMPORT IO, Fmt, Math;

VAR i: INTEGER;

PROCEDURE NonSquare(n: INTEGER): INTEGER =
  BEGIN
    RETURN n + FLOOR(0.5D0 + Math.sqrt(FLOAT(n, LONGREAL)));
  END NonSquare;

BEGIN
  FOR n := 1 TO 22 DO
    IO.Put(Fmt.Int(NonSquare(n)) & " ");
  END;
  IO.Put("\n");
  FOR n := 1 TO 1000000 DO
    i := NonSquare(n);
    IF i = FLOOR(Math.sqrt(FLOAT(i, LONGREAL))) THEN
      IO.Put("Found square: " & Fmt.Int(n) & "\n");
    END;
  END;
END NonSquare.
```

```txt
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
```



## Nim


```nim
import math

proc nosqr(n: int): seq[int] =
  result = newSeq[int] n
  for i in 1..n:
    result[i - 1] = i + i.float.sqrt.round.int

proc issqr(n: int): bool =
  let sqr = sqrt(float(n))
  let err = abs(sqr - float(round(sqr)))
  err < 1e-7

echo nosqr(22)
for i in nosqr(1_000_000):
  assert(not issqr(i))
```

```txt
@[2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27]
```



## OCaml


```ocaml
# let nonsqr n = n + truncate (0.5 +. sqrt (float n));;
val nonsqr : int -> int = <fun>
# (* first 22 values (as a list) has no squares: *)
  for i = 1 to 22 do
    Printf.printf "%d " (nonsqr i)
  done;
  print_newline ();;
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
- : unit = ()
# (* The following check shows no squares up to one million: *)
  for i = 1 to 1_000_000 do
    let j = sqrt (float (nonsqr i)) in
      assert (j <> floor j)
  done;;
- : unit = ()
```



## Oforth



```Oforth
22 seq map(#[ dup sqrt 0.5 + floor + ]) println

1000000 seq map(#[ dup sqrt 0.5 + floor + ]) conform(#[ sqrt dup floor <>]) println
```


```txt

[2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27]
1

```



## Ol


```scheme

(import (lib math))

(print
   ; sequence for 1 .. 22
   (map (lambda (n)
         (+ n (floor (+ 1/2 (exact (sqrt n))))))
      (iota 22 1)))
; ==> (2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27)

(print
   ; filter out non squares
   (filter
      (lambda (x)
         (let ((s (floor (exact (sqrt x)))))
            (= (* s s) x)))
      (map (lambda (n)
            (+ n (floor (+ 1/2 (exact (sqrt n))))))
         (iota 1000000 1))))
; ==> ()


```



## Oz


```oz
declare
  fun {NonSqr N}
     N + {Float.toInt {Floor 0.5 + {Sqrt {Int.toFloat N}}}}
  end

  fun {SqrtInt N}
     {Float.toInt {Sqrt {Int.toFloat N}}}
  end

  fun {IsSquare N}
     {Pow {SqrtInt N} 2} == N
  end

  Ns = {Map {List.number 1 999999 1} NonSqr}
in
  {Show {List.take Ns 22}}
  {Show {Some Ns IsSquare}}
```



## PARI/GP


```parigp
[vector(22,n,n + floor(1/2 + sqrt(n))), sum(n=1,1e6,issquare(n + floor(1/2 + sqrt(n))))]
```



## Pascal

```pascal
Program SequenceOfNonSquares(output);

uses
  Math;

var
  m, n, test: longint;

begin
  for n := 1 to 22 do
  begin
    test :=  n + floor(0.5 + sqrt(n));
    write(test, ' ');
  end;
  writeln;

  for n := 1 to 1000000 do
  begin
    test :=  n + floor(0.5 + sqrt(n));
    m := round(sqrt(test));
    if (m*m = test) then
      writeln('square found for n = ', n);
  end;
end.
```

```txt
:> ./SequenceOfNonSquares
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

```


a little speedup in testing upto 1 billion.
5 secs instead of 21 secs using fpc2.6.4

```pascal
program seqNonSq;
 //sequence of non-squares
 //n = i + floor(1/2 + sqrt(i))
 function NonSquare(i: LongInt): LongInt;
 Begin
   NonSquare := i+trunc(sqrt(i) + 0.5);
 end;

 procedure First22;
 var
  i  : integer;
 begin
   For i := 1 to 21 do
     write(NonSquare(i):3,',');
   writeln(NonSquare(22):3);
 end;

 procedure OutSquare(i: integer);
 var
   n : LongInt;
 begin
   n := NonSquare(i);
   writeln('Square ',n,' found at ',i);
 end;

procedure Test(Limit: LongWord);
 var
  i ,n,sq,sn : LongWord;
 Begin
   sn := 1;
   sq := 1;
   For i := 1 to Limit do
   begin
     n := NonSquare(i);
     if n >= sq then
     begin
       if n > sq then
       begin
         sq := sq+2*sn+1; inc(sn);
       end
       else
         OutSquare(i);
     end;
   end;
 end;

 Begin
   First22;
   Test(1000*1000*1000);
 end.
```



## Perl


```perl
sub nonsqr { my $n = shift;  $n + int(0.5 + sqrt $n) }

print join(' ', map nonsqr($_), 1..22), "\n";

foreach my $i (1..1_000_000) {
  my $root = sqrt nonsqr($i);
  die "Oops, nonsqr($i) is a square!" if $root == int $root;
}
```


```txt
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27
```



## Perl 6


```perl6
sub nth-term (Int $n) { $n + round sqrt $n }

# Print the first 22 values of the sequence
say (nth-term $_ for 1 .. 22);

# Check that the first million values of the sequence are indeed non-square
for 1 .. 1_000_000 -> $i {
    say "Oops, nth-term($i) is square!" if (sqrt nth-term $i) %% 1;
}
```


```txt
(2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27)
```



## Phix


```Phix
sequence s = repeat(0,22)
for n=1 to length(s) do
    s[n] = n + floor(1/2 + sqrt(n))
end for
?s
integer nxt = 2, snxt = nxt*nxt, k
for n=1 to 1000000 do
    k = n + floor(1/2 + sqrt(n))
    if k>snxt then
--      printf(1,"%d didn't occur\n",snxt)
        nxt += 1
        snxt = nxt*nxt
    end if
    if k=snxt then
        puts(1,"error!!\n")
    end if
end for
puts(1,"none found ")
?{nxt,snxt}
```

```txt

{2,3,5,6,7,8,10,11,12,13,14,15,17,18,19,20,21,22,23,24,26,27}
none found {1001,1002001}

```



## PHP


```php
<?php
	//First Task
	for($i=1;$i<=22;$i++){
		echo($i + floor(1/2 + sqrt($i)) . "\n");
	}

	//Second Task
	$found_square=False;
	for($i=1;$i<=1000000;$i++){
		$non_square=$i + floor(1/2 + sqrt($i));
		if(sqrt($non_square)==intval(sqrt($non_square))){
			$found_square=True;
		}
	}
	echo("\n");
	if($found_square){
		echo("Found a square number, so the formula does not always work.");
	} else {
		echo("Up to 1000000, found no square number in the sequence!");
	}
?>
```

```txt
>php nsqrt.php
2
3
5
6
7
8
10
11
12
13
14
15
17
18
19
20
21
22
23
24
26
27

Up to 1000000, found no square number in the sequence!
>
```



## PicoLisp


```PicoLisp
(de sqfun (N)
   (+ N (sqrt N T)) )  # 'sqrt' rounds when called with 'T'

(for I 22
   (println I (sqfun I)) )

(for I 1000000
   (let (N (sqfun I)  R (sqrt N))
      (when (= N (* R R))
         (prinl N " is square") ) ) )
```

```txt
1 2
2 3
3 5
4 6
5 7
6 8
7 10
8 11
9 12
10 13
11 14
12 15
13 17
14 18
15 19
16 20
17 21
18 22
19 23
20 24
21 26
22 27
```



## PL/I


```PL/I

   put skip edit ((n, n + floor(sqrt(n) + 0.5) do n = 1 to n))
      (skip, 2 f(5));

```


Results:

<lang>
    1    2
    2    3
    3    5
    4    6
    5    7
    6    8
    7   10
    8   11
    9   12
   10   13
   11   14
   12   15
   13   17
   14   18
   15   19
   16   20
   17   21
   18   22
   19   23
   20   24
   21   26

```


Test 1,000,000 values:

<lang>
test: proc options (main);
   declare n fixed (15);

   do n = 1 to 1000000;
      if perfect_square (n + fixed(sqrt(n) + 0.5, 15)) then
         do; put skip list ('formula fails for n = ', n); stop; end;
   end;

perfect_square: procedure (N) returns (bit (1) aligned);
   declare N fixed (15);
   declare K fixed (15);

   k = sqrt(N)+0.1;
   return ( k*k = N );
end perfect_square;

end test;

```



## PostScript

<lang>/nonsquare { dup sqrt .5 add floor add } def
/issquare { dup sqrt floor dup mul eq } def

1 1 22 { nonsquare = } for

1 1 1000 {
        dup nonsquare issquare {
                (produced a square!) = = exit
        } if pop
} for

```

{{out}} (lack of error message shows none below 1000 produced a square)

```txt

2.0
3.0
5.0
6.0
7.0
8.0
10.0
11.0
12.0
13.0
14.0
15.0
17.0
18.0
19.0
20.0
21.0
22.0
23.0
24.0
26.0
27.0

```



## PowerShell

Implemented as a filter here, which can be used directly on the pipeline.

```powershell
filter Get-NonSquare {
    return $_ + [Math]::Floor(1/2 + [Math]::Sqrt($_))
}
```

Printing out the first 22 values is straightforward, then:

```powershell
1..22 | Get-NonSquare
```

If there were any squares for ''n'' up to one million, they would be printed with the following, but there is no output:

```powershell
1..1000000 `
    | Get-NonSquare `
    | Where-Object {
          $r = [Math]::Sqrt($_)
          [Math]::Truncate($r) -eq $r
      }
```



## PureBasic


```PureBasic
OpenConsole()
For a = 1 To 22
  ; Integer, so no floor needed
  tmp = 1 / 2 + Sqr(a)
  Print(Str(a + tmp) + ", ")
Next
PrintN("")
PrintN("Starting check till one million")
For a = 1 To 1000000
  value.d = a + Round((1 / 2 + Sqr(a)), #PB_Round_Down)
  root    = Sqr(value)
  If value - root*root = 0
    found + 1
    If found < 20
      PrintN("Found a square! " + Str(value))
    ElseIf found = 20
      PrintN("And more")
    EndIf
  EndIf
Next
If found
  PrintN(Str(found) + " Squares found, see above")
Else
  PrintN("No squares, all ok")
EndIf
; Wait for enter
Input()
```



## Python


```python
>>>
 from math import floor, sqrt
>>> def non_square(n):
        return n + floor(1/2 + sqrt(n))

>>> # first 22 values has no squares:
>>> print(*map(non_square, range(1, 23)))
2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

>>> # The following check shows no squares up to one million:
>>> def is_square(n):
        return sqrt(n).is_integer()

>>> non_squares = map(non_square, range(1, 10 ** 6))
>>> next(filter(is_square, non_squares))
StopIteration                             Traceback (most recent call last)
<ipython-input-45-f32645fc1c0a> in <module>()
      1 non_squares = map(non_square, range(1, 10 ** 6))
----> 2 next(filter(is_square, non_squares))

StopIteration:
```



## R

Printing the first 22 nonsquares.

```R
nonsqr <- function(n) n + floor(1/2 + sqrt(n))
nonsqr(1:22)
```

 [1]  2  3  5  6  7  8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

Testing the first million nonsquares.

```R
is.square <- function(x)
{
   sqrx <- sqrt(x)
   err <- abs(sqrx - round(sqrx))
   err < 100*.Machine$double.eps
}
any(is.square(nonsqr(1:1e6)))
```

 [1] FALSE


## Racket



```racket

#lang racket

(define (non-square n)
  (+ n (exact-floor (+ 1/2 (sqrt n)))))

(map non-square (range 1 23))

(define (square? n) (integer? (sqrt n)))

(for/or ([n (in-range 1 1000001)])
  (square? (non-square n)))

```


```txt

'(2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27)
#f

```



## REXX

REXX has no native support for   '''floor'''   or   '''sqrt''',   so these subroutines (functionsa) are written in REXX and are included below.

The   '''iSqrt'''   is a special integer square root function, it returns the   ''integer''   root   (and uses no floating point).
:::*   7   =   iSqrt(63)
:::*   8   =   iSqrt(64)
:::*   8   =   iSqrt(65)

```rexx
/*REXX pgm displays some non─square numbers, & also displays a validation check up to 1M*/
parse arg N M .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=      22               /*Not specified?  Then use the default.*/
if M=='' | M==","  then M= 1000000               /* "      "         "   "   "     "    */
say 'The first '    N    " non─square numbers:"  /*display a header of what's to come.  */
say                                              /* [↑]  default for  M  is one million.*/
say center('index', 20)        center("non─square numbers", 20)
say center(''     , 20, "═")   center(''                  , 20, "═")
          do j=1  for N
          say  center(j, 20)   center(j +floor(1/2 +sqrt(j)), 20)
          end   /*j*/
#=0
          do k=1  for M                          /*have it step through a million of 'em*/
          $= k + floor( sqrt(k) + .5 )           /*use the specified formula (algorithm)*/
          iRoot=iSqrt($)                         /*··· and also use the  ISQRT function.*/
          if iRoot*iRoot==$  then #=# + 1        /*have we found a mistook?  (sic)      */
          end   /*k*/
say;                     if #==0  then #= 'no'   /*use gooder English for display below.*/
say 'Using the formula:  floor[ 1/2 +  sqrt(n) ], '    #    " squares found up to "   M'.'
                                                 /* [↑]  display (possible) error count.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
floor: parse arg floor_;         return trunc( floor_ - (floor_ < 0) )
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt: procedure; parse arg x;  #=1;   r=0;         do  while # <= x; #=#*4; end
       do while #>1; #=#%4; _=x-r-#; r=r%2; if _<0 then iterate; x=_; r=r+#; end; return r
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x; if x=0 then return 0; d=digits(); m.=9; numeric form; h=d+6
       numeric digits;  parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .; g=g *.5'e'_ %2
         do j=0  while h>9;      m.j=h;               h=h % 2  + 1;  end /*j*/
         do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end /*k*/;   return g
```

```txt

The first  22  non─square numbers:

       index          non─square numbers
════════════════════ ════════════════════
         1                    2
         2                    3
         3                    5
         4                    6
         5                    7
         6                    8
         7                    10
         8                    11
         9                    12
         10                   13
         11                   14
         12                   15
         13                   17
         14                   18
         15                   19
         16                   20
         17                   21
         18                   22
         19                   23
         20                   24
         21                   26
         22                   27

Using the formula:  floor[ 1/2 +  sqrt(n) ],  no  squares found up to  1000000.

```



## Ring


```ring

for n=1 to 22
    x = n + floor(1/2 + sqrt(n))
    see "" + x + " "
next
see nl

```



## Ruby


```ruby
def f(n)
  n + (0.5 + Math.sqrt(n)).floor
end

(1..22).each { |n| puts "#{n} #{f(n)}" }

non_squares = (1..1_000_000).map { |n| f(n) }
squares = (1..1001).map { |n| n**2 } # Note: 1001*1001 = 1_002_001 > 1_001_000 = f(1_000_000)
(squares & non_squares).each do |n|
  puts "Oops, found a square f(#{non_squares.index(n)}) = #{n}"
end
```



## Rust

```rust

fn f(n: i64) -> i64 {
    n + (0.5 + (n as f64).sqrt()) as i64
}

fn is_sqr(n: i64) -> bool {
    let a = (n as f64).sqrt() as i64;
    n == a * a || n == (a+1) * (a+1) || n == (a-1) * (a-1)
}

fn main() {
    println!( "{:?}", (1..23).map(|n| f(n)).collect::<Vec<i64>>() );
    let count = (1..1_000_000).map(|n| f(n)).filter(|&n| is_sqr(n)).count();
    println!("{} unexpected squares found", count);
}

```



## Scala


```scala
def nonsqr(n:Int)=n+math.round(math.sqrt(n)).toInt

for(n<-1 to 22) println(n + "  "+ nonsqr(n))

val test=(1 to 1000000).exists{n =>
   val j=math.sqrt(nonsqr(n))
   j==math.floor(j)
}
println("squares up to one million="+test)
```



## Scheme


```scheme
(define non-squares
  (lambda (index)
    (+ index (inexact->exact (floor (+ (/ 1 2) (sqrt index)))))))

(define sequence
  (lambda (function)
    (lambda (start)
      (lambda (stop)
        (if (> start stop)
            (list)
            (cons (function start)
                  (((sequence function) (+ start 1)) stop)))))))

(define square?
  (lambda (number)
    ((lambda (root)
       (= (* root root) number))
     (floor (sqrt number)))))

(define any?
  (lambda (predicate?)
    (lambda (list)
      (and (not (null? list))
           (or (predicate? (car list))
               ((any? predicate?) (cdr list)))))))

(display (((sequence non-squares) 1) 22))
(newline)

(display ((any? square?) (((sequence non-squares) 1) 999999)))
(newline)
```

 (2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27)
 #f


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const func integer: nonsqr (in integer: n) is
  return n + trunc(0.5 + sqrt(flt(n)));

const proc: main is func
  local
    var integer: i is 0;
    var float: j is 0.0;
  begin
    # First 22 values (as a list) has no squares:
    for i range 1 to 22 do
      write(nonsqr(i) <& " ");
    end for;
    writeln;

    # The following check shows no squares up to one million:
    for i range 1 to 1000000 do
      j := sqrt(flt(nonsqr(i)));
      if j = floor(j) then
        writeln("Found square for nonsqr(" <& i <& ")");
      end if;
    end for;
  end func;
```



## Sidef


```ruby
func nonsqr(n) { 0.5 + n.sqrt -> floor + n }
{|i| nonsqr(i) }.map(1..22).join(' ').say

{ |i|
  if (nonsqr(i).is_sqr) {
     die "Found a square in the sequence: #{i}"
  }
} << 1..1e6
```



## Smalltalk


```smalltalk
| nonSquare isSquare squaresFound |
nonSquare := [:n |
    n + (n sqrt) rounded
].
isSquare := [:n |
    n = (((n sqrt) asInteger) raisedTo: 2)
].
Transcript show: 'The first few non-squares:'; cr.
1 to: 22 do: [:n |
    Transcript show: (nonSquare value: n) asString; cr
].
squaresFound := 0.
1 to: 1000000 do: [:n |
    (isSquare value: (nonSquare value: n)) ifTrue: [
        squaresFound := squaresFound + 1
    ]
].
Transcript show: 'Squares found for values up to 1,000,000: ';
show: squaresFound asString; cr
```



## Standard ML


```sml
- fun nonsqr n = n + round (Math.sqrt (real n));
val nonsqr = fn : int -> int
- List.tabulate (23, nonsqr);
val it = [0,2,3,5,6,7,8,10,11,12,13,14,...] : int list
- let fun loop i = if i = 1000000 then true
                                  else let val j = Math.sqrt (real (nonsqr i)) in
                                         Real.!= (j, Real.realFloor j) andalso
                                           loop (i+1)
                                       end in
    loop 1
  end;
val it = true : bool
```



## Tcl


```tcl
package require Tcl 8.5

set f {n {expr {$n + floor(0.5 + sqrt($n))}}}

for {set x 1} {$x <= 22} {incr x} {
    puts [format "%d\t%s" $x [apply $f $x]]
}

puts "looking for a square..."
for {set x 1} {$x <= 1000000} {incr x} {
    set y [apply $f $x]
    set s [expr {sqrt($y)}]
    if {$s == int($s)} {
        error "found a square in the sequence: $x -> $y"
    }
}
puts "done"
```

```txt
1	2.0
2	3.0
3	5.0
4	6.0
5	7.0
6	8.0
7	10.0
8	11.0
9	12.0
10	13.0
11	14.0
12	15.0
13	17.0
14	18.0
15	19.0
16	20.0
17	21.0
18	22.0
19	23.0
20	24.0
21	26.0
22	27.0
looking for a square...
done
```


=={{header|TI-89 BASIC}}==

Definition and 1 to 22, interactively:


```ti89b
■ n+floor(1/2+√(n)) → f(n)
    Done
■ seq(f(n),n,1,22)
    {2,3,5,6,7,8,10,11,12,13,14,15,17,18,19,20,21,22,23,24,26,27}
```


Program testing up to one million:


```ti89b
test()
Prgm
  Local i, ns
  For i, 1, 10^6
    f(i) → ns
    If (floor(√(ns)))^2 = ns Then
      Disp "Oops: " & string(ns)
    EndIf
  EndFor
  Disp "Done"
EndPrgm
```


(This program has not been run to completion.)


## Ursala


```Ursala
#import nat
#import flo

nth_non_square = float; plus^/~& math..trunc+ plus/0.5+ sqrt
is_square      = sqrt; ^E/~& math..trunc

#show+

examples = %neALP ^(~&,nth_non_square)*t iota23
check    = (is_square*~+nth_non_square*t; ~&i&& %eLP)||-[no squares found]-! iota 1000000
```

```txt

<
   1: 2.000000e+00,
   2: 3.000000e+00,
   3: 5.000000e+00,
   4: 6.000000e+00,
   5: 7.000000e+00,
   6: 8.000000e+00,
   7: 1.000000e+01,
   8: 1.100000e+01,
   9: 1.200000e+01,
   10: 1.300000e+01,
   11: 1.400000e+01,
   12: 1.500000e+01,
   13: 1.700000e+01,
   14: 1.800000e+01,
   15: 1.900000e+01,
   16: 2.000000e+01,
   17: 2.100000e+01,
   18: 2.200000e+01,
   19: 2.300000e+01,
   20: 2.400000e+01,
   21: 2.600000e+01,
   22: 2.700000e+01>
no squares found

```


## VBA


```vb

Sub Main()
Dim i&, c&, j#, s$
Const N& = 1000000
   s = "values for n in the range 1 to 22 : "
   For i = 1 To 22
      s = s & ns(i) & ", "
   Next
   For i = 1 To N
      j = Sqr(ns(i))
      If j = CInt(j) Then c = c + 1
   Next

   Debug.Print s
   Debug.Print c & " squares less than " & N
End Sub

Private Function ns(l As Long) As Long
   ns = l + Int(1 / 2 + Sqr(l))
End Function
```

```txt
values for n in the range 1 to 22 : 2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27,
0 squares less than 1000000
```


## XLISP


```lisp
(defun non-square (n)
    (+ n (floor (+ 0.5 (sqrt n)))))

(defun range (x y)
    (if (< x y)
        (cons x (range (+ x 1) y))))

(defun squarep (x)
    (= x (expt (floor (sqrt x)) 2)))

(defun count-squares (x y)
    (define squares 0)
    (if (squarep (non-square x))
        (define squares (+ squares 1)))
    (if (= x y)
        squares
        (count-squares (+ x 1) y)))

(print (mapcar non-square (range 1 23)))

(print `(number of squares for values less than 1000000 = ,(count-squares 1 1000000)))
```

```txt
(2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27)
(NUMBER OF SQUARES FOR VALUES LESS THAN 1000000 = 0)
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

func real Floor(X);             \Truncate X toward - infinity
real X;
return float(fix(X-0.5));

func PerfectSq(N);              \Return 'true' if N is a perfect square
int N;
return sqrt(N)*sqrt(N) = N;

int N, M, M0;
[for N:= 1 to 22 do
        [IntOut(0, fix(float(N) + Floor(0.5 + sqrt(float(N)))));  ChOut(0,^ )];
CrLf(0);
M0:= 1;
for N:= 1 to 999_999 do
        [M:= fix(float(N) + Floor(0.5 + sqrt(float(N))));
        if PerfectSq(M) then [IntOut(0, M);  Crlf(0)];  \error: have square
        if M#M0+1 and not PerfectSq(M0+1) then          \error: not sequential
                [IntOut(0, M);  Crlf(0)];
        M0:= M;
        ];
]
```


```txt

2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27

```



## zkl


```zkl
fcn seq(n){n + (0.5+n.toFloat().sqrt()).floor()}
[1..22].apply(seq).toString(*).println();

fcn isSquare(n){n.toFloat().sqrt().modf()[1]==0.0}
isSquare(25)  //-->True
isSquare(26)  //-->False
[2..0d1_000_000].filter(fcn(n){isSquare(seq(n))}).println();
```

modf returns the integer and fractional parts of a float
```txt

L(2,3,5,6,7,8,10,11,12,13,14,15,17,18,19,20,21,22,23,24,26,27)
L()

```
