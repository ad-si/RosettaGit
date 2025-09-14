+++
title = "Averages/Pythagorean means"
description = ""
date = 2019-10-09T18:26:14Z
aliases = []
[extra]
id = 6095
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "actionscript",
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "applescript",
  "autohotkey",
  "awk",
  "bbc_basic",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "elixir",
  "erlang",
  "erre",
  "euler_math_toolbox",
  "euphoria",
  "excel",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "funl",
  "futhark",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "matlab",
  "maxima",
  "mumps",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "postscript",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "smalltalk",
  "sql",
  "stata",
  "tcl",
  "ursala",
  "vala",
  "vba",
  "vbscript",
  "visual_basic_.net",
  "xpl0",
  "zkl",
]
+++

## Task

Compute all three of the [[wp:Pythagorean means|Pythagorean means]] of the set of integers <big>1</big> through <big>10</big> (inclusive).

Show that <big><math>A(x_1,\ldots,x_n) \geq G(x_1,\ldots,x_n) \geq H(x_1,\ldots,x_n)</math></big> for this set of positive integers.

*  The most common of the three means, the [[Averages/Arithmetic mean|arithmetic mean]], is the sum of the list divided by its length:
: <big><math> A(x_1, \ldots, x_n) = \frac{x_1 + \cdots + x_n}{n}</math></big>

* The [[wp:Geometric mean|geometric mean]] is the <math>n</math>th root of the product of the list:
: <big><math> G(x_1, \ldots, x_n) = \sqrt[n]{x_1 \cdots x_n} </math></big>

* The [[wp:Harmonic mean|harmonic mean]] is <math>n</math> divided by the sum of the reciprocal of each item in the list:
: <big><math> H(x_1, \ldots, x_n) = \frac{n}{\frac{1}{x_1} + \cdots + \frac{1}{x_n}} </math></big>



{{task heading|See also}}

{{Related tasks/Statistical measures}}


<hr>


## 11l


```11l
F amean(num)
   R sum(num)/Float(num.len)

F gmean(num)
   R product(num) ^ (1.0/num.len)

F hmean(num)
   return num.len / sum(num.map(n -> 1.0/n))

V numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print(amean(numbers))
print(gmean(numbers))
print(hmean(numbers))
```


{{out}}

```txt

5.5
4.52873
3.41417

```



## ActionScript


```ActionScript
function arithmeticMean(v:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i: uint = 0; i < v.length; i++)
		sum += v[i];
	return sum/v.length;
}
function geometricMean(v:Vector.<Number>):Number
{
	var product:Number = 1;
	for(var i: uint = 0; i < v.length; i++)
		product *= v[i];
	return Math.pow(product, 1/v.length);
}
function harmonicMean(v:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i: uint = 0; i < v.length; i++)
		sum += 1/v[i];
	return v.length/sum;
}
var list:Vector.<Number> = Vector.<Number>([1,2,3,4,5,6,7,8,9,10]);
trace("Arithmetic: ", arithmeticMean(list));
trace("Geometric: ", geometricMean(list));
trace("Harmonic: ", harmonicMean(list));
```



## Ada


pythagorean_means.ads:

```Ada
package Pythagorean_Means is
   type Set is array (Positive range <>) of Float;
   function Arithmetic_Mean (Data : Set) return Float;
   function Geometric_Mean  (Data : Set) return Float;
   function Harmonic_Mean   (Data : Set) return Float;
end Pythagorean_Means;
```


pythagorean_means.adb:

```Ada
with Ada.Numerics.Generic_Elementary_Functions;
package body Pythagorean_Means is
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);
   function "**" (Left, Right : Float) return Float renames Math."**";

   function Arithmetic_Mean (Data : Set) return Float is
      Sum : Float := 0.0;
   begin
      for I in Data'Range loop
         Sum := Sum + Data (I);
      end loop;
      return Sum / Float (Data'Length);
   end Arithmetic_Mean;

   function Geometric_Mean (Data : Set) return Float is
      Product : Float := 1.0;
   begin
      for I in Data'Range loop
         Product := Product * Data (I);
      end loop;
      return Product**(1.0/Float(Data'Length));
   end Geometric_Mean;

   function Harmonic_Mean (Data : Set) return Float is
      Reciprocal_Sum : Float := 0.0;
   begin
      for I in Data'Range loop
         Reciprocal_Sum := Reciprocal_Sum + Data (I)**(-1);
      end loop;
      return Float (Data'Length) / Reciprocal_Sum;
   end Harmonic_Mean;

end Pythagorean_Means;
```


example main.adb:

```Ada
with Ada.Text_IO;
with Pythagorean_Means;
procedure Main is
   My_Set : Pythagorean_Means.Set := (1.0, 2.0, 3.0, 4.0,  5.0,
                                      6.0, 7.0, 8.0, 9.0, 10.0);
   Arithmetic_Mean : Float := Pythagorean_Means.Arithmetic_Mean (My_Set);
   Geometric_Mean  : Float := Pythagorean_Means.Geometric_Mean  (My_Set);
   Harmonic_Mean   : Float := Pythagorean_Means.Harmonic_Mean   (My_Set);
begin
   Ada.Text_IO.Put_Line (Float'Image (Arithmetic_Mean) & " >= " &
                         Float'Image (Geometric_Mean)  & " >= " &
                         Float'Image (Harmonic_Mean));
end Main;
```



## ALGOL 68

{{trans|C}}
{{wont work with|ALGOL 68|Standard - argc and argv implementation dependent}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - argc and argv implementation dependent}}

```algol68
main: (
  INT count:=0;
  LONG REAL f, sum:=0, prod:=1, resum:=0;

  FORMAT real = $g(0,4)$; # preferred real format #

  FILE fbuf; STRING sbuf; associate(fbuf,sbuf);

  BOOL opts := TRUE;

  FOR i TO argc DO
    IF opts THEN # skip args up to the - token #
      opts := argv(i) NE "-"
    ELSE
      rewind(fbuf); sbuf := argv(i); get(fbuf,f);
      count +:= 1;
      sum +:= f;
      prod *:= f;
      resum +:= 1/f
    FI
  OD;
  printf(($"c: "f(real)l"s: "f(real)l"p: "f(real)l"r: "f(real)l$,count,sum,prod,resum));
  printf(($"Arithmetic mean = "f(real)l$,sum/count));
  printf(($"Geometric mean = "f(real)l$,prod**(1/count)));
  printf(($"Harmonic mean = "f(real)l$,count/resum))
)
```

Lunix command:
 a68g Averages_Pythagorean_means.a68 - 1 2 3 4 5 6 7 8 9 10
{{out}}

```txt

c: 10.0000
s: 55.0000
p: 3628800.0000
r: 2.9290
Arithmetic mean = 5.5000
Geometric mean = 4.5287
Harmonic mean = 3.4142

```



## ALGOL W


```algolw
begin
    % returns the arithmetic mean of the elements of n from lo to hi %
    real procedure arithmeticMean ( real array n ( * ); integer value lo, hi ) ;
    begin
        real sum;
        sum := 0;
        for i := lo until hi do sum := sum + n( i );
        sum / ( 1 + ( hi - lo ) )
    end arithmeticMean ;
    % returns the geometric mean of the elements of n from lo to hi %
    real procedure geometricMean ( real array n ( * ); integer value lo, hi ) ;
    begin
        real product;
        product := 1;
        for i := lo until hi do product := product * n( i );
        exp( ln( product ) / ( 1 + ( hi - lo ) ) )
    end geometricMean ;
    % returns the harminic mean of the elements of n from lo to hi %
    real procedure harmonicMean ( real array n ( * ); integer value lo, hi ) ;
    begin
        real sum;
        sum := 0;
        for i := lo until hi do sum := sum + ( 1 / n( i ) );
        ( 1 + ( hi - lo ) ) / sum
    end harmonicMean ;

    real array v ( 1 :: 10 );
    for i := 1 until 10 do v( i ) := i;

    r_w := 10; r_d := 5; r_format := "A"; s_w := 0; % set output format %

    write( "Arithmetic mean: ", arithmeticMean( v, 1, 10 ) );
    write( "Geometric  mean: ",  geometricMean( v, 1, 10 ) );
    write( "Harmonic   mean: ",   harmonicMean( v, 1, 10 ) )

end.
```

{{out}}

```txt

Arithmetic mean:    5.50000
Geometric  mean:    4.52872
Harmonic   mean:    3.41417

```



## APL


```APL

 arithmetic←{(+/⍵)÷⍴⍵}
 geometric←{(×/⍵)*÷⍴⍵}
 harmonic←{(⍴⍵)÷(+/÷⍵)}


 x←⍳10

 arithmetic x
5.5
 geometric x
4.528728688
 harmonic x
3.414171521
```



## AppleScript

{{trans|JavaScript}}

```AppleScript
-- arithmetic_mean :: [Number] -> Number
on arithmetic_mean(xs)

    -- sum :: Number -> Number -> Number
    script sum
        on |λ|(accumulator, x)
            accumulator + x
        end |λ|
    end script

    foldl(sum, 0, xs) / (length of xs)
end arithmetic_mean

-- geometric_mean :: [Number] -> Number
on geometric_mean(xs)

    -- product :: Number -> Number -> Number
    script product
        on |λ|(accumulator, x)
            accumulator * x
        end |λ|
    end script

    foldl(product, 1, xs) ^ (1 / (length of xs))
end geometric_mean

-- harmonic_mean :: [Number] -> Number
on harmonic_mean(xs)

    -- addInverse :: Number -> Number -> Number
    script addInverse
        on |λ|(accumulator, x)
            accumulator + (1 / x)
        end |λ|
    end script

    (length of xs) / (foldl(addInverse, 0, xs))
end harmonic_mean

-- TEST -----------------------------------------------------------------------
on run
    set {A, G, H} to ap({arithmetic_mean, geometric_mean, harmonic_mean}, ¬
        {{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}})

    {values:{arithmetic:A, geometric:G, harmonic:H}, inequalities:¬
        {|A >= G|:A ≥ G}, |G >= H|:G ≥ H}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on ap(fs, xs)
    set {nf, nx} to {length of fs, length of xs}
    set acc to {}
    repeat with i from 1 to nf
        tell mReturn(item i of fs)
            repeat with j from 1 to nx
                set end of acc to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return acc
end ap

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
```

{{Out}}

```AppleScript
{values:{arithmetic:5.5, geometric:4.528728688117, harmonic:3.414171521474},
inequalities:{|A >= G|:true}, |G >= H|:true}
```



## AutoHotkey


```autohotkey
A := ArithmeticMean(1, 10)
G := GeometricMean(1, 10)
H := HarmonicMean(1, 10)

If G Between %H% And %A%
    Result := "True"
Else
    Result := "False"

MsgBox, %A%`n%G%`n%H%`n%Result%


;---------------------------------------------------------------------------
ArithmeticMean(a, b) { ; of integers a through b
;---------------------------------------------------------------------------
    n := b - a + 1
    Loop, %n%
        Sum += (a + A_Index - 1)
    Return, Sum / n
}


;---------------------------------------------------------------------------
GeometricMean(a, b) { ; of integers a through b
;---------------------------------------------------------------------------
    n := b - a + 1
    Prod := 1
    Loop, %n%
        Prod *= (a + A_Index - 1)
    Return, Prod ** (1 / n)
}


;---------------------------------------------------------------------------
HarmonicMean(a, b) { ; of integers a through b
;---------------------------------------------------------------------------
    n := b - a + 1
    Loop, %n%
        Sum += 1 / (a + A_Index - 1)
    Return, n / Sum
}
```

Message box shows:

```txt

5.500000
4.528729
3.414172
True

```



## AWK


```awk
#!/usr/bin/awk -f
{
    x  = $1;   # value of 1st column
    A += x;
    G += log(x);
    H += 1/x;
    N++;
}

END {
   print "Arithmethic mean: ",A/N;
   print "Geometric mean  : ",exp(G/N);
   print "Harmonic mean   : ",N/H;
}
```



## BBC BASIC

The arithmetic and harmonic means use BBC BASIC's built-in array operations; only the geometric mean needs a loop.

```bbcbasic
      DIM a(9)
      a() = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
      PRINT "Arithmetic mean = " ; FNarithmeticmean(a())
      PRINT "Geometric mean =  " ; FNgeometricmean(a())
      PRINT "Harmonic mean =  " ; FNharmonicmean(a())
      END

      DEF FNarithmeticmean(a())
      = SUM(a()) / (DIM(a(),1)+1)

      DEF FNgeometricmean(a())
      LOCAL a, I%
      a = 1
      FOR I% = 0 TO DIM(a(),1)
        a *= a(I%)
      NEXT
      = a ^ (1/(DIM(a(),1)+1))

      DEF FNharmonicmean(a())
      LOCAL b()
      DIM b(DIM(a(),1))
      b() = 1/a()
      = (DIM(a(),1)+1) / SUM(b())

```

{{out}}

```txt
Arithmetic mean = 5.5
Geometric mean =  4.52872869
Harmonic mean =  3.41417152
```



## C


```c
#include <stdio.h>
#include <stdlib.h> // atoi()
#include <math.h> // pow()

int main(int argc, char* argv[])
{
  int i, count=0;
  double f, sum=0.0, prod=1.0, resum=0.0;

  for (i=1; i<argc; ++i) {
    f = atof(argv[i]);
    count++;
    sum += f;
    prod *= f;
    resum += (1.0/f);
  }
  //printf(" c:%d\n s:%f\n p:%f\n r:%f\n",count,sum,prod,resum);
  printf("Arithmetic mean = %f\n",sum/count);
  printf("Geometric mean = %f\n",pow(prod,(1.0/count)));
  printf("Harmonic mean = %f\n",count/resum);

  return 0;
}
```



## C++


```cpp
#include <vector>
#include <iostream>
#include <numeric>
#include <cmath>
#include <algorithm>

double toInverse ( int i ) {
   return  1.0 / i  ;
}

int main( ) {
   std::vector<int> numbers ;
   for ( int i = 1 ; i < 11 ; i++ )
      numbers.push_back( i ) ;
   double arithmetic_mean = std::accumulate( numbers.begin( ) , numbers.end( ) , 0 ) / 10.0 ;
   double geometric_mean =
      pow( std::accumulate( numbers.begin( ) , numbers.end( ) , 1 , std::multiplies<int>( ) ), 0.1 ) ;
   std::vector<double> inverses ;
   inverses.resize( numbers.size( ) ) ;
   std::transform( numbers.begin( ) , numbers.end( ) , inverses.begin( ) , toInverse ) ;
   double harmonic_mean = 10 / std::accumulate( inverses.begin( ) , inverses.end( ) , 0.0 ); //initial value of accumulate must be a double!
   std::cout << "The arithmetic mean is " << arithmetic_mean << " , the geometric mean "
      << geometric_mean << " and the harmonic mean " << harmonic_mean << " !\n" ;
   return 0 ;
}
```

{{out}}

```txt
The arithmetic mean is 5.5 , the geometric mean 4.52873 and the harmonic mean 3.41417 !
```


## C#
The standard Linq extension method <tt>Average</tt> provides arithmetic mean. This example adds two more extension methods for the geometric and harmonic means.

{{works with|C sharp|C#|3}}


```c#
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace PythMean
{
    static class Program
    {
        static void Main(string[] args) {
            var nums = from n in Enumerable.Range(1, 10) select (double)n;

            var a = nums.Average();
            var g = nums.Gmean();
            var h = nums.Hmean();

            Console.WriteLine("Arithmetic mean {0}", a);
            Console.WriteLine("Geometric mean  {0}", g);
            Console.WriteLine("Harmonic mean   {0}", h);

            Debug.Assert(a >= g && g >= h);
        }

        // Geometric mean extension method.
        static double Gmean(this IEnumerable<double> n) {
            return Math.Pow(n.Aggregate((s, i) => s * i), 1.0 / n.Count());
        }

        // Harmonic mean extension method.
        static double Hmean(this IEnumerable<double> n) {
            return n.Count() / n.Sum(i => 1.0 / i);
        }
    }
}
```

{{out}}

```txt

Arithmetic mean 5.5
Geometric mean  4.52872868811677
Harmonic mean   3.41417152147406
```



## CoffeeScript


```coffeescript
a = [ 1..10 ]
arithmetic_mean = (a) -> a.reduce(((s, x) -> s + x), 0) / a.length
geometic_mean = (a) -> Math.pow(a.reduce(((s, x) -> s * x), 1), (1 / a.length))
harmonic_mean = (a) -> a.length / a.reduce(((s, x) -> s + 1 / x), 0)

A = arithmetic_mean a
G = geometic_mean a
H = harmonic_mean a

console.log "A = ", A, " G = ", G, " H = ", H
console.log "A >= G : ", A >= G, " G >= H : ", G >= H
```

{{out}}

```txt
A =  5.5  G =  4.528728688116765  H =  3.414171521474055
A >= G :  true  G >= H :  true
```



## Common Lisp


```lisp
(defun generic-mean (nums reduce-op final-op)
  (funcall final-op (reduce reduce-op nums)))

(defun a-mean (nums)
  (generic-mean nums #'+ (lambda (x) (/ x (length nums)))))

(defun g-mean (nums)
  (generic-mean nums #'* (lambda (x) (expt x (/ 1 (length nums))))))

(defun h-mean (nums)
  (generic-mean nums
                (lambda (x y) (+ x
                                 (/ 1 y)))
                (lambda (x) (/ (length nums) x))))

(let ((numbers (loop for i from 1 to 10 collect i)))
  (let ((a-mean (a-mean numbers))
        (g-mean (g-mean numbers))
        (h-mean (h-mean numbers)))
    (assert (> a-mean g-mean h-mean))
    (format t "a-mean ~a~%" a-mean)
    (format t "g-mean ~a~%" g-mean)
    (format t "h-mean ~a~%" h-mean)))
```



## Clojure


```Clojure
(use '[clojure.contrib.math :only (expt)])

(defn a-mean [coll]
  (/ (apply + coll) (count coll)))

(defn g-mean [coll]
  (expt (apply * coll) (/ (count coll))))

(defn h-mean [coll]
  (/ (count coll) (apply + (map / coll))))

(let [numbers (range 1 11)
      a (a-mean numbers) g (g-mean numbers) h (h-mean numbers)]
  (println a ">=" g ">=" h)
  (>= a g h))
```



## D

The output for the harmonic mean is wrong.

```d
import std.stdio, std.algorithm, std.range, std.functional;

auto aMean(T)(T data) pure nothrow @nogc {
    return data.sum / data.length;
}

auto gMean(T)(T data) pure /*@nogc*/ {
    return data.reduce!q{a * b} ^^ (1.0 / data.length);
}

auto hMean(T)(T data) pure /*@nogc*/ {
    return data.length / data.reduce!q{ 1.0 / a + b };
}

void main() {
    immutable m = [adjoin!(hMean, gMean, aMean)(iota(1.0L, 11.0L))[]];
    writefln("%(%.19f %)", m);
    assert(m.isSorted);
}
```

{{out}}

```txt
0.9891573712076470036 4.5287286881167647619 5.5000000000000000000
```



## Delphi


```Delphi
program AveragesPythagoreanMeans;

{$APPTYPE CONSOLE}

uses Types, Math;

function ArithmeticMean(aArray: TDoubleDynArray): Double;
var
  lValue: Double;
begin
  Result := 0;
  for lValue in aArray do
    Result := Result + lValue;
  if Result > 0 then
    Result := Result / Length(aArray);
end;

function GeometricMean(aArray: TDoubleDynArray): Double;
var
  lValue: Double;
begin
  Result := 1;
  for lValue in aArray do
    Result := Result * lValue;
  Result := Power(Result, 1 / Length(aArray));
end;

function HarmonicMean(aArray: TDoubleDynArray): Double;
var
  lValue: Double;
begin
  Result := 0;
  for lValue in aArray do
    Result := Result + 1 / lValue;
  Result := Length(aArray) / Result;
end;

var
  lSourceArray: TDoubleDynArray;
  AMean, GMean, HMean: Double;
begin
  lSourceArray := TDoubleDynArray.Create(1,2,3,4,5,6,7,8,9,10);
  AMean := ArithmeticMean(lSourceArray));
  GMean := GeometricMean(lSourceArray));
  HMean := HarmonicMean(lSourceArray));
  if (AMean >= GMean) and (GMean >= HMean) then
    Writeln(AMean, " ≥ ", GMean, " ≥ ", HMean)
  else
    writeln("Error!");
end.
```



## E


Given that we're defining all three together, it makes sense to express their regularities:


```e
def makeMean(base, include, finish) {
    return def mean(numbers) {
        var count := 0
        var acc := base
        for x in numbers {
            acc := include(acc, x)
            count += 1
        }
        return finish(acc, count)
    }
}

def A := makeMean(0, fn b,x { b+x   }, fn acc,n { acc / n      })
def G := makeMean(1, fn b,x { b*x   }, fn acc,n { acc ** (1/n) })
def H := makeMean(0, fn b,x { b+1/x }, fn acc,n { n / acc      })
```



```e
? A(1..10)
# value: 5.5

? G(1..10)
# value: 4.528728688116765

? H(1..10)
# value: 3.414171521474055
```



## EchoLisp


```scheme

(define (A xs) (// (for/sum ((x xs)) x) (length xs)))

(define (G xs) (expt (for/product ((x xs)) x) (// (length xs))))

(define (H xs) (// (length xs) (for/sum ((x xs)) (// x))))

(define xs (range 1 11))
(and (>= (A xs) (G xs)) (>= (G xs) (H xs)))
    → #t

```



## Elixir


```elixir
defmodule Means do
  def arithmetic(list) do
    Enum.sum(list) / length(list)
  end
  def geometric(list) do
    :math.pow(Enum.reduce(list, &(*/2)), 1 / length(list))
  end
  def harmonic(list) do
    1 / arithmetic(Enum.map(list, &(1 / &1)))
  end
end

list = Enum.to_list(1..10)
IO.puts "Arithmetic mean: #{am = Means.arithmetic(list)}"
IO.puts "Geometric mean:  #{gm = Means.geometric(list)}"
IO.puts "Harmonic mean:   #{hm = Means.harmonic(list)}"
IO.puts "(#{am} >= #{gm} >= #{hm}) is #{am >= gm and gm >= hm}"
```

{{out}}

```txt

Arithmetic mean: 5.5
Geometric mean:  4.528728688116765
Harmonic mean:   3.414171521474055
(5.5 >= 4.528728688116765 >= 3.414171521474055) is true

```



## Erlang



```Erlang
%% Author: Abhay Jain <abhay_1303@yahoo.co.in>

-module(mean_calculator).
-export([find_mean/0]).

find_mean() ->
%% This is function calling. First argument is the the beginning number
%% and second argument is the initial value of sum for AM & HM and initial value of product for GM.
	arithmetic_mean(1, 0),
	geometric_mean(1, 1),
	harmonic_mean(1, 0).

%% Function to calculate Arithmetic Mean
arithmetic_mean(Number, Sum) when Number > 10 ->
	AM = Sum / 10,
	io:format("Arithmetic Mean ~p~n", [AM]);
arithmetic_mean(Number, Sum) ->
	NewSum = Sum + Number,
	arithmetic_mean(Number+1, NewSum).

%% Function to calculate Geometric Mean
geometric_mean(Number, Product) when Number > 10 ->
	GM = math:pow(Product, 0.1),
	io:format("Geometric Mean ~p~n", [GM]);
geometric_mean(Number, Product) ->
	NewProd = Product * Number,
	geometric_mean(Number+1, NewProd).

%% Function to calculate Harmonic Mean
harmonic_mean(Number, Sum) when Number > 10 ->
	HM = 10 / Sum,
	io:format("Harmonic Mean ~p~n", [HM]);
harmonic_mean(Number, Sum) ->
	NewSum = Sum + (1/Number),
	harmonic_mean(Number+1, NewSum).
```


{{out}}

```txt
Arithmetic Mean 5.5
Geometric Mean 4.528728688116765
Harmonic Mean 3.414171521474055
```



## ERRE

<lang>
PROGRAM MEANS

DIM A[9]

PROCEDURE ARITHMETIC_MEAN(A[]->M)
      LOCAL S,I%
      NEL%=UBOUND(A,1)
      S=0
      FOR I%=0 TO NEL% DO
        S+=A[I%]
      END FOR
      M=S/(NEL%+1)
END PROCEDURE

PROCEDURE GEOMETRIC_MEAN(A[]->M)
      LOCAL S,I%
      NEL%=UBOUND(A,1)
      S=1
      FOR I%=0 TO NEL% DO
        S*=A[I%]
      END FOR
      M=S^(1/(NEL%+1))
END PROCEDURE

PROCEDURE HARMONIC_MEAN(A[]->M)
      LOCAL S,I%
      NEL%=UBOUND(A,1)
      S=0
      FOR I%=0 TO NEL% DO
        S+=1/A[I%]
      END FOR
      M=(NEL%+1)/S
END PROCEDURE

BEGIN
      A[]=(1,2,3,4,5,6,7,8,9,10)
      ARITHMETIC_MEAN(A[]->M)
      PRINT("Arithmetic mean = ";M)
      GEOMETRIC_MEAN(A[]->M)
      PRINT("Geometric mean =  ";M)
      HARMONIC_MEAN(A[]->M)
      PRINT("Harmonic mean =  ";M)
END PROGRAM

```



## Euler Math Toolbox



```Euler Math Toolbox

>function A(x) := mean(x)
>function G(x) := exp(mean(log(x)))
>function H(x) := 1/mean(1/x)
>x=1:10; A(x), G(x), H(x)
 5.5
 4.52872868812
 3.41417152147

```


Alternatively, e.g.,


```Euler Math Toolbox

>function G(x) := prod(x)^(1/length(x))

```



## Euphoria


```euphoria
function arithmetic_mean(sequence s)
    atom sum
    if length(s) = 0 then
        return 0
    else
        sum = 0
        for i = 1 to length(s) do
            sum += s[i]
        end for
        return sum/length(s)
    end if
end function

function geometric_mean(sequence s)
    atom p
    p = 1
    for i = 1 to length(s) do
        p *= s[i]
    end for
    return power(p,1/length(s))
end function

function harmonic_mean(sequence s)
    atom sum
    if length(s) = 0 then
        return 0
    else
        sum = 0
        for i = 1 to length(s) do
            sum += 1/s[i]
        end for
        return length(s) / sum
    end if
end function

function true_or_false(atom x)
    if x then
        return "true"
    else
        return "false"
    end if
end function

constant s = {1,2,3,4,5,6,7,8,9,10}
constant arithmetic = arithmetic_mean(s),
    geometric = geometric_mean(s),
    harmonic = harmonic_mean(s)
printf(1,"Arithmetic: %g\n", arithmetic)
printf(1,"Geometric: %g\n", geometric)
printf(1,"Harmonic: %g\n", harmonic)
printf(1,"Arithmetic>=Geometric>=Harmonic: %s\n",
    {true_or_false(arithmetic>=geometric and geometric>=harmonic)})
```


{{out}}

```txt
Arithmetic: 5.5
Geometric: 4.52873
Harmonic: 3.41417
Arithmetic>=Geometric>=Harmonic: true

```



## Excel


Use the functions : AVERAGE, GEOMEAN and HARMEAN


```Excel

=AVERAGE(1;2;3;4;5;6;7;8;9;10)
=GEOMEAN(1;2;3;4;5;6;7;8;9;10)
=HARMEAN(1;2;3;4;5;6;7;8;9;10)

```

{{out}}

```txt

5.5
4.528728688
3,414171521

```



=={{header|F_Sharp|F#}}==

```fsharp
let P = [1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0]

let arithmeticMean (x : float list) =
    x |> List.sum
      |> (fun acc -> acc / float (List.length(x)))

let geometricMean (x: float list) =
    x |> List.reduce (*)
      |> (fun acc -> Math.Pow(acc, 1.0 / (float (List.length(x)))))

let harmonicMean (x: float list) =
    x |> List.map (fun a -> 1.0 / a)
      |> List.sum
      |> (fun acc -> float (List.length(x)) / acc)

printfn "Arithmetic Mean: %A" (arithmeticMean P)
printfn "Geometric Mean: %A" (geometricMean P)
printfn "Harmonic Mean: %A" (harmonicMean P)
```



## Factor


```factor
: a-mean ( seq -- mean )
    [ sum ] [ length ] bi / ;

: g-mean ( seq -- mean )
    [ product ] [ length recip ] bi ^ ;

: h-mean ( seq -- mean )
    [ length ] [ [ recip ] map-sum ] bi / ;
```


 ( scratchpad ) 10 [1,b] [ a-mean ] [ g-mean ] [ h-mean ] tri
                "%f >= %f >= %f\n" printf
 5.500000 >= 4.528729 >= 3.414172


## Fantom



```fantom

class Main
{
  static Float arithmeticMean (Int[] nums)
  {
    if (nums.size == 0) return 0.0f
    sum := 0
    nums.each |n| { sum += n }
    return sum.toFloat / nums.size
  }

  static Float geometricMean (Int[] nums)
  {
    if (nums.size == 0) return 0.0f
    product := 1
    nums.each |n| { product *= n }
    return product.toFloat.pow(1f/nums.size)
  }

  static Float harmonicMean (Int[] nums)
  {
    if (nums.size == 0) return 0.0f
    reciprocals := 0f
    nums.each |n| { reciprocals += 1f / n }
    return nums.size.toFloat / reciprocals
  }

  public static Void main ()
  {
    items := (1..10).toList
    // display results
    echo (arithmeticMean (items))
    echo (geometricMean (items))
    echo (harmonicMean (items))
    // check given relation
    if ((arithmeticMean (items) >= geometricMean (items)) &&
        (geometricMean (items) >= harmonicMean (items)))
      echo ("relation holds")
    else
      echo ("relation failed")
  }
}

```



## Forth


```forth
: famean ( faddr n -- f )
  0e
  tuck floats bounds do
    i f@ f+
  float +loop
  0 d>f f/ ;

: fgmean ( faddr n -- f )
  1e
  tuck floats bounds do
    i f@ f*
  float +loop
  0 d>f 1/f f** ;

: fhmean ( faddr n -- f )
  dup 0 d>f  0e
  floats bounds do
    i f@ 1/f f+
  float +loop
  f/ ;

create test 1e f, 2e f, 3e f, 4e f, 5e f, 6e f, 7e f, 8e f, 9e f, 10e f,
test 10 famean fdup f.
test 10 fgmean fdup fdup f.
test 10 fhmean fdup f.
( A G G H )
f>= . f>= .  \ -1 -1
```



## Fortran

{{works with|Fortran|90}}

```fortran
program Mean

  real :: a(10) = (/ (i, i=1,10) /)
  real :: amean, gmean, hmean

  amean = sum(a) / size(a)
  gmean = product(a)**(1.0/size(a))
  hmean = size(a) / sum(1.0/a)

  if ((amean < gmean) .or. (gmean < hmean)) then
    print*, "Error!"
  else
    print*, amean, gmean, hmean
  end if

end program Mean
```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Function ArithmeticMean(array() As Double) As Double
  Dim length As Integer = Ubound(array) - Lbound(array) + 1
  Dim As Double sum = 0.0
  For i As Integer = LBound(array) To UBound(array)
    sum += array(i)
  Next
  Return sum/length
End Function

Function GeometricMean(array() As Double) As Double
  Dim length As Integer = Ubound(array) - Lbound(array) + 1
  Dim As Double product = 1.0
  For i As Integer = LBound(array) To UBound(array)
    product *= array(i)
  Next
  Return product ^ (1.0 / length)
End Function

Function HarmonicMean(array() As Double) As Double
  Dim length As Integer = Ubound(array) - Lbound(array) + 1
  Dim As Double sum = 0.0
  For i As Integer = LBound(array) To UBound(array)
    sum += 1.0 / array(i)
  Next
  Return length / sum
End Function

Dim vector(1 To 10) As Double
For i As Integer = 1 To 10
  vector(i) = i
Next

Print "Arithmetic mean is :"; ArithmeticMean(vector())
Print "Geometric mean is  :"; GeometricMean(vector())
Print "Harmonic mean is   :"; HarmonicMean(vector())
Print
Print "Press any key to quit the program"
Sleep

```


{{out}}

```txt

Arithmetic mean is : 5.5
Geometric mean is  : 4.528728688116765
Harmonic mean is   : 3.414171521474055

```



## FunL


```funl
import lists.zip

def
  mean( s, 0 ) = product( s )^(1/s.length())
  mean( s, p ) = (1/s.length() sum( x^p | x <- s ))^(1/p)

def
  monotone( [_], _ ) = true
  monotone( a1:a2:as, p ) = p( a1, a2 ) and monotone( a2:as, p )

means = [mean( 1..10, m ) | m <- [1, 0, -1]]

for (m, l) <- zip( means, ['Arithmetic', 'Geometric', 'Harmonic'] )
  println( "$l: $m" + (if m is Rational then " or ${m.doubleValue()}" else '') )

println( monotone(means, (>=)) )
```


{{out}}


```txt

Arithmetic: 11/2 or 5.5
Geometric: 4.528728688116765
Harmonic: 25200/7381 or 3.414171521474055
true

```



## Futhark



```Futhark

fun arithmetic_mean(as: [n]f64): f64 =
  reduce (+) 0.0 (map (/f64(n)) as)

fun geometric_mean(as: [n]f64): f64 =
  reduce (*) 1.0 (map (**(1.0/f64(n))) as)

fun harmonic_mean(as: [n]f64): f64 =
  f64(n) / reduce (+) 0.0 (map (1.0/) as)

fun main(as: [n]f64): (f64,f64,f64) =
  (arithmetic_mean as,
   geometric_mean as,
   harmonic_mean as)

```



## GAP


```gap
# The first two work with rationals or with floats
# (but bear in mind that support of floating point is very poor in GAP)
mean := v -> Sum(v) / Length(v);
harmean := v -> Length(v) / Sum(v, Inverse);
geomean := v -> EXP_FLOAT(Sum(v, LOG_FLOAT) / Length(v));

mean([1 .. 10]);
# 11/2
harmean([1 .. 10]);
# 25200/7381

v := List([1..10], FLOAT_INT);;
mean(v);
# 5.5
harmean(v);
# 3.41417
geomean(v);
# 4.52873
```


## Go


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    sum, sumr, prod := 0., 0., 1.
    for n := 1.; n <= 10; n++ {
        sum += n
        sumr += 1 / n
        prod *= n
    }
    a, g, h := sum/10, math.Pow(prod, .1), 10/sumr
    fmt.Println("A:", a, "G:", g, "H:", h)
    fmt.Println("A >= G >= H:", a >= g && g >= h)
}
```

{{out}}

```txt

A: 5.5 G: 4.528728688116765 H: 3.414171521474055
A >= G >= H: true

```



## Groovy

Solution:

```groovy
def arithMean = { list ->
    list == null \
        ? null \
        : list.empty \
            ? 0 \
            : list.sum() / list.size()
}

def geomMean = { list ->
    list == null \
        ? null \
        : list.empty \
            ? 1 \
            : list.inject(1) { prod, item -> prod*item } ** (1 / list.size())
}

def harmMean = { list ->
    list == null \
        ? null \
        : list.empty \
            ? 0 \
            : list.size() / list.collect { 1.0/it }.sum()
}
```


Test:

```groovy
def list = 1..10
def A = arithMean(list)
def G = geomMean(list)
assert A >= G
def H = harmMean(list)
assert G >= H
println """
list: ${list}
   A: ${A}
   G: ${G}
   H: ${H}
"""
```


{{out}}

```txt
list: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
   A: 5.5
   G: 4.528728688116765
   H: 3.4141715214
```



## Haskell


### =One generalized function=

The [[wp:Generalized mean|general function]] given here yields an arithmetic mean when its first argument is <code>1</code>, a geometric mean when its first argument is <code>0</code>, and a harmonic mean when its first argument is <code>-1</code>.


```haskell
import Data.List (genericLength)
import Control.Monad (zipWithM_)

mean :: Double -> [Double] -> Double
mean 0 xs = product xs ** (1 / genericLength xs)
mean p xs = (1 / genericLength xs * sum (map (** p) xs)) ** (1/p)

main = do
  let ms = zipWith ((. flip mean [1..10]). (,)) "agh" [1, 0, -1]
  mapM_ (\(t,m) -> putStrLn $ t : ": " ++ show m) ms
  putStrLn $ " a >= g >= h is " ++  show ((\(_,[a,g,h])-> a>=g && g>=h) (unzip ms))
```



### =Three applicatively defined functions=

These three functions (each combining the length of a list with some kind of fold over the elements of that same list), all share the same applicative structure.


```haskell
import Data.List (genericLength)

-- ARITHMETIC, GEOMETRIC AND HARMONIC MEANS ---------------
arithmetic, geometric, harmonic :: [Double] -> Double
arithmetic = (/) . sum <*> genericLength

geometric = (**) . product <*> ((1 /) . genericLength)

harmonic = (/) . genericLength <*> foldr ((+) . (1 /)) 0

-- TEST ---------------------------------------------------
xs :: [Double]
xs = [arithmetic, geometric, harmonic] <*> [[1 .. 10]]

main :: IO ()
main =
  (putStrLn . unlines)
    [ zip ["Arithmetic", "Geometric", "Harmonic"] xs >>= show
    , mappend "\n A >= G >= H is " $ --
      (show . and) $ zipWith (>=) xs (tail xs)
    ]
```

{{Out}}

```txt
("Arithmetic",5.5)("Geometric",4.528728688116765)("Harmonic",3.414171521474055)

 a >= g >= h is True
```



## HicEst


```HicEst
AGH = ALIAS( A, G, H ) ! named vector elements
AGH = (0, 1, 0)
DO i = 1, 10
   A = A + i
   G = G * i
   H = H + 1/i
ENDDO
AGH = (A/10, G^0.1, 10/H)

WRITE(ClipBoard, Name) AGH, "Result = " // (A>=G) * (G>=H)
```

! A=5.5; G=4.528728688; H=3.414171521; Result = 1;

=={{header|Icon}} and {{header|Unicon}}==

```Icon
link numbers     # for a/g/h means

procedure main()
every put(x := [], 1 to 10)
writes("x := [ "); every writes(!x," "); write("]")

write("Arithmetic mean:", a := amean!x)
write("Geometric mean:",g := gmean!x)
write("Harmonic mean:", h := hmean!x)
write(" a >= g >= h is ", if a >= g >= h then "true" else "false")
end

```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers:amean, numbers:gmean, and numbers:hmean] are shown below:

```Icon
procedure amean(L[])		#: arithmetic mean
   local m
   if *L = 0 then fail
   m := 0.0
   every m +:= !L
   return m / *L
end

procedure gmean(L[])		#: geometric mean
   local m
   if *L = 0 then fail
   m := 1.0
   every m *:= !L
   m := abs(m)
   if m > 0.0 then
      return exp (log(m) / *L)
   else
      fail
end

procedure hmean(L[])		#: harmonic mean
   local m, r
   if *L = 0 then fail
   m := 0.0
   every r := !L do {
      if r = 0.0 then fail
      else m +:= 1.0 / r
      }
   return *L / m
end
```


{{out}}

```txt
#means.exe
x := [ 1 2 3 4 5 6 7 8 9 10 ]
Arithmetic mean:5.5
Geometric mean:4.528728688116765
Harmonic mean:3.414171521474055
 a >= g >= h is true
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Averages.bas"
110 NUMERIC ARR(1 TO 10)
120 FOR I=LBOUND(ARR) TO UBOUND(ARR)
130   LET ARR(I)=I
140 NEXT
150 PRINT "Arithmetic mean =";ARITHM(ARR)
160 PRINT "Geometric mean  =";GEOMETRIC(ARR)
170 PRINT "Harmonic mean   =";HARMONIC(ARR)
180 DEF ARITHM(REF A)
190   LET T=0
200   FOR I=LBOUND(A) TO UBOUND(A)
210     LET T=T+A(I)
220   NEXT
230   LET ARITHM=T/SIZE(A)
240 END DEF
250 DEF GEOMETRIC(REF A)
260   LET T=1
270   FOR I=LBOUND(A) TO UBOUND(A)
280     LET T=T*A(I)
290   NEXT
300   LET GEOMETRIC=T^(1/SIZE(A))
310 END DEF
320 DEF HARMONIC(REF A)
330   LET T=0
340   FOR I=LBOUND(A) TO UBOUND(A)
350     LET T=T+(1/A(I))
360   NEXT
370   LET HARMONIC=SIZE(A)/T
380 END DEF
```



## J

'''Solution:'''

```j
amean=: +/ % #
gmean=: # %: */
hmean=: amean&.:%
```


'''Example Usage:'''

```j
   (amean , gmean , hmean) >: i. 10
5.5 4.528729 3.414172
   assert 2 >:/\ (amean , gmean , hmean) >: i. 10    NB. check amean >= gmean and gmean >= hmean
```


Note that gmean could have instead been defined as mean under logarithm, for example:


```j
gmean=:amean&.:^.
```


(and this variant should probably be preferred - especially if the argument list is long, to avoid problems with floating point infinity.)


## Java


```java
import java.util.Arrays;
import java.util.List;

public class PythagoreanMeans {
    public static double arithmeticMean(List<Double> numbers) {
        if (numbers.isEmpty()) return Double.NaN;
        double mean = 0.0;
        for (Double number : numbers) {
            mean += number;
        }
        return mean / numbers.size();
    }

    public static double geometricMean(List<Double> numbers) {
        if (numbers.isEmpty()) return Double.NaN;
        double mean = 1.0;
        for (Double number : numbers) {
            mean *= number;
        }
        return Math.pow(mean, 1.0 / numbers.size());
    }

    public static double harmonicMean(List<Double> numbers) {
        if (numbers.isEmpty() || numbers.contains(0.0)) return Double.NaN;
        double mean = 0.0;
        for (Double number : numbers) {
            mean += (1.0 / number);
        }
        return numbers.size() / mean;
    }

    public static void main(String[] args) {
        Double[] array = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
        List<Double> list = Arrays.asList(array);
        double arithmetic = arithmeticMean(list);
        double geometric = geometricMean(list);
        double harmonic = harmonicMean(list);
        System.out.format("A = %f  G = %f  H = %f%n", arithmetic, geometric, harmonic);
        System.out.format("A >= G is %b, G >= H is %b%n", (arithmetic >= geometric), (geometric >= harmonic));
    }
}
```

{{out}}

```txt
A = 5.500000  G = 4.528729  H = 3.414172
A >= G is true, G >= H is true
```


{{works with|Java|1.8}}
We can rewrite the 3 methods using the new JAVA Stream API:

```java

   public static double arithmAverage(double array[]){
       if (array == null ||array.length == 0) {
         return 0.0;
      }
      else {
         return DoubleStream.of(array).average().getAsDouble();
      }
   }

    public static double geomAverage(double array[]){
      if (array == null ||array.length == 0) {
         return 0.0;
      }
      else {
         double aver = DoubleStream.of(array).reduce(1, (x, y) -> x * y);
         return   Math.pow(aver, 1.0 / array.length);
      }
   }

     public static double harmAverage(double array[]){
         if (array == null ||array.length == 0) {
         return 0.0;
      }
      else {
         double aver = DoubleStream.of(array)
                  // remove null values
                  .filter(n -> n > 0.0)
                  // generate 1/n array
                  .map( n-> 1.0/n)
                  // accumulating
                  .reduce(0, (x, y) -> x + y);
                  // just this reduce is not working- need to do in 2 steps
                 // .reduce(0, (x, y) -> 1.0/x + 1.0/y);
         return   array.length / aver ;
      }
   }

```



## JavaScript



### ES5


```javascript
(function () {
    'use strict';

    // arithmetic_mean :: [Number] -> Number
    function arithmetic_mean(ns) {
        return (
            ns.reduce( // sum
                function (sum, n) {
                    return (sum + n);
                },
                0
            ) / ns.length
        );
    }

    // geometric_mean :: [Number] -> Number
    function geometric_mean(ns) {
        return Math.pow(
            ns.reduce( // product
                function (product, n) {
                    return (product * n);
                },
                1
            ),
            1 / ns.length
        );
    }

    // harmonic_mean :: [Number] -> Number
    function harmonic_mean(ns) {
        return (
            ns.length / ns.reduce( // sum of inverses
                function (invSum, n) {
                    return (invSum + (1 / n));
                },
                0
            )
        );
    }

    var values = [arithmetic_mean, geometric_mean, harmonic_mean]
        .map(function (f) {
            return f([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
        }),
        mean = {
            Arithmetic: values[0], // arithmetic
            Geometric: values[1], // geometric
            Harmonic: values[2] // harmonic
        }

    return JSON.stringify({
        values: mean,
        test: "is A >= G >= H ? " +
            (
                mean.Arithmetic >= mean.Geometric &&
                mean.Geometric >= mean.Harmonic ? "yes" : "no"
            )
    }, null, 2);

})();

```


{{Out}}

```JavaScript
{
  "values": {
    "Arithmetic": 5.5,
    "Geometric": 4.528728688116765,
    "Harmonic": 3.414171521474055
  },
  "test": "is A >= G >= H ? yes"
}
```



### ES6


```JavaScript
(() => {

    // arithmeticMean :: [Number] -> Number
    const arithmeticMean = xs =>
        foldl((sum, n) => sum + n, 0, xs) / length(xs);

    // geometricMean :: [Number] -> Number
    const geometricMean = xs =>
        raise(foldl((product, x) => product * x, 1, xs), 1 / length(xs));

    // harmonicMean :: [Number] -> Number
    const harmonicMean = xs =>
        length(xs) / foldl((invSum, n) => invSum + (1 / n), 0, xs);

    // GENERIC FUNCTIONS ------------------------------------------------------

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // length :: [a] -> Int
    const length = xs => xs.length;

    // mapFromList :: [(k, v)] -> Dictionary
    const mapFromList = kvs =>
        foldl((a, [k, v]) =>
            (a[(typeof k === 'string' && k) || show(k)] = v, a), {}, kvs);

    // raise :: Num -> Int -> Num
    const raise = (n, e) => Math.pow(n, e);

    // show :: a -> String
    // show :: a -> Int -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[0], null, x[1]] : x
        );

    // zip :: [a] -> [b] -> [(a,b)]
    const zip = (xs, ys) =>
        xs.slice(0, Math.min(xs.length, ys.length))
        .map((x, i) => [x, ys[i]]);

    // TEST -------------------------------------------------------------------
    // mean :: Dictionary
    const mean = mapFromList(zip(
        ['Arithmetic', 'Geometric', 'Harmonic'],
        ap([arithmeticMean, geometricMean, harmonicMean], [
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        ])
    ));

    return show({
        values: mean,
        test: `is A >= G >= H ? ${mean.Arithmetic >= mean.Geometric &&
            mean.Geometric >= mean.Harmonic ? "yes" : "no"}`
    }, 2);
})();
```

{{Out}}

```JavaScript
{
  "values": {
    "Arithmetic": 5.5,
    "Geometric": 4.528728688116765,
    "Harmonic": 3.414171521474055
  },
  "test": "is A >= G >= H ? yes"
}
```



## jq


```jq
def amean: add/length;

def logProduct: map(log) | add;

def gmean:  (logProduct / length) | exp;

def hmean: length / (map(1/.) | add);

# Tasks:
 [range(1;11) ] | [amean, gmean, hmean] as $ans
 | ( $ans[],
   "amean > gmean > hmean => \($ans[0] > $ans[1] and $ans[1] > $ans[2] )" )

```

{{Out}}

```txt
5.5
4.528728688116766
3.414171521474055
"amean > gmean > hmean => true"
```



## Julia

Julia has a `mean` function to compute the arithmetic mean of a collections
of numbers. We can redefine it as follows.

```Julia
amean(A) = sum(A)/length(A)

gmean(A) = prod(A)^(1/length(A))

hmean(A) = length(A)/sum(1./A)
```

{{Out}}

```txt
julia> map(f-> f(1:10), [amean, gmean, hmean])
3-element Array{Float64,1}:
 5.5
 4.52873
 3.41417
julia> ans[1] > ans[2] > ans[3]
true
```



## K


```K

  am:{(+/x)%#x}
  gm:{(*/x)^(%#x)}
  hm:{(#x)%+/%:'x}

  {(am x;gm x;hm x)} 1+!10
5.5 4.528729 3.414172

```



## Kotlin


```scala>fun Collection<Double
.geometricMean() =
    if (isEmpty()) Double.NaN
    else Math.pow(reduce { n1, n2 -> n1 * n2 }, 1.0 / size)

fun Collection<Double>.harmonicMean() =
    if (isEmpty() || contains(0.0)) Double.NaN
    else size / reduce { n1, n2 -> n1 + 1.0 / n2 }

fun main(args: Array<String>) {
    val list = listOf(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val a = list.average()  // arithmetic mean
    val g = list.geometricMean()
    val h = list.harmonicMean()
    println("A = %f  G = %f  H = %f".format(a, g, h))
    println("A >= G is %b, G >= H is %b".format(a >= g, g >= h))
    require(g in h..a)
}
```

{{out}}

```txt
A = 5.500000  G = 4.528729  H = 3.414172
A >= G is true, G >= H is true
```



## Lasso


```Lasso
define arithmetic_mean(a::staticarray)::decimal => {
	//sum of the list divided by its length
	return (with e in #a sum #e) / decimal(#a->size)
}
define geometric_mean(a::staticarray)::decimal => {
	// The geometric mean is the nth root of the product of the list
	local(prod = 1)
	with e in #a do => { #prod *= #e }
	return math_pow(#prod,1/decimal(#a->size))
}
define harmonic_mean(a::staticarray)::decimal => {
	// The harmonic mean is n divided by the sum of the reciprocal of each item in the list
	return decimal(#a->size)/(with e in #a sum 1/decimal(#e))
}

arithmetic_mean(generateSeries(1,10)->asStaticArray)
geometric_mean(generateSeries(1,10)->asStaticArray)
harmonic_mean(generateSeries(1,10)->asStaticArray)
```


{{out}}

```txt
5.500000
4.528729
3.414172
```



## Liberty BASIC


```lb
for i = 1 to 10
    a = a + i
next
ArithmeticMean = a/10

b = 1
for i = 1 to 10
    b = b * i
next
GeometricMean = b ^ (1/10)

for i = 1 to 10
    c = c + (1/i)
next
HarmonicMean = 10/c

print "ArithmeticMean: ";ArithmeticMean
print "Geometric Mean: ";GeometricMean
print "Harmonic Mean: ";HarmonicMean

if (ArithmeticMean>=GeometricMean) and (GeometricMean>=HarmonicMean) then
print "True"
else
print "False"
end if

```


## Logo


```logo
to compute_means :count
  local "sum
  make "sum     0
  local "product
  make "product 1
  local "reciprocal_sum
  make "reciprocal_sum  0

  repeat :count [
    make "sum sum :sum repcount
    make "product product :product repcount
    make "reciprocal_sum sum :reciprocal_sum (quotient repcount)
  ]

  output (sentence (quotient :sum :count) (power :product (quotient :count))
                   (quotient :count :reciprocal_sum))
end

make "means compute_means 10
print sentence [Arithmetic mean is] item 1 :means
print sentence [Geometric mean is] item 2 :means
print sentence [Harmonic mean is] item 3 :means
bye
```



## Lua


```lua
function fsum(f, a, ...) return a and f(a) + fsum(f, ...) or 0 end
function pymean(t, f, finv) return finv(fsum(f, unpack(t)) / #t) end
nums = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

--arithmetic
a = pymean(nums, function(n) return n end, function(n) return n end)
--geometric
g = pymean(nums, math.log, math.exp)
--harmonic
h = pymean(nums, function(n) return 1/n end, function(n) return 1/n end)
print(a, g, h)
assert(a >= g and g >= h)
```


## M2000 Interpreter

Dimension(m,0) is the base (lower bound) for each dimension in an array, and can be 0 or 1.
Len(a) or len(M()) return length of a pointer to array and an array, as number of array elements.
For one dimension arrays len() is equal to Dimension(m(),1) where 1 is the first dimension
Dim A(10,10) : Print Len(A())=100




```M2000 Interpreter

Module CheckIt {
      sum=lambda -> {
              Read m as array
              if len(m)=0 then =0 : exit
              sum=Array(m, Dimension(m,0))
              If len(m)=1 then =sum : exit
              k=each(m,2,-1)
              While k {
                  sum+=Array(k)
            }
            =sum
      }
      mean=lambda sum (a as array) ->{
            =sum(a)/len(a)
      }
      prod=lambda -> {
              m=array
              if len(m)=0 then =0 : exit
              prod=Array(m, Dimension(m,0))
              If len(m)=1 then =prod : exit
              k=each(m,2,-1)
              While k {
                  prod*=Array(k)
            }
            =prod
      }
      geomean=lambda prod (a as array) -> {
            =prod(a)^(1/len(a))
      }
      harmomean=lambda (a as array) -> {
              if len(a)=0 then =0 : exit
              sum=1/Array(a, Dimension(a,0))
              If len(a)=1 then =1/sum : exit
              k=each(a,2,-1)
              While k {
                  sum+=1/Array(k)
            }
            =len(a)/sum
      }
      Print sum((1,2,3,4,5))=15
      Print prod((1,2,3,4,5))=120
      Print mean((1,2,3,4,5))==3
      \\ use == to apply rounding before comparison
      Print geomean((1,2,3,4,5))==2.60517108469735
      Print harmomean((1,2,3,4,5))==2.18978102189784
      Generator =lambda x=1 ->{=x : x++}
      dim a(10)<<Generator()
      Print mean(a())==5.5
      Print geomean(a())==4.52872868811677
      Print harmomean(a())==3.41417152147412
}
CheckIt

```



## Maple


```Maple
x := [ seq( 1 .. 10 ) ];
Means := proc( x )
    uses Statistics;
    return Mean( x ), GeometricMean( x ), HarmonicMean( x );
end proc:
Arithmeticmean, Geometricmean, Harmonicmean := Means( x );

is( Arithmeticmean >= Geometricmean and Geometricmean >= Harmonicmean );

```

{{out}}

```txt

Arithmeticmean, Geometricmean, Harmonicmean := 5.50000000000000, 4.52872868811677, 3.41417152147406

true

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Print["{Arithmetic Mean, Geometric Mean, Harmonic Mean} = ",
 N@Through[{Mean, GeometricMean, HarmonicMean}[Range@10]]]
```

{{out}}

```txt
{Arithmetic Mean, Geometric Mean, Harmonic Mean} = {5.5,4.52873,3.41417}
```



## MATLAB


```MATLAB
function [A,G,H] = pythagoreanMeans(list)

    A = mean(list);
    G = geomean(list);
    H = harmmean(list);

end
```


A solution that works for both, Matlab and Octave, is this


```MATLAB
function [A,G,H] = pythagoreanMeans(list)
    A = mean(list);           % arithmetic mean
    G = exp(mean(log(list))); % geometric mean
    H = 1./mean(1./list);     % harmonic mean
end
```


Solution:

```MATLAB>>
 [A,G,H]=pythagoreanMeans((1:10))

A =

   5.500000000000000


G =

   4.528728688116765


H =

   3.414171521474055
```



## Maxima


```maxima
/* built-in */
L: makelist(i, i, 1, 10)$

mean(L), numer;            /* 5.5 */
geometric_mean(L), numer;  /* 4.528728688116765 */
harmonic_mean(L), numer;   /* 3.414171521474055 */
```


=={{header|Modula-2}}==

```modula2
MODULE PythagoreanMeans;
FROM FormatString IMPORT FormatString;
FROM LongMath IMPORT power;
FROM LongStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE ArithmeticMean(numbers : ARRAY OF LONGREAL) : LONGREAL;
VAR
    i,cnt : CARDINAL;
    mean : LONGREAL;
BEGIN
    mean := 0.0;
    cnt := 0;
    FOR i:=0 TO HIGH(numbers) DO
        mean := mean + numbers[i];
        INC(cnt);
    END;
    RETURN mean / LFLOAT(cnt)
END ArithmeticMean;

PROCEDURE GeometricMean(numbers : ARRAY OF LONGREAL) : LONGREAL;
VAR
    i,cnt : CARDINAL;
    mean : LONGREAL;
BEGIN
    mean := 1.0;
    cnt := 0;
    FOR i:=0 TO HIGH(numbers) DO
        mean := mean * numbers[i];
        INC(cnt);
    END;
    RETURN power(mean, 1.0 / LFLOAT(cnt))
END GeometricMean;

PROCEDURE HarmonicMean(numbers : ARRAY OF LONGREAL) : LONGREAL;
VAR
    i,cnt : CARDINAL;
    mean : LONGREAL;
BEGIN
    mean := 0.0;
    cnt := 0;
    FOR i:=0 TO HIGH(numbers) DO
        mean := mean + ( 1.0 / numbers[i]);
        INC(cnt);
    END;
    RETURN LFLOAT(cnt) / mean
END HarmonicMean;


CONST Size = 10;
TYPE DA = ARRAY[1..Size] OF LONGREAL;

VAR
    buf : ARRAY[0..63] OF CHAR;
    array : DA;
    arithmetic,geometric,harmonic : LONGREAL;
BEGIN
    array := DA{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};

    arithmetic := ArithmeticMean(array);
    geometric := GeometricMean(array);
    harmonic := HarmonicMean(array);

    WriteString("A = ");
    RealToStr(arithmetic, buf);
    WriteString(buf);
    WriteString(" G = ");
    RealToStr(geometric, buf);
    WriteString(buf);
    WriteString(" H = ");
    RealToStr(harmonic, buf);
    WriteString(buf);
    WriteLn;

    FormatString("A >= G is %b, G >= H is %b\n", buf, arithmetic >= geometric, geometric >= harmonic);
    WriteString(buf);

    ReadChar
END PythagoreanMeans.
```



## MUMPS



```MUMPS
Pyth(n)	New a,ii,g,h,x
	For ii=1:1:n set x(ii)=ii
	;
	; Average
	Set a=0 For ii=1:1:n Set a=a+x(ii)
	Set a=a/n
	;
	; Geometric
	Set g=1 For ii=1:1:n Set g=g*x(ii)
	Set g=g**(1/n)
	;
	; Harmonic
	Set h=0 For ii=1:1:n Set h=1/x(ii)+h
	Set h=n/h
	;
	Write !,"Pythagorean means for 1..",n,":",!
	Write "Average = ",a," >= Geometric ",g," >= harmonic ",h,!
	Quit
Do Pyth(10)

Pythagorean means for 1..10:
Average = 5.5 >= Geometric 4.528728688116178495 >= harmonic 3.414171521474055006
```



## NetRexx

{{trans|ooRexx}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 20

a1 = ArrayList(Arrays.asList([Rexx 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]))
say "Arithmetic =" arithmeticMean(a1)", Geometric =" geometricMean(a1)", Harmonic =" harmonicMean(a1)

return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method arithmeticMean(numbers = java.util.List) public static returns Rexx
  -- somewhat arbitrary return for ooRexx
  if numbers.isEmpty then return "NaN"

  mean = 0
  number = Rexx
  loop number over numbers
      mean = mean + number
  end
  return mean / numbers.size

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method geometricMean(numbers = java.util.List) public static returns Rexx
  -- somewhat arbitrary return for ooRexx
  if numbers.isEmpty then return "NaN"

  mean = 1
  number = Rexx
  loop number over numbers
      mean = mean * number
  end
  return Math.pow(mean, 1 / numbers.size)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method harmonicMean(numbers = java.util.List) public static returns Rexx
  -- somewhat arbitrary return for ooRexx
  if numbers.isEmpty then return "NaN"

  mean = 0
  number = Rexx
  loop number over numbers
      if number = 0 then return "Nan"
      mean = mean + (1 / number)
  end

  -- problem here...
  return numbers.size / mean

```

{{out}}

```txt

Arithmetic = 5.5, Geometric = 4.528728688116765, Harmonic = 3.4141715214740550062

```



## Nim


```nim
import math, sequtils, future

proc amean(num: seq[float]): float =
  sum(num) / float(len(num))

proc gmean(num: seq[float]): float =
  result = 1
  for n in num: result *= n
  result = pow(result, 1.0 / float(num.len))

proc hmean(num: seq[float]): float =
  for n in num: result += 1.0 / n
  result = float(num.len) / result

proc ameanFunctional(num: seq[float]): float =
  sum(num) / float(num.len)

proc gmeanFunctional(num: seq[float]): float =
  num.foldl(a * b).pow(1.0 / float(num.len))

proc hmeanFunctional(num: seq[float]): float =
  float(num.len) / sum(num.mapIt(float, 1.0 / it))

let numbers = toSeq(1..10).map((x: int) => float(x))
echo amean(numbers), " ", gmean(numbers), " ", hmean(numbers)
```

{{out}}

```txt
5.5000000000000000e+00 4.5287286881167654e+00 3.4141715214740551e+00
```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE PythMean;
IMPORT Out, ML := MathL;

PROCEDURE Triplets(a: ARRAY OF INTEGER;VAR triplet: ARRAY OF LONGREAL);
VAR
	i: INTEGER;
BEGIN
	triplet[0] := 0.0;triplet[1] := 0.0; triplet[2] := 0.0;
	FOR i:= 0 TO LEN(a) - 1 DO
		triplet[0] := triplet[0] + a[i];
		triplet[1] := triplet[1] + ML.Ln(a[i]);
		triplet[2] := triplet[2] + (1 / a[i])
	END
END Triplets;

PROCEDURE Means*(a: ARRAY OF INTEGER);
VAR
	triplet: ARRAY 3 OF LONGREAL;
BEGIN
	Triplets(a,triplet);
	Out.String("A(1 .. 10): ");Out.LongReal(triplet[0] / LEN(a));Out.Ln;
	Out.String("G(1 .. 10): ");Out.LongReal(ML.Exp(triplet[1]/ LEN(a)));Out.Ln;
	Out.String("H(1 .. 10): ");Out.LongReal(LEN(a) / triplet[2]);Out.Ln;
END Means;

VAR
	nums: ARRAY 10 OF INTEGER;
	i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(nums) - 1 DO
		nums[i] := i + 1
	END;
	Means(nums)
END PythMean.


```

{{out}}

```txt

A(1 .. 10): 5.50000000000
G(1 .. 10): 4.52872868812
H(1 .. 10): 3.41417152147

```


## Objeck

{{trans|Java}}

```objeck
class PythagMeans {
  function : Main(args : String[]) ~ Nil {
    array := [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
    arithmetic := ArithmeticMean(array);
    geometric := GeometricMean(array);
    harmonic := HarmonicMean(array);

    arith_geo := arithmetic >= geometric;
    geo_harm := geometric >= harmonic;

    "A = {$arithmetic}, G = {$geometric}, H = {$harmonic}"->PrintLine();
    "A >= G is {$arith_geo}, G >= H is {$geo_harm}"->PrintLine();
  }

  function : native : ArithmeticMean(numbers : Float[]) ~ Float {
    if(numbers->Size() = 0) { return -1.0; };

    mean := 0.0;
    each(i : numbers) {
      mean += numbers[i];
    };

    return mean / numbers->Size();
  }

  function : native : GeometricMean(numbers : Float[]) ~ Float {
    if(numbers->Size() = 0) { return -1.0; };

    mean := 1.0;
    each(i : numbers) {
      mean *= numbers[i];
    };

    return mean->Power(1.0 / numbers->Size());
  }

  function : native : HarmonicMean(numbers : Float[]) ~ Float {
    if(numbers->Size() = 0) { return -1.0; };

    mean := 0.0;
    each(i : numbers) {
      mean += (1.0 / numbers[i]);
    };

    return numbers->Size() / mean;
  }
}
```


Output:

```txt

A = 5.500, G = 4.529, H = 3.414
A >= G is true, G >= H is true

```



## OCaml


The three means in one function


```ocaml
let means v =
  let n = Array.length v
  and a = ref 0.0
  and b = ref 1.0
  and c = ref 0.0 in
  for i=0 to n-1 do
    a := !a +. v.(i);
    b := !b *. v.(i);
    c := !c +. 1.0/.v.(i);
  done;
  let nn = float_of_int n in
  (!a /. nn, !b ** (1.0/.nn), nn /. !c)
;;
```


{{out}}

```txt
means (Array.init 10 (function i -> (float_of_int (i+1)))) ;;
(* (5.5, 4.5287286881167654, 3.4141715214740551) *)
```


Another implementation using <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html#VALfold_left Array.fold_left]</code> instead of a '''for''' loop:


```ocaml
let means v =
  let (a, b, c) =
    Array.fold_left
      (fun (a, b, c) x -> (a+.x, b*.x, c+.1./.x))
      (0.,1.,0.) v
  in
  let n = float_of_int (Array.length v) in
  (a /. n, b ** (1./.n), n /. c)
;;
```



## Octave


```Octave

    A = mean(list);     % arithmetic mean
    G = mean(list,'g'); % geometric mean
    H = mean(list,'a'); % harmonic mean

```


See also Matlab implementation [[#MATLAB]]


## Oforth



```Oforth
import: mapping

: A ( x )
   x sum
   x size dup ifZero: [ 2drop null ] else: [ >float / ]
;

: G( x )   #* x reduce  x size inv powf ;

: H( x )   x size x map( #inv ) sum / ;

: averages
| g |
   "Geometric mean  :" . 10 seq G dup .cr ->g
   "Arithmetic mean :" . 10 seq A dup . g >= ifTrue: [ " ==> A >= G" .cr ]
   "Harmonic mean   :" . 10 seq H dup . g <= ifTrue: [ " ==> G >= H" .cr ]
;
```


{{out}}

```txt

Geometric mean  : 4.52872868811677
Arithmetic mean : 5.5 ==> A >= G
Harmonic mean   : 3.41417152147406 ==> G >= H

```



## ooRexx


```ooRexx
a = .array~of(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
say "Arithmetic =" arithmeticMean(a)", Geometric =" geometricMean(a)", Harmonic =" harmonicMean(a)

::routine arithmeticMean
  use arg numbers
  -- somewhat arbitrary return for ooRexx
  if numbers~isEmpty then return "NaN"

  mean = 0
  loop number over numbers
      mean += number
  end
  return mean / numbers~items

::routine geometricMean
  use arg numbers
  -- somewhat arbitrary return for ooRexx
  if numbers~isEmpty then return "NaN"

  mean = 1
  loop number over numbers
      mean *= number
  end

  return rxcalcPower(mean, 1 / numbers~items)

::routine harmonicMean
  use arg numbers
  -- somewhat arbitrary return for ooRexx
  if numbers~isEmpty then return "NaN"

  mean = 0
  loop number over numbers
      if number = 0 then return "Nan"
      mean += 1 / number
  end

  -- problem here....
  return numbers~items / mean

::requires rxmath LIBRARY
```

{{out}}

```txt
Arithmetic = 5.5, Geometric = 4.52872869, Harmonic = 3.41417153
```



## Oz


```oz
declare
  %% helpers
  fun {Sum Xs} {FoldL Xs Number.'+' 0.0} end
  fun {Product Xs} {FoldL Xs Number.'*' 1.0} end
  fun {Len Xs} {Int.toFloat {Length Xs}} end

  fun {AMean Xs}
     {Sum Xs}
     /
     {Len Xs}
  end

  fun {GMean Xs}
     {Pow
      {Product Xs}
      1.0/{Len Xs}}
  end

  fun {HMean Xs}
     {Len Xs}
     /
     {Sum {Map Xs fun {$ X} 1.0 / X end}}
  end

  Numbers = {Map {List.number 1 10 1} Int.toFloat}

  [A G H] = [{AMean Numbers} {GMean Numbers} {HMean Numbers}]
in
  {Show [A G H]}
  A >= G = true
  G >= H = true
```



## PARI/GP

General implementations:

```parigp
arithmetic(v)={
  sum(i=1,#v,v[i])/#v
};
geometric(v)={
  prod(i=1,#v,v[i])^(1/#v)
};
harmonic(v)={
  #v/sum(i=1,#v,1/v[i])
};

v=vector(10,i,i);
[arithmetic(v),geometric(v),harmonic(v)]
```


Specific to the first ''n'' positive integers:

```parigp
arithmetic_first(n)={
  (n+1)/2
};
geometric_first(n)={
  n!^(1/n)
};
harmonic_first(n)={
  n/if(n>1000,
    log(n)+Euler+1/(n+n)+1/(12*n^2)-1/(120*n^4)+1/(252*n^6)-1/(240*n^8)+1/(132*n^10)
  ,
    n/sum(k=1,n,1/k)
  )
};

[arithmetic_first(10),geometric_first(10),harmonic_first(10)]
%[1]>=%[2] && %[2] >= %[3]
```


These are, asymptotically, n/2, n/e, and n/log n.


## Pascal

See [[Averages/Pythagorean_means&#Delphi | Delphi]]


## Perl


```perl
sub A
{
        my $a = 0;
        $a += $_ for @_;
        return $a / @_;
}
sub G
{
        my $p = 1;
        $p *= $_ for @_;
        return  $p**(1/@_); # power of 1/n == root of n
}
sub H
{
        my $h = 0;
        $h += 1/$_ for @_;
        return @_/$h;
}
my @ints = (1..10);

my $a = A(@ints);
my $g = G(@ints);
my $h = H(@ints);

print "A=$a\nG=$g\nH=$h\n";
die "Error" unless $a >= $g and $g >= $h;
```



## Perl 6

{{works with|Rakudo|2015.12}}


```perl6
sub A { ([+] @_) / @_ }
sub G { ([*] @_) ** (1 / @_) }
sub H { @_ / [+] 1 X/ @_ }

say "A(1,...,10) = ", A(1..10);
say "G(1,...,10) = ", G(1..10);
say "H(1,...,10) = ", H(1..10);

```


{{out}}

```txt
A(1,...,10) = 5.5
G(1,...,10) = 4.52872868811677
H(1,...,10) = 3.41417152147406
```



## Phix

(note to self: iff should really be a builtin)

```Phix
function arithmetic_mean(sequence s)
    return sum(s)/length(s)
end function

function geometric_mean(sequence s)
atom p = 1
    for i=1 to length(s) do
        p *= s[i]
    end for
    return power(p,1/length(s))
end function

function harmonic_mean(sequence s)
atom rsum = 0
    for i=1 to length(s) do
        rsum += 1/s[i]
    end for
    return length(s)/rsum
end function

function iff(integer condition, object Tval, object Fval)
    if condition then return Tval else return Fval end if
end function

constant s = {1,2,3,4,5,6,7,8,9,10}
constant arithmetic = arithmetic_mean(s),
         geometric = geometric_mean(s),
         harmonic = harmonic_mean(s)
printf(1,"Arithmetic: %.10g\n", arithmetic)
printf(1,"Geometric: %.10g\n", geometric)
printf(1,"Harmonic: %.10g\n", harmonic)
printf(1,"Arithmetic>=Geometric>=Harmonic: %s\n", {iff((arithmetic>=geometric and geometric>=harmonic),"true","false")})
```

{{out}}

```txt

Arithmetic: 5.5
Geometric: 4.528728688
Harmonic: 3.414171521
Arithmetic>=Geometric>=Harmonic: true

```



## PHP


```PHP
<?php
// Created with PHP 7.0

function ArithmeticMean(array $values)
{
    return array_sum($values) / count($values);
}

function GeometricMean(array $values)
{
    return array_product($values) ** (1 / count($values));
}

function HarmonicMean(array $values)
{
    $sum = 0;

    foreach ($values as $value) {
        $sum += 1 / $value;
    }

    return count($values) / $sum;
}

$values = array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

echo "Arithmetic: " . ArithmeticMean($values) . "\n";
echo "Geometric: " . GeometricMean($values) . "\n";
echo "Harmonic: " . HarmonicMean($values) . "\n";

```

{{out}}

```txt

Arithmetic: 5.5
Geometric: 4.5287286881168
Harmonic: 3.4141715214741

```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(let (Lst (1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)  Len (length Lst))
   (prinl "Arithmetic mean: "
      (format
         (/ (apply + Lst) Len)
         *Scl ) )
   (prinl "Geometric mean: "
      (format
         (pow (*/ (apply * Lst) (** 1.0 (dec Len))) (/ 1.0 Len))
         *Scl ) )
   (prinl "Harmonic mean: "
      (format
         (*/ (* 1.0 Len) 1.0 (sum '((N) (*/ 1.0 1.0 N)) Lst))
         *Scl ) ) )
```

{{out}}

```txt
Arithmetic mean: 5.500000
Geometric mean: 4.528729
Harmonic mean: 3.414172
```



## PL/I


```PL/I

declare n fixed binary,
        (Average, Geometric, Harmonic) float;
declare A(10) float static initial (1,2,3,4,5,6,7,8,9,10);

n = hbound(A,1);

/* compute the average */
Average = sum(A)/n;

/* Compute the geometric mean: */
Geometric = prod(A)**(1/n);

/* Compute the Harmonic mean: */
Harmonic = n / sum(1/A);

put skip data (Average);
put skip data (Geometric);
put skip data (Harmonic);

if Average < Geometric then put skip list ('Error');
if Geometric < Harmonic then put skip list ('Error');

```

Results:

```txt

AVERAGE= 5.50000E+0000;
GEOMETRIC= 4.52873E+0000;
HARMONIC= 3.41417E+0000;

```



## PostScript

<lang>
/pythamean{
/x exch def
/sum 0 def
/prod 1 def
/invsum 0 def
/i 1 def

x{
/sum sum i add def
/prod prod i mul def
/invsum invsum i -1 exp add def
/i i 1 add def
}repeat
(Arithmetic Mean : ) print
sum x div =
(Geometric Mean : ) print
prod x -1 exp exp =
(Harmonic Mean : ) print
x invsum div =
}def

10 pythamean

```


{{out}}

```txt

Arithmetic Mean : 5.5
Geometric Mean : 4.52873
Harmonic Mean : 3.41417

```


{{libheader|initlib}}

```postscript

/numbers {[1 10] 1 range}.
/recip {1 exch div}.

% Arithmetic mean
numbers dup 0 {+} fold exch length div
% Geometric mean
numbers dup 1 {*} fold exch length recip exp
% Harmonic mean
numbers dup 0 {recip +} fold exch length exch div

```



## PowerShell


```PowerShell
$A = 0
$LogG = 0
$InvH = 0

$ii = 1..10
foreach($i in $ii) {
	# Arithmetic mean is computed directly
	$A += $i / $ii.Count
	# Geometric mean is computed using Logarithms
	$LogG += [Math]::Log($i) / $ii.Count
	# Harmonic mean is computed using its inverse
	$InvH += 1 / ($i * $ii.Count)
}

$G = [Math]::Exp($LogG)
$H = 1/$InvH

write-host "Arithmetic mean: A = $A"
write-host "Geometric mean:  G = $G"
write-host "Harmonic mean:   H = $H"

write-host "Is A >= G ? $($A -ge $G)"
write-host "Is G >= H ? $($G -ge $H)"
```


{{out}}

```txt
Arithmetic mean: A = 5.5
Geometric mean:  G = 4.52872868811676
Harmonic mean:   H = 3.41417152147405
Is A >= G ? True
Is G >= H ? True
```



## PureBasic


```PureBasic
Procedure.d ArithmeticMean()
  For a = 1 To 10
    mean + a
  Next
  ProcedureReturn mean / 10
EndProcedure
Procedure.d GeometricMean()
  mean = 1
  For a = 1 To 10
    mean * a
  Next
  ProcedureReturn Pow(mean, 1 / 10)
EndProcedure
Procedure.d HarmonicMean()
  For a = 1 To 10
    mean.d + 1 / a
  Next
  ProcedureReturn 10 / mean
EndProcedure

If HarmonicMean() <= GeometricMean() And GeometricMean() <= ArithmeticMean()
  Debug "true"
EndIf
Debug ArithmeticMean()
Debug GeometricMean()
Debug HarmonicMean()
```



## Python

{{works with|Python|3}}

```Python
from operator import mul
from functools import reduce


def amean(num):
    return sum(num) / len(num)


def gmean(num):
    return reduce(mul, num, 1)**(1 / len(num))


def hmean(num):
    return len(num) / sum(1 / n for n in num)


numbers = range(1, 11)  # 1..10
a, g, h = amean(numbers), gmean(numbers), hmean(numbers)
print(a, g, h)
assert a >= g >= h
```

{{out}}

```txt
5.5 4.52872868812 3.41417152147
```


These are the same in Python 2 apart from requiring explicit float division (either through <code>float()</code> casts or float literals such as <code>1./n</code>); or better, do a <code>from __future__ import division</code>, which works on Python 2.2+ as well as Python 3, and makes division work consistently like it does in Python 3.


## R

Initialise x

```R

 x <- 1:10

```

Arithmetic mean

```R

a <- sum(x)/length(x)


```

or

```R

a <- mean(x)

```


The geometric mean

```R

g <- prod(x)^(1/length(x))

```


The harmonic mean (no error checking that <math>x_i\ne 0,\text{ } \forall \text{ }i=1\ldots n</math>)

```R

h <- length(x)/sum(1/x)

```


Then:


```R

a > g

```


and


```R

g > h

```


give both


```txt
[1] TRUE
```



## Racket


```racket

#lang racket

(define (arithmetic xs)
  (/ (for/sum ([x xs]) x)
     (length xs)))

(define (geometric xs)
  (expt (for/product ([x xs]) x)
        (/ (length xs))))

(define (harmonic xs)
  (/ (length xs)
     (for/sum ([x xs]) (/ x))))

(define xs (range 1 11))
(arithmetic xs)
(geometric xs)
(harmonic xs)
(>= (arithmetic xs) (geometric xs) (harmonic xs))

```

{{out}}

```txt

5 1/2
4.528728688116765
3 3057/7381
#t

```



## REXX

REXX doesn't have a   '''POW'''   function, so an   '''IROOT'''   ('''i'''nteger '''root''')   function is included here;   it includes an

extra error check if used as a general purpose function that would otherwise yield a complex result.

```rexx
/*REXX program  computes and displays the   Pythagorean means  [Amean,  Gmean,  Hmean]. */
numeric digits 20                                /*use a little extra for the precision.*/
parse arg n .                                    /*obtain the optional argument from CL.*/
if n=='' | n==","  then n=10                     /*None specified?  Then use the default*/
sum=0;  prod=1;  rSum=0                          /*initialize sum/product/reciprocal sum*/
$=;                     do #=1  for n;   $=$ #   /*generate list by appending # to list.*/
                        sum = sum   +    #       /*compute the sum of all the elements. */
                        prod= prod  *    #       /*compute the product of all elements. */
                        rSum= rSum  +  1/#       /*compute the sum of the reciprocals.  */
                        end   /*#*/
say ' list ='$                                   /*display the list of numbers used.    */
say 'Amean ='  sum / n                           /*calculate & display  arithmetic mean.*/
say 'Gmean ='  Iroot(prod, n)                    /*    "     "     "    geometric    "  */
say 'Hmean ='  n   / rSum                        /*    "     "     "    harmonic     "  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Iroot: procedure; arg x 1 ox, y 1 oy             /*get both args, and also a copy of X&Y*/
if x=0 | x=1 | y=1  then return x                /*handle special case of zero and unity*/
if y=0              then return 1                /*   "      "      "   " a   zero root.*/
if x<0 & y//2==0    then return IrootErr()
x=abs(x);         y=abs(y);         m=y - 1      /*use the absolute value for  X and Y. */
oDigs=digits();   a=oDigs + 5                    /*save original digits;  add five digs.*/
g=(x+1) / y**y                                   /*use this as the first guesstimate.   */
d=5                                              /*start with 5 dec digs, saves CPU time*/
   do  until d==a                                /*keep going as digits are increased.  */
   d=min(d+d, a);     numeric digits d;   f=d-2  /*limit digits to  original digits + 5.*/
   og=                                           /*use a non-guess for the old G (guess)*/
       do forever;    gm=g**m                    /*keep computing at the   Yth   root.  */
       _=format( (m*g*gm + x) / (y*gm), , f)     /*this is the nitty─gritty calculation.*/
       if _=g | _=og  then leave                 /*are we close enough yet?             */
       og=g;    g=_                              /*save guess ──► OG; set the new guess.*/
       end   /*forever*/
   end       /*until  */

if oy<0  then g=1/g                              /*use reciprocal when  Y  is negative. */
numeric digits oDigs;      return sign(ox)*g/1   /*normalize to original decimal digits.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
IrootErr: say '***error*** (from Iroot): root'  y  "can't be even if 1st argument is < 0."
          return  '[n/a]'                        /*return a  "not applicable"  string.  */
```

'''output'''   using the default inputs:

```txt

 list = 1 2 3 4 5 6 7 8 9 10
Amean = 5.5
Gmean = 4.5287286881167647622
Hmean = 3.4141715214740550062

```



## Ring


```ring

decimals(8)
array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
see "arithmetic mean = "  + arithmeticMean(array) + nl
see "geometric mean =  "  + geometricMean(array) + nl
see "harmonic mean =  "  + harmonicMean(array) + nl

func arithmeticMean a
     return summary(a) / len(a)

func geometricMean a
     b = 1
     for i = 1 to len(a)
         b *= a[i]
     next
     return pow(b, (1/len(a)))

func harmonicMean a
     b = list(len(a))
     for nr = 1 to len(a)
         b[nr] = 1/a[nr]
     next
     return len(a) / summary(b)

func summary s
     sum = 0
     for n = 1 to len(s)
         sum += s[n]
     next
     return sum

```

Output:

```txt

arithmetic mean = 5.50000000
geometric mean =  4.52872869
harmonic mean =  3.41417152

```



## Ruby

{{works with|Ruby|1.9+}}

```ruby
class Array
  def arithmetic_mean
    inject(0.0, :+) / length
  end

  def geometric_mean
    inject(:*) ** (1.0 / length)
  end

  def harmonic_mean
    length / inject(0.0) {|s, m| s + 1.0/m}
  end
end

class Range
  def method_missing(m, *args)
    case m
    when /_mean$/ then to_a.send(m)
    else super
    end
  end
end

p a = (1..10).arithmetic_mean
p g = (1..10).geometric_mean
p h = (1..10).harmonic_mean
# is h < g < a ??
p g.between?(h, a)
```


{{out}}

```txt

5.5
4.528728688116765
3.414171521474055
true

```



## Run BASIC


```runbasic
bXsum   = 1
for i   = 1 to 10
  sum   = sum + i                ' sum of 1 -> 10
  bXsum = bXsum * i              ' sum i * i
  sum1i = sum1i + (1/i)          ' sum 1/i
next

average   = sum / 10
geometric = bXsum ^ (1/10)
harmonic  = 10/sum1i

 print "ArithmeticMean:";average
 print "Geometric Mean:";geometric
 print " Harmonic Mean:";harmonic

 if (average >= geometric) and (geometric >= harmonic) then print "True" else print "False"
```

{{out}}

```txt

Arithmetic Mean:5.5
 Geometric Mean:4.52872869
  Harmonic Mean:3.41417132
True
```



## Rust


```rust
fn main() {
    let mut sum = 0.0;
    let mut prod = 1;
    let mut recsum = 0.0;
    for i in 1..11{
        sum += i as f32;
        prod *= i;
        recsum += 1.0/(i as f32);
    }
    let avg = sum/10.0;
    let gmean = (prod as f32).powf(0.1);
    let hmean = 10.0/recsum;
    println!("Average: {}, Geometric mean: {}, Harmonic mean: {}", avg, gmean, hmean);
    assert!( ( (avg >= gmean) && (gmean >= hmean) ), "Incorrect calculation");

}

```

{{out}}

```txt

Average: 5.5, Geometric mean:4.528729, Harmonic mean: 3.4141712

```



## Scala

{{works with|Scala|2.8+}}

```scala
def arithmeticMean(n: Seq[Int]) = n.sum / n.size.toDouble
def geometricMean(n: Seq[Int])  = math.pow(n.foldLeft(1.0)(_*_), 1.0 / n.size.toDouble)
def harmonicMean(n: Seq[Int])   = n.size / n.map(1.0 / _).sum

var nums = 1 to 10
var a = arithmeticMean(nums)
var g = geometricMean(nums)
var h = harmonicMean(nums)

println("Arithmetic mean " + a)
println("Geometric mean  " + g)
println("Harmonic mean   " + h)

assert(a >= g && g >= h)
```

{{out}}

```txt
Arithmetic mean 5.5
Geometric mean  4.528728688116765
Harmonic mean   3.414171521474055
```



## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
(define (a-mean l)
  (/ (apply + l) (length l)))

(define (g-mean l)
  (expt (apply * l) (/ (length l))))

(define (h-mean l)
  (/ (length l) (apply + (map / l))))

(define (iota start stop)
  (if (> start stop)
      (list)
      (cons start (iota (+ start 1) stop))))

(let* ((l (iota 1 10)) (a (a-mean l)) (g (g-mean l)) (h (h-mean l)))
  (display a)
  (display " >= ")
  (display g)
  (display " >= ")
  (display h)
  (newline)
  (display (>= a g h))
  (newline))
```

{{out}}
<lang>11/2 >= 4.528728688116765 >= 25200/7381
#t
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const array float: numbers is [] (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0);

const func proc: main is func
  local
    var float: number is 0.0;
    var float: sum is 0.0;
    var float: product is 1.0;
    var float: reciprocalSum is 0.0;
  begin
    for number range numbers do
      sum +:= number;
      product *:= number;
      reciprocalSum +:= 1.0 / number;
    end for;
    writeln("Arithmetic mean: " <& sum / flt(length(numbers)));
    writeln("Geometric mean:  " <& product ** (1.0 / flt(length(numbers))));
    writeln("Harmonic mean:   " <& flt(length(numbers)) / reciprocalSum);
  end func;
```


{{out}}

```txt

Arithmetic mean: 5.5
Geometric mean:  4.528728961944580078125
Harmonic mean:   3.4141712188720703125

```



## Sidef


```sidef
func A(a) { a.sum / a.len }
func G(a) { a.prod.root(a.len) }
func H(a) { a.len / a.map{1/_}.sum }
```


The same thing, using hyper-operators:

```sidef
func A(a) { a«+» / a.len }
func G(a) { a«*» ** (1/a.len) }
func H(a) { a.len / (a«/«1 «+») }
```


Calling the functions:

```sidef
say("A(1,...,10) = ", A(1..10));
say("G(1,...,10) = ", G(1..10));
say("H(1,...,10) = ", H(1..10));
```

{{out}}

```txt
A(1,...,10) = 5.5
G(1,...,10) = 4.528728688116764762203309337195508793499
H(1,...,10) = 3.414171521474055006096734859775098225173
```



## SQL

It may not be possible to calculate a geometric mean in a query, but the other two are easy enough.

```sql

--setup
create table averages (val integer);
insert into averages values (1);
insert into averages values (2);
insert into averages values (3);
insert into averages values (4);
insert into averages values (5);
insert into averages values (6);
insert into averages values (7);
insert into averages values (8);
insert into averages values (9);
insert into averages values (10);
-- calculate means
select
  1/avg(1/val) as harm,
  avg(val) as arith
from
  averages;

```

{{out}}

```txt

      HARM      ARITH
---------- ----------
3.41417152        5.5

```


## Smalltalk

{{works with|GNU Smalltalk}}

This extends the class Collection, so these three methods can be called over any kind of collection, it is enough the the objects of the collection understand +, *, raisedTo, reciprocal and /.


```smalltalk
Collection extend
[
    arithmeticMean
    [
	^ (self fold: [:a :b| a + b ]) / (self size)
    ]

    geometricMean
    [
	^ (self fold: [:a :b| a * b]) raisedTo: (self size reciprocal)
    ]

    harmonicMean
    [
	^ (self size) / ((self collect: [:x|x reciprocal]) fold: [:a :b| a + b ] )
    ]
]

|a|
a := #(1 2 3 4 5 6 7 8 9 10).

a arithmeticMean asFloat displayNl.
a geometricMean asFloat displayNl.
a harmonicMean asFloat displayNl.

((a arithmeticMean) >= (a geometricMean)) displayNl.
((a geometricMean) >= (a harmonicMean)) displayNl.
```


{{out}}

```txt
5.5
4.528728688116765
3.414171521474055
true
true
```



## Stata

The command [http://www.stata.com/help.cgi?ameans ameans] prints the arithmetic, geometric and harmonic means, together with confidence intervals.
<lang>clear all
set obs 10
gen x=_n
ameans x

    Variable |    Type             Obs        Mean       [95% Conf. Interval]
-------------+---------------------------------------------------------------
           x | Arithmetic           10         5.5        3.334149   7.665851
             |  Geometric           10    4.528729        2.680672   7.650836
             |   Harmonic           10    3.414172        2.035664   10.57602
-----------------------------------------------------------------------------
```


## Tcl


```tcl
proc arithmeticMean list {
    set sum 0.0
    foreach value $list { set sum [expr {$sum + $value}] }
    return [expr {$sum / [llength $list]}]
}
proc geometricMean list {
    set product 1.0
    foreach value $list { set product [expr {$product * $value}] }
    return [expr {$product ** (1.0/[llength $list])}]
}
proc harmonicMean list {
    set sum 0.0
    foreach value $list { set sum [expr {$sum + 1.0/$value}] }
    return [expr {[llength $list] / $sum}]
}

set nums {1 2 3 4 5 6 7 8 9 10}
set A10 [arithmeticMean $nums]
set G10 [geometricMean $nums]
set H10 [harmonicMean $nums]
puts "A10=$A10, G10=$G10, H10=$H10"
if {$A10 >= $G10} { puts "A10 >= G10" }
if {$G10 >= $H10} { puts "G10 >= H10" }
```


{{out}}

```txt

A10=5.5, G10=4.528728688116765, H10=3.414171521474055
A10 >= G10
G10 >= H10

```



## Ursala



```Ursala
#import std
#import flo

data = ari10(1.,10.)   # arithmetic progression, length 10 with endpoints 1 and 10

a = mean data
g = exp mean ln* data
h = div/1. mean div/*1. data

#cast %eLbX

main = ^(~&,ordered not fleq) <a,g,h>
```

{{out}}

```txt
(
   <5.500000e+00,4.528729e+00,3.414172e+00>,
   true)
```



## Vala

Most valac setups will need "-X -lm" added to the compile command to include the C math library.


```vala

double arithmetic(int[] list){
	double mean;
	double sum = 0;
	foreach(int number in list){
		sum += number;
	} // foreach

	mean = sum / list.length;

	return mean;
} // end arithmetic mean

double geometric(int[] list){
	double mean;
	double product = 1;
	foreach(int number in list){
		product *= number;
	} // foreach

	mean = Math.pow(product, (1 / (double) list.length));

	return mean;
} // end geometric mean

double harmonic(int[] list){
	double mean;
	double sum_inverse = 0;
	foreach(int number in list){
		sum_inverse += (1 / (double) number);
	} // foreach

	mean = (double) list.length / sum_inverse;

	return mean;
} // end harmonic mean

public static void main(){
	int[] list = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

	double arithmetic_mean = arithmetic(list);
	double geometric_mean = geometric(list);
	double harmonic_mean = harmonic(list);

	// should be 5.5
	stdout.printf("Arithmetic mean: %s\n", arithmetic_mean.to_string());

	// should be 4.528728688116765
	stdout.printf("Geometric mean: %s\n", geometric_mean.to_string());

	// should be 4.528728688116765
	stdout.printf("Harmonic mean: %s\n", harmonic_mean.to_string());
}

```


{{out}}

```txt

Arithmetic mean: 5.5
Geometric mean: 4.5287286881167654
Harmonic mean: 3.4141715214740551

```



## VBA

Uses Excel VBA.

```vb
Private Function arithmetic_mean(s() As Variant) As Double
    arithmetic_mean = WorksheetFunction.Average(s)
End Function
Private Function geometric_mean(s() As Variant) As Double
    geometric_mean = WorksheetFunction.GeoMean(s)
End Function
Private Function harmonic_mean(s() As Variant) As Double
    harmonic_mean = WorksheetFunction.HarMean(s)
End Function
Public Sub pythagorean_means()
    Dim s() As Variant
    s = [{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}]
    Debug.Print "A ="; arithmetic_mean(s)
    Debug.Print "G ="; geometric_mean(s)
    Debug.Print "H ="; harmonic_mean(s)
End Sub
```
{{out}}

```txt
A = 5,5
G = 4,52872868811677
H = 3,41417152147406
```



## VBScript


```vb

Function arithmetic_mean(arr)
	sum = 0
	For i = 0 To UBound(arr)
		sum = sum + arr(i)
	Next
	arithmetic_mean = sum / (UBound(arr)+1)
End Function

Function geometric_mean(arr)
	product = 1
	For i = 0 To UBound(arr)
		product = product * arr(i)
	Next
	geometric_mean = product ^ (1/(UBound(arr)+1))
End Function

Function harmonic_mean(arr)
	sum = 0
	For i = 0 To UBound(arr)
		sum = sum + (1/arr(i))
	Next
	harmonic_mean = (UBound(arr)+1) / sum
End Function

WScript.StdOut.WriteLine arithmetic_mean(Array(1,2,3,4,5,6,7,8,9,10))
WScript.StdOut.WriteLine geometric_mean(Array(1,2,3,4,5,6,7,8,9,10))
WScript.StdOut.WriteLine harmonic_mean(Array(1,2,3,4,5,6,7,8,9,10))

```


{{Out}}

```txt

5.5
4.52872868811677
3.41417152147406

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Runtime.CompilerServices

Module Module1

    <Extension()>
    Function Gmean(n As IEnumerable(Of Double)) As Double
        Return Math.Pow(n.Aggregate(Function(s, i) s * i), 1.0 / n.Count())
    End Function

    <Extension()>
    Function Hmean(n As IEnumerable(Of Double)) As Double
        Return n.Count() / n.Sum(Function(i) 1.0 / i)
    End Function

    Sub Main()
        Dim nums = From n In Enumerable.Range(1, 10) Select CDbl(n)

        Dim a = nums.Average()
        Dim g = nums.Gmean()
        Dim h = nums.Hmean()

        Console.WriteLine("Arithmetic mean {0}", a)
        Console.WriteLine(" Geometric mean {0}", g)
        Console.WriteLine("  Harmonic mean {0}", h)
        Debug.Assert(a >= g AndAlso g >= h)
    End Sub

End Module
```

{{out}}

```txt
Arithmetic mean 5.5
 Geometric mean 4.52872868811677
  Harmonic mean 3.41417152147406
```



## XPL0


```XPL0
include c:\cxpl\codes;

func real Power(X, Y);          \X raised to the Y power
real X, Y;                      \ (from StdLib.xpl)
return Exp(Y * Ln(X));

int  N, Order;
real R, A, A1, G, G1, H, H1;
[A1:= 0.0;  G1:= 1.0;  H1:= 0.0;
Order:= true;
for N:= 1 to 10 do
        [R:= float(N);          \convert integer N to real R
        A1:= A1 + R;
        A:= A1/R;               \arithmetic mean
        G1:= G1 * R;
        G:= Power(G1, 1.0/R);   \geometric mean (Nth root of G1)
        if G>A then Order:= false;
        H1:= H1 + 1.0/R;
        H:= R/H1;               \harmonic mean
        if H>G then Order:= false;
        ];
RlOut(0, A); CrLf(0);
RlOut(0, G); CrLf(0);
RlOut(0, H); CrLf(0);
if not Order then Text(0, "NOT ");
Text(0, "ALWAYS DECREASING ORDER
");
]
```


{{out}}

```txt

    5.50000
    4.52873
    3.41417
ALWAYS DECREASING ORDER

```



## zkl


```zkl
ns:=T(1,2,3,4,5,6,7,8,9,10);
ns.sum(0.0)/ns.len();					   // Arithmetic mean
ns.reduce('*,1.0).pow(1.0/ns.len());			   // Geometric mean
ns.len().toFloat() / ns.reduce(fcn(p,n){ p + 1.0/n },0.0); // Harmonic mean
```

{{out}}

```txt

5.5
4.52873
3.41417

```


[[Category:Geometry]]
