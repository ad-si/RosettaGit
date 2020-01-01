+++
title = "Averages/Root mean square"
description = ""
date = 2019-10-12T23:25:11Z
aliases = []
[extra]
id = 6099
[taxonomies]
categories = []
tags = []
+++

{{task}}

{{task heading}}

Compute the   [[wp:Root mean square|Root mean square]]   of the numbers 1..10.


The   ''root mean square''   is also known by its initials RMS (or rms), and as the '''quadratic mean'''.

The RMS is calculated as the mean of the squares of the numbers, square-rooted:


::: <big><math>x_{\mathrm{rms}} = \sqrt {{{x_1}^2 + {x_2}^2 + \cdots + {x_n}^2} \over n}. </math></big>


{{task heading|See also}}

{{Related tasks/Statistical measures}}


<hr>


## 11l

{{trans|Python}}

```11l
F qmean(num)
   R sqrt(sum(num.map(n -> n * n)) / Float(num.len))

print(qmean(1..10))
```

{{out}}

```txt

6.20484

```



## Ada


```Ada
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
procedure calcrms is
	type float_arr is array(1..10) of Float;

	function rms(nums : float_arr) return Float is
		sum : Float := 0.0;
		begin
		for p in nums'Range loop
			sum := sum + nums(p)**2;
		end loop;
		return sqrt(sum/Float(nums'Length));
	end rms;

	list : float_arr;
begin
list := (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0);
put( rms(list) , Exp=>0);
end calcrms;
```

{{out}}

```txt

 6.20484

```



## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards)}}

```algol68
# Define the rms PROCedure & ABS OPerators for LONG... REAL #
MODE RMSFIELD = #LONG...# REAL;
PROC (RMSFIELD)RMSFIELD rms field sqrt = #long...# sqrt;
INT rms field width = #long...# real width;

PROC crude rms = ([]RMSFIELD v)RMSFIELD: (
  RMSFIELD sum := 0;
  FOR i FROM LWB v TO UPB v DO sum +:= v[i]**2 OD;
  rms field sqrt(sum / (UPB v - LWB v + 1))
);

PROC rms = ([]RMSFIELD v)RMSFIELD: (
# round off error accumulated at standard precision #
  RMSFIELD sum := 0, round off error:= 0;
  FOR i FROM LWB v TO UPB v DO
    RMSFIELD org = sum, prod = v[i]**2;
    sum +:= prod;
    round off error +:= sum - org - prod
  OD;
  rms field sqrt((sum - round off error)/(UPB v - LWB v + 1))
);

main: (
  []RMSFIELD one to ten = (1,2,3,4,5,6,7,8,9,10);

  print(("crude rms(one to ten): ", crude rms(one to ten), new line));
  print(("rms(one to ten): ",       rms(one to ten), new line))
)
```

{{out}}

```txt

crude rms(one to ten): +6.20483682299543e  +0
rms(one to ten): +6.20483682299543e  +0

```



## ALGOL W


```algolw
begin
    % computes the root-mean-square of an array of numbers with               %
    % the specified lower bound (lb) and upper bound (ub)                     %
    real procedure rms( real    array numbers ( * )
                      ; integer value lb
                      ; integer value ub
                      ) ;
        begin
            real sum;
            sum := 0;
            for i := lb until ub do sum := sum + ( numbers(i) * numbers(i) );
            sqrt( sum / ( ( ub - lb ) + 1 ) )
        end rms ;

    % test the rms procedure with the numbers 1 to 10                         %
    real array testNumbers( 1 :: 10 );
    for i := 1 until 10 do testNumbers(i) := i;
    r_format := "A"; r_w := 10; r_d := 4; % set fixed point output           %
    write( "rms of 1 .. 10: ", rms( testNumbers, 1, 10 ) );

end.
```

{{out}}

```txt

rms of 1 .. 10:     6.2048

```



## APL


```APL
 rms←{((+/⍵*2)÷⍴⍵)*0.5}
 x←⍳10

 rms x
6.204836823
```



## AppleScript

{{Trans|JavaScript}}( ES6 version )

```AppleScript
-- rootMeanSquare :: [Num] -> Real
on rootMeanSquare(xs)
    script
        on |λ|(a, x)
            a + x * x
        end |λ|
    end script

    (foldl(result, 0, xs) / (length of xs)) ^ (1 / 2)
end rootMeanSquare


-- TEST -----------------------------------------------------------------------
on run

    rootMeanSquare({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})

    -- > 6.204836822995
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

```txt
6.204836822995
```



## Astro


```python
sqrt(mean(x²))
```



## AutoHotkey


### Using a loop


```autohotkey
MsgBox, % RMS(1, 10)


;---------------------------------------------------------------------------
RMS(a, b) { ; Root Mean Square of integers a through b
;---------------------------------------------------------------------------
    n := b - a + 1
    Loop, %n%
        Sum += (a + A_Index - 1) ** 2
    Return, Sqrt(Sum / n)
}
```

Message box shows:

```txt

6.204837

```


### Avoiding a loop

Using these equations:

<math>\sum_{i=1}^n i^2 = \frac{n(n+1)(2n+1)}{6}</math> See [[wp:List of mathematical series]]


for <math>a<b</math> : <math>\sum_{i=a}^b i^2 = \sum_{i=1}^b i^2 - \sum_{i=1}^{a-1} i^2</math>


We can show that:

<math>\sum_{i=a}^b i^2 = \frac{b(b+1)(2b+1)-a(a-1)(2a-1)}{6}</math>

```autohotkey
MsgBox, % RMS(1, 10)


;---------------------------------------------------------------------------
RMS(a, b) { ; Root Mean Square of integers a through b
;---------------------------------------------------------------------------
    Return, Sqrt((b*(b+1)*(2*b+1)-a*(a-1)*(2*a-1))/6/(b-a+1))
}
```

Message box shows:

```txt

6.204837

```



## AWK


```awk
#!/usr/bin/awk -f
# computes RMS of the 1st column of a data file
{
    x  = $1;   # value of 1st column
    S += x*x;
    N++;
}

END {
   print "RMS: ",sqrt(S/N);
}
```



## BASIC

{{works with|QBasic}}

Note that this will work in [[Visual Basic]] and the Windows versions of [[PowerBASIC]] by simply wrapping the module-level code into the <code>MAIN</code> function, and changing <code>PRINT</code> to <code>MSGBOX</code>.


```qbasic
DIM i(1 TO 10) AS DOUBLE, L0 AS LONG
FOR L0 = 1 TO 10
    i(L0) = L0
NEXT
PRINT STR$(rms#(i()))

FUNCTION rms# (what() AS DOUBLE)
    DIM L0 AS LONG, tmp AS DOUBLE, rt AS DOUBLE
    FOR L0 = LBOUND(what) TO UBOUND(what)
        rt = rt + (what(L0) ^ 2)
    NEXT
    tmp = UBOUND(what) - LBOUND(what) + 1
    rms# = SQR(rt / tmp)
END FUNCTION
```


See also: [[#BBC BASIC|BBC BASIC]], [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]], [[#Run BASIC|Run BASIC]]

=
## Applesoft BASIC
=


```ApplesoftBasic
 10 N = 10
 20  FOR I = 1 TO N
 30 S = S + I * I
 40  NEXT
 50 X =  SQR (S / N)
 60  PRINT X
```


{{out}}

```txt
6.20483683
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PRINT RMS(10)
110 DEF RMS(N)
120   LET R=0
130   FOR X=1 TO N
140     LET R=R+X^2
150   NEXT
160   LET RMS=SQR(R/N)
170 END DEF
```


=
## Sinclair ZX81 BASIC
=

```basic
10 FAST
20 LET RMS=0
30 FOR X=1 TO 10
40 LET RMS=RMS+X**2
50 NEXT X
60 LET RMS=SQR (RMS/10)
70 SLOW
80 PRINT RMS
```

{{out}}

```txt
6.2048368
```


=
## BBC BASIC
=

```bbcbasic
      DIM array(9)
      array() = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

      PRINT FNrms(array())
      END

      DEF FNrms(a()) = MOD(a()) / SQR(DIM(a(),1)+1)
```



## C


```c
#include <stdio.h>
#include <math.h>

double rms(double *v, int n)
{
  int i;
  double sum = 0.0;
  for(i = 0; i < n; i++)
    sum += v[i] * v[i];
  return sqrt(sum / n);
}

int main(void)
{
  double v[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10.};
  printf("%f\n", rms(v, sizeof(v)/sizeof(double)));
  return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
using System;

namespace rms
{
    class Program
    {
        static void Main(string[] args)
        {
            int[] x = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            Console.WriteLine(rootMeanSquare(x));
        }

        private static double rootMeanSquare(int[] x)
        {
            double sum = 0;
            for (int i = 0; i < x.Length; i++)
            {
                sum += (x[i]*x[i]);
            }
            return Math.Sqrt(sum / x.Length);
        }
    }
}
```

An alternative method demonstrating the more functional style introduced by LINQ and lambda expressions in C# 3.
{{works with|C sharp|C#|3}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace rms
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine(rootMeanSquare(Enumerable.Range(1, 10)));
        }

        private static double rootMeanSquare(IEnumerable<int> x)
        {
            return Math.Sqrt(x.Average(i => (double)i * i));
        }
    }
}
```



## C++


```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <numeric>

int main( ) {
  std::vector<int> numbers ;
  for ( int i = 1 ; i < 11 ; i++ )
    numbers.push_back( i ) ;
  double meansquare = sqrt( ( std::inner_product( numbers.begin(), numbers.end(), numbers.begin(), 0 ) ) / static_cast<double>( numbers.size() ) );
  std::cout << "The quadratic mean of the numbers 1 .. " << numbers.size() << " is " << meansquare << " !\n" ;
  return 0 ;
}
```

{{out}}

```txt

The quadratic mean of the numbers 1 .. 10 is 6.20484 !

```



## Clojure


```clojure

(defn rms [xs]
  (Math/sqrt (/ (reduce + (map #(* % %) xs))
	   (count xs))))

(println (rms (range 1 11)))
```

{{out}}

```txt

6.2048368229954285

```



## COBOL

Could be written more succinctly, with an inline loop and more <tt>COMPUTE</tt> statements; but that wouldn't be very COBOLic.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. QUADRATIC-MEAN-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  QUADRATIC-MEAN-VARS.
    05 N               PIC 99        VALUE 0.
    05 N-SQUARED       PIC 999.
    05 RUNNING-TOTAL   PIC 999       VALUE 0.
    05 MEAN-OF-SQUARES PIC 99V9(16).
    05 QUADRATIC-MEAN  PIC 9V9(15).
PROCEDURE DIVISION.
CONTROL-PARAGRAPH.
    PERFORM MULTIPLICATION-PARAGRAPH 10 TIMES.
    DIVIDE  RUNNING-TOTAL BY 10 GIVING MEAN-OF-SQUARES.
    COMPUTE QUADRATIC-MEAN = FUNCTION SQRT(MEAN-OF-SQUARES).
    DISPLAY QUADRATIC-MEAN UPON CONSOLE.
    STOP RUN.
MULTIPLICATION-PARAGRAPH.
    ADD      1         TO N.
    MULTIPLY N         BY N GIVING N-SQUARED.
    ADD      N-SQUARED TO RUNNING-TOTAL.
```

{{out}}

```txt
6.204836822995428
```



## CoffeeScript

{{trans|JavaScript}}

```coffeescript
    root_mean_square = (ary) ->
        sum_of_squares = ary.reduce ((s,x) -> s + x*x), 0
        return Math.sqrt(sum_of_squares / ary.length)

    alert root_mean_square([1..10])
```



## Common Lisp


```lisp
(loop for x from 1 to 10
      for xx = (* x x)
      for n from 1
      summing xx into xx-sum
      finally (return (sqrt (/ xx-sum n))))
```


Here's a non-iterative solution.


```lisp

(defun root-mean-square (numbers)
  "Takes a list of numbers, returns their quadratic mean."
  (sqrt
   (/ (apply #'+ (mapcar #'(lambda (x) (* x x)) numbers))
      (length numbers))))

(root-mean-square (loop for i from 1 to 10 collect i))

```



## Crystal

{{trans|Ruby}}

```ruby
def rms(seq)
  Math.sqrt(seq.reduce(0.0) {|sum, x| sum + x*x} / seq.size)
end

puts rms (1..10).to_a

```


{{out}}

```txt

6.2048368229954285

```



## D


```d
import std.stdio, std.math, std.algorithm, std.range;

real rms(R)(R d) pure {
    return sqrt(d.reduce!((a, b) => a + b * b) / real(d.length));
}

void main() {
    writefln("%.19f", iota(1, 11).rms);
}
```

{{out}}

```txt

6.2048368229954282979

```


=={{header|Delphi}}/{{header|Pascal}}==

```Delphi
program AveragesMeanSquare;

{$APPTYPE CONSOLE}

uses Types;

function MeanSquare(aArray: TDoubleDynArray): Double;
var
  lValue: Double;
begin
  Result := 0;

  for lValue in aArray do
    Result := Result + (lValue * lValue);
  if Result > 0 then
    Result := Sqrt(Result / Length(aArray));
end;

begin
  Writeln(MeanSquare(TDoubleDynArray.Create()));
  Writeln(MeanSquare(TDoubleDynArray.Create(1,2,3,4,5,6,7,8,9,10)));
end.
```



## E

Using the same generic mean function as used in [[../Pythagorean means#E|pythagorean means]]:

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

def RMS := makeMean(0, fn b,x { b+x**2 }, fn acc,n { (acc/n).sqrt() })
```



```e
? RMS(1..10)
# value: 6.2048368229954285
```



## EchoLisp


```scheme

(define (rms xs)
    (sqrt (// (for/sum ((x xs)) (* x x)) (length xs))))

(rms (range 1 11))
    → 6.2048368229954285

```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import extensions;
import system'routines;
import system'math;

extension op
{
    get RootMeanSquare()
    {
        ^ (self.selectBy:(x => x * x).summarize(new Real()) / self.Length).sqrt()
    }
}

public program()
{
    console.printLine(new Range(1, 10).RootMeanSquare)
}
```

{{out}}

```txt

6.204836822995

```



## Elixir


```elixir

defmodule RC do
  def root_mean_square(enum) do
    enum
    |> square
    |> mean
    |> :math.sqrt
  end

  defp mean(enum), do: Enum.sum(enum) / Enum.count(enum)

  defp square(enum), do: (for x <- enum, do: x * x)
end

IO.puts RC.root_mean_square(1..10)

```

{{out}}

```txt

6.2048368229954285

```



## Emacs Lisp

<Lang lisp>
(defun rms (nums)
  ;; `/' returns a float only when given floats
  (setq nums (mapcar 'float nums))
  (sqrt (/ (apply '+ (mapcar (lambda (x) (* x x)) nums))
	   (length nums))))

```


or, if using Emacs's Common Lisp library <code>cl-lib.el</code> to use <code>cl-map</code>:
<Lang lisp>
(defun rms (nums)
  (setq nums (mapcar 'float nums))
  (sqrt (/ (apply '+ (cl-map 'list '* nums nums))
	   (length nums))))

(rms (number-sequence 1 10))

```


```txt
6.2048368229954285
```



## Erlang


```erlang
rms(Nums) ->
    math:sqrt(lists:foldl(fun(E,S) -> S+E*E end, 0, Nums) / length(Nums)).

rms([1,2,3,4,5,6,7,8,9,10]).
```

{{out}}

```txt
6.2048368229954285
```



## ERRE

<lang>
PROGRAM ROOT_MEAN_SQUARE
BEGIN
  N=10
  FOR I=1 TO N DO
     S=S+I*I
  END FOR
  X=SQR(S/N)
  PRINT("Root mean square is";X)
END PROGRAM

```

You can, obviously, generalize reading data from a DATA line or from a file.


## Euphoria


```euphoria
function rms(sequence s)
    atom sum
    if length(s) = 0 then
        return 0
    end if
    sum = 0
    for i = 1 to length(s) do
        sum += power(s[i],2)
    end for
    return sqrt(sum/length(s))
end function

constant s = {1,2,3,4,5,6,7,8,9,10}
? rms(s)
```

{{out}}

```txt
6.204836823
```



## Excel

If values are entered in the cells A1 to A10, the below expression will give the RMS value

```excel

=SQRT(SUMSQ($A1:$A10)/COUNT($A1:$A10))

```


The RMS of [1,10] is then : 6.204836823 ( Actual displayed value 6.204837)

=={{header|F Sharp|F#}}==
Uses a lambda expression and function piping.

```Fsharp
let RMS (x:float list) : float = List.map (fun y -> y**2.0) x |> List.average |> System.Math.Sqrt

let res = RMS [1.0..10.0]
```

Answer (in F# Interactive window):

```txt
val res : float = 6.204836823
```



## Fantom


```fantom
class Main
{
  static Float averageRms (Float[] nums)
  {
    if (nums.size == 0) return 0.0f
    Float sum := 0f
    nums.each { sum += it * it }
    return (sum / nums.size.toFloat).sqrt
  }

  public static Void main ()
  {
    a := [1f,2f,3f,4f,5f,6f,7f,8f,9f,10f]
    echo ("RMS Average of $a is: " + averageRms(a))
  }
}
```



## Factor


```factor
: root-mean-square ( seq -- mean )
    [ [ sq ] map-sum ] [ length ] bi / sqrt ;
```


 ( scratchpad ) 10 [1,b] root-mean-square .
 6.204836822995428


## Forth


```forth
: rms ( faddr len -- frms )
  dup >r 0e
  floats bounds do
    i f@ fdup f* f+
  float +loop
  r> s>f f/ fsqrt ;

create test 1e f, 2e f, 3e f, 4e f, 5e f, 6e f, 7e f, 8e f, 9e f, 10e f,
test 10 rms f.    \ 6.20483682299543
```



## Fortran

Assume <math> x </math> stored in array x.

```Fortran
print *,sqrt( sum(x**2)/size(x) )
```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Function QuadraticMean(array() As Double) As Double
  Dim length As Integer = Ubound(array) - Lbound(array) + 1
  Dim As Double sum = 0.0
  For i As Integer = LBound(array) To UBound(array)
    sum += array(i) * array(i)
  Next
  Return Sqr(sum/length)
End Function

Dim vector(1 To 10) As Double
For i As Integer = 1 To 10
  vector(i) = i
Next

Print "Quadratic mean (or RMS) is :"; QuadraticMean(vector())
Print
Print "Press any key to quit the program"
Sleep

```


{{out}}

```txt

Quadratic mean (or RMS) is : 6.204836822995429

```



## Futhark



```Futhark

import "futlib/math"

fun main(as: [n]f64): f64 =
  f64.sqrt ((reduce (+) 0.0 (map (**2.0) as)) / f64(n))

```



## GEORGE


```GEORGE

1, 10 rep (i)
   i i | (v) ;
0
 1, 10 rep (i)
   i dup mult +
   ]
10 div
 sqrt
 print

```


```txt

 6.204836822995428

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    const n = 10
    sum := 0.
    for x := 1.; x <= n; x++ {
        sum += x * x
    }
    fmt.Println(math.Sqrt(sum / n))
}
```

{{out}}

```txt

6.2048368229954285

```



## Groovy

Solution:

```groovy
def quadMean = { list ->
    list == null \
        ? null \
        : list.empty \
            ? 0 \
            : ((list.collect { it*it }.sum()) / list.size()) ** 0.5
}
```

Test:

```groovy
def list = 1..10
def Q = quadMean(list)
println """
list: ${list}
   Q: ${Q}
"""
```

{{out}}

```txt
list: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
   Q: 6.2048368229954285
```



## Haskell

Given the <code>mean</code> function defined in [[Averages/Pythagorean means]]:

```haskell
main = print $ mean 2 [1 .. 10]
```


Or, writing a naive '''mean''' of our own, (but see https://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/):


```haskell
import Data.List (genericLength)

rootMeanSquare :: [Double] -> Double
rootMeanSquare = sqrt . (((/) . foldr ((+) . (^ 2)) 0) <*> genericLength)

main :: IO ()
main = print $ rootMeanSquare [1 .. 10]
```

{{Out}}

```txt
6.2048368229954285
```



## HicEst


```HicEst
sum = 0
DO i = 1, 10
   sum = sum + i^2
ENDDO
WRITE(ClipBoard) "RMS(1..10) = ", (sum/10)^0.5
```

RMS(1..10) = 6.204836823

=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
every put(x := [], 1 to 10)
writes("x := [ "); every writes(!x," "); write("]")
write("Quadratic mean:",q := qmean!x)
end
```




```Icon
procedure qmean(L[])             #: quadratic mean
   local m
   if *L = 0 then fail
   every (m := 0.0) +:= !L^2
   return sqrt(m / *L)
end
```



## Io


```Io
rms := method (figs, (figs map(** 2) reduce(+) / figs size) sqrt)

rms( Range 1 to(10) asList ) println
```



## J

'''Solution:'''

```j
rms=: (+/ % #)&.:*:
```


'''Example Usage:'''

```j
  rms 1 + i. 10
6.20484
```

<code>*:</code> means [http://jsoftware.com/help/dictionary/d112.htm square]

<code>(+/ % #)</code> is an idiom for [[../Arithmetic_mean#J|mean]].

<code>&.:</code> means [http://jsoftware.com/help/dictionary/d631c.htm under]  -- in other words, we square numbers, take their average and then use the inverse of square on the result.  (see also the page on [http://jsoftware.com/help/dictionary/d631.htm &.] which does basically the same thing but with different granularity -- item at a time instead of everything at once.


## Java


```java
public class RootMeanSquare {

    public static double rootMeanSquare(double... nums) {
        double sum = 0.0;
        for (double num : nums)
            sum += num * num;
        return Math.sqrt(sum / nums.length);
    }

    public static void main(String[] args) {
        double[] nums = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
        System.out.println("The RMS of the numbers from 1 to 10 is " + rootMeanSquare(nums));
    }
}
```

{{out}}

```txt
The RMS of the numbers from 1 to 10 is 6.2048368229954285
```



## JavaScript


### ES5

{{works with|JavaScript|1.8}}
{{works with|Firefox|3.0}}

```javascript
function root_mean_square(ary) {
    var sum_of_squares = ary.reduce(function(s,x) {return (s + x*x)}, 0);
    return Math.sqrt(sum_of_squares / ary.length);
}

print( root_mean_square([1,2,3,4,5,6,7,8,9,10]) ); // ==> 6.2048368229954285
```




### ES6



```JavaScript
(() => {
    'use strict';


    // rootMeanSquare :: [Num] -> Real
    const rootMeanSquare = xs =>
       Math.sqrt(
            xs.reduce(
                (a, x) => (a + x * x),
                0
           ) / xs.length
        );


    return rootMeanSquare([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

     // -> 6.2048368229954285
})();
```


{{Out}}

```txt
6.2048368229954285
```



## jq

The following filter returns ''null'' if given an empty array:

```jq
def rms: length as $length
  | if $length == 0 then null
    else map(. * .) | add | sqrt / $length
    end ;
```
With this definition, the following program would compute the rms of each array in a file or stream of numeric arrays:
```jq>rms</lang


## Julia

There are a variety of ways to do this via built-in functions in Julia, given an array <code>A = [1:10]</code> of values.  The formula can be implemented directly as:

```julia
sqrt(sum(A.^2.) / length(A))
```

or shorter with using Statistics (and as spoken: root-mean-square)

```julia
sqrt(mean(A.^2.))
```

or the implicit allocation of a new array by <code>A.^2.</code> can be avoided by using <code>sum</code> as a higher-order function:
```julia
sqrt(sum(x -> x*x, A) / length(A))
```

One can also use an explicit loop for near-C performance

```julia

function rms(A)
   s = 0.0
   for a in A
      s += a*a
   end
   return sqrt(s / length(A))
end

```

Potentially even better is to use the built-in <code>norm</code> function, which computes the square root of the sum of the squares of the entries of <code>A</code> in a way that avoids the possibility of spurious floating-point overflow (if the entries of <code>A</code> are so large that they may overflow if squared):
```julia
norm(A) / sqrt(length(A))
```



## K


```K

  rms:{_sqrt (+/x^2)%#x}
  rms 1+!10
6.204837

```



## Kotlin


```scala
// version 1.0.5-2

fun quadraticMean(vector: Array<Double>) : Double {
    val sum = vector.sumByDouble { it * it }
    return Math.sqrt(sum / vector.size)
}

fun main(args: Array<String>) {
    val vector = Array(10, { (it + 1).toDouble() })
    print("Quadratic mean of numbers 1 to 10 is ${quadraticMean(vector)}")
}
```


{{out}}

```txt

Quadratic mean of numbers 1 to 10 is 6.2048368229954285

```



## Lasso


```Lasso
define rms(a::staticarray)::decimal => {
	return math_sqrt((with n in #a sum #n*#n) / decimal(#a->size))
}
rms(generateSeries(1,10)->asStaticArray)
```


{{out}}

```txt
6.204837
```



## Liberty BASIC


```lb
'   [RC] Averages/Root mean square

    SourceList$     ="1 2 3 4 5 6 7 8 9 10"

    '   If saved as an array we'd have to have a flag for last data.
    '   LB has the very useful word$() to read from delimited strings.
    '   The default delimiter is a space character, " ".

    SumOfSquares    =0
    n               =0      '   This holds index to number, and counts number of data.
    data$           ="666"  '   temporary dummy to enter the loop.

    while data$ <>""                                '   we loop until no data left.
        data$           =word$( SourceList$, n +1)  '   first data, as a string
        NewVal          =val( data$)                '   convert string to number
        SumOfSquares    =SumOfSquares +NewVal^2     '   add to existing sum of squares
        n =n +1                                     '   increment number of data items found
    wend

    n =n -1

    print "Supplied data was ";         SourceList$
    print "This contained ";            n; " numbers."
    print "R.M.S. value is ";           ( SumOfSquares /n)^0.5

    end
```



## Logo


```logo
to rms :v
  output sqrt quotient (apply "sum map [? * ?] :v) count :v
end

show rms iseq 1 10
```



## Lua


```lua
function sumsq(a, ...) return a and a^2 + sumsq(...) or 0 end
function rms(t) return (sumsq(unpack(t)) / #t)^.5 end

print(rms{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
```



## Maple


```Maple
y := [ seq(1..10) ]:
RMS := proc( x )
    return sqrt( Statistics:-Mean( x ^~ 2 ) );
end proc:
RMS( y );

```

{{out}}

```txt
6.20483682299543

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
RootMeanSquare@Range[10]
```

The above will give the precise solution <math>\sqrt{\frac{77}{2}}</math>, to downgrade to 6.20484, use '<code>10.</code>' to imply asking for numeric solution, or append '<code>//N</code>' after the whole expression.


## MATLAB


```MATLAB
function rms = quadraticMean(list)
    rms = sqrt(mean(list.^2));
end
```

Solution:

```MATLAB>>
 quadraticMean((1:10))

ans =

   6.204836822995429
```



## Maxima


```maxima
L: makelist(i, i, 10)$

rms(L) := sqrt(lsum(x^2, x, L)/length(L))$

rms(L), numer;   /* 6.204836822995429 */
```


## MAXScript


```MAXScript

fn RMS arr =
(
	local sumSquared = 0
	for i in arr do sumSquared += i^2
	return (sqrt (sumSquared/arr.count as float))
)

```

Output:

```MAXScript

rms #{1..10}
6.20484

```


=={{header|МК-61/52}}==
<lang>0	П0	П1	С/П	x^2	ИП0	x^2	ИП1	*
+	ИП1	1	+	П1	/	КвКор	П0	БП
03
```


''Instruction:'' В/О С/П Number С/П Number ...

Each time you press the С/П on the indicator would mean already entered numbers.


## Morfa

{{trans|D}}

```morfa

import morfa.base;
import morfa.functional.base;

template <TRange>
func rms(d: TRange): float
{
    var count = 1;
    return sqrt(reduce( (a: float, b: float) { count += 1; return a + b * b; }, d) / count);
}

func main(): void
{
    println(rms(1 .. 11));
}

```

{{out}}

```txt

6.204837

```



## Nemerle


```Nemerle
using System;
using System.Console;
using System.Math;

module RMS
{
    RMS(x : list[int]) : double
    {
        def sum = x.Map(fun (x) {x*x}).FoldLeft(0, _+_);
        Sqrt((sum :> double) / x.Length)
    }

    Main() : void
    {
        WriteLine("RMS of [1 .. 10]: {0:g6}", RMS($[1 .. 10]));
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

parse arg maxV .
if maxV = '' | maxV = '.' then maxV = 10

sum = 0
loop nr = 1 for maxV
  sum = sum + nr ** 2
  end nr
rmsD = Math.sqrt(sum / maxV)

say 'RMS of values from 1 to' maxV':' rmsD

return

```

{{out}}

```txt

RMS of values from 1 to 10: 6.204836822995428

```



## Nim


```nim
from math import sqrt, sum
from sequtils import mapIt

proc qmean(num: seq[float]): float =
  result = num.mapIt(it * it).sum
  result = sqrt(result / float(num.len))

echo qmean(@[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0])
```

{{out}}

```txt
6.2048368229954285e+00
```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE QM;
IMPORT ML := MathL, Out;
VAR
	nums: ARRAY 10 OF LONGREAL;
	i: INTEGER;

PROCEDURE Rms(a: ARRAY OF LONGREAL): LONGREAL;
VAR
	i: INTEGER;
	s: LONGREAL;
BEGIN
	s := 0.0;
	FOR i := 0 TO LEN(a) - 1 DO
		s := s + (a[i] * a[i])
	END;
	RETURN ML.Sqrt(s / LEN(a))
END Rms;

BEGIN
	FOR i := 0 TO LEN(nums) - 1 DO
		nums[i] := i + 1
	END;
	Out.String("Quadratic Mean: ");Out.LongReal(Rms(nums));Out.Ln
END QM.

```

{{out}}

```txt

Quadratic Mean: 6.20483682300

```


## Objeck


```objeck
bundle Default {
  class Hello {
    function : Main(args : String[]) ~ Nil {
      values := [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
      RootSquareMean(values)->PrintLine();
    }

    function : native : RootSquareMean(values : Float[]) ~ Float {
      sum := 0.0;
      each(i : values) {
        x := values[i]->Power(2.0);
        sum += values[i]->Power(2.0);
      };

      return (sum / values->Size())->SquareRoot();
    }
  }
}
```



## OCaml


```ocaml
let rms a =
  sqrt (Array.fold_left (fun s x -> s +. x*.x) 0.0 a /.
        float_of_int (Array.length a))
;;

rms (Array.init 10 (fun i -> float_of_int (i+1))) ;;
(* 6.2048368229954285 *)
```



## Oforth



```Oforth
10 seq map(#sq) sum 10.0 / sqrt .
```


{{out}}

```txt

6.20483682299543

```



## ooRexx


```ooRexx
call testAverage .array~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
call testAverage .array~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11)
call testAverage .array~of(30, 10, 20, 30, 40, 50, -100, 4.7, -11e2)

::routine testAverage
  use arg list
  say "list =" list~toString("l", ", ")
  say "root mean square =" rootmeansquare(list)
  say

::routine rootmeansquare
  use arg numbers
  -- return zero for an empty list
  if numbers~isempty then return 0

  sum = 0
  do number over numbers
      sum += number * number
  end
  return rxcalcsqrt(sum/numbers~items)

::requires rxmath LIBRARY
```

{{out}}

```txt
list = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
root mean square = 6.20483682

list = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11
root mean square = 5.06630766

list = 30, 10, 20, 30, 40, 50, -100, 4.7, -1100
root mean square = 369.146476
```



## Oz


```oz
declare
  fun {Square X} X*X end

  fun {RMS Xs}
     {Sqrt
      {Int.toFloat {FoldL {Map Xs Square} Number.'+' 0}}
      /
      {Int.toFloat {Length Xs}}}
  end
in
  {Show {RMS {List.number 1 10 1}}}
```

{{out}}

```txt

6.2048

```



## PARI/GP

General RMS calculation:

```parigp
RMS(v)={
  sqrt(sum(i=1,#v,v[i]^2)/#v)
};

RMS(vector(10,i,i))
```


Specific functions for the first ''n'' positive integers:

```parigp
RMS_first(n)={
  sqrt((n+1)*(2*n+1)/6)
};

RMS_first(10)
```

Asymptotically this is n/sqrt(3).


## Perl


```perl
use v5.10.0;
sub rms
{
        my $r = 0;
        $r += $_**2 for @_;
        sqrt( $r/@_ );
}

say rms(1..10);
```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
sub rms(*@nums) { sqrt [+](@nums X** 2) / @nums }

say rms 1..10;
```


Here's a slightly more concise version, albeit arguably less readable:

```perl6
sub rms { sqrt @_ R/ [+] @_ X** 2 }
```



## Phix


```Phix
function rms(sequence s)
atom sqsum = 0
    for i=1 to length(s) do
        sqsum += power(s[i],2)
    end for
    return sqrt(sqsum/length(s))
end function

? rms({1,2,3,4,5,6,7,8,9,10})
```

{{out}}

```txt

6.204836823

```



## PHP


```PHP
<?php
// Created with PHP 7.0

function rms(array $numbers)
{
    $sum = 0;

    foreach ($numbers as $number) {
        $sum += $number**2;
    }

    return sqrt($sum / count($numbers));
}

echo rms(array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));

```

{{out}}

```txt

6.2048368229954

```



## PicoLisp


```PicoLisp
(scl 5)

(let Lst (1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)
   (prinl
      (format
         (sqrt
            (*/
               (sum '((N) (*/ N N 1.0)) Lst)
               1.0
               (length Lst) )
            T )
         *Scl ) ) )
```

{{out}}

```txt
6.20484
```



## PL/I


```PL/I
 atest: Proc Options(main);
 declare A(10) Dec Float(15) static initial (1,2,3,4,5,6,7,8,9,10);
 declare (n,RMS) Dec Float(15);
 n = hbound(A,1);
 RMS = sqrt(sum(A**2)/n);
 put Skip Data(rms);
 End;
```

{{out}}

```txt
RMS= 6.20483682299543E+0000;
```



## PostScript


```postscript
/findrms{
/x exch def
/sum 0 def
/i 0 def
x length 0 eq{}
{
x length{
/sum x i get 2 exp sum add def
/i i 1 add def
}repeat
/sum sum x length div sqrt def
}ifelse
sum ==
}def

[1 2 3 4 5 6 7 8 9 10] findrms
```

{{out}}

```txt

6.20483685

```

{{libheader|initlib}}

```postscript
[1 10] 1 range dup 0 {dup * +} fold exch length div sqrt
```



## Powerbuilder


```powerbuilder
long ll_x, ll_y, ll_product
decimal ld_rms

ll_x = 1
ll_y = 10
DO WHILE ll_x <= ll_y
	ll_product += ll_x * ll_x
	ll_x ++
LOOP
ld_rms = Sqrt(ll_product / ll_y)

//ld_rms value is 6.20483682299542849
```



## PowerShell


```PowerShell
function get-rms([float[]]$nums){
   $sqsum=$nums | foreach-object { $_*$_} | measure-object -sum | select-object -expand Sum
   return [math]::sqrt($sqsum/$nums.count)
}

get-rms @(1..10)
```



## PureBasic


```PureBasic
NewList MyList()  ; To hold a unknown amount of numbers to calculate

If OpenConsole()
  Define.d result
  Define i, sum_of_squares

  ;Populate a random amounts of numbers to calculate
  For i=0 To (Random(45)+5) ; max elements is unknown to the program
    AddElement(MyList())
    MyList()=Random(15)  ; Put in a random number
  Next

  Print("Averages/Root mean square"+#CRLF$+"of : ")

  ; Calculate square of each element, print each & add them together
  ForEach MyList()
    Print(Str(MyList())+" ")             ; Present to our user
    sum_of_squares+MyList()*MyList()     ; Sum the squares, e.g
  Next

  ;Present the result
  result=Sqr(sum_of_squares/ListSize(MyList()))
  PrintN(#CRLF$+"= "+StrD(result))

  PrintN("Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```



## Python

{{works with|Python|3}}

```Python>>>
 from math import sqrt
>>> def qmean(num):
	return sqrt(sum(n*n for n in num)/len(num))

>>> qmean(range(1,11))
6.2048368229954285
```

<small>Note that function [http://docs.python.org/release/3.2/library/functions.html#range range] in Python includes the first limit of 1, excludes the second limit of 11, and has a default increment of 1.</small>

The Python 2 version of this is nearly identical, except you must cast the sum to a float to get float division instead of integer division; or better, do a <code>from __future__ import division</code>, which works on Python 2.2+ as well as Python 3, and makes division work consistently like it does in Python 3.


Alternatively in terms of '''reduce''':

```python
from functools import (reduce)
from math import (sqrt)


# rootMeanSquare :: [Num] -> Float
def rootMeanSquare(xs):
    return sqrt(reduce(lambda a, x: a + x * x, xs, 0) / len(xs))


print(
    rootMeanSquare([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
)
```

{{Out}}

```txt
6.2048368229954285
```



## Qi


```qi
(define rms
  R -> (sqrt (/ (APPLY + (MAPCAR * R R)) (length R))))
```



## R

We may calculate the answer directly using R's built-in <code>sqrt</code> and <code>mean</code> functions:

```R
sqrt(mean((1:10)^2))
```

The following function works for any vector x:

```R
RMS = function(x){
 sqrt(mean(x^2))
}
```

Usage:

```R>
 RMS(1:10)
[1] 6.204837
```



## Racket


```Racket

#lang racket
(define (rms nums)
  (sqrt (/ (for/sum ([n nums]) (* n n)) (length nums))))

```



## REXX

REXX has no built-in   '''sqrt'''   function,  so a RYO version is included here.


This particular   '''sqrt'''   function was programmed for speed, as it has two critical components:
:::*   the initial guess (for the square root)
:::*   the number of (increasing) decimal digits used during the computations

The   '''sqrt'''   code was optimized to use the minimum amount of digits (precision) for each iteration of the

calculation as well as a reasonable attempt at providing a first-guess square root by essentially halving

the number using logarithmic (base ten) arithmetic.

```rexx
/*REXX program computes and displays the  root mean square (RMS)  of a number sequence. */
parse arg nums digs show .                       /*obtain the optional arguments from CL*/
if nums==''  |  nums==","  then nums=10          /*Not specified?  Then use the default.*/
if digs==''  |  digs==","  then digs=50          /* "      "         "   "   "     "    */
if show==''  |  show==","  then show=10          /* "      "         "   "   "     "    */
numeric digits digs                              /*uses  DIGS  decimal digits for calc. */
$=0;                     do j=1  for nums        /*process each of the   N   integers.  */
                         $=$ + j**2              /*sum the   squares   of the integers. */
                         end   /*j*/
                                                 /* [↓]  displays  SHOW  decimal digits.*/
rms=format( sqrt($/nums), , show ) / 1           /*divide by N, then calculate the SQRT.*/
say 'root mean square for 1──►'nums  "is: "  rms /*display the  root mean square (RMS). */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x;  if x=0  then return 0;  d=digits();  numeric digits;  m.=9
       numeric form;  parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g *.5'e'_ % 2
       h=d+6;    do j=0  while h>9;       m.j=h;                h=h%2+1;        end  /*j*/
                 do k=j+5  to 0  by -1;   numeric digits m.k;   g=(g+x/g)*.5;   end  /*k*/
       return g
```

'''output'''   when using the default inputs:

```txt

root mean square for 1──►10 is:  6.204836823

```



## Ring


```ring

nums = [1,2,3,4,5,6,7,8,9,10]
sum = 0
decimals(5)
see "Average = " + average(nums) + nl

func average number
     for i = 1 to len(number)
         sum = sum + pow(number[i],2)
     next
     x = sqrt(sum / len(number))
     return x

```



## Ruby


```ruby
class Array
  def quadratic_mean
    Math.sqrt( self.inject(0.0) {|s, y| s + y*y} / self.length )
  end
end

class Range
  def quadratic_mean
    self.to_a.quadratic_mean
  end
end

(1..10).quadratic_mean  # => 6.2048368229954285
```


and a non object-oriented solution:

```ruby
def rms(seq)
  Math.sqrt(seq.inject(0.0) {|sum, x| sum + x*x} / seq.length)
end
puts rms (1..10).to_a   # => 6.2048368229954285
```



## Run BASIC


```runbasic
valueList$   = "1 2 3 4 5 6 7 8 9 10"
while word$(valueList$,i +1) <> ""             ' grab values from list
  thisValue  = val(word$(valueList$,i +1))     ' turn values into numbers
  sumSquares = sumSquares + thisValue ^ 2      ' sum up the squares
  i = i +1                                     '
wend
print "List of Values:";valueList$;" containing ";i;" values"
print "Root Mean Square =";(sumSquares/i)^0.5
```


{{out}}
List of Values:1 2 3 4 5 6 7 8 9 10 containing 10 values
Root Mean Square =6.20483682


## Rust


```rust
fn root_mean_square(vec: Vec<i32>) -> f32 {
    let sum_squares = vec.iter().fold(0, |acc, &x| acc + x.pow(2));
    return ((sum_squares as f32)/(vec.len() as f32)).sqrt();
}

fn main() {
    let vec = (1..11).collect();
    println!("The root mean square is: {}", root_mean_square(vec));
}
```


{{out}}
The root mean square is: 6.204837


=={{header|S-lang}}==
Many of math operations in S-Lang are 'vectorized', that is, given
an array, they apply themselves to each element.  In this case, that
means no array_map() function needed.  Also, "range arrays" have a
built-in syntax.

<lang S-lang>define rms(arr)
{
  return sqrt(sum(sqr(arr)) / length(arr));
}

print(rms([1:10]));
```



## Sather


```sather
class MAIN is
  -- irrms stands for Integer Ranged RMS
  irrms(i, f:INT):FLT
    pre i <= f
  is
    sum ::= 0;
    loop
      sum := sum + i.upto!(f).pow(2);
    end;
    return (sum.flt / (f-i+1).flt).sqrt;
  end;

  main is
    #OUT + irrms(1, 10) + "\n";
  end;
end;
```



## Scala


```scala
def rms(nums: Seq[Int]) = math.sqrt(nums.map(math.pow(_, 2)).sum / nums.size)
println(rms(1 to 10))
```

{{out}}

```txt
6.2048368229954285
```



## Scheme


```scheme
(define (rms nums)
  (sqrt (/ (apply + (map * nums nums))
           (length nums))))

(rms '(1 2 3 4 5 6 7 8 9 10))
```

{{out}}

```txt
6.20483682299543
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const array float: numbers is [] (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0);

const func float: rms (in array float: numbers) is func
  result
    var float: rms is 0.0;
  local
    var float: number is 0.0;
    var float: sum is 0.0;
  begin
    for number range numbers do
      sum +:= number ** 2;
    end for;
    rms := sqrt(sum / flt(length(numbers)));
  end func;

const proc: main is func
  begin
    writeln(rms(numbers) digits 7);
  end func;
```


## Shen

{{works with|shen-scheme|0.17}}

```Shen
(declare scm.sqrt [number --> number])

(tc +)

(define mean
  { (list number) --> number }
  Xs -> (/ (sum Xs) (length Xs)))

(define square
  { number --> number }
  X -> (* X X))

(define rms
  { (list number) --> number }
  Xs -> (scm.sqrt (mean (map (function square) Xs))))

(define iota-h
  { number --> number --> (list number) }
  X X -> [X]
  X Lim -> (cons X (iota-h (+ X 1) Lim)))

(define iota
  { number --> (list number) }
  Lim -> (iota-h 1 Lim))

(output "~A~%" (rms (iota 10)))
```



## Sidef


```ruby
func rms(a) {
    sqrt(a.map{.**2}.sum / a.len)
}

say rms(1..10)
```


Using hyper operators, we can write it as:

```ruby
func rms(a) { a »**» 2 «+» / a.len -> sqrt }
```


{{out}}

```txt
6.20483682299542829806662097772473784992796529536
```



## Smalltalk


```smalltalk
(((1 to: 10) inject: 0 into: [ :s :n | n*n + s ]) / 10) sqrt.
```



## SNOBOL4

{{works with|Macro Spitbol}}
{{works with|CSnobol}}
There is no built-in sqrt( ) function in Snobol4+.

```SNOBOL4
        define('rms(a)i,ssq') :(rms_end)
rms     i = i + 1; ssq = ssq + (a<i> * a<i>) :s(rms)
        rms = sqrt(1.0 * ssq / prototype(a)) :(return)
rms_end

*       # Fill array, test and display
        str = '1 2 3 4 5 6 7 8 9 10'; a = array(10)
loop    i = i + 1; str len(p) span('0123456789') . a<i> @p :s(loop)
        output = str ' -> ' rms(a)
end
```

{{out}}

```txt
1 2 3 4 5 6 7 8 9 10 -> 6.20483682
```



## Standard ML


```sml
fun rms(v: real vector) =
  let
    val v' = Vector.map (fn x => x*x) v
    val sum = Vector.foldl op+ 0.0 v'
  in
    Math.sqrt( sum/real(Vector.length(v')) )
  end;

rms(Vector.tabulate(10, fn n => real(n+1)));
```

{{out}}

```txt
val it = 6.204836823 : real
```



## Stata

Compute the RMS of a variable and return the result in r(rms).


```stata
program rms, rclass
	syntax varname(numeric) [if] [in]
	tempvar x
	gen `x'=`varlist'^2 `if' `in'
	qui sum `x' `if' `in'
	return scalar rms=sqrt(r(mean))
end
```


'''Example'''


```stata
clear
set obs 20
gen x=rnormal()

rms x
di r(rms)
1.0394189

rms x if x>0
di r(rms)
.7423647
```



## Tcl

{{works with|Tcl|8.5}}

```tcl
proc qmean list {
    set sum 0.0
    foreach value $list { set sum [expr {$sum + $value**2}] }
    return [expr { sqrt($sum / [llength $list]) }]
}

puts "RMS(1..10) = [qmean {1 2 3 4 5 6 7 8 9 10}]"
```

{{out}}

```txt

RMS(1..10) = 6.2048368229954285

```



## Ursala

using the <code>mean</code> function among others from the <code>flo</code> library

```Ursala
#import nat
#import flo

#cast %e

rms = sqrt mean sqr* float* nrange(1,10)
```

{{out}}

```txt

6.204837e+00

```



## Vala

Valac probably needs to have the flag "-X -lm" added to include the C Math library.

```vala
double rms(double[] list){
	double sum_squares = 0;
	double mean;

	foreach ( double number in list){
		sum_squares += (number * number);
	}

	mean = Math.sqrt(sum_squares / (double) list.length);

	return mean;
} // end rms

public static void main(){
	double[] list = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	double mean = rms(list);

	stdout.printf("%s\n", mean.to_string());
}
```

{{out}}

```txt

6.2048368229954285

```




## VBA

Using Excel VBA

```vb
Private Function root_mean_square(s() As Variant) As Double
    For i = 1 To UBound(s)
        s(i) = s(i) ^ 2
    Next i
    root_mean_square = Sqr(WorksheetFunction.sum(s) / UBound(s))
End Function
Public Sub pythagorean_means()
    Dim s() As Variant
    s = [{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}]
    Debug.Print root_mean_square(s)
End Sub
```

Without using Excel worksheetfunction:

```vb
Function rms(iLow As Integer, iHigh As Integer)
    Dim i As Integer
    If iLow > iHigh Then
        i = iLow
        iLow = iHigh
        iHigh = i
    End If
    For i = iLow To iHigh
        rms = rms + i ^ 2
    Next i
    rms = Sqr(rms / (iHigh - iLow + 1))
End Function

Sub foo()
    Debug.Print rms(1, 10)
End Sub

```


Output:

```txt

 6.20483682299543

```



## Wortel


```wortel
@let {
  ; using a composition and a fork (like you would do in J)
  rms1 ^(@sqrt @(@sum / #) *^@sq)

  ; using a function with a named argument
  rms2 &a @sqrt ~/ #a @sum !*^@sq a

  [[
    !rms1 @to 10
    !rms2 @to 10
  ]]
}
```

{{out}}

```txt
[6.2048368229954285 6.2048368229954285]
```



## XLISP


```lisp
(defun quadratic-mean (xs)
    (sqrt
        (/
            (apply +
                (mapcar (lambda (x) (expt x 2)) xs))
            (length xs))))

; define a RANGE function, for testing purposes

(defun range (x y)
    (if (< x y)
        (cons x (range (+ x 1) y))))

; test QUADRATIC-MEAN

(print (quadratic-mean (range 1 11)))
```

{{out}}

```txt
6.20483682299543
```



## XPL0


```XPL0
code CrLf=9;
code real RlOut=48;
int  N;
real S;
[S:= 0.0;
for N:= 1 to 10 do S:= S + sq(float(N));
RlOut(0, sqrt(S/10.0));
CrLf(0);
]
```

{{out}}

```txt

    6.20484

```



## Yacas


```Yacas
Sqrt(Add((1 .. 10)^2)/10)
```

The above will give the precise solution <math>\sqrt{\frac{77}{2}}</math>, to downgrade to 6.20483682299, surround the expression with '<code>N()</code>'.


## zkl


```zkl
fcn rms(z){ ( z.reduce(fcn(p,n){ p + n*n },0.0) /z.len() ).sqrt() }
```

The order in the reduce function is important as it coerces n*n to float.

```txt

zkl: rms([1..10].walk())  //-->rms(T(1,2,3,4,5,6,7,8,9,10))
6.20484

```

