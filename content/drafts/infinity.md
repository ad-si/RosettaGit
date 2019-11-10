+++
title = "Infinity"
description = ""
date = 2019-10-22T03:34:40Z
aliases = []
[extra]
id = 2476
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Discrete math]]

;Task:
Write a function which tests if infinity is supported for floating point numbers (this step should be omitted for languages where the language specification already demands the existence of infinity, e.g. by demanding [[IEEE]] numbers), and if so, returns positive infinity.   Otherwise, return the largest possible positive floating point number.

For languages with several floating point types, use the type of the literal constant   '''1.5'''   as floating point type.


;Related task:
*   [[Extreme floating point values]]





## ActionScript

ActionScript has the built in function isFinite() to test if a number is finite or not.

```actionscript
trace(5 / 0); // outputs "Infinity"
trace(isFinite(5 / 0)); // outputs "false"
```



## Ada


```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Infinities is
   function Sup return Float is -- Only for predefined types
      Result : Float := Float'Last;
   begin
      if not Float'Machine_Overflows then
         Result := Float'Succ (Result);
      end if;
      return Result;
   end Sup;

   function Inf return Float is -- Only for predefined types
      Result : Float := Float'First;
   begin
      if not Float'Machine_Overflows then
         Result := Float'Pred (Result);
      end if;
      return Result;
   end Inf;
begin
   Put_Line ("Supremum" & Float'Image (Sup));
   Put_Line ("Infimum " & Float'Image (Inf));
end Infinities;
```

The language-defined attribute Machine_Overflows is defined for each floating-point type. It is true when an overflow or divide-by-zero results in Constraint_Error exception propagation. When the underlying machine type is incapable to implement this semantics the attribute is false. It is to expect that on the machines with [[IEEE]] 754 hardware Machine_Overflows is true. The language-defined attributes Succ and Pred yield the value next or previous to the argument, correspondingly.

Sample output on a machine where Float is [[IEEE]] 754:

```txt

Supremum +Inf*******
Infimum -Inf*******

```

Note that the code above does not work for user-defined types, which may have range of values narrower than one of the underlying hardware type. This case represents one of the reasons why [[Ada]] programmers are advised not to use predefined floating-point types. There is a danger that the implementation of might be [[IEEE]] 754, and so the program semantics could be broken.

Here is the code that should work for any type on any machine:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Infinities is
   type Real is digits 5 range -10.0..10.0;

   function Sup return Real is
      Result : Real := Real'Last;
   begin
      return Real'Succ (Result);
   exception
      when Constraint_Error =>
         return Result;
   end Sup;

   function Inf return Real is
      Result : Real := Real'First;
   begin
      return Real'Pred (Result);
   exception
      when Constraint_Error =>
         return Result;
   end Inf;
begin
   Put_Line ("Supremum" & Real'Image (Sup));
   Put_Line ("Infimum " & Real'Image (Inf));
end Infinities;
```

Sample output. Note that the compiler is required to generate Constraint_Error even if the hardware is [[IEEE]] 754. So the upper and lower bounds are 10.0 and -10.0:

```txt

Supremum 1.0000E+01
Infimum -1.0000E+01

```


### Getting rid of IEEE ideals

There is a simple way to strip [[IEEE]] 754 ideals (non-numeric values) from a predefined floating-point type such as Float or Long_Float:

```ada
subtype Safe_Float is Float range Float'Range;
```

The subtype Safe_Float keeps all the range of Float, yet behaves properly upon overflow, underflow and zero-divide.


## ALGOL 68

[[ALGOL 68R]] (from [[wp:Royal_Radar_Establishment|Royal Radar Establishment]]) has an ''infinity'' variable as part of the ''standard prelude'', on the [[wp:ICT 1900|ICL 1900 Series]] [[wp:mainframe|mainframe]]s the value of ''infinity'' is 5.79860446188₁₀76 (the same as ''max float'').

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''ted transput}}
Note: The underlying hardware may sometimes support an infinity, but the ALGOL 68 standard itself does not, and gives no way of setting a variable to either &plusmn;&infin;.

ALGOL 68 does have some 7 built in [[Exceptions#ALGOL_68|exceptions]], these might be used to detect exceptions during transput, and so <u>if</u> the underlying hardware <u>does</u> support &infin;, then it would be detected with a ''on value error'' while printing and if ''mended'' would appear as a field full of ''error char''.


```algol68
printf(($"max int: "gl$,max int));
printf(($"long max int: "gl$,long max int));
printf(($"long long max int: "gl$,long long max int));
printf(($"max real: "gl$,max real));
printf(($"long max real: "gl$,long max real));
printf(($"long long max real: "gl$,long long max real));
printf(($"error char: "gl$,error char))
```

Output:

```txt

max int: +2147483647
long max int: +99999999999999999999999999999999999
long long max int: +9999999999999999999999999999999999999999999999999999999999999999999999
max real: +1.79769313486235e+308
long max real: +1.000000000000000000000000e+999999
long long max real: +1.00000000000000000000000000000000000000000000000000000000000e+999999
error char: *

```



## Argile

{{trans|C}} (simplified)

```Argile
use std
printf "%f\n" atof "infinity" (: this prints "inf" :)
#extern :atof<text>: -> real
```



## AWK



```AWK
  BEGIN {
    k=1;
    while (2^(k-1) < 2^k) k++;
    INF = 2^k;
    print INF;
  }
```


This has been tested with GAWK 3.1.7 and MAWK, both return

```txt
 inf
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      *FLOAT 64
      PRINT FNinfinity
      END

      DEF FNinfinity
      LOCAL supported%, maxpos, prev, inct
      supported% = TRUE
      ON ERROR LOCAL supported% = FALSE
      IF supported% THEN = 1/0
      RESTORE ERROR
      inct = 1E10
      REPEAT
        prev = maxpos
        inct *= 2
        ON ERROR LOCAL inct /= 2
        maxpos += inct
        RESTORE ERROR
      UNTIL maxpos = prev
      = maxpos
```

Output:

```txt

1.79769313E308

```



## C

A previous solution used <tt>atof("infinity")</tt>, which returned infinity with some C libraries but returned zero with [[MinGW]].

C89 has a macro HUGE_VAL in <math.h>. HUGE_VAL is a <tt>double</tt>. HUGE_VAL will be infinity if infinity exists, else it will be the largest possible number. HUGE_VAL is a <tt>double</tt>.


```c>#include <math.h
	/* HUGE_VAL */
#include <stdio.h>	/* printf() */

double inf(void) {
  return HUGE_VAL;
}

int main() {
  printf("%g\n", inf());
  return 0;
}
```


The output from the above program might be "inf", "1.#INF", or something else.

C99 also has a macro for infinity:


```c
#define _ISOC99_SOURCE

#include <math.h>
#include <stdio.h>

int main() {
  printf("%g\n", INFINITY);
  return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
using System;

class Program
{
    static double PositiveInfinity()
    {
        return double.PositiveInfinity;
    }

    static void Main()
    {
        Console.WriteLine(PositiveInfinity());
    }
}
```

Output:
<lang>Infinity
```


## C++



```cpp>#include <limits


double inf()
{
  if (std::numeric_limits<double>::has_infinity)
    return std::numeric_limits<double>::infinity();
  else
    return std::numeric_limits<double>::max();
}
```


## Clojure

{{trans|Java}}
Java's floating-point types (float, double) all support infinity. Clojure has literals for infinity:

```clojure
##Inf  ;; same as Double/POSITIVE_INFINITY
##-Inf ;; same as Double/NEGATIVE_INFINITY
(Double/isInfinite ##Inf) ;; true
```


The largest possible number in Java (without using the Big classes) is also in the Double class
(def biggestNumber Double/MAX_VALUE). Its value is (1+(1-2^(-52)))*2^1023 or 1.7976931348623157*10^308 (a.k.a. "big"). Other number classes (Integer, Long, Float, Byte, and Short) have maximum values that can be accessed in the same way.


## CoffeeScript

{{trans|JavaScript}}
CoffeeScript compiles to JavaScript, and as such it inherits the properties of JavaScript.

JavaScript has a special global property called "Infinity":

```coffeescript>Infinity</lang

as well as constants in the Number class:

```coffeescript
Number.POSITIVE_INFINITY
Number.NEGATIVE_INFINITY
```


The global isFinite function tests for finiteness:

```coffeescript>isFinite x</lang



## Common Lisp


Common Lisp does not specify an infinity value.  Some implementations may have support for IEEE infinity, however.  For instance, CMUCL supports [http://common-lisp.net/project/cmucl/downloads/doc/cmu-user-old/extensions.html#toc7 IEEE Special Values].  Common Lisp does specify that implementations define [http://www.lispworks.com/documentation/HyperSpec/Body/v_most_1.htm constants] with most (and least) positive (and negative) values.  These may vary between implementations.

{{works with|LispWorks}} 5.1.2, Intel, OS X, 32-bit


```lisp>
 (apropos "MOST-POSITIVE" :cl)
MOST-POSITIVE-LONG-FLOAT, value: 1.7976931348623158D308
MOST-POSITIVE-SHORT-FLOAT, value: 3.4028172S38
MOST-POSITIVE-SINGLE-FLOAT, value: 3.4028235E38
MOST-POSITIVE-DOUBLE-FLOAT, value: 1.7976931348623158D308
MOST-POSITIVE-FIXNUM, value: 536870911

> (apropos "MOST-NEGATIVE" :cl)
MOST-NEGATIVE-SINGLE-FLOAT, value: -3.4028235E38
MOST-NEGATIVE-LONG-FLOAT, value: -1.7976931348623158D308
MOST-NEGATIVE-SHORT-FLOAT, value: -3.4028172S38
MOST-NEGATIVE-DOUBLE-FLOAT, value: -1.7976931348623158D308
MOST-NEGATIVE-FIXNUM, value: -536870912
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Infinity;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	x: REAL;
BEGIN
	x := 1 / 0;
	StdLog.String("x:> ");StdLog.Real(x);StdLog.Ln
END Do;


```

Execute: ^Q Infinity.Do<br/>
Output:

```txt

x:>  inf

```


## D



```d
auto inf() {
    return typeof(1.5).infinity;
}

void main() {}
```



## Delphi


Delphi defines the following constants in Math:

```Delphi
  Infinity    =  1.0 / 0.0;
  NegInfinity = -1.0 / 0.0;
```


Test for infinite value using:

```Delphi
Math.IsInfinite()
```



## Dyalect


Dyalect floating point number support positive infinity:


```Dyalect
func infinityTask() {
    Float.inf
}
```



## E



```e
def infinityTask() {
    return Infinity # predefined variable holding positive infinity
}
```



## Eiffel


```eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make
feature {NONE} -- Initialization
	number:REAL_64
	make
			-- Run application.
		do
			number := 2^2000
			print(number)
			print("%N")
			print(number.is_positive_infinity)
			print("%N")
		end
end

```


Output:

```txt

Infinity
True

```



## Erlang


No infinity available. Largest floating point number is supposed to be 1.80e308 (IEEE 754-1985 double precision 64 bits) but that did not work. However 1.79e308 is fine, so max float is somewhere close to 1.80e308.


## ERRE

Every type has its "infinity" constant: MAXINT for 16-bit integer, MAXREAL for single precision
floating and MAXLONGREAL for double precision floating. An infinity test can be achieved with
an EXCEPTION:

```ERRE

PROGRAM INFINITY

EXCEPTION
    PRINT("INFINITY")
    ESCI%=TRUE
END EXCEPTION

BEGIN
    ESCI%=FALSE
    K=1
    WHILE 2^K>0 DO
       EXIT IF ESCI%
       K+=1
    END WHILE
END PROGRAM

```



## Euphoria



```Euphoria
constant infinity = 1E400

? infinity -- outputs "inf"
```


=={{header|F_Sharp|F#}}==

```fsharp

printfn "%f" (1.0/0.0)

```

{{out}}

```txt

Infinity

```


## Factor


```factor>1/0.</lang



## Fantom


Fantom's <code>Float</code> data type is an IEEE 754 64-bit floating point type.  Positive infinity is represented by the constant <code>posInf</code>.


```fantom

class Main
{
  static Float getInfinity () { Float.posInf }
  public static Void main () { echo (getInfinity ()) }
}

```



## Forth


```forth
: inf ( -- f ) 1e 0e f/ ;
inf f.    \ implementation specific. GNU Forth will output "inf"

: inf? ( f -- ? ) s" MAX-FLOAT" environment? drop f> ;
\ IEEE infinity is the only value for which this will return true

: has-inf ( -- ? ) ['] inf catch if false else inf? then ;
```



## Fortran

ISO Fortran 2003 or later supports an IEEE_ARITHMETIC module which defines a wide range of intrinsic functions and types in support of IEEE floating point formats and arithmetic rules.

```fortran
program to_f_the_ineffable
   use, intrinsic :: ieee_arithmetic
   integer :: i
   real dimension(2) :: y, x = (/ 30, ieee_value(y,ieee_positive_inf) /)

   do i = 1, 2
      if (ieee_support_datatype(x(i))) then
         if (ieee_is_finite(x(i))) then
            print *, 'x(',i,') is finite'
         else
            print *, 'x(',i,') is infinite'
         end if

      else
         print *, 'x(',i,') is not in an IEEE-supported format'
      end if
   end do
end program to_f_the_ineffable
```


ISO Fortran 90 or later supports a HUGE intrinsic which returns the largest value supported by the data type of the number given.

```fortran
real :: x
real :: huge_real = huge(x)
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#Include "crt/math.bi"
#Print Typeof(1.5) ' Prints DOUBLE at compile time

Dim d As Typeof(1.5) = INFINITY
Print d; " (String representation of Positive Infinity)"
Sleep

```


{{out}}

```txt

 1.#INF (String representation of Positive Infinity)

```



## GAP


```gap
# Floating point infinity
inf := FLOAT_INT(1) / FLOAT_INT(0);

IS_FLOAT(inf);
#true;

# GAP has also a formal ''infinity'' value
infinity in Cyclotomics;
# true
```



## Go


```go
package main

import (
    "fmt"
    "math"
)

// function called for by task
func posInf() float64 {
    return math.Inf(1) // argument specifies positive infinity
}

func main() {
    x := 1.5 // type of x determined by literal
    // that this compiles demonstrates that PosInf returns same type as x,
    // the type specified by the task.
    x = posInf()                     // test function
    fmt.Println(x, math.IsInf(x, 1)) // demonstrate result
}
```

Output:

```txt

+Inf true

```



## Groovy

Groovy, like Java, requires full support for IEEE 32-bit (Float) and 64-bit (Double) formats. So the solution function would simply return either the Float or Double constant encoded as IEEE infinity.

```groovy
def biggest = { Double.POSITIVE_INFINITY }
```


Test program:

```groovy
println biggest()
printf ( "0x%xL \n", Double.doubleToLongBits(biggest()) )
```


Output:

```txt
Infinity
0x7ff0000000000000L
```



## Haskell


The Haskell 98 standard does not require full IEEE numbers, and the required operations on floating point numbers leave some degree of freedom to the implementation. Also, it's not possible to use the type of the literal 1.0 to decide which concrete type to use, because Haskell number literals are automatically converted.

Nevertheless, the following may come close to the task description:


```haskell>maxRealFloat :: RealFloat a =
 a -> a
maxRealFloat x = encodeFloat b (e-1) `asTypeOf` x where
  b     = floatRadix x - 1
  (_,e) = floatRange x

infinity :: RealFloat a => a
infinity = if isInfinite inf then inf else maxRealFloat 1.0 where
  inf = 1/0
```


Test for the two standard floating point types:


```haskell
*Main> infinity :: Float
Infinity
*Main> infinity :: Double
Infinity
```


Or you can simply use division by 0:

```haskell>Prelude
 1 / 0 :: Float
Infinity
Prelude> 1 / 0 :: Double
Infinity
```


Or use "read" to read the string representation:

```haskell>Prelude
 read "Infinity" :: Float
Infinity
Prelude> read "Infinity" :: Double
Infinity
```


=={{header|Icon}} and {{header|Unicon}}==

Icon and Unicon have no infinity value (or defined maximum or minimum values).  Reals are implemented as C doubles and the behavior could vary somewhat from platform to platform.
Both explicitly check for divide by zero and treat it as a runtime error (201), so it's not clear how you could produce one with the possible exception of externally called code.


## IDL


IDL provides the standard IEEE values for _inf and _NaN in the !Values system structure:


```idl
print, !Values.f_infinity             ;; for normal floats or
print, !Values.D_infinity             ;; for doubles
```



## Io


```io>inf := 1/0</lang


or


```io>Number constants inf</lang


=={{header|IS-BASIC}}==
<lang IS-BASIC>PRINT INF
```

Output:

```txt

9.999999999E62

```



## J

Positive infinity is produced by the primary constant function<tt> _: </tt>.

It is also represented directly as a numeric value by an underscore, used alone.

Example:

```j

   _ * 5 NB. multiplying infinity to 5 results in infinity
_
   5 % _ NB. dividing 5 by infinity results in 0
0
   5 % 0 NB. dividing 5 by 0 results in infinity
_

```



## Java

Java's floating-point types (<tt>float</tt>, <tt>double</tt>) all support infinity. You can get infinity from constants in the corresponding wrapper class; for example, <tt>Double</tt>:

```java
double infinity = Double.POSITIVE_INFINITY; //defined as 1.0/0.0
Double.isInfinite(infinity); //true
```

As a function:

```java
public static double getInf(){
   return Double.POSITIVE_INFINITY;
}
```

The largest possible number in Java (without using the <tt>Big</tt> classes) is also in the <tt>Double</tt> class.

```java
double biggestNumber = Double.MAX_VALUE;
```

Its value is (2-2<sup>-52</sup>)*2<sup>1023</sup> or 1.7976931348623157*10<sup>308</sup> (a.k.a. "big"). Other number classes (<tt>Integer</tt>, <tt>Long</tt>, <tt>Float</tt>, <tt>Byte</tt>, and <tt>Short</tt>) have maximum values that can be accessed in the same way.


## JavaScript

JavaScript has a special global property called "Infinity":

```javascript>Infinity</lang

as well as constants in the Number class:

```javascript
Number.POSITIVE_INFINITY
Number.NEGATIVE_INFINITY
```


The global isFinite() function tests for finiteness:

```javascript
isFinite(x)
```



## jq

jq uses IEEE 754 64-bit floating-point arithmetic, and very large number literals, e.g. 1e1000, are evaluated as IEEE 754 infinity.  If your version of jq does not include `infinite` as a built-in, you could therefore define it as follows:


```jq>def infinite: 1e1000;</lang


To test whether a JSON entity is equal to `infinite`, one can simply use `==` in the expected manner. Thus, assuming `infinite` has been defined, one could define a predicate, isinfinite, as follows:


```jq>def isinfinite: . == infinite;</lang


Currently, the infinite value prints as though it were a very large floating point number.


## Julia

Julia uses IEEE floating-point arithmetic and includes a built-in constant `Inf` for (64-bit) floating-point infinity.


```Julia

infinity() = Inf

```

There is actually a built-in function that does returns infinity in various types, called <tt>inf</tt>, which takes as its argument the type to return infinity for.

```Julia

inf(Float64) # 64-bit Inf
inf(Float32) # 32-bit Inf32
inf(BigFloat) # infinity for arbitrary-precision floating-point arithmetic

```



## Lingo

Lingo stores floats using IEEE 754 double-precision (64-bit) format.
INF is not a constant that can be used programmatically, but only a special return value.

```lingo
x = (1-power(2, -53)) * power(2, 1023) * 2
put ilk(x), x
-- #float 1.79769313486232e308

x = (1-power(2, -53)) * power(2, 1023) * 3
put ilk(x), x, -x
-- #float INF -INF
```



## Lua


```lua

function infinity()
  return 1/0 --lua uses unboxed C floats for all numbers
end

```



## K

K has predefined positive and negative integer and float infinities: -0I, 0I, -0i, 0i. They have following properties:
{{works with|Kona}}

```K
   / Integer infinities
   / 0I is just 2147483647
   / -0I is just -2147483647
   / -2147483648 is a special "null integer"(NaN) 0N
   0I*0I
1
   0I-0I
0
   0I+1
0N
   0I+2
-0I
   0I+3 / -0I+1
-2147483646
   0I-1
2147483646
   0I%0I
1
   0I^2
4.611686e+18
   0I^0I
0i
   0I^-0I
0.0
   1%0
0I
   0%0
0
   0i^2
0i
   0i^0i
0i

   / Floating point infinities in K are something like
   / IEEE 754 values
   / Also there is floating point NaN -- 0n
   0i+1
0i
   0i*0i
0i
   0i-0i
0n
   0i%0i
0n
   0i%0n
0n
   / but
   0.0%0.0
0.0
```



## Kotlin


```scala
fun main(args: Array<String>) {
    val p = Double.POSITIVE_INFINITY // +∞
    println(p.isInfinite()) // true
    println(p.isFinite()) // false
    println("${p < 0} ${p > 0}")  // false true

    val n = Double.NEGATIVE_INFINITY // -∞
    println(n.isInfinite()) // true
    println(n.isFinite()) // false
    println("${n < 0} ${n > 0}")  // true false
}
```

{{out}}

```txt
true
false
false true
true
false
true false
```



## Langur


```Langur
val .isPosInf = f isInfinity(.i) and isPosNumType(.i)
val .x = Infinity
val .y = -Infinity
val .z = 1

writeln ".x: ", .x, ": ", .isPosInf(.x)
writeln ".y: ", .y, ": ", .isPosInf(.y)
writeln ".z: ", .z, ": ", .isPosInf(.z)
```


{{out}}

```txt
.x: Infinity: true
.y: -Infinity: false
.z: 1: false
```



## Lasso

Lasso supports 64-bit decimals.. This gives Lasso's decimal numbers a range from approximately negative to positive 2x10^300 and with precision down to 2x10^-300. Lasso also supports decimal literals for NaN (not a number) as well and positive and negative infinity.

```Lasso
infinity
'<br />'
infinity -> type
```

-> inf

decimal


## M2000 Interpreter


```M2000 Interpreter

Rem : locale 1033
Module CheckIt {
      Form 66,40
      Cls 5
      Pen 14
      \\ Ensure True/False for Print boolean (else -1/0)
      \\ from m2000 console use statement Switches without Set.
      \\ use Monitor statement to see all switches.
      Set Switches "+SBL"
      IF version<9.4 then exit
      IF version=9.4 and revision<25 then exit
      Function Infinity(positive=True) {
            buffer clear inf as byte*8
            m=0x7F
            if not positive then m+=128
            return inf, 7:=m, 6:=0xF0
            =eval(inf, 0 as double)
      }
      K=Infinity(false)
      L=Infinity()
      Function TestNegativeInfinity(k) {
            =str$(k, 1033) = "-1.#INF"
      }
      Function TestPositiveInfinity(k) {
            =str$(k, 1033) = "1.#INF"
      }
      Function TestInvalid {
            =str$(Number, 1033) = "-1.#IND"
      }
      Pen 11 {Print "       True       True"}
      Print TestNegativeInfinity(K), TestPositiveInfinity(L)
      Pen 11 {Print "    -1.#INF     1.#INF    -1.#INF     1.#INF    -1.#INF     1.#INF"}
      Print K, L, K*100, L*100, K+K, L+L
      M=K/L
      Pen 11 {Print "    -1.#IND    -1.#IND       True       True" }
      Print K/L, L/K, TestInvalid(M), TestInvalid(K/L)
      M=K+L
      Pen 11 {Print  "    -1.#IND    -1.#IND    -1.#IND       True       True"}
      Print M, K+L, L+K, TestInvalid(M), TestInvalid(K+L)
      Pen 11 {Print "    -1.#INF     1.#INF"}
      Print 1+K+2, 1+L+2
      Pen 11 {Print "    -1.#INF"}
      Print K-L
      Pen 11 {Print "     1.#INF"}
      Print L-K
}
Checkit

```



## Maple

Maple's floating point numerics are a strict extension of IEEE/754 and IEEE/854 so there is already a built-in infinity.  (In fact, there are several.)  The following procedure just returns the floating point (positive) infinity directly.

```Maple

> proc() Float(infinity) end();
                            Float(infinity)

```

There is also an exact infinity ("infinity"), a negative float infinity ("Float(-infinity)" or "-Float(infinity)") and a suite of complex infinities.  The next procedure returns a boxed machine (double precision) float infinity.

```Maple

> proc() HFloat(infinity) end();
                            HFloat(infinity)

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica has infinity built-in as a symbol. Which can be used throughout the software:

```Mathematica
Sum[1/n^2,{n,Infinity}]
1/Infinity
Integrate[Exp[-x^2], {x, -Infinity, Infinity}]
10^100 < Infinity
```

gives back:

```txt
Pi^2/6
0
Sqrt[Pi]
True
```

Moreover Mathematica has 2 other variables that represent 'infinity': DirectedInfinity[r] and ComplexInfinity. DirectInfinity[r] represents an infinite quantity with complex direction r. ComplexInfinity represents an infinite quantity with an undetermined direction; like 1/0. Which has infinite size but undetermined direction. So the general infinity is DirectedInfinity, however if the direction is unknown it will turn to ComplexInfinity, DirectedInfinity[-1] will return -infinity and DirectedInfinity[1] will return infinity. Directed infinity can, for example, be used to integrate over an infinite domain with a given complex direction: one might want to integrate Exp[-x^2]/(x^2-1) from 0 to DirectedInfinity[Exp[I Pi/4]]:

```Mathematica
Integrate[Exp[-x^2]/(x^2 - 1), {x, 0, DirectedInfinity[Exp[I Pi/4]]}]
```

gives back:

```txt
-((Pi (I+Erfi[1]))/(2 E))
```


=={{header|MATLAB}} / {{header|Octave}}==
MATLAB implements the IEEE 754 floating point standard as the default for all numeric data types. +Inf and -Inf are by default implemented and supported by MATLAB. To check if a variable has the value +/-Inf, one can use the built-in function "isinf()" which will return a Boolean 1 if the number is +/-inf.


```Matlab
a = +Inf;
isinf(a)

```


Returns:

```txt

ans =
     1

```



## Maxima


```maxima
/* Maxima has inf (positive infinity) and minf (negative infinity) */

declare(x, real)$

is(x < inf);
/* true */

is(x > minf);
/* true */

/* However, it is an error to try to divide by zero, even with floating-point numbers */
1.0/0.0;
/* expt: undefined: 0 to a negative exponent.
   -- an error. To debug this try: debugmode(true); */
```



## Metafont


Metafont numbers are a little bit odd (it uses fixed binary arithmetic). For Metafont, the biggest number (and so the one which is also considered to be infinity) is 4095.99998. In fact, in the basic set of macros for Metafont, we can read


```metafont>infinity := 4095.99998;</lang



## MiniScript

MiniScript uses IEEE numerics, so:


```MiniScript
posInfinity = 1/0
print posInfinity
```

{{out}}

```txt
INF
```


=={{header|Modula-2}}==
<lang Modula-2>MODULE inf;

IMPORT  InOut;

BEGIN
  InOut.WriteReal (1.0 / 0.0, 12, 12);
  InOut.WriteLn
END inf.
```

Producing
<lang Modula-2>jan@Beryllium:~/modula/rosetta$ inf

**** RUNTIME ERROR  bound check error
Floating point exception
```


=={{header|Modula-3}}==
IEEESpecial contains 3 variables defining negative infinity, positive infinity, and NaN for all 3 floating point types in Modula-3 (REAL, LONGREAL, and EXTENDED).

If the implementation doesn't support IEEE floats, the program prints arbitrary values (Critical Mass Modula-3 implementation does support IEEE floats).

```modula3
MODULE Inf EXPORTS Main;

IMPORT IO, IEEESpecial;

BEGIN
  IO.PutReal(IEEESpecial.RealPosInf);
  IO.Put("\n");
END Inf.
```


Output:

```txt

Infinity

```



## Nemerle

Both single and double precision floating point numbers support PositiveInfinity, NegativeInfinity and NaN.

```Nemerle
def posinf = double.PositiveInfinity;
def a = IsInfinity(posinf);         // a = true
def b = IsNegativeInfinity(posinf); // b = false
def c = IsPositiveInfinity(posinf); // c = true
```



## Nim


```nim>Inf</lang

is a predefined constant in Nim:

```nim
var f = Inf
echo f
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 PRINT 1/0
```

{{out}}
?DZ ERROR is a division by zero error in NS-HUBASIC.

```txt

?DZ ERROR IN 10

```



## OCaml


```ocaml>infinity</lang

is already a pre-defined value in OCaml.


```txt

# infinity;;
- : float = infinity
# 1.0 /. 0.0;;
- : float = infinity

```




## Oforth



```Oforth>10 1000.0 powf dup println dup neg println 1 swap / println</lang


{{out}}

```txt

1.#INF
-1.#INF
0

```



## Ol


Inexact numbers support can be disabled during recompilation using "-DOLVM_INEXACTS=0" command line argument. Inexact numbers in Ol demands the existence of infinity, by demanding IEEE numbers. There are two signed infinity numbers (as constants) in Ol:
 +inf.0 ; positive infinity
 -inf.0 ; negative infinity


```scheme

(define (infinite? x) (or (equal? x +inf.0) (equal? x -inf.0)))

(infinite? +inf.0) ==> #true
(infinite? -inf.0) ==> #true
(infinite? +nan.0) ==> #false
(infinite? 123456) ==> #false
(infinite? 1/3456) ==> #false
(infinite? 17+28i) ==> #false

```



## OpenEdge/Progress


The unknown value (represented by a question mark) can be considered to equal infinity. There is no difference between positive and negative infinity but the unknown value sometimes sorts low and sometimes sorts high when used in queries.


```progress
MESSAGE
   1.0 / 0.0 SKIP
   -1.0 / 0.0 SKIP(1)
   ( 1.0 / 0.0 ) = ( -1.0 / 0.0 )
VIEW-AS ALERT-BOX.
```


Output


```txt
---------------------------
Message (Press HELP to view stack trace)
---------------------------
?
?

yes
---------------------------
OK   Help
---------------------------
```



## OxygenBasic

Using double precision floats:

```oxygenbasic

print 1.5e-400 '0

print 1.5e400  '#INF

print -1.5e400 '#-INF

print 0/-1.5   '-0

print 1.5/0    '#INF

print -1.5/0   '#-INF

print 0/0      '#qNAN


function f() as double
return -1.5/0
end function

print f '#-INF

```



## Oz


```oz
declare
  PosInf = 1./0.
  NegInf = ~1./0.
in
  {Show PosInf}
  {Show NegInf}

  %% some assertion
  42. / PosInf = 0.
  42. / NegInf = 0.
  PosInf * PosInf = PosInf
  PosInf * NegInf = NegInf
  NegInf * NegInf = PosInf
```



## PARI/GP

{{works with|PARI/GP|version 2.8.0 and higher}}

```parigp>+oo</lang


{{works with|PARI/GP|version 2.2.9 to 2.7.0}}

```parigp
infty()={
  [1] \\ Used for many functions like intnum
};
```



## Pascal

See [[Infinity#Delphi | Delphi]]


## Perl

Positive infinity:

```perl
my $x = 0 + "inf";
my $y = 0 + "+inf";
```

Negative infinity:

```perl
my $x = 0 - "inf";
my $y = 0 + "-inf";
```

The "<code>0 + </code>..." is used here to make sure that the variable stores a value that is actually an infinitive number instead of just a string <code>"inf"</code> but in practice one can use just:

```perl
my $x = "inf";
```

and <code>$x</code> while originally holding a string will get converted to an infinite number when it is first used as a number.

Some programmers use expressions that overflow the IEEE floating point numbers such as:

```perl
my $x = 1e1000;
```

which is 10<sup>1000</sup> or googol<sup>10</sup> or even numbers like this one:

```perl
my $y = 10**10**10;
```

which is 10<sup>10000000000</sup> but it has to make some assumptions about the underlying hardware format and its size. Furthermore, using such literals in the scope of some pragmas such as <code>bigint</code>, <code>bignum</code> or <code>bigrat</code> would actually compute those numbers:


```perl
use bigint;
my $x = 1e1000;
my $y = 10**10**10; # N.B. this will consume vast quantities of RAM
```

Here the <code>$x</code> and <code>$y</code> when printed would give 1001 and 10000000001-digit numbers respectively, the latter taking no less than 10GB of space to just output.

Under those pragmas, however, there is a simpler way to use infinite values, thanks to the <code>inf</code> symbol being exported into the namespace by default:

```perl
use bigint;
my $x = inf;
my $y = -inf;
```



## Perl 6

Inf support is required by language spec on all abstract Numeric types (in the absence of subset constraints) including Num, Rat and Int types. Native integers cannot support Inf, so attempting to assign Inf will result in an exception; native floats are expected to follow IEEE standards including +/- Inf and NaN.

```perl6
my $x = 1.5/0;       # Failure: catchable error, if evaluated will return: "Attempt to divide by zero ...
my $y = (1.5/0).Num; # assigns 'Inf'
```



## Phix


```Phix
constant infinity = 1e300*1e300
? infinity
```

{{out}}

```txt

inf

```



## PHP

This is how you get infinity:

```php>INF</lang

Unfortunately, "1.0 / 0.0" doesn't evaluate to infinity; but instead seems to evaluate to False, which is more like 0 than infinity.

PHP has functions is_finite() and is_infinite() to test for infiniteness.


## PL/I


```PL/I

declare x float, y float (15), z float (18);

put skip list (huge(x), huge(y), huge(z));

```



## PicoLisp

The symbol '[http://software-lab.de/doc/refT.html#T T]' is used to represent
infinite values, e.g. for the length of circular lists, and is greater than any
other value in comparisons. PicoLisp has only very limited floating point
support (scaled bignum arithmetics), but some functions return 'T' for infinite
results.

```PicoLisp
(load "@lib/math.l")

: (exp 1000.0)
-> T
```



## PostScript


```postscript
/infinity { 9 99 exp } def
```



## PowerShell

A .NET floating-point number representing infinity is available.

```powershell
function infinity {
    [double]::PositiveInfinity
}
```



## PureBasic


PureBasic uses [[wp:IEEE_754-2008|IEEE 754]] coding for float types.  PureBasic also includes the function <tt>Infinity()</tt> that return the positive value for infinity  and the boolean function <tt>IsInfinite(value.f)</tt> that returns true if the floating point value is either positive or negative infinity.


```PureBasic
If OpenConsole()
  Define.d a, b
  b = 0

  ;positive infinity
  PrintN(StrD(Infinity())) ;returns the value for positive infinity from builtin function

  a = 1.0
  PrintN(StrD(a / b)) ;calculation results in the value of positive infinity

  ;negative infinity
  PrintN(StrD(-Infinity())) ;returns the value for negative infinity from builtin function

  a = -1.0
  PrintN(StrD(a / b)) ;calculation results in the value of negative infinity

  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf

```


''Outputs''
 +Infinity
 +Infinity
 -Infinity
 -Infinity


## Python

This is how you get infinity:

```python>>>
 float('infinity')
inf
```

''Note: When passing in a string to float(), values for NaN and Infinity may be returned, depending on the underlying C library. The specific set of strings accepted which cause these values to be returned depends entirely on the underlying C library used to compile Python itself, and is known to vary.''

''The Decimal module explicitly supports +/-infinity Nan, +/-0.0, etc without exception.''

Floating-point division by 0 doesn't give you infinity, it raises an exception:

```python>>>
 1.0 / 0.0
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ZeroDivisionError: float division
```


If <tt>float('infinity')</tt> doesn't work on your platform, you could use this trick:

```txt
>>> 1e999
1.#INF
```

It works by trying to create a float bigger than the machine can handle.


## R


```R
 Inf                    #positive infinity
 -Inf                   #negative infinity
 .Machine$double.xmax   # largest finite floating-point number
 is.finite              # function to test to see if a number is finite

# function that returns the input if it is finite, otherwise returns (plus or minus) the largest finite floating-point number
 forcefinite <- function(x) ifelse(is.finite(x), x, sign(x)*.Machine$double.xmax)

 forcefinite(c(1, -1, 0, .Machine$double.xmax, -.Machine$double.xmax, Inf, -Inf))
# [1]   1.000000e+00  -1.000000e+00   0.000000e+00  1.797693e+308
# [5] -1.797693e+308  1.797693e+308 -1.797693e+308
```



## Racket


as in Scheme:


```Racket
#lang racket

+inf.0 ; positive infinity
(define (finite? x) (< -inf.0 x +inf.0))
(define (infinite? x) (not (finite? x)))
```



## REXX

The language specifications for REXX are rather open-ended when it comes to language limits.


Limits on numbers are expressed as:  The REXX interpreter has to at '''least''' handle exponents up to nine (decimal) digits.


So it's up to the writers of the REXX interpreter to decide what limits are to be implemented or enforced.
<pre style="overflow:scroll">
For the default setting of

               NUMERIC DIGITS 9

the biggest number that can be used is  (for the Regina REXX  and  R4  REXX interpreters):

.999999999e+999999999

```

<pre style="overflow:scroll">
For a setting of

              NUMERIC DIGITS 100

the biggest number that can be used is:


(for the Regina REXX interpreter)

.9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999e+999999999


(for the R4 REXX interpreter)

.9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999e+9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999



... and so on with larger  NUMERIC DIGITS

```

For most REXX interpreters, the maximum number of digits is only limited by virtual storage,

but the pratical limit would be a little less than half of available virtual storage,

which would (realistically) be around one billion digits. Other interpreters have a limitation of roughly 8 million digits.




## RLaB


```RLaB

>> x = inf()
         inf
>> isinf(x)
           1
>> inf() > 10
           1
>> -inf() > 10
           0

```




## Ruby

Infinity is a Float value

```ruby>a = 1.0/0       # =
 Infinity
a.finite?       # => false
a.infinite?     # => 1

a = -1/0.0      # => -Infinity
a.infinite?     # => -1

a = Float::MAX  # => 1.79769313486232e+308
a.finite?       # => true
a.infinite?     # => nil
```

{{works with|Ruby|1.9.2+}}

```ruby>a = Float::INFINITY       # => Infinity</lang



## Rust

Rust has builtin function for floating types which returns infinity. This program outputs 'inf'.

```rust
fn main() {
    let inf = std::f32::INFINITY;
    println!("{}", inf);
}
```



## Scala

{{libheader|Scala}}

'''See also'''
* [[Extreme_floating_point_values#Scala]]
In order to be compliant with IEEE-754, Scala has all support for infinity on its floating-point types (<tt>float</tt>, <tt>double</tt>). You can get infinity from constants in the corresponding wrapper class; for example, <tt>Double</tt>:

```Scala
val inf = Double.PositiveInfinity //defined as 1.0/0.0
inf.isInfinite; //true
```

The largest possible number in Scala (without using the <tt>Big</tt> classes) is also in the <tt>Double</tt> class.

```Scala>val biggestNumber = Double.MaxValue</lang


REPL session:

```scala>scala
 1 / 0.
res2: Double = Infinity

scala> -1 / 0.
res3: Double = -Infinity

scala> 1 / Double.PositiveInfinity
res4: Double = 0.0

scala> 1 / Double.NegativeInfinity
res5: Double = -0.0
```



## Scheme


```scheme
+inf.0 ; positive infinity
(define (finite? x) (< -inf.0 x +inf.0))
(define (infinite? x) (not (finite? x)))
```



## Seed7

Seed7s floating-point type ([http://seed7.sourceforge.net/manual/types.htm#float float]) supports infinity.
The library [http://seed7.sourceforge.net/libraries/float.htm float.s7i] defines
the constant [http://seed7.sourceforge.net/libraries/float.htm#Infinity Infinity] as:

```seed7>const float: Infinity is 1.0 / 0.0;</lang

Checks for infinity can be done by comparing with this constant.


## Sidef


```ruby
var a = 1.5/0        # Inf
say a.is_inf         # true
say a.is_pos         # true
 
var b = -1.5/0       # -Inf
say b.is_ninf        # true
say b.is_neg         # true

var inf = Inf
var ninf = -Inf
say (inf == -ninf)   # true
```



## Slate



```slate>PositiveInfinity</lang



## Smalltalk

{{works with|GNU Smalltalk}}
Each of the finite-precision Float classes (FloatE, FloatD, FloatQ), have an "infinity" method that returns infinity in that type.

```txt

st> FloatD infinity
Inf
st> 1.0 / 0.0
Inf

```

{{works with|Smalltalk/X}}
The class names are different (Float, ShortFloat and LongFloat);
for sourcecode compatibility, you can do "Smalltalk at:#FloatQ put:LongFloat".
The behavior is slightly different, in that an exception is raised:

```smalltalk
Float infinity -> INF
1.0 / 0.0 -> "ZeroDivide exception"
```

but we can simulate the other behavior with:

```smalltalk
[
  1.0 / 0.0
] on: ZeroDivide do:[:ex |
  ex proceedWith: (Float infinity)
]
-> INF
```



## Standard ML


```sml>Real.posInf</lang



```txt

- Real.posInf;
val it = inf : real
- 1.0 / 0.0;
val it = inf : real

```



## Swift

Swift's floating-point types (<tt>Float</tt>, <tt>Double</tt>, and any other type that conforms to the <tt>FloatingPointNumber</tt> protocol) all support infinity. You can get infinity from the <tt>infinity</tt> class property in the type:

```swift
let inf = Double.infinity
inf.isInfinite //true
```

As a function:

```swift
func getInf() -> Double {
   return Double.infinity
}
```



## Tcl

{{works with|Tcl|8.5}}

Tcl 8.5 has Infinite as a floating point value, not an integer value

```tcl
package require Tcl 8.5

expr {1.0 / 0}  ;# ==> Inf
expr {-1.0 / 0} ;# ==> -Inf
expr {inf}      ;# ==> Inf
expr {1 / 0}    ;# ==> "divide by zero" error; Inf not part of range of integer division
```


A maximal integer is not easy to find, as Tcl switches to unbounded integers when a 64-bit integer is about to roll over:

```Tcl
% format %lx -1      ;# all bits set
ffffffffffffffff

% regsub f 0x[format %lx -1] 7 ;# unset the sign bit for positive
0x7fffffffffffffff

% set ii [expr [regsub f 0x[format %lx -1] 7]] ;# show as decimal
9223372036854775807

% incr ii
9223372036854775808 ;# silently upgrade to unbounded integer, still positive
```

A theoretical MAXINT, though very impractical, could be
 string repeat 9 [expr 2**32-1]

=={{header|TI-89 BASIC}}==


```ti89b
∞
```



## TorqueScript


```TorqueScript
function infinity()
{
    return 1/0;
}
```



## Trith

The following functions are included as part of the core operators:

```trith

: inf 1.0 0.0 / ;
: -inf inf neg ;
: inf? abs inf = ;

```



## Ursa

Infinity is a defined value in Ursa.

```ursa
decl double d
set d Infinity
```



## Ursala


IEEE double precision floating point numbers are a primitive type in Ursala.
This function returns IEEE double precision infinity when applied to any argument,
using the value inf, which is declared as a constant in the flo library.



```Ursala
#import flo

infinity = inf!
```



## Visual Basic

{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|Access 97}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}
Positive infinity, negative infinity and indefinite number (usable as NaN) can be generated by deliberately dividing by zero under the influence of <code>On Error Resume Next</code>:


```vb
Option Explicit

Private Declare Sub GetMem8 Lib "msvbvm60.dll" _
  (ByVal SrcAddr As Long, ByVal TarAddr As Long)

Sub Main()
Dim PlusInfinity As Double
Dim MinusInfinity As Double
Dim IndefiniteNumber As Double
    On Error Resume Next
    PlusInfinity = 1 / 0
    MinusInfinity = -1 / 0
    IndefiniteNumber = 0 / 0
    Debug.Print "PlusInfinity     = " & CStr(PlusInfinity) _
      & "  (" & DoubleAsHex(PlusInfinity) & ")"
    Debug.Print "MinusInfinity    = " & CStr(MinusInfinity) _
      & " (" & DoubleAsHex(MinusInfinity) & ")"
    Debug.Print "IndefiniteNumber = " & CStr(IndefiniteNumber) _
      & " (" & DoubleAsHex(IndefiniteNumber) & ")"
End Sub

Function DoubleAsHex(ByVal d As Double) As String
Dim l(0 To 1) As Long
GetMem8 VarPtr(d), VarPtr(l(0))
DoubleAsHex = Right$(String$(8, "0") & Hex$(l(1)), 8) _
            & Right$(String$(8, "0") & Hex$(l(0)), 8)
End Function
```

{{out}}
```txt
PlusInfinity     = 1,#INF  (7FF0000000000000)
MinusInfinity    = -1,#INF (FFF0000000000000)
IndefiniteNumber = -1,#IND (FFF8000000000000)

```



## Yabasic


```Yabasic
infinity = 1e300*1e300
if str$(infinity) = "inf" print "Infinity"
```



## zkl

zkl doesn't like INF, NaN, etc but sorta knows about them:

```zkl>1.5/0</lang

{{out}}

```txt

Exception thrown: MathError(INF (number is infinite))

```



## ZX Spectrum Basic

ZX Spectrum BASIC has no infinity handling;
```zxbasic>PRINT 1/0
```
 will be met with <pre
6 Number too big, 0:1
```

A quick doubling loop will get you halfway to the maximum floating point value:

```zxbasic
10 LET z=1
20 PRINT z
30 LET z=z*2
40 GO TO 20
```


Output will end with:

```txt

4.2535296E+37
8.5070592E+37

6 Number too big, 30:1

```

Precision has been lost by this stage through the loop, but one more manual double and subtract 1 will get you the true displayable maximum of 1.7014118E+38 (or 2^127-1).

{{omit from|bc|No infinity. Numbers have unlimited precision, so no largest possible value.}}
{{omit from|Brainfuck}}
{{omit from|dc|No infinity. Numbers have unlimited precision, so no largest possible value.}}
{{omit from|Integer BASIC}}
{{omit from|Retro|No floating point in standard VM}}
{{omit from|sed|Only has strings, not numbers.}}
{{omit from|VBScript}}
{{omit from|UNIX Shell}}

[[Category:Irrational numbers]]
