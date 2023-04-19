+++
title = "Zero to the zero power"
description = ""
date = 2019-10-12T09:14:46Z
aliases = []
[extra]
id = 17401
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Simple]]

Some computer programming languages are not exactly consistent   (with other computer programming languages)

when   ''raising zero to the zeroth power'':     <b><big>0<sup>0</sup></big></b>


;Task:
Show the results of raising   zero   to the   zeroth   power.


If your computer language objects to     <big> '''0**0''' </big>     or     <big> '''0^0''' </big>     at compile time,   you may also try something like:
            x = 0
            y = 0
            z = x**y
            say  'z='  z


'''Show the result here.'''

And of course use any symbols or notation that is supported in your computer programming language for exponentiation.


;See also:
* The Wiki entry: [[wp:Exponentiation#Zero_to_the_power_of_zero|Zero to the power of zero]].
* The Wiki entry: [[wp:Exponentiation#History_of_differing_points_of_view|History of differing points of view]].
* The MathWorld™ entry: [http://mathworld.wolfram.com/ExponentLaws.html exponent laws].
** Also, in the above MathWorld™ entry, see formula ('''9'''): <math>x^0=1</math>.
* The OEIS entry: [https://oeis.org/wiki/The_special_case_of_zero_to_the_zeroth_power The special case of zero to the zeroth power]





## 8th


```forth

0 0 ^ .

```

{{out}}
1

## ARM Assembly

{{omit from|ARM Assembly}}


## AutoHotkey


```AutoHotkey
MsgBox % 0 ** 0
```

{{out}}

```txt
1
```



## Ada


```Ada
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Long_Integer_Text_IO,
  Ada.Long_Long_Integer_Text_IO, Ada.Float_Text_IO, Ada.Long_Float_Text_IO,
  Ada.Long_Long_Float_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO, Ada.Long_Integer_Text_IO,
  Ada.Long_Long_Integer_Text_IO, Ada.Float_Text_IO, Ada.Long_Float_Text_IO,
  Ada.Long_Long_Float_Text_IO;

procedure Test5 is

   I    : Integer           := 0;
   LI   : Long_Integer      := 0;
   LLI  : Long_Long_Integer := 0;
   F    : Float             := 0.0;
   LF   : Long_Float        := 0.0;
   LLF  : Long_Long_Float   := 0.0;
   Zero : Natural           := 0;

begin
   Put ("Integer           0^0 = ");
   Put (I ** Zero, 2);   New_Line;
   Put ("Long Integer      0^0 = ");
   Put (LI ** Zero, 2);  New_Line;
   Put ("Long Long Integer 0^0 = ");
   Put (LLI ** Zero, 2); New_Line;
   Put ("Float           0.0^0 = ");
   Put (F ** Zero);   New_Line;
   Put ("Long Float      0.0^0 = ");
   Put (LF ** Zero);  New_Line;
   Put ("Long Long Float 0.0^0 = ");
   Put (LLF ** Zero); New_Line;
end Test5;

```

{{out}}

```txt
Integer           0^0 =  1
Long Integer      0^0 =  1
Long Long Integer 0^0 =  1
Float           0.0^0 =  1.00000E+00
Long Float      0.0^0 =  1.00000000000000E+00
Long Long Float 0.0^0 =  1.00000000000000000E+00

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.6.win32}}

```algol68
print( ( 0 ^ 0, newline ) )

```

{{out}}

```txt

         +1

```



## APL


```apl
      0*0
1
```



## Applesoft BASIC


```txt
]? 0^0
1
```



## AWK


```AWK

# syntax: GAWK -f ZERO_TO_THE_ZERO_POWER.AWK
BEGIN {
    print(0 ^ 0)
    exit(0)
}

```

{{out}}

```txt

1

```



## BaCon


```freebasic
PRINT POW(0, 0)
```


{{out}}

```txt
prompt$ ./zerotothezero
1
```



## Bc


```Bc

0 ^ 0

```

{{out}}
1


## Befunge

'''Befunge-93''' doesn't have explicit support for exponentiation, but there are a couple of fingerprint extensions for '''Befunge-98''' which add that functionality. The example below makes use of the '''FPDP''' fingerprint (double precision floating point).

Note that the result is potentially dependent on the underlying language of the interpreter, but all those tested so far have returned 1. Interpreters that don't support '''Befunge-98''', or don't support this fingerprint, should just terminate (possibly with a warning).


```befunge
"PDPF"4#@(0F0FYP)@
```


{{out}}

```txt
1.000000
```



## Bracmat


```bracmat
0^0
```

{{out}}

```txt
1
```



## Burlesque


```blsq

blsq ) 0.0 0.0?^
1.0
blsq ) 0 0?^
1

```



## BBC BASIC


```bbcbasic
      PRINT 0^0
```


{{out}}

```txt

1

```



## C

{{works with|C99}}
This example uses the standard <code>pow</code> function in the math library.
0^0 is given as 1.

```c
#include <stdio.h>
#include <math.h>
#include <complex.h>

int main()
{
	printf("0 ^ 0 = %f\n", pow(0,0));
        double complex c = cpow(0,0);
	printf("0+0i ^ 0+0i = %f+%fi\n", creal(c), cimag(c));
	return 0;
}
```


{{out}}

```txt

0 ^ 0 = 1.000000
0+0i ^ 0+0i = nan+nani

```



## C++


```cpp
#include <iostream>
#include <cmath>
#include <complex>

int main()
{
  std::cout << "0 ^ 0 = " << std::pow(0,0) << std::endl;
  std::cout << "0+0i ^ 0+0i = " <<
    std::pow(std::complex<double>(0),std::complex<double>(0)) << std::endl;
  return 0;
}
```


{{out}}

```txt

0 ^ 0 = 1
0+0i ^ 0+0i = (nan,nan)

```


## C#

```c#
using System;

namespace ZeroToTheZeroeth
{
    class Program
    {
        static void Main(string[] args)
        {
            double k = Math.Pow(0, 0);
            Console.Write("0^0 is {0}", k);
        }
    }
}
```


{{out}}

```txt

0^0 is 1

```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>ZEROPOW
  // default behavior is incorrect:
  set (x,y) = 0
  w !,"0 to the 0th power (wrong): "_(x**y)  ; will output 0

  // if one or both of the values is a double, this works
  set (x,y) = $DOUBLE(0)
  w !,"0 to the 0th power (right): "_(x**y)

  quit
```


{{out}}
```txt
SAMPLES>do ^ZEROPOW

0 to the 0th power (wrong): 0
0 to the 0th power (right): 1
```



## Clojure


```txt

user=> (use 'clojure.math.numeric-tower)
user=> (expt 0 0)
1

; alternative java-interop route:
user=> (Math/pow 0 0)
1.0

```



## COBOL


```cobol
identification division.
program-id. zero-power-zero-program.
data division.
working-storage section.
77  n                         pic 9.
procedure division.
    compute n = 0**0.
    display n upon console.
    stop run.
```

{{out}}

```txt
1
```



## ColdFusion


###  Classic tag based CFML


```cfm

<cfset zeroPowerTag = 0^0>
<cfoutput>"#zeroPowerTag#"</cfoutput>

```

{{Output}}

```txt

"1"

```



###  Script Based CFML


```cfm><cfscript

  zeroPower = 0^0;
  writeOutput( zeroPower );
</cfscript>
```

{{Output}}

```txt

1

```



## Common Lisp


```txt
> (expt 0 0)
1
```



## D


```d
void main() {
    import std.stdio, std.math, std.bigint, std.complex;

    writeln("Int:     ", 0 ^^ 0);
    writeln("Ulong:   ", 0UL ^^ 0UL);
    writeln("Float:   ", 0.0f ^^ 0.0f);
    writeln("Double:  ", 0.0 ^^ 0.0);
    writeln("Real:    ", 0.0L ^^ 0.0L);
    writeln("pow:     ", pow(0, 0));
    writeln("BigInt:  ", 0.BigInt ^^ 0);
    writeln("Complex: ", complex(0.0, 0.0) ^^ 0);
}
```

{{out}}

```txt
Int:     1
Ulong:   1
Float:   1
Double:  1
Real:    1
pow:     1
BigInt:  1
Complex: 1+0i
```



## Dc


```dc
0 0^p

```

{{Output}}

```txt

1

```



## EchoLisp


```scheme

;; trying the 16 combinations
;; all return the integer 1

(lib 'bigint)
(define zeroes '(integer: 0 inexact=float: 0.000 complex: 0+0i bignum: #0))
(for* ((z1 zeroes) (z2 zeroes)) (write (expt z1 z2)))
    →  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

```



## Eiffel


```Eiffel
print (0^0)
```

{{out}}

```txt
1
```


## Elena

ELENA 4.x

```elena
import extensions;

public program()
{
    console.printLine("0^0 is ",0.power:0)
}
```

{{out}}

```txt

0^0 is 0

```



## Elixir

Elixir uses Erlang's <code>:math</code> for power operations and can handle zero to the zero power.

```Elixir

:math.pow(0,0)

```


{{out}}
1.0


## ERRE


```ERRE

.....
PRINT(0^0)
.....

```

{{out}}

```txt
 1

```


=={{header|F_Sharp|F#}}==
In the REPL:

```txt
> let z = 0.**0.;;

val z : float = 1.0
```



## Factor


```factor
USING: math.functions.private ; ! ^complex
0 0 ^
C{ 0 0 } C{ 0 0 } ^complex
```

{{out}}

```txt
--- Data stack:
NAN: 8000000000000
C{ NAN: 8000000000000 NAN: 8000000000000 }
```




## Falcon

'''VBA/Python programmer's approach not sure if it's the most falconic way'''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

x = 0
y = 0
z = x**y
> "z=", z


```

{{out}}

```txt

z=1
[Finished in 0.2s]

```



## Forth


```forth
0e 0e f** f.
```


{{out}}

```txt
1.
```


Of course in an embedded program we would be tempted to "pre-calculate" the answer :-)


```Forth
: ^0     DROP  1 ;
```


{{Output}}

```txt

0 ^0 . 1 ok

```



## Fortran



```Fortran

program zero
double precision :: i, j
double complex :: z1, z2
i = 0.0D0
j = 0.0D0
z1 = (0.0D0,0.0D0)
z2 = (0.0D0,0.0D0)
write(*,*) 'When integers are used, we have 0^0 = ', 0**0
write(*,*) 'When double precision numbers are used, we have 0.0^0.0 = ', i**j
write(*,*) 'When complex numbers are used, we have (0.0+0.0i)^(0.0+0.0i) = ', z1**z2
end program

```

{{out}}

```txt

 When integers are used, we have 0^0 =            1
 When double precision numbers are used, we have 0.0^0.0 =    1.0000000000000000
 When complex numbers are used, we have (0.0+0.0i)^(0.0+0.0i) =  (             NaN,             NaN)

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Print "0 ^ 0 ="; 0 ^ 0
Sleep
```


{{out}}

```txt

0 ^ 0 = 1

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=7d505dbe89227e9b4423f92ef12d6829 Click this link to run this code]'''

```gambas
Public Sub Main()

Print 0 ^ 0

End
```

Output:

```txt

1

```



## Go

Go does not have an exponentiation operator but has functions in the standard library for three types, float64, complex128, and big.Int.
As of Go 1.3, all are documented to return 1.

```go
package main

import (
    "fmt"
    "math"
    "math/big"
    "math/cmplx"
)

func main() {
    fmt.Println("float64:    ", math.Pow(0, 0))
    var b big.Int
    fmt.Println("big integer:", b.Exp(&b, &b, nil))
    fmt.Println("complex:    ", cmplx.Pow(0, 0))
}
```

{{out}}

```txt

float64:     1
big integer: 1
complex:     (1+0i)

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

print 0^0

```

Output:

```txt

1

```



## Groovy

{{trans|Java}}
Test:

```groovy
println 0**0
```

{{out}}

```txt
1
```



## Haskell


```haskell
import Data.Complex

main = do
  print $ 0 ^ 0
  print $ 0.0 ^ 0
  print $ 0 ^^ 0
  print $ 0 ** 0
  print $ (0 :+ 0) ^ 0
  print $ (0 :+ 0) ** (0 :+ 0)
```

{{out}}

```txt

1
1.0
1.0
1.0
1.0 :+ 0.0
NaN :+ NaN

```



## HolyC


```holyc
F64 a = 0 ` 0;
Print("0 ` 0 = %5.3f\n", a);
```


{{out}}

```txt

0 ` 0 = 1.000

```


=={{header|Icon}} and {{header|Unicon}}==

"Works" in both languages:

```unicon
procedure main()
    write(0^0)
end
```


{{out}}

```txt

->z2z

Run-time error 204
File z2z.icn; Line 2
real overflow, underflow, or division by zero
Traceback:
   main()
   {0 ^ 0} from line 2 in z2z.icn
->

```



## J


```j
   0 ^ 0
1
```



## Java


```java
System.out.println(Math.pow(0, 0));
```

{{out}}

```txt
1.0
```



## JavaScript


### Math.pow

{{Works with|Node.js}}
In interactive mode:

```javascript>
 Math.pow(0, 0);
1
```

===exponentiation operator (**)===

```javascript>
 0**0
1
```



## jq

jq version 1.4 does not have a builtin "power" function. If it were to be defined
using the exp and log builtins as 'log * y | exp', then 0 | power(0) would yield null, and therefore
a definition that makes a special case of 0^0 should be considered, e.g.
along the following lines:

```jq
def power(y): y as $y | if $y == 0 then 1 elif . == 0 then 0 else log * $y | exp end;
```


This definition will however be unsatisfactory for many purposes
because it does not maintain precision for integer values of the input (.) and y.


## Jsish


```javascript
puts(Math.pow(0,0));
```

{{out}}

```txt
1
```



## Julia

Try all combinations of complex, float, rational, integer and boolean.

```Julia
const types = (Complex, Float64, Rational, Int, Bool)

for Tb in types, Te in types
    zb, ze = zero(Tb), zero(Te)
    r = zb ^ ze
    @printf("%10s ^ %-10s = %7s ^ %-7s = %-12s (%s)\n", Tb, Te, zb, ze, r, typeof(r))
end
```


{{out}}

```txt
   Complex ^ Complex    = 0 + 0im ^ 0 + 0im = 1.0 + 0.0im  (Complex{Float64})
   Complex ^ Float64    = 0 + 0im ^ 0.0     = 1.0 + 0.0im  (Complex{Float64})
   Complex ^ Rational   = 0 + 0im ^ 0//1    = 1.0 + 0.0im  (Complex{Float64})
   Complex ^ Int64      = 0 + 0im ^ 0       = 1 + 0im      (Complex{Int64})
   Complex ^ Bool       = 0 + 0im ^ false   = 1 + 0im      (Complex{Int64})
   Float64 ^ Complex    =     0.0 ^ 0 + 0im = 1.0 + 0.0im  (Complex{Float64})
   Float64 ^ Float64    =     0.0 ^ 0.0     = 1.0          (Float64)
   Float64 ^ Rational   =     0.0 ^ 0//1    = 1.0          (Float64)
   Float64 ^ Int64      =     0.0 ^ 0       = 1.0          (Float64)
   Float64 ^ Bool       =     0.0 ^ false   = 1.0          (Float64)
  Rational ^ Complex    =    0//1 ^ 0 + 0im = 1.0 + 0.0im  (Complex{Float64})
  Rational ^ Float64    =    0//1 ^ 0.0     = 1.0          (Float64)
  Rational ^ Rational   =    0//1 ^ 0//1    = 1.0          (Float64)
  Rational ^ Int64      =    0//1 ^ 0       = 1//1         (Rational{Int64})
  Rational ^ Bool       =    0//1 ^ false   = 1//1         (Rational{Int64})
     Int64 ^ Complex    =       0 ^ 0 + 0im = 1.0 + 0.0im  (Complex{Float64})
     Int64 ^ Float64    =       0 ^ 0.0     = 1.0          (Float64)
     Int64 ^ Rational   =       0 ^ 0//1    = 1.0          (Float64)
     Int64 ^ Int64      =       0 ^ 0       = 1            (Int64)
     Int64 ^ Bool       =       0 ^ false   = 1            (Int64)
      Bool ^ Complex    =   false ^ 0 + 0im = 1.0 + 0.0im  (Complex{Float64})
      Bool ^ Float64    =   false ^ 0.0     = 1.0          (Float64)
      Bool ^ Rational   =   false ^ 0//1    = 1.0          (Float64)
      Bool ^ Int64      =   false ^ 0       = true         (Bool)
      Bool ^ Bool       =   false ^ false   = true         (Bool)
```



## K


```K

  0^0
1.0

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
   println("0 ^ 0 = ${Math.pow(0.0, 0.0)}")
}
```


{{out}}

```txt

0 ^ 0 = 1.0

```



## Lua

No need to try different data types or with / without decimal points as all numbers in Lua are stored in double-precision floating-point format.

```Lua
print(0^0)
```

{{out}}

```txt
1
```


## M2000 Interpreter

M2000 use ** and ^ for power.

```M2000 Interpreter

Module Checkit {
      x=0
      y=0
      Print x**y=1, x^y=1    ' True True
}
Checkit

```




## Maple


```Maple
0^0
```

{{out}}

```txt
1
```


However, for consistency with IEEE-754 numerics, we also have a NaN result for the equivalent floating-point exponentiation:

```Maple
0^0.0
```

{{out}}

```txt
Float(undefined)
```



## Mathematica


```Mathematica
0^0
```

{{out}}

```txt
Indeterminate
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
0^0
complex(0,0)^0
```

{{out}}

```txt
1
1
```



## Mercury


```Mercury
:- module zero_to_the_zero_power.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float, int, integer, list, string.

main(!IO) :-
   io.format("    int.pow(0, 0) = %d\n", [i(pow(0, 0))], !IO),
   io.format("integer.pow(zero, zero) = %s\n",
        [s(to_string(pow(zero, zero)))], !IO),
   io.format("  float.pow(0.0, 0) = %.1f\n", [f(pow(0.0, 0))], !IO).

:- end_module zero_to_the_zero_power.
```

{{out}}

```txt
    int.pow(0, 0) = 1
integer.pow(zero, zero) = 1
  float.pow(0.0, 0) = 1.0
```



## Microsoft Small Basic


```smallbasic
TextWindow.WriteLine(Math.Power(0,0))
```

{{out}}
```txt
1
```



## min

{{works with|min|0.19.3}}

```min>0 0 pow puts</lang

{{out}}

```txt

1.0

```



## MiniScript


```MiniScript
print "The result of zero to the zero power is " + 0^0
```

{{out}}

```txt

The result of zero to the zero power is 1

```


=={{header|MK-61/52}}==
<lang>Сx	^	x^y	С/П
```


The result is error message.


## Neko

Neko uses the C math library for exponentiation, Zero to the zero in math.pow(x, y) is treated as being 1.


```ActionScript
/**
 Zero to the zeroth power, in Neko
*/

var math_pow = $loader.loadprim("std@math_pow", 2)

$print(math_pow(0, 0), "\n")
```


{{out}}

```txt
prompt$ nekoc zero-to-the-zero.neko
prompt$ neko zero-to-the-zero.n
1
```



## NetRexx


```netrexx
x=0
Say '0**0='||x**x
```

{{out}}

```txt
0**0=1
```



## NewLISP


```newlisp
(pow 0 0)
```

{{out}}

```txt
1
```



## Nial


Create an exponentiation table for all type combinations (of integer <code>0</code>, float <code>0.0</code> and boolean <code>o</code>):


```nial
     0 0.0 o outer power 0 0.0 o
+--+--+--+
| 1|1.| 1|
+--+--+--+
|1.|1.|1.|
+--+--+--+
| 1|1.| 1|
+--+--+--+
```



## Nim


```nim
import math

echo pow(0, 0)
```

{{out}}

```txt
1.0
```



## OCaml

In the interpreter:

```txt

# 0.0 ** 0.0;;
- : float = 1.
# Complex.pow Complex.zero Complex.zero;;
- : Complex.t = {Complex.re = nan; Complex.im = nan}
# #load "nums.cma";;
# open Num;;
# Int 0 **/ Int 0;;
- : Num.num = Int 1

```



## Oforth



```Oforth>0 0 pow println</lang


{{out}}

```txt

1

```



## Ol


```scheme

(print "0^0: " (expt 0 0))
(print "0.0^0: " (expt (inexact 0) 0))

```

{{out}}

```txt

0^0: 1
0.0^0: 1

```



## ooRexx


```oorexx
/**********************************************************************
* 21.04.2014 Walter Pachl
**********************************************************************/
Say 'rxCalcpower(0,0)  ->' rxCalcpower(0,0)
Say '0**0              ->' 0**0
::requires rxmath library
```

{{out}}

```txt

rxCalcpower(0,0)  -> 1
0**0              -> 1

```



## PARI/GP

0 raised to the power of exact 0 is 0, but 0 cannot be raised to the power of an inexact 0:

```parigp
0^0
0.^0
0^0.
```

{{out}}

```txt
%1 = 1
%2 = 1
  ***   at top-level: 0^0.
  ***                   ^---
  *** _^_: domain error in gpow(0,n): n <= 0
  ***   Break loop: type 'break' to go back to GP prompt
```



## Pascal

{{works with|Free Pascal}} {{Libheader|math}}

```Pascal
program ZToZ;
uses
  math;
begin
  write('0.0 ^ 0 :',IntPower(0.0,0):4:2);
  writeln('   0.0 ^ 0.0 :',Power(0.0,0.0):4:2);
end.
```

;output:

```txt
0.0 ^ 0 :1.00   0.0 ^ 0.0 :1.00
```



## Perl


```perl
print 0 ** 0, "\n";

use Math::Complex;

print cplx(0,0) ** cplx(0,0), "\n";
```

{{out}}

```txt

1
1

```



## Perl 6


{{works with|Rakudo|2018.03}}

```perl6
say '    type         n      n**n  exp(n,n)';
say '--------  --------  --------  --------';

for 0, 0.0, FatRat.new(0), 0e0, 0+0i {
    printf "%8s  %8s  %8s  %8s\n", .^name, $_, $_**$_, exp($_,$_);
}
```


{{out}}

```txt

    type         n      n**n  exp(n,n)
--------  --------  --------  --------
     Int         0         1         1
     Rat         0         1         1
  FatRat         0         1         1
     Num         0         1         1
 Complex      0+0i      1+0i      1+0i

```



## Phix

Fair enough, I have no strong opinions on this matter, so I have just removed the test/error that was present in previous versions. Should you for any reason want to change it back, just edit builtins/VM/pPower.e, search for the two mods dated 3/11/15 (32 and 64 bit, both are two lines, test eax/rax; jz :e102cr0tple0), save and rebuild (run "p -c p"), which should take less than 10 seconds.

```Phix
?power(0,0)
```

{{out}}

```txt
1
```



## PHP


```PHP
<?php
echo pow(0,0);
echo 0 ** 0; // PHP 5.6+ only
?>
```

{{out}}

```txt

1
1

```



## PicoLisp


```PicoLisp

(** 0 0)

```

{{out}}
1

## PL/I


```pli
 zhz: Proc Options(Main);
 Dcl a dec float(10) Init(1);
 Dcl b dec float(10) Init(0);
 Put skip list('1**0=',a**b);
 Put skip list('0**1=',b**a);
 Put skip list('0**0=',b**b);
 End;
```

{{out}}

```txt

1**0=                    1.000000000E+0000
0**1=                    0.000000000E+0000
0**0=
IBM0682I  ONCODE=1553  X in EXPONENT(X) was invalid.
   At offset +0000025B in procedure with entry ZHZ

```



## PowerShell



```powershell
Write-Host "0 ^ 0 = " ([math]::pow(0,0))
```


Output :


```txt

0 ^ 0 =  1

```



## PureBasic


```PureBasic

If OpenConsole()
  PrintN("Zero to the zero power is " + Pow(0,0))
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out}}

```txt

Zero to the zero power is 1

```




## Pyret


```Pyret
num-expt(0, 0)
```

{{out}}
1


## Python


### Python3


```python
from decimal import Decimal
from fractions import Fraction
from itertools import product

zeroes = [0, 0.0, 0j, Decimal(0), Fraction(0, 1), -0.0, -0.0j, Decimal(-0.0)]
for i, j in product(zeroes, repeat=2):
    try:
        ans = i**j
    except:
        ans = '<Exception raised>'
    print(f'{i!r:>15} ** {j!r:<15} = {ans!r}')
```

{{out}}

```txt
              0 ** 0               = 1
              0 ** 0.0             = 1.0
              0 ** 0j              = (1+0j)
              0 ** Decimal('0')    = '<Exception raised>'
              0 ** Fraction(0, 1)  = 1
              0 ** -0.0            = 1.0
              0 ** (-0-0j)         = (1+0j)
              0 ** Decimal('-0')   = '<Exception raised>'
            0.0 ** 0               = 1.0
            0.0 ** 0.0             = 1.0
            0.0 ** 0j              = (1+0j)
            0.0 ** Decimal('0')    = '<Exception raised>'
            0.0 ** Fraction(0, 1)  = 1.0
            0.0 ** -0.0            = 1.0
            0.0 ** (-0-0j)         = (1+0j)
            0.0 ** Decimal('-0')   = '<Exception raised>'
             0j ** 0               = (1+0j)
             0j ** 0.0             = (1+0j)
             0j ** 0j              = (1+0j)
             0j ** Decimal('0')    = '<Exception raised>'
             0j ** Fraction(0, 1)  = (1+0j)
             0j ** -0.0            = (1+0j)
             0j ** (-0-0j)         = (1+0j)
             0j ** Decimal('-0')   = '<Exception raised>'
   Decimal('0') ** 0               = '<Exception raised>'
   Decimal('0') ** 0.0             = '<Exception raised>'
   Decimal('0') ** 0j              = '<Exception raised>'
   Decimal('0') ** Decimal('0')    = '<Exception raised>'
   Decimal('0') ** Fraction(0, 1)  = '<Exception raised>'
   Decimal('0') ** -0.0            = '<Exception raised>'
   Decimal('0') ** (-0-0j)         = '<Exception raised>'
   Decimal('0') ** Decimal('-0')   = '<Exception raised>'
 Fraction(0, 1) ** 0               = Fraction(1, 1)
 Fraction(0, 1) ** 0.0             = 1.0
 Fraction(0, 1) ** 0j              = (1+0j)
 Fraction(0, 1) ** Decimal('0')    = '<Exception raised>'
 Fraction(0, 1) ** Fraction(0, 1)  = Fraction(1, 1)
 Fraction(0, 1) ** -0.0            = 1.0
 Fraction(0, 1) ** (-0-0j)         = (1+0j)
 Fraction(0, 1) ** Decimal('-0')   = '<Exception raised>'
           -0.0 ** 0               = 1.0
           -0.0 ** 0.0             = 1.0
           -0.0 ** 0j              = (1+0j)
           -0.0 ** Decimal('0')    = '<Exception raised>'
           -0.0 ** Fraction(0, 1)  = 1.0
           -0.0 ** -0.0            = 1.0
           -0.0 ** (-0-0j)         = (1+0j)
           -0.0 ** Decimal('-0')   = '<Exception raised>'
        (-0-0j) ** 0               = (1+0j)
        (-0-0j) ** 0.0             = (1+0j)
        (-0-0j) ** 0j              = (1+0j)
        (-0-0j) ** Decimal('0')    = '<Exception raised>'
        (-0-0j) ** Fraction(0, 1)  = (1+0j)
        (-0-0j) ** -0.0            = (1+0j)
        (-0-0j) ** (-0-0j)         = (1+0j)
        (-0-0j) ** Decimal('-0')   = '<Exception raised>'
  Decimal('-0') ** 0               = '<Exception raised>'
  Decimal('-0') ** 0.0             = '<Exception raised>'
  Decimal('-0') ** 0j              = '<Exception raised>'
  Decimal('-0') ** Decimal('0')    = '<Exception raised>'
  Decimal('-0') ** Fraction(0, 1)  = '<Exception raised>'
  Decimal('-0') ** -0.0            = '<Exception raised>'
  Decimal('-0') ** (-0-0j)         = '<Exception raised>'
  Decimal('-0') ** Decimal('-0')   = '<Exception raised>'
```



### Python2


```python
from decimal import Decimal
from fractions import Fraction
for n in (Decimal(0), Fraction(0, 1), complex(0), float(0), int(0)):
	try:
		n1 = n**n
	except:
		n1 = '<Raised exception>'
	try:
		n2 = pow(n, n)
	except:
		n2 = '<Raised exception>'
	print('%8s: ** -> %r; pow -> %r' % (n.__class__.__name__, n1, n2))
```

{{out}}

```txt

 Decimal: ** -> '<Raised exception>'; pow -> '<Raised exception>'
Fraction: ** -> Fraction(1, 1); pow -> Fraction(1, 1)
 complex: ** -> (1+0j); pow -> (1+0j)
   float: ** -> 1.0; pow -> 1.0
     int: ** -> 1; pow -> 1

```



## R


```rsplus
print(0^0)
```

{{out}}

```txt
1
```



## Racket



```racket
#lang racket
;; as many zeros as I can think of...
(define zeros (list
               0  ; unspecified number type
               0. ; hinted as float
               #e0 ; explicitly exact
               #i0 ; explicitly inexact
               0+0i ; exact complex
               0.+0.i ; float inexact
               ))
(for*((z zeros) (p zeros))
  (printf "(~a)^(~a) = ~s~%" z p
  (with-handlers [(exn:fail:contract:divide-by-zero? exn-message)]
    (expt z p))))
```


{{out}}

```txt
(0)^(0) = 1
(0)^(0.0) = 1.0
(0)^(0) = 1
(0)^(0.0) = 1.0
(0)^(0) = 1
(0)^(0.0+0.0i) = "expt: undefined for 0 and 0.0+0.0i"
(0.0)^(0) = 1
(0.0)^(0.0) = 1.0
(0.0)^(0) = 1
(0.0)^(0.0) = 1.0
(0.0)^(0) = 1
(0.0)^(0.0+0.0i) = +nan.0+nan.0i
(0)^(0) = 1
(0)^(0.0) = 1.0
(0)^(0) = 1
(0)^(0.0) = 1.0
(0)^(0) = 1
(0)^(0.0+0.0i) = "expt: undefined for 0 and 0.0+0.0i"
(0.0)^(0) = 1
(0.0)^(0.0) = 1.0
(0.0)^(0) = 1
(0.0)^(0.0) = 1.0
(0.0)^(0) = 1
(0.0)^(0.0+0.0i) = +nan.0+nan.0i
(0)^(0) = 1
(0)^(0.0) = 1.0
(0)^(0) = 1
(0)^(0.0) = 1.0
(0)^(0) = 1
(0)^(0.0+0.0i) = "expt: undefined for 0 and 0.0+0.0i"
(0.0+0.0i)^(0) = 1
(0.0+0.0i)^(0.0) = 1.0+0.0i
(0.0+0.0i)^(0) = 1
(0.0+0.0i)^(0.0) = 1.0+0.0i
(0.0+0.0i)^(0) = 1
(0.0+0.0i)^(0.0+0.0i) = +nan.0+nan.0i
```



## REXX


```rexx
/*REXX program shows the results of  raising zero  to the  zeroth power.*/
say  '0 ** 0  (zero to the zeroth power) ───► '    0**0
```


using PC/REXX

using Personal REXX

using REGINA

using ooRexx
{{out}}

```txt

0 ** 0  (zero to the zeroth power) ───►  1

```


using R4
{{out}}

```txt

Error 26 : Invalid whole number (SYNTAX)
Information: 0 ** 0 is undefined
Error occurred in statement# 2
Statement source: say '0 ** 0  (zero to the zeroth power) ───► ' 0**0
Statement context: C:\ZERO_TO0.REX, procedure: ZERO_TO0

```


using ROO
{{out}}

```txt

Error 26 : Invalid whole number (SYNTAX)
Information: 0 ** 0 is undefined
Error occurred in statement# 2
Statement source: say '0 ** 0  (zero to the zeroth power) ───► ' 0**0
Statement context: C:\ZERO_TO0.REX, procedure: ZERO_TO0

```



## Ring


```ring

x = 0
y = 0
z = pow(x,y)
see "z=" + z + nl   # z=1

```



## Ruby


```ruby
require 'bigdecimal'

[0, 0.0, Complex(0), Rational(0), BigDecimal.new("0")].each do |n|
  printf "%10s: ** -> %s\n" % [n.class, n**n]
end
```

{{out}}

```txt

    Fixnum: ** -> 1
     Float: ** -> 1.0
   Complex: ** -> 1+0i
  Rational: ** -> 1/1
BigDecimal: ** -> 0.1E1

```



## Rust


```rust
fn main() {
    println!("{}",0u32.pow(0));
}
```


{{out}}

```txt
1
```



=={{header|S-lang}}==
<lang S-lang>print(0^0);
```

{{out}}

```txt
1.0
```



## Scala

{{libheader|Scala}}
```Scala
  assert(math.pow(0, 0) == 1, "Scala blunder, should go back to school !")
```



## Scheme


```scheme
(display (expt 0 0)) (newline)
(display (expt 0.0 0.0)) (newline)
(display (expt 0+0i 0+0i)) (newline)
```

{{out}}

```txt
1
1.0
1.0
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "complex.s7i";

const proc: main is func
  begin
    writeln("0      ** 0   = " <& 0 ** 0);
    writeln("0.0    ** 0   = " <& 0.0 ** 0);
    writeln("0.0    ** 0.0 = " <& 0.0 ** 0.0);
    writeln("0.0+0i ** 0   = " <& complex(0.0) ** 0);
  end func;

```


{{out}}

```txt

0      ** 0   = 1
0.0    ** 0   = 1.0
0.0    ** 0.0 = 1.0
0.0+0i ** 0   = 1.0+0.0i

```



## Sidef


```ruby
[0, Complex(0, 0)].each {|n|
    say n**n
}
```

{{out}}

```txt

1
1

```


Taking the 0'th root of a number and raising it back to the zero power, we also get a 1:


```ruby
say 0.root(0).pow(0)       # => 1
say ((0**(1/0))**0)        # => 1
```



## Sinclair ZX81 BASIC


```basic
PRINT 0**0
```

{{out}}

```txt
1
```



## Smalltalk



```smalltalk

0 raisedTo: 0
0.0 raisedTo: 0.0

```

{{out}}

```txt

1
1.0

```




## smart BASIC


```qbasic
PRINT 0^0
```


{{out}}

```txt

1

```



## SQL



```SQL

SQL> select power(0,0) from dual;

```

{{out}}

```txt

POWER(0,0)
----------
         1

```



## Standard ML

In the interpreter:

```txt

- Math.pow (0.0, 0.0);
val it = 1.0 : real

```



## Stata


```stata
. display 0^0
1
```



## Swift


```swift
import Darwin
print(pow(0.0,0.0))
```

{{out}}

```txt
1.0
```



## Tcl

Interactively…

```tcl
% expr 0**0
1
% expr 0.0**0.0
1.0
```


=={{header|TI-83_BASIC}}==

```tibasic
0^0
```

{{out}}

```txt
ERROR:DOMAIN
```



## uBasic/4tH

<lang>Print 0^0
```

{{out}}

```txt
1

0 OK, 0:9
```



## Ursa

Cygnus/X Ursa is written in Java, and as a result returns 1.0 when raising 0 to the 0.

```ursa>
 out (pow 0 0) endl console
1.0
```



## VBA


```vb
Public Sub zero()
    x = 0
    y = 0
    z = 0 ^ 0
    Debug.Print "z ="; z
End Sub
```
{{out}}

```txt
z = 1
```


## VBScript


```vb
WScript.Echo 0 ^ 0
```

{{Out}}

```txt
1
```



## Visual Basic .NET


```vbnet
Module Program
    Sub Main()
        Console.Write(0^0)
    End Sub
End Module
```


{{out}}

```txt
1
```



## XLISP


```scheme
XLISP 3.3, September 6, 2002 Copyright (c) 1984-2002, by David Betz
[1] (expt 0 0)

1
[2]
```



## zkl


```zkl
(0.0).pow(0)  //--> 1.0
var BN=Import("zklBigNum"); // big ints
BN(0).pow(0) //--> 1
```

