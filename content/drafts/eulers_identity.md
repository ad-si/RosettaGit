+++
title = "Euler's identity"
description = ""
date = 2019-09-20T09:40:08Z
aliases = []
[extra]
id = 21791
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{Wikipedia|Euler's_identity}}


In mathematics,   ''Euler's identity''   (also known as   ''Euler's equation'')   is the equality:

                <span style="font-size:150%;font-style:bold;"><span style="font-style:italic">e<sup>i<math>\pi</math></sup></span> + 1 = 0</span>

where

    e is Euler's number, the base of natural logarithms,
    ''i'' is the imaginary unit, which satisfies ''i''<sup>2</sup> = −1, and
    <math>\pi</math> is pi, the ratio of the circumference of a circle to its diameter.

Euler's identity is often cited as an example of deep mathematical beauty. Three of the basic arithmetic operations occur exactly once each: addition, multiplication, and exponentiation. The identity also links five fundamental mathematical constants:

    The number 0.
    The number 1.
    The number <math>\pi</math> (<math>\pi</math> = 3.14159<small>+</small>),
    The number e (e = 2.71828<small>+</small>), which occurs widely in mathematical analysis.
    The number ''i'', the imaginary unit of the complex numbers.

;Task
Show in your language that Euler's identity is true. As much as possible and practical, mimic the Euler's identity equation. 

Most languages are limited to IEEE 754 floating point calculations so will have some error in the calculation. 

If that is the case, or there is some other limitation, show 
that   <big>e<sup>i<math>\pi</math></sup> + 1</big>   is ''approximately'' equal to zero and 
show the amount of error in the calculation.

If your language is capable of symbolic calculations, show 
that   <big>e<sup>i<math>\pi</math></sup> + 1</big>   is ''exactly'' equal to zero for bonus kudos points.





## ALGOL 68

Whilst Algol 68 has complex numbers as standard, it does not have a standard complex exp function.


We could use the identity exp(x + iy) = exp(x)( cos y + i sin y ), however the following uses a series expansion for exp(ix).

```algol68
BEGIN
    # calculate an approximation to e^(i pi) + 1 which should be 0 (Euler's identity) #

    # returns e^ix for long real x, using the series:                                 #
    #      exp(ix) = 1 - x^2/2! + x^4/4! - ... + i(x - x^3/3! + x^5/5! - x^7/7! ... ) #
    #      the expansion stops when successive terms differ by less than 1e-15        #
    PROC expi = ( LONG REAL x )LONG COMPL:
         BEGIN
            LONG REAL t              := 1;
            LONG REAL real part      := 1;
            LONG REAL imaginary part := 0;
            LONG REAL divisor        := 1;
            BOOL      even power     := FALSE;
            BOOL      subtract       := FALSE;
            LONG REAL diff           := 1;
            FOR n FROM 1 WHILE ABS diff > 1e-15 DO
                divisor *:= n;
                t       *:= x;
                LONG REAL term := t / divisor;
                IF even power THEN
                    # this term is real #
                    subtract := NOT subtract;
                    LONG REAL prev := real part;
                    IF subtract THEN
                        real part -:= term
                    ELSE
                        real part +:= term
                    FI;
                    diff := prev - real part
                ELSE
                    # this term is imaginary #
                    LONG REAL prev := imaginary part;
                    IF subtract THEN
                        imaginary part -:= term
                    ELSE
                        imaginary part +:= term
                    FI;
                    diff := prev - imaginary part
                FI;
                even power := NOT even power
            OD;
            ( real part, imaginary part )
         END # expi # ;
    LONG COMPL eulers identity = expi( long pi ) + 1;
    print( ( "e^(i*pi) + 1 ~ "
           , fixed( re OF eulers identity, -23, 20 )
           , " "
           , fixed( im OF eulers identity,  23, 20 )
           , "i"
           , newline
           )
         )
END
```

{{out}}

```txt

e^(i*pi) + 1 ~  0.00000000000000000307 -0.00000000000000002926i

```



## C

The C99  standard did, of course, introduce built-in support for complex number arithmetic into the language and so we can therefore compute (e ^ πi + 1) directly without having to resort to methods which sum the power series for e ^ x.

The following code has been tested with gcc 5.4.0 on Ubuntu 16.04.

```c>#include <stdio.h

#include <math.h>
#include <complex.h>
#include <wchar.h>
#include <locale.h>

int main() {
    wchar_t pi = L'\u03c0'; /* Small pi symbol */
    wchar_t ae = L'\u2245'; /* Approximately equals symbol */
    double complex e = cexp(M_PI * I) + 1.0;
    setlocale(LC_CTYPE, "");
    printf("e ^ %lci + 1 = [%.16f, %.16f] %lc 0\n", pi, creal(e), cimag(e), ae);
    return 0;
}
```


{{output}}

```txt

e ^ πi + 1 = [0.0000000000000000, 0.0000000000000001] ≅ 0

```



## C++


```cpp>#include <iostream

#include <complex>

int main() {
  std::cout << std::exp(std::complex<double>(0.0, M_PI)) + 1.0 << std::endl;
  return 0;
}
```


{{output}}
Zero and a little floating dust ...

```txt

(0,1.22465e-16)

```




## C sharp


```csharp
using System;
using System.Numerics;

public class Program
{
    static void Main() {
        Complex e = Math.E;
        Complex i = Complex.ImaginaryOne;
        Complex π = Math.PI;
        Console.WriteLine(Complex.Pow(e, i * π) + 1);
    }
}
```

{{out}}

```txt

(0, 1.22464679914735E-16)

```



## Common Lisp

Common Lisp has complex number arithmetic built into it.

```Common Lisp

(+ 1 (exp (complex 0 pi)))

```


```txt

#C(0.0L0 -5.0165576136843360246L-20)

```



## Factor


```factor
USING: math math.constants math.functions prettyprint ;
1 e pi C{ 0 1 } * ^ + .
```

{{out}}

```txt

C{ 0.0 1.224646799147353e-016 }

```



## Go


```go
package main
 
import (
    "fmt"
    "math"
    "math/cmplx"
)
 
func main() {
    fmt.Println(cmplx.Exp(math.Pi * 1i) + 1.0)
}
```


{{output}}
Zero and a little floating dust ...

```txt

(0+1.2246467991473515e-16i)

```



## Groovy

Because the Groovy language does not provide a built-in facility for complex arithmetic, this example relies on the Complex class defined in the [[Complex_numbers#Groovy|Complex numbers]] example.

```groovy
import static Complex.*

Number.metaClass.mixin ComplexCategory

def π = Math.PI
def e = Math.E

println "e ** (π * i) + 1 = " + (e ** (π * i) + 1)

println "| e ** (π * i) + 1 | = " + (e ** (π * i) + 1).ρ
```

Output: (yadda, yadda, dust, yadda)

```txt
e ** (π * i) + 1 = 1.2246467991473532E-16i
| e ** (π * i) + 1 | = 1.2246467991473532E-16
```



## Haskell


A double is not quite real.

```Haskell
import Data.Complex

eulerIdentityZeroIsh :: Complex Double
eulerIdentityZeroIsh =
  exp (0 :+ pi) + 1
  
main :: IO ()
main = print eulerIdentityZeroIsh
```

{{Out}}

Zero and a little floating dust ...

```txt
0.0 :+ 1.2246467991473532e-16
```



## J


```j

   NB. Euler's number is the default base for power
   NB. using j's expressive numeric notation:
   1 + ^ 0j1p1
0j1.22465e_16
   

   NB. Customize the comparison tolerance to 10 ^ (-15)
   NB. to show that
   _1 (=!.1e_15) ^ 0j1p1
1


   
   TAU =: 2p1

   NB. tauday.com  pi is wrong
   NB. with TAU as 2 pi,
   NB. Euler's identity should have read


   1 (=!.1e_15) ^ j. TAU
1

```



## jq

For speed and for conformance with the complex plane interpretation,
x+iy is represented as [x,y]; for flexibility, all the functions
defined here will accept both real and complex numbers, and for
uniformity, they are implemented as functions that ignore their input.

Recent versions of jq support modules, so these functions could all
be placed in a module to avoid name conflicts, and thus no special
prefix is used here.

```jq
def multiply(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x*y, 0 ]
       else [x * y[0], x * y[1]]
       end
    elif (y|type) == "number" then multiply(y;x)
    else [ x[0] * y[0] - x[1] * y[1],  x[0] * y[1] + x[1] * y[0]]
    end;

def plus(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x+y, 0 ]
       else [ x + y[0], y[1]]
       end
    elif (y|type) == "number" then plus(y;x)
    else [ x[0] + y[0], x[1] + y[1] ]
    end;

def exp(z):
  def expi(x): [ (x|cos), (x|sin) ];
  if (z|type) == "number" then z|exp
  elif z[0] == 0 then expi(z[1])  # for efficiency
  else multiply( (z[0]|exp); expi(z[1]) )
  end ;

def pi: 4 * (1|atan);

```



### The Task


```jq
"e^iπ:     \( exp( [0, pi ] ) )",
"e^iπ + 1: \( plus(1; exp( [0, pi ] ) ))"
```


{{out}}

```txt
e^iπ:     [-1,1.2246467991473532e-16]
e^iπ + 1: [0,1.2246467991473532e-16]
```



## Julia

{{works with|Julia|1.2}}
Julia has a builtin <tt>Complex{T}</tt> parametrized type.


```julia
@show ℯ^(π * im) + 1
@assert ℯ^(π * im) ≈ -1
```


{{out}}

```txt
e ^ (π * im) + 1 = 0.0 + 1.2246467991473532e-16im
```


Using symbolic algebra, through the [https://github.com/chakravala/Reduce.jl Reduce.jl] package.


```julia
using Reduce
@force using Reduce.Algebra

@show ℯ^(π * :i) + 1
@assert ℯ^(π * :i) + 1 == 0
```


{{out}}

```txt
ℯ^(π * :i) + 1 = 0
```



## Kotlin

As the JVM lacks a complex number class, we use our own which has sufficient operations to perform this task.

e ^ πi is calculated by summing successive terms of the power series for e ^ x until the modulus of the difference between terms is no longer significant given the precision of the Double type (about 10 ^ -16).

```scala
// Version 1.2.40

import kotlin.math.sqrt
import kotlin.math.PI

const val EPSILON = 1.0e-16
const val SMALL_PI = '\u03c0'
const val APPROX_EQUALS = '\u2245'

class Complex(val real: Double, val imag: Double) {
    operator fun plus(other: Complex) =
        Complex(real + other.real, imag + other.imag)

    operator fun times(other: Complex) = Complex(
        real * other.real - imag * other.imag,
        real * other.imag + imag * other.real
    )

    fun inv(): Complex {
        val denom = real * real + imag * imag
        return Complex(real / denom, -imag / denom)
    }

    operator fun unaryMinus() = Complex(-real, -imag)

    operator fun minus(other: Complex) = this + (-other)

    operator fun div(other: Complex) = this * other.inv()

    val modulus: Double get() = sqrt(real * real + imag * imag)

    override fun toString() =
        if (imag >= 0.0) "$real + ${imag}i"
        else "$real - ${-imag}i"
}

fun main(args: Array<String>) {
    var fact = 1.0
    val x = Complex(0.0, PI)
    var e = Complex(1.0, PI)
    var n = 2
    var pow = x
    do {
        val e0 = e
        fact *= n++
        pow *= x
        e += pow / Complex(fact, 0.0)
    }
    while ((e - e0).modulus >= EPSILON)
    e += Complex(1.0, 0.0)
    println("e^${SMALL_PI}i + 1 = $e $APPROX_EQUALS 0")
}
```


{{output}}

```txt

e^πi + 1 = -8.881784197001252E-16 - 9.714919754267985E-17i ≅ 0

```



## OCaml


```ocaml
# open Complex;;
# let pi = acos (-1.0);;
val pi : float = 3.14159265358979312
# add (exp { re = 0.0; im = pi }) { re = 1.0; im = 0.0 };;
- : Complex.t = {re = 0.; im = 1.22464679914735321e-16}
```



## Perl


```perl
use Math::Complex;
print exp(pi * i) + 1, "\n";
```

{{out}}

```txt
1.22464679914735e-16i
```



## Perl 6

{{works with|Rakudo|2018.03}}
Implementing an "invisible times" operator (Unicode character (U+2062)) to more closely emulate the layout. Alas, Perl 6 does not do symbolic calculations at this time and is limited to IEEE 754 floating point for transcendental and irrational number calculations.

e, i and π are all available as built-in constants in Perl 6. 


```perl6
sub infix:<⁢> is tighter(&infix:<**>) { $^a * $^b };

say 'e**i⁢π + 1 ≅ 0 : ', e**i⁢π + 1 ≅ 0;
say 'Error: ', e**i⁢π + 1;
```

{{out}}

```txt
e**i⁢π + 1 ≅ 0 : True
Error: 0+1.2246467991473532e-16i
```



## Phix


```Phix
include builtins\complex.e -- (0.8.0+)
complex res = complex_add(complex_exp(complex_mul(PI,I)),1)
?complex_sprint(res,both:=true)
?complex_sprint(complex_round(res,1e16),true)
?complex_sprint(complex_round(res,1e15))
```

{{out}}
The actual result and two rounded versions (to prove the rounding is doing what it should - the second arg is an inverted precision).

```txt

"0+1.2246e-16i"
"0+1e-16i"
"0"

```



## Python


```python>>>
 import math
>>> math.e ** (math.pi * 1j) + 1
1.2246467991473532e-16j
```



## Racket


```racket
#lang racket
(+ (exp (* 0+i pi)) 1)
```

{{out}}
```txt
0.0+1.2246063538223773e-016i
```



## REXX

The   ''Euler formula''   (or   ''Euler identity'')   states:

:::::::::: <big><big>e<sup>''i''x</sup>   =   cos(x)   +   ''i'' sin(x)</big></big>

Substituting   x   with   <big><math>\pi</math></big>   yields:

:::::::::: <big><big>e<sup>''i''<math>\pi</math></sup>   =   cos(<math>\pi</math>)   +   ''i'' sin(<math>\pi</math>)</big></big>

So, using this Rosetta Code task's version of Euler's identity:

:::::::::: <big><big>e<sup>''i''<math>\pi</math></sup>                           +   1   =   0</big></big>

then we have:

:::::::::: <big><big>cos(<math>\pi</math>)   +   ''i'' sin(<math>\pi</math>)   +   1   =   0</big></big>

So, if the left hand side is evaluated to zero, then Euler's identity is proven.


The REXX language doesn't have any trig or sqrt functions, so some stripped-down
RYO versions are included here.

The   '''sqrt'''   function below supports complex roots.

Note that REXX uses decimal floating point, not binary.   REXX also 
uses a   ''guard''   (decimal) digit when multiplying 

 and dividing,   which aids in increasing the precision. 

This REXX program calculates the trigonometric functions   ('''sin''' and 
'''cos''')   to around half of the number of decimal 

digits that are used in defining the   pi   constant in the REXX program;   so 
the limiting factor for accuracy for the 

trigonometric functions is based on the number of decimal digits (accuracy) 
of   pi   being defined within the REXX 

program.

```rexx
/*REXX program proves  Euler's  identity by showing that:      e^(i pi) + 1  ≡     0    */
numeric digits length( pi() )  -  1              /*set number of decimal digs precision.*/
   cosPi= fmt( cos( pi() ) )                     /*calculate the value of   cos(pi).    */
   sinPi= fmt( sin( pi() ) )                     /*    "      "    "    "   sin(pi).    */
say  '     cos(pi) = '  cosPi                    /*display    "    "    "   cos(Pi).    */
say  '     sin(pi) = '  sinPi                    /*   "       "    "    "   sin(Pi).    */
say                                              /*separate the wheat from the chaff.   */
       $= cosPi +  mult( sqrt(-1), sinPi)  +  1  /*calc. product of sin(x) and sqrt(-1).*/
say  'e^(i pi) + 1 = '  fmt($)     proof($ = 0)  /*display both sides of the equation.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fmt:   procedure; parse arg x; x=format(x,,digits()%2,0);  return left('', x>=0)x / 1
mult:  procedure; parse arg a,b;       if a=0  | b=0  then return 0;            return a*b
pi:    pi= 3.1415926535897932384626433832795028841971693993751058209749445923;  return pi
proof: procedure; parse arg ?;         return '  '  word("unproven proven", ? + 1)
cos:   procedure; parse arg x;                             return .sinCos(1, -1)
sin:   procedure; parse arg x;                             return .sinCos(x,  1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
.sinCos: parse arg z 1 _,i; q=x*x;   do k=2  by 2  until p=z; p=z; _=-_*q/(k*(k+i)); z=z+_
                                     end   /*k*';          return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x;   if x=0  then return 0;  d=digits();  i=;     m.=9;  h=d+6
       numeric digits;  numeric form;    if x<0  then  do;  x= -x;    i= 'i';   end
       parse value format(x, 2, 1, , 0)  'E0'   with   g  'E'  _  .;     g= g * .5'e'_ % 2
             do j=0  while h>9;        m.j=h;                h= h%2 + 1;        end  /*j*/
             do k=j+5  to 0  by -1;    numeric digits m.k;   g= (g+x/g) * .5;   end  /*k*/
       numeric digits d;               return (g/1)i           /*make complex if  X < 0.*/
```

{{out|output}}

```txt

     cos(pi) =  -1
     sin(pi) =   0

e^(i pi) + 1 =   0    proven

```


'''Programming note:'''

To increase the decimal precision of the trigonometric functions past the
current '''500''' decimal digits in the above REXX program,

use the following REXX assignment statement   (the author has a REXX
program with '''1,000,052''' decimal digits of pi that can be 

programmatically invoked with the requested number of decimal digits).


```rexx
/*────────────────── 1,051  decimal digs of  pi. ──────────────────*/

pi=     3.14159265358979323846264338327950288419716939937510
pi= pi || 58209749445923078164062862089986280348253421170679
pi= pi || 82148086513282306647093844609550582231725359408128
pi= pi || 48111745028410270193852110555964462294895493038196
pi= pi || 44288109756659334461284756482337867831652712019091
pi= pi || 45648566923460348610454326648213393607260249141273
pi= pi || 72458700660631558817488152092096282925409171536436
pi= pi || 78925903600113305305488204665213841469519415116094
pi= pi || 33057270365759591953092186117381932611793105118548
pi= pi || 07446237996274956735188575272489122793818301194912
pi= pi || 98336733624406566430860213949463952247371907021798
pi= pi || 60943702770539217176293176752384674818467669405132
pi= pi || 00056812714526356082778577134275778960917363717872
pi= pi || 14684409012249534301465495853710507922796892589235
pi= pi || 42019956112129021960864034418159813629774771309960
pi= pi || 51870721134999999837297804995105973173281609631859
pi= pi || 50244594553469083026425223082533446850352619311881
pi= pi || 71010003137838752886587533208381420617177669147303
pi= pi || 59825349042875546873115956286388235378759375195778
pi= pi || 18577805321712268066130019278766111959092164201989
pi= pi || 38095257201065485863278865936153381827968230301952 
```



## Ruby


```ruby>
 require 'complex'
> Math::E ** (Math::PI * Complex::I) + 1
=> (0.0+0.0i)
```



## Rust


```rust
use std::f64::consts::PI;

extern crate num_complex;
use num_complex::Complex;

fn main() {
    println!("{:e}", Complex::new(0.0, PI).exp() + 1.0);
}
```


{{output}}

```txt

0e0+1.2246467991473532e-16i

```



## Scala

This example makes use of Spire's numeric data types. Complex takes a type parameter determining the type of the coefficients of a + bi, and Real is Spire's exact (i.e. arbitrary precision) numeric data type.


```scala
import spire.math.{Complex, Real}

object Scratch extends App{
  //Declare values with friendly names to clean up the final expression
  val e = Complex[Real](Real.e, 0)
  val pi = Complex[Real](Real.pi, 0)
  val i = Complex[Real](0, 1)
  val one = Complex.one[Real]
  
  println(e.pow(pi*i) + one)
}
```


{{output}}

```txt

(0 + 0i)

```



## Sidef


```ruby
say ('e**i⁢π + 1 ≅ 0 : ', Num.e**Num.pi.i + 1 ≅ 0)
say ('Error: ', Num.e**Num.pi.i + 1)
```

{{out}}

```txt

e**i⁢π + 1 ≅ 0 : true
Error: -2.42661922624586582047028764157944836122122513308e-58i

```



## zkl


```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
Z,pi,e := GSL.Z, (0.0).pi, (0.0).e;

println("e^(\u03c0i) + 1 = %s \u2245 0".fmt( Z(e).pow(Z(0,1)*pi) + 1 ));
println("TMI: ",(Z(e).pow(Z(0,1)*pi) + 1 ).format(0,25,"g"));
```

{{out}}

```txt

e^(πi) + 1 = (0.00+0.00i) ≅ 0
TMI: (0+1.224646799147353207173764e-16i)

```

