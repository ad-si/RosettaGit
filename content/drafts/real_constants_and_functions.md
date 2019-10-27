+++
title = "Real constants and functions"
description = ""
date = 2019-04-04T13:26:38Z
aliases = []
[extra]
id = 2849
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Arithmetic operations]]
[[Category:Simple]]

;Task:
Show how to use the following math constants and functions in your language   (if not available, note it):
:*   <big>''e''</big>   (base of the natural logarithm)
:*   <big><math>\pi</math></big>
:*   square root
:*   logarithm   (any base allowed)
:*   exponential   (<big>''e''<sup>''x''</sup></big> )
:*   absolute value   (a.k.a. "magnitude")
:*   floor   (largest integer less than or equal to this number--not the same as truncate or int)
:*   ceiling   (smallest integer not less than this number--not the same as round up)
:*   power   (<big>''x''<sup>''y''</sup></big> )


;Related task:
*   [[Trigonometric Functions]]





## ACL2

Only the last three are available as built in functions.


```Lisp
(floor 15 2) ;; This is the floor of 15/2
(ceiling 15 2)
(expt 15 2) ;; 15 squared
```



## ActionScript

Actionscript has all the functions and constants mentioned in the task, available in the Math class.

```ActionScript
Math.E;       //e
Math.PI;      //pi
Math.sqrt(u); //square root of u
Math.log(u);  //natural logarithm of u
Math.exp(u);  //e to the power of u
Math.abs(u);  //absolute value of u
Math.floor(u);//floor of u
Math.ceil(u); //ceiling of u
Math.pow(u,v);//u to the power of v
```

The Math class also contains several other constants.

```ActionScript
Math.LN10;   // natural logarithm of 10
Math.LN2;    // natural logarithm of 2
Math.LOG10E; // base-10 logarithm of e
Math.LOG2E;  // base-2 logarithm of e
Math.SQRT1_2;// square root of 1/2
Math.SQRT2;  //square root of 2
```



## Ada

Most of the constants and functions used in this task are defined in the pre-defined Ada package Ada.Numerics.Elementary_Functions.

```ada
Ada.Numerics.e  -- Euler's number
Ada.Numerics.pi -- pi
sqrt(x)         -- square root
log(x, base)    -- logarithm to any specified base
exp(x)          -- exponential
abs(x)          -- absolute value
S'floor(x)      -- Produces the floor of an instance of subtype S
S'ceiling(x)    -- Produces the ceiling of an instance of subtype S
x**y            -- x raised to the y power
```



## Aime


```aime
# e
exp(1);
# pi
2 * asin(1);

sqrt(x);
log(x);
exp(x);
fabs(x);
floor(x);
ceil(x);
pow(x, y);
```



## ALGOL 68


```algol68
REAL x:=exp(1), y:=4*atan(1);
printf(($g(-8,5)"; "$,
    exp(1),    # e #
    pi,        # pi #
    sqrt(x),   # square root #
    log(x),    # logarithm base 10 #
    ln(x),     # natural logarithm #
    exp(x),    # exponential #
    ABS x,     # absolute value #
    ENTIER x,  # floor #
   -ENTIER -x, # ceiling #
    x ** y     # power #
))
```

{{out}}

```txt
 2.71828;  3.14159;  1.64872;  0.43429;  1.00000; 15.15426;  2.71828;  2.00000;  3.00000; 23.14069; 
```


''ALGOL 68'' also includes assorted long, short and complex versions of the above, eg: ''long exp'', ''long long exp'', ''short exp'', ''complex exp'' etc.

And assorted trig functions:  ''sin(x)'', ''arcsin(x)'', ''cos(x)'', ''arccos(x)'', ''tan(x)'', ''arctan(x)'', ''arctan2(x,y)'', ''sinh(x)'', ''arcsinh(x)'', ''cosh(x)'', ''arccosh(x)'', ''tanh(x)'' AND ''arctanh(x)''.


## ALGOL W


```algolw
begin
    real t, u;
    t := 10;
    u := -2.3;
    i_w := 4; s_w := 0; r_format := "A"; r_d := 4; r_w := 9; % set output format %
    write( "         e: ", exp( 1 ) );         % e              %
    write( "        pi: ", pi );               % pi             %
    write( "    root t: ", sqrt( t ) );        % square root    %
    write( "     log t: ", log( t ) );         % log base 10    %
    write( "      ln t: ", ln( t ) );          % log base e     %
    write( "     exp u: ", exp( u ) );         % exponential    %
    write( "     abs u: ", abs u );            % absolute value %
    write( "  floor pi: ", entier( pi ) );     % floor          %
    write( "ceiling pi: ", - entier( - pi ) ); % ceiling        %
    % the raise-to-the-power operator is "**" - it only allows integers for the power %
    write( "  pi cubed: ", pi ** 3 ) % use exp( ln( x ) * y ) for general x^y %
end.
```



## ARM Assembly


{{omit from|ARM Assembly}}
<lang>
/* functions not availables */

```



## AutoHotkey

The following math functions are built into AutoHotkey:

```autohotkey
Sqrt(Number) ; square root
Log(Number) ; logarithm (base 10)
Ln(Number) ; natural logarithm (base e)
Exp(N) ; e to the power N
Abs(Number) ; absolute value
Floor(Number) ; floor
Ceil(Number) ; ceiling
x**y ; x to the power y
```

No mathematical constants are built-in, but they can all be calculated:

```autohotkey
e:=exp(1)
pi:=2*asin(1)
```

The following are additional trigonometric functions that are built into the AutoHotkey language:

```autohotkey
Sin(Number) ; sine
Cos(Number) ; cosine
Tan(Number) ; tangent
ASin(Number) ; arcsine
ACos(Number) ; arccosine
ATan(Number) ; arctangent
```



## AWK

Awk has square root, logarithm, exponential and power.


```awk
BEGIN {
	print sqrt(2)		# square root
	print log(2)		# logarithm base e
	print exp(2)		# exponential
	print 2 ^ -3.4		# power
}
# outputs 1.41421, 0.693147, 7.38906, 0.0947323
```


<blockquote style="font-size: smaller;">'''Power's note:''' 
With [[nawk]] or [[gawk]], <code>2 ** -3.4</code> acts like <code>2 ^ -3.4</code>. 
With [[mawk]], <code>2 ** -3.4</code> is a syntax error. 
Nawk allows <code>**</code>, but its manual page only has <code>^</code>. 
Gawk's manual warns, ''"The POSIX standard only specifies the use of `^' for exponentiation. 
For maximum portability, do not use the `**' operator."''</blockquote>

Awk misses e, pi, absolute value, floor and ceiling; but these are all easy to implement:


```awk
BEGIN {
	E = exp(1)
	PI = atan2(0, -1)
}

function abs(x) {
	return x < 0 ? -x : x
}

function floor(x) {
	y = int(x)
	return y > x ? y - 1 : y
}

function ceil(x) {
	y = int(x)
	return y < x ? y + 1 : y
}

BEGIN {
	print E
	print PI
	print abs(-3.4)		# absolute value
	print floor(-3.4)	# floor
	print ceil(-3.4)	# ceiling
}
# outputs 2.71828, 3.14159, 3.4, -4, -3
```



## Axe

In general, Axe does not support many operations on real numbers. 
However, there are a few special cases that it does support.

To take the square root of an integer X:

```axe
√(X)
```


To take the square root of an 8.8 fixed-point number Y:

```axe
√(Y)ʳ
```


To take the base-2 logarithm of an integer X:

```axe
ln(X)
```


To take 2 raised to an integer X: (Note that the base is not Euler's number)

```axe
e^(X)
```


To take the absolute value of a signed integer X:

```axe
abs(X)
```



## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
abs(x) 'absolute value
sqr(x) 'square root
exp(x) 'exponential
log(x) 'natural logarithm
x ^ y 'power
'floor, ceiling, e, and pi not available
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET X=2:LET Y=5
110 PRINT EXP(1)         ! value of e
120 PRINT PI             ! value of Pi
130 PRINT ROUND(PI,3)    ! rounds Pi to 3 decimal places
140 PRINT TRUNCATE(PI,3) ! cuts 3 decimal places from Pi
150 PRINT SQR(X)         ! square root of x
160 PRINT LOG(X)         ! the natural logarithm of number x
170 PRINT LOG2(X)        ! logarithm of x to base 2
180 PRINT LOG10(X)       ! logarithm of x to base 10
190 PRINT EXP(X)         ! exponential
200 PRINT ABS(X)         ! the absolute value of a number
210 PRINT INT(X)         ! the largest whole number not bigger than x
220 PRINT IP(X)          ! the integer part of x
230 PRINT FP(X)          ! stands for fractorial part
240 PRINT CEIL(X)        ! ceiling: gives the smallest whole number not less than x
250 PRINT X^Y            ! power
260 PRINT MIN(X,Y)       ! the smaller number of x and y
270 PRINT MAX(X,Y)       ! the bigger number of x and y
280 PRINT EPS(X)         ! the smallest quantity that can be added to or subtracted from x to make the interpreter register a change in the value of x
290 PRINT INF            ! The largest positive number the tinterpreter can handle. This number is 9.999999999*10^62
```


=
## Sinclair ZX81 BASIC
=
Arguments to built-in functions may be placed in parentheses, but are not required to be.

Base of the natural logarithm:

```basic>EXP 1</lang


<math>\pi</math>:

```basic>PI</lang


Square root:

```basic>SQR X</lang


Natural logarithm:

```basic>LN X</lang


Exponential:

```basic>EXP X</lang


Absolute value:

```basic>ABS X</lang


Floor:

```basic>INT X</lang

(NB. Although this function is called <code>INT</code>, it corresponds to <code>floor</code>: e.g. <code>INT -3.1</code> returns -4 not -3.)

Ceiling:

not provided as a built-in function.

Power:

```basic
X**Y
```

NB. Both <math>x</math> and <math>y</math> can be real numbers.

=
## BBC BASIC
=

```bbcbasic
      e = EXP(1)
      Pi = PI
      Sqr2 = SQR(2)
      Ln2 = LN(2)
      Log2 = LOG(2) : REM Base 10
      Exp2 = EXP(2)
      Abs2 = ABS(-2)
      Floor = INT(1.234)
      Ceil = FNceil(1.234)
      Power = 1.23^4
      END
      
      DEF FNceil(n) = INT(n) - (INT(n) <> n)

```



## bc

The language has square root and power, but power only works if the exponent is an integer.


```bc
scale = 6
sqrt(2)		/* 1.414213	square root */
4.3 ^ -2	/* .054083	power (integer exponent) */
```


The standard library has natural logarithm and exponential functions. It can calculate e and pi: e comes from the exponential function, while pi is four times the arctangent of one. The usual formulas can calculate the powers with fractional exponents, and the logarithms with any base.

{{libheader|bc -l}}

```bc
scale = 6
l(2)		/* .693147	natural logarithm */
e(2)		/* 7.389056	exponential */

p = 4 * a(1)
e = e(1)
p		/* 3.141592	pi to 6 fractional digits */
e		/* 2.178281	e to 6 fractional digits */

e(l(2) * -3.4)	/* .094734	2 to the power of -3.4 */
l(1024) / l(2)	/* 10.000001	logarithm base 2 of 1024 */
```


The missing functions are absolute value, floor and ceiling. You can implement these functions, if you know what to do.

{{trans|AWK}}

```bc
/* absolute value */
define v(x) {
	if (x < 0) return (-x)
	return (x)
}

/* floor */
define f(x) {
	auto s, y

	s = scale
	scale = 0
	y = x / 1
	scale = s

	if (y > x) return (y - 1)
	return (y)
}

/* ceiling */
define g(x) {
	auto s, y

	s = scale
	scale = 0
	y = x / 1
	scale = s

	if (y < x) return (y + 1)
	return (y)
}

v(-3.4)		/* 3.4		absolute value */
f(-3.4)		/* -4		floor */
g(-3.4)		/* -3		ceiling */
```



## blz

The constant e

```blz
{e}
```


The constant pi

```blz
{pi}
```


Square root

```blz
x ** 0.5
```


Logarithm (base n)

```blz
x __ n
```


Exponential

```blz
{e} ** x
```


Absolute Value

```blz
abs(x)
```


Floor

```blz
floor(x)
```


Ceiling

```blz
ceil(x)
```


Power x to the y

```blz
x ** y
```



## Bracmat

Bracmat has no real number type, but the constants <code>e</code> and <code>pi</code>, together with <code>i</code> can be used as symbols with the intended mathematical meaning in exponential functions.
For example, differentiation <code>10^x</code> to <code>x</code>

```bracmat
x \D (10^x) { \D is the differentiation operator }
```

has the result

```bracmat
10^x*e\L10 { \L is the logarithm operator }
```

Likewise <code>e^(i*pi)</code> evaluates to <code>-1</code> and <code>e^(1/2*i*pi)</code> evaluates to <code>i</code>.

When taking the square root of a (rational) number, and nominator and denominator are not too big (convertible to 32 or 64 bit integers, depending on platform), Bracmat resolves the number in prime factors and halves the exponents of each of the prime factors.

Bracmat handles logarithms in any base, except real numbers that are not rational. Example: <code>24/7 \L 119/9</code> evaluates to <code>2+24/7\L5831/5184</code>.

Bracmat does not attempt to compute the numerical value of the exponential function, except for a the special case where the result is a rational number.
Thus <code>e^0</code> evaluates to <code>1</code>.

Bracmat has no built-in functions for computing the absolute value,
floor or ceiling. For real numbers that are rational such functions can be written. 

If the result of taking the power of a rational number to another rational number is rational, Bracmat can in many compute it, if needed using prime factorization. See root above. Example:
<code> 243/1024^2/5</code> evaluates to <code>9/16</code>.


## C

Most of the following functions take a double.

```c>#include <math.h


M_E; /* e - not standard but offered by most implementations */
M_PI; /* pi - not standard but offered by most implementations */
sqrt(x); /* square root--cube root also available in C99 (cbrt) */
log(x); /* natural logarithm--log base 10 also available (log10) */
exp(x); /* exponential */
abs(x); /* absolute value (for integers) */
fabs(x); /* absolute value (for doubles) */
floor(x); /* floor */
ceil(x); /* ceiling */
pow(x,y); /* power */
```


To access the M_PI, etc. constants in Visual Studio, you may need to add the line <code>#define _USE_MATH_DEFINES</code> before the <code>#include <math.h></code>.


## C++


```cpp>#include <iostream

#include <cmath>

#ifdef M_E
static double euler_e = M_E;
#else
static double euler_e = std::exp(1); // standard fallback
#endif

#ifdef M_PI
static double pi = M_PI;
#else
static double pi = std::acos(-1);
#endif

int main()
{
  std::cout << "e = " << euler_e
            << "\npi = " << pi
            << "\nsqrt(2) = " << std::sqrt(2.0)
            << "\nln(e) = " << std::log(e)
            << "\nlg(100) = " << std::log10(100.0)
            << "\nexp(3) = " << std::exp(3.0)
            << "\n|-4.5| = " << std::abs(-4.5)   // or std::fabs(4.0); both work in C++
            << "\nfloor(4.5) = " << std::floor(4.5)
            << "\nceiling(4.5) = " << std::ceil(4.5)
            << "\npi^2 = " << std::pow(pi,2.0) << std::endl;
}
```



## C sharp


```csharp
using System;

class Program {
    static void Main(string[] args) {        
        Console.WriteLine(Math.E); //E
        Console.WriteLine(Math.PI); //PI
        Console.WriteLine(Math.Sqrt(10)); //Square Root
        Console.WriteLine(Math.Log(10)); // Logarithm
        Console.WriteLine(Math.Log10(10)); // Base 10 Logarithm
        Console.WriteLine(Math.Exp(10)); // Exponential
        Console.WriteLine(Math.Abs(10)); //Absolute value
        Console.WriteLine(Math.Floor(10.0)); //Floor
        Console.WriteLine(Math.Ceiling(10.0)); //Ceiling
        Console.WriteLine(Math.Pow(2, 5)); // Exponentiation
    }
}
```



## Chef

See [[Basic integer arithmetic#Chef]] for powers.


## Clojure

{{trans|Java}} which is directly available.

```lisp
(Math/E); //e
(Math/PI); //pi
(Math/sqrt x); //square root--cube root also available (cbrt)
(Math/log x); //natural logarithm--log base 10 also available (log10)
(Math/exp x); //exponential
(Math/abs x); //absolute value
(Math/floor x); //floor
(Math/ceil x); //ceiling
(Math/pow x y); //power
```


Clojure does provide arbitrary precision versions as well:


```lisp
(ns user (:require [clojure.contrib.math :as math]))
(math/sqrt x)
(math/abs x)
(math/floor x)
(math/ceil x)
(math/expt x y) 
```


.. and as multimethods that can be defined for any type (e.g. complex numbers).


```lisp
(ns user (:require [clojure.contrib.generic.math-functions :as generic]))
(generic/sqrt x)
(generic/log x)
(generic/exp x)
(generic/abs x)
(generic/floor x)
(generic/ceil x)
(generic/pow x y)
```



## COBOL

Everything that follows can take any number (except for <code>SQRT</code> which expects a non-negative number).
The task constants and (intrinsic) functions:

```cobol
E          *> e
PI         *> Pi
SQRT(n)    *> Sqaure root
LOG(n)     *> Natural logarithm
LOG10(n)   *> Logarithm (base 10)
EXP(n)     *> e to the nth power
ABS(n)     *> Absolute value
INTEGER(n) *> While not a proper floor function, it is implemented in the same way.
*> There is no ceiling function. However, it could be implemented like so:
ADD 1 TO N
MOVE INTEGER(N) TO Result
*> There is no pow function, although the COMPUTE verb does have an exponention operator.
COMPUTE Result = N ** 2 
```

COBOL also has the following extra mathematical functions:

```cobol
FACTORIAL(n) *> Factorial
EXP10(n)     *> 10 to the nth power
*> Trigonometric functions, including inverse ones, named as would be expected.
```



## Common Lisp

In Lisp we should really be talking about numbers rather than the type <code>real</code>. The types <code>real</code> and <code>complex</code> are subtypes of <code>number</code>. Math operations that accept or produce complex numbers generally do.

```lisp

(exp 1)     ; e (Euler's number)
pi          ; pi constant
(sqrt x)    ; square root: works for negative reals and complex
(log x)     ; natural logarithm: works for negative reals and complex
(log x 10)  ; logarithm base 10
(exp x)     ; exponential
(abs x)     ; absolute value: result exact if input exact: (abs -1/3) -> 1/3.
(floor x)   ; floor: restricted to real, two valued (second value gives residue)
(ceiling x) ; ceiling: restricted to real, two valued (second value gives residue)
(expt x y)  ; power

```



## D


```d
import std.math ; // need to import this module
E        // Euler's number
PI       // pi constant
sqrt(x)  // square root
log(x)   // natural logarithm
log10(x) // logarithm base 10
log2(x)  // logarithm base 2
exp(x)   // exponential
abs(x)   // absolute value (= magnitude for complex)
floor(x) // floor
ceil(x)  // ceiling
pow(x,y) // power
```



## Delphi

Log, Floor, Ceil and Power functions defined in Math.pas.


```Delphi
Exp(1);         // e (Euler's number)
Pi;             // π (Pi)
Sqrt(x);        // square root
LogN(BASE, x)   // log of x for a specified base
Log2(x)         // log of x for base 2
Log10(x)        // log of x for base 10
Ln(x);          // natural logarithm (for good measure)
Exp(x);         // exponential
Abs(x);         // absolute value (a.k.a. "magnitude")
Floor(x);       // floor
Ceil(x);        // ceiling
Power(x, y);    // power
```



## DWScript


See [[Real_constants_and_functions#Delphi|Delphi]].


## E


```e
? 1.0.exp()
# value: 2.7182818284590455

? 0.0.acos() * 2
# value: 3.141592653589793

? 2.0.sqrt()
# value: 1.4142135623730951

? 2.0.log()
# value: 0.6931471805599453

? 5.0.exp()
# value: 148.4131591025766

? (-5).abs()
# value: 5

? 1.2.floor()
# value: 1

? 1.2.ceil()
# value: 2

? 10 ** 6
# value: 1000000
```


## Elena

ELENA 4.x :

```elena
import system'math;
import extensions;
 
public program()
{
    console.printLine(E_value);       //E
    console.printLine(Pi_value);      //PI
    console.printLine(10.sqrt());     //Square Root        
    console.printLine(10.ln());       //Logarithm        
    console.printLine(10.log10());    // Base 10 Logarithm
    console.printLine(10.exp());      //Exponential       
    console.printLine(10.Absolute); //Absolute value
    console.printLine(10.0r.floor()); //Floor
    console.printLine(10.0r.ceil());  //Ceiling    
    console.printLine(2.power(5));    //Exponentiation    
}
```



## Elixir


```elixir
defmodule Real_constants_and_functions do
  def main do
    IO.puts :math.exp(1)                # e
    IO.puts :math.pi                    # pi
    IO.puts :math.sqrt(16)              # square root
    IO.puts :math.log(10)               # natural logarithm
    IO.puts :math.log10(10)             # base 10 logarithm
    IO.puts :math.exp(2)                # e raised to the power of x
    IO.puts abs(-2.24)                  # absolute value
    IO.puts Float.floor(3.1423)         # floor
    IO.puts Float.ceil(20.125)          # ceiling
    IO.puts :math.pow(3,2)              # exponentiation
  end
end

Real_constants_and_functions.main
```



## Elm

The following are all in the Basics module, which is imported by default:

```elm
e           -- e
pi          -- pi
sqrt x      -- square root
logBase 3 9 -- logarithm (any base)
e^x         -- exponential
abs x       -- absolute value
floor x     -- floor
ceiling x   -- ceiling
2 ^ 3       -- power
```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(math_constants).
-export([main/0]).
main() ->
	io:format("~p~n",	[math:exp(1)]		),		% e
	io:format("~p~n",	[math:pi()]		),		% pi
	io:format("~p~n",	[math:sqrt(16)]		),		% square root
	io:format("~p~n",	[math:log(10)]		),		% natural logarithm			
	io:format("~p~n",	[math:log10(10)]	),		% base 10 logarithm
	io:format("~p~n",	[math:exp(2)]		),		% e raised to the power of x
	io:format("~p~n",	[abs(-2.24)]		),		% absolute value
	io:format("~p~n",	[floor(3.1423)]		),		% floor
	io:format("~p~n",	[ceil(20.125)]		),		% ceiling
	io:format("~p~n",	[math:pow(3,2)]	        ).		% exponentiation 

floor(X) when X < 0 ->
	T = trunc(X),
	case X - T == 0 of
		true -> T;
        	false -> T - 1
	end;

floor(X) -> 
	trunc(X).


ceil(X) when X < 0 ->
	trunc(X);

ceil(X) ->
	T = trunc(X),
	case X - T == 0 of
		true -> T;
		false -> T + 1
	end.	

```

{{out}}

```txt
2.718281828459045
3.141592653589793
4.0
2.302585092994046
1.0
7.38905609893065
2.24
3
21
9.0
ok


```



## ERRE


```ERRE
PROGRAM R_C_F

FUNCTION CEILING(X)
   CEILING=INT(X)-(X-INT(X)>0)
END FUNCTION

FUNCTION FLOOR(X)
   FLOOR=INT(X)
END FUNCTION

BEGIN
     PRINT(EXP(1))           ! e  not available
     PRINT(π)                ! pi is available or ....
     PRINT(4*ATN(1))         ! .... equal to

     X=12.345
     Y=1.23

     PRINT(SQR(X),X^0.5)     ! square root
     PRINT(LOG(X))           ! natural logarithm base e
     PRINT(LOG(X)/LOG(10))   ! base 10 logarithm
     PRINT(LOG(X)/LOG(Y))    ! arbitrary base logarithm (y>0)
     PRINT(EXP(X))           ! exponential
     PRINT(ABS(X))           ! absolute value
     PRINT(FLOOR(X))         ! floor
     PRINT(CEILING(X))       ! ceiling
     PRINT(X^Y)              ! power
END PROGRAM
```

{{out}}

```txt
 2.718282
 3.141592653589793
 3.141593
 3.513545      3.513545
 2.513251
 1.091491
 12.14048
 229808.1
 12.345
 12
 13
 22.00564
```


=={{header|F Sharp|F#}}==
{{trans|C#|C sharp}}

```fsharp
open System

let main _ =
    Console.WriteLine(Math.E);             // e
    Console.WriteLine(Math.PI);            // Pi
    Console.WriteLine(Math.Sqrt(10.0));    // Square Root
    Console.WriteLine(Math.Log(10.0));     // Logarithm
    Console.WriteLine(Math.Log10(10.0));   // Base 10 Logarithm
    Console.WriteLine(Math.Exp(10.0));     // Exponential
    Console.WriteLine(Math.Abs(10));       // Absolute value
    Console.WriteLine(Math.Floor(10.0));   // Floor
    Console.WriteLine(Math.Ceiling(10.0)); // Ceiling
    Console.WriteLine(Math.Pow(2.0, 5.0)); // Exponentiation

    0
```



## Factor


```factor
e          ! e
pi         ! π
sqrt       ! square root
log        ! natural logarithm
exp        ! exponentiation
abs        ! absolute value
floor      ! greatest whole number smaller than or equal
ceiling    ! smallest whole number greater than or equal
truncate   ! remove the fractional part (i.e. round towards 0)
round      ! round to next whole number
^          ! power
```



## Fantom


The <code>Float</code> class holds 64-bit floating point numbers, and contains most of the useful mathematical functions.  A floating point number must be specified when entered with the suffix 'f', e.g. <code>9f</code>


```fantom

Float.e
Float.pi
9f.sqrt
9f.log      // natural logarithm
9f.log10    // logarithm to base 10
9f.exp      // exponentiation
(-3f).abs   // absolute value, note bracket
3.2f.floor  // nearest Int smaller than this number
3.2f.ceil   // nearest Int bigger than this number
3.2f.round  // nearest Int
3f.pow(2f)  // power

```


Note, . binds more tightly than -, so use brackets around negative numbers:


```txt

> -3f.pow(2f)
-9
> (-3f).pow(2f)
9

```



## Forth


```forth
1e fexp fconstant e
0e facos 2e f* fconstant pi  \ predefined in gforth
fsqrt ( f -- f )
fln ( f -- f )   \ flog for base 10
fexp ( f -- f )
fabs ( f -- f )
floor ( f -- f )  \ round towards -inf
: ceil ( f -- f ) fnegate floor fnegate ; \ not standard, though fround is available
f** ( f e -- f^e )
```



## Fortran


```fortran
 e          ! Not available. Can be calculated EXP(1.0)
 pi         ! Not available. Can be calculated 4.0*ATAN(1.0)
 SQRT(x)    ! square root
 LOG(x)     ! natural logarithm
 LOG10(x)   ! logarithm to base 10
 EXP(x)     ! exponential
 ABS(x)     ! absolute value
 FLOOR(x)   ! floor - Fortran 90 or later only
 CEILING(x) ! ceiling - Fortran 90 or later only
 x**y       ! x raised to the y power
```


4*ATAN(1.0) will be calculated in single precision, likewise EXP(1.0) (not EXP(1), because 1 is an integer) and although double precision functions can be named explicitly, 4*DATAN(1.0) will be rejected because 1.0 is in single precision and DATAN expects double. Thus, 4*DATAN(1.0D0) or 4*DATAN(1D0) will do, as the D in the exponent form specifies double precision. Whereupon, the generic names can be returned to: 4*ATAN(1D0). Some systems go further and offer quadruple precision. Others allow that all constants will be deemed double precision as a compiler option.

The 4 need not be named as 4.0, or 4D0, as 4 the integer will be converted by the compiler to double precision, because it is to meet a known double precision value in simple multiplication and so will be promoted. Hopefully, at compile time.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#Include "crt/math.bi"

Print M_E          '' constant "e" from C runtime library
Print M_PI         '' constant "pi" from C runtime library
Print Sqr(2)       '' square root function built into FB
Print Log(M_E)     '' log to base "e" built into FB
Print log10(10)    '' log to base 10 from C runtime library   
Print Exp(1)       '' exponential function built into FB
Print Abs(-1)      '' absolute value function (integers or floats) built into FB
Print Int(-2.5)    '' floor function built into FB 
Print ceil(-2.5)   '' ceiling function from C runtime library
Print 2.5 ^ 3.5    '' exponentiation operator built into FB
Sleep 
```


{{out}}

```txt

 2.718281828459045
 3.141592653589793
 1.414213562373095
 1
 1
 2.718281828459045
 1
-3
-2
 24.70529422006547

```



## Frink

All of the following operations work for any numerical type, including rational numbers, complex numbers and intervals of real numbers.

```frink

e
pi, π   // Unicode can also be written in ASCII programs as \u03C0
sqrt[x]
ln[x]   // Natural log
log[x]  // Log to base 10
exp[x], e^x
abs[x]
floor[x] // Except for complex numbers where there's no good interpretation.
ceil[x]  // Except for complex numbers where there's no good interpretation.
x^y

```




## FutureBasic


```futurebasic

include "ConsoleWindow"

// Set width of tab
def tab 8

print "exp:",   exp(1)
print "pi:",    pi
print "sqr:",   sqr(2)
print "log:",   log(2)
print "log2:",  log2(2)
print "log10",  log10(2)
print "abs:",   abs(-2)
print "floor:", int(1.534)
print "ceil:",  val( using"###"; 1.534 )
print "power:", 1.23 ^ 4

```

Output:

```txt

exp:     2.7182818285
pi:      3.1415926536
sqr:     1.4142135624
log:     0.6931471806
log2:    1
log10    0.3010299957
abs:     2
floor:   2
ceil:    2
power:   2.28886641

```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/big"
)

func main() {
    // e and pi defined as constants.
    // In Go, that means they are not of a specific data type and can be used
    // as float32 or float64.  Println takes the float64 values.
    fmt.Println("float64 values:")
    fmt.Println("e:", math.E)
    fmt.Println("π:", math.Pi)

    // The following functions all take and return the float64 data type.

    // square root.  cube root also available (math.Cbrt)
    fmt.Println("square root(1.44):", math.Sqrt(1.44))
    // natural logarithm--log base 10, 2 also available (math.Log10, math.Log2)
    // also available is log1p, the log of 1+x.  (using log1p can be more
    // accurate when x is near zero.)
    fmt.Println("ln(e):", math.Log(math.E))
    // exponential.  also available are exp base 10, 2 (math.Pow10, math.Exp2)
    fmt.Println("exponential(1):", math.Exp(1))
    fmt.Println("absolute value(-1.2):", math.Abs(-1.2))
    fmt.Println("floor(-1.2):", math.Floor(-1.2))
    fmt.Println("ceiling(-1.2):", math.Ceil(-1.2))
    fmt.Println("power(1.44, .5):", math.Pow(1.44, .5))

    // Equivalent functions for the float32 type are not in the standard
    // library.  Here are the constants e and π as float32s however.
    fmt.Println("\nfloat32 values:")
    fmt.Println("e:", float32(math.E))
    fmt.Println("π:", float32(math.Pi))

    // The standard library has an arbitrary precision floating point type but
    // provides only the most basic methods.  Also while the constants math.E
    // and math.Pi are provided to over 80 decimal places, there is no
    // convenient way of loading these numbers (with their full precision)
    // into a big.Float.  A hack is cutting and pasting into a string, but
    // of course if you're going to do that you are free to cut and paste from
    // any other source.  (The documentation cites OEIS as its source.)
    pi := "3.141592653589793238462643383279502884197169399375105820974944"
    π, _, _ := big.ParseFloat(pi, 10, 200, 0)
    fmt.Println("\nbig.Float values:")
    fmt.Println("π:", π)
    // Of functions requested by the task, only absolute value is provided.
    x := new(big.Float).Neg(π)
    y := new(big.Float)
    fmt.Println("x:", x)
    fmt.Println("abs(x):", y.Abs(x))
}
```

{{out}}

```txt

float64 values:
e: 2.718281828459045
π: 3.141592653589793
square root(1.44): 1.2
ln(e): 1
exponential(1): 2.718281828459045
absolute value(-1.2): 1.2
floor(-1.2): -2
ceiling(-1.2): -1
power(1.44, .5): 1.2

float32 values:
e: 2.7182817
π: 3.1415927

big.Float values:
π: 3.141592653589793238462643383279502884197169399375105820974944
x: -3.141592653589793238462643383279502884197169399375105820974944
abs(x): 3.141592653589793238462643383279502884197169399375105820974944

```



## Groovy

Math constants and functions are as outlined in the [[#Java|Java]] example, except as follows:

'''Absolute Value'''

In addition to the java.lang.Math.abs() method, each numeric type has an abs() method, which can be invoked directly on the number:

```groovy
println ((-22).abs())
```

{{out}}

```txt
22
```


'''Power'''

In addition to the java.lang.Math.pow() method, each numeric type works with the power operator (**), which can be invoked as an in-fix operator between two numbers:

```groovy
println 22**3.5
```

{{out}}

```txt
49943.547010599876
```


Power results are not defined for all possible pairs of operands. 
Any power operation that does not have a result returns a 64-bit IEEE NaN (Not a Number) value.

```groovy
println ((-22)**3.5)
```

{{out}}

```txt
NaN
```


Also note that at the moment (07:00, 19 March 2011 (UTC)) Groovy (1.7.7) gives a mathematically incorrect result for "0**0". 
The correct result should be "NaN", but the Groovy operation result is "1".


## Haskell

The operations are defined for the various numeric typeclasses, as defined in their type signature.

```haskell
exp 1     -- Euler number
pi        -- pi
sqrt x    -- square root
log x     -- natural logarithm
exp x     -- exponential
abs x     -- absolute value
floor x   -- floor
ceiling x -- ceiling
x ** y    -- power (e.g. floating-point exponentiation)
x ^ y     -- power (e.g. integer exponentiation, nonnegative y only)
x ^^ y    -- power (e.g. integer exponentiation of rationals, also negative y)
```



## HicEst

Except for x^y, this is identical to Fortran:

```HicEst
e          ! Not available. Can be calculated EXP(1)
pi         ! Not available. Can be calculated 4.0*ATAN(1.0)
x^0.5      ! square root
LOG(x)     ! natural logarithm
LOG(x, 10) ! logarithm to base 10
EXP(x)     ! exponential
ABS(x)     ! absolute value
FLOOR(x)   ! floor
CEILING(x) ! ceiling
x**y       ! x raised to the y power
x^y        ! same as x**y
```


== {{header|Icon}} and {{header|Unicon}} ==

```Icon
link numbers  # for floor and ceil

procedure main()
write("e=",&e)
write("pi=",&pi)
write("phi=",&phi)
write("sqrt(2)=",sqrt(2.0))
write("log(e)=",log(&e))
write("log(100.,10)=",log(100,10))
write("exp(1)=",exp(1.0))
write("abs(-2)=",abs(-2))
write("floor(-2.2)=",floor(-2.2))
write("ceil(-2.2)=",ceil(-2.2))
write("power: 3^3=",3^3)
end
```

{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers provides floor and ceiling] 

{{out}}

```txt
e=2.718281828459045
pi=3.141592653589793
phi=1.618033988749895
sqrt(2)=1.414213562373095
log(e)=1.0
log(100.,10)=2.0
exp(1)=2.718281828459045
abs(-2)=2
floor(-2.2)=-2
ceil(-2.2)=-3
```



## J

The examples below require arguments (x and y) to be numeric nouns.

```j
e =. 1x1   NB. Euler's number, specified as a numeric literal.
e =. ^ 1   NB. Euler's number, computed by exponentiation.
pi=. 1p1   NB. pi, specified as a numeric literal.
pi=. o.1   NB. pi, computed trigonometrically.
magnitude_of_x   =. |x
floor_of_x       =. <.x
ceiling_of_x     =. >.x
natural_log_of_x =. ^.x
base_x_log_of_y  =. x^.y
x_squared        =. *:x     NB. special form
x_squared        =. x^2     NB. exponential form
square_root_of_x =. %:x     NB. special form
square_root_of_x =. x^0.5   NB. exponential form
x_to_the_y_power =. x^y
```



## Java

All of these functions are in Java's <tt>Math</tt> class which, does not require any imports:

```java
Math.E; //e
Math.PI; //pi
Math.sqrt(x); //square root--cube root also available (cbrt)
Math.log(x); //natural logarithm--log base 10 also available (log10)
Math.exp(x); //exponential
Math.abs(x); //absolute value
Math.floor(x); //floor
Math.ceil(x); //ceiling
Math.pow(x,y); //power
```



## JavaScript


```javascript
Math.E
Math.PI
Math.sqrt(x)
Math.log(x)
Math.exp(x)
Math.abs(x)
Math.floor(x)
Math.ceil(x)
Math.pow(x,y)
```



## jq

The mathematical functions available in jq are defined as 0-arity filters, so to evaluate the sqrt of 4, one writes <tt>4|sqrt</tt>. 
In jq, "." refers to the output coming from the left in the pipeline. 

In the following, comments appear after the "#":
```jq

1 | exp                      # i.e. e
1 | atan * 4                 # i.e. π
sqrt
log                          # Naperian log
exp
if . < 0 then -. else . end  # absolute value
floor
# jq does not currently have a ceiling function
# jq does not currently have a function to compute x^y
```



## Jsish


```javascript
/* real constants and functions, in JSI */
var x, y;

;Math.E;
;Math.PI;

;x = 100.0;
;Math.sqrt(x);
;Math.log(x);

;x = 2.0;
;Math.exp(x);

;x = -x;
;Math.abs(x);

;x = 42.42;
;Math.floor(x);
;Math.ceil(x);

;x = 10.0;
;y = 5;
;Math.pow(x,y);

/*
=!EXPECTSTART!=
Math.E ==> 2.718281828459045
Math.PI ==> 3.141592653589793
x = 100.0 ==> 100
Math.sqrt(x) ==> 10
Math.log(x) ==> 4.605170185988092
x = 2.0 ==> 2
Math.exp(x) ==> 7.38905609893065
x = -x ==> -2
Math.abs(x) ==> 2
x = 42.42 ==> 42.42
Math.floor(x) ==> 42
Math.ceil(x) ==> 43
x = 10.0 ==> 10
y = 5 ==> 5
Math.pow(x,y) ==> 100000
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U real-constants.jsi
Math.E ==> 2.718281828459045
Math.PI ==> 3.141592653589793
x = 100.0 ==> 100
Math.sqrt(x) ==> 10
Math.log(x) ==> 4.605170185988092
x = 2.0 ==> 2
Math.exp(x) ==> 7.38905609893065
x = -x ==> -2
Math.abs(x) ==> 2
x = 42.42 ==> 42.42
Math.floor(x) ==> 42
Math.ceil(x) ==> 43
x = 10.0 ==> 10
y = 5 ==> 5
Math.pow(x,y) ==> 100000

# Run the unit tests
prompt$ jsish -u real-constants.jsi
[PASS] real-constants.jsi
```



## Julia


```julia
e
π, pi
sqrt(x)
log(x)
exp(x)
abs(x)
floor(x)
ceil(x)
x^y
```

Note that Julia supports Unicode identifiers, and allows either <code>π</code> or <code>pi</code> for that constant.

Also, mathematical constants like <i>e</i> and π in Julia are of a special type that is automatically converted to the correct precision when used in aritmetic operations.  So, for example, <code>BigFloat(2) * π</code> computes 2π in arbitrary precision arithmetic.


## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    println(Math.E)                // e
    println(Math.PI)               // pi
    println(Math.sqrt(2.0))        // square root
    println(Math.log(Math.E))      // log to base e
    println(Math.log10(10.0))      // log to base 10
    println(Math.exp(1.0))         // exponential
    println(Math.abs(-1))          // absolute value
    println(Math.floor(-2.5))      // floor 
    println(Math.ceil(-2.5))       // ceiling 
    println(Math.pow(2.5, 3.5))    // power
}
```


{{out}}

```txt

2.718281828459045
3.141592653589793
1.4142135623730951
1.0
1.0
2.718281828459045
1
-3.0
-2.0
24.705294220065465

```



## Lasso


```Lasso
//e
define e => 2.7182818284590452

//π
define pi => 3.141592653589793

e
pi
9.0->sqrt
1.64->log
1.64->log10
1.64->exp
1.64->abs
1.64->floor
1.64->ceil
1.64->pow(10.0)
```



## Liberty BASIC

Ceiling and floor easily implemented as functions.


sqr( is the LB function for square root.


e & pi not available- calculate as shown.

```lb

print exp( 1)            ' e  not available
print 4 *atn( 1)         ' pi not available

x =12.345: y =1.23

print sqr( x), x^0.5     ' square root- NB the unusual name
print log( x)            ' natural logarithm base e
print log( x) /2.303     ' base 10 logarithm
print log( x) /log( y)   ' arbitrary base logarithm
print exp( x)            ' exponential
print abs( x)            ' absolute value
print floor( x)          ' floor
print ceiling( x)        ' ceiling
print x^y                ' power

end

function floor( x)
    if x >0 then
        floor =int( x)
    else
        if x <>int( x) then floor =int( x) -1 else floor =int( x)
    end if
end function

function ceiling( x)
    if x <0 then
        ceiling =int( x)
    else
        ceiling =int( x) +1
    end if
end function

```



## Lingo


```lingo
the floatPrecision = 8

-- e (base of the natural logarithm)
put exp(1)
-- 2.71828183

-- pi
put PI
-- 3.14159265

-- square root
put sqrt(2.0)
-- 1.41421356

-- logarithm (any base allowed)
x = 100

put log(x) -- calculate log for base e
-- 4.60517019

put log(x)/log(10) -- calculate log for base 10
-- 2.00000000

-- exponential (ex)
put exp(3)
-- 20.08553692

-- absolute value (a.k.a. "magnitude")
put abs(-1)
-- 1

-- floor (largest integer less than or equal to this number--not the same as truncate or int)
n = 23.536
put bitOr(n, 0) -- calculates floor
-- 23

-- ceiling (smallest integer not less than this number--not the same as round up)
n = 23.536
-- calculates ceil
floor = bitOr(n, 0)
if (floor >= n) then put floor
else put floor+1
-- 24

-- power
put power(2, 8)
-- 256.00000000
```



## LiveCode

LC 7.1+, prior to this floor & ceil were not built-in.

```LiveCode
e‬: exp(1)
pi: pi
square root: sqrt(x)
logarithm: log(x)
exponential (‪ex‬): exp(x)
absolute value: abs(x)
floor: floor(x)
ceiling: ceil(x)
power: x^y
```



## Logo

{{works with|UCB Logo}}

```logo
make "e exp 1
make "pi 2*(RADARCTAN 0 1)
sqrt :x
ln :x
exp :x
; there is no standard abs, floor, or ceiling; only INT and ROUND.
power :x :y
```



## Logtalk


```logtalk

:- object(constants_and_functions).

    :- public(show/0).
    show :-
        write('e = '), E is e, write(E), nl,
        write('pi = '), PI is pi, write(PI), nl,
        write('sqrt(2) = '), SQRT is sqrt(2), write(SQRT), nl,
        % only base e logorithm is avaialable as a standard built-in function
        write('log(2) = '), LOG is log(2), write(LOG), nl,
        write('exp(2) = '), EXP is exp(2), write(EXP), nl,
        write('abs(-1) = '), ABS is abs(-1), write(ABS), nl,
        write('floor(-3.4) = '), FLOOR is floor(-3.4), write(FLOOR), nl,
        write('ceiling(-3.4) = '), CEILING is ceiling(-3.4), write(CEILING), nl,
        write('2 ** -3.4 = '), POWER is 2 ** -3.4, write(POWER), nl.

:- end_object.

```

{{out}}

```txt

| ?- constants_and_functions::show.
e = 2.718281828459045
pi = 3.141592653589793
sqrt(2) = 1.4142135623730951
log(2) = 0.6931471805599453
exp(2) = 7.38905609893065
abs(-1) = 1
floor(-3.4) = -4
ceiling(-3.4) = -3
2 ** -3.4 = 0.09473228540689989
yes

```



## Lua


```lua
math.exp(1)
math.pi
math.sqrt(x)
math.log(x)
math.log10(x)
math.exp(x)
math.abs(x)
math.floor(x)
math.ceil(x)
x^y
```




## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Def exp(x)= 2.71828182845905^x
      Print Ln(exp(1))==1
      Print Log(10^5)==5
      Print Sgn(-5)=-1
      Print Abs(-2.10#)=2.1#
      Def exptype$(x)=type$(x)
      Print exptype$(Abs(-2.1#))="Currency"
      Print exptype$(Abs(-2.1~))="Single"
      Print exptype$(Abs(-2.1@))="Decimal"
      Print exptype$(Abs(-2&))="Long"
      Print exptype$(Abs(-2%))="Integer"
      Print exptype$(Abs(-2.212e34))="Double"
      
      Print exptype$(Sgn(-2.1#))="Integer"
      \\ Sgn return integer type
      Print exptype$(Sgn(-2.212e34))="Integer"
      \\ Log, Len return double
      Print exptype$(Log(1000))="Double"
      Print exptype$(exp(1%))="Double"
      Print exptype$(Ln(1212%))="Double"
      \\ power return type Double
      Print exptype$(2&^2&)="Double"
      Print exptype$(2&**2&)="Double"
      Print exptype$(2&*2&)="Long"
      Print 2**2=4, 2^2=4, 2^2^2=16, 2**2**2=16
      \\ floor() and Int() is the same
      Print Int(-2.7)=-3, Int(2.7)=2
      Print Floor(-2.7)=-3, Floor(2.7)=2
      Print Ceil(-2.7)=-2, Ceil(2.7)=3
      
      Print Sqrt(4)=2
}
Checkit

```





## Maple


```Maple>
 abs(ceil(floor(ln(exp(1)^sqrt(exp(Pi*I)+1)))));      
                                   0
```



## Mathematica


```Mathematica
E
Pi
Sqrt[x]
Log[x]
Log[b,x]
Exp[x]
Abs[x]
Floor[x]
Ceiling[x]
Power[x, y]
```

Where x is the number, and b the base.
Exp[x] can also be inputted as E^x or E<sup>x</sup> and Power[x,y] can be also inputted as x^y or x<sup>y</sup>. All functions work with symbols, integers, floats and can be complex. Abs giving the modulus (|x|) if the argument is a complex number. Constant like E and Pi are kep unevaluated until someone explicitly tells it to give a numerical approximation: N[Pi,n] gives Pi to n-digit precision. Functions given an exact argument will be kept unevaluated if the answer can't be written more compact, approximate arguments will always be evaluated:

```Mathematica
Log[1.23] => 0.207014
Log[10] => Log[10]
Log[10,100] => 2
Log[E^4] => 4
Log[1 + I] => Log[1+I]
Log[1. + I] => 0.346574 + 0.785398 I
Ceiling[Pi] => 4
Floor[Pi] => 3
Sqrt[2] => Sqrt[2]
Sqrt[4] => 2
Sqrt[9/2] => 3/Sqrt[2]
Sqrt[3.5] => 1.87083
Sqrt[-5 + 12 I] => 2 + 3 I
Sqrt[-4] => 2I
Exp[2] => E^2
Exp[Log[4]] => 4
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
exp(1)    % e
pi        % pi
sqrt(x)   % square root
log(x)    % natural logarithm
log2(x)   % logarithm base 2 
log10(x)  % logarithm base 10 
exp(x)    % exponential
abs(-x)   % absolute value
floor(x)  % floor
ceil(x)   % ceiling
x^y       % power
```



## MAXScript


```maxscript
e       -- Euler's number
pi      -- pi
log x   -- natural logarithm
log10 x -- log base 10
exp x   -- exponantial
abs x   -- absolute value
floor x -- floor
ceil x  -- ceiling
pow x y -- power
```



## Mercury

<lang>
math.pi         % Pi.
math.e          % Euler's number.
math.sqrt(X)    % Square root of X.
math.ln(X)      % Natural logarithm of X.
math.log10(X)   % Logarithm to the base 10 of X.
math.log2(X)    % Logarithm to the base 2 of X.
math.log(B, X)  % Logarithm to the base B of X.
math.exp(X)     % e raised to the power of X.
float.abs(X)    % Absolute value of X.
math.floor(X)   % Floor of X.
math.ceiling(X) % Ceiling of X.
math.pow(X, Y)  % X raised to the power of Y.
```



## Metafont


```metafont
show mexp(256);   % outputs e; since MF uses mexp(x) = exp(x/256)
show 3.14159;     % no pi constant built in; of course we can define it
                  % in several ways... even computing
                  % C/2r (which would be funny since MF handles paths,
                  % and a circle is a path...)
show sqrt2;       % 1.41422, or in general sqrt(a)
show mexp(256*x); % see e.
show abs(x);      % returns |x| (the absolute value of the number x, or
                  % the length of the vector x); it is the same as
                  % length(x); plain Metafont in fact says:
                  % let abs = length;
show floor(x);    % floor
show ceiling(x);  % ceiling
show x**y;        % ** is not a built in: it is defined in the basic macros
                  % set for Metafont (plain Metafont) as a primarydef
```



## min

{{works with|min|0.19.3}}

```min
e       ; e
pi      ; π
sqrt    ; square root
log10   ; common logarithm
log2    ; binary logarithm
        ; no exponential
        ; no absolute value
floor   ; greatest whole number smaller than or equal
ceil    ; smallest whole number greater than or equal
trunc   ; remove the fractional part (i.e. round towards 0)
round   ; round number to nth decimal place
pow     ; power
```


=={{header|МК-61/52}}==
<lang>1	e^x	С/П

пи	С/П

КвКор	С/П

lg	С/П

e^x	С/П

|x|	С/П

П0	^	[x]	П1	-	x=0	09	ИП0	С/П	ЗН
x>=0	14	ИП1	С/П	ИП1	1	-	С/П

П0	^	[x]	П1	-	x=0	09	ИП0	С/П	ЗН
x<0	14	ИП1	С/П	ИП1	1	+	С/П

x^y	С/П
```


=={{header|Modula-3}}==
Modula-3 uses a module that is a wrapper around [[C]]'s <tt>math.h</tt>.

Note that all of these procedures (except the built ins) take <tt>LONGREAL</tt>s as their argument, and return <tt>LONGREAL</tt>s.

```modula3
Math.E;
Math.Pi;
Math.sqrt(x);
Math.log(x);
Math.exp(x);
ABS(x); (* Built in function. *)
FLOOR(x); (* Built in function. *)
CEILING(x); (* Built in function. *)
Math.pow(x, y);
```



## Neko


```ActionScript
/**
 Real constants and functions, in Neko
 Tectonics:
   nekoc real-constants.neko
   neko real-constants
*/

var euler = $loader.loadprim("std@math_exp", 1)(1)
var pi = $loader.loadprim("std@math_pi", 0)()

var math_sqrt = $loader.loadprim("std@math_sqrt", 1)
var math_log = $loader.loadprim("std@math_log", 1)
var math_exp = $loader.loadprim("std@math_exp", 1)
var math_abs = $loader.loadprim("std@math_abs", 1)
var math_floor = $loader.loadprim("std@math_floor", 1)
var math_ceil = $loader.loadprim("std@math_ceil", 1)
var math_pow = $loader.loadprim("std@math_pow", 2)

$print("Euler      : ", euler, "\n")
$print("Pi         : ", pi, "\n")

$print("Sqrt(2)    : ", math_sqrt(2), "\n")
$print("Log(10)    : ", math_log(10), "\n")
$print("Exp(1)     : ", math_pow(euler, 1), "\n")
$print("Abs(-2.2)  : ", math_abs(-2.2), "\n")
$print("Floor(-2.2): ", math_floor(-2.2), "\n")
$print("Ceil(-2.2) : ", math_ceil(-2.2), "\n")
$print("Pow(2, 8)  : ", math_pow(2, 8), "\n")
```


{{out}}

```txt
prompt$ nekoc real-contstants.neko
prompt$ neko real-contstants.n
Euler      : 2.71828182845905
Pi         : 3.14159265358979
Sqrt(2)    : 1.4142135623731
Log(10)    : 2.30258509299405
Exp(1)     : 2.71828182845905
Abs(-2.2)  : 2.2
Floor(-2.2): -3
Ceil(-2.2) : -2
Pow(2, 8)  : 256
```



## NetRexx

All the required constants and functions (and more) are in [[Java|Java's]] <tt>Math</tt> class.  NetRexx also provides a limited set of built in numeric manipulation functions for it's Rexx object.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary utf8

numeric digits 30

x = 2.5
y = 3
pad = 40
say
say 'Java Math constants & functions:'
say Rexx('  Euler''s number (e):').left(pad)                    Math.E
say Rexx('  Pi:').left(pad)                                     Math.PI
say Rexx('  Square root of' x':').left(pad)                     Math.sqrt(x)
say Rexx('  Log(e) of' x':').left(pad)                          Math.log(x)
say Rexx('  Log(e) of e:').left(pad)                            Math.log(Math.E)
say Rexx('  Log(10) of' x':').left(pad)                         Math.log10(x)
say Rexx('  Log(10) of 10:').left(pad)                          Math.log10(10)
say Rexx('  Exponential (e**x) of' x':').left(pad)              Math.exp(x)
say Rexx('  Exponential (e**x) of log(e)' x':').left(pad)       Math.exp(Math.log(x))
say Rexx('  Abs of' x':').left(pad)                             Math.abs(x.todouble)
say Rexx('  Abs of' (-x)':').left(pad)                          Math.abs((-x).todouble)
say Rexx('  Floor of' x':').left(pad)                           Math.floor(x)
say Rexx('  Floor of' (-x)':').left(pad)                        Math.floor((-x))
say Rexx('  Ceiling of' x':').left(pad)                         Math.ceil(x)
say Rexx('  Ceiling of' (-x)':').left(pad)                      Math.ceil((-x))
say Rexx(' ' x 'to the power of' y':').left(pad)                Math.pow(x, y)
say Rexx(' ' x 'to the power of' 1 / y':').left(pad)            Math.pow(x, 1 / y)
say Rexx('  10 to the power of log10' x':').left(pad)           Math.pow(10, Math.log10(x))

-- Extras
say Rexx('  Cube root of' x':').left(pad)                       Math.cbrt(x)
say Rexx('  Hypotenuse of' 3 'x' 4 'right triangle:').left(pad) Math.hypot(3, 4) 
say Rexx('  Max of' (-x) '&' x':').left(pad)                    Math.max((-x).todouble, x)
say Rexx('  Min of' (-x) '&' x':').left(pad)                    Math.min((-x).todouble, x)
say Rexx('  Signum of' x':').left(pad)                          Math.signum((x).todouble)
say Rexx('  Signum of' x '-' x':').left(pad)                    Math.signum((x - x).todouble)
say Rexx('  Signum of' (-x)':').left(pad)                       Math.signum((-x).todouble)

say
say 'NetRexx built-in support for numeric data:'
say Rexx('  Abs of' x':').left(pad)                        x.abs()
say Rexx('  Abs of' (-x)':').left(pad)                     (-x).abs()
say Rexx('  Sign of' x':').left(pad)                       x.sign()
say Rexx('  Sign of' x '-' x':').left(pad)                 (x - x).sign()
say Rexx('  Sign of' (-x)':').left(pad)                    (-x).sign()
say Rexx('  Max of' (-x) '&' x':').left(pad)               (-x).max(x)
say Rexx('  Min of' (-x) '&' x':').left(pad)               (-x).min(x)
say Rexx('  Truncate' x 'by' y':').left(pad)               x.trunc(y)
say Rexx('  Format (with rounding)' x 'by' y':').left(pad) x.format(y, 0)

```


{{out}}

```txt

Java Math constants & functions:
  Euler's number (e):                    2.718281828459045
  Pi:                                    3.141592653589793
  Square root of 2.5:                    1.58113883008419
  Log(e) of 2.5:                         0.9162907318741551
  Log(e) of e:                           1
  Log(10) of 2.5:                        0.3979400086720376
  Log(10) of 10:                         1
  Exponential (e**x) of 2.5:             12.18249396070347
  Exponential (e**x) of log(e) 2.5:      2.5
  Abs of 2.5:                            2.5
  Abs of -2.5:                           2.5
  Floor of 2.5:                          2
  Floor of -2.5:                         -3
  Ceiling of 2.5:                        3
  Ceiling of -2.5:                       -2
  2.5 to the power of 3:                 15.625
  2.5 to the power of 0.3333333333333333 1.357208808297453
  10 to the power of log10 2.5:          2.5
  Cube root of 2.5:                      1.357208808297453
  Hypotenuse of 3 x 4 right triangle:    5
  Max of -2.5 & 2.5:                     2.5
  Min of -2.5 & 2.5:                     -2.5
  Signum of 2.5:                         1
  Signum of 2.5 - 2.5:                   0
  Signum of -2.5:                        -1

NetRexx built-in support for numeric data:
  Abs of 2.5:                            2.5
  Abs of -2.5:                           2.5
  Sign of 2.5:                           1
  Sign of 2.5 - 2.5:                     0
  Sign of -2.5:                          -1
  Max of -2.5 & 2.5:                     2.5
  Min of -2.5 & 2.5:                     -2.5
  Truncate 2.5 by 3:                     2.500
  Format (with rounding) 2.5 by 3:         3

```



## Nim


```nim
import math

var x, y = 12.5

echo E
echo Pii
echo sqrt(x)
echo ln(x)
echo log10(x)
echo exp(x)
echo abs(x)
echo floor(x)
echo ceil(x)
echo pow(x, y)
```



## Objeck


```objeck
Float->Pi();
Float->E();
4.0->SquareRoot();
1.5->Log();
# exponential is not supported
3.99->Abs();
3.99->Floor();
3.99->Ceiling();
4.5->Ceiling(2.0);
```



## OCaml

Unless otherwise noted, the following functions are for floats only:

```ocaml
sqrt x      (* square root *)
log x       (* natural logarithm--log base 10 also available (log10) *)
exp x       (* exponential *)
abs_float x (* absolute value *)
abs x       (* absolute value (for integers) *)
floor x     (* floor *)
ceil x      (* ceiling *)
x ** y      (* power *)
-. x        (* negation for floats *)
```



## Octave


```octave
e         % e
pi        % pi
sqrt(pi)  % square root
log(e)    % natural logarithm
exp(pi)   % exponential
abs(-e)   % absolute value
floor(pi) % floor
ceil(pi)  % ceiling
e**pi     % power
```



## Oforth



```Oforth
import: math

: testReal
   E println
   Pi println
   9 sqrt println
   2 ln println
   2 exp println
   -3.4 abs println
   3.4 exp println

   2.4 floor println
   3.9 floor println
   5.5 floor println
  -2.4 floor println
  -3.9 floor println
  -5.5 floor println

   2.4 ceil println
   3.9 ceil println
   5.5 ceil println
  -2.4 ceil println
  -3.9 ceil println
  -5.5 ceil println ;
```



## ooRexx

{{trans|NetRexx}}
{{uses from|OoRexx|RxMath}}

```ooRexx
/* Rexx */

-- MathLoadFuncs & MathDropFuncs are no longer needed and are effectively NOPs
-- but MathLoadFuncs does return its copyright statement when given a string argument
RxMathCopyright = MathLoadFuncs('')
say RxMathCopyright

numeric digits 16

x = 2.5
y = 3
pad = 40
digs = digits()
say
say 'Working with precision' digs
say 'Math constants & functions:'
say ('  Euler''s number (e):')~left(pad)                    RxCalcExp(1, digs)
say ('  Pi:')~left(pad)                                     RxCalcPi(digs)
say ('  Square root of' x':')~left(pad)                     RxCalcSqrt(x, digs)
say ('  Log(e) of' x':')~left(pad)                          RxCalcLog(x, digs)
say ('  Log(e) of e:')~left(pad)                            RxCalcLog(RxCalcExp(1, digs), digs)
say ('  Log(10) of' x':')~left(pad)                         RxCalcLog10(x, digs)
say ('  Log(10) of 10:')~left(pad)                          RxCalcLog10(10, digs)
say ('  Exponential (e**x) of' x':')~left(pad)              RxCalcExp(x, digs)
say ('  Exponential (e**x) of log(e)' x':')~left(pad)       RxCalcExp(RxCalcLog(x, digs), digs)
say (' ' x 'to the power of' y':')~left(pad)                RxCalcPower(x, y, digs)
say (' ' x 'to the power of 1/'y':')~left(pad)              RxCalcPower(x, 1 / y, digs)
say ('  10 to the power of log10' x':')~left(pad)           RxCalcPower(10, RxCalcLog10(x), digs)

say
say 'Rexx built-in support for numeric data:'
say ('  Abs of' x':')~left(pad)                             x~abs()
say ('  Abs of' (-x)':')~left(pad)                          (-x)~abs()
say ('  Sign of' x':')~left(pad)                            x~sign()
say ('  Sign of' x '-' x':')~left(pad)                      (x - x)~sign()
say ('  Sign of' (-x)':')~left(pad)                         (-x)~sign()
say ('  Max of' (-x) '&' x':')~left(pad)                    (-x)~max(x)
say ('  Min of' (-x) '&' x':')~left(pad)                    (-x)~min(x)
say ('  Truncate' x 'by' y':')~left(pad)                    x~trunc(y)
say ('  Format (with rounding)' x 'by' y':')~left(pad)      x~format(y, 0)

say
say 'Use RYO functions for floor & ceiling:'
say ('  Floor of' x':')~left(pad)                           floor(x)
say ('  Floor of' (-x)':')~left(pad)                        floor((-x))
say ('  Ceiling of' x':')~left(pad)                         ceiling(x)
say ('  Ceiling of' (-x)':')~left(pad)                      ceiling((-x))

return

-- floor and ceiling functions are not part of ooRexx
floor: procedure
  return arg(1)~trunc() - (arg(1) < 0) * (arg(1) \= arg(1)~trunc())

ceiling: procedure
  return arg(1)~trunc() + (arg(1) > 0) * (arg(1) \= arg(1)~trunc())

::requires 'RxMath' library
```

{{out}}

```txt

rxmath 1.1 - REXX mathematical function package
(c) Copyright RexxLanguage Association 2005.
All Rights Reserved.



Working with precision 16
Math constants & functions:
  Euler's number (e):                    2.718281828459045
  Pi:                                    3.141592653589793
  Square root of 2.5:                    1.581138830084190
  Log(e) of 2.5:                         0.9162907318741551
  Log(e) of e:                           1
  Log(10) of 2.5:                        0.3979400086720376
  Log(10) of 10:                         1
  Exponential (e**x) of 2.5:             12.18249396070347
  Exponential (e**x) of log(e) 2.5:      2.5
  2.5 to the power of 3:                 15.625
  2.5 to the power of 1/3:               1.357208808297453
  10 to the power of log10 2.5:          2.5

Rexx built-in support for numeric data:
  Abs of 2.5:                            2.5
  Abs of -2.5:                           2.5
  Sign of 2.5:                           1
  Sign of 2.5 - 2.5:                     0
  Sign of -2.5:                          -1
  Max of -2.5 & 2.5:                     2.5
  Min of -2.5 & 2.5:                     -2.5
  Truncate 2.5 by 3:                     2.500
  Format (with rounding) 2.5 by 3:         3

Use RYO functions for floor & ceiling:
  Floor of 2.5:                          2
  Floor of -2.5:                         -3
  Ceiling of 2.5:                        3
  Ceiling of -2.5:                       -2

```



## Oz


```oz
{ForAll
 [
  {Exp 1.}           %% 2.7183   Euler's number: not predefined
  4. * {Atan2 1. 1.} %% 3.1416   pi: not predefined
  {Sqrt 81.}         %% 9.0      square root; expects a float
  {Log 2.7183}       %% 1.0      natural logarithm
  {Abs ~1}           %% 1        absolute value; expects a float or an integer
  {Floor 1.999}      %% 1.0      floor; expects and returns a float
  {Ceil 1.999}       %% 2.0      ceiling; expects and returns a float
  {Pow 2 3}          %% 8        power; both arguments must be of the same type
 ]
 Show}
```



## PARI/GP


```parigp
[exp(1), Pi, sqrt(2), log(2), abs(2), floor(2), ceil(2), 2^3]
```



## Pascal

See [[Real_constants_and_functions#Delphi | Delphi]]


## Perl


```perl
use POSIX; # for floor() and ceil()

exp(1); # e
4 * atan2(1, 1); # pi
sqrt($x); # square root
log($x); # natural logarithm; log10() available in POSIX module
exp($x); # exponential
abs($x); # absolute value
floor($x); # floor
ceil($x); # ceiling
$x ** $y; # power

use Math::Trig;
pi; # alternate way to get pi

use Math::Complex;
pi; # alternate way to get pi
```



## Perl 6


```perl6
say e;            # e
say π; # or pi    # pi
say τ; # or tau   # tau

# Common mathmatical function are availble
# as subroutines and as numeric methods.
# It is a matter of personal taste and
# programming style as to which is used.
say sqrt 2;       # Square root
say 2.sqrt;       # Square root

# If you omit a base, does natural logarithm
say log 2;        # Natural logarithm
say 2.log;        # Natural logarithm

# Specify a base if other than e
say log 4, 10;    # Base 10 logarithm
say 4.log(10);    # Base 10 logarithm
say 4.log10;      # Convenience, base 10 only logarithm

say exp 7;        # Exponentiation base e
say 7.exp;        # Exponentiation base e

# Specify a base if other than e
say exp 7, 4;     # Exponentiation
say 7.exp(4);     # Exponentiation
say 4 ** 7;       # Exponentiation

say abs -2;       # Absolute value
say (-2).abs;     # Absolute value

say floor -3.5;   # Floor
say (-3.5).floor; # Floor

say ceiling pi;   # Ceiling
say pi.ceiling;   # Ceiling

say e ** π\i + 1 ≅ 0; # :-)
```



## Phix


```Phix
?E                  -- Euler number
?PI                 -- pi
?log(E)             -- natural logarithm
?log10(10)          -- base 10 logarithm
?exp(log(5))        -- exponential
?sqrt(5)            -- square root
?abs(-1.2)          -- absolute value
?floor(-1.2)        -- floor,    -2
?ceil(-1.2)         -- ceiling,  -1
?round(-1.8)        -- rounded,  -2
?trunc(-1.8)        -- truncate, -1
?power(E,log(5))    -- displays 5.0
?power(10,log10(5)) -- displays 5.0
?INVLN10            -- displays 0.434..
?exp(1/INVLN10)     -- displays 10.0
```



## PHP


```php
M_E; //e
M_PI; //pi
sqrt(x); //square root
log(x); //natural logarithm--log base 10 also available (log10)
exp(x); //exponential
abs(x); //absolute value
floor(x); //floor
ceil(x); //ceiling
pow(x,y); //power
```



## PL/I


```pli
/* e  not available other than by using exp(1q0).*/
/* pi not available other than by using a trig function such as: pi=4*atan(1) */
y = sqrt(x);
y = log(x);
y = log2(x);
y = log10(x);
y = exp(x);
y = abs(x);
y = floor(x);
y = ceil(x);
a = x**y;      /* power */
/* extra functions: */
y = erf(x);    /* the error function. */
y = erfc(x);   /* the error function complemented. */
y = gamma (x);
y = loggamma (x);
```



## PicoLisp

PicoLisp has only limited floating point support (scaled bignum arithmetics). It
can handle real numbers with as many positions after the decimal point as
desired, but is practically limited by the precision of the C-library functions
(about 16 digits). The default precision is six, and can be changed with
'[http://software-lab.de/doc/refS.html#scl scl]':

```PicoLisp
(scl 12)  # 12 places after decimal point
(load "@lib/math.l")

(prinl (format (exp 1.0) *Scl))        # e, exp
(prinl (format pi *Scl))               # pi

(prinl (format (pow 2.0 0.5) *Scl))    # sqare root
(prinl (format (sqrt 2.0 1.0) *Scl))

(prinl (format (log 2.0) *Scl))        # logarithm
(prinl (format (exp 4.0) *Scl))        # exponential

(prinl (format (abs -7.2) *Scl))       # absolute value
(prinl (abs -123))

(prinl (format (pow 3.0 4.0) *Scl))    # power
```

{{out}}

```txt
2.718281828459
3.141592653590
1.414213562373
1.414213562373
0.693147180560
54.598150033144
7.200000000000
123
81.000000000000
```



## Pop11


```pop11
pi        ;;; Number Pi
sqrt(x)   ;;; Square root
log(x)    ;;; Natural logarithm
exp(x)    ;;; Exponential function
abs(x)    ;;; Absolute value
x ** y    ;;; x to the power y
```


The number e is not provided directly, one has to compute 'exp(1)'
instead.  Also, f/math>)

See also [[Trigonometric Functions]]


## ACL2

Only the last three are available as built in functions.

loor and ceiling are not provided, one can
define them using integer part:


```pop11
define floor(x);
    if x < 0 then
        -intof(x);
    else
        intof(x);
    endif;
enddefine;

define ceiling(x);
    -floor(-x);
enddefine;
```



## PowerShell

Since PowerShell has access to .NET all this can be achieved using the .NET Base Class Library:

```powershell
Write-Host ([Math]::E)
Write-Host ([Math]::Pi)
Write-Host ([Math]::Sqrt(2))
Write-Host ([Math]::Log(2))
Write-Host ([Math]::Exp(2))
Write-Host ([Math]::Abs(-2))
Write-Host ([Math]::Floor(3.14))
Write-Host ([Math]::Ceiling(3.14))
Write-Host ([Math]::Pow(2, 3))
```



## PureBasic


```PureBasic
Debug #E
Debug #PI 
Debug Sqr(f)
Debug Log(f)
Debug Exp(f)
Debug Log10(f)
Debug Abs(f)
Debug Pow(f,f)
```



## Python


```python
import math

math.e          # e
math.pi         # pi
math.sqrt(x)    # square root  (Also commonly seen as x ** 0.5 to obviate importing the math module)
math.log(x)     # natural logarithm
math.log10(x)   # base 10 logarithm
math.exp(x)     # e raised to the power of x
abs(x)          # absolute value
math.floor(x)   # floor
math.ceil(x)    # ceiling
x ** y          # exponentiation 
pow(x, y[, n])  # exponentiation [, modulo n (useful in certain encryption/decryption algorithms)]

# The math module constants and functions can, of course, be imported directly by:
#   from math import e, pi, sqrt, log, log10, exp, floor, ceil
```



## R


```R
exp(1)             # e
pi                 # pi
sqrt(x)            # square root
log(x)             # natural logarithm
log10(x)           # base 10 logarithm
log(x, y)          # arbitrary base logarithm
exp(x)             # exponential
abs(x)             # absolute value
floor(x)           # floor
ceiling(x)         # ceiling
x^y                # power
```



## Racket


```racket
(exp 1)         ; e
pi              ; pi
(sqrt x)        ; square root
(log x)         ; natural logarithm
(exp x)         ; exponential
(abs x)         ; absolute value
(floor x)       ; floor
(ceiling x)     ; ceiling
(expt x y)      ; power
```



## REXX

REXX has no built-in functions for trig functions, square root, pi, exponential ('''e''' raised to a power), logarithms and other similar functions.  

REXX doesn't have any built-in (math) constants.

### abs


```rexx
a=abs(y)                       /*takes the absolute value of y.*/
```

===exponentiation (**)===

```rexx
r=x**y                         /*REXX only supports integer powers.*/
                               /*Y may be negative, zero, positive.*/
                               /*X may be any real number.         */
```



### ceiling

A ceiling function for REXX:

```rexx

ceiling: procedure; parse arg x; t=trunc(x); return t+(x>0)*(x\=t)

```



### floor

A floor function for REXX:

```rexx

floor:   procedure; parse arg x; t=trunc(x); return t-(x<0)-(x\=t)

```


===sqrt (optimized)===
A [principal] square root (SQRT) function for REXX   (with arbitrary precision):

```rexx
/*──────────────────────────────────SQRT subroutine───────────────────────────*/
sqrt: procedure;  parse arg x;         if x=0  then return 0  /*handle 0 case.*/
if \datatype(x,'N')  then return '[n/a]'   /*Not Applicable ───if not numeric.*/
i=;  if x<0  then do; x=-x; i='i'; end /*handle complex numbers if  X  is < 0.*/
d=digits()                             /*get the current numeric precision.   */
m.=9                                   /*technique uses just enough digits.   */
h=d+6                                  /*use extra decimal digits for accuracy*/
numeric digits 9                       /*use "small" precision at first.      */
numeric form                           /*force scientific form of the number. */
if fuzz()\==0  then numeric fuzz 0     /*just in case invoker has a FUZZ  set.*/
parse value format(x,2,1,,0)  'E0'  with  g 'E' _ .  /*get the  X's  exponent.*/
     g=(g * .5) || 'e' || (_ % 2)      /*1st guesstimate for the square root. */
  /* g= g * .5     'e'    (_ % 2) */   /*a shorter & concise version of above.*/
                                       /*Note: to insure enough accuracy for  */
                                       /*  the result, the precision during   */
                                       /*  the SQRT calculations is increased */
                                       /*  by two extra decimal digits.       */
  do j=0  while  h>9;  m.j=h;  h=h%2+1 /*compute the sizes (digs) of precision*/
  end   /*j*/                          /* [↑]  precisions are stored in  M.   */
                                       /*now, we start to do the heavy lifting*/
  do k=j+5  to 0  by -1                /*compute the  √  with increasing digs.*/
  numeric digits m.k                   /*each iteration, increase the digits. */
  g=(g+x/g) * .5                       /*perform the nitty-gritty calculations*/
  end   /*k*/                          /* [↑]  * .5   is faster than   / 2    */
                                       /* [↓]  normalize √ ──► original digits*/
numeric digits d                       /* [↓]  make answer complex if  X < 0. */
return (g/1)i                          /*normalize, and add possible I suffix.*/
```


```rexx
  ╔════════════════════════════════════════════════════════════════════╗
╔═╝                                __                                  ╚═╗
║                                 √                                      ║
║                                                                        ║
║ While the above REXX code seems like it's doing a lot of extra work,   ║
║ it saves a substantial amount of processing time when the precision    ║
║ (DIGITs)  is a lot greater than the default  (default is nine digits). ║
║                                                                        ║
║ Indeed, when computing square roots in the hundreds  (even thousands)  ║
║ of digits,  this technique reduces the amount of CPU processing time   ║
║ by keeping the length of the computations to a minimum (due to a large ║
║ precision),  while the accuracy at the beginning isn't important for   ║
║ calculating the (first) guesstimate  (the running square root guess).  ║
║                                                                        ║
║ Each iteration of   K   (approximately) doubles the number of digits,  ║
║ but takes almost four times longer to compute  (actually, around 3.8). ║
║                                                                        ║
║ The REXX code could be streamlined (pruned)  by removing  the          ║
║ The    NUMERIC FUZZ 0      statement can be removed  if  it is known   ║
║ that  it is already set to zero.  (which is the default).              ║
║                                                                        ║
║ Also, the   NUMERIC FORM   statement can be removed  if  it is known   ║
║ that the   form  is  SCIENTIFIC   (which is the default).              ║
║                                  __                                    ║
╚═╗                               √                                    ╔═╝
  ╚════════════════════════════════════════════════════════════════════╝
```


===sqrt (simple)===

```rexx
/*──────────────────────────────────SQRT subroutine─────────────────────*/
sqrt: procedure;  arg x                /*a simplistic  SQRT  subroutine.*/
if x=0  then return 0                  /*handle special case of zero.   */
d=digits()                             /*get the current precision (dig)*/
numeric digits d+2                     /*ensure extra precision (2 digs)*/
g=x/4                                  /*try get a so-so 1st guesstimate*/
old=0                                  /*set OLD guess to zero.         */
                  do forever           /*keep at it 'til  G (guess)=old.*/
                  g=(g+x/g) / 2        /*do the nitty-gritty calculation*/
                  if g=old  then leave /*if G is the same as old, quit. */
                  old=g                /*save OLD for next iteration.   */
                  end   /*forever*/    /* [↑] ···'til we run out of digs*/
numeric digits d                       /*restore the original precision.*/
return g/1                             /*normalize to old precision (d).*/
```



### other

Other mathematical-type functions supported are:

```rexx
numeric digits ddd               /*sets the current precision to  DDD   */
numeric fuzz   fff               /*arithmetic comparisons with FFF fuzzy*/
numeric form   kkk               /*exponential: scientific | engineering*/

low=min(a,b,c,d,e,f,g, ...)      /*finds the min of specified arguments.*/
big=min(a,b,c,d,e,f,g, ...)      /*finds the max of specified arguments.*/

rrr=random(low,high)             /*gets a random integer from LOW-->HIGH*/
arr=random(low,high,seed)        /* ... with a seed (to make repeatable)*/

mzp=sign(x)                      /*finds the sign of  x   (-1, 0, +1).  */

 fs=format(x)                    /*formats X  with the current DIGITS() */
 fb=format(x,bbb)                /*            BBB  digs  before decimal*/
 fa=format(x,bbb,aaa)            /*            AAA  digs  after  decimal*/
 fa=format(x,,0)                 /*            rounds  X  to an integer.*/
 fe=format(x,,eee)               /*            exponent has eee places. */
 ft=format(x,,eee,ttt)           /*if x exceeds TTT digits, force exp.  */

hh=b2x(bbb)                      /*converts binary/bits to hexadecimal. */
dd=c2d(ccc)                      /*converts character   to decimal.     */
hh=c2x(ccc)                      /*converts character   to hexadecimal. */
cc=d2c(ddd)                      /*converts decimal     to character.   */
hh=d2x(ddd)                      /*converts decimal     to hexadecimal. */
bb=x2b(hhh)                      /*converts hexadecimal to binary (bits)*/
cc=x2c(hhh)                      /*converts hexadecimal to character.   */
dd=x2d(hhh)                      /*converts hexadecimal to decimal.     */
```



## Ring


```ring

See "Mathematical Functions" + nl
See "Sin(0) = " + sin(0) + nl
See "Sin(90) radians = " + sin(90) + nl
See "Sin(90) degree = " + sin(90*3.14/180) + nl

See "Cos(0) = " + cos(0) + nl
See "Cos(90) radians = " + cos(90) + nl
See "Cos(90) degree = " + cos(90*3.14/180) + nl

See "Tan(0) = " + tan(0) + nl
See "Tan(90) radians = " + tan(90) + nl
See "Tan(90) degree = " + tan(90*3.14/180) + nl

See "asin(0) = " + asin(0) + nl
See "acos(0) = " + acos(0) + nl
See "atan(0) = " + atan(0) + nl
See "atan2(1,1) = " + atan2(1,1) + nl

See "sinh(0) = " + sinh(0) + nl
See "sinh(1) = " + sinh(1) + nl
See "cosh(0) = " + cosh(0) + nl
See "cosh(1) = " + cosh(1) + nl
See "tanh(0) = " + tanh(0) + nl
See "tanh(1) = " + tanh(1) + nl

See "exp(0) = " + exp(0) + nl
See "exp(1) = " + exp(1) + nl
See "log(1) = " + log(1) + nl
See "log(2) = " + log(2) + nl
See "log10(1) = " + log10(1) + nl
See "log10(2) = " + log10(2) + nl
See "log10(10) = " + log10(10) + nl

See "Ceil(1.12) = " + Ceil(1.12) + nl
See "Ceil(1.72) = " + Ceil(1.72) + nl

See "Floor(1.12) = " + floor(1.12) + nl
See "Floor(1.72) = " + floor(1.72) + nl

See "fabs(1.12) = " + fabs(1.12) + nl
See "fabs(1.72) = " + fabs(1.72) + nl

See "pow(2,3) = " + pow(2,3) + nl

see "sqrt(16) = " + sqrt(16) + nl

```



## RLaB



###  Mathematical Constants 


RLaB has a number of mathematical constants built-in within the list ''const''. These facilities are provided through the Gnu Science Library [[http://www.gnu.org/software/gsl]].

```RLaB>>
 const
   e                    euler           ln10            ln2             lnpi
   log10e               log2e           pi              pihalf          piquarter
   rpi                  sqrt2           sqrt2r          sqrt3           sqrtpi
   tworpi
```



###  Physical Constants 

Another list of physical constants and unit conversion factors exists and is called ''mks''. 
Here the conversion goes between that particular unit and the equivalent unit in, one and only, metric system.

```RLaB>>
 mks
   F                    G               J               L               N
   Na                   R0              Ry              Tsp             V0
   a                    a0              acre            alpha           atm
   au                   bar             barn            btu             c
   cal                  cgal            cm              cm2             cm3
   ct                   cup             curie           day             dm
   dm2                  dm3             dyne            e               eV
   eps0                 erg             fathom          floz            ft
   ftcan                ftlam           g               gal             gauss
   gf                   h               ha              hbar            hour
   hp                   in              inH2O           inHg            kSB
   kb                   kcal            km              km2             km3
   kmh                  knot            kpf             lam             lb
   lumen                lux             ly              mHg             mSun
   me                   micron          mil             mile            min
   mm                   mm2             mm3             mmu             mn
   mp                   mph             mu0             mub             mue
   mun                  mup             nmi             oz              pal
   parsec               pf              phot            poise           psi
   rad                  roe             stilb           stokes          tcs
   therm                tntton          ton             torr            toz
   tsp                  uam             ukgal           ukton           uston
   week                 yd
```



###  Elementary Functions 


```RLaB>>
 x = rand()
>> sqrt(x)
  2.23606798
>> log(x)
  1.60943791
>> log10(x)
 0.698970004
>> exp(x)
  148.413159
>> abs(x)
  5
>> floor(x)
  5
>> ceil(x)
  5
>> x .^ 2
  25
```



## Ruby


```ruby
x.abs #absolute value
x.magnitude #absolute value
x.floor #floor
x.ceil #ceiling
x ** y #power
include Math
E #e
PI #pi
sqrt(x) #square root
log(x) #natural logarithm
log(x, y) #logarithm base y
log10(x) #base 10 logarithm
exp(x) #exponential

```



## Run BASIC


```runbasic
print "exp:";chr$(9);   EXP(1)
print "PI:";chr$(9);    22/7
print "Sqr2:";chr$(9);  SQR(2)
print "Log2:";chr$(9);  LOG(2) : REM Base 10
print "Exp2:";chr$(9);  EXP(2)
print "Abs2:";chr$(9);  ABS(-2)
print "Floor:";chr$(9); INT(1.534)
print "ceil:";chr$(9);  val(using("###",1.534))
print "Power:";chr$(9); 1.23^4
```


```txt
exp:	2.71828183
PI:	3.14285707
Sqr2:	1.41421356
Log2:	0.693147181
Exp2:	7.3890561
Abs2:	2
Floor:	1
ceil:	2
Power:	2.28886641
```




## Rust


```rust
use std::f64::consts::*;

fn main() {
    // e (base of the natural logarithm)
    let mut x = E;
    // π
    x += PI;
    // square root
    x = x.sqrt();
    // logarithm (any base allowed)
    x = x.ln();
    // ceiling (smallest integer not less than this number--not the same as round up)
    x = x.ceil();
    // exponential (ex)
    x = x.exp();
    // absolute value (a.k.a. "magnitude")
    x = x.abs();
    // floor (largest integer less than or equal to this number--not the same as truncate or int)
    x = x.floor();
    // power (xy) 
    x = x.powf(x);

    assert_eq!(x, 4.0);
}
```



## Scala


```scala
object RealConstantsFunctions extends App{
  println(math.E)                // e
  println(math.Pi)               // pi
  println(math.sqrt(2.0))        // square root
  println(math.log(math.E))      // log to base e
  println(math.log10(10.0))      // log to base 10
  println(math.exp(1.0))         // exponential
  println(math.abs(-1))          // absolute value
  println(math.floor(-2.5))      // floor
  println(math.ceil(-2.5))       // ceiling
  println(math.pow(2.5, 3.5))    // power
}
```


## Scheme


```scheme
(sqrt x) ;square root
(log x) ;natural logarithm
(exp x) ;exponential
(abs x) ;absolute value
(floor x) ;floor
(ceiling x) ;ceiling
(expt x y) ;power
```



## Seed7

The [http://seed7.sourceforge.net/libraries/math.htm math.s7i] library defines:
{| class="wikitable" style="text-align:left"
| [http://seed7.sourceforge.net/libraries/math.htm#E E] || # e (Euler's number)
|-
| [http://seed7.sourceforge.net/libraries/math.htm#PI PI] || # Pi
|-
| [http://seed7.sourceforge.net/libraries/math.htm#sqrt%28ref_float%29 sqrt(x)] || # square root
|-
| [http://seed7.sourceforge.net/libraries/math.htm#log%28ref_float%29 log(x)] || # natural logarithm - log base 10 is also available: [http://seed7.sourceforge.net/libraries/math.htm#log10%28ref_float%29 log10(x)])
|-
| [http://seed7.sourceforge.net/libraries/math.htm#exp%28ref_float%29 exp(x)] || # exponential
|-
| [http://seed7.sourceforge.net/libraries/math.htm#abs%28ref_float%29 abs(x)] || # absolute value
|-
| [http://seed7.sourceforge.net/libraries/math.htm#floor%28ref_float%29 floor(x)] || # floor
|-
| [http://seed7.sourceforge.net/libraries/math.htm#ceil%28ref_float%29 ceil(x)] || # ceiling
|}

The [http://seed7.sourceforge.net/libraries/float.htm float.s7i] library defines:
{| class="wikitable" style="text-align:left"
| [http://seed7.sourceforge.net/libraries/float.htm#%28ref_float%29**%28ref_integer%29 x ** y] || # power with [http://seed7.sourceforge.net/libraries/integer.htm integer] exponent
|-
| [http://seed7.sourceforge.net/libraries/float.htm#%28ref_float%29**%28ref_float%29 x ** y] || # power with float exponent
|}


## Sidef


```ruby
Num.e     # e
Num.pi    # pi
x.sqrt    # square root
x.log     # natural logarithm
x.log10   # base 10 logarithm
x.exp     # e raised to the power of x
x.abs     # absolute value
x.floor   # floor
x.ceil    # ceiling
x**y      # exponentiation
```



## Slate


```slate
numerics E.
numerics Pi.
n sqrt.
n log10. "base 10 logarithm"
n ln. "natural logarithm"
n log: m. "arbitrary base logarithm"
n exp. "exponential"
n abs. "absolute value"
n floor. 
n ceiling.
n raisedTo: anotherNumber
```



## Smalltalk


```smalltalk
Float e.
Float pi.
aNumber sqrt.
aNumber log. "base 10 logarithm"
aNumber ln. "natural logarithm"
aNumber exp. "exponential"
aNumber abs. "absolute value"
aNumber floor. 
aNumber ceiling.
aNumber raisedTo: anotherNumber
```



## Sparkling


```sparkling
// e:
print(M_E);

// π:
print(M_PI);

// square root:
let five = sqrt(25);

// logarithm
// natural:
let one = log(M_E);
// base-2:
let six = log2(64);
// base-10
let three = log10(1000);

// exponential
let e_cubed = exp(3);

// absolute value
let ten = abs(-10);

// floor
let seven = floor(7.8);

// ceiling
let four = ceil(3.2);

// power
let eighty_one = pow(3, 4);
```



## Standard ML


```sml
Math.e; (* e *)
Math.pi; (* pi *)
Math.sqrt x; (* square root *)
Math.ln x; (* natural logarithm--log base 10 also available (Math.log10) *)
Math.exp x; (* exponential *)
abs x; (* absolute value *)
floor x; (* floor *)
ceil x; (* ceiling *)
Math.pow (x, y); (* power *)
~ x; (* negation *)
```



## Stata


```stata
scalar x=2
scalar y=3
di exp(1)
di _pi
di c(pi)
di sqrt(x)
di log(x)
di log10(x)
di exp(x)
di abs(x)
di floor(x)
di ceil(x)
di x^y
```



## Swift


```swift
import Darwin

M_E // e
M_PI // pi
sqrt(x) // square root--cube root also available (cbrt)
log(x) // natural logarithm--log base 10 also available (log10)
exp(x) // exponential
abs(x) // absolute value
floor(x) // floor
ceil(x) // ceiling
pow(x,y) // power
```



## Tcl


```tcl
expr {exp(1)}       ;# e
expr {4 * atan(1)}  ;# pi -- also, simpler: expr acos(-1)
expr {sqrt($x)}     ;# square root
expr {log($x)}      ;# natural logarithm, also log10
expr {exp($x)}      ;# exponential
expr {abs($x)}      ;# absolute value
expr {floor($x)}    ;# floor
expr {ceil($x)}     ;# ceiling
expr {$x**$y}       ;# power, also pow($x,$y)
```

The constants <math>e</math> and <math>\pi</math> are also available with high precision in a support library.
{{tcllib|math::constants}}

```tcl
package require math::constants
math::constants::constants e pi
puts "e = $e, pi = $pi"
```


=={{header|TI-89 BASIC}}==
{|
|-
! Mathematical !! TI-89 !! Notes
|-
| <math>e               </math> || <code style="font-family:'TI Uni'">ℯ         </code> || (U+212F SCRIPT SMALL E)
|-
| <math>\pi             </math> || <code style="font-family:'TI Uni'">π         </code> || (U+03C0 GREEK SMALL LETTER PI)
|-
| <math>\sqrt{x}        </math> || <code style="font-family:'TI Uni'">√(x)      </code> || (U+221A SQUARE ROOT)
|-
| <math>\log_e(x)       </math> || <code style="font-family:'TI Uni'">ln(x)     </code>
|-
| <math>\log_{10}(x)    </math> || <code style="font-family:'TI Uni'">log(x)    </code>
|-
| <math>\log_b(x)       </math> || <code style="font-family:'TI Uni'">log(b, x) </code> || The optional base argument comes ''first''
|-
| <math>\lfloor x\rfloor</math> || <code style="font-family:'TI Uni'">floor(x)  </code>
|-
| <math>\lceil x\rceil  </math> || <code style="font-family:'TI Uni'">ceiling(x)</code>
|-
| <math>x^y             </math> || <code style="font-family:'TI Uni'">x^y       </code>
|}


## UNIX Shell

{{works with|ksh93}}
ksh93 exposes math functions from the C math library

```bash
echo $(( exp(1) ))      # e
echo $(( acos(-1) ))    # PI
x=5  
echo $(( sqrt(x) ))     # square root
echo $(( log(x) ))      # logarithm base e
echo $(( log2(x) ))     # logarithm base 2
echo $(( log10(x) ))    # logarithm base 10
echo $(( exp(x) ))      # exponential
x=-42 
echo $(( abs(x) ))      # absolute value
x=-5.5 
echo $(( floor(x) ))    # floor
echo $(( ceil(x) ))     # ceiling
x=10 y=3 
echo $(( pow(x,y) ))    # power
```


{{out}}

```txt
2.71828182845904524
3.14159265358979324
2.2360679774997897
1.60943791243410037
2.32192809488736235
0.698970004336018805
148.413159102576603
42
-6
-5
1000
```
 


## XPL0


```XPL0
include c:\cxpl\codes;                          \intrinsic 'code' declarations

func real Power(X, Y);  \X raised to the Y power
real X, Y;
return Exp(Y*Ln(X));

real E, Pi;
[Format(4, 16);                                 \places shown before and after .
E:= Exp(1.0);
RlOut(0, E);  CrLf(0);
RlOut(0, Ln(E));  CrLf(0);
CrLf(0);
Pi:= ATan2(0.0, -1.0);                          \Pi is also a defined constant
RlOut(0, Pi);  CrLf(0);
RlOut(0, Cos(Pi));  CrLf(0);
CrLf(0);
RlOut(0, Sqrt(2.0));  CrLf(0);                  \Sqrt is a call to an intrinsic
RlOut(0, Log(100.0));  CrLf(0);
RlOut(0, Ln(Exp(123.456789)));  CrLf(0);
CrLf(0);
RlOut(0, abs(-1234.5));  CrLf(0);               \abs works for both reals & ints
CrLf(0);
RlOut(0, float(fix(1.999-0.5)));  CrLf(0);      \floor   rounds toward -infinity
RlOut(0, float(fix(1.001+0.5)));  CrLf(0);      \ceiling rounds toward +infinity
RlOut(0, Power(sqrt(2.0), 4.0));  CrLf(0);      \sqrt is an inline function and
]                                               \ can be used for both reals & ints
```


{{out}}

```txt

   2.7182818284590500
   1.0000000000000000

   3.1415926535897900
  -1.0000000000000000

   1.4142135623731000
   2.0000000000000000
 123.4567890000000000

1234.5000000000000000

   1.0000000000000000
   2.0000000000000000
   4.0000000000000000

```



## zkl


```zkl
(0.0).e        // Euler's number, a property of all floats
(0.0).e.pi     // pi, yep, all floats
(2.0).sqrt()   // square root
(2.0).log()    // natural (base e) logarithm
(2.0).log10()  // log base 10
(0.0).e.pow(x) // e^x
(-10.0).abs()  // absolute value, both floats and ints
x.pow(y)       // x raised to the y power
x.ceil()       // ceiling
x.floor()      // floor
```


{{omit from|GUISS}}
{{omit from|M4}}
{{omit from|ML/I}}
