+++
title = "Safe addition"
description = ""
date = 2019-03-18T23:39:17Z
aliases = []
[extra]
id = 4723
[taxonomies]
categories = ["Arithmetic operations", "task"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "c",
  "c_plus_plus",
  "c_sharp",
  "d",
  "e",
  "julia",
  "kotlin",
  "mathematica",
  "nim",
  "perl",
  "perl_6",
  "phix",
  "ruby",
  "scala",
  "swift",
  "tcl",
]
+++

## Task

Implementation of   [[wp:Interval_arithmetic|interval arithmetic]]   and more generally fuzzy number arithmetic require operations that yield safe upper and lower bounds of the exact result.

For example, for an addition, it is the operations   <big> +&uarr; </big>   and   <big> +&darr; </big>   defined as:   <big> ''a'' +&darr; ''b'' &le; ''a'' + ''b'' &le; ''a'' +&uarr; ''b''. </big>

Additionally it is desired that the width of the interval   <big> (''a'' +&uarr; ''b'') - (''a'' +&darr; ''b'') </big>    would be about the machine epsilon after removing the exponent part.

Differently to the standard floating-point arithmetic, safe interval arithmetic is '''accurate''' (but still imprecise).

I.E.:   the result of each defined operation contains (though does not identify) the exact mathematical outcome.

Usually a   [[wp:Floating_Point_Unit|FPU's]]   have machine   <big> +,-,*,/ </big>   operations accurate within the machine precision.

To illustrate it, let us consider a machine with decimal floating-point arithmetic that has the precision is '''3''' decimal points.

If the result of the machine addition is   <big> 1.23, </big>   then the exact mathematical result is within the interval   <big> ]1.22, 1.24[. </big>

When the machine rounds towards zero, then the exact result is within   <big> [1.23,1.24[. </big>   This is the basis for an implementation of safe addition.


## Task
Show how   <big> +&darr; </big>   and   <big> +&uarr; </big>   can be implemented in your language using the standard floating-point type.

Define an interval type based on the standard floating-point one,   and implement an interval-valued addition of two floating-point numbers considering them exact, in short an operation that yields the interval   <big> [''a'' +&darr; ''b'', ''a'' +&uarr; ''b'']. </big>





## Ada

An interval type based on Float:

```Ada
type Interval is record
   Lower : Float;
   Upper : Float;
end record;
```

Implementation of safe addition:

```Ada
function "+" (A, B : Float) return Interval is
   Result : constant Float := A + B;
begin
   if Result < 0.0 then
      if Float'Machine_Rounds then
         return (Float'Adjacent (Result, Float'First), Float'Adjacent (Result, 0.0));
      else
         return (Float'Adjacent (Result, Float'First), Result);
      end if;
   elsif Result > 0.0 then
      if Float'Machine_Rounds then
         return (Float'Adjacent (Result, 0.0), Float'Adjacent (Result, Float'Last));
      else
         return (Result, Float'Adjacent (Result, Float'Last));
      end if;
   else -- Underflow
      return (Float'Adjacent (0.0, Float'First), Float'Adjacent (0.0, Float'Last));
   end if;
end "+";
```

The implementation uses the attribute T'Machine_Rounds to determine if rounding is performed on inexact results. If the machine rounds a symmetric interval around the result is used. When the machine does not round, it truncates. Truncating is rounding towards zero. In this case the implementation is twice better (in terms of the result interval width), because depending on its sign, the outcome of addition can be taken for either of the bounds. Unfortunately most of modern processors are rounding.

Test program:

```Ada
with Ada.Text_IO;  use Ada.Text_IO;
procedure Test_Interval_Addition is
   -- Definitions from above
   procedure Put (I : Interval) is
   begin
      Put (Long_Float'Image (Long_Float (I.Lower)) & "," & Long_Float'Image (Long_Float (I.Upper)));
   end Put;
begin
   Put (1.14 + 2000.0);
end Test_Interval_Addition;
```

Sample output:

```txt
 2.00113989257813E+03, 2.00114013671875E+03
```



## AutoHotkey


```ahk
Msgbox % IntervalAdd(1,2) ; [2.999999,3.000001]

SetFormat, FloatFast, 0.20
Msgbox % IntervalAdd(1,2) ; [2.99999999999999910000,3.00000000000000090000]

;In v1.0.48+, floating point variables have about 15 digits of precision internally
;unless SetFormat Float (i.e. the slow mode) is present anywhere in the script.
;In that case, the stored precision of floating point numbers is determined by A_FormatFloat.
;As there is no way for this function to know whether this is the case or not,
;it conservatively uses A_FormatFloat in all cases.
IntervalAdd(a,b){
	err:=0.1**(SubStr(A_FormatFloat,3) > 15 ? 15 : SubStr(A_FormatFloat,3))
	Return "[" a+b-err ","a+b+err "]"
}
```




## C

Most systems use the IEEE floating-point numbers. These systems have four rounding modes.

* Round toward zero.
* Round down (toward -infinity).
* Round to nearest.
* Round up (toward +infinity).

If one can change the rounding mode, then [a + b rounded down, a + b rounded up] solves the task. C99 provides a standard way, through <fenv.h>, to change the rounding mode, but not every system has <fenv.h>. Microsoft has _controlfp(). Some Unix clones, like [[OpenBSD]], have fpsetround().

An optimizing compiler might break the code. (For example, it might calculate a + b only one time.) To prevent such optimizations, we declare our floating-point numbers as <code>volatile</code>. This forces the compiler to calculate a + b two times, between the correct function calls.

=== C99 with fesetround() ===

```c
#include <fenv.h> /* fegetround(), fesetround() */
#include <stdio.h>	/* printf() */

/*
 * Calculates an interval for a + b.
 *   interval[0] <= a + b
 *   a + b <= interval[1]
 */
void
safe_add(volatile double interval[2], volatile double a, volatile double b)
{
#pragma STDC FENV_ACCESS ON
	unsigned int orig;

	orig = fegetround();
	fesetround(FE_DOWNWARD);	/* round to -infinity */
	interval[0] = a + b;
	fesetround(FE_UPWARD);		/* round to +infinity */
	interval[1] = a + b;
	fesetround(orig);
}

int
main()
{
	const double nums[][2] = {
		{1, 2},
		{0.1, 0.2},
		{1e100, 1e-100},
		{1e308, 1e308},
	};
	double ival[2];
	int i;

	for (i = 0; i < sizeof(nums) / sizeof(nums[0]); i++) {
		/*
		 * Calculate nums[i][0] + nums[i][1].
		 */
		safe_add(ival, nums[i][0], nums[i][1]);

		/*
		 * Print the result. %.17g gives the best output.
		 * %.16g or plain %g gives not enough digits.
		 */
		printf("%.17g + %.17g =\n", nums[i][0], nums[i][1]);
		printf("    [%.17g, %.17g]\n", ival[0], ival[1]);
		printf("    size %.17g\n\n", ival[1] - ival[0]);
	}
	return 0;
}
```

Output:

```txt

1 + 2 =
    [3, 3]
    size 0

0.10000000000000001 + 0.20000000000000001 =
    [0.29999999999999999, 0.30000000000000004]
    size 5.5511151231257827e-17

1e+100 + 1e-100 =
    [1e+100, 1.0000000000000002e+100]
    size 1.9426688922257291e+84

1e+308 + 1e+308 =
    [1.7976931348623157e+308, inf]
    size inf

```


=== _controlfp() ===
{{works with|MinGW}}


```c
#include <float.h> /* _controlfp() */
#include <stdio.h>	/* printf() */

/*
 * Calculates an interval for a + b.
 *   interval[0] <= a + b
 *   a + b <= interval[1]
 */
void
safe_add(volatile double interval[2], volatile double a, volatile double b)
{
	unsigned int orig;

	orig = _controlfp(0, 0);
	_controlfp(_RC_DOWN, _MCW_RC);	/* round to -infinity */
	interval[0] = a + b;
	_controlfp(_RC_UP, _MCW_RC);	/* round to +infinity */
	interval[1] = a + b;
	_controlfp(orig, _MCW_RC);
}

int
main()
{
	const double nums[][2] = {
		{1, 2},
		{0.1, 0.2},
		{1e100, 1e-100},
		{1e308, 1e308},
	};
	double ival[2];
	int i;

	for (i = 0; i < sizeof(nums) / sizeof(nums[0]); i++) {
		/*
		 * Calculate nums[i][0] + nums[i][1].
		 */
		safe_add(ival, nums[i][0], nums[i][1]);

		/*
		 * Print the result. %.17g gives the best output.
		 * %.16g or plain %g gives not enough digits.
		 */
		printf("%.17g + %.17g =\n", nums[i][0], nums[i][1]);
		printf("    [%.17g, %.17g]\n", ival[0], ival[1]);
		printf("    size %.17g\n\n", ival[1] - ival[0]);
	}
	return 0;
}
```


=== fpsetround() ===
{{works with|OpenBSD|4.8/amd64}}


```c
#include <ieeefp.h> /* fpsetround() */
#include <stdio.h>	/* printf() */

/*
 * Calculates an interval for a + b.
 *   interval[0] <= a + b
 *   a + b <= interval[1]
 */
void
safe_add(volatile double interval[2], volatile double a, volatile double b)
{
	fp_rnd orig;

	orig = fpsetround(FP_RM);	/* round to -infinity */
	interval[0] = a + b;
	fpsetround(FP_RP);		/* round to +infinity */
	interval[1] = a + b;
	fpsetround(orig);
}

int
main()
{
	const double nums[][2] = {
		{1, 2},
		{0.1, 0.2},
		{1e100, 1e-100},
		{1e308, 1e308},
	};
	double ival[2];
	int i;

	for (i = 0; i < sizeof(nums) / sizeof(nums[0]); i++) {
		/*
		 * Calculate nums[i][0] + nums[i][1].
		 */
		safe_add(ival, nums[i][0], nums[i][1]);

		/*
		 * Print the result. With OpenBSD libc, %.17g gives
		 * the best output; %.16g or plain %g gives not enough
		 * digits.
		 */
		printf("%.17g + %.17g =\n", nums[i][0], nums[i][1]);
		printf("    [%.17g, %.17g]\n", ival[0], ival[1]);
		printf("    size %.17g\n\n", ival[1] - ival[0]);
	}
	return 0;
}
```


Output from OpenBSD:
```txt
1 + 2 =
    [3, 3]
    size 0

0.10000000000000001 + 0.20000000000000001 =
    [0.29999999999999999, 0.30000000000000004]
    size 5.5511151231257827e-17

1e+100 + 1e-100 =
    [1e+100, 1.0000000000000002e+100]
    size 1.9426688922257291e+84

1e+308 + 1e+308 =
    [1.7976931348623157e+308, inf]
    size inf

```



## C++

{{trans|C#}}

```cpp
#include <iostream>
#include <tuple>

union conv {
    int i;
    float f;
};

float nextUp(float d) {
    if (isnan(d) || d == -INFINITY || d == INFINITY) return d;
    if (d == 0.0) return FLT_EPSILON;

    conv c;
    c.f = d;
    c.i++;

    return c.f;
}

float nextDown(float d) {
    if (isnan(d) || d == -INFINITY || d == INFINITY) return d;
    if (d == 0.0) return -FLT_EPSILON;

    conv c;
    c.f = d;
    c.i--;

    return c.f;
}

auto safeAdd(float a, float b) {
    return std::make_tuple(nextDown(a + b), nextUp(a + b));
}

int main() {
    float a = 1.20f;
    float b = 0.03f;

    auto result = safeAdd(a, b);
    printf("(%f + %f) is in the range (%0.16f, %0.16f)\n", a, b, std::get<0>(result), std::get<1>(result));

    return 0;
}
```

{{out}}

```txt
(1.200000 + 0.030000) is in the range (1.2299998998641968, 1.2300001382827759)
```


## C#
{{trans|Java}}

```c#
using System;

namespace SafeAddition {
    class Program {
        static float NextUp(float d) {
            if (d == 0.0) return float.Epsilon;
            if (float.IsNaN(d) || float.IsNegativeInfinity(d) || float.IsPositiveInfinity(d)) return d;

            byte[] bytes = BitConverter.GetBytes(d);
            int dl = BitConverter.ToInt32(bytes, 0);
            dl++;
            bytes = BitConverter.GetBytes(dl);

            return BitConverter.ToSingle(bytes, 0);
        }

        static float NextDown(float d) {
            if (d == 0.0) return -float.Epsilon;
            if (float.IsNaN(d) || float.IsNegativeInfinity(d) || float.IsPositiveInfinity(d)) return d;

            byte[] bytes = BitConverter.GetBytes(d);
            int dl = BitConverter.ToInt32(bytes, 0);
            dl--;
            bytes = BitConverter.GetBytes(dl);

            return BitConverter.ToSingle(bytes, 0);
        }

        static Tuple<float, float> SafeAdd(float a, float b) {
            return new Tuple<float, float>(NextDown(a + b), NextUp(a + b));
        }

        static void Main(string[] args) {
            float a = 1.20f;
            float b = 0.03f;

            Console.WriteLine("({0} + {1}) is in the range {2}", a, b, SafeAdd(a, b));
        }
    }
}
```

{{out}}

```txt
(1.2 + 0.03) is in the range (1.23, 1.23)
```



## D

{{trans|Kotlin}}

```D
import std.traits;
auto safeAdd(T)(T a, T b)
if (isFloatingPoint!T) {
    import std.math;     // nexDown, nextUp
    import std.typecons; // tuple
    return tuple!("d", "u")(nextDown(a+b), nextUp(a+b));
}

import std.stdio;
void main() {
    auto a = 1.2;
    auto b = 0.03;

    auto r = safeAdd(a, b);
    writefln("(%s + %s) is in the range %0.16f .. %0.16f", a, b, r.d, r.u);
}
```


{{out}}

```txt
(1.2 + 0.03) is in the range 1.2299999999999998 .. 1.2300000000000002
```



## E


<!-- I'm using "I" a lot here, but I'm not sure what to do instead. -->

[Note: this task has not yet had attention from a floating-point expert.] <!-- Please remove this notice if you know what you're doing and have reviewed this example. -->

In E, operators are defined on the left object involved (they have to be ''somewhere'', as global definitions violate capability principles); this implies that I can't define + such that aFloat + anotherFloat yields an Interval. Instead, I shall define an Interval with its +, but restrict + (for the sake of this example) to intervals containing only one number.

(E has a built-in numeric interval type as well, but it is always closed-open rather than closed-closed and so would be particularly confusing for this task.)

E currently inherits [[Java]]'s choices of [[IEEE]] floating point behavior, including round to nearest. Therefore, as in the Ada example, given ignorance of the actual direction of rounding, we must take one step away in ''both'' directions to get a correct interval.


```e
def makeInterval(a :float64, b :float64) {
    require(a <= b)
    def interval {
        to least() { return a }
        to greatest() { return b }
        to __printOn(out) {
            out.print("[", a, ", ", b, "]")
        }
        to add(other) {
            require(a <=> b)
            require(other.least() <=> other.greatest())
            def result := a + other.least()
            return makeInterval(result.previous(), result.next())
        }
    }
    return interval
}
```



```e
? makeInterval(1.14, 1.14) + makeInterval(2000.0, 2000.0)
# value: [2001.1399999999999, 2001.1400000000003]
```


E provides only 64-bit "double precision" floats, and always prints them with sufficient decimal places to reproduce the original floating point number exactly.


## Forth

{{trans|Tcl}}

```forth
c-library m
s" m" add-lib
\c #include <math.h>
c-function fnextafter nextafter r r -- r
end-c-library

s" MAX-FLOAT" environment? drop fconstant MAX-FLOAT

: fstepdown ( F: r1 -- r2 )
   MAX-FLOAT fnegate fnextafter ;
: fstepup ( F: r1 -- r2 )
   MAX-FLOAT fnextafter ;

: savef+ ( F: r1 r2 -- r3 r4 ) \ r4 <= r1+r2 <= r3
   f+  fdup fstepup  fswap fstepdown ;
```

{{output}}

```txt

1.2e0 3e-2 savef+  ok
17 set-precision \ to get enough digits on output  ok
fs. fs. 1.2299999999999998E0 1.2300000000000002E0  ok

```


## Go

{{trans|Tcl}}

```go
package main

import (
    "fmt"
    "math"
)

// type requested by task
type interval struct {
    lower, upper float64
}

// a constructor
func stepAway(x float64) interval {
    return interval {
        math.Nextafter(x, math.Inf(-1)),
        math.Nextafter(x, math.Inf(1))}
}

// function requested by task
func safeAdd(a, b float64) interval {
    return stepAway(a + b)

}

// example
func main() {
    a, b := 1.2, .03
    fmt.Println(a, b, safeAdd(a, b))
}
```

Output:

```txt

1.2 0.03 {1.2299999999999998 1.2300000000000002}

```



## J

J uses 64 bit IEEE floating points, providing 53 binary digits of accuracy.

```j
   err =. 2^ 53-~ 2 <.@^. |   NB. get the size of one-half unit in the last place
   safeadd =. + (-,+) +&err
   0j15": 1.14 safeadd 2000.0 NB. print with 15 digits after the decimal
2001.139999999999873 2001.140000000000327
```



## Java

{{trans|Kotlin}}

```Java
public class SafeAddition {
    private static double stepDown(double d) {
        return Math.nextAfter(d, Double.NEGATIVE_INFINITY);
    }

    private static double stepUp(double d) {
        return Math.nextUp(d);
    }

    private static double[] safeAdd(double a, double b) {
        return new double[]{stepDown(a + b), stepUp(a + b)};
    }

    public static void main(String[] args) {
        double a = 1.2;
        double b = 0.03;
        double[] result = safeAdd(a, b);
        System.out.printf("(%.2f + %.2f) is in the range %.16f..%.16f", a, b, result[0], result[1]);
    }
}
```

{{out}}

```txt
(1.20 + 0.03) is in the range 1.2299999999999998..1.2300000000000002
```



## Julia

Julia has the IntervalArithmetic module, which provides arithmetic with defined precision along with the option of simply computing with actual two-number intervals:

```Julia

julia> using IntervalArithmetic

julia> n = 2.0
2.0

julia> @interval 2n/3 + 1
[2.33333, 2.33334]

julia> showall(ans)
Interval(2.333333333333333, 2.3333333333333335)

julia> a = @interval(0.1, 0.3)
[0.0999999, 0.300001]

julia> b = @interval(0.3, 0.6)
[0.299999, 0.600001]

julia> a + b
[0.399999, 0.900001]

```



## Kotlin

{{trans|Tcl}}

```scala
// version 1.1.2

fun stepDown(d: Double) = Math.nextAfter(d, Double.NEGATIVE_INFINITY)

fun stepUp(d: Double) = Math.nextUp(d)

fun safeAdd(a: Double, b: Double) = stepDown(a + b).rangeTo(stepUp(a + b))

fun main(args: Array<String>) {
    val a = 1.2
    val b = 0.03
    println("($a + $b) is in the range ${safeAdd(a, b)}")
}
```


{{out}}

```txt

(1.2 + 0.03) is in the range 1.2299999999999998..1.2300000000000002

```



## Mathematica

When you use //N to get a numerical result, Mathematica does what a standard calculator would do: it gives you a result to a fixed number of significant figures. You can also tell Mathematica exactly how many significant figures to keep in a particular calculation. This allows you to get numerical results in Mathematica to any degree of precision.


## Nim

{{trans|C}}

```nim
import posix, strutils

proc `++`(a, b: float): tuple[lower, upper: float] =
  let
    a {.volatile.} = a
    b {.volatile.} = b
    orig = fegetround()
  discard fesetround FE_DOWNWARD
  result.lower = a + b
  discard fesetround FE_UPWARD
  result.upper = a + b
  discard fesetround orig

proc ff(a: float): string = a.formatFloat(ffDefault, 17)

for x, y in [(1.0, 2.0), (0.1, 0.2), (1e100, 1e-100), (1e308, 1e308)].items:
  let (d,u) = x ++ y
  echo x.ff, " + ", y.ff, " ="
  echo "    [", d.ff, ", ", u.ff, "]"
  echo "    size ", (u - d).ff, "\n"
```

Output:

```txt
1.0000000000000000 + 2.0000000000000000 =
    [3.0000000000000000, 3.0000000000000000]
    size 0.0000000000000000

0.10000000000000001 + 0.20000000000000001 =
    [0.29999999999999999, 0.30000000000000004]
    size 5.5511151231257827e-17

1.0000000000000000e+100 + 1.0000000000000000e-100 =
    [1.0000000000000000e+100, 1.0000000000000002e+100]
    size 1.9426688922257291e+84

1.0000000000000000e+308 + 1.0000000000000000e+308 =
    [1.7976931348623157e+308, inf]
    size inf
```



## Perl

There are ways avoid this whole problem (e.g. <code>Math::Decimal</code>), but another module suffices for this task.

```perl
use strict;
use warnings;
use Data::IEEE754::Tools <nextUp nextDown>;

sub safe_add {
    my($a,$b) = @_;
    my $c = $a + $b;
    return $c, nextDown($c), nextUp($c)
}

printf "%.17f (%.17f, %.17f)\n", safe_add (1/9,1/7);
```

{{out}}

```txt
0.25396825396825395 (0.25396825396825390, 0.25396825396825401)
```



## Perl 6

{{works with|Rakudo|2018.12}}

Perl 6 uses 64 bit IEEE floating points numbers which  provide 53 binary digits
of accuracy. If you insist on using floats your answer will be accurate in the range
± 1.1102230246251565e-16. Perl 6 actually makes it somewhat difficult to output
an exact stringified float with more than 15 digits of accuracy. It automatically
rounds since it doesn't try to pretend that it is more accurate than that.

If you really DO need very high precision and accuracy, Perl 6 has native
built-in Rational number support. Rationals are the ratio of two integers, and
are always exact. Standard Rats are limited to 64 bits of precision in the
denominator. On overflow, they will downgrade to a float (Num).
FatRats have unlimited denominators.

There is no need to load any external libraries or force the use of Rats. Unless
you force it otherwise, decimal number are always stored as Rats as long as they
will fit within the denominator limit.

You can see the two integers making up the ratio of a Rat (or FatRat) using the
.nude method ('''nu'''merator, '''de'''nominator).

Rats and FatRats by default stringify to a fixed number of places for
non-terminating fractions and the same number of digits as the denominator for
terminating fractions. If you need more control over stringification, load the
module Rat::Precise for configurable stringification. Rat::Precise ''only'' affects
stringification, not calculation. Calculations are always done at full (exact) precision.



```perl6
say "Floating points: (Nums)";
say "Error: " ~ (2**-53).Num;

sub infix:<±+> (Num $a, Num $b) {
    my \ε = (2**-53).Num;
    $a - ε + $b, $a + ε + $b,
}

printf "%4.16f .. %4.16f\n", (1.14e0 ±+ 2e3);

say "\nRationals:";

say ".1 + .2 is exactly equal to .3: ", .1 + .2 === .3;

say "\nLarge denominators require explicit coercion to FatRats:";
say "Sum of inverses of the first 500 natural numbers:";
my $sum = sum (1..500).map: { FatRat.new(1,$_) };
say $sum;
say $sum.nude;

{
    say "\nRat stringification may not show full precision for terminating fractions by default.";
    say "Use a module to get full precision.";
    use Rat::Precise; # module loading is scoped to the enclosing block
    my $rat = 1.5**63;
    say "\nPerl 6 default stringification for 1.5**63:\n" ~ $rat; # standard stringification
    say "\nRat::Precise stringification for 1.5**63:\n" ~$rat.precise; # full precision
}
```

{{out}}

```txt
Floating points: (Nums)
Error: 1.1102230246251565e-16
2001.1400000000000000 .. 2001.1400000000000000

Rationals:
.1 + .2 is exactly equal to .3: True

Large denominators require explicit coercion to FatRats:
Sum of inverses of the first 500 natural numbers:
6.79282342999052460298928714536797369481981381439677911664308889685435662379055049245764940373586560391756598584375065928223134688479711715030249848314807266844371012370203147722209400570479644295921001097190193214586271
(6633384299891198065461433023874214660151383488987829406868700907802279376986364154005690172480537248349310365871218591743641116766728139494727637850449054802989613344274500453825922847052235859615378238909694581687099 976528297586037954584851660253897317730151176683856787284655867127950765610716178591036797598551026470244168088645451676177520177514977827924165875515464044694152207479405310883385229609852607806002629415184926954240)

Rat stringification may not show full precision for terminating fractions by default.
Use a module to get full precision.

Perl 6 default stringification for 1.5**63:
124093581919.64894769782737365038

Rat::Precise stringification for 1.5**63:
124093581919.648947697827373650380188008224280338254175148904323577880859375

```



## Phix

{{Trans|C}}
Note the Phix printf builtin has (automatic rounding and) a built-in limit of 16 digits,
(20 digits on 64 bit) for pretty much the same reason the C version went with 17 digits,
namely that the 17th digit is so inaccurate as to be completely meaningless in normal use.

Hence errors in the low,high are a bit more disguised, but as always it is the size that matters.

Not surprisingly you have to get a bit down and dirty to manage this sort of stuff in Phix.

```Phix
include builtins\VM\pFPU.e  -- :%down53 etc

function safe_add(atom a, atom b)
atom low,high
    -- NB: be sure to restore the usual/default rounding!
    #ilASM{
        [32]
            lea esi,[a]
            call :%pLoadFlt
            lea esi,[b]
            call :%pLoadFlt
            fld st0
            call :%down53
            fadd st0,st2
            lea edi,[low]
            call :%pStoreFlt
            call :%up53
            faddp
            lea edi,[high]
            call :%pStoreFlt
            call :%near53       -- usual/default
        [64]
            lea rsi,[a]
            call :%pLoadFlt
            lea rsi,[b]
            call :%pLoadFlt
            fld st0
            call :%down64
            fadd st0,st2
            lea rdi,[low]
            call :%pStoreFlt
            call :%up64
            faddp
            lea rdi,[high]
            call :%pStoreFlt
            call :%near64   -- usual/default
        []
          }
    return {low,high}
end function

constant nums = {{1, 2},
                 {0.1, 0.2},
                 {1e100, 1e-100},
                 {1e308, 1e308}}

    for i=1 to length(nums) do
        atom {a,b} = nums[i]
        atom {low,high} = safe_add(a,b)
        printf(1,"%.16g + %.16g =\n", {a, b});
        printf(1,"    [%.16g, %.16g]\n", {low, high});
        printf(1,"    size %.16g\n\n", high - low);
    end for
```

{{out}}

```txt

1 + 2 =
    [3, 3]
    size 0

0.1 + 0.2 =
    [0.3, 0.3]
    size 5.551115123125782e-17

1e+100 + 1e-100 =
    [1e+100, 1e+100]
    size 1.942668892225729e+84

1e+308 + 1e+308 =
    [1.797693134862316e+308, inf]
    size inf

```



## PicoLisp

PicoLisp uses scaled integer arithmetic, with unlimited precision, for all
operations on real numbers. For that reason addition and subtraction are always
exact. Multiplication is also exact (unless the result is explicitly scaled by
the user), and division in combination with the remainder.


## Python

Python doesn't include a module that returns an interval for safe addition, however, it does [http://docs.python.org/library/math.html#math.fsum include] a routine for performing additions of floating point numbers whilst preserving precision:


```python>>>
 sum([.1, .1, .1, .1, .1, .1, .1, .1, .1, .1])
0.9999999999999999
>>> from math import fsum
>>> fsum([.1, .1, .1, .1, .1, .1, .1, .1, .1, .1])
1.0
```



## Racket



```Racket

#lang racket

;; 1. Racket has exact unlimited integers and fractions, which can be
;;    used to perform exact operations.  For example, given an inexact
;;    flonum, we can convert it to an exact fraction and work with that:
(define (exact+ x y)
  (+ (inexact->exact x) (inexact->exact y)))
;; (A variant of this would be to keep all numbers exact, so the default
;; operations never get to inexact numbers)

;; 2. We can implement the required operation using a bunch of
;;    functionality provided by the math library, for example, use
;;    `flnext' and `flprev' to get the surrounding numbers for both
;;    inputs and use them to produce the resulting interval:
(require math)
(define (interval+ x y)
  (cons (+ (flprev x) (flprev y)) (+ (flnext x) (flnext y))))
(interval+ 1.14 2000.0) ; -> '(2001.1399999999999 . 2001.1400000000003)
;; (Note: I'm not a numeric expert in any way, so there must be room for
;; improvement here...)

;; 3. Yet another option is to use the math library's bigfloats, with an
;;    arbitrary precision:
(bf-precision 1024) ; 1024 bit floats
;; add two numbers, specified as strings to avoid rounding of number
;; literals
(bf+ (bf "1.14") (bf "2000.0"))

```



## REXX

REXX can solve the safe addition problem by simply increasing the   '''numeric digits'''   (a REXX statement).


There is effectively no limit to the number of digits, but it is
constrained by how much virtual memory is (or can be) allocated.

Eight million digits seems about a practical high end, however.

```rexx
numeric digits 1000           /*defines precision to be 1,000 decimal digits. */

y=digits()                    /*sets  Y  to existing number of decimal digits.*/

numeric digits y + y%10       /*increase the  (numeric) decimal digits by 10%.*/
```





## Ruby

The <tt>Float</tt> class provides no way to change the rounding mode. We instead use the <tt>BigDecimal</tt> class from the standard library. <tt>BigDecimal</tt> is a floating-point number of radix 10 with arbitrary precision.

When adding <tt>BigDecimal</tt> values, <tt>a + b</tt> is always safe. This example uses <tt>a.add(b, prec)</tt>, which is not safe because it rounds to <tt>prec</tt> digits. This example computes a safe interval by rounding to both floor and ceiling.


```ruby
require 'bigdecimal'
require 'bigdecimal/util'  # String#to_d

def safe_add(a, b, prec)
  a, b = a.to_d, b.to_d
  rm = BigDecimal::ROUND_MODE
  orig = BigDecimal.mode(rm)

  BigDecimal.mode(rm, BigDecimal::ROUND_FLOOR)
  low = a.add(b, prec)

  BigDecimal.mode(rm, BigDecimal::ROUND_CEILING)
  high = a.add(b, prec)

  BigDecimal.mode(rm, orig)
  low..high
end

[["1", "2"],
 ["0.1", "0.2"],
 ["0.1", "0.00002"],
 ["0.1", "-0.00002"],
].each { |a, b| puts "#{a} + #{b} = #{safe_add(a, b, 3)}" }
```


Output:
```txt
1 + 2 = 0.3E1..0.3E1
0.1 + 0.2 = 0.3E0..0.3E0
0.1 + 0.00002 = 0.1E0..0.101E0
0.1 + -0.00002 = 0.999E-1..0.1E0
```



## Scala


```Scala
object SafeAddition extends App {
  val (a, b) = (1.2, 0.03)
  val result = safeAdd(a, b)

  private def safeAdd(a: Double, b: Double) = Seq(stepDown(a + b), stepUp(a + b))

  private def stepDown(d: Double) = Math.nextAfter(d, Double.NegativeInfinity)

  private def stepUp(d: Double) = Math.nextUp(d)

  println(f"($a%.2f + $b%.2f) is in the range ${result.head}%.16f .. ${result.last}%.16f")

}
```



## Swift


```swift
let a = 1.2
let b = 0.03

print("\(a) + \(b) is in the range \((a + b).nextDown)...\((a + b).nextUp)")
```


{{out}}

```txt
1.2 + 0.03 is in the range 1.2299999999999998...1.2300000000000002
```



## Tcl

Tcl's floating point handling is done using IEEE double-precision floating point with the default rounding mode (i.e., to nearest representable value). This means that it is necessary to step away from the computed value slightly in both directions in order to compute a safe range. However, Tcl doesn't expose the <code>nextafter</code> function from <code>math.h</code> by default (necessary to do this right), so a little extra magic is needed.


{{libheader|critcl}}

```tcl
package require critcl
package provide stepaway 1.0
critcl::ccode {
    #include <math.h>
    #include <float.h>
}
critcl::cproc stepup {double value} double {
    return nextafter(value, DBL_MAX);
}
critcl::cproc stepdown {double value} double {
    return nextafter(value, -DBL_MAX);
}
```

With that package it's then trivial to define a "safe addition" that returns an interval as a list (lower bound, upper bound).

```tcl
package require stepaway
proc safe+ {a b} {
    set val [expr {double($a) + $b}]
    return [list [stepdown $val] [stepup $val]]
}
```


{{omit from|M4}}
