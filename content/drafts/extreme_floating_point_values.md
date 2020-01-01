+++
title = "Extreme floating point values"
description = ""
date = 2018-04-18T13:00:19Z
aliases = []
[extra]
id = 7770
[taxonomies]
categories = []
tags = []
+++

{{task}} [[Category:Irrational numbers]]
{{omit from|BBC BASIC}}
{{omit from|Golfscript}}
{{omit from|Retro}}

The IEEE floating point specification defines certain 'extreme' floating point values such as minus zero, -0.0, a value distinct from plus zero; not a number, NaN; and plus and minus infinity.

The task is to use expressions involving other 'normal' floating point values in your language to calculate these, (and maybe other), extreme floating point values in your language and assign them to variables.

Print the values of these variables if possible; and show some arithmetic with these values and variables.

If your language can directly enter these extreme floating point values then show it.


;See also:
*   [http://www.cl.cam.ac.uk/teaching/1011/FPComp/floatingmath.pdf What Every Computer Scientist Should Know About Floating-Point Arithmetic]


;Related tasks:
*   [[Infinity]]
*   [[Detect division by zero]]
*   [[Literals/Floating point]]





## Ada

The language specifies model floating-point numbers independent of the underlying hardware. Even if the machine numbers are IEEE 754, the user-defined floating-point numbers are guaranteed to have no IEEE 754 semantics. In particular, their values do not include any non-numeric ideals. Constraint_Error exception is propagated when the result of a numeric operation assigned to a floating-point variable is not in the range (the range is always '''numeric''').

For performance reasons, the built-in floating-point types like Float and Long_Float are allowed to have IEEE 754 semantics if the machine numbers are IEEE 754. But the language provides means to exclude all non numbers from these types by defining a subtype with an explicit range:

```Ada

subtype Consistent_Float is Float range Float'Range; -- No IEEE ideals

```

In general in properly written Ada programs variables may not become invalid when standard numeric operations are applied. The language also provides the attribute 'Valid to verify values obtained from unsafe sources e.g. from input, unchecked conversions etc.

As stated above on a machine where Float is implemented by an IEEE 754 machine number, IEEE 754 is permitted leak through. The following program illustrates how this leak can be exploited:

```Ada

with Ada.Text_IO; use Ada.Text_IO;

procedure IEEE is -- Non portable, bad, never do this!
   Zero  : Float := 0.0;
   PInf  : Float := 1.0 / Zero;
   NInf  : Float := -PInf;
   PZero : Float := 1.0 / PInf;
   NZero : Float := 1.0 / NInf;
   NaN   : Float := 0.0 / Zero;
begin
   Put_Line (" -oo = " & Float'Image (NInf));
   Put_Line (" +oo = " & Float'Image (PInf));
   Put_Line (" NaN = " & Float'Image (NaN));
   Put_Line ("  -0 = " & Float'Image (NZero));

   Put_Line (" -oo < first " & Boolean'Image (NInf < Float'First));
   Put_Line (" +oo > last  " & Boolean'Image (PInf > Float'Last));
   Put_Line (" NaN = NaN   " & Boolean'Image (NaN = NaN));
   Put_Line ("  -0 = 0     " & Boolean'Image (NZero = 0.0));
   Put_Line ("  +0 = 0     " & Boolean'Image (PZero = 0.0));
   Put_Line ("  +0 < least positive   " & Boolean'Image (PZero < Float'Succ (Zero)));
   Put_Line ("  -0 > biggest negative " & Boolean'Image (NZero > Float'Pred (Zero)));

      -- Validness checks
   Put_Line ("Valid -oo is " & Boolean'Image (NInf'Valid));
   Put_Line ("Valid +oo is " & Boolean'Image (PInf'Valid));
   Put_Line ("Valid NaN is " & Boolean'Image (NaN'Valid));

end IEEE;

```

The expression -1.0 / 0.0 were non-numeric and thus could not be used.
To fool the compiler the variable Zero is used,
which circumvents type checks giving desired broken result.
{{out}}

```txt

 -oo = -Inf*******
 +oo =  +Inf*******
 NaN = NaN********
  -0 = -0.00000E+00
 -oo < first TRUE
 +oo > last  TRUE
 NaN = NaN   FALSE
  -0 = 0     TRUE
  +0 = 0     TRUE
  +0 < least positive   TRUE
  -0 > biggest negative TRUE
Valid -oo is FALSE
Valid +oo is FALSE
Valid NaN is FALSE

```



## AWK

The One True Awk ([[nawk]]) uses the native floating-point numbers.
We can get the extreme values if these are IEEE numbers.
(If you run Awk on a VAX, there are no signed zeros, infinities nor NaN on a VAX.)

Awk raises a fatal error if a program divides by zero.
If a call to exp(x), log(x) and sqrt(x) goes out of range, Awk displays a warning and changes the result to 1.
Therefore tricks like 1 / 0, or log(0), or sqrt(-1), will not provide the extreme values.
There remains some loopholes. Awk never checks for overflow,
so we can still get positive or negative infinity.
When we have infinity, we can get NaN.

{{works with|nawk|20100523}}

```awk
BEGIN {
	# This requires 1e400 to overflow to infinity.
	nzero = -0
	nan  = 0 * 1e400
	pinf = 1e400
	ninf = -1e400

	print "nzero =", nzero
	print "nan =", nan
	print "pinf =", pinf
	print "ninf =", ninf
	print

	# When y == 0, sign of x decides if atan2(y, x) is 0 or pi.
	print "atan2(0, 0) =", atan2(0, 0)
	print "atan2(0, pinf) =", atan2(0, pinf)
	print "atan2(0, nzero) =", atan2(0, nzero)
	print "atan2(0, ninf) =", atan2(0, ninf)
	print

	# From least to most: ninf, -1e200, 1e200, pinf.
	print "ninf * -1 =", ninf * -1
	print "pinf * -1 =", pinf * -1
	print "-1e200 > ninf?", (-1e200 > ninf) ? "yes" : "no"
	print "1e200 < pinf?", (1e200 < pinf) ? "yes" : "no"
	print

	# NaN spreads from input to output.
	print "nan test:", (1 + 2 * 3 - 4) / (-5.6e7 + nan)

	# NaN never equals anything. These tests should print "no".
	print "nan == nan?", (nan == nan) ? "yes" : "no"
	print "nan == 42?", (nan == 42) ? "yes" : "no"
}
```


----
{{out}} from [[nawk]] version 2010:


```txt
$ awk -f extreme.awk
nzero = -0
nan = nan
pinf = inf
ninf = -inf

atan2(0, 0) = 0
atan2(0, pinf) = 0
atan2(0, nzero) = 3.14159
atan2(0, ninf) = 3.14159

nan test: nan
nan == nan? yes
nan == 42? yes
```


The last two lines are wrong. IEEE says that NaN != NaN (and also NaN != 42).
The problem is that Awk assumes <tt>a == b</tt> unless <tt>(a - b) < 0</tt> or <tt>(a - b) > 0</tt>; but NaN - NaN (or NaN - 42) is NaN, and NaN < 0 is false, and NaN > 0 is false, so Awk supposes that NaN == NaN (or NaN == 42) is true.

----
{{out}} from [[gawk]] version 3.1.7:


```txt
nzero = 0
nan = NaN
pinf = Inf
ninf = NaN

atan2(0, 0) = 0
atan2(0, pinf) = 0
atan2(0, nzero) = 0
atan2(0, ninf) = 3.14159

ninf * -1 = Inf
pinf * -1 = NaN
-1e200 > ninf? yes
1e200 < pinf? yes

nan test: NaN
nan == nan? no
nan == 42? no
```


The attempts to use negative zero have failed. GNU awk uses both integers and floating point; GNU awk converted negative zero to an integer and lost the negative sign.

NaN works. Negative infinity seems to work, except when printing.
Whenever GNU awk tries to print negative infinity, it prints "NaN".


## bc

bc numbers are very different from IEEE floating-point numbers.
bc numbers have a variable number of digits.
They can always have more digits (until bc has no memory,
runs too slow or crashes), so there is no overflow,
and no way to reach infinity.

bc also has no negative zero, and no NaN.

{{works with|OpenBSD bc}}
 $ '''bc'''
 '''# trying for negative zero'''
 '''-0
 0
 '''# trying to underflow to negative zero'''
 '''-1 / 2'''
 0
 '''# trying for NaN (not a number)'''
 '''0 / 0'''
 dc: divide by zero
 0
 '''sqrt(-1)'''
 dc: square root of negative number
 dc: stack empty
 dc: stack empty


## C

Note: Under the C standard, division by zero (of any type) is undefined behavior.

: The result of the / operator is the quotient from the division of the first operand by the second; the result of the % operator is the remainder. In both operations, if the value of the second operand is zero, the behavior is undefined.

: -- C99 standard, section 6.5.5 paragraph 5

Floating-point division by zero in the following examples to obtain infinity or NaN are dependent on implementation-specific behavior.

{{works with|gcc|4.4.3}}

```c
#include <stdio.h>

int main()
{
    double inf = 1/0.0;
    double minus_inf = -1/0.0;
    double minus_zero = -1/ inf ;
    double nan = 0.0/0.0;

    printf("positive infinity: %f\n",inf);
    printf("negative infinity: %f\n",minus_inf);
    printf("negative zero: %f\n",minus_zero);
    printf("not a number: %f\n",nan);

    /* some arithmetic */

    printf("+inf + 2.0 = %f\n",inf + 2.0);
    printf("+inf - 10.1 = %f\n",inf - 10.1);
    printf("+inf + -inf = %f\n",inf + minus_inf);
    printf("0.0 * +inf = %f\n",0.0 * inf);
    printf("1.0/-0.0 = %f\n",1.0/minus_zero);
    printf("NaN + 1.0 = %f\n",nan + 1.0);
    printf("NaN + NaN = %f\n",nan + nan);

    /* some comparisons */

    printf("NaN == NaN = %s\n",nan == nan ? "true" : "false");
    printf("0.0 == -0.0 = %s\n",0.0 == minus_zero ? "true" : "false");

    return 0;
}
```


{{out}}

```txt
positive infinity: inf
negative infinity: -inf
negative zero: -0.000000
not a number: -nan
+inf + 2.0 = inf
+inf - 10.1 = inf
+inf + -inf = -nan
0.0 * +inf = -nan
1.0/-0.0 = -inf
NaN + 1.0 = -nan
NaN + NaN = -nan
NaN == NaN = false
0.0 == -0.0 = true
```


{{out}} using [[MinGW]] with [[gcc]] 4.5.2 on [[Windows]] 7:

```txt
positive infinity: 1.#INF00
negative infinity: -1.#INF00
negative zero: -0.000000
not a number: -1.#IND00
+inf + 2.0 = 1.#INF00
+inf - 10.1 = 1.#INF00
+inf + -inf = -1.#IND00
0.0 * +inf = -1.#IND00
1.0/-0.0 = -1.#INF00
NaN + 1.0 = -1.#IND00
NaN + NaN = -1.#IND00
NaN == NaN = false
0.0 == -0.0 = true
```


{{out}} using icpc version 12.1.4 (gcc version 4.6.0 compatibility) on Ubuntu 12.04 (64 bit):

```txt
positive infinity: inf
negative infinity: -inf
negative zero: -0.000000
not a number: -nan
+inf + 2.0 = inf
+inf - 10.1 = inf
+inf + -inf = -nan
0.0 * +inf = 0.000000
1.0/-0.0 = -inf
NaN + 1.0 = -nan
NaN + NaN = -nan
NaN == NaN = false
0.0 == -0.0 = true
```


Some values may be directly defined in various headers.
Following code also shows some of those values' bit patterns
(most significant bit first for each byte).
It should be pretty portable.

```c
#include <stdio.h>
#include <values.h>
#include <math.h>

char * bits(double v) {
	static char s[sizeof(double) * (CHARBITS + 1)];
	int n, i, j;
	unsigned char *c = (void*)&v;
	for (i = n = 0; i < sizeof(double); i++) {
		for (j = 1 << (CHARBITS - 1); j; j >>= 1)
			s[n++] = (c[i] & j) ? '1' : '.';
		s[n++] = ' ';
	}
	s[n-1] = 0;
	return s;
}

int main(void)
{
	double x[] = {
		1.0, -1.0, 1.0/256, 0.0, // "normal" values
		-0.0, INFINITY, -INFINITY, NAN, -NAN, // special
		DBL_MAX, DBL_MIN // not required by task
	};
	int i;

	for (i = 0; i < sizeof(x) / sizeof(x[0]); i++)
		printf("%s | %g\n", bits(x[i]), x[i]);

	return 0;
}
```



## Clojure

{{Trans|Groovy}}

```clojure

(def neg-inf (/ -1.0 0.0)) ; Also Double/NEGATIVE_INFINITY
(def inf (/ 1.0 0.0))      ; Also Double/POSITIVE_INFINITY
(def nan (/ 0.0 0.0))      ; Also Double/NaN
(def neg-zero (/ -2.0 Double/POSITIVE_INFINITY))   ; Also -0.0
(println "  Negative inf: " neg-inf)
(println "  Positive inf: " inf)
(println "           NaN: " nan)
(println "    Negative 0: " neg-zero)
(println "    inf + -inf: " (+ inf neg-inf))
(println "    NaN == NaN: " (= Double/NaN Double/NaN))
(println "NaN equals NaN: " (.equals Double/NaN Double/NaN))

```


{{out}}

```txt

  Negative inf:  -Infinity
  Positive inf:  Infinity
           NaN:  NaN
    Negative 0:  -0.0
    inf + -inf:  NaN
    NaN == NaN:  false
NaN equals NaN:  true

```



## D

D V.2 has a pretty comprehensive approach to floating point values,
and unlike Ada embraces IEEE 754.
This program shows only part of the floating point features
supported by D and its Phobos standard library.

```d
// Compile this module without -O

import std.stdio: writeln, writefln;
import std.string: format;
import std.math: NaN, getNaNPayload;

void show(T)() {
    static string toHex(T x) {
        string result;
        auto ptr = cast(ubyte*)&x;
        foreach_reverse (immutable i; 0 .. T.sizeof)
            result ~= format("%02x", ptr[i]);
        return result;
    }

    enum string name =  T.stringof;
    writeln("Computed extreme ", name, " values:");

    T zero     = 0.0;
    T pos_inf  = T(1.0) / zero;
    writeln(" ", name, " +oo = ", pos_inf);

    T neg_inf  = -pos_inf;
    writeln(" ", name, " -oo = ", neg_inf);

    T pos_zero = T(1.0) / pos_inf;
    writeln(" ", name, " +0 (pos_zero) = ", pos_zero);

    T neg_zero = T(1.0) / neg_inf;
    writeln(" ", name, " -0 = ", neg_zero);

    T nan      = zero / pos_zero;
    writefln(" " ~ name ~ " zero / pos_zero = %f  %s", nan, toHex(nan));
    writeln();

    writeln("Some ", T.stringof, " properties and literals:");
    writeln(" ", name, " +oo = ", T.infinity);
    writeln(" ", name, " -oo = ", -T.infinity);
    writeln(" ", name, " +0 = ", T(0.0));
    writeln(" ", name, " -0 = ", T(-0.0));
    writefln(" " ~ name ~ " nan = %f   %s", T.nan, toHex(T.nan));
    writefln(" " ~ name ~ " init = %f  %s", T.init, toHex(T.init));
    writeln(" ", name, " epsilon = ", T.epsilon);
    writeln(" ", name, " max = ", T.max);
    writeln(" ", name, " -max = ", -T.max);
    writeln(" ", name, " min_normal = ", -T.min_normal);
    writeln("-----------------------------");
}

void main() {
    show!float;
    show!double;
    show!real;

    writeln("Largest possible payload for float, double and real NaNs:");
    immutable float f1 = NaN(0x3F_FFFF);
    writeln(getNaNPayload(f1));

    immutable double f2 = NaN(0x3_FFFF_FFFF_FFFF);
    writeln(getNaNPayload(f2));

    immutable real f3 = NaN(0x3FFF_FFFF_FFFF_FFFF);
    writeln(getNaNPayload(f3));
}
```

{{out}}

```txt
Computed extreme float values:
 float +oo = inf
 float -oo = -inf
 float +0 = 0
 float -0 = -0
 float init = -nan  ffc00000

Some float properties and literals:
 float +oo = inf
 float -oo = -inf
 float +0 = 0
 float -0 = -0
 float nan = nan   7fc00000
 float init = nan  7fa00000
 float epsilon = 1.19209e-07
 float max = 3.40282e+38
 float -max = -3.40282e+38
 float min_normal = -1.17549e-38
-----------------------------
Computed extreme double values:
 double +oo = inf
 double -oo = -inf
 double +0 = 0
 double -0 = -0
 double init = -nan  fff8000000000000

Some double properties and literals:
 double +oo = inf
 double -oo = -inf
 double +0 = 0
 double -0 = -0
 double nan = nan   7ff8000000000000
 double init = nan  7ff4000000000000
 double epsilon = 2.22045e-16
 double max = 1.79769e+308
 double -max = -1.79769e+308
 double min_normal = -2.22507e-308
-----------------------------
Computed extreme real values:
 real +oo = inf
 real -oo = -inf
 real +0 = 0
 real -0 = -0
 real init = -nan  ffffc000000000000000

Some real properties and literals:
 real +oo = inf
 real -oo = -inf
 real +0 = 0
 real -0 = -0
 real nan = nan   7fffc000000000000000
 real init = nan  7fffa000000000000000
 real epsilon = 1.0842e-19
 real max = 1.18973e+4932
 real -max = -1.18973e+4932
 real min_normal = -3.3621e-4932
-----------------------------
Largest possible payload for float, double and real NaNs:
4194303
1125899906842623
4610560118520545279
```


If you compile it with -O you get results like:


```txt
Computed extreme float values:
 float +oo = 2.9411e-36
 float -oo = -2.9411e-36
 float +0 (pos_zero) = 3.40008e+35
 float -0 = -3.40008e+35
 float zero / pos_zero = 0.000000  00000000

Some float properties and literals:
 float +oo = inf
 float -oo = -inf
 float +0 = 0
 float -0 = -0
 float nan = nan   7fc00000
 float init = nan  7fa00000
 float epsilon = 1.19209e-07
 float max = 3.40282e+38
 float -max = -3.40282e+38
 float min_normal = -1.17549e-38
-----------------------------
Computed extreme double values:
 double +oo = 2.04581e-275
 double -oo = -2.04581e-275
 double +0 (pos_zero) = 4.88804e+274
 double -0 = -4.88804e+274
 double zero / pos_zero = 0.000000  0000000000000000

Some double properties and literals:
 double +oo = inf
 double -oo = -inf
 double +0 = 0
 double -0 = -0
 double nan = nan   7ff8000000000000
 double init = nan  7ff4000000000000
 double epsilon = 2.22045e-16
 double max = 1.79769e+308
 double -max = -1.79769e+308
 double min_normal = -2.22507e-308
-----------------------------
Computed extreme real values:
 real +oo = 1.81242e-4933
 real -oo = -1.81242e-4933
 real +0 (pos_zero) = inf
 real -0 = -inf
 real zero / pos_zero = 0.000000  00000000000000000000

Some real properties and literals:
 real +oo = inf
 real -oo = -inf
 real +0 = 0
 real -0 = -0
 real nan = nan   7fffc000000000000000
 real init = nan  7fffa000000000000000
 real epsilon = 1.0842e-19
 real max = 1.18973e+4932
 real -max = -1.18973e+4932
 real min_normal = -3.3621e-4932
-----------------------------
Largest possible payload for float, double and real NaNs:
4194303
1125899906842623
4610560118520545279
```


Among other things, it is possible to trap FP hardware exceptions:

```d
import std.math: FloatingPointControl;

void main() {
    // Enable hardware exceptions for division by zero, overflow
    // to infinity, invalid operations, and uninitialized
    // floating-point variables.
    FloatingPointControl fpc;
    fpc.enableExceptions(FloatingPointControl.severeExceptions);

    double f0 = 0.0;
    double y1 = f0 / f0; // generates hardware exception
                         // unless it's compiled with -O)
}
```

{{out}}

```txt
object.Error: Invalid Floating Point Operation
```



## Delphi


Tested on Delphi 2009:


```Delphi
program Floats;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  PlusInf, MinusInf, NegZero, NotANum: Double;

begin
  PlusInf:= 1.0/0.0;
  MinusInf:= -1.0/0.0;
  NegZero:= -1.0/PlusInf;
  NotANum:= 0.0/0.0;

  Writeln('Positive Infinity: ', PlusInf);      // +Inf
  Writeln('Negative Infinity: ', MinusInf);     // -Inf
  Writeln('Negative Zero: ', NegZero);          // -0.0
  Writeln('Not a Number: ', NotANum);           // Nan

// allowed arithmetic
  Writeln('+Inf + 2.0 = ', PlusInf + 2.0);      // +Inf
  Writeln('+Inf - 10.1 = ', PlusInf - 10.1);    // +Inf
  Writeln('NaN + 1.0 = ', NotANum + 1.0);       // Nan
  Writeln('NaN + NaN = ', NotANum + NotANum);   // Nan

// throws exception
  try
    Writeln('+inf + -inf = ', PlusInf + MinusInf);  // EInvalidOp
    Writeln('0.0 * +inf = ', 0.0 * PlusInf);        // EInlalidOp
    Writeln('1.0/-0.0 = ', 1.0 / NegZero);          // EZeroDivide
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;

  Readln;
end.
```



## Eiffel


```Eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			negInf, posInf,	negZero, nan: REAL_64
		do
			negInf := -1. / 0.	-- also {REAL_64}.negative_infinity
			posInf := 1. / 0.	-- also {REAL_64}.positive_infinity
			negZero := -1. / posInf
			nan := 0. / 0.		-- also {REAL_64}.nan

			print("Negative Infinity: ") print(negInf) print("%N")
			print("Positive Infinity: ") print(posInf) print("%N")
			print("Negative Zero: ") print(negZero) print("%N")
			print("NaN: ") print(nan) print("%N%N")

			print("1.0 + Infinity = ") print((1.0 + posInf)) print("%N")
			print("1.0 - Infinity = ") print((1.0 - posInf)) print("%N")
			print("-Infinity + Infinity = ") print((negInf + posInf)) print("%N")
			print("-0.0 * Infinity = ") print((negZero * posInf)) print("%N")
			print("NaN + NaN = ") print((nan + nan)) print("%N")
			print("(NaN = NaN) = ") print((nan = nan)) print("%N")
			print("(0.0 = -0.0) = ") print((0.0 = negZero)) print("%N")
		end
end
```

{{out}}

```txt
Negative Infinity: -Infinity
Positive Infinity: Infinity
Negative Zero: -0
NaN: NaN

1.0 + Infinity = Infinity
1.0 - Infinity = -Infinity
-Infinity + Infinity = NaN
-0.0 * Infinity = NaN
NaN + NaN = NaN
(NaN = NaN) = False
(0.0 = -0.0) = True

```



## Euphoria

{{trans|C}}

```Euphoria
constant inf = 1E400
constant minus_inf = -inf
constant nan = 0*inf

printf(1,"positive infinity: %f\n", inf)
printf(1,"negative infinity: %f\n", minus_inf)
printf(1,"not a number: %f\n", nan)

-- some arithmetic

printf(1,"+inf + 2.0 = %f\n", inf + 2.0)
printf(1,"+inf - 10.1 = %f\n", inf - 10.1)
printf(1,"+inf + -inf = %f\n", inf + minus_inf)
printf(1,"0.0 * +inf = %f\n", 0.0 * inf)
printf(1,"NaN + 1.0 = %f\n", nan + 1.0)
printf(1,"NaN + NaN = %f\n", nan + nan)
```


{{out}}
 positive infinity: inf
 negative infinity: -inf
 not a number: -nan
 +inf + 2.0 = inf
 +inf - 10.1 = inf
 +inf + -inf = -nan
 0.0 * +inf = -nan
 NaN + 1.0 = -nan
 NaN + NaN = -nan

=={{header|F_Sharp|F#}}==

```fsharp

0.0/0.0            //->nan
0.0/(-0.0)         //->nan
1.0/infinity       //->0.0
1.0/(-infinity)    //->0.0
1.0/0.0            //->infinity
1.0/(-0.0)         //->-infinity
-infinity<infinity //->true
(-0.0)<0.0         //->false

```


## Factor


```factor
-0.             . ! -0.0  literal negative zero
 0. neg         . ! -0.0  neg works with floating point zeros
 0. -1. *       . ! -0.0  calculating negative zero
 1/0.           . !  1/0. literal positive infinity
 1e3 1e3 ^      . !  1/0. calculating positive infinity
-1/0.           . ! -1/0. literal negative infinity
-1. 1e3 1e3 ^ * . ! -1/0. calculating negative infinity
-1/0. neg       . !  1/0. neg works with the inifinites
 0/0.           . !  NAN: 8000000000000 literal NaN, configurable with
                  !                     arbitrary 64-bit hex payload
 1/0. 1/0. -    . !  NAN: 8000000000000 calculating NaN by subtracting
                  !                     infinity from infinity
```



## Forth

{{works with|GNU Forth}}

```forth
 1e 0e f/ f.     \ inf
-1e 0e f/ f.     \ inf (output bug: should say "-inf")
-1e 0e f/ f0< .  \ -1 (true, it is -inf)
 0e 0e f/ f.     \ nan
-1e 0e f/ 1/f f0< .   \ 0 (false, can't represent IEEE negative zero)
```



## Fortran


### Honest numbers

The floating-point number services offered by computers over the decades have varied greatly in format and behaviour, being in base ten, two, four, eight, or sixteen, and of various storage sizes, with various choices for precision and exponent range - for a fixed size, more precision means a smaller dynamic range and ''vice-versa''. The IBM1620 offered (discrete transistor) floating-point hardware that allowed two decimal digits for the exponent and from two to 100 decimal digits for the mantissa; extreme values are easily deduced. For base two (or four, etc.) computers, presenting the exact extreme value in decimal produces troublesomely long strings of digits, and there is no clear guarantee that such a value, when converted by the compiler, will in fact manifest the desired extreme value in binary - the compiler is itself using computer arithmetic of limited precision. One must know ''exactly'' what format is used for floating-point numbers, on the specific computer in question. And if a programme using those values is moved to a different computer or a different compiler, you may well have to start again.

F90 however contains facilities to help. Pseudo-function HUGE(x) returns the largest possible number of the type of its parameter - whether an integer or a floating-point variable, of single or double precision, etc. But, this does not solve the problem, as if you are dealing with a computer that represents integers in two's complement, the maximum sixteen-bit number is 32767 but for negative numbers it is -32768. Thus, if you intend to find the extrema of a set of numbers and it is not convenient to set MinX and MaxX to the first value, and you don't want to have special case code testing for N = 1, you might try MinX = HUGE(Minx) and MaxX = -HUGE(MaxX) and gain wrong results should there for instance be only one value and it -32768.

There is a TINY(x) pseudo-function, for floating-point types only, that gives the smallest possible floating-point number, but, it is not clear whether this is the smallest possible normalised floating-point number, or, does it allow "denormalised" floating-point numbers that are even smaller?

Still further pseudo-functions offer PRECISION(x), EPSILON(x) and RADIX(x) whereby one can determine whether the implicit leading-one of normalised base two floating-point numbers is in use or not. Thanks to the proliferation of Intel 8087 ''et seq'' floating-point processors, single and double precision numbers on many modern computers use an implicit leading-one bit, but, the 80-bit format does not, and it allows denormalised numbers.

===Peculiar "numbers"===
Modern computers following the Intel 8087 also reserve some bit patterns to represent what really aren't floating-point numbers at all. To be finicky, for example, zero cannot be represented as a normalised floating-point number, but nearly every design finds a way to represent zero - possibly as the smallest possible number if not an actual zero. Useful additions include ±underflow, ±overflow for finite numbers resulting from arithmetic that require an exponent part that is too large or too small to be represented. Thus, underflow is not zero, and overflow is not infinity. As well, there are representations of ±infinity, though this doesn't solve the MinX, MaxX problem above as these states are not available for integer variables. Oddest of all is "Not a Number" - which, to be finicky can't be called an ''extreme'' floating-point value and it declares itself not to be a ''number'' anyway. But, it ''is'' a possible state of a variable of the ''modern'' floating-point type.

Certain calculations are said to benefit from the states "positive zero", and "negative zero" being available in recondite ways, and theoretical investigations of differentiation can be recast to use an "infinitesimal" adduced to the Real number system, but these notions are even further away from "normal" number crunching.

Fortran does not recognise names for these states, as in <code>X = +Inf</code>, though later compilers for systems that do offer these states ''do'' produce +Infinity on output, or NaN, and also recognise these texts when being read in a numeric field. Further, the special logical function IsNaN(x) is the only safe way to detect a bit pattern representing NaN (there are many) because the comparison operators behave oddly by design. A test X = X returns ''false'' if x has the NaN state (if not optimised away by the compiler to always be ''true'' as per the millenia-old definition of equality), but x ¬= NaN may be compiled as ¬(x = Nan) to further confusion.

Similarly, the library functions may or may not recognise these special values and "pass them through" in ways that might be expected. For instance, ABS(NaN) returns NaN, but EXP(NaN) delivers a run-time error - at least for Compaq Visual Fortran 6.6 F90/95. Nor might they generate them as could be hoped for. For instance, ATAN(x,y) would be used in converting from Cartesian (x,y) coordinates to angular coordinates <r,a> = <sqrt(x^2 + y^2),atan(x,y)>  where a is the angle. ATAN(0,0) could return NaN, since a zero-length vector points in no direction, but if so, converting back via (x,y) = (r*cos(a),r*sin(a)) will not return (0,0) unless 0*NaN gives 0, which it doesn't. And it may not be clear what special value should be generated anyway. For instance, TAND(90) - which represents ninety degrees ''exactly'' unlike TAN(pi/2) which cannot - should yield Infinity as its result, but, which sign?

Since there are multiple bit patterns that constitute a NaN state, there is an opportunity for an affronted function to set a specific bit according to the error. Thus there could be one bit to mark a sqrt(neg), another for log(not positive), and so on. Later examination of a variable containing a bad state could give some provenance to the problem. Organising this would require a lot of work in standardisation fora.

It is also possible to set various options for the processing of floating-point numbers that affects rounding and much else. Confusion will almost certainly be the result.


### Pragmatics

To prepare variables with these non-numerical states is troublesome, because attaining infinity by x = 1/0 or the like is not only bad behaviour, it invites complaint from the compiler or the generation of a run-time error and immediate cancellation of the run. One could mess about by using a READ statement on special texts, but that prevents the results being constants. Instead, one studies the definitions and devises code such as ...

```Fortran

       REAL*8		BAD,NaN			!Sometimes a number is not what is appropriate.
       PARAMETER (NaN = Z'FFFFFFFFFFFFFFFF')	!This value is recognised in floating-point arithmetic.
       PARAMETER (BAD = Z'FFFFFFFFFFFFFFFF')	!I pay special attention to BAD values.
       CHARACTER*3	BADASTEXT		!Speakable form.
       DATA		BADASTEXT/" ? "/	!Room for "NaN", short for "Not a Number", if desired.
       REAL*8		PINF,NINF		!Special values. No sign of an "overflow" state, damnit.
       PARAMETER (PINF = Z'7FF0000000000000')	!May well cause confusion
       PARAMETER (NINF = Z'FFF0000000000000')	!On a cpu not using this scheme.

```

After experimenting with code such as

```Fortran

Cause various arithmetic errors to see what sort of hissy fit is thrown.
      REAL X2,X3,X4,Y4,XX,ZERO
      INTEGER IX4,IY4
      EQUIVALENCE (X4,IX4),(Y4,IY4)	!To view bits without provoking special fp handling.
      REAL*4 NaN4
      PARAMETER (NaN4 = Z'FFC00000')	!FFFFFFFF
c      PARAMETER (NaN4 = Z'FFFFFFFF')	!FFFFFFFF
      REAL*8 NaN8,X8(5),Y8,INF8
      PARAMETER (NaN8 = Z'FFF8000000000000')	!FFFFFFFF
c      PARAMETER (NaN8 = Z'FFFFFFFFFFFFFFFF')
      LOGICAL LX(5)
      INTEGER I
      X4 = NaN4
      WRITE (6,1) X4,IX4
    1 FORMAT ("X4 =",F12.4,' Hex ',Z8)
      WRITE (6,*) "Test X4 .EQ. Bad?  ",X4.EQ.NaN4
      WRITE (6,*) "Test X4 .NE. Bad?  ",X4.NE.NaN4
      WRITE (6,*) "Test IsNaN(X4)  ",ISNAN(X4)
      WRITE (6,*) "Test Abs(bad)   ",ABS(X4)
c      WRITE (6,*) "Test Exp(bad)",EXP(X4)
      Y8 = HUGE(Y8)
      WRITE(6,*) "Huge",Y8,LOG(Y8)
      Y8 = LOG(Y8)
      WRITE (6,*) "Hic",EXP(Y8)

      X2 = 0
      X3 = 0
      ZERO = 0
      XX = 666.66
      X2 = XX + X4
      WRITE (6,*) "Test x + BAD    ",X2
      WRITE (6,*) "Test 0/0        ",X3/ZERO
      WRITE (6,*) "Test 1/0        ",1/ZERO
      WRITE (6,*) "Test-1/0        ",-1/ZERO
      X2 = MIN(XX,X4)
      WRITE (6,*) "Test min(x,Bad) ",X2
      WRITE (6,*) "Test min(x,NaN4)",MIN(XX,NaN4)
c      WRITE (6,*) "Test mod(x,Bad) ",MOD(XX,X4)
c      WRITE (6,*) "Test mod(Bad,x) ",MOD(X4,XX)
c      WRITE (6,*) "Test mod(x,0)   ",MOD(XX,Z)
c      WRITE (6,*) "Sqrt(Bad)",SQRT(X4)

      DO I = 1,0,-1	!for sqrt(-1), a snarl.
        X4 = I
        X4 = X4/FLOAT(I)
        Y4 = SQRT(FLOAT(I))
        WRITE (6,10) I,I,X4,IX4,I,Y4,IY4
   10   FORMAT (I3,"/",I3," gives",F9.5," Hex ",Z8,
     1   ", Sqrt(",I3,") gives",F9.5," Hex ",Z8)
      END DO

Contemplate double precision.
      WRITE (6,*)
      WRITE (6,*) "Problems with IsNaN and arrays..."
      DO I = 1,5
        X8(I) = I
      END DO
      X8(3:4) = NaN8
      WRITE (6,*) "X=",X8
      WRITE (6,*) "X(2:4)=",X8(2:4)
      WRITE (6,*) "isnan(x(2:4))",ISNAN(X8(2:4))
      WRITE (6,*) "isnan(x(2))..(4))",ISNAN(X8(2)),ISNAN(X8(3)),
     1 ISNAN(X8(4))
      WRITE (6,*) "abs(x(2:4))",ABS(X8(2:4))
      WRITE (6,*) "isnan(abs(x(2:4)))",ISNAN(ABS(X8(2:4)))
      LX = ISNAN(X8)
      WRITE (6,*) "LX = isnan(X)",LX

      XX = HUGE(XX)
      WRITE(6,*) "Huge(x)=",XX,-XX
      XX = 1/ZERO
      WRITE(6,11) XX,-XX
   11 FORMAT("1/Zero=",Z8,", neg ",Z8)
      INF8 = XX
      WRITE (6,12) INF8,-INF8
   12 FORMAT("1/Zero=",Z16,", neg ",Z16)
      WRITE (6,*) "Burp!"
      END

```

Which provides output such as

```txt

X4 = NaN         Hex FFC00000
 Test X4 .EQ. Bad?   F
 Test X4 .NE. Bad?   T
 Test IsNaN(X4)   T
 Test Abs(bad)    NaN
 Huge  1.797693134862316E+308   709.782712893384
 Hic  1.797693134862273E+308
 Test x + BAD     NaN
 Test 0/0         NaN
 Test 1/0         Infinity
 Test-1/0         -Infinity
 Test min(x,Bad)    666.6600
 Test min(x,NaN4)   666.6600
  1/  1 gives  1.00000 Hex 3F800000, Sqrt(  1) gives  1.00000 Hex 3F800000
  0/  0 gives NaN      Hex FFC00000, Sqrt(  0) gives  0.00000 Hex        0

 Problems with IsNaN and arrays...
 X=   1.00000000000000        2.00000000000000      NaN
 NaN                       5.00000000000000
 X(2:4)=   2.00000000000000      NaN                     NaN
 isnan(x(2:4)) T T F
 isnan(x(2))..(4)) F T T
 abs(x(2:4))   2.00000000000000      NaN
 NaN
 isnan(abs(x(2:4))) F T T
 LX = isnan(X) F F T T F
 Huge(x)=  3.4028235E+38 -3.4028235E+38
1/Zero=7F800000, neg FF800000
1/Zero=7FF0000000000000, neg FFF0000000000000
 Burp!

```

Some functions "pass through" bad values, and some raise an error and stop the run.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#Include "crt/math.bi"

Dim inf     As Double = INFINITY
Dim negInf  As Double = -INFINITY
Dim notNum  As Double = NAN_
Dim negZero As Double = 1.0 / negInf

Print inf,     inf / inf
Print negInf,  negInf * negInf
Print notNum,  notNum + inf + negInf
Print negZero, negZero - 1
Sleep
```


{{out}}

```txt

 1.#INF       -1.#IND
-1.#INF        1.#INF
-1.#IND       -1.#IND
-0            -1

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    // compute "extreme values" from non-extreme values
    var zero float64                         // zero is handy.
    var negZero, posInf, negInf, nan float64 // values to compute.
    negZero = zero * -1
    posInf = 1 / zero
    negInf = -1 / zero
    nan = zero / zero

    // print extreme values stored in variables
    fmt.Println(negZero, posInf, negInf, nan)

    // directly obtain extreme values
    fmt.Println(math.Float64frombits(1<<63),
        math.Inf(1), math.Inf(-1), math.NaN())

    // validate some arithmetic on extreme values
    fmt.Println()
    validateNaN(negInf+posInf, "-Inf + Inf")
    validateNaN(0*posInf, "0 * Inf")
    validateNaN(posInf/posInf, "Inf / Inf")
    // mod is specifically named in "What every computer scientist..."
    // Go math package doc lists many special cases for other package functions.
    validateNaN(math.Mod(posInf, 1), "Inf % 1")
    validateNaN(1+nan, "1 + NaN")
    validateZero(1/posInf, "1 / Inf")
    validateGT(posInf, math.MaxFloat64, "Inf > max value")
    validateGT(-math.MaxFloat64, negInf, "-Inf < max neg value")
    validateNE(nan, nan, "NaN != NaN")
    validateEQ(negZero, 0, "-0 == 0")
}

func validateNaN(n float64, op string) {
    if math.IsNaN(n) {
        fmt.Println(op, "-> NaN")
    } else {
        fmt.Println("!!! Expected NaN from", op, "  Found", n)
    }
}

func validateZero(n float64, op string) {
    if n == 0 {
        fmt.Println(op, "-> 0")
    } else {
        fmt.Println("!!! Expected 0 from", op, "  Found", n)
    }
}

func validateGT(a, b float64, op string) {
    if a > b {
        fmt.Println(op)
    } else {
        fmt.Println("!!! Expected", op, "  Found not true.")
    }
}

func validateNE(a, b float64, op string) {
    if a == b {
        fmt.Println("!!! Expected", op, "  Found not true.")
    } else {
        fmt.Println(op)
    }
}

func validateEQ(a, b float64, op string) {
    if a == b {
        fmt.Println(op)
    } else {
        fmt.Println("!!! Expected", op, "  Found not true.")
    }
}
```

{{out}}

```txt

-0 +Inf -Inf NaN
-0 +Inf -Inf NaN

-Inf + Inf -> NaN
0 * Inf -> NaN
Inf / Inf -> NaN
Inf % 1 -> NaN
1 + NaN -> NaN
1 / Inf -> 0
Inf > max value
-Inf < max neg value
NaN != NaN
-0 == 0

```



## Groovy

{{Trans|Java}}
Solution:

```groovy
def negInf = -1.0d / 0.0d;   //also Double.NEGATIVE_INFINITY
def inf = 1.0d / 0.0d;       //also Double.POSITIVE_INFINITY
def nan = 0.0d / 0.0d;       //also Double.NaN
def negZero = -2.0d / inf;

println("  Negative inf: " + negInf);
println("  Positive inf: " + inf);
println("           NaN: " + nan);
println("    Negative 0: " + negZero);
println("    inf + -inf: " + (inf + negInf));
println("       0 * NaN: " + (0 * nan));
println("    NaN == NaN: " + (nan == nan));
println("NaN equals NaN: " + (nan.equals(nan)));
```


{{out}}

```txt
  Negative inf: -Infinity
  Positive inf: Infinity
           NaN: NaN
    Negative 0: -0.0
    inf + -inf: NaN
       0 * NaN: NaN
    NaN == NaN: true
NaN equals NaN: true
```


Note that the Groovy implementation of 'equals' incorrectly allows that "NaN == NaN" is true.
In a correct IEEE implementation NaN is never equal to '''anything''', including itself.


## haskell


```haskell

main = do
let inf = 1/0
let minus_inf = -1/0
let minus_zero = -1/inf
let nan = 0/0

putStrLn ("Positive infinity = "++(show inf))
putStrLn ("Negative infinity = "++(show minus_inf))
putStrLn ("Negative zero = "++(show minus_zero))
putStrLn ("Not a number = "++(show nan))

--Some Arithmetic

putStrLn ("inf + 2.0 = "++(show (inf+2.0)))
putStrLn ("inf - 10 = "++(show (inf-10)))
putStrLn ("inf - inf = "++(show (inf-inf)))
putStrLn ("inf * 0 = "++(show (inf * 0)))
putStrLn ("nan + 1.0= "++(show (nan+1.0)))
putStrLn ("nan + nan = "++(show (nan + nan)))

--Some Comparisons

putStrLn ("nan == nan = "++(show (nan == nan)))
putStrLn ("0.0 == - 0.0 = "++(show (0.0 == minus_zero)))
putStrLn ("inf == inf = "++(show (inf == inf)))

```

{{out}}

```txt

Positive infinity = Infinity
Negative infinity = -Infinity
Negative Zero = -0.0
Not a number = NaN
inf + 2.0 = Infinity
inf - 10 = Infinity
inf - inf = NaN
inf * 0 = NaN
nan + 1.0 = NaN
nan + nan = NaN
nan == nan = False
0.0 == -0.0 = True
inf == inf = True

```


=={{header|Icon}} and {{header|Unicon}}==

Icon and Unicon don't define minimum or maximum values of reals, or a negative 0.0.  Real numbers are implemented as C doubles and the behavior could vary somewhat from platform to platform.
Both explicitly check for divide by zero and treat it as a runtime error (201), so it's not clear how you could produce one of these with the possible exception of the value being introduced through externally called code.


## J


'''Extreme values'''

```j
   Inf=: _
   NegInf=: __
   NB. Negative zero cannot be represented in J to be distinct from 0.
   NaN=. _.
```

The numeric atom <code>_.</code> ([[j:Essays/Indeterminate|Indeterminate]]) is provided as a means for dealing with NaN in data from sources outside J.
J itself generates NaN errors rather than NaN values and recommends that <code>_.</code> be removed from data as soon as possible because, by definition, NaN values will produce inconsistent results in contexts where value is important.

'''Extreme values from expressions'''

```j
   (1 % 0) , (_1 % 0)
_ __
   (1e234 * 1e234) , (_1e234 * 1e234)
_ __
   _ + __         NB. generates NaN error, rather than NaN
|NaN error
|   _    +__

   _ - _          NB. generates NaN error, rather than NaN
|NaN error
|   _    -_
   %_
0
  %__             NB. Under the covers, the reciprocal of NegInf produces NegZero, but this fact isn't exposed to the user, who just sees zero
0

```


'''Some arithmetic'''

```j
   _ + _
_
   __ + __
__
   Inf + 0
_
   NegInf * 0
0
```



## Java


```java
public class Extreme {
    public static void main(String[] args) {
        double negInf = -1.0 / 0.0; //also Double.NEGATIVE_INFINITY
        double inf = 1.0 / 0.0; //also Double.POSITIVE_INFINITY
        double nan = 0.0 / 0.0; //also Double.NaN
        double negZero = -2.0 / inf;

        System.out.println("Negative inf: " + negInf);
        System.out.println("Positive inf: " + inf);
        System.out.println("NaN: " + nan);
        System.out.println("Negative 0: " + negZero);
        System.out.println("inf + -inf: " + (inf + negInf));
        System.out.println("0 * NaN: " + (0 * nan));
        System.out.println("NaN == NaN: " + (nan == nan));
    }
}
```

{{out}}

```txt
Negative inf: -Infinity
Positive inf: Infinity
NaN: NaN
Negative 0: -0.0
inf + -inf: NaN
0 * NaN: NaN
NaN == NaN: false
```



## jq


jq uses IEEE 754 64-bit numbers, and certain numeric expressions yield the exceptional floating point values in the usual way.
However, since JSON does not support such values, jq currently prints the NaN value as null, and the infinite value as a very large float, so some care is required
in interpreting the printed values.

For example, here are two expressions and the result of displaying their values:

```jq>0/0    #=
 null
1e1000 #=> 1.7976931348623157e+308
```


If your jq does not already have `infinite` and `nan` defined as built-in functions, they can be defined as follows:


```jq
def infinite: 1e1000;
def nan: 0/0;
```


Here are some further expressions with their results:


```jq
-0                                #=> -0
0 == -0                           # => true
infinite == infinite              #=> true
infinite == -(-infinite)          #=> true
(infinite + infinite) == infinite #=> true
1/infinite                        #=> 0

nan == nan                        #=> false # N.B.
```


Since `==` cannot be used to check if a value is IEEE NaN, jq 1.5 provides the builtin function `isnan` for doing so:

```jq
nan | isnan                       #=> true
infinite | isnan                  #=> false
```


Exceptional values can be assigned to jq variables in the usual way:

```jq
infinite as $inf | 1 / $inf       #=> 0
-0 as $z | $z                     #=> -0
```



## Julia


```julia

function showextremes()
  values = [0.0, -0.0, Inf, -Inf, NaN]
  println(1 ./ values)
end

showextremes()

@show Inf + 2.0
@show Inf + Inf
@show Inf - Inf
@show Inf * Inf
@show Inf / Inf
@show Inf * 0
@show 0 == -0
@show NaN == NaN
@show NaN === NaN

```


{{out}}

```txt

[Inf,-Inf,0.0,-0.0,NaN]
Inf + 2.0 = Inf
Inf + Inf = Inf
Inf - Inf = NaN
Inf * Inf = Inf
Inf / Inf = NaN
Inf * 0 = NaN
0 == 0 = true
NaN == NaN = false
NaN === NaN = true

```



## Kotlin


```scala
// version 1.0.5-2

@Suppress("DIVISION_BY_ZERO", "FLOAT_LITERAL_CONFORMS_ZERO")

fun main(args: Array<String>) {
    val inf     =  1.0 / 0.0
    val negInf  = -1.0 / 0.0
    val nan     =  0.0 / 0.0
    val negZero = -1.0e-325

    println("*** Indirect ***\n")
    println("Infinity          :  $inf")
    println("Negative infinity :  $negInf")
    println("Not a number      :  $nan")
    println("Negative zero     :  $negZero")

    println("\n*** Direct ***\n")
    println("Infinity          :  ${Double.POSITIVE_INFINITY}")
    println("Negative infinity :  ${Double.NEGATIVE_INFINITY}")
    println("Not a number      :  ${Double.NaN}")
    println("Negative zero     :  ${-0.0}")

    println("\n*** Calculations ***\n")
    println("inf * inf         :  ${inf * inf}")
    println("inf + negInf      :  ${inf + negInf}")
    println("nan / nan         :  ${nan / nan}")
    println("negZero + 0.0     :  ${negZero + 0.0}")
}
```


{{out}}

```txt

*** Indirect ***

Infinity          :  Infinity
Negative infinity :  -Infinity
Not a number      :  NaN
Negative zero     :  -0.0

*** Direct ***

Infinity          :  Infinity
Negative infinity :  -Infinity
Not a number      :  NaN
Negative zero     :  -0.0

*** Calculations ***

inf * inf         :  Infinity
inf + negInf      :  NaN
nan / nan         :  NaN
negZero + 0.0     :  0.0

```



## Lua

Infinity and NaN are straight forward. for negative 0 you need to resort to a literal with a large,
negative exponent, which is not the same thing.

```lua


local inf=math.huge
local minusInf=-math.huge
local NaN=0/0
local negativeZeroSorta=-1E-240

```

Lua seems to break x==1/(1/x) for infinity:

```lua

1/(1/-math.huge)==math.huge
true

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
Column@{ReleaseHold[
   Function[expression,
     Row@{HoldForm@InputForm@expression, " = ", Quiet@expression},
     HoldAll] /@
    Hold[1./0., 0./0., Limit[-Log[x], x -> 0], Limit[Log[x], x -> 0],
     Infinity + 1, Infinity + Infinity, 2 Infinity,
     Infinity - Infinity, 0 Infinity, ComplexInfinity + 1,
     ComplexInfinity + ComplexInfinity, 2 ComplexInfinity,
     0 ComplexInfinity, Indeterminate + 1, 0 Indeterminate]]}
```

{{out}}

```txt
1./0. = ComplexInfinity
0./0. = Indeterminate
Limit[-Log[x], x -> 0] = ∞
Limit[Log[x], x -> 0] = -∞
Infinity + 1 = ∞
Infinity + Infinity = ∞
2*Infinity = ∞
Infinity - Infinity = Indeterminate
0*Infinity = Indeterminate
ComplexInfinity + 1 = ComplexInfinity
ComplexInfinity + ComplexInfinity = Indeterminate
2*ComplexInfinity = ComplexInfinity
0*ComplexInfinity = Indeterminate
Indeterminate + 1 = Indeterminate
0*Indeterminate = Indeterminate
```



## Maxima

With ordinary floating point numbers, <code>1.0 / 0.0</code>, <code>0.0 / 0.0</code> or <code>1e300^2</code> all throw an exception.

However, Maxima has big floats and knows how to manage the <code>inf</code>, <code>minf</code> and <code>infinity</code> symbols
(resp. positive, negative and complex infinity), with the function <code>limit</code>. It also has <code>zeroa</code> and <code>zerob</code>
for positive and negative infinitesimal (though their usage is quite obscure),
and <code>und</code> for undefined value.


## MUMPS


### ANSI MUMPS

The 1995 Standard MUMPS (X11.1–1995) implementations do not deal with floating point numbers following IEEE 754.
Attempting to use a number over the precision of the system results in a <MAXNUMBER> error:

```txt
USER>write 3e145
30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
USER>write 3e146

<MAXNUMBER>

```


===Intersystems Caché===
Caché has the function $DOUBLE which complies with the IEEE 754 standard. The negative zero is indistinguishable from positive zero by operations. The special values evaluate to 0 when converted to a number in a later operation.

```MUMPS

EXTREMES
  NEW INF,NINF,ZERO,NOTNUM,NEGZERO
  SET INF=$DOUBLE(3.0E310),NINF=$DOUBLE(-3.0E310),ZERO=$DOUBLE(0),NOTNUM=$DOUBLE(INF-INF),NEGZERO=$DOUBLE(ZERO*-1)
  WRITE "Infinity: ",INF,!
  WRITE "Infinity ",$SELECT($ISVALIDNUM(INF):"is a number",1:"is not a number"),!
  WRITE "Negative Infinity: ",NINF,!
  WRITE "Negative Infinity ",$SELECT($ISVALIDNUM(NINF):"is a number",1:"is not a number"),!
  WRITE "Zero: ",ZERO,!
  WRITE "Zero ",$SELECT($ISVALIDNUM(ZERO):"is a number",1:"is not a number"),!
  WRITE "Negative Zero: ",NEGZERO,!
  WRITE "Negative Zero ",$SELECT($ISVALIDNUM(NEGZERO):"is a number",1:"is not a number"),!
  WRITE "Not a Number: ",NOTNUM,!
  WRITE "Not a Number ",$SELECT($ISVALIDNUM(NOTNUM):"is a number",1:"is not a number"),!
  KILL INF,NINF,ZERO,NONNUM,NEGZERO
 QUIT

```

{{out}}

```txt
USER>d EXTREMES^ROSETTA
Infinity: INF
Infinity is not a number
Negative Infinity: -INF
Negative Infinity is not a number
Zero: 0
Zero is a number
Negative Zero: 0
Negative Zero is a number
Not a Number: NAN
Not a Number is not a number
```



## NetRexx

While [[NetRexx]] native support for numbers allows for very large decimal precision, the [[Java]] primitives (<tt>int</tt>, <tt>long</tt>, <tt>float</tt>, <tt>double</tt> etc.), can use the constants and methods provided for &quot;extreme&quot; values:
{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

negInf  = double -1.0 / 0.0; knegInf  = Double.NEGATIVE_INFINITY
inf     = double  1.0 / 0.0; kinf     = Double.POSITIVE_INFINITY
nan     = double  0.0 / 0.0; knan     = Double.NaN
negZero = double -2.0 / inf; knegZero = -2.0 / Double.POSITIVE_INFINITY

say "Negative inf: "         Rexx(negInf).right(10) '|' knegInf
say "Positive inf: "            Rexx(inf).right(10) '|' kinf
say "NaN:          "            Rexx(nan).right(10) '|' knan
say "Negative 0:   "        Rexx(negZero).right(10) '|' knegZero
say "inf + -inf:   "   Rexx(inf + negInf).right(10) '|' (kinf + knegInf)
say "0 * NaN:      "        Rexx(0 * nan).right(10) '|' (0 * knan)
say "NaN == NaN:   "     Rexx(nan == nan).right(10) '|' (knan == knan)

return

```

{{out}}

```txt

Negative inf:    Infinity | Infinity
Positive inf:    Infinity | Infinity
NaN:                  NaN | NaN
Negative 0:             0 | 0
inf + -inf:           NaN | NaN
0 * NaN:              NaN | NaN
NaN == NaN:             0 | 0

```



## Nim


```nim
echo 1e234 * 1e234 # inf
echo 1e234 * -1e234 # -inf
echo 1 / inf # 0
echo inf + -inf # -nan
echo nan # nan

echo nan == nan # false
echo 0.0 == -0.0 # true
echo 0.0 * nan # nan
echo nan * 0.0 # nan
echo 0.0 * inf # -nan
echo inf * 0.0 # -nan
```



## OCaml


```ocaml
# infinity;;
- : float = infinity
# neg_infinity;;
- : float = neg_infinity
# nan;;
- : float = nan
# -0.;;
- : float = -0.
# -. 0.;;
- : float = -0.
# 1. /. 0.;;
- : float = infinity
# -1. /. 0.;;
- : float = neg_infinity
# -. infinity;;
- : float = neg_infinity
# infinity +. neg_infinity;;
- : float = nan
# 0. /. 0.;;
- : float = nan
# infinity /. infinity;;
- : float = nan
# nan = nan;;
- : bool = false
# nan == nan;;
- : bool = true
# 0. *. infinity;;
- : float = nan
# 0. = -0.;;
- : bool = true
# 0. == -0.;;
- : bool = false
```



## Oforth


In Oforth, the only 'extreme' floating point values are PInfinity (+oo) and NInfinity (-oo).

{{out}}

```txt

>10.0 1000.0 powf PInf == println
1
ok
>10.0 1000.0 powf neg NInf == println
1
ok

```



## Oz


```oz
declare
Inf = 1.0e234 * 1.0e234
MinusInf = 1.0e234 * ~1.0e234
Zero = 1.0 / Inf
MinusZero = 1.0 / MinusInf
NaN = 0.0 / 0.0

{System.showInfo "infinite: "#Inf}
{System.showInfo "-infinite: "#MinusInf}
{System.showInfo "0: "#Zero}
{System.showInfo "-0: "#MinusZero}  %% seems to be identical to Zero
{System.showInfo "NaN: "#NaN}

{System.showInfo "inf + -inf: "#Inf+MinusInf}
{System.showInfo "NaN * 0: "#NaN*0.0}
{System.showInfo "0 * NaN: "#0.0*NaN}
{System.showInfo "inf * 0: "#Inf*0.0}
{System.showInfo "0 * inf: "#0.0*Inf}

{Show NaN == NaN}  %% shows 'true' !
{Show Zero == MinusZero}

{Show 1.0/0.0 == Inf} %% true
{Show 1.0/~0.0 == MinusInf} %% true
```


{{out}}

```oz
infinite: 1.#INF
-infinite: -1.#INF
0: 0.0
-0: 0.0
NaN: -1.#IND
inf + -inf: -1.#IND
NaN * 0: -1.#IND
0 * NaN: -1.#IND
inf * 0: -1.#IND
0 * inf: -1.#IND
true
true
true
true
```



## PARI/GP

PARI t_REALs are not IEEE floating-point numbers; in particular they cannot store NaN or infinite values. (The latter have their own type, t_INFINITY, with values <code>+oo</code> and <code>-oo</code>.)

PARI t_REAL numbers have a maximum value of

{|class="wikitable"
! 32-bit
| <math>2^{2^{29}}(1-\epsilon)</math>
| 161,614,249 decimal digits
|-
! 64-bit
| <math>2^{2^{61}}(1-\epsilon)</math>
| 694,127,911,065,419,642 decimal digits
|}
where <math>\epsilon</math> is the machine epsilon at the selected precision.
The minimum value is the opposite of the maximum value (reverse the sign bit).


## Pascal

See [[Extreme_floating_point_values#Delphi | Delphi]]


## Perl

Perl numbers have three formats (integer, floating-point, string) and [http://perldoc.perl.org/perlnumber.html perlnumber] explains the automatic conversions. Arithmetic tends to convert numbers to integers.
To get negative zero, one must negate a floating-point zero, not an integer zero.
So <tt>-0</tt> is "0", <tt>-0.0</tt> is "-0", but <tt>-(1.0 - 1.0)</tt> is again "0" because the result of <tt>1.0 - 1.0</tt> is an integer zero.
Stringification of minus zero may or may not keep the sign in the string, depending on the platform and Perl version.
If the sign is important, use <code>printf "%f"</code> instead (<code>"%g"</code> won't work: it gives "0").

Division by zero, <tt>sqrt(-1)</tt> and <tt>log(0)</tt> are fatal errors.
To get infinity and NaN, use corresponding string and force a numeric conversion by adding zero to it, or prepending a "+" or "-":

```perl
#!/usr/bin/perl
use strict;
use warnings;

my $nzero = -0.0;
my $nan = 0 + "nan";
my $pinf = +"inf";
my $ninf = -"inf";

printf "\$nzero = %.1f\n", $nzero;
print "\$nan = $nan\n";
print "\$pinf = $pinf\n";
print "\$ninf = $ninf\n\n";

printf "atan2(0, 0) = %g\n", atan2(0, 0);
printf "atan2(0, \$nzero) = %g\n", atan2(0, $nzero);
printf "sin(\$pinf) = %g\n", sin($pinf);
printf "\$pinf / -1 = %g\n", $pinf / -1;
printf "\$ninf + 1e100 = %g\n\n", $ninf + 1e100;

printf "nan test: %g\n", (1 + 2 * 3 - 4) / (-5.6e7 * $nan);
printf "nan == nan? %s\n", ($nan == $nan) ? "yes" : "no";
printf "nan == 42? %s\n", ($nan == 42) ? "yes" : "no";
```


{{out}}

```txt
$nzero = -0.0
$nan = nan
$pinf = inf
$ninf = -inf

atan2(0, 0) = 0
atan2(0, $nzero) = 3.14159
sin($pinf) = nan
$pinf / -1 = -inf
$ninf + 1e100 = -inf

nan test: nan
nan == nan? no
nan == 42? no
```


----

Here is a rare example of NaN and infinity for an ''integer'' type.
Math::BigInt, a module that comes with Perl, provides integers of arbitrary sizes,
but also has NaN, positive infinity, and negative infinity.
There is no negative zero.


```perl
#!/usr/bin/perl
use strict;
use warnings;

use Math::BigInt;

my $nan = Math::BigInt->bnan();
my $pinf = Math::BigInt->binf();
my $ninf = Math::BigInt->binf('-');

print "\$nan = $nan\n";
print "\$pinf = $pinf\n";
print "\$ninf = $ninf\n\n";

my $huge = Math::BigInt->new("123456789");
$huge->bmul($huge)->bmul($huge)->bmul($huge);

print "\$huge = $huge\n";
printf "\$ninf + \$huge = %s\n", $ninf->copy()->badd($huge);
printf "\$pinf - \$huge = %s\n", $pinf->copy()->bsub($huge);
printf "\$nan * \$huge = %s\n", $nan->copy()->bmul($huge);
printf "\$nan == \$nan? %s\n", defined($nan->bcmp($nan)) ? "maybe" : "no";
printf "\$nan == \$huge? %s\n", defined($nan->bcmp($huge)) ? "maybe" : "no";
```


{{out}}

```txt
$nan = NaN
$pinf = inf
$ninf = -inf

$huge = 53965948844821664748141453212125737955899777414752273389058576481
$ninf + $huge = -inf
$pinf - $huge = inf
$nan * $huge = NaN
$nan == $nan? no
$nan == $huge? no
```



## Perl 6

{{Works with|rakudo|2018.03}}
Floating point limits are to a large extent implementation dependent, but currently both Perl 6 backends (MoarVM, JVM) running on a 64 bit OS have an infinity threshold of just under 1.8e308.

```perl6
print qq:to 'END'
positive infinity: {1.8e308}
negative infinity: {-1.8e308}
negative zero: {0e0 * -1}
not a number: {0 * 1e309}
+Inf + 2.0 = {Inf + 2}
+Inf - 10.1 = {Inf - 10.1}
0 * +Inf = {0 * Inf}
+Inf + -Inf = {Inf + -Inf}
+Inf == -Inf = {+Inf == -Inf}
(-Inf+0i)**.5 = {(-Inf+0i)**.5}
NaN + 1.0 = {NaN + 1.0}
NaN + NaN = {NaN + NaN}
NaN == NaN = {NaN == NaN}
0.0 == -0.0 = {0e0 == -0e0}
END
```


<code>0e0</code> is used to have floating point number.
Simply using <code>0.0</code> makes rational number that doesn't recognize <code>-0</code>.
<code>qq:to</code> is heredoc syntax, where <code>qq</code> means
that variables and closures (between braces) are interpolated.

{{out}}

```txt
positive infinity: Inf
negative infinity: -Inf
negative zero: -0
not a number: NaN
+Inf + 2.0 = Inf
+Inf - 10.1 = Inf
0 * +Inf = NaN
+Inf + -Inf = NaN
+Inf == -Inf = False
(-Inf+0i)**.5 = Inf+Inf\i
NaN + 1.0 = NaN
NaN + NaN = NaN
NaN == NaN = False
0.0 == -0.0 = True
```



## Phix


```Phix
constant inf = 1e300*1e300, -- (works on both 32 and 64 bit)
         ninf = -inf,
         nan = -(inf/inf),
         nzero = -1/inf     -- (not supported)

printf(1," inf: %f\n",{inf})
printf(1," ninf: %f\n",{ninf})
printf(1," nan: %f\n",{nan})
printf(1,"*nzero: %f\n",{nzero})
printf(1," inf+2: %f\n",{inf+2})
printf(1," inf+ninf: %f\n",{inf+ninf})
printf(1," 0*inf: %f\n",{0*inf})
printf(1," nan+1: %f\n",{nan+1})
printf(1," nan+nan: %f\n",{nan+nan})
printf(1," inf>1e300: %d\n",{inf>1e300})
printf(1," ninf<1e300: %d\n",{ninf<-1e300})
printf(1,"*nan=nan: %d\n",{nan=nan})
printf(1," nan=42: %d\n",{nan=42})
printf(1,"*nan<0: %d\n",{nan<0})
printf(1," nan>0: %d\n",{nan>0})
```

{{out}}

```txt

 inf: inf
 ninf: -inf
 nan: nan
*nzero: 0.000000
 inf+2: inf
 inf+ninf: -nan
 0*inf: -nan
 nan+1: nan
 nan+nan: nan
 inf>1e300: 1
 ninf<1e300: 1
*nan=nan: 1
 nan=42: 0
*nan<0: 1
 nan>0: 0
```


The * lines are wrong. negative zero is not supported, and might not be practical. nan=nan should be false (0), as should nan<0. division by 0 is a fatal error.

If you fancy having a go at getting nan to work properly (x86 assembly required), see builtins\VM\pJcc.e (search for nan, 4 places, marked with --DEV this may be the wrong thing to do entirely)
and also (if you succeed) test\t41infan.exw will need a few corrections.

If you fancy having a go at negative zero support (ditto), your first stop should be :%pStoreFlt in builtins\VM\pHeap.e and use whatever the test is for -0.0 there. I would be happiest if apps that needed support of -0.0 had to explicitly call something in pHeap.e to set a flag to enable any new code.


## PicoLisp

PicoLisp has only very limited built-in floating point support,
and handles the rest by calling native (typically C) libraries.
Minus zero and negative infinity cannot be represented,
while NaN is represented by NIL

```PicoLisp
(load "@lib/math.l")

: (exp 1000.0)  # Too large for IEEE floats
-> T

: (+ 1 2 NIL 3)  # NaN propagates
-> NIL
```



## PureBasic


```PureBasic
Define.f
If OpenConsole()
  inf =  Infinity() ; or 1/None ;None represents a variable of value = 0
  minus_inf  =  -Infinity() ; or -1/None
  minus_zero = -1/inf
  nan = NaN() ; or None/None

  PrintN("positive infinity: "+StrF(inf))
  PrintN("negative infinity: "+StrF(minus_inf))
  PrintN("positive zero: "+StrF(None))
  PrintN("negative zero: "+StrF(minus_zero)) ; handles as 0.0
  PrintN("not a number: "+StrF(nan))
  PrintN("Arithmetics")
  PrintN("+inf + 2.0 =  "+StrF(inf + 2.0))
  PrintN("+inf - 10.1 = "+StrF(inf - 10.1))
  PrintN("+inf + -inf = "+StrF(inf + minus_inf))
  PrintN("0.0 * +inf =  "+StrF(0.0 * inf))
  PrintN("1.0/-0.0 =  "+StrF(1.0/minus_zero))
  PrintN("NaN + 1.0 = "+StrF(nan + 1.0))
  PrintN("NaN + NaN = "+StrF(nan + nan))
  PrintN("Logics")
  If IsInfinity(inf): PrintN("Variable 'Infinity' is infinite"): EndIf
  If IsNAN(nan): PrintN("Variable 'nan' is not a number"): EndIf

  Print(#CRLF$+"Press ENTER to EXIT"): Input()
EndIf
```


```txt
positive infinity: +Infinity
negative infinity: -Infinity
positive zero: 0.0000000000
negative zero: 0.0000000000
not a number: -1.#IND000000
Arithmetics
+inf + 2.0 =  +Infinity
+inf - 10.1 = +Infinity
+inf + -inf = -1.#IND000000
0.0 * +inf =  -1.#IND000000
1.0/-0.0 =  -Infinity
NaN + 1.0 = -1.#IND000000
NaN + NaN = -1.#IND000000
Logics
Variabel 'Infinity' is infinite
Variable 'nan' is not a number

Press ENTER to EXIT
```



## Python


```python>>>
 # Extreme values from expressions
>>> inf = 1e234 * 1e234
>>> _inf = 1e234 * -1e234
>>> _zero = 1 / _inf
>>> nan = inf + _inf
>>> inf, _inf, _zero, nan
(inf, -inf, -0.0, nan)
>>> # Print
>>> for value in (inf, _inf, _zero, nan): print (value)

inf
-inf
-0.0
nan
>>> # Extreme values from other means
>>> float('nan')
nan
>>> float('inf')
inf
>>> float('-inf')
-inf
>>> -0.
-0.0
>>> # Some arithmetic
>>> nan == nan
False
>>> nan is nan
True
>>> 0. == -0.
True
>>> 0. is -0.
False
>>> inf + _inf
nan
>>> 0.0 * nan
nan
>>> nan * 0.0
nan
>>> 0.0 * inf
nan
>>> inf * 0.0
nan
```



```python>>>
 # But note!
>>> 1 / -0.0

Traceback (most recent call last):
  File "<pyshell#106>", line 1, in <module>
    1 / -0.0
ZeroDivisionError: float division by zero
>>> # (Not minus infinity)
```



## R


```r
# 0 and -0 are recognized but are both printed as simply 0.
1/c(0, -0, Inf, -Inf, NaN)
# Inf -Inf    0    0  NaN
```



## Racket


```Racket
#lang racket
(define division-by-zero (/ 1.0 0.0))      ;+inf.0
(define negative-inf (- (/ 1.0 0.0)))      ;-inf.0
(define zero 0.0)                          ;0.0
(define negative-zero (- 0.0))             ;-0.0
(define nan (/ 0.0 0.0))                   ;+nan.0

(displayln division-by-zero)
(displayln negative-inf)
(displayln zero)
(displayln negative-zero)
(displayln nan)

(+ zero negative-zero) ;0.0
(- negative-inf division-by-zero) ; +nan.0
(+ zero nan) ; +nan.0
(= nan +nan.0) ;#f

```


This values can be assigned to a variable just as normal values


## REXX

Classic REXX has native support for extremely large decimal precision, including
extremely large (decimal) exponents.

The methods employed below can be used to display the smallest and largest positive
decimal numbers.   Both can be made negative.

Using a   '''NaN'''   will cause REXX to raise the   '''syntax'''
  error, which can be trapped and maybe handle the condition   (and
possibly recover from the "error"), but usage   (doing arithmetic operations)
  of a not─a─number is generally not an easy and transparent thing to do in REXX.

Each implementation of REXX is allowed to "define" the extreme values, the REXX language
dictates that a minimum number of decimal digits be supported as well as a minimum number
of decimal digits in the exponent.

```rexx
/*REXX pgm shows smallest & largest positive numbers that can be expressed, compares 0's*/
parse version v;    say 'version=' v;    say
   zero=  '0.0'                                          /*a (positive) value for zero. */
negZero= '-0.0'                                          /*"  negative     "   "    "   */
say 'value of zero         equals negZero: '     word('no yes',   1 + (zero  = negZero) )
say 'value of zero exactly equals negZero: '     word('no yes',   1 + (zero == negZero) )
say
     do digs=20  by 20  to 100;   numeric digits digs          /*use a range of digits. */
     say center(' number of decimal digits being used:'  digs" ", 79, '═')
     say 'tiny=' tiny()
     say 'huge=' huge()
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tiny:  return $xnum('1e-')
huge:  return $xnum('.'copies(9, digits() )"e+")
/*──────────────────────────────────────────────────────────────────────────────────────*/
$xnum: procedure;  parse arg $                           /*use the given mantissa value.*/
       !=10                                              /*use starting  exponent value.*/
                        do forever;  _=$ || !            /*construct a REXX decimal num.*/
                        if \datatype(_, 'N')  then leave /*Not numeric?   Then leave.   */
                        p=!;         !=! * 10            /*save number; magnify mantissa*/
                        end   /*forever*/
       j=! % 2                                           /*halve the exponent (power).  */
                        do forever;  _=$ || !            /* [+]  Not numeric?  Halve it.*/
                        if \datatype(_, 'N')  then do; !=p;     j=j % 2
                                                       if j==0  then leave
                                                   end
                        p=!;         !=! + j             /*save number;  bump mantissa. */
                        end   /*forever*/
       return $ || !
```

{{out|output|text=   when using Regina REXX:}}

```txt

version= REXX-Regina_3.9.1(MT) 5.00 5 Apr 2015

value of zero         equals negZero:  yes
value of zero exactly equals negZero:  no

═══════════════════ number of decimal digits being used: 20 ═══════════════════
tiny= 1e-999999999
huge= .99999999999999999999e+999999999
═══════════════════ number of decimal digits being used: 40 ═══════════════════
tiny= 1e-999999999
huge= .9999999999999999999999999999999999999999e+999999999
═══════════════════ number of decimal digits being used: 60 ═══════════════════
tiny= 1e-999999999
huge= .999999999999999999999999999999999999999999999999999999999999e+999999999
═══════════════════ number of decimal digits being used: 80 ═══════════════════
tiny= 1e-999999999
huge= .99999999999999999999999999999999999999999999999999999999999999999999999999999999e+999999999
══════════════════ number of decimal digits being used: 100 ═══════════════════
tiny= 1e-999999999
huge= .9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999e+999999999

```

{{out|output|text=   when using R4 REXX:}}

```txt

version= REXX-r4 4.00 7 Aug 2016

value of zero         equals negZero:  yes
value of zero exactly equals negZero:  no

═══════════════════ number of decimal digits being used: 20 ═══════════════════
tiny= 1e-99999999999999999999
huge= .99999999999999999999e+99999999999999999999
═══════════════════ number of decimal digits being used: 40 ═══════════════════
tiny= 1e-9999999999999999999999999999999999999999
huge= .9999999999999999999999999999999999999999e+9999999999999999999999999999999999999999
═══════════════════ number of decimal digits being used: 60 ═══════════════════
tiny= 1e-999999999999999999999999999999999999999999999999999999999999
huge= .999999999999999999999999999999999999999999999999999999999999e+999999999999999999999999999999999999999999999999999999999999
═══════════════════ number of decimal digits being used: 80 ═══════════════════
tiny= 1e-99999999999999999999999999999999999999999999999999999999999999999999999999999999
huge= .99999999999999999999999999999999999999999999999999999999999999999999999999999999e+99999999999999999999999999999999999999999999999999999999999999999999999999999999
══════════════════ number of decimal digits being used: 100 ═══════════════════
tiny= 1e-9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
huge= .9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999e+9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999

```

{{out|output|text=   when using REXX/Personal REXX:}}

```txt

version= REXX/Personal 4.00 21 Mar 1992

value of zero         equals negZero:  yes
value of zero exactly equals negZero:  no

═══════════════════ number of decimal digits being used: 20 ═══════════════════
tiny= 1e-999999999
huge= .99999999999999999999e+999999999
═══════════════════ number of decimal digits being used: 40 ═══════════════════
tiny= 1e-999999999
huge= .9999999999999999999999999999999999999999e+999999999
═══════════════════ number of decimal digits being used: 60 ═══════════════════
tiny= 1e-999999999
huge= .999999999999999999999999999999999999999999999999999999999999e+999999999
═══════════════════ number of decimal digits being used: 80 ═══════════════════
tiny= 1e-999999999
huge= .99999999999999999999999999999999999999999999999999999999999999999999999999999999e+999999999
══════════════════ number of decimal digits being used: 100 ═══════════════════
tiny= 1e-999999999
huge= .9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999e+999999999

```



## Ruby


```ruby
inf = 1.0 / 0.0    # or Float::INFINITY
nan = 0.0 / 0.0               # or Float::NAN

expression = [
  "1.0 / 0.0", "-1.0 / 0.0", "0.0 / 0.0", "- 0.0",
  "inf + 1", "5 - inf", "inf * 5", "inf / 5", "inf * 0",
  "1.0 / inf", "-1.0 / inf", "inf + inf", "inf - inf",
  "inf * inf", "inf / inf", "inf * 0.0", " 0 < inf", "inf == inf",
  "nan + 1", "nan * 5", "nan - nan", "nan * inf", "- nan",
  "nan == nan", "nan > 0", "nan < 0", "nan == 0", "nan <=> 0.0", "0.0 == -0.0",
]

expression.each do |exp|
  puts "%15s => %p" % [exp, eval(exp)]
end
```


{{out}}

```txt

      1.0 / 0.0 => Infinity
     -1.0 / 0.0 => -Infinity
      0.0 / 0.0 => NaN
          - 0.0 => -0.0
        inf + 1 => Infinity
        5 - inf => -Infinity
        inf * 5 => Infinity
        inf / 5 => Infinity
        inf * 0 => NaN
      1.0 / inf => 0.0
     -1.0 / inf => -0.0
      inf + inf => Infinity
      inf - inf => NaN
      inf * inf => Infinity
      inf / inf => NaN
      inf * 0.0 => NaN
        0 < inf => true
     inf == inf => true
        nan + 1 => NaN
        nan * 5 => NaN
      nan - nan => NaN
      nan * inf => NaN
          - nan => NaN
     nan == nan => false
        nan > 0 => false
        nan < 0 => false
       nan == 0 => false
    nan <=> 0.0 => nil
    0.0 == -0.0 => true

```



## Rust


Negative zero needs to printed using the [https://doc.rust-lang.org/std/fmt/trait.Debug.html Debug trait] (rather than the "user-facing" [https://doc.rust-lang.org/std/fmt/trait.Display.html Display trait]) because <code>0 == -0</code> to most users. See https://github.com/rust-lang/rfcs/issues/1074 and https://github.com/rust-lang/rust/issues/24623 for further discussion about this.


```rust
fn main() {
    let inf: f64 = 1. / 0.;          // or std::f64::INFINITY
    let minus_inf: f64 = -1. / 0.;   // or std::f64::NEG_INFINITY
    let minus_zero: f64 = -1. / inf; // or -0.0
    let nan: f64 = 0. / 0.;          // or std::f64::NAN
                                     // or std::f32 for the above
    println!("positive infinity: {:+}", inf);
    println!("negative infinity: {:+}", minus_inf);
    println!("negative zero: {:+?}", minus_zero);
    println!("not a number: {:+}", nan);
    println!();
    println!("+inf + 2.0 = {:+}", inf + 2.);
    println!("+inf - 10.0 = {:+}", inf - 10.);
    println!("+inf + -inf = {:+}", inf + minus_inf);
    println!("0.0 * inf = {:+}", 0. * inf);
    println!("1.0 / -0.0 = {:+}", 1. / -0.);
    println!("NaN + 1.0 = {:+}", nan + 1.);
    println!("NaN + NaN = {:+}", nan + nan);
    println!();
    println!("NaN == NaN = {}", nan == nan);
    println!("0.0 == -0.0 = {}", 0. == -0.);
}
```

{{out}}

```txt

positive infinity: +inf
negative infinity: -inf
negative zero: -0
not a number: NaN

+inf + 2.0 = +inf
+inf - 10.0 = +inf
+inf + -inf = NaN
0.0 * inf = NaN
1.0 / -0.0 = -inf
NaN + 1.0 = NaN
NaN + NaN = NaN

NaN == NaN = false
0.0 == -0.0 = true

```


=={{header|S-lang}}==
Each of these can be directly input; I'll calc the Infs for good measure:
<lang S-lang>foreach $1 ([{-0.0}, {_Inf, "1.0/0"}, {-_Inf, "-1.0/0"}, {_NaN}]) {
    () = printf("%S", $1[0]);
    if (length($1) > 1) () = printf("\t%S\n", eval($1[1]));
    else () = printf("\n");
}
```
{{out}}

```txt
-0.0
inf	inf
-inf	-inf
nan

```

<lang S-lang>% And make some comparisons:
() = printf("-0.0 and 0.0 are %sequal\n", -0.0 == 0.0 ? "" : "not ");
() = printf("-_Inf == _Inf are %sequal\n", -_Inf == _Inf ? "" : "not ");
() = printf("-0.0 and 0.0 are %sthe 'same'\n", __is_same(-0.0, 0.0) ? "" : "not ");

```
{{out}}

```txt
-0.0 and 0.0 are equal
-_Inf == _Inf are not equal
-0.0 and 0.0 are not the 'same'

```



## Scala

{{libheader|Scala}}

```Scala
object ExtremeFloatingPoint extends App {
  val negInf = -1.0 / 0.0 //also Double.NegativeInfinity
  val inf = 1.0 / 0.0 //  //also Double.PositiveInfinity
  val nan = 0.0 / 0.0 //  //also Double.NaN
  val negZero = -2.0 / inf

  println("Value:         Result:      Infinity? Whole?")
  println(f"Negative inf: ${negInf}%9s ${negInf.isInfinity}%9s ${negInf.isWhole}%9s")
  println(f"Positive inf: ${inf}%9s ${inf.isInfinity}%9s ${inf.isWhole}%9s")
  println(f"NaN:          ${nan}%9s ${nan.isInfinity}%9s ${nan.isWhole}%9s")
  println(f"Negative 0:   ${negZero}%9s ${negZero.isInfinity}%9s ${negZero.isWhole}%9s")
  println(f"inf + -inf:   ${inf + negInf}%9s ${(inf + negInf).isInfinity}%9s ${(inf + negInf).isWhole}%9s")
  println(f"0 * NaN:      ${0 * nan}%9s ${(inf + negInf).isInfinity}%9s ${(inf + negInf).isWhole}%9s")
  println(f"NaN == NaN:   ${nan == nan}%9s")
}
```

{{out}}

```txt
Value:         Result:      Infinity? Whole?
Negative inf: -Infinity      true     false
Positive inf:  Infinity      true     false
NaN:                NaN     false     false
Negative 0:        -0.0     false      true
inf + -inf:         NaN     false     false
0 * NaN:            NaN     false     false
NaN == NaN:       false

```



## Scheme


```Scheme
(define infinity (/ 1.0 0.0))
(define minus-infinity (- infinity))
(define zero 0.0)
(define minus-zero (- zero))
(define not-a-number (/ 0.0 0.0))

(equal? (list infinity minus-infinity zero minus-zero not-a-number)
        (list   +inf.0         -inf.0  0.0       -0.0       +nan.0))
; #t

```



## Seed7

The type [http://seed7.sourceforge.net/libraries/float.htm float] works according to IEEE 754.
Constants like [http://seed7.sourceforge.net/libraries/float.htm#Infinity Infinity] and
[http://seed7.sourceforge.net/libraries/float.htm#NaN NaN] are predefined in the library
[http://seed7.sourceforge.net/libraries/float.htm float.s7i].
A zero is always written without sign (e.g.: ''write(-0.0)'' writes ''0.0'',
and ''write(-0.004 [http://seed7.sourceforge.net/libraries/float.htm#%28ref_float%29digits%28ref_integer%29 digits] 2);'' writes ''0.00'').
To recognize negative zero the function [http://seed7.sourceforge.net/libraries/float.htm#isNegativeZero%28in_float%29 isNegativeZero]
can be used. NaN can be checked with [http://seed7.sourceforge.net/libraries/float.htm#isNaN%28ref_float%29 isNaN].


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const proc: main is func
  begin
    writeln("positive infinity: " <& Infinity);
    writeln("negative infinity: " <& -Infinity);
    writeln("negative zero: " <& -0.0);
    writeln("not a number: " <& NaN);

    # some arithmetic
    writeln("+Infinity + 2.0 = " <& Infinity + 2.0);
    writeln("+Infinity - 10.1 = " <& Infinity - 10.1);
    writeln("+Infinity + -Infinity = " <& Infinity + -Infinity);
    writeln("0.0 * +Infinity = " <& 0.0 * Infinity);
    writeln("1.0/-0.0 = " <& 1.0 / -0.0);
    writeln("NaN + 1.0 = " <& NaN + 1.0);
    writeln("NaN + NaN = " <& NaN + NaN);

    # some comparisons
    writeln("NaN = NaN = " <& NaN = NaN);
    writeln("isNaN(NaN) = " <& isNaN(NaN));
    writeln("0.0 = -0.0 = " <& 0.0 = -0.0);
    writeln("isNegativeZero(-0.0) = " <& isNegativeZero(-0.0));
    writeln("isNegativeZero(0.0) = " <& isNegativeZero(0.0));
  end func;
```


{{out}}

```txt

positive infinity: Infinity
negative infinity: -Infinity
negative zero: 0.0
not a number: NaN
+Infinity + 2.0 = Infinity
+Infinity - 10.1 = Infinity
+Infinity + -Infinity = NaN
0.0 * +Infinity = NaN
1.0/-0.0 = -Infinity
NaN + 1.0 = NaN
NaN + NaN = NaN
NaN = NaN = FALSE
isNaN(NaN) = TRUE
0.0 = -0.0 = TRUE
isNegativeZero(-0.0) = TRUE
isNegativeZero(0.0) = FALSE

```



## Sidef

{{trans|Ruby}}
''NaN'' and ''Inf'' literals can be used to represent the ''Not-a-Number'' and ''Infinity'' values, which are returned in special cases, such as ''0/0'' and ''1/0''. However, one thing to notice, is that in Sidef there is no distinction between ''0.0'' and ''-0.0'' and can't be differentiated from each other.

```ruby
var inf = 1/0    # same as: Inf
var nan = 0/0    # same as: NaN

var exprs = [
  "1.0 / 0.0", "-1.0 / 0.0", "0.0 / 0.0", "- 0.0",
  "inf + 1", "5 - inf", "inf * 5", "inf / 5", "inf * 0",
  "1.0 / inf", "-1.0 / inf", "inf + inf", "inf - inf",
  "inf * inf", "inf / inf", "inf * 0.0", " 0 < inf", "inf == inf",
  "nan + 1", "nan * 5", "nan - nan", "nan * inf", "- nan",
  "nan == nan", "nan > 0", "nan < 0", "nan == 0", "0.0 == -0.0",
]

exprs.each { |expr|
  "%15s => %s\n".printf(expr, eval(expr))
}

say "-"*40
say("NaN equality: ",        NaN ==  nan)
say("Infinity equality: ",   Inf ==  inf)
say("-Infinity equality: ", -Inf == -inf)

say "-"*40
say("sqrt(-1)   = ",   sqrt(-1))
say("tanh(-Inf) = ", tanh(-inf))
say("(-Inf)**2  = ",  (-inf)**2)
say("(-Inf)**3  = ",  (-inf)**3)
say("acos(Inf)  = ",  acos(inf))
say("atan(Inf)  = ",  atan(inf))
say("log(-1)    = ",    log(-1))
say("atanh(Inf) = ", atanh(inf))
```

{{out}}

```txt

      1.0 / 0.0 => Inf
     -1.0 / 0.0 => -Inf
      0.0 / 0.0 => NaN
          - 0.0 => 0
        inf + 1 => Inf
        5 - inf => -Inf
        inf * 5 => Inf
        inf / 5 => Inf
        inf * 0 => NaN
      1.0 / inf => 0
     -1.0 / inf => 0
      inf + inf => Inf
      inf - inf => NaN
      inf * inf => Inf
      inf / inf => NaN
      inf * 0.0 => NaN
        0 < inf => true
     inf == inf => true
        nan + 1 => NaN
        nan * 5 => NaN
      nan - nan => NaN
      nan * inf => NaN
          - nan => NaN
     nan == nan => false
        nan > 0 =>
        nan < 0 =>
       nan == 0 => false
    0.0 == -0.0 => true
----------------------------------------
NaN equality: false
Infinity equality: true
-Infinity equality: true
----------------------------------------
sqrt(-1)   = i
tanh(-Inf) = -1
(-Inf)**2  = Inf
(-Inf)**3  = -Inf
acos(Inf)  = -Infi
atan(Inf)  = 1.57079632679489661923132169163975144209858469969
log(-1)    = 3.14159265358979323846264338327950288419716939938i
atanh(Inf) = 1.57079632679489661923132169163975144209858469969i

```



## Stata


Stata does not use NaN values, but instead it has several kinds of missing values, which are denoted by . and .a to .z. These are stored as large floating point numbers, as can be seen in the hexadecimal representation:


```stata
. display %21x .
+1.0000000000000X+3ff

. display %21x .a
+1.0010000000000X+3ff

. display %21x .z
+1.01a0000000000X+3ff

. display %21x c(maxdouble)
+1.fffffffffffffX+3fe
```


Notice that .z > ... > .a > . and . is greater than any real number, and c(maxdouble) is the value 8.9884656743115785e+307.

The hexadecimal representation of floating-point numbers is discussed in two articles by William Gould on Stata blog: [http://blog.stata.com/2011/02/02/how-to-read-the-percent-21x-format/ part 1], [http://blog.stata.com/2011/02/10/how-to-read-the-percent-21x-format-part-2/ part 2].


## Swift


```swift
let negInf = -1.0 / 0.0
let inf = 1.0 / 0.0 //also Double.infinity
let nan = 0.0 / 0.0 //also Double.NaN
let negZero = -2.0 / inf

println("Negative inf: \(negInf)")
println("Positive inf: \(inf)")
println("NaN: \(nan)")
println("Negative 0: \(negZero)")
println("inf + -inf: \(inf + negInf)")
println("0 * NaN: \(0 * nan)")
println("NaN == NaN: \(nan == nan)")
```

{{out}}

```txt

Negative inf: -inf
Positive inf: inf
NaN: nan
Negative 0: -0.0
inf + -inf: nan
0 * NaN: nan
NaN == NaN: false

```



## Tcl

Tcl includes support in expressions for all IEEE “extreme” values except for NaN,
which it throws a catchable exception on encountering numerically.
Moreover, all can be just written directly as literals
(they are parsed case-insensitively).
For example, see this log of an interactive session:

```tcl
% package require Tcl 8.5
8.5.2
% expr inf+1
Inf
% set inf_val [expr {1.0 / 0.0}]
Inf
% set neginf_val [expr {-1.0 / 0.0}]
-Inf
% set negzero_val [expr {1.0 / $neginf_val}]
-0.0
% expr {0.0 / 0.0}
domain error: argument not in valid range
% expr nan
domain error: argument not in valid range
% expr {1/-inf}
-0.0
```

It ''is'' possible to introduce a real NaN though numeric computation,
but only by using the mechanisms for dealing with external binary data
(it being judged better to just deal with it in that case
rather than throwing an exception):

```tcl
% binary scan [binary format q nan] q nan
1
% puts $nan
NaN
% # Show that it is a real NaN in there
% expr {$nan+0}
can't use non-numeric floating-point value as operand of "+"
```

