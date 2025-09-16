+++
title = "Roots of a quadratic function"
description = ""
date = 2018-11-09T16:53:13Z
aliases = []
[extra]
id = 3149
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erre",
  "factor",
  "forth",
  "fortran",
  "gap",
  "go",
  "haskell",
  "idl",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lambdatalk",
  "liberty_basic",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "maxima",
  "ocaml",
  "octave",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "stata",
  "tcl",
  "zkl",
]
+++

## Task

{{task|Arithmetic operations}}{{Clarified-review}}Write a program to find the roots of a quadratic equation, i.e., solve the equation <math>ax^2 + bx + c = 0</math>.
Your program must correctly handle non-real roots, but it need not check that <math>a \neq 0</math>.

The problem of solving a quadratic equation is a good example of how dangerous it can be to ignore the peculiarities of floating-point arithmetic.
The obvious way to implement the quadratic formula suffers catastrophic loss of accuracy when one of the roots to be found is much closer to 0 than the other.
In their classic textbook on numeric methods ''[http://www.pdas.com/fmm.htm Computer Methods for Mathematical Computations]'', George Forsythe, Michael Malcolm, and Cleve Moler suggest trying the naive algorithm with <math>a = 1</math>, <math>b = -10^5</math>, and <math>c = 1</math>.
(For double-precision floats, set <math>b = -10^9</math>.)
Consider the following implementation in [[Ada]]:

```ada
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;

procedure Quadratic_Equation is
   type Roots is array (1..2) of Float;
   function Solve (A, B, C : Float) return Roots is
      SD : constant Float := sqrt (B**2 - 4.0 * A * C);
      AA : constant Float := 2.0 * A;
   begin
      return ((- B + SD) / AA, (- B - SD) / AA);
   end Solve;

   R : constant Roots := Solve (1.0, -10.0E5, 1.0);
begin
   Put_Line ("X1 =" & Float'Image (R (1)) & " X2 =" & Float'Image (R (2)));
end Quadratic_Equation;
```

```txt
X1 = 1.00000E+06 X2 = 0.00000E+00
```

As we can see, the second root has lost all significant figures. The right answer is that <code>X2</code> is about <math>10^{-6}</math>. The naive method is numerically unstable.

Suggested by Middlebrook (D-OA), a better numerical method: to define two parameters <math> q = \sqrt{a c} / b </math> and <math> f = 1/2 + \sqrt{1 - 4 q^2} /2 </math>

and the two roots of the quardratic are: <math> \frac{-b}{a} f </math> and <math> \frac{-c}{b f} </math>


'''Task''': do it better. This means that given <math>a = 1</math>, <math>b = -10^9</math>, and <math>c = 1</math>, both of the roots your program returns should be greater than <math>10^{-11}</math>. Or, if your language can't do floating-point arithmetic any more precisely than single precision, your program should be able to handle <math>b = -10^6</math>. Either way, show what your program gives as the roots of the quadratic in question. See page 9 of
[https://web.archive.org/web/20080921074325/http://dlc.sun.com/pdf//800-7895/800-7895.pdf "What Every Scientist Should Know About Floating-Point Arithmetic"] for a possible algorithm.


## Ada


```ada
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;

procedure Quadratic_Equation is
   type Roots is array (1..2) of Float;
   function Solve (A, B, C : Float) return Roots is
      SD : constant Float := sqrt (B**2 - 4.0 * A * C);
      X  : Float;
   begin
      if B < 0.0 then
         X := (- B + SD) / 2.0 * A;
         return (X, C / (A * X));
      else
         X := (- B - SD) / 2.0 * A;
         return (C / (A * X), X);
      end if;
   end Solve;

   R : constant Roots := Solve (1.0, -10.0E5, 1.0);
begin
   Put_Line ("X1 =" & Float'Image (R (1)) & " X2 =" & Float'Image (R (2)));
end Quadratic_Equation;
```

Here precision loss is prevented by checking signs of operands. On errors, Constraint_Error is propagated on numeric errors and when roots are complex.
```txt

X1 = 1.00000E+06 X2 = 1.00000E-06

```



## ALGOL 68

```algol68
quadratic equation:
BEGIN

  MODE ROOTS  = UNION([]REAL, []COMPL);
  MODE QUADRATIC = STRUCT(REAL a,b,c);

  PROC solve  = (QUADRATIC q)ROOTS:
  BEGIN
    REAL a = a OF q, b = b OF q, c = c OF q;
    REAL sa = b**2 - 4*a*c;
    IF sa >=0 THEN # handle the +ve case as REAL #
      REAL sqrt sa = ( b<0 | sqrt(sa) | -sqrt(sa));
      REAL r1 = (-b + sqrt sa)/(2*a),
           r2 = (-b - sqrt sa)/(2*a);
      []REAL((r1,r2))
    ELSE # handle the -ve case as COMPL conjugate pairs #
      COMPL compl sqrt sa = ( b<0 | complex sqrt(sa) | -complex sqrt(sa));
      COMPL r1 = (-b + compl sqrt sa)/(2*a),
            r2 = (-b - compl sqrt sa)/(2*a);
      []COMPL (r1, r2)
    FI
  END # solve #;

  PROC real  evaluate = (QUADRATIC q, REAL x )REAL:  (a OF q*x + b OF q)*x + c OF q;
  PROC compl evaluate = (QUADRATIC q, COMPL x)COMPL: (a OF q*x + b OF q)*x + c OF q;

  # only a very tiny difference between the 2 examples #
  []QUADRATIC test = ((1, -10e5, 1), (1, 0, 1), (1,-3,2), (1,3,2), (4,0,4), (3,4,5));

  FORMAT real fmt = $g(-0,8)$;
  FORMAT compl fmt = $f(real fmt)"+"f(real fmt)"i"$;
  FORMAT quadratic fmt = $f(real fmt)" x**2 + "f(real fmt)" x + "f(real fmt)" = 0"$;

  FOR index TO UPB test DO
    QUADRATIC quadratic = test[index];
    ROOTS r = solve(quadratic);

# Output the two different scenerios #
    printf(($"Quadratic: "$, quadratic fmt, quadratic, $l$));
    CASE r IN
      ([]REAL r):
        printf(($"REAL x1 = "$, real fmt, r[1],
                   $", x2 = "$, real fmt, r[2],  $"; "$,
                $"REAL y1 = "$, real fmt, real evaluate(quadratic,r[1]),
                   $", y2 = "$, real fmt, real evaluate(quadratic,r[2]), $";"ll$
        )),
      ([]COMPL c):
        printf(($"COMPL x1,x2 = "$, real fmt, re OF c[1], $"+/-"$,
                                    real fmt, ABS im OF c[1], $"; "$,
                  $"COMPL y1 = "$, compl fmt, compl evaluate(quadratic,c[1]),
                      $", y2 = "$, compl fmt, compl evaluate(quadratic,c[2]), $";"ll$
        ))
    ESAC
  OD
END # quadratic_equation #
```

```txt

Quadratic: 1.00000000 x**2 + -1000000.00000000 x + 1.00000000 = 0
REAL x1 = 999999.99999900, x2 = .00000100; REAL y1 = -.00000761, y2 = -.00000761;

Quadratic: 1.00000000 x**2 + .00000000 x + 1.00000000 = 0
COMPL x1,x2 = .00000000+/-1.00000000; COMPL y1 = .00000000+.00000000i, y2 = .00000000+.00000000i;

Quadratic: 1.00000000 x**2 + -3.00000000 x + 2.00000000 = 0
REAL x1 = 2.00000000, x2 = 1.00000000; REAL y1 = .00000000, y2 = .00000000;

Quadratic: 1.00000000 x**2 + 3.00000000 x + 2.00000000 = 0
REAL x1 = -2.00000000, x2 = -1.00000000; REAL y1 = .00000000, y2 = .00000000;

Quadratic: 4.00000000 x**2 + .00000000 x + 4.00000000 = 0
COMPL x1,x2 = .00000000+/-1.00000000; COMPL y1 = .00000000+.00000000i, y2 = .00000000+.00000000i;

Quadratic: 3.00000000 x**2 + 4.00000000 x + 5.00000000 = 0
COMPL x1,x2 = -.66666667+/-1.10554160; COMPL y1 = .00000000+.00000000i, y2 = .00000000+-.00000000i;

```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/viewtopic.php?p=276617#276617 discussion]

```AutoHotkey
MsgBox % quadratic(u,v, 1,-3,2) ", " u ", " v
MsgBox % quadratic(u,v, 1,3,2) ", " u ", " v
MsgBox % quadratic(u,v, -2,4,-2) ", " u ", " v
MsgBox % quadratic(u,v, 1,0,1) ", " u ", " v
SetFormat FloatFast, 0.15e
MsgBox % quadratic(u,v, 1,-1.0e8,1) ", " u ", " v

quadratic(ByRef x1, ByRef x2, a,b,c) { ; -> #real roots {x1,x2} of ax²+bx+c
   If (a = 0)
      Return -1 ; ERROR: not quadratic
   d := b*b - 4*a*c
   If (d < 0) {
      x1 := x2 := ""
      Return 0
   }
   If (d = 0) {
      x1 := x2 := -b/2/a
      Return 1
   }
   x1 := (-b - (b<0 ? -sqrt(d) : sqrt(d)))/2/a
   x2 := c/a/x1
   Return 2
}
```



## BBC BASIC


```bbcbasic
      FOR test% = 1 TO 7
        READ a$, b$, c$
        PRINT "For a = " ; a$ ", b = " ; b$ ", c = " ; c$ TAB(32) ;
        PROCsolvequadratic(EVAL(a$), EVAL(b$), EVAL(c$))
      NEXT
      END

      DATA 1, -1E9, 1
      DATA 1, 0, 1
      DATA 2, -1, -6
      DATA 1, 2, -2
      DATA 0.5, SQR(2), 1
      DATA 1, 3, 2
      DATA 3, 4, 5

      DEF PROCsolvequadratic(a, b, c)
      LOCAL d, f
      d = b^2 - 4*a*c
      CASE SGN(d) OF
        WHEN 0:
          PRINT "the single root is " ; -b/2/a
        WHEN +1:
          f = (1 + SQR(1-4*a*c/b^2))/2
          PRINT "the real roots are " ; -f*b/a " and " ; -c/b/f
        WHEN -1:
          PRINT "the complex roots are " ; -b/2/a " +/- " ; SQR(-d)/2/a "*i"
      ENDCASE
      ENDPROC
```

```txt
For a = 1, b = -1E9, c = 1      the real roots are 1E9 and 1E-9
For a = 1, b = 0, c = 1         the complex roots are 0 +/- 1*i
For a = 2, b = -1, c = -6       the real roots are 2 and -1.5
For a = 1, b = 2, c = -2        the real roots are -2.73205081 and 0.732050808
For a = 0.5, b = SQR(2), c = 1  the single root is -1.41421356
For a = 1, b = 3, c = 2         the real roots are -2 and -1
For a = 3, b = 4, c = 5         the complex roots are -0.666666667 +/- 1.1055416*i
```



## C

Code that tries to avoid floating point overflow and other unfortunate loss of precissions: (compiled with <code>gcc -std=c99</code> for <code>complex</code>, though easily adapted to just real numbers)

```c
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <math.h>

typedef double complex cplx;

void quad_root
(double a, double b, double c, cplx * ra, cplx *rb)
{
	double d, e;
	if (!a) {
		*ra = b ? -c / b : 0;
		*rb = 0;
		return;
	}
	if (!c) {
		*ra = 0;
		*rb = -b / a;
		return;
	}

	b /= 2;
	if (fabs(b) > fabs(c)) {
		e = 1 - (a / b) * (c / b);
		d = sqrt(fabs(e)) * fabs(b);
	} else {
		e = (c > 0) ? a : -a;
		e = b * (b / fabs(c)) - e;
		d = sqrt(fabs(e)) * sqrt(fabs(c));
	}

	if (e < 0) {
		e = fabs(d / a);
		d = -b / a;
		*ra = d + I * e;
		*rb = d - I * e;
		return;
	}

	d = (b >= 0) ? d : -d;
	e = (d - b) / a;
	d = e ? (c / e) / a : 0;
	*ra = d;
	*rb = e;
	return;
}

int main()
{
	cplx ra, rb;
	quad_root(1, 1e12 + 1, 1e12, &ra, &rb);
	printf("(%g + %g i), (%g + %g i)\n",
		creal(ra), cimag(ra), creal(rb), cimag(rb));

	quad_root(1e300, -1e307 + 1, 1e300, &ra, &rb);
	printf("(%g + %g i), (%g + %g i)\n",
		creal(ra), cimag(ra), creal(rb), cimag(rb));

	return 0;
}
```

```txt
(-1e+12 + 0 i), (-1 + 0 i)
(1.00208e+07 + 0 i), (9.9792e-08 + 0 i)
```



```c
#include <stdio.h>
#include <math.h>
#include <complex.h>

void roots_quadratic_eq(double a, double b, double c, complex double *x)
{
  double delta;

  delta = b*b - 4.0*a*c;
  x[0] = (-b + csqrt(delta)) / (2.0*a);
  x[1] = (-b - csqrt(delta)) / (2.0*a);
}
```

```c
void roots_quadratic_eq2(double a, double b, double c, complex double *x)
{
  b /= a;
  c /= a;
  double delta = b*b - 4*c;
  if ( delta < 0 ) {
    x[0] = -b/2 + I*sqrt(-delta)/2.0;
    x[1] = -b/2 - I*sqrt(-delta)/2.0;
  } else {
    double root = sqrt(delta);
    double sol = (b>0) ? (-b - root)/2.0 : (-b + root)/2.0;
    x[0] = sol;
    x[1] = c/sol;
  }
}
```



```c
int main()
{
  complex double x[2];

  roots_quadratic_eq(1, -1e20, 1, x);
  printf("x1 = (%.20le, %.20le)\nx2 = (%.20le, %.20le)\n\n",
	 creal(x[0]), cimag(x[0]),
	 creal(x[1]), cimag(x[1]));
  roots_quadratic_eq2(1, -1e20, 1, x);
  printf("x1 = (%.20le, %.20le)\nx2 = (%.20le, %.20le)\n\n",
	 creal(x[0]), cimag(x[0]),
	 creal(x[1]), cimag(x[1]));

  return 0;
}
```



```txt
x1 = (1.00000000000000000000e+20, 0.00000000000000000000e+00)
x2 = (0.00000000000000000000e+00, 0.00000000000000000000e+00)

x1 = (1.00000000000000000000e+20, 0.00000000000000000000e+00)
x2 = (9.99999999999999945153e-21, 0.00000000000000000000e+00)
```


## C#

```c#
using System;
using System.Numerics;

class QuadraticRoots
{
    static Tuple<Complex, Complex> Solve(double a, double b, double c)
    {
        var q = -(b + Math.Sign(b) * Complex.Sqrt(b * b - 4 * a * c)) / 2;
        return Tuple.Create(q / a, c / q);
    }

    static void Main()
    {
        Console.WriteLine(Solve(1, -1E20, 1));
    }
}
```

```txt
((1E+20, 0), (1E-20, 0))
```



## C++


```cpp
#include <iostream>
#include <utility>
#include <complex>

typedef std::complex<double> complex;

std::pair<complex, complex>
 solve_quadratic_equation(double a, double b, double c)
{
  b /= a;
  c /= a;
  double discriminant = b*b-4*c;
  if (discriminant < 0)
    return std::make_pair(complex(-b/2, std::sqrt(-discriminant)/2),
                          complex(-b/2, -std::sqrt(-discriminant)/2));

  double root = std::sqrt(discriminant);
  double solution1 = (b > 0)? (-b - root)/2
                            : (-b + root)/2;

  return std::make_pair(solution1, c/solution1);
}

int main()
{
  std::pair<complex, complex> result = solve_quadratic_equation(1, -1e20, 1);
  std::cout << result.first << ", " << result.second << std::endl;
}
```

 (1e+20,0), (1e-20,0)


## Clojure



```clojure
(defn quadratic
  "Compute the roots of a quadratic in the form ax^2 + bx + c = 1.
   Returns any of nil, a float, or a vector."
  [a b c]
  (let [sq-d (Math/sqrt (- (* b b) (* 4 a c)))
        f    #(/ (% b sq-d) (* 2 a))]
    (cond
       (neg? sq-d)  nil
       (zero? sq-d) (f +)
       (pos? sq-d)  [(f +) (f -)]
       :else nil))) ; maybe our number ended up as NaN
```


```clojure
user=>
 (quadratic 1.0 1.0 1.0)
nil
user=> (quadratic 1.0 2.0 1.0)
1.0
user=> (quadratic 1.0 3.0 1.0)
[2.618033988749895 0.3819660112501051]

```



## Common Lisp


```lisp
(defun quadratic (a b c)
  (list
   (/ (+ (- b) (sqrt (- (expt b 2) (* 4 a c)))) (* 2 a))
   (/ (- (- b) (sqrt (- (expt b 2) (* 4 a c)))) (* 2 a))))
```



## D


```d
import std.math, std.traits;

CommonType!(T1, T2, T3)[] naiveQR(T1, T2, T3)
                                 (in T1 a, in T2 b, in T3 c)
pure nothrow if (isFloatingPoint!T1) {
    alias ReturnT = typeof(typeof(return).init[0]);
    if (a == 0)
        return [ReturnT(c / b)]; // It's a linear function.
    immutable ReturnT det = b ^^ 2 - 4 * a * c;
    if (det < 0)
        return []; // No real number root.
    immutable SD = sqrt(det);
    return [(-b + SD) / 2 * a, (-b - SD) / 2 * a];
}

CommonType!(T1, T2, T3)[] cautiQR(T1, T2, T3)
                                 (in T1 a, in T2 b, in T3 c)
pure nothrow if (isFloatingPoint!T1) {
    alias ReturnT = typeof(typeof(return).init[0]);
    if (a == 0)
        return [ReturnT(c / b)]; // It's a linear function.
    immutable ReturnT det = b ^^ 2 - 4 * a * c;
    if (det < 0)
        return []; // No real number root.
    immutable SD = sqrt(det);

    if (b * a < 0) {
        immutable x = (-b + SD) / 2 * a;
        return [x, c / (a * x)];
    } else {
        immutable x = (-b - SD) / 2 * a;
        return [c / (a * x), x];
    }
}

void main() {
    import std.stdio;
    writeln("With 32 bit float type:");
    writefln("   Naive: [%(%g, %)]", naiveQR(1.0f, -10e5f, 1.0f));
    writefln("Cautious: [%(%g, %)]", cautiQR(1.0f, -10e5f, 1.0f));
    writeln("\nWith 64 bit double type:");
    writefln("   Naive: [%(%g, %)]", naiveQR(1.0, -10e5, 1.0));
    writefln("Cautious: [%(%g, %)]", cautiQR(1.0, -10e5, 1.0));
    writeln("\nWith real type:");
    writefln("   Naive: [%(%g, %)]", naiveQR(1.0L, -10e5L, 1.0L));
    writefln("Cautious: [%(%g, %)]", cautiQR(1.0L, -10e5L, 1.0L));
}
```

```txt
With 32 bit float type:
   Naive: [1e+06, 0]
Cautious: [1e+06, 1e-06]

With 64 bit double type:
   Naive: [1e+06, 1.00001e-06]
Cautious: [1e+06, 1e-06]

With real type:
   Naive: [1e+06, 1e-06]
Cautious: [1e+06, 1e-06]
```



## Elixir


```elixir
defmodule Quadratic do
  def roots(a, b, c) do
    IO.puts "Roots of a quadratic function (#{a}, #{b}, #{c})"
    d = b * b - 4 * a * c
    a2 = a * 2
    cond do
      d > 0 ->
        sd = :math.sqrt(d)
        IO.puts "  the real roots are #{(- b + sd) / a2} and #{(- b - sd) / a2}"
      d == 0 ->
        IO.puts "  the single root is #{- b / a2}"
      true ->
        sd = :math.sqrt(-d)
        IO.puts "  the complex roots are #{- b / a2} +/- #{sd / a2}*i"
    end
  end
end

Quadratic.roots(1, -2, 1)
Quadratic.roots(1, -3, 2)
Quadratic.roots(1, 0, 1)
Quadratic.roots(1, -1.0e10, 1)
Quadratic.roots(1, 2, 3)
Quadratic.roots(2, -1, -6)
```


```txt

Roots of a quadratic function (1, -2, 1)
  the single root is 1.0
Roots of a quadratic function (1, -3, 2)
  the real roots are 2.0 and 1.0
Roots of a quadratic function (1, 0, 1)
  the complex roots are 0.0 +/- 1.0*i
Roots of a quadratic function (1, -1.0e10, 1)
  the real roots are 1.0e10 and 0.0
Roots of a quadratic function (1, 2, 3)
  the complex roots are -1.0 +/- 1.4142135623730951*i
Roots of a quadratic function (2, -1, -6)
  the real roots are 2.0 and -1.5

```



## ERRE

<lang>PROGRAM QUADRATIC

PROCEDURE SOLVE_QUADRATIC
  D=B*B-4*A*C
  IF ABS(D)<1D-6 THEN D=0 END IF
  CASE SGN(D) OF
    0->
       PRINT("the single root is ";-B/2/A)
    END ->
    1->
       F=(1+SQR(1-4*A*C/(B*B)))/2
       PRINT("the real roots are ";-F*B/A;"and ";-C/B/F)
    END ->
    -1->
       PRINT("the complex roots are ";-B/2/A;"+/-";SQR(-D)/2/A;"*i")
    END ->
  END CASE
END PROCEDURE

BEGIN
  PRINT(CHR$(12);) ! CLS
  FOR TEST%=1 TO 7 DO
     READ(A,B,C)
     PRINT("For a=";A;",b=";B;",c=";C;TAB(32);)
     SOLVE_QUADRATIC
  END FOR
  DATA(1,-1E9,1)
  DATA(1,0,1)
  DATA(2,-1,-6)
  DATA(1,2,-2)
  DATA(0.5,1.4142135,1)
  DATA(1,3,2)
  DATA(3,4,5)
END PROGRAM
```

```txt
For a= 1 ,b=-1E+09 ,c= 1       the real roots are  1E+09 and  1E-09
For a= 1 ,b= 0 ,c= 1           the complex roots are  0 +/- 1 *i
For a= 2 ,b=-1 ,c=-6           the real roots are  2 and -1.5
For a= 1 ,b= 2 ,c=-2           the real roots are -2.732051 and  .7320508
For a= .5 ,b= 1.414214 ,c= 1   the single root is -1.414214
For a= 1 ,b= 3 ,c= 2           the real roots are -2 and -1
For a= 3 ,b= 4 ,c= 5           the complex roots are -.6666667 +/- 1.105542 *i

```



## Factor

```factor
:: quadratic-equation ( a b c -- x1 x2 )
    b sq a c * 4 * - sqrt :> sd
    b 0 <
    [ b neg sd + a 2 * / ]
    [ b neg sd - a 2 * / ] if :> x
    x c a x * / ;
```



```factor
( scratchpad ) 1 -1.e20 1 quadratic-equation
--- Data stack:
1.0e+20
9.999999999999999e-21
```


Middlebrook method

```factor
:: quadratic-equation2 ( a b c -- x1 x2 )
 a c * sqrt b / :> q
 1 4 q sq * - sqrt 0.5 * 0.5 + :> f
 b neg a / f * c neg b / f / ;

```




```factor
( scratchpad ) 1 -1.e20 1 quadratic-equation
--- Data stack:
1.0e+20
1.0e-20
```



## Forth

Without locals:

```forth
: quadratic ( fa fb fc -- r1 r2 )
  frot frot
  ( c a b )
  fover 3 fpick f* -4e f*  fover fdup f* f+
  ( c a b det )
  fdup f0< if abort" imaginary roots" then
  fsqrt
  fover f0< if fnegate then
  f+ fnegate
  ( c a b-det )
  2e f/ fover f/
  ( c a r1 )
  frot frot f/ fover f/ ;
```

With locals:

```forth
: quadratic { F: a F: b F: c -- r1 r2 }
  b b f*  4e a f* c f* f-
  fdup f0< if abort" imaginary roots" then
  fsqrt
  b f0< if fnegate then b f+ fnegate 2e f/ a f/
  c a f/ fover f/ ;

\ test
1 set-precision
1e -1e6 1e quadratic fs. fs.     \ 1e-6 1e6
```



## Fortran


### Fortran 90

```fortran
PROGRAM QUADRATIC

 IMPLICIT NONE
 INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15)
 REAL(dp) :: a, b, c, e, discriminant, rroot1, rroot2
 COMPLEX(dp) :: croot1, croot2

 WRITE(*,*) "Enter the coefficients of the equation ax^2 + bx + c"
 WRITE(*, "(A)", ADVANCE="NO") "a = "
 READ *, a
 WRITE(*,"(A)", ADVANCE="NO") "b = "
 READ *, b
 WRITE(*,"(A)", ADVANCE="NO") "c = "
 READ *, c

 WRITE(*,"(3(A,E23.15))") "Coefficients are: a = ", a, "   b = ", b, "   c = ", c
 e = 1.0e-9_dp
 discriminant = b*b - 4.0_dp*a*c

 IF (ABS(discriminant) < e) THEN
    rroot1 = -b / (2.0_dp * a)
    WRITE(*,*) "The roots are real and equal:"
    WRITE(*,"(A,E23.15)") "Root = ", rroot1
 ELSE IF (discriminant > 0) THEN
    rroot1 = -(b + SIGN(SQRT(discriminant), b)) / (2.0_dp * a)
    rroot2 = c / (a * rroot1)
    WRITE(*,*) "The roots are real:"
    WRITE(*,"(2(A,E23.15))") "Root1 = ", rroot1, "  Root2 = ", rroot2
 ELSE
    croot1 = (-b + SQRT(CMPLX(discriminant))) / (2.0_dp*a)
    croot2 = CONJG(croot1)
    WRITE(*,*) "The roots are complex:"
    WRITE(*,"(2(A,2E23.15,A))") "Root1 = ", croot1, "j ", "  Root2 = ", croot2, "j"
 END IF
```

 Coefficients are: a =   0.300000000000000E+01   b =   0.400000000000000E+01   c =   0.133333333330000E+01
 The roots are real and equal:
 Root =  -0.666666666666667E+00

 Coefficients are: a =   0.300000000000000E+01   b =   0.200000000000000E+01   c =  -0.100000000000000E+01
 The roots are real:
 Root1 =  -0.100000000000000E+01  Root2 =   0.333333333333333E+00

 Coefficients are: a =   0.300000000000000E+01   b =   0.200000000000000E+01   c =   0.100000000000000E+01
 The roots are complex:
 Root1 =  -0.333333333333333E+00  0.471404512723287E+00j   Root2 =  -0.333333333333333E+00 -0.471404512723287E+00j

 Coefficients are: a =   0.100000000000000E+01   b =  -0.100000000000000E+07   c =   0.100000000000000E+01
 The roots are real:
 Root1 =   0.999999999999000E+06  Root2 =   0.100000000000100E-05


### Fortran I

Source code written in FORTRAN I (october 1956) for the IBM 704.

```fortran

COMPUTE ROOTS OF A QUADRATIC FUNCTION - 1956
      READ 100,A,B,C
 100  FORMAT(3F8.3)
      PRINT 100,A,B,C
      DISC=B**2-4.*A*C
      IF(DISC),1,2,3
 1    XR=-B/(2.*A)
      XI=SQRT(-DISC)/(2.*A)
      XJ=-XI
      PRINT 311
      PRINT 312,XR,XI,XR,XJ
 311  FORMAT(13HCOMPLEX ROOTS)
 312  FORMAT(4HX1=(,2E12.4,6H),X2=(,2E12.4,1H))
      GO TO 999
 2    X1=-B/(2.*A)
      X2=X1
      PRINT 321
      PRINT 332,X1,X2
 321  FORMAT(16HEQUAL REAL ROOTS)
      GO TO 999
 3    X1= (-B+SQRT(DISC)) / (2.*A)
      X2= (-B-SQRT(DISC)) / (2.*A)
      PRINT 331
      PRINT 332,X1,X2
 331  FORMAT(10HREAL ROOTS)
 332  FORMAT(3HX1=,E12.5,4H,X2=,E12.5)
 999  STOP

```



## GAP


```gap
QuadraticRoots := function(a, b, c)
  local d;
  d := Sqrt(b*b - 4*a*c);
  return [ (-b+d)/(2*a), (-b-d)/(2*a) ];
end;

# Hint : E(12) is a 12th primitive root of 1
QuadraticRoots(2, 2, -1);
# [ 1/2*E(12)^4-1/2*E(12)^7+1/2*E(12)^8+1/2*E(12)^11,
#   1/2*E(12)^4+1/2*E(12)^7+1/2*E(12)^8-1/2*E(12)^11 ]

# This works also with floating-point numbers
QuadraticRoots(2.0, 2.0, -1.0);
# [ 0.366025, -1.36603 ]
```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func qr(a, b, c float64) ([]float64, []complex128) {
    d := b*b-4*a*c
    switch {
    case d == 0:
        // single root
        return []float64{-b/(2*a)}, nil
    case d > 0:
        // two real roots
        if b < 0 {
            d = math.Sqrt(d)-b
        } else {
            d = -math.Sqrt(d)-b
        }
        return []float64{d/(2*a), (2*c)/d}, nil
    case d < 0:
        // two complex roots

        den := 1/(2*a)
        t1 := complex(-b*den, 0)
        t2 := complex(0, math.Sqrt(-d)*den)
        return nil, []complex128{t1+t2, t1-t2}
    }
    // otherwise d overflowed or a coefficient was NAN
    return []float64{d}, nil
}

func test(a, b, c float64) {
    fmt.Print("coefficients: ", a, b, c, " -> ")
    r, i := qr(a, b, c)
    switch len(r) {
    case 1:
        fmt.Println("one real root:", r[0])
    case 2:
        fmt.Println("two real roots:", r[0], r[1])
    default:
        fmt.Println("two complex roots:", i[0], i[1])
    }
}

func main() {
    for _, c := range [][3]float64{
        {1, -2, 1},
        {1, 0, 1},
        {1, -10, 1},
        {1, -1000, 1},
        {1, -1e9, 1},
    } {
        test(c[0], c[1], c[2])
    }
}
```

```txt

coefficients: 1 -2 1 -> one real root: 1
coefficients: 1 0 1 -> two complex roots: (0+1i) (-0-1i)
coefficients: 1 -10 1 -> two real roots: 9.898979485566356 0.10102051443364381
coefficients: 1 -1000 1 -> two real roots: 999.998999999 0.001000001000002
coefficients: 1 -1e+09 1 -> two real roots: 1e+09 1e-09

```



## Haskell


```haskell
import Data.Complex (Complex, realPart)

type CD = Complex Double

quadraticRoots :: (CD, CD, CD) -> (CD, CD)
quadraticRoots (a, b, c) =
  if realPart b > 0
    then ((2 * c) / (-b - d), (-b - d) / (2 * a))
    else ((-b + d) / (2 * a), (2 * c) / (-b + d))
  where
    d = sqrt $ b ^ 2 - 4 * a * c

main :: IO ()
main =
  mapM_
    (print . quadraticRoots)
    [(3, 4, 4 / 3), (3, 2, -1), (3, 2, 1), (1, -10e5, 1), (1, -10e9, 1)]
```

```txt
((-0.6666666666666666) :+ 0.0,(-0.6666666666666666) :+ 0.0)
(0.3333333333333333 :+ 0.0,(-1.0) :+ 0.0)
((-0.33333333333333326) :+ 0.4714045207910316,(-0.3333333333333333) :+ (-0.47140452079103173))
(999999.999999 :+ 0.0,1.000000000001e-6 :+ 0.0)
(1.0e10 :+ 0.0,1.0e-10 :+ 0.0)
```



## IDL


```idl
compile_OPT IDL2

print, "input a, press enter, input b, press enter, input c, press enter"
read,a,b,c
Promt='Enter values of a,b,c and hit enter'

a0=0.0
b0=0.0
c0=0.0   ;make them floating point variables

x=-b+sqrt((b^2)-4*a*c)
y=-b-sqrt((b^2)-4*a*c)
z=2*a
d= x/z
e= y/z

print, d,e
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages.

```unicon
procedure main()
    solve(1.0, -10.0e5, 1.0)
end

procedure solve(a,b,c)
    d := sqrt(b*b - 4.0*a*c)
    roots := if b < 0 then [r1 := (-b+d)/2.0*a, c/(a*r1)]
                      else [r1 := (-b-d)/2.0*a, c/(a*r1)]
    write(a,"*x^2 + ",b,"*x + ",c," has roots ",roots[1]," and ",roots[2])
end
```


```txt

->rqf 1.0 -0.000000001 1.0
1.0*x^2 + -1000000.0*x + 1.0 has roots 999999.999999 and 1.000000000001e-06
->

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>
100 PROGRAM "Quadratic.bas"
110 PRINT "Enter coefficients a, b and c:":INPUT PROMPT "a= ,b= ,c= ":A,B,C
120 IF A=0 THEN
130   PRINT "The coefficient of x^2 can not be 0."
140 ELSE
150   LET D=B^2-4*A*C
160   SELECT CASE SGN(D)
170   CASE 0
180     PRINT "The single root is ";-B/2/A
190   CASE 1
200     PRINT "The real roots are ";(-B+SQR(D))/(2*A);"and ";(-B-SQR(D))/(2*A)
210   CASE -1
220     PRINT "The complex roots are ";-B/2/A;"+/- ";STR$(SQR(-D)/2/A);"*i"
230   END SELECT
240 END IF
```



## J

'''Solution''' use J's built-in polynomial solver:
    p.
'''Example''' using inputs from other solutions and the unstable example from the task description:


```j
   coeff =. _3 |.\ 3 4 4r3   3 2 _1   3 2 1   1 _1e6 1   1 _1e9 1
   > {:"1 p. coeff
         _0.666667           _0.666667
                _1            0.333333
_0.333333j0.471405 _0.333333j_0.471405
               1e6                1e_6
               1e9                1e_9
```


Of course <code>p.</code> generalizes to polynomials of arbitrary order (which isn't as great as that might sound, because of practical limitations). Given the coefficients <code>p.</code> returns the multiplier and roots of the polynomial. Given the multiplier and roots it returns the coefficients. For example using the cubic <math>0 + 16x - 12x^2 + 2x^3</math>:

```j
   p. 0 16 _12 2   NB. return multiplier ; roots
+-+-----+
|2|4 2 0|
+-+-----+
   p. 2 ; 4 2 0    NB. return coefficients
0 16 _12 2
```


Exploring the limits of precision:


```j
   1{::p. 1 _1e5 1                   NB. display roots
100000 1e_5
   1 _1e5 1 p. 1{::p. 1 _1e5 1       NB. test roots
_3.38436e_7 0
   1 _1e5 1 p. 1e5 1e_5              NB. test displayed roots
1 9.99999e_11
   1e5 1e_5 - 1{::p. 1 _1e5 1        NB. find difference
1e_5 _1e_15
   1 _1e5 1 p. 1e5 1e_5-1e_5 _1e_15  NB. test displayed roots with adjustment
_3.38436e_7 0
```


When these "roots" are plugged back into the original polynomial, the results are nowhere near zero.  However, double precision floating point does not have enough bits to represent the (extremely close) answers that would give a zero result.

Middlebrook formula implemented in J


```j
q_r=: verb define
  'a b c' =. y
  q=. b %~ %: a * c
  f=. 0.5 + 0.5 * %:(1-4*q*q)
  (-b*f%a),(-c%b*f)
)

   q_r 1 _1e6 1
1e6 1e_6
```



## Java


```java
public class QuadraticRoots {
    private static class Complex {
        double re, im;

        public Complex(double re, double im) {
            this.re = re;
            this.im = im;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {return true;}
            if (!(obj instanceof Complex)) {return false;}
            Complex other = (Complex) obj;
            return (re == other.re) && (im == other.im);
        }

        @Override
        public String toString() {
            if (im == 0.0) {return String.format("%g", re);}
            if (re == 0.0) {return String.format("%gi", im);}
            return String.format("%g %c %gi", re,
                (im < 0.0 ? '-' : '+'), Math.abs(im));
        }
    }

    private static Complex[] quadraticRoots(double a, double b, double c) {
        Complex[] roots = new Complex[2];
        double d = b * b - 4.0 * a * c;  // discriminant
        double aa = a + a;

        if (d < 0.0) {
            double re = -b / aa;
            double im = Math.sqrt(-d) / aa;
            roots[0] = new Complex(re, im);
            roots[1] = new Complex(re, -im);
        } else if (b < 0.0) {
            // Avoid calculating -b - Math.sqrt(d), to avoid any
            // subtractive cancellation when it is near zero.
            double re = (-b + Math.sqrt(d)) / aa;
            roots[0] = new Complex(re, 0.0);
            roots[1] = new Complex(c / (a * re), 0.0);
        } else {
            // Avoid calculating -b + Math.sqrt(d).
            double re = (-b - Math.sqrt(d)) / aa;
            roots[1] = new Complex(re, 0.0);
            roots[0] = new Complex(c / (a * re), 0.0);
        }
        return roots;
    }

    public static void main(String[] args) {
        double[][] equations = {
            {1.0, 22.0, -1323.0},   // two distinct real roots
            {6.0, -23.0, 20.0},     //   with a != 1.0
            {1.0, -1.0e9, 1.0},     //   with one root near zero
            {1.0, 2.0, 1.0},        // one real root (double root)
            {1.0, 0.0, 1.0},        // two imaginary roots
            {1.0, 1.0, 1.0}         // two complex roots
        };
        for (int i = 0; i < equations.length; i++) {
            Complex[] roots = quadraticRoots(
                equations[i][0], equations[i][1], equations[i][2]);
            System.out.format("%na = %g   b = %g   c = %g%n",
                equations[i][0], equations[i][1], equations[i][2]);
            if (roots[0].equals(roots[1])) {
                System.out.format("X1,2 = %s%n", roots[0]);
            } else {
                System.out.format("X1 = %s%n", roots[0]);
                System.out.format("X2 = %s%n", roots[1]);
            }
        }
    }
}
```

```txt

a = 1.00000   b = 22.0000   c = -1323.00
X1 = 27.0000
X2 = -49.0000

a = 6.00000   b = -23.0000   c = 20.0000
X1 = 2.50000
X2 = 1.33333

a = 1.00000   b = -1.00000e+09   c = 1.00000
X1 = 1.00000e+09
X2 = 1.00000e-09

a = 1.00000   b = 2.00000   c = 1.00000
X1,2 = -1.00000

a = 1.00000   b = 0.00000   c = 1.00000
X1 = 1.00000i
X2 = -1.00000i

a = 1.00000   b = 1.00000   c = 1.00000
X1 = -0.500000 + 0.866025i
X2 = -0.500000 - 0.866025i
```



## jq

Currently jq does not include support for complex number operations, so
a small library is included in the first section.

The second section defines <tt>quadratic_roots(a;b;c)</tt>,
which emits a stream of 0 or two solutions, or the value <tt>true</tt> if a==b==c==0.

The third section defines a function for producing a table showing (i, error, solution) for solutions to x^2 - 10^i + 1 = 0 for various values of i.

'''Section 1''': Complex numbers (scrolling window)
<div style="overflow:scroll; height:200px;">

```jq
# Complex numbers as points [x,y] in the Cartesian plane
def real(z): if (z|type) == "number" then z else z[0] end;

def imag(z): if (z|type) == "number" then 0 else z[1] end;

def plus(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x+y, 0 ]
       else [ x + y[0], y[1]]
       end
    elif (y|type) == "number" then plus(y;x)
    else [ x[0] + y[0], x[1] + y[1] ]
    end;

def multiply(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x*y, 0 ]
       else [x * y[0], x * y[1]]
       end
    elif (y|type) == "number" then multiply(y;x)
    else [ x[0] * y[0] - x[1] * y[1],  x[0] * y[1] + x[1] * y[0]]
    end;

def negate(x): multiply(-1; x);

def minus(x; y): plus(x; multiply(-1; y));

def conjugate(z):
  if (z|type) == "number" then [z, 0]
  else  [z[0], -(z[1]) ]
  end;

def invert(z):
  if (z|type) == "number" then [1/z, 0]
  else
    ( (z[0] * z[0]) + (z[1] * z[1]) ) as $d
   # use "0 + ." to convert -0 back to 0
    | [ z[0]/$d, (0 + -(z[1]) / $d)]
  end;

def divide(x;y): multiply(x; invert(y));

def magnitude(z):
  real( multiply(z; conjugate(z))) | sqrt;

# exp^z
def complex_exp(z):
  def expi(x): [ (x|cos), (x|sin) ];
  if (z|type) == "number" then z|exp
  elif z[0] == 0 then expi(z[1])  # for efficiency
  else multiply( (z[0]|exp); expi(z[1]) )
  end ;

def complex_sqrt(z):
  if imag(z) == 0 and real(z) >= 0 then [(real(z)|sqrt), 0]
  else
    magnitude(z) as $r
    | if $r == 0 then [0,0]
      else
      (real(z)/$r) as $a
      | (imag(z)/$r) as $b
      | $r | sqrt as $r
      | (($a | acos) / 2)
      | [ ($r * cos), ($r * sin)]
      end
  end ;
```
</div>
'''Section 2:''' quadratic_roots(a;b;c)

```jq
# If there are infinitely many solutions, emit true;
# if none, emit empty;
# otherwise always emit two.
# For numerical accuracy, Middlebrook's approach is adopted:
def quadratic_roots(a; b; c):
  if a == 0 and b == 0 then
     if c == 0 then true # infinitely many
     else empty          # none
     end
  elif a == 0 then [-c/b, 0]
  elif b == 0 then (complex_sqrt(1/a) | (., negate(.)))
  else
    divide( plus(1.0; complex_sqrt( minus(1.0; (4 * a * c / (b*b))))); 2) as $f
    | negate(divide(multiply(b; $f); a)),
      negate(divide(c; multiply(b; $f)))
  end
;
```

'''Section 3''':
Produce a table showing [i, error, solution] for solutions to x^2 - 10^i + 1 = 0

```jq
def example:
  def pow(i): . as $in | reduce range(0;i) as $i (1; . * $in);
  def poly(a;b;c): plus( plus( multiply(a; multiply(.;.)); multiply(b;.)); c);
  def abs: if . < 0 then -. else . end;
  def zero(z):
    if z == 0 then 0
    else (magnitude(z)|abs) as $zero
    | if $zero < 1e-10 then "+0" else $zero end
    end;
  def lpad(n): tostring | (n - length) * " " + .;

  range(0; 13) as $i
  | (- (10|pow($i))) as $b
  | quadratic_roots(1; $b; 1) as $x
  | $x | poly(1; $b; 1) as $zero
  | "\($i|lpad(4)): error = \(zero($zero)|lpad(18)) x=\($x)"
;

example
```

{{Out}} (scrolling window)
<div style="overflow:scroll; height:200px;">

```sh

$ jq -M -r -n -f Roots_of_a_quadratic_function.jq
   0: error =                 +0 x=[0.5,0.8660254037844386]
   0: error =                 +0 x=[0.5000000000000001,-0.8660254037844387]
   1: error =                 +0 x=[9.898979485566356,0]
   1: error =                 +0 x=[0.10102051443364382,-0]
   2: error =                 +0 x=[99.98999899979995,0]
   2: error =                 +0 x=[0.010001000200050014,-0]
   3: error = 1.1641532182693481e-10 x=[999.998999999,0]
   3: error =                 +0 x=[0.0010000010000019998,-0]
   4: error =                 +0 x=[9999.999899999999,0]
   4: error =                 +0 x=[0.00010000000100000003,-0]
   5: error =                 +0 x=[99999.99999,0]
   5: error =                 +0 x=[1.0000000001e-05,-0]
   6: error =    0.0001220703125 x=[999999.9999989999,0]
   6: error =                 +0 x=[1.000000000001e-06,-0]
   7: error =           0.015625 x=[9999999.9999999,0]
   7: error =                 +0 x=[1.0000000000000101e-07,-0]
   8: error =                  1 x=[99999999.99999999,0]
   8: error =                 +0 x=[1e-08,-0]
   9: error =                  1 x=[1000000000,0]
   9: error =                 +0 x=[1e-09,-0]
  10: error =                  1 x=[10000000000,0]
  10: error =                 +0 x=[1e-10,-0]
  11: error =                  1 x=[100000000000,0]
  11: error =                 +0 x=[1e-11,-0]
  12: error =                  1 x=[1000000000000,0]
  12: error =                 +0 x=[1e-12,-0]
```
</div>


## Julia

This solution is an implementation of algorithm from the Goldberg paper cited in the task description.  It does check for <tt>a=0</tt> and returns the linear solution in that case.  Julia's <tt>sqrt</tt> throws a domain error for negative real inputs, so negative discriminants are converted to complex by adding <tt>0im</tt> prior to taking the square root.

Alternative solutions might make use of Julia's Polynomials or Roots packages.


```julia
function quadroots(x::Real, y::Real, z::Real)
    a, b, c = promote(float(x), y, z)
    if a ≈ 0.0 return [-c / b] end
    Δ = b ^ 2 - 4a * c
    if Δ ≈ 0.0 return [-sqrt(c / a)] end
    if Δ < 0.0 Δ = complex(Δ) end
    d = sqrt(Δ)
    if b < 0.0
        d -= b
        return [d / 2a, 2c / d]
    else
        d = -d - b
        return [2c / d, d / 2a]
    end
end

a = [1, 1, 1.0, 10]
b = [10, 2, -10.0 ^ 9, 1]
c = [1, 1, 1, 1]

for (x, y, z) in zip(a, b, c)
    @printf "The roots of %.2fx² + %.2fx + %.2f\n\tx₀ = (%s)\n" x y z join(round.(quadroots(x, y, z), 2), ", ")
end
```


```txt
The roots of 1.00x² + 10.00x + 1.00
	x₀ = (-0.1, -9.9)
The roots of 1.00x² + 2.00x + 1.00
	x₀ = (-1.0)
The roots of 1.00x² + -1000000000.00x + 1.00
	x₀ = (1.0e9, 0.0)
The roots of 10.00x² + 1.00x + 1.00
	x₀ = (-0.05 + 0.31im, -0.05 - 0.31im)
```



## Kotlin

```scala
import java.lang.Math.*

data class Equation(val a: Double, val b: Double, val c: Double) {
    data class Complex(val r: Double, val i: Double) {
        override fun toString() = when {
            i == 0.0 -> r.toString()
            r == 0.0 -> "${i}i"
            else -> "$r + ${i}i"
        }
    }

    data class Solution(val x1: Any, val x2: Any) {
        override fun toString() = when(x1) {
            x2 -> "X1,2 = $x1"
            else -> "X1 = $x1, X2 = $x2"
        }
    }

    val quadraticRoots by lazy {
        val _2a = a + a
        val d = b * b - 4.0 * a * c  // discriminant
         if (d < 0.0) {
            val r = -b / _2a
            val i = sqrt(-d) / _2a
            Solution(Complex(r, i), Complex(r, -i))
        } else {
            // avoid calculating -b +/- sqrt(d), to avoid any
            // subtractive cancellation when it is near zero.
            val r = if (b < 0.0) (-b + sqrt(d)) / _2a else (-b - sqrt(d)) / _2a
            Solution(r, c / (a * r))
        }
    }
}

fun main(args: Array<String>) {
    val equations = listOf(Equation(1.0, 22.0, -1323.0),   // two distinct real roots
                           Equation(6.0, -23.0, 20.0),     //  with a != 1.0
                           Equation(1.0, -1.0e9, 1.0),     //  with one root near zero
                           Equation(1.0, 2.0, 1.0),        // one real root (double root)
                           Equation(1.0, 0.0, 1.0),        // two imaginary roots
                           Equation(1.0, 1.0, 1.0))        // two complex roots

    equations.forEach { println("$it\n" + it.quadraticRoots) }
}
```

```txt
Equation(a=1.0, b=22.0, c=-1323.0)
X1 = -49.0, X2 = 27.0
Equation(a=6.0, b=-23.0, c=20.0)
X1 = 2.5, X2 = 1.3333333333333333
Equation(a=1.0, b=-1.0E9, c=1.0)
X1 = 1.0E9, X2 = 1.0E-9
Equation(a=1.0, b=2.0, c=1.0)
X1,2 = -1.0
Equation(a=1.0, b=0.0, c=1.0)
X1 = 1.0i, X2 = -1.0i
Equation(a=1.0, b=1.0, c=1.0)
X1 = -0.5 + 0.8660254037844386i, X2 = -0.5 + -0.8660254037844386i
```



## lambdatalk


```scheme

1) using lambdas:

{def equation
 {lambda {:a :b :c}
  {b equation :a*x{sup 2}+:b*x+:c=0}
  {{lambda {:a' :b' :d}
   {if {> :d 0}
   then {{lambda {:b' :d'}
         {equation.disp {+ :b' :d'} {- :b' :d'} 2 real roots}
        } :b' {/ {sqrt :d} :a'}}
   else {if {< :d 0}
   then {{lambda {:b' :d'}
         {equation.disp [:b',:d'] [:b',-:d'] 2 complex roots}
        } :b' {/ {sqrt {- :d}} :a'} }
   else {equation.disp :b'  :b' one real double root}
  }}
  } {* 2 :a} {/ {- :b} {* 2 :a}} {- {* :b :b} {* 4 :a :c}} } }}

2) using let:

{def equation
 {lambda {:a :b :c}
  {b equation :a*x{sup 2}+:b*x+:c=0}
  {let { {:a' {* 2 :a}}
         {:b' {/ {- :b} {* 2 :a}}}
         {:d  {- {* :b :b} {* 4 :a :c}}} }
   {if {> :d 0}
    then {let { {:b' :b'}
                {:d' {/ {sqrt :d} :a'}} }
          {equation.disp {+ :b' :d'} {- :b' :d'} 2 real roots} }
    else {if {< :d 0}
    then {let { {:b' :b'}
                {:d' {/ {sqrt {- :d}} :a'}} }
          {equation.disp [:b',:d'] [:b',-:d'] 2 complex roots} }
    else  {equation.disp :b' :b' one real double root} }} }}}

3) a function to display results in an HTML table format

{def equation.disp
 {lambda {:x1 :x2 :txt}
  {table {@ style="background:#ffa"}
   {tr {td :txt:    }}
   {tr {td x1 = :x1 }}
   {tr {td x2 = :x2 }} } }}

4) testing:

equation 1*x2+1*x+-1=0
2 real roots:
 x1 = 0.6180339887498949
 x2 = -1.618033988749895

equation 1*x2+1*x+1=0
2 complex roots:
 x1 = [-0.5,0.8660254037844386]
 x2 = [-0.5,-0.8660254037844386]

equation 1*x2+-2*x+1=0
one real double root:
 x1 = 1
 x2 = 1

```



## Liberty BASIC


```lb
a=1:b=2:c=3
    'assume a<>0
    print quad$(a,b,c)
    end

function quad$(a,b,c)
    D=b^2-4*a*c
    x=-1*b
    if D<0 then
        quad$=str$(x/(2*a));" +i";str$(sqr(abs(D))/(2*a));" , ";str$(x/(2*a));" -i";str$(sqr(abs(D))/abs(2*a))
    else
        quad$=str$(x/(2*a)+sqr(D)/(2*a));" , ";str$(x/(2*a)-sqr(D)/(2*a))
    end if
end function
```



## Logo


```logo
to quadratic :a :b :c
  localmake "d sqrt (:b*:b - 4*:a*:c)
  if :b < 0 [make "d minus :d]
  output list (:d-:b)/(2*:a) (2*:c)/(:d-:b)
end
```



## Lua

In order to correctly handle complex roots, qsolve must be given objects from a suitable complex number library,
like that from the Complex Numbers article. However, this should be enough to demonstrate its accuracy:


```lua
function qsolve(a, b, c)
  if b < 0 then return qsolve(-a, -b, -c) end
  val = b + (b^2 - 4*a*c)^(1/2) --this never exhibits instability if b > 0
  return -val / (2 * a), -2 * c / val --2c / val is the same as the "unstable" second root
end

for i = 1, 12 do
  print(qsolve(1, 0-10^i, 1))
end
```

The "trick" lies in avoiding subtracting large values that differ by a small amount, which is the source of instability in the "normal" formula. It is trivial to prove that 2c/(b + sqrt(b^2-4ac)) = (b - sqrt(b^2-4ac))/2a.



## Maple


```Maple
solve(a*x^2+b*x+c,x);

solve(1.0*x^2-10.0^9*x+1.0,x,explicit,allsolutions);

fsolve(x^2-10^9*x+1,x,complex);
```

```txt
                                (1/2)                     (1/2)
                   /          2\             /          2\
              -b + \-4 a c + b /         b + \-4 a c + b /
              -----------------------, - ----------------------
                        2 a                       2 a

                                    9                -9
                      1.000000000 10 , 1.000000000 10

                                    -9                9
                      1.000000000 10  , 1.000000000 10
```



## Mathematica

Possible ways to do this are (symbolic and numeric examples):

```Mathematica
Solve[a x^2 + b x + c == 0, x]
Solve[x^2 - 10^5 x + 1 == 0, x]
Root[#1^2 - 10^5 #1 + 1 &, 1]
Root[#1^2 - 10^5 #1 + 1 &, 2]
Reduce[a x^2 + b x + c == 0, x]
Reduce[x^2 - 10^5 x + 1 == 0, x]
FindInstance[x^2 - 10^5 x + 1 == 0, x, Reals, 2]
FindRoot[x^2 - 10^5 x + 1 == 0, {x, 0}]
FindRoot[x^2 - 10^5 x + 1 == 0, {x, 10^6}]
```

gives back:

<math>\left\{\left\{x\to \frac{-b-\sqrt{b^2-4 a c}}{2 a}\right\},\left\{x\to \frac{-b+\sqrt{b^2-4 a c}}{2 a}\right\}\right\}</math>

<math>\left\{\left\{x\to \frac{1}{50000+\sqrt{2499999999}}\right\},\left\{x\to 50000+\sqrt{2499999999}\right\}\right\}</math>

<math>50000-\sqrt{2499999999}</math>

<math>50000+\sqrt{2499999999}</math>

<math>\begin{align}
  \Biggl(
  a & \neq 0  \And \And
     \left(
        x==\frac{-b-\sqrt{b^2-4 a c}}{2 a}
        \|
        x==\frac{-b+\sqrt{b^2-4 a c}}{2 a}
      \right)
  \Biggr)\\
  &\biggl\|
    \left(
      a==0 \And\And b\neq 0 \And\And x==-\frac{c}{b}
    \right)\\
  &\biggr\|
  (c==0 \And \And b==0 \And \And a==0)
\end{align}
</math>

<math>x==\frac{1}{50000+\sqrt{2499999999}}\|x==50000+\sqrt{2499999999}</math>

<math>\left\{\left\{x\to \frac{1}{50000+\sqrt{2499999999}}\right\},\left\{x\to 50000+\sqrt{2499999999}\right\}\right\}</math>

<math>\{x\to 0.00001\}</math>

<math>\{x\to 100000.\}</math>

Note that some functions do not really give the answer (like reduce) rather it gives another way of writing it (boolean expression). However note that reduce gives the explicit cases for a zero and nonzero, b zero and nonzero, et cetera. Some functions are numeric by nature, other can handle both symbolic and numeric. In generals the solution will be exact if the input is exact. Any exact result can be approximated to '''arbitrary''' precision using the function N[expression,number of digits]. Further notice that some functions give back exact answers in a different form then others, however the answers are both correct, the answers are just written differently.

=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
roots([1 -3 2])    % coefficients in decreasing order of power e.g. [x^n ... x^2 x^1 x^0]
```



## Maxima


```maxima
solve(a*x^2 + b*x + c = 0, x);

/*                2                         2
            sqrt(b  - 4 a c) + b      sqrt(b  - 4 a c) - b
     [x = - --------------------, x = --------------------]
                    2 a                       2 a              */

fpprec: 40$

solve(x^2 - 10^9*x + 1 = 0, x);
/* [x = 500000000 - sqrt(249999999999999999),
    x = sqrt(249999999999999999) + 500000000] */

bfloat(%);
/* [x = 1.0000000000000000009999920675269450501b-9,
    x = 9.99999999999999998999999999999999999b8] */
```


=={{header|MK-61/52}}==
<lang>П2	С/П	/-/	<->	/	2	/	П3	x^2	С/П
ИП2	/	-	Вx	<->	КвКор	НОП	x>=0	28	ИП3
x<0	24	<->	/-/	+	/	Вx	С/П	/-/	КвКор
ИП3	С/П
```


''Input:'' a С/П b С/П c С/П

{{out}} x<sub>1</sub> - РX; x<sub>2</sub> - РY (or error message, if D < 0).

=={{header|Modula-3}}==
```modula3
MODULE Quad EXPORTS Main;

IMPORT IO, Fmt, Math;

TYPE Roots = ARRAY [1..2] OF LONGREAL;

VAR r: Roots;

PROCEDURE Solve(a, b, c: LONGREAL): Roots =
  VAR sd: LONGREAL := Math.sqrt(b * b - 4.0D0 * a * c);
      x: LONGREAL;
  BEGIN
    IF b < 0.0D0 THEN
      x := (-b + sd) / 2.0D0 * a;
      RETURN Roots{x, c / (a * x)};
    ELSE
      x := (-b - sd) / 2.0D0 * a;
      RETURN Roots{c / (a * x), x};
    END;
  END Solve;

BEGIN
  r := Solve(1.0D0, -10.0D5, 1.0D0);
  IO.Put("X1 = " & Fmt.LongReal(r[1]) & " X2 = " & Fmt.LongReal(r[2]) & "\n");
END Quad.
```



## OCaml



```ocaml
type quadroots =
  | RealRoots of float * float
  | ComplexRoots of Complex.t * Complex.t ;;

let quadsolve a b c =
  let d = (b *. b) -. (4.0 *. a *. c) in
  if d < 0.0
  then
    let r = -. b /. (2.0 *. a)
    and i = sqrt(-. d) /. (2.0 *. a) in
    ComplexRoots ({ Complex.re = r; Complex.im = i },
                  { Complex.re = r; Complex.im = (-.i) })
  else
    let r =
      if b < 0.0
      then ((sqrt d) -. b) /. (2.0 *. a)
      else ((sqrt d) +. b) /. (-2.0 *. a)
    in
    RealRoots (r, c /. (r *. a))
;;
```


```ocaml
# quadsolve 1.0 0.0 (-2.0) ;;
- : quadroots = RealRoots (-1.4142135623730951, 1.4142135623730949)

# quadsolve 1.0 0.0 2.0 ;;
- : quadroots =
ComplexRoots ({Complex.re = 0.; Complex.im = 1.4142135623730951},
 {Complex.re = 0.; Complex.im = -1.4142135623730951})

# quadsolve 1.0 (-1.0e5) 1.0 ;;
- : quadroots = RealRoots (99999.99999, 1.0000000001000001e-005)
```



## Octave

See [[Quadratic Equation#MATLAB|MATLAB]].


## PARI/GP

```parigp
roots(a,b,c)=polrootsreal(Pol([a,b,c]))
```


Otherwise, coding directly:

```parigp
roots(a,b,c)={
  b /= a;
  c /= a;
  my (delta = b^2 - 4*c, root=sqrt(delta));
  if (delta < 0,
    [root-b,-root-b]/2
  ,
    my(sol=if(b>0, -b - root,-b + root)/2);
    [sol,c/sol]
  )
};
```


Either way,

```parigp
roots(1,-1e9,1)
```

gives one root around 0.000000001000000000000000001 and one root around 999999999.999999999.


## Pascal

some parts translated from Modula2

```pascal
Program QuadraticRoots;

var
  a, b, c, q, f: double;

begin
  a := 1;
  b := -10e9;
  c := 1;
  q := sqrt(a * c) / b;
  f := (1 + sqrt(1 - 4 * q * q)) / 2;

  writeln ('Version 1:');
  writeln ('x1: ', (-b * f / a):16, ', x2: ', (-c / (b * f)):16);

  writeln ('Version 2:');
  q := sqrt(b * b - 4 * a * c);
  if b < 0 then
  begin
    f :=  (-b + q) / 2 * a;
    writeln ('x1: ', f:16, ', x2: ', (c / (a * f)):16);
  end
  else
  begin
    f := (-b - q) / 2 * a;
    writeln ('x1: ', (c / (a * f)):16, ', x2: ', f:16);
  end;
end.

```

```txt

Version 1:
x1:  1.00000000E+010, x2:  1.00000000E-010
Version 2:
x1:  1.00000000E+010, x2:  1.00000000E-010

```



## Perl

When using [http://perldoc.perl.org/Math/Complex.html Math::Complex] perl automatically convert numbers when necessary.

```perl
use Math::Complex;

($x1,$x2) = solveQuad(1,2,3);

print "x1 = $x1, x2 = $x2\n";

sub solveQuad
{
	my ($a,$b,$c) = @_;
	my $root = sqrt($b**2 - 4*$a*$c);
	return ( -$b + $root )/(2*$a), ( -$b - $root )/(2*$a);
}
```



## Perl 6


Perl 6 has complex number handling built in.


```perl6
for
[1, 2, 1],
[1, 2, 3],
[1, -2, 1],
[1, 0, -4],
[1, -10**6, 1]
-> @coefficients {
    printf "Roots for %d, %d, %d\t=> (%s, %s)\n",
    |@coefficients, |quadroots(@coefficients);
}

sub quadroots (*[$a, $b, $c]) {
    ( -$b + $_ ) / (2 * $a),
    ( -$b - $_ ) / (2 * $a)
    given
    ($b ** 2 - 4 * $a * $c ).Complex.sqrt.narrow
}
```

```txt
Roots for 1, 2, 1       => (-1, -1)
Roots for 1, 2, 3       => (-1+1.4142135623731i, -1-1.4142135623731i)
Roots for 1, -2, 1      => (1, 1)
Roots for 1, 0, -4      => (2, -2)
Roots for 1, -1000000, 1        => (999999.999999, 1.00000761449337e-06)
```



## Phix

```Phix
procedure solve_quadratic(sequence t3)
atom {a,b,c} = t3
atom d = b*b-4*a*c, f
string s = sprintf("for a=%g,b=%g,c=%g",t3), t
sequence u
    if abs(d)<1e-6 then d=0 end if
    switch sign(d) do
        case 0: t = "single root is %g"
                u = {-b/2/a}
        case 1: t = "real roots are %g and %g"
                f = (1+sqrt(1-4*a*c/(b*b)))/2
                u = {-f*b/a,-c/b/f}
        case-1: t = "complex roots are %g +/- %g*i"
                u = {-b/2/a,sqrt(-d)/2/a}
    end switch
    printf(1,"%-25s the %s\n",{s,sprintf(t,u)})
end procedure

constant tests = {{1,-1E9,1},
                  {1,0,1},
                  {2,-1,-6},
                  {1,2,-2},
                  {0.5,1.4142135,1},
                  {1,3,2},
                  {3,4,5}}

for i=1 to length(tests) do
    solve_quadratic(tests[i])
end for
```


```txt

for a=1,b=-1e+9,c=1       the real roots are 1e+9 and 1e-9
for a=1,b=0,c=1           the complex roots are 0 +/- 1*i
for a=2,b=-1,c=-6         the real roots are 2 and -1.5
for a=1,b=2,c=-2          the real roots are -2.73205 and 0.732051
for a=0.5,b=1.41421,c=1   the single root is -1.41421
for a=1,b=3,c=2           the real roots are -2 and -1
for a=3,b=4,c=5           the complex roots are -0.666667 +/- 1.10554*i

```



## PicoLisp


```PicoLisp
(scl 40)

(de solveQuad (A B C)
   (let SD (sqrt (- (* B B) (* 4 A C)))
      (if (lt0 B)
         (list
            (*/ (- SD B) A 2.0)
            (*/ C 2.0 (*/ A A (- SD B) `(* 1.0 1.0))) )
         (list
            (*/ C 2.0 (*/ A A (- 0 B SD) `(* 1.0 1.0)))
            (*/ (- 0 B SD) A 2.0) ) ) ) )

(mapcar round
   (solveQuad 1.0 -1000000.0 1.0)
   (6 .) )
```

```txt
-> ("999,999.999999" "0.000001")
```



## PL/I


```PL/I

   declare (c1, c2) float complex,
           (a, b, c, x1, x2) float;

   get list (a, b, c);
   if b**2 < 4*a*c then
      do;
         c1 = (-b + sqrt(b**2 - 4+0i*a*c)) / (2*a);
         c2 = (-b - sqrt(b**2 - 4+0i*a*c)) / (2*a);
         put data (c1, c2);
      end;
   else
      do;
         x1 = (-b + sqrt(b**2 - 4*a*c)) / (2*a);
         x2 = (-b - sqrt(b**2 - 4*a*c)) / (2*a);
         put data (x1, x2);
      end;

```



## Python

This solution compares the naïve method with three "better" methods.

```python
#!/usr/bin/env python3

import math
import cmath
import numpy

def quad_discriminating_roots(a,b,c, entier = 1e-5):
    """For reference, the naive algorithm which shows complete loss of
    precision on the quadratic in question.  (This function also returns a
    characterization of the roots.)"""
    discriminant = b*b - 4*a*c
    a,b,c,d =complex(a), complex(b), complex(c), complex(discriminant)
    root1 = (-b + cmath.sqrt(d))/2./a
    root2 = (-b - cmath.sqrt(d))/2./a
    if abs(discriminant) < entier:
        return "real and equal", abs(root1), abs(root1)
    if discriminant > 0:
        return "real", root1.real, root2.real
    return "complex", root1, root2

def middlebrook(a, b, c):
    try:
        q = math.sqrt(a*c)/b
        f = .5+ math.sqrt(1-4*q*q)/2
    except ValueError:
        q = cmath.sqrt(a*c)/b
        f = .5+ cmath.sqrt(1-4*q*q)/2
    return (-b/a)*f, -c/(b*f)

def whatevery(a, b, c):
    try:
        d = math.sqrt(b*b-4*a*c)
    except ValueError:
        d = cmath.sqrt(b*b-4*a*c)
    if b > 0:
        return div(2*c, (-b-d)), div((-b-d), 2*a)
    else:
        return div((-b+d), 2*a), div(2*c, (-b+d))

def div(n, d):
    """Divide, with a useful interpretation of division by zero."""
    try:
        return n/d
    except ZeroDivisionError:
        if n:
            return n*float('inf')
        return float('nan')

testcases = [
    (3, 4, 4/3),    # real, equal
    (3, 2, -1),     # real, unequal
    (3, 2, 1),      # complex
    (1, -1e9, 1),   # ill-conditioned "quadratic in question" required by task.
    (1, -1e100, 1),
    (1, -1e200, 1),
    (1, -1e300, 1),
]

print('Naive:')
for c in testcases:
    print("{} {:.5} {:.5}".format(*quad_discriminating_roots(*c)))

print('\nMiddlebrook:')
for c in testcases:
    print(("{:.5} "*2).format(*middlebrook(*c)))

print('\nWhat Every...')
for c in testcases:
    print(("{:.5} "*2).format(*whatevery(*c)))

print('\nNumpy:')
for c in testcases:
    print(("{:.5} "*2).format(*numpy.roots(c)))
```

```txt

Naive:
real and equal 0.66667 0.66667
real 0.33333 -1.0
complex (-0.33333+0.4714j) (-0.33333-0.4714j)
real 1e+09 0.0
real 1e+100 0.0
real nan nan
real nan nan

Middlebrook:
-0.66667 -0.66667
(-1+0j) (0.33333+0j)
(-0.33333-0.4714j) (-0.33333+0.4714j)
1e+09 1e-09
1e+100 1e-100
1e+200 1e-200
1e+300 1e-300

What Every...
-0.66667 -0.66667
0.33333 -1.0
(-0.33333+0.4714j) (-0.33333-0.4714j)
1e+09 1e-09
1e+100 1e-100
inf 0.0
inf 0.0

Numpy:
-0.66667 -0.66667
-1.0 0.33333
(-0.33333+0.4714j) (-0.33333-0.4714j)
1e+09 1e-09
1e+100 1e-100
1e+200 1e-200
1e+300 0.0

```



## R

```R
quaddiscrroots <- function(a,b,c, tol=1e-5) {
  d <- b*b - 4*a*c + 0i
  root1 <- (-b + sqrt(d))/(2*a)
  root2 <- (-b - sqrt(d))/(2*a)
  if ( abs(Re(d)) < tol ) {
    list("real and equal", abs(root1), abs(root1))
  } else if ( Re(d) > 0 ) {
    list("real", Re(root1), Re(root2))
  } else {
    list("complex", root1, root2)
  }
}

for(coeffs in list(c(3,4,4/3), c(3,2,-1), c(3,2,1), c(1, -1e6, 1)) ) {
  cat(sprintf("roots of %gx^2 %+gx^1 %+g are\n", coeffs[1], coeffs[2], coeffs[3]))
  r <- quaddiscrroots(coeffs[1], coeffs[2], coeffs[3])
  cat(sprintf("  %s: %s, %s\n", r[[1]], r[[2]], r[[3]]))
}
```



## Racket


```Racket
#lang racket
(define (quadratic a b c)
  (let* ((-b (- b))
         (delta (- (expt b 2) (* 4 a c)))
         (denominator (* 2 a)))
    (list
     (/ (+ -b (sqrt delta)) denominator)
     (/ (- -b (sqrt delta)) denominator))))

;(quadratic 1 0.0000000000001 -1)
;'(0.99999999999995 -1.00000000000005)
;(quadratic 1 0.0000000000001 1)
;'(-5e-014+1.0i -5e-014-1.0i)
```



## REXX


### version 1

The REXX language doesn't have a   '''sqrt'''   function,   nor does it support complex numbers natively.

Since "unlimited" decimal precision is part of the REXX language, the   '''numeric digits'''   was increased

(from a default of   '''9''')   to   '''200'''   to accommodate when a root is closer to zero than the other root.

Note that only ten decimal digits (precision) are shown in the   ''displaying''   of the output.

This REXX version supports   ''complex numbers''   for the result.

```rexx
/*REXX program finds the roots (which may be complex) of a quadratic function.*/
numeric digits 200                     /*use enough digits to handle extremes.*/
parse arg a b c .                      /*obtain the specified arguments: A B C*/
call quadratic  a,b,c                  /*solve quadratic function via the sub.*/
numeric digits  10                     /*reduce (output) digits for human eyes*/
r1=r1/1;  r2=r2/1; a=a/1; b=b/1; c=c/1 /*normalize numbers to the new digits. */
if r1j\=0  then r1=r1 || left('+',r1j>0)(r1j/1)"i"    /*handle complex number.*/
if r2j\=0  then r2=r2 || left('+',r2j>0)(r2j/1)"i"    /*   "      "       "   */
              say '    a ='   a        /*display the normalized value of   A. */
              say '    b ='   b        /*   "     "       "       "    "   B. */
              say '    c ='   c        /*   "     "       "       "    "   C. */
      say;    say 'root1 ='   r1       /*   "     "       "       "   1st root*/
              say 'root2 ='   r2       /*   "     "       "       "   2nd root*/
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
quadratic:  parse arg aa,bb,cc         /*obtain the specified three arguments.*/
   $=sqrt(bb**2-4*aa*cc);  L=length($) /*compute  SQRT (which may be complex).*/
   r=1/(aa+aa);     ?=right($,1)=='i'  /*compute reciprocal of 2*aa;  Complex?*/
   if ?  then do;  r1= -bb   *r;  r2=r1;        r1j=left($,L-1)*r; r2j=-r1j; end
         else do;  r1=(-bb+$)*r;  r2=(-bb-$)*r; r1j=0;             r2j= 0;   end
   return
/*────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x 1 ox;   if x=0  then return 0;   d=digits();   m.=9
      numeric digits 9;    numeric form;    h=d+6;    x=abs(x)
      parse value format(x,2,1,,0)  'E0'   with   g 'E' _ .;      g=g *.5'e'_ %2
            do j=0  while h>9;      m.j=h;              h=h%2+1;       end /*j*/
            do k=j+5  to 0  by -1;  numeric digits m.k; g=(g+x/g)*.5;  end /*k*/
      numeric digits d;    return (g/1)left('i',ox<0)  /*make complex if OX<0.*/
```

'''output'''   when using the input of:   <tt> 1   -10e5   1 </tt>

```txt

    a = 1
    b = -1000000
    c = 1

root1 = 1000000
root2 = 0.000001

```

The following output is when Regina 3.9.1 REXX is used.

'''output'''   when using the input of:   <tt> 1   -10e9   1 </tt>

```txt

    a = 1
    b = -10000000000
    c = 1

root1 = 1.000000000E+10
root2 = 1E-10

```

The following output is when R4 REXX is used.

'''output'''   when using the input of:   <tt> 1   -10e9   1 </tt>

```txt

    a = 1
    b = -1E+10
    c = 1

root1 = 1E+10
root2 = 0.0000000001

```

'''output'''   when using the input of:   <tt> 3   2   1 </tt>

```txt

    a = 3
    b = 2
    c = 1

root1 = -0.3333333333+0.4714045208i
root2 = -0.3333333333-0.4714045208i

```

'''output'''   when using the input of:   <tt> 1   0   1 </tt>

```txt

    a = 1
    b = 0
    c = 1

root1 = 0+1i
root2 = 0-1i

```



###  Version 2


```rexx
/* REXX ***************************************************************
* 26.07.2913 Walter Pachl
**********************************************************************/
  Numeric Digits 30
  Parse Arg a b c 1 alist
  Select
    When a='' | a='?' Then
      Call exit 'rexx qgl a b c solves a*x**2+b*x+c'
    When words(alist)<>3 Then
      Call exit 'three numbers are required'
    Otherwise
      Nop
    End
  gl=a'*x**2'
  Select
    When b<0 Then gl=gl||b'*x'
    When b>0 Then gl=gl||'+'||b'*x'
    Otherwise Nop
    End
  Select
    When c<0 Then gl=gl||c
    When c>0 Then gl=gl||'+'||c
    Otherwise Nop
    End
  Say gl '= 0'

  d=b**2-4*a*c
  If d<0 Then Do
    dd=sqrt(-d)
    r=-b/(2*a)
    i=dd/(2*a)
    x1=r'+'i'i'
    x2=r'-'i'i'
    End
  Else Do
    dd=sqrt(d)
    x1=(-b+dd)/(2*a)
    x2=(-b-dd)/(2*a)
    End
  Say 'x1='||x1
  Say 'x2='||x2
  Exit
sqrt:
/* REXX ***************************************************************
* EXEC to calculate the square root of x with high precision
**********************************************************************/
  Parse Arg x
  prec=digits()
  prec1=2*prec
  eps=10**(-prec1)
  k = 1
  Numeric Digits prec1
  r0= x
  r = 1
  Do i=1 By 1 Until r=r0 | (abs(r*r-x)<eps)
    r0 = r
    r  = (r + x/r) / 2
    k  = min(prec1,2*k)
    Numeric Digits (k + 5)
    End
  Numeric Digits prec
  Return (r+0)
exit: Say arg(1)
```

```txt

Version 1:
    a = 1
    b = -1
    c = 0

root1 = 1
root2 = 0

Version 2:
1*x**2-1.0000000001*x+1.e-9 = 0
x1=0.9999999991000000000025
x2=0.0000000009999999999975

```



## Ring

<lang>
x1 = 0
x2 = 0
quadratic(3, 4, 4/3.0)  # [-2/3]
see "x1 = " + x1 + " x2 = " + x2 + nl
quadratic(3, 2, -1)      # [1/3, -1]
see "x1 = " + x1 + " x2 = " + x2 + nl
quadratic(-2,  7, 15)    # [-3/2, 5]
see "x1 = " + x1 + " x2 = " + x2 + nl
quadratic(1, -2,  1)     # [1]
see "x1 = " + x1 + " x2 = " + x2 + nl

func quadratic a, b, c
     sqrtDiscriminant = sqrt(pow(b,2) - 4*a*c)
     x1 = (-b + sqrtDiscriminant) / (2.0*a)
     x2 = (-b - sqrtDiscriminant) / (2.0*a)
     return [x1, x2]

```



## Ruby

The CMath#sqrt method will return a Complex instance if necessary.

```ruby
require 'cmath'

def quadratic(a, b, c)
  sqrt_discriminant = CMath.sqrt(b**2 - 4*a*c)
  [(-b + sqrt_discriminant) / (2.0*a), (-b - sqrt_discriminant) / (2.0*a)]
end

p quadratic(3, 4, 4/3.0)  # [-2/3]
p quadratic(3, 2, -1)     # [1/3, -1]
p quadratic(3, 2,  1)     # [(-1/3 + sqrt(2/9)i), (-1/3 - sqrt(2/9)i)]
p quadratic(1, 0,  1)     # [(0+i), (0-i)]
p quadratic(1, -1e6, 1)   # [1e6, 1e-6]
p quadratic(-2,  7, 15)   # [-3/2, 5]
p quadratic(1, -2,  1)    # [1]
p quadratic(1,  3,  3)    # [(-3 + sqrt(3)i)/2), (-3 - sqrt(3)i)/2)]
```

```txt

[-0.6666666666666666, -0.6666666666666666]
[0.3333333333333333, -1.0]
[(-0.3333333333333333+0.47140452079103173i), (-0.3333333333333333-0.47140452079103173i)]
[(0.0+1.0i), (0.0-1.0i)]
[999999.999999, 1.00000761449337e-06]
[-1.5, 5.0]
[1.0, 1.0]
[(-1.5+0.8660254037844386i), (-1.5-0.8660254037844386i)]

```



## Run BASIC


```runbasic
print "FOR 1,2,3 => ";quad$(1,2,3)
print "FOR 4,5,6 => ";quad$(4,5,6)

FUNCTION quad$(a,b,c)
    d  = b^2-4 * a*c
    x  = -1*b
    if d<0 then
        quad$ = str$(x/(2*a));" +i";str$(sqr(abs(d))/(2*a))+" , "+str$(x/(2*a));" -i";str$(sqr(abs(d))/abs(2*a))
    else
        quad$ = str$(x/(2*a)+sqr(d)/(2*a))+" , "+str$(x/(2*a)-sqr(d)/(2*a))
    end if
END FUNCTION
```

```txt
FOR 1,2,3 => -1 +i1.41421356 , -1 -i1.41421356
FOR 4,5,6 => -0.625 +i1.05326872 , -0.625 -i1.05326872
```



## Scala

Using [[Arithmetic/Complex#Scala|Complex]] class from task Arithmetic/Complex.

```scala
import ArithmeticComplex._
object QuadraticRoots {
  def solve(a:Double, b:Double, c:Double)={
    val d = b*b-4.0*a*c
    val aa = a+a

    if (d < 0.0) {  // complex roots
      val re= -b/aa;
      val im = math.sqrt(-d)/aa;
      (Complex(re, im), Complex(re, -im))
    }
    else { // real roots
      val re=if (b < 0.0) (-b+math.sqrt(d))/aa else (-b -math.sqrt(d))/aa
      (re, (c/(a*re)))
    }
  }
}
```

Usage:

```scala
val equations=Array(
  (1.0, 22.0, -1323.0),   // two distinct real roots
  (6.0, -23.0, 20.0),     //   with a != 1.0
  (1.0, -1.0e9, 1.0),     //   with one root near zero
  (1.0, 2.0, 1.0),        // one real root (double root)
  (1.0, 0.0, 1.0),        // two imaginary roots
  (1.0, 1.0, 1.0)         // two complex roots
);

equations.foreach{v =>
  val (a,b,c)=v
  println("a=%g   b=%g   c=%g".format(a,b,c))
  val roots=solve(a, b, c)
  println("x1="+roots._1)
  if(roots._1 != roots._2) println("x2="+roots._2)
  println
}
```

```txt
a=1.00000   b=22.0000   c=-1323.00
x1=-49.0
x2=27.0

a=6.00000   b=-23.0000   c=20.0000
x1=2.5
x2=1.3333333333333333

a=1.00000   b=-1.00000e+09   c=1.00000
x1=1.0E9
x2=1.0E-9

a=1.00000   b=2.00000   c=1.00000
x1=-1.0

a=1.00000   b=0.00000   c=1.00000
x1=-0.0 + 1.0i
x2=-0.0 + -1.0i

a=1.00000   b=1.00000   c=1.00000
x1=-0.5 + 0.8660254037844386i
x2=-0.5 + -0.8660254037844386i
```



## Scheme


```scheme
(define (quadratic a b c)
	(if (= a 0)
	(if (= b 0) 'fail (- (/ c b)))
	(let ((delta (- (* b b) (* 4 a c))))
	(if (and (real? delta) (> delta 0))
		(let ((u (+ b (* (if (>= b 0) 1 -1) (sqrt delta)))))
			(list (/ u -2 a) (/ (* -2 c) u)))
		(list
			(/ (- (sqrt delta) b) 2 a)
			(/ (+ (sqrt delta) b) -2 a))))))


; examples

(quadratic 1 -1 -1)
; (1.618033988749895 -0.6180339887498948)

(quadratic 1 0 -2)
; (-1.4142135623730951 1.414213562373095)

(quadratic 1 0 2)
; (0+1.4142135623730951i 0-1.4142135623730951i)

(quadratic 1+1i 2 5)
; (-1.0922677260818898-1.1884256155834088i 0.09226772608188982+2.1884256155834088i)

(quadratic 0 4 3)
; -3/4

(quadratic 0 0 1)
; fail

(quadratic 1 2 0)
; (-2 0)

(quadratic 1 2 1)
; (-1 -1)

(quadratic 1 -1e5 1)
; (99999.99999 1.0000000001000001e-05)
```



## Seed7

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const type: roots is new struct
    var float: x1 is 0.0;
    var float: x2 is 0.0;
  end struct;

const func roots: solve (in float: a, in float: b, in float: c) is func
  result
    var roots: solution is roots.value;
  local
    var float: sd is 0.0;
    var float: x is 0.0;
  begin
    sd := sqrt(b**2 - 4.0 * a * c);
    if b < 0.0 then
      x := (-b + sd) / 2.0 * a;
      solution.x1 := x;
      solution.x2 := c / (a * x);
    else
      x := (-b - sd) / 2.0 * a;
      solution.x1 := c / (a * x);
      solution.x2 := x;
    end if;
  end func;

const proc: main is func
  local
    var roots: r is roots.value;
  begin
    r := solve(1.0, -10.0E5, 1.0);
    writeln("X1 = " <& r.x1 digits 6 <& " X2 = " <& r.x2 digits 6);
  end func;
```


```txt

X1 = 1000000.000000 X2 = 0.000001

```



## Sidef


```ruby
var sets = [
            [1,    2,  1],
            [1,    2,  3],
            [1,   -2,  1],
            [1,    0, -4],
            [1, -1e6,  1],
           ]

func quadroots(a, b, c) {
    var root = sqrt(b**2 - 4*a*c)

    [(-b + root) / (2 * a),
     (-b - root) / (2 * a)]
}

sets.each { |coefficients|
    say ("Roots for #{coefficients}",
        "=> (#{quadroots(coefficients...).join(', ')})")
}
```

```txt

Roots for [1, 2, 1]=> (-1, -1)
Roots for [1, 2, 3]=> (-1+1.41421356237309504880168872420969807856967187538i, -1-1.41421356237309504880168872420969807856967187538i)
Roots for [1, -2, 1]=> (1, 1)
Roots for [1, 0, -4]=> (2, -2)
Roots for [1, -1000000, 1]=> (999999.999998999999999998999999999997999999999995, 0.00000100000000000100000000000200000000000500000000002)

```



## Stata


```stata
mata
: polyroots((-2,0,1))
                 1             2
    +-----------------------------+
  1 |   1.41421356   -1.41421356  |
    +-----------------------------+

: polyroots((2,0,1))
                  1              2
    +-------------------------------+
  1 |  -1.41421356i    1.41421356i  |
    +-------------------------------+
```


## Tcl

```tcl
package require math::complexnumbers
namespace import math::complexnumbers::complex math::complexnumbers::tostring

proc quadratic {a b c} {
    set discrim [expr {$b**2 - 4*$a*$c}]
    set roots [list]
    if {$discrim < 0} {
        set term1 [expr {(-1.0*$b)/(2*$a)}]
        set term2 [expr {sqrt(abs($discrim))/(2*$a)}]
        lappend roots [tostring [complex $term1 $term2]] \
                [tostring [complex $term1 [expr {-1 * $term2}]]]
    } elseif {$discrim == 0} {
        lappend roots [expr {-1.0*$b / (2*$a)}]
    } else {
        lappend roots [expr {(-1*$b + sqrt($discrim)) / (2 * $a)}] \
                [expr {(-1*$b - sqrt($discrim)) / (2 * $a)}]
    }
    return $roots
}

proc report_quad {a b c} {
    puts [format "%sx**2 + %sx + %s = 0" $a $b $c]
    foreach root [quadratic $a $b $c] {
        puts "    x = $root"
    }
}

# examples on this page
report_quad 3 4 [expr {4/3.0}] ;# {-2/3}
report_quad 3 2 -1    ;# {1/3, -1}
report_quad 3 2  1    ;# {(-1/3 + sqrt(2/9)i), (-1/3 - sqrt(2/9)i)}
report_quad 1 0  1    ;# {(0+i), (0-i)}
report_quad 1 -1e6 1  ;# {1e6, 1e-6}
# examples from http://en.wikipedia.org/wiki/Quadratic_equation
report_quad -2  7 15  ;# {5, -3/2}
report_quad  1 -2  1  ;# {1}
report_quad  1  3  3  ;# {(-3 - sqrt(3)i)/2), (-3 + sqrt(3)i)/2)}
```

```txt
3x**2 + 4x + 1.3333333333333333 = 0
    x = -0.6666666666666666
3x**2 + 2x + -1 = 0
    x = 0.3333333333333333
    x = -1.0
3x**2 + 2x + 1 = 0
    x = -0.3333333333333333+0.47140452079103173i
    x = -0.3333333333333333-0.47140452079103173i
1x**2 + 0x + 1 = 0
    x = i
    x = -i
1x**2 + -1e6x + 1 = 0
    x = 999999.999999
    x = 1.00000761449337e-6
-2x**2 + 7x + 15 = 0
    x = -1.5
    x = 5.0
1x**2 + -2x + 1 = 0
    x = 1.0
1x**2 + 3x + 3 = 0
    x = -1.5+0.8660254037844386i
    x = -1.5-0.8660254037844386i
```


=={{header|TI-89 BASIC}}==

TI-89 BASIC has built-in numeric and algebraic solvers.
<lang>solve(x^2-1E9x+1.0)
```

returns

```txt
x=1.E-9 or x=1.E9
```



## zkl

zkl doesn't have a complex number package.
```zkl
fcn quadratic(a,b,c){ b=b.toFloat();
   println("Roots of a quadratic function %s, %s, %s".fmt(a,b,c));
   d,a2:=(b*b - 4*a*c), a+a;
   if(d>0){
      sd:=d.sqrt();
      println("  the real roots are %s and %s".fmt((-b + sd)/a2,(-b - sd)/a2));
   }
   else if(d==0) println("  the single root is ",-b/a2);
   else{
      sd:=(-d).sqrt();
      println("  the complex roots are %s and \U00B1;%si".fmt(-b/a2,sd/a2));
   }
}
```


```zkl
foreach a,b,c in (T( T(1,-2,1), T(1,-3,2), T(1,0,1), T(1,-1.0e10,1), T(1,2,3), T(2,-1,-6)) ){
   quadratic(a,b,c)
}
```

```txt

Roots of a quadratic function 1, -2, 1
  the single root is 1
Roots of a quadratic function 1, -3, 2
  the real roots are 2 and 1
Roots of a quadratic function 1, 0, 1
  the complex roots are 0 and ±1i
Roots of a quadratic function 1, -1e+10, 1
  the real roots are 1e+10 and 0
Roots of a quadratic function 1, 2, 3
  the complex roots are -1 and ±1.41421i
Roots of a quadratic function 2, -1, -6
  the real roots are 2 and -1.5

```


