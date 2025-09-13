+++
title = "Continued fraction"
description = ""
date = 2019-10-21T03:53:13Z
aliases = []
[extra]
id = 11445
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

{{task}}A number may be represented as a [[wp:Continued fraction|continued fraction]] (see [http://mathworld.wolfram.com/ContinuedFraction.html Mathworld] for more information) as follows:

:<math>a_0 + \cfrac{b_1}{a_1 + \cfrac{b_2}{a_2 + \cfrac{b_3}{a_3 + \ddots}}}</math>

The task is to write a program which generates such a number and prints a real representation of it. The code should be tested by calculating and printing the square root of 2, Napier's Constant, and Pi, using the following coefficients:

For the square root of 2, use <math>a_0 = 1</math> then <math>a_N = 2</math>. <math>b_N</math> is always <math>1</math>.

:<math>\sqrt{2} = 1 + \cfrac{1}{2 + \cfrac{1}{2 + \cfrac{1}{2 + \ddots}}}</math>

For Napier's Constant, use <math>a_0 = 2</math>, then <math>a_N = N</math>. <math>b_1 = 1</math> then <math>b_N = N-1</math>.

:<math>e = 2 + \cfrac{1}{1 + \cfrac{1}{2 + \cfrac{2}{3 + \cfrac{3}{4 + \ddots}}}}</math>

For Pi, use <math>a_0 = 3</math> then <math>a_N = 6</math>. <math>b_N = (2N-1)^2</math>.

:<math>\pi = 3 + \cfrac{1}{6 + \cfrac{9}{6 + \cfrac{25}{6 + \ddots}}}</math>

;See also<nowiki>:</nowiki>
* [[Continued fraction/Arithmetic]] for tasks that do arithmetic over continued fractions.



## 11l


```11l
F calc(f_a, f_b, =n = 1000)
   V r = 0.0
   L n > 0
      r = f_b(n) / (f_a(n) + r)
      n--
   R f_a(0) + r

print(calc(n -> I n > 0 {2} E 1, n -> 1))
print(calc(n -> I n > 0 {n} E 2, n -> I n > 1 {n - 1} E 1))
print(calc(n -> I n > 0 {6} E 3, n -> (2 * n - 1) ^ 2))
```



## Ada

(The source text for these examples can also be found on [https://bitbucket.org/ada_on_rosetta_code/solutions Bitbucket].)

Generic function for estimating continued fractions:

```Ada
generic
   type Scalar is digits <>;

   with function A (N : in Natural)  return Natural;
   with function B (N : in Positive) return Natural;
function Continued_Fraction (Steps : in Natural) return Scalar;
```



```Ada
function Continued_Fraction (Steps : in Natural) return Scalar is
   function A (N : in Natural)  return Scalar is (Scalar (Natural'(A (N))));
   function B (N : in Positive) return Scalar is (Scalar (Natural'(B (N))));

   Fraction : Scalar := 0.0;
begin
   for N in reverse Natural range 1 .. Steps loop
      Fraction := B (N) / (A (N) + Fraction);
   end loop;
   return A (0) + Fraction;
end Continued_Fraction;
```

Test program using the function above to estimate the square root of 2, Napiers constant and pi:

```Ada
with Ada.Text_IO;

with Continued_Fraction;

procedure Test_Continued_Fractions is
   type Scalar is digits 15;

   package Square_Root_Of_2 is
      function A (N : in Natural)  return Natural is (if N = 0 then 1 else 2);
      function B (N : in Positive) return Natural is (1);

      function Estimate is new Continued_Fraction (Scalar, A, B);
   end Square_Root_Of_2;

   package Napiers_Constant is
      function A (N : in Natural)  return Natural is (if N = 0 then 2 else N);
      function B (N : in Positive) return Natural is (if N = 1 then 1 else N-1);

      function Estimate is new Continued_Fraction (Scalar, A, B);
   end Napiers_Constant;

   package Pi is
      function A (N : in Natural)  return Natural is  (if N = 0 then 3 else 6);
      function B (N : in Positive) return Natural is ((2 * N - 1) ** 2);

      function Estimate is new Continued_Fraction (Scalar, A, B);
   end Pi;

   package Scalar_Text_IO is new Ada.Text_IO.Float_IO (Scalar);
   use Ada.Text_IO, Scalar_Text_IO;
begin
   Put (Square_Root_Of_2.Estimate (200), Exp => 0); New_Line;
   Put (Napiers_Constant.Estimate (200), Exp => 0); New_Line;
   Put (Pi.Estimate (10000),             Exp => 0); New_Line;
end Test_Continued_Fractions;
```


### Using only Ada 95 features

This example is exactly the same as the preceding one, but implemented using only Ada 95 features.

```Ada
generic
   type Scalar is digits <>;

   with function A (N : in Natural)  return Natural;
   with function B (N : in Positive) return Natural;
function Continued_Fraction_Ada95 (Steps : in Natural) return Scalar;
```



```Ada
function Continued_Fraction_Ada95 (Steps : in Natural) return Scalar is
   function A (N : in Natural)  return Scalar is
   begin
      return Scalar (Natural'(A (N)));
   end A;

   function B (N : in Positive) return Scalar is
   begin
      return Scalar (Natural'(B (N)));
   end B;

   Fraction : Scalar := 0.0;
begin
   for N in reverse Natural range 1 .. Steps loop
      Fraction := B (N) / (A (N) + Fraction);
   end loop;
   return A (0) + Fraction;
end Continued_Fraction_Ada95;
```


```Ada
with Ada.Text_IO;

with Continued_Fraction_Ada95;

procedure Test_Continued_Fractions_Ada95 is
   type Scalar is digits 15;

   package Square_Root_Of_2 is
      function A (N : in Natural)  return Natural;
      function B (N : in Positive) return Natural;

      function Estimate is new Continued_Fraction_Ada95 (Scalar, A, B);
   end Square_Root_Of_2;

   package body Square_Root_Of_2 is
      function A (N : in Natural) return Natural is
      begin
         if N = 0 then
            return 1;
         else
            return 2;
         end if;
      end A;

      function B (N : in Positive) return Natural is
      begin
         return 1;
      end B;
   end Square_Root_Of_2;

   package Napiers_Constant is
      function A (N : in Natural)  return Natural;
      function B (N : in Positive) return Natural;

      function Estimate is new Continued_Fraction_Ada95 (Scalar, A, B);
   end Napiers_Constant;

   package body Napiers_Constant is
      function A (N : in Natural) return Natural is
      begin
         if N = 0 then
            return 2;
         else
            return N;
         end if;
      end A;

      function B (N : in Positive) return Natural is
      begin
          if N = 1 then
             return 1;
          else
             return N - 1;
          end if;
      end B;
   end Napiers_Constant;

   package Pi is
      function A (N : in Natural)  return Natural;
      function B (N : in Positive) return Natural;

      function Estimate is new Continued_Fraction_Ada95 (Scalar, A, B);
   end Pi;

   package body Pi is
      function A (N : in Natural) return Natural is
      begin
         if N = 0 then
            return 3;
         else
            return 6;
         end if;
      end A;

      function B (N : in Positive) return Natural is
      begin
         return (2 * N - 1) ** 2;
      end B;
   end Pi;

   package Scalar_Text_IO is new Ada.Text_IO.Float_IO (Scalar);
   use Ada.Text_IO, Scalar_Text_IO;
begin
   Put (Square_Root_Of_2.Estimate (200), Exp => 0); New_Line;
   Put (Napiers_Constant.Estimate (200), Exp => 0); New_Line;
   Put (Pi.Estimate (10000),             Exp => 0); New_Line;
end Test_Continued_Fractions_Ada95;
```

```txt
 1.41421356237310
 2.71828182845905
 3.14159265358954
```



## ALGOL 68

```ALGOL68

PROC cf = (INT steps, PROC (INT) INT a, PROC (INT) INT b) REAL:
BEGIN
  REAL result;
  result := 0;
  FOR n FROM steps BY -1 TO 1 DO
      result := b(n) / (a(n) + result)
  OD;
  a(0) + result
END;

PROC asqr2 = (INT n) INT: (n = 0 | 1 | 2);
PROC bsqr2 = (INT n) INT: 1;

PROC anap = (INT n) INT: (n = 0 | 2 | n);
PROC bnap = (INT n) INT: (n = 1 | 1 | n - 1);

PROC api = (INT n) INT: (n = 0 | 3 | 6);
PROC bpi = (INT n) INT: (n = 1 | 1 | (2 * n - 1) ** 2);

INT precision = 10000;

print (("Precision: ", precision, newline));
print (("Sqr(2):    ", cf(precision, asqr2, bsqr2), newline));
print (("Napier:    ", cf(precision, anap, bnap), newline));
print (("Pi:        ", cf(precision, api, bpi)))

```

```txt

Precision:      +10000
Sqr(2):    +1.41421356237310e  +0
Napier:    +2.71828182845905e  +0
Pi:        +3.14159265358954e  +0

```



## ATS

A fairly direct translation of the [[#C|C version]] without using advanced features of the type system:

```ATS
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)
//
(*
** a coefficient function creates double values from in paramters
*)
typedef coeff_f = int -> double
//
(*
** a continued fraction is described by a record of two coefficent
** functions a and b
*)
typedef frac = @{a= coeff_f, b= coeff_f}
//
(* ****** ****** *)

fun calc
(
  f: frac, n: int
) : double = let
//
(*
** recursive definition of the approximation
*)
fun loop
(
  n: int, r: double
) : double =
(
if n = 0
  then f.a(0) + r
  else loop (n - 1, f.b(n) / (f.a(n) + r))
// end of [if]
)
//
in
  loop (n, 0.0)
end // end of [calc]

(* ****** ****** *)

val sqrt2 = @{
  a= lam (n: int): double => if n = 0 then 1.0 else 2.0
,
  b= lam (n: int): double => 1.0
} (* end of [val] *)

val napier = @{
  a= lam (n: int): double => if n = 0 then 2.0 else 1.0 * n
,
  b= lam (n: int): double => if n = 1 then 1.0 else n - 1.0
} (* end of [val] *)

val pi = @{
  a= lam (n: int): double => if n = 0 then 3.0 else 6.0
,
  b= lam (n: int): double => let val x = 2.0 * n - 1 in x * x end
}

(* ****** ****** *)

implement
main0 () =
(
  println! ("sqrt2  = ", calc(sqrt2,  100));
  println! ("napier = ", calc(napier, 100));
  println! ("  pi   = ", calc(  pi  , 100));
) (* end of [main0] *)
```



## AutoHotkey


```AutoHotkey
sqrt2_a(n) ; function definition is as simple as that
{
return n?2.0:1.0
}

sqrt2_b(n)
{
return 1.0
}

napier_a(n)
{
return n?n:2.0
}

napier_b(n)
{
return n>1.0?n-1.0:1.0
}

pi_a(n)
{
return n?6.0:3.0
}

pi_b(n)
{
return (2.0*n-1.0)**2.0 ; exponentiation operator
}

calc(f,expansions)
{
r:=0,i:=expansions
; A nasty trick: the names of the two coefficient functions are generated dynamically
; a dot surrounded by spaces means string concatenation
f_a:=f . "_a",f_b:=f . "_b"

while i>0 {
; You can see two dynamic function calls here
r:=%f_b%(i)/(%f_a%(i)+r)
i--
}

return %f_a%(0)+r
}

Msgbox, % "sqrt 2 = " . calc("sqrt2", 1000) . "`ne = " . calc("napier", 1000) . "`npi = " . calc("pi", 1000)
```

Output with Autohotkey v1 (currently 1.1.16.05):

```AutoHotkey
sqrt 2 = 1.414214
e = 2.718282
pi = 3.141593
```

Output with Autohotkey v2 (currently alpha 56):

```AutoHotkey
sqrt 2 = 1.4142135623730951
e = 2.7182818284590455
pi = 3.1415926533405418
```

Note the far superiour accuracy of v2.


## Axiom

Axiom provides a ContinuedFraction domain:

```Axiom
get(obj) == convergents(obj).1000 -- utility to extract the 1000th value
get continuedFraction(1, repeating [1], repeating [2]) :: Float
get continuedFraction(2, cons(1,[i for i in 1..]), [i for i in 1..]) :: Float
get continuedFraction(3, [i^2 for i in 1.. by 2], repeating [6]) :: Float
```

Output:
```Axiom
   (1)  1.4142135623 730950488
                                                                  Type: Float

   (2)  2.7182818284 590452354
                                                                  Type: Float

   (3)  3.1415926538 39792926
                                                                  Type: Float
```

The value for <math>\pi</math> has an accuracy to only 9 decimal places after 1000 iterations, with an accuracy to 12 decimal places after 10000 iterations.

We could re-implement this, with the same output:

```Axiom
cf(initial, a, b, n) ==
  n=1 => initial
  temp := 0
  for i in (n-1)..1 by -1 repeat
    temp := a.i/(b.i+temp)
  initial+temp
cf(1, repeating [1], repeating [2], 1000) :: Float
cf(2, cons(1,[i for i in 1..]), [i for i in 1..], 1000) :: Float
cf(3, [i^2 for i in 1.. by 2], repeating [6], 1000) :: Float
```



## BBC BASIC

```bbcbasic
      *FLOAT64
      @% = &1001010

      PRINT "SQR(2) = " ; FNcontfrac(1, 1, "2", "1")
      PRINT "     e = " ; FNcontfrac(2, 1, "N", "N")
      PRINT "    PI = " ; FNcontfrac(3, 1, "6", "(2*N+1)^2")
      END

      REM a$ and b$ are functions of N
      DEF FNcontfrac(a0, b1, a$, b$)
      LOCAL N, expr$
      REPEAT
        N += 1
        expr$ += STR$(EVAL(a$)) + "+" + STR$(EVAL(b$)) + "/("
      UNTIL LEN(expr$) > (65500 - N)
      = a0 + b1 / EVAL (expr$ + "1" + STRING$(N, ")"))
```

```txt

SQR(2) = 1.414213562373095
     e = 2.718281828459046
    PI = 3.141592653588017

```



## C

```c
/* calculate approximations for continued fractions */
#include <stdio.h>

/* kind of function that returns a series of coefficients */
typedef double (*coeff_func)(unsigned n);

/* calculates the specified number of expansions of the continued fraction
 * described by the coefficient series f_a and f_b */
double calc(coeff_func f_a, coeff_func f_b, unsigned expansions)
{
	double a, b, r;
	a = b = r = 0.0;

	unsigned i;
	for (i = expansions; i > 0; i--) {
		a = f_a(i);
		b = f_b(i);
		r = b / (a + r);
	}
	a = f_a(0);

	return a + r;
}

/* series for sqrt(2) */
double sqrt2_a(unsigned n)
{
	return n ? 2.0 : 1.0;
}

double sqrt2_b(unsigned n)
{
	return 1.0;
}

/* series for the napier constant */
double napier_a(unsigned n)
{
	return n ? n : 2.0;
}

double napier_b(unsigned n)
{
	return n > 1.0 ? n - 1.0 : 1.0;
}

/* series for pi */
double pi_a(unsigned n)
{
	return n ? 6.0 : 3.0;
}

double pi_b(unsigned n)
{
	double c = 2.0 * n - 1.0;

	return c * c;
}

int main(void)
{
	double sqrt2, napier, pi;

	sqrt2  = calc(sqrt2_a,  sqrt2_b,  1000);
	napier = calc(napier_a, napier_b, 1000);
	pi     = calc(pi_a,     pi_b,     1000);

	printf("%12.10g\n%12.10g\n%12.10g\n", sqrt2, napier, pi);

	return 0;
}
```

```txt
 1.414213562
 2.718281828
 3.141592653
```



## C++


```cpp
#include <iomanip>
#include <iostream>
#include <tuple>

typedef std::tuple<double,double> coeff_t; // coefficients type
typedef coeff_t (*func_t)(int); // callback function type

double calc(func_t func, int n)
{
    double a, b, temp = 0;
    for (; n > 0; --n) {
        std::tie(a, b) = func(n);
        temp = b / (a + temp);
    }
    std::tie(a, b) = func(0);
    return a + temp;
}

coeff_t sqrt2(int n)
{
    return coeff_t(n > 0 ? 2 : 1, 1);
}

coeff_t napier(int n)
{
    return coeff_t(n > 0 ? n : 2, n > 1 ? n - 1 : 1);
}

coeff_t pi(int n)
{
    return coeff_t(n > 0 ? 6 : 3, (2 * n - 1) * (2 * n - 1));
}

int main()
{
    std::streamsize old_prec = std::cout.precision(15); // set output digits
    std::cout
        << calc(sqrt2, 20) << '\n'
        << calc(napier, 15) << '\n'
        << calc(pi, 10000) << '\n'
        << std::setprecision(old_prec); // reset precision
}
```

```txt

1.41421356237309
2.71828182845905
3.14159265358954

```


## C#
```c#
using System;
using System.Collections.Generic;

namespace ContinuedFraction {
    class Program {
        static double Calc(Func<int, int[]> f, int n) {
            double temp = 0.0;
            for (int ni = n; ni >= 1; ni--) {
                int[] p = f(ni);
                temp = p[1] / (p[0] + temp);
            }
            return f(0)[0] + temp;
        }

        static void Main(string[] args) {
            List<Func<int, int[]>> fList = new List<Func<int, int[]>>();
            fList.Add(n => new int[] { n > 0 ? 2 : 1, 1 });
            fList.Add(n => new int[] { n > 0 ? n : 2, n > 1 ? (n - 1) : 1 });
            fList.Add(n => new int[] { n > 0 ? 6 : 3, (int) Math.Pow(2 * n - 1, 2) });

            foreach (var f in fList) {
                Console.WriteLine(Calc(f, 200));
            }
        }
    }
}
```

```txt
1.4142135623731
2.71828182845905
3.14159262280485
```



## Clojure


```clojure

(defn cfrac
  [a b n]
  (letfn [(cfrac-iter [[x k]] [(+ (a k) (/ (b (inc k)) x)) (dec k)])]
    (ffirst (take 1 (drop (inc n) (iterate cfrac-iter [1 n]))))))

(def sq2 (cfrac #(if (zero? %) 1.0 2.0) (constantly 1.0) 100))
(def e (cfrac #(if (zero? %) 2.0 %) #(if (= 1 %) 1.0 (double (dec %))) 100))
(def pi (cfrac #(if (zero? %) 3.0 6.0) #(let [x (- (* 2.0 %) 1.0)] (* x x)) 900000))

```

```txt

user=> sq2 e pi
1.4142135623730951
2.7182818284590455
3.141592653589793

```



## COBOL

```COBOL
       identification division.
       program-id. show-continued-fractions.

       environment division.
       configuration section.
       repository.
           function continued-fractions
           function all intrinsic.

       procedure division.
       fractions-main.

       display "Square root 2 approximately   : "
               continued-fractions("sqrt-2-alpha", "sqrt-2-beta", 100)
       display "Napier constant approximately : "
               continued-fractions("napier-alpha", "napier-beta", 40)
       display "Pi approximately              : "
               continued-fractions("pi-alpha", "pi-beta", 10000)

       goback.
       end program show-continued-fractions.

      *> **************************************************************
       identification division.
       function-id. continued-fractions.

       data division.
       working-storage section.
       01 alpha-function       usage program-pointer.
       01 beta-function        usage program-pointer.
       01 alpha                usage float-long.
       01 beta                 usage float-long.
       01 running              usage float-long.
       01 i                    usage binary-long.

       linkage section.
       01 alpha-name           pic x any length.
       01 beta-name            pic x any length.
       01 iterations           pic 9 any length.
       01 approximation        usage float-long.

       procedure division using
           alpha-name beta-name iterations
           returning approximation.

       set alpha-function to entry alpha-name
       if alpha-function = null then
           display "error: no " alpha-name " function" upon syserr
           goback
       end-if
       set beta-function to entry beta-name
       if beta-function = null then
           display "error: no " beta-name " function" upon syserr
           goback
       end-if

       move 0 to alpha beta running
       perform varying i from iterations by -1 until i = 0
           call alpha-function using i returning alpha
           call beta-function using i returning beta
           compute running = beta / (alpha + running)
       end-perform
       call alpha-function using 0 returning alpha
       compute approximation = alpha + running

       goback.
       end function continued-fractions.

      *> ******************************
       identification division.
       program-id. sqrt-2-alpha.

       data division.
       working-storage section.
       01 result               usage float-long.

       linkage section.
       01 iteration            usage binary-long unsigned.

       procedure division using iteration returning result.
       if iteration equal 0 then
           move 1.0 to result
       else
           move 2.0 to result
       end-if

       goback.
       end program sqrt-2-alpha.

      *> ******************************
       identification division.
       program-id. sqrt-2-beta.

       data division.
       working-storage section.
       01 result               usage float-long.

       linkage section.
       01 iteration            usage binary-long unsigned.

       procedure division using iteration returning result.
       move 1.0 to result

       goback.
       end program sqrt-2-beta.

      *> ******************************
       identification division.
       program-id. napier-alpha.

       data division.
       working-storage section.
       01 result               usage float-long.

       linkage section.
       01 iteration            usage binary-long unsigned.

       procedure division using iteration returning result.
       if iteration equal 0 then
           move 2.0 to result
       else
           move iteration to result
       end-if

       goback.
       end program napier-alpha.

      *> ******************************
       identification division.
       program-id. napier-beta.

       data division.
       working-storage section.
       01 result               usage float-long.

       linkage section.
       01 iteration            usage binary-long unsigned.

       procedure division using iteration returning result.
       if iteration = 1 then
           move 1.0 to result
       else
           compute result = iteration - 1.0
       end-if

       goback.
       end program napier-beta.

      *> ******************************
       identification division.
       program-id. pi-alpha.

       data division.
       working-storage section.
       01 result               usage float-long.

       linkage section.
       01 iteration            usage binary-long unsigned.

       procedure division using iteration returning result.
       if iteration equal 0 then
           move 3.0 to result
       else
           move 6.0 to result
       end-if

       goback.
       end program pi-alpha.

      *> ******************************
       identification division.
       program-id. pi-beta.

       data division.
       working-storage section.
       01 result               usage float-long.

       linkage section.
       01 iteration            usage binary-long unsigned.

       procedure division using iteration returning result.
       compute result = (2 * iteration - 1) ** 2

       goback.
       end program pi-beta.

```


```txt
prompt$ cobc -xj continued-fractions.cob
Square root 2 approximately   : 1.414213562373095
Napier constant approximately : 2.718281828459045
Pi approximately              : 3.141592653589543
```



## CoffeeScript


```coffeescript
# Compute a continuous fraction of the form
# a0 + b1 / (a1 + b2 / (a2 + b3 / ...
continuous_fraction = (f) ->
  a = f.a
  b = f.b
  c = 1
  for n in [100000..1]
    c = b(n) / (a(n) + c)
  a(0) + c

# A little helper.
p = (a, b) ->
  console.log a
  console.log b
  console.log "---"

do ->
  fsqrt2 =
    a: (n) -> if n is 0 then 1 else 2
    b: (n) -> 1
  p Math.sqrt(2), continuous_fraction(fsqrt2)

  fnapier =
    a: (n) -> if n is 0 then 2 else n
    b: (n) -> if n is 1 then 1 else n - 1
  p Math.E, continuous_fraction(fnapier)

  fpi =
    a: (n) ->
      return 3 if n is 0
      6
    b: (n) ->
      x = 2*n - 1
      x * x
  p Math.PI, continuous_fraction(fpi)
```

```txt

> coffee continued_fraction.coffee
1.4142135623730951
1.4142135623730951
---
2.718281828459045
2.7182818284590455
---
3.141592653589793
3.141592653589793
---

```



## Common Lisp

```lisp
(defun estimate-continued-fraction (generator n)
  (let ((temp 0))
    (loop for n1 from n downto 1
       do (multiple-value-bind (a b)
	      (funcall generator n1)
	    (setf temp (/ b (+ a temp)))))
    (+ (funcall generator 0) temp)))

(format t "sqrt(2) = ~a~%" (coerce (estimate-continued-fraction
				    (lambda (n)
				      (values (if (> n 0) 2 1) 1)) 20)
				   'double-float))
(format t "napier's = ~a~%" (coerce (estimate-continued-fraction
				     (lambda (n)
				       (values (if (> n 0) n 2)
					       (if (> n 1) (1- n) 1))) 15)
				    'double-float))

(format t "pi = ~a~%" (coerce (estimate-continued-fraction
			       (lambda (n)
				 (values (if (> n 0) 6 3)
					 (* (1- (* 2 n))
					    (1- (* 2 n))))) 10000)
			      'double-float))
```

```txt
sqrt(2) = 1.4142135623730947d0
napier's = 2.7182818284590464d0
pi = 3.141592653589543d0
```



## Chapel


Functions don't take other functions as arguments, so I wrapped them in a dummy record each.

```chapel
proc calc(f, n) {
        var r = 0.0;

        for k in 1..n by -1 {
                var v = f.pair(k);
                r = v(2) / (v(1) + r);
        }

        return f.pair(0)(1) + r;
}

record Sqrt2 {
        proc pair(n) {
                return (if n == 0 then 1 else 2,
                        1);
        }
}

record Napier {
        proc pair(n) {
                return (if n == 0 then 2 else n,
                        if n == 1 then 1 else n - 1);
        }
}
record Pi {
        proc pair(n) {
                return (if n == 0 then 3 else 6,
                        (2*n - 1)**2);
        }
}

config const n = 200;
writeln(calc(new Sqrt2(), n));
writeln(calc(new Napier(), n));
writeln(calc(new Pi(), n));
```



## D


```d
import std.stdio, std.functional, std.traits;

FP calc(FP, F)(in F fun, in int n) pure nothrow if (isCallable!F) {
    FP temp = 0;

    foreach_reverse (immutable ni; 1 .. n + 1) {
        immutable p = fun(ni);
        temp = p[1] / (FP(p[0]) + temp);
    }
    return fun(0)[0] + temp;
}

int[2] fSqrt2(in int n) pure nothrow {
    return [n > 0 ? 2 : 1,   1];
}

int[2] fNapier(in int n) pure nothrow {
    return [n > 0 ? n : 2,   n > 1 ? (n - 1) : 1];
}

int[2] fPi(in int n) pure nothrow {
    return [n > 0 ? 6 : 3,   (2 * n - 1) ^^ 2];
}

alias print = curry!(writefln, "%.19f");

void main() {
    calc!real(&fSqrt2, 200).print;
    calc!real(&fNapier, 200).print;
    calc!real(&fPi, 200).print;
}
```

```txt
1.4142135623730950487
2.7182818284590452354
3.1415926228048469486
```



## Erlang


```erlang

-module(continued).
-compile([export_all]).

pi_a (0) -> 3;
pi_a (_N) -> 6.

pi_b (N) ->
    (2*N-1)*(2*N-1).

sqrt2_a (0) ->
    1;
sqrt2_a (_N) ->
    2.

sqrt2_b (_N) ->
    1.

nappier_a (0) ->
    2;
nappier_a (N) ->
    N.

nappier_b (1) ->
    1;
nappier_b (N) ->
    N-1.

continued_fraction(FA,_FB,0) -> FA(0);
continued_fraction(FA,FB,N) ->
    continued_fraction(FA,FB,N-1,FB(N)/FA(N)).

continued_fraction(FA,_FB,0,Acc) -> FA(0) + Acc;
continued_fraction(FA,FB,N,Acc) ->
    continued_fraction(FA,FB,N-1,FB(N)/ (FA(N) + Acc)).

test_pi (N) ->
    continued_fraction(fun pi_a/1,fun pi_b/1,N).

test_sqrt2 (N) ->
    continued_fraction(fun sqrt2_a/1,fun sqrt2_b/1,N).

test_nappier (N) ->
    continued_fraction(fun nappier_a/1,fun nappier_b/1,N).

```


```erlang

29> continued:test_pi(1000).
3.141592653340542
30> continued:test_sqrt2(1000).
1.4142135623730951
31> continued:test_nappier(1000).
2.7182818284590455

```


=={{header|F_Sharp|F#}}==

### The Functions


```fsharp

// I provide four functions:-
// cf2S general purpose continued fraction to sequence of float approximations
// cN2S Normal continued fractions (a-series always 1)
// cfSqRt uses cf2S to calculate sqrt of float
// π takes a sequence of b values returning the next until the list is exhausted after which  it injects infinity
// Nigel Galloway: December 19th., 2018
let cf2S α β=let n0,g1,n1,g2=β(),α(),β(),β()
             seq{let (Π:decimal)=g1/n1 in yield n0+Π; yield! Seq.unfold(fun(n,g,Π)->let a,b=α(),β() in let Π=Π*g/n in Some(n0+Π,(b+a/n,b+a/g,Π)))(g2+α()/n1,g2,Π)}
let cN2S = cf2S (fun()->1M)
let cfSqRt n=(cf2S (fun()->n-1M) (let mutable n=false in fun()->if n then 2M else (n<-true; 1M)))
let π n=let mutable π=n in (fun ()->match π with h::t->π<-t; h |_->9999999999999999999999999999M)

```


### The Tasks


```fsharp

cfSqRt 2M |> Seq.take 10 |> Seq.pairwise |> Seq.iter(fun(n,g)->printfn "%1.14f < √2 < %1.14f" (min n g) (max n g))

```

```txt

1.40000000000000 < √2 < 1.50000000000000
1.40000000000000 < √2 < 1.41666666666667
1.41379310344828 < √2 < 1.41666666666667
1.41379310344828 < √2 < 1.41428571428571
1.41420118343195 < √2 < 1.41428571428571
1.41420118343195 < √2 < 1.41421568627451
1.41421319796954 < √2 < 1.41421568627451
1.41421319796954 < √2 < 1.41421362489487
1.41421355164605 < √2 < 1.41421362489487

```


```fsharp

cfSqRt 0.25M |> Seq.take 30 |> Seq.iter (printfn "%1.14f")

```

```txt

0.62500000000000
0.53846153846154
0.51250000000000
0.50413223140496
0.50137362637363
0.50045745654163
0.50015243902439
0.50005080784473
0.50001693537461
0.50000564506114
0.50000188167996
0.50000062722587
0.50000020907520
0.50000006969172
0.50000002323057
0.50000000774352
0.50000000258117
0.50000000086039
0.50000000028680
0.50000000009560
0.50000000003187
0.50000000001062
0.50000000000354
0.50000000000118
0.50000000000039
0.50000000000013
0.50000000000004
0.50000000000001
0.50000000000000
0.50000000000000

```


```fsharp

let aπ()=let mutable n=0M in (fun ()->n<-n+1M;let b=n+n-1M in b*b)
let bπ()=let mutable n=true in (fun ()->match n with true->n<-false;3M |_->6M)
cf2S (aπ()) (bπ()) |> Seq.take 10 |> Seq.pairwise |> Seq.iter(fun(n,g)->printfn "%1.14f < π < %1.14f" (min n g) (max n g))

```

```txt

3.13333333333333 < π < 3.16666666666667
3.13333333333333 < π < 3.14523809523810
3.13968253968254 < π < 3.14523809523810
3.13968253968254 < π < 3.14271284271284
3.14088134088134 < π < 3.14271284271284
3.14088134088134 < π < 3.14207181707182
3.14125482360776 < π < 3.14207181707182
3.14125482360776 < π < 3.14183961892940
3.14140671849650 < π < 3.14183961892940

```


```fsharp

let pi = π [3M;7M;15M;1M;292M;1M;1M;1M;2M;1M;3M;1M;14M;2M;1M;1M;2M;2M;2M;2M]
cN2S pi |> Seq.take 10 |> Seq.pairwise |> Seq.iter(fun(n,g)->printfn "%1.14f < π < %1.14f" (min n g) (max n g))

```

```txt

3.14150943396226 < π < 3.14285714285714
3.14150943396226 < π < 3.14159292035398
3.14159265301190 < π < 3.14159292035398
3.14159265301190 < π < 3.14159265392142
3.14159265346744 < π < 3.14159265392142
3.14159265346744 < π < 3.14159265361894
3.14159265358108 < π < 3.14159265361894
3.14159265358108 < π < 3.14159265359140
3.14159265358939 < π < 3.14159265359140

```


```fsharp

let ae()=let mutable n=0.5M in (fun ()->match n with 0.5M->n<-0M; 1M |_->n<-n+1M; n)
let be()=let mutable n=0.5M in (fun ()->match n with 0.5M->n<-0M; 2M |_->n<-n+1M; n)
cf2S (ae()) (be()) |> Seq.take 10 |> Seq.pairwise |> Seq.iter(fun(n,g)->printfn "%1.14f < e < %1.14f" (min n g) (max n g))

```

```txt

2.66666666666667 < e < 3.00000000000000
2.66666666666667 < e < 2.72727272727273
2.71698113207547 < e < 2.72727272727273
2.71698113207547 < e < 2.71844660194175
2.71826333176026 < e < 2.71844660194175
2.71826333176026 < e < 2.71828369389345
2.71828165766640 < e < 2.71828369389345
2.71828165766640 < e < 2.71828184277783
2.71828182735187 < e < 2.71828184277783

```



## Factor

''cfrac-estimate'' uses [[Arithmetic/Rational|rational arithmetic]] and never truncates the intermediate result. When ''terms'' is large, ''cfrac-estimate'' runs slow because numerator and denominator grow big.

```factor
USING: arrays combinators io kernel locals math math.functions
  math.ranges prettyprint sequences ;
IN: rosetta.cfrac

! Every continued fraction must implement these two words.
GENERIC: cfrac-a ( n cfrac -- a )
GENERIC: cfrac-b ( n cfrac -- b )

! square root of 2
SINGLETON: sqrt2
M: sqrt2 cfrac-a
    ! If n is 1, then a_n is 1, else a_n is 2.
    drop { { 1 [ 1 ] } [ drop 2 ] } case ;
M: sqrt2 cfrac-b
    ! Always b_n is 1.
    2drop 1 ;

! Napier's constant
SINGLETON: napier
M: napier cfrac-a
    ! If n is 1, then a_n is 2, else a_n is n - 1.
    drop { { 1 [ 2 ] } [ 1 - ] } case ;
M: napier cfrac-b
    ! If n is 1, then b_n is 1, else b_n is n - 1.
    drop { { 1 [ 1 ] } [ 1 - ] } case ;

SINGLETON: pi
M: pi cfrac-a
    ! If n is 1, then a_n is 3, else a_n is 6.
    drop { { 1 [ 3 ] } [ drop 6 ] } case ;
M: pi cfrac-b
    ! Always b_n is (n * 2 - 1)^2.
    drop 2 * 1 - 2 ^ ;

:: cfrac-estimate ( cfrac terms -- number )
    terms cfrac cfrac-a             ! top = last a_n
    terms 1 - 1 [a,b] [ :> n
        n cfrac cfrac-b swap /      ! top = b_n / top
        n cfrac cfrac-a +           ! top = top + a_n
    ] each ;

:: decimalize ( rational prec -- string )
    rational 1 /mod             ! split whole, fractional parts
    prec 10^ *                  ! multiply fraction by 10 ^ prec
    [ >integer unparse ] bi@    ! convert digits to strings
    :> fraction
    "."                         ! push decimal point
    prec fraction length -
    dup 0 < [ drop 0 ] when
    "0" <repetition> concat     ! push padding zeros
    fraction 4array concat ;

<PRIVATE
: main ( -- )
    " Square root of 2: " write
    sqrt2 50 cfrac-estimate 30 decimalize print
    "Napier's constant: " write
    napier 50 cfrac-estimate 30 decimalize print
    "               Pi: " write
    pi 950 cfrac-estimate 10 decimalize print ;
PRIVATE>

MAIN: main
```

```txt
 Square root of 2: 1.414213562373095048801688724209
Napier's constant: 2.718281828459045235360287471352
               Pi: 3.1415926538
```



## Felix


```felix
fun pi (n:int) : (double*double) =>
    let a = match n with | 0 => 3.0 | _ => 6.0 endmatch in
    let b = pow(2.0 * n.double - 1.0, 2.0) in
    (a,b);

fun sqrt_2 (n:int) : (double*double) =>
    let a = match n with | 0 => 1.0 | _ => 2.0 endmatch in
    let b = 1.0 in
    (a,b);

fun napier (n:int) : (double*double) =>
    let a = match n with | 0 => 2.0 | _ => n.double endmatch in
    let b = match n with | 1 => 1.0 | _ => (n.double - 1.0) endmatch in
    (a,b);

fun cf_iter (steps:int) (f:int -> double*double)  = {
    var acc = 0.0;
    for var n in steps downto 0 do
        var a, b = f(n);
        acc = if (n > 0) then (b / (a + acc)) else (acc + a);
    done
    return acc;
}

println$ cf_iter 200 sqrt_2; // => 1.41421
println$ cf_iter 200 napier; // => 2.71818
println$ cf_iter 1000 pi; // => 3.14159
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Continued_fraction this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

```forth>: fsqrt2 1 s>f 0> if 2 s
f else fdup then ;
: fnapier dup dup 1 > if 1- else drop 1 then s>f dup 1 < if drop 2 then s>f ;
: fpi dup 2* 1- dup * s>f 0> if 6 else 3 then s>f ;
                                       ( n -- f1 f2)
: cont.fraction                        ( xt n -- f)
  1 swap 1+ 0 s>f                      \ calculate for 1 .. n
  do i over execute frot f+ f/ -1 +loop
  0 swap execute fnip f+               \ calcucate for 0
;
```

```txt

' fsqrt2  200 cont.fraction f. cr 1.4142135623731
 ok
' fnapier 200 cont.fraction f. cr 2.71828182845905
 ok
' fpi     200 cont.fraction f. cr 3.14159268391981
 ok

```



## Fortran


```Fortran
module continued_fractions
  implicit none

  integer, parameter :: long = selected_real_kind(7,99)

  type continued_fraction
    integer                            :: a0, b1
    procedure(series), pointer, nopass :: a, b
  end type

  interface
    pure function series (n)
      integer, intent(in) :: n
      integer             :: series
    end function
  end interface

contains

  pure function define_cf (a0,a,b1,b) result(x)
    integer, intent(in)           :: a0
    procedure(series)             :: a
    integer, intent(in), optional :: b1
    procedure(series),   optional :: b
    type(continued_fraction)      :: x
    x%a0 = a0
    x%a => a
    if ( present(b1) ) then
       x%b1 = b1
    else
       x%b1 = 1
    end if
    if ( present(b) ) then
       x%b => b
    else
       x%b => const_1
    end if
  end function define_cf

  pure integer function const_1(n)
    integer,intent(in) :: n
    const_1 = 1
  end function

  pure real(kind=long) function output(x,iterations)
    type(continued_fraction), intent(in) :: x
    integer,                  intent(in) :: iterations
    integer                              :: i
    output = x%a(iterations)
    do i = iterations-1,1,-1
      output = x%a(i) + (x%b(i+1) / output)
    end do
    output = x%a0 + (x%b1 / output)
  end function output

end module continued_fractions


program examples
  use continued_fractions

  type(continued_fraction) :: sqr2,napier,pi

  sqr2   = define_cf(1,a_sqr2)
  napier = define_cf(2,a_napier,1,b_napier)
  pi     = define_cf(3,a=a_pi,b=b_pi)

  write (*,*) output(sqr2,10000)
  write (*,*) output(napier,10000)
  write (*,*) output(pi,10000)

contains

  pure integer function a_sqr2(n)
    integer,intent(in) :: n
    a_sqr2 = 2
  end function

  pure integer function a_napier(n)
    integer,intent(in) :: n
    a_napier = n
  end function

  pure integer function b_napier(n)
    integer,intent(in) :: n
    b_napier = n-1
  end function

  pure integer function a_pi(n)
    integer,intent(in) :: n
    a_pi = 6
  end function

  pure integer function b_pi(n)
    integer,intent(in) :: n
    b_pi = (2*n-1)*(2*n-1)
  end function

end program examples
```

```txt

   1.4142135623730951
   2.7182818284590455
   3.1415926535895435

```



## Go


```go
package main

import "fmt"

type cfTerm struct {
    a, b int
}

// follows subscript convention of mathworld and WP where there is no b(0).
// cf[0].b is unused in this representation.
type cf []cfTerm

func cfSqrt2(nTerms int) cf {
    f := make(cf, nTerms)
    for n := range f {
        f[n] = cfTerm{2, 1}
    }
    f[0].a = 1
    return f
}

func cfNap(nTerms int) cf {
    f := make(cf, nTerms)
    for n := range f {
        f[n] = cfTerm{n, n - 1}
    }
    f[0].a = 2
    f[1].b = 1
    return f
}

func cfPi(nTerms int) cf {
    f := make(cf, nTerms)
    for n := range f {
        g := 2*n - 1
        f[n] = cfTerm{6, g * g}
    }
    f[0].a = 3
    return f
}

func (f cf) real() (r float64) {
    for n := len(f) - 1; n > 0; n-- {
        r = float64(f[n].b) / (float64(f[n].a) + r)
    }
    return r + float64(f[0].a)
}

func main() {
    fmt.Println("sqrt2:", cfSqrt2(20).real())
    fmt.Println("nap:  ", cfNap(20).real())
    fmt.Println("pi:   ", cfPi(20).real())
}
```

```txt

sqrt2: 1.4142135623730965
nap:   2.7182818284590455
pi:    3.141623806667839

```



## Haskell


```haskell
import Data.List (unfoldr)
import Data.Char (intToDigit)

-- continued fraction represented as a (possibly infinite) list of pairs
sqrt2, napier, myPi :: [(Integer, Integer)]
sqrt2 = zip (1 : [2,2..]) [1,1..]
napier = zip (2 : [1..]) (1 : [1..])
myPi = zip (3 : [6,6..]) (map (^2) [1,3..])

-- approximate a continued fraction after certain number of iterations
approxCF :: (Integral a, Fractional b) => Int -> [(a, a)] -> b
approxCF t =
  foldr (\(a,b) z -> fromIntegral a + fromIntegral b / z) 1 . take t

-- infinite decimal representation of a real number
decString :: RealFrac a => a -> String
decString frac = show i ++ '.' : decString' f where
  (i,f) = properFraction frac
  decString' = map intToDigit . unfoldr (Just . properFraction . (10*))

main :: IO ()
main = mapM_ (putStrLn . take 200 . decString .
              (approxCF 950 :: [(Integer, Integer)] -> Rational))
             [sqrt2, napier, myPi]
```

```txt

1.414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641572735013846230912297024924836055850737212644121497099935831413222665927505592755799950501152782060571
2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260595630738132328627943490763233829880753195251019
3.141592653297590947683406834261190738869139611505752231394089152890909495973464508817163306557131591579057202097715021166512662872910519439747609829479577279606075707015622200744006783543589980682386

```


```haskell
import Data.Ratio

-- ignoring the task-given pi sequence: sucky convergence
-- pie = zip (3:repeat 6) (map (^2) [1,3..])

pie    = zip (0:[1,3..]) (4:map (^2) [1..])
sqrt2  = zip (1:repeat 2) (repeat 1)
napier = zip (2:[1..]) (1:[1..])

-- truncate after n terms
cf2rat n = foldr (\(a,b) f -> (a%1) + ((b%1) / f)) (1%1) . take n

-- truncate after error is at most 1/p
cf2rat_p p s = f $ map (\i -> (cf2rat i s, cf2rat (1+i) s)) $ map (2^) [0..]
	where f ((x,y):ys) = if abs (x-y) < (1/fromIntegral p) then x else f ys

-- returns a decimal string of n digits after the dot; all digits should
-- be correct (doesn't mean it's the best approximation! the decimal
-- string is simply truncated to given digits: pi=3.141 instead of 3.142)
cf2dec n = (ratstr n) . cf2rat_p (10^n) where
	ratstr l a = (show t) ++ '.':fracstr l n d where
		d = denominator a
		(t, n) = quotRem (numerator a) d
		fracstr 0 _ _ = []
		fracstr l n d = (show t)++ fracstr (l-1) n1 d where (t,n1) = quotRem (10 * n) d

main = do
	putStrLn $ cf2dec 200 sqrt2
	putStrLn $ cf2dec 200 napier
	putStrLn $ cf2dec 200 pie
```



## J


```J
   cfrac=: +`% / NB. Evaluate a list as a continued fraction

   sqrt2=: cfrac 1 1,200$2 1x
   pi=:cfrac 3, , ,&6"0 *:<:+:>:i.100x
   e=: cfrac 2 1, , ,~"0 >:i.100x

   NB. translate from fraction to decimal string
   NB. translated from factor
   dec =: (-@:[ (}.,'.',{.) ":@:<.@:(* 10x&^)~)"0

   100 10 100 dec sqrt2, pi, e
1.4142135623730950488016887242096980785696718753769480731766797379907324784621205551109457595775322165
3.1415924109
2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274
```


Note that there are two kinds of continued fractions. The kind here where we alternate between '''a''' and '''b''' values, but in some other tasks '''b''' is always 1 (and not included in the list we use to represent the continued fraction). The other kind is evaluated in J using <code>(+%)/</code> instead of <code>+`%/</code>.


## Java

```java
import static java.lang.Math.pow;
import java.util.*;
import java.util.function.Function;

public class Test {
    static double calc(Function<Integer, Integer[]> f, int n) {
        double temp = 0;

        for (int ni = n; ni >= 1; ni--) {
            Integer[] p = f.apply(ni);
            temp = p[1] / (double) (p[0] + temp);
        }
        return f.apply(0)[0] + temp;
    }

    public static void main(String[] args) {
        List<Function<Integer, Integer[]>> fList = new ArrayList<>();
        fList.add(n -> new Integer[]{n > 0 ? 2 : 1, 1});
        fList.add(n -> new Integer[]{n > 0 ? n : 2, n > 1 ? (n - 1) : 1});
        fList.add(n -> new Integer[]{n > 0 ? 6 : 3, (int) pow(2 * n - 1, 2)});

        for (Function<Integer, Integer[]> f : fList)
            System.out.println(calc(f, 200));
    }
}
```


```txt
1.4142135623730951
2.7182818284590455
3.141592622804847
```



## jq

We take one of the points of interest here to be the task of
representing the infinite series a0, a1, .... and b0, b1, .... compactly,
preferably functionally.  For the type of series typically encountered in continued fractions,
this is most readily accomplished in jq 1.4 using a filter (a function), here called "next", which, given the triple [i, [a[i], b[i]], will produce the next triple [i+1, a[i+1], b[i+1]].

Another point of interest is avoiding having to specify the number
of iterations.  The approach adopted here allows one to specify the
desired accuracy; in some cases, it is feasible to specify that the
computation should continue until the accuracy permitted by the
underlying floating point representation is achieved.  This is done
by specifying delta as 0, as shown in the examples.

We therefore proceed in two steps: continued_fraction( first; next; count ) computes an approximation
based on the first "count" terms; and then continued_fraction_delta(first; next; delta)
computes the continued fraction until the difference in approximations is less than or equal to delta,
which may be 0, as previously noted.

```jq

# "first" is the first triple,
# e.g. [1,a,b]; count specifies the number of terms to use.
def continued_fraction( first; next; count ):
  # input: [i, a, b]]
  def cf:
      if .[0] == count then 0
      else next as $ab
      | .[1] + (.[2] / ($ab | cf))
      end ;
  first | cf;

# "first" and "next" are as above;
# if delta is 0 then continue until there is no detectable change.
def continued_fraction_delta(first; next; delta):
  def abs: if . < 0 then -. else . end;
  def cf:
    # state: [n, prev]
    .[0] as $n | .[1] as $prev
    |  continued_fraction(first; next; $n+1) as $this
    | if $prev == null then [$n+1, $this] | cf
      elif delta <= 0 and ($prev == $this) then $this
      elif (($prev - $this)|abs) <= delta then $this
      else [$n+1, $this] | cf
      end;
  [2,null] | cf;

```

'''Examples''':

The convergence for pi is slow so we select delta = 1e-12 in that case.

```jq
"Value  :        Direct      : Continued Fraction",
 "2|sqrt : \(2|sqrt) : \(continued_fraction_delta( [1,1,1]; [.[0]+1, 2, 1]; 0))",
 "1|exp  : \(1|exp)  : \(2 + (1 / (continued_fraction_delta( [1,1,1]; [.[0]+1, .[1]+1, .[2]+1]; 0))))",
 "pi     : \(1|atan * 4)  : \(continued_fraction_delta( [1,3,1]; .[0]+1 | [., 6, ((2*. - 1) | (.*.))]; 1e-12)) (1e-12)"

```

```sh
$ jq -M -n -r -f Continued_fraction.jq
Value  :        Direct      : Continued Fraction
2|sqrt : 1.4142135623730951 : 1.4142135623730951
1|exp  : 2.718281828459045  : 2.7182818284590455
pi     : 3.141592653589793  : 3.1415926535892935 (1e-12)
```



## Julia

```julia
function _sqrt(a::Bool, n)
    if a
        return n > 0 ? 2.0 : 1.0
    else
        return 1.0
    end
end

function _napier(a::Bool, n)
    if a
        return n > 0 ? Float64(n) : 2.0
    else
        return n > 1 ? n - 1.0 : 1.0
    end
end

function _pi(a::Bool, n)
    if a
        return n > 0 ? 6.0 : 3.0
    else
        return (2.0 * n - 1.0) ^ 2.0 # exponentiation operator
    end
end

function calc(f::Function, expansions::Integer)
    a, b = true, false
    r = 0.0
    for i in expansions:-1:1
        r = f(b, i) / (f(a, i) + r)
    end
    return f(a, 0) + r
end

for (v, f) in (("√2", _sqrt), ("e", _napier), ("π", _pi))
    @printf("%3s = %f\n", v, calc(f, 1000))
end
```


```txt
 √2 = 1.414214
  e = 2.718282
  π = 3.141593
```



## Klong


```K

cf::{[f g i];f::x;g::y;i::z;
     f(0)+z{i::i-1;g(i+1)%f(i+1)+x}:*0}
cf({:[0=x;1;2]};{x;1};1000)
cf({:[0=x;2;x]};{:[x>1;x-1;x]};1000)
cf({:[0=x;3;6]};{((2*x)-1)^2};1000)

```

```txt

:triad
1.41421356237309504
2.71828182845904523
3.14159265334054205

```



## Kotlin

```scala
// version 1.1.2

typealias Func = (Int) -> IntArray

fun calc(f: Func, n: Int): Double {
    var temp = 0.0
    for (i in n downTo 1) {
        val p = f(i)
        temp = p[1] / (p[0] + temp)
    }
    return f(0)[0] + temp
}

fun main(args: Array<String>) {
    val pList = listOf<Pair<String, Func>>(
        "sqrt(2)" to { n -> intArrayOf(if (n > 0) 2 else 1, 1) },
        "e      " to { n -> intArrayOf(if (n > 0) n else 2, if (n > 1) n - 1 else 1) },
        "pi     " to { n -> intArrayOf(if (n > 0) 6 else 3, (2 * n - 1) * (2 * n - 1)) }
    )
    for (pair in pList) println("${pair.first} = ${calc(pair.second, 200)}")
}
```


```txt

sqrt(2) = 1.4142135623730951
e       = 2.7182818284590455
pi      = 3.141592622804847

```



## Maple


```Maple

contfrac:=n->evalf(Value(NumberTheory:-ContinuedFraction(n)));
contfrac(2^(0.5));
contfrac(Pi);
contfrac(exp(1));

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
sqrt2=Function[n,{1,Transpose@{Array[2&,n],Array[1&,n]}}];
napier=Function[n,{2,Transpose@{Range[n],Prepend[Range[n-1],1]}}];
pi=Function[n,{3,Transpose@{Array[6&,n],Array[(2#-1)^2&,n]}}];
approx=Function[l,
	N[Divide@@First@Fold[{{#2.#[[;;,1]],#2.#[[;;,2]]},#[[1]]}&,{{l[[2,1,1]]l[[1]]+l[[2,1,2]],l[[2,1,1]]},{l[[1]],1}},l[[2,2;;]]],10]];
r2=approx/@{sqrt2@#,napier@#,pi@#}&@10000;r2//TableForm
```

```txt

1.414213562
2.718281828
3.141592654

```



## Maxima


```maxima
cfeval(x) := block([a, b, n, z], a: x[1], b: x[2], n: length(a), z: 0,
   for i from n step -1 thru 2 do z: b[i]/(a[i] + z), a[1] + z)$

cf_sqrt2(n) := [cons(1, makelist(2, i, 2, n)), cons(0, makelist(1, i, 2, n))]$

cf_e(n) := [cons(2, makelist(i, i, 1, n - 1)), append([0, 1], makelist(i, i, 1, n - 2))]$

cf_pi(n) := [cons(3, makelist(6, i, 2, n)), cons(0, makelist((2*i - 1)^2, i, 1, n - 1))]$

cfeval(cf_sqrt2(20)), numer;   /* 1.414213562373097 */
% - sqrt(2), numer;            /* 1.3322676295501878*10^-15 */

cfeval(cf_e(20)), numer;       /* 2.718281828459046 */
% - %e, numer;                 /* 4.4408920985006262*10^-16 */

cfeval(cf_pi(20)), numer;      /* 3.141623806667839 */
% - %pi, numer;                /* 3.115307804568701*10^-5 */


/* convergence is much slower for pi */
fpprec: 20$
x: cfeval(cf_pi(10000))$
bfloat(x - %pi);               /* 2.4999999900104930006b-13 */
```



## NetRexx


```netrexx
/* REXX ***************************************************************
* Derived from REXX ... Derived from PL/I with a little "massage"
* SQRT2=  1.41421356237309505              <- PL/I Result
*         1.41421356237309504880168872421  <- NetRexx Result 30 digits
* NAPIER= 2.71828182845904524
*         2.71828182845904523536028747135
* PI=     3.14159262280484695
*         3.14159262280484694855146925223
* 07.09.2012 Walter Pachl
* 08.09.2012 Walter Pachl simplified (with the help of a friend)
**********************************************************************/
options replace format comments java crossref savelog symbols
  class CFB public

properties static
  Numeric Digits 30
  Sqrt2 =1
  napier=2
  pi    =3
  a     =0
  b     =0

method main(args = String[]) public static
  Say 'SQRT2='.left(7)  calc(sqrt2,  200)
  Say 'NAPIER='.left(7) calc(napier, 200)
  Say 'PI='.left(7)     calc(pi,     200)
  Return

method get_Coeffs(form,n) public static
  select
    when form=Sqrt2 Then do
      if n > 0 then a = 2; else a = 1
      b = 1
      end
    when form=Napier Then do
      if n > 0 then a = n; else a = 2
      if n > 1 then b = n - 1; else b = 1
      end
    when form=pi Then do
      if n > 0 then a = 6; else a = 3
      b = (2*n - 1)**2
      end
    end
  Return

method calc(form,n)  public static
  temp=0
  loop ni = n to 1 by -1
    Get_Coeffs(form,ni)
    temp = b/(a + temp)
    end
  Get_Coeffs(form,0)
  return (a + temp)
```

Who could help me make  a,b,sqrt2,napier,pi  global (public) variables?
This would simplify the solution:-)

I got this help and simplified the program.

However, I am told that 'my' value of pi is incorrect. I will investigate!

Apparently the coefficients given in the task description are only good for an approximation. One should, therefore, not SHOW more that 15 digits. See http://de.wikipedia.org/wiki/Kreiszahl

See [[#REXX|Rexx]] for a better computation


## Nim


```nim
proc calc(f: proc(n: int): tuple[a, b: float], n: int): float =
  var a, b, temp = 0.0
  for i in countdown(n, 1):
    (a, b) = f(i)
    temp = b / (a + temp)
  (a, b) = f(0)
  a + temp

proc sqrt2(n: int): tuple[a, b: float] =
  if n > 0:
    (2.0, 1.0)
  else:
    (1.0, 1.0)

proc napier(n: int): tuple[a, b: float] =
  let a = if n > 0: float(n) else: 2.0
  let b = if n > 1: float(n - 1) else: 1.0
  (a, b)

proc pi(n: int): tuple[a, b: float] =
  let a = if n > 0: 6.0 else: 3.0
  let b = (2 * float(n) - 1) * (2 * float(n) - 1)
  (a, b)

echo $calc(sqrt2, 20)
echo $calc(napier, 15)
echo $calc(pi, 10000)
```

```txt
1.414213562373095
2.718281828459046
3.141592653589544
```



## OCaml


```ocaml
let pi = 3, fun n -> ((2*n-1)*(2*n-1), 6)
and nap = 2, fun n -> (max 1 (n-1), n)
and root2 = 1, fun n -> (1, 2) in

let eval (i,f) k =
  let rec frac n =
    let a, b = f n in
    float a /. (float b +.
      if n >= k then 0.0 else frac (n+1)) in
  float i +. frac 1 in

Printf.printf "sqrt(2)\t= %.15f\n" (eval root2 1000);
Printf.printf "e\t= %.15f\n" (eval nap 1000);
Printf.printf "pi\t= %.15f\n" (eval pi 1000);
```

Output (inaccurate due to too few terms):

```txt
sqrt(2)	= 1.414213562373095
e	= 2.718281828459046
pi	= 3.141592653340542
```



## PARI/GP

Partial solution for simple continued fractions.

```parigp
back(v)=my(t=contfracpnqn(v));t[1,1]/t[2,1]*1.
back(vector(100,i,2-(i==1)))
```


Output:

```txt
%1 = 1.4142135623730950488016887242096980786
```



## Perl

We'll use closures to implement the infinite lists of coeffficients.


```perl
sub continued_fraction {
    my ($a, $b, $n) = (@_[0,1], $_[2] // 100);

    $a->() + ($n && $b->() / continued_fraction($a, $b, $n-1));
}

printf "√2  ≈ %.9f\n", continued_fraction do { my $n; sub { $n++ ? 2 : 1 } }, sub { 1 };
printf "e   ≈ %.9f\n", continued_fraction do { my $n; sub { $n++ || 2 } }, do { my $n; sub { $n++ || 1 } };
printf "π   ≈ %.9f\n", continued_fraction do { my $n; sub { $n++ ? 6 : 3 } }, do { my $n; sub { (2*$n++ + 1)**2 } }, 1_000;
printf "π/2 ≈ %.9f\n", continued_fraction do { my $n; sub { 1/($n++ || 1) } }, sub { 1 }, 1_000;
```

```txt
√2  ≈ 1.414213562
e   ≈ 2.718281828
π   ≈ 3.141592653
π/2 ≈ 1.570717797
```



## Perl 6

```perl6
sub continued-fraction(:@a, :@b, Int :$n = 100)
{
    my $x = @a[$n - 1];
    $x = @a[$_ - 1] + @b[$_] / $x for reverse 1 ..^ $n;
    $x;
}

printf "√2 ≈%.9f\n", continued-fraction(:a(1, |(2 xx *)), :b(Nil, |(1 xx *)));
printf "e  ≈%.9f\n", continued-fraction(:a(2, |(1 .. *)), :b(Nil, 1, |(1 .. *)));
printf "π  ≈%.9f\n", continued-fraction(:a(3, |(6 xx *)), :b(Nil, |((1, 3, 5 ... *) X** 2)));
```

```txt
√2 ≈ 1.414213562
e  ≈ 2.718281828
π  ≈ 3.141592654
```


A more original and a bit more abstract method would consist in viewing a continued fraction on rank n as a function of a variable x:
:<math>\mathrm{CF}_3(x) = a_0 + \cfrac{b_1}{a_1 + \cfrac{b_2}{a_2 + \cfrac{b_3}{a_3 + x}}}</math>
Or, more consistently:
:<math>\mathrm{CF}_3(x) = a_0 + \cfrac{b_0}{a_1 + \cfrac{b_1}{a_2 + \cfrac{b_2}{a_3 + \cfrac{b_3}{x}}}}</math>
Viewed as such, <math>\mathrm{CF}_n(x)</math> could be written recursively:
:<math>\mathrm{CF}_n(x) = \mathrm{CF}_{n-1}(a_n + \frac{b_n}{x})</math>
Or in other words:
:<math>\mathrm{CF}_n= \mathrm{CF}_{n-1}\circ f_n = \mathrm{CF}_{n-2}\circ f_{n-1}\circ f_n=\ldots=f_0\circ f_1 \ldots \circ f_n</math>
where <math>f_n(x) = a_n + \frac{b_n}{x}</math>

Perl6 has a builtin composition operator.  We can use it with the triangular reduction metaoperator, and evaluate each resulting function at infinity (any value would do actually, but infinite makes it consistent with this particular task).

```Perl 6
sub continued-fraction(@a, @b) {
    map { .(Inf) }, [\o] map { @a[$_] + @b[$_] / * }, ^Inf
}

printf "√2 ≈ %.9f\n", continued-fraction((1, |(2 xx *)), (1 xx *))[10];
printf "e  ≈ %.9f\n", continued-fraction((2, |(1 .. *)), (1, |(1 .. *)))[10];
printf "π  ≈ %.9f\n", continued-fraction((3, |(6 xx *)), ((1, 3, 5 ... *) X** 2))[100];
```

```txt
√2 ≈ 1.414213552
e  ≈ 2.718281827
π  ≈ 3.141592411
```



## Phix

```Phix
function continued_fraction(integer steps, integer rid_a, integer rid_b)
atom res = 0
  for n=steps to 1 by -1 do
     res := call_func(rid_b,{n}) / (call_func(rid_a,{n}) + res)
  end for
  return call_func(rid_a,{0}) + res
end function

function sqr2_a(integer n) return iff(n=0?1:2) end function
function sqr2_b(integer n) return 1 end function

function nap_a(integer n) return iff(n=0?2:n) end function
function nap_b(integer n) return iff(n=1?1:n-1) end function

function pi_a(integer n) return iff(n=0?3:6) end function
function pi_b(integer n) return iff(n=1?1:power(2*n-1,2)) end function

constant precision = 10000

printf(1,"Precision: %d\n", {precision})
printf(1,"Sqr(2):    %.10g\n", {continued_fraction(precision, routine_id("sqr2_a"), routine_id("sqr2_b"))})
printf(1,"Napier:    %.10g\n", {continued_fraction(precision, routine_id("nap_a"), routine_id("nap_b"))})
printf(1,"Pi:        %.10g\n", {continued_fraction(precision, routine_id("pi_a"), routine_id("pi_b"))})
```

```txt

Precision: 10000
Sqr(2):    1.414213562
Napier:    2.718281828
Pi:        3.141592654

```



## PicoLisp


```PicoLisp
(scl 49)
(de fsqrt2 (N A)
   (default A 1)
   (cond
      ((> A (inc N)) 2)
      (T
         (+
            (if (=1 A) 1.0 2.0)
            (*/ `(* 1.0 1.0) (fsqrt2 N (inc A))) ) ) ) )
(de pi (N A)
   (default A 1)
   (cond
      ((> A (inc N)) 6.0)
      (T
         (+
            (if (=1 A) 3.0 6.0)
            (*/
               (* (** (dec (* 2 A)) 2) 1.0)
               1.0
               (pi N (inc A)) ) ) ) ) )
(de napier (N A)
   (default A 0)
   (cond
      ((> A N) (* A 1.0))
      (T
         (+
            (if (=0 A) 2.0 (* A 1.0))
            (*/
               (if (> 1 A) 1.0 (* A 1.0))
               1.0
               (napier N (inc A)) ) ) ) ) )
(prinl (format (fsqrt2 200) *Scl))
(prinl (format (napier 200) *Scl))
(prinl (format (pi 200) *Scl))
```

```txt

1.4142135623730950488016887242096980785696718753770
2.7182818284590452353602874713526624977572470937000
3.1415926839198062649342019294083175420335002640134

```



## PL/I


```PLI
/* Version for SQRT(2) */
test: proc options (main);
   declare n fixed;

denom: procedure (n) recursive returns (float (18));
   declare n fixed;
   n = n + 1;
   if n > 100 then return (2);
   return (2 + 1/denom(n));
end denom;

   put (1 + 1/denom(2));

end test;
```

```txt
 1.41421356237309505E+0000
```

Version for NAPIER:

```PLI
test: proc options (main);
   declare n fixed;

denom: procedure (n) recursive returns (float (18));
   declare n fixed;
   n = n + 1;
   if n > 100 then return (n);
   return (n + n/denom(n));
end denom;

   put (2 + 1/denom(0));

end test;
```


```txt
 2.71828182845904524E+0000
```

Version for SQRT2, NAPIER, PI

```PLI
/* Derived from continued fraction in Wiki Ada program */

continued_fractions:                         /* 6 Sept. 2012 */
   procedure options (main);
   declare (Sqrt2 initial (1), napier initial (2), pi initial (3)) fixed (1);

Get_Coeffs: procedure (form, n, coefA, coefB);
      declare form fixed (1), n fixed, (coefA, coefB) float (18);

      select (form);
         when (Sqrt2) do;
               if n > 0 then coefA = 2; else coefA = 1;
               coefB = 1;
            end;
         when (Napier) do;
               if n > 0 then coefA = n; else coefA = 2;
               if n > 1 then coefB = n - 1; else coefB = 1;
            end;
         when (Pi) do;
               if n > 0 then coefA = 6; else coefA = 3;
               coefB = (2*n - 1)**2;
            end;
      end;
   end Get_Coeffs;

   Calc: procedure (form, n) returns (float (18));
      declare form fixed (1), n fixed;
      declare (A, B) float (18);
      declare Temp float (18) initial (0);
      declare ni fixed;

      do ni = n to 1 by -1;
         call Get_Coeffs (form, ni, A, B);
         Temp = B/(A + Temp);
      end;
      call Get_Coeffs (form, 0, A, B);
      return (A + Temp);
   end Calc;

   put      edit ('SQRT2=',  calc(sqrt2,  200)) (a(10), f(20,17));
   put skip edit ('NAPIER=', calc(napier, 200)) (a(10), f(20,17));
   put skip edit ('PI=',     calc(pi,   99999)) (a(10), f(20,17));

end continued_fractions;
```

```txt

SQRT2=     1.41421356237309505
NAPIER=    2.71828182845904524
PI=        3.14159265358979349

```



## Prolog


```Prolog
continued_fraction :-
	% square root 2
	continued_fraction(200, sqrt_2_ab, V1),
	format('sqrt(2) = ~w~n', [V1]),

	% napier
	continued_fraction(200, napier_ab, V2),
	format('e       = ~w~n', [V2]),

	% pi
	continued_fraction(200, pi_ab, V3),
	format('pi      = ~w~n', [V3]).


% code for continued fractions
continued_fraction(N, Compute_ab, V) :-
	continued_fraction(N,  Compute_ab, 0, V).

continued_fraction(0,  Compute_ab, Temp, V) :-
	call(Compute_ab, 0, A, _),
	V is A + Temp.

continued_fraction(N, Compute_ab, Tmp, V) :-
	call(Compute_ab, N, A, B),
	Tmp1 is B / (A + Tmp),
	N1 is N - 1,
	continued_fraction(N1, Compute_ab, Tmp1, V).

% specific codes for examples
% definitions for square root of 2
sqrt_2_ab(0, 1, 1).
sqrt_2_ab(_, 2, 1).

% definitions for napier
napier_ab(0, 2, _).
napier_ab(1, 1, 1).
napier_ab(N, N, V) :-
	V is N - 1.

% definitions for pi
pi_ab(0, 3, _).
pi_ab(N, 6, V) :-
	V is (2 * N - 1)*(2 * N - 1).
```

```txt
 ?- continued_fraction.
sqrt(2) = 1.4142135623730951
e       = 2.7182818284590455
pi      = 3.141592622804847
true .

```



## Python

```python
from fractions import Fraction
import itertools
try: zip = itertools.izip
except: pass

# The Continued Fraction
def CF(a, b, t):
  terms = list(itertools.islice(zip(a, b), t))
  z = Fraction(1,1)
  for a, b in reversed(terms):
    z = a + b / z
  return z

# Approximates a fraction to a string
def pRes(x, d):
  q, x = divmod(x, 1)
  res = str(q)
  res += "."
  for i in range(d):
    x *= 10
    q, x = divmod(x, 1)
    res += str(q)
  return res

# Test the Continued Fraction for sqrt2
def sqrt2_a():
  yield 1
  for x in itertools.repeat(2):
    yield x

def sqrt2_b():
  for x in itertools.repeat(1):
    yield x

cf = CF(sqrt2_a(), sqrt2_b(), 950)
print(pRes(cf, 200))
#1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721264412149709993583141322266592750559275579995050115278206057147


# Test the Continued Fraction for Napier's Constant
def Napier_a():
  yield 2
  for x in itertools.count(1):
    yield x

def Napier_b():
  yield 1
  for x in itertools.count(1):
    yield x

cf = CF(Napier_a(), Napier_b(), 950)
print(pRes(cf, 200))
#2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901

# Test the Continued Fraction for Pi
def Pi_a():
  yield 3
  for x in itertools.repeat(6):
    yield x

def Pi_b():
  for x in itertools.count(1,2):
    yield x*x

cf = CF(Pi_a(), Pi_b(), 950)
print(pRes(cf, 10))
#3.1415926532
```


### Fast iterative version

```python
from decimal import Decimal, getcontext

def calc(fun, n):
    temp = Decimal("0.0")

    for ni in xrange(n+1, 0, -1):
        (a, b) = fun(ni)
        temp = Decimal(b) / (a + temp)

    return fun(0)[0] + temp

def fsqrt2(n):
    return (2 if n > 0 else 1, 1)

def fnapier(n):
    return (n if n > 0 else 2, (n - 1) if n > 1 else 1)

def fpi(n):
    return (6 if n > 0 else 3, (2 * n - 1) ** 2)

getcontext().prec = 50
print calc(fsqrt2, 200)
print calc(fnapier, 200)
print calc(fpi, 200)
```

```txt
1.4142135623730950488016887242096980785696718753770
2.7182818284590452353602874713526624977572470937000
3.1415926839198062649342019294083175420335002640134
```



## Racket


### Using Doubles

This version uses standard double precision floating point numbers:

```racket

#lang racket
(define (calc cf n)
  (match/values (cf 0)
    [(a0 b0)
     (+ a0
        (for/fold ([t 0.0]) ([i (in-range (+ n 1) 0 -1)])
          (match/values (cf i)
                        [(a b) (/ b (+ a t))])))]))

(define (cf-sqrt i)   (values  (if (> i 0) 2 1)  1))
(define (cf-napier i) (values  (if (> i 0) i 2)  (if (> i 1) (- i 1) 1)))
(define (cf-pi i)     (values  (if (> i 0) 6 3)  (sqr (- (* 2 i) 1))))

(calc cf-sqrt   200)
(calc cf-napier 200)
(calc cf-pi     200)

```

Output:

```racket

1.4142135623730951
2.7182818284590455
3.1415926839198063

```


===Version - Using Doubles===
This versions uses big floats (arbitrary precision floating point):

```racket

#lang racket
(require math)
(bf-precision 2048) ; in bits

(define (calc cf n)
  (match/values (cf 0)
    [(a0 b0)
     (bf+ (bf a0)
        (for/fold ([t (bf 0)]) ([i (in-range (+ n 1) 0 -1)])
          (match/values (cf i)
                        [(a b) (bf/ (bf b) (bf+ (bf a) t))])))]))

(define (cf-sqrt i)   (values  (if (> i 0) 2 1)  1))
(define (cf-napier i) (values  (if (> i 0) i 2)  (if (> i 1) (- i 1) 1)))
(define (cf-pi i)     (values  (if (> i 0) 6 3)  (sqr (- (* 2 i) 1))))

(calc cf-sqrt   200)
(calc cf-napier 200)
(calc cf-pi     200)

```

Output:

```racket

(bf #e1.4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727350138462309122970249248360558507372126441214970999358960036439214262599769155193770031712304888324413327207659690547583107739957489062466508437105234564161085482146113860092820802430986649987683947729823677905101453725898480737256099166805538057375451207262441039818826744940289448489312217214883459060818483750848688583833366310472320771259749181255428309841375829513581694269249380272698662595131575038315461736928338289219865139248048189188905788104310928762952913687232022557677738108337499350045588767581063729)
(bf #e2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901157383418793070215408914993488416750924476146066808226480016847741185374234544243710753907774499206955170276183860626133138458300075204493382656029760673711320070932870912744374704723624212700454495421842219077173525899689811474120614457405772696521446961165559468253835854362096088934714907384964847142748311021268578658461064714894910680584249490719358138073078291397044213736982988247857479512745588762993966446075)
(bf #e3.14159268391980626493420192940831754203350026401337226640663040854412059241988978103217808449508253393479795573626200366332733859609651462659489470805432281782785922056335606047700127154963266242144951481397480765182268219697420028007903565511884267297358842935537138583640066772149177226656227031792115896439889412205871076985598822285367358003457939603015797225018209619662200081521930463480571130673429337524564941105654923909951299948539893933654293161126559643573974163405197696633200469475250152247413175932572922175467223988860975105100904322239324381097207835036465269418118204894206705789759765527734394105147)

```



## REXX


### version 1

The    '''cf'''   subroutine    (for '''C'''ontinued '''F'''ractions)   isn't limited to positive integers.

Any form of REXX numbers (negative, exponentiated, decimal fractions) can be used.

Note the use of negative fractions for the   <big>'''ß'''</big>   terms when computing <big>'''<b>√</b>{{overline|   ½  }}'''</big>.

There isn't any practical limit for the decimal digits that can be used, although 100k digits would be a bit unwieldy to display.

A generalized   <big>'''<b>√</b>{{overline|  }}'''</big>   function was added to calculate a few low integers   (and also   <big><sup>'''1'''</sup></big>/<big><sub>'''2'''</sub></big>).

<!--
In addition,   <big><sup>'''1'''</sup></big>/<big><sub>'''2'''</sub> π</big>   was calculated (as described in the ''talk'' page under ''Gold Credit'').
-->

More code is used for nicely formatting the output than the continued fraction calculation.

```rexx
/*REXX program  calculates and displays  values of  various  continued fractions.       */
parse arg terms digs .
if terms=='' | terms==","  then terms=500
if  digs=='' |  digs==","  then  digs=100
numeric digits digs                              /*use  100  decimal digits for display.*/
b.=1                                             /*omitted ß terms are assumed to be  1.*/
/*══════════════════════════════════════════════════════════════════════════════════════*/
a.=2;                                                           call tell '√2',      cf(1)
/*══════════════════════════════════════════════════════════════════════════════════════*/
a.=1;  do N=2  by  2  to terms; a.N=2; end;                     call tell '√3',      cf(1)     /*also:  2∙sin(π/3) */
/*══════════════════════════════════════════════════════════════════════════════════════*/
a.=2                  /*              ___ */
      do N=2  to 17   /*generalized  √ N  */
      b.=N-1;                          NN=right(N, 2);          call tell 'gen √'NN, cf(1)
      end   /*N*/
/*══════════════════════════════════════════════════════════════════════════════════════*/
a.=2;   b.=-1/2;                                                call tell 'gen √ ½', cf(1)
/*══════════════════════════════════════════════════════════════════════════════════════*/
  do j=1 for terms; a.j=j;  if j>1   then b.j=a.p; p=j; end;    call tell 'e',       cf(2)
/*══════════════════════════════════════════════════════════════════════════════════════*/
a.=1;                                                           call tell 'φ, phi',  cf(1)
/*══════════════════════════════════════════════════════════════════════════════════════*/
a.=1;    do j=1 for terms;  if j//2  then a.j=j;        end;    call tell 'tan(1)',  cf(1)
/*══════════════════════════════════════════════════════════════════════════════════════*/
         do j=1 for terms;                a.j=2*j+1;    end;    call tell 'coth(1)', cf(1)
/*══════════════════════════════════════════════════════════════════════════════════════*/
         do j=1 for terms;                a.j=4*j+2;    end;    call tell 'coth(½)', cf(2)    /*also:  [e+1]÷[e-1] */
/*══════════════════════════════════════════════════════════════════════════════════════*/
                     terms=100000
a.=6;    do j=1  for terms;  b.j=(2*j-1)**2;            end;    call tell 'π, pi',   cf(3)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cf:      procedure expose a. b. terms;  parse arg C;     !=0;    numeric digits 9+digits()
                                          do k=terms  by -1  for terms;  d=a.k+!;  !=b.k/d
                                          end   /*k*/
         return !+C
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:    parse arg ?,v;   $=left(format(v)/1,1+digits());    w=50    /*50 bytes of terms*/
         aT=;     do k=1;  _=space(aT a.k);  if length(_)>w  then leave;  aT=_;  end /*k*/
         bT=;     do k=1;  _=space(bT b.k);  if length(_)>w  then leave;  bT=_;  end /*k*/
                          say right(?,8)   "="    $     '  α terms='aT  ...
         if b.1\==1  then say right("",12+digits())     '  ß terms='bT  ...
         a=;   b.=1;  return       /*only 50 bytes of  α & ß terms  ↑   are displayed.  */
```

'''output'''

```txt

      √2 = 1.414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641573   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
      √3 = 1.732050807568877293527446341505872366942805253810380628055806979451933016908800037081146186757248576   α terms=1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 ...
 gen √ 2 = 1.414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641573   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
 gen √ 3 = 1.732050807568877293527446341505872366942805253810380628055806979451933016908800037081146186757248576   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
 gen √ 4 = 2                                                                                                       α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 ...
 gen √ 5 = 2.236067977499789696409173668731276235440618359611525724270897245410520925637804899414414408378782275   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...
 gen √ 6 = 2.449489742783178098197284074705891391965947480656670128432692567250960377457315026539859433104640235   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 ...
 gen √ 7 = 2.645751311064590590501615753639260425710259183082450180368334459201068823230283627760392886474543611   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 ...
 gen √ 8 = 2.828427124746190097603377448419396157139343750753896146353359475981464956924214077700775068655283145   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 ...
 gen √ 9 = 3                                                                                                       α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 ...
 gen √10 = 3.162277660168379331998893544432718533719555139325216826857504852792594438639238221344248108379300295   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 ...
 gen √11 = 3.316624790355399849114932736670686683927088545589353597058682146116484642609043846708843399128290651   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 ...
 gen √12 = 3.464101615137754587054892683011744733885610507620761256111613958903866033817600074162292373514497151   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 ...
 gen √13 = 3.605551275463989293119221267470495946251296573845246212710453056227166948293010445204619082018490718   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 ...
 gen √14 = 3.741657386773941385583748732316549301756019807778726946303745467320035156306939027976809895194379572   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 ...
 gen √15 = 3.872983346207416885179265399782399610832921705291590826587573766113483091936979033519287376858673518   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 ...
 gen √16 = 4                                                                                                       α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 ...
 gen √17 = 4.123105625617660549821409855974077025147199225373620434398633573094954346337621593587863650810684297   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 ...
 gen √ ½ = 0.707106781186547524400844362104849039284835937688474036588339868995366239231053519425193767163820786   α terms=2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
                                                                                                                   ß terms=-0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 ...
       e = 2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427   α terms=1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...
  φ, phi = 1.618033988749894848204586834365638117720309179805762862135448622705260462818902449707207204189391137   α terms=1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ...
  tan(1) = 1.557407724654902230506974807458360173087250772381520038383946605698861397151727289555099965202242984   α terms=1 1 3 1 5 1 7 1 9 1 11 1 13 1 15 1 17 1 19 1 21 1 ...
 coth(1) = 1.313035285499331303636161246930847832912013941240452655543152967567084270461874382674679241480856303   α terms=3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 ...
 coth(½) = 2.163953413738652848770004010218023117093738602150792272533574119296087634783339486574409418809750115   α terms=6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 ...
   π, pi = 3.141592653589792988470143264530440384041017830472772036746332303472711537960073664096818977224037083   α terms=6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 ...

```

Note:   even with 200 digit accuracy and 100,000 terms, the last calculation of pi is only accurate to 15 digits.

===version 2 derived from [[#PL/I|PL/I]]===

```rexx
/* REXX **************************************************************
* Derived from PL/I with a little "massage"
* SQRT2=     1.41421356237309505              <- PL/I Result
*            1.41421356237309504880168872421  <- REXX Result 30 digits
* NAPIER=    2.71828182845904524
*            2.71828182845904523536028747135
* PI=        3.14159262280484695
*            3.14159262280484694855146925223
* 06.09.2012 Walter Pachl
**********************************************************************/
  Numeric Digits 30
  Parse Value '1 2 3 0 0' with Sqrt2 napier pi a b
  Say left('SQRT2=' ,10) calc(sqrt2,  200)
  Say left('NAPIER=',10) calc(napier, 200)
  Say left('PI='    ,10) calc(pi,     200)
  Exit

Get_Coeffs: procedure Expose a b Sqrt2 napier pi
  Parse Arg form, n
  select
    when form=Sqrt2 Then do
      if n > 0 then a = 2; else a = 1
      b = 1
      end
    when form=Napier Then do
      if n > 0 then a = n; else a = 2
      if n > 1 then b = n - 1; else b = 1
      end
    when form=pi Then do
      if n > 0 then a = 6; else a = 3
      b = (2*n - 1)**2
      end
    end
  Return

Calc: procedure Expose a b Sqrt2 napier pi
  Parse Arg form,n
  Temp=0
  do ni = n to 1 by -1
    Call Get_Coeffs form, ni
    Temp = B/(A + Temp)
    end
  call Get_Coeffs  form, 0
  return (A + Temp)
```



### version 3 better approximation


```rexx
/* REXX *************************************************************
* The task description specifies a continued fraction for pi
* that gives a reasonable approximation.
* Literature shows a better CF that yields pi with a precision of
* 200 digits.
* http://de.wikipedia.org/wiki/Kreiszahl
*                    1
* pi = 3 + ------------------------
*                      1
*          7 + --------------------
*                         1
*              15 + ---------------
*                            1
*                   1 + -----------
*
*                       292 + ...
*
* This program uses that CF and shows the first 50 digits
* PI =3.1415926535897932384626433832795028841971693993751...
* PIX=3.1415926535897932384626433832795028841971693993751...
* 201 correct digits
* 18.09.2012 Walter Pachl
**********************************************************************/
  pi='3.1415926535897932384626433832795028841971'||,
     '693993751058209749445923078164062862089986280348'||,
     '253421170679821480865132823066470938446095505822'||,
     '317253594081284811174502841027019385211055596446'||,
     '229489549303819644288109756659334461284756482337'||,
     '867831652712019091456485669234603486104543266482'||,
     '133936072602491412737245870066063155881748815209'||,
     '209628292540917153643678925903600113305305488204'||,
     '665213841469519415116094330572703657595919530921'||,
     '861173819326117931051185480744623799627495673518'||,
     '857527248912279381830119491298336733624'
  Numeric Digits 1000
  al='7 15 1 292 1 1 1 2 1 3 1 14 2 1 1 2 2 2 2 1 84 2',
     '1 1 15 3 13 1 4 2 6 6 99 1 2 2 6 3 5 1 1 6 8 1 7 1 2',
     '3 7 1 2 1 1 12 1 1 1 3 1 1 8 1 1 2 1 6 1 1 5 2 2 3 1',
     '2 4 4 16 1 161 45 1 22 1 2 2 1 4 1 2 24 1 2 1 3 1 2',
     '1 1 10 2 5 4 1 2 2 8 1 5 2 2 26 1 4 1 1 8 2 42 2 1 7',
     '3 3 1 1 7 2 4 9 7 2 3 1 57 1 18 1 9 19 1 2 18 1 3 7',
     '30 1 1 1 3 3 3 1 2 8 1 1 2 1 15 1 2 13 1 2 1 4 1 12',
     '1 1 3 3 28 1 10 3 2 20 1 1 1 1 4 1 1 1 5 3 2 1 6 1 4'
  a.=3
  Do i=1 By 1 while al<>''
    Parse Var al a.i al
    End
  pix=calc(194)
  Do e=1 To length(pi)
    If substr(pix,e,1)<>substr(pi,e,1) Then Leave
    End
  Numeric Digits 50
  Say 'PI ='||(pi+0)||'...'
  Say 'PIX='||(pix+0)||'...'
  Say (e-1) 'correct digits'
  Exit

Get_Coeffs: procedure Expose a b a.
  Parse Arg n
  a=a.n
  b=1
  Return

Calc: procedure Expose a b a.
  Parse Arg n
  Temp=0
  do ni = n to 1 by -1
    Call Get_Coeffs ni
    Temp = B/(A + Temp)
    end
  call Get_Coeffs 0
  return (A + Temp)
```



## Ring


```ring

# Project : Continued fraction

see "SQR(2) = " + contfrac(1, 1, "2", "1") + nl
see "        e = " + contfrac(2, 1, "n", "n") + nl
see "       PI = " + contfrac(3, 1, "6", "(2*n+1)^2") + nl

func contfrac(a0, b1, a, b)
        expr = ""
        n = 0
        while len(expr) < (700 - n)
                 n = n + 1
                 eval("temp1=" + a)
                 eval("temp2=" + b)
                 expr = expr + string(temp1) + char(43) + string(temp2) + "/("
        end
        str = copy(")",n)
        eval("temp3=" + expr + "1" + str)
        return a0 + b1 / temp3

```

Output:

```txt

SQR(2) = 1.414213562373095
         e = 2.718281828459046
        PI = 3.141592653588017

```



## Ruby


```ruby
require 'bigdecimal'

# square root of 2
sqrt2 = Object.new
def sqrt2.a(n); n == 1 ? 1 : 2; end
def sqrt2.b(n); 1; end

# Napier's constant
napier = Object.new
def napier.a(n); n == 1 ? 2 : n - 1; end
def napier.b(n); n == 1 ? 1 : n - 1; end

pi = Object.new
def pi.a(n); n == 1 ? 3 : 6; end
def pi.b(n); (2*n - 1)**2; end

# Estimates the value of a continued fraction _cfrac_, to _prec_
# decimal digits of precision. Returns a BigDecimal. _cfrac_ must
# respond to _cfrac.a(n)_ and _cfrac.b(n)_ for integer _n_ >= 1.
def estimate(cfrac, prec)
  last_result = nil
  terms = prec

  loop do
    # Estimate continued fraction for _n_ from 1 to _terms_.
    result = cfrac.a(terms)
    (terms - 1).downto(1) do |n|
      a = BigDecimal cfrac.a(n)
      b = BigDecimal cfrac.b(n)
      digits = [b.div(result, 1).exponent + prec, 1].max
      result = a + b.div(result, digits)
    end
    result = result.round(prec)

    if result == last_result
      return result
    else
      # Double _terms_ and try again.
      last_result = result
      terms *= 2
    end
  end
end

puts estimate(sqrt2, 50).to_s('F')
puts estimate(napier, 50).to_s('F')
puts estimate(pi, 10).to_s('F')
```

```txt
$ ruby cfrac.rb
1.41421356237309504880168872420969807856967187537695
2.71828182845904523536028747135266249775724709369996
3.1415926536
```



## Rust


```rust

use std::iter;

// Calculating a continued fraction is quite easy with iterators, however
// writing a proper iterator adapter is less so. We settle for a macro which
// for most purposes works well enough.
//
// One limitation with this iterator based approach is that we cannot reverse
// input iterators since they are not usually DoubleEnded. To circumvent this
// we can collect the elements and then reverse them, however this isn't ideal
// as we now have to store elements equal to the number of iterations.
//
// Another is that iterators cannot be resused once consumed, so it is often
// required to make many clones of iterators.
macro_rules! continued_fraction {
    ($a:expr, $b:expr ; $iterations:expr) => (
        ($a).zip($b)
            .take($iterations)
            .collect::<Vec<_>>().iter()
            .rev()
            .fold(0 as f64, |acc: f64, &(x, y)| {
                x as f64 + (y as f64 / acc)
            })
    );

    ($a:expr, $b:expr) => (continued_fraction!($a, $b ; 1000));
}

fn main() {
    // Sqrt(2)
    let sqrt2a = (1..2).chain(iter::repeat(2));
    let sqrt2b = iter::repeat(1);
    println!("{}", continued_fraction!(sqrt2a, sqrt2b));


    // Napier's Constant
    let napiera = (2..3).chain(1..);
    let napierb = (1..2).chain(1..);
    println!("{}", continued_fraction!(napiera, napierb));


    // Pi
    let pia = (3..4).chain(iter::repeat(6));
    let pib = (1i64..).map(|x| (2 * x - 1).pow(2));
    println!("{}", continued_fraction!(pia, pib));
}

```


```txt

1.4142135623730951
2.7182818284590455
3.141592653339042

```



## Scala

Note that Scala-BigDecimal provides a precision of 34 digits. Therefore we take a limitation of 32 digits to avoiding rounding problems.

```Scala
object CF extends App {
  import Stream._
  val sqrt2 = 1 #:: from(2,0) zip from(1,0)
  val napier = 2 #:: from(1) zip (1 #:: from(1))
  val pi = 3 #:: from(6,0) zip (from(1,2) map {x=>x*x})

  // reference values, source: wikipedia
  val refPi     = "3.14159265358979323846264338327950288419716939937510"
  val refNapier = "2.71828182845904523536028747135266249775724709369995"
  val refSQRT2  = "1.41421356237309504880168872420969807856967187537694"

  def calc(cf: Stream[(Int, Int)], numberOfIters: Int=200): BigDecimal = {
    (cf take numberOfIters toList).foldRight[BigDecimal](1)((a, z) => a._1+a._2/z)
  }

  def approx(cfV: BigDecimal, cfRefV: String): String = {
    val p: Pair[Char,Char] => Boolean = pair =>(pair._1==pair._2)
    ((cfV.toString+" "*34).substring(0,34) zip cfRefV.toString.substring(0,34))
      .takeWhile(p).foldRight[String]("")((a:Pair[Char,Char],z)=>a._1+z)
  }

  List(("sqrt2",sqrt2,50,refSQRT2),("napier",napier,50,refNapier),("pi",pi,3000,refPi)) foreach {t=>
    val (name,cf,iters,refV) = t
    val cfV = calc(cf,iters)
    println(name+":")
    println("ref value: "+refV.substring(0,34))
    println("cf value:  "+(cfV.toString+" "*34).substring(0,34))
    println("precision: "+approx(cfV,refV))
    println()
  }
}
```

```txt
sqrt2:
ref value: 1.41421356237309504880168872420969
cf value:  1.41421356237309504880168872420969
precision: 1.41421356237309504880168872420969

napier:
ref value: 2.71828182845904523536028747135266
cf value:  2.71828182845904523536028747135266
precision: 2.71828182845904523536028747135266

pi:
ref value: 3.14159265358979323846264338327950
cf value:  3.14159265358052780404906362935452
precision: 3.14159265358
```

For higher accuracy of pi we have to take more iterations. Unfortunately the foldRight function in calc isn't tail recursiv - therefore a stack overflow exception will be thrown for higher numbers of iteration, thus we have to implement an iterative way for calculation:

```Scala
object CFI extends App {
  import Stream._
  val sqrt2 = 1 #:: from(2,0) zip from(1,0)
  val napier = 2 #:: from(1) zip (1 #:: from(1))
  val pi = 3 #:: from(6,0) zip (from(1,2) map {x=>x*x})

  // reference values, source: wikipedia
  val refPi     = "3.14159265358979323846264338327950288419716939937510"
  val refNapier = "2.71828182845904523536028747135266249775724709369995"
  val refSQRT2  = "1.41421356237309504880168872420969807856967187537694"

  def calc_i(cf: Stream[(Int, Int)], numberOfIters: Int=50): BigDecimal = {
    val cfl = cf take numberOfIters toList
    var z: BigDecimal = 1.0
    for (i <- 0 to cfl.size-1 reverse)
      z=cfl(i)._1+cfl(i)._2/z
    z
  }

  def approx(cfV: BigDecimal, cfRefV: String): String = {
    val p: Pair[Char,Char] => Boolean = pair =>(pair._1==pair._2)
    ((cfV.toString+" "*34).substring(0,34) zip cfRefV.toString.substring(0,34))
      .takeWhile(p).foldRight[String]("")((a:Pair[Char,Char],z)=>a._1+z)
  }

  List(("sqrt2",sqrt2,50,refSQRT2),("napier",napier,50,refNapier),("pi",pi,50000,refPi)) foreach {t=>
    val (name,cf,iters,refV) = t
    val cfV = calc_i(cf,iters)
    println(name+":")
    println("ref value: "+refV.substring(0,34))
    println("cf value:  "+(cfV.toString+" "*34).substring(0,34))
    println("precision: "+approx(cfV,refV))
    println()
  }
}
```

```txt
sqrt2:
ref value: 1.41421356237309504880168872420969
cf value:  1.41421356237309504880168872420969
precision: 1.41421356237309504880168872420969

napier:
ref value: 2.71828182845904523536028747135266
cf value:  2.71828182845904523536028747135266
precision: 2.71828182845904523536028747135266

pi:
ref value: 3.14159265358979323846264338327950
cf value:  3.14159265358983426214354599901745
precision: 3.141592653589
```



## Scheme

The following code relies on a library implementing SRFI 41 (lazy streams). Most Scheme interpreters include an implementation.


```scheme
#!r6rs
(import (rnrs base (6))
        (srfi :41 streams))

(define nats (stream-cons 0 (stream-map (lambda (x) (+ x 1)) nats)))

(define (build-stream fn) (stream-map fn nats))

(define (stream-cycle s . S)
  (cond
    ((stream-null? (car S)) stream-null)
    (else (stream-cons (stream-car s)
                       (apply stream-cycle (append S (list (stream-cdr s))))))))

(define (cf-floor cf) (stream-car cf))
(define (cf-num cf) (stream-car (stream-cdr cf)))
(define (cf-denom cf) (stream-cdr (stream-cdr cf)))

(define (cf-integer? x) (stream-null? (stream-cdr x)))

(define (cf->real x)
  (let refine ((x x) (n 65536))
    (cond
      ((= n 0) +inf.0)
      ((cf-integer? x) (cf-floor x))
      (else (+ (cf-floor x)
               (/ (cf-num x)
                  (refine (cf-denom x) (- n 1))))))))

(define (real->cf x)
  (let-values (((integer-part fractional-part) (div-and-mod x 1)))
    (if (= fractional-part 0.0)
        (stream (exact integer-part))
        (stream-cons
         (exact integer-part)
         (stream-cons
          1
          (real->cf (/ fractional-part)))))))


(define sqrt2 (stream-cons 1 (stream-constant 1 2)))

(define napier
  (stream-append (stream 2 1)
                 (stream-cycle (stream-cdr nats) (stream-cdr nats))))

(define pi
  (stream-cons 3
               (stream-cycle (build-stream (lambda (n) (expt (- (* 2 (+ n 1)) 1) 2)))
                             (stream-constant 6))))
```


Test:

```scheme>
 (cf->real sqrt2)
1.4142135623730951
> (cf->real napier)
2.7182818284590455
> (cf->real pi)
3.141592653589794
```




## Sidef


```ruby
func continued_fraction(a, b, f, n = 1000, r = 1) {
    f(func (r) {
        r < n ? (a(r) / (b(r) + __FUNC__(r+1))) : 0
    }(r))
}

var params = Hash(
    "φ"  => [ { 1 }, { 1 }, { 1 + _ } ],
    "√2" => [ { 1 }, { 2 }, { 1 + _ } ],
    "e"  => [ { _ }, { _ }, { 1 + 1/_ } ],
    "π"  => [ { (2*_ - 1)**2 }, { 6 }, { 3 + _ } ],
    "τ"  => [ { _**2 }, { 2*_ + 1 }, { 8 / (1 + _) } ],
)

for k in (params.keys.sort) {
    printf("%2s ≈ %s\n", k, continued_fraction(params{k}...))
}
```

```txt

 e ≈ 2.7182818284590452353602874713526624977572470937
 π ≈ 3.14159265383979292596359650286939597045138933078
 τ ≈ 6.28318530717958647692528676655900576839433879875
 φ ≈ 1.61803398874989484820458683436563811772030917981
√2 ≈ 1.41421356237309504880168872420969807856967187538

```



## Tcl

Note that Tcl does not provide arbitrary precision floating point numbers by default, so all result computations are done with IEEE <code>double</code>s.

```tcl
package require Tcl 8.6

# Term generators; yield list of pairs
proc r2 {} {
    yield {1 1}
    while 1 {yield {2 1}}
}
proc e {} {
    yield {2 1}
    while 1 {yield [list [incr n] $n]}
}
proc pi {} {
    set n 0; set a 3
    while 1 {
	yield [list $a [expr {(2*[incr n]-1)**2}]]
	set a 6
    }
}

# Continued fraction calculator
proc cf {generator {termCount 50}} {
    # Get the chunk of terms we want to work with
    set terms [list [coroutine cf.c $generator]]
    while {[llength $terms] < $termCount} {
	lappend terms [cf.c]
    }
    rename cf.c {}

    # Merge the terms to compute the result
    set val 0.0
    foreach pair [lreverse $terms] {
	lassign $pair a b
	set val [expr {$a + $b/$val}]
    }
    return $val
}

# Demonstration
puts [cf r2]
puts [cf e]
puts [cf pi 250]; # Converges more slowly
```

```txt
1.4142135623730951
2.7182818284590455
3.1415926373965735
```



## VBA

```vb
Public Const precision = 10000
Private Function continued_fraction(steps As Integer, rid_a As String, rid_b As String) As Double
    Dim res As Double
    res = 0
    For n = steps To 1 Step -1
       res = Application.Run(rid_b, n) / (Application.Run(rid_a, n) + res)
    Next n
    continued_fraction = Application.Run(rid_a, 0) + res
End Function

Function sqr2_a(n As Integer) As Integer
    sqr2_a = IIf(n = 0, 1, 2)
End Function

Function sqr2_b(n As Integer) As Integer
    sqr2_b = 1
End Function

Function nap_a(n As Integer) As Integer
    nap_a = IIf(n = 0, 2, n)
End Function

Function nap_b(n As Integer) As Integer
    nap_b = IIf(n = 1, 1, n - 1)
End Function

Function pi_a(n As Integer) As Integer
    pi_a = IIf(n = 0, 3, 6)
End Function

Function pi_b(n As Integer) As Long
    pi_b = IIf(n = 1, 1, (2 * n - 1) ^ 2)
End Function

Public Sub main()
    Debug.Print "Precision:", precision
    Debug.Print "Sqr(2):", continued_fraction(precision, "sqr2_a", "sqr2_b")
    Debug.Print "Napier:", continued_fraction(precision, "nap_a", "nap_b")
    Debug.Print "Pi:", continued_fraction(precision, "pi_a", "pi_b")
End Sub
```
```txt
Precision:     10000
Sqr(2):        1,4142135623731
Napier:        2,71828182845905
Pi:            3,14159265358954
```


## Visual Basic .NET

```vbnet
Module Module1
    Function Calc(f As Func(Of Integer, Integer()), n As Integer) As Double
        Dim temp = 0.0
        For ni = n To 1 Step -1
            Dim p = f(ni)
            temp = p(1) / (p(0) + temp)
        Next
        Return f(0)(0) + temp
    End Function

    Sub Main()
        Dim fList = {
            Function(n As Integer) New Integer() {If(n > 0, 2, 1), 1},
            Function(n As Integer) New Integer() {If(n > 0, n, 2), If(n > 1, n - 1, 1)},
            Function(n As Integer) New Integer() {If(n > 0, 6, 3), Math.Pow(2 * n - 1, 2)}
            }

        For Each f In fList
            Console.WriteLine(Calc(f, 200))
        Next
    End Sub

End Module
```

```txt
1.4142135623731
2.71828182845905
3.14159262280485
```



## XPL0

The number of iterations (N) needed to get the 13 digits of accuracy was determined by experiment.

```XPL0
include c:\cxpl\codes;
int     N;
real    A, B, F;
[Format(1, 15);
A:= 2.0;  B:= 1.0;  N:= 16;
IntOut(0, N); CrLf(0);
F:= 0.0;
while N>=1 do [F:= B/(A+F);  N:= N-1];
RlOut(0, 1.0+F);  CrLf(0);
RlOut(0, sqrt(2.0));  CrLf(0);

N:= 13;
IntOut(0, N); CrLf(0);
F:= 0.0;
while N>=2 do [F:= float(N-1)/(float(N)+F);  N:= N-1];
RlOut(0, 2.0 + 1.0/(1.0+F));  CrLf(0);
RlOut(0, Exp(1.0));  CrLf(0);

N:= 10000;
IntOut(0, N); CrLf(0);
F:= 0.0;
while N>=1 do [F:= float(sq(2*N-1))/(6.0+F);  N:= N-1];
RlOut(0, 3.0+F);  CrLf(0);
RlOut(0, ACos(-1.0));  CrLf(0);
]
```

```txt

16
1.414213562372820
1.414213562373100
13
2.718281828459380
2.718281828459050
10000
3.141592653589540
3.141592653589790

```



## zkl


```zkl
fcn cf(fa,fb,a0){fcn(fa,fb,a0,n){
   a0 + [n..1,-1].reduce(
        'wrap(p,n){ fb(n)/(fa(n)+p) },0.0) }.fp(fa,fb,a0)
}
```

cf creates a function that calculates the continued fraction from the bottom up. The new function takes a single parameter, n, which is used to calculate the nth term.

```zkl
sqrt2:=cf((2.0).noop,(1.0).noop,1.0);
   sqrt2(200) : "%.20e".fmt(_).println();
nap:=cf((0.0).create,fcn(n){ (n==1) and 1.0 or (n-1).toFloat() },2.0);
   println(nap(15) - (1.0).e);
pi:=cf((6.0).noop,fcn(n){ n=2*n-1; (n*n).toFloat() },3.0);
   println(pi(1000) - (1.0).pi);
```

(1.0).create(n) --> n, (1.0).noop(n) --> 1.0
```txt

1.41421356237309514547e+00
1.33227e-15
-2.49251e-10

```



## ZX Spectrum Basic

```zxbasic
10 LET a0=1: LET b1=1: LET a$="2": LET b$="1": PRINT "SQR(2) = ";: GO SUB 1000
20 LET a0=2: LET b1=1: LET a$="N": LET b$="N": PRINT "e = ";: GO SUB 1000
30 LET a0=3: LET b1=1: LET a$="6": LET b$="(2*N+1)^2": PRINT "PI = ";: GO SUB 1000
100 STOP
1000 LET n=0: LET e$="": LET p$=""
1010 LET n=n+1
1020 LET e$=e$+STR$ VAL a$+"+"+STR$ VAL b$+"/("
1030 IF LEN e$<(4000-n) THEN GO TO 1010
1035 FOR i=1 TO n: LET p$=p$+")": NEXT i
1040 PRINT a0+b1/VAL (e$+"1"+p$)
1050 RETURN
```

