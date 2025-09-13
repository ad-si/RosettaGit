+++
title = "Thiele's interpolation formula"
description = ""
date = 2019-10-21T14:57:34Z
aliases = []
[extra]
id = 8375
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
+++

## Task

'''[[wp:Thiele's_interpolation_formula|Thiele's interpolation formula]]''' is an interpolation formula for a function ''f''(•) of a single variable.   It is expressed as a [[continued fraction]]:

:: <big><big><math> f(x) = f(x_1) + \cfrac{x-x_1}{\rho_1(x_1,x_2) + \cfrac{x-x_2}{\rho_2(x_1,x_2,x_3) - f(x_1) + \cfrac{x-x_3}{\rho_3(x_1,x_2,x_3,x_4) - \rho_1(x_1,x_2) + \cdots}}} </math></big></big>

<big><big><math>\rho</math></big></big>   represents the   [[wp:reciprocal difference|reciprocal difference]],   demonstrated here for reference:

:: <big><big><math>\rho_1(x_0, x_1) = \frac{x_0 - x_1}{f(x_0) - f(x_1)}</math></big></big>

:: <big><big><math>\rho_2(x_0, x_1, x_2) = \frac{x_0 - x_2}{\rho_1(x_0, x_1) - \rho_1(x_1, x_2)} + f(x_1)</math></big></big>

:: <big><big><math>\rho_n(x_0,x_1,\ldots,x_n)=\frac{x_0-x_n}{\rho_{n-1}(x_0,x_1,\ldots,x_{n-1})-\rho_{n-1}(x_1,x_2,\ldots,x_n)}+\rho_{n-2}(x_1,\ldots,x_{n-1})</math></big></big>

Demonstrate Thiele's interpolation function by:
# Building a   '''32'''   row ''trig table'' of values   for   <big><big><math> x </math></big></big>   from   '''0'''   by   '''0.05'''   to   '''1.55'''   of the trig functions:
#*   '''sin'''
#*   '''cos'''
#*   '''tan'''
# Using columns from this table define an inverse - using Thiele's interpolation - for each trig function;
# Finally: demonstrate the following well known trigonometric identities:
#*   <big><big> 6 &times; sin<sup>-1</sup> &frac12; = <math>\pi</math></big></big>
#*   <big><big> 3 &times; cos<sup>-1</sup> &frac12; = <math>\pi</math></big></big>
#*   <big><big> 4 &times; tan<sup>-1</sup> 1        = <math>\pi</math></big></big>





## Ada

thiele.ads:

```Ada
with Ada.Numerics.Generic_Real_Arrays;

generic
   type Real is digits <>;
package Thiele is
   package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Real);
   subtype Real_Array is Real_Arrays.Real_Vector;

   type Thiele_Interpolation (Length : Natural) is private;

   function Create (X, Y : Real_Array) return Thiele_Interpolation;
   function Inverse (T : Thiele_Interpolation; X : Real) return Real;
private
   type Thiele_Interpolation (Length : Natural) is record
      X, Y, RhoX : Real_Array (1 .. Length);
   end record;
end Thiele;
```


thiele.adb:

```Ada
package body Thiele is
   use type Real_Array;

   function "/" (Left, Right : Real_Array) return Real_Array is
      Result : Real_Array (Left'Range);
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with "arrays not same size";
      end if;
      for I in Result'Range loop
         Result (I) := Left (I) / Right (I);
      end loop;
      return Result;
   end "/";

   function Rho (X, Y : Real_Array) return Real_Array is
      N      : constant Natural                      := X'Length;
      P      : array (1 .. N) of Real_Array (1 .. N) :=
        (others => (others => 9.9));
      Result : Real_Array (1 .. N);
   begin
      P (1) (1 .. N)      := Y (1 .. N);
      P (2) (1 .. N - 1)  := (X (1 .. N - 1) - X (2 .. N)) /
        (P (1) (1 .. N - 1) - P (1) (2 .. N));
      for I in 3 .. N loop
         P (I) (1 .. N - I + 1)  := P (I - 2) (2 .. N - I + 2) +
           (X (1 .. N - I + 1) - X (I .. N)) /
           (P (I - 1) (1 .. N - I + 1) - P (I - 1) (2 .. N - I + 2));
      end loop;
      for I in X'Range loop
         Result (I) := P (I) (1);
      end loop;
      return Result;
   end Rho;

   function Create (X, Y : Real_Array) return Thiele_Interpolation is
   begin
      if X'Length < 3 then
         raise Constraint_Error with "at least 3 values";
      end if;
      if X'Length /= Y'Length then
         raise Constraint_Error with "input arrays not same size";
      end if;
      return (Length => X'Length, X => X, Y => Y, RhoX => Rho (X, Y));
   end Create;

   function Inverse (T : Thiele_Interpolation; X : Real) return Real is
      A : Real := 0.0;
   begin
      for I in reverse 3 .. T.Length loop
         A := (X - T.X (I - 1)) / (T.RhoX (I) - T.RhoX (I - 2) + A);
      end loop;
      return T.Y (1) + (X - T.X (1)) / (T.RhoX (2) + A);
   end Inverse;

end Thiele;
```


example:

```Ada
with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Thiele;

procedure Main is
   package Math is new Ada.Numerics.Generic_Elementary_Functions
     (Long_Float);
   package Float_Thiele is new Thiele (Long_Float);
   use Float_Thiele;

   Row_Count : Natural := 32;

   X_Values   : Real_Array (1 .. Row_Count);
   Sin_Values : Real_Array (1 .. Row_Count);
   Cos_Values : Real_Array (1 .. Row_Count);
   Tan_Values : Real_Array (1 .. Row_Count);
begin
   -- build table
   for I in 1 .. Row_Count loop
      X_Values (I)   := Long_Float (I) * 0.05 - 0.05;
      Sin_Values (I) := Math.Sin (X_Values (I));
      Cos_Values (I) := Math.Cos (X_Values (I));
      Tan_Values (I) := Math.Tan (X_Values (I));
   end loop;
   declare
      Sin : Thiele_Interpolation := Create (Sin_Values, X_Values);
      Cos : Thiele_Interpolation := Create (Cos_Values, X_Values);
      Tan : Thiele_Interpolation := Create (Tan_Values, X_Values);
   begin
      Ada.Text_IO.Put_Line
        ("Internal Math.Pi:    " &
         Long_Float'Image (Ada.Numerics.Pi));
      Ada.Text_IO.Put_Line
        ("Thiele 6*InvSin(0.5):" &
         Long_Float'Image (6.0 * Inverse (Sin, 0.5)));
      Ada.Text_IO.Put_Line
        ("Thiele 3*InvCos(0.5):" &
         Long_Float'Image (3.0 * Inverse (Cos, 0.5)));
      Ada.Text_IO.Put_Line
        ("Thiele 4*InvTan(1):  " &
         Long_Float'Image (4.0 * Inverse (Tan, 1.0)));
   end;
end Main;
```


output:

```txt
Internal Math.Pi:     3.14159265358979E+00
Thiele 6*InvSin(0.5): 3.14159265358979E+00
Thiele 3*InvCos(0.5): 3.14159265358979E+00
Thiele 4*InvTan(1):   3.14159265358979E+00
```



## ALGOL 68

```algol68
PROC raise exception = ([]STRING msg)VOID: ( putf(stand error,("Exception:", $" "g$, msg, $l$)); stop );

# The MODE of lx and ly here should really be a UNION of "something REAL",
"something COMPLex", and "something SYMBOLIC" ... #

PROC thiele=([]REAL lx,ly, REAL x) REAL:
BEGIN
  []REAL xx=lx[@1],yy=ly[@1];
  INT n=UPB xx;
  IF UPB yy=n THEN
# Assuming that the values of xx are distinct ... #
    [0:n-1,1:n]REAL p;
    p[0,]:=yy[];
    FOR i TO n-1 DO p[1,i]:=(xx[i]-xx[1+i])/(p[0,i]-p[0,1+i]) OD;
    FOR i FROM 2 TO n-1 DO
      FOR j TO n-i DO
        p[i,j]:=(xx[j]-xx[j+i])/(p[i-1,j]-p[i-1,j+1])+p[i-2,j+1]
      OD
    OD;
    REAL a:=0;
    FOR i FROM n-1 BY -1 TO 2 DO a:=(x-xx[i])/(p[i,1]-p[i-2,1]+a) OD;
    yy[1]+(x-xx[1])/(p[1,1]+a)
  ELSE
    raise exception(("Unequal length arrays supplied: ",whole(UPB xx,0)," NE ",whole(UPB yy,0))); SKIP
  FI
END;

test:(
  FORMAT real fmt = $g(0,real width-2)$;

  REAL lwb x=0, upb x=1.55, delta x = 0.05;

  [0:ENTIER ((upb x-lwb x)/delta x)]STRUCT(REAL x, sin x, cos x, tan x) trig table;

  PROC init trig table = VOID:
    FOR i FROM LWB trig table TO UPB trig table DO
      REAL x = lwb x+i*delta x;
      trig table[i]:=(x, sin(x), cos(x), tan(x))
    OD;

  init trig table;

# Curry the thiele function to create matching inverse trigonometric functions #
  PROC (REAL)REAL inv sin = thiele(sin x OF trig table, x OF trig table,),
                  inv cos = thiele(cos x OF trig table, x OF trig table,),
                  inv tan = thiele(tan x OF trig table, x OF trig table,);

  printf(($"pi estimate using "g" interpolation: "f(real fmt)l$,
    "sin", 6*inv sin(1/2),
    "cos", 3*inv cos(1/2),
    "tan", 4*inv tan(1)
  ))
)
```

Output:

```txt

pi estimate using sin interpolation: 3.1415926535898
pi estimate using cos interpolation: 3.1415926535898
pi estimate using tan interpolation: 3.1415926535898

```



## C

The recursive relations of <math>\rho</math>s can be made clearer: Given <math>N+1</math> sampled points <math>x_0, x_1, \cdots x_N</math>, rewrite the symbol <math>\rho</math> as

:<math>\rho_{n,i} = \rho_n(x_i, x_{i+1}, \cdots x_{i+n})</math> where <math>1 \leq n \leq N</math>,

with suplements

:<math>\rho_{0, i} = f(x_i)\qquad\text{and}\qquad\rho_{n, i} = 0\quad\text{for}\quad n < 0</math>.

Now the recursive relation is simply

:<math>\rho_{n,i} = \displaystyle{x_i - x_{i + n} \over \rho_{n-1,i} - \rho_{n-1,i+1}} + \rho_{n-2,i+1}, \ 0\leq i+n \leq N</math>

Also note how <math>f(x_1)</math> in the interpolation formula can be replaced by <math>\rho_{0,1}</math>; define Thiele interpolation at step <math>n</math> as

:<math>\displaystyle{F_n(x) = \rho_{n,1} - \rho_{n - 2, 1} + { x - x_{n+1}\over F_{n+1}(x)}}</math>

with the termination <math>F_N(x) = 1</math>, and the interpolation formula is now <math>f(x) = F_0(x)</math>, easily implemented as a recursive function.

Note that each <math>\rho_n</math> needs to look up <math>\rho_{n-1}</math> twice, so the total look ups go up as <math>O(2^N)</math> while there are only <math>O(N^2)</math> values.  This is a text book situation for memoization.

```c
#include <stdio.h>
#include <string.h>
#include <math.h>

#define N 32
#define N2 (N * (N - 1) / 2)
#define STEP .05

double xval[N], t_sin[N], t_cos[N], t_tan[N];

/* rho tables, layout:
	rho_{n-1}(x0)
	rho_{n-2}(x0), rho_{n-1}(x1),
	....
	rho_0(x0), rho_0(x1), ... rho_0(x_{n-1})
   rho_i row starts at index (n - 1 - i) * (n - i) / 2  	*/
double r_sin[N2], r_cos[N2], r_tan[N2];

/* both rho and thiele functions recursively resolve values as decribed by
   formulas.  rho is cached, thiele is not. */

/* rho_n(x_i, x_{i+1}, ..., x_{i + n}) */
double rho(double *x, double *y, double *r, int i, int n)
{
	if (n < 0) return 0;
	if (!n) return y[i];

	int idx = (N - 1 - n) * (N - n) / 2 + i;
	if (r[idx] != r[idx]) /* only happens if value not computed yet */
		r[idx] = (x[i] - x[i + n])
			/ (rho(x, y, r, i, n - 1) - rho(x, y, r, i + 1, n - 1))
			+ rho(x, y, r, i + 1, n - 2);
	return r[idx];
}

double thiele(double *x, double *y, double *r, double xin, int n)
{
	if (n > N - 1) return 1;
	return rho(x, y, r, 0, n) - rho(x, y, r, 0, n - 2)
		+ (xin - x[n]) / thiele(x, y, r, xin, n + 1);
}

#define i_sin(x) thiele(t_sin, xval, r_sin, x, 0)
#define i_cos(x) thiele(t_cos, xval, r_cos, x, 0)
#define i_tan(x) thiele(t_tan, xval, r_tan, x, 0)

int main()
{
	int i;
	for (i = 0; i < N; i++) {
		xval[i] = i * STEP;
		t_sin[i] = sin(xval[i]);
		t_cos[i] = cos(xval[i]);
		t_tan[i] = t_sin[i] / t_cos[i];
	}
	for (i = 0; i < N2; i++)
		/* init rho tables to NaN */
		r_sin[i] = r_cos[i] = r_tan[i] = 0/0.;

	printf("%16.14f\n", 6 * i_sin(.5));
	printf("%16.14f\n", 3 * i_cos(.5));
	printf("%16.14f\n", 4 * i_tan(1.));
	return 0;
}
```
output<lang>3.14159265358979
3.14159265358979
3.14159265358979
```



## Common Lisp

Using the notations from above the C code instead of task desc.

```lisp
;; 256 is heavy overkill, but hey, we memoized
(defparameter *thiele-length* 256)
(defparameter *rho-cache* (make-hash-table :test #'equal))

(defmacro make-thele-func (f name xx0 xx1)
  (let ((xv (gensym)) (yv (gensym))
	(x0 (gensym)) (x1 (gensym)))
    `(let* ((,xv (make-array (1+ *thiele-length*)))
	    (,yv (make-array (1+ *thiele-length*)))
	    (,x0 ,xx0)
	    (,x1 ,xx1))
       (loop for i to *thiele-length* with x do
	     (setf x (+ ,x0 (* (/ (- ,x1 ,x0) *thiele-length*) i))
		   (aref ,yv i) x
		   (aref ,xv i) (funcall ,f x)))
       (defun ,name (x) (thiele x ,yv ,xv, 0)))))

(defun rho (yv xv n i)
  (let (hit (key (list yv xv n i)))
    (if (setf hit (gethash key *rho-cache*))
      hit
      (setf (gethash key *rho-cache*)
	    (cond ((zerop n) (aref yv i))
		  ((minusp n) 0)
		  (t (+ (rho yv xv (- n 2) (1+ i))
			(/  (- (aref xv i)
			       (aref xv (+ i n)))
			    (- (rho yv xv (1- n) i)
			       (rho yv xv (1- n) (1+ i)))))))))))

(defun thiele (x yv xv n)
  (if (= n *thiele-length*)
    1
    (+ (- (rho yv xv n 1) (rho yv xv (- n 2) 1))
       (/ (- x (aref xv (1+ n)))
	  (thiele x yv xv (1+ n))))))

(make-thele-func #'sin inv-sin 0 (/ pi 2))
(make-thele-func #'cos inv-cos 0 (/ pi 2))
(make-thele-func #'tan inv-tan 0 (/ pi 2.1)) ; tan(pi/2) is INF

(format t "~f~%" (* 6 (inv-sin .5)))
(format t "~f~%" (* 3 (inv-cos .5)))
(format t "~f~%" (* 4 (inv-tan 1)))
```
output (SBCL):<lang>3.141592653589793
3.1415926535885172
3.141592653589819
```



## D


```d
import std.stdio, std.range, std.array, std.algorithm, std.math;

struct Domain {
    const real b, e, s;

    auto range() const pure /*nothrow*/ @safe /*@nogc*/ {
        return iota(b, e + s, s);
    }
}

real eval0(alias RY, alias X, alias Y)(in real x) pure nothrow @safe @nogc {
    real a = 0.0L;
    foreach_reverse (immutable i; 2 .. X.length - 3)
        a = (x - X[i]) / (RY[i] - RY[i-2] + a);
    return Y[1] + (x - X[1]) / (RY[1] + a);
}

immutable struct Thiele {
    immutable real[] Y, X, rhoY, rhoX;

    this(real[] y, real[] x) immutable pure nothrow /*@safe*/
    in {
        assert(x.length > 2, "at leat 3 values");
        assert(x.length == y.length, "input arrays not of same size");
    } body {
        this.Y = y.idup;
        this.X = x.idup;
        rhoY = rhoN(Y, X);
        rhoX = rhoN(X, Y);
    }

    this(in real function(real) pure nothrow @safe @nogc f,
         Domain d = Domain(0.0L, 1.55L, 0.05L))
    immutable pure /*nothrow @safe*/ {
        auto xrng = d.range.array;
        this(xrng.map!f.array, xrng);
    }

    auto rhoN(immutable real[] y, immutable real[] x)
    pure nothrow @safe {
        immutable int N = x.length;
        auto p = new real[][](N, N);
        p[0][] = y[];
        p[1][0 .. $ - 1] = (x[0 .. $-1] - x[1 .. $]) /
                           (p[0][0 .. $-1] - p[0][1 .. $]);
        foreach (immutable int j; 2 .. N - 1) {
            immutable M = N - j - 1;
            p[j][0..M] = p[j-2][1..M+1] + (x[0..M] - x[j..M+j]) /
                         (p[j-1][0 .. M] - p[j-1][1 .. M+1]);
        }
        return p.map!q{ a[1] }.array;
    }

    alias eval = eval0!(rhoY, X, Y);
    alias inverse = eval0!(rhoX, Y, X);
}

void main() {
    // Can't pass sin, cos and tan directly.
    immutable tsin = Thiele(x => x.sin);
    immutable tcos = Thiele(x => x.cos);
    immutable ttan = Thiele(x => x.tan);

    writefln(" %d interpolating points\n", tsin.X.length);
    writefln("std.math.sin(0.5): %20.18f", 0.5L.sin);
    writefln("  Thiele sin(0.5): %20.18f\n", tsin.eval(0.5L));

    writefln("*%20.19f library constant", PI);
    writefln(" %20.19f 6 * inv_sin(0.5)", tsin.inverse(0.5L) * 6.0L);
    writefln(" %20.19f 3 * inv_cos(0.5)", tcos.inverse(0.5L) * 3.0L);
    writefln(" %20.19f 4 * inv_tan(1.0)", ttan.inverse(1.0L) * 4.0L);
}
```

```txt
 32 interpolating points

std.math.sin(0.5): 0.479425538604203000
  Thiele sin(0.5): 0.479425538604203000

*3.1415926535897932385 library constant
 3.1415926535897932380 6 * inv_sin(0.5)
 3.1415926535897932382 3 * inv_cos(0.5)
 3.1415926535897932382 4 * inv_tan(1.0)
```



## Go

```go
package main

import (
    "fmt"
    "math"
)

func main() {
    // task 1: build 32 row trig table
    const nn = 32
    const step = .05
    xVal := make([]float64, nn)
    tSin := make([]float64, nn)
    tCos := make([]float64, nn)
    tTan := make([]float64, nn)
    for i := range xVal {
        xVal[i] = float64(i) * step
        tSin[i], tCos[i] = math.Sincos(xVal[i])
        tTan[i] = tSin[i] / tCos[i]
    }
    // task 2: define inverses
    iSin := thieleInterpolator(tSin, xVal)
    iCos := thieleInterpolator(tCos, xVal)
    iTan := thieleInterpolator(tTan, xVal)
    // task 3: demonstrate identities
    fmt.Printf("%16.14f\n", 6*iSin(.5))
    fmt.Printf("%16.14f\n", 3*iCos(.5))
    fmt.Printf("%16.14f\n", 4*iTan(1))
}

func thieleInterpolator(x, y []float64) func(float64) float64 {
    n := len(x)
    ρ := make([][]float64, n)
    for i := range ρ {
        ρ[i] = make([]float64, n-i)
        ρ[i][0] = y[i]
    }
    for i := 0; i < n-1; i++ {
        ρ[i][1] = (x[i] - x[i+1]) / (ρ[i][0] - ρ[i+1][0])
    }
    for i := 2; i < n; i++ {
        for j := 0; j < n-i; j++ {
            ρ[j][i] = (x[j]-x[j+i])/(ρ[j][i-1]-ρ[j+1][i-1]) + ρ[j+1][i-2]
        }
    }
    // ρ0 used in closure.  the rest of ρ becomes garbage.
    ρ0 := ρ[0]
    return func(xin float64) float64 {
        var a float64
        for i := n - 1; i > 1; i-- {
            a = (xin - x[i-1]) / (ρ0[i] - ρ0[i-2] + a)
        }
        return y[0] + (xin-x[0])/(ρ0[1]+a)
    }
}
```

Output:

```txt

3.14159265358979
3.14159265358979
3.14159265358980

```



## Haskell

Caching of rho is automatic due to lazy lists.

```haskell
thiele :: [Double] -> [Double] -> Double -> Double
thiele xs ys = f rho1 (tail xs)
  where
    f _ [] _ = 1
    f r@(r0:r1:r2:rs) (x:xs) v = r2 - r0 + (v - x) / f (tail r) xs v
    rho1 = ((!! 1) . (++ [0])) <$> rho
    rho = [0,0 ..] : [0,0 ..] : ys : rnext (tail rho) xs (tail xs)
      where
        rnext _ _ [] = []
        rnext r@(r0:r1:rs) x xn =
          let z_ = zipWith
          in z_ (+) (tail r0) (z_ (/) (z_ (-) x xn) (z_ (-) r1 (tail r1))) :
             rnext (tail r) x (tail xn)

-- Inverted interpolation function of f
invInterp :: (Double -> Double) -> [Double] -> Double -> Double
invInterp f xs = thiele (map f xs) xs

main :: IO ()
main =
  mapM_
    print
    [ 3.21 * inv_sin (sin (pi / 3.21))
    , pi / 1.2345 * inv_cos (cos 1.2345)
    , 7 * inv_tan (tan (pi / 7))
    ]
  where
    [inv_sin, inv_cos, inv_tan] =
      uncurry ((. div_pi) . invInterp) <$>
      [(sin, (2, 31)), (cos, (2, 100)), (tan, (4, 1000))]
    -- N points taken uniformly from 0 to Pi/d
    div_pi (d, n) = (* (pi / (d * n))) <$> [0 .. n]
```

```txt
3.141592653589795
3.141592653589791
3.141592653587783
```



## J


```j

span =: {. - {:  NB. head - tail
spans =: span\   NB. apply span to successive infixes

```



```txt

   span 12 888 6 4 8 3
9
   4 spans 12 888 6 4 8 3
8 880 3

```



```j

NB. abscissae_of_knots coef ordinates_of_knots
NB. returns the interpolation coefficients for eval
coef =: 4 : 0
 p =. _2 _{.,:y
 for_i. i. # x do.
   p =. (p , ([: }. - }. p {~ _2:) + (x spans~ 2+]) % 2 spans - }. [: {: p"_) i
 end.
 x; , _ 1 {. p
)

NB. unknown_abscissae eval coefficients
eval =: 4 : 0
 'xx p' =. y
 a =. 0
 i =. <: # xx
 while. 0 < i=.<:i do.
   a =. (x-i{xx)%-/(p{~i+2),(i{p),a
 end.
 (p{~>:i)+(x-i{xx)%(p{~i+2)+a
)

```



```txt

   trig_table =: 1 2 3 o./ angles =: 5r100*i.32

   0 _1 }. ": (;:'angle sin cos tan'),.<"1] 8j4": _ 5{.angles,trig_table
┌─────┬────────────────────────────────────────
│angle│  0.0000  0.0500  0.1000  0.1500  0.2000
├─────┼────────────────────────────────────────
│sin  │  0.0000  0.0500  0.0998  0.1494  0.1987
├─────┼────────────────────────────────────────
│cos  │  1.0000  0.9988  0.9950  0.9888  0.9801
├─────┼────────────────────────────────────────
│tan  │  0.0000  0.0500  0.1003  0.1511  0.2027
└─────┴────────────────────────────────────────


   ('Thiele pi';'error'),;/"1(,. 1p1&-)6 3 4 * 1r2 1r2 1 eval"0 1 trig_table coef"1 angles
┌─────────┬────────────┐
│Thiele pi│error       │
├─────────┼────────────┤
│3.14159  │_4.44089e_15│
├─────────┼────────────┤
│3.14159  │_4.44089e_16│
├─────────┼────────────┤
│3.14159  │_7.10543e_15│
└─────────┴────────────┘

```



```j

thiele =: 2 : 0
 p =. _2 _{.,:n
 for_i. i.#m do.
   p =. (p , ([: }. - }. p {~ _2:) + (m spans~ 2+]) % 2 spans - }. [: {: p"_) i
 end.
 p =. , _ 1 {. p
 a =. 0
 i =. <:#m
 while. 0 < i=.<:i do.
   a =. (y-i{m)%-/(p{~i+2),(i{p),a
 end.
 (p{~>:i)+(y-i{m)%a+p{~i+2
)

```



```txt

   's c t' =: trig_table
   asin =: s thiele angles

   6*asin 0.5
3.14159

   1r5 * i.6
0 1r5 2r5 3r5 4r5 1
   100*(_1&o. %~ _1&o. - asin) 1r5*i.6   NB. % error arcsin
0 1.4052 4.50319 9.32495 16.9438 39.321

```



## Java

```java
import static java.lang.Math.*;

public class Test {
    final static int N = 32;
    final static int N2 = (N * (N - 1) / 2);
    final static double STEP = 0.05;

    static double[] xval = new double[N];
    static double[] t_sin = new double[N];
    static double[] t_cos = new double[N];
    static double[] t_tan = new double[N];

    static double[] r_sin = new double[N2];
    static double[] r_cos = new double[N2];
    static double[] r_tan = new double[N2];

    static double rho(double[] x, double[] y, double[] r, int i, int n) {
        if (n < 0)
            return 0;

        if (n == 0)
            return y[i];

        int idx = (N - 1 - n) * (N - n) / 2 + i;
        if (r[idx] != r[idx])
            r[idx] = (x[i] - x[i + n])
                    / (rho(x, y, r, i, n - 1) - rho(x, y, r, i + 1, n - 1))
                    + rho(x, y, r, i + 1, n - 2);

        return r[idx];
    }

    static double thiele(double[] x, double[] y, double[] r, double xin, int n) {
        if (n > N - 1)
            return 1;
        return rho(x, y, r, 0, n) - rho(x, y, r, 0, n - 2)
                + (xin - x[n]) / thiele(x, y, r, xin, n + 1);
    }

    public static void main(String[] args) {
        for (int i = 0; i < N; i++) {
            xval[i] = i * STEP;
            t_sin[i] = sin(xval[i]);
            t_cos[i] = cos(xval[i]);
            t_tan[i] = t_sin[i] / t_cos[i];
        }

        for (int i = 0; i < N2; i++)
            r_sin[i] = r_cos[i] = r_tan[i] = Double.NaN;

        System.out.printf("%16.14f%n", 6 * thiele(t_sin, xval, r_sin, 0.5, 0));
        System.out.printf("%16.14f%n", 3 * thiele(t_cos, xval, r_cos, 0.5, 0));
        System.out.printf("%16.14f%n", 4 * thiele(t_tan, xval, r_tan, 1.0, 0));
    }
}
```


```txt
3.14159265358979
3.14159265358979
3.14159265358980
```



## Julia

Accuracy improves with a larger table and smaller step size.
```julia
const N = 256
const N2 = N * div(N - 1, 2)
const step = 0.01
const xval_table = zeros(Float64, N)
const tsin_table = zeros(Float64, N)
const tcos_table = zeros(Float64, N)
const ttan_table = zeros(Float64, N)
const rsin_cache = Dict{Float64, Float64}()
const rcos_cache = Dict{Float64, Float64}()
const rtan_cache = Dict{Float64, Float64}()

function rho(x, y, rhocache, i, n)
    if n < 0
        return 0.0
    elseif n == 0
        return y[i+1]
    end
    idx = (N - 1 - n) * div(N - n, 2) + i
    if !haskey(rhocache, idx)
        rhocache[idx] = (x[i+1] - x[i + n+1]) / (rho(x, y, rhocache, i, n - 1) -
            rho(x, y, rhocache, i + 1, n - 1)) + rho(x, y, rhocache, i + 1, n - 2)
    end
    rhocache[idx]
end

function thiele(x, y, r, xin, n)
    if n > N - 1
        return 1.0
    end
    rho(x, y, r, 0, n) - rho(x, y, r, 0, n - 2) + (xin - x[n + 1]) / thiele(x, y, r, xin, n + 1)
end

function thiele_tables()
    for i in 1:N
        xval_table[i] = (i-1) * step
        tsin_table[i] = sin(xval_table[i])
        tcos_table[i] = cos(xval_table[i])
        ttan_table[i] = tsin_table[i] / tcos_table[i]
    end
    println(6 * thiele(tsin_table, xval_table, rsin_cache, 0.5, 0))
    println(3 * thiele(tcos_table, xval_table, rcos_cache, 0.5, 0))
    println(4 * thiele(ttan_table, xval_table, rtan_cache, 1.0, 0))
end

thiele_tables()

```
```txt

 3.1415926535898335
 3.141592653589818
 3.141592653589824

```



## Kotlin

```scala
// version 1.1.2

const val N = 32
const val N2 = N * (N - 1) / 2
const val STEP = 0.05

val xval = DoubleArray(N)
val tsin = DoubleArray(N)
val tcos = DoubleArray(N)
val ttan = DoubleArray(N)
val rsin = DoubleArray(N2) { Double.NaN }
val rcos = DoubleArray(N2) { Double.NaN }
val rtan = DoubleArray(N2) { Double.NaN }

fun rho(x: DoubleArray, y: DoubleArray, r: DoubleArray, i: Int, n: Int): Double {
    if (n < 0) return 0.0
    if (n == 0) return y[i]
    val idx = (N - 1 - n) * (N - n) / 2 + i
    if (r[idx].isNaN()) {
        r[idx] = (x[i] - x[i + n]) /
                 (rho(x, y, r, i, n - 1) - rho(x, y, r, i + 1, n - 1)) +
                  rho(x, y, r, i + 1, n - 2)
    }
    return r[idx]
}

fun thiele(x: DoubleArray, y: DoubleArray, r: DoubleArray, xin: Double, n: Int): Double {
    if (n > N - 1) return 1.0
    return rho(x, y, r, 0, n) - rho(x, y, r, 0, n - 2) +
           (xin - x[n]) / thiele(x, y, r, xin, n + 1)
}

fun main(args: Array<String>) {
    for (i in 0 until N) {
        xval[i] = i * STEP
        tsin[i] = Math.sin(xval[i])
        tcos[i] = Math.cos(xval[i])
        ttan[i] = tsin[i] / tcos[i]
    }
    println("%16.14f".format(6 * thiele(tsin, xval, rsin, 0.5, 0)))
    println("%16.14f".format(3 * thiele(tcos, xval, rcos, 0.5, 0)))
    println("%16.14f".format(4 * thiele(ttan, xval, rtan, 1.0, 0)))
}
```


```txt

3.14159265358979
3.14159265358979
3.14159265358980

```



## Nim

```Nim
import strformat
import math

const N = 32
const N2 = N * (N - 1) div 2
const STEP = 0.05

var xval = newSeq[float](N)
var tsin = newSeq[float](N)
var tcos = newSeq[float](N)
var ttan = newSeq[float](N)
var rsin = newSeq[float](N2)
var rcos = newSeq[float](N2)
var rtan = newSeq[float](N2)

proc rho(x, y: openArray[float], r: var openArray[float], i, n: int): float =
  if n < 0:
    return 0
  if n == 0:
    return y[i]

  let idx = (N - 1 - n) * (N - n) div 2 + i
  if r[idx] != r[idx]:
    r[idx] = (x[i] - x[i + n]) /
      (rho(x, y, r, i, n - 1) - rho(x, y, r, i + 1, n - 1)) +
       rho(x, y, r, i + 1, n - 2)
  return r[idx]

proc thiele(x, y: openArray[float], r: var openArray[float], xin: float, n: int): float =
  if n > N - 1:
    return 1
  return rho(x, y, r, 0, n) - rho(x, y, r, 0, n - 2) +
    (xin - x[n]) / thiele(x, y, r, xin, n + 1)

for i in 0..<N:
  xval[i] = float(i) * STEP
  tsin[i] = sin(xval[i])
  tcos[i] = cos(xval[i])
  ttan[i] = tsin[i] / tcos[i]

for i in 0..<N2:
  rsin[i] = NaN
  rcos[i] = NaN
  rtan[i] = NaN

echo fmt"{6 * thiele(tsin, xval, rsin, 0.5, 0):16.14}"
echo fmt"{3 * thiele(tcos, xval, rcos, 0.5, 0):16.14}"
echo fmt"{4 * thiele(ttan, xval, rtan, 1.0, 0):16.14}"
```

```txt

 3.1415926535898
 3.1415926535898
 3.1415926535898

```



## OCaml

This example shows how the accuracy changes with the degree of interpolation. The table 'columns' are only constructed implicitly during the recursive calculation of <em>rdiff</em> and <em>thiele</em>, but (as mentioned in the C code example) using memoization or explicit tabulation would speed up the calculation. The interpolation uses the nearest points around <em>x</em> for accuracy.


```OCaml
let xv, fv = fst, snd

let rec rdiff a l r =
   if l > r then 0.0 else
   if l = r then fv a.(l) else
   if l+1 = r then (xv a.(l) -. xv a.(r)) /. (fv a.(l) -. fv a.(r)) else
   (xv a.(l) -. xv a.(r)) /. (rdiff a l (r-1) -. rdiff a (l+1) r) +. rdiff a (l+1) (r-1)

let rec thiele x a a0 k n =
   if k = n then 1.0 else
   rdiff a a0 (a0+k) -. rdiff a a0 (a0+k-2) +. (x -. xv a.(a0+k)) /. thiele x a a0 (k+1) n

let interpolate x a n =
   let m = Array.length a in
   let dist i = abs_float (x -. xv a.(i)) in
   let nearer i j = if dist j < dist i then j else i in
   let rec closest i j = if j = m then i else closest (nearer i j) (j+1) in
   let c = closest 0 1 in
   let c' = if c < n/2 then 0 else if c > m-n then m-n else c-(n/2) in
   thiele x a c' 0 n

let table a b n f =
   let g i =
      let x = a +. (b-.a)*.(float i)/.(float (n-1)) in
      (f x, x) in
   Array.init n g

let [sin_tab; cos_tab; tan_tab] = List.map (table 0.0 1.55 32) [sin; cos; tan]

let test n =
   Printf.printf "\nDegree %d interpolation:\n" n;
   Printf.printf "6*arcsin(0.5) = %.15f\n" (6.0*.(interpolate 0.5 sin_tab n));
   Printf.printf "3*arccos(0.5) = %.15f\n" (3.0*.(interpolate 0.5 cos_tab n));
   Printf.printf "4*arctan(1.0) = %.15f\n" (4.0*.(interpolate 1.0 tan_tab n));;

List.iter test [8; 12; 16]
```

Output:

```txt
Degree 8 interpolation:
6*arcsin(0.5) = 3.141592654456238
3*arccos(0.5) = 3.141592653520809
4*arctan(1.0) = 3.141592653437432

Degree 12 interpolation:
6*arcsin(0.5) = 3.141592653587590
3*arccos(0.5) = 3.141592653562618
4*arctan(1.0) = 3.141592653589756

Degree 16 interpolation:
6*arcsin(0.5) = 3.141592653589793
3*arccos(0.5) = 3.141592653589793
4*arctan(1.0) = 3.141592653589793
```



## Perl 6

Implemented to parallel the generalized formula, making for clearer, but slower, code.  Offsetting that, the use of <code>Promise</code> allows concurrent calculations, so running all three types of interpolation should not take any longer than running just one (presuming available cores).


```perl6
# reciprocal difference:
multi sub ρ(&f, @x where * < 1) { 0 } # Identity
multi sub ρ(&f, @x where * == 1) { &f(@x[0]) }
multi sub ρ(&f, @x where * > 1) {
    ( @x[0] - @x[* - 1] )       # ( x - x[n] )
    / (ρ(&f, @x[^(@x - 1)])     # / ( ρ[n-1](x[0], ..., x[n-1])
    - ρ(&f, @x[1..^@x]) )       # - ρ[n-1](x[1], ..., x[n]) )
    + ρ(&f, @x[1..^(@x - 1)]);  # + ρ[n-2](x[1], ..., x[n-1])
}

# Thiele:
multi sub thiele($x, %f, $ord where { $ord == +%f }) { 1 } # Identity
multi sub thiele($x, %f, $ord) {
  my &f = {%f{$^a}};                # f(x) as a table lookup

  # must sort hash keys to maintain order between invocations
  my $a = ρ(&f, %f.keys.sort[^($ord +1)]);
  my $b = ρ(&f, %f.keys.sort[^($ord -1)]);

  my $num = $x - %f.keys.sort[$ord];
  my $cont = thiele($x, %f, $ord +1);

  # Thiele always takes this form:
  return $a - $b + ( $num / $cont );
}

## Demo
sub mk-inv(&fn, $d, $lim) {
  my %h;
  for 0..$lim { %h{ &fn($_ * $d) } = $_ * $d }
  return %h;
}

sub MAIN($tblsz = 12) {

  my ($sin_pi, $cos_pi, $tan_pi);
  my $p1 = Promise.start( { my %invsin = mk-inv(&sin, 0.05, $tblsz); $sin_pi = 6 * thiele(0.5, %invsin, 0) } );
  my $p2 = Promise.start( { my %invcos = mk-inv(&cos, 0.05, $tblsz); $cos_pi = 3 * thiele(0.5, %invcos, 0) } );
  my $p3 = Promise.start( { my %invtan = mk-inv(&tan, 0.05, $tblsz); $tan_pi = 4 * thiele(1.0, %invtan, 0) } );
  await $p1, $p2, $p3;

  say "pi = {pi}";
  say "estimations using a table of $tblsz elements:";
  say "sin interpolation: $sin_pi";
  say "cos interpolation: $cos_pi";
  say "tan interpolation: $tan_pi";
}
```


Output:


```txt
pi = 3.14159265358979
estimations using a table of 12 elements:
sin interpolation: 3.14159265358961
cos interpolation: 3.1387286696692
tan interpolation: 3.14159090545243
```



## Phix

To be honest I was slightly wary of this, what with tables being passed by reference and fairly heavy use of closures in other languages, but in the end all it took was a simple enum (R_SIN..R_TRIG).

```Phix
constant N = 32,
         N2 = (N * (N - 1) / 2),
         STEP = 0.05

constant inf = 1e300*1e300,
         nan = -(inf/inf)

sequence {xval, t_sin, t_cos, t_tan} @= repeat(0,N)

for i=1 to N do
    xval[i] = (i-1) * STEP
    t_sin[i] = sin(xval[i])
    t_cos[i] = cos(xval[i])
    t_tan[i] = t_sin[i] / t_cos[i]
end for

enum R_SIN, R_COS, R_TAN, R_TRIG=$

sequence rhot = repeat(repeat(nan,N2),R_TRIG)

function rho(sequence x, y, integer rdx, int i, int n)
    if n<0 then return 0 end if
    if n=0 then return y[i+1] end if

    integer idx = (N - 1 - n) * (N - n) / 2 + i + 1;
    if rhot[rdx][idx]=nan then -- value not computed yet
        rhot[rdx][idx] = (x[i+1] - x[i+1 + n])
                        / (rho(x, y, rdx, i, n-1) - rho(x, y, rdx, i+1, n-1))
                        + rho(x, y, rdx, i+1, n-2)
    end if
    return rhot[rdx][idx]
end function

function thiele(sequence x, y, integer rdx, atom xin, integer n)
    if n>N-1 then return 1 end if
    return rho(x, y, rdx, 0, n) - rho(x, y, rdx, 0, n-2)
            + (xin-x[n+1]) / thiele(x, y, rdx, xin, n+1)
end function

constant fmt = iff(machine_bits()=32?"%32s : %.14f\n"
                                    :"%32s : %.17f\n")
printf(1,fmt,{"PI",PI})
printf(1,fmt,{"6*arcsin(0.5)",6*arcsin(0.5)})
printf(1,fmt,{"3*arccos(0.5)",3*arccos(0.5)})
printf(1,fmt,{"4*arctan(1)",4*arctan(1)})

printf(1,fmt,{"6*thiele(t_sin,xval,R_SIN,0.5,0)",6*thiele(t_sin,xval,R_SIN,0.5,0)})
printf(1,fmt,{"3*thiele(t_cos,xval,R_COS,0.5,0)",3*thiele(t_cos,xval,R_COS,0.5,0)})
printf(1,fmt,{"4*thiele(t_tan,xval,R_TAN,1,0)",4*thiele(t_tan,xval,R_TAN,1,0)})
```

(64 bit, obviously 3 fewer digits on 32 bit)

```txt

                              PI : 3.14159265358979324
                   6*arcsin(0.5) : 3.14159265358979324
                   3*arccos(0.5) : 3.14159265358979324
                     4*arctan(1) : 3.14159265358979324
6*thiele(t_sin,xval,R_SIN,0.5,0) : 3.14159265358979324
3*thiele(t_cos,xval,R_COS,0.5,0) : 3.14159265358979324
  4*thiele(t_tan,xval,R_TAN,1,0) : 3.14159265358979324

```



## PicoLisp

```PicoLisp
(scl 17)
(load "@lib/math.l")

(setq
   *X-Table (range 0.0 1.55 0.05)
   *SinTable (mapcar sin *X-Table)
   *CosTable (mapcar cos *X-Table)
   *TanTable (mapcar tan *X-Table)
   *TrigRows (length *X-Table) )

(let N2 (>> 1 (* *TrigRows (dec *TrigRows)))
   (setq
      *InvSinTable (need N2)
      *InvCosTable (need N2)
      *InvTanTable (need N2) ) )

(de rho (Tbl Inv I N)
   (cond
      ((lt0 N) 0)
      ((=0 N) (get *X-Table I))
      (T
         (let Idx (+ I (>> 1 (* (- *TrigRows 1 N) (- *TrigRows N))))
            (or
               (get Inv Idx)
               (set (nth Inv Idx)  # only happens if value not computed yet
                  (+
                     (rho Tbl Inv (inc I) (- N 2))
                     (*/
                        (- (get Tbl I) (get Tbl (+ I N)))
                        1.0
                        (-
                           (rho Tbl Inv I (dec N))
                           (rho Tbl Inv (inc I) (dec N)) ) ) ) ) ) ) ) ) )

(de thiele (Tbl Inv X N)
   (if (> N *TrigRows)
      1.0
      (+
         (-
            (rho Tbl Inv 1 (dec N))
            (rho Tbl Inv 1 (- N 3)) )
         (*/
            (- X (get Tbl N))
            1.0
            (thiele Tbl Inv X (inc N)) ) ) ) )

(de iSin (X)
   (thiele *SinTable *InvSinTable X 1) )

(de iCos (X)
   (thiele *CosTable *InvCosTable X 1) )

(de iTan (X)
   (thiele *TanTable *InvTanTable 1.0 1) )
```

Test:

```PicoLisp
(prinl (round (* 6 (iSin 0.5)) 15))
(prinl (round (* 3 (iCos 0.5)) 15))
(prinl (round (* 4 (iTan 1.0)) 15))
```

Output:

```txt
3.141592653589793
3.141592653589793
3.141592653589793
```



## PowerShell


```PowerShell
Function Reciprocal-Difference( [Double[][]] $function )
{
    $rho=@()
    $rho+=0
    $funcl = $function.length
    if( $funcl -gt 0 )
    {
        -2..($funcl-1) | ForEach-Object {
            $i=$_
            #Write-Host "$($i+1) - $($rho[$i+1]) - $($rho[$i+1].GetType())"
            $rho[$i+2] = $( 0..($funcl-$i-1) | Where-Object {$_ -lt $funcl} | ForEach-Object {
                $j=$_
                switch ($i) {
                    {$_ -lt 0 } { 0 }
                    {$_ -eq 0 } { $function[$j][1] }
                    {$_ -gt 0 } { ( $function[$j][0] - $function[$j+$i][0] ) / ( $rho[$i+1][$j] - $rho[$i+1][$j+1] ) + $rho[$i][$j+1] }
                }
            if( $_ -lt $funcl )
            {
                $rho += 0
            }
        })
        }
    }
    $rho
}

Function Thiele-Interpolation ( [Double[][]] $function )
{
    $funcl = $function.length
    $invoke = "{`n`tparam([Double] `$x)`n"
    if($funcl -gt 1)
    {
        $rho = Reciprocal-Difference $function
        ($funcl-1)..0 | ForEach-Object {
            $invoke += "`t"
            $invoke += '$x{0} = {1} - {2}' -f $_, @($rho[$_+2])[0], @($rho[$_])[0]
            if($_ -lt ($funcl-1))
            {
                $invoke += ' + ( $x - {0} ) / $x{1} ' -f $function[$_][0], ($_+1)
            }
            $invoke += "`n"
        }
        $invoke+="`t`$x0`n}"
    } else {
        $invoke += "`t`$x`n}"
    }
    invoke-expression $invoke
}

$sint=@{}; 0..31 | ForEach-Object { $_ * 0.05 } | ForEach-Object { $sint[$_] = [Math]::sin($_) }
$cost=@{}; 0..31 | ForEach-Object { $_ * 0.05 } | ForEach-Object { $cost[$_] = [Math]::cos($_) }
$tant=@{}; 0..31 | ForEach-Object { $_ * 0.05 } | ForEach-Object { $tant[$_] = [Math]::tan($_) }
$asint=New-Object 'Double[][]' 32,2; $sint.GetEnumerator() | Sort-Object Value | ForEach-Object {$i=0}{ $asint[$i][0] = $_.Value; $asint[$i][1] = $_.Name; $i++ }
$acost=New-Object 'Double[][]' 32,2; $cost.GetEnumerator() | Sort-Object Value | ForEach-Object { $i=0 }{ $acost[$i][0] = $_.Value; $acost[$i][1] = $_.Name; $i++ }
$atant=New-Object 'Double[][]' 32,2; $tant.GetEnumerator() | Sort-Object Value | ForEach-Object {$i=0}{ $atant[$i][0] = $_.Value; $atant[$i][1] = $_.Name; $i++ }

$asin = (Thiele-Interpolation $asint)
#uncomment to see the function
#"{$asin}"
6*$asin.InvokeReturnAsIs(.5)
$acos = (Thiele-Interpolation $acost)
#uncomment to see the function
#"{$acos}"
3*$acos.InvokeReturnAsIs(.5)
$atan = (Thiele-Interpolation $atant)
#uncomment to see the function
#"{$atan}"
4*$atan.InvokeReturnAsIs(1)
```



## Python

```python
#!/usr/bin/env python3

import math

def thieleInterpolator(x, y):
    ρ = [[yi]*(len(y)-i) for i, yi in enumerate(y)]
    for i in range(len(ρ)-1):
        ρ[i][1] = (x[i] - x[i+1]) / (ρ[i][0] - ρ[i+1][0])
    for i in range(2, len(ρ)):
        for j in range(len(ρ)-i):
            ρ[j][i] = (x[j]-x[j+i]) / (ρ[j][i-1]-ρ[j+1][i-1]) + ρ[j+1][i-2]
    ρ0 = ρ[0]
    def t(xin):
        a = 0
        for i in range(len(ρ0)-1, 1, -1):
            a = (xin - x[i-1]) / (ρ0[i] - ρ0[i-2] + a)
        return y[0] + (xin-x[0]) / (ρ0[1]+a)
    return t

# task 1: build 32 row trig table
xVal = [i*.05 for i in range(32)]
tSin = [math.sin(x) for x in xVal]
tCos = [math.cos(x) for x in xVal]
tTan = [math.tan(x) for x in xVal]
# task 2: define inverses
iSin = thieleInterpolator(tSin, xVal)
iCos = thieleInterpolator(tCos, xVal)
iTan = thieleInterpolator(tTan, xVal)
# task 3: demonstrate identities
print('{:16.14f}'.format(6*iSin(.5)))
print('{:16.14f}'.format(3*iCos(.5)))
print('{:16.14f}'.format(4*iTan(1)))
```

```txt

3.14159265358979
3.14159265358979
3.14159265358980

```



## Racket


```racket

#lang racket
(define xs (for/vector ([x (in-range 0.0 1.6 0.05)]) x))
(define (x i) (vector-ref xs i))

(define-syntax define-table
  (syntax-rules ()
    [(_ f tf rf if)
     (begin (define tab (for/vector ([x xs]) (f x)))
            (define (tf n) (vector-ref tab n))
            (define cache (make-vector (/ (* 32 31) 2) #f))
            (define (rf n thunk)
              (or (vector-ref cache n)
                  (let ([v (thunk)])
                    (vector-set! cache n v)
                    v)))
            (define (if t) (thiele tf x rf t 0)))]))

(define-table sin tsin rsin isin)
(define-table cos tcos rcos icos)
(define-table tan ttan rtan itan)

(define (rho x y r i n)
  (cond
    [(< n 0) 0]
    [(= n 0) (y i)]
    [else (r (+ (/ (* (- 32 1 n) (- 32 n)) 2) i)
             (λ() (+ (/ (- (x i) (x (+ i n)))
                        (- (rho x y r i (- n 1)) (rho x y r (+ i 1) (- n 1))))
                     (rho x y r (+ i 1) (- n 2)))))]))

(define (thiele x y r xin n)
  (cond
    [(> n 31) 1]
    [(+ (rho x y r 0 n) (- (rho x y r 0 (- n 2)))
        (/ (- xin (x n)) (thiele x y r xin (+ n 1))))]))

(* 6 (isin 0.5))
(* 3 (icos 0.5))
(* 4 (itan 1.))

```

Output:

```racket

3.141592653589793
3.1415926535897936
3.1415926535897953

```



## Sidef

```ruby
func thiele(x, y) {
    var ρ = {|i| [y[i]]*(y.len-i) }.map(^y)
 
    for i in ^(ρ.end) {
        ρ[i][1] = ((x[i] - x[i+1]) / (ρ[i][0] - ρ[i+1][0]))
    }
    for i (2 .. ρ.end) {
        for j (0 .. ρ.end-i) {
            ρ[j][i] = (((x[j]-x[j+i]) / (ρ[j][i-1]-ρ[j+1][i-1])) + ρ[j+1][i-2])
        }
    }
 
    var ρ0 = ρ[0]
 
    func t(xin) {
        var a = 0
        for i (ρ0.len ^.. 2) {
            a = ((xin - x[i-1]) / (ρ0[i] - ρ0[i-2] + a))
        }
        y[0] + ((xin-x[0]) / (ρ0[1]+a))
    }
    return t
}
 
# task 1: build 32 row trig table
var xVal = {|k| k * 0.05 }.map(^32)
var tSin = xVal.map { .sin }
var tCos = xVal.map { .cos }
var tTan = xVal.map { .tan }
 
# task 2: define inverses
var iSin = thiele(tSin, xVal)
var iCos = thiele(tCos, xVal)
var iTan = thiele(tTan, xVal)
 
# task 3: demonstrate identities
say 6*iSin(0.5)
say 3*iCos(0.5)
say 4*iTan(1)
```

```txt

3.14159265358979323846438729976818601771260734312
3.14159265358979323846157620314930763214337987744
3.14159265358979323846264318595256260456200366896

```



## Tcl

```tcl
#
### Create a thiele-interpretation function with the given name that interpolates
### off the given table.
#
proc thiele {name : X -> F} {
    # Sanity check
    if {[llength $X] != [llength $F]} {
	error "unequal length lists supplied: [llength $X] != [llength $F]"
    }

    #
    ### Compute the table of reciprocal differences
    #
    set p [lrepeat [llength $X] [lrepeat [llength $X] 0.0]]
    set i 0
    foreach x0 [lrange $X 0 end-1] x1 [lrange $X 1 end] \
	    f0 [lrange $F 0 end-1] f1 [lrange $F 1 end] {
	lset p $i 0 $f0
	lset p $i 1 [expr {($x0 - $x1) / ($f0 - $f1)}]
	lset p [incr i] 0 $f1
    }
    for {set j 2} {$j<[llength $X]-1} {incr j} {
	for {set i 0} {$i<[llength $X]-$j} {incr i} {
	    lset p $i $j [expr {
		[lindex $p $i+1 $j-2] +
		([lindex $X $i] - [lindex $X $i+$j]) /
		([lindex $p $i $j-1] - [lindex $p $i+1 $j-1])
	    }]
	}
    }

    #
    ### Make pseudo-curried function that actually evaluates Thiele's formula
    #
    interp alias {} $name {} apply {{X rho f1 x} {
	set a 0.0
	foreach Xi  [lreverse [lrange $X 2 end]] \
		Ri  [lreverse [lrange $rho 2 end]] \
		Ri2 [lreverse [lrange $rho 0 end-2]] {
	    set a [expr {($x - $Xi) / ($Ri - $Ri2 + $a)}]
	}
	expr {$f1 + ($x - [lindex $X 1]) / ([lindex $rho 1] + $a)}
    }} $X [lindex $p 1] [lindex $F 1]
}
```

Demonstration code:

```tcl
proc initThieleTest {} {
    for {set i 0} {$i < 32} {incr i} {
	lappend trigTable(x)   [set x [expr {0.05 * $i}]]
	lappend trigTable(sin) [expr {sin($x)}]
	lappend trigTable(cos) [expr {cos($x)}]
	lappend trigTable(tan) [expr {tan($x)}]
    }

    thiele invSin : $trigTable(sin) -> $trigTable(x)
    thiele invCos : $trigTable(cos) -> $trigTable(x)
    thiele invTan : $trigTable(tan) -> $trigTable(x)
}
initThieleTest
puts "pi estimate using sin interpolation: [expr {6 * [invSin 0.5]}]"
puts "pi estimate using cos interpolation: [expr {3 * [invCos 0.5]}]"
puts "pi estimate using tan interpolation: [expr {4 * [invTan 1.0]}]"
```

Output:

```txt

pi estimate using sin interpolation: 3.1415926535897936
pi estimate using cos interpolation: 3.141592653589793
pi estimate using tan interpolation: 3.141592653589794

```



## zkl

Please see the C example for the comments I've removed (this is an as pure-as-I-make-it translation).

```zkl
const N=32, N2=(N * (N - 1) / 2), STEP=0.05;

fcn rho(xs,ys,rs, i,n){
   if (n < 0) return(0.0);
   if (not n) return(ys[i]);

   idx := (N - 1 - n) * (N - n) / 2 + i;
   if (Void==rs[idx])
      rs[idx] = (xs[i] - xs[i + n])
		/ (rho(xs, ys, rs, i, n - 1) - rho(xs, ys, rs, i + 1, n - 1))
		+ rho(xs, ys, rs, i + 1, n - 2);
   return(rs[idx]);
}

fcn thiele(xs,ys,rs, xin, n){
   if (n > N - 1) return(1.0);
   rho(xs, ys, rs, 0, n) - rho(xs, ys, rs, 0, n - 2)
      + (xin - xs[n]) / thiele(xs, ys, rs, xin, n + 1);
}

///////////

reg t_sin=L(), t_cos=L(), t_tan=L(),
    r_sin=L(), r_cos=L(), r_tan=L(),  xval=L();

i_sin := thiele.fpM("11101",t_sin, xval, r_sin, 0);
i_cos := thiele.fpM("11101",t_cos, xval, r_cos, 0);
i_tan := thiele.fpM("11101",t_tan, xval, r_tan, 0);

foreach i in (N){
   xval.append(x:=STEP*i);
   t_sin.append(x.sin());
   t_cos.append(x.cos());
   t_tan.append(t_sin[i] / t_cos[i]);
}
foreach i in (N2){ r_sin+Void; r_cos+Void; r_tan+Void; }

print("%16.14f\n".fmt( 6.0 * i_sin(0.5)));
print("%16.14f\n".fmt( 3.0 * i_cos(0.5)));
print("%16.14f\n".fmt( 4.0 * i_tan(1.0)));
```

```txt

3.14159265358979
3.14159265358979
3.14159265358979

```



