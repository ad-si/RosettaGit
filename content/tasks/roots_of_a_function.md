+++
title = "Roots of a function"
description = ""
date = 2018-10-15T13:28:39Z
aliases = []
[extra]
id = 2619
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
+++

## Task

Create a program that finds and outputs the roots of a given function, range and (if applicable) step width.

The program should identify whether the root is exact or approximate.


For this task, use:     <big><big> ƒ(x)   =   x<sup>3</sup> - 3x<sup>2</sup> + 2x </big></big>





## Ada


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Roots_Of_Function is
   package Real_Io is new Ada.Text_Io.Float_Io(Long_Float);
   use Real_Io;

   function F(X : Long_Float) return Long_Float is
   begin
      return (X**3 - 3.0*X*X + 2.0*X);
   end F;

   Step  : constant Long_Float := 1.0E-6;
   Start : constant Long_Float := -1.0;
   Stop  : constant Long_Float := 3.0;
   Value : Long_Float := F(Start);
   Sign  : Boolean := Value > 0.0;
   X     : Long_Float := Start + Step;

begin
   if Value = 0.0 then
      Put("Root found at ");
      Put(Item => Start, Fore => 1, Aft => 6, Exp => 0);
      New_Line;
   end if;
   while X <= Stop loop
      Value := F(X);
      if (Value > 0.0) /= Sign then
         Put("Root found near ");
         Put(Item => X, Fore => 1, Aft => 6, Exp => 0);
         New_Line;
      elsif Value = 0.0 then
         Put("Root found at ");
         Put(Item => X, Fore => 1, Aft => 6, Exp => 0);
         New_Line;
      end if;
      Sign := Value > 0.0;
      X := X + Step;
   end loop;
end Roots_Of_Function;
```



## ALGOL 68

Finding 3 roots using the secant method:

```algol68
MODE DBL = LONG REAL;
FORMAT dbl = $g(-long real width, long real width-6, -2)$;

MODE XY = STRUCT(DBL x, y);
FORMAT xy root = $f(dbl)" ("b("Exactly", "Approximately")")"$;

MODE DBLOPT = UNION(DBL, VOID);
MODE XYRES = UNION(XY, VOID);

PROC find root = (PROC (DBL)DBL f, DBLOPT in x1, in x2, in x error, in y error)XYRES:(
  INT limit = ENTIER (long real width / log(2)); # worst case of a binary search) #
  DBL x1 := (in x1|(DBL x1):x1|-5.0), # if x1 is EMPTY then -5.0 #
      x2 := (in x2|(DBL x2):x2|+5.0),
      x error := (in x error|(DBL x error):x error|small real),
      y error := (in y error|(DBL y error):y error|small real);
  DBL y1 := f(x1), y2;
  DBL dx := x1 - x2, dy;

  IF y1 = 0 THEN
    XY(x1, y1) # we already have a solution! #
  ELSE
    FOR i WHILE
      y2 := f(x2);
      IF y2 = 0 THEN stop iteration FI;
      IF i = limit THEN value error FI;
      IF y1 = y2 THEN value error FI;
      dy := y1 - y2;
      dx := dx / dy * y2;
      x1 := x2; y1 := y2; # retain for next iteration #
      x2 -:= dx;
  # WHILE # ABS dx > x error AND ABS dy > y error DO
      SKIP
    OD;
    stop iteration:
      XY(x2, y2) EXIT
    value error:
      EMPTY
  FI
);

PROC f = (DBL x)DBL: x UP 3 - LONG 3.1 * x UP 2 + LONG 2.0 * x;

DBL first root, second root, third root;

XYRES first result = find root(f, LENG -1.0, LENG 3.0, EMPTY, EMPTY);
CASE first result IN
  (XY first result): (
    printf(($"1st root found at x = "f(xy root)l$, x OF first result, y OF first result=0));
    first root := x OF first result
  )
  OUT printf($"No first root found"l$); stop
ESAC;

XYRES second result = find root( (DBL x)DBL: f(x) / (x - first root), EMPTY, EMPTY, EMPTY, EMPTY);
CASE second result IN
  (XY second result): (
    printf(($"2nd root found at x = "f(xy root)l$, x OF second result, y OF second result=0));
    second root := x OF second result
  )
  OUT printf($"No second root found"l$); stop
ESAC;

XYRES third result = find root( (DBL x)DBL: f(x) / (x - first root) / ( x - second root ), EMPTY, EMPTY, EMPTY, EMPTY);
CASE third result IN
  (XY third result): (
    printf(($"3rd root found at x = "f(xy root)l$, x OF third result, y OF third result=0));
    third root := x OF third result
  )
  OUT printf($"No third root found"l$); stop
ESAC
```

Output:

```txt
1st root found at x =  9.1557112297752398099031e-1 (Approximately)
2nd root found at x =  2.1844288770224760190097e 0 (Approximately)
3rd root found at x =  0.0000000000000000000000e 0 (Exactly)

```



## ATS


```ATS

#include
"share/atspre_staload.hats"

typedef d = double

fun
findRoots
(
  start: d, stop: d, step: d, f: (d) -> d, nrts: int, A: d
) : void = (
//
if
start < stop
then let
  val A2 = f(start)
  var nrts: int = nrts
  val () =
  if A2 = 0.0
    then (
      nrts := nrts + 1;
      $extfcall(void, "printf", "An exact root is found at %12.9f\n", start)
    ) (* end of [then] *)
  // end of [if]
  val () =
  if A * A2 < 0.0
    then (
      nrts := nrts + 1;
      $extfcall(void, "printf", "An approximate root is found at %12.9f\n", start)
    ) (* end of [then] *)
  // end of [if]
in
  findRoots(start+step, stop, step, f, nrts, A2)
end // end of [then]
else (
  if nrts = 0
    then $extfcall(void, "printf", "There are no roots found!\n")
  // end of [if]
) (* end of [else] *)
//
) (* end of [findRoots] *)

(* ****** ****** *)

implement
main0 () =
findRoots (~1.0, 3.0, 0.001, lam (x) => x*x*x - 3.0*x*x + 2.0*x, 0, 0.0)

```



## AutoHotkey

Poly(x) is a test function of one variable, here we are searching for its roots:
* roots() searches for intervals within given limits, shifted by a given “step”, where our function has different signs at the endpoints.
* Having found such an interval, the root() function searches for a value where our function is 0, within a given tolerance.
* It also sets ErrorLevel to info about the root found.

[http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=139 discussion]

```autohotkey
MsgBox % roots("poly", -0.99, 2, 0.1, 1.0e-5)
MsgBox % roots("poly", -1, 3, 0.1, 1.0e-5)

roots(f,x1,x2,step,tol) { ; search for roots in intervals of length "step", within tolerance "tol"
   x := x1, y := %f%(x), s := (y>0)-(y<0)
   Loop % ceil((x2-x1)/step) {
      x += step, y := %f%(x), t := (y>0)-(y<0)
      If (s=0 || s!=t)
         res .= root(f, x-step, x, tol) " [" ErrorLevel "]`n"
      s := t
   }
   Sort res, UN ; remove duplicate endpoints
   Return res
}

root(f,x1,x2,d) { ; find x in [x1,x2]: f(x)=0 within tolerance d, by bisection
   If (!y1 := %f%(x1))
      Return x1, ErrorLevel := "Exact"
   If (!y2 := %f%(x2))
      Return x2, ErrorLevel := "Exact"
   If (y1*y2>0)
      Return "", ErrorLevel := "Need different sign ends!"
   Loop {
      x := (x2+x1)/2, y := %f%(x)
      If (y = 0 || x2-x1 < d)
         Return x, ErrorLevel := y ? "Approximate" : "Exact"
      If ((y>0) = (y1>0))
         x1 := x, y1 := y
      Else
         x2 := x, y2 := y
   }
}

poly(x) {
   Return ((x-3)*x+2)*x
}
```


## Axiom

Using a polynomial solver:

```Axiom
expr := x^3-3*x^2+2*x
solve(expr,x)
```

Output:

```Axiom
  (1)  [x= 2,x= 1,x= 0]
                          Type: List(Equation(Fraction(Polynomial(Integer))))
```

Using the secant method in the interpreter:

```Axiom
digits(30)
secant(eq: Equation Expression Float, binding: SegmentBinding(Float)):Float ==
  eps := 1.0e-30
  expr := lhs eq - rhs eq
  x := variable binding
  seg := segment binding
  x1 := lo seg
  x2 := hi seg
  fx1 := eval(expr, x=x1)::Float
  abs(fx1)<eps => return x1
  for i in 1..100 repeat
    fx2 := eval(expr, x=x2)::Float
    abs(fx2)<eps => return x2
    (x1, fx1, x2) := (x2, fx2, x2 - fx2 * (x2 - x1) / (fx2 - fx1))
  error "Function not converging."
```

The example can now be called using:

```Axiom
secant(expr=0,x=-0.5..0.5)
```



## BBC BASIC


```bbcbasic
      function$ = "x^3-3*x^2+2*x"
      rangemin = -1
      rangemax = 3
      stepsize = 0.001
      accuracy = 1E-8
      PROCroots(function$, rangemin, rangemax, stepsize, accuracy)
      END

      DEF PROCroots(func$, min, max, inc, eps)
      LOCAL x, sign%, oldsign%
      oldsign% = 0
      FOR x = min TO max STEP inc
        sign% = SGN(EVAL(func$))
        IF sign% = 0 THEN
          PRINT "Root found at x = "; x
          sign% = -oldsign%
        ELSE IF sign% <> oldsign% AND oldsign% <> 0 THEN
            IF inc < eps THEN
              PRINT "Root found near x = "; x
            ELSE
              PROCroots(func$, x-inc, x+inc/8, inc/8, eps)
            ENDIF
          ENDIF
        ENDIF
        oldsign% = sign%
      NEXT x
      ENDPROC
```

Output:

```txt
Root found near x = 2.29204307E-9
Root found near x = 1
Root found at x = 2
```



## C



###  Secant Method



```c
#include <math.h>
#include <stdio.h>

double f(double x)
{
    return x*x*x-3.0*x*x +2.0*x;
}

double secant( double xA, double xB, double(*f)(double) )
{
    double e = 1.0e-12;
    double fA, fB;
    double d;
    int i;
    int limit = 50;

    fA=(*f)(xA);
    for (i=0; i<limit; i++) {
        fB=(*f)(xB);
        d = (xB - xA) / (fB - fA) * fB;
        if (fabs(d) < e)
            break;
        xA = xB;
        fA = fB;
        xB -= d;
    }
    if (i==limit) {
        printf("Function is not converging near (%7.4f,%7.4f).\n", xA,xB);
        return -99.0;
    }
    return xB;
}

int main(int argc, char *argv[])
{
    double step = 1.0e-2;
    double e = 1.0e-12;
    double x = -1.032;		// just so we use secant method
    double xx, value;

    int s = (f(x)> 0.0);

    while (x < 3.0) {
        value = f(x);
        if (fabs(value) < e) {
            printf("Root found at x= %12.9f\n", x);
            s = (f(x+.0001)>0.0);
        }
        else if ((value > 0.0) != s) {
            xx = secant(x-step, x,&f);
            if (xx != -99.0)   // -99 meaning secand method failed
                printf("Root found at x= %12.9f\n", xx);
            else
                printf("Root found near x= %7.4f\n", x);
            s = (f(x+.0001)>0.0);
        }
        x += step;
    }
    return 0;
}
```



###  GNU Scientific Library



```C
#include <gsl/gsl_poly.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    /* 0 + 2x - 3x^2 + 1x^3 */
    double p[] = {0, 2, -3, 1};
    double z[6];
    gsl_poly_complex_workspace *w = gsl_poly_complex_workspace_alloc(4);
    gsl_poly_complex_solve(p, 4, w, z);
    gsl_poly_complex_workspace_free(w);

    for(int i = 0; i < 3; ++i)
        printf("%.12f\n", z[2 * i]);

    return 0;
}
```


One can also use the GNU Scientific Library to find roots of functions. Compile with
```txt
gcc roots.c -lgsl -lcblas -o roots
```



## C++


```cpp
#include <iostream>

double f(double x)
{
	return (x*x*x - 3*x*x + 2*x);
}

int main()
{
	double step = 0.001; // Smaller step values produce more accurate and precise results
	double start = -1;
	double stop = 3;
	double value = f(start);
	double sign = (value > 0);

	// Check for root at start
	if ( 0 == value )
		std::cout << "Root found at " << start << std::endl;

	for(	double x = start + step;
			x <= stop;
			x += step )
	{
		value = f(x);

		if ( ( value > 0 ) != sign )
			// We passed a root
			std::cout << "Root found near " << x << std::endl;
		else if ( 0 == value )
			// We hit a root
			std::cout << "Root found at " << x << std::endl;

		// Update our sign
		sign = ( value > 0 );
	}
}
```


===Brent's Method===
Brent's Method uses a combination of the bisection method, inverse quadratic interpolation, and the secant method to find roots. It has a guaranteed run time equal to that of the bisection method (which always converges in a known number of steps (log2[(upper_bound-lower_bound)/tolerance] steps to be precise ) unlike the other methods), but the algorithm uses the much faster inverse quadratic interpolation and secant method whenever possible. The algorithm is robust and commonly used in libraries with a roots() function built in.

The algorithm is coded as a function that returns a double value for the root. The function takes an input that requires the function being evaluated, the lower and upper bounds, the tolerance one is looking for before converging (i recommend 0.0001) and the maximum number of iterations before giving up on finding the root (the root will always be found if the root is bracketed and a sufficient number of iterations is allowed).

The implementation is taken from the pseudo code on the wikipedia page for Brent's Method found here: https://en.wikipedia.org/wiki/Brent%27s_method.

```cpp
#include <iostream>
#include <cmath>
#include <algorithm>
#include <functional>

double brents_fun(std::function<double (double)> f, double lower, double upper, double tol, unsigned int max_iter)
{
	double a = lower;
	double b = upper;
	double fa = f(a);	// calculated now to save function calls
	double fb = f(b);	// calculated now to save function calls
	double fs = 0;		// initialize

	if (!(fa * fb < 0))
	{
		std::cout << "Signs of f(lower_bound) and f(upper_bound) must be opposites" << std::endl; // throws exception if root isn't bracketed
		return -11;
	}

	if (std::abs(fa) < std::abs(b))	// if magnitude of f(lower_bound) is less than magnitude of f(upper_bound)
	{
		std::swap(a,b);
		std::swap(fa,fb);
	}

	double c = a;			// c now equals the largest magnitude of the lower and upper bounds
	double fc = fa;			// precompute function evalutation for point c by assigning it the same value as fa
	bool mflag = true;		// boolean flag used to evaluate if statement later on
	double s = 0;			// Our Root that will be returned
	double d = 0;			// Only used if mflag is unset (mflag == false)

	for (unsigned int iter = 1; iter < max_iter; ++iter)
	{
		// stop if converged on root or error is less than tolerance
		if (std::abs(b-a) < tol)
		{
			std::cout << "After " << iter << " iterations the root is: " << s << std::endl;
			return s;
		} // end if

		if (fa != fc && fb != fc)
		{
			// use inverse quadratic interopolation
			s =	  ( a * fb * fc / ((fa - fb) * (fa - fc)) )
				+ ( b * fa * fc / ((fb - fa) * (fb - fc)) )
				+ ( c * fa * fb / ((fc - fa) * (fc - fb)) );
		}
		else
		{
			// secant method
			s = b - fb * (b - a) / (fb - fa);
		}

			// checks to see whether we can use the faster converging quadratic && secant methods or if we need to use bisection
		if (	( (s < (3 * a + b) * 0.25) || (s > b) ) ||
				( mflag && (std::abs(s-b) >= (std::abs(b-c) * 0.5)) ) ||
				( !mflag && (std::abs(s-b) >= (std::abs(c-d) * 0.5)) ) ||
				( mflag && (std::abs(b-c) < tol) ) ||
				( !mflag && (std::abs(c-d) < tol))	)
		{
			// bisection method
			s = (a+b)*0.5;

			mflag = true;
		}
		else
		{
			mflag = false;
		}

		fs = f(s);	// calculate fs
		d = c;		// first time d is being used (wasnt used on first iteration because mflag was set)
		c = b;		// set c equal to upper bound
		fc = fb;	// set f(c) = f(b)

		if ( fa * fs < 0)	// fa and fs have opposite signs
		{
			b = s;
			fb = fs;	// set f(b) = f(s)
		}
		else
		{
			a = s;
			fa = fs;	// set f(a) = f(s)
		}

		if (std::abs(fa) < std::abs(fb)) // if magnitude of fa is less than magnitude of fb
		{
			std::swap(a,b);		// swap a and b
			std::swap(fa,fb);	// make sure f(a) and f(b) are correct after swap
		}

	} // end for

	std::cout<< "The solution does not converge or iterations are not sufficient" << std::endl;

} // end brents_fun


```



## Clojure


```Clojure


(defn findRoots [f start stop step eps]
      (filter #(-> (f %) Math/abs (< eps)) (range start stop step)))

```



```txt

> (findRoots #(+ (* % % %) (* -3 % %) (* 2 %)) -1.0 3.0 0.0001 0.00000001)
(-9.381755897326649E-14 0.9999999999998124 1.9999999999997022)

```



## CoffeeScript

```coffeescript

print_roots = (f, begin, end, step) ->
  # Print approximate roots of f between x=begin and x=end,
  # using sign changes as an indicator that a root has been
  # encountered.
  x = begin
  y = f(x)
  last_y = y

  cross_x_axis = ->
    (last_y < 0 and y > 0) or (last_y > 0 and y < 0)

  console.log '-----'
  while x <= end
    y = f(x)
    if y == 0
      console.log "Root found at", x
    else if cross_x_axis()
      console.log "Root found near", x
    x += step
    last_y = y

do ->
  # Smaller steps produce more accurate/precise results in general,
  # but for many functions we'll never get exact roots, either due
  # to imperfect binary representation or irrational roots.
  step = 1 / 256

  f1 = (x) -> x*x*x - 3*x*x + 2*x
  print_roots f1, -1, 5, step
  f2 = (x) -> x*x - 4*x + 3
  print_roots f2, -1, 5, step
  f3 = (x) -> x - 1.5
  print_roots f3, 0, 4, step
  f4 = (x) -> x*x - 2
  print_roots f4, -2, 2, step

```


output

<lang>
> coffee roots.coffee
-----
Root found at 0
Root found at 1
Root found at 2
-----
Root found at 1
Root found at 3
-----
Root found at 1.5
-----
Root found near -1.4140625
Root found near 1.41796875

```



## Common Lisp


<code>find-roots</code> prints roots (and values near roots) and returns a list of root designators, each of which is either a number <code><var>n</var></code>, in which case <code>(zerop (funcall function <var>n</var>))</code> is true, or a <code>cons</code> whose <code>car</code> and <code>cdr</code> are such that the sign of function at car and cdr changes.


```lisp
(defun find-roots (function start end &optional (step 0.0001))
  (let* ((roots '())
         (value (funcall function start))
         (plusp (plusp value)))
    (when (zerop value)
      (format t "~&Root found at ~W." start))
    (do ((x (+ start step) (+ x step)))
        ((> x end) (nreverse roots))
      (setf value (funcall function x))
      (cond
       ((zerop value)
        (format t "~&Root found at ~w." x)
        (push x roots))
       ((not (eql plusp (plusp value)))
        (format t "~&Root found near ~w." x)
        (push (cons (- x step) x) roots)))
      (setf plusp (plusp value)))))
```



```txt
> (find-roots #'(lambda (x) (+ (* x x x) (* -3 x x) (* 2 x))) -1 3)
Root found near 5.3588345E-5.
Root found near 1.0000072.
Root found near 2.000073.
((-4.6411653E-5 . 5.3588345E-5)
 (0.99990714 . 1.0000072)
 (1.9999729 . 2.000073))
```



## D


```d
import std.stdio, std.math, std.algorithm;

bool nearZero(T)(in T a, in T b = T.epsilon * 4) pure nothrow {
    return abs(a) <= b;
}

T[] findRoot(T)(immutable T function(in T) pure nothrow fi,
                in T start, in T end, in T step=T(0.001L),
                T tolerance = T(1e-4L)) {
    if (step.nearZero)
        writefln("WARNING: step size may be too small.");

    /// Search root by simple bisection.
    T searchRoot(T a, T b) pure nothrow {
        T root;
        int limit = 49;
        T gap = b - a;

        while (!nearZero(gap) && limit--) {
            if (fi(a).nearZero)
                return a;
            if (fi(b).nearZero)
                return b;
            root = (b + a) / 2.0L;
            if (fi(root).nearZero)
                return root;
            ((fi(a) * fi(root) < 0) ? b : a) = root;
            gap = b - a;
        }

        return root;
    }

    immutable dir = T(end > start ? 1.0 : -1.0);
    immutable step2 = (end > start) ? abs(step) : -abs(step);
    T[T] result;
    for (T x = start; (x * dir) <= (end * dir); x += step2)
        if (fi(x) * fi(x + step2) <= 0) {
            immutable T r = searchRoot(x, x + step2);
            result[r] = fi(r);
        }

    return result.keys.sort().release;
}

void report(T)(in T[] r, immutable T function(in T) pure f,
               in T tolerance = T(1e-4L)) {
    if (r.length) {
        writefln("Root found (tolerance = %1.4g):", tolerance);

        foreach (const x; r) {
            immutable T y = f(x);

            if (nearZero(y))
                writefln("... EXACTLY at %+1.20f, f(x) = %+1.4g",x,y);
            else if (nearZero(y, tolerance))
                writefln(".... MAY-BE at %+1.20f, f(x) = %+1.4g",x,y);
            else
                writefln("Verify needed, f(%1.4g) = " ~
                         "%1.4g > tolerance in magnitude", x, y);
        }
    } else
        writefln("No root found.");
}

void main() {
    static real f(in real x) pure nothrow {
        return x ^^ 3 - (3 * x ^^ 2) + 2 * x;
    }

    findRoot(&f, -1.0L, 3.0L, 0.001L).report(&f);
}
```

```txt
Root found (tolerance = 0.0001):
.... MAY-BE at -0.00000000000000000080, f(x) = -1.603e-18
... EXACTLY at +1.00000000000000000020, f(x) = -2.168e-19
.... MAY-BE at +1.99999999999999999950, f(x) = -8.674e-19
```

NB: smallest increment for real type in D is real.epsilon = 1.0842e-19.


## Dart

```dart
double fn(double x) => x * x * x - 3 * x * x + 2 * x;

findRoots(Function(double) f, double start, double stop, double step, double epsilon) sync* {
  for (double x = start; x < stop; x = x + step) {
    if (fn(x).abs() < epsilon) yield x;
  }
}

main() {
  // Vector(-9.381755897326649E-14, 0.9999999999998124, 1.9999999999997022)
  print(findRoots(fn, -1.0, 3.0, 0.0001, 0.000000001));
}
```



## DWScript

```delphi
type TFunc = function (x : Float) : Float;

function f(x : Float) : Float;
begin
   Result := x*x*x-3.0*x*x +2.0*x;
end;

const e = 1.0e-12;

function Secant(xA, xB : Float; f : TFunc) : Float;
const
   limit = 50;
var
   fA, fB : Float;
   d : Float;
   i : Integer;
begin
   fA := f(xA);
   for i := 0 to limit do begin
      fB := f(xB);
      d := (xB-xA)/(fB-fA)*fB;
      if Abs(d) < e then
         Exit(xB);
      xA := xB;
      fA := fB;
      xB -= d;
   end;
   PrintLn(Format('Function is not converging near (%7.4f,%7.4f).', [xA, xB]));
   Result := -99.0;
end;

const fstep = 1.0e-2;

var x := -1.032;		// just so we use secant method
var xx, value : Float;
var s := f(x)>0.0;

while (x < 3.0) do begin
   value := f(x);
   if Abs(value)<e then begin
      PrintLn(Format("Root found at x= %12.9f", [x]));
      s := (f(x+0.0001)>0.0);
   end else if (value>0.0) <> s then begin
      xx := Secant(x-fstep, x, f);
      if xx <> -99.0 then   // -99 meaning secand method failed
         PrintLn(Format('Root found at x = %12.9f', [xx]))
      else PrintLn(Format('Root found near x= %7.4f', [xx]));
      s := (f(x+0.0001)>0.0);
   end;
   x += fstep;
end;
```



## EchoLisp

We use the 'math' library, and define f(x) as the polynomial : x<sup>3</sup> -3x<sup>2</sup> +2x


```lisp

(lib 'math.lib)
Lib: math.lib loaded.
(define fp ' ( 0 2 -3 1))
(poly->string 'x fp) → x^3 -3x^2 +2x
(poly->html 'x fp) → x<sup>3</sup> -3x<sup>2</sup> +2x
(define (f x) (poly x fp))
(math-precision 1.e-6) → 0.000001

(root f -1000 1000) → 2.0000000133245677 ;; 2
(root f -1000 (- 2 epsilon)) → 1.385559938161431e-7 ;; 0
(root f epsilon (- 2 epsilon)) → 1.0000000002190812 ;; 1

```



## Elixir

```elixir
defmodule RC do
  def find_roots(f, range, step \\ 0.001) do
    first .. last = range
    max = last + step / 2
    Stream.iterate(first, &(&1 + step))
    |> Stream.take_while(&(&1 < max))
    |> Enum.reduce(sign(first), fn x,sn ->
         value = f.(x)
         cond do
           abs(value) < step / 100 ->
             IO.puts "Root found at #{x}"
             0
           sign(value) == -sn ->
             IO.puts "Root found between #{x-step} and #{x}"
             -sn
           true -> sign(value)
         end
       end)
  end

  defp sign(x) when x>0, do: 1
  defp sign(x) when x<0, do: -1
  defp sign(0)         , do: 0
end

f = fn x -> x*x*x - 3*x*x + 2*x end
RC.find_roots(f, -1..3)
```


```txt

Root found at 8.81239525796218e-16
Root found at 1.0000000000000016
Root found at 1.9999999999998914

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(roots).
-export([main/0]).
main() ->
	F = fun(X)->X*X*X - 3*X*X + 2*X end,
	Step = 0.001,	 % Using smaller steps will provide more accurate results
	Start = -1,
	Stop = 3,
	Sign = F(Start) > 0,
	X = Start,
	while(X, Step, Start, Stop, Sign,F).

while(X, Step, Start, Stop, Sign,F) ->
	Value = F(X),
	if
		Value == 0  ->		% We hit a root
        	io:format("Root found at ~p~n",[X]),
        	while(X+Step, Step, Start, Stop,  Value > 0,F);

		(Value < 0) == Sign ->	% We passed a root
		io:format("Root found near ~p~n",[X]),
		while(X+Step , Step, Start, Stop,  Value > 0,F);

		 X > Stop ->
		 	io:format("") ;
		true ->
        	while(X+Step, Step, Start, Stop,  Value > 0,F)
	end.

```

```txt
Root found near 8.81239525796218e-16
Root found near 1.0000000000000016
Root found near 2.0009999999998915
ok
```



## ERRE


```ERRE

PROGRAM ROOTS_FUNCTION

!VAR E,X,STP,VALUE,S%,I%,LIMIT%,X1,X2,D

FUNCTION F(X)
    F=X*X*X-3*X*X+2*X
END FUNCTION

BEGIN
  X=-1
  STP=1.0E-6
  E=1.0E-9
  S%=(F(X)>0)

  PRINT("VERSION 1: SIMPLY STEPPING X")
  WHILE X<3.0 DO
    VALUE=F(X)
    IF ABS(VALUE)<E THEN
        PRINT("ROOT FOUND AT X =";X)
        S%=NOT S%
      ELSE
        IF ((VALUE>0)<>S%) THEN
          PRINT("ROOT FOUND AT X =";X)
          S%=NOT S%
        END IF
    END IF
    X=X+STP
  END WHILE

  PRINT
  PRINT("VERSION 2: SECANT METHOD")
  X1=-1.0
  X2=3.0
  E=1.0E-15
  I%=1
  LIMIT%=300
  LOOP
    IF I%>LIMIT% THEN
       PRINT("ERROR: FUNCTION NOT CONVERGING")
       EXIT
    END IF
    D=(X2-X1)/(F(X2)-F(X1))*F(X2)
    IF ABS(D)<E THEN
      IF D=0 THEN
         PRINT("EXACT ";)
       ELSE
         PRINT("APPROXIMATE ";)
      END IF
      PRINT("ROOT FOUND AT X =";X2)
      EXIT
    END IF
    X1=X2
    X2=X2-D
    I%=I%+1
  END LOOP
END PROGRAM

```

Note: Outputs are calculated in single precision.
```txt

VERSION 1: SIMPLY STEPPING X
ROOT FOUND AT X = 8.866517E-07
ROOT FOUND AT X = 1.000001
ROOT FOUND AT X = 2

VERSION 2: SECANT METHOD
EXACT ROOT FOUND AT X = 1

```



## Fortran

```fortran
PROGRAM ROOTS_OF_A_FUNCTION

  IMPLICIT NONE

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15)
  REAL(dp) :: f, e, x, step, value
  LOGICAL :: s

  f(x) = x*x*x - 3.0_dp*x*x + 2.0_dp*x

  x = -1.0_dp ; step = 1.0e-6_dp ; e = 1.0e-9_dp

  s = (f(x) > 0)
  DO WHILE (x < 3.0)
    value = f(x)
    IF(ABS(value) < e) THEN
      WRITE(*,"(A,F12.9)") "Root found at x =", x
      s = .NOT. s
    ELSE IF ((value > 0) .NEQV. s) THEN
      WRITE(*,"(A,F12.9)") "Root found near x = ", x
      s = .NOT. s
    END IF
    x = x + step
  END DO

END PROGRAM ROOTS_OF_A_FUNCTION
```

The following approach uses the [[wp:Secant_method|Secant Method]] to numerically find one root. Which root is found will depend on the start values x1 and x2 and if these are far from a root this method may not converge.

```fortran
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15)
INTEGER :: i=1, limit=100
REAL(dp) :: d, e, f, x, x1, x2

f(x) = x*x*x - 3.0_dp*x*x + 2.0_dp*x

x1 = -1.0_dp ; x2 = 3.0_dp ; e = 1.0e-15_dp

DO
  IF (i > limit) THEN
    WRITE(*,*) "Function not converging"
    EXIT
  END IF
  d = (x2 - x1) / (f(x2) - f(x1)) * f(x2)
  IF (ABS(d) < e) THEN
    WRITE(*,"(A,F18.15)") "Root found at x = ", x2
    EXIT
  END IF
  x1 = x2
  x2 = x2 - d
  i = i + 1
END DO
```



## Go

Secant method.  No error checking.

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	example := func(x float64) float64 { return x*x*x - 3*x*x + 2*x }
	findroots(example, -.5, 2.6, 1)
}

func findroots(f func(float64) float64, lower, upper, step float64) {
	for x0, x1 := lower, lower+step; x0 < upper; x0, x1 = x1, x1+step {
		x1 = math.Min(x1, upper)
		r, status := secant(f, x0, x1)
		if status != "" && r >= x0 && r < x1 {
			fmt.Printf("  %6.3f %s\n", r, status)
		}
	}
}

func secant(f func(float64) float64, x0, x1 float64) (float64, string) {
	var f0 float64
	f1 := f(x0)
	for i := 0; i < 100; i++ {
		f0, f1 = f1, f(x1)
		switch {
		case f1 == 0:
			return x1, "exact"
		case math.Abs(x1-x0) < 1e-6:
			return x1, "approximate"
		}
		x0, x1 = x1, x1-f1*(x1-x0)/(f1-f0)
	}
	return 0, ""
}
```

Output:

```txt

   0.000 approximate
   1.000 exact
   2.000 approximate

```



## Haskell


```haskell
f x = x^3-3*x^2+2*x

findRoots start stop step eps =
  [x | x <- [start, start+step .. stop], abs (f x) < eps]
```

Executed in GHCi:

```haskell
*Main> findRoots (-1.0) 3.0 0.0001 0.000000001
[-9.381755897326649e-14,0.9999999999998124,1.9999999999997022]
```


Or using package [http://hackage.haskell.org/package/hmatrix hmatrix] from HackageDB.

```haskell
import Numeric.GSL.Polynomials
import Data.Complex

*Main> mapM_ print $ polySolve [0,2,-3,1]
(-5.421010862427522e-20) :+ 0.0
2.000000000000001 :+ 0.0
0.9999999999999996 :+ 0.0
```

No complex roots, so:

```haskell
*Main> mapM_ (print.realPart) $ polySolve [0,2,-3,1]
-5.421010862427522e-20
2.000000000000001
0.9999999999999996
```


It is possible to solve the problem directly and elegantly using robust bisection method and Alternative type class.

```haskell
import Control.Applicative

data Root a = Exact a | Approximate a deriving (Show, Eq)

-- looks for roots on an interval
bisection :: (Alternative f, Floating a, Ord a) =>
             (a -> a) -> a -> a -> f (Root a)
bisection f a b | f a * f b > 0 = empty
                | f a == 0      = pure (Exact a)
                | f b == 0      = pure (Exact b)
                | smallInterval = pure (Approximate c)
                | otherwise     = bisection f a c <|> bisection f c b
  where c = (a + b) / 2
        smallInterval = abs (a-b) < 1e-15 || abs ((a-b)/c) < 1e-15

-- looks for roots on a grid
findRoots :: (Alternative f, Floating a, Ord a) =>
             (a -> a) -> [a] -> а (Root a)
findRoots f []       = empty
findRoots f [x]      = if f x == 0 then pure (Exact x) else empty
findRoots f (a:b:xs) = bisection f a b <|> findRoots f (b:xs)
```


It is possible to use these functions with different Alternative functors: IO, Maybe or List:

```txt
λ> bisection (\x -> x*x-2) 1 2
Approximate 1.414213562373094
λ> bisection (\x -> x-1) 1 2
Exact 1.0
λ> bisection (\x -> x*x-2) 2 3 :: Maybe (Root Double)
Nothing
λ> findRoots (\x ->  x^3 - 3*x^2 + 2*x) [-3..3] :: Maybe (Root Double)
Just (Exact 0.0)
λ> findRoots (\x ->  x^3 - 3*x^2 + 2*x) [-3..3] :: [Root Double]
[Exact 0.0,Exact 0.0,Exact 1.0,Exact 2.0]
```


To get rid of repeated roots use `Data.List.nub`

```txt
λ> Data.List.nub $ findRoots (\x ->  x^3 - 3*x^2 + 2*x) [-3..3]
[Exact 0.0,Exact 1.0,Exact 2.0]
λ> Data.List.nub $ findRoots (\x ->  x^3 - 3*x^2 + x) [-3..3]
[Exact 0.0,Approximate 2.6180339887498967]
```



## HicEst

HicEst's [http://www.HicEst.com/SOLVE.htm SOLVE] function employs the Levenberg-Marquardt method:

```HicEst
OPEN(FIle='test.txt')

1 DLG(NameEdit=x0, DNum=3)

x = x0
chi2 = SOLVE(NUL=x^3 - 3*x^2 + 2*x, Unknown=x, I=iterations, NumDiff=1E-15)
EDIT(Text='approximate exact ', Word=(chi2 == 0), Parse=solution)

WRITE(FIle='test.txt', LENgth=6, Name) x0, x, solution, chi2, iterations
GOTO 1
```


```HicEst
x0=0.5; x=1; solution=exact; chi2=79E-32 iterations=65;
x0=0.4; x=2E-162 solution=exact; chi2=0; iterations=1E4;
x0=0.45; x=1; solution=exact; chi2=79E-32 iterations=67;
x0=0.42; x=2E-162 solution=exact; chi2=0; iterations=1E4;
x0=1.5; x=1.5; solution=approximate; chi2=0.1406; iterations=14:
x0=1.54; x=1; solution=exact; chi2=44E-32 iterations=63;
x0=1.55; x=2; solution=exact; chi2=79E-32 iterations=55;
x0=1E10; x=2; solution=exact; chi2=18E-31 iterations=511;
x0=-1E10; x=0; solution=exact; chi2=0; iterations=1E4;
```


=={{header|Icon}} and {{header|Unicon}}==
Works in both languages:

```unicon
procedure main()
    showRoots(f, -1.0, 4, 0.002)
end

procedure f(x)
    return x^3 - 3*x^2 + 2*x
end

procedure showRoots(f, lb, ub, step)
    ox := x := lb
    oy := f(x)
    os := sign(oy)
    while x <= ub do {
        if (s := sign(y := f(x))) = 0 then write(x)
        else if s ~= os then {
            dx := x-ox
            dy := y-oy
            cx := x-dx*(y/dy)
            write("~",cx)
            }
        (ox := x, oy := y, os := s)
        x +:= step
        }
end

procedure sign(x)
    return (x<0, -1) | (x>0, 1) | 0
end
```


Output:

```txt

->roots
~2.616794878713638e-18
~1.0
~2.0
->

```



## J


J has builtin a root-finding operator, '''<tt>p.</tt>''', whose input is the coeffiecients of the polynomial (where the exponent of the indeterminate variable matches the index of the coefficient: 0 1 2 would be 0 + x + (2 times x squared)).  Hence:


```j
   1{::p.  0 2 _3 1
2 1 0
```


We can determine whether the roots are exact or approximate by evaluating the polynomial at the candidate roots, and testing for zero:


```j
   (0=]p.1{::p.) 0 2 _3 1
1 1 1
```


As you can see, <tt>p.</tt> is also the operator which evaluates polynomials.   This is not a coincidence.

That said, we could also implement the technique used by most others here.  Specifically: we can implement the function as a black box and check every 1 millionth of a unit between minus one and three, and we can test that result for exactness.


```J
   blackbox=: 0 2 _3 1&p.
   (#~ (=<./)@:|@blackbox) i.&.(1e6&*)&.(1&+) 3
0 1 2
   0=blackbox 0 1 2
1 1 1
```


Here, we see that each of the results (0, 1 and 2) are as accurate as we expect our computer arithmetic to be.  (The = returns 1 where paired values are equal and 0 where they are not equal).


## Java


```java
public class Roots {
    public interface Function {
	public double f(double x);
    }

    private static int sign(double x) {
	return (x < 0.0) ? -1 : (x > 0.0) ? 1 : 0;
    }

    public static void printRoots(Function f, double lowerBound,
				  double upperBound, double step) {
	double x = lowerBound, ox = x;
	double y = f.f(x), oy = y;
	int s = sign(y), os = s;

	for (; x <= upperBound ; x += step) {
	    s = sign(y = f.f(x));
	    if (s == 0) {
		System.out.println(x);
	    } else if (s != os) {
		double dx = x - ox;
		double dy = y - oy;
		double cx = x - dx * (y / dy);
		System.out.println("~" + cx);
	    }
	    ox = x; oy = y; os = s;
	}
    }

    public static void main(String[] args) {
	Function poly = new Function () {
	    public double f(double x) {
		return x*x*x - 3*x*x + 2*x;
	    }
	};
	printRoots(poly, -1.0, 4, 0.002);
    }
}
```

Produces this output:

```txt
~2.616794878713638E-18
~1.0000000000000002
~2.000000000000001
```



## JavaScript

```javascript

// This function notation is sorta new, but useful here
// Part of the EcmaScript 6 Draft
// developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions_and_function_scope
var poly = (x => x*x*x - 3*x*x + 2*x);

function sign(x) {
	return (x < 0.0) ? -1 : (x > 0.0) ? 1 : 0;
}

function printRoots(f, lowerBound, upperBound, step) {
	var  x = lowerBound, ox = x,
		 y = f(x), oy = y,
		 s = sign(y), os = s;

	for (; x <= upperBound ; x += step) {
	    s = sign(y = f(x));
	    if (s == 0) {
			console.log(x);
	    }
	    else if (s != os) {
			var dx = x - ox;
			var dy = y - oy;
			var cx = x - dx * (y / dy);
			console.log("~" + cx);
	    }
	    ox = x; oy = y; os = s;
	}
}

printRoots(poly, -1.0, 4, 0.002);

```



## jq

printRoots(f; lower; upper; step) finds approximations to the roots
of an arbitrary continuous real-valued function, f, in the range
[lower, upper], assuming step is small enough.

The algorithm is similar to that used for example in the Javascript section on this page, except that a bug has been removed at the point when the previous and current signs are compared.

The function, f, may be an expression (as in the example below) or a defined filter.

printRoots/3 emits an array of results, each of which is either a
number (representing an exact root within the limits of machine arithmetic) or a string consisting of "~" followed by an approximation to the root.

```jq
def sign:
  if . < 0 then -1 elif . > 0 then 1 else 0 end;

def printRoots(f; lowerBound; upperBound; step):
 lowerBound as $x
 | ($x|f) as $y
 | ($y|sign) as $s
 | reduce range($x; upperBound+step; step) as $x
   # state: [ox, oy, os, roots]
   ( [$x, $y, $s, [] ];
     .[0] as $ox | .[1] as $oy | .[2] as $os
     | ($x|f) as $y
     | ($y | sign) as $s
     | if $s == 0 then  [$x, $y, $s, (.[3] + [$x] )]
       elif $s != $os and $os != 0 then
	  ($x - $ox) as $dx
          | ($y - $oy) as $dy
    	  | ($x - ($dx *  $y / $dy)) as $cx       # by geometry
          | [$x, $y, $s, (.[3] + [ "~\($cx)" ])]  # an approximation
       else [$x, $y, $s, .[3] ]
       end )
  | .[3] ;

```

We present two examples, one where step is a power of 1/2, and one where it is not:
```jq
printRoots( .*.*. - 3*.*. + 2*.; -1.0; 4; 1/256)

[
  0,
  1,
  2
]

printRoots( .*.*. - 3*.*. + 2*.; -1.0; 4; .001)
[
  "~1.320318770141425e-18",
  "~1.0000000000000002",
  "~1.9999999999999993"
]
```



## Julia


Assuming that one has the Roots package installed:


```Julia
using Roots

println(fzeros(x -> x^3 - 3x^2 + 2x))
```


```txt
[0.0,1.0,2.0]
```



Without the Roots package, Newton's method may be defined in this manner:

```Julia
function newton(f, fp, x::Float64,tol=1e-14::Float64,maxsteps=100::Int64)
         ##f: the function of x
         ##fp: the derivative of f

	 local xnew, xold = x, Inf
	 local fn, fo = f(xnew), Inf
	 local counter = 1

	 while (counter < maxsteps) && (abs(xnew - xold) > tol) && ( abs(fn - fo) > tol )
	   x = xnew - f(xnew)/fp(xnew) ## update x
	   xnew, xold = x, xnew
           fn, fo = f(xnew), fn
	   counter += 1
	 end

	 if counter >= maxsteps
	    error("Did not converge in ", string(maxsteps), " steps")
         else
	   xnew, counter
         end
end

```


Finding the roots of f(x) = x3 - 3x2 + 2x:


```Julia

f(x) = x^3 - 3*x^2 + 2*x
fp(x) = 3*x^2-6*x+2

x_s, count = newton(f,fp,1.00)

```

(1.0,2)


## Kotlin

```scala
// version 1.1.2

typealias DoubleToDouble = (Double) -> Double

fun f(x: Double) = x * x * x - 3.0 * x * x + 2.0 * x

fun secant(x1: Double, x2: Double, f: DoubleToDouble): Double {
    val e = 1.0e-12
    val limit = 50
    var xa = x1
    var xb = x2
    var fa = f(xa)
    var  i = 0
    while (i++ < limit) {
        var fb = f(xb)
        val d = (xb - xa) / (fb - fa) * fb
        if (Math.abs(d) < e) break
        xa = xb
        fa = fb
        xb -= d
    }
    if (i == limit) {
        println("Function is not converging near (${"%7.4f".format(xa)}, ${"%7.4f".format(xb)}).")
        return -99.0
    }
    return xb
}

fun main(args: Array<String>) {
    val step = 1.0e-2
    val e = 1.0e-12
    var x = -1.032
    var s = f(x) > 0.0
    while (x < 3.0) {
        val value = f(x)
        if (Math.abs(value) < e) {
            println("Root found at x = ${"%12.9f".format(x)}")
            s = f(x + 0.0001) > 0.0
        }
        else if ((value > 0.0) != s) {
            val xx = secant(x - step, x, ::f)
            if (xx != -99.0)
                println("Root found at x = ${"%12.9f".format(xx)}")
            else
                println("Root found near x = ${"%7.4f".format(x)}")
            s = f(x + 0.0001) > 0.0
        }
        x += step
    }
}
```


```txt

Root found at x =  0.000000000
Root found at x =  1.000000000
Root found at x =  2.000000000

```



## Liberty BASIC


```lb
'   Finds and output the roots of a given function f(x),
'       within a range of x values.

'   [RC]Roots of an function

    mainwin 80 12

    xMin  =-1
    xMax  = 3
    y     =f( xMin) '   Since Liberty BASIC has an 'eval(' function the fn
    '                       and limits would be better entered via 'input'.
    LastY =y

    eps  =1E-12 '   closeness acceptable

    bigH=0.01

    print
    print " Checking for roots of x^3 -3 *x^2 +2 *x =0 over range -1 to +3"
    print

    x=xMin: dx = bigH
    do
        x=x+dx
        y = f(x)
        'print x, dx, y
        if y*LastY <0 then 'there is a root, should drill deeper
            if dx < eps then    'we are close enough
                print " Just crossed axis, solution f( x) ="; y; " at x ="; using( "#.#####", x)
                LastY = y
                dx = bigH   'after closing on root, continue with big step
            else
                x=x-dx  'step back
                dx = dx/10  'repeat with smaller step
            end if
        end if
    loop while x<xMax

    print
    print " Finished checking in range specified."

    end

    function f( x)
        f =x^3 -3 *x^2 +2 *x
    end function
```



## Lua


```Lua
-- Function to have roots found
function f (x) return x^3 - 3*x^2 + 2*x end

-- Find roots of f within x=[start, stop] or approximations thereof
function root (f, start, stop, step)
    local roots, x, sign, foundExact, value = {}, start, f(start) > 0
    while x <= stop do
        value = f(x)
        if value == 0 then
            table.insert(roots, {val = x, err = 0})
            foundExact = true
        end
        if value > 0 ~= sign then
            if foundExact then
                foundExact = false
            else
                table.insert(roots, {val = x, err = step})
            end
        end
        sign = value > 0
        x = x + step
    end
    return roots
end

-- Main procedure
print("Root (to 12DP)\tMax. Error\n")
for _, r in pairs(root(f, -1, 3, 10^-6)) do
    print(string.format("%0.12f", r.val), r.err)
end
```

```txt
Root (to 12DP)  Max. Error

0.000000000008  1e-06
1.000000000016  1e-06
2.000000999934  1e-06
```

Note that the roots found are all near misses because fractional numbers that seem nice and 'round' in decimal (such as 10^-6) often have some rounding error when represented in binary.  To increase the chances of finding exact integer roots, try using an integer start value with a step value that is a power of two.

```Lua
-- Main procedure
print("Root (to 12DP)\tMax. Error\n")
for _, r in pairs(root(f, -1, 3, 2^-10)) do
    print(string.format("%0.12f", r.val), r.err)
end
```

```txt
Root (to 12DP)  Max. Error

0.000000000000  0
1.000000000000  0
2.000000000000  0
```



## Maple



```maple
f := x^3-3*x^2+2*x;
roots(f,x);
```


outputs:


```maple
[[0, 1], [1, 1], [2, 1]]
```


which means there are three roots. Each root is named as a pair where the first element is the value (0, 1, and 2), the second one the multiplicity (=1 for each means none of the three are degenerate).

By itself (i.e. unless specifically asked to do so), Maple will only perform exact (symbolic) operations and not attempt to do any kind of numerical approximation.


## Mathematica


There are multiple obvious ways to do this in Mathematica.


### Solve

This requires a full equation and will perform symbolic operations only:

```mathematica
Solve[x^3-3*x^2+2*x==0,x]
```

Output

```txt
 {{x->0},{x->1},{x->2}}
```



### NSolve

This requires merely the polynomial and will perform numerical operations if needed:

```mathematica
 NSolve[x^3 - 3*x^2 + 2*x , x]
```

Output

```txt
 {{x->0.},{x->1.},{x->2.}}
```

(note that the results here are floats)


### FindRoot

This will numerically try to find one(!) local root from a given starting point:

```mathematica
FindRoot[x^3 - 3*x^2 + 2*x , {x, 1.5}]
```

Output

```txt
 {x->0.}
```

From a different start point:

```mathematica
FindRoot[x^3 - 3*x^2 + 2*x , {x, 1.1}]
```

Output

```txt
{x->1.}
```

(note that there is no guarantee which one is found).


### FindInstance

This finds a value (optionally out of a given domain) for the given variable (or a set of values for a set of given variables) that satisfy a given equality or inequality:

```mathematica
 FindInstance[x^3 - 3*x^2 + 2*x == 0, x]
```

Output

```txt
```



### Reduce

This will (symbolically) reduce a given expression to the simplest possible form, solving equations and performing substitutions in the process:

```mathematica
Reduce[x^3 - 3*x^2 + 2*x == 0, x]
```


```txt
 x==0||x==1||x==2
```

(note that this doesn't yield a "solution" but a different expression that expresses the same thing as the original)


## Maxima


```maxima
e: x^3 - 3*x^2 + 2*x$

/* Number of roots in a real interval, using Sturm sequences */
nroots(e, -10, 10);
3

solve(e, x);
[x=1, x=2, x=0]

/* 'solve sets the system variable 'multiplicities */

solve(x^4 - 2*x^3 + 2*x - 1, x);
[x=-1, x=1]

multiplicities;
[1, 3]

/* Rational approximation of roots using Sturm sequences and bisection */

realroots(e);
[x=1, x=2, x=0]

/* 'realroots also sets the system variable 'multiplicities */

multiplicities;
[1, 1, 1]

/* Numerical root using Brent's method (here with another equation) */

find_root(sin(t) - 1/2, t, 0, %pi/2);
0.5235987755983

fpprec: 60$

bf_find_root(sin(t) - 1/2, t, 0, %pi/2);
5.23598775598298873077107230546583814032861566562517636829158b-1

/* Numerical root using Newton's method */

load(newton1)$
newton(e, x, 1.1, 1e-6);
1.000000017531147

/* For polynomials, Jenkins–Traub algorithm */

allroots(x^3 + x + 1);
[x=1.161541399997252*%i+0.34116390191401,
 x=0.34116390191401-1.161541399997252*%i,
 x=-0.68232780382802]

bfallroots(x^3 + x + 1);
[x=1.16154139999725193608791768724717407484314725802151429063617b0*%i + 3.41163901914009663684741869855524128445594290948999288901864b-1,
 x=3.41163901914009663684741869855524128445594290948999288901864b-1 - 1.16154139999725193608791768724717407484314725802151429063617b0*%i,
 x=-6.82327803828019327369483739711048256891188581897998577803729b-1]
```



## Objeck

```objeck

bundle Default {
  class Roots {
    function : f(x : Float) ~ Float
    {
      return (x*x*x - 3.0*x*x + 2.0*x);
    }

    function : Main(args : String[]) ~ Nil
    {
      step := 0.001;
      start := -1.0;
      stop := 3.0;
      value := f(start);
      sign := (value > 0);

      if(0.0 = value) {
        start->PrintLine();
      };

      for(x := start + step; x <= stop;  x += step;)  {
        value := f(x);

        if((value > 0) <> sign) {
          IO.Console->Instance()->Print("~")->PrintLine(x);
        }
        else if(0 = value) {
          IO.Console->Instance()->Print("~")->PrintLine(x);
        };

        sign := (value > 0);
      };
    }
  }
}

```



## OCaml


A general root finder using the False Position (Regula Falsi) method, which will find all simple roots given a small step size.


```ocaml
let bracket u v =
  ((u > 0.0) && (v < 0.0)) || ((u < 0.0) && (v > 0.0));;

let xtol a b = (a = b);; (* or use |a-b| < epsilon *)

let rec regula_falsi a b fa fb f =
  if xtol a b then (a, fa) else
  let c = (fb*.a -. fa*.b) /. (fb -. fa) in
  let fc = f c in
  if fc = 0.0 then (c, fc) else
  if bracket fa fc then
    regula_falsi a c fa fc f
  else
    regula_falsi c b fc fb f;;

let search lo hi step f =
  let rec next x fx =
    if x > hi then [] else
      let y = x +. step in
      let fy = f y in
      if fx = 0.0 then
        (x,fx) :: next y fy
      else if bracket fx fy then
        (regula_falsi x y fx fy f) :: next y fy
      else
        next y fy in
  next lo (f lo);;

let showroot (x,fx) =
  Printf.printf "f(%.17f) = %.17f [%s]\n"
    x fx (if fx = 0.0 then "exact" else "approx") in
let f x = ((x -. 3.0)*.x +. 2.0)*.x  in
List.iter showroot (search (-5.0) 5.0 0.1 f);;
```


Output:

```txt

f(0.00000000000000000) = 0.00000000000000000 [exact]
f(1.00000000000000022) = 0.00000000000000000 [exact]
f(1.99999999999999978) = 0.00000000000000000 [exact]

```


Note these roots are exact solutions with floating-point calculation.


## Oforth



```Oforth
: findRoots(f, a, b, st)
| x y lasty |
   a f perform dup ->y ->lasty

   a b st step: x [
      x f perform -> y
      y ==0 ifTrue: [ System.Out "Root found at " << x << cr ]
      else: [ y lasty * sgn -1 == ifTrue: [ System.Out "Root near " << x << cr ] ]
      y ->lasty
      ] ;

: f(x)   x 3 pow x sq 3 * - x 2 * + ;
```


```txt

findRoots(#f, -1, 3, 0.0001)
Root found at 0
Root found at 1
Root found at 2

findRoots(#f, -1.000001, 3, 0.0001)
Root near 9.90000000000713e-005
Root near 1.000099
Root near 2.000099

```



## Octave


If the equation is a polynomial, we can put the coefficients in a vector and use ''roots'':


```octave
a = [ 1, -3, 2, 0 ];
r = roots(a);
% let's print it
for i = 1:3
  n = polyval(a, r(i));
  printf("x%d = %f (%f", i, r(i), n);
  if (n != 0.0)
    printf(" not");
  endif
  printf(" exact)\n");
endfor
```


Otherwise we can program our (simple) method:

```octave
function y = f(x)
  y = x.^3 -3.*x.^2 + 2.*x;
endfunction

step = 0.001;
tol = 10 .* eps;
start = -1;
stop = 3;
se = sign(f(start));

x = start;
while (x <= stop)
  v = f(x);
  if ( (v < tol) && (v > -tol) )
    printf("root at %f\n", x);
  elseif ( sign(v) != se )
    printf("root near %f\n", x);
  endif
  se = sign(v);
  x = x + step;
endwhile
```



## PARI/GP

===Gourdon–Schönhage algorithm===<!-- X. Gourdon, "Algorithmique du théorème fondamental de l'algèbre" (1993). -->

```parigp
polroots(x^3-3*x^2+2*x)
```


===Newton's method===
This uses a modified version of the Newton–Raphson method.

```parigp
polroots(x^3-3*x^2+2*x,1)
```


===Brent's method===

```parigp
solve(x=-.5,.5,x^3-3*x^2+2*x)
solve(x=.5,1.5,x^3-3*x^2+2*x)
solve(x=1.5,2.5,x^3-3*x^2+2*x)
```



### Factorization to linear factors


```parigp
findRoots(P)={
  my(f=factor(P),t);
  for(i=1,#f[,1],
    if(poldegree(f[i,1]) == 1,
      for(j=1,f[i,2],
        print(-polcoeff(f[i,1], 0), " (exact)")
      )
    );
    if(poldegree(f[i,1]) > 1,
      t=polroots(f[i,1]);
      for(j=1,#t,
        for(k=1,f[i,2],
          print(if(imag(t[j]) == 0.,real(t[j]),t[j]), " (approximate)")
        )
      )
    )
  )
};
findRoots(x^3-3*x^2+2*x)
```



### Factorization to quadratic factors

Of course this process could be continued to degrees 3 and 4 with sufficient additional work.

```parigp
findRoots(P)={
  my(f=factor(P),t);
  for(i=1,#f[,1],
    if(poldegree(f[i,1]) == 1,
      for(j=1,f[i,2],
        print(-polcoeff(f[i,1], 0), " (exact)")
      )
    );
    if(poldegree(f[i,1]) == 2,
      t=solveQuadratic(polcoeff(f[i,1],2),polcoeff(f[i,1],1),polcoeff(f[i,1],0));
      for(j=1,f[i,2],
        print(t[1]" (exact)\n"t[2]" (exact)")
      )
    );
    if(poldegree(f[i,1]) > 2,
      t=polroots(f[i,1]);
      for(j=1,#t,
        for(k=1,f[i,2],
          print(if(imag(t[j]) == 0.,real(t[j]),t[j]), " (approximate)")
        )
      )
    )
  )
};
solveQuadratic(a,b,c)={
  my(t=-b/2/a,s=b^2/4/a^2-c/a,inner=core(numerator(s))/core(denominator(s)),outer=sqrtint(s/inner));
  if(inner < 0,
    outer *= I;
    inner *= -1
  );
  s=if(inner == 1,
    outer
  ,
    if(outer == 1,
      Str("sqrt(", inner, ")")
    ,
      Str(outer, " * sqrt(", inner, ")")
    )
  );
  if (t,
    [Str(t, " + ", s), Str(t, " - ", s)]
  ,
    [s, Str("-", s)]
  )
};
findRoots(x^3-3*x^2+2*x)
```



## Pascal

```pascal
Program RootsFunction;

var
  e, x, step, value: double;
  s: boolean;
  i, limit: integer;
  x1, x2, d: double;

function f(const x: double): double;
  begin
    f := x*x*x - 3*x*x + 2*x;
  end;

begin
  x    := -1;
  step := 1.0e-6;
  e    := 1.0e-9;
  s    := (f(x) > 0);

  writeln('Version 1: simply stepping x:');
  while x < 3.0 do
  begin
    value := f(x);
    if abs(value) < e then
    begin
      writeln ('root found at x = ', x);
      s := not s;
    end
    else if ((value > 0) <> s) then
    begin
      writeln ('root found at x = ', x);
      s := not s;
    end;
    x := x + step;
  end;

  writeln('Version 2: secant method:');
  x1 := -1.0;
  x2 :=  3.0;
  e  :=  1.0e-15;
  i  :=  1;
  limit := 300;
  while true do
  begin
    if i > limit then
    begin
      writeln('Error: function not converging');
      exit;
    end;
    d := (x2 - x1) / (f(x2) - f(x1)) * f(x2);
    if abs(d) < e then
    begin
      if d = 0 then
        write('Exact ')
      else
        write('Approximate ');
      writeln('root found at x = ', x2);
      exit;
    end;
    x1 := x2;
    x2 := x2 - d;
    i  := i + 1;
  end;
end.

```

Output:

```txt

Version 1: simply stepping x:
root found at x =  7.91830063542152E-012
root found at x =  1.00000000001584E+000
root found at x =  1.99999999993357E+000
Version 2: secant method:
Exact root found at x =  1.00000000000000E+000

```



## Perl


```perl
sub f
{
        my $x = shift;

        return ($x * $x * $x - 3*$x*$x + 2*$x);
}

my $step = 0.001; # Smaller step values produce more accurate and precise results
my $start = -1;
my $stop = 3;
my $value = &f($start);
my $sign = $value > 0;

# Check for root at start

print "Root found at $start\n" if ( 0 == $value );

for(    my $x = $start + $step;
        $x <= $stop;
        $x += $step )
{
        $value = &f($x);

        if ( 0 == $value )
        {
                # We hit a root
                print "Root found at $x\n";
        }
        elsif ( ( $value > 0 ) != $sign )
        {
                # We passed a root
                print "Root found near $x\n";
        }

        # Update our sign
        $sign = ( $value > 0 );
}
```


## Perl 6

Uses exact arithmetic.

```perl6
sub f(\x) { x³ - 3*x² + 2*x }

my $start = -1;
my $stop = 3;
my $step = 0.001;

for $start, * + $step ... $stop -> $x {
    state $sign = 0;
    given f($x) {
        my $next = .sign;
        when 0.0 {
            say "Root found at $x";
        }
        when $sign and $next != $sign {
            say "Root found near $x";
        }
        NEXT $sign = $next;
    }
}
```

```txt
Root found at 0
Root found at 1
Root found at 2
```



## Phix

```Phix
procedure print_roots(integer f, atom start, atom stop, atom step)
-- Print approximate roots of f between x=start and x=stop, using
--  sign changes as an indicator that a root has been encountered.
atom x = start, y = 0

    puts(1,"-----\n")
    while x<=stop do
        atom last_y = y
        y = call_func(f,{x})
        if y=0
        or (last_y<0 and y>0)
        or (last_y>0 and y<0) then
            printf(1,"Root found %s %.10g\n", {iff(y=0?"at":"near"),x})
        end if
        x += step
    end while
end procedure

-- Smaller steps produce more accurate/precise results in general,
-- but for many functions we'll never get exact roots, either due
-- to imperfect binary representation or irrational roots.
constant step = 1/256

function f1(atom x) return x*x*x-3*x*x+2*x  end function
function f2(atom x) return x*x-4*x+3        end function
function f3(atom x) return x-1.5            end function
function f4(atom x) return x*x-2            end function

print_roots(routine_id("f1"), -1, 5, step)
print_roots(routine_id("f2"), -1, 5, step)
print_roots(routine_id("f3"),  0, 4, step)
print_roots(routine_id("f4"), -2, 2, step)
```


```txt

-----
Root found at 0
Root found at 1
Root found at 2
-----
Root found at 1
Root found at 3
-----
Root found at 1.5
-----
Root found near -1.4140625
Root found near 1.41796875

```



## PicoLisp

```PicoLisp
(de findRoots (F Start Stop Step Eps)
   (filter
      '((N) (> Eps (abs (F N))))
      (range Start Stop Step) ) )

(scl 12)

(mapcar round
   (findRoots
      '((X) (+ (*/ X X X `(* 1.0 1.0)) (*/ -3 X X 1.0) (* 2 X)))
      -1.0 3.0 0.0001 0.00000001 ) )
```

Output:

```txt
-> ("0.000" "1.000" "2.000")
```



## PL/I


```PL/I

f: procedure (x) returns (float (18));
   declare x float (18);
   return (x**3 - 3*x**2 + 2*x );
end f;

declare eps float, (x, y) float (18);
declare dx fixed decimal (15,13);

eps = 1e-12;

do dx = -5.03 to 5 by 0.1;
   x = dx;
   if sign(f(x)) ^= sign(f(dx+0.1)) then
      call locate_root;
end;

locate_root: procedure;
   declare (left, mid, right) float (18);

   put skip list ('Looking for root in [' || x, x+0.1 || ']' );
   left = x; right = dx+0.1;
   PUT SKIP LIST (F(LEFT), F(RIGHT) );
   if abs(f(left) ) < eps then
      do; put skip list ('Found a root at x=', left); return; end;
   else if abs(f(right) ) < eps then
      do; put skip list ('Found a root at x=', right); return; end;
   do forever;
      mid = (left+right)/2;
      if sign(f(mid)) = 0 then
         do; put skip list ('Root found at x=', mid); return; end;
      else if sign(f(left)) ^= sign(f(mid)) then
         right = mid;
      else
         left = mid;
      /* put skip list (left || right); */
      if abs(right-left) < eps then
         do; put skip list ('There is a root near ' ||
            (left+right)/2); return;
         end;
   end;
end locate_root;

```


## PureBasic

```PureBasic
Procedure.d f(x.d)
  ProcedureReturn x*x*x-3*x*x+2*x
EndProcedure

Procedure main()
  OpenConsole()
  Define.d StepSize= 0.001
  Define.d Start=-1, stop=3
  Define.d value=f(start), x=start
  Define.i oldsign=Sign(value)

  If value=0
    PrintN("Root found at "+StrF(start))
  EndIf

  While x<=stop
    value=f(x)
    If Sign(value) <> oldsign
      PrintN("Root found near "+StrF(x))
    ElseIf value = 0
      PrintN("Root found at "+StrF(x))
    EndIf
    oldsign=Sign(value)
    x+StepSize
  Wend
EndProcedure

main()
```



## Python

```python
f = lambda x: x * x * x - 3 * x * x + 2 * x

step = 0.001 # Smaller step values produce more accurate and precise results
start = -1
stop = 3

sign = f(start) > 0

x = start
while x <= stop:
    value = f(x)

    if value == 0:
        # We hit a root
        print "Root found at", x
    elif (value > 0) != sign:
        # We passed a root
        print "Root found near", x

    # Update our sign
    sign = value > 0

    x += step
```



## R

```R
f <- function(x) x^3 -3*x^2 + 2*x

findroots <- function(f, begin, end, tol = 1e-20, step = 0.001) {
  se <- ifelse(sign(f(begin))==0, 1, sign(f(begin)))
  x <- begin
  while ( x <= end ) {
    v <- f(x)
    if ( abs(v) < tol ) {
      print(sprintf("root at %f", x))
    } else if ( ifelse(sign(v)==0, 1, sign(v)) != se )  {
      print(sprintf("root near %f", x))
    }
    se <- ifelse( sign(v) == 0 , 1, sign(v))
    x <- x + step
  }
}

findroots(f, -1, 3)
```



## Racket



```racket

#lang racket

;; Attempts to find all roots of a real-valued function f
;; in a given interval [a b] by dividing the interval into N parts
;; and using the root-finding method on each subinterval
;; which proves to contain a root.
(define (find-roots f a b
                    #:divisions [N 10]
                    #:method [method secant])
  (define h (/ (- b a) N))
  (for*/list ([x1 (in-range a b h)]
              [x2 (in-value (+ x1 h))]
              #:when (or (root? f x1)
                         (includes-root? f x1 x2)))
    (find-root f x1 x2 #:method method)))

;; Finds a root of a real-valued function f
;; in a given interval [a b].
(define (find-root f a b #:method [method secant])
  (cond
    [(root? f a) a]
    [(root? f b) b]
    [else (and (includes-root? f a b) (method f a b))]))

;; Returns #t if x is a root of a real-valued function f
;; with absolute accuracy (tolerance).
(define (root? f x) (almost-equal? 0 (f x)))

;; Returns #t if interval (a b) contains a root
;; (or the odd number of roots) of a real-valued function f.
(define (includes-root? f a b) (< (* (f a) (f b)) 0))

;; Returns #t if a and b are equal with respect to
;; the relative accuracy (tolerance).
(define (almost-equal? a b)
  (or (< (abs (+ b a)) (tolerance))
      (< (abs (/ (- b a) (+ b a))) (tolerance))))

(define tolerance (make-parameter 5e-16))

```


Different root-finding methods


```racket

(define (secant f a b)
  (let next ([x1 a] [y1 (f a)] [x2 b] [y2 (f b)] [n 50])
    (define x3 (/ (- (* x1 y2) (* x2 y1)) (- y2 y1)))
    (cond
      ; if the method din't converge within given interval
      ; switch to more robust bisection method
      [(or (not (< a x3 b)) (zero? n)) (bisection f a b)]
      [(almost-equal? x3 x2) x3]
      [else (next x2 y2 x3 (f x3) (sub1 n))])))

(define (bisection f x1 x2)
  (let divide ([a x1] [b x2])
    (and (<= (* (f a) (f b)) 0)
         (let ([c (* 0.5 (+ a b))])
           (if (almost-equal? a b)
               c
               (or (divide a c) (divide c b)))))))

```


Examples:

```racket

-> (find-root (λ (x) (- 2. (* x x))) 1 2)
1.414213562373095
-> (sqrt 2)
1.4142135623730951

-> (define (f x) (+ (* x x x) (* -3.0 x x) (* 2.0 x)))
-> (find-roots f -3 4 #:divisions 50)
'(2.4932181969624796e-33 1.0 2.0)

```


In order to provide a comprehensive code the given solution does not optimize the number of function calls.
The functional nature of Racket allows to perform the optimization without changing the main code using memoization.

Simple memoization operator

```racket

(define (memoized f)
  (define tbl (make-hash))
  (λ x
    (cond [(hash-ref tbl x #f) => values]
          [else (define res (apply f x))
                (hash-set! tbl x res)
                res])))

```


To use memoization just call

```racket

-> (find-roots (memoized f) -3 4 #:divisions 50)
'(2.4932181969624796e-33 1.0 2.0)

```


The profiling shows that memoization reduces the number of function calls
in this example from 184 to 67 (50 calls for primary interval division and about 6 calls for each point refinement).


## REXX

Both of REXX versions use the   '''bisection method'''   is used.

### function is coded as a REXX function


```rexx
/*REXX program finds the roots of a specific function:  x^3 - 3*x^2 + 2*x  via bisection*/
parse arg bot top inc .                          /*obtain optional arguments from the CL*/
if bot=='' | bot==","  then bot= -5              /*Not specified?  Then use the default.*/
if top=='' | top==","  then top= +5              /* "       "        "   "   "     "    */
if inc=='' | inc==","  then inc=   .0001         /* "       "        "   "   "     "    */
z=f(bot-inc);                 !=sign(z)          /*use these values for initial compare.*/

      do j=bot  to top  by  inc                  /*traipse through the specified range. */
      z=f(j);              $=sign(z)             /*compute new value;  obtain the sign. */
      if z=0  then                                 say  'found an exact root at'   j/1
              else if  !\==$  then  if !\==0  then say  'passed a root at'         j/1
      !=$                                        /*use the new sign for the next compare*/
      end   /*j*/                                /*dividing by unity normalizes J  [↑]  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:    parse arg x;     return x*(x*(x-3)+2)      /*formula used   ──► x^3 - 3x^2  + 2x  */
                                                 /*with factoring ──► x{ x^2 -3x  + 2 } */
                                                 /*more     "     ──► x{ x( x-3 ) + 2 } */
```

'''output'''   when using the defaults for input:

```txt

found an exact root at 0
found an exact root at 1
found an exact root at 2

```


===function is coded in-line===
This version is about 40% faster than the 1<sup>st</sup> REXX version.

```rexx
/*REXX program finds the roots of a specific function:  x^3 - 3*x^2 + 2*x  via bisection*/
parse arg bot top inc .                          /*obtain optional arguments from the CL*/
if bot=='' | bot==","  then bot= -5              /*Not specified?  Then use the default.*/
if top=='' | top==","  then top= +5              /* "       "        "   "   "     "    */
if inc=='' | inc==","  then inc=   .0001         /* "       "        "   "   "     "    */
x=bot-inc                                        /*compute 1st value to start compares. */
z=x*(x*(x-3)+2)                                  /*formula used   ──► x^3 - 3x^2  + 2x  */
!=sign(z)                                        /*obtain the sign of the initial value.*/
            do x=bot  to top  by  inc            /*traipse through the specified range. */
            z=x*(x*(x-3)+2);       $=sign(z)     /*compute new value;  obtain the sign. */
            if z=0  then                               say  'found an exact root at'   x/1
                    else if !\==$  then if !\==0  then say  'passed a root at'         x/1
            !=$                                  /*use the new sign for the next compare*/
            end   /*x*/                          /*dividing by unity normalizes X  [↑]  */
```

## Ring


```ring

load "stdlib.ring"
function = "return pow(x,3)-3*pow(x,2)+2*x"
rangemin = -1
rangemax = 3
stepsize = 0.001
accuracy = 0.1
roots(function, rangemin, rangemax, stepsize, accuracy)

func roots funct, min, max, inc, eps
     oldsign = 0
     for x = min to max step inc
         num = sign(eval(funct))
         if num = 0
            see "root found at x = " + x + nl
            num = -oldsign
         else if num != oldsign and oldsign != 0
              if inc < eps
                 see "root found near x = " + x + nl
              else roots(funct, x-inc, x+inc/8, inc/8, eps) ok ok ok
         oldsign = num
     next

```

Output:

```txt

root found near x = 0.00
root found near x = 1.00
root found near x = 2.00

```



## RLaB

RLaB implements a number of solvers from the GSL and the netlib that find the roots of a real or vector function of a real or vector variable.
The solvers are grouped with respect whether the variable is a scalar, ''findroot'', or a vector, ''findroots''. Furthermore, for each group there are two types of solvers, one that does not require the derivative of the objective function (which root(s) are being sought), and one that does.

The script that finds a root of a scalar function <math>f(x) = x^3-3\,x^2 + 2\,x</math> of a scalar variable ''x''
using the bisection method on the interval -5 to 5 is,

```RLaB

f = function(x)
{
  rval = x .^ 3 - 3 * x .^ 2 + 2 * x;
  return rval;
};

>> findroot(f, , [-5,5])
  0

```


For a detailed description of the solver and its parameters interested reader is directed to the ''rlabplus'' manual.


## Ruby

```ruby
def sign(x)
  x <=> 0
end

def find_roots(f, range, step=0.001)
  sign = sign(f[range.begin])
  range.step(step) do |x|
    value = f[x]
    if value == 0
      puts "Root found at #{x}"
    elsif sign(value) == -sign
      puts "Root found between #{x-step} and #{x}"
    end
    sign = sign(value)
  end
end

f = lambda { |x| x**3 - 3*x**2 + 2*x }
find_roots(f, -1..3)
```


```txt

Root found at 0.0
Root found at 1.0
Root found at 2.0

```


Or we could use Enumerable#inject, monkey patching and block:


```ruby
class Numeric
  def sign
    self <=> 0
  end
end

def find_roots(range, step = 1e-3)
  range.step( step ).inject( yield(range.begin).sign ) do |sign, x|
    value = yield(x)
    if value == 0
      puts "Root found at #{x}"
    elsif value.sign == -sign
      puts "Root found between #{x-step} and #{x}"
    end
    value.sign
  end
end

find_roots(-1..3) { |x| x**3 - 3*x**2 + 2*x }
```




## Scala

===Imperative version (Ugly, side effects)===
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/T63KUsH/0 (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/bh8von94Q1y0tInvEZ3cBQ Scastie (remote JVM)].

```Scala
object Roots extends App {
  val poly = (x: Double) => x * x * x - 3 * x * x + 2 * x

  private def printRoots(f: Double => Double,
                         lowerBound: Double,
                         upperBound: Double,
                         step: Double): Unit = {
    val y = f(lowerBound)
    var (ox, oy, os) = (lowerBound, y, math.signum(y))

    for (x <- lowerBound to upperBound by step) {
      val y = f(x)
      val s = math.signum(y)
      if (s == 0) println(x)
      else if (s != os) println(s"~${x - (x - ox) * (y / (y - oy))}")

      ox = x
      oy = y
      os = s
    }
  }

  printRoots(poly, -1.0, 4, 0.002)

}
```

===Functional version (Recommended)===

```Scala
object RootsOfAFunction extends App {
    def findRoots(fn: Double => Double, start: Double, stop: Double, step: Double, epsilon: Double) = {
    for {
      x <- start to stop by step
      if fn(x).abs < epsilon
    } yield x
  }

  def fn(x: Double) = x * x * x - 3 * x * x + 2 * x

  println(findRoots(fn, -1.0, 3.0, 0.0001, 0.000000001))
}
```

 Vector(-9.381755897326649E-14, 0.9999999999998124, 1.9999999999997022)


## Sidef


```ruby
func f(x) {
    x*x*x - 3*x*x + 2*x
}
 
var step = 0.001
var start = -1
var stop = 3
 
for x in range(start+step, stop, step) {
    static sign = false
    given (var value = f(x)) {
        when (0) {
            say "Root found at #{x}"
        }
        case (sign && ((value > 0) != sign)) {
            say "Root found near #{x}"
        }
    }
    sign = value>0
}
```

```txt
Root found at 0
Root found at 1
Root found at 2
```



## Tcl

This simple brute force iteration marks all results, with a leading "~", as approximate. This version always reports its results as approximate because of the general limits of computation using fixed-width floating-point numbers (i.e., IEEE double-precision floats).

```Tcl
proc froots {lambda {start -3} {end 3} {step 0.0001}} {
    set res {}
    set lastsign [sgn [apply $lambda $start]]
    for {set x $start} {$x <= $end} {set x [expr {$x + $step}]} {
        set sign [sgn [apply $lambda $x]]
        if {$sign != $lastsign} {
            lappend res [format ~%.11f $x]
        }
        set lastsign $sign
    }
    return $res
}
proc sgn x {expr {($x>0) - ($x<0)}}

puts [froots {x {expr {$x**3 - 3*$x**2 + 2*$x}}}]
```

Result and timing:

```txt
/Tcl $ time ./froots.tcl
~0.00000000000 ~1.00000000000 ~2.00000000000

real    0m0.368s
user    0m0.062s
sys     0m0.030s
```

A more elegant solution (and faster, because you can usually make the initial search coarser) is to use brute-force iteration and then refine with [[wp:Newton's method|Newton-Raphson]], but that requires the differential of the function with respect to the search variable.

```Tcl
proc frootsNR {f df {start -3} {end 3} {step 0.001}} {
    set res {}
    set lastsign [sgn [apply $f $start]]
    for {set x $start} {$x <= $end} {set x [expr {$x + $step}]} {
        set sign [sgn [apply $f $x]]
        if {$sign != $lastsign} {
            lappend res [format ~%.15f [nr $x $f $df]]
        }
        set lastsign $sign
    }
    return $res
}
proc sgn x {expr {($x>0) - ($x<0)}}
proc nr {x1 f df} {
    # Newton's method converges very rapidly indeed
    for {set iters 0} {$iters < 10} {incr iters} {
        set x1 [expr {
            [set x0 $x1] - [apply $f $x0]/[apply $df $x0]
        }]
        if {$x0 == $x1} {
            break
        }
    }
    return $x1
}

puts [frootsNR \
    {x {expr {$x**3 - 3*$x**2 + 2*$x}}} \
    {x {expr {3*$x**2 - 6*$x + 2}}}]
```


=={{header|TI-89 BASIC}}==

Finding roots is a built-in function: <code>zeros(x^3-3x^2+2x, x)</code> returns <code>{0,1,2}</code>.

In this case, the roots are exact; inexact results are marked by decimal points.


## zkl

```zkl
fcn findRoots(f,start,stop,step,eps){
   [start..stop,step].filter('wrap(x){ f(x).closeTo(0.0,eps) })
}
```


```zkl
fcn f(x){ x*x*x - 3.0*x*x + 2.0*x }
findRoots(f, -1.0, 3.0, 0.0001, 0.00000001).println();
```

```txt
L(-9.38176e-14,1,2)
```

```zkl
fcn secant(f,xA,xB){
   reg e=1.0e-12;

   fA:=f(xA); if(fA.closeTo(0.0,e)) return(xA);

   do(50){
      fB:=f(xB);
      d:=(xB - xA) / (fB - fA) * fB;
      if(d.closeTo(0,e)) break;
      xA = xB; fA = fB; xB -= d;
   }
   if(f(xB).closeTo(0.0,e)) xB
   else "Function is not converging near (%7.4f,%7.4f).".fmt(xA,xB);
}
```


```zkl
step:=0.1;
xs:=findRoots(f, -1.032, 3.0, step, 0.1);
xs.println(" --> ",xs.apply('wrap(x){ secant(f,x-step,x+step) }));
```

```txt
L(-0.032,0.968,1.068,1.968) --> L(1.87115e-19,1,1,2)
```


