+++
title = "Formal power series"
description = ""
date = 2018-10-14T22:48:04Z
aliases = []
[extra]
id = 2783
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "c",
  "clojure",
  "common_lisp",
  "d",
  "echolisp",
  "elisa",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "maxima",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "scheme",
  "tcl",
  "zkl",
]
+++

## Task

A ''power series'' is an infinite sum of the form

<math>a_0 + a_1 \cdot x + a_2 \cdot x^2 + a_3 \cdot x^3 + \cdots</math>

The ''a<sub>i</sub>'' are called the ''coefficients'' of the series. Such sums can be added, multiplied etc., where the new coefficients of the powers of ''x'' are calculated according to the usual rules.

If one is not interested in evaluating such a series for particular values of ''x'', or in other words, if convergence doesn't play a role, then such a collection of coefficients is called ''formal power series''. It can be treated like a new kind of number.

'''Task''': Implement formal power series as a numeric type. Operations should at least include ''addition'', ''multiplication'', ''division'' and additionally non-numeric operations like ''differentiation'' and ''integration'' (with an integration constant of zero). Take care that your implementation deals with the potentially infinite number of coefficients.

As an example, define the power series of sine and cosine in terms of each other using integration, as in

<math>\sin x = \int_0^x \cos t\, dt</math>

<math>\cos x = 1 - \int_0^x \sin t\, dt</math>

'''Goals''': Demonstrate how the language handles new numeric types and delayed (or ''lazy'') evaluation.


## Ada

The Taylor series package is generic to be instantiated with any rational type implementation provided by the task [[Rational Arithmetic]]:

```ada
with Generic_Rational;

generic
   with package Rational_Numbers is new Generic_Rational (<>);
package Generic_Taylor_Series is
   use Rational_Numbers;
   type Taylor_Series is array (Natural range <>) of Rational;

   function "+" (A : Taylor_Series) return Taylor_Series;
   function "-" (A : Taylor_Series) return Taylor_Series;

   function "+" (A, B : Taylor_Series) return Taylor_Series;
   function "-" (A, B : Taylor_Series) return Taylor_Series;
   function "*" (A, B : Taylor_Series) return Taylor_Series;

   function Integral (A : Taylor_Series) return Taylor_Series;
   function Differential (A : Taylor_Series) return Taylor_Series;

   function Value (A : Taylor_Series; X : Rational) return Rational;

   Zero : constant Taylor_Series := (0 => Rational_Numbers.Zero);
   One  : constant Taylor_Series := (0 => Rational_Numbers.One);
end Generic_Taylor_Series;
```

The package implementation:

```ada
package body Generic_Taylor_Series is
   function Normalize (A : Taylor_Series) return Taylor_Series is
   begin
      for Power in reverse A'Range loop
         if A (Power) /= 0 then
            return A (0..Power);
         end if;
      end loop;
      return Zero;
   end Normalize;

   function "+" (A : Taylor_Series) return Taylor_Series is
   begin
      return A;
   end "+";

   function "-" (A : Taylor_Series) return Taylor_Series is
      Result : Taylor_Series (A'Range);
   begin
      for Power in A'Range loop
         Result (Power) := -A (Power);
      end loop;
      return Result;
   end "-";

   function "+" (A, B : Taylor_Series) return Taylor_Series is
   begin
      if A'Last > B'Last then
         return B + A;
      else
         declare
            Result : Taylor_Series (0..B'Last);
         begin
            for Power in A'Range loop
               Result (Power) := A (Power) + B (Power);
            end loop;
            for Power in A'Last + 1..B'Last loop
               Result (Power) := B (Power);
            end loop;
            return Normalize (Result);
         end;
      end if;
   end "+";

   function "-" (A, B : Taylor_Series) return Taylor_Series is
   begin
      return A + (-B);
   end "-";

   function "*" (A, B : Taylor_Series) return Taylor_Series is
      Result : Taylor_Series (0..A'Last + B'Last);
   begin
      for I in A'Range loop
         for J in B'Range loop
            Result (I + J) := A (I) * B (J);
         end loop;
      end loop;
      return Normalize (Result);
   end "*";

   function Integral (A : Taylor_Series) return Taylor_Series is
   begin
      if A = Zero then
         return Zero;
      else
         declare
            Result : Taylor_Series (0..A'Last + 1);
         begin
            for Power in A'Range loop
               Result (Power + 1) := A (Power) / Number (Power + 1);
            end loop;
            Result (0) := Rational_Numbers.Zero;
            return Result;
         end;
      end if;
   end Integral;

   function Differential (A : Taylor_Series) return Taylor_Series is
   begin
      if A'Length = 1 then
         return Zero;
      else
         declare
            Result : Taylor_Series (0..A'Last - 1);
         begin
            for Power in Result'Range loop
               Result (Power) := A (Power + 1) * Number (Power);
            end loop;
            return Result;
         end;
      end if;
   end Differential;

   function Value (A : Taylor_Series; X : Rational) return Rational is
      Sum : Rational := A (A'Last);
   begin
      for Power in reverse 0..A'Last - 1 loop
         Sum := Sum * X + A (Power);
      end loop;
      return Sum;
   end Value;

end Generic_Taylor_Series;
```

The procedure Normalize is used to truncate the series when the coefficients are zero. The summation of a series (function Value) uses [[wp:Horner_scheme|Horner scheme]].

### Test task


```ada
with Ada.Text_IO;  use Ada.Text_IO;

with Generic_Taylor_Series;
with Generic_Rational;

procedure Test_Taylor_Series is
   package Integer_Rationals is new Generic_Rational (Integer);
   package Integer_Taylor_Series is new Generic_Taylor_Series (Integer_Rationals);
   use Integer_Taylor_Series;
      -- Procedure to print a series
   procedure Put (A : Taylor_Series) is
      use Integer_Rationals;
      procedure Put (A : Rational) is
      begin
         if Numerator (A) = 1 then
            Put (" 1");
         else
            Put (Integer'Image (Numerator (A)));
         end if;
         if Denominator (A) /= 1 then
            Put (" /");
            Put (Integer'Image (Denominator (A)));
         end if;
      end Put;
   begin
      if A (0) /= 0 then
         Put (A (0));
      end if;
      for Power in 1..A'Last loop
         if A (Power) > 0 then
            Put (" +");
            Put (A (Power));
            Put (" X **" & Integer'Image (Power));
         elsif A (Power) < 0 then
            Put (" -");
            Put (abs A (Power));
            Put (" X **" & Integer'Image (Power));
         end if;
      end loop;
   end Put;
      -- Cosine generator
   function Cos (N : Natural) return Taylor_Series is
   begin
      if N = 0 then
         return One;
      else
         return One - Integral (Integral (Cos (N - 1)));
      end if;
   end Cos;
begin
   Put ("Cos ="); Put (Cos (5)); Put_Line (" ...");
   Put ("Sin ="); Put (Integral (Cos (5))); Put_Line (" ...");
end Test_Taylor_Series;
```

Sample output:

```txt

Cos = 1 - 1 / 2 X ** 2 + 1 / 24 X ** 4 - 1 / 720 X ** 6 + 1 / 40320 X ** 8 - 1 / 3628800 X ** 10 ...
Sin = + 1 X ** 1 - 1 / 6 X ** 3 + 1 / 120 X ** 5 - 1 / 5040 X ** 7 + 1 / 362880X ** 9 - 1 / 39916800 X ** 11 ...

```



## Clojure


This version takes advantage of the laziness of most of Clojure's sequence functions, including ''map'', ''for'', ''take-while'', ''concat'', and ''drop''. A formal power series (FPS) is represented as a sequence of coefficients; for example, ''[1 2 3]'' represents ''1 + 2*x + 3*x*x''.

First addition and subtraction. Note that most of the complication arises in allowing for finite and infinite FPSs; if only infinite power series were at issue, the function ''(defn ips+ [ips0 ips1] (map + ips0 ips1))'' would suffice.

```clojure
(defn ps+ [ps0 ps1]
  (letfn [(+zs   [ps] (concat ps (repeat :z)))
          (notz? [a] (not= :z a))
          (nval  [a] (if (notz? a) a 0))
          (z+    [a0 a1] (if (= :z a0 a1) :z (+ (nval a0) (nval a1))))]
    (take-while notz? (map z+ (+zs ps0) (+zs ps1)))))

(defn ps- [ps0 ps1] (ps+ ps0 (map - ps1)))
```

Multiplication next; again most of the complication is dealing with both finite and infinite FPS. This function explicitly uses the standard function ''lazy-seq'' to define the product sequence.

```clojure
(defn ps*
  ([ps0 ps1] (ps* [0] ps0 ps1))
  ([[a0 & resta] [p0 & rest0] [p1 & rest1 :as ps1]]
    (lazy-seq
      (cons
        (+ a0 (* p0 p1))
        (let [mrest1 (if (or (nil? rest1) (zero? p0)) nil, (map #(* p0 %) rest1))
              accum  (cond (nil? resta) mrest1, (nil? mrest1) resta, :else (ps+ resta mrest1))]
          (if (nil? rest0) accum, (ps* (or accum [0]) rest0 ps1)))))))
```

As with most of the other examples on this page, there's no definition for division. Mathematically, FPS is a commutative ring (in fact a Euclidean domain), but not a field: the set of FPSs is not closed under division.

Now we can define integration and differentiation:

```clojure
(defn indexed [ps] (map vector (iterate inc 0) ps))

(defn differentiate [ps]
  (drop 1 (for [[n a] (indexed ps)] (* n a))))

(defn integrate [ps]
  (cons 0 (for [[n a] (indexed ps)] (/ a (inc n)))))
```

Some examples of using these functions; in each case a ''println'' call (which forces the lazy sequence) is followed by a comment showing the output.

```clojure
(println (ps+ [1 2] [3 4 5]))
; (4 6 5)

(println (ps* [1 2] [3 4 5]))
; (3 10 13 10)
```

And some examples using infinite FPSs. First define the sequence of factorials (''facts''), then define ''sin'' and ''cos'' Taylor series.

```clojure
(def nfacts (iterate (fn [[f n]] [(* f n) (inc n)]) [1 1]))
(def facts (map first nfacts))

(def sin (map / (cycle [0  1  0 -1]) facts))
(def cos (map / (cycle [1  0 -1  0]) facts))

(println (take 10 sin))
; (0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)

(println (take 10 (integrate cos)))
; (0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)

(println (take 20 (ps+ (ps* sin sin) (ps* cos cos))))
; (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
```

By using ''letfn'', which supports defining mutually recursive functions, we can define the ''sin'' and ''cos'' power series directly in terms of integrals of the other series:

```clojure
(letfn [(fsin [] (lazy-seq (integrate (fcos))))
        (fcos [] (ps- [1] (integrate (fsin))))]
  (def sinx (fsin))
  (def cosx (fcos)))

(println (take 10 sinx))
; (0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)
```



## Common Lisp


Common Lisp isn't lazy, and doesn't define the arithmetic operators as generic functions.  As such, this implementation defines lazy primitives (delay and force), and a lazy list built on top of them (lons, lar, ldr).  This implementation also defines a package #:formal-power-series which uses all the symbols of the "COMMON-LISP" package except for +, -, *, and /, which are shadowed.  Shadowing these symbols allows for definitions of generic functions which can be specialized for the power series, can default to the normal CL arithmetic operations for other kinds of objects, and can do "the right thing" for mixes thereof.


```lisp
(defpackage #:formal-power-series
  (:nicknames #:fps)
  (:use "COMMON-LISP")
  (:shadow
   #:+ #:- #:* #:/))

(in-package #:formal-power-series)
```



### Lazy Primitives



```lisp
(defstruct promise
  thunk value)

(defmacro delay (form)
  `(make-promise :thunk #'(lambda () ,form)))

(defun force (object)
  (cond
   ((not (promise-p object))
    object)
   ((null (promise-thunk object))
    (promise-value object))
   (t (let ((val (funcall (promise-thunk object))))
        (setf (promise-thunk object) nil
              (promise-value object) val)))))
```



### Lazy Lists



```lisp
(defstruct lons
  lar
  ldr)

(defun lar (lons)
  (lons-lar lons))

(defun ldr (lons)
  (if (not (promise-p (lons-ldr lons)))
    (lons-ldr lons)
    (setf (lons-ldr lons)
          (force (lons-ldr lons)))))

(defmacro lons (lar ldr)
  `(make-lons :lar ,lar :ldr (delay ,ldr)))
```


A few utilities to make working with lazy lists easier.


```lisp
(defun invoke-with-lons (function lons)
  (funcall function (lar lons) (ldr lons)))

(defmacro with-lons ((lar ldr) lons &body body)
  `(invoke-with-lons #'(lambda (,lar ,ldr) ,@body) ,lons))

(defun maplar (function llist &rest llists)
  (let ((llists (list* llist llists)))
    (if (some 'null llists) nil
      (lons (apply function (cl:mapcar 'lar llists))
            (apply 'maplar function (cl:mapcar 'ldr llists))))))

(defun take (n llist)
  (if (zerop n) '()
    (lons (lar llist)
          (take (1- n) (ldr llist)))))

(defun force-list (llist)
  (do ((fl '() (cons (lar l) fl))
       (l llist (ldr l)))
      ((null l) (nreverse fl))))

(defun repeat (x)
  (lons x (repeat x)))

(defun up-from (n)
  (lons n (up-from (1+ n))))
```



### Formal Power Series


The mathematical operations here are translations of the Haskell code, but we specialize the operations in various ways so that behavior for normal numeric operations is preserved.


```lisp
(defstruct (series (:constructor series (coeffs)) (:conc-name))
  coeffs)

(defgeneric negate (f)
  (:method (f)
   (cl:- f))
  (:method ((f series))
   (series (maplar 'negate (coeffs f)))))

(defgeneric + (f g)
  (:method (f g)
   (cl:+ f g))
  (:method (f (g series))
   (series (lons (+ f (lar (coeffs g))) (ldr (coeffs g)))))
  (:method ((f series) g)
   (+ g f))
  (:method ((f series) (g series))
   (series (maplar '+ (coeffs f) (coeffs g)))))

(defun - (f g)
  (+ f (negate g)))

(defun series-* (f g)
  (with-lons (f ft) (coeffs f)
    (with-lons (g gt) (coeffs g)
      (series (lons (* f g)
                    (coeffs (+ (* (series ft)
                                  (series gt))
                               (* f (series gt)))))))))

(defgeneric * (f g)
  (:method (f g)
   (cl:* f g))
  (:method ((f series) g)
   (series (maplar #'(lambda (x) (* x g)) (coeffs f))))
  (:method (f (g series))
   (* g f))
  (:method ((f series) (g series))
   (series-* f g)))

(defun series-/ (f g)
  (with-lons (f ft) (coeffs f)
    (with-lons (g gt) (coeffs g)
      (let ((qs nil))
        (setf qs (lons (/ f g)
                       (maplar #'(lambda (x) (/ x g))
                               (coeffs (- (series ft)
                                          (* (series qs)
                                             (series gt)))))))))))

(defgeneric / (f g)
  (:method (f g)
   (cl:/ f g))
  (:method ((f series) g)
   (series (maplar #'(lambda (x) (/ x g)) (coeffs f))))
  (:method (f (g series))
   (/ (series (lons f (repeat 0))) g))
  (:method ((f series) (g series))
   (series-/ f g)))

(defun int (f)
  (series (lons 0 (maplar '/ (coeffs (force f)) (up-from 1)))))

(defun diff (f)
  (series (maplar '* (ldr (coeffs f)) (up-from 1))))
```



### Example



```lisp
(defparameter *sinx*
  (locally (declare (special *cosx*))
    (delay (int (force *cosx*)))))

(defparameter *cosx*
  (delay (- 1 (int *sinx*))))
```



```txt
FPS > (force-list (take 10 (coeffs (force *sinx*))))
(0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)

FPS > (force-list (take 10 (coeffs (force *cosx*))))
(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0)

FPS > (force-list (take 10 (coeffs (+ (force *cosx*) (force *sinx*)))))
(1 1 -1/2 -1/6 1/24 1/120 -1/720 -1/5040 1/40320 1/362880)

FPS > (force-list (take 10 (coeffs (- (+ 2 (force *cosx*))
                                      (* 3 (force *sinx*))))))
(3 -3 -1/2 1/2 1/24 -1/40 -1/720 1/1680 1/40320 -1/120960)
```



## C

Following is a simple implementation of formal power series in C.  It's not "new datatype for the language" per se, but does demonstrate how lazy evaluation and infinite list generation can be done for this task.  Note that, to be of real use, one should also cache terms looked up and free up memory.  Both are trivially done (I actually had them, but removed them for simplicity).

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h> /* for NaN */

enum fps_type {
        FPS_CONST = 0,
        FPS_ADD,
        FPS_SUB,
        FPS_MUL,
        FPS_DIV,
        FPS_DERIV,
        FPS_INT,
};

typedef struct fps_t *fps;
typedef struct fps_t {
        int type;
        fps s1, s2;
        double a0;
} fps_t;

fps fps_new()
{
        fps x = malloc(sizeof(fps_t));
        x->a0 = 0;
        x->s1 = x->s2 = 0;
        x->type = 0;
        return x;
}

/* language limit of C; when self or mutual recursive definition is needed,
 * one has to be defined, then defined again after it's used.  See how
 * sin and cos are defined this way below
 */
void fps_redefine(fps x, int op, fps y, fps z)
{
        x->type = op;
        x->s1 = y;
        x->s2 = z;
}

fps _binary(fps x, fps y, int op)
{
        fps s = fps_new();
        s->s1 = x;
        s->s2 = y;
        s->type = op;
        return s;
}

fps _unary(fps x, int op)
{
        fps s = fps_new();
        s->s1 = x;
        s->type = op;
        return s;
}

/* Taking the n-th term of series.  This is where actual work is done. */
double term(fps x, int n)
{
        double ret = 0;
        int i;

        switch (x->type) {
        case FPS_CONST: return n > 0 ? 0 : x->a0;
        case FPS_ADD:
                ret = term(x->s1, n) + term(x->s2, n); break;

        case FPS_SUB:
                ret = term(x->s1, n) - term(x->s2, n); break;

        case FPS_MUL:
                for (i = 0; i <= n; i++)
                        ret += term(x->s1, i) * term(x->s2, n - i);
                break;

        case FPS_DIV:
                if (! term(x->s2, 0)) return NAN;

                ret = term(x->s1, n);
                for (i = 1; i <= n; i++)
                        ret -= term(x->s2, i) * term(x, n - i) / term(x->s2, 0);
                break;

        case FPS_DERIV:
                ret = n * term(x->s1, n + 1);
                break;

        case FPS_INT:
                if (!n) return x->a0;
                ret = term(x->s1, n - 1) / n;
                break;

        default:
                fprintf(stderr, "Unknown operator %d\n", x->type);
                exit(1);
        }

        return ret;
}

#define _add(x, y) _binary(x, y, FPS_ADD)
#define _sub(x, y) _binary(x, y, FPS_SUB)
#define _mul(x, y) _binary(x, y, FPS_MUL)
#define _div(x, y) _binary(x, y, FPS_DIV)
#define _integ(x)  _unary(x, FPS_INT)
#define _deriv(x)  _unary(x, FPS_DERIV)

fps fps_const(double a0)
{
        fps x = fps_new();
        x->type = FPS_CONST;
        x->a0 = a0;
        return x;
}

int main()
{
        int i;
        fps one = fps_const(1);
        fps fcos = fps_new();           /* cosine */
        fps fsin = _integ(fcos);        /* sine */
        fps ftan = _div(fsin, fcos);    /* tangent */

        /* redefine cos to complete the mutual recursion; maybe it looks
         * better if I said
         *     *fcos = *( _sub(one, _integ(fsin)) );
         */
        fps_redefine(fcos, FPS_SUB, one, _integ(fsin));

        fps fexp = fps_const(1);        /* exponential */
        /* make exp recurse on self */
        fps_redefine(fexp, FPS_INT, fexp, 0);

        printf("Sin:");   for (i = 0; i < 10; i++) printf(" %g", term(fsin, i));
        printf("\nCos:"); for (i = 0; i < 10; i++) printf(" %g", term(fcos, i));
        printf("\nTan:"); for (i = 0; i < 10; i++) printf(" %g", term(ftan, i));
        printf("\nExp:"); for (i = 0; i < 10; i++) printf(" %g", term(fexp, i));

        return 0;
}
```
Output:
```txt
Sin: 0 1 0 -0.166667 0 0.00833333 0 -0.000198413 0 2.75573e-06
Cos: 1 0 -0.5 0 0.0416667 0 -0.00138889 0 2.48016e-05 0
Tan: 0 1 0 0.333333 0 0.133333 0 0.0539683 0 0.0218695
Exp: 1 1 0.5 0.166667 0.0416667 0.00833333 0.00138889 0.000198413 2.48016e-05 2.75573e-06
```



## D

See [[Formal_power_series/D]].


## EchoLisp

We implement infinite formal power series (FPS) using '''streams'''. No operator overloading in EchoLisp, so we provide the operators '''s-add, s-mul''' ,.. which implement the needed operations. '''poly->stream''' converts a finite polynomial into an infinite FPS, and '''s-value''' gives the value of a FPS at x.

```scheme

(require 'math)
;; converts a finite polynomial (a_0 a_1 .. a_n) to an infinite serie (a_0 ..a_n 0 0 0 ...)
(define (poly->stream list)
	(make-stream (lambda(n) (cons (if (< n (length list)) (list-ref list n) 0) (1+ n))) 0))

;; c = a + b , c_n = a_n + b_n
(define (s-add a b)
	(make-stream (lambda (n) (cons (+ (stream-ref a n) (stream-ref b n)) (1+ n))) 0))

;; c = a * b , c_n = ∑ (0 ..n) a_i * b_n-i
(define (s-mul-coeff n a b) (sigma (lambda(i) (* (stream-ref a i)(stream-ref b (- n i)))) 0 n))

(define (s-mul a b)
	(make-stream (lambda(n) (cons (s-mul-coeff n a b) (1+ n))) 0))

;; b = 1/a ; b_0 = 1/a_0, b_n =  - ∑ (1..n) a_i * b_n-i / a_0
(define (s-inv-coeff n a b)
			(if (zero? n) (/ (stream-ref a 0))
			(- (/ (sigma (lambda(i) (* (stream-ref a i)(stream-ref b (- n i)))) 1 n)
			 (stream-ref a 0)))))

;; note the self keyword which refers to b = (s-inv a)
(define (s-inv a)
	(make-stream (lambda(n) (cons (s-inv-coeff n a self ) (1+ n))) 0))

;; b = (s-k-add k a) = k + a_0, a_1, a_2, ...
(define (s-k-add k a)
	(make-stream (lambda(n) (cons
	(if(zero? n) (+ k (stream-ref a 0)) (stream-ref a n)) (1+ n))) 0))

;; b = (s-neg a) = -a_0,-a_1, ....
(define (s-neg a)
	(make-stream (lambda(n) (cons (- (stream-ref a n)) (1+ n))) 0))

;; b = (s-int a) = ∫ a ; b_0 = 0 by convention, b_n = a_n-1/n
(define (s-int a)
	(make-stream (lambda(n) (cons (if (zero? n) 0 (/ (stream-ref a (1- n)) n)) (1+ n))) 0))

;; value of power serie at x, n terms
(define (s-value a x (n 20))
	(poly x (take a n)))

;; stream-cons allows mutual delayed references
;; sin = ∫ cos
(define sin-x (stream-cons 0 (stream-rest (s-int cos-x))))
;; cos = 1 - ∫ sin
(define cos-x (stream-cons 1 (stream-rest (s-k-add 1  (s-neg (s-int sin-x))))))



```

```scheme

(take cos-x 16)
    → (1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0 -1/3628800 0 1/479001600 0 -1.1470745597729725e-11 0)
(take sin-x 16)
    → (0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880 0 -1/39916800 0 1.6059043836821613e-10 0 -7.647163731819816e-13)

;; compute (cos PI)
(s-value cos-x PI)
    → -1.0000000035290808

;; check that 1 / (1 - x) = 1 + x + x^1 + x^2 + ...
(define fps-1 (poly->stream '( 1 -1)))
(take fps-1 13)
   → (1 -1 0 0 0 0 0 0 0 0 0 0 0)

(define inv-fps-1 (s-inv fps-1))
(take inv-fps-1 13)
    → (1 1 1 1 1 1 1 1 1 1 1 1 1)
(s-value inv-fps-1 0.5) ;; check that 1 / (1 - 0.5) = 2
   → 1.9999980926513672
(s-value inv-fps-1 0.5 100) ;; 100 terms
   → 2


```



## Go


```go
package main

import (
    "fmt"
    "math"
)

// Task:  Formal power series type
//
// Go does not have a concept of numeric types other than the built in
// integers, floating points, and so on.  Nor does it have function or
// operator overloading, or operator defintion.  The type use to implement
// fps here is an interface with a single method, extract.
// While not named in the task description, extract is described in the
// WP article as "important."  In fact, by representing a way to index
// all of the coefficients of a fps, any type that implements the interface
// represents a formal power series.

type fps interface {
    extract(int) float64
}

// Task:  Operations on FPS
//
// Separate operations are implemented with separate extract methods.
// This requires each operation on the fps type to have a concrete type.
// Executing a fps operation is the act of instantiating the concrete type.
// This is implemented here with constructor functions that construct a
// new fps from fps arguments.

// Constructor functions are shown here as a group, followed by concrete
// type definitions and associated extract methods.

func one() fps {
    return &oneFps{}
}

func add(s1, s2 fps) fps {
    return &sum{s1: s1, s2: s2}
}

func sub(s1, s2 fps) fps {
    return &diff{s1: s1, s2: s2}
}

func mul(s1, s2 fps) fps {
    return &prod{s1: s1, s2: s2}
}

func div(s1, s2 fps) fps {
    return &quo{s1: s1, s2: s2}
}

func differentiate(s1 fps) fps {
    return &deriv{s1: s1}
}

func integrate(s1 fps) fps {
    return &integ{s1: s1}
}

// Example:  Mutually recursive defintion of sine and cosine.
// This is a constructor just as those above.  It is nullary and returns
// two fps.  Note sin and cos implemented as instances of other fps defined
// above, and so do not need new concrete types.  Note also the constant
// term of the integration fps provides the case that terminates recursion
// of the extract function.
func sinCos() (fps, fps) {
    sin := &integ{}
    cos := sub(one(), integrate(sin))
    sin.s1 = cos
    return sin, cos
}

// Following are type definitions and extract methods for fps operators
// (constructor functions) just defined.
//
// Goal:  lazy evaluation
//
// Go has no built in support for lazy evaluation, so we make it from
// scratch here.  Types contain, at a minimum, their fps operands and
// representation neccessary to implement lazy evaluation.  Typically
// this is a coefficient slice, although constant terms are not stored,
// so in the case of a constant fps, no slice is needed at all.
// Coefficients are generated only as they are requested.  Computed
// coefficients are stored in the slice and if requested subsequently,
// are returned immediately rather than recomputed.
//
// Types can also contain any other intermediate values useful for
// computing coefficients.

// Constant one:  A constant is a nullary function and no coefficent
// storage is needed so an empty struct is used for the type.
type oneFps struct{}

// The extract method implements the fps interface.  It simply has to
// return 1 for the first term and return 0 for all other terms.
func (*oneFps) extract(n int) float64 {
    if n == 0 {
        return 1
    }
    return 0
}

// Addition is a binary function so the sum type stores its two fps operands
// and its computed terms.
type sum struct {
    s      []float64
    s1, s2 fps
}

func (s *sum) extract(n int) float64 {
    for i := len(s.s); i <= n; i++ {
        s.s = append(s.s, s.s1.extract(i)+s.s2.extract(i))
    }
    return s.s[n]
}

// Subtraction and other binary operations are similar.
// (The common field definitions could be factored out with an embedded
// struct, but the clutter of the extra syntax required doesn't seem
// to be worthwhile.)
type diff struct {
    s      []float64
    s1, s2 fps
}

func (s *diff) extract(n int) float64 {
    for i := len(s.s); i <= n; i++ {
        s.s = append(s.s, s.s1.extract(i)-s.s2.extract(i))
    }
    return s.s[n]
}

type prod struct {
    s      []float64
    s1, s2 fps
}

func (s *prod) extract(n int) float64 {
    for i := len(s.s); i <= n; i++ {
        c := 0.
        for k := 0; k <= i; k++ {
            c += s.s1.extract(k) * s.s1.extract(n-k)
        }
        s.s = append(s.s, c)
    }
    return s.s[n]
}

// Note a couple of fields in addition to those of other binary operators.
// They simply optimize computations a bit.
type quo struct {
    s1, s2 fps
    inv    float64   // optimizes a divide
    c      []float64 // saves multiplications
    s      []float64
}

// WP formula.  Note the limitation s2[0] cannot be 0.  In this case
// the function returns NaN for all terms.  The switch statement catches
// this case and avoids storing a slice of all NaNs.
func (s *quo) extract(n int) float64 {
    switch {
    case len(s.s) > 0:
    case !math.IsInf(s.inv, 1):
        a0 := s.s2.extract(0)
        s.inv = 1 / a0
        if a0 != 0 {
            break
        }
        fallthrough
    default:
        return math.NaN()
    }
    for i := len(s.s); i <= n; i++ {
        c := 0.
        for k := 1; k <= i; k++ {
            c += s.s2.extract(k) * s.c[n-k]
        }
        c = s.s1.extract(i) - c*s.inv
        s.c = append(s.c, c)
        s.s = append(s.s, c*s.inv)
    }
    return s.s[n]
}

// Note differentiation and integration are unary so their types contain
// only a single fps operand.

type deriv struct {
    s   []float64
    s1  fps
}

func (s *deriv) extract(n int) float64 {
    for i := len(s.s); i <= n; {
        i++
        s.s = append(s.s, float64(i)*s.s1.extract(i))
    }
    return s.s[n]
}

type integ struct {
    s   []float64
    s1  fps
}

func (s *integ) extract(n int) float64 {
    if n == 0 {
        return 0 // constant term C=0
    }
    // with constant term handled, s starts at 1
    for i := len(s.s) + 1; i <= n; i++ {
        s.s = append(s.s, s.s1.extract(i-1)/float64(i))
    }
    return s.s[n-1]
}

// Demonstrate working sin, cos.
func main() {
    // Format several terms in a way that is easy to compare visually.
    partialSeries := func(f fps) (s string) {
        for i := 0; i < 6; i++ {
            s = fmt.Sprintf("%s %8.5f ", s, f.extract(i))
        }
        return
    }
    sin, cos := sinCos()
    fmt.Println("sin:", partialSeries(sin))
    fmt.Println("cos:", partialSeries(cos))
}
```

Output:

```txt

sin:   0.00000   1.00000   0.00000  -0.16667   0.00000   0.00833
cos:   1.00000   0.00000  -0.50000   0.00000   0.04167   0.00000

```



## Elisa

The generic component FormalPowerSeries is instantiated with the Rational type as provided by the task [[Arithmetic/Rational]]

```Elisa
component FormalPowerSeries(Number);
  type PowerSeries;
       PowerSeries(Size = integer) -> PowerSeries;

       + PowerSeries -> PowerSeries;
       - PowerSeries -> PowerSeries;

       PowerSeries + PowerSeries -> PowerSeries;
       PowerSeries - PowerSeries -> PowerSeries;
       PowerSeries * PowerSeries -> PowerSeries;

       Integral(PowerSeries)     -> PowerSeries;
       Differential(PowerSeries) -> PowerSeries;

       Zero -> PowerSeries;
       One  -> PowerSeries;

       Array(PowerSeries) -> array(Number);
 begin
       PowerSeries(Size) = PowerSeries:[T = array(Number, Size); Size];

       + A = A;

       - A = [ C = PowerSeries(A.Size);
		       [ i = 1 .. A.Size; C.T[i] := - A.T[i] ];
		     C];

       A + B = [ if A.Size > B.Size then return(B + A);
		         C = PowerSeries(B.Size);
		         [ i = 1 .. A.Size; C.T[i] := A.T[i] + B.T[i] ];
		         [ i = (A.Size +1) .. B.Size;  C.T[i] := B.T[i] ];
		       C];

       A - B = A + (- B );

       A * B = [ C = PowerSeries(A.Size + B.Size - 1);
 	         [ i = 1 .. A.Size;
		     [j = 1.. B.Size;
		         C.T[i + j - 1] := C.T[i + j - 1] + A.T[i] * B.T[j] ] ];
	          C];

      Integral(A) = [ if A.Size == 0 then return (A);
 		      C = PowerSeries(A.Size + 1);
		      [ i = 1 .. A.Size; C.T[i +1] := A.T[i] / Number( i )];
		      C.T[1]:= Number(0);
		      C ];

      Differential(A) = [ if A.Size == 1 then return (A);
		          C = PowerSeries(A.Size - 1);
		          [ i = 1 .. C.Size; C.T[i] := A.T[i + 1] * Number( i )];
		          C ];

      Zero = [ C = PowerSeries (1); C.T[1]:= Number(0);  C];
      One =  [ C = PowerSeries (1); C.T[1]:= Number(1);  C];

      Array(PowerSeries) -> array(Number);
      Array(TS) = TS.T;

end component FormalPowerSeries;

```

Tests

```Elisa
use RationalNumbers;
use FormalPowerSeries(Rational);

 X => symbol;
 term + term => term;
 term / term => term;
 term * term => term;
 symbol ** integer => term;

 Output(text,PowerSeries) -> term;
 Output(Name,PS) = [ E1 := term:symbol(Name); E2:= null(term);
	             [ i = 1..size(Array(PS));
			   Num = Numerator(Array(PS)[i]);
			   if Num <> 0 then
			       [ E2:= term: Num / term: Denominator(Array(PS)[i]) * X ** (i-1);
			         E1:= E1 + E2 ];
		     ];
		    E1];

 Cos(integer) -> PowerSeries;
 Cos(Limit) = [ if Limit == 1 then return(One);
	        ( One - Integral(Integral(Cos (Limit - 1)))) ];

 Sin(integer) -> PowerSeries;
 Sin(Limit) = Integral(Cos (Limit));

 Output("cos = ",Cos(5))?
 Output("sin = ",Sin(5))?

```

Output

```Elisa
cos =  + 1 / 1 * X ** 0 + -1 / 2 * X ** 2 + 1 / 24 * X ** 4 + -1 / 720 * X ** 6 + 1 / 40320 * X ** 8
sin =  + 1 / 1 * X ** 1 + -1 / 6 * X ** 3 + 1 / 120 * X ** 5 + -1 / 5040 * X ** 7 + 1 / 362880 * X ** 9
```



## Haskell

It's simpler to assume we are always dealing with an infinite list of coefficients. Mathematically, a finite power series can be generalized to an infinite power series with trailing zeros.

```haskell
newtype Series a = S { coeffs :: [a] } deriving (Eq, Show)
-- Invariant: coeffs must be an infinite list

instance Num a => Num (Series a) where
  fromInteger n = S $ fromInteger n : repeat 0
  negate (S fs) = S $ map negate fs
  S fs + S gs   = S $ zipWith (+) fs gs
  S (f:ft) * S gs@(g:gt) = S $ f*g : coeffs (S ft * S gs + S (map (f*) gt))

instance Fractional a => Fractional (Series a) where
  fromRational n = S $ fromRational n : repeat 0
  S (f:ft) / S (g:gt) = S qs where qs = f/g : map (/g) (coeffs (S ft - S qs * S gt))

-- utility function to convert from a finite polynomial
fromFiniteList xs = S (xs ++ repeat 0)

int (S fs) = S $ 0 : zipWith (/) fs [1..]

diff (S (_:ft)) = S $ zipWith (*) ft [1..]

sinx,cosx :: Series Rational
sinx = int cosx
cosx = 1 - int sinx

fiboS = 1 / fromFiniteList [1,-1,-1]
```


Output:


```txt

*Main> take 11 $ coeffs sinx
[0 % 1,1 % 1,0 % 1,(-1) % 6,0 % 1,1 % 120,0 % 1,(-1) % 5040,0 % 1,1 % 362880,0 % 1]

*Main> take 11 $ coeffs cosx
[1 % 1,0 % 1,(-1) % 2,0 % 1,1 % 24,0 % 1,(-1) % 720,0 % 1,1 % 40320,0 % 1,(-1) % 3628800]

*Main> take 11 $ coeffs $ sinx / cosx -- tangent
[0 % 1,1 % 1,0 % 1,1 % 3,0 % 1,2 % 15,0 % 1,17 % 315,0 % 1,62 % 2835,0 % 1]

*Main> take 11 $ map truncate $ coeffs $ fiboS  -- some fibonaccis
[1,1,2,3,5,8,13,21,34,55,89]

*Main> take 11 $ coeffs $ fromFiniteList [1,5] * fromFiniteList [2,3,7] -- multiplying polynomials
[2,13,22,35,0,0,0,0,0,0,0]


```



## J


J does not allow the definition of types.  Also, J requires the programmer be lazy, rather than being lazy itself.  With these "limitations" understood, here is an implementation in J:


```J
Ai=: (i.@]  =/ i.@[ -/ i.@>:@-)&#
divide=: [ +/ .*~ [:%.&.x: ] +/ .* Ai
diff=: 1 }. ] * i.@#
intg=: 0 ,  ] % 1 + i.@#
mult=: +//.@(*/)
plus=: +/@,:
minus=: -/@,:
```


See also section 2 at http://www.jsoftware.com/papers/tot1.htm

Note that this approach to division will not be accurate when exact division is not possible (a best fit polynomial will be used).  Note also that if extended precision results are expected the &.x: in the definition of divide should be replaced with &x: or removed entirely (which would then require extended precision arguments).

Example use:
    1 2 1 mult 1 3 3 1
 1 5 10 10 5 1
    1 5 10 10 5 1 divide 1 3 3 1
 1 2 1
    1 0 plus 1 2 1
 2 2 1
    intg@(1 0 minus intg)^:20 i.0    NB. sine
 0 1 0 _0.166667 0 0.00833333 0 _0.000198413 0 2.75573e_6 0 _2.50521e_8 0 1.6059e_10 0 _7.64716e_13 0 2.81146e_15 0 _8.22064e_18 ...
    (1 0 minus intg)@intg^:20 i.0    NB. cosine
 1 0 _0.5 0 0.0416667 0 _0.00138889 0 2.48016e_5 0 _2.75573e_7 0 2.08768e_9 0 _1.14707e_11 0 4.77948e_14 0 _1.56192e_16 0 ...

These sine and cosine results can be compared with taylor series expansions:

    1&o. t. i. 20    NB. sine
 0 1 0 _0.166667 0 0.00833333 0 _0.000198413 0 2.75573e_6 0 _2.50521e_8 0 1.6059e_10 0 _7.64716e_13 0 2.81146e_15 0 _8.22064e_18
    2&o. t. i. 20    NB. cosine
 1 0 _0.5 0 0.0416667 0 _0.00138889 0 2.48016e_5 0 _2.75573e_7 0 2.08768e_9 0 _1.14707e_11 0 4.77948e_14 0 _1.56192e_16 0


## Java

See [[Formal power series/Java]]


## Julia

'''Module''':

```julia
module FormalPowerSeries

_div(a, b) = a / b
_div(a::Union{Integer,Rational}, b::Union{Integer,Rational}) = a // b

abstract type AbstractFPS{T<:Number} end

Base.iteratorsize(::AbstractFPS) = Base.IsInfinite()
Base.done(::AbstractFPS, ::Any) = false
Base.iteratoreltype(::AbstractFPS) = Base.HasEltype()
Base.eltype(::AbstractFPS{T}) where T = T
Base.one(::AbstractFPS{T}) where T = ConstantFPS(one(T))

function Base.show(io::IO, fps::AbstractFPS{T}) where T
    itr = Iterators.take(fps, 8)
    s = start(itr)
    a, s = next(itr, s)
    print(io, a)
    a, s = next(itr, s)
    @printf(io, " %s %s⋅x",
        ifelse(sign(a) ≥ 0, '+', '-'), abs(a))
    local i = 2
    while !done(itr, s)
        a, s = next(itr, s)
        @printf(io, " %s %s⋅x^%i",
            ifelse(sign(a) ≥ 0, '+', '-'), abs(a), i)
        i += 1
    end
    print(io, "...")
end

struct MinusFPS{T,A<:AbstractFPS{T}} <: AbstractFPS{T}
    a::A
end
Base.:-(a::AbstractFPS{T}) where T = MinusFPS{T,typeof(a)}(a)

Base.start(fps::MinusFPS) = start(fps.a)
function Base.next(fps::MinusFPS, st)
    v, s = next(fps.a, st)
    return -v, s
end

struct SumFPS{T,A<:AbstractFPS,B<:AbstractFPS} <: AbstractFPS{T}
    a::A
    b::B
end
Base.:+(a::AbstractFPS{A}, b::AbstractFPS{B}) where {A,B} =
    SumFPS{promote_type(A, B),typeof(a),typeof(b)}(a, b)
Base.:-(a::AbstractFPS, b::AbstractFPS) = a + (-b)

Base.start(fps::SumFPS) = (start(fps.a), start(fps.b))
function Base.next(fps::SumFPS{T,A,B}, st) where {T,A,B}
    stateA, stateB = st
    valueA, stateA = next(fps.a, stateA)
    valueB, stateB = next(fps.b, stateB)
    return T(valueA + valueB), (stateA, stateB)
end

struct ProductFPS{T,A<:AbstractFPS,B<:AbstractFPS} <: AbstractFPS{T}
    a::A
    b::B
end
Base.:*(a::AbstractFPS{A}, b::AbstractFPS{B}) where {A,B} =
    ProductFPS{promote_type(A, B),typeof(a),typeof(b)}(a, b)

Base.start(fps::ProductFPS{T}) where T = (start(fps.a), start(fps.b), T[], T[])
function Base.next(fps::ProductFPS{T,A,B}, st) where {T,A,B}
    stateA, stateB, listA, listB = st
    valueA, stateA = next(fps.a, stateA)
    valueB, stateB = next(fps.b, stateB)
    push!(listA, valueA)
    unshift!(listB, valueB)
    return T(sum(listA .* listB)), (stateA, stateB, listA, listB)
end

struct DifferentiatedFPS{T,A<:AbstractFPS} <: AbstractFPS{T}
    a::A
end
differentiate(fps::AbstractFPS{T}) where T = DifferentiatedFPS{T,typeof(fps)}(fps)

function Base.start(fps::DifferentiatedFPS{T,A}) where {T,A}
    s = start(fps.a)
    _, s = next(fps.a, s)
    n = zero(T)
    return n, s
end
function Base.next(fps::DifferentiatedFPS{T,A}, st) where {T,A}
    n, s = st
    n += one(n)
    v, s = next(fps.a, s)
    return n * v, (n, s)
end

struct IntegratedFPS{T,A<:AbstractFPS} <: AbstractFPS{T}
    a::A
    k::T
end
integrate(fps::AbstractFPS{T}, k::T=zero(T)) where T = IntegratedFPS{T,typeof(fps)}(fps, k)
integrate(fps::AbstractFPS{T}, k::T=zero(T)) where T <: Integer =
    IntegratedFPS{Rational{T},typeof(fps)}(fps, k)

Base.start(fps::IntegratedFPS{T,A}) where {T,A} = zero(T), start(fps.a)
function Base.next(fps::IntegratedFPS{T,A}, st) where {T,A}
    n, s = st
    iszero(n) && return fps.k, (one(n), s)
    v, s = next(fps.a, s)
    r::T = _div(v, n)
    n += one(n)
    return r, (n, s)
end

# Examples of FPS: constant

struct FiniteFPS{T} <: AbstractFPS{T}
    v::NTuple{N,T} where N
end
Base.start(fps::FiniteFPS) = 1
Base.next(fps::FiniteFPS{T}, st) where T =
    st > endof(fps.v) ? (zero(T), st) : (fps.v[st], st + 1)
Base.convert(::Type{FiniteFPS}, x::Real) = FiniteFPS{typeof(x)}((x,))
for op in (:+, :-, :*)
    @eval Base.$op(x::Number, a::AbstractFPS) = $op(FiniteFPS(x), a)
    @eval Base.$op(a::AbstractFPS, x::Number) = $op(a, FiniteFPS(x))
end

struct ConstantFPS{T} <: AbstractFPS{T}
    k::T
end
Base.start(::ConstantFPS) = nothing
Base.next(c::ConstantFPS, ::Any) = c.k, nothing

struct SineFPS{T} <: AbstractFPS{T} end
SineFPS() = SineFPS{Rational{Int}}()
Base.start(::SineFPS) = 0, 1, 1
function Base.next(::SineFPS{T}, st) where T
    n, fac, s = st
    local r::T
    if iseven(n)
        r = zero(T)
    else
        r = _div(one(T), (s * fac))
        s = -s
    end
    n += 1
    fac *= n
    return r, (n, fac, s)
end

struct CosineFPS{T} <: AbstractFPS{T} end
CosineFPS() = CosineFPS{Rational{Int}}()
Base.start(::CosineFPS) = 0, 1, 1
function Base.next(::CosineFPS{T}, st) where T
    n, fac, s = st
    local r::T
    if iseven(n)
        r = _div(one(T), (s * fac))
    else
        r = zero(T)
        s = -s
    end
    n += 1
    fac *= n
    return r, (n, fac, s)
end

end  # module FormalPowerSeries
```


'''Main''':

```julia
@show cosine = FormalPowerSeries.CosineFPS()
@show sine = FormalPowerSeries.SineFPS()

intcosine = FormalPowerSeries.integrate(cosine)
uminintsine = 1 - FormalPowerSeries.integrate(sine)

# Check coefficients up to the 20th term
coefsine = collect(Iterators.take(sine, 20))
coefintcosine = collect(Iterators.take(intcosine, 20))

coefcosine = collect(Iterators.take(cosine, 20))
coefuminintsine = collect(Iterators.take(uminintsine, 20))

@assert coefsine == coefintcosine "The integral of cos should be sin"
@assert coefcosine == coefuminintsine "1 minus the integral of sin should be cos"
```


```txt
cosine = FormalPowerSeries.CosineFPS() = 1//1 + 0//1⋅x - 1//2⋅x^2 + 0//1⋅x^3 + 1//24⋅x^4 + 0//1⋅x^5 - 1//720⋅x^6 + 0//1⋅x^7...
sine = FormalPowerSeries.SineFPS() = 0//1 + 1//1⋅x + 0//1⋅x^2 - 1//6⋅x^3 + 0//1⋅x^4 + 1//120⋅x^5 + 0//1⋅x^6 - 1//5040⋅x^7...
```



## Kotlin

This is a translation of the Java entry except that it uses fractions rather than double precision floating point numbers. The Frac class from the [[Arithmetic/Rational]] task has been embedded in the program for this purpose.


```scala
// version 1.2.10

fun gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

class Frac : Comparable<Frac> {
    val num: Long
    val denom: Long

    companion object {
        val ZERO = Frac(0, 1)
        val ONE  = Frac(1, 1)
    }

    constructor(n: Long, d: Long) {
        require(d != 0L)
        var nn = n
        var dd = d
        if (nn == 0L) {
            dd = 1
        }
        else if (dd < 0) {
            nn = -nn
            dd = -dd
        }
        val g = Math.abs(gcd(nn, dd))
        if (g > 1) {
            nn /= g
            dd /= g
        }
        num = nn
        denom = dd
    }

    constructor(n: Int, d: Int) : this(n.toLong(), d.toLong())

    operator fun plus(other: Frac) =
        Frac(num * other.denom + denom * other.num, other.denom * denom)

    operator fun unaryPlus() = this

    operator fun unaryMinus() = Frac(-num, denom)

    operator fun minus(other: Frac) = this + (-other)

    operator fun times(other: Frac) =
        Frac(this.num * other.num, this.denom * other.denom)

    operator fun rem(other: Frac) = this - Frac((this / other).toLong(), 1) * other

    operator fun inc() = this + ONE
    operator fun dec() = this - ONE

    fun inverse(): Frac {
        require(num != 0L)
        return Frac(denom, num)
    }

    operator fun div(other: Frac) = this * other.inverse()

    fun abs() = if (num >= 0) this else -this

    override fun compareTo(other: Frac): Int {
        val diff = this.toDouble() - other.toDouble()
        return when {
            diff < 0.0  -> -1
            diff > 0.0  -> +1
            else        ->  0
        }
    }

    override fun equals(other: Any?): Boolean {
       if (other == null || other !is Frac) return false
       return this.compareTo(other) == 0
    }

    override fun hashCode() = num.hashCode() xor denom.hashCode()

    override fun toString() = if (denom == 1L) "$num" else "$num/$denom"

    fun toDouble() = num.toDouble() / denom

    fun toLong() = num / denom
}

interface Gene {
    fun coef(n: Int): Frac
}

class Term(private val gene: Gene) {
    private val cache = mutableListOf<Frac>()

    operator fun get(n: Int): Frac {
        if (n < 0) return Frac.ZERO
        if (n >= cache.size) {
            for (i in cache.size..n) cache.add(gene.coef(i))
        }
        return cache[n]
    }
}

class FormalPS {
    private lateinit var term: Term

    private companion object {
        const val DISP_TERM = 12
        const val X_VAR = "x"
    }

    constructor() {}

    constructor(term: Term) {
        this.term = term
    }

    constructor(polynomial: List<Frac>) :
        this(Term(object : Gene {
            override fun coef(n: Int) =
                if (n < 0 || n >= polynomial.size)
                    Frac.ZERO
                else
                    polynomial[n]
        }))

    fun copyFrom(other: FormalPS) {
        term = other.term
    }

    fun inverseCoef(n: Int): Frac {
        val res = Array(n + 1) { Frac.ZERO }
        res[0] = term[0].inverse()
        for (i in 1..n) {
            for (j in 0 until i) res[i] += term[i - j] * res[j]
            res[i] *= -res[0]
        }
        return res[n]
    }

    operator fun plus(other: FormalPS) =
        FormalPS(Term(object : Gene {
            override fun coef(n: Int) = term[n] + other.term[n]
        }))

    operator fun minus(other: FormalPS) =
        FormalPS(Term(object : Gene {
            override fun coef(n: Int) = term[n] - other.term[n]
        }))

    operator fun times(other: FormalPS) =
        FormalPS(Term(object : Gene {
            override fun coef(n: Int): Frac {
                var res = Frac.ZERO
                for (i in 0..n) res += term[i] * other.term[n - i]
                return res
            }
        }))

    operator fun div(other: FormalPS) =
        FormalPS(Term(object : Gene {
            override fun coef(n: Int): Frac {
                var res = Frac.ZERO
                for (i in 0..n) res += term[i] * other.inverseCoef(n - i)
                return res
            }
        }))

    fun diff() =
        FormalPS(Term(object : Gene {
            override fun coef(n: Int) = term[n + 1] * Frac(n + 1, 1)
        }))

    fun intg() =
        FormalPS(Term(object : Gene {
            override fun coef(n: Int) =
                if (n == 0) Frac.ZERO else term[n - 1] * Frac(1, n)
        }))

    override fun toString() = toString(DISP_TERM)

    private fun toString(dpTerm: Int): String {
        val sb = StringBuilder()
        var c = term[0]
        if (c != Frac.ZERO) sb.append(c.toString())
        for (i in 1 until dpTerm) {
            c = term[i]
            if (c != Frac.ZERO) {
                if (c > Frac.ZERO && sb.length > 0) sb.append(" + ")
                sb.append (when {
                    c == Frac.ONE  -> X_VAR
                    c == -Frac.ONE -> " - $X_VAR"
                    c.num < 0      -> " - ${-c}$X_VAR"
                    else           -> "$c$X_VAR"
                })
                if (i > 1) sb.append("^$i")
            }
        }
        if (sb.length == 0) sb.append("0")
        sb.append(" + ...")
        return sb.toString()
    }
}

fun main(args: Array<String>) {
    var cos = FormalPS()
    val sin = cos.intg()
    cos.copyFrom(FormalPS(listOf(Frac.ONE)) - sin.intg())
    println("SIN(x) = $sin")
    println("COS(x) = $cos")
}
```


```txt

SIN(x) = x - 1/6x^3 + 1/120x^5 - 1/5040x^7 + 1/362880x^9 - 1/39916800x^11 + ...
COS(x) = 1 - 1/2x^2 + 1/24x^4 - 1/720x^6 + 1/40320x^8 - 1/3628800x^10 + ...

```



## jq

### =Introduction and Examples=

Since a formal power series can be viewed as a function from the non-negative integers onto a suitable range,
we shall identify a jq filter that maps integers to the appropriate range as a power series. For example, the jq function

```jq
1/(1+.)
```

represents the power series 1 + x/2 + x/3 + ... because 1/(1+.) maps i to 1/(i+1).

Similarly, the jq filter 1 (i.e. the filter that always returns 1) represents the power series Σ x^i.

The exponential power series, Σ (x^i)/i!, can be represented in jq by the filter:

```jq>1/factorial</lang


assuming "factorial" is defined in the usual way:

```jq
def factorial:
  reduce range(1; . + 1) as $i
    (1; . * $i);
```


For ease of reference, we shall also define ps_exp as 1/factorial:

```jq
def ps_exp: 1/factorial;
```


In a later subsection of this article, we will define another function, ps_evaluate(p), for evaluating the power series, p, at the value specified by the input, so for example:

```jq
1 | ps_evaluate(ps_exp)
```

should evaluate to the number e approximately; using the version of ps_evaluate defined below, we find:

```jq
1 | ps_evaluate(1/factorial)
```

evaluates to 2.7182818284590455.

The following function definitions are useful for other power series:

```jq
def pow(n):
  . as $x | n as $n
  | reduce range(0;$n) as $i (1; . * $x);
```

For example, the power series 1 + Σ ( (x/i)^n ) where the summation is over i>0 can be written:

```jq
1/pow(.)
```


The power series representation of ln(1 + x) is as follows:

```jq
# ln(1+x) = x - x^2 / 2 + ...
def ln_1px:
  def c: if . % 2 == 0 then -1 else 1 end;
  . as $i | if $i == 0 then 0 else ($i|c) / $i  end;

```

jq numbers are currently implemented using IEEE 754 64-bit arithmetic, and therefore this article will focus on power series that can be adequately represented using jq numbers. However, the approach used here can be used for power series defined on other domains, e.g. rationals, complex numbers, and so on.


### =Finite power series=

To make it easy to represent finite power series, we define poly(ary) as follows:

```jq
def poly(ary): ary[.] // 0;
```


For example, poly( [1,2,3] ) represents the finite power series: 1 + 2x + 3x^2.

(The "// 0" ensures that the result is 0 for integers that are out-of-range with respect to the array.)


### =Addition and Subtraction=

jq's "+" operator can be used to add two power series with the intended semantics;
for example:

```jq
(poly([1,2,3]) + poly([-1,-2,-3]))
```


is equal to poly([]), i.e. 0.

This is simply because in jq, (i | (poly([1,2,3]) + poly([-1,-2,-3]))) evaluates to  (i | (poly([1,2,3])) + (i|poly([-1,-2,-3]))).

Subtraction works in the same way and for the same reason. The product of two power series, however, must be handled specially.

====Multiplication, Differentiation and Integration====

```jq
# Multiply two power series, s and t:
def M(s;t):
  . as $i | reduce range(0; 1+$i) as $k
    (0; . + ($k|s) * (($i - $k)|t));

# Derivative of the power series, s:
def D(s): (. + 1) as $i | $i * ($i|s);

# Integral of the power series, s,
# with an integration constant equal to 0:
def I(s):
  . as $i
  | if $i == 0 then 0 else (($i-1)|s) /$i end;

```


### =Equality and Evaluation=

The following function, ps_equal(s;t;k;eps) will check whether the first k coefficients of the two power series agree to within eps:

```jq
def ps_equal(s; t; k; eps):
  def abs: if . < 0 then -. else . end;
  reduce range(0;k) as $i
    (true;
     if . then ((($i|s) - ($i|t))|abs) <= eps
     else .
     end);
```

To evaluate a power series, P(x), at a particular point, say y, we
can define a function, ps_evaluate(p), so that (y|ps_evaluate(p))
evaluates to P(y), assuming that P(x) converges sufficiently rapidly
to a value that can be represented using IEEE 754 64-bit arithmetic.

```jq
# evaluate p(x) based on the first k terms of polynomial p, where x is the input
def ps_eval(p; k):
  . as $x
  | reduce range(0;k) as $i
    # state: [sum, x^i]
      ([0, 1];
       .[1] as $xn
       | ($i|p) as $coeff
       | [ .[0] + $coeff * $xn, $x * $xn])
  | .[0];

# If |x| < 1 then ps_evaluate(x) will evaluate to p(x) with high precision
# if the coefficients of the polynomial are eventually bounded.
#
# WARNING: ps_evaluate(p) will not detect divergence and is not intended to
# produce accurate results unless the terms of p(x) are reasonably well-behaved.
# For |x| > 1, the result will be null if x^n overflows before convergence is achieved.
#
def ps_evaluate(p):
  def abs: if . < 0 then -. else . end;
  def eval(p;x):
    # state: [i, x^i, sum of i terms, delta, prevdelta]
    recurse(
      .[0] as $i
      | .[1] as $xi
      | .[2] as $sum
      | .[3] as $delta
      | .[4] as $prevdelta
      | if $delta < 1e-17 and $prevdelta < 1e-17
           and ( $xi < 1e-100
                 or ( $sum != 0 and
                      (($delta/$sum) | abs) < 1e-10 and
                      (($prevdelta/$sum) | abs) < 1e-10) )
        then empty
        else
          ($xi * ($i|p)) as $newdelta
        | [ $i + 1,
            x*$xi,
            $sum+$newdelta,
            ($newdelta|abs), $delta]
        end ) ;
   . as $x
   | [0, 1, 0, 1, 1]
   | reduce eval(p; $x) as $vector (0; $vector[2]);
```


### =Examples=


```jq
# Utility functions:

def abs: if . < 0 then -. else . end;

# The power series whose only non-zero coefficient is 1 at x^i:
def ps_at(i): if . ==  i then 1 else 0 end;

# Create an array consisting of the first . coefficients of the power series, p:
def ps_to_array(p): . as $in | reduce range(0;$in) as $i ([]; . + [$i|p]);

def pi: 4 * (1|atan);

```

''''cos == I(sin)''''

```jq

# Verify that the first 100 terms of I(cos) and of sin are the same:

ps_equal( I(ps_cos); ps_sin; 100; 1e-15)
# => true

# Verify that the two power series agree when evaluated at pi:

((pi | ps_evaluate(I(ps_cos))) - (pi | ps_evaluate(ps_sin))) | abs < 1e-15
# => true
```

''''cos == 1 - I(sin)''''

```jq
# Verify that the first 100 terms of cos and (1 - I(sin)) are the same:

ps_equal( ps_cos; ps_at(0) - I(ps_sin); 100; 1e-5)
# => true

# Verify that the two power series agree at pi:

((pi | ps_evaluate(ps_cos)) - (pi | ps_evaluate(ps_at(0) - I(ps_sin)))) | abs < 1e-15
# => true
```



## Maxima


```maxima
deftaylor(f(x), sum(n! * x^n, n, 0, inf))$

taylor(f(x), x, 0, 10);
/ * 1 + x + 2 * x^2 + 6 * x^3 + 24 * x^4 + 120 * x^5 + 720 * x^6 + 5040 * x^7 + 40320 * x^8 + 362880 * x^9 + 3628800 * x^10 + ... * /

taylor(f(x)^2, x, 0, 10);
/ * 1 + 2 * x + 5 * x^2 + 16 * x^3 + 64 * x^4 + 312 * x^5 + 1812 * x^6 + 12288 * x^7 + 95616 * x^8 + 840960 * x^9 + 8254080 * x^10 + ... * /


deftaylor(fcos(x), sum((-1)^n * x^(2 * n) / (2 * n)!, n, 0, inf))$
deftaylor(fsin(x), sum((-1)^n * x^(2 * n + 1) / (2 * n + 1)!, n, 0, inf))$

taylor(fcos(x)^2 + fsin(x)^2, x, 0, 20);
/ * 1 + ... * /
```



## Lua

Parts of this depend on the formula for integration of a power series: integral(sum(a_n x^n)) = sum(a_n / n * x(n+1))


```lua
powerseries = setmetatable({
__add = function(z1, z2) return powerseries(function(n) return z1.coeff(n) + z2.coeff(n) end) end,
__sub = function(z1, z2) return powerseries(function(n) return z1.coeff(n) - z2.coeff(n) end) end,
__mul = function(z1, z2) return powerseries(function(n)
  local ret = 0
  for i = 0, n do
    ret = ret + z1.coeff(i) * z2.coeff(n-i)
  end
  return ret
end) end,
__div = function(z1, z2) return powerseries(function(n)
  local ret = z1.coeff(n)
  local function coeffs(a)
    local c = z1.coeff(a)
	for j = 0, a - 1 do c = c - coeffs(j) * z2.coeff(a-j) end
	return c / z2.coeff(0)
  end
  for i = 0, n-1 do
    ret = ret - coeffs(i) * z2.coeff(n-i)
  end
  return ret / z2.coeff(0)
end) end,
__pow = function(z1, p) -- for a series z, z^n returns the nth derivative of z. negative values take integrals.
  if p == 0 then return z1
  elseif p > 0 then return powerseries(function(i) return z1.coeff(i+1)*(i+1) end)^(p-1)
  else return powerseries(function(i) return z1.coeff(i-1)/i end)^(p+1)
  end
end,
__unm = function(z1) return powerseries(function(n) return -z1.coeff(n) end) end,
__index = function(z, n) return z.coeff(n) end,
__call = function(z, n)
  local ret = 0
  for i = 0, 15 do --we do 20 terms, which is simpler than trying to check error bounds
    ret = ret + z[i]*(n^i)
  end
  return ret
end},
{__call = function(z, f) return setmetatable({coeff = f}, z) end})

cosine = powerseries(function(n)
  if(n == 0) then return 1
  else return -((sine^(-1))[n]) --defer to the integral of sine function
  end
end)

sine = powerseries(function(n)
  if(n == 0) then return 0
  else return (cosine^(-1))[n] --defer to the integral of cosine function
  end
end)

print(sine[1], sine[3], sine[5], sine[7], cosine[0], cosine[2], cosine[4], cosine[6])
print(sine(math.pi/3), sine(math.pi/2), cosine(math.pi/3), cosine(math.pi/2))

tangent = sine / cosine
print(tangent(math.pi/3), tangent(math.pi/4), tangent(math.pi/6)) --something like 30000 function calls!
```



## Mathematica

Mathematica natively supports symbolic power series. For example, this input demonstrates that the integral of the series of Cos minus the series for sin is zero to the order of cancellation.

```Mathematica
cos = Series[Cos[x], {x, 0, 10}];
sin = Series[Sin[x], {x, 0, 8}];
sin - Integrate[cos, x]
```

```txt
O[x]^9
```



## PARI/GP

Uses the built-in power series handling.  Change default(seriesprecision) to get more terms.

```parigp
sin('x)
cos('x)
```



## Perl

Although true Lazy Lists *can* be implemented using perl (using the builtin "tie" function), I felt that doing so would make the example harder to understand.

Instead, I chose to implement delayed evaluation with a generator function and a cache.

Creating a new arithmetic type in perl is relatively easy, using the "overload" module which comes with perl.

This was partly inspired by the perl6 example, but is far from being a direct translation.


```perl

package FPS;
use strict;
use warnings;
use Math::BigRat;

sub new {
   my $class = shift;
   return bless {@_}, $class unless @_ == 1;
   my $arg = shift;
   return bless { more => $arg }, $class if 'CODE' eq ref $arg;
   return bless { coeff => $arg }, $class if 'ARRAY' eq ref $arg;
   bless { coeff => [$arg] }, $class;
}

sub coeff {
   my ($self, $i) = @_;
   my $cache = ($self->{coeff} ||= []);
   my $more = $self->{more};
   for my $j ( @$cache .. $i ) {
      last unless $more;
      $cache->[$j] = $more->($j, $self);
   }
   $cache->[$i] or 0;
}

sub invert {
   my $orig = shift;
   ref($orig)->new( sub {
      my ($i, $self) = @_;
      unless( $i ) {
         my $a0 = $orig->coeff(0);
         die "Cannot invert power series with zero constant term."
            unless $a0;
         (Math::BigRat->new(1) / $a0);
      } else {
         my $sum = 0;
         my $terms = $self->{coeff};
         for my $j (1 .. $i) {
            $sum += $orig->coeff($j) * $terms->[$i - $j];
         }
         -$terms->[0] * $sum;
      }
   } );
}

sub fixargs {
   my ($x, $y, $swap) = @_;
   my $class = ref $x;
   $y = $class->new($y) unless UNIVERSAL::isa($y, $class);
   ($x, $y) = ($y, $x) if $swap;
   ($class, $x, $y);
}

use overload '+' => sub {
   my ($class, $x, $y) = &fixargs;
   $class->new( sub { $x->coeff($_[0]) + $y->coeff($_[0]) } );
}, '-' => sub {
   my ($class, $x, $y) = &fixargs;
   $class->new( sub { $x->coeff($_[0]) - $y->coeff($_[0]) } );
}, '*' => sub {
   my ($class, $x, $y) = &fixargs;
   $class->new( sub {
      my $i = shift;
      my $sum = 0;
      $sum += $x->coeff($_) * $y->coeff($i-$_) for 0..$i;
      $sum;
   } );
}, '/' => sub {
   my ($class, $x, $y) = &fixargs;
   $x * $y->invert;
}, '""' => sub {
   my $self = shift;
   my $str = $self->coeff(0);
   for my $i (1..10) {
      my $c = $self->coeff($i);
      next unless $c;
      $str .= ($c < 0) ? (" - " . (-$c)) : (" + ".$c);
      $str .= "x^$i";
   }
   $str;
};

sub differentiate {
   my $orig = shift;
   ref($orig)->new( sub {
      my $i = shift;
      ($i+1) * $orig->coeff($i);
   } );
}

sub integrate {
   my $orig = shift;
   ref($orig)->new( coeff => [0], more => sub {
      my $i = shift;
      $orig->coeff($i-1) / Math::BigRat->new($i);
   } );
}

my $sin = __PACKAGE__->new;
my $cos = 1 - $sin->integrate;
%$sin = %{$cos->integrate};
my $tan = $sin / $cos;
my $exp = __PACKAGE__->new();
%$exp = (%{$exp->integrate}, coeff => [1]);

print "sin(x) ~= $sin\n";
print "cos(x) ~= $cos\n";
print "tan(x) ~= $tan\n";
print "exp(x) ~= $exp\n";

print "sin^2 + cos^s = ", $sin*$sin + $cos*$cos, "\n";

1;
__END__

```

```txt

sin(x) ~= 0 + 1x^1 - 1/6x^3 + 1/120x^5 - 1/5040x^7 + 1/362880x^9
cos(x) ~= 1 - 1/2x^2 + 1/24x^4 - 1/720x^6 + 1/40320x^8 - 1/3628800x^10
tan(x) ~= 0 + 1x^1 + 1/3x^3 + 2/15x^5 + 17/315x^7 + 62/2835x^9
exp(x) ~= 1 + 1x^1 + 1/2x^2 + 1/6x^3 + 1/24x^4 + 1/120x^5 + 1/720x^6 + 1/5040x^7 + 1/40320x^8 + 1/362880x^9 + 1/3628800x^10
sin^2 + cos^s = 1

```


For a version which *does* use proper lazy lists, see [[Formal power series/Perl]]


## Perl 6


```perl6
class DerFPS { ... }
class IntFPS { ... }

role FPS {
    method coeffs        { ... }
    method differentiate { DerFPS.new(:x(self)) }
    method integrate     { IntFPS.new(:x(self)) }

    method pretty($n) {
        sub super($i) { $i.trans('0123456789' => '⁰¹²³⁴⁵⁶⁷⁸⁹') }
        my $str = $.coeffs[0].perl;
        for 1..$n Z $.coeffs[1..$n] -> $i, $_ {
            when * > 0 { $str ~= " + {(+$_).perl}∙x{super($i)}" }
            when * < 0 { $str ~= " - {(-$_).perl}∙x{super($i)}" }
        }
        $str;
    }
}

class ExplicitFPS does FPS { has @.coeffs }

class SumFPS does FPS {
    has FPS ($.x, $.y);
    method coeffs { $.x.coeffs Z+ $.y.coeffs }
}

class DifFPS does FPS {
    has FPS ($.x, $.y);
    method coeffs { $.x.coeffs Z- $.y.coeffs }
}

class ProFPS does FPS {
    has FPS ($.x, $.y);
    method coeffs { (0..*).map: { [+] ($.x.coeffs[0..$_] Z* $.y.coeffs[$_...0]) } }
}

class InvFPS does FPS {
    has FPS $.x;
    method coeffs {
        # see http://en.wikipedia.org/wiki/Formal_power_series#Inverting_series
        gather {
            my @a := $.x.coeffs;
            @a[0] != 0 or fail "Cannot invert power series with zero constant term.";
            take my @b = (1 / @a[0]);
            take @b[$_] = -@b[0] * [+] (@a[1..$_] Z* @b[$_-1...0]) for 1..*;
        }
    }
}

class DerFPS does FPS {
    has FPS $.x;
    method coeffs { (1..*).map: { $_ * $.x.coeffs[$_] } }
}

class IntFPS does FPS {
    has FPS $.x;
    method coeffs { 0, (0..*).map: { $.x.coeffs[$_] / ($_+1) } }
}

class DeferredFPS does FPS {
    has FPS $.realized is rw;
    method coeffs { $.realized.coeffs }
}

# some arithmetic operations for formal power series
multi infix:<+>(FPS $x, FPS $y) { SumFPS.new(:$x, :$y) }
multi infix:<->(FPS $x, FPS $y) { DifFPS.new(:$x, :$y) }
multi infix:<*>(FPS $x, FPS $y) { ProFPS.new(:$x, :$y) }
multi infix:</>(FPS $x, FPS $y) { $x * InvFPS.new(:x($y)) }

# an example of a mixed-type operator:
multi infix:<->(Numeric $x, FPS $y) { ExplicitFPS.new(:coeffs($x, 0 xx *)) - $y }

# define sine and cosine in terms of each other
my $sin       = DeferredFPS.new;
my $cos       = 1 - $sin.integrate;
$sin.realized = $cos.integrate;

# define tangent in terms of sine and cosine
my $tan       = $sin / $cos;

say 'sin(x) ≈ ', $sin.pretty(10);
say 'cos(x) ≈ ', $cos.pretty(10);
say 'tan(x) ≈ ', $tan.pretty(10);
```


```txt
sin(x) ≈ 0 + 1/1∙x¹ - 1/6∙x³ + 1/120∙x⁵ - 1/5040∙x⁷ + 1/362880∙x⁹
cos(x) ≈ 1 - 1/2∙x² + 1/24∙x⁴ - 1/720∙x⁶ + 1/40320∙x⁸ - 1/3628800∙x¹⁰
tan(x) ≈ 0/1 + 1/1∙x¹ + 1/3∙x³ + 2/15∙x⁵ + 17/315∙x⁷ + 62/2835∙x⁹
```



## Phix

```Phix
enum type fps_type FPS_UNDEF = 0,
                   FPS_CONST,
                   FPS_ADD,
                   FPS_SUB,
                   FPS_MUL,
                   FPS_DIV,
                   FPS_DERIV,
                   FPS_INT
end type

enum FPS_TYPE, FPS_S1, FPS_S2, FPS_A0
sequence fpss = {}

type fps(object id)
    return integer(id) and id>=1 and id<=length(fpss)
end type

type fpsn(object id)
    return id=NULL or fps(id)
end type

function fps_new(fps_type ft=FPS_UNDEF, fpsn s1=0, s2=0, atom a0=0)
    fpss = append(fpss,{ft,s1,s2,a0})
    fps fpsid = length(fpss)
    return fpsid
end function

-- as per C, for (eg) self or mutually recursive definitions.
procedure fps_redefine(fps fpsid, fps_type ft, fpsn s1id, s2id, object a0="")
    fpss[fpsid][FPS_TYPE] = ft
    fpss[fpsid][FPS_S1] = s1id
    fpss[fpsid][FPS_S2] = s2id
    if atom(a0) then
        fpss[fpsid][FPS_A0] = a0
    end if
end procedure

function fps_const(atom a0)
    fps x = fps_new(FPS_CONST,a0:=a0)
    -- (aside: in the above, the ":=a0" refers to the local namespace
    --         as usual, whereas "a0:=" refers to the param namespace
    --         /inside/ the () of fps_new(), so there is no conflict.)
    return x
end function

constant INF = 1e300*1e300,
         NAN = -(INF/INF)

/* Taking the n-th term of series.  This is where actual work is done. */
function term(fps x, int n)
    atom ret = 0

    {fps_type ft, fpsn s1id, fpsn s2id, atom a0} = fpss[x]
    --  FPS_TYPE,    FPS_S1,    FPS_S2,  FPS_A0 <-- nb above must match
    switch ft do
        case FPS_CONST: ret := iff(n>0 ? 0 : a0)
        case FPS_ADD:   ret := term(s1id, n) + term(s2id, n)
        case FPS_SUB:   ret := term(s1id, n) - term(s2id, n)
        case FPS_MUL:
                for i=0 to n do
                        ret += term(s1id, i) * term(s2id, n-i)
                end for
        case FPS_DIV:
                if not term(s2id, 0) then return NAN end if
                ret = term(s1id, n)
                for i=1 to n do
                        ret -= term(s2id, i) * term(x, n-i) / term(s2id, 0)
                end for
        case FPS_DERIV: ret := n * term(s1id, n+1)
        case FPS_INT:   ret := iff(n=0 ? a0 : term(s1id, n-1)/n)
        default:        ret := 9/0 -- (fatal error)
    end switch
    return ret
end function

procedure term9(string txt, fps x)
    printf(1,"%s:",{txt})
    for i=0 to 9 do printf(1," %g", term(x, i)) end for
    printf(1,"\n")
end procedure

procedure main()
    fps one = fps_const(1)
    fps fcos = fps_new()                    /* cosine */
    fps fsin = fps_new(FPS_INT,fcos)        /* sine */
    fps ftan = fps_new(FPS_DIV,fsin,fcos)   /* tangent */

    /* redefine cos to complete the mutual recursion */
    fps_redefine(fcos, FPS_SUB, one, fps_new(FPS_INT,fsin))

    fps fexp = fps_const(1);        /* exponential */
    /* make exp recurse on self */
    fps_redefine(fexp, FPS_INT, fexp, 0);

    term9("Sin",fsin)
    term9("Cos",fcos)
    term9("Tan",ftan)
    term9("Exp",fexp)
end procedure
main()
```

```txt

Sin: 0 1 0 -0.166667 0 0.00833333 0 -0.000198413 0 2.75573e-6
Cos: 1 0 -0.5 0 0.0416667 0 -0.00138889 0 2.48016e-5 0
Tan: 0 1 0 0.333333 0 0.133333 0 0.0539683 0 0.0218695
Exp: 1 1 0.5 0.166667 0.0416667 0.00833333 0.00138889 0.000198413 2.48016e-5 2.75573e-6

```



## PicoLisp

With a 'lazy' function, as a frontend to '[http://software-lab.de/doc/refC.html#cache cache]',

```PicoLisp
(de lazy Args
   (def (car Args)
      (list (cadr Args)
         (cons 'cache (lit (cons))
            (caadr Args)
            (cddr Args) ) ) ) )
```

we can build a formal power series functionality:

```PicoLisp
(scl 20)

(de fpsOne (N)
   (if (=0 N) 1.0 0) )

(de fpsInverse (N X)
   (last
      (make
         (let Res1 (- (link (*/ 1.0 1.0 (X 0))))
            (for I N
               (link
                  (*/
                     (sum '((Res J) (*/ (X J) Res 1.0))
                        (made)
                        (range I 1) )
                     Res1
                     1.0 ) ) ) ) ) ) )

(de fpsAdd (N X Y)
   (+ (X N) (Y N)) )

(de fpsSub (N X Y)
   (- (X N) (Y N)) )

(de fpsMul (N X Y)
   (sum
      '((I)
         (*/ (X I) (Y (- N I)) 1.0) )
      (range 0 N) ) )

(de fpsDiv (N X Y)
   (sum
      '((I)
         (*/ (X I) (fpsInverse (- N I) Y) 1.0) )
      (range 0 N) ) )

(de fpsDifferentiate (N)
   (curry (X) (N)
      (* (X (inc N)) N) ) )

(de fpsIntegrate (X)
   (curry (X) (N)
      (or
         (=0 N)
         (*/ (X (dec N)) N) ) ) )

(lazy fpsSin (N)
   ((fpsIntegrate fpsCos) N) )

(lazy fpsCos (N)
   (fpsSub N fpsOne (fpsIntegrate fpsSin)) )

(lazy fpsTan (N)
   (fpsDiv N fpsSin fpsCos) )

(lazy fpsExp (N)
   (if (=0 N)
      1.0
      ((fpsIntegrate fpsExp) N) ) )
```

Test:

```PicoLisp
(prin "SIN:")
(for N (range 1 11 2)
   (prin " " (round (fpsSin N) 9)) )
(prinl)

(prin "COS:")
(for N (range 0 10 2)
   (prin " " (round (fpsCos N) 9)) )
(prinl)

(prin "TAN:")
(for N (range 1 13 2)
   (prin " " (round (fpsTan N) 7)) )
(prinl)

(prin "EXP:")
(for N (range 0 6)
   (prin " " (round (fpsExp N) 7)) )
(prinl)
```

Output:

```txt
SIN: 1.000000000 -0.166666667 0.008333333 -0.000198413 0.000002756 -0.000000025
COS: 1.000000000 -0.500000000 0.041666667 -0.001388889 0.000024802 -0.000000276
TAN: 1.0000000 0.3333333 0.1333333 0.0539683 0.0218695 0.0088632 0.0035921
EXP: 1.0000000 1.0000000 0.5000000 0.1666667 0.0416667 0.0083333 0.0013889
```



## Python

```python
''' \
For a discussion on pipe() and head() see
  http://paddy3118.blogspot.com/2009/05/pipe-fitting-with-python-generators.html
'''

from itertools import islice
from fractions import Fraction
from functools import reduce
try:
    from itertools import izip as zip # for 2.6
except:
    pass

def head(n):
    ''' return a generator that passes through at most n items
    '''
    return lambda seq: islice(seq, n)

def pipe(gen, *cmds):
    ''' pipe(a,b,c,d, ...) -> yield from ...d(c(b(a)))
    '''
    return reduce(lambda gen, cmd: cmd(gen), cmds, gen)

def sinepower():
    n = 0
    fac = 1
    sign = +1
    zero = 0
    yield zero
    while True:
        n +=1
        fac *= n
        yield Fraction(1, fac*sign)
        sign = -sign
        n +=1
        fac *= n
        yield zero
def cosinepower():
    n = 0
    fac = 1
    sign = +1
    yield Fraction(1,fac)
    zero = 0
    while True:
        n +=1
        fac *= n
        yield zero
        sign = -sign
        n +=1
        fac *= n
        yield Fraction(1, fac*sign)
def pluspower(*powergenerators):
    for elements in zip(*powergenerators):
        yield sum(elements)
def minuspower(*powergenerators):
    for elements in zip(*powergenerators):
        yield elements[0] - sum(elements[1:])
def mulpower(fgen,ggen):
    'From: http://en.wikipedia.org/wiki/Power_series#Multiplication_and_division'
    a,b = [],[]
    for f,g in zip(fgen, ggen):
        a.append(f)
        b.append(g)
        yield sum(f*g for f,g in zip(a, reversed(b)))
def constpower(n):
    yield n
    while True:
        yield 0
def diffpower(gen):
    'differentiatiate power series'
    next(gen)
    for n, an in enumerate(gen, start=1):
        yield an*n
def intgpower(k=0):
    'integrate power series with constant k'
    def _intgpower(gen):
        yield k
        for n, an in enumerate(gen, start=1):
            yield an * Fraction(1,n)
    return _intgpower


print("cosine")
c = list(pipe(cosinepower(), head(10)))
print(c)
print("sine")
s = list(pipe(sinepower(), head(10)))
print(s)
# integrate cosine
integc = list(pipe(cosinepower(),intgpower(0), head(10)))
# 1 - (integrate sine)
integs1 = list(minuspower(pipe(constpower(1), head(10)),
                          pipe(sinepower(),intgpower(0), head(10))))

assert s == integc, "The integral of cos should be sin"
assert c == integs1, "1 minus the integral of sin should be cos"
```


'''Sample output'''

```txt
cosine
[Fraction(1, 1), 0, Fraction(-1, 2), 0, Fraction(1, 24), 0, Fraction(-1, 720), 0, Fraction(1, 40320), 0]
sine
[0, Fraction(1, 1), 0, Fraction(-1, 6), 0, Fraction(1, 120), 0, Fraction(-1, 5040), 0, Fraction(1, 362880)]

```



### Using cyclic iterators

Alternate version that uses a generator function to allow sine and cosine to be defined recursively, following the same method as [[Hamming numbers#Alternate version using "Cyclic Iterators"]]:
```python
from itertools import islice, tee
from fractions import Fraction
try:
    from itertools import izip as zip # for 2.6
except:
    pass

def pluspower(*powergenerators):
    for elements in zip(*powergenerators):
        yield sum(elements)
def minuspower(*powergenerators):
    for elements in zip(*powergenerators):
        yield elements[0] - sum(elements[1:])
def mulpower(fgen,ggen):
    'From: http://en.wikipedia.org/wiki/Power_series#Multiplication_and_division'
    a,b = [],[]
    for f,g in zip(fgen, ggen):
        a.append(f)
        b.append(g)
        yield sum(f*g for f,g in zip(a, reversed(b)))
def constpower(n):
    yield n
    while True:
        yield 0
def diffpower(gen):
    'differentiatiate power series'
    next(gen)
    for n, an in enumerate(gen, start=1):
        yield an*n
def intgpower(gen):
    'integrate power series with bounds from 0 to x'
    yield 0
    for n, an in enumerate(gen, start=1):
        yield an * Fraction(1,n)


def sine_cosine_series():
    def deferred_sin():
        for i in sinx_temp:
            yield i
    def deferred_cos():
        for i in cosx_temp:
            yield i

    sinx_result, sinx_copy1 = tee(deferred_sin(), 2)
    cosx_result, cosx_copy1 = tee(deferred_cos(), 2)

    sinx_temp = intgpower(cosx_copy1)
    cosx_temp = minuspower(constpower(1), intgpower(sinx_copy1))

    return sinx_result, cosx_result

sinx, cosx = sine_cosine_series()

print("cosine")
print(list(islice(sinx, 10)))
print("sine")
print(list(islice(cosx, 10)))
```


'''Sample output'''

```txt

cosine
[0, Fraction(1, 1), Fraction(0, 1), Fraction(-1, 6), Fraction(0, 1), Fraction(1, 120), Fraction(0, 1), Fraction(-1, 5040), Fraction(0, 1), Fraction(1, 362880)]
sine
[1, Fraction(0, 1), Fraction(-1, 2), Fraction(0, 1), Fraction(1, 24), Fraction(0, 1), Fraction(-1, 720), Fraction(0, 1), Fraction(1, 40320), Fraction(0, 1)]

```



### Operator overloading

Define an iterator class as a polynomial to provide overloaded operators and automatic tee-ing.  It is kind of overkill.

```python
from itertools import count, chain, tee, islice, cycle
from fractions import Fraction

# infinite polynomial class
class Poly:
    def __init__(self, gen = None):
        self.gen, self.source = (None, gen) if type(gen) is Poly \
            else (gen, None)

    def __iter__(self):
        # We're essentially tee'ing it everytime the iterator
        # is, well, iterated.  This may be excessive.
        return Poly(self)

    def getsource(self):
        if self.gen == None:
            s = self.source
            s.getsource()
            (a,b) = tee(s.gen, 2)
            s.gen = a
            self.gen = b

    def next(self):
        self.getsource()
        return next(self.gen)

    __next__ = next

    # Overload "<<" as stream input operator. Hey, C++ does it.
    def __lshift__(self, a): self.gen = a

    # The other operators are pretty much what one would expect
    def __neg__(self): return Poly(-x for x in self)

    def __sub__(a, b): return a + (-b)

    def __rsub__(a, n):
        a = Poly(a)
        def gen():
            yield(n - next(a))
            for x in a: yield(-x)
        return Poly(gen())

    def __add__(a, b):
        if type(b) is Poly:
            return Poly(x + y for (x,y) in zip(a,b))

        a = Poly(a)
        def gen():
            yield(next(a) + b)
            for x in a: yield(x)
        return Poly(gen())

    def __radd__(a,b):
        return a + b

    def __mul__(a,b):
        if not type(b) is Poly:
            return Poly(x*b for x in a)

        def gen():
            s = Poly(cycle([0]))
            for y in b:
                s += y*a
                yield(next(s))

        return Poly(gen())

    def __rmul__(a,b): return a*b

    def __truediv__(a,b):
        if not type(b) is Poly:
            return Poly(Fraction(x, b) for x in a)

        a, b = Poly(a), Poly(b)
        def gen():
            r, bb = a,next(b)
            while True:
                aa = next(r)
                q = Fraction(aa, bb)
                yield(q)
                r -= q*b

        return Poly(gen())

# these two would probably be better as class methods
def inte(a):
    def gen():
        yield(0)
        for (x,n) in zip(a, count(1)):
            yield(Fraction(x,n))
    return Poly(gen())

def diff(a):
    def gen():
        for (x, n) in zip(a, count(0)):
            if n: yield(x*n)
    return Poly(gen())


# all that for the syntactic sugar
sinx, cosx, tanx, expx = Poly(), Poly(), Poly(), Poly()

sinx << inte(cosx)
cosx << 1 - inte(sinx)
tanx << sinx / cosx        # "=" would also work here
expx << 1 + inte(expx)

for n,x in zip(("sin", "cos", "tan", "exp"), (sinx, cosx, tanx, expx)):
    print(n, ', '.join(map(str, list(islice(x, 10)))))
```



## Racket

Using '''Lazy Racket''':


```racket

#lang lazy

(require racket/match)

;; element-wise addition and subtraction
(define (<+> s1 s2) (map + s1 s2))
(define (<-> s1 s2) (map - s1 s2))

;; element-wise scaling
(define (scale a s) (map (λ (x) (* a x)) s))

;; series multiplication
(define (<*> fs gs)
  (match-let ([(cons f ft) (! fs)]
              [(cons g gt) (! gs)])
    (cons (* f g) (<+> (scale f gt) (<*> ft gs)))))

;; series division
(define (</> fs gs)
  (match-letrec ([(cons f ft) (! fs)]
                 [(cons g gt) (! gs)]
                 [qs (cons (/ f g) (scale (/ g) (<-> ft (<*> qs gt))))])
      qs))

;; integration and differentiation
(define (int f) (map / f (enum 1)))
(define (diff f) (map * (cdr f) (enum 1)))

;; series of natural numbers greater then n
(define (enum n) (cons n (enum (+ 1 n ))))

```


Examples:


```racket

(define <sin> (cons 0 (int <cos>)))
(define <cos> (cons 1 (scale -1 (int <sin>))))

-> (!! (take 10 <sin>))
'(0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)

-> (!! (take 10 <cos>))
'(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0)

-> (!! (take 10 (diff <sin>)))
'(1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0)

; sin(x)² + cos(x)² = 1
-> (!! (take 10 (<+> (<*> <cos> <cos>) (<*> <sin> <sin>))))
'(1 0 0 0 0 0 0 0 0 0)

; series of (tan x)
-> (!! (take 10 (</> <sin> <cos>)))
'(0 1 0 1/3 0 2/15 0 17/315 0 62/2835)

```



## Scheme

Definitions of operations on lazy lists:

```scheme
(define-syntax lons
  (syntax-rules ()
    ((_ lar ldr) (delay (cons lar (delay ldr))))))

(define (lar lons)
  (car (force lons)))

(define (ldr lons)
  (force (cdr (force lons))))

(define (lap proc . llists)
  (lons (apply proc (map lar llists)) (apply lap proc (map ldr llists))))

(define (take n llist)
  (if (zero? n)
      (list)
      (cons (lar llist) (take (- n 1) (ldr llist)))))

(define (iota n)
  (lons n (iota (+ n 1))))

(define (repeat n)
  (lons n (repeat n)))
```

Definitions of operations on formal power series:

```scheme
(define (fps+ . llists)
  (apply lap + llists))

(define (fps- . llists)
  (apply lap - llists))

(define (fps* . llists)
  (define (*fps* p q)
    (let ((larp (lar p)) (larq (lar q)) (ldrp (ldr p)) (ldrq (ldr q)))
      (lons (* larp larq)
            (fps+ (lap (lambda (p) (* p larp)) ldrq)
                  (lap (lambda (p) (* p larq)) ldrp)
                  (lons 0 (*fps* ldrp ldrq))))))
  (cond ((null? llists) (lons 1 (repeat 0)))
        ((null? (cdr llists)) (car llists))
        (else
         (apply fps* (cons (*fps* (car llists) (cadr llists)) (cddr llists))))))

(define (fps/ n . llists)
  (define (*fps/ n d)
    (let ((q (/ (lar n) (lar d))))
      (lons q (*fps/ (fps- (ldr n) (lap (lambda (p) (* p q)) (ldr d))) d))))
  (if (null? llists)
      (*fps/ (lons 1 (repeat 0)) n)
      (*fps/ n (apply fps* llists))))

(define (fpsint llist)
  (lons 0 (lap * llist (lap / (iota 1)))))

(define (fpsdif llist)
  (lap * (iota 1) (ldr llist)))
```

Now the sine and cosine functions can be defined in terms of eachother using integrals:

```scheme
(define fpscos
  (fps- (lons 1 (repeat 0)) (fpsint (delay (force fpssin)))))

(define fpssin
  (fpsint (delay (force fpscos))))

(display (take 10 fpssin))
(newline)

(display (take 10 fpscos))
(newline)
```

Output:
 (0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)
 (1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0)
Now we can do some calculations with these, e.g. show that <math>\sin^2 x + \cos^2 x = 1</math> or define <math>\tan x = \frac{\sin x}{\cos x}</math>:

```scheme
(display (take 10 (fps+ (fps* fpssin fpssin) (fps* fpscos fpscos))))
(newline)

(define fpstan
  (fps/ fpssin fpscos))

(display (take 10 fpstan))
(newline)
```

Output:
 (1 0 0 0 0 0 0 0 0 0)
 (0 1 0 1/3 0 2/15 0 17/315 0 62/2835)


## Tcl

Tcl doesn't arbitrary definitions of numbers without extension packages, so we'll represent these formal power series as objects, which are really just wrappers around a pair of functions: one determining how many terms there are in the series (possibly including "infinitely many") and the other producing the factor for a particular term.

This code makes ''extensive'' use of the fact that objects can have methods and variables independent of their class. This greatly reduces the requirement for singleton classes.

```tcl
package require TclOO

oo::class create PowerSeries {
    variable name
    constructor {{body {}} args} {
        # Use the body to adapt the methods of the _object_
	oo::objdefine [self] $body
        # Use the rest to configure variables in the object
	foreach {var val} $args {
	    set [my varname $var] $val
	}
        # Guess the name if not already set
	if {![info exists [my varname name]]} {
	    set name [namespace tail [self]]
	}
    }
    method name {} {
	return $name
    }
    method term i {
	return 0
    }
    method limit {} {
	return inf
    }

    # A pretty-printer, that prints the first $terms non-zero terms
    method print {terms} {
	set result "${name}(x) == "
	set limit [my limit]
	if {$limit == 0} {
	    # Special case
	    return $result[my term 0]
	}
	set tCount 0
	for {set i 0} {$tCount<$terms && $i<=$limit} {incr i} {
	    set t [my term $i]
	    if {$t == 0} continue
	    incr tCount
	    set t [format %.4g $t]
            if {$t eq "1" && $i != 0} {set t ""}
	    if {$i == 0} {
		append result "$t + "
	    } elseif {$i == 1} {
		append result "${t}x + "
	    } else {
		set p [string map {
		    0 \u2070 1 \u00b9 2 \u00b2 3 \u00b3 4 \u2074
		    5 \u2075 6 \u2076 7 \u2077 8 \u2078 9 \u2079
		} $i]
		append result "${t}x$p + "
	    }
	}
	return [string trimright $result "+ "]
    }

    # Evaluate (a prefix of) the series at a particular x
    # The terms parameter gives the number; 5 is enough for show
    method evaluate {x {terms 5}} {
	set result 0
	set limit [my limit]
	set tCount 0
	for {set i 0} {$tCount<$terms && $i<=$limit} {incr i} {
	    set t [my term $i]
	    if {$t == 0} continue
	    incr tCount
	    set result [expr {$result + $t * ($x ** $i)}]
	}
	return $result
    }

    # Operations to build new sequences from old ones
    method add {s} {
	PowerSeries new {
	    variable S1 S2
	    method limit {} {expr {max([$S1 limit],[$S2 limit])}}
	    method term i {
		set t1 [expr {$i>[$S1 limit] ? 0 : [$S1 term $i]}]
		set t2 [expr {$i>[$S2 limit] ? 0 : [$S2 term $i]}]
		expr {$t1 + $t2}
	    }
	} S1 [self] S2 $s name "$name+[$s name]"
    }
    method subtract {s} {
	PowerSeries new {
	    variable S1 S2
	    method limit {} {expr {max([$S1 limit],[$S2 limit])}}
	    method term i {
		set t1 [expr {$i>[$S1 limit] ? 0 : [$S1 term $i]}]
		set t2 [expr {$i>[$S2 limit] ? 0 : [$S2 term $i]}]
		expr {$t1 - $t2}
	    }
	} S1 [self] S2 $s name "$name-[$s name]"
    }
    method integrate {{Name ""}} {
	if {$Name eq ""} {set Name "Integrate\[[my name]\]"}
	PowerSeries new {
	    variable S limit
	    method limit {} {
		if {[info exists limit]} {return $limit}
		try {
		    return [expr {[$S limit] + 1}]
		} on error {} {
		    # If the limit spirals out of control, it's infinite!
		    return [set limit inf]
		}
	    }
	    method term i {
		if {$i == 0} {return 0}
		set t [$S term [expr {$i-1}]]
		expr {$t / double($i)}
	    }
	} S [self] name $Name
    }
    method differentiate {{Name ""}} {
	if {$Name eq ""} {set Name "Differentiate\[[my name]\]"}
	PowerSeries new {
	    variable S
	    method limit {} {expr {[$S limit] ? [$S limit] - 1 : 0}}
	    method term  i  {expr {[incr i] * [$S term $i]}}
	} S [self] name $Name
    }
    # Special constructor for making constants
    self method constant n {
	PowerSeries new {
	    variable n
	    method limit {} {return 0}
	    method term i {return $n}
	} n $n name $n
    }
}

# Define the two power series in terms of each other
PowerSeries create cos ;# temporary dummy object...
rename [cos integrate "sin"] sin
cos destroy            ;# remove the dummy to make way for the real one...
rename [[PowerSeries constant 1] subtract [sin integrate]] cos
```

Demonstrating:

```tcl
% sin print 7
sin(x) == x + -0.1667x³ + 0.008333x⁵ + -0.0001984x⁷ + 2.756e-06x⁹ + -2.505e-08x¹¹ + 1.606e-10x¹³
% cos print 7
1-Integrate[sin](x) == 1 + -0.5x² + 0.04167x⁴ + -0.001389x⁶ + 2.48e-05x⁸ + -2.756e-07x¹⁰ + 2.088e-09x¹²
% sin evaluate [expr acos(0)]
1.0000035425842861
% cos evaluate [expr acos(0)]
2.473727636463901e-5
```



## zkl

zkl iterators (aka Walkers) are more versatile than the run-of-the-mill iterator and can be used to represent infinite sequences (eg a finite set can be padded forever or cycled over), which works well here. The Walker tweak method is used to modify iterator behavior (ie how to filter the sequence, what to do if the sequence ends, etc). The Haskell like zipWith Walker method knows how to deal with infinite sequences.

```zkl
class IPS{
   var [protected] w;   // the coefficients of the infinite series
   fcn init(w_or_a,b,c,etc){  // IPS(1,2,3) --> (1,2,3,0,0,...)
      switch [arglist]{
	 case(Walker)		{ w=w_or_a.tweak(Void,0) }
	 else			{ w=vm.arglist.walker().tweak(Void,0) }
      }
   }
   fcn __opAdd(ipf){   //IPS(1,2,3)+IPS(4,5)-->IPS(5,6,3,0,...), returns modified self
      switch[arglist]{
         case(1){ addConst(ipf) }         // IPS + int/float
	 else   { w=w.zipWith('+,ipf.w) } // IPS + IPS
      }
      self
   }
   fcn __opSub(ipf){ w=w.zipWith('-,ipf.w); self } // IPS - IPSHaskell
   fcn __opMul(ipf){ }  // stub
   fcn __opDiv(x){ w.next().toFloat()/x } // *IPS/x, for integtate()
   fcn __opNegate  { w=w.tweak(Op("--")); self }
   // integtate: b0 = 0 by convention, bn = an-1/n
   fcn integrate{ w=w.zipWith('/,[1..]).push(0.0); self }
   fcn diff     { w=w.zipWith('*,[1..]); 	   self }
   fcn facts{ (1).walker(*).tweak(fcn(n){ (1).reduce(n,'*,1) }) } // 1!,2!...
   fcn walk(n){ w.walk(n) }
   fcn value(x,N=15){ ns:=[1..]; w.reduce(N,'wrap(s,an){ s + an*x.pow(ns.next()) }) }
   fcn cons(k){ w.push(k); self }  //--> k, a0, a1, a2, ...
   // addConst(k) --> k + a0, a1, a2, ..., same as k + IPS
   fcn addConst(k){ (w.next() + k) : w.push(_); self }
}
```

Add two power series. Add a const to get: 11 - (1 + 2x + 3x^2) ...

```zkl
(IPS(1,2,3) + IPS(4,5)).walk(5).println();
(-IPS([1..]) + 11).walk(5).println();
```

```txt

L(5,7,3,0,0)
L(10,-2,-3,-4,-5)

```

Define sine in terms of a Taylor series, cos in terms of sine.

```zkl
fcn sine{  // sine Taylor series: 0 + x - x^3/3! + x^5/5! - x^7/7! + x^9/9! - ...
   IPS(Utils.Helpers.cycle(1.0, 0.0, -1.0, 0.0).zipWith('/,IPS.facts()))
   .cons(0.0)
}
print("Sine Taylor series: "); dostuff(sine,"sin");

fcn cosine{ -sine().integrate() + 1.0 }
print("Cosine power series: "); dostuff(cosine,"cos");

fcn dostuff(ips,name){  // print series, evaluate at various points
   f:='wrap(x,xnm){ v:=ips().value(x);
      println("%s(%s) \U2192; %f  \U394;=%f".fmt(name,xnm,v,x.Method(name)()-v));
   };
   ips().walk(15).println();
   f(0.0,"0"); f((1.0).pi/4,"\Ubc;\U3c0;");
   f((1.0).pi/2,"\Ubd;\U3c0;"); f((1.0).pi,"\U3c0;");
}
```

```txt

Sine Taylor series: L(0,1,0,-0.166667,0,0.00833333,0,-0.000198413,0,2.75573e-06,0,-2.50521e-08,0,1.6059e-10,0)
sin(0) → 0.000000  Δ=0.000000
sin(¼π) → 0.707107  Δ=-0.000000
sin(½π) → 1.000000  Δ=-0.000000
sin(π) → 0.000021  Δ=-0.000021
Cosine power series: L(1,0,-0.5,0,0.0416667,0,-0.00138889,0,2.48016e-05,0,-2.75573e-07,0,2.08768e-09,0,-1.14707e-11)
cos(0) → 1.000000  Δ=0.000000
cos(¼π) → 0.707107  Δ=0.000000
cos(½π) → -0.000000  Δ=0.000000
cos(π) → -1.000004  Δ=0.000004

```

