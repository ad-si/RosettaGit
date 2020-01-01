+++
title = "Elliptic curve arithmetic"
description = ""
date = 2019-03-06T03:50:08Z
aliases = []
[extra]
id = 12635
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

[[wp:Elliptic curve|Elliptic curve]]s   are sometimes used in   [[wp:cryptography|cryptography]]   as a way to perform   [[wp:digital signature|digital signature]]s.

The purpose of this task is to implement a simplified (without modular arithmetic) version of the elliptic curve arithmetic which is required by the   [[wp:ECDSA|elliptic curve DSA]]   protocol.

In a nutshell, an elliptic curve is a bi-dimensional curve defined by the following relation between the '''x''' and '''y''' coordinates of any point on the curve:

::::   <big><big><math>y^2 = x^3 + a x + b</math></big></big>

'''a''' and '''b''' are arbitrary parameters that define the specific curve which is used.

For this particular task, we'll use the following parameters:

::::   <big> a=0<b>,</b>   b=7 </big>

The most interesting thing about elliptic curves is the fact that it is possible to define a   [[wp:group|group]]   structure on it.

To do so we define an   [[wp:internal composition|internal composition]]   rule with an additive notation '''+''',   such that for any three distinct points '''P''', '''Q''' and '''R''' on the curve, whenever these points are aligned, we have:

::::   <big> P + Q + R = 0 </big>

Here   <big>'''0'''</big>   (zero)   is the ''infinity point'',   for which the '''x''' and '''y''' values are not defined.   It's basically the same kind of point which defines the horizon in   [[wp:projective geometry|projective geometry]].

We'll also assume here that this infinity point is unique and defines the   [[wp:identity element|neutral element]]   of the addition.

This was not the definition of the addition, but only its desired property.    For a more accurate definition, we proceed as such:

Given any three aligned points '''P''', '''Q''' and '''R''',   we define the sum   '''S = P + Q'''   as the point (possibly the infinity point) such that   '''S''', '''R'''   and the infinity point are aligned.

Considering the symmetry of the curve around the x-axis, it's easy to convince oneself that two points '''S''' and '''R''' can be aligned with the infinity point if and only if '''S''' and '''R''' are symmetric of one another towards the x-axis   (because in that case there is no other candidate than the infinity point to complete the alignment triplet).

'''S''' is thus defined as the symmetric of '''R''' towards the '''x''' axis.

The task consists in defining the addition which, for any two points of the curve, returns the sum of these two points.   You will pick two random points on the curve, compute their sum and show that the symmetric of the sum is aligned with the two initial points.

You will use the '''a''' and '''b''' parameters of secp256k1, i.e.  respectively zero and seven.

''Hint'':   You might need to define a "doubling" function, that returns '''P+P''' for any given point '''P'''.

''Extra credit'':   define the full elliptic curve arithmetic (still not modular, though) by defining a "multiply" function that returns,

for any point '''P''' and integer '''n''',   the point '''P + P + ... + P'''     ('''n''' times).





## C


```c
#include <stdio.h>
#include <math.h>

#define C 7
typedef struct { double x, y; } pt;

pt zero(void) { return (pt){ INFINITY, INFINITY }; }

// should be INFINITY, but numeric precision is very much in the way
int is_zero(pt p) { return p.x > 1e20 || p.x < -1e20; }

pt neg(pt p) { return (pt){ p.x, -p.y }; }

pt dbl(pt p) {
	if (is_zero(p)) return p;

	pt r;
	double L = (3 * p.x * p.x) / (2 * p.y);
	r.x = L * L - 2 * p.x;
	r.y = L * (p.x - r.x) - p.y;
	return r;
}

pt add(pt p, pt q) {
	if (p.x == q.x && p.y == q.y) return dbl(p);
	if (is_zero(p)) return q;
	if (is_zero(q)) return p;

	pt r;
	double L = (q.y - p.y) / (q.x - p.x);
	r.x = L * L - p.x - q.x;
	r.y = L * (p.x - r.x) - p.y;
	return r;
}

pt mul(pt p, int n) {
	int i;
	pt r = zero();

	for (i = 1; i <= n; i <<= 1) {
		if (i & n) r = add(r, p);
		p = dbl(p);
	}
	return r;
}

void show(const char *s, pt p) {
	printf("%s", s);
	printf(is_zero(p) ? "Zero\n" : "(%.3f, %.3f)\n", p.x, p.y);
}

pt from_y(double y) {
	pt r;
	r.x = pow(y * y - C, 1.0/3);
	r.y = y;
	return r;
}

int main(void) {
	pt a, b, c, d;

	a = from_y(1);
	b = from_y(2);

	show("a = ", a);
	show("b = ", b);
	show("c = a + b = ", c = add(a, b));
	show("d = -c = ", d = neg(c));
	show("c + d = ", add(c, d));
	show("a + b + d = ", add(a, add(b, d)));
	show("a * 12345 = ", mul(a, 12345));

	return 0;
}
```

{{out}}

```txt

a = (-1.817, 1.000)
b = (-1.442, 2.000)
c = a + b = (10.375, -33.525)
d = -c = (10.375, 33.525)
c + d = Zero
a + b + d = Zero
a * 12345 = (10.759, 35.387)

```



## D

{{trans|Go}}

```d
import std.stdio, std.math, std.string;

enum bCoeff = 7;

struct Pt {
    double x, y;

    @property static Pt zero() pure nothrow @nogc @safe {
        return Pt(double.infinity, double.infinity);
    }

    @property bool isZero() const pure nothrow @nogc @safe {
        return x > 1e20 || x < -1e20;
    }

    @property static Pt fromY(in double y) nothrow /*pure*/ @nogc @safe {
        return Pt(cbrt(y ^^ 2 - bCoeff), y);
    }

    @property Pt dbl() const pure nothrow @nogc @safe {
        if (this.isZero)
            return this;
        immutable L = (3 * x * x) / (2 * y);
        immutable x2 = L ^^ 2  - 2 * x;
        return Pt(x2, L * (x - x2) - y);
    }

    string toString() const pure /*nothrow*/ @safe {
        if (this.isZero)
            return "Zero";
        else
            return format("(%.3f, %.3f)", this.tupleof);
    }

    Pt opUnary(string op)() const pure nothrow @nogc @safe
    if (op == "-") {
        return Pt(this.x, -this.y);
    }

    Pt opBinary(string op)(in Pt q) const pure nothrow @nogc @safe
    if (op == "+") {
        if (this.x == q.x && this.y == q.y)
            return this.dbl;
        if (this.isZero)
            return q;
        if (q.isZero)
            return this;
        immutable L = (q.y - this.y) / (q.x - this.x);
        immutable x = L ^^ 2 - this.x - q.x;
        return Pt(x, L * (this.x - x) - this.y);
    }

    Pt opBinary(string op)(in uint n) const pure nothrow @nogc @safe
    if (op == "*") {
        auto r = Pt.zero;
        Pt p = this;
        for (uint i = 1; i <= n; i <<= 1) {
            if ((i & n) != 0)
                r = r + p;
            p = p.dbl;
        }
        return r;
    }
}

void main() @safe {
    immutable a = Pt.fromY(1);
    immutable b = Pt.fromY(2);
    writeln("a = ", a);
    writeln("b = ", b);
    immutable c = a + b;
    writeln("c = a + b = ", c);
    immutable d = -c;
    writeln("d = -c = ", d);
    writeln("c + d = ", c + d);
    writeln("a + b + d = ", a + b + d);
    writeln("a * 12345 = ", a * 12345);
}
```

{{out}}

```txt
a = (-1.817, 1.000)
b = (-1.442, 2.000)
c = a + b = (10.375, -33.525)
d = -c = (10.375, 33.525)
c + d = Zero
a + b + d = Zero
a * 12345 = (10.759, 35.387)
```



## EchoLisp


### Arithmetic


```scheme

(require 'struct)
(decimals 4)
(string-delimiter "")
(struct pt (x y))

(define-syntax-id _.x (struct-get _ #:pt.x))
(define-syntax-id _.y (struct-get _ #:pt.y))

(define (E-zero) (pt Infinity Infinity))
(define (E-zero? p) (=  (abs p.x) Infinity))
(define (E-neg p) (pt  p.x  (- p.y)))

;; magic formulae from "C"
;; p + p
(define (E-dbl p)
	(if (E-zero? p) p
	(let* (
	[L (// (* 3 p.x p.x) (* 2 p.y))]
	[rx (- (* L L) (* 2 p.x))]
	[ry (- (* L (- p.x rx)) p.y)]
	)
	(pt rx ry))))

;; p + q
(define (E-add p q)
(cond
 [ (and (= p.x p.x) (= p.y q.y)) (E-dbl p)]
 [ (E-zero? p) q ]
 [ (E-zero? q) p ]
 [ else
 	(let* (
 	[L (// (- q.y p.y) (- q.x p.x))]
 	[rx (- (* L L) p.x q.x)] ;; match
 	[ry (- (* L (- p.x rx)) p.y)]
 	)
 	(pt rx ry))]))

 ;; (E-add* a b c ...)
(define (E-add* . pts) (foldl E-add (E-zero) pts))

;; p * n
(define (E-mul p n (r (E-zero)) (i 1))
	(while (<= i n)
		(when (!zero? (bitwise-and i n))  (set! r (E-add r p)))
		(set! p (E-dbl p))
		(set! i (* i 2)))
	r)

;; make points from x or y
(define (Ey.pt y  (c 7))
	(pt (expt (- (* y y) c) 1/3 ) y))
(define (Ex.pt x  (c 7))
	(pt x (sqrt (+ ( * x x x ) c))))


;; Check floating point precision
;; P * n is not always P+P+P+P....P

(define (E-ckmul a n )
	(define e a)
	(for ((i (in-range 1 n))) (set! e (E-add a e)))
	(printf "%d additions a+(a+(a+...)))  → %a" n e)
	(printf "multiplication a x %d        → %a" n (E-mul a n)))

```

{{out}}

```txt

(define P (Ey.pt 1))
(define Q (Ey.pt 2))
(define R (E-add P Q))
    → #<pt> (10.3754 -33.5245)
(E-zero? (E-add* P Q (E-neg R)))
    → #t
(E-mul P 12345)
    → #<pt> (10.7586 35.3874)

;; check floating point precision
(E-ckmul P 10) ;; OK
10 additions a+(a+(a+...))) → #<pt> (0.3797 -2.6561)
multiplication a x 10       → #<pt> (0.3797 -2.6561)

(E-ckmul P 12345) ;; KO
     12345 additions a+(a+(a+...))) → #<pt> (-1.3065 2.4333)
           multiplication a x 12345 → #<pt> (10.7586 35.3874)

```


### Plotting

;; Result at http://www.echolalie.org/echolisp/help.html#plot-xy

```scheme

(define (E-plot (r 3))
	(define (Ellie x y) (- (* y y) (* x x x) 7))
	(define P (Ey.pt 0))
	(define Q (Ex.pt 0))
	(define R (E-add P Q))

	(plot-clear)
	(plot-xy Ellie -10 -10) ;; curve
	(plot-axis 0 0 "red")
	(plot-circle P.x P.y r) ;; points
	(plot-circle Q.x Q.y r)
	(plot-circle R.x R.y r)
	(plot-circle R.x (- R.y) r)
	(plot-segment P.x P.y R.x (- R.y))
	(plot-segment R.x R.y R.x (- R.y))
	)

```



## Go

{{trans|C}}

```go
package main

import (
    "fmt"
    "math"
)

const bCoeff = 7

type pt struct{ x, y float64 }

func zero() pt {
    return pt{math.Inf(1), math.Inf(1)}
}

func is_zero(p pt) bool {
    return p.x > 1e20 || p.x < -1e20
}

func neg(p pt) pt {
    return pt{p.x, -p.y}
}

func dbl(p pt) pt {
    if is_zero(p) {
        return p
    }
    L := (3 * p.x * p.x) / (2 * p.y)
    x := L*L - 2*p.x
    return pt{
        x: x,
        y: L*(p.x-x) - p.y,
    }
}

func add(p, q pt) pt {
    if p.x == q.x && p.y == q.y {
        return dbl(p)
    }
    if is_zero(p) {
        return q
    }
    if is_zero(q) {
        return p
    }
    L := (q.y - p.y) / (q.x - p.x)
    x := L*L - p.x - q.x
    return pt{
        x: x,
        y: L*(p.x-x) - p.y,
    }
}

func mul(p pt, n int) pt {
    r := zero()
    for i := 1; i <= n; i <<= 1 {
        if i&n != 0 {
            r = add(r, p)
        }
        p = dbl(p)
    }
    return r
}

func show(s string, p pt) {
    fmt.Printf("%s", s)
    if is_zero(p) {
        fmt.Println("Zero")
    } else {
        fmt.Printf("(%.3f, %.3f)\n", p.x, p.y)
    }
}

func from_y(y float64) pt {
    return pt{
        x: math.Cbrt(y*y - bCoeff),
        y: y,
    }
}

func main() {
    a := from_y(1)
    b := from_y(2)
    show("a = ", a)
    show("b = ", b)
    c := add(a, b)
    show("c = a + b = ", c)
    d := neg(c)
    show("d = -c = ", d)
    show("c + d = ", add(c, d))
    show("a + b + d = ", add(a, add(b, d)))
    show("a * 12345 = ", mul(a, 12345))
}
```

{{out}}

```txt
a = (-1.817, 1.000)
b = (-1.442, 2.000)
c = a + b = (10.375, -33.525)
d = -c = (10.375, 33.525)
c + d = Zero
a + b + d = Zero
a * 12345 = (10.759, 35.387)
```



## Haskell

First, some useful imports:

```haskell
import Data.Monoid
import Control.Monad (guard)
import Test.QuickCheck (quickCheck)
```


The datatype for a point on an elliptic curve:


```haskell
import Data.Monoid

data Elliptic = Elliptic Double Double | Zero
   deriving Show

instance Eq Elliptic where
  p == q = dist p q < 1e-14
    where
      dist Zero Zero = 0
      dist Zero p = 1/0
      dist p Zero = 1/0
      dist (Elliptic x1 y1) (Elliptic x2 y2) = (x2-x1)^2 + (y2-y1)^2

inv Zero = Zero
inv (Elliptic x y) = Elliptic x (-y)
```


Points on elliptic curve form a monoid:

```haskell
instance Monoid Elliptic where
  mempty = Zero

  mappend Zero p = p
  mappend p Zero = p
  mappend p@(Elliptic x1 y1) q@(Elliptic x2 y2)
    | p == inv q = Zero
    | p == q     = mkElliptic $ 3*x1^2/(2*y1)
    | otherwise  = mkElliptic $ (y2 - y1)/(x2 - x1)
    where
      mkElliptic l = let x = l^2 - x1 - x2
                         y = l*(x1 - x) - y1
                     in Elliptic x y
```


Examples given in other solutions:


```haskell
ellipticX b y = Elliptic (qroot (y^2 - b)) y
  where qroot x = signum x * abs x ** (1/3)
```



```txt
λ> let a = ellipticX 7 1
λ> let b = ellipticX 7 2
λ> a
Elliptic (-1.8171205928321397) 1.0
λ> b
Elliptic (-1.4422495703074083) 2.0
λ> let c = a <> b
λ> c
Elliptic 10.375375389201409 (-33.524509096269696)
λ> let d = inv c
λ> c <> d
Zero
λ> a <> b <> d
Zero
```


Extra credit: multiplication.

1. direct monoidal solution:

```haskell
mult :: Int -> Elliptic -> Elliptic
mult n = mconcat . replicate n
```


2. efficient recursive solution:

```haskell
n `mult` p
  | n == 0 = Zero
  | n == 1 = p
  | n == 2 = p <> p
  | n < 0  = inv ((-n) `mult` p)
  | even n = 2 `mult` ((n `div` 2) `mult` p)
  | odd n  = p <> (n -1) `mult` p
```



```txt
λ> 12345 `mult` a
Elliptic 10.758570529320476 35.387434774280486
```



### Testing


We use QuickCheck to test general properties of points on arbitrary elliptic curve.


```haskell
-- for given a, b and x returns a point on the positive branch of elliptic curve (if point exists)
elliptic a b Nothing  = Just Zero
elliptic a b (Just x) =
  do let y2 = x**3 + a*x + b
     guard (y2 > 0)
     return $ Elliptic x (sqrt y2)

addition a b x1 x2 =
  let p = elliptic a b
      s = p x1 <> p x2
  in (s /= Nothing) ==> (s <> (inv <$> s) == Just Zero)

associativity a b x1 x2 x3 =
  let p = elliptic a b
  in (p x1 <> p x2) <> p x3 == p x1 <> (p x2 <> p x3)

commutativity a b x1 x2 =
  let p = elliptic a b
  in p x1 <> p x2 == p x2 <> p x1
```



```txt
λ> quickCheck addition
+++ OK, passed 100 tests.
λ> quickCheck associativity
+++ OK, passed 100 tests.
λ> quickCheck commutativity
+++ OK, passed 100 tests.
```



## J

Follows the C contribution.


```j
zero=: _j_

isZero=: 1e20 < |@{.@+.

neg=: +

dbl=: monad define
  'p_x p_y'=. +. p=. y
  if. isZero p do. p return. end.
  L=. 1.5 * p_x*p_x % p_y
  r=. (L*L) - 2*p_x
  r j. (L * p_x-r) - p_y
)

add=: dyad define
  'p_x p_y'=. +. p=. x
  'q_x q_y'=. +. q=. y
  if. x=y do. dbl x return. end.
  if. isZero x do. y return. end.
  if. isZero y do. x return. end.
  L=. %~/ +. q-p
  r=. (L*L) - p_x + q_x
  r j. (L * p_x-r) - p_y
)

mul=: dyad define
  a=. zero
  for_bit.|.#:y do.
    if. bit do.
      a=. a add x
    end.
    x=. dbl x
  end.
  a
)

NB. C is 7
from=: j.~ [:(* * 3 |@%: ]) _7 0 1 p. ]

show=: monad define
  if. isZero y do. 'Zero' else.
    'a b'=. ":each +.y
    '(',a,', ', b,')'
  end.
)

task=: 3 :0
  a=. from 1
  b=. from 2

  echo 'a         = ', show a
  echo 'b         = ', show b
  echo 'c = a + b = ', show c =. a add b
  echo 'd = -c    = ', show d =. neg c
  echo 'c + d     = ', show c add d
  echo 'a + b + d = ', show add/ a, b, d
  echo 'a * 12345 = ', show a mul 12345
)
```

{{out}}

```j
   task ''
a         = (_1.81712, 1)
b         = (_1.44225, 2)
c = a + b = (10.3754, _33.5245)
d = -c    = (10.3754, 33.5245)
c + d     = Zero
a + b + d = Zero
a * 12345 = (10.7586, 35.3874)
```



## Java

{{trans|D}}

```java
import static java.lang.Math.*;
import java.util.Locale;

public class Test {

    public static void main(String[] args) {
        Pt a = Pt.fromY(1);
        Pt b = Pt.fromY(2);
        System.out.printf("a = %s%n", a);
        System.out.printf("b = %s%n", b);
        Pt c = a.plus(b);
        System.out.printf("c = a + b = %s%n", c);
        Pt d = c.neg();
        System.out.printf("d = -c = %s%n", d);
        System.out.printf("c + d = %s%n", c.plus(d));
        System.out.printf("a + b + d = %s%n", a.plus(b).plus(d));
        System.out.printf("a * 12345 = %s%n", a.mult(12345));
    }
}

class Pt {
    final static int bCoeff = 7;

    double x, y;

    Pt(double x, double y) {
        this.x = x;
        this.y = y;
    }

    static Pt zero() {
        return new Pt(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY);
    }

    boolean isZero() {
        return this.x > 1e20 || this.x < -1e20;
    }

    static Pt fromY(double y) {
        return new Pt(cbrt(pow(y, 2) - bCoeff), y);
    }

    Pt dbl() {
        if (isZero())
            return this;
        double L = (3 * this.x * this.x) / (2 * this.y);
        double x2 = pow(L, 2) - 2 * this.x;
        return new Pt(x2, L * (this.x - x2) - this.y);
    }

    Pt neg() {
        return new Pt(this.x, -this.y);
    }

    Pt plus(Pt q) {
        if (this.x == q.x && this.y == q.y)
            return dbl();

        if (isZero())
            return q;

        if (q.isZero())
            return this;

        double L = (q.y - this.y) / (q.x - this.x);
        double xx = pow(L, 2) - this.x - q.x;
        return new Pt(xx, L * (this.x - xx) - this.y);
    }

    Pt mult(int n) {
        Pt r = Pt.zero();
        Pt p = this;
        for (int i = 1; i <= n; i <<= 1) {
            if ((i & n) != 0)
                r = r.plus(p);
            p = p.dbl();
        }
        return r;
    }

    @Override
    public String toString() {
        if (isZero())
            return "Zero";
        return String.format(Locale.US, "(%.3f,%.3f)", this.x, this.y);
    }
}
```


```txt
a = (-1.817,1.000)
b = (-1.442,2.000)
c = a + b = (10.375,-33.525)
d = -c = (10.375,33.525)
c + d = Zero
a + b + d = Zero
a * 12345 = (10.759,35.387)
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
struct Point{T<:AbstractFloat}
    x::T
    y::T
end
Point{T}() where T<:AbstractFloat = Point{T}(Inf, Inf)
Point() = Point{Float64}()

Base.show(io::IO, p::Point{T}) where T = iszero(p) ? print(io, "Zero{$T}") : @printf(io, "{%s}(%.3f, %.3f)", T, p.x, p.y)
Base.copy(p::Point) = Point(p.x, p.y)
Base.iszero(p::Point{T}) where T = p.x in (-Inf, Inf)
Base.:-(p::Point) = Point(p.x, -p.y)

function dbl(p::Point{T}) where T
    iszero(p) && return p

    L = 3p.x ^ 2 / 2p.y
    x = L ^ 2 - 2p.x
    y = L * (p.x - x) - p.y
    return Point{T}(x, y)
end
Base.:(==)(a::Point{T}, C::Point{T}) where T = a.x == C.x && a.y == C.y

function Base.:+(p::Point{T}, q::Point{T}) where T
    p == q && return dbl(p)
    iszero(p) && return q
    iszero(q) && return p

    L = (q.y - p.y) / (q.x - p.x)
    x = L ^ 2 - p.x - q.x
    y = L * (p.x - x) - p.y
    return Point{T}(x, y)
end
function Base.:*(p::Point, n::Integer)
    r = Point()
    i = 1
    while i ≤ n
        if i & n != 0 r += p end
        p = dbl(p)
        i <<= 1
    end
    return r
end
Base.:*(n::Integer, p::Point) = p * n

const C = 7
function Point(y::AbstractFloat)
    n = y ^ 2 - C
    x = n ≥ 0 ? n ^ (1 / 3) : -((-n) ^ (1 / 3))
    return Point{typeof(y)}(x, y)
end

a = Point(1.0)
b = Point(2.0)
@show a b
@show c = a + b
@show d = -c
@show c + d
@show a + b + d
@show 12345a
```


{{out}}

```txt
a = {Float64}(-1.817, 1.000)
b = {Float64}(-1.442, 2.000)
c = a + b = {Float64}(10.375, -33.525)
d = -c = {Float64}(10.375, 33.525)
c + d = Zero{Float64}
a + b + d = Zero{Float64}
12345a = {Float64}(10.759, 35.387)
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.4

const val C = 7

class Pt(val x: Double, val y: Double) {
    val zero get() = Pt(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY)

    val isZero get() = x > 1e20 || x < -1e20

    fun dbl(): Pt {
        if (isZero) return this
        val l = 3.0 * x * x / (2.0 * y)
        val t = l * l - 2.0 * x
        return Pt(t, l * (x - t) - y)
    }

    operator fun unaryMinus() = Pt(x, -y)

    operator fun plus(other: Pt): Pt {
        if (x == other.x && y == other.y) return dbl()
        if (isZero) return other
        if (other.isZero) return this
        val l = (other.y - y) / (other.x - x)
        val t = l * l - x - other.x
        return Pt(t, l * (x - t) - y)
    }

    operator fun times(n: Int): Pt {
        var r: Pt = zero
        var p = this
        var i = 1
        while (i <= n) {
            if ((i and n) != 0) r += p
            p = p.dbl()
            i = i shl 1
        }
        return r
    }

    override fun toString() =
        if (isZero) "Zero" else "(${"%.3f".format(x)}, ${"%.3f".format(y)})"
}

fun Double.toPt() = Pt(Math.cbrt(this * this - C), this)

fun main(args: Array<String>) {
    val a = 1.0.toPt()
    val b = 2.0.toPt()
    val c = a + b
    val d = -c
    println("a         = $a")
    println("b         = $b")
    println("c = a + b = $c")
    println("d = -c    = $d")
    println("c + d     = ${c + d}")
    println("a + b + d = ${a + b + d}")
    println("a * 12345 = ${a * 12345}")
}
```


{{out}}

```txt

a         = (-1.817, 1.000)
b         = (-1.442, 2.000)
c = a + b = (10.375, -33.525)
d = -c    = (10.375, 33.525)
c + d     = Zero
a + b + d = Zero
a * 12345 = (10.759, 35.387)

```



## PARI/GP

The examples were borrowed from C, though the coding is built-in for GP and so not ported.

```parigp
e=ellinit([0,7]);
a=[-6^(1/3),1]
b=[-3^(1/3),2]
c=elladd(e,a,b)
d=ellneg(e,c)
elladd(e,c,d)
elladd(e,elladd(e,a,b),d)
ellmul(e,a,12345)
```

{{output}}

```txt
%1 = [-1.8171205928321396588912117563272605024, 1]
%2 = [-1.4422495703074083823216383107801095884, 2]
%3 = [10.375375389201411959219947350723254093, -33.524509096269714974732957666465317961]
%4 = [10.375375389201411959219947350723254093, 33.524509096269714974732957666465317961]
%5 = [0]
%6 = [0]
%7 = [10.758570529079026647817660298097473136, 35.387434773095871032744887640370612568]
```



## Perl

{{trans|C}}

```perl
package EC;
{
    our ($A, $B) = (0, 7);
    package EC::Point;
    sub new { my $class = shift; bless [ @_ ], $class }
    sub zero { bless [], shift }
    sub x { shift->[0] }; sub y { shift->[1] };
    sub double {
        my $self = shift;
        return $self unless @$self;
        my $L = (3 * $self->x**2) / (2*$self->y);
        my $x = $L**2 - 2*$self->x;
        bless [ $x, $L * ($self->x - $x) - $self->y ], ref $self;
    }
    use overload
    '==' => sub { my ($p, $q) = @_; $p->x == $q->x and $p->y == $q->y },
    '+' => sub {
        my ($p, $q) = @_;
        return $p->double if $p == $q;
        return $p unless @$q;
        return $q unless @$p;
        my $slope = ($q->y - $p->y) / ($q->x - $p->x);
        my $x = $slope**2 - $p->x - $q->x;
        bless [ $x, $slope * ($p->x - $x)  - $p->y ], ref $p;
    },
    q{""} => sub {
        my $self = shift;
        return @$self
        ? sprintf "EC-point at x=%f, y=%f", @$self
        : 'EC point at infinite';
    }
}

package Test;
my $p = +EC::Point->new(-($EC::B - 1)**(1/3), 1);
my $q = +EC::Point->new(-($EC::B - 4)**(1/3), 2);
my $s = $p + $q, "\n";
print "$_\n" for $p, $q, $s;
print "check alignment... ";
print abs(($q->x - $p->x)*(-$s->y - $p->y) - ($q->y - $p->y)*($s->x - $p->x)) < 0.001
    ? "ok" : "wrong";
```

{{out}}

```txt
EC-point at x=-1.817121, y=1.000000
EC-point at x=-1.442250, y=2.000000
EC-point at x=10.375375, y=-33.524509
check alignment... ok

```



## Perl 6


```perl6
unit module EC;
our ($A, $B) = (0, 7);

role Horizon { method gist { 'EC Point at horizon' } }
class Point {
    has ($.x, $.y);
    multi method new(
        $x, $y where $y**2 ~~ $x**3 + $A*$x + $B
    ) { self.bless(:$x, :$y) }
    multi method new(Horizon $) { self.bless but Horizon }
    method gist { "EC Point at x=$.x, y=$.y" }
}

multi prefix:<->(Point $p) { Point.new: x => $p.x, y => -$p.y }
multi prefix:<->(Horizon $) { Horizon }
multi infix:<->(Point $a, Point $b) { $a + -$b }

multi infix:<+>(Horizon $, Point $p) { $p }
multi infix:<+>(Point $p, Horizon) { $p }

multi infix:<*>(Point $u, Int $n) { $n * $u }
multi infix:<*>(Int $n, Horizon) { Horizon }
multi infix:<*>(0, Point) { Horizon }
multi infix:<*>(1, Point $p) { $p }
multi infix:<*>(2, Point $p) {
    my $l = (3*$p.x**2 + $A) / (2 *$p.y);
    my $y = $l*($p.x - my $x = $l**2 - 2*$p.x) - $p.y;
    $p.bless(:$x, :$y);
}
multi infix:<*>(Int $n where $n > 2, Point $p) {
    2 * ($n div 2 * $p) + $n % 2 * $p;
}

multi infix:<+>(Point $p, Point $q) {
    if $p.x ~~ $q.x {
        return $p.y ~~ $q.y ?? 2 * $p !! Horizon;
    }
    else {
        my $slope = ($q.y - $p.y) / ($q.x - $p.x);
        my $y = $slope*($p.x - my $x = $slope**2 - $p.x - $q.x) - $p.y;
        return $p.new(:$x, :$y);
    }
}

say my $p = Point.new: x => $_, y => sqrt(abs(1 - $_**3 - $A*$_ - $B)) given 1;
say my $q = Point.new: x => $_, y => sqrt(abs(1 - $_**3 - $A*$_ - $B)) given 2;
say my $s = $p + $q;

say "checking alignment:  ", abs ($p.x - $q.x)*(-$s.y - $q.y) - ($p.y - $q.y)*($s.x - $q.x);
```

{{out}}

```txt
EC Point at x=1, y=2.64575131106459
EC Point at x=2, y=3.74165738677394
EC Point at x=-1.79898987322333, y=0.421678696849803
checking alignment:  8.88178419700125e-16
```



## Phix

{{Trans|C}}

```Phix
constant X=1, Y=2, bCoeff=7, INF = 1e300*1e300

type point(object pt)
    return sequence(pt) and length(pt)=2 and atom(pt[X]) and atom(pt[Y])
end type

function zero()
point pt = {INF, INF}
    return pt
end function

function is_zero(point p)
    return p[X]>1e20 or p[X]<-1e20
end function

function neg(point p)
    p = {p[X], -p[Y]}
    return p
end function

function dbl(point p)
point r = p
    if not is_zero(p) then
        atom L = (3*p[X]*p[X])/(2*p[Y])
        atom x = L*L-2*p[X]
        r = {x, L*(p[X]-x)-p[Y]}
    end if
    return r
end function

function add(point p, point q)
    if p==q then return dbl(p) end if
    if is_zero(p) then return q end if
    if is_zero(q) then return p end if
    atom L = (q[Y]-p[Y])/(q[X]-p[X])
    atom x = L*L-p[X]-q[X]
    point r = {x, L*(p[X]-x)-p[Y]}
    return r
end function

function mul(point p, integer n)
point r = zero()
integer i = 1
    while i<=n do
        if and_bits(i, n) then r = add(r, p) end if
        p = dbl(p)
        i = i*2
    end while
    return r
end function

procedure show(string s, point p)
    puts(1, s&iff(is_zero(p)?"Zero\n":sprintf("(%.3f, %.3f)\n", p)))
end procedure

function cbrt(atom c)
    return iff(c>=0?power(c,1/3):-power(-c,1/3))
end function

function from_y(atom y)
    point r = {cbrt(y*y-bCoeff), y}
    return r
end function

point a, b, c, d

    a = from_y(1)
    b = from_y(2)
    c = add(a, b)
    d = neg(c)

    show("a = ", a)
    show("b = ", b)
    show("c = a + b = ", c)
    show("d = -c = ", d)
    show("c + d = ", add(c, d))
    show("a + b + d = ", add(a, add(b, d)))
    show("a * 12345 = ", mul(a, 12345))

```

{{out}}

```txt

a = (-1.817, 1.000)
b = (-1.442, 2.000)
c = a + b = (10.375, -33.525)
d = -c = (10.375, 33.525)
c + d = Zero
a + b + d = Zero
a * 12345 = (10.759, 35.387)

```



## Python

{{trans|C}}

```python
#!/usr/bin/env python3

class Point:
    b = 7
    def __init__(self, x=float('inf'), y=float('inf')):
        self.x = x
        self.y = y

    def copy(self):
        return Point(self.x, self.y)

    def is_zero(self):
        return self.x > 1e20 or self.x < -1e20

    def neg(self):
        return Point(self.x, -self.y)

    def dbl(self):
        if self.is_zero():
            return self.copy()
        try:
            L = (3 * self.x * self.x) / (2 * self.y)
        except ZeroDivisionError:
            return Point()
        x = L * L - 2 * self.x
        return Point(x, L * (self.x - x) - self.y)

    def add(self, q):
        if self.x == q.x and self.y == q.y:
            return self.dbl()
        if self.is_zero():
            return q.copy()
        if q.is_zero():
            return self.copy()
        try:
            L = (q.y - self.y) / (q.x - self.x)
        except ZeroDivisionError:
            return Point()
        x = L * L - self.x - q.x
        return Point(x, L * (self.x - x) - self.y)

    def mul(self, n):
        p = self.copy()
        r = Point()
        i = 1
        while i <= n:
            if i&n:
                r = r.add(p)
            p = p.dbl()
            i <<= 1
        return r

    def __str__(self):
        return "({:.3f}, {:.3f})".format(self.x, self.y)

def show(s, p):
    print(s, "Zero" if p.is_zero() else p)

def from_y(y):
    n = y * y - Point.b
    x = n**(1./3) if n>=0 else -((-n)**(1./3))
    return Point(x, y)

# demonstrate
a = from_y(1)
b = from_y(2)
show("a =", a)
show("b =", b)
c = a.add(b)
show("c = a + b =", c)
d = c.neg()
show("d = -c =", d)
show("c + d =", c.add(d))
show("a + b + d =", a.add(b.add(d)))
show("a * 12345 =", a.mul(12345))
```

{{out}}

```txt

a = (-1.817, 1.000)
b = (-1.442, 2.000)
c = a + b = (10.375, -33.525)
d = -c = (10.375, 33.525)
c + d = Zero
a + b + d = Zero
a * 12345 = (10.759, 35.387)

```



## Racket


```racket

#lang racket
(define a 0) (define b 7)
(define (ε? x) (<= (abs x) 1e-14))
(define (== p q) (for/and ([pi p] [qi q]) (ε? (- pi qi))))
(define zero #(0 0))
(define (zero? p) (== p zero))
(define (neg p) (match-define (vector x y) p) (vector x (- y)))
(define (⊕ p q)
  (cond [(== q (neg p)) zero]
        [else
         (match-define (vector px py) p)
         (match-define (vector qx qy) q)
         (define (done λ px py qx)
           (define x (- (* λ λ) px qx))
           (vector x (- (+ (* λ (- x px)) py))))
         (cond [(and (== p q) (ε? py)) zero]
               [(or (== p q) (ε? (- px qx)))
                (done (/ (+ (* 3 px px) a) (* 2 py)) px py qx)]
               [(done (/ (- py qy) (- px qx)) px py qx)])]))
(define (⊗ p n)
  (cond [(= n 0)       zero]
        [(= n 1)       p]
        [(= n 2)       (⊕ p p)]
        [(negative? n) (neg (⊗ p (- n)))]
        [(even? n)     (⊗ (⊗ p (/ n 2)) 2)]
        [(odd? n)      (⊕ p (⊗ p (- n 1)))]))

```

Test:

```racket

(define (root3 x) (* (sgn x) (expt (abs x) 1/3)))
(define (y->point y) (vector (root3 (- (* y y) b)) y))
(define p (y->point 1))
(define q (y->point 2))
(displayln (~a "p = " p))
(displayln (~a "q = " q))
(displayln (~a "p+q = " (⊕ p q)))
(displayln (~a "-(p+q) = " (neg (⊕ p q))))
(displayln (~a "(p+q)+(-(p+q)) = " (⊕ (⊕ p q) (neg (⊕ p q)))))
(displayln (~a "p+(q+(-(p+q))) = 0 " (zero? (⊕ p (⊕ q (neg (⊕ p q)))))))
(displayln (~a "p*12345 " (⊗ p 12345)))

```

Output:

```racket

p = #(-1.8171205928321397 1)
q = #(-1.4422495703074083 2)
p+q = #(10.375375389201409 -33.524509096269696)
-(p+q) = #(10.375375389201409 33.524509096269696)
(p+q)+(-(p+q)) = #(0 0)
p+(q+(-(p+q))) = 0 #t
p*12345 #(10.758570529320806 35.387434774282106)

```



## REXX

REXX doesn't have any higher math functions, so a cube root   ('''cbrt''')   function was included here as well as a

general purpose '''root'''   (and accompanying '''rootG''', and '''rootI''')   functions.

Also, some code was added to have the output better aligned   (for instance, negative and positive numbers).

```rexx
/*REXX program defines (for any 2 points on the curve), returns the sum of the 2 points.*/
numeric digits 100                               /*try to ensure a min. of accuracy loss*/
a=func(1)              ;          say  '    a =              '          show(a)
b=func(2)              ;          say  '    b =              '          show(b)
c=add(a, b)            ;          say  '    c = (a+b)       ='          show(c)
d=neg(c)               ;          say  '    d = -c          ='          show(d)
e=add(c, d)            ;          say  '    e = (c+d)       ='          show(e)
g=add(a, add(b, d))    ;          say  '    g = (a+b+d)     ='          show(g)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cbrt:  procedure; parse arg x;                                            return root(x,3)
conv:  procedure; arg z; if isZ(z)  then return 'zero'; return left('',z>=0)format(z,,5)/1
root:  procedure; parse arg x,y;  if x=0 | y=1  then return x/1;   d=5;   return rootI()/1
rootG: parse value format(x,2,1,,0)  'E0'  with  ? 'E' _ .;    return (?/y'E'_ %y) + (x>1)
func:  procedure; parse arg y,k;  if k=='' then k=7;           return cbrt(y**2-k) y
inf:                                                           return '1e' || (digits()%2)
isZ:   procedure; parse arg px . ;                             return abs(px) >= inf()
neg:   procedure; parse arg px py;                             return px         (-py)
show:  procedure; parse arg x  y ;                             return conv(x)  conv(y)
zero:                                                          return inf()    inf()
/*──────────────────────────────────────────────────────────────────────────────────────*/
add:   procedure; parse arg px py, qx qy;     if px=qx & py=qy  then return dbl(px py)
       if isZ(px  py)  then return qx qy;     if isZ(qx qy)     then return     px py
       z=qx - px;        if z=0  then do;     $=inf();          rx=inf();          end
                                 else do;     $=(qy-py) / z;    rx=$*$ - px - qx;  end
       ry=$ * (px-rx) - py;                                          return  rx  ry
/*──────────────────────────────────────────────────────────────────────────────────────*/
dbl:   procedure;  parse arg px py;     if isZ(px py)  then return px py;          z=py+py
                   if z=0  then $=inf()
                           else $=(3*px*py) / (py+py)
                   rx=$*$ - px*px;     ry=$ * (px-rx) - py;   return rx ry
/*──────────────────────────────────────────────────────────────────────────────────────*/
rootI: ox=x;  oy=y;  x=abs(x);  y=abs(y);  a=digits()+5;  numeric form;  g=rootG();  m=y-1
          do  until  d==a;    d=min(d+d,a);               numeric digits d;          o=0
            do  until o=g;    o=g;   g=format((m*g**y+x)/y/g**m,,d-2);  end  /*until o=g*/
          end  /*until d==a*/;       _=g*sign(ox);   if oy<0  then _=1/_;         return _
```

{{out|output}}

```txt

    a =               -1.81712  1
    b =               -1.44225  2
    c = (a+b)       =  10.37538 -33.52451
    d = -c          =  10.37538  33.52451
    e = (c+d)       = zero zero
    g = (a+b+d)     = zero zero

```



## Sage

Examples from C, using the built-in Elliptic curves library.

```sage

Ellie = EllipticCurve(RR,[0,7]) # RR = field of real numbers

# a point (x,y) on Ellie, given y
def point ( y) :
    x = var('x')
    x = (y^2 - 7 - x^3).roots(x,ring=RR,multiplicities = False)[0]
    P = Ellie([x,y])
    return P

print Ellie
P = point(1)
print 'P',P
Q = point(2)
print 'Q',Q
S = P+Q
print 'S = P + Q',S
print 'P+Q-S', P+Q-S
print 'P*12345' ,P*12345


```

{{out}}

```txt

Elliptic Curve defined by y^2 = x^3 + 7.00000000000000 over Real Field
with 53 bits of precision

P (-1.81712059283214 : 1.00000000000000 : 1.00000000000000)
Q (-1.44224957030741 : 2.00000000000000 : 1.00000000000000)
S = P + Q (10.3753753892014 : -33.5245090962697 : 1.00000000000000)
P+Q-S (0.000000000000000 : 1.00000000000000 : 0.000000000000000) ## Zero
P*12345 (10.7585721817304 : 35.3874428812067 : 1.00000000000000)

```



## Sidef

{{trans|Perl 6}}

```ruby
module EC {

    var A = 0
    var B = 7

    class Horizon {
        method to_s {
            "EC Point at horizon"
        }

        method *(_) {
            self
        }

        method -(_) {
            self
        }
    }

    class Point(Number x, Number y) {
        method to_s {
            "EC Point at x=#{x}, y=#{y}"
        }

        method neg {
            Point(x, -y)
        }

        method -(Point p) {
            self + -p
        }

        method +(Point p) {

            if (x == p.x) {
                return (y == p.y ? self*2 : Horizon())
            }
            else {
                var slope = (p.y - y)/(p.x - x)
                var x2 = (slope**2 - x - p.x)
                var y2 = (slope * (x - x2) - y)
                Point(x2, y2)
            }
        }

        method +(Horizon _) {
            self
        }

        method *((0)) {
            Horizon()
        }

        method *((1)) {
            self
        }

        method *((2)) {
            var l = (3 * x**2 + A)/(2 * y)
            var x2 = (l**2 - 2*x)
            var y2 = (l * (x - x2) - y)
            Point(x2, y2)
        }

        method *(Number n) {
            2*(self * (n>>1)) + self*(n % 2)
        }
    }

    class Horizon {
        method +(Point p) {
            p
        }
    }

    class Number {
        method +(Point p) {
            p + self
        }
        method *(Point p) {
            p * self
        }
        method *(Horizon h) {
            h
        }
        method -(Point p) {
            -p + self
        }
    }
}

say var p = with(1) {|v| EC::Point(v, sqrt(abs(1 - v**3 - EC::A*v - EC::B))) }
say var q = with(2) {|v| EC::Point(v, sqrt(abs(1 - v**3 - EC::A*v - EC::B))) }
say var s = (p + q)

say ("checking alignment:  ", abs((p.x - q.x)*(-s.y - q.y) - (p.y - q.y)*(s.x - q.x)) < 1e-20)
```

{{out}}

```txt

EC Point at x=1, y=2.64575131106459059050161575363926042571025918308
EC Point at x=2, y=3.74165738677394138558374873231654930175601980778
EC Point at x=-1.79898987322333068322364213893577309997540625528, y=0.421678696849803028974882458314430376814790014487
checking alignment:  true

```



## Tcl

{{trans|C}}

```tcl
set C 7
set zero {x inf y inf}
proc tcl::mathfunc::cuberoot n {
    # General power operator doesn't like negative, but its defined for root3
    expr {$n>=0 ? $n**(1./3) : -((-$n)**(1./3))}
}
proc iszero p {
    dict with p {}
    return [expr {$x > 1e20 || $x<-1e20}]
}
proc negate p {
    dict set p y [expr {-[dict get $p y]}]
}
proc double p {
    if {[iszero $p]} {return $p}
    dict with p {}
    set L [expr {(3.0 * $x**2) / (2.0 * $y)}]
    set rx [expr {$L**2 - 2.0 * $x}]
    set ry [expr {$L * ($x - $rx) - $y}]
    return [dict create x $rx y $ry]
}
proc add {p q} {
    if {[dict get $p x]==[dict get $q x] && [dict get $p y]==[dict get $q y]} {
	return [double $p]
    }
    if {[iszero $p]} {return $q}
    if {[iszero $q]} {return $p}

    dict with p {}
    set L [expr {([dict get $q y]-$y) / ([dict get $q x]-$x)}]
    dict set r x [expr {$L**2 - $x - [dict get $q x]}]
    dict set r y [expr {$L * ($x - [dict get $r x]) - $y}]
    return $r
}
proc multiply {p n} {
    set r $::zero
    for {set i 1} {$i <= $n} {incr i $i} {
	if {$i & int($n)} {
	    set r [add $r $p]
	}
	set p [double $p]
    }
    return $r
}
```

Demonstrating:

```tcl
proc show {s p} {
    if {[iszero $p]} {
	puts "${s}Zero"
    } else {
	dict with p {}
	puts [format "%s(%.3f, %.3f)" $s $x $y]
    }
}
proc fromY y {
    global C
    dict set r x [expr {cuberoot($y**2 - $C)}]
    dict set r y [expr {double($y)}]
}

set a [fromY 1]
set b [fromY 2]
show "a = " $a
show "b = " $b
show "c = a + b = " [set c [add $a $b]]
show "d = -c = " [set d [negate $c]]
show "c + d = " [add $c $d]
show "a + b + d = " [add $a [add $b $d]]
show "a * 12345 = " [multiply $a 12345]
```

{{out}}

```txt

a = (-1.817, 1.000)
b = (-1.442, 2.000)
c = a + b = (10.375, -33.525)
d = -c = (10.375, 33.525)
c + d = Zero
a + b + d = Zero
a * 12345 = (10.759, 35.387)

```


{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
{{omit from|TPP}}
