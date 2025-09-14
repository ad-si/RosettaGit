+++
title = "Arithmetic-geometric mean/Calculate Pi"
description = ""
date = 2019-10-09T15:22:30Z
aliases = []
[extra]
id = 17074
[taxonomies]
categories = ["task"]
tags = ["geometry"]
languages = [
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "d",
  "erlang",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "mathematica",
  "mk_61_52",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "tcl",
  "visual_basic_dotnet",
]
+++


[Almkvist Berndt 1988](http://www.maa.org/sites/default/files/pdf/upload_library/22/Ford/Almkvist-Berndt585-608.pdf)
begins with an investigation of why the agm is such an efficient algorithm,
and proves that it converges quadratically.
This is an efficient method to calculate <math>\pi</math>.

With the same notations used in [[Arithmetic-geometric mean]], we can summarize the paper by writing:

<math>\pi =
\frac{4\; \mathrm{agm}(1, 1/\sqrt{2})^2}
{1 - \sum\limits_{n=1}^{\infty} 2^{n+1}(a_n^2-g_n^2)}
</math>

This allows you to make the approximation, for any large   '''N''':

<math>\pi \approx
\frac{4\; a_N^2}
{1 - \sum\limits_{k=1}^N 2^{k+1}(a_k^2-g_k^2)}
</math>

The purpose of this task is to demonstrate how to use this approximation
in order to compute a large number of decimals of <math>\pi</math>.


## C

See [[Talk:Arithmetic-geometric mean]]

```c
#include "gmp.h"

void agm (const mpf_t in1, const mpf_t in2, mpf_t out1, mpf_t out2) {
	mpf_add (out1, in1, in2);
	mpf_div_ui (out1, out1, 2);
	mpf_mul (out2, in1, in2);
	mpf_sqrt (out2, out2);
}

int main (void) {
	mpf_set_default_prec (300000);
	mpf_t x0, y0, resA, resB, Z, var;

	mpf_init_set_ui (x0, 1);
	mpf_init_set_d (y0, 0.5);
	mpf_sqrt (y0, y0);
	mpf_init (resA);
	mpf_init (resB);
	mpf_init_set_d (Z, 0.25);
	mpf_init (var);

	int n = 1;
        int i;
	for(i=0; i<8; i++){
		agm(x0, y0, resA, resB);
		mpf_sub(var, resA, x0);
		mpf_mul(var, var, var);
		mpf_mul_ui(var, var, n);
		mpf_sub(Z, Z, var);
		n += n;
		agm(resA, resB, x0, y0);
		mpf_sub(var, x0, resA);
		mpf_mul(var, var, var);
		mpf_mul_ui(var, var, n);
		mpf_sub(Z, Z, var);
		n += n;
	}
	mpf_mul(x0, x0, x0);
	mpf_div(x0, x0, Z);
	gmp_printf ("%.100000Ff\n", x0);
	return 0;
}
```


```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## C\#

{{libheader|System.Numerics}}
Can specify the number of desired digits on the command line,
default is 25000, which takes a few seconds
(depending on your system's performance).

```c#
using System;
using System.Numerics;

class AgmPie
{
    static BigInteger IntSqRoot(BigInteger valu, BigInteger guess)
    {
        BigInteger term; do {
            term = valu / guess; if (BigInteger.Abs(term - guess) <= 1) break;
            guess += term; guess >>= 1;
        } while (true); return guess;
    }

    static BigInteger ISR(BigInteger term, BigInteger guess)
    {
        BigInteger valu = term * guess; do {
            if (BigInteger.Abs(term - guess) <= 1) break;
            guess += term; guess >>= 1; term = valu / guess;
        } while (true); return guess;
    }

    static BigInteger CalcAGM(BigInteger lam, BigInteger gm, ref BigInteger z,
                              BigInteger ep)
    {
        BigInteger am, zi; ulong n = 1; do {
            am = (lam + gm) >> 1; gm = ISR(lam, gm);
            BigInteger v = am - lam; if ((zi = v * v * n) < ep) break;
            z -= zi; n <<= 1; lam = am;
        } while (true); return am;
    }

    static BigInteger BIP(int exp, ulong man = 1)
    {
        BigInteger rv = BigInteger.Pow(10, exp); return man == 1 ? rv : man * rv;
    }

    static void Main(string[] args)
    {
        int d = 25000;
        if (args.Length > 0)
        {
            int.TryParse(args[0], out d);
            if (d < 1 || d > 999999) d = 25000;
        }
        DateTime st = DateTime.Now;
        BigInteger am = BIP(d),
          gm = IntSqRoot(BIP(d + d - 1, 5),
                             BIP(d - 15, (ulong)(Math.Sqrt(0.5) * 1e+15))),
          z = BIP(d + d - 2, 25),
          agm = CalcAGM(am, gm, ref z, BIP(d + 1)),
          pi = agm * agm * BIP(d - 2) / z;
        Console.WriteLine("Computation time: {0:0.0000} seconds ",
                             (DateTime.Now - st).TotalMilliseconds / 1000);
        string s = pi.ToString();
        Console.WriteLine("{0}.{1}", s[0], s.Substring(1));
        if (System.Diagnostics.Debugger.IsAttached) Console.ReadKey();
    }
}
```

```txt
Computation time: 4.1380 seconds
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## C++

{{Output?}}
{{trans|C}}
{{libheader|GMP}}

```cpp
#include <gmpxx.h>

void agm(mpf_class& rop1, mpf_class& rop2, const mpf_class& op1,
        const mpf_class& op2)
{
    rop1 = (op1 + op2) / 2;
    rop2 = op1 * op2;
    mpf_sqrt(rop2.get_mpf_t(), rop2.get_mpf_t());
}

int main(void)
{
    mpf_set_default_prec(300000);
    mpf_class x0, y0, resA, resB, Z;

    x0 = 1;
    y0 = 0.5;
    Z  = 0.25;
    mpf_sqrt(y0.get_mpf_t(), y0.get_mpf_t());

    int n = 1;
    for (int i = 0; i < 8; i++) {
        agm(resA, resB, x0, y0);
        Z -= n * (resA - x0) * (resA - x0);
        n *= 2;

        agm(x0, y0, resA, resB);
        Z -= n * (x0 - resA) * (x0 - resA);
        n *= 2;
    }

    x0 = x0 * x0 / Z;
    gmp_printf ("%.100000Ff\n", x0.get_mpf_t());
    return 0;
}
```


## Clojure

Translation from Ruby

```lisp
(ns async-example.core
  (:use [criterium.core])
  (:gen-class))

; Java Arbitray Precision Library
(import '(org.apfloat Apfloat ApfloatMath))

(def precision 8192)

; Define big constants (i.e. 1, 2, 4, 0.5, .25, 1/sqrt(2))
(def one (Apfloat. 1M precision))
(def two (Apfloat. 2M precision))
(def four (Apfloat. 4M precision))
(def half (Apfloat. 0.5M precision))
(def quarter (Apfloat. 0.25M precision))
(def isqrt2 (.divide one  (ApfloatMath/pow two half)))

(defn compute-pi [iterations]
    (loop [i 0, n one, [a g] [one isqrt2], z quarter]
        (if (> i iterations)
          (.divide (.multiply a a) z)
          (let [x [(.multiply (.add a g) half) (ApfloatMath/pow (.multiply a g) half)]
                v (.subtract (first x) a)]
            (recur (inc i) (.add n n) x (.subtract z (.multiply (.multiply v v) n)))))))

(doseq [q (partition-all 200 (str (compute-pi 18)))]
    (println (apply str q)))

```

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## Common Lisp

{{libheader|MMA}}

This is an example that uses the Common Lisp Bigfloat Package
(http://www.cs.berkeley.edu/~fateman/lisp/mma4max/more/bf.lisp)

```lisp
(load "bf.fasl")

;;(setf mma::bigfloat-bin-prec 1000)

(let ((A (mma:bigfloat-convert 1.0d0))
      (N (mma:bigfloat-convert 1.0d0))
      (Z (mma:bigfloat-convert 0.25d0))
      (G (mma:bigfloat-/ (mma:bigfloat-convert 1.0d0)
			 (mma:bigfloat-sqrt (mma:bigfloat-convert 2.0d0)))))
  (loop repeat 18 do
       (let* ((X1  (mma:bigfloat-* (mma:bigfloat-+ A G) (mma:bigfloat-convert 0.5d0)))
	      (X2 (mma:bigfloat-sqrt (mma:bigfloat-* A G)))
	      (V (mma:bigfloat-- X1 A)))
	 (setf Z (mma:bigfloat-- Z  (mma:bigfloat-* (mma:bigfloat-/ (mma:bigfloat-* V V) (mma:bigfloat-convert 1.0d0)) N) ))
	 (setf N (mma:bigfloat-+ N N))
	 (setf A X1)
	 (setf G X2)))
  (mma:bigfloat-/ (mma:bigfloat-* A A) Z))
```

{{out}}

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## D

Translation from C#

```d
import std.bigint;
import std.conv;
import std.math;
import std.stdio;

BigInt IntSqRoot(BigInt value, BigInt guess) {
    BigInt term;
    do {
        term = value / guess;
        auto temp = term - guess;
        if (temp < 0) {
            temp = -temp;
        }
        if (temp <= 1) {
            break;
        }
        guess += term;
        guess >>= 1;
        term = value / guess;
    } while (true);
    return guess;
}

BigInt ISR(BigInt term, BigInt guess) {
    BigInt value = term * guess;
    do {
        auto temp = term - guess;
        if (temp < 0) {
            temp = -temp;
        }
        if (temp <= 1) {
            break;
        }
        guess += term;
        guess >>= 1;
        term = value / guess;
    } while (true);
    return guess;
}

BigInt CalcAGM(BigInt lam, BigInt gm, ref BigInt z, BigInt ep) {
    BigInt am, zi;
    ulong n = 1;
    do {
        am = (lam + gm) >> 1;
        gm = ISR(lam, gm);
        BigInt v = am - lam;
        if ((zi = v * v * n) < ep) {
            break;
        }
        z -= zi;
        n <<= 1;
        lam = am;
    } while(true);
    return am;
}

BigInt BIP(int exp, ulong man = 1) {
    BigInt rv = BigInt(10) ^^ exp;
    return man == 1 ? rv : man * rv;
}

void main() {
    int d = 25000;
    // ignore setting d from commandline for now
    BigInt am = BIP(d);
    BigInt gm = IntSqRoot(BIP(d + d - 1, 5), BIP(d - 15, cast(ulong)(sqrt(0.5) * 1e15)));
    BigInt z = BIP(d + d - 2, 25);
    BigInt agm = CalcAGM(am, gm, z, BIP(d + 1));
    BigInt pi = agm * agm * BIP(d - 2) / z;

    string piStr = to!string(pi);
    writeln(piStr[0], '.', piStr[1..$]);
}
```

{{out}}

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## Erlang

Translation from Python

```erlang
-module(pi).
-export([agmPi/1, agmPiBody/5]).

agmPi(Loops) ->
    % Tail recursive function that produces pi from the Arithmetic Geometric Mean method
    A = 1,
    B = 1/math:sqrt(2),
    J = 1,
    Running_divisor = 0.25,
    A_n_plus_one = 0.5*(A+B),
    B_n_plus_one = math:sqrt(A*B),
    Step_difference = A_n_plus_one - A,
    agmPiBody(Loops-1, Running_divisor-(math:pow(Step_difference, 2)*J), A_n_plus_one, B_n_plus_one, J+J).

agmPiBody(0, Running_divisor, A, _, _) ->
    math:pow(A, 2)/Running_divisor;
agmPiBody(Loops, Running_divisor, A, B, J) ->
    A_n_plus_one = 0.5*(A+B),
    B_n_plus_one = math:sqrt(A*B),
    Step_difference = A_n_plus_one - A,
    agmPiBody(Loops-1, Running_divisor-(math:pow(Step_difference, 2)*J), A_n_plus_one, B_n_plus_one, J+J).

```

{{out}}

```txt
3.141592653589794
```


## Go

```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    one := big.NewFloat(1)
    two := big.NewFloat(2)
    four := big.NewFloat(4)
    prec := uint(768) // say

    a := big.NewFloat(1).SetPrec(prec)
    g := new(big.Float).SetPrec(prec)

    // temporary variables
    t := new(big.Float).SetPrec(prec)
    u := new(big.Float).SetPrec(prec)

    g.Quo(a, t.Sqrt(two))
    sum := new(big.Float)
    pow := big.NewFloat(2)

    for a.Cmp(g) != 0 {
        t.Add(a, g)
        t.Quo(t, two)
        g.Sqrt(u.Mul(a, g))
        a.Set(t)
        pow.Mul(pow, two)
        t.Sub(t.Mul(a, a), u.Mul(g, g))
        sum.Add(sum, t.Mul(t, pow))
    }

    t.Mul(a, a)
    t.Mul(t, four)
    pi := t.Quo(t, u.Sub(one, sum))
    fmt.Println(pi)
}
```

{{out}}

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```



## Haskell

{{libheader|MPFR}}
{{libheader|hmpfr}}

```Haskell
import Prelude hiding (pi)
import Data.Number.MPFR hiding (sqrt, pi, div)
import Data.Number.MPFR.Instances.Near ()

-- A generous overshoot of the number of bits needed for a
-- given number of digits.
digitBits :: (Integral a, Num a) => a -> a
digitBits n = (n + 1) `div` 2 * 8

-- Calculate pi accurate to a given number of digits.
pi :: Integer -> MPFR
pi digits =
  let eps = fromString ("1e-" ++ show digits)
            (fromInteger $ digitBits digits) 0
      two = fromInt Near (getPrec eps) 2
      twoi = 2 :: Int
      twoI = 2 :: Integer
      pis a g s n =
        let aB = (a + g) / two
            gB = sqrt (a * g)
            aB2 = aB ^^ twoi
            sB = s + (two ^^ n) * (aB2 - gB ^^ twoi)
            num = 4 * aB2
            den = 1 - sB
        in (num / den) : pis aB gB sB (n + 1)
      puntil f (a:b:xs) = if f a b then b else puntil f (b:xs)
  in puntil (\a b -> abs (a - b) < eps)
     $ pis one (one / sqrt two) zero twoI

main :: IO ()
main = do
  -- The last decimal is rounded.
  putStrLn $ toString 1000 $ pi 1000
```

{{out}}

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```



## J

Relevant J essays:
[http://www.jsoftware.com/jwiki/Essays/Extended%20Precision%20Functions|Extended precision functions]
and [http://www.jsoftware.com/jwiki/Essays/Chudnovsky%20Algorithm|Pi]

Translated from Python:

```J
DP=: 100

round=: DP&$: : (4 : 0)
 b %~ <.1r2+y*b=. 10x^x
)

sqrt=: DP&$: : (4 : 0) " 0
 assert. 0<:y
 %/ <.@%: (2 x: (2*x) round y)*10x^2*x+0>.>.10^.y
)

pi =: 3 : 0
 A =. N =. 1x
 'G Z HALF' =. (% sqrt 2) , 1r4 1r2
 for_I. i.18 do.
  X =. ((A + G) * HALF) , sqrt A * G
  VAR =. ({.X) - A
  Z =. Z - VAR * VAR * N
  N =. +: N
  'A G' =. X
  PI =: A * A % Z
  smoutput (0j100":PI) , 4 ": I
 end.
 PI
)
```

In this run the result is a rational approximation to pi.
Only part of the numerator shows.
The algorithm produces 100 decimal digits by the eighth iteration.

```txt
      pi''
3.1876726427121086272019299705253692326510535718593692264876339862751228325281223301147286106601617974   0
3.1416802932976532939180704245600093827957194388154028326441894631956630010102553193888894275152646103   1
3.1415926538954464960029147588180434861088792372613115896511013576846530795030865017740975862898631570   2
3.1415926535897932384663606027066313217577024113424293564868460152384109486069277582680622007332762131   3
3.1415926535897932384626433832795028841971699491647266058346961259487480060953290058518515759317101939   4
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280468522286541150   5
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680   6
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680   7
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680   8
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680   9
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  10
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  11
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  12
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  13
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  14
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  15
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  16
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170680  17
1583455951826865080542496790424338362837978447536228171662934224565463064033895909488933268392567279887495006936541219489670405121434573776487989539520749180843985094860051126840117004097133550161882511486508109869673199973040182062140382647367514024790194...
```


## Java

{{trans|Kotlin}}

Uses features of Java 8

```Java
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Objects;

public class Calculate_Pi {
    private static final MathContext con1024 = new MathContext(1024);
    private static final BigDecimal bigTwo = new BigDecimal(2);
    private static final BigDecimal bigFour = new BigDecimal(4);

    private static BigDecimal bigSqrt(BigDecimal bd, MathContext con) {
        BigDecimal x0 = BigDecimal.ZERO;
        BigDecimal x1 = BigDecimal.valueOf(Math.sqrt(bd.doubleValue()));
        while (!Objects.equals(x0, x1)) {
            x0 = x1;
            x1 = bd.divide(x0, con).add(x0).divide(bigTwo, con);
        }
        return x1;
    }

    public static void main(String[] args) {
        BigDecimal a = BigDecimal.ONE;
        BigDecimal g = a.divide(bigSqrt(bigTwo, con1024), con1024);
        BigDecimal t;
        BigDecimal sum = BigDecimal.ZERO;
        BigDecimal pow = bigTwo;
        while (!Objects.equals(a, g)) {
            t = a.add(g).divide(bigTwo, con1024);
            g = bigSqrt(a.multiply(g), con1024);
            a = t;
            pow = pow.multiply(bigTwo);
            sum = sum.add(a.multiply(a).subtract(g.multiply(g)).multiply(pow));
        }
        BigDecimal pi = bigFour.multiply(a.multiply(a)).divide(BigDecimal.ONE.subtract(sum), con1024);
        System.out.println(pi);
    }
}
```

{{out}}

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## Julia

Tested with Julia 1.2


```julia
using Printf

agm1step(x, y) = (x + y) / 2, sqrt(x * y)

function approxπstep(x, y, z, n::Integer)
    a, g = agm1step(x, y)
    k = n + 1
    s = z + 2 ^ (k + 1) * (a ^ 2 - g ^ 2)
    return a, g, s, k
end

approxπ(a, g, s) = 4a ^ 2 / (1 - s)

function testmakepi()
	setprecision(512)
	a, g, s, k = BigFloat(1.0), 1 / √BigFloat(2.0), BigFloat(0.0), 0
	oldπ = BigFloat(0.0)
	println("Approximating π using ", precision(BigFloat), "-bit floats.")
	println("   k     Error  Result")
	for i in 1:100
		a, g, s, k = approxπstep(a, g, s, k)
		estπ = approxπ(a, g, s)
		if abs(estπ - oldπ) < 2eps(estπ) break end
		oldπ = estπ
		err = abs(π - estπ)
		@printf("%4d%10.1e%68.60e\n", i, err, estπ)
	end
end

testmakepi()

```

{{out}}


```txt

Approximating π using 512-bit floats.
   k     Error  Result
   1   4.6e-02  3.187672642712108627201929970525369232651053571859369226487634e+00
   2   8.8e-05  3.141680293297653293918070424560009382795719438815402832644189e+00
   3   3.1e-10  3.141592653895446496002914758818043486108879237261311589651101e+00
   4   3.7e-21  3.141592653589793238466360602706631321757702411342429356486846e+00
   5   5.5e-43  3.141592653589793238462643383279502884197169949164726605834696e+00
   6   1.2e-86  3.141592653589793238462643383279502884197169399375105820974945e+00
   7  2.6e-152  3.141592653589793238462643383279502884197169399375105820974945e+00

```

<tt>Error</tt> shows the difference between Julia's built-in &pi; constant and the result.
I've restricted the result output to 60 digits after the decimal point
to avoid excessive output line length.

'''Note'''

If I increase to precision to <tt>1024</tt>,
the result converges somewhat rapidly before oscillating about a small error
for several tens of iterations.
A more sophisticated convergence criterion is called for
if one desires such heroic precision.
Perhaps a check on the order of magnitude change in the difference
from one iteration to the next would be more appropriate.


## Kotlin

```kotlin
import java.math.BigDecimal
import java.math.MathContext

val con1024 = MathContext(1024)
val bigTwo  = BigDecimal(2)
val bigFour = bigTwo * bigTwo

fun bigSqrt(bd: BigDecimal, con: MathContext): BigDecimal {
    var x0 = BigDecimal.ZERO
    var x1 = BigDecimal.valueOf(Math.sqrt(bd.toDouble()))
    while (x0 != x1) {
        x0 = x1
        x1 = bd.divide(x0, con).add(x0).divide(bigTwo, con)
    }
    return x1
}

fun main(args: Array<String>) {
    var a = BigDecimal.ONE
    var g = a.divide(bigSqrt(bigTwo, con1024), con1024)
    var t : BigDecimal
    var sum = BigDecimal.ZERO
    var pow = bigTwo
    while (a != g) {
        t = (a + g).divide(bigTwo, con1024)
        g = bigSqrt(a * g, con1024)
        a = t
        pow *= bigTwo
        sum += (a * a - g * g) * pow
    }
    val pi = (bigFour * a * a).divide(BigDecimal.ONE - sum, con1024)
    println(pi)
}
```

{{out}}

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## Mathematica

{{Output?}}
Note that the precision setting does not control
the number of significant digits,
but after 10 steps with a precision specification of 5,
its difference with the actual number is 0.x10^-996.

```mathematica
piCalc[n_,precision_]:=($precision=precision;4*a[n]^2)/(1-Sum[2^(1+k)*(a[k]^2-b[k]^2),{k,1,n}])
a[h_]:=(a[h]=(N[#,$precision]&@a[h-1]+b[h-1])/2)
b[h_]:=(b[h]=N[#,$precision]&@Sqrt[a[h-1] b[h-1]])
a[0]=1;
b[0]=1/Sqrt[2];

N[Pi, 1000000] - piCalc[10, 5]
0.*10^-996
```


## MK-61/52

```txt
3	П0	1	П1	П4	2	КвКор	1/x	П2	1
^	4	/	П3	ИП3	ИП1	ИП2	+	2	/
П5	ИП1	-	x^2	ИП4	*	-	П3	ИП1	ИП2
*	КвКор	П2	ИП5	П1	КИП4	L0	14	ИП1	x^2
ИП3	/	С/П
```


## OCaml

Program for calculating digits of &pi;

```OCaml
let limit = 10000 and n = 2800
let x = Array.make (n+1) 2000

let rec g j sum =
  if j < 1 then sum else
    let sum = sum * j + limit * x.(j) in
    x.(j) <- sum mod (j * 2 - 1);
    g (j - 1) (sum / (j * 2 - 1))

let rec f i carry =
  if i = 0 then () else
    let sum = g i 0 in
    Printf.printf "%04d" (carry + sum / limit);
    f (i - 14) (sum mod limit)

let () =
  f n 0;
  print_newline()

```

{{out}}

```txt
314159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## PARI/GP

```parigp
pi(n)=my(a=1,g=2^-.5);(1-2*sum(k=1,n,[a,g]=[(a+g)/2,sqrt(a*g)];(a^2-g^2)<<k))^-1*4*a^2
pi(6)
```

{{out}}

```txt
%1 = 3.1415926535897932384626433832795028841971693993751058209749445923078164062878
```


## Perl

We use excess precision internally to make sure the last digits
are rounded correctly (the caveat being that no number of fixed guard digits
can work for all of Pi).
For performance we try to use the GMP or Pari backends for Math::BigInt,
which if installed will make this run '''hundreds''' of times faster.
Because Math::BigInt is inefficient,
we've used the methods directly when possible to avoid unnecessary copies,
although it obfuscates somewhat.
Additionally using the object methods lets us trim excess digits
from intermediates as part of the calculation (e.g. in the square root).

The number of steps used is based on the desired accuracy
rather than being hard coded,
as this is intended to work for 1M digits as well as for 100.


```perl
>use Math::BigFloat try =
 "GMP,Pari";

my $digits = shift || 100;   # Get number of digits from command line
print agm_pi($digits), "\n";

sub agm_pi {
  my $digits = shift;
  my $acc = $digits + 8;
  my $HALF = Math::BigFloat->new("0.5");
  my ($an, $bn, $tn, $pn) = (Math::BigFloat->bone, $HALF->copy->bsqrt($acc),
                             $HALF->copy->bmul($HALF), Math::BigFloat->bone);
  while ($pn < $acc) {
    my $prev_an = $an->copy;
    $an->badd($bn)->bmul($HALF, $acc);
    $bn->bmul($prev_an)->bsqrt($acc);
    $prev_an->bsub($an);
    $tn->bsub($pn * $prev_an * $prev_an);
    $pn->badd($pn);
  }
  $an->badd($bn);
  $an->bmul($an,$acc)->bdiv(4*$tn, $digits);
  return $an;
}
```

{{out}}

```txt
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
```

The following is a translation, almost line-for-line, of the Ruby code.
It is slower than the above and the last digit or two may not be correct.


```perl
use strict;
use warnings;
use Math::BigFloat;

Math::BigFloat->div_scale(100);

my $a = my $n = 1;
my $g = 1 / sqrt(Math::BigFloat->new(2));
my $z = 0.25;
for( 0 .. 17 ) {
	my $x = [ ($a + $g) * 0.5, sqrt($a * $g) ];
	my $var = $x->[0] - $a;
	$z -= $var * $var * $n;
	$n += $n;
	($a, $g) = @$x;
}
print $a * $a / $z, "\n";
```

{{out}}

```txt
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067
```



## Perl 6

Translated from Ruby.

There is not yet a FixDecimal type module in Perl 6,
and using FatRat all along would be too slow and would be coerced to Num
when computing the square root anyway,
so we'll use a custom definition of the square root for Int and FatRat,
with a limitation to the number of decimals.
We'll show all the intermediate results.

The trick to compute the square root of a rational <math>n\over d</math>
up to a certain amount of decimals N is to write:

<math>\sqrt{\frac{n}{d}} = \sqrt{
\frac{n 10^{2N} / d}{d 10^{2N} / d}
} = \frac{\sqrt{n 10^{2N} / d}}{10^N}</math>

so that what we need is one square root of a big number
that we'll truncate to its integer part.
We'll compute the square root of this big integer
by using the convergence of the recursive sequence:

<math>u_{n+1} = \frac{1}{2}(u_n + \frac{x}{u_n})</math>

It's not too hard to see that such a sequence converges
towards <math>\sqrt x</math>.

Notice that we don't get the exact number of decimals required :
the last two decimals or so can be wrong.
This is because we don't need <math>a_n</math>, but rather <math>a_n^2</math>.
Elevating to the square makes us lose a bit of precision.
It could be compensated by choosing a slightly higher value of N
(in a way that could be precisely calculated),
but that would probably be overkill.

```perl6
constant number-of-decimals = 100;

multi sqrt(Int $n) {
    my $guess = 10**($n.chars div 2);
    my $iterator = { ( $^x   +   $n div ($^x) ) div 2 };
    my $endpoint = { $^x == $^y|$^z };
    return min (+$guess, $iterator … $endpoint)[*-1, *-2];
}

multi sqrt(FatRat $r --> FatRat) {
    return FatRat.new:
    sqrt($r.nude[0] * 10**(number-of-decimals*2) div $r.nude[1]),
    10**number-of-decimals;
}

my FatRat ($a, $n) = 1.FatRat xx 2;
my FatRat $g = sqrt(1/2.FatRat);
my $z = .25;

for ^10 {
    given [ ($a + $g)/2, sqrt($a * $g) ] {
	$z -= (.[0] - $a)**2 * $n;
	$n += $n;
	($a, $g) = @$_;
	say ($a ** 2 / $z).substr: 0, 2 + number-of-decimals;
    }
}
```

{{out}}

```txt
3.1876726427121086272019299705253692326510535718593692264876339862751228325281223301147286106601617972
3.1416802932976532939180704245600093827957194388154028326441894631956630010102553193888894275152646100
3.1415926538954464960029147588180434861088792372613115896511013576846530795030865017740975862898631567
3.1415926535897932384663606027066313217577024113424293564868460152384109486069277582680622007332762125
3.1415926535897932384626433832795028841971699491647266058346961259487480060953290058518515759317101932
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280468522286541140
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170668
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170665
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170664
3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170663
```



## Phix

{{libheader|mpfr}}
{{trans|Python}}

```Phix
include mpfr.e

mpfr_set_default_prec(-200) -- set precision to 200 decimal places
mpfr a = mpfr_init(1),
     n = mpfr_init(1),
     g = mpfr_init(1),
     z = mpfr_init(0.25),
     half = mpfr_init(0.5),
     x1 = mpfr_init(2),
     x2 = mpfr_init(),
     var = mpfr_init()
mpfr_sqrt(x1,x1)
mpfr_div(g,g,x1)    -- g:= 1/sqrt(2)
string prev, this, fmt = "%.200Rf\n"
for i=1 to 18 do
    mpfr_add(x1,a,g)
    mpfr_mul(x1,x1,half)
    mpfr_mul(x2,a,g)
    mpfr_sqrt(x2,x2)
    mpfr_sub(var,x1,a)
    mpfr_mul(var,var,var)
    mpfr_mul(var,var,n)
    mpfr_sub(z,z,var)
    mpfr_add(n,n,n)
    mpfr_set(a,x1)
    mpfr_set(g,x2)
    mpfr_mul(var,a,a)
    mpfr_div(var,var,z)
    this = mpfr_sprintf(fmt,var)
    if i>1 then
        if this=prev then exit end if
        for j=3 to length(this) do
            if prev[j]!=this[j] then
                printf(1,"iteration %d matches previous to %d places\n",{i,j-3})
                exit
            end if
        end for
    end if
    prev = this
end for
if this=prev then
    printf(1,"identical result to last iteration:\n%s\n",{this})
else
    printf(1,"insufficient iterations\n")
end if
```

{{out}}

```txt

iteration 2 matches previous to 1 places
iteration 3 matches previous to 3 places
iteration 4 matches previous to 9 places
iteration 5 matches previous to 20 places
iteration 6 matches previous to 42 places
iteration 7 matches previous to 85 places
iteration 8 matches previous to 173 places
identical result to last iteration:
3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196

```



## PicoLisp

Translated from Python

```PicoLisp
(scl 40)

(de pi ()
   (let
      (A 1.0  N 1.0  Z 0.25
         G (/ (* 1.0 1.0) (sqrt 2.0 1.0)) )
      (use (X1 X2 V)
         (do 18
            (setq
               X1 (/ (* (+ A G) 0.5) 1.0)
               X2 (sqrt (* A G))
               V (- X1 A)
               Z (- Z (/ (* (/ (* V V) 1.0) N) 1.0))
               N (+ N N)
               A X1
               G X2 ) ) )
      (round (/ (* A A) Z) 40)) )

(println (pi))

(bye)
```

{{out}}

```txt
"3.1415926535897932384626433832795028841841"
```


## Python

Translated from Ruby

```python
from decimal import *

D = Decimal
getcontext().prec = 100
a = n = D(1)
g, z, half = 1 / D(2).sqrt(), D(0.25), D(0.5)
for i in range(18):
    x = [(a + g) * half, (a * g).sqrt()]
    var = x[0] - a
    z -= var * var * n
    n += n
    a, g = x
print(a * a / z)
```


{{out}}

```txt
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067
```


## Racket

Translated from Ruby

```Racket
#lang racket
(require math/bigfloat)

(define (pi/a-g rep)
  (let loop ([a 1.bf]
             [g (bf1/sqrt 2.bf)]
             [z (bf/ 1.bf 4.bf)]
             [n (bf 1)]
             [r 0])
    (if (< r rep)
        (let* ([a-p (bf/ (bf+ a g) 2.bf)]
               [g-p (bfsqrt (bf* a g))]
               [z-p (bf- z (bf* (bfsqr (bf- a-p a)) n))])
          (loop a-p g-p z-p (bf* n 2.bf) (add1 r)))
        (bf/ (bfsqr a) z))))

(parameterize ([bf-precision 100])
  (displayln (bigfloat->string (pi/a-g 5)))
  (displayln (bigfloat->string pi.bf)))

(parameterize ([bf-precision 200])
  (displayln (bigfloat->string (pi/a-g 6)))
  (displayln (bigfloat->string pi.bf)))
```

{{Out}}

```txt
3.1415926535897932384626433832793
3.1415926535897932384626433832793
3.141592653589793238462643383279502884197169399375105820974942
3.1415926535897932384626433832795028841971693993751058209749445
```



## REXX

{{trans|Ruby}}

Programming note:
The number of digits to be used in the calculations
can be specified on the C.L. ('''c'''ommand '''l'''ine).

Whatever number of digits used,
the actual number of digits is five larger than specified,
and then the result is rounded to the requested number of digits.


### Version 1

```rexx
/*REXX program calculates the value of  pi  using the  AGM  algorithm.                  */
parse arg d .;   if d=='' | d==","  then d=500   /*D  not specified?  Then use default. */
numeric digits d+5                               /*set the numeric decimal digits to D+5*/
z=1/4;                  a=1;       g=sqrt(1/2)   /*calculate some initial values.       */
n=1
        do j=1   until  a==old;    old=a         /*keep calculating until no more noise.*/
        x=(a+g)*.5;                g=sqrt(a*g)   /*calculate the next set of terms.     */
        z=z - n*(x-a)**2;  n=n+n;  a=x           /*Z  is used in the final calculation. */
        end   /*j*/                              /* [↑]  stop if  A  equals  OLD.       */

pi=a**2 / z                                      /*compute the finished  value of  pi.  */
numeric digits d                                 /*set the numeric decimal digits to  D.*/
say pi / 1                                       /*display the computed value of  pi.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();  numeric digits;  h=d+6
      numeric form; m.=9; parse value format(x,2,1,,0) 'E0' with g "E" _ .; g=g *.5'e'_ %2
            do j=0  while h>9;        m.j=h;                 h=h%2+1;          end  /*j*/
            do k=j+5  to 0  by -1;    numeric digits m.k;    g=(g+x/g)*.5;     end  /*k*/
      numeric digits d;     return g/1
```

Programming note:
The   '''sqrt'''   subroutine (above) is optimized for larger ''digits''.

'''output'''   using the default number of digits:   <tt> 500 </tt>

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```

All digits shown above are accurate;
it is rounded however, to the last digit shown
(in this case, there is no rounding).


### version 2

This REXX version shows the accurate (correct) number of digits
in each iteration of the calculation of pi.

```rexx
/*REXX program calculates value of   pi   using the AGM algorithm (with running digits).*/
parse arg d .;   if d=='' | d==","  then d=500   /*D  not specified?  Then use default. */
numeric digits d+5                               /*set the numeric decimal digits to D+5*/
z=1/4;                  a=1;       g=sqrt(1/2)   /*calculate some initial values.       */
n=1

        do j=1   until  a==old;  old=a           /*keep calculating until no more noise.*/
        x=(a+g)*.5;     g=sqrt(a*g)              /*calculate the next set of terms.     */
        z=z-n*(x-a)**2; n=n+n;   a=x             /*Z  is used in the final calculation. */
        many=compare(a,old)                      /*how many accurate digits computed?   */
        if many==0   then many=d                 /*adjust for the very last time.       */
        say right('iteration' j, 20)     right(many, 9)     "digits"      /*show digits.*/
        end   /*j*/                              /* [↑]  stop if    A    equals    OLD. */
say                                              /*display a blank line for a separator.*/
pi=a**2 / z                                      /*compute the finished  value of  pi.  */
numeric digits d                                 /*set the numeric decimal digits to  D.*/
say pi / 1                                       /*display the computed value of  pi.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();  numeric digits;  h=d+6
      numeric form; m.=9; parse value format(x,2,1,,0) 'E0' with g "E" _ .; g=g *.5'e'_ %2
            do j=0  while h>9;        m.j=h;                 h=h%2+1;          end  /*j*/
            do k=j+5  to 0  by -1;    numeric digits m.k;    g=(g+x/g)*.5;     end  /*k*/
      numeric digits d;     return g/1
```

'''output'''   using the default number of digits:   <tt> 500 </tt>

```txt

         iteration 1         1 digits
         iteration 2         4 digits
         iteration 3         7 digits
         iteration 4        12 digits
         iteration 5        23 digits
         iteration 6        46 digits
         iteration 7        89 digits
         iteration 8       175 digits
         iteration 9       351 digits
        iteration 10       500 digits

3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```

'''output'''   using the number of digits:   <tt> 100000 </tt>

```txt

         iteration 1         1 digits
         iteration 2         4 digits
         iteration 3         7 digits
         iteration 4        12 digits
         iteration 5        23 digits
         iteration 6        46 digits
         iteration 7        89 digits
         iteration 8       175 digits
         iteration 9       351 digits
        iteration 10       700 digits
        iteration 11      1399 digits
        iteration 12      2796 digits
        iteration 13      5590 digits
        iteration 14     11178 digits
        iteration 15     22356 digits
        iteration 16     44710 digits
        iteration 17     89417 digits
        iteration 18    100000 digits

3.141592653589793··· elided

```



### version 3


```rexx
Do d=10 To 13
  Say d pib(d)
  End
Do d=1000 To 1005
  pi=pib(d)
  say d left(pi,5)'...'substr(pi,997)
  End
Exit
pib: Procedure
/* REXX ---------------------------------------------------------------
* program calculates the value of pi using the  AGM  algorithm.
* building on top of version 2
* reformatted, improved, and using 'my own' sqrt
* 08.07.2014 Walter Pachl
*--------------------------------------------------------------------*/
  Parse Arg d .
  If d=='' Then
    d=500                           /* D specified?  Then use default.*/
  Numeric Digits d+5                /* set the numeric digits to D+5. */
  a=1
  n=1
  z=1/4
  g=sqrt(1/2)                       /* calculate some initial values. */
  Do j=1 Until a==old
    old=a                           /* keep calculating until no noise*/
    x=(a+g)*.5
    g=sqrt(a*g)                     /* calculate the next set of terms*/
    z=z-n*(x-a)**2
    n=n+n
    a=x
    End
  pi=a**2/z
  Numeric Digits d                  /* set the  numeric digits  to  D */
  Return pi+0

sqrt: Procedure
  Parse Arg x
  xprec=digits()
  iprec=xprec+10
  Numeric Digits iprec
  r0=x
  r =1
  Do i=1 By 1 Until r=r0 | (abs(r*r-x)<10**-iprec)
    r0 = r
    r  = (r + x/r) / 2
    End
  Numeric Digits xprec
  Return (r+0)
```

[{out}]

```txt
10 3.141592654
11 3.1415926536
12 3.14159265359
13 3.141592653590
1000 3.141...20199
1001 3.141...201989
1002 3.141...2019894
1003 3.141...20198938
1004 3.141...201989381
1005 3.141...2019893810
```



## Ruby

Using agm.
See [[Talk:Arithmetic-geometric mean]]

```ruby
# Calculate Pi using the Arithmetic Geometric Mean of 1 and 1/sqrt(2)
#
#
#  Nigel_Galloway
#  March 8th., 2012.
#
require 'flt'
Flt::BinNum.Context.precision = 8192
a = n = 1
g = 1 / Flt::BinNum(2).sqrt
z = 0.25
(0..17).each{
  x = [(a + g) * 0.5, (a * g).sqrt]
  var = x[0] - a
  z -= var * var * n
  n += n
  a = x[0]
  g = x[1]
}
puts a * a / z
```

Produces:

```txt
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```


## Rust


```rust
/// calculate pi with algebraic/geometric mean
pub fn pi(n: usize) -> f64 {
    let mut a : f64 = 1.0;
    let two : f64= 2.0;
    let mut g = 1.0 / two.sqrt();
    let mut s = 0.0;
    let mut k = 1;
    while k<=n  {

        let a1 = (a+g)/two;
        let g1 = (a*g).sqrt();
        a = a1;
        g = g1;
        s += (a.powi(2)-g.powi(2)) * two.powi((k+1) as i32);
        k += 1;


    }

    4.0 * a.powi(2) / (1.0-s)
}

```

Can be invoked like:

```rust

fn main() {
    println!("pi(7): {}", pi(7));
}

```

Outputs:

```txt
pi(7): 3.1415926535901733
```


Note: num crate could be used if sqrt was supported
(https://github.com/rust-num/num-rational/issues/35)


## Scala

### Completely (tail) recursive

```Scala
import java.math.MathContext

import scala.annotation.tailrec
import scala.compat.Platform.currentTime
import scala.math.BigDecimal

object Calculate_Pi extends App {
  val precision = new MathContext(32768 /*65536*/)
  val (bigZero, bigOne, bigTwo, bigFour) =
    (BigDecimal(0, precision), BigDecimal(1, precision), BigDecimal(2, precision), BigDecimal(4, precision))

  def bigSqrt(bd: BigDecimal) = {
    @tailrec
    def iter(x0: BigDecimal, x1: BigDecimal): BigDecimal =
      if (x0 == x1) x1 else iter(x1, (bd / x1 + x1) / bigTwo)

    iter(bigZero, BigDecimal(Math.sqrt(bd.toDouble), precision))
  }

  @tailrec
  private def loop(a: BigDecimal, g: BigDecimal, sum: BigDecimal, pow: BigDecimal): BigDecimal = {
    if (a == g) (bigFour * (a * a)) / (bigOne - sum)
    else {
      val (_a, _g, _pow) = ((a + g) / bigTwo, bigSqrt(a * g), pow * bigTwo)
      loop(_a, _g, sum + ((_a * _a - (_g * _g)) * _pow), _pow)
    }
  }

  println(precision)
  val pi = loop(bigOne, bigOne / bigSqrt(bigTwo), bigZero, bigTwo)
  println(s"This are ${pi.toString.length - 1} digits of π:")
  val lines = pi.toString().sliding(103, 103).mkString("\n")
  println(lines)

  println(s"Successfully completed without errors. [total ${currentTime - executionStart} ms]")
}
```

{{Out}}See it running in your browser by
[ScalaFiddle (JavaScript, non JVM)](https://scalafiddle.io/sf/z8KNd5c/2)
or by
[Scastie (JVM)](https://scastie.scala-lang.org/lTZhfzz2Ry2W7kJT0Iyoww).

Be patient, some heavy computing (~ 30 s) involved.


## Sidef

```ruby
func agm_pi(digits) {
    var acc = (digits + 8);

    local Num!PREC = 4*digits;

    var an = 1;
    var bn = sqrt(0.5);
    var tn = 0.5**2;
    var pn = 1;

    while (pn < acc) {
        var prev_an = an;
        an = (bn+an / 2);
        bn = sqrt(bn * prev_an);
        prev_an -= an;
        tn -= (pn * prev_an**2);
        pn *= 2;
    }

    ((an+bn)**2 / 4*tn).to_s
}

say agm_pi(100);
```

{{out}}

```txt
3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117068
```



## Tcl

{{trans|Ruby}}
{{tcllib|math::bigfloat}}

```tcl
package require math::bigfloat
namespace import math::bigfloat::*

proc agm/π {N {precision 8192}} {
    set 1 [int2float 1 $precision]
    set 2 [int2float 2 $precision]
    set n 1
    set a $1
    set g [div $1 [sqrt $2]]
    set z [div $1 [int2float 4 $precision]]
    for {set i 0} {$i <= $N} {incr i} {
	set x0 [div [add $a $g] $2]
	set x1 [sqrt [mul $a $g]]
	set var [sub $x0 $a]
	set z [sub $z [mul [mul $var $n] $var]]
	incr n $n
	set a $x0
	set g $x1
    }
    return [tostr [div [mul $a $a] $z]]
}

puts [agm/π 17]
```

{{out}}
<small>(with added line breaks for clarity)</small>
{{:Arithmetic-geometric mean/Calculate Pi/Tcl Output}}


## Visual Basic .NET

{{trans|C#}}
{{Libheader|System.Numerics}}

```vbnet
Imports System, System.Numerics

Module Program
    Function IntSqRoot(ByVal valu As BigInteger, ByVal guess As BigInteger) As BigInteger
        Dim term As BigInteger : Do
            term = valu / guess
            If BigInteger.Abs(term - guess) <= 1 Then Exit Do
            guess += term : guess >>= 1
        Loop While True : Return guess
    End Function

    Function ISR(ByVal term As BigInteger, ByVal guess As BigInteger) As BigInteger
        Dim valu As BigInteger = term * guess : Do
            If BigInteger.Abs(term - guess) <= 1 Then Exit Do
            guess += term : guess >>= 1 : term = valu / guess
        Loop While True : Return guess
    End Function

    Function CalcAGM(ByVal lam As BigInteger, ByVal gm As BigInteger, ByRef z As BigInteger,
                     ByVal ep As BigInteger) As BigInteger
        Dim am, zi As BigInteger : Dim n As ULong = 1 : Do
            am = (lam + gm) >> 1 : gm = ISR(lam, gm)
            Dim v As BigInteger = am - lam
            zi = v * v * n : If zi < ep Then Exit Do
            z -= zi : n <<= 1 : lam = am
        Loop While True : Return am
    End Function

    Function BIP(ByVal exp As Integer, ByVal Optional man As ULong = 1) As BigInteger
        Dim rv As BigInteger = BigInteger.Pow(10, exp) : Return If(man = 1, rv, man * rv)
    End Function

    Sub Main(args As String())
        Dim d As Integer = 25000
        If args.Length > 0 Then
            Integer.TryParse(args(0), d)
            If d < 1 OrElse d > 999999 Then d = 25000
        End If
        Dim st As DateTime = DateTime.Now
        Dim am As BigInteger = BIP(d),
            gm As BigInteger = IntSqRoot(BIP(d + d - 1, 5),
                                         BIP(d - 15, Math.Sqrt(0.5) * 1.0E+15)),
             z As BigInteger = BIP(d + d - 2, 25),
             agm As BigInteger = CalcAGM(am, gm, z, BIP(d + 1)),
             pi As BigInteger = agm * agm * BIP(d - 2) / z
        Console.WriteLine("Computation time: {0:0.0000} seconds ",
                          (DateTime.Now - st).TotalMilliseconds / 1000)
        If args.Length > 1 OrElse d <= 1000 Then
            Dim s As String = pi.ToString()
            Console.WriteLine("{0}.{1}", s(0), s.Substring(1))
        End If
        If Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module

```

```txt
Computation time: 4.1539 seconds
3.14159265358979323846264338327950288419716939937510582097494459230781640628
<cropped>
```
