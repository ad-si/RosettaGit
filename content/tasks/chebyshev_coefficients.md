+++
title = "Chebyshev coefficients"
description = ""
date = 2018-12-22T12:40:34Z
aliases = []
[extra]
id = 19463
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "cpp",
  "d",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "microsoft_small_basic",
  "perl",
  "perl_6",
  "phix",
  "racket",
  "rexx",
  "scala",
  "sidef",
  "vbscript",
  "zkl",
]
+++

<p>Chebyshev coefficients are the basis of polynomial approximations of functions.  Write a program to generate Chebyshev coefficients.</p>
<p>Calculate coefficients:  cosine function, 10 coefficients, interval 0 1</p>

## C

C99.

```c
#include <stdio.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

double test_func(double x)
{
	//return sin(cos(x)) * exp(-(x - 5)*(x - 5)/10);
	return cos(x);
}

// map x from range [min, max] to [min_to, max_to]
double map(double x, double min_x, double max_x, double min_to, double max_to)
{
	return (x - min_x)/(max_x - min_x)*(max_to - min_to) + min_to;
}

void cheb_coef(double (*func)(double), int n, double min, double max, double *coef)
{
	memset(coef, 0, sizeof(double) * n);
	for (int i = 0; i < n; i++) {
		double f = func(map(cos(M_PI*(i + .5f)/n), -1, 1, min, max))*2/n;
		for (int j = 0; j < n; j++)
			coef[j] += f*cos(M_PI*j*(i + .5f)/n);
	}
}

// f(x) = sum_{k=0}^{n - 1} c_k T_k(x) - c_0/2
// Note that n >= 2 is assumed; probably should check for that, however silly it is.
double cheb_approx(double x, int n, double min, double max, double *coef)
{
	double a = 1, b = map(x, min, max, -1, 1), c;
	double res = coef[0]/2 + coef[1]*b;

	x = 2*b;
	for (int i = 2; i < n; a = b, b = c, i++)
		// T_{n+1} = 2x T_n - T_{n-1}
		res += coef[i]*(c = x*b - a);

	return res;
}

int main(void)
{
#define N 10
	double c[N], min = 0, max = 1;
	cheb_coef(test_func, N, min, max, c);

	printf("Coefficients:");
	for (int i = 0; i < N; i++)
		printf(" %lg", c[i]);

	puts("\n\nApproximation:\n   x           func(x)     approx      diff");
	for (int i = 0; i <= 20; i++) {
		double x = map(i, 0, 20, min, max);
		double f = test_func(x);
		double approx = cheb_approx(x, N, min, max, c);

		printf("% 10.8lf % 10.8lf % 10.8lf % 4.1le\n",
			x, f, approx, approx - f);
	}

	return 0;
}
```



## C++

Based on the C99 implementation above.  The main improvement is that, because C++ containers handle memory for us, we can use a more functional style.

The two overloads of cheb_coef show a useful idiom for working with C++ templates; the non-template code, which does all the mathematical work, can be placed in a source file so that it is compiled only once (reducing code bloat from repeating substantial blocks of code).  The template function is a minimal wrapper to call the non-template implementation.

The wrapper class ChebyshevApprox_ supports very terse user code.


```CPP

#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include <utility>
#include <vector>

using namespace std;

static const double PI = acos(-1.0);

double affine_remap(const pair<double, double>& from, double x, const pair<double, double>& to)
{
	return to.first + (x - from.first) * (to.second - to.first) / (from.second - from.first);
}

vector<double> cheb_coef(const vector<double>& f_vals)
{
	const int n = f_vals.size();
	const double theta = PI / n;
	vector<double> retval(n, 0.0);
	for (int ii = 0; ii < n; ++ii)
	{
		double f = f_vals[ii] * 2.0 / n;
		const double phi = (ii + 0.5) * theta;
		double c1 = cos(phi), s1 = sin(phi);
		double c = 1.0, s = 0.0;
		for (int j = 0; j < n; j++)
		{
			retval[j] += f * c;
			// update c -> cos(j*phi) for next value of j
			const double cNext = c * c1 - s * s1;
			s = c * s1 + s * c1;
			c = cNext;
		}
	}
	return retval;
}

template<class F_> vector<double> cheb_coef(const F_& func, int n, const pair<double, double>& domain)
{
	auto remap = [&](double x){return affine_remap({ -1.0, 1.0 }, x, domain); };
	const double theta = PI / n;
	vector<double> fVals(n);
	for (int ii = 0; ii < n; ++ii)
		fVals[ii] = func(remap(cos((ii + 0.5) * theta)));
	return cheb_coef(fVals);
}

double cheb_eval(const vector<double>& coef, double x)
{
	double a = 1.0, b = x, c;
	double retval = 0.5 * coef[0] + b * coef[1];
	for (auto pc = coef.begin() + 2; pc != coef.end(); a = b, b = c, ++pc)
	{
		c = 2.0 * b * x - a;
		retval += (*pc) * c;
	}
	return retval;
}
double cheb_eval(const vector<double>& coef, const pair<double, double>& domain, double x)
{
	return cheb_eval(coef, affine_remap(domain, x, { -1.0, 1.0 }));
}

struct ChebyshevApprox_
{
	vector<double> coeffs_;
	pair<double, double> domain_;

	double operator()(double x) const { return cheb_eval(coeffs_, domain_, x); }

	template<class F_> ChebyshevApprox_
		(const F_& func,
		int n,
		const pair<double, double>& domain)
		:
		coeffs_(cheb_coef(func, n, domain)),
		domain_(domain)
	{ }
};


int main(void)
{
	static const int N = 10;
	ChebyshevApprox_ fApprox(cos, N, { 0.0, 1.0 });
	cout << "Coefficients: " << setprecision(14);
	for (const auto& c : fApprox.coeffs_)
		cout << "\t" << c << "\n";

	for (;;)
	{
		cout << "Enter x, or non-numeric value to quit:\n";
		double x;
		if (!(cin >> x))
			return 0;
		cout << "True value: \t" << cos(x) << "\n";
		cout << "Approximate: \t" << fApprox(x) << "\n";
	}
}

```



## D

This imperative code retains some of the style of the original C version.

```d
import std.math: PI, cos;

/// Map x from range [min, max] to [min_to, max_to].
real map(in real x, in real min_x, in real max_x, in real min_to, in real max_to)
pure nothrow @safe @nogc {
	return (x - min_x) / (max_x - min_x) * (max_to - min_to) + min_to;
}


void chebyshevCoef(size_t N)(in real function(in real) pure nothrow @safe @nogc func,
                             in real min, in real max, ref real[N] coef)
pure nothrow @safe @nogc {
    coef[] = 0.0;

    foreach (immutable i; 0 .. N) {
        immutable f = func(map(cos(PI * (i + 0.5f) / N), -1, 1, min, max)) * 2 / N;
        foreach (immutable j, ref cj; coef)
            cj += f * cos(PI * j * (i + 0.5f) / N);
	}
}


/// f(x) = sum_{k=0}^{n - 1} c_k T_k(x) - c_0/2
real chebyshevApprox(size_t N)(in real x, in real min, in real max, in ref real[N] coef)
pure nothrow @safe @nogc if (N >= 2) {
    real a = 1.0L,
         b = map(x, min, max, -1, 1),
         result = coef[0] / 2 + coef[1] * b;

	immutable x2 = 2 * b;
    foreach (immutable ci; coef[2 .. $]) {
		// T_{n+1} = 2x T_n - T_{n-1}
        immutable c = x2 * b - a;
        result += ci * c;
        a = b;
        b = c;
    }

    return result;
}


void main() @safe {
    import std.stdio: writeln, writefln;
    enum uint N = 10;

	real[N] c;
    real min = 0, max = 1;
    static real test(in real x) pure nothrow @safe @nogc { return x.cos; }
	chebyshevCoef(&test, min, max, c);

    writefln("Coefficients:\n%(  %+2.25g\n%)", c);

    enum nX = 20;
	writeln("\nApproximation:\n    x       func(x)        approx      diff");
    foreach (immutable i; 0 .. nX) {
        immutable x = map(i, 0, nX, min, max);
		immutable f = test(x);
		immutable approx = chebyshevApprox(x, min, max, c);

		writefln("%1.3f % 10.10f % 10.10f % 4.2e", x, f, approx, approx - f);
	}
}
```

```txt
Coefficients:
  +1.6471694753903136868
  -0.23229937161517194216
  -0.053715114622047555044
  +0.0024582352669814797779
  +0.00028211905743400579387
  -7.7222291558103533853e-06
  -5.898556452178771968e-07
  +1.1521427332860788728e-08
  +6.5963000382704222411e-10
  -1.0022591914390921452e-11

Approximation:
    x                 func(x)                  approx      diff
0.000  1.00000000000000000000  1.00000000000046961190  4.70e-13
0.050  0.99875026039496624654  0.99875026039487216781 -9.41e-14
0.100  0.99500416527802576609  0.99500416527848803832  4.62e-13
0.150  0.98877107793604228670  0.98877107793599569749 -4.66e-14
0.200  0.98006657784124163110  0.98006657784078136889 -4.60e-13
0.250  0.96891242171064478408  0.96891242171041249593 -2.32e-13
0.300  0.95533648912560601967  0.95533648912586667367  2.61e-13
0.350  0.93937271284737892005  0.93937271284783928305  4.60e-13
0.400  0.92106099400288508277  0.92106099400308274515  1.98e-13
0.450  0.90044710235267692169  0.90044710235242891114 -2.48e-13
0.500  0.87758256189037271615  0.87758256188991362600 -4.59e-13
0.550  0.85252452205950574283  0.85252452205925896211 -2.47e-13
0.600  0.82533561490967829723  0.82533561490987400509  1.96e-13
0.650  0.79608379854905582896  0.79608379854950937939  4.54e-13
0.700  0.76484218728448842626  0.76484218728474395029  2.56e-13
0.750  0.73168886887382088633  0.73168886887359430061 -2.27e-13
0.800  0.69670670934716542091  0.69670670934671868322 -4.47e-13
0.850  0.65998314588498217039  0.65998314588493717370 -4.50e-14
0.900  0.62160996827066445648  0.62160996827110870299  4.44e-13
0.950  0.58168308946388349416  0.58168308946379353278 -9.00e-14
```


The same code, with N = 16:

```txt
Coefficients:
  +1.6471694753903136868
  -0.23229937161517194214
  -0.053715114622047555035
  +0.0024582352669814797982
  +0.00028211905743400571932
  -7.722229155810705751e-06
  -5.898556452177348953e-07
  +1.1521427330794028337e-08
  +6.5963022091481034181e-10
  -1.0016894235462866363e-11
  -4.5865582517937500406e-13
  +5.6974586994888026802e-15
  +2.1752822525027137867e-16
  -2.3140940118987485263e-18
  -1.0333801956502464137e-19
  +2.5410988417629010172e-20

Approximation:
    x                 func(x)                  approx      diff
0.000  1.00000000000000000000  1.00000000000000000030  3.25e-19
0.050  0.99875026039496624654  0.99875026039496624646 -1.08e-19
0.100  0.99500416527802576609  0.99500416527802576557 -5.42e-19
0.150  0.98877107793604228670  0.98877107793604228636 -3.79e-19
0.200  0.98006657784124163110  0.98006657784124163127  1.08e-19
0.250  0.96891242171064478408  0.96891242171064478451  3.79e-19
0.300  0.95533648912560601967  0.95533648912560601967  0.00e+00
0.350  0.93937271284737892005  0.93937271284737891962 -3.79e-19
0.400  0.92106099400288508277  0.92106099400288508260 -2.17e-19
0.450  0.90044710235267692169  0.90044710235267692169  5.42e-20
0.500  0.87758256189037271615  0.87758256189037271632  2.17e-19
0.550  0.85252452205950574283  0.85252452205950574274 -5.42e-20
0.600  0.82533561490967829723  0.82533561490967829697 -2.17e-19
0.650  0.79608379854905582896  0.79608379854905582861 -3.25e-19
0.700  0.76484218728448842626  0.76484218728448842630  5.42e-20
0.750  0.73168886887382088633  0.73168886887382088637  5.42e-20
0.800  0.69670670934716542091  0.69670670934716542087 -5.42e-20
0.850  0.65998314588498217039  0.65998314588498217022 -1.63e-19
0.900  0.62160996827066445648  0.62160996827066445674  2.71e-19
0.950  0.58168308946388349416  0.58168308946388349403 -1.63e-19
```



## Go

Wikipedia gives a formula for coefficients in a section [https://en.wikipedia.org/wiki/Chebyshev_polynomials#Example_1 "Example 1"].  Read past the bit about the inner product to where it gives the technique based on the discrete orthogonality condition.  The N of the WP formulas is the parameter nNodes in the code here.  It is not necessarily the same as n, the number of polynomial coefficients, the parameter nCoeff here.

The evaluation method is the [https://en.wikipedia.org/wiki/Clenshaw_algorithm Clenshaw algorithm].

Two variances here from the WP presentation and most mathematical presentations follow other examples on this page and so keep output directly comparable.  One variance is that the Kronecker delta factor is dropped, which has the effect of doubling the first coefficient.  This simplifies both coefficient generation and polynomial evaluation.  A further variance is that there is no scaling for the range of function values.  The result is that coefficients are not necessarily bounded by 1 (2 for the first coefficient) but by the maximum function value over the argument range from min to max (or twice that for the first coefficient.)

```go
package main

import (
    "fmt"
    "math"
)

type cheb struct {
    c        []float64
    min, max float64
}

func main() {
    fn := math.Cos
    c := newCheb(0, 1, 10, 10, fn)
    fmt.Println("coefficients:")
    for _, c := range c.c {
        fmt.Printf("% .15f\n", c)
    }
    fmt.Println("\nx     computed    approximated    computed-approx")
    const n = 10
    for i := 0.; i <= n; i++ {
        x := (c.min*(n-i) + c.max*i) / n
        computed := fn(x)
        approx := c.eval(x)
        fmt.Printf("%.1f %12.8f  %12.8f   % .3e\n",
            x, computed, approx, computed-approx)
    }
}

func newCheb(min, max float64, nCoeff, nNodes int, fn func(float64) float64) *cheb {
    c := &cheb{
        c:   make([]float64, nCoeff),
        min: min,
        max: max,
    }
    f := make([]float64, nNodes)
    p := make([]float64, nNodes)
    z := .5 * (max + min)
    r := .5 * (max - min)
    for k := 0; k < nNodes; k++ {
        p[k] = math.Pi * (float64(k) + .5) / float64(nNodes)
        f[k] = fn(z + math.Cos(p[k])*r)
    }
    n2 := 2 / float64(nNodes)
    for j := 0; j < nCoeff; j++ {
        sum := 0.
        for k := 0; k < nNodes; k++ {
            sum += f[k] * math.Cos(float64(j)*p[k])
        }
        c.c[j] = sum * n2
    }
    return c
}

func (c *cheb) eval(x float64) float64 {
    x1 := (2*x - c.min - c.max) / (c.max - c.min)
    x2 := 2 * x1
    var s, t float64
    for j := len(c.c) - 1; j >= 1; j-- {
        t, s = x2*t-s+c.c[j], t
    }
    return x1*t - s + .5*c.c[0]
}
```

```txt

coefficients:
 1.647169475390314
-0.232299371615172
-0.053715114622048
 0.002458235266982
 0.000282119057434
-0.000007722229156
-0.000000589855645
 0.000000011521427
 0.000000000659630
-0.000000000010022

x     computed    approximated    computed-approx
0.0   1.00000000    1.00000000   -4.685e-13
0.1   0.99500417    0.99500417   -4.620e-13
0.2   0.98006658    0.98006658    4.601e-13
0.3   0.95533649    0.95533649   -2.607e-13
0.4   0.92106099    0.92106099   -1.972e-13
0.5   0.87758256    0.87758256    4.587e-13
0.6   0.82533561    0.82533561   -1.965e-13
0.7   0.76484219    0.76484219   -2.552e-13
0.8   0.69670671    0.69670671    4.470e-13
0.9   0.62160997    0.62160997   -4.449e-13
1.0   0.54030231    0.54030231   -4.476e-13

```



## J

From 'J for C Programmers: Calculating Chebyshev Coefficients [[http://www.jsoftware.com/learning/a_first_look_at_j_programs.htm#_Toc191734318]]

```J

chebft =: adverb define
:
f =. u 0.5 * (+/y) - (-/y) * 2 o. o. (0.5 + i. x) % x
   (2 % x) * +/ f * 2 o. o. (0.5 + i. x) *"0 1 (i. x) % x
)

```

Calculate coefficients:

```J

      10 (2&o.) chebft 0 1
1.64717 _0.232299 _0.0537151 0.00245824 0.000282119 _7.72223e_6 _5.89856e_7 1.15214e_8 6.59629e_10 _1.00227e_11

```



## Java

Partial translation of [[Chebyshev_coefficients#C|C]] via [[Chebyshev_coefficients#D|D]]
```java
import static java.lang.Math.*;
import java.util.function.Function;

public class ChebyshevCoefficients {

    static double map(double x, double min_x, double max_x, double min_to,
            double max_to) {
        return (x - min_x) / (max_x - min_x) * (max_to - min_to) + min_to;
    }

    static void chebyshevCoef(Function<Double, Double> func, double min,
            double max, double[] coef) {

        int N = coef.length;

        for (int i = 0; i < N; i++) {

            double m = map(cos(PI * (i + 0.5f) / N), -1, 1, min, max);
            double f = func.apply(m) * 2 / N;

            for (int j = 0; j < N; j++) {
                coef[j] += f * cos(PI * j * (i + 0.5f) / N);
            }
        }
    }

    public static void main(String[] args) {
        final int N = 10;
        double[] c = new double[N];
        double min = 0, max = 1;
        chebyshevCoef(x -> cos(x), min, max, c);

        System.out.println("Coefficients:");
        for (double d : c)
            System.out.println(d);
    }
}
```



```txt
Coefficients:
1.6471694753903139
-0.23229937161517178
-0.0537151146220477
0.002458235266981773
2.8211905743405485E-4
-7.722229156320592E-6
-5.898556456745974E-7
1.1521427770166959E-8
6.59630183807991E-10
-1.0021913854352249E-11
```



## Julia

```julia
mutable struct Cheb
    c::Vector{Float64}
    min::Float64
    max::Float64
end

function Cheb(min::Float64, max::Float64, ncoeff::Int, nnodes::Int, fn::Function)::Cheb
    c = Cheb(Vector{Float64}(ncoeff), min, max)
    f = Vector{Float64}(nnodes)
    p = Vector{Float64}(nnodes)
    z = (max + min) / 2
    r = (max - min) / 2
    for k in 0:nnodes-1
        p[k+1] = π * (k + 0.5) / nnodes
        f[k+1] = fn(z + cos(p[k+1]) * r)
    end
    n2 = 2 / nnodes
    for j in 0:nnodes-1
        s = sum(fk * cos(j * pk) for (fk, pk) in zip(f, p))
        c.c[j+1] = s * n2
    end
    return c
end

function evaluate(c::Cheb, x::Float64)::Float64
    x1 = (2x - c.max - c.min) / (c.max - c.min)
    x2 = 2x1
    t = s = 0
    for j in length(c.c):-1:2
        t, s = x2 * t - s + c.c[j], t
    end
    return x1 * t - s + c.c[1] / 2
end

fn = cos
c  = Cheb(0.0, 1.0, 10, 10, fn)
# coefs
println("Coefficients:")
for x in c.c
    @printf("% .15f\n", x)
end
# values
println("\nx     computed    approximated    computed-approx")
const n = 10
for i in 0.0:n
    x = (c.min * (n - i) + c.max * i) / n
    computed = fn(x)
    approx   = evaluate(c, x)
    @printf("%.1f %12.8f  %12.8f   % .3e\n", x, computed, approx, computed - approx)
end
```


```txt
Coefficients:
 1.647169475390314
-0.232299371615172
-0.053715114622048
 0.002458235266981
 0.000282119057434
-0.000007722229156
-0.000000589855645
 0.000000011521427
 0.000000000659630
-0.000000000010022

x     computed    approximated    computed-approx
0.0   1.00000000    1.00000000   -4.685e-13
0.1   0.99500417    0.99500417   -4.620e-13
0.2   0.98006658    0.98006658    4.601e-13
0.3   0.95533649    0.95533649   -2.605e-13
0.4   0.92106099    0.92106099   -1.970e-13
0.5   0.87758256    0.87758256    4.586e-13
0.6   0.82533561    0.82533561   -1.967e-13
0.7   0.76484219    0.76484219   -2.551e-13
0.8   0.69670671    0.69670671    4.470e-13
0.9   0.62160997    0.62160997   -4.449e-13
1.0   0.54030231    0.54030231   -4.476e-13
```



## Kotlin

```scala
// version 1.1.2

typealias DFunc = (Double) -> Double

fun mapRange(x: Double, min: Double, max: Double, minTo: Double, maxTo:Double): Double {
    return (x - min) / (max - min) * (maxTo - minTo) + minTo
}

fun chebCoeffs(func: DFunc, n: Int, min: Double, max: Double): DoubleArray {
    val coeffs = DoubleArray(n)
    for (i in 0 until n) {
        val f = func(mapRange(Math.cos(Math.PI * (i + 0.5) / n), -1.0, 1.0, min, max)) * 2.0 / n
        for (j in 0 until n) coeffs[j] += f * Math.cos(Math.PI * j * (i + 0.5) / n)
    }
    return coeffs
}

fun chebApprox(x: Double, n: Int, min: Double, max: Double, coeffs: DoubleArray): Double {
    require(n >= 2 && coeffs.size >= 2)
    var a = 1.0
    var b = mapRange(x, min, max, -1.0, 1.0)
    var res = coeffs[0] / 2.0 + coeffs[1] * b
    val xx = 2 * b
    var i = 2
    while (i < n) {
        val c = xx * b - a
        res += coeffs[i] * c
        a = b
        b = c
        i++
    }
    return res
}

fun main(args: Array<String>) {
    val n = 10
    val min = 0.0
    val max = 1.0
    val coeffs = chebCoeffs(Math::cos, n, min, max)
    println("Coefficients:")
    for (coeff in coeffs) println("%+1.15g".format(coeff))
    println("\nApproximations:\n  x      func(x)     approx       diff")
    for (i in 0..20) {
        val x = mapRange(i.toDouble(), 0.0, 20.0, min, max)
        val f = Math.cos(x)
        val approx = chebApprox(x, n, min, max, coeffs)
        System.out.printf("%1.3f  %1.8f  %1.8f  % 4.1e\n", x, f, approx, approx - f)
    }
}
```


```txt

Coefficients:
+1.64716947539031
-0.232299371615172
-0.0537151146220477
+0.00245823526698177
+0.000282119057434055
-7.72222915632059e-06
-5.89855645674597e-07
+1.15214277701670e-08
+6.59630183807991e-10
-1.00219138543522e-11

Approximations:
  x      func(x)     approx       diff
0.000  1.00000000  1.00000000   4.7e-13
0.050  0.99875026  0.99875026  -9.4e-14
0.100  0.99500417  0.99500417   4.6e-13
0.150  0.98877108  0.98877108  -4.7e-14
0.200  0.98006658  0.98006658  -4.6e-13
0.250  0.96891242  0.96891242  -2.3e-13
0.300  0.95533649  0.95533649   2.6e-13
0.350  0.93937271  0.93937271   4.6e-13
0.400  0.92106099  0.92106099   2.0e-13
0.450  0.90044710  0.90044710  -2.5e-13
0.500  0.87758256  0.87758256  -4.6e-13
0.550  0.85252452  0.85252452  -2.5e-13
0.600  0.82533561  0.82533561   2.0e-13
0.650  0.79608380  0.79608380   4.5e-13
0.700  0.76484219  0.76484219   2.5e-13
0.750  0.73168887  0.73168887  -2.3e-13
0.800  0.69670671  0.69670671  -4.5e-13
0.850  0.65998315  0.65998315  -4.4e-14
0.900  0.62160997  0.62160997   4.5e-13
0.950  0.58168309  0.58168309  -9.0e-14
1.000  0.54030231  0.54030231   4.5e-13

```



## Microsoft Small Basic

```smallbasic
' N Chebyshev coefficients for the range 0 to 1 - 18/07/2018
  pi=Math.pi
  a=0
  b=1
  n=10
  For i=0 To n-1
    coef[i]=Math.cos(Math.cos(pi/n*(i+1/2))*(b-a)/2+(b+a)/2)
  EndFor
  For i=0 To n-1
    w=0
    For j=0 To n-1
      w=w+coef[j]*Math.cos(pi/n*i*(j+1/2))
    EndFor
    cheby[i]=w*2/n
    t=" "
    If cheby[i]<=0 Then
      t=""
    EndIf
    TextWindow.WriteLine(i+" : "+t+cheby[i])
  EndFor
```

```txt

0 :  1,6471694753903136
1 : -0,2322993716151700684187787635
2 : -0,0537151146220494010749946688
3 :  0,0024582352669837594966069584
4 :  0,0002821190574317389206759282
5 : -0,0000077222291539069653168878
6 : -0,0000005898556481086082412444
7 :  0,0000000115214300591398939205
8 :  0,0000000006596278553822696656
9 : -0,0000000000100189955816952521

```




## Perl

```perl
use constant PI =
 3.141592653589793;

sub chebft {
  my($func, $a, $b, $n) = @_;
  my($bma, $bpa) = ( 0.5*($b-$a), 0.5*($b+$a) );

  my @pin = map { ($_ + 0.5) * (PI/$n) } 0..$n-1;
  my @f = map { $func->( cos($_) * $bma + $bpa ) } @pin;
  my @c = (0) x $n;
  for my $j (0 .. $n-1) {
    $c[$j] += $f[$_] * cos($j * $pin[$_])   for 0..$n-1;
    $c[$j] *= (2.0/$n);
  }
  @c;
}

print "$_\n" for chebft(sub{cos($_[0])}, 0, 1, 10);
```


```txt
1.64716947539031
-0.232299371615172
-0.0537151146220477
0.00245823526698163
0.000282119057433938
-7.72222915566001e-06
-5.89855645105608e-07
1.15214274787334e-08
6.59629917354465e-10
-1.00219943455215e-11
```



## Perl 6

```perl6
sub chebft ( Code $func, Real $a, Real $b, Int $n ) {

    my $bma = 0.5 * ( $b - $a );
    my $bpa = 0.5 * ( $b + $a );

    my @pi_n = ( (^$n).list »+» 0.5 ) »*» ( pi / $n );
    my @f    = ( @pi_n».cos »*» $bma »+» $bpa )».$func;
    my @sums = map { [+] @f »*« ( @pi_n »*» $_ )».cos }, ^$n;

    return @sums »*» ( 2 / $n );
}

say .fmt('%+13.7e') for chebft &cos, 0, 1, 10;
```


```txt

+1.6471695e+00
-2.3229937e-01
-5.3715115e-02
+2.4582353e-03
+2.8211906e-04
-7.7222292e-06
-5.8985565e-07
+1.1521427e-08
+6.5962992e-10
-1.0021994e-11

```



## Phix

```Phix
function Cheb(atom cmin, cmax, integer ncoeff, nnodes)
    sequence c = repeat(0,ncoeff),
             f = repeat(0,nnodes),
             p = repeat(0,nnodes)
    atom z = (cmax + cmin) / 2,
         r = (cmax - cmin) / 2
    for k=1 to nnodes do
        p[k] = PI * ((k-1) + 0.5) / nnodes
        f[k] = cos(z + cos(p[k]) * r)
    end for
    atom n2 = 2 / nnodes
    for j=1 to nnodes do
        atom s := 0
        for k=1 to nnodes do
            s += f[k] * cos((j-1)*p[k])
        end for
        c[j] = s * n2
    end for
    return c
end function

function evaluate(sequence c, atom cmin, cmax, x)
    atom x1 = (2*x - cmax - cmin) / (cmax - cmin),
         x2 = 2*x1,
         t = 0, s = 0
    for j=length(c) to 2 by -1 do
        {t, s} = {x2 * t - s + c[j], t}
    end for
    return x1 * t - s + c[1] / 2
end function

atom cmin = 0.0, cmax = 1.0
sequence c  = Cheb(cmin, cmax, 10, 10)
printf(1, "Coefficients:\n")
pp(c,{pp_Nest,1,pp_FltFmt,"%18.15f"})
printf(1,"\nx     computed    approximated    computed-approx\n")
constant n = 10
for i=0 to 10 do
    atom x = (cmin * (n - i) + cmax * i) / n,
         calc = cos(x),
         est = evaluate(c, cmin, cmax, x)
    printf(1,"%.1f %12.8f  %12.8f   %10.3e\n", {x, calc, est, calc-est})
end for
```

```txt

Coefficients:
{ 1.647169475390314,
 -0.232299371615172,
 -0.053715114622048,
  0.002458235266981,
  0.000282119057434,
 -0.000007722229156,
 -0.000000589855645,
  0.000000011521427,
  0.000000000659630,
 -0.000000000010022}

x     computed    approximated    computed-approx
0.0   1.00000000    1.00000000   -4.686e-13
0.1   0.99500417    0.99500417   -4.620e-13
0.2   0.98006658    0.98006658    4.600e-13
0.3   0.95533649    0.95533649   -2.604e-13
0.4   0.92106099    0.92106099   -1.970e-13
0.5   0.87758256    0.87758256    4.587e-13
0.6   0.82533561    0.82533561   -1.968e-13
0.7   0.76484219    0.76484219   -2.551e-13
0.8   0.69670671    0.69670671    4.470e-13
0.9   0.62160997    0.62160997   -4.450e-13
1.0   0.54030231    0.54030231   -4.477e-13

```



## Racket


```racket
#lang typed/racket
(: chebft (Real Real Nonnegative-Integer (Real -> Real) -> (Vectorof Real)))
(define (chebft a b n func)
  (define b-a/2 (/ (- b a) 2))
  (define b+a/2 (/ (+ b a) 2))
  (define pi/n (/ pi n))
  (define fac (/ 2 n))

  (define f (for/vector : (Vectorof Real)
              ((k : Nonnegative-Integer (in-range n)))
              (define y (cos (* pi/n (+ k 1/2))))
              (func (+ (* y b-a/2) b+a/2))))

  (for/vector : (Vectorof Real)
    ((j : Nonnegative-Integer (in-range n)))
    (define s (for/sum : Real
                ((k : Nonnegative-Integer (in-range n)))
                (* (vector-ref f k)
                   (cos (* pi/n j (+ k 1/2))))))
    (* fac s)))

(module+ test
  (chebft 0 1 10 cos))
;; Tim Brown 2015
```


```txt
'#(1.6471694753903137
   -0.2322993716151719
   -0.05371511462204768
   0.0024582352669816343
   0.0002821190574339161
   -7.722229155637806e-006
   -5.898556451056081e-007
   1.1521427500937876e-008
   6.596299173544651e-010
   -1.0022016549982027e-011)
```



## REXX

This REXX program is a translation of the   '''C'''   program plus added optimizations.
     Pafnuty Lvovich Chebysheff:   Chebyshev       [English  transliteration]
                                   Chebysheff      [   "           "        ]
                                   Chebyshov       [   "           "        ]
                                   Tchebychev      [French         "        ]
                                   Tchebysheff     [   "           "        ]
                                   Tschebyschow    [German         "        ]
                                   Tschebyschev    [   "           "        ]
                                   Tschebyschef    [   "           "        ]
                                   Tschebyscheff   [   "           "        ]


The numeric precision is dependent on the number of decimal digits specified in the value of '''pi'''.

```rexx
/*REXX program calculates  N  Chebyshev coefficients for the range  0 ──► 1  (inclusive)*/
numeric digits length( pi() )  - 1               /*DIGITS default is nine,  but use 71. */
parse arg a b N .                                /*obtain optional arguments from the CL*/
if a==''  |  a==","  then a= 0                   /*A  not specified?   Then use default.*/
if b==''  |  b==","  then b= 1                   /*B   "      "          "   "     "    */
if N==''  |  N==","  then N=10                   /*N   "      "          "   "     "    */
fac=2 / N;                pin=pi / N             /*calculate a couple handy─dandy values*/
Dma= (b-a) / 2                                   /*calculate one─half of the difference.*/
Dpa= (b+a) / 2                                   /*    "        "      "  "     sum.    */
                     do k=0  for N
                     f.k=cos( cos( pin * (k + .5) ) * Dma    +    Dpa)
                     end   /*k*/

     do j=0  for N;  z=pin * j                   /*The   LEFT('', ···)  ──────►──────┐  */
     $=0                                         /*clause is used to align           │  */
                     do m=0  for N               /*the non─negative values with      ↓  */
                     $=$ + f.m * cos(z*(m + .5)) /*the     negative values.          │  */
                     end   /*m*/                 /*                     ┌─────◄──────┘  */
     cheby.j=fac * $                             /*                     ↓               */
     say right(j, length(N) +3)   " Chebyshev coefficient  is:"   left('', cheby.j >= 0),
         format(cheby.j, , 30)                   /*only show 30 decimal digits of coeff.*/
     end  /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos: procedure; parse arg x; numeric digits digits()+9; x=r2r(x); a=abs(x); numeric fuzz 5
                if a=pi   then return -1;  if a=pi*.5 | a=pi*2  then return 0;   pit= pi/3
                if a=pit  then return .5;  if a=pit*2 then return -.5;   q=x*x;  z=1;  _=1
                      do ?=2  by 2  until p=z;  p=z;  _=-_*q/(?*(?-1));  z=z+_;  end /*?*/
     return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:  pi=3.1415926535897932384626433832795028841971693993751058209749445923078164;return pi
r2r: return  arg(1)  //  (pi() * 2)              /*normalize radians ───► a unit circle.*/
```

```txt

    0  Chebyshev coefficient  is:   1.647169475390313686961473816798
    1  Chebyshev coefficient  is:  -0.232299371615171942121038341178
    2  Chebyshev coefficient  is:  -0.053715114622047555071596203933
    3  Chebyshev coefficient  is:   0.002458235266981479866768882753
    4  Chebyshev coefficient  is:   0.000282119057434005702410217295
    5  Chebyshev coefficient  is:  -0.000007722229155810577892832847
    6  Chebyshev coefficient  is:  -5.898556452177103343296676960522E-7
    7  Chebyshev coefficient  is:   1.152142733310315857327524390711E-8
    8  Chebyshev coefficient  is:   6.596300035120132380676859918562E-10
    9  Chebyshev coefficient  is:  -1.002259170944625675156620531665E-11

```

    0  Chebyshev coefficient  is:   1.647169475390313686961473816799
    1  Chebyshev coefficient  is:  -0.232299371615171942121038341150
    2  Chebyshev coefficient  is:  -0.053715114622047555071596207909
    3  Chebyshev coefficient  is:   0.002458235266981479866768726383
    4  Chebyshev coefficient  is:   0.000282119057434005702429677244
    5  Chebyshev coefficient  is:  -0.000007722229155810577212604038
    6  Chebyshev coefficient  is:  -5.898556452177850238987693546709E-7
    7  Chebyshev coefficient  is:   1.152142733081886533841160480101E-8
    8  Chebyshev coefficient  is:   6.596302208686010678189261798322E-10
    9  Chebyshev coefficient  is:  -1.001689435637395512060196156843E-11
   10  Chebyshev coefficient  is:  -4.586557765969596848147502951921E-13
   11  Chebyshev coefficient  is:   5.697353072301630964243748212466E-15
   12  Chebyshev coefficient  is:   2.173565878297512401879760404343E-16
   13  Chebyshev coefficient  is:  -2.284293234863639106096540267786E-18
   14  Chebyshev coefficient  is:  -7.468956910165861862760811388638E-20
   15  Chebyshev coefficient  is:   6.802288097339388765485830636223E-22
   16  Chebyshev coefficient  is:   1.945994872442404773393679283660E-23
   17  Chebyshev coefficient  is:  -1.563704507245591241161562138364E-25
   18  Chebyshev coefficient  is:  -3.976201538410589537318561880598E-27
   19  Chebyshev coefficient  is:   2.859065292763079576513213370136E-29

## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/DqRNe2A/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/M5Ye6h8ZRkmTCNzexUh3uw Scastie (remote JVM)].

```Scala
import scala.math.{Pi, cos}

object ChebyshevCoefficients extends App {
  final val N = 10
  final val (min, max) = (0, 1)
  val c = new Array[Double](N)

  def chebyshevCoef(func: Double => Double,
                    min: Double,
                    max: Double,
                    coef: Array[Double]): Unit =
    for (i <- coef.indices) {
      def map(x: Double,
              min_x: Double,
              max_x: Double,
              min_to: Double,
              max_to: Double): Double =
        (x - min_x) / (max_x - min_x) * (max_to - min_to) + min_to

      val m = map(cos(Pi * (i + 0.5f) / N), -1, 1, min, max)

      def f = func.apply(m) * 2 / N

      for (j <- coef.indices) coef(j) += f * cos(Pi * j * (i + 0.5f) / N)
    }

  chebyshevCoef((x: Double) => cos(x), min, max, c)
  println("Coefficients:")
  c.foreach(d => println(f"$d%23.16e"))

}
```



## Sidef

```ruby
func chebft (callback, a, b, n) {

    var bma = (0.5 * b-a);
    var bpa = (0.5 * b+a);

    var pi_n = ((0..(n-1) »+» 0.5) »*» (Number.pi / n));
    var f = (pi_n »cos»() »*» bma »+» bpa «call« callback);
    var sums = (0..(n-1) «run« {|i| f »*« ((pi_n »*» i) »cos»()) «+» });

    sums »*» (2/n);
}

chebft(func(v){v.cos}, 0, 1, 10).each { |v|
    say ("%+.10e" % v);
}
```


```txt

+1.6471694754e+00
-2.3229937162e-01
-5.3715114622e-02
+2.4582352670e-03
+2.8211905743e-04
-7.7222291558e-06
-5.8985564522e-07
+1.1521427333e-08
+6.5963000351e-10
-1.0022591709e-11

```



## VBScript

To run in console mode with cscript.

```vb
' N Chebyshev coefficients for the range 0 to 1
  Dim coef(10),cheby(10)
  pi=4*Atn(1)
  a=0: b=1: n=10
  For i=0 To n-1
    coef(i)=Cos(Cos(pi/n*(i+1/2))*(b-a)/2+(b+a)/2)
  Next
  For i=0 To n-1
    w=0
    For j=0 To n-1
      w=w+coef(j)*Cos(pi/n*i*(j+1/2))
    Next
    cheby(i)=w*2/n
    If cheby(i)<=0 Then t="" Else t=" "
    WScript.StdOut.WriteLine i&" : "&t&cheby(i)
  Next
```

```txt

0 :  1,64716947539031
1 : -0,232299371615172
2 : -5,37151146220477E-02
3 :  2,45823526698163E-03
4 :  2,82119057433916E-04
5 : -7,72222915563781E-06
6 : -5,89855645105608E-07
7 :  1,15214275009379E-08
8 :  6,59629917354465E-10
9 : -1,0022016549982E-11

```



## zkl

```zkl
var [const] PI=(1.0).pi;
fcn chebft(a,b,n,func){
   bma,bpa,fac := 0.5*(b - a), 0.5*(b + a), 2.0/n;
   f:=n.pump(List,'wrap(k){ (PI*(0.5 + k)/n).cos():func(_*bma + bpa) });
   n.pump(List,'wrap(j){
      fac*n.reduce('wrap(sum,k){ sum + f[k]*(PI*j*(0.5 + k)/n).cos() },0.0);
   })
}
chebft(0.0,1.0,10,fcn(x){ x.cos() }).enumerate().concat("\n").println();
```

```txt

L(0,1.64717)
L(1,-0.232299)
L(2,-0.0537151)
L(3,0.00245824)
L(4,0.000282119)
L(5,-7.72223e-06)
L(6,-5.89856e-07)
L(7,1.15214e-08)
L(8,6.5963e-10)
L(9,-1.00219e-11)

```

