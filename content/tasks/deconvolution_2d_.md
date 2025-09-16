+++
title = "Deconvolution/2D+"
description = ""
date = 2019-06-08T02:32:49Z
aliases = []
[extra]
id = 6110
[taxonomies]
categories = ["task", "Mathematical operations"]
tags = []
languages = [
  "c",
  "d",
  "go",
  "j",
  "julia",
  "perl",
  "perl_6",
  "phix",
  "python",
  "tcl",
  "ursala",
]
+++

## Task

This task is a straightforward generalization of [[Deconvolution/1D]] to higher dimensions. For example, the one dimensional case would be applicable to audio signals, whereas two dimensions would pertain to images. Define the discrete convolution in <math>\mathit d</math> dimensions of two functions

:<math>H,F:\mathbb{Z}^d\rightarrow\mathbb{R}</math>

taking <math>\mathit d</math>-tuples of integers to real numbers as the function

:<math>G:\mathbb{Z}^d\rightarrow\mathbb{R}</math>

also taking <math>\mathit d</math>-tuples of integers to reals and satisfying

:<math>G(n_0, \dots, n_{d-1})=\sum_{m_0=-\infty}^{\infty}\dots\sum_{m_{d-1}=-\infty}^{\infty}F(m_0, \dots, m_{d-1})H(n_0-m_0, \dots, n_{d-1}-m_{d-1})</math>

for all <math>\mathit d</math>-tuples of integers <math>(n_0, \dots, n_{d-1})\in\mathbb{Z}^d</math>. Assume
<math>\mathit F</math> and <math>\mathit H</math> (and therefore <math>\mathit G</math>) are non-zero over only a finite domain bounded by the origin, hence possible to represent as finite multi-dimensional arrays or nested lists <math>\mathit f</math>, <math>\mathit h</math>, and <math>\mathit g</math>.

For this task, implement a function (or method, procedure, subroutine, etc.) <code>deconv</code> to perform ''deconvolution'' (i.e., the ''inverse'' of convolution) by solving for <math>\mathit{h}</math> given <math>\mathit{f}</math> and <math>\mathit{g}</math>. (See [[Deconvolution/1D]] for details.)
* The function should work for <math>\mathit{g}</math> of arbitrary length in each dimension (i.e., not hard coded or constant) and <math>\mathit{f}</math> of any length up to that of <math>\mathit{g}</math> in the corresponding dimension.
* The <code>deconv</code> function will need to be parameterized by the dimension <math>\mathit d</math> unless the dimension can be inferred from the data structures representing <math>\mathit g</math> and <math>\mathit f</math>.
* There may be more equations than unknowns. If convenient, use a function from a [http://www.netlib.org/lapack/lug/node27.html library] that finds the best fitting solution to an overdetermined system of linear equations (as in the [[Multiple regression]] task).  Otherwise, prune the set of equations as needed and solve as in the [[Reduced row echelon form]] task.
* Debug your solution using [http://rosettacode.org/mw/index.php?title=Deconvolution/2D%2B/Test_data&action=raw this test data], of which a portion is shown below. Be sure to verify both that the deconvolution of <math>\mathit g</math> with <math>\mathit f</math> is <math>\mathit h</math> and that the deconvolution of <math>\mathit g</math> with <math>\mathit h</math> is <math>\mathit f</math>. Display the results in a human readable form for the three dimensional case ''only''.

dimension 1:

```txt

h: [-8, 2, -9, -2, 9, -8, -2]
f: [ 6, -9, -7, -5]
g: [-48, 84, -16, 95, 125, -70, 7, 29, 54, 10]

```

dimension 2:

```txt

h: [
      [-8, 1, -7, -2, -9, 4],
      [4, 5, -5, 2, 7, -1],
      [-6, -3, -3, -6, 9, 5]]
f: [
      [-5, 2, -2, -6, -7],
      [9, 7, -6, 5, -7],
      [1, -1, 9, 2, -7],
      [5, 9, -9, 2, -5],
      [-8, 5, -2, 8, 5]]
g: [
      [40, -21, 53, 42, 105, 1, 87, 60, 39, -28],
      [-92, -64, 19, -167, -71, -47, 128, -109, 40, -21],
      [58, 85, -93, 37, 101, -14, 5, 37, -76, -56],
      [-90, -135, 60, -125, 68, 53, 223, 4, -36, -48],
      [78, 16, 7, -199, 156, -162, 29, 28, -103, -10],
      [-62, -89, 69, -61, 66, 193, -61, 71, -8, -30],
      [48, -6, 21, -9, -150, -22, -56, 32, 85, 25]]

```

dimension 3:

```txt

h: [
      [[-6, -8, -5, 9], [-7, 9, -6, -8], [2, -7, 9, 8]],
      [[7, 4, 4, -6], [9, 9, 4, -4], [-3, 7, -2, -3]]]
f: [
      [[-9, 5, -8], [3, 5, 1]],
      [[-1, -7, 2], [-5, -6, 6]],
      [[8, 5, 8],[-2, -6, -4]]]
g: [
      [
         [54, 42, 53, -42, 85, -72],
         [45, -170, 94, -36, 48, 73],
         [-39, 65, -112, -16, -78, -72],
         [6, -11, -6, 62, 49, 8]],
      [
         [-57, 49, -23, 52, -135, 66],
         [-23, 127, -58, -5, -118, 64],
         [87, -16, 121, 23, -41, -12],
         [-19, 29, 35, -148, -11, 45]],
      [
         [-55, -147, -146, -31, 55, 60],
         [-88, -45, -28, 46, -26, -144],
         [-12, -107, -34, 150, 249, 66],
         [11, -15, -34, 27, -78, -50]],
      [
         [56, 67, 108, 4, 2, -48],
         [58, 67, 89, 32, 32, -8],
         [-42, -31, -103, -30, -23, -8],
         [6, 4, -26, -10, 26, 12]]]

```




## C

Very tedious code: unpacks 2D or 3D matrix into a vector with padding, do 1D FFT, then pack result back into matrix.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>

double PI;
typedef double complex cplx;

void _fft(cplx buf[], cplx out[], int n, int step)
{
	if (step < n) {
		_fft(out, buf, n, step * 2);
		_fft(out + step, buf + step, n, step * 2);

		for (int i = 0; i < n; i += 2 * step) {
			cplx t = cexp(-I * PI * i / n) * out[i + step];
			buf[i / 2]     = out[i] + t;
			buf[(i + n)/2] = out[i] - t;
		}
	}
}

void fft(cplx buf[], int n)
{
	cplx out[n];
	for (int i = 0; i < n; i++) out[i] = buf[i];
	_fft(buf, out, n, 1);
}

/* pad array length to power of two */
cplx *pad_two(double g[], int len, int *ns)
{
	int n = 1;
	if (*ns) n = *ns;
	else while (n < len) n *= 2;

	cplx *buf = calloc(sizeof(cplx), n);
	for (int i = 0; i < len; i++) buf[i] = g[i];
	*ns = n;
	return buf;
}

void deconv(double g[], int lg, double f[], int lf, double out[], int row_len) {
	int ns = 0;
	cplx *g2 = pad_two(g, lg, &ns);
	cplx *f2 = pad_two(f, lf, &ns);

	fft(g2, ns);
	fft(f2, ns);

	cplx h[ns];
	for (int i = 0; i < ns; i++) h[i] = g2[i] / f2[i];
	fft(h, ns);

	for (int i = 0; i < ns; i++) {
		if (cabs(creal(h[i])) < 1e-10)
			h[i] = 0;
	}

	for (int i = 0; i > lf - lg - row_len; i--)
		out[-i] = h[(i + ns) % ns]/32;
	free(g2);
	free(f2);
}

double* unpack2(void *m, int rows, int len, int to_len)
{
	double *buf = calloc(sizeof(double), rows * to_len);
	for (int i = 0; i < rows; i++)
		for (int j = 0; j < len; j++)
			buf[i * to_len + j] = ((double(*)[len])m)[i][j];
	return buf;
}

void pack2(double * buf, int rows, int from_len, int to_len, void *out)
{
	for (int i = 0; i < rows; i++)
		for (int j = 0; j < to_len; j++)
			((double(*)[to_len])out)[i][j] = buf[i * from_len + j] / 4;
}

void deconv2(void *g, int row_g, int col_g, void *f, int row_f, int col_f, void *out) {
	double *g2 = unpack2(g, row_g, col_g, col_g);
	double *f2 = unpack2(f, row_f, col_f, col_g);

	double ff[(row_g - row_f + 1) * col_g];
	deconv(g2, row_g * col_g, f2, row_f * col_g, ff, col_g);
	pack2(ff, row_g - row_f + 1, col_g, col_g - col_f + 1, out);

	free(g2);
	free(f2);
}

double* unpack3(void *m, int x, int y, int z, int to_y, int to_z)
{
	double *buf = calloc(sizeof(double), x * to_y * to_z);
	for (int i = 0; i < x; i++)
		for (int j = 0; j < y; j++) {
			for (int k = 0; k < z; k++)
				buf[(i * to_y + j) * to_z + k] =
					((double(*)[y][z])m)[i][j][k];
		}
	return buf;
}

void pack3(double * buf, int x, int y, int z, int to_y, int to_z, void *out)
{
	for (int i = 0; i < x; i++)
		for (int j = 0; j < to_y; j++)
			for (int k = 0; k < to_z; k++)
				((double(*)[to_y][to_z])out)[i][j][k] =
					buf[(i * y + j) * z + k] / 4;
}

void deconv3(void *g, int gx, int gy, int gz, void *f, int fx, int fy, int fz, void *out) {
	double *g2 = unpack3(g, gx, gy, gz, gy, gz);
	double *f2 = unpack3(f, fx, fy, fz, gy, gz);

	double ff[(gx - fx + 1) * gy * gz];
	deconv(g2, gx * gy * gz, f2, fx * gy * gz, ff, gy * gz);
	pack3(ff, gx - fx + 1, gy, gz, gy - fy + 1, gz - fz + 1, out);

	free(g2);
	free(f2);
}

int main()
{
	PI = atan2(1,1) * 4;
	double h[2][3][4] = {
		{{-6, -8, -5,  9}, {-7, 9, -6, -8}, { 2, -7,  9,  8}},
		{{ 7,  4,  4, -6}, { 9, 9,  4, -4}, {-3,  7, -2, -3}}
	};
	int hx = 2, hy = 3, hz = 4;
	double f[3][2][3] = {	{{-9,  5, -8}, { 3,  5,  1}},
				{{-1, -7,  2}, {-5, -6,  6}},
				{{ 8,  5,  8}, {-2, -6, -4}} };
	int fx = 3, fy = 2, fz = 3;
	double g[4][4][6] = {
		{	{ 54,  42,  53, -42,  85, -72}, { 45,-170,  94, -36,  48,  73},
			{-39,  65,-112, -16, -78, -72}, {  6, -11,  -6,  62,  49,   8} },
		{ 	{-57,  49, -23,   52, -135,  66},{-23, 127, -58,   -5, -118,  64},
			{ 87, -16,  121,  23,  -41, -12},{-19,  29,   35,-148,  -11,  45} },
		{	{-55, -147, -146, -31,  55,  60},{-88,  -45,  -28,  46, -26,-144},
			{-12, -107,  -34, 150, 249,  66},{ 11,  -15,  -34,  27, -78, -50} },
		{	{ 56,  67, 108,   4,  2,-48},{ 58,  67,  89,  32, 32, -8},
			{-42, -31,-103, -30,-23, -8},{  6,   4, -26, -10, 26, 12}
		}
	};
	int gx = 4, gy = 4, gz = 6;

	double h2[gx - fx + 1][gy - fy + 1][gz - fz + 1];
	deconv3(g, gx, gy, gz, f, fx, fy, fz, h2);
	printf("deconv3(g, f):\n");
	for (int i = 0; i < gx - fx + 1; i++) {
		for (int j = 0; j < gy - fy + 1; j++) {
			for (int k = 0; k < gz - fz + 1; k++)
				printf("%g ", h2[i][j][k]);
			printf("\n");
		}
		if (i < gx - fx) printf("\n");
	}

	double f2[gx - hx + 1][gy - hy + 1][gz - hz + 1];
	deconv3(g, gx, gy, gz, h, hx, hy, hz, f2);
	printf("\ndeconv3(g, h):\n");
	for (int i = 0; i < gx - hx + 1; i++) {
		for (int j = 0; j < gy - hy + 1; j++) {
			for (int k = 0; k < gz - hz + 1; k++)
				printf("%g ", f2[i][j][k]);
			printf("\n");
		}
		if (i < gx - hx) printf("\n");
	}
}

/* two-D case; since task doesn't require showing it, it's commented out */
/*
int main()
{
	PI = atan2(1,1) * 4;
	double h[][6] = { 	{-8, 1, -7, -2, -9, 4},
				{4, 5, -5, 2, 7, -1},
				{-6, -3, -3, -6, 9, 5} };
	int hr = 3, hc = 6;

	double f[][5] = {	{-5, 2, -2, -6, -7},
				{9, 7, -6, 5, -7},
				{1, -1, 9, 2, -7},
				{5, 9, -9, 2, -5},
				{-8, 5, -2, 8, 5} };
	int fr = 5, fc = 5;
	double g[][10] = {
			{40, -21, 53, 42, 105, 1, 87, 60, 39, -28},
			{-92, -64, 19, -167, -71, -47, 128, -109, 40, -21},
			{58, 85, -93, 37, 101, -14, 5, 37, -76, -56},
			{-90, -135, 60, -125, 68, 53, 223, 4, -36, -48},
			{78, 16, 7, -199, 156, -162, 29, 28, -103, -10},
			{-62, -89, 69, -61, 66, 193, -61, 71, -8, -30},
			{48, -6, 21, -9, -150, -22, -56, 32, 85, 25}	};
	int gr = 7, gc = 10;

	double h2[gr - fr + 1][gc - fc + 1];
	deconv2(g, gr, gc, f, fr, fc, h2);
	for (int i = 0; i < gr - fr + 1; i++) {
		for (int j = 0; j < gc - fc + 1; j++)
			printf(" %g", h2[i][j]);
		printf("\n");
	}

	double f2[gr - hr + 1][gc - hc + 1];
	deconv2(g, gr, gc, h, hr, hc, f2);
	for (int i = 0; i < gr - hr + 1; i++) {
		for (int j = 0; j < gc - hc + 1; j++)
			printf(" %g", f2[i][j]);
		printf("\n");
	}
}*/
```
Output<lang>deconv3(g, f):
-6 -8 -5 9
-7 9 -6 -8
2 -7 9 8

7 4 4 -6
9 9 4 -4
-3 7 -2 -3

deconv3(g, h):
-9 5 -8
3 5 1

-1 -7 2
-5 -6 6

8 5 8
-2 -6 -4
```



## D


```d
import std.stdio, std.conv, std.algorithm, std.numeric, std.range;

class M(T) {
    private size_t[] dim;
    private size_t[] subsize;
    private T[] d;

    this(size_t[] dimension...) pure nothrow {
        setDimension(dimension);
        d[] = 0; // init each  entry to zero;
    }

    M!T dup() {
        auto m = new M!T(dim);
        return m.set1DArray(d);
    }

    M!T setDimension(size_t[] dimension ...) pure nothrow {
        foreach (const e; dimension)
            assert(e > 0, "no zero dimension");
        dim = dimension.dup;
        subsize = dim.dup;
        foreach (immutable i; 0 .. dim.length)
            subsize[i] = reduce!q{a * b}(1, dim[i + 1 .. $]);
        immutable dlength = dim[0] * subsize[0];
        if (d.length != dlength)
            d = new T[dlength];
        return this;
    }

    M!T set1DArray(in T[] t ...) pure nothrow @nogc {
        auto minLen = min(t.length, d.length);
        d[] = 0;
        d[0 .. minLen] = t[0 .. minLen];
        return this;
    }

    size_t[] seq2idx(in size_t seq) const pure nothrow {
        size_t acc = seq, tmp;
        size_t[] idx;
        foreach (immutable e; subsize) {
            idx ~= tmp = acc / e;
            acc = acc - tmp * e; // same as % (mod) e.
        }
        return idx;
    }

    size_t size() const pure nothrow @nogc @property {
        return d.length;
    }

    size_t rank() const pure nothrow @nogc @property {
        return dim.length;
    }

    size_t[] shape() const pure nothrow @property { return dim.dup; }

    T[] raw() const pure nothrow @property { return d.dup; }

    bool checkBound(size_t[] idx ...) const pure nothrow @nogc {
        if (idx.length > dim.length)
            return false;
        foreach (immutable i, immutable dm; idx)
            if (dm >= dim[i])
                return false;
        return true;
    }

    T opIndex(size_t[] idx ...) const pure nothrow @nogc {
        assert(checkBound(idx), "OOPS");
        return d[dotProduct(idx, subsize)];
    }

    T opIndexAssign(T v, size_t[] idx ...) pure nothrow @nogc {
        assert(checkBound(idx), "OOPS");
        d[dotProduct(idx, subsize)] = v;
        return v;
    }

    override bool opEquals(Object o) const pure {
        const rhs = to!(M!T)(o);
        return dim == rhs.dim && d == rhs.d;
    }

    int opApply(int delegate(ref size_t[]) dg) const {
        size_t[] yieldIdx;
        foreach (immutable i; 0 .. d.length) {
            yieldIdx = seq2idx(i);
            if (dg(yieldIdx))
                break;
        }
        return 0;
    }

    int opApply(int delegate(ref size_t[], ref T) dg) {
        size_t idx1d = 0;
        foreach (idx; this) {
            if (dg(idx, d[idx1d++]))
                break;
        }
        return 0;
    }

    // _this_ is h, rhs is f, output g.
    M!T convolute(M!T rhs) const pure nothrow {
        auto dm = dim.dup;
        dm[] += rhs.dim[] - 1;
        M!T m = new M!T(dm); // dm will be reused as m's idx.
        auto bound = m.size;
        foreach (immutable i; 0 .. d.length) {
            auto thisIdx = seq2idx(i);
            foreach (immutable j; 0 .. rhs.d.length) {
                dm[] = thisIdx[] + rhs.seq2idx(j)[];
                immutable midx1d = dotProduct(dm, m.subsize);
                if (midx1d < bound)
                    m.d[midx1d] += d[i] * rhs.d[j];
                else
                    break; // Bound reach, OK to break.
            }
        }
        return m;
    }

    // _this_ is g, rhs is f, output is h.
    M!T deconvolute(M!T rhs) const pure nothrow {
        auto dm = dim.dup;
        foreach (i, e; dm)
            assert(e + 1 > rhs.dim[i],
                   "deconv : dimensions is zero or negative");
        dm[] -= (rhs.dim[] - 1);
        auto m = new M!T(dm); // dm will be reused as rhs' idx.

        foreach (immutable i; 0 .. m.size) {
            auto idx = m.seq2idx(i);
            m.d[i] = this[idx];
            foreach (immutable j; 0 .. i) {
                immutable jdx = m.seq2idx(j);
                dm[] = idx[] - jdx[];
                if (rhs.checkBound(dm))
                    m.d[i] -=  m.d[j] * rhs[dm];
            }
            m.d[i] /= rhs.d[0];
        }
        return m;
    }

    override string toString() const pure { return d.text; }
}

auto fold(T)(T[] arr, ref size_t[] d) pure {
    if (d.length == 0)
        d ~= arr.length;

    static if (is(T U : U[])) { // Is arr an array of arrays?
        assert(arr.length > 0, "no empty dimension");
        d ~= arr[0].length;
        foreach (e; arr)
            assert(e.length == arr[0].length, "Not rectangular");
        return fold(arr.reduce!q{a ~ b}, d);
    } else {
        assert(arr.length == d.reduce!q{a * b}, "Not same size");
        return arr;
    }
}

auto arr2M(T)(T a) pure {
    size_t[] dm;
    auto d = fold(a, dm);
    alias E = ElementType!(typeof(d));
    auto m = new M!E(dm);
    m.set1DArray(d);
    return m;
}

void main() {
    alias Mi = M!int;
    auto hh = [[[-6, -8, -5, 9], [-7, 9, -6, -8], [2, -7, 9, 8]],
               [[7, 4, 4, -6], [9, 9, 4, -4], [-3, 7, -2, -3]]];
    auto ff = [[[-9, 5, -8], [3, 5, 1]],[[-1, -7, 2], [-5, -6, 6]],
               [[8, 5, 8],[-2, -6, -4]]];
    auto h = arr2M(hh);
    auto f = arr2M(ff);

    const g = h.convolute(f);

    writeln("g == f convolute h ? ", g == f.convolute(h));
    writeln("h == g deconv f    ? ", h == g.deconvolute(f));
    writeln("f == g deconv h    ? ", f == g.deconvolute(h));
    writeln("         f = ", f);
    writeln("g deconv h = ", g.deconvolute(h));
}
```

''todo(may be not :): pretty print & convert to normal D array''
```txt
g == f convolute h ? true
h == g deconv f    ? true
f == g deconv h    ? true
         f = [-9, 5, -8, 3, 5, 1, -1, -7, 2, -5, -6, 6, 8, 5, 8, -2, -6, -4]
g deconv h = [-9, 5, -8, 3, 5, 1, -1, -7, 2, -5, -6, 6, 8, 5, 8, -2, -6, -4]
```



## Go

```go
package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

func fft(buf []complex128, n int) {
    out := make([]complex128, n)
    copy(out, buf)
    fft2(buf, out, n, 1)
}

func fft2(buf, out []complex128, n, step int) {
    if step < n {
        fft2(out, buf, n, step*2)
        fft2(out[step:], buf[step:], n, step*2)
        for j := 0; j < n; j += 2 * step {
            fj, fn := float64(j), float64(n)
            t := cmplx.Exp(-1i*complex(math.Pi, 0)*complex(fj, 0)/complex(fn, 0)) * out[j+step]
            buf[j/2] = out[j] + t
            buf[(j+n)/2] = out[j] - t
        }
    }
}

/* pad slice length to power of two */
func padTwo(g []float64, le int, ns *int) []complex128 {
    n := 1
    if *ns != 0 {
        n = *ns
    } else {
        for n < le {
            n *= 2
        }
    }
    buf := make([]complex128, n)
    for i := 0; i < le; i++ {
        buf[i] = complex(g[i], 0)
    }
    *ns = n
    return buf
}

func deconv(g []float64, lg int, f []float64, lf int, out []float64, rowLe int) {
    ns := 0
    g2 := padTwo(g, lg, &ns)
    f2 := padTwo(f, lf, &ns)
    fft(g2, ns)
    fft(f2, ns)
    h := make([]complex128, ns)
    for i := 0; i < ns; i++ {
        h[i] = g2[i] / f2[i]
    }
    fft(h, ns)
    for i := 0; i < ns; i++ {
        if math.Abs(real(h[i])) < 1e-10 {
            h[i] = 0
        }
    }
    for i := 0; i > lf-lg-rowLe; i-- {
        out[-i] = real(h[(i+ns)%ns] / 32)
    }
}

func unpack2(m [][]float64, rows, le, toLe int) []float64 {
    buf := make([]float64, rows*toLe)
    for i := 0; i < rows; i++ {
        for j := 0; j < le; j++ {
            buf[i*toLe+j] = m[i][j]
        }
    }
    return buf
}

func pack2(buf []float64, rows, fromLe, toLe int, out [][]float64) {
    for i := 0; i < rows; i++ {
        for j := 0; j < toLe; j++ {
            out[i][j] = buf[i*fromLe+j] / 4
        }
    }
}

func deconv2(g [][]float64, rowG, colG int, f [][]float64, rowF, colF int, out [][]float64) {
    g2 := unpack2(g, rowG, colG, colG)
    f2 := unpack2(f, rowF, colF, colG)
    ff := make([]float64, (rowG-rowF+1)*colG)
    deconv(g2, rowG*colG, f2, rowF*colG, ff, colG)
    pack2(ff, rowG-rowF+1, colG, colG-colF+1, out)
}

func unpack3(m [][][]float64, x, y, z, toY, toZ int) []float64 {
    buf := make([]float64, x*toY*toZ)
    for i := 0; i < x; i++ {
        for j := 0; j < y; j++ {
            for k := 0; k < z; k++ {
                buf[(i*toY+j)*toZ+k] = m[i][j][k]
            }
        }
    }
    return buf
}

func pack3(buf []float64, x, y, z, toY, toZ int, out [][][]float64) {
    for i := 0; i < x; i++ {
        for j := 0; j < toY; j++ {
            for k := 0; k < toZ; k++ {
                out[i][j][k] = buf[(i*y+j)*z+k] / 4
            }
        }
    }
}

func deconv3(g [][][]float64, gx, gy, gz int, f [][][]float64, fx, fy, fz int, out [][][]float64) {
    g2 := unpack3(g, gx, gy, gz, gy, gz)
    f2 := unpack3(f, fx, fy, fz, gy, gz)
    ff := make([]float64, (gx-fx+1)*gy*gz)
    deconv(g2, gx*gy*gz, f2, fx*gy*gz, ff, gy*gz)
    pack3(ff, gx-fx+1, gy, gz, gy-fy+1, gz-fz+1, out)
}

func main() {
    f := [][][]float64{
        {{-9, 5, -8}, {3, 5, 1}},
        {{-1, -7, 2}, {-5, -6, 6}},
        {{8, 5, 8}, {-2, -6, -4}},
    }
    fx, fy, fz := len(f), len(f[0]), len(f[0][0])

    g := [][][]float64{
        {{54, 42, 53, -42, 85, -72}, {45, -170, 94, -36, 48, 73},
            {-39, 65, -112, -16, -78, -72}, {6, -11, -6, 62, 49, 8}},
        {{-57, 49, -23, 52, -135, 66}, {-23, 127, -58, -5, -118, 64},
            {87, -16, 121, 23, -41, -12}, {-19, 29, 35, -148, -11, 45}},
        {{-55, -147, -146, -31, 55, 60}, {-88, -45, -28, 46, -26, -144},
            {-12, -107, -34, 150, 249, 66}, {11, -15, -34, 27, -78, -50}},
        {{56, 67, 108, 4, 2, -48}, {58, 67, 89, 32, 32, -8},
            {-42, -31, -103, -30, -23, -8}, {6, 4, -26, -10, 26, 12},
        },
    }
    gx, gy, gz := len(g), len(g[0]), len(g[0][0])

    h := [][][]float64{
        {{-6, -8, -5, 9}, {-7, 9, -6, -8}, {2, -7, 9, 8}},
        {{7, 4, 4, -6}, {9, 9, 4, -4}, {-3, 7, -2, -3}},
    }
    hx, hy, hz := gx-fx+1, gy-fy+1, gz-fz+1

    h2 := make([][][]float64, hx)
    for i := 0; i < hx; i++ {
        h2[i] = make([][]float64, hy)
        for j := 0; j < hy; j++ {
            h2[i][j] = make([]float64, hz)
        }
    }
    deconv3(g, gx, gy, gz, f, fx, fy, fz, h2)
    fmt.Println("deconv3(g, f):\n")
    for i := 0; i < hx; i++ {
        for j := 0; j < hy; j++ {
            for k := 0; k < hz; k++ {
                fmt.Printf("% .10g  ", h2[i][j][k])
            }
            fmt.Println()
        }
        if i < hx-1 {
            fmt.Println()
        }
    }

    kx, ky, kz := gx-hx+1, gy-hy+1, gz-hz+1
    f2 := make([][][]float64, kx)
    for i := 0; i < kx; i++ {
        f2[i] = make([][]float64, ky)
        for j := 0; j < ky; j++ {
            f2[i][j] = make([]float64, kz)
        }
    }
    deconv3(g, gx, gy, gz, h, hx, hy, hz, f2)
    fmt.Println("\ndeconv(g, h):\n")
    for i := 0; i < kx; i++ {
        for j := 0; j < ky; j++ {
            for k := 0; k < kz; k++ {
                fmt.Printf("% .10g  ", f2[i][j][k])
            }
            fmt.Println()
        }
        if i < kx-1 {
            fmt.Println()
        }
    }
}
```


```txt

deconv3(g, f):

-6  -8  -5   9
-7   9  -6  -8
 2  -7   9   8

 7   4   4  -6
 9   9   4  -4
-3   7  -2  -3

deconv(g, h):

-9   5  -8
 3   5   1

-1  -7   2
-5  -6   6

 8   5   8
-2  -6  -4

```



## J

Actually it is a matter of setting up the linear equations and then solving them.

'''Implementation''':
```j
deconv3 =: 4 : 0
 sz  =. x >:@-&$ y                                      NB. shape of z
 poi =.  ,<"1 ($y) ,"0/&(,@i.) sz                       NB. pair of indexes
 t=. /: sc=: , <@(+"1)/&(#: ,@i.)/ ($y),:sz             NB. order of ,y
 T0=. (<"0,x) ,:~ (]/:"1 {.)&.> (<, y) ({:@] ,: ({"1~ {.))&.>  sc <@|:@:>/.&(t&{) poi   NB. set of boxed equations
 T1=. (,x),.~(<0 #~ */sz) (({:@])`({.@])`[})&> {.T0     NB. set of linear equations
 sz $ 1e_8 round ({:"1 %. }:"1) T1
)
round=: [ * <.@%~
```


'''Data''':
```j
h1=: _8 2 _9 _2 9 _8 _2
f1=: 6 _9 _7 _5
g1=: _48 84 _16 95 125 _70 7 29 54 10

h2=: ".;._2]0 :0
  _8 1 _7 _2 _9 4
  4 5 _5 2 7 _1
  _6 _3 _3 _6 9 5
)

f2=: ".;._2]0 :0
  _5 2 _2 _6 _7
  9 7 _6 5 _7
  1 _1 9 2 _7
  5 9 _9 2 _5
  _8 5 _2 8 5
)

g2=: ".;._2]0 :0
  40 _21 53 42 105 1 87 60 39 _28
  _92 _64 19 _167 _71 _47 128 _109 40 _21
  58 85 _93 37 101 _14 5 37 _76 _56
  _90 _135 60 _125 68 53 223 4 _36 _48
  78 16 7 _199 156 _162 29 28 _103 _10
  _62 _89 69 _61 66 193 _61 71 _8 _30
  48 _6 21 _9 _150 _22 _56 32 85 25
)

h3=: ".;._1;._2]0 :0
/  _6 _8 _5 9/ _7 9 _6 _8/ 2 _7 9 8
/  7 4 4 _6/ 9 9 4 _4/ _3 7 _2 _3
)

f3=: ".;._1;._2]0 :0
/  _9 5 _8/ 3 5 1
/  _1 _7 2/ _5 _6 6
/  8 5 8/_2 _6 _4
)

g3=: ".;._2;._1]0 :0
/  54 42 53 _42 85 _72
   45 _170 94 _36 48 73
   _39 65 _112 _16 _78 _72
   6 _11 _6 62 49 8
/  _57 49 _23 52 _135 66
   _23 127 _58 _5 _118 64
   87 _16 121 23 _41 _12
   _19 29 35 _148 _11 45
/  _55 _147 _146 _31 55 60
   _88 _45 _28 46 _26 _144
   _12 _107 _34 150 249 66
   11 _15 _34 27 _78 _50
/  56 67 108 4 2 _48
   58 67 89 32 32 _8
   _42 _31 _103 _30 _23 _8
   6 4 _26 _10 26 12
)
```


'''Tests''':
```j
   h1 -: g1 deconv3 f1
1
   h2 -: g2 deconv3 f2
1
   h3 -: g3 deconv3 f3                      NB. -: checks for matching structure and data
1
```




## Julia

Julia has a deconv() function that works on Julia's builtin multidimensional arrays, but not on the nested type 2D and 3D arrays used in the task. So, the solution function, deconvn(), sets up repackaging for 1D fft. The actual solving work is done on one line of ifft/fft, and the rest of the code is merely to repackage the nested arrays.

```julia
using FFTW, DSP

const h1 = [-8, 2, -9, -2, 9, -8, -2]
const f1 = [ 6, -9, -7, -5]
const g1 = [-48, 84, -16, 95, 125, -70, 7, 29, 54, 10]

const h2nested = [
      [-8, 1, -7, -2, -9, 4],
      [4, 5, -5, 2, 7, -1],
      [-6, -3, -3, -6, 9, 5]]
const f2nested = [
      [-5, 2, -2, -6, -7],
      [9, 7, -6, 5, -7],
      [1, -1, 9, 2, -7],
      [5, 9, -9, 2, -5],
      [-8, 5, -2, 8, 5]]
const g2nested = [
      [40, -21, 53, 42, 105, 1, 87, 60, 39, -28],
      [-92, -64, 19, -167, -71, -47, 128, -109, 40, -21],
      [58, 85, -93, 37, 101, -14, 5, 37, -76, -56],
      [-90, -135, 60, -125, 68, 53, 223, 4, -36, -48],
      [78, 16, 7, -199, 156, -162, 29, 28, -103, -10],
      [-62, -89, 69, -61, 66, 193, -61, 71, -8, -30],
      [48, -6, 21, -9, -150, -22, -56, 32, 85, 25]]

const h3nested = [
      [[-6, -8, -5, 9], [-7, 9, -6, -8], [2, -7, 9, 8]],
      [[7, 4, 4, -6], [9, 9, 4, -4], [-3, 7, -2, -3]]]
const f3nested = [
      [[-9, 5, -8], [3, 5, 1]],
      [[-1, -7, 2], [-5, -6, 6]],
      [[8, 5, 8],[-2, -6, -4]]]
const g3nested = [
      [  [54, 42, 53, -42, 85, -72],
         [45, -170, 94, -36, 48, 73],
         [-39, 65, -112, -16, -78, -72],
         [6, -11, -6, 62, 49, 8]],
      [  [-57, 49, -23, 52, -135, 66],
         [-23, 127, -58, -5, -118, 64],
         [87, -16, 121, 23, -41, -12],
         [-19, 29, 35, -148, -11, 45]],
      [  [-55, -147, -146, -31, 55, 60],
         [-88, -45, -28, 46, -26, -144],
         [-12, -107, -34, 150, 249, 66],
         [11, -15, -34, 27, -78, -50]],
      [  [56, 67, 108, 4, 2, -48],
         [58, 67, 89, 32, 32, -8],
         [-42, -31, -103, -30, -23, -8],
         [6, 4, -26, -10, 26, 12]]]

function flatnested2d(a, siz)
    ret = zeros(Int, prod(siz))
    for i in 1:length(a), j in 1:length(a[1])
        ret[siz[2] * (i - 1) + j] = a[i][j]
    end
    Float64.(ret)
end

function flatnested3d(a, siz)
    ret = zeros(Int, prod(siz))
    for i in 1:length(a), j in 1:length(a[1]), k in 1:length(a[1][1])
        ret[siz[2] * siz[3] * (i - 1) + siz[3] * (j - 1) + k] = a[i][j][k]
    end
    Float64.(ret)
end

topow2(siz) = map(x -> nextpow(2, x), siz)
deconv1d(f1, g1) = Int.(round.(deconv(Float64.(g1), Float64.(f1))))

function deconv2d(f2, g2, xd2)
    siz = topow2([length(g2), length(g2[1])])
    h2 = Int.(round.(real.(ifft(fft(flatnested2d(g2, siz)) ./ fft(flatnested2d(f2, siz))))))
    [[h2[siz[2] * (i - 1) + j] for j in 1:xd2[2]] for i in 1:xd2[1]]
end

function deconv3d(f3, g3, xd3)
    siz = topow2([length(g3), length(g3[1]), length(g3[1][1])])
    h3 = Int.(round.(real.(ifft(fft(flatnested3d(g3, siz)) ./ fft(flatnested3d(f3, siz))))))
    [[[h3[siz[2] * siz[3] *(i - 1) + siz[3] * (j - 1) + k] for k in 1:xd3[3]]
        for j in 1:xd3[2]] for i in 1:xd3[1]]
end

deconvn(f, g, tup=()) = length(tup) < 2 ? deconv1d(f, g) :
                       length(tup) == 2 ? deconv2d(f, g, tup) :
                       length(tup) == 3 ? deconv3d(f, g, tup) :
                       println("Array nesting > 3D not supported")

deconvn(f1, g1)  # 1D
deconvn(f2nested, g2nested, (length(h2nested), length(h2nested[1]))) # 2D
println(deconvn(f3nested, g3nested,
    (length(h3nested), length(h3nested[1]), length(h3nested[1][1])))) # 3D

```
```txt

Array{Array{Int64,1},1}[[[-6, -8, -5, 9], [-7, 9, -6, -8], [2, -7, 9, 8]], [[7, 4, 4, -6], [9, 9, 4, -4], [-3, 7, -2, -3]]]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Round[ListDeconvolve[{6, -9, -7, -5}, {-48, 84, -16, 95, 125, -70, 7, 29, 54, 10}, Method -> "Wiener"]]

Round[ListDeconvolve[{{-5, 2, -2, -6, -7}, {9, 7, -6, 5, -7}, {1, -1, 9, 2, -7}, {5, 9, -9, 2, -5}, {-8, 5, -2, 8, 5}},
{{40, -21, 53, 42, 105, 1, 87, 60, 39, -28}, {-92, -64, 19, -167, -71, -47, 128, -109, 40, -21},
{58, 85, -93, 37, 101, -14, 5, 37, -76, -56}, {-90, -135, 60, -125, 68, 53, 223, 4, -36, -48},
{78, 16, 7, -199, 156, -162, 29, 28, -103, -10}, {-62, -89, 69, -61, 66, 193, -61, 71, -8, -30},
{48, -6, 21, -9, -150, -22, -56, 32, 85, 25}}, Method -> "Wiener"]]

Round[ListDeconvolve [{{{-9, 5, -8}, {3, 5, 1}}, {{-1, -7, 2}, {-5, -6, 6}}, {{8, 5, 8}, {-2, -6, -4}}},
{{{54, 42, 53, -42,   85, -72}, {45, -170, 94, -36, 48, 73}, {-39, 65, -112, -16, -78, -72},
{6, -11, -6, 62, 49, 8}}, {{-57, 49, -23, 52, -135, 66}, {-23, 127, -58, -5, -118, 64}, {87, -16, 121, 23, -41, -12},
{-19, 29, 35, -148, -11, 45}}, {{-55, -147, -146, -31, 55, 60}, {-88, -45, -28, 46, -26, -144},
{-12, -107, -34, 150, 249, 66}, {11, -15, -34, 27, -78, -50}}, {{56, 67, 108, 4, 2, -48}, {58, 67, 89, 32, 32, -8},
{-42, -31, -103, -30, -23, -8}, {6, 4, -26, -10, 26, 12}}}, Method -> "Wiener"]]
```


The built-in ListDeconvolve function pads output to the same dimensions as the original data...


```txt
{-8, 2, -9, -2, 9, -8, -2, 0, 0, 0}

{{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, -8, 1, -7, -2, -9, 4, 0, 0},
{0, 0, 4, 5, -5, 2, 7, -1, 0, 0}, {0, 0, -6, -3, -3, -6, 9, 5, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}}

{{{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}}, {{0, 0, 0, 0, 0, 0},
{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}}, {{-6, -8, -5, 9, 0, 0}, {-7, 9, -6, -8, 0, 0},
{2, -7, 9, 8, 0, 0}, {0, 0, 0, 0, 0, 0}}, {{7, 4, 4, -6, 0, 0}, {9, 9, 4, -4, 0, 0}, {-3, 7, -2, -3, 0, 0},
{0, 0, 0, 0, 0, 0}}}
```



## Perl

```perl
use feature 'say';
use ntheory qw/forsetproduct/;

# Deconvolution of N dimensional matrices
sub deconvolve_N {
    our @g; local *g = shift;
    our @f; local *f = shift;
    my @df = shape(@f);
    my @dg = shape(@g);
    my @hsize;
    push @hsize, $dg[$_] - $df[$_] + 1 for 0..$#df;
    my @toSolve = map { [row(\@g, \@f, \@hsize, $_)] } coords(shape(@g));
    rref( \@toSolve );

    my @h;
    my $n = 0;
    for (coords(@hsize)) {
        my($k,$j,$i) = split ' ', $_;
        $h[$i][$j][$k] = $toSolve[$n++][-1];
    }
    @h;
}

sub row {
    our @g;      local *g      = shift;
    our @f;      local *f      = shift;
    our @hsize;  local *hsize  = shift;
    my @gc = reverse split ' ', shift;

    my @row;
    my @fdim = shape(@f);
    for (coords(@hsize)) {
        my @hc = reverse split ' ', $_;
        my @fc;
        for my $i (0..$#hc) {
            my $window = $gc[$i] - $hc[$i];
            push(@fc, $window), next if 0 <= $window && $window < $fdim[$i];
        }
        push @row, $#fc == $#hc ? $f [$fc[0]] [$fc[1]] [$fc[2]] : 0;
    }
    push @row, $g [$gc[0]] [$gc[1]] [$gc[2]];
    return @row;
}

sub rref {
  our @m; local *m = shift;
  @m or return;
  my ($lead, $rows, $cols) = (0, scalar(@m), scalar(@{$m[0]}));

  foreach my $r (0 .. $rows - 1) {
     $lead < $cols or return;
      my $i = $r;

      until ($m[$i][$lead])
         {++$i == $rows or next;
          $i = $r;
          ++$lead == $cols and return;}

      @m[$i, $r] = @m[$r, $i];
      my $lv = $m[$r][$lead];
      $_ /= $lv foreach @{ $m[$r] };

      my @mr = @{ $m[$r] };
      foreach my $i (0 .. $rows - 1)
         {$i == $r and next;
          ($lv, my $n) = ($m[$i][$lead], -1);
          $_ -= $lv * $mr[++$n] foreach @{ $m[$i] };}

      ++$lead;}
}

# Constructs an AoA of coordinates to all elements of N dimensional array
sub coords {
    my(@dimensions) = reverse @_;
    my(@ranges,@coords);
    push @ranges, [0..$_-1] for @dimensions;
    forsetproduct { push @coords, "@_" } @ranges;
    @coords;
}

sub shape {
    my(@dim);
    push @dim, scalar @_;
    push @dim, shape(@{$_[0]}) if 'ARRAY' eq ref $_[0];
    @dim;
}

# Pretty printer for N dimensional arrays
# Assumes if first element in level is an array, then all are
sub pretty_print {
    my($i, @a) = @_;
    if ('ARRAY' eq ref $a[0]) {
        say ' 'x$i, '[';
        pretty_print($i+2, @$_) for @a;
        say ' 'x$i, ']', $i ? ',' : '';
    } else {
        say ' 'x$i, '[', sprintf("@{['%5s'x@a]}",@a), ']', $i ? ',' : '';
    }
}

my @f = (
  [
    [ -9,  5, -8 ],
    [  3,  5,  1 ],
  ],
  [
    [ -1, -7,  2 ],
    [ -5, -6,  6 ],
  ],
  [
    [  8,  5,  8 ],
    [ -2, -6, -4 ],
  ]
);

my @g = (
  [
    [  54,  42,  53, -42,  85, -72 ],
    [  45,-170,  94, -36,  48,  73 ],
    [ -39,  65,-112, -16, -78, -72 ],
    [   6, -11,  -6,  62,  49,   8 ],
  ],
  [
    [ -57,  49, -23,  52,-135,  66 ],
    [ -23, 127, -58,  -5,-118,  64 ],
    [  87, -16, 121,  23, -41, -12 ],
    [ -19,  29,  35,-148, -11,  45 ],
  ],
  [
    [ -55,-147,-146, -31,  55,  60 ],
    [ -88, -45, -28,  46, -26,-144 ],
    [ -12,-107, -34, 150, 249,  66 ],
    [  11, -15, -34,  27, -78, -50 ],
  ],
  [
    [  56,  67, 108,   4,   2, -48 ],
    [  58,  67,  89,  32,  32,  -8 ],
    [ -42, -31,-103, -30, -23,  -8 ],
    [   6,   4, -26, -10,  26,  12 ],
  ]
);

my @h  = deconvolve_N( \@g, \@f );
my @ff = deconvolve_N( \@g, \@h );

my $d = scalar shape(@g);
print "${d}D arrays:\n";
print "h =\n";
pretty_print(0,@h);
print "\nff =\n";
pretty_print(0,@ff);
```

```txt
3D arrays:
h =
[
  [
    [   -6   -8   -5    9],
    [   -7    9   -6   -8],
    [    2   -7    9    8],
  ],
  [
    [    7    4    4   -6],
    [    9    9    4   -4],
    [   -3    7   -2   -3],
  ],
]

ff =
[
  [
    [   -9    5   -8],
    [    3    5    1],
  ],
  [
    [   -1   -7    2],
    [   -5   -6    6],
  ],
  [
    [    8    5    8],
    [   -2   -6   -4],
  ],
]
```



## Perl 6

Works with Rakudo 2018.03.

Translation of Tcl.

```perl6
# Deconvolution of N dimensional matrices.
sub deconvolve-N ( @g, @f ) {
    my @hsize = @g.shape »-« @f.shape »+» 1;

    my @toSolve = coords(@g.shape).map:
      { [row(@g, @f, $^coords, @hsize)] };

    my @solved = rref( @toSolve );

    my @h;
    for flat coords(@hsize) Z @solved[*;*-1] -> $_, $v {
        @h.AT-POS(|$_) = $v;
    }
    return @h;
}

# Construct a row for each value in @g to be sent to the simultaneous equation solver
sub row ( @g, @f, @gcoord, $hsize ) {
    my @row;
    @gcoord = @gcoord[(^@f.shape)]; # clip extraneous values
    for coords( $hsize ) -> @hc {
        my @fcoord;
        for ^@hc -> $i {
            my $window = @gcoord[$i] - @hc[$i];
            @fcoord.push($window) and next if 0 <= $window < @f.shape[$i];
            last;
        }
        @row.push: @fcoord == @hc ?? @f.AT-POS(|@fcoord) !! 0;
    }
    @row.push: @g.AT-POS(|@gcoord);
    return @row;
}

# Constructs an AoA of coordinates to all elements of N dimensional array
sub coords ( @dim ) {
    @[reverse $_ for [X] ([^$_] for reverse @dim)];
}

# Reduced Row Echelon Form simultaneous equation solver
# Can handle over-specified systems (N unknowns in N + M equations)
sub rref (@m) {
    return unless @m;
    my ($lead, $rows, $cols) = 0, +@m, +@m[0];

    # Trim off over specified rows if they exist, for efficiency
    if $rows >= $cols {
        @m = trim_system(@m);
        $rows = +@m;
    }

    for ^$rows -> $r {
        $lead < $cols or return @m;
        my $i = $r;
        until @m[$i;$lead] {
            ++$i == $rows or next;
            $i = $r;
            ++$lead == $cols and return @m;
        }
        @m[$i, $r] = @m[$r, $i] if $r != $i;
        my $lv = @m[$r;$lead];
        @m[$r] »/=» $lv;
        for ^$rows -> $n {
            next if $n == $r;
            @m[$n] »-=» @m[$r] »*» (@m[$n;$lead] // 0);
        }
        ++$lead;
    }
    return @m;

    # Reduce a system of equations to N equations with N unknowns
    sub trim_system ($m) {
        my ($vars, @t) = +$m[0]-1;
        for ^$vars -> $lead {
            for ^$m -> $row {
                @t.push: | $m.splice( $row, 1 ) and last if $m[$row;$lead];
            }
        }
        while (+@t < $vars) and +$m { @t.push: $m.splice(0, 1) };
        return @t;
    }
}

# Pretty printer for N dimensional arrays
# Assumes if first element in level is an array, then all are
sub pretty_print ( @array, $indent = 0 ) {
    if @array[0] ~~ Array {
        say ' ' x $indent,"[";
        pretty_print( $_, $indent + 2 ) for @array;
        say ' ' x $indent, "]{$indent??','!!''}";
    } else {
        say ' ' x $indent, "[{say_it(@array)} ]{$indent??','!!''}";
    }

    sub say_it ( @array ) { return join ",", @array».fmt("%4s"); }
}

my @f[3;2;3] = (
  [
    [ -9,  5, -8 ],
    [  3,  5,  1 ],
  ],
  [
    [ -1, -7,  2 ],
    [ -5, -6,  6 ],
  ],
  [
    [  8,  5,  8 ],
    [ -2, -6, -4 ],
  ]
);

my @g[4;4;6] = (
  [
    [  54,  42,  53, -42,  85, -72 ],
    [  45,-170,  94, -36,  48,  73 ],
    [ -39,  65,-112, -16, -78, -72 ],
    [   6, -11,  -6,  62,  49,   8 ],
  ],
  [
    [ -57,  49, -23,  52,-135,  66 ],
    [ -23, 127, -58,  -5,-118,  64 ],
    [  87, -16, 121,  23, -41, -12 ],
    [ -19,  29,  35,-148, -11,  45 ],
  ],
  [
    [ -55,-147,-146, -31,  55,  60 ],
    [ -88, -45, -28,  46, -26,-144 ],
    [ -12,-107, -34, 150, 249,  66 ],
    [  11, -15, -34,  27, -78, -50 ],
  ],
  [
    [  56,  67, 108,   4,   2, -48 ],
    [  58,  67,  89,  32,  32,  -8 ],
    [ -42, -31,-103, -30, -23,  -8 ],
    [   6,   4, -26, -10,  26,  12 ],
  ]
);

say "# {+@f.shape}D array:";
my @h = deconvolve-N( @g, @f );
say "h =";
pretty_print( @h );
my @h-shaped[2;3;4] = @(deconvolve-N( @g, @f ));
my @ff = deconvolve-N( @g, @h-shaped );
say "\nff =";
pretty_print( @ff );
```


Output:

```txt
# 3D array:
h =
[
  [
    [  -6,  -8,  -5,   9 ],
    [  -7,   9,  -6,  -8 ],
    [   2,  -7,   9,   8 ],
  ],
  [
    [   7,   4,   4,  -6 ],
    [   9,   9,   4,  -4 ],
    [  -3,   7,  -2,  -3 ],
  ],
]

ff =
[
  [
    [  -9,   5,  -8 ],
    [   3,   5,   1 ],
  ],
  [
    [  -1,  -7,   2 ],
    [  -5,  -6,   6 ],
  ],
  [
    [   8,   5,   8 ],
    [  -2,  -6,  -4 ],
  ],
]
```



## Phix

Quite frankly I'm fairly astonished that it actually works...

(be warned this contains an exciting mix of 0- and 1- based indexes)

```Phix
-- demo\rosetta\Deconvolution.exw

function m_size(sequence m)
--
-- returns the size of a matrix as a list of lengths
--
    sequence res = {}
    object me = m
    while sequence(me) do
        res &= length(me)
        me = me[1]
    end while
    return res
end function

function product(sequence s)
--
-- multiply all elements of s together
--
    integer res = s[1]
    for i=2 to length(s) do
        res *= s[i]
    end for
    return res
end function

function make_coordset(sequence size)
--
-- returns all points in the matrix, in zero-based indexes,
-- eg {{0,0,0}..{3,3,5}} for a 4x4x6 matrix [96 in total]
--
    sequence res = {}
    integer count = product(size)
    for i=0 to count-1 do
        sequence coords = {}
        integer j = i
        for s=length(size) to 1 by -1 do
            integer dimension = size[s]
            coords &= mod(j,dimension)
            j = floor(j/dimension)
        end for
        coords = reverse(coords)
        res = append(res,coords)
    end for
    return res
end function

function row(sequence g, f, gs, gc, fs, hs)
--
--# Assembles a row, which is one of the simultaneous equations that needs
--# to be solved by reducing the whole set to reduced row echelon form. Note
--# that each row describes the equation for a single cell of the 'g' function.
--#
--# Arguments:
--# g   The "result" matrix of the convolution being undone.
--# h   The known "input" matrix of the convolution being undone.
--# gs  The size descriptor of 'g', passed as argument for efficiency.
--# gc  The coordinate in 'g' that we are generating the equation for.
--# fs  The size descriptor of 'f', passed as argument for efficiency.
--# hs  The size descriptor of 'h' (the unknown "input" matrix), passed
--#     as argument for efficiency.
--
    sequence row = {},
             coords = make_coordset(hs)
    for i=1 to length(coords) do
        sequence hc = coords[i]
        object fn = f
        for k=1 to length(gc) do
            integer d = gc[k]-hc[k]
            if d<0 or d>=fs[k] then
                fn = 0
                exit
            end if
            fn = fn[d+1]
        end for
        row = append(row,fn)
    end for
    object gn = g
    for i=1 to length(gc) do
        gn = gn[gc[i]+1]
    end for
    row = append(row,gn)
    return row
end function

function toRREF(sequence m)
--
-- [renamed] copy of Reduced_row_echelon_form.htm#Phix
-- plus one small tweak, as noted below, exit->return,
-- not that said seems to make any actual difference.
--
integer lead = 1,
        rows = length(m),
        cols = length(m[1])
    for r=1 to rows do
        if lead>=cols then exit end if
        integer i = r
        while m[i][lead]=0 do
            i += 1
            if i=rows then
                i = r
                lead += 1
--              if lead=cols then exit end if
                if lead=cols then return m end if
            end if
        end while
        -- nb m[i] is assigned before m[r], which matters when i=r:
        {m[r],m[i]} = {sq_div(m[i],m[i][lead]),m[r]}
        for j=1 to rows do
            if j!=r then
                m[j] = sq_sub(m[j],sq_mul(m[j][lead],m[r]))
            end if
        end for
        lead += 1
    end for
    return m
end function

function lset(sequence h, sequence idx, object v)
-- helper routine: store v somewhere deep inside h
    integer i1 = idx[1]+1
    if length(idx)=1 then
        h[i1] = v
    else
        h[i1] = lset(h[i1],idx[2..$],v)
    end if
    return h
end function

function deconvolve(sequence g, f)
--
--# Deconvolve a pair of matrixes. Solves for 'h' such that 'g = f convolve h'.
--#
--# Arguments:
--# g     The matrix of data to be deconvolved.
--# f     The matrix describing the convolution to be removed.
--
    -- Compute the sizes of the various matrixes involved.
    sequence gsize = m_size(g),
             fsize = m_size(f),
             hsize = sq_add(sq_sub(gsize,fsize),1)

    -- Prepare the set of simultaneous equations to solve
    sequence toSolve = {},
             coords = make_coordset(gsize)
    for i=1 to length(coords) do
        toSolve = append(toSolve,row(g,f,gsize,coords[i],fsize,hsize))
    end for

    -- Solve the equations
    sequence solved = toRREF(toSolve)

    -- Create a result matrix of the right size
    object h = 0
    for i=length(hsize) to 1 by -1 do
        h = repeat(h,hsize[i])
    end for

    -- Fill the results from the equations into the result matrix
    coords = make_coordset(hsize)
    for i=1 to length(coords) do
        h = lset(h,coords[i],solved[i][$])
    end for
    return h
end function

constant f1 = { 6, -9, -7, -5},
         g1 = {-48, 84, -16, 95, 125, -70, 7, 29, 54, 10},
         h1 = {-8, 2, -9, -2, 9, -8, -2}

if deconvolve(g1, f1)!=h1 then ?9/0 end if
if deconvolve(g1, h1)!=f1 then ?9/0 end if

constant f2 = {{-5, 2,-2,-6,-7},
               { 9, 7,-6, 5,-7},
               { 1,-1, 9, 2,-7},
               { 5, 9,-9, 2,-5},
               {-8, 5,-2, 8, 5}},
         g2 = {{ 40, -21, 53,  42, 105,   1,  87,  60,  39, -28},
               {-92, -64, 19,-167, -71, -47, 128,-109,  40, -21},
               { 58,  85,-93,  37, 101, -14,   5,  37, -76, -56},
               {-90,-135, 60,-125,  68,  53, 223,   4, -36, -48},
               { 78,  16,  7,-199, 156,-162,  29,  28,-103, -10},
               {-62, -89, 69, -61,  66, 193, -61,  71,  -8, -30},
               { 48,  -6, 21,  -9,-150, -22, -56,  32,  85,  25}},
         h2 = {{-8, 1,-7,-2,-9, 4},
               { 4, 5,-5, 2, 7,-1},
               {-6,-3,-3,-6, 9, 5}}

if deconvolve(g2, f2)!=h2 then ?9/0 end if
if deconvolve(g2, h2)!=f2 then ?9/0 end if

constant f3 = {{{-9,  5, -8}, { 3,  5,  1}},
               {{-1, -7,  2}, {-5, -6,  6}},
               {{ 8,  5,  8}, {-2, -6, -4}}},
         g3 = {{{ 54,  42,  53, -42,  85, -72},
                { 45,-170,  94, -36,  48,  73},
                {-39,  65,-112, -16, -78, -72},
                {  6, -11,  -6,  62,  49,   8}},
               {{-57,  49, -23,  52,-135,  66},
                {-23, 127, -58,  -5,-118,  64},
                { 87, -16, 121,  23, -41, -12},
                {-19,  29,  35,-148, -11,  45}},
               {{-55,-147,-146, -31,  55,  60},
                {-88, -45, -28,  46, -26,-144},
                {-12,-107, -34, 150, 249,  66},
                { 11, -15, -34,  27, -78, -50}},
               {{ 56,  67, 108,   4,   2, -48},
                { 58,  67,  89,  32,  32,  -8},
                {-42, -31,-103, -30, -23,  -8},
                {  6,   4, -26, -10,  26,  12}}},
         h3 = {{{ -6, -8, -5,  9},
                { -7,  9, -6, -8},
                {  2, -7,  9,  8}},
               {{  7,  4,  4, -6},
                {  9,  9,  4, -4},
                { -3,  7, -2, -3}}}

if deconvolve(g3, f3)!=h3 then ?9/0 end if
if deconvolve(g3, h3)!=f3 then ?9/0 end if

ppOpt({pp_Nest,2,pp_IntFmt,"%3d"})
pp(deconvolve(g3, f3))
pp(deconvolve(g3, h3))
```

```txt

{{{ -6, -8, -5,  9},
  { -7,  9, -6, -8},
  {  2, -7,  9,  8}},
 {{  7,  4,  4, -6},
  {  9,  9,  4, -4},
  { -3,  7, -2, -3}}}
{{{ -9,  5, -8},
  {  3,  5,  1}},
 {{ -1, -7,  2},
  { -5, -6,  6}},
 {{  8,  5,  8},
  { -2, -6, -4}}}

```

The version shipped in demo\rosetta contains the full 5 test sets: note that 5D takes a minute or two to complete.


## Python


Tested on all 5 test cases.

Blows up with divide by zero error on 4d deconv(g,f) because
the fft(f) returns 0 for a sample. This shows the limits of
doing a deconvolution with fft.

https://math.stackexchange.com/questions/380720/is-deconvolution-simply-division-in-frequency-domain


```python

"""

https://rosettacode.org/wiki/Deconvolution/2D%2B

Working on 3 dimensional example using test data from the
RC task.

Python fft:

https://docs.scipy.org/doc/numpy/reference/routines.fft.html

"""

import numpy
import pprint

h =  [
      [[-6, -8, -5, 9], [-7, 9, -6, -8], [2, -7, 9, 8]],
      [[7, 4, 4, -6], [9, 9, 4, -4], [-3, 7, -2, -3]]]
f =  [
      [[-9, 5, -8], [3, 5, 1]],
      [[-1, -7, 2], [-5, -6, 6]],
      [[8, 5, 8],[-2, -6, -4]]]
g =  [
      [
         [54, 42, 53, -42, 85, -72],
         [45, -170, 94, -36, 48, 73],
         [-39, 65, -112, -16, -78, -72],
         [6, -11, -6, 62, 49, 8]],
      [
         [-57, 49, -23, 52, -135, 66],
         [-23, 127, -58, -5, -118, 64],
         [87, -16, 121, 23, -41, -12],
         [-19, 29, 35, -148, -11, 45]],
      [
         [-55, -147, -146, -31, 55, 60],
         [-88, -45, -28, 46, -26, -144],
         [-12, -107, -34, 150, 249, 66],
         [11, -15, -34, 27, -78, -50]],
      [
         [56, 67, 108, 4, 2, -48],
         [58, 67, 89, 32, 32, -8],
         [-42, -31, -103, -30, -23, -8],
         [6, 4, -26, -10, 26, 12]]]

def trim_zero_empty(x):
    """

    Takes a structure that represents an n dimensional example.
    For a 2 dimensional example it will be a list of lists.
    For a 3 dimensional one it will be a list of list of lists.
    etc.

    Actually these are multidimensional numpy arrays but I was thinking
    in terms of lists.

    Returns the same structure without trailing zeros in the inner
    lists and leaves out inner lists with all zeros.

    """

    if len(x) > 0:
        if type(x[0]) != numpy.ndarray:
            # x is 1d array
            return list(numpy.trim_zeros(x))
        else:
            # x is a multidimentional array
            new_x = []
            for l in x:
               tl = trim_zero_empty(l)
               if len(tl) > 0:
                   new_x.append(tl)
            return new_x
    else:
        # x is empty list
        return x

def deconv(a, b):
    """

    Returns function c such that b * c = a.

    https://en.wikipedia.org/wiki/Deconvolution

    """

    # Convert larger polynomial using fft

    ffta = numpy.fft.fftn(a)

    # Get it's shape so fftn will expand
    # smaller polynomial to fit.

    ashape = numpy.shape(a)

    # Convert smaller polynomial with fft
    # using the shape of the larger one

    fftb = numpy.fft.fftn(b,ashape)

    # Divide the two in frequency domain

    fftquotient = ffta / fftb

    # Convert back to polynomial coefficients using ifft
    # Should give c but with some small extra components

    c = numpy.fft.ifftn(fftquotient)

    # Get rid of imaginary part and round up to 6 decimals
    # to get rid of small real components

    trimmedc = numpy.around(numpy.real(c),decimals=6)

    # Trim zeros and eliminate
    # empty rows of coefficients

    cleanc = trim_zero_empty(trimmedc)

    return cleanc

print("deconv(g,h)=")

pprint.pprint(deconv(g,h))

print(" ")

print("deconv(g,f)=")

pprint.pprint(deconv(g,f))

```


Output:


```txt

deconv(g,h)=
[[[-9.0, 5.0, -8.0], [3.0, 5.0, 1.0]],
 [[-1.0, -7.0, 2.0], [-5.0, -6.0, 6.0]],
 [[8.0, 5.0, 8.0], [-2.0, -6.0, -4.0]]]

deconv(g,f)=
[[[-6.0, -8.0, -5.0, 9.0], [-7.0, 9.0, -6.0, -8.0], [2.0, -7.0, 9.0, 8.0]],
 [[7.0, 4.0, 4.0, -6.0], [9.0, 9.0, 4.0, -4.0], [-3.0, 7.0, -2.0, -3.0]]]

```



## Tcl

The trick to doing this (without using a library to do all the legwork for you) is to recast the higher-order solutions into solutions in the 1D case. This is done by regarding an ''n''-dimensional address as a coding of a 1-D address.


```tcl
package require Tcl 8.5
namespace path {::tcl::mathfunc ::tcl::mathop}

# Utility to extract the number of dimensions of a matrix
proc rank m {
    for {set rank 0} {[llength $m] > 1} {incr rank} {
	set m [lindex $m 0]
    }
    return $rank
}

# Utility to get the size of a matrix, as a list of lengths
proc size m {
    set r [rank $m]
    set index {}
    set size {}
    for {set i 0} {$i<$r} {incr i} {
	lappend size [llength [lindex $m $index]]
	lappend index 0
    }
    return $size
}

# Utility that iterates over the space of coordinates within a matrix.
#
# Arguments:
#   var   The name of the variable (in the caller's context) to set to each
#         coordinate.
#   size  The size of matrix whose coordinates are to be iterated over.
#   body  The script to evaluate (in the caller's context) for each coordinate,
#         with the variable named by 'var' set to the coordinate for the particular
#         iteration.
proc loopcoords {var size body} {
    upvar 1 $var v
    set count [* {*}$size]
    for {set i 0} {$i < $count} {incr i} {
	set coords {}
	set j $i
	for {set s $size} {[llength $s]} {set s [lrange $s 0 end-1]} {
	    set dimension [lindex $s end]
	    lappend coords [expr {$j % $dimension}]
	    set j [expr {$j / $dimension}]
	}
	set v [lreverse $coords]
	uplevel 1 $body
    }
}

# Assembles a row, which is one of the simultaneous equations that needs
# to be solved by reducing the whole set to reduced row echelon form. Note
# that each row describes the equation for a single cell of the 'g' function.
#
# Arguments:
#   g	The "result" matrix of the convolution being undone.
#   h	The known "input" matrix of the convolution being undone.
#   gs	The size descriptor of 'g', passed as argument for efficiency.
#   gc	The coordinate in 'g' that we are generating the equation for.
#   fs	The size descriptor of 'f', passed as argument for efficiency.
#   hs	The size descriptor of 'h' (the unknown "input" matrix), passed
#	as argument for efficiency.
proc row {g f gs gc fs hs} {
    loopcoords hc $hs {
	set fc {}
	set ok 1
	foreach a $gc b $fs c $hc {
	    set d [expr {$a - $c}]
	    if {$d < 0 || $d >= $b} {
		set ok 0
		break
	    }
	    lappend fc $d
	}
	if {$ok} {
	    lappend row [lindex $f $fc]
	} else {
	    lappend row 0
	}
    }
    return [lappend row [lindex $g $gc]]
}

# Utility for converting a matrix to Reduced Row Echelon Form
# From http://rosettacode.org/wiki/Reduced_row_echelon_form#Tcl
proc toRREF {m} {
    set lead 0
    set rows [llength $m]
    set cols [llength [lindex $m 0]]
    for {set r 0} {$r < $rows} {incr r} {
	if {$cols <= $lead} {
	    break
	}
	set i $r
	while {[lindex $m $i $lead] == 0} {
	    incr i
	    if {$rows == $i} {
		set i $r
		incr lead
		if {$cols == $lead} {
		    # Tcl can't break out of nested loops
		    return $m
		}
	    }
	}
	# swap rows i and r
	foreach j [list $i $r] row [list [lindex $m $r] [lindex $m $i]] {
	    lset m $j $row
	}
	# divide row r by m(r,lead)
	set val [lindex $m $r $lead]
	for {set j 0} {$j < $cols} {incr j} {
	    lset m $r $j [/ [double [lindex $m $r $j]] $val]
	}

	for {set i 0} {$i < $rows} {incr i} {
	    if {$i != $r} {
		# subtract m(i,lead) multiplied by row r from row i
		set val [lindex $m $i $lead]
		for {set j 0} {$j < $cols} {incr j} {
		    lset m $i $j \
			[- [lindex $m $i $j] [* $val [lindex $m $r $j]]]
		}
	    }
	}
	incr lead
    }
    return $m
}

# Deconvolve a pair of matrixes. Solves for 'h' such that 'g = f convolve h'.
#
# Arguments:
#   g     The matrix of data to be deconvolved.
#   f     The matrix describing the convolution to be removed.
#   type  Optional description of the type of data expected. Defaults to 32-bit
#         integer data; use 'double' for floating-point data.
proc deconvolve {g f {type int}} {
    # Compute the sizes of the various matrixes involved.
    set gsize [size $g]
    set fsize [size $f]
    foreach gs $gsize fs $fsize {
	lappend hsize [expr {$gs - $fs + 1}]
    }

    # Prepare the set of simultaneous equations to solve
    set toSolve {}
    loopcoords coords $gsize {
	lappend toSolve [row $g $f $gsize $coords $fsize $hsize]
    }

    # Solve the equations
    set solved [toRREF $toSolve]

    # Make a dummy result matrix of the right size
    set h 0
    foreach hs [lreverse $hsize] {set h [lrepeat $hs $h]}

    # Fill the results from the equations into the result matrix
    set idx 0
    loopcoords coords $hsize {
	lset h $coords [$type [lindex $solved $idx end]]
	incr idx
    }

    return $h
}
```

Demonstrating how to use for the 3-D case:

```tcl
# A pretty-printer
proc pretty matrix {
    set size [rank $matrix]
    if {$size == 1} {
	return \[[join $matrix ", "]\]
    } elseif {$size == 2} {
	set out ""
	foreach row $matrix {
	    append out " " [pretty $row] ",\n"
	}
	return \[[string trimleft [string trimright $out ,\n]]\]
    }
    set rowout {}
    foreach row $matrix {append rowout [pretty $row] ,\n}
    set rowout2 {}
    foreach row [split [string trimright $rowout ,\n] \n] {
	append rowout2 "   " $row \n
    }
    return \[\n[string trimright $rowout2 \n]\n\]
}

# The 3D test data
set f {
    {{-9 5 -8} {3 5 1}}
    {{-1 -7 2} {-5 -6 6}}
    {{8 5 8} {-2 -6 -4}}
}
set g {
    {
	{54 42 53 -42 85 -72}
	{45 -170 94 -36 48 73}
	{-39 65 -112 -16 -78 -72}
	{6 -11 -6 62 49 8}}
    {
	{-57 49 -23 52 -135 66}
	{-23 127 -58 -5 -118 64}
	{87 -16 121 23 -41 -12}
	{-19 29 35 -148 -11 45}}
    {
	{-55 -147 -146 -31 55 60}
	{-88 -45 -28 46 -26 -144}
	{-12 -107 -34 150 249 66}
	{11 -15 -34 27 -78 -50}}
    {
	{56 67 108 4 2 -48}
	{58 67 89 32 32 -8}
	{-42 -31 -103 -30 -23 -8}
	{6 4 -26 -10 26 12}}
}

# Now do the deconvolution and print it out
puts h:\ [pretty [deconvolve $g $f]]
```

Output:

```txt

h: [
   [[-6, -8, -5, 9],
    [-7, 9, -6, -8],
    [2, -7, 9, 8]],
   [[7, 4, 4, -6],
    [9, 9, 4, -4],
    [-3, 7, -2, -3]]

```



## Ursala

This is done mostly with list operations that are either primitive or standard library functions in the language (e.g., <code>zipp</code>, <code>zipt</code>, and <code>pad</code>). The equations are solved by
the [http://www.netlib.org/lapack/lug/node27.html <code>dgelsd</code>] function from the Lapack library.
The <code>break</code> function breaks a long list into a sequence of sublists according to a given template, and the <code>band</code> function is taken from the [[Deconvolution/1D]] solution.

```Ursala
#import std
#import nat

break = ~&r**+ zipt*+ ~&lh*~+ ~&lzyCPrX|\+ -*^|\~&tK33 :^/~& 0!*t

band = pad0+ ~&rSS+ zipt^*D(~&r,^lrrSPT/~&ltK33tx zipt^/~&r ~&lSNyCK33+ zipp0)^/~&rx ~&B->NlNSPC ~&bt

deconv = # takes a natural number n to the n-dimensional deconvolution function

~&?\math..div! iota; ~&!*; @h|\; (~&al^?\~&ar break@alh2faltPrXPRX)^^(
   ~&B->NlC~&bt*++ gang@t+ ~~*,
   lapack..dgelsd^^(
      (~&||0.!**+ ~&B^?a\~&Y@a ^lriFhNSS2iDrlYSK7LS2SL2rQ/~&alt band@alh2faltPrDPMX)^|\~&+ gang,
      @t =>~&l ~&L+@r))
```

The equations tend to become increasingly sparse in higher dimensions,
so the following alternative implementation uses the sparse matrix
solver from the [http://www.cise.ufl.edu/research/sparse/umfpack/ UMFPACK] library
instead of Lapack, which is also callable in Ursala, adjusted as shown for the different
[http://www.basis.uklinux.net/avram/refman/umf-input-parameters.html calling convention].

```Ursala
deconv = # takes a number n to the n-dimensional deconvolution function

~&?\math..div! iota; ~&!*; @h|\; -+
   //+ ~&al^?\~&ar @alh2faltPrXPRX @liX ~&arr2arl2arrh3falrbt2XPRXlrhPCrtPCPNfallrrPXXPRCQNNCq,
   ^^/-+~&B->NlC~&bt*+,gang@t,~~*+- (umf..di_a_trp^/~&DSLlrnPXrmPXS+num@lmS ^niK10mS/num@r ~&lnS)^^(
      gang; ^|\~&; //+ -+
         ^niK10/~& @NnmlSPASX ~&r->lL @lrmK2K8SmtPK20PPPX ^/~&rrnS2lC ~&rnPrmPljASmF@rrmhPSPlD,
         num+ ~&B^?a\~&Y@a -+
            ~&l?\~&r *=r ~&K7LS+ * (*D ^\~&rr sum@lrlPX)^*D\~&r product^|/~& successor@zhl,
            ^/~&alt @alh2faltPrDPMX -+
               ~&rFS+ num*rSS+ zipt^*D/~&r ^lrrSPT/~&ltK33tx zipt^/~&r ~&lSNyCK33+ zipp0,
               ^/~&rx ~&B->NlNSPC ~&bt+-+-+-,
      @t =>~&l ~&L+@r)+-
```

UMFPACK doesn't solve systems with more equations than unknowns, so
the system is pruned to a square matrix by first selecting an equation
containing only a single variable, then selecting one from those
remaining that contains only a single variable not already selected,
and so on until all variables are covered, with any remaining
unselected equations discarded. A random selection is made whenever
there is a choice. This method will cope with larger data sets than feasible
using dense and overdetermined matrices, but is less robust in the presence of noise.
However, some improvement may be possible by averaging the results over several runs.
Here is a test program.

```Ursala
h = <<<-6.,-8.,-5.,9.>,<-7.,9.,-6.,-8.>,<2.,-7.,9.,8.>>,<<7.,4.,4.,-6.>,<9.,9.,4.,-4.>,<-3.,7.,-2.,-3.>>>
f = <<<-9.,5.,-8.>,<3.,5.,1.>>,<<-1.,-7.,2.>,<-5.,-6.,6.>>,<<8.,5.,8.>,<-2.,-6.,-4.>>>

g =

<
   <
      <54.,42.,53.,-42.,85.,-72.>,
      <45.,-170.,94.,-36.,48.,73.>,
      <-39.,65.,-112.,-16.,-78.,-72.>,
      <6.,-11.,-6.,62.,49.,8.>>,
   <
      <-57.,49.,-23.,52.,-135.,66.>,
      <-23.,127.,-58.,-5.,-118.,64.>,
      <87.,-16.,121.,23.,-41.,-12.>,
      <-19.,29.,35.,-148.,-11.,45.>>,
   <
      <-55.,-147.,-146.,-31.,55.,60.>,
      <-88.,-45.,-28.,46.,-26.,-144.>,
      <-12.,-107.,-34.,150.,249.,66.>,
      <11.,-15.,-34.,27.,-78.,-50.>>,
   <
      <56.,67.,108.,4.,2.,-48.>,
      <58.,67.,89.,32.,32.,-8.>,
      <-42.,-31.,-103.,-30.,-23.,-8.>,
      <6.,4.,-26.,-10.,26.,12.>>>

#cast %eLLLm

test =

<
   'h': deconv3(g,f),
   'f': deconv3(g,h)>

```

output:

```txt
<
   'h': <
      <
         <
            -6.000000e+00,
            -8.000000e+00,
            -5.000000e+00,
            9.000000e+00>,
         <
            -7.000000e+00,
            9.000000e+00,
            -6.000000e+00,
            -8.000000e+00>,
         <
            2.000000e+00,
            -7.000000e+00,
            9.000000e+00,
            8.000000e+00>>,
      <
         <
            7.000000e+00,
            4.000000e+00,
            4.000000e+00,
            -6.000000e+00>,
         <
            9.000000e+00,
            9.000000e+00,
            4.000000e+00,
            -4.000000e+00>,
         <
            -3.000000e+00,
            7.000000e+00,
            -2.000000e+00,
            -3.000000e+00>>>,
   'f': <
      <
         <-9.000000e+00,5.000000e+00,-8.000000e+00>,
         <3.000000e+00,5.000000e+00,1.000000e+00>>,
      <
         <-1.000000e+00,-7.000000e+00,2.000000e+00>,
         <-5.000000e+00,-6.000000e+00,6.000000e+00>>,
      <
         <8.000000e+00,5.000000e+00,8.000000e+00>,
         <-2.000000e+00,-6.000000e+00,-4.000000e+00>>>>
```

