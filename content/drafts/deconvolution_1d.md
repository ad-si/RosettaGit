+++
title = "Deconvolution/1D"
description = ""
date = 2019-10-21T04:14:15Z
aliases = []
[extra]
id = 6107
[taxonomies]
categories = []
tags = []
+++

{{task|Mathematical operations}}
The convolution of two functions <math>\mathit{F}</math> and <math>\mathit{H}</math> of
an integer variable is defined as the function <math>\mathit{G}</math>
satisfying

:<math> G(n) = \sum_{m=-\infty}^{\infty} F(m) H(n-m) </math>

for all integers <math>\mathit{n}</math>. Assume <math>F(n)</math> can be non-zero only for <math>0</math> &le; <math>\mathit{n}</math> &le; <math>|\mathit{F}|</math>, where <math>|\mathit{F}|</math> is the "length" of <math>\mathit{F}</math>, and similarly for <math>\mathit{G}</math> and <math>\mathit{H}</math>, so that the functions can be modeled as finite sequences by identifying <math>f_0, f_1, f_2, \dots</math> with <math>F(0), F(1), F(2), \dots</math>, etc. 
Then for example, values of <math>|\mathit{F}| = 6</math> and <math>|\mathit{H}| = 5</math> would determine the following value of <math>\mathit{g}</math> by definition.

:<math>
\begin{array}{lllllllllll}
g_0 &= &f_0h_0\\
g_1 &= &f_1h_0 &+ &f_0h_1\\
g_2 &= &f_2h_0 &+ &f_1h_1 &+ &f_0h_2\\
g_3 &= &f_3h_0 &+ &f_2h_1 &+ &f_1h_2 &+ &f_0h_3\\
g_4 &= &f_4h_0 &+ &f_3h_1 &+ &f_2h_2 &+ &f_1h_3 &+ &f_0h_4\\
g_5 &= &f_5h_0 &+ &f_4h_1 &+ &f_3h_2 &+ &f_2h_3 &+ &f_1h_4\\
g_6 &= &       &  &f_5h_1 &+ &f_4h_2 &+ &f_3h_3 &+ &f_2h_4\\
g_7 &= &       &  &       &  &f_5h_2 &+ &f_4h_3 &+ &f_3h_4\\
g_8 &= &       &  &       &  &       &  &f_5h_3 &+ &f_4h_4\\
g_9 &= &       &  &       &  &       &  &       &  &f_5h_4
\end{array}
</math>

We can write this in matrix form as:

:<math>
\left( 
\begin{array}{l}
g_0 \\
g_1 \\
g_2 \\
g_3 \\
g_4 \\
g_5 \\
g_6 \\
g_7 \\
g_8 \\
g_9 \\
\end{array}
\right) = \left(
\begin{array}{lllll}
f_0\\
f_1 & f_0\\
f_2 & f_1 & f_0\\
f_3 & f_2 & f_1 & f_0\\
f_4 & f_3 & f_2 & f_1 & f_0\\
f_5 & f_4 & f_3 & f_2 & f_1\\
    & f_5 & f_4 & f_3 & f_2\\
    &     & f_5 & f_4 & f_3\\
    &     &     & f_5 & f_4\\
    &     &     &     & f_5
\end{array}
\right) \; \left(
\begin{array}{l}
h_0 \\
h_1 \\
h_2 \\
h_3 \\
h_4 \\
\end{array} \right)
</math>

or

:<math>
g = A \; h
</math>

For this task, implement a function (or method, procedure, subroutine, etc.) <code>deconv</code> to perform ''deconvolution'' (i.e., the ''inverse'' of convolution) by constructing and solving such a system of equations represented by the above matrix <math>A</math> for <math>\mathit{h}</math> given <math>\mathit{f}</math> and <math>\mathit{g}</math>.

* The function should work for <math>\mathit{G}</math> of arbitrary length (i.e., not hard coded or constant) and <math>\mathit{F}</math> of any length up to that of <math>\mathit{G}</math>. Note that <math>|\mathit{H}|</math> will be given by <math>|\mathit{G}| - |\mathit{F}| + 1</math>.
* There may be more equations than unknowns. If convenient, use a function from a [http://www.netlib.org/lapack/lug/node27.html library] that finds the best fitting solution to an overdetermined system of linear equations (as in the [[Multiple regression]] task).  Otherwise, prune the set of equations as needed and solve as in the [[Reduced row echelon form]] task.
* Test your solution on the following data. Be sure to verify both that <code>deconv</code><math>(g,f) = h</math> and <code>deconv</code><math>(g,h) = f</math> and display the results in a human readable form.
<code>
h = [-8,-9,-3,-1,-6,7]

f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1]

g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7]
</code>


## 11l

{{trans|D}}

```11l
F deconv(g, f)
   V result = [0]*(g.len - f.len + 1)
   L(&e) result
      V n = L.index
      e = g[n]
      V lower_bound = I n >= f.len {n - f.len + 1} E 0
      L(i) lower_bound .< n
         e -= result[i] * f[n - i]
      e /= f[0]
   R result

V h = [-8,-9,-3,-1,-6,7]
V f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1]
V g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7]
print(deconv(g, f))
print(deconv(g, h))
```

{{out}}

```txt

[-8, -9, -3, -1, -6, 7]
[-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
As several others, this is a translation of the '''D''' solution.

```bbcbasic
      *FLOAT 64
      DIM h(5), f(15), g(20)
      h() = -8,-9,-3,-1,-6,7
      f() = -3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1
      g() = 24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7
      
      PROCdeconv(g(), f(), x())
      PRINT "deconv(g,f) = " FNprintarray(x())
      x() -= h() : IF SUM(x()) <> 0 PRINT "Error!"
      
      PROCdeconv(g(), h(), y())
      PRINT "deconv(g,h) = " FNprintarray(y())
      y() -= f() : IF SUM(y()) <> 0 PRINT "Error!"
      END
      
      DEF PROCdeconv(g(), f(), RETURN h())
      LOCAL f%, g%, i%, l%, n%
      f% = DIM(f(),1) + 1
      g% = DIM(g(),1) + 1
      DIM h(g% - f%)
      FOR n% = 0 TO g% - f%
        h(n%) = g(n%)
        IF n% < f% THEN l% = 0 ELSE l% = n% - f% + 1
        IF n% THEN
          FOR i% = l% TO n% - 1
            h(n%) -= h(i%) * f(n% - i%)
          NEXT
        ENDIF
        h(n%) /= f(0)
      NEXT n%
      ENDPROC
      
      DEF FNprintarray(a())
      LOCAL i%, a$
      FOR i% = 0 TO DIM(a(),1)
        a$ += STR$(a(i%)) + ", "
      NEXT
      = LEFT$(LEFT$(a$))
```

{{out}}

```txt

deconv(g,f) = -8, -9, -3, -1, -6, 7
deconv(g,h) = -3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1

```



## C

Using [[FFT]]:

```C>#include <stdio.h

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

void deconv(double g[], int lg, double f[], int lf, double out[]) {
	int ns = 0;
	cplx *g2 = pad_two(g, lg, &ns);
	cplx *f2 = pad_two(f, lf, &ns);

	fft(g2, ns);
	fft(f2, ns);

	cplx h[ns];
	for (int i = 0; i < ns; i++) h[i] = g2[i] / f2[i];
	fft(h, ns);

	for (int i = 0; i >= lf - lg; i--)
		out[-i] = h[(i + ns) % ns]/32;
	free(g2);
	free(f2);
}

int main()
{
	PI = atan2(1,1) * 4;
	double g[] = {24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7};
	double f[] = { -3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1 };
	double h[] = { -8,-9,-3,-1,-6,7 };

	int lg = sizeof(g)/sizeof(double);
	int lf = sizeof(f)/sizeof(double);
	int lh = sizeof(h)/sizeof(double);

	double h2[lh];
	double f2[lf];

	printf("f[] data is : ");
	for (int i = 0; i < lf; i++) printf(" %g", f[i]);
	printf("\n");

	printf("deconv(g, h): ");
	deconv(g, lg, h, lh, f2);
	for (int i = 0; i < lf; i++) printf(" %g", f2[i]);
	printf("\n");

	printf("h[] data is : ");
	for (int i = 0; i < lh; i++) printf(" %g", h[i]);
	printf("\n");

	printf("deconv(g, f): ");
	deconv(g, lg, f, lf, h2);
	for (int i = 0; i < lh; i++) printf(" %g", h2[i]);
	printf("\n");
}
```

{{out}}
```txt
f[] data is :  -3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1
deconv(g, h):  -3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1
h[] data is :  -8 -9 -3 -1 -6 7
deconv(g, f):  -8 -9 -3 -1 -6 7
```



## Common Lisp

Uses the routine (lsqr A b) from [[Multiple regression]] and (mtp A) from [[Matrix transposition]].


```lisp
;; Assemble the mxn matrix A from the 2D row vector x.
(defun make-conv-matrix (x m n)
  (let ((lx (cadr (array-dimensions x)))
        (A  (make-array `(,m ,n) :initial-element 0)))

    (loop for j from 0 to (- n 1) do
         (loop for i from 0 to (- m 1) do
              (setf (aref A i j)
                    (cond ((or (< i j) (>= i (+ j lx)))
                           0)
                          ((and (>= i j) (< i (+ j lx)))
                           (aref x 0 (- i j)))))))
    A))

;; Solve the overdetermined system A(f)*h=g by linear least squares.
(defun deconv (g f)
  (let* ((lg (cadr (array-dimensions g)))
         (lf (cadr (array-dimensions f)))
         (lh (+ (- lg lf) 1))
         (A  (make-conv-matrix f lg lh)))

    (lsqr A (mtp g))))
```


Example:


```lisp
(setf f #2A((-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1)))
(setf h #2A((-8 -9 -3 -1 -6 7)))
(setf g #2A((24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7)))

(deconv g f)
#2A((-8.0)
    (-9.000000000000002)
    (-2.999999999999999)
    (-0.9999999999999997)
    (-6.0)
    (7.000000000000002))

(deconv g h)
#2A((-2.999999999999999)
    (-6.000000000000001)
    (-1.0000000000000002)
    (8.0)
    (-5.999999999999999)
    (3.0000000000000004)
    (-1.0000000000000004)
    (-9.000000000000002)
    (-9.0)
    (2.9999999999999996)
    (-1.9999999999999991)
    (5.0)
    (1.9999999999999996)
    (-2.0000000000000004)
    (-7.000000000000001)
    (-0.9999999999999994))
```



## D


```d
T[] deconv(T)(in T[] g, in T[] f) pure nothrow {
    int flen = f.length;
    int glen = g.length;
    auto result = new T[glen - flen + 1];
    foreach (int n, ref e; result) {
        e = g[n];
        immutable lowerBound = (n >= flen) ? n - flen + 1 : 0;
        foreach (i; lowerBound .. n)
                e -= result[i] * f[n - i];
        e /= f[0];
    }
    return result;
}

void main() {
    import std.stdio;
    immutable h = [-8,-9,-3,-1,-6,7];
    immutable f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1];
    immutable g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,
                   -96,96,31,55,36,29,-43,-7];
    writeln(deconv(g, f) == h, " ", deconv(g, f));
    writeln(deconv(g, h) == f, " ", deconv(g, h));
}
```

{{out}}

```txt
true [-8, -9, -3, -1, -6, 7]
true [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]
```



## Fortran

This solution uses the LAPACK95 library.

```fortran

! Build
! Windows: ifort /I "%IFORT_COMPILER11%\mkl\include\ia32" deconv1d.f90 "%IFORT_COMPILER11%\mkl\ia32\lib\*.lib"
! Linux:

program deconv
  ! Use gelsd from LAPACK95.
  use mkl95_lapack, only : gelsd

  implicit none
  real(8), allocatable :: g(:), href(:), A(:,:), f(:)
  real(8), pointer     :: h(:), r(:)
  integer              :: N
  character(len=16)    :: cbuff
  integer              :: i
  intrinsic            :: nint

  ! Allocate data arrays
  allocate(g(21),f(16))
  g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7]
  f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1]

  ! Calculate deconvolution
  h => deco(f, g)

  ! Check result against reference
  N = size(h)
  allocate(href(N))
  href = [-8,-9,-3,-1,-6,7]
  cbuff = ' '
  write(cbuff,'(a,i0,a)') '(a,',N,'(i0,a),i0)'
  if (any(abs(h-href) > 1.0d-4)) then
     write(*,'(a)') 'deconv(f, g) - FAILED'
  else
     write(*,cbuff) 'deconv(f, g) = ',(nint(h(i)),', ',i=1,N-1),nint(h(N))
  end if

  ! Calculate deconvolution
  r => deco(h, g)

  cbuff = ' '
  N = size(r)
  write(cbuff,'(a,i0,a)') '(a,',N,'(i0,a),i0)'
  if (any(abs(r-f) > 1.0d-4)) then
     write(*,'(a)') 'deconv(h, g) - FAILED'
  else
     write(*,cbuff) 'deconv(h, g) = ',(nint(r(i)),', ',i=1,N-1),nint(r(N))
  end if

contains
  function deco(p, q)
    real(8), pointer    :: deco(:)
    real(8), intent(in) :: p(:), q(:)

    real(8), allocatable, target :: r(:)
    real(8), allocatable         :: A(:,:)
    integer :: N

    ! Construct derived arrays
    N = size(q) - size(p) + 1
    allocate(A(size(q),N),r(size(q)))
    A = 0.0d0
    do i=1,N
       A(i:i+size(p)-1,i) = p
    end do
    
    ! Invoke the LAPACK routine to do the work
    r = q
    call gelsd(A, r)

    deco => r(1:N)
  end function deco

end program deconv

```

Results:

```fortran

deconv(f, g) = -8, -9, -3, -1, -6, 7
deconv(h, g) = -3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1

```


## Go

{{trans|D}}

```go
package main

import "fmt"

func main() {
    h := []float64{-8, -9, -3, -1, -6, 7}
    f := []float64{-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1}
    g := []float64{24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96,
        96, 31, 55, 36, 29, -43, -7}
    fmt.Println(h)
    fmt.Println(deconv(g, f))
    fmt.Println(f)
    fmt.Println(deconv(g, h))
}

func deconv(g, f []float64) []float64 {
    h := make([]float64, len(g)-len(f)+1)
    for n := range h {
        h[n] = g[n]
        var lower int
        if n >= len(f) {
            lower = n - len(f) + 1
        }
        for i := lower; i < n; i++ {
            h[n] -= h[i] * f[n-i]
        }
        h[n] /= f[0]
    }
    return h
}
```

{{out}}

```txt

[-8 -9 -3 -1 -6 7]
[-8 -9 -3 -1 -6 7]
[-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1]
[-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1]

```


{{trans|C}}

```go
package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

func main() {
    h := []float64{-8, -9, -3, -1, -6, 7}
    f := []float64{-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1}
    g := []float64{24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96,
        96, 31, 55, 36, 29, -43, -7}
    fmt.Printf("%.1f\n", h)
    fmt.Printf("%.1f\n", deconv(g, f))
    fmt.Printf("%.1f\n", f)
    fmt.Printf("%.1f\n", deconv(g, h))
}

func deconv(g, f []float64) []float64 {
    n := 1
    for n < len(g) {
        n *= 2
    }
    g2 := make([]complex128, n)
    for i, x := range g {
        g2[i] = complex(x, 0)
    }
    f2 := make([]complex128, n)
    for i, x := range f {
        f2[i] = complex(x, 0)
    }
    gt := fft(g2)
    ft := fft(f2)
    for i := range gt {
        gt[i] /= ft[i]
    }
    ht := fft(gt)
    it := 1 / float64(n)
    out := make([]float64, len(g)-len(f)+1)
    out[0] = real(ht[0]) * it
    for i := 1; i < len(out); i++ {
        out[i] = real(ht[n-i]) * it
    }
    return out
}

func fft(in []complex128) []complex128 {
    out := make([]complex128, len(in))
    ditfft2(in, out, len(in), 1)
    return out
}

func ditfft2(x, y []complex128, n, s int) {
    if n == 1 {
        y[0] = x[0]
        return
    }
    ditfft2(x, y, n/2, 2*s)
    ditfft2(x[s:], y[n/2:], n/2, 2*s)
    for k := 0; k < n/2; k++ {
        tf := cmplx.Rect(1, -2*math.Pi*float64(k)/float64(n)) * y[k+n/2]
        y[k], y[k+n/2] = y[k]+tf, y[k]-tf
    }
}
```

{{out}}
Some results have errors out in the last decimal place or so.  Only one decimal place shown here to let results fit in 80 columns.

```txt

[-8.0 -9.0 -3.0 -1.0 -6.0 7.0]
[-8.0 -9.0 -3.0 -1.0 -6.0 7.0]
[-3.0 -6.0 -1.0 8.0 -6.0 3.0 -1.0 -9.0 -9.0 3.0 -2.0 5.0 2.0 -2.0 -7.0 -1.0]
[-3.0 -6.0 -1.0 8.0 -6.0 3.0 -1.0 -9.0 -9.0 3.0 -2.0 5.0 2.0 -2.0 -7.0 -1.0]

```

'''Library gonum/mat:'''

```go
package main

import (
    "fmt"

    "gonum.org/v1/gonum/mat"
)

var (
    h = []float64{-8, -9, -3, -1, -6, 7}
    f = []float64{-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1}
    g = []float64{24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96,
        96, 31, 55, 36, 29, -43, -7}
)

func band(g, f []float64) *mat.Dense {
    nh := len(g) - len(f) + 1
    b := mat.NewDense(len(g), nh, nil)
    for j := 0; j < nh; j++ {
        for i, fi := range f {
            b.Set(i+j, j, fi)
        }
    }
    return b
}

func deconv(g, f []float64) mat.Matrix {
    z := mat.NewDense(len(g)-len(f)+1, 1, nil)
    z.Solve(band(g, f), mat.NewVecDense(len(g), g))
    return z
}

func main() {
    fmt.Printf("deconv(g, f) =\n%.1f\n\n", mat.Formatted(deconv(g, f)))
    fmt.Printf("deconv(g, h) =\n%.1f\n", mat.Formatted(deconv(g, h)))
}
```

{{out}}

```txt

deconv(g, f) =
⎡-8.0⎤
⎢-9.0⎥
⎢-3.0⎥
⎢-1.0⎥
⎢-6.0⎥
⎣ 7.0⎦

deconv(g, h) =
⎡-3.0⎤
⎢-6.0⎥
⎢-1.0⎥
⎢ 8.0⎥
⎢-6.0⎥
⎢ 3.0⎥
⎢-1.0⎥
⎢-9.0⎥
⎢-9.0⎥
⎢ 3.0⎥
⎢-2.0⎥
⎢ 5.0⎥
⎢ 2.0⎥
⎢-2.0⎥
⎢-7.0⎥
⎣-1.0⎦

```



## Haskell


```haskell
deconv1d :: [Double] -> [Double] -> [Double]
deconv1d xs ys = takeWhile (/= 0) $ deconv xs ys
  where
    [] `deconv` _ = []
    (0:xs) `deconv` (0:ys) = xs `deconv` ys
    (x:xs) `deconv` (y:ys) =
      let q = x / y
      in q : zipWith (-) xs (scale q ys ++ repeat 0) `deconv` (y : ys)

scale :: Double -> [Double] -> [Double]
scale = map . (*)

h, f, g :: [Double]
h = [-8, -9, -3, -1, -6, 7]

f = [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]

g =
  [ 24
  , 75
  , 71
  , -34
  , 3
  , 22
  , -45
  , 23
  , 245
  , 25
  , 52
  , 25
  , -67
  , -96
  , 96
  , 31
  , 55
  , 36
  , 29
  , -43
  , -7
  ]

main :: IO ()
main = print $ (h == deconv1d g f) && (f == deconv1d g h)
```

{{Out}}

```txt
True
```



## J


This solution borrowed from [[Formal_power_series#J|Formal power series]]:


```J
Ai=: (i.@]  =/ i.@[ -/ i.@>:@-)&#
divide=: [ +/ .*~ [:%.&.x: ] +/ .* Ai
```


Sample data:


```J
h=: _8 _9 _3 _1 _6 7
f=: _3 _6 _1 8 _6 3 _1 _9 _9 3 _2 5 2 _2 _7 _1
g=: 24 75 71 _34 3 22 _45 23 245 25 52 25 _67 _96 96 31 55 36 29
```


Example use:

```J
   g divide f
_8 _9 _3 _1 _6 7
   g divide h
_3 _6 _1 8 _6 3 _1 _9 _9 3 _2 5 2 _2 _7 _1
```


That said, note that this particular implementation is slow since it uses extended precision intermediate results.  It will run quite a bit faster for this example with no notable loss of precision if floating point is used.  In other words:


```J
divide=: [ +/ .*~ [:%. ] +/ .* Ai
```



## Java

{{trans|Go}}

```java
import java.util.Arrays;

public class Deconvolution1D {
    public static int[] deconv(int[] g, int[] f) {
        int[] h = new int[g.length - f.length + 1];
        for (int n = 0; n < h.length; n++) {
            h[n] = g[n];
            int lower = Math.max(n - f.length + 1, 0);
            for (int i = lower; i < n; i++)
                h[n] -= h[i] * f[n - i];
            h[n] /= f[0];
        }
        return h;
    }

    public static void main(String[] args) {
        int[] h = { -8, -9, -3, -1, -6, 7 };
        int[] f = { -3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1 };
        int[] g = { 24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96,
                96, 31, 55, 36, 29, -43, -7 };

        StringBuilder sb = new StringBuilder();
        sb.append("h = " + Arrays.toString(h) + "\n");
        sb.append("deconv(g, f) = " + Arrays.toString(deconv(g, f)) + "\n");
        sb.append("f = " + Arrays.toString(f) + "\n");
        sb.append("deconv(g, h) = " + Arrays.toString(deconv(g, h)) + "\n");
        System.out.println(sb.toString());
    }
}
```

{{out}}

```txt

h = [-8, -9, -3, -1, -6, 7]
deconv(g, f) = [-8, -9, -3, -1, -6, 7]
f = [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]
deconv(g, h) = [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]

```



## Julia

The deconv function for floating point data is built into Julia, though <code>using DSP</code> is required with version 1.0.
Integer inputs may need to be converted and copied to floating point to use deconv().


```julia
h = [-8, -9, -3, -1, -6, 7]
g = [24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96, 96, 31, 55, 36, 29, -43, -7]
f = [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]

hanswer = deconv(float.(g), float.(f))
println("The deconvolution deconv(g, f) is $hanswer, which is the same as h = $h\n")

fanswer = deconv(float.(g), float.(h))
println("The deconvolution deconv(g, h) is $fanswer, which is the same as f = $f\n")
```


{{output}}

```txt
The deconvolution deconv(g, f) is [-8.0, -9.0, -3.0, -1.0, -6.0, 7.0], 
which is the same as h = [-8, -9, -3, -1, -6, 7]

The deconvolution deconv(g, h) is [-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0], 
which is the same as f = [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]
```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.3

fun deconv(g: DoubleArray, f: DoubleArray): DoubleArray {
    val fs = f.size   
    val h = DoubleArray(g.size - fs + 1)
    for (n in h.indices) {
        h[n] = g[n]
        val lower = if (n >= fs) n - fs + 1 else 0
        for (i in lower until n) h[n] -= h[i] * f[n -i]
        h[n] /= f[0] 
    }
    return h
}

fun main(args: Array<String>) {
    val h = doubleArrayOf(-8.0, -9.0, -3.0, -1.0, -6.0, 7.0)
    val f = doubleArrayOf(-3.0, -6.0, -1.0,  8.0, -6.0,  3.0, -1.0, -9.0, 
                          -9.0,  3.0, -2.0,  5.0,  2.0, -2.0, -7.0, -1.0)
    val g = doubleArrayOf(24.0,  75.0, 71.0, -34.0,  3.0,  22.0, -45.0, 
                          23.0, 245.0, 25.0,  52.0, 25.0, -67.0, -96.0,
                          96.0,  31.0, 55.0,  36.0, 29.0, -43.0,  -7.0)
    println("${h.map { it.toInt() }}")
    println("${deconv(g, f).map { it.toInt() }}")
    println()
    println("${f.map { it.toInt() }}")
    println("${deconv(g, h).map { it.toInt() }}")
}
```


{{out}}

```txt

[-8, -9, -3, -1, -6, 7]
[-8, -9, -3, -1, -6, 7]

[-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]
[-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]

```



## Lua

Using metatables:

```lua
function deconvolve(f, g)
   local h = setmetatable({}, {__index = function(self, n)
      if n == 1 then self[1] = g[1] / f[1]
      else
         self[n] = g[n]
         for i = 1, n - 1 do
            self[n] = self[n] - self[i] * (f[n - i + 1] or 0)
         end
         self[n] = self[n] / f[1]
      end
      return self[n]
   end})
   local _ = h[#g - #f + 1]
   return setmetatable(h, nil)
end
```


Tests:

```lua

local f = {-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1}
local g = {24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7}
local h = {-8,-9,-3,-1,-6,7}
print(unpack(deconvolve(f, g))) --> -8  -9  -3  -1  -6   7
print(unpack(deconvolve(h, g))) --> -3  -6  -1   8  -6   3  -1  -9  -9   3  -2   5   2  -2  -7  -1
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
This function creates a sparse array for the A matrix and then solves it with a built-in function. It may fail for overdetermined systems, though. Fast approximate methods for deconvolution are also built into Mathematica. See [[Deconvolution/2D%2B]]

```Mathematica

deconv[f_List, g_List] := 
 Module[{A = 
    SparseArray[
     Table[Band[{n, 1}] -> f[[n]], {n, 1, Length[f]}], {Length[g], Length[f] - 1}]}, 
  Take[LinearSolve[A, g], Length[g] - Length[f] + 1]]

```

Usage:

```txt

f = {-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1};
g = {24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96, 96, 31, 55, 36, 29, -43, -7};
deconv[f,g]

```

{{out}}

```txt
{-8, -9, -3, -1, -6, 7}
```



## MATLAB

The deconvolution function is built-in to MATLAB as the "deconv(a,b)" function, where "a" and "b" are vectors storing the convolved function values and the values of one of the deconvoluted vectors of "a". 
To test that this operates according to the task spec we can test the criteria above:

```MATLAB>>
 h = [-8,-9,-3,-1,-6,7];
>> g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7];
>> f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1];
>> deconv(g,f)

ans =

   -8.0000   -9.0000   -3.0000   -1.0000   -6.0000    7.0000

>> deconv(g,h)

ans =

    -3    -6    -1     8    -6     3    -1    -9    -9     3    -2     5     2    -2    -7    -1
```


Therefore, "deconv(a,b)" behaves as expected.


## Nim


```Nim
proc deconv(g, f: openArray[float]): seq[float] =
  var h: seq[float] = newSeq[float](len(g) - len(f) + 1)
  for n in 0..<len(h):
    h[n] = g[n]
    var lower: int
    if n >= len(f):
      lower = n - len(f) + 1
    for i in lower..<n:
      h[n] -= h[i] * f[n - i]
    h[n] /= f[0]
  h

let h = [-8'f64, -9, -3, -1, -6, 7]
let f = [-3'f64, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1]
let g = [24'f64, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96, 
         96, 31, 55, 36, 29, -43, -7]
echo $h
echo $deconv(g, f)
echo $f
echo $deconv(g, h)
```

{{out}}

```txt

[-8.0, -9.0, -3.0, -1.0, -6.0, 7.0]
@[-8.0, -9.0, -3.0, -1.0, -6.0, 7.0]
[-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0]
@[-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0]

```



## Perl

Using <code>rref</code> routine from [[Reduced row echelon form#Perl|Reduced row echelon form]] task.
{{trans|Perl 6}}

```perl
use Math::Cartesian::Product;

sub deconvolve {
    our @g; local *g = shift;
    our @f; local *f = shift;
    my(@m,@d);

    my $h = 1 + @g - @f;
    push @m, [(0) x $h, $g[$_]] for 0..$#g;
    for my $j (0..$h-1) {
        for my $k (0..$#f) {
            $m[$j + $k][$j] = $f[$k]
        }
    }
    rref(\@m);
    push @d, @{ $m[$_] }[$h] for 0..$h-1;
    @d;
}

sub convolve {
    our @f; local *f = shift;
    our @h; local *h = shift;
    my @i;
    for my $x (cartesian {@_} [0..$#f], [0..$#h]) {
        push @i, @$x[0]+@$x[1];
    }
    my $cnt = 0;
    my @g = (0) x (@f + @h - 1);
    for my $x (cartesian {@_} [@f], [@h]) {
        $g[$i[$cnt++]] += @$x[0]*@$x[1];
    }
    @g;
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

my @h = qw<-8 -9 -3 -1 -6 7>;
my @f = qw<-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1>;
print '  conv(f,h) = g = ' . join(' ', my @g = convolve(\@f, \@h)) . "\n";
print 'deconv(g,f) = h = ' . join(' ', deconvolve(\@g, \@f)) . "\n";
print 'deconv(g,h) = f = ' . join(' ', deconvolve(\@g, \@h)) . "\n";
```

{{out}}

```txt
  conv(f,h) = g = 24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7
deconv(g,f) = h = -8 -9 -3 -1 -6 7
deconv(g,h) = f = -3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1
```



## Perl 6

{{works with|Rakudo|2018.02}}

Translation of Python, using a modified version of the Reduced Row Echelon Form subroutine <code>rref()</code> from [[Reduced row echelon form#Perl 6|here]].


```perl6
sub deconvolve (@g, @f) {
    my $h = 1 + @g - @f;
    my @m;
    @m[^@g;^$h] >>+=>> 0;
    @m[^@g;$h] >>=<< @g;
    for ^$h -> $j { for @f.kv -> $k, $v { @m[$j + $k][$j] = $v } }
    return rref( @m )[^$h;$h];
}
 
sub convolve (@f, @h) {
    my @g = 0 xx + @f + @h - 1;
    @g[^@f X+ ^@h] >>+=<< (@f X* @h);
    return @g;
}
 
# Reduced Row Echelon Form simultaneous equation solver.
# Can handle over-specified systems of equations.
# (n unknowns in n + m equations)
sub rref ($m is copy) {
    return unless $m;
    my ($lead, $rows, $cols) = 0, +$m, +$m[0];

    # Trim off over specified rows if they exist, for efficiency
    if $rows >= $cols {
        $m = trim_system($m);
        $rows = +$m;
    }

    for ^$rows -> $r {
        $lead < $cols or return $m;
        my $i = $r;
        until $m[$i][$lead] {
            ++$i == $rows or next;
            $i = $r;
            ++$lead == $cols and return $m;
        }
        $m[$i, $r] = $m[$r, $i] if $r != $i;
        my $lv = $m[$r][$lead];
        $m[$r] >>/=>> $lv;
        for ^$rows -> $n {
            next if $n == $r;
            $m[$n] >>-=>> $m[$r] >>*>> ($m[$n][$lead]//0);
        }
        ++$lead;
    }
    return $m;
 
    # Reduce a system of equations to n equations with n unknowns.
    # Looks for an equation with a true value for each position.
    # If it can't find one, assumes that it has already taken one
    # and pushes in the first equation it sees. This assumtion
    # will alway be successful except in some cases where an
    # under-specified system has been supplied, in which case,
    # it would not have been able to reduce the system anyway.
    sub trim_system ($m is rw) {
        my ($vars, @t) = +$m[0]-1, ();
        for ^$vars -> $lead {
            for ^$m -> $row {
                @t.push: | $m.splice( $row, 1 ) and last if $m[$row][$lead];
            }
        }
        while (+@t < $vars) and +$m { @t.push: $m.splice( 0, 1 ) };
        return @t;
    }
}
 
 
my @h = (-8,-9,-3,-1,-6,7);
my @f = (-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1);
my @g = (24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7);
 
 
.say for ~@g, ~convolve(@f, @h),'';
 
.say for ~@h, ~deconvolve(@g, @f),'';
 
.say for ~@f, ~deconvolve(@g, @h),'';
```


{{out}}

```txt

24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7
24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7

-8 -9 -3 -1 -6 7
-8 -9 -3 -1 -6 7

-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1
-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1

```



## Phix

{{trans|D}}

```Phix
function deconv(sequence g, sequence f)
integer lf = length(f)
sequence h = repeat(0,length(g)-lf+1)
    for n = 1 to length(h) do
        atom e = g[n]
        for i=max(n-lf,0) to n-2 do
            e -= h[i+1] * f[n-i]
        end for
        h[n] = e/f[1]
    end for
    return h
end function
 
constant h = {-8,-9,-3,-1,-6,7}
constant f = {-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1}
constant g = {24,75,71,-34,3,22,-45,23,245,25,52,25,-67,
                   -96,96,31,55,36,29,-43,-7}
sequence r
r = deconv(g, f)    ?{r==h,r}
r = deconv(g, h)    ?{r==f,r}
```

{{out}}

```txt

{1,{-8,-9,-3,-1,-6,7}}
{1,{-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1}}

```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de deconv (G F)
   (let A (pop 'F)
      (make
         (for (N . H) (head (- (length F)) G)
            (for (I . M) (made)
               (dec 'H
                  (*/ M (get F (- N I)) 1.0) ) )
            (link (*/ H 1.0 A)) ) ) ) )
```

Test:

```PicoLisp
(setq
   F (-3. -6. -1. 8. -6. 3. -1. -9. -9. 3. -2. 5. 2. -2. -7. -1.)
   G (24. 75. 71. -34. 3. 22. -45. 23. 245. 25. 52. 25. -67. -96. 96. 31. 55. 36. 29. -43. -7.)
   H (-8. -9. -3. -1. -6. 7.) )

(test H (deconv G F))
(test F (deconv G H))
```



## Python

{{works with|Python|3.x}}

Inspired by the TCL solution, and using the <code>ToReducedRowEchelonForm</code> function to reduce to row echelon form from [[Reduced row echelon form#Python|here]]

```python
def ToReducedRowEchelonForm( M ):
    if not M: return
    lead = 0
    rowCount = len(M)
    columnCount = len(M[0])
    for r in range(rowCount):
        if lead >= columnCount:
            return
        i = r
        while M[i][lead] == 0:
            i += 1
            if i == rowCount:
                i = r
                lead += 1
                if columnCount == lead:
                    return
        M[i],M[r] = M[r],M[i]
        lv = M[r][lead]
        M[r] = [ mrx / lv for mrx in M[r]]
        for i in range(rowCount):
            if i != r:
                lv = M[i][lead]
                M[i] = [ iv - lv*rv for rv,iv in zip(M[r],M[i])]
        lead += 1
    return M
 
def pmtx(mtx):
    print ('\n'.join(''.join(' %4s' % col for col in row) for row in mtx))
 
def convolve(f, h):
    g = [0] * (len(f) + len(h) - 1)
    for hindex, hval in enumerate(h):
        for findex, fval in enumerate(f):
            g[hindex + findex] += fval * hval
    return g

def deconvolve(g, f):
    lenh = len(g) - len(f) + 1
    mtx = [[0 for x in range(lenh+1)] for y in g]
    for hindex in range(lenh):
        for findex, fval in enumerate(f):
            gindex = hindex + findex
            mtx[gindex][hindex] = fval
    for gindex, gval in enumerate(g):        
        mtx[gindex][lenh] = gval
    ToReducedRowEchelonForm( mtx )
    return [mtx[i][lenh] for i in range(lenh)]  # h

if __name__ == '__main__':
    h = [-8,-9,-3,-1,-6,7]
    f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1]
    g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7]
    assert convolve(f,h) == g
    assert deconvolve(g, f) == h
```


Based on the R version.


```python


import numpy

h = [-8,-9,-3,-1,-6,7]
f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1]
g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7]

# https://stackoverflow.com/questions/14267555/find-the-smallest-power-of-2-greater-than-n-in-python

def shift_bit_length(x):
    return 1<<(x-1).bit_length()

def conv(a, b):
    p = len(a)
    q = len(b)
    n = p + q - 1
    r = shift_bit_length(n)
    y = numpy.fft.ifft(numpy.fft.fft(a,r) * numpy.fft.fft(b,r),r)
    return numpy.trim_zeros(numpy.around(numpy.real(y),decimals=6))

def deconv(a, b):
    p = len(a)
    q = len(b)
    n = p - q + 1
    r = shift_bit_length(max(p, q))
    y = numpy.fft.ifft(numpy.fft.fft(a,r) / numpy.fft.fft(b,r), r)
    return numpy.trim_zeros(numpy.around(numpy.real(y),decimals=6))
    
# should return g
   
print(conv(h,f))

# should return h

print(deconv(g,f))

# should return f

print(deconv(g,h))


```


Output


```txt

[ 24.  75.  71. -34.   3.  22. -45.  23. 245.  25.  52.  25. -67. -96.
  96.  31.  55.  36.  29. -43.  -7.]
[-8. -9. -3. -1. -6.  7.]
[-3. -6. -1.  8. -6.  3. -1. -9. -9.  3. -2.  5.  2. -2. -7. -1.]

```



## R


Here we won't solve the system but use the FFT instead.
The method :
* extend vector arguments so that they are the same length, a power of 2 larger than the length of the solution,
* solution is ifft(fft(a)*fft(b)), truncated.


```R
conv <- function(a, b) {
	p <- length(a)
	q <- length(b)
	n <- p + q - 1
	r <- nextn(n, f=2)
	y <- fft(fft(c(a, rep(0, r-p))) * fft(c(b, rep(0, r-q))), inverse=TRUE)/r
	y[1:n]
}

deconv <- function(a, b) {
	p <- length(a)
	q <- length(b)
	n <- p - q + 1
	r <- nextn(max(p, q), f=2)
	y <- fft(fft(c(a, rep(0, r-p))) / fft(c(b, rep(0, r-q))), inverse=TRUE)/r
	return(y[1:n])
}

```


To check :


```R

h <- c(-8,-9,-3,-1,-6,7)
f <- c(-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1)
g <- c(24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7)

max(abs(conv(f,h) - g))
max(abs(deconv(g,f) - h))
max(abs(deconv(g,h) - f))

```


This solution often introduces complex numbers, with null or tiny imaginary part. If it hurts in applications, type Re(conv(f,h)) and Re(deconv(g,h)) instead, to return only the real part. It's not hard-coded in the functions, since they may be used for complex arguments as well.


R has also a function convolve,

```R

conv(a, b) == convolve(a, rev(b), type="open")

```



## Racket


```racket

#lang racket
(require math/matrix)
(define T matrix-transpose)

(define (convolution-matrix f m n)
  (define l (matrix-num-rows f))
  (for*/matrix m n ([i (in-range 0 m)] [j (in-range 0 n)])
      (cond [(or  (< i j) (>= i (+ j l)))  0]
            [(matrix-ref f (- i j) 0)])))
 
(define (least-square X y)
  (matrix-solve (matrix* (T X) X) (matrix* (T X) y)))

(define (deconvolve g f)
  (define lg (matrix-num-rows g))
  (define lf (matrix-num-rows f))
  (define lh (+ (- lg lf) 1))
  (least-square (convolution-matrix f lg lh) g))

```

Test:

```racket

(define f (col-matrix [-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1]))
(define h (col-matrix [-8 -9 -3 -1 -6 7]))
(define g (col-matrix [24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7]))

(deconvolve g f)
(deconvolve g h)

```

{{out}}

```racket

#<array '#(6 1) #[-8 -9 -3 -1 -6 7]>
#<array '#(16 1) #[-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1]>

```



## REXX


```rexx
/*REXX pgm performs deconvolution of two arrays:    deconv(g,f)=h   and   deconv(g,h)=f */
call make@ 'H', "-8 -9 -3 -1 -6 7"
call make@ 'F', "-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1"
call make@ 'G', "24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7"
call show@ 'H'                                   /*display the elements of array  H.    */
call show@ 'F'                                   /*   "     "     "      "   "    F.    */
call show@ 'G'                                   /*   "     "     "      "   "    G.    */
call deco@ 'G', "F", 'X'                         /*deconvolution of  G  and  F  ───►  X */
call test@ 'X', "H"                              /*test: is array  H  equal to array  X?*/
call deco@ 'G', "H", 'Y'                         /*deconvolution of  G  and  H  ───►  Y */
call test@ 'F', "Y"                              /*test: is array  F  equal to array  Y?*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
deco@: parse arg $1,$2,$r;    b=@.$2.# + 1;   a=@.$1.# + 1      /*get sizes of array 1&2*/
       @.$r.#=a - b                                             /*size of return array. */
               do n=0  to a-b                                   /*define  return array. */
               @.$r.n=@.$1.n                                    /*define RETURN element.*/
               if n<b  then L=0                                 /*define the variable L.*/
                       else L=n - b + 1                         /*   "    "     "     " */
               if n>0  then do j=L  to n-1;               _=n-j /*define elements > 0.  */
                            @.$r.n=@.$r.n - @.$r.j * @.$2._     /*compute   "     " "   */
                            end   /*j*/                         /* [↑] subtract product.*/
               @.$r.n=@.$r.n / @.$2.0                           /*divide array element. */
               end   /*n*/;                   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
make@: parse arg $,z;  @.$.#=words(z) - 1                       /*obtain args; set size.*/
                 do k=0  to @.$.#;            @.$.k=word(z,k+1) /*define array element. */
                 end   /*k*/;                 return            /*array starts at unity.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show@: parse arg $,z,_;     do s=0 to @.$.#;  _=strip(_ @.$.s)  /*obtain the arguments. */
                            end   /*s*/                         /* [↑]  build the list. */
       say 'array' $": "_;                    return            /*show the list;  return*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
test@: parse arg $1,$2;     do t=0  to max(@.$1.#, @.$2.#)      /*obtain the arguments. */
                            if @.$1.t=@.$2.t  then iterate      /*create array list.    */
                            say "***error*** arrays"   $1   ' and '   $2   "aren't equal."
                            end   /*t*/;      return            /* [↑]  build the list. */
```

{{out|output|text=  when using the default internal inputs:}}

```txt

array H: -8 -9 -3 -1 -6 7
array F: -3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1
array G: 24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/ENWyl3Z/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/bFag8sS1Qr2Z062LN8dr6A Scastie (remote JVM)].

```Scala
object Deconvolution1D extends App {
  val (h, f) = (Array(-8, -9, -3, -1, -6, 7), Array(-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1))
  val g = Array(24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96, 96, 31, 55, 36, 29, -43, -7)
  val sb = new StringBuilder

  private def deconv(g: Array[Int], f: Array[Int]) = {
    val h = Array.ofDim[Int](g.length - f.length + 1)

    for (n <- h.indices) {
      h(n) = g(n)
      for (i <- math.max(n - f.length + 1, 0) until n) h(n) -= h(i) * f(n - i)
      h(n) /= f(0)
    }
    h
  }

  sb.append(s"h = ${h.mkString("[", ", ", "]")}\n")
    .append(s"deconv(g, f) = ${deconv(g, f).mkString("[", ", ", "]")}\n")
    .append(s"f = ${f.mkString("[", ", ", "]")}\n")
    .append(s"deconv(g, h) = ${deconv(g, h).mkString("[", ", ", "]")}")
  println(sb.result())

}
```


## Tcl

{{works with|Tcl|8.5}}
This builds the a command, <code>1D</code>, with two subcommands (<code>convolve</code> and <code>deconvolve</code>) for performing convolution and deconvolution of these kinds of arrays. The deconvolution code is based on a reduction to [[Reduced row echelon form#Tcl|reduced row echelon form]].

```tcl
package require Tcl 8.5
namespace eval 1D {
    namespace ensemble create;   # Will be same name as namespace
    namespace export convolve deconvolve
    # Access core language math utility commands
    namespace path {::tcl::mathfunc ::tcl::mathop}

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

    # How to apply a 1D convolution of two "functions"
    proc convolve {f h} {
	set g [lrepeat [+ [llength $f] [llength $h] -1] 0]
	set fi -1
	foreach fv $f {
	    incr fi
	    set hi -1
	    foreach hv $h {
		set gi [+ $fi [incr hi]]
		lset g $gi [+ [lindex $g $gi] [* $fv $hv]]
	    }
	}
	return $g
    }

    # How to apply a 1D deconvolution of two "functions"
    proc deconvolve {g f} {
	# Compute the length of the result vector
	set hlen [- [llength $g] [llength $f] -1]

	# Build a matrix of equations to solve
	set matrix {}
	set i -1
	foreach gv $g {
	    lappend matrix [list {*}[lrepeat $hlen 0] $gv]
	    set j [incr i]
	    foreach fv $f {
		if {$j < 0} {
		    break
		} elseif {$j < $hlen} {
		    lset matrix $i $j $fv
		}
		incr j -1
	    }
	}

	# Convert to RREF, solving the system of simultaneous equations
	set reduced [toRREF $matrix]

	# Extract the deconvolution from the last column of the reduced matrix
	for {set i 0} {$i<$hlen} {incr i} {
	    lappend result [lindex $reduced $i end]
	}
	return $result
    }
}
```

To use the above code, a simple demonstration driver (which solves the specific task):

```tcl
# Simple pretty-printer
proc pp {name nlist} {
    set sep ""
    puts -nonewline "$name = \["
    foreach n $nlist {
	puts -nonewline [format %s%g $sep $n]
	set sep ,
    }
    puts "\]"
}

set h {-8 -9 -3 -1 -6 7}
set f {-3 -6 -1 8 -6 3 -1 -9 -9 3 -2 5 2 -2 -7 -1}
set g {24 75 71 -34 3 22 -45 23 245 25 52 25 -67 -96 96 31 55 36 29 -43 -7}

pp "deconv(g,f) = h" [1D deconvolve $g $f]
pp "deconv(g,h) = f" [1D deconvolve $g $h]
pp "  conv(f,h) = g" [1D convolve $f $h]
```

{{out}}

```txt
deconv(g,f) = h = [-8,-9,-3,-1,-6,7]
deconv(g,h) = f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1]
  conv(f,h) = g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7]
```



## Ursala

The user defined function <code>band</code> constructs the required
matrix as a list of lists given the pair of sequences to be
deconvolved, and the [http://www.netlib.org/lapack/lug/node27.html <code>lapack..dgelsd</code>] function solves the system. Some other library functions used are <code>zipt</code> (zipping two unequal length
lists by truncating the longer one) <code>zipp0</code> (zipping unequal length lists by padding the
shorter with zeros) and <code>pad0</code> (making a list of lists all
the same length by appending zeros to the short ones).


```Ursala
#import std
#import nat

band = pad0+ ~&rSS+ zipt^*D(~&r,^lrrSPT/~&ltK33tx zipt^/~&r ~&lSNyCK33+ zipp0)^/~&rx ~&B->NlNSPC ~&bt

deconv = lapack..dgelsd^\~&l ~&||0.!**+ band

```

test program:

```Ursala
h = <-8.,-9.,-3.,-1.,-6.,7.>
f = <-3.,-6.,-1.,8.,-6.,3.,-1.,-9.,-9.,3.,-2.,5.,2.,-2.,-7.,-1.>
g = <24.,75.,71.,-34.,3.,22.,-45.,23.,245.,25.,52.,25.,-67.,-96.,96.,31.,55.,36.,29.,-43.,-7.>

#cast %eLm

test =

<
   'h': deconv(g,f),
   'f': deconv(g,h)>

```

{{out}}

```txt

<
   'h': <
      -8.000000e+00,
      -9.000000e+00,
      -3.000000e+00,
      -1.000000e+00,
      -6.000000e+00,
      7.000000e+00>,
   'f': <
      -3.000000e+00,
      -6.000000e+00,
      -1.000000e+00,
      8.000000e+00,
      -6.000000e+00,
      3.000000e+00,
      -1.000000e+00,
      -9.000000e+00,
      -9.000000e+00,
      3.000000e+00,
      -2.000000e+00,
      5.000000e+00,
      2.000000e+00,
      -2.000000e+00,
      -7.000000e+00,
      -1.000000e+00>>

```



## zkl

Using GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
fcn dconv1D(f,g){
   fsz,hsz:=f.len(), g.len() - fsz +1;
   A:=GSL.Matrix(g.len(),hsz);
   foreach n,fn in ([0..].zip(f)){ foreach rc in (hsz){ A[rc+n,rc]=fn } }
   h:=A.AxEQb(g);
   h
}
```


```zkl
f:=GSL.VectorFromData(-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1);
g:=GSL.VectorFromData(24,75,71,-34,3,22,-45,23,245,25,52,25,-67,-96,96,31,55,36,29,-43,-7);
h:=dconv1D(f,g);
h.format().println();

f:=dconv1D(h,g);
f.format().println();
```

{{out}}

```txt

-8.00,-9.00,-3.00,-1.00,-6.00,7.00
-3.00,-6.00,-1.00,8.00,-6.00,3.00,-1.00,-9.00,-9.00,3.00,-2.00,5.00,2.00,-2.00,-7.00,-1.00

```

Or, using lists:
{{trans|D}}

```zkl
fcn deconv(g,f){
   flen, glen, delta:=f.len(), g.len(), glen - flen + 1;
   result:=List.createLong(delta); // allocate list with space for items
   foreach n in (delta){
      e:=g[n];
      lowerBound:=(if (n>=flen) n - flen + 1 else 0);
      foreach i in ([lowerBound .. n-1]){ e-=result[i]*f[n - i]; }
      result.append(e/f[0]);
    }
    result;
}
```


```zkl
h:=T(-8,-9,-3,-1,-6,7);
f:=T(-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1);
g:=T(24,75,71,-34,3,22,-45,23,245,25,52,25,-67,
                   -96,96,31,55,36,29,-43,-7);
println(deconv(g, f) == h, " ", deconv(g, f));
println(deconv(g, h) == f, " ", deconv(g, h));
```

{{out}}

```txt

True L(-8,-9,-3,-1,-6,7)
True L(-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1)

```

