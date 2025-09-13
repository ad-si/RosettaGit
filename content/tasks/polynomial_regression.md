+++
title = "Polynomial regression"
description = ""
date = 2019-09-20T17:31:59Z
aliases = []
[extra]
id = 2436
[taxonomies]
categories = ["task", "Mathematical operations|Matrices"]
tags = []
+++

## Task

Find an approximating polynomial of known degree for a given data.

Example:
For input data:
 x = {0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10};
 y = {1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321};
The approximating polynomial is:
 3 x<sup>2</sup> + 2 x + 1
Here, the polynomial's coefficients are (3, 2, 1).

This task is intended as a subtask for [[Measure relative performance of sorting algorithms implementations]].



## Ada


```ada
with Ada.Numerics.Real_Arrays;  use Ada.Numerics.Real_Arrays;

function Fit (X, Y : Real_Vector; N : Positive) return Real_Vector is
   A : Real_Matrix (0..N, X'Range);  -- The plane
begin
   for I in A'Range (2) loop
      for J in A'Range (1) loop
         A (J, I) := X (I)**J;
      end loop;
   end loop;
   return Solve (A * Transpose (A), A * Y);
end Fit;
```

The function Fit implements least squares approximation of a function defined in the points as specified by the arrays ''x''<sub>''i''</sub> and ''y''<sub>''i''</sub>. The basis &phi;<sub>''j''</sub> is ''x''<sup>''j''</sup>, ''j''=0,1,..,''N''. The implementation is straightforward. First the plane matrix A is created. A<sub>ji</sub>=&phi;<sub>''j''</sub>(''x''<sub>''i''</sub>). Then the linear problem AA<sup>''T''</sup>''c''=A''y'' is solved. The result ''c''<sub>''j''</sub> are the coefficients. Constraint_Error is propagated when dimensions of X and Y differ or else when the problem is ill-defined.

### Example


```ada
with Fit;
with Ada.Float_Text_IO;  use Ada.Float_Text_IO;

procedure Fitting is
   C : constant Real_Vector :=
          Fit
          (  (0.0, 1.0,  2.0,  3.0,  4.0,  5.0,   6.0,   7.0,   8.0,   9.0,  10.0),
             (1.0, 6.0, 17.0, 34.0, 57.0, 86.0, 121.0, 162.0, 209.0, 262.0, 321.0),
             2
          );
begin
   Put (C (0), Aft => 3, Exp => 0);
   Put (C (1), Aft => 3, Exp => 0);
   Put (C (2), Aft => 3, Exp => 0);
end Fitting;
```

```txt

 1.000 2.000 3.000

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}} <!-- with version's of a68g up to mk18 the GSL library has problems with VEC and MAT with LWB 0 , and transposes of non square MAT, hence we include source code of replacement OPerators here -->

<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has no FORMATted transput}} -->

```algol68
MODE FIELD = REAL;

MODE
  VEC = [0]FIELD,
  MAT = [0,0]FIELD;

PROC VOID raise index error := VOID: (
  print(("stop", new line));
  stop
);

COMMENT from http://rosettacode.org/wiki/Matrix_Transpose#ALGOL_68 END COMMENT
OP ZIP = ([,]FIELD in)[,]FIELD:(
  [2 LWB in:2 UPB in,1 LWB in:1UPB in]FIELD out;
  FOR i FROM LWB in TO UPB in DO
     out[,i]:=in[i,]
  OD;
  out
);

COMMENT from http://rosettacode.org/wiki/Matrix_multiplication#ALGOL_68 END COMMENT
OP * = (VEC a,b)FIELD: ( # basically the dot product #
    FIELD result:=0;
    IF LWB a/=LWB b OR UPB a/=UPB b THEN raise index error FI;
    FOR i FROM LWB a TO UPB a DO result+:= a[i]*b[i] OD;
    result
  );

OP * = (VEC a, MAT b)VEC: ( # overload vector times matrix #
    [2 LWB b:2 UPB b]FIELD result;
    IF LWB a/=LWB b OR UPB a/=UPB b THEN raise index error FI;
    FOR j FROM 2 LWB b TO 2 UPB b DO result[j]:=a*b[,j] OD;
    result
  );

OP * = (MAT a, b)MAT: ( # overload matrix times matrix #
     [LWB a:UPB a, 2 LWB b:2 UPB b]FIELD result;
     IF 2 LWB a/=LWB b OR 2 UPB a/=UPB b THEN raise index error FI;
     FOR k FROM LWB result TO UPB result DO result[k,]:=a[k,]*b OD;
     result
   );

COMMENT from http://rosettacode.org/wiki/Pyramid_of_numbers#ALGOL_68 END COMMENT
OP / = (VEC a, MAT b)VEC: ( # vector division #
  [LWB a:UPB a,1]FIELD transpose a;
  transpose a[,1]:=a;
  (transpose a/b)[,1]
);

OP / = (MAT a, MAT b)MAT:( # matrix division #
  [LWB b:UPB b]INT p ;
  INT sign;
  [,]FIELD lu = lu decomp(b, p, sign);
  [LWB a:UPB a, 2 LWB a:2 UPB a]FIELD out;
  FOR col FROM 2 LWB a TO 2 UPB a DO
    out[,col] := lu solve(b, lu, p, a[,col]) [@LWB out[,col]]
  OD;
  out
);

FORMAT int repr = $g(0)$,
       real repr = $g(-7,4)$;

PROC fit =  (VEC x, y, INT order)VEC:
BEGIN
   [0:order, LWB x:UPB x]FIELD a;  # the plane #
   FOR i FROM 2 LWB a TO 2 UPB a  DO
      FOR j FROM LWB a TO UPB a DO
         a [j, i] := x [i]**j
      OD
   OD;
   ( y * ZIP a ) / ( a * ZIP a )
END # fit #;

PROC print polynomial = (VEC x)VOID: (
   BOOL empty := TRUE;
   FOR i FROM UPB x BY -1 TO LWB x DO
     IF x[i] NE 0 THEN
       IF x[i] > 0 AND NOT empty THEN print ("+") FI;
       empty := FALSE;
       IF x[i] NE 1 OR i=0 THEN
         IF ENTIER x[i] = x[i] THEN
           printf((int repr, x[i]))
         ELSE
           printf((real repr, x[i]))
         FI
       FI;
       CASE i+1 IN
         SKIP,print(("x"))
       OUT
         printf(($"x**"g(0)$,i))
       ESAC
     FI
   OD;
   IF empty THEN print("0") FI;
   print(new line)
);

fitting: BEGIN
   VEC c =
          fit
          (  (0.0, 1.0,  2.0,  3.0,  4.0,  5.0,   6.0,   7.0,   8.0,   9.0,  10.0),
             (1.0, 6.0, 17.0, 34.0, 57.0, 86.0, 121.0, 162.0, 209.0, 262.0, 321.0),
             2
          );
   print polynomial(c);
   VEC d =
          fit
          ( (0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
            (2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0),
            2
          );
   print polynomial(d)
END # fitting #
```

```txt

3x**2+2x+1
 1.0848x**2+10.3552x-0.6164

```



## BBC BASIC

The code listed below is good for up to 10000 data points
and fits an order-5 polynomial, so the test data for this task
is hardly challenging!

```bbcbasic
      INSTALL @lib$+"ARRAYLIB"

      Max% = 10000
      DIM vector(5), matrix(5,5)
      DIM x(Max%), x2(Max%), x3(Max%), x4(Max%), x5(Max%)
      DIM x6(Max%), x7(Max%), x8(Max%), x9(Max%), x10(Max%)
      DIM y(Max%), xy(Max%), x2y(Max%), x3y(Max%), x4y(Max%), x5y(Max%)

      npts% = 11
      x() = 0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10
      y() = 1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321

      sum_x = SUM(x())
      x2()  = x() * x()   : sum_x2  = SUM(x2())
      x3()  = x() * x2()  : sum_x3  = SUM(x3())
      x4()  = x2() * x2() : sum_x4  = SUM(x4())
      x5()  = x2() * x3() : sum_x5  = SUM(x5())
      x6()  = x3() * x3() : sum_x6  = SUM(x6())
      x7()  = x3() * x4() : sum_x7  = SUM(x7())
      x8()  = x4() * x4() : sum_x8  = SUM(x8())
      x9()  = x4() * x5() : sum_x9  = SUM(x9())
      x10() = x5() * x5() : sum_x10 = SUM(x10())

      sum_y = SUM(y())
      xy()  = x() * y()   : sum_xy  = SUM(xy())
      x2y() = x2() * y()  : sum_x2y = SUM(x2y())
      x3y() = x3() * y()  : sum_x3y = SUM(x3y())
      x4y() = x4() * y()  : sum_x4y = SUM(x4y())
      x5y() = x5() * y()  : sum_x5y = SUM(x5y())

      matrix() = \
      \ npts%,  sum_x,   sum_x2,  sum_x3,  sum_x4,  sum_x5, \
      \ sum_x,  sum_x2,  sum_x3,  sum_x4,  sum_x5,  sum_x6, \
      \ sum_x2, sum_x3,  sum_x4,  sum_x5,  sum_x6,  sum_x7, \
      \ sum_x3, sum_x4,  sum_x5,  sum_x6,  sum_x7,  sum_x8, \
      \ sum_x4, sum_x5,  sum_x6,  sum_x7,  sum_x8,  sum_x9, \
      \ sum_x5, sum_x6,  sum_x7,  sum_x8,  sum_x9,  sum_x10

      vector() = \
      \ sum_y,  sum_xy,  sum_x2y, sum_x3y, sum_x4y, sum_x5y

      PROC_invert(matrix())
      vector() = matrix().vector()

      @% = &2040A
      PRINT "Polynomial coefficients = "
      FOR term% = 5 TO 0 STEP -1
        PRINT ;vector(term%) " * x^" STR$(term%)
      NEXT
```

```txt

Polynomial coefficients =
0.0000 * x^5
-0.0000 * x^4
0.0002 * x^3
2.9993 * x^2
2.0012 * x^1
0.9998 * x^0

```



## C

'''Include''' file (to make the code reusable easily) named <tt>polifitgsl.h</tt>

```c
#ifndef _POLIFITGSL_H
#define _POLIFITGSL_H
#include <gsl/gsl_multifit.h>
#include <stdbool.h>
#include <math.h>
bool polynomialfit(int obs, int degree,
		   double *dx, double *dy, double *store); /* n, p */
#endif
```

'''Implementation''' (the examples [http://www.gnu.org/software/gsl/manual/html_node/Fitting-Examples.html here] helped alot to code this quickly):

```c
#include "polifitgsl.h"

bool polynomialfit(int obs, int degree,
		   double *dx, double *dy, double *store) /* n, p */
{
  gsl_multifit_linear_workspace *ws;
  gsl_matrix *cov, *X;
  gsl_vector *y, *c;
  double chisq;

  int i, j;

  X = gsl_matrix_alloc(obs, degree);
  y = gsl_vector_alloc(obs);
  c = gsl_vector_alloc(degree);
  cov = gsl_matrix_alloc(degree, degree);

  for(i=0; i < obs; i++) {
    for(j=0; j < degree; j++) {
      gsl_matrix_set(X, i, j, pow(dx[i], j));
    }
    gsl_vector_set(y, i, dy[i]);
  }

  ws = gsl_multifit_linear_alloc(obs, degree);
  gsl_multifit_linear(X, y, c, cov, &chisq, ws);

  /* store result ... */
  for(i=0; i < degree; i++)
  {
    store[i] = gsl_vector_get(c, i);
  }

  gsl_multifit_linear_free(ws);
  gsl_matrix_free(X);
  gsl_matrix_free(cov);
  gsl_vector_free(y);
  gsl_vector_free(c);
  return true; /* we do not "analyse" the result (cov matrix mainly)
		  to know if the fit is "good" */
}
```

'''Testing''':

```c
#include <stdio.h>

#include "polifitgsl.h"

#define NP 11
double x[] = {0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10};
double y[] = {1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321};

#define DEGREE 3
double coeff[DEGREE];

int main()
{
  int i;

  polynomialfit(NP, DEGREE, x, y, coeff);
  for(i=0; i < DEGREE; i++) {
    printf("%lf\n", coeff[i]);
  }
  return 0;
}
```

```txt
1.000000
2.000000
3.000000
```



## C++

```cpp
#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>

void polyRegression(const std::vector<int>& x, const std::vector<int>& y) {
    int n = x.size();
    std::vector<int> r(n);
    std::iota(r.begin(), r.end(), 0);
    double xm = std::accumulate(x.begin(), x.end(), 0.0) / x.size();
    double ym = std::accumulate(y.begin(), y.end(), 0.0) / y.size();
    double x2m = std::transform_reduce(r.begin(), r.end(), 0.0, std::plus<double>{}, [](double a) {return a * a; }) / r.size();
    double x3m = std::transform_reduce(r.begin(), r.end(), 0.0, std::plus<double>{}, [](double a) {return a * a * a; }) / r.size();
    double x4m = std::transform_reduce(r.begin(), r.end(), 0.0, std::plus<double>{}, [](double a) {return a * a * a * a; }) / r.size();

    double xym = std::transform_reduce(x.begin(), x.end(), y.begin(), 0.0, std::plus<double>{}, std::multiplies<double>{});
    xym /= fmin(x.size(), y.size());

    double x2ym = std::transform_reduce(x.begin(), x.end(), y.begin(), 0.0, std::plus<double>{}, [](double a, double b) { return a * a * b; });
    x2ym /= fmin(x.size(), y.size());

    double sxx = x2m - xm * xm;
    double sxy = xym - xm * ym;
    double sxx2 = x3m - xm * x2m;
    double sx2x2 = x4m - x2m * x2m;
    double sx2y = x2ym - x2m * ym;

    double b = (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
    double c = (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
    double a = ym - b * xm - c * x2m;

    auto abc = [a, b, c](int xx) {
        return a + b * xx + c * xx*xx;
    };

    std::cout << "y = " << a << " + " << b << "x + " << c << "x^2" << std::endl;
    std::cout << " Input  Approximation" << std::endl;
    std::cout << " x   y     y1" << std::endl;

    auto xit = x.cbegin();
    auto xend = x.cend();
    auto yit = y.cbegin();
    auto yend = y.cend();
    while (xit != xend && yit != yend) {
        printf("%2d %3d  %5.1f\n", *xit, *yit, abc(*xit));
        xit = std::next(xit);
        yit = std::next(yit);
    }
}

int main() {
    using namespace std;

    vector<int> x(11);
    iota(x.begin(), x.end(), 0);

    vector<int> y{ 1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321 };

    polyRegression(x, y);

    return 0;
}
```

```txt
y = 1 + 2x + 3x^2
 Input  Approximation
 x   y     y1
 0   1    1.0
 1   6    6.0
 2  17   17.0
 3  34   34.0
 4  57   57.0
 5  86   86.0
 6 121  121.0
 7 162  162.0
 8 209  209.0
 9 262  262.0
10 321  321.0
```


## C#
```c#
        public static double[] Polyfit(double[] x, double[] y, int degree)
        {
            // Vandermonde matrix
            var v = new DenseMatrix(x.Length, degree + 1);
            for (int i = 0; i < v.RowCount; i++)
                for (int j = 0; j <= degree; j++) v[i, j] = Math.Pow(x[i], j);
            var yv = new DenseVector(y).ToColumnMatrix();
            QR qr = v.QR();
            // Math.Net doesn't have an "economy" QR, so:
            // cut R short to square upper triangle, then recompute Q
            var r = qr.R.SubMatrix(0, degree + 1, 0, degree + 1);
            var q = v.Multiply(r.Inverse());
            var p = r.Inverse().Multiply(q.TransposeThisAndMultiply(yv));
            return p.Column(0).ToArray();
        }
```

Example:

```C sharp
        static void Main(string[] args)
        {
            const int degree = 2;
            var x = new[] {0.0, 1.0,  2.0,  3.0,  4.0,  5.0,   6.0,   7.0,   8.0,   9.0,  10.0};
            var y = new[] {1.0, 6.0, 17.0, 34.0, 57.0, 86.0, 121.0, 162.0, 209.0, 262.0, 321.0};
            var p = Polyfit(x, y, degree);
            foreach (var d in p) Console.Write("{0} ",d);
            Console.WriteLine();
            for (int i = 0; i < x.Length; i++ )
                Console.WriteLine("{0} => {1} diff {2}", x[i], Polyval(p,x[i]), y[i] - Polyval(p,x[i]));
            Console.ReadKey(true);
        }
```



## Common Lisp

Uses the routine (lsqr A b) from [[Multiple regression]] and (mtp A) from [[Matrix transposition]].


```lisp
;; Least square fit of a polynomial of order n the x-y-curve.
(defun polyfit (x y n)
  (let* ((m (cadr (array-dimensions x)))
         (A (make-array `(,m ,(+ n 1)) :initial-element 0)))
    (loop for i from 0 to (- m 1) do
          (loop for j from 0 to n do
                (setf (aref A i j)
                      (expt (aref x 0 i) j))))
    (lsqr A (mtp y))))
```


Example:


```lisp
(let ((x (make-array '(1 11) :initial-contents '((0 1 2 3 4 5 6 7 8 9 10))))
      (y (make-array '(1 11) :initial-contents '((1 6 17 34 57 86 121 162 209 262 321)))))
  (polyfit x y 2))

#2A((0.9999999999999759d0) (2.000000000000005d0) (3.0d0))
```



## D

```D
import std.algorithm;
import std.range;
import std.stdio;

auto average(R)(R r) {
    auto t = r.fold!("a+b", "a+1")(0, 0);
    return cast(double) t[0] / t[1];
}

void polyRegression(int[] x, int[] y) {
    auto n = x.length;
    auto r = iota(0, n).array;
    auto xm = x.average();
    auto ym = y.average();
    auto x2m = r.map!"a*a".average();
    auto x3m = r.map!"a*a*a".average();
    auto x4m = r.map!"a*a*a*a".average();
    auto xym = x.zip(y).map!"a[0]*a[1]".average();
    auto x2ym = x.zip(y).map!"a[0]*a[0]*a[1]".average();

    auto sxx = x2m - xm * xm;
    auto sxy = xym - xm * ym;
    auto sxx2 = x3m - xm * x2m;
    auto sx2x2 = x4m - x2m * x2m;
    auto sx2y = x2ym - x2m * ym;

    auto b = (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
    auto c = (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
    auto a = ym - b * xm - c * x2m;

    real abc(int xx) {
        return a + b * xx + c * xx * xx;
    }

    writeln("y = ", a, " + ", b, "x + ", c, "x^2");
    writeln(" Input  Approximation");
    writeln(" x   y     y1");
    foreach (i; 0..n) {
        writefln("%2d %3d  %5.1f", x[i], y[i], abc(x[i]));
    }
}

void main() {
    auto x = iota(0, 11).array;
    auto y = [1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321];
    polyRegression(x, y);
}
```

```txt
y = 1 + 2x + 3x^2
 Input  Approximation
 x   y     y1
 0   1    1.0
 1   6    6.0
 2  17   17.0
 3  34   34.0
 4  57   57.0
 5  86   86.0
 6 121  121.0
 7 162  162.0
 8 209  209.0
 9 262  262.0
10 321  321.0
```



## Emacs Lisp


Simple solution by Emacs Lisp and built-in Emacs Calc.

<lang emacs-lisp>
(setq x '[0 1 2 3 4 5 6 7 8 9 10])
(setq y '[1 6 17 34 57 86 121 162 209 262 321])
(calc-eval
 (format "fit(a*x^2+b*x+c,[x],[a,b,c],[%s %s])" x y))

```


```txt

"3. x^2 + 1.99999999996 x + 1.00000000006"

```



## Fortran

```fortran
module fitting
contains

  function polyfit(vx, vy, d)
    implicit none
    integer, intent(in)                   :: d
    integer, parameter                    :: dp = selected_real_kind(15, 307)
    real(dp), dimension(d+1)              :: polyfit
    real(dp), dimension(:), intent(in)    :: vx, vy

    real(dp), dimension(:,:), allocatable :: X
    real(dp), dimension(:,:), allocatable :: XT
    real(dp), dimension(:,:), allocatable :: XTX

    integer :: i, j

    integer     :: n, lda, lwork
    integer :: info
    integer, dimension(:), allocatable :: ipiv
    real(dp), dimension(:), allocatable :: work

    n = d+1
    lda = n
    lwork = n

    allocate(ipiv(n))
    allocate(work(lwork))
    allocate(XT(n, size(vx)))
    allocate(X(size(vx), n))
    allocate(XTX(n, n))

    ! prepare the matrix
    do i = 0, d
       do j = 1, size(vx)
          X(j, i+1) = vx(j)**i
       end do
    end do

    XT  = transpose(X)
    XTX = matmul(XT, X)

    ! calls to LAPACK subs DGETRF and DGETRI
    call DGETRF(n, n, XTX, lda, ipiv, info)
    if ( info /= 0 ) then
       print *, "problem"
       return
    end if
    call DGETRI(n, XTX, lda, ipiv, work, lwork, info)
    if ( info /= 0 ) then
       print *, "problem"
       return
    end if

    polyfit = matmul( matmul(XTX, XT), vy)

    deallocate(ipiv)
    deallocate(work)
    deallocate(X)
    deallocate(XT)
    deallocate(XTX)

  end function

end module
```



### Example


```fortran
program PolynomalFitting
  use fitting
  implicit none

  ! let us test it
  integer, parameter      :: degree = 2
  integer, parameter      :: dp = selected_real_kind(15, 307)
  integer                 :: i
  real(dp), dimension(11) :: x = (/ (i,i=0,10) /)
  real(dp), dimension(11) :: y = (/ 1,   6,  17,  34, &
                                   57,  86, 121, 162, &
                                   209, 262, 321 /)
  real(dp), dimension(degree+1) :: a

  a = polyfit(x, y, degree)

  write (*, '(F9.4)') a

end program
```


{{out}} (lower powers first, so this seems the opposite of the Python output):

```txt

   1.0000
   2.0000
   3.0000

```



## FreeBASIC


```FreeBASIC
Sub GaussJordan(matrix() As Double,rhs() As Double,ans() As Double)
    Dim As Integer n=Ubound(matrix,1)
    Redim ans(0):Redim ans(1 To n)
    Dim As Double b(1 To n,1 To n),r(1 To n)
    For c As Integer=1 To n 'take copies
        r(c)=rhs(c)
        For d As Integer=1 To n
            b(c,d)=matrix(c,d)
        Next d
    Next c
    #macro pivot(num)
    For p1 As Integer  = num To n - 1
        For p2 As Integer  = p1 + 1 To n
            If Abs(b(p1,num))<Abs(b(p2,num)) Then
                Swap r(p1),r(p2)
                For g As Integer=1 To n
                    Swap b(p1,g),b(p2,g)
                Next g
            End If
        Next p2
    Next p1
    #endmacro
    For k As Integer=1 To n-1
        pivot(k)              'full pivoting
        For row As Integer =k To n-1
            If b(row+1,k)=0 Then Exit For
            Var f=b(k,k)/b(row+1,k)
            r(row+1)=r(row+1)*f-r(k)
            For g As Integer=1 To n
                b((row+1),g)=b((row+1),g)*f-b(k,g)
            Next g
        Next row
    Next k
    'back substitute
    For z As Integer=n To 1 Step -1
        ans(z)=r(z)/b(z,z)
        For j As Integer = n To z+1 Step -1
            ans(z)=ans(z)-(b(z,j)*ans(j)/b(z,z))
        Next j
        Next    z
    End Sub

    'Interpolate through points.
    Sub Interpolate(x_values() As Double,y_values() As Double,p() As Double)
        Var n=Ubound(x_values)
        Redim p(0):Redim p(1 To n)
        Dim As Double matrix(1 To n,1 To n),rhs(1 To n)
        For a As Integer=1 To n
            rhs(a)=y_values(a)
            For b As Integer=1 To n
                matrix(a,b)=x_values(a)^(b-1)
            Next b
        Next a
        'Solve the linear equations
        GaussJordan(matrix(),rhs(),p())
    End Sub

 '
### ===================== SET UP THE POINTS ============


    Dim As Double x(1 To ...)={0,1,2,3,4,5,6,7,8,9,10}
    Dim As Double y(1 To ...)={1,6,17,34,57,86,121,162,209,262,321}

    Redim As Double Poly(0)
    'Get the polynomial Poly()
    Interpolate(x(),y(),Poly())

    'print coefficients to console
    print "Polynomial Coefficients:"
    print
    For z As Integer=1 To Ubound(Poly)
        If z=1 Then
            Print "constant term  ";tab(20);Poly(z)
        Else
            Print tab(8); "x^";z-1;" =  ";tab(20);Poly(z)
        End If
    Next z

    sleep
```

```txt
Polynomial Coefficients:

constant term       1
       x^ 1 =       2
       x^ 2 =       3
       x^ 3 =       0
       x^ 4 =       0
       x^ 5 =       0
       x^ 6 =       0
       x^ 7 =       0
       x^ 8 =       0
       x^ 9 =       0
       x^ 10 =      0
```



## GAP


```gap
PolynomialRegression := function(x, y, n)
	local a;
	a := List([0 .. n], i -> List(x, s -> s^i));
	return TransposedMat((a * TransposedMat(a))^-1 * a * TransposedMat([y]))[1];
end;

x := [0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10];
y := [1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321];

# Return coefficients in ascending degree order
PolynomialRegression(x, y, 2);
# [ 1, 2, 3 ]
```



## gnuplot



```gnuplot
# The polynomial approximation
f(x) = a*x**2 + b*x + c

# Initial values for parameters
a = 0.1
b = 0.1
c = 0.1

# Fit f to the following data by modifying the variables a, b, c
fit f(x) '-' via a, b, c
   0   1
   1   6
   2  17
   3  34
   4  57
   5  86
   6 121
   7 162
   8 209
   9 262
  10 321
e

print sprintf("\n --- \n Polynomial fit: %.4f x^2 + %.4f x + %.4f\n", a, b, c)
```



## Go


### Library gonum/matrix


```go
package main

import (
    "fmt"

    "github.com/gonum/matrix/mat64"
)

var (
    x = []float64{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    y = []float64{1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}

    degree = 2
)

func main() {
    a := Vandermonde(x, degree)
    b := mat64.NewDense(len(y), 1, y)
    c := mat64.NewDense(degree+1, 1, nil)

    qr := new(mat64.QR)
    qr.Factorize(a)

    err := c.SolveQR(qr, false, b)
    if err != nil {
        fmt.Println(err)
    } else {
        fmt.Printf("%.3f\n", mat64.Formatted(c))
    }
}

func Vandermonde(a []float64, degree int) *mat64.Dense {
    x := mat64.NewDense(len(a), degree+1, nil)
    for i := range a {
        for j, p := 0, 1.; j <= degree; j, p = j+1, p*a[i] {
            x.Set(i, j, p)
        }
    }
    return x
}
```

```txt

⎡1.000⎤
⎢2.000⎥
⎣3.000⎦

```



### Library go.matrix

Least squares solution using QR decomposition and package [http://github.com/skelterjohn/go.matrix go.matrix].

```go
package main

import (
    "fmt"

    "github.com/skelterjohn/go.matrix"
)

var xGiven = []float64{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
var yGiven = []float64{1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}
var degree = 2

func main() {
    m := len(yGiven)
    n := degree + 1
    y := matrix.MakeDenseMatrix(yGiven, m, 1)
    x := matrix.Zeros(m, n)
    for i := 0; i < m; i++ {
        ip := float64(1)
        for j := 0; j < n; j++ {
            x.Set(i, j, ip)
            ip *= xGiven[i]
        }
    }

    q, r := x.QR()
    qty, err := q.Transpose().Times(y)
    if err != nil {
        fmt.Println(err)
        return
    }
    c := make([]float64, n)
    for i := n - 1; i >= 0; i-- {
        c[i] = qty.Get(i, 0)
        for j := i + 1; j < n; j++ {
            c[i] -= c[j] * r.Get(i, j)
        }
        c[i] /= r.Get(i, i)
    }
    fmt.Println(c)
}
```

{{out}} (lowest order coefficient first)

```txt

[0.9999999999999758 2.000000000000015 2.999999999999999]

```



## Haskell

Uses module Matrix.LU from [http://hackage.haskell.org/package/dsp hackageDB DSP]

```haskell
import Data.List
import Data.Array
import Control.Monad
import Control.Arrow
import Matrix.LU

ppoly p x = map (x**) p

polyfit d ry = elems $ solve mat vec  where
   mat = listArray ((1,1), (d,d)) $ liftM2 concatMap ppoly id [0..fromIntegral $ pred d]
   vec = listArray (1,d) $ take d ry
```

{{out}} in GHCi:

```haskell
*Main> polyfit 3 [1,6,17,34,57,86,121,162,209,262,321]
[1.0,2.0,3.0]
```



## HicEst


```hicest
REAL :: n=10, x(n), y(n), m=3, p(m)

   x = (0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10)
   y = (1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321)

   p = 2 ! initial guess for the polynom's coefficients

   SOLVE(NUL=Theory()-y(nr), Unknown=p, DataIdx=nr, Iters=iterations)

   WRITE(ClipBoard, Name) p, iterations

FUNCTION Theory()
   ! called by the solver of the SOLVE function. All variables are global
   Theory = p(1)*x(nr)^2 + p(2)*x(nr) + p(3)
 END
```

```txt
SOLVE performs a (nonlinear) least-square fit (Levenberg-Marquardt):
p(1)=2.997135145; p(2)=2.011348347; p(3)=0.9906627242; iterations=19;
```



## Hy


```lisp
(import [numpy [polyfit]])

(setv x (range 11))
(setv y [1 6 17 34 57 86 121 162 209 262 321])

(print (polyfit x y 2))
```



## J



```j
   Y=:1 6 17 34 57 86 121 162 209 262 321
   (%. ^/~@x:@i.@#) Y
1 2 3 0 0 0 0 0 0 0 0
```


Note that this implementation does not use floating point numbers,
so we do not introduce floating point errors.
Using exact arithmetic has a speed penalty,
but for small problems like this it is inconsequential.

The above solution fits a polynomial of order 11.
If the order of the polynomial is known to be 3
(as is implied in the task description)
then the following solution is probably preferable:

```j
   Y %. (i.3) ^/~ i.#Y
1 2 3
```

(note that this time we used floating point numbers, so that result is approximate rather than exact - it only looks exact because of how J displays floating point numbers (by default, J assumes six digits of accuracy) - changing (i.3) to (x:i.3) would give us an exact result, if that mattered.)


## Java

```Java
import java.util.Arrays;
import java.util.function.IntToDoubleFunction;
import java.util.stream.IntStream;

public class PolynomialRegression {
    private static void polyRegression(int[] x, int[] y) {
        int n = x.length;
        int[] r = IntStream.range(0, n).toArray();
        double xm = Arrays.stream(x).average().orElse(Double.NaN);
        double ym = Arrays.stream(y).average().orElse(Double.NaN);
        double x2m = Arrays.stream(r).map(a -> a * a).average().orElse(Double.NaN);
        double x3m = Arrays.stream(r).map(a -> a * a * a).average().orElse(Double.NaN);
        double x4m = Arrays.stream(r).map(a -> a * a * a * a).average().orElse(Double.NaN);
        double xym = 0.0;
        for (int i = 0; i < x.length && i < y.length; ++i) {
            xym += x[i] * y[i];
        }
        xym /= Math.min(x.length, y.length);
        double x2ym = 0.0;
        for (int i = 0; i < x.length && i < y.length; ++i) {
            x2ym += x[i] * x[i] * y[i];
        }
        x2ym /= Math.min(x.length, y.length);

        double sxx = x2m - xm * xm;
        double sxy = xym - xm * ym;
        double sxx2 = x3m - xm * x2m;
        double sx2x2 = x4m - x2m * x2m;
        double sx2y = x2ym - x2m * ym;

        double b = (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
        double c = (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
        double a = ym - b * xm - c * x2m;

        IntToDoubleFunction abc = (int xx) -> a + b * xx + c * xx * xx;

        System.out.println("y = " + a + " + " + b + "x + " + c + "x^2");
        System.out.println(" Input  Approximation");
        System.out.println(" x   y     y1");
        for (int i = 0; i < n; ++i) {
            System.out.printf("%2d %3d  %5.1f\n", x[i], y[i], abc.applyAsDouble(x[i]));
        }
    }

    public static void main(String[] args) {
        int[] x = IntStream.range(0, 11).toArray();
        int[] y = new int[]{1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321};
        polyRegression(x, y);
    }
}
```

```txt
y = 1.0 + 2.0x + 3.0x^2
 Input  Approximation
 x   y     y1
 0   1    1.0
 1   6    6.0
 2  17   17.0
 3  34   34.0
 4  57   57.0
 5  86   86.0
 6 121  121.0
 7 162  162.0
 8 209  209.0
 9 262  262.0
10 321  321.0
```



## Julia

The least-squares fit problem for a degree <i>n</i>
can be solved with the built-in backslash operator (coefficients in increasing order of degree):

```julia
polyfit(x::Vector, y::Vector, deg::Int) = collect(v ^ p for v in x, p in 0:deg) \ y

x = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
y = [1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321]
@show polyfit(x, y, 2)
```


```txt
polyfit(x, y, 2) = [1.0, 2.0, 3.0]
```



## Kotlin

```scala
// version 1.1.51

fun polyRegression(x: IntArray, y: IntArray) {
    val n = x.size
    val r = 0 until n
    val xm = x.average()
    val ym = y.average()
    val x2m = r.map { it * it }.average()
    val x3m = r.map { it * it * it }.average()
    val x4m = r.map { it * it * it * it }.average()
    val xym = x.zip(y).map { it.first * it.second }.average()
    val x2ym = x.zip(y).map { it.first * it.first * it.second }.average()

    val sxx = x2m - xm * xm
    val sxy = xym - xm * ym
    val sxx2 = x3m - xm * x2m
    val sx2x2 = x4m - x2m * x2m
    val sx2y = x2ym - x2m * ym

    val b = (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
    val c = (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
    val a = ym - b * xm - c * x2m

    fun abc(xx: Int) = a + b * xx + c * xx * xx

    println("y = $a + ${b}x + ${c}x^2\n")
    println(" Input  Approximation")
    println(" x   y     y1")
    for (i in 0 until n) {
        System.out.printf("%2d %3d  %5.1f\n", x[i], y[i], abc(x[i]))
    }
}

fun main(args: Array<String>) {
    val x = IntArray(11) { it }
    val y = intArrayOf(1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321)
    polyRegression(x, y)
}
```


```txt

y = 1.0 + 2.0x + 3.0x^2

 Input  Approximation
 x   y     y1
 0   1    1.0
 1   6    6.0
 2  17   17.0
 3  34   34.0
 4  57   57.0
 5  86   86.0
 6 121  121.0
 7 162  162.0
 8 209  209.0
 9 262  262.0
10 321  321.0

```



## Lua

```lua
function eval(a,b,c,x)
    return a + (b + c * x) * x
end

function regression(xa,ya)
    local n = #xa

    local xm = 0.0
    local ym = 0.0
    local x2m = 0.0
    local x3m = 0.0
    local x4m = 0.0
    local xym = 0.0
    local x2ym = 0.0

    for i=1,n do
        xm = xm + xa[i]
        ym = ym + ya[i]
        x2m = x2m + xa[i] * xa[i]
        x3m = x3m + xa[i] * xa[i] * xa[i]
        x4m = x4m + xa[i] * xa[i] * xa[i] * xa[i]
        xym = xym + xa[i] * ya[i]
        x2ym = x2ym + xa[i] * xa[i] * ya[i]
    end
    xm = xm / n
    ym = ym / n
    x2m = x2m / n
    x3m = x3m / n
    x4m = x4m / n
    xym = xym / n
    x2ym = x2ym / n

    local sxx = x2m - xm * xm
    local sxy = xym - xm * ym
    local sxx2 = x3m - xm * x2m
    local sx2x2 = x4m - x2m * x2m
    local sx2y = x2ym - x2m * ym

    local b = (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
    local c = (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
    local a = ym - b * xm - c * x2m

    print("y = "..a.." + "..b.."x + "..c.."x^2")

    for i=1,n do
        print(string.format("%2d %3d  %3d", xa[i], ya[i], eval(a, b, c, xa[i])))
    end
end

local xa = {0, 1,  2,  3,  4,  5,   6,   7,   8,   9,  10}
local ya = {1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}
regression(xa, ya)
```

```txt
y = 1 + 2x + 3x^2
 0   1    1
 1   6    6
 2  17   17
 3  34   34
 4  57   57
 5  86   86
 6 121  121
 7 162  162
 8 209  209
 9 262  262
10 321  321
```



## Maple


```Maple
with(CurveFitting);
PolynomialInterpolation([[0, 1], [1, 6], [2, 17], [3, 34], [4, 57], [5, 86], [6, 121], [7, 162], [8, 209], [9, 262], [10, 321]], 'x');

```

Result:

```txt
3*x^2+2*x+1
```



## Mathematica

Using the built-in "Fit" function.


```Mathematica
data = Transpose@{Range[0, 10], {1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}};
Fit[data, {1, x, x^2}, x]
```


Second version: using built-in "InterpolatingPolynomial" function.

```Mathematica
Simplify@InterpolatingPolynomial[{{0, 1}, {1, 6}, {2, 17}, {3, 34}, {4, 57}, {5, 86}, {6, 121}, {7, 162}, {8, 209}, {9, 262}, {10, 321}}, x]
```

Result:

```txt
1 + 2x + 3x^2
```



## MATLAB

Matlab has a built-in function "polyfit(x,y,n)" which performs this task.
The arguments x and y are vectors which are parametrized by the index suck that <math>point_{i} = (x_{i},y_{i})</math> and the argument n is the order of the polynomial you want to fit.
The output of this function is the coefficients of the polynomial which best fit these x,y value pairs.


```MATLAB>>
 x = [0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10];
>> y = [1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321];
>> polyfit(x,y,2)

ans =

   2.999999999999998   2.000000000000019   0.999999999999956
```


=={{header|MK-61/52}}==
Part 1:
<lang>ПC	С/П	ПD	ИП9	+	П9	ИПC	ИП5	+	П5
ИПC	x^2	П2	ИП6	+	П6	ИП2	ИПC	*	ИП7
+	П7	ИП2	x^2	ИП8	+	П8	ИПC	ИПD	*
ИПA	+	ПA	ИП2	ИПD	*	ИПB	+	ПB	ИПD
КИП4	С/П	БП	00
```


''Input'': В/О x<sub>1</sub> С/П y<sub>1</sub> С/П x<sub>2</sub> С/П y<sub>2</sub> С/П ...

Part 2:
<lang>ИП5	ПC	ИП6	ПD	П2	ИП7	П3	ИП4	ИПD	*
ИПC	ИП5	*	-	ПD	ИП4	ИП7	*	ИПC	ИП6
*	-	П7	ИП4	ИПA	*	ИПC	ИП9	*	-
ПA	ИП4	ИП3	*	ИП2	ИП5	*	-	П3	ИП4
ИП8	*	ИП2	ИП6	*	-	П8	ИП4	ИПB	*
ИП2	ИП9	*	-	ИПD	*	ИП3	ИПA	*	-
ИПD	ИП8	*	ИП7	ИП3	*	-	/	ПB	ИПA
ИПB	ИП7	*	-	ИПD	/	ПA	ИП9	ИПB	ИП6
*	-	ИПA	ИП5	*	-	ИП4	/	П9	С/П
```


''Result'': Р9 = a<sub>0</sub>, РA = a<sub>1</sub>, РB = a<sub>2</sub>.

=={{header|Modula-2}}==

```modula2
MODULE PolynomialRegression;
FROM FormatString IMPORT FormatString;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Eval(a,b,c,x : REAL) : REAL;
BEGIN
    RETURN a + b*x + c*x*x;
END Eval;

PROCEDURE Regression(x,y : ARRAY OF INTEGER);
VAR
    n,i : INTEGER;
    xm,x2m,x3m,x4m : REAL;
    ym : REAL;
    xym,x2ym : REAL;
    sxx,sxy,sxx2,sx2x2,sx2y : REAL;
    a,b,c : REAL;
    buf : ARRAY[0..63] OF CHAR;
BEGIN
    n := SIZE(x)/SIZE(INTEGER);

    xm := 0.0;
    ym := 0.0;
    x2m := 0.0;
    x3m := 0.0;
    x4m := 0.0;
    xym := 0.0;
    x2ym := 0.0;
    FOR i:=0 TO n-1 DO
        xm := xm + FLOAT(x[i]);
        ym := ym + FLOAT(y[i]);
        x2m := x2m + FLOAT(x[i]) * FLOAT(x[i]);
        x3m := x3m + FLOAT(x[i]) * FLOAT(x[i]) * FLOAT(x[i]);
        x4m := x4m + FLOAT(x[i]) * FLOAT(x[i]) * FLOAT(x[i]) * FLOAT(x[i]);
        xym := xym + FLOAT(x[i]) * FLOAT(y[i]);
        x2ym := x2ym + FLOAT(x[i]) * FLOAT(x[i]) * FLOAT(y[i]);
    END;
    xm := xm / FLOAT(n);
    ym := ym / FLOAT(n);
    x2m := x2m / FLOAT(n);
    x3m := x3m / FLOAT(n);
    x4m := x4m / FLOAT(n);
    xym := xym / FLOAT(n);
    x2ym := x2ym / FLOAT(n);

    sxx := x2m - xm * xm;
    sxy := xym - xm * ym;
    sxx2 := x3m - xm * x2m;
    sx2x2 := x4m - x2m * x2m;
    sx2y := x2ym - x2m * ym;

    b := (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
    c := (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2);
    a := ym - b * xm - c * x2m;

    WriteString("y = ");
    RealToStr(a, buf);
    WriteString(buf);
    WriteString(" + ");
    RealToStr(b, buf);
    WriteString(buf);
    WriteString("x + ");
    RealToStr(c, buf);
    WriteString(buf);
    WriteString("x^2");
    WriteLn;

    FOR i:=0 TO n-1 DO
        FormatString("%2i %3i  ", buf, x[i], y[i]);
        WriteString(buf);
        RealToStr(Eval(a,b,c,FLOAT(x[i])), buf);
        WriteString(buf);
        WriteLn;
    END;
END Regression;

TYPE R = ARRAY[0..10] OF INTEGER;
VAR
    x,y : R;
BEGIN
    x := R{0,1,2,3,4,5,6,7,8,9,10};
    y := R{1,6,17,34,57,86,121,162,209,262,321};
    Regression(x,y);

    ReadChar;
END PolynomialRegression.
```



## Octave



```octave
x = [0:10];
y = [1,   6,  17,  34,  57,  86, 121, 162, 209, 262, 321];
coeffs = polyfit(x, y, 2)
```



## PARI/GP

Lagrange interpolating polynomial:

```parigp
polinterpolate([0,1,2,3,4,5,6,7,8,9,10],[1,6,17,34,57,86,121,162,209,262,321])
```

In newer versions, this can be abbreviated:

```parigp
polinterpolate([0..10],[1,6,17,34,57,86,121,162,209,262,321])
```

```txt
3*x^2 + 2*x + 1
```


Least-squares fit:

```parigp
V=[1,6,17,34,57,86,121,162,209,262,321]~;
M=matrix(#V,3,i,j,(i-1)^(j-1));Polrev(matsolve(M~*M,M~*V))
```

<small>Code thanks to [http://pari.math.u-bordeaux.fr/archives/pari-users-1105/msg00006.html Bill Allombert]</small>
```txt
3*x^2 + 2*x + 1
```


Least-squares polynomial fit in its own function:

```parigp
lsf(X,Y,n)=my(M=matrix(#X,n+1,i,j,X[i]^(j-1))); Polrev(matsolve(M~*M,M~*Y~))
lsf([0..10], [1,6,17,34,57,86,121,162,209,262,321], 2)
```



## Perl

This script depends on the <tt>Math::MatrixReal</tt> CPAN module to compute matrix determinants.

```Perl
use strict;
use warnings;
use feature 'say';

#This is a script to calculate an equation for a given set of coordinates.
#Input will be taken in sets of x and y. It can handle a grand total of 26 pairs.
#For matrix functions, we depend on the Math::MatrixReal package.
use Math::MatrixReal;

=pod

Step 1: Get each x coordinate all at once (delimited by " ") and each for y at once
on the next prompt in the same format (delimited by " ").
=cut

sub getPairs() {
    my $buffer = <STDIN>;
    chomp($buffer);
    return split(" ", $buffer);
}
say("Please enter the values for the x coordinates, each delimited by a space. \(Ex: 0 1 2 3\)");
my @x = getPairs();
say("Please enter the values for the y coordinates, each delimited by a space. \(Ex: 0 1 2 3\)");
my @y = getPairs();
#This whole thing depends on the number of x's being the same as the number of y's
my $pairs = scalar(@x);

=pod

Step 2: Devise the base equation of our polynomial using the following idea
There is some polynomial of degree n (n == number of pairs - 1) such that
f(x)=ax^n + bx^(n-1) + ... yx + z
=cut

#Create an array of coefficients and their degrees with the format ("coefficent degree")
my @alphabet;
my @degrees;
for(my $alpha = "a", my $degree = $pairs - 1; $degree >= 0; $degree--, $alpha++) {
    push(@alphabet, "$alpha");
    push(@degrees, "$degree");
}


=pod

Step 3: Using the array of coeffs and their degrees, set up individual equations solving for
each coordinate pair. Why put it in this format? It interfaces witht he Math::MatrixReal package better this way.
=cut

my @coeffs;
for(my $count = 0; $count < $pairs; $count++) {
    my $buffer = "[ ";
    foreach (@degrees) {
        $buffer .= (($x[$count] ** $_) . " ");
    }
    push(@coeffs, ($buffer . "]"));
}
my $row;
foreach (@coeffs) {
    $row .= ("$_\n");
}

=pod

Step 4: We now have rows of x's raised to powers. With this in mind, we create a coefficient matrix.
=cut

my $matrix = Math::MatrixReal->new_from_string($row);
my $buffMatrix = $matrix->new_from_string($row);

=pod

Step 5: Now that we've gotten the matrix to do what we want it to do, we need to calculate the various determinants of the matrices
=cut

my $coeffDet = $matrix->det();

=pod

Step 6: Now that we have the determinant of the coefficient matrix, we need to find the determinants of the coefficient matrix with each column (1 at a time) replaced with the y values.
=cut

#NOTE: Unlike in Perl, matrix indices start at 1, not 0.
for(my $rows = my $column = 1; $column <= $pairs; $column++) {
    #Reassign the values in the current column to the y values
    foreach (@y) {
        $buffMatrix->assign($rows, $column, $_);
        $rows++;
    }
    #Find the values for the variables a, b, ... y, z in the original polynomial
    #To round the difference of the determinants, I had to get creative
    my $buffDet = $buffMatrix->det() / $coeffDet;
    my $tempDet = int(abs($buffDet) + .5);
    $alphabet[$column - 1] = $buffDet >= 0 ? $tempDet : 0 - $tempDet;
    #Reset the buffer matrix and the row counter
    $buffMatrix = $matrix->new_from_string($row);
    $rows = 1;
}


=pod

Step 7: Now that we've found the values of a, b, ... y, z of the original polynomial, it's time to form our polynomial!
=cut

my $polynomial;
for(my $i = 0; $i < $pairs-1; $i++) {
    if($alphabet[$i] == 0) {
        next;
    }
    if($alphabet[$i] == 1) {
        $polynomial .= ($degrees[$i] . " + ");
    }
    if($degrees[$i] == 1) {
        $polynomial .= ($alphabet[$i] . "x" . " + ");
    }
    else {
        $polynomial .= ($alphabet[$i] . "x^" . $degrees[$i] . " + ");
    }
}
#Now for the last piece of the poly: the y-intercept.
$polynomial .= $alphabet[scalar(@alphabet)-1];

print("An approximating polynomial for your dataset is $polynomial.\n");

```

```txt
Please enter the values for the x coordinates, each delimited by a space. (Ex: 0 1 2 3)
0 1 2 3 4 5 6 7 8 9 10
Please enter the values for the y coordinates, each delimited by a space. (Ex: 0 1 2 3)
1 6 17 34 57 86 121 162 209 262 321
An approximating polynomial for your dataset is 3x^2 + 2x + 1.
```



## Perl 6

We'll use a Clifford algebra library.


```perl6
use Clifford;

constant @x1 = <0 1 2 3 4 5 6 7 8 9 10>;
constant @y = <1 6 17 34 57 86 121 162 209 262 321>;

constant $x0 = [+] @e[^@x1];
constant $x1 = [+] @x1 Z* @e;
constant $x2 = [+] @x1 »**» 2  Z* @e;

constant $y  = [+] @y Z* @e;

my $J = $x1 ∧ $x2;
my $I = $x0 ∧ $J;

my $I2 = ($I·$I.reversion).Real;

.say for
(($y ∧ $J)·$I.reversion)/$I2,
(($y ∧ ($x2 ∧ $x0))·$I.reversion)/$I2,
(($y ∧ ($x0 ∧ $x1))·$I.reversion)/$I2;
```

```txt
1
2
3

```



## Phix

```Phix
constant x = {0,1,2,3,4,5,6,7,8,9,10}
constant y = {1,6,17,34,57,86,121,162,209,262,321}
constant n = length(x)

function regression()
atom {xm, ym, x2m, x3m, x4m, xym, x2ym} @= 0
    for i=1 to n do
        atom xi = x[i],
             yi = y[i]
        xm += xi
        ym += yi
        x2m += power(xi,2)
        x3m += power(xi,3)
        x4m += power(xi,4)
        xym += xi*yi
        x2ym += power(xi,2)*yi
    end for
    xm /= n
    ym /= n
    x2m /= n
    x3m /= n
    x4m /= n
    xym /= n
    x2ym /= n
    atom Sxx = x2m-power(xm,2),
         Sxy = xym-xm*ym,
         Sxx2 = x3m-xm*x2m,
         Sx2x2 = x4m-power(x2m,2),
         Sx2y = x2ym-x2m*ym,
         B = (Sxy*Sx2x2-Sx2y*Sxx2)/(Sxx*Sx2x2-power(Sxx2,2)),
         C = (Sx2y*Sxx-Sxy*Sxx2)/(Sxx*Sx2x2-power(Sxx2,2)),
         A = ym-B*xm-C*x2m
    return {C,B,A}
end function

atom {a,b,c} = regression()

function f(atom x)
    return a*x*x+b*x+c
end function

printf(1,"y=%gx^2+%gx+%g\n",{a,b,c})
printf(1,"\n  x   y  f(x)\n")
for i=1 to n do
  printf(1," %2d %3d   %3g\n",{x[i],y[i],f(x[i])})
end for
```

```txt

y=3x^2+2x+1

  x   y  f(x)
  0   1     1
  1   6     6
  2  17    17
  3  34    34
  4  57    57
  5  86    86
  6 121   121
  7 162   162
  8 209   209
  9 262   262
 10 321   321

```

Alternatively, a simple plot, (as per [[Polynomial_regression#Racket|Racket]]):
```Phix
include pGUI.e

constant x = {0,1,2,3,4,5,6,7,8,9,10}
constant y = {1,6,17,34,57,86,121,162,209,262,321}

IupOpen()

Ihandle plot = IupPlot("GRID=YES, MARGINLEFT=50, MARGINBOTTOM=40")
             -- (just add ", AXS_YSCALE=LOG10" for a nice log scale)
IupPlotBegin(plot, 0)
for i=1 to length(x) do
    IupPlotAdd(plot, x[i], y[i])
end for
{} = IupPlotEnd(plot)

Ihandle dlg = IupDialog(plot)
IupSetAttributes(dlg, "RASTERSIZE=%dx%d", {640, 480})
IupSetAttribute(dlg, "TITLE", "simple plot")
IupShow(dlg)

IupMainLoop()
IupClose()
```


## PowerShell


```PowerShell

function qr([double[][]]$A) {
    $m,$n = $A.count, $A[0].count
    $pm,$pn = ($m-1), ($n-1)
    [double[][]]$Q = 0..($m-1) | foreach{$row = @(0) * $m; $row[$_] = 1; ,$row}
    [double[][]]$R = $A | foreach{$row = $_; ,@(0..$pn | foreach{$row[$_]})}
    foreach ($h in 0..$pn) {
        [double[]]$u = $R[$h..$pm] | foreach{$_[$h]}
        [double]$nu = $u | foreach {[double]$sq = 0} {$sq += $_*$_} {[Math]::Sqrt($sq)}
        $u[0] -= if ($u[0] -lt 1) {$nu} else {-$nu}
        [double]$nu = $u | foreach {$sq = 0} {$sq += $_*$_} {[Math]::Sqrt($sq)}
        [double[]]$u = $u | foreach { $_/$nu}
        [double[][]]$v = 0..($u.Count - 1) | foreach{$i = $_; ,($u | foreach{2*$u[$i]*$_})}
        [double[][]]$CR = $R | foreach{$row = $_; ,@(0..$pn | foreach{$row[$_]})}
        [double[][]]$CQ = $Q | foreach{$row = $_; ,@(0..$pm | foreach{$row[$_]})}
        foreach ($i in  $h..$pm) {
            foreach ($j in  $h..$pn) {
                $R[$i][$j] -=  $h..$pm | foreach {[double]$sum = 0} {$sum += $v[$i-$h][$_-$h]*$CR[$_][$j]} {$sum}
            }
        }
        if (0 -eq $h)  {
            foreach ($i in  $h..$pm) {
                foreach ($j in  $h..$pm) {
                    $Q[$i][$j] -=  $h..$pm | foreach {$sum = 0} {$sum += $v[$i][$_]*$CQ[$_][$j]} {$sum}
                }
            }
        } else  {
            $p = $h-1
            foreach ($i in  $h..$pm) {
                foreach ($j in  0..$p) {
                    $Q[$i][$j] -=  $h..$pm | foreach {$sum = 0} {$sum += $v[$i-$h][$_-$h]*$CQ[$_][$j]} {$sum}
                }
                foreach ($j in  $h..$pm) {
                    $Q[$i][$j] -=  $h..$pm | foreach {$sum = 0} {$sum += $v[$i-$h][$_-$h]*$CQ[$_][$j]} {$sum}
                }
            }
        }
    }
    foreach ($i in  0..$pm) {
        foreach ($j in  $i..$pm) {$Q[$i][$j],$Q[$j][$i] = $Q[$j][$i],$Q[$i][$j]}
    }
    [PSCustomObject]@{"Q" = $Q; "R" = $R}
}

function leastsquares([Double[][]]$A,[Double[]]$y) {
    $QR = qr $A
    [Double[][]]$Q = $QR.Q
    [Double[][]]$R = $QR.R
    $m,$n = $A.count, $A[0].count
    [Double[]]$z = foreach ($j in  0..($m-1)) {
            0..($m-1) | foreach {$sum = 0} {$sum += $Q[$_][$j]*$y[$_]} {$sum}
    }
    [Double[]]$x = @(0)*$n
    for ($i = $n-1; $i -ge 0; $i--) {
        for ($j = $i+1; $j -lt $n; $j++) {
            $z[$i] -= $x[$j]*$R[$i][$j]
        }
        $x[$i] = $z[$i]/$R[$i][$i]
    }
    $x
}

function polyfit([Double[]]$x,[Double[]]$y,$n) {
    $m = $x.Count
    [Double[][]]$A = 0..($m-1) | foreach{$row = @(1) * ($n+1); ,$row}
    for ($i = 0; $i -lt $m; $i++) {
        for ($j = $n-1; 0 -le $j; $j--) {
            $A[$i][$j] = $A[$i][$j+1]*$x[$i]
        }
    }
    leastsquares $A $y
}

function show($m) {$m | foreach {write-host "$_"}}

$A = @(@(12,-51,4), @(6,167,-68), @(-4,24,-41))
$x = @(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
$y = @(1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321)
"polyfit "
"X^2 X constant"
"$(polyfit $x $y 2)"

```

```txt

polyfit
X^2 X constant
3 1.99999999999998 1.00000000000005

```




## Python


```python>>>
 x = [0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10]
>>> y = [1,   6,  17,  34,  57,  86, 121, 162, 209, 262, 321]
>>> coeffs = numpy.polyfit(x,y,deg=2)
>>> coeffs
array([ 3.,  2.,  1.])
```

Substitute back received coefficients.

```python>>>
 yf = numpy.polyval(numpy.poly1d(coeffs), x)
>>> yf
array([   1.,    6.,   17.,   34.,   57.,   86.,  121.,  162.,  209., 262.,  321.])
```

Find max absolute error:

```python>>>
 '%.1g' % max(y-yf)
'1e-013'
```



### Example

For input arrays `x' and `y':

```python>>>
 x = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
>>> y = [2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0]
```



```python>>>
 p = numpy.poly1d(numpy.polyfit(x, y, deg=2), variable='N')
>>> print p
       2
1.085 N + 10.36 N - 0.6164
```

Thus we confirm once more that for already sorted sequences
the considered quick sort implementation has
quadratic dependence on sequence length
(see [[Query Performance|'''Example''' section for Python language
on ''Query Performance'' page]]).


## R

The easiest (and most robust) approach to solve this in R
is to use the base package's ''lm'' function
which will find the least squares solution via a QR decomposition:


```R

x <- c(0,  1,  2,  3,  4,  5,  6,   7,   8,   9,   10)
y <- c(1,  6,  17, 34, 57, 86, 121, 162, 209, 262, 321)
coef(lm(y ~ x + I(x^2)))
```


```txt

(Intercept)           x      I(x^2)
          1           2           3

```


Alternately, use poly:


```R
coef(lm(y ~ poly(x, 2, raw=T)))
```
```txt
         (Intercept) poly(x, 2, raw = T)1 poly(x, 2, raw = T)2
                   1                    2                    3
```



## Racket


```racket

#lang racket
(require math plot)

(define xs '(0 1  2  3  4  5   6   7   8   9  10))
(define ys '(1 6 17 34 57 86 121 162 209 262 321))

(define (fit x y n)
  (define Y (->col-matrix y))
  (define V (vandermonde-matrix x (+ n 1)))
  (define VT (matrix-transpose V))
  (matrix->vector (matrix-solve (matrix* VT V) (matrix* VT Y))))

(define ((poly v) x)
  (for/sum ([c v] [i (in-naturals)])
    (* c (expt x i))))

(plot (list (points   (map vector xs ys))
            (function (poly (fit xs ys 2)))))

```

[[File:polyreg-racket.png]]


## REXX


```rexx
/* REXX ---------------------------------------------------------------
* Implementation of http://keisan.casio.com/exec/system/14059932254941
*--------------------------------------------------------------------*/
xl='0 1  2  3  4  5   6   7   8   9  10'
yl='1 6 17 34 57 86 121 162 209 262 321'
n=11
Do i=1 To n
  Parse Var xl x.i xl
  Parse Var yl y.i yl
  End
xm=0
ym=0
x2m=0
x3m=0
x4m=0
xym=0
x2ym=0
Do i=1 To n
  xm=xm+x.i
  ym=ym+y.i
  x2m=x2m+x.i**2
  x3m=x3m+x.i**3
  x4m=x4m+x.i**4
  xym=xym+x.i*y.i
  x2ym=x2ym+(x.i**2)*y.i
  End
xm =xm /n
ym =ym /n
x2m=x2m/n
x3m=x3m/n
x4m=x4m/n
xym=xym/n
x2ym=x2ym/n
Sxx=x2m-xm**2
Sxy=xym-xm*ym
Sxx2=x3m-xm*x2m
Sx2x2=x4m-x2m**2
Sx2y=x2ym-x2m*ym
B=(Sxy*Sx2x2-Sx2y*Sxx2)/(Sxx*Sx2x2-Sxx2**2)
C=(Sx2y*Sxx-Sxy*Sxx2)/(Sxx*Sx2x2-Sxx2**2)
A=ym-B*xm-C*x2m
Say 'y='a'+'||b'*x+'c'*x**2'
Say ' Input  "Approximation"'
Say ' x   y     y1'
Do i=1 To 11
  Say right(x.i,2) right(y.i,3) format(fun(x.i),5,3)
  End
Exit
fun:
  Parse Arg x
  Return a+b*x+c*x**2
```

```txt
y=1+2*x+3*x**2
 Input  "Approximation"
 x   y     y1
 0   1     1.000
 1   6     6.000
 2  17    17.000
 3  34    34.000
 4  57    57.000
 5  86    86.000
 6 121   121.000
 7 162   162.000
 8 209   209.000
 9 262   262.000
10 321   321.000
```



## Ruby


```ruby
require 'matrix'

def regress x, y, degree
  x_data = x.map { |xi| (0..degree).map { |pow| (xi**pow).to_r } }

  mx = Matrix[*x_data]
  my = Matrix.column_vector(y)

  ((mx.t * mx).inv * mx.t * my).transpose.to_a[0].map(&:to_f)
end
```

'''Testing:'''

```ruby
p regress([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
          [1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321],
          2)
```

```txt
[1.0, 2.0, 3.0]
```



## Scala

{{Out}}See it yourself by running in your browser [https://scastie.scala-lang.org/NklZH2LlScCpfsN4NSfFvA Scastie (remote JVM)].
```Scala
object PolynomialRegression extends App {
  private def xy = Seq(1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321).zipWithIndex.map(_.swap)

  private def polyRegression(xy: Seq[(Int, Int)]): Unit = {
    val r = xy.indices

    def average[U](ts: Iterable[U])(implicit num: Numeric[U]) = num.toDouble(ts.sum) / ts.size

    def x3m: Double = average(r.map(a => a * a * a))
    def x4m: Double = average(r.map(a => a * a * a * a))
    def x2ym = xy.reduce((a, x) => (a._1 + x._1 * x._1 * x._2, 0))._1.toDouble / xy.size
    def xym = xy.reduce((a, x) => (a._1 + x._1 * x._2, 0))._1.toDouble / xy.size

    val x2m: Double = average(r.map(a => a * a))
    val (xm, ym) = (average(xy.map(_._1)), average(xy.map(_._2)))
    val (sxx, sxy) = (x2m - xm * xm, xym - xm * ym)
    val sxx2: Double = x3m - xm * x2m
    val sx2x2: Double = x4m - x2m * x2m
    val sx2y: Double = x2ym - x2m * ym
    val c: Double = (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
    val b: Double = (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
    val a: Double = ym - b * xm - c * x2m

    def abc(xx: Int) = a + b * xx + c * xx * xx

    println(s"y = $a + ${b}x + ${c}x^2")
    println(" Input  Approximation")
    println(" x   y     y1")
    xy.foreach {el => println(f"${el._1}%2d ${el._2}%3d  ${abc(el._1)}%5.1f")}
  }

  polyRegression(xy)

}
```


## Sidef

```ruby
func regress(x, y, degree) {
    var A = Matrix.build(x.len, degree+1, {|i,j|
        x[i]**j
    })

    var B = Matrix.column_vector(y...)
    ((A.transpose * A)**(-1) * A.transpose * B).transpose[0]
}

func poly(x) {
    3*x**2 + 2*x + 1
}

var coeff = regress(
    10.of { _ },
    10.of { poly(_) },
    2
)

say coeff
```

```txt
[1, 2, 3]
```



## Stata

See '''[http://www.stata.com/help.cgi?fvvarlist Factor variables]''' in Stata help for explanations on the ''c.x##c.x'' syntax.

```stata
. clear
. input x y
0 1
1 6
2 17
3 34
4 57
5 86
6 121
7 162
8 209
9 262
10 321
end

. regress y c.x##c.x

      Source |       SS           df       MS      Number of obs   =        11
-------------+----------------------------------   F(2, 8)         =         .
       Model |      120362         2       60181   Prob > F        =         .
    Residual |           0         8           0   R-squared       =    1.0000
-------------+----------------------------------   Adj R-squared   =    1.0000
       Total |      120362        10     12036.2   Root MSE        =         0

------------------------------------------------------------------------------
           y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
           x |          2          .        .       .            .           .
             |
     c.x#c.x |          3          .        .       .            .           .
             |
       _cons |          1          .        .       .            .           .
------------------------------------------------------------------------------
```



## Tcl

<!-- This implementation from Emiliano Gavilan;
posted here with his explicit permission -->

```tcl
package require math::linearalgebra

proc build.matrix {xvec degree} {
    set sums [llength $xvec]
    for {set i 1} {$i <= 2*$degree} {incr i} {
        set sum 0
        foreach x $xvec {
            set sum [expr {$sum + pow($x,$i)}]
        }
        lappend sums $sum
    }

    set order [expr {$degree + 1}]
    set A [math::linearalgebra::mkMatrix $order $order 0]
    for {set i 0} {$i <= $degree} {incr i} {
        set A [math::linearalgebra::setrow A $i [lrange $sums $i $i+$degree]]
    }
    return $A
}

proc build.vector {xvec yvec degree} {
    set sums [list]
    for {set i 0} {$i <= $degree} {incr i} {
        set sum 0
        foreach x $xvec y $yvec {
            set sum [expr {$sum + $y * pow($x,$i)}]
        }
        lappend sums $sum
    }

    set x [math::linearalgebra::mkVector [expr {$degree + 1}] 0]
    for {set i 0} {$i <= $degree} {incr i} {
        set x [math::linearalgebra::setelem x $i [lindex $sums $i]]
    }
    return $x
}

# Now, to solve the example from the top of this page
set x {0   1   2   3   4   5   6   7   8   9  10}
set y {1   6  17  34  57  86 121 162 209 262 321}

# build the system A.x=b
set degree 2
set A [build.matrix $x $degree]
set b [build.vector $x $y $degree]
# solve it
set coeffs [math::linearalgebra::solveGauss $A $b]
# show results
puts $coeffs
```

This will print:
 1.0000000000000207 1.9999999999999958 3.0
which is a close approximation to the correct solution.

=={{header|TI-89 BASIC}}==

```ti89b
DelVar x
seq(x,x,0,10) → xs
{1,6,17,34,57,86,121,162,209,262,321} → ys
QuadReg xs,ys
Disp regeq(x)
```


<code>seq(''expr'',''var'',''low'',''high'')</code> evaluates ''expr'' with ''var'' bound to integers from ''low'' to ''high'' and returns a list of the results. <code> →</code> is the assignment operator.
<code>QuadReg</code>, "quadratic regression", does the fit and stores the details in a number of standard variables, including <var>regeq</var>, which receives the fitted quadratic (polynomial) function itself.
We then apply that function to the (undefined as ensured by <code>DelVar</code>) variable x to obtain the expression in terms of x, and display it.

<code>3.·x<sup>2</sup> + 2.·x + 1.</code>


## Ursala

The fit function defined below returns the coefficients
of an nth-degree polynomial in order of descending degree
fitting the lists of inputs x and outputs y.
The real work is done by the dgelsd function from the lapack library.
Ursala provides a simplified interface to this library
whereby the data can be passed as lists rather than arrays,
and all memory management is handled automatically.

```Ursala
#import std
#import nat
#import flo

(fit "n") ("x","y") = ..dgelsd\"y" (gang \/*pow float*x iota successor "n")* "x"
```

test program:

```Ursala
x = <0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.>
y = <1.,6.,17.,34.,57.,86.,121.,162.,209.,262.,321.>

#cast %eL

example = fit2(x,y)
```

```txt
<3.000000e+00,2.000000e+00,1.000000e+00>
```



## VBA

Excel VBA has built in capability for line estimation.

```vb
Option Base 1
Private Function polynomial_regression(y As Variant, x As Variant, degree As Integer) As Variant
    Dim a() As Double
    ReDim a(UBound(x), 2)
    For i = 1 To UBound(x)
        For j = 1 To degree
            a(i, j) = x(i) ^ j
        Next j
    Next i
    polynomial_regression = WorksheetFunction.LinEst(WorksheetFunction.Transpose(y), a, True, True)
End Function
Public Sub main()
    x = [{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}]
    y = [{1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}]
    result = polynomial_regression(y, x, 2)
    Debug.Print "coefficients   : ";
    For i = UBound(result, 2) To 1 Step -1
        Debug.Print Format(result(1, i), "0.#####"),
    Next i
    Debug.Print
    Debug.Print "standard errors: ";
    For i = UBound(result, 2) To 1 Step -1
        Debug.Print Format(result(2, i), "0.#####"),
    Next i
    Debug.Print vbCrLf
    Debug.Print "R^2 ="; result(3, 1)
    Debug.Print "F   ="; result(4, 1)
    Debug.Print "Degrees of freedom:"; result(4, 2)
    Debug.Print "Standard error of y estimate:"; result(3, 2)
End Sub
```
```txt
coefficients   : 1,         2,            3,
standard errors: 0,         0,            0,

R^2 = 1
F   = 7,70461300500498E+31
Degrees of freedom: 8
Standard error of y estimate: 2,79482284961344E-14
```


## zkl

Using the GNU Scientific Library

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
xs:=GSL.VectorFromData(0,  1,  2,  3,  4,  5,   6,   7,   8,   9,  10);
ys:=GSL.VectorFromData(1,  6, 17, 34, 57, 86, 121, 162, 209, 262, 321);
v :=GSL.polyFit(xs,ys,2);
v.format().println();
GSL.Helpers.polyString(v).println();
GSL.Helpers.polyEval(v,xs).format().println();
```

```txt

1.00,2.00,3.00
 1 + 2x + 3x^2
1.00,6.00,17.00,34.00,57.00,86.00,121.00,162.00,209.00,262.00,321.00

```

Or, using lists:
Uses the code from [[Multiple regression#zkl]].

Example:

```zkl
polyfit(T(T(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0)),
        T(T(1.0,6.0,17.0,34.0,57.0,86.0,121.0,162.0,209.0,262.0,321.0)), 2)
.flatten().println();
```

```txt
L(1,2,3)
```

