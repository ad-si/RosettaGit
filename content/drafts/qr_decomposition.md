+++
title = "QR decomposition"
description = ""
date = 2019-09-12T22:46:15Z
aliases = []
[extra]
id = 9939
[taxonomies]
categories = []
tags = []
+++

{{task|Matrices}}[[Category:Mathematics]]
Any rectangular <math>m \times n</math> matrix <math>\mathit A</math> can be decomposed to a product of an orthogonal matrix <math>\mathit Q</math> and an upper (right) triangular matrix <math>\mathit R</math>, as described in [[wp:QR decomposition|QR decomposition]].

'''Task'''

Demonstrate the QR decomposition on the example matrix from the [[wp:QR_decomposition#Example_2|Wikipedia article]]:

::<math>A = \begin{pmatrix}
12 & -51 & 4 \\
6 & 167 & -68 \\
-4 & 24 & -41 \end{pmatrix}</math>

and the usage for linear least squares problems on the example from [[Polynomial_regression]]. The method of [[wp: Householder transformation|Householder reflections]] should be used:

'''Method'''

Multiplying a given vector <math>\mathit a</math>, for example the first column of matrix <math>\mathit A</math>, with the Householder matrix <math>\mathit H</math>, which is given as

::<math>H = I - \frac {2} {u^T u} u u^T</math>

reflects <math>\mathit a</math> about a plane given by its normal vector <math>\mathit u</math>. When the normal vector of the plane <math>\mathit u</math> is given as

::<math>u = a - \|a\|_2 \; e_1</math>

then the transformation reflects <math>\mathit a</math> onto the first standard basis vector

::<math>e_1 = [1 \; 0 \; 0 \; ...]^T</math>

which means that all entries but the first become zero. To avoid numerical cancellation errors, we should take the opposite sign of <math>a_1</math>:

::<math>u = a + \textrm{sign}(a_1)\|a\|_2 \; e_1</math>

and normalize with respect to the first element:

::<math>v = \frac{u}{u_1}</math>

The equation for <math>H</math> thus becomes:

::<math>H = I - \frac {2} {v^T v} v v^T</math>

or, in another form

::<math>H = I - \beta v v^T</math>

with
::<math>\beta = \frac {2} {v^T v}</math>

Applying <math>\mathit H</math> on <math>\mathit a</math> then gives

::<math>H \; a = -\textrm{sign}(a_1) \; \|a\|_2 \; e_1</math>

and applying <math>\mathit H</math> on the matrix <math>\mathit A</math> zeroes all subdiagonal elements of the first column:

::<math>H_1 \; A = \begin{pmatrix}
r_{11} & r_{12} & r_{13} \\
0    & *    & * \\
0    & *    & * \end{pmatrix}</math>

In the second step, the second column of <math>\mathit A</math>, we want to zero all elements but the first two, which means that we have to calculate <math>\mathit H</math> with the first column of the ''submatrix'' (denoted *), not on the whole second column of <math>\mathit A</math>.

To get <math>H_2</math>, we then embed the new <math>\mathit H</math> into an <math>m \times n</math> identity:

::<math>H_2 = \begin{pmatrix}
1 & 0 & 0 \\
0 & H & \\
0 &   & \end{pmatrix}</math>

This is how we can, column by column, remove all subdiagonal elements of <math>\mathit A</math> and thus transform it into <math>\mathit R</math>.

::<math>H_n \; ... \; H_3 H_2 H_1 A = R</math>

The product of all the Householder matrices <math>\mathit H</math>, for every column, in reverse order, will then yield the orthogonal matrix <math>\mathit Q</math>.

::<math>H_1 H_2 H_3 \; ... \; H_n = Q</math>

The QR decomposition should then be used to solve linear least squares ([[Multiple regression]]) problems <math>\mathit A x = b</math> by solving

::<math>R \; x = Q^T \; b</math>

When <math>\mathit R</math> is not square, i.e. <math>m > n</math> we have to cut off the <math>\mathit m - n</math> zero padded bottom rows.

::<math>R =
\begin{pmatrix}
R_1 \\
0 \end{pmatrix}</math>

and the same for the RHS:

::<math>Q^T \; b =
\begin{pmatrix}
q_1 \\
q_2 \end{pmatrix}</math>

Finally, solve the square upper triangular system by back substitution:

::<math>R_1 \; x = q_1</math>


## Ada

Output matches that of Matlab solution, not tested with other matrices.

```Ada

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;
with Ada.Numerics.Generic_Elementary_Functions;
procedure QR is

   procedure Show (mat : Real_Matrix) is
      package FIO is new Ada.Text_IO.Float_IO (Float);
   begin
      for row in mat'Range (1) loop
         for col in mat'Range (2) loop
            FIO.Put (mat (row, col), Exp => 0, Aft => 4, Fore => 5);
         end loop;
         New_Line;
      end loop;
   end Show;

   function GetCol (mat : Real_Matrix; n : Integer) return Real_Matrix is
      column : Real_Matrix (mat'Range (1), 1 .. 1);
   begin
      for row in mat'Range (1) loop
         column (row, 1) := mat (row, n);
      end loop;
      return column;
   end GetCol;

   function Mag (mat : Real_Matrix) return Float is
      sum : Real_Matrix := Transpose (mat) * mat;
      package Math is new Ada.Numerics.Generic_Elementary_Functions
         (Float);
   begin
      return Math.Sqrt (sum (1, 1));
   end Mag;

   function eVect (col : Real_Matrix; n : Integer) return Real_Matrix is
      vect : Real_Matrix (col'Range (1), 1 .. 1);
   begin
      for row in col'Range (1) loop
         if row /= n then vect (row, 1) := 0.0;
         else vect (row, 1) := 1.0; end if;
      end loop;
      return vect;
   end eVect;

   function Identity (n : Integer) return Real_Matrix is
      mat : Real_Matrix (1 .. n, 1 .. n) := (1 .. n => (others => 0.0));
   begin
      for i in Integer range 1 .. n loop mat (i, i) := 1.0; end loop;
      return mat;
   end Identity;

   function Chop (mat : Real_Matrix; n : Integer) return Real_Matrix is
      small : Real_Matrix (n .. mat'Length (1), n .. mat'Length (2));
   begin
      for row in small'Range (1) loop
         for col in small'Range (2) loop
            small (row, col) := mat (row, col);
         end loop;
      end loop;
      return small;
   end Chop;

   function H_n (inmat : Real_Matrix; n : Integer)
      return Real_Matrix is
      mat : Real_Matrix := Chop (inmat, n);
      col : Real_Matrix := GetCol (mat, n);
      colT : Real_Matrix (1 .. 1, mat'Range (1));
      H : Real_Matrix := Identity (mat'Length (1));
      Hall : Real_Matrix := Identity (inmat'Length (1));
   begin
      col := col - Mag (col) * eVect (col, n);
      col := col / Mag (col);
      colT := Transpose (col);
      H := H - 2.0 * (col * colT);
      for row in H'Range (1) loop
         for col in H'Range (2) loop
            Hall (n - 1 + row, n - 1 + col) := H (row, col);
         end loop;
      end loop;
      return Hall;
   end H_n;

   A : constant Real_Matrix (1 .. 3, 1 .. 3) := (
      (12.0, -51.0, 4.0),
      (6.0, 167.0, -68.0),
      (-4.0, 24.0, -41.0));
   Q1, Q2, Q3, Q, R: Real_Matrix (1 .. 3, 1 .. 3);
begin
   Q1 := H_n (A, 1);
   Q2 := H_n (Q1 * A, 2);
   Q3 := H_n (Q2 * Q1* A, 3);
   Q := Transpose (Q1) * Transpose (Q2) * TransPose(Q3);
   R := Q3 * Q2 * Q1 * A;
   Put_Line ("Q:"); Show (Q);
   Put_Line ("R:"); Show (R);
end QR;
```

{{out}}

```txt
Q:
    0.8571   -0.3943   -0.3314
    0.4286    0.9029    0.0343
   -0.2857    0.1714   -0.9429
R:
   14.0000   21.0000  -14.0000
   -0.0000  175.0000  -70.0000
   -0.0000    0.0000   35.0000
```



## Axiom

The following provides a generic QR decomposition for arbitrary precision floats, double floats and exact calculations:

```Axiom
)abbrev package TESTP TestPackage
TestPackage(R:Join(Field,RadicalCategory)): with
    unitVector: NonNegativeInteger -> Vector(R)
    "/": (Vector(R),R) -> Vector(R)
    "^": (Vector(R),NonNegativeInteger) -> Vector(R)
    solveUpperTriangular: (Matrix(R),Vector(R)) -> Vector(R)
    signValue: R -> R
    householder: Vector(R) -> Matrix(R)
    qr: Matrix(R) -> Record(q:Matrix(R),r:Matrix(R))
    lsqr: (Matrix(R),Vector(R)) -> Vector(R)
    polyfit: (Vector(R),Vector(R),NonNegativeInteger) -> Vector(R)
  == add
    unitVector(dim) ==
      out := new(dim,0@R)$Vector(R)
      out(1) := 1@R
      out
    v:Vector(R) / a:R == map((vi:R):R +-> vi/a, v)$Vector(R)
    v:Vector(R) ^ n:NonNegativeInteger == map((vi:R):R +-> vi^n, v)$Vector(R)
    solveUpperTriangular(r,b) ==
      n := ncols r
      x := new(n,0@R)$Vector(R)
      for k in n..1 by -1 repeat
        index := min(n,k+1)
	x(k) := (b(k)-reduce("+",subMatrix(r,k,k,index,n)*x.(index..n)))/r(k,k)
      x
    signValue(r) ==
      R has (sign: R -> Integer) => coerce(sign(r)$R)$R
      zero? r => r
      if sqrt(r*r) = r then 1 else -1
    householder(a) ==
      m := #a
      u := a + length(a)*signValue(a(1))*unitVector(m)
      v := u/u(1)
      beta := (1+1)/dot(v,v)
      scalarMatrix(m,1) - beta*transpose(outerProduct(v,v))
    qr(a) ==
      (m,n) := (nrows a, ncols a)
      qm := scalarMatrix(m,1)
      rm := copy a
      for i in 1..(if m=n then n-1 else n) repeat
        x := column(subMatrix(rm,i,m,i,i),1)
	h := scalarMatrix(m,1)
	setsubMatrix!(h,i,i,householder x)
	qm := qm*h
	rm := h*rm
      [qm,rm]
    lsqr(a,b) ==
      dc := qr a
      n := ncols(dc.r)
      solveUpperTriangular(subMatrix(dc.r,1,n,1,n),transpose(dc.q)*b)
    polyfit(x,y,n) ==
      a := new(#x,n+1,0@R)$Matrix(R)
      for j in 0..n repeat
        setColumn!(a,j+1,x^j)
      lsqr(a,y)
```

This can be called using:

```Axiom
m := matrix [[12, -51, 4], [6, 167, -68], [-4, 24, -41]];
qr m
x := vector [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
y := vector [1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321];
polyfit(x, y, 2)
```

With output in exact form:

```Axiom
qr m

             +  6    69     58 +
             |- -   ---    --- |
             |  7   175    175 |
             |                 |    +- 14  - 21    14 +
             |  3    158     6 |    |                 |
         [q= |- -  - ---  - ---|,r= | 0    - 175   70 |]
             |  7    175    175|    |                 |
             |                 |    + 0      0    - 35+
             | 2      6    33  |
             | -   - --    --  |
             + 7     35    35  +

          Type: Record(q: Matrix(AlgebraicNumber),r: Matrix(AlgebraicNumber))

polyfit(x, y, 2)

   [1,2,3]
                              Type: Vector(AlgebraicNumber)
```

The calculations are comparable to those from the default QR decomposition in R.


## BBC BASIC

{{works with|BBC BASIC for Windows}}
Makes heavy use of BBC BASIC's matrix arithmetic.

```bbcbasic
      *FLOAT 64
      @% = &2040A
      INSTALL @lib$+"ARRAYLIB"

      REM Test matrix for QR decomposition:
      DIM A(2,2)
      A() = 12, -51,   4, \
      \      6, 167, -68, \
      \     -4,  24, -41

      REM Do the QR decomposition:
      DIM Q(2,2), R(2,2)
      PROCqrdecompose(A(), Q(), R())
      PRINT "Q:"
      PRINT Q(0,0), Q(0,1), Q(0,2)
      PRINT Q(1,0), Q(1,1), Q(1,2)
      PRINT Q(2,0), Q(2,1), Q(2,2)
      PRINT "R:"
      PRINT R(0,0), R(0,1), R(0,2)
      PRINT R(1,0), R(1,1), R(1,2)
      PRINT R(2,0), R(2,1), R(2,2)

      REM Test data for least-squares solution:
      DIM x(10) : x() = 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
      DIM y(10) : y() = 1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321

      REM Do the least-squares solution:
      DIM a(10,2), q(10,10), r(10,2), t(10,10), b(10), z(2)
      FOR i% = 0 TO 10
        FOR j% = 0 TO 2
          a(i%,j%) = x(i%) ^ j%
        NEXT
      NEXT
      PROCqrdecompose(a(), q(), r())
      PROC_transpose(q(),t())
      b() = t() . y()
      FOR k% = 2 TO 0 STEP -1
        s = 0
        IF k% < 2 THEN
          FOR j% = k%+1 TO 2
            s += r(k%,j%) * z(j%)
          NEXT
        ENDIF
        z(k%) = (b(k%) - s) / r(k%,k%)
      NEXT k%
      PRINT '"Least-squares solution:"
      PRINT z(0), z(1), z(2)
      END

      DEF PROCqrdecompose(A(), Q(), R())
      LOCAL i%, k%, m%, n%, H()
      m% = DIM(A(),1) : n% = DIM(A(),2)
      DIM H(m%,m%)
      FOR i% = 0 TO m% : Q(i%,i%) = 1 : NEXT
      WHILE n%
        PROCqrstep(n%, k%, A(), H())
        A() = H() . A()
        Q() = Q() . H()
        k% += 1
        m% -= 1
        n% -= 1
      ENDWHILE
      R() = A()
      ENDPROC

      DEF PROCqrstep(n%, k%, A(), H())
      LOCAL a(), h(), i%, j%
      DIM a(n%,0), h(n%,n%)
      FOR i% = 0 TO n% : a(i%,0) = A(i%+k%,k%) : NEXT
      PROChouseholder(h(), a())
      H() = 0  : H(0,0) = 1
      FOR i% = 0 TO n%
        FOR j% = 0 TO n%
          H(i%+k%,j%+k%) = h(i%,j%)
        NEXT
      NEXT
      ENDPROC

      REM Create the Householder matrix for the supplied column vector:
      DEF PROChouseholder(H(), a())
      LOCAL e(), u(), v(), vt(), vvt(), I(), d()
      LOCAL i%, n% : n% = DIM(a(),1)
      REM Create the scaled standard basis vector e():
      DIM e(n%,0) : e(0,0) = SGN(a(0,0)) * MOD(a())
      REM Create the normal vector u():
      DIM u(n%,0) : u() = a() + e()
      REM Normalise with respect to the first element:
      DIM v(n%,0) : v() = u() / u(0,0)
      REM Get the transpose of v() and its dot product with v():
      DIM vt(0,n%), d(0) : PROC_transpose(v(), vt()) : d() = vt() . v()
      REM Get the product of v() and vt():
      DIM vvt(n%,n%) : vvt() = v() . vt()
      REM Create an identity matrix I():
      DIM I(n%,n%) : FOR i% = 0 TO n% : I(i%,i%) = 1 : NEXT
      REM Create the Householder matrix H() = I - 2/vt()v() v()vt():
      vvt() *= 2 / d(0) : H() = I() - vvt()
      ENDPROC
```

'''Output:'''

```txt

Q:
   -0.8571    0.3943    0.3314
   -0.4286   -0.9029   -0.0343
    0.2857   -0.1714    0.9429
R:
  -14.0000  -21.0000   14.0000
    0.0000 -175.0000   70.0000
    0.0000    0.0000  -35.0000

Least-squares solution:
    1.0000    2.0000    3.0000

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct {
	int m, n;
	double ** v;
} mat_t, *mat;

mat matrix_new(int m, int n)
{
	mat x = malloc(sizeof(mat_t));
	x->v = malloc(sizeof(double*) * m);
	x->v[0] = calloc(sizeof(double), m * n);
	for (int i = 0; i < m; i++)
		x->v[i] = x->v[0] + n * i;
	x->m = m;
	x->n = n;
	return x;
}

void matrix_delete(mat m)
{
	free(m->v[0]);
	free(m->v);
	free(m);
}

void matrix_transpose(mat m)
{
	for (int i = 0; i < m->m; i++) {
		for (int j = 0; j < i; j++) {
			double t = m->v[i][j];
			m->v[i][j] = m->v[j][i];
			m->v[j][i] = t;
		}
	}
}

mat matrix_copy(int n, double a[][n], int m)
{
	mat x = matrix_new(m, n);
	for (int i = 0; i < m; i++)
		for (int j = 0; j < n; j++)
			x->v[i][j] = a[i][j];
	return x;
}

mat matrix_mul(mat x, mat y)
{
	if (x->n != y->m) return 0;
	mat r = matrix_new(x->m, y->n);
	for (int i = 0; i < x->m; i++)
		for (int j = 0; j < y->n; j++)
			for (int k = 0; k < x->n; k++)
				r->v[i][j] += x->v[i][k] * y->v[k][j];
	return r;
}

mat matrix_minor(mat x, int d)
{
	mat m = matrix_new(x->m, x->n);
	for (int i = 0; i < d; i++)
		m->v[i][i] = 1;
	for (int i = d; i < x->m; i++)
		for (int j = d; j < x->n; j++)
			m->v[i][j] = x->v[i][j];
	return m;
}

/* c = a + b * s */
double *vmadd(double a[], double b[], double s, double c[], int n)
{
	for (int i = 0; i < n; i++)
		c[i] = a[i] + s * b[i];
	return c;
}

/* m = I - v v^T */
mat vmul(double v[], int n)
{
	mat x = matrix_new(n, n);
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			x->v[i][j] = -2 *  v[i] * v[j];
	for (int i = 0; i < n; i++)
		x->v[i][i] += 1;

	return x;
}

/* ||x|| */
double vnorm(double x[], int n)
{
	double sum = 0;
	for (int i = 0; i < n; i++) sum += x[i] * x[i];
	return sqrt(sum);
}

/* y = x / d */
double* vdiv(double x[], double d, double y[], int n)
{
	for (int i = 0; i < n; i++) y[i] = x[i] / d;
	return y;
}

/* take c-th column of m, put in v */
double* mcol(mat m, double *v, int c)
{
	for (int i = 0; i < m->m; i++)
		v[i] = m->v[i][c];
	return v;
}

void matrix_show(mat m)
{
	for(int i = 0; i < m->m; i++) {
		for (int j = 0; j < m->n; j++) {
			printf(" %8.3f", m->v[i][j]);
		}
		printf("\n");
	}
	printf("\n");
}

void householder(mat m, mat *R, mat *Q)
{
	mat q[m->m];
	mat z = m, z1;
	for (int k = 0; k < m->n && k < m->m - 1; k++) {
		double e[m->m], x[m->m], a;
		z1 = matrix_minor(z, k);
		if (z != m) matrix_delete(z);
		z = z1;

		mcol(z, x, k);
		a = vnorm(x, m->m);
		if (m->v[k][k] > 0) a = -a;

		for (int i = 0; i < m->m; i++)
			e[i] = (i == k) ? 1 : 0;

		vmadd(x, e, a, e, m->m);
		vdiv(e, vnorm(e, m->m), e, m->m);
		q[k] = vmul(e, m->m);
		z1 = matrix_mul(q[k], z);
		if (z != m) matrix_delete(z);
		z = z1;
	}
	matrix_delete(z);
	*Q = q[0];
	*R = matrix_mul(q[0], m);
	for (int i = 1; i < m->n && i < m->m - 1; i++) {
		z1 = matrix_mul(q[i], *Q);
		if (i > 1) matrix_delete(*Q);
		*Q = z1;
		matrix_delete(q[i]);
	}
	matrix_delete(q[0]);
	z = matrix_mul(*Q, m);
	matrix_delete(*R);
	*R = z;
	matrix_transpose(*Q);
}

double in[][3] = {
	{ 12, -51,   4},
	{  6, 167, -68},
	{ -4,  24, -41},
	{ -1, 1, 0},
	{ 2, 0, 3},
};

int main()
{
	mat R, Q;
	mat x = matrix_copy(3, in, 5);
	householder(x, &R, &Q);

	puts("Q"); matrix_show(Q);
	puts("R"); matrix_show(R);

	// to show their product is the input matrix
	mat m = matrix_mul(Q, R);
	puts("Q * R"); matrix_show(m);

	matrix_delete(x);
	matrix_delete(R);
	matrix_delete(Q);
	matrix_delete(m);
	return 0;
}
```

{{out}}

```txt

Q
    0.846   -0.391    0.343    0.082    0.078
    0.423    0.904   -0.029    0.026    0.045
   -0.282    0.170    0.933   -0.047   -0.137
   -0.071    0.014   -0.001    0.980   -0.184
    0.141   -0.017   -0.106   -0.171   -0.969

R
   14.177   20.667  -13.402
   -0.000  175.043  -70.080
    0.000    0.000  -35.202
   -0.000   -0.000   -0.000
    0.000    0.000   -0.000

Q * R
   12.000  -51.000    4.000
    6.000  167.000  -68.000
   -4.000   24.000  -41.000
   -1.000    1.000   -0.000
    2.000   -0.000    3.000

```



## C++


```cpp
/*
 * g++ -O3 -Wall --std=c++11 qr_standalone.cpp -o qr_standalone
 */
#include <cstdio>
#include <cstdlib>
#include <cstring> // for memset
#include <limits>
#include <iostream>
#include <vector>

#include <math.h>

class Vector;

class Matrix {

public:
  // default constructor (don't allocate)
  Matrix() : m(0), n(0), data(nullptr) {}

  // constructor with memory allocation, initialized to zero
  Matrix(int m_, int n_) : Matrix() {
    m = m_;
    n = n_;
    allocate(m_,n_);
  }

  // copy constructor
  Matrix(const Matrix& mat) : Matrix(mat.m,mat.n) {

    for (int i = 0; i < m; i++)
      for (int j = 0; j < n; j++)
	(*this)(i,j) = mat(i,j);
  }

  // constructor from array
  template<int rows, int cols>
  Matrix(double (&a)[rows][cols]) : Matrix(rows,cols) {

    for (int i = 0; i < m; i++)
      for (int j = 0; j < n; j++)
	(*this)(i,j) = a[i][j];
  }

  // destructor
  ~Matrix() {
    deallocate();
  }


  // access data operators
  double& operator() (int i, int j) {
    return data[i+m*j]; }
  double  operator() (int i, int j) const {
    return data[i+m*j]; }

  // operator assignment
  Matrix& operator=(const Matrix& source) {

    // self-assignment check
    if (this != &source) {
      if ( (m*n) != (source.m * source.n) ) { // storage cannot be reused
	allocate(source.m,source.n);          // re-allocate storage
      }
      // storage can be used, copy data
      std::copy(source.data, source.data + source.m*source.n, data);
    }
    return *this;
  }

  // compute minor
  void compute_minor(const Matrix& mat, int d) {

    allocate(mat.m, mat.n);

    for (int i = 0; i < d; i++)
      (*this)(i,i) = 1.0;
    for (int i = d; i < mat.m; i++)
      for (int j = d; j < mat.n; j++)
	(*this)(i,j) = mat(i,j);

  }

  // Matrix multiplication
  // c = a * b
  // c will be re-allocated here
  void mult(const Matrix& a, const Matrix& b) {

    if (a.n != b.m) {
      std::cerr << "Matrix multiplication not possible, sizes don't match !\n";
      return;
    }

    // reallocate ourself if necessary i.e. current Matrix has not valid sizes
    if (a.m != m or b.n != n)
      allocate(a.m, b.n);

    memset(data,0,m*n*sizeof(double));

    for (int i = 0; i < a.m; i++)
      for (int j = 0; j < b.n; j++)
	for (int k = 0; k < a.n; k++)
	  (*this)(i,j) += a(i,k) * b(k,j);

  }

  void transpose() {
    for (int i = 0; i < m; i++) {
      for (int j = 0; j < i; j++) {
	double t = (*this)(i,j);
	(*this)(i,j) = (*this)(j,i);
	(*this)(j,i) = t;
      }
    }
  }

  // take c-th column of m, put in v
  void extract_column(Vector& v, int c);

  // memory allocation
  void allocate(int m_, int n_) {

    // if already allocated, memory is freed
    deallocate();

    // new sizes
    m = m_;
    n = n_;

    data = new double[m_*n_];
    memset(data,0,m_*n_*sizeof(double));

  } // allocate

  // memory free
  void deallocate() {

    if (data)
      delete[] data;

    data = nullptr;

  }

  int m, n;

private:
  double* data;

}; // struct Matrix

// column vector
class Vector {

public:
  // default constructor (don't allocate)
  Vector() : size(0), data(nullptr) {}

  // constructor with memory allocation, initialized to zero
  Vector(int size_) : Vector() {
    size = size_;
    allocate(size_);
  }

  // destructor
  ~Vector() {
    deallocate();
  }

  // access data operators
  double& operator() (int i) {
    return data[i]; }
  double  operator() (int i) const {
    return data[i]; }

  // operator assignment
  Vector& operator=(const Vector& source) {

    // self-assignment check
    if (this != &source) {
      if ( size != (source.size) ) {   // storage cannot be reused
	allocate(source.size);         // re-allocate storage
      }
      // storage can be used, copy data
      std::copy(source.data, source.data + source.size, data);
    }
    return *this;
  }

  // memory allocation
  void allocate(int size_) {

    deallocate();

    // new sizes
    size = size_;

    data = new double[size_];
    memset(data,0,size_*sizeof(double));

  } // allocate

  // memory free
  void deallocate() {

    if (data)
      delete[] data;

    data = nullptr;

  }

  //   ||x||
  double norm() {
    double sum = 0;
    for (int i = 0; i < size; i++) sum += (*this)(i) * (*this)(i);
    return sqrt(sum);
  }

  // divide data by factor
  void rescale(double factor) {
    for (int i = 0; i < size; i++) (*this)(i) /= factor;
  }

  void rescale_unit() {
    double factor = norm();
    rescale(factor);
  }

  int size;

private:
  double* data;

}; // class Vector

// c = a + b * s
void vmadd(const Vector& a, const Vector& b, double s, Vector& c)
{
  if (c.size != a.size or c.size != b.size) {
    std::cerr << "[vmadd]: vector sizes don't match\n";
    return;
  }

  for (int i = 0; i < c.size; i++)
    c(i) = a(i) + s * b(i);
}

// mat = I - 2*v*v^T
// !!! m is allocated here !!!
void compute_householder_factor(Matrix& mat, const Vector& v)
{

  int n = v.size;
  mat.allocate(n,n);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      mat(i,j) = -2 *  v(i) * v(j);
  for (int i = 0; i < n; i++)
    mat(i,i) += 1;
}

// take c-th column of a matrix, put results in Vector v
void Matrix::extract_column(Vector& v, int c) {
  if (m != v.size) {
    std::cerr << "[Matrix::extract_column]: Matrix and Vector sizes don't match\n";
    return;
  }

  for (int i = 0; i < m; i++)
    v(i) = (*this)(i,c);
}

void matrix_show(const Matrix&  m, const std::string& str="")
{
  std::cout << str << "\n";
  for(int i = 0; i < m.m; i++) {
    for (int j = 0; j < m.n; j++) {
      printf(" %8.3f", m(i,j));
    }
    printf("\n");
  }
  printf("\n");
}

// L2-norm ||A-B||^2
double matrix_compare(const Matrix& A, const Matrix& B) {
  // matrices must have same size
  if (A.m != B.m or  A.n != B.n)
    return std::numeric_limits<double>::max();

  double res=0;
  for(int i = 0; i < A.m; i++) {
    for (int j = 0; j < A.n; j++) {
      res += (A(i,j)-B(i,j)) * (A(i,j)-B(i,j));
    }
  }

  res /= A.m*A.n;
  return res;
}

void householder(Matrix& mat,
		 Matrix& R,
		 Matrix& Q)
{

  int m = mat.m;
  int n = mat.n;

  // array of factor Q1, Q2, ... Qm
  std::vector<Matrix> qv(m);

  // temp array
  Matrix z(mat);
  Matrix z1;

  for (int k = 0; k < n && k < m - 1; k++) {

    Vector e(m), x(m);
    double a;

    // compute minor
    z1.compute_minor(z, k);

    // extract k-th column into x
    z1.extract_column(x, k);

    a = x.norm();
    if (mat(k,k) > 0) a = -a;

    for (int i = 0; i < e.size; i++)
      e(i) = (i == k) ? 1 : 0;

    // e = x + a*e
    vmadd(x, e, a, e);

    // e = e / ||e||
    e.rescale_unit();

    // qv[k] = I - 2 *e*e^T
    compute_householder_factor(qv[k], e);

    // z = qv[k] * z1
    z.mult(qv[k], z1);

  }

  Q = qv[0];

  // after this loop, we will obtain Q (up to a transpose operation)
  for (int i = 1; i < n && i < m - 1; i++) {

    z1.mult(qv[i], Q);
    Q = z1;

  }

  R.mult(Q, mat);
  Q.transpose();
}

double in[][3] = {
  { 12, -51,   4},
  {  6, 167, -68},
  { -4,  24, -41},
  { -1,   1,   0},
  {  2,   0,   3},
};

int main()
{
  Matrix A(in);
  Matrix Q, R;

  matrix_show(A,"A");

  // compute QR decompostion
  householder(A, R, Q);

  matrix_show(Q,"Q");
  matrix_show(R,"R");

  // compare Q*R to the original matrix A
  Matrix A_check;
  A_check.mult(Q, R);

  // compute L2 norm ||A-A_check||^2
  double l2 = matrix_compare(A,A_check);

  // display Q*R
  matrix_show(A_check, l2 < 1e-12 ? "A == Q * R ? yes" : "A == Q * R ? no");

  return EXIT_SUCCESS;
}

```

{{out}}

```txt

A
   12.000  -51.000    4.000
    6.000  167.000  -68.000
   -4.000   24.000  -41.000
   -1.000    1.000    0.000
    2.000    0.000    3.000

Q
    0.846   -0.391    0.343    0.082    0.078
    0.423    0.904   -0.029    0.026    0.045
   -0.282    0.170    0.933   -0.047   -0.137
   -0.071    0.014   -0.001    0.980   -0.184
    0.141   -0.017   -0.106   -0.171   -0.969

R
   14.177   20.667  -13.402
   -0.000  175.043  -70.080
    0.000    0.000  -35.202
   -0.000   -0.000   -0.000
    0.000    0.000   -0.000

A == Q * R ? yes
   12.000  -51.000    4.000
    6.000  167.000  -68.000
   -4.000   24.000  -41.000
   -1.000    1.000   -0.000
    2.000   -0.000    3.000

```


## C#

{{libheader|Math.Net}}


```c#
using System;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Double;


class Program
{

    static void Main(string[] args)
    {
        Matrix<double> A = DenseMatrix.OfArray(new double[,]
        {
                {  12,  -51,    4 },
                {   6,  167,  -68 },
                {  -4,   24,  -41 }
        });
        Console.WriteLine("A:");
        Console.WriteLine(A);
        var qr = A.QR();
        Console.WriteLine();
        Console.WriteLine("Q:");
        Console.WriteLine(qr.Q);
        Console.WriteLine();
        Console.WriteLine("R:");
        Console.WriteLine(qr.R);
    }
}
```


{{out}}


```txt
A:
DenseMatrix 3x3-Double
12  -51    4
 6  167  -68
-4   24  -41


Q:
DenseMatrix 3x3-Double
-0.857143   0.394286  -0.331429
-0.428571  -0.902857  0.0342857
 0.285714  -0.171429  -0.942857


R:
DenseMatrix 3x3-Double
-14   -21  14
  0  -175  70
  0     0  35
```




## Common Lisp

Uses the routines m+, m-, .*, ./ from [[Element-wise_operations]], mmul from [[Matrix multiplication]], mtp from [[Matrix transposition]].

Helper functions:

```lisp
(defun sign (x)
  (if (zerop x)
      x
      (/ x (abs x))))

(defun norm (x)
  (let ((len (car (array-dimensions x))))
    (sqrt (loop for i from 0 to (1- len) sum (expt (aref x i 0) 2)))))

(defun make-unit-vector (dim)
  (let ((vec (make-array `(,dim ,1) :initial-element 0.0d0)))
    (setf (aref vec 0 0) 1.0d0)
    vec))

;; Return a nxn identity matrix.
(defun eye (n)
  (let ((I (make-array `(,n ,n) :initial-element 0)))
    (loop for j from 0 to (- n 1) do
          (setf (aref I j j) 1))
    I))

(defun array-range (A ma mb na nb)
  (let* ((mm (1+ (- mb ma)))
         (nn (1+ (- nb na)))
         (B (make-array `(,mm ,nn) :initial-element 0.0d0)))

    (loop for i from 0 to (1- mm) do
         (loop for j from 0 to (1- nn) do
              (setf (aref B i j)
                    (aref A (+ ma i) (+ na j)))))
    B))

(defun rows (A) (car  (array-dimensions A)))
(defun cols (A) (cadr (array-dimensions A)))
(defun mcol (A n) (array-range A 0 (1- (rows A)) n n))
(defun mrow (A n) (array-range A n n 0 (1- (cols A))))

(defun array-embed (A B row col)
  (let* ((ma (rows A))
         (na (cols A))
         (mb (rows B))
         (nb (cols B))
         (C  (make-array `(,ma ,na) :initial-element 0.0d0)))

    (loop for i from 0 to (1- ma) do
         (loop for j from 0 to (1- na) do
              (setf (aref C i j) (aref A i j))))

    (loop for i from 0 to (1- mb) do
         (loop for j from 0 to (1- nb) do
              (setf (aref C (+ row i) (+ col j))
                    (aref B i j))))

    C))

```


Main routines:

```lisp

(defun make-householder (a)
  (let* ((m    (car (array-dimensions a)))
         (s    (sign (aref a 0 0)))
         (e    (make-unit-vector m))
         (u    (m+ a (.* (* (norm a) s) e)))
         (v    (./ u (aref u 0 0)))
         (beta (/ 2 (aref (mmul (mtp v) v) 0 0))))

    (m- (eye m)
        (.* beta (mmul v (mtp v))))))

(defun qr (A)
  (let* ((m (car  (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (Q (eye m)))

    ;; Work on n columns of A.
    (loop for i from 0 to (if (= m n) (- n 2) (- n 1)) do

         ;; Select the i-th submatrix. For i=0 this means the original matrix A.
         (let* ((B (array-range A i (1- m) i (1- n)))
                ;; Take the first column of the current submatrix B.
                (x (mcol B 0))
                ;; Create the Householder matrix for the column and embed it into an mxm identity.
                (H (array-embed (eye m) (make-householder x) i i)))

           ;; The product of all H matrices from the right hand side is the orthogonal matrix Q.
           (setf Q (mmul Q H))

           ;; The product of all H matrices with A from the LHS is the upper triangular matrix R.
           (setf A (mmul H A))))

    ;; Return Q and R.
    (values Q A)))

```


Example 1:


```lisp
(qr #2A((12 -51 4) (6 167 -68) (-4 24 -41)))

#2A((-0.85  0.39  0.33)
    (-0.42 -0.90 -0.03)
    ( 0.28 -0.17  0.94))

#2A((-14.0  -21.0  14.0)
    (  0.0 -175.0  70.0)
    (  0.0    0.0 -35.0))
```


Example 2, [[Polynomial regression]]:


```lisp
(defun polyfit (x y n)
  (let* ((m (cadr (array-dimensions x)))
         (A (make-array `(,m ,(+ n 1)) :initial-element 0)))
    (loop for i from 0 to (- m 1) do
          (loop for j from 0 to n do
                (setf (aref A i j)
                      (expt (aref x 0 i) j))))
    (lsqr A (mtp y))))

;; Solve a linear least squares problem by QR decomposition.
(defun lsqr (A b)
  (multiple-value-bind (Q R) (qr A)
    (let* ((n (cadr (array-dimensions R))))
      (solve-upper-triangular (array-range R                0 (- n 1) 0 (- n 1))
                              (array-range (mmul (mtp Q) b) 0 (- n 1) 0 0)))))

;; Solve an upper triangular system by back substitution.
(defun solve-upper-triangular (R b)
  (let* ((n (cadr (array-dimensions R)))
         (x (make-array `(,n 1) :initial-element 0.0d0)))

    (loop for k from (- n 1) downto 0
       do (setf (aref x k 0)
                (/ (- (aref b k 0)
                      (loop for j from (+ k 1) to (- n 1)
                         sum (* (aref R k j)
                                (aref x j 0))))
                   (aref R k k))))
    x))
```



```lisp
;; Finally use the data:
(let ((x #2A((0 1 2 3 4 5 6 7 8 9 10)))
      (y #2A((1 6 17 34 57 86 121 162 209 262 321))))
    (polyfit x y 2))

#2A((0.999999966345088) (2.000000015144699) (2.99999999879804))
```



## D

{{trans|Common Lisp}}
Uses the functions copied from [[Element-wise_operations]], [[Matrix multiplication]], and [[Matrix transposition]].

```d
import std.stdio, std.math, std.algorithm, std.traits,
       std.typecons, std.numeric, std.range, std.conv;

template elementwiseMat(string op) {
    T[][] elementwiseMat(T)(in T[][] A, in T B) pure nothrow {
        if (A.empty)
            return null;
        auto R = new typeof(return)(A.length, A[0].length);
        foreach (immutable r, const row; A)
            R[r][] = mixin("row[] " ~ op ~ "B");
        return R;
    }

    T[][] elementwiseMat(T, U)(in T[][] A, in U[][] B)
    pure nothrow if (is(Unqual!T == Unqual!U)) {
        assert(A.length == B.length);
        if (A.empty)
            return null;
        auto R = new typeof(return)(A.length, A[0].length);
        foreach (immutable r, const row; A) {
            assert(row.length == B[r].length);
            R[r][] = mixin("row[] " ~ op ~ "B[r][]");
        }
        return R;
    }
}

alias mSum = elementwiseMat!q{ + },
      mSub = elementwiseMat!q{ - },
      pMul = elementwiseMat!q{ * },
      pDiv = elementwiseMat!q{ / };

bool isRectangular(T)(in T[][] mat) pure nothrow {
    return mat.all!(r => r.length == mat[0].length);
}

T[][] matMul(T)(in T[][] a, in T[][] b) pure nothrow
in {
    assert(a.isRectangular && b.isRectangular &&
           a[0].length == b.length);
} body {
    auto result = new T[][](a.length, b[0].length);
    auto aux = new T[b.length];
    foreach (immutable j; 0 .. b[0].length) {
        foreach (immutable k; 0 .. b.length)
            aux[k] = b[k][j];
        foreach (immutable i; 0 .. a.length)
            result[i][j] = a[i].dotProduct(aux);
    }
    return result;
}

Unqual!T[][] transpose(T)(in T[][] m) pure nothrow {
    auto r = new Unqual!T[][](m[0].length, m.length);
    foreach (immutable nr, row; m)
        foreach (immutable nc, immutable c; row)
            r[nc][nr] = c;
    return r;
}

T norm(T)(in T[][] m) pure nothrow {
    return transversal(m, 0).map!q{ a ^^ 2 }.sum.sqrt;
}

Unqual!T[][] makeUnitVector(T)(in size_t dim) pure nothrow {
    auto result = new Unqual!T[][](dim, 1);
    foreach (row; result)
        row[] = 0;
    result[0][0] = 1;
    return result;
}

/// Return a nxn identity matrix.
Unqual!T[][] matId(T)(in size_t n) pure nothrow {
    auto Id = new Unqual!T[][](n, n);
    foreach (immutable r, row; Id) {
        row[] = 0;
        row[r] = 1;
    }
    return Id;
}

T[][] slice2D(T)(in T[][] A,
                 in size_t ma, in size_t mb,
                 in size_t na, in size_t nb) pure nothrow {
    auto B = new T[][](mb - ma + 1, nb - na + 1);
    foreach (immutable i, brow; B)
        brow[] = A[ma + i][na .. na + brow.length];
    return B;
}

size_t rows(T)(in T[][] A) pure nothrow { return A.length; }

size_t cols(T)(in T[][] A) pure nothrow {
    return A.length ? A[0].length : 0;
}

T[][] mcol(T)(in T[][] A, in size_t n) pure nothrow {
    return slice2D(A, 0, A.rows - 1, n, n);
}

T[][] matEmbed(T)(in T[][] A, in T[][] B,
                  in size_t row, in size_t col) pure nothrow {
    auto C = new T[][](rows(A), cols(A));
    foreach (immutable i, const arow; A)
        C[i][] = arow[]; // Some wasted copies.
    foreach (immutable i, const brow; B)
        C[row + i][col .. col + brow.length] = brow[];
    return C;
}

// Main routines ---------------

T[][] makeHouseholder(T)(in T[][] a) {
    immutable m = a.rows;
    immutable T s = a[0][0].sgn;
    immutable e = makeUnitVector!T(m);
    immutable u = mSum(a, pMul(e, a.norm * s));
    immutable v = pDiv(u, u[0][0]);
    immutable beta = 2.0 / v.transpose.matMul(v)[0][0];
    return mSub(matId!T(m), pMul(v.matMul(v.transpose), beta));
}

Tuple!(T[][],"Q", T[][],"R") QRdecomposition(T)(T[][] A) {
    immutable m = A.rows;
    immutable n = A.cols;
    auto Q = matId!T(m);

    // Work on n columns of A.
    foreach (immutable i; 0 .. (m == n ? n - 1 : n)) {
        // Select the i-th submatrix. For i=0 this means the original
        // matrix A.
        immutable B = slice2D(A, i, m - 1, i, n - 1);

        // Take the first column of the current submatrix B.
        immutable x = mcol(B, 0);

        // Create the Householder matrix for the column and embed it
        // into an mxm identity.
        immutable H = matEmbed(matId!T(m), x.makeHouseholder, i, i);

        // The product of all H matrices from the right hand side is
        // the orthogonal matrix Q.
        Q = Q.matMul(H);

        // The product of all H matrices with A from the LHS is the
        // upper triangular matrix R.
        A  = H.matMul(A);
    }

    // Return Q and R.
    return typeof(return)(Q, A);
}

// Polynomial regression ---------------

/// Solve an upper triangular system by back substitution.
T[][] solveUpperTriangular(T)(in T[][] R, in T[][] b) pure nothrow {
    immutable n = R.cols;
    auto x = new T[][](n, 1);

    foreach_reverse (immutable k; 0 .. n) {
        T tot = 0;
        foreach (immutable j; k + 1 .. n)
            tot += R[k][j] * x[j][0];
        x[k][0] = (b[k][0] - tot) / R[k][k];
    }

    return x;
}

/// Solve a linear least squares problem by QR decomposition.
T[][] lsqr(T)(T[][] A, in T[][] b) pure nothrow {
    const qr = A.QRdecomposition;
    immutable n = qr.R.cols;
    return solveUpperTriangular(
        slice2D(qr.R, 0, n - 1, 0, n - 1),
        slice2D(qr.Q.transpose.matMul(b), 0, n - 1, 0, 0));
}

T[][] polyFit(T)(in T[][] x, in T[][] y, in size_t n) pure nothrow {
    immutable size_t m = x.cols;
    auto A = new T[][](m, n + 1);
    foreach (immutable i, row; A)
        foreach (immutable j, ref item; row)
            item = x[0][i] ^^ j;
    return lsqr(A, y.transpose);
}

void main() {
    // immutable (Q, R) = QRdecomposition([[12.0, -51,   4],
    immutable qr = QRdecomposition([[12.0, -51,   4],
                                    [ 6.0, 167, -68],
                                    [-4.0,  24, -41]]);
    immutable form = "[%([%(%2.3f, %)]%|,\n %)]\n";
    writefln(form, qr.Q);
    writefln(form, qr.R);

    immutable x = [[0.0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]];
    immutable y = [[1.0, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321]];
    polyFit(x, y, 2).writeln;
}
```

{{out}}

```txt
[[-0.857, 0.394, 0.331],
 [-0.429, -0.903, -0.034],
 [0.286, -0.171, 0.943]]

[[-14.000, -21.000, 14.000],
 [0.000, -175.000, 70.000],
 [0.000, -0.000, -35.000]]

[[1], [2], [3]]
```



## Futhark


```Futhark

import "lib/github.com/diku-dk/linalg/linalg"

module linalg_f64 = mk_linalg f64

let eye (n: i32): [n][n]f64 =
  let arr = map (\ind -> let (i,j) = (ind/n,ind%n) in if (i==j) then 1.0 else 0.0) (iota (n*n))
  in unflatten n n arr

let norm v = linalg_f64.dotprod v v |> f64.sqrt

let qr [n] [m] (a: [m][n]f64): ([m][m]f64, [m][n]f64) =

  let make_householder [d] (x: [d]f64): [d][d]f64 =
    let div = if x[0] > 0 then x[0] + norm x else x[0] - norm x
    let v = map (/div) x
    let v[0] = 1
    let fac = 2.0 / linalg_f64.dotprod v v
    in map2 (map2 (-)) (eye d) (map (map (*fac)) (linalg_f64.outer v v))

  let step ((x,y):([m][m]f64,[m][n]f64)) (i:i32): ([m][m]f64,[m][n]f64) =
    let h = eye m
    let h[i:m,i:m] = make_householder y[i:m,i]
    let q': [m][m]f64 = linalg_f64.matmul x h
    let a': [m][n]f64 = linalg_f64.matmul h y
    in (q',a')

  let q = eye m
  in foldl step (q,a) (iota n)

entry main = qr [[12.0, -51.0, 4.0],[6.0, 167.0, -68.0],[-4.0, 24.0, -41.0]]

```

{{out}}

```txt

$ ./qr
[[-0.857143f64, 0.394286f64, -0.331429f64], [-0.428571f64, -0.902857f64, 0.034286f64], [0.285714f64, -0.171429f64, -0.942857f64]]
[[-14.000000f64, -21.000000f64, 14.000000f64], [0.000000f64, -175.000000f64, 70.000000f64], [-0.000000f64, 0.000000f64, 35.000000f64]]

```



## Go

===Method of task description, library go.matrix===
{{trans|Common Lisp}}
A fairly close port of the Common Lisp solution, this solution uses the [http://github.com/skelterjohn/go.matrix go.matrix library] for supporting functions.  Note though, that go.matrix has QR decomposition, as shown in the [[Polynomial_regression#Go|Go solution]] to Polynomial regression.  The solution there is coded more directly than by following the CL example here.  Similarly, examination of the go.matrix QR source shows that it computes the decomposition more directly.

```go
package main

import (
    "fmt"
    "math"

    "github.com/skelterjohn/go.matrix"
)

func sign(s float64) float64 {
    if s > 0 {
        return 1
    } else if s < 0 {
        return -1
    }
    return 0
}

func unitVector(n int) *matrix.DenseMatrix {
    vec := matrix.Zeros(n, 1)
    vec.Set(0, 0, 1)
    return vec
}

func householder(a *matrix.DenseMatrix) *matrix.DenseMatrix {
    m := a.Rows()
    s := sign(a.Get(0, 0))
    e := unitVector(m)
    u := matrix.Sum(a, matrix.Scaled(e, a.TwoNorm()*s))
    v := matrix.Scaled(u, 1/u.Get(0, 0))
    // (error checking skipped in this solution)
    prod, _ := v.Transpose().TimesDense(v)
    β := 2 / prod.Get(0, 0)

    prod, _ = v.TimesDense(v.Transpose())
    return matrix.Difference(matrix.Eye(m), matrix.Scaled(prod, β))
}

func qr(a *matrix.DenseMatrix) (q, r *matrix.DenseMatrix) {
    m := a.Rows()
    n := a.Cols()
    q = matrix.Eye(m)

    last := n - 1
    if m == n {
        last--
    }
    for i := 0; i <= last; i++ {
        // (copy is only for compatibility with an older version of gomatrix)
        b := a.GetMatrix(i, i, m-i, n-i).Copy()
        x := b.GetColVector(0)
        h := matrix.Eye(m)
        h.SetMatrix(i, i, householder(x))
        q, _ = q.TimesDense(h)
        a, _ = h.TimesDense(a)
    }
    return q, a
}

func main() {
    // task 1: show qr decomp of wp example
    a := matrix.MakeDenseMatrixStacked([][]float64{
        {12, -51, 4},
        {6, 167, -68},
        {-4, 24, -41}})
    q, r := qr(a)
    fmt.Println("q:\n", q)
    fmt.Println("r:\n", r)

    // task 2: use qr decomp for polynomial regression example
    x := matrix.MakeDenseMatrixStacked([][]float64{
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}})
    y := matrix.MakeDenseMatrixStacked([][]float64{
        {1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}})
    fmt.Println("\npolyfit:\n", polyfit(x, y, 2))
}

func polyfit(x, y *matrix.DenseMatrix, n int) *matrix.DenseMatrix {
    m := x.Cols()
    a := matrix.Zeros(m, n+1)
    for i := 0; i < m; i++ {
        for j := 0; j <= n; j++ {
            a.Set(i, j, math.Pow(x.Get(0, i), float64(j)))
        }
    }
    return lsqr(a, y.Transpose())
}

func lsqr(a, b *matrix.DenseMatrix) *matrix.DenseMatrix {
    q, r := qr(a)
    n := r.Cols()
    prod, _ := q.Transpose().TimesDense(b)
    return solveUT(r.GetMatrix(0, 0, n, n), prod.GetMatrix(0, 0, n, 1))
}

func solveUT(r, b *matrix.DenseMatrix) *matrix.DenseMatrix {
    n := r.Cols()
    x := matrix.Zeros(n, 1)
    for k := n - 1; k >= 0; k-- {
        sum := 0.
        for j := k + 1; j < n; j++ {
            sum += r.Get(k, j) * x.Get(j, 0)
        }
        x.Set(k, 0, (b.Get(k, 0)-sum)/r.Get(k, k))
    }
    return x
}
```

Output:

```txt

q:
 {-0.857143,  0.394286,  0.331429,
 -0.428571, -0.902857, -0.034286,
  0.285714, -0.171429,  0.942857}
r:
 { -14,  -21,   14,
    0, -175,   70,
    0,    0,  -35}

polyfit:
 {1,
 2,
 3}

```


===Library QR, gonum/matrix===

```go
package main

import (
    "fmt"

    "github.com/gonum/matrix/mat64"
)

func main() {
    // task 1: show qr decomp of wp example
    a := mat64.NewDense(3, 3, []float64{
        12, -51, 4,
        6, 167, -68,
        -4, 24, -41,
    })
    var qr mat64.QR
    qr.Factorize(a)
    var q, r mat64.Dense
    q.QFromQR(&qr)
    r.RFromQR(&qr)
    fmt.Printf("q: %.3f\n\n", mat64.Formatted(&q, mat64.Prefix("   ")))
    fmt.Printf("r: %.3f\n\n", mat64.Formatted(&r, mat64.Prefix("   ")))

    // task 2: use qr decomp for polynomial regression example
    x := []float64{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    y := []float64{1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}
    a = Vandermonde(x, 2)
    b := mat64.NewDense(11, 1, y)
    qr.Factorize(a)
    var f mat64.Dense
    f.SolveQR(&qr, false, b)
    fmt.Printf("polyfit: %.3f\n",
        mat64.Formatted(&f, mat64.Prefix("         ")))
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

{{out}}

```txt

q: ⎡-0.857   0.394   0.331⎤
   ⎢-0.429  -0.903  -0.034⎥
   ⎣ 0.286  -0.171   0.943⎦

r: ⎡ -14.000   -21.000    14.000⎤
   ⎢   0.000  -175.000    70.000⎥
   ⎣   0.000     0.000   -35.000⎦

polyfit: ⎡1.000⎤
         ⎢2.000⎥
         ⎣3.000⎦

```



## Haskell

Square matrices only; decompose A and solve Rx = q by back substitution

```haskell

import Data.List
import Text.Printf (printf)

eps = 1e-6 :: Double

-- a matrix is represented as a list of columns
mmult :: Num a => [[a]] -> [[a]] -> [[a]]
nth :: Num a => [[a]] -> Int -> Int -> a
mmult_num :: Num a => [[a]] -> a -> [[a]]
madd :: Num a => [[a]] -> [[a]] -> [[a]]
idMatrix :: Num a => Int -> Int -> [[a]]

adjustWithE :: [[Double]] -> Int -> [[Double]]

mmult a b = [ [ sum $ zipWith (*) ak bj | ak <- (transpose a) ] | bj <- b ]
nth mA i j = (mA !! j) !! i
mmult_num mA n = map (\c -> map (*n) c) mA
madd mA mB = zipWith (\c1 c2 -> zipWith (+) c1 c2) mA mB
idMatrix n m = [ [if (i==j) then 1 else 0 | i <- [1..n]] | j <- [1..m]]

adjustWithE mA n = let lA = length mA in
    (idMatrix n (n - lA)) ++ (map (\c -> (take (n - lA) (repeat 0.0)) ++ c ) mA)

-- auxiliary functions
sqsum :: Floating a => [a] -> a
norm :: Floating a => [a] -> a
epsilonize :: [[Double]] -> [[Double]]

sqsum a = foldl (\x y -> x + y*y) 0 a
norm a = sqrt $! sqsum a
epsilonize mA = map (\c -> map (\x -> if abs x <= eps then 0 else x) c) mA

-- Householder transformation; householder A = (Q, R)
uTransform :: [Double] -> [Double]
hMatrix :: [Double] -> Int -> Int -> [[Double]]
householder :: [[Double]] -> ([[Double]], [[Double]])

-- householder_rec Q R A
householder_rec :: [[Double]] -> [[Double]] -> Int -> ([[Double]], [[Double]])

uTransform a = let t = (head a) + (signum (head a))*(norm a) in
    1 : map (\x -> x/t) (tail a)

hMatrix a n i = let u = uTransform (drop i a) in
    madd
        (idMatrix (n-i) (n-i))
        (mmult_num
            (mmult [u] (transpose [u]))
            ((/) (-2) (sqsum u)))

householder_rec mQ mR 0 = (mQ, mR)
householder_rec mQ mR n = let mSize = length mR in
    let mH = adjustWithE (hMatrix (mR!!(mSize - n)) mSize (mSize - n)) mSize in
        householder_rec (mmult mQ mH) (mmult mH mR) (n - 1)

householder mA = let mSize = length mA in
    let (mQ, mR) = householder_rec (idMatrix mSize mSize) mA mSize in
        (epsilonize mQ, epsilonize mR)

backSubstitution :: [[Double]] -> [Double] -> [Double] -> [Double]
backSubstitution mR [] res = res
backSubstitution mR@(hR:tR) q@(h:t) res =
    let x = (h / (head hR)) in
        backSubstitution
            (map tail tR)
            (tail (zipWith (-) q (map (*x) hR)))
            (x : res)

showMatrix :: [[Double]] -> String
showMatrix mA =
    concat $ intersperse "\n"
        (map (\x -> unwords $ printf "%10.4f" <$> (x::[Double])) (transpose mA))

mY = [[12, 6, -4], [-51, 167, 24], [4, -68, -41]] :: [[Double]]
q = [21, 245, 35] :: [Double]
main = let (mQ, mR) = householder mY in
    putStrLn ("Q: \n" ++ showMatrix mQ) >>
    putStrLn ("R: \n" ++ showMatrix mR) >>
    putStrLn ("q: \n" ++ show q) >>
    putStrLn ("x: \n" ++ show (backSubstitution (reverse (map reverse mR)) (reverse q) []))

```

{{out}}

```txt

Q:
   -0.8571     0.3943    -0.3314
   -0.4286    -0.9029     0.0343
    0.2857    -0.1714    -0.9429
R:
  -14.0000   -21.0000    14.0000
    0.0000  -175.0000    70.0000
    0.0000     0.0000    35.0000
q:
[21.0,245.0,35.0]
x:
[1.0000000000000004,-0.9999999999999999,1.0]

```



## J


'''Solution''' (built-in):
```j
   QR =: 128!:0
```

'''Solution''' (custom implementation):
```j
   mp=: +/ . *  NB. matrix product
   h =: +@|:    NB. conjugate transpose

   QR=: 3 : 0
    n=.{:$A=.y
    if. 1>:n do.
     A ((% {.@,) ; ]) %:(h A) mp A
    else.
     m =.>.n%2
     A0=.m{."1 A
     A1=.m}."1 A
     'Q0 R0'=.QR A0
     'Q1 R1'=.QR A1 - Q0 mp T=.(h Q0) mp A1
     (Q0,.Q1);(R0,.T),(-n){."1 R1
    end.
   )
```


'''Example''':
```j
   QR 12 _51 4,6 167 _68,:_4 24 _41
+-----------------------------+----------+
| 0.857143 _0.394286 _0.331429|14  21 _14|
| 0.428571  0.902857 0.0342857| 0 175 _70|
|_0.285714  0.171429 _0.942857| 0   0  35|
+-----------------------------+----------+
```


'''Example''' (polynomial fitting using QR reduction):
```j
   X=:i.# Y=:1 6 17 34 57 86 121 162 209 262 321
   'Q R'=: QR X ^/ i.3
   R %.~(|:Q)+/ .* Y
1 2 3
```

'''Notes''':J offers a built-in QR decomposition function, <tt>128!:0</tt>. If J did not offer this function as a built-in, it could be written in J along the lines of the second version, which is covered in [[j:Essays/QR Decomposition|an essay on the J wiki]].


## Java

Note: uses the [https://math.nist.gov/javanumerics/jama/ JAMA Java Matrix Package].

Compile with: '''javac -cp Jama-1.0.3.jar Decompose.java'''.


```java
import Jama.Matrix;
import Jama.QRDecomposition;

import java.io.StringWriter;
import java.io.PrintWriter;

public class Decompose {
    public static void main(String[] args) {
        Matrix matrix = new Matrix(new double[][] {
            { 12, -51,   4 },
            {  6, 167, -68 },
            { -4,  24, -41 },
        });

        QRDecomposition d = new QRDecomposition(matrix);
        System.out.print(toString(d.getQ()));
        System.out.print(toString(d.getR()));
    }

    public static String toString(Matrix m) {
        StringWriter sw = new StringWriter();
        m.print(new PrintWriter(sw, true), 8, 6);
        return sw.toString();
    }
}
```


{{out}}

```txt


 -0.857143  0.394286 -0.331429
 -0.428571 -0.902857  0.034286
  0.285714 -0.171429 -0.942857


 -14.000000 -21.000000 14.000000
  0.000000 -175.000000 70.000000
  0.000000  0.000000 35.000000

```



## Julia

Built-in function

```julia
Q, R = qr([12 -51 4; 6 167 -68; -4 24 -41])
```

{{out}}

```txt

(
3x3 Array{Float64,2}:
 -0.857143   0.394286   0.331429
 -0.428571  -0.902857  -0.0342857
  0.285714  -0.171429   0.942857 ,

3x3 Array{Float64,2}:
 -14.0   -21.0   14.0
   0.0  -175.0   70.0
   0.0     0.0  -35.0)

```



## Maple



```Maple

with(LinearAlgebra):

Q,R := QRDecomposition( evalf( <<12|-51|4>,<6|167|-68>,<-4|24|-41>>) ):

Q;
R;

```

Output:

```txt

        [-0.857142857142857   0.394285714285714    0.331428571428571]
        [                                                           ]
        [-0.428571428571429  -0.902857142857143  -0.0342857142857143]
        [                                                           ]
        [ 0.285714285714286  -0.171428571428571    0.942857142857143]

                 [-14.               -21.  14.0000000000000]
                 [                                         ]
                 [  0.  -175.000000000000  70.0000000000000]
                 [                                         ]
                 [  0.                 0.              -35.]

```



## Mathematica


```Mathematica
{q,r}=QRDecomposition[{{12, -51, 4}, {6, 167, -68}, {-4, 24, -41}}];
q//MatrixForm

-> 6/7 3/7 -(2/7)
-69/175 158/175 6/35
-58/175 6/175 -33/35

r//MatrixForm
-> 14 21 -14
   0  175 -70
   0  0  35
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
 A = [12 -51   4
       6 167 -68
      -4  24 -41];
 [Q,R]=qr(A)
```

Output:

```txt
Q =

   0.857143  -0.394286  -0.331429
   0.428571   0.902857   0.034286
  -0.285714   0.171429  -0.942857

R =

    14    21   -14
     0   175   -70
     0     0    35
```



## Maxima


```maxima
load(lapack)$   /* This may hang up in wxMaxima, if this happens, use xMaxima or plain MAxima in a terminal */

a: matrix([12, -51,   4],
          [ 6, 167, -68],
          [-4,  24, -41])$

[q, r]: dgeqrf(a)$

mat_norm(q . r - a, 1);
4.2632564145606011E-14

/* Note: the lapack package is a lisp translation of the fortran lapack library */
```

For an exact or arbitrary precision solution:
```maxima
load("linearalgebra")$
load("eigen")$
unitVector(n) := ematrix(n,1,1,1,1);
signValue(r) := block([s:sign(r)],
  if s='pos then 1 else if s='zero then 0 else -1);
householder(a) := block([m : length(a),u,v,beta],
  u : a + sqrt(a .  a)*signValue(a[1,1])*unitVector(m),
  v : u / u[1,1],
  beta : 2/(v . v),
  diagmatrix(m,1) - beta*transpose(v . transpose(v)));
getSubmatrix(obj,i1,j1,i2,j2) :=
genmatrix(lambda([i,j], obj[i+i1-1,j+j1-1]),i2-i1+1,j2-j1+1);
setSubmatrix(obj,i1,j1,subobj) := block([m,n],
  [m,n] : matrix_size(subobj),
  for i: 0 thru m-1 do
  (for j: 0 thru n-1 do
    obj[i1+i,j1+j] : subobj[i+1,j+1]));
qr(obj) := block([m,n,qm,rm,i],
  [m,n] : matrix_size(obj),
  qm : diagmatrix(m,1),
  rm : copymatrix(obj),
  for i: 1 thru (if m=n then n-1 else n) do
  block([x,h],
    x : getSubmatrix(rm,i,i,m,i),
    h : diagmatrix(m,1),
    setSubmatrix(h,i,i,householder(x)),
    qm : qm . h,
    rm : h . rm),
  [qm,rm]);
solveUpperTriangular(r,b) := block([n,x,index,k],
  n : second(matrix_size(r)),
  x : genmatrix(lambda([a, b], 0), n, 1),
  for k: n thru 1 step -1 do
  (index : min(n,k+1),
    x[k,1] : (b[k,1] - (getSubmatrix(r,k,index,k,n) . getSubmatrix(x,index,1,n,1)))/r[k,k]),
  x);
lsqr(a,b) := block([q,r,n],
  [q,r] : qr(a),
  n : second(matrix_size(r)),
  solveUpperTriangular(getSubmatrix(r,1,1,n,n), transpose(q) . b));
polyfit(x,y,n) := block([a,j],
  a : genmatrix(lambda([i,j], if j=1 then 1.0b0 else bfloat(x[i,1]^(j-1))),
    length(x),n+1),
  lsqr(a,y));
```
Then we have the examples:
```maxima
(%i) [q,r] : qr(a);

                 [   6   69     58   ]
                 [ - -   ---    ---  ]
                 [   7   175    175  ]
                 [                   ]  [ - 14  - 21    14  ]
                 [   3    158     6  ]  [                   ]
(%o)            [[ - -  - ---  - --- ], [  0    - 175   70  ]]
                 [   7    175    175 ]  [                   ]
                 [                   ]  [  0      0    - 35 ]
                 [  2     6     33   ]
                 [  -   - --    --   ]
                 [  7     35    35   ]
(%i) mat_norm(q . r - a, 1);

(%o)                                   0
(%i) x : transpose(matrix([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))$

(%i) y : transpose(matrix([1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321]))$

(%i) fpprec : 30$

(%i) polyfit(x, y, 2);

                    [ 9.99999999999999999999999999996b-1 ]
                    [                                    ]
(%o)                [ 2.00000000000000000000000000002b0  ]
                    [                                    ]
                    [               3.0b0                ]
```



## PARI/GP

{{works with|PARI/GP|2.6.0 and above}}

```parigp
matqr(M)
```



## Perl

Letting the <code>PDL</code> module do all the work.

```perl
use strict;
use warnings;

use PDL;
use PDL::LinearAlgebra qw(mqr);

my $a = pdl(
      [12, -51,   4],
      [ 6, 167, -68],
      [-4,  24, -41],
      [-1,   1,   0],
      [ 2,   0,   3]
);

my ($q, $r) = mqr($a);
print $q, $r, $q x $r;
```

{{out}}

```txt
[
 [ -0.84641474   0.39129081  -0.34312406]
 [ -0.42320737  -0.90408727  0.029270162]
 [  0.28213825  -0.17042055  -0.93285599]
 [ 0.070534562 -0.014040652  0.001099372]
 [ -0.14106912  0.016655511   0.10577161]
]

[
 [-14.177447 -20.666627  13.401567]
 [         0 -175.04254  70.080307]
 [         0          0  35.201543]
]

[
 [           12           -51             4]
 [            6           167           -68]
 [           -4            24           -41]
 [           -1             1             0]
 [            2             0             3]
]
```



## Perl 6

{{Works with|rakudo|2018.06}}

```perl6
# sub householder translated from https://codereview.stackexchange.com/questions/120978/householder-transformation

use v6;

sub identity(Int:D $m --> Array of Array) {
   my Array @M;

   for 0 ..^ $m -> $i {
      @M.push: [0 xx $m];
      @M[$i; $i] = 1;
   }

   @M;
}

multi multiply(Array:D @A, @b where Array:D --> Array) {
   my @c;

   for ^@A X ^@b -> ($i, $j) {
      @c[$i] += @A[$i; $j] * @b[$j];
   }

   @c;
}

multi multiply(Array:D @A, Array:D @B --> Array of Array) {
   my Array @C;

   for ^@A X ^@B[0] -> ($i, $j) {
      @C[$i; $j] += @A[$i; $_] * @B[$_; $j] for ^@B;
   }

   @C;
}

sub transpose(Array:D @M --> Array of Array) {
   my ($rows, $cols) = (@M.elems, @M[0].elems);

   my Array @T;

   for ^$cols X ^$rows -> ($j, $i) {
      @T[$j; $i] = @M[$i; $j];
   }

   @T;
}

####################################################
# NOTE: @A gets overwritten and becomes @R, only need
# to return @Q.
####################################################
sub householder(Array:D @A --> Array) {
   my Int ($m, $n) = (@A.elems, @A[0].elems);
   my @v = 0 xx $m;
   my Array @Q = identity($m);

   for 0 ..^ $n -> $k {
      my Real $sum = 0;
      my Real $A0 = @A[$k; $k];
      my Int $sign = $A0 < 0 ?? -1 !! 1;

      for $k ..^ $m -> $i {
         $sum += @A[$i; $k] * @A[$i; $k];
      }

      my Real $sqr_sum = $sign * sqrt($sum);
      my Real $tmp = sqrt(2 * ($sum + $A0 * $sqr_sum));
      @v[$k] = ($sqr_sum  + $A0) / $tmp;

      for ($k + 1) ..^ $m -> $i {
         @v[$i] = @A[$i; $k] / $tmp;
      }

      for 0 ..^ $n -> $j {
         $sum = 0;

         for $k ..^ $m -> $i {
            $sum += @v[$i] * @A[$i; $j];
         }

         for $k ..^ $m -> $i {
            @A[$i; $j] -= 2 * @v[$i] * $sum;
         }
      }

      for 0 ..^ $m -> $j {
         $sum = 0;

         for $k ..^ $m -> $i {
            $sum += @v[$i] * @Q[$i; $j];
         }

         for $k ..^ $m -> $i {
            @Q[$i; $j] -= 2 * @v[$i] * $sum;
         }
      }
   }

   @Q
}

sub dotp(@a where Array:D, @b where Array:D --> Real) {
   [+] @a >>*<< @b;
}

sub upper-solve(Array:D @U, @b where Array:D, Int:D $n --> Array) {
   my @y = 0 xx $n;

   @y[$n - 1] = @b[$n - 1] / @U[$n - 1; $n - 1];

   for reverse ^($n - 1) -> $i {
      @y[$i] = (@b[$i] - (dotp(@U[$i], @y))) / @U[$i; $i];
   }

   @y;
}

sub polyfit(@x where Array:D, @y where Array:D, Int:D $n) {
   my Int $m = @x.elems;
   my Array @V;

   # Vandermonde matrix
   for ^$m X (0 .. $n) -> ($i, $j) {
      @V[$i; $j] = @x[$i] ** $j
   }

   # least squares
   my $Q = householder(@V);
   my @b = multiply($Q, @y);

   return upper-solve(@V, @b, $n + 1);
}

sub print-mat(Array:D @M, Str:D $name) {
   my Int ($m, $n) = (@M.elems, @M[0].elems);
   print "\n$name:\n";

   for 0 ..^ $m -> $i {
      for 0 ..^ $n -> $j {
         print @M[$i; $j].fmt("%12.6f ");
      }

      print "\n";
   }
}

sub MAIN() {
   ############
   # 1st part #
   ############
   my Array @A = (
      [12, -51,   4],
      [ 6, 167, -68],
      [-4,  24, -41],
      [-1,   1,   0],
      [ 2,   0,   3]
   );

   print-mat(@A, 'A');
   my $Q = householder(@A);
   $Q = transpose($Q);
   print-mat($Q, 'Q');
   # after householder, @A is now @R
   print-mat(@A, 'R');
   print-mat(multiply($Q, @A), 'check Q x R = A');

   ############
   # 2nd part #
   ############
   my @x = [^11];
   my @y = [1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321];

   my @coef = polyfit(@x, @y, 2);

   say
      "\npolyfit:\n",
      <constant X X^2>.fmt("%12s"),
      "\n",
      @coef.fmt("%12.6f");
}
```


output:

```txt

A:
   12.000000   -51.000000     4.000000
    6.000000   167.000000   -68.000000
   -4.000000    24.000000   -41.000000
   -1.000000     1.000000     0.000000
    2.000000     0.000000     3.000000

Q:
   -0.846415     0.391291    -0.343124     0.066137    -0.091462
   -0.423207    -0.904087     0.029270     0.017379    -0.048610
    0.282138    -0.170421    -0.932856    -0.021942     0.143712
    0.070535    -0.014041     0.001099     0.997401     0.004295
   -0.141069     0.016656     0.105772     0.005856     0.984175

R:
  -14.177447   -20.666627    13.401567
   -0.000000  -175.042539    70.080307
    0.000000     0.000000    35.201543
   -0.000000     0.000000     0.000000
    0.000000    -0.000000     0.000000

check Q x R = A:
   12.000000   -51.000000     4.000000
    6.000000   167.000000   -68.000000
   -4.000000    24.000000   -41.000000
   -1.000000     1.000000    -0.000000
    2.000000    -0.000000     3.000000

polyfit:
    constant            X          X^2
    1.000000     2.000000     3.000000


```




## Phix

using matrix_mul from [[Matrix_multiplication#Phix]]

```Phix
-- demo/rosettacode/QRdecomposition.exw
function vtranspose(sequence v)
-- transpose a vector of length m into an mx1 matrix,
--                       eg {1,2,3} -> {{1},{2},{3}}
    for i=1 to length(v) do v[i] = {v[i]} end for
    return v
end function

function mat_col(sequence a, integer col)
sequence res = repeat(0,length(a))
    for i=col to length(a) do
        res[i] = a[i,col]
    end for
    return res
end function

function mat_norm(sequence a)
    atom res = 0
    for i=1 to length(a) do
        res += a[i]*a[i]
    end for
    res = sqrt(res)
    return res
end function

function mat_ident(integer n)
    sequence res = repeat(repeat(0,n),n)
    for i=1 to n do
        res[i,i] = 1
    end for
    return res
end function

function QRHouseholder(sequence a)
integer columns = length(a[1]),
        rows = length(a),
        m = max(columns,rows),
        n = min(rows,columns)
sequence q, I = mat_ident(m), Q = I, u, v

--
-- Programming note: The code of this main loop was not as easily
-- written as the first glance might suggest. Explicitly setting
-- to 0 any a[i,j] [etc] that should be 0 but have inadvertently
-- gotten set to +/-1e-15 or thereabouts may be advisable. The
-- commented-out code was retrieved from a backup and should be
-- treated as an example and not be trusted (iirc, it made no
-- difference to the test cases used, so I deleted it, and then
-- had second thoughts a few days later).
--
    for j=1 to min(m-1,n) do
        u = mat_col(a,j)
        u[j] -= mat_norm(u)
        v = sq_div(u,mat_norm(u))
        q = sq_sub(I,sq_mul(2,matrix_mul(vtranspose(v),{v})))
        a = matrix_mul(q,a)
--      for row=j+1 to length(a) do
--          a[row][j] = 0
--      end for
        Q = matrix_mul(Q,q)
    end for

    -- Get the upper triangular matrix R.
    sequence R = repeat(repeat(0,n),m)
    for i=1 to n do -- (logically 1 to m(>=n), but no need)
        for j=i to n do
            R[i,j] = a[i,j]
        end for
    end for

    return {Q,R}
end function

sequence a = {{12, -51,   4},
              { 6, 167, -68},
              {-4,  24, -41}},
         {q,r} = QRHouseholder(a)

?"A"        pp(a,{pp_Nest,1})
?"Q"        pp(q,{pp_Nest,1})
?"R"        pp(r,{pp_Nest,1})
?"Q * R"    pp(matrix_mul(q,r),{pp_Nest,1})
```

{{out}}

```txt

"A"
{{12,-51,4},
 {6,167,-68},
 {-4,24,-41}}
"Q"
{{0.8571428571,-0.3942857143,0.3314285714},
 {0.4285714286,0.9028571429,-0.03428571429},
 {-0.2857142857,0.1714285714,0.9428571429}}
"R"
{{14,21,-14},
 {0,175,-70},
 {0,0,-35}}
"Q * R"
{{12,-51,4},
 {6,167,-68},
 {-4,24,-41}}

```

using matrix_transpose from [[Matrix_transposition#Phix]]

```Phix
procedure least_squares()
    sequence x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
             y = {1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321},
             a = repeat(repeat(0,3),length(x))
    for i=1 to length(x) do
        for j=1 to 3 do
            a[i,j] = power(x[i],j-1)
        end for
    end for
    {q,r} = QRHouseholder(a)
    sequence t = matrix_transpose(q),
             b = matrix_mul(t,vtranspose(y)),
             z = repeat(0,3)
    for k=3 to 1 by -1 do
        atom s = 0
        if k<3 then
            for j = k+1 to 3 do
                s += r[k,j]*z[j]
            end for
        end if
        z[k] = (b[k][1]-s)/r[k,k]
    end for
    ?{"Least-squares solution:",z}
end procedure
least_squares()
```

{{out}}

```txt

{"Least-squares solution:",{1.0,2.0,3.0}}

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
$QR = qr $A
$ps = (polyfit $x $y 2)
"Q = "
show $QR.Q
"R = "
show $QR.R
"polyfit "
"X^2 X constant"
"$(polyfit $x $y 2)"

```

{{out}}

```txt

Q =
-0.857142857142857 0.394285714285714 -0.331428571428571
-0.428571428571429 -0.902857142857143 0.0342857142857143
0.285714285714286 -0.171428571428571 -0.942857142857143
R =
-14 -21 14
8.88178419700125E-16 -175 70
-4.44089209850063E-16 0 35
polyfit
X^2 X constant
3 1.99999999999998 1.00000000000005

```



## Python

{{libheader|NumPy}}
Numpy has a qr function but here is a reimplementation to show construction and use of the Householder reflections.

```python
#!/usr/bin/env python3

import numpy as np

def qr(A):
    m, n = A.shape
    Q = np.eye(m)
    for i in range(n - (m == n)):
        H = np.eye(m)
        H[i:, i:] = make_householder(A[i:, i])
        Q = np.dot(Q, H)
        A = np.dot(H, A)
    return Q, A

def make_householder(a):
    v = a / (a[0] + np.copysign(np.linalg.norm(a), a[0]))
    v[0] = 1
    H = np.eye(a.shape[0])
    H -= (2 / np.dot(v, v)) * np.dot(v[:, None], v[None, :])
    return H

# task 1: show qr decomp of wp example
a = np.array(((
    (12, -51,   4),
    ( 6, 167, -68),
    (-4,  24, -41),
)))

q, r = qr(a)
print('q:\n', q.round(6))
print('r:\n', r.round(6))

# task 2: use qr decomp for polynomial regression example
def polyfit(x, y, n):
    return lsqr(x[:, None]**np.arange(n + 1), y.T)

def lsqr(a, b):
    q, r = qr(a)
    _, n = r.shape
    return np.linalg.solve(r[:n, :], np.dot(q.T, b)[:n])

x = np.array((0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
y = np.array((1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321))

print('\npolyfit:\n', polyfit(x, y, 2))
```

{{out}}

```txt

q:
 [[-0.857143  0.394286  0.331429]
 [-0.428571 -0.902857 -0.034286]
 [ 0.285714 -0.171429  0.942857]]
r:
 [[ -14.  -21.   14.]
 [   0. -175.   70.]
 [   0.    0.  -35.]]

polyfit:
 [ 1.  2.  3.]

```



## R


```r
# R has QR decomposition built-in (using LAPACK or LINPACK)

a <- matrix(c(12, -51, 4, 6, 167, -68, -4, 24, -41), nrow=3, ncol=3, byrow=T)
d <- qr(a)
qr.Q(d)
qr.R(d)

# now fitting a polynomial
x <- 0:10
y <- 3*x^2 + 2*x + 1

# using QR decomposition directly
a <- cbind(1, x, x^2)
qr.coef(qr(a), y)

# using least squares
a <- cbind(x, x^2)
lsfit(a, y)$coefficients

# using a linear model
xx <- x*x
m <- lm(y ~ x + xx)
coef(m)
```



## Racket


Racket has QR-decomposition builtin:

```racket

> (require math)
> (matrix-qr (matrix [[12 -51   4]
                      [ 6 167 -68]
                      [-4  24 -41]]))
(array #[#[6/7 -69/175 -58/175] #[3/7 158/175 6/175] #[-2/7 6/35 -33/35]])
(array #[#[14 21 -14] #[0 175 -70] #[0 0 35]])

```


The builtin QR-decomposition uses the Gram-Schmidt algorithm.

Here is an implementation of the Householder method:

```racket

#lang racket
(require math/matrix math/array)
(define-values (T I col size)
  (values ; short names
   matrix-transpose identity-matrix matrix-col matrix-num-rows))

(define (scale c A) (matrix-scale A c))
(define (unit n i) (build-matrix n 1 (λ (j _) (if (= j i) 1 0))))

(define (H u)
  (matrix- (I (size u))
           (scale (/ 2 (matrix-dot u u))
                  (matrix* u (T u)))))

(define (normal a)
  (define a0 (matrix-ref a 0 0))
  (matrix- a (scale (* (sgn a0) (matrix-2norm a))
                    (unit (size a) 0))))

(define (QR A)
  (define n (size A))
  (for/fold ([Q (I n)] [R A]) ([i (- n 1)])
    (define Hi (H (normal (submatrix R (:: i n) (:: i (+ i 1))))))
    (define Hi* (if (= i 0) Hi (block-diagonal-matrix (list (I i) Hi))))
    (values (matrix* Q Hi*) (matrix* Hi* R))))

(QR (matrix [[12 -51   4]
               [ 6 167 -68]
               [-4  24 -41]]))

```

Output:

```racket

(array #[#[6/7 69/175 -58/175]
             #[3/7 -158/175 6/175]
             #[-2/7 -6/35 -33/35]])
(array #[#[14 21 -14]
             #[0 -175 70]
             #[0 0 35]])

```



## Rascal

[[File:Qrresult.jpeg||200px|thumb|right]]
This function applies the Gram Schmidt algorithm. Q is printed in the console, R can be printed or visualized.


```Rascal
import util::Math;
import Prelude;
import vis::Figure;
import vis::Render;

public rel[real,real,real] QRdecomposition(rel[real x, real y, real v] matrix){
	//orthogonalcolumns
	oc = domainR(matrix, {0.0});
	for (x <- sort(toList(domain(matrix)-{0.0}))){
		c = domainR(matrix, {x});
		o = domainR(oc, {x-1});

		for (n <- [1.0 .. x]){
			o = domainR(oc, {n-1});
			c = matrixSubtract(c, matrixMultiplybyN(o, matrixDotproduct(o, c)/matrixDotproduct(o, o)));
			}

		oc += c;
	}

	Q = {};
	//from orthogonal to orthonormal columns
	for (el <- oc){
		c = domainR(oc, {el[0]});
		Q += matrixNormalize({el}, c);
	}

	//from Q to R
	R= matrixMultiplication(matrixTranspose(Q), matrix);
	R= {<x,y,toReal(round(v))> | <x,y,v> <- R};

	println("Q:");
	iprintlnExp(Q);
	println();
	println("R:");
	return R;
}

//a function that takes the transpose of a matrix, see also Rosetta Code problem "Matrix transposition"
public rel[real, real, real] matrixTranspose(rel[real x, real y, real v] matrix){
	return {<y, x, v> | <x, y, v> <- matrix};
}

//a function to normalize an element of a matrix by the normalization of a column
public rel[real,real,real] matrixNormalize(rel[real x, real y, real v] element, rel[real x, real y, real v] column){
	normalized = 1.0/nroot((0.0 | it + v*v | <x,y,v> <- column), 2);
	return matrixMultiplybyN(element, normalized);
}

//a function that takes the dot product, see also Rosetta Code problem "Dot product"
public real matrixDotproduct(rel[real x, real y, real v] column1, rel[real x, real y, real v] column2){
	return (0.0 | it + v1*v2 | <x1,y1,v1> <- column1, <x2,y2,v2> <- column2, y1==y2);
}

//a function to subtract two columns
public rel[real,real,real] matrixSubtract(rel[real x, real y, real v] column1, rel[real x, real y, real v] column2){
	return {<x1,y1,v1-v2> | <x1,y1,v1> <- column1, <x2,y2,v2> <- column2, y1==y2};
}

//a function to multiply a column by a number
public rel[real,real,real] matrixMultiplybyN(rel[real x, real y, real v] column, real n){
	return {<x,y,v*n> | <x,y,v> <- column};
}

//a function to perform matrix multiplication, see also Rosetta Code problem "Matrix multiplication".
public rel[real, real, real] matrixMultiplication(rel[real x, real y, real v] matrix1, rel[real x, real y, real v] matrix2){
	if (max(matrix1.x) == max(matrix2.y)){
		p = {<x1,y1,x2,y2, v1*v2> | <x1,y1,v1> <- matrix1, <x2,y2,v2> <- matrix2};

		result = {};
		for (y <- matrix1.y){
			for (x <- matrix2.x){
				v = (0.0 | it + v | <x1, y1, x2, y2, v> <- p,  x==x2 && y==y1, x1==y2 && y2==x1);
				result += <x,y,v>;
			}
		}
		return result;
	}
	else throw "Matrix sizes do not match.";
}

// a function to visualize the result
public void displayMatrix(rel[real x, real y, real v] matrix){
	points = [box(text("<v>"), align(0.3333*(x+1),0.3333*(y+1)),shrink(0.25)) | <x,y,v> <- matrix];
	render(overlay([*points], aspectRatio(1.0)));
}

//a matrix, given by a relation of <x-coordinate, y-coordinate, value>.
public rel[real x, real y, real v] matrixA = {
<0.0,0.0,12.0>, <0.0,1.0, 6.0>, <0.0,2.0,-4.0>,
<1.0,0.0,-51.0>, <1.0,1.0,167.0>, <1.0,2.0,24.0>,
<2.0,0.0,4.0>, <2.0,1.0,-68.0>, <2.0,2.0,-41.0>
};
```


Example using visualization

```txt
rascal>displayMatrix(QRdecomposition(matrixA))

Q:
{
  <1.0,0.0,-0.394285714285714285714285714285714285714285714285714285714285714285713300>,
  <0.0,0.0,0.857142857142857142857142857142857142857142857142857142857142857142840>,
  <0.0,1.0,0.428571428571428571428571428571428571428571428571428571428571428571420>,
  <0.0,2.0,-0.285714285714285714285714285714285714285714285714285714285714285714280>,
  <2.0,0.0,-0.33142857142857142857142857142857142857142857142857142857142857142858800>,
  <1.0,2.0,0.171428571428571428571428571428571428571428571428571428571428571428571000>,
  <2.0,2.0,-0.94285714285714285714285714285714285714285714285714285714285714285719000>,
  <1.0,1.0,0.902857142857142857142857142857142857142857142857142857142857142857140600>,
  <2.0,1.0,0.03428571428571428571428571428571428571428571428571428571428571428571600>
}
See R in picture
```



## SAS


```sas
/* See http://support.sas.com/documentation/cdl/en/imlug/63541/HTML/default/viewer.htm#imlug_langref_sect229.htm */

proc iml;
a={12 -51 4,6 167 -68,-4 24 -41};
print(a);
call qr(q,r,p,d,a);
print(q);
print(r);
quit;

/*
                  a

           12       -51         4
            6       167       -68
           -4        24       -41


                  q

    -0.857143 0.3942857 -0.331429
    -0.428571 -0.902857 0.0342857
    0.2857143 -0.171429 -0.942857


                  r

          -14       -21        14
            0      -175        70
            0         0        35
*/
```


## Scala

{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/NMueO16uQl6oivliBKZHew Scastie (remote JVM)].

```Scala
import java.io.{PrintWriter, StringWriter}

import Jama.{Matrix, QRDecomposition}

object QRDecomposition extends App {
  val matrix =
    new Matrix(
      Array[Array[Double]](Array(12, -51, 4),
        Array(6, 167, -68),
        Array(-4, 24, -41)))
  val d = new QRDecomposition(matrix)

  def toString(m: Matrix): String = {
    val sw = new StringWriter
    m.print(new PrintWriter(sw, true), 8, 6)
    sw.toString
  }

  print(toString(d.getQ))
  print(toString(d.getR))

}
```



## SequenceL

{{trans|Go}}

```sequencel>import <Utilities/Math.sl
;
import <Utilities/Sequence.sl>;
import <Utilities/Conversion.sl>;

main :=
    let
        qrTest := [[12.0, -51.0,   4.0],
                   [ 6.0, 167.0, -68.0],
                   [-4.0,  24.0, -41.0]];

        qrResult := qr(qrTest);

        x := 1.0*(0 ... 10);
        y := 1.0*[1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321];

        regResult := polyfit(x, y, 2);
    in
        "q:\n" ++ delimit(delimit(floatToString(qrResult[1], 6), ','), '\n') ++ "\n\n" ++
        "r:\n" ++ delimit(delimit(floatToString(qrResult[2], 1), ','), '\n') ++ "\n\n" ++
        "polyfit:\n" ++ "[" ++ delimit(floatToString(regResult, 1), ',') ++ "]";

//---Polynomial Regression---

polyfit(x(1), y(1), n) :=
    let
        a[j] := x ^ j foreach j within 0 ... n;
    in
        lsqr(transpose(a), transpose([y]));

lsqr(a(2), b(2)) :=
    let
        qrDecomp := qr(a);
        prod := mm(transpose(qrDecomp[1]), b);
    in
        solveUT(qrDecomp[2], prod);

solveUT(r(2), b(2)) :=
    let
        n := size(r[1]);
    in
        solveUTHelper(r, b, n, duplicate(0.0, n));

solveUTHelper(r(2), b(2), k, x(1)) :=
    let
        n := size(r[1]);
        newX :=  setElementAt(x, k, (b[k][1] - sum(r[k][(k+1) ... n] * x[(k+1) ... n])) / r[k][k]);
    in
        x when k <= 0
    else
        solveUTHelper(r, b, k - 1, newX);

//---QR Decomposition---

qr(A(2)) := qrHelper(A, id(size(A)), 1);

qrHelper(A(2), Q(2), i) :=
    let
        m := size(A);
        n := size(A[1]);

        householder := makeHouseholder(A[i ... m, i]);

        H[j,k] :=
                householder[j - i + 1][k - i + 1] when j >= i and k >= i
            else
                1.0 when j = k else 0.0
            foreach j within 1 ... m,
                    k within 1 ... m;
    in
        [Q,A] when i > (n - 1 when m = n else n)
    else
        qrHelper(mm(H, A), mm(Q, H), i + 1);


makeHouseholder(a(1)) :=
    let
        v := [1.0] ++ tail(a / (a[1] + sqrt(sum(a ^ 2)) * sign(a[1])));

        H := id(size(a)) - (2.0 / mm([v], transpose([v])))[1,1] * mm(transpose([v]), [v]);
    in
        H;

//---Utilities---

id(n)[i,j] := 1.0 when i = j else 0.0
              foreach i within 1 ... n,
                      j within 1 ... n;

mm(A(2), B(2))[i,j] := sum( A[i] * transpose(B)[j] );
```


{{out}}

```txt

"q:
-0.857143,0.394286,0.331429
-0.428571,-0.902857,-0.034286
0.285714,-0.171429,0.942857

r:
-14.0,-21.0,14.0
-0.0,-175.0,70.0
0.0,0.0,-35.0

polyfit:
[1.0,2.0,3.0]"

```



## SPAD

See [[QR_decomposition#Axiom]] in Axiom.


## Standard ML

{{trans|Axiom}}
We first define a signature for a radical category joined with a field. We then define a functor with (a) structures to define operators and functions for Array and Array2, and (b) functions for the QR decomposition:

```sml
signature RADCATFIELD = sig
type real
val zero : real
val one : real
val + : real * real -> real
val - : real * real -> real
val * : real * real -> real
val / : real * real -> real
val sign : real -> real
val sqrt : real -> real
end

functor QR(F: RADCATFIELD) = struct
structure A = struct
local
    open Array
in
fun unitVector n = tabulate (n, fn i => if i=0 then F.one else F.zero)
fun map f x = tabulate(length x, fn i => f(sub(x,i)))
fun map2 f (x, y) = tabulate(length x, fn i => f(sub(x,i),sub(y,i)))
val op + = map2 F.+
val op - = map2 F.-
val op * = map2 F.*
fun multc(c,x) = array(length x,c)*x
fun dot (x,y) = foldl F.+ F.zero (x*y)
fun outer f (x,y) =
    Array2.tabulate Array2.RowMajor (length x, length y,
				     fn (i,j) => f(sub(x,i),sub(y,j)))
fun copy x = map (fn x => x) x
fun fromVector v = tabulate(Vector.length v, fn i => Vector.sub(v,i))
fun slice(x,i,sz) =
    let	open ArraySlice
	val s = slice(x,i,sz)
    in Array.tabulate(length s, fn i => sub(s,i)) end
end
end
structure M = struct
local
    open Array2
in
fun map f x = tabulate RowMajor (nRows x, nCols x, fn (i,j) => f(sub(x,i,j)))
fun map2 f (x, y) =
    tabulate RowMajor (nRows x, nCols x, fn (i,j) => f(sub(x,i,j),sub(y,i,j)))
fun scalarMatrix(m, x) = tabulate RowMajor (m,m,fn (i,j) => if i=j then x else F.zero)
fun multc(c, x) = map (fn xij => F.*(c,xij)) x
val op + = map2 F.+
val op - = map2 F.-
fun column(x,i) = A.fromVector(Array2.column(x,i))
fun row(x,i) = A.fromVector(Array2.row(x,i))
fun x*y = tabulate RowMajor (nRows x, nCols y,
			     fn (i,j) => A.dot(row(x,i), column(y,j)))
fun multa(x,a) = Array.tabulate (nRows x, fn i => A.dot(row(x,i), a))
fun copy x = map (fn x => x) x
fun subMatrix(h, i1, i2, j1, j2) =
    tabulate RowMajor (Int.+(Int.-(i2,i1),1),
		       Int.+(Int.-(j2,j1),1),
		       fn (a,b) => sub(h,Int.+(i1,a),Int.+(j1,b)))
fun transpose m = tabulate RowMajor (nCols m,
				     nRows m,
				     fn (i,j) => sub(m,j,i))
fun updateSubMatrix(h,i,j,s) =
    tabulate RowMajor (nRows s, nCols s, fn (a,b) => update(h,Int.+(i,a),Int.+(j,b),sub(s,a,b)))
end
end
fun toList a =
    List.tabulate(Array2.nRows a, fn i => List.tabulate(Array2.nCols a, fn j => Array2.sub(a,i,j)))
fun householder a =
    let open Array
	val m = length a
	val len = F.sqrt(A.dot(a,a))
	val u = A.+(a, A.multc(F.*(len,F.sign(sub(a,0))), A.unitVector m))
	val v = A.multc(F./(F.one,sub(u,0)), u)
	val beta = F./(F.+(F.one,F.one),A.dot(v,v))
    in
	M.-(M.scalarMatrix(m,F.one), M.multc(beta,A.outer F.* (v,v)))
    end
fun qr mat =
    let open Array2
	val (m,n) = dimensions mat
	val upperIndex = if m=n then Int.-(n,1) else n
	fun loop(i,qm,rm) = if i=upperIndex then {q=qm,r=rm} else
			    let val x = A.slice(A.fromVector(column(rm,i)),i,NONE)
				val h = M.scalarMatrix(m,F.one)
				val _ = M.updateSubMatrix(h,i,i,householder x)
			    in
				loop(Int.+(i,1), M.*(qm,h), M.*(h,rm))
			    end
    in
	loop(0, M.scalarMatrix(m,F.one), mat)
    end
fun solveUpperTriangular(r,b) =
    let open Array
	val n = Array2.nCols r
	val x = array(n, F.zero)
	fun loop k =
	    let val index = Int.min(Int.-(n,1),Int.+(k,1))
		val _ = update(x,k,
			       F./(F.-(sub(b,k),
				       A.dot(A.slice(x,index,NONE),
					     A.slice(M.row(r,k),index,NONE))),
				   Array2.sub(r,k,k)))
	    in
		if k=0 then x else loop(Int.-(k,1))
	    end
    in
	loop (Int.-(n,1))
    end
fun lsqr(a,b) =
    let val {q,r} = qr a
	val n = Array2.nCols r
    in
	solveUpperTriangular(M.subMatrix(r, 0, Int.-(n,1), 0, Int.-(n,1)),
			     M.multa(M.transpose(q), b))
    end
fun pow(x,1) = x
  | pow(x,n) = F.*(x,pow(x,Int.-(n,1)))
fun polyfit(x,y,n) =
    let open Array2
	val a = tabulate RowMajor (Array.length x,
				   Int.+(n,1),
				   fn (i,j) => if j=0 then F.one else
					       pow(Array.sub(x,i),j))
    in
	lsqr(a,y)
    end
end
```

We can then show the examples:
```sml
structure RealRadicalCategoryField : RADCATFIELD = struct
open Real
val one = 1.0
val zero = 0.0
val sign = real o Real.sign
val sqrt = Real.Math.sqrt
end

structure Q = QR(RealRadicalCategoryField);

let
    val mat = Array2.fromList [[12.0, ~51.0, 4.0], [6.0, 167.0, ~68.0], [~4.0, 24.0, ~41.0]]
    val {q,r} = Q.qr(mat)
in
    {q=Q.toList q; r=Q.toList r}
end;
(* output *)
val it =
  {q=[[~0.857142857143,0.394285714286,0.331428571429],
      [~0.428571428571,~0.902857142857,~0.0342857142857],
      [0.285714285714,~0.171428571429,0.942857142857]],
   r=[[~14.0,~21.0,14.0],[5.97812397875E~18,~175.0,70.0],
      [4.47505280695E~16,0.0,~35.0]]} : {q:real list list, r:real list list}

let open Array
    val x = fromList [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    val y = fromList [1.0, 6.0, 17.0, 34.0, 57.0, 86.0, 121.0, 162.0, 209.0, 262.0, 321.0]
in
    Q.polyfit(x, y, 2)
end;

(* output *)
val it = [|1.0,2.0,3.0|] : real array
```



## Stata

See [http://www.stata.com/help.cgi?mf_qrd QR decomposition] in Stata help.


```stata
mata
: qrd(a=(12,-51,4\6,167,-68\-4,24,-41),q=.,r=.)

: a
         1     2     3
    +-------------------+
  1 |   12   -51     4  |
  2 |    6   167   -68  |
  3 |   -4    24   -41  |
    +-------------------+

: q
                  1              2              3
    +----------------------------------------------+
  1 |  -.8571428571    .3942857143    .3314285714  |
  2 |  -.4285714286   -.9028571429   -.0342857143  |
  3 |   .2857142857   -.1714285714    .9428571429  |
    +----------------------------------------------+

: r
          1      2      3
    +----------------------+
  1 |   -14    -21     14  |
  2 |     0   -175     70  |
  3 |     0      0    -35  |
    +----------------------+
```



## Tcl

Assuming the presence of the Tcl solutions to these tasks: [[Element-wise operations]], [[Matrix multiplication]], [[Matrix transposition]]
{{trans|Common Lisp}}

```tcl
package require Tcl 8.5
namespace path {::tcl::mathfunc ::tcl::mathop}
proc sign x {expr {$x == 0 ? 0 : $x < 0 ? -1 : 1}}
proc norm vec {
    set s 0
    foreach x $vec {set s [expr {$s + $x**2}]}
    return [sqrt $s]
}
proc unitvec n {
    set v [lrepeat $n 0.0]
    lset v 0 1.0
    return $v
}
proc I n {
    set m [lrepeat $n [lrepeat $n 0.0]]
    for {set i 0} {$i < $n} {incr i} {lset m $i $i 1.0}
    return $m
}

proc arrayEmbed {A B row col} {
    # $A will be copied automatically; Tcl values are copy-on-write
    lassign [size $B] mb nb
    for {set i 0} {$i < $mb} {incr i} {
	for {set j 0} {$j < $nb} {incr j} {
	    lset A [expr {$row + $i}] [expr {$col + $j}] [lindex $B $i $j]
	}
    }
    return $A
}

# Unlike the Common Lisp version, here we use a specialist subcolumn
# extraction function: like that, there's a lot less intermediate memory allocation
# and the code is actually clearer.
proc subcolumn {A size column} {
    for {set i $column} {$i < $size} {incr i} {lappend x [lindex $A $i $column]}
    return $x
}

proc householder A {
    lassign [size $A] m
    set U [m+ $A [.* [unitvec $m] [expr {[norm $A] * [sign [lindex $A 0 0]]}]]]
    set V [./ $U [lindex $U 0 0]]
    set beta [expr {2.0 / [lindex [matrix_multiply [transpose $V] $V] 0 0]}]
    return [m- [I $m] [.* [matrix_multiply $V [transpose $V]] $beta]]
}

proc qrDecompose A {
    lassign [size $A] m n
    set Q [I $m]
    for {set i 0} {$i < ($m==$n ? $n-1 : $n)} {incr i} {
	# Construct the Householder matrix
	set H [arrayEmbed [I $m] [householder [subcolumn $A $n $i]] $i $i]
	# Apply to build the decomposition
	set Q [matrix_multiply $Q $H]
	set A [matrix_multiply $H $A]
    }
    return [list $Q $A]
}
```

Demonstrating:

```tcl
set demo [qrDecompose {{12 -51 4} {6 167 -68} {-4 24 -41}}]
puts "==Q=="
print_matrix [lindex $demo 0] "%f"
puts "==R=="
print_matrix [lindex $demo 1] "%.1f"
```

Output:

```txt

==Q==
-0.857143  0.394286  0.331429
-0.428571 -0.902857 -0.034286
 0.285714 -0.171429  0.942857
==R==
-14.0  -21.0  14.0
  0.0 -175.0  70.0
  0.0    0.0 -35.0

```



## VBA

{{trans|Phix}}
```vb
Option Base 1
Private Function vtranspose(v As Variant) As Variant
'-- transpose a vector of length m into an mx1 matrix,
'--                       eg {1,2,3} -> {1;2;3}
    vtranspose = WorksheetFunction.Transpose(v)
End Function

Private Function mat_col(a As Variant, col As Integer) As Variant
    Dim res() As Double
    ReDim res(UBound(a))
    For i = col To UBound(a)
        res(i) = a(i, col)
    Next i
    mat_col = res
End Function

Private Function mat_norm(a As Variant) As Double
    mat_norm = Sqr(WorksheetFunction.SumProduct(a, a))
End Function

Private Function mat_ident(n As Integer) As Variant
    mat_ident = WorksheetFunction.Munit(n)
End Function

Private Function sq_div(a As Variant, p As Double) As Variant
    Dim res() As Variant
    ReDim res(UBound(a))
    For i = 1 To UBound(a)
        res(i) = a(i) / p
    Next i
    sq_div = res
End Function

Private Function sq_mul(p As Double, a As Variant) As Variant
    Dim res() As Variant
    ReDim res(UBound(a), UBound(a, 2))
    For i = 1 To UBound(a)
        For j = 1 To UBound(a, 2)
            res(i, j) = p * a(i, j)
        Next j
    Next i
    sq_mul = res
End Function

Private Function sq_sub(x As Variant, y As Variant) As Variant
    Dim res() As Variant
    ReDim res(UBound(x), UBound(x, 2))
    For i = 1 To UBound(x)
        For j = 1 To UBound(x, 2)
            res(i, j) = x(i, j) - y(i, j)
        Next j
    Next i
    sq_sub = res
End Function

Private Function matrix_mul(x As Variant, y As Variant) As Variant
    matrix_mul = WorksheetFunction.MMult(x, y)
End Function

Private Function QRHouseholder(ByVal a As Variant) As Variant
    Dim columns As Integer: columns = UBound(a, 2)
    Dim rows As Integer: rows = UBound(a)
    Dim m As Integer: m = WorksheetFunction.Max(columns, rows)
    Dim n As Integer: n = WorksheetFunction.Min(rows, columns)
    I_ = mat_ident(m)
    Q_ = I_
    Dim q As Variant
    Dim u As Variant, v As Variant, j As Integer
    For j = 1 To WorksheetFunction.Min(m - 1, n)
        u = mat_col(a, j)
        u(j) = u(j) - mat_norm(u)
        v = sq_div(u, mat_norm(u))
        q = sq_sub(I_, sq_mul(2, matrix_mul(vtranspose(v), v)))
        a = matrix_mul(q, a)
        Q_ = matrix_mul(Q_, q)
    Next j

    '-- Get the upper triangular matrix R.
    Dim R() As Variant
    ReDim R(m, n)
    For i = 1 To m 'in Phix this is n
        For j = 1 To n 'in Phix this is i to n. starting at 1 to fill zeroes
            R(i, j) = a(i, j)
        Next j
    Next i
    Dim res(2) As Variant
    res(1) = Q_
    res(2) = R
    QRHouseholder = res
End Function

Private Sub pp(m As Variant)
    For i = 1 To UBound(m)
        For j = 1 To UBound(m, 2)
            Debug.Print Format(m(i, j), "0.#####"),
        Next j
        Debug.Print
    Next i
End Sub
Public Sub main()
    a = [{12, -51,   4; 6, 167, -68; -4,  24, -41;-1,1,0;2,0,3}]
    result = QRHouseholder(a)
    q = result(1)
    r_ = result(2)
    Debug.Print "A"
    pp a
    Debug.Print "Q"
    pp q
    Debug.Print "R"
    pp r_
    Debug.Print "Q * R"
    pp matrix_mul(q, r_)
End Sub
```
{{out}}

```txt
A
12,           -51,          4,
6,            167,          -68,
-4,           24,           -41,
-1,           1,            0,
2,            0,            3,
Q
0,84641       -0,39129      -0,34312      0,06641       -0,09126
0,42321       0,90409       0,02927       0,01752       -0,04856
-0,28214      0,17042       -0,93286      -0,02237      0,14365
-0,07053      0,01404       0,0011        0,99738       0,00728
0,14107       -0,01666      0,10577       0,00291       0,98419
R
14,17745      20,66663      -13,40157
0,            175,04254     -70,08031
0,            0,            35,20154
0,            0,            0,
0,            0,            0,
Q * R
12,           -51,          4,
6,            167,          -68,
-4,           24,           -41,
-1,           1,            0,
2,            0,            3,
```

Least squares

```vb
Public Sub least_squares()
    x = [{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}]
    y = [{1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}]
    Dim a() As Double
    ReDim a(UBound(x), 3)
    For i = 1 To UBound(x)
        For j = 1 To 3
            a(i, j) = x(i) ^ (j - 1)
        Next j
    Next i
    result = QRHouseholder(a)
    q = result(1)
    r_ = result(2)
    t = WorksheetFunction.Transpose(q)
    b = matrix_mul(t, vtranspose(y))
    Dim z(3) As Double
    For k = 3 To 1 Step -1
        Dim s As Double: s = 0
        If k < 3 Then
            For j = k + 1 To 3
                s = s + r_(k, j) * z(j)
            Next j
        End If
        z(k) = (b(k, 1) - s) / r_(k, k)
    Next k
    Debug.Print "Least-squares solution:",
    For i = 1 To 3
        Debug.Print Format(z(i), "0.#####"),
    Next i
End Sub
```
{{out}}

```txt
Least-squares solution:     1,            2,            3,
```


## zkl


```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
A:=GSL.Matrix(3,3).set(12.0, -51.0,   4.0,
			6.0, 167.0, -68.0,
			4.0,  24.0, -41.0);
Q,R:=A.QRDecomp();
println("Q:\n",Q.format());
println("R:\n",R.format());
println("Q*R:\n",(Q*R).format());
```

{{out}}

```txt

Q:
     -0.86,      0.47,     -0.22
     -0.43,     -0.88,     -0.20
     -0.29,     -0.08,      0.95
R:
    -14.00,    -34.71,     37.43
      0.00,   -172.80,     65.07
      0.00,      0.00,    -26.19
Q*R:
     12.00,    -51.00,      4.00
      6.00,    167.00,    -68.00
      4.00,     24.00,    -41.00

```

