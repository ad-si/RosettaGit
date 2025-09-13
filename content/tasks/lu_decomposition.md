+++
title = "LU decomposition"
description = ""
date = 2019-08-02T15:00:28Z
aliases = []
[extra]
id = 9359
[taxonomies]
categories = ["task", "Matrices"]
tags = []
+++

## Task

Every square matrix <math>A</math> can be decomposed into a product of a lower triangular matrix <math>L</math> and a upper triangular matrix <math>U</math>,
as described in [[wp:LU decomposition|LU decomposition]].

:<math>A = LU</math>

It is a modified form of Gaussian elimination.
While the [[Cholesky decomposition]] only works for symmetric,
positive definite matrices, the more general LU decomposition
works for any square matrix.

There are several algorithms for calculating L and U.
To derive ''Crout's algorithm'' for a 3x3 example,
we have to solve the following system:

:<math>
A =
\begin{pmatrix}
   a_{11} & a_{12} & a_{13}\\
   a_{21} & a_{22} & a_{23}\\
   a_{31} & a_{32} & a_{33}\\
\end{pmatrix}
=
\begin{pmatrix}
   l_{11} & 0 & 0 \\
   l_{21} & l_{22} & 0 \\
   l_{31} & l_{32} & l_{33}\\
\end{pmatrix}
\begin{pmatrix}
   u_{11} & u_{12} & u_{13} \\
   0 & u_{22} & u_{23} \\
   0 & 0 & u_{33}
\end{pmatrix}
= LU
</math>

We now would have to solve 9 equations with 12 unknowns. To make the system uniquely solvable, usually the diagonal elements of <math>L</math> are set to 1

:<math>l_{11}=1</math>
:<math>l_{22}=1</math>
:<math>l_{33}=1</math>

so we get a solvable system of 9 unknowns and 9 equations.

:<math>
A =
\begin{pmatrix}
   a_{11} & a_{12} & a_{13}\\
   a_{21} & a_{22} & a_{23}\\
   a_{31} & a_{32} & a_{33}\\
\end{pmatrix}
=
\begin{pmatrix}
   1      & 0      & 0 \\
   l_{21} & 1      & 0 \\
   l_{31} & l_{32} & 1\\
\end{pmatrix}
\begin{pmatrix}
   u_{11} & u_{12} & u_{13} \\
   0 & u_{22} & u_{23} \\
   0 & 0 & u_{33}
\end{pmatrix}
=
\begin{pmatrix}
   u_{11}        & u_{12}                    & u_{13}              \\
   u_{11}l_{21}  & u_{12}l_{21}+u_{22}       & u_{13}l_{21}+u_{23} \\
   u_{11}l_{31}  & u_{12}l_{31}+u_{22}l_{32} & u_{13}l_{31} + u_{23}l_{32}+u_{33}
\end{pmatrix}
= LU
</math>

Solving for the other <math>l</math> and <math>u</math>, we get the following equations:

:<math>u_{11}=a_{11}</math>
:<math>u_{12}=a_{12}</math>
:<math>u_{13}=a_{13}</math>

:<math>u_{22}=a_{22} - u_{12}l_{21}</math>
:<math>u_{23}=a_{23} - u_{13}l_{21}</math>

:<math>u_{33}=a_{33} - (u_{13}l_{31} + u_{23}l_{32})</math>

and for <math>l</math>:

:<math>l_{21}=\frac{1}{u_{11}} a_{21}</math>
:<math>l_{31}=\frac{1}{u_{11}} a_{31}</math>

:<math>l_{32}=\frac{1}{u_{22}} (a_{32} - u_{12}l_{31})</math>

We see that there is a calculation pattern, which can be expressed as the following formulas, first for <math>U</math>

:<math>u_{ij} = a_{ij} - \sum_{k=1}^{i-1} u_{kj}l_{ik}</math>

and then for <math>L</math>

:<math>l_{ij} = \frac{1}{u_{jj}} (a_{ij} - \sum_{k=1}^{j-1} u_{kj}l_{ik})</math>

We see in the second formula that to get the <math>l_{ij}</math> below the diagonal, we have to divide by the diagonal element (pivot) <math>u_{jj}</math>, so we get problems when <math>u_{jj}</math> is either 0 or very small, which leads to numerical instability.

The solution to this problem is ''pivoting'' <math>A</math>, which means rearranging the rows of <math>A</math>, prior to the <math>LU</math> decomposition, in a way that the largest element of each column gets onto the diagonal of <math>A</math>. Rearranging the rows means to multiply <math>A</math> by a permutation matrix <math>P</math>:

:<math>PA \Rightarrow A'</math>

Example:

:<math>
\begin{pmatrix}
   0 & 1 \\
   1 & 0
\end{pmatrix}
\begin{pmatrix}
   1 & 4 \\
   2 & 3
\end{pmatrix}
\Rightarrow
\begin{pmatrix}
   2 & 3 \\
   1 & 4
\end{pmatrix}
</math>

The decomposition algorithm is then applied on the rearranged matrix so that

:<math>PA = LU</math>


'''Task description'''

The task is to implement a routine which will take a square nxn matrix <math>A</math> and return a lower triangular matrix <math>L</math>, a upper triangular matrix <math>U</math> and a permutation matrix <math>P</math>,
so that the above equation is fullfilled.
You should then test it on the following two examples and include your output.

Example 1:

```txt

A

1   3   5
2   4   7
1   1   0

L

1.00000   0.00000   0.00000
0.50000   1.00000   0.00000
0.50000  -1.00000   1.00000

U

2.00000   4.00000   7.00000
0.00000   1.00000   1.50000
0.00000   0.00000  -2.00000

P

0   1   0
1   0   0
0   0   1

```


Example 2:

```txt

A

11    9   24    2
 1    5    2    6
 3   17   18    1
 2    5    7    1

L

1.00000   0.00000   0.00000   0.00000
0.27273   1.00000   0.00000   0.00000
0.09091   0.28750   1.00000   0.00000
0.18182   0.23125   0.00360   1.00000

U

11.00000    9.00000   24.00000    2.00000
 0.00000   14.54545   11.45455    0.45455
 0.00000    0.00000   -3.47500    5.68750
 0.00000    0.00000    0.00000    0.51079

P

1   0   0   0
0   0   1   0
0   1   0   0
0   0   0   1

```



## Ada

decomposition.ads:

```Ada
with Ada.Numerics.Generic_Real_Arrays;
generic
   with package Matrix is new Ada.Numerics.Generic_Real_Arrays (<>);
package Decomposition is

   -- decompose a square matrix A by PA = LU
   procedure Decompose (A : Matrix.Real_Matrix; P, L, U : out Matrix.Real_Matrix);

end Decomposition;
```


decomposition.adb:

```Ada
package body Decomposition is

   procedure Swap_Rows (M : in out Matrix.Real_Matrix; From, To : Natural) is
      Temporary : Matrix.Real;
   begin
      if From = To then
         return;
      end if;
      for I in M'Range (2) loop
         Temporary := M (M'First (1) + From, I);
         M (M'First (1) + From, I) := M (M'First (1) + To, I);
         M (M'First (1) + To, I) := Temporary;
      end loop;
   end Swap_Rows;

   function Pivoting_Matrix
     (M : Matrix.Real_Matrix)
      return Matrix.Real_Matrix
   is
      use type Matrix.Real;
      Order     : constant Positive := M'Length (1);
      Result    : Matrix.Real_Matrix := Matrix.Unit_Matrix (Order);
      Max       : Matrix.Real;
      Row       : Natural;
   begin
      for J in 0 .. Order - 1 loop
         Max := M (M'First (1) + J, M'First (2) + J);
         Row := J;
         for I in J .. Order - 1 loop
            if M (M'First (1) + I, M'First (2) + J) > Max then
               Max := M (M'First (1) + I, M'First (2) + J);
               Row := I;
            end if;
         end loop;
         if J /= Row then
            -- swap rows J and Row
            Swap_Rows (Result, J, Row);
         end if;
      end loop;
      return Result;
   end Pivoting_Matrix;

   procedure Decompose (A : Matrix.Real_Matrix; P, L, U : out Matrix.Real_Matrix) is
      use type Matrix.Real_Matrix, Matrix.Real;
      Order : constant Positive := A'Length (1);
      A2 : Matrix.Real_Matrix (A'Range (1), A'Range (2));
      S : Matrix.Real;
   begin
      L := (others => (others => 0.0));
      U := (others => (others => 0.0));
      P := Pivoting_Matrix (A);
      A2 := P * A;
      for J in 0 .. Order - 1 loop
         L (L'First (1) + J, L'First (2) + J) := 1.0;
         for I in 0 .. J loop
            S := 0.0;
            for K in 0 .. I - 1 loop
               S := S + U (U'First (1) + K, U'First (2) + J) *
                 L (L'First (1) + I, L'First (2) + K);
            end loop;
            U (U'First (1) + I, U'First (2) + J) :=
              A2 (A2'First (1) + I, A2'First (2) + J) - S;
         end loop;
         for I in J + 1 .. Order - 1 loop
            S := 0.0;
            for K in 0 .. J loop
               S := S + U (U'First (1) + K, U'First (2) + J) *
                 L (L'First (1) + I, L'First (2) + K);
            end loop;
            L (L'First (1) + I, L'First (2) + J) :=
              (A2 (A2'First (1) + I, A2'First (2) + J) - S) /
              U (U'First (1) + J, U'First (2) + J);
         end loop;
      end loop;
   end Decompose;

end Decomposition;
```


Example usage:

```Ada
with Ada.Numerics.Real_Arrays;
with Ada.Text_IO;
with Decomposition;
procedure Decompose_Example is
   package Real_Decomposition is new Decomposition
     (Matrix => Ada.Numerics.Real_Arrays);

   package Real_IO is new Ada.Text_IO.Float_IO (Float);

   procedure Print (M : Ada.Numerics.Real_Arrays.Real_Matrix) is
   begin
      for Row in M'Range (1) loop
         for Col in M'Range (2) loop
            Real_IO.Put (M (Row, Col), 3, 2, 0);
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print;

   Example_1 : constant Ada.Numerics.Real_Arrays.Real_Matrix :=
     ((1.0, 3.0, 5.0),
      (2.0, 4.0, 7.0),
      (1.0, 1.0, 0.0));
   P_1, L_1, U_1 : Ada.Numerics.Real_Arrays.Real_Matrix (Example_1'Range (1),
                                                         Example_1'Range (2));
   Example_2 : constant Ada.Numerics.Real_Arrays.Real_Matrix :=
     ((11.0, 9.0, 24.0, 2.0),
      (1.0, 5.0, 2.0, 6.0),
      (3.0, 17.0, 18.0, 1.0),
      (2.0, 5.0, 7.0, 1.0));
   P_2, L_2, U_2 : Ada.Numerics.Real_Arrays.Real_Matrix (Example_2'Range (1),
                                                         Example_2'Range (2));
begin
   Real_Decomposition.Decompose (A => Example_1,
                                 P => P_1,
                                 L => L_1,
                                 U => U_1);
   Real_Decomposition.Decompose (A => Example_2,
                                 P => P_2,
                                 L => L_2,
                                 U => U_2);
   Ada.Text_IO.Put_Line ("Example 1:");
   Ada.Text_IO.Put_Line ("A:"); Print (Example_1);
   Ada.Text_IO.Put_Line ("L:"); Print (L_1);
   Ada.Text_IO.Put_Line ("U:"); Print (U_1);
   Ada.Text_IO.Put_Line ("P:"); Print (P_1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Example 2:");
   Ada.Text_IO.Put_Line ("A:"); Print (Example_2);
   Ada.Text_IO.Put_Line ("L:"); Print (L_2);
   Ada.Text_IO.Put_Line ("U:"); Print (U_2);
   Ada.Text_IO.Put_Line ("P:"); Print (P_2);
end Decompose_Example;
```


```txt
Example 1:
A:
  1.00  3.00  5.00
  2.00  4.00  7.00
  1.00  1.00  0.00
L:
  1.00  0.00  0.00
  0.50  1.00  0.00
  0.50 -1.00  1.00
U:
  2.00  4.00  7.00
  0.00  1.00  1.50
  0.00  0.00 -2.00
P:
  0.00  1.00  0.00
  1.00  0.00  0.00
  0.00  0.00  1.00

Example 2:
A:
 11.00  9.00 24.00  2.00
  1.00  5.00  2.00  6.00
  3.00 17.00 18.00  1.00
  2.00  5.00  7.00  1.00
L:
  1.00  0.00  0.00  0.00
  0.27  1.00  0.00  0.00
  0.09  0.29  1.00  0.00
  0.18  0.23  0.00  1.00
U:
 11.00  9.00 24.00  2.00
  0.00 14.55 11.45  0.45
  0.00  0.00 -3.47  5.69
  0.00  0.00  0.00  0.51
P:
  1.00  0.00  0.00  0.00
  0.00  0.00  1.00  0.00
  0.00  1.00  0.00  0.00
  0.00  0.00  0.00  1.00
```



## BBC BASIC


```bbcbasic
      DIM A1(2,2)
      A1() = 1, 3, 5, 2, 4, 7, 1, 1, 0
      PROCLUdecomposition(A1(), L1(), U1(), P1())
      PRINT "L1:" ' FNshowmatrix(L1())
      PRINT "U1:" ' FNshowmatrix(U1())
      PRINT "P1:" ' FNshowmatrix(P1())

      DIM A2(3,3)
      A2() = 11, 9, 24, 2, 1, 5, 2, 6, 3, 17, 18, 1, 2, 5, 7, 1
      PROCLUdecomposition(A2(), L2(), U2(), P2())
      PRINT "L2:" ' FNshowmatrix(L2())
      PRINT "U2:" ' FNshowmatrix(U2())
      PRINT "P2:" ' FNshowmatrix(P2())
      END

      DEF PROCLUdecomposition(a(), RETURN l(), RETURN u(), RETURN p())
      LOCAL i%, j%, k%, n%, s, b() : n% = DIM(a(),2)
      DIM l(n%,n%), u(n%,n%), b(n%,n%)
      PROCpivot(a(), p())
      b() = p() . a()
      FOR j% = 0 TO n%
        l(j%,j%) = 1
        FOR i% = 0 TO j%
          s = 0
          FOR k% = 0 TO i% : s += u(k%,j%) * l(i%,k%) : NEXT
          u(i%,j%) = b(i%,j%) - s
        NEXT
        FOR i% = j% TO n%
          s = 0
          FOR k% = 0 TO j% : s += u(k%,j%) * l(i%,k%) : NEXT
          IF i%<>j% l(i%,j%) = (b(i%,j%) - s) / u(j%,j%)
        NEXT
      NEXT j%
      ENDPROC

      DEF PROCpivot(a(), RETURN p())
      LOCAL i%, j%, m%, n%, r% : n% = DIM(a(),2)
      DIM p(n%,n%) : FOR i% = 0 TO n% : p(i%,i%) = 1 : NEXT
      FOR i% = 0 TO n%
        m% = a(i%,i%)
        r% = i%
        FOR j% = i% TO n%
          IF a(j%,i%) > m% m% = a(j%,i%) : r% = j%
        NEXT
        IF i%<>r% THEN
          FOR j% = 0 TO n% : SWAP p(i%,j%),p(r%,j%) : NEXT
        ENDIF
      NEXT i%
      ENDPROC

      DEF FNshowmatrix(a())
      LOCAL @%, i%, j%, a$
      @% = &102050A
      FOR i% = 0 TO DIM(a(),1)
        FOR j% = 0 TO DIM(a(),2)
          a$ += STR$(a(i%,j%)) + ", "
        NEXT
        a$ = LEFT$(LEFT$(a$)) + CHR$(13) + CHR$(10)
      NEXT i%
      = a$
```

```txt

L1:
1.00000, 0.00000, 0.00000
0.50000, 1.00000, 0.00000
0.50000, -1.00000, 1.00000

U1:
2.00000, 4.00000, 7.00000
0.00000, 1.00000, 1.50000
0.00000, 0.00000, -2.00000

P1:
0.00000, 1.00000, 0.00000
1.00000, 0.00000, 0.00000
0.00000, 0.00000, 1.00000

L2:
1.00000, 0.00000, 0.00000, 0.00000
0.27273, 1.00000, 0.00000, 0.00000
0.09091, 0.28750, 1.00000, 0.00000
0.18182, 0.23125, 0.00360, 1.00000

U2:
11.00000, 9.00000, 24.00000, 2.00000
0.00000, 14.54545, 11.45455, 0.45455
0.00000, 0.00000, -3.47500, 5.68750
0.00000, 0.00000, 0.00000, 0.51079

P2:
1.00000, 0.00000, 0.00000, 0.00000
0.00000, 0.00000, 1.00000, 0.00000
0.00000, 1.00000, 0.00000, 0.00000
0.00000, 0.00000, 0.00000, 1.00000

```



## C

Compiled with <code>gcc -std=gnu99 -Wall -lm -pedantic</code>.  Demonstrating how to do LU decomposition, and how (not) to use macros.
```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define foreach(a, b, c) for (int a = b; a < c; a++)
#define for_i foreach(i, 0, n)
#define for_j foreach(j, 0, n)
#define for_k foreach(k, 0, n)
#define for_ij for_i for_j
#define for_ijk for_ij for_k
#define _dim int n
#define _swap(x, y) { typeof(x) tmp = x; x = y; y = tmp; }
#define _sum_k(a, b, c, s) { s = 0; foreach(k, a, b) s+= c; }

typedef double **mat;

#define _zero(a) mat_zero(a, n)
void mat_zero(mat x, int n) { for_ij x[i][j] = 0; }

#define _new(a) a = mat_new(n)
mat mat_new(_dim)
{
	mat x = malloc(sizeof(double*) * n);
	x[0]  = malloc(sizeof(double) * n * n);

	for_i x[i] = x[0] + n * i;
	_zero(x);

	return x;
}

#define _copy(a) mat_copy(a, n)
mat mat_copy(void *s, _dim)
{
	mat x = mat_new(n);
	for_ij x[i][j] = ((double (*)[n])s)[i][j];
	return x;
}

#define _del(x) mat_del(x)
void mat_del(mat x) { free(x[0]); free(x); }

#define _QUOT(x) #x
#define QUOTE(x) _QUOT(x)
#define _show(a) printf(QUOTE(a)" =");mat_show(a, 0, n)
void mat_show(mat x, char *fmt, _dim)
{
	if (!fmt) fmt = "%8.4g";
	for_i {
		printf(i ? "      " : " [ ");
		for_j {
			printf(fmt, x[i][j]);
			printf(j < n - 1 ? "  " : i == n - 1 ? " ]\n" : "\n");
		}
	}
}

#define _mul(a, b) mat_mul(a, b, n)
mat mat_mul(mat a, mat b, _dim)
{
	mat c = _new(c);
	for_ijk c[i][j] += a[i][k] * b[k][j];
	return c;
}

#define _pivot(a, b) mat_pivot(a, b, n)
void mat_pivot(mat a, mat p, _dim)
{
	for_ij { p[i][j] = (i == j); }
	for_i  {
		int max_j = i;
		foreach(j, i, n)
			if (fabs(a[j][i]) > fabs(a[max_j][i])) max_j = j;

		if (max_j != i)
			for_k { _swap(p[i][k], p[max_j][k]); }
	}
}

#define _LU(a, l, u, p) mat_LU(a, l, u, p, n)
void mat_LU(mat A, mat L, mat U, mat P, _dim)
{
	_zero(L); _zero(U);
	_pivot(A, P);

	mat Aprime = _mul(P, A);

	for_i  { L[i][i] = 1; }
	for_ij {
		double s;
		if (j <= i) {
			_sum_k(0, j, L[j][k] * U[k][i], s)
			U[j][i] = Aprime[j][i] - s;
		}
		if (j >= i) {
			_sum_k(0, i, L[j][k] * U[k][i], s);
			L[j][i] = (Aprime[j][i] - s) / U[i][i];
		}
	}

	_del(Aprime);
}

double A3[][3] = {{ 1, 3, 5 }, { 2, 4, 7 }, { 1, 1, 0 }};
double A4[][4] = {{11, 9, 24, 2}, {1, 5, 2, 6}, {3, 17, 18, 1}, {2, 5, 7, 1}};

int main()
{
	int n = 3;
	mat A, L, P, U;

	_new(L); _new(P); _new(U);
	A = _copy(A3);
	_LU(A, L, U, P);
	_show(A); _show(L); _show(U); _show(P);
	_del(A);  _del(L);  _del(U);  _del(P);

	printf("\n");

	n = 4;

	_new(L); _new(P); _new(U);
	A = _copy(A4);
	_LU(A, L, U, P);
	_show(A); _show(L); _show(U); _show(P);
	_del(A);  _del(L);  _del(U);  _del(P);

	return 0;
}
```



## Common Lisp


Uses the routine (mmul A B) from [[Matrix multiplication]].


```lisp
;; Creates a nxn identity matrix.
(defun eye (n)
  (let ((I (make-array `(,n ,n) :initial-element 0)))
    (loop for j from 0 to (- n 1) do
          (setf (aref I j j) 1))
    I))

;; Swap two rows l and k of a mxn matrix A, which is a 2D array.
(defun swap-rows (A l k)
  (let* ((n (cadr (array-dimensions A)))
         (row (make-array n :initial-element 0)))
    (loop for j from 0 to (- n 1) do
          (setf (aref row j) (aref A l j))
          (setf (aref A l j) (aref A k j))
          (setf (aref A k j) (aref row j)))))

;; Creates the pivoting matrix for A.
(defun pivotize (A)
  (let* ((n (car (array-dimensions A)))
         (P (eye n)))
    (loop for j from 0 to (- n 1) do
          (let ((max (aref A j j))
                (row j))
            (loop for i from j to (- n 1) do
                  (if (> (aref A i j) max)
                      (setq max (aref A i j)
                            row i)))
            (if (not (= j row))
                (swap-rows P j row))))

  ;; Return P.
  P))

;; Decomposes a square matrix A by PA=LU and returns L, U and P.
(defun lu (A)
  (let* ((n (car (array-dimensions A)))
         (L (make-array `(,n ,n) :initial-element 0))
         (U (make-array `(,n ,n) :initial-element 0))
         (P (pivotize A))
         (A (mmul P A)))

    (loop for j from 0 to (- n 1) do
          (setf (aref L j j) 1)
          (loop for i from 0 to j do
                (setf (aref U i j)
                      (- (aref A i j)
                         (loop for k from 0 to (- i 1)
                               sum (* (aref U k j)
                                      (aref L i k))))))
          (loop for i from j to (- n 1) do
                (setf (aref L i j)
                      (/ (- (aref A i j)
                            (loop for k from 0 to (- j 1)
                                  sum (* (aref U k j)
                                         (aref L i k))))
                         (aref U j j)))))

  ;; Return L, U and P.
  (values L U P)))
```


Example 1:


```lisp
(setf g (make-array '(3 3) :initial-contents '((1 3 5) (2 4 7)(1 1 0))))
#2A((1 3 5) (2 4 7) (1 1 0))

(lu g)
#2A((1 0 0) (1/2 1 0) (1/2 -1 1))
#2A((2 4 7) (0 1 3/2) (0 0 -2))
#2A((0 1 0) (1 0 0) (0 0 1))
```


Example 2:


```lisp
(setf h (make-array '(4 4) :initial-contents '((11 9 24 2)(1 5 2 6)(3 17 18 1)(2 5 7 1))))
#2A((11 9 24 2) (1 5 2 6) (3 17 18 1) (2 5 7 1))

(lup h)
#2A((1 0 0 0) (3/11 1 0 0) (1/11 23/80 1 0) (2/11 37/160 1/278 1))
#2A((11 9 24 2) (0 160/11 126/11 5/11) (0 0 -139/40 91/16) (0 0 0 71/139))
#2A((1 0 0 0) (0 0 1 0) (0 1 0 0) (0 0 0 1))
```



## D

```d
import std.stdio, std.algorithm, std.typecons, std.numeric,
       std.array, std.conv, std.string, std.range;

bool isRectangular(T)(in T[][] m) pure nothrow @nogc {
    return m.all!(r => r.length == m[0].length);
}

bool isSquare(T)(in T[][] m) pure nothrow @nogc {
    return m.isRectangular && m[0].length == m.length;
}

T[][] matrixMul(T)(in T[][] A, in T[][] B) pure nothrow
in {
    assert(A.isRectangular && B.isRectangular &&
           !A.empty && !B.empty && A[0].length == B.length);
} body {
    auto result = new T[][](A.length, B[0].length);
    auto aux = new T[B.length];

    foreach (immutable j; 0 .. B[0].length) {
        foreach (immutable k, const row; B)
            aux[k] = row[j];
        foreach (immutable i, const ai; A)
            result[i][j] = dotProduct(ai, aux);
    }

    return result;
}

/// Creates the pivoting matrix for m.
T[][] pivotize(T)(immutable T[][] m) pure nothrow
in {
    assert(m.isSquare);
} body {
    immutable n = m.length;
    auto id = iota(n)
              .map!((in j) => n.iota.map!(i => T(i == j)).array)
              .array;

    foreach (immutable i; 0 .. n) {
        // immutable row = iota(i, n).reduce!(max!(j => m[j][i]));
        T maxm = m[i][i];
        size_t row = i;
        foreach (immutable j; i .. n)
            if (m[j][i] > maxm) {
                maxm = m[j][i];
                row = j;
            }

        if (i != row)
            swap(id[i], id[row]);
    }

    return id;
}

/// Decomposes a square matrix A by PA=LU and returns L, U and P.
Tuple!(T[][],"L", T[][],"U", const T[][],"P")
lu(T)(immutable T[][] A) pure nothrow
in {
    assert(A.isSquare);
} body {
    immutable n = A.length;
    auto L = new T[][](n, n);
    auto U = new T[][](n, n);
    foreach (immutable i; 0 .. n) {
        L[i][i .. $] = 0;
        U[i][0 .. i] = 0;
    }

    immutable P = A.pivotize!T;
    immutable A2 = matrixMul!T(P, A);

    foreach (immutable j; 0 .. n) {
        L[j][j] = 1;
        foreach (immutable i; 0 .. j+1) {
            T s1 = 0;
            foreach (immutable k; 0 .. i)
                s1 += U[k][j] * L[i][k];
            U[i][j] = A2[i][j] - s1;
        }
        foreach (immutable i; j .. n) {
            T s2 = 0;
            foreach (immutable k; 0 .. j)
                s2 += U[k][j] * L[i][k];
            L[i][j] = (A2[i][j] - s2) / U[j][j];
        }
    }

    return typeof(return)(L, U, P);
}

void main() {
    immutable a = [[1.0, 3, 5],
                   [2.0, 4, 7],
                   [1.0, 1, 0]];
    immutable b = [[11.0, 9, 24, 2],
                   [1.0,  5,  2, 6],
                   [3.0, 17, 18, 1],
                   [2.0,  5,  7, 1]];

    auto f = "[%([%(%.1f, %)],\n %)]]\n\n".replicate(3);
    foreach (immutable m; [a, b])
        writefln(f, lu(m).tupleof);
}
```

```txt
[[1.0, 0.0, 0.0],
 [0.5, 1.0, 0.0],
 [0.5, -1.0, 1.0]]

[[2.0, 4.0, 7.0],
 [0.0, 1.0, 1.5],
 [0.0, 0.0, -2.0]]

[[0.0, 1.0, 0.0],
 [1.0, 0.0, 0.0],
 [0.0, 0.0, 1.0]]


[[1.0, 0.0, 0.0, 0.0],
 [0.3, 1.0, 0.0, 0.0],
 [0.0, 0.3, 1.0, 0.0],
 [0.2, 0.2, 0.0, 1.0]]

[[11.0, 9.0, 24.0, 2.0],
 [0.0, 14.5, 11.5, 0.5],
 [0.0, 0.0, -3.5, 5.7],
 [0.0, 0.0, 0.0, 0.5]]

[[1.0, 0.0, 0.0, 0.0],
 [0.0, 0.0, 1.0, 0.0],
 [0.0, 1.0, 0.0, 0.0],
 [0.0, 0.0, 0.0, 1.0]]
```



## EchoLisp


```scheme

(lib 'matrix) ;; the matrix library provides LU-decomposition
(decimals 5)

(define A (list->array' (1 3 5 2 4 7 1 1 0 ) 3 3))
(define PLU (matrix-lu-decompose A)) ;; -> list of three matrices, P, Lower, Upper

(array-print (first PLU))
0  1  0
1  0  0
0  0  1
(array-print (second PLU))
1    0   0
0.5  1   0
0.5  -1  1
(array-print (caddr PLU))
2  4  7
0  1  1.5
0  0  -2

(define A (list->array '(11 9 24 2 1 5 2 6 3 17 18 1 2 5 7 1 ) 4 4))
(define PLU (matrix-lu-decompose A)) ;; -> list of three matrices, P, Lower, Upper
(array-print (first PLU))
1  0  0  0
0  0  1  0
0  1  0  0
0  0  0  1

(array-print (second PLU))
1        0        0       0
0.27273  1        0       0
0.09091  0.2875   1       0
0.18182  0.23125  0.0036  1

(array-print (caddr PLU))
11  9         24        2
0   14.54545  11.45455  0.45455
0   0         -3.475    5.6875
0   0         0         0.51079

```




## Fortran


```Fortran

program lu1
  implicit none
  call check( reshape([real(8)::1,2,1,3,4,1,5,7,0                  ],[3,3]) )
  call check( reshape([real(8)::11,1,3,2,9,5,17,5,24,2,18,7,2,6,1,1],[4,4]) )

contains

  subroutine check(a)
    real(8), intent(in)   :: a(:,:)
    integer               :: i,j,n
    real(8), allocatable  :: aa(:,:),l(:,:),u(:,:)
    integer, allocatable  :: p(:,:)
    integer, allocatable  :: ipiv(:)
    n = size(a,1)
    allocate(aa(n,n),l(n,n),u(n,n),p(n,n),ipiv(n))
    forall (j=1:n,i=1:n)
      aa(i,j) = a(i,j)
      u (i,j) = 0d0
      p (i,j) = merge(1  ,0  ,i.eq.j)
      l (i,j) = merge(1d0,0d0,i.eq.j)
    end forall
    call lu(aa, ipiv)
    do i = 1,n
       l(i, :i-1) = aa(ipiv(i), :i-1)
       u(i,i:   ) = aa(ipiv(i),i:   )
    end do
    p(ipiv,:) = p
    call mat_print('a',a)
    call mat_print('p',p)
    call mat_print('l',l)
    call mat_print('u',u)
    print *, "residual"
    print *, "|| P.A - L.U || =  ", maxval(abs(matmul(p,a)-matmul(l,u)))
  end subroutine

  subroutine lu(a,p)
!   in situ decomposition, corresponds to LAPACK's dgebtrf
    real(8), intent(inout) :: a(:,:)
    integer, intent(out  ) :: p(:)
    integer                :: n, i,j,k,kmax
    n = size(a,1)
    p = [ ( i, i=1,n ) ]
    do k = 1,n-1
        kmax = maxloc(abs(a(p(k:),k)),1) + k-1
        if (kmax /= k ) p([k, kmax]) = p([kmax, k])
        a(p(k+1:),k) = a(p(k+1:),k) / a(p(k),k)
        forall (j=k+1:n) a(p(k+1:),j) = a(p(k+1:),j) - a(p(k+1:),k) * a(p(k),j)
    end do
  end subroutine

  subroutine mat_print(amsg,a)
    character(*), intent(in) :: amsg
    class    (*), intent(in) :: a(:,:)
    integer                  :: i
    print*,' '
    print*,amsg
    do i=1,size(a,1)
      select type (a)
        type is (real(8)) ; print'(100f8.2)',a(i,:)
        type is (integer) ; print'(100i8  )',a(i,:)
      end select
    end do
    print*,' '
  end subroutine

end program

```

```txt

 a
    1.00     3.00     5.00
    2.00     4.00     7.00
    1.00     1.00     0.00
 p
    0.00     1.00     0.00
    1.00     0.00     0.00
    0.00     0.00     1.00
 l
    1.00     0.00     0.00
    0.50     1.00     0.00
    0.50    -1.00     1.00
 u
    2.00     4.00     7.00
    0.00     1.00     1.50
    0.00     0.00    -2.00
 residual
 || P.A - L.U || =     0.0000000000000000
 a
   11.00     9.00    24.00     2.00
    1.00     5.00     2.00     6.00
    3.00    17.00    18.00     1.00
    2.00     5.00     7.00     1.00
 p
    1.00     0.00     0.00     0.00
    0.00     0.00     1.00     0.00
    0.00     1.00     0.00     0.00
    0.00     0.00     0.00     1.00
 l
    1.00     0.00     0.00     0.00
    0.27     1.00     0.00     0.00
    0.09     0.29     1.00     0.00
    0.18     0.23     0.00     1.00
 u
   11.00     9.00    24.00     2.00
    0.00    14.55    11.45     0.45
    0.00     0.00    -3.47     5.69
    0.00     0.00     0.00     0.51
 residual
 || P.A - L.U || =     0.0000000000000000

```




## Go


### 2D representation

```go
package main

import "fmt"

type matrix [][]float64

func zero(n int) matrix {
    r := make([][]float64, n)
    a := make([]float64, n*n)
    for i := range r {
        r[i] = a[n*i : n*(i+1)]
    }
    return r
}

func eye(n int) matrix {
    r := zero(n)
    for i := range r {
        r[i][i] = 1
    }
    return r
}

func (m matrix) print(label string) {
    if label > "" {
        fmt.Printf("%s:\n", label)
    }
    for _, r := range m {
        for _, e := range r {
            fmt.Printf(" %9.5f", e)
        }
        fmt.Println()
    }
}

func (a matrix) pivotize() matrix {
    p := eye(len(a))
    for j, r := range a {
        max := r[j]
        row := j
        for i := j; i < len(a); i++ {
            if a[i][j] > max {
                max = a[i][j]
                row = i
            }
        }
        if j != row {
            // swap rows
            p[j], p[row] = p[row], p[j]
        }
    }
    return p
}

func (m1 matrix) mul(m2 matrix) matrix {
    r := zero(len(m1))
    for i, r1 := range m1 {
        for j := range m2 {
            for k := range m1 {
                r[i][j] += r1[k] * m2[k][j]
            }
        }
    }
    return r
}

func (a matrix) lu() (l, u, p matrix) {
    l = zero(len(a))
    u = zero(len(a))
    p = a.pivotize()
    a = p.mul(a)
    for j := range a {
        l[j][j] = 1
        for i := 0; i <= j; i++ {
            sum := 0.
            for k := 0; k < i; k++ {
                sum += u[k][j] * l[i][k]
            }
            u[i][j] = a[i][j] - sum
        }
        for i := j; i < len(a); i++ {
            sum := 0.
            for k := 0; k < j; k++ {
                sum += u[k][j] * l[i][k]
            }
            l[i][j] = (a[i][j] - sum) / u[j][j]
        }
    }
    return
}

func main() {
    showLU(matrix{
        {1, 3, 5},
        {2, 4, 7},
        {1, 1, 0}})
    showLU(matrix{
        {11, 9, 24, 2},
        {1, 5, 2, 6},
        {3, 17, 18, 1},
        {2, 5, 7, 1}})
}

func showLU(a matrix) {
    a.print("\na")
    l, u, p := a.lu()
    l.print("l")
    u.print("u")
    p.print("p")
}
```

```txt

a:
   1.00000   3.00000   5.00000
   2.00000   4.00000   7.00000
   1.00000   1.00000   0.00000
l:
   1.00000   0.00000   0.00000
   0.50000   1.00000   0.00000
   0.50000  -1.00000   1.00000
u:
   2.00000   4.00000   7.00000
   0.00000   1.00000   1.50000
   0.00000   0.00000  -2.00000
p:
   0.00000   1.00000   0.00000
   1.00000   0.00000   0.00000
   0.00000   0.00000   1.00000

a:
  11.00000   9.00000  24.00000   2.00000
   1.00000   5.00000   2.00000   6.00000
   3.00000  17.00000  18.00000   1.00000
   2.00000   5.00000   7.00000   1.00000
l:
   1.00000   0.00000   0.00000   0.00000
   0.27273   1.00000   0.00000   0.00000
   0.09091   0.28750   1.00000   0.00000
   0.18182   0.23125   0.00360   1.00000
u:
  11.00000   9.00000  24.00000   2.00000
   0.00000  14.54545  11.45455   0.45455
   0.00000   0.00000  -3.47500   5.68750
   0.00000   0.00000   0.00000   0.51079
p:
   1.00000   0.00000   0.00000   0.00000
   0.00000   0.00000   1.00000   0.00000
   0.00000   1.00000   0.00000   0.00000
   0.00000   0.00000   0.00000   1.00000

```


### Flat representation


```go
package main

import "fmt"

type matrix struct {
    stride int
    ele    []float64
}

func (m *matrix) print(heading string) {
    if heading > "" {
        fmt.Print("\n", heading, "\n")
    }
    for e := 0; e < len(m.ele); e += m.stride {
        fmt.Printf("%8.5f ", m.ele[e:e+m.stride])
        fmt.Println()
    }
}

func (m1 *matrix) mul(m2 *matrix) (m3 *matrix, ok bool) {
    if m1.stride*m2.stride != len(m2.ele) {
        return nil, false
    }
    m3 = &matrix{m2.stride, make([]float64, (len(m1.ele)/m1.stride)*m2.stride)}
    for m1c0, m3x := 0, 0; m1c0 < len(m1.ele); m1c0 += m1.stride {
        for m2r0 := 0; m2r0 < m2.stride; m2r0++ {
            for m1x, m2x := m1c0, m2r0; m2x < len(m2.ele); m2x += m2.stride {
                m3.ele[m3x] += m1.ele[m1x] * m2.ele[m2x]
                m1x++
            }
            m3x++
        }
    }
    return m3, true
}

func zero(rows, cols int) *matrix {
    return &matrix{cols, make([]float64, rows*cols)}
}

func eye(n int) *matrix {
    m := zero(n, n)
    for ix := 0; ix < len(m.ele); ix += n + 1 {
        m.ele[ix] = 1
    }
    return m
}

func (a *matrix) pivotize() *matrix {
    pv := make([]int, a.stride)
    for i := range pv {
        pv[i] = i
    }
    for j, dx := 0, 0; j < a.stride; j++ {
        row := j
        max := a.ele[dx]
        for i, ixcj := j, dx; i < a.stride; i++ {
            if a.ele[ixcj] > max {
                max = a.ele[ixcj]
                row = i
            }
            ixcj += a.stride
        }
        if j != row {
            pv[row], pv[j] = pv[j], pv[row]
        }
        dx += a.stride + 1
    }
    p := zero(a.stride, a.stride)
    for r, c := range pv {
        p.ele[r*a.stride+c] = 1
    }
    return p
}

func (a *matrix) lu() (l, u, p *matrix) {
    l = zero(a.stride, a.stride)
    u = zero(a.stride, a.stride)
    p = a.pivotize()
    a, _ = p.mul(a)
    for j, jxc0 := 0, 0; j < a.stride; j++ {
        l.ele[jxc0+j] = 1
        for i, ixc0 := 0, 0; ixc0 <= jxc0; i++ {
            sum := 0.
            for k, kxcj := 0, j; k < i; k++ {
                sum += u.ele[kxcj] * l.ele[ixc0+k]
                kxcj += a.stride
            }
            u.ele[ixc0+j] = a.ele[ixc0+j] - sum
            ixc0 += a.stride
        }
        for ixc0 := jxc0; ixc0 < len(a.ele); ixc0 += a.stride {
            sum := 0.
            for k, kxcj := 0, j; k < j; k++ {
                sum += u.ele[kxcj] * l.ele[ixc0+k]
                kxcj += a.stride
            }
            l.ele[ixc0+j] = (a.ele[ixc0+j] - sum) / u.ele[jxc0+j]
        }
        jxc0 += a.stride
    }
    return
}

func main() {
    showLU(&matrix{3, []float64{
        1, 3, 5,
        2, 4, 7,
        1, 1, 0}})
    showLU(&matrix{4, []float64{
        11, 9, 24, 2,
        1, 5, 2, 6,
        3, 17, 18, 1,
        2, 5, 7, 1}})
}

func showLU(a *matrix) {
    a.print("\na")
    l, u, p := a.lu()
    l.print("l")
    u.print("u")
    p.print("p")
}
```

Output is same as from 2D solution.


### Library gonum/mat


```go
package main

import (
    "fmt"

    "gonum.org/v1/gonum/mat"
)

func main() {
    showLU(mat.NewDense(3, 3, []float64{
        1, 3, 5,
        2, 4, 7,
        1, 1, 0,
    }))
    fmt.Println()
    showLU(mat.NewDense(4, 4, []float64{
        11, 9, 24, 2,
        1, 5, 2, 6,
        3, 17, 18, 1,
        2, 5, 7, 1,
    }))
}

func showLU(a *mat.Dense) {
    fmt.Printf("a: %v\n\n", mat.Formatted(a, mat.Prefix("   ")))
    var lu mat.LU
    lu.Factorize(a)
    l := lu.LTo(nil)
    u := lu.UTo(nil)
    fmt.Printf("l: %.5f\n\n", mat.Formatted(l, mat.Prefix("   ")))
    fmt.Printf("u: %.5f\n\n", mat.Formatted(u, mat.Prefix("   ")))
    fmt.Println("p:", lu.Pivot(nil))
}
```

Pivot format is a little different here.  (But library solutions don't really meet task requirements anyway.)

```txt

a: ⎡1  3  5⎤
   ⎢2  4  7⎥
   ⎣1  1  0⎦

l: ⎡ 1.00000   0.00000   0.00000⎤
   ⎢ 0.50000   1.00000   0.00000⎥
   ⎣ 0.50000  -1.00000   1.00000⎦

u: ⎡ 2.00000   4.00000   7.00000⎤
   ⎢ 0.00000   1.00000   1.50000⎥
   ⎣ 0.00000   0.00000  -2.00000⎦

p: [1 0 2]

a: ⎡11   9  24   2⎤
   ⎢ 1   5   2   6⎥
   ⎢ 3  17  18   1⎥
   ⎣ 2   5   7   1⎦

l: ⎡1.00000  0.00000  0.00000  0.00000⎤
   ⎢0.27273  1.00000  0.00000  0.00000⎥
   ⎢0.09091  0.28750  1.00000  0.00000⎥
   ⎣0.18182  0.23125  0.00360  1.00000⎦

u: ⎡11.00000   9.00000  24.00000   2.00000⎤
   ⎢ 0.00000  14.54545  11.45455   0.45455⎥
   ⎢ 0.00000   0.00000  -3.47500   5.68750⎥
   ⎣ 0.00000   0.00000   0.00000   0.51079⎦

p: [0 2 1 3]

```



### Library go.matrix


```go
package main

import (
    "fmt"

    mat "github.com/skelterjohn/go.matrix"
)

func main() {
    showLU(mat.MakeDenseMatrixStacked([][]float64{
        {1, 3, 5},
        {2, 4, 7},
        {1, 1, 0}}))
    showLU(mat.MakeDenseMatrixStacked([][]float64{
        {11, 9, 24, 2},
        {1, 5, 2, 6},
        {3, 17, 18, 1},
        {2, 5, 7, 1}}))
}

func showLU(a *mat.DenseMatrix) {
    fmt.Printf("\na:\n%v\n", a)
    l, u, p := a.LU()
    fmt.Printf("l:\n%v\n", l)
    fmt.Printf("u:\n%v\n", u)
    fmt.Printf("p:\n%v\n", p)
}
```

```txt

a:
{1, 3, 5,
 2, 4, 7,
 1, 1, 0}
l:
{  1,   0,   0,
 0.5,   1,   0,
 0.5,  -1,   1}
u:
{  2,   4,   7,
   0,   1, 1.5,
   0,   0,  -2}
p:
{0, 1, 0,
 1, 0, 0,
 0, 0, 1}

a:
{11,  9, 24,  2,
  1,  5,  2,  6,
  3, 17, 18,  1,
  2,  5,  7,  1}
l:
{       1,        0,        0,        0,
 0.272727,        1,        0,        0,
 0.090909,   0.2875,        1,        0,
 0.181818,  0.23125, 0.003597,        1}
u:
{       11,         9,        24,         2,
         0, 14.545455, 11.454545,  0.454545,
         0,         0,    -3.475,    5.6875,
         0,         0,         0,  0.510791}
p:
{1, 0, 0, 0,
 0, 0, 1, 0,
 0, 1, 0, 0,
 0, 0, 0, 1}

```




## Haskell

''Without elem-at-index modifications; doesn't find maximum but any non-zero element''

```Haskell

import Data.List
import Data.Maybe
import Text.Printf

-- a matrix is represented as a list of columns
mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult a b = [ [ sum $ zipWith (*) ak bj | ak <- (transpose a) ] | bj <- b ]

nth mA i j = (mA !! j) !! i

idMatrixPart n m k = [ [if (i==j) then 1 else 0 | i <- [1..n]] | j <- [k..m]]
idMatrix n = idMatrixPart n n 1

permMatrix n ix1 ix2 =
    [ [ if ((i==ix1 && j==ix2) || (i==ix2 && j==ix1) || (i==j && j /= ix1 && i /= ix2))
        then 1 else 0| i <- [0..n-1]] | j <- [0..n-1]]
permMatrix_inv n ix1 ix2 = permMatrix n ix2 ix1

-- count k from zero
elimColumn :: Int -> [[Rational]] -> Int -> [Rational]
elimMatrix :: Int -> [[Rational]] -> Int -> [[Rational]]
elimMatrix_inv :: Int -> [[Rational]] -> Int -> [[Rational]]

elimColumn n mA k = [(let mAkk = (nth mA k k) in  if (i>k) then (-(nth mA i k)/mAkk)
    else if (i==k) then 1 else 0) | i <- [0..n-1]]
elimMatrix n mA k = (idMatrixPart n k 1) ++ [elimColumn n mA k] ++ (idMatrixPart n n (k+2))
elimMatrix_inv n mA k = (idMatrixPart n k 1) ++ --mA is elimMatrix there
    [let c = (mA!!k) in [if (i==k) then 1 else if (i<k) then 0 else (-(c!!i)) | i <- [0..n-1]]]
     ++ (idMatrixPart n n (k+2))

swapIndx :: [[Rational]] -> Int -> Int
swapIndx mA k = fromMaybe k (findIndex (>0) (drop k (mA!!k)))

-- LUP; lupStep returns [L:U:P]
paStep_recP :: Int -> [[Rational]] -> [[Rational]] -> [[Rational]] -> Int -> [[[Rational]]]
paStep_recM :: Int -> [[Rational]] -> [[Rational]] -> [[Rational]] -> Int -> [[[Rational]]]
lupStep :: Int -> [[Rational]] -> [[[Rational]]]

paStep_recP n mP mA mL cnt =
    let mPt = permMatrix n cnt (swapIndx mA cnt) in
        let mPtInv = permMatrix_inv n cnt (swapIndx mA cnt) in
    if (cnt >= n) then [(mmult mP mL),mA,mP] else
        (paStep_recM n (mmult mPt mP) (mmult mPt mA) (mmult mL mPtInv) cnt)

paStep_recM n mP mA mL cnt =
    let mMt = elimMatrix n mA cnt in
        let mMtInv = elimMatrix_inv n mMt cnt in
    paStep_recP n mP (mmult mMt mA) (mmult mL mMtInv) (cnt + 1)

lupStep n mA = paStep_recP n (idMatrix n) mA (idMatrix n) 0

--IO
matrixFromRationalToString m = concat $ intersperse "\n"
    (map (\x -> unwords $ printf "%8.4f" <$> (x::[Double]))
        (transpose (matrixFromRational m))) where
        matrixFromRational m = map (\x -> map fromRational x) m

solveTask mY = let mLUP = lupStep (length mY) mY in
    putStrLn ("A: \n" ++ matrixFromRationalToString mY) >>
    putStrLn ("L: \n" ++ matrixFromRationalToString (mLUP!!0)) >>
    putStrLn ("U: \n" ++ matrixFromRationalToString (mLUP!!1)) >>
    putStrLn ("P: \n" ++ matrixFromRationalToString (mLUP!!2)) >>
    putStrLn ("Verify: PA\n" ++ matrixFromRationalToString (mmult (mLUP!!2) mY)) >>
    putStrLn ("Verify: LU\n" ++ matrixFromRationalToString (mmult (mLUP!!0) (mLUP!!1)))

mY1 = [[1, 2, 1], [3, 4, 7], [5, 7, 0]] :: [[Rational]]
mY2 = [[11, 1, 3, 2], [9, 5, 17, 5], [24, 2, 18, 7], [2, 6, 1, 1]] :: [[Rational]]
main = putStrLn "Task1: \n" >> solveTask mY1 >>
    putStrLn "Task2: \n" >> solveTask mY2

```

```txt

Task1:

A:
  1.0000   3.0000   5.0000
  2.0000   4.0000   7.0000
  1.0000   7.0000   0.0000
L:
  1.0000   0.0000   0.0000
  2.0000   1.0000   0.0000
  1.0000  -2.0000   1.0000
U:
  1.0000   3.0000   5.0000
  0.0000  -2.0000  -3.0000
  0.0000   0.0000 -11.0000
P:
  1.0000   0.0000   0.0000
  0.0000   1.0000   0.0000
  0.0000   0.0000   1.0000
Verify: PA
  1.0000   3.0000   5.0000
  2.0000   4.0000   7.0000
  1.0000   7.0000   0.0000
Verify: LU
  1.0000   3.0000   5.0000
  2.0000   4.0000   7.0000
  1.0000   7.0000   0.0000
Task2:

A:
 11.0000   9.0000  24.0000   2.0000
  1.0000   5.0000   2.0000   6.0000
  3.0000  17.0000  18.0000   1.0000
  2.0000   5.0000   7.0000   1.0000
L:
  1.0000   0.5556   0.2317   0.0000
  0.0000   1.0000   0.0000   0.0000
  0.0000   1.8889   1.0000   0.0000
  0.0000   0.0909   0.0000   1.0000
U:
  0.0081   0.0000   0.0000   0.5325
 11.0000   9.0000  24.0000   2.0000
-17.7778   0.0000 -27.3333  -2.7778
  0.0000   4.1818  -0.1818   5.8182
P:
  0.0000   0.0000   0.0000   1.0000
  1.0000   0.0000   0.0000   0.0000
  0.0000   0.0000   1.0000   0.0000
  0.0000   1.0000   0.0000   0.0000
Verify: PA
  2.0000   5.0000   7.0000   1.0000
 11.0000   9.0000  24.0000   2.0000
  3.0000  17.0000  18.0000   1.0000
  1.0000   5.0000   2.0000   6.0000
Verify: LU
  2.0000   5.0000   7.0000   1.0000
 11.0000   9.0000  24.0000   2.0000
  3.0000  17.0000  18.0000   1.0000
  1.0000   5.0000   2.0000   6.0000

```


## Idris

'''works with Idris 0.10'''

Uses The Method Of Partial Pivoting

'''Solution:'''

```Idris

module Main

import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix m n t = Vect m (Vect n t)

-- Creates list from 0 to n (not including n)
upTo : (m : Nat) -> Vect m (Fin m)
upTo Z = []
upTo (S n) = 0 :: (map FS (upTo n))

-- Creates list from 0 to n-1  (not including n-1)
upToM1 : (m : Nat) -> (sz ** Vect sz (Fin m))
upToM1 m = case (upTo m) of
                  (y::ys) => (_ ** init(y::ys))
                  [] => (_ ** [])

-- Creates list from i to n (not including n)
fromUpTo : {n : Nat} -> Fin n -> (sz ** Vect sz (Fin n))
fromUpTo {n} m = filter (>= m) (upTo n)

-- Creates list from i+1 to n (not including n)
fromUpTo1 : {n : Nat} -> Fin n -> (sz ** Vect sz (Fin n))
fromUpTo1 {n} m with (fromUpTo m)
  | (_ ** xs) = case xs of
                  (y::ys) => (_ ** ys)
                  [] => (_ ** [])


-- Create Zero Matrix of size m by n
zeros : (m : Nat) -> (n : Nat) -> Matrix m n Double
zeros m n = replicate m (replicate n 0.0)

replaceAtM : (Fin m, Fin n) -> t -> Matrix m n t -> Matrix m n t
replaceAtM (i, j) e a = replaceAt i (replaceAt j e (index i a)) a

-- Create Identity Matrix of size m by m
eye : (m : Nat) -> Matrix m m Double
eye m = map create1Vec (upTo m)
  where
    set1 : Vect m Double -> Fin m -> Vect m Double
    set1 a n = replaceAt n 1.0 a

    create1Vec : Fin m -> Vect m Double
    create1Vec n = set1 (replicate m 0.0) n


indexM : (Fin m, Fin n) -> Matrix m n t -> t
indexM (i, j) a = index j (index i a)


-- Obtain index for the row containing the
-- largest absolute value for the given column
colAbsMaxIndex : Fin m -> Fin m -> Matrix m m Double -> Fin m
colAbsMaxIndex startRow col a {m} with (fromUpTo startRow)
  | (_ ** xs) =
    snd $ foldl (\(absMax, idx), curIdx =>
          let curAbsVal = abs(indexM (curIdx, col) a) in
            if (curAbsVal > absMax)
              then (curAbsVal, curIdx)
              else (absMax, idx)
        ) (0.0, startRow) xs


-- Swaps two rows in a given matrix
swapRows : Fin m -> Fin m -> Matrix m n t -> Matrix m n t
swapRows r1 r2 a = replaceAt r2 tempRow $ replaceAt r1 (index r2 a) a
  where tempRow = index r1 a


-- Swaps two individual values in a matrix
swapValues : (Fin m, Fin m) -> (Fin m, Fin m) -> Matrix m m Double -> Matrix m m Double
swapValues (i1, j1) (i2, j2) m = replaceAtM (i2, j2) v1 $ replaceAtM (i1, j1) v2 m
  where
      v1 = indexM (i1, j1) m
      v2 = indexM (i2, j2) m

-- Perform row Swap on Lower Triangular Matrix
lSwapRow : Fin m -> Fin m -> Matrix m m Double -> Matrix m m Double
lSwapRow row1 row2 l {m} with (filter (< row1) (upTo m))
  | (_ ** xs) =  foldl (\l',col => swapValues (row1, col) (row2, col) l') l xs


rowSwap : Fin m -> (Matrix m m Double,  Matrix m m Double, Matrix m m Double) ->
                        (Matrix m m Double, Matrix m m Double, Matrix m m Double)
rowSwap col (l,u,p) = (lSwapRow col row l, swapRows col row u, swapRows col row p)
      where row = colAbsMaxIndex col col u


calc : (Fin m) -> (Fin m) -> (Matrix m m Double, Matrix m m Double) ->
                                (Matrix m m Double, Matrix m m Double)
calc i j (l, u) {m} = (l', u')
   where
         l' : Matrix m m Double
         l' = replaceAtM (j, i) ((indexM (j, i) u) / indexM (i, i) u) l

         u'' : (Fin m) -> (Matrix m m Double) -> (Matrix m m Double)
         u'' k u = replaceAtM (j, k) ((indexM (j, k) u) -
                  ((indexM (j, i) l') * (indexM (i, k) u))) u

         u' : (Matrix m m Double)
         u' with (fromUpTo i) | (_ ** xs) = foldl (\curU, idx => u'' idx curU) u xs


-- Perform a single iteration of the algorithm for the given column
iteration : Fin m -> (Matrix m m Double, Matrix m m Double, Matrix m m Double) ->
                        (Matrix m m Double, Matrix m m Double, Matrix m m Double)
iteration i lup {m} = iterate' (rowSwap i lup)

          where
                modify : (Matrix m m Double, Matrix m m Double) ->
                            (Matrix m m Double, Matrix m m Double)
                modify lu with (fromUpTo1 i) | (_ ** xs) =
                                            foldl (\lu',j => calc i j lu') lu xs

                iterate' : (Matrix m m Double, Matrix m m Double, Matrix m m Double) ->
                              (Matrix m m Double, Matrix m m Double, Matrix m m Double)
                iterate' (l, u, p) with (modify (l, u)) | (l', u') = (l', u', p)


-- Generate L, U, P matricies from a given square matrix.
-- Where L * U = A, and P is the permutation matrix
luDecompose : Matrix m m Double -> (Matrix m m Double, Matrix m m Double, Matrix m m Double)
luDecompose a {m} with (upToM1 m)
  | (_ ** xs) = foldl (\lup,idx => iteration idx lup) (eye m,a,eye m) xs



ex1 : (Matrix 3 3 Double, Matrix 3 3 Double, Matrix 3 3 Double)
ex1 = luDecompose [[1, 3, 5], [2, 4, 7], [1, 1, 0]]

ex2 : (Matrix 4 4 Double, Matrix 4 4 Double, Matrix 4 4 Double)
ex2 = luDecompose [[11, 9, 24, 2], [1, 5, 2, 6], [3, 17, 18, 1], [2, 5, 7, 1]]

printEx : (Matrix n n Double, Matrix n n Double, Matrix n n Double) -> IO ()
printEx (l, u, p) = do
  putStr "l:"
  print l
  putStrLn "\n"

  putStr "u:"
  print u
  putStrLn "\n"

  putStr "p:"
  print p
  putStrLn "\n"

main : IO()
main = do
  putStrLn "Solution 1:"
  printEx ex1
  putStrLn "Solution 2:"
  printEx ex2

```


```txt

Solution 1:
l:[[1, 0, 0], [0.5, 1, 0], [0.5, -1, 1]]

u:[[2, 4, 7], [0, 1, 1.5], [0, 0, -2]]

p:[[0, 1, 0], [1, 0, 0], [0, 0, 1]]

Solution 2:
l:[[1, 0, 0, 0], [0.2727272727272727, 1, 0, 0], [0.09090909090909091, 0.2875, 1, 0], [0.1818181818181818, 0.23125, 0.003597122302158069, 1]]

u:[[11, 9, 24, 2], [0, 14.54545454545455, 11.45454545454546, 0.4545454545454546], [0, 0, -3.475, 5.6875], [0, 0, 0, 0.510791366906476]]

p:[[1, 0, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1]]


```



## J

Taken with slight modification from [http://www.jsoftware.com/jwiki/Essays/LU%20Decomposition].

'''Solution:'''

```j
mp=: +/ .*

LU=: 3 : 0
  'm n'=. $ A=. y
  if. 1=m do.
    p ; (=1) ; p{"1 A [ p=. C. (n-1);~.0,(0~:,A)i.1
  else.
    m2=. >.m%2
    'p1 L1 U1'=. LU m2{.A
    D=. (/:p1) {"1 m2}.A
    F=. m2 {."1 D
    E=. m2 {."1 U1
    FE1=. F mp %. E
    G=. m2}."1 D - FE1 mp U1
    'p2 L2 U2'=. LU G
    p3=. (i.m2),m2+p2
    H=. (/:p3) {"1 U1
    (p1{p3) ; (L1,FE1,.L2) ; H,(-n){."1 U2
  end.
)

permtomat=: 1 {.~"0 -@>:@:/:
LUdecompose=: (permtomat&.>@{. , }.)@:LU
```


'''Example use:'''

```j
   A=:3 3$1 3 5 2 4 7 1 1 0
   LUdecompose A
┌─────┬─────┬───────┐
│1 0 0│1 0 0│1  3  5│
│0 1 0│2 1 0│0 _2 _3│
│0 0 1│1 1 1│0  0 _2│
└─────┴─────┴───────┘
   mp/> LUdecompose A
1 3 5
2 4 7
1 1 0

   A=:4 4$11 9 24 2  1 5 2 6  3 17 18 1  2 5 7 1
   LUdecompose A
┌───────┬─────────────────────────────┬─────────────────────────────┐
│1 0 0 0│        1        0        0 0│11       9        24        2│
│0 1 0 0│0.0909091        1        0 0│ 0 4.18182 _0.181818  5.81818│
│0 0 1 0│ 0.272727  3.47826        1 0│ 0       0    12.087 _19.7826│
│0 0 0 1│ 0.181818 0.804348 0.230216 1│ 0       0         0 0.510791│
└───────┴─────────────────────────────┴─────────────────────────────┘
   mp/> LUdecompose A
11  9 24 2
 1  5  2 6
 3 17 18 1
 2  5  7 1
```



## Java

Translation of [[#Common_Lisp|Common Lisp]] via [[#D|D]]
```java
import static java.util.Arrays.stream;
import java.util.Locale;
import static java.util.stream.IntStream.range;

public class Test {

    static double dotProduct(double[] a, double[] b) {
        return range(0, a.length).mapToDouble(i -> a[i] * b[i]).sum();
    }

    static double[][] matrixMul(double[][] A, double[][] B) {
        double[][] result = new double[A.length][B[0].length];
        double[] aux = new double[B.length];

        for (int j = 0; j < B[0].length; j++) {

            for (int k = 0; k < B.length; k++)
                aux[k] = B[k][j];

            for (int i = 0; i < A.length; i++)
                result[i][j] = dotProduct(A[i], aux);
        }
        return result;
    }

    static double[][] pivotize(double[][] m) {
        int n = m.length;
        double[][] id = range(0, n).mapToObj(j -> range(0, n)
                .mapToDouble(i -> i == j ? 1 : 0).toArray())
                .toArray(double[][]::new);

        for (int i = 0; i < n; i++) {
            double maxm = m[i][i];
            int row = i;
            for (int j = i; j < n; j++)
                if (m[j][i] > maxm) {
                    maxm = m[j][i];
                    row = j;
                }

            if (i != row) {
                double[] tmp = id[i];
                id[i] = id[row];
                id[row] = tmp;
            }
        }
        return id;
    }

    static double[][][] lu(double[][] A) {
        int n = A.length;
        double[][] L = new double[n][n];
        double[][] U = new double[n][n];
        double[][] P = pivotize(A);
        double[][] A2 = matrixMul(P, A);

        for (int j = 0; j < n; j++) {
            L[j][j] = 1;
            for (int i = 0; i < j + 1; i++) {
                double s1 = 0;
                for (int k = 0; k < i; k++)
                    s1 += U[k][j] * L[i][k];
                U[i][j] = A2[i][j] - s1;
            }
            for (int i = j; i < n; i++) {
                double s2 = 0;
                for (int k = 0; k < j; k++)
                    s2 += U[k][j] * L[i][k];
                L[i][j] = (A2[i][j] - s2) / U[j][j];
            }
        }
        return new double[][][]{L, U, P};
    }

    static void print(double[][] m) {
        stream(m).forEach(a -> {
            stream(a).forEach(n -> System.out.printf(Locale.US, "%5.1f ", n));
            System.out.println();
        });
        System.out.println();
    }

    public static void main(String[] args) {
        double[][] a = {{1.0, 3, 5}, {2.0, 4, 7}, {1.0, 1, 0}};

        double[][] b = {{11.0, 9, 24, 2}, {1.0, 5, 2, 6}, {3.0, 17, 18, 1},
        {2.0, 5, 7, 1}};

        for (double[][] m : lu(a))
            print(m);

        System.out.println();

        for (double[][] m : lu(b))
            print(m);
    }
}
```


```txt
  1.0   0.0   0.0
  0.5   1.0   0.0
  0.5  -1.0   1.0

  2.0   4.0   7.0
  0.0   1.0   1.5
  0.0   0.0  -2.0

  0.0   1.0   0.0
  1.0   0.0   0.0
  0.0   0.0   1.0


  1.0   0.0   0.0   0.0
  0.3   1.0   0.0   0.0
  0.1   0.3   1.0   0.0
  0.2   0.2   0.0   1.0

 11.0   9.0  24.0   2.0
  0.0  14.5  11.5   0.5
  0.0   0.0  -3.5   5.7
  0.0   0.0   0.0   0.5

  1.0   0.0   0.0   0.0
  0.0   0.0   1.0   0.0
  0.0   1.0   0.0   0.0
  0.0   0.0   0.0   1.0
```



## jq

jq currently does not have builtin support for matrices and therefore
some infrastructure is needed to make the following self-contained.
Matrices here are represented as arrays of arrays in the usual way.

'''Infrastructure'''

```jq
# Create an m x n matrix
def matrix(m; n; init):
  if m == 0 then []
  elif m == 1 then [range(0;n)] | map(init)
  elif m > 0 then
    matrix(1;n;init) as $row
    | [range(0;m)] | map( $row )
  else error("matrix\(m);_;_) invalid")
  end ;

def I(n): matrix(n;n;0) as $m
  | reduce range(0;n) as $i ($m; . | setpath( [$i,$i]; 1));

def dot_product(a; b):
  reduce range(0;a|length) as $i (0; . + (a[$i] * b[$i]) );

# transpose/0 expects its input to be a rectangular matrix
def transpose:
  if (.[0] | length) == 0 then []
  else [map(.[0])] + (map(.[1:]) | transpose)
  end ;

# A and B should both be numeric matrices, A being m by n, and B being n by p.
def multiply(A; B):
  (B[0]|length) as $p
  | (B|transpose) as $BT
  | reduce range(0; A|length) as $i
       ([];
       reduce range(0; $p) as $j
         (.;
          .[$i][$j] = dot_product( A[$i]; $BT[$j] ) ));

def swap_rows(i;j):
  if i == j then .
  else .[i] as $i | .[i] = .[j] | .[j] = $i
  end ;

# Print a matrix neatly, each cell occupying n spaces, but without truncation
def neatly(n):
  def right: tostring | ( " " * (n-length) + .);
  . as $in
  | length as $length
  | reduce range (0;$length) as $i
      (""; . + reduce range(0;$length) as $j
      (""; "\(.) \($in[$i][$j] | right )" ) + "\n" ) ;
```

'''LU decomposition'''

```jq
# Create the pivot matrix for the input matrix.
# Use "range(0;$n) as $i" to handle ill-conditioned cases.
def pivotize:
  def abs: if .<0 then -. else . end;
  length as $n
  | . as $m
  | reduce range(0;$n) as $j
      (I($n);
       # state: [row; max]
       (reduce range(0; $n) as $i
          ([$j, $m[$j][$j]|abs ];
           ($m[$i][$j]|abs) as $a
           | if $a > .[1] then [ $i, $a ] else . end) | .[0]) as $row
        | swap_rows( $j; $row)
      ) ;

# Decompose the input nxn matrix A by PA=LU and return [L, U, P].
def lup:
  def div(i;j):
    if j == 0 then if i==0 then 0 else error("\(i)/0") end
    else i/j
    end;
  . as $A
  | length as $n
  | I($n) as $L         #  matrix($n; $n; 0.0) as $L
  | matrix($n; $n; 0.0) as $U
  | ($A|pivotize) as $P
  | multiply($P;$A) as $A2
  # state: [L, U]
  | reduce range(0; $n) as $i ( [$L, $U];
      reduce range(0; $n) as $j (.;
          .[0] as $L
        | .[1] as $U
        | if ($j >= $i) then
            (reduce range(0;$i) as $k (0; . + ($U[$k][$j] * $L[$i][$k] ))) as $s1
            | [$L, ($U| setpath([$i,$j]; ($A2[$i][$j] - $s1))) ]
          else
            (reduce range(0;$j) as $k (0; . + ($U[$k][$j] * $L[$i][$k]))) as $s2
            | [ ($L | setpath([$i,$j]; div(($A2[$i][$j] - $s2) ; $U[$j][$j] ))), $U ]
          end ))
    | . + [ $P ]
;

```

'''Example 1''':

```jq
def a: [[1, 3, 5], [2, 4, 7], [1, 1, 0]];
a | lup[] | neatly(4)

```

```sh
 $ /usr/local/bin/jq -M -n -r -f LU.jq
    1    0    0
  0.5    1    0
  0.5   -1    1

    2    4    7
    0    1  1.5
    0    0   -2

    0    1    0
    1    0    0
    0    0    1

```

'''Example 2''':

```jq
def b: [[11,9,24,2],[1,5,2,6],[3,17,18,1],[2,5,7,1]];
b | lup[] | neatly(21)
```

```sh
$ /usr/local/bin/jq -M -n -r -f LU.jq
                     1                     0                     0                     0
    0.2727272727272727                     1                     0                     0
   0.09090909090909091                0.2875                     1                     0
   0.18181818181818182   0.23124999999999996 0.0035971223021580693                     1

                    11                     9                    24                     2
                     0    14.545454545454547    11.454545454545455    0.4545454545454546
                     0                     0   -3.4749999999999996                5.6875
                     0                     0                     0     0.510791366906476

                     1                     0                     0                     0
                     0                     0                     1                     0
                     0                     1                     0                     0
                     0                     0                     0                     1
```


'''Example 3''':

```jq

# A|lup|verify(A) should be true
def verify(A):
  .[0] as $L | .[1] as $U | .[2] as $P
  | multiply($P; A) == multiply($L; $U);

def A:
  [[1,  1,  1,  1],
   [1,  1, -1, -1],
   [1, -1,  0,  0],
   [0,  0,  1, -1]];

A|lup|verify(A)
```

 true


## Julia

Julia has the predefined functions `lu`, `lufact` and `lufact!` in the standard library to compute the lu decomposition of a matrix.
```txt
julia> lu([1 3 5 ; 2 4 7 ; 1 1 0])
(
3x3 Array{Float64,2}:
 1.0   0.0  0.0
 0.5   1.0  0.0
 0.5  -1.0  1.0,

3x3 Array{Float64,2}:
 2.0  4.0   7.0
 0.0  1.0   1.5
 0.0  0.0  -2.0,

[2,1,3])
```



## Kotlin


```scala
// version 1.1.4-3

typealias Vector = DoubleArray
typealias Matrix = Array<Vector>

operator fun Matrix.times(other: Matrix): Matrix {
    val rows1 = this.size
    val cols1 = this[0].size
    val rows2 = other.size
    val cols2 = other[0].size
    require(cols1 == rows2)
    val result = Matrix(rows1) { Vector(cols2) }
    for (i in 0 until rows1) {
        for (j in 0 until cols2) {
            for (k in 0 until rows2) {
                result[i][j] += this[i][k] * other[k][j]
            }
        }
    }
    return result
}

fun pivotize(m: Matrix): Matrix {
    val n = m.size
    val im = Array(n) { Vector(n) }
    for (i in 0 until n) im[i][i] = 1.0
    for (i in 0 until n) {
        var max = m[i][i]
        var row = i
        for (j in i until n) {
            if (m[j][i] > max) {
                max = m[j][i]
                row = j
            }
        }
        if (i != row) {
            val t = im[i]
            im[i] = im[row]
            im[row] = t
        }
    }
    return im
}

fun lu(a: Matrix): Array<Matrix> {
    val n = a.size
    val l = Array(n) { Vector(n) }
    val u = Array(n) { Vector(n) }
    val p = pivotize(a)
    val a2 = p * a

    for (j in 0 until n) {
        l[j][j] = 1.0
        for (i in 0 until j + 1) {
            var sum = 0.0
            for (k in 0 until i) sum += u[k][j] * l[i][k]
            u[i][j] = a2[i][j] - sum
        }
        for (i in j until n) {
            var sum2 = 0.0
            for(k in 0 until j) sum2 += u[k][j] * l[i][k]
            l[i][j] = (a2[i][j] - sum2) / u[j][j]
        }
    }
    return arrayOf(l, u, p)
}

fun printMatrix(title: String, m: Matrix, f: String) {
    val n = m.size
    println("\n$title\n")
    for (i in 0 until n) {
        for (j in 0 until n) print("${f.format(m[i][j])}  ")
        println()
    }
}

fun main(args: Array<String>) {
    val a1 = arrayOf(
        doubleArrayOf( 1.0,  3.0,  5.0),
        doubleArrayOf( 2.0,  4.0,  7.0),
        doubleArrayOf( 1.0,  1.0,  0.0)
    )
    val (l1, u1, p1) = lu(a1)
    println("EXAMPLE 1:-")
    printMatrix("A:", a1, "%1.0f")
    printMatrix("L:", l1, "% 7.5f")
    printMatrix("U:", u1, "% 8.5f")
    printMatrix("P:", p1, "%1.0f")

    val a2 = arrayOf(
        doubleArrayOf(11.0,  9.0, 24.0,  2.0),
        doubleArrayOf( 1.0,  5.0,  2.0,  6.0),
        doubleArrayOf( 3.0, 17.0, 18.0,  1.0),
        doubleArrayOf( 2.0,  5.0,  7.0,  1.0)
    )
    val (l2, u2, p2) = lu(a2)
    println("\nEXAMPLE 2:-")
    printMatrix("A:", a2, "%2.0f")
    printMatrix("L:", l2, "%7.5f")
    printMatrix("U:", u2, "%8.5f")
    printMatrix("P:", p2, "%1.0f")
}
```


```txt

EXAMPLE 1:-

A:

1  3  5
2  4  7
1  1  0

L:

 1.00000   0.00000   0.00000
 0.50000   1.00000   0.00000
 0.50000  -1.00000   1.00000

U:

 2.00000   4.00000   7.00000
 0.00000   1.00000   1.50000
 0.00000   0.00000  -2.00000

P:

0  1  0
1  0  0
0  0  1

EXAMPLE 2:-

A:

11   9  24   2
 1   5   2   6
 3  17  18   1
 2   5   7   1

L:

1.00000  0.00000  0.00000  0.00000
0.27273  1.00000  0.00000  0.00000
0.09091  0.28750  1.00000  0.00000
0.18182  0.23125  0.00360  1.00000

U:

11.00000   9.00000  24.00000   2.00000
 0.00000  14.54545  11.45455   0.45455
 0.00000   0.00000  -3.47500   5.68750
 0.00000   0.00000   0.00000   0.51079

P:

1  0  0  0
0  0  1  0
0  1  0  0
0  0  0  1

```



## Maple


```Maple

A:=<<1.0|3.0|5.0>,<2.0|4.0|7.0>,<1.0|1.0|0.0>>:

LinearAlgebra:-LUDecomposition(A);

```

```txt

    [0  1  0]  [              1.0   0.   0.]  [2.  4.                7.]
    [       ]  [                           ]  [                        ]
    [1  0  0], [0.500000000000000  1.0   0.], [0.  1.  1.50000000000000]
    [       ]  [                           ]  [                        ]
    [0  0  1]  [0.500000000000000  -1.  1.0]  [0.  0.               -2.]

```


```Maple

A:=<<11.0|9.0|24.0|2.0>,<1.0|5.0|2.0|6.0>,
    <3.0|17.0|18.0|1.0>,<2.0|5.0|7.0|1.0>>:

with(LinearAlgebra):

LUDecomposition(A);

```

```txt

    [1  0  0  0]
    [          ]
    [0  0  1  0]
    [          ],
    [0  1  0  0]
    [          ]
    [0  0  0  1]

      [               1.0                 0.                   0.   0.]
      [                                                               ]
      [ 0.272727272727273                1.0                   0.   0.]
      [                                                               ],
      [0.0909090909090909  0.287500000000000                  1.0   0.]
      [                                                               ]
      [ 0.181818181818182  0.231250000000000  0.00359712230215807  1.0]

      [11.                9.                24.                 2.]
      [                                                           ]
      [ 0.  14.5454545454545   11.4545454545455  0.454545454545455]
      [                                                           ]
      [ 0.                0.  -3.47500000000000   5.68750000000000]
      [                                                           ]
      [ 0.                0.                 0.  0.510791366906476]


```



## Mathematica


```Mathematica
(*Ex1*)a = {{1, 3, 5}, {2, 4, 7}, {1, 1, 0}};
{lu, p, c} = LUDecomposition[a];
l = LowerTriangularize[lu, -1] + IdentityMatrix[Length[p]];
u = UpperTriangularize[lu];
P = Part[IdentityMatrix[Length[p]], p] ;
MatrixForm /@ {P.a , P, l, u, l.u}

(*Ex2*)a = {{11, 9, 24, 2}, {1, 5, 2, 6}, {3, 17, 18, 1}, {2, 5, 7, 1}};
{lu, p, c} = LUDecomposition[a];
l = LowerTriangularize[lu, -1] + IdentityMatrix[Length[p]];
u = UpperTriangularize[lu];
P = Part[IdentityMatrix[Length[p]], p] ;
MatrixForm /@ {P.a , P, l, u, l.u}

```

[[File:LUex1.png]]
[[File:LUex2.png]]


=={{header|MATLAB}} / {{header|Octave}}==

LU decomposition is part of language


```Matlab
  A = [
  1   3   5
  2   4   7
  1   1   0];

  [L,U,P] = lu(A)
```

```txt

  L =

   1.00000   0.00000   0.00000
   0.50000   1.00000   0.00000
   0.50000  -1.00000   1.00000

  U =

   2.00000   4.00000   7.00000
   0.00000   1.00000   1.50000
   0.00000   0.00000  -2.00000

  P =

   0   1   0
   1   0   0
   0   0   1

```

2nd example:

```Matlab
  A = [
   11    9   24    2
    1    5    2    6
    3   17   18    1
    2    5    7    1 ];

  [L,U,P] = lu(A)
```

```txt

  L =

   1.00000   0.00000   0.00000   0.00000
   0.27273   1.00000   0.00000   0.00000
   0.09091   0.28750   1.00000   0.00000
   0.18182   0.23125   0.00360   1.00000

  U =

   11.00000    9.00000   24.00000    2.00000
    0.00000   14.54545   11.45455    0.45455
    0.00000    0.00000   -3.47500    5.68750
    0.00000    0.00000    0.00000    0.51079

  P =

   1   0   0   0
   0   0   1   0
   0   1   0   0
   0   0   0   1

```



### Creating a MATLAB function


```Matlab

function [ P, L, U ] = LUdecomposition(A)

% Ensures A is n by n
sz = size(A);
if sz(1)~=sz(2)
    fprintf('A is not n by n\n');
    clear x;
    return;
end

n = sz(1);
L = eye(n);
P = eye(n);
U = A;

for i=1:sz(1)

    % Row reducing
    if U(i,i)==0
        maximum = max(abs(U(i:end,1)));
        for k=1:n
           if maximum == abs(U(k,i))
               temp = U(1,:);
               U(1,:) = U(k,:);
               U(k,:) = temp;

               temp = P(:,1);
               P(1,:) = P(k,:);
               P(k,:) = temp;
           end
        end

    end

    if U(i,i)~=1
        temp = eye(n);
        temp(i,i)=U(i,i);
        L = L * temp;
        U(i,:) = U(i,:)/U(i,i); %Ensures the pivots are 1.
    end

    if i~=sz(1)

        for j=i+1:length(U)
            temp = eye(n);
            temp(j,i) = U(j,i);
            L = L * temp;
            U(j,:) = U(j,:)-U(j,i)*U(i,:);

        end
    end


end
P = P';
end


```


## Maxima



```maxima
/* LU decomposition is built-in */

a: hilbert_matrix(4)$

/* LU in "packed" form */

lup: lu_factor(a);
/* [matrix([1,   1/2,  1/3,   1/4   ],
           [1/2, 1/12, 1/12,  3/40  ],
           [1/3, 1,    1/180, 1/120 ],
           [1/4, 9/10, 3/2,   1/2800]),
    [1, 2, 3, 4], generalring] */

/* extract actual factors */

get_lu_factors(lup);
/* [matrix([1, 0, 0, 0],
           [0, 1, 0, 0],
           [0, 0, 1, 0],
           [0, 0, 0, 1]),

    matrix([1,   0,    0,   0],
           [1/2, 1,    0,   0],
           [1/3, 1,    1,   0],
           [1/4, 9/10, 3/2, 1]),

    matrix([1, 1/2,  1/3,   1/4   ],
           [0, 1/12, 1/12,  3/40  ],
           [0, 0,    1/180, 1/120 ],
           [0, 0,    0,     1/2800])
   ] */

/* solve for a given right-hand side */

lu_backsub(lup, transpose([1, 1, -1, -1]));
/* matrix([-204], [2100], [-4740], [2940]) */
```



## PARI/GP



```parigp
matlup(M) =
{
  my (L = matid(#M), U = M, P = L);

  for (i = 1, #M-1,     \\ pivoting
    p = M[z=i,i];
    for (k = i, #M, if (M[k,i] > p, p = M[z=k,i]));

    if (i != z,         \\ swap rows
      k = U[i,]; U[i,] = U[z,]; U[z,] = k;
      k = P[i,]; P[i,] = P[z,]; P[z,] = k;
    );
  );

  for (i = 1, #M-1,     \\ decompose
    for (k = i+1, #M,
      L[k,i] = U[k,i] / U[i,i];
      for (j = i, #M, U[k,j] -= L[k,i] * U[i,j])
    )
  );

  [L,U,P]       \\ return L,U,P triple matrix
}
```


Output:

```txt

gp > [L,U,P] = matlup([1,3,5;2,4,7;1,1,0]);

gp > L

[  1  0 0]

[1/2  1 0]

[1/2 -1 1]

gp > U

[2 4   7]

[0 1 3/2]

[0 0  -2]

gp > P

[0 1 0]

[1 0 0]

[0 0 1]

gp > [L,U,P] = matlup([11,9,24,2;1,5,2,6;3,17,18,1;2,5,7,1]);

gp > L

[   1      0     0 0]

[3/11      1     0 0]

[1/11  23/80     1 0]

[2/11 37/160 1/278 1]

gp > U

[11      9      24      2]

[ 0 160/11  126/11   5/11]

[ 0      0 -139/40  91/16]

[ 0      0       0 71/139]

gp > P

[1 0 0 0]

[0 0 1 0]

[0 1 0 0]

[0 0 0 1]

```



## Perl

```perl
use List::Util qw(sum);

for $test (
    [[1, 3, 5],
     [2, 4, 7],
     [1, 1, 0]],

    [[11,  9, 24,  2],
     [ 1,  5,  2,  6],
     [ 3, 17, 18,  1],
     [ 2,  5,  7,  1]]
) {
    my($P, $AP, $L, $U) = lu(@$test);
    say_it('A matrix', @$test);
    say_it('P matrix',  @$P);
    say_it('AP matrix', @$AP);
    say_it('L matrix',  @$L);
    say_it('U matrix',  @$U);

}

sub lu {
    my (@a) = @_;
    my $n = +@a;
    my @P  = pivotize(@a);
    my $AP = mmult(\@P, \@a);
    my @L  = matrix_ident($n);
    my @U  = matrix_zero($n);
    for $i (0..$n-1) {
        for $j (0..$n-1) {
            if ($j >= $i) {
                $U[$i][$j] =  $$AP[$i][$j] - sum map { $U[$_][$j] * $L[$i][$_] } 0..$i-1;
            } else {
                $L[$i][$j] = ($$AP[$i][$j] - sum map { $U[$_][$j] * $L[$i][$_] } 0..$j-1) / $U[$j][$j];
            }
        }
    }
    return \@P, $AP, \@L, \@U;
}

sub pivotize {
    my(@m) = @_;
    my $size = +@m;
    my @id = matrix_ident($size);
    for $i (0..$size-1) {
        my $max = $m[$i][$i];
        my $row = $i;
        for $j ($i .. $size-2) {
            if ($m[$j][$i] > $max) {
                $max = $m[$j][$i];
                $row = $j;
            }
        }
        ($id[$row],$id[$i]) = ($id[$i],$id[$row]) if $row != $i;
    }
    @id
}

sub matrix_zero  { my($n) = @_; map { [ (0) x $n ] } 0..$n-1 }
sub matrix_ident { my($n) = @_; map { [ (0) x $_, 1, (0) x ($n-1 - $_) ] } 0..$n-1 }

sub mmult {
  local *a = shift;
  local *b = shift;
  my @p = [];
  my $rows = @a;
  my $cols = @{ $b[0] };
  my $n = @b - 1;
  for (my $r = 0 ; $r < $rows ; ++$r) {
      for (my $c = 0 ; $c < $cols ; ++$c) {
          $p[$r][$c] += $a[$r][$_] * $b[$_][$c] foreach 0 .. $n;
      }
  }
  return [@p];
}

sub say_it {
    my($message, @array) = @_;
    print "$message\n";
    $line = sprintf join("\n" => map join(" " => map(sprintf("%8.5f", $_), @$_)), @{+\@array})."\n";
    $line =~ s/\.00000/      /g;
    $line =~ s/0000\b/    /g;
    print "$line\n";
}

```

<pre style="height:35ex">A matrix
 1        3        5
 2        4        7
 1        1        0

P matrix
 0        1        0
 1        0        0
 0        0        1

AP matrix
 2        4        7
 1        3        5
 1        1        0

L matrix
 1        0        0
 0.5      1        0
 0.5     -1        1

U matrix
 2        4        7
 0        1        1.5
 0        0       -2

A matrix
11        9       24        2
 1        5        2        6
 3       17       18        1
 2        5        7        1

P matrix
 1        0        0        0
 0        0        1        0
 0        1        0        0
 0        0        0        1

AP matrix
11        9       24        2
 3       17       18        1
 1        5        2        6
 2        5        7        1

L matrix
 1        0        0        0
 0.27273  1        0        0
 0.09091  0.28750  1        0
 0.18182  0.23125  0.00360  1

U matrix
11        9       24        2
 0       14.54545 11.45455  0.45455
 0        0       -3.47500  5.68750
 0        0        0        0.51079
```



## Perl 6

Translation of Ruby.


```perl6
for (  [1, 3, 5], # Test Matrices
       [2, 4, 7],
       [1, 1, 0]
    ),
    (  [11,  9, 24,  2],
       [ 1,  5,  2,  6],
       [ 3, 17, 18,  1],
       [ 2,  5,  7,  1]
    )
    -> @test {
    say-it 'A Matrix', @test;
    say-it( $_[0], @($_[1]) ) for 'P Matrix', 'Aʼ Matrix', 'L Matrix', 'U Matrix' Z, lu @test;
}

sub lu (@a) {
    die unless @a.&is-square;
    my $n = +@a;
    my @P = pivotize @a;
    my @Aʼ = mmult @P, @a;
    my @L = matrix-ident $n;
    my @U = matrix-zero  $n;
    for ^$n -> $i {
        for ^$n -> $j {
            if $j >= $i {
                @U[$i][$j] =  @Aʼ[$i][$j] - [+] map { @U[$_][$j] * @L[$i][$_] }, ^$i
            } else {
                @L[$i][$j] = (@Aʼ[$i][$j] - [+] map { @U[$_][$j] * @L[$i][$_] }, ^$j) / @U[$j][$j];
            }
        }

    }
    return @P, @Aʼ, @L, @U;
}

sub pivotize (@m) {
    my $size = +@m;
    my @id = matrix-ident $size;
    for ^$size -> $i {
        my $max = @m[$i][$i];
        my $row = $i;
        for $i ..^ $size -> $j {
            if @m[$j][$i] > $max {
                $max = @m[$j][$i];
                $row = $j;
            }
        }
        if $row != $i {
            @id[$row, $i] = @id[$i, $row]
        }
    }
    @id
}

sub is-square (@m) { so @m == all @m[*] }

sub matrix-zero ($n, $m = $n) { map { [ flat 0 xx $n ] }, ^$m }

sub matrix-ident ($n) { map { [ flat 0 xx $_, 1, 0 xx $n - 1 - $_ ] }, ^$n }

sub mmult(@a,@b) {
    my @p;
    for ^@a X ^@b[0] -> ($r, $c) {
        @p[$r][$c] += @a[$r][$_] * @b[$_][$c] for ^@b;
    }
    @p
}

sub rat-int ($num) {
    return $num unless $num ~~ Rat;
    return $num.narrow if $num.narrow.WHAT ~~ Int;
    $num.nude.join: '/';
}

sub say-it ($message, @array) {
    say "\n$message";
    $_».&rat-int.fmt("%7s").say for @array;
}
```

```txt
A Matrix
      1       3       5
      2       4       7
      1       1       0

P Matrix
      0       1       0
      1       0       0
      0       0       1

Aʼ Matrix
      2       4       7
      1       3       5
      1       1       0

L Matrix
      1       0       0
    1/2       1       0
    1/2      -1       1

U Matrix
      2       4       7
      0       1     3/2
      0       0      -2

A Matrix
     11       9      24       2
      1       5       2       6
      3      17      18       1
      2       5       7       1

P Matrix
      1       0       0       0
      0       0       1       0
      0       1       0       0
      0       0       0       1

Aʼ Matrix
     11       9      24       2
      3      17      18       1
      1       5       2       6
      2       5       7       1

L Matrix
      1       0       0       0
   3/11       1       0       0
   1/11   23/80       1       0
   2/11  37/160   1/278       1

U Matrix
     11       9      24       2
      0  160/11  126/11    5/11
      0       0 -139/40   91/16
      0       0       0  71/139

```



## Phix

```Phix
function matrix_mul(sequence a, sequence b)
sequence c
    if length(a[1]) != length(b) then
        return 0
    else
        c = repeat(repeat(0,length(b[1])),length(a))
        for i=1 to length(a) do
            for j=1 to length(b[1]) do
                for k=1 to length(a[1]) do
                    c[i][j] += a[i][k]*b[k][j]
                end for
            end for
        end for
        return c
    end if
end function

function pivotize(sequence m)
    integer n = length(m)
    sequence im = repeat(repeat(0,n),n)
    for i=1 to n do
        im[i][i] = 1
    end for
    for i=1 to n do
        atom mx = m[i][i]
        integer row = i
        for j=i to n do
            if m[j][i]>mx then
                mx = m[j][i]
                row = j
            end if
        end for
        if i!=row then
            {im[i],im[row]} = {im[row],im[i]}
        end if
    end for
    return im
end function

function lu(sequence a)
    integer n = length(a)
    sequence l = repeat(repeat(0,n),n),
             u = l,
             p = pivotize(a),
             a2 = matrix_mul(p,a)

    for j=1 to n do
        l[j][j] = 1.0
        for i=1 to j do
            atom sum1 = 0.0
            for k=1 to i do
                sum1 += u[k][j] * l[i][k]
            end for
            u[i][j] = a2[i][j] - sum1
        end for
        for i=j+1 to n do
            atom sum2 = 0.0
            for k=1 to j do
                sum2 += u[k][j] * l[i][k]
            end for
            l[i][j] = (a2[i][j] - sum2) / u[j][j]
        end for
    end for
    return {a, l, u, p}
end function

constant a = {{{1, 3, 5},
               {2, 4, 7},
               {1, 1, 0}},
              {{11, 9,24, 2},
               { 1, 5, 2, 6},
               { 3,17,18, 1},
               { 2, 5, 7, 1}}}
for i=1 to length(a) do
    ?"== a,l,u,p: =="
    pp(lu(a[i]),{pp_Nest,2,pp_Pause,0})
end for
```

```txt

"== a,l,u,p: =="
{{{1,3,5},
  {2,4,7},
  {1,1,0}},
 {{1,0,0},
  {0.5,1,0},
  {0.5,-1,1}},
 {{2,4,7},
  {0,1,1.5},
  {0,0,-2}},
 {{0,1,0},
  {1,0,0},
  {0,0,1}}}
"== a,l,u,p: =="
{{{11,9,24,2},
  {1,5,2,6},
  {3,17,18,1},
  {2,5,7,1}},
 {{1,0,0,0},
  {0.2727272727,1,0,0},
  {0.09090909091,0.2875,1,0},
  {0.1818181818,0.23125,0.003597122302,1}},
 {{11,9,24,2},
  {0,14.54545455,11.45454545,0.4545454545},
  {0,0,-3.475,5.6875},
  {0,0,0,0.5107913669}},
 {{1,0,0,0},
  {0,0,1,0},
  {0,1,0,0},
  {0,0,0,1}}}

```



## PL/I


```PL/I
(subscriptrange, fofl, size):                         /* 2 Nov. 2013 */
LU_Decomposition: procedure options (main);
   declare a1(3,3) float (18) initial ( 1,  3,  5,
                                        2,  4,  7,
                                        1,  1,  0);
   declare a2(4,4) float (18) initial (11,  9, 24, 2,
                                        1,  5,  2, 6,
                                        3, 17, 18, 1,
                                        2,  5,  7, 1);
   call check(a1);
   call check(a2);


/* In-situ decomposition */
LU: procedure(a, p);
   declare a(*,*) float (18);
   declare p(*)   fixed binary;
   declare (maximum, rtemp) float (18);
   declare (n, i, j, k, ii, temp) fixed binary;

   n = hbound(a,1);
   do i = 1 to n; p(i) = i; end;

   do k = 1 to n-1;

      maximum = 0; ii = k;
      do i = k to n;
         if maximum < abs(a(p(i),k)) then
            do; maximum = abs(a(p(i),k)); ii = i; end;
      end;
      if ii ^= k then do; temp = p(k); p(k) = p(ii); p(ii) = temp; end;

      do i = k+1 to n; a(p(i),k) = a(p(i),k) / a(p(k),k); end;

      do j = k+1 to n;
         do i = k+1 to n;
            a(p(i),j) = a(p(i),j) - a(p(i),k) * a(p(k),j);
         end;
      end;

   end;
end LU;

CHECK: procedure(a);
   declare a(*,*) float (18) nonassignable;

   declare aa(hbound(a,1), hbound(a,2)) float (18);
   declare  L(hbound(a,1), hbound(a,2)) float (18);
   declare  U(hbound(a,1), hbound(a,2)) float (18);
   declare (p(hbound(a,1), hbound(a,2)), ipiv(hbound(a,1)) ) fixed binary;
   declare pp(hbound(a,1), hbound(a,2)) fixed binary;
   declare (i, j, n, temp(hbound(a,1))) fixed binary;

   n = hbound(a,1);
   aa = A; /* work with a copy */
   P = 0; L = 0; U = 0;
   do i = 1 to n;
      p(i,i) = 1; L(i,i) = 1; /* convert permutation vector to a matrix */
   end;

   call LU(aa, ipiv);

   do i = 1 to n;
      do j = 1 to i-1; L(i,j) = aa(ipiv(i),j); end;
      do j = i to n;   U(i,j) = aa(ipiv(i),j); end;
   end;

   pp = p;
   do i = 1 to n;
      p(ipiv(i), *) = pp(i,*);
   end;

   put skip list ('A');
   put edit (A) (skip, (n) f(10,5));

   put skip list ('P');
   put edit (P) (skip, (n) f(11));

   put skip list ('L');
   put edit (L) (skip, (n) f(10,5));

   put skip list ('U');
   put edit (U) (skip, (n) f(10,5));

end CHECK;

end LU_Decomposition;

```

Derived from Fortran version above.
Results:

```txt

A
   1.00000   3.00000   5.00000
   2.00000   4.00000   7.00000
   1.00000   1.00000   0.00000
P
          0          1          0
          1          0          0
          0          0          1
L
   1.00000   0.00000   0.00000
   0.50000   1.00000   0.00000
   0.50000  -1.00000   1.00000
U
   2.00000   4.00000   7.00000
   0.00000   1.00000   1.50000
   0.00000   0.00000  -2.00000
A
  11.00000   9.00000  24.00000   2.00000
   1.00000   5.00000   2.00000   6.00000
   3.00000  17.00000  18.00000   1.00000
   2.00000   5.00000   7.00000   1.00000
P
          1          0          0          0
          0          0          1          0
          0          1          0          0
          0          0          0          1
L
   1.00000   0.00000   0.00000   0.00000
   0.27273   1.00000   0.00000   0.00000
   0.09091   0.28750   1.00000   0.00000
   0.18182   0.23125   0.00360   1.00000
U
  11.00000   9.00000  24.00000   2.00000
   0.00000  14.54545  11.45455   0.45455
   0.00000   0.00000  -3.47500   5.68750
   0.00000   0.00000   0.00000   0.51079

```



## Python

```python
from pprint import pprint

def matrixMul(A, B):
    TB = zip(*B)
    return [[sum(ea*eb for ea,eb in zip(a,b)) for b in TB] for a in A]

def pivotize(m):
    """Creates the pivoting matrix for m."""
    n = len(m)
    ID = [[float(i == j) for i in xrange(n)] for j in xrange(n)]
    for j in xrange(n):
        row = max(xrange(j, n), key=lambda i: abs(m[i][j]))
        if j != row:
            ID[j], ID[row] = ID[row], ID[j]
    return ID

def lu(A):
    """Decomposes a nxn matrix A by PA=LU and returns L, U and P."""
    n = len(A)
    L = [[0.0] * n for i in xrange(n)]
    U = [[0.0] * n for i in xrange(n)]
    P = pivotize(A)
    A2 = matrixMul(P, A)
    for j in xrange(n):
        L[j][j] = 1.0
        for i in xrange(j+1):
            s1 = sum(U[k][j] * L[i][k] for k in xrange(i))
            U[i][j] = A2[i][j] - s1
        for i in xrange(j, n):
            s2 = sum(U[k][j] * L[i][k] for k in xrange(j))
            L[i][j] = (A2[i][j] - s2) / U[j][j]
    return (L, U, P)

a = [[1, 3, 5], [2, 4, 7], [1, 1, 0]]
for part in lu(a):
    pprint(part, width=19)
    print
print
b = [[11,9,24,2],[1,5,2,6],[3,17,18,1],[2,5,7,1]]
for part in lu(b):
    pprint(part)
    print
```

```txt
[[1.0, 0.0, 0.0],
 [0.5, 1.0, 0.0],
 [0.5, -1.0, 1.0]]

[[2.0, 4.0, 7.0],
 [0.0, 1.0, 1.5],
 [0.0, 0.0, -2.0]]

[[0.0, 1.0, 0.0],
 [1.0, 0.0, 0.0],
 [0.0, 0.0, 1.0]]


[[1.0, 0.0, 0.0, 0.0],
 [0.27272727272727271, 1.0, 0.0, 0.0],
 [0.090909090909090912, 0.28749999999999998, 1.0, 0.0],
 [0.18181818181818182, 0.23124999999999996, 0.0035971223021580693, 1.0]]

[[11.0, 9.0, 24.0, 2.0],
 [0.0, 14.545454545454547, 11.454545454545455, 0.45454545454545459],
 [0.0, 0.0, -3.4749999999999996, 5.6875],
 [0.0, 0.0, 0.0, 0.51079136690647597]]

[[1.0, 0.0, 0.0, 0.0],
 [0.0, 0.0, 1.0, 0.0],
 [0.0, 1.0, 0.0, 0.0],
 [0.0, 0.0, 0.0, 1.0]]
```




## R

```txt

> A <- c(1, 2, 1, 3, 4, 1, 5, 7, 0)
> dim(A) <- c(3, 3)
> library(Matrix)
> expand(lu(A))
$L
3 x 3 Matrix of class "dtrMatrix" (unitriangular)
     [,1] [,2] [,3]
[1,]  1.0    .    .
[2,]  0.5  1.0    .
[3,]  0.5 -1.0  1.0

$U
3 x 3 Matrix of class "dtrMatrix"
     [,1] [,2] [,3]
[1,]  2.0  4.0  7.0
[2,]    .  1.0  1.5
[3,]    .    . -2.0

$P
3 x 3 sparse Matrix of class "pMatrix"

[1,] . | .
[2,] | . .
[3,] . . |

```




## Racket


```racket

#lang racket
(require math)
(define A (matrix
           [[1   3   5]
            [2   4   7]
            [1   1   0]]))

(matrix-lu A)
; result:
; (mutable-array #[#[1 0 0]
;                  #[2 1 0]
;                  #[1 1 1]])
; (mutable-array #[#[1 3 5]
;                  #[0 -2 -3]
;                  #[0 0 -2]])

```



## REXX


```rexx
/*REXX program creates a  matrix  from console input, performs/shows  LU  decomposition.*/
#=0;     P.=0;     PA.=0;      L.=0;      U.=0   /*initialize some variables to zero.   */
parse arg x                                      /*obtain matrix elements from the C.L. */
                  call bldAMat;       call showMat 'A'    /*build and display A  matrix.*/
                  call bldPmat;       call showMat 'P'    /*  "    "     "    P     "   */
                  call multMat;       call showMat 'PA'   /*  "    "     "    PA    "   */
  do y=1  for N;  call bldUmat;       call bldLmat        /*build     U  and  L     "   */
  end   /*y*/
                  call showMat 'L';   call showMat 'U'    /*display   L  and  U     "   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bldAMat: ?=words(x);   do N=1  for ?  until N**2>=?                 /*find matrix size. */
                       end  /*N*/
         if N**2\==?  then do;  say '***error*** wrong # of elements entered:'  ?;  exit 9
                           end
                       do    r=1  for N                             /*build   A  matrix.*/
                          do c=1  for N;     #=# + 1;    _=word(x, #);        A.r.c=_
                          if \datatype(_, 'N')  then call er "element isn't numeric: " _
                          end   /*c*/
                       end      /*r*/;       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
bldLmat:               do    r=1  for N                             /*build lower matrix*/
                          do c=1  for N;     if r==c  then do;  L.r.c=1;  iterate;     end
                          if c\==y | r==c | c>r  then iterate
                          _=PA.r.c
                                             do k=1  for c-1;   _=_   -   U.k.c * L.r.k
                                             end  /*k*/
                          L.r.c=_ / U.c.c
                          end   /*c*/
                       end      /*r*/;       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
bldPmat: c=N;          do r=N  by -1  for N; P.r.c=1;    c=c+1      /*build perm. matrix*/
                       if c>N  then c=N%2;   if c==N  then c=1
                       end   /*r*/;          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
bldUmat:               do    r=1  for N;     if r\==y  then iterate /*build upper matrix*/
                          do c=1  for N;     if c<r    then iterate
                          _=PA.r.c
                                             do k=1  for r-1;   _=_   -   U.k.c * L.r.k
                                             end   /*k*/
                          U.r.c=_ / 1
                          end   /*c*/
                       end      /*r*/;       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
multMat:               do      i=1  for N              /*multiply matrix  P & A  ──► PA */
                          do   j=1  for N
                            do k=1  for N;   pa.i.j=(pa.i.j   +   p.i.k * a.k.j)  /  1
                            end   /*k*/
                          end     /*j*/
                       end        /*i*/;     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat: parse arg mat,rows,cols;     say;   rows=word(rows N,1);   cols=word(cols rows,1)
         w=0;          do    r=1  for rows
                          do c=1  for cols;  w=max(w,  length( value( mat'.'r"."c ) ) )
                          end  /*c*/
                       end     /*r*/
         say center(mat  'matrix',  cols * (w + 1) + 7,  "─")       /*display the header*/
                       do    r=1  for rows;  _=
                          do c=1  for cols;  _=_ right( value(mat'.'r"."c),   w + 1)
                          end   /*c*/
                       say _
                       end      /*r*/;       return
```

```txt

──A matrix───
  1  3  5
  2  4  7
  1  1  0

──P matrix───
  0  1  0
  1  0  0
  0  0  1

──PA matrix──
  2  4  7
  1  3  5
  1  1  0

─────L matrix──────
    1    0    0
  0.5    1    0
  0.5   -1    1

─────U matrix──────
    2    4    7
    0    1  1.5
    0    0   -2

```

```txt

─────A matrix──────
  11   9  24   2
   1   5   2   6
   3  17  18   1
   2   5   7   1

───P matrix────
  1  0  0  0
  0  0  1  0
  0  1  0  0
  0  0  0  1

─────PA matrix─────
  11   9  24   2
   3  17  18   1
   1   5   2   6
   2   5   7   1

───────────────────────────L matrix────────────────────────────
              1              0              0              0
    0.272727273              1              0              0
   0.0909090909    0.287500001              1              0
    0.181818182        0.23125  0.00359712804              1

───────────────────────U matrix────────────────────────
           11            9           24            2
            0   14.5454545   11.4545455   0.45454545
            0            0  -3.47500002       5.6875
            0            0            0  0.510791339

```



## Ruby


```ruby
require 'matrix'

class Matrix
  def lu_decomposition
    p = get_pivot
    tmp = p * self
    u = Matrix.zero(row_size).to_a
    l = Matrix.identity(row_size).to_a
    (0 ... row_size).each do |i|
      (0 ... row_size).each do |j|
        if j >= i
          # upper
          u[i][j] = tmp[i,j] - (0 ... i).inject(0.0) {|sum, k| sum + u[k][j] * l[i][k]}
        else
          # lower
          l[i][j] = (tmp[i,j] - (0 ... j).inject(0.0) {|sum, k| sum + u[k][j] * l[i][k]}) / u[j][j]
        end
      end
    end
    [ Matrix[*l], Matrix[*u], p ]
  end

  def get_pivot
    raise ArgumentError, "must be square" unless square?
    id = Matrix.identity(row_size).to_a
    (0 ... row_size).each do |i|
      max = self[i,i]
      row = i
      (i ... row_size).each do |j|
        if self[j,i] > max
          max = self[j,i]
          row = j
        end
      end
      id[i], id[row] = id[row], id[i]
    end
    Matrix[*id]
  end

  def pretty_print(format, head=nil)
    puts head if head
    puts each_slice(column_size).map{|row| format*row_size % row}
  end
end

puts "Example 1:"
a = Matrix[[1,  3,  5],
           [2,  4,  7],
           [1,  1,  0]]
a.pretty_print(" %2d", "A")
l, u, p = a.lu_decomposition
l.pretty_print(" %8.5f", "L")
u.pretty_print(" %8.5f", "U")
p.pretty_print(" %d",    "P")

puts "\nExample 2:"
a = Matrix[[11, 9,24,2],
           [ 1, 5, 2,6],
           [ 3,17,18,1],
           [ 2, 5, 7,1]]
a.pretty_print(" %2d", "A")
l, u, p = a.lu_decomposition
l.pretty_print(" %8.5f", "L")
u.pretty_print(" %8.5f", "U")
p.pretty_print(" %d",    "P")
```


<pre style="height: 40ex; overflow: scroll">
Example 1:
A
  1  3  5
  2  4  7
  1  1  0
L
  1.00000  0.00000  0.00000
  0.50000  1.00000  0.00000
  0.50000 -1.00000  1.00000
U
  2.00000  4.00000  7.00000
  0.00000  1.00000  1.50000
  0.00000  0.00000 -2.00000
P
 0 1 0
 1 0 0
 0 0 1

Example 2:
A
 11  9 24  2
  1  5  2  6
  3 17 18  1
  2  5  7  1
L
  1.00000  0.00000  0.00000  0.00000
  0.27273  1.00000  0.00000  0.00000
  0.09091  0.28750  1.00000  0.00000
  0.18182  0.23125  0.00360  1.00000
U
 11.00000  9.00000 24.00000  2.00000
  0.00000 14.54545 11.45455  0.45455
  0.00000  0.00000 -3.47500  5.68750
  0.00000  0.00000  0.00000  0.51079
P
 1 0 0 0
 0 0 1 0
 0 1 0 0
 0 0 0 1

```


Matrix has a <code>lup_decomposition</code> built-in method.

```ruby
l, u, p = a.lup_decomposition
l.pretty_print(" %8.5f", "L")
u.pretty_print(" %8.5f", "U")
p.pretty_print(" %d",    "P")
```

Output is the same.


## Rust

```Rust

#![allow(non_snake_case)]
use ndarray::{Array, Axis, Array2, arr2, Zip, NdFloat, s};

fn main() {
    println!("Example 1:");
    let A: Array2<f64> = arr2(&[
        [1.0, 3.0, 5.0],
        [2.0, 4.0, 7.0],
        [1.0, 1.0, 0.0],
    ]);
    println!("A \n {}", A);
    let (L, U, P) = lu_decomp(A);
    println!("L \n {}", L);
    println!("U \n {}", U);
    println!("P \n {}", P);

    println!("\nExample 2:");
    let A: Array2<f64> = arr2(&[
        [11.0, 9.0, 24.0, 2.0],
        [1.0, 5.0, 2.0, 6.0],
        [3.0, 17.0, 18.0, 1.0],
        [2.0, 5.0, 7.0, 1.0],
    ]);
    println!("A \n {}", A);
    let (L, U, P) = lu_decomp(A);
    println!("L \n {}", L);
    println!("U \n {}", U);
    println!("P \n {}", P);
}

fn pivot<T>(A: &Array2<T>) -> Array2<T>
where T: NdFloat {
    let matrix_dimension = A.rows();
    let mut P: Array2<T> = Array::eye(matrix_dimension);
    for (i, column) in A.axis_iter(Axis(1)).enumerate() {
        // find idx of maximum value in column i
        let mut max_pos = i;
        for j in i..matrix_dimension {
            if column[max_pos].abs() < column[j].abs() {
                max_pos = j;
            }
        }
        // swap rows of P if necessary
        if max_pos != i {
            swap_rows(&mut P, i, max_pos);
        }
    }
    P
}

fn swap_rows<T>(A: &mut Array2<T>, idx_row1: usize, idx_row2: usize)
where T: NdFloat {
    // to swap rows, get two ArrayViewMuts for the corresponding rows
    // and apply swap elementwise using ndarray::Zip
    let (.., mut matrix_rest) = A.view_mut().split_at(Axis(0), idx_row1);
    let (row0, mut matrix_rest) = matrix_rest.view_mut().split_at(Axis(0), 1);
    let (_matrix_helper, mut matrix_rest) = matrix_rest.view_mut().split_at(Axis(0), idx_row2 - idx_row1 - 1);
    let (row1, ..) = matrix_rest.view_mut().split_at(Axis(0), 1);
    Zip::from(row0).and(row1).apply(std::mem::swap);
}

fn lu_decomp<T>(A: Array2<T>) -> (Array2<T>, Array2<T>, Array2<T>)
where T: NdFloat {

    let matrix_dimension = A.rows();
    assert_eq!(matrix_dimension, A.cols(), "Tried LU decomposition with a non-square matrix.");
    let P = pivot(&A);
    let pivotized_A = P.dot(&A);

    let mut L: Array2<T> = Array::eye(matrix_dimension);
    let mut U: Array2<T> = Array::zeros((matrix_dimension, matrix_dimension));
    for idx_col in 0..matrix_dimension {
        // fill U
        for idx_row in 0..idx_col+1 {
            U[[idx_row, idx_col]] = pivotized_A[[idx_row, idx_col]] -
                U.slice(s![0..idx_row,idx_col]).dot(&L.slice(s![idx_row,0..idx_row]));
        }
        // fill L
        for idx_row in idx_col+1..matrix_dimension {
            L[[idx_row, idx_col]] = (pivotized_A[[idx_row, idx_col]] -
                U.slice(s![0..idx_col,idx_col]).dot(&L.slice(s![idx_row,0..idx_col]))) /
                U[[idx_col, idx_col]];
        }
    }
    (L, U, P)
}

```



<pre style="height: 40ex; overflow: scroll">
Example 1:
A
 [[1, 3, 5],
 [2, 4, 7],
 [1, 1, 0]]
L
 [[1, 0, 0],
 [0.5, 1, 0],
 [0.5, -1, 1]]
U
 [[2, 4, 7],
 [0, 1, 1.5],
 [0, 0, -2]]
P
 [[0, 1, 0],
 [1, 0, 0],
 [0, 0, 1]]

Example 2:
A
 [[11, 9, 24, 2],
 [1, 5, 2, 6],
 [3, 17, 18, 1],
 [2, 5, 7, 1]]
L
 [[1, 0, 0, 0],
 [0.2727272727272727, 1, 0, 0],
 [0.09090909090909091, 0.2875, 1, 0],
 [0.18181818181818182, 0.23124999999999996, 0.0035971223021580693, 1]]
U
 [[11, 9, 24, 2],
 [0, 14.545454545454547, 11.454545454545455, 0.4545454545454546],
 [0, 0, -3.4749999999999996, 5.6875],
 [0, 0, 0, 0.510791366906476]]
P
 [[1, 0, 0, 0],
 [0, 0, 1, 0],
 [0, 1, 0, 0],
 [0, 0, 0, 1]]

```



## Sidef

```ruby
func is_square(m) { m.all { .len == m.len } }
func matrix_zero(n, m=n) { m.of { n.of(0) } }
func matrix_ident(n) { n.of {|i| n.of {|j| i==j ? 1 : 0 } } }
 
func pivotize(m) {
    var size = m.len
    var id = matrix_ident(size)
    for i (^size) {
        var max = m[i][i]
        var row = i
        for j (i .. size-1) {
            if (m[j][i] > max) {
                max = m[j][i]
                row = j
            }
        }
        if (row != i) {
            id.swap(row, i)
        }
    }
    return id
}
 
func mmult(a, b) {
    var p = []
    for r,c (^a ~X ^b[0]) {
        for i (^b) {
            p[r][c] := 0 += (a[r][i] * b[i][c])
        }
    }
    return p
}
 
func lu(a) {
    is_square(a) || die "Defined only for square matrices!";
    var n = a.len
    var P = pivotize(a)
    var Aʼ = mmult(P, a)
    var L = matrix_ident(n)
    var U = matrix_zero(n)
    for i,j (^n ~X ^n) {
        if (j >= i) {
            U[i][j] = (Aʼ[i][j] - ({ U[_][j] * L[i][_] }.map(^i).sum))
        } else {
            L[i][j] = (Aʼ[i][j] - ({ U[_][j] * L[i][_] }.map(^j).sum))/U[j][j]
        }
    }
    return [P, Aʼ, L, U]
}
 
func say_it(message, array) {
    say "\n#{message}"
    array.each { |row|
        say row.map{"%7s" % .as_rat}.join(' ')
    }
}
 
var t = [[
   %n(1 3 5),
   %n(2 4 7),
   %n(1 1 0),
],[
   %n(11  9 24  2),
   %n( 1  5  2  6),
   %n( 3 17 18  1),
   %n( 2  5  7  1),
]]
 
for test (t) {
    say_it('A Matrix', test);
    for a,b (['P Matrix', 'Aʼ Matrix', 'L Matrix', 'U Matrix'] ~Z lu(test)) {
        say_it(a, b)
    }
}
```

<pre style="height: 40ex; overflow: scroll">
A Matrix
      1       3       5
      2       4       7
      1       1       0

P Matrix
      0       1       0
      1       0       0
      0       0       1

Aʼ Matrix
      2       4       7
      1       3       5
      1       1       0

L Matrix
      1       0       0
    1/2       1       0
    1/2      -1       1

U Matrix
      2       4       7
      0       1     3/2
      0       0      -2

A Matrix
     11       9      24       2
      1       5       2       6
      3      17      18       1
      2       5       7       1

P Matrix
      1       0       0       0
      0       0       1       0
      0       1       0       0
      0       0       0       1

Aʼ Matrix
     11       9      24       2
      3      17      18       1
      1       5       2       6
      2       5       7       1

L Matrix
      1       0       0       0
   3/11       1       0       0
   1/11   23/80       1       0
   2/11  37/160   1/278       1

U Matrix
     11       9      24       2
      0  160/11  126/11    5/11
      0       0 -139/40   91/16
      0       0       0  71/139

```



## Stata


###  Builtin LU decoposition

See [http://www.stata.com/help.cgi?mf_lud LU decomposition] in Stata help.


```stata
mata
: lud(a=(1,3,5\2,4,7\1,1,0),l=.,u=.,p=.)

: a
       1   2   3
    +-------------+
  1 |  1   3   5  |
  2 |  2   4   7  |
  3 |  1   1   0  |
    +-------------+

: l
        1    2    3
    +----------------+
  1 |   1    0    0  |
  2 |  .5    1    0  |
  3 |  .5   -1    1  |
    +----------------+

: u
         1     2     3
    +-------------------+
  1 |    2     4     7  |
  2 |    0     1   1.5  |
  3 |    0     0    -2  |
    +-------------------+

: p
       1
    +-----+
  1 |  2  |
  2 |  1  |
  3 |  3  |
    +-----+
```



###  Implementation


```stata
void ludec(real matrix a, real matrix l, real matrix u, real vector p) {
	real scalar i,j,n,s
	real vector js

	l = a
	n = rows(a)
	p = 1::n
	for (i=1; i<n; i++) {
		maxindex(abs(l[i::n,i]), 1, js=., .)
		j = js[1]+i-1
		if (j!=i) {
			l[(i\j),.] = l[(j\i),.]
			p[(i\j)] = p[(j\i)]
		}
		for (j=i+1; j<=n; j++) {
			l[j,i] = s = l[j,i]/l[i,i]
			l[j,i+1..n] = l[j,i+1..n]-s*l[i,i+1..n]
		}
	}

	u = uppertriangle(l)
	l = lowertriangle(l, 1)
}
```


'''Example''':

```stata
: ludec(a=(1,3,5\2,4,7\1,1,0),l=.,u=.,p=.)

: a
       1   2   3
    +-------------+
  1 |  1   3   5  |
  2 |  2   4   7  |
  3 |  1   1   0  |
    +-------------+

: l
        1    2    3
    +----------------+
  1 |   1    0    0  |
  2 |  .5    1    0  |
  3 |  .5   -1    1  |
    +----------------+

: u
         1     2     3
    +-------------------+
  1 |    2     4     7  |
  2 |    0     1   1.5  |
  3 |    0     0    -2  |
    +-------------------+

: p
       1
    +-----+
  1 |  2  |
  2 |  1  |
  3 |  3  |
    +-----+
```



## Tcl


```tcl
package require Tcl 8.5
namespace eval matrix {
    namespace path {::tcl::mathfunc ::tcl::mathop}

    # Construct an identity matrix of the given size
    proc identity {order} {
	set m [lrepeat $order [lrepeat $order 0]]
	for {set i 0} {$i < $order} {incr i} {
	    lset m $i $i 1
	}
	return $m
    }

    # Produce the pivot matrix for a given matrix
    proc pivotize {matrix} {
	set n [llength $matrix]
	set p [identity $n]
	for {set j 0} {$j < $n} {incr j} {
	    set max [lindex $matrix $j $j]
	    set row $j
	    for {set i $j} {$i < $n} {incr i} {
		if {[lindex $matrix $i $j] > $max} {
		    set max [lindex $matrix $i $j]
		    set row $i
		}
	    }
	    if {$j != $row} {
		# Row swap inlined; too trivial to have separate procedure
		set tmp [lindex $p $j]
		lset p $j [lindex $p $row]
		lset p $row $tmp
	    }
	}
	return $p
    }

    # Decompose a square matrix A by PA=LU and return L, U and P
    proc luDecompose {A} {
	set n [llength $A]
	set L [lrepeat $n [lrepeat $n 0]]
	set U $L
	set P [pivotize $A]
	set A [multiply $P $A]

	for {set j 0} {$j < $n} {incr j} {
	    lset L $j $j 1
	    for {set i 0} {$i <= $j} {incr i} {
		lset U $i $j [- [lindex $A $i $j] [SumMul $L $U $i $j $i]]
	    }
	    for {set i $j} {$i < $n} {incr i} {
		set sum [SumMul $L $U $i $j $j]
		lset L $i $j [/ [- [lindex $A $i $j] $sum] [lindex $U $j $j]]
	    }
	}

	return [list $L $U $P]
    }

    # Helper that makes inner loop nicer; multiplies column and row,
    # possibly partially...
    proc SumMul {A B i j kmax} {
	set s 0.0
	for {set k 0} {$k < $kmax} {incr k} {
	    set s [+ $s [* [lindex $A $i $k] [lindex $B $k $j]]]
	}
	return $s
    }
}
```

Support code:

```tcl
# Code adapted from Matrix_multiplication and Matrix_transposition tasks
namespace eval matrix {
    # Get the size of a matrix; assumes that all rows are the same length, which
    # is a basic well-formed-ness condition...
    proc size {m} {
	set rows [llength $m]
	set cols [llength [lindex $m 0]]
	return [list $rows $cols]
    }

    # Matrix multiplication implementation
    proc multiply {a b} {
	lassign [size $a] a_rows a_cols
	lassign [size $b] b_rows b_cols
	if {$a_cols != $b_rows} {
	    error "incompatible sizes: a($a_rows, $a_cols), b($b_rows, $b_cols)"
	}
	set temp [lrepeat $a_rows [lrepeat $b_cols 0]]
	for {set i 0} {$i < $a_rows} {incr i} {
	    for {set j 0} {$j < $b_cols} {incr j} {
		lset temp $i $j [SumMul $a $b $i $j $a_cols]
	    }
	}
	return $temp
    }

    # Pretty printer for matrices
    proc print {matrix {fmt "%g"}} {
	set max [Widest $matrix $fmt]
	lassign [size $matrix] rows cols
	foreach row $matrix {
	    foreach val $row width $max {
		puts -nonewline [format "%*s " $width [format $fmt $val]]
	    }
	    puts ""
	}
    }
    proc Widest {m fmt} {
	lassign [size $m] rows cols
	set max [lrepeat $cols 0]
	foreach row $m {
	    for {set j 0} {$j < $cols} {incr j} {
		set s [format $fmt [lindex $row $j]]
		lset max $j [max [lindex $max $j] [string length $s]]
	    }
	}
	return $max
    }
}
```

Demonstrating:

```tcl
# This does the decomposition and prints it out nicely
proc demo {A} {
    lassign [matrix::luDecompose $A] L U P
    foreach v {A L U P} {
	upvar 0 $v matrix
	puts "${v}:"
	matrix::print $matrix %.5g
	if {$v ne "P"} {puts "---------------------------------"}
    }
}
demo {{1 3 5} {2 4 7} {1 1 0}}
puts "
### ===========================
"
demo {{11 9 24 2} {1 5 2 6} {3 17 18 1} {2 5 7 1}}
```

```txt

A:
1 3 5
2 4 7
1 1 0
---------------------------------
L:
  1  0 0
0.5  1 0
0.5 -1 1
---------------------------------
U:
2 4   7
0 1 1.5
0 0  -2
---------------------------------
P:
0 1 0
1 0 0
0 0 1

### ===========================

A:
11  9 24 2
 1  5  2 6
 3 17 18 1
 2  5  7 1
---------------------------------
L:
       1       0         0 0
 0.27273       1         0 0
0.090909  0.2875         1 0
 0.18182 0.23125 0.0035971 1
---------------------------------
U:
11      9     24       2
 0 14.545 11.455 0.45455
 0      0 -3.475  5.6875
 0      0      0 0.51079
---------------------------------
P:
1 0 0 0
0 0 1 0
0 1 0 0
0 0 0 1

```



## VBA

```vb
Option Base 1
Private Function pivotize(m As Variant) As Variant
    Dim n As Integer: n = UBound(m)
    Dim im() As Double
    ReDim im(n, n)
    For i = 1 To n
        For j = 1 To n
            im(i, j) = 0
        Next j
        im(i, i) = 1
    Next i
    For i = 1 To n
        mx = m(i, i)
        row_ = i
        For j = i To n
            If m(j, i) > mx Then
                mx = m(j, i)
                row_ = j
            End If
        Next j
        If i <> Row Then
            For j = 1 To n
                tmp = im(i, j)
                im(i, j) = im(row_, j)
                im(row_, j) = tmp
            Next j
        End If
    Next i
    pivotize = im
End Function

Private Function lu(a As Variant) As Variant
    Dim n As Integer: n = UBound(a)
    Dim l() As Double
    ReDim l(n, n)
    For i = 1 To n
        For j = 1 To n
            l(i, j) = 0
        Next j
    Next i
    u = l
    p = pivotize(a)
    a2 = WorksheetFunction.MMult(p, a)
    For j = 1 To n
        l(j, j) = 1#
        For i = 1 To j
            sum1 = 0#
            For k = 1 To i
                sum1 = sum1 + u(k, j) * l(i, k)
            Next k
            u(i, j) = a2(i, j) - sum1
        Next i
        For i = j + 1 To n
            sum2 = 0#
            For k = 1 To j
                sum2 = sum2 + u(k, j) * l(i, k)
            Next k
            l(i, j) = (a2(i, j) - sum2) / u(j, j)
        Next i
    Next j
    Dim res(4) As Variant
    res(1) = a
    res(2) = l
    res(3) = u
    res(4) = p
    lu = res
End Function

Public Sub main()

    a = [{1, 3, 5; 2, 4, 7; 1, 1, 0}]
    Debug.Print "== a,l,u,p: =="
    result = lu(a)
    For i = 1 To 4
        For j = 1 To UBound(result(1))
            For k = 1 To UBound(result(1), 2)
                Debug.Print result(i)(j, k),
            Next k
            Debug.Print
        Next j
        Debug.Print
    Next i
    a = [{11, 9,24, 2; 1, 5, 2, 6; 3,17,18, 1; 2, 5, 7, 1}]
    Debug.Print "== a,l,u,p: =="
    result = lu(a)
    For i = 1 To 4
        For j = 1 To UBound(result(1))
            For k = 1 To UBound(result(1), 2)
                Debug.Print Format(result(i)(j, k), "0.#####"),
            Next k
            Debug.Print
        Next j
        Debug.Print
    Next i
End Sub
```
```txt
== a,l,u,p: ==
 1             3             5
 2             4             7
 1             1             0

 1             0             0
 0,5           1             0
 0,5          -1             1

 2             4             7
 0             1             1,5
 0             0            -2

 0             1             0
 1             0             0
 0             0             1

== a,l,u,p: ==
11,           9,            24,           2,
1,            5,            2,            6,
3,            17,           18,           1,
2,            5,            7,            1,

1,            0,            0,            0,
0,27273       1,            0,            0,
0,09091       0,2875        1,            0,
0,18182       0,23125       0,0036        1,

11,           9,            24,           2,
0,            14,54545      11,45455      0,45455
0,            0,            -3,475        5,6875
0,            0,            0,            0,51079

1,            0,            0,            0,
0,            0,            1,            0,
0,            1,            0,            0,
0,            0,            0,            1,
```


## zkl

Using the GNU Scientific Library, which does the decomposition without returning the permutations:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
fcn luTask(A){
   A.LUDecompose();	//  in place, contains L & U
   L:=A.copy().lowerTriangle().setDiagonal(0,0,1);
   U:=A.copy().upperTriangle();
   return(L,U);
}

A:=GSL.Matrix(3,3).set(1,3,5,  2,4,7,   1,1,0);  // example 1
L,U:=luTask(A);
println("L:\n",L.format(),"\nU:\n",U.format());

A:=GSL.Matrix(4,4).set(11.0,  9.0, 24.0, 2.0,	// example 2
			1.0,  5.0,  2.0, 6.0,
			3.0, 17.0, 18.0, 1.0,
			2.0,  5.0,  7.0, 1.0);
L,U:=luTask(A);
println("L:\n",L.format(8,4),"\nU:\n",U.format(8,4));
```

```txt

L:
      1.00,      0.00,      0.00
      0.50,      1.00,      0.00
      0.50,     -1.00,      1.00
U:
      2.00,      4.00,      7.00
      0.00,      1.00,      1.50
      0.00,      0.00,     -2.00
L:
  1.0000,  0.0000,  0.0000,  0.0000
  0.2727,  1.0000,  0.0000,  0.0000
  0.0909,  0.2875,  1.0000,  0.0000
  0.1818,  0.2312,  0.0036,  1.0000
U:
 11.0000,  9.0000, 24.0000,  2.0000
  0.0000, 14.5455, 11.4545,  0.4545
  0.0000,  0.0000, -3.4750,  5.6875
  0.0000,  0.0000,  0.0000,  0.5108

```

Or, using lists:
A matrix is a list of lists, ie list of rows in row major order.

```zkl
fcn make_array(n,m,v){ (m).pump(List.createLong(m).write,v)*n }
fcn eye(n){ // Creates a nxn identity matrix.
   I:=make_array(n,n,0.0);
   foreach j in (n){ I[j][j]=1.0 }
   I
}

// Creates the pivoting matrix for A.
fcn pivotize(A){
   n:=A.len();	// rows
   P:=eye(n);
   foreach i in (n){
      max,row:=A[i][i],i;
      foreach j in ([i..n-1]){
         if(A[j][i]>max) max,row=A[j][i],j;
      }
      if(i!=row) P.swap(i,row);
   }
   // Return P.
   P
}

// Decomposes a square matrix A by PA=LU and returns L, U and P.
fcn lu(A){
   n:=A.len();
   L:=eye(n);
   U:=make_array(n,n,0.0);
   P:=pivotize(A);
   A=matMult(P,A);

   foreach j in (n){
      foreach i in (j+1){
         U[i][j]=A[i][j] - (i).reduce('wrap(s,k){ s + U[k][j]*L[i][k] },0.0);
      }
      foreach i in ([j..n-1]){
         L[i][j]=( A[i][j] -
		   (j).reduce('wrap(s,k){ s + U[k][j]*L[i][k] },0.0) ) /
		 U[j][j];
      }
   }
   // Return L, U and P.
   return(L,U,P);
}

fcn matMult(a,b){
   n,m,p:=a[0].len(),a.len(),b[0].len();
   ans:=make_array(n,m,0.0);
   foreach i,j,k in (m,p,n){ ans[i][j]+=a[i][k]*b[k][j]; }
   ans
}
```

Example 1

```zkl
g:=L(L(1.0,3.0,5.0),L(2.0,4.0,7.0),L(1.0,1.0,0.0));
lu(g).apply2("println");
```

```txt

L(L(1,0,0),L(0.5,1,0),L(0.5,-1,1))
L(L(2,4,7),L(0,1,1.5),L(0,0,-2))
L(L(0,1,0),L(1,0,0),L(0,0,1))

```

Example 2

```zkl
lu(L( L(11.0,  9.0, 24.0, 2.0),
      L( 1.0,  5.0,  2.0, 6.0),
      L( 3.0, 17.0, 18.0, 1.0),
      L( 2.0,  5.0,  7.0, 1.0) )).apply2(T(printM,Console.writeln.fpM("-")));

fcn printM(m)  { m.pump(Console.println,rowFmt) }
fcn rowFmt(row){ ("%9.5f "*row.len()).fmt(row.xplode()) }
```

The list apply2 method is side effects only, it doesn't aggregate results. When given a list of actions, it applies the action and passes the result to the next action. The fpM method is partial application with a mask, "-" truncates the parameters at that point (in this case, no parameters, ie just print a blank line, not the result of printM).
```txt

  1.00000   0.00000   0.00000   0.00000
  0.27273   1.00000   0.00000   0.00000
  0.09091   0.28750   1.00000   0.00000
  0.18182   0.23125   0.00360   1.00000

 11.00000   9.00000  24.00000   2.00000
  0.00000  14.54545  11.45455   0.45455
  0.00000   0.00000  -3.47500   5.68750
  0.00000   0.00000   0.00000   0.51079

  1.00000   0.00000   0.00000   0.00000
  0.00000   0.00000   1.00000   0.00000
  0.00000   1.00000   0.00000   0.00000
  0.00000   0.00000   0.00000   1.00000

```

