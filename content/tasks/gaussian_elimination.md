+++
title = "Gaussian elimination"
description = ""
date = 2019-09-09T12:35:26Z
aliases = []
[extra]
id = 11391
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "algol_68",
  "c",
  "common_lisp",
  "c_sharp",
  "d",
  "delphi",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "klong",
  "kotlin",
  "m2000_interpreter",
  "matlab",
  "ocaml",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "pl_i",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "sidef",
  "rust",
  "stata",
  "tcl",
  "vba",
  "vbscript",
  "zkl",
]
+++

{{task}}[[Category:Matrices]]

## Task

Solve   '''Ax=b'''   using Gaussian elimination then backwards substitution.

'''A'''   being an   '''n''' by '''n'''   matrix.

Also,   '''x''' and '''b'''   are   '''n''' by '''1'''   vectors.

To improve accuracy, please use partial pivoting and scaling.


## See also

:*   the Wikipedia entry:   [[wp:Gaussian elimination|<u>Gaussian elimination</u>]]





## 360 Assembly

```360asm
*        Gaussian elimination      09/02/2019
GAUSSEL  CSECT
         USING  GAUSSEL,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R7,1               j=1
       DO WHILE=(C,R7,LE,N)        do j=1 to n
         LA     R9,1(R7)             j+1
         LR     R6,R9                i=j+1
       DO WHILE=(C,R6,LE,N)          do i=j+1 to n
         LR     R1,R7                  j
         MH     R1,=AL2(NN)            *n
         AR     R1,R7                  +j
         BCTR   R1,0                   j*n+j-1
         SLA    R1,2                   ~
         LE     F0,A-(NN*4)(R1)        a(j,j)
         LR     R1,R6                  i
         MH     R1,=AL2(NN)            *n
         AR     R1,R7                  j
         BCTR   R1,0                   i*n+j-1
         SLA    R1,2                   ~
         LE     F2,A-(NN*4)(R1)        a(i,j)
         DER    F0,F2                  a(j,j)/a(i,j)
         STE    F0,W                   w=a(j,j)/a(i,j)
         LR     R8,R9                  k=j+1
       DO WHILE=(C,R8,LE,N)            do k=j+1 to n
         LR     R1,R7                    j
         MH     R1,=AL2(NN)              *n
         AR     R1,R8                    +k
         BCTR   R1,0                     j*n+k-1
         SLA    R1,2                     ~
         LE     F0,A-(NN*4)(R1)          a(j,k)
         LR     R1,R6                    i
         MH     R1,=AL2(NN)              *n
         AR     R1,R8                    +k
         BCTR   R1,0                     i*n+k-1
         SLA    R1,2                     ~
         LE     F2,A-(NN*4)(R1)          a(i,k)
         LE     F6,W                     w
         MER    F6,F2                    *a(i,k)
         SER    F0,F6                    a(j,k)-w*a(i,k)
         STE    F0,A-(NN*4)(R1)          a(i,k)=a(j,k)-w*a(i,k)
         LA     R8,1(R8)                 k=k+1
       ENDDO    ,                      end do k
         LR     R1,R7                  j
         SLA    R1,2                   ~
         LE     F0,B-4(R1)             b(j)
         LR     R1,R6                  i
         SLA    R1,2                   ~
         LE     F2,B-4(R1)             b(i)
         LE     F6,W                   w
         MER    F6,F2                  *b(i)
         SER    F0,F6                  b(j)-w*b(i)
         STE    F0,B-4(R1)             b(i)=b(j)-w*b(i)
         LA     R6,1(R6)               i=i+1
       ENDDO    ,                    end do i
         LA     R7,1(R7)             j=j+1
       ENDDO    ,                  end do j
         L      R2,N               n
         SLA    R2,2               ~
         LE     F0,B-4(R1)         b(n)
         L      R1,N               n
         MH     R1,=AL2(NN)        *n
         A      R1,N               n
         BCTR   R1,0               n*n+n-1
         SLA    R1,2               ~
         LE     F2,A-(NN*4)(R1)    a(n,n)
         DER    F0,F2              b(n)/a(n,n)
         STE    F0,X-4(R2)         x(n)=b(n)/a(n,n)
         L      R7,N               n
         BCTR   R7,0               j=n-1
       DO WHILE=(C,R7,GE,=F'1')    do j=n-1 to 1 by -1
         LE     F0,=E'0'             0
         STE    F0,W                 w=0
         LA     R9,1(R7)             j+1
         LR     R6,R9                i=j+1
       DO WHILE=(C,R6,LE,N)          do i=j+1 to n
         LR     R1,R7                  j
         MH     R1,=AL2(NN)            *n
         AR     R1,R6                  i
         BCTR   R1,0                   j*n+i-1
         SLA    R1,2                   ~
         LE     F0,A-(NN*4)(R1)        a(j,i)
         LR     R1,R6                  i
         SLA    R1,2                   ~
         LE     F2,X-4(R1)             x(i)
         MER    F0,F2                  a(j,i)*x(i)
         LE     F6,W                   w
         AER    F6,F0                  +a(j,i)*x(i)
         STE    F6,W                   w=w+a(j,i)*x(i)
         LA     R6,1(R6)               i=i+1
       ENDDO    ,                    end do i
         LR     R2,R7                j
         SLA    R2,2                 ~
         LE     F0,B-4(R2)           b(j)
         SE     F0,W                 -w
         LR     R1,R7                j
         MH     R1,=AL2(NN)          *n
         AR     R1,R7                j
         BCTR   R1,0                 j*n+j-1
         SLA    R1,2                 ~
         LE     F2,A-(NN*4)(R1)      a(j,j)
         DER    F0,F2                (b(j)-w)/a(j,j)
         STE    F0,X-4(R2)           x(j)=(b(j)-w)/a(j,j)
         BCTR   R7,0                 j=j-1
       ENDDO    ,                  end do j
         XPRNT  =CL8'SOLUTION',8   print
         MVC    PG,=CL91' '        clear buffer
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n
         LR     R1,R6                i
         SLA    R1,2                 ~
         LE     F0,X-4(R1)           x(i)
         LA     R0,5                 number of decimals
         BAL    R14,FORMATF          edit
         MVC    PG(13),0(R1)         output
         XPRNT  PG,L'PG              print
         LA     R6,1(R6)             i=i+1
       ENDDO    ,                  end do i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
         COPY   plig\$_FORMATF.MLC format F13.n
NN       EQU    (X-B)/4            n
N        DC     A(NN)              n
A        DC  E'1',E'0',E'0',E'0',E'0',E'0'
         DC  E'1',E'0.63',E'0.39',E'0.25',E'0.16',E'0.10'
         DC  E'1',E'1.26',E'1.58',E'1.98',E'2.49',E'3.13'
         DC  E'1',E'1.88',E'3.55',E'6.70',E'12.62',E'23.80'
         DC  E'1',E'2.51',E'6.32',E'15.88',E'39.90',E'100.28'
         DC  E'1',E'3.14',E'9.87',E'31.01',E'97.41',E'306.02'
B        DC  E'-0.01',E'0.61',E'0.91',E'0.99',E'0.60',E'0.02'
X        DS     (NN)E              x(n)
W        DS     E                  w
PG       DC     CL91' '            buffer
         REGEQU
         END    GAUSSEL
```

```txt

SOLUTION
     -0.00999
      1.60279
     -1.61322
      1.24552
     -0.49100
      0.06576

```



## ALGOL 68

'''File: prelude_exception.a68'''
```algol68
# -*- coding: utf-8 -*- #
COMMENT PROVIDES
  MODE FIXED; INT fixed exception, unfixed exception;
  PROC (STRING message) FIXED raise, raise value error
END COMMENT

# Note: ℵ indicates attribute is "private", and
        should not be used outside of this prelude #

MODE FIXED = BOOL; # if an exception is detected, can it be fixed "on-site"? #
FIXED fixed exception = TRUE, unfixed exception = FALSE;

MODE #ℵ#SIMPLEOUTV = [0]UNION(CHAR, STRING, INT, REAL, BOOL, BITS);
MODE #ℵ#SIMPLEOUTM = [0]#ℵ#SIMPLEOUTV;
MODE #ℵ#SIMPLEOUTT = [0]#ℵ#SIMPLEOUTM;
MODE SIMPLEOUT  = [0]#ℵ#SIMPLEOUTT;

PROC raise = (#ℵ#SIMPLEOUT message)FIXED: (
  putf(stand error, ($"Exception:"$, $xg$, message, $l$));
  stop
);

PROC raise value error = (#ℵ#SIMPLEOUT message)FIXED:
  IF raise(message) NE fixed exception THEN exception value error; FALSE FI;

SKIP
```
'''File: prelude_mat_lib.a68'''
```algol68
# -*- coding: utf-8 -*- #
COMMENT PRELUDE REQUIRES
  MODE SCAL = REAL;
  FORMAT scal repr = real repr
  # and various SCAL OPerators #
END COMMENT

COMMENT PRELUDE PROIVIDES
  MODE VEC, MAT;
  OP :=:, -:=, +:=, *:=, /:=;
  FORMAT sub, sep, bus;
  FORMAT vec repr, mat repr
END COMMENT

# Note: ℵ indicates attribute is "private", and
        should not be used outside of this prelude #

INT #ℵ#lwb vec := 1, #ℵ#upb vec := 0;
INT #ℵ#lwb mat := 1, #ℵ#upb mat := 0;
MODE VEC = [lwb vec:upb vec]SCAL,
     MAT = [lwb mat:upb mat,lwb vec:upb vec]SCAL;

FORMAT sub := $"( "$, sep := $", "$, bus := $")"$, nl:=$lxx$;
FORMAT vec repr := $f(sub)n(upb vec - lwb vec)(f(scal repr)f(sep))f(scal repr)f(bus)$;
FORMAT mat repr := $f(sub)n(upb mat - lwb mat)(f( vec repr)f(nl))f( vec repr)f(bus)$;

# OPerators to swap the contents of two VECtors #
PRIO =:= = 1;
OP =:= = (REF VEC u, v)VOID:
  FOR i TO UPB u DO SCAL scal=u[i]; u[i]:=v[i]; v[i]:=scal OD;

OP +:= = (REF VEC lhs, VEC rhs)REF VEC: (
  FOR i TO UPB lhs DO lhs[i] +:= rhs[i] OD;
  lhs
);

OP -:= = (REF VEC lhs, VEC rhs)REF VEC: (
  FOR i TO UPB lhs DO lhs[i] -:= rhs[i] OD;
  lhs
);

OP *:= = (REF VEC lhs, SCAL rhs)REF VEC: (
  FOR i TO UPB lhs DO lhs[i] *:= rhs OD;
  lhs
);

OP /:= = (REF VEC lhs, SCAL rhs)REF VEC: (
  SCAL inv = 1 / rhs; # multiplication is faster #
  FOR i TO UPB lhs DO lhs[i] *:= inv OD;
  lhs
);

SKIP
```
'''File: prelude_gaussian_elimination.a68'''
```algol68
# -*- coding: utf-8 -*- #
COMMENT PRELUDE REQUIRES
  MODE SCAL = REAL,
  REAL near min scal = min real ** 0.99,
  MODE VEC = []REAL,
  MODE MAT = [,]REAL,
  FORMAT scal repr = real repr,
  and various OPerators of MAT and VEC
END COMMENT

COMMENT PRELUDE PROVIDES
  PROC(MAT a, b)MAT gaussian elimination;
  PROC(REF MAT a, b)REF MAT in situ gaussian elimination
END COMMENT

####################################################
# using Gaussian elimination, find x where A*x = b #
####################################################
PROC in situ gaussian elimination = (REF MAT a, b)REF MAT: (
# Note: a and b are modified "in situ", and b is returned as x #

  FOR diag TO UPB a-1 DO
    INT pivot row := diag; SCAL pivot factor := ABS a[diag,diag];
    FOR row FROM diag + 1 TO UPB a DO # Full pivoting #
      SCAL abs a diag = ABS a[row,diag];
      IF abs a diag>=pivot factor THEN
        pivot row := row; pivot factor := abs a diag FI
    OD;
  # now we have the "best" diag to full pivot, do the actual pivot #
    IF diag NE pivot row THEN
    # a[pivot row,] =:= a[diag,]; XXX: unoptimised # #DB#
      a[pivot row,diag:] =:= a[diag,diag:]; # XXX: optimised #
      b[pivot row,] =:= b[diag,] # swap/pivot the diags of a & b #
    FI;

    IF ABS a[diag,diag] <= near min scal THEN
      raise value error("singular matrix") FI;
    SCAL a diag reciprocal := 1 / a[diag, diag];

    FOR row FROM diag+1 TO UPB a DO
      SCAL factor = a[row,diag] * a diag reciprocal;
    # a[row,] -:= factor * a[diag,] XXX: "unoptimised" # #DB#
      a[row,diag+1:] -:= factor * a[diag,diag+1:];# XXX: "optimised" #
      b[row,] -:= factor * b[diag,]
    OD
  OD;

# We have a triangular matrix, at this point we can traverse backwards
  up the diagonal calculating b\A Converting it initial to a diagonal
  matrix, then to the identity.  #

  FOR diag FROM UPB a BY -1 TO 1+LWB a DO

    IF ABS a[diag,diag] <= near min scal THEN
      raise value error("Zero pivot encountered?") FI;
    SCAL a diag reciprocal = 1 / a[diag,diag];

    FOR row TO diag-1 DO
      SCAL factor = a[row,diag] * a diag reciprocal;
    # a[row,diag] -:= factor * a[diag,diag]; XXX: "unoptimised" so remove # #DB#
      b[row,] -:= factor * b[diag,]
    OD;
# Now we have only diagonal elements we can simply divide b
  by the values along the diagonal of A. #
    b[diag,] *:= a diag reciprocal
  OD;

  b # EXIT #
);

PROC gaussian elimination = (MAT in a, in b)MAT: (
# Note: a and b are cloned and not modified "in situ" #
  [UPB in a, 2 UPB in a]SCAL a := in a;
  [UPB in b, 2 UPB in b]SCAL b := in b;
  in situ gaussian elimination(a,b)
);

SKIP
```
'''File: postlude_exception.a68'''
```algol68
# -*- coding: utf-8 -*- #
COMMENT POSTLUDE PROIVIDES
  PROC VOID exception too many iterations, exception value error;
END COMMENT

SKIP EXIT
exception too many iterations:
exception value error:
  stop
```
'''File: test_Gaussian_elimination.a68'''
```algol68
#!/usr/bin/algol68g-full --script #
# -*- coding: utf-8 -*- #

PR READ "prelude_exception.a68" PR;

# define the attributes of the scalar field being used #
MODE SCAL = REAL;
FORMAT scal repr = $g(-0,real width)$;
# create "near min scal" as is scales better then small real #
SCAL near min scal = min real ** 0.99;

PR READ "prelude_mat_lib.a68" PR;
PR READ "prelude_gaussian_elimination.a68" PR;

MAT a =(( 1.00, 0.00, 0.00,  0.00,  0.00,   0.00),
        ( 1.00, 0.63, 0.39,  0.25,  0.16,   0.10),
        ( 1.00, 1.26, 1.58,  1.98,  2.49,   3.13),
        ( 1.00, 1.88, 3.55,  6.70, 12.62,  23.80),
        ( 1.00, 2.51, 6.32, 15.88, 39.90, 100.28),
        ( 1.00, 3.14, 9.87, 31.01, 97.41, 306.02));
VEC b = (-0.01, 0.61, 0.91, 0.99,   0.60,   0.02);

[UPB b,1]SCAL col b; col b[,1]:= b;

upb vec := 2 UPB a;

printf((vec repr, gaussian elimination(a,col b)));

PR READ "postlude_exception.a68" PR
```
'''Output:'''

```txt

( -.010000000000002, 1.602790394502130, -1.613203059905640, 1.245494121371510, -.490989719584686, .065760696175236)

```



## C

This modifies A and b in place, which might not be quite desirable.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define mat_elem(a, y, x, n) (a + ((y) * (n) + (x)))

void swap_row(double *a, double *b, int r1, int r2, int n)
{
	double tmp, *p1, *p2;
	int i;

	if (r1 == r2) return;
	for (i = 0; i < n; i++) {
		p1 = mat_elem(a, r1, i, n);
		p2 = mat_elem(a, r2, i, n);
		tmp = *p1, *p1 = *p2, *p2 = tmp;
	}
	tmp = b[r1], b[r1] = b[r2], b[r2] = tmp;
}

void gauss_eliminate(double *a, double *b, double *x, int n)
{
#define A(y, x) (*mat_elem(a, y, x, n))
	int i, j, col, row, max_row,dia;
	double max, tmp;

	for (dia = 0; dia < n; dia++) {
		max_row = dia, max = A(dia, dia);

		for (row = dia + 1; row < n; row++)
			if ((tmp = fabs(A(row, dia))) > max)
				max_row = row, max = tmp;

		swap_row(a, b, dia, max_row, n);

		for (row = dia + 1; row < n; row++) {
			tmp = A(row, dia) / A(dia, dia);
			for (col = dia+1; col < n; col++)
				A(row, col) -= tmp * A(dia, col);
			A(row, dia) = 0;
			b[row] -= tmp * b[dia];
		}
	}
	for (row = n - 1; row >= 0; row--) {
		tmp = b[row];
		for (j = n - 1; j > row; j--)
			tmp -= x[j] * A(row, j);
		x[row] = tmp / A(row, row);
	}
#undef A
}

int main(void)
{
	double a[] = {
		1.00, 0.00, 0.00,  0.00,  0.00, 0.00,
		1.00, 0.63, 0.39,  0.25,  0.16, 0.10,
		1.00, 1.26, 1.58,  1.98,  2.49, 3.13,
		1.00, 1.88, 3.55,  6.70, 12.62, 23.80,
		1.00, 2.51, 6.32, 15.88, 39.90, 100.28,
		1.00, 3.14, 9.87, 31.01, 97.41, 306.02
	};
	double b[] = { -0.01, 0.61, 0.91, 0.99, 0.60, 0.02 };
	double x[6];
	int i;

	gauss_eliminate(a, b, x, 6);

	for (i = 0; i < 6; i++)
		printf("%g\n", x[i]);

	return 0;
}
```
```txt

-0.01
1.60279
-1.6132
1.24549
-0.49099
0.0657607

```




## Common Lisp


```CommonLisp

(defmacro mapcar-1 (fn n list)
 "Maps a function of two parameters where the first one is fixed, over a list"
  `(mapcar #'(lambda (l) (funcall ,fn ,n l)) ,list) )


(defun gauss (m)
  (labels
    ((redc (m) ; Reduce to triangular form
       (if (null (cdr m))
         m
        (cons (car m) (mapcar-1 #'cons 0 (redc (mapcar #'cdr (mapcar #'(lambda (r) (mapcar #'- (mapcar-1 #'* (caar m) r)
                                                                                            (mapcar-1 #'* (car r) (car m)))) (cdr m)))))) ))
     (rev (m) ; Reverse each row except the last element
       (reverse (mapcar #'(lambda (r) (append (reverse (butlast r)) (last r))) m)) ))
    (catch 'result
      (let ((m1 (redc (rev (redc m)))))
        (reverse (mapcar #'(lambda (r) (let ((pivot (find-if-not #'zerop r))) (if pivot (/ (car (last r)) pivot) (throw 'result 'singular)))) m1)) ))))

```


```txt

(setq m1 '((1.00 0.00 0.00  0.00  0.00   0.00   -0.01)
           (1.00 0.63 0.39  0.25  0.16   0.10    0.61)
           (1.00 1.26 1.58  1.98  2.49   3.13    0.91)
           (1.00 1.88 3.55  6.70 12.62  23.80    0.99)
           (1.00 2.51 6.32 15.88 39.90 100.28    0.60)
           (1.00 3.14 9.87 31.01 97.41 306.02    0.02) ))

(gauss m1)
=> (-0.009999999 1.6027923 -1.6132091 1.2455008 -0.4909925 0.06576109)

```


## C#

This modifies A and b in place, which might not be quite desirable.

```c#

using System;

namespace Rosetta
{
    internal class Vector
    {
        private double[] b;
        internal readonly int rows;

        internal Vector(int rows)
        {
            this.rows = rows;
            b = new double[rows];
        }

        internal Vector(double[] initArray)
        {
            b = (double[])initArray.Clone();
            rows = b.Length;
        }

        internal Vector Clone()
        {
            Vector v = new Vector(b);
            return v;
        }

        internal double this[int row]
        {
            get { return b[row]; }
            set { b[row] = value; }
        }

        internal void SwapRows(int r1, int r2)
        {
            if (r1 == r2) return;
            double tmp = b[r1];
            b[r1] = b[r2];
            b[r2] = tmp;
        }

        internal double norm(double[] weights)
        {
            double sum = 0;
            for (int i = 0; i < rows; i++)
            {
                double d = b[i] * weights[i];
                sum +=  d*d;
            }
            return Math.Sqrt(sum);
        }

        internal void print()
        {
            for (int i = 0; i < rows; i++)
                Console.WriteLine(b[i]);
            Console.WriteLine();
        }

        public static Vector operator-(Vector lhs, Vector rhs)
        {
            Vector v = new Vector(lhs.rows);
            for (int i = 0; i < lhs.rows; i++)
                v[i] = lhs[i] - rhs[i];
            return v;
        }
    }

    class Matrix
    {
        private double[] b;
        internal readonly int rows, cols;

        internal Matrix(int rows, int cols)
        {
            this.rows = rows;
            this.cols = cols;
            b = new double[rows * cols];
        }

        internal Matrix(int size)
        {
            this.rows = size;
            this.cols = size;
            b = new double[rows * cols];
            for (int i = 0; i < size; i++)
                this[i, i] = 1;
        }

        internal Matrix(int rows, int cols, double[] initArray)
        {
            this.rows = rows;
            this.cols = cols;
            b = (double[])initArray.Clone();
            if (b.Length != rows * cols) throw new Exception("bad init array");
        }

        internal double this[int row, int col]
        {
            get { return b[row * cols + col]; }
            set { b[row * cols + col] = value; }
        }

        public static Vector operator*(Matrix lhs, Vector rhs)
        {
            if (lhs.cols != rhs.rows) throw new Exception("I can't multiply matrix by vector");
            Vector v = new Vector(lhs.rows);
            for (int i = 0; i < lhs.rows; i++)
            {
                double sum = 0;
                for (int j = 0; j < rhs.rows; j++)
                    sum += lhs[i,j]*rhs[j];
                v[i] = sum;
            }
            return v;
        }

        internal void SwapRows(int r1, int r2)
        {
            if (r1 == r2) return;
            int firstR1 = r1 * cols;
            int firstR2 = r2 * cols;
            for (int i = 0; i < cols; i++)
            {
                double tmp = b[firstR1 + i];
                b[firstR1 + i] = b[firstR2 + i];
                b[firstR2 + i] = tmp;
            }
        }

        //with partial pivot
        internal void ElimPartial(Vector B)
        {
            for (int diag = 0; diag < rows; diag++)
            {
                int max_row = diag;
                double max_val = Math.Abs(this[diag, diag]);
                double d;
                for (int row = diag + 1; row < rows; row++)
                    if ((d = Math.Abs(this[row, diag])) > max_val)
                    {
                        max_row = row;
                        max_val = d;
                    }
                SwapRows(diag, max_row);
                B.SwapRows(diag, max_row);
                double invd = 1 / this[diag, diag];
                for (int col = diag; col < cols; col++)
                    this[diag, col] *= invd;
                B[diag] *= invd;
                for (int row = 0; row < rows; row++)
                {
                    d = this[row, diag];
                    if (row != diag)
                    {
                        for (int col = diag; col < cols; col++)
                            this[row, col] -= d * this[diag, col];
                        B[row] -= d * B[diag];
                    }
                }
            }
        }

        internal void print()
        {
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < cols; j++)
                    Console.Write(this[i,j].ToString()+"  ");
                Console.WriteLine();
            }
        }
    }
}

```


```c#

using System;

namespace Rosetta
{
    class Program
    {
        static void Main(string[] args)
        {
            Matrix A = new Matrix(6, 6,
            new double[] {1.1,0.12,0.13,0.12,0.14,-0.12,
            1.21,0.63,0.39,0.25,0.16,0.1,
            1.03,1.26,1.58,1.98,2.49,3.13,
            1.06,1.88,3.55,6.7,12.62,23.8,
            1.12,2.51,6.32,15.88,39.9,100.28,
            1.16,3.14,9.87,31.01,97.41,306.02});
            Vector B = new Vector(new double[] { -0.01, 0.61, 0.91, 0.99, 0.60, 0.02 });
            A.ElimPartial(B);
            B.print();
        }
    }
}

```


```txt
-0.0597391027501976
1.85018966726278
-1.97278330181163
1.4697587750651
-0.553874184782179
0.0723048745759396

```



## D

```d
import std.stdio, std.math, std.algorithm, std.range, std.numeric,
       std.typecons;

Tuple!(double[],"x", string,"err")
gaussPartial(in double[][] a0, in double[] b0) pure /*nothrow*/
in {
    assert(a0.length == a0[0].length);
    assert(a0.length == b0.length);
    assert(a0.all!(row => row.length == a0[0].length));
} body {
    enum eps = 1e-6;
    immutable m = b0.length;

    // Make augmented matrix.
    //auto a = a0.zip(b0).map!(c => c[0] ~ c[1]).array; // Not mutable.
    auto a = a0.zip(b0).map!(c => [] ~ c[0] ~ c[1]).array;

    // Wikipedia algorithm from Gaussian elimination page,
    // produces row-eschelon form.
    foreach (immutable k; 0 .. a.length) {
        // Find pivot for column k and swap.
        a[k .. m].minPos!((x, y) => x[k] > y[k]).front.swap(a[k]);
        if (a[k][k].abs < eps)
            return typeof(return)(null, "singular");

        // Do for all rows below pivot.
        foreach (immutable i; k + 1 .. m) {
            // Do for all remaining elements in current row.
            a[i][k+1 .. m+1] -= a[k][k+1 .. m+1] * (a[i][k] / a[k][k]);

            a[i][k] = 0; // Fill lower triangular matrix with zeros.
        }
    }

    // End of WP algorithm. Now back substitute to get result.
    auto x = new double[m];
    foreach_reverse (immutable i; 0 .. m)
        x[i] = (a[i][m] - a[i][i+1 .. m].dotProduct(x[i+1 .. m])) / a[i][i];

    return typeof(return)(x, null);
}

void main() {
    // The test case result is correct to this tolerance.
    enum eps = 1e-13;

    // Common RC example. Result computed with rational arithmetic
    // then converted to double, and so should be about as close to
    // correct as double represention allows.
    immutable a = [[1.00, 0.00, 0.00,  0.00,  0.00,   0.00],
                   [1.00, 0.63, 0.39,  0.25,  0.16,   0.10],
                   [1.00, 1.26, 1.58,  1.98,  2.49,   3.13],
                   [1.00, 1.88, 3.55,  6.70, 12.62,  23.80],
                   [1.00, 2.51, 6.32, 15.88, 39.90, 100.28],
                   [1.00, 3.14, 9.87, 31.01, 97.41, 306.02]];
    immutable b = [-0.01, 0.61, 0.91,  0.99,  0.60,   0.02];

    immutable r = gaussPartial(a, b);
    if (!r.err.empty)
        return writeln("Error: ", r.err);
    r.x.writeln;

    immutable result = [-0.01,               1.602790394502114,
                        -1.6132030599055613, 1.2454941213714368,
                        -0.4909897195846576, 0.065760696175232];
    foreach (immutable i, immutable xi; result)
        if (abs(r.x[i] - xi) > eps)
            return writeln("Out of tolerance: ", r.x[i], " ", xi);
}
```

```txt
[-0.01, 1.60279, -1.6132, 1.24549, -0.49099, 0.0657607]
```



## Delphi


```Delphi
program GuassianElimination;

// Modified from:
// R. Sureshkumar (10 January 1997)
// Gregory J. McRae (22 October 1997)
// http://web.mit.edu/10.001/Web/Course_Notes/Gauss_Pivoting.c

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

type
  TMatrix = class
     private
      _r, _c : integer;
      data : array of TDoubleArray;
      function    getValue(rIndex, cIndex : integer): double;
      procedure   setValue(rIndex, cIndex : integer; value: double);
     public
      constructor Create (r, c : integer);
      destructor  Destroy; override;

      property r : integer read _r;
      property c : integer read _c;
      property value[rIndex, cIndex: integer]: double read getValue write setValue; default;
  end;


constructor TMatrix.Create (r, c : integer);
begin
  inherited Create;
  self.r := r; self.c := c;
  setLength (data, r, c);
end;

destructor TMatrix.Destroy;
begin
  data := nil;
  inherited;
end;

function TMatrix.getValue(rIndex, cIndex: Integer): double;
begin
  Result := data[rIndex-1, cIndex-1]; // 1-based array
end;

procedure TMatrix.setValue(rIndex, cIndex : integer; value: double);
begin
  data[rIndex-1, cIndex-1] := value; // 1-based array
end;

// Solve A x = b
procedure gauss (A, b, x : TMatrix);
var rowx : integer;
    i, j, k, n, m : integer;
    amax, xfac, temp, temp1 : double;
begin
  rowx := 0;  // Keep count of the row interchanges
  n := A.r;
  for k := 1 to n - 1 do
      begin
      amax := abs (A[k,k]);
      m := k;
      // Find the row with largest pivot
      for i := k + 1 to n do
          begin
          xfac := abs (A[i,k]);
          if xfac > amax then
             begin
             amax := xfac;
             m := i;
             end;
          end;

      if m <> k then
         begin  // Row interchanges
         rowx := rowx+1;
         temp1 := b[k,1];
         b[k,1] := b[m,1];
         b[m,1]  := temp1;
         for j := k to n do
             begin
             temp := a[k,j];
             a[k,j] := a[m,j];
             a[m,j] := temp;
             end;
      end;

      for i := k+1 to n do
          begin
          xfac := a[i, k]/a[k, k];
          for j := k+1 to n do
              a[i,j] := a[i,j]-xfac*a[k,j];
          b[i,1] := b[i,1] - xfac*b[k,1]
          end;
      end;

  // Back substitution
  for j := 1 to n do
      begin
      k := n-j + 1;
      x[k,1] := b[k,1];
      for i := k+1 to n do
          begin
          x[k,1] := x[k,1] - a[k,i]*x[i,1];
          end;
  x[k,1] := x[k,1]/a[k,k];
  end;
end;


var A, b, x : TMatrix;

begin
  try
    // Could have been done with simple arrays rather than a specific TMatrix class
    A := TMatrix.Create (4,4);
    // Note ideal but use TMatrix to define the vectors as well
    b := TMatrix.Create (4,1);
    x := TMatrix.Create (4,1);

    A[1,1] := 2; A[1,2] := 1; A[1,3] := 0; A[1,4] := 0;
    A[2,1] := 1; A[2,2] := 1; A[2,3] := 1; A[2,4] := 0;
    A[3,1] := 0; A[3,2] := 1; A[3,3] := 2; A[3,4] := 1;
    A[4,1] := 0; A[3,2] := 0; A[4,3] := 1; A[4,4] := 2;

    b[1,1] := 2; b[2,1] := 1; b[3,1] := 4; b[4,1] := 8;

    gauss (A, b, x);

    writeln (x[1,1]:5:2);
    writeln (x[2,1]:5:2);
    writeln (x[3,1]:5:2);
    writeln (x[4,1]:5:2);

    readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


```

```txt
1.00, 0.00, 0.00, 4.00
```


=={{header|F_Sharp|F#}}==

### The Function


```fsharp

// Gaussian Elimination. Nigel Galloway: February 2nd., 2019
let gelim augM=
  let f=List.length augM
  let fG n (g:bigint list) t=n|>List.map(fun n->List.map2(fun n g->g-n)(List.map(fun n->n*g.[t])n)(List.map(fun g->g*n.[t])g))
  let rec fN i (g::e as l)=
    match i with i when i=f->l|>List.mapi(fun n (g:bigint list)->(g.[f],g.[n]))
                |_->fN (i+1) (fG e g i@[g])
  fN 0 augM

```


### The Task

This task uses functionality from [[Continued_fraction/Arithmetic/Construct_from_rational_number#F.23]] and [[Continued_fraction#F.23]]

```fsharp

let test=[[ -6I; -18I;  13I;   6I;  -6I; -15I;  -2I;  -9I;  -231I];
          [  2I;  20I;   9I;   2I;  16I; -12I; -18I;  -5I;   647I];
          [ 23I;  18I; -14I; -14I;  -1I;  16I;  25I; -17I;  -907I];
          [ -8I;  -1I; -19I;   4I;   3I; -14I;  23I;   8I;   248I];
          [ 25I;  20I;  -6I;  15I;   0I; -10I;   9I;  17I;  1316I];
          [-13I;  -1I;   3I;   5I;  -2I;  17I;  14I; -12I; -1080I];
          [ 19I;  24I; -21I;  -5I; -19I;   0I; -24I; -17I;  1006I];
          [ 20I;  -3I; -14I; -16I; -23I; -25I; -15I;  20I;  1496I]]
let fN (n,g)=cN2S(π(rI2cf n g))
gelim test |> List.map fN |> List.iteri(fun i n->(printfn "x[%d]=%1.14f " (i+1) (snd (Seq.pairwise n|> Seq.find(fun (n,g)->n-g < 0.0000000000001M)))))

```

```txt

x[1]=12.00000000000000
x[2]=10.00000000000000
x[3]=-20.00000000000000
x[4]=22.00000000000000
x[5]=-1.00000000000000
x[6]=-20.00000000000000
x[7]=-25.00000000000000
x[8]=23.00000000000000

```


## Fortran

Gaussian Elimination with partial pivoting using augmented matrix

```fortran

        program ge

          real, allocatable :: a(:,:),b(:)
          a = reshape(                             &
          [1.0, 1.00, 1.00,  1.00,   1.00,   1.00, &
           0.0, 0.63, 1.26,  1.88,   2.51,   3.14, &
           0.0, 0.39, 1.58,  3.55,   6.32,   9.87, &
           0.0, 0.25, 1.98,  6.70,  15.88,  31.01, &
           0.0, 0.16, 2.49, 12.62,  39.90,  97.41, &
           0.0, 0.10, 3.13, 23.80, 100.28, 306.02], [6,6] )
          b = [-0.01, 0.61, 0.91, 0.99, 0.60, 0.02]
          print'(f15.7)',solve_wbs(ge_wpp(a,b))

        contains

          function solve_wbs(u) result(x) ! solve with backward substitution
            real                 :: u(:,:)
            integer              :: i,n
            real   , allocatable :: x(:)
            n = size(u,1)
            allocate(x(n))
            forall (i=n:1:-1) x(i) = ( u(i,n+1) - sum(u(i,i+1:n)*x(i+1:n)) ) / u(i,i)
          end function

          function  ge_wpp(a,b) result(u) ! gaussian eliminate with partial pivoting
            real                 :: a(:,:),b(:),upi
            integer              :: i,j,n,p
            real   , allocatable :: u(:,:)
            n = size(a,1)
            u = reshape( [a,b], [n,n+1] )
            do j=1,n
              p = maxloc(abs(u(j:n,j)),1) + j-1 ! maxloc returns indices between (1,n-j+1)
              if (p /= j) u([p,j],j) = u([j,p],j)
              u(j+1:,j) = u(j+1:,j)/u(j,j)
              do i=j+1,n+1
                upi = u(p,i)
                if (p /= j) u([p,j],i) = u([j,p],i)
                u(j+1:n,i) = u(j+1:n,i) - upi*u(j+1:n,j)
              end do
            end do
          end function

        end program

```



## FreeBASIC

Gaussian elimination with pivoting.
FreeBASIC version 1.05

```FreeBASIC


Sub GaussJordan(matrix() As Double,rhs() As Double,ans() As Double)
    Dim As Long n=Ubound(matrix,1)
    Redim ans(0):Redim ans(1 To n)
    Dim As Double b(1 To n,1 To n),r(1 To n)
    For c As Long=1 To n 'take copies
        r(c)=rhs(c)
        For d As Long=1 To n
            b(c,d)=matrix(c,d)
        Next d
    Next c
    #macro pivot(num)
    For p1 As Long  = num To n - 1
        For p2 As Long  = p1 + 1 To n
            If Abs(b(p1,num))<Abs(b(p2,num)) Then
                Swap r(p1),r(p2)
                For g As Long=1 To n
                    Swap b(p1,g),b(p2,g)
                Next g
            End If
        Next p2
    Next p1
    #endmacro

    For k As Long=1 To n-1
        pivot(k)              'full pivoting
        For row As Long =k To n-1
            If b(row+1,k)=0 Then Exit For
            Var f=b(k,k)/b(row+1,k)
            r(row+1)=r(row+1)*f-r(k)
            For g As Long=1 To n
                b((row+1),g)=b((row+1),g)*f-b(k,g)
            Next g
        Next row
    Next k
    'back substitute
    For z As Long=n To 1 Step -1
        ans(z)=r(z)/b(z,z)
        For j As Long = n To z+1 Step -1
            ans(z)=ans(z)-(b(z,j)*ans(j)/b(z,z))
        Next j
    Next    z
End Sub

dim as double a(1 to 6,1 to 6) = { _
		                  {1.00, 0.00, 0.00,  0.00,  0.00, 0.00}, _
		                  {1.00, 0.63, 0.39,  0.25,  0.16, 0.10}, _
		                  {1.00, 1.26, 1.58,  1.98,  2.49, 3.13}, _
		                  {1.00, 1.88, 3.55,  6.70, 12.62, 23.80}, _
		                  {1.00, 2.51, 6.32, 15.88, 39.90, 100.28}, _
		                  {1.00, 3.14, 9.87, 31.01, 97.41, 306.02} _
	                      }

dim as double b(1 to 6) = { -0.01, 0.61, 0.91, 0.99, 0.60, 0.02 }

redim as double result()
GaussJordan(a(),b(),result())

for n as long=lbound(result) to ubound(result)
    print result(n)
next n
sleep

```

```txt

-0.01
 1.602790394502115
-1.613203059905572
 1.245494121371448
-0.490989719584662
 0.06576069617523256


```





## Go

===Partial pivoting, no scaling===
Gaussian elimination with partial pivoting by [https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode pseudocode] on WP page [https://en.wikipedia.org/wiki/Gaussian_elimination Gaussian elimination]."

```go
package main

import (
    "errors"
    "fmt"
    "log"
    "math"
)

type testCase struct {
    a [][]float64
    b []float64
    x []float64
}

var tc = testCase{
    // common RC example.  Result x computed with rational arithmetic then
    // converted to float64, and so should be about as close to correct as
    // float64 represention allows.
    a: [][]float64{
        {1.00, 0.00, 0.00, 0.00, 0.00, 0.00},
        {1.00, 0.63, 0.39, 0.25, 0.16, 0.10},
        {1.00, 1.26, 1.58, 1.98, 2.49, 3.13},
        {1.00, 1.88, 3.55, 6.70, 12.62, 23.80},
        {1.00, 2.51, 6.32, 15.88, 39.90, 100.28},
        {1.00, 3.14, 9.87, 31.01, 97.41, 306.02}},
    b: []float64{-0.01, 0.61, 0.91, 0.99, 0.60, 0.02},
    x: []float64{-0.01, 1.602790394502114, -1.6132030599055613,
        1.2454941213714368, -0.4909897195846576, 0.065760696175232},
}

// result from above test case turns out to be correct to this tolerance.
const ε = 1e-13

func main() {
    x, err := GaussPartial(tc.a, tc.b)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(x)
    for i, xi := range x {
        if math.Abs(tc.x[i]-xi) > ε {
            log.Println("out of tolerance")
            log.Fatal("expected", tc.x)
        }
    }
}

func GaussPartial(a0 [][]float64, b0 []float64) ([]float64, error) {
    // make augmented matrix
    m := len(b0)
    a := make([][]float64, m)
    for i, ai := range a0 {
        row := make([]float64, m+1)
        copy(row, ai)
        row[m] = b0[i]
        a[i] = row
    }
    // WP algorithm from Gaussian elimination page
    // produces row-eschelon form
    for k := range a {
        // Find pivot for column k:
        iMax := k
        max := math.Abs(a[k][k])
        for i := k + 1; i < m; i++ {
            if abs := math.Abs(a[i][k]); abs > max {
                iMax = i
                max = abs
            }
        }
        if a[iMax][k] == 0 {
            return nil, errors.New("singular")
        }
        // swap rows(k, i_max)
        a[k], a[iMax] = a[iMax], a[k]
        // Do for all rows below pivot:
        for i := k + 1; i < m; i++ {
            // Do for all remaining elements in current row:
            for j := k + 1; j <= m; j++ {
                a[i][j] -= a[k][j] * (a[i][k] / a[k][k])
            }
            // Fill lower triangular matrix with zeros:
            a[i][k] = 0
        }
    }
    // end of WP algorithm.
    // now back substitute to get result.
    x := make([]float64, m)
    for i := m - 1; i >= 0; i-- {
        x[i] = a[i][m]
        for j := i + 1; j < m; j++ {
            x[i] -= a[i][j] * x[j]
        }
        x[i] /= a[i][i]
    }
    return x, nil
}
```

```txt

[-0.01 1.6027903945020987 -1.613203059905494 1.245494121371364 -0.49098971958462834 0.06576069617522803]

```


### Scaled partial pivoting

Changes from above version noted with comments.  For the example data scaling does help a bit.

```go
package main

import (
    "errors"
    "fmt"
    "log"
    "math"
)

type testCase struct {
    a [][]float64
    b []float64
    x []float64
}

var tc = testCase{
    a: [][]float64{
        {1.00, 0.00, 0.00, 0.00, 0.00, 0.00},
        {1.00, 0.63, 0.39, 0.25, 0.16, 0.10},
        {1.00, 1.26, 1.58, 1.98, 2.49, 3.13},
        {1.00, 1.88, 3.55, 6.70, 12.62, 23.80},
        {1.00, 2.51, 6.32, 15.88, 39.90, 100.28},
        {1.00, 3.14, 9.87, 31.01, 97.41, 306.02}},
    b: []float64{-0.01, 0.61, 0.91, 0.99, 0.60, 0.02},
    x: []float64{-0.01, 1.602790394502114, -1.6132030599055613,
        1.2454941213714368, -0.4909897195846576, 0.065760696175232},
}

// result from above test case turns out to be correct to this tolerance.
const ε = 1e-14

func main() {
    x, err := GaussPartial(tc.a, tc.b)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(x)
    for i, xi := range x {
        if math.Abs(tc.x[i]-xi) > ε {
            log.Println("out of tolerance")
            log.Fatal("expected", tc.x)
        }
    }
}

func GaussPartial(a0 [][]float64, b0 []float64) ([]float64, error) {
    m := len(b0)
    a := make([][]float64, m)
    for i, ai := range a0 {
        row := make([]float64, m+1)
        copy(row, ai)
        row[m] = b0[i]
        a[i] = row
    }
    for k := range a {
        iMax := 0
        max := -1.
        for i := k; i < m; i++ {
            row := a[i]
            // compute scale factor s = max abs in row
            s := -1.
            for j := k; j < m; j++ {
                x := math.Abs(row[j])
                if x > s {
                    s = x
                }
            }
            // scale the abs used to pick the pivot.
            if abs := math.Abs(row[k]) / s; abs > max {
                iMax = i
                max = abs
            }
        }
        if a[iMax][k] == 0 {
            return nil, errors.New("singular")
        }
        a[k], a[iMax] = a[iMax], a[k]
        for i := k + 1; i < m; i++ {
            for j := k + 1; j <= m; j++ {
                a[i][j] -= a[k][j] * (a[i][k] / a[k][k])
            }
            a[i][k] = 0
        }
    }
    x := make([]float64, m)
    for i := m - 1; i >= 0; i-- {
        x[i] = a[i][m]
        for j := i + 1; j < m; j++ {
            x[i] -= a[i][j] * x[j]
        }
        x[i] /= a[i][i]
    }
    return x, nil
}
```

```txt

[-0.01 1.6027903945021131 -1.6132030599055596 1.245494121371436 -0.49098971958465754 0.065760696175232]

```



## Haskell


### Version 1

We use Rational numbers for having more precision. a % b is the rational a / b.

```Haskell

foldlZipWith::(a -> b -> c) -> (d -> c -> d) -> d -> [a] -> [b]  -> d
foldlZipWith _ _ u [] _          = u
foldlZipWith _ _ u _ []          = u
foldlZipWith f g u (x:xs) (y:ys) = foldlZipWith f g (g u (f x y)) xs ys

foldl1ZipWith::(a -> b -> c) -> (c -> c -> c) -> [a] -> [b] -> c
foldl1ZipWith _ _ [] _          = error "First list is empty"
foldl1ZipWith _ _ _ []          = error "Second list is empty"
foldl1ZipWith f g (x:xs) (y:ys) = foldlZipWith f g (f x y) xs ys

multAdd::(a -> b -> c) -> (c -> c -> c) -> [[a]] -> [[b]] -> [[c]]
multAdd f g xs ys = map (\us -> foldl1ZipWith (\u vs -> map (f u) vs) (zipWith g) us ys) xs

mult:: Num a => [[a]] -> [[a]] -> [[a]]
mult xs ys = multAdd (*) (+) xs ys

bubble::([a] -> c) -> (c -> c -> Bool) -> [[a]] -> [[b]] -> ([[a]],[[b]])
bubble _ _ [] ts         = ([],ts)
bubble _ _ rs []         = (rs,[])
bubble f g (r:rs) (t:ts) = bub r t (f r) rs ts [] []
  where
  bub l k _ [] _ xs ys          = (l:xs,k:ys)
  bub l k _ _ [] xs ys          = (l:xs,k:ys)
  bub l k m (u:us) (v:vs) xs ys = ans
    where
    mu = f u
    ans | g m mu    = bub l k m us vs (u:xs) (v:ys)
        | otherwise = bub u v mu us vs (l:xs) (k:ys)

pivot::Num a => [a] -> [a] -> [[a]] -> [[a]] -> ([[a]],[[a]])
pivot xs ks ys ls = go ys ls [] []
  where
  x              = head xs
  fun r          = zipWith (\u v ->  u*r - v*x)
  val rs ts      = let f = fun (head rs) in (tail $ f xs rs,f ks ts)
  go [] _ us vs  = (us,vs)
  go _ [] us vs  = (us,vs)
  go rs ts us vs = go (tail rs) (tail ts) (es:us) (fs:vs)
    where (es,fs) = val (head rs) (head ts)

triangle::(Num a,Ord a) => [[a]] -> [[a]] -> ([[a]],[[a]])
triangle as bs = go (as,bs) [] []
  where
  go ([],_) us vs  = (us,vs)
  go (_,[]) us vs  = (us,vs)
  go (rs,ts) us vs = ans
    where
    (xs:ys,ks:ls) = bubble (abs.head) (>=) rs ts
    ans = go (pivot xs ks ys ls) (xs:us) (ks:vs)

solveTriangle::(Fractional a,Eq a) => [[a]] -> [[a]] -> [[a]]
solveTriangle [] _ = []
solveTriangle _ [] = []
solveTriangle as _ | not.null.dropWhile ((/= 0).head) $ as = []
solveTriangle ([c]:as) (b:bs) = go as bs [map (/c) b]
  where
  val us vs ws = let u = head us in map (/u) $ zipWith (-) vs (head $ mult [tail us] ws)
  go [] _ zs          = zs
  go _ [] zs          = zs
  go (x:xs) (y:ys) zs = go xs ys $ (val x y zs):zs

solveGauss:: (Fractional a, Ord a) => [[a]] -> [[a]] -> [[a]]
solveGauss as bs = uncurry solveTriangle $ triangle as bs

matI::(Num a) => Int -> [[a]]
matI n = [ [fromIntegral.fromEnum $ i == j | j <- [1..n]] | i <- [1..n]]

task::[[Rational]] -> [[Rational]] -> IO()
task a b = do
  let x         = solveGauss a b
  let u         = map (map fromRational) x
  let y         = mult a x
  let identity  = matI (length x)
  let a1        = solveGauss a identity
  let h         = mult a a1
  let z         = mult a1 b
  putStrLn "a ="
  mapM_ print a
  putStrLn "b ="
  mapM_ print b
  putStrLn "solve: a * x = b => x = solveGauss a b ="
  mapM_ print x
  putStrLn "u = fromRationaltoDouble x ="
  mapM_ print u
  putStrLn "verification: y = a * x = mult a x ="
  mapM_ print y
  putStrLn $ "test: y == b = "
  print $ y == b
  putStrLn "identity matrix: identity ="
  mapM_ print identity
  putStrLn "find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveGauss a identity ="
  mapM_ print a1
  putStrLn "verification: h = a * a1 = mult a a1 ="
  mapM_ print h
  putStrLn $ "test: h == identity = "
  print $ h == identity
  putStrLn "z = a1 * b = mult a1 b ="
  mapM_ print z
  putStrLn "test: z == x ="
  print $ z == x

main = do
  let a  = [[1.00, 0.00, 0.00,  0.00,  0.00,   0.00],
            [1.00, 0.63, 0.39,  0.25,  0.16,   0.10],
            [1.00, 1.26, 1.58,  1.98,  2.49,   3.13],
            [1.00, 1.88, 3.55,  6.70, 12.62,  23.80],
            [1.00, 2.51, 6.32, 15.88, 39.90, 100.28],
            [1.00, 3.14, 9.87, 31.01, 97.41, 306.02]]
  let b = [[-0.01], [0.61], [0.91], [0.99], [0.60], [0.02]]
  task a b

```

```txt

a =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[1 % 1,63 % 100,39 % 100,1 % 4,4 % 25,1 % 10]
[1 % 1,63 % 50,79 % 50,99 % 50,249 % 100,313 % 100]
[1 % 1,47 % 25,71 % 20,67 % 10,631 % 50,119 % 5]
[1 % 1,251 % 100,158 % 25,397 % 25,399 % 10,2507 % 25]
[1 % 1,157 % 50,987 % 100,3101 % 100,9741 % 100,15301 % 50]
b =
[(-1) % 100]
[61 % 100]
[91 % 100]
[99 % 100]
[3 % 5]
[1 % 50]
solve: a * x = b => x = solveGauss a b =
[(-1) % 100]
[655870882787 % 409205648497]
[(-660131804286) % 409205648497]
[509663229635 % 409205648497]
[(-200915766608) % 409205648497]
[26909648324 % 409205648497]
u = fromRationaltoDouble x =
[-1.0e-2]
[1.602790394502114]
[-1.6132030599055613]
[1.2454941213714368]
[-0.4909897195846576]
[6.5760696175232e-2]
verification: y = a * x = mult a x =
[(-1) % 100]
[61 % 100]
[91 % 100]
[99 % 100]
[3 % 5]
[1 % 50]
test: y == b =
True
identity matrix: identity =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,1 % 1]
find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveGauss a identity =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[(-1373267314900) % 409205648497,2792895413400 % 409205648497,(-2539722499600) % 409205648497,1620086418000 % 409205648497,(-593562467900) % 409205648497,93570451000 % 409205648497]
[1683936576500 % 409205648497,(-5515373801600) % 409205648497,7425272193600 % 409205648497,(-5318952383900) % 409205648497,2060945510400 % 409205648497,(-335828095000) % 409205648497]
[(-955389934100) % 409205648497,3910562856500 % 409205648497,(-6532196158200) % 409205648497,5493636552500 % 409205648497,(-2312764532500) % 409205648497,396151215800 % 409205648497]
[253880215500 % 409205648497,(-1187959549100) % 409205648497,2281116328400 % 409205648497,(-2180688584400) % 409205648497,1021846842100 % 409205648497,(-188195252500) % 409205648497]
[(-25558559000) % 409205648497,131101344100 % 409205648497,(-277605537500) % 409205648497,292380217600 % 409205648497,(-151287558900) % 409205648497,30970093700 % 409205648497]
verification: h = a * a1 = mult a a1 =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,1 % 1]
test: h == identity =
True
z = a1 * b = mult a1 b =
[(-1) % 100]
[655870882787 % 409205648497]
[(-660131804286) % 409205648497]
[509663229635 % 409205648497]
[(-200915766608) % 409205648497]
[26909648324 % 409205648497]
test: z == x =
True

```


### Determinant and permutation matrix are given


```Haskell

foldlZipWith::(a -> b -> c) -> (d -> c -> d) -> d -> [a] -> [b]  -> d
foldlZipWith _ _ u [] _          = u
foldlZipWith _ _ u _ []          = u
foldlZipWith f g u (x:xs) (y:ys) = foldlZipWith f g (g u (f x y)) xs ys

foldl1ZipWith::(a -> b -> c) -> (c -> c -> c) -> [a] -> [b] -> c
foldl1ZipWith _ _ [] _          = error "First list is empty"
foldl1ZipWith _ _ _ []          = error "Second list is empty"
foldl1ZipWith f g (x:xs) (y:ys) = foldlZipWith f g (f x y) xs ys

multAdd::(a -> b -> c) -> (c -> c -> c) -> [[a]] -> [[b]] -> [[c]]
multAdd f g xs ys = map (\us -> foldl1ZipWith (\u vs -> map (f u) vs) (zipWith g) us ys) xs

mult:: Num a => [[a]] -> [[a]] -> [[a]]
mult xs ys = multAdd (*) (+) xs ys

triangle::(Fractional a, Ord a) => [[a]] -> [[a]] -> (a,[(([a],[a]),Int)])
triangle as bs = pivot 1 [] $ zipWith3 (\x y i -> ((x,y),i)) as bs [(0::Int)..]
  where
  good rs ts = (abs.head.fst.fst $ ts) <= (abs.head.fst.fst $ rs)
  go (us,vs) ((os,ps),i) = if o == 0 then ((rs,f vs ps),i) else ((f us rs,f vs ps),i)
    where
    (o,rs) = (head os,tail os)
    f = zipWith (\x y -> y - x*o)
  change i (ys:zs) = map (\xs -> if (==i).snd $ xs then ys else xs) zs
  pivot d ls [] = (d,ls)
  pivot d ls zs@((_,j):ys) = if u == 0 then (0,ls) else pivot e (ps:ls) ws
    where
    e  = if i == j then u*d else -u*d
    ws = map (go (map (/u) us,map (/u) vs)) $ if i == j then ys else change i zs
    ps@((u:us,vs),i) = foldl1 (\rs ts ->  if good rs ts then rs else ts) zs

-- ((det,sol),permutation) = gauss as bs
-- det = determinant as
-- sol is solution of: as * sol = bs
-- perm is a permutation with: (matPerm perm) * as * sol = (matPerm perm) * bs
gauss::(Fractional a,Ord a) => [[a]] -> [[a]] -> ((a,[[a]]),[Int])
gauss as bs = if 0 == det then ((0,[]),[]) else solveTriangle ms
  where
  (det,ms) = triangle as bs
  solveTriangle ((([c],b),i):sys) = go sys [map (/c) b] [i]
    where
    val us vs ws = let u = head us in map (/u) $ zipWith (-) vs (head $ mult [tail us] ws)
    go [] zs is        = ((det,zs),is)
    go (((x,y),i):sys) zs is = go sys ((val x y zs):zs) (i:is)

solveGauss::(Fractional a,Ord a) => [[a]] -> [[a]] -> [[a]]
solveGauss as = snd.fst.gauss as

matI::Num a => Int -> [[a]]
matI n = [ [fromIntegral.fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

matPerm::Num a => [Int] -> [[a]]
matPerm ns = [ [fromIntegral.fromEnum $ i == j | (j,_) <- zip [0..] ns] | i <- ns]

task::[[Rational]] -> [[Rational]] -> IO()
task a b = do
  let ((d,x),perm)   = gauss a b
  let ps             = matPerm perm
  let u              = map (map fromRational) x
  let y              = mult a x
  let identity       = matI (length x)
  let a1             = solveGauss a identity
  let h              = mult a a1
  let z              = mult a1 b
  putStrLn "d = determinant a ="
  print d
  putStrLn "a ="
  mapM_ print a
  putStrLn "b ="
  mapM_ print b
  putStrLn "solve: a * x = b => x = solveGauss a b ="
  mapM_ print x
  putStrLn "u = fromRationaltoDouble x ="
  mapM_ print u
  putStrLn "verification: y = a * x = mult a x ="
  mapM_ print y
  putStrLn $ "test: y == b = "
  print $ y == b
  putStrLn "ps is the permutation associated to matrix a and ps ="
  mapM_ print ps
  putStrLn "identity matrix: identity ="
  mapM_ print identity
  putStrLn "find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveGauss a identity ="
  mapM_ print a1
  putStrLn "verification: h = a * a1 = mult a a1 ="
  mapM_ print h
  putStrLn $ "test: h == identity = "
  print $ h == identity
  putStrLn "z = a1 * b = mult a1 b ="
  mapM_ print z
  putStrLn "test: z == x ="
  print $ z == x

main = do
  let a  = [[1.00, 0.00, 0.00,  0.00,  0.00,   0.00],
            [1.00, 0.63, 0.39,  0.25,  0.16,   0.10],
            [1.00, 1.26, 1.58,  1.98,  2.49,   3.13],
            [1.00, 1.88, 3.55,  6.70, 12.62,  23.80],
            [1.00, 2.51, 6.32, 15.88, 39.90, 100.28],
            [1.00, 3.14, 9.87, 31.01, 97.41, 306.02]]
  let b = [[-0.01], [0.61], [0.91],  [0.99],  [0.60], [0.02]]
  task a b

```

```txt

d = determinant a =
409205648497 % 10000000000
a =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[1 % 1,63 % 100,39 % 100,1 % 4,4 % 25,1 % 10]
[1 % 1,63 % 50,79 % 50,99 % 50,249 % 100,313 % 100]
[1 % 1,47 % 25,71 % 20,67 % 10,631 % 50,119 % 5]
[1 % 1,251 % 100,158 % 25,397 % 25,399 % 10,2507 % 25]
[1 % 1,157 % 50,987 % 100,3101 % 100,9741 % 100,15301 % 50]
b =
[(-1) % 100]
[61 % 100]
[91 % 100]
[99 % 100]
[3 % 5]
[1 % 50]
solve: a * x = b => x = solveGauss a b =
[(-1) % 100]
[655870882787 % 409205648497]
[(-660131804286) % 409205648497]
[509663229635 % 409205648497]
[(-200915766608) % 409205648497]
[26909648324 % 409205648497]
u = fromRationaltoDouble x =
[-1.0e-2]
[1.602790394502114]
[-1.6132030599055613]
[1.2454941213714368]
[-0.4909897195846576]
[6.5760696175232e-2]
verification: y = a * x = mult a x =
[(-1) % 100]
[61 % 100]
[91 % 100]
[99 % 100]
[3 % 5]
[1 % 50]
test: y == b =
True
ps is the permutation associated to matrix a and ps =
[1,0,0,0,0,0]
[0,0,0,0,0,1]
[0,0,1,0,0,0]
[0,0,0,0,1,0]
[0,1,0,0,0,0]
[0,0,0,1,0,0]
identity matrix: identity =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,1 % 1]
find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveGauss a identity =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[(-1373267314900) % 409205648497,2792895413400 % 409205648497,(-2539722499600) % 409205648497,1620086418000 % 409205648497,(-593562467900) % 409205648497,93570451000 % 409205648497]
[1683936576500 % 409205648497,(-5515373801600) % 409205648497,7425272193600 % 409205648497,(-5318952383900) % 409205648497,2060945510400 % 409205648497,(-335828095000) % 409205648497]
[(-955389934100) % 409205648497,3910562856500 % 409205648497,(-6532196158200) % 409205648497,5493636552500 % 409205648497,(-2312764532500) % 409205648497,396151215800 % 409205648497]
[253880215500 % 409205648497,(-1187959549100) % 409205648497,2281116328400 % 409205648497,(-2180688584400) % 409205648497,1021846842100 % 409205648497,(-188195252500) % 409205648497]
[(-25558559000) % 409205648497,131101344100 % 409205648497,(-277605537500) % 409205648497,292380217600 % 409205648497,(-151287558900) % 409205648497,30970093700 % 409205648497]
verification: h = a * a1 = mult a a1 =
[1 % 1,0 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,0 % 1,0 % 1,1 % 1]
test: h == identity =
True
z = a1 * b = mult a1 b =
[(-1) % 100]
[655870882787 % 409205648497]
[(-660131804286) % 409205648497]
[509663229635 % 409205648497]
[(-200915766608) % 409205648497]
[26909648324 % 409205648497]
test: z == x =
True

```



## J

%. , J's matrix divide verb, directly solves systems of determined and of over-determined linear equations directly.  This example J session builds a noisy sine curve on the half circle, fits quintic and quadratic equations, and displays the results of evaluating these polynomials.


```J

   f=: 6j2&":   NB. formatting verb

   sin=: 1&o.   NB. verb to evaluate circle function 1, the sine

   add_noise=: ] + (* (_0.5 + 0 ?@:#~ #))   NB. AMPLITUDE add_noise SIGNAL

   f RADIANS=: o.@:(%~ i.@:>:)5  NB. monadic circle function is  pi times
  0.00  0.63  1.26  1.88  2.51  3.14

   f SINES=: sin RADIANS
  0.00  0.59  0.95  0.95  0.59  0.00

   f NOISY_SINES=: 0.1 add_noise SINES
 _0.01  0.61  0.91  0.99  0.60  0.02

   A=: (^/ i.@:#) RADIANS  NB. A is the quintic coefficient matrix

   NB. display the equation to solve
   (f A) ; 'x' ; '=' ; f@:,. NOISY_SINES
┌────────────────────────────────────┬─┬─┬──────┐
│  1.00  0.00  0.00  0.00  0.00  0.00│x│=│ _0.01│
│  1.00  0.63  0.39  0.25  0.16  0.10│ │ │  0.61│
│  1.00  1.26  1.58  1.98  2.49  3.13│ │ │  0.91│
│  1.00  1.88  3.55  6.70 12.62 23.80│ │ │  0.99│
│  1.00  2.51  6.32 15.88 39.90100.28│ │ │  0.60│
│  1.00  3.14  9.87 31.01 97.41306.02│ │ │  0.02│
└────────────────────────────────────┴─┴─┴──────┘

   f QUINTIC_COEFFICIENTS=: NOISY_SINES %. A   NB. %. solves the linear system
 _0.01  1.71 _1.88  1.48 _0.58  0.08

   quintic=: QUINTIC_COEFFICIENTS&p.  NB. verb to evaluate the polynomial

   NB. %. also solves the least squares fit for overdetermined system
   quadratic=: (NOISY_SINES %. (^/ i.@:3:) RADIANS)&p.  NB. verb to evaluate quadratic.
   quadratic
_0.0200630695393961729 1.26066877804926536 _0.398275112136019516&p.

   NB. The quintic is agrees with the noisy data, as it should
   f@:(NOISY_SINES ,. sin ,. quadratic ,. quintic) RADIANS
 _0.01  0.00 _0.02 _0.01
  0.61  0.59  0.61  0.61
  0.91  0.95  0.94  0.91
  0.99  0.95  0.94  0.99
  0.60  0.59  0.63  0.60
  0.02  0.00  0.01  0.02

   f MID_POINTS=: (+ -:@:(-/@:(2&{.)))RADIANS
 _0.31  0.31  0.94  1.57  2.20  2.83

   f@:(sin ,. quadratic ,. quintic) MID_POINTS
 _0.31 _0.46 _0.79
  0.31  0.34  0.38
  0.81  0.81  0.77
  1.00  0.98  1.00
  0.81  0.83  0.86
  0.31  0.36  0.27

```



## JavaScript

From Numerical Recipes in C:

```javascript
// Lower Upper Solver
function lusolve(A, b, update) {
	var lu = ludcmp(A, update)
	if (lu === undefined) return // Singular Matrix!
	return lubksb(lu, b, update)
}

// Lower Upper Decomposition
function ludcmp(A, update) {
	// A is a matrix that we want to decompose into Lower and Upper matrices.
	var d = true
	var n = A.length
	var idx = new Array(n) // Output vector with row permutations from partial pivoting
	var vv = new Array(n)  // Scaling information

	for (var i=0; i<n; i++) {
		var max = 0
		for (var j=0; j<n; j++) {
			var temp = Math.abs(A[i][j])
			if (temp > max) max = temp
		}
		if (max == 0) return // Singular Matrix!
		vv[i] = 1 / max // Scaling
	}

	if (!update) { // make a copy of A
		var Acpy = new Array(n)
		for (var i=0; i<n; i++) {
			var Ai = A[i]
			Acpyi = new Array(Ai.length)
			for (j=0; j<Ai.length; j+=1) Acpyi[j] = Ai[j]
			Acpy[i] = Acpyi
		}
		A = Acpy
	}

	var tiny = 1e-20 // in case pivot element is zero
	for (var i=0; ; i++) {
		for (var j=0; j<i; j++) {
			var sum = A[j][i]
			for (var k=0; k<j; k++) sum -= A[j][k] * A[k][i];
			A[j][i] = sum
		}
		var jmax = 0
		var max = 0;
		for (var j=i; j<n; j++) {
			var sum = A[j][i]
			for (var k=0; k<i; k++) sum -= A[j][k] * A[k][i];
			A[j][i] = sum
			var temp = vv[j] * Math.abs(sum)
			if (temp >= max) {
				max = temp
				jmax = j
			}
		}
		if (i <= jmax) {
			for (var j=0; j<n; j++) {
				var temp = A[jmax][j]
				A[jmax][j] = A[i][j]
				A[i][j] = temp
			}
			d = !d;
			vv[jmax] = vv[i]
		}
		idx[i] = jmax;
		if (i == n-1) break;
		var temp = A[i][i]
		if (temp == 0) A[i][i] = temp = tiny
		temp = 1 / temp
		for (var j=i+1; j<n; j++) A[j][i] *= temp
	}
	return {A:A, idx:idx, d:d}
}

// Lower Upper Back Substitution
function lubksb(lu, b, update) {
	// solves the set of n linear equations A*x = b.
	// lu is the object containing A, idx and d as determined by the routine ludcmp.
	var A = lu.A
	var idx = lu.idx
	var n = idx.length

	if (!update) { // make a copy of b
		var bcpy = new Array(n)
		for (var i=0; i<b.length; i+=1) bcpy[i] = b[i]
		b = bcpy
	}

	for (var ii=-1, i=0; i<n; i++) {
		var ix = idx[i]
		var sum = b[ix]
		b[ix] = b[i]
		if (ii > -1)
			for (var j=ii; j<i; j++) sum -= A[i][j] * b[j]
		else if (sum)
			ii = i
		b[i] = sum
	}
	for (var i=n-1; i>=0; i--) {
		var sum = b[i]
		for (var j=i+1; j<n; j++) sum -= A[i][j] * b[j]
		b[i] = sum / A[i][i]
	}
	return b // solution vector x
}

document.write(
	lusolve(
		[
			[1.00, 0.00, 0.00,  0.00,  0.00,   0.00],
                	[1.00, 0.63, 0.39,  0.25,  0.16,   0.10],
                	[1.00, 1.26, 1.58,  1.98,  2.49,   3.13],
                	[1.00, 1.88, 3.55,  6.70, 12.62,  23.80],
                	[1.00, 2.51, 6.32, 15.88, 39.90, 100.28],
                	[1.00, 3.14, 9.87, 31.01, 97.41, 306.02]
		],
    		[-0.01, 0.61, 0.91,  0.99,  0.60,   0.02]
	)
)
```


```txt
-0.01000000000000004, 1.6027903945021095, -1.6132030599055475, 1.2454941213714232, -0.4909897195846526, 0.06576069617523138
```



## Julia

Using built-in LAPACK-based linear solver (which employs partial-pivoted Gaussian elimination):

```julia
x = A \ b
```



## Klong


```K

elim::{[h m];h::*m::x@>*'x;
       :[2>#x;x;(,h),0,:\.f({1_x}'{x-h**x%*h}'1_m)]}
subst::{[v];v::[];
        {v::v,((*x)-/:[[]~v;[];v*x@1+!#v])%x@1+#v}'||'x;|v}
gauss::{subst(elim(x))}

```


Example, matrix taken from C version:


```K

    gauss([[1.00 0.00 0.00  0.00  0.00   0.00 -0.01]
           [1.00 0.63 0.39  0.25  0.16   0.10  0.61]
           [1.00 1.26 1.58  1.98  2.49   3.13  0.91]
           [1.00 1.88 3.55  6.70 12.62  23.80  0.99]
           [1.00 2.51 6.32 15.88 39.90 100.28  0.60]
           [1.00 3.14 9.87 31.01 97.41 306.02  0.02]]
[-0.00999999999999981
 1.60279039450211414
 -1.6132030599055625
 1.24549412137143782
 -0.490989719584658025
 0.0657606961752320591]

```



## Kotlin

```scala
// version 1.1.51

val ta = arrayOf(
    doubleArrayOf(1.00, 0.00, 0.00, 0.00, 0.00, 0.00),
    doubleArrayOf(1.00, 0.63, 0.39, 0.25, 0.16, 0.10),
    doubleArrayOf(1.00, 1.26, 1.58, 1.98, 2.49, 3.13),
    doubleArrayOf(1.00, 1.88, 3.55, 6.70, 12.62, 23.80),
    doubleArrayOf(1.00, 2.51, 6.32, 15.88, 39.90, 100.28),
    doubleArrayOf(1.00, 3.14, 9.87, 31.01, 97.41, 306.02)
)

val tb = doubleArrayOf(-0.01, 0.61, 0.91, 0.99, 0.60, 0.02)

val tx = doubleArrayOf(
    -0.01, 1.602790394502114, -1.6132030599055613,
    1.2454941213714368, -0.4909897195846576, 0.065760696175232
)

const val EPSILON = 1e-14  // tolerance required

fun gaussPartial(a0: Array<DoubleArray>, b0: DoubleArray): DoubleArray {
    val m = b0.size
    val a = Array(m) { DoubleArray(m) }
    for ((i, ai) in a0.withIndex()) {
        val row = ai.copyOf(m + 1)
        row[m] = b0[i]
        a[i] = row
    }
    for (k in 0 until a.size) {
        var iMax = 0
        var max = -1.0
        for (i in k until m) {
            val row = a[i]
            // compute scale factor s = max abs in row
            var s = -1.0
            for (j in k until m) {
                val e = Math.abs(row[j])
                if (e > s) s = e
            }
            // scale the abs used to pick the pivot
            val abs = Math.abs(row[k]) / s
            if (abs > max) {
                iMax = i
                max = abs
            }
        }
        if (a[iMax][k] == 0.0) {
            throw RuntimeException("Matrix is singular.")
        }
        val tmp = a[k]
        a[k] = a[iMax]
        a[iMax] = tmp
        for (i in k + 1 until m) {
            for (j in k + 1..m) {
                a[i][j] -= a[k][j] * a[i][k] / a[k][k]
            }
            a[i][k] = 0.0
        }
    }
    val x = DoubleArray(m)
    for (i in m - 1 downTo 0) {
        x[i] = a[i][m]
        for (j in i + 1 until m) {
            x[i] -= a[i][j] * x[j]
        }
        x[i] /= a[i][i]
    }
    return x
}

fun main(args: Array<String>) {
    val x = gaussPartial(ta, tb)
    println(x.asList())
    for ((i, xi) in x.withIndex()) {
        if (Math.abs(tx[i] - xi) > EPSILON) {
            println("Out of tolerance.")
            println("Expected values are ${tx.asList()}")
            return
        }
    }
}
```


```txt

[-0.01, 1.6027903945021138, -1.6132030599055616, 1.2454941213714392, -0.49098971958465953, 0.06576069617523238]

```


## M2000 Interpreter

Faster, with accuracy of 25 decimals

```M2000 Interpreter

module checkit {
      Dim Base 1, a(6, 6), b(6)
      a(1,1)= 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.63, 0.39, 0.25, 0.16, 0.10, 1.00, 1.26, 1.58, 1.98, 2.49, 3.13, 1.00, 1.88, 3.55, 6.70, 12.62, 23.80, 1.00, 2.51, 6.32, 15.88, 39.90, 100.28, 1.00, 3.14, 9.87, 31.01, 97.41, 306.02
      \\ remove \\ to feed next array
      \\ a(1,1)=1.1,0.12,0.13,0.12,0.14,-0.12,1.21,0.63,0.39,0.25,0.16,0.1,1.03,1.26,1.58,1.98,2.49,3.13, 1.06,1.88,3.55,6.7,12.62,23.8, 1.12,2.51,6.32,15.88,39.9,100.28,1.16,3.14,9.87,31.01,97.41,306.02
      for i=1 to 6 : for j=1 to 6 : a(i,j)=val(a(i,j)->Decimal) :Next j:Next i
      b(1)=-0.01, 0.61, 0.91, 0.99, 0.60, 0.02
      for i=1 to 6 : b(i)=val(b(i)->Decimal) :Next i
      function GaussJordan(a(), b()) {
            cols=dimension(a(),1)
            rows=dimension(a(),2)
            \\ make augmented matrix
            Dim Base 1, a(cols, rows)
            \\ feed array with rationals
            Dim Base 1, b(Len(b()))
            for diag=1 to rows {
                        max_row=diag
                        max_val=abs(a(diag, diag))
                        if diag<rows Then {
                              for ro=diag+1 to rows {
                                    d=abs(a(ro, diag))
                                    if d>max_val then max_row=ro : max_val=d
                              }
                        }
            \\         SwapRows diag, max_row
                        if diag<>max_row then {
                              for i=1 to cols {
                                    swap a(diag, i), a(max_row, i)
                              }
                              swap b(diag), b(max_row)
                        }
                        invd= a(diag, diag)
                        if diag<=cols then {
                              for col=diag to cols {
                                    a(diag, col)/=invd
                              }
                        }
                        b(diag)/=invd
                        for ro=1 to rows {
                              d1=a(ro,diag)
                              d2=d1*b(diag)
                              if ro<>diag Then {
                                         for col=diag to cols {a(ro, col)-=d1*a(diag, col)}
                                          b(ro)-=d2
                              }
                        }
                  }
            =b()
      }
      Function ArrayLines$(a(), leftmargin=6, maxwidth=8,decimals$="") {
            \\ defualt  no set  decimals, can show any number
            ex$={
            }
           const way$=", {0:"+decimals$+":-"+str$(maxwidth,"")+"}"
            if dimension(a())=1 then {
                  m=each(a())
                  while m {ex$+=format$(way$,array(m))}
                  Insert 3, 2  ex$=string$(" ", leftmargin)
                  =ex$ :    Break
            }
            for i=1 to dimension(a(),1)  {
                  ex1$=""
                  for j=1 to dimension(a(),2 ) {
                              ex1$+=format$(way$,a(i,j))
                  }
                  Insert 1,2  ex1$=string$(" ", leftmargin)
                  ex$+=ex1$+{
                  }
            }
            =ex$
      }
      mm=GaussJordan(a(), b())
            c=each(mm)
            while c {
                  print array(c)
            }
      \\ check accuracy
      link mm to r()
      \\ prepare output document
      Document out$={Algorithm using decimals
                  }+"Matrix A:"+ArrayLines$(a(),,,"2")+{
                  }+"Vector B:"+ArrayLines$(b(),,,"2")+{
                  }+"Solution: "+{
                  }
      acc=25
      for i=1 to  dimension(a(),1)
            sum=a(1,1)-a(1,1)
            For j=1 to dimension(a(),2)
                  sum+=r(j)*a(i,j)
            next j
            p$=format$("Coef. {0::-2},  rounding to {1} decimal, compare {2:-5}, solution: {3}", i, acc, round(sum-b(i),acc)=0@, r(i) )
            Print p$
            Out$=p$+{
            }
      next i
      Report out$
      clipboard out$
}
checkit

```


slower with accuracy of 26 decimals

```M2000 Interpreter

Module Checkit2 {
      Dim Base 1, a(6, 6), b(6)
      \\ a(1,1)= 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.63, 0.39, 0.25, 0.16, 0.10, 1.00, 1.26, 1.58, 1.98, 2.49, 3.13, 1.00, 1.88, 3.55, 6.70, 12.62, 23.80, 1.00, 2.51, 6.32, 15.88, 39.90, 100.28, 1.00, 3.14, 9.87, 31.01, 97.41, 306.02
      a(1,1)=1.1,0.12,0.13,0.12,0.14,-0.12,1.21,0.63,0.39,0.25,0.16,0.1,1.03,1.26,1.58,1.98,2.49,3.13, 1.06,1.88,3.55,6.7,12.62,23.8, 1.12,2.51,6.32,15.88,39.9,100.28,1.16,3.14,9.87,31.01,97.41,306.02
      for i=1 to 6 : for j=1 to 6 : a(i,j)=val(a(i,j)->Decimal) :Next j:Next i
      b(1)=-0.01, 0.61, 0.91, 0.99, 0.60, 0.02
      for i=1 to 6 : b(i)=val(b(i)->Decimal) :Next i
      \\ modules/function to use rational nymbers
      Module Global  subd(m as array, n as array) { ' change m
            link m to m()
            link n to n()
            if m(0)=0 then  return m, 0:=-n(0), 1:=n(1) : exit
            if n(0)=0 then  exit
             return m, 0:=m(0)*(n(1)/m(1))-n(0), 1:=n(1)
      }
      Function Global Inv(m as array){
            link m to m()
            if m(0)=0@ then =m : exit
            =(m(1), m(0))
      }
      Function Global mul(m as array, n as array){' nothing change
             link m to m()
            link n to n()
            if n(0)=0 or n(1)=0 then =(0@,0@) : exit
           =((m(0)/n(1))*n(0),m(1))
      }
      Module Global  mul(m as array, n as array) { ' change m
             link m to m()
            link n to n()
            if n(0)=0 or n(1)=0 then m=(0@,0@) : exit
             return m, 0:=(m(0)/n(1))*n(0)
      }
      Function Global Res(m as array) {
            link m to m()
            if m(0)=0@ then =0@: exit
            =m(0)/m(1)
      }
      \\  GaussJordan  get arrays byvalue
      function GaussJordan(a(), b()) {
            Function  copypointer(m) {  Dim a() : a()=m:=a()}
            \\ we can use : def copypointer(a())=a(0),a(1)
            cols=dimension(a(),1)
            rows=dimension(a(),2)
            Dim Base 1, a(cols, rows)
            for i=1 to cols : for j=1 to rows : a(i, j)=(a(i, j), 1@) : next j : next i
            def d as decimal
            for j=1 to rows : b(j)=(b(j), 1@) : next j
            for diag=1 to rows {
                        max_row=diag
                        max_val=abs(Res(a(diag, diag)))
                        if diag<rows Then {
                              for ro=diag+1 to rows {
                                    d=abs(Res(a(ro, diag)))
                                    if d>max_val then max_row=ro : max_val=d
                              }
                        }
            \\         SwapRows diag, max_row
                        if diag<>max_row then {
                              for i=1 to cols {
                                    swap a(diag, i), a(max_row, i)
                              }
                              swap b(diag), b(max_row)
                        }
                        invd= Inv(a(diag, diag))
                        if diag<=cols then {
                              for col=diag to cols {
                                    mul a(diag, col), invd
                              }
                        }
                        mul b(diag), invd
                         for ro=1 to rows {
                              \\ work also d1=(a(ro,diag)(0), a(ro,diag)(1))
                              d1=copypointer(a(ro, diag))
                              if ro<>diag Then {
                                         for col=diag to cols {subd a(ro, col), mul(d1, a(diag, col))}
                                          subd b(ro), mul(d1, b(diag))
                              }
                        }

                  }
                  dim base 1, ans(len(b()))
                  for i=1 to cols {
                        ans(i)=res(b(i))   \\ : Print b(i)  ' print pairs
                  }
                  =ans()
      }
      Function ArrayLines$(a(), leftmargin=6, maxwidth=8,decimals$="") {
            \\ defualt  no set  decimals, can show any number
            ex$={
            }
           const way$=", {0:"+decimals$+":-"+str$(maxwidth,"")+"}"
            if dimension(a())=1 then {
                  m=each(a())
                  while m {ex$+=format$(way$,array(m))}
                  Insert 3, 2  ex$=string$(" ", leftmargin)
                  =ex$ :    Break
            }
            for i=1 to dimension(a(),1)  {
                  ex1$=""
                  for j=1 to dimension(a(),2 ) {
                              ex1$+=format$(way$,a(i,j))
                  }
                  Insert 1,2  ex1$=string$(" ", leftmargin)
                  ex$+=ex1$+{
                  }
            }
            =ex$
      }
      mm=GaussJordan(a(), b())
            c=each(mm)
            while c {
                  print array(c)
            }
      \\ check accuracy
      link mm to r()
      for i=1 to  dimension(a(),1)
            sum=a(1,1)-a(1,1)
            For j=1 to dimension(a(),2)
                  sum+=r(j)*a(i,j)
            next j
            Print round(sum-b(i),26), b(i)
      next i
      \\ check accuracy
      Document out$={Algorithm using pair of decimals as rational numbers
                  }+"Matrix A:"+ArrayLines$(a(),,,"2")+{
                  }+"Vector B:"+ArrayLines$(b(),,,"2")+{
                  }+"Solution: "+{
                  }
      acc=26
      for i=1 to  dimension(a(),1)
            sum=a(1,1)-a(1,1)
            For j=1 to dimension(a(),2)
                  sum+=r(j)*a(i,j)
            next j
            p$=format$("Coef. {0::-2},  rounding to {1} decimal, compare {2:-5}, solution: {3}", i, acc, round(sum-b(i),acc)=0@, r(i) )
            Print p$
            Out$=p$+{
            }
      next i
      Report out$
      clipboard out$
}
Checkit2

```

<pre style="height:30ex;overflow:scroll">
Algorithm using decimals
Matrix A:
          1,10,     0,12,     0,13,     0,12,     0,14,    -0,12
          1,21,     0,63,     0,39,     0,25,     0,16,     0,10
          1,03,     1,26,     1,58,     1,98,     2,49,     3,13
          1,06,     1,88,     3,55,     6,70,    12,62,    23,80
          1,12,     2,51,     6,32,    15,88,    39,90,   100,28
          1,16,     3,14,     9,87,    31,01,    97,41,   306,02

Vector B:
         -0,01,     0,61,     0,91,     0,99,     0,60,     0,02
Solution:
Coef.  1,  rounding to 26 decimal, compare  True, solution: -0,0597391027501962649904316335
Coef.  2,  rounding to 26 decimal, compare  True, solution: 1,8501896672627829700670299288
Coef.  3,  rounding to 26 decimal, compare  True, solution: -1,9727833018116428175300387318
Coef.  4,  rounding to 26 decimal, compare  True, solution: 1,4697587750651240151384675034
Coef.  5,  rounding to 26 decimal, compare  True, solution: -0,5538741847821888403564152897
Coef.  6,  rounding to 26 decimal, compare  True, solution: 0,0723048745759411900531809852

Algorithm using pair of decimals as rational numbers
Matrix A:
          1,10,     0,12,     0,13,     0,12,     0,14,    -0,12
          1,21,     0,63,     0,39,     0,25,     0,16,     0,10
          1,03,     1,26,     1,58,     1,98,     2,49,     3,13
          1,06,     1,88,     3,55,     6,70,    12,62,    23,80
          1,12,     2,51,     6,32,    15,88,    39,90,   100,28
          1,16,     3,14,     9,87,    31,01,    97,41,   306,02

Vector B:
         -0,01,     0,61,     0,91,     0,99,     0,60,     0,02
Solution:
Coef.  1,  rounding to 26 decimal, compare  True, solution: -0,0597391027501962649904316335
Coef.  2,  rounding to 26 decimal, compare  True, solution: 1,8501896672627829700670299288
Coef.  3,  rounding to 26 decimal, compare  True, solution: -1,9727833018116428175300387317
Coef.  4,  rounding to 26 decimal, compare  True, solution: 1,4697587750651240151384675034
Coef.  5,  rounding to 26 decimal, compare  True, solution: -0,5538741847821888403564152897
Coef.  6,  rounding to 26 decimal, compare  True, solution: 0,0723048745759411900531809852



Algorithm using decimals
Matrix A:
          1,00,     0,00,     0,00,     0,00,     0,00,     0,00
          1,00,     0,63,     0,39,     0,25,     0,16,     0,10
          1,00,     1,26,     1,58,     1,98,     2,49,     3,13
          1,00,     1,88,     3,55,     6,70,    12,62,    23,80
          1,00,     2,51,     6,32,    15,88,    39,90,   100,28
          1,00,     3,14,     9,87,    31,01,    97,41,   306,02

Vector B:
         -0,01,     0,61,     0,91,     0,99,     0,60,     0,02
Solution:
Coef.  1,  rounding to 25 decimal, compare  True, solution: -0,01
Coef.  2,  rounding to 25 decimal, compare  True, solution: 1,6027903945021139442641548525
Coef.  3,  rounding to 25 decimal, compare  True, solution: -1,6132030599055614189052834829
Coef.  4,  rounding to 25 decimal, compare  True, solution: 1,2454941213714367443882298102
Coef.  5,  rounding to 25 decimal, compare  True, solution: -0,4909897195846576129526569211
Coef.  6,  rounding to 25 decimal, compare  True, solution: 0,0657606961752320046201065486


Algorithm using pair of decimals as rational numbers
Matrix A:
          1,00,     0,00,     0,00,     0,00,     0,00,     0,00
          1,00,     0,63,     0,39,     0,25,     0,16,     0,10
          1,00,     1,26,     1,58,     1,98,     2,49,     3,13
          1,00,     1,88,     3,55,     6,70,    12,62,    23,80
          1,00,     2,51,     6,32,    15,88,    39,90,   100,28
          1,00,     3,14,     9,87,    31,01,    97,41,   306,02

Vector B:
         -0,01,     0,61,     0,91,     0,99,     0,60,     0,02
Solution:
Coef.  1,  rounding to 26 decimal, compare  True, solution: -0,01
Coef.  2,  rounding to 26 decimal, compare  True, solution: 1,6027903945021139442641548522
Coef.  3,  rounding to 26 decimal, compare  True, solution: -1,6132030599055614189052834817
Coef.  4,  rounding to 26 decimal, compare  True, solution: 1,2454941213714367443882298085
Coef.  5,  rounding to 26 decimal, compare  True, solution: -0,4909897195846576129526569203
Coef.  6,  rounding to 26 decimal, compare  True, solution: 0,0657606961752320046201065485
</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
GaussianElimination[A_?MatrixQ, b_?VectorQ] := Last /@ RowReduce[Flatten /@ Transpose[{A, b}]]
```



## MATLAB


```MATLAB

function [ x ] = GaussElim( A, b)

% Ensures A is n by n
sz = size(A);
if sz(1)~=sz(2)
    fprintf('A is not n by n\n');
    clear x;
    return;
end

n = sz(1);

% Ensures b is n x 1.
if n~=sz(1)
    fprintf('b is not 1 by n.\n');
    return
end

x = zeros(n,1);
aug = [A b];
tempmatrix = aug;

for i=2:sz(1)


    % Find maximum of row and divide by the maximum
    tempmatrix(1,:) = tempmatrix(1,:)/max(tempmatrix(1,:));

    % Finds the maximum in column
    temp = find(abs(tempmatrix) - max(abs(tempmatrix(:,1))));
    if length(temp)>2
        for j=1:length(temp)-1
            if j~=temp(j)
                maxi = j; %maxi = column number of maximum
                break;
            end
        end
    else % length(temp)==2
        maxi=1;
    end

    % Row swap if maxi is not 1
    if maxi~=1
        temp = tempmatrix(maxi,:);
        tempmatrix(maxi,:) = tempmatrix(1,:);
        tempmatrix(1,:) = temp;
    end

    % Row reducing
    for j=2:length(tempmatrix)-1
        tempmatrix(j,:) = tempmatrix(j,:)-tempmatrix(j,1)/tempmatrix(1,1)*tempmatrix(1,:);
        if tempmatrix(j,j)==0 || isnan(tempmatrix(j,j)) || abs(tempmatrix(j,j))==Inf
            fprintf('Error: Matrix is singular.\n');
            clear x;
            return
        end
    end
    aug(i-1:end,i-1:end) = tempmatrix;

    % Decrease matrix size
    tempmatrix = tempmatrix(2:end,2:end);
end

% Backwards Substitution
x(end) = aug(end,end)/aug(end,end-1);
for i=n-1:-1:1
    x(i) = (aug(i,end)-dot(aug(i,1:end-1),x))/aug(i,i);
end

end

```


=={{header|Modula-3}}==
This implementation defines a generic <code>Matrix</code> type so that the code can be used with different types. As a bonus, we implemented it to work with <i>rings</i> rather than fields, and tested it on two rings: the ring of integers and the ring of integers modulo 46. We include the interface of a ring modulo 46 below; the project's <code>m3makefile</code> (not included) is set up to automatically generates an interface and module for a matrix over each ring.

;requirements of the generic type
The <code>Matrix</code> needs its generic type to implement the following:
* It must have a type <code>T</code>, as per Modula-3 convention.
* It must have procedures
** <code>Nonzero(a: T): BOOLEAN</code>, which indicates whether <code>a</code> is nonzero;
** <code>Minus(a, b: T): T</code> and <code>Times(a, b: T): T</code>, which return the results of the procedures' names; and
** <code>Print(a: T)</code> which does what the name implies.

;Matrix interface

```modula3
GENERIC INTERFACE Matrix(RingElem);

(*
"RingElem" must export the following:
- a type T;
- procedures
  + "Nonzero(a: T): BOOLEAN", which indicates whether "a" is nonzero;
  + "Minus(a, b: T): T" and "Times(a, b: T): T",
    which return the results you'd guess from the procedures' names; and
  + "Print(a: T)", which does what the name implies.
*)

TYPE

  T <: Public;

  Public = OBJECT
  METHODS
    init(READONLY data: ARRAY OF ARRAY OF RingElem.T): T;
    (* use this to copy the entries in "data"; returns "self" *)
    initDimensions(m, n: CARDINAL): T;
    (* use this for an mxn matrix of random entries *)
    num_rows(): CARDINAL;
    (* returns the number of rows in "self" *)
    num_cols(): CARDINAL;
    (* returns the number of columns in "self" *)
    entries(): REF ARRAY OF ARRAY OF RingElem.T;
    (* returns the entries in "self" *)
    triangularize();
    (*
      Performs Gaussian elimination in the context of a ring.
      We can add scalar multiples of rows,
      and we can swap rows, but we may lack multiplicative inverses,
      so we cannot necessarily obtain 1 as a row's first entry.
    *)
  END;

  PROCEDURE PrintMatrix(m: T);
  (* prints the matrix row-by-row; sorry, no special padding to line up columns *)

END Matrix.
```


;Matrix implementation


```modula3
GENERIC MODULE Matrix(RingElem);

IMPORT IO;

TYPE

  REVEAL T = Public BRANDED OBJECT
    rows, cols: CARDINAL;
    data: REF ARRAY OF ARRAY OF RingElem.T;
  OVERRIDES
    init := Init;
    initDimensions := InitDimensions;
    num_rows := Rows;
    num_cols := Columns;
    entries := Entries;
    triangularize := Triangularize;
  END;

PROCEDURE Init(self: T; READONLY d: ARRAY OF ARRAY OF RingElem.T): T =
BEGIN
  self.rows := NUMBER(d);
  self.cols := NUMBER(d[0]);
  self.data := NEW(REF ARRAY OF ARRAY OF RingElem.T, self.rows, self.cols);
  FOR i := FIRST(d) TO LAST(d) DO
    FOR j := FIRST(d[0]) TO LAST(d[0]) DO
      self.data[i-FIRST(d)][j-FIRST(d[0])] := d[i][j];
    END;
  END;
  RETURN self;
END Init;

PROCEDURE InitDimensions(self: T; r, c: CARDINAL): T =
BEGIN
  self.rows := r;
  self.cols := c;
  self.data := NEW(REF ARRAY OF ARRAY OF RingElem.T, r, c);
  RETURN self;
END InitDimensions;

PROCEDURE Rows(self: T): CARDINAL =
BEGIN
  RETURN self.rows;
END Rows;

PROCEDURE Columns(self: T): CARDINAL =
BEGIN
  RETURN self.cols;
END Columns;

PROCEDURE Entries(self: T): REF ARRAY OF ARRAY OF RingElem.T =
BEGIN
  RETURN self.data;
END Entries;

PROCEDURE SwapRows(VAR data: ARRAY OF ARRAY OF RingElem.T; i, j: CARDINAL) =
(* swaps rows i and j of data *)
VAR
  a: RingElem.T;
BEGIN
  WITH Ai = data[i], Aj = data[j], m = FIRST(data[0]), n = LAST(data[0]) DO
    FOR k := m TO n DO
      a     := Ai[k];
      Ai[k] := Aj[k];
      Aj[k] := a;
    END;
  END;
END SwapRows;

PROCEDURE PivotExists(
    VAR data: ARRAY OF ARRAY OF RingElem.T;
    r: CARDINAL;
    VAR i: CARDINAL;
    j: CARDINAL
): BOOLEAN =
(*
  Returns true iff column j of data has a pivot in some row at or after r.
  The row with a pivot is stored in i.
*)
VAR
  searching := TRUE;
  result := LAST(data) + 1;
BEGIN
  i := r;
  WHILE searching AND i <= LAST(data) DO
    IF RingElem.Nonzero(data[i,j]) THEN
      searching := FALSE;
      result := i;
    ELSE
      INC(i);
    END;
  END;
  RETURN NOT searching;
END PivotExists;

PROCEDURE Pivot(VAR data: ARRAY OF ARRAY OF RingElem.T; i, j, k: CARDINAL) =
(*
  Pivots on row i, column j to eliminate row k, column j.
*)
BEGIN
  WITH n = LAST(data[0]), Ai = data[i], Ak = data[k] DO
    VAR a := Ai[j]; b := Ak[j];
    BEGIN
      FOR l := j TO n DO
        IF RingElem.Nonzero(Ai[l]) THEN
          Ak[l] := RingElem.Minus(
              RingElem.Times(Ak[l], a),
              RingElem.Times(Ai[l], b)
          );
        ELSE
          Ak[l] := RingElem.Times(Ak[l], a);
        END;
      END;
    END;
  END;
END Pivot;

PROCEDURE Triangularize(self: T) =
VAR
  i: CARDINAL;
  r := FIRST(self.data[0]);
BEGIN
  WITH data = self.data, m = FIRST(data[0]), n = LAST(data[0]) DO
    FOR j := m TO n DO
      IF PivotExists(data^, r, i, j) THEN
        IF i # j THEN
          SwapRows(data^, i, r);
        END;
        FOR k := r + 1 TO LAST(data^) DO
          IF RingElem.Nonzero(data[k][j]) THEN
            Pivot(data^, r, j, k);
          END;
        END;
        INC(r);
      END;
    END;
  END;
END Triangularize;

PROCEDURE PrintMatrix(self: T) =
BEGIN
  WITH data = self.data DO
    FOR i := FIRST(data^) TO LAST(data^) DO
      IO.Put("[ ");
      WITH Ai = data[i] DO
        FOR j := FIRST(Ai) TO LAST(Ai) DO
          RingElem.Print(Ai[j]);
          IF j # LAST(Ai) THEN
            IO.PutChar(' ');
          END;
        END;
      END;
      IO.Put(" ]\n");
    END;
  END;
END PrintMatrix;

BEGIN
END Matrix.
```


; interface for the ring of integers modulo an integer


```modula3
INTERFACE ModularRing;

(*
Implements arithmetic modulo a nonzero integer.
Assertions check that the modulus is nonzero.
*)

TYPE

  T = RECORD
    value, modulus: CARDINAL;
  END;

PROCEDURE Init(VAR a: T; value: INTEGER; modulus: CARDINAL);
(* initializes a to the given value and modulus *)

PROCEDURE Nonzero(n: T): BOOLEAN;

PROCEDURE Plus(a, b: T): T;

PROCEDURE Minus(a, b: T): T;

PROCEDURE Times(a, b: T): T;

PROCEDURE Print(a: T; withModulus := FALSE);
(*
   when "withModulus" is "TRUE",
   this adds after "a" the letter "m",
   followed by the modulus
*)

END ModularRing.
```


;test implementation

It's fairly easy to initialize an array of types in Modula-3, but it can get cumbersome with structured types, so we wrote a procedure to convert an integer matrix to a matrix of integers modulo a number.


```modula3
MODULE GaussianElimination EXPORTS Main;

IMPORT IO, ModularRing AS MR, IntMatrix AS IM, ModMatrix AS MM;

CONST

  (* data to set up the matrices *)

  A1 = ARRAY OF INTEGER { 2, 1, 0 };
  A2 = ARRAY OF INTEGER { 1, 2, 0 };
  A3 = ARRAY OF INTEGER { 0, 3, 0 };
  A = ARRAY OF ARRAY OF INTEGER { A1, A2, A3 };

  B1 = ARRAY OF INTEGER {  4,  8, 0, -4, 0 };
  B2 = ARRAY OF INTEGER { -3, -6, 0,  9, 0 };
  B3 = ARRAY OF INTEGER {  1,  3, 5,  7, 2 };
  B4 = ARRAY OF INTEGER {  7,  5, 3,  1, 2 };
  B = ARRAY OF ARRAY OF INTEGER { B1, B2, B3, B4 };

PROCEDURE IntToModArray(READONLY A: IM.T; VAR B: MM.T; mod: CARDINAL) =
(*
   copies a two-dimensional array of integers
   to a two-dimension array of integers modulo "mod"
*)
BEGIN
  B := NEW(MM.T).initDimensions(A.num_rows(), A.num_cols());
  WITH Adata = A.entries(), Bdata = B.entries() DO
    FOR i := FIRST(Adata^) TO LAST(Adata^) DO
      WITH Ai = Adata[i], Bi = Bdata[i] DO
        FOR j := FIRST(Ai) TO LAST(Ai) DO
          MR.Init(Bi[j], Ai[j], mod);
        END;
      END;
    END;
  END;
END IntToModArray;

VAR

  M: IM.T;
  N: MM.T;

BEGIN

  (* triangularize the data in A *)
  M := NEW(IM.T).init(A);
  IO.Put("Initial A:\n");
  IM.PrintMatrix(M);
  IO.PutChar('\n');
  M.triangularize();
  IO.Put("Final A:\n");
  IM.PrintMatrix(M);
  IO.PutChar('\n');
  IO.PutChar('\n');

  (* triangularize the data in B, all computations modulo 46 *)
  M := NEW(IM.T).init(B);
  IntToModArray(M, N, 46);
  IO.Put("Initial B:\n");
  MM.PrintMatrix(N);
  IO.PutChar('\n');
  N.triangularize();
  IO.Put("Final B:\n");
  MM.PrintMatrix(N);
  IO.PutChar('\n');

END GaussianElimination.
```


```txt

Initial A:
[ 2 1 0 ]
[ 1 2 0 ]
[ 0 3 0 ]

Final A:
[ 2 1 0 ]
[ 0 3 0 ]
[ 0 0 0 ]


Initial B:
[ 4 8 0 42 0 ]
[ 43 40 0 9 0 ]
[ 1 3 5 7 2 ]
[ 7 5 3 1 2 ]

Final B:
[ 4 8 0 42 0 ]
[ 0 4 20 32 8 ]
[ 0 0 32 38 44 ]
[ 0 0 0 24 0 ]


```



## OCaml

The OCaml stdlib is fairly lean, so these stand-alone solutions often need to include support functions which would be part of a codebase, like these...

```OCaml

module Array = struct
  include Array
  (* Computes: f a.(0) + f a.(1) + ... where + is 'g'. *)
  let foldmap g f a =
    let n = Array.length a in
    let rec aux acc i =
      if i >= n then acc else aux (g acc (f a.(i))) (succ i)
    in aux (f a.(0)) 1

  (* like the stdlib fold_left, but also provides index to f *)
  let foldi_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f i !r (unsafe_get a i)
    done;
    !r
end

let foldmap_range g f (a,b) =
  let rec aux acc n =
    let n = succ n in
    if n > b then acc else aux (g acc (f n)) n
  in aux (f a) a

let fold_range f init (a,b) =
  let rec aux acc n =
    if n > b then acc else aux (f acc n) (succ n)
  in aux init a

```

The solver:

```OCaml

(* Some less-general support functions for 'solve'. *)
let swap_elem m i j = let x = m.(i) in m.(i) <- m.(j); m.(j) <- x
let maxtup a b = if (snd a) > (snd b) then a else b
let augmented_matrix m b =
  Array.(init (length m) ( fun i -> append m.(i) [|b.(i)|] ))

(* Solve Ax=b for x, using gaussian elimination with scaled partial pivot,
 * and then back-substitution of the resulting row-echelon matrix. *)
let solve m b =
  let n = Array.length m in
  let n' = pred n in (* last index = n-1 *)
  let s = Array.(map (foldmap max abs_float) m) in  (* scaling vector *)
  let a = augmented_matrix m b in

  for k = 0 to pred n' do
    (* Scaled partial pivot, to preserve precision *)
    let pair i = (i, abs_float a.(i).(k) /. s.(i)) in
    let i_max,v = foldmap_range maxtup pair (k,n') in
    if v < epsilon_float then failwith "Matrix is singular.";
    swap_elem a k i_max;
    swap_elem s k i_max;

    (* Eliminate one column *)
    for i = succ k to n' do
      let tmp = a.(i).(k) /. a.(k).(k) in
      for j = succ k to n do
        a.(i).(j) <- a.(i).(j) -. tmp *. a.(k).(j);
      done
    done
  done;

  (* Backward substitution; 'b' is in the 'nth' column of 'a' *)
  let x = Array.copy b in (* just a fresh array of the right size and type *)
  for i = n' downto 0 do
    let minus_dprod t j = t -. x.(j) *. a.(i).(j) in
    x.(i) <- fold_range minus_dprod a.(i).(n) (i+1,n') /. a.(i).(i);
  done;
  x

```

Example data...

```OCaml

let a =
  [| [| 1.00; 0.00; 0.00;  0.00;  0.00; 0.00 |];
     [| 1.00; 0.63; 0.39;  0.25;  0.16; 0.10 |];
     [| 1.00; 1.26; 1.58;  1.98;  2.49; 3.13 |];
     [| 1.00; 1.88; 3.55;  6.70; 12.62; 23.80 |];
     [| 1.00; 2.51; 6.32; 15.88; 39.90; 100.28 |];
     [| 1.00; 3.14; 9.87; 31.01; 97.41; 306.02 |] |]
let b = [| -0.01; 0.61; 0.91; 0.99; 0.60; 0.02 |]

```

In the REPL, the solution is:

```OCaml

# let x = solve a b;;
val x : float array =
[|-0.0100000000000000991; 1.60279039450210536; -1.61320305990553226;
  1.24549412137140547; -0.490989719584644546; 0.0657606961752301433|]

```

Further, let's define multiplication and subtraction to check our results...

```OCaml

let mul m v =
  Array.mapi (fun i u ->
    Array.foldi_left (fun j sum uj ->
      sum +. uj *. v.(j)
    ) 0. u
  ) m

let sub u v = Array.mapi (fun i e -> e -. v.(i)) u

```

Now 'x' can be plugged into the equation to calculate the residual:

```OCaml

# let residual = sub b (mul a x);;
val residual : float array =
  [|9.8879238130678e-17; 1.11022302462515654e-16; 2.22044604925031308e-16;
    8.88178419700125232e-16; -5.5511151231257827e-16; 4.26741975090294545e-16|]

```



## PARI/GP

If A and B have floating-point numbers (<code>t_REAL</code>s) then the following uses Gaussian elimination:

```parigp
matsolve(A,B)
```


If the entries are integers, then ''p''-adic lifting (Dixon 1982) is used instead.


## Perl

```Perl
use Math::Matrix;
my $a = Math::Matrix->new([0,1,0],
                          [0,0,1],
                          [2,0,1]);
my $b = Math::Matrix->new([1],
                          [2],
                          [4]);
my $x = $a->concat($b)->solve;
print $x;
```


<code>Math::Matrix</code> <code>solve()</code> expects the column vector to be an extra column in the matrix, hence <code>concat()</code>.  Putting not just a column there but a whole identity matrix (making Nx2N) is how its <code>invert()</code> is implemented.  Note that <code>solve()</code> doesn't notice singular matrices and still gives a return when there is in fact no solution to Ax=B.


## Perl 6

Gaussian elimination results in a matrix in row echelon form. Gaussian elimination with back-substitution (also known as Gauss-Jordan elimination) results in a matrix in reduced row echelon form. That being the case, we can reuse much of the code from the [[Reduced row echelon form]] task. Perl 6 stores and does calculations on decimal numbers within its limit of precision using Rational numbers by default, meaning the calculations are exact.


```perl6
sub gauss-jordan-solve (@a, @b) {
    @b.kv.map: { @a[$^k].append: $^v };
    @a.&rref[*]»[*-1];
}

# reduced row echelon form (Gauss-Jordan elimination)
sub rref (@m) {
    return unless @m;
    my ($lead, $rows, $cols) = 0, +@m, +@m[0];

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
    @m
}

sub rat-or-int ($num) {
    return $num unless $num ~~ Rat;
    return $num.narrow if $num.narrow.WHAT ~~ Int;
    $num.nude.join: '/';
}

sub say-it ($message, @array, $fmt = " %8s") {
    say "\n$message";
    $_».&rat-or-int.fmt($fmt).put for @array;
}

my @a = (
    [ 1.00, 0.00, 0.00,  0.00,  0.00,   0.00 ],
    [ 1.00, 0.63, 0.39,  0.25,  0.16,   0.10 ],
    [ 1.00, 1.26, 1.58,  1.98,  2.49,   3.13 ],
    [ 1.00, 1.88, 3.55,  6.70, 12.62,  23.80 ],
    [ 1.00, 2.51, 6.32, 15.88, 39.90, 100.28 ],
    [ 1.00, 3.14, 9.87, 31.01, 97.41, 306.02 ],
);
my @b = ( -0.01, 0.61, 0.91, 0.99, 0.60, 0.02 );

say-it 'A matrix:', @a, "%6.2f";
say-it 'or, A in exact rationals:', @a;
say-it 'B matrix:', @b, "%6.2f";
say-it 'or, B in exact rationals:', @b;
say-it 'x matrix:', (my @gj = gauss-jordan-solve @a, @b), "%16.12f";
say-it 'or, x in exact rationals:', @gj, "%28s";

```

```txt
A matrix:
  1.00   0.00   0.00   0.00   0.00   0.00
  1.00   0.63   0.39   0.25   0.16   0.10
  1.00   1.26   1.58   1.98   2.49   3.13
  1.00   1.88   3.55   6.70  12.62  23.80
  1.00   2.51   6.32  15.88  39.90 100.28
  1.00   3.14   9.87  31.01  97.41 306.02

or, A in exact rationals:
        1         0         0         0         0         0
        1    63/100    39/100       1/4      4/25      1/10
        1     63/50     79/50     99/50   249/100   313/100
        1     47/25     71/20     67/10    631/50     119/5
        1   251/100    158/25    397/25    399/10   2507/25
        1    157/50   987/100  3101/100  9741/100  15301/50

B matrix:
 -0.01
  0.61
  0.91
  0.99
  0.60
  0.02

or, B in exact rationals:
   -1/100
   61/100
   91/100
   99/100
      3/5
     1/50

x matrix:
 -0.010000000000
  1.602790394502
 -1.613203059906
  1.245494121371
 -0.490989719585
  0.065760696175

or, x in exact rationals:
                      -1/100
   655870882787/409205648497
  -660131804286/409205648497
   509663229635/409205648497
  -200915766608/409205648497
    26909648324/409205648497

```



## Phix

```Phix
function gauss_eliminate(sequence a, b)
    integer n = length(b)
    atom tmp
    for col=1 to n do
        integer m = col
        atom mx = a[m][m]
        for i=col+1 to n do
            tmp = abs(a[i][col])
            if tmp>mx then
                {m,mx} = {i,tmp}
            end if
        end for
        if col!=m then
            {a[col],a[m]} = {a[m],a[col]}
            {b[col],b[m]} = {b[m],b[col]}
        end if
        for i=col+1 to n do
            tmp = a[i][col]/a[col][col]
            for j=col+1 to n do
                a[i][j] -= tmp*a[col][j]
            end for
            a[i][col] = 0
            b[i] -= tmp*b[col]
        end for
    end for
    sequence x = repeat(0,n)
    for col=n to 1 by -1 do
        tmp = b[col]
        for j=n to col+1 by -1 do
            tmp -= x[j]*a[col][j]
        end for
        x[col] = tmp/a[col][col]
    end for
    return x
end function

constant a = {{1.00, 0.00, 0.00,  0.00,  0.00,   0.00},
              {1.00, 0.63, 0.39,  0.25,  0.16,   0.10},
              {1.00, 1.26, 1.58,  1.98,  2.49,   3.13},
              {1.00, 1.88, 3.55,  6.70, 12.62,  23.80},
              {1.00, 2.51, 6.32, 15.88, 39.90, 100.28},
              {1.00, 3.14, 9.87, 31.01, 97.41, 306.02}},
         b = {-0.01, 0.61, 0.91,  0.99,  0.60,   0.02}

pp(gauss_eliminate(a, b))
```

```txt

{-0.01,1.602790395,-1.61320306,1.245494121,-0.4909897196,0.06576069618}

```



## PHP


```php
function swap_rows(&$a, &$b, $r1, $r2)
{
    if ($r1 == $r2) return;

    $tmp = $a[$r1];
    $a[$r1] = $a[$r2];
    $a[$r2] = $tmp;

    $tmp = $b[$r1];
    $b[$r1] = $b[$r2];
    $b[$r2] = $tmp;
}

function gauss_eliminate($A, $b, $N)
{
    for ($col = 0; $col < $N; $col++)
    {
        $j = $col;
        $max = $A[$j][$j];

        for ($i = $col + 1; $i < $N; $i++)
        {
            $tmp = abs($A[$i][$col]);
            if ($tmp > $max)
            {
                $j = $i;
                $max = $tmp;
            }
        }

        swap_rows($A, $b, $col, $j);

        for ($i = $col + 1; $i < $N; $i++)
        {
            $tmp = $A[$i][$col] / $A[$col][$col];
            for ($j = $col + 1; $j < $N; $j++)
            {
                $A[$i][$j] -= $tmp * $A[$col][$j];
            }
            $A[$i][$col] = 0;
            $b[$i] -= $tmp * $b[$col];
        }
    }
    $x = array();
    for ($col = $N - 1; $col >= 0; $col--)
    {
        $tmp = $b[$col];
        for ($j = $N - 1; $j > $col; $j--)
        {
            $tmp -= $x[$j] * $A[$col][$j];
        }
        $x[$col] = $tmp / $A[$col][$col];
    }
    return $x;
}

function test_gauss()
{
    $a = array(
        array(1.00, 0.00, 0.00,  0.00,  0.00, 0.00),
        array(1.00, 0.63, 0.39,  0.25,  0.16, 0.10),
        array(1.00, 1.26, 1.58,  1.98,  2.49, 3.13),
        array(1.00, 1.88, 3.55,  6.70, 12.62, 23.80),
        array(1.00, 2.51, 6.32, 15.88, 39.90, 100.28),
        array(1.00, 3.14, 9.87, 31.01, 97.41, 306.02)
    );
    $b = array( -0.01, 0.61, 0.91, 0.99, 0.60, 0.02 );

    $x = gauss_eliminate($a, $b, 6);

    ksort($x);
    print_r($x);
}

test_gauss();
```


```txt

Array
(
    [0] => -0.01
    [1] => 1.6027903945021
    [2] => -1.6132030599055
    [3] => 1.2454941213714
    [4] => -0.49098971958463
    [5] => 0.065760696175228
)

```



## PL/I


```pli
Solve: procedure options (main);    /* 11 January 2014 */

   declare n fixed binary;
   put ('Program to solve n simultaneous equations of the form Ax = b. Please type n:' );
   get (n);

begin;
   declare (A(n, n), b(n), x(n)) float(18);
   declare (SA(n,n), Sb(n)) float (18);
   declare i fixed binary;

   put skip list ('Please type A:');
   get (a);
   put skip list ('Please type the right-hand sides, b:');
   get (b);

   SA = A; Sb = b;

   put skip list ('The equations are:');
   do i = 1 to n;
      put skip edit (A(i,*), b(i)) (f(5), x(1));
   end;

   call Gauss_elimination (A, b);

   call Backward_substitution (A, b, x);

   put skip list ('Solutions:'); put skip data (x);

   /* Check solutions: */
   put skip list ('Residuals:');
   do i = 1 to n;
      put skip list (sum(SA(i,*) * x(*)) - Sb(i));
   end;
end;

Gauss_elimination: procedure (A, b) options (reorder); /* Triangularise */
   declare (A(*,*), b(*)) float(18);
   declare n fixed binary initial (hbound(A, 1));
   declare (i, j, k) fixed binary;
   declare t float(18);

   do j = 1 to n;
      do i = j+1 to n; /* For each of the rows beneath the current (pivot) row. */
         t = A(j,j) / A(i,j);
         do k = j+1 to n; /* Subtract a multiple of row i from row j. */
            A(i,k) = A(j,k) - t*A(i,k);
         end;
         b(i) = b(j) - t*b(i); /* ... and the right-hand side. */
      end;
   end;
end Gauss_elimination;

Backward_substitution: procedure (A, b, x) options (reorder);
   declare (A(*,*), b(*), x(*)) float(18);
   declare t float(18);
   declare n fixed binary initial (hbound(A, 1));
   declare (i, j) fixed binary;

   x(n) = b(n) / a(n,n);

   do j = n-1 to 1 by -1;
      t = 0;
      do i = j+1 to n;
         t = t + a(j,i)*x(i);
      end;
      x(j) = (b(j) - t) / a(j,j);
   end;
end Backward_substitution;

end Solve;
```

```txt

Program to solve n simultaneous equations of the form Ax = b. Please type n:

Please type A:

Please type the right-hand sides, b:

The equations are:
    1     2     3    14
    2     1     3    13
    3    -2    -1    -4
Solutions:
X(1)= 1.00000000000000000E+0000                 X(2)= 2.00000000000000000E+0000
X(3)= 3.00000000000000000E+0000;
Residuals:
 0.00000000000000000E+0000
 0.00000000000000000E+0000
 0.00000000000000000E+0000

```



## PowerShell


### Gauss


```PowerShell

function gauss($a,$b) {
    $n = $a.count
    for ($k = 0; $k -lt $n; $k++) {
        $lmax, $max = $k, [Math]::Abs($a[$k][$k])
        for ($l = $k+1; $l -lt $n; $l++) {
            $tmp = [Math]::Abs($a[$l][$k])
            if($max -lt $tmp) {
                $max, $lmax = $tmp, $l
            }
        }
        if ($k -ne $lmax) {
            $a[$k], $a[$lmax] = $a[$lmax], $a[$k]
            $b[$k], $b[$lmax] = $b[$lmax], $b[$k]
        }
        $akk = $a[$k][$k]
        for ($i = $k+1; $i -lt $n; $i++){
            $aik  = $a[$i][$k]
            for ($j = $k; $j -lt $n; $j++) {
                $a[$i][$j] = $a[$i][$j]*$akk - $a[$k][$j]*$aik
            }
            $b[$i] = $b[$i]*$akk - $b[$k]*$aik
        }
    }
    for ($i = $n-1; $i -ge 0; $i--) {
        for ($j = $i+1; $j -lt $n; $j++) {
            $b[$i] -= $b[$j]*$a[$i][$j]
        }
        $b[$i] = $b[$i]/$a[$i][$i]
    }
    $b
}
function show($a) {
    if($a) {
        0..($a.Count - 1) | foreach{ if($a[$_]){"$($a[$_][0..($a[$_].count -1)])"}else{""} }
    }
}
$a =(
@(1.00, 0.00, 0.00,  0.00,  0.00, 0.00),
@(1.00, 0.63, 0.39,  0.25,  0.16, 0.10),
@(1.00, 1.26, 1.58,  1.98,  2.49, 3.13),
@(1.00, 1.88, 3.55,  6.70, 12.62, 23.80),
@(1.00, 2.51, 6.32, 15.88, 39.90, 100.28),
@(1.00, 3.14, 9.87, 31.01, 97.41, 306.02)
)
"a ="
show $a
""
$b = @(-0.01, 0.61, 0.91, 0.99, 0.60, 0.02)
"b ="
$b
""
"x ="
gauss $a $b


```

<b>Output:</b>

```txt

a =
1 0 0 0 0 0
1 0.63 0.39 0.25 0.16 0.1
1 1.26 1.58 1.98 2.49 3.13
1 1.88 3.55 6.7 12.62 23.8
1 2.51 6.32 15.88 39.9 100.28
1 3.14 9.87 31.01 97.41 306.02

b =
-0.01
0.61
0.91
0.99
0.6
0.02

x =
-0.01
1.60279039450213
-1.6132030599056
1.24549412137148
-0.490989719584674
0.0657606961752342

```

===Gauss-Jordan===

```PowerShell

function gauss-jordan($a,$b) {
    $n = $a.count
    for ($k = 0; $k -lt $n; $k++) {
        $lmax, $max = $k, [Math]::Abs($a[$k][$k])
        for ($l = $k+1; $l -lt $n; $l++) {
            $tmp = [Math]::Abs($a[$l][$k])
            if($max -lt $tmp) {
                $max, $lmax = $tmp, $l
            }
        }
        if ($k -ne $lmax) {
            $a[$k], $a[$lmax] = $a[$lmax], $a[$k]
            $b[$k], $b[$lmax] = $b[$lmax], $b[$k]
        }
        $akk = $a[$k][$k]
        for ($j = $k; $j -lt $n; $j++) {$a[$k][$j] /= $akk}
        $b[$k] /= $akk
        for ($i = 0; $i -lt $n; $i++){
            if ($i -ne $k) {
                $aik  = $a[$i][$k]
                for ($j = $k; $j -lt $n; $j++) {
                    $a[$i][$j] = $a[$i][$j] - $a[$k][$j]*$aik
                }
                $b[$i] = $b[$i] - $b[$k]*$aik
            }
        }
    }
    $b
}
function show($a) {
    if($a) {
        0..($a.Count - 1) | foreach{ if($a[$_]){"$($a[$_][0..($a[$_].count -1)])"}else{""} }
    }
}
$a =(
@(1.00, 0.00, 0.00,  0.00,  0.00, 0.00),
@(1.00, 0.63, 0.39,  0.25,  0.16, 0.10),
@(1.00, 1.26, 1.58,  1.98,  2.49, 3.13),
@(1.00, 1.88, 3.55,  6.70, 12.62, 23.80),
@(1.00, 2.51, 6.32, 15.88, 39.90, 100.28),
@(1.00, 3.14, 9.87, 31.01, 97.41, 306.02)
)
"a ="
show $a
""
$b = @(-0.01, 0.61, 0.91, 0.99, 0.60, 0.02)
"b ="
$b
""
"x ="
gauss-jordan $a $b

```

<b>Output:</b>

```txt

a =
1 0 0 0 0 0
1 0.63 0.39 0.25 0.16 0.1
1 1.26 1.58 1.98 2.49 3.13
1 1.88 3.55 6.7 12.62 23.8
1 2.51 6.32 15.88 39.9 100.28
1 3.14 9.87 31.01 97.41 306.02

b =
-0.01
0.61
0.91
0.99
0.6
0.02

x =
-0.01
1.60279039450211
-1.61320305990556
1.24549412137144
-0.490989719584659
0.0657606961752323

```



## Python


```python
# The 'gauss' function takes two matrices, 'a' and 'b', with 'a' square, and it return the determinant of 'a' and a matrix 'x' such that a*x = b.
# If 'b' is the identity, then 'x' is the inverse of 'a'.

import copy
from fractions import Fraction

def gauss(a, b):
    a = copy.deepcopy(a)
    b = copy.deepcopy(b)
    n = len(a)
    p = len(b[0])
    det = 1
    for i in range(n - 1):
        k = i
        for j in range(i + 1, n):
            if abs(a[j][i]) > abs(a[k][i]):
                k = j
        if k != i:
            a[i], a[k] = a[k], a[i]
            b[i], b[k] = b[k], b[i]
            det = -det

        for j in range(i + 1, n):
            t = a[j][i]/a[i][i]
            for k in range(i + 1, n):
                a[j][k] -= t*a[i][k]
            for k in range(p):
                b[j][k] -= t*b[i][k]

    for i in range(n - 1, -1, -1):
        for j in range(i + 1, n):
            t = a[i][j]
            for k in range(p):
                b[i][k] -= t*b[j][k]
        t = 1/a[i][i]
        det *= a[i][i]
        for j in range(p):
            b[i][j] *= t
    return det, b

def zeromat(p, q):
    return [[0]*q for i in range(p)]

def matmul(a, b):
    n, p = len(a), len(a[0])
    p1, q = len(b), len(b[0])
    if p != p1:
        raise ValueError("Incompatible dimensions")
    c = zeromat(n, q)
    for i in range(n):
        for j in range(q):
                c[i][j] = sum(a[i][k]*b[k][j] for k in range(p))
    return c


def mapmat(f, a):
    return [list(map(f, v)) for v in a]

def ratmat(a):
    return mapmat(Fraction, a)

# As an example, compute the determinant and inverse of 3x3 magic square

a = [[2, 9, 4], [7, 5, 3], [6, 1, 8]]
b = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
det, c = gauss(a, b)

det
-360.0

c
[[-0.10277777777777776, 0.18888888888888888, -0.019444444444444438],
[0.10555555555555554, 0.02222222222222223, -0.061111111111111116],
[0.0638888888888889, -0.14444444444444446, 0.14722222222222223]]

# Check product
matmul(a, c)
[[1.0, 0.0, 0.0], [5.551115123125783e-17, 1.0, 0.0],
[1.1102230246251565e-16, -2.220446049250313e-16, 1.0]]

# Same with fractions, so the result is exact

det, c = gauss(ratmat(a), ratmat(b))

det
Fraction(-360, 1)

c
[[Fraction(-37, 360), Fraction(17, 90), Fraction(-7, 360)],
[Fraction(19, 180), Fraction(1, 45), Fraction(-11, 180)],
[Fraction(23, 360), Fraction(-13, 90), Fraction(53, 360)]]

matmul(a, c)
[[Fraction(1, 1), Fraction(0, 1), Fraction(0, 1)],
[Fraction(0, 1), Fraction(1, 1), Fraction(0, 1)],
[Fraction(0, 1), Fraction(0, 1), Fraction(1, 1)]]
```


### Using numpy


```python3

$ python3
Python 3.6.0 |Anaconda custom (64-bit)| (default, Dec 23 2016, 12:22:00)
[GCC 4.4.7 20120313 (Red Hat 4.4.7-1)] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> # https://docs.scipy.org/doc/numpy/reference/generated/numpy.linalg.solve.html
>>> import numpy.linalg
>>> a = [[2, 9, 4], [7, 5, 3], [6, 1, 8]]
>>> b = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
>>> numpy.linalg.solve(a,b)
array([[-0.10277778,  0.18888889, -0.01944444],
       [ 0.10555556,  0.02222222, -0.06111111],
       [ 0.06388889, -0.14444444,  0.14722222]])
>>>

```



## Racket


```racket

#lang racket
(require math/matrix)
(define A
  (matrix [[1.00  0.00  0.00  0.00  0.00   0.00]
           [1.00  0.63  0.39  0.25  0.16   0.10]
           [1.00  1.26  1.58  1.98  2.49   3.13]
           [1.00  1.88  3.55  6.70 12.62  23.80]
           [1.00  2.51  6.32 15.88 39.90 100.28]
           [1.00  3.14  9.87 31.01 97.41 306.02]]))

(define b (col-matrix [-0.01 0.61 0.91 0.99 0.60 0.02]))

(matrix-solve A b)

```

```racket

#<array
  '#(6 1)
  #[-0.01
   1.602790394502109
   -1.613203059905556
   1.2454941213714346
   -0.4909897195846582
   0.06576069617523222]>

```



## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 07.08.2014 Walter Pachl translated from PL/I)
* improved to get integer results for, e.g. this input:
  -6 -18  13   6  -6 -15  -2  -9    -231
   2  20   9   2  16 -12 -18  -5     647
  23  18 -14 -14  -1  16  25 -17    -907
  -8  -1 -19   4   3 -14  23   8     248
  25  20  -6  15   0 -10   9  17    1316
 -13  -1   3   5  -2  17  14 -12   -1080
  19  24 -21  -5 -19   0 -24 -17    1006
  20  -3 -14 -16 -23 -25 -15  20    1496
*--------------------------------------------------------------------*/
  Numeric Digits 20
  Parse Arg t
  n=3
  Parse Value '1  2  3 14' With a.1.1 a.1.2 a.1.3 b.1
  Parse Value '2  1  3 13' With a.2.1 a.2.2 a.2.3 b.2
  Parse Value '3 -2 -1 -4' With a.3.1 a.3.2 a.3.3 b.3
  If t=6 Then Do
    n=6
    Parse Value '1.00 0.00 0.00  0.00  0.00 0.00  ' With a.1.1 a.1.2 a.1.3 a.1.4 a.1.5 a.1.6 .
    Parse Value '1.00 0.63 0.39  0.25  0.16 0.10  ' With a.2.1 a.2.2 a.2.3 a.2.4 a.2.5 a.2.6 .
    Parse Value '1.00 1.26 1.58  1.98  2.49 3.13  ' With a.3.1 a.3.2 a.3.3 a.3.4 a.3.5 a.3.6 .
    Parse Value '1.00 1.88 3.55  6.70 12.62 23.80 ' With a.4.1 a.4.2 a.4.3 a.4.4 a.4.5 a.4.6 .
    Parse Value '1.00 2.51 6.32 15.88 39.90 100.28' With a.5.1 a.5.2 a.5.3 a.5.4 a.5.5 a.5.6 .
    Parse Value '1.00 3.14 9.87 31.01 97.41 306.02' With a.6.1 a.6.2 a.6.3 a.6.4 a.6.5 a.6.6 .
    Parse Value '-0.01 0.61 0.91 0.99 0.60 0.02'    With b.1 b.2 b.3 b.4 b.5 b.6 .
    End
  Do i=1 To n
    Do j=1 To n
      sa.i.j=a.i.j
      End
    sb.i=b.i
    End
  Say 'The equations are:'
  do i = 1 to n;
    ol=''
    Do j=1 To n
      ol=ol format(a.i.j,4,4)
      End
    ol=ol'  'format(b.i,4,4)
    Say ol
    end

  call Gauss_elimination

  call Backward_substitution

  Say 'Solutions:'
  Do i=1 To n
    Say 'x('i')='||x.i
    End

  /* Check solutions: */
  Say 'Residuals:'
  do i = 1 to n
    res=0
    Do j=1 To n
      res=res+(sa.i.j*x.j)
      End
    res=res-sb.i
    Say 'res('i')='res
    End

Exit

Gauss_elimination:
  Do j=1 to n-1
    ma=a.j.j
    Do ja=j+1 To n
      mb=a.ja.j
      Do i=1 To n
        new=a.j.i*mb-a.ja.i*ma
        a.ja.i=new
        End
      b.ja=b.j*mb-b.ja*ma
      End
    End
  Return

Backward_substitution:
  x.n = b.n / a.n.n
  do j = n-1 to 1 by -1
     t = 0
     do i = j+1 to n
        t = t + a.j.i*x.i
     end
     x.j = (b.j - t) / a.j.j
  end
  Return
```

```txt
The equations are:
    1.0000    2.0000    3.0000    14.0000
    2.0000    1.0000    3.0000    13.0000
    3.0000   -2.0000   -1.0000    -4.0000
Solutions:
x(1)=1
x(2)=2
x(3)=3
Residuals:
res(1)=0
res(2)=0
res(3)=0
```

and with test data from PHP

```txt
The equations are:
    1.0000    0.0000    0.0000    0.0000    0.0000    0.0000    -0.0100
    1.0000    0.6300    0.3900    0.2500    0.1600    0.1000     0.6100
    1.0000    1.2600    1.5800    1.9800    2.4900    3.1300     0.9100
    1.0000    1.8800    3.5500    6.7000   12.6200   23.8000     0.9900
    1.0000    2.5100    6.3200   15.8800   39.9000  100.2800     0.6000
    1.0000    3.1400    9.8700   31.0100   97.4100  306.0200     0.0200
Solutions:
x(1)=-0.01
x(2)=1.6027903945021139463
x(3)=-1.6132030599055614262
x(4)=1.2454941213714367527
x(5)=-0.49098971958465761669
x(6)=0.065760696175232005188
Residuals:
res(1)=0
res(2)=0.00000000000000000001
res(3)=-0.00000000000000000016
res(4)=0
res(5)=-0.0000000000000000017
res(6)=0.000000000000000001
```



### version 2

(Data was placed into a file instead of placing the data into the REXX program.)

Programming note:   with the large precision   ('''numeric digits 1000'''),   the residuals were insignificant.

Only   '''8'''   (fractional) decimal digits were used for the output display.

```rexx
/*REXX program solves   Ax=b   with Gaussian elimination  and  backwards  substitution. */
parse arg iFID .                                 /*obtain optional argument from the CL.*/
numeric digits 1000                              /*heavy─duty decimal digits precision. */
if iFID=='' | iFID=="," then iFID= 'GAUSS_E.DAT' /*Not specified?  Then use the default.*/
     do rec=1    while lines(iFID) \== 0         /*read the              equation sets. */
     #=0                                         /*the number of equations  (so far).   */
         do $=1  while lines(iFID) \== 0         /*process the equation.                */
         z=linein(iFID);    if z=''  then leave  /*Is this a blank line?    end─of─data.*/
         if $==1  then do;  say;     say center(' equations ', 75, "▓");        say
                       end                       /* [↑]  if 1st equation, then show hdr.*/
         say z                                   /*display an equation to the terminal. */
         if left(space(z), 1)=='*'  then iterate /*Is this a comment?    Then ignore it.*/
         #=# + 1;      n=words(z) - 1            /*assign equation #; calculate # items.*/
           do e=1  for n;     a.#.e= word(z, e)
           end   /*e*/                           /* [↑]  process  A  numbers.           */
         b.#=word(z, n + 1)                      /* ◄───    "     B     "               */
         end     /*$*/
     if #\==0  then call Gauss_elim              /*Not zero?  Then display the results. */
     end         /*rec*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Gauss_elim:                   do     j=1  for n;   jp=j + 1
                                do   i=jp  to n;   _=a.j.j / a.i.j
                                  do k=jp  to n;   a.i.k=a.j.k   -   _ * a.i.k
                                  end   /*k*/
                                b.i=b.j   -   _ * b.i
                                end     /*i*/
                              end       /*j*/
            x.n=b.n / a.n.n
                              do   j=n-1  to 1  by -1;   _=0
                                do i=j+1  to n;    _=_   +   a.j.i * x.i
                                end     /*i*/
                              x.j=(b.j - _) / a.j.j
                              end       /*j*/    /* [↑]  uses backwards substitution.   */
            say
            numeric digits 8                     /*for the display,  only use 8 digits. */
            say center('solution', 75, "═"); say /*a title line for articulated output. */
                   do o=1  for n;   say right('x['o"] = ", 38)   left('', x.o>=0)    x.o/1
                   end   /*o*/
            return
```

```txt

*     a1   a2   a3     b
*    ───  ───  ───    ───
      1    2    3     14
      2    1    3     13
      3   -2   -1     -4

*       a1       a2       a3       a4       a5       a6          b
*    ───────  ───────  ───────  ───────  ───────  ───────     ───────
        1       0        0        0        0        0          -0.01
        1       0.63     0.39     0.25     0.16     0.10        0.61
        1       1.26     1.58     1.98     2.49     3.13        0.91
        1       1.88     3.55     6.70    12.62    23.80        0.99
        1       2.51     6.32    15.88    39.90   100.28        0.60
        1       3.14     9.87    31.01    97.41   306.02        0.02

```

```txt

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ equations ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

*     a1   a2   a3     b
*    ───  ───  ───    ───
      1    2    3     14
      2    1    3     13
      3   -2   -1     -4

═════════════════════════════════solution══════════════════════════════════

                               x[1] =    1
                               x[2] =    2
                               x[3] =    3

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ equations ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

*       a1       a2       a3       a4       a5       a6          b
*    ───────  ───────  ───────  ───────  ───────  ───────     ───────
        1       0        0        0        0        0          -0.01
        1       0.63     0.39     0.25     0.16     0.10        0.61
        1       1.26     1.58     1.98     2.49     3.13        0.91
        1       1.88     3.55     6.70    12.62    23.80        0.99
        1       2.51     6.32    15.88    39.90   100.28        0.60
        1       3.14     9.87    31.01    97.41   306.02        0.02

═════════════════════════════════solution══════════════════════════════════

                               x[1] =   -0.01
                               x[2] =    1.6027904
                               x[3] =   -1.6132031
                               x[4] =    1.2454941
                               x[5] =   -0.49098972
                               x[6] =    0.065760696

```



### version 3

This is the same as version 2, but in addition, it also shows the residuals.

Code was added to this program version to keep a copy of the original   '''A.i.k'''   and   '''B.#'''   arrays   (for calculating the residuals).

Also added was rounding the residual numbers to zero if the number of significant decimal digits was
less or equal to 5% of the number of significant fractional decimal digits   (in this case, 5% of 1,000 digits for the decimal fraction).

```rexx
/*REXX program solves   Ax=b   with Gaussian elimination  and  backwards  substitution. */
numeric digits 1000                              /*heavy─duty decimal digits precision. */
parse arg iFID .                                 /*obtain optional argument from the CL.*/
if iFID=='' | iFID=="," then iFID= 'GAUSS_E.DAT' /*Not specified?  Then use the default.*/
pad=left('', 23)                                 /*used for indenting residual numbers. */
     do rec=1    while lines(iFID) \== 0         /*read the equation sets.              */
     #=0                                         /*the number of equations  (so far).   */
         do $=1  while lines(iFID) \== 0         /*process the equation.                */
         z=linein(iFID);    if z=''  then leave  /*Is this a blank line?    end─of─data.*/
         if $==1  then do;  say;     say center(' equations ', 75, "▓");        say
                       end                       /* [↑]  if 1st equation, then show hdr.*/
         say z                                   /*display an equation to the terminal. */
         if left(space(z), 1)=='*'  then iterate /*Is this a comment?    Then ignore it.*/
         #=# + 1;      n=words(z) - 1            /*assign equation #; calculate # items.*/
           do e=1  for n;     a.#.e= word(z, e);     oa.#.e= a.#.e
           end   /*e*/                           /* [↑]  process  A  numbers; save orig.*/
         b.#=word(z, n + 1);  ob.#=b.#           /* ◄───    "     B     "       "    "  */
         end     /*$*/
     if #\==0  then call Gauss_elim              /*Not zero?  Then display the results. */
     say
         do   i=1  for n;  r=0                   /*display the residuals to the terminal*/
           do j=1  for n;  r=r  +  oa.i.j * x.j  /*  ┌───◄ don't display a fraction  if */
           end   /*j*/                           /*  ↓     res ≤ 5% of significant digs.*/
         r=format(r - ob.i, , digits() - digits() * 0.05 % 1 ,  0) / 1  /*should be tiny*/
         say pad 'residual['right(i, length(n) )"] = " left('', r>=0) r /*right justify.*/
         end     /*i*/
     end         /*rec*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Gauss_elim:                   do     j=1  for n;   jp=j + 1
                                do   i=jp  to n;   _=a.j.j / a.i.j
                                  do k=jp  to n;   a.i.k=a.j.k   -   _ * a.i.k
                                  end   /*k*/
                                b.i=b.j   -   _ * b.i
                                end     /*i*/
                              end       /*j*/
            x.n=b.n / a.n.n
                              do   j=n-1  to 1  by -1;   _=0
                                do i=j+1  to n;    _=_   +   a.j.i * x.i
                                end     /*i*/
                              x.j=(b.j - _) / a.j.j
                              end       /*j*/    /* [↑]  uses backwards substitution.   */
            say
            numeric digits 8                     /*for the display,  only use 8 digits. */
            say center('solution', 75, "═"); say /*a title line for articulated output. */
                   do o=1  for n;   say right('x['o"] = ", 38)   left('', x.o>=0)    x.o/1
                   end   /*o*/
            return
```

```txt

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ equations ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

*     a1   a2   a3     b
*    ───  ───  ───    ───
      1    2    3     14
      2    1    3     13
      3   -2   -1     -4

═════════════════════════════════solution══════════════════════════════════

                               x[1] =    1
                               x[2] =    2
                               x[3] =    3

                        residual[1] =    0
                        residual[2] =    0
                        residual[3] =    0

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ equations ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

*       a1       a2       a3       a4       a5       a6          b
*    ───────  ───────  ───────  ───────  ───────  ───────     ───────
        1       0        0        0        0        0          -0.01
        1       0.63     0.39     0.25     0.16     0.10        0.61
        1       1.26     1.58     1.98     2.49     3.13        0.91
        1       1.88     3.55     6.70    12.62    23.80        0.99
        1       2.51     6.32    15.88    39.90   100.28        0.60
        1       3.14     9.87    31.01    97.41   306.02        0.02

═════════════════════════════════solution══════════════════════════════════

                               x[1] =   -0.01
                               x[2] =    1.6027904
                               x[3] =   -1.6132031
                               x[4] =    1.2454941
                               x[5] =   -0.49098972
                               x[6] =    0.065760696

                        residual[1] =    0
                        residual[2] =    0
                        residual[3] =    0
                        residual[4] =    0
                        residual[5] =    0
                        residual[6] =    0

```



## Ruby


```ruby

require 'bigdecimal/ludcmp'
include LUSolve

BigDecimal::limit(30)

a = [1.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     1.00, 0.63, 0.39, 0.25, 0.16, 0.10,
     1.00, 1.26, 1.58, 1.98, 2.49, 3.13,
     1.00, 1.88, 3.55, 6.70, 12.62, 23.80,
     1.00, 2.51, 6.32, 15.88, 39.90, 100.28,
     1.00, 3.14, 9.87, 31.01, 97.41, 306.02].map{|i|BigDecimal(i,16)}
b = [-0.01, 0.61, 0.91, 0.99, 0.60, 0.02].map{|i|BigDecimal(i,16)}

n = 6
zero = BigDecimal("0.0")
one  = BigDecimal("1.0")

lusolve(a, b, ludecomp(a, n, zero,one), zero).each{|v| puts v.to_s('F')[0..20]}
```

```txt

-0.01
1.6027903945021135753
-1.613203059905560094
1.2454941213714351826
-0.490989719584656871
0.0657606961752318825

```



## Sidef

Uses the '''rref(A)''' function from [https://rosettacode.org/wiki/Reduced_row_echelon_form#Sidef Reduced row echelon form].
```ruby
func gauss_jordan_solve (a, b) {

    var A = gather {
        ^b -> each {|i| take(a[i] + b[i]) }
    }

    rref(A).map{ .last }
}

var a = [
    [ 1.00, 0.00, 0.00,  0.00,  0.00,   0.00 ],
    [ 1.00, 0.63, 0.39,  0.25,  0.16,   0.10 ],
    [ 1.00, 1.26, 1.58,  1.98,  2.49,   3.13 ],
    [ 1.00, 1.88, 3.55,  6.70, 12.62,  23.80 ],
    [ 1.00, 2.51, 6.32, 15.88, 39.90, 100.28 ],
    [ 1.00, 3.14, 9.87, 31.01, 97.41, 306.02 ],
]

var b = [ -0.01, 0.61, 0.91, 0.99, 0.60, 0.02 ]

var G = gauss_jordan_solve(a, b)
say G.map { "%27s" % .as_rat }.join("\n")
```

```txt

                     -1/100
  655870882787/409205648497
 -660131804286/409205648497
  509663229635/409205648497
 -200915766608/409205648497
   26909648324/409205648497

```



## Rust


```rust

// using a Vec<f32> might be a better idea
// for now, let us create a fixed size array
// of size:
const SIZE: usize = 6;

pub fn eliminate(mut system: [[f32; SIZE+1]; SIZE]) -> Option<Vec<f32>> {
    // produce the row reduced echelon form
    //
    // for every row...
    for i in 0..SIZE-1 {
        // for every column in that row...
        for j in i..SIZE-1 {
            if system[i][i] == 0f32 {
                continue;
            } else {
                // reduce every element under that element to 0
                let factor = system[j + 1][i] as f32 / system[i][i] as f32;
                for k in i..SIZE+1 {
                    // potential optimization: set every element to zero, instead of subtracting
                    // i think subtraction helps showcase the process better
                    system[j + 1][k] -= factor * system[i][k] as f32;
                }
            }
        }
    }

    // produce gaussian eliminated array
    //
    // the process follows a similar pattern
    // but this one reduces the upper triangular
    // elements
    for i in (1..SIZE).rev() {
        if system[i][i] == 0f32 {
            continue;
        } else {
            for j in (1..i+1).rev() {
                let factor = system[j - 1][i] as f32 / system[i][i] as f32;
                for k in (0..SIZE+1).rev() {
                    system[j - 1][k] -= factor * system[i][k] as f32;
                }
            }
        }
    }

    // produce solutions through back substitution
    let mut solutions: Vec<f32> = vec![];
    for i in 0..SIZE {
        if system[i][i] == 0f32 {
            return None;
        }
        else {
            system[i][SIZE] /= system[i][i] as f32;
            system[i][i] = 1f32;
            println!("X{} = {}", i + 1, system[i][SIZE]);
            solutions.push(system[i][SIZE])
        }
    }
    return Some(solutions);
}

#[cfg(test)]
mod tests {
    use super::*;
    // sample run of the program
    #[test]
    fn eliminate_seven_by_six() {
        let system: [[f32; SIZE +1]; SIZE] = [
            [1.00 , 0.00 , 0.00 , 0.00  , 0.00  , 0.00   , -0.01 ] ,
            [1.00 , 0.63 , 0.39 , 0.25  , 0.16  , 0.10   , 0.61  ] ,
            [1.00 , 1.26 , 1.58 , 1.98  , 2.49  , 3.13   , 0.91  ] ,
            [1.00 , 1.88 , 3.55 , 6.70  , 12.62 , 23.80  , 0.99  ] ,
            [1.00 , 2.51 , 6.32 , 15.88 , 39.90 , 100.28 , 0.60  ] ,
            [1.00 , 3.14 , 9.87 , 31.01 , 97.41 , 306.02 , 0.02  ]
        ] ;
        let solutions = eliminate(system).unwrap();
        assert_eq!(6, solutions.len());
        let assert_solns = vec![-0.01, 1.60278, -1.61320, 1.24549, -0.49098, 0.06576];
        for (ans, key) in solutions.iter().zip(assert_solns.iter()) {
            if (ans - key).abs() > 1E-4 { panic!("Test Failed!") }
        }
    }
}

```



## Stata


###  Gaussian elimination

This implementation computes also the determinant of the matrix A, as it requires only a few operations. The matrix B is overwritten with the solution of the system, and A is overwritten with garbage.


```stata
void gauss(real matrix a, real matrix b, real scalar det) {
	real scalar i,j,n,s
	real vector js

	det = 1
	n = rows(a)
	for (i=1; i<n; i++) {
		maxindex(abs(a[i::n,i]), 1, js=., .)
		j = js[1]+i-1
		if (j!=i) {
			a[(i\j),i..n] = a[(j\i),i..n]
			b[(i\j),.] = b[(j\i),.]
			det = -det
		}
		for (j=i+1; j<=n; j++) {
			s = a[j,i]/a[i,i]
			a[j,i+1..n] = a[j,i+1..n]-s*a[i,i+1..n]
			b[j,.] = b[j,.]-s*b[i,.]
		}
	}

	for (i=n; i>=1; i--) {
		for (j=i+1; j<=n; j++) {
			b[i,.] = b[i,.]-a[i,j]*b[j,.]
		}
		b[i,.] = b[i,.]/a[i,i]
		det = det*a[i,i]
	}
}
```



###  LU decomposition and backsubstitution



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

void luback(real matrix l, real matrix u, real vector p, real matrix y) {
	real scalar i,j,n

	n = rows(y)
	y = y[p,.]
	for (i=1; i<=n; i++) {
		for (j=1; j<i; j++) {
			y[i,.] = y[i,.]-l[i,j]*y[j,.]
		}
		/*y[i,.] = y[i,.]/l[i,i]*/
	}

	for (i=n; i>=1; i--) {
		for (j=i+1; j<=n; j++) {
			y[i,.] = y[i,.]-u[i,j]*y[j,.]
		}
		y[i,.] = y[i,.]/u[i,i]
	}
}
```



###  Example

Here we are computing the inverse of a 3x3 matrix (which happens to be a magic square), using both methods.


```stata
: gauss(a=(2,9,4\7,5,3\6,1,8),b=I(3),det=.)

: b
                  1              2              3
    +----------------------------------------------+
  1 |  -.1027777778    .1888888889   -.0194444444  |
  2 |   .1055555556    .0222222222   -.0611111111  |
  3 |   .0638888889   -.1444444444    .1472222222  |
    +----------------------------------------------+

: ludec(a=(2,9,4\7,5,3\6,1,8),l=.,u=.,p=.)

: luback(l,u,p,y=I(3))

: y
                  1              2              3
    +----------------------------------------------+
  1 |  -.1027777778    .1888888889   -.0194444444  |
  2 |   .1055555556    .0222222222   -.0611111111  |
  3 |   .0638888889   -.1444444444    .1472222222  |
    +----------------------------------------------+
```



## Tcl

```tcl
package require math::linearalgebra

set A {
    {1.00  0.00  0.00  0.00  0.00   0.00}
    {1.00  0.63  0.39  0.25  0.16   0.10}
    {1.00  1.26  1.58  1.98  2.49   3.13}
    {1.00  1.88  3.55  6.70 12.62  23.80}
    {1.00  2.51  6.32 15.88 39.90 100.28}
    {1.00  3.14  9.87 31.01 97.41 306.02}
}
set b {-0.01 0.61 0.91 0.99 0.60 0.02}
puts -nonewline [math::linearalgebra::show [math::linearalgebra::solveGauss $A $b] "%.2f"]
```

```txt

-0.01
1.60
-1.61
1.25
-0.49
0.07

```


=={{header|TI-83 BASIC}}==
The '''rref()''' function performs reduced row-echelon form using Gaussian elimination
on a n*(n+1) matrix. The (n+1)th column receives the resulting vector.
The n*n maxtrix is set to 0 and the pivots are set to 1.

The '''Matr>List()''' subroutine extracts the (n+1)th column to a list.

The matrix can be more easily entered by the '''matrix editor'''.

On TI-83 or TI-84, another way to solve this task is to use the '''PlySmlt2''' internal apps and choose
"simult equ solver" with 6 equations and 6 unknowns.

```ti83b
[[   1.00   0.00   0.00   0.00   0.00   0.00  -0.01]
 [   1.00   0.63   0.39   0.25   0.16   0.10   0.61]
 [   1.00   1.26   1.58   1.98   2.49   3.13   0.91]
 [   1.00   1.88   3.55   6.70  12.62  23.80   0.99]
 [   1.00   2.51   6.32  15.88  39.90 100.28   0.60]
 [   1.00   3.14   9.87  31.01  97.41 306.02   0.02]]→[A]
Matr>List(rref([A]),7,L1)
L1
```

```txt

{-.01 1.602790395 -1.61320306 1.245494121 -.4909897196 .0657606962}

```



## VBA

```vb
'Option Base 1
Private Function gauss_eliminate(a As Variant, b As Variant) As Variant
    Dim n As Integer: n = UBound(b)
    Dim tmp As Variant, m As Integer, mx As Variant
    For col = 1 To n
        m = col
        mx = a(m, m)
        For i = col + 1 To n
            tmp = Abs(a(i, col))
            If tmp > mx Then
                m = i
                mx = tmp
            End If
        Next i
        If col <> m Then
            For j = 1 To UBound(a, 2)
                tmp = a(col, j)
                a(col, j) = a(m, j)
                a(m, j) = tmp
            Next j
            tmp = b(col)
            b(col) = b(m)
            b(m) = tmp
        End If
        For i = col + 1 To n
            tmp = a(i, col) / a(col, col)
            For j = col + 1 To n
                a(i, j) = a(i, j) - tmp * a(col, j)
            Next j
            a(i, col) = 0
            b(i) = b(i) - tmp * b(col)
        Next i
    Next col
    Dim x() As Variant
    ReDim x(n)
    For col = n To 1 Step -1
        tmp = b(col)
        For j = n To col + 1 Step -1
            tmp = tmp - x(j) * a(col, j)
        Next j
        x(col) = tmp / a(col, col)
    Next col
    gauss_eliminate = x
End Function
Public Sub main()
    a = [{1.00, 0.00, 0.00,  0.00,  0.00,   0.00; 1.00, 0.63, 0.39,  0.25,  0.16,   0.10; 1.00, 1.26, 1.58,  1.98,  2.49,   3.13; 1.00, 1.88, 3.55,  6.70, 12.62,  23.80; 1.00, 2.51, 6.32, 15.88, 39.90, 100.28; 1.00, 3.14, 9.87, 31.01, 97.41, 306.02}]
    b = [{-0.01, 0.61, 0.91,  0.99,  0.60,   0.02}]
    Dim s() As String, x() As Variant
    ReDim s(UBound(b)), x(UBound(b))
    Debug.Print "(";
    x = gauss_eliminate(a, b)
    For i = 1 To UBound(x)
        s(i) = CStr(x(i))
    Next i
    t = Join(s, ", ")
    Debug.Print t; ")"
End Sub
```
```txt
(-0.01, 1.60279039450209, -1.61320305990548, 1.24549412137136, -0.490989719584628, 0.065760696175228)
```


## VBScript


```vb
' Gaussian elimination - VBScript
    const n=6
    dim a(6,6),b(6),x(6),ab
    ab=array(   1   ,   0   ,   0   ,   0   ,   0   ,   0   ,  -0.01, _
                1   ,   0.63,   0.39,   0.25,   0.16,   0.10,   0.61, _
                1   ,   1.26,   1.58,   1.98,   2.49,   3.13,   0.91, _
                1   ,   1.88,   3.55,   6.70,  12.62,  23.80,   0.99, _
                1   ,   2.51,   6.32,  15.88,  39.90, 100.28,   0.60, _
                1   ,   3.14,   9.87,  31.01,  97.41, 306.02,   0.02)
    k=-1
    for i=1 to n
        buf=""
        for j=1 to n+1
            k=k+1
            if j<=n then
                a(i,j)=ab(k)
            else
                b(i)=ab(k)
            end if
            buf=buf&right(space(8)&formatnumber(ab(k),2),8)&" "
        next
        wscript.echo buf
    next
    for j=1 to n
        for i=j+1 to n
            w=a(j,j)/a(i,j)
            for k=j+1 to n
                a(i,k)=a(j,k)-w*a(i,k)
            next
            b(i)=b(j)-w*b(i)
        next
    next
    x(n)=b(n)/a(n,n)
    for j=n-1 to 1 step -1
        w=0
        for i=j+1 to n
            w=w+a(j,i)*x(i)
        next
        x(j)=(b(j)-w)/a(j,j)
    next
    wscript.echo "solution"
    buf=""
    for i=1 to n
        buf=buf&right(space(8)&formatnumber(x(i),2),8)&vbcrlf
    next
    wscript.echo buf
```

```txt

   -0,01
    1,60
   -1,61
    1,25
   -0,49
    0,07

```



## zkl

Using the GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
a:=GSL.Matrix(6,6).set(
   1.00, 0.00, 0.00,  0.00,  0.00,   0.00,
   1.00, 0.63, 0.39,  0.25,  0.16,   0.10,
   1.00, 1.26, 1.58,  1.98,  2.49,   3.13,
   1.00, 1.88, 3.55,  6.70, 12.62,  23.80,
   1.00, 2.51, 6.32, 15.88, 39.90, 100.28,
   1.00, 3.14, 9.87, 31.01, 97.41, 306.02);
b:=GSL.VectorFromData(-0.01, 0.61, 0.91,  0.99,  0.60,   0.02);
x:=a.AxEQb(b);
x.format(8,5).println();
```

```txt

-0.01000, 1.60279,-1.61320, 1.24549,-0.49099, 0.06576

```

Or, using lists:
```zkl
fcn gaussEliminate(a,b){  // modifies a&b --> vector
   n:=b.len();
   foreach dia in ([0..n-1]){
      maxRow:=dia; max:=a[dia][dia];
      foreach row in ([dia+1 .. n-1]){
         if((tmp:=a[row][dia].abs()) > max){ maxRow=row; max=tmp; }
      }
      a.swap(dia,maxRow); b.swap(dia,maxRow);  // swap rows
      foreach row in ([dia+1 .. n-1]){
         ar:=a[row]; ad:=a[dia]; tmp:=ar[dia] / ad[dia];
	 foreach col in ([dia+1 .. n-1]){ ar[col]-=tmp*ad[col]; }
	 ar[dia]=0.0;
	 b[row]-=tmp*b[dia];
      }
   }
   x:=(0).pump(n,List().write);  // -->list filled with garbage
   foreach row in ([n-1 .. 0,-1]){
      tmp:=b[row]; ar:=a[row];
      foreach j in ([n-1 .. row+1,-1]){ tmp-=x[j]*ar[j]; }
      x[row]=tmp/a[row][row];
   }
   x
}
```


```zkl
a:=List( List(1.00, 0.00, 0.00,  0.00,  0.00, 0.00,),
         List(1.00, 0.63, 0.39,  0.25,  0.16, 0.10,),
         List(1.00, 1.26, 1.58,  1.98,  2.49, 3.13,),
         List(1.00, 1.88, 3.55,  6.70, 12.62, 23.80,),
         List(1.00, 2.51, 6.32, 15.88, 39.90, 100.28,),
         List(1.00, 3.14, 9.87, 31.01, 97.41, 306.02) );
b:=List( -0.01, 0.61, 0.91, 0.99, 0.60, 0.02 );
gaussEliminate(a,b).println();
```

```txt
L(-0.01,1.60279,-1.6132,1.24549,-0.49099,0.0657607)
```
