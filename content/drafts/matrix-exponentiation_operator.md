+++
title = "Matrix-exponentiation operator"
description = ""
date = 2019-09-14T11:55:48Z
aliases = []
[extra]
id = 2604
[taxonomies]
categories = []
tags = []
+++

{{task|Matrices}}Most programming languages have a built-in implementation of exponentiation for integers and reals only.
Demonstrate how to implement matrix exponentiation as an operator.
{{Omit From|Java}}

## Ada

This is a generic solution for any natural power exponent. It will work with any type that has +,*, additive and multiplicative 0s. The implementation factors out powers A<sup>2<sup>n</sup></sup>:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Matrix is
   generic
      type Element is private;
      Zero : Element;
      One  : Element;
      with function "+" (A, B : Element) return Element is <>;
      with function "*" (A, B : Element) return Element is <>;
      with function Image (X : Element) return String is <>;
   package Matrices is
      type Matrix is array (Integer range <>, Integer range <>) of Element;
      function "*" (A, B : Matrix) return Matrix;
      function "**" (A : Matrix; Power : Natural) return Matrix;
      procedure Put (A : Matrix);
   end Matrices;

   package body Matrices is
      function "*" (A, B : Matrix) return Matrix is
         R   : Matrix (A'Range (1), B'Range (2));
         Sum : Element := Zero;
      begin
         for I in R'Range (1) loop
            for J in R'Range (2) loop
               Sum := Zero;
               for K in A'Range (2) loop
                  Sum := Sum + A (I, K) * B (K, J);
               end loop;
               R (I, J) := Sum;
            end loop;
         end loop;
         return R;
      end "*";

      function "**" (A : Matrix; Power : Natural) return Matrix is
      begin
         if Power = 1 then
            return A;
         end if;
         declare
            R : Matrix (A'Range (1), A'Range (2)) := (others => (others => Zero));
            P : Matrix  := A;
            E : Natural := Power;
         begin
            for I in P'Range (1) loop -- R is identity matrix
               R (I, I) := One;
            end loop;
            if E = 0 then
               return R;
            end if;
            loop
               if E mod 2 /= 0 then
                  R := R * P;
               end if;
               E := E / 2;
               exit when E = 0;
               P := P * P;
            end loop;
            return R;
         end;
      end "**";

      procedure Put (A : Matrix) is
      begin
         for I in A'Range (1) loop
            for J in A'Range (1) loop
               Put (Image (A (I, J)));
            end loop;
            New_Line;
         end loop;
      end Put;
   end Matrices;

   package Integer_Matrices is new Matrices (Integer, 0, 1, Image => Integer'Image);
   use Integer_Matrices;

   M : Matrix (1..2, 1..2) := ((3,2),(2,1));
begin
   Put_Line ("M =");       Put (M);
   Put_Line ("M**0 =");    Put (M**0);
   Put_Line ("M**1 =");    Put (M**1);
   Put_Line ("M**2 =");    Put (M**2);
   Put_Line ("M*M =");     Put (M*M);
   Put_Line ("M**3 =");    Put (M**3);
   Put_Line ("M*M*M =");   Put (M*M*M);
   Put_Line ("M**4 =");    Put (M**4);
   Put_Line ("M*M*M*M ="); Put (M*M*M*M);
   Put_Line ("M**10 =");   Put (M**10);
   Put_Line ("M*M*M*M*M*M*M*M*M*M ="); Put (M*M*M*M*M*M*M*M*M*M);
end Test_Matrix;
```

Sample output:

```txt

M =
 3 2
 2 1
M**0 =
 1 0
 0 1
M**1 =
 3 2
 2 1
M**2 =
 13 8
 8 5
M*M =
 13 8
 8 5
M**3 =
 55 34
 34 21
M*M*M =
 55 34
 34 21
M**4 =
 233 144
 144 89
M*M*M*M =
 233 144
 144 89
M**10 =
 1346269 832040
 832040 514229
M*M*M*M*M*M*M*M*M*M =
 1346269 832040
 832040 514229

```

The following program implements exponentiation of a square Hermitian complex matrix by any complex power. The limitation to be Hermitian is not essential and comes for the limitation of the standard Ada linear algebra library.

```ada
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Complex_Text_IO;          use Ada.Complex_Text_IO;
with Ada.Numerics.Complex_Types;   use Ada.Numerics.Complex_Types;
with Ada.Numerics.Real_Arrays;     use Ada.Numerics.Real_Arrays;
with Ada.Numerics.Complex_Arrays;  use Ada.Numerics.Complex_Arrays;
with Ada.Numerics.Complex_Elementary_Functions; use Ada.Numerics.Complex_Elementary_Functions;

procedure Test_Matrix is
   function "**" (A : Complex_Matrix; Power : Complex) return Complex_Matrix is
      L  : Real_Vector (A'Range (1));
      X  : Complex_Matrix (A'Range (1), A'Range (2));
      R  : Complex_Matrix (A'Range (1), A'Range (2));
      RL : Complex_Vector (A'Range (1));
   begin
      Eigensystem (A, L, X);
      for I in L'Range loop
         RL (I) := (L (I), 0.0) ** Power;
      end loop;
      for I in R'Range (1) loop
         for J in R'Range (2) loop
            declare
               Sum : Complex := (0.0, 0.0);
            begin
               for K in RL'Range (1) loop
                  Sum := Sum + X (K, I) * RL (K) * X (K, J);
               end loop;
               R (I, J) := Sum;
            end;
         end loop;
      end loop;
      return R;
   end "**";
   procedure Put (A : Complex_Matrix) is
   begin
      for I in A'Range (1) loop
        for J in A'Range (1) loop
           Put (A (I, J));
        end loop;
        New_Line;
      end loop;
   end Put;
   M : Complex_Matrix (1..2, 1..2) := (((3.0,0.0),(2.0,1.0)),((2.0,-1.0),(1.0,0.0)));
begin
   Put_Line ("M =");      Put (M);
   Put_Line ("M**0 =");   Put (M**(0.0,0.0));
   Put_Line ("M**1 =");   Put (M**(1.0,0.0));
   Put_Line ("M**0.5 ="); Put (M**(0.5,0.0));
end Test_Matrix;
```

This solution is not tested, because the available version of GNAT GPL Ada compiler (20070405-41) does not provide an implementation of the standard library.


## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: Matrix_algebra.a68'''

```algol68
INT default upb=3;
MODE VEC = [default upb]COSCAL;
MODE MAT = [default upb,default upb]COSCAL;

OP * = (VEC a,b)COSCAL: (
    COSCAL result:=0;
    FOR i FROM LWB a TO UPB a DO result+:= a[i]*b[i] OD;
    result
  );

OP * = (VEC a, MAT b)VEC: ( # overload vec times matrix #
    [2 LWB b:2 UPB b]COSCAL result;
    FOR j FROM 2 LWB b TO 2 UPB b DO result[j]:=a*b[,j] OD;
    result
  );

OP * = (MAT a, b)MAT: ( # overload matrix times matrix #
    [LWB a:UPB a, 2 LWB b:2 UPB b]COSCAL result;
    FOR k FROM LWB result TO UPB result DO result[k,]:=a[k,]*b OD;
    result
  );

OP IDENTITY = (INT upb)MAT:(
  [upb,upb] COSCAL out;
  FOR i TO upb DO
    FOR j TO upb DO
      out[i,j]:= ( i=j |1|0)
    OD
  OD;
  out
);
```
'''File: Matrix-exponentiation_operator.a68'''

```algol68
OP ** = (MAT base, INT exponent)MAT: (
  BITS binary exponent:=BIN exponent ;
  MAT out := IF bits width ELEM binary exponent THEN base ELSE IDENTITY UPB base FI;
  MAT sq:=base;

  WHILE
    binary exponent := binary exponent SHR 1;
    binary exponent /= BIN 0
  DO
    sq := sq * sq;
    IF bits width ELEM binary exponent THEN out := out * sq FI
  OD;
  out
);
```
'''File: test_Matrix-exponentiation_operator.a68'''

```algol68
#!/usr/local/bin/a68g --script #

MODE COSCAL = COMPL;
PR READ "Matrix_algebra.a68" PR
PR READ "Matrix-exponentiation_operator.a68" PR

PROC compl mat printf= (FORMAT scal fmt, MAT m)VOID:(
  FORMAT
    vec math = $n(2 UPB m)(f(scal fmt)"&")$,
    mat math = $"<math>\begin{bmat}"ln(UPB m)(xxf(vec fmt)"\\"l)"\end{bmat}</math>"$,
    vec fmt = $"("n(2 UPB m-1)(f(scal fmt)",")f(scal fmt)")"$,
    mat fmt = $x"("n(UPB m-1)(f(vec fmt)","lxx)f(vec fmt)");"$;
  # finally print the result #
  printf((mat fmt,m))
);

FORMAT scal fmt = $-d.dddd,+d.dddd"i"$; # width of 4, with no leading '+' sign, 1 decimals #
MAT mat=((sqrt(0.5)I0         , sqrt(0.5)I0        , 0I0),
         (        0I-sqrt(0.5),         0Isqrt(0.5), 0I0),
         (        0I0         ,         0I0        , 0I1))

printf(($" mat ** "g(0)":"l$,24));
compl mat printf(scal fmt, mat**24);
print(newline)
```

Output:

```txt

 mat ** 24:
 (( 1.0000+0.0000i, 0.0000+0.0000i, 0.0000+0.0000i),
  ( 0.0000+0.0000i, 1.0000+0.0000i, 0.0000+0.0000i),
  ( 0.0000+0.0000i, 0.0000+0.0000i, 1.0000+0.0000i));

```



## BBC BASIC


```bbcbasic
      DIM matrix(1,1), output(1,1)
      matrix() = 3, 2, 2, 1

      FOR power% = 0 TO 9
        PROCmatrixpower(matrix(), output(), power%)
        PRINT "matrix()^" ; power% " = "
        FOR row% = 0 TO DIM(output(), 1)
          FOR col% = 0 TO DIM(output(), 2)
            PRINT output(row%,col%);
          NEXT
          PRINT
        NEXT row%
      NEXT power%
      END

      DEF PROCmatrixpower(src(), dst(), pow%)
      LOCAL i%
      dst() = 0
      FOR i% = 0 TO DIM(dst(), 1) : dst(i%,i%) = 1 : NEXT
      IF pow% THEN
        FOR i% = 1 TO pow%
          dst() = dst() . src()
        NEXT
      ENDIF
      ENDPROC
```

Output:

```txt

matrix()^0 =
         1         0
         0         1
matrix()^1 =
         3         2
         2         1
matrix()^2 =
        13         8
         8         5
matrix()^3 =
        55        34
        34        21
matrix()^4 =
       233       144
       144        89
matrix()^5 =
       987       610
       610       377
matrix()^6 =
      4181      2584
      2584      1597
matrix()^7 =
     17711     10946
     10946      6765
matrix()^8 =
     75025     46368
     46368     28657
matrix()^9 =
    317811    196418
    196418    121393

```



## Burlesque


```burlesque
blsq ) {{1 1} {1 0}} 10 .*{mm}r[
{{89 55} {55 34}}
```



## C

C doesn't support classes or allow operator overloading. The following is code that defines a function, <tt>SquareMtxPower</tt> that will raise a matrix to a positive integer power.

```c
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct squareMtxStruct {
    int   dim;
    double *cells;
    double **m;
} *SquareMtx;

/* function for initializing row r of a new matrix */
typedef void (*FillFunc)( double *cells, int r, int dim, void *ff_data);

SquareMtx NewSquareMtx( int dim, FillFunc fillFunc, void *ff_data )
{
    SquareMtx sm = malloc(sizeof(struct squareMtxStruct));
    if (sm) {
        int rw;
        sm->dim = dim;
        sm->cells = malloc(dim*dim * sizeof(double));
        sm->m = malloc( dim * sizeof(double *));
        if ((sm->cells != NULL) && (sm->m != NULL)) {
            for (rw=0; rw<dim; rw++) {
                sm->m[rw] = sm->cells + dim*rw;
                fillFunc( sm->m[rw], rw, dim, ff_data );
            }
        }
        else {
            free(sm->m);
            free(sm->cells);
            free(sm);
            printf("Square Matrix allocation failure\n");
            return NULL;
        }
    }
    else {
        printf("Malloc failed for square matrix\n");
    }
    return sm;
}

void ffMatxSquare( double *cells, int rw, int dim, SquareMtx m0 )
{
    int col, ix;
    double sum;
    double *m0rw = m0->m[rw];

    for (col = 0; col < dim; col++) {
        sum = 0.0;
        for (ix=0; ix<dim; ix++)
            sum += m0rw[ix] * m0->m[ix][col];
        cells[col] = sum;
    }
}

void ffMatxMulply( double *cells, int rw, int dim, SquareMtx mplcnds[] )
{
    SquareMtx mleft = mplcnds[0];
    SquareMtx mrigt = mplcnds[1];
    double sum;
    double *m0rw = mleft->m[rw];
    int col, ix;

    for (col = 0; col < dim; col++) {
        sum = 0.0;
        for (ix=0; ix<dim; ix++)
            sum += m0rw[ix] * mrigt->m[ix][col];
        cells[col] = sum;
    }
}

void MatxMul( SquareMtx mr, SquareMtx left, SquareMtx rigt)
{
    int rw;
    SquareMtx mplcnds[2];
    mplcnds[0] = left; mplcnds[1] = rigt;

    for (rw = 0; rw < left->dim; rw++)
        ffMatxMulply( mr->m[rw], rw, left->dim, mplcnds);
}

void ffIdentity( double *cells, int rw, int dim, void *v )
{
    int col;
    for (col=0; col<dim; col++) cells[col] = 0.0;
    cells[rw] = 1.0;
}
void ffCopy(double *cells, int rw, int dim, SquareMtx m1)
{
    int col;
    for (col=0; col<dim; col++) cells[col] = m1->m[rw][col];
}

void FreeSquareMtx( SquareMtx m )
{
    free(m->m);
    free(m->cells);
    free(m);
}

SquareMtx SquareMtxPow( SquareMtx m0, int exp )
{
    SquareMtx v0 = NewSquareMtx(m0->dim, ffIdentity, NULL);
    SquareMtx v1 = NULL;
    SquareMtx base0 = NewSquareMtx( m0->dim, ffCopy, m0);
    SquareMtx base1 = NULL;
    SquareMtx mplcnds[2], t;

    while (exp) {
        if (exp % 2) {
            if (v1)
                MatxMul( v1, v0, base0);
            else  {
                mplcnds[0] = v0; mplcnds[1] = base0;
                v1 = NewSquareMtx(m0->dim, ffMatxMulply, mplcnds);
            }
            {t = v0; v0=v1; v1 = t;}
        }
        if (base1)
            MatxMul( base1, base0, base0);
        else
            base1 = NewSquareMtx( m0->dim, ffMatxSquare, base0);
        t = base0; base0 = base1; base1 = t;
        exp = exp/2;
    }
    if (base0) FreeSquareMtx(base0);
    if (base1) FreeSquareMtx(base1);
    if (v1) FreeSquareMtx(v1);
    return v0;
}

FILE *fout;
void SquareMtxPrint( SquareMtx mtx, const char *mn )
{
    int rw, col;
    int d = mtx->dim;

    fprintf(fout, "%s dim:%d =\n", mn, mtx->dim);

    for (rw=0; rw<d; rw++) {
        fprintf(fout, " |");
        for(col=0; col<d; col++)
            fprintf(fout, "%8.5f ",mtx->m[rw][col] );
        fprintf(fout, " |\n");
    }
    fprintf(fout, "\n");
}

void fillInit( double *cells, int rw, int dim, void *data)
{
    double theta = 3.1415926536/6.0;
    double c1 = cos( theta);
    double s1 = sin( theta);

    switch(rw) {
    case 0:
        cells[0]=c1; cells[1]=s1; cells[2]=0.0;
        break;
    case 1:
        cells[0]=-s1; cells[1]=c1; cells[2]=0;
        break;
    case 2:
        cells[0]=0.0; cells[1]=0.0; cells[2]=1.0;
        break;
    }
}

int main()
{
    SquareMtx m0 = NewSquareMtx( 3, fillInit, NULL);
    SquareMtx m1 = SquareMtxPow( m0, 5);
    SquareMtx m2 = SquareMtxPow( m0, 9);
    SquareMtx m3 = SquareMtxPow( m0, 2);

//  fout = stdout;
    fout = fopen("matrx_exp.txt", "w");
    SquareMtxPrint(m0, "m0"); FreeSquareMtx(m0);
    SquareMtxPrint(m1, "m0^5"); FreeSquareMtx(m1);
    SquareMtxPrint(m2, "m0^9"); FreeSquareMtx(m2);
    SquareMtxPrint(m3, "m0^2"); FreeSquareMtx(m3);
    fclose(fout);

    return 0;
}
```

Output:

```txt
m0 dim:3 =
 | 0.86603  0.50000  0.00000  |
 |-0.50000  0.86603  0.00000  |
 | 0.00000  0.00000  1.00000  |

m0^5 dim:3 =
 |-0.86603  0.50000  0.00000  |
 |-0.50000 -0.86603  0.00000  |
 | 0.00000  0.00000  1.00000  |

m0^9 dim:3 =
 | 0.00000 -1.00000  0.00000  |
 | 1.00000  0.00000  0.00000  |
 | 0.00000  0.00000  1.00000  |

m0^2 dim:3 =
 | 0.50000  0.86603  0.00000  |
 |-0.86603  0.50000  0.00000  |
 | 0.00000  0.00000  1.00000  |

```



## C++

This is an implementation in C++.

```cpp
#include <complex>
#include <cmath>
#include <iostream>
using namespace std;

template<int MSize = 3, class T = complex<double> >
class SqMx {
  typedef T Ax[MSize][MSize];
  typedef SqMx<MSize, T> Mx;

private:
  Ax a;
  SqMx() { }

public:
  SqMx(const Ax &_a) { // constructor with pre-defined array
    for (int r = 0; r < MSize; r++)
      for (int c = 0; c < MSize; c++)
        a[r][c] = _a[r][c];
  }

  static Mx identity() {
    Mx m;
    for (int r = 0; r < MSize; r++)
      for (int c = 0; c < MSize; c++)
        m.a[r][c] = (r == c ? 1 : 0);
    return m;
  }

  friend ostream &operator<<(ostream& os, const Mx &p)
  { // ugly print
    for (int i = 0; i < MSize; i++) {
      for (int j = 0; j < MSize; j++)
        os << p.a[i][j] << ",";
      os << endl;
    }
    return os;
  }

  Mx operator*(const Mx &b) {
    Mx d;
    for (int r = 0; r < MSize; r++)
      for (int c = 0; c < MSize; c++) {
        d.a[r][c] = 0;
        for (int k = 0; k < MSize; k++)
          d.a[r][c] += a[r][k] * b.a[k][c];
      }
    return d;
  }
```

This is the task part.

```cpp
  // C++ does not have a ** operator, instead, ^ (bitwise Xor) is used.
  Mx operator^(int n) {
    if (n < 0)
      throw "Negative exponent not implemented";

    Mx d = identity();
    for (Mx sq = *this; n > 0; sq = sq * sq, n /= 2)
      if (n % 2 != 0)
        d = d * sq;
    return d;
  }
};

typedef SqMx<> M3;
typedef complex<double> creal;

int main() {
  double q = sqrt(0.5);
  creal array[3][3] =
    {{creal(q,  0), creal(q, 0), creal(0, 0)},
     {creal(0, -q), creal(0, q), creal(0, 0)},
     {creal(0,  0), creal(0, 0), creal(0, 1)}};
  M3 m(array);

  cout << "m ^ 23=" << endl
       << (m ^ 23) << endl;

  return 0;
}
```

Output:

```txt

m ^ 23=
(0.707107,0),(0,0.707107),(0,0),
(0.707107,0),(0,-0.707107),(0,0),
(0,0),(0,0),(0,-1),

```


An alternative way would be to implement <tt>operator*=</tt> and conversion from number (giving multiples of the identity matrix) for the matrix and use the generic code from [[Exponentiation operator#C++]] with support for negative exponents removed (or alternatively, implement matrix inversion as well, implement /= in terms of it, and use the generic code unchanged). Note that the algorithm used there is much faster as well.


## C#


```c#
using System;
using System.Collections;
using System.Collections.Generic;
using static System.Linq.Enumerable;

public static class MatrixExponentation
{
    public static double[,] Identity(int size) {
        double[,] matrix = new double[size, size];
        for (int i = 0; i < size; i++) matrix[i, i] = 1;
        return matrix;
    }

    public static double[,] Multiply(this double[,] left, double[,] right) {
        if (left.ColumnCount() != right.RowCount()) throw new ArgumentException();
        double[,] m = new double[left.RowCount(), right.ColumnCount()];
        foreach (var (row, column) in from r in Range(0, m.RowCount()) from c in Range(0, m.ColumnCount()) select (r, c)) {
            m[row, column] = Range(0, m.RowCount()).Sum(i => left[row, i] * right[i, column]);
        }
        return m;
    }

    public static double[,] Pow(this double[,] matrix, int exp) {
        if (matrix.RowCount() != matrix.ColumnCount()) throw new ArgumentException("Matrix must be square.");
        double[,] accumulator = Identity(matrix.RowCount());
        for (int i = 0; i < exp; i++) {
            accumulator = accumulator.Multiply(matrix);
        }
        return accumulator;
    }

    private static int RowCount(this double[,] matrix) => matrix.GetLength(0);
    private static int ColumnCount(this double[,] matrix) => matrix.GetLength(1);

    private static void Print(this double[,] m) {
        foreach (var row in Rows()) {
            Console.WriteLine("[ " + string.Join("   ", row) + " ]");
        }
        Console.WriteLine();

        IEnumerable<IEnumerable<double>> Rows() =>
            Range(0, m.RowCount()).Select(row => Range(0, m.ColumnCount()).Select(column => m[row, column]));
    }

    public static void Main() {
        var matrix = new double[,] {
            { 3, 2 },
            { 2, 1 }
        };

        matrix.Pow(0).Print();
        matrix.Pow(1).Print();
        matrix.Pow(2).Print();
        matrix.Pow(3).Print();
        matrix.Pow(4).Print();
        matrix.Pow(50).Print();
    }

}
```

{{out}}
<pre style="height:30ex;overflow:scroll">
[ 1   0 ]
[ 0   1 ]

[ 3   2 ]
[ 2   1 ]

[ 13   8 ]
[ 8   5 ]

[ 55   34 ]
[ 34   21 ]

[ 233   144 ]
[ 144   89 ]

[ 1.61305314249046E+31   9.9692166771893E+30 ]
[ 9.9692166771893E+30   6.16131474771528E+30 ]
```



## Common Lisp

This Common Lisp implementation uses 2D Arrays to represent matrices, and checks to make sure that the arrays are the right dimensions for multiplication and square for exponentiation.

```lisp
(defun multiply-matrices (matrix-0 matrix-1)
  "Takes two 2D arrays and returns their product, or an error if they cannot be multiplied"
  (let* ((m0-dims (array-dimensions matrix-0))
         (m1-dims (array-dimensions matrix-1))
         (m0-dim (length m0-dims))
         (m1-dim (length m1-dims)))
    (if (or (/= 2 m0-dim) (/= 2 m1-dim))
        (error "Array given not a matrix")
        (let ((m0-rows (car m0-dims))
              (m0-cols (cadr m0-dims))
              (m1-rows (car m1-dims))
              (m1-cols (cadr m1-dims)))
          (if (/= m0-cols m1-rows)
              (error "Incompatible dimensions")
              (do ((rarr (make-array (list m0-rows m1-cols)
                                     :initial-element 0) rarr)
                   (n 0 (if (= n (1- m0-cols)) 0 (1+ n)))
                   (cc 0 (if (= n (1- m0-cols))
                             (if (/= cc (1- m1-cols))
                                 (1+ cc) 0) cc))
                   (cr 0 (if (and (= (1- m0-cols) n)
                                  (= (1- m1-cols) cc))
                             (1+ cr)
                             cr)))
                  ((= cr m0-rows) rarr)
                (setf (aref rarr cr cc)
                      (+ (aref rarr cr cc)
                         (* (aref matrix-0 cr n)
                            (aref matrix-1 n cc))))))))))

(defun matrix-identity (dim)
  "Creates a new identity matrix of size dim*dim"
  (do ((rarr (make-array (list dim dim)
                         :initial-element 0) rarr)
       (n 0 (1+ n)))
      ((= n dim) rarr)
    (setf (aref rarr n n) 1)))

(defun matrix-expt (matrix exp)
  "Takes the first argument (a matrix) and multiplies it by itself exp times"
  (let* ((m-dims (array-dimensions matrix))
         (m-rows (car m-dims))
         (m-cols (cadr m-dims)))
    (cond
      ((/= m-rows m-cols) (error "Non-square matrix"))
      ((zerop exp) (matrix-identity m-rows))
      ((= 1 exp) (do ((rarr (make-array (list m-rows m-cols)) rarr)
                      (cc 0 (if (= cc (1- m-cols))
                                0
                                (1+ cc)))
                      (cr 0 (if (= cc (1- m-cols))
                                (1+ cr)
                                cr)))
                     ((= cr m-rows) rarr)
                   (setf (aref rarr cr cc) (aref matrix cr cc))))
      ((zerop (mod exp 2)) (let ((me2 (matrix-expt matrix (/ exp 2))))
                             (multiply-matrices me2 me2)))
      (t (let ((me2 (matrix-expt matrix (/ (1- exp) 2))))
           (multiply-matrices matrix (multiply-matrices me2 me2)))))))
```

Output (note that this lisp implementation uses single-precision floats for decimals by default).  We can also use rationals:
 CL-USER> (setf 5x5-matrix
                (make-array '(5 5)
                            :initial-contents
                            '((0    1 -1   -2    2)
                              (0.4  4  3.2 -3  -10)
                              (4.5 -2  0.5  1    7)
                              (10   1  0    1.5 -2)
                              (4    5 -3   -2    1))))
 #2A((0 1 -1 -2 2)
     (0.4 4 3.2 -3 -10)
     (4.5 -2 0.5 1 7)
     (10 1 0 1.5 -2)
     (4 5 -3 -2 1))
 CL-USER> (matrix-expt 5x5-matrix 3)
 #2A((-163.25 -19.5 92.25 -7.5999985 -184.3)
     (156.6 -412.09998 0.7999954 331.45 597.4)
     (-129.82501 401.25 -66.975 -302.55 -390.15)
     (-148.9 39.25 -5.200001 -67.225006 -7.300003)
     (-495.05 -231.5 310.85 33.0 -328.5))
 CL-USER> (setf 4x4-matrix
                (make-array '(4 4)
                            :initial-contents
                            '(( 1/2 -1/2  4    8)
                              (-3/4  7/3  8/5 -2)
                              (-5   17   20/3 -5/2)
                              ( 3/2 -1   -7/3  6))))
 #2A((1/2 -1/2 4 8) (-3/4 7/3 8/5 -2) (-5 17 20/3 -5/2) (3/2 -1 -7/3 6))
 CL-USER> (matrix-expt 4x4-matrix 3)
 #2A((-233/8 182723/720 757/30 353/6)
     (-73517/480 838241/2160 77789/450 -67537/180)
     (-5315/9 66493/45 90883/135 -54445/36)
     (37033/144 -27374/45 -15515/54 12109/18))


## Chapel


This uses the '*' operator for arrays as defined in [[Matrix_multiplication#Chapel]]

```chapel
proc **(a, e) {
    // create result matrix of same dimensions
    var r:[a.domain] a.eltType;
    // and initialize to identity matrix
    forall ij in r.domain do
        r(ij) = if ij(1) == ij(2) then 1 else 0;

    for 1..e do
        r *= a;

    return r;
}
```


Usage example (like Perl):

```chapel
var m:[1..3, 1..3] int;
m(1,1) = 1; m(1,2) = 2; m(1,3) = 0;
m(2,1) = 0; m(2,2) = 3; m(2,3) = 1;
m(3,1) = 1; m(3,2) = 0; m(3,3) = 0;

config param n = 10;

for i in 0..n do {
    writeln("Order ", i);
    writeln(m ** i, "\n");
}
```


{{out}}
 Order 0
 1 0 0
 0 1 0
 0 0 1

 Order 1
 1 2 0
 0 3 1
 1 0 0

 Order 2
 1 8 2
 1 9 3
 1 2 0

 Order 3
 3 26 8
 4 29 9
 1 8 2

 Order 4
 11 84 26
 13 95 29
 3 26 8

 Order 5
 37 274 84
 42 311 95
 11 84 26

 Order 6
 121 896 274
 137 1017 311
 37 274 84

 Order 7
 395 2930 896
 448 3325 1017
 121 896 274

 Order 8
 1291 9580 2930
 1465 10871 3325
 395 2930 896

 Order 9
 4221 31322 9580
 4790 35543 10871
 1291 9580 2930

 Order 10
 13801 102408 31322
 15661 116209 35543
 4221 31322 9580


## D


```d
import std.stdio, std.string, std.math, std.array, std.algorithm;

struct SquareMat(T = creal) {
    public static string fmt = "%8.3f";
    private alias TM = T[][];
    private TM a;

    public this(in size_t side) pure nothrow @safe
    in {
        assert(side > 0);
    } body {
        a = new TM(side, side);
    }

    public this(in TM m) pure nothrow @safe
    in {
        assert(!m.empty);
        assert(m.all!(row => row.length == m.length)); // Is square.
    } body {
        // 2D dup.
        a.length = m.length;
        foreach (immutable i, const row; m)
            a[i] = row.dup;
    }

    string toString() const @safe {
        return format("<%(%(" ~ fmt ~ ", %)\n %)>", a);
    }

    public static SquareMat identity(in size_t side) pure nothrow @safe {
        auto m = SquareMat(side);
        foreach (immutable r, ref row; m.a)
            foreach (immutable c; 0 .. side)
                row[c] = (r == c) ? 1+0i : 0+0i;
        return m;
    }

    public SquareMat opBinary(string op:"*")(in SquareMat other)
    const pure nothrow @safe in {
        assert (a.length == other.a.length);
    } body {
        immutable side = other.a.length;
        auto d = SquareMat(side);
        foreach (immutable r; 0 .. side)
            foreach (immutable c; 0 .. side) {
                d.a[r][c] = 0+0i;
                foreach (immutable k, immutable ark; a[r])
                    d.a[r][c] += ark * other.a[k][c];
            }
        return d;
    }

    public SquareMat opBinary(string op:"^^")(int n) // The task part.
    const pure nothrow @safe in {
        assert(n >= 0, "Negative exponent not implemented.");
    } body {
        auto sq = SquareMat(this.a);
        auto d = SquareMat.identity(a.length);
        for (; n > 0; sq = sq * sq, n >>= 1)
            if (n & 1)
                d = d * sq;
        return d;
    }
}

void main() {
    alias M = SquareMat!();
    enum real q = 0.5.sqrt;
    immutable m = M([[   q + 0*1.0Li,    q + 0*1.0Li, 0.0L + 0.0Li],
                     [0.0L - q*1.0Li, 0.0L + q*1.0Li, 0.0L + 0.0Li],
                     [0.0L +   0.0Li, 0.0L +   0.0Li, 0.0L + 1.0Li]]);
    M.fmt = "%5.2f";
    foreach (immutable p; [0, 1, 23, 24])
        writefln("m ^^ %d =\n%s", p, m ^^ p);
}
```

{{out}}

```txt
m ^^ 0 =
< 1.00+ 0.00i,  0.00+ 0.00i,  0.00+ 0.00i
  0.00+ 0.00i,  1.00+ 0.00i,  0.00+ 0.00i
  0.00+ 0.00i,  0.00+ 0.00i,  1.00+ 0.00i>
m ^^ 1 =
< 0.71+ 0.00i,  0.71+ 0.00i,  0.00+ 0.00i
  0.00+-0.71i,  0.00+ 0.71i,  0.00+ 0.00i
  0.00+ 0.00i,  0.00+ 0.00i,  0.00+ 1.00i>
m ^^ 23 =
< 0.71+ 0.00i,  0.00+ 0.71i,  0.00+ 0.00i
  0.71+ 0.00i,  0.00+-0.71i,  0.00+ 0.00i
  0.00+ 0.00i,  0.00+ 0.00i,  0.00+-1.00i>
m ^^ 24 =
< 1.00+ 0.00i,  0.00+ 0.00i,  0.00+ 0.00i
  0.00+ 0.00i,  1.00+ 0.00i,  0.00+ 0.00i
  0.00+ 0.00i,  0.00+ 0.00i,  1.00+ 0.00i>
```



## ERRE


```txt

                               10
This example calculates | 3 2 |
                        | 2 1 |

```


```ERRE
PROGRAM MAT_PROD

!$MATRIX

!-----------------
! calculate A[]^N
!-----------------

CONST ORDER=1

DIM A[1,1],B[1,1],ANS[1,1]

BEGIN

DATA(3,2,2,1)
DATA(10)  ! integer power only

FOR I=0 TO ORDER DO
   FOR J=0 TO ORDER DO
      READ(A[I,J])
   END FOR
END FOR

READ(M)  N=M-1

IF N=0 THEN   ! A[]^0=matrice identit…
   for I=0 TO ORDER DO
      B[I,I]=1
   END FOR
 ELSE
   B[]=A[]
   FOR Z=1 TO N DO
      ANS[]=0
      FOR I=0 TO ORDER DO
         FOR J=0 TO ORDER DO
            FOR K=0 TO ORDER DO
               ANS[I,J]=ANS[I,J]+(A[I,K]*B[K,J])
            END FOR
         END FOR
      END FOR
      B[]=ANS[]
  END FOR
END IF

! print answer
  FOR I=0 TO ORDER DO
     FOR J=0 TO ORDER DO
        PRINT(B[I,J],)
     END FOR
     PRINT
  END FOR

END PROGRAM
```

Sample output:

```txt

 1346269   832040
 832040    514229

```



## Factor

There is already a built-in word (<code>m^n</code>) that implements exponentiation. Here is a simple and less efficient implementation.


```factor
USING: kernel math math.matrices sequences ;

: my-m^n ( m n -- m' )
    dup 0 < [ "no negative exponents" throw ] [
        [ drop length identity-matrix ]
        [ swap '[ _ m. ] times ] 2bi
    ] if ;
```


 ( scratchpad ) { { 3 2 } { 2 1 } } 0 my-m^n .
 { { 1 0 } { 0 1 } }
 ( scratchpad ) { { 3 2 } { 2 1 } } 4 my-m^n .
 { { 233 144 } { 144 89 } }


## Fortran

{{works with|Fortran|90 and later}}

```fortran
module matmod
  implicit none

! Overloading the ** operator does not work because the compiler cannot
! differentiate between matrix exponentiation and the elementwise raising
! of an array to a power therefore we define a new operator
  interface operator (.matpow.)
    module procedure matrix_exp
  end interface

contains

function matrix_exp(m, n) result (res)
  real, intent(in)  :: m(:,:)
  integer, intent(in)  :: n
  real :: res(size(m,1),size(m,2))
  integer :: i

  if(n == 0) then
    res = 0
    do i = 1, size(m,1)
      res(i,i) = 1
    end do
    return
  end if

  res = m
  do i = 2, n
    res = matmul(res, m)
  end do

end function matrix_exp
end module matmod

program Matrix_exponentiation
  use matmod
  implicit none

  integer, parameter :: n = 3
  real, dimension(n,n) :: m1, m2
  integer :: i, j

  m1 = reshape((/ (i, i = 1, n*n) /), (/ n, n /), order = (/ 2, 1 /))

  do i = 0, 4
    m2 = m1 .matpow. i
    do j = 1, size(m2,1)
      write(*,*) m2(j,:)
    end do
    write(*,*)
  end do

end program Matrix_exponentiation
```

Output

```txt
      1.00000         0.00000         0.00000
      0.00000         1.00000         0.00000
      0.00000         0.00000         1.00000

      1.00000         2.00000         3.00000
      4.00000         5.00000         6.00000
      7.00000         8.00000         9.00000

      30.0000         36.0000         42.0000
      66.0000         81.0000         96.0000
      102.000         126.000         150.000

      468.000         576.000         684.000
      1062.00         1305.00         1548.00
      1656.00         2034.00         2412.00

      7560.00         9288.00         11016.0
      17118.0         21033.0         24948.0
      26676.0         32778.0         38880.0
```



## GAP


```gap
# Matrix exponentiation is built-in
A := [[0 , 1], [1, 1]];
PrintArray(A);
#   [ [  0,  1 ],
#     [  1,  1 ] ]
PrintArray(A^10);
#   [ [  34,  55 ],
#     [  55,  89 ] ]
```



## Go

{{trans|Kotlin}}


Like some other languages here, Go doesn't have a symbolic operator for numeric exponentiation and even if it did doesn't support operator overloading. We therefore write the exponentiation operation for matrices as an equivalent 'pow' function.

```go
package main

import "fmt"

type vector = []float64
type matrix []vector

func (m1 matrix) mul(m2 matrix) matrix {
    rows1, cols1 := len(m1), len(m1[0])
    rows2, cols2 := len(m2), len(m2[0])
    if cols1 != rows2 {
        panic("Matrices cannot be multiplied.")
    }
    result := make(matrix, rows1)
    for i := 0; i < rows1; i++ {
        result[i] = make(vector, cols2)
        for j := 0; j < cols2; j++ {
            for k := 0; k < rows2; k++ {
                result[i][j] += m1[i][k] * m2[k][j]
            }
        }
    }
    return result
}

func identityMatrix(n int) matrix {
    if n < 1 {
        panic("Size of identity matrix can't be less than 1")
    }
    ident := make(matrix, n)
    for i := 0; i < n; i++ {
        ident[i] = make(vector, n)
        ident[i][i] = 1
    }
    return ident
}

func (m matrix) pow(n int) matrix {
    le := len(m)
    if le != len(m[0]) {
        panic("Not a square matrix")
    }
    switch {
    case n < 0:
        panic("Negative exponents not supported")
    case n == 0:
        return identityMatrix(le)
    case n == 1:
        return m
    }
    pow := identityMatrix(le)
    base := m
    e := n
    for e > 0 {
        if (e & 1) == 1 {
            pow = pow.mul(base)
        }
        e >>= 1
        base = base.mul(base)
    }
    return pow
}

func main() {
    m := matrix{{3, 2}, {2, 1}}
    for i := 0; i <= 10; i++ {
        fmt.Println("** Power of", i, "**")
        fmt.Println(m.pow(i))
        fmt.Println()
    }
}
```


{{out}}

```txt

** Power of 0 **
[[1 0] [0 1]]

** Power of 1 **
[[3 2] [2 1]]

** Power of 2 **
[[13 8] [8 5]]

** Power of 3 **
[[55 34] [34 21]]

** Power of 4 **
[[233 144] [144 89]]

** Power of 5 **
[[987 610] [610 377]]

** Power of 6 **
[[4181 2584] [2584 1597]]

** Power of 7 **
[[17711 10946] [10946 6765]]

** Power of 8 **
[[75025 46368] [46368 28657]]

** Power of 9 **
[[317811 196418] [196418 121393]]

** Power of 10 **
[[1.346269e+06 832040] [832040 514229]]

```



## Haskell


Instead of writing it directly, we can re-use the built-in [[exponentiation operator]] if we declare matrices as an instance of ''Num'', using [[matrix multiplication]] (and addition). For simplicity, we use the inefficient representation as list of lists. Note that we don't check the dimensions (there are several ways to do that on the type-level, for example with phantom types).


```haskell
import Data.List (transpose)

(<+>)
  :: Num a
  => [a] -> [a] -> [a]
(<+>) = zipWith (+)

(<*>)
  :: Num a
  => [a] -> [a] -> a
(<*>) = (sum .) . zipWith (*)

newtype Mat a =
  Mat [[a]]
  deriving (Eq, Show)

instance Num a =>
         Num (Mat a) where
  negate (Mat x) = Mat $ map (map negate) x
  Mat x + Mat y = Mat $ zipWith (<+>) x y
  Mat x * Mat y =
    Mat
      [ [ xs Main.<*> ys -- Main prefix to distinguish fron applicative operator
        | ys <- transpose y ]
      | xs <- x ]
  abs = undefined
  fromInteger _ = undefined -- don't know dimension of the desired matrix
  signum = undefined

-- TEST ----------------------------------------------------------------------
main :: IO ()
main = print $ Mat [[1, 2], [0, 1]] ^ 4
```

{{Out}}

```txt
Mat [[1,8],[0,1]]
```


This will work for matrices over any numeric type, including complex numbers. The implementation of (^) uses the fast binary algorithm for exponentiation.

Note: this implementation does not work for a power of 0.


## J




```j
mp=: +/ .*   NB. Matrix multiplication
pow=: pow0=: 4 : 'mp&x^:y =i.#x'
```


or, from [[j:Essays/Linear Recurrences|the J wiki]], and faster for large exponents:


```j
pow=: pow1=: 4 : 'mp/ mp~^:(I.|.#:y) x'
```


This implements an optimization where the exponent is represented in base 2, and repeated squaring is used to create a list of relevant powers of the base matrix, which are then combined using matrix multiplication.  Note, however, that these two definitions treat a zero exponent differently (m pow0 0 gives an identity matrix whose shape matches m, while m pow1 0 gives a scalar 1).

Example use:

    (3 2,:2 1) pow 3
 55 34
 34 21


## JavaScript

{{works with|SpiderMonkey}} for the <code>print()</code> and <code>''Array''.forEach()</code> functions.

Extends [[Matrix Transpose#JavaScript]] and [[Matrix multiplication#JavaScript]]

```javascript
// IdentityMatrix is a "subclass" of Matrix
function IdentityMatrix(n) {
    this.height = n;
    this.width = n;
    this.mtx = [];
    for (var i = 0; i < n; i++) {
        this.mtx[i] = [];
        for (var j = 0; j < n; j++) {
            this.mtx[i][j] = (i == j ? 1 : 0);
        }
    }
}
IdentityMatrix.prototype = Matrix.prototype;

// the Matrix exponentiation function
// returns a new matrix
Matrix.prototype.exp = function(n) {
    var result = new IdentityMatrix(this.height);
    for (var i = 1; i <= n; i++) {
        result = result.mult(this);
    }
    return result;
}

var m = new Matrix([[3, 2], [2, 1]]);
[0,1,2,3,4,10].forEach(function(e){print(m.exp(e)); print()})
```

output

```txt
1,0
0,1

3,2
2,1

13,8
8,5

55,34
34,21

233,144
144,89

1346269,832040
832040,514229

```



## jq

In this section we define matrix_exp(n) for computing the n-th power of the input matrix, where it is assumed that n is a non-negative integer.

The implementation here can be used with any matrix multiplication function, multiply(A;B), for example as defined at [[Matrix_multiplication#jq]].
Thus matrix_exp(n) could be used with complex-valued matrices.

matrix_exp(n) adopts a "divide-and-conquer" strategy to avoid unnecessarily many matrix multiplications. The implementation uses direct_matrix_exp(n) for small n; this function could be defined as an inner function, but is defined separately first for clarity, and second to simplify timing comparisons, as shown below.

```jq
# produce an array of length n that is 1 at i and 0 elsewhere
def indicator(i;n): [range(0;n) | 0] | .[i] = 1;

# Identity matrix:
def identity(n): reduce range(0;n) as $i ([]; . + [indicator( $i; n )] );

def direct_matrix_exp(n):
  . as $in
  | if n == 0 then identity($in|length)
    else reduce range(1;n) as $i ($in; . as $m | multiply($m; $in))
    end;

def matrix_exp(n):
  if n < 4 then direct_matrix_exp(n)
  else . as $in
  | ((n|2)|floor) as $m
  | matrix_exp($m) as $ans
  | multiply($ans;$ans) as $ans
  | (n - (2 * $m) ) as $residue
  | if $residue == 0 then $ans
    else matrix_exp($residue) as $residue
    | multiply($ans; $residue )
    end
  end;
```

'''Examples'''
The execution speeds of matrix_exp and direct_matrix_exp are compared using a one-eighth-rotation matrix, which
is raised to the 10,000th power.  The direct method turns out to be almost as fast.

```jq
def pi: 4 * (1|atan);

def rotation_matrix(theta):
  [[(theta|cos), (theta|sin)], [-(theta|sin), (theta|cos)]];

def demo_matrix_exp(n):
  rotation_matrix( pi / 4 ) | matrix_exp(n) ;

def demo_direct_matrix_exp(n):
  rotation_matrix( pi / 4 ) | direct_matrix_exp(n) ;
```

'''Results''':

```sh
# For demo_matrix_exp(10000)
$ time jq -n -c -f Matrix-exponentiation_operator.rc
[[1,-1.1102230246251565e-12],[1.1102230246251565e-12,1]]
user	0m0.490s
sys	0m0.008s
```


```sh
# For demo_direct_matrix_exp(10000)
$ time jq -n -c -f Matrix-exponentiation_operator.rc
[[1,-7.849831895612169e-13],[7.849831895612169e-13,1]]
user	0m0.625s
sys	0m0.006s
```



## Jsish

Based on Javascript matrix entries.

Uses module listed in [[Matrix Transpose#Jsish]].  Fails the task spec actually, as Matrix.exp() is implemented as a method, not an operator.


```javascript
/* Matrix exponentiation, in Jsish */
require('Matrix');

if (Interp.conf('unitTest')) {
    var m = new Matrix([[3, 2], [2, 1]]);
;    m;
;    m.exp(0);
;    m.exp(1);
;    m.exp(2);
;    m.exp(4);
;    m.exp(10);
}

/*
=!EXPECTSTART!=
m ==> { height:2, mtx:[ [ 3, 2 ], [ 2, 1 ] ], width:2 }
m.exp(0) ==> { height:2, mtx:[ [ 1, 0 ], [ 0, 1 ] ], width:2 }
m.exp(1) ==> { height:2, mtx:[ [ 3, 2 ], [ 2, 1 ] ], width:2 }
m.exp(2) ==> { height:2, mtx:[ [ 13, 8 ], [ 8, 5 ] ], width:2 }
m.exp(4) ==> { height:2, mtx:[ [ 233, 144 ], [ 144, 89 ] ], width:2 }
m.exp(10) ==> { height:2, mtx:[ [ 1346269, 832040 ], [ 832040, 514229 ] ], width:2 }
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u matrixExponentiation.jsi
[PASS] matrixExponentiation.jsi
```



## Julia

Matrix exponentiation is implemented by the built-in <code>^</code> operator.

```Julia>julia
 [1 1 ; 1 0]^10
2x2 Array{Int64,2}:
 89  55
 55  34
```



## K


```K

/Matrix Exponentiation
/mpow.k
pow: {:[0=y; :({a=/:a:!x}(#x))];a: x; do[y-1; a: x _mul a]; :a}


```

The output of a session is given below:
{{out}}

```txt

K Console - Enter \ for help

  \l mpow

  a:(3 2;2 1)
(3 2
 2 1)
  pow[a;0]
(1 0
 0 1)
  pow[a;1]
(3 2
 2 1)
  pow[a;2]
(13 8
 8 5)
  pow[a;3]
(55 34
 34 21)
  pow[a;4]
(233 144
 144 89)
  pow[a;10]
(1346269 832040
 832040 514229)


```



## Kotlin


```scala
// version 1.1.3

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

fun identityMatrix(n: Int): Matrix {
    require(n >= 1)
    val ident = Matrix(n) { Vector(n) }
    for (i in 0 until n) ident[i][i] = 1.0
    return ident
}

infix fun Matrix.pow(n : Int): Matrix {
    require (n >= 0 && this.size == this[0].size)
    if (n == 0) return identityMatrix(this.size)
    if (n == 1) return this
    var pow = identityMatrix(this.size)
    var base = this
    var e = n
    while (e > 0) {
        if ((e and 1) == 1) pow *= base
        e = e shr 1
        base *= base
    }
    return pow
}

fun printMatrix(m: Matrix, n: Int) {
    println("** Power of $n **")
    for (i in 0 until m.size) println(m[i].contentToString())
    println()
}

fun main(args: Array<String>) {
    val m = arrayOf(
        doubleArrayOf(3.0, 2.0),
        doubleArrayOf(2.0, 1.0)
    )
    for (i in 0..10) printMatrix(m pow i, i)
}
```


{{out}}

```txt

** Power of 0 **
[1.0, 0.0]
[0.0, 1.0]

** Power of 1 **
[3.0, 2.0]
[2.0, 1.0]

** Power of 2 **
[13.0, 8.0]
[8.0, 5.0]

** Power of 3 **
[55.0, 34.0]
[34.0, 21.0]

** Power of 4 **
[233.0, 144.0]
[144.0, 89.0]

** Power of 5 **
[987.0, 610.0]
[610.0, 377.0]

** Power of 6 **
[4181.0, 2584.0]
[2584.0, 1597.0]

** Power of 7 **
[17711.0, 10946.0]
[10946.0, 6765.0]

** Power of 8 **
[75025.0, 46368.0]
[46368.0, 28657.0]

** Power of 9 **
[317811.0, 196418.0]
[196418.0, 121393.0]

** Power of 10 **
[1346269.0, 832040.0]
[832040.0, 514229.0]

```



## Liberty BASIC

There is no native matrix capability. A set of functions is available at http://www.diga.me.uk/RCMatrixFuncs.bas implementing matrices of arbitrary dimension in a string format.

```lb

MatrixD$ ="3, 3,          0.86603,  0.50000,  0.00000,     -0.50000,  0.86603,  0.00000,     0.00000,  0.00000,  1.00000"


print "Exponentiation of a matrix"
call DisplayMatrix MatrixD$
print "         Raised to power 5 ="
MatrixE$ =MatrixToPower$( MatrixD$, 5)
call DisplayMatrix MatrixE$
print "         Raised to power 9 ="
MatrixE$ =MatrixToPower$( MatrixD$, 9)
call DisplayMatrix MatrixE$

```


{{out}}

```txt
Exponentiation of a matrix
| 0.86603 0.50000 0.00000 |
| -0.50000 0.86603 0.00000 |
| 0.00000 0.00000 1.00000 |

Raised to power 5 =
| -0.86604 0.50002 0.00000 |
| -0.50002 -0.86604 0.00000 |
| 0.00000 0.00000 1.00000 |

Raised to power 9 =
| -0.00002 -1.00004 0.00000 |
| 1.00004 -0.00002 0.00000 |
| 0.00000 0.00000 1.00000 |
```



## Lua


```lua
Matrix = {}

function Matrix.new( dim_y, dim_x )
    assert( dim_y and dim_x )

    local matrix = {}
    local metatab = {}
    setmetatable( matrix, metatab )
    metatab.__add = Matrix.Add
    metatab.__mul = Matrix.Mul
    metatab.__pow = Matrix.Pow

    matrix.dim_y = dim_y
    matrix.dim_x = dim_x

    matrix.data = {}
    for i = 1, dim_y do
        matrix.data[i] = {}
    end
    return matrix
end

function Matrix.Show( m )
    for i = 1, m.dim_y do
        for j = 1, m.dim_x do
            io.write( tostring( m.data[i][j] ), " " )
        end
        io.write( "\n" )
    end
end

function Matrix.Add( m, n )
    assert( m.dim_x == n.dim_x and m.dim_y == n.dim_y )

    local r = Matrix.new( m.dim_y, m.dim_x )
    for i = 1, m.dim_y do
        for j = 1, m.dim_x do
            r.data[i][j] = m.data[i][j] + n.data[i][j]
        end
    end
    return r
end

function Matrix.Mul( m, n )
    assert( m.dim_x == n.dim_y )

    local r = Matrix.new( m.dim_y, n.dim_x )
    for i = 1, m.dim_y do
        for j = 1, n.dim_x do
            r.data[i][j] = 0
            for k = 1, m.dim_x do
                r.data[i][j] = r.data[i][j] + m.data[i][k] * n.data[k][j]
            end
        end
    end
    return r
end

function Matrix.Pow( m, p )
    assert( m.dim_x == m.dim_y )

    local r = Matrix.new( m.dim_y, m.dim_x )

    if p == 0 then
        for i = 1, m.dim_y do
            for j = 1, m.dim_x do
                if i == j then
                    r.data[i][j] = 1
                else
                    r.data[i][j] = 0
                end
            end
        end
    elseif p == 1 then
        for i = 1, m.dim_y do
            for j = 1, m.dim_x do
                r.data[i][j] = m.data[i][j]
            end
        end
    else
        r = m
        for i = 2, p do
            r = r * m
        end
    end

    return r
end


m = Matrix.new( 2, 2 )
m.data = { { 1, 2 }, { 3, 4 } }

n = m^4;

Matrix.Show( n )
```


## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
	Class cArray {
		a=(,)
		Function Power(n as integer){
			cArr=This     ' create a copy
			dim new()
			new()=cArr.a   ' get a pointer from a to new()
			Let cArr.a=new()    ' now new() return a copy
			cArr.a*=0  ' make zero all elements
			link cArr.a to v()
			for i=dimension(cArr.a,1,0) to dimension(cArr.a, 1,1) : v(i,i)=1: next i
			while n>0
				let cArr=cArr*this    ' * is the operator "*"
				n--
			end while
			=cArr
		}
		Operator "*"{
			Read cArr
			b=cArr.a
			if dimension(.a)<>2 or dimension(b)<>2 then Error "Need two 2D arrays "
			let a2=dimension(.a,2), b1=dimension(b,1)
			if a2<>b1 then Error "Need columns of first array equal to rows of second array"
			let a1=dimension(.a,1), b2=dimension(b,2)
			let aBase=dimension(.a,1,0)-1, bBase=dimension(b,1,0)-1
			let aBase1=dimension(.a,2,0)-1, bBase1=dimension(b,2,0)-1
			link .a,b to a(), b()  ' change interface for arrays
			dim base 1, c(a1, b2)
			for i=1 to a1 : let ia=i+abase : for j=1 to b2 : let jb=j+bBase1 : for k=1 to a2
			c(i,j)+=a(ia,k+aBase1)*b(k+bBase,jb)
			next k : next j : next i
			\\ redim to base 0
			dim base 0, c(a1, b2)
			.a<=c()
			}
		Module Print {
			link .a to v()
			for i=dimension(.a,1,0) to dimension(.a, 1,1)
			for j=dimension(.a,2,0) to dimension(.a, 2,1)
			print  v(i,j),: next j: print : next i

		}
	Class:
		\\ this module used as constructor, and not returned to final group (user object in M2000)
		Module cArray (r) {
			c=r
			Dim a(r,c)
			For i=0 to r-1 : For j=0 to c-1: Read a(i,j): Next j : Next i
			.a<=a()
		}
	}
	Print "matrix():"
	P=cArray(2,3,2,2,1)
	P.Print
	For i=0 to 9
		Print "matrix()^"+str$(i,0)+"="
		K=P.Power(i)
		K.Print
	next i
}
Checkit

```


{{out}}
<pre style="height:30ex;overflow:scroll">
matrix():
      3      2
      2      1
matrix()^0=
      1      0
      0      1
matrix()^1=
      3      2
      2      1
matrix()^2=
     13      8
      8      5
matrix()^3=
     55     34
     34     21
matrix()^4=
    233    144
    144     89
matrix()^5=
    987    610
    610    377
matrix()^6=
   4181   2584
   2584   1597
matrix()^7=
  17711  10946
  10946   6765
matrix()^8=
  75025  46368
  46368  28657
matrix()^9=
 317811 196418
 196418 121393
</pre >



## Maple

Maple handles matrix powers implicitly with the built-in exponentiation operator:

```Maple>
 M := <<1,2>|<3,4>>;
> M ^ 2;
```

<math>\left[\begin{array}{cc}
 7 & 15 \\
 10 & 22
\end{array}\right]</math>

If you want elementwise powers, you can use the elementwise <code>^~</code> operator:

```Maple>
 M := <<1,2>|<3,4>>;
> M ^~ 2;
```

<math>\left[\begin{array}{cc}
 1 & 9 \\
 4 & 16
\end{array}\right]</math>


## Mathematica

In Mathematica there is an distinction between powering elements wise and as a matrix. So m^2 will give m with each element squared. To do matrix exponentation we use the function MatrixPower. It can handle all types of numbers for the power (integers, floats, rationals, complex) but also symbols for the power, and all types for the matrix (numbers, symbols et cetera), and will always keep the result exact if the matrix and the exponent is exact.

```Mathematica
a = {{3, 2}, {4, 1}};
MatrixPower[a, 0]
MatrixPower[a, 1]
MatrixPower[a, -1]
MatrixPower[a, 4]
MatrixPower[a, 1/2]
MatrixPower[a, Pi]
```

gives back:

<math>
\left(
\begin{array}{cc}
 1 & 0 \\
 0 & 1
\end{array}
\right)
</math>

<math>
\left(
\begin{array}{cc}
 3 & 2 \\
 4 & 1
\end{array}
\right)
</math>

<math>
\left(
\begin{array}{cc}
 -\frac{1}{5} & \frac{2}{5} \\
 \frac{4}{5} & -\frac{3}{5}
\end{array}
\right)
</math>

<math>
\left(
\begin{array}{cc}
 417 & 208 \\
 416 & 209
\end{array}
\right)
</math>

<math>
\left(
\begin{array}{cc}
 \frac{2 \sqrt{5}}{3}+\frac{i}{3} & \frac{\sqrt{5}}{3}-\frac{i}{3} \\
 \frac{2 \sqrt{5}}{3}-\frac{2 i}{3} & \frac{\sqrt{5}}{3}+\frac{2 i}{3}
\end{array}
\right)
</math>

<math>
\left(
\begin{array}{cc}
 \frac{(-1)^{\pi }}{3}+2\frac{5^{\pi }}{3} & \frac{5^{\pi }}{3}-\frac{1}{3} (-1)^{\pi } \\
 2\frac{5^{\pi }}{3}-\frac{2}{3} (-1)^{\pi } & \frac{2 (-1)^{\pi }}{3}+\frac{5^{\pi }}{3}
\end{array}
\right)
</math>

Symbolic matrices like {{i,j},{k,l}} to the power m give general solutions for all possible i,j,k,l, and m:

```Mathematica
MatrixPower[{{i, j}, {k, l}}, m] // Simplify
```

gives back (note that the simplification is not necessary for the evaluation, it just gives a shorter output):

<div style="overflow: auto;"><!-- This <div> puts a scroll bar on the <math>. -->
<math>
\left(
\begin{array}{cc}
 \frac{2^{-m-1} \left(\left(\sqrt{i^2-2 i l+4 j k+l^2}-i+l\right) \left(-\sqrt{i^2-2 i l+4 j k+l^2}+i+l\right)^m+\left(\sqrt{i^2-2 i l+4 j k+l^2}+i-l\right)
   \left(\sqrt{i^2-2 i l+4 j k+l^2}+i+l\right)^m\right)}{\sqrt{i^2-2 i l+4 j k+l^2}} & \frac{j 2^{-m} \left(\left(\sqrt{i^2-2 i l+4 j
   k+l^2}+i+l\right)^m-\left(-\sqrt{i^2-2 i l+4 j k+l^2}+i+l\right)^m\right)}{\sqrt{i^2-2 i l+4 j k+l^2}} \\
 \frac{k 2^{-m} \left(\left(\sqrt{i^2-2 i l+4 j k+l^2}+i+l\right)^m-\left(-\sqrt{i^2-2 i l+4 j k+l^2}+i+l\right)^m\right)}{\sqrt{i^2-2 i l+4 j k+l^2}} &
   \frac{2^{-m-1} \left(\left(\sqrt{i^2-2 i l+4 j k+l^2}+i-l\right) \left(-\sqrt{i^2-2 i l+4 j k+l^2}+i+l\right)^m+\left(\sqrt{i^2-2 i l+4 j k+l^2}-i+l\right)
   \left(\sqrt{i^2-2 i l+4 j k+l^2}+i+l\right)^m\right)}{\sqrt{i^2-2 i l+4 j k+l^2}}
\end{array}
\right)
</math>
</div>

Final note: Do not confuse MatrixPower with MatrixExp; the former is for matrix exponentiation, and the latter for the matrix exponential (E^m).


## MATLAB

For exponents in the form of A*A*A*A*...*A, A must be a square matrix:

```Matlab
function [output] = matrixexponentiation(matrixA, exponent)
   output = matrixA^(exponent);
```


Otherwise, to take the individual array elements to the power of an exponent (the matrix need not be square):

```Matlab
function [output] = matrixexponentiation(matrixA, exponent)
   output = matrixA.^(exponent);
```



## Maxima


```maxima
a: matrix([3, 2],
          [4, 1])$

a ^^ 4;
/* matrix([417, 208],
          [416, 209]) */

a ^^ -1;
/* matrix([-1/5, 2/5],
          [4/5, -3/5]) */
```



## OCaml


We will use some auxiliary functions


```ocaml
(* identity matrix *)
let eye n =
  let a = Array.make_matrix n n 0.0 in
  for i=0 to n-1 do
    a.(i).(i) <- 1.0
  done;
  (a)
;;

(* matrix dimensions *)
let dim a = Array.length a, Array.length a.(0);;

(* make matrix from list in row-major order *)
let matrix p q v =
  if (List.length v) <> (p * q)
  then failwith "bad dimensions"
  else
    let a = Array.make_matrix p q (List.hd v) in
    let rec g i j = function
    | [] -> a
    | x::v ->
        a.(i).(j) <- x;
        if j+1 < q
        then g i (j+1) v
        else g (i+1) 0 v
    in
    g 0 0 v
;;

(* matrix product *)
let matmul a b =
  let n, p = dim a
  and q, r = dim b in
  if p <> q then failwith "bad dimensions" else
  let c = Array.make_matrix n r 0.0 in
  for i=0 to n-1 do
    for j=0 to r-1 do
      for k=0 to p-1 do
        c.(i).(j) <- c.(i).(j) +. a.(i).(k) *. b.(k).(j)
      done
    done
  done;
  (c)
;;

(* generic exponentiation, usual algorithm *)
let pow one mul a n =
  let rec g p x = function
  | 0 -> x
  | i ->
      g (mul p p) (if i mod 2 = 1 then mul p x else x) (i/2)
  in
  g a one n
;;

(* example with integers *)
pow 1 ( * ) 2 16;;
(* - : int = 65536 *)
```


Now matrix power is simply a special case of pow :


```ocaml
let matpow a n =
  let p, q = dim a in
  if p <> q then failwith "bad dimensions" else
  pow (eye p) matmul a n;;

matpow (matrix 2 2 [ 1.0; 1.0; 1.0; 0.0 ]) 10;;
(* - : float array array = [|[|89.; 55.|]; [|55.; 34.|]|] *)

(* use as infix operator *)
let ( ^^ ) = matpow;;

[| [| 1.0; 1.0|]; [| 1.0; 0.0 |] |] ^^ 10;;
(* - : float array array = [|[|89.; 55.|]; [|55.; 34.|]|] *)
```



## Octave


Of course GNU Octave handles matrix and operations on matrix "naturally".


```octave
M = [ 3, 2; 2, 1 ];
M^0
M^1
M^2
M^(-1)
M^0.5
```


Output:


```txt
ans =

   1   0
   0   1

ans =

   3   2
   2   1

ans =

   13    8
    8    5

ans =

  -1.0000   2.0000
   2.0000  -3.0000

ans =

   1.48931 + 0.13429i   0.92044 - 0.21729i
   0.92044 - 0.21729i   0.56886 + 0.35158i
```


(Of course this is not an implementation, but it can be used as reference for the results)


## PARI/GP


```parigp
M^n
```



## Perl


```perl
use strict;
package SquareMatrix;
use Carp;                       # standard, "it's not my fault" module

use overload (
        '""'    => \&_string,   # overload string operator so we can just print
        '*'     => \&_mult,     # multiplication, needed for expo
        '*='    => \&_mult,     # ditto, explicitly defined to trigger copy
        '**'    => \&_expo,     # overload exponentiation
        '='     => \&_copy,     # copy operator
);

sub make {
        my $cls = shift;
        my $n = @_;
        for (@_) {
                # verify each row given is the right length
                confess "Bad data @$_: matrix must be square "
                        if @$_ != $n;
        }

        bless [ map [@$_], @_ ] # important: actually copy all the rows
}

sub identity {
        my $self = shift;
        my $n = @$self - 1;
        my @rows = map [ (0) x $_, 1, (0) x ($n - $_) ], 0 .. $n;
        bless \@rows
}

sub zero {
        my $self = shift;
        my $n = @$self;
        bless [ map [ (0) x $n ], 1 .. $n ]
}

sub _string {
        "[ ".join("\n  " =>
                map join(" " => map(sprintf("%12.6g", $_), @$_)), @{+shift}
        )."  ]\n";
}

sub _mult {
        my ($a, $b) = @_;
        my $x = $a->zero;
        my @idx = (0 .. $#$x);
        for my $j (@idx) {
                my @col = map($a->[$_][$j], @idx);
                for my $i (@idx) {
                        my $row = $b->[$i];
                        $x->[$i][$j] += $row->[$_] * $col[$_] for @idx;
                }
        }
        $x
}

sub _expo {
        my ($self, $n) = @_;
        confess "matrix **: must be non-negative integer power"
                        unless $n >= 0 && $n == int($n);

        my ($tmp, $out) = ($self, $self->identity);
        do {
                $out *= $tmp    if $n & 1;
                $tmp *= $tmp;
        } while $n >>= 1;

        $out
}

sub _copy { bless [ map [ @$_ ], @{+shift} ] }

# now use our matrix class
package main;

my $m = SquareMatrix->make(
                [1, 2, 0],
                [0, 3, 1],
                [1, 0, 0] );
print "### Order $_\n", $m ** $_        for 0 .. 10;

$m = SquareMatrix->make(
        [ 1.0001, 0,      0, 1       ],
        [ 0,      1.001,  0, 0       ],
        [ 0,      0,      1, 0.99998 ],
        [ 1e-8,   0,      0, 1.0002  ]);

print "\n### Matrix is now\n",  $m;
print "\n### Big power:\n",     $m ** 100_000;
print "\n### Too big:\n",       $m ** 1_000_000;
print "\n### WAY too big:\n",   $m ** 1_000_000_000_000;
print "\n### But identity matrix can handle that\n",
                $m->identity ** 1_000_000_000_000;
```


## Perl 6

{{works with|rakudo|2015.11}}

```perl6
subset SqMat of Array where { .elems == all(.[]».elems) }

multi infix:<*>(SqMat $a, SqMat $b) {[
    for ^$a -> $r {[
        for ^$b[0] -> $c {
            [+] ($a[$r][] Z* $b[].map: *[$c])
        }
    ]}
]}

multi infix:<**> (SqMat $m, Int $n is copy where { $_ >= 0 }) {
    my $tmp = $m;
    my $out = [for ^$m -> $i { [ for ^$m -> $j { +($i == $j) } ] } ];
    loop {
        $out = $out * $tmp if $n +& 1;
        last unless $n +>= 1;
        $tmp = $tmp * $tmp;
    }

    $out;
}

multi show (SqMat $m) {
    my $size = $m.flatmap( *.list».chars ).max;
    say .fmt("%{$size}s", ' ') for $m.list;
}

my @m = [1, 2, 0],
        [0, 3, 1],
        [1, 0, 0];

for 0 .. 10 -> $order {
    say "### Order $order";
    show @m ** $order;
}
```

{{out}}

```txt
### Order 0
1 0 0
0 1 0
0 0 1
### Order 1
1 2 0
0 3 1
1 0 0
### Order 2
1 8 2
1 9 3
1 2 0
### Order 3
 3 26  8
 4 29  9
 1  8  2
### Order 4
11 84 26
13 95 29
 3 26  8
### Order 5
 37 274  84
 42 311  95
 11  84  26
### Order 6
 121  896  274
 137 1017  311
  37  274   84
### Order 7
 395 2930  896
 448 3325 1017
 121  896  274
### Order 8
 1291  9580  2930
 1465 10871  3325
  395  2930   896
### Order 9
 4221 31322  9580
 4790 35543 10871
 1291  9580  2930
### Order 10
 13801 102408  31322
 15661 116209  35543
  4221  31322   9580
```



## Phix

Phix does not permit operator overloading, however here is a simple function to raise a square matrix to a non-negative integer power.

First two routines copied straight from the [[Identity_matrix#Phix|Identity_matrix]] and [[Matrix_multiplication#Phix|Matrix_multiplication]] tasks.

```Phix
function identity(integer n)
sequence res = repeat(repeat(0,n),n)
    for i=1 to n do
        res[i][i] = 1
    end for
    return res
end function

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

function matrix_exponent(sequence m, integer n)
integer l = length(m)
    if n=0 then return identity(l) end if
    sequence res = m
    for i=2 to n do
        res = matrix_mul(res,m)
    end for
    return res
end function

constant M1 = {{5}}
constant M2 = {{3, 2},
               {2, 1}}
constant M3 = {{1, 2, 0},
               {0, 3, 1},
               {1, 0, 0}}

ppOpt({pp_Nest,1})
pp(matrix_exponent(M1,0))
pp(matrix_exponent(M1,1))
pp(matrix_exponent(M1,2))
puts(1,"==\n")
pp(matrix_exponent(M2,0))
pp(matrix_exponent(M2,1))
pp(matrix_exponent(M2,2))
pp(matrix_exponent(M2,10))
puts(1,"==\n")
pp(matrix_exponent(M3,10))
puts(1,"==\n")
pp(matrix_exponent(identity(4),5))
```

{{out}}

```txt

{{1}}
{{5}}
{{25}}
==
{{1,0},
 {0,1}}
{{3,2},
 {2,1}}
{{13,8},
 {8,5}}
{{1346269,832040},
 {832040,514229}}
==
{{13801,102408,31322},
 {15661,116209,35543},
 {4221,31322,9580}}
==
{{1,0,0,0},
 {0,1,0,0},
 {0,0,1,0},
 {0,0,0,1}}

```



## PicoLisp

Uses the 'matMul' function from [[Matrix multiplication#PicoLisp]]

```PicoLisp
(de matIdent (N)
   (let L (need N (1) 0)
      (mapcar '(() (copy (rot L))) L) ) )

(de matExp (Mat N)
   (let M (matIdent (length Mat))
      (do N
         (setq M (matMul M Mat)) )
      M ) )

(matExp '((3 2) (2 1)) 3)
```

Output:

```txt
-> ((55 34) (34 21))
```



## Python

Using matrixMul from [[Matrix multiplication#Python]]

```python>>>
 from operator import mul
>>> def matrixMul(m1, m2):
  return map(
    lambda row:
      map(
        lambda *column:
          sum(map(mul, row, column)),
        *m2),
    m1)

>>> def identity(size):
	size = range(size)
	return [[(i==j)*1 for i in size] for j in size]

>>> def matrixExp(m, pow):
	assert pow>=0 and int(pow)==pow, "Only non-negative, integer powers allowed"
	accumulator = identity(len(m))
	for i in range(pow):
		accumulator = matrixMul(accumulator, m)
	return accumulator

>>> def printtable(data):
	for row in data:
		print ' '.join('%-5s' % ('%s' % cell) for cell in row)


>>> m = [[3,2], [2,1]]
>>> for i in range(5):
	print '\n%i:' % i
	printtable( matrixExp(m, i) )



0:
1     0
0     1

1:
3     2
2     1

2:
13    8
8     5

3:
55    34
34    21

4:
233   144
144   89
>>> printtable( matrixExp(m, 10) )
1346269 832040
832040 514229
>>>
```


Alternative Based Upon @ operator of Python 3.5 PEP 465 and using Matrix exponentation for faster computation of powers
<lang>
class Mat(list) :
    def __matmul__(self, B) :
        A = self
        return Mat([[sum(A[i][k]*B[k][j] for k in range(len(B)))
                    for j in range(len(B[0])) ] for i in range(len(A))])


def identity(size):
    size = range(size)
    return [[(i==j)*1 for i in size] for j in size]

def power(F, n):
    result = Mat(identity(len(F)))
    b = Mat(F)
    while n > 0:
        if (n%2) == 0:
            b = b @ b
            n //= 2
        else:
            result = b @ result
            b = b @ b
            n //= 2
    return result

def printtable(data):
    for row in data:
        print (' '.join('%-5s' % ('%s' % cell) for cell in row))

m = [[3,2], [2,1]]
for i in range(5):
    print('\n%i:' % i)
    printtable(power(m, i))

```

{{Output}}

```txt

0:
[[1, 0], [0, 1]]

1:
[[3, 2], [2, 1]]

2:
[[13, 8], [8, 5]]

3:
[[55, 34], [34, 21]]

4:
[[233, 144], [144, 89]]

```



## R

{{libheader|Biodem}}

```R
library(Biodem)
m <- matrix(c(3,2,2,1), nrow=2)
mtx.exp(m, 0)
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
mtx.exp(m, 1)
#      [,1] [,2]
# [1,]    3    2
# [2,]    2    1
mtx.exp(m, 2)
#      [,1] [,2]
# [1,]   13    8
# [2,]    8    5
mtx.exp(m, 3)
#      [,1] [,2]
# [1,]   55   34
# [2,]   34   21
mtx.exp(m, 10)
#         [,1]   [,2]
# [1,] 1346269 832040
# [2,]  832040 514229
```

Note that non-integer powers are not supported with this function.


## Racket



```Racket

#lang racket
(require math)

(define a (matrix ((3 2) (2 1))))

;; Using the builtin matrix exponentiation
(for ([i 11])
  (printf "a^~a = ~s\n" i (matrix-expt a i)))

;; Output:
;; a^0 = (array #[#[1 0] #[0 1]])
;; a^1 = (array #[#[3 2] #[2 1]])
;; a^2 = (array #[#[13 8] #[8 5]])
;; a^3 = (array #[#[55 34] #[34 21]])
;; a^4 = (array #[#[233 144] #[144 89]])
;; a^5 = (array #[#[987 610] #[610 377]])
;; a^6 = (array #[#[4181 2584] #[2584 1597]])
;; a^7 = (array #[#[17711 10946] #[10946 6765]])
;; a^8 = (array #[#[75025 46368] #[46368 28657]])
;; a^9 = (array #[#[317811 196418] #[196418 121393]])
;; a^10 = (array #[#[1346269 832040] #[832040 514229]])

;; But it could be implemented manually, using matrix multiplication
(define (mpower M p)
  (cond [(= p 1) M]
        [(even? p) (mpower (matrix* M M) (/ p 2))]
        [else (matrix* M (mpower M (sub1 p)))]))
(for ([i (in-range 1 11)])
  (printf "a^~a = ~s\n" i (matrix-expt a i)))

```



## Ruby

Ruby's standard library already provides the matrix-exponentiation operator. It is <code>Matrix#**</code> from package 'matrix' of the standard library. [[MRI]] 1.9.x implements the matrix-exponentiation operator in file [http://redmine.ruby-lang.org/projects/ruby-19/repository/entry/lib/matrix.rb matrix.rb], <code>def **</code> (around [http://redmine.ruby-lang.org/projects/ruby-19/repository/entry/lib/matrix.rb#L961 line 961]).


```txt
$ irb
irb(main):001:0> require 'matrix'
=> true
irb(main):002:0> m=Matrix[[3,2],[2,1]]
=> Matrix[[3, 2], [2, 1]]
irb(main):003:0> m**0
=> Matrix[[1, 0], [0, 1]]
irb(main):004:0> m ** 1
=> Matrix[[3, 2], [2, 1]]
irb(main):005:0> m ** 2
=> Matrix[[13, 8], [8, 5]]
irb(main):006:0> m ** 5
=> Matrix[[987, 610], [610, 377]]
irb(main):007:0> m ** 10
=> Matrix[[1346269, 832040], [832040, 514229]]
```


Starting with Ruby 1.9.3, it can also calculate Matrix ** Float.

{{works with|Ruby|1.9.3}}


```txt
irb(main):008:0> m ** 1.5
=> Matrix[[(6.308803769316981-0.03170173099577213i), (3.8990551577913446+0.05129
4478253365354i)], [(3.899055157791345+0.05129447825336536i), (2.4097486115256355
-0.0829962092491375i)]]
```


With older Ruby, it raises an exception for Matrix ** Float.


```txt
irb(main):008:0> m ** 1.5
ExceptionForMatrix::ErrOperationNotDefined: This operation(**) can't defined
        from /usr/lib/ruby/1.8/matrix.rb:665:in `**'
        from (irb):8
```



## Rust

Rust (1.37.0) does not allow to overload the ** operator, instead ^ (bitwise xor) is used.

```rust
use std::fmt;
use std::ops;
const WIDTH: usize = 6;

#[derive(Clone)]
struct SqMat {
    data: Vec<Vec<i64>>,
}

impl fmt::Debug for SqMat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut row = "".to_string();
        for i in &self.data {
            for j in i {
                row += &format!("{:>w$} ", j, w = WIDTH);
            }
            row += &"\n";
        }
        write!(f, "{}", row)
    }
}

impl ops::BitXor<u32> for SqMat {
    type Output = Self;

    fn bitxor(self, n: u32) -> Self::Output {
        let mut aux = self.data.clone();
        let mut ans: SqMat = SqMat {
            data: vec![vec![0; aux.len()]; aux.len()],
        };
        for i in 0..aux.len() {
            ans.data[i][i] = 1;
        }
        let mut b = n;
        while b > 0 {
            if b & 1 > 0 {
                // ans = ans * aux
                let mut tmp = aux.clone();
                for i in 0..aux.len() {
                    for j in 0..aux.len() {
                        tmp[i][j] = 0;
                        for k in 0..aux.len() {
                            tmp[i][j] += ans.data[i][k] * aux[k][j];
                        }
                    }
                }
                ans.data = tmp;
            }
            b >>= 1;
            if b > 0 {
                // aux = aux * aux
                let mut tmp = aux.clone();
                for i in 0..aux.len() {
                    for j in 0..aux.len() {
                        tmp[i][j] = 0;
                        for k in 0..aux.len() {
                            tmp[i][j] += aux[i][k] * aux[k][j];
                        }
                    }
                }
                aux = tmp;
            }
        }
        ans
    }
}

fn main() {
    let sm: SqMat = SqMat {
        data: vec![vec![1, 2, 0], vec![0, 3, 1], vec![1, 0, 0]],
    };
    for i in 0..11 {
        println!("Power of {}:\n{:?}", i, sm.clone() ^ i);
    }
}
```

{{out}}

```txt

Power of 0:
     1      0      0
     0      1      0
     0      0      1

Power of 1:
     1      2      0
     0      3      1
     1      0      0

Power of 2:
     1      8      2
     1      9      3
     1      2      0

Power of 3:
     3     26      8
     4     29      9
     1      8      2

Power of 4:
    11     84     26
    13     95     29
     3     26      8

Power of 5:
    37    274     84
    42    311     95
    11     84     26

Power of 6:
   121    896    274
   137   1017    311
    37    274     84

Power of 7:
   395   2930    896
   448   3325   1017
   121    896    274

Power of 8:
  1291   9580   2930
  1465  10871   3325
   395   2930    896

Power of 9:
  4221  31322   9580
  4790  35543  10871
  1291   9580   2930

Power of 10:
 13801 102408  31322
 15661 116209  35543
  4221  31322   9580

```


## Scala


```scala
class Matrix[T](matrix:Array[Array[T]])(implicit n: Numeric[T], m: ClassManifest[T])
{
  import n._
  val rows=matrix.size
  val cols=matrix(0).size
  def row(i:Int)=matrix(i)
  def col(i:Int)=matrix map (_(i))

  def *(other: Matrix[T]):Matrix[T] = new Matrix(
    Array.tabulate(rows, other.cols)((row, col) =>
      (this.row(row), other.col(col)).zipped.map(_*_) reduceLeft (_+_)
  ))

  def **(x: Int)=x match {
    case 0 => createIdentityMatrix
    case 1 => this
    case 2 => this * this
    case _ => List.fill(x)(this) reduceLeft (_*_)
  }

  def createIdentityMatrix=new Matrix(Array.tabulate(rows, cols)((row,col) =>
    if (row == col) one else zero)
  )

  override def toString = matrix map (_.mkString("[", ", ", "]")) mkString "\n"
}

object MatrixTest {
  def main(args:Array[String])={
    val m=new Matrix[BigInt](Array(Array(3,2), Array(2,1)))
    println("-- m --\n"+m)

    Seq(0,1,2,3,4,10,20,50) foreach {x =>
      println("-- m**"+x+" --")
      println(m**x)
    }
  }
}
```

{{out}}

```txt
-- m --
[3, 2]
[2, 1]
-- m**0 --
[1, 0]
[0, 1]
-- m**1 --
[3, 2]
[2, 1]
-- m**2 --
[13, 8]
[8, 5]
-- m**3 --
[55, 34]
[34, 21]
-- m**4 --
[233, 144]
[144, 89]
-- m**10 --
[1346269, 832040]
[832040, 514229]
-- m**20 --
[2504730781961, 1548008755920]
[1548008755920, 956722026041]
-- m**50 --
[16130531424904581415797907386349, 9969216677189303386214405760200]
[9969216677189303386214405760200, 6161314747715278029583501626149]
```



## Scheme


For simplicity, the matrix is represented as a list of lists, and no dimension checking occurs. This implementation does not work when the exponent is 0.


```scheme

(define (dec x)
  (- x 1))

(define (halve x)
  (/ x 2))

(define (row*col row col)
  (apply + (map * row col)))

(define (matrix-multiply m1 m2)
  (map
    (lambda (row)
      (apply map (lambda col (row*col row col))
        m2))
    m1))

(define (matrix-exp mat exp)
  (cond ((= exp 1) mat)
        ((even? exp) (square-matrix (matrix-exp mat (halve exp))))
        (else (matrix-multiply mat (matrix-exp mat (dec exp))))))

(define (square-matrix mat)
  (matrix-multiply mat mat))

```



{{out}}

```txt

> (matrix-exp '((3 2) (2 1)) 50)
((16130531424904581415797907386349 9969216677189303386214405760200)
 (9969216677189303386214405760200 6161314747715278029583501626149))

```



## Seed7

The example below uses several features of Seed7:
*Overloading of the operators ''*'' and ''**'' .
*The template [http://seed7.sourceforge.net/libraries/enable_io.htm#enable_output%28in_type%29 enable_output], which allows writing a ''matrix'' with write (the function ''str'' must be defined before calling ''enable_output'').
*A ''for'' loop which loops over values listed in an array literal


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const type: matrix is array array float;

const func string: str (in matrix: mat) is func
  result
    var string: stri is "";
  local
    var integer: row is 0;
    var integer: column is 0;
  begin
    for row range 1 to length(mat) do
      for column range 1 to length(mat[row]) do
        stri &:= str(mat[row][column]);
        if column < length(mat[row]) then
          stri &:= ", ";
        end if;
      end for;
      if row < length(mat) then
        stri &:= "\n";
      end if;
    end for;
  end func;

enable_output(matrix);

const func matrix: (in matrix: mat1) * (in matrix: mat2) is func
  result
    var matrix: product is matrix.value;
  local
    var integer: row is 0;
    var integer: column is 0;
    var integer: k is 0;
  begin
    product := length(mat1) times length(mat1) times 0.0;
    for row range 1 to length(mat1) do
      for column range 1 to length(mat1) do
        product[row][column] := 0.0;
        for k range 1 to length(mat1) do
          product[row][column] +:= mat1[row][k] * mat2[k][column];
        end for;
      end for;
    end for;
  end func;

const func matrix: (in var matrix: base) ** (in var integer: exponent) is func
  result
    var matrix: power is matrix.value;
  local
    var integer: row is 0;
    var integer: column is 0;
  begin
    if exponent < 0 then
      raise NUMERIC_ERROR;
    else
      if odd(exponent) then
        power := base;
      else
        # Create identity matrix
        power := length(base) times length(base) times 0.0;
        for row range 1 to length(base) do
          for column range 1 to length(base) do
            if row = column then
              power[row][column] := 1.0;
            end if;
          end for;
        end for;
      end if;
      exponent := exponent div 2;
      while exponent > 0 do
        base := base * base;
        if odd(exponent) then
          power := power * base;
        end if;
        exponent := exponent div 2;
      end while;
    end if;
  end func;

const proc: main is func
  local
    var matrix: m is [] (
      [] (4.0, 3.0),
      [] (2.0, 1.0));
    var integer: exponent is 0;
  begin
    for exponent range [] (0, 1, 2, 3, 5, 7, 11, 13, 17, 19, 23) do
      writeln("m ** " <& exponent <& " =");
      writeln(m ** exponent);
    end for;
  end func;
```


Original source of matrix exponentiation: [http://seed7.sourceforge.net/algorith/math.htm#matrix_exponentiation]

Output:

```txt

m ** 0 =
1.0, 0.0
0.0, 1.0
m ** 1 =
4.0, 3.0
2.0, 1.0
m ** 2 =
22.0, 15.0
10.0, 7.0
m ** 3 =
118.0, 81.0
54.0, 37.0
m ** 5 =
3406.0, 2337.0
1558.0, 1069.0
m ** 7 =
98302.0, 67449.0
44966.0, 30853.0
m ** 11 =
81883680.0, 56183720.0
37455816.0, 25699956.0
m ** 13 =
2363278336.0, 1621541248.0
1081027456.0, 741736960.0
m ** 17 =
1968565387264.0, 1350712688640.0
900475125760.0, 617852567552.0
m ** 19 =
56815568027648.0, 38983467794432.0
25988979228672.0, 17832093941760.0
m ** 23 =
47326274699395072.0, 32472478198530048.0
21648320946503680.0, 14853792205897728.0

```



## Sidef


```ruby
class Array {
    method ** (Number n { .>= 0 }) {
        var tmp = self
        var out = self.len.of {|i| self.len.of {|j| i == j ? 1 : 0 }}
        loop {
            out = (out `mmul` tmp) if n.is_odd
            n >>= 1 || break
            tmp = (tmp `mmul` tmp)
        }
        return out
    }
}

var m = [[1, 2, 0],
         [0, 3, 1],
         [1, 0, 0]]

for order in (0..5) {
    say "### Order #{order}"
    var t = (m ** order)
    say ('  ', t.join("\n  "))
}
```

{{out}}

```txt

### Order 0
  [1, 0, 0]
  [0, 1, 0]
  [0, 0, 1]
### Order 1
  [1, 2, 0]
  [0, 3, 1]
  [1, 0, 0]
### Order 2
  [1, 8, 2]
  [1, 9, 3]
  [1, 2, 0]
### Order 3
  [3, 26, 8]
  [4, 29, 9]
  [1, 8, 2]
### Order 4
  [11, 84, 26]
  [13, 95, 29]
  [3, 26, 8]
### Order 5
  [37, 274, 84]
  [42, 311, 95]
  [11, 84, 26]

```



## SPAD

{{works with|FriCAS}}
{{works with|OpenAxiom}}
{{works with|Axiom}}

```SPAD
(1) -> A:=matrix [[0,-%i],[%i,0]]

        +0   - %i+
   (1)  |        |
        +%i   0  +
                                               Type: Matrix(Complex(Integer))
(2) -> A^4

        +1  0+
   (2)  |    |
        +0  1+
                                               Type: Matrix(Complex(Integer))
(3) -> A^(-1)

        +0   - %i+
   (3)  |        |
        +%i   0  +
                                     Type: Matrix(Fraction(Complex(Integer)))
(4) -> inverse A

        +0   - %i+
   (4)  |        |
        +%i   0  +
                          Type: Union(Matrix(Fraction(Complex(Integer))),...)
```


Domain:[http://fricas.github.io/api/Matrix.html?highlight=matrix Matrix(R)]


## Stata


This implementation uses [https://en.wikipedia.org/wiki/Exponentiation_by_squaring Exponentiation by squaring] to compute a^n for a matrix a and an integer n (which may be positive, negative or zero).


```stata
real matrix matpow(real matrix a, real scalar n) {
	real matrix p, x
	real scalar i, s
	s = n<0
	n = abs(n)
	x = a
	p = I(rows(a))
	for (i=n; i>0; i=floor(i/2)) {
		if (mod(i,2)==1) p = p*x
		x = x*x
	}
	return(s?luinv(p):p)
}
```


Here is an example to compute Fibonacci numbers:


```stata
: matpow((0,1\1,1),10)
[symmetric]
        1    2
    +-----------+
  1 |  34       |
  2 |  55   89  |
    +-----------+
```



## Tcl

Using code at [[Matrix multiplication#Tcl]] and [[Matrix Transpose#Tcl]]

```tcl
package require Tcl 8.5
namespace path {::tcl::mathop ::tcl::mathfunc}

proc matrix_exp {m pow} {
    if { ! [string is int -strict $pow]} {
        error "non-integer exponents not implemented"
    }
    if {$pow < 0} {
        error "negative exponents not implemented"
    }
    lassign [size $m] rows cols
    # assume square matrix
    set temp [identity $rows]
    for {set n 1} {$n <= $pow} {incr n} {
        set temp [matrix_multiply $temp $m]
    }
    return $temp
}

proc identity {size} {
    set i [lrepeat $size [lrepeat $size 0]]
    for {set n 0} {$n < $size} {incr n} {lset i $n $n 1}
    return $i
}
```


```txt
% print_matrix [matrix_exp {{3 2} {2 1}} 1]
3 2
2 1
% print_matrix [matrix_exp {{3 2} {2 1}} 0]
1 0
0 1
% print_matrix [matrix_exp {{3 2} {2 1}} 2]
13 8
 8 5
% print_matrix [matrix_exp {{3 2} {2 1}} 3]
55 34
34 21
% print_matrix [matrix_exp {{3 2} {2 1}} 4]
233 144
144  89
% print_matrix [matrix_exp {{3 2} {2 1}} 10]
1346269 832040
 832040 514229

```


=={{header|TI-89 BASIC}}==

{{improve|TI-89 BASIC|Explicitly implement exponentiation.}}

Built-in exponentiation:


```ti89b
[3,2;4,1]^4
```


Output: <math>\begin{bmatrix}417 & 208 \\ 416 & 209\end{bmatrix}</math>


## Ursala

For matrices of floating point numbers, the library function <code>mmult</code> can be used as shown. The user-defined <code>id</code> function takes a square matrix to the identity matrix of the same dimensions. The <code>mex</code> function takes a pair <math>(A,n)</math>
representing a real matrix <math>A</math> and a natural exponent <math>n</math> to the exponentiation <math>A^n</math> using the naive algorithm.

```Ursala
#import nat
#import lin

id  = @h ^|CzyCK33/1.! 0.!*
mex = ||id@l mmult:-0^|DlS/~& iota
```

Alternatively, this version uses the fast binary algorithm.

```Ursala
mex = ~&ar^?\id@al (~&lr?/mmult@llPrX ~&r)^/~&alrhPX mmult@falrtPXPRiiX
```

This test program raises a 2 by 2 matrix to a selection of powers.

```Ursala
#cast %eLLL

test = mex/*<<3.,2.>,<2.,1.>> <0,1,2,3,4,10>
```

output:

```txt
<
   <
      <1.000000e+00,0.000000e+00>,
      <0.000000e+00,1.000000e+00>>,
   <
      <3.000000e+00,2.000000e+00>,
      <2.000000e+00,1.000000e+00>>,
   <
      <1.300000e+01,8.000000e+00>,
      <8.000000e+00,5.000000e+00>>,
   <
      <5.500000e+01,3.400000e+01>,
      <3.400000e+01,2.100000e+01>>,
   <
      <2.330000e+02,1.440000e+02>,
      <1.440000e+02,8.900000e+01>>,
   <
      <1.346269e+06,8.320400e+05>,
      <8.320400e+05,5.142290e+05>>>
```


## VBA

No operator overloading in VBA. Implemented as a function. Can not handle scalars. Requires matrix size greater than one. Does allow for negative exponents.

```vb
Option Base 1
Private Function Identity(n As Integer) As Variant
    Dim I() As Variant
    ReDim I(n, n)
    For j = 1 To n
        For k = 1 To n
            I(j, k) = 0
        Next k
    Next j
    For j = 1 To n
        I(j, j) = 1
    Next j
    Identity = I
End Function
 Function MatrixExponentiation(ByVal x As Variant, ByVal n As Integer) As Variant
    If n < 0 Then
        x = WorksheetFunction.MInverse(x)
        n = -n
    End If
    If n = 0 Then
        MatrixExponentiation = Identity(UBound(x))
        Exit Function
    End If
    Dim y() As Variant
    y = Identity(UBound(x))
    Do While n > 1
        If n Mod 2 = 0 Then
            x = WorksheetFunction.MMult(x, x)
            n = n / 2
        Else
            y = WorksheetFunction.MMult(x, y)
            x = WorksheetFunction.MMult(x, x)
            n = (n - 1) / 2
        End If
    Loop
    MatrixExponentiation = WorksheetFunction.MMult(x, y)
End Function
Public Sub pp(x As Variant)
    For i_ = 1 To UBound(x)
        For j_ = 1 To UBound(x)
            Debug.Print x(i_, j_),
        Next j_
        Debug.Print
    Next i_
End Sub
Public Sub main()
    M2 = [{3,2;2,1}]
    M3 = [{1,2,0;0,3,1;1,0,0}]
    pp MatrixExponentiation(M2, -1)
    Debug.Print
    pp MatrixExponentiation(M2, 0)
    Debug.Print
    pp MatrixExponentiation(M2, 10)
    Debug.Print
    pp MatrixExponentiation(M3, 10)
End Sub
```
{{out}}

```txt
-1             2
 2            -3

 1             0
 0             1

 1346269       832040
 832040        514229

 13801         102408        31322
 15661         116209        35543
 4221          31322         9580
```

{{omit from|Icon|no operator overloading}}
{{omit from|Unicon|no operator overloading}}
