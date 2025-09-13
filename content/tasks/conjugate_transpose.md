+++
title = "Conjugate transpose"
description = ""
date = 2019-10-07T22:16:12Z
aliases = []
[extra]
id = 11330
[taxonomies]
categories = ["task"]
tags = []
+++

Suppose that a [[matrix]] <big><math>M</math></big> contains [[Arithmetic/Complex|complex numbers]]. Then the [[wp:conjugate transpose|conjugate transpose]] of <math>M</math> is a matrix <math>M^H</math> containing the [[complex conjugate]]s of the [[matrix transposition]] of <math>M</math>.

::: <big><math>(M^H)_{ji} = \overline{M_{ij}}</math></big>


This means that   row <big><math>j</math></big>, column <big><math>i</math></big> of the conjugate transpose equals the 

complex conjugate of row <big><math>i</math></big>, column <big><math>j</math></big> of the original matrix.


In the next list, <big><math>M</math></big> must also be a square matrix.

* A [[wp:Hermitian matrix|Hermitian matrix]] equals its own conjugate transpose: <math>M^H = M</math>.
* A [[wp:normal matrix|normal matrix]] is commutative in [[matrix multiplication|multiplication]] with its conjugate transpose: <math>M^HM = MM^H</math>.
* A [[wp:unitary matrix|unitary matrix]] has its [[inverse matrix|inverse]] equal to its conjugate transpose: <math>M^H = M^{-1}</math>. 
 This is true [[wikt:iff|iff]] <math>M^HM = I_n</math> and iff <math>MM^H = I_n</math>, where <math>I_n</math> is the identity matrix.




## Task

Given some matrix of complex numbers, find its conjugate transpose. 

Also determine if the matrix is a:
::* Hermitian matrix,
::* normal matrix,  or 
::* unitary matrix.


## See also

* MathWorld entry: [http://mathworld.wolfram.com/ConjugateTranspose.html conjugate transpose]
* MathWorld entry: [http://mathworld.wolfram.com/HermitianMatrix.html Hermitian matrix]
* MathWorld entry: [http://mathworld.wolfram.com/NormalMatrix.html normal matrix]
* MathWorld entry: [http://mathworld.wolfram.com/UnitaryMatrix.html unitary matrix]





## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Complex_Text_IO; use Ada.Complex_Text_IO;
with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;
with Ada.Numerics.Complex_Arrays; use Ada.Numerics.Complex_Arrays;
procedure ConTrans is
   subtype CM is Complex_Matrix;
   S2O2 : constant Float := 0.7071067811865;

   procedure Print (mat : CM) is begin
      for row in mat'Range(1) loop for col in mat'Range(2) loop
         Put(mat(row,col), Exp=>0, Aft=>4);
      end loop; New_Line; end loop;
   end Print;

   function almostzero(mat : CM; tol : Float) return Boolean is begin
      for row in mat'Range(1) loop for col in mat'Range(2) loop
         if abs(mat(row,col)) > tol then return False; end if;
      end loop; end loop;
      return True;
   end almostzero;

   procedure Examine (mat : CM) is
      CT : CM := Conjugate (Transpose(mat));
      isherm, isnorm, isunit : Boolean;
   begin
      isherm := almostzero(mat-CT, 1.0e-6);
      isnorm := almostzero(mat*CT-CT*mat, 1.0e-6);
      isunit := almostzero(CT-Inverse(mat), 1.0e-6);
      Print(mat);
      Put_Line("Conjugate transpose:"); Print(CT);
      Put_Line("Hermitian?: " & isherm'Img);
      Put_Line("Normal?: " & isnorm'Img);
      Put_Line("Unitary?: " & isunit'Img);
   end Examine;

   hmat : CM := ((3.0+0.0*i, 2.0+1.0*i), (2.0-1.0*i, 1.0+0.0*i));
   nmat : CM := ((1.0+0.0*i, 1.0+0.0*i, 0.0+0.0*i),
                 (0.0+0.0*i, 1.0+0.0*i, 1.0+0.0*i),
                 (1.0+0.0*i, 0.0+0.0*i, 1.0+0.0*i));
   umat : CM := ((S2O2+0.0*i, S2O2+0.0*i, 0.0+0.0*i),
                 (0.0+S2O2*i, 0.0-S2O2*i, 0.0+0.0*i),
                 (0.0+0.0*i, 0.0+0.0*i, 0.0+1.0*i));
begin
   Put_Line("hmat:"); Examine(hmat); New_Line;
   Put_Line("nmat:"); Examine(nmat); New_Line;
   Put_Line("umat:"); Examine(umat);
end ConTrans;
```

```txt
hmat:
( 3.0000, 0.0000)( 2.0000, 1.0000)
( 2.0000,-1.0000)( 1.0000, 0.0000)
Conjugate transpose:
( 3.0000,-0.0000)( 2.0000, 1.0000)
( 2.0000,-1.0000)( 1.0000,-0.0000)
Hermitian?: TRUE
Normal?: TRUE
Unitary?: FALSE

nmat:
( 1.0000, 0.0000)( 1.0000, 0.0000)( 0.0000, 0.0000)
( 0.0000, 0.0000)( 1.0000, 0.0000)( 1.0000, 0.0000)
( 1.0000, 0.0000)( 0.0000, 0.0000)( 1.0000, 0.0000)
Conjugate transpose:
( 1.0000,-0.0000)( 0.0000,-0.0000)( 1.0000,-0.0000)
( 1.0000,-0.0000)( 1.0000,-0.0000)( 0.0000,-0.0000)
( 0.0000,-0.0000)( 1.0000,-0.0000)( 1.0000,-0.0000)
Hermitian?: FALSE
Normal?: TRUE
Unitary?: FALSE

umat:
( 0.7071, 0.0000)( 0.7071, 0.0000)( 0.0000, 0.0000)
( 0.0000, 0.7071)( 0.0000,-0.7071)( 0.0000, 0.0000)
( 0.0000, 0.0000)( 0.0000, 0.0000)( 0.0000, 1.0000)
Conjugate transpose:
( 0.7071,-0.0000)( 0.0000,-0.7071)( 0.0000,-0.0000)
( 0.7071,-0.0000)( 0.0000, 0.7071)( 0.0000,-0.0000)
( 0.0000,-0.0000)( 0.0000,-0.0000)( 0.0000,-1.0000)
Hermitian?: FALSE
Normal?: TRUE
Unitary?: TRUE
```



## C


```c
/* Uses C99 specified complex.h, complex datatype has to be defined and operation provided if used on non-C99 compilers */

#include<stdlib.h>
#include<stdio.h>
#include<complex.h>

typedef struct
{
  int rows, cols;
  complex **z;
} matrix;

matrix
transpose (matrix a)
{
  int i, j;
  matrix b;

  b.rows = a.cols;
  b.cols = a.rows;

  b.z = malloc (b.rows * sizeof (complex *));

  for (i = 0; i < b.rows; i++)
    {
      b.z[i] = malloc (b.cols * sizeof (complex));
      for (j = 0; j < b.cols; j++)
        {
          b.z[i][j] = conj (a.z[j][i]);
        }
    }

  return b;
}

int
isHermitian (matrix a)
{
  int i, j;
  matrix b = transpose (a);

  if (b.rows == a.rows && b.cols == a.cols)
    {
      for (i = 0; i < b.rows; i++)
        {
          for (j = 0; j < b.cols; j++)
            {
              if (b.z[i][j] != a.z[i][j])
                return 0;
            }
        }
    }

  else
    return 0;

  return 1;
}

matrix
multiply (matrix a, matrix b)
{
  matrix c;
  int i, j;

  if (a.cols == b.rows)
    {
      c.rows = a.rows;
      c.cols = b.cols;

      c.z = malloc (c.rows * (sizeof (complex *)));

      for (i = 0; i < c.rows; i++)
        {
          c.z[i] = malloc (c.cols * sizeof (complex));
          c.z[i][j] = 0 + 0 * I;
          for (j = 0; j < b.cols; j++)
            {
              c.z[i][j] += a.z[i][j] * b.z[j][i];
            }
        }

    }

  return c;
}

int
isNormal (matrix a)
{
  int i, j;
  matrix a_ah, ah_a;

  if (a.rows != a.cols)
    return 0;

  a_ah = multiply (a, transpose (a));
  ah_a = multiply (transpose (a), a);

  for (i = 0; i < a.rows; i++)
    {
      for (j = 0; j < a.cols; j++)
        {
          if (a_ah.z[i][j] != ah_a.z[i][j])
            return 0;
        }
    }

  return 1;
}

int
isUnitary (matrix a)
{
  matrix b;
  int i, j;
  if (isNormal (a) == 1)
    {
      b = multiply (a, transpose(a));

      for (i = 0; i < b.rows; i++)
        {
          for (j = 0; j < b.cols; j++)
            {
              if ((i == j && b.z[i][j] != 1) || (i != j && b.z[i][j] != 0))
                return 0;
            }
        }
      return 1;
    }
  return 0;
}


int
main ()
{
  complex z = 3 + 4 * I;
  matrix a, aT;
  int i, j;
  printf ("Enter rows and columns :");
  scanf ("%d%d", &a.rows, &a.cols);

  a.z = malloc (a.rows * sizeof (complex *));
  printf ("Randomly Generated Complex Matrix A is : ");
  for (i = 0; i < a.rows; i++)
    {
      printf ("\n");
      a.z[i] = malloc (a.cols * sizeof (complex));
      for (j = 0; j < a.cols; j++)
        {
          a.z[i][j] = rand () % 10 + rand () % 10 * I;
          printf ("\t%f + %fi", creal (a.z[i][j]), cimag (a.z[i][j]));
        }
    }

  aT = transpose (a);

  printf ("\n\nTranspose of Complex Matrix A is : ");
  for (i = 0; i < aT.rows; i++)
    {
      printf ("\n");
      aT.z[i] = malloc (aT.cols * sizeof (complex));
      for (j = 0; j < aT.cols; j++)
        {
          aT.z[i][j] = rand () % 10 + rand () % 10 * I;
          printf ("\t%f + %fi", creal (aT.z[i][j]), cimag (aT.z[i][j]));
        }
    }

  printf ("\n\nComplex Matrix A %s hermitian",
          isHermitian (a) == 1 ? "is" : "is not");
  printf ("\n\nComplex Matrix A %s unitary",
          isUnitary (a) == 1 ? "is" : "is not");
  printf ("\n\nComplex Matrix A %s normal",
          isNormal (a) == 1 ? "is" : "is not");



  return 0;
}
```

```txt

Enter rows and columns :3 3
Randomly Generated Complex Matrix A is :
        3.000000 + 6.000000i    7.000000 + 5.000000i    3.000000 + 5.000000i
        6.000000 + 2.000000i    9.000000 + 1.000000i    2.000000 + 7.000000i
        0.000000 + 9.000000i    3.000000 + 6.000000i    0.000000 + 6.000000i

Transpose of Complex Matrix A is :
        2.000000 + 6.000000i    1.000000 + 8.000000i    7.000000 + 9.000000i
        2.000000 + 0.000000i    2.000000 + 3.000000i    7.000000 + 5.000000i
        9.000000 + 2.000000i    2.000000 + 8.000000i    9.000000 + 7.000000i

Complex Matrix A is not hermitian

Complex Matrix A is not unitary

Complex Matrix A is not normal
```



## Common Lisp


```Lisp

(defun matrix-multiply (m1 m2)
 (mapcar
  (lambda (row)
   (apply #'mapcar
    (lambda (&rest column)
     (apply #'+ (mapcar #'* row column))) m2)) m1))

(defun identity-p (m &optional (tolerance 1e-6))
 "Is m an identity matrix?"
  (loop for row in m
    for r = 1 then (1+ r) do
      (loop for col in row
        for c = 1 then (1+ c) do
          (if (eql r c)
            (unless (< (abs (- col 1)) tolerance) (return-from identity-p nil))
            (unless (< (abs col) tolerance) (return-from identity-p nil)) )))
  T )

(defun conjugate-transpose (m)
  (apply #'mapcar #'list (mapcar #'(lambda (r) (mapcar #'conjugate r)) m)) )

(defun hermitian-p (m)
  (equalp m (conjugate-transpose m)))

(defun normal-p (m)
  (let ((m* (conjugate-transpose m)))
    (equalp (matrix-multiply m m*) (matrix-multiply m* m)) ))
    
(defun unitary-p (m)
  (identity-p (matrix-multiply m (conjugate-transpose m))) )

```


```txt

(hermitian-p
  '((3        #C(2 1))
    (#C(2 -1) 1) ))
=> T

(normal-p
  '((#C(0 1) 0)
    (0       #C(3 -5)) ))
==> T

(unitary-p
  '((0.70710677        0.70710677       0)
    (#C(0 -0.70710677) #C(0 0.70710677) 0)
    (0                 0                1) ))
==> T

```



## D

{{trans|Python}} A well typed and mostly imperative version:

```d
import std.stdio, std.complex, std.math, std.range, std.algorithm,
       std.numeric;

T[][] conjugateTranspose(T)(in T[][] m) pure nothrow @safe {
    auto r = new typeof(return)(m[0].length, m.length);
    foreach (immutable nr, const row; m)
        foreach (immutable nc, immutable c; row)
            r[nc][nr] = c.conj;
    return r;
}

bool isRectangular(T)(in T[][] M) pure nothrow @safe @nogc {
    return M.all!(row => row.length == M[0].length);
}

T[][] matMul(T)(in T[][] A, in T[][] B) pure nothrow /*@safe*/
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

/// Check any number of complex matrices for equality within
/// some bits of mantissa.
bool areEqual(T)(in Complex!T[][][] matrices, in size_t nBits=20)
pure nothrow /*@safe*/ {
    static bool allSame(U)(in U[] v) pure nothrow @nogc {
        return v[1 .. $].all!(c => c == v[0]);
    }

    bool allNearSame(in Complex!T[] v) pure nothrow @nogc {
        auto v0 = v[0].Complex!T; // To avoid another cast.
        return v[1 .. $].all!(c => feqrel(v0.re, c.re) >= nBits &&
                                   feqrel(v0.im, c.im) >= nBits);
    }

    immutable x = matrices.map!(m => m.length).array;
    if (!allSame(x))
        return false;
    immutable y = matrices.map!(m => m[0].length).array;
    if (!allSame(y))
        return false;
    foreach (immutable s; 0 .. x[0])
        foreach (immutable t; 0 .. y[0])
            if (!allNearSame(matrices.map!(m => m[s][t]).array))
                return false;
    return true;
}

bool isHermitian(T)(in Complex!T[][] m, in Complex!T[][] ct)
pure nothrow /*@safe*/ {
    return [m, ct].areEqual;
}

bool isNormal(T)(in Complex!T[][] m, in Complex!T[][] ct)
pure nothrow /*@safe*/ {
    return [matMul(m, ct), matMul(ct, m)].areEqual;
}

auto complexIdentitymatrix(in size_t side) pure nothrow /*@safe*/ {
    return side.iota.map!(r => side.iota.map!(c => complex(r == c)).array).array;
}

bool isUnitary(T)(in Complex!T[][] m, in Complex!T[][] ct)
pure nothrow /*@safe*/ {
    immutable mct = matMul(m, ct);
    immutable ident = mct.length.complexIdentitymatrix;
    return [mct, matMul(ct, m), ident].areEqual;
}

void main() /*@safe*/ {
    alias C = complex;
    immutable x = 2 ^^ 0.5 / 2;

    immutable data = [[[C(3.0,  0.0), C(2.0, 1.0)],
                       [C(2.0, -1.0), C(1.0, 0.0)]],

                      [[C(1.0, 0.0), C(1.0, 0.0), C(0.0, 0.0)],
                       [C(0.0, 0.0), C(1.0, 0.0), C(1.0, 0.0)],
                       [C(1.0, 0.0), C(0.0, 0.0), C(1.0, 0.0)]],

                      [[C(x,    0.0), C(x,   0.0), C(0.0, 0.0)],
                       [C(0.0, -x),   C(0.0, x),   C(0.0, 0.0)],
                       [C(0.0,  0.0), C(0.0, 0.0), C(0.0, 1.0)]]];

    foreach (immutable mat; data) {
        enum mFormat = "[%([%(%1.3f, %)],\n %)]]";
        writefln("Matrix:\n" ~ mFormat, mat);
        immutable ct = conjugateTranspose(mat);
        "Its conjugate transpose:".writeln;
        writefln(mFormat, ct);
        writefln("Hermitian? %s.", isHermitian(mat, ct));
        writefln("Normal?    %s.", isNormal(mat, ct));
        writefln("Unitary?   %s.\n", isUnitary(mat, ct));
    }
}
```

```txt
Matrix:
[[3.000+0.000i, 2.000+1.000i],
 [2.000-1.000i, 1.000+0.000i]]
Its conjugate transpose:
[[3.000-0.000i, 2.000+1.000i],
 [2.000-1.000i, 1.000-0.000i]]
Hermitian? true.
Normal?    true.
Unitary?   false.

Matrix:
[[1.000+0.000i, 1.000+0.000i, 0.000+0.000i],
 [0.000+0.000i, 1.000+0.000i, 1.000+0.000i],
 [1.000+0.000i, 0.000+0.000i, 1.000+0.000i]]
Its conjugate transpose:
[[1.000-0.000i, 0.000-0.000i, 1.000-0.000i],
 [1.000-0.000i, 1.000-0.000i, 0.000-0.000i],
 [0.000-0.000i, 1.000-0.000i, 1.000-0.000i]]
Hermitian? false.
Normal?    true.
Unitary?   false.

Matrix:
[[0.707+0.000i, 0.707+0.000i, 0.000+0.000i],
 [0.000-0.707i, 0.000+0.707i, 0.000+0.000i],
 [0.000+0.000i, 0.000+0.000i, 0.000+1.000i]]
Its conjugate transpose:
[[0.707-0.000i, 0.000+0.707i, 0.000-0.000i],
 [0.707-0.000i, 0.000-0.707i, 0.000-0.000i],
 [0.000-0.000i, 0.000-0.000i, 0.000-1.000i]]
Hermitian? false.
Normal?    true.
Unitary?   true.

```



### Alternative Version

A more functional version that contains some typing problems (same output).

```d
import std.stdio, std.complex, std.math, std.range, std.algorithm,
       std.numeric, std.exception, std.traits;

// alias CM(T) = Complex!T[][]; // Not yet useful.

auto conjugateTranspose(T)(in Complex!T[][] m) pure nothrow /*@safe*/
if (!hasIndirections!T) {
    return iota(m[0].length).map!(i => m.transversal(i).map!conj.array).array;
}

T[][] matMul(T)(immutable T[][] A, immutable T[][] B) pure nothrow /*@safe*/ {
    immutable Bt = B[0].length.iota.map!(i => B.transversal(i).array).array;
    return A.map!(a => Bt.map!(b => a.dotProduct(b)).array).array;
}

/// Check any number of complex matrices for equality within
/// some bits of mantissa.
bool areEqual(T)(in Complex!T[][][] matrices, in size_t nBits=20)
pure nothrow /*@safe*/ {
    static bool allSame(U)(in U[] v) pure nothrow @nogc @safe {
        return v[1 .. $].all!(c => c == v[0]);
    }

    bool allNearSame(in Complex!T[] v) pure nothrow @nogc @safe {
        auto v0 = v[0].Complex!T; // To avoid another cast.
        return v[1 .. $].all!(c => feqrel(v0.re, c.re) >= nBits &&
                                   feqrel(v0.im, c.im) >= nBits);
    }

    immutable x = matrices.map!(m => m.length).array;
    if (!allSame(x))
        return false;
    immutable y = matrices.map!(m => m[0].length).array;
    if (!allSame(y))
        return false;
    foreach (immutable s; 0 .. x[0])
        foreach (immutable t; 0 .. y[0])
            if (!allNearSame(matrices.map!(m => m[s][t]).array))
                return false;
    return true;
}

bool isHermitian(T)(in Complex!T[][] m, in Complex!T[][] ct)
pure nothrow /*@safe*/ {
    return [m, ct].areEqual;
}

bool isNormal(T)(immutable Complex!T[][] m, immutable Complex!T[][] ct)
pure nothrow /*@safe*/ {
    return [matMul(m, ct), matMul(ct, m)].areEqual;
}

auto complexIdentitymatrix(in size_t side) pure nothrow /*@safe*/ {
    return side.iota.map!(r => side.iota.map!(c => complex(r == c)).array).array;
}

bool isUnitary(T)(immutable Complex!T[][] m, immutable Complex!T[][] ct)
pure nothrow /*@safe*/ {
    immutable mct = matMul(m, ct);
    immutable ident = mct.length.complexIdentitymatrix;
    return [mct, matMul(ct, m), ident].areEqual;
}

void main() {
    alias C = complex;
    immutable x = 2 ^^ 0.5 / 2;

    foreach (/*immutable*/ const matrix;
        [[[C(3.0,  0.0), C(2.0, 1.0)],
          [C(2.0, -1.0), C(1.0, 0.0)]],

         [[C(1.0, 0.0), C(1.0, 0.0), C(0.0, 0.0)],
          [C(0.0, 0.0), C(1.0, 0.0), C(1.0, 0.0)],
          [C(1.0, 0.0), C(0.0, 0.0), C(1.0, 0.0)]],

         [[C(x,    0.0), C(x,   0.0), C(0.0, 0.0)],
          [C(0.0, -x),   C(0.0, x),   C(0.0, 0.0)],
          [C(0.0,  0.0), C(0.0, 0.0), C(0.0, 1.0)]]]) {
        immutable mat = matrix.assumeUnique; //*

        enum mFormat = "[%([%(%1.3f, %)],\n %)]]";
        writefln("Matrix:\n" ~ mFormat, mat);
        immutable ct = conjugateTranspose(mat);
        "Its conjugate transpose:".writeln;
        writefln(mFormat, ct);
        writefln("Hermitian? %s.", isHermitian(mat, ct));
        writefln("Normal?    %s.", isNormal(mat, ct));
        writefln("Unitary?   %s.\n", isUnitary(mat, ct));
    }
}
```



## Factor

Before the fix to [https://github.com/slavapestov/factor/issues/484 Factor bug #484], <code>m.</code> gave the wrong answer and this code failed. Factor 0.94 is too old to work.

```factor
USING: kernel math.functions math.matrices sequences ;
IN: rosetta.hermitian

: conj-t ( matrix -- conjugate-transpose )
    flip [ [ conjugate ] map ] map ;

: hermitian-matrix? ( matrix -- ? )
    dup conj-t = ;

: normal-matrix? ( matrix -- ? )
    dup conj-t [ m. ] [ swap m. ] 2bi = ;

: unitary-matrix? ( matrix -- ? )
    [ dup conj-t m. ] [ length identity-matrix ] bi = ;
```


Usage:

 '''USE: rosetta.hermitian'''
 IN: scratchpad '''{ { C{ 1 2 } 0 }'''
                '''  { 0 C{ 3 4 } } }'''
                '''[ hermitian-matrix? . ]'''
                '''[ normal-matrix? . ]'''
                '''[ unitary-matrix? . ] tri'''
 f
 t
 f


## Fortran


The examples and algorithms are taken from the j solution, except for UnitaryQ.  The j solution uses the matrix inverse verb.  Compilation on linux, assuming the program is file f.f08 :
```txt

gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
```


```FORTRAN

program conjugate_transpose

  complex, dimension(3, 3) :: a
  integer :: i
  a = reshape((/ (i, i=1,9) /), shape(a))
  call characterize(a)
  a(:2,:2) = reshape((/cmplx(3,0),cmplx(2,-1),cmplx(2,1),cmplx(1,0)/),(/2,2/))
  call characterize(a(:2,:2))
  call characterize(cmplx(reshape((/1,0,1,1,1,0,0,1,1/),(/3,3/)),0))
  a(3,:) = (/cmplx(0,0), cmplx(0,0), cmplx(0,1)/)*sqrt(2.0)
  a(2,:) = (/cmplx(0,-1),cmplx(0,1),cmplx(0,0)/)
  a(1,:) = (/1,1,0/)
  a = a * sqrt(2.0)/2.0
  call characterize(a)

contains

  subroutine characterize(a)
    complex, dimension(:,:), intent(in) :: a
    integer :: i, j
    do i=1, size(a,1)
       print *,(a(i, j), j=1,size(a,1))
    end do
    print *,'Is Hermitian?  ',HermitianQ(a)
    print *,'Is normal?  ',NormalQ(a)
    print *,'Unitary?  ',UnitaryQ(a)
    print '(/)'
  end subroutine characterize

  function ct(a) result(b) ! return the conjugate transpose of a matrix
    complex, dimension(:,:), intent(in) :: a
    complex, dimension(size(a,1),size(a,1)) :: b
    b = conjg(transpose(a))
  end function ct

  function identity(n) result(b) ! return identity matrix
    integer, intent(in) :: n
    real, dimension(n,n) :: b
    integer :: i
    b = 0
    do i=1, n
       b(i,i) = 1
    end do
  end function identity

  logical function HermitianQ(a)
    complex, dimension(:,:), intent(in) :: a
    HermitianQ = all(a .eq. ct(a))
  end function HermitianQ

  logical function NormalQ(a)
    complex, dimension(:,:), intent(in) :: a
    NormalQ = all(matmul(ct(a),a) .eq. matmul(a,ct(a)))
  end function NormalQ

  logical function UnitaryQ(a)
    ! if  A inverse equals A star
    ! then multiplying each side by A should result in the identity matrix
    ! Thus show that  A times A star  is sufficiently close to  I .
    complex, dimension(:,:), intent(in) :: a
    UnitaryQ = all(abs(matmul(a,ct(a)) - identity(size(a,1))) .lt. 1e-6)
  end function UnitaryQ

end program conjugate_transpose

```


```txt

-*- mode: compilation; default-directory: "/tmp/" -*-
Compilation started at Fri Jun  7 16:31:38

a=./f && make $a && time $a
gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
 (  1.00000000    ,  0.00000000    ) (  4.00000000    ,  0.00000000    ) (  7.00000000    ,  0.00000000    )
 (  2.00000000    ,  0.00000000    ) (  5.00000000    ,  0.00000000    ) (  8.00000000    ,  0.00000000    )
 (  3.00000000    ,  0.00000000    ) (  6.00000000    ,  0.00000000    ) (  9.00000000    ,  0.00000000    )
 Is Hermitian?   F
 Is normal?   F
 Unitary?   F


 (  3.00000000    ,  0.00000000    ) (  2.00000000    ,  1.00000000    )
 (  2.00000000    , -1.00000000    ) (  1.00000000    ,  0.00000000    )
 Is Hermitian?   T
 Is normal?   T
 Unitary?   F


 (  1.00000000    ,  0.00000000    ) (  1.00000000    ,  0.00000000    ) (  0.00000000    ,  0.00000000    )
 (  0.00000000    ,  0.00000000    ) (  1.00000000    ,  0.00000000    ) (  1.00000000    ,  0.00000000    )
 (  1.00000000    ,  0.00000000    ) (  0.00000000    ,  0.00000000    ) (  1.00000000    ,  0.00000000    )
 Is Hermitian?   F
 Is normal?   T
 Unitary?   F


 ( 0.707106769    ,  0.00000000    ) ( 0.707106769    ,  0.00000000    ) (  0.00000000    ,  0.00000000    )
 (  0.00000000    ,-0.707106769    ) (  0.00000000    , 0.707106769    ) (  0.00000000    ,  0.00000000    )
 (  0.00000000    ,  0.00000000    ) (  0.00000000    ,  0.00000000    ) (  0.00000000    , 0.999999940    )
 Is Hermitian?   F
 Is normal?   T
 Unitary?   T



real	0m0.002s
user	0m0.000s
sys	0m0.000s

Compilation finished at Fri Jun  7 16:31:38

```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

// a type to represent matrices
type matrix struct {
    ele  []complex128
    cols int
}

// conjugate transpose, implemented here as a method on the matrix type.
func (m *matrix) conjTranspose() *matrix {
    r := &matrix{make([]complex128, len(m.ele)), len(m.ele) / m.cols}
    rx := 0
    for _, e := range m.ele {
        r.ele[rx] = cmplx.Conj(e)
        rx += r.cols
        if rx >= len(r.ele) {
            rx -= len(r.ele) - 1
        }
    }
    return r
}

// program to demonstrate capabilites on example matricies
func main() {
    show("h", matrixFromRows([][]complex128{
        {3, 2 + 1i},
        {2 - 1i, 1}}))

    show("n", matrixFromRows([][]complex128{
        {1, 1, 0},
        {0, 1, 1},
        {1, 0, 1}}))

    show("u", matrixFromRows([][]complex128{
        {math.Sqrt2 / 2, math.Sqrt2 / 2, 0},
        {math.Sqrt2 / -2i, math.Sqrt2 / 2i, 0},
        {0, 0, 1i}}))
}

func show(name string, m *matrix) {
    m.print(name)
    ct := m.conjTranspose()
    ct.print(name + "_ct")

    fmt.Println("Hermitian:", m.equal(ct, 1e-14))

    mct := m.mult(ct)
    ctm := ct.mult(m)
    fmt.Println("Normal:", mct.equal(ctm, 1e-14))

    i := eye(m.cols)
    fmt.Println("Unitary:", mct.equal(i, 1e-14) && ctm.equal(i, 1e-14))
}

// two constructors
func matrixFromRows(rows [][]complex128) *matrix {
    m := &matrix{make([]complex128, len(rows)*len(rows[0])), len(rows[0])}
    for rx, row := range rows {
        copy(m.ele[rx*m.cols:(rx+1)*m.cols], row)
    }
    return m
}

func eye(n int) *matrix {
    r := &matrix{make([]complex128, n*n), n}
    n++
    for x := 0; x < len(r.ele); x += n {
        r.ele[x] = 1
    }
    return r
}

// print method outputs matrix to stdout
func (m *matrix) print(heading string) {
    fmt.Print("\n", heading, "\n")
    for e := 0; e < len(m.ele); e += m.cols {
        fmt.Printf("%6.3f ", m.ele[e:e+m.cols])
        fmt.Println()
    }
}

// equal method uses ε to allow for floating point error.
func (a *matrix) equal(b *matrix, ε float64) bool {
    for x, aEle := range a.ele {
        if math.Abs(real(aEle)-real(b.ele[x])) > math.Abs(real(aEle))*ε ||
            math.Abs(imag(aEle)-imag(b.ele[x])) > math.Abs(imag(aEle))*ε {
            return false
        }
    }
    return true
}

// mult method taken from matrix multiply task
func (m1 *matrix) mult(m2 *matrix) (m3 *matrix) {
    m3 = &matrix{make([]complex128, (len(m1.ele)/m1.cols)*m2.cols), m2.cols}
    for m1c0, m3x := 0, 0; m1c0 < len(m1.ele); m1c0 += m1.cols {
        for m2r0 := 0; m2r0 < m2.cols; m2r0++ {
            for m1x, m2x := m1c0, m2r0; m2x < len(m2.ele); m2x += m2.cols {
                m3.ele[m3x] += m1.ele[m1x] * m2.ele[m2x]
                m1x++
            }
            m3x++
        }
    }
    return m3
}
```

Output:

```txt

h
[( 3.000+0.000i) (+2.000+1.000i)] 
[( 2.000-1.000i) (+1.000+0.000i)] 

h_ct
[( 3.000-0.000i) (+2.000+1.000i)] 
[( 2.000-1.000i) (+1.000-0.000i)] 
Hermitian: true
Normal: true
Unitary: false

n
[( 1.000+0.000i) (+1.000+0.000i) (+0.000+0.000i)] 
[( 0.000+0.000i) (+1.000+0.000i) (+1.000+0.000i)] 
[( 1.000+0.000i) (+0.000+0.000i) (+1.000+0.000i)] 

n_ct
[( 1.000-0.000i) (+0.000-0.000i) (+1.000-0.000i)] 
[( 1.000-0.000i) (+1.000-0.000i) (+0.000-0.000i)] 
[( 0.000-0.000i) (+1.000-0.000i) (+1.000-0.000i)] 
Hermitian: false
Normal: true
Unitary: false

u
[( 0.707+0.000i) (+0.707+0.000i) (+0.000+0.000i)] 
[( 0.000+0.707i) (+0.000-0.707i) (+0.000+0.000i)] 
[( 0.000+0.000i) (+0.000+0.000i) (+0.000+1.000i)] 

u_ct
[( 0.707-0.000i) (+0.000-0.707i) (+0.000-0.000i)] 
[( 0.707-0.000i) (+0.000+0.707i) (+0.000-0.000i)] 
[( 0.000-0.000i) (+0.000-0.000i) (+0.000-1.000i)] 
Hermitian: false
Normal: true
Unitary: true

```



## Haskell

Slow implementation using lists.

```haskell
import Data.List (transpose)
import Data.Complex

type Matrix a = [[a]]

main :: IO ()
main =
    mapM_ (\a -> do
        putStrLn "\nMatrix:"
        mapM_ print a
        putStrLn "Conjugate Transpose:"
        mapM_ print (conjTranspose a)
        putStrLn $ "Hermitian? " ++ show (isHermitianMatrix a)
        putStrLn $ "Normal? " ++ show (isNormalMatrix a)
        putStrLn $ "Unitary? " ++ show (isUnitaryMatrix a))
        ([[[3,       2:+1],
           [2:+(-1), 1   ]],

          [[1, 1, 0],
           [0, 1, 1],
           [1, 0, 1]],

          [[sqrt 2/2:+0, sqrt 2/2:+0,     0   ],
           [0:+sqrt 2/2, 0:+ (-sqrt 2/2), 0   ],
           [0,           0,               0:+1]]] :: [Matrix (Complex Double)])

isHermitianMatrix, isNormalMatrix, isUnitaryMatrix :: RealFloat a => Matrix (Complex a) -> Bool
isHermitianMatrix a = a `approxEqualMatrix` conjTranspose a
isNormalMatrix a = (a `mmul` conjTranspose a) `approxEqualMatrix` (conjTranspose a `mmul` a)
isUnitaryMatrix a = (a `mmul` conjTranspose a) `approxEqualMatrix` ident (length a)

approxEqualMatrix :: (Fractional a, Ord a) => Matrix (Complex a) -> Matrix (Complex a) -> Bool
approxEqualMatrix a b = length a == length b && length (head a) == length (head b) &&
                        and (zipWith approxEqualComplex (concat a) (concat b))
    where approxEqualComplex (rx :+ ix) (ry :+ iy) = abs (rx - ry) < eps && abs (ix - iy) < eps
          eps = 1e-14

mmul :: Num a => Matrix a -> Matrix a -> Matrix a
mmul a b = [[sum (zipWith (*) row column) | column <- transpose b] | row <- a]

ident :: Num a => Int -> Matrix a
ident size = [[fromIntegral $ div a b * div b a | a <- [1..size]] | b <- [1..size]]

conjTranspose :: Num a => Matrix (Complex a) -> Matrix (Complex a)
conjTranspose = map (map conjugate) . transpose
```

Output:

```txt

Matrix:
[3.0 :+ 0.0,2.0 :+ 1.0]
[2.0 :+ (-1.0),1.0 :+ 0.0]
Conjugate Transpose:
[3.0 :+ (-0.0),2.0 :+ 1.0]
[2.0 :+ (-1.0),1.0 :+ (-0.0)]
Hermitian? True
Normal? True
Unitary? False

Matrix:
[1.0 :+ 0.0,1.0 :+ 0.0,0.0 :+ 0.0]
[0.0 :+ 0.0,1.0 :+ 0.0,1.0 :+ 0.0]
[1.0 :+ 0.0,0.0 :+ 0.0,1.0 :+ 0.0]
Conjugate Transpose:
[1.0 :+ (-0.0),0.0 :+ (-0.0),1.0 :+ (-0.0)]
[1.0 :+ (-0.0),1.0 :+ (-0.0),0.0 :+ (-0.0)]
[0.0 :+ (-0.0),1.0 :+ (-0.0),1.0 :+ (-0.0)]
Hermitian? False
Normal? True
Unitary? False

Matrix:
[0.7071067811865476 :+ 0.0,0.7071067811865476 :+ 0.0,0.0 :+ 0.0]
[0.0 :+ 0.7071067811865476,0.0 :+ (-0.7071067811865476),0.0 :+ 0.0]
[0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 1.0]
Conjugate Transpose:
[0.7071067811865476 :+ (-0.0),0.0 :+ (-0.7071067811865476),0.0 :+ (-0.0)]
[0.7071067811865476 :+ (-0.0),0.0 :+ 0.7071067811865476,0.0 :+ (-0.0)]
[0.0 :+ (-0.0),0.0 :+ (-0.0),0.0 :+ (-1.0)]
Hermitian? False
Normal? True
Unitary? True

```


## J


'''Solution''': 
```j
   ct =: +@|:                      NB.  Conjugate transpose (ct A is A_ct)
```

'''Examples''': 
```j
   X         =: +/ . *             NB. Matrix Multiply (x)

   HERMITIAN =:  3 2j1 ,: 2j_1 1  
   (-: ct) HERMITIAN               NB.  A_ct = A
1

   NORMAL    =:  1 1 0 , 0 1 1 ,: 1 0 1
   ((X~ -: X) ct) NORMAL           NB. A_ct x A = A x A_ct
1

   UNITARY   =:  (-:%:2) * 1 1 0 , 0j_1 0j1 0 ,: 0 0 0j1 * %:2
   (ct -: %.)  UNITARY             NB.  A_ct = A^-1
1
```


'''Reference''' (example matrices for other langs to use):
```j
   HERMITIAN;NORMAL;UNITARY
+--------+-----+--------------------------+
|   3 2j1|1 1 0|   0.707107   0.707107   0|
|2j_1   1|0 1 1|0j_0.707107 0j0.707107   0|
|        |1 0 1|          0          0 0j1|
+--------+-----+--------------------------+
   NB. In J, PjQ is P + Q*i and the 0.7071... is sqrt(2)

   hermitian=: -: ct
   normal =: (X~ -: X) ct
   unitary=: ct -: %.

   (hermitian,normal,unitary)&.>HERMITIAN;NORMAL;UNITARY
+-----+-----+-----+
|1 1 0|0 1 0|0 1 1|
+-----+-----+-----+
```



## jq

In the following, we use the array [x,y] to represent the complex number x + iy,
but the following functions also accept a number wherever a complex number is acceptable.


### =Infrastructure=

'''(1) transpose/0''':

If your jq does not have "transpose" then the following may be used:

```jq
# transpose/0 expects its input to be a rectangular matrix
# (an array of equal-length arrays):
def transpose:
  if (.[0] | length) == 0 then []
  else [map(.[0])] + (map(.[1:]) | transpose)
  end ;
```

'''(2) Operations on real/complex numbers'''

```jq
# x must be real or complex, and ditto for y;
# always return complex
def plus(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x+y, 0 ]
       else [ x + y[0], y[1]]
       end
    elif (y|type) == "number" then plus(y;x)
    else [ x[0] + y[0], x[1] + y[1] ]
    end;

# x must be real or complex, and ditto for y;
# always return complex
def multiply(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x*y, 0 ]
       else [x * y[0], x * y[1]]
       end
    elif (y|type) == "number" then multiply(y;x)
    else [ x[0] * y[0] - x[1] * y[1],  x[0] * y[1] + x[1] * y[0]]
    end;

# conjugate of a real or complex number
def conjugate:
  if type == "number" then [.,0]
  else [.[0], -(.[1]) ]
  end;
```

'''(3) Array operations'''

```jq
# a and b are arrays of real/complex numbers
def dot_product(a; b):
  a as $a | b as $b
  | reduce range(0;$a|length) as $i
      (0; . as $s | plus($s; multiply($a[$i]; $b[$i]) ));
```

'''(4) Matrix operations'''

```jq
# convert a matrix of mixed real/complex entries to all complex entries
def to_complex:
  def toc: if type == "number" then [.,0] else . end;
  map( map(toc) );

# simple matrix pretty-printer
def pp(wide):
  def pad: tostring | (wide - length) * " " + .;
  def row: reduce .[] as $x (""; . + ($x|pad));
  reduce .[] as $row (""; . + "\n\($row|row)");

# Matrix multiplication
# A and B should both be real/complex matrices,
# A being m by n, and B being n by p.
def matrix_multiply(A; B):
  A as $A | B as $B
  | ($B[0]|length) as $p
  | ($B|transpose) as $BT
  | reduce range(0; $A|length) as $i
       ([]; reduce range(0; $p) as $j 
         (.; .[$i][$j] = dot_product( $A[$i]; $BT[$j] ) )) ;

# Complex identity matrix of dimension n
def complex_identity(n):
  def indicator(i;n):  [range(0;n)] | map( [0,0]) | .[i] = [1,0];
  reduce range(0; n) as $i ([]; . + [indicator( $i; n )] );

# Approximate equality of two matrices
# Are two real/complex matrices essentially equal
# in the sense that the sum of the squared element-wise differences
# is less than or equal to epsilon?
# The two matrices must be conformal.
def approximately_equal(M; N; epsilon):
  def norm: multiply(. ; conjugate ) | .[0];
  def sqdiff( x; y): plus(x; multiply(y; -1)) | norm;
  reduce range(0;M|length) as $i
    (0;  reduce range(0; M[0]|length) as $j
      (.; 0 + sqdiff( M[$i][$j]; N[$i][$j] ) ) ) <= epsilon;
```


### =Conjugate transposition=


```jq
# (entries may be real and/or complex)
def conjugate_transpose:
  map( map(conjugate) ) | transpose;

# A Hermitian matrix equals its own conjugate transpose
def is_hermitian:
  to_complex == conjugate_transpose;

# A matrix is normal if it commutes multiplicatively
# with its conjugate transpose
def is_normal:
  . as $M
  | conjugate_transpose as $H
  | matrix_multiply($H; $M) == matrix_multiply($H; $M);

# A unitary matrix (U) has its inverse equal to its conjugate transpose (T)
# i.e. U^-1 == T; NASC is I == UT == TU
def is_unitary:
  . as $M
  | conjugate_transpose as $H
  | complex_identity(length) as $I
  | approximately_equal( $I; matrix_multiply($H;$M); 1e-10)
    and approximately_equal( $I ; matrix_multiply($M;$H); 1e-10)  ; 
```



### =Examples=


```jq
def hermitian_example:
  [ [ 3,    [2,1]],
    [[2,-1], 1   ] ];

def normal_example:
  [ [1, 1, 0],
    [0, 1, 1],
    [1, 0, 1] ];

def unitary_example:
  0.707107
  |  [ [ [., 0], [.,  0],   0 ],
       [ [0, -.], [0, .],   0 ],
       [ 0,      0,      [0,1] ] ];

def demo:
  hermitian_example
  | ("Hermitian example:", pp(8)),
    "",
    ("Its conjugate transpose is:",  (to_complex | conjugate_transpose | pp(8))),
    "",
    "Hermitian example: \(hermitian_example | is_hermitian )",
    "",
    "Normal example:    \(normal_example    | is_normal )",
    "",
    "Unitary example:   \(unitary_example   | is_unitary)"
;

demo
```

```sh
$ jq -r -c -n -f Conjugate_transpose.jq
Hermitian example:

       3   [2,1]
  [2,-1]       1

Conjugate transpose:

  [3,-0]   [2,1]
  [2,-1]  [1,-0]

Hermitian example: true

Normal example:    true

Unitary example:   true
```




## Julia

Julia has a built-in matrix type, and the conjugate-transpose of a complex matrix <code>A</code> is simply:

```julia
A'
```

(similar to Matlab).   You can check whether <code>A</code> is Hermitian via the built-in function

```julia
ishermitian(A)
```

Ignoring the possibility of roundoff errors for floating-point matrices (like most of the examples in the other languages), you can check whether a matrix is normal or unitary by the following functions

```julia
eye(A) = A^0
isnormal(A) = size(A,1) == size(A,2) && A'*A == A*A'
isunitary(A) = size(A,1) == size(A,2) && A'*A == eye(A)
```



## Kotlin

As Kotlin doesn't have built in classes for complex numbers or matrices, some basic functionality needs to be coded in order to tackle this task:

```scala
// version 1.1.3

typealias C = Complex
typealias Vector = Array<C>
typealias Matrix = Array<Vector>

class Complex(val real: Double, val imag: Double) {

    operator fun plus(other: Complex) =
        Complex(this.real + other.real, this.imag + other.imag)

    operator fun times(other: Complex) =
        Complex(this.real * other.real - this.imag * other.imag,
                this.real * other.imag + this.imag * other.real)

    fun conj() = Complex(this.real, -this.imag)

    /* tolerable equality allowing for rounding of Doubles */
    infix fun teq(other: Complex) =
        Math.abs(this.real - other.real) <= 1e-14 &&
        Math.abs(this.imag - other.imag) <= 1e-14

    override fun toString() = "${"%.3f".format(real)} " + when {
        imag > 0.0   -> "+ ${"%.3f".format(imag)}i"
        imag == 0.0  -> "+ 0.000i"
        else         -> "- ${"%.3f".format(-imag)}i"
    }
}

fun Matrix.conjTranspose(): Matrix {
    val rows = this.size
    val cols = this[0].size
    return Matrix(cols) { i -> Vector(rows) { j -> this[j][i].conj() } }
}

operator fun Matrix.times(other: Matrix): Matrix {
    val rows1 = this.size
    val cols1 = this[0].size
    val rows2 = other.size
    val cols2 = other[0].size
    require(cols1 == rows2)
    val result = Matrix(rows1) { Vector(cols2) { C(0.0, 0.0) } }
    for (i in 0 until rows1) {
        for (j in 0 until cols2) {
            for (k in 0 until rows2) {
                result[i][j] += this[i][k] * other[k][j]
            }
        }
    }
    return result
}

/* tolerable matrix equality using the same concept as for complex numbers */
infix fun Matrix.teq(other: Matrix): Boolean {
    if (this.size != other.size || this[0].size != other[0].size) return false
    for (i in 0 until this.size) {
        for (j in 0 until this[0].size) if (!(this[i][j] teq other[i][j])) return false
    }
    return true
}

fun Matrix.isHermitian() = this teq this.conjTranspose()

fun Matrix.isNormal(): Boolean {
    val ct = this.conjTranspose()
    return (this * ct) teq (ct * this)
}

fun Matrix.isUnitary(): Boolean {
    val ct = this.conjTranspose()
    val prod = this * ct
    val ident = identityMatrix(prod.size)
    val prod2 = ct * this
    return (prod teq ident) && (prod2 teq ident)
}

fun Matrix.print() {
    val rows = this.size
    val cols = this[0].size
    for (i in 0 until rows) {
        for (j in 0 until cols) {
            print(this[i][j])
            print(if(j < cols - 1) ",  " else "\n")
        }
    }
    println()
}

fun identityMatrix(n: Int): Matrix {
    require(n >= 1)
    val ident = Matrix(n) { Vector(n) { C(0.0, 0.0) } }
    for (i in 0 until n) ident[i][i] = C(1.0, 0.0)
    return ident
}

fun main(args: Array<String>) {
    val x = Math.sqrt(2.0) / 2.0
    val matrices = arrayOf(
        arrayOf(
            arrayOf(C(3.0,  0.0), C(2.0, 1.0)),
            arrayOf(C(2.0, -1.0), C(1.0, 0.0))
        ),
        arrayOf(
            arrayOf(C(1.0, 0.0), C(1.0, 0.0), C(0.0, 0.0)),
            arrayOf(C(0.0, 0.0), C(1.0, 0.0), C(1.0, 0.0)),
            arrayOf(C(1.0, 0.0), C(0.0, 0.0), C(1.0, 0.0))
        ),
        arrayOf(
            arrayOf(C(x,   0.0), C(x,   0.0), C(0.0, 0.0)),
            arrayOf(C(0.0,  -x), C(0.0,   x), C(0.0, 0.0)),
            arrayOf(C(0.0, 0.0), C(0.0, 0.0), C(0.0, 1.0))
        )
    )

    for (m in matrices) {
        println("Matrix:")
        m.print()
        val mct = m.conjTranspose()
        println("Conjugate transpose:")
        mct.print()
        println("Hermitian? ${mct.isHermitian()}")
        println("Normal?    ${mct.isNormal()}")
        println("Unitary?   ${mct.isUnitary()}\n")
    }
}
```


```txt

Matrix:
3.000 + 0.000i,  2.000 + 1.000i
2.000 - 1.000i,  1.000 + 0.000i

Conjugate transpose:
3.000 + 0.000i,  2.000 + 1.000i
2.000 - 1.000i,  1.000 + 0.000i

Hermitian? true
Normal?    true
Unitary?   false

Matrix:
1.000 + 0.000i,  1.000 + 0.000i,  0.000 + 0.000i
0.000 + 0.000i,  1.000 + 0.000i,  1.000 + 0.000i
1.000 + 0.000i,  0.000 + 0.000i,  1.000 + 0.000i

Conjugate transpose:
1.000 + 0.000i,  0.000 + 0.000i,  1.000 + 0.000i
1.000 + 0.000i,  1.000 + 0.000i,  0.000 + 0.000i
0.000 + 0.000i,  1.000 + 0.000i,  1.000 + 0.000i

Hermitian? false
Normal?    true
Unitary?   false

Matrix:
0.707 + 0.000i,  0.707 + 0.000i,  0.000 + 0.000i
0.000 - 0.707i,  0.000 + 0.707i,  0.000 + 0.000i
0.000 + 0.000i,  0.000 + 0.000i,  0.000 + 1.000i

Conjugate transpose:
0.707 + 0.000i,  0.000 + 0.707i,  0.000 + 0.000i
0.707 + 0.000i,  0.000 - 0.707i,  0.000 + 0.000i
0.000 + 0.000i,  0.000 + 0.000i,  0.000 - 1.000i

Hermitian? false
Normal?    true
Unitary?   true

```



## Maple

The commands <code>HermitianTranspose</code> and <code>IsUnitary</code> are provided by the <code>LinearAlgebra</code> package.

```Maple
M:=<<3|2+I>,<2-I|1>>:

with(LinearAlgebra):
IsNormal:=A->EqualEntries(A^%H.A,A.A^%H):

M^%H;
HermitianTranspose(M);
type(M,'Matrix'(hermitian));
IsNormal(M);
IsUnitary(M);
```

Output:

```txt
                               [  3    2 + I]
                               [            ]
                               [2 - I    1  ]

                               [  3    2 + I]
                               [            ]
                               [2 - I    1  ]

                                    true

                                    true

                                    false
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
NormalMatrixQ[a_List?MatrixQ] := Module[{b = Conjugate@Transpose@a},a.b === b.a]
UnitaryQ[m_List?MatrixQ] := (Conjugate@Transpose@m.m == IdentityMatrix@Length@m)

m = {{1, 2I, 3}, {3+4I, 5, I}};
m //MatrixForm
->
(1	2I	3
3+4I	5	I)

ConjugateTranspose[m] //MatrixForm
->
(1	3-4I
-2I	5
3	-I)

{HermitianMatrixQ@#, NormalMatrixQ@#, UnitaryQ@#}&@m
-> {False, False, False}
```



## PARI/GP

<lang>conjtranspose(M)=conj(M~)
isHermitian(M)=M==conj(M~)
isnormal(M)=my(H=conj(M~));H*M==M*H
isunitary(M)=M*conj(M~)==1
```



## Perl

In general, using two or more modules which overload operators can be problematic. For this task, using both Math::Complex and Math::MatrixReal gives us the behavior we want for everything except matrix I/O, i.e. parsing and stringification.

```perl
use strict;
use English;
use Math::Complex;
use Math::MatrixReal;

my @examples = (example1(), example2(), example3());
foreach my $m (@examples) {
    print "Starting matrix:\n", cmat_as_string($m), "\n";
    my $m_ct = conjugate_transpose($m);
    print "Its conjugate transpose:\n", cmat_as_string($m_ct), "\n";
    print "Is Hermitian? ", (cmats_are_equal($m, $m_ct) ? 'TRUE' : 'FALSE'), "\n";
    my $product = $m_ct * $m;
    print "Is normal? ", (cmats_are_equal($product, $m * $m_ct) ? 'TRUE' : 'FALSE'), "\n";
    my $I = identity(($m->dim())[0]);
    print "Is unitary? ", (cmats_are_equal($product, $I) ? 'TRUE' : 'FALSE'), "\n";
    print "\n";
}
exit 0;

sub cmats_are_equal {
    my ($m1, $m2) = @ARG;
    my $max_norm = 1.0e-7;
    return abs($m1 - $m2) < $max_norm;  # Math::MatrixReal overloads abs().
}

# Note that Math::Complex and Math::MatrixReal both overload '~', for
# complex conjugates and matrix transpositions respectively.
sub conjugate_transpose {
    my $m_T = ~ shift;
    my $result = $m_T->each(sub {~ $ARG[0]});
    return $result;
}

sub cmat_as_string {
    my $m = shift;
    my $n_rows = ($m->dim())[0];
    my @row_strings = map { q{[} . join(q{, }, $m->row($ARG)->as_list) . q{]} }
                          (1 .. $n_rows);
    return join("\n", @row_strings);
}

sub identity {
    my $N = shift;
    my $m = new Math::MatrixReal($N, $N);
    $m->one();
    return $m;
}

sub example1 {
    my $m = new Math::MatrixReal(2, 2);
    $m->assign(1, 1, cplx(3, 0));
    $m->assign(1, 2, cplx(2, 1));
    $m->assign(2, 1, cplx(2, -1));
    $m->assign(2, 2, cplx(1, 0));
    return $m;
}

sub example2 {
    my $m = new Math::MatrixReal(3, 3);
    $m->assign(1, 1, cplx(1, 0));
    $m->assign(1, 2, cplx(1, 0));
    $m->assign(1, 3, cplx(0, 0));
    $m->assign(2, 1, cplx(0, 0));
    $m->assign(2, 2, cplx(1, 0));
    $m->assign(2, 3, cplx(1, 0));
    $m->assign(3, 1, cplx(1, 0));
    $m->assign(3, 2, cplx(0, 0));
    $m->assign(3, 3, cplx(1, 0));
    return $m;
}

sub example3 {
    my $m = new Math::MatrixReal(3, 3);
    $m->assign(1, 1, cplx(0.70710677, 0));
    $m->assign(1, 2, cplx(0.70710677, 0));
    $m->assign(1, 3, cplx(0, 0));
    $m->assign(2, 1, cplx(0, -0.70710677));
    $m->assign(2, 2, cplx(0, 0.70710677));
    $m->assign(2, 3, cplx(0, 0));
    $m->assign(3, 1, cplx(0, 0));
    $m->assign(3, 2, cplx(0, 0));
    $m->assign(3, 3, cplx(0, 1));
    return $m;
}
```

```txt

Starting matrix:
[3, 2+i]
[2-i, 1]
Its conjugate transpose:
[3, 2+i]
[2-i, 1]
Is Hermitian? TRUE
Is normal? TRUE
Is unitary? FALSE

Starting matrix:
[1, 1, 0]
[0, 1, 1]
[1, 0, 1]
Its conjugate transpose:
[1, 0, 1]
[1, 1, 0]
[0, 1, 1]
Is Hermitian? FALSE
Is normal? TRUE
Is unitary? FALSE

Starting matrix:
[0.70710677, 0.70710677, 0]
[-0.70710677i, 0.70710677i, 0]
[0, 0, i]
Its conjugate transpose:
[0.70710677, 0.70710677i, 0]
[0.70710677, -0.70710677i, 0]
[0, 0, -i]
Is Hermitian? FALSE
Is normal? TRUE
Is unitary? TRUE

```



## Perl 6

```perl6
for [ # Test Matrices
       [   1, 1+i, 2i],
       [ 1-i,   5, -3],
       [0-2i,  -3,  0]
    ],
    [
       [1, 1, 0],
       [0, 1, 1],
       [1, 0, 1]
    ],
    [
       [0.707 ,    0.707,  0],
       [0.707i, 0-0.707i,  0],
       [0     ,        0,  i]
    ]
    -> @m {
        say "\nMatrix:";
        @m.&say-it;
        my @t = @m».conj.&mat-trans;
        say "\nTranspose:";
        @t.&say-it;
        say "Is Hermitian?\t{is-Hermitian(@m, @t)}";
        say "Is Normal?\t{is-Normal(@m, @t)}";
        say "Is Unitary?\t{is-Unitary(@m, @t)}";
    }

sub is-Hermitian (@m, @t, --> Bool) {
    so @m».Complex eqv @t».Complex
 }

sub is-Normal (@m, @t, --> Bool) {
    so mat-mult(@m, @t)».Complex eqv mat-mult(@t, @m)».Complex
}

sub is-Unitary (@m, @t, --> Bool) {
    so mat-mult(@m, @t, 1e-3)».Complex eqv mat-ident(+@m)».Complex;
}

sub mat-trans (@m) { map { [ @m[*;$_] ] }, ^@m[0] }

sub mat-ident ($n) { [ map { [ flat 0 xx $_, 1, 0 xx $n - 1 - $_ ] }, ^$n ] }

sub mat-mult (@a, @b, \ε = 1e-15) {
    my @p;
    for ^@a X ^@b[0] -> ($r, $c) {
        @p[$r][$c] += @a[$r][$_] * @b[$_][$c] for ^@b;
        @p[$r][$c].=round(ε); # avoid floating point math errors
    }
    @p
}

sub say-it (@array) { $_».fmt("%9s").say for @array }
```

```txt
Matrix:
[        1      1+1i      0+2i]
[     1-1i         5        -3]
[     0-2i        -3         0]

Transpose:
[        1      1+1i      0+2i]
[     1-1i         5        -3]
[     0-2i        -3         0]
Is Hermitian?	True
Is Normal?	True
Is Unitary?	False

Matrix:
[        1         1         0]
[        0         1         1]
[        1         0         1]

Transpose:
[        1         0         1]
[        1         1         0]
[        0         1         1]
Is Hermitian?	False
Is Normal?	True
Is Unitary?	False

Matrix:
[    0.707     0.707         0]
[ 0+0.707i  0-0.707i         0]
[        0         0      0+1i]

Transpose:
[    0.707  0-0.707i         0]
[    0.707  0+0.707i         0]
[        0         0      0-1i]
Is Hermitian?	False
Is Normal?	True
Is Unitary?	True

```



## Phix

Phix has no support for complex numbers, so roll our own, ditto matrix maths. Note this code has no testing for non-square matrices.

```Phix
enum REAL, IMAG
 
type complex(sequence s)
    return length(s)=2 and atom(s[REAL]) and atom(s[IMAG])
end type
 
function c_add(complex a, complex b)
    return sq_add(a,b)
end function
 
function c_mul(complex a, complex b)
    return {a[REAL] * b[REAL] - a[IMAG] * b[IMAG],
            a[REAL] * b[IMAG] + a[IMAG] * b[REAL]}
end function

function c_conj(complex a)
    return {a[REAL],-a[IMAG]}
end function

function c_print(complex a)
    if a[IMAG]=0 then return sprintf("%g",a[REAL]) end if
    return sprintf("%g%+gi",a)
end function

procedure m_print(sequence a)
    integer l = length(a)
    for i=1 to l do
        for j=1 to l do
            a[i][j] = c_print(a[i][j])
        end for
        a[i] = "["&join(a[i],",")&"]"
    end for
    puts(1,join(a,"\n")&"\n")
end procedure


function conjugate_transpose(sequence a)
    sequence res = a
    integer l = length(a)
    for i=1 to l do
        for j=1 to l do
          res[i][j] = c_conj(a[j][i])
        end for
    end for
    return res
end function

function m_unitary(sequence act)
-- note: a was normal and act = a*ct already
    integer l = length(act)
    for i=1 to l do
        for j=1 to l do
            atom {re,im} = act[i,j]
            -- round to nearest billionth
            -- (powers of 2 help the FPU out)
            re = round(re,1024*1024*1024)
            im = round(im,1024*1024*1024)
            if im!=0
            or (i=j and re!=1)
            or (i!=j and re!=0) then
                return 0
            end if
        end for
    end for
    return 1
end function

function m_mul(sequence a, sequence b)
    sequence res = sq_mul(a,0)
    integer l = length(a)
    for i=1 to l do
        for j=1 to l do
            for k=1 to l do
                res[i][j] = c_add(res[i][j],c_mul(a[i][k],b[k][j]))
            end for
        end for
    end for
    return res
end function

procedure test(sequence a)
sequence ct = conjugate_transpose(a)
    printf(1,"Original matrix:\n")
    m_print(a)
    printf(1,"Conjugate transpose:\n")
    m_print(ct)
    -- note: rounding similar to that in m_unitary may be rqd (in a similar 
    --       loop in a new m_equal function) on these two equality tests, 
    --       but as it is, all tests pass with the builtin = operator.
    printf(1,"Hermitian?: %s\n",{iff(a=ct?"TRUE":"FALSE")}) -- (this one)
    sequence act = m_mul(a,ct), cta = m_mul(ct,a)
    bool normal = act=cta                                   -- (&this one)
    printf(1,"Normal?: %s\n",{iff(normal?"TRUE":"FALSE")})
    printf(1,"Unitary?: %s\n\n",{iff(normal and m_unitary(act)?"TRUE":"FALSE")})
end procedure

constant x = sqrt(2)/2

constant tests = {{{{3, 0},{2,1}},
                   {{2,-1},{1,0}}},

                  {{{ 1, 0},{ 1, 1},{ 0, 2}},
                   {{ 1,-1},{ 5, 0},{-3, 0}},
                   {{ 0,-2},{-3, 0},{ 0, 0}}},

                  {{{0.5,+0.5},{0.5,-0.5}},
                   {{0.5,-0.5},{0.5,+0.5}}},

                  {{{ 1, 0},{ 1, 0},{ 0, 0}},
                   {{ 0, 0},{ 1, 0},{ 1, 0}},
                   {{ 1, 0},{ 0, 0},{ 1, 0}}},

                  {{{x, 0},{x, 0},{0, 0}},
                   {{0,-x},{0, x},{0, 0}},
                   {{0, 0},{0, 0},{0, 1}}},

                  {{{2,7},{9,-5}},
                   {{3,4},{8,-6}}}}

for i=1 to length(tests) do test(tests[i]) end for
```

```txt

Original matrix:
[3,2+1i]
[2-1i,1]
Conjugate transpose:
[3,2+1i]
[2-1i,1]
Hermitian?: TRUE
Normal?: TRUE
Unitary?: FALSE

Original matrix:
[1,1+1i,0+2i]
[1-1i,5,-3]
[0-2i,-3,0]
Conjugate transpose:
[1,1+1i,0+2i]
[1-1i,5,-3]
[0-2i,-3,0]
Hermitian?: TRUE
Normal?: TRUE
Unitary?: FALSE

Original matrix:
[0.5+0.5i,0.5-0.5i]
[0.5-0.5i,0.5+0.5i]
Conjugate transpose:
[0.5-0.5i,0.5+0.5i]
[0.5+0.5i,0.5-0.5i]
Hermitian?: FALSE
Normal?: TRUE
Unitary?: TRUE

Original matrix:
[1,1,0]
[0,1,1]
[1,0,1]
Conjugate transpose:
[1,0,1]
[1,1,0]
[0,1,1]
Hermitian?: FALSE
Normal?: TRUE
Unitary?: FALSE

Original matrix:
[0.707107,0.707107,0]
[0-0.707107i,0+0.707107i,0]
[0,0,0+1i]
Conjugate transpose:
[0.707107,0+0.707107i,0]
[0.707107,0-0.707107i,0]
[0,0,0-1i]
Hermitian?: FALSE
Normal?: TRUE
Unitary?: TRUE

Original matrix:
[2+7i,9-5i]
[3+4i,8-6i]
Conjugate transpose:
[2-7i,3-4i]
[9+5i,8+6i]
Hermitian?: FALSE
Normal?: FALSE
Unitary?: FALSE

```



## PL/I


```PL/I

test: procedure options (main); /* 1 October 2012 */
   declare n fixed binary;

   put ('Conjugate a complex square matrix.');
   put skip list ('What is the order of the matrix?:');
   get (n);
   begin;
      declare (M, MH, MM, MM_MMH, MM_MHM, IDENTITY)(n,n) fixed complex;
      declare i fixed binary;

      IDENTITY = 0; do i = 1 to n; IDENTITY(I,I) = 1; end;
      put skip list ('Please type the matrix:');
      get list (M);
      do i = 1 to n;
         put skip list (M(i,*));
      end;
      do i = 1 to n;
         MH(i,*) = conjg(M(*,i));
      end;
      put skip list ('The conjugate transpose is:');
      do i = 1 to n;
         put skip list (MH(i,*));
      end;
      if all(M=MH) then
         put skip list ('Matrix is Hermitian');
      call MMULT(M, MH, MM_MMH);
      call MMULT(MH, M, MM_MHM);

      if all(MM_MMH = MM_MHM) then
         put skip list ('Matrix is Normal');

      if all(ABS(MM_MMH - IDENTITY) < 0.0001) then
         put skip list ('Matrix is unitary');
      if all(ABS(MM_MHM - IDENTITY) < 0.0001) then
         put skip list ('Matrix is unitary');
   end;

MMULT: procedure (M, MH, MM);
   declare (M, MH, MM)(*,*) fixed complex;
   declare (i, j, n) fixed binary;

   n = hbound(M,1);
   do i = 1 to n;
      do j = 1 to n;
         MM(i,j) = sum(M(i,*) * MH(*,j) );
      end;
   end;
end MMULT;
end test;

```

Outputs from separate runs:

```txt

Please type the matrix: 

       1+0I                    1+0I                    1+0I       
       1+0I                    1+0I                    1+0I       
       1+0I                    1+0I                    1+0I       
The conjugate transpose is: 
       1-0I                    1-0I                    1-0I       
       1-0I                    1-0I                    1-0I       
       1-0I                    1-0I                    1-0I       
Matrix is Hermitian 
Matrix is Normal 

       1+0I                    1+0I                    0+0I
       0+0I                    1+0I                    1+0I       
       1+0I                    0+0I                    1+0I       
The conjugate transpose is: 
       1-0I                    0-0I                    1-0I       
       1-0I                    1-0I                    0-0I       
       0-0I                    1-0I                    1-0I       
Matrix is Normal 

```

Next test performed with declaration of matrixes changed to
decimal precision (10,5).

```txt

Please type the matrix:

      0.70710+0.00000I        0.70710+0.00000I        0.00000+0.00000I
      0.00000+0.70710I        0.00000-0.70710I        0.00000+0.00000I
      0.00000+0.00000I        0.00000+0.00000I        0.00000+1.00000I
    
The conjugate transpose is: 
      0.70710-0.00000I        0.00000-0.70710I        0.00000-0.00000I
      0.70710-0.00000I        0.00000+0.70710I        0.00000-0.00000I
      0.00000-0.00000I        0.00000-0.00000I        0.00000-1.00000I

Matrix is Normal 
Matrix is unitary 
Matrix is unitary

```



## PowerShell


```PowerShell

function conjugate-transpose($a) {
    $arr = @()
    if($a) { 
        $n = $a.count - 1 
        if(0 -lt $n) { 
            $m = ($a | foreach {$_.count} | measure-object -Minimum).Minimum - 1
            if( 0 -le $m) {
                if (0 -lt $m) {
                    $arr =@(0)*($m+1)
                    foreach($i in 0..$m) {
                        $arr[$i] = foreach($j in 0..$n) {@([System.Numerics.complex]::Conjugate($a[$j][$i]))}    
                    }
                } else {$arr = foreach($row in $a) {[System.Numerics.complex]::Conjugate($row[0])}}
            }
        } else {$arr = foreach($row in $a) {[System.Numerics.complex]::Conjugate($row[0])}}
    }
    $arr
}

function multarrays-complex($a, $b) {
    $c = @()
    if($a -and $b) {
        $n = $a.count - 1
        $m = $b[0].count - 1
        $c = @([System.Numerics.complex]::new(0,0))*($n+1)
        foreach ($i in 0..$n) {    
            $c[$i] = foreach ($j in 0..$m) { 
                [System.Numerics.complex]$sum = [System.Numerics.complex]::new(0,0)
                foreach ($k in 0..$n){$sum = [System.Numerics.complex]::Add($sum, ([System.Numerics.complex]::Multiply($a[$i][$k],$b[$k][$j])))}
                $sum
            }
        }
    }
    $c
}

function identity-complex($n) {
    if(0 -lt $n) { 
        $array = @(0) * $n
        foreach ($i in 0..($n-1)) {
            $array[$i] = @([System.Numerics.complex]::new(0,0)) * $n
            $array[$i][$i] = [System.Numerics.complex]::new(1,0)
        }  
        $array 
    } else { @() }
}

function are-eq ($a,$b) { -not (Compare-Object $a $b -SyncWindow 0)}

function show($a) {
    if($a) { 
        0..($a.Count - 1) | foreach{ if($a[$_]){"$($a[$_])"}else{""} }
    }
}
function complex($a,$b) {[System.Numerics.complex]::new($a,$b)}
 
$id2 = identity-complex 2
$m = @(@((complex 2 7), (complex 9 -5)),@((complex 3 4), (complex 8 -6)))
$hm = conjugate-transpose $m
$mhm = multarrays-complex $m $hm
$hmm = multarrays-complex $hm $m
"`$m ="
show $m
""
"`$hm = conjugate-transpose `$m ="
show $hm
""
"`$m * `$hm ="
show $mhm
""
"`$hm * `$m ="
show $hmm
""
"Hermitian? `$m = $(are-eq $m $hm)"
"Normal? `$m = $(are-eq $mhm $hmm)"
"Unitary? `$m = $((are-eq $id2 $hmm) -and (are-eq $id2 $mhm))"

```

<b>Output:</b>

```txt

$m =
(2, 7) (9, -5)
(3, 4) (8, -6)

$hm = conjugate-transpose $m =
(2, -7) (3, -4)
(9, 5) (8, 6)

$m * $hm =
(159, 0) (136, 27)
(136, -27) (125, 0)

$hm * $m =
(78, 0) (-17, -123)
(-17, 123) (206, 0)

Hermitian? $m = False
Normal? $m = False
Unitary? $m = False

```



## Python

Internally, matrices must be represented as rectangular tuples of tuples of complex numbers.

```python
def conjugate_transpose(m):
    return tuple(tuple(n.conjugate() for n in row) for row in zip(*m))

def mmul( ma, mb):
    return tuple(tuple(sum( ea*eb for ea,eb in zip(a,b)) for b in zip(*mb)) for a in ma)

def mi(size):
    'Complex Identity matrix'
    sz = range(size)
    m = [[0 + 0j for i in sz] for j in sz]
    for i in range(size):
        m[i][i] = 1 + 0j
    return tuple(tuple(row) for row in m)

def __allsame(vector):
    first, rest = vector[0], vector[1:]
    return all(i == first for i in rest)

def __allnearsame(vector, eps=1e-14):
    first, rest = vector[0], vector[1:]
    return all(abs(first.real - i.real) < eps and abs(first.imag - i.imag) < eps
               for i in rest)

def isequal(matrices, eps=1e-14):
    'Check any number of matrices for equality within eps'
    x = [len(m) for m in matrices]
    if not __allsame(x): return False
    y = [len(m[0]) for m in matrices]
    if not __allsame(y): return False
    for s in range(x[0]):
        for t in range(y[0]):
            if not __allnearsame([m[s][t] for m in matrices], eps): return False
    return True
    

def ishermitian(m, ct):
    return isequal([m, ct])

def isnormal(m, ct):
    return isequal([mmul(m, ct), mmul(ct, m)])

def isunitary(m, ct):
    mct, ctm = mmul(m, ct), mmul(ct, m)
    mctx, mcty, cmx, ctmy = len(mct), len(mct[0]), len(ctm), len(ctm[0])
    ident = mi(mctx)
    return isequal([mct, ctm, ident])

def printm(comment, m):
    print(comment)
    fields = [['%g%+gj' % (f.real, f.imag) for f in row] for row in m]
    width = max(max(len(f) for f in row) for row in fields)
    lines = (', '.join('%*s' % (width, f) for f in row) for row in fields)
    print('\n'.join(lines))

if __name__ == '__main__':
    for matrix in [
            ((( 3.000+0.000j), (+2.000+1.000j)), 
            (( 2.000-1.000j), (+1.000+0.000j))),

            ((( 1.000+0.000j), (+1.000+0.000j), (+0.000+0.000j)), 
            (( 0.000+0.000j), (+1.000+0.000j), (+1.000+0.000j)), 
            (( 1.000+0.000j), (+0.000+0.000j), (+1.000+0.000j))),

            ((( 2**0.5/2+0.000j), (+2**0.5/2+0.000j), (+0.000+0.000j)), 
            (( 0.000+2**0.5/2j), (+0.000-2**0.5/2j), (+0.000+0.000j)), 
            (( 0.000+0.000j), (+0.000+0.000j), (+0.000+1.000j)))]:
        printm('\nMatrix:', matrix)
        ct = conjugate_transpose(matrix)
        printm('Its conjugate transpose:', ct)
        print('Hermitian? %s.' % ishermitian(matrix, ct))
        print('Normal?    %s.' % isnormal(matrix, ct))
        print('Unitary?   %s.' % isunitary(matrix, ct))
```


```txt
Matrix:
3+0j, 2+1j
2-1j, 1+0j
Its conjugate transpose:
3-0j, 2+1j
2-1j, 1-0j
Hermitian? True.
Normal?    True.
Unitary?   False.

Matrix:
1+0j, 1+0j, 0+0j
0+0j, 1+0j, 1+0j
1+0j, 0+0j, 1+0j
Its conjugate transpose:
1-0j, 0-0j, 1-0j
1-0j, 1-0j, 0-0j
0-0j, 1-0j, 1-0j
Hermitian? False.
Normal?    True.
Unitary?   False.

Matrix:
0.707107+0j, 0.707107+0j,        0+0j
0-0.707107j, 0+0.707107j,        0+0j
       0+0j,        0+0j,        0+1j
Its conjugate transpose:
0.707107-0j, 0+0.707107j,        0-0j
0.707107-0j, 0-0.707107j,        0-0j
       0-0j,        0-0j,        0-1j
Hermitian? False.
Normal?    True.
Unitary?   True.
```



## Racket


```racket

#lang racket
(require math)
(define H matrix-hermitian)

(define (normal? M)
  (define MH (H M))
  (equal? (matrix* MH M) 
          (matrix* M MH)))

(define (unitary? M)
  (define MH (H M))
  (and (matrix-identity? (matrix* MH M))
       (matrix-identity? (matrix* M MH))))

(define (hermitian? M)
  (equal? (H M) M))

```

Test:

```racket

(define M (matrix [[3.000+0.000i +2.000+1.000i]
                   [2.000-1.000i +1.000+0.000i]]))
(H M)
(normal? M)
(unitary? M)
(hermitian? M)

```

Output:

```racket

(array #[#[3.0-0.0i 2.0+1.0i] #[2.0-1.0i 1.0-0.0i]])
#t
#f
#f

```



## REXX


```rexx
/*REXX program performs a  conjugate transpose  on a  complex  square matrix.           */
parse arg N elements;  if N==''|N==","  then N=3 /*Not specified?  Then use the default.*/
k=0;                do   r=1  for N
                      do c=1  for N;  k=k+1;  M.r.c=word(word(elements,k) 1,1);  end /*c*/
                    end   /*r*/
call showCmat 'M'        ,N                      /*display a nicely formatted matrix.   */
identity.=0;                       do d=1  for N;   identity.d.d=1;   end  /*d*/
call conjCmat 'MH', "M"  ,N                      /*conjugate the  M  matrix  ───►  MH   */
call showCmat 'MH'       ,N                      /*display a nicely formatted matrix.   */
say 'M is Hermitian:  '    word('no yes',isHermitian('M',"MH",N)+1)
call multCmat 'M',  'MH', 'MMH',  N              /*multiple the two matrices together.  */
call multCmat 'MH', 'M',  'MHM',  N              /*    "     "   "      "        "      */
say '  M is Normal:   '           word('no yes', isHermitian('MMH', "MHM", N) + 1)
say '  M is Unary:    '           word('no yes',     isUnary('M',   N) + 1)
say 'MMH is Unary:    '           word('no yes',     isUnary('MMH', N) + 1)
say 'MHM is Unary:    '           word('no yes',     isUnary('MHM', N) + 1)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cP:       procedure; arg ',' c;        return word( strip( translate(c, , 'IJ') )  0, 1)
rP:       procedure; parse arg r ',';  return word( r 0, 1)  /*◄──maybe return a 0 ↑    */
/*──────────────────────────────────────────────────────────────────────────────────────*/
conjCmat: parse arg matX,matY,rows 1 cols;          call normCmat matY, rows
                      do   r=1  for rows;   _=
                        do c=1  for cols;   v=value(matY'.'r"."c)
                        rP=rP(v);    cP=-cP(v);     call value  matX'.'c"."r, rP','cP
                        end   /*c*/
                      end     /*r*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
isHermitian: parse arg matX,matY,rows 1 cols;       call normCmat matX, rows
                                                    call normCmat matY, rows
                      do   r=1  for rows;  _=
                        do c=1  for cols
                        if value(matX'.'r"."c) \= value(matY'.'r"."c)  then return 0
                        end   /*c*/
                      end     /*r*/
             return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
isUnary: parse arg matX,rows 1 cols
                      do   r=1  for rows;    _=
                        do c=1  for cols;    z=value(matX'.'r"."c);    rP=rP(z);  cP=cP(z)
                        if abs(sqrt(rP(z)**2 + cP(z)**2) - (r==c)) >= .0001  then return 0
                        end   /*c*/
                      end     /*r*/
        return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
multCmat: parse arg matA,matB,matT,rows 1 cols;            call value matT'.', 0
                      do     r=1  for rows;  _=
                        do   c=1  for cols
                          do k=1  for cols;  T=value(matT'.'r"."c);   Tr=rP(T);   Tc=cP(T)
                                             A=value(matA'.'r"."k);   Ar=rP(A);   Ac=cP(A)
                                             B=value(matB'.'k"."c);   Br=rP(B);   Bc=cP(B)
                          Pr=Ar*Br - Ac*Bc;  Pc=Ac*Br + Ar*Bc;        Tr=Tr+Pr;   Tc=Tc+Pc
                          call value matT'.'r"."c,Tr','Tc
                          end   /*k*/
                        end     /*c*/
                      end       /*r*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
normCmat: parse arg matN,rows 1 cols
                      do   r=1  to rows;  _=
                        do c=1  to cols;  v=translate(value(matN'.'r"."c), , "IiJj")
                        parse upper  var  v  real  ','  cplx
                        if real\==''  then real=real/1
                        if cplx\==''  then cplx=cplx/1;       if cplx=0  then cplx=
                        if cplx\==''  then cplx=cplx"j"
                        call value matN'.'r"."c, strip(real','cplx, "T", ',')
                        end   /*c*/
                      end     /*r*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
showCmat: parse arg matX,rows,cols;    if cols==''  then cols=rows;          @@=left('',6)
          say;  say center('matrix' matX, 79, '─');      call normCmat matX, rows, cols
                      do   r=1  to rows;  _=
                        do c=1  to cols;  _=_ @@ left(value(matX'.'r"."c), 9);  end  /*c*/
                      say _
                      end     /*r*/
          say; return
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;   d=digits();  numeric form;   h=d+6
      numeric digits; parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g *.5'e'_ % 2
      m.=9;  do j=0  while h>9;     m.j=h;              h=h%2+1;       end /*j*/
             do k=j+5  to 0  by -1; numeric digits m.k; g=(g+x/g)*.5;  end /*k*/; return g
```

'''output'''   when using the default input:

```txt

───────────────────────────────────matrix M────────────────────────────────────
        1                1                1
        1                1                1
        1                1                1


───────────────────────────────────matrix MH───────────────────────────────────
        1                1                1
        1                1                1
        1                1                1

M is Hermitian:   yes
  M is Normal:    yes
  M is Unary:     no
MMH is Unary:     no
MHM is Unary:     no

```

'''output'''   when using the input of:   <tt> 3   .7071   .7071   0   0,.7071   0,-.7071   0   0   0   0,1 </tt>

```txt

───────────────────────────────────matrix M────────────────────────────────────
        0.7071           0.7071           0
        0,0.7071j        0,-0.7071        0
        0                0                0,1j


───────────────────────────────────matrix MH───────────────────────────────────
        0.7071           0,-0.7071        0
        0.7071           0,0.7071j        0
        0                0                0,-1j

M is Hermitian:   no
  M is Normal:    yes
  M is Unary:     no
MMH is Unary:     yes
MHM is Unary:     yes

```



## Ruby

```ruby
require 'matrix'

# Start with some matrix.
i = Complex::I
matrix = Matrix[[i, 0, 0],
                [0, i, 0],
                [0, 0, i]]

# Find the conjugate transpose.
#   Matrix#conjugate appeared in Ruby 1.9.2.
conjt = matrix.conj.t           # aliases for matrix.conjugate.tranpose
print 'conjugate tranpose: '; puts conjt

if matrix.square?
  # These predicates appeared in Ruby 1.9.3.
  print 'Hermitian? '; puts matrix.hermitian?
  print '   normal? '; puts matrix.normal?
  print '  unitary? '; puts matrix.unitary?
else
  # Matrix is not square. These predicates would
  # raise ExceptionForMatrix::ErrDimensionMismatch.
  print 'Hermitian? false'
  print '   normal? false'
  print '  unitary? false'
end
```

Note: Ruby 1.9 had a bug in the Matrix#hermitian? method. It's fixed in 2.0.

## Rust

Uses external crate 'num', version 0.1.34

```rust

extern crate num; // crate for complex numbers

use num::complex::Complex;
use std::ops::Mul;
use std::fmt;


#[derive(Debug, PartialEq)]
struct Matrix<f32> {
    grid: [[Complex<f32>; 2]; 2], // used to represent matrix
}


impl Matrix<f32> { // implements a method call for calculating the conjugate transpose
    fn conjugate_transpose(&self) -> Matrix<f32> {
        Matrix {grid: [[self.grid[0][0].conj(), self.grid[1][0].conj()],
        [self.grid[0][1].conj(), self.grid[1][1].conj()]]}
    }
}

impl Mul for Matrix<f32> { // implements '*' (multiplication) for the matrix
    type Output = Matrix<f32>;

    fn mul(self, other: Matrix<f32>) -> Matrix<f32> {
        Matrix {grid: [[self.grid[0][0]*other.grid[0][0] + self.grid[0][1]*other.grid[1][0],
            self.grid[0][0]*other.grid[0][1] + self.grid[0][1]*other.grid[1][1]],
            [self.grid[1][0]*other.grid[0][0] + self.grid[1][1]*other.grid[1][0],
            self.grid[1][0]*other.grid[1][0] + self.grid[1][1]*other.grid[1][1]]]}
    }
}

impl Copy for Matrix<f32> {} // implemented to prevent 'moved value' errors in if statements below
impl Clone for Matrix<f32> {
    fn clone(&self) -> Matrix<f32> {
        *self
    }
}

impl fmt::Display for Matrix<f32> { // implemented to make output nicer
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})\n({}, {})", self.grid[0][0], self.grid[0][1], self.grid[1][0], self.grid[1][1])
    }
}

fn main() {
    let a = Matrix {grid: [[Complex::new(3.0, 0.0), Complex::new(2.0, 1.0)],
        [Complex::new(2.0, -1.0), Complex::new(1.0, 0.0)]]};

    let b = Matrix {grid: [[Complex::new(0.5, 0.5), Complex::new(0.5, -0.5)],
        [Complex::new(0.5, -0.5), Complex::new(0.5, 0.5)]]};

    test_type(a);
    test_type(b);
}

fn test_type(mat: Matrix<f32>) {
    let identity = Matrix {grid: [[Complex::new(1.0, 0.0), Complex::new(0.0, 0.0)],
        [Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)]]};
    let mat_conj = mat.conjugate_transpose();

    println!("Matrix: \n{}\nConjugate transpose: \n{}", mat, mat_conj);

    if mat == mat_conj {
        println!("Hermitian?: TRUE");
    } else {
        println!("Hermitian?: FALSE");
    }

    if mat*mat_conj == mat_conj*mat {
        println!("Normal?: TRUE");
    } else {
        println!("Normal?: FALSE");
    }

    if mat*mat_conj == identity {
        println!("Unitary?: TRUE");
    } else {
        println!("Unitary?: FALSE");
    }
}
```

Output:

```txt

Matrix:
(3+0i, 2+1i)
(2-1i, 1+0i)
Conjugate transpose:
(3+0i, 2+1i)
(2-1i, 1+0i)
Hermitian?: TRUE
Normal?: TRUE
Unitary?: FALSE
Matrix:
(0.5+0.5i, 0.5-0.5i)
(0.5-0.5i, 0.5+0.5i)
Conjugate transpose:
(0.5-0.5i, 0.5+0.5i)
(0.5+0.5i, 0.5-0.5i)
Hermitian?: FALSE
Normal?: TRUE
Unitary?: TRUE

```



## Scala


```Scala
object ConjugateTranspose {
  
  case class Complex(re: Double, im: Double) {
    def conjugate(): Complex = Complex(re, -im)
    def +(other: Complex) = Complex(re + other.re, im + other.im)
    def *(other: Complex) = Complex(re * other.re - im * other.im, re * other.im + im * other.re)
    override def toString(): String = {
      if (im < 0) {
        s"${re}${im}i"
      } else {
        s"${re}+${im}i"
      }
    }
  }
  
  case class Matrix(val entries: Vector[Vector[Complex]]) {
    
    def *(other: Matrix): Matrix = {
      new Matrix(
        Vector.tabulate(entries.size, other.entries(0).size)((r, c) => {
          val rightRow = entries(r)
          val leftCol = other.entries.map(_(c))
          rightRow.zip(leftCol)
            .map{ case (x, y) => x * y } // multiply pair-wise
            .foldLeft(new Complex(0,0)){ case (x, y) => x + y } // sum over all
        })
      )
    }
    
    def conjugateTranspose(): Matrix = {
      new Matrix(
        Vector.tabulate(entries(0).size, entries.size)((r, c) => entries(c)(r).conjugate)
      )
    }
    
    def isHermitian(): Boolean = {
      this == conjugateTranspose()
    }
    
    def isNormal(): Boolean = {
      val ct = conjugateTranspose()
      this * ct == ct * this
    }
    
    def isIdentity(): Boolean = {
      val entriesWithIndexes = for (r <- 0 until entries.size; c <- 0 until entries(r).size) yield (r, c, entries(r)(c))
      entriesWithIndexes.forall { case (r, c, x) =>
        if (r == c) {
          x == Complex(1.0, 0.0)
        } else {
          x == Complex(0.0, 0.0)
        }
      }
    }
    
    def isUnitary(): Boolean = {
      (this * conjugateTranspose()).isIdentity()
    }
    
    override def toString(): String = {
      entries.map("  " + _.mkString("[", ",", "]")).mkString("[\n", "\n", "\n]")
    }
    
  }
  
  def main(args: Array[String]): Unit = {
    val m = new Matrix(
      Vector.fill(3, 3)(new Complex(Math.random() * 2 - 1.0, Math.random() * 2 - 1.0))
    )
    println("Matrix: " + m)
    println("Conjugate Transpose: " + m.conjugateTranspose())
    println("Hermitian: " + m.isHermitian())
    println("Normal: " + m.isNormal())
    println("Unitary: " + m.isUnitary())
  }
  
}
```

```txt

Matrix: [
  [-0.7679977131543951-0.439979346567841i,-0.6011221529373452+0.510336881376179i,-0.22458301626795674-0.2036390034398219i]
  [-0.29309032295973036+0.3034337168992096i,-0.06392399629070344-0.8178102917845342i,0.06006452944412022-0.6141208421036348i]
  [0.34841978725201117+0.3778314407778909i,0.6768867572228499+0.7323625144544055i,-0.8246879334889017-0.009443253424316733i]
]
Conjugate Transpose: [
  [-0.7679977131543951+0.439979346567841i,-0.29309032295973036-0.3034337168992096i,0.34841978725201117-0.3778314407778909i]
  [-0.6011221529373452-0.510336881376179i,-0.06392399629070344+0.8178102917845342i,0.6768867572228499-0.7323625144544055i]
  [-0.22458301626795674+0.2036390034398219i,0.06006452944412022+0.6141208421036348i,-0.8246879334889017+0.009443253424316733i]
]
Hermitian: false
Normal: false
Unitary: false

```



## Sidef

```ruby
func is_Hermitian (Array m, Array t) -> Bool { m == t }

func mat_mult (Array a, Array b, Number ε = -3) {
    var p = []
    for r, c in (^a ~X ^b[0]) {
        for k in (^b) {
            p[r][c] := 0 += (a[r][k] * b[k][c]) -> round!(ε)
        }
    }
    return p
}

func mat_trans (Array m) {
    var r = []
    for i,j in (^m ~X ^m[0]) {
        r[j][i] = m[i][j]
    }
    return r
}

func mat_ident (Number n) {
    ^n -> map {|i|
        [i.of(0)..., 1, (n - i - 1).of(0)...]
    }
}

func is_Normal (Array m, Array t) -> Bool {
    mat_mult(m, t) == mat_mult(t, m)
}

func is_Unitary (Array m, Array t) -> Bool {
    mat_mult(m, t) == mat_ident(m.len)
}

func say_it (Array a) {
    a.each {|b|
        b.map { "%9s" % _ }.join(' ').say
    }
}

[
    [
       [   1, 1+1i, 2i],
       [1-1i,    5, -3],
       [0-2i,   -3,  0]
    ],
    [
       [1, 1, 0],
       [0, 1, 1],
       [1, 0, 1]
    ],
    [
       [0.707 ,   0.707,  0],
       [0.707i, -0.707i,  0],
       [0     ,       0,  1i]
    ]
].each { |m|
    say "\nMatrix:"
    say_it(m)
    var t = mat_trans(m.map{.map{.conj}})
    say "\nTranspose:"
    say_it(t)
    say "Is Hermitian?\t#{is_Hermitian(m, t)}"
    say "Is Normal?\t#{is_Normal(m, t)}"
    say "Is Unitary?\t#{is_Unitary(m, t)}"
}
```

```txt

Matrix:
        1       1+i        2i
      1-i         5        -3
      -2i        -3         0

Transpose:
        1       1+i        2i
      1-i         5        -3
      -2i        -3         0
Is Hermitian?	true
Is Normal?	true
Is Unitary?	false

Matrix:
        1         1         0
        0         1         1
        1         0         1

Transpose:
        1         0         1
        1         1         0
        0         1         1
Is Hermitian?	false
Is Normal?	true
Is Unitary?	false

Matrix:
    0.707     0.707         0
   0.707i   -0.707i         0
        0         0         i

Transpose:
    0.707   -0.707i         0
    0.707    0.707i         0
        0         0        -i
Is Hermitian?	false
Is Normal?	true
Is Unitary?	true

```



## Sparkling

Sparkling has support for basic complex algebraic operations, but complex matrix operations are not in the standard library.


```sparkling
# Computes conjugate transpose of M
let conjTransp = function conjTransp(M) {
	return map(range(sizeof M[0]), function(row) {
		return map(range(sizeof M), function(col) {
			return cplx_conj(M[col][row]);
		});
	});
};

# Helper for cplxMatMul
let cplxVecScalarMul = function cplxVecScalarMul(A, B, row, col) {
	var M = { "re": 0.0, "im": 0.0 };
	let N = sizeof A;
	for (var i = 0; i < N; i++) {
		let P = cplx_mul(A[row][i], B[i][col]);
		M = cplx_add(M, P);
	}
	return M;
};

# Multiplies matrices A and B
# A and B are assumed to be square and of the same size,
# this condition is not checked.
let cplxMatMul = function cplxMatMul(A, B) {
	var R = {};
	let N = sizeof A;
	for (var row = 0; row < N; row++) {
		R[row] = {};
		for (var col = 0; col < N; col++) {
			R[row][col] = cplxVecScalarMul(A, B, row, col);
		}
	}
	return R;
};

# Helper for creating an array representing a complex number
# given its textual representation
let _ = function makeComplex(str) {
	let sep = indexof(str, "+", 1);
	if sep < 0 {
		sep = indexof(str, "-", 1);
	}
	let reStr = substrto(str, sep);
	let imStr = substrfrom(str, sep);
	return { "re": tofloat(reStr), "im": tofloat(imStr) };
};

# Formats a complex matrix
let printCplxMat = function printCplxMat(M) {
	foreach(M, function(i, row) {
		foreach(row, function(j, elem) {
			printf("    %.2f%+.2fi", elem.re, elem.im);
		});
		print();
	});
};

# A Hermitian matrix
let H = {
	{ _("3+0i"), _("2+1i") },
	{ _("2-1i"), _("0+0i") }
};

# A normal matrix
let N = {
	{ _("1+0i"), _("1+0i"), _("0+0i") },
	{ _("0+0i"), _("1+0i"), _("1+0i") },
	{ _("1+0i"), _("0+0i"), _("1+0i") }
};

# A unitary matrix
let U = {
	{ _("0.70710678118+0i"), _("0.70710678118+0i"), _("0+0i") },
	{ _("0-0.70710678118i"), _("0+0.70710678118i"), _("0+0i") },
	{ _("0+0i"),             _("0+0i"),             _("0+1i") }
};


print("Hermitian matrix:\nH = ");
printCplxMat(H);
print("H* = ");
printCplxMat(conjTransp(H));
print();

print("Normal matrix:\nN = ");
printCplxMat(N);
print("N* = ");
printCplxMat(conjTransp(N));
print("N* x N = ");
printCplxMat(cplxMatMul(conjTransp(N), N));
print("N x N* = ");
printCplxMat(cplxMatMul(N, conjTransp(N)));
print();

print("Unitary matrix:\nU = ");
printCplxMat(U);
print("U* = ");
printCplxMat(conjTransp(U));
print("U x U* = ");
printCplxMat(cplxMatMul(U, conjTransp(U)));
print();
```



## Tcl

Tcl's matrixes (in [[Tcllib]]) do not assume that the contents are numeric at all. As such, they do not provide mathematical operations over them and this considerably increases the complexity of the code below. Note the use of lambda terms to simplify access to the complex number package.
```tcl
package require struct::matrix
package require math::complexnumbers

proc complexMatrix.equal {m1 m2 {epsilon 1e-14}} {
    if {[$m1 rows] != [$m2 rows] || [$m1 columns] != [$m2 columns]} {
	return 0
    }
    # Compute the magnitude of the difference between two complex numbers
    set ceq [list apply {{epsilon a b} {
	expr {[mod [- $a $b]] < $epsilon}
    } ::math::complexnumbers} $epsilon]
    for {set i 0} {$i<[$m1 columns]} {incr i} {
	for {set j 0} {$j<[$m1 rows]} {incr j} {
	    if {![{*}$ceq [$m1 get cell $i $j] [$m2 get cell $i $j]]} {
		return 0
	    }
	}
    }
    return 1
}

proc complexMatrix.multiply {a b} {
    if {[$a columns] != [$b rows]} {
        error "incompatible sizes"
    }
    # Simplest to use a lambda in the complex NS
    set cpm {{sum a b} {
	+ $sum [* $a $b]
    } ::math::complexnumbers}
    set c0 [math::complexnumbers::complex 0.0 0.0];   # Complex zero
    set c [struct::matrix]
    $c add columns [$b columns]
    $c add rows [$a rows]
    for {set i 0} {$i < [$a rows]} {incr i} {
        for {set j 0} {$j < [$b columns]} {incr j} {
            set sum $c0
	    foreach rv [$a get row $i] cv [$b get column $j] {
		set sum [apply $cpm $sum $rv $cv]
            }
	    $c set cell $j $i $sum
        }
    }
    return $c
}

proc complexMatrix.conjugateTranspose {matrix} {
    set mat [struct::matrix]
    $mat = $matrix
    $mat transpose
    for {set c 0} {$c < [$mat columns]} {incr c} {
	for {set r 0} {$r < [$mat rows]} {incr r} {
	    set val [$mat get cell $c $r]
	    $mat set cell $c $r [math::complexnumbers::conj $val]
	}
    }
    return $mat
}
```

Using these tools to test for the properties described in the task:

```tcl
proc isHermitian {matrix {epsilon 1e-14}} {
    if {[$matrix rows] != [$matrix columns]} {
	# Must be square!
	return 0
    }
    set cc [complexMatrix.conjugateTranspose $matrix]
    set result [complexMatrix.equal $matrix $cc $epsilon]
    $cc destroy
    return $result
}

proc isNormal {matrix {epsilon 1e-14}} {
    if {[$matrix rows] != [$matrix columns]} {
	# Must be square!
	return 0
    }
    set mh [complexMatrix.conjugateTranspose $matrix]
    set mhm [complexMatrix.multiply $mh $matrix]
    set mmh [complexMatrix.multiply $matrix $mh]
    $mh destroy
    set result [complexMatrix.equal $mhm $mmh $epsilon]
    $mhm destroy
    $mmh destroy
    return $result
}

proc isUnitary {matrix {epsilon 1e-14}} {
    if {[$matrix rows] != [$matrix columns]} {
	# Must be square!
	return 0
    }
    set mh [complexMatrix.conjugateTranspose $matrix]
    set mhm [complexMatrix.multiply $mh $matrix]
    set mmh [complexMatrix.multiply $matrix $mh]
    $mh destroy
    set result [complexMatrix.equal $mhm $mmh $epsilon]
    $mhm destroy
    if {$result} {
	set id [struct::matrix]
	$id = $matrix;   # Just for its dimensions
	for {set c 0} {$c < [$id columns]} {incr c} {
	    for {set r 0} {$r < [$id rows]} {incr r} {
		$id set cell $c $r \
		    [math::complexnumbers::complex [expr {$c==$r}] 0]
	    }
	}
	set result [complexMatrix.equal $mmh $id $epsilon]
	$id destroy
    }
    $mmh destroy
    return $result
}
```

<!-- Wot, no demonstration? -->
