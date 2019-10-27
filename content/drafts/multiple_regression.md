+++
title = "Multiple regression"
description = ""
date = 2018-08-24T01:17:04Z
aliases = []
[extra]
id = 4456
[taxonomies]
categories = []
tags = []
+++

{{task|Probability and statistics}}

;Task:
Given a set of data vectors in the following format:

    <math>y = \{ y_1, y_2, ..., y_n \}\,</math>

    <math>X_i = \{ x_{i1}, x_{i2}, ..., x_{in} \}, i \in 1..k\,</math>

Compute the vector <math>\beta = \{ \beta_1, \beta_2, ..., \beta_k \}</math> using [[wp:Ordinary least squares|ordinary least squares]] regression using the following equation:

    <math>y_j = \Sigma_i \beta_i \cdot x_{ij} ,    j \in 1..n</math>

You can assume <i> y </i> is given to you as a vector (a one-dimensional array), and <i> X </i> is given to you as a two-dimensional array (i.e. matrix).





## Ada


Extension of [[Reduced row echelon form#Ada]]:

matrices.ads:

```Ada
generic
   type Element_Type is private;
   Zero : Element_Type;
   One : Element_Type;
   with function "+" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left, Right : Element_Type) return Element_Type is <>;
   with function "*" (Left, Right : Element_Type) return Element_Type is <>;
   with function "/" (Left, Right : Element_Type) return Element_Type is <>;
package Matrices is
   type Vector is array (Positive range <>) of Element_Type;
   type Matrix is
     array (Positive range <>, Positive range <>) of Element_Type;

   function "*" (Left, Right : Matrix) return Matrix;
   function Invert (Source : Matrix) return Matrix;
   function Reduced_Row_Echelon_Form (Source : Matrix) return Matrix;
   function Regression_Coefficients
     (Source     : Vector;
      Regressors : Matrix)
      return       Vector;
   function To_Column_Vector
     (Source : Matrix;
      Row    : Positive := 1)
      return   Vector;
   function To_Matrix
     (Source        : Vector;
      Column_Vector : Boolean := True)
      return          Matrix;
   function To_Row_Vector
     (Source : Matrix;
      Column : Positive := 1)
      return   Vector;
   function Transpose (Source : Matrix) return Matrix;

   Size_Mismatch     : exception;
   Not_Square_Matrix : exception;
   Not_Invertible    : exception;
end Matrices;
```


matrices.adb:

```Ada
package body Matrices is
   function "*" (Left, Right : Matrix) return Matrix is
      Result : Matrix (Left'Range (1), Right'Range (2)) :=
        (others => (others => Zero));
   begin
      if Left'Length (2) /= Right'Length (1) then
         raise Size_Mismatch;
      end if;
      for I in Result'Range (1) loop
         for K in Result'Range (2) loop
            for J in Left'Range (2) loop
               Result (I, K) := Result (I, K) + Left (I, J) * Right (J, K);
            end loop;
         end loop;
      end loop;
      return Result;
   end "*";

   function Invert (Source : Matrix) return Matrix is
      Expanded : Matrix (Source'Range (1),
         Source'First (2) .. Source'Last (2) * 2);
      Result   : Matrix (Source'Range (1), Source'Range (2));
   begin
      -- Matrix has to be square.
      if Source'Length (1) /= Source'Length (2) then
         raise Not_Square_Matrix;
      end if;
      -- Copy Source into Expanded matrix and attach identity matrix to right
      for Row in Source'Range (1) loop
         for Col in Source'Range (2) loop
            Expanded (Row, Col)                    := Source (Row, Col);
            Expanded (Row, Source'Last (2) + Col)  := Zero;
         end loop;
         Expanded (Row, Source'Last (2) + Row)  := One;
      end loop;
      Expanded := Reduced_Row_Echelon_Form (Source => Expanded);
      -- Copy right side to Result (= inverted Source)
      for Row in Result'Range (1) loop
         for Col in Result'Range (2) loop
            Result (Row, Col) := Expanded (Row, Source'Last (2) + Col);
         end loop;
      end loop;
      return Result;
   end Invert;

   function Reduced_Row_Echelon_Form (Source : Matrix) return Matrix is
      procedure Divide_Row
        (From    : in out Matrix;
         Row     : Positive;
         Divisor : Element_Type)
      is
      begin
         for Col in From'Range (2) loop
            From (Row, Col) := From (Row, Col) / Divisor;
         end loop;
      end Divide_Row;

      procedure Subtract_Rows
        (From                : in out Matrix;
         Subtrahend, Minuend : Positive;
         Factor              : Element_Type)
      is
      begin
         for Col in From'Range (2) loop
            From (Minuend, Col) := From (Minuend, Col) -
                                   From (Subtrahend, Col) * Factor;
         end loop;
      end Subtract_Rows;

      procedure Swap_Rows (From : in out Matrix; First, Second : Positive) is
         Temporary : Element_Type;
      begin
         for Col in From'Range (2) loop
            Temporary          := From (First, Col);
            From (First, Col)  := From (Second, Col);
            From (Second, Col) := Temporary;
         end loop;
      end Swap_Rows;

      Result : Matrix   := Source;
      Lead   : Positive := Result'First (2);
      I      : Positive;
   begin
      Rows : for Row in Result'Range (1) loop
         exit Rows when Lead > Result'Last (2);
         I := Row;
         while Result (I, Lead) = Zero loop
            I := I + 1;
            if I = Result'Last (1) then
               I    := Row;
               Lead := Lead + 1;
               exit Rows when Lead = Result'Last (2);
            end if;
         end loop;
         if I /= Row then
            Swap_Rows (From => Result, First => I, Second => Row);
         end if;
         Divide_Row
           (From    => Result,
            Row     => Row,
            Divisor => Result (Row, Lead));
         for Other_Row in Result'Range (1) loop
            if Other_Row /= Row then
               Subtract_Rows
                 (From       => Result,
                  Subtrahend => Row,
                  Minuend    => Other_Row,
                  Factor     => Result (Other_Row, Lead));
            end if;
         end loop;
         Lead := Lead + 1;
      end loop Rows;
      return Result;
   end Reduced_Row_Echelon_Form;

   function Regression_Coefficients
     (Source     : Vector;
      Regressors : Matrix)
      return       Vector
   is
      Result : Matrix (Regressors'Range (2), 1 .. 1);
   begin
      if Source'Length /= Regressors'Length (1) then
         raise Size_Mismatch;
      end if;
      declare
         Regressors_T : constant Matrix := Transpose (Regressors);
      begin
         Result := Invert (Regressors_T * Regressors) *
                   Regressors_T *
                   To_Matrix (Source);
      end;
      return To_Row_Vector (Source => Result);
   end Regression_Coefficients;

   function To_Column_Vector
     (Source : Matrix;
      Row    : Positive := 1)
      return   Vector
   is
      Result : Vector (Source'Range (2));
   begin
      for Column in Result'Range loop
         Result (Column) := Source (Row, Column);
      end loop;
      return Result;
   end To_Column_Vector;

   function To_Matrix
     (Source        : Vector;
      Column_Vector : Boolean := True)
      return          Matrix
   is
      Result : Matrix (1 .. 1, Source'Range);
   begin
      for Column in Source'Range loop
         Result (1, Column) := Source (Column);
      end loop;
      if Column_Vector then
         return Transpose (Result);
      else
         return Result;
      end if;
   end To_Matrix;

   function To_Row_Vector
     (Source : Matrix;
      Column : Positive := 1)
      return   Vector
   is
      Result : Vector (Source'Range (1));
   begin
      for Row in Result'Range loop
         Result (Row) := Source (Row, Column);
      end loop;
      return Result;
   end To_Row_Vector;

   function Transpose (Source : Matrix) return Matrix is
      Result : Matrix (Source'Range (2), Source'Range (1));
   begin
      for Row in Result'Range (1) loop
         for Column in Result'Range (2) loop
            Result (Row, Column) := Source (Column, Row);
         end loop;
      end loop;
      return Result;
   end Transpose;
end Matrices;
```


Example multiple_regression.adb:

```Ada
with Ada.Text_IO;
with Matrices;
procedure Multiple_Regression is
   package Float_Matrices is new Matrices (
      Element_Type => Float,
      Zero => 0.0,
      One => 1.0);
   subtype Vector is Float_Matrices.Vector;
   subtype Matrix is Float_Matrices.Matrix;
   use type Matrix;

   procedure Output_Matrix (X : Matrix) is
   begin
      for Row in X'Range (1) loop
         for Col in X'Range (2) loop
            Ada.Text_IO.Put (Float'Image (X (Row, Col)) & ' ');
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Output_Matrix;

   -- example from Ruby solution
   V : constant Vector := (1.0, 2.0, 3.0, 4.0, 5.0);
   M : constant Matrix :=
     ((1 => 2.0),
      (1 => 1.0),
      (1 => 3.0),
      (1 => 4.0),
      (1 => 5.0));
   C : constant Vector :=
      Float_Matrices.Regression_Coefficients (Source => V, Regressors => M);
   -- Wikipedia example
   Weight        : constant Vector (1 .. 15) :=
     (52.21, 53.12, 54.48, 55.84, 57.20,
      58.57, 59.93, 61.29, 63.11, 64.47,
      66.28, 68.10, 69.92, 72.19, 74.46);
   Height        : Vector (1 .. 15)          :=
     (1.47, 1.50, 1.52, 1.55, 1.57,
      1.60, 1.63, 1.65, 1.68, 1.70,
      1.73, 1.75, 1.78, 1.80, 1.83);
   Height_Matrix : Matrix (1 .. 15, 1 .. 3);
begin
   Ada.Text_IO.Put_Line ("Example from Ruby solution:");
   Ada.Text_IO.Put_Line ("V:");
   Output_Matrix (Float_Matrices.To_Matrix (V));
   Ada.Text_IO.Put_Line ("M:");
   Output_Matrix (M);
   Ada.Text_IO.Put_Line ("C:");
   Output_Matrix (Float_Matrices.To_Matrix (C));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Example from Wikipedia:");
   for I in Height'Range loop
      Height_Matrix (I, 1) := 1.0;
      Height_Matrix (I, 2) := Height (I);
      Height_Matrix (I, 3) := Height (I) ** 2;
   end loop;
   Ada.Text_IO.Put_Line ("Matrix:");
   Output_Matrix (Height_Matrix);
   declare
      Coefficients : constant Vector :=
         Float_Matrices.Regression_Coefficients
           (Source     => Weight,
            Regressors => Height_Matrix);
   begin
      Ada.Text_IO.Put_Line ("Coefficients:");
      Output_Matrix (Float_Matrices.To_Matrix (Coefficients));
   end;
end Multiple_Regression;
```


{{out}}

```txt
Example from Ruby solution:
V:
 1.00000E+00
 2.00000E+00
 3.00000E+00
 4.00000E+00
 5.00000E+00
M:
 2.00000E+00
 1.00000E+00
 3.00000E+00
 4.00000E+00
 5.00000E+00
C:
 9.81818E-01

Example from Wikipedia:
Matrix:
 1.00000E+00  1.47000E+00  2.16090E+00
 1.00000E+00  1.50000E+00  2.25000E+00
 1.00000E+00  1.52000E+00  2.31040E+00
 1.00000E+00  1.55000E+00  2.40250E+00
 1.00000E+00  1.57000E+00  2.46490E+00
 1.00000E+00  1.60000E+00  2.56000E+00
 1.00000E+00  1.63000E+00  2.65690E+00
 1.00000E+00  1.65000E+00  2.72250E+00
 1.00000E+00  1.68000E+00  2.82240E+00
 1.00000E+00  1.70000E+00  2.89000E+00
 1.00000E+00  1.73000E+00  2.99290E+00
 1.00000E+00  1.75000E+00  3.06250E+00
 1.00000E+00  1.78000E+00  3.16840E+00
 1.00000E+00  1.80000E+00  3.24000E+00
 1.00000E+00  1.83000E+00  3.34890E+00
Coefficients:
 1.35403E+02
-1.51161E+02
 6.43514E+01
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      *FLOAT 64
      INSTALL @lib$+"ARRAYLIB"
      
      DIM y(14), x(2,14), c(2)
      y() = 52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, \
      \     63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46
      x() =  1.47,  1.50,  1.52,  1.55,  1.57,  1.60,  1.63,  1.65, \
      \      1.68,  1.70,  1.73,  1.75,  1.78,  1.80,  1.83
      
      FOR row% = DIM(x(),1) TO 0 STEP -1
        FOR col% = 0 TO DIM(x(),2)
          x(row%,col%) = x(0,col%) ^ row%
        NEXT
      NEXT row%
      
      PROCmultipleregression(y(), x(), c())
      FOR i% = 0 TO DIM(c(),1) : PRINT c(i%) "  "; : NEXT
      PRINT
      END
      
      DEF PROCmultipleregression(y(), x(), c())
      LOCAL m(), t()
      DIM m(DIM(x(),1), DIM(x(),1)), t(DIM(x(),2),DIM(x(),1))
      PROC_transpose(x(), t())
      m() = x().t()
      PROC_invert(m())
      t() = t().m()
      c() = y().t()
      ENDPROC
```

{{out}}

```txt

128.812804  -143.162023  61.9603254

```



## C

Using GNU gsl and c99, with the WP data
```C>#include <stdio.h

#include <gsl/gsl_matrix.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_multifit.h>

double w[] = {	52.21, 53.12, 54.48, 55.84, 57.20,
		58.57, 59.93, 61.29, 63.11, 64.47,
		66.28, 68.10, 69.92, 72.19, 74.46 };
double h[] = {	1.47, 1.50, 1.52, 1.55, 1.57,
		1.60, 1.63, 1.65, 1.68, 1.70,
		1.73, 1.75, 1.78, 1.80, 1.83	};

int main()
{
	int n = sizeof(h)/sizeof(double);
	gsl_matrix *X = gsl_matrix_calloc(n, 3);
	gsl_vector *Y = gsl_vector_alloc(n);
	gsl_vector *beta = gsl_vector_alloc(3);

	for (int i = 0; i < n; i++) {
		gsl_vector_set(Y, i, w[i]);

		gsl_matrix_set(X, i, 0, 1);
		gsl_matrix_set(X, i, 1, h[i]);
		gsl_matrix_set(X, i, 2, h[i] * h[i]);
	}

	double chisq;
	gsl_matrix *cov = gsl_matrix_alloc(3, 3);
	gsl_multifit_linear_workspace * wspc = gsl_multifit_linear_alloc(n, 3);
	gsl_multifit_linear(X, Y, beta, cov, &chisq, wspc);

	printf("Beta:");
	for (int i = 0; i < 3; i++)
		printf("  %g", gsl_vector_get(beta, i));
	printf("\n");

	gsl_matrix_free(X);
	gsl_matrix_free(cov);
	gsl_vector_free(Y);
	gsl_vector_free(beta);
	gsl_multifit_linear_free(wspc);

}
```


=={{header|C sharp|C#}}==
{{libheader|Math.Net}}

```csharp
using System;
using MathNet.Numerics.LinearRegression;
using MathNet.Numerics.LinearAlgebra;
using MathNet.Numerics.LinearAlgebra.Double;

class Program
{
    static void Main(string[] args)
    {
        var col = DenseVector.OfArray(new double[] { 1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63, 1.65,
            1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83 });
        var X = DenseMatrix.OfColumns(new Vector<double>[] { col.PointwisePower(0), col, col.PointwisePower(2) });
        var y = DenseVector.OfArray(new double[] { 52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
            61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46 });
        var β = MultipleRegression.QR(X, y);
        Console.WriteLine(β);
    }
}
```


{{out}}

```txt
DenseVector 3-Double
 128.813
-143.162
 61.9603
```




## Common Lisp


Uses the routine (chol A) from [[Cholesky decomposition]], (mmul A B) from [[Matrix multiplication]], (mtp A) from [[Matrix transposition]].


```lisp

;; Solve a linear system AX=B where A is symmetric and positive definite, so it can be Cholesky decomposed.
(defun linsys (A B)
  (let* ((n (car  (array-dimensions A)))
         (m (cadr (array-dimensions B)))
         (y (make-array n        :element-type 'long-float :initial-element 0.0L0))
         (X (make-array `(,n ,m) :element-type 'long-float :initial-element 0.0L0))
         (L (chol A))) ; A=LL'

    (loop for col from 0 to (- m 1) do
       ;; Forward substitution: y = L\B
       (loop for k from 0 to (- n 1)
             do (setf (aref y k)
                      (/ (- (aref B k col)
                            (loop for j from 0 to (- k 1)
                                  sum (* (aref L k j)
                                         (aref y j))))
                         (aref L k k))))

       ;; Back substitution. x=L'\y
       (loop for k from (- n 1) downto 0
             do (setf (aref X k col)
                      (/ (- (aref y k)
                            (loop for j from (+ k 1) to (- n 1)
                                  sum (* (aref L j k)
                                         (aref X j col))))
                         (aref L k k)))))
    X))

;; Solve a linear least squares problem. Ax=b, with A being mxn, with m>n.
;; Solves the linear system A'Ax=A'b.
(defun lsqr (A b)
  (linsys (mmul (mtp A) A)
          (mmul (mtp A) b)))

```


To show an example of multiple regression, (polyfit x y n) from [[Polynomial regression]], which itself uses (linsys A B) and (lsqr A b), will be used to fit a second degree order polynomial to data.


```lisp
(let ((x (make-array '(1 11) :initial-contents '((0 1 2 3 4 5 6 7 8 9 10))))
      (y (make-array '(1 11) :initial-contents '((1 6 17 34 57 86 121 162 209 262 321)))))
  (polyfit x y 2))
 
#2A((0.9999999999999759d0) (2.000000000000005d0) (3.0d0))
```



## Emacs Lisp


Multiple regression analysis by Emacs Lisp and built-in Emacs Calc.

<lang emacs-lisp>
(setq X1 '[0 1 2 3 4 5 6 7 8 9 10])
(setq X2 '[0 1 1 3 3 7 6 7 3 9 8])
(setq Y '[1 6 17 34 57 86 121 162 209 262 321])
(calc-eval
 (format "fit(a*X1+b*X2+c,[X1,X2],[a,b,c],[%s %s %s])" X1 X2 Y))

```


{{out}}

```txt

"35.2014388489 X1 - 3.95683453237 X2 - 42.7410071942"

```





## ERRE


```ERRE
PROGRAM MULTIPLE_REGRESSION

!$DOUBLE

CONST N=14,M=2,Q=3 ! number of points and M.R. polynom degree

DIM X[N],Y[N]      ! data points
DIM S[N],T[N]      ! linear system coefficient
DIM A[M,Q]         ! sistem to be solved

BEGIN

   DATA(1.47,1.50,1.52,1.55,1.57,1.60,1.63,1.65,1.68,1.70,1.73,1.75,1.78,1.80,1.83)
   DATA(52.21,53.12,54.48,55.84,57.20,58.57,59.93,61.29,63.11,64.47,66.28,68.10,69.92,72.19,74.46)

   FOR I%=0 TO N DO
     READ(X[I%])
   END FOR

   FOR I%=0 TO N DO
     READ(Y[I%])
   END FOR

   FOR K%=0 TO 2*M DO
      S[K%]=0  T[K%]=0
      FOR I%=0 TO N DO
         S[K%]=S[K%]+X[I%]^K%
         IF K%<=M THEN T[K%]=T[K%]+Y[I%]*X[I%]^K% END IF
      END FOR
   END FOR

! build linear system

   FOR ROW%=0 TO M DO
     FOR COL%=0 TO M DO
       A[ROW%,COL%]=S[ROW%+COL%]
     END FOR
     A[ROW%,COL%]=T[ROW%]
   END FOR

   PRINT("LINEAR SYSTEM COEFFICENTS") PRINT
   FOR I%=0 TO M DO
     FOR J%=0 TO M+1 DO
        WRITE(" ######.#";A[I%,J%];)
     END FOR
     PRINT
   END FOR
   PRINT

   FOR J%=0 TO M DO
         FOR I%=J% TO M DO
              EXIT IF A[I%,J%]<>0
         END FOR
         IF I%=M+1 THEN
             PRINT("SINGULAR MATRIX !")
             !$STOP
         END IF
         FOR K%=0 TO M+1 DO
             SWAP(A[J%,K%],A[I%,K%])
         END FOR
         Y=1/A[J%,J%]
         FOR K%=0 TO M+1 DO
             A[J%,K%]=Y*A[J%,K%]
         END FOR
         FOR I%=0 TO M DO
             IF I%<>J% THEN
                 Y=-A[I%,J%]
                 FOR K%=0 TO M+1 DO
                    A[I%,K%]=A[I%,K%]+Y*A[J%,K%]
                 END FOR
             END IF
         END FOR
   END FOR
   PRINT

   PRINT("SOLUTIONS") PRINT
   FOR I%=0 TO M DO
      PRINT("c";I%;"=";)
      WRITE("#####.#######";A[I%,M+1])
   END FOR

END PROGRAM
```

{{out}}

```txt
LINEAR SYSTEM COEFFICENTS

     15.0     24.8     41.1    931.2
     24.8     41.1     68.4   1548.2
     41.1     68.4    114.3   2585.5


SOLUTIONS

c 0 =  128.8128036
c 1 = -143.1620229
c 2 =   61.9603254

```



## Fortran


{{libheader|SLATEC}} [http://netlib.org/slatec/ Available at the Netlib]


```Fortran
*-----------------------------------------------------------------------
* MR - multiple regression using the SLATEC library routine DHFTI
*
* Finds the nearest approximation to BETA in the system of linear equations:
*                     
*              X(j,i) . BETA(i) = Y(j)
* where   
*                  1 ... j ... N  
*                  1 ... i ... K
* and               
*                  K .LE. N
*
* INPUT ARRAYS ARE DESTROYED!
*
*___Name___________Type_______________In/Out____Description_____________
*   X(N,K)         Double precision   In        Predictors
*   Y(N)           Double precision   Both      On input:   N Observations
*                                               On output:  K beta weights
*   N              Integer            In        Number of observations
*   K              Integer            In        Number of predictor variables
*   DWORK(N+2*K)   Double precision   Neither   Workspace
*   IWORK(K)       Integer            Neither   Workspace
*-----------------------------------------------------------------------
      SUBROUTINE MR (X, Y, N, K, DWORK, IWORK)
       IMPLICIT NONE
       INTEGER K, N, IWORK
       DOUBLE PRECISION X, Y, DWORK
       DIMENSION X(N,K), Y(N), DWORK(N+2*K), IWORK(K)
       
*         local variables
       INTEGER I, J
       DOUBLE PRECISION TAU, TOT
       
*        maximum of all column sums of magnitudes
       TAU = 0.
       DO J = 1, K
         TOT = 0.
         DO I = 1, N
           TOT = TOT + ABS(X(I,J))
         END DO
         IF (TOT > TAU) TAU = TOT
       END DO
       TAU = TAU * EPSILON(TAU)        ! tolerance argument
       
*            call function
       CALL DHFTI (X, N, N, K, Y, N, 1, TAU, 
     $  J, DWORK(1), DWORK(N+1), DWORK(N+K+1), IWORK)
       IF (J < K) PRINT *, 'mr: solution is rank deficient!'
       RETURN
      END  ! of MR
      
*-----------------------------------------------------------------------
      PROGRAM t_mr        ! polynomial regression example
       IMPLICIT NONE
       INTEGER N, K
       PARAMETER (N=15, K=3)
       INTEGER IWORK(K), I, J
       DOUBLE PRECISION XIN(N), X(N,K), Y(N), DWORK(N+2*K)

       DATA XIN / 1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63, 1.65, 1.68, 
     $            1.70, 1.73, 1.75, 1.78, 1.80, 1.83 /
       DATA Y / 52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29,
     $          63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46 /

*              make coefficient matrix
       DO J = 1, K
         DO I = 1, N
           X(I,J) = XIN(I) **(J-1)
         END DO
       END DO

*               solve
       CALL MR (X, Y, N, K, DWORK, IWORK)
       
*               print result
  10   FORMAT ('beta: ', $)
  20   FORMAT (F12.4, $)
  30   FORMAT ()
       PRINT 10
       DO J = 1, K
         PRINT 20, Y(J)
       END DO       
       PRINT 30
       STOP 'program complete'
      END

```

{{out}}

```txt

beta:     128.8126   -143.1618     61.9603
STOP program complete

```



## Go

The [http://en.wikipedia.org/wiki/Ordinary_least_squares#Example_with_real_data example] on WP happens to be a polynomial regression example, and so code from the [[Polynomial regression]] task can be reused here.  The only difference here is that givens x and y are computed in a separate function as a task prerequisite.

### Library gonum/matrix


```go
package main

import (
    "fmt"

    "github.com/gonum/matrix/mat64"
)

func givens() (x, y *mat64.Dense) {
    height := []float64{1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63,
        1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83}
    weight := []float64{52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
        61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46}
    degree := 2
    x = Vandermonde(height, degree)
    y = mat64.NewDense(len(weight), 1, weight)
    return
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

func main() {
    x, y := givens()
    fmt.Printf("%.4f\n", mat64.Formatted(mat64.QR(x).Solve(y)))
}
```

{{out}}

```txt

⎡ 128.8128⎤
⎢-143.1620⎥
⎣  61.9603⎦

```


### Library go.matrix


```go
package main

import (
    "fmt"

    "github.com/skelterjohn/go.matrix"
)

func givens() (x, y *matrix.DenseMatrix) {
    height := []float64{1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63,
        1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83}
    weight := []float64{52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
        61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46}
    m := len(height)
    n := 3
    y = matrix.MakeDenseMatrix(weight, m, 1)
    x = matrix.Zeros(m, n)
    for i := 0; i < m; i++ {
        ip := float64(1)
        for j := 0; j < n; j++ {
            x.Set(i, j, ip)
            ip *= height[i]
        }
    }
    return
}

func main() {
    x, y := givens()
    n := x.Cols()
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

{{out}}

```txt

[128.8128035784373 -143.16202286476116 61.960325442472865]

```



## Haskell

Using package [http://hackage.haskell.org/package/hmatrix hmatrix] from HackageDB

```haskell
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.LAPACK

m :: Matrix Double
m = (3><3) 
  [7.589183,1.703609,-4.477162,
    -4.597851,9.434889,-6.543450,
    0.4588202,-6.115153,1.331191]

v :: Matrix Double
v = (3><1)
  [1.745005,-4.448092,-4.160842]
```

Using lapack::dgels

```haskell
*Main> linearSolveLSR m v
(3><1)
 [ 0.9335611922087276
 ,  1.101323491272865
 ,    1.6117769115824 ]
```

Or

```haskell
*Main> inv m `multiply`  v
(3><1)
 [ 0.9335611922087278
 ,  1.101323491272865
 , 1.6117769115824006 ]
```



## Hy


```lisp
(import
  [numpy [ones column-stack]]
  [numpy.random [randn]]
  [numpy.linalg [lstsq]])

(setv n 1000)
(setv x1 (randn n))
(setv x2 (randn n))
(setv y (+ 3 (* 1 x1) (* -2 x2) (* .25 x1 x2) (randn n)))

(print (first (lstsq
  (column-stack (, (ones n) x1 x2 (* x1 x2)))
  y)))
```



## J



```j
   NB. Wikipedia data
   x=: 1.47 1.50 1.52 1.55 1.57 1.60 1.63 1.65 1.68 1.70 1.73 1.75 1.78 1.80 1.83
   y=: 52.21 53.12 54.48 55.84 57.20 58.57 59.93 61.29 63.11 64.47 66.28 68.10 69.92 72.19 74.46

   y %. x ^/ i.3   NB. calculate coefficients b1, b2 and b3 for 2nd degree polynomial
128.813 _143.162 61.9603
```


Breaking it down:

```j
   X=: x ^/ i.3                  NB. form Design matrix
   X=: (x^0) ,. (x^1) ,. (x^2)   NB. equivalent of previous line
   4{.X                          NB. show first 4 rows of X
1 1.47 2.1609
1  1.5   2.25
1 1.52 2.3104
1 1.55 2.4025

   NB. Where y is a set of observations and X is the design matrix
   NB. y %. X does matrix division and gives the regression coefficients
   y %. X
128.813 _143.162 61.9603
```

In other words <tt> beta=: y %. X </tt> is the equivalent of:

<math> \hat\beta = (X'X)^{-1}X'y</math>


To confirm:

```j
   mp=: +/ .*                    NB. matrix product
                                 NB. %.X is matrix inverse of X
                                 NB. |:X is transpose of X
   
   (%.(|:X) mp X) mp (|:X) mp y
128.814 _143.163 61.9606
   xpy=: mp~ |:                  NB. Or factoring out "X prime y" (monadically "X prime X")
   X (%.@:xpy@[ mp xpy) y
128.814 _143.163 61.9606

```


LAPACK routines are also available via the Addon <tt>math/lapack</tt>.

```j
   load 'math/lapack'
   load 'math/lapack/gels'
   gels_jlapack_ X;y
128.813 _143.162 61.9603
```



## Julia

{{trans|MATLAB}}

As in Matlab, the backslash or slash operator (depending on the matrix ordering) can be used for solving this problem, for example:


```julia
x = [1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63, 1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83]
y = [52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46]
X = [x.^0 x.^1 x.^2];
b = X \ y
```

{{out}}

```txt

3-element Array{Float64,1}:
  128.813 
 -143.162 
   61.9603

```



## JavaScript

{{works with|SpiderMonkey}} for the <code>print()</code> and <code>''Array''.map()</code> functions.

{{trans|Ruby}}

Extends the Matrix class from [[Matrix Transpose#JavaScript]], [[Matrix multiplication#JavaScript]], [[Reduced row echelon form#JavaScript]].
Uses the IdentityMatrix from [[Matrix exponentiation operator#JavaScript]]

```javascript
// modifies the matrix "in place"
Matrix.prototype.inverse = function() {
    if (this.height != this.width) {
        throw "can't invert a non-square matrix";
    }   

    var I = new IdentityMatrix(this.height);
    for (var i = 0; i < this.height; i++) 
        this.mtx[i] = this.mtx[i].concat(I.mtx[i])
    this.width *= 2;

    this.toReducedRowEchelonForm();

    for (var i = 0; i < this.height; i++) 
        this.mtx[i].splice(0, this.height);
    this.width /= 2;

    return this;
}

function ColumnVector(ary) {
    return new Matrix(ary.map(function(v) {return [v]}))
}
ColumnVector.prototype = Matrix.prototype

Matrix.prototype.regression_coefficients = function(x) {
    var x_t = x.transpose();
    return x_t.mult(x).inverse().mult(x_t).mult(this);
}

// the Ruby example
var y = new ColumnVector([1,2,3,4,5]);
var x = new ColumnVector([2,1,3,4,5]);
print(y.regression_coefficients(x));
print();

// the Tcl example
y = new ColumnVector([
    52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, 
    63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46
]);
x = new Matrix(
    [1.47,1.50,1.52,1.55,1.57,1.60,1.63,1.65,1.68,1.70,1.73,1.75,1.78,1.80,1.83].map(
        function(v) {return [Math.pow(v,0), Math.pow(v,1), Math.pow(v,2)]}
    )
);
print(y.regression_coefficients(x));
```

{{out}}

```txt
0.9818181818181818

128.8128035798277
-143.1620228653037
61.960325442985436
```



## Kotlin

As neither the JDK nor the Kotlin Standard Library has matrix operations built-in, we re-use functions written for various other tasks.

```scala
// Version 1.2.31

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

fun Matrix.transpose(): Matrix {
    val rows = this.size
    val cols = this[0].size
    val trans = Matrix(cols) { Vector(rows) }
    for (i in 0 until cols) {
        for (j in 0 until rows) trans[i][j] = this[j][i]
    }
    return trans
}

fun Matrix.inverse(): Matrix {
    val len = this.size
    require(this.all { it.size == len }) { "Not a square matrix" }
    val aug = Array(len) { DoubleArray(2 * len) }
    for (i in 0 until len) {
        for (j in 0 until len) aug[i][j] = this[i][j]
        // augment by identity matrix to right
        aug[i][i + len] = 1.0
    }
    aug.toReducedRowEchelonForm()
    val inv = Array(len) { DoubleArray(len) }
    // remove identity matrix to left
    for (i in 0 until len) {
        for (j in len until 2 * len) inv[i][j - len] = aug[i][j]
    }
    return inv
}

fun Matrix.toReducedRowEchelonForm() {
    var lead = 0
    val rowCount = this.size
    val colCount = this[0].size
    for (r in 0 until rowCount) {
        if (colCount <= lead) return
        var i = r

        while (this[i][lead] == 0.0) {
            i++
            if (rowCount == i) {
                i = r
                lead++
                if (colCount == lead) return
            }
        }

        val temp = this[i]
        this[i] = this[r]
        this[r] = temp

        if (this[r][lead] != 0.0) {
           val div = this[r][lead]
           for (j in 0 until colCount) this[r][j] /= div
        }
 
        for (k in 0 until rowCount) {
            if (k != r) {
                val mult = this[k][lead]
                for (j in 0 until colCount) this[k][j] -= this[r][j] * mult
            }
        }

        lead++
    }
}

fun printVector(v: Vector) {
    println(v.asList())
    println()
}

fun multipleRegression(y: Vector, x: Matrix): Vector {
    val cy = (arrayOf(y)).transpose()  // convert 'y' to column vector
    val cx = x.transpose()             // convert 'x' to column vector array
    return ((x * cx).inverse() * x * cy).transpose()[0]
}

fun main(args: Array<String>) {
    var y = doubleArrayOf(1.0, 2.0, 3.0, 4.0, 5.0)
    var x = arrayOf(doubleArrayOf(2.0, 1.0, 3.0, 4.0, 5.0))
    var v = multipleRegression(y, x)
    printVector(v)

    y = doubleArrayOf(3.0, 4.0, 5.0)
    x = arrayOf(
        doubleArrayOf(1.0, 2.0, 1.0),
        doubleArrayOf(1.0, 1.0, 2.0)
    )
    v = multipleRegression(y, x)
    printVector(v)

    y = doubleArrayOf(52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29,
                      63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46)

    val a = doubleArrayOf(1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63, 1.65, 1.68, 1.70,
                          1.73, 1.75, 1.78, 1.80, 1.83)
    x = arrayOf(DoubleArray(a.size) { 1.0 }, a, a.map { it * it }.toDoubleArray())
    v = multipleRegression(y, x)
    printVector(v)
}
```


{{out}}

```txt

[0.9818181818181818]

[0.9999999999999996, 2.000000000000001]

[128.8128035798277, -143.1620228653037, 61.960325442985436]

```



## Mathematica


```Mathematica
x = {1.47, 1.50 , 1.52, 1.55, 1.57, 1.60, 1.63, 1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83};
y = {52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46};
X = {x^0, x^1, x^2};
b = y.PseudoInverse[X]

->{128.813, -143.162, 61.9603}
```



## MATLAB


The slash and backslash operator can be used for solving this problem. Here some random data is generated. 


```Matlab
  n=100; k=10; 
  y = randn (1,n);  % generate random vector y
  X = randn (k,n);  % generate random matrix X
  b = y / X
  b = 0.1457109  -0.0777564  -0.0712427  -0.0166193   0.0292955  -0.0079111   0.2265894  -0.0561589  -0.1752146  -0.2577663 
```


In its transposed form yt = Xt * bt, the backslash operator can be used.


```Matlab
  yt = y'; Xt = X';
  bt = Xt \ yt
  bt = 
   0.1457109
  -0.0777564
  -0.0712427
  -0.0166193
   0.0292955
  -0.0079111
   0.2265894
  -0.0561589
  -0.1752146
  -0.2577663
```


Here is the example for estimating the polynomial fit


```Matlab
  x = [1.47 1.50 1.52 1.55 1.57 1.60 1.63 1.65 1.68 1.70 1.73 1.75 1.78 1.80 1.83]
  y = [52.21 53.12 54.48 55.84 57.20 58.57 59.93 61.29 63.11 64.47 66.28 68.10 69.92 72.19 74.46]
  X = [x.^0;x.^1;x.^2];
  b = y/X

   128.813  -143.162    61.960
```


Instead of "/", the slash operator, one can also write : 

```Matlab
 b = y * X' * inv(X * X') 
```

or 

```Matlab
 b = y * pinv(X) 
```



## PARI/GP


```parigp
pseudoinv(M)=my(sz=matsize(M),T=conj(M))~;if(sz[1]<sz[2],T/(M*T),(T*M)^-1*T)
addhelp(pseudoinv, "pseudoinv(M): Moore pseudoinverse of the matrix M.");

y*pseudoinv(X)
```



## Perl 6

We're going to solve the example on the Wikipedia article using [https://github.com/grondilu/clifford Clifford], a [https://en.wikipedia.org/wiki/Geometric_algebra geometric algebra] module.  Optimization for large vector space does not quite work yet, so it's going to take (a lof of) time and a fair amount of memory, but it should work.

Let's create four vectors containing our input data:

<math>\begin{align}
\mathbf{w} & = w^k\mathbf{e}_k\\
\mathbf{h_0} & = (h^k)^0\mathbf{e}_k\\
\mathbf{h_1} & = (h^k)^1\mathbf{e}_k\\
\mathbf{h_2} & = (h^k)^2\mathbf{e}_k
\end{align}</math>

Then what we're looking for are three scalars <math>\alpha</math>, <math>\beta</math> and <math>\gamma</math> such that:

<math>\alpha\mathbf{h0} + \beta\mathbf{h1} + \gamma\mathbf{h2} = \mathbf{w}</math>

To get for instance <math>\alpha</math> we can first make the <math>\beta</math> and <math>\gamma</math> terms disappear:

<math>\alpha\mathbf{h0}\wedge\mathbf{h1}\wedge\mathbf{h2} = \mathbf{w}\wedge\mathbf{h1}\wedge\mathbf{h2}</math>

Noting <math>I = \mathbf{h0}\wedge\mathbf{h1}\wedge\mathbf{h2}</math>, we then get:

<math>\alpha = (\mathbf{w}\wedge\mathbf{h1}\wedge\mathbf{h2})\cdot\tilde{I}/I\cdot\tilde{I}</math>

'''Note:''' a number of the formulae above are invisible to the majority of browsers, including Chrome, IE/Edge, Safari and Opera. They may (subject to the installation of necessary fronts) be visible to Firefox.



```perl6
use Clifford;
my @height = <1.47 1.50 1.52 1.55 1.57 1.60 1.63 1.65 1.68 1.70 1.73 1.75 1.78 1.80 1.83>;
my @weight = <52.21 53.12 54.48 55.84 57.20 58.57 59.93 61.29 63.11 64.47 66.28 68.10 69.92 72.19 74.46>;

my $w = [+] @weight Z* @e;

my $h0 = [+] @e[^@weight];
my $h1 = [+] @height Z* @e;
my $h2 = [+] (@height X** 2) Z* @e;

my $I = $h0∧$h1∧$h2;
my $I2 = ($I·$I.reversion).Real;

say "α = ", ($w∧$h1∧$h2)·$I.reversion/$I2;
say "β = ", ($w∧$h2∧$h0)·$I.reversion/$I2;
say "γ = ", ($w∧$h0∧$h1)·$I.reversion/$I2;
```

{{out}}

```txt
α = 128.81280357844
β = -143.1620228648
γ = 61.960325442
```


This computation took over an hour with the april 2016 version of rakudo on MoarVM, running in a VirtualBox linux system guest hosted by a windows laptop with a i7 intel processor.


## Phix

{{trans|ERRE}}

```Phix
constant N = 15, M=3
sequence x = {1.47,1.50,1.52,1.55,1.57,
              1.60,1.63,1.65,1.68,1.70,
              1.73,1.75,1.78,1.80,1.83},
         y = {52.21,53.12,54.48,55.84,57.20,
              58.57,59.93,61.29,63.11,64.47,
              66.28,68.10,69.92,72.19,74.46},
         s = repeat(0,N),
         t = repeat(0,N),
         a = repeat(repeat(0,M+1),M)
 
    for k=1 to 2*M do
        for i=1 to N do
            s[k] += power(x[i],k-1)
            if k<=M then t[k] += y[i]*power(x[i],k-1) end if
        end for
    end for
 
    -- build linear system
 
    for row=1 to M do
        for col=1 to M do
            a[row,col] = s[row+col-1]
        end for
        a[row,M+1] = t[row]
    end for
 
    puts(1,"Linear system coefficents:\n")
    pp(a,{pp_Nest,1,pp_IntFmt,"%7.1f",pp_FltFmt,"%7.1f"})

    for j=1 to M do
        integer i = j
        while a[i,j]=0 do i += 1 end while
        if i=M+1 then
            ?"SINGULAR MATRIX !"
            ?9/0
        end if
        for k=1 to M+1 do
            {a[j,k],a[i,k]} = {a[i,k],a[j,k]}
        end for
        atom Y = 1/a[j,j]
        a[j] = sq_mul(a[j],Y)
        for i=1 to M do
            if i<>j then
                Y=-a[i,j]
                for k=1 to M+1 do
                    a[i,k] += Y*a[j,k]
                end for
            end if
        end for
    end for
 
    puts(1,"Solutions:\n")
    ?columnize(a,M+1)[1]
```

{{out}}

```txt

Linear system coefficents:
{{   15.0,   24.8,   41.1,  931.2},
 {   24.8,   41.1,   68.4, 1548.2},
 {   41.1,   68.4,  114.3, 2585.5}}
Solutions:
{128.8128036,-143.1620229,61.96032544}

```



## PicoLisp


```PicoLisp
(scl 20)

# Matrix transposition
(de matTrans (Mat)
   (apply mapcar Mat list) )

# Matrix multiplication
(de matMul (Mat1 Mat2)
   (mapcar
      '((Row)
         (apply mapcar Mat2
            '(@ (sum */ Row (rest) (1.0 .))) ) )
      Mat1 ) )

# Matrix identity
(de matIdent (N)
   (let L (need N (1.0) 0)
      (mapcar '(() (copy (rot L))) L) ) )

# Reduced row echelon form
(de reducedRowEchelonForm (Mat)
   (let (Lead 1  Cols (length (car Mat)))
      (for (X Mat X (cdr X))
         (NIL
            (loop
               (T (seek '((R) (n0 (get R 1 Lead))) X)
                  @ )
               (T (> (inc 'Lead) Cols)) ) )
         (xchg @ X)
         (let D (get X 1 Lead)
            (map
               '((R) (set R (*/ (car R) 1.0 D)))
               (car X) ) )
         (for Y Mat
            (unless (== Y (car X))
               (let N (- (get Y Lead))
                  (map
                     '((Dst Src)
                        (inc Dst (*/ N (car Src) 1.0)) )
                     Y
                     (car X) ) ) ) )
         (T (> (inc 'Lead) Cols)) ) )
   Mat )
```

{{trans|JavaScript}}

```PicoLisp
(de matInverse (Mat)
   (let N (length Mat)
      (unless (= N (length (car Mat)))
         (quit "can't invert a non-square matrix") )
      (mapc conc Mat (matIdent N))
      (mapcar '((L) (tail N L)) (reducedRowEchelonForm Mat)) ) )

(de columnVector (Ary)
   (mapcar cons Ary) )

(de regressionCoefficients (Mat X)
   (let Xt (matTrans X)
      (matMul (matMul (matInverse (matMul Xt X)) Xt) Mat) ) )

(setq
   Y (columnVector (1.0 2.0 3.0 4.0 5.0))
   X (columnVector (2.0 1.0 3.0 4.0 5.0)) )

(round (caar (regressionCoefficients Y X)) 17)
```

{{out}}

```txt
-> "0.98181818181818182"
```



## Python

{{libheader|NumPy}}
'''Method with matrix operations'''

```python
import numpy as np

height = [1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63,
    1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83]
weight = [52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
    61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46]

X = np.mat(height**np.arange(3)[:, None])
y = np.mat(weight)

print(y * X.T * (X*X.T).I)
```

{{out}}

```txt

[[ 128.81280359 -143.16202288   61.96032545]]

```

'''Using numpy lstsq function'''

```python
import numpy as np

height = [1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63,
    1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83]
weight = [52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
    61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46]

X = np.array(height)[:, None]**range(3)
y = weight

print(np.linalg.lstsq(X, y)[0])
```

{{out}}

```txt

[ 128.81280358 -143.16202286   61.96032544]

```



## R


R provides the lm() function for linear regression. 


```R
## Wikipedia Data
x <- c(1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63, 1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83)
}
y <- c(52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46)

lm( y ~ x + I(x^2))
```

{{out}}

```txt

Call:
lm(formula = y ~ x + I(x^2))

Coefficients:
(Intercept)            x       I(x^2)  
     128.81      -143.16        61.96  

```


A simple implementation of multiple regression in native R 
is useful to illustrate R's model description and linear algebra capabilities.


```R
simpleMultipleReg <- function(formula) {

    ## parse and evaluate the model formula
    mf <- model.frame(formula)

    ## create design matrix
    X <- model.matrix(attr(mf, "terms"), mf)

    ## create dependent variable
    Y <- model.response(mf)

    ## solve
    solve(t(X) %*% X) %*% t(X) %*% Y
}

simpleMultipleReg(y ~ x + I(x^2))
```


This produces the same coefficients as lm()

```txt

                  [,1]
(Intercept)  128.81280
x           -143.16202
I(x^2)        61.96033

```



A more efficient way to solve <math>(X'X)^{-1} X' y</math>, 
than the method above, is to solve the linear system directly 
and use the crossprod function:

```R
solve( crossprod(X), crossprod(X, Y))
```



## Racket


```racket

#lang racket
(require math)
(define T matrix-transpose)

(define (fit X y)
  (matrix-solve (matrix* (T X) X) (matrix* (T X) y)))

```

Test:

```racket

(fit (matrix [[1 2]
              [2 5]
              [3 7]
              [4 9]])
     (matrix [[1]
              [2]
              [3]
              [9]]))
{{out}}
(array #[#[9 1/3] #[-3 1/3]])

```



## Ruby


Using the standard library Matrix class:


```ruby
require 'matrix'

def regression_coefficients y, x
  y = Matrix.column_vector y.map { |i| i.to_f }
  x = Matrix.columns x.map { |xi| xi.map { |i| i.to_f }}

  (x.t * x).inverse * x.t * y
end
```


Testing 2-dimension:

```ruby
puts regression_coefficients([1, 2, 3, 4, 5], [ [2, 1, 3, 4, 5] ])
```

{{out}}

```txt
Matrix[[0.981818181818182]]
```


Testing 3-dimension:
Points(x,y,z): [1,1,3], [2,1,4] and [1,2,5]

```ruby
puts regression_coefficients([3,4,5], [ [1,2,1], [1,1,2] ])
```

{{out}}

```txt
Matrix[[0.9999999999999996], [2.0]]
```



## Stata


First, build a random dataset:


```stata
clear
set seed 17760704
set obs 200
forv i=1/4 {
	gen x`i'=rnormal()
}
gen y=1.5+0.8*x1-0.7*x2+1.1*x3-1.7*x4+rnormal()
```


Now, use the '''[https://www.stata.com/help.cgi?regress regress]''' command:


```stata
reg y x*
```


'''Output'''

The command shows the coefficients along with a bunch of useful information, such as R<sup>2</sup>, F statistic, standard errors of the coefficients...

```txt

      Source |       SS           df       MS      Number of obs   =       200
-------------+----------------------------------   F(4, 195)       =    355.15
       Model |  1343.81757         4  335.954392   Prob > F        =    0.0000
    Residual |  184.458622       195  .945941649   R-squared       =    0.8793
-------------+----------------------------------   Adj R-squared   =    0.8768
       Total |  1528.27619       199  7.67977985   Root MSE        =     .9726

------------------------------------------------------------------------------
           y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
          x1 |   .7525247   .0689559    10.91   0.000     .6165295    .8885198
          x2 |  -.7036303   .0697456   -10.09   0.000    -.8411828   -.5660778
          x3 |   1.157477    .072189    16.03   0.000     1.015106    1.299849
          x4 |  -1.718201   .0621758   -27.63   0.000    -1.840824   -1.595577
       _cons |   1.399131   .0697862    20.05   0.000     1.261499    1.536764
------------------------------------------------------------------------------
```


The regress command also sets a number of '''[https://www.stata.com/help.cgi?ereturn ereturn]''' values, which can be used by subsequent commands. The coefficients and their standard errors also have a [https://www.stata.com/help.cgi?_variables special syntax]:


```stata
. di _b[x1]
.75252466

. di _b[_cons]
1.3991314

. di _se[x1]
.06895593

. di _se[_cons]
.06978623
```


See '''[https://www.stata.com/help.cgi?estat estat]''', '''[https://www.stata.com/help.cgi?predict predict]''', '''[https://www.stata.com/help.cgi?estimates estimates]''', '''[https://www.stata.com/help.cgi?margins margins]''' for examples of commands that can be used after a regression.

Here we compute [[wp:Akaike information criterion|Akaike's AIC]], the covariance matrix of the estimates, the predicted values and residuals:


```stata
. estat ic

Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |        Obs  ll(null)  ll(model)      df         AIC        BIC
-------------+---------------------------------------------------------------
           . |        200 -487.1455  -275.6985       5     561.397   577.8886
-----------------------------------------------------------------------------
               Note: N=Obs used in calculating BIC; see [R] BIC note.

. estat vce

Covariance matrix of coefficients of regress model

        e(V) |         x1          x2          x3          x4       _cons 
-------------+------------------------------------------------------------
          x1 |  .00475492                                                 
          x2 | -.00040258   .00486445                                     
          x3 | -.00042516   .00017355   .00521125                         
          x4 | -.00011915   -.0002568   .00054646   .00386583             
       _cons |  .00030777  -.00031109  -.00023794   .00058926   .00487012

. predict yhat, xb
. predict r, r
```



## Tcl

{{tcllib|math::linearalgebra}}

```tcl
package require math::linearalgebra
namespace eval multipleRegression {
    namespace export regressionCoefficients
    namespace import ::math::linearalgebra::*

    # Matrix inversion is defined in terms of Gaussian elimination
    # Note that we assume (correctly) that we have a square matrix
    proc invert {matrix} {
	solveGauss $matrix [mkIdentity [lindex [shape $matrix] 0]]
    }
    # Implement the Ordinary Least Squares method
    proc regressionCoefficients {y x} {
	matmul [matmul [invert [matmul $x [transpose $x]]] $x] $y
    }
}
namespace import multipleRegression::regressionCoefficients
```

Using an example from the Wikipedia page on the correlation of height and weight:

```tcl
# Simple helper just for this example
proc map {n exp list} {
    upvar 1 $n v
    set r {}; foreach v $list {lappend r [uplevel 1 $exp]}; return $r
}

# Data from wikipedia
set x {
    1.47 1.50 1.52 1.55 1.57 1.60 1.63 1.65 1.68 1.70 1.73 1.75 1.78 1.80 1.83
}
set y {
    52.21 53.12 54.48 55.84 57.20 58.57 59.93 61.29 63.11 64.47 66.28 68.10
    69.92 72.19 74.46
}
# Wikipedia states that fitting up to the square of x[i] is worth it
puts [regressionCoefficients $y [map n {map v {expr {$v**$n}} $x} {0 1 2}]]
```

{{out}} (a 3-vector of coefficients):

```txt
128.81280358170625 -143.16202286630732 61.96032544293041
```



## Ursala

This exact problem is solved by the DGELSD function from
the Lapack library [http://www.netlib.org/lapack/lug/node27.html],
which is callable in Ursala like this:

```Ursala
regression_coefficients = lapack..dgelsd
```

test program:

```Ursala
x = 

<
   <7.589183e+00,1.703609e+00,-4.477162e+00>,
   <-4.597851e+00,9.434889e+00,-6.543450e+00>,
   <4.588202e-01,-6.115153e+00,1.331191e+00>>

y = <1.745005e+00,-4.448092e+00,-4.160842e+00>

#cast %eL

example = regression_coefficients(x,y)
```

The matrix x needn't be square, and has one row for each data point. 
The length of y must equal the number of rows in x, 
and the number of coefficients returned will be the number of columns in x. 
It would be more typical in practice to initialize x 
by evaluating a set of basis functions chosen to model some empirical data,
but the regression solver is indifferent to the model.

{{out}}

```txt

<9.335612e-01,1.101323e+00,1.611777e+00>

```

A similar method can be used for regression with complex numbers by substituting
zgelsd for dgelsd, above.


## zkl

Using the GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
height:=GSL.VectorFromData(1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63,
		   1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83);
weight:=GSL.VectorFromData(52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
		   61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46);
v:=GSL.polyFit(height,weight,2);
v.format().println();
GSL.Helpers.polyString(v).println();
GSL.Helpers.polyEval(v,height).format().println();
```

{{out}}

```txt

128.81,-143.16,61.96
 128.813 - 143.162x + 61.9603x^2 
52.25,53.48,54.36,55.77,56.77,58.37,60.08,61.28,63.18,64.50,66.58,68.03,70.30,71.87,74.33

```


Or, using Lists:
{{trans|Common Lisp}}

```zkl
// Solve a linear system AX=B where A is symmetric and positive definite, so it can be Cholesky decomposed.
fcn linsys(A,B){
   n,m:=A.len(),B[1].len();  // A.rows,B.cols
   y:=n.pump(List.createLong(n).write,0.0); // writable vector of n zeros
   X:=make_array(n,m,0.0);
   L:=cholesky(A); // A=LL'

   foreach col in (m){
      foreach k in (n){ // Forward substitution: y = L\B
         y[k]=( B[k][col] - k.reduce('wrap(s,j){ s + L[k][j]*y[j] },0.0) )
	      /L[k][k];
      }
      foreach k in ([n-1..0,-1]){   // Back substitution. x=L'\y
         X[k][col]=
	  ( y[k] - (k+1).reduce(n-k-1,'wrap(s,j){ s + L[j][k]*X[j][col] },0.0) )
	  /L[k][k];
      }
   }
   X   
}
fcn cholesky(mat){   // Cholesky decomposition task
   rows:=mat.len();
   r:=(0).pump(rows,List().write, (0).pump(rows,List,0.0).copy); // matrix of zeros
   foreach i,j in (rows,i+1){ 
      s:=(0).reduce(j,'wrap(s,k){ s + r[i][k]*r[j][k] },0.0);
      r[i][j]=( if(i==j)(mat[i][i] - s).sqrt()
	        else    1.0/r[j][j]*(mat[i][j] - s) );
   }
   r
}

// Solve a linear least squares problem. Ax=b, with A being mxn, with m>n.
// Solves the linear system A'Ax=A'b.
fcn lsqr(A,b){
   at:=transpose(A);
   linsys(matMult(at,A), matMult(at,b));
}
// Least square fit of a polynomial of order n the x-y-curve.
fcn polyfit(x,y,n){
   n+=1;
   m:=x[0].len();  // columns
   A:=make_array(m,n,0.0);
   foreach i,j in (m,n){ A[i][j]=x[0][i].pow(j); }
   lsqr(A, transpose(y));
}
fcn make_array(n,m,v){ (m).pump(List.createLong(m).write,v)*n }
fcn matMult(a,b){
   n,m,p:=a[0].len(),a.len(),b[0].len();
   ans:=make_array(m,p,0.0);
   foreach i,j,k in (m,p,n){ ans[i][j]+=a[i][k]*b[k][j]; }
   ans
}
fcn transpose(M){ 
   if(M.len()==1) M[0].pump(List,List.create); // 1 row --> n columns
   else M[0].zip(M.xplode(1));
}
```


```zkl
height:=T(T(1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63,
        1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83));
weight:=T(T(52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93,
        61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46));
polyfit(height,weight,2).flatten().println();
```

{{out}}

```txt

L(128.813,-143.162,61.9603)

```

