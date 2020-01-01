+++
title = "Matrix transposition"
description = ""
date = 2019-10-19T10:39:44Z
aliases = []
[extra]
id = 2542
[taxonomies]
categories = []
tags = []
+++

{{task|Matrices}}
[[wp:Transpose|Transpose]] an arbitrarily sized rectangular [[wp:Matrix (mathematics)|Matrix]].





## 360 Assembly


```360asm
...
KN       EQU    3
KM       EQU    5
N        DC     AL2(KN)
M        DC     AL2(KM)
A        DS     (KN*KM)F           matrix a(n,m)
B        DS     (KM*KN)F           matrix b(m,n)
...
*        b(j,i)=a(i,j)
*        transposition using Horner's formula
         LA     R4,0               i,from 1
         LA     R7,KN              to n
         LA     R6,1               step 1
LOOPI    BXH    R4,R6,ELOOPI       do i=1 to n
         LA     R5,0               j,from 1
         LA     R9,KM              to m
         LA     R8,1               step 1
LOOPJ    BXH    R5,R8,ELOOPJ       do j=1 to m
         LR     R1,R4              i
         BCTR   R1,0               i-1
         MH     R1,M               (i-1)*m
         LR     R2,R5              j
         BCTR   R2,0               j-1
         AR     R1,R2              r1=(i-1)*m+(j-1)
         SLA    R1,2               r1=((i-1)*m+(j-1))*itemlen
         L      R0,A(R1)           r0=a(i,j)
         LR     R1,R5              j
         BCTR   R1,0               j-1
         MH     R1,N               (j-1)*n
         LR     R2,R4              i
         BCTR   R2,0               i-1
         AR     R1,R2              r1=(j-1)*n+(i-1)
         SLA    R1,2               r1=((j-1)*n+(i-1))*itemlen
         ST     R0,B(R1)           b(j,i)=r0
         B      LOOPJ              next j
ELOOPJ   EQU    *                  out of loop j
         B      LOOPI              next i
ELOOPI   EQU    *                  out of loop i
...
```



## ACL2


```Lisp
(defun cons-each (xs xss)
   (if (or (endp xs) (endp xss))
       nil
       (cons (cons (first xs) (first xss))
             (cons-each (rest xs) (rest xss)))))

(defun list-each (xs)
   (if (endp xs)
       nil
       (cons (list (first xs))
             (list-each (rest xs)))))

(defun transpose-list (xss)
   (if (endp (rest xss))
       (list-each (first xss))
       (cons-each (first xss)
                  (transpose-list (rest xss)))))
```



## ActionScript

In ActionScript, multi-dimensional arrays are created by making an "Array of arrays" where each element is an array.

```ActionScript
function transpose( m:Array):Array
{
	//Assume each element in m is an array. (If this were production code, use typeof to be sure)

	//Each element in m is a row, so this gets the length of a row in m,
	//which is the same as the number of rows in m transpose.
	var mTranspose = new Array(m[0].length);
	for(var i:uint = 0; i < mTranspose.length; i++)
	{
                //create a row
		mTranspose[i] = new Array(m.length);
                //set the row to the appropriate values
		for(var j:uint = 0; j < mTranspose[i].length; j++)
			mTranspose[i][j] = m[j][i];
	}
	return mTranspose;
}
var m:Array = [[1, 2, 3, 10],
	       [4, 5, 6, 11],
	       [7, 8, 9, 12]];
var M:Array = transpose(m);
for(var i:uint = 0; i < M.length; i++)
	trace(M[i]);
```



## Ada

Transpose is a function of the standard Ada library provided for matrices built upon any floating-point or complex type. The relevant packages are Ada.Numerics.Generic_Real_Arrays and Ada.Numerics.Generic_Complex_Arrays, correspondingly.

This example illustrates use of Transpose for the matrices built upon the standard type Float:

```ada
with Ada.Numerics.Real_Arrays;  use Ada.Numerics.Real_Arrays;
with Ada.Text_IO;               use Ada.Text_IO;

procedure Matrix_Transpose is
   procedure Put (X : Real_Matrix) is
      type Fixed is delta 0.01 range -500.0..500.0;
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Put (Fixed'Image (Fixed (X (I, J))));
         end loop;
         New_Line;
      end loop;
   end Put;

   Matrix : constant Real_Matrix :=
            (  (0.0, 0.1, 0.2, 0.3),
               (0.4, 0.5, 0.6, 0.7),
               (0.8, 0.9, 1.0, 1.1)
            );
begin
   Put_Line ("Before Transposition:");
   Put (Matrix);
   New_Line;
   Put_Line ("After Transposition:");
   Put (Transpose (Matrix));
end Matrix_Transpose;
```

{{out}}

```txt

Before Transposition:
 0.00 0.10 0.20 0.30
 0.40 0.50 0.60 0.70
 0.80 0.90 1.00 1.10

After Transposition:
 0.00 0.40 0.80
 0.10 0.50 0.90
 0.20 0.60 1.00
 0.30 0.70 1.10

```



## Agda


```agda
module Matrix where

open import Data.Nat
open import Data.Vec

Matrix : (A : Set) → ℕ → ℕ → Set
Matrix A m n = Vec (Vec A m) n

transpose : ∀ {A m n} → Matrix A m n → Matrix A n m
transpose [] = replicate []
transpose (xs ∷ xss) = zipWith _∷_ xs (transpose xss)

a = (1 ∷ 2 ∷ 3 ∷ []) ∷ (4 ∷ 5 ∷ 6 ∷ []) ∷ []
b = transpose a
```


'''b''' evaluates to the following normal form:


```agda
(1 ∷ 4 ∷ []) ∷ (2 ∷ 5 ∷ []) ∷ (3 ∷ 6 ∷ []) ∷ []
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
main:(

  [,]REAL m=((1,  1,  1,   1),
             (2,  4,  8,  16),
             (3,  9, 27,  81),
             (4, 16, 64, 256),
             (5, 25,125, 625));

  OP ZIP = ([,]REAL in)[,]REAL:(
    [2 LWB in:2 UPB in,1 LWB in:1UPB in]REAL out;
    FOR i FROM LWB in TO UPB in DO
       out[,i]:=in[i,]
    OD;
    out
  );

  PROC pprint = ([,]REAL m)VOID:(
    FORMAT real fmt = $g(-6,2)$; # width of 6, with no '+' sign, 2 decimals #
     FORMAT vec fmt = $"("n(2 UPB m-1)(f(real fmt)",")f(real fmt)")"$;
    FORMAT matrix fmt = $x"("n(UPB m-1)(f(vec fmt)","lxx)f(vec fmt)");"$;
    # finally print the result #
    printf((matrix fmt,m))
  );

  printf(($x"Transpose:"l$));
  pprint((ZIP m))
)
```

{{out}}

```txt

Transpose:
((  1.00,  2.00,  3.00,  4.00,  5.00),
 (  1.00,  4.00,  9.00, 16.00, 25.00),
 (  1.00,  8.00, 27.00, 64.00,125.00),
 (  1.00, 16.00, 81.00,256.00,625.00));

```



## APL

If M is a matrix, ⍉M is its transpose. For example,

```apl

      3 3⍴⍳10
1 2 3
4 5 6
7 8 9
      ⍉ 3 3⍴⍳10
1 4 7
2 5 8
3 6 9

```



## AppleScript


We can do this iteratively, by manually setting up two nested loops, and initialising iterators and empty lists,


```applescript
on run
    transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]])

    --> {{1, 4, 7, 10}, {2, 5, 8, 11}, {3, 6, 9, 12}}
end run

on transpose(xss)
    set lstTrans to {}

    repeat with iCol from 1 to length of item 1 of xss
        set lstCol to {}

        repeat with iRow from 1 to length of xss
            set end of lstCol to item iCol of item iRow of xss
        end repeat

        set end of lstTrans to lstCol
    end repeat

    return lstTrans
end transpose
```



or, if our library contains some generic basics like '''map()''', and we use the AS script mechanism for closures, we can delegate the iterative details and write transpose() a little more declaratively, without having to reach for '''set''', '''repeat''', or '''return''' inside its definition.

{{trans|JavaScript}}


```applescript
-- TRANSPOSE -----------------------------------------------------------------

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose


-- TEST ----------------------------------------------------------------------
on run
    transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]])

    --> {{1, 4, 7, 10}, {2, 5, 8, 11}, {3, 6, 9, 12}}
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```


{{Out}}

```AppleScript
{{1, 4, 7, 10}, {2, 5, 8, 11}, {3, 6, 9, 12}}
```



## AutoHotkey


```AutoHotkey
a = a
m = 10
n = 10
Loop, 10
{
  i := A_Index - 1
  Loop, 10
  {
    j := A_Index - 1
    %a%%i%%j% := i - j
  }
}
before := matrix_print("a", m, n)
transpose("a", m, n)
after := matrix_print("a", m, n)
MsgBox % before . "`ntransposed:`n" . after
Return

transpose(a, m, n)
{
  Local i, j, row, matrix
  Loop, % m
  {
    i := A_Index - 1
    Loop, % n
    {
      j := A_Index - 1
      temp%i%%j% := %a%%j%%i%
    }
  }
  Loop, % m
  {
    i := A_Index - 1
    Loop, % n
    {
      j := A_Index - 1
      %a%%i%%j% := temp%i%%j%
    }
  }
}

matrix_print(a, m, n)
{
  Local i, j, row, matrix
  Loop, % m
  {
    i := A_Index - 1
    row := ""
    Loop, % n
    {
      j := A_Index - 1
      row .= %a%%i%%j% . ","
    }
    StringTrimRight, row, row, 1
    matrix .= row . "`n"
  }
  Return matrix
}

```


### Using Objects


```AutoHotkey
Transpose(M){
	R := []
	for i, row in M
		for j, col in row
			R[j,i] := col
	return R
}
```

Examples:
```AutoHotkey
Matrix := [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
MsgBox % 	""
		. "Original Matrix :`n" 		Print(Matrix)
		. "`nTransposed Matrix :`n" 	Print(Transpose(Matrix))

Print(M){
	for i, row in M
		for j, col in row
			Res .= (A_Index=1?"":"`t") col (Mod(A_Index,M[1].MaxIndex())?"":"`n")
	return Trim(Res,"`n")
}
```

{{out}}

```txt
Original Matrix :
1	2	3
4	5	6
7	8	9
10	11	12

Transposed Matrix :
1	4	7	10
2	5	8	11
3	6	9	12

```



## AWK


```AWK

# syntax: GAWK -f MATRIX_TRANSPOSITION.AWK filename
{   if (NF > nf) {
        nf = NF
    }
    for (i=1; i<=nf; i++) {
        row[i] = row[i] $i " "
    }
}
END {
    for (i=1; i<=nf; i++) {
        printf("%s\n",row[i])
    }
    exit(0)
}

```

<p>input:</p>

```txt

1 2 3 4
5 6 7 8
9 10 11 12

```

{{out}}

```txt

1 5 9
2 6 10
3 7 11
4 8 12

```

===Using 2D-Arrays===

```AWK
# Usage: GAWK -f MATRIX_TRANSPOSITION.AWK filename
{
    i = NR
    for (j = 1; j <= NF; j++) {
        a[i,j] = $j
    }
    ranka1 = i
    ranka2 = max(ranka2, NF)
}
END {
    rankb1 = ranka2
    rankb2 = ranka1
    b[rankb1, rankb2] = 0
    transpose_matrix(b, a)
    for (i = 1; i <= rankb1; i++) {
        for (j = 1; j <= rankb2; j++) {
            printf("%g%c", b[i,j], j < rankb2 ? " " : "\n");
        }
    }
}
function transpose_matrix(target, source,     key, idx) {
    for (key in source) {
        split(key, idx, SUBSEP)
        target[idx[2], idx[1]] = source[idx[1], idx[2]]
    }
}
function max(m, n) {
    return m > n ? m : n
}
```

<p><b>Input:</b></p>

```txt

1 2 3
4 5 6

```

{{out}}

```txt

1. 4.
2. 5.
3. 6.

```



## BASIC

{{works with|QuickBasic|4.5}}
 CLS
 DIM m(1 TO 5, 1 TO 4) 'any dimensions you want

 'set up the values in the array
 FOR rows = LBOUND(m, 1) TO UBOUND(m, 1) 'LBOUND and UBOUND can take a dimension as their second argument
        FOR cols = LBOUND(m, 2) TO UBOUND(m, 2)
        m(rows, cols) = rows ^ cols 'any formula you want
        NEXT cols
 NEXT rows

 'declare the new matrix
 DIM trans(LBOUND(m, 2) TO UBOUND(m, 2), LBOUND(m, 1) TO UBOUND(m, 1))

 'copy the values
 FOR rows = LBOUND(m, 1) TO UBOUND(m, 1)
        FOR cols = LBOUND(m, 2) TO UBOUND(m, 2)
        trans(cols, rows) = m(rows, cols)
        NEXT cols
 NEXT rows

 'print the new matrix
 FOR rows = LBOUND(trans, 1) TO UBOUND(trans, 1)
        FOR cols = LBOUND(trans, 2) TO UBOUND(trans, 2)
        PRINT trans(rows, cols);
        NEXT cols
 PRINT
 NEXT rows


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"ARRAYLIB"

      DIM matrix(3,4), transpose(4,3)
      matrix() = 78,19,30,12,36,49,10,65,42,50,30,93,24,78,10,39,68,27,64,29

      PROC_transpose(matrix(), transpose())

      FOR row% = 0 TO DIM(matrix(),1)
        FOR col% = 0 TO DIM(matrix(),2)
          PRINT ;matrix(row%,col%) " ";
        NEXT
        PRINT
      NEXT row%

      PRINT

      FOR row% = 0 TO DIM(transpose(),1)
        FOR col% = 0 TO DIM(transpose(),2)
          PRINT ;transpose(row%,col%) " ";
        NEXT
        PRINT
      NEXT row%
```

{{out}}

```txt

78 19 30 12 36
49 10 65 42 50
30 93 24 78 10
39 68 27 64 29

78 49 30 39
19 10 93 68
30 65 24 27
12 42 78 64
36 50 10 29

```



## Burlesque



```burlesque

blsq ) {{78 19 30 12 36}{49 10 65 42 50}{30 93 24 78 10}{39 68 27 64 29}}tpsp
78 49 30 39
19 10 93 68
30 65 24 27
12 42 78 64
36 50 10 29

```



## C

Transpose a 2D double array.

```c
#include <stdio.h>

void transpose(void *dest, void *src, int src_h, int src_w)
{
	int i, j;
	double (*d)[src_h] = dest, (*s)[src_w] = src;
	for (i = 0; i < src_h; i++)
		for (j = 0; j < src_w; j++)
			d[j][i] = s[i][j];
}

int main()
{
	int i, j;
	double a[3][5] = {{ 0, 1, 2, 3, 4 },
			  { 5, 6, 7, 8, 9 },
			  { 1, 0, 0, 0, 42}};
	double b[5][3];
	transpose(b, a, 3, 5);

	for (i = 0; i < 5; i++)
		for (j = 0; j < 3; j++)
			printf("%g%c", b[i][j], j == 2 ? '\n' : ' ');
	return 0;
}
```


Transpose a matrix of size w x h in place with only O(1) space and without moving any element more than once. See the [[wp:In-place_matrix_transposition|Wikipedia article]] for more information.

```c
#include <stdio.h>

void transpose(double *m, int w, int h)
{
	int start, next, i;
	double tmp;

	for (start = 0; start <= w * h - 1; start++) {
		next = start;
		i = 0;
		do {	i++;
			next = (next % h) * w + next / h;
		} while (next > start);
		if (next < start || i == 1) continue;

		tmp = m[next = start];
		do {
			i = (next % h) * w + next / h;
			m[next] = (i == start) ? tmp : m[i];
			next = i;
		} while (next > start);
	}
}

void show_matrix(double *m, int w, int h)
{
	int i, j;
	for (i = 0; i < h; i++) {
		for (j = 0; j < w; j++)
			printf("%2g ", m[i * w + j]);
		putchar('\n');
	}
}

int main(void)
{
	int i;
	double m[15];
	for (i = 0; i < 15; i++) m[i] = i + 1;

	puts("before transpose:");
	show_matrix(m, 3, 5);

	transpose(m, 3, 5);

	puts("\nafter transpose:");
	show_matrix(m, 5, 3);

	return 0;
}
```

{{out}}

```txt

before transpose:
 1  2  3
 4  5  6
 7  8  9
10 11 12
13 14 15

after transpose:
 1  4  7 10 13
 2  5  8 11 14
 3  6  9 12 15
```



## C++

C++ does not have a built-in or standard-library Matrix class, so many users have rolled their own.  Boost supplies one (boost::numeric::ublas::matrix<element_t> in the example below).  Many users have rolled their own matrix class; a (long) code sample below shows such a class.

{{libheader|Boost.uBLAS}}

```cpp
#include <boost/numeric/ublas/matrix.hpp>

#include <boost/numeric/ublas/io.hpp>

int main()
{
  using namespace boost::numeric::ublas;

  matrix<double> m(3,3);

  for(int i=0; i!=m.size1(); ++i)
    for(int j=0; j!=m.size2(); ++j)
      m(i,j)=3*i+j;

  std::cout << trans(m) << std::endl;
}
```


{{out}}

```txt

 [3,3]((0,3,6),(1,4,7),(2,5,8))

```



### Generic solution

;main.cpp

```cpp
#include <iostream>
#include "matrix.h"

#if !defined(ARRAY_SIZE)
    #define ARRAY_SIZE(x) (sizeof((x)) / sizeof((x)[0]))
#endif

template<class T>
void printMatrix(const Matrix<T>& m) {
    std::cout << "rows = " << m.rowNum() << "   columns = " << m.colNum() << std::endl;
    for (unsigned int i = 0; i < m.rowNum(); i++) {
        for (unsigned int j = 0; j < m.colNum(); j++) {
            std::cout <<  m[i][j] << "  ";
        }
        std::cout << std::endl;
    }
} /* printMatrix() */

int main() {
    int  am[2][3] = {
        {1,2,3},
        {4,5,6},
    };

    Matrix<int> a(ARRAY_SIZE(am), ARRAY_SIZE(am[0]), am[0], ARRAY_SIZE(am)*ARRAY_SIZE(am[0]));

    try {
        std::cout << "Before transposition:" << std::endl;
        printMatrix(a);
        std::cout << std::endl;
        a.transpose();
        std::cout << "After transposition:" << std::endl;
        printMatrix(a);
    } catch (MatrixException& e) {
        std::cerr << e.message() << std::endl;
        return e.errorCode();
    }

} /* main() */
```

;matrix.h

```cpp
#ifndef _MATRIX_H
#define	_MATRIX_H

#include <sstream>
#include <string>
#include <vector>
#include <algorithm>

#define MATRIX_ERROR_CODE_COUNT 5
#define MATRIX_ERR_UNDEFINED "1 Undefined exception!"
#define MATRIX_ERR_WRONG_ROW_INDEX "2 The row index is out of range."
#define MATRIX_ERR_MUL_ROW_AND_COL_NOT_EQUAL "3 The row number of second matrix must be equal with the column number of first matrix!"
#define MATRIX_ERR_MUL_ROW_AND_COL_BE_GREATER_THAN_ZERO "4 The number of rows and columns must be greater than zero!"
#define MATRIX_ERR_TOO_FEW_DATA "5 Too few data in matrix."

class MatrixException {
private:
    std::string message_;
    int errorCode_;
public:
    MatrixException(std::string message = MATRIX_ERR_UNDEFINED);

    inline std::string message() {
        return message_;
    };

    inline int errorCode() {
        return errorCode_;
    };
};

MatrixException::MatrixException(std::string message) {
    errorCode_ = MATRIX_ERROR_CODE_COUNT + 1;
    std::stringstream ss(message);
    ss >> errorCode_;
    if (errorCode_ < 1) {
        errorCode_ = MATRIX_ERROR_CODE_COUNT + 1;
    }
    std::string::size_type pos = message.find(' ');
    if (errorCode_ <= MATRIX_ERROR_CODE_COUNT && pos != std::string::npos) {
        message_ = message.substr(pos + 1);
    } else {
        message_ = message + " (This an unknown and unsupported exception!)";
    }
}

/**
 * Generic class for matrices.
 */
template <class T>
class Matrix {
private:
    std::vector<T> v; // the data of matrix
    unsigned int m;   // the number of rows
    unsigned int n;   // the number of columns
protected:

    virtual void clear() {
        v.clear();
        m = n = 0;
    }
public:

    Matrix() {
        clear();
    }
    Matrix(unsigned int, unsigned int, T* = 0, unsigned int = 0);
    Matrix(unsigned int, unsigned int, const std::vector<T>&);

    virtual ~Matrix() {
        clear();
    }
    Matrix& operator=(const Matrix&);
    std::vector<T> operator[](unsigned int) const;
    Matrix operator*(const Matrix&);
    void transpose();

    inline unsigned int rowNum() const {
        return m;
    }

    inline unsigned int colNum() const {
        return n;
    }

    inline unsigned int size() const {
        return v.size();
    }

    inline void add(const T& t) {
        v.push_back(t);
    }
};

template <class T>
Matrix<T>::Matrix(unsigned int row, unsigned int col, T* data, unsigned int dataLength) {
    clear();
    if (row > 0 && col > 0) {
        m = row;
        n = col;
        unsigned int mxn = m * n;
        if (dataLength && data) {
            for (unsigned int i = 0; i < dataLength && i < mxn; i++) {
                v.push_back(data[i]);
            }
        }
    }
}

template <class T>
Matrix<T>::Matrix(unsigned int row, unsigned int col, const std::vector<T>& data) {
    clear();
    if (row > 0 && col > 0) {
        m = row;
        n = col;
        unsigned int mxn = m * n;
        if (data.size() > 0) {
            for (unsigned int i = 0; i < mxn && i < data.size(); i++) {
                v.push_back(data[i]);
            }
        }
    }
}

template<class T>
Matrix<T>& Matrix<T>::operator=(const Matrix<T>& other) {
    clear();
    if (other.m > 0 && other.n > 0) {
        m = other.m;
        n = other.n;
        unsigned int mxn = m * n;
        for (unsigned int i = 0; i < mxn && i < other.size(); i++) {
            v.push_back(other.v[i]);
        }
    }
    return *this;
}

template<class T>
std::vector<T> Matrix<T>::operator[](unsigned int index) const {
    std::vector<T> result;
    if (index >= m) {
        throw MatrixException(MATRIX_ERR_WRONG_ROW_INDEX);
    } else if ((index + 1) * n > size()) {
        throw MatrixException(MATRIX_ERR_TOO_FEW_DATA);
    } else {
        unsigned int begin = index * n;
        unsigned int end = begin + n;
        for (unsigned int i = begin; i < end; i++) {
            result.push_back(v[i]);
        }
    }
    return result;
}

template<class T>
Matrix<T> Matrix<T>::operator*(const Matrix<T>& other) {
    Matrix result(m, other.n);
    if (n != other.m) {
        throw MatrixException(MATRIX_ERR_MUL_ROW_AND_COL_NOT_EQUAL);
    } else if (m <= 0 || n <= 0 || other.n <= 0) {
        throw MatrixException(MATRIX_ERR_MUL_ROW_AND_COL_BE_GREATER_THAN_ZERO);
    } else if (m * n > size() || other.m * other.n > other.size()) {
        throw MatrixException(MATRIX_ERR_TOO_FEW_DATA);
    } else {
        for (unsigned int i = 0; i < m; i++) {
            for (unsigned int j = 0; j < other.n; j++) {
                T temp = v[i * n] * other.v[j];
                for (unsigned int k = 1; k < n; k++) {
                    temp += v[i * n + k] * other.v[k * other.n + j];
                }
                result.v.push_back(temp);
            }
        }
    }
    return result;
}

template<class T>
void Matrix<T>::transpose() {
    if (m * n > size()) {
        throw MatrixException(MATRIX_ERR_TOO_FEW_DATA);
    } else {
        std::vector<T> v2;
        std::swap(v, v2);
        for (unsigned int i = 0; i < n; i++) {
            for (unsigned int j = 0; j < m; j++) {
                v.push_back(v2[j * n + i]);
            }
        }
        std::swap(m, n);
    }
}

#endif	/* _MATRIX_H */
```


{{out}}

```txt
Before transposition:
rows = 2   columns = 3
1  2  3
4  5  6

After transposition:
rows = 3   columns = 2
1  4
2  5
3  6
```



### Easy Mode


```cpp
#include <iostream>

int main(){
    const int l = 5;
    const int w = 3;
    int m1[l][w] = {{1,2,3}, {4,5,6}, {7,8,9}, {10,11,12}, {13,14,15}};
    int m2[w][l];

    for(int i=0; i<w; i++){
        for(int x=0; x<l; x++){
            m2[i][x]=m1[x][i];
        }
    }

    // This is just output...

    std::cout << "Before:";
    for(int i=0; i<l; i++){
        std::cout << std::endl;
        for(int x=0; x<w; x++){
            std::cout << m1[i][x] << " ";
        }
    }

    std::cout << "\n\nAfter:";
    for(int i=0; i<w; i++){
        std::cout << std::endl;
        for(int x=0; x<l; x++){
            std::cout << m2[i][x] << " ";
        }
    }

    std::cout << std::endl;

    return 0;
}
```

{{out}}

```txt

Before:
1 2 3
4 5 6
7 8 9
10 11 12
13 14 15

After:
1 4 7 10 13
2 5 8 11 14
3 6 9 12 15

```


## C#

```c#
using System;
using System.Text;

namespace prog
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			double[,] m = { {1,2,3},{4,5,6},{7,8,9} };

			double[,] t = Transpose( m );

			for( int i=0; i<t.GetLength(0); i++ )
			{
				for( int j=0; j<t.GetLength(1); j++ )
					Console.Write( t[i,j] + "  " );
				Console.WriteLine("");
			}
		}

		public static double[,] Transpose( double[,] m )
		{
			double[,] t = new double[m.GetLength(1),m.GetLength(0)];
			for( int i=0; i<m.GetLength(0); i++ )
				for( int j=0; j<m.GetLength(1); j++ )
					t[j,i] = m[i,j];

			return t;
		}
	}
}
```



## Clojure


```lisp
(defmulti matrix-transpose
  "Switch rows with columns."
  class)

(defmethod matrix-transpose clojure.lang.PersistentList
  [mtx]
  (apply map list mtx))

(defmethod matrix-transpose clojure.lang.PersistentVector
  [mtx]
  (apply mapv vector mtx))

```

{{out}}

```txt
=> (matrix-transpose [[1 2 3] [4 5 6]])
[[1 4] [2 5] [3 6]]
```



## CoffeeScript


```coffeescript
transpose = (matrix) ->
    (t[i] for t in matrix) for i in [0...matrix[0].length]
```

{{out}}

```txt

> transpose [[1,2,3],[4,5,6]]

[[1,4],[2,5],[3,6]]

```



## Common Lisp

If the matrix is given as a list:

```lisp
(defun transpose (m)
  (apply #'mapcar #'list m))
```


If the matrix A is given as a 2D array:

```lisp
;; Transpose a mxn matrix A to a nxm matrix B=A'.
(defun mtp (A)
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (B (make-array `(,n ,m) :initial-element 0)))
    (loop for i from 0 below m do
          (loop for j from 0 below n do
                (setf (aref B j i)
                      (aref A i j))))
    B))
```



## D


### Standard Version


```d
void main() {
    import std.stdio, std.range;

    /*immutable*/ auto M = [[10, 11, 12, 13],
                            [14, 15, 16, 17],
                            [18, 19, 20, 21]];
    writefln("%(%(%2d %)\n%)", M.transposed);
}
```

{{out}}

```txt
10 14 18
11 15 19
12 16 20
13 17 21
```



### Locally Procedural Style


```d
T[][] transpose(T)(in T[][] m) pure nothrow {
    auto r = new typeof(return)(m[0].length, m.length);
    foreach (immutable nr, const row; m)
        foreach (immutable nc, immutable c; row)
            r[nc][nr] = c;
    return r;
}

void main() {
    import std.stdio;

    immutable M = [[10, 11, 12, 13],
                   [14, 15, 16, 17],
                   [18, 19, 20, 21]];
    writefln("%(%(%2d %)\n%)", M.transpose);
}
```

Same output.


### Functional Style


```d
import std.stdio, std.algorithm, std.range, std.functional;

auto transpose(T)(in T[][] m) pure nothrow {
    return m[0].length.iota.map!(curry!(transversal, m));
}

void main() {
    immutable M = [[10, 11, 12, 13],
                   [14, 15, 16, 17],
                   [18, 19, 20, 21]];
    writefln("%(%(%2d %)\n%)", M.transpose);
}
```

Same output.


## EchoLisp


```scheme

(lib 'matrix)

(define M (list->array (iota 6) 3 2))
(array-print M)
  0   1
  2   3
  4   5
(array-print (matrix-transpose M))
  0   2   4
  1   3   5

```



## Elixir


```elixir
m = [[1,  1,  1,   1],
     [2,  4,  8,  16],
     [3,  9, 27,  81],
     [4, 16, 64, 256],
     [5, 25,125, 625]]

transpose = fn(m)-> List.zip(m) |> Enum.map(&Tuple.to_list(&1)) end

IO.inspect transpose.(m)
```


{{out}}

```txt

[[1, 2, 3, 4, 5], [1, 4, 9, 16, 25], [1, 8, 27, 64, 125], [1, 16, 81, 256, 625]]

```



## Emacs Lisp


```lisp

(defun transpose (m)
  (apply #'mapcar* #'list m))

;;test for transposition function
(transpose '((2 3 4 5) (3 5 6 9) (9 9 9 9)))

```


{{out}}

```txt

((2 3 9)
 (3 5 9)
 (4 6 9)
 (5 9 9))

```



## Erlang


A nice introduction http://langintro.com/erlang/article2/ which is much more explicit.


```erlang

-module(transmatrix).
-export([trans/1,transL/1]).

% using built-ins hd = head, tl = tail

trans([[]|_]) -> [];
trans(M) ->
  [ lists:map(fun hd/1, M) | transpose( lists:map(fun tl/1, M) ) ].

% Purist version

transL( [ [Elem | Rest] | List] ) ->
    [ [Elem | [H || [H | _] <- List] ] |
      transL( [Rest |
                      [ T || [_ | T] <- List ] ]
       ) ];
transL([ [] | List] ) -> transL(List);
transL([]) -> [].

```


{{out}}

```txt


2> transmatrix:transL( [ [1,2,3],[4,5,6],[7,8,9] ] ).
[[1,4,7],[2,5,8],[3,6,9]]

3> transmatrix:trans( [ [1,2,3],[4,5,6],[7,8,9] ] ).
[[1,4,7],[2,5,8],[3,6,9]]

```



## ELLA

Sample originally from ftp://ftp.dra.hmg.gb/pub/ella (a now dead link) - Public release.

Code for matrix transpose hardware design verification:
```ella
MAC TRANSPOSE = ([INT n][INT m]TYPE t: matrix) -> [m][n]t:
  [INT i = 1..m] [INT j = 1..n] matrix[j][i].
```



## Euphoria


```euphoria
function transpose(sequence in)
    sequence out
    out = repeat(repeat(0,length(in)),length(in[1]))
    for n = 1 to length(in) do
        for m = 1 to length(in[1]) do
            out[m][n] = in[n][m]
        end for
    end for
    return out
end function

sequence m
m = {
  {1,2,3,4},
  {5,6,7,8},
  {9,10,11,12}
}

? transpose(m)
```


{{out}}

```txt
 {
   {1,5,9},
   {2,6,10},
   {3,7,11},
   {4,8,12}
 }
```



## Factor

<code>flip</code> can be used.

```factor
( scratchpad ) { { 1 2 3 } { 4 5 6 } } flip .
 { { 1 4 } { 2 5 } { 3 6 } }
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Matrix_transposition this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

{{libheader|Forth Scientific Library}}
{{works with|gforth|0.7.9_20170308}}

```forth
S" fsl-util.fs" REQUIRED
S" fsl/dynmem.seq" REQUIRED
: F+! ( addr -- ) ( F: r -- )  DUP F@ F+ F! ;
: FSQR ( F: r1 -- r2 ) FDUP F* ;
S" fsl/gaussj.seq" REQUIRED

5 3 float matrix a{{
1e 2e 3e  4e 5e 6e  7e 8e 9e  10e 11e 12e  13e 14e 15e  5 3 a{{ }}fput
float dmatrix b{{

a{{ 5 3 & b{{ transpose
3 5 b{{ }}fprint
```



## Fortran

In ISO Fortran 90 or later, use the TRANSPOSE intrinsic function:

```fortran
integer, parameter   :: n = 3, m = 5
real, dimension(n,m) :: a = reshape( (/ (i,i=1,n*m) /), (/ n, m /) )
real, dimension(m,n) :: b

b = transpose(a)

do i = 1, n
    print *, a(i,:)
end do

do j = 1, m
    print *, b(j,:)
end do
```


In ANSI FORTRAN 77 with MIL-STD-1753 extensions or later, use nested structured DO loops:

```fortran
REAL A(3,5), B(5,3)
DATA ((A(I,J),I=1,3),J=1,5) /1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15/

DO I = 1, 3
   DO J = 1, 5
      B(J,I) = A(I,J)
   END DO
END DO
```


In ANSI FORTRAN 66 or later, use nested labeled DO loops:

```fortran
   REAL A(3,5), B(5,3)
   DATA ((A(I,J),I=1,3),J=1,5) /1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15/

   DO 10 I = 1, 3
      DO 20 J = 1, 5
         B(J,I) = A(I,J)
20    CONTINUE
10 CONTINUE
```


Explicit transposition via DO-loops was available from the start. Less obvious is that Fortran uses what is called "column major" order rather than "row major", which is to say that consecutive elements of the array are stored in memory with indices counting down the columns first, not along the rows. The above examples acknowledge this in the DATA statement with the <code>((A(row,col),row=1,3),col=1,5)</code> which could therefore be replaced with just <code>A</code>, however one could use <code>((A(row,col),col=1,5),row=1,3)</code> instead and the DATA values could be arranged so as to appear in the same layout as one expects for a matrix. Consider
```Fortran
      DIMENSION A(3,5),B(5,3),C(5,3)
      EQUIVALENCE (A,C)	!Occupy the same storage.
      DATA A/
     1     1, 2, 3, 4, 5,
     2     6, 7, 8, 9,10,
     3    11,12,13,14,15/	!Supplies values in storage order.

      WRITE (6,*) "Three rows of five values:"
      WRITE (6,1) A	!This shows values in storage order.
      WRITE (6,*) "...written as C(row,column):"
      WRITE (6,2) ((C(I,J),J = 1,3),I = 1,5)
      WRITE (6,*) "... written as A(row,column):"
      WRITE (6,1) ((A(I,J),J = 1,5),I = 1,3)

      WRITE (6,*)
      WRITE (6,*) "B = Transpose(A)"
      DO 10 I = 1,3
        DO 10 J = 1,5
   10     B(J,I) = A(I,J)

      WRITE (6,*) "Five rows of three values:"
      WRITE (6,2) B
      WRITE (6,*) "... written as B(row,column):"
      WRITE (6,2) ((B(I,J),J = 1,3),I = 1,5)

    1 FORMAT (5F6.1)	!Five values per line.
    2 FORMAT (3F6.1)	!Three values per line.
      END
```


Output:

```txt

 Three rows of five values:
   1.0   2.0   3.0   4.0   5.0
   6.0   7.0   8.0   9.0  10.0
  11.0  12.0  13.0  14.0  15.0
 ...written as C(row,column):
   1.0   6.0  11.0
   2.0   7.0  12.0
   3.0   8.0  13.0
   4.0   9.0  14.0
   5.0  10.0  15.0
 ... written as A(row,column):
   1.0   4.0   7.0  10.0  13.0
   2.0   5.0   8.0  11.0  14.0
   3.0   6.0   9.0  12.0  15.0

 B = Transpose(A)
 Five rows of three values:
   1.0   4.0   7.0
  10.0  13.0   2.0
   5.0   8.0  11.0
  14.0   3.0   6.0
   9.0  12.0  15.0
 ... written as B(row,column):
   1.0   2.0   3.0
   4.0   5.0   6.0
   7.0   8.0   9.0
  10.0  11.0  12.0
  13.0  14.0  15.0
```

Thus, the first output of A replicates the layout of the DATA statement, and the output of matrix C gives its transpose. ''But'', the values in matrix A do ''not'' appear where they would be expected to appear in terms of (row,column) as applied to the layout of the DATA statement. Only ''after'' the transposition is this so. Put another way, the ordering of array values for statements just naming the matrix (the DATA statement, and the simple write statements of A and B) is the transpose of the (row,column) expectation for a matrix. All input and output statements for matrices should thus explicitly specify the index order, even for temporary debugging, lest confusion ensue.

=={{header|F_Sharp|F#}}==
Very straightforward solution...

```fsharp
let transpose (mtx : _ [,]) = Array2D.init (mtx.GetLength 1) (mtx.GetLength 0) (fun x y -> mtx.[y,x])
```



## GAP


```gap
originalMatrix := [[1, 1, 1, 1],
                   [2, 4, 8, 16],
                   [3, 9, 27, 81],
                   [4, 16, 64, 256],
                   [5, 25, 125, 625]];
transposedMatrix := TransposedMat(originalMatrix);
```



## Go


### Library gonum/mat


```go
package main

import (
    "fmt"

    "gonum.org/v1/gonum/mat"
)

func main() {
    m := mat.NewDense(2, 3, []float64{
        1, 2, 3,
        4, 5, 6,
    })
    fmt.Println(mat.Formatted(m))
    fmt.Println()
    fmt.Println(mat.Formatted(m.T()))
}
```

{{out}}

```txt

⎡1  2  3⎤
⎣4  5  6⎦

⎡1  4⎤
⎢2  5⎥
⎣3  6⎦

```



### Library go.matrix


```go
package main

import (
    "fmt"

    mat "github.com/skelterjohn/go.matrix"
)

func main() {
    m := mat.MakeDenseMatrixStacked([][]float64{
        {1, 2, 3},
        {4, 5, 6},
    })
    fmt.Println("original:")
    fmt.Println(m)
    m = m.Transpose()
    fmt.Println("transpose:")
    fmt.Println(m)
}
```

{{out}}

```txt

original:
{1, 2, 3,
 4, 5, 6}
transpose:
{1, 4,
 2, 5,
 3, 6}

```



### 2D representation

Go arrays and slices are only one-dimensional.  The obvious way to represent two-dimensional arrays is with a slice of slices:

```go
package main

import "fmt"

type row []float64
type matrix []row

func main() {
    m := matrix{
        {1, 2, 3},
        {4, 5, 6},
    }
    printMatrix(m)
    t := transpose(m)
    printMatrix(t)
}

func printMatrix(m matrix) {
    for _, s := range m {
        fmt.Println(s)
    }
}

func transpose(m matrix) matrix {
    r := make(matrix, len(m[0]))
    for x, _ := range r {
        r[x] = make(row, len(m))
    }
    for y, s := range m {
        for x, e := range s {
            r[x][y] = e
        }
    }
    return r
}
```

{{out}}

```txt
[1 2 3]
[4 5 6]
[1 4]
[2 5]
[3 6]
```


### Flat representation

Slices of slices turn out to have disadvantages.  It is possible to construct ill-formed matricies with a different number of elements on different rows, for example.  They require multiple allocations, and the compiler must generate complex address calculations to index elements.

A flat element representation with a stride is almost always better.

```go
package main

import "fmt"

type matrix struct {
    ele    []float64
    stride int
}

// construct new matrix from slice of slices
func matrixFromRows(rows [][]float64) *matrix {
    if len(rows) == 0 {
        return &matrix{nil, 0}
    }
    m := &matrix{make([]float64, len(rows)*len(rows[0])), len(rows[0])}
    for rx, row := range rows {
        copy(m.ele[rx*m.stride:(rx+1)*m.stride], row)
    }
    return m
}

func main() {
    m := matrixFromRows([][]float64{
        {1, 2, 3},
        {4, 5, 6},
    })
    m.print("original:")
    m.transpose().print("transpose:")
}

func (m *matrix) print(heading string) {
    if heading > "" {
        fmt.Print("\n", heading, "\n")
    }
    for e := 0; e < len(m.ele); e += m.stride {
        fmt.Println(m.ele[e : e+m.stride])
    }
}

func (m *matrix) transpose() *matrix {
    r := &matrix{make([]float64, len(m.ele)), len(m.ele) / m.stride}
    rx := 0
    for _, e := range m.ele {
        r.ele[rx] = e
        rx += r.stride
        if rx >= len(r.ele) {
            rx -= len(r.ele) - 1
        }
    }
    return r
}
```

{{out}}

```txt

original:
[1 2 3]
[4 5 6]

transpose:
[1 4]
[2 5]
[3 6]

```


### Transpose in place

{{trans|C}}
Note representation is "flat," as above, but without the fluff of constructing from rows.

```go
package main

import "fmt"

type matrix struct {
    stride int
    ele    []float64
}

func main() {
    m := matrix{3, []float64{
        1, 2, 3,
        4, 5, 6,
    }}
    m.print("original:")
    m.transposeInPlace()
    m.print("transpose:")
}

func (m *matrix) print(heading string) {
    if heading > "" {
        fmt.Print("\n", heading, "\n")
    }
    for e := 0; e < len(m.ele); e += m.stride {
        fmt.Println(m.ele[e : e+m.stride])
    }
}

func (m *matrix) transposeInPlace() {
    h := len(m.ele) / m.stride
    for start := range m.ele {
        next := start
        i := 0
        for {
            i++
            next = (next%h)*m.stride + next/h
            if next <= start {
                break
            }
        }
        if next < start || i == 1 {
            continue
        }

        next = start
        tmp := m.ele[next]
        for {
            i = (next%h)*m.stride + next/h
            if i == start {
                m.ele[next] = tmp
            } else {
                m.ele[next] = m.ele[i]
            }
            next = i
            if next <= start {
                break
            }
        }
    }
    m.stride = h
}
```

Output same as above.


## Groovy

The Groovy extensions to the List class provides a transpose method:

```groovy
def matrix = [ [ 1, 2, 3, 4 ],
               [ 5, 6, 7, 8 ] ]

matrix.each { println it }
println()
def transpose = matrix.transpose()

transpose.each { println it }
```


{{out}}

```txt
[1, 2, 3, 4]
[5, 6, 7, 8]

[1, 5]
[2, 6]
[3, 7]
[4, 8]
```



## Haskell

For matrices represented as lists, there's ''transpose'':

```haskell
*Main> transpose [[1,2],[3,4],[5,6]]
[[1,3,5],[2,4,6]]
```


For matrices in arrays, one can use ''ixmap'':

```haskell
import Data.Array

swap (x,y) = (y,x)

transpArray :: (Ix a, Ix b) => Array (a,b) e -> Array (b,a) e
transpArray a = ixmap (swap l, swap u) swap a where
  (l,u) = bounds a
```



## Haxe


```haxe
class Matrix {
    static function main() {
        var m = [ [1,  1,   1,   1],
                  [2,  4,   8,  16],
                  [3,  9,  27,  81],
                  [4, 16,  64, 256],
                  [5, 25, 125, 625] ];
        var t = [ for (i in 0...m[0].length)
                      [ for (j in 0...m.length) 0 ] ];
        for(i in 0...m.length)
            for(j in 0...m[0].length)
                t[j][i] = m[i][j];

        for(aa in [m, t])
            for(a in aa) Sys.println(a);
    }
}
```

{{Out}}

```txt
[1,1,1,1]
[2,4,8,16]
[3,9,27,81]
[4,16,64,256]
[5,25,125,625]
[1,2,3,4,5]
[1,4,9,16,25]
[1,8,27,64,125]
[1,16,81,256,625]
```



## Hope


```hope
uses lists;
dec transpose : list (list alpha) -> list (list alpha);
--- transpose ([]::_) <= [];
--- transpose n <= map head n :: transpose (map tail n);
```



## HicEst


```hicest
REAL :: mtx(2, 4)

mtx = 1.1 * $
WRITE() mtx

SOLVE(Matrix=mtx, Transpose=mtx)
WRITE() mtx
```

{{out}}

```txt
1.1 2.2 3.3 4.4
5.5 6.6 7.7 8.8

1.1 5.5
2.2 6.6
3.3 7.7
4.4 8.8
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure transpose_matrix (matrix)
  result := []
  # for each column
  every (i := 1 to *matrix[1]) do {
    col := []
    # extract the number in each row for that column
    every (row := !matrix) do put (col, row[i])
    # and push that column as a row in the result matrix
    put (result, col)
  }
  return result
end

procedure print_matrix (matrix)
  every (row := !matrix) do {
    every writes (!row || " ")
    write ()
  }
end

procedure main ()
  matrix := [[1,2,3],[4,5,6]]
  write ("Start:")
  print_matrix (matrix)
  transposed := transpose_matrix (matrix)
  write ("Transposed:")
  print_matrix (transposed)
end
```

{{out}}

```txt

Start:
1 2 3
4 5 6
Transposed:
1 4
2 5
3 6

```



## IDL

Standard IDL function <tt>transpose()</tt>

```idl
m=[[1,1,1,1],[2, 4, 8, 16],[3, 9,27, 81],[5, 25,125, 625]]
print,transpose(m)
```



## Idris


```idris>Idris
 transpose [[1,2],[3,4],[5,6]]
[[1, 3, 5], [2, 4, 6]] : List (List Integer)
```



## J

'''Solution:'''

Transpose is the monadic primary verb <code>|:</code>

'''Example:'''

```j
   ]matrix=: (^/ }:) >:i.5    NB. make and show example matrix
1  1   1   1
2  4   8  16
3  9  27  81
4 16  64 256
5 25 125 625
   |: matrix
1  2  3   4   5
1  4  9  16  25
1  8 27  64 125
1 16 81 256 625
```


As an aside, note that <code>.</code> and <code>:</code> are token forming suffixes (if they immediately follow a token forming character, they are a part of the token). This usage is in analogy to the use of [[wp:Diacritic|diacritics]] in many languages. (If you want to use <code> :</code> or <code>.</code> as tokens by themselves you must precede them with a space - beware though that wiki rendering software may sometimes elide the preceding space in <nowiki><code> .</code></nowiki> contexts.)


## Java


```java
import java.util.Arrays;
public class Transpose{
       public static void main(String[] args){
               double[][] m = {{1, 1, 1, 1},
                               {2, 4, 8, 16},
                               {3, 9, 27, 81},
                               {4, 16, 64, 256},
                               {5, 25, 125, 625}};
               double[][] ans = new double[m[0].length][m.length];
               for(int rows = 0; rows < m.length; rows++){
                       for(int cols = 0; cols < m[0].length; cols++){
                               ans[cols][rows] = m[rows][cols];
                       }
               }
               for(double[] i:ans){//2D arrays are arrays of arrays
                       System.out.println(Arrays.toString(i));
               }
       }
}
```



## JavaScript


### ES5

{{works with|SpiderMonkey}} for the <code>print()</code> function

```javascript
function Matrix(ary) {
    this.mtx = ary
    this.height = ary.length;
    this.width = ary[0].length;
}

Matrix.prototype.toString = function() {
    var s = []
    for (var i = 0; i < this.mtx.length; i++)
        s.push( this.mtx[i].join(",") );
    return s.join("\n");
}

// returns a new matrix
Matrix.prototype.transpose = function() {
    var transposed = [];
    for (var i = 0; i < this.width; i++) {
        transposed[i] = [];
        for (var j = 0; j < this.height; j++) {
            transposed[i][j] = this.mtx[j][i];
        }
    }
    return new Matrix(transposed);
}

var m = new Matrix([[1,1,1,1],[2,4,8,16],[3,9,27,81],[4,16,64,256],[5,25,125,625]]);
print(m);
print();
print(m.transpose());
```


produces

```txt
1,1,1,1
2,4,8,16
3,9,27,81
4,16,64,256
5,25,125,625

1,2,3,4,5
1,4,9,16,25
1,8,27,64,125
1,16,81,256,625
```



Or, as a functional expression (rather than an imperative procedure):

```javascript

(function () {
    'use strict';

    function transpose(lst) {
        return lst[0].map(function (_, iCol) {
            return lst.map(function (row) {
                return row[iCol];
            })
        });
    }

    return transpose(
        [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]
    );

})();

```


{{Out}}


```txt
[[1, 4, 7, 10], [2, 5, 8, 11], [3, 6, 9, 12]]
```



### ES6



```JavaScript
(() => {
    'use strict';

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map(row => row[iCol]));


    // TEST -----------------------------------------------
    return(
        transpose([
            [1, 2],
            [3, 4],
            [5, 6]
        ])
    );
})();
```

{{Out}}

```JavaScript
[[1, 3, 5], [2, 4, 6]]
```



## Joy

For matrices represented as lists, there's ''transpose'', defined in seqlib like this:

```joy
DEFINE transpose == [ [null] [true] [[null] some] ifte ]
                    [ pop [] ]
                    [ [[first] map] [[rest] map] cleave ]
                    [ cons ]
                    linrec .
```


## jq

{{works with|jq|1.4}}
Recent versions of jq include a more general "transpose" that can be used to transpose jagged matrices.

The following definition of transpose/0 expects its input to be a non-empty array, each element of which should be an array of the same size. The result is an array that represents the transposition of the input.

```jq
def transpose:
  if (.[0] | length) == 0 then []
  else [map(.[0])] + (map(.[1:]) | transpose)
  end ;
```

'''Examples'''
 [[], []] | transpose
 # => []

 [[1], [3]] | transpose
 # => [[1,3]]

 [[1,2], [3,4]] | transpose
 # => [[1,3],[2,4]]


## Jsish

From the Javascript Matrix entries.

First a module, shared by the Transposition, Multiplication and Exponentiation tasks.


```javascript
/* Matrix transposition, multiplication, identity, and exponentiation, in Jsish */
function Matrix(ary) {
    this.mtx = ary;
    this.height = ary.length;
    this.width = ary[0].length;
}

Matrix.prototype.toString = function() {
    var s = [];
    for (var i = 0; i < this.mtx.length; i++) s.push(this.mtx[i].join(","));
    return s.join("\n");
};

// returns a transposed matrix
Matrix.prototype.transpose = function() {
    var transposed = [];
    for (var i = 0; i < this.width; i++) {
        transposed[i] = [];
        for (var j = 0; j < this.height; j++) transposed[i][j] = this.mtx[j][i];
    }
    return new Matrix(transposed);
};

// returns a matrix as the product of two others
Matrix.prototype.mult = function(other) {
    if (this.width != other.height) throw "error: incompatible sizes";

    var result = [];
    for (var i = 0; i < this.height; i++) {
        result[i] = [];
        for (var j = 0; j < other.width; j++) {
            var sum = 0;
            for (var k = 0; k < this.width; k++) sum += this.mtx[i][k] * other.mtx[k][j];
            result[i][j] = sum;
        }
    }
    return new Matrix(result);
};

// IdentityMatrix is a "subclass" of Matrix
function IdentityMatrix(n) {
    this.height = n;
    this.width = n;
    this.mtx = [];
    for (var i = 0; i < n; i++) {
        this.mtx[i] = [];
        for (var j = 0; j < n; j++) this.mtx[i][j] = (i == j ? 1 : 0);
    }
}
IdentityMatrix.prototype = Matrix.prototype;

// the Matrix exponentiation function
Matrix.prototype.exp = function(n) {
    var result = new IdentityMatrix(this.height);
    for (var i = 1; i <= n; i++) result = result.mult(this);
    return result;
};

provide('Matrix', '0.60');
```


Then a unitTest of the transposition.


```javascript
/* Matrix transposition, in Jsish */
require('Matrix');

if (Interp.conf('unitTest')) {
    var m = new Matrix([[1,1,1,1],[2,4,8,16],[3,9,27,81],[4,16,64,256],[5,25,125,625]]);
;    m;
;    m.transpose();
}

/*
=!EXPECTSTART!=
m ==> { height:5, mtx:[ [ 1, 1, 1, 1 ], [ 2, 4, 8, 16 ], [ 3, 9, 27, 81 ], [ 4, 16, 64, 256 ], [ 5, 25, 125, 625 ] ], width:4 }
m.transpose() ==> { height:4, mtx:[ [ 1, 2, 3, 4, 5 ], [ 1, 4, 9, 16, 25 ], [ 1, 8, 27, 64, 125 ], [ 1, 16, 81, 256, 625 ] ], width:5 }
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u matrixTranspose.jsi
[PASS] matrixTranspose.jsi
```



## Julia

The transposition is obtained by quoting the matrix.

```Julia>julia
 [1 2 3 ; 4 5 6]  # a 2x3 matrix
2x3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> [1 2 3 ; 4 5 6]'  # note the quote
3x2 LinearAlgebra.Adjoint{Int64,Array{Int64,2}}:
 1  4
 2  5
 3  6
```


If you do not want change the type, convert the result back to Array{Int64,2}.


## K

Transpose is the monadic verb <code>+</code>

```k
  {x^\:-1_ x}1+!:5
(1 1 1 1.0
 2 4 8 16.0
 3 9 27 81.0
 4 16 64 256.0
 5 25 125 625.0)

  +{x^\:-1_ x}1+!:5
(1 2 3 4 5.0
 1 4 9 16 25.0
 1 8 27 64 125.0
 1 16 81 256 625.0)
```



## Klong

Transpose is the monadic verb <code>+</code>

```k
    [5 5]:^!25
[[0 1 2 3 4]
 [5 6 7 8 9]
 [10 11 12 13 14]
 [15 16 17 18 19]
 [20 21 22 23 24]]

    +[5 5]:^!25
[[0 5 10 15 20]
 [1 6 11 16 21]
 [2 7 12 17 22]
 [3 8 13 18 23]
 [4 9 14 19 24]]
```



## Kotlin


```scala
// version 1.1.3

typealias Vector = DoubleArray
typealias Matrix = Array<Vector>

fun Matrix.transpose(): Matrix {
    val rows = this.size
    val cols = this[0].size
    val trans = Matrix(cols) { Vector(rows) }
    for (i in 0 until cols) {
        for (j in 0 until rows) trans[i][j] = this[j][i]
    }
    return trans
}

fun printMatrix(m: Matrix) {
    for (i in 0 until m.size) println(m[i].contentToString())
}

fun main(args: Array<String>) {
    val m = arrayOf(
        doubleArrayOf( 1.0,  2.0,  3.0),
        doubleArrayOf( 4.0,  5.0,  6.0),
        doubleArrayOf( 7.0,  8.0,  9.0),
        doubleArrayOf(10.0, 11.0, 12.0)
    )
    printMatrix(m.transpose())
}
```


{{out}}

```txt

[1.0, 4.0, 7.0, 10.0]
[2.0, 5.0, 8.0, 11.0]
[3.0, 6.0, 9.0, 12.0]

```



## Lang5


```Lang5
12 iota [3 4] reshape 1 + dup .
1 transpose .
```

{{out}}

```txt
[
  [    1     2     3     4  ]
  [    5     6     7     8  ]
  [    9    10    11    12  ]
][
  [    1     5     9  ]
  [    2     6    10  ]
  [    3     7    11  ]
  [    4     8    12  ]
]
```



## LFE



```lisp

(defun transpose (matrix)
  (transpose matrix '()))

(defun transpose (matrix acc)
  (cond
    ((lists:any
        (lambda (x) (== x '()))
        matrix)
     acc)
    ('true
      (let ((heads (lists:map #'car/1 matrix))
            (tails (lists:map #'cdr/1 matrix)))
        (transpose tails (++ acc `(,heads)))))))

```


Usage in the LFE REPL:


```lisp

> (transpose '((1  2  3)
               (4  5  6)
               (7  8  9)
               (10 11 12)
               (13 14 15)
               (16 17 18)))
((1 4 7 10 13 16) (2 5 8 11 14 17) (3 6 9 12 15 18))
>

```



## Liberty BASIC

There is no native matrix capability. A set of functions is available at http://www.diga.me.uk/RCMatrixFuncs.bas implementing matrices of arbitrary dimension in a string format.

```lb
MatrixC$ ="4, 3,          0, 0.10, 0.20, 0.30,       0.40, 0.50, 0.60, 0.70,      0.80, 0.90, 1.00, 1.10"

print "Transpose of matrix"
call DisplayMatrix MatrixC$
print "         ="
MatrixT$ =MatrixTranspose$( MatrixC$)
call DisplayMatrix MatrixT$
```


{{out}}

```txt
Transpose of matrix
| 0.00000 0.10000 0.20000 0.30000 |
| 0.40000 0.50000 0.60000 0.70000 |
| 0.80000 0.90000 1.00000 1.10000 |

=
| 0.00000 0.40000 0.80000 |
| 0.10000 0.50000 0.90000 |
| 0.20000 0.60000 1.00000 |
| 0.30000 0.70000 1.10000 |
```



## Lua


```lua
function Transpose( m )
    local res = {}

    for i = 1, #m[1] do
        res[i] = {}
        for j = 1, #m do
            res[i][j] = m[j][i]
        end
    end

    return res
end

-- a test for Transpose(m)
mat = { { 1, 2, 3 }, { 4, 5, 6 } }
erg = Transpose( mat )
for i = 1, #erg do
    for j = 1, #erg[1] do
        io.write( erg[i][j] )
        io.write( "  " )
    end
    io.write( "\n" )
end
```


Using apply map list

```lua
function map(f, a)
  local b = {}
  for k,v in ipairs(a) do b[k] = f(v) end
  return b
end

function mapn(f, ...)
  local c = {}
  local k = 1
  local aarg = {...}
  local n = #aarg
  while true do
    local a = map(function(b) return b[k] end, aarg)
    if #a < n then return c end
    c[k] = f(unpack(a))
    k = k + 1
  end
end

function apply(f1, f2, a)
 return f1(f2, unpack(a))
end

xy = {{1,2,3,4},{1,2,3,4},{1,2,3,4}}
yx = apply(mapn, function(...) return {...} end, xy)
print(table.concat(map(function(a) return table.concat(a,",") end, xy), "\n"),"\n")
print(table.concat(map(function(a) return table.concat(a,",") end, yx), "\n"))
```


--Edit: table.getn() deprecated, using # instead


## Maple


The <code>Transpose</code> function in Maple's <code>LinearAlgebra</code> package computes this. The computation can also be accomplished by raising the Matrix to the <code>%T</code> power. Similarly for <code>HermitianTranspose</code> and the <code>%H</code> power.


```Maple

M := <<2,3>|<3,4>|<5,6>>;

M^%T;

with(LinearAlgebra):
Transpose(M);

```

{{out}}

```txt

                                    [2  3  5]
                               M := [       ]
                                    [3  4  6]

                                   [2  3]
                                   [    ]
                                   [3  4]
                                   [    ]
                                   [5  6]

                                   [2  3]
                                   [    ]
                                   [3  4]
                                   [    ]
                                   [5  6]

```



## Mathematica


```mathematica
originalMatrix = {{1, 1, 1, 1},
                  {2, 4, 8, 16},
                  {3, 9, 27, 81},
                  {4, 16, 64, 256},
                  {5, 25, 125, 625}}
transposedMatrix = Transpose[originalMatrix]
```



## MATLAB

Matlab contains two built-in methods of transposing a matrix: by using the <code>transpose()</code> function, or by using the <code>.'</code> operator. The <code>'</code> operator yields the [[conjugate transpose|complex conjugate transpose]].

```Matlab>>
 transpose([1 2;3 4])

ans =

     1     3
     2     4

>> [1 2;3 4].'

ans =

     1     3
     2     4
```


But, you can, obviously, do the transposition of a matrix without a built-in method, in this case, the code can be this hereafter code:

```Matlab


B=size(A);   %In this code, we assume that a previous matrix "A" has already been inputted.
for j=1:B(1)
    for i=1:B(2)
        C(i,j)=A(j,i);
    end      %The transposed A-matrix should be C
end


```


Transposing nested cells using apply map list

```Matlab
xy = {{1,2,3,4},{1,2,3,4},{1,2,3,4}}
yx = feval(@(x) cellfun(@(varargin)[varargin],x{:},'un',0), xy)
```



## Maxima


```maxima
originalMatrix : matrix([1, 1, 1, 1],
                        [2, 4, 8, 16],
                        [3, 9, 27, 81],
                        [4, 16, 64, 256],
                        [5, 25, 125, 625]);
transposedMatrix : transpose(originalMatrix);
```



## MAXScript

Uses the built in transpose() function

```maxscript
m = bigMatrix 5 4
for i in 1 to 5 do for j in 1 to 4 do m[i][j] = pow i j
m = transpose m
```



## Nial

make an array

```nial
|a := 2 3 reshape count 6
=1 2 3
=4 5 6
```

transpose it

```nial
|transpose a
=1 4
=2 5
=3 6
```



## Nim

For statically sized arrays:

```nim
proc transpose[X, Y; T](s: array[Y, array[X, T]]): array[X, array[Y, T]] =
  for i in low(X)..high(X):
    for j in low(Y)..high(Y):
      result[i][j] = s[j][i]

let b = [[ 0, 1, 2, 3, 4],
         [ 5, 6, 7, 8, 9],
         [ 1, 0, 0, 0,42]]
let c = transpose(b)
for r in c:
  for i in r:
    stdout.write i, " "
  echo ""
```

{{out}}

```txt
0 5 1
1 6 0
2 7 0
3 8 0
4 9 42
```

For dynamically sized seqs:

```nim
proc transpose[T](s: seq[seq[T]]): seq[seq[T]] =
  result = newSeq[seq[T]](s[0].len)
  for i in 0 .. < s[0].len:
    result[i] = newSeq[T](s.len)
    for j in 0 .. < s.len:
      result[i][j] = s[j][i]

let a = @[@[ 0, 1, 2, 3, 4],
          @[ 5, 6, 7, 8, 9],
          @[ 1, 0, 0, 0,42]]
echo transpose(a)
```

{{out}}

```txt
@[@[0, 5, 1], @[1, 6, 0], @[2, 7, 0], @[3, 8, 0], @[4, 9, 42]]
```



## Objeck


```objeck

bundle Default {
  class Transpose {
    function : Main(args : String[]) ~ Nil {
      input := [[1, 1, 1, 1]
        [2, 4, 8, 16]
        [3, 9, 27, 81]
        [4, 16, 64, 256]
        [5, 25, 125, 625]];
      dim := input->Size();

      output := Int->New[dim[0],dim[1]];
      for(i := 0; i < dim[0]; i+=1;) {
        for(j := 0; j < dim[1]; j+=1;) {
          output[i,j] := input[i,j];
        };
      };

      Print(output);
    }

    function : Print(matrix : Int[,]) ~ Nil {
      dim := matrix->Size();
      for(i := 0; i < dim[0]; i+=1;) {
        for(j := 0; j < dim[1]; j+=1;) {
          IO.Console->Print(matrix[i,j])->Print('\t');
        };
        '\n'->Print();
      };
    }
  }
}

```


{{out}}

```txt

1	2	3	4	5
1	4	9	16	25
1	8	27	64	125
1	16	81	256	625

```



## OCaml

Matrices can be represented in OCaml as a type <tt>'a array array</tt>, or using the module [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.html Bigarray].
The implementation below uses a bigarray:


```ocaml
open Bigarray

let transpose b =
  let dim1 = Array2.dim1 b
  and dim2 = Array2.dim2 b in
  let kind = Array2.kind b
  and layout = Array2.layout b in
  let b' = Array2.create kind layout dim2 dim1 in
  for i=0 to pred dim1 do
    for j=0 to pred dim2 do
      b'.{j,i} <- b.{i,j}
    done;
  done;
  (b')
;;

let array2_display print newline b =
  for i=0 to Array2.dim1 b - 1 do
    for j=0 to Array2.dim2 b - 1 do
      print b.{i,j}
    done;
    newline();
  done;
;;

let a = Array2.of_array int c_layout [|
  [| 1; 2; 3; 4 |];
  [| 5; 6; 7; 8 |];
|]

array2_display (Printf.printf " %d") print_newline (transpose a) ;;
```


{{out}}

```txt

 1 5
 2 6
 3 7
 4 8

```

A version for lists:

```ocaml
let rec transpose m =
  assert (m <> []);
  if List.mem [] m then
    []
  else
    List.map List.hd m :: transpose (List.map List.tl m)
```

Example:
 # transpose [[1;2;3;4];
              [5;6;7;8]];;
 - : int list list = [[1; 5]; [2; 6]; [3; 7]; [4; 8]]


## Octave


```octave
a = [ 1, 1, 1, 1 ;
      2, 4, 8, 16 ;
      3, 9, 27, 81 ;
      4, 16, 64, 256 ;
      5, 25, 125, 625 ];
tranposed = a.'; % tranpose
ctransp = a'; % conjugate transpose
```



## OxygenBasic


```oxygenbasic

function Transpose(double *A,*B, sys nx,ny)
'
### ====================================

  sys x,y
  indexbase 0
  for x=0 to <nx
    for y=0 to <ny
      B[y*nx+x]=A[x*ny+y]
    next
  next
end function

function MatrixShow(double*A, sys nx,ny) as string
'
### ===========================================

  sys x,y
  indexbase 0
  string pr="",tab=chr(9),cr=chr(13)+chr(10)
  for y=0 to <ny
    for x=0 to <nx
      pr+=tab A[x*ny+y]
    next
    pr+=cr
  next
  return pr
end function

'====
'DEMO
'====

double A[5*4],B[4*5]
'columns x
'rows    y

A <= 'y minor, x major
11,12,13,14,15,
21,22,23,24,25,
31,32,33,34,35,
41,42,43,44,45

print MatrixShow A,5,4
Transpose        A,B,5,4
print MatrixShow B,4,5

```



## PARI/GP

The GP function for matrix (or vector) transpose is <code>mattranspose</code>, but it is usually invoked with a tilde:

```parigp
M~
```


In PARI the function is

```C
gtrans(M)
```

though <code>shallowtrans</code> is also available when deep copying is not desired.


## Pascal


```pascal
Program Transpose;

const
  A: array[1..3,1..5] of integer = (( 1,  2,  3,  4,  5),
                                    ( 6,  7,  8,  9, 10),
				    (11, 12, 13, 14, 15)
				   );
var
  B: array[1..5,1..3] of integer;
  i, j: integer;

begin
  for i := low(A) to high(A) do
    for j := low(A[1]) to high(A[1]) do
      B[j,i] := A[i,j];

  writeln ('A:');
  for i := low(A) to high(A) do
  begin
    for j := low(A[1]) to high(A[1]) do
      write (A[i,j]:3);
    writeln;
  end;

  writeln ('B:');
  for i := low(B) to high(B) do
  begin
    for j := low(B[1]) to high(B[1]) do
      write (B[i,j]:3);
    writeln;
  end;
end.
```

{{out}}

```txt
% ./Transpose
A:
  1  2  3  4  5
  6  7  8  9 10
 11 12 13 14 15
B:
  1  6 11
  2  7 12
  3  8 13
  4  9 14
  5 10 15

```



## Perl

{{libheader|Math::Matrix}}

```perl
use Math::Matrix;

$m = Math::Matrix->new(
  [1, 1, 1, 1],
  [2, 4, 8, 16],
  [3, 9, 27, 81],
  [4, 16, 64, 256],
  [5, 25, 125, 625],
);

$m->transpose->print;
```


{{out}}

```txt

 1.00000    2.00000    3.00000    4.00000    5.00000
 1.00000    4.00000    9.00000   16.00000   25.00000
 1.00000    8.00000   27.00000   64.00000  125.00000
 1.00000   16.00000   81.00000  256.00000  625.00000

```

Manually:

```perl
my @m = (
  [1, 1, 1, 1],
  [2, 4, 8, 16],
  [3, 9, 27, 81],
  [4, 16, 64, 256],
  [5, 25, 125, 625],
);

my @transposed;
foreach my $j (0..$#{$m[0]}) {
  push(@transposed, [map $_->[$j], @m]);
}
```



## Perl 6

{{Works with|rakudo|2018.03}}

```perl6
# Transposition can be done with the reduced zip meta-operator
# on list-of-lists data structures

say [Z] (<A B C D>, <E F G H>, <I J K L>);

# For native shaped arrays, a more traditional procedure of copying item-by-item
# Here the resulting matrix is also a native shaped array

my @a[3;4] =
  [
    [<A B C D>],
    [<E F G H>],
    [<I J K L>],
  ];

(my $n, my $m) = @a.shape;
my @b[$m;$n];
for ^$m X ^$n -> (\i, \j) {
   @b[i;j] = @a[j;i];
}

say @b;
```


{{output}}


```txt
((A E I) (B F J) (C G K) (D H L))
[[A E I] [B F J] [C G K] [D H L]]
```



## Phix

Copy of [[Matrix_transposition#Euphoria|Euphoria]]

```Phix
function transpose(sequence in)
sequence out = repeat(repeat(0,length(in)),length(in[1]))
    for n=1 to length(in) do
        for m=1 to length(in[1]) do
            out[m][n] = in[n][m]
        end for
    end for
    return out
end function
```



## PHP


### =Up to PHP version 5.6=


```php

function transpose($m) {
  if (count($m) == 0) // special case: empty matrix
    return array();
  else if (count($m) == 1) // special case: row matrix
    return array_chunk($m[0], 1);

  // array_map(NULL, m[0], m[1], ..)
  array_unshift($m, NULL); // the original matrix is not modified because it was passed by value
  return call_user_func_array('array_map', $m);
}
```




### =Starting with PHP 5.6=


```php


function transpose($m) {
    return count($m) == 0 ? $m : (count($m) == 1 ? array_chunk($m[0], 1) : array_map(null, ...$m));
}

```



## PicoLisp


```PicoLisp
(de matTrans (Mat)
   (apply mapcar Mat list) )

(matTrans '((1 2 3) (4 5 6)))
```

{{out}}

```txt
-> ((1 4) (2 5) (3 6))
```



## PL/I


```PL/I
/* The short method: */
declare A(m, n) float, B (n,m) float defined (A(2sub, 1sub));
/* Any reference to B gives the transpose of matrix A. */
```

Traditional method:

```PL/I
/* Transpose matrix A, result at B. */
transpose: procedure (a, b);
   declare (a, b) (*,*) float controlled;
   declare (m, n) fixed binary;

   if allocation(b) > 0 then free b;

   m = hbound(a,1); n = hbound(a,2);

   allocate b(n,m);

   do i = 1 to m;
      b(*,i) = a(i,*);
   end;
end transpose;
```



## Pop11


```pop11
define transpose(m) -> res;
    lvars bl = boundslist(m);
    if length(bl) /= 4 then
        throw([need_2d_array ^a])
    endif;
    lvars i, i0 = bl(1), i1 = bl(2);
    lvars j, j0 = bl(3), j1 = bl(4);
    newarray([^j0 ^j1 ^i0 ^i1], 0) -> res;
    for i from i0 to i1 do
        for j from j0 to j1 do
            m(i, j) -> res(j, i);
        endfor;
    endfor;
enddefine;
```



## PostScript

{{libheader|initlib}}

```postscript
/transpose {
    [ exch {
        { {empty? exch pop} map all?} {pop exit} ift
        [ exch {} {uncons {exch cons} dip exch} fold counttomark 1 roll] uncons
    } loop ] {reverse} map
}.
```



## PowerBASIC

PowerBASIC has the MAT statement to simplify Matrix Algebra calculations; in conjunction with the TRN operation the actual transposition is just a one-liner.

```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6
'----------------------------------------------------------------------
SUB TransposeMatrix(InitMatrix() AS DWORD, TransposedMatrix() AS DWORD)
LOCAL l1, l2, u1, u2 AS LONG
  l1 = LBOUND(InitMatrix, 1)
  l2 = LBOUND(InitMatrix, 2)
  u1 = UBOUND(InitMatrix, 1)
  u2 = UBOUND(InitMatrix, 2)
  REDIM TransposedMatrix(l2 TO u2, l1 TO u1)
  MAT TransposedMatrix() = TRN(InitMatrix())
END SUB
'----------------------------------------------------------------------
SUB PrintMatrix(a() AS DWORD)
LOCAL l1, l2, u1, u2, r, c AS LONG
LOCAL s AS STRING * 8
  l1 = LBOUND(a(), 1)
  l2 = LBOUND(a(), 2)
  u1 = UBOUND(a(), 1)
  u2 = UBOUND(a(), 2)
  FOR r = l1 TO u1
    FOR c = l2 TO u2
      RSET s = STR$(a(r, c))
      CON.PRINT s;
    NEXT c
  CON.PRINT
  NEXT r
END SUB
'----------------------------------------------------------------------
SUB TranspositionDemo(BYVAL DimSize1 AS DWORD, BYVAL DimSize2 AS DWORD)
LOCAL r, c, cc AS DWORD
LOCAL a() AS DWORD
LOCAL b() AS DWORD
  cc = DimSize2
  DECR DimSize1
  DECR DimSize2
  REDIM a(0 TO DimSize1, 0 TO DimSize2)
  FOR r = 0 TO DimSize1
    FOR c = 0 TO DimSize2
      a(r, c)= (cc * r) + c + 1
    NEXT c
  NEXT r
  CON.PRINT "initial matrix:"
  PrintMatrix a()
  TransposeMatrix a(), b()
  CON.PRINT "transposed matrix:"
  PrintMatrix b()
END SUB
'----------------------------------------------------------------------
FUNCTION PBMAIN () AS LONG
  TranspositionDemo 3, 3
  TranspositionDemo 3, 7
END FUNCTION
```

{{out}}

```txt
initial matrix:
       1       2       3
       4       5       6
       7       8       9
transposed matrix:
       1       4       7
       2       5       8
       3       6       9
initial matrix:
       1       2       3       4       5       6       7
       8       9      10      11      12      13      14
      15      16      17      18      19      20      21
transposed matrix:
       1       8      15
       2       9      16
       3      10      17
       4      11      18
       5      12      19
       6      13      20
       7      14      21
```



## PowerShell


### Any Matrix


```PowerShell

function transpose($a) {
    $arr = @()
    if($a) {
        $n = $a.count - 1
        if(0 -lt $n) {
            $m = ($a | foreach {$_.count} | measure-object -Minimum).Minimum - 1
            if( 0 -le $m) {
                if (0 -lt $m) {
                    $arr =@(0)*($m+1)
                    foreach($i in 0..$m) {
                        $arr[$i] = foreach($j in 0..$n) {@($a[$j][$i])}
                    }
                } else {$arr = foreach($row in $a) {$row[0]}}
            }
        } else {$arr = $a}
    }
    $arr
}
function show($a) {
    if($a) {
        0..($a.Count - 1) | foreach{ if($a[$_]){"$($a[$_])"}else{""} }
    }
}

$a = @(@(2, 0, 7, 8),@(3, 5, 9, 1),@(4, 1, 6, 3))
"`$a ="
show $a
""
"transpose `$a ="
show (transpose $a)
""
$a = @(1)
"`$a ="
show $a
""
"transpose `$a ="
show (transpose $a)
""
"`$a ="
$a = @(1,2,3)
show $a
""
"transpose `$a ="
"$(transpose $a)"
""
"`$a ="
$a = @(@(4,7,8),@(1),@(2,3))
show $a
""
"transpose `$a ="
"$(transpose $a)"
""
"`$a ="
$a = @(@(4,7,8),@(1,5,9,0),@(2,3))
show $a
""
"transpose `$a ="
show (transpose $a)

```

<b>Output:</b>

```txt

$a =
2 0 7 8
3 5 9 1
4 1 6 3

transpose $a =
2 3 4
0 5 1
7 9 6
8 1 3

$a =
1

transpose $a =
1

$a =
1
2
3

transpose $a =
1 2 3

$a =
4 7 8
1
2 3

transpose $a =
4 1 2

$a =
4 7 8
1 5 9 0
2 3

transpose $a =
4 1 2
7 5 3

```



### Square Matrix


```PowerShell

function transpose($a) {
    if($a) {
        $n = $a.Count - 1
        foreach($i in 0..$n) {
            $j = 0
            while($j -lt $i) {
                $a[$i][$j], $a[$j][$i] = $a[$j][$i], $a[$i][$j]
                $j++
            }
        }
    }
    $a
}
function show($a) {
    if($a) {
        0..($a.Count - 1) | foreach{ if($a[$_]){"$($a[$_])"}else{""} }
    }
}
$a = @(@(2, 4, 7),@(3, 5, 9),@(4, 1, 6))
show $a
""
show (transpose $a)

```

<b>Output:</b>

```txt

2 4 7
3 5 9
4 1 6

2 3 4
4 5 1
7 9 6

```



## Prolog

Predicate transpose/2 exists in libray clpfd of SWI-Prolog.<BR>
In Prolog, a matrix is a list of lists. transpose/2 can be written like that.
{{works with|SWI-Prolog}}

```Prolog
% transposition of a rectangular matrix
% e.g.   [[1,2,3,4], [5,6,7,8]]
% give [[1,5],[2,6],[3,7],[4,8]]

transpose(In, Out) :-
    In = [H | T],
    maplist(initdl, H, L),
    work(T, In, Out).

% we use the difference list to make "quick" appends (one inference)
initdl(V, [V | X] - X).

work(Lst, [H], Out) :-
	maplist(my_append_last, Lst, H, Out).

work(Lst, [H | T], Out) :-
    maplist(my_append, Lst, H, Lst1),
    work(Lst1, T, Out).

my_append(X-Y, C, X1-Y1) :-
    append_dl(X-Y, [C | U]- U, X1-Y1).

my_append_last(X-Y, C, X1) :-
	append_dl(X-Y, [C | U]- U, X1-[]).

% "quick" append
append_dl(X-Y, Y-Z, X-Z).
```



## PureBasic

Matrices represented by integer arrays using rows as the first dimension and columns as the second dimension.

```PureBasic
Procedure transposeMatrix(Array a(2), Array trans(2))
  Protected rows, cols

  Protected ar = ArraySize(a(), 1) ;rows in original matrix
  Protected ac = ArraySize(a(), 2) ;cols in original matrix

  ;size the matrix receiving the transposition
  Dim trans(ac, ar)

  ;copy the values
  For rows = 0 To ar
    For cols = 0 To ac
      trans(cols, rows) = a(rows, cols)
    Next
  Next
EndProcedure

Procedure displayMatrix(Array a(2), text.s = "")
  Protected i, j
  Protected cols = ArraySize(a(), 2), rows = ArraySize(a(), 1)

  PrintN(text + ": (" + Str(rows + 1) + ", " + Str(cols + 1) + ")")
  For i = 0 To rows
    For j = 0 To cols
      Print(LSet(Str(a(i, j)), 4, " "))
    Next
    PrintN("")
  Next
  PrintN("")
EndProcedure

;setup a matrix of arbitrary size
Dim m(random(5), random(5))

Define rows, cols
;fill matrix with 'random' data
For rows = 0 To ArraySize(m(),1)      ;ArraySize() can take a dimension as its second argument
  For cols = 0 To ArraySize(m(), 2)
    m(rows, cols) = random(10) - 10
  Next
Next

Dim t(0,0) ;this will be resized during transposition
If OpenConsole()
  displayMatrix(m(), "matrix before transposition")
  transposeMatrix(m(), t())
  displayMatrix(t(), "matrix after transposition")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
matrix m, before: (3, 4)
-4  -9  -7  -9
-3  -6  -4  -6
-1  -2  0   -6

matrix m after transposition: (4, 3)
-4  -3  -1
-9  -6  -2
-7  -4  0
-9  -6  -6
```



## Python


```python
m=((1,  1,  1,   1),
   (2,  4,  8,  16),
   (3,  9, 27,  81),
   (4, 16, 64, 256),
   (5, 25,125, 625))
print(zip(*m))
# in Python 3.x, you would do:
# print(list(zip(*m)))
```

{{out}}

```txt

 [(1, 2, 3, 4, 5),
  (1, 4, 9, 16, 25),
  (1, 8, 27, 64, 125),
  (1, 16, 81, 256, 625)]

```


Note, however, that '''zip''', while very useful, doesn't give us a simple type-safe transposition – it is actually a ''''transpose + coerce'''' function rather than a pure '''transpose''' function; polymorphic in its inputs, but not in its outputs.

zip accepts matrices in any of the 4 permutations of (outer lists or tuples) * (inner lists or tuples), but it always and only returns a '''zip''' of '''tuples''', losing any information about what the input type was.

For type-specific transpositions '''without''' coercion (and for a richer set of matrix types, and higher level of efficiency – transpositions are an inherently expensive operation) we can turn to '''numpy'''.

Meanwhile, for the four basic types of Python matrices (the cartesian product of (inner type, container type) * (tuple, list), the simplest (though not necessarily most efficient) approach (in the absence of numpy) may be to write a type-sensitive wrapper, which retains and restores the type information that zip discards.

Perhaps, for example, something like:

```python
# transpose :: Matrix a -> Matrix a
def transpose(m):
    if m:
        inner = type(m[0])
        z = zip(*m)
        return (type(m))(
            map(inner, z) if tuple != inner else z
        )
    else:
        return m


if __name__ == '__main__':

    # TRANSPOSING FOUR BASIC TYPES OF PYTHON MATRIX
    # Cartesian product of (Outer, Inner) with (List, Tuple)

    # Matrix any = Tuple of Tuples of any type
    tts = ((1, 2, 3), (4, 5, 6), (7, 8, 9))

    # Matrix any = Tuple of Lists of any  type
    tls = ([1, 2, 3], [4, 5, 6], [7, 8, 9])

    emptyTuple = ()

    # Matrix any = List of Lists of any type
    lls = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    # Matrix any = List of Tuples of any type
    lts = [(1, 2, 3), (4, 5, 6), (7, 8, 9)]

    emptyList = []

    print('transpose function :: (Transposition without type change):\n')
    for m in [emptyTuple, tts, tls, emptyList, lls, lts]:
        tm = transpose(m)
        print (
            type(tm).__name__ + (
                (' of ' + type(tm[0]).__name__) if m else ''
            ) + ' :: ' + str(m) + ' -> ' + str(tm)
        )
```

{{Out}}

```txt
transpose function :: (Transposition without type change):

tuple :: () -> ()
tuple of tuple :: ((1, 2, 3), (4, 5, 6), (7, 8, 9)) -> ((1, 4, 7), (2, 5, 8), (3, 6, 9))
tuple of list :: ([1, 2, 3], [4, 5, 6], [7, 8, 9]) -> ([1, 4, 7], [2, 5, 8], [3, 6, 9])
list :: [] -> []
list of list :: [[1, 2, 3], [4, 5, 6], [7, 8, 9]] -> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
list of tuple :: [(1, 2, 3), (4, 5, 6), (7, 8, 9)] -> [(1, 4, 7), (2, 5, 8), (3, 6, 9)]
```



Even with its type amnesia fixed, '''zip''' may still not be the instrument to reach for when it's possible that our matrices may contain gaps.

If any of the rows in a '''list of lists''' matrix are not wide enough for a full set of data for one or more of the columns, then '''zip(*xs)''' will drop all the data entirely, without warning or error message, returning no more than an empty list:


```python
# Uneven list of lists
uls = [[10, 11], [20], [], [30, 31, 32]]

print (
    list(zip(*uls))
)

#  --> []
```


At this point, short of turning to '''numpy''', we might need to write a custom function.
An obvious approach is to return the full number of potential columns, each containing such data as the rows do have.
For example:

{{Works with|Python|3.7}}

```python
'''Transposition of row sets with possible gaps'''

from collections import defaultdict


# listTranspose :: [[a]] -> [[a]]
def listTranspose(xss):
    '''Transposition of a matrix which may
       contain gaps.
    '''
    def go(xss):
        if xss:
            h, *t = xss
            return (
                [[h[0]] + [xs[0] for xs in t if xs]] + (
                    go([h[1:]] + [xs[1:] for xs in t])
                )
            ) if h and isinstance(h, list) else go(t)
        else:
            return []
    return go(xss)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests with various lists of rows or non-row data.'''

    def labelledList(kxs):
        k, xs = kxs
        return k + ': ' + showList(xs)

    print(
        fTable(
            __doc__ + ':\n'
        )(labelledList)(fmapFn(showList)(snd))(
            fmapTuple(listTranspose)
        )([
            ('Square', [[1, 2, 3], [4, 5, 6], [7, 8, 9]]),
            ('Rectangle', [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]]),
            ('Rows with gaps', [[10, 11], [20], [], [31, 32, 33]]),
            ('Single row', [[1, 2, 3]]),
            ('Single row, one cell', [[1]]),
            ('Not rows', [1, 2, 3]),
            ('Nothing', [])
        ])
    )


# TEST RESULT FORMATTING ----------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# fmapFn :: (a -> b) -> (r -> a) -> r -> b
def fmapFn(f):
    '''The application of f to the result of g.
       fmap over a function is composition.
    '''
    return lambda g: lambda x: f(g(x))


# fmapTuple :: (a -> b) -> (c, a) -> (c, b)
def fmapTuple(f):
    '''A pair in which f has been
       applied to the second item.
    '''
    return lambda ab: (ab[0], f(ab[1])) if (
        2 == len(ab)
    ) else None


# show :: a -> String
def show(x):
    '''Stringification of a value.'''
    def go(v):
        return defaultdict(lambda: repr, [
            ('list', showList)
            # ('Either', showLR),
            # ('Maybe', showMaybe),
            # ('Tree', drawTree)
        ])[
            typeName(v)
        ](v)
    return go(x)


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(show(x) for x in xs) + ']'


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# typeName :: a -> String
def typeName(x):
    '''Name string for a built-in or user-defined type.
       Selector for type-specific instances
       of polymorphic functions.
    '''
    if isinstance(x, dict):
        return x.get('type') if 'type' in x else 'dict'
    else:
        return 'iter' if hasattr(x, '__next__') else (
            type(x).__name__
        )

# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Transposition of row sets with possible gaps:

              Square: [[1,2,3],[4,5,6],[7,8,9]] -> [[1,4,7],[2,5,8],[3,6,9]]
Rectangle: [[1,2,3],[4,5,6],[7,8,9],[10,11,12]] -> [[1,4,7,10],[2,5,8,11],[3,6,9,12]]
   Rows with gaps: [[10,11],[20],[],[31,32,33]] -> [[10,20,31],[11,32],[33]]
                          Single row: [[1,2,3]] -> [[1],[2],[3]]
                    Single row, one cell: [[1]] -> [[1]]
                              Not rows: [1,2,3] -> []
                                    Nothing: [] -> []
```



## R


```R
b <- 1:5
m <- matrix(c(b, b^2, b^3, b^4), 5, 4)
print(m)
tm <- t(m)
print(tm)
```



## Racket


```racket

#lang racket
(require math)
(matrix-transpose (matrix [[1 2] [3 4]]))

```

{{out}}

```txt

(array #[#[1 3] #[2 4]])

```


(Another method, without math, and using lists is demonstrated in the Scheme solution.)


## Rascal


```Rascal
public rel[real, real, real] matrixTranspose(rel[real x, real y, real v] matrix){
    return {<y, x, v> | <x, y, v> <- matrix};
}

//a matrix
public rel[real x, real y, real v] matrixA = {
<0.0,0.0,12.0>, <0.0,1.0, 6.0>, <0.0,2.0,-4.0>,
<1.0,0.0,-51.0>, <1.0,1.0,167.0>, <1.0,2.0,24.0>,
<2.0,0.0,4.0>, <2.0,1.0,-68.0>, <2.0,2.0,-41.0>
};
```



## REXX


```rexx
/*REXX program transposes any sized rectangular matrix, displays before & after matrices*/
@.=;     @.1 =   1.02     2.03      3.04       4.05        5.06         6.07          7.08
         @.2 = 111     2222     33333     444444     5555555     66666666     777777777
w=0
                             do    row=1  while @.row\==''
                                do col=1  until @.row==''; parse var @.row A.row.col @.row
                                w=max(w, length(A.row.col) )    /*max width for elements*/
                                end   /*col*/                   /*(used to align ouput).*/
                             end      /*row*/    /* [↑]  build matrix A from the @ lists*/
row= row-1                                       /*adjust for  DO  loop index increment.*/
                             do    j=1  for row  /*process each    row    of the matrix.*/
                                do k=1  for col  /*   "      "    column   "  "     "   */
                                B.k.j= A.j.k     /*transpose the  A  matrix  (into  B). */
                                end   /*k*/
                             end      /*j*/
call showMat  'A', row, col                      /*display the   A   matrix to terminal.*/
call showMat  'B', col, row                      /*   "     "    B      "    "     "    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat: arg mat,rows,cols;     say;       say center( mat  'matrix',  (w+1)*cols +4, "─")
                 do      r=1  for rows;    _=                                  /*newLine*/
                      do c=1  for cols;    _=_ right( value( mat'.'r"."c), w)  /*append.*/
                      end   /*c*/
                 say _                                                         /*1 line.*/
                 end        /*r*/;         return
```

{{out|output|text=  when using the default input:}}

```txt

─────────────────────────────────A matrix─────────────────────────────────
      1.02      2.03      3.04      4.05      5.06      6.07      7.08
       111      2222     33333    444444   5555555  66666666 777777777

────────B matrix────────
      1.02       111
      2.03      2222
      3.04     33333
      4.05    444444
      5.06   5555555
      6.07  66666666
      7.08 777777777

```



## Ring


```ring

load "stdlib.ring"
transpose = newlist(5,4)
matrix = [[78,19,30,12,36], [49,10,65,42,50], [30,93,24,78,10], [39,68,27,64,29]]
for i = 1 to 5
    for j = 1 to 4
        transpose[i][j] = matrix[j][i]
        see "" + transpose[i][j] + " "
    next
    see nl
next

```

Output:

```txt

78 49 30 39
19 10 93 68
30 65 24 27
12 42 78 64
36 50 10 29

```



## RLaB


```RLaB> >
 m = rand(3,5)
  0.41844289   0.476591435    0.75054022   0.226388925   0.963880314
  0.91267171   0.941762397   0.464227895   0.693482786   0.203839405
 0.261512966   0.157981873    0.26582235    0.11557427  0.0442493069
>> m'
  0.41844289    0.91267171   0.261512966
 0.476591435   0.941762397   0.157981873
  0.75054022   0.464227895    0.26582235
 0.226388925   0.693482786    0.11557427
 0.963880314   0.203839405  0.0442493069
```



## Ruby


```ruby
m=[[1,  1,  1,   1],
   [2,  4,  8,  16],
   [3,  9, 27,  81],
   [4, 16, 64, 256],
   [5, 25,125, 625]]
puts m.transpose
```

{{out}}

```txt

 [[1, 2, 3, 4, 5], [1, 4, 9, 16, 25], [1, 8, 27, 64, 125], [1, 16, 81, 256, 625]]

```

or using 'matrix' from the standard library

```ruby
require 'matrix'

m=Matrix[[1,  1,  1,   1],
         [2,  4,  8,  16],
         [3,  9, 27,  81],
         [4, 16, 64, 256],
         [5, 25,125, 625]]
puts m.transpose
```

{{out}}

```txt

 Matrix[[1, 2, 3, 4, 5], [1, 4, 9, 16, 25], [1, 8, 27, 64, 125], [1, 16, 81, 256, 625]]

```

or using zip:

```ruby
def transpose(m)
  m[0].zip(*m[1..-1])
end
p transpose([[1,2,3],[4,5,6]])
```

{{out}}

```txt

  [[1, 4], [2, 5], [3, 6]]

```



## Run BASIC


```runbasic
mtrx$ ="4, 3,   0, 0.10, 0.20, 0.30,   0.40, 0.50, 0.60, 0.70,  0.80, 0.90, 1.00, 1.10"

print "Transpose of matrix"
call DisplayMatrix mtrx$
print "         ="
MatrixT$ =MatrixTranspose$(mtrx$)
call DisplayMatrix MatrixT$

end

function MatrixTranspose$(in$)
  w	= val(word$(in$, 1, ","))    '   swap w and h parameters
  h	= val(word$(in$, 2, ","))
  t$	= str$(h); ","; str$(w); ","
  for i =1 to w
    for j =1 to h
      t$ = t$ +word$(in$, 2 +i +(j -1) *w, ",") +","
    next j
  next i
MatrixTranspose$ =left$(t$, len(t$) -1)
end function

sub DisplayMatrix in$   '   Display looking like a matrix!
html "<table border=2>"
  w	= val(word$(in$, 1, ","))
  h	= val(word$(in$, 2, ","))
  for i =0 to h -1
   html "<tr align=right>"
   for j =1 to w
      term$	= word$(in$, j +2 +i *w, ",")
      html "<td>";val(term$);"</td>"
    next j
html "</tr>"
next i
html "</table>"
end sub
```

{{out}}
Transpose of matrix
<table border=2><tr align=right><td>0</td><td>0.1</td><td>0.2</td><td>0.3</td></tr><tr align=right><td>0.4</td><td>0.5</td><td>0.6</td><td>0.7</td></tr><tr align=right><td>0.8</td><td>0.9</td><td>1.0</td><td>1.1</td></tr></table>     =<br />
<table border=2><tr align=right><td>0</td><td>0.4</td><td>0.8</td></tr><tr align=right><td>0.1</td><td>0.5</td><td>0.9</td></tr><tr align=right><td>0.2</td><td>0.6</td><td>1.0</td></tr><tr align=right><td>0.3</td><td>0.7</td><td>1.1</td></tr></table>


## Rust


### version 1


```rust

struct Matrix {
    dat: [[i32; 3]; 3]
}



impl Matrix {
    pub fn transpose_m(a: Matrix) -> Matrix
    {
        let mut out = Matrix {
            dat: [[0, 0, 0],
                  [0, 0, 0],
                  [0, 0, 0]
                  ]
        };

        for i in 0..3{
            for j in 0..3{

                    out.dat[i][j] = a.dat[j][i];
            }
        }

        out
    }

    pub fn print(self)
    {
        for i in 0..3 {
            for j in 0..3 {
                print!("{} ", self.dat[i][j]);
            }
            print!("\n");
        }
    }
}

fn main()
{
    let  a = Matrix {
        dat: [[1, 2, 3],
              [4, 5, 6],
              [7, 8, 9] ]
    };

let c = Matrix::transpose_m(a);
    c.print();
}

```



### version 2


```rust

fn main() {
    let m = vec![vec![1, 2, 3], vec![4, 5, 6]];
    println!("Matrix:\n{}", matrix_to_string(&m));
    let t = matrix_transpose(m);
    println!("Transpose:\n{}", matrix_to_string(&t));
}

fn matrix_to_string(m: &Vec<Vec<i32>>) -> String {
    m.iter().fold("".to_string(), |a, r| {
        a + &r
            .iter()
            .fold("".to_string(), |b, e| b + "\t" + &e.to_string())
            + "\n"
    })
}

fn matrix_transpose(m: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    let mut t = vec![Vec::with_capacity(m.len()); m[0].len()];
    for r in m {
        for i in 0..r.len() {
            t[i].push(r[i]);
        }
    }
    t
}

```


<b>Output:</b>

```txt

Matrix:
	1	2	3
	4	5	6

Transpose:
	1	4
	2	5
	3	6

```



## Scala


```scala>scala
 Array.tabulate(4)(i => Array.tabulate(4)(j => i*4 + j))
res12: Array[Array[Int]] = Array(Array(0, 1, 2, 3), Array(4, 5, 6, 7), Array(8, 9, 10, 11), Array(12, 13, 14, 15))

scala> res12.transpose
res13: Array[Array[Int]] = Array(Array(0, 4, 8, 12), Array(1, 5, 9, 13), Array(2, 6, 10, 14), Array(3, 7, 11, 15))

scala> res12 map (_ map ("%2d" format _) mkString " ") mkString "\n"
res16: String =
 0  1  2  3
 4  5  6  7
 8  9 10 11
12 13 14 15

scala> res13 map (_ map ("%2d" format _) mkString " ") mkString "\n"
res17: String =
 0  4  8 12
 1  5  9 13
 2  6 10 14
 3  7 11 15
```



## Scheme


```scheme
(define (transpose m)
  (apply map list m))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const type: matrix is array array float;

const func matrix: transpose (in matrix: aMatrix) is func
  result
    var matrix: transposedMatrix is matrix.value;
  local
    var integer: i is 0;
    var integer: j is 0;
  begin
    transposedMatrix := length(aMatrix[1]) times length(aMatrix) times 0.0;
    for i range 1 to length(aMatrix) do
      for j range 1 to length(aMatrix[1]) do
        transposedMatrix[j][i] := aMatrix[i][j];
      end for;
    end for;
  end func;

const proc: write (in matrix: aMatrix) is func
  local
    var integer: line is 0;
    var integer: column is 0;
  begin
    for line range 1 to length(aMatrix) do
      for column range 1 to length(aMatrix[line]) do
        write(" " <& aMatrix[line][column] digits 2);
      end for;
      writeln;
    end for;
  end func;

const matrix: testMatrix is [] (
    [] (0.0, 0.1, 0.2, 0.3),
    [] (0.4, 0.5, 0.6, 0.7),
    [] (0.8, 0.9, 1.0, 1.1));

const proc: main is func
  begin
    writeln("Before Transposition:");
    write(testMatrix);
    writeln;
    writeln("After Transposition:");
    write(transpose(testMatrix));
  end func;
```


{{out}}

```txt

Before Transposition:
 0.00 0.10 0.20 0.30
 0.40 0.50 0.60 0.70
 0.80 0.90 1.00 1.10

After Transposition:
 0.00 0.40 0.80
 0.10 0.50 0.90
 0.20 0.60 1.00
 0.30 0.70 1.10

```



## Sidef


```ruby
func transpose(matrix) {
    matrix[0].range.map{|i| matrix.map{_[i]}};
};

var m = [
  [1,  1,   1,   1],
  [2,  4,   8,  16],
  [3,  9,  27,  81],
  [4, 16,  64, 256],
  [5, 25, 125, 625],
];

transpose(m).each { |row|
    "%5d" * row.len -> printlnf(row...);
}
```

{{out}}

```txt
    1    2    3    4    5
    1    4    9   16   25
    1    8   27   64  125
    1   16   81  256  625
```



## SPAD

{{works with|FriCAS}}
{{works with|OpenAxiom}}
{{works with|Axiom}}

```SPAD
(1) -> originalMatrix := matrix [[1, 1, 1, 1],[2, 4, 8, 16], _
                                 [3, 9, 27, 81],[4, 16, 64, 256], _
                                 [5, 25, 125, 625]]

        +1  1    1    1 +
        |               |
        |2  4    8   16 |
        |               |
   (1)  |3  9   27   81 |
        |               |
        |4  16  64   256|
        |               |
        +5  25  125  625+
                                                        Type: Matrix(Integer)
(2) -> transposedMatrix := transpose(originalMatrix)

        +1  2   3    4    5 +
        |                   |
        |1  4   9   16   25 |
   (2)  |                   |
        |1  8   27  64   125|
        |                   |
        +1  16  81  256  625+
                                                        Type: Matrix(Integer)
```


Domain:[http://fricas.github.io/api/Matrix.html?highlight=matrix Matrix(R)]


## Sparkling


```sparkling
function transpose(A) {
    return map(range(sizeof A), function(k, idx) {
        return map(A, function(k, row) {
            return row[idx];
        });
    });
}
```



## Stata

Stata matrices are always real, so there is no ambiguity about the transpose operator. Mata matrices, however, may be real or complex. The transpose operator is actually a conjugate transpose, but there is also a '''[https://www.stata.com/help.cgi?mf_transposeonly transposeonly()]''' function.


###  Stata matrices


```stata
. mat a=1,2,3\4,5,6
. mat b=a'
. mat list a

a[2,3]
    c1  c2  c3
r1   1   2   3
r2   4   5   6

. mat list b

b[3,2]
    r1  r2
c1   1   4
c2   2   5
c3   3   6
```



###  Mata


```stata
: a=1,1i

: a
        1    2
    +-----------+
  1 |   1   1i  |
    +-----------+

: a'
         1
    +-------+
  1 |    1  |
  2 |  -1i  |
    +-------+

: transposeonly(a)
        1
    +------+
  1 |   1  |
  2 |  1i  |
    +------+
```



## Swift



```swift
@inlinable
public func matrixTranspose<T>(_ matrix: [[T]]) -> [[T]] {
  guard !matrix.isEmpty else {
    return []
  }

  var ret = Array(repeating: [T](), count: matrix[0].count)

  for row in matrix {
    for j in 0..<row.count {
      ret[j].append(row[j])
    }
  }

  return ret
}

@inlinable
public func printMatrix<T>(_ matrix: [[T]]) {
  guard !matrix.isEmpty else {
    print()

    return
  }

  let rows = matrix.count
  let cols = matrix[0].count

  for i in 0..<rows {
    for j in 0..<cols {
      print(matrix[i][j], terminator: " ")
    }

    print()
  }
}

let m1 = [
  [1, 2, 3],
  [4, 5, 6]
]

print("Input:")
printMatrix(m1)


let m2 = matrixTranspose(m1)

print("Output:")
printMatrix(m2)
```


{{out}}


```txt
Input:
1 2 3
4 5 6
Output:
1 4
2 5
3 6
```



## Tailspin


```tailspin

templates transpose
  def a: $;
  [1..$a(1)::length -> $a(1..-1;$)] !
end transpose

templates printMatrix@{w:}
  templates formatN
    @: [];
    $ -> #
    '$@ -> $::length~..$w -> ' ';$@(-1..1:-1)...;' !
    <1..> ..|@: $ mod 10; $ / 10 -> #
  end formatN
  $... -> '|$(1) -> formatN;$(2..-1)... -> ', $ -> formatN;';|
' !
end printMatrix

def m: [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]];
'before:
' -> !OUT::write
$m -> printMatrix@{w:2} -> !OUT::write

def mT: $m -> transpose;
'
transposed:
' -> !OUT::write
$mT -> printMatrix@{w:2} -> !OUT::write

```

{{out}}

```txt

before:
| 1,  2,  3,  4|
| 5,  6,  7,  8|
| 9, 10, 11, 12|

transposed:
| 1,  5,  9|
| 2,  6, 10|
| 3,  7, 11|
| 4,  8, 12|

```



## Tcl

With core Tcl, representing a matrix as a list of lists:

```tcl
package require Tcl 8.5
namespace path ::tcl::mathfunc

proc size {m} {
    set rows [llength $m]
    set cols [llength [lindex $m 0]]
    return [list $rows $cols]
}
proc transpose {m} {
    lassign [size $m] rows cols
    set new [lrepeat $cols [lrepeat $rows ""]]
    for {set i 0} {$i < $rows} {incr i} {
        for {set j 0} {$j < $cols} {incr j} {
            lset new $j $i [lindex $m $i $j]
        }
    }
    return $new
}
proc print_matrix {m {fmt "%.17g"}} {
    set max [widest $m $fmt]
    lassign [size $m] rows cols
    for {set i 0} {$i < $rows} {incr i} {
        for {set j 0} {$j < $cols} {incr j} {
	    set s [format $fmt [lindex $m $i $j]]
            puts -nonewline [format "%*s " [lindex $max $j] $s]
        }
        puts ""
    }
}
proc widest {m {fmt "%.17g"}} {
    lassign [size $m] rows cols
    set max [lrepeat $cols 0]
    for {set i 0} {$i < $rows} {incr i} {
        for {set j 0} {$j < $cols} {incr j} {
	    set s [format $fmt [lindex $m $i $j]]
            lset max $j [max [lindex $max $j] [string length $s]]
        }
    }
    return $max
}

set m {{1 1 1 1} {2 4 8 16} {3 9 27 81} {4 16 64 256} {5 25 125 625}}
print_matrix $m "%d"
print_matrix [transpose $m] "%d"
```

{{out}}

```txt
1  1   1   1
2  4   8  16
3  9  27  81
4 16  64 256
5 25 125 625
1  2  3   4   5
1  4  9  16  25
1  8 27  64 125
1 16 81 256 625
```

{{tcllib|struct::matrix}}

```tcl
package require struct::matrix
struct::matrix M
M deserialize {5 4 {{1 1 1 1} {2 4 8 16} {3 9 27 81} {4 16 64 256} {5 25 125 625}}}
M format 2string
M transpose
M format 2string
```

{{out}}

```txt
1 1  1   1
2 4  8   16
3 9  27  81
4 16 64  256
5 25 125 625
1 2 3 4 5
1 4 9 16 25

1 8 27 64 125


1 16 81 256 625


```


=={{header|TI-83 BASIC}}, {{header|TI-89 BASIC}}==
TI-83: Assuming the original matrix is in <tt>[A]</tt>, place its transpose in <tt>[B]</tt>:
 [A]<sup>T</sup>->[B]
The <sup>T</sup> operator can be found in the matrix math menu.

TI-89: The same except that matrix variables do not have special names:

 A<sup>T</sup> &rarr; B


## Ursala

Matrices are stored as lists of lists, and transposing them is a built in operation.

```Ursala
#cast %eLL

example =

~&K7 <
   <1.,2.,3.,4.>,
   <5.,6.,7.,8.>,
   <9.,10.,11.,12.>>
```

For a more verbose version, replace the ~&K7 operator with the standard library function
named transpose.
{{out}}

```txt

<
   <1.000000e+00,5.000000e+00,9.000000e+00>,
   <2.000000e+00,6.000000e+00,1.000000e+01>,
   <3.000000e+00,7.000000e+00,1.100000e+01>,
   <4.000000e+00,8.000000e+00,1.200000e+01>>

```



## VBA


```vb
Function transpose(m As Variant) As Variant
    transpose = WorksheetFunction.transpose(m)
End Function
```


## VBScript


```vb

'create and display the initial matrix
WScript.StdOut.WriteLine "Initial Matrix:"
x = 4 : y = 6 : n = 1
Dim matrix()
ReDim matrix(x,y)
For i = 0 To y
	For j = 0 To x
		matrix(j,i) = n
		If j < x Then
			WScript.StdOut.Write n & vbTab
		Else
			WScript.StdOut.Write n
		End If
		n = n + 1
	Next
	WScript.StdOut.WriteLine
Next

'display the trasposed matrix
WScript.StdOut.WriteBlankLines(1)
WScript.StdOut.WriteLine "Transposed Matrix:"
For i = 0 To x
	For j = 0 To y
		If j < y Then
			WScript.StdOut.Write matrix(i,j) & vbTab
		Else
			WScript.StdOut.Write matrix(i,j)
		End If
	Next
	WScript.StdOut.WriteLine
Next

```


{{Out}}

```txt

Initial Matrix:
1	2	3	4	5
6	7	8	9	10
11	12	13	14	15
16	17	18	19	20
21	22	23	24	25
26	27	28	29	30
31	32	33	34	35

Transposed Matrix:
1	6	11	16	21	26	31
2	7	12	17	22	27	32
3	8	13	18	23	28	33
4	9	14	19	24	29	34
5	10	15	20	25	30	35

```



## Visual Basic

{{trans|PowerBASIC}}
{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}

```vb
Option Explicit
'----------------------------------------------------------------------
Function TransposeMatrix(InitMatrix() As Long, TransposedMatrix() As Long)
Dim l1 As Long, l2 As Long, u1 As Long, u2 As Long, r As Long, c As Long
  l1 = LBound(InitMatrix, 1)
  l2 = LBound(InitMatrix, 2)
  u1 = UBound(InitMatrix, 1)
  u2 = UBound(InitMatrix, 2)
  ReDim TransposedMatrix(l2 To u2, l1 To u1)
  For r = l1 To u1
    For c = l2 To u2
      TransposedMatrix(c, r) = InitMatrix(r, c)
    Next c
  Next r
End Function
'----------------------------------------------------------------------
Sub PrintMatrix(a() As Long)
Dim l1 As Long, l2 As Long, u1 As Long, u2 As Long, r As Long, c As Long
Dim s As String * 8
  l1 = LBound(a(), 1)
  l2 = LBound(a(), 2)
  u1 = UBound(a(), 1)
  u2 = UBound(a(), 2)
  For r = l1 To u1
    For c = l2 To u2
      RSet s = Str$(a(r, c))
      Debug.Print s;
    Next c
  Debug.Print
  Next r
End Sub
'----------------------------------------------------------------------
Sub TranspositionDemo(ByVal DimSize1 As Long, ByVal DimSize2 As Long)
Dim r, c, cc As Long
Dim a() As Long
Dim b() As Long
  cc = DimSize2
  DimSize1 = DimSize1 - 1
  DimSize2 = DimSize2 - 1
  ReDim a(0 To DimSize1, 0 To DimSize2)
  For r = 0 To DimSize1
    For c = 0 To DimSize2
      a(r, c) = (cc * r) + c + 1
    Next c
  Next r
  Debug.Print "initial matrix:"
  PrintMatrix a()
  TransposeMatrix a(), b()
  Debug.Print "transposed matrix:"
  PrintMatrix b()
End Sub
'----------------------------------------------------------------------
Sub Main()
  TranspositionDemo 3, 3
  TranspositionDemo 3, 7
End Sub
```

{{out}}

```txt
initial matrix:
       1       2       3
       4       5       6
       7       8       9
transposed matrix:
       1       4       7
       2       5       8
       3       6       9
initial matrix:
       1       2       3       4       5       6       7
       8       9      10      11      12      13      14
      15      16      17      18      19      20      21
transposed matrix:
       1       8      15
       2       9      16
       3      10      17
       4      11      18
       5      12      19
       6      13      20
       7      14      21
```



## Wortel

The <code>@zipm</code> operator zips together an array of arrays, this is the same as transposition if the matrix is represented as an array of arrays.

```wortel
@zipm [[1 2 3] [4 5 6] [7 8 9]]
```

Returns:

```txt
[[1 4 7] [2 5 8] [3 6 9]]
```



## zkl

Using the GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
GSL.Matrix(2,3).set(1,2,3, 4,5,6).transpose().format(5).println();  // in place
println("---");
GSL.Matrix(2,2).set(1,2, 3,4).transpose().format(5).println();  // in place
println("---");
GSL.Matrix(3,1).set(1,2,3).transpose().format(5).println();  // in place
```

{{out}}

```txt

 1.00, 4.00
 2.00, 5.00
 3.00, 6.00
---
 1.00, 3.00
 2.00, 4.00
---
 1.00, 2.00, 3.00

```

Or, using lists:
{{trans|Wortel}}

```zkl
fcn transpose(M){
   if(M.len()==1) M[0].pump(List,List.create); // 1 row --> n columns
   else M[0].zip(M.xplode(1));
}
```

The list xplode method pushes list contents on to the call stack.

```zkl
m:=T(T(1,2,3),T(4,5,6));          transpose(m).println();
m:=L(L(1,"a"),L(2,"b"),L(3,"c")); transpose(m).println();
transpose(L(L(1,2,3))).println();
transpose(L(L(1),L(2),L(3))).println();
transpose(L(L(1))).println();
```

{{out}}

```txt

L(L(1,4),L(2,5),L(3,6))
L(L(1,2,3),L("a","b","c"))
L(L(1),L(2),L(3))
(L(1,2,3))
L(L(1))

```


## zonnon


```zonnon

module MatrixOps;
type
	Matrix = array {math} *,* of integer;


	procedure WriteMatrix(x: array {math} *,* of integer);
	var
		i,j: integer;
	begin
		for i := 0 to len(x,0) - 1 do
			for j := 0 to len(x,1) - 1 do
				write(x[i,j]);
			end;
			writeln;
		end
	end WriteMatrix;

	procedure Transposition;
	var
		m,x: Matrix;
	begin
		m := [[1,2,3],[3,4,5]]; (* matrix initialization *)
		x := !m; (* matrix trasposition *)
		WriteMatrix(x);
	end Transposition;

begin
	Transposition;
end MatrixOps.

```

