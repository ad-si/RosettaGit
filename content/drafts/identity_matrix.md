+++
title = "Identity matrix"
description = ""
date = 2019-10-19T09:26:45Z
aliases = []
[extra]
id = 11548
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Matrices]]

;Task:
Build an   [[wp:identity matrix|identity matrix]]   of a size known at run-time.


An ''identity matrix'' is a square matrix of size '''''n'' &times; ''n''''',

where the diagonal elements are all '''1'''s (ones),

and all the other elements are all '''0'''s (zeroes).


<math>I_n = \begin{bmatrix}
  1      & 0      & 0      & \cdots & 0      \\
  0      & 1      & 0      & \cdots & 0      \\
  0      & 0      & 1      & \cdots & 0      \\
  \vdots & \vdots & \vdots & \ddots & \vdots \\
  0      & 0      & 0      & \cdots & 1      \\
\end{bmatrix}</math>


;Related tasks:
*   [[Spiral matrix]]
*   [[Zig-zag matrix]]
*   [[Ulam_spiral_(for_primes)]]





## 360 Assembly


```360asm
*        Identity matrix           31/03/2017
INDENMAT CSECT
         USING  INDENMAT,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         L      R1,N               n
         MH     R1,N+2             n*n
         SLA    R1,2               *4
         ST     R1,LL              amount of storage required
         GETMAIN RU,LV=(R1)        allocate storage for matrix
         USING  DYNA,R11           make storage addressable
         LR     R11,R1             set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,N)          do j=1 to n
       IF CR,R6,EQ,R7 THEN             if i=j then
         LA     R2,1                     k=1
       ELSE     ,                      else
         LA     R2,0                     k=0
       ENDIF    ,                      endif
         LR     R1,R6                  i
         BCTR   R1,0                   -1
         MH     R1,N+2                 *n
         AR     R1,R7                  (i-1)*n+j
         BCTR   R1,0                   -1
         SLA    R1,2                   *4
         ST     R2,A(R1)               a(i,j)=k
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n
         LA     R10,PG               pgi=0
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,N)          do j=1 to n
         LR     R1,R6                  i
         BCTR   R1,0                   -1
         MH     R1,N+2                 *n
         AR     R1,R7                  (i-1)*n+j
         BCTR   R1,0                   -1
         SLA    R1,2                   *4
         L      R2,A(R1)               a(i,j)
         XDECO  R2,XDEC                edit
         MVC    0(1,R10),XDEC+11       output
         LA     R10,1(R10)             pgi+=1
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XPRNT  PG,L'PG              print
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         LA     R1,A               address to free
         LA     R2,LL              amount of storage to free
         FREEMAIN A=(R1),LV=(R2)   free allocated storage
         DROP   R11                drop register
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
NN       EQU    10                 parameter n  (90=>32K)
N        DC     A(NN)              n
LL       DS     F                  n*n*4
PG       DC     CL(NN)' '          buffer
XDEC     DS     CL12               temp
DYNA     DSECT
A        DS     F                  a(n,n)
         YREGS
         END    INDENMAT
```

{{out}}

```txt

1000000000
0100000000
0010000000
0001000000
0000100000
0000010000
0000001000
0000000100
0000000010
0000000001

```



## Ada

When using floating point matrices in Ada 2005+ the function is defined as "Unit_Matrix" in Ada.Numerics.Generic_Real_Arrays. As a generic package it can work with user defined floating point types, or the predefined Short_Real_Arrays, Real_Arrays, and Long_Real_Arrays initializations. As seen below, the first indices of both dimensions can also be set since Ada array indices do not arbitrarily begin with a particular number.

```Ada
--  As prototyped in the Generic_Real_Arrays specification:
--  function Unit_Matrix (Order : Positive; First_1, First_2 : Integer := 1) return Real_Matrix;
-- For the task:
mat : Real_Matrix := Unit_Matrix(5);
```

For prior versions of Ada, or non floating point types its back to basics:

```Ada
type Matrix is array(Positive Range <>, Positive Range <>) of Integer;
mat : Matrix(1..5,1..5) := (others => (others => 0));
--  then after the declarative section:
for i in mat'Range(1) loop mat(i,i) := 1; end loop;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - one extension to language used - PRAGMA READ - a non standard feature similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.8 algol68g-2.8].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''Note:''' The generic vector and matrix code should be moved to a more generic page.

'''File: prelude/vector_base.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

# Define some generic vector initialisation and printing operations #

COMMENT REQUIRES:
  MODE SCAL = ~ # a scalar, eg REAL #;
  FORMAT scal fmt := ~;
END COMMENT

INT vec lwb := 1, vec upb := 0;
MODE VECNEW = [vec lwb:vec upb]SCAL; MODE VEC = REF VECNEW;
FORMAT vec fmt := $"("n(vec upb-vec lwb)(f(scal fmt)", ")f(scal fmt)")"$;

PRIO INIT = 1;

OP INIT = (VEC self, SCAL scal)VEC: (
  FOR col FROM LWB self TO UPB self DO self[col]:= scal OD;
  self
);

# ZEROINIT: defines the additive identity #
OP ZEROINIT = (VEC self)VEC:
  self INIT SCAL(0);

OP REPR = (VEC self)STRING: (
  FILE f; STRING s; associate(f,s);
  vec lwb := LWB self; vec upb := UPB self;
  putf(f, (vec fmt, self)); close(f);
  s
);

SKIP
```
'''File: prelude/matrix_base.a68'''
```algol68
# -*- coding: utf-8 -*- #

# Define some generic matrix initialisation and printing operations #

COMMENT REQUIRES:
  MODE SCAL = ~ # a scalar, eg REAL #;
  MODE VEC = []SCAL;
  FORMAT scal fmt := ~;
  et al.
END COMMENT

INT mat lwb := 1, mat upb := 0;
MODE MATNEW = [mat lwb:mat upb, vec lwb: vec upb]SCAL; MODE MAT = REF MATNEW;
FORMAT mat fmt = $"("n(vec upb-vec lwb)(f(vec fmt)","lx)f(vec fmt)")"l$;

PRIO DIAGINIT = 1;

OP INIT = (MAT self, SCAL scal)MAT: (
  FOR row FROM LWB self TO UPB self DO self[row,] INIT scal OD;
  self
);

# ZEROINIT: defines the additive identity #
OP ZEROINIT = (MAT self)MAT:
  self INIT SCAL(0);

OP REPR = (MATNEW self)STRING: (
  FILE f; STRING s; associate(f,s);
  vec lwb := 2 LWB self; vec upb := 2 UPB self;
  mat lwb :=   LWB self; mat upb :=   UPB self;
  putf(f, (mat fmt, self)); close(f);
  s
);

OP DIAGINIT = (MAT self, VEC diag)MAT: (
  ZEROINIT self;
  FOR d FROM LWB diag TO UPB diag DO self[d,d]:= diag[d] OD;
# or alternatively using TORRIX ...
  DIAG self := diag;
#
  self
);

# ONEINIT: defines the multiplicative identity #
OP ONEINIT = (MAT self)MAT: (
  ZEROINIT self DIAGINIT (LOC[LWB self:UPB self]SCAL INIT SCAL(1))
# or alternatively using TORRIX ...
  (DIAG out) VECINIT SCAL(1)
#
);

SKIP
```
'''File: prelude/matrix_ident.a68'''
```algol68
# -*- coding: utf-8 -*- #

PRIO IDENT = 9; # The same as I for COMPLex #

OP IDENT = (INT lwb, upb)MATNEW:
  ONEINIT LOC [lwb:upb,lwb:upb]SCAL;

OP IDENT = (INT upb)MATNEW: # default lwb is 1 #
  1 IDENT upb;

SKIP
```
'''File: prelude/matrix.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

PR READ "prelude/vector_base.a68" PR;
PR READ "prelude/matrix_base.a68" PR;
PR READ "prelude/matrix_ident.a68" PR;

SKIP
```
'''File: test/matrix_ident.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

MODE SCAL = REAL;
FORMAT scal fmt := $g(-3,1)$;

PR READ "prelude/matrix.a68" PR;

print(REPR IDENT 4)
```

{{out}}

```txt

((1.0, 0.0, 0.0, 0.0),
 (0.0, 1.0, 0.0, 0.0),
 (0.0, 0.0, 1.0, 0.0),
 (0.0, 0.0, 0.0, 1.0))

```



## ALGOL W


```algolw
begin
    % set m to an identity matrix of size s                                  %
    procedure makeIdentity( real    array m ( *, * )
                          ; integer value s
                          ) ;
        for i := 1 until s do begin
            for j := 1 until s do m( i, j ) := 0.0;
            m( i, i ) := 1.0
        end makeIdentity ;

    % test the makeIdentity procedure                                        %
    begin
        real array id5( 1 :: 5, 1 :: 5 );
        makeIdentity( id5, 5 );
        r_format := "A"; r_w := 6; r_d := 1; % set output format for reals   %
        for i := 1 until 5 do begin
            write();
            for j := 1 until 5 do writeon( id5( i, j ) )
        end for_i ;
    end text

end.
```



## APL

Making an identity matrix in APL involves the outer product of the equality function.

For a square matrix of 3:

```apl

    ∘.=⍨⍳3
1 0 0
0 1 0
0 0 1

```


For a function that makes an identity matrix:

```apl

    ID←{∘.=⍨⍳⍵}
    ID 5
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

```

An tacit function can be defined with one of the following equivalent lines:

```apl

    ID←∘.=⍨⍳
    ID←⍳∘.=⍳

```


There is a more idomatic way however:

```apl

    ID←{⍵ ⍵ ρ 1, ⍵ρ0}

```



## AppleScript



```AppleScript
-- ID MATRIX -----------------------------------------------------------------

-- idMatrix :: Int -> [(0|1)]
on idMatrix(n)
    set xs to enumFromTo(1, n)

    script row
        on |λ|(x)
            script
                on |λ|(i)
                    if i = x then
                        1
                    else
                        0
                    end if
                end |λ|
            end script

            map(result, xs)
        end |λ|
    end script

    map(row, xs)
end idMatrix


-- TEST ----------------------------------------------------------------------
on run

    idMatrix(5)

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

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

```txt
{{1, 0, 0, 0, 0},
{0, 1, 0, 0, 0},
{0, 0, 1, 0, 0},
{0, 0, 0, 1, 0},
{0, 0, 0, 0, 1}}
```



## Applesoft BASIC


```Applesoft BASIC

100 INPUT "MATRIX SIZE:"; SIZE%
110 GOSUB 200"IDENTITYMATRIX
120 FOR R = 0 TO SIZE%
130     FOR C = 0 TO SIZE%
140         LET S$ = CHR$(13)
150         IF C < SIZE% THEN S$ = " "
160         PRINT IM(R, C) S$; : NEXT C, R
170 END

200 REMIDENTITYMATRIX SIZE%
210 LET SIZE% = SIZE% - 1
220 DIM IM(SIZE%, SIZE%)
230 FOR I = 0 TO SIZE%
240     LET IM(I, I) = 1 : NEXT I
250 RETURN :IM

```



## ATS


```ATS

(* ****** ****** *)
//
// How to compile:
//
// patscc -DATS_MEMALLOC_LIBC -o idmatrix idmatrix.dats
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

extern
fun
idmatrix{n:nat}(n: size_t(n)): matrixref(int, n, n)
implement
idmatrix(n) =
matrixref_tabulate_cloref<int> (n, n, lam(i, j) => bool2int0(i = j))

(* ****** ****** *)

implement
main0 () =
{
//
val N = 5
//
val M = idmatrix(i2sz(N))
val () = fprint_matrixref_sep (stdout_ref, M, i2sz(N), i2sz(N), " ", "\n")
val () = fprint_newline (stdout_ref)
//
} (* end of [main0] *)

```



## AutoHotkey


```autohotkey
msgbox % Clipboard := I(6)
return

I(n){
    r := "--`n" , s := " "
    loop % n
    {
        k := A_index , r .= "|  "
        loop % n
            r .= A_index=k ? "1, " : "0, "
        r := RTrim(r, " ,") , r .= "  |`n"
    }
    loop % 4*n
        s .= " "
    return Rtrim(r,"`n") "`n" s "--"
}
```


{{out}}

```txt

--
|  1, 0, 0, 0, 0, 0  |
|  0, 1, 0, 0, 0, 0  |
|  0, 0, 1, 0, 0, 0  |
|  0, 0, 0, 1, 0, 0  |
|  0, 0, 0, 0, 1, 0  |
|  0, 0, 0, 0, 0, 1  |
                    --

```



## AWK


```AWK

# syntax: GAWK -f IDENTITY_MATRIX.AWK size
BEGIN {
    size = ARGV[1]
    if (size !~ /^[0-9]+$/) {
      print("size invalid or missing from command line")
      exit(1)
    }
    for (i=1; i<=size; i++) {
      for (j=1; j<=size; j++) {
        x = (i == j) ? 1 : 0
        printf("%2d",x) # print
        arr[i,j] = x # build
      }
      printf("\n")
    }
    exit(0)
}

```

{{out}} for command: GAWK -f IDENTITY_MATRIX.AWK 5

```txt

 1 0 0 0 0
 0 1 0 0 0
 0 0 1 0 0
 0 0 0 1 0
 0 0 0 0 1

```



## Bash


```Bash

for i in `seq $1`;do printf '%*s\n' $1|tr ' ' '0'|sed "s/0/1/$i";done

```

{{out}} for command: ./scriptname 5

```txt

 1 0 0 0 0
 0 1 0 0 0
 0 0 1 0 0
 0 0 0 1 0
 0 0 0 0 1

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INPUT "Enter size of matrix: " size%
      PROCidentitymatrix(size%, im())
      FOR r% = 0 TO size%-1
        FOR c% = 0 TO size%-1
          PRINT im(r%, c%),;
        NEXT
        PRINT
      NEXT r%
      END

      DEF PROCidentitymatrix(s%, RETURN m())
      LOCAL i%
      DIM m(s%-1, s%-1)
      FOR i% = 0 TO s%-1
        m(i%,i%) = 1
      NEXT
      ENDPROC
```



## Burlesque


Neither very elegant nor short but it'll do


```burlesque

blsq ) 6 -.^^0\/r@\/'0\/.*'1+]\/{\/{rt}\/E!XX}x/+]m[sp
1 0 0 0 0 0
0 1 0 0 0 0
0 0 1 0 0 0
0 0 0 1 0 0
0 0 0 0 1 0
0 0 0 0 0 1

```


The example above uses strings to generate the identity matrix. If you need a matrix with real numbers (Integers) then use:


```burlesque

6hd0bx#a.*\[#a.*0#a?dr@{(D!)\/1\/^^bx\/[+}m[e!

```


Shorter alternative:


```burlesque

blsq ) 6 ^^^^10\/**XXcy\/co.+sp

```



## C


```C

#include <stdlib.h>
#include <stdio.h>
int main(int argc, char** argv) {
   if (argc < 2) {
      printf("usage: identitymatrix <number of rows>\n");
      exit(EXIT_FAILURE);
   }
   signed int rowsize = atoi(argv[1]);
   if (rowsize < 0) {
      printf("Dimensions of matrix cannot be negative\n");
      exit(EXIT_FAILURE);
   }
   volatile int numElements = rowsize * rowsize;
   if (numElements < rowsize) {
      printf("Squaring %d caused result to overflow to %d.\n", rowsize, numElements);
      abort();
   }
   int** matrix = calloc(numElements, sizeof(int*));
   if (!matrix) {
      printf("Failed to allocate %d elements of %d bytes each\n", numElements, sizeof(int*));
      abort();
   }
   for (unsigned int row = 0;row < rowsize;row++) {
      matrix[row] = calloc(numElements, sizeof(int));
      if (!matrix[row]) {
         printf("Failed to allocate %d elements of %d bytes each\n", numElements, sizeof(int));
         abort();
      }
      matrix[row][row] = 1;
   }
   printf("Matrix is: \n");
   for (unsigned int row = 0;row < rowsize;row++) {
      for (unsigned int column = 0;column < rowsize;column++) {
         printf("%d ", matrix[row][column]);
      }
      printf("\n");
   }
}

```



## C++

{{libheader|STL}}

```cpp
template<class T>

class matrix
{
public:
    matrix( unsigned int nSize ) :
      m_oData(nSize * nSize, 0), m_nSize(nSize) {}

      inline T& operator()(unsigned int x, unsigned int y)
      {
          return m_oData[x+m_nSize*y];
      }

      void identity()
      {
          int nCount = 0;
          int nStride = m_nSize + 1;
          std::generate( m_oData.begin(), m_oData.end(),
              [&]() { return !(nCount++%nStride); } );
      }

      inline unsigned int size() { return m_nSize; }

private:
    std::vector<T>    m_oData;
    unsigned int      m_nSize;
};

int main()
{
    int nSize;
    std::cout << "Enter matrix size (N): ";
    std::cin >> nSize;

    matrix<int> oMatrix( nSize );

    oMatrix.identity();

    for ( unsigned int y = 0; y < oMatrix.size(); y++ )
    {
        for ( unsigned int x = 0; x < oMatrix.size(); x++ )
        {
            std::cout << oMatrix(x,y) << " ";
        }
        std::cout << std::endl;
    }
    return 0;
}

```

{{libheader|boost}}

```cpp

#include <boost/numeric/ublas/matrix.hpp>

int main()
{
    using namespace boost::numeric::ublas;

    int nSize;
    std::cout << "Enter matrix size (N): ";
    std::cin >> nSize;

    identity_matrix<int> oMatrix( nSize );

    for ( unsigned int y = 0; y < oMatrix.size2(); y++ )
    {
        for ( unsigned int x = 0; x < oMatrix.size1(); x++ )
        {
            std::cout << oMatrix(x,y) << " ";
        }
        std::cout << std::endl;
    }

    return 0;
}

```

{{out}}

```txt

Enter matrix size (N): 5
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

```



## C sharp


```csharp

using System;
using System.Linq;

namespace IdentityMatrix
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Requires exactly one argument");
                return;
            }
            int n;
            if (!int.TryParse(args[0], out n))
            {
                Console.WriteLine("Requires integer parameter");
                return;
            }

            var identity =
                Enumerable.Range(0, n).Select(i => Enumerable.Repeat(0, n).Select((z,j) => j == i ? 1 : 0).ToList()).ToList();
            foreach (var row in identity)
            {
                foreach (var elem in row)
                {
                    Console.Write(" " + elem);
                }
                Console.WriteLine();
            }
            Console.ReadKey();
        }
    }
}

```

{{out}}

```txt

 1 0 0 0 0 0
 0 1 0 0 0 0
 0 0 1 0 0 0
 0 0 0 1 0 0
 0 0 0 0 1 0
 0 0 0 0 0 1

```



## Clio


```clio
fn identity-matrix n:
  [0:n] -> * fn i:
    [0:n] -> * if = i: 1
                 else: 0

5 -> identity-matrix -> * print
```



## Clojure

{{trans|PicoLisp}}

The (vec ) function in the following solution is with respect to vector matrices. If dealing with normal lists matrices (e.g.

```clojure
 '( (0 1) (2 3) )

```

, then care to remove the vec function.

```clojure
(defn identity-matrix [n]
  (let [row (conj (repeat (dec n) 0) 1)]
    (vec
      (for [i (range 1 (inc n))]
        (vec
          (reduce conj (drop i row ) (take i row)))))))

```

{{out}}

```clojure>=
 (identity-matrix 5)
[[1 0 0 0 0] [0 1 0 0 0] [0 0 1 0 0] [0 0 0 1 0] [0 0 0 0 1]]

```


The following is a more idomatic definition that utilizes infinite lists and cycling.

```clojure

(defn identity-matrix [n]
  (take n
    (partition n (dec n)
                         (cycle (conj (repeat (dec n) 0) 1)))))

```



## Common Lisp


Common Lisp provides multi-dimensional arrays.


```lisp
(defun make-identity-matrix (n)
  (let ((array (make-array (list n n) :initial-element 0)))
    (loop for i below n do (setf (aref array i i) 1))
    array))

```


{{out}}
 * (make-identity-matrix 5)
 #2A((1 0 0 0 0) (0 1 0 0 0) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 1))


```lisp
(defun identity-matrix (n)
  (loop for a from 1 to n
        collect (loop for e from 1 to n
                      if (= a e) collect 1
                      else collect 0)))

```


{{out}}

```txt

> (identity-matrix 5)
((1 0 0 0 0) (0 1 0 0 0) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 1))

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Algebras;
IMPORT StdLog,Strings;

TYPE
	Matrix = POINTER TO ARRAY OF ARRAY OF INTEGER;

PROCEDURE NewIdentityMatrix(n: INTEGER): Matrix;
VAR
	m: Matrix;
	i: INTEGER;
BEGIN
	NEW(m,n,n);
	FOR i := 0 TO n - 1 DO
		m[i,i] := 1;
	END;
	RETURN m;
END NewIdentityMatrix;

PROCEDURE Show(m: Matrix);
VAR
	i,j: INTEGER;
BEGIN
	FOR i := 0 TO LEN(m,0) - 1 DO
		FOR j := 0 TO LEN(m,1) - 1 DO
			StdLog.Int(m[i,j]);
		END;
		StdLog.Ln
	END
END Show;

PROCEDURE Do*;
BEGIN
	Show(NewIdentityMatrix(5));
END Do;
END Algebras.

```

Execute: ^Q Algebras.Do<br/>
{{out}}

```txt

 1 0 0 0 0
 0 1 0 0 0
 0 0 1 0 0
 0 0 0 1 0
 0 0 0 0 1

```



## D


```d
import std.traits;

T[][] matId(T)(in size_t n) pure nothrow if (isAssignable!(T, T)) {
    auto Id = new T[][](n, n);

    foreach (r, row; Id) {
        static if (__traits(compiles, {row[] = 0;})) {
            row[] = 0; // vector op doesn't work with T = BigInt
            row[r] = 1;
        } else {
            foreach (c; 0 .. n)
                row[c] = (c == r) ? 1 : 0;
        }
    }

    return Id;
}

void main() {
    import std.stdio, std.bigint;
    enum form = "[%([%(%s, %)],\n %)]]";

    immutable id1 = matId!real(5);
    writefln(form ~ "\n", id1);

    immutable id2 = matId!BigInt(3);
    writefln(form ~ "\n", id2);

    // auto id3 = matId!(const int)(2); // cant't compile
}
```

{{out}}

```txt
[[1, 0, 0, 0, 0],
 [0, 1, 0, 0, 0],
 [0, 0, 1, 0, 0],
 [0, 0, 0, 1, 0],
 [0, 0, 0, 0, 1]]

[[1, 0, 0],
 [0, 1, 0],
 [0, 0, 1]]
```



## Delphi


```Delphi
program IdentityMatrix;

// Modified from the Pascal version

{$APPTYPE CONSOLE}

var
  matrix: array of array of integer;
  n, i, j: integer;

begin
  write('Size of matrix: ');
  readln(n);
  setlength(matrix, n, n);

  for i := 0 to n - 1 do
    matrix[i,i] := 1;

  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
      write (matrix[i,j], ' ');
    writeln;
  end;
end.
```

{{out}}

```txt

Size of matrix: 5
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

```



## Eiffel


```Eiffel

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
		    dim : INTEGER -- Dimension of the identity matrix
		do
		    from dim := 1 until dim > 10 loop
		    	print_matrix( identity_matrix(dim) )
				dim := dim + 1
				io.new_line
			end

		end

feature -- Access

	identity_matrix(dim : INTEGER) : ARRAY2[REAL_64]

		require
			dim > 0
		local
			matrix : ARRAY2[REAL_64]
			i : INTEGER
		do

			create matrix.make_filled (0.0, dim, dim)
			from i := 1 until i > dim loop
				matrix.put(1.0, i, i)
				i := i + 1
			end

			Result := matrix
		end

	print_matrix(matrix : ARRAY2[REAL_64])
		local
			i, j : INTEGER
		do
			from i := 1 until i > matrix.height loop
				print("[ ")
				from j := 1 until j > matrix.width loop
					print(matrix.item (i, j))
					print(" ")
					j := j + 1
				end
				print("]%N")
				i := i + 1
			end
		end

end

```


{{out}}

```txt

[ 1 0 0 0 0 0 0 0 0 0 ]
[ 0 1 0 0 0 0 0 0 0 0 ]
[ 0 0 1 0 0 0 0 0 0 0 ]
[ 0 0 0 1 0 0 0 0 0 0 ]
[ 0 0 0 0 1 0 0 0 0 0 ]
[ 0 0 0 0 0 1 0 0 0 0 ]
[ 0 0 0 0 0 0 1 0 0 0 ]
[ 0 0 0 0 0 0 0 1 0 0 ]
[ 0 0 0 0 0 0 0 0 1 0 ]
[ 0 0 0 0 0 0 0 0 0 1 ]

```



## Elena

ELENA 4.x :

```elena
import extensions;
import system'routines;
import system'collections;

public program()
{
    var n := console.write:"Enter the matrix size:".readLine().toInt();

    var identity := new Range(0, n).selectBy:(i => new Range(0,n).selectBy:(j => (i == j).iif(1,0) ).summarize(new ArrayList()))
                         .summarize(new ArrayList());

    identity.forEach:
        (row) { console.printLine(row.asEnumerable()) }
}
```

{{out}}

```txt

Enter the matrix size:3
1,0,0
0,1,0
0,0,1

```



## Elixir


```elixir
defmodule Matrix do
  def identity(n) do
    Enum.map(0..n-1, fn i ->
      for j <- 0..n-1, do: (if i==j, do: 1, else: 0)
    end)
  end
end

IO.inspect Matrix.identity(5)
```


{{out}}

```txt

[[1, 0, 0, 0, 0], [0, 1, 0, 0, 0], [0, 0, 1, 0, 0], [0, 0, 0, 1, 0],
 [0, 0, 0, 0, 1]]

```



## Erlang


```erlang
%% Identity Matrix in Erlang for the Rosetta Code Wiki.
%% Implemented by Arjun Sunel

-module(identity_matrix).
-export([square_matrix/2 , identity/1]).

square_matrix(Size, Elements) ->
    [[Elements(Column, Row) || Column <- lists:seq(1, Size)] || Row <- lists:seq(1, Size)].

identity(Size) ->
    square_matrix(Size, fun(Column, Row) -> case Column of Row -> 1; _ -> 0 end end).
```



## ERRE


```ERRE

PROGRAM IDENTITY

!$DYNAMIC
DIM A[0,0]

BEGIN
  PRINT(CHR$(12);) ! CLS
  INPUT("Matrix size",N%)
  !$DIM A[N%,N%]
  FOR I%=1 TO N% DO
    A[I%,I%]=1
  END FOR
! print matrix
  FOR I%=1 TO N% DO
    FOR J%=1 TO N% DO
      WRITE("###";A[I%,J%];)
    END FOR
    PRINT
  END FOR
END PROGRAM

```



## Euler Math Toolbox



```Euler Math Toolbox

function IdentityMatrix(n)
  $  X:=zeros(n,n);
  $  for i=1 to n
  $    X[i,i]:=1;
  $  end;
  $  return X;
  $endfunction

```



```Euler Math Toobox

>function IdentityMatrix (n:index)
$  return setdiag(zeros(n,n),0,1);
$endfunction

```


<lang>
>id(5)

```


=={{header|F Sharp|F#}}==
Builds a 2D matrix with the given square size.

```FSharp

let ident n = Array2D.init n n (fun i j -> if i = j then 1 else 0)

```


{{out}}

```FSharp

ident 10;;
val it : int [,] = [[1; 0; 0; 0; 0; 0; 0; 0; 0; 0]
                    [0; 1; 0; 0; 0; 0; 0; 0; 0; 0]
                    [0; 0; 1; 0; 0; 0; 0; 0; 0; 0]
                    [0; 0; 0; 1; 0; 0; 0; 0; 0; 0]
                    [0; 0; 0; 0; 1; 0; 0; 0; 0; 0]
                    [0; 0; 0; 0; 0; 1; 0; 0; 0; 0]
                    [0; 0; 0; 0; 0; 0; 1; 0; 0; 0]
                    [0; 0; 0; 0; 0; 0; 0; 1; 0; 0]
                    [0; 0; 0; 0; 0; 0; 0; 0; 1; 0]
                    [0; 0; 0; 0; 0; 0; 0; 0; 0; 1]]

```



## Factor


```factor
USE: math.matrices
IN: scratchpad 6 identity-matrix .
{
    { 1 0 0 0 0 0 }
    { 0 1 0 0 0 0 }
    { 0 0 1 0 0 0 }
    { 0 0 0 1 0 0 }
    { 0 0 0 0 1 0 }
    { 0 0 0 0 0 1 }
}
```



## FBSL

FBSL's BASIC layer can easily manipulate square matrices of arbitrary sizes and data types in ways similar to e.g. [[BBC BASIC]] or [[OxygenBasic]] as shown elsewhere on this page. But FBSL has also an '''extremely fast built-in''' single-precision vector2f/3f/4f, plane4f, quaternion4f, and matrix4f math library totaling 150 functions and targeting primarily 3D rendering tasks:
<div style="overflow:auto;white-space:nowrap;background-color:ivory;border:1px dashed rgb(47, 111, 171); padding:12px"><b><code>
<span style="color:gray">#APPTYPE CONSOLE</span>

<span style="color:red">TYPE</span> M4F <span style="color:green">' Matrix 4F</span>
:m11 <span style="color:blueviolet">AS SINGLE</span>
:m12 <span style="color:blueviolet">AS SINGLE</span>
:m13 <span style="color:blueviolet">AS SINGLE</span>
:m14 <span style="color:blueviolet">AS SINGLE</span>
:m21 <span style="color:blueviolet">AS SINGLE</span>
:m22 <span style="color:blueviolet">AS SINGLE</span>
:m23 <span style="color:blueviolet">AS SINGLE</span>
:m24 <span style="color:blueviolet">AS SINGLE</span>
:m31 <span style="color:blueviolet">AS SINGLE</span>
:m32 <span style="color:blueviolet">AS SINGLE</span>
:m33 <span style="color:blueviolet">AS SINGLE</span>
:m34 <span style="color:blueviolet">AS SINGLE</span>
:m41 <span style="color:blueviolet">AS SINGLE</span>
:m42 <span style="color:blueviolet">AS SINGLE</span>
:m43 <span style="color:blueviolet">AS SINGLE</span>
:m44 <span style="color:blueviolet">AS SINGLE</span>

<span style="color:red">END TYPE</span>

<span style="color:blueviolet">DIM</span> m <span style="color:blueviolet">AS</span> M4F <span style="color:green">' DIM zeros out any variable automatically</span>

<span style="color:blue">PRINT</span> <span style="color:maroon">"Matrix 'm' is identity: "</span>, <span style="color:blue">IIF</span>(<span style="color:blue">MATRIXISIDENTITY</span>(<span style="color:maroon">@</span>m), <span style="color:maroon">"TRUE"</span>, <span style="color:maroon">"FALSE"</span>) <span style="color:green">' is matrix an identity?</span>

<span style="color:blue">MATRIXIDENTITY</span>(<span style="color:maroon">@</span>m) <span style="color:green">' set matrix to identity</span>

<span style="color:blue">PRINT</span> <span style="color:maroon">"Matrix 'm' is identity: "</span>, <span style="color:blue">IIF</span>(<span style="color:blue">MATRIXISIDENTITY</span>(<span style="color:maroon">@</span>m), <span style="color:maroon">"TRUE"</span>, <span style="color:maroon">"FALSE"</span>) <span style="color:green">' is matrix an identity?</span>

<span style="color:blue">PAUSE</span>
</code></b></div>
{{out}}
<div style="overflow:auto;white-space:nowrap;background-color:black;border:1px dashed rgb(167, 215, 249); padding:12px"><b><code>
<span style="color:white">Matrix 'm' is identity: FALSE

Matrix 'm' is identity: TRUE


Press any key to continue...</span>
</code></b></div>

=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Identity_matrix this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

{{libheader|Forth Scientific Library}}
{{works with|gforth|0.7.9_20170308}}

```forth
S" fsl-util.fs" REQUIRED

: build-identity ( 'p n -- 'p )  \ make an NxN identity matrix
  0 DO
    I 1+ 0 DO
      I J = IF  1.0E0 DUP I J }} F!
      ELSE
        0.0E0 DUP J I }} F!
        0.0E0 DUP I J }} F!
      THEN
    LOOP
  LOOP ;

6 6 float matrix a{{
a{{ 6 build-identity
6 6 a{{ }}fprint
```



## Fortran

{{works with|Fortran|95}}


```fortran

program identitymatrix

  real, dimension(:, :), allocatable :: I
  character(len=8) :: fmt
  integer :: ms, j

  ms = 10   ! the desired size

  allocate(I(ms,ms))
  I = 0                           ! Initialize the array.
  forall(j = 1:ms) I(j,j) = 1     ! Set the diagonal.

  ! I is the identity matrix, let's show it:

  write (fmt, '(A,I2,A)') '(', ms, 'F6.2)'
  ! if you consider to have used the (row, col) convention,
  ! the following will print the transposed matrix (col, row)
  ! but I' = I, so it's not important here
  write (*, fmt) I(:,:)

  deallocate(I)

end program identitymatrix

```



### Notorious trick

The objective is to do the assignment in one fell swoop, rather than separately setting the 0 values and the 1 values. It works because, with integer arithmetic, the only way that both i/j and j/i are one is when they are equal - thus one on the diagonal elements, and zero elsewhere because either i < j so that i/j = 0, or i > j so that j/i = 0. While this means two divides and a multiply per element instead of simply transferring a constant, the constraint on speed is likely to be the limited bandwidth from cpu to memory. The expression's code would surely fit in the cpu's internal memory, and registers would be used for the variables.

```Fortran
      Program Identity
      Integer N
      Parameter (N = 666)
      Real A(N,N)
      Integer i,j

      ForAll(i = 1:N, j = 1:N) A(i,j) = (i/j)*(j/i)

      END
```


The <code>ForAll</code> statement is a feature of F90, and carries the implication that the assignments may be done in any order, even "simultaneously" (as with multiple cpus), plus that all RHS values are calculated before any LHS part receives a value - not relevant here since the RHS makes no reference to items altered in the LHS. Earlier Fortran compilers lack this statement and so one must use explicit DO-loops:

```Fortran
      DO 1 I = 1,N
        DO 1 J = 1,N
    1     A(I,J) = (I/J)*(J/I)
```

Array assignment statements are also a feature of F90 and later.

An alternative might be a simpler logical expression testing ''i = j'' except that the numerical values for ''true'' and ''false'' on a particular system may well not be 1 and 0 but (for instance, via Compaq F90/95 on Windows XP) 0 and -1 instead. On an IBM 390 mainframe, pl/i and Fortran used different values. The Burroughs 6700 inspected the low-order bit only, with the intriguing result that odd integers would be deemed ''true'' and even ''false''. Integer arithmetic can't be relied upon across languages either, because in pl/i, integer division doesn't truncate.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim As Integer n

Do
  Input "Enter size of matrix "; n
Loop Until n > 0

Dim identity(1 To n, 1 To n) As Integer '' all zero by default

' enter 1s in diagonal elements
For i As Integer =  1 To n
  identity(i, i) = 1
Next

' print identity matrix if n < 40
Print

If n < 40 Then
  For i As Integer = 1 To n
    For j As Integer = 1 To n
      Print identity(i, j);
    Next j
    Print
  Next i
Else
  Print "Matrix is too big to display on 80 column console"
End If

Print
Print "Press any key to quit"
Sleep
```

Sample input/output
{{out}}

```txt

Enter size of matrix ? 5

 1 0 0 0 0
 0 1 0 0 0
 0 0 1 0 0
 0 0 0 1 0
 0 0 0 0 1

```



## FunL


```funl
def identity( n ) = vector( n, n, \r, c -> if r == c then 1 else 0 )

println( identity(3) )
```


{{out}}


```txt

((1, 0, 0), (0, 1, 0), (0, 0, 1))

```



## GAP


```gap
# Built-in
IdentityMat(3);

# One can also specify the base ring
IdentityMat(3, Integers mod 10);
```



## Go


### Library gonum/mat


```go
package main

import (
    "fmt"

    "gonum.org/v1/gonum/mat"
)

func eye(n int) *mat.Dense {
    m := mat.NewDense(n, n, nil)
    for i := 0; i < n; i++ {
        m.Set(i, i, 1)
    }
    return m
}

func main() {
    fmt.Println(mat.Formatted(eye(3)))
}
```

{{out}}

```txt

⎡1  0  0⎤
⎢0  1  0⎥
⎣0  0  1⎦

```



### Library go.matrix

A somewhat earlier matrix library for Go.

```go
package main

import (
    "fmt"

    mat "github.com/skelterjohn/go.matrix"
)

func main() {
    fmt.Println(mat.Eye(3))
}
```

{{out}}

```txt

{1, 0, 0,
 0, 1, 0,
 0, 0, 1}

```



### From scratch

'''Simplest: '''  A matrix as a slice of slices, allocated separately.

```go
package main

import "fmt"

func main() {
    fmt.Println(I(3))
}

func I(n int) [][]float64 {
    m := make([][]float64, n)
    for i := 0; i < n; i++ {
        a := make([]float64, n)
        a[i] = 1
        m[i] = a
    }
    return m
}
```

{{out}}
No special formatting method used.

```txt

[[1 0 0] [0 1 0] [0 0 1]]

```


'''2D, resliced: ''' Representation as a slice of slices still, but with all elements based on single underlying slice.  Might save a little memory management, might have a little better locality.

```go
package main

import "fmt"

func main() {
    fmt.Println(I(3))
}

func I(n int) [][]float64 {
    m := make([][]float64, n)
    a := make([]float64, n*n)
    for i := 0; i < n; i++ {
        a[i] = 1
        m[i] = a[:n]
        a = a[n:]
    }
    return m
}
```


{{out}}
Same as previous.

'''Flat: ''' Representation as a single flat slice.  You just have to know to handle it as a square matrix.  In many cases that's not a problem and the code is simpler this way.  If you want to add a little bit of type checking, you can define a matrix type as shown here.

```go
package main

import "fmt"

type matrix []float64

func main() {
    n := 3
    m := I(n)
    // dump flat represenation
    fmt.Println(m)

    // function x turns a row and column into an index into the
    // flat representation.
    x := func(r, c int) int { return r*n + c }

    // access m by row and column.
    for r := 0; r < n; r++ {
        for c := 0; c < n; c++ {
            fmt.Print(m[x(r, c)], " ")
        }
        fmt.Println()
    }
}

func I(n int) matrix {
    m := make(matrix, n*n)
    // a fast way to initialize the flat representation
    n++
    for i := 0; i < len(m); i += n {
        m[i] = 1
    }
    return m
}
```

{{out}}

```txt

[1 0 0 0 1 0 0 0 1]
1 0 0
0 1 0
0 0 1

```



## Groovy

Solution:

```groovy
def makeIdentityMatrix = { n ->
    (0..<n).collect { i -> (0..<n).collect { j -> (i == j) ? 1 : 0 } }
}
```


Test:

```groovy
(2..6).each { order ->
    def iMatrix = makeIdentityMatrix(order)
    iMatrix.each { println it }
    println()
}
```


{{out}}

```txt
[1, 0]
[0, 1]

[1, 0, 0]
[0, 1, 0]
[0, 0, 1]

[1, 0, 0, 0]
[0, 1, 0, 0]
[0, 0, 1, 0]
[0, 0, 0, 1]

[1, 0, 0, 0, 0]
[0, 1, 0, 0, 0]
[0, 0, 1, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 0, 0, 1]

[1, 0, 0, 0, 0, 0]
[0, 1, 0, 0, 0, 0]
[0, 0, 1, 0, 0, 0]
[0, 0, 0, 1, 0, 0]
[0, 0, 0, 0, 1, 0]
[0, 0, 0, 0, 0, 1]
```



## Haskell


```haskell
matI n = [ [fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]
```

And a function to show matrix pretty:

```haskell
showMat :: [[Int]] -> String
showMat = unlines . map (unwords . map show)
```




```haskell
*Main> putStr $ showMat $ matId 9
1 0 0 0 0 0 0 0 0
0 1 0 0 0 0 0 0 0
0 0 1 0 0 0 0 0 0
0 0 0 1 0 0 0 0 0
0 0 0 0 1 0 0 0 0
0 0 0 0 0 1 0 0 0
0 0 0 0 0 0 1 0 0
0 0 0 0 0 0 0 1 0
0 0 0 0 0 0 0 0 1

```


We could alternatively bypassing the syntactic sugaring of list comprehension notation, and use a bind function directly:


```haskell
idMatrix :: Int -> [[Int]]
idMatrix n =
  let xs = [1 .. n]
  in xs >>= \x -> [xs >>= \y -> [fromEnum (x == y)]]
```


or reduce the number of terms a little to:

```haskell
idMatrix :: Int -> [[Int]]
idMatrix n =
  let xs = [1 .. n]
  in (\x -> fromEnum . (x ==) <$> xs) <$> xs

main :: IO ()
main = (putStr . unlines) $ unwords . fmap show <$> idMatrix 5
```

{{Out}}

```txt
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1
```


=={{header|Icon}} and {{header|Unicon}}==

This code works for Icon and Unicon.

```unicon
link matrix
procedure main(argv)
    if not (integer(argv[1]) > 0) then stop("Argument must be a positive integer.")
    matrix1 := identity_matrix(argv[1], argv[1])
    write_matrix(&output,matrix1)
end

```


{{out}}

```txt

->im 6
1 0 0 0 0 0
0 1 0 0 0 0
0 0 1 0 0 0
0 0 0 1 0 0
0 0 0 0 1 0
0 0 0 0 0 1
->

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Identity.bas"
110 INPUT PROMPT "Enter size of matrix: ":N
120 NUMERIC A(1 TO N,1 TO N)
130 CALL INIT(A)
140 CALL WRITE(A)
150 DEF INIT(REF T)
160   FOR I=LBOUND(T,1) TO UBOUND(T,1)
170     FOR J=LBOUND(T,2) TO UBOUND(T,2)
180       LET T(I,J)=0
190     NEXT
200     LET T(I,I)=1
210   NEXT
220 END DEF
230 DEF WRITE(REF T)
240   FOR I=LBOUND(T,1) TO UBOUND(T,1)
250     FOR J=LBOUND(T,2) TO UBOUND(T,2)
260       PRINT T(I,J);
270     NEXT
280     PRINT
290   NEXT
300 END DEF
```



## J


```j
   = i. 4          NB. create an Identity matrix of size 4
1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1
   makeI=: =@i.    NB. define as a verb with a user-defined name
   makeI 5         NB. create an Identity matrix of size 5
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1
```



## Java


```Java
public class PrintIdentityMatrix {

    public static void main(String[] args) {
        int n = 5;
        int[][] array = new int[n][n];

        IntStream.range(0, n).forEach(i -> array[i][i] = 1);

        Arrays.stream(array)
                .map((int[] a) -> Arrays.toString(a))
                .forEach(System.out::println);
    }
}
```


{{out}}

```txt
[1, 0, 0, 0, 0]
[0, 1, 0, 0, 0]
[0, 0, 1, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 0, 0, 1]
```



## JavaScript



### ES5



```Javascript
function idMatrix(n) {
    return Array.apply(null, new Array(n))
        .map(function (x, i, xs) {
            return xs.map(function (_, k) {
                return i === k ? 1 : 0;
            })
        });
}
```



### ES6



```JavaScript
(() => {

    // idMatrix :: Int -> [[0 | 1]]
    const idMatrix = n => Array.from({
            length: n
        }, (_, i) => Array.from({
            length: n
        }, (_, j) => i !== j ? 0 : 1));

    // show :: a -> String
    const show = JSON.stringify;

    // TEST
    return idMatrix(5)
        .map(show)
        .join('\n');
})();
```


{{Out}}

```txt
[1,0,0,0,0]
[0,1,0,0,0]
[0,0,1,0,0]
[0,0,0,1,0]
[0,0,0,0,1]
```



## jq


### Construction


```jq
def identity(n):
  [range(0;n) | 0] as $row
  | reduce range(0;n) as $i ([]; . + [ $row | .[$i] = 1 ] );
```

Example:

```jq
identity(4)
```
produces:
 [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]


### Using matrix/2

Using the definition of matrix/2 at [[Create_a_two-dimensional_array_at_runtime#jq]]:

```jq
def identity(n):
  reduce range(0;n) as $i
    (0 | matrix(n;n); .[$i][$i] = 1);

```



## Jsish


```javascript
/* Identity matrix, in Jsish */
function identityMatrix(n) {
    var mat = new Array(n).fill(0);
    for (var r in mat) {
        mat[r] = new Array(n).fill(0);
        mat[r][r] = 1;
    }
    return mat;
}

provide('identityMatrix', 1);

if (Interp.conf('unitTest')) {
;    identityMatrix(0);
;    identityMatrix(1);
;    identityMatrix(2);
;    identityMatrix(3);
    var mat = identityMatrix(4);
    for (var r in mat) puts(mat[r]);
}

/*
=!EXPECTSTART!=
identityMatrix(0) ==> []
identityMatrix(1) ==> [ [ 1 ] ]
identityMatrix(2) ==> [ [ 1, 0 ], [ 0, 1 ] ]
identityMatrix(3) ==> [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ]
[ 1, 0, 0, 0 ]
[ 0, 1, 0, 0 ]
[ 0, 0, 1, 0 ]
[ 0, 0, 0, 1 ]
=!EXPECTEND!=
*/
```


{{out}}

```txt
promt$ jsish -u identityMatrix.jsi
[PASS] identityMatrix.jsi
```



## Julia

The <tt>eye</tt> function takes an integer argument and returns a square identity matrix of that size.

```Julia

eye(3)

```

This returns:

```txt

3x3 Float64 Array:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0

```


If you want to take the size from the commandline:

```Julia

eye(int(readline(STDIN)))

```


You can also can also call <tt>eye(m,n)</tt> to create an M-by-N identity matrix.
For example:

```Julia

eye(2,3)

```

results in:

```txt

2x3 Float64 Array:
 1.0  0.0  0.0
 0.0  1.0  0.0

```



## K



```K
  =4
(1 0 0 0
 0 1 0 0
 0 0 1 0
 0 0 0 1)
  =5
(1 0 0 0 0
 0 1 0 0 0
 0 0 1 0 0
 0 0 0 1 0
 0 0 0 0 1)

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    print("Enter size of matrix : ")
    val n = readLine()!!.toInt()
    println()
    val identity = Array(n) { IntArray(n) } // create n x n matrix of integers

    // enter 1s in diagonal elements
    for(i in 0 until n) identity[i][i] = 1

    // print identity matrix if n <= 40
    if (n <= 40)
        for (i in 0 until n) println(identity[i].joinToString(" "))
    else
        println("Matrix is too big to display on 80 column console")
}
```

Sample input/output
{{out}}

```txt

Enter size of matrix : 5

1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

```



## LSL

To test it yourself; rez a box on the ground, and add the following as a New Script.

```LSL
default {
	state_entry() {
		llListen(PUBLIC_CHANNEL, "", llGetOwner(), "");
		llOwnerSay("Please Enter a Dimension for an Identity Matrix.");
	}
	listen(integer iChannel, string sName, key kId, string sMessage) {
		llOwnerSay("You entered "+sMessage+".");
		list lMatrix = [];
		integer x = 0;
		integer n = (integer)sMessage;
		for(x=0 ; x<n*n ; x++) {
			lMatrix += [(integer)(((x+1)%(n+1))==1)];
		}
		//llOwnerSay("["+llList2CSV(lMatrix)+"]");
		for(x=0 ; x<n ; x++) {
			llOwnerSay("["+llList2CSV(llList2ListStrided(lMatrix, x*n, (x+1)*n-1, 1))+"]");
		}
	}
}
```

{{out}}

```txt
You: 0
Identity_Matrix: You entered 0.
You: 1
Identity_Matrix: You entered 1.
Identity_Matrix: [1]
You: 3
Identity_Matrix: You entered 3.
Identity_Matrix: [1, 0, 0]
Identity_Matrix: [0, 1, 0]
Identity_Matrix: [0, 0, 1]
You: 5
Identity_Matrix: You entered 5.
Identity_Matrix: [1, 0, 0, 0, 0]
Identity_Matrix: [0, 1, 0, 0, 0]
Identity_Matrix: [0, 0, 1, 0, 0]
Identity_Matrix: [0, 0, 0, 1, 0]
Identity_Matrix: [0, 0, 0, 0, 1]
```



## Lang5


```lang5
: identity-matrix
    dup iota 'A set

    : i.(*) A in ;
    [1] swap append reverse A swap reshape 'i. apply
    ;

5 identity-matrix .
```

{{out}}

```txt
[
  [    1     0     0     0     0  ]
  [    0     1     0     0     0  ]
  [    0     0     1     0     0  ]
  [    0     0     0     1     0  ]
  [    0     0     0     0     1  ]
]
```



## LFE



```lisp

(defun identity
  ((`(,m ,n))
   (identity m n))
  ((m)
   (identity m m)))

(defun identity (m n)
  (lists:duplicate m (lists:duplicate n 1)))

```


From the LFE REPL; note that the last two usage examples demonstrate how identify could be used when composed with functions that get the dimension of a matrix:


```lisp

> (identity 3)
((1 1 1) (1 1 1) (1 1 1))
> (identity 3 3)
((1 1 1) (1 1 1) (1 1 1))
> (identity '(3 3))
((1 1 1) (1 1 1) (1 1 1))


```



## Lua


```lua

function identity_matrix (size)
        local m = {}
        for i = 1, size do
                m[i] = {}
                for j = 1, size do
                        m[i][j] = i == j and 1 or 0
                end
        end
        return m
end

function print_matrix (m)
        for i = 1, #m do
                print(table.concat(m[i], " "))
        end
end

print_matrix(identity_matrix(5))
```

{{out}}

```txt

1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1
```



## Maple

One of a number of ways to do this:

```Maple

> LinearAlgebra:-IdentityMatrix( 4 );
                           [1    0    0    0]
                           [                ]
                           [0    1    0    0]
                           [                ]
                           [0    0    1    0]
                           [                ]
                           [0    0    0    1]

```

Here, for instance, is another, in which the entries are (4-byte) floats.

```Maple

> Matrix( 4, shape = scalar[1], datatype = float[4] );
                         [1.    0.    0.    0.]
                         [                    ]
                         [0.    1.    0.    0.]
                         [                    ]
                         [0.    0.    1.    0.]
                         [                    ]
                         [0.    0.    0.    1.]

```

Yet another, with 2-byte integer entries:

```Maple

> Matrix( 4, shape = identity, datatype = integer[ 2 ] );
                           [1    0    0    0]
                           [                ]
                           [0    1    0    0]
                           [                ]
                           [0    0    1    0]
                           [                ]
                           [0    0    0    1]

```



## MathCortex


```MathCortex
I = eye(10)
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
IdentityMatrix[4]
```


=={{header|MATLAB}} / {{header|Octave}}==
The '''eye''' function create the identity (I) matrix, e.g.:


```MATLAB
I = eye(10)
```



## Maxima


```maxima
ident(4);
/* matrix([1, 0, 0, 0],
          [0, 1, 0, 0],
          [0, 0, 1, 0],
          [0, 0, 0, 1]) */
```



## NetRexx


### Using int Array

{{trans|REXX}}

```NetRexx
/* NetRexx ************************************************************
* show identity matrix of size n
* I consider m[i,j] to represent the matrix
* 09.07.2013 Walter Pachl (translated from REXX Version 2)
**********************************************************************/
options replace format comments java crossref symbols binary

Parse Arg n .
If n='' then n=5
Say 'Identity Matrix of size' n '(m[i,j] IS the Matrix)'
m=int[n,n] -- Allocate 2D square array at run-time
Loop i=0 To n-1 -- Like Java, arrays in NetRexx start at 0
  ol=''
  Loop j=0 To n-1
    m[i,j]=(i=j)
    ol=ol m[i,j]
    End
  Say ol
  End

```



### Using Indexed String


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method createIdMatrix(n) public static
  DIM_ = 'DIMENSION'
  m = 0 -- Indexed string to hold matrix; default value for all elements is zero
  m[DIM_] = n
  loop i = 1 to n -- NetRexx indexed strings don't have to start at zero
    m[i, i] = 1   -- set this diagonal element to 1
    end i
  return m

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method displayIdMatrix(m) public static
  DIM_ = 'DIMENSION'
  if \m.exists(DIM_) then signal RuntimeException('Matrix dimension not set')
  n = m[DIM_]
  loop i = 1 to n
    ol = ''
    loop j = 1 To n
      ol = ol m[i, j]
      end j
    say ol
    end i
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  parse arg n .
  if n = '' then n = 5
  say 'Identity Matrix of size' n
  displayIdMatrix(createIdMatrix(n))
  return

```



## Nim


```nim
proc identityMatrix(n): auto =
  result = newSeq[seq[int]](n)
  for i in 0 .. < result.len:
    result[i] = newSeq[int](n)
    result[i][i] = 1
```



## Objeck


```objeck
class IdentityMatrix {
  function : Matrix(n : Int) ~ Int[,] {
    array := Int->New[n,n];

    for(row:=0; row<n; row+=1;){
      for(col:=0; col<n; col+=1;){
        if(row = col){
          array[row, col] := 1;
        }
        else{
          array[row,col] := 0;
        };
      };
    };
    return array;
  }

  function : PrintMatrix(array : Int[,]) ~ Nil {
    sizes := array->Size();
    for(row:=0; row<sizes[0]; row+=1;){
      for(col:=0; col<sizes[1]; col+=1;){
        value := array[row,col];
        "{$value} \t"->Print();
      };
      '\n'->PrintLine();
    };
  }

  function : Main(args : String[]) ~ Nil {
    PrintMatrix(Matrix(5));
  }
}

```



## OCaml


From the interactive loop (that we call the "toplevel"):


```ocaml
$ ocaml

# let make_id_matrix n =
    let m = Array.make_matrix n n 0.0 in
    for i = 0 to pred n do
      m.(i).(i) <- 1.0
    done;
    (m)
  ;;
val make_id_matrix : int -> float array array = <fun>

# make_id_matrix 4 ;;
- : float array array =
[| [|1.; 0.; 0.; 0.|];
   [|0.; 1.; 0.; 0.|];
   [|0.; 0.; 1.; 0.|];
   [|0.; 0.; 0.; 1.|] |]
```


another way:


```ocaml
# let make_id_matrix n =
    Array.init n (fun i ->
      Array.init n (fun j ->
        if i = j then 1.0 else 0.0))
  ;;
val make_id_matrix : int -> float array array = <fun>

# make_id_matrix 4 ;;
- : float array array =
[| [|1.; 0.; 0.; 0.|];
   [|0.; 1.; 0.; 0.|];
   [|0.; 0.; 1.; 0.|];
   [|0.; 0.; 0.; 1.|] |]
```


When we write a function in the toplevel, it returns us its signature (the prototype), and when we write a variable (or a function call), it returns its type and its value.


## Octave

The '''eye''' function create the identity (I) matrix, e.g.:


```octave
I = eye(10)
```



## ooRexx

ooRexx doesn't have a proper matrix class, but it does have multidimensional arrays.

```ooRexx

say "a 3x3 identity matrix"
say
call printMatrix createIdentityMatrix(3)
say
say "a 5x5 identity matrix"
say
call printMatrix createIdentityMatrix(5)

::routine createIdentityMatrix
  use arg size
  matrix = .array~new(size, size)
  loop i = 1 to size
      loop j = 1 to size
          if i == j then matrix[i, j] = 1
          else matrix[i, j] = 0
      end j
  end i
  return matrix

::routine printMatrix
  use arg matrix

  loop i = 1 to matrix~dimension(1)
      line = ""
      loop j = 1 to matrix~dimension(2)
          line = line matrix[i, j]
      end j
      say line
  end i

```

{{out}}
<pre style="height:20ex;overflow:scroll">
a 3x3 identity matrix

 1 0 0
 0 1 0
 0 0 1

a 5x5 identity matrix

 1 0 0 0 0
 0 1 0 0 0
 0 0 1 0 0
 0 0 0 1 0
 0 0 0 0 1

```



## OxygenBasic


```oxygenbasic

Class SquareMatrix
'
### ===========


  double *Cell
  sys    size

  method SetIdentity()
  indexbase 0
  sys e,i,j
  e=size*size
  for i=0 to <size
    cell(i*size+j)=1 : j++
  next
  end method

  method constructor(sys n)
  @cell=getmemory n*n*sizeof double
  size=n
  end method

  method destructor()
  freememory @cell
  end method

end class

new SquareMatrix M(8)
M.SetIdentity
'...
del M

```



## Pascal


```pascal
program IdentityMatrix(input, output);

var
  matrix: array of array of integer;
  n, i, j: integer;

begin
  write('Size of matrix: ');
  readln(n);
  setlength(matrix, n, n);

  for i := 0 to n - 1 do
    matrix[i,i] := 1;

  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
      write (matrix[i,j], ' ');
    writeln;
  end;
end.
```

{{out}}

```txt

% ./IdentityMatrix
Size of matrix: 5
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

```



## PARI/GP

Built-in:

```parigp
matid(9)
```


Custom:

```parigp
matrix(9,9,i,j,i==j)
```



## Perl


```perl
sub identity_matrix {
    my $n = shift;
    map {
      my $i = $_;
      [ map { ($_ == $i) - 0 } 1 .. $n ]
    } 1 .. $n;
}

@ARGV = (4, 5, 6) unless @ARGV;

for (@ARGV) {
  my @id = identity_matrix $_;
  print "$_:\n";
  for (my $i=0; $i<@id; ++$i) {
    print join ' ', @{$id[$i]}, "\n";
  }
  print "\n";
}

```

{{out}}

```txt
4:
1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1

5:
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

6:
1 0 0 0 0 0
0 1 0 0 0 0
0 0 1 0 0 0
0 0 0 1 0 0
0 0 0 0 1 0
0 0 0 0 0 1

```



## Perl 6

{{works with|rakudo|2015-09-15}}

```perl6
sub identity-matrix($n) {
    my @id;
    for flat ^$n X ^$n -> $i, $j {
        @id[$i][$j] = +($i == $j);
    }
    @id;
}

.say for identity-matrix(5);
```

{{out}}

```txt
[1 0 0 0 0]
[0 1 0 0 0]
[0 0 1 0 0]
[0 0 0 1 0]
[0 0 0 0 1]
```

On the other hand, this may be clearer and/or faster:

```perl6
sub identity-matrix($n) {
    my @id = [0 xx $n] xx $n;
    @id[$_][$_] = 1 for ^$n;
    @id;
}
```


Here is yet an other way to do it:

```perl6
sub identity-matrix($n) {
    [1, |(0 xx $n-1)], *.rotate(-1) ... *[*-1]
}
```



## Phix


```Phix
function identity(integer n)
sequence res = repeat(repeat(0,n),n)
    for i=1 to n do
        res[i][i] = 1
    end for
    return res
end function

ppOpt({pp_Nest,1})
pp(identity(3))
pp(identity(5))
pp(identity(7))
pp(identity(9))
```

{{out}}
<pre style="float:left">
{{1,0,0},
 {0,1,0},
 {0,0,1}}

```

<pre style="float:left">
{{1,0,0,0,0},
 {0,1,0,0,0},
 {0,0,1,0,0},
 {0,0,0,1,0},
 {0,0,0,0,1}}

```

<pre style="float:left">
{{1,0,0,0,0,0,0},
 {0,1,0,0,0,0,0},
 {0,0,1,0,0,0,0},
 {0,0,0,1,0,0,0},
 {0,0,0,0,1,0,0},
 {0,0,0,0,0,1,0},
 {0,0,0,0,0,0,1}}

```


```txt

{{1,0,0,0,0,0,0,0,0},
 {0,1,0,0,0,0,0,0,0},
 {0,0,1,0,0,0,0,0,0},
 {0,0,0,1,0,0,0,0,0},
 {0,0,0,0,1,0,0,0,0},
 {0,0,0,0,0,1,0,0,0},
 {0,0,0,0,0,0,1,0,0},
 {0,0,0,0,0,0,0,1,0},
 {0,0,0,0,0,0,0,0,1}}

```



## PHP


```php

function createMatrix($size)
{
    $result = array();

    for ($i = 0; $i < $size; $i++) {
        $row      = array_fill(0, $size, 0);
        $row[$i]  = 1;
        $result[] = $row;
    }

    return $result;
}

function printMatrix(array $matrix)
{
    foreach ($matrix as $row) {
        foreach ($row as $column) {
            echo $column . " ";
        }
        echo PHP_EOL;
    }
    echo PHP_EOL;
}

printMatrix(createMatrix(5));

```

{{out}}

```txt

1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

```



## PicoLisp


```PicoLisp
(de identity (Size)
   (let L (need Size (1) 0)
      (make
         (do Size
            (link (copy (rot L))) ) ) ) )
```

Test:

```PicoLisp
: (identity 3)
-> ((1 0 0) (0 1 0) (0 0 1))

: (mapc println (identity 5))
(1 0 0 0 0)
(0 1 0 0 0)
(0 0 1 0 0)
(0 0 0 1 0)
(0 0 0 0 1)
```



## PL/I


```PL/I

identity: procedure (A, n);
   declare A(n,n) fixed controlled;
   declare (i,n) fixed binary;
   allocate A; A = 0;
   do i = 1 to n; A(i,i) = 1; end;
end identity;

```



## PostScript


```PostScript

% n  ident  [identity-matrix]
% create an identity matrix of dimension n*n.
% Uses a local dictionary for its one parameter, perhaps overkill.
% Constructs arrays of arrays of integers using [], for loops, and stack manipulation.
/ident { 1 dict begin /n exch def
    [
    1 1 n {                              % [ i
        [ exch                           % [ [ i
        1 1 n {                          % [ [ i j
            1 index eq { 1 }{ 0 } ifelse % [ [ i b
            exch                         % [ [ b i
        } for                            % [ [ b+ i
        pop ]                            % [ [ b+ ]
    } for                                % [ [b+]+ ]
    ]
end } def

```



## PowerShell


```PowerShell

function identity($n) {
    0..($n-1) | foreach{$row = @(0) * $n; $row[$_] = 1; ,$row}
}
function show($a) { $a | foreach{ "$_"} }
$array = identity 4
show $array

```

<b>Output:</b>

```txt

1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1

```


```PowerShell

$array[0][0]
$array[0][1]

```

<b>Output:</b>

```txt

1
0

```



## PureBasic


```purebasic>
Procedure identityMatrix(Array i(2), size) ;valid only for size >= 0
  ;formats array i() as an identity matrix of size x size
  Dim i(size - 1, size - 1)

  Protected j
  For j = 0 To size - 1
    i(j, j) = 1
  Next
EndProcedure


Procedure displayMatrix(Array a(2))
  Protected rows = ArraySize(a(), 2), columns = ArraySize(a(), 1)
  Protected i, j

  For i = 0 To rows
    For j = 0 To columns
      Print(RSet(Str(a(i, j)), 3, " "))
    Next
    PrintN("")
  Next
EndProcedure

If OpenConsole()
  Dim i3(0, 0)
  Dim i4(0, 0)

  identityMatrix(i3(), 3)
  identityMatrix(i4(), 4)

  displayMatrix(i3())
  PrintN("")
  displayMatrix(i4())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
  1  0  0
  0  1  0
  0  0  1

  1  0  0  0
  0  1  0  0
  0  0  1  0
  0  0  0  1
```



## Python


### Nested lists

A simple solution, using nested lists to represent the matrix.

```python
def identity(size):
    matrix = [[0]*size for i in range(size)]
    #matrix = [[0] * size] * size    #Has a flaw. See http://stackoverflow.com/questions/240178/unexpected-feature-in-a-python-list-of-lists

    for i in range(size):
        matrix[i][i] = 1

    for rows in matrix:
        for elements in rows:
            print elements,
        print ""
```



### Nested maps and comprehensions

{{Works with|Python|3.7}}

```python
'''Identity matrices by maps and equivalent list comprehensions'''

import operator


# idMatrix :: Int -> [[Int]]
def idMatrix(n):
    '''Identity matrix of order n,
       expressed as a nested map.
    '''
    eq = curry(operator.eq)
    xs = range(0, n)
    return list(map(
        lambda x: list(map(
            compose(int)(eq(x)),
            xs
        )),
        xs
    ))


# idMatrix3 :: Int -> [[Int]]
def idMatrix2(n):
    '''Identity matrix of order n,
       expressed as a nested comprehension.
    '''
    xs = range(0, n)
    return ([int(x == y) for x in xs] for y in xs)


# TEST ----------------------------------------------------
def main():
    '''
        Identity matrix of dimension five,
        by two different routes.
    '''
    for f in [idMatrix, idMatrix2]:
        print(
            '\n' + f.__name__ + ':',
            '\n\n' + '\n'.join(map(str, f(5))),
        )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
idMatrix:

[1, 0, 0, 0, 0]
[0, 1, 0, 0, 0]
[0, 0, 1, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 0, 0, 1]

idMatrix2:

[1, 0, 0, 0, 0]
[0, 1, 0, 0, 0]
[0, 0, 1, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 0, 0, 1]
```



### Dict of points

A dict of tuples of two ints (x, y) are used to represent the matrix.

```python>>>
 def identity(size):
...     return {(x, y):int(x == y) for x in range(size) for y in range(size)}
...
>>> size = 4
>>> matrix = identity(size)
>>> print('\n'.join(' '.join(str(matrix[(x, y)]) for x in range(size)) for y in range(size)))
1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1
>>>
```



### Numpy

A solution using the numpy library

```python

np.mat(np.eye(size))

```



## R

When passed a single scalar argument, <code>diag</code> produces an identity matrix of size given by the scalar.  For example:


```rsplus
diag(3)
```


produces:


```txt
     [,1] [,2] [,3]
[1,]    1    0    0
[2,]    0    1    0
[3,]    0    0    1
```

Or you can also use the method that is shown below

```rsplus
Identity_matrix=function(size){
  x=matrix(0,size,size)
  for (i in 1:size) {
    x[i,i]=1
  }
  return(x)
}
```



## Racket


```racket

#lang racket
(require math)
(identity-matrix 5)

```

{{out}}

```txt

(array #[#[1 0 0 0 0]
         #[0 1 0 0 0]
         #[0 0 1 0 0]
         #[0 0 0 1 0]
         #[0 0 0 0 1]])

```



## REXX


### version 1

The REXX language doesn't have matrices as such, so the problem is largely how to display the "matrix".

The code to display the matrices was kept as a stand-alone general-purpose (square) matrix display

subroutine,   which, in part,   determines if the square matrix is indeed a square matrix based on the

number of elements given.

It also finds the maximum widths of the integer and decimal fraction parts   (if any)   and uses those widths

to align   (right-justify according to the [possibly implied] decimal point)   the columns of the square matrix.

It also tries to display a centered (and easier to read) matrix,   along with a title.

```rexx
/*REXX program  creates and displays any sized  identity matrix  (centered, with title).*/
           do k=3  to 6                          /* [↓]  build and display a sq. matrix.*/
           call ident_mat  k                     /*build & display a KxK square matrix. */
           end   /*k*/                           /* [↑]  use general─purpose display sub*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ident_mat: procedure;  parse arg n; $=
              do    r=1  for n                   /*build identity matrix, by row and col*/
                 do c=1  for n;     $= $ (r==c)  /*append  zero  or  one  (if on diag). */
                 end   /*c*/
              end      /*r*/
           call showMat  'identity matrix of size'   n,   $
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat: procedure; parse arg hdr,x;  #=words(x) /*#  is the number of matrix elements. */
         dp= 0                                   /*DP:  max width of decimal fractions. */
         w= 0                                    /*W:   max width of integer part.      */
                 do n=1  until n*n>=#;  _= word(x,n)      /*determine the matrix order. */
                 parse var _ y '.' f;   w= max(w, length(y));      dp= max(dp, length(f) )
                 end   /*n*/                     /* [↑]  idiomatically find the widths. */
         w= w +1
         say;  say center(hdr, max(length(hdr)+8, (w+1)*#%n), '─');  say
         #= 0                                                            /*#: element #.*/
                 do   row=1  for n;     _= left('', n+w)                 /*indentation. */
                   do col=1  for n;     #= # + 1                         /*bump element.*/
                   _=_ right(format(word(x, #), , dp)/1, w)
                   end   /*col*/                 /* [↑]  division by unity normalizes #.*/
                 say _                           /*display a single line of the matrix. */
                 end     /*row*/
         return
```

{{out|output|text=  when using the default sizes   (3 ──► 6)   for generating four matrices:}}

```txt

────identity matrix of size 3────

       1  0  0
       0  1  0
       0  0  1

────identity matrix of size 4────

        1  0  0  0
        0  1  0  0
        0  0  1  0
        0  0  0  1

────identity matrix of size 5────

         1  0  0  0  0
         0  1  0  0  0
         0  0  1  0  0
         0  0  0  1  0
         0  0  0  0  1

────identity matrix of size 6────

          1  0  0  0  0  0
          0  1  0  0  0  0
          0  0  1  0  0  0
          0  0  0  1  0  0
          0  0  0  0  1  0
          0  0  0  0  0  1

```



### version 2

An alternative?!

```rexx

/* REXX ***************************************************************
* show identity matrix of size n
* I consider m.i.j to represent the matrix (not needed for showing)
* 06.07.2012 Walter Pachl
**********************************************************************/
Parse Arg n
Say 'Identity Matrix of size' n '(m.i.j IS the Matrix)'
m.=0
Do i=1 To n
  ol=''
  Do j=1 To n
    m.i.j=(i=j)
    ol=ol''format(m.i.j,2) /* or ol=ol (i=j)                         */
    End
  Say ol
  End

```

{{out}}

```txt

Identity Matrix of size 3  (m.i.j IS the Matrix)
 1 0 0
 0 1 0
 0 0 1

```

This could be a 3-dimensional sparse matrix with one element set:

```rexx

m.=0
m.0=1000 /* the matrix' size */
m.4.17.333='Walter'

```



## Ring


```ring

size = 5
im = newlist(size, size)
identityMatrix(size, im)
for r = 1 to size
    for c = 1 to size
        see im[r][c]
    next
    see nl
next

func identityMatrix s, m
     m = newlist(s, s)
     for i = 1 to s
         m[i][i] = 1
     next
     return m

func newlist x, y
     if isstring(x) x=0+x ok
     if isstring(y) y=0+y ok
     alist = list(x)
     for t in alist
         t = list(y)
     next
     return alist

```

Output:

```txt

10000
01000
00100
00010
00001

```



## Ruby


### Using Array


```ruby
def identity(size)
  Array.new(size){|i| Array.new(size){|j| i==j ? 1 : 0}}
end

[4,5,6].each do |size|
  puts size, identity(size).map {|r| r.to_s}, ""
end
```


{{out}}

```txt

4
[1, 0, 0, 0]
[0, 1, 0, 0]
[0, 0, 1, 0]
[0, 0, 0, 1]

5
[1, 0, 0, 0, 0]
[0, 1, 0, 0, 0]
[0, 0, 1, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 0, 0, 1]

6
[1, 0, 0, 0, 0, 0]
[0, 1, 0, 0, 0, 0]
[0, 0, 1, 0, 0, 0]
[0, 0, 0, 1, 0, 0]
[0, 0, 0, 0, 1, 0]
[0, 0, 0, 0, 0, 1]

```


### Using Matrix


```ruby

2.1.1 :001 > require "matrix"
 => true
2.1.1 :002 > Matrix.identity(5)
 => Matrix[[1, 0, 0, 0, 0], [0, 1, 0, 0, 0], [0, 0, 1, 0, 0], [0, 0, 0, 1, 0], [0, 0, 0, 0, 1]]

```



## Run BASIC


```runbasic
' formats array im() of size ims
for ims = 4 to 6

print :print "--- Size: ";ims;" ---"
 Dim im(ims,ims)

 For i = 1 To ims
   im(i,i) = 1
 next

 For row = 1 To ims
   print "[";
   cma$ = ""
     For col = 1 To ims
       print cma$;im(row, col);
       cma$ = ", "
    next
   print "]"
 next
next ims
```

{{out}}

```txt
--- Size: 4 ---
[1, 0, 0, 0]
[0, 1, 0, 0]
[0, 0, 1, 0]
[0, 0, 0, 1]

--- Size: 5 ---
[1, 0, 0, 0, 0]
[0, 1, 0, 0, 0]
[0, 0, 1, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 0, 0, 1]

--- Size: 6 ---
[1, 0, 0, 0, 0, 0]
[0, 1, 0, 0, 0, 0]
[0, 0, 1, 0, 0, 0]
[0, 0, 0, 1, 0, 0]
[0, 0, 0, 0, 1, 0]
[0, 0, 0, 0, 0, 1]
```



## Rust


Run with command-line containing the matrix size.


```rust

extern crate num;
struct Matrix<T> {
    data: Vec<T>,
    size: usize,
}

impl<T> Matrix<T>
where
    T: num::Num + Clone + Copy,
{
    fn new(size: usize) -> Self {
        Self {
            data: vec![T::zero(); size * size],
            size: size,
        }
    }
    fn get(&mut self, x: usize, y: usize) -> T {
        self.data[x + self.size * y]
    }
    fn identity(&mut self) {
        for (i, item) in self.data.iter_mut().enumerate() {
            *item = if i % (self.size + 1) == 0 {
                T::one()
            } else {
                T::zero()
            }
        }
    }
}

fn main() {
    let size = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut matrix = Matrix::<i32>::new(size);
    matrix.identity();
    for y in 0..size {
        for x in 0..size {
            print!("{} ", matrix.get(x, y));
        }
        println!();
    }
}

```



## Scala


```scala
def identityMatrix(n:Int)=Array.tabulate(n,n)((x,y) => if(x==y) 1 else 0)
def printMatrix[T](m:Array[Array[T]])=m map (_.mkString("[", ", ", "]")) mkString "\n"

printMatrix(identityMatrix(5))
```

{{out}}

```txt
[1, 0, 0, 0, 0]
[0, 1, 0, 0, 0]
[0, 0, 1, 0, 0]
[0, 0, 0, 1, 0]
[0, 0, 0, 0, 1]
```



## Scheme

When representing a matrix as a collection of nested lists:

```scheme

(define (identity n)
  (letrec
      ((uvec
	(lambda (m i acc)
	  (if (= i n)
	      acc
	      (uvec m (+ i 1)
		    (cons (if (= i m) 1 0) acc)))))
       (idgen
	(lambda (i acc)
	  (if (= i n)
	      acc
	      (idgen (+ i 1)
		     (cons (uvec i 0 '()) acc))))))
       (idgen 0 '())))

```

Test program:

```scheme

(display (identity 4))

```

{{out}}

```txt

((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: matrix is array array integer;

const func matrix: identity (in integer: size) is func
  result
    var matrix: identity is matrix.value;
  local
    var integer: index is 0;
  begin
    identity := size times size times 0;
    for index range 1 to size do
      identity[index][index] := 1;
    end for;
  end func;

const proc: writeMat (in matrix: a) is func
  local
    var integer: i is 0;
    var integer: num is 0;
  begin
    for key i range a do
      for num range a[i] do
        write(num lpad 2);
      end for;
      writeln;
    end for;
  end func;

const proc: main is func
  begin
    writeMat(identity(6));
  end func;
```


{{out}}

```txt

 1 0 0 0 0 0
 0 1 0 0 0 0
 0 0 1 0 0 0
 0 0 0 1 0 0
 0 0 0 0 1 0
 0 0 0 0 0 1

```



## Sidef


```ruby
func identity_matrix(n) {
    n.of { |i|
        n.of { |j|
            i == j ? 1 : 0
        }
    }
}

for n (ARGV ? ARGV.map{.to_i} : [4, 5, 6]) {
  say "\n#{n}:"
  for row (identity_matrix(n)) {
    say row.join(' ')
  }
}
```


{{out}}

```txt

4:
1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1

5:
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

6:
1 0 0 0 0 0
0 1 0 0 0 0
0 0 1 0 0 0
0 0 0 1 0 0
0 0 0 0 1 0
0 0 0 0 0 1

```



## Sinclair ZX81 BASIC

Works with 1k of RAM, but for a larger matrix you'll want at least 2k.

```zxbasic
 10 INPUT S
 20 DIM M(S,S)
 30 FOR I=1 TO S
 40 LET M(I,I)=1
 50 NEXT I
 60 FOR I=1 TO S
 70 SCROLL
 80 FOR J=1 TO S
 90 PRINT M(I,J);
100 NEXT J
110 PRINT
120 NEXT I
```

{{in}}

```txt
10
```

{{out}}

```txt
1000000000
0100000000
0010000000
0001000000
0000100000
0000010000
0000001000
0000000100
0000000010
0000000001
```



## Smalltalk

{{works with|Pharo Smalltalk}}

```smalltalk
(Array2D identity: (UIManager default request: 'Enter size of the matrix:') asInteger) asString
```

{{Out}}

```txt

'(1 0 0 0
0 1 0 0
0 0 1 0
0 0 0 1 )'

```



## Sparkling


```sparkling
function unitMatrix(n) {
	return map(range(n), function(k1, v1) {
		return map(range(n), function(k2, v2) {
			return v2 == v1 ? 1 : 0;
		});
	});
}
```



## Stata


###  Stata matrix


```stata
. mat a = I(3)
. mat list a

symmetric a[3,3]
    c1  c2  c3
r1   1
r2   0   1
r3   0   0   1
```



###  Mata


```stata
: I(3)
[symmetric]
       1   2   3
    +-------------+
  1 |  1          |
  2 |  0   1      |
  3 |  0   0   1  |
    +-------------+
```




## Swift


{{trans|Elixir}}


```swift
func identityMatrix(size: Int) -> [[Int]] {
  return (0..<size).map({i in
    return (0..<size).map({ $0 == i ? 1 : 0})
  })
}

print(identityMatrix(size: 5))
```


{{out}}


```txt
[[1, 0, 0, 0, 0], [0, 1, 0, 0, 0], [0, 0, 1, 0, 0], [0, 0, 0, 1, 0], [0, 0, 0, 0, 1]]
```



## Tailspin


```tailspin

templates identityMatrix
  def n: $;
  [1..$n -> (def i: $; [1..$n -> (<$i> 1 ! <> 0 !)] !)] !
end identityMatrix

def identity: 5 -> identityMatrix;
$identity... -> '|$(1);$(2..-1)... -> ', $;';|
' -> !OUT::write

```

{{out}}

```txt

|1, 0, 0, 0, 0|
|0, 1, 0, 0, 0|
|0, 0, 1, 0, 0|
|0, 0, 0, 1, 0|
|0, 0, 0, 0, 1|

```



## Tcl

When representing a matrix as a collection of nested lists:

```tcl
proc I {rank {zero 0.0} {one 1.0}} {
    set m [lrepeat $rank [lrepeat $rank $zero]]
    for {set i 0} {$i < $rank} {incr i} {
	lset m $i $i $one
    }
    return $m
}
```

Or alternatively with the help of the tcllib package for rectangular data structures:
{{tcllib|struct::matrix}}

```tcl
package require struct::matrix

proc I {rank {zero 0.0} {one 1.0}} {
    set m [struct::matrix]
    $m add columns $rank
    $m add rows $rank
    for {set i 0} {$i < $rank} {incr i} {
	for {set j 0} {$j < $rank} {incr j} {
	    $m set cell $i $j [expr {$i==$j ? $one : $zero}]
	}
    }
    return $m
}
```

Demonstrating the latter:

```tcl
set m [I 5 0 1]    ;# Integer 0/1 for clarity of presentation
puts [$m format 2string]
```

{{out}}

```txt
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1
```



## TypeScript


```typescript

function identity(n) {
    if (n < 1) return "Not defined";
    else if (n == 1) return 1;
    else {
        var idMatrix:number[][];
        for (var i: number = 0; i < n; i++) {
            for (var j: number = 0; j < n; j++) {
                if (i != j) idMatrix[i][j] = 0;
                else idMatrix[i][j] = 1;
            }
        }
        return idMatrix;
    }
}

```



## Vala


```vala
int main (string[] args) {
	if (args.length < 2) {
		print ("Please, input an integer > 0.\n");
		return 0;
	}
	var n = int.parse (args[1]);
	if (n <= 0) {
		print ("Please, input an integer > 0.\n");
		return 0;
	}
	int[,] array = new int[n, n];
	for (var i = 0; i < n; i ++) {
		for (var j = 0; j < n; j ++) {
			if (i == j) array[i,j] = 1;
			else array[i,j] = 0;
		}
	}
	for (var i = 0; i < n; i ++) {
		for (var j = 0; j < n; j ++) {
			print ("%d ", array[i,j]);
		}
		print ("\b\n");
	}
	return 0;
}
```



## VBA


```vb
Private Function Identity(n As Integer) As Variant
    Dim I() As Integer
    ReDim I(n - 1, n - 1)
    For j = 0 To n - 1
        I(j, j) = 1
    Next j
    Identity = I
End Function
```


## VBScript


```vb

build_matrix(7)

Sub build_matrix(n)
	Dim matrix()
	ReDim matrix(n-1,n-1)
	i = 0
	'populate the matrix
	For row = 0 To n-1
		For col = 0 To n-1
			If col = i Then
				matrix(row,col) = 1
			Else
				matrix(row,col) = 0
			End If
		Next
		i = i + 1
	Next
	'display the matrix
	For row = 0 To n-1
		For col = 0 To n-1
			If col < n-1 Then
				WScript.StdOut.Write matrix(row,col) & " "
			Else
				WScript.StdOut.Write matrix(row,col)
			End If
		Next
		WScript.StdOut.WriteLine
	Next
End Sub

```


{{Out}}

```txt

1 0 0 0 0 0 0
0 1 0 0 0 0 0
0 0 1 0 0 0 0
0 0 0 1 0 0 0
0 0 0 0 1 0 0
0 0 0 0 0 1 0
0 0 0 0 0 0 1

```



'''Alternate version'''


```vbscript

n = 8

arr = Identity(n)

for i = 0 to n-1
    for j = 0 to n-1
        wscript.stdout.Write arr(i,j) & " "
    next
    wscript.stdout.writeline
next

Function Identity (size)
    Execute Replace("dim a(#,#):for i=0 to #:for j=0 to #:a(i,j)=0:next:a(i,i)=1:next","#",size-1)
    Identity = a
End Function

```

{{Out}}

```txt

1 0 0 0 0 0 0 0
0 1 0 0 0 0 0 0
0 0 1 0 0 0 0 0
0 0 0 1 0 0 0 0
0 0 0 0 1 0 0 0
0 0 0 0 0 1 0 0
0 0 0 0 0 0 1 0
0 0 0 0 0 0 0 1

```



## Visual Basic

{{works with|Visual Basic|6}}

```vb
Option Explicit
'------------
Public Function BuildIdentityMatrix(ByVal Size As Long) As Byte()
Dim i As Long
Dim b() As Byte

  Size = Size - 1
  ReDim b(0 To Size, 0 To Size)
  'at this point, the matrix is allocated and
  'all elements are initialized to 0 (zero)
  For i = 0 To Size
    b(i, i) = 1   'set diagonal elements to 1
  Next i
  BuildIdentityMatrix = b

End Function
'------------
Sub IdentityMatrixDemo(ByVal Size As Long)
Dim b() As Byte
Dim i As Long, j As Long

  b() = BuildIdentityMatrix(Size)
  For i = LBound(b(), 1) To UBound(b(), 1)
    For j = LBound(b(), 2) To UBound(b(), 2)
      Debug.Print CStr(b(i, j));
    Next j
  Debug.Print
  Next i

End Sub
'------------
Sub Main()

  IdentityMatrixDemo 5
  Debug.Print
  IdentityMatrixDemo 10

End Sub

```

{{out}}

```txt
10000
01000
00100
00010
00001

1000000000
0100000000
0010000000
0001000000
0000100000
0000010000
0000001000
0000000100
0000000010
0000000001
```



## Wortel


```wortel
@let {
  im ^(%^\@table ^(@+ =) @to)

  !im 4
}
```

Returns:

```txt
[[1 0 0 0]
 [0 1 0 0]
 [0 0 1 0]
 [0 0 0 1]]
```



## XPL0


```XPL0
include c:\cxpl\codes;
def IntSize = 4;                        \number of bytes in an integer
int Matrix, Size, I, J;

[Text(0, "Size: ");  Size:= IntIn(0);
Matrix:= Reserve(Size*IntSize);         \reserve memory for 2D integer array
for I:= 0 to Size-1 do
        Matrix(I):= Reserve(Size*IntSize);
for J:= 0 to Size-1 do                  \make array an identity matrix
    for I:= 0 to Size-1 do
        Matrix(I,J):= if I=J then 1 else 0;
for J:= 0 to Size-1 do                  \display the result
    [for I:= 0 to Size-1 do
        [IntOut(0, Matrix(I,J));  ChOut(0, ^ )];
    CrLf(0);
    ];
]
```


{{out}}

```txt

Size: 5
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1

```



## zkl

Using lists of lists:

```zkl
fcn idMatrix(n){
   m:=(0).pump(n,List.createLong(n).write,0)*n;
   m.apply2(fcn(row,rc){ row[rc.inc()]=1 },Ref(0));
   m
}
idMatrix(5).println();
idMatrix(5).pump(Console.println);
```

{{out}}

```txt

L(L(1,0,0,0,0),L(0,1,0,0,0),L(0,0,1,0,0),L(0,0,0,1,0),L(0,0,0,0,1))
L(1,0,0,0,0)
L(0,1,0,0,0)
L(0,0,1,0,0)
L(0,0,0,1,0)
L(0,0,0,0,1)

```



## ZX Spectrum Basic

{{trans|Applesoft_BASIC}}

```zxbasic
10 INPUT "Matrix size: ";size
20 GO SUB 200: REM Identity matrix
30 FOR r=1 TO size
40 FOR c=1 TO size
50 LET s$=CHR$ 13
60 IF c<size THEN LET s$=" "
70 PRINT i(r,c);s$;
80 NEXT c
90 NEXT r
100 STOP
200 REM Identity matrix size
220 DIM i(size,size)
230 FOR i=1 TO size
240 LET i(i,i)=1
250 NEXT i
260 RETURN
```

