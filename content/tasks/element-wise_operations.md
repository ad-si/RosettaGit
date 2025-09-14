+++
title = "Element-wise operations"
description = ""
date = 2019-06-04T20:56:42Z
aliases = []
[extra]
id = 9896
[taxonomies]
categories = ["task", "Matrices"]
tags = []
languages = [
  "ada",
  "algol_68",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "csharp",
  "d",
  "factor",
  "fortran",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "k",
  "kotlin",
  "maple",
  "matlab",
  "maxima",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "sidef",
  "standard_ml",
  "stata",
  "tcl",
  "zkl",
]
+++

This task is similar to:
::*   [[Matrix multiplication]]
::*   [[Matrix transposition]]


## Task

Implement basic element-wise matrix-matrix and scalar-matrix operations, which can be referred to in other, higher-order tasks.

Implement:
:::*   addition
:::*   subtraction
:::*   multiplication
:::*   division
:::*   exponentiation



Extend the task if necessary to include additional basic operations, which should not require their own specialised task.





## Ada


Using Generics, the task is quite trivial in Ada. Here is the main program:


```Ada
with Ada.Text_IO, Matrix_Scalar;

procedure Scalar_Ops is

   subtype T is Integer range 1 .. 3;

   package M is new Matrix_Scalar(T, T, Integer);

   -- the functions to solve the task
        function "+" is new M.Func("+");
        function "-" is new M.Func("-");
        function "*" is new M.Func("*");
        function "/" is new M.Func("/");
        function "**" is new M.Func("**");
        function "mod" is new M.Func("mod");

   -- for output purposes, we need a Matrix->String conversion
        function Image is new M.Image(Integer'Image);

   A: M.Matrix := ((1,2,3),(4,5,6),(7,8,9)); -- something to begin with

begin
   Ada.Text_IO.Put_Line("  Initial M=" & Image(A));
   Ada.Text_IO.Put_Line("        M+2=" & Image(A+2));
   Ada.Text_IO.Put_Line("        M-2=" & Image(A-2));
   Ada.Text_IO.Put_Line("        M*2=" & Image(A*2));
   Ada.Text_IO.Put_Line("        M/2=" & Image(A/2));
   Ada.Text_IO.Put_Line("  square(M)=" & Image(A ** 2));
   Ada.Text_IO.Put_Line("    M mod 2=" & Image(A mod 2));
   Ada.Text_IO.Put_Line("(M*2) mod 3=" & Image((A*2) mod 3));
end Scalar_Ops;
```


```txt
  Initial M=((1,2,3),(4,5,6),(7,8,9))
        M+2=((3,4,5),(6,7,8),(9,10,11))
        M-2=((-1,0,1),(2,3,4),(5,6,7))
        M*2=((2,4,6),(8,10,12),(14,16,18))
        M/2=((0,1,1),(2,2,3),(3,4,4))
  square(M)=((1,4,9),(16,25,36),(49,64,81))
    M mod 2=((1,0,1),(0,1,0),(1,0,1))
(M*2) mod 3=((2,1,0),(2,1,0),(2,1,0))
```



Our main program uses a generic package Matrix_Scalar. Here is the specification:


```Ada
generic
   type Rows is (<>);
   type Cols is (<>);
   type Num is private;
package Matrix_Scalar is
   type Matrix is array(Rows, Cols) of Num;

   generic
      with function F(L, R: Num) return Num;
   function Func(Left: Matrix; Right: Num) return Matrix;

   generic
      with function Image(N: Num) return String;
   function Image(M: Matrix) return String;

end Matrix_Scalar;
```


And here is the corresponding implementation. Note that the function Image (which we just use to output the results) takes much more lines than the function Func we need for actually solving the task:


```Ada
package body Matrix_Scalar is

   function Func(Left: Matrix; Right: Num) return Matrix is
      Result: Matrix;
   begin
      for R in Rows loop
         for C in Cols loop
            Result(R,C) := F(Left(R,C), Right);
         end loop;
      end loop;
      return Result;
   end Func;

   function Image(M: Matrix) return String is

      function Img(R: Rows) return String is

         function I(C: Cols) return String is
            S: String := Image(M(R,C));
            L: Positive := S'First;
         begin
            while S(L) = ' ' loop
               L := L + 1;
            end loop;
            if C=Cols'Last then
               return S(L .. S'Last);
            else
               return S(L .. S'Last) & "," & I(Cols'Succ(C));
            end if;
         end I;

         Column: String := I(Cols'First);
      begin
         if R=Rows'Last then
            return "(" & Column & ")";
         else
            return "(" & Column & ")," & Img(Rows'Succ(R));
         end if;
      end Img;

   begin
      return("(" & Img(Rows'First) & ")");
   end Image;

end Matrix_Scalar;
```



## ALGOL 68

{{trans|D}} Note: This specimen retains the original [[#D|D]] coding style.
```algol68
#!/usr/local/bin/a68g --script #

MODE SCALAR = REAL;
FORMAT scalar fmt = $g(0, 2)$;

MODE MATRIX = [3, 3]SCALAR;
FORMAT vector fmt = $"("n(2 UPB LOC MATRIX - 2 LWB LOC MATRIX)(f(scalar fmt)", ")f(scalar fmt)")"$;
FORMAT matrix fmt = $"("n(1 UPB LOC MATRIX - 1 LWB LOC MATRIX)(f(vector fmt)","l" ")f(vector fmt)")"$;

PROC elementwise op = (PROC(SCALAR, SCALAR)SCALAR op, MATRIX a, UNION(SCALAR, MATRIX) b)MATRIX: (
  [LWB a:UPB a, 2 LWB a:2 UPB a]SCALAR out;
  CASE b IN
  (SCALAR b):
    FOR i FROM LWB out TO UPB out DO
      FOR j FROM 2 LWB out TO 2 UPB out DO
        out[i, j]:=op(a[i, j], b)
      OD
    OD,
  (MATRIX b):
    FOR i FROM LWB out TO UPB out DO
      FOR j FROM 2 LWB out TO 2 UPB out DO
        out[i, j]:=op(a[i, j], b[i, j])
      OD
    OD
  ESAC;
  out
);

PROC plus  = (SCALAR a, b)SCALAR: a+b,
     minus = (SCALAR a, b)SCALAR: a-b,
     times = (SCALAR a, b)SCALAR: a*b,
     div   = (SCALAR a, b)SCALAR: a/b,
     pow   = (SCALAR a, b)SCALAR: a**b;

main:(
    SCALAR scalar := 10;
    MATRIX matrix = (( 7, 11, 13),
                     (17, 19, 23),
                     (29, 31, 37));

    printf(($f(matrix fmt)";"l$,
      elementwise op(plus,  matrix, scalar),
      elementwise op(minus, matrix, scalar),
      elementwise op(times, matrix, scalar),
      elementwise op(div,   matrix, scalar),
      elementwise op(pow,   matrix, scalar),

      elementwise op(plus,  matrix, matrix),
      elementwise op(minus, matrix, matrix),
      elementwise op(times, matrix, matrix),
      elementwise op(div,   matrix, matrix),
      elementwise op(pow,   matrix, matrix)
  ))
)
```

```txt

((17.00, 21.00, 23.00),
 (27.00, 29.00, 33.00),
 (39.00, 41.00, 47.00));
((-3.00, 1.00, 3.00),
 (7.00, 9.00, 13.00),
 (19.00, 21.00, 27.00));
((70.00, 110.00, 130.00),
 (170.00, 190.00, 230.00),
 (290.00, 310.00, 370.00));
((.70, 1.10, 1.30),
 (1.70, 1.90, 2.30),
 (2.90, 3.10, 3.70));
((282475249.00, 25937424601.00, 137858491849.00),
 (2015993900449.00, 6131066257800.99, 41426511213648.90),
 (420707233300200.00, 819628286980799.00, 4808584372417840.00));
((14.00, 22.00, 26.00),
 (34.00, 38.00, 46.00),
 (58.00, 62.00, 74.00));
((.00, .00, .00),
 (.00, .00, .00),
 (.00, .00, .00));
((49.00, 121.00, 169.00),
 (289.00, 361.00, 529.00),
 (841.00, 961.00, 1369.00));
((1.00, 1.00, 1.00),
 (1.00, 1.00, 1.00),
 (1.00, 1.00, 1.00));
((823543.00, 285311670611.00, 302875106592253.00),
 (827240261886340000000.00, 1978419655660300000000000.00, 20880467999847700000000000000000.00),
 (2567686153161210000000000000000000000000000.00, 17069174130723200000000000000000000000000000000.00, 10555134955777600000000000000000000000000000000000000000000.00));

```



## BBC BASIC

All except exponentiation (^) are native operations in BBC BASIC.

```bbcbasic
      DIM a(1,2), b(1,2), c(1,2)
      a() = 7, 8, 7, 4, 0, 9 : b() = 4, 5, 1, 6, 2, 1

      REM Matrix-Matrix:
      c() = a() + b() : PRINT FNshowmm(a(), "+", b(), c())
      c() = a() - b() : PRINT FNshowmm(a(), "-", b(), c())
      c() = a() * b() : PRINT FNshowmm(a(), "*", b(), c())
      c() = a() / b() : PRINT FNshowmm(a(), "/", b(), c())
      PROCpowmm(a(), b(), c()) : PRINT FNshowmm(a(), "^", b(), c()) '

      REM Matrix-Scalar:
      c() = a() + 3 : PRINT FNshowms(a(), "+", 3, c())
      c() = a() - 3 : PRINT FNshowms(a(), "-", 3, c())
      c() = a() * 3 : PRINT FNshowms(a(), "*", 3, c())
      c() = a() / 3 : PRINT FNshowms(a(), "/", 3, c())
      PROCpowms(a(), 3, c()) : PRINT FNshowms(a(), "^", 3, c())
      END

      DEF PROCpowmm(a(), b(), c())
      LOCAL i%, j%
      FOR i% = 0 TO DIM(a(),1)
        FOR j% = 0 TO DIM(a(),2)
          c(i%,j%) = a(i%,j%) ^ b(i%,j%)
        NEXT
      NEXT
      ENDPROC

      DEF PROCpowms(a(), b, c())
      LOCAL i%, j%
      FOR i% = 0 TO DIM(a(),1)
        FOR j% = 0 TO DIM(a(),2)
          c(i%,j%) = a(i%,j%) ^ b
        NEXT
      NEXT
      ENDPROC

      DEF FNshowmm(a(), op$, b(), c())
      = FNlist(a()) + " " + op$ + " " + FNlist(b()) + " = " + FNlist(c())

      DEF FNshowms(a(), op$, b, c())
      = FNlist(a()) + " " + op$ + " " + STR$(b) + " = " + FNlist(c())

      DEF FNlist(a())
      LOCAL i%, j%, a$
      a$ = "["
      FOR i% = 0 TO DIM(a(),1)
        a$ += "["
        FOR j% = 0 TO DIM(a(),2)
          a$ += STR$(a(i%,j%)) + ", "
        NEXT
        a$ = LEFT$(LEFT$(a$)) + "]"
      NEXT
      = a$ + "]"
```

```txt

[[7, 8, 7][4, 0, 9]] + [[4, 5, 1][6, 2, 1]] = [[11, 13, 8][10, 2, 10]]
[[7, 8, 7][4, 0, 9]] - [[4, 5, 1][6, 2, 1]] = [[3, 3, 6][-2, -2, 8]]
[[7, 8, 7][4, 0, 9]] * [[4, 5, 1][6, 2, 1]] = [[28, 40, 7][24, 0, 9]]
[[7, 8, 7][4, 0, 9]] / [[4, 5, 1][6, 2, 1]] = [[1.75, 1.6, 7][0.666666667, 0, 9]]
[[7, 8, 7][4, 0, 9]] ^ [[4, 5, 1][6, 2, 1]] = [[2401, 32768, 7][4096, 0, 9]]

[[7, 8, 7][4, 0, 9]] + 3 = [[10, 11, 10][7, 3, 12]]
[[7, 8, 7][4, 0, 9]] - 3 = [[4, 5, 4][1, -3, 6]]
[[7, 8, 7][4, 0, 9]] * 3 = [[21, 24, 21][12, 0, 27]]
[[7, 8, 7][4, 0, 9]] / 3 = [[2.33333333, 2.66666667, 2.33333333][1.33333333, 0, 3]]
[[7, 8, 7][4, 0, 9]] ^ 3 = [[343, 512, 343][64, 0, 729]]

```



## C

Matrices are 2D double arrays.

```c
#include <math.h>

#define for_i for(i = 0; i < h; i++)
#define for_j for(j = 0; j < w; j++)
#define _M double**
#define OPM(name, _op_) \
	void eop_##name(_M a, _M b, _M c, int w, int h){int i,j;\
		for_i for_j c[i][j] = a[i][j] _op_ b[i][j];}
OPM(add, +);OPM(sub, -);OPM(mul, *);OPM(div, /);

#define OPS(name, res) \
	void eop_s_##name(_M a, double s, _M b, int w, int h) {double x;int i,j;\
		for_i for_j {x = a[i][j]; b[i][j] = res;}}
OPS(mul, x*s);OPS(div, x/s);OPS(add, x+s);OPS(sub, x-s);OPS(pow, pow(x, s));
```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

public static class ElementWiseOperations
{
    private static readonly Dictionary<string, Func<double, double, double>> operations =
        new Dictionary<string, Func<double, double, double>> {
            { "add", (a, b) => a + b },
            { "sub", (a, b) => a - b },
            { "mul", (a, b) => a * b },
            { "div", (a, b) => a / b },
            { "pow", (a, b) => Math.Pow(a, b) }
        };

    private static readonly Func<double, double, double> nothing = (a, b) => a;

    public static double[,] DoOperation(this double[,] m, string name, double[,] other) =>
        DoOperation(m, operations.TryGetValue(name, out var operation) ? operation : nothing, other);

    public static double[,] DoOperation(this double[,] m, Func<double, double, double> operation, double[,] other) {
        if (m == null || other == null) throw new ArgumentNullException();
        int rows = m.GetLength(0), columns = m.GetLength(1);
        if (rows != other.GetLength(0) || columns != other.GetLength(1)) {
            throw new ArgumentException("Matrices have different dimensions.");
        }

        double[,] result = new double[rows, columns];
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < columns; c++) {
                result[r, c] = operation(m[r, c], other[r, c]);
            }
        }
        return result;
    }

    public static double[,] DoOperation(this double[,] m, string name, double number) =>
        DoOperation(m, operations.TryGetValue(name, out var operation) ? operation : nothing, number);

    public static double[,] DoOperation(this double[,] m, Func<double, double, double> operation, double number) {
        if (m == null) throw new ArgumentNullException();
        int rows = m.GetLength(0), columns = m.GetLength(1);
        double[,] result = new double[rows, columns];
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < columns; c++) {
                result[r, c] = operation(m[r, c], number);
            }
        }
        return result;
    }

    public static void Print(this double[,] m) {
        if (m == null) throw new ArgumentNullException();
        int rows = m.GetLength(0), columns = m.GetLength(1);
        for (int r = 0; r < rows; r++) {
            Console.WriteLine("[ " + string.Join(", ", Enumerable.Range(0, columns).Select(c => m[r, c])) + " ]");
        }
    }

}

public class Program
{
    public static void Main() {
        double[,] matrix = {
            { 1, 2, 3, 4 },
            { 5, 6, 7, 8 },
            { 9, 10, 11, 12 }
        };

        double[,] tens = {
            { 10, 10, 10, 10 },
            { 20, 20, 20, 20 },
            { 30, 30, 30, 30 }
        };

        matrix.Print();
        WriteLine();

        (matrix = matrix.DoOperation("add", tens)).Print();
        WriteLine();

        matrix.DoOperation((a, b) => b - a, 100).Print();
    }
}
```

```txt

[ 1, 2, 3, 4 ]
[ 5, 6, 7, 8 ]
[ 9, 10, 11, 12 ]

[ 11, 12, 13, 14 ]
[ 25, 26, 27, 28 ]
[ 39, 40, 41, 42 ]

[ 89, 88, 87, 86 ]
[ 75, 74, 73, 72 ]
[ 61, 60, 59, 58 ]
```



## Clojure

This function is for vector matrices; for list matrices, change the (vector?) function to the (list?) function and remove all the (vec) functions.

```clojure
(defn initial-mtx [i1 i2 value]
  (vec (repeat i1 (vec (repeat i2 value)))))

(defn operation [f mtx1 mtx2]
  (if (vector? mtx1)
    (vec (map #(vec (map f %1 %2)) mtx1 mtx2)))
    (recur f (initial-mtx (count mtx2) (count (first mtx2)) mtx1) mtx2)
  ))
```

The mtx1 argument can either be a matrix or scalar; the function will sort the difference.


## Common Lisp

Element-wise matrix-matrix operations. Matrices are represented as 2D-arrays.

```lisp
(defun element-wise-matrix (fn A B)
  (let* ((len (array-total-size A))
         (m   (car (array-dimensions A)))
         (n   (cadr (array-dimensions A)))
         (C   (make-array `(,m ,n) :initial-element 0.0d0)))

    (loop for i from 0 to (1- len) do
         (setf (row-major-aref C i)
               (funcall fn
                        (row-major-aref A i)
                        (row-major-aref B i))))
    C))

;; A.+B, A.-B, A.*B, A./B, A.^B.
(defun m+ (A B) (element-wise-matrix #'+    A B))
(defun m- (A B) (element-wise-matrix #'-    A B))
(defun m* (A B) (element-wise-matrix #'*    A B))
(defun m/ (A B) (element-wise-matrix #'/    A B))
(defun m^ (A B) (element-wise-matrix #'expt A B))
```


Elementwise scalar-matrix operations.

```lisp
(defun element-wise-scalar (fn A c)
  (let* ((len (array-total-size A))
         (m   (car (array-dimensions A)))
         (n   (cadr (array-dimensions A)))
         (B   (make-array `(,m ,n) :initial-element 0.0d0)))

    (loop for i from 0 to (1- len) do
         (setf (row-major-aref B i)
               (funcall fn
                        (row-major-aref A i)
                        c)))
    B))

;; c.+A, A.-c, c.*A, A./c, A.^c.
(defun .+ (c A) (element-wise-scalar #'+    A c))
(defun .- (A c) (element-wise-scalar #'-    A c))
(defun .* (c A) (element-wise-scalar #'*    A c))
(defun ./ (A c) (element-wise-scalar #'/    A c))
(defun .^ (A c) (element-wise-scalar #'expt A c))
```



## D


```d
import std.stdio, std.typetuple, std.traits;

T[][] elementwise(string op, T, U)(in T[][] A, in U B) {
  auto R = new typeof(return)(A.length, A[0].length);
  foreach (r, row; A)
    R[r][] = mixin("row[] " ~ op ~ (isNumeric!U ? "B" : "B[r][]"));
  return R;
}

void main() {
  const M = [[3, 5, 7], [1, 2, 3], [2, 4, 6]];
  foreach (op; TypeTuple!("+", "-", "*", "/", "^^"))
    writefln("%s:\n[%([%(%d, %)],\n %)]]\n\n[%([%(%d, %)],\n %)]]\n",
             op, elementwise!op(M, 2), elementwise!op(M, M));
}
```

```txt
+:
[[5, 7, 9],
 [3, 4, 5],
 [4, 6, 8]]

[[6, 10, 14],
 [2, 4, 6],
 [4, 8, 12]]

-:
[[1, 3, 5],
 [-1, 0, 1],
 [0, 2, 4]]

[[0, 0, 0],
 [0, 0, 0],
 [0, 0, 0]]

*:
[[6, 10, 14],
 [2, 4, 6],
 [4, 8, 12]]

[[9, 25, 49],
 [1, 4, 9],
 [4, 16, 36]]

/:
[[1, 2, 3],
 [0, 1, 1],
 [1, 2, 3]]

[[1, 1, 1],
 [1, 1, 1],
 [1, 1, 1]]

^^:
[[9, 25, 49],
 [1, 4, 9],
 [4, 16, 36]]

[[27, 3125, 823543],
 [1, 4, 27],
 [4, 256, 46656]]
```


This alternative version offers more guarantees, same output:

```d
import std.stdio, std.typetuple, std.traits;

T[][] elementwise(string op, T, U)(in T[][] A, in U B)
@safe pure nothrow
if (isNumeric!U || (isArray!U && isArray!(ForeachType!U) &&
    isNumeric!(ForeachType!(ForeachType!U)))) {
    static if (!isNumeric!U)
        assert(A.length == B.length);
    if (!A.length)
        return null;
    auto R = new typeof(return)(A.length, A[0].length);

    foreach (immutable r, const row; A)
        static if (isNumeric!U) {
            R[r][] = mixin("row[] " ~ op ~ "B");
        } else {
            assert(row.length == B[r].length);
            R[r][] = mixin("row[] " ~ op ~ "B[r][]");
        }

    return R;
}

void main() {
    enum scalar = 2;
    enum matFormat = "[%([%(%d, %)],\n %)]]\n";
    immutable matrix = [[3, 5, 7],
                        [1, 2, 3],
                        [2, 4, 6]];

    foreach (immutable op; TypeTuple!("+", "-", "*", "/", "^^")) {
        writeln(op, ":");
        writefln(matFormat, elementwise!op(matrix, scalar));
        writefln(matFormat, elementwise!op(matrix, matrix));
    }
}
```



## Factor

The <code>math.matrices</code> vocabulary provides matrix-matrix and matrix-scalar arithmetic words. I wasn't able to find any for exponentiation, so I wrote them.

```factor
USING: combinators.extras formatting kernel math.functions
math.matrices math.vectors prettyprint sequences ;

: show ( a b words -- )
    [
        3dup execute( x x -- x ) [ unparse ] dip
        "%u %u %s = %u\n" printf
    ] 2with each ; inline

: m^n ( m n -- m ) [ ^ ] curry matrix-map ;
: m^  ( m m -- m ) [ v^ ] 2map ;

{ { 1 2 } { 3 4 } } { { 5 6 } { 7 8 } } { m+ m- m* m/ m^ }
{ { -1 9 4 } { 5 -13 0 } } 3 { m+n m-n m*n m/n m^n }
[ show ] 3bi@
```

```txt

{ { 1 2 } { 3 4 } } { { 5 6 } { 7 8 } } m+ = { { 6 8 } { 10 12 } }
{ { 1 2 } { 3 4 } } { { 5 6 } { 7 8 } } m- = { { -4 -4 } { -4 -4 } }
{ { 1 2 } { 3 4 } } { { 5 6 } { 7 8 } } m* = { { 5 12 } { 21 32 } }
{ { 1 2 } { 3 4 } } { { 5 6 } { 7 8 } } m/ = { { 1/5 1/3 } { 3/7 1/2 } }
{ { 1 2 } { 3 4 } } { { 5 6 } { 7 8 } } m^ = { { 1 64 } { 2187 65536 } }
{ { -1 9 4 } { 5 -13 0 } } 3 m+n = { { 2 12 7 } { 8 -10 3 } }
{ { -1 9 4 } { 5 -13 0 } } 3 m-n = { { -4 6 1 } { 2 -16 -3 } }
{ { -1 9 4 } { 5 -13 0 } } 3 m*n = { { -3 27 12 } { 15 -39 0 } }
{ { -1 9 4 } { 5 -13 0 } } 3 m/n = { { -1/3 3 1+1/3 } { 1+2/3 -4-1/3 0 } }
{ { -1 9 4 } { 5 -13 0 } } 3 m^n = { { -1 729 64 } { 125 -2197 0 } }

```



## Fortran


All element based operations are suported by default in Fortran(90+)


```Fortran

program element_operations
  implicit none

  real(kind=4), dimension(3,3) :: a,b
  integer :: i

  a=reshape([(i,i=1,9)],shape(a))

  print*,'addition'
  b=a+a
  call print_arr(b)

  print*,'multiplication'
  b=a*a
  call print_arr(b)

  print*,'division'
  b=a/b
  call print_arr(b)

  print*,'exponentiation'
  b=a**a
  call print_arr(b)

  print*,'trignometric'
  b=cos(a)
  call print_arr(b)

  print*,'mod'
  b=mod(int(a),3)
  call print_arr(b)

  print*,'element selection'
  b=0
  where(a>3) b=1
  call print_arr(b)

  print*,'elemental functions can be applied to single values:'
  print*,square(3.0)
  print*,'or element wise to arrays:'
  b=square(a)
  call print_arr(b)


contains

  elemental real function square(a)
    real, intent(in) :: a
    square=a*a
  end function square

  subroutine print_arr(arr)
    real, intent(in) :: arr(:,:)
    integer :: i
    do i=1,size(arr,dim=2)
       print*,arr(:,i)
    end do
  end subroutine print_arr


end program element_operations

```


```txt

 addition
   2.00000000       4.00000000       6.00000000
   8.00000000       10.0000000       12.0000000
   14.0000000       16.0000000       18.0000000
 multiplication
   1.00000000       4.00000000       9.00000000
   16.0000000       25.0000000       36.0000000
   49.0000000       64.0000000       81.0000000
 division
   1.00000000      0.500000000      0.333333343
  0.250000000      0.200000003      0.166666672
  0.142857149      0.125000000      0.111111112
 exponentiation
   1.00000000       4.00000000       27.0000000
   256.000000       3125.00000       46656.0000
   823543.000       16777216.0       387420480.
 trignometric
  0.540302277     -0.416146845     -0.989992499
 -0.653643608      0.283662200      0.960170269
  0.753902256     -0.145500034     -0.911130250
 mod
   1.00000000       2.00000000       0.00000000
   1.00000000       2.00000000       0.00000000
   1.00000000       2.00000000       0.00000000
 element selection
   0.00000000       0.00000000       0.00000000
   1.00000000       1.00000000       1.00000000
   1.00000000       1.00000000       1.00000000
 elemental functions can be applied to single values:
   9.00000000
 or element wise to arrays:
   1.00000000       4.00000000       9.00000000
   16.0000000       25.0000000       36.0000000
   49.0000000       64.0000000       81.0000000


```



## Go

A package, which can be referred to in other, higher-order tasks.

```go
package element

import (
    "fmt"
    "math"
)

type Matrix struct {
    ele    []float64
    stride int
}

func MatrixFromRows(rows [][]float64) Matrix {
    if len(rows) == 0 {
        return Matrix{nil, 0}
    }
    m := Matrix{make([]float64, len(rows)*len(rows[0])), len(rows[0])}
    for rx, row := range rows {
        copy(m.ele[rx*m.stride:(rx+1)*m.stride], row)
    }
    return m
}

func like(m Matrix) Matrix {
    return Matrix{make([]float64, len(m.ele)), m.stride}
}

func (m Matrix) String() string {
    s := ""
    for e := 0; e < len(m.ele); e += m.stride {
        s += fmt.Sprintf("%6.3f \n", m.ele[e:e+m.stride])
    }
    return s
}

type binaryFunc64 func(float64, float64) float64

func elementWiseMM(m1, m2 Matrix, f binaryFunc64) Matrix {
    z := like(m1)
    for i, m1e := range m1.ele {
        z.ele[i] = f(m1e, m2.ele[i])
    }
    return z
}

func elementWiseMS(m Matrix, s float64, f binaryFunc64) Matrix {
    z := like(m)
    for i, e := range m.ele {
        z.ele[i] = f(e, s)
    }
    return z
}

func add(a, b float64) float64 { return a + b }
func sub(a, b float64) float64 { return a - b }
func mul(a, b float64) float64 { return a * b }
func div(a, b float64) float64 { return a / b }
func exp(a, b float64) float64 { return math.Pow(a, b) }

func AddMatrix(m1, m2 Matrix) Matrix { return elementWiseMM(m1, m2, add) }
func SubMatrix(m1, m2 Matrix) Matrix { return elementWiseMM(m1, m2, sub) }
func MulMatrix(m1, m2 Matrix) Matrix { return elementWiseMM(m1, m2, mul) }
func DivMatrix(m1, m2 Matrix) Matrix { return elementWiseMM(m1, m2, div) }
func ExpMatrix(m1, m2 Matrix) Matrix { return elementWiseMM(m1, m2, exp) }

func AddScalar(m Matrix, s float64) Matrix { return elementWiseMS(m, s, add) }
func SubScalar(m Matrix, s float64) Matrix { return elementWiseMS(m, s, sub) }
func MulScalar(m Matrix, s float64) Matrix { return elementWiseMS(m, s, mul) }
func DivScalar(m Matrix, s float64) Matrix { return elementWiseMS(m, s, div) }
func ExpScalar(m Matrix, s float64) Matrix { return elementWiseMS(m, s, exp) }
```

Package use:

```go
package main

import (
    "fmt"

    "element"
)

func h(heading string, m element.Matrix) {
    fmt.Println(heading)
    fmt.Print(m)
}

func main() {
    m1 := element.MatrixFromRows([][]float64{{3, 1, 4}, {1, 5, 9}})
    m2 := element.MatrixFromRows([][]float64{{2, 7, 1}, {8, 2, 8}})
    h("m1:", m1)
    h("m2:", m2)
    fmt.Println()
    h("m1 + m2:", element.AddMatrix(m1, m2))
    h("m1 - m2:", element.SubMatrix(m1, m2))
    h("m1 * m2:", element.MulMatrix(m1, m2))
    h("m1 / m2:", element.DivMatrix(m1, m2))
    h("m1 ^ m2:", element.ExpMatrix(m1, m2))
    fmt.Println()
    s := .5
    fmt.Println("s:", s)
    h("m1 + s:", element.AddScalar(m1, s))
    h("m1 - s:", element.SubScalar(m1, s))
    h("m1 * s:", element.MulScalar(m1, s))
    h("m1 / s:", element.DivScalar(m1, s))
    h("m1 ^ s:", element.ExpScalar(m1, s))
}
```

```txt

m1:
[ 3.000  1.000  4.000]
[ 1.000  5.000  9.000]
m2:
[ 2.000  7.000  1.000]
[ 8.000  2.000  8.000]

m1 + m2:
[ 5.000  8.000  5.000]
[ 9.000  7.000 17.000]
m1 - m2:
[ 1.000 -6.000  3.000]
[-7.000  3.000  1.000]
m1 * m2:
[ 6.000  7.000  4.000]
[ 8.000 10.000 72.000]
m1 / m2:
[ 1.500  0.143  4.000]
[ 0.125  2.500  1.125]
m1 ^ m2:
[ 9.000  1.000  4.000]
[ 1.000 25.000 43046721.000]

s: 0.5
m1 + s:
[ 3.500  1.500  4.500]
[ 1.500  5.500  9.500]
m1 - s:
[ 2.500  0.500  3.500]
[ 0.500  4.500  8.500]
m1 * s:
[ 1.500  0.500  2.000]
[ 0.500  2.500  4.500]
m1 / s:
[ 6.000  2.000  8.000]
[ 2.000 10.000 18.000]
m1 ^ s:
[ 1.732  1.000  2.000]
[ 1.000  2.236  3.000]

```



## Groovy

Solution:

```groovy
class NaiveMatrix {

    List<List<Number>> contents = []

    NaiveMatrix(Iterable<Iterable<Number>> elements) {
        contents.addAll(elements.collect{ row -> row.collect{ cell -> cell } })
        assertWellFormed()
    }

    void assertWellFormed() {
        assert contents != null
        assert contents.size() > 0
        def nCols = contents[0].size()
        assert nCols > 0
        assert contents.every { it != null && it.size() == nCols }
    }

    Map getOrder() { [r: contents.size() , c: contents[0].size()] }

    void assertConformable(NaiveMatrix that) { assert this.order == that.order }

    NaiveMatrix unaryOp(Closure op) {
        new NaiveMatrix(contents.collect{ row -> row.collect{ cell -> op(cell) } } )
    }
    NaiveMatrix binaryOp(NaiveMatrix m, Closure op) {
        assertConformable(m)
        new NaiveMatrix(
            (0..<(this.order.r)).collect{ i ->
                (0..<(this.order.c)).collect{ j -> op(this.contents[i][j],m.contents[i][j]) }
            }
        )
    }
    NaiveMatrix binaryOp(Number n, Closure op) {
        assert n != null
        new NaiveMatrix(contents.collect{ row -> row.collect{ cell -> op(cell,n) } } )
    }

    def plus = this.&binaryOp.rcurry { a, b -> a+b }

    def minus = this.&binaryOp.rcurry { a, b -> a-b }

    def multiply = this.&binaryOp.rcurry { a, b -> a*b }

    def div = this.&binaryOp.rcurry { a, b -> a/b }

    def mod = this.&binaryOp.rcurry { a, b -> a%b }

    def power = this.&binaryOp.rcurry { a, b -> a**b }

    def negative = this.&unaryOp.curry { - it }

    def recip = this.&unaryOp.curry { 1/it }

    String toString() {
        contents.toString()
    }

    boolean equals(Object other) {
        if (other == null || ! other instanceof NaiveMatrix) return false
        def that = other as NaiveMatrix
        this.contents == that.contents
    }

    int hashCode() {
        contents.hashCode()
    }
}
```

The following ''NaiveMatrixCategory'' class allows for modification of regular ''Number'' behavior when interacting with ''NaiveMatrix''.

```groovy
import org.codehaus.groovy.runtime.DefaultGroovyMethods

class NaiveMatrixCategory {
   static NaiveMatrix plus (Number a, NaiveMatrix b) { b + a }
   static NaiveMatrix minus (Number a, NaiveMatrix b) { -b + a }
   static NaiveMatrix multiply (Number a, NaiveMatrix b) { b * a }
   static NaiveMatrix div (Number a, NaiveMatrix b) { a * b.recip() }
   static NaiveMatrix power (Number a, NaiveMatrix b) { b.binaryOp(a) { elt, scalar -> scalar ** elt } }
   static NaiveMatrix mod (Number a, NaiveMatrix b) { b.binaryOp(a) { elt, scalar -> scalar % elt } }

   static <T> T asType (Number a, Class<T> type) {
       type == NaiveMatrix \
           ? [[a]] as NaiveMatrix
           : DefaultGroovyMethods.asType(a, type)
   }
}
```

Test:

```groovy
Number.metaClass.mixin NaiveMatrixCategory

println 'Demo 1: functionality as requested'
def a = [[5,3],[4,2]] as NaiveMatrix
println 'a == ' + a
def b = new NaiveMatrix([[1,2],[7,8]])
println 'b == ' + b

def z = [[0,0],[0,0]] as NaiveMatrix
println "a + b  == (${a}) + (${b})  == " + (a + b)
println "a - b  == (${a}) - (${b})  == " + (a - b)
println "a * b  == (${a}) * (${b})  == " + (a * b)
println "a / b  == (${a}) / (${b})  == " + (a / b)
println "a ** b == (${a}) ** (${b}) == " + (a ** b)

println '\nDemo 2: Extended functionality'
println "a % b  == (${a}) % (${b})  == " + (a % b)

println '\nDemo 3: Element-wise scalar operations'

println "2 + b  == 2 + (${b})  == " + (2 + b)
println "2 - b  == 2 - (${b})  == " + (2 - b)
println "2 * b  == 2 * (${b})  == " + (2 * b)
println "2 / b  == 2 / (${b})  == " + (2 / b)
println "2 ** b == 2 ** (${b}) == " + (2 ** b)
println "2 % b  == 2 % (${b})  == " + (2 % b)

println "\na + 2  == (${a}) + 2  == " + (a + 2)
println "a - 2  == (${a}) - 2  == " + (a - 2)
println "a * 2  == (${a}) * 2  == " + (a * 2)
println "a / 2  == (${a}) / 2  == " + (a / 2)
println "a ** 2 == (${a}) ** 2 == " + (a ** 2)
println "a % 2  == (${a}) % 2  == " + (a % 2)
```

Output:

```txt
Demo 1: functionality as requested
a == [[5, 3], [4, 2]]
b == [[1, 2], [7, 8]]
a + b  == ([[5, 3], [4, 2]]) + ([[1, 2], [7, 8]])  == [[6, 5], [11, 10]]
a - b  == ([[5, 3], [4, 2]]) - ([[1, 2], [7, 8]])  == [[4, 1], [-3, -6]]
a * b  == ([[5, 3], [4, 2]]) * ([[1, 2], [7, 8]])  == [[5, 6], [28, 16]]
a / b  == ([[5, 3], [4, 2]]) / ([[1, 2], [7, 8]])  == [[5, 1.5], [0.5714285714, 0.25]]
a ** b == ([[5, 3], [4, 2]]) ** ([[1, 2], [7, 8]]) == [[5, 9], [16384, 256]]

Demo 2: Extended functionality
a % b  == ([[5, 3], [4, 2]]) % ([[1, 2], [7, 8]])  == [[0, 1], [4, 2]]

Demo 3: Element-wise scalar operations
2 + b  == 2 + ([[1, 2], [7, 8]])  == [[3, 4], [9, 10]]
2 - b  == 2 - ([[1, 2], [7, 8]])  == [[1, 0], [-5, -6]]
2 * b  == 2 * ([[1, 2], [7, 8]])  == [[2, 4], [14, 16]]
2 / b  == 2 / ([[1, 2], [7, 8]])  == [[2, 1.0], [0.2857142858, 0.250]]
2 ** b == 2 ** ([[1, 2], [7, 8]]) == [[2, 4], [128, 256]]
2 % b  == 2 % ([[1, 2], [7, 8]])  == [[0, 0], [2, 2]]

a + 2  == ([[5, 3], [4, 2]]) + 2  == [[7, 5], [6, 4]]
a - 2  == ([[5, 3], [4, 2]]) - 2  == [[3, 1], [2, 0]]
a * 2  == ([[5, 3], [4, 2]]) * 2  == [[10, 6], [8, 4]]
a / 2  == ([[5, 3], [4, 2]]) / 2  == [[2.5, 1.5], [2, 1]]
a ** 2 == ([[5, 3], [4, 2]]) ** 2 == [[25, 9], [16, 4]]
a % 2  == ([[5, 3], [4, 2]]) % 2  == [[1, 1], [0, 0]]
```



## Haskell

Matrices are represented here as Immutable Arrays.

```Haskell
{-# OPTIONS_GHC -fno-warn-duplicate-constraints #-}
{-# LANGUAGE RankNTypes #-}

import Data.Array (Array, Ix)
import Data.Array.Base

-- | Element-wise combine the values of two arrays 'a' and 'b' with 'f'.
-- 'a' and 'b' must have the same bounds.
zipWithA :: (IArray arr a, IArray arr b, IArray arr c, Ix i) =>
            (a -> b -> c) -> arr i a -> arr i b -> arr i c
zipWithA f a b =
  case bounds a of
    ba ->
      if ba /= bounds b
      then error "elemwise: bounds mismatch"
      else
        let n = numElements a
        in unsafeArray ba [ (i, f (unsafeAt a i) (unsafeAt b i))
                          | i <- [0 .. n - 1]]

-- Convenient aliases for matrix-matrix element-wise operations.
type ElemOp a b c = (IArray arr a, IArray arr b, IArray arr c, Ix i) =>
                    arr i a -> arr i b -> arr i c
type ElemOp1 a = ElemOp a a a

infixl 6 +:, -:
infixl 7 *:, /:, `divE`

(+:), (-:), (*:) :: (Num a) => ElemOp1 a
(+:) = zipWithA (+)
(-:) = zipWithA (-)
(*:) = zipWithA (*)

divE :: (Integral a) => ElemOp1 a
divE = zipWithA div

(/:) :: (Fractional a) => ElemOp1 a
(/:) = zipWithA (/)

infixr 8 ^:, **:, ^^:

(^:) :: (Num a, Integral b) => ElemOp a b a
(^:) = zipWithA (^)

(**:) :: (Floating a) => ElemOp1 a
(**:) = zipWithA (**)

(^^:) :: (Fractional a, Integral b) => ElemOp a b a
(^^:) = zipWithA (^^)

-- Convenient aliases for matrix-scalar element-wise operations.
type ScalarOp a b c = (IArray arr a, IArray arr c, Ix i) =>
                      arr i a -> b -> arr i c
type ScalarOp1 a = ScalarOp a a a

samap :: (IArray arr a, IArray arr c, Ix i) =>
         (a -> b -> c) -> arr i a -> b -> arr i c
samap f a s = amap (`f` s) a

infixl 6 +., -.
infixl 7 *., /., `divS`

(+.), (-.), (*.) :: (Num a) => ScalarOp1 a
(+.) = samap (+)
(-.) = samap (-)
(*.) = samap (*)

divS :: (Integral a) => ScalarOp1 a
divS = samap div

(/.) :: (Fractional a) => ScalarOp1 a
(/.) = samap (/)

infixr 8 ^., **., ^^.

(^.) :: (Num a, Integral b) => ScalarOp a b a
(^.) = samap (^)

(**.) :: (Floating a) => ScalarOp1 a
(**.) = samap (**)

(^^.) :: (Fractional a, Integral b) => ScalarOp a b a
(^^.) = samap (^^)

main :: IO ()
main = do
  let m1, m2 :: (forall a. (Enum a, Num a) => Array (Int, Int) a)
      m1 = listArray ((0, 0), (2, 3)) [1..]
      m2 = listArray ((0, 0), (2, 3)) [10..]
      s :: (forall a. Num a => a)
      s = 99
  putStrLn "m1"
  print m1
  putStrLn "m2"
  print m2
  putStrLn "s"
  print s
  putStrLn "m1 + m2"
  print $ m1 +: m2
  putStrLn "m1 - m2"
  print $ m1 -: m2
  putStrLn "m1 * m2"
  print $ m1 *: m2
  putStrLn "m1 `div` m2"
  print $ m1 `divE` m2
  putStrLn "m1 / m2"
  print $ m1 /: m2
  putStrLn "m1 ^ m2"
  print $ m1 ^: m2
  putStrLn "m1 ** m2"
  print $ m1 **: m2
  putStrLn "m1 ^^ m2"
  print $ m1 ^^: m2
  putStrLn "m1 + s"
  print $ m1 +. s
  putStrLn "m1 - s"
  print $ m1 -. s
  putStrLn "m1 * s"
  print $ m1 *. s
  putStrLn "m1 `div` s"
  print $ m1 `divS` s
  putStrLn "m1 / s"
  print $ m1 /. s
  putStrLn "m1 ^ s"
  print $ m1 ^. s
  putStrLn "m1 ** s"
  print $ m1 **. s
  putStrLn "m1 ^^ s"
  print $ m1 ^^. s
```

```txt
m1
array ((0,0),(2,3)) [((0,0),1),((0,1),2),((0,2),3),((0,3),4),((1,0),5),((1,1),6),((1,2),7),((1,3),8),((2,0),9),((2,1),10),((2,2),11),((2,3),12)]
m2
array ((0,0),(2,3)) [((0,0),10),((0,1),11),((0,2),12),((0,3),13),((1,0),14),((1,1),15),((1,2),16),((1,3),17),((2,0),18),((2,1),19),((2,2),20),((2,3),21)]
s
99
m1 + m2
array ((0,0),(2,3)) [((0,0),11),((0,1),13),((0,2),15),((0,3),17),((1,0),19),((1,1),21),((1,2),23),((1,3),25),((2,0),27),((2,1),29),((2,2),31),((2,3),33)]
m1 - m2
array ((0,0),(2,3)) [((0,0),-9),((0,1),-9),((0,2),-9),((0,3),-9),((1,0),-9),((1,1),-9),((1,2),-9),((1,3),-9),((2,0),-9),((2,1),-9),((2,2),-9),((2,3),-9)]
m1 * m2
array ((0,0),(2,3)) [((0,0),10),((0,1),22),((0,2),36),((0,3),52),((1,0),70),((1,1),90),((1,2),112),((1,3),136),((2,0),162),((2,1),190),((2,2),220),((2,3),252)]
m1 `div` m2
array ((0,0),(2,3)) [((0,0),0),((0,1),0),((0,2),0),((0,3),0),((1,0),0),((1,1),0),((1,2),0),((1,3),0),((2,0),0),((2,1),0),((2,2),0),((2,3),0)]
m1 / m2
array ((0,0),(2,3)) [((0,0),0.1),((0,1),0.18181818181818182),((0,2),0.25),((0,3),0.3076923076923077),((1,0),0.35714285714285715),((1,1),0.4),((1,2),0.4375),((1,3),0.47058823529411764),((2,0),0.5),((2,1),0.5263157894736842),((2,2),0.55),((2,3),0.5714285714285714)]
m1 ^ m2
array ((0,0),(2,3)) [((0,0),1),((0,1),2048),((0,2),531441),((0,3),67108864),((1,0),6103515625),((1,1),470184984576),((1,2),33232930569601),((1,3),2251799813685248),((2,0),150094635296999121),((2,1),10000000000000000000),((2,2),672749994932560009201),((2,3),46005119909369701466112)]
m1 ** m2
array ((0,0),(2,3)) [((0,0),1.0),((0,1),2048.0),((0,2),531441.0),((0,3),6.7108864e7),((1,0),6.103515625e9),((1,1),4.70184984576e11),((1,2),3.3232930569601e13),((1,3),2.251799813685248e15),((2,0),1.5009463529699914e17),((2,1),1.0e19),((2,2),6.727499949325601e20),((2,3),4.60051199093697e22)]
m1 ^^ m2
array ((0,0),(2,3)) [((0,0),1.0),((0,1),2048.0),((0,2),531441.0),((0,3),6.7108864e7),((1,0),6.103515625e9),((1,1),4.70184984576e11),((1,2),3.3232930569601e13),((1,3),2.251799813685248e15),((2,0),1.5009463529699914e17),((2,1),1.0e19),((2,2),6.7274999493256e20),((2,3),4.60051199093697e22)]
m1 + s
array ((0,0),(2,3)) [((0,0),100),((0,1),101),((0,2),102),((0,3),103),((1,0),104),((1,1),105),((1,2),106),((1,3),107),((2,0),108),((2,1),109),((2,2),110),((2,3),111)]
m1 - s
array ((0,0),(2,3)) [((0,0),-98),((0,1),-97),((0,2),-96),((0,3),-95),((1,0),-94),((1,1),-93),((1,2),-92),((1,3),-91),((2,0),-90),((2,1),-89),((2,2),-88),((2,3),-87)]
m1 * s
array ((0,0),(2,3)) [((0,0),99),((0,1),198),((0,2),297),((0,3),396),((1,0),495),((1,1),594),((1,2),693),((1,3),792),((2,0),891),((2,1),990),((2,2),1089),((2,3),1188)]
m1 `div` s
array ((0,0),(2,3)) [((0,0),0),((0,1),0),((0,2),0),((0,3),0),((1,0),0),((1,1),0),((1,2),0),((1,3),0),((2,0),0),((2,1),0),((2,2),0),((2,3),0)]
m1 / s
array ((0,0),(2,3)) [((0,0),1.0101010101010102e-2),((0,1),2.0202020202020204e-2),((0,2),3.0303030303030304e-2),((0,3),4.040404040404041e-2),((1,0),5.0505050505050504e-2),((1,1),6.060606060606061e-2),((1,2),7.07070707070707e-2),((1,3),8.080808080808081e-2),((2,0),9.090909090909091e-2),((2,1),0.10101010101010101),((2,2),0.1111111111111111),((2,3),0.12121212121212122)]
m1 ^ s
array ((0,0),(2,3)) [((0,0),1),((0,1),633825300114114700748351602688),((0,2),171792506910670443678820376588540424234035840667),((0,3),401734511064747568885490523085290650630550748445698208825344),((1,0),1577721810442023610823457130565572459346412870218046009540557861328125),((1,1),108886437250011817682781711193009636756190618412159145257178661061582856912896),((1,2),462068072803536855906378252728602401551029028414946485847699333055955922805275437143),((1,3),254629497041810760783555711051172270131433549208242031329517556169297662470417088272924672),((2,0),29512665430652752148753480226197736314359272517043832886063884637676943433478020332709411004889),((2,1),1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000),((2,2),12527829399838427440107579247354215251149392000034969484678615956504532008683916069945559954314411495091),((2,3),69014978768345458548673686329780708168010234321157869622016822008604576610843435253147523608071501615464448)]
m1 ** s
array ((0,0),(2,3)) [((0,0),1.0),((0,1),6.338253001141147e29),((0,2),1.7179250691067045e47),((0,3),4.017345110647476e59),((1,0),1.5777218104420236e69),((1,1),1.0888643725001182e77),((1,2),4.620680728035369e83),((1,3),2.5462949704181076e89),((2,0),2.9512665430652752e94),((2,1),1.0e99),((2,2),1.2527829399838427e103),((2,3),6.901497876834546e106)]
m1 ^^ s
array ((0,0),(2,3)) [((0,0),1.0),((0,1),6.338253001141147e29),((0,2),1.7179250691067043e47),((0,3),4.017345110647476e59),((1,0),1.5777218104420238e69),((1,1),1.0888643725001181e77),((1,2),4.620680728035369e83),((1,3),2.5462949704181076e89),((2,0),2.9512665430652752e94),((2,1),1.0000000000000001e99),((2,2),1.2527829399838425e103),((2,3),6.901497876834545e106)]

```


==Icon and {{header|Unicon}}==

This is a Unicon-specific solution solely because of the use of the <tt>[: ... :]</tt> operator.
It would be easy to replace this with another construct to produce a version that works in both languages.
The output flattens each displayed matrix onto a single line to save space here.

```unicon
procedure main()
   a := [[1,2,3],[4,5,6],[7,8,9]]
   b := [[9,8,7],[6,5,4],[3,2,1]]
   showMat("  a: ",a)
   showMat("  b: ",b)
   showMat("a+b: ",mmop("+",a,b))
   showMat("a-b: ",mmop("-",a,b))
   showMat("a*b: ",mmop("*",a,b))
   showMat("a/b: ",mmop("/",a,b))
   showMat("a^b: ",mmop("^",a,b))
   showMat("a+2: ",msop("+",a,2))
   showMat("a-2: ",msop("-",a,2))
   showMat("a*2: ",msop("*",a,2))
   showMat("a/2: ",msop("/",a,2))
   showMat("a^2: ",msop("^",a,2))
end

procedure mmop(op,A,B)
    if (*A = *B) & (*A[1] = *B[1]) then {
        C := [: |list(*A[1])\*A[1] :]
        a1 := create !!A
        b1 := create !!B
        every (!!C) := op(@a1,@b1)
        return C
        }
end

procedure msop(op,A,s)
    C := [: |list(*A[1])\*A[1] :]
    a1 := create !!A
    every (!!C) := op(@a1,s)
    return C
end

procedure showMat(label, m)
    every writes(label | right(!!m,5) | "\n")
end
```


```txt

->ewo
  a:     1    2    3    4    5    6    7    8    9
  b:     9    8    7    6    5    4    3    2    1
a+b:    10   10   10   10   10   10   10   10   10
a-b:    -8   -6   -4   -2    0    2    4    6    8
a*b:     9   16   21   24   25   24   21   16    9
a/b:     0    0    0    0    1    1    2    4    9
a^b:     1  256 2187 4096 3125 1296  343   64    9
a+2:     3    4    5    6    7    8    9   10   11
a-2:    -1    0    1    2    3    4    5    6    7
a*2:     2    4    6    8   10   12   14   16   18
a/2:     0    1    1    2    2    3    3    4    4
a^2:     1    4    9   16   25   36   49   64   81
->

```



## J

'''Solution''': J's arithmetical primitives act elementwise by default (in J parlance, such operations are known as "scalar" or "rank zero", which means they generalize to high-order arrays transparently, operating elementwise).  Thus:
```j
   scalar =: 10
   vector =: 2 3 5
   matrix =: 3 3 $    7 11 13  17 19 23  29 31 37

   scalar * scalar
100
   scalar * vector
20 30 50
   scalar * matrix
 70 110 130
170 190 230
290 310 370

   vector * vector
4 9 25
   vector * matrix
 14  22  26
 51  57  69
145 155 185

   matrix * matrix
 49 121  169
289 361  529
841 961 1369
```
 And similarly for <tt>+</tt>, <tt>-</tt>, <tt>%</tt> (division), and <tt>^</tt> .

Note that in some branches of mathematics, it has been traditional to define multiplication such that it is not performed element-wise.  This can introduce some complications ([[wp:Einstein notation]] is arguably the best approach for resolving those complexities in latex, when they occur frequently enough that mentioning and using the notation is not more complicated than explicitly describing the multiply-and-sum) and makes expressing element-wise multiplication complicated.  J deals with this conflict by making its multiplication primitive be elementwise (consistent with the rest of the language) and by using a different verb (typically +/ .*) to represent the traditional non-element-wise multiply and sum operation.

## Java


```Java

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Stream;

@SuppressWarnings("serial")
public class ElementWiseOp {
	static final Map<String, BiFunction<Double, Double, Double>> OPERATIONS = new HashMap<String, BiFunction<Double, Double, Double>>() {
		{
			put("add", (a, b) -> a + b);
			put("sub", (a, b) -> a - b);
			put("mul", (a, b) -> a * b);
			put("div", (a, b) -> a / b);
			put("pow", (a, b) -> Math.pow(a, b));
			put("mod", (a, b) -> a % b);
		}
	};
	public static Double[][] scalarOp(String op, Double[][] matr, Double scalar) {
		BiFunction<Double, Double, Double> operation = OPERATIONS.getOrDefault(op, (a, b) -> a);
		Double[][] result = new Double[matr.length][matr[0].length];
		for (int i = 0; i < matr.length; i++) {
			for (int j = 0; j < matr[i].length; j++) {
				result[i][j] = operation.apply(matr[i][j], scalar);
			}
		}
		return result;
	}
	public static Double[][] matrOp(String op, Double[][] matr, Double[][] scalar) {
		BiFunction<Double, Double, Double> operation = OPERATIONS.getOrDefault(op, (a, b) -> a);
		Double[][] result = new Double[matr.length][Stream.of(matr).mapToInt(a -> a.length).max().getAsInt()];
		for (int i = 0; i < matr.length; i++) {
			for (int j = 0; j < matr[i].length; j++) {
				result[i][j] = operation.apply(matr[i][j], scalar[i % scalar.length][j
						% scalar[i % scalar.length].length]);
			}
		}
		return result;
	}
	public static void printMatrix(Double[][] matr) {
		Stream.of(matr).map(Arrays::toString).forEach(System.out::println);
	}
	public static void main(String[] args) {
		printMatrix(scalarOp("mul", new Double[][] {
				{ 1.0, 2.0, 3.0 },
				{ 4.0, 5.0, 6.0 },
				{ 7.0, 8.0, 9.0 }
		}, 3.0));

		printMatrix(matrOp("div", new Double[][] {
				{ 1.0, 2.0, 3.0 },
				{ 4.0, 5.0, 6.0 },
				{ 7.0, 8.0, 9.0 }
		}, new Double[][] {
				{ 1.0, 2.0},
				{ 3.0, 4.0}
		}));
	}
}

```



## jq

The following definition of elementwise allows matrices of any type
to be processed, e.g. the matrices could be string or object-valued,
and they can be of mixed type.

The matrices also need not be rectangular or conformant,
but the resultant matrix will be rectangular, with the same number
of rows as self, and if that number is greater than 0, then the number of
columns in the result will be the length of the first row of self.

In jq, it is idiomatic to specify an operation by using a jq filter.
This means that composite and user-defined operations can be
specified.  In the following definition of "elementwise", the
"operator" argument for addition, for example, would be given as
(.[0] + .[1]) rather than the string "+".

In Part 2 below, a variation of "elementwise" is presented that does
accept string specifications of common operators, e.g. "+" for addition.
However this is done mainly for illustration and is not recommended,
primarily because it introduces certain complexities.

'''Part 1'''

```jq
# Occurrences of .[0] in "operator" will refer to an element in self,
# and occurrences of .[1] will refer to the corresponding element in other.
def elementwise( operator; other ):
  length as $rows
  | if $rows == 0 then .
    else . as $self
    | other as $other
    | ($self[0]|length) as $cols
    | reduce range(0; $rows) as $i
        ([]; reduce range(0; $cols) as $j
          (.; .[$i][$j] = ([$self[$i][$j], $other[$i][$j]] | operator) ) )
    end ;
```

'''Example''':

```jq
[[3,1,4],[1,5,9]] as $m1 | [[2,7,1],[8,2,2]] as $m2
| ( ($m1|elementwise(.[0] + .[1]; $m2) ),
    ($m1|elementwise(.[0] + 2 * .[1]; $m2) ),
    ($m1|elementwise(.[0] < .[1]; $m2) ) )

```

```sh
[[5,8,5],[9,7,11]]
[[7,15,6],[17,9,13]]
[[false,true,false],[true,false,false]]

```


'''Part 2'''

In elementwise2, the operator can be any jq filter e.g. (.[0] <
.[1]), where .[0] refers to an element in self and .[1] to the
corresponding element in other, but if it is one of the strings "+",
"-", "*", "/", "%", "//", "**", "^" or "pow", then the corresponding
operator will be applied.  Note that in jq, operators are in general
polymorphic.  For example, + is defined on strings and other types
besides numbers.

```jq
def elementwise2( operator; other ):
  def pow(i): . as $in | reduce range(0;i) as $i (1; .*$in);
  def operation(x; op; y):
    [x,y] | op as $op
    | if $op == "+" then x+y
      elif $op == "-" then x-y
      elif $op == "*" then x*y
      elif $op == "/" then x/y
      elif $op == "%" then x%y
      elif $op == "//" then x/y|floor
      elif $op == "**" or $op == "^" or $op == "pow" then x|pow(y)
      else $op
      end;

  length as $rows
  | if $rows == 0 then .
    else . as $self
    | other as $other
    | ($self[0]|length) as $cols
    | reduce range(0; $rows) as $i
        ([]; reduce range(0; $cols) as $j
          (.; .[$i][$j] = operation($self[$i][$j]; operator; $other[$i][$j] ) ) )
    end;
```

'''Example''':

```jq
[[3,1,4],[1,5,9]] as $m1 | [[2,7,1],[8,2,2]] as $m2
  | ( ($m1|elementwise2("+"; $m2) ),
      ($m1|elementwise2("//"; $m2)),
      ($m1|elementwise2(.[0] < .[1]; $m2) ) )
```

```sh
[[5,8,5],[9,7,11]]
[[1,0,4],[0,2,4]]
[[false,true,false],[true,false,false]]

```



## Julia

In Julia operations with `.` before are for convention Element-wise:

```julia
@show [1 2 3; 3 2 1] .+ [2 1 2; 0 2 1]
@show [1 2 3; 2 1 2] .+ 1
@show [1 2 3; 2 2 1] .- [1 1 1; 2 1 0]
@show [1 2 1; 1 2 3] .* [3 2 1; 1 0 1]
@show [1 2 3; 3 2 1] .* 2
@show [9 8 6; 3 2 3] ./ [3 1 2; 2 1 2]
@show [3 2 2; 1 2 3] .^ [1 2 3; 2 1 2]
```


```txt
[1 2 3; 3 2 1] .+ [2 1 2; 0 2 1] = [3 3 5; 3 4 2]
[1 2 3; 2 1 2] .+ 1 = [2 3 4; 3 2 3]
[1 2 3; 2 2 1] .- [1 1 1; 2 1 0] = [0 1 2; 0 1 1]
[1 2 1; 1 2 3] .* [3 2 1; 1 0 1] = [3 4 1; 1 0 3]
[1 2 3; 3 2 1] .* 2 = [2 4 6; 6 4 2]
[9 8 6; 3 2 3] ./ [3 1 2; 2 1 2] = [3.0 8.0 3.0; 1.5 2.0 1.5]
[3 2 2; 1 2 3] .^ [1 2 3; 2 1 2] = [3 4 8; 1 2 9]
```



## K

```K
   scalar: 10
   vector: 2 3 5
   matrix: 3 3 # 7 11 13  17 19 23  29 31 37

   scalar * scalar
100
   scalar * vector
20 30 50
   scalar * matrix
(70 110 130
 170 190 230
 290 310 370)

   vector * vector
4 9 25
   vector * matrix
(14 22 26
 51 57 69
 145 155 185)

   matrix * matrix
(49 121 169
 289 361 529
 841 961 1369)

```
 And similarly for <tt>+</tt>, <tt>-</tt>, <tt>%</tt> (division), and <tt>^</tt> .


## Kotlin


```scala
// version 1.1.51

typealias Matrix = Array<DoubleArray>
typealias Op = Double.(Double) -> Double

fun Double.dPow(exp: Double) = Math.pow(this, exp)

fun Matrix.elementwiseOp(other: Matrix, op: Op): Matrix {
    require(this.size == other.size && this[0].size == other[0].size)
    val result = Array(this.size) { DoubleArray(this[0].size) }
    for (i in 0 until this.size) {
        for (j in 0 until this[0].size) result[i][j] = this[i][j].op(other[i][j])
    }
    return result
}

fun Matrix.elementwiseOp(d: Double, op: Op): Matrix {
    val result = Array(this.size) { DoubleArray(this[0].size) }
    for (i in 0 until this.size) {
        for (j in 0 until this[0].size) result[i][j] = this[i][j].op(d)
    }
    return result
}

fun Matrix.print(name: Char?, scalar: Boolean? = false) {
    println(when (scalar) {
        true  -> "m $name s"
        false -> "m $name m"
        else  -> "m"
    } + ":")
    for (i in 0 until this.size) println(this[i].asList())
    println()
}

fun main(args: Array<String>) {
    val ops = listOf(Double::plus, Double::minus, Double::times, Double::div, Double::dPow)
    val names = "+-*/^"
    val m = arrayOf(
        doubleArrayOf(3.0, 5.0, 7.0),
        doubleArrayOf(1.0, 2.0, 3.0),
        doubleArrayOf(2.0, 4.0, 6.0)
    )
    m.print(null, null)
    for ((i, op) in ops.withIndex()) m.elementwiseOp(m, op).print(names[i])
    val s = 2.0
    println("s = $s:\n")
    for ((i, op) in ops.withIndex()) m.elementwiseOp(s, op).print(names[i], true)
}
```


```txt

m:
[3.0, 5.0, 7.0]
[1.0, 2.0, 3.0]
[2.0, 4.0, 6.0]

m + m:
[6.0, 10.0, 14.0]
[2.0, 4.0, 6.0]
[4.0, 8.0, 12.0]

m - m:
[0.0, 0.0, 0.0]
[0.0, 0.0, 0.0]
[0.0, 0.0, 0.0]

m * m:
[9.0, 25.0, 49.0]
[1.0, 4.0, 9.0]
[4.0, 16.0, 36.0]

m / m:
[1.0, 1.0, 1.0]
[1.0, 1.0, 1.0]
[1.0, 1.0, 1.0]

m ^ m:
[27.0, 3125.0, 823543.0]
[1.0, 4.0, 27.0]
[4.0, 256.0, 46656.0]

s = 2.0:

m + s:
[5.0, 7.0, 9.0]
[3.0, 4.0, 5.0]
[4.0, 6.0, 8.0]

m - s:
[1.0, 3.0, 5.0]
[-1.0, 0.0, 1.0]
[0.0, 2.0, 4.0]

m * s:
[6.0, 10.0, 14.0]
[2.0, 4.0, 6.0]
[4.0, 8.0, 12.0]

m / s:
[1.5, 2.5, 3.5]
[0.5, 1.0, 1.5]
[1.0, 2.0, 3.0]

m ^ s:
[9.0, 25.0, 49.0]
[1.0, 4.0, 9.0]
[4.0, 16.0, 36.0]

```



## Maple


```Maple
# Built-in element-wise operator ~

#addition
<1,2,3;4,5,6> +~ 2;

#subtraction
<2,3,1,4;0,-2,-2,1> -~ 4;

#multiplication
<2,3,1,4;0,-2,-2,1> *~ 4;

#division
<2,3,7,9;6,8,4,5;7,0,10,11> /~ 2;

#exponentiation
<1,2,0; 7,2,7; 6,11,3>^~5;
```

```txt

Matrix(2, 3, [[3, 4, 5], [6, 7, 8]])
Matrix(2, 4, [[-2, -1, -3, 0], [-4, -6, -6, -3]])
Matrix(2, 4, [[8, 12, 4, 16], [0, -8, -8, 4]])
Matrix(3, 4, [[1, 3/2, 7/2, 9/2], [3, 4, 2, 5/2], [7/2, 0, 5, 11/2]])
Matrix(3, 3, [[1, 32, 0], [16807, 32, 16807], [7776, 161051, 243]])

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
S = 10 ; M = {{7, 11, 13}, {17 , 19, 23} , {29, 31, 37}};
M + S
M - S
M * S
M / S
M ^ S

M + M
M - M
M * M
M / M
M ^ M

Gives:

->{{17, 21, 23}, {27, 29, 33}, {39, 41, 47}}
->{{-3, 1, 3}, {7, 9, 13}, {19, 21, 27}}
->{{70, 110, 130}, {170, 190, 230}, {290, 310, 370}}
->{{7/10, 11/10, 13/10}, {17/10, 19/10, 23/10}, {29/10, 31/10, 37/10}}
->{{282475249, 25937424601, 137858491849}, {2015993900449,
  6131066257801, 41426511213649}, {420707233300201, 819628286980801,
  4808584372417849}}

->{{14, 22, 26}, {34, 38, 46}, {58, 62, 74}}
->{{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}
->{{49, 121, 169}, {289, 361, 529}, {841, 961, 1369}}
->{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}
->{{823543, 285311670611, 302875106592253}, {827240261886336764177,
  1978419655660313589123979,
  20880467999847912034355032910567}, {2567686153161211134561828214731016126483469,
  17069174130723235958610643029059314756044734431,
  10555134955777783414078330085995832946127396083370199442517}}
```



## MATLAB



```Matlab
a = rand;
b = rand(10,10);
scalar_matrix = a * b;
component_wise = b .* b;
```



## Maxima


```maxima
a: matrix([1, 2], [3, 4]);
b: matrix([2, 4], [3, 1]);

a * b;
a / b;
a + b;
a - b;
a^3;
a^b;  /* won't work */
fullmapl("^", a, b);
sin(a);
```



## PARI/GP

GP already implements element-wise matrix-matrix addition and subtraction and element-wise scalar-matrix multiplication and division.  Other element-wise matrix-matrix functions:

```parigp
multMM(A,B)=matrix(#A[,1],#A,i,j,A[i,j]*B[i,j]);
divMM(A,B)=matrix(#A[,1],#A,i,j,A[i,j]/B[i,j]);
powMM(A,B)=matrix(#A[,1],#A,i,j,A[i,j]^B[i,j]);
```


Other element-wise scalar-matrix functions:

```parigp
addMs(A,s)=A+matrix(#A[,1],#A,i,j,s);
subMs(A,s)=A-matrix(#A[,1],#A,i,j,s);
powMs(A,s)=matrix(#A[,1],#A,i,j,A[i,j]^s);
```


PARI implements convenience functions <code>vecmul</code> (element-wise matrix-matrix multiplication), <code>vecdiv</code> (element-wise matrix-matrix division), and <code>vecpow</code> (element-wise matrix-scalar exponentiation), as well as <code>vecmodii</code> and <code>vecinv</code>. These operate on vectors, but a <code>t_MAT</code> is simply an array of vectors in PARI so it applies fairly easily.


## Perl

There's no need to use real multi-dimentional arrays to represent matrix.
Since matrices have fixed row length, they can be represented by flat array.

This example demonstrates Perl's operator overload ability and bulk list operations using <tt>map</tt>.

File <tt>Elementwise.pm</tt>:

```perl
package Elementwise;

use Exporter 'import';

use overload
'='  => sub { $_[0]->clone() },
'+'  => sub { $_[0]->add($_[1]) },
'-'  => sub { $_[0]->sub($_[1]) },
'*'  => sub { $_[0]->mul($_[1]) },
'/'  => sub { $_[0]->div($_[1]) },
'**'  => sub { $_[0]->exp($_[1]) },
;

sub new
{
	my ($class, $v) = @_;
	return bless $v, $class;
}

sub clone
{
	my @ret = @{$_[0]};
	return bless \@ret, ref($_[0]);
}

sub add { new Elementwise ref($_[1]) ? [map { $_[0][$_]  + $_[1][$_] } 0 .. $#{$_[0]} ] : [map { $_[0][$_]  + $_[1] } 0 .. $#{$_[0]} ] }
sub sub { new Elementwise ref($_[1]) ? [map { $_[0][$_]  - $_[1][$_] } 0 .. $#{$_[0]} ] : [map { $_[0][$_]  - $_[1] } 0 .. $#{$_[0]} ] }
sub mul { new Elementwise ref($_[1]) ? [map { $_[0][$_]  * $_[1][$_] } 0 .. $#{$_[0]} ] : [map { $_[0][$_]  * $_[1] } 0 .. $#{$_[0]} ] }
sub div { new Elementwise ref($_[1]) ? [map { $_[0][$_]  / $_[1][$_] } 0 .. $#{$_[0]} ] : [map { $_[0][$_]  / $_[1] } 0 .. $#{$_[0]} ] }
sub exp { new Elementwise ref($_[1]) ? [map { $_[0][$_] ** $_[1][$_] } 0 .. $#{$_[0]} ] : [map { $_[0][$_] ** $_[1] } 0 .. $#{$_[0]} ] }

1;
```


File <tt>test.pl</tt>:

```perl
use Elementwise;

$a = new Elementwise [
	1,2,3,
	4,5,6,
	7,8,9
];

print << "_E";
a  @$a
a OP a
+  @{$a+$a}
-  @{$a-$a}
*  @{$a*$a}
/  @{$a/$a}
^  @{$a**$a}
a OP 5
+  @{$a+5}
-  @{$a-5}
*  @{$a*5}
/  @{$a/5}
^  @{$a**5}
_E
```


```txt
a  1 2 3 4 5 6 7 8 9
a OP a
+  2 4 6 8 10 12 14 16 18
-  0 0 0 0 0 0 0 0 0
*  1 4 9 16 25 36 49 64 81
/  1 1 1 1 1 1 1 1 1
^  1 4 27 256 3125 46656 823543 16777216 387420489
a OP 5
+  6 7 8 9 10 11 12 13 14
-  -4 -3 -2 -1 0 1 2 3 4
*  5 10 15 20 25 30 35 40 45
/  0.2 0.4 0.6 0.8 1 1.2 1.4 1.6 1.8
^  1 32 243 1024 3125 7776 16807 32768 59049
```



## Perl 6

Perl 6 already implements this and other metaoperators as higher-order functions (cross, zip, reduce, triangle, etc.) that are usually accessed through a meta-operator syntactic sugar that is productive over all appropriate operators, including user-defined ones.  In this case, a dwimmy element-wise operator (generically known as a "hyper") is indicated by surrounding the operator with double angle quotes.  Hypers dwim on the pointy end with cyclic APL semantics as necessary.  You can turn the quote the other way to suppress dwimmery on that end.  In this case we could have used <tt>»op»</tt> instead of <tt>«op»</tt> since the short side is always on the right.
```perl6
my @a =
    [1,2,3],
    [4,5,6],
    [7,8,9];

sub msay(@x) {
    for @x -> @row {
        print ' ', $_%1 ?? $_.nude.join('/') !! $_ for @row;
        say '';
    }
    say '';
}

msay @a «+» @a;
msay @a «-» @a;
msay @a «*» @a;
msay @a «/» @a;
msay @a «+» [1,2,3];
msay @a «-» [1,2,3];
msay @a «*» [1,2,3];
msay @a «/» [1,2,3];
msay @a «+» 2;
msay @a «-» 2;
msay @a «*» 2;
msay @a «/» 2;

# In addition to calling the underlying higher-order functions directly, it's possible to name a function.

sub infix:<M+> (\l,\r) { l <<+>> r }

msay @a M+ @a;
msay @a M+ [1,2,3];
msay @a M+ 2;

```

```txt
 2 4 6
 8 10 12
 14 16 18

 0 0 0
 0 0 0
 0 0 0

 1 4 9
 16 25 36
 49 64 81

 1 1 1
 1 1 1
 1 1 1

 2 3 4
 6 7 8
 10 11 12

 0 1 2
 2 3 4
 4 5 6

 1 2 3
 8 10 12
 21 24 27

 1 2 3
 2 5/2 3
 7/3 8/3 3

 3 4 5
 6 7 8
 9 10 11

 -1 0 1
 2 3 4
 5 6 7

 2 4 6
 8 10 12
 14 16 18

 1/2 1 3/2
 2 5/2 3
 7/2 4 9/2

 2 4 6
 8 10 12
 14 16 18

 2 3 4
 6 7 8
 10 11 12

 3 4 5
 6 7 8
 9 10 11
```



## Phix

Phix has builtin sequence ops, which work fine with a multi-dimensional array / matrix:

```Phix
constant m = {{7, 8, 7},{4, 0, 9}},
         m2 = {{4, 5, 1},{6, 2, 1}}
?{m,"+",m2,"=",sq_add(m,m2)}
?{m,"-",m2,"=",sq_sub(m,m2)}
?{m,"*",m2,"=",sq_mul(m,m2)}
?{m,"/",m2,"=",sq_div(m,m2)}
?{m,"^",m2,"=",sq_power(m,m2)}
?{m,"+ 3 =",sq_add(m,3)}
?{m,"- 3 =",sq_sub(m,3)}
?{m,"* 3 =",sq_mul(m,3)}
?{m,"/ 3 =",sq_div(m,3)}
?{m,"^ 3 =",sq_power(m,3)}
```

```txt

```



## PicoLisp


```PicoLisp
(de elementWiseMatrix (Fun Mat1 Mat2)
   (mapcar '((L1 L2) (mapcar Fun L1 L2)) Mat1 Mat2) )

(de elementWiseScalar (Fun Mat Scalar)
   (elementWiseMatrix Fun Mat (circ (circ Scalar))) )
```

Test:

```txt
(let (S 10  M '((7 11 13) (17 19 23) (29 31 37)))
   (println (elementWiseScalar + M S))
   (println (elementWiseScalar - M S))
   (println (elementWiseScalar * M S))
   (println (elementWiseScalar / M S))
   (println (elementWiseScalar ** M S))
   (prinl)
   (println (elementWiseMatrix + M M))
   (println (elementWiseMatrix - M M))
   (println (elementWiseMatrix * M M))
   (println (elementWiseMatrix / M M))
   (println (elementWiseMatrix ** M M)) )
```

```txt
((17 21 23) (27 29 33) (39 41 47))
((-3 1 3) (7 9 13) (19 21 27))
((70 110 130) (170 190 230) (290 310 370))
((0 1 1) (1 1 2) (2 3 3))
((282475249 25937424601 137858491849) (2015993900449 6131066257801 ...

((14 22 26) (34 38 46) (58 62 74))
((0 0 0) (0 0 0) (0 0 0))
((49 121 169) (289 361 529) (841 961 1369))
((1 1 1) (1 1 1) (1 1 1))
((823543 285311670611 302875106592253) (827240261886336764177 ...
```



## PL/I

Any arithmetic operation can be applied to elements of arrays.
These examples illustrate element-by-element multiplication, but addition, subtraction, division, and exponentiation can also be written.

```PL/I
declare (matrix(3,3), vector(3), scalar) fixed;
declare (m(3,3), v(3) fixed;

m = scalar * matrix;
m = vector * matrix;
m = matrix * matrix;

v = scalar * vector;
v = vector * vector;
```



## Python


```python>>>
 import random
>>> from operator import add, sub, mul, floordiv
>>> from pprint import pprint as pp
>>>
>>> def ewise(matrix1, matrix2, op):
	return [[op(e1,e2) for e1,e2 in zip(row1, row2)] for row1,row2 in zip(matrix1, matrix2)]

>>> m,n = 3,4 	# array dimensions
>>> a0 = [[random.randint(1,9) for y in range(n)] for x in range(m)]
>>> a1 = [[random.randint(1,9) for y in range(n)] for x in range(m)]
>>> pp(a0); pp(a1)
[[7, 8, 7, 4], [4, 9, 4, 1], [2, 3, 6, 4]]
[[4, 5, 1, 6], [6, 8, 3, 4], [2, 2, 6, 3]]
>>> pp(ewise(a0, a1, add))
[[11, 13, 8, 10], [10, 17, 7, 5], [4, 5, 12, 7]]
>>> pp(ewise(a0, a1, sub))
[[3, 3, 6, -2], [-2, 1, 1, -3], [0, 1, 0, 1]]
>>> pp(ewise(a0, a1, mul))
[[28, 40, 7, 24], [24, 72, 12, 4], [4, 6, 36, 12]]
>>> pp(ewise(a0, a1, floordiv))
[[1, 1, 7, 0], [0, 1, 1, 0], [1, 1, 1, 1]]
>>> pp(ewise(a0, a1, pow))
[[2401, 32768, 7, 4096], [4096, 43046721, 64, 1], [4, 9, 46656, 64]]
>>> pp(ewise(a0, a1, lambda x, y:2*x - y))
[[10, 11, 13, 2], [2, 10, 5, -2], [2, 4, 6, 5]]
>>>
>>> def s_ewise(scalar1, matrix1, op):
	return [[op(scalar1, e1) for e1 in row1] for row1 in matrix1]

>>> scalar = 10
>>> a0
[[7, 8, 7, 4], [4, 9, 4, 1], [2, 3, 6, 4]]
>>> for op in ( add, sub, mul, floordiv, pow, lambda x, y:2*x - y ):
	print("%10s :" % op.__name__, s_ewise(scalar, a0, op))


       add : [[17, 18, 17, 14], [14, 19, 14, 11], [12, 13, 16, 14]]
       sub : [[3, 2, 3, 6], [6, 1, 6, 9], [8, 7, 4, 6]]
       mul : [[70, 80, 70, 40], [40, 90, 40, 10], [20, 30, 60, 40]]
  floordiv : [[1, 1, 1, 2], [2, 1, 2, 10], [5, 3, 1, 2]]
       pow : [[10000000, 100000000, 10000000, 10000], [10000, 1000000000, 10000, 10], [100, 1000, 1000000, 10000]]
  <lambda> : [[13, 12, 13, 16], [16, 11, 16, 19], [18, 17, 14, 16]]
>>>
```



## R


In R most operations work on vectors and matrices:


```R
# create a 2-times-2 matrix
mat <- matrix(1:4, 2, 2)

# matrix with scalar
mat + 2
mat * 2
mat ^ 2

# matrix with matrix
mat + mat
mat * mat
mat ^ mat
```


```txt
> mat <- matrix(1:4, 2, 2)
     [,1] [,2]
[1,]    1    3
[2,]    2    4

> mat + 2
     [,1] [,2]
[1,]    3    5
[2,]    4    6

> mat * 2
     [,1] [,2]
[1,]    2    6
[2,]    4    8

> mat ^ 2
     [,1] [,2]
[1,]    1    9
[2,]    4   16

> mat + mat
     [,1] [,2]
[1,]    2    6
[2,]    4    8

> mat * mat
     [,1] [,2]
[1,]    1    9
[2,]    4   16

> mat ^ mat
     [,1] [,2]
[1,]    1   27
[2,]    4  256
```



## Racket

```racket
#lang racket(require math/array)

(define mat (list->array #(2 2) '(1 3 2 4)))

mat
(array+ mat (array 2))
(array* mat (array 2))
(array-map expt mat (array 2))

(array+ mat mat)
(array* mat mat)
(array-map expt mat mat)

```


```txt

(array #[#[1 3] #[2 4]])
(array #[#[3 5] #[4 6]])
(array #[#[2 6] #[4 8]])
(array #[#[1 9] #[4 16]])
(array #[#[2 6] #[4 8]])
(array #[#[1 9] #[4 16]])
(array #[#[1 27] #[4 256]])

```



## REXX


### discrete


```rexx
/*REXX program  multiplies two matrixes together, displays the  matrixes and the result.*/
m=(1 2 3)  (4 5 6)  (7 8 9)
w=words(m);               do k=1;  if k*k>=w  then leave;  end  /*k*/;     rows=k;  cols=k
call showMat  M, 'M matrix'
answer=matAdd(m, 2  );    call showMat  answer, 'M matrix, added 2'
answer=matSub(m, 7  );    call showMat  answer, 'M matrix, subtracted 7'
answer=matMul(m, 2.5);    call showMat  answer, 'M matrix, multiplied by 2½'
answer=matPow(m, 3  );    call showMat  answer, 'M matrix, cubed'
answer=matDiv(m, 4  );    call showMat  answer, 'M matrix, divided by 4'
answer=matIdv(m, 2  );    call showMat  answer, 'M matrix, integer halved'
answer=matMod(m, 3  );    call showMat  answer, 'M matrix, modulus 3'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
matAdd:   parse arg @,#;  call mat#;    do j=1  for w; !.j=!.j+#;     end;   return mat@()
matSub:   parse arg @,#;  call mat#;    do j=1  for w; !.j=!.j-#;     end;   return mat@()
matMul:   parse arg @,#;  call mat#;    do j=1  for w; !.j=!.j*#;     end;   return mat@()
matDiv:   parse arg @,#;  call mat#;    do j=1  for w; !.j=!.j/#;     end;   return mat@()
matIdv:   parse arg @,#;  call mat#;    do j=1  for w; !.j=!.j%#;     end;   return mat@()
matPow:   parse arg @,#;  call mat#;    do j=1  for w; !.j=!.j**#;    end;   return mat@()
matMod:   parse arg @,#;  call mat#;    do j=1  for w; !.j=!.j//#;    end;   return mat@()
mat#:     w=words(@);                   do j=1  for w; !.j=word(@,j); end;   return
mat@:     @=!.1;                        do j=2   to w; @=@ !.j;       end;   return @
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat:  parse arg @, hdr; L=0;  say
                                        do j=1  for w;  L=max(L,length(word(@,j)));  end
          say  center(hdr, max(length(hdr)+4, cols*(L+1)+4), "─")
          n=0
                    do r    =1 for rows;         _=
                        do c=1 for cols; n=n+1;  _=_ right(word(@,n),L); end;    say _
                    end
          return
```

'''output'''
<pre style="height:63ex">
──M matrix──
 1 2 3
 4 5 6
 7 8 9

──M matrix, added 2──
  3  4  5
  6  7  8
  9 10 11

──M matrix, subtracted 7──
 -6 -5 -4
 -3 -2 -1
  0  1  2

──M matrix, multiplied by 2½──
  2.5  5.0  7.5
 10.0 12.5 15.0
 17.5 20.0 22.5

──M matrix, cubed──
   1   8  27
  64 125 216
 343 512 729

──M matrix, divided by 4──
 0.25  0.5 0.75
    1 1.25  1.5
 1.75    2 2.25

──M matrix, integer halved──
 0 1 1
 2 2 3
 3 4 4

──M matrix, modulus 3──
 1 2 0
 1 2 0
 1 2 0

```



### generalized


```rexx
/*REXX program  multiplies two matrixes together, displays the matrixes and the result. */
m=(1 2 3)  (4 5 6)  (7 8 9)
w=words(m);                  do k=1; if k*k>=w then leave; end  /*k*/;     rows=k;  cols=k
call showMat  M, 'M matrix'
ans=matOp(m, '+2'   );   call showMat  ans,  "M matrix, added 2"
ans=matOp(m, '-7'   );   call showMat  ans,  "M matrix, subtracted 7"
ans=matOp(m, '*2.5' );   call showMat  ans,  "M matrix, multiplied by 2½"
ans=matOp(m, '**3'  );   call showMat  ans,  "M matrix, cubed"
ans=matOp(m, '/4'   );   call showMat  ans,  "M matrix, divided by 4"
ans=matOp(m, '%2'   );   call showMat  ans,  "M matrix, integer halved"
ans=matOp(m, '//3'  );   call showMat  ans,  "M matrix, modulus 3"
ans=matOp(m, '*3-1' );   call showMat  ans,  "M matrix, tripled, less one"
exit                                             /*stick a fork in it,  we"re all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
matOp:  parse arg @,#; call mat#; do j=1 for w; interpret '!.'j"=!."j #;end; return mat@()
mat#:   w=words(@);               do j=1 for w; !.j=word(@,j);          end; return
mat@:   @=!.1;                    do j=2  to w; @=@ !.j;                end; return @
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat:  parse arg @, hdr;  say
          L=0;                    do j=1  for w;     L=max(L,length(word(@,j)));   end
          say;   say center(hdr,max(length(hdr)+4,cols*(L+1)+4),"─")
          n=0
                    do r    =1 for rows;         _=
                        do c=1 for cols; n=n+1;  _=_ right(word(@,n),L); end;    say _
                    end
          return
```

'''output'''
<pre style="height:63ex">
──M matrix──
 1 2 3
 4 5 6
 7 8 9


──M matrix, added 2──
  3  4  5
  6  7  8
  9 10 11


──M matrix, subtracted 7──
 -6 -5 -4
 -3 -2 -1
  0  1  2


──M matrix, multiplied by 2½──
  2.5  5.0  7.5
 10.0 12.5 15.0
 17.5 20.0 22.5


──M matrix, cubed──
   1   8  27
  64 125 216
 343 512 729


──M matrix, divided by 4──
 0.25  0.5 0.75
    1 1.25  1.5
 1.75    2 2.25


──M matrix, integer halved──
 0 1 1
 2 2 3
 3 4 4


──M matrix, modulus 3──
 1 2 0
 1 2 0
 1 2 0


──M matrix, tripled, less one──
  2  5  8
 11 14 17
 20 23 26

```



## Ruby


```ruby
require 'matrix'

class Matrix
  def element_wise( operator, other )
    Matrix.build(row_size, column_size) do |row, col|
      self[row, col].send(operator, other[row, col])
    end
  end
end

m1, m2 = Matrix[[3,1,4],[1,5,9]], Matrix[[2,7,1],[8,2,2]]
puts "m1: #{m1}\nm2: #{m2}\n\n"

[:+, :-, :*, :/, :fdiv, :**, :%].each do |op|
  puts "m1 %-4s m2  =  %s" % [op, m1.element_wise(op, m2)]
end
```

```txt

m1: Matrix[[3, 1, 4], [1, 5, 9]]
m2: Matrix[[2, 7, 1], [8, 2, 2]]

m1 +    m2  =  Matrix[[5, 8, 5], [9, 7, 11]]
m1 -    m2  =  Matrix[[1, -6, 3], [-7, 3, 7]]
m1 *    m2  =  Matrix[[6, 7, 4], [8, 10, 18]]
m1 /    m2  =  Matrix[[1, 0, 4], [0, 2, 4]]
m1 fdiv m2  =  Matrix[[1.5, 0.14285714285714285, 4.0], [0.125, 2.5, 4.5]]
m1 **   m2  =  Matrix[[9, 1, 4], [1, 25, 81]]
m1 %    m2  =  Matrix[[1, 1, 0], [1, 1, 1]]

```



## Rust


```rust
struct Matrix {
    elements: Vec<f32>,
    pub height: u32,
    pub width: u32,
}

impl Matrix {
    fn new(elements: Vec<f32>, height: u32, width: u32) -> Matrix {
        // Should check for dimensions but omitting to be succient
        Matrix {
            elements: elements,
            height: height,
            width: width,
        }
    }

    fn get(&self, row: u32, col: u32) -> f32 {
        let row = row as usize;
        let col = col as usize;
        self.elements[col + row * (self.width as usize)]
    }

    fn set(&mut self, row: u32, col: u32, value: f32) {
        let row = row as usize;
        let col = col as usize;
        self.elements[col + row * (self.width as usize)] = value;
    }

    fn print(&self) {
        for row in 0..self.height {
            for col in 0..self.width {
                print!("{:3.0}", self.get(row, col));
            }
            println!("");
        }
        println!("");
    }
}

// Matrix addition will perform element-wise addition
fn matrix_addition(first: &Matrix, second: &Matrix) -> Result<Matrix, String> {
    if first.width == second.width && first.height == second.height {
        let mut result = Matrix::new(vec![0.0f32; (first.height * first.width) as usize],
                                     first.height,
                                     first.width);
        for row in 0..first.height {
            for col in 0..first.width {
                let first_value = first.get(row, col);
                let second_value = second.get(row, col);
                result.set(row, col, first_value + second_value);
            }
        }
        Ok(result)
    } else {
        Err("Dimensions don't match".to_owned())
    }
}

fn scalar_multiplication(scalar: f32, matrix: &Matrix) -> Matrix {
    let mut result = Matrix::new(vec![0.0f32; (matrix.height * matrix.width) as usize],
                                 matrix.height,
                                 matrix.width);
    for row in 0..matrix.height {
        for col in 0..matrix.width {
            let value = matrix.get(row, col);
            result.set(row, col, scalar * value);
        }
    }
    result
}

// Subtract second from first
fn matrix_subtraction(first: &Matrix, second: &Matrix) -> Result<Matrix, String> {
    if first.width == second.width && first.height == second.height {
        let negative_matrix = scalar_multiplication(-1.0, second);
        let result = matrix_addition(first, &negative_matrix).unwrap();
        Ok(result)
    } else {
        Err("Dimensions don't match".to_owned())
    }
}

// First must be a l x m matrix and second a m x n matrix for this to work.
fn matrix_multiplication(first: &Matrix, second: &Matrix) -> Result<Matrix, String> {
    if first.width == second.height {
        let mut result = Matrix::new(vec![0.0f32; (first.height * second.width) as usize],
                                     first.height,
                                     second.width);
        for row in 0..result.height {
            for col in 0..result.width {
                let mut value = 0.0;
                for it in 0..first.width {
                    value += first.get(row, it) * second.get(it, col);
                }
                result.set(row, col, value);
            }
        }
        Ok(result)
    } else {
        Err("Dimensions don't match. Width of first must equal height of second".to_owned())
    }
}


fn main() {
    let height = 2;
    let width = 3;
    // Matrix will look like:
    // | 1.0  2.0  3.0  |
    // | 4.0  5.0  6.0 |
    let matrix1 = Matrix::new(vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0], height, width);

    // Matrix will look like:
    // | 6.0  5.0  4.0  |
    // | 3.0  2.0  1.0 |
    let matrix2 = Matrix::new(vec![6.0, 5.0, 4.0, 3.0, 2.0, 1.0], height, width);

    // | 7.0  7.0  7.0  |
    // | 7.0  7.0  7.0 |
    matrix_addition(&matrix1, &matrix2).unwrap().print();
    // | 2.0   4.0   6.0  |
    // | 8.0  10.0  12.0 |
    scalar_multiplication(2.0, &matrix1).print();
    // | -5.0  -3.0  -1.0  |
    // |  1.0   3.0   5.0 |
    matrix_subtraction(&matrix1, &matrix2).unwrap().print();

    // | 1.0 |
    // | 1.0 |
    // | 1.0 |
    let matrix3 = Matrix::new(vec![1.0, 1.0, 1.0], width, 1);
    // |  6 |
    // | 15 |
    matrix_multiplication(&matrix1, &matrix3).unwrap().print();
}
```



## Sidef

The built-in metaoperators `~W<op>`, `~S<op>` and `~RS<op>` are defined for arbitrary nested arrays.

```ruby
var m1 = [[3,1,4],[1,5,9]]
var m2 = [[2,7,1],[8,2,2]]

say ":: Matrix-matrix operations"
say (m1 ~W+  m2)
say (m1 ~W-  m2)
say (m1 ~W*  m2)
say (m1 ~W/  m2)
say (m1 ~W// m2)
say (m1 ~W** m2)
say (m1 ~W%  m2)

say "\n:: Matrix-scalar operations"
say (m1 ~S+  42)
say (m1 ~S-  42)
say (m1 ~S/  42)
say (m1 ~S** 10)
# ...

say "\n:: Scalar-matrix operations"
say (m1 ~RS+  42)
say (m1 ~RS-  42)
say (m1 ~RS/  42)
say (m1 ~RS** 10)
# ...
```

```txt

:: Matrix-matrix operations
[[5, 8, 5], [9, 7, 11]]
[[1, -6, 3], [-7, 3, 7]]
[[6, 7, 4], [8, 10, 18]]
[[3/2, 1/7, 4], [1/8, 5/2, 9/2]]
[[1, 0, 4], [0, 2, 4]]
[[9, 1, 4], [1, 25, 81]]
[[1, 1, 0], [1, 1, 1]]

:: Matrix-scalar operations
[[45, 43, 46], [43, 47, 51]]
[[-39, -41, -38], [-41, -37, -33]]
[[1/14, 1/42, 2/21], [1/42, 5/42, 3/14]]
[[59049, 1, 1048576], [1, 9765625, 3486784401]]

:: Scalar-matrix operations
[[45, 43, 46], [43, 47, 51]]
[[39, 41, 38], [41, 37, 33]]
[[14, 42, 21/2], [42, 42/5, 14/3]]
[[1000, 10, 10000], [10, 100000, 1000000000]]

```



## Standard ML


```sml
structure Matrix = struct
local
    open Array2
    fun mapscalar f (x, scalar) =
	tabulate RowMajor (nRows x, nCols x, fn (i,j) => f(sub(x,i,j),scalar))
    fun map2 f (x, y) =
	tabulate RowMajor (nRows x, nCols x, fn (i,j) => f(sub(x,i,j),sub(y,i,j)))
in
infix splus sminus stimes
val op splus = mapscalar Int.+
val op sminus = mapscalar Int.-
val op stimes = mapscalar Int.*
val op + = map2 Int.+
val op - = map2 Int.-
val op * = map2 Int.*
val fromList = fromList
fun toList a =
    List.tabulate(nRows a, fn i => List.tabulate(nCols a, fn j => sub(a,i,j)))
end
end;

(* example *)
let open Matrix
    infix splus sminus stimes
    val m1 = fromList [[1,2],[3,4]]
    val m2 = fromList [[4,3],[2,1]]
    val s = 2
in
    List.map toList [m1+m2, m1-m2, m1*m2,
		     m1 splus s, m1 sminus s, m1 stimes s]
end;
```

'''Output:'''

```sml
val it =
  [[[5,5],[5,5]],[[~3,~1],[1,3]],[[4,6],[6,4]],[[3,4],[5,6]],[[~1,0],[1,2]],
   [[2,4],[6,8]]] : int list list list
```



## Stata



```stata
mata
a = rnormal(5,5,0,1)
b = 2
a:+b
a:-b
a:*b
a:/b
a:^b

a = rnormal(5,5,0,1)
b = rnormal(5,1,0,1)
a:+b
a:-b
a:*b
a:/b
a:^b
end
```



## Tcl


```tcl
package require Tcl 8.5
proc alias {name args} {uplevel 1 [list interp alias {} $name {} {*}$args]}

# Engine for elementwise operations between matrices
proc elementwiseMatMat {lambda A B} {
    set C {}
    foreach rA $A rB $B {
	set rC {}
	foreach vA $rA vB $rB {
	    lappend rC [apply $lambda $vA $vB]
	}
	lappend C $rC
    }
    return $C
}
# Lift some basic math ops
alias m+  elementwiseMatMat {{a b} {expr {$a+$b}}}
alias m-  elementwiseMatMat {{a b} {expr {$a-$b}}}
alias m*  elementwiseMatMat {{a b} {expr {$a*$b}}}
alias m/  elementwiseMatMat {{a b} {expr {$a/$b}}}
alias m** elementwiseMatMat {{a b} {expr {$a**$b}}}

# Engine for elementwise operations between a matrix and a scalar
proc elementwiseMatSca {lambda A b} {
    set C {}
    foreach rA $A {
	set rC {}
	foreach vA $rA {
	    lappend rC [apply $lambda $vA $b]
	}
	lappend C $rC
    }
    return $C
}
# Lift some basic math ops
alias .+  elementwiseMatSca {{a b} {expr {$a+$b}}}
alias .-  elementwiseMatSca {{a b} {expr {$a-$b}}}
alias .*  elementwiseMatSca {{a b} {expr {$a*$b}}}
alias ./  elementwiseMatSca {{a b} {expr {$a/$b}}}
alias .** elementwiseMatSca {{a b} {expr {$a**$b}}}
```



## zkl


```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
M:=GSL.Matrix(3,3).set(3,5,7, 1,2,3, 2,4,6);
x:=2;
println("M = \n%s\nx = %s".fmt(M.format(),x));
foreach op in (T('+,'-,'*,'/)){
   println("M %s x:\n%s\n".fmt(op.toString()[3,1],op(M.copy(),x).format()));
}
foreach op in (T("addElements","subElements","mulElements","divElements")){
   println("M %s M:\n%s\n".fmt(op, M.copy().resolve(op)(M).format()));
}
mSqrd:=M.pump(0,M.copy(),fcn(x){ x*x });  // M element by element
println("M square elements:\n%s\n".fmt(mSqrd.format()));
```

```txt

M =
      3.00,      5.00,      7.00
      1.00,      2.00,      3.00
      2.00,      4.00,      6.00
x = 2
M + x:
      5.00,      7.00,      9.00
      3.00,      4.00,      5.00
      4.00,      6.00,      8.00

M - x:
      1.00,      3.00,      5.00
     -1.00,      0.00,      1.00
      0.00,      2.00,      4.00

M * x:
      6.00,     10.00,     14.00
      2.00,      4.00,      6.00
      4.00,      8.00,     12.00

M / x:
      1.50,      2.50,      3.50
      0.50,      1.00,      1.50
      1.00,      2.00,      3.00

M addElements M:
      6.00,     10.00,     14.00
      2.00,      4.00,      6.00
      4.00,      8.00,     12.00

M subElements M:
      0.00,      0.00,      0.00
      0.00,      0.00,      0.00
      0.00,      0.00,      0.00

M mulElements M:
      9.00,     25.00,     49.00
      1.00,      4.00,      9.00
      4.00,     16.00,     36.00

M divElements M:
      1.00,      1.00,      1.00
      1.00,      1.00,      1.00
      1.00,      1.00,      1.00

M square elements:
      9.00,     25.00,     49.00
      1.00,      4.00,      9.00
      4.00,     16.00,     36.00

```

