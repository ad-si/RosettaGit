+++
title = "Cramer's rule"
description = ""
date = 2019-09-17T18:54:25Z
aliases = []
[extra]
id = 19985
[taxonomies]
categories = ["task", "Mathematics"]
tags = []
languages = [
  "algol_68",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "factor",
  "fortran",
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "maple",
  "mathematica",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "sidef",
  "tcl",
  "visual_basic_.net",
  "zkl",
]
+++

{{task|Mathematics}} In [[wp:linear algebra|linear algebra]], [[wp:Cramer's rule|Cramer's rule]] is an explicit formula for the solution of a [[wp:system of linear equations|system of linear equations]] with as many equations as unknowns, valid whenever the system has a unique solution. It expresses the solution in terms of the determinants of the (square) coefficient matrix and of matrices obtained from it by replacing one column by the vector of right hand sides of the equations.


Given
<big>
: <math>\left\{\begin{matrix}a_1x + b_1y + c_1z&= {\color{red}d_1}\\a_2x + b_2y + c_2z&= {\color{red}d_2}\\a_3x + b_3y + c_3z&= {\color{red}d_3}\end{matrix}\right.</math>
</big>

which in matrix format is

<big>
: <math>\begin{bmatrix} a_1 & b_1 & c_1 \\ a_2 & b_2 & c_2 \\ a_3 & b_3 & c_3 \end{bmatrix}\begin{bmatrix} x \\ y \\ z \end{bmatrix}=\begin{bmatrix} {\color{red}d_1} \\ {\color{red}d_2} \\ {\color{red}d_3} \end{bmatrix}.</math>
</big>

Then the values of <math>x, y</math> and <math>z</math> can be found as follows:

<big>
:<math>x = \frac{\begin{vmatrix} {\color{red}d_1} & b_1 & c_1 \\ {\color{red}d_2} & b_2 & c_2 \\ {\color{red}d_3} & b_3 & c_3 \end{vmatrix} } { \begin{vmatrix} a_1 & b_1 & c_1 \\ a_2 & b_2 & c_2 \\ a_3 & b_3 & c_3 \end{vmatrix}}, \quad y = \frac {\begin{vmatrix} a_1 & {\color{red}d_1} & c_1 \\ a_2 & {\color{red}d_2} & c_2 \\ a_3 & {\color{red}d_3} & c_3 \end{vmatrix}} {\begin{vmatrix} a_1 & b_1 & c_1 \\ a_2 & b_2 & c_2 \\ a_3 & b_3 & c_3 \end{vmatrix}}, \text{ and }z = \frac { \begin{vmatrix} a_1 & b_1 & {\color{red}d_1} \\ a_2 & b_2 & {\color{red}d_2} \\ a_3 & b_3 & {\color{red}d_3} \end{vmatrix}} {\begin{vmatrix} a_1 & b_1 & c_1 \\ a_2 & b_2 & c_2 \\ a_3 & b_3 & c_3 \end{vmatrix} }.</math>
</big>


## Task

Given the following system of equations:

<big>
:<math>
\begin{cases}
2w-x+5y+z=-3 \\
3w+2x+2y-6z=-32 \\
w+3x+3y-z=-47 \\
5w-2x-3y+3z=49 \\
\end{cases}
</math>
</big>

solve for <big><math>w</math>, <math>x</math>, <math>y</math></big> and <big><math>z</math></big>, using Cramer's rule.





## ALGOL 68

Uses the non-standard DET operator available in Algol 68G.

```algol68
# returns the solution of a.x = b via Cramer's rule                    #
#         this is for REAL arrays, could define additional operators   #
#         for INT, COMPL, etc.                                         #
PRIO CRAMER = 1;
OP   CRAMER = ( [,]REAL a, []REAL b )[]REAL:
     IF 1 UPB a /= 2 UPB a
     OR 1 LWB a /= 2 LWB a
     OR 1 UPB a /=   UPB b
     THEN
        # the array sizes and bounds do not match                       #
        print( ( "Invaid parameters to CRAMER", newline ) );
        stop
     ELIF REAL deta = DET a;
          det a = 0
     THEN
        # a is singular                                                 #
        print( ( "Singular matrix for CRAMER", newline ) );
        stop
     ELSE
        # the arrays have matching bounds                               #
        [ LWB b : UPB b ]REAL result;
        FOR col FROM LWB b TO UPB b DO
            # form a matrix from a with the col'th column replaced by b #
            [ 1 LWB a : 1 UPB a, 2 LWB a : 2 UPB a ]REAL m := a;
            m[ : , col ] := b[ : AT 1 ];
            # col'th result elemet as per Cramer's rule                 #
            result[ col ] := DET m / det a
        OD;
        result
     FI; # CRAMER #

# test CRAMER using the matrix and column vector specified in the task  #
[,]REAL a = ( (  2, -1,  5,  1 )
            , (  3,  2,  2, -6 )
            , (  1,  3,  3, -1 )
            , (  5, -2, -3,  3 )
            );
[]REAL  b = (  -3
            , -32
            , -47
            ,  49
            );
[]REAL  solution = a CRAMER b;
FOR c FROM LWB solution TO UPB solution DO
    print( ( " ", fixed( solution[ c ], -8, 4 ) ) )
OD;
print( ( newline ) )

```

```txt

   2.0000 -12.0000  -4.0000   1.0000

```



## C



```C>#include <math.h

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int n;
    double **elems;
} SquareMatrix;

SquareMatrix init_square_matrix(int n, double elems[n][n]) {
    SquareMatrix A = {
        .n = n,
        .elems = malloc(n * sizeof(double *))
    };
    for(int i = 0; i < n; ++i) {
        A.elems[i] = malloc(n * sizeof(double));
        for(int j = 0; j < n; ++j)
            A.elems[i][j] = elems[i][j];
    }

    return A;
}

SquareMatrix copy_square_matrix(SquareMatrix src) {
    SquareMatrix dest;
    dest.n = src.n;
    dest.elems = malloc(dest.n * sizeof(double *));
    for(int i = 0; i < dest.n; ++i) {
        dest.elems[i] = malloc(dest.n * sizeof(double));
        for(int j = 0; j < dest.n; ++j)
            dest.elems[i][j] = src.elems[i][j];
    }

    return dest;
}

double det(SquareMatrix A) {
    double det = 1;

    for(int j = 0; j < A.n; ++j) {
        int i_max = j;
        for(int i = j; i < A.n; ++i)
            if(A.elems[i][j] > A.elems[i_max][j])
                i_max = i;

        if(i_max != j) {
            for(int k = 0; k < A.n; ++k) {
                double tmp = A.elems[i_max][k];
                A.elems[i_max][k] = A.elems[j][k];
                A.elems[j][k]     = tmp;
            }

            det *= -1;
        }

        if(abs(A.elems[j][j]) < 1e-12) {
            puts("Singular matrix!");
            return NAN;
        }

        for(int i = j + 1; i < A.n; ++i) {
            double mult = -A.elems[i][j] / A.elems[j][j];
            for(int k = 0; k < A.n; ++k)
                A.elems[i][k] += mult * A.elems[j][k];
        }
    }

    for(int i = 0; i < A.n; ++i)
        det *= A.elems[i][i];

    return det;
}

void deinit_square_matrix(SquareMatrix A) {
    for(int i = 0; i < A.n; ++i)
        free(A.elems[i]);
    free(A.elems);
}

double cramer_solve(SquareMatrix A, double det_A, double *b, int var) {
    SquareMatrix tmp = copy_square_matrix(A);
    for(int i = 0; i < tmp.n; ++i)
        tmp.elems[i][var] = b[i];

    double det_tmp = det(tmp);
    deinit_square_matrix(tmp);

    return det_tmp / det_A;
}

int main(int argc, char **argv) {
#define N 4
    double elems[N][N] = {
        { 2, -1,  5,  1},
        { 3,  2,  2, -6},
        { 1,  3,  3, -1},
        { 5, -2, -3,  3}
    };
    SquareMatrix A = init_square_matrix(N, elems);

    SquareMatrix tmp = copy_square_matrix(A);
    int det_A = det(tmp);
    deinit_square_matrix(tmp);

    double b[] = {-3, -32, -47, 49};

    for(int i = 0; i < N; ++i)
        printf("%7.3lf\n", cramer_solve(A, det_A, b, i));

    deinit_square_matrix(A);
    return EXIT_SUCCESS;
}
```


```txt
  2.000
-12.000
 -4.000
  1.000
```



## C++

```cpp
#include <algorithm>
#include <iostream>
#include <vector>

class SubMatrix {
    const std::vector<std::vector<double>> *source;
    std::vector<double> replaceColumn;
    const SubMatrix *prev;
    size_t sz;
    int colIndex = -1;

public:
    SubMatrix(const std::vector<std::vector<double>> &src, const std::vector<double> &rc) : source(&src), replaceColumn(rc), prev(nullptr), colIndex(-1) {
        sz = replaceColumn.size();
    }

    SubMatrix(const SubMatrix &p) : source(nullptr), prev(&p), colIndex(-1) {
        sz = p.size() - 1;
    }

    SubMatrix(const SubMatrix &p, int deletedColumnIndex) : source(nullptr), prev(&p), colIndex(deletedColumnIndex) {
        sz = p.size() - 1;
    }

    int columnIndex() const {
        return colIndex;
    }
    void columnIndex(int index) {
        colIndex = index;
    }

    size_t size() const {
        return sz;
    }

    double index(int row, int col) const {
        if (source != nullptr) {
            if (col == colIndex) {
                return replaceColumn[row];
            } else {
                return (*source)[row][col];
            }
        } else {
            if (col < colIndex) {
                return prev->index(row + 1, col);
            } else {
                return prev->index(row + 1, col + 1);
            }
        }
    }

    double det() const {
        if (sz == 1) {
            return index(0, 0);
        }
        if (sz == 2) {
            return index(0, 0) * index(1, 1) - index(0, 1) * index(1, 0);
        }
        SubMatrix m(*this);
        double det = 0.0;
        int sign = 1;
        for (size_t c = 0; c < sz; ++c) {
            m.columnIndex(c);
            double d = m.det();
            det += index(0, c) * d * sign;
            sign = -sign;
        }
        return det;
    }
};

std::vector<double> solve(SubMatrix &matrix) {
    double det = matrix.det();
    if (det == 0.0) {
        throw new std::runtime_error("The determinant is zero.");
    }

    std::vector<double> answer(matrix.size());
    for (int i = 0; i < matrix.size(); ++i) {
        matrix.columnIndex(i);
        answer[i] = matrix.det() / det;
    }
    return answer;
}

std::vector<double> solveCramer(const std::vector<std::vector<double>> &equations) {
    int size = equations.size();
    if (std::any_of(
        equations.cbegin(), equations.cend(),
        [size](const std::vector<double> &a) { return a.size() != size + 1; }
    )) {
        throw new std::runtime_error("Each equation must have the expected size.");
    }

    std::vector<std::vector<double>> matrix(size);
    std::vector<double> column(size);
    for (int r = 0; r < size; ++r) {
        column[r] = equations[r][size];
        matrix[r].resize(size);
        for (int c = 0; c < size; ++c) {
            matrix[r][c] = equations[r][c];
        }
    }

    SubMatrix sm(matrix, column);
    return solve(sm);
}

template<typename T>
std::ostream &operator<<(std::ostream &os, const std::vector<T> &v) {
    auto it = v.cbegin();
    auto end = v.cend();

    os << '[';
    if (it != end) {
        os << *it++;
    }
    while (it != end) {
        os << ", " << *it++;
    }

    return os << ']';
}

int main() {
    std::vector<std::vector<double>> equations = {
        { 2, -1,  5,  1,  -3},
        { 3,  2,  2, -6, -32},
        { 1,  3,  3, -1, -47},
        { 5, -2, -3,  3,  49},
    };

    auto solution = solveCramer(equations);
    std::cout << solution << '\n';

    return 0;
}
```

```txt
[2, -12, -4, 1]
```



## C#

Instead of copying a bunch of arrays, I made a class with an indexer that simply accesses the correct items in the original matrix.

```c#
using System;
using System.Collections.Generic;
using static System.Linq.Enumerable;

public static class CramersRule
{
    public static void Main() {
        var equations = new [] {
            new [] { 2, -1,  5,  1,  -3 },
            new [] { 3,  2,  2, -6, -32 },
            new [] { 1,  3,  3, -1, -47 },
            new [] { 5, -2, -3,  3,  49 }
        };
        var solution = SolveCramer(equations);
        Console.WriteLine(solution.DelimitWith(", "));
    }

    public static int[] SolveCramer(int[][] equations) {
        int size = equations.Length;
        if (equations.Any(eq => eq.Length != size + 1)) throw new ArgumentException($"Each equation must have {size+1} terms.");
        int[,] matrix = new int[size, size];
        int[] column = new int[size];
        for (int r = 0; r < size; r++) {
            column[r] = equations[r][size];
            for (int c = 0; c < size; c++) {
                matrix[r, c] = equations[r][c];
            }
        }
        return Solve(new SubMatrix(matrix, column));
    }

    private static int[] Solve(SubMatrix matrix) {
        int det = matrix.Det();
        if (det == 0) throw new ArgumentException("The determinant is zero.");

        int[] answer = new int[matrix.Size];
        for (int i = 0; i < matrix.Size; i++) {
            matrix.ColumnIndex = i;
            answer[i] = matrix.Det() / det;
        }
        return answer;
    }

    //Extension method from library.
    static string DelimitWith<T>(this IEnumerable<T> source, string separator = " ") =>
        string.Join(separator ?? " ", source ?? Empty<T>());

    private class SubMatrix
    {
        private int[,] source;
        private SubMatrix prev;
        private int[] replaceColumn;

        public SubMatrix(int[,] source, int[] replaceColumn) {
            this.source = source;
            this.replaceColumn = replaceColumn;
            this.prev = null;
            this.ColumnIndex = -1;
            Size = replaceColumn.Length;
        }

        private SubMatrix(SubMatrix prev, int deletedColumnIndex = -1) {
            this.source = null;
            this.prev = prev;
            this.ColumnIndex = deletedColumnIndex;
            Size = prev.Size - 1;
        }

        public int ColumnIndex { get; set; }
        public int Size { get; }

        public int this[int row, int column] {
            get {
                if (source != null) return column == ColumnIndex ? replaceColumn[row] : source[row, column];
                return prev[row + 1, column < ColumnIndex ? column : column + 1];
            }
        }

        public int Det() {
            if (Size == 1) return this[0, 0];
            if (Size == 2) return this[0, 0] * this[1, 1] - this[0, 1] * this[1, 0];
            SubMatrix m = new SubMatrix(this);
            int det = 0;
            int sign = 1;
            for (int c = 0; c < Size; c++) {
                m.ColumnIndex = c;
                int d = m.Det();
                det += this[0, c] * d * sign;
                sign = -sign;
            }
            return det;
        }

        public void Print() {
            for (int r = 0; r < Size; r++) {
                Console.WriteLine(Range(0, Size).Select(c => this[r, c]).DelimitWith(", "));
            }
            Console.WriteLine();
        }
    }

}
```

```txt

2, -12, -4, 1

```



## Common Lisp


```lisp
(defun minor (m col)
  (loop with dim = (1- (array-dimension m 0))
        with result = (make-array (list dim dim))
        for i from 1 to dim
        for r = (1- i)
        do (loop with c = 0
                 for j to dim
                 when (/= j col)
                   do (setf (aref result r c) (aref m i j))
                      (incf c))
        finally (return result)))

(defun det (m)
  (assert (= (array-rank m) 2))
  (assert (= (array-dimension m 0) (array-dimension m 1)))
  (let ((dim (array-dimension m 0)))
    (if (= dim 1)
        (aref m 0 0)
        (loop for col below dim
              for sign = 1 then (- sign)
              sum (* sign (aref m 0 col) (det (minor m col)))))))

(defun replace-column (m col values)
  (let* ((dim (array-dimension m 0))
         (result (make-array (list dim dim))))
    (dotimes (r dim result)
      (dotimes (c dim)
        (setf (aref result r c)
              (if (= c col) (aref values r) (aref m r c)))))))

(defun solve (m v)
  (loop with dim = (array-dimension m 0)
        with det = (det m)
        for col below dim
        collect (/ (det (replace-column m col v)) det)))

(solve #2A((2 -1  5  1)
           (3  2  2 -6)
           (1  3  3 -1)
           (5 -2 -3  3))
       #(-3 -32 -47 49))
```

```txt
(2 -12 -4 1)
```



## D

```d
import std.array : array, uninitializedArray;
import std.range : iota;
import std.stdio : writeln;
import std.typecons : tuple;

alias vector = double[4];
alias matrix = vector[4];

auto johnsonTrotter(int n) {
    auto p = iota(n).array;
    auto q = iota(n).array;
    auto d = uninitializedArray!(int[])(n);
    d[] = -1;
    auto sign = 1;
    int[][] perms;
    int[] signs;

    void permute(int k) {
        if (k >= n) {
            perms ~= p.dup;
            signs ~= sign;
            sign *= -1;
            return;
        }
        permute(k + 1);
        foreach (i; 0..k) {
            auto z = p[q[k] + d[k]];
            p[q[k]] = z;
            p[q[k] + d[k]] = k;
            q[z] = q[k];
            q[k] += d[k];
            permute(k + 1);
        }
        d[k] *= -1;
    }

    permute(0);
    return tuple!("sigmas", "signs")(perms, signs);
}

auto determinant(matrix m) {
    auto jt = johnsonTrotter(m.length);
    auto sum = 0.0;
    foreach (i,sigma; jt.sigmas) {
        auto prod = 1.0;
        foreach (j,s; sigma) {
            prod *= m[j][s];
        }
        sum += jt.signs[i] * prod;
    }
    return sum;
}

auto cramer(matrix m, vector d) {
    auto divisor = determinant(m);
    auto numerators = uninitializedArray!(matrix[])(m.length);
    foreach (i; 0..m.length) {
        foreach (j; 0..m.length) {
            foreach (k; 0..m.length) {
                numerators[i][j][k] = m[j][k];
            }
        }
    }
    vector v;
    foreach (i; 0..m.length) {
        foreach (j; 0..m.length) {
            numerators[i][j][i] = d[j];
        }
    }
    foreach (i; 0..m.length) {
        v[i] = determinant(numerators[i]) / divisor;
    }
    return v;
}

void main() {
    matrix m = [
        [2.0, -1.0,  5.0,  1.0],
        [3.0,  2.0,  2.0, -6.0],
        [1.0,  3.0,  3.0, -1.0],
        [5.0, -2.0, -3.0,  3.0]
    ];
    vector d = [-3.0, -32.0, -47.0, 49.0];
    auto wxyz = cramer(m, d);
    writeln("w = ", wxyz[0], ", x = ", wxyz[1], ", y = ", wxyz[2], ", z = ", wxyz[3]);
}
```

```txt
w = 2, x = -12, y = -4, z = 1
```



## EchoLisp


```scheme

(lib 'matrix)
(string-delimiter "")
(define (cramer A B (X)) ;; --> vector X
	(define ∆ (determinant A))
	(for/vector [(i (matrix-col-num A))]
		(set! X (matrix-set-col! (array-copy A) i B))
		(// (determinant X) ∆)))

(define (task)
	(define A (list->array
  	'( 2 -1 5 1 3 2 2 -6 1 3 3 -1 5 -2 -3 3) 4 4))
	(define B #(-3 -32 -47 49))
	(writeln "Solving A * X = B")
	(array-print A)
	(writeln "B = " B)
	(writeln "X = " (cramer A B)))

```

```txt

(task)
Solving A * X = B
  2   -1   5    1
  3   2    2    -6
  1   3    3    -1
  5   -2   -3   3
B =      #( -3 -32 -47 49)
X =      #( 2 -12 -4 1)

```



## Factor


```factor
USING: kernel math math.matrices.laplace prettyprint sequences ;
IN: rosetta-code.cramers-rule

: replace-col ( elt n seq -- seq' ) flip [ set-nth ] keep flip ;

: solve ( m v -- seq )
    dup length <iota> [
        rot [ replace-col ] keep [ determinant ] bi@ /
    ] 2with map ;

: cramers-rule-demo ( -- )
    {
        { 2 -1  5  1 }
        { 3  2  2 -6 }
        { 1  3  3 -1 }
        { 5 -2 -3  3 }
    }
    { -3 -32 -47 49 } solve . ;

MAIN: cramers-rule-demo
```

```txt

{ 2 -12 -4 1 }

```



## Fortran

In ''Numerical Methods That Work (Usually)'', in the section What ''not'' to compute, F. S. Acton remarks "...perhaps we should be glad he didn't resort to Cramer's rule (still taught as the practical method in some high schools) and solve his equations as the ratios of determinants - a process that requires labor proportional to N! if done in the schoolboy manner. The contrast with N<sup>3</sup> can be startling!"
And further on, "Having hinted darkly at my computational fundamentalism, it is probably time to commit to a public heresy by denouncing recursive calculations. I have ''never'' seen a numerical problem arising from the physical world that was best calculated by a recursive subroutine..."

Since this problem requires use of Cramer's rule, one might as well be hung for a sheep instead of a lamb, so the traditions of Old Fortran and heavy computation will be ignored and the fearsome RECURSIVE specification employed so that the determinants will be calculated recursively, all the way down to N = 1 even though the N = 2 case is easy. This requires F90 and later. Similarly, the MODULE protocol will be employed, even though there is no significant context to share. The alternative method for calculating a determinant involves generating permutations, a tiresome process.

Array passing via the modern arrangements of F90 is a source of novel difficulty to set against the slight convenience of not having to pass an additional parameter, N. Explicitly, at least. There are "secret" additional parameters when an array is being passed in the modern way, which are referred to by the new SIZE function. Anyway, for an order N square matrix, the array ''must'' be declared A(N,N), and specifically not something like A(100,100) with usage only of elements up to N = 7, say, because the locations in storage of elements in use would be quite different from those used by an array declared A(7,7). This means that the array must be re-declared for each different size usage, a tiresome and error-inviting task. One-dimensional arrays do not have this problem, but they do have to be "long enough" so B and X might as well be included. This also means that the auxiliary matrices needed within the routines have to be made the right size, and fortunately they can be declared in a way that requests this without the blather of ALLOCATE, this being a protocol introduced by Algol in the 1960s. Unfortunately, there is no scheme such as in pl/i to declare AUX "like" A, so some grotesquery results. And in the case of function DET which needs an array of order N - 1, when its recursion bottoms out with N = 1 it will have declared MINOR(0,0), a rather odd situation that fortunately evokes no complaint, and a test run in which its "value" was written out by WRITE (6,*) MINOR produced a blank line: no complaint there either, presumably because zero elements were being sent forth and so there was no improper access of ... nothing.

With matrices, there is a problem all the way from the start in 1958. Everyone agrees that a matrix should be indexed as A(''row'',''column'') and that when written out, rows should run down the page and columns across. This is unexceptional and the F90 function MATMUL follows this usage. However, Fortran has always stored its array elements in what is called "column major" order, which is to say that element A(1,1) is followed by element A(2,1) in storage, not A(1,2). Thus, if an array is written (or read) by something like <code>WRITE (6,*) A</code>, consecutive elements, written along a line, will be A(1,1), A(2,1), A(3,1), ... So, subroutine SHOWMATRIX is employed to write the matrix out in the desired form, and to read the values into the array, an explicit loop is used to place them where expected rather than just <code>READ(INF,*) A</code>

Similarly, if instead a DATA statement were used to initialise the array for the example problem, and it looked something like
```Fortran
      DATA A/2, -1,  5,  1
     1       3,  2,  2, -6
     2       1,  3,  3, -1
     3       5, -2, -3,  3/
```
 (ignoring integer/floating-point issues) thus corresponding to the layout of the example problem, there would need to be a statement <code>A = TRANSPOSE(A)</code> to obtain the required order.

I have never seen an explanation of why this choice was made for Fortran.
```Fortran
      MODULE BAD IDEA	!Employ Cramer's rule for solving A.x = b...
       INTEGER MSG	!Might as well be here.
       CONTAINS		!The details.
        SUBROUTINE SHOWMATRIX(A)	!With nice vertical bars.
         DOUBLE PRECISION A(:,:)	!The matrix.
         INTEGER R,N			!Assistants.
          N = SIZE(A, DIM = 1)		!Instead of passing an explicit parameter.
          DO R = 1,N			!Work down the rows.
            WRITE (MSG,1) A(R,:)		!Fling forth a row at a go.
    1       FORMAT ("|",<N>F12.3,"|")		!Bounded by bars.
          END DO			!On to the next row.
        END SUBROUTINE SHOWMATRIX	!Furrytran's default order is the transpose.

        RECURSIVE DOUBLE PRECISION FUNCTION DET(A)	!Determine the determinant.
         DOUBLE PRECISION A(:,:)	!The square matrix, order N.
         DOUBLE PRECISION MINOR(SIZE(A,DIM=1) - 1,SIZE(A,DIM=1) - 1)	!Order N - 1.
         DOUBLE PRECISION D		!A waystation.
         INTEGER C,N			!Steppers.
          N = SIZE(A, DIM = 1)		!Suplied via secret parameters.
          IF (N .LE. 0) THEN		!This really ought not happen.
            STOP "DET: null array!"		!But I'm endlessly suspicious.
          ELSE IF (N .NE. SIZE(A, DIM = 2)) THEN	!And I'd rather have a decent message
            STOP "DET: not a square array!"	!In place of a crashed run.
          ELSE IF (N .EQ. 1) THEN	!Alright, now get on with it.
            DET = A(1,1)		!This is really easy.
          ELSE				!But otherwise,
            D = 0			!Here we go.
            DO C = 1,N			!Step along the columns of the first row.
              CALL FILLMINOR(C)			!Produce the auxiliary array for each column.
              IF (MOD(C,2) .EQ. 0) THEN		!Odd or even case?
                D = D - A(1,C)*DET(MINOR)	!Even: subtract.
               ELSE				!Otherwise,
                D = D + A(1,C)*DET(MINOR)	!Odd: add.
              END IF				!So much for that term.
            END DO			!On to the next.
            DET = D		!Declare the result.
          END IF	!So much for that.
         CONTAINS	!An assistant.
           SUBROUTINE FILLMINOR(CC)	!Corresponding to A(1,CC).
           INTEGER CC	!The column being omitted.
           INTEGER R	!A stepper.
           DO R = 2,N		!Ignoring the first row,
             MINOR(R - 1,1:CC - 1) = A(R,1:CC - 1)	!Copy columns 1 to CC - 1. There may be none.
             MINOR(R - 1,CC:) = A(R,CC + 1:)		!And from CC + 1 to N. There may be none.
           END DO		!On to the next row.
          END SUBROUTINE FILLMINOR	!Divide and conquer.
        END FUNCTION DET	!Rather than mess with permutations.

        SUBROUTINE CRAMER(A,X,B)	!Solve A.x = b, where A is a matrix...
Careful! The array must be A(N,N), and not say A(100,100) of which only up to N = 6 are in use.
         DOUBLE PRECISION A(:,:)	!A square matrix. I hope.
         DOUBLE PRECISION X(:),B(:)	!Column matrices look rather like 1-D arrays.
         DOUBLE PRECISION AUX(SIZE(A,DIM=1),SIZE(A,DIM=1))	!Can't say "LIKE A", as in pl/i, alas.
         DOUBLE PRECISION D	!To be calculated once.
         INTEGER N		!The order of the square matrix. I hope.
         INTEGER C		!A stepper.
          N = SIZE(A, DIM = 1)	!Alright, what's the order of battle?
          D = DET(A)		!Once only.
          IF (D.EQ.0) STOP "Cramer: zero determinant!"	!Surely, this won't happen...
          AUX = A		!Prepare the assistant.
          DO C = 1,N		!Step across the columns.
            IF (C.GT.1) AUX(1:N,C - 1) = A(1:N,C - 1)	!Repair previous damage.
            AUX(1:N,C) = B(1:N)		!Place the current damage.
            X(C) = DET(AUX)/D		!The result!
          END DO		!On to the next column.
        END SUBROUTINE CRAMER	!This looks really easy!
      END MODULE BAD IDEA	!But actually, it is a bad idea for N > 2.

      PROGRAM TEST	!Try it and see.
      USE BAD IDEA	!Just so.
      DOUBLE PRECISION, ALLOCATABLE ::A(:,:), B(:), X(:)	!Custom work areas.
      INTEGER N,R	!Assistants..
      INTEGER INF	!An I/O unit.

      MSG = 6		!Output.
      INF = 10		!For reading test data.
      OPEN (INF,FILE="Test.dat",STATUS="OLD",ACTION="READ")	!As in this file..

Chew into the next problem.
   10 IF (ALLOCATED(A)) DEALLOCATE(A)	!First,
      IF (ALLOCATED(B)) DEALLOCATE(B)	!Get rid of
      IF (ALLOCATED(X)) DEALLOCATE(X)	!The hired help.
      READ (INF,*,END = 100) N		!Since there is a new order.
      IF (N.LE.0) GO TO 100		!Perhaps a final order.
      WRITE (MSG,11) N			!Othewise, announce prior to acting.
   11 FORMAT ("Order ",I0," matrix A, as follows...")	!In case something goes wrong.
      ALLOCATE(A(N,N))			!For instance,
      ALLOCATE(B(N))			!Out of memory.
      ALLOCATE(X(N))			!But otherwise, a tailored fit.
      DO R = 1,N			!Now read in the values for the matrix.
        READ(INF,*,END=667,ERR=665) A(R,:),B(R)	!One row of A at a go, followed by B's value.
      END DO				!In free format.
      CALL SHOWMATRIX(A)		!Show what we have managed to obtain.
      WRITE (MSG,12) "In the scheme A.x = b, b = ",B	!In case something goes wrong.
   12 FORMAT (A,<N>F12.6)		!How many would be too many?
      CALL CRAMER(A,X,B)		!The deed!
      WRITE (MSG,12) "    Via Cramer's rule, x = ",X	!The result!
      GO TO 10				!And try for another test problem.

Complaints.
  665 WRITE (MSG,666) "Format error",R	!I know where I came from.
  666 FORMAT (A," while reading row ",I0,"!")	!So I can refer to R.
      GO TO 100		!So much for that.
  667 WRITE (MSG,666) "End-of-file", R		!Some hint as to where.

Closedown.
  100 WRITE (6,*) "That was interesting."	!Quite.
      END	!Open files are closed, allocated memory is released.
```


Oddly, the Compaq Visual Fortran F90/95 compiler is confused by the usage "BAD IDEA" instead of "BADIDEA" - spaces are not normally relevant in Fortran source. Anyway, file Test.dat has been filled with the numbers of the example, as follows:

```txt

4           /The order, for A.x = b.
2  -1   5   1,  -3    /First row of A, b
3   2   2  -6, -32    /Second row...
1   3   3  -1, -47     third row.
5  -2  -3   3,  49    /Last row.


```

Fortran's free-form allows a comma, a tab, and spaces between numbers, and regards the / as starting a comment, but, because each row is read separately, once the required five (N + 1) values have been read, no further scanning of the line takes place and the next READ statement will start with a new line of input. So the / isn't needed for the third row, as shown. Omitted values lead to confusion as the input process would read additional lines to fill the required count and everything gets out of step. Echoing input very soon after it is obtained is helpful in making sense of such mistakes.

For more practical use it would probably be better to constrain the freedom somewhat, perhaps requiring that all the N + 1 values for a row appear on one input record. In such a case, the record could first be read into a text variable (from which the data would be read) so that if a problem arises the text could be printed as a part of the error message. But, this requires guessing a suitably large length for the text variable so as to accommodate the longest possible input line.

Output:

```txt

Order 4 matrix A, as follows...
|       2.000      -1.000       5.000       1.000|
|       3.000       2.000       2.000      -6.000|
|       1.000       3.000       3.000      -1.000|
|       5.000      -2.000      -3.000       3.000|
In the scheme A.x = b, b =    -3.000000  -32.000000  -47.000000   49.000000
    Via Cramer's rule, x =     2.000000  -12.000000   -4.000000    1.000000
 That was interesting.

```

And at this point I suddenly noticed that the habits of Old Fortran are not so easily suppressed: all calculations are done with double precision. Curiously enough, for the specific example data, the same results are obtained if all variables are integer.


## Go

'''Library gonum:'''

```go
package main

import (
    "fmt"

    "gonum.org/v1/gonum/mat"
)

var m = mat.NewDense(4, 4, []float64{
    2, -1, 5, 1,
    3, 2, 2, -6,
    1, 3, 3, -1,
    5, -2, -3, 3,
})

var v = []float64{-3, -32, -47, 49}

func main() {
    x := make([]float64, len(v))
    b := make([]float64, len(v))
    d := mat.Det(m)
    for c := range v {
        mat.Col(b, c, m)
        m.SetCol(c, v)
        x[c] = mat.Det(m) / d
        m.SetCol(c, b)
    }
    fmt.Println(x)
}
```

```txt

[2 -12.000000000000007 -4.000000000000001 1.0000000000000009]

```

'''Library go.matrix:'''

```go
package main

import (
    "fmt"

    "github.com/skelterjohn/go.matrix"
)

var m = matrix.MakeDenseMatrixStacked([][]float64{
    {2, -1, 5, 1},
    {3, 2, 2, -6},
    {1, 3, 3, -1},
    {5, -2, -3, 3},
})

var v = []float64{-3, -32, -47, 49}

func main() {
    x := make([]float64, len(v))
    b := make([]float64, len(v))
    d := m.Det()
    for c := range v {
        m.BufferCol(c, b)
        m.FillCol(c, v)
        x[c] = m.Det() / d
        m.FillCol(c, b)
    }
    fmt.Println(x)
}
```

```txt

[2.0000000000000004 -11.999999999999998 -4 0.9999999999999999]

```



## Haskell


### Version 1


```haskell
import Data.Matrix

solveCramer :: (Ord a, Fractional a) => Matrix a -> Matrix a -> Maybe [a]
solveCramer a y
  | da == 0 = Nothing
  | otherwise = Just $ map (\i -> d i / da) [1..n]
  where da = detLU a
        d i = detLU $ submatrix 1 n 1 n $ switchCols i (n+1) ay
        ay = a <|> y
        n = ncols a

task = solveCramer a y
  where a = fromLists [[2,-1, 5, 1]
                      ,[3, 2, 2,-6]
                      ,[1, 3, 3,-1]
                      ,[5,-2,-3, 3]]
        y = fromLists [[-3], [-32], [-47], [49]]
```

 λ> task
 Just [2.0000000000000004,-11.999999999999998,-4.0,0.9999999999999999]

### Version 2

We use Rational numbers for having more precision.
a % b is the rational a / b.

```Haskell

s_permutations :: [a] -> [([a], Int)]
s_permutations = flip zip (cycle [1, -1]) . (foldl aux [[]])
  where aux items x = do
          (f,item) <- zip (cycle [reverse,id]) items
          f (insertEv x item)
        insertEv x [] = [[x]]
        insertEv x l@(y:ys) = (x:l) :  map (y:) (insertEv x ys)

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

elemPos::[[a]] -> Int -> Int -> a
elemPos ms i j = (ms !! i) !! j

prod:: Num a => ([[a]] -> Int -> Int -> a) -> [[a]] -> [Int] -> a
prod f ms = product.zipWith (f ms) [0..]

s_determinant:: Num a => ([[a]] -> Int -> Int -> a) -> [[a]] -> [([Int],Int)] -> a
s_determinant f ms = sum.map (\(is,s) -> fromIntegral s * prod f ms is)

elemCramerPos::Int -> Int -> [[a]] -> [[a]] -> Int -> Int -> a
elemCramerPos l k ks ms i j = if j /= l then elemPos ms i j else elemPos ks i k

solveCramer:: [[Rational]] -> [[Rational]] -> [[Rational]]
solveCramer ms ks = xs
  where
  xs | d /= 0    = go (reverse [0..pred.length.head $ ks])
     | otherwise = []
  go (u:us) = foldl glue (col u) us
  glue us u = zipWith (\ys (y:_) -> y:ys) us (col u)
  col k = map (\l -> [(/d) $ s_determinant (elemCramerPos l k ks) ms ps]) $ ls
  ls = [0..pred.length $ ms]
  ps = s_permutations ls
  d = s_determinant elemPos ms ps
matI::(Num a) => Int -> [[a]]
matI n = [ [fromIntegral.fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

task::[[Rational]] -> [[Rational]] -> IO()
task a b = do
  let x         = solveCramer a b
  let u         = map (map fromRational) x
  let y         = mult a x
  let identity  = matI (length x)
  let a1        = solveCramer a identity
  let h         = mult a a1
  let z         = mult a1 b
  putStrLn "a ="
  mapM_ print a
  putStrLn "b ="
  mapM_ print b
  putStrLn "solve: a * x = b => x = solveCramer a b ="
  mapM_ print x
  putStrLn "u = fromRationaltoDouble x ="
  mapM_ print u
  putStrLn "verification: y = a * x = mult a x ="
  mapM_ print y
  putStrLn $ "test: y == b = "
  print $ y == b
  putStrLn "identity matrix: identity ="
  mapM_ print identity
  putStrLn "find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveCramer a identity ="
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
  let a  = [[2,-1, 5, 1]
           ,[3, 2, 2,-6]
           ,[1, 3, 3,-1]
           ,[5,-2,-3, 3]]
  let b   =  [[-3], [-32], [-47], [49]]
  task a b

```

```txt

a =
[2 % 1,(-1) % 1,5 % 1,1 % 1]
[3 % 1,2 % 1,2 % 1,(-6) % 1]
[1 % 1,3 % 1,3 % 1,(-1) % 1]
[5 % 1,(-2) % 1,(-3) % 1,3 % 1]
b =
[(-3) % 1]
[(-32) % 1]
[(-47) % 1]
[49 % 1]
solve: a * x = b => x = solveCramer a b =
[2 % 1]
[(-12) % 1]
[(-4) % 1]
[1 % 1]
u = fromRationaltoDouble x =
[2.0]
[-12.0]
[-4.0]
[1.0]
verification: y = a * x = mult a x =
[(-3) % 1]
[(-32) % 1]
[(-47) % 1]
[49 % 1]
test: y == b =
True
identity matrix: identity =
[1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1]
find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveCramer a identity =
[4 % 171,11 % 171,10 % 171,8 % 57]
[(-55) % 342,(-23) % 342,119 % 342,2 % 57]
[107 % 684,(-5) % 684,11 % 684,(-7) % 114]
[7 % 684,(-109) % 684,103 % 684,7 % 114]
verification: h = a * a1 = mult a a1 =
[1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1]
test: h == identity =
True
z = a1 * b = mult a1 b =
[2 % 1]
[(-12) % 1]
[(-4) % 1]
[1 % 1]
test: z == x =
True

```


### Version 3


```Haskell

import Data.List

determinant::(Fractional a, Ord a) => [[a]] -> a
determinant ls = if null ls then 0 else pivot 1 (zip ls [(0::Int)..])
  where
  good rs ts = (abs.head.fst $ ts) <= (abs.head.fst $ rs)
  go us (vs,i) = if v == 0 then (ws,i) else (zipWith (\x y -> y - x*v) us ws,i)
    where (v,ws) = (head $ vs,tail vs)
  change i (ys:zs) = map (\xs -> if (==i).snd $ xs then ys else xs) zs
  pivot d [] = d
  pivot d zs@((_,j):ys) = if 0 == u then 0 else pivot e ws
    where
    e = if i == j then u*d else -u*d
    ((u:us),i) = foldl1 (\rs ts ->  if good rs ts then rs else ts) zs
    ws = map (go (map (/u) us)) $ if i == j then ys else change i zs

solveCramer::(Fractional a, Ord a) => [[a]] -> [[a]] -> [[a]]
solveCramer as bs = if 0 == d then [] else ans bs
  where
  d = determinant as
  ans = transpose.map go.transpose
    where
    ms = zip [0..] (transpose as)
    go us =  [ (/d) $ determinant [if i /= j then vs else us | (j,vs) <- ms] | (i,_) <- ms]

matI::(Num a) => Int -> [[a]]
matI n = [ [fromIntegral.fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

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

task::[[Rational]] -> [[Rational]] -> IO()
task a b = do
  let x         = solveCramer a b
  let u         = map (map fromRational) x
  let y         = mult a x
  let identity  = matI (length x)
  let a1        = solveCramer a identity
  let h         = mult a a1
  let z         = mult a1 b
  putStrLn "a ="
  mapM_ print a
  putStrLn "b ="
  mapM_ print b
  putStrLn "solve: a * x = b => x = solveCramer a b ="
  mapM_ print x
  putStrLn "u = fromRationaltoDouble x ="
  mapM_ print u
  putStrLn "verification: y = a * x = mult a x ="
  mapM_ print y
  putStrLn $ "test: y == b = "
  print $ y == b
  putStrLn "identity matrix: identity ="
  mapM_ print identity
  putStrLn "find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveCramer a identity ="
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
  let a  = [[2,-1, 5, 1]
           ,[3, 2, 2,-6]
           ,[1, 3, 3,-1]
           ,[5,-2,-3, 3]]
  let b  = [[-3], [-32], [-47], [49]]
  task a b

```

```txt

a =
[2 % 1,(-1) % 1,5 % 1,1 % 1]
[3 % 1,2 % 1,2 % 1,(-6) % 1]
[1 % 1,3 % 1,3 % 1,(-1) % 1]
[5 % 1,(-2) % 1,(-3) % 1,3 % 1]
b =
[(-3) % 1]
[(-32) % 1]
[(-47) % 1]
[49 % 1]
solve: a * x = b => x = solveCramer a b =
[2 % 1]
[(-12) % 1]
[(-4) % 1]
[1 % 1]
u = fromRationaltoDouble x =
[2.0]
[-12.0]
[-4.0]
[1.0]
verification: y = a * x = mult a x =
[(-3) % 1]
[(-32) % 1]
[(-47) % 1]
[49 % 1]
test: y == b =
True
identity matrix: identity =
[1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1]
find: a1 = inv(a) => solve: a * a1 = identity => a1 = solveCramer a identity =
[4 % 171,11 % 171,10 % 171,8 % 57]
[(-55) % 342,(-23) % 342,119 % 342,2 % 57]
[107 % 684,(-5) % 684,11 % 684,(-7) % 114]
[7 % 684,(-109) % 684,103 % 684,7 % 114]
verification: h = a * a1 = mult a a1 =
[1 % 1,0 % 1,0 % 1,0 % 1]
[0 % 1,1 % 1,0 % 1,0 % 1]
[0 % 1,0 % 1,1 % 1,0 % 1]
[0 % 1,0 % 1,0 % 1,1 % 1]
test: h == identity =
True
z = a1 * b = mult a1 b =
[2 % 1]
[(-12) % 1]
[(-4) % 1]
[1 % 1]
test: z == x =
True

```



## J

Implementation:


```J
cramer=:4 :0
  A=. x [ b=. y
  det=. -/ .*
  A %~&det (i.#A) b"_`[`]}&.|:"0 2 A
)
```


Task data:


```J
A=: _&".;._2]t=: 0 :0
  2 -1  5  1
  3  2  2 -6
  1  3  3 -1
  5 -2 -3  3
)

b=: _3 _32 _47 49
```


Task example:


```J
   A cramer b
2 _12 _4 1
```



## JavaScript



```Javascript

var matrix = [
	[2, -1,  5,  1],
	[3,  2,  2, -6],
	[1,  3,  3, -1],
	[5, -2, -3,  3]
];
var freeTerms = [-3, -32, -47, 49];

var result = cramersRule(matrix,freeTerms);
console.log(result);

/**
 * Compute Cramer's Rule
 * @param  {array} matrix    x,y,z, etc. terms
 * @param  {array} freeTerms
 * @return {array}           solution for x,y,z, etc.
 */
function cramersRule(matrix,freeTerms) {
	var det = detr(matrix),
		returnArray = [],
		i,
		tmpMatrix;

	for(i=0; i < matrix[0].length; i++) {
		var tmpMatrix = insertInTerms(matrix, freeTerms,i)
		returnArray.push(detr(tmpMatrix)/det)
	}
	return returnArray;
}

/**
 * Inserts single dimensional array into
 * @param  {array} matrix multidimensional array to have ins inserted into
 * @param  {array} ins single dimensional array to be inserted vertically into matrix
 * @param  {array} at  zero based offset for ins to be inserted into matrix
 * @return {array}     New multidimensional array with ins replacing the at column in matrix
 */
function insertInTerms(matrix, ins, at) {
	var tmpMatrix = clone(matrix),
		i;
	for(i=0; i < matrix.length; i++) {
		tmpMatrix[i][at] = ins[i];
	}
	return tmpMatrix;
}
/**
 * Compute the determinate of a matrix.  No protection, assumes square matrix
 * function borrowed, and adapted from MIT Licensed numericjs library (www.numericjs.com)
 * @param  {array} m Input Matrix (multidimensional array)
 * @return {number}   result rounded to 2 decimal
 */
function detr(m) {
	var ret = 1,
		k,
		A=clone(m),
		n=m[0].length,
		alpha;

	for(var j =0; j < n-1; j++) {
		k=j;
		for(i=j+1;i<n;i++) { if(Math.abs(A[i][j]) > Math.abs(A[k][j])) { k = i; } }
		if(k !== j) {
		    temp = A[k]; A[k] = A[j]; A[j] = temp;
		    ret *= -1;
		}
		Aj = A[j];
		for(i=j+1;i<n;i++) {
			Ai = A[i];
            alpha = Ai[j]/Aj[j];
            for(k=j+1;k<n-1;k+=2) {
                k1 = k+1;
                Ai[k] -= Aj[k]*alpha;
                Ai[k1] -= Aj[k1]*alpha;
            }
            if(k!==n) { Ai[k] -= Aj[k]*alpha; }
        }
        if(Aj[j] === 0) { return 0; }
        ret *= Aj[j];
	    }
    return Math.round(ret*A[j][j]*100)/100;
}

/**
 * Clone two dimensional Array using ECMAScript 5 map function and EcmaScript 3 slice
 * @param  {array} m Input matrix (multidimensional array) to clone
 * @return {array}   New matrix copy
 */
function clone(m) {
	return m.map(function(a){return a.slice();});
}


```

```txt
[ 2, -12, -4, 1 ]
```



## Julia

```julia
function cramersolve(A::Matrix, b::Vector)
    return collect(begin B = copy(A); B[:, i] = b; det(B) end for i in eachindex(b)) ./ det(A)
end

A = [2 -1  5  1
     3  2  2 -6
     1  3  3 -1
     5 -2 -3  3]

b = [-3, -32, -47, 49]

@show cramersolve(A, b)
```


```txt
cramersolve(A, b) = [2.0, -12.0, -4.0, 1.0]
```


Note that it is entirely impractical to use Cramer's rule in this situation. It would be much better to use the built-in operator for solving linear systems. Assuming that the coefficient matrix and constant vector are defined as before, the solution vector is given by:


```julia>@show A \ b</lang



## Kotlin

As in the case of the [[Matrix arithmetic]] task, I've used the Johnson-Trotter permutations generator to assist with the calculation of the determinants for the various matrices:

```scala
// version 1.1.3

typealias Vector = DoubleArray
typealias Matrix = Array<Vector>

fun johnsonTrotter(n: Int): Pair<List<IntArray>, List<Int>> {
    val p = IntArray(n) { it }  // permutation
    val q = IntArray(n) { it }  // inverse permutation
    val d = IntArray(n) { -1 }  // direction = 1 or -1
    var sign = 1
    val perms = mutableListOf<IntArray>()
    val signs = mutableListOf<Int>()

    fun permute(k: Int) {
        if (k >= n) {
            perms.add(p.copyOf())
            signs.add(sign)
            sign *= -1
            return
        }
        permute(k + 1)
        for (i in 0 until k) {
            val z = p[q[k] + d[k]]
            p[q[k]] = z
            p[q[k] + d[k]] = k
            q[z] = q[k]
            q[k] += d[k]
            permute(k + 1)
        }
        d[k] *= -1
    }

    permute(0)
    return perms to signs
}

fun determinant(m: Matrix): Double {
    val (sigmas, signs) = johnsonTrotter(m.size)
    var sum = 0.0
    for ((i, sigma) in sigmas.withIndex()) {
        var prod = 1.0
        for ((j, s) in sigma.withIndex()) prod *= m[j][s]
        sum += signs[i] * prod
    }
    return sum
}

fun cramer(m: Matrix, d: Vector): Vector {
    val divisor = determinant(m)
    val numerators = Array(m.size) { Matrix(m.size) { m[it].copyOf() } }
    val v = Vector(m.size)
    for (i in 0 until m.size) {
        for (j in 0 until m.size) numerators[i][j][i] = d[j]
    }
    for (i in 0 until m.size) v[i] = determinant(numerators[i]) / divisor
    return v
}

fun main(args: Array<String>) {
    val m = arrayOf(
        doubleArrayOf(2.0, -1.0,  5.0,  1.0),
        doubleArrayOf(3.0,  2.0,  2.0, -6.0),
        doubleArrayOf(1.0,  3.0,  3.0, -1.0),
        doubleArrayOf(5.0, -2.0, -3.0,  3.0)
    )
    val d = doubleArrayOf(-3.0, -32.0, -47.0, 49.0)
    val (w, x, y, z) = cramer(m, d)
    println("w = $w, x = $x, y = $y, z = $z")
}
```


```txt

w = 2.0, x = -12.0, y = -4.0, z = 1.0

```



## Lua


```Lua
local matrix = require "matrix" -- https://github.com/davidm/lua-matrix

local function cramer(mat, vec)
  -- Check if matrix is quadratic
  assert(#mat == #mat[1], "Matrix is not square!")
  -- Check if vector has the same size of the matrix
  assert(#mat == #vec, "Vector has not the same size of the matrix!")

  local size = #mat
  local main_det = matrix.det(mat)

  local aux_mats = {}
  local dets = {}
  local result = {}
  for i = 1, size do
    -- Construct the auxiliary matrixes
    aux_mats[i] = matrix.copy(mat)
    for j = 1, size do
      aux_mats[i][j][i] = vec[j]
    end

    -- Calculate the auxiliary determinants
    dets[i] = matrix.det(aux_mats[i])

    -- Calculate results
    result[i] = dets[i]/main_det
  end

  return result
end

-----------------------------------------------

local A = {{ 2, -1,  5,  1},
           { 3,  2,  2, -6},
           { 1,  3,  3, -1},
           { 5, -2, -3,  3}}
local b = {-3, -32, -47, 49}

local result = cramer(A, b)
print("Result: " .. table.concat(result, ", "))

```


```txt
Result: 2, -12, -4, 1
```



## Maple


```Maple
A := Matrix([[2,-1,5,1],[3,2,2,-6],[1,3,3,-1],[5,-2,-3,3]]):
w := LinearAlgebra:-Determinant(Matrix([[-3,-1,5,1],[-32,2,2,-6],[-47,3,3,-1],[49,-2,-3,3]]))/ LinearAlgebra:-Determinant(A);
x := LinearAlgebra:-Determinant(Matrix([[2,-3,5,1],[3,-32,2,-6],[1,-47,3,-1],[5,49,-3,3]]))/LinearAlgebra:-Determinant(A);
y := LinearAlgebra:-Determinant(Matrix([[2,-1,-3,1],[3,2,-32,-6],[1,3,-47,-1],[5,-2,49,3]]))/LinearAlgebra:-Determinant(A);
z := LinearAlgebra:-Determinant(Matrix([[2,-1,5,-3],[3,2,2,-32],[1,3,3,-47],[5,-2,-3,49]]))/LinearAlgebra:-Determinant(A);
```

```txt
w := 2
x := -12
y := -4
z := 1
```



## Mathematica


```Mathematica
crule[m_, b_] := Module[{d = Det[m], a},
  Table[a = m; a[[All, k]] = b; Det[a]/d, {k, Length[m]}]]

crule[{
  {2, -1, 5, 1},
  {3, 2, 2, -6},
  {1, 3, 3, -1},
  {5, -2, -3, 3}
 } , {-3, -32, -47, 49}]
```


```txt
{2,-12,-4,1}
```



## PARI/GP



```parigp

M = [2,-1,5,1;3,2,2,-6;1,3,3,-1;5,-2,-3,3];
V = Col([-3,-32,-47,49]);

matadjoint(M) * V / matdet(M)

```


Output:

```txt
[2, -12, -4, 1]~
```



## Perl


```perl
use Math::Matrix;

sub cramers_rule {
    my ($A, $terms) = @_;
    my @solutions;
    my $det = $A->determinant;
    foreach my $i (0 .. $#{$A}) {
        my $Ai = $A->clone;
        foreach my $j (0 .. $#{$terms}) {
            $Ai->[$j][$i] = $terms->[$j];
        }
        push @solutions, $Ai->determinant / $det;
    }
    @solutions;
}

my $matrix = Math::Matrix->new(
    [2, -1,  5,  1],
    [3,  2,  2, -6],
    [1,  3,  3, -1],
    [5, -2, -3,  3],
);

my $free_terms = [-3, -32, -47, 49];
my ($w, $x, $y, $z) = cramers_rule($matrix, $free_terms);

print "w = $w\n";
print "x = $x\n";
print "y = $y\n";
print "z = $z\n";
```

```txt

w = 2
x = -12
y = -4
z = 1

```



## Perl 6


```perl6
sub det(@matrix) {
    my @a = @matrix.map: { [|$_] };
    my $sign = +1;
    my $pivot = 1;
    for ^@a -> $k {
      my @r = ($k+1 .. @a.end);
      my $previous-pivot = $pivot;
      if 0 == ($pivot = @a[$k][$k]) {
        (my $s = @r.first: { @a[$_][$k] != 0 }) // return 0;
        (@a[$s],@a[$k]) = (@a[$k], @a[$s]);
        my $pivot = @a[$k][$k];
        $sign = -$sign;
      }
      for @r X @r -> ($i, $j) {
        ((@a[$i][$j] *= $pivot) -= @a[$i][$k]*@a[$k][$j]) /= $previous-pivot;
      }
    }
    $sign * $pivot
}

sub cramers_rule(@A, @terms) {
    gather for ^@A -> $i {
        my @Ai = @A.map: { [|$_] };
        for ^@terms -> $j {
            @Ai[$j][$i] = @terms[$j];
        }
        take det(@Ai);
    } »/» det(@A);
}

my @matrix = (
    [2, -1,  5,  1],
    [3,  2,  2, -6],
    [1,  3,  3, -1],
    [5, -2, -3,  3],
);

my @free_terms = (-3, -32, -47, 49);
my ($w, $x, $y, $z) = |cramers_rule(@matrix, @free_terms);

say "w = $w";
say "x = $x";
say "y = $y";
say "z = $z";
```

```txt

w = 2
x = -12
y = -4
z = 1

```



## Phix

The copy-on-write semantics of Phix really shine here; because there is no explicit return/re-assign, you can treat parameters as a private workspace, confident in the knowledge that the updated version will be quietly discarded; all the copying and freeing of the C version is automatic/unnecessary here.

```Phix
constant inf = 1e300*1e300,
         nan = -(inf/inf)

function det(sequence a)
atom res = 1

    for j=1 to length(a) do
        integer i_max = j
        for i=j+1 to length(a) do
            if a[i][j] > a[i_max][j] then
                i_max = i
            end if
        end for
        if i_max != j then
            {a[i_max],a[j]} = {a[j],a[i_max]}
            res *= -1
        end if

        if abs(a[j][j]) < 1e-12 then
            puts(1,"Singular matrix!")
            return nan
        end if

        for i=j+1 to length(a) do
            atom mult = -a[i][j] / a[j][j]
            for k=1 to length(a) do
                a[i][k] += mult * a[j][k]
            end for
        end for
    end for

    for i=1 to length(a) do
        res *= a[i][i]
    end for
    return res
end function

function cramer_solve(sequence a, atom det_a, sequence b, integer var)
    for i=1 to length(a) do
      a[i][var] = b[i]
    end for
    return det(a)/det_a
end function

sequence a = {{2,-1, 5, 1},
              {3, 2, 2,-6},
              {1, 3, 3,-1},
              {5,-2,-3, 3}},
         b = {-3,-32,-47,49}
integer det_a = det(a)
for i=1 to length(a) do
    printf(1, "%7.3f\n", cramer_solve(a, det_a, b, i))
end for
```

```txt

  2.000
-12.000
 -4.000
  1.000

```



## Prolog

```prolog
removeElement([_|Tail], 0, Tail).
removeElement([Head|Tail], J, [Head|X]) :-
    J_2 is J - 1,
    removeElement(Tail, J_2, X).

removeColumn([], _, []).
removeColumn([Matrix_head|Matrix_tail], J, [X|Y]) :-
    removeElement(Matrix_head, J, X),
    removeColumn(Matrix_tail, J, Y).

removeRow([_|Matrix_tail], 0, Matrix_tail).
removeRow([Matrix_head|Matrix_tail], I, [Matrix_head|X]) :-
    I_2 is I - 1,
    removeRow(Matrix_tail, I_2, X).

cofactor(Matrix, I, J, X) :-
    removeRow(Matrix, I, Matrix_2),
    removeColumn(Matrix_2, J, Matrix_3),
    det(Matrix_3, Y),
    X is (-1) ** (I + J) * Y.

det_summand(_, _, [], 0).
det_summand(Matrix, J, B, X) :-
    B = [B_head|B_tail],
    cofactor(Matrix, 0, J, Z),
    J_2 is J + 1,
    det_summand(Matrix, J_2, B_tail, Y),
    X is B_head * Z + Y.

det([[X]], X).
det(Matrix, X) :-
    Matrix = [Matrix_head|_],
    det_summand(Matrix, 0, Matrix_head, X).

replaceElement([_|Tail], 0, New, [New|Tail]).
replaceElement([Head|Tail], J, New, [Head|Y]) :-
    J_2 is J - 1,
    replaceElement(Tail, J_2, New, Y).

replaceColumn([], _, _, []).
replaceColumn([Matrix_head|Matrix_tail], J, [Column_head|Column_tail], [X|Y]) :-
    replaceElement(Matrix_head, J, Column_head, X),
    replaceColumn(Matrix_tail, J, Column_tail, Y).

cramerElements(_, B, L, []) :- length(B, L).
cramerElements(A, B, J, [X_J|Others]) :-
    replaceColumn(A, J, B, A_J),
    det(A_J, Det_A_J),
    det(A, Det_A),
    X_J is Det_A_J / Det_A,
    J_2 is J + 1,
    cramerElements(A, B, J_2, Others).

cramer(A, B, X) :- cramerElements(A, B, 0, X).

results(X) :-
    A = [
            [2, -1,  5,  1],
            [3,  2,  2, -6],
            [1,  3,  3, -1],
            [5, -2, -3,  3]
        ],
    B = [-3, -32, -47, 49],
    cramer(A, B, X).
```


```txt

| ?- results(X).
X = [2.0,-12.0,-4.0,1.0] ?
yes

```



## Python


```python

# a simple implementation using numpy
from numpy import linalg

A=[[2,-1,5,1],[3,2,2,-6],[1,3,3,-1],[5,-2,-3,3]]
B=[-3,-32,-47,49]
C=[[2,-1,5,1],[3,2,2,-6],[1,3,3,-1],[5,-2,-3,3]]
X=[]
for i in range(0,len(B)):
    for j in range(0,len(B)):
        C[j][i]=B[j]
        if i>0:
            C[j][i-1]=A[j][i-1]
    X.append(round(linalg.det(C)/linalg.det(A),1))

print('w=%s'%X[0],'x=%s'%X[1],'y=%s'%X[2],'z=%s'%X[3])

```

```txt

w=2.0 x=-12.0 y=-4.0 z=1.0

```



## Racket


```racket
#lang typed/racket
(require math/matrix)

(define A (matrix [[2 -1  5  1]
                   [3  2  2 -6]
                   [1  3  3 -1]
                   [5 -2 -3  3]]))

(define B (col-matrix [ -3
                       -32
                       -47
                        49]))

(matrix->vector (matrix-solve A B))
```

```txt
'#(2 -12 -4 1)
```



## REXX


```rexx
/* REXX Use Cramer's rule to compute solutions of given linear equations  */
Numeric Digits 20
names='w x y z'
M='  2  -1   5   1',
  '  3   2   2  -6',
  '  1   3   3  -1',
  '  5  -2  -3   3'
v=' -3',
  '-32',
  '-47',
  ' 49'
Call mk_mat(m)                      /* M -> a.i.j                    */
Do j=1 To dim                       /* Show the input                */
  ol=''
  Do i=1 To dim
    ol=ol format(a.i.j,6)
    End
  ol=ol format(word(v,j),6)
  Say ol
  End
Say copies('-',35)

d=det(m)                            /* denominator determinant       */

Do k=1 To dim                       /* construct nominator matrix    */
  Do j=1 To dim
    Do i=1 To dim
      If i=k Then
        b.i.j=word(v,j)
      Else
        b.i.j=a.i.j
      End
    End
  Call show_b
  d.k=det(mk_str())                 /* numerator determinant         */
  Say word(names,k) '=' d.k/d       /* compute value of variable k   */
  End
Exit

mk_mat: Procedure Expose a. dim     /* Turn list into matrix a.i.j */
  Parse Arg list
  dim=sqrt(words(list))
  k=0
  Do j=1 To dim
    Do i=1 To dim
      k=k+1
      a.i.j=word(list,k)
      End
    End
  Return

mk_str: Procedure Expose b. dim     /* Turn matrix b.i.j into list   */
  str=''
Do j=1 To dim
  Do i=1 To dim
    str=str b.i.j
    End
  End
Return str

show_b: Procedure Expose b. dim     /* show numerator matrix         */
  do j=1 To dim
    ol=''
    Do i=1 To dim
      ol=ol format(b.i.j,6)
      end
    Call dbg ol
    end
  Return

det: Procedure                      /* compute determinant           */
Parse Arg list
n=words(list)
call dbg 'det:' list
do dim=1 To 10
  If dim**2=n Then Leave
  End
call dbg 'dim='dim
If dim=2 Then Do
  det=word(list,1)*word(list,4)-word(list,2)*word(list,3)
  call dbg 'det=>'det
  Return det
  End
k=0
Do j=1 To dim
  Do i=1 To dim
    k=k+1
    a.i.j=word(list,k)
    End
  End
Do j=1 To dim
  ol=j
  Do i=1 To dim
    ol=ol format(a.i.j,6)
    End
  call dbg ol
  End
det=0
Do i=1 To dim
  ol=''
  Do j=2 To dim
    Do ii=1 To dim
      If ii<>i Then
        ol=ol a.ii.j
      End
    End
  call dbg 'i='i 'ol='ol
  If i//2 Then
    det=det+a.i.1*det(ol)
  Else
    det=det-a.i.1*det(ol)
  End
Call dbg 'det=>>>'det
Return det
sqrt: Procedure
/* REXX ***************************************************************
* EXEC to calculate the square root of a = 2 with high precision
**********************************************************************/
  Parse Arg x,prec
  If prec<9 Then prec=9
  prec1=2*prec
  eps=10**(-prec1)
  k = 1
  Numeric Digits 3
  r0= x
  r = 1
  Do i=1 By 1 Until r=r0 | (abs(r*r-x)<eps)
    r0 = r
    r  = (r + x/r) / 2
    k  = min(prec1,2*k)
    Numeric Digits (k + 5)
    End
  Numeric Digits prec
  r=r+0
  Return r


dbg: Return
```

```txt
      2     -1      5      1     -3
      3      2      2     -6    -32
      1      3      3     -1    -47
      5     -2     -3      3     49
-----------------------------------
w = 2
x = -12
y = -4
z = 1
```



## Ruby


```ruby
require 'matrix'

def cramers_rule(a, terms)
  raise ArgumentError, " Matrix not square"  unless a.square?
  cols = a.to_a.transpose
  cols.each_index.map do |i|
    c = cols.dup
    c[i] = terms
    Matrix.columns(c).det / a.det
  end
end

matrix = Matrix[
    [2, -1,  5,  1],
    [3,  2,  2, -6],
    [1,  3,  3, -1],
    [5, -2, -3,  3],
]

vector = [-3, -32, -47, 49]
puts cramers_rule(matrix, vector)
```

```txt

2
-12
-4
1

```



## Rust


```Rust
use std::ops::{Index, IndexMut};

fn main() {
    let m = matrix(
        vec![
            2., -1., 5., 1., 3., 2., 2., -6., 1., 3., 3., -1., 5., -2., -3., 3.,
        ],
        4,
    );
    let mm = m.solve(&vec![-3., -32., -47., 49.]);
    println!("{:?}", mm);
}

#[derive(Clone)]
struct Matrix {
    elts: Vec<f64>,
    dim: usize,
}

impl Matrix {
    // Compute determinant using cofactor method
    // Using Gaussian elimination would have been more efficient, but it also solves the linear
    // system, so…
    fn det(&self) -> f64 {
        match self.dim {
            0 => 0.,
            1 => self[0][0],
            2 => self[0][0] * self[1][1] - self[0][1] * self[1][0],
            d => {
                let mut acc = 0.;
                let mut signature = 1.;
                for k in 0..d {
                    acc += signature * self[0][k] * self.comatrix(0, k).det();
                    signature *= -1.
                }
                acc
            }
        }
    }

    // Solve linear systems using Cramer's method
    fn solve(&self, target: &Vec<f64>) -> Vec<f64> {
        let mut solution: Vec<f64> = vec![0.; self.dim];
        let denominator = self.det();
        for j in 0..self.dim {
            let mut mm = self.clone();
            for i in 0..self.dim {
                mm[i][j] = target[i]
            }
            solution[j] = mm.det() / denominator
        }
        solution
    }

    // Compute the cofactor matrix for determinant computations
    fn comatrix(&self, k: usize, l: usize) -> Matrix {
        let mut v: Vec<f64> = vec![];
        for i in 0..self.dim {
            for j in 0..self.dim {
                if i != k && j != l {
                    v.push(self[i][j])
                }
            }
        }
        matrix(v, self.dim - 1)
    }
}

fn matrix(elts: Vec<f64>, dim: usize) -> Matrix {
    assert_eq!(elts.len(), dim * dim);
    Matrix { elts, dim }
}

impl Index<usize> for Matrix {
    type Output = [f64];

    fn index(&self, i: usize) -> &Self::Output {
        let m = self.dim;
        &self.elts[m * i..m * (i + 1)]
    }
}

impl IndexMut<usize> for Matrix {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        let m = self.dim;
        &mut self.elts[m * i..m * (i + 1)]
    }
}
```


Which outputs:

```txt
[2.0, -12.0, -4.0, 1.0]
```



## Sidef


```ruby
func cramers_rule(A, terms) {
    gather {
        for i in ^A {
            var Ai = A.map{.map{_}}
            for j in ^terms {
                Ai[j][i] = terms[j]
            }
            take(Ai.det)
        }
    } »/» A.det
}

var matrix = [
    [2, -1,  5,  1],
    [3,  2,  2, -6],
    [1,  3,  3, -1],
    [5, -2, -3,  3],
]

var free_terms = [-3, -32, -47, 49]
var (w, x, y, z) = cramers_rule(matrix, free_terms)...

say "w = #{w}"
say "x = #{x}"
say "y = #{y}"
say "z = #{z}"
```

```txt

w = 2
x = -12
y = -4
z = 1

```



## Tcl


```tcl

package require math::linearalgebra
namespace path ::math::linearalgebra

# Setting matrix to variable A and size to n
set A [list { 2 -1  5  1} { 3  2  2 -6} { 1  3  3 -1} { 5 -2 -3  3}]
set n [llength $A]
# Setting right side of equation
set right {-3 -32 -47 49}

# Calculating determinant of A
set detA [det $A]

# Apply Cramer's rule
for {set i 0} {$i < $n} {incr i} {
  set tmp $A                    ;# copy A to tmp
  setcol tmp $i $right          ;# replace column i with right side vector
  set detTmp [det $tmp]         ;# calculate determinant of tmp
  set v [expr $detTmp / $detA]  ;# divide two determinants
  puts [format "%0.4f" $v]      ;# format and display result
}

```

```txt

2.0000
-12.0000
-4.0000
1.0000

```



## Visual Basic .NET

```vbnet
Imports System.Runtime.CompilerServices
Imports System.Linq.Enumerable

Module Module1
    <Extension()>
    Function DelimitWith(Of T)(source As IEnumerable(Of T), Optional seperator As String = " ") As String
        Return String.Join(seperator, source)
    End Function

    Private Class SubMatrix
        Private ReadOnly source As Integer(,)
        Private ReadOnly prev As SubMatrix
        Private ReadOnly replaceColumn As Integer()

        Public Sub New(source As Integer(,), replaceColumn As Integer())
            Me.source = source
            Me.replaceColumn = replaceColumn
            prev = Nothing
            ColumnIndex = -1
            Size = replaceColumn.Length
        End Sub

        Public Sub New(prev As SubMatrix, Optional deletedColumnIndex As Integer = -1)
            source = Nothing
            replaceColumn = Nothing
            Me.prev = prev
            ColumnIndex = deletedColumnIndex
            Size = prev.Size - 1
        End Sub

        Public Property ColumnIndex As Integer
        Public ReadOnly Property Size As Integer

        Default Public ReadOnly Property Index(row As Integer, column As Integer) As Integer
            Get
                If Not IsNothing(source) Then
                    Return If(column = ColumnIndex, replaceColumn(row), source(row, column))
                Else
                    Return prev(row + 1, If(column < ColumnIndex, column, column + 1))
                End If
            End Get
        End Property

        Public Function Det() As Integer
            If Size = 1 Then Return Me(0, 0)
            If Size = 2 Then Return Me(0, 0) * Me(1, 1) - Me(0, 1) * Me(1, 0)
            Dim m As New SubMatrix(Me)
            Dim detVal = 0
            Dim sign = 1
            For c = 0 To Size - 1
                m.ColumnIndex = c
                Dim d = m.Det()
                detVal += Me(0, c) * d * sign
                sign = -sign
            Next
            Return detVal
        End Function

        Public Sub Print()
            For r = 0 To Size - 1
                Dim rl = r
                Console.WriteLine(Range(0, Size).Select(Function(c) Me(rl, c)).DelimitWith(", "))
            Next
            Console.WriteLine()
        End Sub
    End Class

    Private Function Solve(matrix As SubMatrix) As Integer()
        Dim det = matrix.Det()
        If det = 0 Then Throw New ArgumentException("The determinant is zero.")

        Dim answer(matrix.Size - 1) As Integer
        For i = 0 To matrix.Size - 1
            matrix.ColumnIndex = i
            answer(i) = matrix.Det() / det
        Next
        Return answer
    End Function

    Public Function SolveCramer(equations As Integer()()) As Integer()
        Dim size = equations.Length
        If equations.Any(Function(eq) eq.Length <> size + 1) Then Throw New ArgumentException($"Each equation must have {size + 1} terms.")
        Dim matrix(size - 1, size - 1) As Integer
        Dim column(size - 1) As Integer
        For r = 0 To size - 1
            column(r) = equations(r)(size)
            For c = 0 To size - 1
                matrix(r, c) = equations(r)(c)
            Next
        Next
        Return Solve(New SubMatrix(matrix, column))
    End Function

    Sub Main()
        Dim equations = {
            ({2, -1, 5, 1, -3}),
            ({3, 2, 2, -6, -32}),
            ({1, 3, 3, -1, -47}),
            ({5, -2, -3, 3, 49})
        }
        Dim solution = SolveCramer(equations)
        Console.WriteLine(solution.DelimitWith(", "))
    End Sub

End Module
```

```txt
2, -12, -4, 1
```



## zkl

Using the GNU Scientific Library, we define the values:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
A:=GSL.Matrix(4,4).set(2,-1, 5, 1,
		       3, 2, 2,-6,
		       1, 3, 3,-1,
		       5,-2,-3, 3);
b:=GSL.Vector(4).set(-3,-32,-47,49);
```

First, just let GSL solve:

```zkl
A.AxEQb(b).format().println();
```

Actually implement Cramer's rule:
```zkl
fcn cramersRule(A,b){
   b.len().pump(GSL.Vector(b.len()),'wrap(i){ // put calculations into new Vector
      A.copy().setColumn(i,b).det();
   }).close()/A.det();
}
cramersRule(A,b).format().println();
```

```txt

2.00,-12.00,-4.00,1.00

```

