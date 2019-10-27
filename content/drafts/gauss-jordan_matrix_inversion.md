+++
title = "Gauss-Jordan matrix inversion"
description = ""
date = 2019-09-02T18:07:52Z
aliases = []
[extra]
id = 21753
[taxonomies]
categories = []
tags = []
+++

Gauss-Jordan matrix inversion

{{Draft task}}[[Category:Matrices]]

;Task:
Invert matrix   '''A'''   using Gauss-Jordan method.

'''A'''   being an   '''n''' by '''n'''   matrix.

## C#


```csharp

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
        internal bool InvPartial()
        {
            const double Eps = 1e-12;
            if (rows != cols) throw new Exception("rows != cols for Inv");
            Matrix M = new Matrix(rows); //unitary
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
                if (max_val <= Eps) return false;
                SwapRows(diag, max_row);
                M.SwapRows(diag, max_row);
                double invd = 1 / this[diag, diag];
                for (int col = diag; col < cols; col++)
                {
                    this[diag, col] *= invd;
                }
                for (int col = 0; col < cols; col++)
                {
                    M[diag, col] *= invd;
                }
                for (int row = 0; row < rows; row++)
                {
                    d = this[row, diag];
                    if (row != diag)
                    {
                        for (int col = diag; col < this.cols; col++)
                        {
                            this[row, col] -= d * this[diag, col];
                        }
                        for (int col = 0; col < this.cols; col++)
                        {
                            M[row, col] -= d * M[diag, col];
                        }
                    }
                }
            }
            b = M.b;
            return true;
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


```csharp

using System;

namespace Rosetta
{
    class Program
    {
        static void Main(string[] args)
        {
            Matrix M = new Matrix(4, 4, new double[] { -1, -2, 3, 2, -4, -1, 6, 2, 7, -8, 9, 1, 1, -2, 1, 3 });            
            M.InvPartial();
            M.print();
        }
    }
}

```

{{out}}
```txt

-0.913043478260869  0.246376811594203  0.0942028985507246  0.413043478260869
-1.65217391304348  0.652173913043478  0.0434782608695652  0.652173913043478
-0.695652173913043  0.36231884057971  0.0797101449275362  0.195652173913043
-0.565217391304348  0.231884057971014  -0.0289855072463768  0.565217391304348

```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

type vector = []float64
type matrix []vector

func (m matrix) inverse() matrix {
    le := len(m)
    for _, v := range m {
        if len(v) != le {
            panic("Not a square matrix")
        }
    }
    aug := make(matrix, le)
    for i := 0; i < le; i++ {
        aug[i] = make(vector, 2*le)
        copy(aug[i], m[i])
        // augment by identity matrix to right
        aug[i][i+le] = 1
    }
    aug.toReducedRowEchelonForm()
    inv := make(matrix, le)
    // remove identity matrix to left
    for i := 0; i < le; i++ {
        inv[i] = make(vector, le)
        copy(inv[i], aug[i][le:])
    }
    return inv
}

// note: this mutates the matrix in place
func (m matrix) toReducedRowEchelonForm() {
    lead := 0
    rowCount, colCount := len(m), len(m[0])
    for r := 0; r < rowCount; r++ {
        if colCount <= lead {
            return
        }
        i := r

        for m[i][lead] == 0 {
            i++
            if rowCount == i {
                i = r
                lead++
                if colCount == lead {
                    return
                }
            }
        }

        m[i], m[r] = m[r], m[i]
        if div := m[r][lead]; div != 0 {
            for j := 0; j < colCount; j++ {
                m[r][j] /= div
            }
        }

        for k := 0; k < rowCount; k++ {
            if k != r {
                mult := m[k][lead]
                for j := 0; j < colCount; j++ {
                    m[k][j] -= m[r][j] * mult
                }
            }
        }
        lead++
    }
}

func (m matrix) print(title string) {
    fmt.Println(title)
    for _, v := range m {
        fmt.Printf("% f\n", v)
    }
    fmt.Println()
}

func main() {
    a := matrix{{1, 2, 3}, {4, 1, 6}, {7, 8, 9}}
    a.inverse().print("Inverse of A is:\n")

    b := matrix{{2, -1, 0}, {-1, 2, -1}, {0, -1, 2}}
    b.inverse().print("Inverse of B is:\n")
}
```


{{out}}

```txt

Inverse of A is:

[-0.812500  0.125000  0.187500]
[ 0.125000 -0.250000  0.125000]
[ 0.520833  0.125000 -0.145833]

Inverse of B is:

[ 0.750000  0.500000  0.250000]
[ 0.500000  1.000000  0.500000]
[ 0.250000  0.500000  0.750000]

```



## J


'''Solution:'''

Uses Gauss-Jordan implementation (as described in [[Reduced_row_echelon_form#J]]) to find reduced row echelon form of the matrix after augmenting with an identity matrix.

```j
require 'math/misc/linear'
augmentR_I1=: ,. e.@i.@#       NB. augment matrix on the right with its Identity matrix
matrix_invGJ=: # }."1 [: gauss_jordan@augmentR_I1
```


'''Usage:'''

```j
   ]A =: 1 2 3, 4 1 6,: 7 8 9
1 2 3
4 1 6
7 8 9
   matrix_invGJ A
 _0.8125 0.125    0.1875
   0.125 _0.25     0.125
0.520833 0.125 _0.145833
```



## Julia

{{works with|Julia|0.6}}

'''Built-in''' LAPACK-based linear solver uses partial-pivoted Gauss elimination):

```julia
A = [1 2 3; 4 1 6; 7 8 9]
@show I / A
@show inv(A)
```


'''Native implementation''':

```julia
function gaussjordan(A::Matrix)
    size(A, 1) == size(A, 2) || throw(ArgumentError("A must be squared"))
    n = size(A, 1)
    M = [convert(Matrix{float(eltype(A))}, A) I]
    i = 1
    local tmp = Vector{eltype(M)}(2n)
    # forward
    while i ≤ n
        if M[i, i] ≈ 0.0
            local j = i + 1
            while j ≤ n && M[j, i] ≈ 0.0
                j += 1
            end
            if j ≤ n
                tmp     .= M[i, :]
                M[i, :] .= M[j, :]
                M[j, :] .= tmp
            else
                throw(ArgumentError("matrix is singular, cannot compute the inverse"))
            end
        end
        for j in (i + 1):n
            M[j, :] .-= M[j, i] / M[i, i] .* M[i, :]
        end
        i += 1
    end
    i = n
    # backward
    while i ≥ 1
        if M[i, i] ≈ 0.0
            local j = i - 1
            while j ≥ 1 && M[j, i] ≈ 0.0
                j -= 1
            end
            if j ≥ 1
                tmp     .= M[i, :]
                M[i, :] .= M[j, :]
                M[j, :] .= tmp
            else
                throw(ArgumentError("matrix is singular, cannot compute the inverse"))
            end
        end
        for j in (i - 1):-1:1
            M[j, :] .-= M[j, i] / M[i, i] .* M[i, :]
        end
        i -= 1
    end
    M ./= diag(M) # normalize
    return M[:, n+1:2n]
end

@show gaussjordan(A)
@assert gaussjordan(A) ≈ inv(A)

A = rand(10, 10)
@assert gaussjordan(A) ≈ inv(A)
```


{{out}}

```txt
I / A = [-0.8125 0.125 0.1875; 0.125 -0.25 0.125; 0.520833 0.125 -0.145833]
inv(A) = [-0.8125 0.125 0.1875; 0.125 -0.25 0.125; 0.520833 0.125 -0.145833]
gaussjordan(A) = [-0.8125 0.125 0.1875; 0.125 -0.25 0.125; 0.520833 0.125 -0.145833]
```



## Kotlin

This follows the description of Gauss-Jordan elimination in Wikipedia whereby the original square matrix is first augmented to the right by its identity matrix, its reduced row echelon form is then found and finally the identity matrix to the left is removed to leave the inverse of the original square matrix.

```scala
// version 1.2.21

typealias Matrix = Array<DoubleArray>

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

fun Matrix.printf(title: String) {
    println(title)
    val rowCount = this.size
    val colCount = this[0].size

    for (r in 0 until rowCount) {
        for (c in 0 until colCount) {
            if (this[r][c] == -0.0) this[r][c] = 0.0  // get rid of negative zeros
            print("${"% 10.6f".format(this[r][c])}  ")
        }
        println()
    }

    println()
}

fun main(args: Array<String>) {
    val a = arrayOf(
        doubleArrayOf(1.0, 2.0, 3.0),
        doubleArrayOf(4.0, 1.0, 6.0),
        doubleArrayOf(7.0, 8.0, 9.0)
    )
    a.inverse().printf("Inverse of A is :\n")

    val b = arrayOf(
        doubleArrayOf( 2.0, -1.0,  0.0),
        doubleArrayOf(-1.0,  2.0, -1.0),
        doubleArrayOf( 0.0, -1.0,  2.0)
    )
    b.inverse().printf("Inverse of B is :\n")    
}
```


{{out}}

```txt

Inverse of A is :

 -0.812500    0.125000    0.187500  
  0.125000   -0.250000    0.125000  
  0.520833    0.125000   -0.145833  

Inverse of B is :

  0.750000    0.500000    0.250000  
  0.500000    1.000000    0.500000  
  0.250000    0.500000    0.750000  

```



## Perl

Included code from [[Reduced_row_echelon_form#Perl|Reduced row echelon form]] task.

```perl
sub rref {
  our @m; local *m = shift;
  @m or return;
  my ($lead, $rows, $cols) = (0, scalar(@m), scalar(@{$m[0]}));

  foreach my $r (0 .. $rows - 1) {
     $lead < $cols or return;
      my $i = $r;

      until ($m[$i][$lead])
         {++$i == $rows or next;
          $i = $r;
          ++$lead == $cols and return;}

      @m[$i, $r] = @m[$r, $i];
      my $lv = $m[$r][$lead];
      $_ /= $lv foreach @{ $m[$r] };

      my @mr = @{ $m[$r] };
      foreach my $i (0 .. $rows - 1)
         {$i == $r and next;
          ($lv, my $n) = ($m[$i][$lead], -1);
          $_ -= $lv * $mr[++$n] foreach @{ $m[$i] };}

      ++$lead;}
}

sub display { join("\n" => map join(" " => map(sprintf("%6.2f", $_), @$_)), @{+shift})."\n" }

sub gauss_jordan_invert {
    my(@m) = @_;
    my $rows = @m;
    my @i = identity(scalar @m);
    push @{$m[$_]}, @{$i[$_]} for 0..$rows-1;
    rref(\@m);
    map { splice @$_, 0, $rows } @m;
    @m;
}

sub identity {
    my($n) = @_;
    map { [ (0) x $_, 1, (0) x ($n-1 - $_) ] } 0..$n-1
}

my @tests = (
    [
      [ 2, -1,  0 ],
      [-1,  2, -1 ],
      [ 0, -1,  2 ]
    ],
    [
      [ -1, -2, 3, 2 ],
      [ -4, -1, 6, 2 ],
      [  7, -8, 9, 1 ],
      [  1, -2, 1, 3 ]
    ],
);

for my $matrix (@tests) {
    print "Original Matrix:\n" . display(\@$matrix) . "\n";
    my @gj = gauss_jordan_invert( @$matrix );
    print "Gauss-Jordan Inverted Matrix:\n" . display(\@gj) . "\n";
    my @rt = gauss_jordan_invert( @gj );
    print "After round-trip:\n" . display(\@rt) . "\n";} . "\n"
}
```

{{out}}

```txt
Original Matrix:
  2.00  -1.00   0.00
 -1.00   2.00  -1.00
  0.00  -1.00   2.00

Gauss-Jordan Inverted Matrix:
  0.75   0.50   0.25
  0.50   1.00   0.50
  0.25   0.50   0.75

After round-trip:
  2.00  -1.00   0.00
 -1.00   2.00  -1.00
  0.00  -1.00   2.00

Original Matrix:
 -1.00  -2.00   3.00   2.00
 -4.00  -1.00   6.00   2.00
  7.00  -8.00   9.00   1.00
  1.00  -2.00   1.00   3.00

Gauss-Jordan Inverted Matrix:
 -0.91   0.25   0.09   0.41
 -1.65   0.65   0.04   0.65
 -0.70   0.36   0.08   0.20
 -0.57   0.23  -0.03   0.57

After round-trip:
 -1.00  -2.00   3.00   2.00
 -4.00  -1.00   6.00   2.00
  7.00  -8.00   9.00   1.00
  1.00  -2.00   1.00   3.00
```



## Perl 6

{{works with|Rakudo|2019.03.1}}
Uses bits and pieces from other tasks, [[Reduced_row_echelon_form#Perl_6|Reduced row echelon form]] primarily.


```perl6
sub gauss-jordan-invert (@m where *.&is-square) {
    ^@m .map: { @m[$_].append: identity(+@m)[$_] };
    @m.&rref[*]»[+@m .. *];
}

sub is-square (@m) { so @m == all @m[*] }

sub identity ($n) { [ 1, |(0 xx $n-1) ], *.rotate(-1) ... *.tail }

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

sub say_it ($message, @array) {
    my $max;
    @array.map: {$max max= max $_».&rat-or-int.comb(/\S+/)».chars};
    say "\n$message";
    $_».&rat-or-int.fmt(" %{$max}s").put for @array;
}

sub to-matrix ($str) { [$str.split(';').map(*.words.Array)] }

my @tests =
  '1 2 3; 4 1 6; 7 8 9',
  '2 -1 0; -1 2 -1; 0 -1 2',
  '-1 -2 3 2; -4 -1 6 2; 7 -8 9 1; 1 -2 1 3',
  '1 2 3 4; 5 6 7 8; 9 33 11 12; 13 14 15 17',
  '3 1 8 9 6; 6 2 8 10 1; 5 7 2 10 3; 3 2 7 7 9; 3 5 6 1 1',
  '-4525/6238  2529/6238 -233/3119 1481/3119 -639/6238;
    1033/6238 -1075/6238  342/3119 -447/3119  871/6238;
    1299/6238  -289/6238 -204/3119 -390/3119  739/6238;
     782/3119  -222/3119  237/3119 -556/3119 -177/3119;
    -474/3119   -17/3119  -24/3119  688/3119 -140/3119';

@tests.map: {
    my @matrix = .&to-matrix;
    say_it( 'Original Matrix:', @matrix );
    say_it( 'Gauss-Jordan Inverted Matrix:', gauss-jordan-invert @matrix );
}
```


{{out}}

```txt
Original Matrix:
 1  2  3
 4  1  6
 7  8  9

Gauss-Jordan Inverted Matrix:
 -13/16     1/8    3/16
    1/8    -1/4     1/8
  25/48     1/8   -7/48

Original Matrix:
  2  -1   0
 -1   2  -1
  0  -1   2

Gauss-Jordan Inverted Matrix:
 3/4  1/2  1/4
 1/2    1  1/2
 1/4  1/2  3/4

Original Matrix:
 -1  -2   3   2
 -4  -1   6   2
  7  -8   9   1
  1  -2   1   3

Gauss-Jordan Inverted Matrix:
 -21/23   17/69  13/138   19/46
 -38/23   15/23    1/23   15/23
 -16/23   25/69  11/138    9/46
 -13/23   16/69   -2/69   13/23

Original Matrix:
  1   2   3   4
  5   6   7   8
  9  33  11  12
 13  14  15  17

Gauss-Jordan Inverted Matrix:
   19/184  -199/184     -1/46       1/2
     1/23     -2/23      1/23         0
 -441/184   813/184     -1/46      -3/2
        2        -3         0         1

Original Matrix:
  3   1   8   9   6
  6   2   8  10   1
  5   7   2  10   3
  3   2   7   7   9
  3   5   6   1   1

Gauss-Jordan Inverted Matrix:
 -4525/6238   2529/6238   -233/3119   1481/3119   -639/6238
  1033/6238  -1075/6238    342/3119   -447/3119    871/6238
  1299/6238   -289/6238   -204/3119   -390/3119    739/6238
   782/3119   -222/3119    237/3119   -556/3119   -177/3119
  -474/3119    -17/3119    -24/3119    688/3119   -140/3119

Original Matrix:
 -4525/6238   2529/6238   -233/3119   1481/3119   -639/6238
  1033/6238  -1075/6238    342/3119   -447/3119    871/6238
  1299/6238   -289/6238   -204/3119   -390/3119    739/6238
   782/3119   -222/3119    237/3119   -556/3119   -177/3119
  -474/3119    -17/3119    -24/3119    688/3119   -140/3119

Gauss-Jordan Inverted Matrix:
  3   1   8   9   6
  6   2   8  10   1
  5   7   2  10   3
  3   2   7   7   9
  3   5   6   1   1
```



## Phix

{{trans|Kotlin}}
uses ToReducedRowEchelonForm() from [[Reduced_row_echelon_form#Phix]]

```Phix
function inverse(sequence mat)
    integer len = length(mat)
    sequence aug = repeat(repeat(0,2*len),len)
    for i=1 to len do
        if length(mat[i])!=len then ?9/0 end if -- "Not a square matrix"
        for j=1 to len do
            aug[i][j] = mat[i][j]
        end for
        -- augment by identity matrix to right
        aug[i][i + len] = 1
    end for
    aug = ToReducedRowEchelonForm(aug)
    sequence inv = repeat(repeat(0,len),len)
    -- remove identity matrix to left
    for i=1 to len do
        for j=len+1 to 2*len do
            inv[i][j-len] = aug[i][j]
        end for
    end for
    return inv
end function

constant test = {{ 2, -1,  0},
                 {-1,  2, -1},
                 { 0, -1,  2}}
pp(inverse(test),{pp_Nest,1})
```

{{out}}

```txt

{{0.75,0.5,0.25},
 {0.5,1,0.5},
 {0.25,0.5,0.75}}

```



## PowerShell


```PowerShell

function gauss-jordan-inv([double[][]]$a) {
    $n = $a.count
    [double[][]]$b = 0..($n-1) | foreach{[double[]]$row = @(0) * $n; $row[$_] = 1; ,$row} 
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
        if (0 -eq $akk) {throw "Irregular matrix"}
        for ($j = 0; $j -lt $n; $j++) {
            $a[$k][$j] /= $akk
            $b[$k][$j] /= $akk
        }
        for ($i = 0; $i -lt $n; $i++){
            if ($i -ne $k) {
                $aik  = $a[$i][$k]
                for ($j = 0; $j -lt $n; $j++) {
                    $a[$i][$j] -= $a[$k][$j]*$aik
                    $b[$i][$j] -= $b[$k][$j]*$aik
                }
            }
        }    
    }
    $b
}
function show($a) { $a | foreach{ "$_"} }

$a = @(@(@(1, 2, 3), @(4, 1, 6), @(7, 8, 9)))
$inva = gauss-jordan-inv $a
"a ="
show $a
""
"inv(a) ="
show $inva
""

$b = @(@(2, -1, 0), @(-1, 2, -1), @(0, -1, 2))
"b ="
show $b
""
$invb = gauss-jordan-inv $b
"inv(b) ="
show $invb


```

<b>Output:</b>

```txt

a =
1 2 3
4 1 6
7 8 9

inv(a) =
-0.8125 0.125 0.1875
0.125 -0.25 0.125
0.520833333333333 0.125 -0.145833333333333

b =
2 -1 0
-1 2 -1
0 -1 2

inv(b) =
0.75 0.5 0.25
0.5 1 0.5
0.25 0.5 0.75

```




## Racket


Using the matrix library that comes with default Racket installation


```racket
#lang racket

(require math/matrix
         math/array)

(define (inverse M)
  (define dim (square-matrix-size M))
  (define MI (matrix-augment (list M (identity-matrix dim))))
  (submatrix (matrix-row-echelon MI #t #t) (::) (:: dim  #f)))

(define A (matrix [[1 2 3] [4 1 6] [7 8 9]]))

(inverse A)
(matrix-inverse A)
```


{{out}}

```txt

(array #[#[-13/16 1/8 3/16] #[1/8 -1/4 1/8] #[25/48 1/8 -7/48]])
(array #[#[-13/16 1/8 3/16] #[1/8 -1/4 1/8] #[25/48 1/8 -7/48]])

```



## REXX


```rexx
/* REXX */
Parse Arg seed nn
If seed='' Then
  seed=23345
If nn='' Then nn=5
If seed='?' Then Do
  Say 'rexx gjmi seed n computes a random matrix with n rows and columns'
  Say 'Default is 23345 5'
  Exit
  End
Numeric Digits 50
Call random 1,2,seed
a=''
Do i=1 To nn**2
  a=a random(9)+1
  End
n2=words(a)
Do n=2 To n2/2
  If n**2=n2 Then
    Leave
  End
If n>n2/2 Then
  Call exit 'Not a square matrix:' a '('n2 'elements).'
det=determinante(a,n)
If det=0 Then
  Call exit 'Determinant is 0'
Do j=1 To n
  Do i=1 To n
    Parse Var A a.i.j a
    aa.i.j=a.i.j
    End
  Do ii=1 To n
    z=(ii=j)
    iii=ii+n
    a.iii.j=z
    End
  End
Call show 1,'The given matrix'
Do m=1 To n-1
  If a.m.m=0 Then Do
    Do j=m+1 To n
      If a.m.j<>0 Then Leave
      End
    If j>n Then Do
      Say 'No pivot>0 found in column' m
      Exit
      End
    Do i=1 To n*2
      temp=a.i.m
      a.i.m=a.i.j
      a.i.j=temp
      End
    End
  Do j=m+1 To n
    If a.m.j<>0 Then Do
      jj=m
      fact=divide(a.m.m,a.m.j)
      Do i=1 To n*2
        a.i.j=subtract(multiply(a.i.j,fact),a.i.jj)
        End
      End
    End
  Call show 2 m
  End
Say 'Lower part has all zeros'
Say ''

Do j=1 To n
  If denom(a.j.j)<0 Then Do
    Do i=1 To 2*n
      a.i.j=subtract(0,a.i.j)
      End
    End
  End
Call show 3

Do m=n To 2 By -1
  Do j=1 To m-1
    jj=m
    fact=divide(a.m.j,a.m.jj)
    Do i=1 To n*2
      a.i.j=subtract(a.i.j,multiply(a.i.jj,fact))
      End
    End
  Call show 4 m
  End
Say 'Upper half has all zeros'
Say ''
Do j=1 To n
  If decimal(a.j.j)<>1 Then Do
    z=a.j.j
    Do i=1 To 2*n
      a.i.j=divide(a.i.j,z)
      End
    End
  End
Call show 5
Say 'Main diagonal has all ones'
Say ''

Do j=1 To n
  Do i=1 To n
    z=i+n
    a.i.j=a.z.j
    End
  End
Call show 6,'The inverse matrix'

do i = 1 to n
  do j = 1 to n
    sum=0
    Do k=1 To n
      sum=add(sum,multiply(aa.i.k,a.k.j))
      End
    c.i.j = sum
    end
  End
Call showc 7,'The product of input and inverse matrix'
Exit

show:
  Parse Arg num,text
  Say 'show' arg(1) text
  If arg(1)<>6 Then rows=n*2
               Else rows=n
  len=0
  Do j=1 To n
    Do i=1 To rows
      len=max(len,length(a.i.j))
      End
    End
  Do j=1 To n
    ol=''
    Do i=1 To rows
      ol=ol||right(a.i.j,len+1)
      End
    Say ol
    End
  Say ''
  Return

showc:
  Parse Arg num,text
  Say text
  clen=0
  Do j=1 To n
    Do i=1 To n
      clen=max(clen,length(c.i.j))
      End
    End
  Do j=1 To n
    ol=''
    Do i=1 To n
      ol=ol||right(c.i.j,clen+1)
      End
    Say ol
    End
  Say ''
  Return

denom: Procedure
  /* Return the denominator */
  Parse Arg d '/' n
  Return d

decimal: Procedure
  /* compute the fraction's value */
  Parse Arg a
  If pos('/',a)=0 Then a=a'/1'; Parse Var a ad '/' an
  Return ad/an

gcd: procedure
/**********************************************************************
* Greatest commn divisor
**********************************************************************/
  Parse Arg a,b
  If b = 0 Then Return abs(a)
  Return gcd(b,a//b)

add: Procedure
  Parse Arg a,b
  If pos('/',a)=0 Then a=a'/1'; Parse Var a ad '/' an
  If pos('/',b)=0 Then b=b'/1'; Parse Var b bd '/' bn
  sum=divide(ad*bn+bd*an,an*bn)
  Return sum

multiply: Procedure
  Parse Arg a,b
  If pos('/',a)=0 Then a=a'/1'; Parse Var a ad '/' an
  If pos('/',b)=0 Then b=b'/1'; Parse Var b bd '/' bn
  prd=divide(ad*bd,an*bn)
  Return prd

subtract: Procedure
  Parse Arg a,b
  If pos('/',a)=0 Then a=a'/1'; Parse Var a ad '/' an
  If pos('/',b)=0 Then b=b'/1'; Parse Var b bd '/' bn
  div=divide(ad*bn-bd*an,an*bn)
  Return div

divide: Procedure
  Parse Arg a,b
  If pos('/',a)=0 Then a=a'/1'; Parse Var a ad '/' an
  If pos('/',b)=0 Then b=b'/1'; Parse Var b bd '/' bn
  sd=ad*bn
  sn=an*bd
  g=gcd(sd,sn)
  Select
    When sd=0 Then res='0'
    When abs(sn/g)=1 Then res=(sd/g)*sign(sn/g)
    Otherwise Do
      den=sd/g
      nom=sn/g
      If nom<0 Then Do
        If den<0 Then den=abs(den)
        Else den=-den
        nom=abs(nom)
        End
      res=den'/'nom
      End
    End
  Return res

determinante: Procedure
/* REXX ***************************************************************
* determinant.rex
* compute the determinant of the given square matrix
* Input: as: the representation of the matrix as vector (n**2 elements)
* 21.05.2013 Walter Pachl
**********************************************************************/
  Parse Arg as,n
  Do i=1 To n
    Do j=1 To n
      Parse Var as a.i.j as
      End
    End
  Select
    When n=2 Then det=subtract(multiply(a.1.1,a.2.2),multiply(a.1.2,a.2.1))
    When n=3 Then Do
      det=multiply(multiply(a.1.1,a.2.2),a.3.3)
      det=add(det,multiply(multiply(a.1.2,a.2.3),a.3.1))
      det=add(det,multiply(multiply(a.1.3,a.2.1),a.3.2))
      det=subtract(det,multiply(multiply(a.1.3,a.2.2),a.3.1))
      det=subtract(det,multiply(multiply(a.1.2,a.2.1),a.3.3))
      det=subtract(det,multiply(multiply(a.1.1,a.2.3),a.3.2))
      End
    Otherwise Do
      det=0
      Do k=1 To n
        sign=((-1)**(k+1))
        If sign=1 Then
          det=add(det,multiply(a.1.k,determinante(subm(k),n-1)))
        Else
          det=subtract(det,multiply(a.1.k,determinante(subm(k),n-1)))
        End
      End
    End
  Return det

subm: Procedure Expose a. n
/**********************************************************************
* compute the submatrix resulting when row 1 and column k are removed
* Input: a.*.*, k
* Output: bs the representation of the submatrix as vector
**********************************************************************/
  Parse Arg k
  bs=''
  do i=2 To n
    Do j=1 To n
      If j=k Then Iterate
      bs=bs a.i.j
      End
    End
  Return bs

Exit: Say arg(1)
```

{{out}} Using the defaults for seed and n

```txt
show 1 The given matrix
 10  3  8  6  3  1  0  0  0  0
  5  7  8  8  2  0  1  0  0  0
  4 10  5  4  7  0  0  1  0  0
  9  4  5  3  3  0  0  0  1  0
  6  3  3  3  7  0  0  0  0  1

show 2 1
    10     3     8     6     3     1     0     0     0     0
     0    11     8    10     1    -1     2     0     0     0
     0    22   9/2     4  29/2    -1     0   5/2     0     0
     0  13/9 -22/9  -8/3   1/3    -1     0     0  10/9     0
     0     2    -3    -1  26/3    -1     0     0     0   5/3

show 2 2
      10       3       8       6       3       1       0       0       0       0
       0      11       8      10       1      -1       2       0       0       0
       0       0   -23/4      -8    25/4     1/2      -2     5/4       0       0
       0       0 -346/13 -394/13   20/13  -86/13      -2       0  110/13       0
       0       0   -49/2   -31/2   140/3    -9/2      -2       0       0    55/6

show 2 3
        10         3         8         6         3         1         0         0         0         0
         0        11         8        10         1        -1         2         0         0         0
         0         0     -23/4        -8      25/4       1/2        -2       5/4         0         0
         0         0         0  1005/692 -4095/692 -1335/692  1085/692      -5/4  1265/692         0
         0         0         0   855/196    395/84  -305/196     75/49      -5/4         0  1265/588

show 2 4
           10            3            8            6            3            1            0            0            0            0
            0           11            8           10            1           -1            2            0            0            0
            0            0        -23/4           -8         25/4          1/2           -2          5/4            0            0
            0            0            0     1005/692    -4095/692    -1335/692     1085/692         -5/4     1265/692            0
            0            0            0            0 221375/29583   13915/9861 -13915/13148  16445/19722    -1265/692 84755/118332

Lower part has all zeros

show 3
           10            3            8            6            3            1            0            0            0            0
            0           11            8           10            1           -1            2            0            0            0
            0            0         23/4            8        -25/4         -1/2            2         -5/4            0            0
            0            0            0     1005/692    -4095/692    -1335/692     1085/692         -5/4     1265/692            0
            0            0            0            0 221375/29583   13915/9861 -13915/13148  16445/19722    -1265/692 84755/118332

show 4 5
           10            3            8            6            0       76/175      297/700     -117/350      513/700     -201/700
            0           11            8           10            0     -208/175     1499/700      -39/350      171/700      -67/700
            0            0         23/4            8            0        19/28      125/112       -31/56     -171/112       67/112
            0            0            0     1005/692            0   -1407/1730  10117/13840   -4087/6920   5293/13840   7839/13840
            0            0            0            0 221375/29583   13915/9861 -13915/13148  16445/19722    -1265/692 84755/118332

show 4 4
           10            3            8            0            0      664/175    -1817/700      737/350     -593/700    -1839/700
            0           11            8            0            0      772/175   -6073/2100    4153/1050   -5017/2100    -2797/700
            0            0         23/4            0            0     3611/700  -24449/8400   11339/4200  -30521/8400   -7061/2800
            0            0            0     1005/692            0   -1407/1730  10117/13840   -4087/6920   5293/13840   7839/13840
            0            0            0            0 221375/29583   13915/9861 -13915/13148  16445/19722    -1265/692 84755/118332

show 4 3
           10            3            0            0            0     -592/175    3053/2100   -1733/1050    8837/2100      617/700
            0           11            0            0            0     -484/175    2431/2100     209/1050    5599/2100     -341/700
            0            0         23/4            0            0     3611/700  -24449/8400   11339/4200  -30521/8400   -7061/2800
            0            0            0     1005/692            0   -1407/1730  10117/13840   -4087/6920   5293/13840   7839/13840
            0            0            0            0 221375/29583   13915/9861 -13915/13148  16445/19722    -1265/692 84755/118332

show 4 2
           10            0            0            0            0       -92/35      239/210     -179/105      731/210        71/70
            0           11            0            0            0     -484/175    2431/2100     209/1050    5599/2100     -341/700
            0            0         23/4            0            0     3611/700  -24449/8400   11339/4200  -30521/8400   -7061/2800
            0            0            0     1005/692            0   -1407/1730  10117/13840   -4087/6920   5293/13840   7839/13840
            0            0            0            0 221375/29583   13915/9861 -13915/13148  16445/19722    -1265/692 84755/118332

Upper half has all zeros

show 5
          1          0          0          0          0    -46/175   239/2100  -179/1050   731/2100     71/700
          0          1          0          0          0    -44/175   221/2100    19/1050   509/2100    -31/700
          0          0          1          0          0    157/175 -1063/2100   493/1050 -1327/2100   -307/700
          0          0          0          1          0     -14/25    151/300    -61/150     79/300     39/100
          0          0          0          0          1     33/175    -99/700     39/350   -171/700     67/700

Main diagonal has all ones

show 6 The inverse matrix
    -46/175   239/2100  -179/1050   731/2100     71/700
    -44/175   221/2100    19/1050   509/2100    -31/700
    157/175 -1063/2100   493/1050 -1327/2100   -307/700
     -14/25    151/300    -61/150     79/300     39/100
     33/175    -99/700     39/350   -171/700     67/700

The product of input and inverse matrix
 1 0 0 0 0
 0 1 0 0 0
 0 0 1 0 0
 0 0 0 1 0
 0 0 0 0 1
```



## Sidef

Uses the '''rref(M)''' function from [https://rosettacode.org/wiki/Reduced_row_echelon_form#Sidef Reduced row echelon form].
{{trans|Perl 6}}

```ruby
func gauss_jordan_invert (M) {

    var I = M.len.of {|i|
        M.len.of {|j|
            i == j ? 1 : 0
        }
    }

    var A = gather {
        ^M -> each {|i| take(M[i] + I[i]) }
    }

    rref(A).map { .last(M.len) }
}

var A = [
    [-1, -2, 3, 2],
    [-4, -1, 6, 2],
    [ 7, -8, 9, 1],
    [ 1, -2, 1, 3],
]

say gauss_jordan_invert(A).map {
    .map { "%6s" % .as_rat }.join("  ")
}.join("\n")
```

{{out}}

```txt

-21/23   17/69  13/138   19/46
-38/23   15/23    1/23   15/23
-16/23   25/69  11/138    9/46
-13/23   16/69   -2/69   13/23

```



## VBA

{{trans|Phix}}
Uses ToReducedRowEchelonForm() from [[Reduced_row_echelon_form#VBA]]
```vb
Private Function inverse(mat As Variant) As Variant
    Dim len_ As Integer: len_ = UBound(mat)
    Dim tmp() As Variant
    ReDim tmp(2 * len_ + 1)
    Dim aug As Variant
    ReDim aug(len_)
    For i = 0 To len_
        If UBound(mat(i)) <> len_ Then Debug.Print 9 / 0 '-- "Not a square matrix"
        aug(i) = tmp
        For j = 0 To len_
            aug(i)(j) = mat(i)(j)
        Next j
        '-- augment by identity matrix to right
        aug(i)(i + len_ + 1) = 1
    Next i
    aug = ToReducedRowEchelonForm(aug)
    Dim inv As Variant
    inv = mat
    '-- remove identity matrix to left
    For i = 0 To len_
        For j = len_ + 1 To 2 * len_ + 1
            inv(i)(j - len_ - 1) = aug(i)(j)
        Next j
    Next i
    inverse = inv
End Function
 
Public Sub main()
    Dim test As Variant
    test = inverse(Array( _
        Array(2, -1, 0), _
        Array(-1, 2, -1), _
        Array(0, -1, 2)))
    For i = LBound(test) To UBound(test)
        For j = LBound(test(0)) To UBound(test(0))
            Debug.Print test(i)(j),
        Next j
    Debug.Print
    Next i
End Sub
```
{{out}}

```txt
 0,75          0,5           0,25         
 0,5           1             0,5          
 0,25          0,5           0,75 
```


## zkl

This uses GSL to invert a matrix via LU decomposition, not Gauss-Jordan.

```zkl
var [const] GSL=Import.lib("zklGSL");    // libGSL (GNU Scientific Library)
m:=GSL.Matrix(3,3).set(1,2,3, 4,1,6, 7,8,9);
i:=m.invert();
i.format(10,4).println("\n");
(m*i).format(10,4).println();
```

{{out}}

```txt

   -0.8125,    0.1250,    0.1875
    0.1250,   -0.2500,    0.1250
    0.5208,    0.1250,   -0.1458

    1.0000,    0.0000,    0.0000
   -0.0000,    1.0000,    0.0000
   -0.0000,    0.0000,    1.0000

```


```zkl
m:=GSL.Matrix(3,3).set(2,-1,0, -1,2,-1, 0,-1,2);
m.invert().format(10,4).println("\n");
```

{{out}}

```txt

    0.7500,    0.5000,    0.2500
    0.5000,    1.0000,    0.5000
    0.2500,    0.5000,    0.7500

```

