+++
title = "Multidimensional Newton-Raphson method"
description = ""
date = 2019-07-29T10:02:57Z
aliases = []
[extra]
id = 21754
[taxonomies]
categories = []
tags = []
+++

{{Draft task|Arithmetic operations}}

;Task:
Create a program that finds and outputs the root of a system of nonlinear equations
using Newton-Raphson metod.




## C#

For matrix inversion and matrix and vector definitions - see C# source from [[Gaussian elimination]]

```csharp

using System;

namespace Rosetta
{
    internal interface IFun
    {
        double F(int index, Vector x);
        double df(int index, int derivative, Vector x);
        double[] weights();
    }

    class Newton
    {                
        internal Vector Do(int size, IFun fun, Vector start)
        {
            Vector X = start.Clone();
            Vector F = new Vector(size);
            Matrix J = new Matrix(size, size);
            Vector D;
            do
            {
                for (int i = 0; i < size; i++)
                    F[i] = fun.F(i, X);
                for (int i = 0; i < size; i++)
                    for (int j = 0; j < size; j++)
                        J[i, j] = fun.df(i, j, X);
                J.ElimPartial(F);
                X -= F;
                //need weight vector because different coordinates can diffs by order of magnitudes
            } while (F.norm(fun.weights()) > 1e-12);
            return X;
        }       
    }
}

```


```csharp

using System;

//example from https://eti.pg.edu.pl/documents/176593/26763380/Wykl_AlgorOblicz_7.pdf
namespace Rosetta
{
    class Program
    {
        class Fun: IFun
        {
            private double[] w = new double[] { 1,1 };

            public double F(int index, Vector x)
            {
                switch (index)
                {
                    case 0: return Math.Atan(x[0]) - x[1] * x[1] * x[1];
                    case 1: return 4 * x[0] * x[0] + 9 * x[1] * x[1] - 36;
                }
                throw new Exception("bad index");
            }

            public double df(int index, int derivative, Vector x)
            {
                switch (index)
                {
                    case 0:
                        switch (derivative)
                        {
                            case 0: return 1 / (1 + x[0] * x[0]);
                            case 1: return -3*x[1]*x[1];
                        }
                        break;
                    case 1:
                        switch (derivative)
                        {
                            case 0: return 8 * x[0];
                            case 1: return 18 * x[1];
                        }
                        break;
                }
                throw new Exception("bad index");
            }
            public double[] weights() { return w; }
        }

        static void Main(string[] args)
        {
            Fun fun = new Fun();
            Newton newton = new Newton();
            Vector start = new Vector(new double[] { 2.75, 1.25 });
            Vector X = newton.Do(2, fun, start);
            X.print();
        }
    }
}

```

{{out}}
```txt

2.54258545959024
1.06149981539336

```



## Go

{{trans|Kotlin}}


We follow the Kotlin example of coding our own matrix methods rather than using a third party library.

```go
package main

import (
    "fmt"
    "math"
)

type vector = []float64
type matrix []vector
type fun = func(vector) float64
type funs = []fun
type jacobian = []funs

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

func (m1 matrix) sub(m2 matrix) matrix {
    rows, cols := len(m1), len(m1[0])
    if rows != len(m2) || cols != len(m2[0]) {
        panic("Matrices cannot be subtracted.")
    }
    result := make(matrix, rows)
    for i := 0; i < rows; i++ {
        result[i] = make(vector, cols)
        for j := 0; j < cols; j++ {
            result[i][j] = m1[i][j] - m2[i][j]
        }
    }
    return result
}

func (m matrix) transpose() matrix {
    rows, cols := len(m), len(m[0])
    trans := make(matrix, cols)
    for i := 0; i < cols; i++ {
        trans[i] = make(vector, rows)
        for j := 0; j < rows; j++ {
            trans[i][j] = m[j][i]
        }
    }
    return trans
}

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

func solve(fs funs, jacob jacobian, guesses vector) vector {
    size := len(fs)
    var gu1 vector
    gu2 := make(vector, len(guesses))
    copy(gu2, guesses)
    jac := make(matrix, size)
    for i := 0; i < size; i++ {
        jac[i] = make(vector, size)
    }
    tol := 1e-8
    maxIter := 12
    iter := 0
    for {
        gu1 = gu2
        g := matrix{gu1}.transpose()
        t := make(vector, size)
        for i := 0; i < size; i++ {
            t[i] = fs[i](gu1)
        }
        f := matrix{t}.transpose()
        for i := 0; i < size; i++ {
            for j := 0; j < size; j++ {
                jac[i][j] = jacob[i][j](gu1)
            }
        }
        g1 := g.sub(jac.inverse().mul(f))
        gu2 = make(vector, size)
        for i := 0; i < size; i++ {
            gu2[i] = g1[i][0]
        }
        iter++
        any := false
        for i, v := range gu2 {
            if math.Abs(v)-gu1[i] > tol {
                any = true
                break
            }
        }
        if !any || iter >= maxIter {
            break
        }
    }
    return gu2
}

func main() {
    /*
       solve the two non-linear equations:
       y = -x^2 + x + 0.5
       y + 5xy = x^2
       given initial guesses of x = y = 1.2

       Example taken from:
       http://www.fixoncloud.com/Home/LoginValidate/OneProblemComplete_Detailed.php?problemid=286

       Expected results: x = 1.23332, y = 0.2122
    */
    f1 := func(x vector) float64 { return -x[0]*x[0] + x[0] + 0.5 - x[1] }
    f2 := func(x vector) float64 { return x[1] + 5*x[0]*x[1] - x[0]*x[0] }
    fs := funs{f1, f2}
    jacob := jacobian{
        funs{
            func(x vector) float64 { return -2*x[0] + 1 },
            func(x vector) float64 { return -1 },
        },
        funs{
            func(x vector) float64 { return 5*x[1] - 2*x[0] },
            func(x vector) float64 { return 1 + 5*x[0] },
        },
    }
    guesses := vector{1.2, 1.2}
    sol := solve(fs, jacob, guesses)
    fmt.Printf("Approximate solutions are x = %.7f,  y = %.7f\n", sol[0], sol[1])

    /*
       solve the three non-linear equations:
       9x^2 + 36y^2 + 4z^2 - 36 = 0
       x^2 - 2y^2 - 20z = 0
       x^2 - y^2 + z^2 = 0
       given initial guesses of x = y = 1.0 and z = 0.0

       Example taken from:
       http://mathfaculty.fullerton.edu/mathews/n2003/FixPointNewtonMod.html (exercise 5)

       Expected results: x = 0.893628, y = 0.894527, z = -0.0400893
    */

    fmt.Println()
    f3 := func(x vector) float64 { return 9*x[0]*x[0] + 36*x[1]*x[1] + 4*x[2]*x[2] - 36 }
    f4 := func(x vector) float64 { return x[0]*x[0] - 2*x[1]*x[1] - 20*x[2] }
    f5 := func(x vector) float64 { return x[0]*x[0] - x[1]*x[1] + x[2]*x[2] }
    fs = funs{f3, f4, f5}
    jacob = jacobian{
        funs{
            func(x vector) float64 { return 18 * x[0] },
            func(x vector) float64 { return 72 * x[1] },
            func(x vector) float64 { return 8 * x[2] },
        },
        funs{
            func(x vector) float64 { return 2 * x[0] },
            func(x vector) float64 { return -4 * x[1] },
            func(x vector) float64 { return -20 },
        },
        funs{
            func(x vector) float64 { return 2 * x[0] },
            func(x vector) float64 { return -2 * x[1] },
            func(x vector) float64 { return 2 * x[2] },
        },
    }
    guesses = vector{1, 1, 0}
    sol = solve(fs, jacob, guesses)
    fmt.Printf("Approximate solutions are x = %.7f,  y = %.7f,  z = %.7f\n", sol[0], sol[1], sol[2])
}
```


{{out}}

```txt

Approximate solutions are x = 1.2333178,  y = 0.2122450

Approximate solutions are x = 0.8936282,  y = 0.8945270,  z = -0.0400893

```




## Julia

NLsolve is a Julia package for nonlinear systems of equations, with the Newton-Raphson method one of the choices for solvers.

```julia
# from the NLSolve documentation: to solve
#     (x, y) -> ((x+3)*(y^3-7)+18, sin(y*exp(x)-1))
using NLsolve

function f!(F, x)
    F[1] = (x[1]+3)*(x[2]^3-7)+18
    F[2] = sin(x[2]*exp(x[1])-1)
end

function j!(J, x)
    J[1, 1] = x[2]^3-7
    J[1, 2] = 3*x[2]^2*(x[1]+3)
    u = exp(x[1])*cos(x[2]*exp(x[1])-1)
    J[2, 1] = x[2]*u
    J[2, 2] = u
end

println(nlsolve(f!, j!, [ 0.1; 1.2], method = :newton))

```
{{out}}

```txt

Results of Nonlinear Solver Algorithm
 * Algorithm: Newton with line-search
 * Starting Point: [0.1, 1.2]
 * Zero: [-3.7818e-16, 1.0]
 * Inf-norm of residuals: 0.000000
 * Iterations: 4
 * Convergence: true
   * |x - x'| < 0.0e+00: false
   * |f(x)| < 1.0e-08: true
 * Function Calls (f): 5
 * Jacobian Calls (df/dx): 4

```





## Kotlin

A straightforward approach multiplying by the inverse of the Jacobian, rather than dividing by f'(x) as one would do in the single dimensional case, which is quick enough here.

As neither the JDK nor the Kotlin Standard Library have matrix functions built in, most of the functions used have been taken from other tasks. 

```scala
// Version 1.2.31

import kotlin.math.abs

typealias Vector = DoubleArray
typealias Matrix = Array<Vector>
typealias Func = (Vector) -> Double
typealias Funcs = List<Func>
typealias Jacobian = List<Funcs>

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

operator fun Matrix.minus(other: Matrix): Matrix {
    val rows = this.size
    val cols = this[0].size
    require(rows == other.size && cols == other[0].size)
    val result = Matrix(rows) { Vector(cols) }
    for (i in 0 until rows) {
        for (j in 0 until cols) {
            result[i][j] = this[i][j] - other[i][j]
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

fun solve(funcs: Funcs, jacobian: Jacobian, guesses: Vector): Vector {
    val size = funcs.size
    var gu1: Vector
    var gu2 = guesses.copyOf()
    val jac = Matrix(size) { Vector(size) }
    val tol = 1.0e-8
    val maxIter = 12
    var iter = 0
    do {
        gu1 = gu2
        val g = arrayOf(gu1).transpose()
        val f = arrayOf(Vector(size) { funcs[it](gu1) }).transpose()
        for (i in 0 until size) {
            for (j in 0 until size) {
                jac[i][j] = jacobian[i][j](gu1)
            }
        }
        val g1 = g - jac.inverse() * f
        gu2 = Vector(size) { g1[it][0] }
        iter++
    }
    while (gu2.withIndex().any { iv -> abs(iv.value - gu1[iv.index]) > tol } && iter < maxIter)
    return gu2
}

fun main(args: Array<String>) {
    /* solve the two non-linear equations:
       y = -x^2 + x + 0.5
       y + 5xy = x^2
       given initial guesses of x = y = 1.2

       Example taken from:
       http://www.fixoncloud.com/Home/LoginValidate/OneProblemComplete_Detailed.php?problemid=286

       Expected results: x = 1.23332, y = 0.2122
    */

    val f1: Func = { x -> -x[0] * x[0] + x[0] + 0.5 - x[1] }
    val f2: Func = { x -> x[1] + 5 * x[0] * x[1] - x[0] * x[0] }
    val funcs = listOf(f1, f2)
    val jacobian = listOf(
        listOf<Func>({ x -> - 2.0 * x[0] + 1.0 }, { _ -> -1.0 }),
        listOf<Func>({ x -> 5.0 * x[1] - 2.0 * x[0] }, { x -> 1.0 + 5.0 * x[0] })
    )
    val guesses = doubleArrayOf(1.2, 1.2)
    val (xx, yy) = solve(funcs, jacobian, guesses)
    System.out.printf("Approximate solutions are x = %.7f,  y = %.7f\n", xx, yy)

    /* solve the three non-linear equations:
       9x^2 + 36y^2 + 4z^2 - 36 = 0
       x^2 - 2y^2 - 20z = 0
       x^2 - y^2 + z^2 = 0
       given initial guesses of x = y = 1.0 and z = 0.0

       Example taken from:
       http://mathfaculty.fullerton.edu/mathews/n2003/FixPointNewtonMod.html (exercise 5)

       Expected results: x = 0.893628, y = 0.894527, z = -0.0400893
    */

    println()
    val f3: Func = { x -> 9.0 * x[0] * x[0] + 36.0 * x[1] * x[1] + 4.0 * x[2] * x[2] - 36.0 }
    val f4: Func = { x -> x[0] * x[0] - 2.0 * x[1] * x[1] - 20.0 * x[2] }
    val f5: Func = { x -> x[0] * x[0] - x[1] * x[1] + x[2] * x[2] }
    val funcs2 = listOf(f3, f4, f5)
    val jacobian2 = listOf(
        listOf<Func>({ x -> 18.0 * x[0] }, { x -> 72.0 * x[1] }, { x -> 8.0 * x[2] }),
        listOf<Func>({ x -> 2.0 * x[0] }, { x -> -4.0 * x[1] }, { _ -> -20.0 }),
        listOf<Func>({ x -> 2.0 * x[0] }, { x -> -2.0 * x[1] }, { x -> 2.0 * x[2] })
    )
    val guesses2 = doubleArrayOf(1.0, 1.0, 0.0)
    val (xx2, yy2, zz2) = solve(funcs2, jacobian2, guesses2)
    System.out.printf("Approximate solutions are x = %.7f,  y = %.7f,  z = %.7f\n", xx2, yy2, zz2)
}
```


{{out}}

```txt

Approximate solutions are x = 1.2333178,  y = 0.2122450

Approximate solutions are x = 0.8936282,  y = 0.8945270,  z = -0.0400893

```



## Perl 6


```perl6
#!/usr/bin/env perl6

# Reference:
# https://github.com/pierre-vigier/Perl6-Math-Matrix
# Mastering Algorithms with Perl
# By Jarkko Hietaniemi, John Macdonald, Jon Orwant
# Publisher: O'Reilly Media, ISBN-10: 1565923987
# https://resources.oreilly.com/examples/9781565923980/blob/master/ch16/solve

use v6;

sub solve_funcs ($funcs, @guesses, $iterations, $epsilon) {
   my ($error_value, @values, @delta, @jacobian); my \ε = $epsilon;
   for 1 .. $iterations {
      for ^+$funcs { @values[$^i] = $funcs[$^i](|@guesses); }
      $error_value = 0;
      for ^+$funcs { $error_value += @values[$^i].abs }
      return @guesses if $error_value ≤ ε;
      for ^+$funcs { @delta[$^i] = -@values[$^i] }
      @jacobian = jacobian $funcs, @guesses, ε;
      @delta = solve_matrix @jacobian, @delta;
      loop (my $j = 0, $error_value = 0; $j < +$funcs; $j++) {
         $error_value += @delta[$j].abs ;
         @guesses[$j] += @delta[$j];
      }
      return @guesses if $error_value ≤ ε;
   }
   return @guesses;
}

sub jacobian ($funcs is copy, @points is copy, $epsilon is copy) {
   my ($Δ, @P, @M, @plusΔ, @minusΔ);
   my Array @jacobian; my \ε = $epsilon;
   for ^+@points -> $i {
      @plusΔ = @minusΔ = @points;
      $Δ = (ε * @points[$i].abs) || ε;
      @plusΔ[$i] = @points[$i] + $Δ;
      @minusΔ[$i] = @points[$i] - $Δ;
      for ^+$funcs { @P[$^k] = $funcs[$^k](|@plusΔ); }
      for ^+$funcs { @M[$^k] = $funcs[$^k](|@minusΔ); }
      for ^+$funcs -> $j {
         @jacobian[$j][$i] = (@P[$j] - @M[$j]) / (2 * $Δ);
      }
   }
   return @jacobian;
}

sub solve_matrix (@matrix_array is copy, @delta is copy) {
   # https://github.com/pierre-vigier/Perl6-Math-Matrix/issues/56
   { use Math::Matrix;
      my $matrix = Math::Matrix.new(@matrix_array);
      my $vector = Math::Matrix.new(@delta.map({.list}));
      die "Matrix is not invertible" unless $matrix.is-invertible;
      my @result = ( $matrix.inverted dot $vector ).transposed;
      return @result.split(" ");
   }
}

my $funcs = [
   { 9*$^x² + 36*$^y² + 4*$^z² - 36 }
   { $^x² - 2*$^y² - 20*$^z }
   { $^x² - $^y² + $^z² }
];

my @guesses = (1,1,0);

my @solution = solve_funcs $funcs, @guesses, 20, 1e-8;

say "Solution: ", @solution;

```

{{out}}

```txt

Solution: [0.8936282344764825 0.8945270103905782 -0.04008928615915281]

```



## Phix

{{trans|Go}}
Uses code from [[Reduced_row_echelon_form#Phix]],
[[Gauss-Jordan_matrix_inversion#Phix]],
[[Matrix_transposition#Phix]], and 
[[Matrix_multiplication#Phix]]

See std distro for a complete runnable version.

```Phix
-- demo\rosetta\Multidimensional_Newton-Raphson_method.exw
function solve(sequence fs, jacob, guesses)
    integer size := length(fs),
            maxIter := 12,
            iter := 0
    sequence gu1, g, t, f, g1,
             gu2 := guesses,
             jac := repeat(repeat(0,size),size)
    atom tol := 1e-8
    while true do
        gu1 := gu2
        g := matrix_transpose({gu1})
        t := repeat(0, size)
        for i=1 to size do
            t[i] := call_func(fs[i],{gu1})
        end for
        f := matrix_transpose({t})
        for i=1 to size do
            for j=1 to size do
                jac[i][j] := call_func(jacob[i][j],{gu1})
            end for
        end for
        g1 := sq_sub(g,matrix_mul(inverse(jac),f))
        gu2 := vslice(g1,1)
        iter += 1
        bool any := find(true,sq_gt(sq_sub(sq_abs(gu2),gu1),tol))!=0
        if not any or iter >= maxIter then exit end if
    end while
    return gu2
end function

function f1(sequence v) atom {x,y} = v return -x*x+x+0.5-y end function
function f2(sequence v) atom {x,y} = v return y+5*x*y-x*x end function
function f3(sequence v) atom {x,y,z} = v return 9*x*x+36*y*y+4*z*z-36 end function
function f4(sequence v) atom {x,y,z} = v return x*x-2*y*y-20*z end function
function f5(sequence v) atom {x,y,z} = v return x*x-y*y+z*z end function

function j1(sequence v) atom {x} = v return -2*x+1 end function
function j2(sequence /*v*/) return -1 end function
function j3(sequence v) atom {x,y} = v return 5*y-2*x end function
function j4(sequence v) atom {x} = v return 1+5*x end function
function j11(sequence v) atom {x} = v return 18*x end function
function j12(sequence v) atom {?,y} = v return 72*y end function
function j13(sequence v) atom {?,?,z} = v return 8*z end function
function j21(sequence v) atom {x} = v return 2*x end function
function j22(sequence v) atom {?,y} = v return -4*y end function
function j23(sequence /*v*/) return -20 end function
function j31(sequence v) atom {x} = v return 2*x end function
function j32(sequence v) atom {?,y} = v return -2*y end function
function j33(sequence v) atom {?,?,z} = v return 2*z end function

procedure main()
sequence fs, jacob, guesses
    /*
       solve the two non-linear equations:
       y = -x^2 + x + 0.5
       y + 5xy = x^2
       given initial guesses of x = y = 1.2
 
       Example taken from:
       http://www.fixoncloud.com/Home/LoginValidate/OneProblemComplete_Detailed.php?problemid=286
 
       Expected results: x = 1.23332, y = 0.2122
    */
    fs = {routine_id("f1"),routine_id("f2")}
    jacob = {{routine_id("j1"),routine_id("j2")},
             {routine_id("j3"),routine_id("j4")}}
    guesses := {1.2, 1.2}
    printf(1,"Approximate solutions are x = %.7f,  y = %.7f\n\n", solve(fs, jacob, guesses))
 
    /*
       solve the three non-linear equations:
       9x^2 + 36y^2 + 4z^2 - 36 = 0
       x^2 - 2y^2 - 20z = 0
       x^2 - y^2 + z^2 = 0
       given initial guesses of x = y = 1.0 and z = 0.0
 
       Example taken from:
       http://mathfaculty.fullerton.edu/mathews/n2003/FixPointNewtonMod.html (exercise 5)
 
       Expected results: x = 0.893628, y = 0.894527, z = -0.0400893
    */
 
    fs = {routine_id("f3"), routine_id("f4"), routine_id("f5")}
    jacob = {{routine_id("j11"),routine_id("j12"),routine_id("j13")},
             {routine_id("j21"),routine_id("j22"),routine_id("j23")},
             {routine_id("j31"),routine_id("j32"),routine_id("j33")}}
    guesses = {1, 1, 0}
    printf(1,"Approximate solutions are x = %.7f,  y = %.7f,  z = %.7f\n", solve(fs, jacob, guesses))

end procedure
main()
```

{{out}}

```txt

Approximate solutions are x = 1.2333178,  y = 0.2122450

Approximate solutions are x = 0.8936282,  y = 0.8945270,  z = -0.04008929

```



## zkl

This doesn't use Newton-Raphson (with derivatives) but a hybrid algorithm.

```zkl
var [const] GSL=Import.lib("zklGSL");    // libGSL (GNU Scientific Library)

   // two functions of two variables: f(x,y)=0
fs:=T(fcn(x,y){ x.atan() - y*y*y }, fcn(x,y){ 4.0*x*x + 9*y*y - 36 });
v=GSL.VectorFromData(2.75, 1.25);	// an initial guess at the solution
GSL.multiroot_fsolver(fs,v);
v.format(11,8).println();		// answer overwrites initial guess

fs.run(True,v.toList().xplode()).println();	// deltas from zero
```

{{out}}

```txt

 2.59807621, 1.06365371
L(2.13651e-09,2.94321e-10)

```

A condensed solver (for a different set of functions):

```zkl
v:=GSL.VectorFromData(-10.0, -15.0);
GSL.multiroot_fsolver(T( fcn(x,y){ 1.0 - x }, fcn(x,y){ 10.0*(y - x*x) }),v)
.format().println();	// --> (1,1)
```

{{out}}

```txt

1.00,1.00

```

Another example:

```zkl
v:=GSL.VectorFromData(1.0, 1.0, 0.0);	// initial guess
fxyzs:=T(
   fcn(x,y,z){ x*x*9 + y*y*36 + z*z*4 - 36 }, // 9x^2 + 36y^2 + 4z^2 - 36 = 0
   fcn(x,y,z){ x*x - y*y*2 - z*20 },	      // x^2 - 2y^2 - 20z = 0
   fcn(x,y,z){ x*x - y*y + z*z });	      // x^2 - y^2 + z^2 = 0
(v=GSL.multiroot_fsolver(fxyzs,v)).format(12,8).println();

fxyzs.run(True,v.toList().xplode()).println();	// deltas from zero
```

{{out}}

```txt

  0.89362824,  0.89452701, -0.04008929
L(6.00672e-08,1.0472e-08,9.84017e-09)

```

