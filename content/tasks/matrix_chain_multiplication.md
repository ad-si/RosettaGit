+++
title = "Matrix chain multiplication"
description = ""
date = 2019-10-05T21:18:40Z
aliases = []
[extra]
id = 21778
[taxonomies]
categories = ["task", "Discrete math"]
tags = []
+++

;Problem
Using the most straightfoward algorithm (which we assume here), computing the [[Matrix multiplication|product of two matrices]] of dimensions (n1,n2) and (n2,n3) requires n1*n2*n3 [[wp:Multiply–accumulate_operation|FMA]] operations. The number of operations required to compute the product of matrices A1, A2... An depends on the order of matrix multiplications, hence on where parens are put. Remember that the matrix product is associative, but not commutative, hence only the parens can be moved.

For instance, with four matrices, one can compute A(B(CD)), A((BC)D), (AB)(CD), (A(BC))D, (AB)C)D. The number of different ways to put the parens is a [[Catalan numbers|Catalan number]], and grows exponentially with the number of factors.

Here is an example of computation of the total cost, for matrices A(5,6), B(6,3), C(3,1):

* AB costs 5*6*3=90 and produces a matrix of dimensions (5,3), then (AB)C costs 5*3*1=15. The total cost is 105.
* BC costs 6*3*1=18 and produces a matrix of dimensions (6,1), then A(BC) costs 5*6*1=30. The total cost is 48.

In this case, computing (AB)C requires more than twice as many operations as A(BC). The difference can be much more dramatic in real cases.

## Task

Write a function which, given a list of the successive dimensions of matrices A1, A2... An, of arbitrary length, returns the optimal way to compute the matrix product, and the total cost. Any sensible way to describe the optimal solution is accepted. The input list does not duplicate shared dimensions: for the previous example of matrices A,B,C, one will only pass the list [5,6,3,1] (and ''not'' [5,6,6,3,3,1]) to mean the matrix dimensions are respectively (5,6), (6,3) and (3,1). Hence, a product of n matrices is represented by a list of n+1 dimensions.

Try this function on the following two lists:

* [1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]
* [1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]

To solve the task, it's possible, but not required, to write a function that enumerates all possible ways to parenthesize the product. This is not optimal because of the many duplicated computations, and this task is a classic application of [[:wp:Dynamic programming|dynamic programming]].

See also [[:wp:Matrix chain multiplication|Matrix chain multiplication]] on Wikipedia.

__TOC__

## C

```c
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>

int **m;
int **s;

void optimal_matrix_chain_order(int *dims, int n) {
    int len, i, j, k, temp, cost;
    n--;
    m = (int **)malloc(n * sizeof(int *));
    for (i = 0; i < n; ++i) {
        m[i] = (int *)calloc(n, sizeof(int));
    }

    s = (int **)malloc(n * sizeof(int *));
    for (i = 0; i < n; ++i) {
        s[i] = (int *)calloc(n, sizeof(int));
    }

    for (len = 1; len < n; ++len) {
        for (i = 0; i < n - len; ++i) {
            j = i + len;
            m[i][j] = INT_MAX;
            for (k = i; k < j; ++k) {
                temp = dims[i] * dims[k + 1] * dims[j + 1];
                cost = m[i][k] + m[k + 1][j] + temp;
                if (cost < m[i][j]) {
                    m[i][j] = cost;
                    s[i][j] = k;
                }
            }
        }
    }
}

void print_optimal_chain_order(int i, int j) {
    if (i == j)
        printf("%c", i + 65);
    else {
        printf("(");
        print_optimal_chain_order(i, s[i][j]);
        print_optimal_chain_order(s[i][j] + 1, j);
        printf(")");
    }
}

int main() {
    int i, j, n;
    int a1[4]  = {5, 6, 3, 1};
    int a2[13] = {1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2};
    int a3[12] = {1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10};
    int *dims_list[3] = {a1, a2, a3};
    int sizes[3] = {4, 13, 12};
    for (i = 0; i < 3; ++i) {
        printf("Dims  : [");
        n = sizes[i];
        for (j = 0; j < n; ++j) {
            printf("%d", dims_list[i][j]);
            if (j < n - 1) printf(", "); else printf("]\n");
        }
        optimal_matrix_chain_order(dims_list[i], n);
        printf("Order : ");
        print_optimal_chain_order(0, n - 2);
        printf("\nCost  : %d\n\n", m[0][n - 2]);
        for (j = 0; j <= n - 2; ++j) free(m[j]);
        free(m);
        for (j = 0; j <= n - 2; ++j) free(s[j]);
        free(s);
    }
    return 0;
}
```


```txt

Dims  : [5, 6, 3, 1]
Order : (A(BC))
Cost  : 48

Dims  : [1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]
Order : ((((((((AB)C)D)E)F)G)(H(IJ)))(KL))
Cost  : 38120

Dims  : [1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]
Order : (A((((((BC)D)(((EF)G)H))I)J)K))
Cost  : 1773740

```



## C#

```c#
using System;

class MatrixChainOrderOptimizer {
    private int[,] m;
    private int[,] s;

    void OptimalMatrixChainOrder(int[] dims) {
        int n = dims.Length - 1;
        m = new int[n, n];
        s = new int[n, n];
        for (int len = 1; len < n; ++len) {
            for (int i = 0; i < n - len; ++i) {
                int j = i + len;
                m[i, j] = Int32.MaxValue;
                for (int k = i; k < j; ++k) {
                    int temp = dims[i] * dims[k + 1] * dims[j + 1];
                    int cost = m[i, k] + m[k + 1, j] + temp;
                    if (cost < m[i, j]) {
                        m[i, j] = cost;
                        s[i, j] = k;
                    }
                }
            }
        }
    }

    void PrintOptimalChainOrder(int i, int j) {
        if (i == j)
            Console.Write((char)(i + 65));
        else {
            Console.Write("(");
            PrintOptimalChainOrder(i, s[i, j]);
            PrintOptimalChainOrder(s[i, j] + 1, j);
            Console.Write(")");
        }
    }

    static void Main() {
        var mcoo = new MatrixChainOrderOptimizer();
        var dimsList = new int[3][];
        dimsList[0] = new int[4] {5, 6, 3, 1};
        dimsList[1] = new int[13] {1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2};
        dimsList[2] = new int[12] {1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10};
        for (int i = 0; i < dimsList.Length; ++i) {
            Console.Write("Dims  : [");
            int n = dimsList[i].Length;
            for (int j = 0; j < n; ++j) {
                Console.Write(dimsList[i][j]);
                if (j < n - 1)
                    Console.Write(", ");
                else
                    Console.WriteLine("]");
            }
            mcoo.OptimalMatrixChainOrder(dimsList[i]);
            Console.Write("Order : ");
            mcoo.PrintOptimalChainOrder(0, n - 2);
            Console.WriteLine("\nCost  : {0}\n",  mcoo.m[0, n - 2]);
        }
    }
}
```


```txt

Dims  : [5, 6, 3, 1]
Order : (A(BC))
Cost  : 48

Dims  : [1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]
Order : ((((((((AB)C)D)E)F)G)(H(IJ)))(KL))
Cost  : 38120

Dims  : [1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]
Order : (A((((((BC)D)(((EF)G)H))I)J)K))
Cost  : 1773740

```



## Fortran

This is a translation of the Python iterative solution.


```fortran
module optim_mod
    implicit none
contains
    subroutine optim(a)
        implicit none
        integer :: a(:), n, i, j, k
        integer, allocatable :: u(:, :)
        integer(8) :: c
        integer(8), allocatable :: v(:, :)

        n = ubound(a, 1) - 1
        allocate (u(n, n), v(n, n))
        v = huge(v)
        u(:, 1) = -1
        v(:, 1) = 0
        do j = 2, n
            do i = 1, n - j + 1
                do k = 1, j - 1
                    c = v(i, k) + v(i + k, j - k) + int(a(i), 8) * int(a(i + k), 8) * int(a(i + j), 8)
                    if (c < v(i, j)) then
                        u(i, j) = k
                        v(i, j) = c
                    end if
                end do
            end do
        end do
        write (*, "(I0,' ')", advance="no") v(1, n)
        call aux(1, n)
        print *
        deallocate (u, v)
    contains
        recursive subroutine aux(i, j)
            integer :: i, j, k

            k = u(i, j)
            if (k < 0) then
                write (*, "(I0)", advance="no") i
            else
                write (*, "('(')", advance="no")
                call aux(i, k)
                write (*, "('*')", advance="no")
                call aux(i + k, j - k)
                write (*, "(')')", advance="no")
            end if
        end subroutine
    end subroutine
end module

program matmulchain
    use optim_mod
    implicit none

    call optim([5, 6, 3, 1])
    call optim([1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2])
    call optim([1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10])
end program
```


'''Output'''


```txt

48 (1*(2*3))
38120 ((((((((1*2)*3)*4)*5)*6)*7)*(8*(9*10)))*(11*12))
1773740 (1*((((((2*3)*4)*(((5*6)*7)*8))*9)*10)*11))

```



## Go

The first <code>for</code> loop is based on the pseudo and Java code from the
[[wp:Matrix_chain_multiplication#A_dynamic_programming_algorithm|Wikipedia article]].

```Go
package main

import "fmt"

// PrintMatrixChainOrder prints the optimal order for chain
// multiplying matrices.
// Matrix A[i] has dimensions dims[i-1]×dims[i].
func PrintMatrixChainOrder(dims []int) {
	n := len(dims) - 1
	m, s := newSquareMatrices(n)

	// m[i,j] will be minimum number of scalar multiplactions
	// needed to compute the matrix A[i]A[i+1]…A[j] = A[i…j].
	// Note, m[i,i] = zero (no cost).
	// s[i,j] will be the index of the subsequence split that
	// achieved minimal cost.
	for lenMinusOne := 1; lenMinusOne < n; lenMinusOne++ {
		for i := 0; i < n-lenMinusOne; i++ {
			j := i + lenMinusOne
			m[i][j] = -1
			for k := i; k < j; k++ {
				cost := m[i][k] + m[k+1][j] + dims[i]*dims[k+1]*dims[j+1]
				if m[i][j] < 0 || cost < m[i][j] {
					m[i][j] = cost
					s[i][j] = k
				}
			}
		}
	}

	// Format and print result.
	const MatrixNames = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	var subprint func(int, int)
	subprint = func(i, j int) {
		if i == j {
			return
		}
		k := s[i][j]
		subprint(i, k)
		subprint(k+1, j)
		fmt.Printf("%*s -> %s × %s%*scost=%d\n",
			n, MatrixNames[i:j+1],
			MatrixNames[i:k+1],
			MatrixNames[k+1:j+1],
			n+i-j, "", m[i][j],
		)
	}
	subprint(0, n-1)
}

func newSquareMatrices(n int) (m, s [][]int) {
	// Allocates two n×n matrices as slices of slices but
	// using only one [2n][]int and one [2n²]int backing array.
	m = make([][]int, 2*n)
	m, s = m[:n:n], m[n:]
	tmp := make([]int, 2*n*n)
	for i := range m {
		m[i], tmp = tmp[:n:n], tmp[n:]
	}
	for i := range s {
		s[i], tmp = tmp[:n:n], tmp[n:]
	}
	return m, s
}

func main() {
	cases := [...][]int{
		{1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2},
		{1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10},
	}
	for _, tc := range cases {
		fmt.Println("Dimensions:", tc)
		PrintMatrixChainOrder(tc)
		fmt.Println()
	}
}
```

```txt

Dimensions: [1 5 25 30 100 70 2 1 100 250 1 1000 2]
          AB -> A × B           cost=125
         ABC -> AB × C          cost=875
        ABCD -> ABC × D         cost=3875
       ABCDE -> ABCD × E        cost=10875
      ABCDEF -> ABCDE × F       cost=11015
     ABCDEFG -> ABCDEF × G      cost=11017
          IJ -> I × J           cost=25000
         HIJ -> H × IJ          cost=25100
  ABCDEFGHIJ -> ABCDEFG × HIJ   cost=36118
          KL -> K × L           cost=2000
ABCDEFGHIJKL -> ABCDEFGHIJ × KL cost=38120

Dimensions: [1000 1 500 12 1 700 2500 3 2 5 14 10]
         BC -> B × C          cost=6000
        BCD -> BC × D         cost=6012
         EF -> E × F          cost=1750000
        EFG -> EF × G         cost=1757500
       EFGH -> EFG × H        cost=1757506
    BCDEFGH -> BCD × EFGH     cost=1763520
   BCDEFGHI -> BCDEFGH × I    cost=1763530
  BCDEFGHIJ -> BCDEFGHI × J   cost=1763600
 BCDEFGHIJK -> BCDEFGHIJ × K  cost=1763740
ABCDEFGHIJK -> A × BCDEFGHIJK cost=1773740


```



## Haskell


```Haskell
import Data.List (elemIndex)
import Data.Char (chr, ord)
import Data.Maybe (fromJust)

mats :: [[Int]]
mats =
  [ [5, 6, 3, 1]
  , [1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]
  , [1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]
  ]

cost :: [Int] -> Int -> Int -> (Int, Int)
cost a i j
  | i < j =
    let m =
          [ fst (cost a i k) + fst (cost a (k + 1) j) +
           (a !! i) * (a !! (j + 1)) * (a !! (k + 1))
          | k <- [i .. j - 1] ]
        mm = minimum m
    in (mm, fromJust (elemIndex mm m) + i)
  | otherwise = (0, -1)

optimalOrder :: [Int] -> Int -> Int -> String
optimalOrder a i j
  | i < j =
    let c = cost a i j
    in "(" ++ optimalOrder a i (snd c) ++ optimalOrder a (snd c + 1) j ++ ")"
  | otherwise = [chr ((+ i) $ ord 'a')]

printBlock :: [Int] -> IO ()
printBlock v =
  let c = cost v 0 (length v - 2)
  in putStrLn
       ("for " ++
        show v ++
        " we have " ++
        show (fst c) ++
        " possibilities, z.B " ++ optimalOrder v 0 (length v - 2))

main :: IO ()
main = mapM_ printBlock mats
```

```txt
for [5,6,3,1] we have 48 possibilities, z.B (a(bc))
for [1,5,25,30,100,70,2,1,100,250,1,1000,2] we have 38120 possibilities, z.B ((((((((ab)c)d)e)f)g)(h(ij)))(kl))
for [1000,1,500,12,1,700,2500,3,2,5,14,10] we have 1773740 possibilities, z.B (a((((((bc)d)(((ef)g)h))i)j)k)
```



## Julia

'''Module''':

```julia
module MatrixChainMultiplications

using OffsetArrays

function optim(a)
    n = length(a) - 1
    u = fill!(OffsetArray{Int}(0:n, 0:n), 0)
    v = fill!(OffsetArray{Int}(0:n, 0:n), typemax(Int))
    u[:, 1] .= -1
    v[:, 1] .= 0
    for j in 2:n, i in 1:n-j+1, k in 1:j-1
        c = v[i, k] + v[i+k, j-k] + a[i] * a[i+k] * a[i+j]
        if c < v[i, j]
            u[i, j] = k
            v[i, j] = c
        end
    end
    return v[1, n], aux(u, 1, n)
end

function aux(u, i, j)
    k = u[i, j]
    if k < 0
        return sprint(print, i)
    else
        return sprint(print, '(', aux(u, i, k), '×', aux(u, i + k, j - k), ")")
    end
end

end  # module MatrixChainMultiplications
```


'''Main''':

```julia
println(MatrixChainMultiplications.optim([1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]))
println(MatrixChainMultiplications.optim([1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]))
```


```txt
(38120, "((((((((1×2)×3)×4)×5)×6)×7)×(8×(9×10)))×(11×12))")
(1773740, "(1×((((((2×3)×4)×(((5×6)×7)×8))×9)×10)×11))")
```



## Kotlin

This is based on the pseudo-code in the Wikipedia article.

```scala
// Version 1.2.31

lateinit var m: List<IntArray>
lateinit var s: List<IntArray>

fun optimalMatrixChainOrder(dims: IntArray) {
    val n = dims.size - 1
    m = List(n) { IntArray(n) }
    s = List(n) { IntArray(n) }
    for (len in 1 until n) {
        for (i in 0 until n - len) {
            val j = i + len
            m[i][j] = Int.MAX_VALUE
            for (k in i until j) {
                val temp = dims[i] * dims [k + 1] * dims[j + 1]
                val cost = m[i][k] + m[k + 1][j] + temp
                if (cost < m[i][j]) {
                    m[i][j] = cost
                    s[i][j] = k
                }
            }
        }
    }
}

fun printOptimalChainOrder(i: Int, j: Int) {
    if (i == j)
        print("${(i + 65).toChar()}")
    else {
        print("(")
        printOptimalChainOrder(i, s[i][j])
        printOptimalChainOrder(s[i][j] + 1, j)
        print(")")
    }
}

fun main(args: Array<String>) {
    val dimsList = listOf(
        intArrayOf(5, 6, 3, 1),
        intArrayOf(1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2),
        intArrayOf(1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10)
    )
    for (dims in dimsList) {
        println("Dims  : ${dims.asList()}")
        optimalMatrixChainOrder(dims)
        print("Order : ")
        printOptimalChainOrder(0, s.size - 1)
        println("\nCost  : ${m[0][s.size - 1]}\n")
    }
}
```


```txt

Dims  : [5, 6, 3, 1]
Order : (A(BC))
Cost  : 48

Dims  : [1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]
Order : ((((((((AB)C)D)E)F)G)(H(IJ)))(KL))
Cost  : 38120

Dims  : [1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]
Order : (A((((((BC)D)(((EF)G)H))I)J)K))
Cost  : 1773740

```


== {{header|MATLAB}} ==
```matlab
function [r,s] = optim(a)
    n = length(a)-1;
    u = zeros(n,n);
    v = ones(n,n)*inf;
    u(:,1) = -1;
    v(:,1) = 0;
    for j = 2:n
        for i = 1:n-j+1
            for k = 1:j-1
                c = v(i,k)+v(i+k,j-k)+a(i)*a(i+k)*a(i+j);
                if c<v(i,j)
                    u(i,j) = k;
                    v(i,j) = c;
                end
            end
        end
    end
    r = v(1,n);
    s = aux(u,1,n);
end

function s = aux(u,i,j)
    k = u(i,j);
    if k<0
        s = sprintf("%d",i);
    else
        s = sprintf("(%s*%s)",aux(u,i,k),aux(u,i+k,j-k));
    end
end
```


'''Output'''


```matlab
[r,s] = optim([1,5,25,30,100,70,2,1,100,250,1,1000,2])

r =

       38120


s =

    "((((((((1*2)*3)*4)*5)*6)*7)*(8*(9*10)))*(11*12))"


[r,s] = optim([1000,1,500,12,1,700,2500,3,2,5,14,10])

r =

     1773740


s =

    "(1*((((((2*3)*4)*(((5*6)*7)*8))*9)*10)*11))"
```



## Perl

```perl
use strict;
use feature 'say';

sub matrix_mult_chaining {
    my(@dimensions) = @_;
    my(@cp,@path);

    # a matrix never needs to be multiplied with itself, so it has cost 0
    $cp[$_][$_] = 0 for keys @dimensions;

    my $n = $#dimensions;
    for my $chain_length (1..$n) {
        for my $start (0 .. $n - $chain_length - 1) {
            my $end = $start + $chain_length;
            $cp[$end][$start] = 10e10;
            for my $step ($start .. $end - 1) {
                my $new_cost = $cp[$step][$start]
                             + $cp[$end][$step + 1]
                             + $dimensions[$start] * $dimensions[$step+1] * $dimensions[$end+1];
                if ($new_cost < $cp[$end][$start]) {
                    $cp[$end][$start] = $new_cost; # cost
                    $cp[$start][$end] = $step;     # path
                }
            }
       }
    }

    $cp[$n-1][0] . ' ' . find_path(0, $n-1, @cp);
}

sub find_path {
    my($start,$end,@cp) = @_;
    my $result;

    if ($start == $end) {
        $result .= 'A' . ($start + 1);
    } else {
        $result .= '(' .
                   find_path($start, $cp[$start][$end], @cp) .
                   find_path($cp[$start][$end] + 1, $end, @cp) .
                   ')';
    }
    return $result;
}

say matrix_mult_chaining(<1 5 25 30 100 70 2 1 100 250 1 1000 2>);
say matrix_mult_chaining(<1000 1 500 12 1 700 2500 3 2 5 14 10>);
```

```txt
38120 ((((((((A1A2)A3)A4)A5)A6)A7)(A8(A9A10)))(A11A12))
1773740 (A1((((((A2A3)A4)(((A5A6)A7)A8))A9)A10)A11))
```



## Perl 6

This example is based on Moritz Lenz's code, written for Carl Mäsak's Perl 6 Coding Contest, in 2010. Slightly simplified, it fulfills the Rosetta Code task as well.

```perl6
sub matrix-mult-chaining(@dimensions) {
    my @cp;
    # @cp has a dual function:
    # * the upper triangle of the diagonal matrix stores the cost (c) for
    #   multiplying matrices $i and $j in @cp[$j][$i], where $j > $i
    # * the lower triangle stores the path (p) that was used for the lowest cost
    #   multiplication to get from $i to $j.

    # a matrix never needs to be multiplied with itself, so it has cost 0
    @cp[$_][$_] = 0 for @dimensions.keys;
    my @path;

    my $n = @dimensions.end;
    for 1 .. $n -> $chain-length {
        for 0 .. $n - $chain-length - 1 -> $start {
            my $end = $start + $chain-length;
            @cp[$end][$start] = Inf;  # until we find a better connection
            for $start .. $end - 1 -> $step {
                my $new-cost = @cp[$step][$start]
                             + @cp[$end][$step + 1]
                             + [*] @dimensions[$start, $step+1, $end+1];
                if $new-cost < @cp[$end][$start] {
                    @cp[$end][$start] = $new-cost; # cost
                    @cp[$start][$end] = $step;     # path
                }
            }
       }
    }

    sub find-path(Int $start, Int $end) {
        if $start == $end {
            take 'A' ~ ($start + 1);
        } else {
            take '(';
            find-path($start, @cp[$start][$end]);
            find-path(@cp[$start][$end] + 1, $end);
            take ')';
        }
    }

   return @cp[$n-1][0], gather { find-path(0, $n - 1) }.join;
}

say matrix-mult-chaining(<1 5 25 30 100 70 2 1 100 250 1 1000 2>);
say matrix-mult-chaining(<1000 1 500 12 1 700 2500 3 2 5 14 10>);
```


```txt
(38120 ((((((((A1A2)A3)A4)A5)A6)A7)(A8(A9A10)))(A11A12)))
(1773740 (A1((((((A2A3)A4)(((A5A6)A7)A8))A9)A10)A11)))
```



## Phix

As per the wp pseudocode

```Phix
function optimal_chain_order(int i, int j, sequence s)
    if i==j then
        return i+'A'-1
    end if
    return "("&optimal_chain_order(i,s[i,j],s)
              &optimal_chain_order(s[i,j]+1,j,s)&")"
end function

function optimal_matrix_chain_order(sequence dims)
    integer n = length(dims)-1
    sequence {m,s} @= repeat(repeat(0,n),n)
    for len=2 to n do
        for i=1 to n-len+1 do
            integer j = i+len-1
            m[i][j] = -1
            for k=i to j-1 do
                atom cost := m[i][k] + m[k+1][j] + dims[i]*dims[k+1]*dims[j+1]
                if m[i][j]<0
                or cost<m[i][j] then
                    m[i][j] = cost;
                    s[i][j] = k;
                end if
            end for
        end for
    end for
    return {optimal_chain_order(1,n,s),m[1,n]}
end function

constant tests = {{5, 6, 3, 1},
                  {1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2},
                  {1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10}}
for i=1 to length(tests) do
    sequence ti = tests[i]
    printf(1,"Dims  : %s\n",{sprint(ti)})
    printf(1,"Order : %s\nCost  : %d\n",optimal_matrix_chain_order(ti))
end for
```

```txt

Dims  : {5,6,3,1}
Order : (A(BC))
Cost  : 48
Dims  : {1,5,25,30,100,70,2,1,100,250,1,1000,2}
Order : ((((((((AB)C)D)E)F)G)(H(IJ)))(KL))
Cost  : 38120
Dims  : {1000,1,500,12,1,700,2500,3,2,5,14,10}
Order : (A((((((BC)D)(((EF)G)H))I)J)K))
Cost  : 1773740

```


== {{header|Python}} ==
We will solve the task in three steps:

1) Enumerate all ways to parenthesize (using a generator to save space), and for each one compute the cost. Then simply look up the minimal cost.

2) Merge the enumeration and the cost function in a recursive cost optimizing function. The computation is roughly the same, but it's much faster as some steps are removed.

3) The recursive solution has many duplicates computations. Memoize the previous function: this yields a dynamic programming approach.


###  Enumeration of parenthesizations



```python
def parens(n):
    def aux(n, k):
        if n == 1:
            yield k
        elif n == 2:
            yield [k, k + 1]
        else:
            a = []
            for i in range(1, n):
                for u in aux(i, k):
                    for v in aux(n - i, k + i):
                        yield [u, v]
    yield from aux(n, 0)
```


'''Example''' (in the same order as in the task description)


```python
for u in parens(4):
    print(u)

[0, [1, [2, 3]]]
[0, [[1, 2], 3]]
[[0, 1], [2, 3]]
[[0, [1, 2]], 3]
[[[0, 1], 2], 3]
```


And here is the optimization step:


```python
def optim1(a):
    def cost(k):
        if type(k) is int:
            return 0, a[k], a[k + 1]
        else:
            s1, p1, q1 = cost(k[0])
            s2, p2, q2 = cost(k[1])
            assert q1 == p2
            return s1 + s2 + p1 * q1 * q2, p1, q2
    cmin = None
    n = len(a) - 1
    for u in parens(n):
        c, p, q = cost(u)
        if cmin is None or c < cmin:
            cmin = c
            umin = u
    return cmin, umin
```



###  Recursive cost optimization


The previous function optim1 already used recursion, but only to compute the cost of a given parens configuration, whereas another function (a generator actually) provides these configurations. Here we will do both recursively in the same function, avoiding the computation of configurations altogether.


```python
def optim2(a):
    def aux(n, k):
        if n == 1:
            p, q = a[k:k + 2]
            return 0, p, q, k
        elif n == 2:
            p, q, r = a[k:k + 3]
            return p * q * r, p, r, [k, k + 1]
        else:
            m = None
            p = a[k]
            q = a[k + n]
            for i in range(1, n):
                s1, p1, q1, u1 = aux(i, k)
                s2, p2, q2, u2 = aux(n - i, k + i)
                assert q1 == p2
                s = s1 + s2 + p1 * q1 * q2
                if m is None or s < m:
                    m = s
                    u = [u1, u2]
            return m, p, q, u
    s, p, q, u = aux(len(a) - 1, 0)
    return s, u
```



###  Memoized recursive call


The only difference between optim2 and optim3 is the [[:wp:https://en.wikipedia.org/wiki/Memoization|@memoize]] [https://www.python.org/dev/peps/pep-0318/ decorator]. Yet the algorithm is way faster with this. According to Wikipedia, the complexity falls from O(2^n) to O(n^3). This is confirmed by plotting log(time) vs log(n) for n up to 580 (this needs [https://docs.python.org/3/library/sys.html#sys.setrecursionlimit changing Python's recursion limit]).


```python
def memoize(f):
    h = {}
    def g(*u):
        if u in h:
            return h[u]
        else:
            r = f(*u)
            h[u] = r
            return r
    return g

def optim3(a):
    @memoize
    def aux(n, k):
        if n == 1:
            p, q = a[k:k + 2]
            return 0, p, q, k
        elif n == 2:
            p, q, r = a[k:k + 3]
            return p * q * r, p, r, [k, k + 1]
        else:
            m = None
            p = a[k]
            q = a[k + n]
            for i in range(1, n):
                s1, p1, q1, u1 = aux(i, k)
                s2, p2, q2, u2 = aux(n - i, k + i)
                assert q1 == p2
                s = s1 + s2 + p1 * q1 * q2
                if m is None or s < m:
                    m = s
                    u = [u1, u2]
            return m, p, q, u
    s, p, q, u = aux(len(a) - 1, 0)
    return s, u
```



###  Putting all together



```python
import time

u = [[1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2],
     [1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]]

for a in u:
    print(a)
    print()
    print("function     time       cost   parens  ")
    print("-" * 90)
    for f in [optim1, optim2, optim3]:
        t1 = time.clock()
        s, u = f(a)
        t2 = time.clock()
        print("%s %10.3f %10d   %s" % (f.__name__, 1000 * (t2 - t1), s, u))
    print()
```


'''Output''' (timings are in milliseconds)


```txt

[1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]

function     time       cost   parens
------------------------------------------------------------------------------------------
optim1    838.636      38120   [[[[[[[[0, 1], 2], 3], 4], 5], 6], [7, [8, 9]]], [10, 11]]
optim2     80.628      38120   [[[[[[[[0, 1], 2], 3], 4], 5], 6], [7, [8, 9]]], [10, 11]]
optim3      0.373      38120   [[[[[[[[0, 1], 2], 3], 4], 5], 6], [7, [8, 9]]], [10, 11]]

[1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]

function     time       cost   parens
------------------------------------------------------------------------------------------
optim1    223.186    1773740   [0, [[[[[[1, 2], 3], [[[4, 5], 6], 7]], 8], 9], 10]]
optim2     27.660    1773740   [0, [[[[[[1, 2], 3], [[[4, 5], 6], 7]], 8], 9], 10]]
optim3      0.307    1773740   [0, [[[[[[1, 2], 3], [[[4, 5], 6], 7]], 8], 9], 10]]

```


A mean on 1000 loops to get a better precision on the optim3, yields respectively 0.365 ms and 0.287 ms.


###  Iterative solution

In the previous solution, memoization is done blindly with a dictionary. However, we need to compute the optimal products for all sublists. A sublist is described by its first index and length (resp. i and j+1 in the following function), hence the set of all sublists can be described by the indices of elements in a triangular array u. We first fill the "solution" (there is no product) for sublists of length 1 (u[0]), then for each successive length we optimize using what when know about smaller sublists. Instead of keeping track of the optimal solutions, the single needed one is computed in the end.


```python
def optim4(a):
    global u
    n = len(a) - 1
    u = [None] * n
    u[0] = [[None, 0]] * n
    for j in range(1, n):
        v = [None] * (n - j)
        for i in range(n - j):
            m = None
            for k in range(j):
                s1, c1 = u[k][i]
                s2, c2 = u[j - k - 1][i + k + 1]
                c = c1 + c2 + a[i] * a[i + k + 1] * a[i + j + 1]
                if m is None or c < m:
                    s = k
                    m = c
            v[i] = [s, m]
        u[j] = v
    def aux(i, j):
        s, c = u[j][i]
        if s is None:
            return i
        else:
            return [aux(i, s), aux(i + s + 1, j - s - 1)]
    return u[n - 1][0][1], aux(0, n - 1)


print(optim4([1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]))
print(optim4([1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]))
```


'''Output'''


```txt

(38120, [[[[[[[[0, 1], 2], 3], 4], 5], 6], [7, [8, 9]]], [10, 11]])
(1773740, [0, [[[[[[1, 2], 3], [[[4, 5], 6], 7]], 8], 9], 10]])

```


== {{header|Racket}} ==

'''Memoization'''


```racket
#lang racket

(define (memoize f)
  (define table (make-hash))
  (λ args (hash-ref! table args (thunk (apply f args)))))

(struct $ (cost expl))
(define @ vector-ref)

(define (+: #:combine [combine (thunk* #f)] . xs)
  ($ (apply + (map $-cost xs)) (apply combine (map $-expl xs))))

(define (min: . xs) (argmin $-cost xs))

(define (compute dims)
  (define loop
    (memoize
     (λ (left right)
       (cond
         [(= 1 (- right left)) ($ 0 left)]
         [else (for/fold ([ans ($ +inf.0 #f)]) ([mid (in-range (add1 left) right)])
                 (min: ans (+: (loop left mid) (loop mid right)
                               ($ (* (@ dims left) (@ dims mid) (@ dims right)) #f)
                               #:combine (λ (left-answer right-answer _)
                                           (list left-answer '× right-answer)))))]))))
  (loop 0 (sub1 (vector-length dims))))
```


'''Main'''


```racket
(define-syntax-rule (echo <x> ...)
  (begin (printf "~a: ~a\n" (~a (quote <x>) #:min-width 12) <x>) ...))

(define (solve input)
  (match-define-values ((list ($ cost explanation)) _ time _) (time-apply compute (list input)))
  (echo input time cost explanation)
  (newline))

(solve #(1 5 25 30 100 70 2 1 100 250 1 1000 2))
(solve #(1000 1 500 12 1 700 2500 3 2 5 14 10))
```


'''Output''' (timings are in milliseconds)


```txt

input       : #(1 5 25 30 100 70 2 1 100 250 1 1000 2)
time        : 1
cost        : 38120
explanation : ((((((((0 × 1) × 2) × 3) × 4) × 5) × 6) × (7 × (8 × 9))) × (10 × 11))

input       : #(1000 1 500 12 1 700 2500 3 2 5 14 10)
time        : 0
cost        : 1773740
explanation : (0 × ((((((1 × 2) × 3) × (((4 × 5) × 6) × 7)) × 8) × 9) × 10))

```



## Rust


```rust
use std::collections::HashMap;

fn main() {
    println!("{}\n", mcm_display(vec![5, 6, 3, 1]));
    println!(
        "{}\n",
        mcm_display(vec![1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2])
    );
    println!(
        "{}\n",
        mcm_display(vec![1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10])
    );
}

fn mcm_display(dims: Vec<i32>) -> String {
    let mut costs: HashMap<Vec<i32>, (i32, Vec<usize>)> = HashMap::new();
    let mut line = format!("Dims : {:?}\n", dims);
    let ans = mcm(dims, &mut costs);
    let mut mats = (1..=ans.1.len() + 1)
        .map(|x| x.to_string())
        .collect::<Vec<String>>();
    for i in 0..ans.1.len() {
        let mat_taken = mats[ans.1[i]].clone();
        mats.remove(ans.1[i]);
        mats[ans.1[i]] = "(".to_string() + &mat_taken + "*" + &mats[ans.1[i]] + ")";
    }
    line += &format!("Order: {}\n", mats[0]);
    line += &format!("Cost : {}", ans.0);
    line
}

fn mcm(dims: Vec<i32>, costs: &mut HashMap<Vec<i32>, (i32, Vec<usize>)>) -> (i32, Vec<usize>) {
    match costs.get(&dims) {
        Some(c) => c.clone(),
        None => {
            let ans = if dims.len() == 3 {
                (dims[0] * dims[1] * dims[2], vec![0])
            } else {
                let mut min_cost = std::i32::MAX;
                let mut min_path = Vec::new();
                for i in 1..dims.len() - 1 {
                    let taken = dims[(i - 1)..(i + 2)].to_vec();
                    let mut rest = dims[..i].to_vec();
                    rest.extend_from_slice(&dims[(i + 1)..]);
                    let a1 = mcm(taken, costs);
                    let a2 = mcm(rest, costs);
                    if a1.0 + a2.0 < min_cost {
                        min_cost = a1.0 + a2.0;
                        min_path = vec![i - 1];
                        min_path.extend_from_slice(&a2.1);
                    }
                }
                (min_cost, min_path)
            };
            costs.insert(dims, ans.clone());
            ans
        }
    }
}
```

```txt

Dims : [5, 6, 3, 1]
Order: (1*(2*3))
Cost : 48

Dims : [1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2]
Order: ((((((((1*2)*3)*4)*5)*6)*7)*(8*(9*10)))*(11*12))
Cost : 38120

Dims : [1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10]
Order: (1*((((((2*3)*4)*(((5*6)*7)*8))*9)*10)*11))
Cost : 1773740

```


== {{header|Stata}} ==

###  Recursive solution

Here is the equivalent of optim3 in Python's solution. Memoization is done with an [https://www.stata.com/help.cgi?mf_asarray associative array]. Multiple results are returned in a [https://www.stata.com/help.cgi?m2_struct structure]. The same effect as optim2 can be achieved by removing the asarray machinery.


```stata
mata
struct ans {
	real scalar p,q,s
	string scalar u
}

struct ans scalar function aux(n,k) {
	external dim,opt
	struct ans scalar r,r1,r2
	real scalar s,i

	if (n==1) {
		r.p = dim[k]
		r.q = dim[k+1]
		r.s = 0
		r.u = strofreal(k)
		return(r)
	} else if (n==2) {
		r.p = dim[k]
		r.q = dim[k+2]
		r.s = r.p*r.q*dim[k+1]
		r.u = sprintf("(%f*%f)",k,k+1)
		return(r)
	} else if (asarray_contains(opt,(n,k))) {
		return(asarray(opt,(n,k)))
	} else {
		r.p = dim[k]
		r.q = dim[k+n]
		r.s = .
		for (i=1; i<n; i++) {
			r1 = aux(i,k)
			r2 = aux(n-i,k+i)
			s = r1.s+r2.s+r1.p*r1.q*r2.q
			if (s<r.s) {
				r.s = s
				r.u = sprintf("(%s*%s)",r1.u,r2.u)
			}
		}
		asarray(opt,(n,k),r)
		return(r)
	}
}

function optim(a) {
	external dim,opt
	struct ans scalar r
	real scalar t

	timer_clear()
	dim = a
	opt = asarray_create("real",2)
	timer_on(1)
	r = aux(length(a)-1,1)
	timer_off(1)
	t = timer_value(1)[1]
	printf("%10.0f %10.0f %s\n",t*1000,r.s,r.u)
}

optim((1,5,25,30,100,70,2,1,100,250,1,1000,2))
optim((1000,1,500,12,1,700,2500,3,2,5,14,10))
end
```


'''Output'''


```txt

         0      38120 ((((((((1*2)*3)*4)*5)*6)*7)*(8*(9*10)))*(11*12))
        16    1773740 (1*((((((2*3)*4)*(((5*6)*7)*8))*9)*10)*11))

```


The timing is in milliseconds, but the time resolution is too coarse to get a usable result. A mean on 1000 loops doing the same computation yields respectively 5.772 ms and 4.430 ms for these two cases. For comparison, the computation was made on the same machine as the Python solution.


###  Iterative solution

```stata
mata
function aux(u,i,j) {
	k = u[i,j]
	if (k<0) {
		printf("%f",i)
	} else {
		printf("(")
		aux(u,i,k)
		printf("*")
		aux(u,i+k,j-k)
		printf(")")
	}
}

function optim(a) {
	n = length(a)-1
	u = J(n,n,.)
	v = J(n,n,.)
	u[.,1] = J(n,1,-1)
	v[.,1] = J(n,1,0)
	for (j=2; j<=n; j++) {
		for (i=1; i<=n-j+1; i++) {
			for (k=1; k<j; k++) {
				c = v[i,k]+v[i+k,j-k]+a[i]*a[i+k]*a[i+j]
				if (c<v[i,j]) {
					u[i,j] = k
					v[i,j] = c
				}
			}
		}
	}
	printf("%f ",v[1,n])
	aux(u,1,n)
	printf("\n")
}

optim((1,5,25,30,100,70,2,1,100,250,1,1000,2))
optim((1000,1,500,12,1,700,2500,3,2,5,14,10))
end
```


'''Output'''


```txt

38120 ((((((((1*2)*3)*4)*5)*6)*7)*(8*(9*10)))*(11*12))
1773740 (1*((((((2*3)*4)*(((5*6)*7)*8))*9)*10)*11))

```


This solution is faster than the recursive one. The 1000 loops run now in 0.234 ms and 0.187 ms per loop on average.


## VBA

```vb
Option Explicit
Option Base 1
Dim N As Long, U() As Long, V() As Long
Sub Optimize(A As Variant)
    Dim I As Long, J As Long, K As Long, C As Long
    N = UBound(A) - 1
    ReDim U(N, N), V(N, N)
    For I = 1 To N
        U(I, 1) = -1
        V(I, 1) = 0
    Next I

    For J = 2 To N
        For I = 1 To N - J + 1
            V(I, J) = &H7FFFFFFF
            For K = 1 To J - 1
                C = V(I, K) + V(I + K, J - K) + A(I) * A(I + K) * A(I + J)
                If C < V(I, J) Then
                    U(I, J) = K
                    V(I, J) = C
                End If
            Next K
        Next I
    Next J

    Debug.Print V(1, N);
    Call Aux(1, N)
    Debug.Print
    Erase U, V
End Sub
Sub Aux(I As Long, J As Long)
    Dim K As Long
    K = U(I, J)
    If K < 0 Then
        Debug.Print CStr(I);
    Else
        Debug.Print "(";
        Call Aux(I, K)
        Debug.Print "*";
        Call Aux(I + K, J - K)
        Debug.Print ")";
    End If
End Sub
Sub Test()
    Call Optimize(Array(5, 6, 3, 1))
    Call Optimize(Array(1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2))
    Call Optimize(Array(1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10))
End Sub
```


'''Output'''


```txt

48 (1*(2*3))
38120 ((((((((1*2)*3)*4)*5)*6)*7)*(8*(9*10)))*(11*12))
1773740 (1*((((((2*3)*4)*(((5*6)*7)*8))*9)*10)*11))

```



## zkl

```zkl
fcn optim3(a){	     // list --> (int,list)
   aux:=fcn(n,k,a){  // (int,int,list) --> (int,int,int,list)
      if(n==1){
	 p,q := a[k,2];
	 return(0,p,q,k);
      }
      if(n==2){
	 p,q,r := a[k,3];
	 return(p*q*r, p, r, T(k,k+1));
      }
      m,p,q,u := Void, a[k], a[k + n], Void;
      foreach i in ([1..n-1]){
      #if 0	// 0.70 sec for both tests
	 s1,p1,q1,u1 := self.fcn(i,k,a);
	 s2,p2,q2,u2 := self.fcn(n - i, k + i, a);
      #else	// 0.33 sec for both tests
	 s1,p1,q1,u1 := memoize(self.fcn, i,k,a);
	 s2,p2,q2,u2 := memoize(self.fcn, n - i, k + i, a);
      #endif
	 _assert_(q1==p2);
	 s:=s1 + s2 + p1*q1*q2;
	 if((Void==m) or (s<m)) m,u = s,T(u1,u2);
      }
      return(m,p,q,u);
   };

   h=Dictionary();		// reset memoize
   s,_,_,u := aux(a.len() - 1, 0,a);
   return(s,u);
}

var h;		// a Dictionary, set/reset in optim3()
fcn memoize(f,n,k,a){
   key:="%d,%d".fmt(n,k);	// Lists make crappy keys
   if(r:=h.find(key)) return(r);
   r:=f(n,k,a);
   h[key]=r;
   return(r);
}
```


```zkl
fcn pp(u){	// pretty print a list of lists
   var letters=["A".."Z"].pump(String);
   u.pump(String,
      fcn(n){ if(List.isType(n)) String("(",pp(n),")") else letters[n] })
}
fcn prnt(s,u){ "%-9,d %s\n\t-->%s\n".fmt(s,u.toString(*,*),pp(u)).println() }
```


```zkl
s,u := optim3(T(1, 5, 25, 30, 100, 70, 2, 1, 100, 250, 1, 1000, 2));
prnt(s,u);

s,u := optim3(T(1000, 1, 500, 12, 1, 700, 2500, 3, 2, 5, 14, 10));
prnt(s,u);

optim3(T(5,6,3,1)) : prnt(_.xplode());
```

```txt

38,120    L(L(L(L(L(L(L(L(0,1),2),3),4),5),6),L(7,L(8,9))),L(10,11))
	-->(((((((AB)C)D)E)F)G)(H(IJ)))(KL)

1,773,740 L(0,L(L(L(L(L(L(1,2),3),L(L(L(4,5),6),7)),8),9),10))
	-->A((((((BC)D)(((EF)G)H))I)J)K)

48        L(0,L(1,2))
	-->A(BC)

```

