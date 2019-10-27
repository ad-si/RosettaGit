+++
title = "Latin Squares in reduced form"
description = ""
date = 2019-10-19T00:24:26Z
aliases = []
[extra]
id = 22424
[taxonomies]
categories = []
tags = []
+++

{{task}}
A Latin Square is in its reduced form if the first row and first column contain items in their natural order. The order n is the number of items. For any given n there is a set of reduced Latin Squares whose size increases rapidly with n. g is a number which identifies a unique element within the set of reduced Latin Squares of order n. The objective of this task is to construct the set of all Latin Squares of a given order and to provide a means which given suitable values for g any element within the set may be obtained.

For a reduced Latin Square the first row is always 1 to n. The second row is all [[Permutations/Derangements]] of 1 to n starting with 2. The third row is all [[Permutations/Derangements]] of 1 to n starting with 3 which do not clash (do not have the same item in any column) with row 2. The fourth row is all [[Permutations/Derangements]] of 1 to n starting with 4 which do not clash with rows 2 or 3. Likewise continuing to the nth row.

Demonstrate by:
* displaying the four reduced Latin Squares of order 4.
* for n = 1 to 6 (or more) produce the set of reduced Latin Squares; produce a table which shows the size of the set of reduced Latin Squares and compares this value times n! times (n-1)! with the values in [http://oeis.org/A002860 OEIS A002860].




=={{header|C#|C sharp}}==
{{trans|D}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace LatinSquares {
    using matrix = List<List<int>>;

    class Program {
        static void Swap<T>(ref T a, ref T b) {
            var t = a;
            a = b;
            b = t;
        }

        static matrix DList(int n, int start) {
            start--; // use 0 basing
            var a = Enumerable.Range(0, n).ToArray();
            a[start] = a[0];
            a[0] = start;
            Array.Sort(a, 1, a.Length - 1);
            var first = a[1];
            // recursive closure permutes a[1:]
            matrix r = new matrix();
            void recurse(int last) {
                if (last == first) {
                    // bottom of recursion. you get here once for each permutation.
                    // test if permutation is deranged.
                    for (int j = 1; j < a.Length; j++) {
                        var v = a[j];
                        if (j == v) {
                            return; //no, ignore it
                        }
                    }
                    // yes, save a copy with 1 based indexing
                    var b = a.Select(v => v + 1).ToArray();
                    r.Add(b.ToList());
                    return;
                }
                for (int i = last; i >= 1; i--) {
                    Swap(ref a[i], ref a[last]);
                    recurse(last - 1);
                    Swap(ref a[i], ref a[last]);
                }
            }
            recurse(n - 1);
            return r;
        }

        static ulong ReducedLatinSquares(int n, bool echo) {
            if (n <= 0) {
                if (echo) {
                    Console.WriteLine("[]\n");
                }
                return 0;
            } else if (n == 1) {
                if (echo) {
                    Console.WriteLine("[1]\n");
                }
                return 1;
            }

            matrix rlatin = new matrix();
            for (int i = 0; i < n; i++) {
                rlatin.Add(new List<int>());
                for (int j = 0; j < n; j++) {
                    rlatin[i].Add(0);
                }
            }
            // first row
            for (int j = 0; j < n; j++) {
                rlatin[0][j] = j + 1;
            }

            ulong count = 0;
            void recurse(int i) {
                var rows = DList(n, i);

                for (int r = 0; r < rows.Count; r++) {
                    rlatin[i - 1] = rows[r];
                    for (int k = 0; k < i - 1; k++) {
                        for (int j = 1; j < n; j++) {
                            if (rlatin[k][j] == rlatin[i - 1][j]) {
                                if (r < rows.Count - 1) {
                                    goto outer;
                                }
                                if (i > 2) {
                                    return;
                                }
                            }
                        }
                    }
                    if (i < n) {
                        recurse(i + 1);
                    } else {
                        count++;
                        if (echo) {
                            PrintSquare(rlatin, n);
                        }
                    }
                outer: { }
                }
            }

            //remaing rows
            recurse(2);
            return count;
        }

        static void PrintSquare(matrix latin, int n) {
            foreach (var row in latin) {
                var it = row.GetEnumerator();
                Console.Write("[");
                if (it.MoveNext()) {
                    Console.Write(it.Current);
                }
                while (it.MoveNext()) {
                    Console.Write(", {0}", it.Current);
                }
                Console.WriteLine("]");
            }
            Console.WriteLine();
        }

        static ulong Factorial(ulong n) {
            if (n <= 0) {
                return 1;
            }
            ulong prod = 1;
            for (ulong i = 2; i < n + 1; i++) {
                prod *= i;
            }
            return prod;
        }

        static void Main() {
            Console.WriteLine("The four reduced latin squares of order 4 are:\n");
            ReducedLatinSquares(4, true);

            Console.WriteLine("The size of the set of reduced latin squares for the following orders");
            Console.WriteLine("and hence the total number of latin squares of these orders are:\n");
            for (int n = 1; n < 7; n++) {
                ulong nu = (ulong)n;

                var size = ReducedLatinSquares(n, false);
                var f = Factorial(nu - 1);
                f *= f * nu * size;
                Console.WriteLine("Order {0}: Size {1} x {2}! x {3}! => Total {4}", n, size, n, n - 1, f);
            }
        }
    }
}
```

{{out}}

```txt
The four reduced latin squares of order 4 are:

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 1, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 2, 1]
[4, 3, 1, 2]

[1, 2, 3, 4]
[2, 4, 1, 3]
[3, 1, 4, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 3, 4, 1]
[3, 4, 1, 2]
[4, 1, 2, 3]

The size of the set of reduced latin squares for the following orders
and hence the total number of latin squares of these orders are:

Order 1: Size 1 x 1! x 0! => Total 1
Order 2: Size 1 x 2! x 1! => Total 2
Order 3: Size 1 x 3! x 2! => Total 12
Order 4: Size 4 x 4! x 3! => Total 576
Order 5: Size 56 x 5! x 4! => Total 161280
Order 6: Size 9408 x 6! x 5! => Total 812851200
```



## D

{{trans|Go}}

```d
import std.algorithm;
import std.array;
import std.range;
import std.stdio;

alias matrix = int[][];

auto dList(int n, int start) {
    start--;    // use 0 basing
    auto a = iota(0, n).array;
    a[start] = a[0];
    a[0] = start;
    sort(a[1..$]);
    auto first = a[1];
    // recursive closure permutes a[1:]
    matrix r;
    void recurse(int last) {
        if (last == first) {
            // bottom of recursion. you get here once for each permutation.
            // test if permutation is deranged.
            foreach (j,v; a[1..$]) {
                if (j + 1 == v) {
                    return; //no, ignore it
                }
            }
            // yes, save a copy with 1 based indexing
            auto b = a.map!"a+1".array;
            r ~= b;
            return;
        }
        for (int i = last; i >= 1; i--) {
            swap(a[i], a[last]);
            recurse(last -1);
            swap(a[i], a[last]);
        }
    }
    recurse(n - 1);
    return r;
}

ulong reducedLatinSquares(int n, bool echo) {
    if (n <= 0) {
        if (echo) {
            writeln("[]\n");
        }
        return 0;
    } else if (n == 1) {
        if (echo) {
            writeln("[1]\n");
        }
        return 1;
    }

    matrix rlatin = uninitializedArray!matrix(n);
    foreach (i; 0..n) {
        rlatin[i] = uninitializedArray!(int[])(n);
    }
    // first row
    foreach (j; 0..n) {
        rlatin[0][j] = j + 1;
    }

    ulong count;
    void recurse(int i) {
        auto rows = dList(n, i);

        outer:
        foreach (r; 0..rows.length) {
            rlatin[i-1] = rows[r].dup;
            foreach (k; 0..i-1) {
                foreach (j; 1..n) {
                    if (rlatin[k][j] == rlatin[i - 1][j]) {
                        if (r < rows.length - 1) {
                            continue outer;
                        }
                        if (i > 2) {
                            return;
                        }
                    }
                }
            }
            if (i < n) {
                recurse(i + 1);
            } else {
                count++;
                if (echo) {
                    printSquare(rlatin, n);
                }
            }
        }
    }

    // remaining rows
    recurse(2);
    return count;
}

void printSquare(matrix latin, int n) {
    foreach (row; latin) {
        writeln(row);
    }
    writeln;
}

ulong factorial(ulong n) {
    if (n == 0) {
        return 1;
    }
    ulong prod = 1;
    foreach (i; 2..n+1) {
        prod *= i;
    }
    return prod;
}

void main() {
    writeln("The four reduced latin squares of order 4 are:\n");
    reducedLatinSquares(4, true);

    writeln("The size of the set of reduced latin squares for the following orders");
    writeln("and hence the total number of latin squares of these orders are:\n");
    foreach (n; 1..7) {
        auto size = reducedLatinSquares(n, false);
        auto f = factorial(n - 1);
        f *= f * n * size;
        writefln("Order %d: Size %-4d x %d! x %d! => Total %d", n, size, n, n - 1, f);
    }
}
```

{{out}}

```txt
The four reduced latin squares of order 4 are:

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 1, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 2, 1]
[4, 3, 1, 2]

[1, 2, 3, 4]
[2, 4, 1, 3]
[3, 1, 4, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 3, 4, 1]
[3, 4, 1, 2]
[4, 1, 2, 3]

The size of the set of reduced latin squares for the following orders
and hence the total number of latin squares of these orders are:

Order 1: Size 1    x 1! x 0! => Total 1
Order 2: Size 1    x 2! x 1! => Total 2
Order 3: Size 1    x 3! x 2! => Total 12
Order 4: Size 4    x 4! x 3! => Total 576
Order 5: Size 56   x 5! x 4! => Total 161280
Order 6: Size 9408 x 6! x 5! => Total 812851200
```


=={{header|F_Sharp|F#}}==

### The Function

This task uses [[Permutations/Derangements#F.23]]

```fsharp

// Generate Latin Squares in reduced form. Nigel Galloway: July 10th., 2019
let normLS α=
  let N=derange α|>List.ofSeq|>List.groupBy(fun n->n.[0])|>List.sortBy(fun(n,_)->n)|>List.map(fun(_,n)->n)|>Array.ofList
  let rec fG n g=match n with h::t->fG t (g|>List.filter(fun g->Array.forall2((<>)) h g )) |_->g
  let rec normLS n g=seq{for i in fG n N.[g] do if g=α-2 then yield [|1..α|]::(List.rev (i::n)) else yield! normLS (i::n) (g+1)}
  match α with 1->seq[[[|1|]]] |2-> seq[[[|1;2|];[|2;1|]]] |_->Seq.collect(fun n->normLS [n] 1) N.[0]

```


### The Task


```fsharp

normLS 4 |> Seq.iter(fun n->List.iter(printfn "%A") n;printfn "");;

```

{{out}}

```txt

[|1; 2; 3; 4|]
[|2; 3; 4; 1|]
[|3; 4; 1; 2|]
[|4; 1; 2; 3|]

[|1; 2; 3; 4|]
[|2; 1; 4; 3|]
[|3; 4; 2; 1|]
[|4; 3; 1; 2|]

[|1; 2; 3; 4|]
[|2; 1; 4; 3|]
[|3; 4; 1; 2|]
[|4; 3; 2; 1|]

[|1; 2; 3; 4|]
[|2; 4; 1; 3|]
[|3; 1; 4; 2|]
[|4; 3; 2; 1|]

```


```fsharp

let rec fact n g=if n<2 then g else fact (n-1) n*g
[1..6] |> List.iter(fun n->let nLS=normLS n|>Seq.length in printfn "order=%d number of Reduced Latin Squares nLS=%d nLS*n!*(n-1)!=%d" n nLS (nLS*(fact n 1)*(fact (n-1) 1)))

```

{{out}}

```txt

order=1 number of Reduced Latin Squares nLS=1 nLS*n!*(n-1)!=1
order=2 number of Reduced Latin Squares nLS=1 nLS*n!*(n-1)!=2
order=3 number of Reduced Latin Squares nLS=1 nLS*n!*(n-1)!=12
order=4 number of Reduced Latin Squares nLS=4 nLS*n!*(n-1)!=576
order=5 number of Reduced Latin Squares nLS=56 nLS*n!*(n-1)!=161280
order=6 number of Reduced Latin Squares nLS=9408 nLS*n!*(n-1)!=812851200

```



## Go

This reuses the dList function from the [[Permutations/Derangements#Go]] task, suitably adjusted for the present one.

```go
package main

import (
    "fmt"
    "sort"
)

type matrix [][]int

// generate derangements of first n numbers, with 'start' in first place.
func dList(n, start int) (r matrix) {
    start-- // use 0 basing
    a := make([]int, n)
    for i := range a {
        a[i] = i
    }
    a[0], a[start] = start, a[0]
    sort.Ints(a[1:])
    first := a[1]
    // recursive closure permutes a[1:]
    var recurse func(last int)
    recurse = func(last int) {
        if last == first {
            // bottom of recursion.  you get here once for each permutation.
            // test if permutation is deranged.
            for j, v := range a[1:] { // j starts from 0, not 1
                if j+1 == v {
                    return // no, ignore it
                }
            }
            // yes, save a copy
            b := make([]int, n)
            copy(b, a)
            for i := range b {
                b[i]++ // change back to 1 basing
            }
            r = append(r, b)
            return
        }
        for i := last; i >= 1; i-- {
            a[i], a[last] = a[last], a[i]
            recurse(last - 1)
            a[i], a[last] = a[last], a[i]
        }
    }
    recurse(n - 1)
    return
}

func reducedLatinSquare(n int, echo bool) uint64 {
    if n <= 0 {
        if echo {
            fmt.Println("[]\n")
        }
        return 0
    } else if n == 1 {
        if echo {
            fmt.Println("[1]\n")
        }
        return 1
    }
    rlatin := make(matrix, n)
    for i := 0; i < n; i++ {
        rlatin[i] = make([]int, n)
    }
    // first row
    for j := 0; j < n; j++ {
        rlatin[0][j] = j + 1
    }

    count := uint64(0)
    // recursive closure to compute reduced latin squares and count or print them
    var recurse func(i int)
    recurse = func(i int) {
        rows := dList(n, i) // get derangements of first n numbers, with 'i' first.
    outer:
        for r := 0; r < len(rows); r++ {
            copy(rlatin[i-1], rows[r])
            for k := 0; k < i-1; k++ {
                for j := 1; j < n; j++ {
                    if rlatin[k][j] == rlatin[i-1][j] {
                        if r < len(rows)-1 {
                            continue outer
                        } else if i > 2 {
                            return
                        }
                    }
                }
            }
            if i < n {
                recurse(i + 1)
            } else {
                count++
                if echo {
                    printSquare(rlatin, n)
                }
            }
        }
        return
    }

    // remaining rows
    recurse(2)
    return count
}

func printSquare(latin matrix, n int) {
    for i := 0; i < n; i++ {
        fmt.Println(latin[i])
    }
    fmt.Println()
}

func factorial(n uint64) uint64 {
    if n == 0 {
        return 1
    }
    prod := uint64(1)
    for i := uint64(2); i <= n; i++ {
        prod *= i
    }
    return prod
}

func main() {
    fmt.Println("The four reduced latin squares of order 4 are:\n")
    reducedLatinSquare(4, true)

    fmt.Println("The size of the set of reduced latin squares for the following orders")
    fmt.Println("and hence the total number of latin squares of these orders are:\n")
    for n := uint64(1); n <= 6; n++ {
        size := reducedLatinSquare(int(n), false)
        f := factorial(n - 1)
        f *= f * n * size
        fmt.Printf("Order %d: Size %-4d x %d! x %d! => Total %d\n", n, size, n, n-1, f)
    }
}
```


{{out}}

```txt

The four reduced latin squares of order 4 are:

[1 2 3 4]
[2 1 4 3]
[3 4 1 2]
[4 3 2 1]

[1 2 3 4]
[2 1 4 3]
[3 4 2 1]
[4 3 1 2]

[1 2 3 4]
[2 4 1 3]
[3 1 4 2]
[4 3 2 1]

[1 2 3 4]
[2 3 4 1]
[3 4 1 2]
[4 1 2 3]

The size of the set of reduced latin squares for the following orders
and hence the total number of latin squares of these orders are:

Order 1: Size 1    x 1! x 0! => Total 1
Order 2: Size 1    x 2! x 1! => Total 2
Order 3: Size 1    x 3! x 2! => Total 12
Order 4: Size 4    x 4! x 3! => Total 576
Order 5: Size 56   x 5! x 4! => Total 161280
Order 6: Size 9408 x 6! x 5! => Total 812851200

```



## Julia


```julia
using Combinatorics

clash(row2, row1::Vector{Int}) = any(i -> row1[i] == row2[i], 1:length(row2))

clash(row, rows::Vector{Vector{Int}}) = any(r -> clash(row, r), rows)

permute_onefixed(i, n) = map(vec -> vcat(i, vec), permutations(filter(x -> x != i, 1:n)))

filter_permuted(rows, i, n) = filter(v -> !clash(v, rows), permute_onefixed(i, n))

function makereducedlatinsquares(n)
    matarray = [reshape(collect(1:n), 1, n)]
    for i in 2:n
        newmatarray = Vector{Matrix{Int}}()
        for mat in matarray
            r = size(mat)[1] + 1
            newrows = filter_permuted(collect(row[:] for row in eachrow(mat)), r, n)
            newmat = zeros(Int, r, n)
            newmat[1:r-1, :] .= mat
            append!(newmatarray, 
                [deepcopy(begin newmat[i, :] .= row; newmat end) for row in newrows])
        end
        matarray = newmatarray
    end
    matarray, length(matarray)
end

function testlatinsquares()
    squares, count = makereducedlatinsquares(4)
    println("The four reduced latin squares of order 4 are:")
    for sq in squares, (i, row) in enumerate(eachrow(sq)), j in 1:4
        print(row[j], j == 4 ? (i == 4 ? "\n\n" : "\n") : " ")
    end
    for i in 1:6
        squares, count = makereducedlatinsquares(i)
        println("Order $i: Size ", rpad(count, 5), "* $(i)! * $(i - 1)! = ", 
            count * factorial(i) * factorial(i - 1)) 
    end
end
    
testlatinsquares()

```
{{out}}

```txt

The four reduced latin squares of order 4 are:
1 2 3 4
2 1 4 3
3 4 1 2
4 3 2 1

1 2 3 4
2 1 4 3
3 4 2 1
4 3 1 2

1 2 3 4
2 3 4 1
3 4 1 2
4 1 2 3

1 2 3 4
2 4 1 3
3 1 4 2
4 3 2 1

Order 1: Size 1    * 1! * 0! = 1
Order 2: Size 1    * 2! * 1! = 2
Order 3: Size 1    * 3! * 2! = 12
Order 4: Size 4    * 4! * 3! = 576
Order 5: Size 56   * 5! * 4! = 161280
Order 6: Size 9408 * 6! * 5! = 812851200

```



## Kotlin

{{trans|D}}

```scala>typealias Matrix = MutableList<MutableList<Int>


fun dList(n: Int, sp: Int): Matrix {
    val start = sp - 1 // use 0 basing

    val a = generateSequence(0) { it + 1 }.take(n).toMutableList()
    a[start] = a[0].also { a[0] = a[start] }
    a.subList(1, a.size).sort()

    val first = a[1]
    // recursive closure permutes a[1:]
    val r = mutableListOf<MutableList<Int>>()
    fun recurse(last: Int) {
        if (last == first) {
            // bottom of recursion. you get here once for each permutation.
            // test if permutation is deranged
            for (jv in a.subList(1, a.size).withIndex()) {
                if (jv.index + 1 == jv.value) {
                    return  // no, ignore it
                }
            }
            // yes, save a copy with 1 based indexing
            val b = a.map { it + 1 }
            r.add(b.toMutableList())
            return
        }
        for (i in last.downTo(1)) {
            a[i] = a[last].also { a[last] = a[i] }
            recurse(last - 1)
            a[i] = a[last].also { a[last] = a[i] }
        }
    }
    recurse(n - 1)
    return r
}

fun reducedLatinSquares(n: Int, echo: Boolean): Long {
    if (n <= 0) {
        if (echo) {
            println("[]\n")
        }
        return 0
    } else if (n == 1) {
        if (echo) {
            println("[1]\n")
        }
        return 1
    }

    val rlatin = MutableList(n) { MutableList(n) { it } }
    // first row
    for (j in 0 until n) {
        rlatin[0][j] = j + 1
    }

    var count = 0L
    fun recurse(i: Int) {
        val rows = dList(n, i)

        outer@
        for (r in 0 until rows.size) {
            rlatin[i - 1] = rows[r].toMutableList()
            for (k in 0 until i - 1) {
                for (j in 1 until n) {
                    if (rlatin[k][j] == rlatin[i - 1][j]) {
                        if (r < rows.size - 1) {
                            continue@outer
                        }
                        if (i > 2) {
                            return
                        }
                    }
                }
            }
            if (i < n) {
                recurse(i + 1)
            } else {
                count++
                if (echo) {
                    printSquare(rlatin)
                }
            }
        }
    }

    // remaining rows
    recurse(2)
    return count
}

fun printSquare(latin: Matrix) {
    for (row in latin) {
        println(row)
    }
    println()
}

fun factorial(n: Long): Long {
    if (n == 0L) {
        return 1
    }
    var prod = 1L
    for (i in 2..n) {
        prod *= i
    }
    return prod
}

fun main() {
    println("The four reduced latin squares of order 4 are:\n")
    reducedLatinSquares(4, true)

    println("The size of the set of reduced latin squares for the following orders")
    println("and hence the total number of latin squares of these orders are:\n")
    for (n in 1 until 7) {
        val size = reducedLatinSquares(n, false)
        var f = factorial(n - 1.toLong())
        f *= f * n * size
        println("Order $n: Size %-4d x $n! x ${n - 1}! => Total $f".format(size))
    }
}
```

{{out}}

```txt
The four reduced latin squares of order 4 are:

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 1, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 2, 1]
[4, 3, 1, 2]

[1, 2, 3, 4]
[2, 4, 1, 3]
[3, 1, 4, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 3, 4, 1]
[3, 4, 1, 2]
[4, 1, 2, 3]

The size of the set of reduced latin squares for the following orders
and hence the total number of latin squares of these orders are:

Order 1: Size 1    x 1! x 0! => Total 1
Order 2: Size 1    x 2! x 1! => Total 2
Order 3: Size 1    x 3! x 2! => Total 12
Order 4: Size 4    x 4! x 3! => Total 576
Order 5: Size 56   x 5! x 4! => Total 161280
Order 6: Size 9408 x 6! x 5! => Total 812851200
```



## MiniZinc

===The Model (lsRF.mnz)===

```MiniZinc

%Latin Squares in Reduced Form. Nigel Galloway, September 5th., 2019
include "alldifferent.mzn";
int: N;
array[1..N,1..N] of var 1..N: p; constraint forall(n in 1..N)(p[1,n]=n /\ p[n,1]=n);
constraint forall(n in 1..N)(alldifferent([p[n,g]|g in 1..N])/\alldifferent([p[g,n]|g in 1..N]));

```


### The Tasks

;displaying the four reduced Latin Squares of order 4

```MiniZinc

include "lsRF.mzn";
 output  [show_int(1,p[i,j])++
          if j == 4 then
              if i != 4 then "\n"
              else "" endif
          else "" endif  
          | i,j in 1..4 ] ++ ["\n"];

```

When the above is run using minizinc --all-solutions -DN=4 the following is produced:
{{out}}

```txt

1234
2143
3421
4312
----------
1234
2143
3412
4321
----------
1234
2413
3142
4321
----------
1234
2341
3412
4123
----------

### ====


```

;counting the solutions
minizinc.exe --all-solutions -DN=5 -s lsRF.mzn produces the following:

```txt

.
.
.
p = array2d(1..5, 1..5, [1, 2, 3, 4, 5, 2, 3, 4, 5, 1, 3, 1, 5, 2, 4, 4, 5, 2, 1, 3, 5, 4, 1, 3, 2]);
----------
p = array2d(1..5, 1..5, [1, 2, 3, 4, 5, 2, 3, 5, 1, 4, 3, 5, 4, 2, 1, 4, 1, 2, 5, 3, 5, 4, 1, 3, 2]);
----------
p = array2d(1..5, 1..5, [1, 2, 3, 4, 5, 2, 3, 4, 5, 1, 3, 5, 2, 1, 4, 4, 1, 5, 2, 3, 5, 4, 1, 3, 2]);
----------

### ====

%%%mzn-stat: initTime=0.057
%%%mzn-stat: solveTime=0.003
%%%mzn-stat: solutions=56
%%%mzn-stat: variables=43
%%%mzn-stat: propagators=8
%%%mzn-stat: propagations=960
%%%mzn-stat: nodes=111
%%%mzn-stat: failures=0
%%%mzn-stat: restarts=0
%%%mzn-stat: peakDepth=7
%%%mzn-stat-end
%%%mzn-stat: nSolutions=56

```

and minizinc.exe --all-solutions -DN=6 -s lsRF.mzn produces the following:

```txt

.
.
.
p = array2d(1..6, 1..6, [1, 2, 3, 4, 5, 6, 2, 4, 5, 6, 3, 1, 3, 1, 4, 2, 6, 5, 4, 6, 2, 5, 1, 3, 5, 3, 6, 1, 2, 4, 6, 5, 1, 3, 4, 2]);
----------
p = array2d(1..6, 1..6, [1, 2, 3, 4, 5, 6, 2, 1, 4, 6, 3, 5, 3, 4, 5, 2, 6, 1, 4, 6, 2, 5, 1, 3, 5, 3, 6, 1, 2, 4, 6, 5, 1, 3, 4, 2]);
----------

### ====

%%%mzn-stat: initTime=0.003
%%%mzn-stat: solveTime=6.669
%%%mzn-stat: solutions=9408
%%%mzn-stat: variables=58
%%%mzn-stat: propagators=10
%%%mzn-stat: propagations=179635
%%%mzn-stat: nodes=19035
%%%mzn-stat: failures=110
%%%mzn-stat: restarts=0
%%%mzn-stat: peakDepth=17
%%%mzn-stat-end
%%%mzn-stat: nSolutions=9408

```

The only way to complete the tasks requirement to produce a table is with another language. Ruby has the ability to run an external program, capture the output, and text handling ability to format it to this tasks requirements. Othe scripting languages are available. 

## Phix

A Simple backtracking search.

aside: in phix here is no difference between res[r][c] and res[r,c]. I mixed them here, using whichever felt the more natural to me.

```Phix
string aleph = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
 
function rfls(integer n, bool count_only=true)
    if n>length(aleph) then ?9/0 end if -- too big...
    if n=1 then return iff(count_only?1:{{1}}) end if
    sequence tn = tagset(n),     -- {1..n}
             vcs = repeat(tn,n), -- valid for cols
             vrs = repeat(tn,n), -- valid for rows
             res = repeat(tn,n)  -- (main workspace/one element of result)
    object result = iff(count_only?0:{})
    vcs[1] = {}     -- (not strictly necessary)
    vrs[1] = {}     --          """
    for i=2 to n do
        res[i] = i & repeat(0,n-1)
        vrs[i][i] = 0
        vcs[i][i] = 0
    end for
    integer r = 2, c = 2
    while true do
        -- place with backtrack:
        -- if we successfully place [n,n] add to results and backtrack
        -- terminate when we fail to place or backtrack from [2,2]
        integer rrc = res[r,c]
        if rrc!=0 then  -- backtrack (/undo)
            if vrs[r][rrc]!=0 then ?9/0 end if  -- sanity check
            if vcs[c][rrc]!=0 then ?9/0 end if  --      ""
            res[r,c] = 0
            vrs[r][rrc] = rrc
            vcs[c][rrc] = rrc
        end if
        bool found = false
        for i=rrc+1 to n do
            if vrs[r][i] and vcs[c][i] then
                res[r,c] = i
                vrs[r][i] = 0
                vcs[c][i] = 0
                found = true
                exit
            end if
        end for
        if found then
            if r=n and c=n then
                if count_only then
                    result += 1 
                else
                    result = append(result,res)
                end if
                -- (here, backtracking == not advancing)
            elsif c=n then
                c = 2
                r += 1
            else
                c += 1  
            end if
        else
            -- backtrack
            if r=2 and c=2 then exit end if
            c -= 1
            if c=1 then
                r -= 1
                c = n
            end if
        end if
    end while
    return result
end function
 
procedure reduced_form_latin_squares(integer n)
    sequence res = rfls(n,false)
    for k=1 to length(res) do
        for i=1 to n do
            string line = ""
            for j=1 to n do
                line &= aleph[res[k][i][j]]
            end for
            res[k][i] = line
        end for
        res[k] = join(res[k],"\n")
    end for
    string r = join(res,"\n\n")
    printf(1,"There are %d reduced form latin squares of order %d:\n%s\n",{length(res),n,r})
end procedure

reduced_form_latin_squares(4)
puts(1,"\n")
for n=1 to 6 do
    integer size = rfls(n),
            f = factorial(n)*factorial(n-1)*size
    printf(1,"Order %d: Size %-4d x %d! x %d! => Total %d\n", {n, size, n, n-1, f})
end for
```

{{out}}

```txt

There are 4 reduced form latin squares of order 4:
1234
2143
3412
4321

1234
2143
3421
4312

1234
2341
3412
4123

1234
2413
3142
4321

Order 1: Size 1    x 1! x 0! => Total 1
Order 2: Size 1    x 2! x 1! => Total 2
Order 3: Size 1    x 3! x 2! => Total 12
Order 4: Size 4    x 4! x 3! => Total 576
Order 5: Size 56   x 5! x 4! => Total 161280
Order 6: Size 9408 x 6! x 5! => Total 812851200

```



## Python

{{trans|D}}

```python
def dList(n, start):
    start -= 1 # use 0 basing
    a = range(n)
    a[start] = a[0]
    a[0] = start
    a[1:] = sorted(a[1:])
    first = a[1]
    # rescursive closure permutes a[1:]
    r = []
    def recurse(last):
        if (last == first):
            # bottom of recursion. you get here once for each permutation.
            # test if permutation is deranged.
            # yes, save a copy with 1 based indexing
            for j,v in enumerate(a[1:]):
                if j + 1 == v:
                    return # no, ignore it
            b = [x + 1 for x in a]
            r.append(b)
            return
        for i in xrange(last, 0, -1):
            a[i], a[last] = a[last], a[i]
            recurse(last - 1)
            a[i], a[last] = a[last], a[i]
    recurse(n - 1)
    return r

def printSquare(latin,n):
    for row in latin:
        print row
    print

def reducedLatinSquares(n,echo):
    if n <= 0:
        if echo:
            print []
        return 0
    elif n == 1:
        if echo:
            print [1]
        return 1

    rlatin = [None] * n
    for i in xrange(n):
        rlatin[i] = [None] * n
    # first row
    for j in xrange(0, n):
        rlatin[0][j] = j + 1

    class OuterScope:
        count = 0
    def recurse(i):
        rows = dList(n, i)

        for r in xrange(len(rows)):
            rlatin[i - 1] = rows[r]
            justContinue = False
            k = 0
            while not justContinue and k < i - 1:
                for j in xrange(1, n):
                    if rlatin[k][j] == rlatin[i - 1][j]:
                        if r < len(rows) - 1:
                            justContinue = True
                            break
                        if i > 2:
                            return
                k += 1
            if not justContinue:
                if i < n:
                    recurse(i + 1)
                else:
                    OuterScope.count += 1
                    if echo:
                        printSquare(rlatin, n)

    # remaining rows
    recurse(2)
    return OuterScope.count

def factorial(n):
    if n == 0:
        return 1
    prod = 1
    for i in xrange(2, n + 1):
        prod *= i
    return prod

print "The four reduced latin squares of order 4 are:\n"
reducedLatinSquares(4,True)

print "The size of the set of reduced latin squares for the following orders"
print "and hence the total number of latin squares of these orders are:\n"
for n in xrange(1, 7):
    size = reducedLatinSquares(n, False)
    f = factorial(n - 1)
    f *= f * n * size
    print "Order %d: Size %-4d x %d! x %d! => Total %d" % (n, size, n, n - 1, f)
```

{{out}}

```txt
The four reduced latin squares of order 4 are:

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 1, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 1, 4, 3]
[3, 4, 2, 1]
[4, 3, 1, 2]

[1, 2, 3, 4]
[2, 4, 1, 3]
[3, 1, 4, 2]
[4, 3, 2, 1]

[1, 2, 3, 4]
[2, 3, 4, 1]
[3, 4, 1, 2]
[4, 1, 2, 3]

The size of the set of reduced latin squares for the following orders
and hence the total number of latin squares of these orders are:

Order 1: Size 1    x 1! x 0! => Total 1
Order 2: Size 1    x 2! x 1! => Total 2
Order 3: Size 1    x 3! x 2! => Total 12
Order 4: Size 4    x 4! x 3! => Total 576
Order 5: Size 56   x 5! x 4! => Total 161280
Order 6: Size 9408 x 6! x 5! => Total 812851200
```

