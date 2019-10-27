+++
title = "Random Latin Squares"
description = ""
date = 2019-07-18T17:50:08Z
aliases = []
[extra]
id = 22365
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

A Latin square of size <code>n</code> is an arrangement of <code>n</code> symbols  in an <code>n-by-n</code> square in such a way that each row and column has each symbol appearing exactly once.

A randomised Latin square generates random configurations of the symbols for any given <code>n</code>.

;Example n=4 randomised Latin square:

```txt
0 2 3 1
2 1 0 3
3 0 1 2
1 3 2 0
```


;Task:
# Create a function/routine/procedure/method/... that given <code>n</code> generates a randomised Latin square of size <code>n</code>.
# Use the function to generate ''and show here'', two randomly generated squares of size 5.

;Note: 
Strict ''Uniformity'' in the random generation is a hard problem and '''not''' a requirement of the task.

;Reference:
* [[wp:Latin_square|Latin square]]
* [http://oeis.org/A002860 OEIS A002860]



=={{header|F_Sharp|F#}}==
This solution uses functions from [[Factorial_base_numbers_indexing_permutations_of_a_collection#F.23]] and [[Latin_Squares_in_reduced_form#F.23]]. This solution generates completely random uniformly distributed Latin Squares from all possible Latin Squares of order 5. It takes 5 thousandths of a second can that really be called hard?

```fsharp

// Generate 2 Random Latin Squares of order 5. Nigel Galloway: July 136th., 2019
let N=let N=System.Random() in (fun n->N.Next(n))
let rc()=let β=lN2p [|0;N 4;N 3;N 2|] [|0..4|] in Seq.item (N 56) (normLS 5) |> List.map(lN2p [|N 5;N 4;N 3;N 2|]) |> List.permute(fun n->β.[n]) |> List.iter(printfn "%A")
rc(); printfn ""; rc()

```

{{out}}

```txt

[|5; 3; 1; 4; 2|]
[|1; 4; 5; 2; 3|]
[|4; 1; 2; 3; 5|]
[|2; 5; 3; 1; 4|]
[|3; 2; 4; 5; 1|]

[|4; 1; 2; 5; 3|]
[|3; 5; 1; 2; 4|]
[|2; 4; 5; 3; 1|]
[|1; 2; 3; 4; 5|]
[|5; 3; 4; 1; 2|]

```

I thought some statistics might be interesting so I generated 1 million Latin Squares of order 5. There are 161280 possible Latin Squares of which 3174 were not generated. The remainder were generated:

```txt

Times Generated    Number of Latin Squares
     1                       1776
     2                       5669
     3                      11985
     4                      19128
     5                      24005
     6                      25333
     7                      22471
     8                      18267
     9                      12569
    10                       7924
    11                       4551
    12                       2452
    13                       1130
    14                        483
    15                        219
    16                         93
    17                         37
    18                          5
    19                          7
    20                          2

```



## Factor

A brute force method for generating uniformly random Latin squares. Repeatedly select a random permutation of (0, 1,...n-1) and add it as the next row of the square. If at any point the rules for being a Latin square are violated, start the entire process over again from the beginning.

```factor
USING: arrays combinators.extras fry io kernel math.matrices
prettyprint random sequences sets ;
IN: rosetta-code.random-latin-squares

: rand-permutation ( n -- seq ) <iota> >array randomize ;
: ls? ( n -- ? ) [ all-unique? ] column-map t [ and ] reduce ;
: (ls) ( n -- m ) dup '[ _ rand-permutation ] replicate ;
: ls ( n -- m ) dup (ls) dup ls? [ nip ] [ drop ls ] if ;
: random-latin-squares ( -- ) [ 5 ls simple-table. nl ] twice ;

MAIN: random-latin-squares
```

{{out}}

```txt

0 4 3 2 1
3 0 2 1 4
4 2 1 3 0
2 1 4 0 3
1 3 0 4 2

4 0 1 3 2
0 2 4 1 3
1 3 0 2 4
2 4 3 0 1
3 1 2 4 0

```



## Go


### Restarting Row method

As the task is not asking for large squares to be generated and even n = 10 is virtually instant, we use a simple brute force approach here known as the 'Restarting Row' method (see Talk page). However, whilst easy to understand, this method does not produce uniformly random squares.

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

type matrix [][]int

func shuffle(row []int, n int) {
    rand.Shuffle(n, func(i, j int) {
        row[i], row[j] = row[j], row[i]
    })
}

func latinSquare(n int) {
    if n <= 0 {
        fmt.Println("[]\n")
        return
    }
    latin := make(matrix, n)
    for i := 0; i < n; i++ {
        latin[i] = make([]int, n)
        if i == n-1 {
            break
        }
        for j := 0; j < n; j++ {
            latin[i][j] = j
        }
    }
    // first row
    shuffle(latin[0], n)

    // middle row(s)
    for i := 1; i < n-1; i++ {
        shuffled := false
    shuffling:
        for !shuffled {
            shuffle(latin[i], n)
            for k := 0; k < i; k++ {
                for j := 0; j < n; j++ {
                    if latin[k][j] == latin[i][j] {
                        continue shuffling
                    }
                }
            }
            shuffled = true
        }
    }

    // last row
    for j := 0; j < n; j++ {
        used := make([]bool, n)
        for i := 0; i < n-1; i++ {
            used[latin[i][j]] = true
        }
        for k := 0; k < n; k++ {
            if !used[k] {
                latin[n-1][j] = k
                break
            }
        }
    }
    printSquare(latin, n)
}

func printSquare(latin matrix, n int) {
    for i := 0; i < n; i++ {
        fmt.Println(latin[i])
    }
    fmt.Println()
}

func main() {
    rand.Seed(time.Now().UnixNano())
    latinSquare(5)
    latinSquare(5)
    latinSquare(10) // for good measure
}
```


{{out}}
Sample run:

```txt

[3 2 1 0 4]
[0 3 2 4 1]
[4 1 0 3 2]
[2 4 3 1 0]
[1 0 4 2 3]

[3 1 0 4 2]
[1 0 2 3 4]
[2 4 3 0 1]
[4 3 1 2 0]
[0 2 4 1 3]

[9 2 8 4 6 1 7 5 0 3]
[4 3 7 6 0 8 5 9 2 1]
[2 1 9 7 3 4 6 0 5 8]
[8 6 0 5 7 2 3 1 9 4]
[5 0 6 8 1 3 9 2 4 7]
[7 5 4 9 2 0 1 3 8 6]
[3 9 2 1 5 6 8 4 7 0]
[1 4 5 2 8 7 0 6 3 9]
[6 8 3 0 4 9 2 7 1 5]
[0 7 1 3 9 5 4 8 6 2]

```



### Latin Squares in Reduced Form method

Unlike the "Restarting Row" method, this method does produce uniformly random Latin squares for n <= 6 (see Talk page) but is more involved and therefore slower. It reuses some (suitably adjusted) code from the [https://rosettacode.org/wiki/Latin_Squares_in_reduced_form#Go Latin Squares in Reduced Form] and [https://rosettacode.org/wiki/Permutations#non-recursive.2C_lexicographical_order Permutations] tasks. 

```go
package main

import (
    "fmt"
    "math/rand"
    "sort"
    "time"
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

func reducedLatinSquares(n int) []matrix {
    var rls []matrix
    if n < 0 {
        n = 0
    }
    rlatin := make(matrix, n)
    for i := 0; i < n; i++ {
        rlatin[i] = make([]int, n)
    }
    if n <= 1 {
        return append(rls, rlatin)
    }
    // first row
    for j := 0; j < n; j++ {
        rlatin[0][j] = j + 1
    }
    // recursive closure to compute reduced latin squares
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
                rl := copyMatrix(rlatin)
                rls = append(rls, rl)
            }
        }
        return
    }

    // remaining rows
    recurse(2)
    return rls
}

func copyMatrix(m matrix) matrix {
    le := len(m)
    cpy := make(matrix, le)
    for i := 0; i < le; i++ {
        cpy[i] = make([]int, le)
        copy(cpy[i], m[i])
    }
    return cpy
}

func printSquare(latin matrix, n int) {
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            fmt.Printf("%d ", latin[i][j]-1)
        }
        fmt.Println()
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

// generate permutations of first n numbers, starting from 0.
func pList(n int) matrix {
    fact := factorial(uint64(n))
    perms := make(matrix, fact)
    a := make([]int, n)
    for i := 0; i < n; i++ {
        a[i] = i
    }
    t := make([]int, n)
    copy(t, a)
    perms[0] = t
    n--
    var i, j int
    for c := uint64(1); c < fact; c++ {
        i = n - 1
        j = n
        for a[i] > a[i+1] {
            i--
        }
        for a[j] < a[i] {
            j--
        }
        a[i], a[j] = a[j], a[i]
        j = n
        i++
        for i < j {
            a[i], a[j] = a[j], a[i]
            i++
            j--
        }
        t := make([]int, n+1)
        copy(t, a)
        perms[c] = t
    }
    return perms
}

func generateLatinSquares(n, tests, echo int) {   
    rls := reducedLatinSquares(n)
    perms := pList(n)
    perms2 := pList(n - 1)
    for test := 0; test < tests; test++ {
        rn := rand.Intn(len(rls))
        rl := rls[rn] // select reduced random square at random
        rn = rand.Intn(len(perms))
        rp := perms[rn] // select a random permuation of 'rl's columns
        // permute columns
        t := make(matrix, n)
        for i := 0; i < n; i++ {
            t[i] = make([]int, n)
        }
        for i := 0; i < n; i++ {
            for j := 0; j < n; j++ {
                t[i][j] = rl[i][rp[j]]
            }
        }
        rn = rand.Intn(len(perms2))
        rp = perms2[rn] // select a random permutation of 't's rows 2 to n
        // permute rows 2 to n
        u := make(matrix, n)
        for i := 0; i < n; i++ {
            u[i] = make([]int, n)
        }
        for i := 0; i < n; i++ {
            for j := 0; j < n; j++ {
                if i == 0 {
                    u[i][j] = t[i][j]
                } else {
                    u[i][j] = t[rp[i-1]+1][j]
                }
            }
        }
        if test < echo {
            printSquare(u, n)
        }
        if n == 4 {
            for i := 0; i < 4; i++ {
                for j := 0; j < 4; j++ {
                    u[i][j]--
                }
            }
            for i := 0; i < 4; i++ {
                copy(a[4*i:], u[i])
            }
            for i := 0; i < 4; i++ {
                if testSquares[i] == a {
                    counts[i]++
                    break
                }
            }
        }
    }
}

var testSquares = [4][16]int{
    {0, 1, 2, 3, 1, 0, 3, 2, 2, 3, 0, 1, 3, 2, 1, 0},
    {0, 1, 2, 3, 1, 0, 3, 2, 2, 3, 1, 0, 3, 2, 0, 1},
    {0, 1, 2, 3, 1, 2, 3, 0, 2, 3, 0, 1, 3, 0, 1, 2},
    {0, 1, 2, 3, 1, 3, 0, 2, 2, 0, 3, 1, 3, 2, 1, 0},
}

var (
    counts [4]int
    a      [16]int
)

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println("Two randomly generated latin squares of order 5 are:\n")
    generateLatinSquares(5, 2, 2)

    fmt.Println("Out of 1,000,000 randomly generated latin squares of order 4, ")
    fmt.Println("of which there are 576 instances ( => expected 1736 per instance),")
    fmt.Println("the following squares occurred the number of times shown:\n")
    generateLatinSquares(4, 1e6, 0)
    for i := 0; i < 4; i++ {
        fmt.Println(testSquares[i][:], ":", counts[i])
    }

    fmt.Println("\nA randomly generated latin square of order 6 is:\n")
    generateLatinSquares(6, 1, 1)
}
```


{{out}}
Sample run:

```txt

Two randomly generated latin squares of order 5 are:

2 1 3 4 0 
4 3 0 1 2 
1 0 2 3 4 
0 4 1 2 3 
3 2 4 0 1 

1 2 3 4 0 
0 3 4 2 1 
2 4 0 1 3 
4 0 1 3 2 
3 1 2 0 4 

Out of 1,000,000 randomly generated latin squares of order 4, 
of which there are 576 instances ( => expected 1736 per instance),
the following squares occurred the number of times shown:

[0 1 2 3 1 0 3 2 2 3 0 1 3 2 1 0] : 1737
[0 1 2 3 1 0 3 2 2 3 1 0 3 2 0 1] : 1736
[0 1 2 3 1 2 3 0 2 3 0 1 3 0 1 2] : 1726
[0 1 2 3 1 3 0 2 2 0 3 1 3 2 1 0] : 1799

A randomly generated latin square of order 6 is:

3 5 1 0 4 2 
2 0 5 4 1 3 
0 4 2 5 3 1 
1 3 4 2 0 5 
5 1 0 3 2 4 
4 2 3 1 5 0 

```



## Javascript


```javascript

class Latin {
  constructor(size = 3) {
    this.size = size;
    this.mst = [...Array(this.size)].map((v, i) => i + 1);
    this.square = Array(this.size).fill(0).map(() => Array(this.size).fill(0));

    if (this.create(0, 0)) {
      console.table(this.square);
    }
  }

  create(c, r) {
    const d = [...this.mst];
    let s;
    while (true) {
      do {
        s = d.splice(Math.floor(Math.random() * d.length), 1)[0];
        if (!s) return false;
      } while (this.check(s, c, r));

      this.square[c][r] = s;
      if (++c >= this.size) {
        c = 0;
        if (++r >= this.size) {
          return true;
        }
      }
      if (this.create(c, r)) return true;
      if (--c < 0) {
        c = this.size - 1;
        if (--r < 0) {
          return false;
        }
      }
    }
  }

  check(d, c, r) {
    for (let a = 0; a < this.size; a++) {
      if (c - a > -1) {
        if (this.square[c - a][r] === d)
          return true;
      }
      if (r - a > -1) {
        if (this.square[c][r - a] === d)
          return true;
      }
    }
    return false;
  }
}
new Latin(5);

```

{{out}}

```txt

3 5 4 1 2
4 3 1 2 5
1 2 3 5 4
5 1 2 4 3
2 4 5 3 1
 
4 5 1 3 2
3 1 4 2 5
5 4 2 1 3
1 2 3 5 4
2 3 5 4 1

```




## Julia

Using the Python algorithm as described in the discussion section.

```julia
using Random

shufflerows(mat) = mat[shuffle(1:end), :]
shufflecols(mat) = mat[:, shuffle(1:end)]

function addatdiagonal(mat)
    n = size(mat)[1] + 1
    newmat = similar(mat, size(mat) .+ 1)
    for j in 1:n, i in 1:n
        newmat[i, j] = (i == n && j < n) ? mat[1, j] : (i == j) ? n - 1 :
            (i < j) ? mat[i, j - 1] : mat[i, j]
    end
    newmat
end

function makelatinsquare(N)
    mat = [0 1; 1 0]
    for i in 3:N
        mat = addatdiagonal(mat)
    end
    shufflecols(shufflerows(mat))
end

function printlatinsquare(N)
    mat = makelatinsquare(N)
    for i in 1:N, j in 1:N
        print(rpad(mat[i, j], 3), j == N ? "\n" : "")
    end
end

printlatinsquare(5), println("\n"), printlatinsquare(5)

```
{{out}}

```txt

1  3  0  4  2
3  0  4  2  1
0  4  2  1  3
2  1  3  0  4
4  2  1  3  0


2  0  1  3  4
4  3  2  1  0
3  2  0  4  1
1  4  3  0  2
0  1  4  2  3

```


## M2000 Interpreter


### Easy Way

One row shuffled to be used as the destination row. One more shuffled and then n times rotated by one and stored to array

for 40x40  need 2~3 sec, including displaying to screen

We use the stack of values, a linked list, for pushing to top (Push) or to bottom (Data), and we can pop from top using Number or by using Read A to read A from stack. Also we can shift from a chosen position to top using Shift, or using shiftback to move an item from top to chosen position. So we shuffle items by shifting them.


```M2000 Interpreter

Module FastLatinSquare {
	n=5
	For k=1 To 2
		latin()
	Next
	n=40
	latin()
	Sub latin()
		Local i,a, a(1 To n), b, k
		Profiler
		flush
		Print "latin square ";n;" by ";n
		For i=1 To n
			Push i
		Next i
		For i=1 To n div 2
			Shiftback random(2, n)
		Next i
		a=[]
		Push ! stack(a)
		a=array(a)  ' change a from stack to array
		For i=1 To n*10
			Shiftback random(2, n)
		Next i
		For i=0 To n-1
			Data number  ' rotate by one the stack items
			b=[]    ' move stack To b, leave empty stack
			a(a#val(i))=b
			Push ! stack(b)  ' Push from a copy of b all items To stack
		Next i
		flush
		For k=1 To  n div 2
			z=random(2, n)
			For i=1 To n
				a=a(i)
				stack a {
					shift z
				}
			Next
		Next
		For i=1 To n
			a=a(i)
			a(i)=array(a)  ' change To array from stack
		Next i
		For i=1 To n
			Print a(i)
		Next i
		Print TimeCount
	End Sub
}
FastLatinSquare
```



### Hard Way

for 5x5 need some miliseconds

for 16X16 need 56 seconds

for 20X20 need 22 min (as for 9.8 version)


```M2000 Interpreter

Module LatinSquare (n, z=1, f$="latin.dat", NewFile As Boolean=False) {
	If Not Exist(f$)  Or NewFile Then
		Open f$ For Wide Output As f
	Else
		Open f$ For Wide Append As f
	End If
	ArrayToString=Lambda -> {
		Shift 2  ' swap two top values in stack
		Push Letter$+Str$(Number)
	}
	Dim line(1 to n)
	flush   ' erase current stack of value
	z=if(z<1->1, z)
	newColumn()
	For j=1 To z
		Profiler
		ResetColumns()
		For i=1 To n
			placeColumn()
		Next
		Print "A latin square of ";n;" by ";n
		For i=1 To n
			Print line(i)
			Print #f, line(i)#Fold$(ArrayToString)
		Next 
		Print TimeCount
		Refresh
	Next 
	close #f
	Flush  ' empty stack again
	End
	Sub ResetColumns()
		Local i
		For i=1 To n:line(i)=(,):Next
	End Sub
	Sub newColumn()
		Local i
		For i=1 To n : Push i: Next
	End Sub
	Sub shuffle()
		Local i
		For i=1 To n div 2: Shift Random(2, n): Next
	End Sub
	Sub shuffleLocal(x)
		If Stack.size<=x Then Exit Sub
		Shift Random(x+1, Stack.size)
		Shiftback x
	End Sub
	Sub PlaceColumn()
		Local i, a, b, k
		shuffle()
		Do
			data number   ' rotate one position
			k=0
			For i=1 To n
				a=line(i)  ' get the pointer
				Do
				If a#Pos(Stackitem(i))=-1 Then k=0 :Exit Do
				shuffleLocal(i)
				k++
				Until k>Stack.size-i
				If k>0 Then Exit For
			Next
		Until k=0
		For i=1 To n
			a=line(i) 
			Append a, (Stackitem(i),)
		Next
	End Sub
}
Form 100,50
LatinSquare 5, 2, True
LatinSquare 16


```

{{out}}
<pre style="height:30ex;overflow:scroll">
A latin square of 5 by 5
 4 5 3 1 2
 5 4 2 3 1
 2 1 5 4 3
 1 3 4 2 5
 3 2 1 5 4
A latin square of 5 by 5
 4 3 5 1 2
 2 4 3 5 1
 1 2 4 3 5
 5 1 2 4 3
 3 5 1 2 4
A latin square of 16 by 16
12 14 5 16 1 2 7 15 9 11 10 8 13 3 6 4
 3 13 16 12 7 4 1 11 5 6 15 2 8 14 10 9
 13 2 8 3 4 12 5 9 14 7 16 10 6 1 15 11
 8 3 13 9 2 10 16 1 15 14 5 4 11 7 12 6
 4 12 2 7 5 3 6 10 1 9 11 16 14 8 13 15
 16 8 3 4 14 6 13 7 11 10 9 15 1 12 2 5
 15 4 14 1 16 8 2 13 6 12 7 9 10 11 5 3
 11 16 12 10 15 9 4 5 7 1 8 6 3 13 14 2
 10 15 4 5 12 16 3 6 8 13 1 11 7 2 9 14
 9 11 15 8 3 1 14 12 13 4 6 5 2 16 7 10
 7 10 11 13 9 14 15 4 3 5 2 12 16 6 1 8
 6 7 10 2 8 13 9 16 12 15 14 3 5 4 11 1
 5 6 1 14 13 11 8 2 10 3 12 7 15 9 4 16
 2 5 6 15 11 7 12 14 4 8 3 1 9 10 16 13
 1 9 7 11 6 15 10 8 2 16 13 14 4 5 3 12
 14 1 9 6 10 5 11 3 16 2 4 13 12 15 8 7
</pre >


## Perl 6

{{works with|Rakudo|2019.03}}
{{trans|Python}}


```perl6
sub latin-square { [[0],] };

sub random ( @ls, :$size = 5 ) {

    # Build
    for 1 ..^ $size -> $i {
        @ls[$i] = @ls[0].clone;
        @ls[$_].splice($_, 0, $i) for 0 .. $i;
    }

    # Shuffle
    @ls = @ls[^$size .pick(*)];
    my @cols = ^$size .pick(*);
    @ls[$_] = @ls[$_][@cols] for ^@ls;

    # Some random Latin glyphs
    my @symbols = ('A' .. 'Z').pick($size);

    @ls.deepmap: { $_ = @symbols[$_] };

}

sub display ( @array ) { $_.fmt("%2s ").put for |@array, '' }


# The Task

# Default size 5
display random latin-square;

# Specified size
display random :size($_), latin-square for 5, 3, 9;

# Or, if you'd prefer:
display random latin-square, :size($_) for 12, 2, 1;
```

{{out|Sample output}}

```txt
 V   Z   M   J   U 
 Z   M   U   V   J 
 U   J   V   M   Z 
 J   V   Z   U   M 
 M   U   J   Z   V 
   
 B   H   K   U   D 
 H   D   U   B   K 
 K   U   H   D   B 
 U   B   D   K   H 
 D   K   B   H   U 
   
 I   P   Y 
 P   Y   I 
 Y   I   P 
   
 Y   J   K   E   Z   B   I   W   H 
 E   Y   B   W   K   H   J   Z   I 
 B   K   Y   H   J   E   Z   I   W 
 I   H   W   J   E   Z   B   Y   K 
 J   I   Z   Y   W   K   H   E   B 
 W   E   H   Z   B   I   Y   K   J 
 H   B   E   I   Y   W   K   J   Z 
 K   Z   J   B   I   Y   W   H   E 
 Z   W   I   K   H   J   E   B   Y 
   
 L   Q   E   M   A   T   Z   C   N   Y   R   D 
 Q   R   Y   L   N   D   C   E   M   T   A   Z 
 E   Y   M   C   D   Q   A   N   Z   L   T   R 
 M   L   C   N   R   Y   D   Z   A   E   Q   T 
 N   M   Z   A   Q   E   T   D   R   C   L   Y 
 T   D   Q   Y   C   A   M   L   E   R   Z   N 
 R   A   T   Q   M   Z   E   Y   L   D   N   C 
 D   Z   R   T   E   N   L   Q   Y   A   C   M 
 Y   T   L   E   Z   R   N   M   C   Q   D   A 
 A   N   D   R   L   C   Y   T   Q   Z   M   E 
 Z   C   A   D   Y   M   Q   R   T   N   E   L 
 C   E   N   Z   T   L   R   A   D   M   Y   Q 
   
 Y   G 
 G   Y 
   
 I
```



## Phix

Brute force, begins to struggle above 42.

```Phix
string aleph = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

function ls(integer n)
    if n>length(aleph) then ?9/0 end if -- too big...
    atom t1 = time()+1
    sequence tn = tagset(n),     -- {1..n}
             vcs = repeat(tn,n), -- valid for cols
             res = {}
    integer clashes = 0
    while length(res)<n do
        sequence rn = {},       -- next row
                 vr = tn,       -- valid for row (ie all)
                 vc = vcs       -- copy (in case of clash)
        bool clash = false
        for c=1 to n do
            sequence v = {}
            for k=1 to n do
                -- collect all still valid options
                if vr[k] and vc[c][k] then v &= k end if
            end for
            if v={} then
                clash = true
                exit
            end if
            integer z = v[rand(length(v))]
            rn &= z
            vr[z] = 0           -- no longer valid
            vc[c][z] = 0        --      ""
        end for
        if not clash then
            res = append(res,rn)
            vcs = vc
        else
            clashes += 1
            if time()>t1 then
                printf(1,"rows completed:%d/%d, clashes:%d\n",
                         {length(res),n,clashes})
                t1 = time()+1
            end if
        end if
    end while
    for i=1 to n do
        string line = ""
        for j=1 to n do
            line &= aleph[res[i][j]]
        end for
        res[i] = line
    end for
    return res
end function

procedure latin_square(integer n)
    atom t0 = time()
    string res = join(ls(n),"\n"),
           e = elapsed(time()-t0)
    printf(1,"Latin square of order %d (%s):\n%s\n",{n,e,res})
end procedure
latin_square(3)
latin_square(5)
latin_square(5)
latin_square(10)
latin_square(42)
```

{{out}}

```txt

Latin square of order 3 (0s):
231
123
312
Latin square of order 5 (0s):
15423
53142
24315
42531
31254
Latin square of order 5 (0s):
32514
21453
43125
15342
54231
Latin square of order 10 (0s):
3258A69417
9314275A86
586312479A
19A2753864
61294873A5
267194A538
473A618952
85973A6241
7A45831629
A486592173
rows completed:40/42, clashes:49854
rows completed:40/42, clashes:104051
Latin square of order 42 (2.1s):
5CMOgPTHbDBKGLU9d1aIREFNV8cQYeZ62AJXW3Sf74
VQEaBOIL2GefUYXbZWKP5cRDd3C4HS89MF7Jg6A1NT
LIRc1AXPWKJH4bTNFC2VS935g6ZEDfaeOGdYQ8B7MU
QPZ1LcN8O4I96dKRATfEWHaJUSM5e37GXBbDYFCV2g
ATIZD2VY8gGRNHBO6P35QaEM1fS79dKFbCX4JWcUeL
eE9Q7CJZ3VP254YMLHGOIBcdf1AbgXFKUR8WaTDNS6
7FfJdWGg4ZDC1UaVIcH9A5XLSb63MBTNPOQEKe2Y8R
DMa53QWdB9bPSeEZJg6GK2A7YR1C4VLcIXTUFO8HfN
G8d2eaARUPNEI6HCKQZSO4b1BM5g3L9VcTYFfXWJD7
9OHSGIRDEMf3YKb2cU1WVdNeTL8XA64gQPC57JZaBF
634YR52FINaeDQPKCXBAEZ8S7dgUG9OHJcfLMVTWb1
aVgXP7EO5f8JdFQ1RY9BHW42eTNGSUb3LMKZIC6AcD
YBWLbFUAP1T8JSZ6N2gMeIQfHc3DC75d9EVOG4aKRX
TXUBHe62JSCNA7WPfGLdYbOc9VRZ1IgD3aMQ5KEF48
dU1IS9D362V5cR8WTZCKfNPYA4OebQXJ7HBMLEFGga
3aDTUf5SCY6g8GOJbVIHZ1KWMNXBFPd7AQRc924LEe
1e2bJBHUDa9dTCRLYAWNXOMZFIK6QgfPE5S38G74Vc
F786NJP1Gb4BLXVdUaAfcTgQC2eWKHYZ53D9ERISOM
U1cRQGOeFBXWag6IPdVJ2Mf8L7YSNCHETK4bDZ539A
2DbgW6MfVAQY9acUX35ZNRBPIOJL81CSe4EHd7GTFK
8bNE2U7XTWHVBZJ5QMcePY9g6Ad1OaIf4DFKRSLC3G
g5L8AbKN7JF6MD1SBEQU3fTXWHVdR2P4ZY9aCceIGO
W2K9FHa5dT1OZ8C3gLScGVI4JB7YPDUbfe6RNAXMQE
X6QdY49JZ7E1fB5THFD2MeGAPaI8VRSWNgUCcLbOK3
EASPTVLWRe7I35Mc24FX9gHO8KbJdN1aB6GfZQUDCY
cSFM9ECbY65DW3GA17RTBQUVOg4K2ZNXadLPeIH8Jf
O9GFf8QEXdAMVN47SRUD13ZB5PH26cWTCJaebYKgLI
RcJUI319KFMXH2fea8dCL6WTG5EO74DYSNZAVgQBPb
M4CHOZdIQUgGE9DB7KJaTLeF2WfN5A38V1c6XbPRYS
IJBCad3cSQ27OEegM6XFb8LK4YDfTWGR1ZHNA59PUV
SRXA4D8GeHUQ7c2aVbNgJP1E3FTILOMBKf5d69YZWC
fNAeKMS7gR34bWIF5BEYDX69QCUTaGV28LO1HdJcZP
PfOD6SeKaXcF2Id4W9bQCU73RZLMBYA5g81GTNVEHJ
JZ5VMTcQfLRaP1SG8DOb4CYHNEFAXKeIW7g2BU36d9
4HVNEgbTLcKUCA7Y95eRFGJIaXBPZ8QMDW3SO1fd62
bgT7VKfBH5dLQJ3E4N86aSCRZUGFcM21Y9eIPDOXAW
CW3fXRg6NOSTKV9HEJYL8D5GbQPcIFBUd2A74M1eaZ
KGY3ZX4CA8OcFPNfeSM17JDbE92aW56LRVIgUHdQTB
NYP4C1ZM9EWSROLDGI786KVaXeQHfJcAFU2T3Bgb5d
HdeGcYB413LZgTFQDfP7UASCKJWVEbRO6IN82aM9X5
BK7W8LYacIZAeMgX3OT4dF26DG9RUEJCHbPVSfN51Q
ZL6K5NFVMCYbXfA8Oe43g7dUcDa9JTEQGSWB1PR2IH

```



## Python


```python
from random import choice, shuffle
from copy import deepcopy

def rls(n):
    if n <= 0:
        return []
    else:
        symbols = list(range(n))
        square = _rls(symbols)
        return _shuffle_transpose_shuffle(square)


def _shuffle_transpose_shuffle(matrix):
    square = deepcopy(matrix)
    shuffle(square)
    trans = list(zip(*square))
    shuffle(trans)
    return trans


def _rls(symbols):
    n = len(symbols)
    if n == 1:
        return [symbols]
    else:
        sym = choice(symbols)
        symbols.remove(sym)
        square = _rls(symbols)
        square.append(square[0].copy())
        for i in range(n):
            square[i].insert(i, sym)
        return square

def _to_text(square):
    if square:
        width = max(len(str(sym)) for row in square for sym in row)
        txt = '\n'.join(' '.join(f"{sym:>{width}}" for sym in row)
                        for row in square)
    else:
        txt = ''
    return txt

def _check(square):
    transpose = list(zip(*square))
    assert _check_rows(square) and _check_rows(transpose), \
        "Not a Latin square"

def _check_rows(square):
    if not square:
        return True
    set_row0 = set(square[0])
    return all(len(row) == len(set(row)) and set(row) == set_row0
               for row in square)


if __name__ == '__main__':
    for i in [3, 3,  5, 5, 12]:
        square = rls(i)
        print(_to_text(square))
        _check(square)
        print()
```


{{out}}

```txt
2 1 0
0 2 1
1 0 2

1 0 2
0 2 1
2 1 0

1 0 3 2 4
3 4 2 0 1
4 2 1 3 0
2 1 0 4 3
0 3 4 1 2

2 1 0 4 3
0 4 3 2 1
3 2 1 0 4
4 3 2 1 0
1 0 4 3 2

 6  2  4  8 11  9  3  1  7  0  5 10
 1 11  5  2  8  6  0  9  4 10  7  3
 2  7 10  5  4  8  9 11  0  6  3  1
 8  5  0  4  7 11  1  2  3  9 10  6
11  4  3  7  5  2  6  8 10  1  0  9
10  1  8  6  9  0  7  3 11  4  2  5
 7  0  1  3 10  5  8  4  6  2  9 11
 9  8  7 11  2  1 10  6  5  3  4  0
 3  9  2  1  6 10  4  0  8  5 11  7
 5  3  6 10  0  4 11  7  9  8  1  2
 4 10  9  0  3  7  2  5  1 11  6  8
 0  6 11  9  1  3  5 10  2  7  8  4
```



## REXX

This REXX version produces a randomized Latin square similar to the   '''Julia'''   program.

The symbols could be any characters (except those that contain a blank),   but the numbers from   '''0''' ──► '''N-1'''   are used.

```rexx
/*REXX program generates and displays a randomized Latin square.                        */
parse arg N seed .                               /*obtain the optional argument from CL.*/
if N=='' | N==","  then N= 5                     /*Not specified?  Then use the default.*/
if datatype(seed, 'W')  then call random ,,seed  /*Seed numeric?   Then use it for seed.*/
w= length(N - 1)                                 /*get the length of the largest number.*/
$=                                               /*initialize  $  string to null.       */
         do i=0  for N;    $= $ right(i, w, '_') /*build a string of numbers (from zero)*/
         end   /*i*/                             /* [↑]  $ string is (so far)  in order.*/
z=                                               /*Z:  will be the 1st row of the square*/
         do N;             ?= random(1,words($)) /*gen a random number from the $ string*/
         z= z word($, ?);  $= delword($, ?, 1)   /*add the number to string; del from $.*/
         end   /*r*/
zz= z||z                                         /*build a double-length string of  Z.  */
         do j=1  for N                           /* [↓]  display rows of random Latin sq*/
         say translate(subword(zz, j, N), , '_') /*translate leading underbar to blank. */
         end   /*j*/                             /*stick a fork in it,  we're all done. */
```

{{out|output|text=  for 1<sup>st</sup> run when using the default inputs:}}

```txt

4 1 3 0 2
1 3 0 2 4
3 0 2 4 1
0 2 4 1 3
2 4 1 3 0

```

{{out|output|text=  for 2<sup>nd</sup> run when using the default inputs:}}

```txt

2 1 0 4 3
1 0 4 3 2
0 4 3 2 1
4 3 2 1 0
3 2 1 0 4

```



## Ruby

This crude algorithm works fine up to a square size of 10; higher values take too much time and memory. It creates an array of all possible permutations, picks a random one as first row an weeds out all permutations which cannot appear in the remaining square. Repeat picking and weeding until there is a square.

```ruby
N = 5

def generate_square
  perms  =  (1..N).to_a.permutation(N).to_a.shuffle
  square = []
  N.times do
    square << perms.pop
    perms.reject!{|perm| perm.zip(square.last).any?{|el1, el2| el1 == el2} }
  end
  square
end

def print_square(square)
  cell_size = N.digits.size + 1
  strings = square.map!{|row| row.map!{|el| el.to_s.rjust(cell_size)}.join }
  puts strings, "\n"
end

2.times{print_square( generate_square)}

```

{{out}}
```txt

 3 4 2 1 5
 2 3 4 5 1
 1 2 5 3 4
 5 1 3 4 2
 4 5 1 2 3

 1 2 5 4 3
 2 3 4 1 5
 5 4 2 3 1
 3 5 1 2 4
 4 1 3 5 2


```



## zkl


```zkl
fcn randomLatinSquare(n,symbols=[1..]){  //--> list of lists
   if(n<=0) return(T);
   square,syms := List(), symbols.walker().walk(n);
   do(n){ syms=syms.copy(); square.append(syms.append(syms.pop(0))) }
   // shuffle rows, transpose & shuffle columns
   T.zip(square.shuffle().xplode()).shuffle();
}
fcn rls2String(square){ square.apply("concat"," ").concat("\n") }
```


```zkl
foreach n in (T(1,2,5)){ randomLatinSquare(n) : rls2String(_).println("\n") }
randomLatinSquare(5, ["A".."Z"])   : rls2String(_).println("\n");
randomLatinSquare(10,"!@#$%^&*()") : rls2String(_).println("\n");
```

{{out}}

```txt

1

1 2
2 1

3 1 4 5 2
4 2 5 1 3
1 4 2 3 5
5 3 1 2 4
2 5 3 4 1

E D A B C
D C E A B
B A C D E
A E B C D
C B D E A

& % # ! * @ ) $ ( ^
@ ) * ^ # & % ( $ !
( & % # ) $ @ ^ ! *
! ( & % @ ^ $ * # )
% # ! ( ^ ) * @ & $
^ $ @ ) & ! ( # * %
# ! ( & $ * ^ ) % @
$ @ ) * % ( & ! ^ #
) * ^ $ ! % # & @ (
* ^ $ @ ( # ! % ) &

```

