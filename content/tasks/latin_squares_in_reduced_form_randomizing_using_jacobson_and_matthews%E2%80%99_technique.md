+++
title = "Latin Squares in reduced form/Randomizing using Jacobson and Matthews’ Technique"
description = ""
date = 2019-08-27T11:25:36Z
aliases = []
[extra]
id = 22450
[taxonomies]
categories = ["task"]
tags = []
+++

Section 3.3 of [[https://pdfs.semanticscholar.org/4a7c/d245f6f6a4ef933c6cf697832607f71a39c1.pdf Generalised 2-designs with Block Size 3(Andy L. Drizen)]] describes a method of generating Latin Squares of order n attributed to Jacobson and Matthews. The purpose of this task is to produce a function which given a valid Latin Square transforms it to another using this method.

;part 1
Use one of the 4 [[Latin Squares in reduced form]] of order 4 as X0 to generate 10000 Latin Squares using X(n-1) to generate X(n). Convert the resulting Latin Squares to their reduced form, display them and the number of times each is produced.

;part 2
As above for order 5, but do not display the squares. Generate the 56 [[Latin Squares in reduced form]] of order 5, confirm that all 56 are produced by the Jacobson and Matthews technique and display the number of each produced.

;part 3
Generate 750 Latin Squares of order 42 and display the 750th.

;part 4
Generate 1000 Latin Squares of order 256. Don't display anything but confirm the approximate time taken and anything else you may find interesting
 
=={{header|F_Sharp|F#}}==

### The Functions


```fsharp

// Jacobson and Matthews technique for generating Latin Squares. Nigel Galloway: August 5th., 2019
let R=let N=System.Random() in (fun n->N.Next(n))

let jmLS α X0=
  let X0=Array2D.copy X0
  let N=let N=[|[0..α-1];[α-1..(-1)..0]|] in (fun()->N.[R 2])
  let rec randLS i j z n g s=
    X0.[i,g]<-s; X0.[n,j]<-s
    if X0.[n,g]=s then X0.[n,g]<-z; X0
    else randLS n g s (List.find(fun n->X0.[n,g]=s)(N())) (List.find(fun g->X0.[n,g]=s)(N())) (if (R 2)=0 then let t=X0.[n,g] in X0.[n,g]<-z; t else z)
  let i,j=R α,R α
  let z  =let z=1+(R (α-1)) in if z<X0.[i,j] then z else 1+(z+1)%α
  let n,g,s=let N=[0..α-1] in (List.find(fun n->X0.[n,j]=z) N,List.find(fun n->X0.[i,n]=z) N,X0.[i,j])
  X0.[i,j]<-z; randLS i j z n g s

let asNormLS α=
  let n=Array.init (Array2D.length1 α) (fun n->(α.[n,0]-1,n))|>Map.ofArray
  let g=Array.init (Array2D.length1 α) (fun g->(α.[n.[0],g]-1,g))|>Map.ofArray
  Array2D.init (Array2D.length1 α) (Array2D.length1 α) (fun i j->α.[n.[i],g.[j]])

let randLS α=Seq.unfold(fun g->Some(g,jmLS α g))(Array2D.init α α (fun n g->1+(n+g)%α))

```


### The Task

;part 1

```fsharp

randLS 4 |> Seq.take 10000 |> Seq.map asNormLS |> Seq.countBy id |> Seq.iter(fun n->printf "%A was produced %d times\n\n" (fst n)(snd n))

```

```txt

[[1; 2; 3; 4]
 [2; 3; 4; 1]
 [3; 4; 1; 2]
 [4; 1; 2; 3]] was produced 2920 times

[[1; 2; 3; 4]
 [2; 4; 1; 3]
 [3; 1; 4; 2]
 [4; 3; 2; 1]] was produced 2262 times

[[1; 2; 3; 4]
 [2; 1; 4; 3]
 [3; 4; 2; 1]
 [4; 3; 1; 2]] was produced 2236 times

[[1; 2; 3; 4]
 [2; 1; 4; 3]
 [3; 4; 1; 2]
 [4; 3; 2; 1]] was produced 2582 times

```

;part 2

```fsharp

randLS 5 |> Seq.take 10000 |> Seq.map asNormLS |> Seq.countBy id |> Seq.iteri(fun n g->printf "%d(%d) " (n+1) (snd g)); printfn ""

```

```txt

1(176) 2(171) 3(174) 4(165) 5(168) 6(182) 7(138) 8(205) 9(165) 10(174) 11(157) 12(187) 13(181) 14(211) 15(184) 16(190) 17(190) 18(192) 19(146) 20(200) 21(162) 22(153) 23(193) 24(156) 25(148) 26(188) 27(186) 28(198) 29(178) 30(217) 31(185) 32(172) 33(223) 34(147) 35(203) 36(167) 37(188) 38(152) 39(165) 40(187) 41(160) 42(199) 43(140) 44(202) 45(186) 46(182) 47(175) 48(161) 49(179) 50(175) 51(201) 52(195) 53(205) 54(183) 55(155) 56(178)

```

;part 3

```fsharp

let q=Seq.item 749 (randLS 42)
for n in [0..41] do (for g in [0..41] do printf "%3d" q.[n,g]); printfn ""

```

```txt

 16  7 41 15 17 40 12  9 10  5 19 29 21 18  8 22  3 36 23 31 11 38 13 30  2 33  6 42 39 14 32 20 28 35 26  1 34 37 27 24  4 25
 38 25 36 32 40 29 35 27  8 26 31 15  9  7 16 11  4  3 12 20 23 33  5 24 41 14 30 34 42 17 39 18 37 22 21 13  1 10  6 19  2 28
  8 34 27 25 21 31  1 23 37 36 26 13 22 24 35 17 10 40 41 30 42  7 15  2 18  3 29 11 32  4 38 39  9  5 16 14 28 12 20 33 19  6
 33 35 13 34 15 24  4 29 41 27  3 17 10 26 39 23 30 32  1 38 16 25 37 14  6 28 19  9 40  5 18  7 42 11 31 20 12 22  2 21  8 36
  2 42 20  1  7 26 11 10 39 41 34 22 40 23 24 29 14 17  5 33 38 30  6 13  3 16 18 19 31 15 28 21 36 37 32 27  8  4 25  9 35 12
 25 33 14 40 28 30 31 24 29  4  8 20 26 38 12 35  2 39 16  6 13 21 18 17  5 41 23  3 36  7 34 22 27  1 10 42 11 19 15 32 37  9
 17 22 35 28 30 18 21  2 15 39  5 40 27 13  1 34 38 37 26 23 41 36  4  3 11  6 20  8  9 10 12 24 31 25  7 29 16 32 42 14 33 19
 14  9 19  7 26 15 10  4 36 25 22 23 39 16  2 40 18  1 38 13 21 37 34 31 35 24 12 27 11  3  5  6 17 20 41 33 32 29  8 30 28 42
  5 27 24 13  2 36 25 30 23  9  6 14 35 15 42 39 16 26 21 34 33 31  3  1 29 12 38 17 37 19 40  4  7  8 22 41 20 28 32 10 18 11
 19 41 28 26  8 10 30 35 18 33 15 27 25 21 29 42 23 12 17  2  5  1 38  6 20  7 34  4 13 36 24 31 14  3 11 32 39 40  9 22 16 37
 41 10  3 19 22  9 27 40  1 29 16 42 33 39 34  7 37 20 11 12  4 18 35  8 28 26 36  5 17 30 25 32  6 15 24 21 13 23 14  2 38 31
 42  3 16 36 33 21 20 14 31 22  9 38 29 19 37 13 28 10 35 18 39 26 25 27  4 30 15 23 41 24 11  1 40  7  5 17  6  2 12  8 34 32
 23 31 34 41 38 33  3 28  4  1 30 25  6  2 20 14 13 24  8 42  7 12 39 32 22 29  5 37 15  9 27 10 35 36 19 40 17 18 16 11 26 21
 37 16 30 11  4 32 42 33 13  6 14  2 15 27 18 31 20 41 39 40  9 24 36  5 10  8  1 26  3 34 22 28 38 19 29 23 21 25 35 12 17  7
  1 19 26 22 16 25 36 39  3 23 41 37 34  6 17 32 40 21 10 27 12  9 31  7 13  4 24 29  8 11  2  5 15 18 35 28 30 20 33 38 42 14
 11 13 23 30 25 41  6 31 14 32 27 36 19 17 10 33 21 15  7  5  8 28 16 35 34 42 40  2 38 39  9 26 20 24 37  4 18  3 22  1 12 29
 24 17 29 38 23 39 32  5 11 15 35 12  8 10 40  1 22 25  2 36 28  4 42 21  9 20  3 31 16 41 13 30 19 34 33 18 27  6  7 37 14 26
 36  4  6 24 12 20  2 34 40 11 32  9 28  8 38 21  5 31 42 17 14 29 19 22 25 15  7 18 30 26  1 13 16 41 23 39 37 33  3 35 10 27
 20 39  2 12 32  7 22  3 17 10 37  6 18 40 27  5 42 35 28  4 24 14 33 29 30 31 26 13 19 23 36 41  1 21  9 11 15  8 34 16 25 38
 35 18 37  6  5 13 29  8 24 19 38 34 12 31 21 10 33  7  3 41 15 42 20 11 27 40 16 14 23  1  4  2 22 32 28  9 25 30 26 39 36 17
 10 32  9 33 39 19 41 38 35 18 28 26 14 30  7  4  1 22 37 21 31 40 27 15 42 34  2 25  5 12 23 36  8  6 17  3 29 24 11 13 20 16
 13 28 39  2 31  8  9 37 21 16 40 19 42 36 41  3 12 14 20 10 17 34  1 33 32 35 25 30 18 38 15 11 24 23  6 26  4  5 29  7 27 22
  7 40 12 39 18  3 16 21 42 17  1 32  5 33 13  6 41  8 29 14 34 35 24 36 38 25 31 28 26 27 20 37 23  2 30 10 22  9 19  4 11 15
  4 21  7 17 35 34 19 25 12 42 11  1 30 28 36 26 32 23 14 29  2 20  8 41 24 27 22 15 10 18 37  9 39 38 13  6  3 16 31 40  5 33
 34 23 42 14 41 27 37  6  9 31  4  5  7  1 25 16 35 30 33 11 19  3 26 12 17 38  8 20 24 13 29 15 32 28 40 22  2 39 18 36 21 10
 30  6 21  9 20 17  5 32 38 13 12 28 16 35 22 36 34 29 40 39 25 15 14 37 33 11  4 41  1  2 19  3 26 27 42  8 10  7 23 31 24 18
  6 38  8 10 42 35 13  1 16 37 21  3 11 34 32 20 29 18 25 22 36  5 30 26 39 23 28 12  2 31  7 19 33 40 14 24  9 41 17 27 15  4
 29 15  1 21 14 11 26 17 30 38 10 33 36 20  4 18 39 16 31  3 35  2 32 28 19 13 42  7 12  8  6 40  5  9 25 37 24 27 41 23 22 34
 21 36 32  8  6 23 15 19  2 14 18  4  3 11  5 28 26 13 34 25 30 17  7 42 16 22 39 40 29 37 33 12 41 10 27 31 35 38 24 20  9  1
 39 20 31 29 19  4 38 16 27 30 24 11  2  3 33 15  8 28 18 37 10 13  9 23 36  1 17 22 25 32 26 35 12 42 34  7 40 14 21  5  6 41
 12 11 17 42  9  2 14  7 22 24 25 31 38 41 15 19 36 33 32 28  1 10 29 40 23 18 37 39  6 21 35 27  3 16  8 30  5 26  4 34 13 20
 18 29 33 16 27 42 40 26  7  8 39 24 41  5 30 38  6  9 13  1 32 22  2 34 12 37 11 10 35 20 14 17 21  4 15 19 23 36 28 25 31  3
 28  2  4 18 11  5 23 20 25 35 42 30 31 14  3  9 24 27 19  7 22  6 12 10  1 32 41 36 21 33 16 34 29 13 39 15 38 17 37 26 40  8
  3 26 11 35 24 37 17 36  6  7 13 41  4 32  9  2 31 34 22 15 29  8 40 18 21  5 27  1 14 16 10 38 25 33 20 12 19 42 39 28 30 23
 31  5 22 27 10  6  8 13 34  2 33  7 32 42 26 12 19  4 15  9 40 16 28 38 37 39 35 24 20 29 17 23 11 14  3 25 41 21 36 18  1 30
 15 24  5 37  3 28  7 22 19 34 20 18 17 12 23  8 25 11 36 16 27 41 10  4 31  2  9 32 33 42 21 14 13 29 38 35 26  1 30  6 39 40
 27 37 25  5 13 16 24 41 28  3  2 10 23  4 14 30 11 38  6 19 26 32 21 20 40  9 33 35 34 22 42  8 18 17 12 36 31 15  1 29  7 39
 26 30 10  3 36 22 33 11  5 20 29 21 13 25 31 37 17  2  9 35 18 27 23 39 14 19 32 16 28  6  8 42  4 12  1 38  7 34 40 15 41 24
 32  8 18 31  1 14 34 12 33 28 17 39 37  9 19 27  7  5 30 24 20 23 11 25 15 36 21  6 22 40 41 16 10 26  4  2 42 35 38  3 29 13
  9 14 40 23 37 38 18 15 20 12 36  8  1 22 28 24 27 42  4 32  6 11 41 19 26 10 13 21  7 25 30 29 34 39  2 16 33 31  5 17  3 35
 22 12 15  4 34  1 39 42 32 40  7 35 20 29 11 25  9  6 24 26 37 19 17 16  8 21 14 38 27 28  3 33 30 31 18  5 36 13 10 41 23  2
 40  1 38 20 29 12 28 18 26 21 23 16 24 37  6 41 15 19 27  8  3 39 22  9  7 17 10 33  4 35 31 25  2 30 36 34 14 11 13 42 32  5

```

;part 4
Generating 1000 Latin Squares of order 256 takes about 1.5secs

```fsharp

printfn "%d" (Array2D.length1 (Seq.item 999 (randLS 256)))

```

```txt

256
Real: 00:00:01.512, CPU: 00:00:01.970, GC gen0: 10, gen1: 10

```



## Go

The J & M implementation is based on the C code [https://brainwagon.org/2016/05/17/code-for-generating-a-random-latin-square/ here] which has been heavily optimized following advice and clarification by Nigel Galloway (see Talk page) on the requirements of this task.

Part 4 is taking about 6.5 seconds on my Celeron @1.6 GHz but will be much faster on a more modern machine. Being able to compute random, uniformly distributed, Latin squares of order 256 reasonably quickly is interesting from a secure communications or cryptographic standpoint as the symbols of such a square can represent the 256 characters of the various extended ASCII encodings. 

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

type (
    vector []int
    matrix []vector
    cube   []matrix
)

func toReduced(m matrix) matrix {
    n := len(m)
    r := make(matrix, n)
    for i := 0; i < n; i++ {
        r[i] = make(vector, n)
        copy(r[i], m[i])
    }
    for j := 0; j < n-1; j++ {
        if r[0][j] != j {
            for k := j + 1; k < n; k++ {
                if r[0][k] == j {
                    for i := 0; i < n; i++ {
                        r[i][j], r[i][k] = r[i][k], r[i][j]
                    }
                    break
                }
            }
        }
    }
    for i := 1; i < n-1; i++ {
        if r[i][0] != i {
            for k := i + 1; k < n; k++ {
                if r[k][0] == i {
                    for j := 0; j < n; j++ {
                        r[i][j], r[k][j] = r[k][j], r[i][j]
                    }
                    break
                }
            }
        }
    }
    return r
}

// 'm' is assumed to be 0 based
func printMatrix(m matrix) {
    n := len(m)
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            fmt.Printf("%2d ", m[i][j]+1) // back to 1 based
        }
        fmt.Println()
    }
    fmt.Println()
}

// converts 4 x 4 matrix to 'flat' array
func asArray16(m matrix) [16]int {
    var a [16]int
    k := 0
    for i := 0; i < 4; i++ {
        for j := 0; j < 4; j++ {
            a[k] = m[i][j]
            k++
        }
    }
    return a
}

// converts 5 x 5 matrix to 'flat' array
func asArray25(m matrix) [25]int {
    var a [25]int
    k := 0
    for i := 0; i < 5; i++ {
        for j := 0; j < 5; j++ {
            a[k] = m[i][j]
            k++
        }
    }
    return a
}

// 'a' is assumed to be 0 based
func printArray16(a [16]int) {
    for i := 0; i < 4; i++ {
        for j := 0; j < 4; j++ {
            k := i*4 + j
            fmt.Printf("%2d ", a[k]+1) // back to 1 based
        }
        fmt.Println()
    }
    fmt.Println()
}

func shuffleCube(c cube) {
    n := len(c[0])
    proper := true
    var rx, ry, rz int
    for {
        rx = rand.Intn(n)
        ry = rand.Intn(n)
        rz = rand.Intn(n)
        if c[rx][ry][rz] == 0 {
            break
        }
    }
    for {
        var ox, oy, oz int
        for ; ox < n; ox++ {
            if c[ox][ry][rz] == 1 {
                break
            }
        }
        if !proper && rand.Intn(2) == 0 {
            for ox++; ox < n; ox++ {
                if c[ox][ry][rz] == 1 {
                    break
                }
            }
        }

        for ; oy < n; oy++ {
            if c[rx][oy][rz] == 1 {
                break
            }
        }
        if !proper && rand.Intn(2) == 0 {
            for oy++; oy < n; oy++ {
                if c[rx][oy][rz] == 1 {
                    break
                }
            }
        }

        for ; oz < n; oz++ {
            if c[rx][ry][oz] == 1 {
                break
            }
        }
        if !proper && rand.Intn(2) == 0 {
            for oz++; oz < n; oz++ {
                if c[rx][ry][oz] == 1 {
                    break
                }
            }
        }

        c[rx][ry][rz]++
        c[rx][oy][oz]++
        c[ox][ry][oz]++
        c[ox][oy][rz]++

        c[rx][ry][oz]--
        c[rx][oy][rz]--
        c[ox][ry][rz]--
        c[ox][oy][oz]--

        if c[ox][oy][oz] < 0 {
            rx, ry, rz = ox, oy, oz
            proper = false
        } else {
            proper = true
            break
        }
    }
}

func toMatrix(c cube) matrix {
    n := len(c[0])
    m := make(matrix, n)
    for i := 0; i < n; i++ {
        m[i] = make(vector, n)
    }
    for i := 0; i < n; i++ {
        for j := 0; j < n; j++ {
            for k := 0; k < n; k++ {
                if c[i][j][k] != 0 {
                    m[i][j] = k
                    break
                }
            }
        }
    }
    return m
}

// 'from' matrix is assumed to be 1 based
func makeCube(from matrix, n int) cube {
    c := make(cube, n)
    for i := 0; i < n; i++ {
        c[i] = make(matrix, n)
        for j := 0; j < n; j++ {
            c[i][j] = make(vector, n)
            var k int
            if from == nil {
                k = (i + j) % n
            } else {
                k = from[i][j] - 1
            }
            c[i][j][k] = 1
        }
    }
    return c
}

func main() {
    rand.Seed(time.Now().UnixNano())

    // part 1
    fmt.Println("PART 1: 10,000 latin Squares of order 4 in reduced form:\n")
    from := matrix{{1, 2, 3, 4}, {2, 1, 4, 3}, {3, 4, 1, 2}, {4, 3, 2, 1}}
    freqs4 := make(map[[16]int]int, 10000)
    c := makeCube(from, 4)
    for i := 1; i <= 10000; i++ {
        shuffleCube(c)
        m := toMatrix(c)
        rm := toReduced(m)
        a16 := asArray16(rm)
        freqs4[a16]++
    }
    for a, freq := range freqs4 {
        printArray16(a)
        fmt.Printf("Occurs %d times\n\n", freq)
    }

    // part 2
    fmt.Println("\nPART 2: 10,000 latin squares of order 5 in reduced form:")
    from = matrix{{1, 2, 3, 4, 5}, {2, 3, 4, 5, 1}, {3, 4, 5, 1, 2},
        {4, 5, 1, 2, 3}, {5, 1, 2, 3, 4}}
    freqs5 := make(map[[25]int]int, 10000)
    c = makeCube(from, 5)
    for i := 1; i <= 10000; i++ {
        shuffleCube(c)
        m := toMatrix(c)
        rm := toReduced(m)
        a25 := asArray25(rm)
        freqs5[a25]++
    }
    count := 0
    for _, freq := range freqs5 {
        count++
        if count > 1 {
            fmt.Print(", ")
        }
        if (count-1)%8 == 0 {
            fmt.Println()
        }
        fmt.Printf("%2d(%3d)", count, freq)
    }
    fmt.Println("\n")

    // part 3
    fmt.Println("\nPART 3: 750 latin squares of order 42, showing the last one:\n")
    var m42 matrix
    c = makeCube(nil, 42)
    for i := 1; i <= 750; i++ {
        shuffleCube(c)
        if i == 750 {
            m42 = toMatrix(c)
        }
    }
    printMatrix(m42)

    // part 4
    fmt.Println("\nPART 4: 1000 latin squares of order 256:\n")
    start := time.Now()
    c = makeCube(nil, 256)
    for i := 1; i <= 1000; i++ {
        shuffleCube(c)
    }
    elapsed := time.Since(start)
    fmt.Printf("Generated in %s\n", elapsed)
}
```


Sample run:

```txt

PART 1: 10,000 latin Squares of order 4 in reduced form:

 1  2  3  4 
 2  1  4  3 
 3  4  2  1 
 4  3  1  2 

Occurs 2550 times

 1  2  3  4 
 2  4  1  3 
 3  1  4  2 
 4  3  2  1 

Occurs 2430 times

 1  2  3  4 
 2  1  4  3 
 3  4  1  2 
 4  3  2  1 

Occurs 2494 times

 1  2  3  4 
 2  3  4  1 
 3  4  1  2 
 4  1  2  3 

Occurs 2526 times


PART 2: 10,000 latin squares of order 5 in reduced form:

 1(165),  2(173),  3(167),  4(204),  5(173),  6(165),  7(215),  8(218), 
 9(168), 10(157), 11(205), 12(152), 13(187), 14(173), 15(215), 16(185), 
17(179), 18(176), 19(179), 20(160), 21(150), 22(166), 23(191), 24(181), 
25(179), 26(192), 27(187), 28(186), 29(176), 30(196), 31(141), 32(187), 
33(165), 34(189), 35(147), 36(175), 37(172), 38(162), 39(180), 40(172), 
41(189), 42(159), 43(197), 44(158), 45(178), 46(179), 47(193), 48(175), 
49(207), 50(174), 51(181), 52(179), 53(193), 54(171), 55(153), 56(204)


PART 3: 750 latin squares of order 42, showing the last one:

29  2 17 41 34 30  8 33 39  7 20 27 12  6 31 14 40 35 25  9 10 32 19 16 24 42  3 26  5 23  1 28  4 13 38 18 21 37 22 15 36 11 
17 15 11 31  9 38 26 10  1 28 37  8 34 41 21 22 12  5 35 36 13 20 29 42 18  3 19 24 39 32 27 23 16 25 33  4 40  6  2 30  7 14 
36 42 35 39 15 34 37 18 32 25 22 31  4 17  3 19 13 11  8 23 12 24 28 27 16  1  6  9 29 40  7  5  2 14 30 26 41 10 21 33 38 20 
21 13 16 42  3 32  2 26 27 17 15 11 25 37 29  6 19 10 12  7 31 18 36  9 39 41 30 40 35 33 22  1 28 38 24  8 34 23  4 20 14  5 
22 39 13  7 38  9 34 41 37 36 35  6 21 26 17 16  4 30 40 20  8 15 25 19 32  2 11 28 23 24 31 10 42  3 27 12 33 14  1 29  5 18 
33 36 34  3 13  4  7 14  2 29  6 12 31 23 26 17  8 20 32 21 19 41 37  5 38 30 25 11 24 35 42 27 18 16 39 15 10 22 28  1  9 40 
14 31  7 22 39 23 32 34 16 33 24  4 40 42 12 25 35 26 18 28 11  3 15 21 20  9 13 19  1 10  2 41 29  6 17 30  5 38 37  8 27 36 
 9  3  6 30 19 39 14 16  4 15 29 28 23 24 32 10 18 41 37 38 40 34  8 25  2 22 31  5 17 26 36 33 13 21 12 35  7 20 11 27 42  1 
 2 18 28  5  6  7 40 35  3 20  8 34 42 39 37 33 26 23 22 13 14  4 12 15 17 25 36 31 16 29 38 19 32 41  1 27 24 11 30  9 10 21 
27 34 19 15 33 22  5 36  9 30 14  1 24  8 38 42 41 39  7 40  4 37 11 23 29 26 18 12  3 21 35 16 20 10 31 25 17 28  6 32  2 13 
41 16  1 35 22 13 20 29  6 38  5 24 19 10 25 27 17 18 11 32  9  7  2 36  4 34 40 21 33 12  8 30 15 42 37 23 14 26  3 39 31 28 
 7  1 15 16 27 31 18 24 20  8 36 38 10 34  9  4 42 29  2  3 26 39  5 22 41 21 37 30 14 11 33 35 25 23 40 28 13 19 17  6 32 12 
 1 10 20 32 23  5 30 12  8  9 21 36 15 14 18 37 33 31 26 39 41 16  6 24 22 35 29 42 27 28  3 38 11  2  7 34  4 40 19 17 13 25 
 6 32 42 11 20 40 27 25 41 22 17 16 26 29 15  7 23 36 39 34 28 13 18  3 10 37  8 14  2 31  4 24  5 19  9 21 38  1 33 12 30 35 
35 40 30 19 21 12 17  4 22 27  3 20 11  9  8 23 24 42 14 10 39 28 26 29 33 13 41 16 34 25 32 37  7 18  5  6 15  2 36 38  1 31 
15 26 40  1 28 20  9 21  7  5 13 18 30 22 10  8  3 25  6  2 17 36 38 31 14 19 35 23 12 27 11 39 24  4 41 32 29 34 42 16 37 33 
 3  6 26 12 32  1 13  8 42 37 25  7  9 16 35  5 29 21 24 27 34 17 14  2 15 11 28 33 20 38 18 22 39 40 23 10 31 30 41 36 19  4 
31 38 36 21 16 26 28 30 15  3 32 41 18  1  6 29  9 17  5 35  7 40 27 37 13 20 23 22 11 19 12 42 34  8 10 14 25 39 24  4 33  2 
40  4 22 38 35 11 21 17 31  1 28 19 37  2 42 24 14 12 13 30 33 25 34 32 27 36 39  3  9 15 10 18  8  5  6 41 26 16 29  7 20 23 
 5 17 39  4 26 14 31 37 35 11 38  3  1 30 19 36 20 33 15 16 21 29  9  6 25 27  2 13 41 34 24 12 10 32 22  7 28 18 40 42 23  8 
 8 29 24 26 31 21 39 23 11 14 19 10 20 15  7 35 32 38  1 12 25 22 16  4  6 40 42 41 18 30 28  2 17 36  3 13 37 33 27  5 34  9 
11 25 14 17 18 24 19 32 33 31  7 26  2 21 20 30 15 27 23 41 29 35 39 28 34 12 10  4  8 42  5 13 37  9 16 40  1 36 38  3  6 22 
26 21 18 25 29 15  1 13 19  2 34 23 38 27 41  3 10 22 17  4 16 11 42 12  8  6  5 35 30 39 37 14  9 24 36 33 20  7 31 28 40 32 
25 27 12 33 17 35 24  9 28 10 42 21  8 13  2 15 34 16  3 18  5 31 41  7 23  4  1  6 22 14 19 36 40 37 26 38 30 32 20 11 39 29 
23 19 25  9 30 37 38 40 14 41 31 17  7  4 16 11  1  6 33  5 24  2  3  8 21 29 34 32 28 22 15 20 12 35 18 36 39 27 10 13 26 42 
34  9 10 13  2  6 22 31 26 40  1 14 41  3 11 12 37 32 27 29 35 19 30 33 28 38 21 25  7  5 16  8 36 15 20 42 23 17 39 18  4 24 
20 11 37 28 41  8 10 15 36 12 26 33 39 32 13  1 25  9 42 19  3  6 24 14  5 23  7 27 38  2 30  4 22 34 35 31 18 29 16 40 21 17 
28 30 21 23 24 29  3  1 10  6 33  2 27 40 14 34 31 15 19 37 18  9  4 13 35  8 12 20 36 16 17 32 41  7 25 39 42  5 26 22 11 38 
32 12  8 40 11 16 23 28 18 42 41 30  3 38 33  2 22 19  4 25 37  1 31 20 36  5  9  7 13 17 14  6 27 39 34 24 35 21 15 26 29 10 
18 37 41 10 36 28 11 42 13 34  2 35  5  7 22 40 39  3 30  1 38 27 20 17 19 33 26 15 25  6 21 29 23 31  4  9 32  8 12 14 24 16 
39 24 29 37 25 19 33 27 17 16 10 40 36 12 30 41 11  4 34 15  2  5 32  1 31 14 38 18 42  3  9  7  6 20 21 22  8 13 23 35 28 26 
19 14  5  8 40  3 29  6 21 26 23 15 16 33 28 31 38 13  9 17 27 12 10 11  7 24 20  1  4 41 39 25 30 22 32  2 36 42 35 34 18 37 
37  7 32 34  8 36 41  2 12 24 16 39 33 31  4 13  6 28 38 22 20 42 40 18  9 10 14 29 26  1 23 15 21 27 19 17 11  3  5 25 35 30 
 4 41 27  2 42 17 15 38 30 35 12 25 13 28 39 20  5  1 16 33 36 23  7 40 37 32 24 10 31  8  6 21 14 26 29 11  3  9 18 19 22 34 
38 35 23 36  4 10 12 11  5 21 27 32 17 25 24 18 28 40 20  6 42 14 22 30 26 39 33  8 37  7 13 34  1 29 15 19  2 41  9 31 16  3 
30 33 31 24 12 41 36 19 23 32  4 37 29 11 34 39 16 14 21 42  6 26  1 38  3 17 22  2 40 18 20  9 35 28 13  5 27 15 25 10  8  7 
42 28  3 14  1 25 16 22 34 23 39  9 35  5 40 26 36  7 10 31 32 21 13 41 30 18  4 38  6 37 29 17 33 12 11 20 19 24  8  2 15 27 
16  5 38  6 10 27  4  3 40 18 11 13 22 35  1 21  2 34 36  8 23 30 17 39 42  7 15 37 32 20 26 31 19 33 28 29  9 25 14 24 12 41 
24 23 33 18 14  2 25 39 29 19  9  5 28 20 27 38  7  8 31 11 15 10 35 34 12 16 32 17 21 36 40  3 26 30 42  1 22  4 13 37 41  6 
12 20  2 29  5 33 42  7 24  4 18 22 14 19 36  9 27 37 28 26 30 38 23 10 11 31 17 34 15 13 41 40  3  1  8 16  6 35 32 21 25 39 
13  8  9 27 37 42  6 20 25 39 40 29 32 18  5 28 30 24 41 14 22 33 21 35  1 15 16 36 10  4 34 26 38 11  2  3 12 31  7 23 17 19 
10 22  4 20  7 18 35  5 38 13 30 42  6 36 23 32 21  2 29 24  1  8 33 26 40 28 27 39 19  9 25 11 31 17 14 37 16 12 34 41  3 15 


PART 4: 1000 latin squares of order 256:

Generated in 6.581088256s

```



## Phix

```Phix
function shuffleCube(sequence c)
    integer n = length(c), rx, ry, rz
    bool proper = true
    while true do
        rx = rand(n)
        ry = rand(n)
        rz = rand(n)
        if c[rx][ry][rz] == 0 then exit end if
    end while
    while true do
        integer ox, oy, oz
        for ox=1 to n do
            if c[ox][ry][rz] == 1 then exit end if
        end for
        if not proper and rand(2)==2 then
            for ox=ox+1 to n do
                if c[ox][ry][rz] == 1 then exit end if
            end for
        end if
        for oy=1 to n do
            if c[rx][oy][rz] == 1 then exit end if
        end for
        if not proper and rand(2)==2 then
            for oy=oy+1 to n do
                if c[rx][oy][rz] == 1 then exit end if 
            end for
        end if
        for oz=1 to n do
            if c[rx][ry][oz] == 1 then exit end if
        end for
        if not proper and rand(2)==2 then
            for oz=oz+1 to n do
                if c[rx][ry][oz] == 1 then exit end if
            end for
        end if
 
        c[rx][ry][rz] += 1
        c[rx][oy][oz] += 1
        c[ox][ry][oz] += 1
        c[ox][oy][rz] += 1
 
        c[rx][ry][oz] -= 1
        c[rx][oy][rz] -= 1
        c[ox][ry][rz] -= 1
        c[ox][oy][oz] -= 1
 
        if c[ox][oy][oz] < 0 then
            {rx, ry, rz} = {ox, oy, oz}
            proper = false
        else
            proper = true
            exit
        end if
    end while
    return c
end function
 
function toMatrix(sequence c)
    integer n = length(c)
    sequence m = repeat(repeat(0,n),n)
    for i=1 to n do
        for j=1 to n do
            for k=1 to n do
                if c[i][j][k] != 0 then
                    m[i][j] = k
                    exit
                end if
            end for
        end for
    end for
    return m
end function
 
function toReduced(sequence m)
    integer n := length(m)
    for j=1 to n-1 do
        if m[1][j]!=j then
            for k=j+1 to n do
                if m[1][k]==j then
                    for i=1 to n do
                        {m[i][j], m[i][k]} = {m[i][k], m[i][j]}
                    end for
                    exit
                end if
            end for
        end if
    end for
    for i=2 to n-1 do
        if m[i][1]!=i then
            for k=i+1 to n do
                if m[k][1]==i then
                    for j=1 to n do
                        {m[i][j], m[k][j]} = {m[k][j], m[i][j]}
                    end for
                    exit
                end if
            end for
        end if
    end for
    return m
end function

function makeCube(object from, integer n)
    sequence c = repeat(repeat(repeat(0,n),n),n)
    for i=1 to n do
        for j=1 to n do
            integer k = iff(from==NULL?mod(i+j,n)+1:from[i][j])
            c[i][j][k] = 1
        end for
    end for
    return c
end function

procedure main()
 
    printf(1,"Part 1: 10,000 latin Squares of order 4 in reduced form:\n\n")
    sequence from = {{1, 2, 3, 4}, {2, 1, 4, 3}, {3, 4, 1, 2}, {4, 3, 2, 1}},
             c := makeCube(from, 4), m, rm, fk
    integer freq = new_dict()
    for i=1 to 10000 do
        c = shuffleCube(c)
        m = toMatrix(c)
        rm = toReduced(m)
        setd(rm,getd(rm,freq)+1,freq)
    end for
    fk = getd_all_keys(freq)
    for i=1 to length(fk) do
        printf(1,"%v occurs %d times\n", {fk[i],getd(fk[i],freq)})
    end for

    printf(1,"\nPart 2: 10,000 latin squares of order 5 in reduced form:\n\n")
    from = {{1, 2, 3, 4, 5}, {2, 3, 4, 5, 1}, {3, 4, 5, 1, 2},
            {4, 5, 1, 2, 3}, {5, 1, 2, 3, 4}}
    c = makeCube(from, 5)
    destroy_dict(freq, justclear:=true)
    for i=1 to 10000 do
        c = shuffleCube(c)
        m = toMatrix(c)
        rm = toReduced(m)
        setd(rm,getd(rm,freq)+1,freq)
    end for
    fk = getd_all_keys(freq)
    for i=1 to length(fk) do
        fk[i] = sprintf("%2d(%3d)", {i,getd(fk[i],freq)})
    end for
    puts(1,join_by(fk,8,7," ","\n"))
    destroy_dict(freq)
 
    -- part 3
    printf(1,"\nPart 3: 750 latin squares of order 42, showing the last one:\n\n")
    c = makeCube(NULL, 42)
    for i=1 to 750 do
        c = shuffleCube(c)
    end for
    m = toMatrix(c)
    integer n := length(m)
    for i=1 to n do
        for j=1 to n do
            m[i,j] = sprintf("%2d",m[i,j])
        end for
        m[i] = join(m[i]," ")
    end for
    printf(1,"%s\n",join(m,"\n"))

    -- part 4
    printf(1,"\nPART 4: 1000 latin squares of order 256:\n\n")
    atom t0 = time()
    c = makeCube(NULL, 256)
    for i=1 to 1000 do
        c = shuffleCube(c)
    end for
    printf(1,"Generated in %s\n", elapsed(time()-t0))
end procedure
main()
```

```txt

Part 1: 10,000 latin Squares of order 4 in reduced form:

{{1,2,3,4},{2,1,4,3},{3,4,1,2},{4,3,2,1}} occurs 2503 times
{{1,2,3,4},{2,1,4,3},{3,4,2,1},{4,3,1,2}} occurs 2560 times
{{1,2,3,4},{2,3,4,1},{3,4,1,2},{4,1,2,3}} occurs 2510 times
{{1,2,3,4},{2,4,1,3},{3,1,4,2},{4,3,2,1}} occurs 2427 times

Part 2: 10,000 latin squares of order 5 in reduced form:

 1(172)  9(197) 17(228) 25(166) 33(171) 41(224) 49(171)
 2(168) 10(162) 18(216) 26(227) 34(172) 42(155) 50(226)
 3(159) 11(198) 19(206) 27(165) 35(189) 43(190) 51(174)
 4(170) 12(207) 20(159) 28(166) 36(177) 44(171) 52(196)
 5(211) 13(148) 21(172) 29(173) 37(183) 45(189) 53(197)
 6(169) 14(163) 22(128) 30(179) 38(184) 46(138) 54(173)
 7(168) 15(155) 23(146) 31(170) 39(187) 47(170) 55(206)
 8(193) 16(177) 24(146) 32(176) 40(157) 48(183) 56(177)

Part 3: 750 latin squares of order 42, showing the last one:

 5 29 15  7 25 26  2 35 21 39  8 12 17 31  3 20 23 22 40 34 13 32 27 38  9  6 36 41 11 19  4 42 10 28 33 18 30 16  1 14 37 24
34 17 22 12 38 28 20 42 15 10  4  3 30 16 35 23 11 19 31  8 32  1 33 36 24  2 18 39  9 41 40 26 25 27 29  5  7 37 21 13  6 14
23 14 41 38  2 36  4 34 29 16 11 10 24 13 26 31 30 12 28 18  7 21 40 42 27  9 37 35  1  3 17 22 20  5  6 33 32 39 25 19 15  8
29 21 27 41  3 10 12 23  4 18 39  1 11  6 20 34  2 35 36 37 40  5 14 26 17 42 24 33 32 16 28  8 13 30 15  9 25 19 38  7 31 22
 8 32 10 17 30 15 18 13 19  6 26 29 34 42 28 40 24 23 33  7  3  4 12 37 38 36  1 21 41 20 16 25  5 11  2 39 14 22 31 35  9 27
27 40 39 16 11 23 14 20  6  4 19 28 36 12 31 24 42 10 35 33 17 18 30  3 21  5 38 15  7  1  9 34  8 32 37 13  2 26 29 22 25 41
31 39 29 22 20  6 11 17 16 19 41 36 35 33 30 14  4  2 15 24 21 10 25  1 18 12 40 28  5 37 32 27  3 13 42 38  9 34 26  8  7 23
11 33 42 28 14  7  6 24 37 26 13 35  9  5 19 18 15 20 25 41 30 17  3 12 22  8 21 27 39 10 34 40 32 36  4 31 23 29  2 38 16  1
20 11  7  8 32 31 40 37 42 13 21 22 26  2 12 29  1 27  6 14 19 41 38 17 36 25  4  5 30 15 24 35 16 34 39  3 28 23  9 18 33 10
24  9 28 40 33 29  3  7 34 11 16 27  2 30 42 25 21 13 41 10 38  8 39 35 12 26 19 20 23 31  5 32  1 22 14  6  4 15 37 36 17 18
 2  8 23 37 27  9 38 36 13 24 31 14 29  7  6 42  3 34 18 32  1 20 22 41 25 30 33 16 15  4 11 10 26 39 21 28 17 40 19  5 12 35
13  2 26 15 10 40 39  6 33 29 42 34 12 17 11 28 22 32 14 25 24 37 21  5  8 23 30  9 18  7 41 31  4  3 27 19 16 35 20  1 38 36
22 23 34 31 28 25 36 38  9 32 30  8  3 11 17 41 26 39 24  6  2 35 13  4  7 21 29 18 14 27 19 37 15 20 16 12 10 33 40 42  1  5
36 28 20 11 29 39 22 41 35  7  5 15 31 24  8 19 27 37  1 38 16 13  6  2 32 40 14 25 33 17 21  4 34 23 30 10 18 42 12  9 26  3
 6 25  8  2 17 33 19 12  1 38 40 39  5 32 18  7 34 30  9 11 15  3 31 23 37 24 27 14 20 28 36 16 21 42 13 29 41  4 35 10 22 26
14 24 38 32 12  3 15  2 17 28 36 40 19 26  1 27 29 41  8  5 23 42 20 13 10 34  6 31 16 35 30  7 11 18 22 21 33 25  4 37 39  9
39 30  5 20  1 22  9 40 36 27  7 33 37 18 29 38 25 42  4 21 14 31 10 28 26 15 16  8  3 13 35 19 41  2 32 24 12 11 17 23 34  6
35 18 17 14 13 41 25 31  2  3 32 24 10 19 22 33  6  1 16 23  9 15  8 39  5  7 11 12 42 34 37 28 38  4 26 20 40 36 27 21 30 29
 9 19 24 26 42 16  7 30 10 40 29  4 33  8 38 22 14 25 37 28  5 27 41 32  1 13 17 36 34 39 23 11 31  6 35  2 20 21 18 15  3 12
12 22 37  1  4 20 32  3 30 25 28 26  6 14 36 11 39 21 38 29 27 24  7 16 15 31  9 34 10 33 13 18 40 35  5 17 19  8 42 41 23  2
 3 16 31 42  7 17 37 25 23 36 15 18 27 22  5 21 40  9 10 39  4 26 29  6  2 33 41 19 35  8 12 20 28 38 24 32 11  1 34 30 14 13
19 41 36 34 21 18 26 29 27 20 14 16 38 40  7 15 32  3 17  4 10 28 35 33 13 22  8  6 25 42 31 23  2 37  9 30  1 12  5 24 11 39
25  4 12 29 26 37 16  9 22 30  6 23 40 21 15 35 20 38 19 42 11  2  1 18  3 41  5 10 28 36 33 39 27 24 34  8 31 32 14 17 13  7
41 12 14 33 40 35 28 15  7  9  1  5 13 23 27 32  8 17 26 31 42 34 37 19 30 38 20 22  2  6 39 21 36 29 18 16  3 24 11  4 10 25
26 20  3 19 16 30  5 14  8 41 10  7 25 15 21 13 38 36 39 22 28 23 17 27 33 37 34 32  4  2 29 12  9 31  1 42 24 18  6 40 35 11
10  1 25 36 37 24  8 26  3 12 34 42 18 38 41 16  9 14 32 35 31 30  5 22 39 27  7  4 13 29  6 15 23 19 28 11 21  2 33 20 40 17
37 35 40 13 39  8 31 33 38 15 12 32 16 41 34  6  5 11 30 27 20 22 26 14 29 18 28 23 36 21 25  2  7  1 17  4 42  9 10  3 24 19
30 34  2 24 35  1 23 10 20 42 22 37 15 39  9 17 12  4  5 26 18 38 16 29 31  3 25 11 21 14  8 41  6 40 19  7 13 27 28 32 36 33
16  7 19 21 18 27 29 22 39 35  2 38 28 20 40  9 36  8 12  1 41 33 15 31 11 10 42 24  6 32 26 17 37 14 25 23  5 13  3 34  4 30
32  3 11 25  5 12  1  4 18 31 33 19 41  9 37 10  7 24 13 40  6 16 42 21 34 20 26  2 38 22 15 14 35 17 23 36  8 30 39 27 29 28
 1 13 30 39 36  4 34 32 12 14 17  6 23 27 24  3 41 40 11 20 22  9 28 15 42 16  2 29 31  5  7 33 19 21 10 35 26 38  8 25 18 37
18 38  4 23 41 19 35 21 26 33 37 20 42 28 13  5 10  7  3 15 25 39 32  9 14 17 31 40 29 24  1 36 30  8 12 34 27  6 22 11  2 16
 4 27 21  3  8 42 41 16 40 37 18  2 22 25 32 36 17  5 23 30 29  6  9 34 19 35 15 13 24 11 14  1 12 10 38 26 39 20  7 33 28 31
40 26  9 30  6 21 42 19  5  2  3 31  4 35 23 37 28 15 20 13 34 12 11  8 16 14 39 17 22 25 27 38 18 33  7  1 36 10 24 29 41 32
28 15  1  4 19 11 24  5 31  8 23 17 21 34 14 26 37 18  7  2 35 29 36 10  6 39 32 30 27 38  3  9 33 16 20 25 22 41 13 12 42 40
21 10 35 27 31  2 13 39 28  5  9 41  1 36  4  8 19 29 34 16 33 40 24 25 20 11 22  7 12 18 42 30 14 26  3 37 15 17 23  6 32 38
17 42 18  6 23  5 33  1 24 34 35 30  7 37 16 12 31 26 21 19 39 14  4 11 41 32 10  3 40  9 38 13 22 25 36 27 29 28 15  2  8 20
42  6 13 35 22 32 10  8 14 21 24 11 39  1  2  4 18 33 27  9 12 25 23 40 28 29  3 26 37 30 20  5 17 41 31 15 38  7 36 16 19 34
 7 36 16  5  9 34 21 11 32 22 20 25  8 10 33 30 35 31 29 12 26 19  2 24  4  1 13 38 17 23 18  6 39 15 40 14 37  3 41 28 27 42
15 37 32  9 24 38 27 28 41 17 25 13 20 29 10 39 33  6  2 36  8  7 18 30 35  4 23  1 19 26 22  3 42 12 11 40 34 14 16 31  5 21
33 31  6 18 34 14 17 27 25  1 38 21 32  4 39  2 13 16 42  3 36 11 19  7 23 28 12 37  8 40 10 29 24  9 41 22 35  5 30 26 20 15
38  5 33 10 15 13 30 18 11 23 27  9 14  3 25  1 16 28 22 17 37 36 34 20 40 19 35 42 26 12  2 24 29  7  8 41  6 31 32 39 21  4

PART 4: 1000 latin squares of order 256:

Generated in 19.5s

```

Unfortunately the last part of this task exposes the relatively poor performance of subscripting in phix.
