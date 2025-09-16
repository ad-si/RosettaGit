+++
title = "Factorial base numbers indexing permutations of a collection"
description = ""
date = 2019-10-11T06:22:47Z
aliases = []
[extra]
id = 22098
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "factor",
  "go",
  "j",
  "julia",
  "perl",
  "perl_6",
  "phix",
  "python",
  "zkl",
]
+++

You need a random arrangement of a deck of cards, you are sick of lame ways of doing this. This task is a super-cool way of doing this using factorial base numbers.
The first 25 factorial base numbers in increasing order are: 0.0.0, 0.0.1, 0.1.0, 0.1.1, 0.2.0, 0.2.1, 1.0.0, 1.0.1, 1.1.0, 1.1.1,1.2.0, 1.2.1, 2.0.0, 2.0.1, 2.1.0, 2.1.1, 2.2.0, 2.2.1, 3.0.0, 3.0.1, 3.1.0, 3.1.1, 3.2.0, 3.2.1, 1.0.0.0
Observe that the least significant digit is base 2 the next base 3, in general an n-digit factorial base number has digits n..1 in base n+1..2.

I want to produce a 1 to 1 mapping between these numbers and permutations:-
        0.0.0 -> 0123
        0.0.1 -> 0132
        0.1.0 -> 0213
        0.1.1 -> 0231
        0.2.0 -> 0312
        0.2.1 -> 0321
        1.0.0 -> 1023
        1.0.1 -> 1032
        1.1.0 -> 1203
        1.1.1 -> 1230
        1.2.0 -> 1302
        1.2.1 -> 1320
        2.0.0 -> 2013
        2.0.1 -> 2031
        2.1.0 -> 2103
        2.1.1 -> 2130
        2.2.0 -> 2301
        2.2.1 -> 2310
        3.0.0 -> 3012
        3.0.1 -> 3021
        3.1.0 -> 3102
        3.1.1 -> 3120
        3.2.0 -> 3201
        3.2.1 -> 3210

The following psudo-code will do this:
Starting with m=0 and Ω, an array of elements to be permutated, for each digit g starting with the most significant digit in the factorial base number.

If g is greater than zero, rotate the elements from m to m+g in Ω (see example)
Increment m and repeat the first step using the next most significant digit until the factorial base number is exhausted.
For example: using the factorial base number 2.0.1 and Ω = 0 1 2 3 where place 0 in both is the most significant (left-most) digit/element.

Step 1: m=0 g=2; Rotate places 0 through 2. 0 1 2 3 becomes 2 0 1 3
Step 2: m=1 g=0; No action.
Step 3: m=2 g=1; Rotate places 2 through 3. 2 0 1 3 becomes 2 0 3 1

Let me work 2.0.1 and 0123
     step 1 n=0 g=2 Ω=2013
     step 2 n=1 g=0 so no action
     step 3 n=2 g=1 Ω=2031

The task:

  First use your function to recreate the above table.
  Secondly use your function to generate all permutaions of 11 digits, perhaps count them don't display them, compare this method with
     methods in rc's permutations task.
  Thirdly here following are two ramdom 51 digit factorial base numbers I prepared earlier:
    39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
    51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
    use your function to crate the corresponding permutation of the following shoe of cards:
       A♠K♠Q♠J♠10♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥10♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦10♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣10♣9♣8♣7♣6♣5♣4♣3♣2♣
  Finally create your own 51 digit factorial base number and produce the corresponding permutation of the above shoe
=={{header|F_Sharp|F#}}==
;The Functıons:

```fsharp

// Factorial base numbers indexing permutations of a collection
// Nigel Galloway: December 7th., 2018
let lN2p (c:int[]) (Ω:'Ω[])=
  let Ω=Array.copy Ω
  let rec fN i g e l=match l-i with 0->Ω.[i]<-e |_->Ω.[l]<-Ω.[l-1]; fN i g e (l-1)// rotate right
  [0..((Array.length Ω)-2)]|>List.iter(fun n->let i=c.[n] in if i>0 then fN n (i+n) Ω.[i+n] (i+n)); Ω
let lN n =
  let    Ω=(Array.length n)
  let fN g=if n.[g]=Ω-g then n.[g]<-0; false else n.[g]<-n.[g]+1; true
  seq{yield n; while [1..Ω]|>List.exists(fun g->fN (Ω-g)) do yield n}

```


;Re-create the table:

```fsharp

lN [|0;0;0|] |> Seq.iter (fun n->printfn "%A -> %A" n (lN2p n [|0;1;2;3|]));;

```

```txt

[|0; 0; 0|] -> [|0; 1; 2; 3|]
[|0; 0; 1|] -> [|0; 1; 3; 2|]
[|0; 1; 0|] -> [|0; 2; 1; 3|]
[|0; 1; 1|] -> [|0; 2; 3; 1|]
[|0; 2; 0|] -> [|0; 3; 1; 2|]
[|0; 2; 1|] -> [|0; 3; 2; 1|]
[|1; 0; 0|] -> [|1; 0; 2; 3|]
[|1; 0; 1|] -> [|1; 0; 3; 2|]
[|1; 1; 0|] -> [|1; 2; 0; 3|]
[|1; 1; 1|] -> [|1; 2; 3; 0|]
[|1; 2; 0|] -> [|1; 3; 0; 2|]
[|1; 2; 1|] -> [|1; 3; 2; 0|]
[|2; 0; 0|] -> [|2; 0; 1; 3|]
[|2; 0; 1|] -> [|2; 0; 3; 1|]
[|2; 1; 0|] -> [|2; 1; 0; 3|]
[|2; 1; 1|] -> [|2; 1; 3; 0|]
[|2; 2; 0|] -> [|2; 3; 0; 1|]
[|2; 2; 1|] -> [|2; 3; 1; 0|]
[|3; 0; 0|] -> [|3; 0; 1; 2|]
[|3; 0; 1|] -> [|3; 0; 2; 1|]
[|3; 1; 0|] -> [|3; 1; 0; 2|]
[|3; 1; 1|] -> [|3; 1; 2; 0|]
[|3; 2; 0|] -> [|3; 2; 0; 1|]
[|3; 2; 1|] -> [|3; 2; 1; 0|]

```


;Shuffles:

```fsharp

let shoe==[|"A♠";"K♠";"Q♠";"J♠";"10♠";"9♠";"8♠";"7♠";"6♠";"5♠";"4♠";"3♠";"2♠";"A♥";"K♥";"Q♥";"J♥";"10♥";"9♥";"8♥";"7♥";"6♥";"5♥";"4♥";"3♥";"2♥";"A♦";"K♦";"Q♦";"J♦";"10♦";"9♦";"8♦";"7♦";"6♦";"5♦";"4♦";"3♦";"2♦";"A♣";"K♣";"Q♣";"J♣";"10♣";"9♣";"8♣";"7♣";"6♣";"5♣";"4♣";"3♣";"2♣";|]
//Random Shuffle
let N=System.Random() in lc2p [|for n in 52..-1..2 do yield N.Next(n)|] shoe|>Array.iter (printf "%s ");printfn ""
//Task Shuffles
lN2p [|39;49;7;47;29;30;2;12;10;3;29;37;33;17;12;31;29;34;17;25;2;4;25;4;1;14;20;6;21;18;1;1;1;4;0;5;15;12;4;3;10;10;9;1;6;5;5;3;0;0;0|] shoe|>Array.iter (printf "%s ");printfn ""
lN2p [|51;48;16;22;3;0;19;34;29;1;36;30;12;32;12;29;30;26;14;21;8;12;1;3;10;4;7;17;6;21;8;12;15;15;13;15;7;3;12;11;9;5;5;6;6;3;4;0;3;2;1|] shoe|>Array.iter (printf "%s ");printfn ""

```

```txt

J♣ Q♦ 10♣ 10♠ 3♥ 7♠ 8♥ 7♥ 8♦ 10♦ 4♥ 9♥ 8♠ K♥ 4♣ 5♥ K♣ Q♥ 9♠ A♦ Q♠ 6♦ K♦ K♠ 2♣ 6♠ 7♦ J♦ 2♥ 5♠ 4♦ 3♦ 6♣ J♥ 9♦ 4♠ 3♣ 2♠ 3♠ 10♥ Q♣ A♥ 2♦ A♠ 7♣ A♣ 9♣ 6♥ 5♦ 5♣ J♠ 8♣

A♣ 3♣ 7♠ 4♣ 10♦ 8♦ Q♠ K♥ 2♠ 10♠ 4♦ 7♣ J♣ 5♥ 10♥ 10♣ K♣ 2♣ 3♥ 5♦ J♠ 6♠ Q♣ 5♠ K♠ A♦ 3♦ Q♥ 8♣ 6♦ 9♠ 8♠ 4♠ 9♥ A♠ 6♥ 5♣ 2♦ 7♥ 8♥ 9♣ 6♣ 7♦ A♥ J♦ Q♦ 9♦ 2♥ 3♠ J♥ 4♥ K♦

2♣ 5♣ J♥ 4♥ J♠ A♠ 5♥ A♣ 6♦ Q♠ 9♣ 3♦ Q♥ J♣ 10♥ K♣ 10♣ 5♦ 7♥ 10♦ 3♠ 8♥ 10♠ 7♠ 6♥ 5♠ K♥ 4♦ A♥ 4♣ 2♥ 9♦ Q♣ 8♣ 7♦ 6♣ 3♥ 6♠ 7♣ 2♦ J♦ 9♥ A♦ Q♦ 8♦ 4♠ K♦ K♠ 3♣ 2♠ 8♠ 9♠
```


;Comparıson wıth [[http://www.rosettacode.org/wiki/Permutations#F.23 Permutations(F#)]]:

```fsharp

let g=[|0..10|]
lC 10 |> Seq.map(fun n->lc2p n g) |>  Seq.length

```

```txt

Real: 00:01:08.430, CPU: 00:01:08.970, GC gen0: 9086, gen1: 0
val it : int = 39916800

```

8GB of memory is insufficient for rc's perm task


## Factor


```factor
USING: assocs io kernel literals math math.factorials
math.parser math.ranges prettyprint qw random sequences
splitting ;
RENAME: factoradic math.combinatorics.private => _factoradic
RENAME: rotate sequences.extras => _rotate
IN: rosetta-code.factorial-permutations

CONSTANT: shoe $[
    qw{ A K Q J 10 9 8 7 6 5 4 3 2 } qw{ ♠ ♥ ♦ ♣ }
    [ append ] cartesian-map flip concat
]

! Factor can already make factoradic numbers, but they always
! have a least-significant digit of 0 to remove.
: factoradic ( n -- seq )
    _factoradic dup [ drop but-last ] unless-empty ;

! Convert "3.1.2.0" to { 3 1 2 0 }, for example.
: string>factoradic ( str -- seq )
    "." split [ string>number ] map ;

! Rotate a subsequence.
! E.g. 0 2 { 3 1 2 0 } (rotate) -> { 2 3 1 0 }.
: (rotate) ( from to seq -- newseq )
    [ 1 + ] dip [ snip ] [ subseq ] 3bi -1 _rotate glue ;

! Only rotate a subsequence if from does not equal to.
: rotate ( from to seq -- newseq )
    2over = [ 2nip ] [ (rotate) ] if ;

! The pseudocode from the task description
: fpermute ( factoradic -- permutation )
    dup length 1 + <iota> swap <enumerated>
    [ over + rot rotate ] assoc-each ;

! Use a factoradic number to index permutations of a collection.
: findex ( factoradic seq -- permutation )
    [ fpermute ] [ nths concat ] bi* ;

: .f ( seq -- ) [ "." write ] [ pprint ] interleave ;   ! Print a factoradic number
: .p ( seq -- ) [ pprint ] each nl ;                    ! Print a permutation

: show-table ( -- )
    "Generate table" print 24
    [ factoradic 3 0 pad-head dup .f fpermute " -> " write .p ]
    each-integer nl ;

: show-shuffles ( -- )
    "Generate given task shuffles" print
    "Original deck:" print shoe concat print nl
    "39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0"
    "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1"
    [ [ print ] [ string>factoradic shoe findex print nl ] bi ] bi@ ;

: show-random-shuffle ( -- )
    "Random shuffle:" print
    51 52 [ n! ] bi@ [a,b] random factoradic shoe findex print ;

: main ( -- ) show-table show-shuffles show-random-shuffle ;

MAIN: main
```

```txt

Generate table
0.0.0 -> 0123
0.0.1 -> 0132
0.1.0 -> 0213
0.1.1 -> 0231
0.2.0 -> 0312
0.2.1 -> 0321
1.0.0 -> 1023
1.0.1 -> 1032
1.1.0 -> 1203
1.1.1 -> 1230
1.2.0 -> 1302
1.2.1 -> 1320
2.0.0 -> 2013
2.0.1 -> 2031
2.1.0 -> 2103
2.1.1 -> 2130
2.2.0 -> 2301
2.2.1 -> 2310
3.0.0 -> 3012
3.0.1 -> 3021
3.1.0 -> 3102
3.1.1 -> 3120
3.2.0 -> 3201
3.2.1 -> 3210

Generate given task shuffles
Original deck:
A♠K♠Q♠J♠10♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥10♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦10♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣10♣9♣8♣7♣6♣5♣4♣3♣2♣

39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
A♣3♣7♠4♣10♦8♦Q♠K♥2♠10♠4♦7♣J♣5♥10♥10♣K♣2♣3♥5♦J♠6♠Q♣5♠K♠A♦3♦Q♥8♣6♦9♠8♠4♠9♥A♠6♥5♣2♦7♥8♥9♣6♣7♦A♥J♦Q♦9♦2♥3♠J♥4♥K♦

51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
2♣5♣J♥4♥J♠A♠5♥A♣6♦Q♠9♣3♦Q♥J♣10♥K♣10♣5♦7♥10♦3♠8♥10♠7♠6♥5♠K♥4♦A♥4♣2♥9♦Q♣8♣7♦6♣3♥6♠7♣2♦J♦9♥A♦Q♦8♦4♠K♦K♠3♣2♠8♠9♠

Random shuffle:
5♠K♣K♠4♣8♥7♠Q♥J♦3♠A♦3♣8♣6♥A♥3♥A♣10♥9♠10♣5♣J♣J♠J♥2♣K♥Q♦Q♣7♣6♦7♥2♥5♥2♠10♦2♦A♠4♦8♠4♠7♦10♠6♣9♣5♦4♥8♦9♦3♦6♠K♦9♥Q♠

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "strconv"
    "strings"
    "time"
)

func factorial(n int) int {
    fact := 1
    for i := 2; i <= n; i++ {
        fact *= i
    }
    return fact
}

func genFactBaseNums(size int, countOnly bool) ([][]int, int) {
    var results [][]int
    count := 0
    for n := 0; ; n++ {
        radix := 2
        var res []int = nil
        if !countOnly { 
            res = make([]int, size)
        }
        k := n
        for k > 0 {
            div := k / radix
            rem := k % radix
            if !countOnly {
                if radix <= size+1 {
                    res[size-radix+1] = rem
                }
            }
            k = div
            radix++
        }
        if radix > size+2 {
            break
        }
        count++
        if !countOnly {
            results = append(results, res)
        }
    }
    return results, count
}

func mapToPerms(factNums [][]int) [][]int {
    var perms [][]int
    psize := len(factNums[0]) + 1
    start := make([]int, psize)
    for i := 0; i < psize; i++ {
        start[i] = i
    }
    for _, fn := range factNums {
        perm := make([]int, psize)
        copy(perm, start)
        for m := 0; m < len(fn); m++ {
            g := fn[m]
            if g == 0 {
                continue
            }
            first := m
            last := m + g
            for i := 1; i <= g; i++ {
                temp := perm[first]
                for j := first + 1; j <= last; j++ {
                    perm[j-1] = perm[j]
                }
                perm[last] = temp
            }
        }
        perms = append(perms, perm)
    }
    return perms
}

func join(is []int, sep string) string {
    ss := make([]string, len(is))
    for i := 0; i < len(is); i++ {
        ss[i] = strconv.Itoa(is[i])
    }
    return strings.Join(ss, sep)
}

func undot(s string) []int {
    ss := strings.Split(s, ".")
    is := make([]int, len(ss))
    for i := 0; i < len(ss); i++ {
        is[i], _ = strconv.Atoi(ss[i])
    }
    return is
}

func main() {
    rand.Seed(time.Now().UnixNano())

    // Recreate the table.
    factNums, _ := genFactBaseNums(3, false)
    perms := mapToPerms(factNums)
    for i, fn := range factNums {
        fmt.Printf("%v -> %v\n", join(fn, "."), join(perms[i], ""))
    }

    // Check that the number of perms generated is equal to 11! (this takes a while).
    _, count := genFactBaseNums(10, true)
    fmt.Println("\nPermutations generated =", count)
    fmt.Println("compared to 11! which  =", factorial(11))
    fmt.Println()

    // Generate shuffles for the 2 given 51 digit factorial base numbers.
    fbn51s := []string{
        "39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0",
        "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1",
    }
    factNums = [][]int{undot(fbn51s[0]), undot(fbn51s[1])}
    perms = mapToPerms(factNums)
    shoe := []rune("A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣")
    cards := make([]string, 52)
    for i := 0; i < 52; i++ {
        cards[i] = string(shoe[2*i : 2*i+2])
        if cards[i][0] == 'T' {
            cards[i] = "10" + cards[i][1:]
        }
    }
    for i, fbn51 := range fbn51s {
        fmt.Println(fbn51)
        for _, d := range perms[i] {
            fmt.Print(cards[d])
        }
        fmt.Println("\n")
    }

    // Create a random 51 digit factorial base number and produce a shuffle from that.
    fbn51 := make([]int, 51)
    for i := 0; i < 51; i++ {
        fbn51[i] = rand.Intn(52 - i)
    }
    fmt.Println(join(fbn51, "."))
    perms = mapToPerms([][]int{fbn51})
    for _, d := range perms[0] {
        fmt.Print(cards[d])
    }
    fmt.Println()
}
```


Random for Part 4:

```txt

0.0.0 -> 0123
0.0.1 -> 0132
0.1.0 -> 0213
0.1.1 -> 0231
0.2.0 -> 0312
0.2.1 -> 0321
1.0.0 -> 1023
1.0.1 -> 1032
1.1.0 -> 1203
1.1.1 -> 1230
1.2.0 -> 1302
1.2.1 -> 1320
2.0.0 -> 2013
2.0.1 -> 2031
2.1.0 -> 2103
2.1.1 -> 2130
2.2.0 -> 2301
2.2.1 -> 2310
3.0.0 -> 3012
3.0.1 -> 3021
3.1.0 -> 3102
3.1.1 -> 3120
3.2.0 -> 3201
3.2.1 -> 3210

Permutations generated = 39916800
compared to 11! which  = 39916800

39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
A♣3♣7♠4♣10♦8♦Q♠K♥2♠10♠4♦7♣J♣5♥10♥10♣K♣2♣3♥5♦J♠6♠Q♣5♠K♠A♦3♦Q♥8♣6♦9♠8♠4♠9♥A♠6♥5♣2♦7♥8♥9♣6♣7♦A♥J♦Q♦9♦2♥3♠J♥4♥K♦

51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
2♣5♣J♥4♥J♠A♠5♥A♣6♦Q♠9♣3♦Q♥J♣10♥K♣10♣5♦7♥10♦3♠8♥10♠7♠6♥5♠K♥4♦A♥4♣2♥9♦Q♣8♣7♦6♣3♥6♠7♣2♦J♦9♥A♦Q♦8♦4♠K♦K♠3♣2♠8♠9♠

18.14.25.48.18.9.1.16.15.11.41.8.26.19.36.11.8.21.20.15.15.14.27.10.5.24.0.11.18.12.6.8.5.14.16.10.13.13.9.7.11.1.1.7.0.2.5.0.3.0.0
9♥K♥K♦2♣7♥5♠K♠6♥8♥A♥3♣4♠4♦J♦5♣J♥3♠6♦7♦A♦Q♦2♥7♣10♥8♠8♣A♠10♦Q♣8♦2♠4♥6♠J♣6♣3♦10♣9♣5♦3♥4♣J♠10♠A♣Q♠Q♥K♣9♠2♦7♠5♥9♦

```




## J

Generalized base and antibase, and anagrams are j verbs making this project directly solvable.

```txt

   NB. A is a numerical matrix corresponding to the input and output
   A =: _&".;._2[0 :0
       0 0 0 0123
       0 0 1 0132
       0 1 0 0213
       0 1 1 0231
       0 2 0 0312
       0 2 1 0321
       1 0 0 1023
       1 0 1 1032
       1 1 0 1203
       1 1 1 1230
       1 2 0 1302
       1 2 1 1320
       2 0 0 2013
       2 0 1 2031
       2 1 0 2103
       2 1 1 2130
       2 2 0 2301
       2 2 1 2310
       3 0 0 3012
       3 0 1 3021
       3 1 0 3102
       3 1 1 3120
       3 2 0 3201
       3 2 1 3210
)

   NB. generalized antibase converts the factorial base representation to integers
   4 3 2 #. _ 3 {. A
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23


   EXPECT =: 10 10 10 10#:{:"1 A

   NB. the 0 through 23 anagrams of 0 1 2 3 matches our expactation
   EXPECT -: (4 3 2 #. _ 3 {. A) A. 0 1 2 3
1
   
   NB. 6 take EXPECT, for you to see what's been matched
   6{.EXPECT
0 1 2 3
0 1 3 2
0 2 1 3 
0 2 3 1
0 3 1 2
0 3 2 1
   

```



## Julia


```julia
function makefactorialbased(N, makelist)
    listlist = Vector{Vector{Int}}()
    count = 0
    while true
        divisor = 2
        makelist && (lis = zeros(Int, N))
        k = count
        while k > 0
            k, r = divrem(k, divisor)
            makelist && (divisor <= N + 1) && (lis[N - divisor + 2] = r)
            divisor += 1
        end
        if divisor > N + 2
            break
        end
        count += 1
        makelist && push!(listlist, lis)
    end
    return count, listlist
end

function facmap(factnumbase)
    perm = [i for i in 0:length(factnumbase)]
    for (n, g) in enumerate(factnumbase)
        if g != 0
            perm[n:n + g] .= circshift(perm[n:n + g], 1)
        end
    end
    perm
end

function factbasenums()
    fcount, factnums = makefactorialbased(3, true)
    perms = map(facmap, factnums)
    for (i, fn) = enumerate(factnums)
        println("$(join(string.(fn), ".")) -> $(join(string(perms[i]), ""))")
    end

    fcount, _ = makefactorialbased(10, false)
    println("\nPermutations generated = $fcount, and 11! = $(factorial(11))\n")

    taskrandom = ["39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0",
        "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1"]
    perms = map(s -> facmap([parse(Int, s) for s in split(s, ".")]), taskrandom)

    cardshoe = split("A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣", "")
    cards = [cardshoe[2*i+1] * cardshoe[2*i+2] for i in 0:51]
    printcardshuffle(t, c, o) = (println(t); for i in 1:length(o) print(c[o[i] + 1]) end; println())
    
    println("\nTask shuffles:")
    map(i -> printcardshuffle(taskrandom[i], cards, perms[i]), 1:2)

    myran = [rand(collect(0:i)) for i in 51:-1:1]
    perm = facmap(myran)
    println("\nMy random shuffle:")
    printcardshuffle(join(string.(myran), "."), cards, perm)
end

factbasenums()

```
```txt

 0.0.0 -> [0, 1, 2, 3]
 0.0.1 -> [0, 1, 3, 2]
 0.1.0 -> [0, 2, 1, 3]
 0.1.1 -> [0, 2, 3, 1]
 0.2.0 -> [0, 3, 1, 2]
 0.2.1 -> [0, 3, 2, 1]
 1.0.0 -> [1, 0, 2, 3]
 1.0.1 -> [1, 0, 3, 2]
 1.1.0 -> [1, 2, 0, 3]
 1.1.1 -> [1, 2, 3, 0]
 1.2.0 -> [1, 3, 0, 2]
 1.2.1 -> [1, 3, 2, 0]
 2.0.0 -> [2, 0, 1, 3]
 2.0.1 -> [2, 0, 3, 1]
 2.1.0 -> [2, 1, 0, 3]
 2.1.1 -> [2, 1, 3, 0]
 2.2.0 -> [2, 3, 0, 1]
 2.2.1 -> [2, 3, 1, 0]
 3.0.0 -> [3, 0, 1, 2]
 3.0.1 -> [3, 0, 2, 1]
 3.1.0 -> [3, 1, 0, 2]
 3.1.1 -> [3, 1, 2, 0]
 3.2.0 -> [3, 2, 0, 1]
 3.2.1 -> [3, 2, 1, 0]
 
 Permutations generated = 39916800, and 11! = 39916800
 
 Task shuffles:
 39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
 A♣3♣7♠4♣T♦8♦Q♠K♥2♠T♠4♦7♣J♣5♥T♥T♣K♣2♣3♥5♦J♠6♠Q♣5♠K♠A♦3♦Q♥8♣6♦9♠8♠4♠9♥A♠6♥5♣2♦7♥8♥9♣6♣7♦A♥J♦Q♦9♦2♥3♠J♥4♥K♦
 51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
 2♣5♣J♥4♥J♠A♠5♥A♣6♦Q♠9♣3♦Q♥J♣T♥K♣T♣5♦7♥T♦3♠8♥T♠7♠6♥5♠K♥4♦A♥4♣2♥9♦Q♣8♣7♦6♣3♥6♠7♣2♦J♦9♥A♦Q♦8♦4♠K♦K♠3♣2♠8♠9♠
 
 My random shuffle:
 29.18.33.22.0.5.10.1.12.18.3.7.39.7.5.12.8.16.28.4.19.18.19.12.4.15.22.3.13.0.5.16.2.16.0.5.10.11.7.0.2.8.9.4.1.6.0.4.1.0.0
 J♦9♥5♦4♥A♠8♠2♠Q♠J♥2♥9♠3♠2♣A♥5♠5♥T♥9♦8♣6♠3♦4♦A♣K♦4♠6♦6♣7♠7♦K♠7♥9♣K♥5♣J♠A♦Q♣T♣8♦T♠6♥7♣3♣T♦8♥4♣Q♥J♣Q♦3♥2♦K♣

```



## Perl

```perl
use strict;
use warnings;
use feature 'say';

sub fpermute {
    my($f,@a) = @_;
    my @f = split /\./, $f;
    for (0..$#f) {
        my @b = @a[$_ ..  $_+$f[$_]];
        unshift @b, splice @b, $#b, 1; # rotate(-1)
        @a[$_ .. $_+$f[$_]] = @b;
    }
    join '', @a;
}

sub base {
    my($n) = @_;
    my @digits;
    push(@digits, int $n/$_) and $n = $n % $_ for <6 2 1>; # reverse <1! 2! 3!>
    join '.', @digits;
}

say 'Generate table';

for (0..23) {
    my $x = base($_);
    say $x . ' -> ' . fpermute($x, <0 1 2 3>)
}

say "\nGenerate the given task shuffles";
my @omega = qw<A♠ K♠ Q♠ J♠ 10♠ 9♠ 8♠ 7♠ 6♠ 5♠ 4♠ 3♠ 2♠ A♥ K♥ Q♥ J♥ 10♥ 9♥ 8♥ 7♥ 6♥ 5♥ 4♥ 3♥ 2♥ A♦ K♦ Q♦ J♦ 10♦ 9♦ 8♦ 7♦ 6♦ 5♦ 4♦ 3♦ 2♦ A♣ K♣ Q♣ J♣ 10♣ 9♣ 8♣ 7♣ 6♣ 5♣ 4♣ 3♣ 2♣>;

my @books = (
'39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0',
'51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1'
);

say "Original deck:";
say join '', @omega;

say "\n$_\n" . fpermute($_,@omega) for @books;

say "\nGenerate a random shuffle";
say my $shoe = join '.', map { int rand($_) } reverse 0..$#omega;
say fpermute($shoe,@omega);
```

```txt
Generate table
0.0.0 -> 0123
0.0.1 -> 0132
0.1.0 -> 0213
0.1.1 -> 0231
0.2.0 -> 0312
0.2.1 -> 0321
1.0.0 -> 1023
1.0.1 -> 1032
1.1.0 -> 1203
1.1.1 -> 1230
1.2.0 -> 1302
1.2.1 -> 1320
2.0.0 -> 2013
2.0.1 -> 2031
2.1.0 -> 2103
2.1.1 -> 2130
2.2.0 -> 2301
2.2.1 -> 2310
3.0.0 -> 3012
3.0.1 -> 3021
3.1.0 -> 3102
3.1.1 -> 3120
3.2.0 -> 3201
3.2.1 -> 3210

Generate the given task shuffles
Original deck:
A♠K♠Q♠J♠10♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥10♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦10♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣10♣9♣8♣7♣6♣5♣4♣3♣2♣

39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
A♣3♣7♠4♣10♦8♦Q♠K♥2♠10♠4♦7♣J♣5♥10♥10♣K♣2♣3♥5♦J♠6♠Q♣5♠K♠A♦3♦Q♥8♣6♦9♠8♠4♠9♥A♠6♥5♣2♦7♥8♥9♣6♣7♦A♥J♦Q♦9♦2♥3♠J♥4♥K♦

51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
2♣5♣J♥4♥J♠A♠5♥A♣6♦Q♠9♣3♦Q♥J♣10♥K♣10♣5♦7♥10♦3♠8♥10♠7♠6♥5♠K♥4♦A♥4♣2♥9♦Q♣8♣7♦6♣3♥6♠7♣2♦J♦9♥A♦Q♦8♦4♠K♦K♠3♣2♠8♠9♠

Generate a random shuffle
47.33.30.9.2.13.9.43.23.40.11.15.15.1.2.11.23.5.21.30.30.14.27.26.7.20.24.13.10.21.1.10.9.2.10.0.12.11.11.11.10.8.2.0.3.4.4.2.0.1.0.0
6♣7♦10♦5♠Q♠Q♥3♠3♣K♦5♣K♥7♥6♥K♠10♠9♥4♦6♠5♦7♣4♣2♥9♣10♣A♥2♦8♣A♦5♥J♣J♠3♥4♥8♠9♦A♠A♣3♦K♣Q♣6♦J♦4♠9♠10♥Q♦8♦J♥7♠8♥2♠2♣
```



## Perl 6

Using my interpretation of the task instructions as shown on the [http://rosettacode.org/wiki/Talk:Factorial_base_numbers_indexing_permutations_of_a_collection#Mojibake_and_misspellings discussion page].


```perl6
sub postfix:<!> (Int $n) { (flat 1, [\*] 1..*)[$n] }

multi base (Int $n is copy, 'F', $length? is copy) {
    constant @fact = [\*] 1 .. *;
    my $i = $length // @fact.first: * > $n, :k;
    my $f;
    [ @fact[^$i].reverse.map: { ($n, $f) = $n.polymod($_); $f } ]
}

sub fpermute (@a is copy, *@f) { (^@f).map: { @a[$_ .. $_ + @f[$_]].=rotate(-1) }; @a }

put "Part 1: Generate table";
put $_.&base('F', 3).join('.') ~ ' -> ' ~ [0,1,2,3].&fpermute($_.&base('F', 3)).join for ^24;

put "\nPart 2: Compare 11! to 11! " ~ '¯\_(ツ)_/¯';
# This is kind of a weird request. Since we don't actually need to _generate_
# the permutations, only _count_ them: compare count of 11! vs count of 11!
put "11! === 11! : {11! === 11!}";

put "\nPart 3: Generate the given task shuffles";
my \Ω = <A♠ K♠ Q♠ J♠ 10♠ 9♠ 8♠ 7♠ 6♠ 5♠ 4♠ 3♠ 2♠ A♥ K♥ Q♥ J♥ 10♥ 9♥ 8♥ 7♥ 6♥ 5♥ 4♥ 3♥ 2♥
         A♦ K♦ Q♦ J♦ 10♦ 9♦ 8♦ 7♦ 6♦ 5♦ 4♦ 3♦ 2♦ A♣ K♣ Q♣ J♣ 10♣ 9♣ 8♣ 7♣ 6♣ 5♣ 4♣ 3♣ 2♣
>;

my @books = <
    39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
    51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
>;

put "Original deck:";
put Ω.join;

put "\n$_\n" ~ Ω[(^Ω).&fpermute($_.split: '.')].join for @books;

put "\nPart 4: Generate a random shuffle";
my @shoe = (+Ω … 2).map: { (^$_).pick };
put @shoe.join('.');
put Ω[(^Ω).&fpermute(@shoe)].join;

put "\nSeems to me it would be easier to just say: Ω.pick(*).join";
put Ω.pick(*).join;
```

```txt
Part 1: Generate table
0.0.0 -> 0123
0.0.1 -> 0132
0.1.0 -> 0213
0.1.1 -> 0231
0.2.0 -> 0312
0.2.1 -> 0321
1.0.0 -> 1023
1.0.1 -> 1032
1.1.0 -> 1203
1.1.1 -> 1230
1.2.0 -> 1302
1.2.1 -> 1320
2.0.0 -> 2013
2.0.1 -> 2031
2.1.0 -> 2103
2.1.1 -> 2130
2.2.0 -> 2301
2.2.1 -> 2310
3.0.0 -> 3012
3.0.1 -> 3021
3.1.0 -> 3102
3.1.1 -> 3120
3.2.0 -> 3201
3.2.1 -> 3210

Part 2: Compare 11! to 11! ¯\_(ツ)_/¯
11! === 11! : True

Part 3: Generate the given task shuffles
Original deck:
A♠K♠Q♠J♠10♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥10♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦10♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣10♣9♣8♣7♣6♣5♣4♣3♣2♣

39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
A♣3♣7♠4♣10♦8♦Q♠K♥2♠10♠4♦7♣J♣5♥10♥10♣K♣2♣3♥5♦J♠6♠Q♣5♠K♠A♦3♦Q♥8♣6♦9♠8♠4♠9♥A♠6♥5♣2♦7♥8♥9♣6♣7♦A♥J♦Q♦9♦2♥3♠J♥4♥K♦

51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
2♣5♣J♥4♥J♠A♠5♥A♣6♦Q♠9♣3♦Q♥J♣10♥K♣10♣5♦7♥10♦3♠8♥10♠7♠6♥5♠K♥4♦A♥4♣2♥9♦Q♣8♣7♦6♣3♥6♠7♣2♦J♦9♥A♦Q♦8♦4♠K♦K♠3♣2♠8♠9♠

Part 4: Generate a random shuffle
47.9.46.16.28.8.36.27.29.1.9.27.1.16.21.22.28.34.30.8.19.27.18.22.3.25.15.20.12.14.8.9.11.1.4.0.3.5.4.2.2.10.8.1.6.1.2.4.1.2.1
6♣5♠5♣10♥10♦6♠K♣9♦6♦K♠2♠5♦Q♠5♥Q♦8♦J♣2♣8♣A♥K♦9♣A♦2♦9♠4♣3♥A♣7♥2♥Q♥9♥4♥J♠4♠A♠3♠8♥J♥7♠K♥3♣10♣8♠Q♣6♥7♦7♣J♦3♦4♦10♠

Seems to me it would be easier to just say: Ω.pick(*).join
5♦3♠8♦10♦2♥7♠7♦Q♦A♠5♣8♣Q♠4♠2♦K♦5♠Q♥7♣10♠2♠K♠J♣9♣3♣4♥3♥4♦3♦Q♣2♣4♣J♦9♠A♣J♠10♣6♣9♦6♠10♥6♥9♥J♥7♥K♥A♦8♠A♥5♥8♥K♣6♦
```



## Phix


```Phix
function fperm(sequence fbn, omega)
integer m=0
    for i=1 to length(fbn) do
        integer g = fbn[i]
        if g>0 then
            omega[m+1..m+g+1] = omega[m+g+1]&omega[m+1..m+g]
        end if
        m += 1
    end for
    return omega
end function

function factorial_base_numbers(integer size, bool countOnly)
-- translation of Go
    sequence results = {}, res = repeat(0,size)
    integer count = 0, n = 0
    while true do
        integer radix = 2, k = n
        while k>0 do
            if not countOnly
            and radix <= size+1 then
                res[size-radix+2] = mod(k,radix)
            end if
            k = floor(k/radix)
            radix += 1
        end while
        if radix > size+2 then exit end if
        count += 1
        if not countOnly then
            results = append(results, res)
        end if
        n += 1
    end while
    return iff(countOnly?count:results)
end function

sequence fbns = factorial_base_numbers(3,false)
for i=1 to length(fbns) do
    printf(1,"%v -> %v\n",{fbns[i],fperm(fbns[i],{0,1,2,3})})
end for
printf(1,"\n")

integer count = factorial_base_numbers(10,true)
printf(1,"Permutations generated = %d\n", count)
printf(1,"  versus factorial(11) = %d\n", factorial(11))

procedure show_cards(sequence s)
    printf(1,"\n")
    for i=1 to length(s) do
        integer c = s[i]-1
        string sep = iff(mod(i,13)=0 or i=length(s)?"\n":" ")
        puts(1,"AKQJT98765432"[mod(c,13)+1]&"SHDC"[floor(c/13)+1]&sep)
    end for
end procedure

function rand_fbn51()
    sequence fbn51 = repeat(0,51)
    for i=1 to 51 do
        fbn51[i] = rand(52-i)
    end for
    return fbn51
end function

sequence fbn51s = {{39,49, 7,47,29,30, 2,12,10, 3,29,37,33,17,12,31,29,
                    34,17,25, 2, 4,25, 4, 1,14,20, 6,21,18, 1, 1, 1, 4,
                     0, 5,15,12, 4, 3,10,10, 9, 1, 6, 5, 5, 3, 0, 0, 0},
                   {51,48,16,22, 3, 0,19,34,29, 1,36,30,12,32,12,29,30,
                    26,14,21, 8,12, 1, 3,10, 4, 7,17, 6,21, 8,12,15,15,
                    13,15, 7, 3,12,11, 9, 5, 5, 6, 6, 3, 4, 0, 3, 2, 1},
                   rand_fbn51()}
for i=1 to length(fbn51s) do
    show_cards(fperm(fbn51s[i],tagset(52)))
end for
```

```txt

{0,0,0} -> {0,1,2,3}
{0,0,1} -> {0,1,3,2}
{0,1,0} -> {0,2,1,3}
{0,1,1} -> {0,2,3,1}
{0,2,0} -> {0,3,1,2}
{0,2,1} -> {0,3,2,1}
{1,0,0} -> {1,0,2,3}
{1,0,1} -> {1,0,3,2}
{1,1,0} -> {1,2,0,3}
{1,1,1} -> {1,2,3,0}
{1,2,0} -> {1,3,0,2}
{1,2,1} -> {1,3,2,0}
{2,0,0} -> {2,0,1,3}
{2,0,1} -> {2,0,3,1}
{2,1,0} -> {2,1,0,3}
{2,1,1} -> {2,1,3,0}
{2,2,0} -> {2,3,0,1}
{2,2,1} -> {2,3,1,0}
{3,0,0} -> {3,0,1,2}
{3,0,1} -> {3,0,2,1}
{3,1,0} -> {3,1,0,2}
{3,1,1} -> {3,1,2,0}
{3,2,0} -> {3,2,0,1}
{3,2,1} -> {3,2,1,0}

Permutations generated = 39916800
  versus factorial(11) = 39916800

AC 3C 7S 4C TD 8D QS KH 2S TS 4D 7C JC
5H TH TC KC 2C 3H 5D JS 6S QC 5S KS AD
3D QH 8C 6D 9S 8S 4S 9H AS 6H 5C 2D 7H
8H 9C 6C 7D AH JD QD 9D 2H 3S JH 4H KD

2C 5C JH 4H JS AS 5H AC 6D QS 9C 3D QH
JC TH KC TC 5D 7H TD 3S 8H TS 7S 6H 5S
KH 4D AH 4C 2H 9D QC 8C 7D 6C 3H 6S 7C
2D JD 9H AD QD 8D 4S KD KS 3C 2S 8S 9S

JS 4H JD 9H 2C 9C 3C KH 9S TH 6D 5S 3H
2H 3S JH 5H QD 4C 7D 4S QC 7C TS 5C 6H
KS 5D QH 2S AD AC 7S QS TC JC 7H 6C 8H
KC 9D 4D 8D KD 6S TD AH 8C 2D 8S 3D AS

```


## Python


```python

"""

http://rosettacode.org/wiki/Factorial_base_numbers_indexing_permutations_of_a_collection

https://en.wikipedia.org/wiki/Factorial_number_system

"""

import math

def apply_perm(omega,fbn):
    """
    
    omega contains a list which will be permuted (scrambled)
    based on fbm.
    
    fbm is a list which represents a factorial base number.
    
    This function just translates the pseudo code in the 
    Rosetta Code task.
    
    """
    for m in range(len(fbn)):
        g = fbn[m]
        if g > 0:
            # do rotation
            # save last number
            new_first = omega[m+g]
            # move numbers right
            omega[m+1:m+g+1] = omega[m:m+g]
            # put last number first
            omega[m] = new_first
            
    return omega
    
def int_to_fbn(i):
    """
    
    convert integer i to factorial based number
    
    """
    current = i
    divisor = 2
    new_fbn = []
    while current > 0:
        remainder = current % divisor
        current = current // divisor
        new_fbn.append(remainder)
        divisor += 1
    
    return list(reversed(new_fbn))
    
def leading_zeros(l,n):
   """
   
   If list l has less than n elements returns l with enough 0 elements
   in front of the list to make it length n.
   
   """
   if len(l) < n:
       return(([0] * (n - len(l))) + l)
   else:
       return l

def get_fbn(n):
    """
    
    Return the n! + 1 first Factorial Based Numbers starting with zero.
            
    """
    max = math.factorial(n)
    
    for i in range(max):
        # from Wikipedia article
        current = i
        divisor = 1
        new_fbn = int_to_fbn(i)
        yield leading_zeros(new_fbn,n-1)
        
def print_write(f, line):
    """

    prints to console and
    output file f

    """
    print(line)
    f.write(str(line)+'\n')     
    
def dot_format(l):
    """
    Take a list l that is a factorial based number
    and returns it in dot format.
    
    i.e. [0, 2, 1] becomes 0.2.1
    """
    # empty list
    if len(l) < 1:
        return ""
    # start with just first element no dot
    dot_string = str(l[0])
    # add rest if any with dots
    for e in l[1:]:
        dot_string += "."+str(e)
        
    return dot_string
    
def str_format(l):
    """
    Take a list l and returns a string
    of those elements converted to strings.
    """
    if len(l) < 1:
        return ""
        
    new_string = ""
        
    for e in l:
        new_string += str(e)
    
    return new_string 
    
with open("output.html", "w", encoding="utf-8") as f:
    f.write("
```txt
\n")
    
    # first print list
        
    omega=[0,1,2,3]
    
    four_list = get_fbn(4)
    
    for l in four_list:
        print_write(f,dot_format(l)+' -> '+str_format(apply_perm(omega[:],l)))
        
    print_write(f," ")
    
    # now generate this output:
    #
    # Permutations generated = 39916800
    # compared to 11! which  = 39916800
    
        
    num_permutations = 0
    
    for p in get_fbn(11):
        num_permutations += 1
        if num_permutations % 1000000 == 0:
            print_write(f,"permutations so far = "+str(num_permutations))
    
    print_write(f," ")
    print_write(f,"Permutations generated = "+str(num_permutations))
    print_write(f,"compared to 11! which  = "+str(math.factorial(11)))
    
    print_write(f," ")
    
       
    
    """
    
    u"\u2660" - spade
    
    u"\u2665" - heart
    
    u"\u2666" - diamond
    
    u"\u2663" - club
        
    """
    
    shoe = []
    
    for suit in [u"\u2660",u"\u2665",u"\u2666",u"\u2663"]:
        for value in ['A','K','Q','J','10','9','8','7','6','5','4','3','2']:
            shoe.append(value+suit)
                    
    print_write(f,str_format(shoe))
    
    p1 = [39,49,7,47,29,30,2,12,10,3,29,37,33,17,12,31,29,34,17,25,2,4,25,4,1,14,20,6,21,18,1,1,1,4,0,5,15,12,4,3,10,10,9,1,6,5,5,3,0,0,0]
    
    p2 = [51,48,16,22,3,0,19,34,29,1,36,30,12,32,12,29,30,26,14,21,8,12,1,3,10,4,7,17,6,21,8,12,15,15,13,15,7,3,12,11,9,5,5,6,6,3,4,0,3,2,1]
    
    print_write(f," ")
    print_write(f,dot_format(p1))
    print_write(f," ")
    print_write(f,str_format(apply_perm(shoe[:],p1)))
    
    print_write(f," ")
    print_write(f,dot_format(p2))
    print_write(f," ")
    print_write(f,str_format(apply_perm(shoe[:],p2)))

    # generate random 51 digit factorial based number
    
    import random
    
    max = math.factorial(52)
    
    random_int = random.randint(0, max-1)

    myperm = leading_zeros(int_to_fbn(random_int),51)
    
    print(len(myperm))
    
    print_write(f," ")
    print_write(f,dot_format(myperm))
    print_write(f," ")
    print_write(f,str_format(apply_perm(shoe[:],myperm)))

    f.write("
```
\n")


```

```txt

0.0.0 -> 0123
0.0.1 -> 0132
0.1.0 -> 0213
0.1.1 -> 0231
0.2.0 -> 0312
0.2.1 -> 0321
1.0.0 -> 1023
1.0.1 -> 1032
1.1.0 -> 1203
1.1.1 -> 1230
1.2.0 -> 1302
1.2.1 -> 1320
2.0.0 -> 2013
2.0.1 -> 2031
2.1.0 -> 2103
2.1.1 -> 2130
2.2.0 -> 2301
2.2.1 -> 2310
3.0.0 -> 3012
3.0.1 -> 3021
3.1.0 -> 3102
3.1.1 -> 3120
3.2.0 -> 3201
3.2.1 -> 3210
 
permutations so far = 1000000
permutations so far = 2000000
permutations so far = 3000000
permutations so far = 4000000
permutations so far = 5000000
permutations so far = 6000000
permutations so far = 7000000
permutations so far = 8000000
permutations so far = 9000000
permutations so far = 10000000
permutations so far = 11000000
permutations so far = 12000000
permutations so far = 13000000
permutations so far = 14000000
permutations so far = 15000000
permutations so far = 16000000
permutations so far = 17000000
permutations so far = 18000000
permutations so far = 19000000
permutations so far = 20000000
permutations so far = 21000000
permutations so far = 22000000
permutations so far = 23000000
permutations so far = 24000000
permutations so far = 25000000
permutations so far = 26000000
permutations so far = 27000000
permutations so far = 28000000
permutations so far = 29000000
permutations so far = 30000000
permutations so far = 31000000
permutations so far = 32000000
permutations so far = 33000000
permutations so far = 34000000
permutations so far = 35000000
permutations so far = 36000000
permutations so far = 37000000
permutations so far = 38000000
permutations so far = 39000000
 
Permutations generated = 39916800
compared to 11! which  = 39916800
 
A♠K♠Q♠J♠10♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥10♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦10♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣10♣9♣8♣7♣6♣5♣4♣3♣2♣
 
39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
 
A♣3♣7♠4♣10♦8♦Q♠K♥2♠10♠4♦7♣J♣5♥10♥10♣K♣2♣3♥5♦J♠6♠Q♣5♠K♠A♦3♦Q♥8♣6♦9♠8♠4♠9♥A♠6♥5♣2♦7♥8♥9♣6♣7♦A♥J♦Q♦9♦2♥3♠J♥4♥K♦
 
51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1
 
2♣5♣J♥4♥J♠A♠5♥A♣6♦Q♠9♣3♦Q♥J♣10♥K♣10♣5♦7♥10♦3♠8♥10♠7♠6♥5♠K♥4♦A♥4♣2♥9♦Q♣8♣7♦6♣3♥6♠7♣2♦J♦9♥A♦Q♦8♦4♠K♦K♠3♣2♠8♠9♠
 
35.21.48.20.4.1.5.41.3.1.25.34.30.4.25.1.11.29.28.4.5.29.17.28.0.18.14.19.20.1.1.4.19.6.2.6.9.5.8.3.10.10.7.1.7.3.0.3.2.0.0
 
5♦6♥3♣7♥10♠K♠7♠6♣9♠Q♠8♦10♣A♣5♠6♦J♠9♥9♣J♣3♠A♥4♣J♦2♣A♠7♦K♦2♦Q♣6♠4♠J♥5♣5♥K♥3♥10♦4♥9♦10♥8♣7♣4♦2♠K♣2♥8♠Q♦A♦Q♥8♥3♦

```



## zkl


```zkl
fcn fpermute(omega,num){  // eg (0,1,2,3), (0,0,0)..(3,2,1)
   omega=omega.copy(); 	  // omega gonna be mutated
   foreach m,g in ([0..].zip(num)){ if(g) omega.insert(m,omega.pop(m+g)) }
   omega
}
```


```zkl
foreach a,b,c in (4,3,2){
   println("%d.%d.%d --> %s".fmt(a,b,c, fpermute(T(0,1,2,3),T(a,b,c)).concat()));
}
```

```txt
0.0.0 --> 0123
0.0.1 --> 0132
0.1.0 --> 0213
0.1.1 --> 0231
0.2.0 --> 0312
0.2.1 --> 0321
1.0.0 --> 1023
1.0.1 --> 1032
1.1.0 --> 1203
1.1.1 --> 1230
1.2.0 --> 1302
1.2.1 --> 1320
2.0.0 --> 2013
2.0.1 --> 2031
2.1.0 --> 2103
2.1.1 --> 2130
2.2.0 --> 2301
2.2.1 --> 2310
3.0.0 --> 3012
3.0.1 --> 3021
3.1.0 --> 3102
3.1.1 --> 3120
3.2.0 --> 3201
3.2.1 --> 3210
```


```zkl
deck:=List();
foreach s,c in ("\u2660 \u2665 \u2666 \u2663".split(), 
                "A K Q J 10 9 8 7 6 5 4 3 2".split()){ deck.append(c+s) }
books:=List(
   "39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0",
   "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1")
   .apply(fcn(s){ s.split(".").apply("toInt") });
foreach book in (books){ println(fpermute(deck,book).concat("")); }
```

```txt

A♣3♣7♠4♣10♦8♦Q♠K♥2♠10♠4♦7♣J♣5♥10♥10♣K♣2♣3♥5♦J♠6♠Q♣5♠K♠A♦3♦Q♥8♣6♦9♠8♠4♠9♥A♠6♥5♣2♦7♥8♥9♣6♣7♦A♥J♦Q♦9♦2♥3♠J♥4♥K♦
2♣5♣J♥4♥J♠A♠5♥A♣6♦Q♠9♣3♦Q♥J♣10♥K♣10♣5♦7♥10♦3♠8♥10♠7♠6♥5♠K♥4♦A♥4♣2♥9♦Q♣8♣7♦6♣3♥6♠7♣2♦J♦9♥A♦Q♦8♦4♠K♦K♠3♣2♠8♠9♠

```


```zkl
r:=[52..2,-1].pump(List,(0).random);
println(r.concat("."),"\n",fpermute(deck,r).concat(""));
```

```txt

36.21.48.31.19.37.16.39.43.1.27.23.30.22.14.32.31.2.27.11.5.24.28.20.23.20.17.19.23.13.11.12.3.12.1.0.11.1.8.10.6.2.8.3.7.1.1.4.2.2.1
4♦6♥3♣8♦8♥Q♣J♥8♣2♣K♠9♦K♦2♦A♦Q♥9♣10♣J♠A♣A♥7♠3♦5♣10♦K♣7♦2♥6♦4♣7♥10♥5♥9♠3♥Q♠A♠J♦8♠4♥J♣K♥5♠7♣3♠6♣6♠4♠5♦9♥Q♦2♠10♠

```

