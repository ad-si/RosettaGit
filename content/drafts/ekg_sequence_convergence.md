+++
title = "EKG sequence convergence"
description = ""
date = 2019-04-08T21:57:15Z
aliases = []
[extra]
id = 21942
[taxonomies]
categories = []
tags = []
+++

{{task}}

The sequence is from the natural numbers and is defined by:
* <code>a(1) = 1</code>;
* <code>a(2) = Start = 2</code>;
* for n > 2, <code>a(n)</code> shares at least one prime factor with <code>a(n-1)</code> and is the ''smallest'' such natural number ''not already used''.


The sequence is called the [http://oeis.org/A064740 EKG sequence] (after its visual similarity to an electrocardiogram when graphed).

Variants of the sequence can be generated starting 1, N where N is any natural number larger than one. For the purposes of this task let us call:
* The sequence described above , starting <code>1, 2, ...</code> the <code>EKG(2)</code> sequence;
* the sequence starting <code>1, 3, ...</code> the <code>EKG(3)</code> sequence;
* ... the sequence starting <code>1, N, ...</code> the <code>EKG(N)</code> sequence.


;Convergence
If an algorithm that keeps track of the minimum amount of numbers and their corresponding prime factors used to generate the next term is used, then this may be known as the generators essential '''state'''. Two EKG generators with differing starts can converge to produce the same sequence after initial differences.

<code>EKG(N1)</code> and <code>EKG(N2)</code> are said to to have converged at and after generation <code>a(c)</code> if <code>state_of(EKG(N1).a(c)) == state_of(EKG(N2).a(c))</code>.


;Task:
# Calculate and show here the first 10 members of <code>EKG(2)</code>.
# Calculate and show here the first 10 members of <code>EKG(5)</code>.
# Calculate and show here the first 10 members of <code>EKG(7)</code>.
# Calculate and show here the first 10 members of <code>EKG(9)</code>.
# Calculate and show here the first 10 members of <code>EKG(10)</code>.
# Calculate and show here at which term <code>EKG(5)</code> and <code>EKG(7)</code> converge   ('''stretch goal''').

;Related Tasks:
# [[Greatest common divisor]]
# [[Sieve of Eratosthenes]]


;Reference:
* [https://www.youtube.com/watch?v=yd2jr30K2R4 The EKG Sequence and the Tree of Numbers]. (Video).





## C

{{trans|Go}}

```c
#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0
#define LIMIT 100

typedef int bool;

int compareInts(const void *a, const void *b) {
    int aa = *(int *)a;
    int bb = *(int *)b;
    return aa - bb;
}

bool contains(int a[], int b, size_t len) {
    int i;
    for (i = 0; i < len; ++i) {
        if (a[i] == b) return TRUE;
    }
    return FALSE;
}

int gcd(int a, int b) {
    while (a != b) {
        if (a > b)
            a -= b;
        else
            b -= a;
    }
    return a;
}

bool areSame(int s[], int t[], size_t len) {
    int i;
    qsort(s, len, sizeof(int), compareInts);
    qsort(t, len, sizeof(int), compareInts);
    for (i = 0; i < len; ++i) {
        if (s[i] != t[i]) return FALSE;
    }
    return TRUE;
}

int main() {
    int s, n, i;
    int starts[5] = {2, 5, 7, 9, 10};
    int ekg[5][LIMIT];
    for (s = 0; s < 5; ++s) {
        ekg[s][0] = 1;
        ekg[s][1] = starts[s];
        for (n = 2; n < LIMIT; ++n) {
            for (i = 2; ; ++i) {
                // a potential sequence member cannot already have been used
                // and must have a factor in common with previous member
                if (!contains(ekg[s], i, n) && gcd(ekg[s][n - 1], i) > 1) {
                    ekg[s][n] = i;
                    break;
                }
            }
        }
        printf("EKG(%2d): [", starts[s]);
        for (i = 0; i < 30; ++i) printf("%d ", ekg[s][i]);
        printf("\b]\n");
    }

    // now compare EKG5 and EKG7 for convergence
    for (i = 2; i < LIMIT; ++i) {
        if (ekg[1][i] == ekg[2][i] && areSame(ekg[1], ekg[2], i)) {
            printf("\nEKG(5) and EKG(7) converge at term %d\n", i + 1);
            return 0;
        }
    }
    printf("\nEKG5(5) and EKG(7) do not converge within %d terms\n", LIMIT);
    return 0;
}
```


{{out}}

```txt

EKG( 2): [1 2 4 6 3 9 12 8 10 5 15 18 14 7 21 24 16 20 22 11 33 27 30 25 35 28 26 13 39 36]
EKG( 5): [1 5 10 2 4 6 3 9 12 8 14 7 21 15 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32]
EKG( 7): [1 7 14 2 4 6 3 9 12 8 10 5 15 18 16 20 22 11 33 21 24 26 13 39 27 30 25 35 28 32]
EKG( 9): [1 9 3 6 2 4 8 10 5 15 12 14 7 21 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32]
EKG(10): [1 10 2 4 6 3 9 12 8 14 7 21 15 5 20 16 18 22 11 33 24 26 13 39 27 30 25 35 28 32]

EKG(5) and EKG(7) converge at term 21

```


=={{header|F_Sharp|F#}}==

### The Function

This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Generate EKG Sequences. Nigel Galloway: December 6th., 2018
let EKG            n=seq{
  let fN,fG=let    i=System.Collections.Generic.Dictionary<int,int>()
            let fN g=(if not (i.ContainsKey g) then i.[g]<-g);(g,i.[g])
            ((fun  e->i.[e]<-i.[e]+e), (fun  l->l|>List.map fN))
  let fU           l= pCache|>Seq.takeWhile(fun n->n<=l)|>Seq.filter(fun n->l%n=0)|>List.ofSeq
  let rec EKG l (α,β)=seq{let b=fU β in if (β=n||β<snd((fG b|>List.maxBy snd))) then fN α;        yield! EKG l (fG l|>List.minBy snd)
                                                                                else fN α;yield β;yield! EKG b (fG b|>List.minBy snd)}
  yield! seq[1;n]; let g=fU n in yield! EKG g (fG g|>Seq.minBy snd)}
let EKGconv n g=Seq.zip(EKG n)(EKG g)|>Seq.skip 2|>Seq.scan(fun(n,i,g,e)(l,β)->(Set.add l n,Set.add β i,l,β))(set[1;n],set[1;g],0,0)|>Seq.takeWhile(fun(n,i,g,e)->g<>e||n<>i)

```



### The Task


```fsharp

EKG 2 |> Seq.take 45 |> Seq.iter(printf "%2d, ")
EKG 3 |> Seq.take 45 |> Seq.iter(printf "%2d, ")
EKG 5 |> Seq.take 45 |> Seq.iter(printf "%2d, ")
EKG 7 |> Seq.take 45 |> Seq.iter(printf "%2d, ")
EKG 9 |> Seq.take 45 |> Seq.iter(printf "%2d, ")
EKG 10 |> Seq.take 45 |> Seq.iter(printf "%2d, ")
printfn "%d" (let n,_,_,_=EKGconv 2 5|>Seq.last in ((Set.count n)+1)

```

{{out}}

```txt

 1, 2, 4, 6, 3, 9,12, 8,10, 5,15,18,14, 7,21,24,16,20,22,11,33,27,30,25,35,28,26,13,39,36,32,34,17,51,42,38,19,57,45,40,44,46,23,69,48
 1, 3, 6, 2, 4, 8,10, 5,15, 9,12,14, 7,21,18,16,20,22,11,33,24,26,13,39,27,30,25,35,28,32,34,17,51,36,38,19,57,42,40,44,46,23,69,45,48
 1, 5,10, 2, 4, 6, 3, 9,12, 8,14, 7,21,15,18,16,20,22,11,33,24,26,13,39,27,30,25,35,28,32,34,17,51,36,38,19,57,42,40,44,46,23,69,45,48
45

```


### Extra Credit


```fsharp

prıntfn "%d" (EKG 2|>Seq.takeWhile(fun n->n<>104729) ((Seq.length n)+1)

```

{{out}}

```txt

203786
Real: 00:10:21.967, CPU: 00:10:25.300, GC gen0: 65296, gen1: 1

```



## Go


```go
package main

import (
    "fmt"
    "sort"
)

func contains(a []int, b int) bool {
    for _, j := range a {
        if j == b {
            return true
        }
    }
    return false
}

func gcd(a, b int) int {
    for a != b {
        if a > b {
            a -= b
        } else {
            b -= a
        }
    }
    return a
}

func areSame(s, t []int) bool {
    le := len(s)
    if le != len(t) {
        return false
    }
    sort.Ints(s)
    sort.Ints(t)
    for i := 0; i < le; i++ {
        if s[i] != t[i] {
            return false
        }
    }
    return true
}

func main() {
    const limit = 100
    starts := [5]int{2, 5, 7, 9, 10}
    var ekg [5][limit]int

    for s, start := range starts {
        ekg[s][0] = 1
        ekg[s][1] = start
        for n := 2; n < limit; n++ {
            for i := 2; ; i++ {
                // a potential sequence member cannot already have been used
                // and must have a factor in common with previous member
                if !contains(ekg[s][:n], i) && gcd(ekg[s][n-1], i) > 1 {
                    ekg[s][n] = i
                    break
                }
            }
        }
        fmt.Printf("EKG(%2d): %v\n", start, ekg[s][:30])
    }

    // now compare EKG5 and EKG7 for convergence
    for i := 2; i < limit; i++ {
        if ekg[1][i] == ekg[2][i] && areSame(ekg[1][:i], ekg[2][:i]) {
            fmt.Println("\nEKG(5) and EKG(7) converge at term", i+1)
            return
        }
    }
    fmt.Println("\nEKG5(5) and EKG(7) do not converge within", limit, "terms")
}
```


{{out}}

```txt

EKG( 2): [1 2 4 6 3 9 12 8 10 5 15 18 14 7 21 24 16 20 22 11 33 27 30 25 35 28 26 13 39 36]
EKG( 5): [1 5 10 2 4 6 3 9 12 8 14 7 21 15 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32]
EKG( 7): [1 7 14 2 4 6 3 9 12 8 10 5 15 18 16 20 22 11 33 21 24 26 13 39 27 30 25 35 28 32]
EKG( 9): [1 9 3 6 2 4 8 10 5 15 12 14 7 21 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32]
EKG(10): [1 10 2 4 6 3 9 12 8 14 7 21 15 5 20 16 18 22 11 33 24 26 13 39 27 30 25 35 28 32]

EKG(5) and EKG(7) converge at term 21

```




## Haskell


```Haskell
import Data.List (findIndex, isPrefixOf, tails)
import Data.Maybe (fromJust)

seqEKGRec :: Int -> Int -> [Int] -> [Int]
seqEKGRec _ 0 l = l
seqEKGRec k n [] = seqEKGRec k (n - 2) [k, 1]
seqEKGRec k n l@(h:t) =
  seqEKGRec
    k
    (n - 1)
    (head (filter (\i -> notElem i l && (gcd h i > 1)) [2 ..]) : l)

seqEKG :: Int -> Int -> [Int]
seqEKG k n = reverse (seqEKGRec k n [])

main :: IO ()
main =
  mapM_
    (\x ->
        putStr "EKG (" >> (putStr . show $ x) >> putStr ") is " >>
        print (seqEKG x 20))
    [2, 5, 7, 9, 10] >>
  putStr "EKG(5) and EKG(7) converge at " >>
  print
    ((+ 1) $
     fromJust $
     findIndex
       (isPrefixOf (replicate 20 True))
       (tails (zipWith (==) (seqEKG 7 80) (seqEKG 5 80))))
```

{{out}}

```txt
EKG (2) is [1,2,4,6,3,9,12,8,10,5,15,18,14,7,21,24,16,20,22,11]
EKG (5) is [1,5,10,2,4,6,3,9,12,8,14,7,21,15,18,16,20,22,11,33]
EKG (7) is [1,7,14,2,4,6,3,9,12,8,10,5,15,18,16,20,22,11,33,21]
EKG (9) is [1,9,3,6,2,4,8,10,5,15,12,14,7,21,18,16,20,22,11,33]
EKG (10) is [1,10,2,4,6,3,9,12,8,14,7,21,15,5,20,16,18,22,11,33]
EKG(5) and EKG(7) converge at 21
```



## J


```j

Until =: 2 :'u^:(0-:v)^:_'  NB. unused but so fun
prime_factors_of_tail =: ~.@:q:@:{:
numbers_not_in_list =: -.~ >:@:i.@:(>./)


ekg =: 3 :0                             NB. return next sequence
 if. 1 = # y do. NB. initialize
  1 , y
  return.
 end.
 a =. prime_factors_of_tail y
 b =. numbers_not_in_list y
 index_of_lowest =. {. _ ,~ I. 1 e."1 a e."1 q:b
 if. index_of_lowest < _ do. NB. if the list doesn't need extension
  y , index_of_lowest { b
  return.
 end.
 NB. otherwise extend the list
 b =. >: >./ y
 while. 1 -.@:e. a e. q: b do.
  b =. >: b
 end.
 y , b
)

   ekg^:9&>2 5 7 9 10
1  2  4 6 3 9 12  8 10  5
1  5 10 2 4 6  3  9 12  8
1  7 14 2 4 6  3  9 12  8
1  9  3 6 2 4  8 10  5 15
1 10  2 4 6 3  9 12  8 14


assert 9 -: >:Until(>&8) 2
assert (,2) -: prime_factors_of_tail 6 8  NB. (nub of)
assert 3 4 5 -: numbers_not_in_list 1 2      6

```

Somewhat shorter is ekg2,

```j

index_of_lowest =: [: {. _ ,~ [: I. 1 e."1 prime_factors_of_tail e."1 q:@:numbers_not_in_list

g =: 3 :0                             NB. return sequence with next term appended
 a =. prime_factors_of_tail y
 (, (index_of_lowest { numbers_not_in_list)`(([: >:Until(1 e. a e. q:) [: >: >./))@.(_ = index_of_lowest)) y
)

ekg2 =: (1&,)`g@.(1<#)

assert (3 -: index_of_lowest { numbers_not_in_list)1 2 4 6

assert (ekg^:9&> -: ekg2^:9&>) 2 5 7 9 10

```



## Julia

{{trans|Perl}}

```julia
using Primes

function ekgsequence(n, limit)
    ekg::Array{Int,1} = [1, n]
    while length(ekg) < limit
        for i in 2:2<<18
            if all(j -> j != i, ekg) && gcd(ekg[end], i) > 1
                push!(ekg, i)
                break
            end
        end
    end
    ekg
end

function convergeat(n, m, max = 100)
    ekgn = ekgsequence(n, max)
    ekgm = ekgsequence(m, max)
    for i in 3:max
        if ekgn[i] == ekgm[i] && sum(ekgn[1:i+1]) == sum(ekgm[1:i+1])
            return i
        end
    end
    warn("no converge in $max terms")
end

[println(rpad("EKG($i): ", 9), join(ekgsequence(i, 30), " ")) for i in [2, 5, 7, 9, 10]]
println("EKGs of 5 & 7 converge at term ", convergeat(5, 7))

```

{{output}}
```txt

EKG(2):  1 2 4 6 3 9 12 8 10 5 15 18 14 7 21 24 16 20 22 11 33 27 30 25 35 28 26 13 39 36
EKG(5):  1 5 10 2 4 6 3 9 12 8 14 7 21 15 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32
EKG(7):  1 7 14 2 4 6 3 9 12 8 10 5 15 18 16 20 22 11 33 21 24 26 13 39 27 30 25 35 28 32
EKG(9):  1 9 3 6 2 4 8 10 5 15 12 14 7 21 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32
EKG(10): 1 10 2 4 6 3 9 12 8 14 7 21 15 5 20 16 18 22 11 33 24 26 13 39 27 30 25 35 28 32
EKGs of 5 & 7 converge at term 21

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.60

fun gcd(a: Int, b: Int): Int {
    var aa = a
    var bb = b
    while (aa != bb) {
        if (aa > bb)
            aa -= bb
        else
            bb -= aa
    }
    return aa
}

const val LIMIT = 100

fun main(args: Array<String>) {
    val starts = listOf(2, 5, 7, 9, 10)
    val ekg = Array(5) { IntArray(LIMIT) }

    for ((s, start) in starts.withIndex()) {
        ekg[s][0] = 1
        ekg[s][1] = start
        for (n in 2 until LIMIT) {
            var i = 2
            while (true) {
                // a potential sequence member cannot already have been used
                // and must have a factor in common with previous member
                if (!ekg[s].slice(0 until n).contains(i) &&
                    gcd(ekg[s][n - 1], i) > 1) {
                        ekg[s][n] = i
                        break
                }
                i++
            }
        }
        System.out.printf("EKG(%2d): %s\n", start, ekg[s].slice(0 until 30))
    }

    // now compare EKG5 and EKG7 for convergence
    for (i in 2 until LIMIT) {
        if (ekg[1][i] == ekg[2][i] &&
        ekg[1].slice(0 until i).sorted() == ekg[2].slice(0 until i).sorted()) {
            println("\nEKG(5) and EKG(7) converge at term ${i + 1}")
            return
        }
    }
    println("\nEKG5(5) and EKG(7) do not converge within $LIMIT terms")
}
```


{{output}}

```txt

EKG( 2): [1, 2, 4, 6, 3, 9, 12, 8, 10, 5, 15, 18, 14, 7, 21, 24, 16, 20, 22, 11, 33, 27, 30, 25, 35, 28, 26, 13, 39, 36]
EKG( 5): [1, 5, 10, 2, 4, 6, 3, 9, 12, 8, 14, 7, 21, 15, 18, 16, 20, 22, 11, 33, 24, 26, 13, 39, 27, 30, 25, 35, 28, 32]
EKG( 7): [1, 7, 14, 2, 4, 6, 3, 9, 12, 8, 10, 5, 15, 18, 16, 20, 22, 11, 33, 21, 24, 26, 13, 39, 27, 30, 25, 35, 28, 32]
EKG( 9): [1, 9, 3, 6, 2, 4, 8, 10, 5, 15, 12, 14, 7, 21, 18, 16, 20, 22, 11, 33, 24, 26, 13, 39, 27, 30, 25, 35, 28, 32]
EKG(10): [1, 10, 2, 4, 6, 3, 9, 12, 8, 14, 7, 21, 15, 5, 20, 16, 18, 22, 11, 33, 24, 26, 13, 39, 27, 30, 25, 35, 28, 32]

EKG(5) and EKG(7) converge at term 21

```



## Perl

{{trans|Perl 6}}

```perl
use List::Util qw(none sum);

sub gcd { my ($u,$v) = @_; $v ? gcd($v, $u%$v) : abs($u) }
sub shares_divisors_with { gcd( $_[0], $_[1]) > 1 }

sub EKG {
    my($n,$limit) = @_;
    my @ekg = (1, $n);
    while (@ekg < $limit) {
        for my $i (2..1e18) {
            next unless none { $_ == $i } @ekg and shares_divisors_with($ekg[-1], $i);
            push(@ekg, $i) and last;
        }
    }
    @ekg;
}

sub converge_at {
    my($n1,$n2) = @_;
    my $max = 100;
    my @ekg1 = EKG($n1,$max);
    my @ekg2 = EKG($n2,$max);
    do { return $_+1 if $ekg1[$_] == $ekg2[$_] && sum(@ekg1[0..$_]) == sum(@ekg2[0..$_])} for 2..$max;
    return "(no convergence in $max terms)";
}

print "EKG($_): " . join(' ', EKG($_,10)) . "\n" for 2, 5, 7, 9, 10;
print "EKGs of 5 & 7 converge at term " . converge_at(5, 7) . "\n"
```

{{out}}

```txt
EKG(2): 1 2 4 6 3 9 12 8 10 5
EKG(5): 1 5 10 2 4 6 3 9 12 8
EKG(7): 1 7 14 2 4 6 3 9 12 8
EKG(9): 1 9 3 6 2 4 8 10 5 15
EKG(10): 1 10 2 4 6 3 9 12 8 14
EKGs of 5 & 7 converge at term 21
```



## Perl 6

{{works with|Rakudo Star|2018.04.1}}

```perl6
sub infix:<shares-divisors-with> { ($^a gcd $^b) > 1 }

sub next-EKG ( *@s ) {
    return first {
        @s ∌ $_  and  @s.tail shares-divisors-with $_
    }, 2..*;
}

sub EKG ( Int $start ) {  1, $start, &next-EKG … *  }

sub converge-at ( @ints ) {
    my @ekgs = @ints.map: &EKG;

    return (2 .. *).first: -> $i {
        [==]  @ekgs.map(     *.[$i]     ) and
        [===] @ekgs.map( *.head($i).Set )
    }
}

say "EKG($_): ", .&EKG.head(10) for 2, 5, 7, 9, 10;

for [5, 7], [2, 5, 7, 9, 10] -> @ints {
    say "EKGs of (@ints[]) converge at term {$_+1}" with converge-at(@ints);
}
```

{{out}}

```txt

EKG(2): (1 2 4 6 3 9 12 8 10 5)
EKG(5): (1 5 10 2 4 6 3 9 12 8)
EKG(7): (1 7 14 2 4 6 3 9 12 8)
EKG(9): (1 9 3 6 2 4 8 10 5 15)
EKG(10): (1 10 2 4 6 3 9 12 8 14)
EKGs of (5 7) converge at term 21
EKGs of (2 5 7 9 10) converge at term 45

```



## Phix

{{trans|C}}

```Phix
constant LIMIT = 100
constant starts = {2, 5, 7, 9, 10}
sequence ekg = {}
string fmt = "EKG(%2d): ["&join(repeat("%d",min(LIMIT,30))," ")&"]\n"
for s=1 to length(starts) do
    ekg = append(ekg,{1,starts[s]}&repeat(0,LIMIT-2))
    for n=3 to LIMIT do
        -- a potential sequence member cannot already have been used
        -- and must have a factor in common with previous member
        integer i = 2
        while find(i,ekg[s])
           or gcd(ekg[s][n-1],i)<=1 do
            i += 1
        end while
        ekg[s][n] = i
    end for
    printf(1,fmt,starts[s]&ekg[s][1..min(LIMIT,30)])
end for

-- now compare EKG5 and EKG7 for convergence
constant EKG5 = find(5,starts),
         EKG7 = find(7,starts)
string msg = sprintf("do not converge within %d terms", LIMIT)
for i=3 to LIMIT do
    if ekg[EKG5][i]=ekg[EKG7][i]
    and sort(ekg[EKG5][1..i-1])=sort(ekg[EKG7][1..i-1]) then
        msg = sprintf("converge at term %d", i)
        exit
    end if
end for
printf(1,"\nEKG5(5) and EKG(7) %s\n", msg)
```

{{out}}

```txt

EKG( 2): [1 2 4 6 3 9 12 8 10 5 15 18 14 7 21 24 16 20 22 11 33 27 30 25 35 28 26 13 39 36]
EKG( 5): [1 5 10 2 4 6 3 9 12 8 14 7 21 15 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32]
EKG( 7): [1 7 14 2 4 6 3 9 12 8 10 5 15 18 16 20 22 11 33 21 24 26 13 39 27 30 25 35 28 32]
EKG( 9): [1 9 3 6 2 4 8 10 5 15 12 14 7 21 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32]
EKG(10): [1 10 2 4 6 3 9 12 8 14 7 21 15 5 20 16 18 22 11 33 24 26 13 39 27 30 25 35 28 32]

EKG5(5) and EKG(7) converge at term 21

```



## Python


### Python: Using math.gcd

If this alternate definition of function EKG_gen is used then the output would be the same as above.
Instead of keeping a cache of prime factors this calculates the gretest common divisor as needed.

```python
from itertools import count, islice, takewhile
from math import gcd

def EKG_gen(start=2):
    """\
    Generate the next term of the EKG together with the minimum cache of
    numbers left in its production; (the "state" of the generator).
    Using math.gcd
    """
    c = count(start + 1)
    last, so_far = start, list(range(2, start))
    yield 1, []
    yield last, []
    while True:
        for index, sf in enumerate(so_far):
            if gcd(last, sf) > 1:
                last = so_far.pop(index)
                yield last, so_far[::]
                break
        else:
            so_far.append(next(c))

def find_convergence(ekgs=(5,7)):
    "Returns the convergence point or zero if not found within the limit"
    ekg = [EKG_gen(n) for n in ekgs]
    for e in ekg:
        next(e)    # skip initial 1 in each sequence
    return 2 + len(list(takewhile(lambda state: not all(state[0] == s for  s in state[1:]),
                                  zip(*ekg))))

if __name__ == '__main__':
    for start in 2, 5, 7, 9, 10:
        print(f"EKG({start}):", str([n[0] for n in islice(EKG_gen(start), 10)])[1: -1])
    print(f"\nEKG(5) and EKG(7) converge at term {find_convergence(ekgs=(5,7))}!")
```


{{out}}
(Same as above).

```txt
EKG(2): 1, 2, 4, 6, 3, 9, 12, 8, 10, 5
EKG(5): 1, 5, 10, 2, 4, 6, 3, 9, 12, 8
EKG(7): 1, 7, 14, 2, 4, 6, 3, 9, 12, 8
EKG(9): 1, 9, 3, 6, 2, 4, 8, 10, 5, 15
EKG(10): 1, 10, 2, 4, 6, 3, 9, 12, 8, 14

EKG(5) and EKG(7) converge at term 21!
```


;Note:
Despite EKG(5) and EKG(7) seeming to converge earlier, as seen above; their hidden states differ.

Here is those series out to 21 terms where you can see them diverge again before finally converging. The state is also shown.

```python
# After running the above, in the terminal:
from pprint import pprint as pp

for start in 5, 7:
    print(f"EKG({start}):\n[(<next>, [<state>]), ...]")
    pp(([n for n in islice(EKG_gen(start), 21)]))
```


'''Generates:'''

```txt
EKG(5):
[(<next>, [<state>]), ...]
[(1, []),
 (5, []),
 (10, [2, 3, 4, 6, 7, 8, 9]),
 (2, [3, 4, 6, 7, 8, 9]),
 (4, [3, 6, 7, 8, 9]),
 (6, [3, 7, 8, 9]),
 (3, [7, 8, 9]),
 (9, [7, 8]),
 (12, [7, 8, 11]),
 (8, [7, 11]),
 (14, [7, 11, 13]),
 (7, [11, 13]),
 (21, [11, 13, 15, 16, 17, 18, 19, 20]),
 (15, [11, 13, 16, 17, 18, 19, 20]),
 (18, [11, 13, 16, 17, 19, 20]),
 (16, [11, 13, 17, 19, 20]),
 (20, [11, 13, 17, 19]),
 (22, [11, 13, 17, 19]),
 (11, [13, 17, 19]),
 (33, [13, 17, 19, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]),
 (24, [13, 17, 19, 23, 25, 26, 27, 28, 29, 30, 31, 32])]
EKG(7):
[(<next>, [<state>]), ...]
[(1, []),
 (7, []),
 (14, [2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13]),
 (2, [3, 4, 5, 6, 8, 9, 10, 11, 12, 13]),
 (4, [3, 5, 6, 8, 9, 10, 11, 12, 13]),
 (6, [3, 5, 8, 9, 10, 11, 12, 13]),
 (3, [5, 8, 9, 10, 11, 12, 13]),
 (9, [5, 8, 10, 11, 12, 13]),
 (12, [5, 8, 10, 11, 13]),
 (8, [5, 10, 11, 13]),
 (10, [5, 11, 13]),
 (5, [11, 13]),
 (15, [11, 13]),
 (18, [11, 13, 16, 17]),
 (16, [11, 13, 17]),
 (20, [11, 13, 17, 19]),
 (22, [11, 13, 17, 19, 21]),
 (11, [13, 17, 19, 21]),
 (33, [13, 17, 19, 21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]),
 (21, [13, 17, 19, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]),
 (24, [13, 17, 19, 23, 25, 26, 27, 28, 29, 30, 31, 32])]
```



## REXX


```rexx
/*REXX program can  generate and display several  EKG  sequences  (with various starts).*/
parse arg nums start                             /*obtain optional arguments from the CL*/
if  nums=='' |  nums==","  then  nums= 50        /*Not specified?  Then use the default.*/
if start= '' | start= ","  then start=2 5 7 9 10 /* "      "         "   "   "     "    */

     do s=1  for words(start);   $=              /*step through the specified  STARTs.  */
     second= word(start, s);     say             /*obtain the second integer in the seq.*/

         do j=1  for nums
         if j<3  then do; #=1;  if j==2  then #=second;  end   /*handle 1st & 2nd number*/
                 else #= ekg(#)
         $= $ right(#,  max(2, length(#) ) )     /*append the EKG integer to the $ list.*/
         end   /*j*/                             /* [↑] the RIGHT BIF aligns the numbers*/
     say '(start'  right(second,  max(2, length(second) ) )"):"$      /*display EKG seq.*/
     end       /*s*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add_:   do  while z//j == 0;    z=z%j;    _=_ j;    w=w+1;    end;         return strip(_)
/*──────────────────────────────────────────────────────────────────────────────────────*/
ekg: procedure expose $; parse arg x 1 z,,_
     w=0                                                        /*W:  number of factors.*/
             do k=1  to 11  by 2;     j=k;  if j==1  then j=2   /*divide by low primes. */
             if j==9  then iterate;   call add_                 /*skip ÷ 9; add to list.*/
             end   /*k*/
                                                                /*↓ skips multiples of 3*/
             do y=0  by 2;  j= j + 2 + y//4                     /*increment J by 2 or 4.*/
             parse var  j  ''  -1  r;  if r==5  then iterate    /*divisible by five ?   */
             if j*j>x | j>z  then leave                         /*passed the sqrt(x) ?  */
             _= add_()                                          /*add a factor to list. */
             end   /*y*/
     j=z;                    if z\==1  then _= add_()           /*Z¬=1? Then add──►list.*/
     if _=''  then _=x                                          /*Null? Then use prime. */
                 do   j=3;                          done=1
                   do k=1  for w
                   if j // word(_, k)==0  then do;  done=0;  leave;  end
                   end   /*k*/
                 if done  then iterate
                 if wordpos(j, $)==0  then return j             /*return an EKG integer.*/
                 end     /*j*/
```

{{out|output|text=  when using the default inputs:}}
<!--    (output is shown   '''<sup>5</sup>/<sub>6</sub>'''   size.)   !-->
<pre style="font-size:84%">
(start  2):  1  2  4  6  3  9 12  8 10  5 15 18 14  7 21 24 16 20 22 11 33 27 30 25 35 28 26 13 39 36 32 34 17 51 42 38 19 57 45 40 44 46 23 69 48 50 52 54 56 49

(start  5):  1  5 10  4  6  3  9 12  8 14  7 21 15 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32 34 17 51 36 38 19 57 42 40 44 46 23 69 45 48 50 52 54 56 49 63

(start  7):  1  7 14  4  6  3  9 12  8 10  5 15 18 16 20 22 11 33 21 24 26 13 39 27 30 25 35 28 32 34 17 51 36 38 19 57 42 40 44 46 23 69 45 48 50 52 54 56 49 63

(start  9):  1  9  3  6  4  8 10  5 15 12 14  7 21 18 16 20 22 11 33 24 26 13 39 27 30 25 35 28 32 34 17 51 36 38 19 57 42 40 44 46 23 69 45 48 50 52 54 56 49 63

(start 10):  1 10  4  6  3  9 12  8 14  7 21 15  5 20 16 18 22 11 33 24 26 13 39 27 30 25 35 28 32 34 17 51 36 38 19 57 42 40 44 46 23 69 45 48 50 52 54 56 49 63

```



## Sidef

{{trans|Perl 6}}

```ruby
class Seq(terms, callback) {
    method next {
        terms += callback(terms)
    }

    method nth(n) {
        while (terms.len < n) {
            self.next
        }
        terms[n-1]
    }

    method first(n) {
        while (terms.len < n) {
            self.next
        }
        terms.first(n)
    }
}

func next_EKG (s) {
    2..Inf -> first {|k|
        !(s.contains(k) || s[-1].is_coprime(k))
    }
}

func EKG (start) {
    Seq([1, start], next_EKG)
}

func converge_at(ints) {
    var ekgs = ints.map(EKG)

    2..Inf -> first {|k|
        (ekgs.map { .nth(k)        }.uniq.len == 1) &&
        (ekgs.map { .first(k).sort }.uniq.len == 1)
    }
}

for k in [2, 5, 7, 9, 10] {
    say "EKG(#{k}) = #{EKG(k).first(10)}"
}

for arr in [[5,7], [2, 5, 7, 9, 10]] {
    var c = converge_at(arr)
    say "EKGs of #{arr} converge at term #{c}"
}
```

{{out}}

```txt

EKG(2) = [1, 2, 4, 6, 3, 9, 12, 8, 10, 5]
EKG(5) = [1, 5, 10, 2, 4, 6, 3, 9, 12, 8]
EKG(7) = [1, 7, 14, 2, 4, 6, 3, 9, 12, 8]
EKG(9) = [1, 9, 3, 6, 2, 4, 8, 10, 5, 15]
EKG(10) = [1, 10, 2, 4, 6, 3, 9, 12, 8, 14]
EKGs of [5, 7] converge at term 21
EKGs of [2, 5, 7, 9, 10] converge at term 45

```



## zkl

Using gcd hint from Go.

```zkl
fcn ekgW(N){	// --> iterator
   Walker.tweak(fcn(rp,buf,w){
      foreach n in (w){
	 if(rp.value.gcd(n)>1)
	    { rp.set(n); w.push(buf.xplode()); buf.clear(); return(n); }
	 buf.append(n);  // save small numbers not used yet
      }
   }.fp(Ref(N),List(),Walker.chain([2..N-1],[N+1..]))).push(1,N)
}
```


```zkl
foreach n in (T(2,5,7,9,10)){ println("EKG(%2d): %s".fmt(n,ekgW(n).walk(10).concat(","))) }
```

{{out}}

```txt

EKG( 2): 1,2,4,6,3,9,12,8,10,5
EKG( 5): 1,5,10,2,4,6,3,9,12,8
EKG( 7): 1,7,14,2,4,6,3,9,12,8
EKG( 9): 1,9,3,6,2,4,8,10,5,15
EKG(10): 1,10,2,4,6,3,9,12,8,14

```


```zkl
fcn convergeAt(n1,n2,etc){ ns:=vm.arglist;
   ekgWs:=ns.apply(ekgW); ekgWs.apply2("next");  // pop initial 1
   ekgNs:=List()*vm.numArgs;	  // ( (ekg(n1)), (ekg(n2)) ...)
   do(1_000){   // find convergence in this many terms or bail
      ekgN:=ekgWs.apply("next");  // (ekg(n1)[n],ekg(n2)[n] ...)
      ekgNs.zipWith(fcn(ns,n){ ns.merge(n) },ekgN);    // keep terms sorted
      // are all ekg[n]s == and both sequences have same terms?
      if(not ekgN.filter1('!=(ekgN[0])) and not ekgNs.filter1('!=(ekgNs[0])) ){
	 println("EKG(", ns.concat(","), ") converge at term ",ekgNs[0].len() + 1);
	 return();
      }
   }
   println(ns.concat(",")," don't converge");
}
convergeAt(5,7);
convergeAt(2,5,7,9,10);
```

{{out}}

```txt

EKG(5,7) converge at term 21
EKG(2,5,7,9,10) converge at term 45

```

