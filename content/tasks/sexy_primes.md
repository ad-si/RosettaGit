+++
title = "Sexy primes"
description = ""
date = 2019-07-14T10:40:38Z
aliases = []
[extra]
id = 22013
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "awk",
  "c",
  "factor",
  "go",
  "julia",
  "kotlin",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "python",
  "rexx",
  "ruby",
  "sidef",
  "zkl",
]
+++

In mathematics, '''sexy primes''' are prime numbers that differ from each other by six.

For example, the numbers '''5''' and '''11''' are both sexy primes, because '''11''' minus '''6''' is '''5'''.

The term "sexy prime" is a pun stemming from the Latin word for six: ''sex''.



'''Sexy prime pairs:''' Sexy prime pairs are groups of two primes that differ by '''6'''. e.g. '''(5 11), (7 13), (11 17)'''

See sequences: [[OEIS:A023201]] and [[OEIS:A046117]]



'''Sexy prime triplets:''' Sexy prime triplets are groups of three primes where each differs from the next by '''6'''. e.g. '''(5 11 17), (7 13 19), (17 23 29)'''

See sequences: [[OEIS:A046118]], [[OEIS:A046119]] and [[OEIS:A046120]]



'''Sexy prime quadruplets:''' Sexy prime quadruplets are groups of four primes where each differs from the next by '''6'''.  e.g. '''(5 11 17 23), (11 17 23 29)'''

See sequences: [[OEIS:A023271]], [[OEIS:A046122]], [[OEIS:A046123]] and [[OEIS:A046124]]



'''Sexy prime quintuplets:''' Sexy prime quintuplets are groups of five primes with a common difference of '''6'''. One of the terms must be divisible by '''5''', because '''5''' and '''6''' are relatively prime. Thus, the only possible sexy prime quintuplet is '''(5 11 17 23 29)'''



## Task

::*For each of pairs, triplets, quadruplets and quintuplets, Find and display the count of each group type of sexy primes less than one million thirty-five ('''1,000,035''').
::*Display at most the '''last''' '''5''', less than one million thirty-five, of each sexy prime group type.
::*Find and display the count of the unsexy primes less than one million thirty-five.
::*Find and display the '''last 10''' unsexy primes less than one million thirty-five.
::*Note that 1000033 '''SHOULD NOT''' be counted in the pair count. It is sexy, but not in a pair within the limit. However, it also '''SHOULD NOT''' be listed in the unsexy primes since it is sexy.




## AWK


```AWK

# syntax: GAWK -f SEXY_PRIMES.AWK
BEGIN {
    cutoff = 1000034
    for (i=1; i<=cutoff; i++) {
      n1 = i
      if (is_prime(n1)) {
        total_primes++
        if ((n2 = n1 + 6) > cutoff) { continue }
        if (is_prime(n2)) {
          save(2,5,n1 FS n2)
          if ((n3 = n2 + 6) > cutoff) { continue }
          if (is_prime(n3)) {
            save(3,5,n1 FS n2 FS n3)
            if ((n4 = n3 + 6) > cutoff) { continue }
            if (is_prime(n4)) {
              save(4,5,n1 FS n2 FS n3 FS n4)
              if ((n5 = n4 + 6) > cutoff) { continue }
              if (is_prime(n5)) {
                save(5,5,n1 FS n2 FS n3 FS n4 FS n5)
              }
            }
          }
        }
        if ((s[2] s[3] s[4] s[5]) !~ (n1 "")) { # check for unsexy
          save(1,10,n1)
        }
      }
    }
    printf("%d primes less than %s\n\n",total_primes,cutoff+1)
    printf("%d unsexy primes\n%s\n\n",c[1],s[1])
    printf("%d sexy prime pairs\n%s\n\n",c[2],s[2])
    printf("%d sexy prime triplets\n%s\n\n",c[3],s[3])
    printf("%d sexy prime quadruplets\n%s\n\n",c[4],s[4])
    printf("%d sexy prime quintuplets\n%s\n\n",c[5],s[5])
    exit(0)
}
function is_prime(x,  i) {
    if (x <= 1) {
      return(0)
    }
    for (i=2; i<=int(sqrt(x)); i++) {
      if (x % i == 0) {
        return(0)
      }
    }
    return(1)
}
function save(key,nbr_to_keep,str) {
    c[key]++
    str = s[key] str ", "
    if (gsub(/,/,"&",str) > nbr_to_keep) {
      str = substr(str,index(str,",")+2)
    }
    s[key] = str
}

```

```txt

78500 primes less than 1000035

48627 unsexy primes
999853, 999863, 999883, 999907, 999917, 999931, 999961, 999979, 999983, 1000003,

16386 sexy prime pairs
999371 999377, 999431 999437, 999721 999727, 999763 999769, 999953 999959,

2900 sexy prime triplets
997427 997433 997439, 997541 997547 997553, 998071 998077 998083, 998617 998623 998629, 998737 998743 998749,

325 sexy prime quadruplets
977351 977357 977363 977369, 983771 983777 983783 983789, 986131 986137 986143 986149, 990371 990377 990383 990389, 997091 997097 997103 997109,

1 sexy prime quintuplets
5 11 17 23 29,

```


## C

Similar approach to the Go entry but only stores the arrays that need to be printed out.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

#define TRUE 1
#define FALSE 0

typedef unsigned char bool;

void sieve(bool *c, int limit) {
    int i, p = 3, p2;
    // TRUE denotes composite, FALSE denotes prime.
    c[0] = TRUE;
    c[1] = TRUE;
    // no need to bother with even numbers over 2 for this task
    for (;;) {
        p2 = p * p;
        if (p2 >= limit) {
            break;
        }
        for (i = p2; i < limit; i += 2*p) {
            c[i] = TRUE;
        }
        for (;;) {
            p += 2;
            if (!c[p]) {
                break;
            }
        }
    }
}

void printHelper(const char *cat, int len, int lim, int n) {
    const char *sp = strcmp(cat, "unsexy primes") ? "sexy prime " : "";
    const char *verb = (len == 1) ? "is" : "are";
    printf("Number of %s%s less than %'d = %'d\n", sp, cat, lim, len);
    printf("The last %d %s:\n", n, verb);
}

void printArray(int *a, int len) {
    int i;
    printf("[");
    for (i = 0; i < len; ++i) printf("%d ", a[i]);
    printf("\b]");
}

int main() {
    int i, ix, n, lim = 1000035;
    int pairs = 0, trips = 0, quads = 0, quins = 0, unsexy = 2;
    int pr = 0, tr = 0, qd = 0, qn = 0, un = 2;
    int lpr = 5, ltr = 5, lqd = 5, lqn = 5, lun = 10;
    int last_pr[5][2], last_tr[5][3], last_qd[5][4], last_qn[5][5];
    int last_un[10];
    bool *sv = calloc(lim - 1, sizeof(bool)); // all FALSE by default
    setlocale(LC_NUMERIC, "");
    sieve(sv, lim);

    // get the counts first
    for (i = 3; i < lim; i += 2) {
        if (i > 5 && i < lim-6 && !sv[i] && sv[i-6] && sv[i+6]) {
            unsexy++;
            continue;
        }
        if (i < lim-6 && !sv[i] && !sv[i+6]) {
            pairs++;
        } else continue;

        if (i < lim-12 && !sv[i+12]) {
            trips++;
        } else continue;

        if (i < lim-18 && !sv[i+18]) {
            quads++;
        } else continue;

        if (i < lim-24 && !sv[i+24]) {
            quins++;
        }
    }
    if (pairs < lpr) lpr = pairs;
    if (trips < ltr) ltr = trips;
    if (quads < lqd) lqd = quads;
    if (quins < lqn) lqn = quins;
    if (unsexy < lun) lun = unsexy;

    // now get the last 'x' for each category
    for (i = 3; i < lim; i += 2) {
        if (i > 5 && i < lim-6 && !sv[i] && sv[i-6] && sv[i+6]) {
            un++;
            if (un > unsexy - lun) {
                last_un[un + lun - 1 - unsexy] = i;
            }
            continue;
        }
        if (i < lim-6 && !sv[i] && !sv[i+6]) {
            pr++;
            if (pr > pairs - lpr) {
                ix = pr + lpr - 1 - pairs;
                last_pr[ix][0] = i; last_pr[ix][1] = i + 6;
            }
        } else continue;

        if (i < lim-12 && !sv[i+12]) {
            tr++;
            if (tr > trips - ltr) {
                ix = tr + ltr - 1 - trips;
                last_tr[ix][0] = i; last_tr[ix][1] = i + 6;
                last_tr[ix][2] = i + 12;
            }
        } else continue;

        if (i < lim-18 && !sv[i+18]) {
            qd++;
            if (qd > quads - lqd) {
                ix = qd + lqd - 1 - quads;
                last_qd[ix][0] = i; last_qd[ix][1] = i + 6;
                last_qd[ix][2] = i + 12; last_qd[ix][3] = i + 18;
            }
        } else continue;

        if (i < lim-24 && !sv[i+24]) {
            qn++;
            if (qn > quins - lqn) {
                ix = qn + lqn - 1 - quins;
                last_qn[ix][0] = i; last_qn[ix][1] = i + 6;
                last_qn[ix][2] = i + 12; last_qn[ix][3] = i + 18;
                last_qn[ix][4] = i + 24;
            }
        }
    }

    printHelper("pairs", pairs, lim, lpr);
    printf("  [");
    for (i = 0; i < lpr; ++i) {
        printArray(last_pr[i], 2);
        printf("\b] ");
    }
    printf("\b]\n\n");

    printHelper("triplets", trips, lim, ltr);
    printf("  [");
    for (i = 0; i < ltr; ++i) {
        printArray(last_tr[i], 3);
        printf("\b] ");
    }
    printf("\b]\n\n");

    printHelper("quadruplets", quads, lim, lqd);
    printf("  [");
    for (i = 0; i < lqd; ++i) {
        printArray(last_qd[i], 4);
        printf("\b] ");
    }
    printf("\b]\n\n");

    printHelper("quintuplets", quins, lim, lqn);
    printf("  [");
    for (i = 0; i < lqn; ++i) {
        printArray(last_qn[i], 5);
        printf("\b] ");
    }
    printf("\b]\n\n");

    printHelper("unsexy primes", unsexy, lim, lun);
    printf("  [");
    printArray(last_un, lun);
    printf("\b]\n");
    free(sv);
    return 0;
}
```


```txt

Number of sexy prime pairs less than 1,000,035 = 16,386
The last 5 are:
  [[999371 999377] [999431 999437] [999721 999727] [999763 999769] [999953 999959]]

Number of sexy prime triplets less than 1,000,035 = 2,900
The last 5 are:
  [[997427 997433 997439] [997541 997547 997553] [998071 998077 998083] [998617 998623 998629] [998737 998743 998749]]

Number of sexy prime quadruplets less than 1,000,035 = 325
The last 5 are:
  [[977351 977357 977363 977369] [983771 983777 983783 983789] [986131 986137 986143 986149] [990371 990377 990383 990389] [997091 997097 997103 997109]]

Number of sexy prime quintuplets less than 1,000,035 = 1
The last 1 is:
  [[5 11 17 23 29]]

Number of unsexy primes less than 1,000,035 = 48,627
The last 10 are:
  [[999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003]

```

=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Sexy primes. Nigel Galloway: October 2nd., 2018
let n=pCache |> Seq.takeWhile(fun n->n<1000035) |> Seq.filter(fun n->(not (isPrime(n+6)) && (not isPrime(n-6))))) |> Array.ofSeq
printfn "There are %d unsexy primes less than 1,000,035. The last 10 are:" n.Length
Array.skip (n.Length-10) n |> Array.iter(fun n->printf "%d " n); printfn ""
let ni=pCache |> Seq.takeWhile(fun n->n<1000035) |> Seq.filter(fun n->isPrime(n-6)) |> Array.ofSeq
printfn "There are %d sexy prime pairs all components of which are less than 1,000,035. The last 5 are:" ni.Length
Array.skip (ni.Length-5) ni |> Array.iter(fun n->printf "(%d,%d) " (n-6) n); printfn ""
let nig=ni |> Array.filter(fun n->isPrime(n-12))
printfn "There are %d sexy prime triplets all components of which are less than 1,000,035. The last 5 are:" nig.Length
Array.skip (nig.Length-5) nig |> Array.iter(fun n->printf "(%d,%d,%d) " (n-12) (n-6) n); printfn ""
let nige=nig |> Array.filter(fun n->isPrime(n-18))
printfn "There are %d sexy prime quadruplets all components of which are less than 1,000,035. The last 5 are:" nige.Length
Array.skip (nige.Length-5) nige |> Array.iter(fun n->printf "(%d,%d,%d,%d) " (n-18) (n-12) (n-6) n); printfn ""
let nigel=nige |> Array.filter(fun n->isPrime(n-24))
printfn "There are %d sexy prime quintuplets all components of which are less than 1,000,035. The last 5 are:" nigel.Length
Array.skip (nigel.Length-5) nigel |> Array.iter(fun n->printf "(%d,%d,%d,%d,%d) " (n-24) (n-18) (n-12) (n-6) n); printfn ""

```

```txt

There are 48627 unsexy primes less than 1,000,035. The last 10 are:
999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003
There are 16386 sexy prime pairs all components of which are less than 1,000,035. The last 5 are:
(999371,999377) (999431,999437) (999721,999727) (999763,999769) (999953,999959)
There are 2900 sexy prime triplets all components of which are less than 1,000,035. The last 5 are:
(997427,997433,997439) (997541,997547,997553) (998071,998077,998083) (998617,998623,998629) (998737,998743,998749)
There are 325 sexy prime quadruplets all components of which are less than 1,000,035. The last 5 are:
(977351,977357,977363,977369) (983771,983777,983783,983789) (986131.986137,986143,986149) (990371,990377,990383,990389) (997091,997097,997103,997109)
There are 1 sexy prime quintuplets all components of which are less than 1,000,035. The last 5 are:
(5,11,17,23,29)

```



## Factor


```factor
USING: combinators.short-circuit fry interpolate io kernel
literals locals make math math.primes math.ranges prettyprint qw
sequences tools.memory.private ;
IN: rosetta-code.sexy-primes

CONSTANT: limit 1,000,035
CONSTANT: primes $[ limit primes-upto ]
CONSTANT: tuplet-names qw{ pair triplet quadruplet quintuplet }

: tuplet ( m n -- seq ) dupd 1 - 6 * + 6 <range> ;

: viable-tuplet? ( seq -- ? )
    [ [ prime? ] [ limit < ] bi and ] all? ;

: sexy-tuplets ( n -- seq ) [ primes ] dip '[
        [ _ tuplet dup viable-tuplet? [ , ] [ drop ] if ] each
    ] { } make ;

: ?last5 ( seq -- seq' ) 5 short tail* ;

: last5 ( seq -- str )
    ?last5 [ { } like unparse ] map " " join ;

:: tuplet-info ( n -- last5 l5-len num-tup limit tuplet-name )
    n sexy-tuplets :> tup tup last5 tup ?last5 length tup length
    commas limit commas n 2 - tuplet-names nth ;

: show-tuplets ( n -- )
    tuplet-info
    [I Number of sexy prime ${0}s < ${1}: ${2}I] nl
    [I Last ${0}: ${1}I] nl nl ;

: unsexy-primes ( -- seq ) primes [
        { [ 6 + prime? not ] [ 6 - prime? not ] } 1&&
    ] filter ;

: show-unsexy ( -- )
    unsexy-primes dup length commas limit commas
    [I Number of unsexy primes < ${0}: ${1}I] nl
    "Last 10: " write 10 short tail* [ pprint bl ] each nl ;

: main ( -- ) 2 5 [a,b] [ show-tuplets ] each show-unsexy ;

MAIN: main
```

```txt

Number of sexy prime pairs < 1,000,035: 16,386
Last 5: { 999371 999377 } { 999431 999437 } { 999721 999727 } { 999763 999769 } { 999953 999959 }

Number of sexy prime triplets < 1,000,035: 2,900
Last 5: { 997427 997433 997439 } { 997541 997547 997553 } { 998071 998077 998083 } { 998617 998623 998629 } { 998737 998743 998749 }

Number of sexy prime quadruplets < 1,000,035: 325
Last 5: { 977351 977357 977363 977369 } { 983771 983777 983783 983789 } { 986131 986137 986143 986149 } { 990371 990377 990383 990389 } { 997091 997097 997103 997109 }

Number of sexy prime quintuplets < 1,000,035: 1
Last 1: { 5 11 17 23 29 }

Number of unsexy primes < 1,000,035: 48,627
Last 10: 999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003

```



## Go


```go
package main

import "fmt"

func sieve(limit int) []bool {
    limit++
    // True denotes composite, false denotes prime.
    c := make([]bool, limit) // all false by default
    c[0] = true
    c[1] = true
    // no need to bother with even numbers over 2 for this task
    p := 3 // Start from 3.
    for {
        p2 := p * p
        if p2 >= limit {
            break
        }
        for i := p2; i < limit; i += 2 * p {
            c[i] = true
        }
        for {
            p += 2
            if !c[p] {
                break
            }
        }
    }
    return c
}

func commatize(n int) string {
    s := fmt.Sprintf("%d", n)
    if n < 0 {
        s = s[1:]
    }
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    if n >= 0 {
        return s
    }
    return "-" + s
}

func printHelper(cat string, le, lim, max int) (int, int, string) {
    cle, clim := commatize(le), commatize(lim)
    if cat != "unsexy primes" {
        cat = "sexy prime " + cat
    }
    fmt.Printf("Number of %s less than %s = %s\n", cat, clim, cle)
    last := max
    if le < last {
        last = le
    }
    verb := "are"
    if last == 1 {
        verb = "is"
    }
    return le, last, verb
}

func main() {
    lim := 1000035
    sv := sieve(lim - 1)
    var pairs [][2]int
    var trips [][3]int
    var quads [][4]int
    var quins [][5]int
    var unsexy = []int{2, 3}
    for i := 3; i < lim; i += 2 {
        if i > 5 && i < lim-6 && !sv[i] && sv[i-6] && sv[i+6] {
            unsexy = append(unsexy, i)
            continue
        }
        if i < lim-6 && !sv[i] && !sv[i+6] {
            pair := [2]int{i, i + 6}
            pairs = append(pairs, pair)
        } else {
            continue
        }
        if i < lim-12 && !sv[i+12] {
            trip := [3]int{i, i + 6, i + 12}
            trips = append(trips, trip)
        } else {
            continue
        }
        if i < lim-18 && !sv[i+18] {
            quad := [4]int{i, i + 6, i + 12, i + 18}
            quads = append(quads, quad)
        } else {
            continue
        }
        if i < lim-24 && !sv[i+24] {
            quin := [5]int{i, i + 6, i + 12, i + 18, i + 24}
            quins = append(quins, quin)
        }
    }
    le, n, verb := printHelper("pairs", len(pairs), lim, 5)
    fmt.Printf("The last %d %s:\n  %v\n\n", n, verb, pairs[le-n:])

    le, n, verb = printHelper("triplets", len(trips), lim, 5)
    fmt.Printf("The last %d %s:\n  %v\n\n", n, verb, trips[le-n:])

    le, n, verb = printHelper("quadruplets", len(quads), lim, 5)
    fmt.Printf("The last %d %s:\n  %v\n\n", n, verb, quads[le-n:])

    le, n, verb = printHelper("quintuplets", len(quins), lim, 5)
    fmt.Printf("The last %d %s:\n  %v\n\n", n, verb, quins[le-n:])

    le, n, verb = printHelper("unsexy primes", len(unsexy), lim, 10)
    fmt.Printf("The last %d %s:\n  %v\n\n", n, verb, unsexy[le-n:])
}
```


```txt

Number of sexy prime pairs less than 1,000,035 = 16,386
The last 5 are:
  [[999371 999377] [999431 999437] [999721 999727] [999763 999769] [999953 999959]]

Number of sexy prime triplets less than 1,000,035 = 2,900
The last 5 are:
  [[997427 997433 997439] [997541 997547 997553] [998071 998077 998083] [998617 998623 998629] [998737 998743 998749]]

Number of sexy prime quadruplets less than 1,000,035 = 325
The last 5 are:
  [[977351 977357 977363 977369] [983771 983777 983783 983789] [986131 986137 986143 986149] [990371 990377 990383 990389] [997091 997097 997103 997109]]

Number of sexy prime quintuplets less than 1,000,035 = 1
The last 1 is:
  [[5 11 17 23 29]]

Number of unsexy primes less than 1,000,035 = 48,627
The last 10 are:
  [999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003]

```



## Julia


```julia

using Primes

function nextby6(n, a)
    top = length(a)
    i = n + 1
    j = n + 2
    k = n + 3
    if n >= top
        return n
    end
    possiblenext = a[n] + 6
    if i <= top && possiblenext == a[i]
        return i
    elseif j <= top && possiblenext == a[j]
        return j
    elseif k <= top && possiblenext == a[k]
        return k
    end
    return n
end

function lastones(dict, n)
    arr = sort(collect(keys(dict)))
    beginidx = max(1, length(arr) - n + 1)
    arr[beginidx: end]
end

function lastoneslessthan(dict, n, ceiling)
    arr = filter(y -> y < ceiling, lastones(dict, n+3))
    beginidx = max(1, length(arr) - n + 1)
    arr[beginidx: end]
end

function primesbysexiness(x)
    twins = Dict{Int64, Array{Int64,1}}()
    triplets = Dict{Int64, Array{Int64,1}}()
    quadruplets = Dict{Int64, Array{Int64,1}}()
    quintuplets = Dict{Int64, Array{Int64,1}}()
    possibles = primes(x + 30)
    singles = filter(y -> y <= x - 6, possibles)
    unsexy = Dict(p => true for p in singles)
    for (i, p) in enumerate(singles)
        twinidx = nextby6(i, possibles)
        if twinidx > i
            delete!(unsexy, p)
            delete!(unsexy, p + 6)
            twins[p] = [i, twinidx]
            tripidx = nextby6(twinidx, possibles)
            if tripidx > twinidx
                triplets[p] = [i, twinidx, tripidx]
                quadidx = nextby6(tripidx, possibles)
                if quadidx > tripidx
                    quadruplets[p] = [i, twinidx, tripidx, quadidx]
                    quintidx = nextby6(quadidx, possibles)
                    if quintidx > quadidx
                        quintuplets[p] = [i, twinidx, tripidx, quadidx, quintidx]
                    end
                end
            end
        end
    end
    # Find and display the count of each group
    println("There are:\n$(length(twins)) twins,\n",
            "$(length(triplets)) triplets,\n",
            "$(length(quadruplets)) quadruplets, and\n",
            "$(length(quintuplets)) quintuplets less than $x.")
    println("The last 5 twin primes start with ", lastoneslessthan(twins, 5, x - 6))
    println("The last 5 triplet primes start with ", lastones(triplets, 5))
    println("The last 5 quadruplet primes start with ", lastones(quadruplets, 5))
    println("The quintuplet primes start with ", lastones(quintuplets, 5))
    println("There are $(length(unsexy)) unsexy primes less than $x.")
    lastunsexy = sort(collect(keys(unsexy)))[length(unsexy) - 9: end]
    println("The last 10 unsexy primes are: $lastunsexy")
end

primesbysexiness(1000035)
```
 {{output}}
```txt

There are:
16386 twins,
2900 triplets,
325 quadruplets, and
1 quintuplets less than 1000035.
The last 5 twin primes start with [999371, 999431, 999721, 999763, 999953]
The last 5 triplet primes start with [997427, 997541, 998071, 998617, 998737]
The last 5 quadruplet primes start with [977351, 983771, 986131, 990371, 997091]
The quintuplet primes start with [5]
There are 48627 unsexy primes less than 1000035.
The last 10 unsexy primes are: [999853, 999863, 999883, 999907, 999917, 999931, 999961, 999979, 999983, 1000003]

```



## Kotlin

```scala
// Version 1.2.71

fun sieve(lim: Int): BooleanArray {
    var limit = lim + 1
    // True denotes composite, false denotes prime.
    val c = BooleanArray(limit)  // all false by default
    c[0] = true
    c[1] = true
    // No need to bother with even numbers over 2 for this task.
    var p = 3  // Start from 3.
    while (true) {
        val p2 = p * p
        if (p2 >= limit) break
        for (i in p2 until limit step 2 * p) c[i] = true
        while (true) {
            p += 2
            if (!c[p]) break
        }
    }
    return c
}

fun printHelper(cat: String, len: Int, lim: Int, max: Int): Pair<Int, String> {
    val cat2 = if (cat != "unsexy primes") "sexy prime " + cat  else cat
    System.out.printf("Number of %s less than %d = %,d\n", cat2, lim, len)
    val last = if (len < max) len else max
    val verb = if (last == 1) "is" else "are"
    return last to verb
}

fun main(args: Array<String>) {
    val lim = 1_000_035
    val sv = sieve(lim - 1)
    val pairs = mutableListOf<List<Int>>()
    val trips = mutableListOf<List<Int>>()
    val quads = mutableListOf<List<Int>>()
    val quins = mutableListOf<List<Int>>()
    val unsexy = mutableListOf(2, 3)
    for (i in 3 until lim step 2) {
        if (i > 5 && i < lim - 6 && !sv[i] && sv[i - 6] && sv[i + 6]) {
            unsexy.add(i)
            continue
        }

        if (i < lim - 6 && !sv[i] && !sv[i + 6]) {
            val pair = listOf(i, i + 6)
            pairs.add(pair)
        } else continue

        if (i < lim - 12 && !sv[i + 12]) {
            val trip = listOf(i, i + 6, i + 12)
            trips.add(trip)
        } else continue

        if (i < lim - 18 && !sv[i + 18]) {
            val quad = listOf(i, i + 6, i + 12, i + 18)
            quads.add(quad)
        } else continue

        if (i < lim - 24 && !sv[i + 24]) {
            val quin = listOf(i, i + 6, i + 12, i + 18, i + 24)
            quins.add(quin)
        }
    }

    var (n2, verb2) = printHelper("pairs", pairs.size, lim, 5)
    System.out.printf("The last %d %s:\n  %s\n\n", n2, verb2, pairs.takeLast(n2))

    var (n3, verb3) = printHelper("triplets", trips.size, lim, 5)
    System.out.printf("The last %d %s:\n  %s\n\n", n3, verb3, trips.takeLast(n3))

    var (n4, verb4) = printHelper("quadruplets", quads.size, lim, 5)
    System.out.printf("The last %d %s:\n  %s\n\n", n4, verb4, quads.takeLast(n4))

    var (n5, verb5) = printHelper("quintuplets", quins.size, lim, 5)
    System.out.printf("The last %d %s:\n  %s\n\n", n5, verb5, quins.takeLast(n5))

    var (nu, verbu) = printHelper("unsexy primes", unsexy.size, lim, 10)
    System.out.printf("The last %d %s:\n  %s\n\n", nu, verbu, unsexy.takeLast(nu))
}
```


```txt

Number of sexy prime pairs less than 1000035 = 16,386
The last 5 are:
  [[999371, 999377], [999431, 999437], [999721, 999727], [999763, 999769], [999953, 999959]]

Number of sexy prime triplets less than 1000035 = 2,900
The last 5 are:
  [[997427, 997433, 997439], [997541, 997547, 997553], [998071, 998077, 998083], [998617, 998623, 998629], [998737, 998743, 998749]]

Number of sexy prime quadruplets less than 1000035 = 325
The last 5 are:
  [[977351, 977357, 977363, 977369], [983771, 983777, 983783, 983789], [986131, 986137, 986143, 986149], [990371, 990377, 990383, 990389], [997091, 997097, 997103, 997109]]

Number of sexy prime quintuplets less than 1000035 = 1
The last 1 is:
  [[5, 11, 17, 23, 29]]

Number of unsexy primes less than 1000035 = 48,627
The last 10 are:
  [999853, 999863, 999883, 999907, 999917, 999931, 999961, 999979, 999983, 1000003]

```


## Pascal

Is the count of unsexy primes = primes-2* SexyPrimesPairs +SexyPrimesTriplets-SexyPrimesQuintuplet?

48627 unsexy primes // = 78500-2*16386+2900-1

37907606 unsexy primes // = 50847538-2*6849047+758163-1
It seems so, not a proove.

```pascal
program SexyPrimes;

uses
  SysUtils;

const
  ctext: array[0..5] of string = ('Primes',
    'sexy prime pairs',
    'sexy prime triplets',
    'sexy prime quadruplets',
    'sexy prime quintuplet',
    'sexy prime sextuplet');

  primeLmt = 1000 * 1000 + 35;
type
  sxPrtpl = record
    spCnt,
    splast5Idx: nativeInt;
    splast5: array[0..6] of NativeInt;
  end;

var
  sieve: array[0..primeLmt] of byte;
  sexyPrimesTpl: array[0..5] of sxPrtpl;
  unsexyprimes: NativeUint;

  procedure dosieve;
  var
    p, delPos, fact: NativeInt;
  begin
    p := 2;
    repeat
      if sieve[p] = 0 then
      begin
        delPos := primeLmt div p;
        if delPos < p then
          BREAK;
        fact := delPos * p;
        while delPos >= p do
        begin
          if sieve[delPos] = 0 then
            sieve[fact] := 1;
          Dec(delPos);
          Dec(fact, p);
        end;
      end;
      Inc(p);
    until False;
  end;
  procedure CheckforSexy;
  var
    i, idx, sieveMask, tstMask: NativeInt;
  begin
    sieveMask := -1;
    for i := 2 to primelmt do
    begin
      tstMask := 1;
      sieveMask := sieveMask + sieveMask + sieve[i];
      idx := 0;
      repeat
        if (tstMask and sieveMask) = 0 then
          with sexyPrimesTpl[idx] do
          begin
            Inc(spCnt);
            //memorize the last entry
            Inc(splast5idx);
            if splast5idx > 5 then
              splast5idx := 1;
            splast5[splast5idx] := i;
            tstMask := tstMask shl 6 + 1;
          end
        else
        begin
          BREAK;
        end;
        Inc(idx);
      until idx > 5;
    end;
  end;

  procedure CheckforUnsexy;
  var
    i: NativeInt;
  begin
    for i := 2 to 6 do
    begin
      if (Sieve[i] = 0) and (Sieve[i + 6] = 1) then
        Inc(unsexyprimes);
    end;
    for i := 2 + 6 to primelmt - 6 do
    begin
      if (Sieve[i] = 0) and (Sieve[i - 6] = 1) and (Sieve[i + 6] = 1) then
        Inc(unsexyprimes);
    end;
  end;

  procedure OutLast5(idx: NativeInt);
  var
    i, j, k: nativeInt;
  begin
    with sexyPrimesTpl[idx] do
    begin
      writeln(cText[idx], '  ', spCnt);
      i := splast5idx + 1;
      for j := 1 to 5 do
      begin
        if i > 5 then
          i := 1;
        if splast5[i] <> 0 then
        begin
          Write('[');
          for k := idx downto 1 do
            Write(splast5[i] - k * 6, ' ');
          Write(splast5[i], ']');
        end;
        Inc(i);
      end;
    end;
    writeln;
  end;

  procedure OutLastUnsexy(cnt:NativeInt);
  var
    i: NativeInt;
    erg: array of NativeUint;
  begin
    if cnt < 1 then
      EXIT;
    setlength(erg,cnt);
    dec(cnt);
    if cnt < 0 then
      EXIT;
    for i := primelmt downto 2 + 6 do
    begin
      if (Sieve[i] = 0) and (Sieve[i - 6] = 1) and (Sieve[i + 6] = 1) then
      Begin
        erg[cnt] := i;
        dec(cnt);
        If cnt < 0 then
          BREAK;
       end;
    end;
    write('the last ',High(Erg)+1,' unsexy primes ');
    For i := 0 to High(erg)-1 do
      write(erg[i],',');
    write(erg[High(erg)]);
  end;
var
  T1, T0: int64;
  i: nativeInt;

begin

  T0 := GettickCount64;
  dosieve;
  T1 := GettickCount64;
  writeln('Sieving is done in ', T1 - T0, ' ms');
  T0 := GettickCount64;
  CheckforSexy;
  T1 := GettickCount64;
  writeln('Checking is done in ', T1 - T0, ' ms');

  unsexyprimes := 0;
  T0 := GettickCount64;
  CheckforUnsexy;
  T1 := GettickCount64;
  writeln('Checking unsexy is done in ', T1 - T0, ' ms');

  writeln('Limit : ', primelmt);
  for i := 0 to 4 do
  begin
    OutLast5(i);
  end;
  writeln;
  writeln(unsexyprimes,' unsexy primes');
  OutLastUnsexy(10);
end.
```

```txt

Sieving is done in 361 ms
Checking is done in 2 ms
Checking unsexy is done in 1 ms
Limit : 1000035
Primes  78500
[999961][999979][999983][1000003][1000033]
sexy prime pairs  16386
[999371 999377][999431 999437][999721 999727][999763 999769][999953 999959]
sexy prime triplets  2900
[997427 997433 997439][997541 997547 997553][998071 998077 998083][998617 998623 998629][998737 998743 998749]
sexy prime quadruplets  325
[977351 977357 977363 977369][983771 983777 983783 983789][986131 986137 986143 986149][990371 990377 990383 990389][997091 997097 997103 997109]
sexy prime quintuplet  1
[5 11 17 23 29]

48627 unsexy primes
the last 10 unsexy primes 999853,999863,999883,999907,999917,999931,999961,999979,999983,1000003
---
Sieving is done in 5248 ms
Checking is done in 1462 ms
Checking unsexy is done in 1062 ms
Limit : 1000000035
Primes  50847538
[999999937][1000000007][1000000009][1000000021][1000000033]
sexy prime pairs  6849047
[999999191 999999197][999999223 999999229][999999607 999999613][999999733 999999739][999999751 999999757]
sexy prime triplets  758163
[999990347 999990353 999990359][999993811 999993817 999993823][999994427 999994433 999994439][999994741 999994747 999994753][999996031 999996037 999996043]
sexy prime quadruplets  56643
[999835261 999835267 999835273 999835279][999864611 999864617 999864623 999864629][999874021 999874027 999874033 999874039][999890981 999890987 999890993 999890999][999956921 999956927 999956933 999956939]
sexy prime quintuplet  1
[5 11 17 23 29]

37907606 unsexy primes // = 50847538-2*6849047+758163-1
the last 10 unsexy primes 999999677,999999761,999999797,999999883,999999893,999999929,999999937,1000000007,1000000009,1000000021

```



## Perl

We will use the prime iterator and primality test from the <code>ntheory</code> module.

```perl
use ntheory qw/prime_iterator is_prime/;

sub tuple_tail {
    my($n,$cnt,@array) = @_;
    $n = @array if $n > @array;
    my @tail;
    for (1..$n) {
        my $p = $array[-$n+$_-1];
        push @tail, "(" . join(" ", map { $p+6*$_ } 0..$cnt-1) . ")";
    }
    return @tail;
}

sub comma {
    (my $s = reverse shift) =~ s/(.{3})/$1,/g;
    ($s = reverse $s) =~ s/^,//;
    return $s;
}

sub sexy_string { my $p = shift; is_prime($p+6) || is_prime($p-6) ? 'sexy' : 'unsexy' }

my $max = 1_000_035;
my $cmax = comma $max;

my $iter = prime_iterator;
my $p = $iter->();
my %primes;
push @{$primes{sexy_string($p)}}, $p;
while ( ($p = $iter->()) < $max) {
    push @{$primes{sexy_string($p)}}, $p;
    $p+ 6 < $max && is_prime($p+ 6) ? push @{$primes{'pair'}},       $p : next;
    $p+12 < $max && is_prime($p+12) ? push @{$primes{'triplet'}},    $p : next;
    $p+18 < $max && is_prime($p+18) ? push @{$primes{'quadruplet'}}, $p : next;
    $p+24 < $max && is_prime($p+24) ? push @{$primes{'quintuplet'}}, $p : next;
}

print "Total primes less than $cmax: " . comma(@{$primes{'sexy'}} + @{$primes{'unsexy'}}) . "\n\n";

for (['pair', 2], ['triplet', 3], ['quadruplet', 4], ['quintuplet', 5]) {
    my($sexy,$cnt) = @$_;
    print "Number of sexy prime ${sexy}s less than $cmax: " . comma(scalar @{$primes{$sexy}}) . "\n";
    print "   Last 5 sexy prime ${sexy}s less than $cmax: " . join(' ', tuple_tail(5,$cnt,@{$primes{$sexy}})) . "\n";
    print "\n";
}

print "Number of unsexy primes less than $cmax: ". comma(scalar @{$primes{unsexy}}) . "\n";
print "  Last 10 unsexy primes less than $cmax: ". join(' ', @{$primes{unsexy}}[-10..-1]) . "\n";
```

```txt
Total primes less than 1,000,035: 78,500

Number of sexy prime pairs less than 1,000,035: 16,386
   Last 5 sexy prime pairs less than 1,000,035: (999371 999377) (999431 999437) (999721 999727) (999763 999769) (999953 999959)

Number of sexy prime triplets less than 1,000,035: 2,900
   Last 5 sexy prime triplets less than 1,000,035: (997427 997433 997439) (997541 997547 997553) (998071 998077 998083) (998617 998623 998629) (998737 998743 998749)

Number of sexy prime quadruplets less than 1,000,035: 325
   Last 5 sexy prime quadruplets less than 1,000,035: (977351 977357 977363 977369) (983771 983777 983783 983789) (986131 986137 986143 986149) (990371 990377 990383 990389) (997091 997097 997103 997109)

Number of sexy prime quintuplets less than 1,000,035: 1
   Last 5 sexy prime quintuplets less than 1,000,035: (5 11 17 23 29)

Number of unsexy primes less than 1,000,035: 48,627
  Last 10 unsexy primes less than 1,000,035: 999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003
```



### Using cluster sieve

The <code>ntheory</code> module includes a function to do very efficient sieving for prime clusters.  Even though we are doing repeated work for this task, it is still faster than the previous code.  The helper subroutines and output code remain identical, as does the generated output.

The cluster sieve becomes more efficient as the number of terms increases.  See for example [[oeis:a213646|OEIS Prime 11-tuplets]].


```perl
use ntheory qw/sieve_prime_cluster forprimes is_prime/;

# ... identical helper functions

my %primes = (
    sexy       => [],
    unsexy     => [],
    pair       => [ sieve_prime_cluster(1, $max-1- 6,  6) ],
    triplet    => [ sieve_prime_cluster(1, $max-1-12,  6, 12) ],
    quadruplet => [ sieve_prime_cluster(1, $max-1-18,  6, 12, 18) ],
    quintuplet => [ sieve_prime_cluster(1, $max-1-24,  6, 12, 18, 24) ],
);

forprimes {
  push @{$primes{sexy_string($_)}}, $_;
} $max-1;

# ... identical output code
```



## Perl 6

```perl6
use Math::Primesieve;
my $sieve = Math::Primesieve.new;

my $max = 1_000_035;
my @primes = $sieve.primes($max);

my $filter = @primes.Set;
my $primes = @primes.categorize: &sexy;

say "Total primes less than {comma $max}: ", comma +@primes;

for <pair 2 triplet 3 quadruplet 4 quintuplet 5> -> $sexy, $cnt {
    say "Number of sexy prime {$sexy}s less than {comma $max}: ", comma +$primes{$sexy};
    say "   Last 5 sexy prime {$sexy}s less than {comma $max}: ",
      join ' ', $primes{$sexy}.tail(5).grep(*.defined).map:
      { "({ $_ «+« (0,6 … 24)[^$cnt] })" }
    say '';
}

say "Number of unsexy primes less than {comma $max}: ", comma +$primes<unsexy>;
say "  Last 10 unsexy primes less than {comma $max}: ", $primes<unsexy>.tail(10);

sub sexy ($i) {
    gather {
        take 'quintuplet' if all($filter{$i «+« (6,12,18,24)});
        take 'quadruplet' if all($filter{$i «+« (6,12,18)});
        take 'triplet'    if all($filter{$i «+« (6,12)});
        take 'pair'       if $filter{$i + 6};
        take (($i >= $max - 6) && ($i + 6).is-prime) ||
          (so any($filter{$i «+« (6, -6)})) ?? 'sexy' !! 'unsexy';
    }
}

sub comma { $^i.flip.comb(3).join(',').flip }
```

```txt
Total primes less than 1,000,035: 78,500
Number of sexy prime pairs less than 1,000,035: 16,386
   Last 5 sexy prime pairs less than 1,000,035: (999371 999377) (999431 999437) (999721 999727) (999763 999769) (999953 999959)

Number of sexy prime triplets less than 1,000,035: 2,900
   Last 5 sexy prime triplets less than 1,000,035: (997427 997433 997439) (997541 997547 997553) (998071 998077 998083) (998617 998623 998629) (998737 998743 998749)

Number of sexy prime quadruplets less than 1,000,035: 325
   Last 5 sexy prime quadruplets less than 1,000,035: (977351 977357 977363 977369) (983771 983777 983783 983789) (986131 986137 986143 986149) (990371 990377 990383 990389) (997091 997097 997103 997109)

Number of sexy prime quintuplets less than 1,000,035: 1
   Last 5 sexy prime quintuplets less than 1,000,035: (5 11 17 23 29)

Number of unsexy primes less than 1,000,035: 48,627
  Last 10 unsexy primes less than 1,000,035: (999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003)
```



## Phix


```Phix
function create_sieve(integer limit)
    sequence sieve = repeat(true,limit)
    sieve[1] = false
    for i=4 to limit by 2 do
        sieve[i] = false
    end for
    for p=3 to floor(sqrt(limit)) by 2 do
        integer p2 = p*p
        if sieve[p2] then
            for k=p2 to limit by p*2 do
                sieve[k] = false
            end for
        end if
    end for
    return sieve
end function

constant lim = 1000035,
--constant lim = 100, -- (this works too)
         limit = lim-(and_bits(lim,1)=0),   -- (limit must be odd)
         sieve = create_sieve(limit+6)  -- (+6 to check for sexiness)

sequence sets = repeat({},5),   -- (unsexy,pairs,trips,quads,quins)
         limits = {10,5,4,3,1},
         counts = 1&repeat(0,4) -- (2 is an unsexy prime)
integer  total = 1              -- ""

for i=limit to 3 by -2 do       -- (this loop skips 2)
    if sieve[i] then
        total += 1
        if sieve[i+6]=false and (i-6<0 or sieve[i-6]=false) then
            counts[1] += 1 -- unsexy
            if length(sets[1])<limits[1] then
                sets[1] = prepend(sets[1],i)
            end if
        else
            sequence set = {i}
            for j=i-6 to 3 by -6 do
                if j<=0 or sieve[j]=false then exit end if
                set = prepend(set,j)
                integer l = length(set)
                if length(sets[l])<limits[l] then
                    sets[l] = prepend(sets[l],set)
                end if
                counts[l] += 1
            end for
        end if
    end if
end for
if length(sets[1])<limits[1] then
    sets[1] = prepend(sets[1],2) -- (as 2 skipped above)
end if

constant fmt = """
Of %,d primes less than %,d there are:
%,d unsexy primes, the last %d being %s
%,d pairs, the last %d being %s
%,d triplets, the last %d being %s
%,d quadruplets, the last %d being %s
%,d quintuplet, the last %d being %s
"""
sequence results = {total,lim,
                    0,0,"",
                    0,0,"",
                    0,0,"",
                    0,0,"",
                    0,0,""}
for i=1 to 5 do
    results[i*3..i*3+2] = {counts[i],length(sets[i]),sprint(sets[i])}
end for
printf(1,fmt,results)
```

```txt

Of 78,500 primes less than 1,000,035 there are:
48,627 unsexy primes, the last 10 being {999853,999863,999883,999907,999917,999931,999961,999979,999983,1000003}
16,386 pairs, the last 5 being {{999371,999377},{999431,999437},{999721,999727},{999763,999769},{999953,999959}}
2,900 triplets, the last 4 being {{997541,997547,997553},{998071,998077,998083},{998617,998623,998629},{998737,998743,998749}}
325 quadruplets, the last 3 being {{986131,986137,986143,986149},{990371,990377,990383,990389},{997091,997097,997103,997109}}
1 quintuplet, the last 1 being {{5,11,17,23,29}}

```



## Python


### Imperative Style


```python
LIMIT = 1_000_035
def primes2(limit=LIMIT):
    if limit < 2: return []
    if limit < 3: return [2]
    lmtbf = (limit - 3) // 2
    buf = [True] * (lmtbf + 1)
    for i in range((int(limit ** 0.5) - 3) // 2 + 1):
        if buf[i]:
            p = i + i + 3
            s = p * (i + 1) + i
            buf[s::p] = [False] * ((lmtbf - s) // p + 1)
    return [2] + [i + i + 3 for i, v in enumerate(buf) if v]

primes = primes2(LIMIT +6)
primeset = set(primes)
primearray = [n in primeset for n in range(LIMIT)]

#%%
s = [[] for x in range(4)]
unsexy = []

for p in primes:
    if p > LIMIT:
        break
    if p + 6 in primeset and p + 6 < LIMIT:
        s[0].append((p, p+6))
    elif p + 6 in primeset:
        break
    else:
        if p - 6 not in primeset:
            unsexy.append(p)
        continue
    if p + 12 in primeset and p + 12 < LIMIT:
        s[1].append((p, p+6, p+12))
    else:
        continue
    if p + 18 in primeset and p + 18 < LIMIT:
        s[2].append((p, p+6, p+12, p+18))
    else:
        continue
    if p + 24 in primeset and p + 24 < LIMIT:
        s[3].append((p, p+6, p+12, p+18, p+24))

#%%
print('"SEXY" PRIME GROUPINGS:')
for sexy, name in zip(s, 'pairs triplets quadruplets quintuplets'.split()):
    print(f'  {len(sexy)} {na (not isPrime(n-6))))) |> Array.ofSeq
printfn "There are %d unsexy primes less than 1,000,035. The last 10 are:" n.Length
Array.skip (n.Length-10) n |> Array.iter(fun n->printf "%d " n); printfn ""
let ni=pCache |> Seq.takeWhile(fun n->nme} ending with ...')
    for sx in sexy[-5:]:
        print('   ',sx)

print(f'\nThere are {len(unsexy)} unsexy primes ending with ...')
for usx in unsexy[-10:]:
    print(' ',usx)
```


```txt
"SEXY" PRIME GROUPINGS:
  16386 pairs ending with ...
    (999371, 999377)
    (999431, 999437)
    (999721, 999727)
    (999763, 999769)
    (999953, 999959)
  2900 triplets ending with ...
    (997427, 997433, 997439)
    (997541, 997547, 997553)
    (998071, 998077, 998083)
    (998617, 998623, 998629)
    (998737, 998743, 998749)
  325 quadruplets ending with ...
    (977351, 977357, 977363, 977369)
    (983771, 983777, 983783, 983789)
    (986131, 986137, 986143, 986149)
    (990371, 990377, 990383, 990389)
    (997091, 997097, 997103, 997109)
  1 quintuplets ending with ...
    (5, 11, 17, 23, 29)

There are 48627 unsexy primes ending with ...
  999853
  999863
  999883
  999907
  999917
  999931
  999961
  999979
  999983
  1000003
```



### Functional style

This task uses [[Extensible_prime_generator#210-wheel_postponed_incremental_sieve]]

```python

#Functional Sexy Primes. Nigel Galloway: October 5th., 2018
from itertools import *
z=primes()
n=frozenset(takewhile(lambda x: x<1000035,z))
ni=sorted(list(filter(lambda g: n.__contains__(g+6) ,n)))
print ("There are",len(ni),"sexy prime pairs all components of which are less than 1,000,035. The last 5 are:")
for g in islice(ni,max(len(ni)-5,0),len(ni)): print(format("(%d,%d) " % (g,g+6)))
nig=list(filter(lambda g: n.__contains__(g+12) ,ni))
print ("There are",len(nig),"sexy prime triplets all components of which are less than 1,000,035. The last 5 are:")
for g in islice(nig,max(len(nig)-5,0),len(nig)): print(format("(%d,%d,%d) " % (g,g+6,g+12)))
nige=list(filter(lambda g: n.__contains__(g+18) ,nig))
print ("There are",len(nige),"sexy prime quadruplets all components of which are less than 1,000,035. The last 5 are:")
for g in islice(nige,max(len(nige)-5,0),len(nige)): print(format("(%d,%d,%d,%d) " % (g,g+6,g+12,g+18)))
nigel=list(filter(lambda g: n.__contains__(g+24) ,nige))
print ("There are",len(nigel),"sexy prime quintuplets all components of which are less than 1,000,035. The last 5 are:")
for g in islice(nigel,max(len(nigel)-5,0),len(nigel)): print(format("(%d,%d,%d,%d,%d) " % (g,g+6,g+12,g+18,g+24)))
un=frozenset(takewhile(lambda x: x<1000050,z)).union(n)
unsexy=sorted(list(filter(lambda g: not un.__contains__(g+6) and not un.__contains__(g-6),n)))
print ("There are",len(unsexy),"unsexy primes less than 1,000,035. The last 10 are:")
for g in islice(unsexy,max(len(unsexy)-10,0),len(unsexy)): print(g)

```

```txt

There are 16386 sexy prime pairs all components of which are less than 1,000,035. The last 5 are:
(999371,999377)
(999431,999437)
(999721,999727)
(999763,999769)
(999953,999959)
There are 2900 sexy prime triplets all components of which are less than 1,000,035. The last 5 are:
(997427,997433,997439)
(997541,997547,997553)
(998071,998077,998083)
(998617,998623,998629)
(998737,998743,998749)
There are 325 sexy prime quadruplets all components of which are less than 1,000,035. The last 5 are:
(977351,977357,977363,977369)
(983771,983777,983783,983789)
(986131,986137,986143,986149)
(990371,990377,990383,990389)
(997091,997097,997103,997109)
There are 1 sexy prime quintuplets all components of which are less than 1,000,035. The last 5 are:
(5,11,17,23,29)
There are 48627 unsexy primes less than 1,000,035. The last 10 are:
999853
999863
999883
999907
999917
999931
999961
999979
999983
1000003

```



## REXX


```rexx
/*REXX program finds and displays various kinds of  sexy and unsexy  primes less than N.*/
parse arg N endU end2 end3 end4 end5 .           /*obtain optional argument from the CL.*/
if    N==''  |    N==","  then    N= 1000035 - 1 /*Not specified?  Then use the default.*/
if endU==''  | endU==","  then endU=      10     /* "      "         "   "   "     "    */
if end2==''  | end2==","  then end2=       5     /* "      "         "   "   "     "    */
if end3==''  | end3==","  then end3=       5     /* "      "         "   "   "     "    */
if end4==''  | end4==","  then end4=       5     /* "      "         "   "   "     "    */
if end5==''  | end5==","  then end4=       5     /* "      "         "   "   "     "    */
call genSq                                       /*gen some squares for the DO k=7 UNTIL*/
call genPx                                       /* " prime (@.) & sexy prime (X.) array*/
call genXU                                       /*gen lists, types of sexy Ps, unsexy P*/
call getXs                                       /*gen lists, last # of types of sexy Ps*/
 @sexy= ' sexy prime'                            /*a handy literal for some of the SAYs.*/
 w2= words( translate(x2,, '~') ); y2= words(x2) /*count #primes in the sexy pairs.     */
 w3= words( translate(x3,, '~') ); y3= words(x3) /*  "   "   "    "  "    "  triplets.  */
 w4= words( translate(x4,, '~') ); y4= words(x4) /*  "   "   "    "  "    "  quadruplets*/
 w5= words( translate(x5,, '~') ); y5= words(x5) /*  "   "   "    "  "    "  quintuplets*/
say 'There are ' commas(w2%2) @sexy "pairs less than "             Nc
say 'The last '  commas(end2) @sexy "pairs are:";        say subword(x2, max(1,y2-end2+1))
say
say 'There are ' commas(w3%3) @sexy "triplets less than "          Nc
say 'The last '  commas(end3) @sexy "triplets are:";     say subword(x3, max(1,y3-end3+1))
say
say 'There are ' commas(w4%4) @sexy "quadruplets less than "       Nc
say 'The last '  commas(end4) @sexy "quadruplets are:";  say subword(x4, max(1,y4-end4+1))
say
say 'There is  ' commas(w5%5) @sexy "quintuplet less than "        Nc
say 'The last '  commas(end4) @sexy "quintuplet are:";   say subword(x5, max(1,y5-end4+1))
say
say 'There are ' commas(s1)         "   sexy primes less than "    Nc
say 'There are ' commas(u1)         " unsexy primes less than "    Nc
say 'The last '  commas(endU)       " unsexy primes are: "   subword(u,  max(1,u1-endU+1))
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;    n= _'.9';     #= 123456789;       b= verify(n, #, "M")
        e= verify(n, #'0', , verify(n, #"0.", 'M') ) - 4
           do j=e  to b  by -3;    _= insert(',', _, j);     end  /*j*/;          return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
genSQ: do i=17  by 2  until i**2 > N+7; s.i= i**2; end; return /*S used for square roots*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
genPx: @.=;              #= 0;          !.= 0.          /*P array; P count; sexy P array*/
       if N>1  then do;  #= 1;   @.1= 2;  !.2= 1;   end /*count of primes found (so far)*/
       x.=!.;                        LPs=3 5 7 11 13 17 /*sexy prime array;  low P list.*/
         do j=3  by 2  to  N+6                          /*start in the cellar & work up.*/
         if j<19  then if wordpos(j, LPs)==0  then iterate
                                              else do; #= #+1;  @.#= j;  !.j= 1;  b= j - 6
                                                       if !.b  then x.b= 1;        iterate
                                                   end
         if j// 3 ==0  then iterate                /* ··· and eliminate multiples of  3.*/
         parse var  j  ''  -1  _                   /* get the rightmost digit of  J.    */
         if     _ ==5  then iterate                /* ··· and eliminate multiples of  5.*/
         if j// 7 ==0  then iterate                /* ···  "      "         "      "  7.*/
         if j//11 ==0  then iterate                /* ···  "      "         "      " 11.*/
         if j//13 ==0  then iterate                /* ···  "      "         "      " 13.*/
                    do k=7  until s._ > j;  _= @.k /*÷ by primes starting at 7th prime. */
                    if j // _ == 0  then iterate j /*get the remainder of  j÷@.k    ___ */
                    end   /*k*/                    /*divide up through & including √ J  */
         if j<=N  then do;  #= #+1;  @.#= j;  end  /*bump P counter;  assign prime to @.*/
         !.j= 1                                    /*define  Jth  number as being prime.*/
              b= j - 6                             /*B: lower part of a sexy prime pair?*/
         if !.b then do; x.b=1; if j<=N then x.j=1; end /*assign (both parts ?) sexy Ps.*/
         end   /*j*/;       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
genXU: u= 2;         Nc=commas(N+1);  s=           /*1st unsexy prime; add commas to N+1*/
       say 'There are ' commas(#)    " primes less than "          Nc;           say
          do k=2  for #-1; p= @.k; if x.p  then s=s p   /*if  sexy prime, add it to list*/
                                           else u= u p  /* " unsexy  "     "   "  "   " */
          end   /*k*/                                   /* [↑]  traispe through odd Ps. */
       s1= words(s);  u1= words(u);   return       /*# of sexy primes;  # unsexy primes.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
getXs: x2=;  do k=2  for #-1;  p=@.k;   if \x.p  then iterate  /*build sexy prime list. */
                               b=p- 6;  if \x.b  then iterate; x2=x2 b'~'p
             end   /*k*/
       x3=;  do k=2  for #-1;  p=@.k;   if \x.p  then iterate  /*build sexy P triplets. */
                               b=p- 6;  if \x.b  then iterate
                               t=p-12;  if \x.t  then iterate; x3=x3 t'~' || b"~"p
             end   /*k*/
       x4=;  do k=2  for #-1;  p=@.k;   if \x.p  then iterate  /*build sexy P quads.    */
                               b=p- 6;  if \x.b  then iterate
                               t=p-12;  if \x.t  then iterate
                               q=p-18;  if \x.q  then iterate; x4=x4 q'~'t"~" || b'~'p
             end   /*k*/
       x5=;  do k=2  for #-1;  p=@.k;   if \x.p  then iterate  /*build sexy P quints.   */
                               b=p- 6;  if \x.b  then iterate
                               t=p-12;  if \x.t  then iterate
                               q=p-18;  if \x.q  then iterate
                               v=p-24;  if \x.v  then iterate; x5=x5 v'~'q"~"t'~' || b"~"p
             end   /*k*/;    return
```

(Shown at   <big>'''<sup>5</sup>/<sub>6</sub>'''</big>   size.)
<pre style="font-size:83%">
There are  78,500  primes less than  1,000,035

There are  16,386  sexy prime pairs less than  1,000,035
The last  5  sexy prime pairs are:
999371~999377 999431~999437 999721~999727 999763~999769 999953~999959

There are  2,900  sexy prime triplets less than  1,000,035
The last  5  sexy prime triplets are:
997427~997433~997439 997541~997547~997553 998071~998077~998083 998617~998623~998629 998737~998743~998749

There are  325  sexy prime quadruplets less than  1,000,035
The last  5  sexy prime quadruplets are:
977351~977357~977363~977369 983771~983777~983783~983789 986131~986137~986143~986149 990371~990377~990383~990389 997091~997097~997103~997109

There is   1  sexy prime quintuplet less than  1,000,035
The last  5  sexy prime quintuplet are:
5~11~17~23~29

There are  29,873    sexy primes less than  1,000,035
There are  48,627  unsexy primes less than  1,000,035
The last  10  unsexy primes are:  999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003

```



## Ruby


```Ruby

require 'prime'

prime_array, sppair2, sppair3, sppair4, sppair5 = Array.new(5) {Array.new()} # arrays for prime numbers and index number to array for each pair.
unsexy, i, start = [2], 0, Time.now
Prime.each(1_000_100) {|prime| prime_array.push prime}

while prime_array[i] < 1_000_035
  i+=1
  unsexy.push(i) if prime_array[(i+1)..(i+2)].include?(prime_array[i]+6) == false && prime_array[(i-2)..(i-1)].include?(prime_array[i]-6) == false && prime_array[i]+6 < 1_000_035
  prime_array[(i+1)..(i+4)].include?(prime_array[i]+6) && prime_array[i]+6 < 1_000_035 ? sppair2.push(i) : next
  prime_array[(i+2)..(i+5)].include?(prime_array[i]+12) && prime_array[i]+12 < 1_000_035 ? sppair3.push(i) : next
  prime_array[(i+3)..(i+6)].include?(prime_array[i]+18) && prime_array[i]+18 < 1_000_035 ? sppair4.push(i) : next
  prime_array[(i+4)..(i+7)].include?(prime_array[i]+24) && prime_array[i]+24 < 1_000_035 ? sppair5.push(i) : next
end

puts "\nSexy prime pairs: #{sppair2.size} found:"
sppair2.last(5).each {|prime| print [prime_array[prime], prime_array[prime]+6].join(" - "), "\n"}
puts "\nSexy prime triplets: #{sppair3.size} found:"
sppair3.last(5).each {|prime| print [prime_array[prime], prime_array[prime]+6, prime_array[prime]+12].join(" - "), "\n"}
puts "\nSexy prime quadruplets: #{sppair4.size} found:"
sppair4.last(5).each {|prime| print [prime_array[prime], prime_array[prime]+6, prime_array[prime]+12, prime_array[prime]+18].join(" - "), "\n"}
puts "\nSexy prime quintuplets: #{sppair5.size} found:"
sppair5.last(5).each {|prime| print [prime_array[prime], prime_array[prime]+6, prime_array[prime]+12, prime_array[prime]+18, prime_array[prime]+24].join(" - "), "\n"}

puts "\nUnSexy prime: #{unsexy.size} found. Last 10 are:"
unsexy.last(10).each {|item| print prime_array[item], " "}
print "\n\n", Time.now - start, " seconds"

```


Output:

```txt

ruby 2.5.3p105 (2018-10-18 revision 65156) [x64-mingw32]


Sexy prime pairs: 16386 found:
999371 - 999377
999431 - 999437
999721 - 999727
999763 - 999769
999953 - 999959

Sexy prime triplets: 2900 found:
997427 - 997433 - 997439
997541 - 997547 - 997553
998071 - 998077 - 998083
998617 - 998623 - 998629
998737 - 998743 - 998749

Sexy prime quadruplets: 325 found:
977351 - 977357 - 977363 - 977369
983771 - 983777 - 983783 - 983789
986131 - 986137 - 986143 - 986149
990371 - 990377 - 990383 - 990389
997091 - 997097 - 997103 - 997109

Sexy prime quintuplets: 1 found:
5 - 11 - 17 - 23 - 29

UnSexy prime: 48627 found. Last 10 are:
999853 999863 999883 999907 999917 999931 999961 999979 999983 1000003

0.176955 seconds

```



## Sidef


```ruby
var limit  = 1e6+35
var primes = limit.primes

say "Total number of primes <= #{limit.commify} is #{primes.len.commify}."
say "Sexy k-tuple primes <= #{limit.commify}:\n"

(2..5).each {|k|
    var groups = []
    primes.each {|p|
        var group = (1..^k -> map {|j| 6*j + p })
        if (group.all{.is_prime} && (group[-1] <= limit)) {
            groups << [p, group...]
        }
    }

    say "...total number of sexy #{k}-tuple primes = #{groups.len.commify}"
    say "...where last 5 tuples are: #{groups.last(5).map{'('+.join(' ')+')'}.join(' ')}\n"
}

var unsexy_primes = primes.grep {|p| is_prime(p+6) || is_prime(p-6) -> not }
say "...total number of unsexy primes = #{unsexy_primes.len.commify}"
say "...where last 10 unsexy primes are: #{unsexy_primes.last(10)}"
```

```txt

Total number of primes <= 1,000,035 is 78,500.
Sexy k-tuple primes <= 1,000,035:

...total number of sexy 2-tuple primes = 16,386
...where last 5 tuples are: (999371 999377) (999431 999437) (999721 999727) (999763 999769) (999953 999959)

...total number of sexy 3-tuple primes = 2,900
...where last 5 tuples are: (997427 997433 997439) (997541 997547 997553) (998071 998077 998083) (998617 998623 998629) (998737 998743 998749)

...total number of sexy 4-tuple primes = 325
...where last 5 tuples are: (977351 977357 977363 977369) (983771 983777 983783 983789) (986131 986137 986143 986149) (990371 990377 990383 990389) (997091 997097 997103 997109)

...total number of sexy 5-tuple primes = 1
...where last 5 tuples are: (5 11 17 23 29)

...total number of unsexy primes = 48,627
...where last 10 unsexy primes are: [999853, 999863, 999883, 999907, 999917, 999931, 999961, 999979, 999983, 1000003]

```



## zkl

Using GMP (GNU Multiple Precision Arithmetic Library, probabilistic
primes), because it is easy and fast to generate primes.

[[Extensible prime generator#zkl]] could be used instead.

```zkl
var [const] BI=Import("zklBigNum");  // libGMP
const N=1_000_035, M=N+24; // M allows prime group to span N, eg N=100, (97,103)
const OVR=6;	// 6 if prime group can NOT span N, else 0
ps,p := Data(M+50).fill(0), BI(1); // slop at the end (for reverse wrap around)
while(p.nextPrime()<=M){ ps[p]=1 } // bitmap of primes

ns:=(N-OVR).filter('wrap(n){ 2==(ps[n] + ps[n+6]) }); # know 2 isn't, check anyway
msg(N,"sexy prime pairs",ns,5,1);

ns:=[3..N-(6+OVR),2].filter('wrap(n){ 3==(ps[n] + ps[n+6] + ps[n+12]) }); # can't be even
msg(N,"sexy triplet primes",ns,5,2);

ns:=[3..N-(12+OVR),2].filter('wrap(n){ 4==(ps[n] + ps[n+6] + ps[n+12] + ps[n+18]) }); # no evens
msg(N,"sexy quadruplet primes",ns,5,3);

ns:=[3..N-(18+OVR),2].filter('wrap(n){ 5==(ps[n] + ps[n+6] + ps[n+12] + ps[n+18] + ps[n+24]) });
msg(N,"sexy quintuplet primes",ns,1,4);

ns:=(N-OVR).filter('wrap(n){ ps[n] and 0==(ps[n-6] + ps[n+6]) });  // include 2
msg(N,"unsexy primes",ns,10,0);

fcn msg(N,s,ps,n,g){
   n=n.min(ps.len());	// if the number of primes is less than n
   gs:=ps[-n,*].apply('wrap(n){ [0..g*6,6].apply('+(n)) })
       .pump(String,T("concat", ","),"(%s) ".fmt);
   println("Number of %s less than %,d is %,d".fmt(s,N,ps.len()));
   println("The last %d %s:\n  %s\n".fmt(n, (n>1 and "are" or "is"), gs));
}
```

<pre style="font-size:80%">
Number of sexy prime pairs less than 1,000,035 is 16,386
The last 5 are:
  (999371,999377) (999431,999437) (999721,999727) (999763,999769) (999953,999959)

Number of sexy triplet primes less than 1,000,035 is 2,900
The last 5 are:
  (997427,997433,997439) (997541,997547,997553) (998071,998077,998083) (998617,998623,998629) (998737,998743,998749)

Number of sexy quadruplet primes less than 1,000,035 is 325
The last 5 are:
  (977351,977357,977363,977369) (983771,983777,983783,983789) (986131,986137,986143,986149) (990371,990377,990383,990389) (997091,997097,997103,997109)

Number of sexy quintuplet primes less than 1,000,035 is 1
The last 1 is:
  (5,11,17,23,29)

Number of unsexy primes less than 1,000,035 is 48,627
The last 10 are:
  (999853) (999863) (999883) (999907) (999917) (999931) (999961) (999979) (999983) (1000003)

```

