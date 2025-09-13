+++
title = "Simulated annealing"
description = ""
date = 2019-10-07T16:18:15Z
aliases = []
[extra]
id = 20664
[taxonomies]
categories = ["task"]
tags = []
+++

Quoted from the [https://en.wikipedia.org/wiki/Simulated_annealing Wikipedia page] : '''Simulated annealing (SA)''' is a probabilistic technique for approximating the global optimum of a given function. Simulated annealing interprets slow cooling as a slow decrease in the probability of temporarily accepting worse solutions as it explores the solution space.

'''Pseudo code''' from [https://en.wikipedia.org/wiki/Simulated_annealing Wikipedia]


```txt

Notations :
  T : temperature. Decreases to 0.
  s : a system state
  E(s) : Energy at s. The function we want to minimize
  ∆E : variation of E, from state s to state s_next
  P(∆E , T) : Probability to move from s to s_next. 
  	if  ( ∆E < 0 ) P = 1
  	      else P = exp ( - ∆E / T) . Decreases as T →  0
  
Pseudo-code:
    Let s = s0  -- initial state
    For k = 0 through kmax (exclusive):
        T ← temperature(k , kmax)
        Pick a random neighbour state , s_next ← neighbour(s)
        ∆E ← E(s) - E(s_next) 
        If P(∆E , T) ≥ random(0, 1), move to the new state:
            s ← s_next
    Output: the final state s

```


'''Problem statement'''

We want to apply SA to the travelling salesman problem. There are 100 cities, numbered 0 to 99, located on a plane, at integer coordinates i,j :  0 <= i,j < 10 . The city at (i,j)  has number 10*i + j. The cities are '''all''' connected : the graph is complete : you can go from one city to any other city in one step.

The salesman wants to start from city 0, visit all cities, each one time, and go back to city 0. The travel cost between two cities is the euclidian distance between there cities. The total travel cost is the total path length.

A path '''s''' is a sequence (0 a b ...z 0) where (a b ..z) is a permutation of the numbers (1 2 .. 99). The path length = E(s) is the sum d(0,a) + d(a,b) + ... + d(z,0) , where d(u,v) is the distance between two cities. Naturally, we want to minimize E(s). 

Definition : The neighbours of a city are the closest cities at distance 1 horizontally/vertically, or √2 diagonally. A corner city (0,9,90,99) has 3 neighbours. A center city has 8 neighbours.


```txt

Distances between cities
d ( 0, 7) → 7
d ( 0, 99) → 12.7279
d ( 23, 78) → 7.0711
d ( 33, 44) → 1.4142 // sqrt(2)

```


'''Task'''

Apply SA to the travelling salesman problem, using the following set of parameters/functions :

* kT = 1 (Multiplication by kT is a placeholder, representing computing temperature as a function of 1-k/kmax):
* temperature (k, kmax) = kT * (1 - k/kmax)
* neighbour (s) : Pick a random city u > 0 .  Pick a random neighbour city v > 0 of u , among u's 8 (max) neighbours on the grid. Swap u and v in s . This gives the new state ''s_next''.
* kmax = 1000_000
* s0 = a random permutation


For k = 0 to kmax by step kmax/10 , display k, T, E(s). Display the final state s_final, and E(s_final).

You will see that the Energy may grow to a local optimum, before decreasing to a global optimum.

'''Illustrated example''' 
	[http://www.echolalie.org/echolisp/annealing.html Temperature charts]
	
'''Numerical example''' 

```txt

kT = 1
E(s0) = 529.9158

k:  0         T:  1       Es:  529.9158
k:  100000    T:  0.9     Es:  201.1726
k:  200000    T:  0.8     Es:  178.1723
k:  300000    T:  0.7     Es:  154.7069
k:  400000    T:  0.6     Es:  158.1412 <== local optimum
k:  500000    T:  0.5     Es:  133.856
k:  600000    T:  0.4     Es:  129.5684
k:  700000    T:  0.3     Es:  112.6919
k:  800000    T:  0.2     Es:  105.799
k:  900000    T:  0.1     Es:  102.8284
k:  1000000   T:  0       Es:  102.2426

E(s_final) =    102.2426    
Path  s_final =   ( 0 10 11 21 31 20 30 40 50 60 70 80 90 91 81 71 73 83 84 74 64 54 55 65 75 76 66
 67 77 78 68 58 48 47 57 56 46 36 37 27 26 16 15 5 6 7 17 18 8 9 19 29 28 38 39 49 59 69 
79 89 99 98 88 87 97 96 86 85 95 94 93 92 82 72 62 61 51 41 42 52 63 53 43 32 22 12 13 
23 33 34 44 45 35 25 24 14 4 3 2 1 0)  

```


'''Extra credit'''

Tune the parameters kT, kmax, or use different temperature() and/or neighbour() functions to demonstrate a quicker convergence, or a better optimum.


## EchoLisp


```scheme

(lib 'math)
;; distances
(define (d ci cj) 
	(distance (% ci 10) (quotient ci 10)  (% cj 10) (quotient cj 10)))
(define _dists 
	(build-vector 10000 (lambda (ij) (d  (quotient ij 100) (% ij 100)))))
(define-syntax-rule  (dist ci cj)
		[_dists (+ ci (* 100 cj))])
	
;; E(s) = length(path)
(define (Es path)
	(define lpath (vector->list path))
	(for/sum ((ci lpath) (cj (rest lpath))) (dist ci cj)))
	
;; temperature() function
(define (T k kmax kT)
		(* kT (- 1  (// k kmax))))
#|
;; alternative temperature()
;; must be decreasing with k increasing and → 0
(define (T k kmax kT)
	(* kT (- 1  (sin (* PI/2  (// k kmax))))))
|#

;; ∆E = Es_new - Es_old >  0
;; probability to move if ∆E > 0,  → 0 when T → 0 (frozen state)
(define (P ∆E k kmax kT)
		(exp (// (- ∆E ) (T k kmax kT))))
		
;;  ∆E from path ( .. a u b .. c v d ..) to (.. a v b ... c u d ..)
;;  ∆E before swapping (u,v)
;;  Quicker than Es(s_next) - Es(s)

(define (dE s u v)
;;old
		(define a (dist [s (1- u)] [s u]))
		(define b (dist [s (1+ u)] [s u]))
		(define c (dist [s (1- v)] [s v]))
		(define d  (dist [s (1+ v)] [s v]))
;; new
		(define na (dist [s (1- u)] [s v]))
		(define nb (dist [s (1+ u)] [s v]))
		(define nc (dist [s (1- v)] [s u]))
		(define nd (dist [s (1+ v)] [s u]))
				
		(cond 
		((= v (1+ u)) (- (+ na nd) (+ a d)))
		((= u (1+ v)) (- (+ nc nb) (+ c b)))
		(else (- (+ na nb nc nd) (+ a b c d)))))

;; all 8 neighbours
(define dirs #(1 -1 10 -10 9 11 -11 -9))

(define (sa  kmax (kT 10))
	(define s (list->vector (cons 0 (append (shuffle (range 1 100)) 0))))
	(printf "E(s0) %d" (Es s)) ;; random starter
	(define Emin (Es s)) ;; E0
	
	(for ((k kmax))
	(when (zero? (% k (/ kmax 10)))
		(printf "k: %10d T: %8.4d Es: %8.4d" k  (T k kmax kT) (Es s))
		)
		
		(define u (1+ (random 99))) ;; city index 1 99
		(define cv (+ [s u] [dirs (random 8)])) ;; city number
		#:continue (or (> cv 99) (<= cv 0))
		#:continue (> (dist [s u] cv) 5) ;; check true neighbour (eg 0 9)
		(define v (vector-index cv s 1)) ;; city index
		
		(define ∆e (dE s u v))
		(when (or 
			(< ∆e 0)  ;; always move if negative
			(>= (P ∆e k kmax kT) (random)))
				(vector-swap! s u v)
				(+= Emin ∆e))
			
		;; (assert  (= (round Emin) (round (Es s))))
		) ;; for
		
		(printf "k: %10d T: %8.4d Es: %8.4d" kmax  (T (1- kmax) kmax kT) (Es s))
		(s-plot s 0)
		(printf "E(s_final) %d" Emin)
		(writeln 'Path s))

```

```txt

(sa 1000000 1)

E(s0) 501.0909

k:  0         T:  1       Es:  501.0909
k:  100000    T:  0.9     Es:  167.3632
k:  200000    T:  0.8     Es:  160.7791
k:  300000    T:  0.7     Es:  166.8746
k:  400000    T:  0.6     Es:  142.579
k:  500000    T:  0.5     Es:  131.0657
k:  600000    T:  0.4     Es:  116.9214
k:  700000    T:  0.3     Es:  110.8569
k:  800000    T:  0.2     Es:  103.3137
k:  900000    T:  0.1     Es:  102.4853
k:  1000000   T:  0       Es:  102.4853

E(s_final)     102.4853    
Path     #( 0 10 20 30 40 50 60 70 71 61 62 53 63 64 54 44 45 55 65
 74 84 83 73 72 82 81 80 90 91 92 93 94 95 85 75 76 86 96 97 98 99
 88 89 79 69 59 49 48 47 57 58 68 78 87 77 67 66 56 46 36 35 25 24
 34 33 32 43 42 52 51 41 31 21 11 12 22 23 13 14 15 16 17 26 27 37 38
 39 29 28 18 19 9 8 7 6 5 4 3 2 1 0)

```



## Go

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

var (
    dists = calcDists()
    dirs  = [8]int{1, -1, 10, -10, 9, 11, -11, -9} // all 8 neighbors
)

// distances
func calcDists() []float64 {
    dists := make([]float64, 10000)
    for i := 0; i < 10000; i++ {
        ab, cd := math.Floor(float64(i)/100), float64(i%100)
        a, b := math.Floor(ab/10), float64(int(ab)%10)
        c, d := math.Floor(cd/10), float64(int(cd)%10)
        dists[i] = math.Hypot(a-c, b-d)
    }
    return dists
}

// index into lookup table of float64s
func dist(ci, cj int) float64 {
    return dists[cj*100+ci]
}

// energy at s, to be minimized
func Es(path []int) float64 {
    d := 0.0
    for i := 0; i < len(path)-1; i++ {
        d += dist(path[i], path[i+1])
    }
    return d
}

// temperature function, decreases to 0
func T(k, kmax, kT int) float64 {
    return (1 - float64(k)/float64(kmax)) * float64(kT)
}

// variation of E, from state s to state s_next
func dE(s []int, u, v int) float64 {
    su, sv := s[u], s[v]
    // old
    a, b, c, d := dist(s[u-1], su), dist(s[u+1], su), dist(s[v-1], sv), dist(s[v+1], sv)
    // new
    na, nb, nc, nd := dist(s[u-1], sv), dist(s[u+1], sv), dist(s[v-1], su), dist(s[v+1], su)
    if v == u+1 {
        return (na + nd) - (a + d)
    } else if u == v+1 {
        return (nc + nb) - (c + b)
    } else {
        return (na + nb + nc + nd) - (a + b + c + d)
    }
}

// probability to move from s to s_next
func P(deltaE float64, k, kmax, kT int) float64 {
    return math.Exp(-deltaE / T(k, kmax, kT))
}

func sa(kmax, kT int) {
    rand.Seed(time.Now().UnixNano())
    temp := make([]int, 99)
    for i := 0; i < 99; i++ {
        temp[i] = i + 1
    }
    rand.Shuffle(len(temp), func(i, j int) {
        temp[i], temp[j] = temp[j], temp[i]
    })
    s := make([]int, 101) // all 0 by default
    copy(s[1:], temp)     // random path from 0 to 0
    fmt.Println("kT =", kT)
    fmt.Printf("E(s0) %f\n\n", Es(s)) // random starter
    Emin := Es(s)                     // E0
    for k := 0; k <= kmax; k++ {
        if k%(kmax/10) == 0 {
            fmt.Printf("k:%10d   T: %8.4f   Es: %8.4f\n", k, T(k, kmax, kT), Es(s))
        }
        u := 1 + rand.Intn(99)          // city index 1 to 99
        cv := s[u] + dirs[rand.Intn(8)] // city number
        if cv <= 0 || cv >= 100 {       // bogus city
            continue
        }
        if dist(s[u], cv) > 5 { // check true neighbor (eg 0 9)
            continue
        }
        v := s[cv] // city index
        deltae := dE(s, u, v)
        if deltae < 0 || // always move if negative
            P(deltae, k, kmax, kT) >= rand.Float64() {
            s[u], s[v] = s[v], s[u]
            Emin += deltae
        }
    }
    fmt.Printf("\nE(s_final) %f\n", Emin)
    fmt.Println("Path:")
    // output final state
    for i := 0; i < len(s); i++ {
        if i > 0 && i%10 == 0 {
            fmt.Println()
        }
        fmt.Printf("%4d", s[i])
    }
    fmt.Println()
}

func main() {
    sa(1e6, 1)
}
```


Sample run:

```txt

kT = 1
E(s0) 520.932463

k:         0   T:   1.0000   Es: 520.9325
k:    100000   T:   0.9000   Es: 185.1279
k:    200000   T:   0.8000   Es: 167.7657
k:    300000   T:   0.7000   Es: 158.6923
k:    400000   T:   0.6000   Es: 151.6564
k:    500000   T:   0.5000   Es: 139.9185
k:    600000   T:   0.4000   Es: 132.9964
k:    700000   T:   0.3000   Es: 121.8962
k:    800000   T:   0.2000   Es: 120.0445
k:    900000   T:   0.1000   Es: 116.8476
k:   1000000   T:   0.0000   Es: 116.5565

E(s_final) 116.556509
Path:
   0  11  21  31  41  51  52  61  62  72
  82  73  74  64  44  45  55  54  63  53
  42  32  43  33  35  34  24  23  22  13
  12   2   3   4  14  25  26   7   6  16
  15   5  17  27  36  46  56  66  65  75
  77  78  68  69  59  49  39  38  37  28
  29  19   9   8  18  47  48  58  57  67
  76  86  85  95  96  97  87  88  79  89
  99  98  84  94  83  93  92  91  90  80
  81  71  70  60  50  40  30  20  10   1
   0

```



## J


Implementation:


```J
dist=: +/&.:*:@:-"1/~10 10#:i.100

satsp=:4 :0
  kT=. 1
  pathcost=. [: +/ 2 {&y@<\ 0 , ] , 0:
  neighbors=. 0 (0}"1) y e. 1 2{/:~~.,y
  s=. (?~#y)-.0
  d=. pathcost s
  step=. x%10
  for_k. i.x+1 do.
    T=. kT*1-k%x
    u=. ({~ ?@#)s
    v=. ({~ ?@#)I.u{neighbors
    sk=. (<s i.u,v) C. s
    dk=. pathcost sk
    dE=. dk-d
    if. (^-dE%T) >?0 do.
      s=.sk
      d=.dk
    end.
    if. 0=step|k do.
      echo k,T,d
    end.
  end.
  0,s,0
)
```


Notes:

E(s_final) gets displayed on the kmax progress line.

We do not do anything special for negative deltaE because the exponential will be greater than 1 for that case and that will always be greater than our random number from the range 0..1.

Also, while we leave connection distances (and, thus, number of cities) as a parameter, some other aspects of this problem made more sense when included in the implementation:

We leave city 0 out of our data structure, since it can't appear in the middle of our path. But we bring it back in when computing path distance.

Neighbors are any city which have one of the two closest non-zero distances from the current city (and specifically excluding city 0, since that is anchored as our start and end city).

Sample run:


```J
   1e6 satsp dist
0 1 538.409
100000 0.9 174.525
200000 0.8 165.541
300000 0.7 173.348
400000 0.6 168.188
500000 0.5 134.983
600000 0.4 121.585
700000 0.3 111.443
800000 0.2 101.657
900000 0.1 101.657
1e6 0 101.657
0 1 2 3 4 13 23 24 34 44 43 33 32 31 41 42 52 51 61 62 53 54 64 65 55 45 35 25 15 14 5 6 7 17 16 26 27 37 36 46 47 48 38 28 18 8 9 19 29 39 49 59 69 79 78 68 58 57 56 66 67 77 76 75 85 86 87 88 89 99 98 97 96 95 94 84 74 73 63 72 82 83 93 92 91 90 80 81 71 70 60 50 40 30 20 21 22 12 11 10 0
```



## Julia

'''Module''':

```julia
module TravelingSalesman

using Random, Printf

# Eₛ: length(path)
Eₛ(distances, path) = sum(distances[ci, cj] for (ci, cj) in zip(path, Iterators.drop(path, 1)))
# T: temperature
T(k, kmax, kT) = kT * (1 - k / kmax)
# Alternative temperature:
#T(k, kmax, kT) = kT * (1 - sin(π / 2 * k / kmax))

# ΔE = Eₛ_new - Eₛ_old > 0
# Prob. to move if ΔE > 0, → 0 when T → 0 (fronzen state)
P(ΔE, k, kmax, kT) = exp(-ΔE / T(k, kmax, kT))

# ∆E from path ( .. a u b .. c v d ..) to (.. a v b ... c u d ..)
# ∆E before swapping (u,v)
# Quicker than Eₛ(s_next) - Eₛ(path)
function dE(distances, path, u, v)
    a = distances[path[u - 1], path[u]]
    b = distances[path[u + 1], path[u]]
    c = distances[path[v - 1], path[v]]
    d = distances[path[v + 1], path[v]]

    na = distances[path[u - 1], path[v]]
    nb = distances[path[u + 1], path[v]]
    nc = distances[path[v - 1], path[u]]
    nd = distances[path[v + 1], path[u]]

    if v == u + 1
        return (na + nd) - (a + d)
    elseif u == v + 1
        return (nc + nb) - (c + b)
    else
        return (na + nb + nc + nd) - (a + b + c + d)
    end
end

const dirs = [1, -1, 10, -10, 9, 11, -11, -9]

function _prettypath(path)
    r = IOBuffer()
    for g in Iterators.partition(path, 10)
        println(r, join(lpad.(g, 3), ", "))
    end
    return String(take!(r))
end

function findpath(distances, kmax, kT)
    n = size(distances, 1)
    path = vcat(1, shuffle(2:n), 1)
    Emin = Eₛ(distances, path)
    @printf("\n# Entropy(s₀) = %10.2f\n", Emin)
    println("# Random path: \n", _prettypath(path))

    for k in Base.OneTo(kmax)
        if iszero(k % (kmax ÷ 10))
            @printf("k: %10d | T: %8.4f | Eₛ: %8.4f\n", k, T(k, kmax, kT), Eₛ(distances, path))
        end
        u = rand(2:n)
        v = path[u] + rand(dirs)
        v ∈ 2:n || continue

        δE = dE(distances, path, u, v)
        if δE < 0 || P(δE, k, kmax, kT) ≥ rand()
            path[u], path[v] = path[v], path[u]
            Emin += δE
        end
    end

    @printf("k: %10d | T: %8.4f | Eₛ: %8.4f\n", kmax, T(kmax, kmax, kT), Eₛ(distances, path))
    println("\n# Found path:\n", _prettypath(path))
    return path
end

end  # module TravelingSalesman
```


'''Main''':

```julia
distance(a, b) = sqrt(sum((a .- b) .^ 2))
const _citydist = collect(distance((ci % 10, ci ÷ 10), (cj % 10, cj ÷ 10)) for ci in 1:100, cj in 1:100)

TravelingSalesman.findpath(_citydist, 1_000_000, 1)
```


```txt
# Entropy(s₀) =     521.86
# Random path:
  1,   2,  11,  80,  78,  73,  68,  19,  43,  69
 86,  79,  66,  67,  77,  96,  26,  62,  60,  98
 71,   3,  59,  37,  18,  40,  34,  92,  97,   6
 84,  94,  29,  63,  36,  50,  87,  45,  83,  90
 76,  28,  15,  38,  91,  58,  47,  44,  85,  17
 25,  33,  31,  99,  27,  74,  53,  95,  16,  13
 42,  88,   8,   4,   7,  64,  54,   9,  14,  41
  5,  81,  65,  23,  75, 100,  89,  51,  20,  48
 82,  12,  21,  55,  24,  70,  49,  10,  35,  72
 52,  22,  61,  32,  46,  57,  30,  93,  39,  56
  1

k:     100000 | T:   0.9000 | Eₛ: 184.4448
k:     200000 | T:   0.8000 | Eₛ: 175.3662
k:     300000 | T:   0.7000 | Eₛ: 169.0505
k:     400000 | T:   0.6000 | Eₛ: 160.8328
k:     500000 | T:   0.5000 | Eₛ: 147.1973
k:     600000 | T:   0.4000 | Eₛ: 132.9186
k:     700000 | T:   0.3000 | Eₛ: 126.9931
k:     800000 | T:   0.2000 | Eₛ: 122.0656
k:     900000 | T:   0.1000 | Eₛ: 119.7924
k:    1000000 | T:   0.0000 | Eₛ: 119.7924
k:    1000000 | T:   0.0000 | Eₛ: 119.7924

# Found path:
  1,   2,  12,  13,   3,   4,   6,   7,   8,   9
 19,  18,  17,   5,  14,  15,  16,  27,  28,  29
 39,  38,  26,  25,  24,  23,  22,  10,  21,  20
 30,  31,  32,  33,  34,  35,  36,  37,  49,  48
 47,  46,  45,  44,  43,  42,  41,  40,  50,  51
 52,  53,  54,  55,  56,  57,  58,  59,  69,  68
 67,  65,  64,  63,  62,  61,  71,  60,  70,  80
 81,  82,  72,  73,  74,  66,  78,  79,  89,  99
 98,  97,  96,  95,  94,  85,  86,  87,  88,  77
 76,  75,  84,  83,  93,  92,  91, 100,  90,  11
  1
```


## Nim


```Nim
import math, random, sugar, strformat
from times import cpuTime

const
  kT = 1
  kMax = 1_000_000

proc randomNeighbor(x: int): int =
  case x
  of 0:
    rand([1, 10, 11])
  of 9:
    rand([8, 18, 19])
  of 90:
    rand([80, 81, 91])
  of 99:
    rand([88, 89, 98])
  elif x > 0 and x < 9:   # top ceiling
    rand [x-1, x+1, x+9, x+10, x+11]
  elif x > 90 and x < 99: # bottom floor
    rand [x-11, x-10, x-9, x-1, x+1]
  elif x mod 10 == 0:     # left wall
    rand([x-10, x-9, x+1, x+10, x+11])
  elif (x+1) mod 10 == 0: # right wall
    rand([x-11, x-10, x-1, x+9, x+10])
  else: # center
    rand([x-11, x-10, x-9, x-1, x+1, x+9, x+10, x+11])

proc neighbor(s: seq[int]): seq[int] =
  result = s
  var city = rand s
  var cityNeighbor = city.randomNeighbor
  while cityNeighbor == 0 or city == 0:
    city = rand s
    cityNeighbor = city.randomNeighbor
  result[s.find city].swap result[s.find cityNeighbor]

func distNeighbor(a, b: int): float =
  template divmod(a: int): (int, int) = (a div 10, a mod 10)
  let
    (diva, moda) = a.divmod
    (divb, modb) = b.divmod
  hypot((diva-divb).float, (moda-modb).float)

func temperature(k, kmax: float): float =
  kT * (1 - (k / kmax))

func pdelta(eDelta, temp: float): float =
  if eDelta < 0: 1.0
  else: exp(-eDelta / temp)

func energy(path: seq[int]): float =
  var sum = 0.distNeighbor path[0]
  for i in 1 ..< path.len:
    sum += path[i-1].distNeighbor(path[i])
  sum + path[^1].distNeighbor 0

proc main =
  randomize()
  var
    s = block:
      var x = lc[x | (x <- 0 .. 99), int]
      template shuffler: int = rand(1 .. x.len-1)
      for i in 1 .. x.len-1:
        x[i].swap x[shuffler()]
      x
  let startTime = cpuTime()
  echo fmt"E(s0): {energy s:6.4f}"
  for k in 0 .. kMax:
    var
      temp = temperature(float k, float kMax)
      lastenergy = energy s
      newneighbor = s.neighbor
      newenergy = newneighbor.energy
    if k mod (kMax div 10) == 0:
      echo fmt"k: {k:7} T: {temp:6.2f} Es: {lastenergy:6.4f}"
    var deltaEnergy = newenergy - lastenergy
    if pDelta(deltaEnergy, temp) >= rand(1.0):
      s = newneighbor

  s.add 0
  echo fmt"E(sFinal): {energy s:6.4f}"
  echo fmt"path: {s}"
  #echo fmt"ended after: {cpuTime() - startTime}"

main()
```


Compile and run: 
```txt
nim c -r -d:release --opt:speed travel_sa.nim
```

Sample run:

```txt

E(s0): 505.1591
k:       0 T:   1.00 Es: 505.1591
k:  100000 T:   0.90 Es: 196.5216
k:  200000 T:   0.80 Es: 165.6735
k:  300000 T:   0.70 Es: 159.3411
k:  400000 T:   0.60 Es: 144.8330
k:  500000 T:   0.50 Es: 131.7888
k:  600000 T:   0.40 Es: 127.6914
k:  700000 T:   0.30 Es: 113.9280
k:  800000 T:   0.20 Es: 104.7279
k:  900000 T:   0.10 Es: 103.3137
k: 1000000 T:   0.00 Es: 103.3137
E(sFinal): 103.3137
path: @[0, 10, 11, 22, 21, 20, 30, 31, 41, 40, 50, 51, 61, 60, 70, 71, 81, 80, 90, 91, 92, 93, 82, 83, 73, 72, 62, 63, 53, 52, 42, 32, 33, 23, 13, 14, 24, 34, 35, 25, 15, 16, 26, 36, 47, 48, 38, 39, 49, 59, 58, 57, 68, 69, 79, 89, 99, 98, 97, 96, 95, 94, 84, 74, 75, 85, 86, 87, 88, 78, 77, 67, 76, 66, 65, 64, 54, 43, 44, 45, 55, 56, 46, 37, 27, 28, 29, 19, 9, 8, 18, 17, 7, 6, 5, 4, 3, 2, 12, 1, 0]

```


## Phix

Note that the standard builtin exp() suffered occasional overflows, so this uses b_a_exp() from bigatom.e, but
it does make it much slower.

```Phix
function hypot(atom a,b) return sqrt(a*a+b*b) end function

function calc_dists()
    sequence dists = repeat(0,10000)
    for abcd=1 to 10000 do
        integer {ab,cd} = {floor(abcd/100),mod(abcd,100)},
                {a,b,c,d} = {floor(ab/10),mod(ab,10),
                             floor(cd/10),mod(cd,10)}
        dists[abcd] = hypot(a-c,b-d)
    end for
    return dists
end function
constant dists = calc_dists()

function dist(integer ci,cj) return dists[cj*100+ci] end function
 
function Es(sequence path)
    atom d = 0
    for i=1 to length(path)-1 do
        d += dist(path[i],path[i+1])
    end for
    return d
end function

-- temperature() function
function T(integer k, kmax, kT) return (1-k/kmax)*kT end function

include bigatom.e -- (just for b_a_exp())
 
-- deltaE = Es_new - Es_old >  0
-- probability to move if deltaE > 0, -->0 when T --> 0 (frozen state)
function P(atom deltaE, integer k, kmax, kT) return b_a_exp(-deltaE/T(k,kmax,kT)) end function
 
--  deltaE from path ( .. a u b .. c v d ..) to (.. a v b ... c u d ..)
function dE(sequence s, integer u,v)
-- (note that u,v are 0-based, but 1..99 here)
--  integer sum1 = s[u-1], su = s[u], sup1 = s[u+1],
--          svm1 = s[v-1], sv = s[v], svp1 = s[v+1]
    integer sum1 = s[u], su = s[u+1], sup1 = s[u+2],
            svm1 = s[v], sv = s[v+1], svp1 = s[v+2]
    -- old
    atom {a,b,c,d}:={dist(sum1,su), dist(su,sup1), dist(svm1,sv), dist(sv,svp1)},
    -- new
     {na,nb,nc,nd}:={dist(sum1,sv), dist(sv,sup1), dist(svm1,su), dist(su,svp1)}
 
    return iff(v==u+1?(na+nd)-(a+d):
           iff(u==v+1?(nc+nb)-(c+b):
              (na+nb+nc+nd)-(a+b+c+d)))
end function
 
-- all 8 neighbours
constant dirs = {1, -1, 10, -10, 9, 11, -11, -9}
 
procedure sa(integer kmax, kT=10)
    sequence s = 0&shuffle(tagset(99))&0
    atom Emin:=Es(s)            -- E0
    printf(1,"E(s0) %f\n",Emin) -- random starter
 
    for k=0 to kmax do
        if mod(k,kmax/10)=0 then
            printf(1,"k:%,10d T: %8.4f Es: %8.4f\n",{k,T(k,kmax,kT),Es(s)})
        end if
        integer u = rand(99),               -- city index 1 99
                cv = s[u+1]+dirs[rand(8)]   -- city number
        if cv>0 and cv<100                  -- not bogus city
        and dist(s[u+1],cv)<5 then          -- and true neighbour
            integer v = s[cv+1]             -- city index
            atom deltae := dE(s,u,v);
            if deltae<0     -- always move if negative
            or P(deltae,k,kmax,kT)>=rnd() then
                {s[u+1],s[v+1]} = {s[v+1],s[u+1]}
                Emin += deltae
            end if
        end if
    end for
    printf(1,"E(s_final) %f\n",Emin)
    printf(1,"Path:\n")
    pp(s,{pp_IntFmt,"%2d",pp_StrFmt,-2})
end procedure
sa(1_000_000,1)
```

```txt

E(s0) 515.164811
k:         0 T:   1.0000 Es: 515.1648
k:   100,000 T:   0.9000 Es: 189.3123
k:   200,000 T:   0.8000 Es: 198.7498
k:   300,000 T:   0.7000 Es: 158.2189
k:   400,000 T:   0.6000 Es: 165.4813
k:   500,000 T:   0.5000 Es: 156.3467
k:   600,000 T:   0.4000 Es: 142.7928
k:   700,000 T:   0.3000 Es: 128.0352
k:   800,000 T:   0.2000 Es: 121.7794
k:   900,000 T:   0.1000 Es: 121.2328
k: 1,000,000 T:   0.0000 Es: 121.1291
E(s_final) 121.129115
Path:
{ 0,10,62,63,64,65,76,75,84,85,95,86,96,97,87,77,67,66,56,46,47,48,49,59,69,
 79,89,99,98,88,78,68,58,57,37,38,27,26,36,35,45,55,54,53,52,43,33,23,22,32,
 42,41,51,61,60,50,40,30,31,21,20,11,12, 2, 3, 4, 5, 6,17,18,28,39,29,19, 9,
  8, 7,16,15,24,44,74,83,93,94,92,91,71,70,90,80,81,82,72,73,34,25,14,13, 1,
  0}

```



## Sidef

```ruby
module TravelingSalesman {

    # Eₛ: length(path)
    func Eₛ(distances, path) {
        var total = 0
        [path, path.slice(1)].zip {|ci,cj|
            total += distances[ci-1][cj-1]
        }
        total
    }

    # T: temperature
    func T(k, kmax, kT) { kT * (1 - k/kmax) }

    # ΔE = Eₛ_new - Eₛ_old > 0
    # Prob. to move if ΔE > 0, → 0 when T → 0 (fronzen state)
    func P(ΔE, k, kmax, kT) { exp(-ΔE / T(k, kmax, kT)) }

    # ∆E from path ( .. a u b .. c v d ..) to (.. a v b ... c u d ..)
    # ∆E before swapping (u,v)
    # Quicker than Eₛ(s_next) - Eₛ(path)
    func dE(distances, path, u, v) {

        var a = distances[path[u-1]-1][path[u]-1]
        var b = distances[path[u+1]-1][path[u]-1]
        var c = distances[path[v-1]-1][path[v]-1]
        var d = distances[path[v+1]-1][path[v]-1]

        var na = distances[path[u-1]-1][path[v]-1]
        var nb = distances[path[u+1]-1][path[v]-1]
        var nc = distances[path[v-1]-1][path[u]-1]
        var nd = distances[path[v+1]-1][path[u]-1]

        if (v == u+1) {
            return ((na+nd) - (a+d))
        }

        if (u == v+1) {
            return ((nc+nb) - (c+b))
        }

        return ((na+nb+nc+nd) - (a+b+c+d))
    }

    const dirs = [1, -1, 10, -10, 9, 11, -11, -9]

    func _prettypath(path) {
        path.slices(10).map { .map{ "%3s" % _ }.join(', ') }.join("\n")
    }

    func findpath(distances, kmax, kT) {

        const n = distances.len
        const R = 2..n

        var path = [1, R.shuffle..., 1]
        var Emin = Eₛ(distances, path)

        printf("# Entropy(s₀) = s%10.2f\n", Emin)
        printf("# Random path:\n%s\n\n", _prettypath(path))

        for k in (1 .. kmax) {

            if (k % (kmax//10) == 0) {
                printf("k: %10d | T: %8.4f | Eₛ: %8.4f\n", k, T(k, kmax, kT), Eₛ(distances, path))
            }

            var u = R.rand
            var v = (path[u-1] + dirs.rand)
            v ~~ R || next

            var δE = dE(distances, path, u-1, v-1)
            if ((δE < 0) || (P(δE, k, kmax, kT) >= 1.rand)) {
                path.swap(u-1, v-1)
                Emin += δE
            }
        }

        printf("k: %10d | T: %8.4f | Eₛ: %8.4f\n", kmax, T(kmax, kmax, kT), Eₛ(distances, path))
        say ("\n# Found path:\n", _prettypath(path))
        return path
    }
}

var citydist = {|ci|
    { |cj|
        var v1 = Vec(ci%10, ci//10)
        var v2 = Vec(cj%10, cj//10)
        v1.dist(v2)
    }.map(1..100)
}.map(1..100)

TravelingSalesman::findpath(citydist, 1e6, 1)
```


```txt

# Entropy(s₀) =     520.29
# Random path:
  1,  10,  79,  52,  24,   9,  58,  11,  42,   4
 15,  87,  62,  88,  21,  91,  99,  84,  61,  14
  5,  17,  33,  95,  74,  31,  40,  13,  37,  69
  6,  22,  97,  45,  56,  63,  75,  83,  53,  41
  3,  47,  89,  80,  78,  98,  46,  18,  25,  51
 93,  16,  50,  30,  48,   8,  66,  68,  59,  73
 49,  96,  36,  32, 100,  27,  76,  44,  64,  39
 90,  82,  20,  12,  54,  86,  29,  81,  26,  72
 60,  94,  35,  92,  43,   7,  85,  55,  28,  57
 23,  34,  65,  71,  38,   2,  77,  70,  19,  67
  1

k:     100000 | T:   0.9000 | Eₛ: 185.1809
k:     200000 | T:   0.8000 | Eₛ: 168.6262
k:     300000 | T:   0.7000 | Eₛ: 146.5948
k:     400000 | T:   0.6000 | Eₛ: 140.1441
k:     500000 | T:   0.5000 | Eₛ: 129.5132
k:     600000 | T:   0.4000 | Eₛ: 132.8942
k:     700000 | T:   0.3000 | Eₛ: 124.2865
k:     800000 | T:   0.2000 | Eₛ: 120.0859
k:     900000 | T:   0.1000 | Eₛ: 115.0771
k:    1000000 | T:   0.0000 | Eₛ: 114.9728
k:    1000000 | T:   0.0000 | Eₛ: 114.9728

# Found path:
  1,   2,  13,   3,   4,   5,   6,   7,   8,   9
 19,  29,  18,  28,  27,  17,  16,  26,  25,  15
 14,  24,  23,  12,  11,  10,  20,  21,  30,  40
 41,  31,  32,  44,  45,  46,  47,  48,  49,  39
 38,  37,  36,  35,  34,  42,  51,  50,  60,  61
 52,  53,  54,  55,  56,  57,  58,  59,  69,  68
 77,  67,  66,  65,  64,  62,  72,  71,  70,  80
 81,  82,  74,  75,  76,  87,  88,  78,  79,  89
 99,  98,  97,  96,  86,  85,  83,  91,  90, 100
 92,  93,  94,  95,  84,  73,  63,  43,  33,  22
  1

```



## zkl

```zkl
var [const] _dists=(0d10_000).pump(List,fcn(abcd){ // two points (a,b) & (c,d), calc distance 
   ab,cd,a,b,c,d:=abcd/100, abcd%100, ab/10,ab%10, cd/10,cd%10;
   (a-c).toFloat().hypot(b-d)
});
fcn dist(ci,cj){ _dists[cj*100 + ci] }  // index into lookup table of floats
 
fcn Es(path)   // E(s) = length(path): E(a,b,c)--> dist(a,b) + dist(b,c)
   { d:=Ref(0.0); path.reduce('wrap(a,b){ d.apply('+,dist(a,b)); b }); d.value }
 
// temperature() function
fcn T(k,kmax,kT){ (1.0 - k.toFloat()/kmax)*kT }
 
// deltaE = Es_new - Es_old >  0
// probability to move if deltaE > 0, -->0 when T --> 0 (frozen state)
fcn P(deltaE,k,kmax,kT){ (-deltaE/T(k,kmax,kT)).exp() }  //-->Float
 
//  deltaE from path ( .. a u b .. c v d ..) to (.. a v b ... c u d ..)
//  deltaE before swapping (u,v) 
fcn dE(s,u,v){ su,sv:=s[u],s[v];  //-->Float
   // old
   a,b,c,d:=dist(s[u-1],su), dist(s[u+1],su), dist(s[v-1],sv), dist(s[v+1],sv);
   // new
   na,nb,nc,nd:=dist(s[u-1],sv), dist(s[u+1],sv), dist(s[v-1],su), dist(s[v+1],su);

   if     (v==u+1) (na+nd) - (a+d);
   else if(u==v+1) (nc+nb) - (c+b);
   else            (na+nb+nc+nd) - (a+b+c+d);
}
 
// all 8 neighbours
var [const] dirs=ROList(1, -1, 10, -10, 9, 11, -11, -9),
    fmt="k:%10,d T: %8.4f Es: %8.4f".fmt;  // since we use it twice
 
fcn sa(kmax,kT=10){
   s:=List(0, [1..99].walk().shuffle().xplode(), 0);  // random path from 0 to 0
   println("E(s0) %f".fmt(Es(s))); // random starter
   Emin:=Es(s);		// E0
 
   foreach k in (kmax){
      if(0==k%(kmax/10)) println(fmt(k,T(k,kmax,kT),Es(s)));
      u:=(1).random(100);		// city index 1 99
      cv:=s[u] + dirs[(0).random(8)];	// city number
      if(not (0<cv<100))  continue;	// bogus city
      if(dist(s[u],cv)>5) continue;	// check true neighbour (eg 0 9)
      v:=s.index(cv,1);			// city index
 
      deltae:=dE(s,u,v);
      if(deltae<0 or	// always move if negative
	    P(deltae,k,kmax,kT)>=(0.0).random(1)){
	 s.swap(u,v);
	 Emin+=deltae;
      }
      // (assert  (= (round Emin) (round (Es s))))
   }//foreach
 
   println(fmt(kmax,T(kmax-1,kmax,kT),Es(s)));
   println("E(s_final) %f".fmt(Emin));
   println("Path: ",s.toString(*));
}
```


```zkl
sa(0d1_000_000,1);
```

```txt

E(s0) 540.897080
k:         0 T:   1.0000 Es: 540.8971
k:   100,000 T:   0.9000 Es: 181.5102
k:   200,000 T:   0.8000 Es: 167.1944
k:   300,000 T:   0.7000 Es: 159.0975
k:   400,000 T:   0.6000 Es: 170.2344
k:   500,000 T:   0.5000 Es: 130.9919
k:   600,000 T:   0.4000 Es: 115.3422
k:   700,000 T:   0.3000 Es: 113.9280
k:   800,000 T:   0.2000 Es: 106.7924
k:   900,000 T:   0.1000 Es: 103.7213
k: 1,000,000 T:   0.0000 Es: 103.7213
E(s_final) 103.721349
Path: L(0,10,11,21,20,30,40,50,60,70,80,81,71,72,73,63,52,62,61,51,41,31,32,22,12,13,14,15,25,16,17,18,28,27,26,36,35,45,34,24,23,33,42,43,44,54,53,64,74,84,83,82,90,91,92,93,94,95,85,86,96,97,87,88,98,99,89,79,69,68,78,77,67,66,76,75,65,55,56,46,37,38,48,47,57,58,59,49,39,29,19,9,8,7,6,5,4,3,2,1,0)

```

