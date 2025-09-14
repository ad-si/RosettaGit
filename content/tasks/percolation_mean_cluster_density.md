+++
title = "Percolation/Mean cluster density"
description = ""
date = 2019-06-09T07:41:42Z
aliases = []
[extra]
id = 15850
[taxonomies]
categories = ["task", "Percolation Simulations"]
tags = []
languages = [
  "c",
  "d",
  "echolisp",
  "factor",
  "go",
  "j",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "tcl",
  "zkl",
]
+++

Let <math>c</math> be a 2D boolean square matrix of <math>n \times n</math> values of either <tt>1</tt> or <tt>0</tt> where the
probability of any value being <tt>1</tt> is <math>p</math>, (and of <tt>0</tt> is therefore <math>1-p</math>).
We define a ''cluster'' of <tt>1</tt>'s as being a group of <tt>1</tt>'s connected vertically or
horizontally (i.e., using the [[wp:Von Neumann neighborhood|Von Neumann neighborhood rule]]) and bounded by either <math>0</math> or by the limits of the matrix.
Let the number of such clusters in such a randomly constructed matrix be <math>C_n</math>.

Percolation theory states that <math>K(p)</math> (the mean cluster density) will satisfy <math>K(p) = C_n / n^2</math> as <math>n</math> tends to infinity. For <math>p = 0.5</math>, <math>K(p)</math> is found numerically to approximate <math>0.065770</math>...

## Task

Show the effect of varying <math>n</math> on the accuracy of simulated <math>K(p)</math> for <math>p = 0.5</math> and
for values of <math>n</math> up to at least <math>1000</math>.
Any calculation of <math>C_n</math> for finite <math>n</math> is subject to randomness, so an approximation should be
computed as the average of <math>t</math> runs, where <math>t</math> &ge; <math>5</math>.

For extra credit, graphically show clusters in a <math>15\times 15</math>, <math>p=0.5</math> grid.

Show your output here.

## See also

* [http://mathworld.wolfram.com/s-Cluster.html s-Cluster] on Wolfram mathworld.


## C


```c
#include <stdio.h>
#include <stdlib.h>

int *map, w, ww;

void make_map(double p)
{
	int i, thresh = RAND_MAX * p;
	i = ww = w * w;

	map = realloc(map, i * sizeof(int));
	while (i--) map[i] = -(rand() < thresh);
}

char alpha[] = "+.ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"abcdefghijklmnopqrstuvwxyz";
#define ALEN ((int)(sizeof(alpha) - 3))

void show_cluster(void)
{
	int i, j, *s = map;

	for (i = 0; i < w; i++) {
		for (j = 0; j < w; j++, s++)
			printf(" %c", *s < ALEN ? alpha[1 + *s] : '?');
		putchar('\n');
	}
}

void recur(int x, int v) {
	if (x >= 0 && x < ww && map[x] == -1) {
		map[x] = v;
		recur(x - w, v);
		recur(x - 1, v);
		recur(x + 1, v);
		recur(x + w, v);
	}
}

int count_clusters(void)
{
	int i, cls;

	for (cls = i = 0; i < ww; i++) {
		if (-1 != map[i]) continue;
		recur(i, ++cls);
	}

	return cls;
}

double tests(int n, double p)
{
	int i;
	double k;

	for (k = i = 0; i < n; i++) {
		make_map(p);
		k += (double)count_clusters() / ww;
	}
	return k / n;
}

int main(void)
{
	w = 15;
	make_map(.5);
	printf("width=15, p=0.5, %d clusters:\n", count_clusters());
	show_cluster();

	printf("\np=0.5, iter=5:\n");
	for (w = 1<<2; w <= 1<<14; w<<=2)
		printf("%5d %9.6f\n", w, tests(5, .5));

	free(map);
	return 0;
}
```

```txt

width=15, p=0.5, 23 clusters:
 A . . . B . C C C C . D . E .
 A . . B B . . . . . . . . . .
 A . . . . . F . . G . H . . I
 . . J J J . . K K . L . M . I
 . J J . . . K K K K . M M . .
 . . . . K K . K . K . M . N .
 O O . K K K . K . . . . N N N
 . O O . K K K K K . P . N . .
 Q . . K K . . . K . P . . . .
 . R . K K . . K K . P . . S .
 . . K K . . . . K . P . . . K
 K K K K K . . K K . . T . . K
 K . K . . . U . K . . T . . .
 K . K K K K . K K K . T . . .
 . . K . K . V . K K . . . W .

p=0.5, iter=5:
    4  0.125000
   16  0.083594
   64  0.064453
  256  0.066864
 1024  0.065922
 4096  0.065836
16384  0.065774

```



## D

```d
import std.stdio, std.algorithm, std.random, std.math, std.array,
       std.range, std.ascii;

alias Cell = ubyte;
alias Grid = Cell[][];
enum Cell notClustered = 1; // Filled cell, but not in a cluster.

Grid initialize(Grid grid, in double prob, ref Xorshift rng) nothrow {
    foreach (row; grid)
        foreach (ref cell; row)
            cell = Cell(rng.uniform01 < prob);
    return grid;
}

void show(in Grid grid) {
    immutable static cell2char = " #" ~ letters;
    writeln('+', "-".replicate(grid.length), '+');
    foreach (row; grid) {
        write('|');
        row.map!(c => c < cell2char.length ? cell2char[c] : '@').write;
        writeln('|');
    }
    writeln('+', "-".replicate(grid.length), '+');
}

size_t countClusters(bool justCount=false)(Grid grid)
pure nothrow @safe @nogc {
    immutable side = grid.length;
    static if (justCount)
        enum Cell clusterID = 2;
    else
        Cell clusterID = 1;

    void walk(in size_t r, in size_t c) nothrow @safe @nogc {
        grid[r][c] = clusterID; // Fill grid.

        if (r < side - 1 && grid[r + 1][c] == notClustered) // Down.
            walk(r + 1, c);
        if (c < side - 1 && grid[r][c + 1] == notClustered) // Right.
            walk(r, c + 1);
        if (c > 0 && grid[r][c - 1] == notClustered) // Left.
            walk(r, c - 1);
        if (r > 0 && grid[r - 1][c] == notClustered) // Up.
            walk(r - 1, c);
    }

    size_t nClusters = 0;

    foreach (immutable r; 0 .. side)
        foreach (immutable c; 0 .. side)
            if (grid[r][c] == notClustered) {
                static if (!justCount)
                    clusterID++;
                nClusters++;
                walk(r, c);
            }
    return nClusters;
}

double clusterDensity(Grid grid, in double prob, ref Xorshift rng) {
    return grid.initialize(prob, rng).countClusters!true /
           double(grid.length ^^ 2);
}

void showDemo(in size_t side, in double prob, ref Xorshift rng) {
    auto grid = new Grid(side, side);
    grid.initialize(prob, rng);
    writefln("Found %d clusters in this %d by %d grid:\n",
             grid.countClusters, side, side);
    grid.show;
}

void main() {
    immutable prob = 0.5;
    immutable nIters = 5;
    auto rng = Xorshift(unpredictableSeed);

    showDemo(15, prob, rng);
    writeln;
    foreach (immutable i; iota(4, 14, 2)) {
        immutable side = 2 ^^ i;
        auto grid = new Grid(side, side);
        immutable density = nIters
                            .iota
                            .map!(_ => grid.clusterDensity(prob, rng))
                            .sum / nIters;
        writefln("n_iters=%3d, p=%4.2f, n=%5d, sim=%7.8f",
                 nIters, prob, side, density);
    }
}
```

```txt
Found 26 clusters in this 15 by 15 grid:

+---------------+
| AA B    CCCC  |
|AA D E F CC  G |
|  DDD FF  CC  H|
| I D FF  J   K |
|  L  FF JJJJ   |
|L LLL      J  M|
|LLLLLL   JJJ MM|
|L LL L N  J   M|
|LL    O P J   M|
|LLL QQ R JJ  S |
|LL T  RR  J SSS|
| L   U  V JJ  S|
|  WW  XX JJ YY |
|    XXX   JJ YY|
|ZZ   XXX   JJ  |
+---------------+

n_iters=  5, p=0.50, n=   16, sim=0.09765625
n_iters=  5, p=0.50, n=   64, sim=0.07260742
n_iters=  5, p=0.50, n=  256, sim=0.06679993
n_iters=  5, p=0.50, n= 1024, sim=0.06609497
n_iters=  5, p=0.50, n= 4096, sim=0.06580237
```

Increasing the index i to 15:

```txt
n_iters=  5, p=0.50, n=32768, sim=0.06578374
```



## EchoLisp

We use the canvas bit-map as 2D-matrix. For extra-extra credit, a 800x800 nice cluster tapestry image is shown here : http://www.echolalie.org/echolisp/images/rosetta-clusters-800.png.

```scheme

(define-constant BLACK  (rgb 0 0 0.6))
(define-constant WHITE -1)
;; sets pixels to clusterize to WHITE
;; returns bit-map vector
(define (init-C n p )
    (plot-size n n)
    (define C (pixels->int32-vector )) ;; get canvas bit-map
    (pixels-map (lambda (x y) (if (< (random) p) WHITE BLACK )) C)
    C )

;; random color for new cluster
(define (new-color)
    (hsv->rgb (random) 0.9 0.9))

;; make-region predicate
(define (in-cluster C x y)
    (= (pixel-ref C x y) WHITE))

;; paint all adjacents to (x0,y0) with new color
(define (make-cluster C x0 y0)
                (pixel-set! C x0 y0 (new-color))
                (make-region in-cluster C x0 y0))

;; task
(define (make-clusters (n 400) (p 0.5))
    (define Cn 0)
    (define C null)
        (for ((t 5)) ;; 5 iterations
        (plot-clear)
        (set!  C (init-C n p))
        (for* ((x0 n) (y0 n))
            #:when  (= (pixel-ref C x0 y0) WHITE)
            (set! Cn (1+ Cn))
         (make-cluster C x0 y0)))

    (writeln 'n n  'Cn Cn  'density  (// Cn (* n n) 5) )
    (vector->pixels C)) ;; to screen

```

```txt

n     100      Cn     3420       density     0.0684
n     400      Cn     53246      density     0.0665575
n     600      Cn     118346     density     0.06574778
n     800      Cn     212081     density     0.0662753125
n     1000     Cn     330732     density     0.0661464

```



## Factor


```factor
USING: combinators formatting generalizations kernel math
math.matrices random sequences ;
IN: rosetta-code.mean-cluster-density

CONSTANT: p 0.5
CONSTANT: iterations 5

: rand-bit-matrix ( n probability -- matrix )
    dupd [ random-unit > 1 0 ? ] curry make-matrix ;

: flood-fill ( x y matrix -- )
    3dup ?nth ?nth 1 = [
        [ [ -1 ] 3dip nth set-nth ] [
            {
                [ [ 1 + ] 2dip ]
                [ [ 1 - ] 2dip ]
                [ [ 1 + ] dip ]
                [ [ 1 - ] dip ]
            } [ flood-fill ] map-compose 3cleave
        ] 3bi
    ] [ 3drop ] if ;

: count-clusters ( matrix -- Cn )
    0 swap dup dim matrix-coordinates flip concat [
        first2 rot 3dup ?nth ?nth 1 = [ flood-fill 1 + ]
        [ 3drop ] if
    ] with each ;

: mean-cluster-density ( matrix -- mcd )
    [ count-clusters ] [ dim first sq / ] bi ;

: simulate ( n -- avg-mcd )
    iterations swap [ p rand-bit-matrix mean-cluster-density ]
    curry replicate sum iterations / ;

: main ( -- )
    { 4 64 256 1024 4096 } [
        [ iterations p ] dip dup simulate
        "iterations = %d p = %.1f n = %4d sim = %.5f\n" printf
    ] each ;

MAIN: main
```

```txt

iterations = 5 p = 0.5 n =    4 sim = 0.13750
iterations = 5 p = 0.5 n =   64 sim = 0.07437
iterations = 5 p = 0.5 n =  256 sim = 0.06786
iterations = 5 p = 0.5 n = 1024 sim = 0.06621
iterations = 5 p = 0.5 n = 4096 sim = 0.06589

```



## Go

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

var (
    n_range = []int{4, 64, 256, 1024, 4096}
    M       = 15
    N       = 15
)

const (
    p             = .5
    t             = 5
    NOT_CLUSTERED = 1
    cell2char     = " #abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
)

func newgrid(n int, p float64) [][]int {
    g := make([][]int, n)
    for y := range g {
        gy := make([]int, n)
        for x := range gy {
            if rand.Float64() < p {
                gy[x] = 1
            }
        }
        g[y] = gy
    }
    return g
}

func pgrid(cell [][]int) {
    for n := 0; n < N; n++ {
        fmt.Print(n%10, ") ")
        for m := 0; m < M; m++ {
            fmt.Printf(" %c", cell2char[cell[n][m]])
        }
        fmt.Println()
    }
}

func cluster_density(n int, p float64) float64 {
    cc := clustercount(newgrid(n, p))
    return float64(cc) / float64(n) / float64(n)
}

func clustercount(cell [][]int) int {
    walk_index := 1
    for n := 0; n < N; n++ {
        for m := 0; m < M; m++ {
            if cell[n][m] == NOT_CLUSTERED {
                walk_index++
                walk_maze(m, n, cell, walk_index)
            }
        }
    }
    return walk_index - 1
}

func walk_maze(m, n int, cell [][]int, indx int) {
    cell[n][m] = indx
    if n < N-1 && cell[n+1][m] == NOT_CLUSTERED {
        walk_maze(m, n+1, cell, indx)
    }
    if m < M-1 && cell[n][m+1] == NOT_CLUSTERED {
        walk_maze(m+1, n, cell, indx)
    }
    if m > 0 && cell[n][m-1] == NOT_CLUSTERED {
        walk_maze(m-1, n, cell, indx)
    }
    if n > 0 && cell[n-1][m] == NOT_CLUSTERED {
        walk_maze(m, n-1, cell, indx)
    }
}

func main() {
    rand.Seed(time.Now().Unix())
    cell := newgrid(N, .5)
    fmt.Printf("Found %d clusters in this %d by %d grid\n\n",
        clustercount(cell), N, N)
    pgrid(cell)
    fmt.Println()

    for _, n := range n_range {
        M = n
        N = n
        sum := 0.
        for i := 0; i < t; i++ {
            sum += cluster_density(n, p)
        }
        sim := sum / float64(t)
        fmt.Printf("t=%3d p=%4.2f n=%5d sim=%7.5f\n", t, p, n, sim)
    }
}
```

```txt

Found 29 clusters in this 15 by 15 grid

0)          a a a   b     c
1)  d     e     a       c c     f
2)    g   e e e
3)      h   e       i     j     k
4)  l   h             m m     n
5)  l           o   p     n n n n
6)      q     o o o   r   n
7)          o o     r r       s
8)  t t     o   u     r   s s s s
9)  t t   v   u u   r r   s s s s
0)    t   v   u     r   r   s
1)      w       x   r   r     y y
2)          z   x   r r r r r   y
3)    A   z z     B     r r r r
4)  A A A   z   C       r   r r r

t=  5 p=0.50 n=    4 sim=0.16250
t=  5 p=0.50 n=   64 sim=0.07334
t=  5 p=0.50 n=  256 sim=0.06710
t=  5 p=0.50 n= 1024 sim=0.06619
t=  5 p=0.50 n= 4096 sim=0.06585

```



## J


The first thing this task seems to need is some mechanism of identifying "clusters", using "percolation". We can achieve this by assigning every "1" in a matrix a unique integer value and then defining an operation which combines two numbers - doing nothing unless the second one (the one on the right) is non-zero. If it is non-zero we pick the larger of the two values. <code>(*@[ * >.)</code>

Once we have this, we can identify clusters by propagating information in a single direction through the matrix using this operation, rotating the matrix 90 degrees, and then repeating this combination of operations four times. And, finally, by keeping at this until there's nothing more to be done.


```J
congeal=: |.@|:@((*@[*>.)/\.)^:4^:_
```


Example:


```J>   M=:0.4
?6 6$0
   M
1 0 0 0 0 0
0 0 0 1 0 0
0 0 0 1 0 0
1 1 0 0 0 0
0 0 0 1 0 1
1 1 0 1 1 0
   M*p:i.$M
  2   0 0   0   0   0
  0   0 0  29   0   0
  0   0 0  53   0   0
 67  71 0   0   0   0
  0   0 0 107   0 113
127 131 0 139 149   0
   congeal M*p:i.$M
  2   0 0   0   0   0
  0   0 0  53   0   0
  0   0 0  53   0   0
 71  71 0   0   0   0
  0   0 0 149   0 113
131 131 0 149 149   0
```


We did not have to use primes there - any mechanism for assigning distinct positive integers to the 1s would work. And, in fact, it might be nice if - once we found our clusters - we assigned the smallest distinct positive integers to the clusters. This would allow us to use simple indexing to map the array to characters.


```J
idclust=: $ $ [: (~. i.])&.(0&,)@,@congeal  ] * 1 + i.@$
```


Example use:


```J
   idclust M
1 0 0 0 0 0
0 0 0 2 0 0
0 0 0 2 0 0
3 3 0 0 0 0
0 0 0 4 0 5
6 6 0 4 4 0
   (idclust M) {'.ABCDEFG'
A.....
...B..
...B..
CC....
...D.E
FF.DD.
```


Now we just need a measure of cluster density. Formally cluster density seems to be defined as the number of clusters divided by the total number of elements of the matrix. Thus:


```J
K=: (%&#~ }.@~.)&,
```


Example use:


```J
   K idclust M
0.1666667
```


So we can create a word that performs a simulation experiment, given a probability getting a 1 and the number of rows (and columns) of our square matrix M.


```J>experiment=: K@ idclust@:
 0 ?@$~ ,~
```


Example use:


```J
   0.4 experiment 6
0.1666667
   0.4 experiment 6
0.1944444
```


The task wants us to perform at least five trials for sizes up to 1000 by 1000 with probability of 1 being 0.5:


```J
trials=: 0.5&experiment"0@#
```


Example use:


```J
   6 trials 3
0.1111111 0.1111111 0.2222222 0.1111111 0.1111111 0.3333333
   6 trials 10
0.16 0.12 0.09 0.1 0.1 0.03
   6 trials 30
0.05666667 0.1033333 0.08222222 0.07444444 0.08333333 0.07666667
   6 trials 100
0.069 0.0678 0.0666 0.0677 0.0653 0.0739
   6 trials 300
0.06563333 0.06663333 0.06713333 0.06727778 0.06658889 0.06664444
   6 trials 1000
0.066079 0.066492 0.065847 0.065943 0.066318 0.065998
```


Now for averages (these are different trials from the above):


```J
mean=: +/%#
   mean 8 trials 3
0.1805556
   mean 8 trials 10
0.0875
   mean 8 trials 30
0.07486111
   mean 8 trials 100
0.0690625
   mean 8 trials 300
0.06749861
   mean 8 trials 1000
0.06616738
```


Finally, for the extra credit (thru taken from the [[Loops/Downward_for#J|Loops/Downward for]] task):


```J
thru=: <./ + i.@(+*)@-~
```



```J
   (idclust 0.5 > 0 ?@$~ 15 15) {'.', 'A' thru&.(a.&i.) 'Z'
A.......B..C...
AAAA...D..E.F..
A..A.G.D.D.FFF.
AA..H..DDD.FF.I
AAA...J...FFF..
..AAAA.A.K...AA
LL.A...A..A.AAA
.L.A..AAA.AAAAA
..AA.AAA.AAA.A.
AA.AAAAAA....A.
A.AAAA.AAAA.AA.
AAA...AAA.AAAAA
..AA..A.A...AAA
.M.A.AA.AA..AA.
.MM..A.N..O..A.
```


'''Collected definitions'''


```J
congeal=: |.@|:@((*@[*>.)/\.)^:4^:_
idclust=: $ $ [: (~. i.])&.(0&,)@,@congeal  ] * 1 + i.@$

K=: (%&#~ }.@~.)&,

experiment=: K@ idclust@: > 0 ?@$~ ,~
trials=: 0.5&experiment"0@#

mean=:+/ % #

thru=: <./ + i.@(+*)@-~
```


'''Extra Credit'''


```J
   M=: (* 1+i.@$)?15 15$2
   M
  0   2   3   4   0   6   0   8   0  10  11  12   0   0  15
  0   0  18  19  20   0  22   0   0   0   0   0  28  29   0
 31  32   0  34  35  36  37  38   0   0   0  42   0   0  45
  0   0  48  49   0  51   0   0  54  55   0  57  58   0   0
 61  62  63  64   0   0  67   0  69   0  71  72   0  74   0
  0   0  78  79   0   0  82   0  84  85  86  87  88   0   0
  0  92   0  94   0   0   0   0  99 100 101   0 103   0 105
106 107 108   0   0 111   0   0 114 115 116   0   0   0   0
  0   0   0 124 125 126 127   0   0   0   0   0 133 134 135
  0   0 138   0   0 141   0 143 144 145   0   0   0   0 150
  0 152 153 154   0   0   0 158   0 160   0 162 163 164 165
  0 167 168 169 170   0 172 173   0 175 176 177   0   0 180
181 182 183   0   0 186   0 188 189 190 191 192   0 194 195
196 197 198   0 200 201 202   0   0 205   0 207   0   0   0
211 212 213   0   0   0 217 218   0 220 221   0   0 224   0
   congeal M
  0  94  94  94   0   6   0   8   0  12  12  12   0   0  15
  0   0  94  94  94   0  94   0   0   0   0   0  29  29   0
 32  32   0  94  94  94  94  94   0   0   0 116   0   0  45
  0   0  94  94   0  94   0   0 116 116   0 116 116   0   0
 94  94  94  94   0   0  82   0 116   0 116 116   0  74   0
  0   0  94  94   0   0  82   0 116 116 116 116 116   0   0
  0 108   0  94   0   0   0   0 116 116 116   0 116   0 105
108 108 108   0   0 141   0   0 116 116 116   0   0   0   0
  0   0   0 141 141 141 141   0   0   0   0   0 221 221 221
  0   0 213   0   0 141   0 221 221 221   0   0   0   0 221
  0 213 213 213   0   0   0 221   0 221   0 221 221 221 221
  0 213 213 213 213   0 221 221   0 221 221 221   0   0 221
213 213 213   0   0 218   0 221 221 221 221 221   0 221 221
213 213 213   0 218 218 218   0   0 221   0 221   0   0   0
213 213 213   0   0   0 218 218   0 221 221   0   0 224   0
   (~.@, i. ])congeal M
 0  1  1  1  0  2  0  3  0  4  4  4  0  0  5
 0  0  1  1  1  0  1  0  0  0  0  0  6  6  0
 7  7  0  1  1  1  1  1  0  0  0  8  0  0  9
 0  0  1  1  0  1  0  0  8  8  0  8  8  0  0
 1  1  1  1  0  0 10  0  8  0  8  8  0 11  0
 0  0  1  1  0  0 10  0  8  8  8  8  8  0  0
 0 12  0  1  0  0  0  0  8  8  8  0  8  0 13
12 12 12  0  0 14  0  0  8  8  8  0  0  0  0
 0  0  0 14 14 14 14  0  0  0  0  0 15 15 15
 0  0 16  0  0 14  0 15 15 15  0  0  0  0 15
 0 16 16 16  0  0  0 15  0 15  0 15 15 15 15
 0 16 16 16 16  0 15 15  0 15 15 15  0  0 15
16 16 16  0  0 17  0 15 15 15 15 15  0 15 15
16 16 16  0 17 17 17  0  0 15  0 15  0  0  0
16 16 16  0  0  0 17 17  0 15 15  0  0 18  0
```



## Julia

```julia
using Distributions

newgrid(p::Float64, r::Int, c::Int=r) = rand(Bernoulli(p), r, c)

function walkmaze!(grid::Matrix{Int}, r::Int, c::Int, indx::Int)
    NOT_CLUSTERED = 1 # const
    N, M = size(grid)
    dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]
    # fill cell
    grid[r, c] = indx
    # check for each direction
    for d in dirs
        rr, cc = (r, c) .+ d
        if checkbounds(Bool, grid, rr, cc) && grid[rr, cc] == NOT_CLUSTERED
            walkmaze!(grid, rr, cc, indx)
        end
    end
end

function clustercount!(grid::Matrix{Int})
    NOT_CLUSTERED = 1 # const
    walkind = 1
    for r in 1:size(grid, 1), c in 1:size(grid, 2)
        if grid[r, c] == NOT_CLUSTERED
            walkind += 1
            walkmaze!(grid, r, c, walkind)
        end
    end
    return walkind - 1
end
clusterdensity(p::Float64, n::Int) = clustercount!(newgrid(p, n)) / n ^ 2

function printgrid(G::Matrix{Int})
    LETTERS = vcat(' ', '#', 'A':'Z', 'a':'z')
    for r in 1:size(G, 1)
        println(r % 10, ") ", join(LETTERS[G[r, :] .+ 1], ' '))
    end
end

G = newgrid(0.5, 15)
@printf("Found %i clusters in this %i×%i grid\n\n", clustercount!(G), size(G, 1), size(G, 2))
printgrid(G)
println()

const nrange = 2 .^ (4:2:12)
const p = 0.5
const nrep = 5
for n in nrange
    sim = mean(clusterdensity(p, n) for _ in 1:nrep)
    @printf("nrep = %2i p = %.2f dim = %-13s sim = %.5f\n", nrep, p, "$n × $n", sim)
end
```


```txt
Found 20 clusters in this 15×15 grid

1) A       B   C C   D D D
2)       E   F         D   D
3)   G       F     D D D D D D
4) G G   H   F   I   D D   D   J
5)   G G   K   L       D       J
6) G G G G       M       N N
7) G G   G G G G   O O O   N
8)         G   G     O     N N N
9) P P P       G G G       N   N
0) P   P P P   G     Q Q Q     N
1) P             Q Q Q Q   N   N
2) P                     N N N N
3) P P P     R       S       N
4) P       R R R   S S     N N
5)     R R R       S   T     N

nrep =  5 p = 0.50 dim = 16 × 16       sim = 0.07500
nrep =  5 p = 0.50 dim = 64 × 64       sim = 0.07178
nrep =  5 p = 0.50 dim = 256 × 256     sim = 0.06690
nrep =  5 p = 0.50 dim = 1024 × 1024   sim = 0.06609
nrep =  5 p = 0.50 dim = 4096 × 4096   sim = 0.06588
```



## Kotlin

```scala
// version 1.2.10

import java.util.Random

val rand = Random()
const val RAND_MAX = 32767

lateinit var map: IntArray
var w = 0
var ww = 0

const val ALPHA = "+.ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
const val ALEN = ALPHA.length - 3

fun makeMap(p: Double) {
    val thresh = (p * RAND_MAX).toInt()
    ww = w * w
    var i = ww
    map = IntArray(i)
    while (i-- != 0) {
        val r = rand.nextInt(RAND_MAX + 1)
        if (r < thresh) map[i] = -1
    }
}

fun showCluster() {
    var k = 0
    for (i in 0 until w) {
        for (j in 0 until w) {
            val s = map[k++]
            val c = if (s < ALEN) ALPHA[1 + s] else '?'
            print(" $c")
        }
        println()
    }
}

fun recur(x: Int, v: Int) {
    if ((x in 0 until ww) && map[x] == -1) {
        map[x] = v
        recur(x - w, v)
        recur(x - 1, v)
        recur(x + 1, v)
        recur(x + w, v)
    }
}

fun countClusters(): Int {
    var cls = 0
    for (i in 0 until ww) {
        if (map[i] != -1) continue
        recur(i, ++cls)
    }
    return cls
}

fun tests(n: Int, p: Double): Double {
    var k = 0.0
    for (i in 0 until n) {
        makeMap(p)
        k += countClusters().toDouble() / ww
    }
    return k / n
}

fun main(args: Array<String>) {
    w = 15
    makeMap(0.5)
    val cls = countClusters()
    println("width = 15, p = 0.5, $cls clusters:")
    showCluster()

    println("\np = 0.5, iter = 5:")
    w = 1 shl 2
    while (w <= 1 shl 13) {
        val t = tests(5, 0.5)
        println("%5d %9.6f".format(w, t))
        w = w shl 1
    }
}
```


Sample output:

```txt

width = 15, p = 0.5, 23 clusters:
 A . B . C C . . . . D D . D .
 . B B B . . D D D . D D . D D
 . B . B B B . . D D D . . D .
 . . E . . B . B . . D D D D .
 F . . B B B B B . G . D . D D
 . . B B . B . . H . D D . D D
 . B B . . . I . H H . . D D .
 B B B . . J . K . . L . D . B
 B . . M . . K K K . . D D . B
 B B . . . . . K K . . D . B B
 B . . N N . . . . . . . O . .
 B . . . . P . . O O O O O . .
 . . Q . . P . . . O O O O . .
 . . . R . . . . S . O . . T T
 . U . . . V . . . . . W . . .

p = 0.5, iter = 5:
    4  0.112500
    8  0.121875
   16  0.075000
   32  0.068750
   64  0.068164
  128  0.065625
  256  0.067093
  512  0.065815
 1024  0.065863
 2048  0.065815
 4096  0.065764
 8192  0.065766

```



## Perl

```perl
$fill = 'x';
$D{$_} = $i++ for qw<DeadEnd Up Right Down Left>;

sub deq { defined $_[0] && $_[0] eq $_[1] }

sub perctest {
    my($grid) = @_;
    generate($grid);
    my $block = 1;
    for my $y (0..$grid-1) {
        for my $x (0..$grid-1) {
            fill($x, $y, $block++) if $perc[$y][$x] eq $fill
        }
    }
    ($block - 1) / $grid**2;
}

sub generate {
    my($grid) = @_;
    for my $y (0..$grid-1) {
        for my $x (0..$grid-1) {
            $perc[$y][$x] = rand() < .5 ? '.' : $fill;
        }
    }
}

sub fill {
    my($x, $y, $block) = @_;
    $perc[$y][$x] = $block;
    my @stack;
    while (1) {
        if (my $dir = direction( $x, $y )) {
            push @stack, [$x, $y];
            ($x,$y) = move($dir, $x, $y, $block)
        } else {
            return unless @stack;
            ($x,$y) = @{pop @stack};
        }
    }
}

sub direction {
    my($x, $y) = @_;
    return $D{Down}  if deq($perc[$y+1][$x  ], $fill);
    return $D{Left}  if deq($perc[$y  ][$x-1], $fill);
    return $D{Right} if deq($perc[$y  ][$x+1], $fill);
    return $D{Up}    if deq($perc[$y-1][$x  ], $fill);
    return $D{DeadEnd};
}

sub move {
    my($dir,$x,$y,$block) = @_;
    $perc[--$y][   $x] = $block if $dir == $D{Up};
    $perc[++$y][   $x] = $block if $dir == $D{Down};
    $perc[  $y][ --$x] = $block if $dir == $D{Left};
    $perc[  $y][ ++$x] = $block if $dir == $D{Right};
    ($x, $y)
}

my $K = perctest(15);
for my $row (@perc) {
    printf "%3s", $_ for @$row;
    print "\n";
}
printf  "𝘱 = 0.5, 𝘕 = 15, 𝘒 = %.4f\n\n", $K;

$trials = 5;
for $N (10, 30, 100, 300, 1000) {
    my $total = 0;
    $total += perctest($N) for 1..$trials;
    printf "𝘱 = 0.5, trials = $trials, 𝘕 = %4d, 𝘒 = %.4f\n", $N, $total / $trials;
}
```

```txt
  1  1  1  .  .  .  .  2  2  2  .  .  .  .  .
  .  1  .  1  1  1  .  2  2  .  2  2  2  .  3
  .  1  .  .  1  .  2  2  2  2  2  2  .  .  3
  1  1  1  .  1  .  2  2  .  .  .  .  4  4  .
  1  1  1  .  1  .  .  2  .  .  .  .  .  .  1
  1  1  1  1  1  .  .  2  .  .  5  .  6  .  .
  1  1  .  .  1  1  .  2  .  7  .  .  .  1  1
  1  .  .  .  1  1  .  2  2  .  .  8  8  .  1
  .  9  9  9  .  1  .  .  2  2  .  .  .  1  1
  .  .  9  9  .  . 10  .  .  . 11  . 12  .  .
  .  9  9  . 13 13  . 13  . 14  .  . 12  .  .
 15  .  . 13 13 13 13 13  .  .  . 16  . 17  .
 15  .  . 13  . 13  . 13 13  .  . 16 16  .  .
  . 18  .  . 13 13 13 13  .  .  .  .  . 19 19
  1  .  1  .  . 13  .  .  .  . 20  . 19 19  .
𝘱 = 0.5, 𝘕 = 15, 𝘒 = 0.0889

𝘱 = 0.5, trials = 5, 𝘕 =   10, 𝘒 = 0.0980
𝘱 = 0.5, trials = 5, 𝘕 =   30, 𝘒 = 0.0738
𝘱 = 0.5, trials = 5, 𝘕 =  100, 𝘒 = 0.0670
𝘱 = 0.5, trials = 5, 𝘕 =  300, 𝘒 = 0.0660
𝘱 = 0.5, trials = 5, 𝘕 = 1000, 𝘒 = 0.0661
```



## Perl 6

```perl6
my @perc;
my $fill = 'x';

enum Direction <DeadEnd Up Right Down Left>;

my $𝘒 = perctest(15);
.fmt("%-2s").say for @perc;
say "𝘱 = 0.5, 𝘕 = 15, 𝘒 = $𝘒\n";

my $trials = 5;
for 10, 30, 100, 300, 1000 -> $𝘕 {
    my $𝘒 = ( [+] perctest($𝘕) xx $trials ) / $trials;
    say "𝘱 = 0.5, trials = $trials, 𝘕 = $𝘕, 𝘒 = $𝘒";
}

sub infix:<deq> ( $a, $b ) { $a.defined && ($a eq $b) }

sub perctest ( $grid ) {
    generate $grid;
    my $block = 1;
    for ^$grid X ^$grid -> ($y, $x) {
        fill( [$x, $y], $block++ ) if @perc[$y; $x] eq $fill
    }
    ($block - 1) / $grid²;
}

sub generate ( $grid ) {
    @perc = ();
    @perc.push: [ ( rand < .5 ?? '.' !! $fill ) xx $grid ] for ^$grid;
}

sub fill ( @cur, $block ) {
    @perc[@cur[1]; @cur[0]] = $block;
    my @stack;
    my $current = @cur;

    loop {
        if my $dir = direction( $current ) {
            @stack.push: $current;
            $current = move $dir, $current, $block
        }
        else {
            return unless @stack;
            $current = @stack.pop
        }
    }

    sub direction( [$x, $y] ) {
        ( Down  if @perc[$y + 1][$x] deq $fill ) ||
        ( Left  if @perc[$y][$x - 1] deq $fill ) ||
        ( Right if @perc[$y][$x + 1] deq $fill ) ||
        ( Up    if @perc[$y - 1][$x] deq $fill ) ||
        DeadEnd
    }

    sub move ( $dir, @cur, $block ) {
        my ( $x, $y ) = @cur;
        given $dir {
            when Up    { @perc[--$y; $x] = $block }
            when Down  { @perc[++$y; $x] = $block }
            when Left  { @perc[$y; --$x] = $block }
            when Right { @perc[$y; ++$x] = $block }
        }
        [$x, $y]
    }
}

```

```txt
.  .  1  .  2  .  .  3  .  .  .  4  .  .  .
2  2  .  .  2  2  2  .  5  5  .  4  4  4  4
2  2  2  2  2  .  2  .  5  .  .  4  .  .  4
2  .  2  .  2  2  .  .  .  .  4  4  4  .  4
.  .  .  .  .  2  .  .  .  .  4  4  4  .  .
6  6  6  6  .  .  7  7  .  .  4  .  4  4  .
6  .  6  .  .  .  .  .  4  4  4  4  4  .  .
6  6  6  .  .  .  8  8  .  4  4  4  .  .  4
6  .  .  .  9  .  .  .  .  .  .  4  4  .  4
.  10 .  11 .  .  12 12 .  4  .  .  4  4  4
11 .  11 11 11 11 .  12 .  4  .  4  4  4  .
11 11 11 11 11 11 11 .  .  4  4  .  .  4  .
11 11 11 11 .  11 .  .  .  4  4  4  4  4  .
11 11 11 .  11 11 11 .  .  4  4  4  4  .  13
.  11 11 .  11 11 .  .  .  .  4  4  .  14 .
𝘱 = 0.5, 𝘕 = 15, 𝘒 = 0.062222

𝘱 = 0.5, trials = 5, 𝘕 = 10, 𝘒 = 0.114
𝘱 = 0.5, trials = 5, 𝘕 = 30, 𝘒 = 0.082444
𝘱 = 0.5, trials = 5, 𝘕 = 100, 𝘒 = 0.06862
𝘱 = 0.5, trials = 5, 𝘕 = 300, 𝘒 = 0.066889
𝘱 = 0.5, trials = 5, 𝘕 = 1000, 𝘒 = 0.0659358

```



## Phix

```Phix
sequence grid
integer w, ww

procedure make_grid(atom p)
    ww = w*w
    grid = repeat(0,ww)
    for i=1 to ww do
        grid[i] = -(rnd()<p)
    end for
end procedure

constant alpha = "+.ABCDEFGHIJKLMNOPQRSTUVWXYZ"&
                   "abcdefghijklmnopqrstuvwxyz"

procedure show_cluster()
    for i=1 to ww do
        integer gi = grid[i]+2
        grid[i] = iff(gi<=length(alpha)?alpha[gi]:'?')
    end for
    puts(1,join_by(grid,w,w,""))
end procedure

procedure recur(integer x, v)
    if x>=1 and x<=ww and grid[x]==-1 then
        grid[x] = v
        recur(x-w, v)
        recur(x-1, v)
        recur(x+1, v)
        recur(x+w, v)
    end if
end procedure

function count_clusters()
    integer cls = 0
    for i=1 to ww do
        if grid[i]=-1 then
            cls += 1
            recur(i, cls)
        end if
    end for
    return cls
end function

function tests(int n, atom p)
    atom k = 0
    for i=1 to n do
        make_grid(p)
        k += count_clusters()/ww
    end for
    return k / n
end function

procedure main()
    w = 15
    make_grid(0.5)
    printf(1,"width=15, p=0.5, %d clusters:\n", count_clusters())
    show_cluster()

    printf(1,"\np=0.5, iter=5:\n")
    w = 4
    while w<=4096 do
        printf(1,"%5d %9.6f\n", {w, tests(5,0.5)})
        w *= 4
    end while
end procedure
main()
```

```txt

width=15, p=0.5, 18 clusters:
..EE.FF...OO..P
.......KK.OOO.P
A..B..I....OO.P
.BBB.G.LL....PP
BBB...J....J.P.
..BB..JJJJJJ.P.
.BBBB.JJJ.J.J..
BBB....JJJ.JJ.Q
BB.D.D.J.JJJ.QQ
..DDDD.JJ.JJJ.Q
.DDDD.JJ..JJ...
C.DDD.J.N.JJ...
C.D...J..JJ....
.....H...J.....
.......M..O...R

p=0.5, iter=5:
    4  0.137500
   16  0.080469
   64  0.068164
  256  0.066809
 1024  0.066018
 4096  0.065777

```



## Python


```python
from __future__ import division
from random import random
import string
from math import fsum

n_range, p, t = (2**n2 for n2 in range(4, 14, 2)), 0.5, 5
N = M = 15

NOT_CLUSTERED = 1   # filled but not clustered cell
cell2char = ' #' + string.ascii_letters

def newgrid(n, p):
    return [[int(random() < p) for x in range(n)] for y in range(n)]

def pgrid(cell):
    for n in range(N):
        print( '%i)  ' % (n % 10)
               + ' '.join(cell2char[cell[n][m]] for m in range(M)))


def cluster_density(n, p):
    cc = clustercount(newgrid(n, p))
    return cc / n / n

def clustercount(cell):
    walk_index = 1
    for n in range(N):
        for m in range(M):
            if cell[n][m] == NOT_CLUSTERED:
                walk_index += 1
                walk_maze(m, n, cell, walk_index)
    return walk_index - 1


def walk_maze(m, n, cell, indx):
    # fill cell
    cell[n][m] = indx
    # down
    if n < N - 1 and cell[n+1][m] == NOT_CLUSTERED:
        walk_maze(m, n+1, cell, indx)
    # right
    if m < M - 1 and cell[n][m + 1] == NOT_CLUSTERED:
        walk_maze(m+1, n, cell, indx)
    # left
    if m and cell[n][m - 1] == NOT_CLUSTERED:
        walk_maze(m-1, n, cell, indx)
    # up
    if n and cell[n-1][m] == NOT_CLUSTERED:
        walk_maze(m, n-1, cell, indx)


if __name__ == '__main__':
    cell = newgrid(n=N, p=0.5)
    print('Found %i clusters in this %i by %i grid\n'
          % (clustercount(cell), N, N))
    pgrid(cell)
    print('')

    for n in n_range:
        N = M = n
        sim = fsum(cluster_density(n, p) for i in range(t)) / t
        print('t=%3i p=%4.2f n=%5i sim=%7.5f'
              % (t, p, n, sim))
```


```txt
Found 20 clusters in this 15 by 15 grid

0)  a a     b     c       d d d d
1)  a a   e     f   g g   d
2)        e   f f f       d
3)  h h   e     f   i i   d d
4)        e       j     d d d d
5)    k k   k   k   l         d d
6)  k k k k k   k   l   m   n
7)  k k k   k   k   l     o   p p
8)      k   k k k   l l l   q
9)  k     k k k k     l     q q q
0)  k   k k k         l     q q q
1)  k k     k k k       r r
2)  k     k k       r r r   s s
3)  k k k k   r r r r r     s s
4)  k   k   t   r   r r     s s

t=  5 p=0.50 n=   16 sim=0.08984
t=  5 p=0.50 n=   64 sim=0.07310
t=  5 p=0.50 n=  256 sim=0.06706
t=  5 p=0.50 n= 1024 sim=0.06612
t=  5 p=0.50 n= 4096 sim=0.06587
```

As n increases, the sim result gets closer to 0.065770...


## Racket


```racket
#lang racket
(require srfi/14) ; character sets

; much faster than safe fixnum functions
(require
  racket/require ; for fancy require clause below
  (filtered-in
          (lambda (name) (regexp-replace #rx"unsafe-" name ""))
          racket/unsafe/ops)
  ; these aren't in racket/unsafe/ops
  (only-in racket/fixnum for/fxvector in-fxvector fxvector-copy))

; ...(but less safe). if in doubt use this rather than the one above
; (require racket/fixnum)

(define t (make-parameter 5))

(define (build-random-grid p M N)
  (define p-num (numerator p))
  (define p-den (denominator p))
  (for/fxvector #:length (fx* M N) ((_ (in-range (* M N))))
                (if (< (random p-den) p-num) 1 0)))

(define letters
  (sort (char-set->list (char-set-intersection
                         char-set:letter
                         ; char-set:ascii
                         )) char<?))
(define n-letters (length letters))
(define cell->char
  (match-lambda
    (0 #\space) (1 #\.)
    (c (list-ref letters (modulo (- c 2) n-letters)))))

(define (draw-percol-grid M N . gs)
  (for ((r N))
    (for ((g gs))
      (define row-str
        (list->string
         (for/list ((idx (in-range (* r M) (* (+ r 1) M))))
           (cell->char (fxvector-ref g idx)))))
      (printf "|~a| " row-str))
    (newline)))

(define (count-clusters! M N g)
  (define (gather-cluster! k c)
    (when (fx= 1 (fxvector-ref g k))
      (define k-r (fxquotient k M))
      (define k-c (fxremainder k M))
      (fxvector-set! g k c)
      (define-syntax-rule (gather-surrounds range? k+)
        (let ((idx k+))
          (when (and range? (fx= 1 (fxvector-ref g idx)))
            (gather-cluster! idx c))))
      (gather-surrounds (fx> k-r 0) (fx- k M))
      (gather-surrounds (fx> k-c 0) (fx- k 1))
      (gather-surrounds (fx< k-c (fx- M 1)) (fx+ k 1))
      (gather-surrounds (fx< k-r (fx- N 1)) (fx+ k M))))

  (define-values (rv _c)
    (for/fold ((rv 0) (c 2))
      ((pos (in-range (fx* M N)))
       #:when (fx= 1 (fxvector-ref g pos)))
      (gather-cluster! pos c)
      (values (fx+ rv 1) (fx+ c 1))))
  rv)

(define (display-sample-clustering p)
  (printf "Percolation cluster sample: p=~a~%" p)
  (define g (build-random-grid p 15 15))
  (define g+ (fxvector-copy g))
  (define g-count (count-clusters! 15 15 g+))
  (draw-percol-grid 15 15 g g+)
  (printf "~a clusters~%" g-count))

(define (experiment p n t)
  (printf "Experiment: ~a ~a ~a\t" p n t) (flush-output)
  (define sum-Cn
    (for/sum ((run (in-range t)))
      (printf "[~a" run) (flush-output)
      (define g (build-random-grid p n n))
      (printf "*") (flush-output)
      (define Cn (count-clusters! n n g))
      (printf "]") (flush-output)
      Cn))
  (printf "\tmean K(p) = ~a~%" (real->decimal-string (/ sum-Cn t (sqr n)) 6)))

(module+ main
  (t 10)
  (for ((n (in-list '(4000 1000 750 500 400 300 200 100 15))))
    (experiment 1/2 n (t)))
  (display-sample-clustering 1/2))

(module+ test
  (define grd (build-random-grid 1/2 1000 1000))
  (/ (for/sum ((g (in-fxvector grd)) #:when (zero? g)) 1) (fxvector-length grd))
  (display-sample-clustering 1/2))
```


Run from DrRacket, which runs the test and main modules. From the command line, you'll
want two commands: ``racket percolation_m_c_d.rkt`` and ``raco test percolation_m_c_d.rkt``
for the same result.

```txt

Experiment: 1/2 4000 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.065860
Experiment: 1/2 1000 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.066130
Experiment: 1/2 750 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.066195
Experiment: 1/2 500 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.066522
Experiment: 1/2 400 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.066778
Experiment: 1/2 300 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.066813
Experiment: 1/2 200 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.067908
Experiment: 1/2 100 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.069980
Experiment: 1/2 15 10	[0*][1*][2*][3*][4*][5*][6*][7*][8*][9*]	mean K(p) = 0.089778
Percolation cluster sample: p=1/2
|.  ...     . . | |A  BBB     A A |
|...    .. .... | |AAA    AA AAAA |
|. .   .... ... | |A A   AAAA AAA |
|. . . .........| |A A C AAAAAAAAA|
| ...   ..  ....| | AAA   AA  AAAA|
|.. ......... ..| |AA AAAAAAAAA AA|
|     . ...     | |     A AAA     |
|. ..  ..       | |D AA  AA       |
|   .. ... . .. | |   AA AAA E AA |
|.  ..  ..  . . | |F  AA  AA  A A |
|. ........ . ..| |F AAAAAAAA A AA|
|.. .  .... ... | |FF A  AAAA AAA |
| .  .  . ....  | | F  G  A AAAA  |
|.... .. ..  . .| |FFFF HH AA  A A|
|  .  ..   .....| |  F  HH   AAAAA|
8 clusters
```



## Tcl

Note that the queue (variables <code>q</code> and <code>k</code>) used to remember where to find cells when flood-filling the cluster is maintained as a list ''segment''; the front of the list is not trimmed for performance reasons. (This would matter with very long queues, in which case the queue could be shortened occasionally; ''frequent'' trimming is still slower though, because Tcl backs its “list” datatype with arrays and not linked lists.)
```tcl
package require Tcl 8.6

proc determineClusters {w h p} {
    # Construct the grid
    set grid [lrepeat $h [lrepeat $w 0]]
    for {set i 0} {$i < $h} {incr i} {
	for {set j 0} {$j < $w} {incr j} {
	    lset grid $i $j [expr {rand() < $p ? -1 : 0}]
	}
    }
    # Find (and count) the clusters
    set cl 0
    for {set i 0} {$i < $h} {incr i} {
	for {set j 0} {$j < $w} {incr j} {
	    if {[lindex $grid $i $j] == -1} {
		incr cl
		for {set q [list $i $j];set k 0} {$k<[llength $q]} {incr k} {
		    set y [lindex $q $k]
		    set x [lindex $q [incr k]]
		    if {[lindex $grid $y $x] != -1} continue
		    lset grid $y $x $cl
		    foreach dx {1 0 -1 0} dy {0 1 0 -1} {
			set nx [expr {$x+$dx}]
			set ny [expr {$y+$dy}]
			if {
			    $nx >= 0 && $ny >= 0 && $nx < $w && $ny < $h &&
			    [lindex $grid $ny $nx] == -1
			} then {
			    lappend q $ny $nx
			}
		    }
		}
	    }
	}
    }
    return [list $cl $grid]
}

# Print a sample 15x15 grid
lassign [determineClusters 15 15 0.5] n g
puts "15x15 grid, p=0.5, with $n clusters"
puts "+[string repeat - 15]+"
foreach r $g {puts |[join [lmap x $r {format %c [expr {$x==0?32:64+$x}]}] ""]|}
puts "+[string repeat - 15]+"

# Determine the densities as the grid size increases
puts "p=0.5, iter=5"
foreach n {5 30 180 1080 6480} {
    set tot 0
    for {set i 0} {$i < 5} {incr i} {
	lassign [determineClusters $n $n 0.5] nC
	incr tot $nC
    }
    puts "n=$n, K(p)=[expr {$tot/5.0/$n**2}]"
}
```

```txt

15x15 grid, p=0.5, with 21 clusters
+---------------+
|    A   B CCCCC|
|  D A BBB  C   |
|E     B  F CCCC|
| B    B  F CC C|
|BBB B BB    CCC|
|B BBBBBB  CCCCC|
|  B   B G  C  C|
|H  II   G G  J |
|HH II   G GG  K|
|HH II GGG  GG K|
|   I  G GGGG   |
|LL  GGG  GG M N|
| L  G G O    P |
|LLLL Q     R   |
|L  L   S T  UUU|
+---------------+
p=0.5, iter=5
n=5, K(p)=0.184
n=30, K(p)=0.07155555555555557
n=180, K(p)=0.06880246913580246
n=1080, K(p)=0.0661267146776406
n=6480, K(p)=0.06582889898643499

```



## zkl

```zkl
const X=-1;	// the sentinal that marks an untouched cell
var C,N,NN,P;
fcn createC(n,p){
   N,P=n,p; NN=N*N;
   C=NN.pump(List.createLong(NN),0);  // vector of ints
   foreach n in (NN){ C[n]=X*(Float.random(1)<=P) } // X is the sentinal
}
fcn showCluster{
   alpha:="-ABCDEFGHIJKLMNOPQRSTUVWXYZ" "abcdefghijklmnopqrstuvwxyz";
   foreach n in ([0..NN,N]){ C[n,N].pump(String,alpha.get).println() }
}
fcn countClusters{
   clusters:=0;
   foreach n in (NN){
      if(X!=C[n]) continue;
      fcn(n,v){
	 if((0<=n<NN) and C[n]==X){
	    C[n]=v;
	    self.fcn(n-N,v); self.fcn(n-1,v); self.fcn(n+1,v); self.fcn(n+N,v);
	 }
      }(n,clusters+=1);
   }
   clusters
}
fcn tests(N,n,p){
   k:=0.0;
   foreach z in (n){ createC(N,p); k+=countClusters().toFloat()/NN; }
   k/n
}
```


```zkl
createC(15,0.5);
println("width=%d, p=%.1f, %d clusters:".fmt(N,P,countClusters()));
showCluster();

println("p=0.5, 5 iterations:");
w:=4; do(6){ println("%5d %9.6f".fmt(w,tests(w, 5, 0.5))); w*=4; }
```

```txt

width=15, p=0.5, 16 clusters:
-AAA-BB-BBB---C
------BBBB--D--
E---F---BB--DD-
EE----G-BB---DD
--H-I--J--J--DD
-K--I--JJ-J--D-
-K--I--JJJJ-L--
KK-III-------MM
-K-I--I--NN-I--
I-IIIII-NNN-III
I-II--I-N-N-II-
III-III--NNN-II
I-II-II-O---I--
I-I-IIII-PP-III
I-II--I---P--II

p=0.5, 5 iterations:
    4  0.062500
   16  0.070312
   64  0.067627
  256  0.067078
 1024  0.065834
 4096  0.065771

```

