+++
title = "Percolation/Site percolation"
description = ""
date = 2019-06-08T20:23:37Z
aliases = []
[extra]
id = 15836
[taxonomies]
categories = ["task", "Percolation Simulations"]
tags = []
languages = [
  "c",
  "d",
  "factor",
  "fortran",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "sidef",
  "tcl",
  "vvvvvv",
  "zkl",
]
+++

## Task

Given an <math>M \times N</math> rectangular array of cells numbered <math>\mathrm{cell}[0..M-1, 0..N-1]</math>assume <math>M</math> is horizontal and <math>N</math> is downwards.

Assume that the probability of any cell being filled is a constant <math>p</math> where
: <math>0.0 \le p \le 1.0</math>

;The task:
Simulate creating the array of cells with probability <math>p</math> and then
testing if there is a route through adjacent filled cells from any on row <math>0</math> to any on row <math>N</math>, i.e. testing for site percolation.

Given <math>p</math> repeat the percolation <math>t</math> times to estimate the proportion of times that the fluid can percolate to the bottom for any given <math>p</math>.

Show how the probability of percolating through the random grid changes with <math>p</math> going from <math>0.0</math> to <math>1.0</math> in <math>0.1</math> increments and with the number of repetitions to estimate the fraction at any given <math>p</math> as <math>t >= 100</math>.

Use an <math>M=15, N=15</math> grid of cells for all cases.

Optionally depict a percolation through a cell grid graphically.

Show all output on this page.


## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *cell, *start, *end;
int m, n;

void make_grid(int x, int y, double p)
{
	int i, j, thresh = p * RAND_MAX;

	m = x, n = y;
	end = start = realloc(start, (x+1) * (y+1) + 1);

	memset(start, 0, m + 1);

	cell = end = start + m + 1;
	for (i = 0; i < n; i++) {
		for (j = 0; j < m; j++)
			*end++ = rand() < thresh ? '+' : '.';
		*end++ = '\n';
	}

	end[-1] = 0;
	end -= ++m; // end is the first cell of bottom row
}

int ff(char *p) // flood fill
{
	if (*p != '+') return 0;

	*p = '#';
	return p >= end || ff(p+m) || ff(p+1) || ff(p-1) || ff(p-m);
}

int percolate(void)
{
	int i;
	for (i = 0; i < m && !ff(cell + i); i++);
	return i < m;
}

int main(void)
{
	make_grid(15, 15, .5);
	percolate();

	puts("15x15 grid:");
	puts(cell);

	puts("\nrunning 10,000 tests for each case:");

	double p;
	int ip, i, cnt;
	for (ip = 0; ip <= 10; ip++) {
		p = ip / 10.;
		for (cnt = i = 0; i < 10000; i++) {
			make_grid(15, 15, p);
			cnt += percolate();
		}
		printf("p=%.1f:  %.4f\n", p, cnt / 10000.);
	}

	return 0;
}
```

```txt

15x15 grid:
.#...##.#.#.#..
...+.###.####.#
...+..#.+...#.#
+..+..##..#####
+...+.#....##..
.+..+.##..##.+.
....+.#...##..+
..+.+.#####.++.
+++....#.###.++
.+.+.#.#.##....
..++.####...++.
+.+.+.##..+++..
+..+.+..+.....+
..........++..+
.+.+.++++.+...+

running 10,000 tests for each case:
p=0.0:  0.0000
p=0.1:  0.0000
p=0.2:  0.0000
p=0.3:  0.0000
p=0.4:  0.0032
p=0.5:  0.0902
p=0.6:  0.5771
p=0.7:  0.9587
p=0.8:  0.9996
p=0.9:  1.0000
p=1.0:  1.0000

```

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>

#define N_COLS 15
#define N_ROWS 15

// Probability granularity 0.0, 0.1, ... 1.0
#define N_STEPS 11

// Simulation tries
#define N_TRIES 100

typedef unsigned char Cell;
enum { EMPTY_CELL   = ' ',
       FILLED_CELL  = '#',
       VISITED_CELL = '.' };
typedef Cell Grid[N_ROWS][N_COLS];

void initialize(Grid grid, const double probability) {
    for (size_t r = 0; r < N_ROWS; r++)
        for (size_t c = 0; c < N_COLS; c++) {
            const double rnd = rand() / (double)RAND_MAX;
            grid[r][c] = (rnd < probability) ? EMPTY_CELL : FILLED_CELL;
        }
}

void show(Grid grid) {
    char line[N_COLS + 3];
    memset(&line[0], '-', N_COLS + 2);
    line[0] = '+';
    line[N_COLS + 1] = '+';
    line[N_COLS + 2] = '\0';

    printf("%s\n", line);
    for (size_t r = 0; r < N_ROWS; r++) {
        putchar('|');
        for (size_t c = 0; c < N_COLS; c++)
            putchar(grid[r][c]);
        puts("|");
    }
    printf("%s\n", line);
}

bool walk(Grid grid, const size_t r, const size_t c) {
    const size_t bottom = N_ROWS - 1;
    grid[r][c] = VISITED_CELL;

    if (r < bottom && grid[r + 1][c] == EMPTY_CELL) { // Down.
        if (walk(grid, r + 1, c))
            return true;
    } else if (r == bottom)
        return true;

    if (c && grid[r][c - 1] == EMPTY_CELL) // Left.
        if (walk(grid, r, c - 1))
            return true;

    if (c < N_COLS - 1 && grid[r][c + 1] == EMPTY_CELL) // Right.
        if (walk(grid, r, c + 1))
            return true;

    if (r && grid[r - 1][c] == EMPTY_CELL) // Up.
        if (walk(grid, r - 1, c))
            return true;

    return false;
}

bool percolate(Grid grid) {
    const size_t startR = 0;
    for (size_t c = 0; c < N_COLS; c++)
        if (grid[startR][c] == EMPTY_CELL)
            if (walk(grid, startR, c))
                return true;
    return false;
}

typedef struct {
    double prob;
    size_t count;
} Counter;

int main() {
    const double probability_step = 1.0 / (N_STEPS - 1);
    Counter counters[N_STEPS];
    for (size_t i = 0; i < N_STEPS; i++)
        counters[i] = (Counter){ i * probability_step, 0 };

    bool sample_shown = false;
    static Grid grid;
    srand(time(NULL));

    for (size_t i = 0; i < N_STEPS; i++) {
        for (size_t t = 0; t < N_TRIES; t++) {
            initialize(grid, counters[i].prob);
            if (percolate(grid)) {
                counters[i].count++;
                if (!sample_shown) {
                    printf("Percolating sample (%dx%d,"
                           " probability =%5.2f):\n",
                           N_COLS, N_ROWS, counters[i].prob);
                    show(grid);
                    sample_shown = true;
                }
            }
        }
    }

    printf("\nFraction of %d tries that percolate through:\n", N_TRIES);
    for (size_t i = 0; i < N_STEPS; i++)
        printf("%1.1f %1.3f\n", counters[i].prob,
               counters[i].count / (double)N_TRIES);

    return 0;
}

```

```txt
Percolating sample (15x15, probability = 0.40):
+---------------+
|###.  #  # #  #|
|###.. #  ##### |
|   #. ######  #|
|###....  ######|
|######.  ### # |
| #####.######  |
|#......... ##  |
|...#...##.# ## |
|##.#...##.### #|
| ###..# #. #   |
|# #######. # ##|
|   # ##...#### |
| ##  # .#####  |
|#######.##  ###|
|# ##   .## # # |
+---------------+

Fraction of 100 tries that percolate through:
0.0 0.000
0.1 0.000
0.2 0.000
0.3 0.000
0.4 0.010
0.5 0.070
0.6 0.630
0.7 0.970
0.8 1.000
0.9 1.000
1.0 1.000
```



## D

```d
import std.stdio, std.random, std.array, std.datetime;

enum size_t nCols = 15,
            nRows = 15,
            nSteps = 11,     // Probability granularity.
            nTries = 20_000; // Simulation tries.

enum Cell : char { empty   = ' ', filled  = '#', visited = '.' }
alias Grid = Cell[nCols][nRows];

void initialize(ref Grid grid, in double probability, ref Xorshift rng) {
    foreach (ref row; grid)
        foreach (ref cell; row)
            cell = (rng.uniform01 < probability) ? Cell.empty : Cell.filled;
}

void show(in ref Grid grid) @safe {
    writefln("%(|%(%c%)|\n%)|", grid);
}

bool percolate(ref Grid grid) pure nothrow @safe @nogc {
    bool walk(in size_t r, in size_t c) nothrow @safe @nogc {
        enum bottom = nRows - 1;
        grid[r][c] = Cell.visited;

        if (r < bottom && grid[r + 1][c] == Cell.empty) { // Down.
            if (walk(r + 1, c))
                return true;
        } else if (r == bottom)
            return true;

        if (c && grid[r][c - 1] == Cell.empty) // Left.
            if (walk(r, c - 1))
                return true;

        if (c < nCols - 1 && grid[r][c + 1] == Cell.empty) // Right.
            if (walk(r, c + 1))
                return true;

        if (r && grid[r - 1][c] == Cell.empty) // Up.
            if (walk(r - 1, c))
                return true;

        return false;
    }

    enum startR = 0;
    foreach (immutable c; 0 .. nCols)
        if (grid[startR][c] == Cell.empty)
            if (walk(startR, c))
                return true;
    return false;
}

void main() {
    static struct Counter {
        double prob;
        size_t count;
    }

    StopWatch sw;
    sw.start;

    enum probabilityStep = 1.0 / (nSteps - 1);
    Counter[nSteps] counters;
    foreach (immutable i, ref co; counters)
        co.prob = i * probabilityStep;

    Grid grid;
    bool sampleShown = false;
    auto rng = Xorshift(unpredictableSeed);

    foreach (ref co; counters) {
        foreach (immutable _; 0 .. nTries) {
            grid.initialize(co.prob, rng);
            if (grid.percolate) {
                co.count++;
                if (!sampleShown) {
                    writefln("Percolating sample (%dx%d, probability =%5.2f):",
                             nCols, nRows, co.prob);
                    grid.show;
                    sampleShown = true;
                }
            }
        }
    }
    sw.stop;

    writefln("\nFraction of %d tries that percolate through:", nTries);
    foreach (const co; counters)
        writefln("%1.3f %1.3f", co.prob, co.count / double(nTries));

    writefln("\nSimulations and grid printing performed" ~
             " in %3.2f seconds.", sw.peek.msecs / 1000.0);
}
```

```txt
Percolating sample (15x15, probability = 0.40):
|#.###.##..#. # |
|#.###.# ###.  #|
|#.##..#####. ##|
|## ####  ...# #|
|# # #  ##.#..##|
|### # ## .#####|
|   ######.## ##|
|    ## #..###  |
|#### ##..##### |
|#   ###...  #  |
|### ## ##.   # |
|# ###  ##. ### |
|## ##### . ####|
|# ## #  #. ####|
|####### #.## ##|

Fraction of 20000 tries that percolate through:
0.000 0.000
0.100 0.000
0.200 0.000
0.300 0.000
0.400 0.004
0.500 0.090
0.600 0.565
0.700 0.958
0.800 1.000
0.900 1.000
1.000 1.000

Simulations and grid printing performed in 0.70 seconds.
```



## Factor


```factor
USING: arrays combinators combinators.short-circuit formatting
fry generalizations io kernel math math.matrices math.order
math.ranges math.vectors prettyprint random sequences ;
IN: rosetta-code.site-percolation

SYMBOLS: ▓ . v ;

: randomly-filled-matrix ( m n probability -- matrix )
    [ random-unit > ▓ . ? ] curry make-matrix ;

: in-bounds? ( matrix loc -- ? )
    [ dim { 1 1 } v- ] dip [ 0 rot between? ] 2map [ t = ] all? ;

: set-coord ( obj loc matrix -- ) [ reverse ] dip set-index ;
: get-coord ( matrix loc -- elt ) swap [ first2 ] dip nth nth ;

: (can-percolate?) ( matrix loc -- ? )
    {
        { [ 2dup in-bounds? not ] [ 2drop f ] }
        { [ 2dup get-coord { v ▓ } member? ] [ 2drop f ] }
        {
            [ 2dup second [ dim second 1 - ] dip = ]
            [ [ v ] 2dip swap set-coord t ]
        }
        [
            2dup get-coord . =
            [ [ v ] 2dip swap [ set-coord ] 2keep swap ] when
            {
                [ { 1 0 } v+ ] [ { 1 0 } v- ]
                [ { 0 1 } v+ ] [ { 0 1 } v- ]
            } [ (can-percolate?) ] map-compose 2||
        ]
    } cond ;

: can-percolate? ( matrix -- ? )
    dup dim first <iota> [ 0 2array (can-percolate?) ] with find
    drop >boolean ;

: show-sample ( -- )
    f [ [ can-percolate? ] keep swap ]
    [ drop 15 15 0.6 randomly-filled-matrix ] do until
    "Sample percolation, p = 0.6" print simple-table. ;

: percolation-rate ( p -- rate )
    [ 500 1 ] dip -
    '[ 15 15 _ randomly-filled-matrix can-percolate? ] replicate
    [ t = ] count 500 / ;

: site-percolation ( -- )
    show-sample nl "Running 500 trials at each porosity:" print
    10 [1,b] [
        10 / dup percolation-rate "p = %.1f: %.3f\n" printf
    ] each ;

MAIN: site-percolation
```

```txt

Sample percolation, p = 0.6
▓ ▓ v v ▓ . ▓ ▓ ▓ ▓ . ▓ ▓ . ▓
▓ ▓ v v v ▓ ▓ ▓ ▓ ▓ . . . ▓ ▓
▓ . v v ▓ . ▓ ▓ ▓ . ▓ . . ▓ ▓
▓ ▓ . v v ▓ . ▓ . ▓ ▓ ▓ . . ▓
▓ ▓ . . v v v v ▓ ▓ ▓ ▓ ▓ ▓ ▓
▓ . ▓ . ▓ ▓ v v ▓ ▓ ▓ . ▓ . .
. ▓ . . ▓ v v ▓ . ▓ . . . ▓ .
▓ ▓ . . . v v v ▓ . ▓ . . . ▓
. ▓ ▓ ▓ ▓ v v ▓ . ▓ ▓ ▓ ▓ . .
. . ▓ ▓ ▓ v v ▓ . . . . ▓ ▓ ▓
▓ ▓ ▓ ▓ ▓ v ▓ . ▓ . . ▓ ▓ ▓ .
▓ ▓ . ▓ ▓ v v ▓ . ▓ ▓ ▓ ▓ . .
▓ ▓ ▓ v v v ▓ . ▓ ▓ . ▓ ▓ ▓ .
▓ ▓ ▓ ▓ ▓ v v ▓ . ▓ . ▓ ▓ . .
▓ . . ▓ . ▓ v . . . . ▓ ▓ . ▓

Running 500 trials at each porosity:
p = 0.1: 0.000
p = 0.2: 0.000
p = 0.3: 0.000
p = 0.4: 0.002
p = 0.5: 0.074
p = 0.6: 0.508
p = 0.7: 0.970
p = 0.8: 1.000
p = 0.9: 1.000
p = 1.0: 1.000

```



## Fortran


Please see sample compilation and program execution in comments at top of program.  Thank you.  This example demonstrates recursion and integer constants of a specific kind.

```fortran

! loosely translated from python.
! compilation: gfortran -Wall -std=f2008 thisfile.f08

!$ a=site && gfortran -o $a -g -O0 -Wall -std=f2008 $a.f08 && $a
!100 trials per
!Fill Fraction goal(%)    simulated through paths(%)
!           0                          0
!          10                          0
!          20                          0
!          30                          0
!          40                          0
!          50                          6
!
!
!    b b b   b   h   j     m m m
!  b b   b b b   h h   m m m m m
!    b b b       h h h     m
!    b     h h h   h h h h
!  b b   h h   h h h h   h h h
!  b b b   h h h   h h h h h h h
!  b b   @   h   h   h h h h h
!      @ @       h h h h h h h h
!    @ @ @ @       h h   h   h
!      @ @ @ @       h h h h h h
!      @ @ @   h h h h     h h h
!  @ @ @     h h   h   h     h h
!    @       h         h h h h h
!  @     h h   h     h h h     h
!  @ @   h h h h h h h   h h   h
!          60                         59
!          70                         97
!          80                        100
!          90                        100
!         100                        100

program percolation_site
  implicit none
  integer, parameter :: m=15,n=15,t=100
  !integer, parameter :: m=2,n=2,t=8
  integer(kind=1), dimension(m, n) :: grid
  real :: p
  integer :: i, ip, trial, successes
  logical :: success, unseen, q
  data unseen/.true./
  write(6,'(i3,a11)') t,' trials per'
  write(6,'(a21,a30)') 'Fill Fraction goal(%)','simulated through paths(%)'
  do ip=0, 10
     p = ip/10.0
     successes = 0
     do trial = 1, t
        call newgrid(grid, p)
        success = .false.
        do i=1, m
           q = walk(grid, i)    ! deliberately compute all paths
           success = success .or. q
        end do
        if ((ip == 6) .and. unseen) then
           call display(grid)
           unseen = .false.
        end if
        successes = successes + merge(1, 0, success)
     end do
     write(6,'(9x,i3,24x,i3)')ip*10,nint(100*real(successes)/real(t))
  end do

contains

  logical function walk(grid, start)
    integer(kind=1), dimension(m,n), intent(inout) :: grid
    integer, intent(in) :: start
    walk = rwalk(grid, 1, start, int(start+1,1))
  end function walk

  recursive function rwalk(grid, i, j, k) result(through)
    logical :: through
    integer(kind=1), dimension(m,n), intent(inout) :: grid
    integer, intent(in) :: i, j
    integer(kind=1), intent(in) :: k
    logical, dimension(4) :: q
    !out of bounds
    through = .false.
    if (i < 1) return
    if (m < i) return
    if (j < 1) return
    if (n < j) return
    !visited or non-pore
    if (1_1 /= grid(i, j)) return
    !update grid and recurse with neighbors.  deny 'shortcircuit' evaluation
    grid(i, j) = k
    q(1) = rwalk(grid,i+0,j+1,k)
    q(2) = rwalk(grid,i+0,j-1,k)
    q(3) = rwalk(grid,i+1,j+0,k)
    q(4) = rwalk(grid,i-1,j+0,k)
    !newly discovered outlet
    through = (i == m) .or. any(q)
  end function rwalk

  subroutine newgrid(grid, probability)
    implicit none
    real :: probability
    integer(kind=1), dimension(m,n), intent(out) :: grid
    real, dimension(m,n) :: harvest
    call random_number(harvest)
    grid = merge(1_1, 0_1, harvest < probability)
  end subroutine newgrid

  subroutine display(grid)
    integer(kind=1), dimension(m,n), intent(in) :: grid
    integer :: i, j, k, L
    character(len=n*2) :: lineout
    write(6,'(/)')
    lineout = ' '
    do i=1,m
       do j=1,n
          k = j+j
          L = grid(i,j)+1
          lineout(k:k) = ' @abcdefghijklmnopqrstuvwxyz'(L:L)
       end do
       write(6,*) lineout
    end do
  end subroutine display

end program percolation_site

```



## Go


```go
package main

import (
	"bytes"
	"fmt"
	"math/rand"
	"time"
)

func main() {
	const (
		m, n           = 15, 15
		t              = 1e4
		minp, maxp, Δp = 0, 1, 0.1
	)

	rand.Seed(2) // Fixed seed for repeatable example grid
	g := NewGrid(.5, m, n)
	g.Percolate()
	fmt.Println(g)

	rand.Seed(time.Now().UnixNano()) // could pick a better seed
	for p := float64(minp); p < maxp; p += Δp {
		count := 0
		for i := 0; i < t; i++ {
			g := NewGrid(p, m, n)
			if g.Percolate() {
				count++
			}
		}
		fmt.Printf("p=%.2f, %.4f\n", p, float64(count)/t)
	}
}

const (
	full  = '.'
	used  = '#'
	empty = ' '
)

type grid struct {
	cell [][]byte // row first, i.e. [y][x]
}

func NewGrid(p float64, xsize, ysize int) *grid {
	g := &grid{cell: make([][]byte, ysize)}
	for y := range g.cell {
		g.cell[y] = make([]byte, xsize)
		for x := range g.cell[y] {
			if rand.Float64() < p {
				g.cell[y][x] = full
			} else {
				g.cell[y][x] = empty
			}
		}
	}
	return g
}

func (g *grid) String() string {
	var buf bytes.Buffer
	// Don't really need to call Grow but it helps avoid multiple
	// reallocations if the size is large.
	buf.Grow((len(g.cell) + 2) * (len(g.cell[0]) + 3))

	buf.WriteByte('+')
	for _ = range g.cell[0] {
		buf.WriteByte('-')
	}
	buf.WriteString("+\n")

	for y := range g.cell {
		buf.WriteByte('|')
		buf.Write(g.cell[y])
		buf.WriteString("|\n")
	}

	buf.WriteByte('+')
	ly := len(g.cell) - 1
	for x := range g.cell[ly] {
		if g.cell[ly][x] == used {
			buf.WriteByte(used)
		} else {
			buf.WriteByte('-')
		}
	}
	buf.WriteByte('+')
	return buf.String()
}

func (g *grid) Percolate() bool {
	for x := range g.cell[0] {
		if g.use(x, 0) {
			return true
		}
	}
	return false
}

func (g *grid) use(x, y int) bool {
	if y < 0 || x < 0 || x >= len(g.cell[0]) || g.cell[y][x] != full {
		return false // Off the edges, empty, or used
	}
	g.cell[y][x] = used
	if y+1 == len(g.cell) {
		return true // We're on the bottom
	}

	// Try down, right, left, up in that order.
	return g.use(x, y+1) ||
		g.use(x+1, y) ||
		g.use(x-1, y) ||
		g.use(x, y-1)
}
```

```txt
+---------------+
|####  ###.  .. |
| ##   # #   . .|
| ###   #### .. |
|### ##### #..  |
|   ### # ## .. |
|# ##     # . ..|
|### .    #.. . |
| ##      ##. ..|
| ## .. .. # .. |
| ##    . .#....|
|##   .. .##  . |
|# .  . . # .   |
| ..   . .#. .. |
|. . .... #  .. |
| . ..  . # .. .|
+---------#-----+
p=0.00, 0.0000
p=0.10, 0.0000
p=0.20, 0.0000
p=0.30, 0.0000
p=0.40, 0.0040
p=0.50, 0.0980
p=0.60, 0.5641
p=0.70, 0.9583
p=0.80, 0.9995
p=0.90, 1.0000
p=1.00, 1.0000

```


## Haskell


```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Random
import Data.Array.Unboxed
import Data.List
import Formatting

type Field = UArray (Int, Int) Char

-- Start percolating some seepage through a field.
-- Recurse to continue percolation with new seepage.
percolateR :: [(Int, Int)] -> Field -> (Field, [(Int,Int)])
percolateR [] f = (f, [])
percolateR seep f =
    let ((xLo,yLo),(xHi,yHi)) = bounds f
        validSeep = filter (\p@(x,y) ->    x >= xLo
                                        && x <= xHi
                                        && y >= yLo
                                        && y <= yHi
                                        && f!p == ' ') $ nub $ sort seep

        neighbors (x,y) = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]

    in  percolateR
            (concatMap neighbors validSeep)
            (f // map (\p -> (p,'.')) validSeep)

-- Percolate a field.  Return the percolated field.
percolate :: Field -> Field
percolate start =
    let ((_,_),(xHi,_)) = bounds start
        (final, _) = percolateR [(x,0) | x <- [0..xHi]] start
    in final

-- Generate a random field.
initField :: Int -> Int -> Double -> Rand StdGen Field
initField w h threshold = do
    frnd <- fmap (\rv -> if rv<threshold then ' ' else  '#') <$> getRandoms
    return $ listArray ((0,0), (w-1, h-1)) frnd

-- Get a list of "leaks" from the bottom of a field.
leaks :: Field -> [Bool]
leaks f =
    let ((xLo,_),(xHi,yHi)) = bounds f
    in [f!(x,yHi)=='.'| x <- [xLo..xHi]]

-- Run test once; Return bool indicating success or failure.
oneTest :: Int -> Int -> Double -> Rand StdGen Bool
oneTest w h threshold =
    or.leaks.percolate <$> initField w h threshold

-- Run test multple times; Return the number of tests that pass.
multiTest :: Int -> Int -> Int -> Double -> Rand StdGen Double
multiTest testCount w h threshold = do
    results <- replicateM testCount $ oneTest w h threshold
    let leakyCount = length $ filter id results
    return $ fromIntegral leakyCount / fromIntegral testCount

-- Display a field with walls and leaks.
showField :: Field -> IO ()
showField a =  do
    let ((xLo,yLo),(xHi,yHi)) = bounds a
    mapM_ print [ [ a!(x,y) | x <- [xLo..xHi]] | y <- [yLo..yHi]]

main :: IO ()
main = do
  g <- getStdGen
  let w = 15
      h = 15
      threshold = 0.6
      (startField, g2) = runRand (initField w h threshold) g

  putStrLn ("Unpercolated field with " ++ show threshold ++ " threshold.")
  putStrLn ""
  showField startField

  putStrLn ""
  putStrLn "Same field after percolation."
  putStrLn ""
  showField $ percolate startField

  let testCount = 10000
      densityCount = 10

  putStrLn ""
  putStrLn (   "Results of running percolation test " ++ show testCount
            ++ " times with thresholds ranging from 0/" ++ show densityCount
            ++ " to " ++ show densityCount ++ "/" ++ show densityCount ++ " .")

  let densities = [0..densityCount]
      tests = sequence [multiTest testCount w h v
                           | density <- densities,
                             let v = fromIntegral density / fromIntegral densityCount ]
      results = zip densities (evalRand tests g2)
  mapM_ print [format ("p=" % int % "/" % int % " -> " % fixed 4) density densityCount x | (density,x) <- results]
```


<pre style="font-size:80%">
Unpercolated field with 0.6 threshold.

"  ### # # # ## "
"### # ##  # #  "
"  #####  #   ##"
"#    # ##  #   "
"###         #  "
" ### ### #     "
"  ### #  ### ##"
"   # ## #    ##"
" #  #   # #  ##"
"### ## #       "
"   ## #      ##"
" #    # ## ## #"
" ### ##  ##    "
"#### # #  ## ##"
"   #    #    # "

Same field after percolation.

"..###.#.#.#.##."
"### #.##..#.#.."
"..#####..#...##"
"#....#.##..#..."
"###.........#.."
" ###.###.#....."
"  ### #..###.##"
"   # ##.#....##"
" #  #...#.#..##"
"### ##.#......."
"   ## #......##"
" #    #.##.##.#"
" ### ##..##...."
"#### # #..##.##"
"   #    #....# "

Results of running percolation test 10000 times with thresholds ranging from 0/10 to 10/10 .
"p=0/10 -> 0.0000"
"p=1/10 -> 0.0000"
"p=2/10 -> 0.0000"
"p=3/10 -> 0.0000"
"p=4/10 -> 0.0028"
"p=5/10 -> 0.0910"
"p=6/10 -> 0.5684"
"p=7/10 -> 0.9572"
"p=8/10 -> 0.9997"
"p=9/10 -> 1.0000"
"p=10/10 -> 1.0000"

```


## J


One approach:


```J
groups=:[: +/\ 2 </\ 0 , *
ooze=: [ >. [ +&* [ * [: ; groups@[ <@(* * 2 < >./)/. +
percolate=: ooze/\.@|.^:2^:_@(* (1 + # {. 1:))

trial=: percolate@([ >: ]?@$0:)
simulate=: %@[ * [: +/ (2 e. {:)@trial&15 15"0@#
```


Example Statistics:

```J
   ,.'  P THRU';(, 100&simulate)"0 (i.%<:)11
┌────────┐
│  P THRU│
├────────┤
│  0    0│
│0.1    0│
│0.2    0│
│0.3    0│
│0.4 0.01│
│0.5 0.09│
│0.6 0.61│
│0.7 0.97│
│0.8    1│
│0.9    1│
│  1    1│
└────────┘
```


Worked sample:


```J
   1j1 #"1 ' .#'{~ percolate 0.6>:?15 15$0
# #   # # #       #   #     #
#   # # #   # # #   # # # # #
# # #   # #   #     #       #
    #   # #   # # #     # # #
#     .       #   # # # # #
#       # # # # # # # #     #
# #   # # # # # #   # # # # #
  # # # # #     #   #   # # #
.                   #   #
  .   .           # #   #   #
. . .   .   # # # # # # # # #
. . . .   # #       # # # # #
. . .     #     .   # #   #
. . . .     . . .     # #   .
  .   . . .   . . . .   # #
```


An [[Percolation/Site_percolation/J|explanation with examples]] would be somewhat longer than the implementation.

Alternative implementation (with an incompatible internal API):


```J

any =: +./
all =: *./

quickCheck =: [: all [: (any"1) 2 *./\ ] NB. a complete path requires connections between all row pairs

percolate =: 15 15&$: : (dyad define) NB. returns 0 iff blocked   Use: (N, M) percolate P
 NB. make a binary grid
 GRID =: y (> ?@($&0)) x

 NB. compute the return value
 if. -. quickCheck GRID do. 0 return. end.
 STARTING_SITES =. 0 ,. ({. GRID) # i. {: x NB. indexes of 1 in head row of GRID
 any STARTING_SITES check GRID
)


NB. use local copy of GRID.  Too slow.
check =: dyad define"1 2 NB. return 1 iff through path found  use: START check GRID
 GRID =. y
 LOCATION =. x
 if. 0 (= #) LOCATION do. 0 return. end. NB. no starting point?  0
 if. LOCATION any@:((>: , 0 > [) $) GRID do. 0 return. end. NB. off grid?  0
 INDEX =. <LOCATION
 if. 1 ~: INDEX { GRID do. 0 return. end. NB. fail.  either already looked here or non-path
 if. (>: {. LOCATION) = (# GRID) do. 1 return. end. NB. Success!  (display GRID here)
 G =: GRID =. INDEX (>:@:{)`[`]}GRID
 any GRID check~ LOCATION +"1 (, -)0 1,:1 0
)

NB. use global GRID.
check =: dyad define"1 2 NB. return 1 iff through path found  use: START check GRID
 LOCATION =. x
 if. 0 (= #) LOCATION do. 0 return. end. NB. no starting point?  0
 if. LOCATION any@:((>: , 0 > [) $) GRID do. 0 return. end. NB. off grid?  0
 INDEX =. <LOCATION
 if. 1 ~: INDEX { GRID do. 0 return. end. NB. fail.  either already looked here or non-path
 if. (>: {. LOCATION) = (# GRID) do. 1 return. end. NB. Success!  (display GRID here)
 GRID =: INDEX (>:@:{)`[`]}GRID
 any GRID check~ LOCATION +"1 (, -)0 1,:1 0
)

simulate =: 100&$: : ([ %~ [: +/ [: percolate"0 #) NB. return fraction of connected cases.  Use: T simulate P

```



```txt

   ,. '   P  THRU' ; (, 100x&simulate)"0 (i. % <:)11x
+-----------+
|   P  THRU |
+-----------+
|   0      0|
|1r10      0|
| 1r5      0|
|3r10      0|
| 2r5  1r100|
| 1r2   1r20|
| 3r5  31r50|
|7r10 97r100|
| 4r5      1|
|9r10      1|
|   1      1|
+-----------+



   NB. example

   simulate 0.6
0.51

   GRID  NB. the final grid of the 100 simulated cases.
2 2 2 2 0 2 2 2 2 0 2 2 0 0 2
2 0 0 2 0 0 2 0 2 0 2 0 0 1 0
2 0 1 0 2 2 0 2 2 0 2 2 0 1 0
2 2 0 0 0 2 2 0 2 0 2 0 0 0 1
0 2 2 2 2 2 2 0 2 0 2 0 0 1 1
0 2 0 2 0 0 0 0 0 0 0 1 1 0 1
0 0 0 0 1 0 0 1 1 1 0 1 1 0 1
1 1 1 1 1 1 1 0 0 0 1 1 0 1 1
0 0 1 1 1 0 1 1 0 0 1 1 1 0 1
0 1 0 1 1 1 1 1 0 0 1 1 1 1 1
1 1 1 1 0 1 1 0 1 1 0 0 1 1 1
0 1 1 1 0 1 1 0 0 0 1 1 1 1 1
0 0 0 0 1 0 1 1 1 1 1 0 0 1 0
1 1 1 1 0 1 1 1 1 1 0 1 0 0 0
1 0 1 1 1 1 1 0 0 1 1 1 1 1 1


   (0 ,. 0 6 10 14) check GRID  NB. show possible starting points all fail
0 0 0 0

   1j1#"1 GRID { '#',~u: 32 16bb7 NB. sample paths with unicode pepper.
# # # #   # # # #   # #     #
#     #     #   #   #     ·
#   ·   # #   # #   # #   ·
# #       # #   #   #       ·
  # # # # # #   #   #     · ·
  #   #               · ·   ·
        ·     · · ·   · ·   ·
· · · · · · ·       · ·   · ·
    · · ·   · ·     · · ·   ·
  ·   · · · · ·     · · · · ·
· · · ·   · ·   · ·     · · ·
  · · ·   · ·       · · · · ·
        ·   · · · · ·     ·
· · · ·   · · · · ·   ·
·   · · · · ·     · · · · · ·

```



## Julia

```julia
using Distributions

newgrid(p::Float64, M::Int=15, N::Int=15) = rand(Bernoulli(p), M, N)

function walkmaze!(grid::Matrix{Int}, r::Int, c::Int, indx::Int)
    NOT_VISITED = 1 # const
    N, M = size(grid)
    dirs = [[1, 0], [-1, 0], [0, 1], [1, 0]]
    # fill cell
    grid[r, c] = indx

    # is the bottom line?
    rst = r == N

    # for each direction, if has not reached the bottom yet and can continue go to that direction
    for d in dirs
        rr, cc = (r, c) .+ d
        if !rst && checkbounds(Bool, grid, rr, cc) && grid[rr, cc] == NOT_VISITED
            rst = walkmaze!(grid, rr, cc, indx)
        end
    end
    return rst
end

function checkpath!(grid::Matrix{Int})
    NOT_VISITED = 1 # const
    N, M = size(grid)
    walkind = 1
    for m in 1:M
        if grid[1, m] == NOT_VISITED
            walkind += 1
            if walkmaze!(grid, 1, m, walkind)
                return true
            end
        end
    end
    return false
end

function printgrid(G::Matrix{Int})
    LETTERS = vcat(' ', '#', 'A':'Z')
    for r in 1:size(G, 1)
        println(r % 10, ") ", join(LETTERS[G[r, :] .+ 1], ' '))
    end
    if any(G[end, :] .> 1)
        println("!) ", join((ifelse(c > 1, LETTERS[c+1], ' ') for c in G[end, :]), ' '))
    end
end

const nrep = 1000 # const
sampleprinted = false

p = collect(0.0:0.1:1.0)
f = similar(p)
for i in linearindices(f)
    c = 0
    for _ in 1:nrep
        G = newgrid(p[i])
        perc = checkpath!(G)
        if perc
            c += 1
            if !sampleprinted
                @printf("Sample percolation, %i×%i grid, p = %.2f\n\n", size(G, 1), size(G, 2), p[i])
                printgrid(G)
                sampleprinted = true
            end
        end
    end
    f[i] = c / nrep
end

println("\nFrequencies for $nrep tries that percolate through\n")
for (pi, fi) in zip(p, f)
    @printf("p = %.1f ⇛ f = %.3f\n", pi, fi)
end
```


```txt
Sample percolation, 15×15 grid, p = 0.40

1) A A   B # #   #   #   #
2) A A   B     #       #   # #
3) A     B B B         #       #
4)   #   B   B     #     #     #
5) # #       B B B #   #       #
6) #   # #   B   B       # #
7)               B     #   #   #
8)   #           B       #     #
9)           # B B
0) # #         B   # #     # #
1) # #         B
2) #   #     # B     # #       #
3)   # # # #   B     #
4) # #   #   B B #   # #
5) #     #   B   #   #       #
!)           B

Frequencies for 1000 tries that percolate through

p = 0.0 ⇛ f = 0.000
p = 0.1 ⇛ f = 0.000
p = 0.2 ⇛ f = 0.000
p = 0.3 ⇛ f = 0.000
p = 0.4 ⇛ f = 0.001
p = 0.5 ⇛ f = 0.089
p = 0.6 ⇛ f = 0.559
p = 0.7 ⇛ f = 0.956
p = 0.8 ⇛ f = 1.000
p = 0.9 ⇛ f = 1.000
p = 1.0 ⇛ f = 1.000
```



## Kotlin

```scala
// version 1.2.10

import java.util.Random

val rand = Random()
const val RAND_MAX = 32767
const val NUL = '\u0000'

val x = 15
val y = 15
var grid = StringBuilder((x + 1) * (y + 1) + 1)
var cell = 0
var end = 0
var m = 0
var n = 0

fun makeGrid(p: Double) {
    val thresh = (p * RAND_MAX).toInt()
    m = x
    n = y
    grid.setLength(0)  // clears grid
    grid.setLength(m + 1)  // sets first (m + 1) chars to NUL
    end = m + 1
    cell = m + 1
    for (i in 0 until n) {
        for (j in 0 until m) {
            val r = rand.nextInt(RAND_MAX + 1)
            grid.append(if (r < thresh) '+' else '.')
            end++
        }
        grid.append('\n')
        end++
    }
    grid[end - 1] = NUL
    end -= ++m  // end is the index of the first cell of bottom row
}

fun ff(p: Int): Boolean { // flood fill
    if (grid[p] != '+') return false
    grid[p] = '#'
    return p >= end || ff(p + m) || ff(p + 1) || ff(p - 1) || ff(p - m)
}

fun percolate(): Boolean {
    var i = 0
    while (i < m && !ff(cell + i)) i++
    return i < m
}

fun main(args: Array<String>) {
    makeGrid(0.5)
    percolate()

    println("$x x $y grid:")
    println(grid)

    println("\nrunning 10,000 tests for each case:")
    for (ip in 0..10) {
        val p = ip / 10.0
        var cnt = 0
        for (i in 0 until 10_000) {
            makeGrid(p)
            if (percolate()) cnt++
        }
        println("p = %.1f:  %.4f".format(p, cnt / 10000.0))
    }
}
```


Sample output:

```txt

15 x 15 grid:
.#.##..##..##.#
.#.##..#..###.#
.....++.###.#..
....+.+..###...
+.+.+..+...####
..+.+.+..#..##.
++...+..###.###
+++.+.+.#.###.#
+..++...#.#.###
++..+.+.#..+...
.+.+.+++..+.+++
...++.+.++++...
+..+..+.++.++.+
+...++..++...+.
..+.+++..+..++.

running 10,000 tests for each case:
p = 0.0:  0.0000
p = 0.1:  0.0000
p = 0.2:  0.0000
p = 0.3:  0.0000
p = 0.4:  0.0038
p = 0.5:  0.0998
p = 0.6:  0.5617
p = 0.7:  0.9558
p = 0.8:  0.9998
p = 0.9:  1.0000
p = 1.0:  1.0000

```



## Perl

```perl
my $block = '▒';
my $water = '+';
my $pore  = ' ';
my $grid  = 15;
my @site;

$D{$_} = $i++ for qw<DeadEnd Up Right Down Left>;

sub deq { defined $_[0] && $_[0] eq $_[1] }

sub percolate {
    my($prob) = shift || 0.6;
    $site[0] = [($pore) x $grid];
    for my $y (1..$grid) {
        for my $x (0..$grid-1) {
            $site[$y][$x] = rand() < $prob ? $pore : $block;
        }
    }
    $site[$grid + 1] = [($pore) x $grid];
    $site[0][0] = $water;

    my $x = 0;
    my $y = 0;
    my @stack;

    while () {
        if (my $dir = direction($x,$y)) {
            push @stack, [$x,$y];
            ($x,$y) = move($dir, $x, $y)
        } else {
            return 0 unless @stack;
            ($x,$y) = @{pop @stack}
        }
        return 1 if $y > $grid;
    }
}

sub direction {
    my($x, $y) = @_;
    return $D{Down}  if deq($site[$y+1][$x  ], $pore);
    return $D{Right} if deq($site[$y  ][$x+1], $pore);
    return $D{Left}  if deq($site[$y  ][$x-1], $pore);
    return $D{Up}    if deq($site[$y-1][$x  ], $pore);
    return $D{DeadEnd};
}

sub move {
    my($dir,$x,$y) = @_;
    $site[--$y][   $x] = $water if $dir == $D{Up};
    $site[++$y][   $x] = $water if $dir == $D{Down};
    $site[  $y][ --$x] = $water if $dir == $D{Left};
    $site[  $y][ ++$x] = $water if $dir == $D{Right};
    $x, $y
}

my $prob = 0.65;
percolate($prob);

print "Sample percolation at $prob\n";
print join '', @$_, "\n" for @site;
print "\n";

my $tests = 100;
print "Doing $tests trials at each porosity:\n";
my @table;
for my $p (1 .. 10) {
    $p = $p/10;
    my $total = 0;
    $total += percolate($p) for 1..$tests;
    push @table, sprintf "p = %0.1f: %0.2f", $p, $total / $tests
}

print "$_\n" for @table;
```

```txt
Sample percolation at 0.65
+++
▒▒+    ▒ ▒
 ▒+▒▒▒ ▒▒ ▒▒  ▒
  +   ▒▒▒▒▒▒
▒▒++▒ ▒▒    ▒
▒ ▒++  ▒      ▒
 ▒▒▒+++▒  ▒ ▒
▒   ▒▒+▒ ▒   ▒
▒    ▒+       ▒
  ▒  ▒++▒▒
   ▒  ▒+  ▒ ▒
 ▒▒ ▒  ++   ▒
▒  ▒▒▒▒▒++ ▒
▒  ▒ ▒ ▒▒+ ▒
 ▒  ▒▒▒  +   ▒▒
▒    ▒   +  ▒▒
         +

Doing 100 trials at each porosity:
p = 0.1: 0.00
p = 0.2: 0.00
p = 0.3: 0.00
p = 0.4: 0.01
p = 0.5: 0.10
p = 0.6: 0.51
p = 0.7: 0.89
p = 0.8: 1.00
p = 0.9: 1.00
p = 1.0: 1.00
```



## Perl 6

```perl6
my $block = '▒';
my $water = '+';
my $pore  = ' ';
my $grid  = 15;
my @site;

enum Direction <DeadEnd Up Right Down Left>;

say 'Sample percolation at .6';
percolate(.6);
.join.say for @site;
say "\n";

my $tests = 1000;
say "Doing $tests trials at each porosity:";
for .1, .2 ... 1 -> $p {
    printf "p = %0.1f: %0.3f\n", $p, (sum percolate($p) xx $tests) / $tests
}

sub infix:<deq> ( $a, $b ) { $a.defined && ($a eq $b) }

sub percolate ( $prob  = .6 ) {
    @site[0] = [$pore xx $grid];
    @site[$grid + 1] = [$pore xx $grid];

    for ^$grid X 1..$grid -> ($x, $y) {
        @site[$y;$x] = rand < $prob ?? $pore !! $block
    }
    @site[0;0] = $water;

    my @stack;
    my $current = [0;0];

    loop {
        if my $dir = direction( $current ) {
            @stack.push: $current;
            $current = move( $dir, $current )
        }
        else {
            return 0 unless @stack;
            $current = @stack.pop
        }
        return 1 if $current[1] > $grid
    }

    sub direction( [$x, $y] ) {
        (Down  if @site[$y + 1][$x] deq $pore) ||
        (Left  if @site[$y][$x - 1] deq $pore) ||
        (Right if @site[$y][$x + 1] deq $pore) ||
        (Up    if @site[$y - 1][$x] deq $pore) ||
        DeadEnd
    }

    sub move ( $dir, @cur ) {
        my ( $x, $y ) = @cur;
        given $dir {
            when Up    { @site[--$y][$x] = $water }
            when Down  { @site[++$y][$x] = $water }
            when Left  { @site[$y][--$x] = $water }
            when Right { @site[$y][++$x] = $water }
        }
        [$x, $y]
    }
}
```

```txt
Sample percolation at .6
++++
▒▒▒+ ▒ ▒ ▒ ▒ ▒▒
 ▒▒++ ▒▒   ▒▒
   ▒+   ▒▒ ▒ ▒▒
▒▒ ▒++++▒ ▒▒
 ▒ ▒+▒▒+▒   ▒
  ▒++▒++ ▒▒▒ ▒
  ▒▒▒ +▒
▒▒ ▒ ▒++ ▒   ▒▒
▒▒▒▒▒▒▒+▒▒▒
▒   ▒  +   ▒
 ▒▒   ▒+ ▒  ▒ ▒
▒  ▒ ▒▒+    ▒
▒▒ ▒ ▒++▒   ▒
   ▒  +▒ ▒▒  ▒▒
▒  ▒▒▒+    ▒▒ ▒
      +


Doing 1000 trials at each porosity:
p = 0.1: 0.000
p = 0.2: 0.000
p = 0.3: 0.000
p = 0.4: 0.005
p = 0.5: 0.096
p = 0.6: 0.573
p = 0.7: 0.959
p = 0.8: 0.999
p = 0.9: 1.000
p = 1.0: 1.000

```



## Phix

```Phix
string grid
integer m, n, last, lastrow

enum SOLID = '#', EMPTY=' ', WET = 'v'

procedure make_grid(integer x, y, atom p)
    m = x
    n = y
    grid = repeat('\n',x*(y+1)+1)
    last = length(grid)
    lastrow = last-n
    for i=0 to x-1 do
        for j=1 to y do
            grid[1+i*(y+1)+j] = iff(rnd()<p?EMPTY:SOLID)
        end for
    end for
end procedure

function ff(integer i) -- flood_fill
    if i<=0 or i>=last or grid[i]!=EMPTY then return 0 end if
    grid[i] = WET
    return i>=lastrow or ff(i+m+1) or ff(i+1) or ff(i-1) or ff(i-m-1)
end function

function percolate()
    for i=2 to m+1 do
        if ff(i) then return true end if
    end for
    return false
end function

procedure main()
    make_grid(15,15,0.55)
    {} = percolate()
    printf(1,"%dx%d grid:%s",{15,15,grid})
    puts(1,"\nrunning 10,000 tests for each case:\n")
    for ip=0 to 10 do
        atom p = ip/10
        integer count = 0
        for i=1 to 10000 do
            make_grid(15, 15, p)
            count += percolate()
        end for
        printf(1,"p=%.1f:  %6.4f\n", {p, count/10000})
    end for
end procedure
main()
```

```txt

15x15 grid:
#v###vvv###vv#
##vvv##v#  #v #
  #vvvvvv#  v #
##vvv#vv# ##vv#
#vv#####  # #vv
### #  ### ###v
 # #####   ##vv
 ### # #  #vvvv
### ####  ##vvv
##   ##  vvvvvv
     #  #v##vvv
    ## #vv# ##v
  #     v######
        v
  ##### v#### #

running 10,000 tests for each case:
p=0.0:  0.0000
p=0.1:  0.0000
p=0.2:  0.0000
p=0.3:  0.0000
p=0.4:  0.0035
p=0.5:  0.0933
p=0.6:  0.5601
p=0.7:  0.9561
p=0.8:  0.9997
p=0.9:  1.0000
p=1.0:  1.0000

```



## Python


```python
from random import random
import string
from pprint import pprint as pp

M, N, t = 15, 15, 100

cell2char = ' #' + string.ascii_letters
NOT_VISITED = 1     # filled cell not walked

class PercolatedException(Exception): pass

def newgrid(p):
    return [[int(random() < p) for m in range(M)] for n in range(N)] # cell

def pgrid(cell, percolated=None):
    for n in range(N):
        print( '%i)  ' % (n % 10)
               + ' '.join(cell2char[cell[n][m]] for m in range(M)))
    if percolated:
        where = percolated.args[0][0]
        print('!)  ' + '  ' * where + cell2char[cell[n][where]])

def check_from_top(cell):
    n, walk_index = 0, 1
    try:
        for m in range(M):
            if cell[n][m] == NOT_VISITED:
                walk_index += 1
                walk_maze(m, n, cell, walk_index)
    except PercolatedException as ex:
        return ex
    return None


def walk_maze(m, n, cell, indx):
    # fill cell
    cell[n][m] = indx
    # down
    if n < N - 1 and cell[n+1][m] == NOT_VISITED:
        walk_maze(m, n+1, cell, indx)
    # THE bottom
    elif n == N - 1:
        raise PercolatedException((m, indx))
    # left
    if m and cell[n][m - 1] == NOT_VISITED:
        walk_maze(m-1, n, cell, indx)
    # right
    if m < M - 1 and cell[n][m + 1] == NOT_VISITED:
        walk_maze(m+1, n, cell, indx)
    # up
    if n and cell[n-1][m] == NOT_VISITED:
        walk_maze(m, n-1, cell, indx)

if __name__ == '__main__':
    sample_printed = False
    pcount = {}
    for p10 in range(11):
        p = p10 / 10.0
        pcount[p] = 0
        for tries in range(t):
            cell = newgrid(p)
            percolated = check_from_top(cell)
            if percolated:
                pcount[p] += 1
                if not sample_printed:
                    print('\nSample percolating %i x %i, p = %5.2f grid\n' % (M, N, p))
                    pgrid(cell, percolated)
                    sample_printed = True
    print('\n p: Fraction of %i tries that percolate through\n' % t )

    pp({p:c/float(t) for p, c in pcount.items()})
```


The Ascii art grid of cells has blanks for cells that were not filled. Filled cells start off as the '#', hash character and are changed to a succession of printable characters by successive tries to navigate from the top, (top - left actually), filled cell to the bottom.

The '!)' row shows where the percolation finished and you can follow the letter backwards from that row, (letter 'c' in this case), to get the route. The program stops after finding its first route through.

```txt
Sample percolating 15 x 15, p =  0.40 grid

0)    a a a       b   c #
1)    a a   #         c c   #   #
2)        # #   # #     c # # #
3)  #   #       # # #   c
4)    #     #         c c c c c c
5)  # # # # # #         c   c   c
6)        # # #         c   c   c
7)  #   #     # #     #   #   # c
8)  #   # #     #   #       c c c
9)    #       #         #   c
0)  #       #   # # # #   c c # #
1)      #     #   #     # c
2)  #     # # # # #   c c c   c
3)  #   # # #         c   c c c
4)      #           # c         #
!)                    c

 p: Fraction of 100 tries that percolate through

{0.0: 0.0,
 0.1: 0.0,
 0.2: 0.0,
 0.3: 0.0,
 0.4: 0.01,
 0.5: 0.11,
 0.6: 0.59,
 0.7: 0.94,
 0.8: 1.0,
 0.9: 1.0,
 1.0: 1.0}
```

Note the abrupt change in percolation at around p = 0.6. These abrupt changes are [http://mathworld.wolfram.com/PercolationThreshold.html expected].


## Racket


```racket
#lang racket
(require racket/require (only-in racket/fixnum for*/fxvector))
(require (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define cell-empty   0)
(define cell-filled  1)
(define cell-wall    2)
(define cell-visited 3)
(define cell-exit    4)

(define ((percol->generator p)) (if (< (random) p) cell-filled cell-empty))

(define t (make-parameter 1000))

(define ((make-percol-grid M N) p)
  (define p->10 (percol->generator p))
  (define M+1 (fx+ 1 M))
  (define M+2 (fx+ 2 M))
  (for*/fxvector
   #:length (fx* N M+2)
   ((n (in-range N)) (m (in-range M+2)))
   (cond
     [(fx= 0 m) cell-wall]
     [(fx= m M+1) cell-wall]
     [else (p->10)])))

(define (cell->str c) (substring " #|+*" c (fx+ 1 c)))

(define ((draw-percol-grid M N) g)
  (define M+2 (fx+ M 2))
  (for ((row N))
    (for ((col (in-range M+2)))
      (define idx (fx+ (fx* M+2 row) col))
      (printf "~a" (cell->str (fxvector-ref g idx))))
    (newline)))

(define ((percolate-percol-grid?! M N) g)
  (define M+2 (fx+ M 2))
  (define N-1 (fx- N 1))
  (define max-idx (fx* N M+2))
  (define (inner-percolate g idx)
    (define row (fxquotient idx M+2))
    (cond
      ((fx< idx 0) #f)
      ((fx>= idx max-idx) #f)
      ((fx= N-1 row) (fxvector-set! g idx cell-exit) #t)
      ((fx= cell-filled (fxvector-ref g idx))
       (fxvector-set! g idx cell-visited)
       (or
        ; gravity first (thanks Mr Newton)
        (inner-percolate g (fx+ idx M+2))
        ; stick-to-the-left
        (inner-percolate g (fx- idx 1))
        (inner-percolate g (fx+ idx 1))
        ; go uphill only if we have to!
        (inner-percolate g (fx- idx M+2))))
      (else #f)))
  (for/first ((m (in-range 1 M+2)) #:when (inner-percolate g m)) g))

(define make-15x15-grid (make-percol-grid 15 15))
(define draw-15x15-grid (draw-percol-grid 15 15))
(define perc-15x15-grid?! (percolate-percol-grid?! 15 15))

(define (display-sample-percolation p)
  (printf "Percolation sample: p=~a~%" p)
  (for*/first
      ((i (in-naturals))
       (g (in-value (make-15x15-grid 0.6)))
       #:when (perc-15x15-grid?! g))
    (draw-15x15-grid g))
  (newline))

(display-sample-percolation 0.4)

(for ((p (sequence-map (curry * 1/10) (in-range 0 (add1 10)))))
  (define n-percolated-grids
    (for/sum
     ((i (in-range (t))) #:when (perc-15x15-grid?! (make-15x15-grid p))) 1))
  (define proportion-percolated (/ n-percolated-grids (t)))
  (printf "p=~a\t->\t~a~%" p (real->decimal-string proportion-percolated 4)))
```


```txt
Percolation sample: p=0.4
|+++ ++++  + +++|
| +++ ++ #     +|
|   +  ++   ##++|
| ##    +  ###+ |
| ###### + #+++#|
|  ##### +  +  #|
|## # # +++++## |
|### # ++ +++#  |
|##  ## +++++#  |
|# ###   ++ +  #|
| ## ## +++   ##|
|##  ##  +++ # #|
|###   #   +### |
|####  ####+  # |
|# ## #    *#  #|

p=0	->	0.0000
p=1/10	->	0.0000
p=1/5	->	0.0000
p=3/10	->	0.0000
p=2/5	->	0.0030
p=1/2	->	0.1110
p=3/5	->	0.5830
p=7/10	->	0.9530
p=4/5	->	1.0000
p=9/10	->	1.0000
p=1	->	1.0000
```



## Sidef

```ruby
class Percolate {

    has block = '▒'
    has water = '+'
    has pore  = ' '
    has grid  = 15
    has site  = []

    enum <DeadEnd, Up, Right, Down, Left>

    method direction(x, y) {
        ((site[y + 1][x] == pore) && Down ) ||
        ((site[y][x - 1] == pore) && Left ) ||
        ((site[y][x + 1] == pore) && Right) ||
        ((site[y - 1][x] == pore) && Up   ) ||
        DeadEnd
    }

    method move(dir, x, y) {
        given (dir) {
            when (Up)    { site[--y][x] = water }
            when (Down)  { site[++y][x] = water }
            when (Left)  { site[y][--x] = water }
            when (Right) { site[y][++x] = water }
        }
        return (x, y)
    }

    method percolate (prob  = 0.6) {
        site[0] = grid.of(pore)
        site[grid + 1] = grid.of(pore)

        for x = ^grid, y = 1..grid {
            site[y][x] = (1.rand < prob ? pore : block)
        }

        site[0][0] = water

        var stack = []
        var (x, y) = (0, 0)

        loop {
            if (var dir = self.direction(x, y)) {
                stack << [x, y]
                (x,y) = self.move(dir, x, y)
            }
            else {
                stack || return 0
                (x,y) = stack.pop...
            }
            return 1 if (y > grid)
        }
    }
}

var obj = Percolate()
say 'Sample percolation at 0.6'
obj.percolate(0.6)
obj.site.each { .join.say }
say ''

var tests = 100
say "Doing #{tests} trials at each porosity:"
for p in (0.1..1 `by` 0.1) {
    printf("p = %0.1f: %0.3f\n", p, tests.of { obj.percolate(p) }.sum / tests)
}
```

```txt

Sample percolation at 0.6
+
+    ▒▒▒  ▒ ▒▒
+  ▒  ▒   ▒ ▒
++++  ▒  ▒    ▒
▒+▒+++++    ▒ ▒
 ▒ ▒▒▒▒+
▒▒ ▒ ▒ +      ▒
▒     ▒+++ ▒  ▒
  ▒ ▒ ▒+▒+  ▒
▒ ▒    ▒ + ▒  ▒
 ▒   ▒ +++   ▒▒
▒▒▒ ▒▒▒+▒+▒ ▒ ▒
▒   ▒▒ + ▒    ▒
 ▒▒  ▒▒+++  ▒ ▒
     ▒ ▒▒+
▒   ▒ ▒  +
         +

Doing 100 trials at each porosity:
p = 0.1: 0.000
p = 0.2: 0.000
p = 0.3: 0.000
p = 0.4: 0.020
p = 0.5: 0.090
p = 0.6: 0.570
p = 0.7: 0.930
p = 0.8: 1.000
p = 0.9: 1.000
p = 1.0: 1.000

```



## Tcl

```tcl
package require Tcl 8.6

oo::class create SitePercolation {
    variable cells w h
    constructor {width height probability} {
	set w $width
	set h $height
	for {set cells {}} {[llength $cells] < $h} {lappend cells $row} {
	    for {set row {}} {[llength $row] < $w} {lappend row $cell} {
		set cell [expr {rand() < $probability}]
	    }
	}
    }
    method print {out} {
	array set map {0 "#" 1 " " -1 .}
	puts "+[string repeat . $w]+"
	foreach row $cells {
	    set s "|"
	    foreach cell $row {
		append s $map($cell)
	    }
	    puts [append s "|"]
	}
	set outline [lrepeat $w "-"]
	foreach index $out {
	    lset outline $index "."
	}
	puts "+[join $outline {}]+"
    }
    method percolate {} {
	for {set work {}; set i 0} {$i < $w} {incr i} {
	    if {[lindex $cells 0 $i]} {lappend work 0 $i}
	}
	try {
	    my Fill $work
	    return {}
	} trap PERCOLATED x {
	    return [list $x]
	}
    }
    method Fill {queue} {
	while {[llength $queue]} {
	    set queue [lassign $queue y x]
	    if {$y >= $h} {throw PERCOLATED $x}
	    if {$y < 0 || $x < 0 || $x >= $w} continue
	    if {[lindex $cells $y $x]<1} continue
	    lset cells $y $x -1
	    lappend queue [expr {$y+1}] $x [expr {$y-1}] $x
	    lappend queue $y [expr {$x-1}] $y [expr {$x+1}]
	}
    }
}

# Demonstrate one run
puts "Sample percolation, 15x15 p=0.6"
SitePercolation create bp 15 15 0.6
bp print [bp percolate]
bp destroy
puts ""

# Collect statistics
apply {{} {
    puts "Percentage of tries that percolate, varying p"
    set tries 100
    for {set pint 0} {$pint <= 10} {incr pint} {
	set p [expr {$pint * 0.1}]
	set tot 0
	for {set i 0} {$i < $tries} {incr i} {
	    set bp [SitePercolation new 15 15 $p]
	    if {[$bp percolate] ne ""} {
		incr tot
	    }
	    $bp destroy
	}
	puts [format "p=%.2f: %2.1f%%" $p [expr {$tot*100./$tries}]]
    }
}}
```

```txt

Sample percolation, 15x15 p=0.6
+...............+
|.##...###.##...|
|.#.#####.####..|
|............##.|
|....###.###.#..|
|.#.##..#....#..|
|#.........#..#.|
|..#...##.##....|
|#.#.#....##...#|
|###.....#.#...#|
|.....##........|
|.#.#..## ......|
|  #..## # .##.#|
| # #.#  ####...|
|# #  # #  ##...|
| ###   ##  # . |
+-------------.-+

Percentage of tries that percolate, varying p
p=0.00: 0.0%
p=0.10: 0.0%
p=0.20: 0.0%
p=0.30: 0.0%
p=0.40: 0.0%
p=0.50: 6.0%
p=0.60: 54.0%
p=0.70: 98.0%
p=0.80: 100.0%
p=0.90: 100.0%
p=1.00: 100.0%

```



## zkl

```zkl
fcn makeGrid(m,n,p){
   grid:=Data((m+1)*(n+1));  // first row and right edges are buffers
   grid.write(" "*m); grid.write("\r");
   do(n){
      do(m){ grid.write(((0.0).random(1)<p) and "+" or "."); }  // cell is porous or not
      grid.write("\n");
   }
   grid
}
fcn ff(grid,x,m){ // walk across row looking for a porous cell
   if(grid[x]!=43) return(0); // '+' == 43 ASCII == porous
   grid[x]="#";
   return(x+m>=grid.len() or
	  ff(grid,x+m,m) or ff(grid,x+1,m) or ff(grid,x-1,m) or ff(grid,x-m,m));
}
fcn percolate(grid,m){
   x:=m+1; i:=0; while(i<m and not ff(grid,x,m)){ x+=1; i+=1; }
   return(i<m);  // percolated through the grid?
}

grid:=makeGrid(15,15,0.60);
println("Did liquid percolate: ",percolate(grid,15));
println("15x15 grid:\n",grid.text);

println("Running 10,000 tests for each case:");
foreach p in ([0.0 .. 1.0, 0.1]){
   cnt:=0.0; do(10000){ cnt+=percolate(makeGrid(15,15,p),15); }
   "p=%.1f:  %.4f".fmt(p, cnt/10000).println();
}
```

```txt

Did liquid percolate: True
15x15 grid:
.###.##.#++..++
......+###..+.+
+...+...##..+++
++..+.+.#+.+.++
..+++###..+..++
.+.##..++.+..++
.+#.+..++++++..
+####+..+....++
.#.#..+..++.+.+
#.#++++.+++.+++
+#++..+.+.+.+++
#######..++++++
#.##.#+++...+..
+.#.#+++.++.+++
+.+#+.++..+..++

Running 10,000 tests for each case:
p=0.0:  0.0000
p=0.1:  0.0000
p=0.2:  0.0000
p=0.3:  0.0000
p=0.4:  0.0006
p=0.5:  0.0304
p=0.6:  0.2989
p=0.7:  0.8189
p=0.8:  0.9903
p=0.9:  1.0000
p=1.0:  1.0000

```

