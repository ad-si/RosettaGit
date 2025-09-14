+++
title = "Percolation/Bond percolation"
description = ""
date = 2019-01-31T17:13:18Z
aliases = []
[extra]
id = 15821
[taxonomies]
categories = ["task", "Percolation Simulations"]
tags = []
languages = [
  "c",
  "cpp",
  "d",
  "go",
  "haskell",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "swift",
  "tcl",
  "zkl",
]
+++

## Task

Given an <math>M \times N</math> rectangular array of cells numbered <math>\mathrm{cell}[0..M-1, 0..N-1]</math>, assume <math>M</math> is horizontal and <math>N</math> is downwards. Each <math>\mathrm{cell}[m, n]</math> is bounded by (horizontal) walls <math>\mathrm{hwall}[m, n]</math> and <math>\mathrm{hwall}[m+1, n]</math>; (vertical) walls <math>\mathrm{vwall}[m, n]</math> and <math>\mathrm{vwall}[m, n+1]</math>

Assume that the probability of any wall being present is a constant <math>p</math> where
: <math>0.0 \le p \le 1.0</math>
Except for the outer horizontal walls at <math>m = 0</math> and <math>m = M</math> which are always present.

;The task:
Simulate pouring a fluid onto the top surface (<math>n = 0</math>) where the fluid will enter any empty cell it is adjacent to if there is no wall between where it currently is and the cell on the other side of the (missing) wall.

The fluid does not move beyond the horizontal constraints of the grid.

The fluid may move “up” within the confines of the grid of cells. If the fluid reaches a bottom cell that has a missing bottom wall then the fluid can be said to 'drip' out the bottom at that point.

Given <math>p</math> repeat the percolation <math>t</math> times to estimate the proportion of times that the fluid can percolate to the bottom for any given <math>p</math>.

Show how the probability of percolating through the random grid changes with <math>p</math> going from <math>0.0</math> to <math>1.0</math> in <math>0.1</math> increments and with the number of repetitions to estimate the fraction at any given <math>p</math> as <math>t = 100</math>.

Use an <math>M=10, N=10</math> grid of cells for all cases.

Optionally depict fluid successfully percolating through a grid graphically.

Show all output on this page.




## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// cell states
#define FILL 1
#define RWALL 2 // right wall
#define BWALL 4 // bottom wall

typedef unsigned int c_t;

c_t *cells, *start, *end;
int m, n;

void make_grid(double p, int x, int y)
{
    int i, j, thresh = RAND_MAX * p;
    m = x, n = y;

    // Allocate two addition rows to avoid checking bounds.
    // Bottom row is also required by drippage
    start = realloc(start, m * (n + 2) * sizeof(c_t));
    cells = start + m;

    for (i = 0; i < m; i++)
        start[i] = BWALL | RWALL;

    for (i = 0, end = cells; i < y; i++) {
        for (j = x; --j; )
            *end++ = (rand() < thresh ? BWALL : 0)
                |(rand() < thresh ? RWALL : 0);
        *end++ = RWALL | (rand() < thresh ? BWALL: 0);
    }
    memset(end, 0, sizeof(c_t) * m);
}

void show_grid(void)
{
    int i, j;

    for (j = 0; j < m; j++) printf("+--");
    puts("+");

    for (i = 0; i <= n; i++) {
        putchar(i == n ? ' ' : '|');
        for (j = 0; j < m; j++) {
            printf((cells[i*m + j] & FILL) ? "[]" : "  ");
            putchar((cells[i*m + j] & RWALL) ? '|' : ' ');
        }
        putchar('\n');

        if (i == n) return;

        for (j = 0; j < m; j++)
            printf((cells[i*m + j] & BWALL) ? "+--" : "+  ");
        puts("+");
    }
}

int fill(c_t *p)
{
    if ((*p & FILL)) return 0;
    *p |= FILL;
    if (p >= end) return 1; // success: reached bottom row

    return  ( !(p[ 0] & BWALL) && fill(p + m) ) ||
        ( !(p[ 0] & RWALL) && fill(p + 1) ) ||
        ( !(p[-1] & RWALL) && fill(p - 1) ) ||
        ( !(p[-m] & BWALL) && fill(p - m) );
}

int percolate(void)
{
    int i;
    for (i = 0; i < m && !fill(cells + i); i++);

    return i < m;
}

int main(void)
{
    make_grid(.5, 10, 10);
    percolate();
    show_grid();

    int cnt, i, p;

    puts("\nrunning 10x10 grids 10000 times for each p:");
    for (p = 1; p < 10; p++) {
        for (cnt = i = 0; i < 10000; i++) {
            make_grid(p / 10., 10, 10);
            cnt += percolate();
            //show_grid(); // don't
        }
        printf("p = %3g: %.4f\n", p / 10., (double)cnt / i);
    }

    free(start);
    return 0;
}
```

```txt

+--+--+--+--+--+--+--+--+--+--+
|[]|[] []|[] [] [] [] []      |
+  +  +  +--+--+--+--+  +  +--+
|[]|[]|[]|  |         [] []|  |
+  +--+  +--+--+--+  +--+  +  +
|[] [] [] []|        |   []|  |
+--+  +--+--+  +--+  +--+  +--+
|  |[]|     |  |        |[]   |
+--+--+  +  +  +  +  +--+  +  +
|     |  |     |         []   |
+--+  +  +--+  +--+--+  +  +--+
|  |     |     |[] [] [] []|  |
+  +  +  +--+  +  +--+--+--+--+
|  |  |     |   []   |  |  |  |
+--+  +--+--+--+  +  +  +--+  +
|  |  |  |  |  |[]|           |
+--+  +  +  +  +  +--+  +  +  +
|  |  |  |   [] []|  |  |  |  |
+--+  +--+--+  +--+  +  +  +  +
|  |     |   []|           |  |
+--+  +--+--+  +  +--+--+  +  +
             []

running 10x10 grids 10000 times for each p:
p = 0.1: 1.0000
p = 0.2: 1.0000
p = 0.3: 0.9958
p = 0.4: 0.9123
p = 0.5: 0.5014
p = 0.6: 0.0791
p = 0.7: 0.0037
p = 0.8: 0.0000
p = 0.9: 0.0000

```



## C++

```cpp
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

using namespace std;

class Grid {
public:
    Grid(const double p, const int x, const int y) : m(x), n(y) {
        const int thresh = static_cast<int>(RAND_MAX * p);

        // Allocate two addition rows to avoid checking bounds.
        // Bottom row is also required by drippage
        start = new cell[m * (n + 2)];
        cells = start + m;
        for (auto i = 0; i < m; i++) start[i] = RBWALL;
        end = cells;
        for (auto i = 0; i < y; i++) {
            for (auto j = x; --j;)
                *end++ = (rand() < thresh ? BWALL : 0) | (rand() < thresh ? RWALL : 0);
            *end++ = RWALL | (rand() < thresh ? BWALL : 0);
        }
        memset(end, 0u, sizeof(cell) * m);
    }

    ~Grid() {
        delete[] start;
        cells = 0;
        start = 0;
        end = 0;
    }

    int percolate() const {
        auto i = 0;
        for (; i < m && !fill(cells + i); i++);
        return i < m;
    }

    void show() const {
        for (auto j = 0; j < m; j++)
            cout << ("+-");
        cout << '+' << endl;

        for (auto i = 0; i <= n; i++) {
            cout << (i == n ? ' ' : '|');
            for (auto j = 0; j < m; j++) {
                cout << ((cells[i * m + j] & FILL) ? "#" : " ");
                cout << ((cells[i * m + j] & RWALL) ? '|' : ' ');
            }
            cout << endl;

            if (i == n) return;

            for (auto j = 0; j < m; j++)
                cout << ((cells[i * m + j] & BWALL) ? "+-" : "+ ");
            cout << '+' << endl;
        }
    }

private:
    enum cell_state {
        FILL   = 1 << 0,
        RWALL  = 1 << 1,       // right wall
        BWALL  = 1 << 2,       // bottom wall
        RBWALL = RWALL | BWALL // right/bottom wall
    };

    typedef unsigned int cell;

    bool fill(cell* p) const {
        if ((*p & FILL)) return false;
        *p |= FILL;
        if (p >= end) return true; // success: reached bottom row

        return (!(p[0] & BWALL) && fill(p + m)) || (!(p[0] & RWALL) && fill(p + 1))
                ||(!(p[-1] & RWALL) && fill(p - 1)) || (!(p[-m] & BWALL) && fill(p - m));
    }

    cell* cells;
    cell* start;
    cell* end;
    const int m;
    const int n;
};

int main() {
    const auto M = 10, N = 10;
    const Grid grid(.5, M, N);
    grid.percolate();
    grid.show();

    const auto C = 10000;
    cout << endl << "running " << M << "x" << N << " grids " << C << " times for each p:" << endl;
    for (auto p = 1; p < M; p++) {
        auto cnt = 0, i = 0;
        for (; i < C; i++)
            cnt += Grid(p / static_cast<double>(M), M, N).percolate();
        cout << "p = " << p / static_cast<double>(M) << ": " << static_cast<double>(cnt) / i << endl;
    }

    return EXIT_SUCCESS;
}
```



## D

```d
import std.stdio, std.random, std.array, std.range, std.algorithm;

struct Grid {
    // Not enforced by runtime and type system:
    // a Cell must contain only the flags bits.
    alias Cell = uint;

    enum : Cell { // Cell states (bit flags).
        empty      = 0,
        filled     = 1,
        rightWall  = 2,
        bottomWall = 4
    }

    const size_t nc, nr;
    Cell[] cells;

    this(in size_t nRows, in size_t nCols) pure nothrow {
        nr = nRows;
        nc = nCols;

        // Allocate two addition rows to avoid checking bounds.
        // Bottom row is also required by drippage.
        cells = new Cell[nc * (nr + 2)];
    }

    void initialize(in double prob, ref Xorshift rng) {
        cells[0 .. nc] = bottomWall | rightWall; // First row.

        uint pos = nc;
        foreach (immutable r; 1 .. nr + 1) {
            foreach (immutable c; 1 .. nc)
                cells[pos++] = (uniform01 < prob ?bottomWall : empty) |
                               (uniform01 < prob ? rightWall : empty);
            cells[pos++] = rightWall |
                           (uniform01 < prob ? bottomWall : empty);
        }

        cells[$ - nc .. $] = empty; // Last row.
    }

    bool percolate() pure nothrow @nogc {
        bool fill(in size_t i) pure nothrow @nogc {
            if (cells[i] & filled)
                return false;

            cells[i] |= filled;

            if (i >= cells.length - nc)
                return true; // Success: reached bottom row.

            return (!(cells[i]      & bottomWall) && fill(i + nc)) ||
                   (!(cells[i]      & rightWall)  && fill(i + 1)) ||
                   (!(cells[i - 1]  & rightWall)  && fill(i - 1)) ||
                   (!(cells[i - nc] & bottomWall) && fill(i - nc));
        }

        return iota(nc, nc + nc).any!fill;
    }

    void show() const {
        writeln("+-".replicate(nc), '+');

        foreach (immutable r; 1 .. nr + 2) {
            write(r == nr + 1 ? ' ' : '|');
            foreach (immutable c; 0 .. nc) {
                immutable cell = cells[r * nc + c];
                write((cell & filled) ? (r <= nr ? '#' : 'X') : ' ');
                write((cell & rightWall) ? '|' : ' ');
            }
            writeln;

            if (r == nr + 1)
                return;

            foreach (immutable c; 0 .. nc)
                write((cells[r * nc + c] & bottomWall) ? "+-" : "+ ");
            '+'.writeln;
        }
    }
}

void main() {
    enum uint nr = 10, nc = 10; // N. rows and columns of the grid.
    enum uint nTries = 10_000;  // N. simulations for each probability.
    enum uint nStepsProb = 10;  // N. steps of probability.

    auto rng = Xorshift(2);
    auto g = Grid(nr, nc);
    g.initialize(0.5, rng);
    g.percolate;
    g.show;

    writefln("\nRunning %dx%d grids %d times for each p:",
             nr, nc, nTries);
    foreach (immutable p; 0 .. nStepsProb) {
        immutable probability = p / double(nStepsProb);
        uint nPercolated = 0;
        foreach (immutable i; 0 .. nTries) {
            g.initialize(probability, rng);
            nPercolated += g.percolate;
        }
        writefln("p = %0.2f: %.4f",
                 probability, nPercolated / double(nTries));
    }
}
```

```txt
+-+-+-+-+-+-+-+-+-+-+
|#|#|#|#|     | |   |
+ +-+-+ +-+-+-+ +-+-+
|#| |  #  | | |   | |
+ +-+-+ + +-+-+ + +-+
|#|# #|#|   | |     |
+ +-+ + +-+ + + +-+ +
|#|# #|#|   | |   | |
+-+ + + + +-+-+-+-+-+
|# # # # #  | |   | |
+ + + + + + + +-+ +-+
|#|# # #|# # #  |   |
+-+ + + +-+-+ + + + +
| |#|# #| | |#      |
+-+-+-+-+ +-+ +-+-+-+
| |   |    # #|     |
+-+-+-+ +-+ +-+-+-+ +
| | |      # # #    |
+ + +-+ +-+-+-+ +-+ +
|     |   |   |#    |
+ +-+ + + + +-+ + + +
               X

Running 10x10 grids 10000 times for each p:
p = 0.00: 1.0000
p = 0.10: 1.0000
p = 0.20: 1.0000
p = 0.30: 0.9973
p = 0.40: 0.9177
p = 0.50: 0.5050
p = 0.60: 0.0880
p = 0.70: 0.0035
p = 0.80: 0.0001
p = 0.90: 0.0000
```

With LDC2 compiler this code runs in 0.26 seconds (almost two times faster than the C entry).


## Go

{{trans|C}}<!-- sort of ended up like the C version, not an actual translation -->

```go
package main

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
)

func main() {
	const (
		m, n           = 10, 10
		t              = 1000
		minp, maxp, Δp = 0.1, 0.99, 0.1
	)

	// Purposely don't seed for a repeatable example grid:
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
		fmt.Printf("p=%.2f, %.3f\n", p, float64(count)/t)
	}
}

type cell struct {
	full        bool
	right, down bool // true if open to the right (x+1) or down (y+1)
}

type grid struct {
	cell [][]cell // row first, i.e. [y][x]
}

func NewGrid(p float64, xsize, ysize int) *grid {
	g := &grid{cell: make([][]cell, ysize)}
	for y := range g.cell {
		g.cell[y] = make([]cell, xsize)
		for x := 0; x < xsize-1; x++ {
			if rand.Float64() > p {
				g.cell[y][x].right = true
			}
			if rand.Float64() > p {
				g.cell[y][x].down = true
			}
		}
		if rand.Float64() > p {
			g.cell[y][xsize-1].down = true
		}
	}
	return g
}

var (
	full  = map[bool]string{false: "  ", true: "**"}
	hopen = map[bool]string{false: "--", true: "  "}
	vopen = map[bool]string{false: "|", true: " "}
)

func (g *grid) String() string {
	var buf strings.Builder
	// Don't really need to call Grow but it helps avoid multiple
	// reallocations if the size is large.
	buf.Grow((len(g.cell) + 1) * len(g.cell[0]) * 7)

	for _ = range g.cell[0] {
		buf.WriteString("+")
		buf.WriteString(hopen[false])
	}
	buf.WriteString("+\n")
	for y := range g.cell {
		buf.WriteString(vopen[false])
		for x := range g.cell[y] {
			buf.WriteString(full[g.cell[y][x].full])
			buf.WriteString(vopen[g.cell[y][x].right])
		}
		buf.WriteByte('\n')
		for x := range g.cell[y] {
			buf.WriteString("+")
			buf.WriteString(hopen[g.cell[y][x].down])
		}
		buf.WriteString("+\n")
	}
	ly := len(g.cell) - 1
	for x := range g.cell[ly] {
		buf.WriteByte(' ')
		buf.WriteString(full[g.cell[ly][x].down && g.cell[ly][x].full])
	}
	return buf.String()
}

func (g *grid) Percolate() bool {
	for x := range g.cell[0] {
		if g.fill(x, 0) {
			return true
		}
	}
	return false
}

func (g *grid) fill(x, y int) bool {
	if y >= len(g.cell) {
		return true // Out the bottom
	}
	if g.cell[y][x].full {
		return false // Allready filled
	}
	g.cell[y][x].full = true

	if g.cell[y][x].down && g.fill(x, y+1) {
		return true
	}
	if g.cell[y][x].right && g.fill(x+1, y) {
		return true
	}
	if x > 0 && g.cell[y][x-1].right && g.fill(x-1, y) {
		return true
	}
	if y > 0 && g.cell[y-1][x].down && g.fill(x, y-1) {
		return true
	}
	return false
}
```

```txt

+--+--+--+--+--+--+--+--+--+--+
|** ** **|  |  |     |  |  |  |
+  +--+  +--+--+  +--+--+--+  +
|**|  |** **|  |     |     |  |
+--+  +--+  +  +  +--+  +  +--+
|     |   **|  |              |
+--+  +--+  +--+--+--+--+--+--+
|     |   ** **|        |     |
+--+  +  +--+  +  +--+  +--+  +
|           |** ** **|     |  |
+  +  +--+  +--+  +  +--+  +--+
|  |  |  |   ** ** ** **|  |  |
+  +--+--+  +  +--+--+  +--+--+
|  |** ** **|**|**|  |** ** **|
+  +  +  +  +  +  +--+  +--+  +
|** **|**|** ** **|  |** ** **|
+  +  +--+--+--+--+  +  +--+  +
|**|** ** **|     |  |**|  |**|
+  +--+--+--+  +  +--+--+--+--+
|**               |  |  |  |  |
+  +  +  +  +--+--+  +--+--+  +
 **
p=0.10, 1.000
p=0.20, 1.000
p=0.30, 0.998
p=0.40, 0.915
p=0.50, 0.502
p=0.60, 0.081
p=0.70, 0.002
p=0.80, 0.000
p=0.90, 0.000

```



## Haskell


```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Random
import Data.Array.Unboxed
import Data.List
import Formatting

data Field = Field { f :: UArray (Int, Int) Char
                   , hWall :: UArray (Int, Int) Bool
                   , vWall :: UArray (Int, Int) Bool
                   }

-- Start percolating some seepage through a field.
-- Recurse to continue percolation with new seepage.
percolateR :: [(Int, Int)] -> Field -> (Field, [(Int,Int)])
percolateR [] (Field f h v) = (Field f h v, [])
percolateR seep (Field f h v) =
    let ((xLo,yLo),(xHi,yHi)) = bounds f
        validSeep = filter (\p@(x,y) ->    x >= xLo
                                        && x <= xHi
                                        && y >= yLo
                                        && y <= yHi
                                        && f!p == ' ') $ nub $ sort seep

        north (x,y) = if v ! (x  ,y  ) then [] else [(x  ,y-1)]
        south (x,y) = if v ! (x  ,y+1) then [] else [(x  ,y+1)]
        west  (x,y) = if h ! (x  ,y  ) then [] else [(x-1,y  )]
        east  (x,y) = if h ! (x+1,y  ) then [] else [(x+1,y  )]
        neighbors (x,y) = north(x,y) ++ south(x,y) ++ west(x,y) ++ east(x,y)

    in  percolateR
            (concatMap neighbors validSeep)
            (Field (f // map (\p -> (p,'.')) validSeep) h v)

-- Percolate a field;  Return the percolated field.
percolate :: Field -> Field
percolate start@(Field f _ _) =
    let ((_,_),(xHi,_)) = bounds f
        (final, _) = percolateR [(x,0) | x <- [0..xHi]] start
    in final

-- Generate a random field.
initField :: Int -> Int -> Double -> Rand StdGen Field
initField width height threshold = do
    let f = listArray ((0,0), (width-1, height-1)) $ repeat ' '

    hrnd <- fmap (<threshold) <$> getRandoms
    let h0 = listArray ((0,0),(width, height-1)) hrnd
        h1 = h0 // [((0,y), True) | y <- [0..height-1]]     -- close left
        h2 = h1 // [((width,y), True) | y <- [0..height-1]] -- close right

    vrnd <- fmap (<threshold) <$> getRandoms
    let v0 = listArray ((0,0),(width-1, height)) vrnd
        v1 = v0 // [((x,0), True) | x <- [0..width-1]]  -- close top

    return $ Field f h2 v1

-- Assess whether or not percolation reached bottom of field.
leaks :: Field -> [Bool]
leaks (Field f _ v) =
    let ((xLo,_),(xHi,yHi)) = bounds f
    in [f!(x,yHi)=='.' && not (v!(x,yHi+1)) | x <- [xLo..xHi]]

-- Run test once; Return bool indicating success or failure.
oneTest :: Int -> Int -> Double -> Rand StdGen Bool
oneTest width height threshold =
    or.leaks.percolate <$> initField width height threshold

-- Run test multple times; Return the number of tests that pass.
multiTest :: Int -> Int -> Int -> Double -> Rand StdGen Double
multiTest testCount width height threshold = do
    results <- replicateM testCount $ oneTest width height threshold
    let leakyCount = length $ filter id results
    return $ fromIntegral leakyCount / fromIntegral testCount

-- Helper function for display
alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate (a:as) bs = a : alternate bs as

-- Display a field with walls and leaks.
showField :: Field -> IO ()
showField field@(Field a h v) =  do
    let ((xLo,yLo),(xHi,yHi)) = bounds a
        fLines =  [ [ a!(x,y) | x <- [xLo..xHi]] | y <- [yLo..yHi]]
        hLines =  [ [ if h!(x,y) then '|' else ' ' | x <- [xLo..xHi+1]] | y <- [yLo..yHi]]
        vLines =  [ [ if v!(x,y) then '-' else ' ' | x <- [xLo..xHi]] | y <- [yLo..yHi+1]]
        lattice =  [ [ '+' | x <- [xLo..xHi+1]] | y <- [yLo..yHi+1]]

        hDrawn = zipWith alternate hLines fLines
        vDrawn = zipWith alternate lattice vLines
    mapM_ putStrLn $ alternate vDrawn hDrawn

    let leakLine = [ if l then '.' else ' ' | l <- leaks field]
    putStrLn $ alternate (repeat ' ') leakLine

main :: IO ()
main = do
  g <- getStdGen
  let threshold = 0.45
      (startField, g2) = runRand (initField 10 10 threshold) g

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
  putStrLn ("Results of running percolation test " ++ show testCount ++ " times with thresholds ranging from 0/" ++ show densityCount ++ " to " ++ show densityCount ++ "/" ++ show densityCount ++ " .")
  let densities = [0..densityCount]
  let tests = sequence [multiTest testCount 10 10 v
                           | density <- densities,
                             let v = fromIntegral density / fromIntegral densityCount ]
  let results = zip densities (evalRand tests g2)
  mapM_ print [format ("p=" % int % "/" % int % " -> " % fixed 4) density densityCount x | (density,x) <- results]
```


```txt

Unpercolated field with 0.45 threshold.

+-+-+-+-+-+-+-+-+-+-+
|       | | | | | | |
+-+-+ +-+ + + + + +-+
| | |       | | | | |
+ + +-+-+ + +-+-+ + +
| |         |       |
+ +-+-+-+ +-+-+ +-+ +
| |     | |       | |
+ +-+ + + +-+-+ + +-+
| |     |       | | |
+-+-+ + + + + +-+ + +
| | |   | | | |     |
+-+ + + + + + + +-+-+
|         |   |   | |
+ + + + + +-+ +-+ + +
| | | | |     | | | |
+ + + +-+-+-+-+-+ + +
| |       |     |   |
+ +-+ +-+ +-+ + + +-+
| | | |           | |
+ + + + +-+ +-+-+-+ +


Same field after percolation.

+-+-+-+-+-+-+-+-+-+-+
|. . . .|.|.|.|.|.|.|
+-+-+ +-+ + + + + +-+
| |.|. . . .|.|.|.|.|
+ + +-+-+ + +-+-+ + +
| |. . . . .|. . . .|
+ +-+-+-+ +-+-+ +-+ +
| |. . .|.|. . . .|.|
+ +-+ + + +-+-+ + +-+
| |. . .|. . . .|.|.|
+-+-+ + + + + +-+ + +
| |.|. .|.|.|.|. . .|
+-+ + + + + + + +-+-+
|. . . . .|. .|. .|.|
+ + + + + +-+ +-+ + +
|.|.|.|.|. . .| |.|.|
+ + + +-+-+-+-+-+ + +
|.|. . . .|. . .|. .|
+ +-+ +-+ +-+ + + +-+
|.| |.|. . . . . .| |
+ + + + +-+ +-+-+-+ +
 .   . .   .

Results of running percolation test 10000 times with thresholds ranging from 0/10 to 10/10 .
"p=0/10 -> 1.0000"
"p=1/10 -> 1.0000"
"p=2/10 -> 1.0000"
"p=3/10 -> 0.9969"
"p=4/10 -> 0.9171"
"p=5/10 -> 0.5026"
"p=6/10 -> 0.0901"
"p=7/10 -> 0.0025"
"p=8/10 -> 0.0000"
"p=9/10 -> 0.0000"
"p=10/10 -> 0.0000"

```



## Julia

```julia
using Distributions

struct Grid
    cells::BitArray{2}
    hwall::BitArray{2}
    vwall::BitArray{2}
end
function Grid(p::AbstractFloat, m::Integer=10, n::Integer=10)
    cells = fill(false, m, n)
    hwall = rand(Bernoulli(p), m + 1, n)
    vwall = rand(Bernoulli(p), m, n + 1)
    vwall[:, 1] = true
    vwall[:, end] = true
    return Grid(cells, hwall, vwall)
end

function Base.show(io::IO, g::Grid)
    H = (" .", " _")
    V = (":", "|")
    C = (" ", "#")
    ind = findfirst(g.cells[end, :] .& .!g.hwall[end, :])
    percolated = !iszero(ind)
    println(io, "$(size(g.cells, 1))×$(size(g.cells, 2)) $(percolated ? "Percolated" : "Not percolated") grid")
    for r in 1:size(g.cells, 1)
        println(io, "    ", join(H[w+1] for w in g.hwall[r, :]))
        println(io, " $(r % 10)) ", join(V[w+1] * C[c+1] for (w, c) in zip(g.vwall[r, :], g.cells[r, :])))
    end
    println(io, "    ", join(H[w+1] for w in g.hwall[end, :]))
    if percolated
        println(io, " !)  ", "  " ^ (ind - 1), '#')
    end
end

function floodfill!(m::Integer, n::Integer, cells::AbstractMatrix{<:Integer},
                    hwall::AbstractMatrix{<:Integer}, vwall::AbstractMatrix{<:Integer})
    # fill cells
    cells[m, n] = true
    percolated = false
    # bottom
    if m < size(cells, 1) && !hwall[m+1, n] && !cells[m+1, n]
        percolated = percolated || floodfill!(m + 1, n, cells, hwall, vwall)
    # The Bottom
    elseif m == size(cells, 1) && !hwall[m+1, n]
        return true
    end
    # left
    if n > 1 && !vwall[m, n] && !cells[m, n-1]
        percolated = percolated || floodfill!(m, n - 1, cells, hwall, vwall)
    end
    # right
    if n < size(cells, 2) && !vwall[m, n+1] && !cells[m, n+1]
        percolated = percolated || floodfill!(m, n + 1, cells, hwall, vwall)
    end
    # top
    if m > 1 && !hwall[m, n] && !cells[m-1, n]
        percolated = percolated || floodfill!(m - 1, n, cells, hwall, vwall)
    end
    return percolated
end
function pourontop!(g::Grid)
    m, n = 1, 1
    percolated = false
    while !percolated && n ≤ size(g.cells, 2)
        percolated = !g.hwall[m, n] && floodfill!(m, n, g.cells, g.hwall, g.vwall)
        n += 1
    end
    return percolated
end

function main(probs, nrep::Integer=1000)
    sampleprinted = false
    pcount = zeros(Int, size(probs))
    for (i, p) in enumerate(probs), _ in 1:nrep
        g = Grid(p)
        percolated = pourontop!(g)
        if percolated
            pcount[i] += 1
            if !sampleprinted
                println(g)
                sampleprinted = true
            end
        end
    end
    return pcount ./ nrep
end

probs = collect(10:-1:0) ./ 10
percprobs = main(probs)

println("Fraction of 1000 tries that percolate through:")
for (pr, pp) in zip(probs, percprobs)
    @printf("\tp = %.3f ⇒ freq. = %5.3f\n", pr, pp)
end
```


```txt
10×10 Percolated grid
     _ . . _ _ _ . _ . .
 1) | |#:#| | : | : | :
     _ _ . _ _ _ _ _ . _
 2) | | |#| : : | : | |
     _ _ . _ _ _ _ _ _ .
 3) | | |#:#| : | : | :
     . _ _ . _ _ _ . _ _
 4) | | | :#: : | | | |
     . _ _ . _ _ _ . _ _
 5) | | : |#| | : | | :
     _ . _ . _ _ . . . _
 6) | | | |#| | | | | |
     . . _ . _ _ _ . . .
 7) | |#:#:#: | | : | |
     _ . . _ . . . . _ _
 8) | |#|#| | | : | | |
     _ . . _ _ _ . _ _ _
 9) |#:#|#| : : | : | |
     . _ _ _ _ . _ _ _ .
 0) |#: | : | | | : : |
     . . _ _ _ _ . _ _ _
 !)  #

Fraction of 1000 tries that percolate through:
    p = 1.000 ⇒ freq. = 0.000
    p = 0.900 ⇒ freq. = 0.000
    p = 0.800 ⇒ freq. = 0.000
    p = 0.700 ⇒ freq. = 0.001
    p = 0.600 ⇒ freq. = 0.064
    p = 0.500 ⇒ freq. = 0.470
    p = 0.400 ⇒ freq. = 0.895
    p = 0.300 ⇒ freq. = 0.997
    p = 0.200 ⇒ freq. = 1.000
    p = 0.100 ⇒ freq. = 1.000
    p = 0.000 ⇒ freq. = 1.000
```



## Kotlin

```scala
// version 1.2.10

import java.util.Random

val rand = Random()
const val RAND_MAX = 32767

// cell states
const val FILL  = 1
const val RWALL = 2  // right wall
const val BWALL = 4  // bottom wall

val x = 10
val y = 10
var grid = IntArray(x * (y + 2))
var cells = 0
var end = 0
var m = 0
var n = 0

fun makeGrid(p: Double) {
    val thresh = (p * RAND_MAX).toInt()
    m = x
    n = y
    grid.fill(0)  // clears grid
    for (i in 0 until m) grid[i] = BWALL or RWALL
    cells = m
    end = m
    for (i in 0 until y) {
        for (j in x - 1 downTo 1) {
            val r1 = rand.nextInt(RAND_MAX + 1)
            val r2 = rand.nextInt(RAND_MAX + 1)
            grid[end++] = (if (r1 < thresh) BWALL else 0) or
                          (if (r2 < thresh) RWALL else 0)
        }
        val r3 = rand.nextInt(RAND_MAX + 1)
        grid[end++] = RWALL or (if (r3 < thresh) BWALL else 0)
    }
}

fun showGrid() {
    for (j in 0 until m) print("+--")
    println("+")

    for (i in 0..n) {
        print(if (i == n) " " else "|")
        for (j in 0 until m) {
            print(if ((grid[i * m + j + cells] and FILL) != 0) "[]" else "  ")
            print(if ((grid[i * m + j + cells] and RWALL) != 0) "|" else " ")
        }
        println()
        if (i == n) return
        for (j in 0 until m) {
            print(if ((grid[i * m + j + cells] and BWALL) != 0) "+--" else "+  ")
        }
        println("+")
    }
}

fun fill(p: Int): Boolean {
    if ((grid[p] and FILL) != 0) return false
    grid[p] = grid[p] or FILL
    if (p >= end) return true  // success: reached bottom row
    return (((grid[p + 0] and BWALL) == 0) && fill(p + m)) ||
           (((grid[p + 0] and RWALL) == 0) && fill(p + 1)) ||
           (((grid[p - 1] and RWALL) == 0) && fill(p - 1)) ||
           (((grid[p - m] and BWALL) == 0) && fill(p - m))
}

fun percolate(): Boolean {
    var i = 0
    while (i < m && !fill(cells + i)) i++
    return i < m
}

fun main(args: Array<String>) {
    makeGrid(0.5)
    percolate()
    showGrid()

    println("\nrunning $x x $y grids 10,000 times for each p:")
    for (p in 1..9) {
        var cnt = 0
        val pp = p / 10.0
        for (i in 0 until 10_000) {
            makeGrid(pp)
            if (percolate()) cnt++
        }
        println("p = %3g: %.4f".format(pp, cnt.toDouble() / 10_000))
    }
}
```


Sample output:

```txt

+--+--+--+--+--+--+--+--+--+--+
|[]|[] [] [] [] []|  |  |  |  |
+--+--+--+--+--+  +--+  +  +  +
|     |  |  |   []|           |
+--+--+--+--+--+  +  +--+  +  +
|  |  |  |  |[] []|           |
+  +  +  +  +  +--+--+--+--+--+
|  |  |   [] [] []|     |     |
+--+--+  +  +--+--+--+--+--+  +
|  |     |[] []|  |        |  |
+--+--+  +  +  +  +--+  +--+--+
|  |  |  |[]|[]|     |     |  |
+--+  +--+--+  +--+--+  +  +  +
|  |     |   []|  |  |  |     |
+--+  +  +  +  +--+--+  +  +  +
|     |  |[] []|  |           |
+  +--+--+  +--+  +--+  +  +--+
|  |      []   |  |        |  |
+  +  +--+  +  +--+--+--+--+  +
|         []      |  |     |  |
+  +--+--+  +  +--+--+  +--+  +
          []

running 10 x 10 grids 10,000 times for each p:
p = 0.100000: 1.0000
p = 0.200000: 1.0000
p = 0.300000: 0.9968
p = 0.400000: 0.9184
p = 0.500000: 0.5047
p = 0.600000: 0.0828
p = 0.700000: 0.0034
p = 0.800000: 0.0000
p = 0.900000: 0.0000

```



## Perl

```perl
my @bond;
my $grid = 10;
my $water = '▒';
$D{$_} = $i++ for qw<DeadEnd Up Right Down Left>;

sub percolate {
    generate(shift || 0.6);
    fill(my $x = 1,my $y = 0);
    my @stack;

    while () {
        if (my $dir = direction($x,$y)) {
            push @stack, [$x,$y];
            ($x,$y) = move($dir, $x, $y)
        } else {
            return 0 unless @stack;
            ($x,$y) = @{pop @stack}
        }
        return 1 if $y == $#bond;
    }
}

sub direction {
    my($x, $y) = @_;
    return $D{Down}  if $bond[$y+1][$x  ] =~ / /;
    return $D{Left}  if $bond[$y  ][$x-1] =~ / /;
    return $D{Right} if $bond[$y  ][$x+1] =~ / /;
    return $D{Up}    if defined $bond[$y-1][$x  ] && $bond[$y-1][$x] =~ / /;
    return $D{DeadEnd}
}

sub move {
    my($dir,$x,$y) = @_;
    fill(  $x,--$y), fill(  $x,--$y) if $dir == $D{Up};
    fill(  $x,++$y), fill(  $x,++$y) if $dir == $D{Down};
    fill(--$x,  $y), fill(--$x,  $y) if $dir == $D{Left};
    fill(++$x,  $y), fill(++$x,  $y) if $dir == $D{Right};
    $x, $y
}

sub fill {
    my($x, $y) = @_;
    $bond[$y][$x] =~ s/ /$water/g
}

sub generate {
    our($prob) = shift || 0.5;
    @bond = ();
    our $sp = '   ';
    push @bond, ['│', ($sp, ' ') x ($grid-1), $sp, '│'],
                ['├', hx('┬'), h(), '┤'];
    push @bond, ['│', vx(   ), $sp, '│'],
                ['├', hx('┼'), h(), '┤'] for 1..$grid-1;
    push @bond, ['│', vx(   ), $sp, '│'],
                ['├', hx('┴'), h(), '┤'],
                ['│', ($sp, ' ') x ($grid-1), $sp, '│'];

    sub hx { my($c)=@_; my @l; push @l, (h(),$c) for 1..$grid-1; return @l; }
    sub vx {            my @l; push @l, $sp, v() for 1..$grid-1; return @l; }
    sub h { rand() < $prob ? $sp : '───' }
    sub v { rand() < $prob ? ' ' : '│'   }
}

print "Sample percolation at .6\n";
percolate(.6);
for my $row (@bond) {
    my $line = '';
    $line .= join '', $_ for @$row;
    print "$line\n";
}

my $tests = 100;
print "Doing $tests trials at each porosity:\n";
my @table;
for my $p (1 .. 10) {
    $p = $p/10;
    my $total = 0;
    $total += percolate($p) for 1..$tests;
    printf "p = %0.1f: %0.2f\n", $p, $total / $tests
}
```

```txt
Sample percolation at .6
│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒                          │
├───┬───┬───┬▒▒▒┬   ┬   ┬   ┬───┬   ┬───┤
│        ▒▒▒▒▒▒▒             │       │   │
├───┼───┼▒▒▒┼───┼───┼   ┼───┼───┼   ┼   ┤
│   │▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│         │   │   │
├───┼───┼───┼───┼▒▒▒┼▒▒▒┼   ┼───┼───┼───┤
│       │       │▒▒▒│▒▒▒▒▒▒▒             │
├───┼───┼   ┼───┼───┼───┼▒▒▒┼   ┼   ┼   ┤
│   │                ▒▒▒▒▒▒▒     │       │
├───┼───┼───┼   ┼   ┼▒▒▒┼───┼   ┼   ┼───┤
│       │           │▒▒▒            │   │
├───┼   ┼───┼   ┼───┼▒▒▒┼   ┼   ┼───┼   ┤
│   │            ▒▒▒▒▒▒▒│        │       │
├   ┼   ┼   ┼   ┼▒▒▒┼───┼   ┼───┼───┼   ┤
│       │        ▒▒▒│   │               │
├   ┼   ┼   ┼   ┼▒▒▒┼───┼   ┼   ┼───┼   ┤
│           │    ▒▒▒│               │   │
├───┼   ┼───┼   ┼▒▒▒┼   ┼   ┼   ┼   ┼   ┤
│   │           │▒▒▒        │   │   │   │
├───┼───┼   ┼   ┼▒▒▒┼───┼   ┼   ┼   ┼───┤
│           │    ▒▒▒│       │   │       │
├   ┴───┴   ┴   ┴▒▒▒┴   ┴───┴   ┴   ┴───┤
│                ▒▒▒                    │
Doing 100 trials at each porosity:
p = 0.1: 0.00
p = 0.2: 0.00
p = 0.3: 0.00
p = 0.4: 0.03
p = 0.5: 0.38
p = 0.6: 0.83
p = 0.7: 0.99
p = 0.8: 1.00
p = 0.9: 1.00
p = 1.0: 1.00
```



## Perl 6

Starts "filling" from the top left. Fluid flow favours directions in Down, Left, Right, Up order. I interpreted p to be porosity, so small p mean low permeability, large p means high permeability.


```perl6
my @bond;
my $grid = 10;
my $geom = $grid - 1;
my $water = '▒';

enum Direction <DeadEnd Up Right Down Left>;

say 'Sample percolation at .6';
percolate .6;
.join.say for @bond;
say "\n";

my $tests = 100;
say "Doing $tests trials at each porosity:";
for .1, .2 ... 1 -> $p {
    printf "p = %0.1f: %0.2f\n", $p, (sum percolate($p) xx $tests) / $tests
}

sub percolate ( $prob ) {
    generate $prob;
    my @stack;
    my $current = [1;0];
    $current.&fill;

    loop {
        if my $dir = direction( $current ) {
            @stack.push: $current;
            $current = move $dir, $current
        }
        else {
            return 0 unless @stack;
            $current = @stack.pop
        }
        return 1 if $current[1] == +@bond - 1
    }

    sub direction( [$x, $y] ) {
        ( Down  if @bond[$y + 1][$x].contains: ' ' ) ||
        ( Left  if @bond[$y][$x - 1].contains: ' ' ) ||
        ( Right if @bond[$y][$x + 1].contains: ' ' ) ||
        ( Up    if @bond[$y - 1][$x].defined && @bond[$y - 1][$x].contains: ' ' ) ||
        DeadEnd
    }

    sub move ( $dir, @cur ) {
        my ( $x, $y ) = @cur;
        given $dir {
            when Up    { [$x,--$y].&fill xx 2 }
            when Down  { [$x,++$y].&fill xx 2 }
            when Left  { [--$x,$y].&fill xx 2 }
            when Right { [++$x,$y].&fill xx 2 }
        }
        [$x, $y]
    }

    sub fill ( [$x, $y] ) { @bond[$y;$x].=subst(' ', $water, :g) }
}

sub generate ( $prob = .5 ) {
    @bond = ();
    my $sp = '   ';
    append @bond, [flat '│', ($sp, ' ') xx $geom, $sp, '│'],
                  [flat '├', (h(), '┬') xx $geom, h(), '┤'];
    append @bond, [flat '│', ($sp, v()) xx $geom, $sp, '│'],
                  [flat '├', (h(), '┼') xx $geom, h(), '┤'] for ^$geom;
    append @bond, [flat '│', ($sp, v()) xx $geom, $sp, '│'],
                  [flat '├', (h(), '┴') xx $geom, h(), '┤'],
                  [flat '│', ($sp, ' ') xx $geom, $sp, '│'];

    sub h () { rand < $prob ?? $sp !! '───' }
    sub v () { rand < $prob ?? ' ' !! '│'   }
}
```

```txt
Sample percolation at .6
│▒▒▒                                    │
├▒▒▒┬   ┬───┬   ┬   ┬   ┬   ┬   ┬───┬   ┤
│▒▒▒▒▒▒▒                │   │           │
├───┼▒▒▒┼   ┼   ┼   ┼   ┼   ┼───┼   ┼   ┤
│▒▒▒▒▒▒▒▒▒▒▒│   │   │   │   │   │       │
├▒▒▒┼───┼▒▒▒┼   ┼───┼   ┼───┼   ┼   ┼   ┤
│▒▒▒│▒▒▒▒▒▒▒▒▒▒▒                │   │   │
├▒▒▒┼───┼───┼▒▒▒┼   ┼   ┼───┼   ┼   ┼   ┤
│▒▒▒│        ▒▒▒│   │   │           │   │
├───┼   ┼   ┼▒▒▒┼───┼   ┼   ┼   ┼   ┼───┤
│           │▒▒▒    │                   │
├   ┼───┼   ┼▒▒▒┼───┼───┼───┼───┼   ┼───┤
│           │▒▒▒│                       │
├───┼   ┼───┼▒▒▒┼───┼───┼───┼───┼   ┼───┤
│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒        │       │       │
├▒▒▒┼▒▒▒┼───┼▒▒▒┼───┼   ┼───┼   ┼   ┼   ┤
│▒▒▒│▒▒▒▒▒▒▒│▒▒▒▒▒▒▒│                   │
├▒▒▒┼───┼───┼───┼───┼───┼   ┼   ┼   ┼   ┤
│▒▒▒▒▒▒▒    │       │   │               │
├▒▒▒┼▒▒▒┼───┼───┼   ┼───┼───┼   ┼   ┼   ┤
│▒▒▒│▒▒▒    │               │           │
├───┴▒▒▒┴   ┴   ┴   ┴───┴   ┴   ┴   ┴───┤
│    ▒▒▒                                │

Doing 100 trials at each porosity:
p = 0.1: 0.00
p = 0.2: 0.00
p = 0.3: 0.00
p = 0.4: 0.05
p = 0.5: 0.42
p = 0.6: 0.92
p = 0.7: 1.00
p = 0.8: 1.00
p = 0.9: 1.00
p = 1.0: 1.00
```



## Phix


```Phix
constant w = 10, h = 10

sequence wall = join(repeat("+",w+1),"---")&"\n",
         cell = join(repeat("|",w+1),"   ")&"\n",
         grid

procedure new_grid(atom p)
    grid = split(join(repeat(wall,h+1),cell),'\n')
    -- now knock down some walls
    for i=1 to length(grid)-1 do
        integer jstart = 5-mod(i,2)*3,
                jlimit = length(grid[i])-3
        -- (ie 2..38 on odd lines, 5..37 on even)
        for j=jstart to jlimit by 4 do
            if rnd()>p then
                grid[i][j..j+2] = "   "
            end if
        end for
    end for
end procedure

function percolate(integer x=0, y=0)
    if x=0 then
        for j=3 to length(grid[1])-2 by 4 do
            if grid[1][j]=' ' and percolate(1,j) then
                return true
            end if
        end for
    elsif grid[x][y]=' ' then
        grid[x][y] = '*'
        if (x=length(grid)-1)
        or (         grid[x+1][y]=' ' and percolate(x+1,y))
        or (y>6  and grid[x][y-2]=' ' and percolate(x,y-4))
        or (y<36 and grid[x][y+2]=' ' and percolate(x,y+4))
        or (x>1  and grid[x-1][y]=' ' and percolate(x-1,y)) then
            return true
        end if
    end if
    return false
end function

constant LIM=1000

for p=0 to 10 do
    integer count = 0
    for t=1 to LIM do
        new_grid(p/10)
        count += percolate()
    end for
    printf(1,"p=%.1f: %5.3f\n",{p/10,count/LIM})
end for
puts(1,"sample grid for p=0.6:\n")
new_grid(0.6)
{} = percolate()
puts(1,join(grid,'\n'))
```

```txt

p=0.0: 1.000
p=0.1: 1.000
p=0.2: 1.000
p=0.3: 0.997
p=0.4: 0.897
p=0.5: 0.434
p=0.6: 0.067
p=0.7: 0.003
p=0.8: 0.000
p=0.9: 0.000
p=1.0: 0.000
sample grid for p=0.6:
+---+---+ * +---+ * + * +---+---+ * + * +
| *   *   * |   | *   * | * |   | *   * |
+---+---+ * +   +---+ * + * +---+---+ * +
| *   * | * |   |   | * | * | *   *   * |
+---+ * + * +---+---+---+ * +---+ * +---+
|   | * | * | * | * | *   *   *   *   * |
+   + * + * + * + * + * +---+ * + * +---+
|   | *   *   *   * | *   * | *   *   * |
+   + * + * +---+ * +---+ * +---+ * + * +
|   | * | *   *   * | *   * | *   * | * |
+---+ * +---+ * +---+---+---+---+ * + * +
|   | * |   | *   *   *   * | *   * | * |
+   +---+   +---+ * +---+---+---+---+ * +
|   |   |   |   | * |           |   | * |
+   +---+---+   +---+---+---+---+---+---+
|   |   |       |   |   |   |   |   |   |
+---+---+---+   +   +---+   +   +---+---+
|   |   |   |   |   |   |       |       |
+   +---+   +---+---+   +---+---+---+---+
|   |   |       |   |   |   |   |   |   |
+---+---+   +   +   +---+---+---+---+   +

```



## Python


```python
from collections import namedtuple
from random import random
from pprint import pprint as pp

Grid = namedtuple('Grid', 'cell, hwall, vwall')

M, N, t = 10, 10, 100

class PercolatedException(Exception): pass

HVF = [(' .', ' _'), (':', '|'), (' ', '#')]  # Horiz, vert, fill chars

def newgrid(p):
    hwall = [[int(random() < p) for m in range(M)]
             for n in range(N+1)]
    vwall = [[(1 if m in (0, M) else int(random() < p)) for m in range(M+1)]
             for n in range(N)]
    cell = [[0 for m in range(M)]
             for n in range(N)]
    return Grid(cell, hwall, vwall)

def pgrid(grid, percolated=None):
    cell, hwall, vwall = grid
    h, v, f = HVF
    for n in range(N):
        print('    ' + ''.join(h[hwall[n][m]] for m in range(M)))
        print('%i)  ' % (n % 10) + ''.join(v[vwall[n][m]] + f[cell[n][m] if m < M else 0]
                                          for m in range(M+1))[:-1])
    n = N
    print('    ' + ''.join(h[hwall[n][m]] for m in range(M)))
    if percolated:
        where = percolated.args[0][0]
        print('!)  ' + '  ' * where + ' ' + f[1])

def pour_on_top(grid):
    cell, hwall, vwall = grid
    n = 0
    try:
        for m in range(M):
            if not hwall[n][m]:
                flood_fill(m, n, cell, hwall, vwall)
    except PercolatedException as ex:
        return ex
    return None


def flood_fill(m, n, cell, hwall, vwall):
    # fill cell
    cell[n][m] = 1
    # bottom
    if n < N - 1 and not hwall[n + 1][m] and not cell[n+1][m]:
        flood_fill(m, n+1, cell, hwall, vwall)
    # THE bottom
    elif n == N - 1 and not hwall[n + 1][m]:
        raise PercolatedException((m, n+1))
    # left
    if m and not vwall[n][m] and not cell[n][m - 1]:
        flood_fill(m-1, n, cell, hwall, vwall)
    # right
    if m < M - 1 and not vwall[n][m + 1] and not cell[n][m + 1]:
        flood_fill(m+1, n, cell, hwall, vwall)
    # top
    if n and not hwall[n][m] and not cell[n-1][m]:
        flood_fill(m, n-1, cell, hwall, vwall)

if __name__ == '__main__':
    sample_printed = False
    pcount = {}
    for p10 in range(11):
        p = (10 - p10) / 10.0    # count down so sample print is interesting
        pcount[p] = 0
        for tries in range(t):
            grid = newgrid(p)
            percolated = pour_on_top(grid)
            if percolated:
                pcount[p] += 1
                if not sample_printed:
                    print('\nSample percolating %i x %i grid' % (M, N))
                    pgrid(grid, percolated)
                    sample_printed = True
    print('\n p: Fraction of %i tries that percolate through' % t )

    pp({p:c/float(t) for p, c in pcount.items()})
```


In the Ascii art, cells are either a space or a hash and are surrounded by either '_', '|' for intact walls and '.' and ':' for missing (leaky) walls.

The bottom-most line starting '!)' shows where the fluid can drip out from. (The percolation stops when one route through the bottom is found).


```txt
Sample percolating 10 x 10 grid
     _ _ . _ . _ _ . _ _
0)  | |#:#:#|#| | :#| | |
     _ _ . _ _ _ . . _ _
1)  | | |#:#| | | |#| : |
     _ _ _ . _ . . . . _
2)  | | |#:#| : | |#: | |
     _ _ _ _ . . _ . . .
3)  | : : | | | : |#: | |
     _ _ . _ . . _ . _ _
4)  | : : : | | | |#: : |
     _ _ _ . _ _ _ . . _
5)  | : | | : | | :#| | |
     _ _ . . _ _ _ . _ .
6)  | : | | : | |#:#:#| |
     _ . _ _ . _ _ _ . .
7)  | : | : | : | | |#: |
     _ _ _ . . _ _ . . _
8)  | | : | | | |#:#:#: |
     _ _ _ . . . . _ _ .
9)  | : : | : : :#: | : |
     . _ . _ . . . . _ _
!)               #

 p: Fraction of 100 tries that percolate through
{0.0: 1.0,
 0.1: 1.0,
 0.2: 1.0,
 0.3: 1.0,
 0.4: 0.9,
 0.5: 0.47,
 0.6: 0.06,
 0.7: 0.0,
 0.8: 0.0,
 0.9: 0.0,
 1.0: 0.0}
```


Note the abrupt cut-off in percolation at around p = 0.5 which is to be [http://mathworld.wolfram.com/PercolationThreshold.html expected].


## Racket



```racket
#lang racket

(define has-left-wall?   (lambda (x) (bitwise-bit-set? x 0)))
(define has-right-wall?  (lambda (x) (bitwise-bit-set? x 1)))
(define has-top-wall?    (lambda (x) (bitwise-bit-set? x 2)))
(define has-bottom-wall? (lambda (x) (bitwise-bit-set? x 3)))
(define has-fluid?       (lambda (x) (bitwise-bit-set? x 4)))

(define (walls->cell l? r? t? b?)
  (+ (if l? 1 0) (if r? 2 0) (if t? 4 0) (if b? 8 0)))

(define (bonded-percol-grid M N p)
  (define rv (make-vector (* M N)))
  (for* ((idx (in-range (* M N))))
    (define left-wall?
      (or (zero? (modulo idx M))
          (has-right-wall? (vector-ref rv (sub1 idx)))))
    (define right-wall?
      (or (= (modulo idx M) (sub1 M))
          (< (random) p)))
    (define top-wall?
      (if (< idx M) (< (random) p)
          (has-bottom-wall? (vector-ref rv (- idx M)))))
    (define bottom-wall? (< (random) p))
    (define cell-value
      (walls->cell left-wall? right-wall? top-wall? bottom-wall?))
    (vector-set! rv idx cell-value))
  rv)

(define (display-percol-grid M . vs)
  (define N (/ (vector-length (car vs)) M))
  (define-syntax-rule (tab-eol m)
    (when (= m (sub1 M)) (printf "\t")))
  (for ((n N))
    (for* ((v vs) (m M))
      (when (zero? m) (printf "+"))
      (printf
       (match (vector-ref v (+ (* n M) m))
         ((? has-top-wall?) "-+")
         ((? has-fluid?)    "#+")
         (else ".+")))
      (tab-eol m))
    (newline)
    (for* ((v vs) (m M))
      (when (zero? m) (printf "|"))
      (printf
       (match (vector-ref v (+ (* n M) m))
         ((and (? has-fluid?) (? has-right-wall?)) "#|")
         ((? has-right-wall?) ".|")
         ((? has-fluid?) "##")
         (else "..")))
      (tab-eol m))
    (newline))
  (for* ((v vs) (m M))
    (when (zero? m) (printf "+"))
    (printf
     (match (vector-ref v (+ (* (sub1 M) M) m))
       ((? has-bottom-wall?) "-+")
       ((? has-fluid?)    "#+")
       (else ".+")))
    (tab-eol m))
  (newline))

(define (find-bonded-grid-t/b-path M v)
  (define N (/ (vector-length v) M))

  (define (flood-cell idx)
    (cond
      [(= (quotient idx M) N) #t] ; wootiments!
      [(has-fluid? (vector-ref v idx)) #f] ; been here
      [else (define cell (vector-ref v idx))
            (vector-set! v idx (bitwise-ior cell 16))
            (or (and (not (has-bottom-wall? cell)) (flood-cell (+ idx M)))
                (and (not (has-left-wall? cell))   (flood-cell (- idx 1)))
                (and (not (has-right-wall? cell))  (flood-cell (+ idx 1)))
                (and (not (has-top-wall? cell))
                     (>= idx M) ; not top row
                     (flood-cell (- idx M))))]))

  (for/first ((m (in-range M))
              #:unless (has-top-wall? (vector-ref v m))
              #:when (flood-cell m)) #t))

(define t (make-parameter 1000))
(define (experiment p)
  (/ (for*/sum ((sample (in-range (t)))
                (v (in-value (bonded-percol-grid 10 10 p)))
                #:when (find-bonded-grid-t/b-path 10 v)) 1)
     (t)))

(define (main)
  (for ((tenths (in-range 0 (add1 10))))
    (define p (/ tenths 10))
    (define e (experiment p))
    (printf "proportion of grids that percolate p=~a : ~a (~a)~%"
            p e (real->decimal-string e 5))))

(module+ test
  (define (make/display/flood/display-bonded-grid M N p attempts (atmpt 1))
    (define v (bonded-percol-grid M N p))
    (define v+ (vector-copy v))
    (cond [(or (find-bonded-grid-t/b-path M v+) (= attempts 0))
           (define v* (vector-copy v+))
           (define (flood-bonded-grid)
             (when (find-bonded-grid-t/b-path M v*)
               (flood-bonded-grid)))
           (flood-bonded-grid)
           (display-percol-grid M v v+ v*)
           (printf "After ~a attempt(s)~%~%" atmpt)]
          [else
           (make/display/flood/display-bonded-grid
            M N p (sub1 attempts) (add1 atmpt))]))

  (make/display/flood/display-bonded-grid 10 10 0   20)
  (make/display/flood/display-bonded-grid 10 10 .25 20)
  (make/display/flood/display-bonded-grid 10 10 .50 20)
  (make/display/flood/display-bonded-grid 10 10 .75 20000))
```


```txt
Welcome to DrRacket, version 5.3.5 [3m].
Language: racket [custom]; memory limit: 1024 MB.
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
|...................|	|##.................|	|###################|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+#+#+#+#+#+#+#+
After 1 attempt(s)

+.+-+-+.+.+.+-+.+.+.+	+#+-+-+.+.+.+-+.+.+.+	+#+-+-+#+#+#+-+#+#+#+
|...................|	|##.................|	|##..###############|
+.+-+.+-+.+.+-+-+-+.+	+#+-+.+-+.+.+-+-+-+.+	+#+-+#+-+#+#+-+-+-+#+
|.................|.|	|##...............|.|	|##..##..####.....|#|
+.+-+.+.+.+.+-+.+.+.+	+#+-+.+.+.+.+-+.+.+.+	+#+-+#+.+#+#+-+.+.+#+
|.............|.....|	|##...........|.....|	|######..#####|....#|
+.+.+.+.+.+.+.+.+.+.+	+#+.+.+.+.+.+.+.+.+.+	+#+#+#+.+#+#+#+.+.+#+
|.....|...|.|.......|	|##...|...|.|.......|	|#####|..#|#|##....#|
+.+.+.+.+.+.+.+-+-+.+	+#+.+.+.+.+.+.+-+-+.+	+#+#+#+#+#+#+#+-+-+#+
|.|.............|...|	|#|.............|...|	|#|############.|..#|
+.+-+-+.+-+.+.+.+.+.+	+#+-+-+.+-+.+.+.+.+.+	+#+-+-+#+-+#+#+.+.+#+
|...................|	|##.................|	|##....##..####....#|
+.+.+-+.+.+.+.+-+-+.+	+#+.+-+.+.+.+.+-+-+.+	+#+.+-+#+.+#+#+-+-+#+
|...|...|...........|	|##.|...|...........|	|##.|###|..####..###|
+.+.+.+-+.+.+.+.+.+.+	+#+#+.+-+.+.+.+.+.+.+	+#+#+#+-+.+#+#+.+#+#+
|...|...|.........|.|	|###|...|.........|.|	|###|##.|..####..#|#|
+-+.+.+-+-+.+.+.+.+-+	+-+#+.+-+-+.+.+.+.+-+	+-+#+#+-+-+#+#+.+#+-+
|.....|.........|...|	|..##.|.........|...|	|..###|....####.|###|
+.+.+.+.+.+.+.+.+.+.+	+.+#+.+.+.+.+.+.+.+.+	+.+#+#+.+.+#+#+#+#+#+
|.........|.......|.|	|..##.....|.......|.|	|..####...|#######|#|
+.+.+.+-+.+.+-+.+-+.+	+.+#+.+-+.+.+-+.+-+.+	+.+#+#+-+.+#+-+#+-+#+
After 1 attempt(s)

+.+.+.+.+-+-+.+-+.+.+	+#+#+#+#+-+-+.+-+.+.+	+#+#+#+#+-+-+#+-+#+#+
|.........|.|.|...|.|	|########.|.|.|...|.|	|########.|.|#|###|#|
+.+-+-+.+-+-+-+.+.+-+	+#+-+-+#+-+-+-+.+.+-+	+#+-+-+#+-+-+-+#+#+-+
|...|...|...|.|.|.|.|	|###|..#|...|.|.|.|.|	|###|..#|...|.|#|#|.|
+-+-+.+.+.+.+-+.+-+.+	+-+-+.+#+#+.+-+.+-+.+	+-+-+.+#+#+.+-+#+-+.+
|.|.|.|...|.|.|.|...|	|.|.|.|###|.|.|.|...|	|.|.|.|###|.|.|#|...|
+.+-+.+-+.+.+.+-+.+-+	+.+-+.+-+#+.+.+-+.+-+	+.+-+.+-+#+.+.+-+.+-+
|.|...|...|.|.....|.|	|.|...|###|.|.....|.|	|.|...|###|.|.....|.|
+.+-+.+.+.+-+-+.+.+.+	+.+-+.+#+#+-+-+.+.+.+	+.+-+.+#+#+-+-+.+.+.+
|.|...|.|.....|.....|	|.|...|#|####.|.....|	|.|...|#|####.|.....|
+-+.+-+.+-+.+-+.+-+-+	+-+.+-+#+-+#+-+#+-+-+	+-+.+-+#+-+#+-+#+-+-+
|.|.|.....|.....|...|	|.|.|#####|#####|...|	|.|.|#####|#####|...|
+-+-+.+.+.+.+-+.+-+-+	+-+-+#+#+#+#+-+#+-+-+	+-+-+#+#+#+#+-+#+-+-+
|...|.|.....|.......|	|...|#|#####|..##...|	|...|#|#####|..##...|
+-+-+-+-+-+-+-+.+-+-+	+-+-+-+-+-+-+-+#+-+-+	+-+-+-+-+-+-+-+#+-+-+
|.|...|.|.|.......|.|	|.|...|.|.|######.|.|	|.|...|.|.|######.|.|
+.+-+-+-+.+.+-+.+.+.+	+.+-+-+-+.+#+-+#+.+.+	+.+-+-+-+.+#+-+#+.+.+
|.|...|.......|.|.|.|	|.|...|....##.|#|.|.|	|.|...|....##.|#|.|.|
+.+.+-+.+.+.+-+-+-+-+	+.+.+-+.+.+#+-+-+-+-+	+.+.+-+.+.+#+-+-+-+-+
|.|.........|.....|.|	|.|........#|.....|.|	|.|........#|.....|.|
+-+.+-+-+-+.+.+.+-+.+	+-+.+-+-+-+#+.+.+-+.+	+-+.+-+-+-+#+.+.+-+.+
After 2 attempt(s)

+-+-+-+-+-+-+.+-+-+.+	+-+-+-+-+-+-+#+-+-+.+	+-+-+-+-+-+-+#+-+-+#+
|.|.|...|.|.|.|.|...|	|.|.|...|.|.|#|.|...|	|.|.|...|.|.|#|.|###|
+-+-+-+-+-+-+.+-+-+-+	+-+-+-+-+-+-+#+-+-+-+	+-+-+-+-+-+-+#+-+-+-+
|.|.|.|...|.|...|.|.|	|.|.|.|...|.|##.|.|.|	|.|.|.|...|.|##.|.|.|
+.+.+.+.+.+-+.+-+-+.+	+.+.+.+.+.+-+#+-+-+.+	+.+.+.+.+.+-+#+-+-+.+
|.|.|.|.|...|.|...|.|	|.|.|.|.|...|#|...|.|	|.|.|.|.|...|#|...|.|
+.+-+.+-+-+-+.+-+.+.+	+.+-+.+-+-+-+#+-+.+.+	+.+-+.+-+-+-+#+-+.+.+
|...|...|.|.|...|.|.|	|...|...|.|.|###|.|.|	|...|...|.|.|###|.|.|
+-+-+-+-+-+-+-+.+-+-+	+-+-+-+-+-+-+-+#+-+-+	+-+-+-+-+-+-+-+#+-+-+
|.|.......|.....|.|.|	|.|.......|#####|.|.|	|.|.......|#####|.|.|
+.+-+-+-+.+.+-+-+-+-+	+.+-+-+-+.+#+-+-+-+-+	+.+-+-+-+.+#+-+-+-+-+
|.|.|.|.|.|.|.|.....|	|.|.|.|.|.|#|.|.....|	|.|.|.|.|.|#|.|.....|
+-+-+-+-+-+.+.+-+.+-+	+-+-+-+-+-+#+.+-+.+-+	+-+-+-+-+-+#+.+-+.+-+
|...|.|.|.|.|.|.|.|.|	|...|.|.|.|#|.|.|.|.|	|...|.|.|.|#|.|.|.|.|
+.+.+.+-+-+.+.+-+.+-+	+.+.+.+-+-+#+#+-+.+-+	+.+.+.+-+-+#+#+-+.+-+
|.|.|.|.|.|...|.|...|	|.|.|.|.|.|###|.|...|	|.|.|.|.|.|###|.|...|
+-+-+-+-+-+-+.+.+.+-+	+-+-+-+-+-+-+#+.+.+-+	+-+-+-+-+-+-+#+.+.+-+
|.|.|.|.|.|...|...|.|	|.|.|.|.|.|###|...|.|	|.|.|.|.|.|###|...|.|
+-+-+.+-+-+.+-+-+-+.+	+-+-+.+-+-+#+-+-+-+.+	+-+-+.+-+-+#+-+-+-+.+
|.|.|.|...|...|.|...|	|.|.|.|...|###|.|...|	|.|.|.|...|###|.|...|
+-+-+.+-+.+-+.+.+.+-+	+-+-+.+-+.+-+#+.+.+-+	+-+-+.+-+.+-+#+.+.+-+
After 4611 attempt(s)

> (main)
proportion of grids that percolate p=0 : 1 (1.00000)
proportion of grids that percolate p=1/10 : 1 (1.00000)
proportion of grids that percolate p=1/5 : 1 (1.00000)
proportion of grids that percolate p=3/10 : 199/200 (0.99500)
proportion of grids that percolate p=2/5 : 179/200 (0.89500)
proportion of grids that percolate p=1/2 : 451/1000 (0.45100)
proportion of grids that percolate p=3/5 : 29/500 (0.05800)
proportion of grids that percolate p=7/10 : 1/1000 (0.00100)
proportion of grids that percolate p=4/5 : 0 (0.00000)
proportion of grids that percolate p=9/10 : 0 (0.00000)
proportion of grids that percolate p=1 : 0 (0.00000)
```



## Swift


```swift
let randMax = 32767.0
let filled = 1
let rightWall = 2
let bottomWall = 4

final class Percolate {
  let height: Int
  let width: Int

  private var grid: [Int]
  private var end: Int

  init(height: Int, width: Int) {
    self.height = height
    self.width = width
    self.end = width
    self.grid = [Int](repeating: 0, count: width * (height + 2))
  }

  private func fill(at p: Int) -> Bool {
    guard grid[p] & filled == 0 else { return false }

    grid[p] |= filled

    guard p < end else { return true }

    return (((grid[p + 0] & bottomWall) == 0) && fill(at: p + width)) ||
            (((grid[p + 0] & rightWall) == 0) && fill(at: p + 1)) ||
            (((grid[p - 1] & rightWall) == 0) && fill(at: p - 1)) ||
            (((grid[p - width] & bottomWall) == 0) && fill(at: p - width))
  }

  func makeGrid(porosity p: Double) {
    grid = [Int](repeating: 0, count: width * (height + 2))
    end = width

    let thresh = Int(randMax * p)

    for i in 0..<width {
      grid[i] = bottomWall | rightWall
    }

    for _ in 0..<height {
      for _ in stride(from: width - 1, through: 1, by: -1) {
        let r1 = Int.random(in: 0..<Int(randMax)+1)
        let r2 = Int.random(in: 0..<Int(randMax)+1)

        grid[end] = (r1 < thresh ? bottomWall : 0) | (r2 < thresh ? rightWall : 0)

        end += 1
      }

      let r3 = Int.random(in: 0..<Int(randMax)+1)

      grid[end] = rightWall | (r3 < thresh ? bottomWall : 0)

      end += 1
    }
  }

  @discardableResult
  func percolate() -> Bool {
    var i = 0

    while i < width && !fill(at: width + i) {
      i += 1
    }

    return i < width
  }

  func showGrid() {
    for _ in 0..<width {
      print("+--", terminator: "")
    }

    print("+")

    for i in 0..<height {
      print(i == height ? " " : "|", terminator: "")

      for j in 0..<width {
        print(grid[i * width + j + width] & filled != 0 ? "[]" : "  ", terminator: "")
        print(grid[i * width + j + width] & rightWall != 0 ? "|" : " ", terminator: "")
      }

      print()

      guard i != height else { return }

      for j in 0..<width {
        print(grid[i * width + j + width] & bottomWall != 0 ? "+--" : "+  ", terminator: "")
      }

      print("+")
    }
  }
}

let p = Percolate(height: 10, width: 10)

p.makeGrid(porosity: 0.5)
p.percolate()
p.showGrid()

print("Running \(p.height) x \(p.width) grid 10,000 times for each porosity")

for factor in 1...10 {
  var count = 0
  let porosity = Double(factor) / 10.0

  for _ in 0..<10_000 {
    p.makeGrid(porosity: porosity)

    if p.percolate() {
      count += 1
    }
  }

  print("p = \(porosity): \(Double(count) / 10_000.0)")
}
```


```txt
+--+--+--+--+--+--+--+--+--+--+
|[]|  |     |     |  |     |  |
+  +  +--+--+  +  +--+  +  +  +
|[] []   |  |           |     |
+--+  +--+--+  +--+--+  +--+  +
|   [] [] []|     |        |  |
+  +--+--+  +  +--+--+  +  +  +
|      [] []|     |           |
+--+--+  +--+  +  +  +  +--+  +
|      [] []   |  |     |     |
+  +--+--+  +--+--+--+--+  +--+
|  |  |  |[]         |  |     |
+  +  +--+  +--+--+--+--+--+--+
|  |     |[] [] []   |        |
+--+--+--+--+--+  +  +--+--+  +
|     |  |     |[]|  |     |  |
+  +  +  +  +--+  +--+  +  +--+
|  |  |  |   [] []|  |     |  |
+--+--+--+--+  +--+--+--+--+  +
|     |  |   []   |        |  |
+--+--+  +--+  +--+  +--+--+  +
Running 10 x 10 grid 10,000 times for each porosity
p = 0.1: 1.0
p = 0.2: 1.0
p = 0.3: 0.9968
p = 0.4: 0.9125
p = 0.5: 0.4959
p = 0.6: 0.0858
p = 0.7: 0.004
p = 0.8: 0.0
p = 0.9: 0.0
p = 1.0: 0.0
```



## Tcl

```tcl
package require Tcl 8.6

# Structure the bond percolation system as a class
oo::class create BondPercolation {
    variable hwall vwall cells M N
    constructor {width height probability} {
	set M $height
	set N $width
	for {set i 0} {$i <= $height} {incr i} {
	    for {set j 0;set walls {}} {$j < $width} {incr j} {
		lappend walls [expr {rand() < $probability}]
	    }
	    lappend hwall $walls
	}
	for {set i 0} {$i <= $height} {incr i} {
	    for {set j 0;set walls {}} {$j <= $width} {incr j} {
		lappend walls [expr {$j==0 || $j==$width || rand() < $probability}]
	    }
	    lappend vwall $walls
	}
	set cells [lrepeat $height [lrepeat $width 0]]
    }

    method print {{percolated ""}} {
	set nw [string length $M]
	set grid $cells
	if {$percolated ne ""} {
	    lappend grid [lrepeat $N 0]
	    lset grid end $percolated 1
	}
	foreach hws $hwall vws [lrange $vwall 0 end-1] r $grid {
	    incr row
	    puts -nonewline [string repeat " " [expr {$nw+2}]]
	    foreach w $hws {
		puts -nonewline [if {$w} {subst "+-"} {subst "+ "}]
	    }
	    puts "+"
	    puts -nonewline [format "%-*s" [expr {$nw+2}] [expr {
		$row>$M ? $percolated eq "" ? " " : ">" : "$row)"
	    }]]
	    foreach v $vws c $r {
		puts -nonewline [if {$v==1} {subst "|"} {subst " "}]
		puts -nonewline [if {$c==1} {subst "#"} {subst " "}]
	    }
	    puts ""
	}
    }

    method percolate {} {
	try {
	    for {set i 0} {$i < $N} {incr i} {
		if {![lindex $hwall 0 $i]} {
		    my FloodFill $i 0
		}
	    }
	    return ""
	} trap PERCOLATED n {
	    return $n
	}
    }
    method FloodFill {x y} {
	# fill cell
	lset cells $y $x 1
	# bottom
	if {![lindex $hwall [expr {$y+1}] $x]} {
	    if {$y == $N-1} {
		# THE bottom
		throw PERCOLATED $x
	    }
	    if {$y < $N-1 && ![lindex $cells [expr {$y+1}] $x]} {
		my FloodFill $x [expr {$y+1}]
	    }
	}
	# left
	if {![lindex $vwall $y $x] && ![lindex $cells $y [expr {$x-1}]]} {
	    my FloodFill [expr {$x-1}] $y
	}
	# right
	if {![lindex $vwall $y [expr {$x+1}]] && ![lindex $cells $y [expr {$x+1}]]} {
	    my FloodFill [expr {$x+1}] $y
	}
	# top
	if {$y>0 && ![lindex $hwall $y $x] && ![lindex $cells [expr {$y-1}] $x]} {
	    my FloodFill $x [expr {$y-1}]
	}
    }
}

# Demonstrate one run
puts "Sample percolation, 10x10 p=0.5"
BondPercolation create bp 10 10 0.5
bp print [bp percolate]
bp destroy
puts ""

# Collect some aggregate statistics
apply {{} {
    puts "Percentage of tries that percolate, varying p"
    set tries 100
    for {set pint 0} {$pint <= 10} {incr pint} {
	set p [expr {$pint * 0.1}]
	set tot 0
	for {set i 0} {$i < $tries} {incr i} {
	    set bp [BondPercolation new 10 10 $p]
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

Sample percolation, 10x10 p=0.5
    + + +-+-+-+ +-+ +-+ +
1)  |#  |   |   |   |   |
    + +-+ + + +-+ + + +-+
2)  |#|       | |     | |
    + + +-+ +-+ +-+ + +-+
3)  |# # #|# # #| | |   |
    + +-+ + +-+ +-+ +-+ +
4)  |#|# # #| |#  |     |
    +-+ + + +-+ +-+-+ +-+
5)  |# # # #| |#  |   | |
    +-+-+-+-+ + + + +-+-+
6)  | |     | |#|   |   |
    +-+-+-+-+-+ + +-+-+ +
7)  | | | |   |#      | |
    + +-+ +-+-+ +-+ +-+ +
8)  |       |  #    |   |
    + +-+-+ +-+ + + + + +
9)  |          #        |
    + + +-+-+ + +-+-+ + +
10) |   | |    #  | |   |
    + + + + + + +-+ +-+ +
>              #

Percentage of tries that percolate, varying p
p=0.00: 100.0%
p=0.10: 100.0%
p=0.20: 100.0%
p=0.30: 100.0%
p=0.40: 86.0%
p=0.50: 50.0%
p=0.60: 6.0%
p=0.70: 0.0%
p=0.80: 0.0%
p=0.90: 0.0%
p=1.00: 0.0%

```



## zkl

```zkl
// cell states
const FILLED=1; // and odd
const RWALL =2; // right wall
const BWALL =4; // bottom wall
fcn P(p,wall){ (0.0).random(1)<p and wall or 0 }

fcn makeGrid(m,n,p){
    // Allocate two addition rows to avoid checking bounds.
    // Bottom row is also required by drippage
   grid:=Data(m*(n+2));
   do(m){ grid.write(BWALL + RWALL); } // grid is topped with walls
   do(n){
      do(m-1){ grid.write( P(p,BWALL) + P(p,RWALL) ) }
      grid.write(RWALL + P(p,BWALL));  // right border is all right wall, as is left border
   }
   do(m){ grid.write(0); } // for drips off the bottom of grid
   grid
}
fcn show(grid,m,n){ n+=1;
    println("+--"*m,"+");
    foreach i in ([1..n]){ y:=i*m;
       print(i==n and " " or "|"); // bottom row is special, otherwise always have left wall
       foreach j in (m){ c:=grid[y + j];
          print(c.bitAnd(FILLED) and "**" or "  ", c.bitAnd(RWALL)and"|"or" ");
       }
       println();

       if(i==n) return();  // nothing under the bottom row

       foreach j in (m){ print((grid[y + j].bitAnd(BWALL)) and "+--" or "+  "); }
       println("+");
    }
}
fcn fill(grid,x,m){
   if(grid[x].isOdd) return(False); // aka .bitAnd(FILLED) aka already been here
   grid[x]+=FILLED;
   if(x+m>=grid.len()) return(True); // success: reached bottom row
   return(( not grid[x]    .bitAnd(BWALL) and fill(grid,x + m,m) ) or // down
          ( not grid[x]    .bitAnd(RWALL) and fill(grid,x + 1,m) ) or // right
          ( not grid[x - 1].bitAnd(RWALL) and fill(grid,x - 1,m) ) or // left
          ( not grid[x - m].bitAnd(BWALL) and fill(grid,x - m,m) ));  // up
}
fcn percolate(grid,m){
   i:=0; while(i<m and not fill(grid,i+m,m)){ i+=1; }  // pour juice on top row
   return(i<m);  // percolated through the grid?
}
```


```zkl
grid:=makeGrid(10,10,0.40);
println("Did liquid percolate: ",percolate(grid,10)); show(grid,10,10);

println("Running 10,000 tests for each case:");
foreach p in ([0.0 .. 1.0, 0.1]){
   cnt:=0.0; do(10000){ cnt+=percolate(makeGrid(10,10,p),10); }
   "p=%.1f:  %.4f".fmt(p, cnt/10000).println();
}
```

```txt

Did liquid percolate: True
+--+--+--+--+--+--+--+--+--+--+
|** **      |              |  |
+--+  +--+--+  +  +  +  +  +  +
|   **|  |        |        |  |
+  +  +  +--+  +--+--+  +--+--+
|   ** **      |              |
+--+--+  +  +  +  +--+  +  +--+
|     |**|  |        |        |
+  +  +  +  +--+  +  +--+  +  +
|      ** **|  |** **|     |  |
+  +--+--+  +--+  +  +--+  +  +
|     |  |**|  |**|** **      |
+  +  +  +  +--+  +--+  +  +  +
|     |  |** ** ** **|**      |
+--+--+--+--+  +--+--+  +--+--+
|  |     |** **|      **   |  |
+  +  +--+  +  +  +  +  +--+--+
|        |** **|     |**|  |  |
+  +--+  +--+--+--+--+  +  +  +
|              |  |   **|     |
+  +  +  +  +  +  +--+  +  +  +
                      **
Running 10,000 tests for each case:
p=0.0:  1.0000
p=0.1:  1.0000
p=0.2:  1.0000
p=0.3:  0.9978
p=0.4:  0.9163
p=0.5:  0.5017
p=0.6:  0.0890
p=0.7:  0.0033
p=0.8:  0.0000
p=0.9:  0.0000
p=1.0:  0.0000

```

