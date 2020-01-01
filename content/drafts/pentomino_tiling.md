+++
title = "Pentomino tiling"
description = ""
date = 2019-06-21T15:52:51Z
aliases = []
[extra]
id = 20682
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

A [[wp:Pentomino|pentomino]] is a polyomino that consists of 5 squares. There are 12 pentomino shapes,
if you don't count rotations and reflections. Most pentominoes can form their own mirror image through
rotation, but some of them have to be flipped over.


```txt
        I
        I     L       N                                                 Y
 FF     I     L      NN     PP     TTT              V       W     X    YY      ZZ
FF      I     L      N      PP      T     U U       V      WW    XXX    Y      Z
 F      I     LL     N      P       T     UUU     VVV     WW      X     Y     ZZ
```



A Pentomino tiling is an example of an [[wp:Exact_cover|exact cover]] problem and can take on many forms.
A traditional tiling presents an 8 by 8 grid, where 4 cells are left uncovered. The other cells are covered
by the 12 pentomino shapes, without overlaps, with every shape only used once.

The 4 uncovered cells should be chosen at random. Note that not all configurations are solvable.


;Task
Create an 8 by 8 tiling and print the result.


;Example


```txt
F I I I I I L N
F F F L L L L N
W F - X Z Z N N
W W X X X Z N V
T W W X - Z Z V
T T T P P V V V
T Y - P P U U U
Y Y Y Y P U - U
```



;Related tasks
* [[Free_polyominoes_enumeration|Free polyominoes enumeration]]





## C#

{{trans|Java}}

```c#
using System;
using System.Linq;

namespace PentominoTiling
{
    class Program
    {
        static readonly char[] symbols = "FILNPTUVWXYZ-".ToCharArray();

        static readonly int nRows = 8;
        static readonly int nCols = 8;
        static readonly int target = 12;
        static readonly int blank = 12;

        static int[][] grid = new int[nRows][];
        static bool[] placed = new bool[target];

        static void Main(string[] args)
        {
            var rand = new Random();

            for (int r = 0; r < nRows; r++)
                grid[r] = Enumerable.Repeat(-1, nCols).ToArray();

            for (int i = 0; i < 4; i++)
            {
                int randRow, randCol;
                do
                {
                    randRow = rand.Next(nRows);
                    randCol = rand.Next(nCols);
                }
                while (grid[randRow][randCol] == blank);

                grid[randRow][randCol] = blank;
            }

            if (Solve(0, 0))
            {
                PrintResult();
            }
            else
            {
                Console.WriteLine("no solution");
            }

            Console.ReadKey();
        }

        private static void PrintResult()
        {
            foreach (int[] r in grid)
            {
                foreach (int i in r)
                    Console.Write("{0} ", symbols[i]);
                Console.WriteLine();
            }
        }

        private static bool Solve(int pos, int numPlaced)
        {
            if (numPlaced == target)
                return true;

            int row = pos / nCols;
            int col = pos % nCols;

            if (grid[row][col] != -1)
                return Solve(pos + 1, numPlaced);

            for (int i = 0; i < shapes.Length; i++)
            {
                if (!placed[i])
                {
                    foreach (int[] orientation in shapes[i])
                    {
                        if (!TryPlaceOrientation(orientation, row, col, i))
                            continue;

                        placed[i] = true;

                        if (Solve(pos + 1, numPlaced + 1))
                            return true;

                        RemoveOrientation(orientation, row, col);
                        placed[i] = false;
                    }
                }
            }
            return false;
        }

        private static void RemoveOrientation(int[] orientation, int row, int col)
        {
            grid[row][col] = -1;
            for (int i = 0; i < orientation.Length; i += 2)
                grid[row + orientation[i]][col + orientation[i + 1]] = -1;
        }

        private static bool TryPlaceOrientation(int[] orientation, int row, int col, int shapeIndex)
        {
            for (int i = 0; i < orientation.Length; i += 2)
            {
                int x = col + orientation[i + 1];
                int y = row + orientation[i];
                if (x < 0 || x >= nCols || y < 0 || y >= nRows || grid[y][x] != -1)
                    return false;
            }

            grid[row][col] = shapeIndex;
            for (int i = 0; i < orientation.Length; i += 2)
                grid[row + orientation[i]][col + orientation[i + 1]] = shapeIndex;

            return true;
        }

        // four (x, y) pairs are listed, (0,0) not included
        static readonly int[][] F = {
            new int[] {1, -1, 1, 0, 1, 1, 2, 1}, new int[] {0, 1, 1, -1, 1, 0, 2, 0},
            new int[] {1, 0, 1, 1, 1, 2, 2, 1}, new int[] {1, 0, 1, 1, 2, -1, 2, 0},
            new int[] {1, -2, 1, -1, 1, 0, 2, -1}, new int[] {0, 1, 1, 1, 1, 2, 2, 1},
            new int[] {1, -1, 1, 0, 1, 1, 2, -1}, new int[] {1, -1, 1, 0, 2, 0, 2, 1}};

        static readonly int[][] I = {
            new int[] { 0, 1, 0, 2, 0, 3, 0, 4 }, new int[] { 1, 0, 2, 0, 3, 0, 4, 0 } };

        static readonly int[][] L = {
            new int[] {1, 0, 1, 1, 1, 2, 1, 3}, new int[] {1, 0, 2, 0, 3, -1, 3, 0},
            new int[] {0, 1, 0, 2, 0, 3, 1, 3}, new int[] {0, 1, 1, 0, 2, 0, 3, 0},
            new int[] {0, 1, 1, 1, 2, 1, 3, 1}, new int[] {0, 1, 0, 2, 0, 3, 1, 0},
            new int[] {1, 0, 2, 0, 3, 0, 3, 1}, new int[] {1, -3, 1, -2, 1, -1, 1, 0}};

        static readonly int[][] N = {
            new int[] {0, 1, 1, -2, 1, -1, 1, 0}, new int[] {1, 0, 1, 1, 2, 1, 3, 1},
            new int[]  {0, 1, 0, 2, 1, -1, 1, 0}, new int[] {1, 0, 2, 0, 2, 1, 3, 1},
            new int[] {0, 1, 1, 1, 1, 2, 1, 3}, new int[] {1, 0, 2, -1, 2, 0, 3, -1},
            new int[] {0, 1, 0, 2, 1, 2, 1, 3}, new int[] {1, -1, 1, 0, 2, -1, 3, -1}};

        static readonly int[][] P = {
            new int[] {0, 1, 1, 0, 1, 1, 2, 1}, new int[] {0, 1, 0, 2, 1, 0, 1, 1},
            new int[] {1, 0, 1, 1, 2, 0, 2, 1}, new int[] {0, 1, 1, -1, 1, 0, 1, 1},
            new int[] {0, 1, 1, 0, 1, 1, 1, 2}, new int[] {1, -1, 1, 0, 2, -1, 2, 0},
            new int[] {0, 1, 0, 2, 1, 1, 1, 2}, new int[] {0, 1, 1, 0, 1, 1, 2, 0}};

        static readonly int[][] T = {
            new int[] {0, 1, 0, 2, 1, 1, 2, 1}, new int[] {1, -2, 1, -1, 1, 0, 2, 0},
            new int[] {1, 0, 2, -1, 2, 0, 2, 1}, new int[] {1, 0, 1, 1, 1, 2, 2, 0}};

        static readonly int[][] U = {
            new int[] {0, 1, 0, 2, 1, 0, 1, 2}, new int[] {0, 1, 1, 1, 2, 0, 2, 1},
            new int[]  {0, 2, 1, 0, 1, 1, 1, 2}, new int[] {0, 1, 1, 0, 2, 0, 2, 1}};

        static readonly int[][] V = {
            new int[] {1, 0, 2, 0, 2, 1, 2, 2}, new int[] {0, 1, 0, 2, 1, 0, 2, 0},
            new int[] {1, 0, 2, -2, 2, -1, 2, 0}, new int[] {0, 1, 0, 2, 1, 2, 2, 2}};

        static readonly int[][] W = {
            new int[] {1, 0, 1, 1, 2, 1, 2, 2}, new int[] {1, -1, 1, 0, 2, -2, 2, -1},
            new int[] {0, 1, 1, 1, 1, 2, 2, 2}, new int[] {0, 1, 1, -1, 1, 0, 2, -1}};

        static readonly int[][] X = { new int[] { 1, -1, 1, 0, 1, 1, 2, 0 } };

        static readonly int[][] Y = {
            new int[] {1, -2, 1, -1, 1, 0, 1, 1}, new int[] {1, -1, 1, 0, 2, 0, 3, 0},
            new int[] {0, 1, 0, 2, 0, 3, 1, 1}, new int[] {1, 0, 2, 0, 2, 1, 3, 0},
            new int[] {0, 1, 0, 2, 0, 3, 1, 2}, new int[] {1, 0, 1, 1, 2, 0, 3, 0},
            new int[] {1, -1, 1, 0, 1, 1, 1, 2}, new int[] {1, 0, 2, -1, 2, 0, 3, 0}};

        static readonly int[][] Z = {
            new int[] {0, 1, 1, 0, 2, -1, 2, 0}, new int[] {1, 0, 1, 1, 1, 2, 2, 2},
            new int[] {0, 1, 1, 1, 2, 1, 2, 2}, new int[] {1, -2, 1, -1, 1, 0, 2, -2}};

        static readonly int[][][] shapes = { F, I, L, N, P, T, U, V, W, X, Y, Z };
    }
}
```


```txt
I N F F - - L L
I N N F F P P L
I - N F W P P L
I Y N W W X P L
I Y W W X X X -
Y Y T T T X Z V
U Y U T Z Z Z V
U U U T Z V V V
```



## Go

{{trans|Java}}

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

var F = [][]int{
    {1, -1, 1, 0, 1, 1, 2, 1}, {0, 1, 1, -1, 1, 0, 2, 0},
    {1, 0, 1, 1, 1, 2, 2, 1}, {1, 0, 1, 1, 2, -1, 2, 0},
    {1, -2, 1, -1, 1, 0, 2, -1}, {0, 1, 1, 1, 1, 2, 2, 1},
    {1, -1, 1, 0, 1, 1, 2, -1}, {1, -1, 1, 0, 2, 0, 2, 1},
}

var I = [][]int{{0, 1, 0, 2, 0, 3, 0, 4}, {1, 0, 2, 0, 3, 0, 4, 0}}

var L = [][]int{
    {1, 0, 1, 1, 1, 2, 1, 3}, {1, 0, 2, 0, 3, -1, 3, 0},
    {0, 1, 0, 2, 0, 3, 1, 3}, {0, 1, 1, 0, 2, 0, 3, 0}, {0, 1, 1, 1, 2, 1, 3, 1},
    {0, 1, 0, 2, 0, 3, 1, 0}, {1, 0, 2, 0, 3, 0, 3, 1}, {1, -3, 1, -2, 1, -1, 1, 0},
}

var N = [][]int{
    {0, 1, 1, -2, 1, -1, 1, 0}, {1, 0, 1, 1, 2, 1, 3, 1},
    {0, 1, 0, 2, 1, -1, 1, 0}, {1, 0, 2, 0, 2, 1, 3, 1}, {0, 1, 1, 1, 1, 2, 1, 3},
    {1, 0, 2, -1, 2, 0, 3, -1}, {0, 1, 0, 2, 1, 2, 1, 3}, {1, -1, 1, 0, 2, -1, 3, -1},
}

var P = [][]int{
    {0, 1, 1, 0, 1, 1, 2, 1}, {0, 1, 0, 2, 1, 0, 1, 1},
    {1, 0, 1, 1, 2, 0, 2, 1}, {0, 1, 1, -1, 1, 0, 1, 1}, {0, 1, 1, 0, 1, 1, 1, 2},
    {1, -1, 1, 0, 2, -1, 2, 0}, {0, 1, 0, 2, 1, 1, 1, 2}, {0, 1, 1, 0, 1, 1, 2, 0},
}

var T = [][]int{
    {0, 1, 0, 2, 1, 1, 2, 1}, {1, -2, 1, -1, 1, 0, 2, 0},
    {1, 0, 2, -1, 2, 0, 2, 1}, {1, 0, 1, 1, 1, 2, 2, 0},
}

var U = [][]int{
    {0, 1, 0, 2, 1, 0, 1, 2}, {0, 1, 1, 1, 2, 0, 2, 1},
    {0, 2, 1, 0, 1, 1, 1, 2}, {0, 1, 1, 0, 2, 0, 2, 1},
}

var V = [][]int{
    {1, 0, 2, 0, 2, 1, 2, 2}, {0, 1, 0, 2, 1, 0, 2, 0},
    {1, 0, 2, -2, 2, -1, 2, 0}, {0, 1, 0, 2, 1, 2, 2, 2},
}

var W = [][]int{
    {1, 0, 1, 1, 2, 1, 2, 2}, {1, -1, 1, 0, 2, -2, 2, -1},
    {0, 1, 1, 1, 1, 2, 2, 2}, {0, 1, 1, -1, 1, 0, 2, -1},
}

var X = [][]int{{1, -1, 1, 0, 1, 1, 2, 0}}

var Y = [][]int{
    {1, -2, 1, -1, 1, 0, 1, 1}, {1, -1, 1, 0, 2, 0, 3, 0},
    {0, 1, 0, 2, 0, 3, 1, 1}, {1, 0, 2, 0, 2, 1, 3, 0}, {0, 1, 0, 2, 0, 3, 1, 2},
    {1, 0, 1, 1, 2, 0, 3, 0}, {1, -1, 1, 0, 1, 1, 1, 2}, {1, 0, 2, -1, 2, 0, 3, 0},
}

var Z = [][]int{
    {0, 1, 1, 0, 2, -1, 2, 0}, {1, 0, 1, 1, 1, 2, 2, 2},
    {0, 1, 1, 1, 2, 1, 2, 2}, {1, -2, 1, -1, 1, 0, 2, -2},
}

var shapes = [][][]int{F, I, L, N, P, T, U, V, W, X, Y, Z}

var symbols = []byte("FILNPTUVWXYZ-")

const (
    nRows = 8
    nCols = 8
    blank = 12
)

var grid [nRows][nCols]int
var placed [12]bool

func tryPlaceOrientation(o []int, r, c, shapeIndex int) bool {
    for i := 0; i < len(o); i += 2 {
        x := c + o[i+1]
        y := r + o[i]
        if x < 0 || x >= nCols || y < 0 || y >= nRows || grid[y][x] != -1 {
            return false
        }
    }
    grid[r][c] = shapeIndex
    for i := 0; i < len(o); i += 2 {
        grid[r+o[i]][c+o[i+1]] = shapeIndex
    }
    return true
}

func removeOrientation(o []int, r, c int) {
    grid[r][c] = -1
    for i := 0; i < len(o); i += 2 {
        grid[r+o[i]][c+o[i+1]] = -1
    }
}

func solve(pos, numPlaced int) bool {
    if numPlaced == len(shapes) {
        return true
    }
    row := pos / nCols
    col := pos % nCols
    if grid[row][col] != -1 {
        return solve(pos+1, numPlaced)
    }

    for i := range shapes {
        if !placed[i] {
            for _, orientation := range shapes[i] {
                if !tryPlaceOrientation(orientation, row, col, i) {
                    continue
                }
                placed[i] = true
                if solve(pos+1, numPlaced+1) {
                    return true
                }
                removeOrientation(orientation, row, col)
                placed[i] = false
            }
        }
    }
    return false
}

func shuffleShapes() {
    rand.Shuffle(len(shapes), func(i, j int) {
        shapes[i], shapes[j] = shapes[j], shapes[i]
        symbols[i], symbols[j] = symbols[j], symbols[i]
    })
}

func printResult() {
    for _, r := range grid {
        for _, i := range r {
            fmt.Printf("%c ", symbols[i])
        }
        fmt.Println()
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    shuffleShapes()
    for r := 0; r < nRows; r++ {
        for i := range grid[r] {
            grid[r][i] = -1
        }
    }
    for i := 0; i < 4; i++ {
        var randRow, randCol int
        for {
            randRow = rand.Intn(nRows)
            randCol = rand.Intn(nCols)
            if grid[randRow][randCol] != blank {
                break
            }
        }
        grid[randRow][randCol] = blank
    }
    if solve(0, 0) {
        printResult()
    } else {
        fmt.Println("No solution")
    }
}
```


{{out}}
Sample output:

```txt

Z I I I I I - Y
Z Z Z V - F Y Y
U U Z V - F F Y
U V V V F F X Y
U U P P W X X X
T P P P W W X -
T T T N N W W L
T N N N L L L L

```



## Java


```java
package pentominotiling;

import java.util.*;

public class PentominoTiling {

    static final char[] symbols = "FILNPTUVWXYZ-".toCharArray();
    static final Random rand = new Random();

    static final int nRows = 8;
    static final int nCols = 8;
    static final int blank = 12;

    static int[][] grid = new int[nRows][nCols];
    static boolean[] placed = new boolean[symbols.length - 1];

    public static void main(String[] args) {
        shuffleShapes();

        for (int r = 0; r < nRows; r++)
            Arrays.fill(grid[r], -1);

        for (int i = 0; i < 4; i++) {
            int randRow, randCol;
            do {
                randRow = rand.nextInt(nRows);
                randCol = rand.nextInt(nCols);
            } while (grid[randRow][randCol] == blank);
            grid[randRow][randCol] = blank;
        }

        if (solve(0, 0)) {
            printResult();
        } else {
            System.out.println("no solution");
        }
    }

    static void shuffleShapes() {
        int n = shapes.length;
        while (n > 1) {
            int r = rand.nextInt(n--);

            int[][] tmp = shapes[r];
            shapes[r] = shapes[n];
            shapes[n] = tmp;

            char tmpSymbol = symbols[r];
            symbols[r] = symbols[n];
            symbols[n] = tmpSymbol;
        }
    }

    static void printResult() {
        for (int[] r : grid) {
            for (int i : r)
                System.out.printf("%c ", symbols[i]);
            System.out.println();
        }
    }

    static boolean tryPlaceOrientation(int[] o, int r, int c, int shapeIndex) {

        for (int i = 0; i < o.length; i += 2) {
            int x = c + o[i + 1];
            int y = r + o[i];
            if (x < 0 || x >= nCols || y < 0 || y >= nRows || grid[y][x] != -1)
                return false;
        }

        grid[r][c] = shapeIndex;
        for (int i = 0; i < o.length; i += 2)
            grid[r + o[i]][c + o[i + 1]] = shapeIndex;

        return true;
    }

    static void removeOrientation(int[] o, int r, int c) {
        grid[r][c] = -1;
        for (int i = 0; i < o.length; i += 2)
            grid[r + o[i]][c + o[i + 1]] = -1;
    }

    static boolean solve(int pos, int numPlaced) {
        if (numPlaced == shapes.length)
            return true;

        int row = pos / nCols;
        int col = pos % nCols;

        if (grid[row][col] != -1)
            return solve(pos + 1, numPlaced);

        for (int i = 0; i < shapes.length; i++) {
            if (!placed[i]) {
                for (int[] orientation : shapes[i]) {

                    if (!tryPlaceOrientation(orientation, row, col, i))
                        continue;

                    placed[i] = true;

                    if (solve(pos + 1, numPlaced + 1))
                        return true;

                    removeOrientation(orientation, row, col);
                    placed[i] = false;
                }
            }
        }
        return false;
    }

    static final int[][] F = {{1, -1, 1, 0, 1, 1, 2, 1}, {0, 1, 1, -1, 1, 0, 2, 0},
    {1, 0, 1, 1, 1, 2, 2, 1}, {1, 0, 1, 1, 2, -1, 2, 0}, {1, -2, 1, -1, 1, 0, 2, -1},
    {0, 1, 1, 1, 1, 2, 2, 1}, {1, -1, 1, 0, 1, 1, 2, -1}, {1, -1, 1, 0, 2, 0, 2, 1}};

    static final int[][] I = {{0, 1, 0, 2, 0, 3, 0, 4}, {1, 0, 2, 0, 3, 0, 4, 0}};

    static final int[][] L = {{1, 0, 1, 1, 1, 2, 1, 3}, {1, 0, 2, 0, 3, -1, 3, 0},
    {0, 1, 0, 2, 0, 3, 1, 3}, {0, 1, 1, 0, 2, 0, 3, 0}, {0, 1, 1, 1, 2, 1, 3, 1},
    {0, 1, 0, 2, 0, 3, 1, 0}, {1, 0, 2, 0, 3, 0, 3, 1}, {1, -3, 1, -2, 1, -1, 1, 0}};

    static final int[][] N = {{0, 1, 1, -2, 1, -1, 1, 0}, {1, 0, 1, 1, 2, 1, 3, 1},
    {0, 1, 0, 2, 1, -1, 1, 0}, {1, 0, 2, 0, 2, 1, 3, 1}, {0, 1, 1, 1, 1, 2, 1, 3},
    {1, 0, 2, -1, 2, 0, 3, -1}, {0, 1, 0, 2, 1, 2, 1, 3}, {1, -1, 1, 0, 2, -1, 3, -1}};

    static final int[][] P = {{0, 1, 1, 0, 1, 1, 2, 1}, {0, 1, 0, 2, 1, 0, 1, 1},
    {1, 0, 1, 1, 2, 0, 2, 1}, {0, 1, 1, -1, 1, 0, 1, 1}, {0, 1, 1, 0, 1, 1, 1, 2},
    {1, -1, 1, 0, 2, -1, 2, 0}, {0, 1, 0, 2, 1, 1, 1, 2}, {0, 1, 1, 0, 1, 1, 2, 0}};

    static final int[][] T = {{0, 1, 0, 2, 1, 1, 2, 1}, {1, -2, 1, -1, 1, 0, 2, 0},
    {1, 0, 2, -1, 2, 0, 2, 1}, {1, 0, 1, 1, 1, 2, 2, 0}};

    static final int[][] U = {{0, 1, 0, 2, 1, 0, 1, 2}, {0, 1, 1, 1, 2, 0, 2, 1},
    {0, 2, 1, 0, 1, 1, 1, 2}, {0, 1, 1, 0, 2, 0, 2, 1}};

    static final int[][] V = {{1, 0, 2, 0, 2, 1, 2, 2}, {0, 1, 0, 2, 1, 0, 2, 0},
    {1, 0, 2, -2, 2, -1, 2, 0}, {0, 1, 0, 2, 1, 2, 2, 2}};

    static final int[][] W = {{1, 0, 1, 1, 2, 1, 2, 2}, {1, -1, 1, 0, 2, -2, 2, -1},
    {0, 1, 1, 1, 1, 2, 2, 2}, {0, 1, 1, -1, 1, 0, 2, -1}};

    static final int[][] X = {{1, -1, 1, 0, 1, 1, 2, 0}};

    static final int[][] Y = {{1, -2, 1, -1, 1, 0, 1, 1}, {1, -1, 1, 0, 2, 0, 3, 0},
    {0, 1, 0, 2, 0, 3, 1, 1}, {1, 0, 2, 0, 2, 1, 3, 0}, {0, 1, 0, 2, 0, 3, 1, 2},
    {1, 0, 1, 1, 2, 0, 3, 0}, {1, -1, 1, 0, 1, 1, 1, 2}, {1, 0, 2, -1, 2, 0, 3, 0}};

    static final int[][] Z = {{0, 1, 1, 0, 2, -1, 2, 0}, {1, 0, 1, 1, 1, 2, 2, 2},
    {0, 1, 1, 1, 2, 1, 2, 2}, {1, -2, 1, -1, 1, 0, 2, -2}};

    static final int[][][] shapes = {F, I, L, N, P, T, U, V, W, X, Y, Z};
}
```



```txt
F I I I I I L L
F F F P P V L -
Z F P P P V L N
Z Z Z V V V L N
- X Z - W W N N
X X X W W - N T
U X U W Y T T T
U U U Y Y Y Y T
```




## Julia

{{trans|Zkl}}

```julia
using Random

struct GridPoint x::Int; y::Int end

const nrows, ncols, target = 8, 8, 12
const grid = fill('*', nrows, ncols)
const shapeletters = ['F', 'I', 'L', 'N', 'P', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
const shapevec = [
    [(1,-1, 1,0, 1,1, 2,1), (0,1, 1,-1, 1,0, 2,0),
        (1,0 , 1,1, 1,2, 2,1), (1,0, 1,1, 2,-1, 2,0), (1,-2, 1,-1, 1,0, 2,-1),
        (0,1, 1,1, 1,2, 2,1), (1,-1, 1,0, 1,1, 2,-1), (1,-1, 1,0, 2,0, 2,1)],
    [(0,1, 0,2, 0,3, 0,4), (1,0, 2,0, 3,0, 4,0)],
    [(1,0, 1,1, 1,2, 1,3), (1,0, 2,0, 3,-1, 3,0),
        (0,1, 0,2, 0,3, 1,3), (0,1, 1,0, 2,0, 3,0), (0,1, 1,1, 2,1, 3,1),
        (0,1, 0,2, 0,3, 1,0), (1,0, 2,0, 3,0, 3,1), (1,-3, 1,-2, 1,-1, 1,0)],
    [(0,1, 1,-2, 1,-1, 1,0), (1,0, 1,1, 2,1, 3,1),
        (0,1, 0,2, 1,-1, 1,0), (1,0, 2,0, 2,1, 3,1), (0,1, 1,1, 1,2, 1,3),
        (1,0, 2,-1, 2,0, 3,-1), (0,1, 0,2, 1,2, 1,3), (1,-1, 1,0, 2,-1, 3,-1)],
    [(0,1, 1,0, 1,1, 2,1), (0,1, 0,2, 1,0, 1,1),
        (1,0, 1,1, 2,0, 2,1), (0,1, 1,-1, 1,0, 1,1), (0,1, 1,0, 1,1, 1,2),
        (1,-1, 1,0, 2,-1, 2,0), (0,1, 0,2, 1,1, 1,2), (0,1, 1,0, 1,1, 2,0)],
    [(0,1, 0,2, 1,1, 2,1), (1,-2, 1,-1, 1,0, 2,0),
        (1,0, 2,-1, 2,0, 2,1), (1,0, 1,1, 1,2, 2,0)],
    [(0,1, 0,2, 1,0, 1,2), (0,1, 1,1, 2,0, 2,1),
        (0,2, 1,0, 1,1, 1,2), (0,1, 1,0, 2,0, 2,1)],
    [(1,0, 2,0, 2,1, 2,2), (0,1, 0,2, 1,0, 2,0),
        (1,0, 2,-2, 2,-1, 2,0), (0,1, 0,2, 1,2, 2,2)],
    [(1,0, 1,1, 2,1, 2,2), (1,-1, 1,0, 2,-2, 2,-1),
        (0,1, 1,1, 1,2, 2,2), (0,1, 1,-1, 1,0, 2,-1)],
    [(1,-1, 1,0, 1,1, 2,0)],
    [(1,-2, 1,-1, 1,0, 1,1), (1,-1, 1,0, 2,0, 3,0),
        (0,1, 0,2, 0,3, 1,1), (1,0, 2,0, 2,1, 3,0), (0,1, 0,2, 0,3, 1,2),
        (1,0, 1,1, 2,0, 3,0), (1,-1, 1,0, 1,1, 1,2), (1,0, 2,-1, 2,0, 3,0)],
    [(0,1, 1,0, 2,-1, 2,0), (1,0, 1,1, 1,2, 2,2),
        (0,1, 1,1, 2,1, 2,2), (1,-2, 1,-1, 1,0, 2,-2)]]
const shapes = Dict{Char,Vector{Vector{GridPoint}}}()
const placed = Dict{Char,Bool}()

for (ltr, vec) in zip(shapeletters, shapevec)
    shapes[ltr] = [[GridPoint(v[i], v[i + 1]) for i in 1:2:7] for v in vec]
    placed[ltr] = false
end

printgrid(m, w, h) = for x in 1:w for y in 1:h print(m[x, y], " ") end; println() end

function tryplaceorientation(o, R, C, shapeindex)
    for p in o
        r, c = R + p.x, C + p.y
        if r < 1 || r > nrows || c < 1 || c > ncols || grid[r, c] != '*'
            return false
        end
    end
    grid[R, C] = shapeindex
    for p in o
        grid[R + p.x, C + p.y] = shapeindex
    end
    true
end

function removeorientation(o, r, c)
    grid[r, c] = '*'
    for p in o
        grid[r + p.x, c + p.y] = '*'
    end
end

function solve(pos, nplaced)
    if nplaced == target
        return true
    end
    row, col = divrem(pos, ncols) .+ 1
    if grid[row, col] != '*'
        return solve(pos + 1, nplaced)
    end
    for i in keys((shapes))
        if !placed[i]
            for orientation in shapes[i]
                if tryplaceorientation(orientation, row, col, i)
                    placed[i] = true
                    if solve(pos + 1, nplaced + 1)
                        return true
                    else
                        removeorientation(orientation, row, col)
                        placed[i] = false
                    end
                end
            end
        end
    end
    false
end

function placepentominoes()
    for p in zip(shuffle(collect(1:nrows))[1:4], shuffle(collect(1:ncols))[1:4])
        grid[p[1], p[2]] = ' '
    end
    if solve(0, 0)
        printgrid(grid, nrows, ncols)
    else
        println("No solution found.")
    end
end

placepentominoes()

```
{{out}}

```txt

Z Z   X P P P V
I Z X X X P P V
I Z Z X L V V V
I T T T L L L L
I N T F F   U U
I N T W F F U
N N W W F Y U U
N W W   Y Y Y Y

```



## Kotlin

{{trans|Java}}

```scala
// Version 1.1.4-3

import java.util.Random

val F = arrayOf(
    intArrayOf(1, -1, 1, 0, 1, 1, 2, 1), intArrayOf(0, 1, 1, -1, 1, 0, 2, 0),
    intArrayOf(1, 0, 1, 1, 1, 2, 2, 1), intArrayOf(1, 0, 1, 1, 2, -1, 2, 0),
    intArrayOf(1, -2, 1, -1, 1, 0, 2, -1), intArrayOf(0, 1, 1, 1, 1, 2, 2, 1),
    intArrayOf(1, -1, 1, 0, 1, 1, 2, -1), intArrayOf(1, -1, 1, 0, 2, 0, 2, 1)
)

val I = arrayOf(
    intArrayOf(0, 1, 0, 2, 0, 3, 0, 4), intArrayOf(1, 0, 2, 0, 3, 0, 4, 0)
)

val L = arrayOf(
    intArrayOf(1, 0, 1, 1, 1, 2, 1, 3), intArrayOf(1, 0, 2, 0, 3, -1, 3, 0),
    intArrayOf(0, 1, 0, 2, 0, 3, 1, 3), intArrayOf(0, 1, 1, 0, 2, 0, 3, 0),
    intArrayOf(0, 1, 1, 1, 2, 1, 3, 1), intArrayOf(0, 1, 0, 2, 0, 3, 1, 0),
    intArrayOf(1, 0, 2, 0, 3, 0, 3, 1), intArrayOf(1, -3, 1, -2, 1, -1, 1, 0)
)

val N = arrayOf(
    intArrayOf(0, 1, 1, -2, 1, -1, 1, 0), intArrayOf(1, 0, 1, 1, 2, 1, 3, 1),
    intArrayOf(0, 1, 0, 2, 1, -1, 1, 0), intArrayOf(1, 0, 2, 0, 2, 1, 3, 1),
    intArrayOf(0, 1, 1, 1, 1, 2, 1, 3), intArrayOf(1, 0, 2, -1, 2, 0, 3, -1),
    intArrayOf(0, 1, 0, 2, 1, 2, 1, 3), intArrayOf(1, -1, 1, 0, 2, -1, 3, -1)
)

val P = arrayOf(
    intArrayOf(0, 1, 1, 0, 1, 1, 2, 1), intArrayOf(0, 1, 0, 2, 1, 0, 1, 1),
    intArrayOf(1, 0, 1, 1, 2, 0, 2, 1), intArrayOf(0, 1, 1, -1, 1, 0, 1, 1),
    intArrayOf(0, 1, 1, 0, 1, 1, 1, 2), intArrayOf(1, -1, 1, 0, 2, -1, 2, 0),
    intArrayOf(0, 1, 0, 2, 1, 1, 1, 2), intArrayOf(0, 1, 1, 0, 1, 1, 2, 0)
)

val T = arrayOf(
    intArrayOf(0, 1, 0, 2, 1, 1, 2, 1), intArrayOf(1, -2, 1, -1, 1, 0, 2, 0),
    intArrayOf(1, 0, 2, -1, 2, 0, 2, 1), intArrayOf(1, 0, 1, 1, 1, 2, 2, 0)
)

val U = arrayOf(
    intArrayOf(0, 1, 0, 2, 1, 0, 1, 2), intArrayOf(0, 1, 1, 1, 2, 0, 2, 1),
    intArrayOf(0, 2, 1, 0, 1, 1, 1, 2), intArrayOf(0, 1, 1, 0, 2, 0, 2, 1)
)

val V = arrayOf(
    intArrayOf(1, 0, 2, 0, 2, 1, 2, 2), intArrayOf(0, 1, 0, 2, 1, 0, 2, 0),
    intArrayOf(1, 0, 2, -2, 2, -1, 2, 0), intArrayOf(0, 1, 0, 2, 1, 2, 2, 2)
)

val W = arrayOf(
    intArrayOf(1, 0, 1, 1, 2, 1, 2, 2), intArrayOf(1, -1, 1, 0, 2, -2, 2, -1),
    intArrayOf(0, 1, 1, 1, 1, 2, 2, 2), intArrayOf(0, 1, 1, -1, 1, 0, 2, -1)
)

val X = arrayOf(intArrayOf(1, -1, 1, 0, 1, 1, 2, 0))

val Y = arrayOf(
    intArrayOf(1, -2, 1, -1, 1, 0, 1, 1), intArrayOf(1, -1, 1, 0, 2, 0, 3, 0),
    intArrayOf(0, 1, 0, 2, 0, 3, 1, 1), intArrayOf(1, 0, 2, 0, 2, 1, 3, 0),
    intArrayOf(0, 1, 0, 2, 0, 3, 1, 2), intArrayOf(1, 0, 1, 1, 2, 0, 3, 0),
    intArrayOf(1, -1, 1, 0, 1, 1, 1, 2), intArrayOf(1, 0, 2, -1, 2, 0, 3, 0)
)

val Z = arrayOf(
    intArrayOf(0, 1, 1, 0, 2, -1, 2, 0), intArrayOf(1, 0, 1, 1, 1, 2, 2, 2),
    intArrayOf(0, 1, 1, 1, 2, 1, 2, 2), intArrayOf(1, -2, 1, -1, 1, 0, 2, -2)
)

val shapes = arrayOf(F, I, L, N, P, T, U, V, W, X, Y, Z)
val rand = Random()

val symbols = "FILNPTUVWXYZ-".toCharArray()

val nRows = 8
val nCols = 8
val blank = 12

val grid = Array(nRows) { IntArray(nCols) }
val placed = BooleanArray(symbols.size - 1)

fun tryPlaceOrientation(o: IntArray, r: Int, c: Int, shapeIndex: Int): Boolean {
    for (i in 0 until o.size step 2) {
        val x = c + o[i + 1]
        val y = r + o[i]
        if (x !in (0 until nCols) || y !in (0 until nRows) || grid[y][x] != - 1) return false
    }
    grid[r][c] = shapeIndex
    for (i in 0 until o.size step 2) grid[r + o[i]][c + o[i + 1]] = shapeIndex
    return true
}

fun removeOrientation(o: IntArray, r: Int, c: Int) {
    grid[r][c] = -1
    for (i in 0 until o.size step 2) grid[r + o[i]][c + o[i + 1]] = -1
}

fun solve(pos: Int, numPlaced: Int): Boolean {
    if (numPlaced == shapes.size) return true
    val row = pos / nCols
    val col = pos % nCols
    if (grid[row][col] != -1) return solve(pos + 1, numPlaced)

    for (i in 0 until shapes.size) {
        if (!placed[i]) {
            for (orientation in shapes[i]) {
                if (!tryPlaceOrientation(orientation, row, col, i)) continue
                placed[i] = true
                if (solve(pos + 1, numPlaced + 1)) return true
                removeOrientation(orientation, row, col)
                placed[i] = false
            }
        }
    }
    return false
}

fun shuffleShapes() {
    var n = shapes.size
    while (n > 1) {
        val r = rand.nextInt(n--)
        val tmp = shapes[r]
        shapes[r] = shapes[n]
        shapes[n] = tmp
        val tmpSymbol= symbols[r]
        symbols[r] = symbols[n]
        symbols[n] = tmpSymbol
    }
}

fun printResult() {
    for (r in grid) {
        for (i in r) print("${symbols[i]} ")
        println()
    }
}

fun main(args: Array<String>) {
    shuffleShapes()
    for (r in 0 until nRows) grid[r].fill(-1)
    for (i in 0..3) {
        var randRow: Int
        var randCol: Int
        do {
            randRow = rand.nextInt(nRows)
            randCol = rand.nextInt(nCols)
        }
        while (grid[randRow][randCol] == blank)
        grid[randRow][randCol] = blank
    }
    if (solve(0, 0)) printResult()
    else println("No solution")
}
```


Sample output:

```txt

I I I I I F - W
N N N F F F W W
U U N N F W W -
- U V L L L L T
U U V L Z T T T
V V V X Z Z Z T
P P X X X Y Z -
P P P X Y Y Y Y

```



## Phix

Apart from the shape table creation, the solving part is a translation of Go.

I also added a fast unsolveable() check routine, not that it desperately needed it.

```Phix
constant pentominoes = split("""
......I................................................................
......I.....L......N.........................................Y.........
.FF...I.....L.....NN....PP....TTT...........V.....W....X....YY.....ZZ..
FF....I.....L.....N.....PP.....T....U.U.....V....WW...XXX....Y.....Z...
.F....I.....LL....N.....P......T....UUU...VVV...WW.....X.....Y....ZZ...""",'\n')
----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

function get_shapes()
    sequence res = {}
    for offset=0 to length(pentominoes[1]) by 6 do
        --
        -- scan left/right, and up/down, both ways, to give 8 orientations
        --      (computes the equivalent of those hard-coded tables in
        --       other examples, albeit in a slightly different order.)
        --
        sequence shape = {}
        integer letter
        for orientation=0 to 7 do
            sequence this = {}
            bool found = false
            integer fr, fc
            for r=1 to 5 do
                for c=1 to 5 do
                    integer rr = iff(and_bits(orientation,#01)?6-r:r),
                            cc = iff(and_bits(orientation,#02)?6-c:c)
                    if and_bits(orientation,#04) then {rr,cc} = {cc,rr} end if
                    integer ch = pentominoes[rr,offset+cc]
                    if ch!='.' then
                        if not found then
                            {found,fr,fc,letter} = {true,r,c,ch}
                        else
                            this &= {r-fr,c-fc}
                        end if
                    end if
                end for
            end for
            if not find(this,shape) then
                shape = append(shape,this)
            end if
        end for
        res = append(res,{letter&"",shape})
    end for
    return res
end function

constant shapes = get_shapes(),
         nRows = 8,
         nCols = 8

sequence grid = repeat(repeat(' ',nCols),nRows),
         placed = repeat(false,length(shapes))

function can_place(sequence o, integer r, c, ch)
    for i=1 to length(o) by 2 do
        integer x := c + o[i+1],
                y := r + o[i]
        if x<1 or x>nCols
        or y<1 or y>nRows
        or grid[y][x]!=' ' then
            return false
        end if
    end for
    grid[r][c] = ch
    for i=1 to length(o) by 2 do
        grid[r+o[i]][c+o[i+1]] = ch
    end for
    return true
end function

procedure un_place(sequence o, integer r, c)
    grid[r][c] = ' '
    for i=1 to length(o) by 2 do
        grid[r+o[i]][c+o[i+1]] = ' '
    end for
end procedure

function solve(integer pos=0, numPlaced=0)
    if numPlaced == length(shapes) then
        return true
    end if
    integer row = floor(pos/8)+1,
            col = mod(pos,8)+1
    if grid[row][col]!=' ' then
        return solve(pos+1, numPlaced)
    end if
    for i=1 to length(shapes) do
        if not placed[i] then
            integer ch = shapes[i][1][1]
            for j=1 to length(shapes[i][2]) do
                sequence o = shapes[i][2][j]
                if can_place(o, row, col, ch) then
                    placed[i] = true
                    if solve(pos+1, numPlaced+1) then
                        return true
                    end if
                    un_place(o, row, col)
                    placed[i] = false
                end if
            end for
        end if
    end for
    return false
end function

function unsolveable()
--
-- The only unsolvable grids seen have
--  -.- or -..- or -... at edge/corner,
--   -      --      ---           -
--  or somewhere in the middle a -.-
--                                -
--
-- Simply place all shapes at all positions/orientations,
--  all the while checking for any untouchable cells.
--
    sequence griddled = grid
    for r=1 to 8 do
        for c=1 to 8 do
            if grid[r][c]=' ' then
                for i=1 to length(shapes) do
                    integer ch = shapes[i][1][1]
                    for j=1 to length(shapes[i][2]) do
                        sequence o = shapes[i][2][j]
                        if can_place(o, r, c, ch) then
                            grid[r][c] = ' '
                            griddled[r][c] = '-'
                            for k=1 to length(o) by 2 do
                                grid[r+o[k]][c+o[k+1]] = ' '
                                griddled[r+o[k]][c+o[k+1]] = '-'
                            end for
                        end if
                    end for
                end for
                if griddled[r][c]!='-' then return true end if
            end if
        end for
    end for
    return false
end function

procedure add_four_randomly()
    integer count = 0
    while count<4 do
        integer r = rand(8),
                c = rand(8)
        if grid[r][c]=' ' then
            grid[r][c] = '-'
            count += 1
        end if
    end while
end procedure

procedure main()
    add_four_randomly()
    if unsolveable() then
        puts(1,"No solution\n")
    else
        if not solve() then ?9/0 end if
    end if
    puts(1,join(grid,"\n")&"\n")

end procedure
main()
```

{{out}}

```txt

FFPPPVVV
YFFPPX-V
YFUUXXXV
YYU-ZX-I
YTUUZZZI
-TTTWWZI
LTNNNWWI
LLLLNNWI

```



## Visual Basic .NET

{{trans|Java}}via{{trans|C#}}
Instead of having a large declaration for each rotation of each pentomino, a small array of encoded positions is supplied '''(seeds)''', and it is expanded into the set of 63 possible orientations. However, the additional expansion routines code does take up about 2/3 of the space that would have been taken by the elaborate Integer array definitions.

```vbnet
Module Module1

    Dim symbols As Char() = "XYPFTVNLUZWI█".ToCharArray(),
        nRows As Integer = 8, nCols As Integer = 8,
        target As Integer = 12, blank As Integer = 12,
        grid As Integer()() = New Integer(nRows - 1)() {},
        placed As Boolean() = New Boolean(target - 1) {},
        pens As List(Of List(Of Integer())), rand As Random,
        seeds As Integer() = {291, 292, 293, 295, 297, 329, 330, 332, 333, 335, 378, 586}

    Sub Main()
        Unpack(seeds) : rand = New Random() : ShuffleShapes(2)
        For r As Integer = 0 To nRows - 1
            grid(r) = Enumerable.Repeat(-1, nCols).ToArray() : Next
        For i As Integer = 0 To 3
            Dim rRow, rCol As Integer : Do : rRow = rand.Next(nRows) : rCol = rand.Next(nCols)
            Loop While grid(rRow)(rCol) = blank : grid(rRow)(rCol) = blank
        Next
        If Solve(0, 0) Then
            PrintResult()
        Else
            Console.WriteLine("no solution for this configuration:") : PrintResult()
        End If
        If System.Diagnostics.Debugger.IsAttached Then Console.ReadKey()
    End Sub

    Sub ShuffleShapes(count As Integer) ' changes order of the pieces for a more random solution
        For i As Integer = 0 To count : For j = 0 To pens.Count - 1
                Dim r As Integer : Do : r = rand.Next(pens.Count) : Loop Until r <> j
                Dim tmp As List(Of Integer()) = pens(r) : pens(r) = pens(j) : pens(j) = tmp
                Dim ch As Char = symbols(r) : symbols(r) = symbols(j) : symbols(j) = ch
            Next : Next
    End Sub

    Sub PrintResult() ' display results
        For Each r As Integer() In grid : For Each i As Integer In r
                Console.Write("{0} ", If(i < 0, ".", symbols(i)))
            Next : Console.WriteLine() : Next
    End Sub

    ' returns first found solution only
    Function Solve(ByVal pos As Integer, ByVal numPlaced As Integer) As Boolean
        If numPlaced = target Then Return True
        Dim row As Integer = pos \ nCols, col As Integer = pos Mod nCols
        If grid(row)(col) <> -1 Then Return Solve(pos + 1, numPlaced)
        For i As Integer = 0 To pens.Count - 1 : If Not placed(i) Then
                For Each orientation As Integer() In pens(i)
                    If Not TPO(orientation, row, col, i) Then Continue For
                    placed(i) = True : If Solve(pos + 1, numPlaced + 1) Then Return True
                    RmvO(orientation, row, col) : placed(i) = False
                Next : End If : Next : Return False
    End Function

    ' removes a placed orientation
    Sub RmvO(ByVal ori As Integer(), ByVal row As Integer, ByVal col As Integer)
        grid(row)(col) = -1 : For i As Integer = 0 To ori.Length - 1 Step 2
            grid(row + ori(i))(col + ori(i + 1)) = -1 : Next
    End Sub

    ' checks an orientation, if possible it is placed, else returns false
    Function TPO(ByVal ori As Integer(), ByVal row As Integer, ByVal col As Integer,
                 ByVal sIdx As Integer) As Boolean
        For i As Integer = 0 To ori.Length - 1 Step 2
            Dim x As Integer = col + ori(i + 1), y As Integer = row + ori(i)
            If x < 0 OrElse x >= nCols OrElse y < 0 OrElse y >= nRows OrElse
                grid(y)(x) <> -1 Then Return False
        Next : grid(row)(col) = sIdx
        For i As Integer = 0 To ori.Length - 1 Step 2
            grid(row + ori(i))(col + ori(i + 1)) = sIdx
        Next : Return True
    End Function

    '!' the following routines expand the seed values into the 63 orientation arrays.
    '   source code space savings comparison:
    '      around 2000 chars for the expansion code, verses about 3000 chars for the integer array defs.
    '   perhaps not worth the savings?

    Sub Unpack(sv As Integer()) ' unpacks a list of seed values into a set of 63 rotated pentominoes
        pens = New List(Of List(Of Integer())) : For Each item In sv
            Dim Gen As New List(Of Integer()), exi As List(Of Integer) = Expand(item),
                fx As Integer() = ToP(exi) : Gen.Add(fx) : For i As Integer = 1 To 7
                If i = 4 Then Mir(exi) Else Rot(exi)
                fx = ToP(exi) : If Not Gen.Exists(Function(Red) TheSame(Red, fx)) Then Gen.Add(ToP(exi))
            Next : pens.Add(Gen) : Next
    End Sub

    ' expands an integer into a set of directions
    Function Expand(i As Integer) As List(Of Integer)
        Expand = {0}.ToList() : For j As Integer = 0 To 3 : Expand.Insert(1, i And 15) : i >>= 4 : Next
    End Function

    ' converts a set of directions to an array of y, x pairs
    Function ToP(p As List(Of Integer)) As Integer()
        Dim tmp As List(Of Integer) = {0}.ToList() : For Each item As Integer In p.Skip(1)
            tmp.Add(tmp.Item(item >> 2) + {1, 8, -1, -8}(item And 3)) : Next
        tmp.Sort() : For i As Integer = tmp.Count - 1 To 0 Step -1 : tmp.Item(i) -= tmp.Item(0) : Next
        Dim res As New List(Of Integer) : For Each item In tmp.Skip(1)
            Dim adj = If((item And 7) > 4, 8, 0)
            res.Add((adj + item) \ 8) : res.Add((item And 7) - adj)
        Next : Return res.ToArray()
    End Function

    ' compares integer arrays for equivalency
    Function TheSame(a As Integer(), b As Integer()) As Boolean
        For i As Integer = 0 To a.Count - 1 : If a(i) <> b(i) Then Return False
        Next : Return True
    End Function

    Sub Rot(ByRef p As List(Of Integer)) ' rotates a set of directions by 90 degrees
        For i As Integer = 0 To p.Count - 1 : p(i) = (p(i) And -4) Or ((p(i) + 1) And 3) : Next
    End Sub

    Sub Mir(ByRef p As List(Of Integer)) ' mirrors a set of directions
        For i As Integer = 0 To p.Count - 1 : p(i) = (p(i) And -4) Or (((p(i) Xor 1) + 1) And 3) : Next
    End Sub

End Module


```

{{out}}
Solution found result ''(typical output)'':

```txt
Y F F █ L L L L
Y Y F F L P P P
Y T F N N N P P
Y T N N █ V V V
T T T W W Z Z V
U U X █ W W Z V
U X X X █ W Z Z
U U X I I I I I
```

Impossible to solve result ''(a somewhat rare occurrence)'':

```txt
no solution for this configuration:
. █ . . . . . .
█ . . █ . . . .
. . . . . . . .
. . . . █ . . .
. . . . . . . .
. . . . . . . .
. . . . . . . .
. . . . . . . .
```

===VB .NET alternative output ''(corner characters)''===
This output may appear better in a command window
[https://tio.run/##rRlLc9u4@e5fgeoQExNKI8nOxvHE7iS20/HUSTySs952eoEoSIJDEVyAlKXNZKbTcw896LCHPfbY4x731@iPpN8H8AWKspN2lRnSBPC938hi1A6k4l@@vJXjNOTEvnp7ZI/A71zMiV7NRzLU5JUmZzOmPEpOSOuHv1y/ufn@3dWHv95etludG4lbr5RiK4/6BhR/0UDeG8DLKOFTrgDyyCfRWYauulrAJExNeeJu9/o@GYUs@ri1XIBNlRhXdj1q@HzH74sVy0yb9HDr0@cSNA5ZwA3waylDzqICNP/OeEJYF5JHRpAroRPv/aR4F0xQ6hPFIoN8AG85L4E152PtsAxUP/Vf9HzSf9HHxwE@nuHjuU8O@i/gcdDFRx8fB/iA3YPnoNNnR999zmw2TEfkLRMgRUHqQxSz4KNnKFJybFmyIlquPFwdztLJJOTDGYu59vol@BupiHJV3yU3khQaLU7mlvAUCnMRpXOu2CjknQGPOUu8di8zPwWXydwFKL/jy8ShJhqoHThU0DMV0Af9Ar7q8WNyLlFG2ARQFLWD@K39jfQI4OwYjhz0V1LG5HYmICKsPABMPYREwawvHu/cKlA5gl1OyFCGC@6BDbuU3Mx45NC8ViJKBlynYVKx3UWouXPsTEYa3LJzq0TCr0TEvVYkCSyliZARmYD6kpnQJJDRRExTxXD5uIWS76AAznA5cdhc6YTPO@eCTSOpExHozjkfpVNQb@dSv0oSFswgYlCCgp0BZ@M/81WGF3GCI1Zc0vWuQKZRNcgp2SfBjEVTrolUYzCjnIAYnMSCB7CGUjEyh0xl7CbnhcCP@o0ldWz27/JFjNzOmdmp@2/hXU1O5fhNiQSVa1zmQ5SIEI69PCV3jViTeVzNGGWmAMyIDyPnuPgrW7sr1u5wDXA04g5meZKGQ1natvgqH@XOXXXHIA5mDl4Ushqd20Z1HApMOBYasumKTFgUrIgyG3rP0So3OW@YAOQUSNo/OhfzOFk5prwAH3NsAAQuI5vnj8sTjsVhX21pBik@KQgB4BwSUetT93PLB1/3BHlJICBbHfjMlSEgcTdpIkPVgn9PyGJ0NannLic2vTOpIq742Du/9QA0R@qqcR@wJqmCQjIRSifg6SnsF@Eso9Dq5U0aBWbF5pDXq@9ZSGJZrSA@sauQdq@LmlYEWFnfqqFenj3JS68J64FhityolLv2g6zqxBiy8Deb1H2ItXB7F1oKu1@la1MnZs4AEyfES7vnULZiIvxTglUj55M@GvBubIOJgNw7mWR1Hoy7nXodp5NK8CgxaXPL/UwQCroFnEmFZG6u33sVFD6qzGjGJxlpdJNERClHmo2oSlZPjAmsELtUgp90t9mqv8F8sYM7k2IqZN@weuGpBIItGZn3wysjW8LggdxjK44@lwsIIZa3XBVGipRiOLR@DNuOAXL/dn0wX3V9j@65/UjhaSfoaMc7nQdodq54NE1mxnmGCY9Jv6G/ARae4mHMFYg4/zK2KKjsSJ1Y63jwETQREccYYoIRowW0TEQkBKq41ZRPOGi2zBSFnousgF73/2vN33Yae0xfjpePZZP/XaWYWJYuZF2lPlm5B6oW2KuF4RJzOnmvsHeCj9OTbObIVlbV7ZXZNp2sXdlSgTH4inrLpjTlhkkWDlsuh@r7HfT0la7nUHMjtEgMTQH6h33Tck1kGMp7LM9KQhGKIGL5MsaRAXdxiCALFqawDMVfmsXvDpysybCz153M12HUkakKONh0DPAwisCTLYAANqnzmCmhoUMtTsOPKVMF@91uF9tCpbO2lltONBJBbD5ZcKUxpYyAVXKwfV5kOjYskTGfVNiKuYJuVJMIsva9VKB3I6Bl7Y@VNiefnxZOYGHDk5odTGkhdHTYtG7ph8GS2QElKQkagn0oI4mci0hy7Q6Tdih7YJ50eh9o0rEo6cVWNP2Jm9pVRVYigWyyFA1dqJnZjKU9RE23s8FkWR9Yb@S1B9iQL6DZeTUee5MlbU6wPfTy53sNdVPA7qGNq7dCWYQmOgcyMV9NnDjEs@KLPFwsQSzt5d7tDaBpQNxDNuf44QMwzeplznOOqbHtMw0FnoLT9KGsbsPEpPXc71wXGAvFDVPazd651usptm6gcmazAXlCoI@FUdqco8WQ0zA8Y9E2MJ3LSHOVeNBAADnA0XuGgIKcnqINatJt1XCYKiHkEt0kE0FRs@jHnZUPyTdm0NbWahUou2kKotTxrr3H5ybarIEyOtzhIO4MP4rY67lWBtTGuvi@BCDj/KAO0qeQUj@Boo58gjcX7aPPdg/VdkC3bi4QwRDyiLfD/3G/7EvBKl2b5E2rUFKnpH1Sfna3yFTGqIb4bswQhrMG0REVG@NUDJNQKdtzSk7JoZG76wIAXaMrD6GeGvwU2v8jaqcju1fF00b8dEc1QojiHughn8MiYfNpmcxtjuc/pgKyLYd5s@ZkWcCzWjM0quXwb2hkWH2qYGgs6AlG@VDxYFfwFSXYNMCQ8l6vBhxaweYY2c/KSHMMjlbkRRcK3VTxSnHZOSs5IsW29ffMGw3YPqTQFRHPrpgxw/r@w9cCmMMfEWEulJJK70yNvwfTdukHwNL7SuaxBSqaHnQve5EBK3GauP4FM33lJoNWLjX2yVim2MFzyJQr04@wIDG1gGh7hC8DHifQM0R8gpeyZWzf1u5EqtFcud/Bqo@XUrd4HwHrTzCA4X1iriV80mrBUDejX5/OLcvtezGGNojpQAiSoNtm9SvIrjJMv5WPa4UuiouOZo04mQtl1h51LoF0ZxiHIvGQd/rt10U5Wuckpkb74bUIaMPS7dK8xbZDRcj04zfPBrTSmjsZETHAQUP/2L7yGyJLUtDy5ug6/emnkHsIYzmi2UVS841xmSbhWA4LUH6zdJ0rhkWwKuEDWRUSCNidmzIB8kIbbY0J9h5z@J6jKNgQg@0xNDMfyC7JXAfIeGOlBbJEm/nCt9sUsqvHckkgw2Z/Zml2hMqs64CVJiqON@N7WcfHmvCNSnxsC9/uGlEA9fcariFbpNffrNeHm/XPm/XfN@tfjzbrf2zW/9ysf9ms/7VZ/3uz/s9m/VvLg3hmNrOxYgTu@iaT1cZdb5QdqJ81/UuJB6uUWT/M1kfFeglzROljvtjkUvhp/@vwy5f/Ag Try it online!]
{{out}}
Solution found result ''(typical output)'':

```txt
┌─────┬─────┬─┬─┐
│ ┌─┐ ├─┐   │ │ │
├─┼─┴─┴─┴─┬─┘ │ │
│ └───┬─┐ │ ┌─┤ │
│ ┌───┼─┼─┴─┘ │ │
├─┘ ┌─┘ └─┐ ┌─┼─┤
│ ┌─┼─┐ ┌─┴─┘ │ │
├─┴─┘ └─┤ ┌───┘ │
└───────┴─┴─────┘
```

Impossible to solve result ''(a somewhat rare occurrence)'':

```txt
no solution for this configuration:
┌─┬─┬─┬─┬───────┐
│ └─┘ └─┘       │
│ ┌─┐           │
├─┼─┘           │
├─┘             │
│               │
│               │
│               │
└───────────────┘
```



## zkl

{{trans|Java}}

```zkl
fcn printResult
   { foreach row in (grid){ row.apply(symbols.get).concat(" ").println() } }
fcn tryPlaceOrientation(o, R,C, shapeIndex){
   foreach ro,co in (o){ r,c:=R+ro, C+co;
      if(r<0 or r>=nRows or c<0 or c>=nCols or grid[r][c]!=-1) return(False);
   }
   grid[R][C]=shapeIndex; foreach ro,co in (o){ grid[R+ro][C+co]=shapeIndex }
   True
}
fcn removeOrientation(o, r,c)
  { grid[r][c]=-1; foreach ro,co in (o){ grid[r+ro][c+co]=-1 } }
fcn solve(pos,numPlaced){
   if(numPlaced==target) return(True);

   row,col:=pos.divr(nCols);
   if(grid[row][col]!=-1) return(solve(pos+1,numPlaced));

   foreach i in (shapes.len()){
      if(not placed[i]){
	 foreach orientation in (shapes[i]){
	    if(not tryPlaceOrientation(orientation, row,col, i)) continue;
	    placed[i]=True;
	    if(solve(pos+1,numPlaced+1)) return(True);
	    removeOrientation(orientation, row,col);
	    placed[i]=False;
	 }
      }
   }
   False
}
```


```zkl
reg [private] // the shapes are made of groups of 4 (r,c) pairs
   _F=T(T(1,-1, 1,0,  1,1,  2,1), T(0,1,  1,-1, 1,0,  2,0),
        T(1,0 , 1,1,  1,2,  2,1), T(1,0,  1,1,  2,-1, 2,0),  T(1,-2, 1,-1, 1,0,  2,-1),
        T(0,1,  1,1,  1,2,  2,1), T(1,-1, 1,0,  1,1,  2,-1), T(1,-1, 1,0,  2,0,  2,1)),
   _I=T(T(0,1,  0,2,  0,3,  0,4), T(1,0,  2,0,  3,0,  4,0)),
   _L=T(T(1,0,  1,1,  1,2,  1,3), T(1,0,  2,0,  3,-1, 3,0),
        T(0,1,  0,2,  0,3,  1,3), T(0,1,  1,0,  2,0,  3,0),  T(0,1,  1,1,  2,1,  3,1),
        T(0,1,  0,2,  0,3,  1,0), T(1,0,  2,0,  3,0,  3,1),  T(1,-3, 1,-2, 1,-1, 1,0)),
   _N=T(T(0,1,  1,-2, 1,-1, 1,0), T(1,0,  1,1,  2,1,  3,1),
        T(0,1,  0,2,  1,-1, 1,0), T(1,0,  2,0,  2,1,  3,1),  T(0,1,  1,1,  1,2,  1,3),
        T(1,0,  2,-1, 2,0,  3,-1),T(0,1,  0,2,  1,2,  1,3),  T(1,-1, 1,0,  2,-1, 3,-1)),
   _P=T(T(0,1,  1,0,  1,1,  2,1), T(0,1,  0,2,  1,0,  1,1),
        T(1,0,  1,1,  2,0,  2,1), T(0,1,  1,-1, 1,0,  1,1),  T(0,1,  1,0,  1,1,  1,2),
        T(1,-1, 1,0,  2,-1, 2,0), T(0,1,  0,2,  1,1,  1,2),  T(0,1,  1,0,  1,1,  2,0)),
   _T=T(T(0,1,  0,2,  1,1,  2,1), T(1,-2, 1,-1, 1,0,  2,0),
        T(1,0,  2,-1, 2,0,  2,1), T(1,0,  1,1,  1,2,  2,0)),
   _U=T(T(0,1,  0,2,  1,0,  1,2), T(0,1,  1,1,  2,0,  2,1),
        T(0,2,  1,0,  1,1,  1,2), T(0,1,  1,0,  2,0,  2,1)),
   _V=T(T(1,0,  2,0,  2,1,  2,2), T(0,1,  0,2,  1,0,  2,0),
        T(1,0,  2,-2, 2,-1, 2,0), T(0,1,  0,2,  1,2,  2,2)),
   _W=T(T(1,0,  1,1,  2,1,  2,2), T(1,-1, 1,0,  2,-2, 2,-1),
        T(0,1,  1,1,  1,2,  2,2), T(0,1,  1,-1, 1,0,  2,-1)),
   _X=T(T(1,-1, 1,0,  1,1,  2,0)),
   _Y=T(T(1,-2, 1,-1, 1,0,  1,1), T(1,-1, 1,0,  2,0,  3,0),
        T(0,1,  0,2,  0,3,  1,1), T(1,0,  2,0,  2,1,  3,0),  T(0,1,  0,2,  0,3,  1,2),
        T(1,0,  1,1,  2,0,  3,0), T(1,-1, 1,0,  1,1,  1,2),  T(1,0,  2,-1, 2,0,  3,0)),
   _Z=T(T(0,1,  1,0,  2,-1, 2,0), T(1,0,  1,1,  1,2,  2,2),
        T(0,1,  1,1,  2,1,  2,2), T(1,-2, 1,-1, 1,0,  2,-2));

const nRows=8, nCols=8, target=12, blank=12;
var [const]
   grid   = nRows.pump(List(),nCols.pump(List(),-1).copy),
   placed = target.pump(List(),False),

   symbols="FILNPTUVWXYZ-".split(""),
   shapes=T(_F,_I,_L,_N,_P,_T,_U,_V,_W,_X,_Y,_Z) // ((a,b, c,d))-->(((a,b),(c,d)))
       .pump(List,List("pump",List,List("pump",List,Void.Read,T.create)));
```


```zkl
foreach r,c in ([0..nRows-1].walk().shuffle().zip([0..nCols-1].walk().shuffle())[0,4])
   { grid[r][c]=blank }  // make sure 4 unique random spots
if(solve(0,0)) printResult();
else	       println("No solution");
```

{{out}}

```txt

F Y Y Y Y U U U
F F F Y - U X U
I F W W L X X X
I W W N L - X T
I W N N L T T T
I V N L L Z Z T
I V N P P P Z -
- V V V P P Z Z

```

