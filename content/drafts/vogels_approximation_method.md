+++
title = "Vogel's approximation method"
description = ""
date = 2019-05-18T18:17:23Z
aliases = []
[extra]
id = 16103
[taxonomies]
categories = []
tags = []
+++

{{task}}
[http://mcajournal.cbu.edu.tr/articleinpress/articleinpress_955.pdf Vogel's Approximation Method (VAM)] is a technique for finding a good initial feasible solution to an allocation problem.

The powers that be have identified 5 tasks that need to be solved urgently. Being imaginative chaps, they have called them “A”, “B”, “C”, “D”, and “E”. They estimate that:
* A will require 30 hours of work,
* B will require 20 hours of work,
* C will require 70 hours of work,
* D will require 30 hours of work, and
* E will require 60 hours of work.

They have identified 4 contractors willing to do the work, called “W”, “X”, “Y”, and “Z”.
* W has 50 hours available to commit to working,
* X has 60 hours available,
* Y has 50 hours available, and
* Z has 50 hours available.
The cost per hour for each contractor for each task is summarized by the following table:


```txt

   A  B  C  D  E
W 16 16 13 22 17
X 14 14 13 19 15
Y 19 19 20 23 50
Z 50 12 50 15 11

```


The task is to use VAM to allocate contractors to tasks. <!--VAM is fun to program (using my definition of fun, which is replaced by torture by some people). -->It scales to large problems, so ideally keep sorts out of the iterative cycle. It works as follows:

:Step 1: Balance the given transportation problem if either (total supply>total demand) or (total supply<total demand)
:Step 2: Determine the penalty cost for each row and column by subtracting the lowest cell cost in the row or column from the next lowest cell cost in the same row or column.
:Step 3: Select the row or column with the highest penalty cost (breaking ties arbitrarily or choosing the lowest-cost cell).
:Step 4: Allocate as much as possible to the feasible cell with the lowest transportation cost in the row or column with the highest penalty cost.
:Step 5: Repeat steps 2, 3 and 4 until all requirements have been meet.
:Step 6: Compute total transportation cost for the feasible allocations.

For this task assume that the model is balanced.

For each task and contractor (row and column above) calculating the difference between the smallest two values produces:

```txt

        A       B       C       D       E       W       X       Y       Z
1       2       2       0       4       4       3       1       0       1   E-Z(50)

```


Determine the largest difference (D or E above). In the case of ties I shall choose the one with the lowest price (in this case E because the lowest price for D is Z=15, whereas for E it is Z=11). For your choice determine the minimum cost (chosen E above so Z=11 is chosen now). Allocate as much as possible from Z to E (50 in this case limited by Z's supply).
Adjust the supply and demand accordingly. If demand or supply becomes 0 for a given task or contractor it plays no further part. In this case Z is out of it. If you choose arbitrarily, and chose D see [http://rosettacode.org/mw/index.php?title=VAM&oldid=167195 here] for the working.

Repeat until all supply and demand is met:

```txt

2       2       2       0       3       2       3       1       0       -   C-W(50)
3       5       5       7       4      35       -       1       0       -   E-X(10)
4       5       5       7       4       -       -       1       0       -   C-X(20)
5       5       5       -       4       -       -       0       0       -   A-X(30)
6       -      19       -      23       -       -       -       4       -   D-Y(30)
        -       -       -       -       -       -       -       -       -   B-Y(20)

```

Finally calculate the cost of your solution. In the example given it is £3100:

```txt

   A  B  C  D  E
W       50
X 30    20    10
Y    20    30
Z             50

```


The optimal solution determined by [[wp:GNU Linear Programming Kit|GLPK]] is £3100:

```txt

   A  B  C  D  E
W       50
X 10 20 20    10
Y 20       30
Z             50

```


;Cf.
* [[Transportation_problem|Transportation problem]]



## C

{{trans|Kotlin}}

```c
#include <stdio.h>
#include <limits.h>

#define TRUE 1
#define FALSE 0
#define N_ROWS 4
#define N_COLS 5

typedef int bool;

int supply[N_ROWS] = { 50, 60, 50, 50 };
int demand[N_COLS] = { 30, 20, 70, 30, 60 };

int costs[N_ROWS][N_COLS] = {
    { 16, 16, 13, 22, 17 },
    { 14, 14, 13, 19, 15 },
    { 19, 19, 20, 23, 50 },
    { 50, 12, 50, 15, 11 }
};

bool row_done[N_ROWS] = { FALSE };
bool col_done[N_COLS] = { FALSE };

void diff(int j, int len, bool is_row, int res[3]) {
    int i, c, min1 = INT_MAX, min2 = min1, min_p = -1;
    for (i = 0; i < len; ++i) {
        if((is_row) ? col_done[i] : row_done[i]) continue;
        c = (is_row) ? costs[j][i] : costs[i][j];
        if (c < min1) {
            min2 = min1;
            min1 = c;
            min_p = i;
        }
        else if (c < min2) min2 = c;
    }
    res[0] = min2 - min1; res[1] = min1; res[2] = min_p;
}

void max_penalty(int len1, int len2, bool is_row, int res[4]) {
    int i, pc = -1, pm = -1, mc = -1, md = INT_MIN;
    int res2[3];

    for (i = 0; i < len1; ++i) {
        if((is_row) ? row_done[i] : col_done[i]) continue;
        diff(i, len2, is_row, res2);
        if (res2[0] > md) {
            md = res2[0];  /* max diff */
            pm = i;        /* pos of max diff */
            mc = res2[1];  /* min cost */
            pc = res2[2];  /* pos of min cost */
        }
    }

    if (is_row) {
        res[0] = pm; res[1] = pc;
    }
    else {
        res[0] = pc; res[1] = pm;
    }
    res[2] = mc; res[3] = md;
}

void next_cell(int res[4]) {
    int i, res1[4], res2[4];
    max_penalty(N_ROWS, N_COLS, TRUE, res1);
    max_penalty(N_COLS, N_ROWS, FALSE, res2);

    if (res1[3] == res2[3]) {
        if (res1[2] < res2[2])
            for (i = 0; i < 4; ++i) res[i] = res1[i];
        else
            for (i = 0; i < 4; ++i) res[i] = res2[i];
        return;
    }
    if (res1[3] > res2[3])
        for (i = 0; i < 4; ++i) res[i] = res2[i];
    else
        for (i = 0; i < 4; ++i) res[i] = res1[i];
}

int main() {
    int i, j, r, c, q, supply_left = 0, total_cost = 0, cell[4];
    int results[N_ROWS][N_COLS] = { 0 };

    for (i = 0; i < N_ROWS; ++i) supply_left += supply[i];
    while (supply_left > 0) {
        next_cell(cell);
        r = cell[0];
        c = cell[1];
        q = (demand[c] <= supply[r]) ? demand[c] : supply[r];
        demand[c] -= q;
        if (!demand[c]) col_done[c] = TRUE;
        supply[r] -= q;
        if (!supply[r]) row_done[r] = TRUE;
        results[r][c] = q;
        supply_left -= q;
        total_cost += q * costs[r][c];
    }

    printf("    A   B   C   D   E\n");
    for (i = 0; i < N_ROWS; ++i) {
        printf("%c", 'W' + i);
        for (j = 0; j < N_COLS; ++j) printf("  %2d", results[i][j]);
        printf("\n");
    }
    printf("\nTotal cost = %d\n", total_cost);
    return 0;
}
```


{{output}}

```txt

    A   B   C   D   E
W   0   0  50   0   0
X  30   0  20   0  10
Y   0  20   0  30   0
Z   0   0   0   0  50

Total cost = 3100

```


If the program is changed to this (to accomodate the second Ruby example):

```go>#include <stdio.h

#include <limits.h>

#define TRUE 1
#define FALSE 0
#define N_ROWS 5
#define N_COLS 5

typedef int bool;

int supply[N_ROWS] = { 461, 277, 356, 488,  393 };
int demand[N_COLS] = { 278,  60, 461, 116, 1060 };

int costs[N_ROWS][N_COLS] = {
    { 46,  74,  9, 28, 99 },
    { 12,  75,  6, 36, 48 },
    { 35, 199,  4,  5, 71 },
    { 61,  81, 44, 88,  9 },
    { 85,  60, 14, 25, 79 }
};

// etc

int main() {
    // etc

    printf("     A    B    C    D    E\n");
    for (i = 0; i < N_ROWS; ++i) {
        printf("%c", 'V' + i);
        for (j = 0; j < N_COLS; ++j) printf("  %3d", results[i][j]);
        printf("\n");
    }
    printf("\nTotal cost = %d\n", total_cost);
    return 0;
}
```


then the output, which agrees with the Phix output but not with the Ruby output itself is:

```txt

     A    B    C    D    E
V    0    0  461    0    0
W  277    0    0    0    0
X    1    0    0    0  355
Y    0    0    0    0  488
Z    0   60    0  116  217

Total cost = 60748

```



## D

Strongly typed version (but K is not divided in Task and Contractor types to keep code simpler).
{{trans|Python}}

```d
void main() {
    import std.stdio, std.string, std.algorithm, std.range;

    enum K { A, B, C, D, E,  X, Y, Z, W }
    immutable int[K][K] costs = cast() //**
        [K.W: [K.A: 16, K.B: 16, K.C: 13, K.D: 22, K.E: 17],
         K.X: [K.A: 14, K.B: 14, K.C: 13, K.D: 19, K.E: 15],
         K.Y: [K.A: 19, K.B: 19, K.C: 20, K.D: 23, K.E: 50],
         K.Z: [K.A: 50, K.B: 12, K.C: 50, K.D: 15, K.E: 11]];
    int[K] demand, supply;
    with (K)
        demand = [A: 30, B: 20, C: 70, D: 30, E: 60],
        supply = [W: 50, X: 60, Y: 50, Z: 50];

    immutable cols = demand.keys.sort().release;
    auto res = costs.byKey.zip((int[K]).init.repeat).assocArray;
    K[][K] g;
    foreach (immutable x; supply.byKey)
        g[x] = costs[x].keys.schwartzSort!(k => cast()costs[x][k]) //**
               .release;
    foreach (immutable x; demand.byKey)
        g[x] = costs.keys.schwartzSort!(k=> cast()costs[k][x]).release;

    while (g.length) {
        int[K] d, s;
        foreach (immutable x; demand.byKey)
            d[x] = g[x].length > 1 ?
                   costs[g[x][1]][x] - costs[g[x][0]][x] :
                   costs[g[x][0]][x];
        foreach (immutable x; supply.byKey)
            s[x] = g[x].length > 1 ?
                   costs[x][g[x][1]] - costs[x][g[x][0]] :
                   costs[x][g[x][0]];
        auto f = d.keys.minPos!((a,b) => d[a] > d[b])[0];
        auto t = s.keys.minPos!((a,b) => s[a] > s[b])[0];
        if (d[f] > s[t]) {
            t = f;
            f = g[f][0];
        } else {
            f = t;
            t = g[t][0];
        }
        immutable v = min(supply[f], demand[t]);
        res[f][t] += v;
        demand[t] -= v;
        if (demand[t] == 0) {
            foreach (immutable k, immutable n; supply)
                if (n != 0)
                    g[k] = g[k].remove!(c => c == t);
            g.remove(t);
            demand.remove(t);
        }
        supply[f] -= v;
        if (supply[f] == 0) {
            foreach (immutable k, immutable n; demand)
                if (n != 0)
                    g[k] = g[k].remove!(c => c == f);
            g.remove(f);
            supply.remove(f);
        }
    }

    writefln("%-(\t%s%)", cols);
    auto cost = 0;
    foreach (immutable c; costs.keys.sort().release) {
        write(c, '\t');
        foreach (immutable n; cols) {
            if (n in res[c]) {
                immutable y = res[c][n];
                if (y != 0) {
                    y.write;
                    cost += y * costs[c][n];
                }
            }
            '\t'.write;
        }
        writeln;
    }
    writeln("\nTotal Cost = ", cost);
}
```

{{out}}

```txt
    A   B   C   D   E
W           50
X           20      40
Y   30  20
Z               30  20

Total Cost = 3130
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "math"
)

var supply = []int{50, 60, 50, 50}
var demand = []int{30, 20, 70, 30, 60}

var costs = make([][]int, 4)

var nRows = len(supply)
var nCols = len(demand)

var rowDone = make([]bool, nRows)
var colDone = make([]bool, nCols)
var results = make([][]int, nRows)

func init() {
    costs[0] = []int{16, 16, 13, 22, 17}
    costs[1] = []int{14, 14, 13, 19, 15}
    costs[2] = []int{19, 19, 20, 23, 50}
    costs[3] = []int{50, 12, 50, 15, 11}

    for i := 0; i < len(results); i++ {
        results[i] = make([]int, nCols)
    }
}

func nextCell() []int {
    res1 := maxPenalty(nRows, nCols, true)
    res2 := maxPenalty(nCols, nRows, false)
    switch {
    case res1[3] == res2[3]:
        if res1[2] < res2[2] {
            return res1
        } else {
            return res2
        }
    case res1[3] > res2[3]:
        return res2
    default:
        return res1
    }
}

func diff(j, l int, isRow bool) []int {
    min1 := math.MaxInt32
    min2 := min1
    minP := -1
    for i := 0; i < l; i++ {
        var done bool
        if isRow {
            done = colDone[i]
        } else {
            done = rowDone[i]
        }
        if done {
            continue
        }
        var c int
        if isRow {
            c = costs[j][i]
        } else {
            c = costs[i][j]
        }
        if c < min1 {
            min2, min1, minP = min1, c, i
        } else if c < min2 {
            min2 = c
        }
    }
    return []int{min2 - min1, min1, minP}
}

func maxPenalty(len1, len2 int, isRow bool) []int {
    md := math.MinInt32
    pc, pm, mc := -1, -1, -1
    for i := 0; i < len1; i++ {
        var done bool
        if isRow {
            done = rowDone[i]
        } else {
            done = colDone[i]
        }
        if done {
            continue
        }
        res := diff(i, len2, isRow)
        if res[0] > md {
            md = res[0]  // max diff
            pm = i       // pos of max diff
            mc = res[1]  // min cost
            pc = res[2]  // pos of min cost
        }
    }
    if isRow {
        return []int{pm, pc, mc, md}
    }
    return []int{pc, pm, mc, md}
}

func main() {
    supplyLeft := 0
    for i := 0; i < len(supply); i++ {
        supplyLeft += supply[i]
    }
    totalCost := 0
    for supplyLeft > 0 {
        cell := nextCell()
        r, c := cell[0], cell[1]
        q := demand[c]
        if q > supply[r] {
            q = supply[r]
        }
        demand[c] -= q
        if demand[c] == 0 {
            colDone[c] = true
        }
        supply[r] -= q
        if supply[r] == 0 {
            rowDone[r] = true
        }
        results[r][c] = q
        supplyLeft -= q
        totalCost += q * costs[r][c]
    }

    fmt.Println("    A   B   C   D   E")
    for i, result := range results {
        fmt.Printf("%c", 'W' + i)
        for _, item := range result {
            fmt.Printf("  %2d", item)
        }
        fmt.Println()
    }
    fmt.Println("\nTotal cost =", totalCost)
}
```


{{out}}

```txt

    A   B   C   D   E
W   0   0  50   0   0
X  30   0  20   0  10
Y   0  20   0  30   0
Z   0   0   0   0  50

Total cost = 3100

```


If the program is changed as follows to accomodate the second Ruby example:

```go
package main

import (
    "fmt"
    "math"
)

var supply = []int{461, 277, 356, 488, 393}
var demand = []int{278, 60, 461, 116, 1060}

var costs = make([][]int, nRows)

var nRows = len(supply)
var nCols = len(demand)

var rowDone = make([]bool, nRows)
var colDone = make([]bool, nCols)
var results = make([][]int, nRows)

func init() {
    costs[0] = []int{46, 74, 9, 28, 99}
    costs[1] = []int{12, 75, 6, 36, 48}
    costs[2] = []int{35, 199, 4, 5, 71}
    costs[3] = []int{61, 81, 44, 88, 9}
    costs[4] = []int{85, 60, 14, 25, 79}

    for i := 0; i < len(results); i++ {
        results[i] = make([]int, nCols)
    }
}

// etc

func main() {
    // etc

    fmt.Println("     A    B    C    D    E")
    for i, result := range results {
        fmt.Printf("%c", 'V'+i)
        for _, item := range result {
            fmt.Printf("  %3d", item)
        }
        fmt.Println()
    }
    fmt.Println("\nTotal cost =", totalCost)
}
```


then the output, which agrees with the C and Phix output but not with the Ruby output itself, is:

```txt

     A    B    C    D    E
V    0    0  461    0    0
W  277    0    0    0    0
X    1    0    0    0  355
Y    0    0    0    0  488
Z    0   60    0  116  217

Total cost = 60748

```



## J


Implementation:


```J
vam=:1 :0
:
  exceeding=. 0 <. -&(+/)
  D=. x,y exceeding x NB. x: demands
  S=. y,x exceeding y NB. y: sources
  C=. (m,.0),0        NB. m: costs
  B=. 1+>./,C         NB. bigger than biggest cost
  mincost=. <./@-.&0  NB. smallest non-zero cost
  penalty=. |@(B * 2 -/@{. /:~ -. 0:)"1 - mincost"1
  R=. C*0
  while. 0 < +/D,S do.
    pS=. penalty C
    pD=. penalty |:C
    if. pS >&(>./) pD do.
      row=. (i. >./) pS
      col=. (i. mincost) row { C
    else.
      col=. (i. >./) pD
      row=. (i. mincost) col {"1 C
    end.
    n=. (row{S) <. col{D
    S=. (n-~row{S) row} S
    D=. (n-~col{D) col} D
    C=. C * S *&*/ D
    R=. n (<row,col)} R
  end.
  _1 _1 }. R
)
```


Note that for our penalty we are using the difference between the two smallest relevant costs multiplied by 1 larger than the highest represented cost and we subtract from that multiple the smallest relevant cost. This gives us the tiebreaker mechanism currently specified for this task.

Task example:


```J
demand=: 30 20 70 30 60
src=: 50 60 50 50
cost=: 16 16 13 22 17,14 14 13 19 15,19 19 20 23 50,:50 12 50 15 11

   demand cost vam src
 0  0 50  0  0
30  0 20  0 10
 0 20  0 30  0
 0  0  0  0 50
```



## Java

{{works with|Java|8}}

```java
import java.util.Arrays;
import static java.util.Arrays.stream;
import java.util.concurrent.*;

public class VogelsApproximationMethod {

    final static int[] demand = {30, 20, 70, 30, 60};
    final static int[] supply = {50, 60, 50, 50};
    final static int[][] costs = {{16, 16, 13, 22, 17}, {14, 14, 13, 19, 15},
    {19, 19, 20, 23, 50}, {50, 12, 50, 15, 11}};

    final static int nRows = supply.length;
    final static int nCols = demand.length;

    static boolean[] rowDone = new boolean[nRows];
    static boolean[] colDone = new boolean[nCols];
    static int[][] result = new int[nRows][nCols];

    static ExecutorService es = Executors.newFixedThreadPool(2);

    public static void main(String[] args) throws Exception {
        int supplyLeft = stream(supply).sum();
        int totalCost = 0;

        while (supplyLeft > 0) {
            int[] cell = nextCell();
            int r = cell[0];
            int c = cell[1];

            int quantity = Math.min(demand[c], supply[r]);
            demand[c] -= quantity;
            if (demand[c] == 0)
                colDone[c] = true;

            supply[r] -= quantity;
            if (supply[r] == 0)
                rowDone[r] = true;

            result[r][c] = quantity;
            supplyLeft -= quantity;

            totalCost += quantity * costs[r][c];
        }

        stream(result).forEach(a -> System.out.println(Arrays.toString(a)));
        System.out.println("Total cost: " + totalCost);

        es.shutdown();
    }

    static int[] nextCell() throws Exception {
        Future<int[]> f1 = es.submit(() -> maxPenalty(nRows, nCols, true));
        Future<int[]> f2 = es.submit(() -> maxPenalty(nCols, nRows, false));

        int[] res1 = f1.get();
        int[] res2 = f2.get();

        if (res1[3] == res2[3])
            return res1[2] < res2[2] ? res1 : res2;

        return (res1[3] > res2[3]) ? res2 : res1;
    }

    static int[] diff(int j, int len, boolean isRow) {
        int min1 = Integer.MAX_VALUE, min2 = Integer.MAX_VALUE;
        int minP = -1;
        for (int i = 0; i < len; i++) {
            if (isRow ? colDone[i] : rowDone[i])
                continue;
            int c = isRow ? costs[j][i] : costs[i][j];
            if (c < min1) {
                min2 = min1;
                min1 = c;
                minP = i;
            } else if (c < min2)
                min2 = c;
        }
        return new int[]{min2 - min1, min1, minP};
    }

    static int[] maxPenalty(int len1, int len2, boolean isRow) {
        int md = Integer.MIN_VALUE;
        int pc = -1, pm = -1, mc = -1;
        for (int i = 0; i < len1; i++) {
            if (isRow ? rowDone[i] : colDone[i])
                continue;
            int[] res = diff(i, len2, isRow);
            if (res[0] > md) {
                md = res[0];  // max diff
                pm = i;       // pos of max diff
                mc = res[1];  // min cost
                pc = res[2];  // pos of min cost
            }
        }
        return isRow ? new int[]{pm, pc, mc, md} : new int[]{pc, pm, mc, md};
    }
}
```



```txt
[0, 0, 50, 0, 0]
[30, 0, 20, 0, 10]
[0, 20, 0, 30, 0]
[0, 0, 0, 0, 50]
Total cost: 3100
```



## Julia

This solution is designed to scale well to large numbers of suppliers and customers.  The opportunity cost matrix is sorted only once, and penalties are recalculated only when the relevant resources are exhausted.  The solution is stored in a [http://docs.julialang.org/en/release-0.3/manual/arrays/#sparse-matrices sparse matrix], because the number of components to a solution is less than s+c (suppliers + customers) but the size of the matrix is s*c.

This solution does not impose the requirement that the problem be balanced.  <code>vogel</code> will iterate until either supply or demand is exhausted and provide a low-cost result even when the problem is unbalanced, whether this result is a good solution is left for the user to decide.  The function <code>isbalanced</code> can be used to test whether a given problem is balanced.

'''Types'''

The immutable type <code>TProblem</code> stores the problem's parameters.  It includes permutation matrices that allow the rows and columns of the total opportunity cost matrix to be sorted as needed.

<code>Resource</code> stores the currently available quantity of a given supply or demand as well as its penalty, cost, and some meta-data.  <code>isavailable</code> indicates whether any of the given resource remains.  <code>isless</code> is designed to make the currently most usable resource appear as a maximum compared to other resources.

```Julia

immutable TProblem{T<:Integer,U<:String}
    sd::Array{Array{T,1},1}
    toc::Array{T,2}
    labels::Array{Array{U,1},1}
    tsort::Array{Array{T,2}, 1}
end

function TProblem{T<:Integer,U<:String}(s::Array{T,1},
                                        d::Array{T,1},
                                        toc::Array{T,2},
                                        slab::Array{U,1},
                                        dlab::Array{U,1})
    scnt = length(s)
    dcnt = length(d)
    size(toc) = (scnt,dcnt) || error("Supply, Demand, TOC Size Mismatch")
    length(slab) == scnt || error("Supply Label Size Labels")
    length(dlab) == dcnt || error("Demand Label Size Labels")
    0 <= minimum(s) || error("Negative Supply Value")
    0 <= minimum(d) || error("Negative Demand Value")
    sd = Array{T,1}[]
    push!(sd, s)
    push!(sd, d)
    labels = Array{U,1}[]
    push!(labels, slab)
    push!(labels, dlab)
    tsort = Array{T,2}[]
    push!(tsort, mapslices(sortperm, toc, 2))
    push!(tsort, mapslices(sortperm, toc, 1))
    TProblem(sd, toc, labels, tsort)
end
isbalanced(tp::TProblem) = sum(tp.sd[1]) == sum(tp.sd[2])

type Resource{T<:Integer}
    dim::T
    i::T
    quant::T
    l::T
    m::T
    p::T
    q::T
end
function Resource{T<:Integer}(dim::T, i::T, quant::T)
    zed = zero(T)
    Resource(dim, i, quant, zed, zed, zed, zed)
end

isavailable(r::Resource) = 0 < r.quant
Base.isless(a::Resource, b::Resource) = a.p < b.p || (a.p == b.p && b.q < a.q)

```


'''Functions'''

<code>penalize!</code> updates the penalty, cost and some meta-data of lists of supplies and demands.  It short-circuits to avoid recalculating these values when the relevant resources remain available.  Sorting is provided by the permutation matrices in <code>TProblem</code>.

<code>vogel</code> implements Vogel's approximation method on a <code>TProblem</code>.  It is somewhat straightforward, given the types and <code>penalize!</code>.

```Julia

function penalize!{T<:Integer,U<:String}(sd::Array{Array{Resource{T},1},1},
                                         tp::TProblem{T,U})
    avail = BitArray{1}[]
    for dim in 2:-1:1
        push!(avail, bitpack(map(isavailable, sd[dim])))
    end
    for dim in 1:2, r in sd[dim]
        if r.quant == 0
            r.l = r.m = r.p = r.q = 0
            continue
        end
        r.l == 0 || !avail[dim][r.l] || !avail[dim][r.m] || continue
        rsort = filter(x->avail[dim][x], vec(slicedim(tp.tsort[dim],dim,r.i)))
        rcost = vec(slicedim(tp.toc, dim, r.i))[rsort]
        if length(rsort) == 1
            r.l = r.m = rsort[1]
            r.p = r.q = rcost[1]
        else
            r.l, r.m = rsort[1:2]
            r.p = rcost[2] - rcost[1]
            r.q = rcost[1]
        end
    end
    nothing
end

function vogel{T<:Integer,U<:String}(tp::TProblem{T,U})
    sdcnt = collect(size(tp.toc))
    sol = spzeros(T, sdcnt[1], sdcnt[2])
    sd = Array{Resource{T},1}[]
    for dim in 1:2
        push!(sd, [Resource(dim, i, tp.sd[dim][i]) for i in 1:sdcnt[dim]])
    end
    while any(map(isavailable, sd[1])) && any(map(isavailable, sd[2]))
        penalize!(sd, tp)
        a = maximum([sd[1], sd[2]])
        b = sd[rem1(a.dim+1,2)][a.l]
        if a.dim == 2 # swap to make a supply and b demand
            a, b = b, a
        end
        expend = min(a.quant, b.quant)
        sol[a.i, b.i] = expend
        a.quant -= expend
        b.quant -= expend
    end
    return sol
end

```


'''Main'''

```Julia

sup = [50, 60, 50, 50]
slab = ["W", "X", "Y", "Z"]
dem = [30, 20, 70, 30, 60]
dlab = ["A", "B", "C", "D", "E"]
c = [16 16 13 22 17;
     14 14 13 19 15;
     19 19 20 23 50;
     50 12 50 15 11]

tp = TProblem(sup, dem, c, slab, dlab)
sol = vogel(tp)
cost = sum(tp.toc .* sol)

println("The solution is:")
print("        ")
for s in tp.labels[2]
    print(@sprintf "%4s" s)
end
println()
for i in 1:size(tp.toc)[1]
    print(@sprintf "    %4s" tp.labels[1][i])
    for j in 1:size(tp.toc)[2]
        print(@sprintf "%4d" sol[i,j])
    end
println()
end
println("The total cost is:  ", cost)

```


{{out}}

```txt

The solution is:
           A   B   C   D   E
       W   0   0  50   0   0
       X  10  20  20   0  10
       Y  20   0   0  30   0
       Z   0   0   0   0  50
The total cost is:  3100

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.3

val supply = intArrayOf(50, 60, 50, 50)
val demand = intArrayOf(30, 20, 70, 30, 60)

val costs = arrayOf(
    intArrayOf(16, 16, 13, 22, 17),
    intArrayOf(14, 14, 13, 19, 15),
    intArrayOf(19, 19, 20, 23, 50),
    intArrayOf(50, 12, 50, 15, 11)
)

val nRows = supply.size
val nCols = demand.size

val rowDone = BooleanArray(nRows)
val colDone = BooleanArray(nCols)
val results = Array(nRows) { IntArray(nCols) }

fun nextCell(): IntArray {
    val res1 = maxPenalty(nRows, nCols, true)
    val res2 = maxPenalty(nCols, nRows, false)
    if (res1[3] == res2[3])
        return if (res1[2] < res2[2]) res1 else res2
    return if (res1[3] > res2[3]) res2 else res1
}

fun diff(j: Int, len: Int, isRow: Boolean): IntArray {
    var min1 = Int.MAX_VALUE
    var min2 = min1
    var minP = -1
    for (i in 0 until len) {
        val done = if (isRow) colDone[i] else rowDone[i]
        if (done) continue
        val c = if (isRow) costs[j][i] else costs[i][j]
        if (c < min1) {
            min2 = min1
            min1 = c
            minP = i
        }
        else if (c < min2) min2 = c
    }
    return intArrayOf(min2 - min1, min1, minP)
}

fun maxPenalty(len1: Int, len2: Int, isRow: Boolean): IntArray {
    var md = Int.MIN_VALUE
    var pc = -1
    var pm = -1
    var mc = -1
    for (i in 0 until len1) {
        val done = if (isRow) rowDone[i] else colDone[i]
        if (done) continue
        val res = diff(i, len2, isRow)
        if (res[0] > md) {
            md = res[0]  // max diff
            pm = i       // pos of max diff
            mc = res[1]  // min cost
            pc = res[2]  // pos of min cost
        }
    }
    return if (isRow) intArrayOf(pm, pc, mc, md) else
                      intArrayOf(pc, pm, mc, md)
}

fun main(args: Array<String>) {
    var supplyLeft = supply.sum()
    var totalCost = 0
    while (supplyLeft > 0) {
        val cell = nextCell()
        val r = cell[0]
        val c = cell[1]
        val q = minOf(demand[c], supply[r])
        demand[c] -= q
        if (demand[c] == 0) colDone[c] = true
        supply[r] -= q
        if (supply[r] == 0) rowDone[r] = true
        results[r][c] = q
        supplyLeft -= q
        totalCost += q * costs[r][c]
    }

    println("    A   B   C   D   E")
    for ((i, result) in results.withIndex()) {
        print(('W'.toInt() + i).toChar())
        for (item in result) print("  %2d".format(item))
        println()
    }
    println("\nTotal Cost = $totalCost")
}
```


{{out}}

```txt

    A   B   C   D   E
W   0   0  50   0   0
X  30   0  20   0  10
Y   0  20   0  30   0
Z   0   0   0   0  50

Total Cost = 3100

```



## Perl 6

{{works with|Rakudo|2019.03.1}}
{{trans|Sidef}}


```perl6
my  %costs =
    :W{:16A, :16B, :13C, :22D, :17E},
    :X{:14A, :14B, :13C, :19D, :15E},
    :Y{:19A, :19B, :20C, :23D, :50E},
    :Z{:50A, :12B, :50C, :15D, :11E};

my %demand = :30A, :20B, :70C, :30D, :60E;
my %supply = :50W, :60X, :50Y, :50Z;

my @cols = %demand.keys.sort;

my %res;
my %g = (|%supply.keys.map: -> $x { $x => [%costs{$x}.sort(*.value)».key]}),
   (|%demand.keys.map: -> $x { $x => [%costs.keys.sort({%costs{$_}{$x}})]});

while (+%g) {
    my @d = %demand.keys.map: -> $x
      {[$x, my $z = %costs{%g{$x}[0]}{$x},%g{$x}[1] ?? %costs{%g{$x}[1]}{$x} - $z !! $z]}

    my @s = %supply.keys.map: -> $x
      {[$x, my $z = %costs{$x}{%g{$x}[0]},%g{$x}[1] ?? %costs{$x}{%g{$x}[1]} - $z !! $z]}

    @d = |@d.grep({ (.[2] == max @d».[2]) }).&min: :by(*.[1]);
    @s = |@s.grep({ (.[2] == max @s».[2]) }).&min: :by(*.[1]);

    my ($t, $f) = @d[2] == @s[2] ?? (@s[1],@d[1]) !! (@d[2],@s[2]);
    my ($d, $s) = $t > $f ?? (@d[0],%g{@d[0]}[0]) !! (%g{@s[0]}[0], @s[0]);

    my $v = %supply{$s} min %demand{$d};

    %res{$s}{$d} += $v;
    %demand{$d} -= $v;

    if (%demand{$d} == 0) {
        %supply.grep( *.value != 0 )».key.map: -> $v
          { %g{$v}.splice((%g{$v}.first: * eq $d, :k),1) };
        %g{$d}:delete;
        %demand{$d}:delete;
    }

    %supply{$s} -= $v;

    if (%supply{$s} == 0) {
        %demand.grep( *.value != 0 )».key.map: -> $v
          { %g{$v}.splice((%g{$v}.first: * eq $s, :k),1) };
        %g{$s}:delete;
        %supply{$s}:delete;
    }
}

say join "\t", flat '', @cols;
my $total;
for %costs.keys.sort -> $g {
    print "$g\t";
    for @cols -> $col {
        print %res{$g}{$col} // '-', "\t";
        $total += (%res{$g}{$col} // 0) * %costs{$g}{$col};
    }
    print "\n";
}
say "\nTotal cost: $total";
```

{{out}}

```txt
	A	B	C	D	E
W	-	-	50	-	-
X	30	-	20	-	10
Y	-	20	-	30	-
Z	-	-	-	-	50

Total cost: 3100
```



## Phix

See [[Transportation_problem#Phix]] for optimal results.
{{trans|YaBasic}}
{{trans|Go}}

```Phix
sequence supply = {50,60,50,50},
         demand = {30,20,70,30,60},
         costs = {{16,16,13,22,17},
                  {14,14,13,19,15},
                  {19,19,20,23,50},
                  {50,12,50,15,11}}

sequence row_done = repeat(false,length(supply)),
         col_done = repeat(false,length(demand))

function diff(integer j, leng, bool is_row)
integer min1 = #3FFFFFFF, min2 = min1, min_p = -1
    for i=1 to leng do
        if not iff(is_row?col_done:row_done)[i] then
            integer c = iff(is_row?costs[j,i]:costs[i,j])
            if c<min1 then
                min2 = min1
                min1 = c
                min_p = i
            elsif c<min2 then
                min2 = c
            end if
        end if
    end for
    return {min2-min1,min1,min_p,j}
end function

function max_penalty(integer len1, len2, bool is_row)
integer pc = -1, pm = -1, mc = -1, md = -#3FFFFFFF
    for i=1 to len1 do
        if not iff(is_row?row_done:col_done)[i] then
            sequence res2 = diff(i, len2, is_row)
            if res2[1]>md then
                {md,mc,pc,pm} = res2
            end if
        end if
    end for
    return {md,mc}&iff(is_row?{pm,pc}:{pc,pm})
end function

integer supply_left = sum(supply),
        total_cost = 0

sequence results = repeat(repeat(0,length(demand)),length(supply))

while supply_left>0 do
    sequence cell = min(max_penalty(length(supply), length(demand), true),
                        max_penalty(length(demand), length(supply), false))
    integer {{},{},r,c} = cell,
            q = min(demand[c], supply[r])
    demand[c] -= q
    col_done[c] = (demand[c]==0)
    supply[r] -= q
    row_done[r] = (supply[r]==0)
    results[r, c] = q
    supply_left -= q
    total_cost += q * costs[r, c]
end while

printf(1,"     A   B   C   D   E\n")
for i=1 to length(supply) do
    printf(1,"%c ",'Z'-length(supply)+i)
    for j=1 to length(demand) do
        printf(1,"%4d",results[i,j])
    end for
    printf(1,"\n")
end for
printf(1,"\nTotal cost = %d\n", total_cost)
```

{{out}}

```txt

     A   B   C   D   E
W    0   0  50   0   0
X   30   0  20   0  10
Y    0  20   0  30   0
Z    0   0   0   0  50

Total cost = 3100

```

Using the sample from Ruby:

```Phix
sequence supply = {461, 277, 356, 488,   393},
         demand = {278, 60, 461, 116, 1060},
         costs  = {{46, 74,  9, 28, 99},
                   {12, 75,  6, 36, 48},
                   {35, 199, 4,  5, 71},
                   {61, 81, 44, 88,  9},
                   {85, 60, 14, 25, 79}}
```

{{Out}}

```txt

     A   B   C   D   E
V    0   0 461   0   0
W  277   0   0   0   0
X    1   0   0   0 355
Y    0   0   0   0 488
Z    0  60   0 116 217

Total cost = 60748

```



## Python

{{trans|Ruby}}

```python
from collections import defaultdict

costs  = {'W': {'A': 16, 'B': 16, 'C': 13, 'D': 22, 'E': 17},
          'X': {'A': 14, 'B': 14, 'C': 13, 'D': 19, 'E': 15},
          'Y': {'A': 19, 'B': 19, 'C': 20, 'D': 23, 'E': 50},
          'Z': {'A': 50, 'B': 12, 'C': 50, 'D': 15, 'E': 11}}
demand = {'A': 30, 'B': 20, 'C': 70, 'D': 30, 'E': 60}
cols = sorted(demand.iterkeys())
supply = {'W': 50, 'X': 60, 'Y': 50, 'Z': 50}
res = dict((k, defaultdict(int)) for k in costs)
g = {}
for x in supply:
    g[x] = sorted(costs[x].iterkeys(), key=lambda g: costs[x][g])
for x in demand:
    g[x] = sorted(costs.iterkeys(), key=lambda g: costs[g][x])

while g:
    d = {}
    for x in demand:
        d[x] = (costs[g[x][1]][x] - costs[g[x][0]][x]) if len(g[x]) > 1 else costs[g[x][0]][x]
    s = {}
    for x in supply:
        s[x] = (costs[x][g[x][1]] - costs[x][g[x][0]]) if len(g[x]) > 1 else costs[x][g[x][0]]
    f = max(d, key=lambda n: d[n])
    t = max(s, key=lambda n: s[n])
    t, f = (f, g[f][0]) if d[f] > s[t] else (g[t][0], t)
    v = min(supply[f], demand[t])
    res[f][t] += v
    demand[t] -= v
    if demand[t] == 0:
        for k, n in supply.iteritems():
            if n != 0:
                g[k].remove(t)
        del g[t]
        del demand[t]
    supply[f] -= v
    if supply[f] == 0:
        for k, n in demand.iteritems():
            if n != 0:
                g[k].remove(f)
        del g[f]
        del supply[f]

for n in cols:
    print "\t", n,
print
cost = 0
for g in sorted(costs):
    print g, "\t",
    for n in cols:
        y = res[g][n]
        if y != 0:
            print y,
        cost += y * costs[g][n]
        print "\t",
    print
print "\n\nTotal Cost = ", cost
```

{{out}}

```txt
    A   B   C   D   E
W           50
X   30      20      10
Y       20      30
Z                   50


Total Cost =  3100
```



## Racket

Losley: {{trans|Ruby}}

Strangely, due to the sub-deterministic nature of the hash tables,
resources were allocated differently to the [[#Ruby]] version; but
somehow at the same total cost!


```racket
#lang racket
(define-values (1st 2nd 3rd) (values first second third))

(define-syntax-rule (?: x t f) (if (zero? x) f t))

(define (hash-ref2
         hsh# key-1 key-2
         #:fail-2 (fail-2 (λ () (error 'hash-ref2 "key-2:~a is not found in hash" key-2)))
         #:fail-1 (fail-1 (λ () (error 'hash-ref2 "key-1:~a is not found in hash" key-1))))
  (hash-ref (hash-ref hsh# key-1 fail-1) key-2 fail-2))

(define (VAM costs all-supply all-demand)
  (define (reduce-g/x g/x x#-- x x-v y y-v)
    (for/fold ((rv (?: x-v g/x (hash-remove g/x x))))
      (#:when (zero? y-v) ((k n) (in-hash x#--)) #:unless (zero? n))
      (hash-update rv k (curry remove y))))

  (define (cheapest-candidate/tie-break candidates)
    (define cand-max3 (3rd (argmax 3rd candidates)))
    (argmin 2nd (for/list ((cand candidates) #:when (= (3rd cand) cand-max3)) cand)))

  (let vam-loop
    ((res (hash))
     (supply all-supply)
     (g/supply
      (for/hash ((x (in-hash-keys all-supply)))
        (define costs#x (hash-ref costs x))
        (define key-fn (λ (g) (hash-ref costs#x g)))
        (values x (sort (hash-keys costs#x) < #:key key-fn #:cache-keys? #t))))
     (demand all-demand)
     (g/demand
      (for/hash ((x (in-hash-keys all-demand)))
        (define key-fn (λ (g) (hash-ref2 costs g x)))
        (values x (sort (hash-keys costs) < #:key key-fn #:cache-keys? #t)))))
    (cond
      [(and (hash-empty? supply) (hash-empty? demand)) res]
      [(or (hash-empty? supply) (hash-empty? demand)) (error 'VAM "Unbalanced supply / demand")]
      [else
       (define D
         (let ((candidates
                (for/list ((x (in-hash-keys demand)))
                  (match-define (hash-table ((== x) (and g#x (list g#x.0 _ ...))) _ ...) g/demand)
                  (define z (hash-ref2 costs g#x.0 x))
                  (match g#x
                    [(list _ g#x.1 _ ...) (list x z (- (hash-ref2 costs g#x.1 x) z))]
                    [(list _) (list x z z)]))))
           (cheapest-candidate/tie-break candidates)))

       (define S
         (let ((candidates
                (for/list ((x (in-hash-keys supply)))
                  (match-define (hash-table ((== x) (and g#x (list g#x.0 _ ...))) _ ...) g/supply)
                  (define z (hash-ref2 costs x g#x.0))
                  (match g#x
                    [(list _ g#x.1 _ ...) (list x z (- (hash-ref2 costs x g#x.1) z))]
                    [(list _) (list x z z)]))))
           (cheapest-candidate/tie-break candidates)))

       (define-values (d s)
         (let ((t>f? (if (= (3rd D) (3rd S)) (> (2nd S) (2nd D)) (> (3rd D) (3rd S)))))
           (if t>f? (values (1st D) (1st (hash-ref g/demand (1st D))))
               (values (1st (hash-ref g/supply (1st S))) (1st S)))))

       (define v (min (hash-ref supply s) (hash-ref demand d)))

       (define d-v (- (hash-ref demand d) v))
       (define s-v (- (hash-ref supply s) v))

       (define demand-- (?: d-v (hash-set demand d d-v) (hash-remove demand d)))
       (define supply-- (?: s-v (hash-set supply s s-v) (hash-remove supply s)))

       (vam-loop
        (hash-update res s (λ (h) (hash-update h d (λ (x) (+ v x)) 0)) hash)
        supply-- (reduce-g/x g/supply supply-- s s-v d d-v)
        demand-- (reduce-g/x g/demand demand-- d d-v s s-v))])))

(define (vam-solution-cost costs demand?cols solution)
  (match demand?cols
    [(? list? demand-cols)
     (for*/sum ((g (in-hash-keys costs)) (n (in-list demand-cols)))
       (* (hash-ref2 solution g n #:fail-2 0) (hash-ref2 costs g n)))]
    [(hash-table (ks _) ...) (vam-solution-cost costs (sort ks symbol<? solution))]))

(define (describe-VAM-solution costs demand sltn)
  (define demand-cols (sort (hash-keys demand) symbol<?))
  (string-join
   (map
    (curryr string-join "\t")
    `(,(map ~a (cons "" demand-cols))
      ,@(for/list ((g (in-hash-keys costs)))
          (cons (~a g) (for/list ((c demand-cols)) (~a (hash-ref2 sltn g c #:fail-2 "-")))))
      ()
      ("Total Cost:" ,(~a (vam-solution-cost costs demand-cols sltn)))))
   "\n"))

;; --------------------------------------------------------------------------------------------------
(let ((COSTS (hash 'W (hash 'A 16 'B 16 'C 13 'D 22 'E 17)
                   'X (hash 'A 14 'B 14 'C 13 'D 19 'E 15)
                   'Y (hash 'A 19 'B 19 'C 20 'D 23 'E 50)
                   'Z (hash 'A 50 'B 12 'C 50 'D 15 'E 11)))
      (DEMAND (hash 'A 30 'B 20 'C 70 'D 30 'E 60))
      (SUPPLY (hash 'W 50 'X 60 'Y 50 'Z 50)))
  (displayln (describe-VAM-solution COSTS DEMAND (VAM COSTS SUPPLY DEMAND))))
```


{{out}}

```txt

	A	B	C	D	E
W	-	-	50	-	-
X	10	20	20	-	10
Y	20	-	-	30	-
Z	-	-	-	-	50

Total Cost:	3100
```



## Ruby

Breaks ties using lowest cost cell.

### Task Example


```ruby
# VAM
#
#  Nigel_Galloway
#  September 1st., 2013
COSTS  = {W: {A: 16, B: 16, C: 13, D: 22, E: 17},
          X: {A: 14, B: 14, C: 13, D: 19, E: 15},
          Y: {A: 19, B: 19, C: 20, D: 23, E: 50},
          Z: {A: 50, B: 12, C: 50, D: 15, E: 11}}
demand = {A: 30, B: 20, C: 70, D: 30, E: 60}
supply = {W: 50, X: 60, Y: 50, Z: 50}
COLS = demand.keys
res = {}; COSTS.each_key{|k| res[k] = Hash.new(0)}
g = {}; supply.each_key{|x| g[x] = COSTS[x].keys.sort_by{|g| COSTS[x][g]}}
        demand.each_key{|x| g[x] = COSTS.keys.sort_by{|g| COSTS[g][x]}}

until g.empty?
  d = demand.collect{|x,y| [x, z = COSTS[g[x][0]][x], g[x][1] ? COSTS[g[x][1]][x] - z : z]}
  dmax = d.max_by{|n| n[2]}
  d = d.select{|x| x[2] == dmax[2]}.min_by{|n| n[1]}
  s = supply.collect{|x,y| [x, z = COSTS[x][g[x][0]], g[x][1] ? COSTS[x][g[x][1]] - z : z]}
  dmax = s.max_by{|n| n[2]}
  s = s.select{|x| x[2] == dmax[2]}.min_by{|n| n[1]}
  t,f = d[2]==s[2] ? [s[1], d[1]] : [d[2],s[2]]
  d,s = t > f ? [d[0],g[d[0]][0]] : [g[s[0]][0],s[0]]
  v = [supply[s], demand[d]].min
  res[s][d] += v
  demand[d] -= v
  if demand[d] == 0 then
    supply.reject{|k, n| n == 0}.each_key{|x| g[x].delete(d)}
    g.delete(d)
    demand.delete(d)
  end
  supply[s] -= v
  if supply[s] == 0 then
    demand.reject{|k, n| n == 0}.each_key{|x| g[x].delete(s)}
    g.delete(s)
    supply.delete(s)
  end
end

COLS.each{|n| print "\t", n}
puts
cost = 0
COSTS.each_key do |g|
  print g, "\t"
  COLS.each do |n|
    y = res[g][n]
    print y if y != 0
    cost += y * COSTS[g][n]
    print "\t"
  end
  puts
end
print "\n\nTotal Cost = ", cost
```

{{out}}

```txt

        A       B       C       D       E
W                       50
X       30              20              10
Y               20              30
Z                                       50


Total Cost = 3100

```



### Reference Example

Replacing the data in the Task Example with:

```ruby
COSTS  = {S1: {D1: 46, D2:  74, D3:  9, D4: 28, D5: 99},
          S2: {D1: 12, D2:  75, D3:  6, D4: 36, D5: 48},
          S3: {D1: 35, D2: 199, D3:  4, D4:  5, D5: 71},
          S4: {D1: 61, D2:  81, D3: 44, D4: 88, D5:  9},
          S5: {D1: 85, D2:  60, D3: 14, D4: 25, D5: 79}}
demand = {D1: 278, D2: 60, D3: 461, D4: 116, D5: 1060}
supply = {S1: 461, S2: 277, S3: 356, S4: 488, S5: 393}
```

Produces:

```txt

        D1      D2      D3      D4      D5
S1      1       60      68              332
S2      277
S3                              116     240
S4                                      488
S5                      393


Total Cost = 68804

```



## Sidef

{{trans|Ruby}}

```ruby
var costs = :(
    W => :(A => 16, B => 16, C => 13, D => 22, E => 17),
    X => :(A => 14, B => 14, C => 13, D => 19, E => 15),
    Y => :(A => 19, B => 19, C => 20, D => 23, E => 50),
    Z => :(A => 50, B => 12, C => 50, D => 15, E => 11)
)

var demand = :(A => 30, B => 20, C => 70, D => 30, E => 60)
var supply = :(W => 50, X => 60, Y => 50, Z => 50)

var cols = demand.keys.sort

var (:res, :g)
supply.each {|x| g{x} = costs{x}.keys.sort_by{|g| costs{x}{g} }}
demand.each {|x| g{x} = costs   .keys.sort_by{|g| costs{g}{x} }}

while (g) {
    var d = demand.collect {|x|
        [x, var z = costs{g{x}[0]}{x}, g{x}[1] ? costs{g{x}[1]}{x}-z : z]
    }

    var s = supply.collect {|x|
        [x, var z = costs{x}{g{x}[0]}, g{x}[1] ? costs{x}{g{x}[1]}-z : z]
    }

    d.grep! { .[2] == d.max_by{ .[2] }[2] }.min_by! { .[1] }
    s.grep! { .[2] == s.max_by{ .[2] }[2] }.min_by! { .[1] }

    var (t,f) = (d[2] == s[2] ? ((s[1], d[1])) : ((d[2], s[2])))
        (d,s) = (t > f ? ((d[0], g{d[0]}[0])) : ((g{s[0]}[0],s[0])))

    var v = (supply{s} `min` demand{d})

    res{s}{d} := 0 += v
    demand{d} -= v

    if (demand{d} == 0) {
        supply.grep {|_,n| n != 0 }.each {|x| g{x}.delete(d) }
        g.delete(d)
        demand.delete(d)
    }

    supply{s} -= v

    if (supply{s} == 0) {
        demand.grep {|_,n| n != 0 }.each {|x| g{x}.delete(s) }
        g.delete(s)
        supply.delete(s)
    }
}

say("\t", cols.join("\t"))

var cost = 0
costs.keys.sort.each { |g|
  print(g, "\t")
  cols.each { |n|
    if (defined(var y = res{g}{n})) {
        print(y)
        cost += (y * costs{g}{n})
    }
    print("\t")
  }
  print("\n")
}

say "\n\nTotal Cost = #{cost}"
```

{{out}}

```txt

	A	B	C	D	E
W			50
X	30		20		10
Y		20		30
Z					50


Total Cost = 3100

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# A sort that works by sorting by an auxiliary key computed by a lambda term
proc sortByFunction {list lambda} {
    lmap k [lsort -index 1 [lmap k $list {
	list $k [uplevel 1 [list apply $lambda $k]]
    }]] {lindex $k 0}
}

# A simple way to pick a “best” item from a list
proc minimax {list maxidx minidx} {
    set max -Inf; set min Inf
    foreach t $list {
	if {[set m [lindex $t $maxidx]] > $max} {
	    set best $t
	    set max $m
	    set min Inf
	} elseif {$m == $max && [set m [lindex $t $minidx]] < $min} {
	    set best $t
	    set min $m
	}
    }
    return $best
}

# The approximation engine. Note that this does not change the provided
# arguments at all since they are copied on write.
proc VAM {costs demand supply} {
    # Initialise the sorted sequence of pairs and the result dictionary
    foreach x [dict keys $demand] {
	dict set g $x [sortByFunction [dict keys $supply] {g {
	    upvar 1 costs costs x x; dict get $costs $g $x
	}}]
	dict set row $x 0
    }
    foreach x [dict keys $supply] {
	dict set g $x [sortByFunction [dict keys $demand] {g {
	    upvar 1 costs costs x x; dict get $costs $x $g
	}}]
	dict set res $x $row
    }

    # While there's work to do...
    while {[dict size $g]} {
	# Select "best" demand
	lassign [minimax [lmap x [dict keys $demand] {
	    if {![llength [set gx [dict get $g $x]]]} continue
	    set z [dict get $costs [lindex $gx 0] $x]
	    if {[llength $gx] > 1} {
		list $x $z [expr {[dict get $costs [lindex $gx 1] $x] - $z}]
	    } else {
		list $x $z $z
	    }
	}] 2 1] d dVal dCost

	# Select "best" supply
	lassign [minimax [lmap x [dict keys $supply] {
	    if {![llength [set gx [dict get $g $x]]]} continue
	    set z [dict get $costs $x [lindex $gx 0]]
	    if {[llength $gx] > 1} {
		list $x $z [expr {[dict get $costs $x [lindex $gx 1]] - $z}]
	    } else {
		list $x $z $z
	    }
	}] 2 1] s sVal sCost

	# Compute how much to transfer, and with which "best"
	if {$sCost == $dCost ? $sVal > $dVal : $sCost < $dCost} {
	    set s [lindex [dict get $g $d] 0]
	} else {
	    set d [lindex [dict get $g $s] 0]
	}
	set v [expr {min([dict get $supply $s], [dict get $demand $d])}]

	# Transfer some supply to demand
	dict update res $s inner {dict incr inner $d $v}
	dict incr demand $d -$v
	if {[dict get $demand $d] == 0} {
	    dict for {k n} $supply {
		if {$n != 0} {
		    # Filter list in dictionary to remove element
		    dict set g $k [lmap x [dict get $g $k] {
			if {$x eq $d} continue; set x
		    }]
		}
	    }
	    dict unset g $d
	    dict unset demand $d
	}
	dict incr supply $s -$v
	if {[dict get $supply $s] == 0} {
	    dict for {k n} $demand {
		if {$n != 0} {
		    dict set g $k [lmap x [dict get $g $k] {
			if {$x eq $s} continue; set x
		    }]
		}
	    }
	    dict unset g $s
	    dict unset supply $s
	}
    }
    return $res
}
```

Demonstration:

```tcl
set COSTS {
    W {A 16 B 16 C 13 D 22 E 17}
    X {A 14 B 14 C 13 D 19 E 15}
    Y {A 19 B 19 C 20 D 23 E 50}
    Z {A 50 B 12 C 50 D 15 E 11}
}
set DEMAND {A 30 B 20 C 70 D 30 E 60}
set SUPPLY {W 50 X 60 Y 50 Z 50}

set RES [VAM $COSTS $DEMAND $SUPPLY]

puts \t[join [dict keys $DEMAND] \t]
set cost 0
foreach g [dict keys $SUPPLY] {
    puts $g\t[join [lmap n [dict keys $DEMAND] {
	set c [dict get $RES $g $n]
	incr cost [expr {$c * [dict get $COSTS $g $n]}]
	expr {$c ? $c : ""}
    }] \t]
}
puts "\nTotal Cost = $cost"
```

{{out}}

```txt

        A       B       C       D       E
W                       50
X       10      20      20              10
Y       20                      30
Z                                       50

Total Cost = 3100

```



## Yabasic

{{trans|C}}

```Yabasic

N_ROWS = 4 : N_COLS = 5

dim supply(N_ROWS)
dim demand(N_COLS)

restore sup
for n = 0 to N_ROWS - 1
	read supply(n)
next n

restore dem
for n = 0 to N_COLS - 1
	read demand(n)
next n

label sup
data 50, 60, 50, 50

label dem
data 30, 20, 70, 30, 60

dim costs(N_ROWS, N_COLS)

label cost
data 16, 16, 13, 22, 17
data 14, 14, 13, 19, 15
data 19, 19, 20, 23, 50
data 50, 12, 50, 15, 11

restore cost
for i = 0 to N_ROWS - 1
	for j = 0 to N_COLS - 1
		read costs(i, j)
	next j
next i

dim row_done(N_ROWS)
dim col_done(N_COLS)

sub diff(j, leng, is_row, res())
    local i, c, min1, min2, min_p, test

    min1 = 10e300 : min2 = min1 : min_p = -1

    for i = 0 to leng - 1
    	if is_row then
    		test = col_done(i)
    	else
    		test = row_done(i)
    	end if
    	if test continue
    	if is_row then
    		c = costs(j, i)
    	else
    		c = costs(i, j)
    	end if
        if c < min1 then
            min2 = min1
            min1 = c
            min_p = i
        elseif c < min2 then
        	min2 = c
        end if
    next i
    res(0) = min2 - min1
    res(1) = min1
    res(2) = min_p
end sub

sub max_penalty(len1, len2, is_row, res())
    local i, pc, pm, mc, md, res2(3), test

    pc = -1 : pm = -1 : mc = -1 : md = -10e300

    for i = 0 to len1 - 1
        if is_row then
    		test = row_done(i)
    	else
    		test = col_done(i)
    	end if
        if test continue
       	diff(i, len2, is_row, res2())
        if res2(0) > md then
            md = res2(0)  //* max diff */
            pm = i        //* pos of max diff */
            mc = res2(1)  //* min cost */
            pc = res2(2)  //* pos of min cost */
        end if
    next i

    if is_row then
        res(0) = pm : res(1) = pc
    else
        res(0) = pc : res(1) = pm
    end if
    res(2) = mc : res(3) = md
end sub

sub next_cell(res())
    local i, res1(4), res2(4)

    max_penalty(N_ROWS, N_COLS, TRUE, res1())
    max_penalty(N_COLS, N_ROWS, FALSE, res2())

    if res1(3) = res2(3) then
        if res1(2) < res2(2) then
            for i = 0 to 3 : res(i) = res1(i) : next i
        else
            for i = 0 to 3 : res(i) = res2(i) : next i
        end if
        return
    end if
    if res1(3) > res2(3) then
        for i = 0 to 3 : res(i) = res2(i) : next i
    else
        for i = 0 to 3 : res(i) = res1(i) : next i
    end if
end sub

supply_left = 0 : total_cost = 0 : dim cell(4)

dim results(N_ROWS, N_COLS)

for i = 0 to N_ROWS - 1 : supply_left = supply_left + supply(i) : next i

while(supply_left > 0)
    next_cell(cell())
    r = cell(0)
    c = cell(1)
    q = min(demand(c), supply(r))
    demand(c) = demand(c) - q
    if not demand(c) col_done(c) = TRUE
    supply(r) = supply(r) - q
    if not supply(r) row_done(r) = TRUE
    results(r, c) = q
    supply_left = supply_left - q
    total_cost = total_cost + q * costs(r, c)
wend

print "    A   B   C   D   E\n"
for i = 0 to N_ROWS - 1
    print chr$(asc("W") + i), " ";
    for j = 0 to N_COLS - 1
    	print results(i, j) using "###";
    next j
    print
next i
print "\nTotal cost = ", total_cost
```



## zkl

{{trans|Python}}{{trans|Ruby}}

```zkl
costs:=Dictionary(
   "W",Dictionary("A",16, "B",16, "C",13, "D",22, "E",17),
   "X",Dictionary("A",14, "B",14, "C",13, "D",19, "E",15),
   "Y",Dictionary("A",19, "B",19, "C",20, "D",23, "E",50),
   "Z",Dictionary("A",50, "B",12, "C",50, "D",15, "E",11)).makeReadOnly();
demand:=Dictionary("A",30, "B",20, "C",70, "D",30, "E",60);  // gonna be modified
supply:=Dictionary("W",50, "X",60, "Y",50, "Z",50);	    // gonna be modified
```


```zkl
cols:=demand.keys.sort();
res :=vogel(costs,supply,demand);
cost:=0;
println("\t",cols.concat("\t"));
foreach g in (costs.keys.sort()){
   print(g,"\t");
   foreach n in (cols){
      y:=res[g].find(n);
      if(y){ y=y[0]; print(y); cost+=y*costs[g][n]; }
      print("\t");
   }
   println();
}
println("\nTotal Cost = ",cost);
```


```zkl
fcn vogel(costs,supply,demand){
   // a Dictionary can be created via a list of (k,v) pairs
   res:= Dictionary(costs.pump(List,fcn([(k,_)]){ return(k,D()) }));
   g  := Dictionary(); // cross index costs and make writable
   supply.pump(Void,'wrap([(k,_)]){ g[k] =
      costs[k].keys.sort('wrap(a,b){ costs[k][a]<costs[k][b] }).copy() });
   demand.pump(Void,'wrap([(k,_)]){ g[k] =
      costs.keys.sort('wrap(a,b){ costs[a][k]<costs[b][k] }).copy() });

   while(g){
      d:=Dictionary(demand.pump(List,'wrap([(k,_)]){ return(k,
	 g[k][0,2].apply('wrap(gk){ costs[gk][k] }).reverse().reduce('-)) }));
      s:=Dictionary(supply.pump(List,'wrap([(k,_)]){ return(k,
	 g[k][0,2].apply('wrap(gk){ costs[k][gk] }).reverse().reduce('-)) }));
      f:=(0).max(d.values); f=d.filter('wrap([(_,v)]){ v==f })[-1][0];
      t:=(0).max(s.values); t=s.filter('wrap([(_,v)]){ v==t })[-1][0];
      t,f=(if(d[f]>s[t]) T(f,g[f][0]) else T(g[t][0],t));
      v:=supply[f].min(demand[t]);
      res[f].appendV(t,v);  // create t:(v) or append v to t:(...)
      if(0 == (demand[t]-=v)){
	 supply.pump(Void,'wrap([(k,n)]){ if(n!=0) g[k].remove(t) });
	 g.del(t); demand.del(t);
      }
      if(0 == (supply[f]-=v)){
	 demand.pump(Void,'wrap([(k,n)]){ if(n!=0) g[k].remove(f) });
	 g.del(f); supply.del(f);
      }
   }//while
   res
}
```

{{out}}

```txt

	A	B	C	D	E
W			50
X	10	20	20		10
Y	20			30
Z					50

Total Cost = 3100

```

