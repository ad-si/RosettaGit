+++
title = "Solve a Hopido puzzle"
description = ""
date = 2019-08-05T10:47:19Z
aliases = []
[extra]
id = 17657
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "go",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "tcl",
  "zkl",
]
+++

Hopido puzzles are similar to [[Solve a Hidato puzzle | Hidato]]. The most important difference is that the only moves allowed are:  hop over one tile diagonally; and over two tiles horizontally and vertically. It should be possible to start anywhere in the path, the end point isn't indicated and there are no intermediate clues. [http://gamesandinnovation.com/2010/02/10/hopido-design-post-mortem/ Hopido Design Post Mortem] contains the following:

"Big puzzles represented another problem. Up until quite late in the project our puzzle solver was painfully slow with most puzzles above 7×7 tiles. Testing the solution from each starting point could take hours. If the tile layout was changed even a little, the whole puzzle had to be tested again. We were just about to give up the biggest puzzles entirely when our programmer suddenly came up with a magical algorithm that cut the testing process down to only minutes. Hooray!"

Knowing the kindness in the heart of every contributor to Rosetta Code, I know that we shall feel that as an act of humanity we must solve these puzzles for them in let's say milliseconds.

Example:

 . 0 0 . 0 0 .
 0 0 0 0 0 0 0
 0 0 0 0 0 0 0
 . 0 0 0 0 0 .
 . . 0 0 0 . .
 . . . 0 . . .

Extra credits are available for other interesting designs.


## Related tasks

* [[A* search algorithm]]
* [[Solve a Holy Knight's tour]]
* [[Knight's tour]]
* [[N-queens problem]]
* [[Solve a Hidato puzzle]]
* [[Solve a Holy Knight's tour]]
* [[Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle]]





## AutoHotkey


```AutoHotkey
SolveHopido(Grid, Locked, Max, row, col, num:=1, R:="", C:=""){
	if (R&&C)							; if neighbors (not first iteration)
	{
		Grid[R, C] := ">" num 					; place num in current neighbor and mark it visited ">"
		row:=R, col:=C						; move to current neighbor
	}

	num++								; increment num
	if (num=max)							; if reached end
		return map(Grid)					; return solution

	if locked[num]							; if current num is a locked value
	{
		row := StrSplit((StrSplit(locked[num], ",").1) , ":").1	; find row of num
		col := StrSplit((StrSplit(locked[num], ",").1) , ":").2	; find col of num
		if SolveHopido(Grid, Locked, Max, row, col, num)	; solve for current location and value
			return map(Grid)				; if solved, return solution
	}
	else
	{
		for each, value in StrSplit(Neighbor(row,col), ",")
		{
			R := StrSplit(value, ":").1
			C := StrSplit(value, ":").2

			if (Grid[R,C] = "")				; a hole or out of bounds
			|| InStr(Grid[R, C], ">")			; visited
			|| Locked[num+1] && !(Locked[num+1]~= "\b" R ":" C "\b") ; not neighbor of locked[num+1]
			|| Locked[num-1] && !(Locked[num-1]~= "\b" R ":" C "\b") ; not neighbor of locked[num-1]
			|| Locked[num]					; locked value
			|| Locked[Grid[R, C]]				; locked cell
				continue

			if SolveHopido(Grid, Locked, Max, row, col, num, R, C)	; solve for current location, neighbor and value
				return map(Grid)			; if solved, return solution
		}
	}
	num--								; step back
	for i, line in Grid
		for j, element in line
			if InStr(element, ">") && (StrReplace(element, ">") >= num)
				Grid[i, j] := 0
}
;--------------------------------
;--------------------------------
;--------------------------------
Neighbor(row,col){
	return Trim( ""
	. ","  row ":" col-3
	. ","  row ":" col+3
	. ","  row-3 ":" col
	. ","  row+3 ":" col

	. ","  row+2 ":" col+2
	. ","  row+2 ":" col-2
	. ","  row-2 ":" col+2
	. ","  row-2 ":" col-2
	, ",")
}
;--------------------------------
map(Grid){
	for i, row in Grid
	{
		for j, element in row
			line .= (A_Index > 1 ? "`t" : "") element
		map .= (map<>""?"`n":"") line
		line := ""
	}
	return StrReplace(map, ">")
}
```

Examples:
```AutoHotkey
;--------------------------------
Grid := [["",0 ,0 ,"",0 ,0 ,""]
	,[0 ,0 ,0 ,0 ,0 ,0 ,0]
	,[0 ,0 ,0 ,0 ,0 ,0 ,0]
	,["",0 ,0 ,0 ,0 ,0 ,""]
	,["","",0 ,0 ,0 ,"",""]
	,["","","",0 ,"","",""]]
;--------------------------------
; find locked cells, find max value
Locked := []
max := 1
for i, line in Grid
	for j, element in line
		if (element >= 0)
			max++ 	, list .= i ":" j "`n"

random, rnd, 1, %max%
loop, parse, list, `n, `r
	if (A_Index = rnd)
	{
		row := StrSplit(A_LoopField, ":").1
		col := StrSplit(A_LoopField, ":").2
		Grid[row,col] := 1
		Locked[1] := row ":" col "," Neighbor(row, col)
		break
	}
;--------------------------------
MsgBox, 262144, ,% SolveHopido(Grid, Locked, Max, row, col)
return
```

Outputs:
```txt
	17	24		16	25
22	8	11	21	7	10	20
13	2	5	14	1	4	15
	18	23	9	19	26
		12	3	6
			27
```



## C++


```cpp

#include <vector>
#include <sstream>
#include <iostream>
#include <iterator>
#include <stdlib.h>
#include <string.h>

using namespace std;

struct node
{
    int val;
    unsigned char neighbors;
};

class nSolver
{
public:
    nSolver()
    {
	dx[0] = -2; dy[0] = -2; dx[1] = -2; dy[1] =  2;
	dx[2] =  2; dy[2] = -2; dx[3] =  2; dy[3] =  2;
	dx[4] = -3; dy[4] =  0; dx[5] =  3; dy[5] =  0;
	dx[6] =  0; dy[6] = -3; dx[7] =  0; dy[7] =  3;
    }

    void solve( vector<string>& puzz, int max_wid )
    {
	if( puzz.size() < 1 ) return;
	wid = max_wid; hei = static_cast<int>( puzz.size() ) / wid;
	int len = wid * hei, c = 0; max = len;
	arr = new node[len]; memset( arr, 0, len * sizeof( node ) );

	for( vector<string>::iterator i = puzz.begin(); i != puzz.end(); i++ )
	{
	    if( ( *i ) == "*" ) { max--; arr[c++].val = -1; continue; }
	    arr[c].val = atoi( ( *i ).c_str() );
	    c++;
	}

	solveIt(); c = 0;
	for( vector<string>::iterator i = puzz.begin(); i != puzz.end(); i++ )
	{
	    if( ( *i ) == "." )
	    {
		ostringstream o; o << arr[c].val;
		( *i ) = o.str();
	    }
	    c++;
	}
	delete [] arr;
    }

private:
    bool search( int x, int y, int w )
    {
	if( w > max ) return true;

	node* n = &arr[x + y * wid];
	n->neighbors = getNeighbors( x, y );

	for( int d = 0; d < 8; d++ )
	{
	    if( n->neighbors & ( 1 << d ) )
	    {
		int a = x + dx[d], b = y + dy[d];
		if( arr[a + b * wid].val == 0 )
		{
		    arr[a + b * wid].val = w;
		    if( search( a, b, w + 1 ) ) return true;
		    arr[a + b * wid].val = 0;
		}
	    }
	}
	return false;
    }

    unsigned char getNeighbors( int x, int y )
    {
	unsigned char c = 0; int a, b;
	for( int xx = 0; xx < 8; xx++ )
	{
	    a = x + dx[xx], b = y + dy[xx];
	    if( a < 0 || b < 0 || a >= wid || b >= hei ) continue;
	    if( arr[a + b * wid].val > -1 ) c |= ( 1 << xx );
	}
	return c;
    }

    void solveIt()
    {
	int x, y, z; findStart( x, y, z );
	if( z == 99999 ) { cout << "\nCan't find start point!\n"; return; }
	search( x, y, z + 1 );
    }

    void findStart( int& x, int& y, int& z )
    {
	for( int b = 0; b < hei; b++ )
	    for( int a = 0; a < wid; a++ )
		if( arr[a + wid * b].val == 0 )
		{
		    x = a; y = b; z = 1;
		    arr[a + wid * b].val = z;
		    return;
		}
    }

    int wid, hei, max, dx[8], dy[8];
    node* arr;
};

int main( int argc, char* argv[] )
{
    int wid; string p;
    p = "* . . * . . * . . . . . . . . . . . . . . * . . . . . * * * . . . * * * * * . * * *"; wid = 7;
    istringstream iss( p ); vector<string> puzz;
    copy( istream_iterator<string>( iss ), istream_iterator<string>(), back_inserter<vector<string> >( puzz ) );
    nSolver s; s.solve( puzz, wid );
    int c = 0;
    for( vector<string>::iterator i = puzz.begin(); i != puzz.end(); i++ )
    {
	if( ( *i ) != "*" && ( *i ) != "." )
	{
	    if( atoi( ( *i ).c_str() ) < 10 ) cout << "0";
	    cout << ( *i ) << " ";
	}
	else cout << "   ";
	if( ++c >= wid ) { cout << endl; c = 0; }
    }
    cout << endl << endl;
    return system( "pause" );
}

```

```txt

   01 04    12 03
27 16 19 22 15 18 21
05 08 11 02 07 10 13
   23 26 17 20 25
      06 09 14
         24

```



## C#

The same solver can solve Hidato, Holy Knight's Tour, Hopido and Numbrix puzzles.<br/>
The input can be an array of strings if each cell is one character. The length of the first row must be the number of columns in the puzzle.<br/>
Any non-numeric value indicates a no-go.<br/>
If there are cells that require more characters, then a 2-dimensional array of ints must be used. Any number < 0 indicates a no-go.<br/>

```c#
using System.Collections;
using System.Collections.Generic;
using static System.Console;
using static System.Math;
using static System.Linq.Enumerable;

public class Solver
{
    private static readonly (int dx, int dy)[]
        //other puzzle types elided
        hopidoMoves = {(-3,0),(0,-3),(0,3),(3,0),(-2,-2),(-2,2),(2,-2),(2,2)},

    private (int dx, int dy)[] moves;

    public static void Main()
    {
        Print(new Solver(hopidoMoves).Solve(false,
            ".00.00.",
            "0000000",
            "0000000",
            ".00000.",
            "..000..",
            "...0..."
        ));
    }

    public Solver(params (int dx, int dy)[] moves) => this.moves = moves;

    public int[,] Solve(bool circular, params string[] puzzle)
    {
        var (board, given, count) = Parse(puzzle);
        return Solve(board, given, count, circular);
    }

    public int[,] Solve(bool circular, int[,] puzzle)
    {
        var (board, given, count) = Parse(puzzle);
        return Solve(board, given, count, circular);
    }

    private int[,] Solve(int[,] board, BitArray given, int count, bool circular)
    {
        var (height, width) = (board.GetLength(0), board.GetLength(1));
        bool solved = false;
        for (int x = 0; x < height && !solved; x++) {
            solved = Range(0, width).Any(y => Solve(board, given, circular, (height, width), (x, y), count, (x, y), 1));
            if (solved) return board;
        }
        return null;
    }

    private bool Solve(int[,] board, BitArray given, bool circular,
        (int h, int w) size, (int x, int y) start, int last, (int x, int y) current, int n)
    {
        var (x, y) = current;
        if (x < 0 || x >= size.h || y < 0 || y >= size.w) return false;
        if (board[x, y] < 0) return false;
        if (given[n - 1]) {
            if (board[x, y] != n) return false;
        } else if (board[x, y] > 0) return false;
        board[x, y] = n;
        if (n == last) {
            if (!circular || AreNeighbors(start, current)) return true;
        }
        for (int i = 0; i < moves.Length; i++) {
            var move = moves[i];
            if (Solve(board, given, circular, size, start, last, (x + move.dx, y + move.dy), n + 1)) return true;
        }
        if (!given[n - 1]) board[x, y] = 0;
        return false;

        bool AreNeighbors((int x, int y) p1, (int x, int y) p2) => moves.Any(m => (p2.x + m.dx, p2.y + m.dy).Equals(p1));
    }

    private static (int[,] board, BitArray given, int count) Parse(string[] input)
    {
        (int height, int width) = (input.Length, input[0].Length);
        int[,] board = new int[height, width];
        int count = 0;
        for (int x = 0; x < height; x++) {
            string line = input[x];
            for (int y = 0; y < width; y++) {
                board[x, y] = y < line.Length && char.IsDigit(line[y]) ? line[y] - '0' : -1;
                if (board[x, y] >= 0) count++;
            }
        }
        BitArray given = Scan(board, count, height, width);
        return (board, given, count);
    }

    private static (int[,] board, BitArray given, int count) Parse(int[,] input)
    {
        (int height, int width) = (input.GetLength(0), input.GetLength(1));
        int[,] board = new int[height, width];
        int count = 0;
        for (int x = 0; x < height; x++)
            for (int y = 0; y < width; y++)
                if ((board[x, y] = input[x, y]) >= 0) count++;
        BitArray given = Scan(board, count, height, width);
        return (board, given, count);
    }

    private static BitArray Scan(int[,] board, int count, int height, int width)
    {
        var given = new BitArray(count + 1);
        for (int x = 0; x < height; x++)
            for (int y = 0; y < width; y++)
                if (board[x, y] > 0) given[board[x, y] - 1] = true;
        return given;
    }

    private static void Print(int[,] board)
    {
        if (board == null) {
            WriteLine("No solution");
        } else {
            int w = board.Cast<int>().Where(i => i > 0).Max(i => (int?)Ceiling(Log10(i+1))) ?? 1;
            string e = new string('-', w);
            foreach (int x in Range(0, board.GetLength(0)))
                WriteLine(string.Join(" ", Range(0, board.GetLength(1))
                    .Select(y => board[x, y] < 0 ? e : board[x, y].ToString().PadLeft(w, ' '))));
        }
        WriteLine();
    }

}
```

<pre style="height:30ex;overflow:scroll">
--  1  8 --  2  7 --
25 22 19 26 23 20 27
 9 16 13 10 17 14 11
--  4 24 21  3  6 --
-- -- 18 15 12 -- --
-- -- --  5 -- -- --

```



## D

From the refactored C++ version with more precise typing. This tries all possible start positions. The HopidoPuzzle struct is created at compile-time, so its pre-conditions can catch most malformed puzzles at compile-time.

```d
import std.stdio, std.conv, std.string, std.range, std.algorithm, std.typecons;


struct HopidoPuzzle {
    private alias InputCellBaseType = char;
    private enum InputCell : InputCellBaseType { available = '#', unavailable = '.' }
    private alias Cell = uint;
    private enum : Cell { unknownCell = 0, unavailableCell = Cell.max } // Special Cell values.

    // Neighbors, [shift row, shift column].
    private static immutable int[2][8] shifts = [[-2, -2], [2, -2], [-2, 2], [2, 2],
                                                 [ 0, -3], [0,  3], [-3, 0], [3, 0]];

    private immutable size_t gridWidth, gridHeight;
    private immutable Cell nAvailableCells;
    private /*immutable*/ const InputCell[] flatPuzzle;
    private Cell[] grid; // Flattened mutable game grid.

    @disable this();


    this(in string[] rawPuzzle) pure @safe
    in {
        assert(!rawPuzzle.empty);
        assert(!rawPuzzle[0].empty);
        assert(rawPuzzle.all!(row => row.length == rawPuzzle[0].length)); // Is rectangular.

        // Has at least one start point.
        assert(rawPuzzle.join.representation.canFind(InputCell.available));
    } body {
        //immutable puzzle = rawPuzzle.to!(InputCell[][]);
        immutable puzzle = rawPuzzle.map!representation.array.to!(InputCell[][]);

        gridWidth = puzzle[0].length;
        gridHeight = puzzle.length;
        flatPuzzle = puzzle.join;
        nAvailableCells = flatPuzzle.representation.count!(ic => ic == InputCell.available);

        grid = flatPuzzle
               .representation
               .map!(ic => ic == InputCell.available ? unknownCell : unavailableCell)
               .array;
    }


    Nullable!(string[][]) solve() pure /*nothrow*/ @safe
    out(result) {
        if (!result.isNull)
            assert(!grid.canFind(unknownCell));
    } body {
        // Try all possible start positions.
        foreach (immutable r; 0 ..  gridHeight) {
            foreach (immutable c; 0 .. gridWidth) {
                immutable pos = r * gridWidth + c;
                if (grid[pos] == unknownCell) {
                    immutable Cell startCell = 1; // To lay the first cell value.
                    grid[pos] = startCell;        // Try.
                    if (search(r, c, startCell + 1)) {
                        auto result = zip(flatPuzzle, grid)
                                      //.map!({p, c} => ...
                                      .map!(pc => (pc[0] == InputCell.available) ?
                                                  pc[1].text :
                                                  InputCellBaseType(pc[0]).text)
                                      .array
                                      .chunks(gridWidth)
                                      .array;
                        return typeof(return)(result);
                    }
                    grid[pos] = unknownCell; // Restore.
                }
            }
        }

        return typeof(return)();
    }


    private bool search(in size_t r, in size_t c, in Cell cell) pure nothrow @safe @nogc {
        if (cell > nAvailableCells)
            return true; // One solution found.

        foreach (immutable sh; shifts) {
            immutable r2 = r + sh[0],
                      c2 = c + sh[1],
                      pos = r2 * gridWidth + c2;
            // No need to test for >= 0 because uint wraps around.
            if (c2 < gridWidth && r2 < gridHeight && grid[pos] == unknownCell) {
                grid[pos] = cell;        // Try.
                if (search(r2, c2, cell + 1))
                    return true;
                grid[pos] = unknownCell; // Restore.
            }
        }

        return false;
    }
}


void main() @safe {
    // enum HopidoPuzzle to catch malformed puzzles at compile-time.
    enum puzzle = ".##.##.
                   #######
                   #######
                   .#####.
                   ..###..
                   ...#...".split.HopidoPuzzle;

    immutable solution = puzzle.solve; // Solved at run-time.
    if (solution.isNull)
        writeln("No solution found.");
    else
        writefln("One solution:\n%(%-(%2s %)\n%)", solution);
}
```

```txt
One solution:
 .  1  4  . 12  3  .
27 16 19 22 15 18 21
 5  8 11  2  7 10 13
 . 23 26 17 20 25  .
 .  .  6  9 14  .  .
 .  .  . 24  .  .  .
```



## Elixir

This solution uses HLPsolver from [[Solve_a_Hidato_puzzle#Elixir | here]]

```elixir
# require HLPsolver

adjacent = [{-3, 0}, {0, -3}, {0, 3}, {3, 0}, {-2, -2}, {-2, 2}, {2, -2}, {2, 2}]

board = """
. 0 0 . 0 0 .
0 0 0 0 0 0 0
0 0 0 0 0 0 0
. 0 0 0 0 0 .
. . 0 0 0 . .
. . . 1 . . .
"""
HLPsolver.solve(board, adjacent)
```


```txt

Problem:
    0  0     0  0
 0  0  0  0  0  0  0
 0  0  0  0  0  0  0
    0  0  0  0  0
       0  0  0
          1

Solution:
    5 25    17  3
27 13 10  7 14 11  8
24 21 18  4 22 19 16
    6 26 12  9  2
      23 20 15
          1

```



## Go

```go
package main

import (
    "fmt"
    "sort"
)

var board = []string{
    ".00.00.",
    "0000000",
    "0000000",
    ".00000.",
    "..000..",
    "...0...",
}

var moves = [][2]int{
    {-3, 0}, {0, 3}, {3, 0}, {0, -3},
    {2, 2}, {2, -2}, {-2, 2}, {-2, -2},
}

var grid [][]int

var totalToFill = 0

func solve(r, c, count int) bool {
    if count > totalToFill {
        return true
    }
    nbrs := neighbors(r, c)
    if len(nbrs) == 0 && count != totalToFill {
        return false
    }
    sort.Slice(nbrs, func(i, j int) bool {
        return nbrs[i][2] < nbrs[j][2]
    })

    for _, nb := range nbrs {
        r = nb[0]
        c = nb[1]
        grid[r][c] = count
        if solve(r, c, count+1) {
            return true
        }
        grid[r][c] = 0
    }
    return false
}

func neighbors(r, c int) (nbrs [][3]int) {
    for _, m := range moves {
        x := m[0]
        y := m[1]
        if grid[r+y][c+x] == 0 {
            num := countNeighbors(r+y, c+x) - 1
            nbrs = append(nbrs, [3]int{r + y, c + x, num})
        }
    }
    return
}

func countNeighbors(r, c int) int {
    num := 0
    for _, m := range moves {
        if grid[r+m[1]][c+m[0]] == 0 {
            num++
        }
    }
    return num
}

func printResult() {
    for _, row := range grid {
        for _, i := range row {
            if i == -1 {
                fmt.Print("   ")
            } else {
                fmt.Printf("%2d ", i)
            }
        }
        fmt.Println()
    }
}

func main() {
    nRows := len(board) + 6
    nCols := len(board[0]) + 6
    grid = make([][]int, nRows)
    for r := 0; r < nRows; r++ {
        grid[r] = make([]int, nCols)
        for c := 0; c < nCols; c++ {
            grid[r][c] = -1
        }
        for c := 3; c < nCols-3; c++ {
            if r >= 3 && r < nRows-3 {
                if board[r-3][c-3] == '0' {
                    grid[r][c] = 0
                    totalToFill++
                }
            }
        }
    }
    pos, r, c := -1, 0, 0
    for {
        for {
            pos++
            r = pos / nCols
            c = pos % nCols
            if grid[r][c] != -1 {
                break
            }
        }
        grid[r][c] = 1
        if solve(r, c, 2) {
            break
        }
        grid[r][c] = 0
        if pos >= nRows*nCols {
            break
        }
    }
    printResult()
}
```


```txt

             1 22    14 21
         18 10  7 17 11  8 16
          5 24 27  4 23 26 13
             2 19  9 15 20
                6 25 12
                   3

```


==Icon and {{header|Unicon}}==

Minor variant of [[Solve_a_Holy_Knight's_tour]].  Works in Unicon only.


```unicon
global nCells, cMap, best
record Pos(r,c)

procedure main(A)
    puzzle := showPuzzle("Input",readPuzzle())
    QMouse(puzzle,findStart(puzzle),&null,0)
    showPuzzle("Output", solvePuzzle(puzzle)) | write("No solution!")
end

procedure readPuzzle()
    # Start with a reduced puzzle space
    p := [[-1],[-1]]
    nCells := maxCols := 0
    every line := !&input do {
        put(p,[: -1 | -1 | gencells(line) | -1 | -1 :])
        maxCols <:= *p[-1]
        }
    every put(p, [-1]|[-1])
    # Now normalize all rows to the same length
    every i := 1 to *p do p[i] := [: !p[i] | (|-1\(maxCols - *p[i])) :]
    return p
end

procedure gencells(s)
    static WS, NWS
    initial {
        NWS := ~(WS := " \t")
        cMap := table()     # Map to/from internal model
        cMap["#"] := -1;  cMap["_"] :=  0
        cMap[-1]  := " "; cMap[0]   := "_"
        }

    s ? while not pos(0) do {
            w := (tab(many(WS))|"", tab(many(NWS))) | break
            w := numeric(\cMap[w]|w)
            if -1 ~= w then nCells +:= 1
            suspend w
            }
end

procedure showPuzzle(label, p)
    write(label," with ",nCells," cells:")
    every r := !p do {
        every c := !r do writes(right((\cMap[c]|c),*nCells+1))
        write()
        }
    return p
end

procedure findStart(p)
    if \p[r := !*p][c := !*p[r]] = 1 then return Pos(r,c)
end

procedure solvePuzzle(puzzle)
    if path := \best then {
        repeat {
            loc := path.getLoc()
            puzzle[loc.r][loc.c] := path.getVal()
            path := \path.getParent() | break
            }
        return puzzle
        }
end

class QMouse(puzzle, loc, parent, val)

    method getVal(); return val; end
    method getLoc(); return loc; end
    method getParent(); return parent; end
    method atEnd(); return nCells = val; end

    method visit(r,c)
        if /best & validPos(r,c) then return Pos(r,c)
    end

    method validPos(r,c)
        v := val+1
        xv := (0 <= puzzle[r][c]) | fail
        if xv = (v|0) then {  # make sure this path hasn't already gone there
            ancestor := self
            while xl := (ancestor := \ancestor.getParent()).getLoc() do
                if (xl.r = r) & (xl.c = c) then fail
            return
            }
    end

initially
    val := val+1
    if atEnd() then return best := self
    QMouse(puzzle, visit(loc.r-3,loc.c),   self, val)
    QMouse(puzzle, visit(loc.r-2,loc.c-2), self, val)
    QMouse(puzzle, visit(loc.r,  loc.c-3), self, val)
    QMouse(puzzle, visit(loc.r+2,loc.c-2), self, val)
    QMouse(puzzle, visit(loc.r+3,loc.c),   self, val)
    QMouse(puzzle, visit(loc.r+2,loc.c+2), self, val)
    QMouse(puzzle, visit(loc.r,  loc.c+3), self, val)
    QMouse(puzzle, visit(loc.r-2,loc.c+2), self, val)
end
```


Sample run:

```txt

->hopido <hopido1.in
Input with 27 cells:


           _  _     _  _
        _  _  _  _  _  _  _
        _  _  _  _  _  _  _
           _  _  _  _  _
              _  _  _
                 1


Output with 27 cells:


           3 21    13 22
       25  9  6 26 10  7 27
       20 17 14  2 18 15 12
           4 24  8  5 23
             19 16 11
                 1


->

```



## Java

```java
import java.util.*;

public class Hopido {

    final static String[] board = {
        ".00.00.",
        "0000000",
        "0000000",
        ".00000.",
        "..000..",
        "...0..."};

    final static int[][] moves = {{-3, 0}, {0, 3}, {3, 0}, {0, -3},
    {2, 2}, {2, -2}, {-2, 2}, {-2, -2}};
    static int[][] grid;
    static int totalToFill;

    public static void main(String[] args) {
        int nRows = board.length + 6;
        int nCols = board[0].length() + 6;

        grid = new int[nRows][nCols];

        for (int r = 0; r < nRows; r++) {
            Arrays.fill(grid[r], -1);
            for (int c = 3; c < nCols - 3; c++)
                if (r >= 3 && r < nRows - 3) {
                    if (board[r - 3].charAt(c - 3) == '0') {
                        grid[r][c] = 0;
                        totalToFill++;
                    }
                }
        }

        int pos = -1, r, c;
        do {
            do {
                pos++;
                r = pos / nCols;
                c = pos % nCols;
            } while (grid[r][c] == -1);

            grid[r][c] = 1;
            if (solve(r, c, 2))
                break;
            grid[r][c] = 0;

        } while (pos < nRows * nCols);

        printResult();
    }

    static boolean solve(int r, int c, int count) {
        if (count > totalToFill)
            return true;

        List<int[]> nbrs = neighbors(r, c);

        if (nbrs.isEmpty() && count != totalToFill)
            return false;

        Collections.sort(nbrs, (a, b) -> a[2] - b[2]);

        for (int[] nb : nbrs) {
            r = nb[0];
            c = nb[1];
            grid[r][c] = count;
            if (solve(r, c, count + 1))
                return true;
            grid[r][c] = 0;
        }

        return false;
    }

    static List<int[]> neighbors(int r, int c) {
        List<int[]> nbrs = new ArrayList<>();

        for (int[] m : moves) {
            int x = m[0];
            int y = m[1];
            if (grid[r + y][c + x] == 0) {
                int num = countNeighbors(r + y, c + x) - 1;
                nbrs.add(new int[]{r + y, c + x, num});
            }
        }
        return nbrs;
    }

    static int countNeighbors(int r, int c) {
        int num = 0;
        for (int[] m : moves)
            if (grid[r + m[1]][c + m[0]] == 0)
                num++;
        return num;
    }

    static void printResult() {
        for (int[] row : grid) {
            for (int i : row) {
                if (i == -1)
                    System.out.printf("%2s ", ' ');
                else
                    System.out.printf("%2d ", i);
            }
            System.out.println();
        }
    }
}
```



```txt

             1 22    14 21
         18 10  7 17 11  8 16
          5 24 27  4 23 26 13
             2 19  9 15 20
                6 25 12
                   3
```



## Julia

Uses the Hidato puzzle solver module, which has its source code listed [[Solve_a_Hidato_puzzle#Julia | here]] in the Hadato task.

```julia
using .Hidato       # Note that the . here means to look locally for the module rather than in the libraries

const hopid = """
 . 0 0 . 0 0 .
 0 0 0 0 0 0 0
 0 0 0 0 0 0 0
 . 0 0 0 0 0 .
 . . 0 0 0 . .
 . . . 0 . . . """

const hopidomoves = [[-3, 0], [0, -3], [-2, -2], [-2, 2], [2, -2], [0, 3], [3, 0], [2, 2]]

board, maxmoves, fixed, starts = hidatoconfigure(hopid)
printboard(board, " 0", "  ")
hidatosolve(board, maxmoves, hopidomoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

```
```txt

   0 0   0 0
 0 0 0 0 0 0 0
 0 0 0 0 0 0 0
   0 0 0 0 0
     0 0 0
       0

     4 15     5 16
  1 22 25  2 21 24 27
 14 11  8 17 12  9  6
     3 20 23 26 19
       13 10  7
          18

```



## Kotlin

```scala
// version 1.2.0

val board = listOf(
    ".00.00.",
    "0000000",
    "0000000",
    ".00000.",
    "..000..",
    "...0..."
)

val moves = listOf(
    -3 to 0, 0 to  3,  3 to 0,  0 to -3,
     2 to 2, 2 to -2, -2 to 2, -2 to -2
)

lateinit var grid: List<IntArray>
var totalToFill = 0

fun solve(r: Int, c: Int, count: Int): Boolean {
    if (count > totalToFill) return true
    val nbrs = neighbors(r, c)
    if (nbrs.isEmpty() && count != totalToFill) return false
    nbrs.sortBy { it[2] }
    for (nb in nbrs) {
        val rr = nb[0]
        val cc = nb[1]
        grid[rr][cc] = count
        if (solve(rr, cc, count + 1)) return true
        grid[rr][cc] = 0
    }
    return false
}

fun neighbors(r: Int, c: Int): MutableList<IntArray> {
    val nbrs = mutableListOf<IntArray>()
    for (m in moves) {
        val x = m.first
        val y = m.second
        if (grid[r + y][c + x] == 0) {
            val num = countNeighbors(r + y, c + x) - 1
            nbrs.add(intArrayOf(r + y, c + x, num))
        }
    }
    return nbrs
}

fun countNeighbors(r: Int, c: Int): Int {
    var num = 0
    for (m in moves)
        if (grid[r + m.second][c + m.first] == 0) num++
    return num
}

fun printResult() {
    for (row in grid) {
        for (i in row) {
            print(if (i == -1) "   " else "%2d ".format(i))
        }
        println()
    }
}

fun main(args: Array<String>) {
    val nRows = board.size + 6
    val nCols = board[0].length + 6
    grid = List(nRows) { IntArray(nCols) { -1} }
    for (r in 0 until nRows) {
        for (c in 3 until nCols - 3) {
            if (r in 3 until nRows - 3) {
                if (board[r - 3][c - 3] == '0') {
                    grid[r][c] = 0
                    totalToFill++
                }
            }
        }
    }
    var pos = -1
    var rr: Int
    var cc: Int
    do {
        do {
            pos++
            rr = pos / nCols
            cc = pos % nCols
        }
        while (grid[rr][cc] == -1)

        grid[rr][cc] = 1
        if (solve(rr, cc, 2)) break
        grid[rr][cc] = 0
    }
    while (pos < nRows * nCols)

    printResult()
}
```


```txt

             1 22    14 21
         18 10  7 17 11  8 16
          5 24 27  4 23 26 13
             2 19  9 15 20
                6 25 12
                   3

```



## Perl


```perl
#!/usr/bin/perl

use strict;             # http://www.rosettacode.org/wiki/Solve_a_Hopido_puzzle
use warnings;

$_ = do { local $/; <DATA> };
s/./$&$&/g;             # double chars
my $w = /\n/ && $-[0];
my $wd = 3 * $w + 1;    # vertical gap
my $wr = 2 * $w + 8;    # down right gap
my $wl = 2 * $w - 8;    # down left gap
place($_, '00');
die "No solution\n";

sub place
  {
  (local $_, my $last) = @_;
  (my $new = $last)++;
  /$last.{10}(?=00)/g   and place( s/\G00/$new/r, $new ); # right
  /(?=00.{10}$last)/g   and place( s/\G00/$new/r, $new ); # left
  /$last.{$wd}(?=00)/gs and place( s/\G00/$new/r, $new ); # down
  /(?=00.{$wd}$last)/gs and place( s/\G00/$new/r, $new ); # up
  /$last.{$wr}(?=00)/gs and place( s/\G00/$new/r, $new ); # down right
  /(?=00.{$wr}$last)/gs and place( s/\G00/$new/r, $new ); # up left
  /$last.{$wl}(?=00)/gs and place( s/\G00/$new/r, $new ); # down left
  /(?=00.{$wl}$last)/gs and place( s/\G00/$new/r, $new ); # up right
  /00/ and return;
  print "Solution\n\n", s/  / /gr =~ s/0\B/ /gr;
  exit;
  }

__DATA__
. 0 0 . 0 0 .
0 0 0 0 0 0 0
0 0 0 0 0 0 0
. 0 0 0 0 0 .
. . 0 0 0 . .
. . . 0 . . .
```

```txt

Solution

..  2 24 ..  1 25 ..
 7 10 13  6  9 12  5
15 22 19 16 23 20 17
..  3  8 11  4 26 ..
.. .. 14 21 18 .. ..
.. .. .. 27 .. .. ..

```


## Perl 6

This uses a Warnsdorff solver, which cuts down the number of tries by more than a factor of six over the brute force approach. This same solver is used in:

* [[Solve a Hidato puzzle#Perl_6|Solve a Hidato puzzle]]
* [[Solve a Hopido puzzle#Perl_6|Solve a Hopido puzzle]]
* [[Solve a Holy Knight's tour#Perl_6|Solve a Holy Knight's tour]]
* [[Solve a Numbrix puzzle#Perl_6|Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle#Perl_6|Solve the no connection puzzle]]


```perl6
my @adjacent = [3, 0],
      [2, -2],         [2, 2],
   [0, -3],                [0, 3],
      [-2, -2],        [-2, 2],
               [-3, 0];

solveboard q:to/END/;
    . _ _ . _ _ .
    _ _ _ _ _ _ _
    _ _ _ _ _ _ _
    . _ _ _ _ _ .
    . . _ _ _ . .
    . . . 1 . . .
    END

sub solveboard($board) {
    my $max = +$board.comb(/\w+/);
    my $width = $max.chars;

    my @grid;
    my @known;
    my @neigh;
    my @degree;

    @grid = $board.lines.map: -> $line {
        [ $line.words.map: { /^_/ ?? 0 !! /^\./ ?? Rat !! $_ } ]
    }

    sub neighbors($y,$x --> List) {
        eager gather for @adjacent {
            my $y1 = $y + .[0];
            my $x1 = $x + .[1];
            take [$y1,$x1] if defined @grid[$y1][$x1];
        }
    }

    for ^@grid -> $y {
        for ^@grid[$y] -> $x {
            if @grid[$y][$x] -> $v {
                @known[$v] = [$y,$x];
            }
            if @grid[$y][$x].defined {
                @neigh[$y][$x] = neighbors($y,$x);
                @degree[$y][$x] = +@neigh[$y][$x];
            }
        }
    }
    print "\e[0H\e[0J";

    my $tries = 0;

    try_fill 1, @known[1];

    sub try_fill($v, $coord [$y,$x] --> Bool) {
        return True if $v > $max;
        $tries++;

        my $old = @grid[$y][$x];

        return False if +$old and $old != $v;
        return False if @known[$v] and @known[$v] !eqv $coord;

        @grid[$y][$x] = $v;               # conjecture grid value

        print "\e[0H";                    # show conjectured board
        for @grid -> $r {
            say do for @$r {
                when Rat { ' ' x $width }
                when 0   { '_' x $width }
                default  { .fmt("%{$width}d") }
            }
        }


        my @neighbors = @neigh[$y][$x][];

        my @degrees;
        for @neighbors -> \n [$yy,$xx] {
            my $d = --@degree[$yy][$xx];  # conjecture new degrees
            push @degrees[$d], n;         # and categorize by degree
        }

        for @degrees.grep(*.defined) -> @ties {
            for @ties.reverse {           # reverse works better for this hidato anyway
                return True if try_fill $v + 1, $_;
            }
        }

        for @neighbors -> [$yy,$xx] {
            ++@degree[$yy][$xx];          # undo degree conjectures
        }

        @grid[$y][$x] = $old;             # undo grid value conjecture
        return False;
    }

    say "$tries tries";
}
```


```txt
   21  4    20  3
26 12 15 25 11 14 24
17  6  9 18  5  8 19
   22 27 13 23  2
      16  7 10
          1
59 tries
```



## Phix

Simple brute force approach.

```Phix
sequence board

integer limit, tries

constant ROW = 1, COL = 2
constant moves = {{-2,-2},{-2,2},{2,-2},{2,2},{-3,0},{3,0},{0,-3},{0,3}}

function solve(integer row, integer col, integer n)
integer nrow, ncol
    tries+= 1
    if n>limit then return 1 end if
    for move=1 to length(moves) do
        nrow = row+moves[move][ROW]
        ncol = col+moves[move][COL]*3
        if nrow>=1 and nrow<=length(board)
        and ncol>=1 and ncol<=length(board[row])
        and board[nrow][ncol]=' ' then
            board[nrow][ncol-1..ncol] = sprintf("%2d",n)
            if solve(nrow,ncol,n+1) then return 1 end if
            board[nrow][ncol-1..ncol] = "  "
        end if
    end for
    return 0
end function

procedure Hopido(sequence s, integer w, integer h)
integer x, y
atom t0 = time()
    board = split(s,'\n')
    limit = 0
    for x=1 to h do
        for y=3 to w*3 by 3 do
            if board[x][y]='0' then
                board[x][y] = ' '
                limit += 1
            end if
        end for
    end for
    while 1 do
        x = rand(h)
        y = rand(w)*3
        if board[x][y]=' ' then exit end if
    end while
    board[x][y] = '1'
    tries = 0
    if solve(x,y,2) then
        puts(1,join(board,"\n"))
        printf(1,"\nsolution found in %d tries (%3.2fs)\n",{tries,time()-t0})
    else
        puts(1,"no solutions found\n")
    end if
end procedure

constant board1 = """
  .  0  0  .  0  0  .
  0  0  0  0  0  0  0
  0  0  0  0  0  0  0
  .  0  0  0  0  0  .
  .  .  0  0  0  .  .
  .  .  .  0  .  .  ."""
Hopido(board1,7,6)
```

The best and worse cases observed were:

```txt

  . 13 22  . 14 11  .
  6  3 25  7  4  1 27
 23 20 17 12 21 18 15
  .  8  5  2 26 10  .
  .  . 24 19 16  .  .
  .  .  .  9  .  .  .
solution found in 46 tries (0.00s)
  . 20 11  . 19 22  .
  2  5  8  1  4  7 27
 10 13 16 21 12 15 18
  . 25  3  6 26 23  .
  .  .  9 14 17  .  .
  .  .  . 24  .  .  .
solution found in 67702 tries (0.09s)

```



## Python


```python

from sys import stdout

neighbours = [[2, 2], [-2, 2], [2, -2], [-2, -2], [3, 0], [0, 3], [-3, 0], [0, -3]]
cnt = 0
pWid = 0
pHei = 0


def is_valid(a, b):
    return -1 < a < pWid and -1 < b < pHei


def iterate(pa, x, y, v):
    if v > cnt:
        return 1

    for i in range(len(neighbours)):
        a = x + neighbours[i][0]
        b = y + neighbours[i][1]
        if is_valid(a, b) and pa[a][b] == 0:
            pa[a][b] = v
            r = iterate(pa, a, b, v + 1)
            if r == 1:
                return r
            pa[a][b] = 0
    return 0


def solve(pz, w, h):
    global cnt, pWid, pHei

    pa = [[-1 for j in range(h)] for i in range(w)]
    f = 0
    pWid = w
    pHei = h
    for j in range(h):
        for i in range(w):
            if pz[f] == "1":
                pa[i][j] = 0
                cnt += 1
            f += 1

    for y in range(h):
        for x in range(w):
            if pa[x][y] == 0:
                pa[x][y] = 1
                if 1 == iterate(pa, x, y, 2):
                    return 1, pa
                pa[x][y] = 0

    return 0, pa

r = solve("011011011111111111111011111000111000001000", 7, 6)
if r[0] == 1:
    for j in range(6):
        for i in range(7):
            if r[1][i][j] == -1:
                stdout.write("   ")
            else:
                stdout.write(" {:0{}d}".format(r[1][i][j], 2))
        print()
else:
    stdout.write("No solution!")

```
 {{out}}
```txt

    01 25    17 03
 27 13 10 07 14 11 08
 24 21 18 02 22 19 16
    06 26 12 09 04
       23 20 15
          05

```



## Racket


This solution uses the module "hidato-family-solver.rkt" from
[[Solve a Numbrix puzzle#Racket]]. The difference between the two is
essentially the neighbourhood function.


```racket
#lang racket
(require "hidato-family-solver.rkt")

(define hoppy-moore-neighbour-offsets
  '((+3 0) (-3 0) (0 +3) (0 -3) (+2 +2) (-2 -2) (-2 +2) (+2 -2)))

(define solve-hopido (solve-hidato-family hoppy-moore-neighbour-offsets))

(displayln
 (puzzle->string
  (solve-hopido
   #(#(_ 0 0 _ 0 0 _)
     #(0 0 0 0 0 0 0)
     #(0 0 0 0 0 0 0)
     #(_ 0 0 0 0 0 _)
     #(_ _ 0 0 0 _ _)
     #(_ _ _ 0 _ _ _)))))

```


```txt
 _  2 20  _  3 19  _
 7 10 13  6  9 12  5
15 22 25 16 21 24 27
 _  1  8 11  4 18  _
 _  _ 14 23 26  _  _
 _  _  _ 17  _  _  _
```



## REXX

This REXX program is a slightly modified version of the REXX    '''Hidato'''   program.

No particular effort was made to reduce the elapsed time in solving the puzzle.

```rexx
/*REXX program solves a Hopido puzzle,  it also displays the puzzle  and  the solution. */
call time 'Reset'                                /*reset the REXX elapsed timer to zero.*/
maxR=0;    maxC=0;    maxX=0;     minR=9e9;      minC=9e9;    minX=9e9;    cells=0;    @.=
parse arg xxx                                    /*get the cell definitions from the CL.*/
xxx=translate(xxx, , "/\;:_", ',')               /*also allow other characters as comma.*/

               do  while xxx\='';       parse var  xxx    r  c  marks  ','  xxx
                   do  while marks\='';               _=@.r.c
                   parse var  marks  x  marks
                   if datatype(x,'N')   then  x=x/1                   /*normalize   X.  */
                   minR=min(minR,r); maxR=max(maxR,r);  minC=min(minC,c); maxC=max(maxC,c)
                   if x==1   then do;  !r=r;  !c=c;  end              /*the START cell. */
                   if _\=='' then call err "cell at"  r  c  'is already occupied with:' _
                   @.r.c=x;   c=c+1;    cells=cells+1                 /*assign a mark.  */
                   if x==.              then iterate                  /*is a hole?  Skip*/
                   if \datatype(x,'W')  then call err 'illegal marker specified:' x
                   minX=min(minX,x);    maxX=max(maxX,x)              /*min and max  X. */
                   end   /*while marks¬='' */
               end       /*while xxx  ¬='' */
call show                                        /* [↓]  is used for making fast moves. */
Nr = '0  3   0  -3    -2   2   2  -2'            /*possible  row     for the next move. */
Nc = '3  0  -3   0     2  -2   2  -2'            /*   "      column   "   "    "    "   */
pMoves=words(Nr)                                 /*the number of possible moves.  */
                   do i=1  for pMoves;   Nr.i=word(Nr, i);   Nc.i=word(Nc,i);   end  /*i*/
if \next(2,!r,!c)  then call err  'No solution possible for this Hopido puzzle.'
say 'A solution for the Hopido exists.';      say;               call show
etime= format(time('Elapsed'), , 2)              /*obtain the elapsed time (in seconds).*/
if etime<.1  then say 'and took less than  1/10  of a second.'
             else say 'and took'       etime         "seconds."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:  say;      say '***error*** (from Hopido): '  arg(1);          say;           exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
next: procedure expose @. Nr. Nc. cells pMoves;  parse arg #,r,c;   ##=#+1
           do t=1  for pMoves                                   /* [↓]  try some moves. */
           parse value  r+Nr.t c+Nc.t  with nr nc  /*next move coördinates*/
           if @.nr.nc==.  then do;                @.nr.nc=#     /*let's try this move.  */
                               if #==cells        then leave    /*is this the last move?*/
                               if next(##,nr,nc)  then return 1
                               @.nr.nc=.                        /*undo the above move.  */
                               iterate                          /*go & try another move.*/
                               end
           if @.nr.nc==#  then do                               /*this a fill-in move ? */
                               if #==cells        then return 1 /*this is the last move.*/
                               if next(##,nr,nc)  then return 1 /*a fill-in move.       */
                               end
           end   /*t*/
return 0                                                        /*This ain't working.   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: if maxR<1 | maxC<1  then call err  'no legal cell was specified.'
      if minX<1           then call err  'no  1  was specified for the puzzle start'
      w=max(2,length(cells));  do    r=maxR  to minR  by -1; _=
                                  do c=minC  to maxC;        _=_ right(@.r.c,w); end /*c*/
                               say _
                               end   /*r*/
      say;    return
```

'''output'''   when the input is:

<tt> 1 4 1 \2 3 . . . \3 2 . . . . . \4 1 . . . . . . . \5 1 . . . . . . . \6 2 . . \6 5 . . </tt>

```txt

     .  .     .  .
  .  .  .  .  .  .  .
  .  .  .  .  .  .  .
     .  .  .  .  .
        .  .  .
           1

A solution for the Hopido exists.

     5 12     4 11
  8 22 25  7 21 24 27
 13 16 19  2 15 18  3
     6  9 23 26 10
       14 17 20
           1

and took less than  1/10  of a second.

```



## Ruby

This solution uses HLPsolver from [[Solve_a_Hidato_puzzle#With_Warnsdorff | here]]

```ruby
require 'HLPsolver'

ADJACENT = [[-3, 0], [0, -3], [0, 3], [3, 0], [-2, -2], [-2, 2], [2, -2], [2, 2]]

board1 = <<EOS
. 0 0 . 0 0 .
0 0 0 0 0 0 0
0 0 0 0 0 0 0
. 0 0 0 0 0 .
. . 0 0 0 . .
. . . 1 . . .
EOS
t0 = Time.now
HLPsolver.new(board1).solve
puts " #{Time.now - t0} sec"
```

Which produces:

```txt

Problem:
     0  0     0  0
  0  0  0  0  0  0  0
  0  0  0  0  0  0  0
     0  0  0  0  0
        0  0  0
           1

Solution:
     3 12     4 11
  8 18 21  7 17 20  6
 13 24 27 14 23 26 15
     2  9 19  5 10
       22 25 16
           1

 0.001 sec

```



## Tcl

```tcl
package require Tcl 8.6

oo::class create HopidoSolver {
    variable grid start limit
    constructor {puzzle} {
	set grid $puzzle
	for {set y 0} {$y < [llength $grid]} {incr y} {
	    for {set x 0} {$x < [llength [lindex $grid $y]]} {incr x} {
		if {[set cell [lindex $grid $y $x]] == 1} {
		    set start [list $y $x]
		}
		incr limit [expr {$cell>=0}]
	    }
	}
	if {![info exist start]} {
	    return -code error "no starting position found"
	}
    }
    method moves {} {
	return {
	            0 -3
	      -2 -2      -2 2
	    -3 0            3 0
              -2 2        2 2
	            0 3
	}
    }
    method Moves {g r c} {
	set valid {}
	foreach {dr dc} [my moves] {
	    set R [expr {$r + $dr}]
	    set C [expr {$c + $dc}]
	    if {[lindex $g $R $C] == 0} {
		lappend valid $R $C
	    }
	}
	return $valid
    }

    method Solve {g r c v} {
	lset g $r $c [incr v]
	if {$v >= $limit} {return $g}
	foreach {r c} [my Moves $g $r $c] {
	    return [my Solve $g $r $c $v]
	}
	return -code continue
    }

    method solve {} {
	while {[incr i]==1} {
	    set grid [my Solve $grid {*}$start 0]
	    return
	}
	return -code error "solution not possible"
    }
    method solution {} {return $grid}
}

proc parsePuzzle {str} {
    foreach line [split $str "\n"] {
	if {[string trim $line] eq ""} continue
	lappend rows [lmap {- c} [regexp -all -inline {(.)\s?} $line] {
	    string map {" " -1 "." -1} $c
	}]
    }
    set len [tcl::mathfunc::max {*}[lmap r $rows {llength $r}]]
    for {set i 0} {$i < [llength $rows]} {incr i} {
	while {[llength [lindex $rows $i]] < $len} {
	    lset rows $i end+1 -1
	}
    }
    return $rows
}
proc showPuzzle {grid name} {
    foreach row $grid {foreach cell $row {incr c [expr {$cell>=0}]}}
    set len [string length $c]
    set u [string repeat "_" $len]
    puts "$name with $c cells"
    foreach row $grid {
	puts [format "  %s" [join [lmap c $row {
	    format "%*s" $len [if {$c==-1} list elseif {$c==0} {set u} {set c}]
	}]]]
    }
}
set puzzle [parsePuzzle {
. 0 0 . 0 0 .
0 0 0 0 0 0 0
0 0 0 0 0 0 0
. 0 0 0 0 0 .
. . 0 0 0 . .
. . . 1 . . .
}]
showPuzzle $puzzle "Input"
HopidoSolver create hop $puzzle
hop solve
showPuzzle [hop solution] "Output"
```

```txt

Input with 27 cells
     __ __    __ __
  __ __ __ __ __ __ __
  __ __ __ __ __ __ __
     __ __ __ __ __
        __ __ __
            1
Output with 27 cells
      3  6    23  7
  27 11 14 26 10 13 25
   5 17 20  4 16 19 22
      2  9 12 24  8
        15 18 21
            1

```



## zkl

This solution uses the code from [[Solve_a_Numbrix_puzzle#zkl]]

```zkl
hi:=  // 0==empty cell, X==not a cell
#<<<
"   X 0 0 X 0 0 X
    0 0 0 0 0 0 0
    0 0 0 0 0 0 0
    X 0 0 0 0 0 X
    X X 0 0 0 X X
    X X X 0 X X X";
#<<<
adjacent:=T( T(-3,0),
      T(-2,-2),   T(-2,2),
    T(0,-3),         T(0,3),
       T(2,-2),   T(2,2),
             T(3,0) );

puzzle:=Puzzle(hi,adjacent);
puzzle.print_board();
puzzle.solve();
println();
puzzle.print_board();
println();
```

```txt

Number of cells = 27
   __ __    __ __
__ __ __ __ __ __ __
__ __ __ __ __ __ __
   __ __ __ __ __
      __ __ __
         __

    1  8     2  9
12 24 21 13 25 22 14
19  6  3 18  7  4 27
   16 11 23 15 10
      20  5 26
         17

```


