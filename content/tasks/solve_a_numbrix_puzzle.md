+++
title = "Solve a Numbrix puzzle"
description = ""
date = 2019-08-05T10:48:43Z
aliases = []
[extra]
id = 17656
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
  "systemverilog",
  "tcl",
  "zkl",
]
+++

Numbrix puzzles are similar to [[Solve a Hidato puzzle|Hidato]].
The most important difference is that it is only possible to move 1 node left, right, up, or down (sometimes referred to as the [[wp:Von Neumann neighborhood|Von Neumann neighborhood]]).
Published puzzles also tend not to have holes in the grid and may not always indicate the end node.
Two examples follow:

;Example 1
Problem.

```txt

 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0

```

Solution.

```txt

 49 50 51 52 53 54 75 76 81
 48 47 46 45 44 55 74 77 80
 37 38 39 40 43 56 73 78 79
 36 35 34 41 42 57 72 71 70
 31 32 33 14 13 58 59 68 69
 30 17 16 15 12 61 60 67 66
 29 18 19 20 11 62 63 64 65
 28 25 24 21 10  1  2  3  4
 27 26 23 22  9  8  7  6  5

```

;Example 2
Problem.

```txt

 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0

```

Solution.

```txt

  9 10 13 14 19 20 63 64 65
  8 11 12 15 18 21 62 61 66
  7  6  5 16 17 22 59 60 67
 34 33  4  3 24 23 58 57 68
 35 32 31  2 25 54 55 56 69
 36 37 30  1 26 53 74 73 70
 39 38 29 28 27 52 75 72 71
 40 43 44 47 48 51 76 77 78
 41 42 45 46 49 50 81 80 79

```

## Task

Write a program to solve puzzles of this ilk,
demonstrating your program by solving the above examples.
Extra credit for other interesting examples.


## Related tasks

* [[A* search algorithm]]
* [[Solve a Holy Knight's tour]]
* [[Knight's tour]]
* [[N-queens problem]]
* [[Solve a Hidato puzzle]]
* [[Solve a Holy Knight's tour]]
* [[Solve a Hopido puzzle]]
* [[Solve the no connection puzzle]]





## AutoHotkey


```AutoHotkey
SolveNumbrix(Grid, Locked, Max, row, col, num:=1, R:="", C:=""){
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
		if SolveNumbrix(Grid, Locked, Max, row, col, num)	; solve for current location and value
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

			if SolveNumbrix(Grid, Locked, Max, row, col, num, R, C)	; solve for current location, neighbor and value
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
	return row-1 ":" col
	. "," row+1 ":" col
	. "," row ":" col+1
	. "," row ":" col-1
}
;--------------------------------
map(Grid){
	for i, row in Grid
	{
		for j, element in row
			line .= (A_Index > 1 ? "`t" : "") . element
		map .= (map<>""?"`n":"") line
		line := ""
	}
	return StrReplace(map, ">")
}
```

Examples:
```AutoHotkey
;--------------------------------
Grid := [[0,	0,	0,	0,	0,	0,	0,	0,	0]
	,[0,	0,	46,	45,	0,	55,	74,	0,	0]
	,[0,	38,	0,	0,	43,	0,	0,	78,	0]
	,[0,	35,	0,	0,	0,	0,	0,	71,	0]
	,[0,	0,	33,	0,	0,	0,	59,	0,	0]
	,[0,	17,	0,	0,	0,	0,	0,	67,	0]
	,[0,	18,	0,	0,	11,	0,	0,	64,	0]
	,[0,	0,	24,	21,	0,	1,	2,	0,	0]
	,[0,	0,	0,	0,	0,	0,	0,	0,	0]]
;--------------------------------
; find locked cells, find row and col of first value "1" and max value
Locked := []
max := 1
for i, line in Grid
	for j, element in line
	{
		max ++
		if element = 1
			row :=i , col := j
		if (element > 0)
			Locked[element] := i ":" j "," Neighbor(i, j)	; save locked elements position and neighbors

	}
;--------------------------------
MsgBox, 262144, ,% SolveNumbrix(Grid, Locked, Max, row, col)
return


```

Outputs:
```txt
49	50	51	52	53	54	75	76	81
48	47	46	45	44	55	74	77	80
37	38	39	40	43	56	73	78	79
36	35	34	41	42	57	72	71	70
31	32	33	14	13	58	59	68	69
30	17	16	15	12	61	60	67	66
29	18	19	20	11	62	63	64	65
28	25	24	21	10	1	2	3	4
27	26	23	22	9	8	7	6	5
```



## C++


```cpp

#include <vector>
#include <sstream>
#include <iostream>
#include <iterator>
#include <cstdlib>
#include <string>
#include <bitset>

using namespace std;
typedef bitset<4> hood_t;

struct node
{
	int val;
	hood_t neighbors;
};

class nSolver
{
public:

	void solve(vector<string>& puzz, int max_wid)
	{
		if (puzz.size() < 1) return;
		wid = max_wid;
		hei = static_cast<int>(puzz.size()) / wid;
		max = wid * hei;
		int len = max, c = 0;
		arr = vector<node>(len, node({ 0, 0 }));
		weHave = vector<bool>(len + 1, false);

		for (const auto& s : puzz)
		{
			if (s == "*") { max--; arr[c++].val = -1; continue; }
			arr[c].val = atoi(s.c_str());
			if (arr[c].val > 0) weHave[arr[c].val] = true;
			c++;
		}

		solveIt(); c = 0;
		for (auto&& s : puzz)
		{
			if (s == ".")
				s = std::to_string(arr[c].val);
			c++;
		}
	}

private:
	bool search(int x, int y, int w, int dr)
	{
		if ((w > max && dr > 0) || (w < 1 && dr < 0) || (w == max && weHave[w])) return true;

		node& n = arr[x + y * wid];
		n.neighbors = getNeighbors(x, y);
		if (weHave[w])
		{
			for (int d = 0; d < 4; d++)
			{
				if (n.neighbors[d])
				{
					int a = x + dx[d], b = y + dy[d];
					if (arr[a + b * wid].val == w)
						if (search(a, b, w + dr, dr))
							return true;
				}
			}
			return false;
		}

		for (int d = 0; d < 4; d++)
		{
			if (n.neighbors[d])
			{
				int a = x + dx[d], b = y + dy[d];
				if (arr[a + b * wid].val == 0)
				{
					arr[a + b * wid].val = w;
					if (search(a, b, w + dr, dr))
						return true;
					arr[a + b * wid].val = 0;
				}
			}
		}
		return false;
	}

	hood_t getNeighbors(int x, int y)
	{
		hood_t retval;
		for (int xx = 0; xx < 4; xx++)
		{
			int a = x + dx[xx], b = y + dy[xx];
			if (a < 0 || b < 0 || a >= wid || b >= hei)
				continue;
			if (arr[a + b * wid].val > -1)
				retval.set(xx);
		}
		return retval;
	}

	void solveIt()
	{
		int x, y, z; findStart(x, y, z);
		if (z == 99999) { cout << "\nCan't find start point!\n"; return; }
		search(x, y, z + 1, 1);
		if (z > 1) search(x, y, z - 1, -1);
	}

	void findStart(int& x, int& y, int& z)
	{
		z = 99999;
		for (int b = 0; b < hei; b++)
		for (int a = 0; a < wid; a++)
		if (arr[a + wid * b].val > 0 && arr[a + wid * b].val < z)
		{
			x = a; y = b;
			z = arr[a + wid * b].val;
		}

	}

	vector<int> dx = vector<int>({ -1, 1, 0, 0 });
	vector<int> dy = vector<int>({ 0, 0, -1, 1 });
	int wid, hei, max;
	vector<node> arr;
	vector<bool> weHave;
};

//------------------------------------------------------------------------------
int main(int argc, char* argv[])
{
	int wid; string p;
	//p = ". . . . . . . . . . . 46 45 . 55 74 . . . 38 . . 43 . . 78 . . 35 . . . . . 71 . . . 33 . . . 59 . . . 17 . . . . . 67 . . 18 . . 11 . . 64 . . . 24 21 . 1  2 . . . . . . . . . . ."; wid = 9;
	//p = ". . . . . . . . . . 11 12 15 18 21 62 61 . .  6 . . . . . 60 . . 33 . . . . . 57 . . 32 . . . . . 56 . . 37 .  1 . . . 73 . . 38 . . . . . 72 . . 43 44 47 48 51 76 77 . . . . . . . . . ."; wid = 9;
	p = "17 . . . 11 . . . 59 . 15 . . 6 . . 61 . . . 3 . . .  63 . . . . . . 66 . . . . 23 24 . 68 67 78 . 54 55 . . . . 72 . . . . . . 35 . . . 49 . . . 29 . . 40 . . 47 . 31 . . . 39 . . . 45"; wid = 9;

	istringstream iss(p); vector<string> puzz;
	copy(istream_iterator<string>(iss), istream_iterator<string>(), back_inserter<vector<string> >(puzz));
	nSolver s; s.solve(puzz, wid);

	int c = 0;
	for (const auto& s : puzz)
	{
		if (s != "*" && s != ".")
		{
			if (atoi(s.c_str()) < 10) cout << "0";
			cout << s << " ";
		}
		else cout << "   ";
		if (++c >= wid) { cout << endl; c = 0; }
	}
	cout << endl << endl;
	return system("pause");
}

```

```txt

49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10 01 02 03 04
27 26 23 22 09 08 07 06 05

09 10 13 14 19 20 63 64 65
08 11 12 15 18 21 62 61 66
07 06 05 16 17 22 59 60 67
34 33 04 03 24 23 58 57 68
35 32 31 02 25 54 55 56 69
36 37 30 01 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

17 16 13 12 11 10 09 60 59
18 15 14 05 06 07 08 61 58
19 20 03 04 65 64 63 62 57
22 21 02 01 66 79 80 81 56
23 24 69 68 67 78 77 54 55
26 25 70 71 72 75 76 53 52
27 28 35 36 73 74 49 50 51
30 29 34 37 40 41 48 47 46
31 32 33 38 39 42 43 44 45

```



## C#

The same solver can solve Hidato, Holy Knight's Tour, Hopido and Numbrix puzzles.<br/>
The input can be an array of strings if each cell is one character. The length of the first row must be the number of columns in the puzzle.<br/>
Any non-numeric value indicates a no-go.<br/>
If there are cells that require more characters, then a 2-dimensional array of ints must be used. Any number < 0 indicates a no-go.

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
        numbrixMoves = {(1,0),(0,1),(-1,0),(0,-1)};

    private (int dx, int dy)[] moves;

    public static void Main()
    {
        var numbrixSolver = new Solver(numbrixMoves);
        Print(numbrixSolver.Solve(false, new [,] {
            {  0,  0,  0,  0,  0,  0,  0,  0,  0 },
            {  0,  0, 46, 45,  0, 55, 74,  0,  0 },
            {  0, 38,  0,  0, 43,  0,  0, 78,  0 },
            {  0, 35,  0,  0,  0,  0,  0, 71,  0 },
            {  0,  0, 33,  0,  0,  0, 59,  0,  0 },
            {  0, 17,  0,  0,  0,  0,  0, 67,  0 },
            {  0, 18,  0,  0, 11,  0,  0, 64,  0 },
            {  0,  0, 24, 21,  0,  1,  2,  0,  0 },
            {  0,  0,  0,  0,  0,  0,  0,  0,  0 },
        }));

        Print(numbrixSolver.Solve(false, new [,] {
            {  0,  0,  0,  0,  0,  0,  0,  0,  0 },
            {  0, 11, 12, 15, 18, 21, 62, 61,  0 },
            {  0,  6,  0,  0,  0,  0,  0, 60,  0 },
            {  0, 33,  0,  0,  0,  0,  0, 57,  0 },
            {  0, 32,  0,  0,  0,  0,  0, 56,  0 },
            {  0, 37,  0,  1,  0,  0,  0, 73,  0 },
            {  0, 38,  0,  0,  0,  0,  0, 72,  0 },
            {  0, 43, 44, 47, 48, 51, 76, 77,  0 },
            {  0,  0,  0,  0,  0,  0,  0,  0,  0 },
        }));
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

```txt

49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

```



## D

From the refactored C++ version with more precise typing. The NumbrixPuzzle struct is created at compile-time, so its asserts and exceptions can catch most malformed puzzles at compile-time.
```d
import std.stdio, std.conv, std.string, std.range, std.array, std.typecons, std.algorithm;

struct  {
    alias BitSet8 = ubyte; // A set of 8 bits.
    alias Cell = uint;
    enum : string { unavailableInCell = "#", availableInCell = "." }
    enum : Cell { unavailableCell = Cell.max, availableCell = 0 }

    this(in string inPuzzle) pure @safe {
        const rawPuzzle = inPuzzle.splitLines.map!(row => row.split).array;
        assert(!rawPuzzle.empty);
        assert(!rawPuzzle[0].empty);
        assert(rawPuzzle.all!(row => row.length == rawPuzzle[0].length)); // Is rectangular.

        gridWidth = rawPuzzle[0].length;
        gridHeight = rawPuzzle.length;
        immutable nMaxCells = gridWidth * gridHeight;
        grid = new Cell[nMaxCells];
        auto knownMutable = new bool[nMaxCells + 1];
        uint nAvailableMutable = nMaxCells;
        bool[Cell] seenCells; // To avoid duplicate input numbers.

        uint i = 0;
        foreach (const piece; rawPuzzle.join) {
            if (piece == unavailableInCell) {
                nAvailableMutable--;
                grid[i++] = unavailableCell;
                continue;
            } else if (piece == availableInCell) {
                grid[i] = availableCell;
            } else {
                immutable cell = piece.to!Cell;
                assert(cell > 0 && cell <= nMaxCells);
                assert(cell !in seenCells);
                seenCells[cell] = true;
                knownMutable[cell] = true;
                grid[i] = cell;
            }

            i++;
        }

        known = knownMutable.idup;
        nAvailable = nAvailableMutable;
    }

    @disable this();


    auto solve() pure nothrow @safe @nogc
    out(result) {
        if (!result.isNull) {
            // Can't verify 'result' here because it's const.
            // assert(!result.get.join.canFind(availableCell.text));

            assert(!grid.canFind(availableCell));
            auto values = grid.filter!(c => c != unavailableCell);
            auto interval = iota(reduce!min(values.front, values.dropOne),
                                 reduce!max(values.front, values.dropOne) + 1);
            assert(values.walkLength == interval.length);
            assert(interval.all!(c => values.count(c) == 1)); // Quadratic.
        }
    } body {
        auto result = grid
                      .map!(c => (c == unavailableCell) ? unavailableInCell : c.text)
                      .chunks(gridWidth);
        alias OutRange = Nullable!(typeof(result));

        const start = findStart;
        if (start.isNull)
            return OutRange();

        search(start.r, start.c, start.cell + 1, 1);
        if (start.cell > 1) {
            immutable direction = -1;
            search(start.r, start.c, start.cell + direction, direction);
        }

        if (grid.any!(c => c == availableCell))
            return OutRange();
        else
            return OutRange(result);
    }

    private:


    bool search(in uint r, in uint c, in Cell cell, in int direction)
    pure nothrow @safe @nogc {
        if ((cell > nAvailable && direction > 0) || (cell == 0 && direction < 0) ||
            (cell == nAvailable && known[cell]))
            return true; // One solution found.

        immutable neighbors = getNeighbors(r, c);

        if (known[cell]) {
            foreach (immutable i, immutable rc; shifts) {
                if (neighbors & (1u << i)) {
                    immutable c2 = c + rc[0],
                              r2 = r + rc[1];
                    if (grid[r2 * gridWidth + c2] == cell)
                        if (search(r2, c2, cell + direction, direction))
                            return true;
                }
            }
            return false;
        }

        foreach (immutable i, immutable rc; shifts) {
            if (neighbors & (1u << i)) {
                immutable c2 = c + rc[0],
                          r2 = r + rc[1],
                          pos = r2 * gridWidth + c2;
                if (grid[pos] == availableCell) {
                    grid[pos] = cell;          // Try.
                    if (search(r2, c2, cell + direction, direction))
                        return true;
                    grid[pos] = availableCell; // Restore.
                }
            }
        }
        return false;
    }


    BitSet8 getNeighbors(in uint r, in uint c) const pure nothrow @safe @nogc {
        typeof(return) usable = 0;

        foreach (immutable i, immutable rc; shifts) {
            immutable c2 = c + rc[0],
                      r2 = r + rc[1];
            if (c2 >= gridWidth || r2 >= gridHeight)
                continue;
            if (grid[r2 * gridWidth + c2] != unavailableCell)
                usable |= (1u << i);
        }

        return usable;
    }


    auto findStart() const pure nothrow @safe @nogc {
        alias Triple = Tuple!(uint,"r", uint,"c", Cell,"cell");
        Nullable!Triple result;

        auto cell = Cell.max;
        foreach (immutable r; 0 .. gridHeight) {
            foreach (immutable c; 0 .. gridWidth) {
                immutable pos = gridWidth * r + c;
                if (grid[pos] != availableCell &&
                    grid[pos] != unavailableCell && grid[pos] < cell) {
                    cell = grid[pos];
                    result = Triple(r, c, cell);
                }
            }
        }

        return result;
    }

    static immutable int[2][4] shifts = [[0, -1], [0, 1], [-1, 0], [1, 0]];
    immutable uint gridWidth, gridHeight;
    immutable int nAvailable;
    immutable bool[] known; // Given known cells of the puzzle.
    Cell[] grid;  // Flattened mutable game grid.
}


void main() {
    // enum NumbrixPuzzle to catch malformed puzzles at compile-time.
    enum puzzle1 = ".  .  .  .  .  .  .  .  .
                    .  . 46 45  . 55 74  .  .
                    . 38  .  . 43  .  . 78  .
                    . 35  .  .  .  .  . 71  .
                    .  . 33  .  .  . 59  .  .
                    . 17  .  .  .  .  . 67  .
                    . 18  .  . 11  .  . 64  .
                    .  . 24 21  .  1  2  .  .
                    .  .  .  .  .  .  .  .  .".NumbrixPuzzle;

    enum puzzle2 = ".  .  .  .  .  .  .  .  .
                    . 11 12 15 18 21 62 61  .
                    .  6  .  .  .  .  . 60  .
                    . 33  .  .  .  .  . 57  .
                    . 32  .  .  .  .  . 56  .
                    . 37  .  1  .  .  . 73  .
                    . 38  .  .  .  .  . 72  .
                    . 43 44 47 48 51 76 77  .
                    .  .  .  .  .  .  .  .  .".NumbrixPuzzle;

    enum puzzle3 = "17  .  .  . 11  .  .  . 59
                     . 15  .  .  6  .  . 61  .
                     .  .  3  .  .  . 63  .  .
                     .  .  .  . 66  .  .  .  .
                    23 24  . 68 67 78  . 54 55
                     .  .  .  . 72  .  .  .  .
                     .  . 35  .  .  . 49  .  .
                     . 29  .  . 40  .  . 47  .
                    31  .  .  . 39  .  .  . 45".NumbrixPuzzle;


    foreach (puzzle; [puzzle1, puzzle2, puzzle3]) {
        auto solution = puzzle.solve; // Solved at run-time.
        if (solution.isNull)
            writeln("No solution found for puzzle.\n");
        else
            writefln("One solution:\n%(%-(%2s %)\n%)\n", solution);
    }
}
```

```txt
One solution:
49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

One solution:
 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

One solution:
17 16 13 12 11 10  9 60 59
18 15 14  5  6  7  8 61 58
19 20  3  4 65 64 63 62 57
22 21  2  1 66 79 80 81 56
23 24 69 68 67 78 77 54 55
26 25 70 71 72 75 76 53 52
27 28 35 36 73 74 49 50 51
30 29 34 37 40 41 48 47 46
31 32 33 38 39 42 43 44 45
```



## Elixir

This solution uses HLPsolver from [[Solve_a_Hidato_puzzle#Elixir | here]]

```elixir
# require HLPsolver

adjacent = [{-1, 0}, {0, -1}, {0, 1}, {1, 0}]

board1 = """
 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0
"""
HLPsolver.solve(board1, adjacent)

board2 = """
 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0
"""
HLPsolver.solve(board2, adjacent)
```


```txt

Problem:
 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0

Solution:
49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

Problem:
 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0

Solution:
 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

```



## Go

```go
package main

import (
    "fmt"
    "sort"
    "strconv"
    "strings"
)

var example1 = []string{
    "00,00,00,00,00,00,00,00,00",
    "00,00,46,45,00,55,74,00,00",
    "00,38,00,00,43,00,00,78,00",
    "00,35,00,00,00,00,00,71,00",
    "00,00,33,00,00,00,59,00,00",
    "00,17,00,00,00,00,00,67,00",
    "00,18,00,00,11,00,00,64,00",
    "00,00,24,21,00,01,02,00,00",
    "00,00,00,00,00,00,00,00,00",
}

var example2 = []string{
    "00,00,00,00,00,00,00,00,00",
    "00,11,12,15,18,21,62,61,00",
    "00,06,00,00,00,00,00,60,00",
    "00,33,00,00,00,00,00,57,00",
    "00,32,00,00,00,00,00,56,00",
    "00,37,00,01,00,00,00,73,00",
    "00,38,00,00,00,00,00,72,00",
    "00,43,44,47,48,51,76,77,00",
    "00,00,00,00,00,00,00,00,00",
}

var moves = [][2]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}

var (
    grid        [][]int
    clues       []int
    totalToFill = 0
)

func solve(r, c, count, nextClue int) bool {
    if count > totalToFill {
        return true
    }

    back := grid[r][c]

    if back != 0 && back != count {
        return false
    }

    if back == 0 && nextClue < len(clues) && clues[nextClue] == count {
        return false
    }

    if back == count {
        nextClue++
    }

    grid[r][c] = count
    for _, move := range moves {
        if solve(r+move[1], c+move[0], count+1, nextClue) {
            return true
        }
    }
    grid[r][c] = back
    return false
}

func printResult(n int) {
    fmt.Println("Solution for example", n, "\b:")
    for _, row := range grid {
        for _, i := range row {
            if i == -1 {
                continue
            }
            fmt.Printf("%2d ", i)
        }
        fmt.Println()
    }
}

func main() {
    for n, board := range [2][]string{example1, example2} {
        nRows := len(board) + 2
        nCols := len(strings.Split(board[0], ",")) + 2
        startRow, startCol := 0, 0
        grid = make([][]int, nRows)
        totalToFill = (nRows - 2) * (nCols - 2)
        var lst []int

        for r := 0; r < nRows; r++ {
            grid[r] = make([]int, nCols)
            for c := 0; c < nCols; c++ {
                grid[r][c] = -1
            }
            if r >= 1 && r < nRows-1 {
                row := strings.Split(board[r-1], ",")
                for c := 1; c < nCols-1; c++ {
                    val, _ := strconv.Atoi(row[c-1])
                    if val > 0 {
                        lst = append(lst, val)
                    }
                    if val == 1 {
                        startRow, startCol = r, c
                    }
                    grid[r][c] = val
                }
            }
        }

        sort.Ints(lst)
        clues = lst
        if solve(startRow, startCol, 1, 0) {
            printResult(n + 1)
        }
    }
}
```


```txt

Solution for example 1:

49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

Solution for example 2:

 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

```


==Icon and {{header|Unicon}}==

This is a Unicon-specific solution, based on the Unicon Hidato problem solver:

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
    p := []
    nCells := maxCols := 0
    every line := !&input do {
        put(p,[: gencells(line) :])
        maxCols <:= *p[-1]
        }
    # Now normalize all rows to the same length
    every i := 1 to *p do p[i] := [: !p[i] | (|-1\(maxCols - *p[i])) :]
    return p
end

procedure gencells(s)
    static WS, NWS
    initial {
        NWS := ~(WS := " \t")
        cMap := table()     # Map to/from internal model
        cMap["_"] :=  0; cMap[0]   := "_"
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
    method atEnd(); return (nCells = val, puzzle[loc.r,loc.c] = (val|0)); end
    method visit(r,c); return (/best, validPos(r,c), Pos(r,c)); end

    method validPos(r,c)
        v := val+1      # number we're looking for
        xv := puzzle[r,c] | fail
        if (xv ~= 0) & (xv != v) then fail
        if xv = (0|v) then {
            ancestor := self
            while xl := (ancestor := \ancestor.getParent()).getLoc() do
                if (xl.r = r) & (xl.c = c) then fail
            return
            }
    end

initially
    val := val+1
    if atEnd() then return best := self
    QMouse(puzzle, visit(loc.r-1,loc.c)  , self, val)   # North
    QMouse(puzzle, visit(loc.r,  loc.c+1), self, val)   # East
    QMouse(puzzle, visit(loc.r+1,loc.c),   self, val)   # South
    QMouse(puzzle, visit(loc.r,  loc.c-1), self, val)   # West
end
```


{{Out}}Sample runs:

```txt

->numbrix <numbrix1.in
Input with 81 cells:

     _  _  _  _  _  _  _  _  _
     _  _ 46 45  _ 55 74  _  _
     _ 38  _  _ 43  _  _ 78  _
     _ 35  _  _  _  _  _ 71  _
     _  _ 33  _  _  _ 59  _  _
     _ 17  _  _  _  _  _ 67  _
     _ 18  _  _ 11  _  _ 64  _
     _  _ 24 21  _  1  2  _  _
     _  _  _  _  _  _  _  _  _

Output with 81 cells:

    49 50 51 52 53 54 75 76 81
    48 47 46 45 44 55 74 77 80
    37 38 39 40 43 56 73 78 79
    36 35 34 41 42 57 72 71 70
    31 32 33 14 13 58 59 68 69
    30 17 16 15 12 61 60 67 66
    29 18 19 20 11 62 63 64 65
    28 25 24 21 10  1  2  3  4
    27 26 23 22  9  8  7  6  5

->numbrix <numbrix2.in
Input with 81 cells:

     _  _  _  _  _  _  _  _  _
     _ 11 12 15 18 21 62 61  _
     _  6  _  _  _  _  _ 60  _
     _ 33  _  _  _  _  _ 57  _
     _ 32  _  _  _  _  _ 56  _
     _ 37  _  1  _  _  _ 73  _
     _ 38  _  _  _  _  _ 72  _
     _ 43 44 47 48 51 76 77  _
     _  _  _  _  _  _  _  _  _

Output with 81 cells:

     9 10 13 14 19 20 63 64 65
     8 11 12 15 18 21 62 61 66
     7  6  5 16 17 22 59 60 67
    34 33  4  3 24 23 58 57 68
    35 32 31  2 25 54 55 56 69
    36 37 30  1 26 53 74 73 70
    39 38 29 28 27 52 75 72 71
    40 43 44 47 48 51 76 77 78
    41 42 45 46 49 50 81 80 79

->

```



## Java

```java
import java.util.*;

public class Numbrix {

    final static String[] board = {
        "00,00,00,00,00,00,00,00,00",
        "00,00,46,45,00,55,74,00,00",
        "00,38,00,00,43,00,00,78,00",
        "00,35,00,00,00,00,00,71,00",
        "00,00,33,00,00,00,59,00,00",
        "00,17,00,00,00,00,00,67,00",
        "00,18,00,00,11,00,00,64,00",
        "00,00,24,21,00,01,02,00,00",
        "00,00,00,00,00,00,00,00,00"};

    final static int[][] moves = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};

    static int[][] grid;
    static int[] clues;
    static int totalToFill;

    public static void main(String[] args) {
        int nRows = board.length + 2;
        int nCols = board[0].split(",").length + 2;
        int startRow = 0, startCol = 0;

        grid = new int[nRows][nCols];
        totalToFill = (nRows - 2) * (nCols - 2);
        List<Integer> lst = new ArrayList<>();

        for (int r = 0; r < nRows; r++) {
            Arrays.fill(grid[r], -1);

            if (r >= 1 && r < nRows - 1) {

                String[] row = board[r - 1].split(",");

                for (int c = 1; c < nCols - 1; c++) {
                    int val = Integer.parseInt(row[c - 1]);
                    if (val > 0)
                        lst.add(val);
                    if (val == 1) {
                        startRow = r;
                        startCol = c;
                    }
                    grid[r][c] = val;
                }
            }
        }

        clues = lst.stream().sorted().mapToInt(i -> i).toArray();

        if (solve(startRow, startCol, 1, 0))
            printResult();
    }

    static boolean solve(int r, int c, int count, int nextClue) {
        if (count > totalToFill)
            return true;

        if (grid[r][c] != 0 && grid[r][c] != count)
            return false;

        if (grid[r][c] == 0 && nextClue < clues.length)
            if (clues[nextClue] == count)
                return false;

        int back = grid[r][c];
        if (back == count)
            nextClue++;

        grid[r][c] = count;
        for (int[] move : moves)
            if (solve(r + move[1], c + move[0], count + 1, nextClue))
                return true;

        grid[r][c] = back;
        return false;
    }

    static void printResult() {
        for (int[] row : grid) {
            for (int i : row) {
                if (i == -1)
                    continue;
                System.out.printf("%2d ", i);
            }
            System.out.println();
        }
    }
}
```



```txt
49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5
```


=={{header|Julia}}
See the Hidato module here.

```julia
using .Hidato

const numbrixmoves = [[-1, 0], [0, -1], [0, 1], [1, 0]]

board, maxmoves, fixed, starts = hidatoconfigure(numbrix1)
printboard(board, " 0 ", "   ")
hidatosolve(board, maxmoves, numbrixmoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

board, maxmoves, fixed, starts = hidatoconfigure(numbrix2)
printboard(board, " 0 ", "   ")
hidatosolve(board, maxmoves, numbrixmoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

```
```txt

 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0

 49 50 51 52 53 54 75 76 81
 48 47 46 45 44 55 74 77 80
 37 38 39 40 43 56 73 78 79
 36 35 34 41 42 57 72 71 70
 31 32 33 14 13 58 59 68 69
 30 17 16 15 12 61 60 67 66
 29 18 19 20 11 62 63 64 65
 28 25 24 21 10  1  2  3  4
 27 26 23 22  9  8  7  6  5

 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0

  9 10 13 14 19 20 63 64 65
  8 11 12 15 18 21 62 61 66
  7  6  5 16 17 22 59 60 67
 34 33  4  3 24 23 58 57 68
 35 32 31  2 25 54 55 56 69
 36 37 30  1 26 53 74 73 70
 39 38 29 28 27 52 75 72 71
 40 43 44 47 48 51 76 77 78
 41 42 45 46 49 50 81 80 79

```




## Julia

Uses the Hidato puzzle solver module, which has its source code listed [[Solve_a_Hidato_puzzle#Julia | here]]  in the Hadato task.

```julia
using .Hidato       # Note that the . here means to look locally for the module rather than in the libraries

const numbrix1 = """
 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0 """

const numbrix2 = """
 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0 """

const numbrixmoves = [[-1, 0], [0, -1], [0, 1], [1, 0]]

board, maxmoves, fixed, starts = hidatoconfigure(numbrix1)
printboard(board, " 0 ", "   ")
hidatosolve(board, maxmoves, numbrixmoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

board, maxmoves, fixed, starts = hidatoconfigure(numbrix2)
printboard(board, " 0 ", "   ")
hidatosolve(board, maxmoves, numbrixmoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

```
```txt

 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0

 49 50 51 52 53 54 75 76 81
 48 47 46 45 44 55 74 77 80
 37 38 39 40 43 56 73 78 79
 36 35 34 41 42 57 72 71 70
 31 32 33 14 13 58 59 68 69
 30 17 16 15 12 61 60 67 66
 29 18 19 20 11 62 63 64 65
 28 25 24 21 10  1  2  3  4
 27 26 23 22  9  8  7  6  5

 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0

  9 10 13 14 19 20 63 64 65
  8 11 12 15 18 21 62 61 66
  7  6  5 16 17 22 59 60 67
 34 33  4  3 24 23 58 57 68
 35 32 31  2 25 54 55 56 69
 36 37 30  1 26 53 74 73 70
 39 38 29 28 27 52 75 72 71
 40 43 44 47 48 51 76 77 78
 41 42 45 46 49 50 81 80 79

```



## Kotlin

```scala
// version 1.2.0

val example1 = listOf(
    "00,00,00,00,00,00,00,00,00",
    "00,00,46,45,00,55,74,00,00",
    "00,38,00,00,43,00,00,78,00",
    "00,35,00,00,00,00,00,71,00",
    "00,00,33,00,00,00,59,00,00",
    "00,17,00,00,00,00,00,67,00",
    "00,18,00,00,11,00,00,64,00",
    "00,00,24,21,00,01,02,00,00",
    "00,00,00,00,00,00,00,00,00"
)

val example2 = listOf(
    "00,00,00,00,00,00,00,00,00",
    "00,11,12,15,18,21,62,61,00",
    "00,06,00,00,00,00,00,60,00",
    "00,33,00,00,00,00,00,57,00",
    "00,32,00,00,00,00,00,56,00",
    "00,37,00,01,00,00,00,73,00",
    "00,38,00,00,00,00,00,72,00",
    "00,43,44,47,48,51,76,77,00",
    "00,00,00,00,00,00,00,00,00"
)

val moves = listOf(1 to 0, 0 to 1, -1 to 0, 0 to -1)

lateinit var board: List<String>
lateinit var grid: List<IntArray>
lateinit var clues: IntArray
var totalToFill = 0

fun solve(r: Int, c: Int, count: Int, nextClue: Int): Boolean {
    if (count > totalToFill) return true
    val back = grid[r][c]
    if (back != 0 && back != count) return false
    if (back == 0 && nextClue < clues.size && clues[nextClue] == count) {
        return false
    }
    var nextClue2 = nextClue
    if (back == count) nextClue2++
    grid[r][c] = count
    for (m in moves) {
        if (solve(r + m.second, c + m.first, count + 1, nextClue2)) return true
    }
    grid[r][c] = back
    return false
}

fun printResult(n: Int) {
    println("Solution for example $n:")
    for (row in grid) {
        for (i in row) {
            if (i == -1) continue
            print("%2d ".format(i))
        }
        println()
    }
}

fun main(args: Array<String>) {
    for ((n, ex) in listOf(example1, example2).withIndex()) {
        board = ex
        val nRows = board.size + 2
        val nCols = board[0].split(",").size + 2
        var startRow = 0
        var startCol = 0
        grid = List(nRows) { IntArray(nCols) { -1 } }
        totalToFill = (nRows - 2) * (nCols - 2)
        val lst = mutableListOf<Int>()
        for (r in 0 until nRows) {
            if (r in 1 until nRows - 1) {
                val row = board[r - 1].split(",")
                for (c in 1 until nCols - 1) {
                    val value = row[c - 1].toInt()
                    if (value > 0) lst.add(value)
                    if (value == 1) {
                        startRow = r
                        startCol = c
                    }
                    grid[r][c] = value
                }
            }
        }
        lst.sort()
        clues = lst.toIntArray()
        if (solve(startRow, startCol, 1, 0)) printResult(n + 1)
    }
}
```


```txt

Solution for example 1:

49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

Solution for example 2:

 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

```



## Perl

Tested on perl v5.26.1

```Perl
#!/usr/bin/perl

use strict;
use warnings;

$_ = <<END;
 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0
END

my $gap = /.\n/ * $-[0];
print;
s/ (?=\d\b)/0/g;
my $max = sprintf "%02d", tr/0-9// / 2;

solve( '01', $_ );

sub solve
  {
  my ($have, $in) = @_;
  $have eq $max and exit !print "solution\n", $in =~ s/\b0/ /gr;
  if( $in =~ ++(my $want = $have) )
    {
    $in =~ /($have|$want)( |.{$gap})($have|$want)/s and solve($want, $in);
    }
  else
    {
    ($_ = $in) =~ s/$have \K00/$want/          and solve( $want, $_ ); # R
    ($_ = $in) =~ s/$have.{$gap}\K00/$want/s   and solve( $want, $_ ); # D
    ($_ = $in) =~ s/00(?= $have)/$want/        and solve( $want, $_ ); # L
    ($_ = $in) =~ s/00(?=.{$gap}$have)/$want/s and solve( $want, $_ ); # U
    }
  }
```


<b>

```txt
 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0
solution
49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

```

</b>


## Perl 6

This uses a Warnsdorff solver, which cuts down the number of tries by more than a factor of six over the brute force approach. This same solver is used in:

* [[Solve a Hidato puzzle#Perl_6|Solve a Hidato puzzle]]
* [[Solve a Hopido puzzle#Perl_6|Solve a Hopido puzzle]]
* [[Solve a Holy Knight's tour#Perl_6|Solve a Holy Knight's tour]]
* [[Solve a Numbrix puzzle#Perl_6|Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle#Perl_6|Solve the no connection puzzle]]


```perl6
my @adjacent =           [-1, 0],
               [ 0, -1],          [ 0, 1],
                         [ 1, 0];
put "\n" xx 60;

solveboard q:to/END/;
    __ __ __ __ __ __ __ __ __
    __ __ 46 45 __ 55 74 __ __
    __ 38 __ __ 43 __ __ 78 __
    __ 35 __ __ __ __ __ 71 __
    __ __ 33 __ __ __ 59 __ __
    __ 17 __ __ __ __ __ 67 __
    __ 18 __ __ 11 __ __ 64 __
    __ __ 24 21 __  1  2 __ __
    __ __ __ __ __ __ __ __ __
    END

# And
put "\n" xx 60;

solveboard q:to/END/;
    0  0  0  0  0  0  0  0  0
    0 11 12 15 18 21 62 61  0
    0  6  0  0  0  0  0 60  0
    0 33  0  0  0  0  0 57  0
    0 32  0  0  0  0  0 56  0
    0 37  0  1  0  0  0 73  0
    0 38  0  0  0  0  0 72  0
    0 43 44 47 48 51 76 77  0
    0  0  0  0  0  0  0  0  0
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
49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5
1275 tries


 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79
4631 tries
```

Oddly, reversing the tiebreaker rule that makes hidato run twice as fast causes this last example to run four times slower.  Go figure...


## Phix


```Phix
sequence board, knownx, knowny

integer size, limit, nchars, tries
string fmt, blank

constant ROW = 1, COL = 2
constant moves = {{-1,0},{0,-1},{0,1},{1,0}}

function onboard(integer row, integer col)
    return row>=1 and row<=size and col>=nchars and col<=nchars*size
end function

function solve(integer row, integer col, integer n)
integer nrow, ncol
    tries+= 1
    if n>limit then return 1 end if
    if knownx[n] then
        for move=1 to length(moves) do
            nrow = row+moves[move][ROW]
            ncol = col+moves[move][COL]*nchars
            if nrow = knownx[n]
            and ncol = knowny[n] then
                if solve(nrow,ncol,n+1) then return 1 end if
                exit
            end if
        end for
        return 0
    end if
    sequence wmoves = {}
    for move=1 to length(moves) do
        nrow = row+moves[move][ROW]
        ncol = col+moves[move][COL]*nchars
        if onboard(nrow,ncol)
        and board[nrow][ncol]='.' then
            board[nrow][ncol-nchars+1..ncol] = sprintf(fmt,n)
            if solve(nrow,ncol,n+1) then return 1 end if
            board[nrow][ncol-nchars+1..ncol] = blank
        end if
    end for
    return 0
end function

procedure Numbrix(sequence s)
integer y, ch, ch2, k
atom t0 = time()
    s = split(s,'\n')
    size = length(s)
    limit = size*size
    nchars = length(sprintf(" %d",limit))
    fmt = sprintf(" %%%dd",nchars-1)
    blank = repeat('.',nchars)
    board = repeat(repeat(' ',size*nchars),size)
    knownx = repeat(0,limit)
    knowny = repeat(0,limit)
    for x=1 to size do
        for y=nchars to size*nchars by nchars do
            ch = s[x][y]
            if ch!='.' then
                k = ch-'0'
                ch2 = s[x][y-1]
                if ch2!=' ' then
                    k += (ch2-'0')*10
                    board[x][y-1] = ch2
                end if
                knownx[k] = x
                knowny[k] = y
            end if
            board[x][y] = ch
        end for
    end for
    tries = 0
    if solve(knownx[1],knowny[1],2) then
        puts(1,join(board,"\n"))
        printf(1,"\nsolution found in %d tries (%3.2fs)\n",{tries,time()-t0})
    else
        puts(1,"no solutions found\n")
    end if
end procedure

constant board1 = """
  .  .  .  .  .  .  .  .  .
  .  . 46 45  . 55 74  .  .
  . 38  .  . 43  .  . 78  .
  . 35  .  .  .  .  . 71  .
  .  . 33  .  .  . 59  .  .
  . 17  .  .  .  .  . 67  .
  . 18  .  . 11  .  . 64  .
  .  . 24 21  .  1  2  .  .
  .  .  .  .  .  .  .  .  ."""
Numbrix(board1)

constant board2 = """
  .  .  .  .  .  .  .  .  .
  . 11 12 15 18 21 62 61  .
  .  6  .  .  .  .  . 60  .
  . 33  .  .  .  .  . 57  .
  . 32  .  .  .  .  . 56  .
  . 37  .  1  .  .  . 73  .
  . 38  .  .  .  .  . 72  .
  . 43 44 47 48 51 76 77  .
  .  .  .  .  .  .  .  .  ."""
Numbrix(board2)
```

```txt

 49 50 51 52 53 54 75 76 81
 48 47 46 45 44 55 74 77 80
 37 38 39 40 43 56 73 78 79
 36 35 34 41 42 57 72 71 70
 31 32 33 14 13 58 59 68 69
 30 17 16 15 12 61 60 67 66
 29 18 19 20 11 62 63 64 65
 28 25 24 21 10  1  2  3  4
 27 26 23 22  9  8  7  6  5
solution found in 580 tries (0.00s)
  9 10 13 14 19 20 63 64 65
  8 11 12 15 18 21 62 61 66
  7  6  5 16 17 22 59 60 67
 34 33  4  3 24 23 58 57 68
 35 32 31  2 25 54 55 56 69
 36 37 30  1 26 53 74 73 70
 39 38 29 28 27 52 75 72 71
 40 43 44 47 48 51 76 77 78
 41 42 45 46 49 50 81 80 79
solution found in 334 tries (0.00s)

```



## Python


```python

from sys import stdout
neighbours = [[-1, 0], [0, -1], [1, 0], [0, 1]]
exists = []
lastNumber = 0
wid = 0
hei = 0


def find_next(pa, x, y, z):
    for i in range(4):
        a = x + neighbours[i][0]
        b = y + neighbours[i][1]
        if wid > a > -1 and hei > b > -1:
            if pa[a][b] == z:
                return a, b

    return -1, -1


def find_solution(pa, x, y, z):
    if z > lastNumber:
        return 1
    if exists[z] == 1:
        s = find_next(pa, x, y, z)
        if s[0] < 0:
            return 0
        return find_solution(pa, s[0], s[1], z + 1)

    for i in range(4):
        a = x + neighbours[i][0]
        b = y + neighbours[i][1]
        if wid > a > -1 and hei > b > -1:
            if pa[a][b] == 0:
                pa[a][b] = z
                r = find_solution(pa, a, b, z + 1)
                if r == 1:
                    return 1
                pa[a][b] = 0

    return 0


def solve(pz, w, h):
    global lastNumber, wid, hei, exists

    lastNumber = w * h
    wid = w
    hei = h
    exists = [0 for j in range(lastNumber + 1)]

    pa = [[0 for j in range(h)] for i in range(w)]
    st = pz.split()
    idx = 0
    for j in range(h):
        for i in range(w):
            if st[idx] == ".":
                idx += 1
            else:
                pa[i][j] = int(st[idx])
                exists[pa[i][j]] = 1
                idx += 1

    x = 0
    y = 0
    t = w * h + 1
    for j in range(h):
        for i in range(w):
            if pa[i][j] != 0 and pa[i][j] < t:
                t = pa[i][j]
                x = i
                y = j

    return find_solution(pa, x, y, t + 1), pa


def show_result(r):
    if r[0] == 1:
        for j in range(hei):
            for i in range(wid):
                stdout.write(" {:0{}d}".format(r[1][i][j], 2))
            print()
    else:
        stdout.write("No Solution!\n")

    print()


r = solve(". . . . . . . . . . . 46 45 . 55 74 . . . 38 . . 43 . . 78 . . 35 . . . . . 71 . . . 33 . . . 59 . . . 17"
          " . . . . . 67 . . 18 . . 11 . . 64 . . . 24 21 . 1  2 . . . . . . . . . . .", 9, 9)
show_result(r)

r = solve(". . . . . . . . . . 11 12 15 18 21 62 61 . .  6 . . . . . 60 . . 33 . . . . . 57 . . 32 . . . . . 56 . . 37"
          " .  1 . . . 73 . . 38 . . . . . 72 . . 43 44 47 48 51 76 77 . . . . . . . . . .", 9, 9)
show_result(r)

r = solve("17 . . . 11 . . . 59 . 15 . . 6 . . 61 . . . 3 . . .  63 . . . . . . 66 . . . . 23 24 . 68 67 78 . 54 55"
          " . . . . 72 . . . . . . 35 . . . 49 . . . 29 . . 40 . . 47 . 31 . . . 39 . . . 45", 9, 9)
show_result(r)

```
```txt

49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10 01 02 03 04
27 26 23 22 09 08 07 06 05

09 10 13 14 19 20 63 64 65
08 11 12 15 18 21 62 61 66
07 06 05 16 17 22 59 60 67
34 33 04 03 24 23 58 57 68
35 32 31 02 25 54 55 56 69
36 37 30 01 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

17 16 13 12 11 10 09 60 59
18 15 14 05 06 07 08 61 58
19 20 03 04 65 64 63 62 57
22 21 00 00 66 79 80 81 56
23 24 69 68 67 78 77 54 55
26 25 70 71 72 75 76 53 52
27 28 35 36 73 74 49 50 51
30 29 34 37 40 41 48 47 46
31 32 33 38 39 42 43 44 45

```



## Racket


This is a general "Hidato" style solver (which is why there is a search
for a 0 start point (which supports [[Solve a Hopido puzzle|Hopido]]). There is
already a Racket implementation of [[Solve a Hidato puzzle|Hidato]], so to
allow a variety of approaches to be demonstrated, the main library for this set
of problems is here.

<code>hidato-family-solver.rkt</code>

```racket
#lang racket
;;; Used in my solutions of:
;;; "Solve a Hidato Puzzle"
;;; "Solve a Holy Knights Tour"
;;; "Solve a Numbrix Puzzle"
;;; "Solve a Hopido Puzzle"

;;; As well as the solver being common, the solution renderer and input formats are common
(provide
 ;; Input:  list of neighbour offsets
 ;; Output: a solver function:
 ;;         Input:  a puzzle
 ;;         Output: either the solved puzzle or #f if impossible
 solve-hidato-family
 ;; Input:  puzzle
 ;;         optional minimum cell width
 ;; Output: a pretty string that can be printed
 puzzle->string)

;; Cell values are:
;; zero?     - unvisited
;; positive? - nth visitied
;; else      - unvisitable. In the puzzle layout, it's a _. In the hash it's a -1, so we can care less
;;                          about number type checking.
;; A puzzle is a sequence of sequences of cell values
;; We work with a puzzle as a hash keyed on (cons row-num col-num)

;; Take a puzzle and get a working hash of it
(define (puzzle->hash p)
  (for*/hash
      (((r row-num) (in-parallel p (in-naturals)))
       ((v col-num) (in-parallel r (in-naturals)))
       #:when (integer? v))
    (values (cons row-num col-num) v)))

;; Takes a hash and recreates a vector of vectors puzzle
(define (hash->puzzle h# (blank '_))
  (define keys (hash-keys h#))
  (define n-rows (add1 (car (argmax car keys))))
  (define n-cols (add1 (cdr (argmax cdr keys))))
  (for/vector #:length n-rows ((r n-rows))
    (for/vector #:length n-cols ((c n-cols))
      (hash-ref h# (cons r c) blank))))

;; See "provide" section for description
(define (puzzle->string p (w #f))
  (match p
    [#f "unsolved"]
    [(? sequence? s)
     (define (max-n-digits p)
       (and p (add1 (order-of-magnitude (* (vector-length p) (vector-length (vector-ref p 0)))))))
     (define min-width (or w (max-n-digits p)))
     (string-join
      (for/list ((r s))
        (string-join
         (for/list ((c r)) (~a c #:align 'right #:min-width min-width))
         " "))
      "\n")]))

(define ((solve-hidato-family neighbour-offsets) board)
  (define board# (puzzle->hash board))
  ;; reverse mapping, will only take note of positive values
  (define targets# (for/hash ([(k v) (in-hash board#)] #:when (positive? v)) (values v k)))

  (define (neighbours r.c)
    (for/list ((r+.c+ neighbour-offsets))
      (match-define (list r+ c+) r+.c+)
      (match-define (cons r  c ) r.c)
      (cons (+ r r+) (+ c c+))))

  ;; Count the moves, rather than check for "no more zeros" in puzzle
  (define last-move (length (filter number? (hash-values board#))))

  ;; Depth first solution of the puzzle (we have to go deep, it's where the solutions are!
  (define (inr-solve-pzl b# move r.c)
    (cond
      [(= move last-move) b#] ; no moves needed, so solved
      [else
       (define m++ (add1 move))
       (for*/or ; check each neighbour as an option
           ((r.c+ (in-list (neighbours r.c)))
            #:when (equal? (hash-ref targets# move r.c) r.c) ; we're where we should be!
            #:when (match (hash-ref b# r.c+ -1) (0 #t) ((== m++) #t) (_ #f)))
         (inr-solve-pzl (hash-set b# r.c+ m++) m++ r.c+))]))

  (define (solution-starting-at n)
    (define start-r.c (for/first (((k v) (in-hash board#)) #:when (= n v)) k))
    (and start-r.c (inr-solve-pzl board# n start-r.c)))

  (define sltn
    (cond [(solution-starting-at 1) => values]
          ;; next clause starts from 0 for hopido
          [(solution-starting-at 0) => values]))

  (and sltn (hash->puzzle sltn)))
```



```racket
#lang racket
(require "hidato-family-solver.rkt")

(define von-neumann-neighbour-offsets
  '((+1 0) (-1 0) (0 +1) (0 -1)))

(define solve-numbrix (solve-hidato-family von-neumann-neighbour-offsets))

(displayln
 (puzzle->string
  (solve-numbrix
   #(#(0  0  0  0  0  0  0  0  0)
     #(0  0 46 45  0 55 74  0  0)
     #(0 38  0  0 43  0  0 78  0)
     #(0 35  0  0  0  0  0 71  0)
     #(0  0 33  0  0  0 59  0  0)
     #(0 17  0  0  0  0  0 67  0)
     #(0 18  0  0 11  0  0 64  0)
     #(0  0 24 21  0  1  2  0  0)
     #(0  0  0  0  0  0  0  0  0)))))

(newline)

(displayln
 (puzzle->string
  (solve-numbrix
   #(#(0  0  0  0  0  0  0  0  0)
     #(0 11 12 15 18 21 62 61  0)
     #(0  6  0  0  0  0  0 60  0)
     #(0 33  0  0  0  0  0 57  0)
     #(0 32  0  0  0  0  0 56  0)
     #(0 37  0  1  0  0  0 73  0)
     #(0 38  0  0  0  0  0 72  0)
     #(0 43 44 47 48 51 76 77  0)
     #(0  0  0  0  0  0  0  0  0)))))
```


```txt
49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79
```



## REXX

This solution is essentially same as the REXX Hidato puzzle solver.

Programming note: the coördinates for the cells used are the same as an X&times;Y grid, that is,
the bottom left-most cell is (1,1) and the tenth cell on row 2 is (2,10).

''Hidato''   and   ''Numbrix''   are registered trademarks.

```rexx
/*REXX program solves a  Numbrix (R) puzzle, it also displays the puzzle and solution.  */
maxR=0;    maxC=0;    maxX=0;     minR=9e9;      minC=9e9;    minX=9e9;    cells=0;    @.=
parse arg xxx;        PZ='Numbrix puzzle'        /*get the cell definitions from the CL.*/
xxx=translate(xxx, , "/\;:_", ',')               /*also allow other characters as comma.*/

               do  while xxx\='';  parse var  xxx    r c   marks  ','  xxx
                   do  while marks\='';          _=@.r.c
                   parse var marks  x  marks
                   if datatype(x,'N')   then x=abs(x)/1               /*normalize  │x│  */
                   minR=min(minR,r);  maxR=max(maxR,r); minC=min(minC,c); maxC=max(maxC,c)
                   if x==1   then do;  !r=r;  !c=c;  end              /*the START cell. */
                   if _\=='' then call err "cell at" r c 'is already occupied with:'  _
                   @.r.c=x;   c=c+1;    cells=cells+1                 /*assign a mark.  */
                   if x==.              then iterate                  /*is a hole?  Skip*/
                   if \datatype(x,'W')  then call err 'illegal marker specified:' x
                   minX=min(minX,x);    maxX=max(maxX,x)              /*min and max  X. */
                   end   /*while marks¬='' */
               end       /*while xxx  ¬='' */
call show                                        /* [↓]  is used for making fast moves. */
Nr = '0  1   0  -1    -1   1   1  -1'            /*possible  row     for the next move. */
Nc = '1  0  -1   0     1  -1   1  -1'            /*   "      column   "   "    "    "   */
pMoves=words(Nr) -4*(left(PZ,1)=='N')            /*is this to be a Numbrix puzzle ?     */
  do i=1  for pMoves;   Nr.i=word(Nr,i);   Nc.i=word(Nc,i);   end     /*for fast moves. */
if \next(2,!r,!c)  then call err 'No solution possible for this' PZ"."
say;  say 'A solution for the'   PZ   "exists.";    say;                call show
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:  say;    say '***error*** (from' PZ"): "    arg(1);        say;          exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
next: procedure expose @. Nr. Nc. cells pMoves;  parse arg #,r,c;   ##=#+1
           do t=1  for pMoves                                   /* [↓]  try some moves. */
           parse value  r+Nr.t c+Nc.t  with nr nc               /*next move coördinates.*/
           if @.nr.nc==.  then do;                @.nr.nc=#     /*let's try this move.  */
                               if #==cells        then return 1 /*is this the last move?*/
                               if next(##,nr,nc)  then return 1
                               @.nr.nc=.                        /*undo the above move.  */
                               iterate                          /*go & try another move.*/
                               end
           if @.nr.nc==#  then do                               /*this a fill-in move ? */
                               if #==cells        then return 1 /*this is the last move.*/
                               if next(##,nr,nc)  then return 1 /*a fill-in move.       */
                               end
           end   /*t*/
      return 0                                                  /*this ain't working.   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: if maxR<1 | maxC<1  then call err  'no legal cell was specified.'
      if minX<1           then call err  'no  1  was specified for the puzzle start'
      w=max(2,length(cells));  do    r=maxR  to minR  by -1; _=
                                  do c=minC  to maxC;        _=_ right(@.r.c,w); end /*c*/
                               say _
                               end   /*r*/
      say;    return
```

'''output'''   when using the input of:

<tt> 1 1 . . . . . . . . ./2 1 . . 24 21 . 1 2 . ./3 1 . 18 . . 11 . . 64 ./4 1 . 17 . . . . . 67 ./5 1 . . 33 . . . 59 . ./6 1 . 35 . . . . . 71 ./7 1 . 38 . . 43 . . 78 ./8 1 . . 46 45 . 55 74 . ./9 1 . . . . . . . . . </tt>

```txt

  .  .  .  .  .  .  .  .  .
  .  . 46 45  . 55 74  .  .
  . 38  .  . 43  .  . 78  .
  . 35  .  .  .  .  . 71  .
  .  . 33  .  .  . 59  .  .
  . 17  .  .  .  .  . 67  .
  . 18  .  . 11  .  . 64  .
  .  . 24 21  .  1  2  .  .
  .  .  .  .  .  .  .  .  .


A solution for the Numbrix puzzle exists.

 49 50 51 52 53 54 75 76 81
 48 47 46 45 44 55 74 77 80
 37 38 39 40 43 56 73 78 79
 36 35 34 41 42 57 72 71 70
 31 32 33 14 13 58 59 68 69
 30 17 16 15 12 61 60 67 66
 29 18 19 20 11 62 63 64 65
 28 25 24 21 10  1  2  3  4
 27 26 23 22  9  8  7  6  5

```

'''output'''   when using the input of:

<tt> 1 1 . . . . . . . . .\2 1 . 43 44 47 48 51 76 77 .\3 1 . 38 . . . . . 72 .\4 1 . 37 . 1 . . . 73 .\5 1 . 32 . . . . . 56 .\6 1 . 33 . . . . . 57 .\7 1 . 6 . . . . . 60 .\8 1 . 11 12 15 18 21 62 61 .\9 1 . . . . . . . . . </tt>

```txt

  .  .  .  .  .  .  .  .  .
  . 11 12 15 18 21 62 61  .
  .  6  .  .  .  .  . 60  .
  . 33  .  .  .  .  . 57  .
  . 32  .  .  .  .  . 56  .
  . 37  .  1  .  .  . 73  .
  . 38  .  .  .  .  . 72  .
  . 43 44 47 48 51 76 77  .
  .  .  .  .  .  .  .  .  .


A solution for the Numbrix puzzle exists.

  9 10 13 14 19 20 63 64 65
  8 11 12 15 18 21 62 61 66
  7  6  5 16 17 22 59 60 67
 34 33  4  3 24 23 58 57 68
 35 32 31  2 25 54 55 56 69
 36 37 30  1 26 53 74 73 70
 39 38 29 28 27 52 75 72 71
 40 43 44 47 48 51 76 77 78

```



## Ruby

This solution uses HLPsolver from [[Solve_a_Hidato_puzzle#With_Warnsdorff | here]]

```ruby
require 'HLPsolver'

ADJACENT = [[-1, 0], [0, -1], [0, 1], [1, 0]]

board1 = <<EOS
 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0
EOS
HLPsolver.new(board1).solve

board2 = <<EOS
 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0
EOS
HLPsolver.new(board2).solve
```

Which produces:

```txt

Problem:
  0  0  0  0  0  0  0  0  0
  0  0 46 45  0 55 74  0  0
  0 38  0  0 43  0  0 78  0
  0 35  0  0  0  0  0 71  0
  0  0 33  0  0  0 59  0  0
  0 17  0  0  0  0  0 67  0
  0 18  0  0 11  0  0 64  0
  0  0 24 21  0  1  2  0  0
  0  0  0  0  0  0  0  0  0

Solution:
 49 50 51 52 53 54 75 76 81
 48 47 46 45 44 55 74 77 80
 37 38 39 40 43 56 73 78 79
 36 35 34 41 42 57 72 71 70
 31 32 33 14 13 58 59 68 69
 30 17 16 15 12 61 60 67 66
 29 18 19 20 11 62 63 64 65
 28 25 24 21 10  1  2  3  4
 27 26 23 22  9  8  7  6  5

Problem:
  0  0  0  0  0  0  0  0  0
  0 11 12 15 18 21 62 61  0
  0  6  0  0  0  0  0 60  0
  0 33  0  0  0  0  0 57  0
  0 32  0  0  0  0  0 56  0
  0 37  0  1  0  0  0 73  0
  0 38  0  0  0  0  0 72  0
  0 43 44 47 48 51 76 77  0
  0  0  0  0  0  0  0  0  0

Solution:
  9 10 13 14 19 20 63 64 65
  8 11 12 15 18 21 62 61 66
  7  6  5 16 17 22 59 60 67
 34 33  4  3 24 23 58 57 68
 35 32 31  2 25 54 55 56 69
 36 37 30  1 26 53 74 73 70
 39 38 29 28 27 52 75 72 71
 40 43 44 47 48 51 76 77 78
 41 42 45 46 49 50 81 80 79

```



## SystemVerilog



```systemverilog


//////////////////////////////////////////////////////////////////////////////
///  NumbrixSolver                                                         ///
///     Solve the puzzle, by using system verilog randomization engine     ///
//////////////////////////////////////////////////////////////////////////////
class NumbrixSolver;
  rand int solvedBoard[][];
  int fixedBoard[][];
  int numCells;
  ////////////////////////////////////////////////////////////////////////////
  /// Dynamically resize the board accordingly to the size of the reference///
  /// board                                                                ///
  ////////////////////////////////////////////////////////////////////////////
  constraint height {
    solvedBoard.size == fixedBoard.size;
  }
  constraint width {
    foreach(solvedBoard[i]) solvedBoard[i].size == fixedBoard[i].size;
  }

  ////////////////////////////////////////////////////////////////////////////
  ///  Fix the positions defined in the input board                        ///
  ////////////////////////////////////////////////////////////////////////////
  constraint fixed {
    foreach(solvedBoard[i]) foreach(solvedBoard[i][j])
      if(fixedBoard[i][j] != 0)solvedBoard[i][j] == fixedBoard[i][j];
  }
  ////////////////////////////////////////////////////////////////////////////
  ///  Ensures that the whole board is filled from the number with numbers ///
  ///   1,2,3,...,numCells                                                 ///
  ////////////////////////////////////////////////////////////////////////////
  constraint range {
    foreach(solvedBoard[i])foreach(solvedBoard[i][j])
      solvedBoard[i][j] inside {[1:numCells]};
  }
  ////////////////////////////////////////////////////////////////////////////
  ///  Ensures that there is no repeated number, consequently every number ///
  ///  is present on the board                                             ///
  ////////////////////////////////////////////////////////////////////////////
  constraint uniqueness {
    foreach(solvedBoard[i1]) foreach(solvedBoard[i1][j1])
    foreach(solvedBoard[i2]) foreach(solvedBoard[i2][j2])
      if((i1 != i2) || (j1 != j2)) solvedBoard[i1][j1] != solvedBoard[i2][j2];
  }

  ////////////////////////////////////////////////////////////////////////////
  /// Ensures that exists one direction connecting the numbers in          ///
  /// increasing order                                                     ///
  ////////////////////////////////////////////////////////////////////////////
  constraint f_seq {
    foreach(solvedBoard[i])foreach(solvedBoard[i][j])
      (solvedBoard[i][j] == (numCells)) ||
      (solvedBoard[(i < solvedBoard.size-1) ? (i+1): i][j]    ==
                                         solvedBoard[i][j]+1) ||
      (solvedBoard[i][(j < solvedBoard[i].size - 1) ? j+1: j] ==
                                         solvedBoard[i][j]+1) ||
      (solvedBoard[(i > 0) ? i-1: i][j]                       ==
                                         solvedBoard[i][j]+1) ||
      (solvedBoard[i][(j > 0)? j-1:j]                         ==
                                         solvedBoard[i][j]+1);
  }


  function void pre_randomize();
    // the multiplication is not supported in the constraints
    numCells = fixedBoard.size * fixedBoard[0].size;
  endfunction
  function void printSolvedBoard();
    foreach(solvedBoard[i]) begin
      foreach(solvedBoard[j]) begin
        $write("%4d", solvedBoard[i][j]);
      end
      $display("");
    end
  endfunction
endclass


//////////////////////////////////////////////////////////////////////////////
/// SolveNumBrix: A program demonstrating how to use NumbrixSolver class   ///
//////////////////////////////////////////////////////////////////////////////

program SolveNumbrix;
  NumbrixSolver board;
  initial begin
    board = new;
    board.fixedBoard = '{
      '{0,  0,  0,  0,  0,  0,  0,  0,  0},
      '{0,  0, 46, 45,  0, 55, 74,  0,  0},
      '{0, 38,  0,  0, 43,  0,  0, 78,  0},
      '{0, 35,  0,  0,  0,  0,  0, 71,  0},
      '{0,  0, 33,  0,  0,  0, 59,  0,  0},
      '{0, 17,  0,  0,  0,  0,  0, 67,  0},
      '{0, 18,  0,  0, 11,  0,  0, 64,  0},
      '{0,  0, 24, 21,  0,  1,  2,  0,  0},
      '{0,  0,  0,  0,  0,  0,  0,  0,  0}};
    if(board.randomize()) begin
      $display("Solution for the Example 1");
      board.printSolvedBoard();
    end
    else begin
      $display("Failed to solve Example 1");
    end

    board.fixedBoard = '{
       {0,  0,  0,  0,  0,  0,  0,  0,  0},
       {0, 11, 12, 15, 18, 21, 62, 61,  0},
       {0,  6,  0,  0,  0,  0,  0, 60,  0},
       {0, 33,  0,  0,  0,  0,  0, 57,  0},
       {0, 32,  0,  0,  0,  0,  0, 56,  0},
       {0, 37,  0,  1,  0,  0,  0, 73,  0},
       {0, 38,  0,  0,  0,  0,  0, 72,  0},
       {0, 43, 44, 47, 48, 51, 76, 77,  0},
      '{0,  0,  0,  0,  0,  0,  0,  0,  0}};

    if(board.randomize()) begin
      $display("Solution for the Example 2");
      board.printSolvedBoard();
    end
    else begin
      $display("Failed to solve Example 2");
    end
    $finish;
  end
endprogram

```


Running the above program in ncverilog

```txt

  > ncverilog +sv numbrix.sv
Solution for the Example 1
  49  50  51  52  53  54  75  76  81
  48  47  46  45  44  55  74  77  80
  37  38  39  40  43  56  73  78  79
  36  35  34  41  42  57  72  71  70
  31  32  33  14  13  58  59  68  69
  30  17  16  15  12  61  60  67  66
  29  18  19  20  11  62  63  64  65
  28  25  24  21  10   1   2   3   4
  27  26  23  22   9   8   7   6   5
Solution for the Example 2
   9  10  13  14  19  20  63  64  65
   8  11  12  15  18  21  62  61  66
   7   6   5  16  17  22  59  60  67
  34  33   4   3  24  23  58  57  68
  35  32  31   2  25  54  55  56  69
  36  37  30   1  26  53  74  73  70
  39  38  29  28  27  52  75  72  71
  40  43  44  47  48  51  76  77  78
  41  42  45  46  49  50  81  80  79

```



## Tcl

Following loosely the structure of [[Solve_a_Hidato_puzzle#Tcl]].


```Tcl
# Loop over adjacent pairs in a list.
# Example:
#  % eachpair {a b} {1 2 3} {puts $a $b}
#  1 2
#  2 3
proc eachpair {varNames ls script} {
    if {[lassign $varNames _i _j] ne ""} {
        return -code error "Must supply exactly two arguments"
    }
    tailcall foreach $_i [lrange $ls 0 end-1] $_j [lrange $ls 1 end] $script
}

namespace eval numbrix {

    namespace path {::tcl::mathop ::tcl::mathfunc}

    proc parse {txt} {
        set map [split [string trim $txt] \n]
    }

    proc print {map} {
        join [lmap row $map {
            join [lmap val $row {
                format %2d $val
            }] " "
        }] \n
    }

    proc mark {map x y i} {
        lset map $x $y $i
    }

    proc moves {x y} {
        foreach {dx dy} {
                0  1
            -1 0      1 0
                0 -1
        } {
            lappend r [+ $dx $x] [+ $dy $y]
        }
        return $r
    }

    proc rmap {map} {   ;# generate a reverse map in a dict {val {x y} ..}
        set rmap {}
        set h [llength $map]
        set w [llength [lindex $map 0]]
        set x $w
        while {[incr x -1]>=0} {
            set y $h
            while {[incr y -1]>=0} {
                set i [lindex $map $x $y]
                if {$i} {
                    dict set rmap [lindex $map $x $y] [list $x $y]
                }
            }
        }
        return $rmap
    }

    proc gaps {rmap} {  ;# list all the gaps to be filled
        set known [lsort -integer [dict keys $rmap]]
        set gaps {}
        eachpair {i j} $known {
            if {$j > $i+1} {
                lappend gaps $i $j
            }
        }
        return $gaps
    }

    proc fixgaps {map rmap gaps} {  ;# add a "tail" gap if needed
        set w [llength $map]
        set h [llength [lindex $map 0]]
        set size [* $h $w]
        set max [max {*}[dict keys $rmap]]
        if {$max ne $size} {
            lappend gaps $max Inf
        }
        return $gaps
    }


    proc paths {map x0 y0 n} {  ;# generate all the maps with a single path filled legally
        if {$n == 0} {return [list $map]}
        set i [lindex $map $x0 $y0]
        set paths {}
        foreach {x y} [moves $x0 $y0] {
            set j [lindex $map $x $y]
            if {$j eq ""} {
                continue
            } elseif {$j == 0 && $n == $n+1} {
                return [list [mark $map $x $y [+ $i 1]]]
            } elseif {$j == $i+1} {
                lappend paths $map
                continue
            } elseif {$j || ($n == 1)} {
                continue
            } else {
                lappend paths {*}[
                    paths [
                        mark $map $x $y [+ $i 1]
                    ] $x $y [- $n 1]
                ]
            }
        }
        return $paths
    }

    proc solve {map} {
        # fixpoint map
        while 1 {   ;# first we iteratively fill in paths with distinct solutions
            set rmap [rmap $map]
            set gaps [gaps $rmap]
            set gaps [fixgaps $map $rmap $gaps]
            if {$gaps eq ""} {
                return $map
            }
            set oldmap $map
            foreach {i j} $gaps {
                lassign [dict get $rmap $i] x0 y0
                set n [- $j $i]
                set paths [paths $map $x0 $y0 $n]
                if {$paths eq ""} {
                    return ""
                } elseif {[llength $paths] == 1} {
                    #puts "solved $i..$j"
                    #puts [print $map]
                    set map [lindex $paths 0]
                }
                ;# we could intersect the paths to maybe get some tiles
            }
            if {$map eq $oldmap} {
                break
            }
        }
        #puts "unique paths exhausted - going DFS"
        try {   ;# for any left over paths, go DFS
            ;# we might want to sort the gaps first
            foreach {i j} $gaps {
                lassign [dict get $rmap $i] x0 y0
                set n [- $j $i]
                set paths [paths $map $x0 $y0 $n]
                foreach path $paths {
                    #puts "recursing on $i..$j"
                    set sol [solve $path]
                    if {$sol ne ""} {
                        return $sol
                    }
                }
            }
        }
    }

    namespace export {[a-z]*}
    namespace ensemble create
}

set puzzles {
    {
        0  0  0  0  0  0  0  0  0
        0  0 46 45  0 55 74  0  0
        0 38  0  0 43  0  0 78  0
        0 35  0  0  0  0  0 71  0
        0  0 33  0  0  0 59  0  0
        0 17  0  0  0  0  0 67  0
        0 18  0  0 11  0  0 64  0
        0  0 24 21  0  1  2  0  0
        0  0  0  0  0  0  0  0  0
    }

    {
        0  0  0  0  0  0  0  0  0
        0 11 12 15 18 21 62 61  0
        0  6  0  0  0  0  0 60  0
        0 33  0  0  0  0  0 57  0
        0 32  0  0  0  0  0 56  0
        0 37  0  1  0  0  0 73  0
        0 38  0  0  0  0  0 72  0
        0 43 44 47 48 51 76 77  0
        0  0  0  0  0  0  0  0  0
    }
}


foreach puzzle $puzzles {
    set map [numbrix parse $puzzle]
    puts "\n== Puzzle [incr i] =="
    puts [numbrix print $map]
    set sol [numbrix solve $map]
    if {$sol ne ""} {
        puts "\n== Solution $i =="
        puts [numbrix print $sol]
    } else {
        puts "\n== No Solution for Puzzle $i =="
    }
}
```


```txt

== Puzzle 1 ==
 0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0

== Solution 1 ==
49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

== Puzzle 2 ==
 0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0

== Solution 2 ==
 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

```



## zkl

This code solves Hidato, Hopido and Numbrix puzzles.

```zkl
     // Solve Hidato/Hopido/Numbrix puzzles
class Puzzle{  // hold info concerning this puzzle
   var board, nrows,ncols, cells,
       start,      // (r,c) where 1 is located, Void if no 1
       terminated, // if board holds highest numbered cell
       given,	   // all the pre-loaded cells
       adj,        // a list of (r,c) that are valid next cells
       ;

   fcn print_board{
      d:=Dictionary(-1,"  ", 0,"__");
      foreach r in (board){
	 r.pump(String,'wrap(c){ "%2s ".fmt(d.find(c,c)) }).println();
      }
   }
   fcn init(s,adjacent){
      adj=adjacent;
      lines:=s.split("\n");
      ncols,nrows=lines[0].split().len(),lines.len();
      board=nrows.pump(List(), ncols.pump(List(),-1).copy);
      given,start=List(),Void;
      cells,terminated=0,True;
      foreach r,row in (lines.enumerate()){
	 foreach c,cell in (row.split().enumerate()){
	    if(cell=="X") continue;   // X == not in play, leave at -1
	    cells+=1;
	    val:=cell.toInt();
	    board[r][c]=val;
	    given.append(val);
	    if(val==1) start=T(r,c);
	 }
      }
      println("Number of cells = ",cells);
      if(not given.holds(cells)){ given.append(cells); terminated=False; }
      given=given.filter().sort();
   }
   fcn solve{   //-->Bool
      if(start) return(_solve(start.xplode()));
      foreach r,c in (nrows,ncols){
	 if(board[r][c]==0 and _solve(r,c)) return(True);
      }
      False
   }
   fcn [private] _solve(r,c,n=1, next=0){
      if(n>given[-1])                       		   return(True);
      if(not ( (0<=r<nrows) and (0<=c<ncols) ))		   return(False);
      if(board[r][c] and board[r][c]!=n)                   return(False);
      if(terminated and board[r][c]==0 and given[next]==n) return(False);

      back:=0;
      if(board[r][c]==n){ next+=1; back=n; }

      board[r][c]=n;
      foreach i,j in (adj){ if(self.fcn(r+i,c+j,n+1, next)) return(True) }
      board[r][c]=back;
      False
   }
} // Puzzle
```


```zkl
hi1:=  // 0==empty cell, X==not a cell
#<<<
"0  0  0  0  0  0  0  0  0
 0  0 46 45  0 55 74  0  0
 0 38  0  0 43  0  0 78  0
 0 35  0  0  0  0  0 71  0
 0  0 33  0  0  0 59  0  0
 0 17  0  0  0  0  0 67  0
 0 18  0  0 11  0  0 64  0
 0  0 24 21  0  1  2  0  0
 0  0  0  0  0  0  0  0  0";
#<<<

hi2:=  // 0==empty cell, X==not a cell
#<<<
"0  0  0  0  0  0  0  0  0
 0 11 12 15 18 21 62 61  0
 0  6  0  0  0  0  0 60  0
 0 33  0  0  0  0  0 57  0
 0 32  0  0  0  0  0 56  0
 0 37  0  1  0  0  0 73  0
 0 38  0  0  0  0  0 72  0
 0 43 44 47 48 51 76 77  0
 0  0  0  0  0  0  0  0  0";
#<<<
adjacent:=T(         T(-1,0),
            T( 0,-1),         T( 0,1),
                     T( 1,0) );

foreach hi in (T(hi1,hi2)){
   puzzle:=Puzzle(hi); puzzle.adjacent=adjacent;
   puzzle.print_board();
   puzzle.solve();
   println();
   puzzle.print_board();
   println();
}
```

```txt

Number of cells = 81
__ __ __ __ __ __ __ __ __
__ __ 46 45 __ 55 74 __ __
__ 38 __ __ 43 __ __ 78 __
__ 35 __ __ __ __ __ 71 __
__ __ 33 __ __ __ 59 __ __
__ 17 __ __ __ __ __ 67 __
__ 18 __ __ 11 __ __ 64 __
__ __ 24 21 __  1  2 __ __
__ __ __ __ __ __ __ __ __

49 50 51 52 53 54 75 76 81
48 47 46 45 44 55 74 77 80
37 38 39 40 43 56 73 78 79
36 35 34 41 42 57 72 71 70
31 32 33 14 13 58 59 68 69
30 17 16 15 12 61 60 67 66
29 18 19 20 11 62 63 64 65
28 25 24 21 10  1  2  3  4
27 26 23 22  9  8  7  6  5

Number of cells = 81
__ __ __ __ __ __ __ __ __
__ 11 12 15 18 21 62 61 __
__  6 __ __ __ __ __ 60 __
__ 33 __ __ __ __ __ 57 __
__ 32 __ __ __ __ __ 56 __
__ 37 __  1 __ __ __ 73 __
__ 38 __ __ __ __ __ 72 __
__ 43 44 47 48 51 76 77 __
__ __ __ __ __ __ __ __ __

 9 10 13 14 19 20 63 64 65
 8 11 12 15 18 21 62 61 66
 7  6  5 16 17 22 59 60 67
34 33  4  3 24 23 58 57 68
35 32 31  2 25 54 55 56 69
36 37 30  1 26 53 74 73 70
39 38 29 28 27 52 75 72 71
40 43 44 47 48 51 76 77 78
41 42 45 46 49 50 81 80 79

```


