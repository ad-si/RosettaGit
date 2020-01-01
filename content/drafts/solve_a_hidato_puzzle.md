+++
title = "Solve a Hidato puzzle"
description = ""
date = 2019-08-14T09:31:51Z
aliases = []
[extra]
id = 11184
[taxonomies]
categories = []
tags = []
+++

{{task}}
The task is to write a program which solves [[wp:Hidato|Hidato (aka Hidoku) puzzles]].

The rules are:
* You are given a grid with some numbers placed in it. The other squares in the grid will be blank.
** The grid is not necessarily rectangular.
** The grid may have holes in it.
** The grid is always connected.
** The number “1” is always present, as is another number that is equal to the number of squares in the grid. Other numbers are present so as to force the solution to be unique.
** It may be assumed that the difference between numbers present on the grid is not greater than lucky 13.
* The aim is to place a natural number in each blank square so that in the sequence of numbered squares from “1” upwards, each square is in the [[wp:Moore neighborhood]] of the squares immediately before and after it in the sequence (except for the first and last squares, of course, which only have one-sided constraints).
** Thus, if the grid was overlaid on a chessboard, a king would be able to make legal moves along the path from first to last square in numerical order.
** A square may only contain one number.
* In a proper Hidato puzzle, the solution is unique.


For example the following problem
[[File:Hidato_Start.png|center|Sample Hidato problem, from Wikipedia]]

has the following solution, with path marked on it:

[[File:HEnd.png|center|Solution to sample Hidato problem]]


;Related tasks:
* [[A* search algorithm]]
* [[N-queens problem]]
* [[Solve a Holy Knight's tour]]
* [[Solve a Knight's tour]]
* [[Solve a Hopido puzzle]]
* [[Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle]];





## AutoHotkey


```AutoHotkey
SolveHidato(Grid, Locked, Max, row, col, num:=1, R:="", C:=""){
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
		if SolveHidato(Grid, Locked, Max, row, col, num)	; solve for current location and value
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

			if SolveHidato(Grid, Locked, Max, row, col, num, R, C)	; solve for current location, neighbor and value
				return map(Grid)			; if solved, return solution
		}
	}
	num--								; step back
	for i, line in Grid
		for j, element in line
			if InStr(element, ">") && (StrReplace(element, ">") >= num)
				Grid[i, j] := "Y"
}
;--------------------------------
;--------------------------------
;--------------------------------
Neighbor(row,col){
	R := row-1
	loop, 9
	{
		DeltaC := Mod(A_Index, 3) ? Mod(A_Index, 3)-2	: 1
		res .= (R=row && !DeltaC) ? "" : R ":" col+DeltaC ","
		R := Mod(A_Index, 3) ? R : R+1
	}
	return Trim(res, ",")
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
Grid := [[ "Y"	, 33	, 35	, "Y"	, "Y"]
	,[ "Y"	, "Y"	, 24	, 22	, "Y"]
	,[ "Y"	, "Y"	, "Y"	, 21	, "Y"	, "Y"]
	,[ "Y"	, 26	, "Y"	, 13	, 40	, 11 ]
	,[ 27	, "Y"	, "Y"	, "Y"	, 9	, "Y"	, 1  ]
	,[ ""	, ""	, "Y"	, "Y"	, 18	, "Y"	, "Y"]
	,[ ""	, ""	, ""	, ""	, "Y"	, 7	, "Y"	, "Y"]
	,[ ""	, ""	, ""	, ""	, ""	, ""	, 5	, "Y"]]
;--------------------------------
; find locked cells, find row and col of first value "1" and max value
Locked := []
for i, line in Grid
	for j, element in line
	{
		if element = 1
			row :=i , col := j
		if element is integer
			Locked[element] := i ":" j "," Neighbor(i, j)	; save locked elements position and neighbors
			, max := element > max ? element : max		; find max value
	}
;--------------------------------
MsgBox, 262144, ,% SolveHidato(Grid, Locked, Max, row, col)
return
```

Outputs:
```txt
32	33	35	36	37
31	34	24	22	38
30	25	23	21	12	39
29	26	20	13	40	11
27	28	14	19	9	10	1
		15	16	18	8	2
				17	7	6	3
						5	4

```



## Bracmat


```bracmat
(
  ( hidato
  =     Line solve lowest Ncells row column rpad
      , Board colWidth maxDigits start curCol curRow
      , range head line cellN solution output tail
    .   out$!arg
      & @(!arg:? ((%@:>" ") ?:?arg))
      & 0:?row:?column
      & :?Board
      & ( Line
        =   token
          .   whl
            ' ( @(!arg:?token [3 ?arg)
              & (   (   @(!token:? "_" ?)
                      & :?token
                    | @(!token:? #?token (|" " ?))
                    )
                  & (!token.!row.!column) !Board:?Board
                |
                )
              & 1+!column:?column
              )
        )
      &   whl
        ' ( @(!arg:?line \n ?arg)
          & Line$!line
          & 1+!row:?row
          & 0:?column
          )
      & Line$!arg
      & ( range
        =   hi lo
          .   (!arg+1:?hi)+-2:?lo
            & '($lo|$arg|$hi)
        )
      & ( solve
        =     ToDo cellN row column head tail remainder
            , candCell Solved rowCand colCand pattern recurse
          .   !arg:(?ToDo.?cellN.?row.?column)
            & range$!row:(=?row)
            & range$!column:(=?column)
            &
                ' (     ?head ($cellN.?rowCand.?colCand) ?tail
                      & (!rowCand.!colCand):($row.$column)
                      & !recurse
                    |   ?head
                        (.($row.$column):(?rowCand.?colCand))
                        (?tail&!recurse)
                  .     ((!rowCand.!colCand).$cellN)
                      : ?candCell
                    &   (   !head !tail:
                          & out$found!
                          & !candCell
                        |       solve
                              $ ( !head !tail
                                . $cellN+1
                                . !rowCand
                                . !colCand
                                )
                            : ?remainder
                          & !candCell+!remainder
                        )
                      : ?Solved
                  )
              : (=?pattern.?recurse)
            & !ToDo:!pattern
            & !Solved
        )
      & infinity:?lowest
      & (   !Board
          : ? (<!lowest:#%?lowest.?start) (?&~)
        | solve$(!Board.!lowest.!start):?solution
        )
      & :?output
      & 0:?curCol
      & !solution:((?curRow.?).?)+?+[?Ncells
      & @(!Ncells:? [?maxDigits)
      & 1+!maxDigits:?colWidth
      & ( rpad
        =   len
          .   !arg:(?arg.?len)
            & @(str$(!arg "    "):?arg [!len ?)
            & !arg
        )
      &   whl
        ' ( !solution:((?row.?column).?cellN)+?solution
          & (   !row:>!curRow:?curRow
              & !output \n:?output
              & 0:?curCol
            |
            )
          &   whl
            ' ( !curCol+1:~>!column:?curCol
              & !output rpad$(.!colWidth):?output
              )
          &   !output rev$(rpad$(rev$(str$(!cellN " ")).!colWidth))
            : ?output
          & !curCol+1:?curCol
          )
      & str$!output
  )
&   "
 __ 33 35 __ __
 __ __ 24 22 __
 __ __ __ 21 __ __
 __ 26 __ 13 40 11
 27 __ __ __  9 __  1
       __ __ 18 __ __
             __  7 __ __
                    5 __"
  : ?board
& out$(hidato$!board)
);
```

Output:

```txt


 __ 33 35 __ __
 __ __ 24 22 __
 __ __ __ 21 __ __
 __ 26 __ 13 40 11
 27 __ __ __  9 __  1
       __ __ 18 __ __
             __  7 __ __
                    5 __
found!
32 33 35 36 37
31 34 24 22 38
30 25 23 21 12 39
29 26 20 13 40 11
27 28 14 19  9 10  1
      15 16 18  8  2
            17  7  6  3
                   5  4
```



## C

Depth-first graph, with simple connectivity check to reject some impossible situations early.  The checks slow down simpler puzzles significantly, but can make some deep recursions backtrack much earilier.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int *board, *flood, *known, top = 0, w, h;

static inline int idx(int y, int x) { return y * w + x; }

int neighbors(int c, int *p)
/*
@c cell
@p list of neighbours
@return amount of neighbours
*/
{
	int i, j, n = 0;
	int y = c / w, x = c % w;

	for (i = y - 1; i <= y + 1; i++) {
		if (i < 0 || i >= h) continue;
		for (j = x - 1; j <= x + 1; j++)
			if (!(j < 0 || j >= w
				|| (j == x && i == y)
				|| board[ p[n] = idx(i,j) ] == -1))
				n++;
	}

	return n;
}

void flood_fill(int c)
/*
fill all free cells around @c with “1” and write output to variable “flood”
@c cell
*/
{
	int i, n[8], nei;

	nei = neighbors(c, n);
	for (i = 0; i < nei; i++) { // for all neighbours
		if (board[n[i]] || flood[n[i]]) continue; // if cell is not free, choose another neighbour

		flood[n[i]] = 1;
		flood_fill(n[i]);
	}
}

/* Check all empty cells are reachable from higher known cells.
   Should really do more checks to make sure cell_x and cell_x+1
   share enough reachable empty cells; I'm lazy. Will implement
   if a good counter example is presented. */
int check_connectity(int lowerbound)
{
	int c;
	memset(flood, 0, sizeof(flood[0]) * w * h);
	for (c = lowerbound + 1; c <= top; c++)
		if (known[c]) flood_fill(known[c]); // mark all free cells around known cells

	for (c = 0; c < w * h; c++)
		if (!board[c] && !flood[c]) // if there are free cells which could not be reached from flood_fill
			return 0;

	return 1;
}

void make_board(int x, int y, const char *s)
{
	int i;

	w = x, h = y;
        top = 0;
	x = w * h;

        known = calloc(x + 1, sizeof(int));
        board = calloc(x,     sizeof(int));
        flood = calloc(x,     sizeof(int));

	while (x--) board[x] = -1;

	for (y = 0; y < h; y++)
	for (x = 0; x < w; x++) {
		i = idx(y, x);

		while (isspace(*s)) s++;

		switch (*s) {
		case '_':	board[i] = 0;
		case '.':	break;
		default:
			known[ board[i] = strtol(s, 0, 10) ] = i;
			if (board[i] > top) top = board[i];
		}

		while (*s && !isspace(*s)) s++;
	}
}

void show_board(const char *s)
{
	int i, j, c;

	printf("\n%s:\n", s);

	for (i = 0; i < h; i++, putchar('\n'))
	for (j = 0; j < w; j++) {
		c = board[ idx(i, j) ];
		printf(!c ? " __" : c == -1 ? "   " : " %2d", c);
	}
}

int fill(int c, int n)
{
	int i, nei, p[8], ko, bo;

	if ((board[c] && board[c] != n) || (known[n] && known[n] != c))
		return 0;

	if (n == top) return 1;

	ko = known[n];
	bo = board[c];
	board[c] = n;

	if (check_connectity(n)) {
		nei = neighbors(c, p);
		for (i = 0; i < nei; i++)
			if (fill(p[i], n + 1))
				return 1;
	}

	board[c] = bo;
	known[n] = ko;
	return 0;
}

int main()
{
	make_board(
#define USE_E 0
#if (USE_E == 0)
		8,8,	" __ 33 35 __ __ .. .. .."
			" __ __ 24 22 __ .. .. .."
			" __ __ __ 21 __ __ .. .."
			" __ 26 __ 13 40 11 .. .."
			" 27 __ __ __  9 __  1 .."
			" .   . __ __ 18 __ __ .."
			" .  ..  .  . __  7 __ __"
			" .  .. .. ..  .  .  5 __"
#elif (USE_E == 1)
	3, 3,	" . 4 ."
		" _ 7 _"
		" 1 _ _"
#else
	50, 3,
	" 1 _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . 74"
	" . . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ ."
	" . . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ ."
#endif
	);

	show_board("Before");
	fill(known[1], 1);
	show_board("After"); /* "40 lbs in two weeks!" */

	return 0;
}
```

{{out}}

```txt
 Before:
 __ 33 35 __ __
 __ __ 24 22 __
 __ __ __ 21 __ __
 __ 26 __ 13 40 11
 27 __ __ __  9 __  1
       __ __ 18 __ __
             __  7 __ __
                    5 __

 After:
 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4
```



## C++


```cpp

#include <iostream>
#include <sstream>
#include <iterator>
#include <vector>

//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
struct node
{
    int val;
    unsigned char neighbors;
};
//------------------------------------------------------------------------------
class hSolver
{
public:
    hSolver()
    {
	dx[0] = -1; dx[1] = 0; dx[2] = 1; dx[3] = -1; dx[4] = 1; dx[5] = -1; dx[6] = 0; dx[7] = 1;
	dy[0] = -1; dy[1] = -1; dy[2] = -1; dy[3] = 0; dy[4] = 0; dy[5] = 1; dy[6] = 1; dy[7] = 1;
    }

    void solve( vector<string>& puzz, int max_wid )
    {
	if( puzz.size() < 1 ) return;
	wid = max_wid; hei = static_cast<int>( puzz.size() ) / wid;
	int len = wid * hei, c = 0; max = 0;
	arr = new node[len]; memset( arr, 0, len * sizeof( node ) );
	weHave = new bool[len + 1]; memset( weHave, 0, len + 1 );

	for( vector<string>::iterator i = puzz.begin(); i != puzz.end(); i++ )
	{
	    if( ( *i ) == "*" ) {    arr[c++].val = -1; continue; }
	    arr[c].val = atoi( ( *i ).c_str() );
	    if( arr[c].val > 0 ) weHave[arr[c].val] = true;
	    if( max < arr[c].val ) max = arr[c].val;
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
	delete [] weHave;
    }

private:
    bool search( int x, int y, int w )
    {
	if( w == max ) return true;

	node* n = &arr[x + y * wid];
	n->neighbors = getNeighbors( x, y );
	if( weHave[w] )
	{
	    for( int d = 0; d < 8; d++ )
	    {
		if( n->neighbors & ( 1 << d ) )
		{
		    int a = x + dx[d], b = y + dy[d];
		    if( arr[a + b * wid].val == w )
		    if( search( a, b, w + 1 ) ) return true;
		}
	    }
	    return false;
	}

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
	unsigned char c = 0; int m = -1, a, b;
	for( int yy = -1; yy < 2; yy++ )
	    for( int xx = -1; xx < 2; xx++ )
	    {
		if( !yy && !xx ) continue;
		m++; a = x + xx, b = y + yy;
		if( a < 0 || b < 0 || a >= wid || b >= hei ) continue;
		if( arr[a + b * wid].val > -1 ) c |= ( 1 << m );
	    }
	return c;
    }

    void solveIt()
    {
	int x, y; findStart( x, y );
	if( x < 0 ) { cout << "\nCan't find start point!\n"; return; }
	search( x, y, 2 );
    }

    void findStart( int& x, int& y )
    {
	for( int b = 0; b < hei; b++ )
	    for( int a = 0; a < wid; a++ )
		if( arr[a + wid * b].val == 1 ) { x = a; y = b; return; }
	x = y = -1;
    }

    int wid, hei, max, dx[8], dy[8];
    node* arr;
    bool* weHave;
};
//------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    int wid;
    string p = ". 33 35 . . * * * . . 24 22 . * * * . . . 21 . . * * . 26 . 13 40 11 * * 27 . . . 9 . 1 * * * . . 18 . . * * * * * . 7 . . * * * * * * 5 ."; wid = 8;
    //string p = "54 . 60 59 . 67 . 69 . . 55 . . 63 65 . 72 71 51 50 56 62 . * * * * . . . 14 * * 17 . * 48 10 11 * 15 . 18 . 22 . 46 . * 3 . 19 23 . . 44 . 5 . 1 33 32 . . 43 7 . 36 . 27 . 31 42 . . 38 . 35 28 . 30"; wid = 9;
    //string p = ". 58 . 60 . . 63 66 . 57 55 59 53 49 . 65 . 68 . 8 . . 50 . 46 45 . 10 6 . * * * . 43 70 . 11 12 * * * 72 71 . . 14 . * * * 30 39 . 15 3 17 . 28 29 . . 40 . . 19 22 . . 37 36 . 1 20 . 24 . 26 . 34 33"; wid = 9;

    istringstream iss( p ); vector<string> puzz;
    copy( istream_iterator<string>( iss ), istream_iterator<string>(), back_inserter<vector<string> >( puzz ) );
    hSolver s; s.solve( puzz, wid );

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
//--------------------------------------------------------------------------------------------------

```

Output:

```txt

32 33 35 36 37
31 34 24 22 38
30 25 23 21 12 39
29 26 20 13 40 11
27 28 14 19 09 10 01
      15 16 18 08 02
            17 07 06 03
                  05 04

56 58 54 60 61 62 63 66 67
57 55 59 53 49 47 65 64 68
09 08 52 51 50 48 46 45 69
10 06 07          44 43 70
05 11 12          72 71 42
04 14 13          30 39 41
15 03 17 18 28 29 38 31 40
02 16 19 22 23 27 37 36 32
01 20 21 24 25 26 35 34 33

```



## C sharp

The same solver can solve Hidato, Holy Knight's Tour, Hopido and Numbrix puzzles.<br/>
The input can be an array of strings if each cell is one character. The length of the first row must be the number of columns in the puzzle.<br/>
Any non-numeric value indicates a no-go.<br/>
If there are cells that require more characters, then a 2-dimensional array of ints must be used. Any number < 0 indicates a no-go.<br/>
The puzzle can be made circular (the end cell must connect to the start cell). In that case, no start cell needs to be given.

```csharp
using System.Collections;
using System.Collections.Generic;
using static System.Console;
using static System.Math;
using static System.Linq.Enumerable;

public class Solver
{
    private static readonly (int dx, int dy)[]
        //other puzzle types elided
        hidatoMoves = {(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)};

    private (int dx, int dy)[] moves;

    public static void Main()
    {
        Print(new Solver(hidatoMoves).Solve(false, new [,] {
            {  0, 33, 35,  0,  0, -1, -1, -1 },
            {  0,  0, 24, 22,  0, -1, -1, -1 },
            {  0,  0,  0, 21,  0,  0, -1, -1 },
            {  0, 26,  0, 13, 40, 11, -1, -1 },
            { 27,  0,  0,  0,  9,  0,  1, -1 },
            { -1, -1,  0,  0, 18,  0,  0, -1 },
            { -1, -1, -1, -1,  0,  7,  0,  0 },
            { -1, -1, -1, -1, -1, -1,  5,  0 }
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

{{out}}

```txt

32 33 35 36 37 -- -- --
31 34 24 22 38 -- -- --
30 25 23 21 12 39 -- --
29 26 20 13 40 11 -- --
27 28 14 19  9 10  1 --
-- -- 15 16 18  8  2 --
-- -- -- -- 17  7  6  3
-- -- -- -- -- --  5  4

```



## Curry

{{Works with|PAKCS}}
Probably not efficient.

```curry
import CLPFD
import Constraint (andC, anyC)
import Findall (unpack)
import Integer (abs)


hidato :: [[Int]] -> Success
hidato path =
    test path inner
  & domain inner 1 40
  & allDifferent inner
  & andFD [x `near` y | x <- cells, y <- cells]
  & labeling [] (concat path)
  where
    andFD = solve . foldr1 (#/\#)
    cells = enumerate path
    inner free

near :: (Int,Int,Int) -> (Int,Int,Int) -> Constraint
(x,rx,cx) `near` (y,ry,cy) =  x #<=# y  #/\#  dist (y -# x)
                        #\/#  x #>#  y  #/\#  dist (x -# y)
                        #\/#  x #=#  0
                        #\/#  y #=#  0
  where
    dist d =  abs (rx - ry) #<=# d
        #/\#  abs (cx - cy) #<=# d

enumerate :: [[Int]] -> [(Int,Int,Int)]
enumerate xss = [(x,row,col) | (xs,row) <- xss `zip` [1..]
                             , (x ,col) <- xs  `zip` [1..]
                ]

test [[ 0,  0,  0,  0,  0,  0,  0, 0, 0, 0]
     ,[ 0,  A, 33, 35,  B,  C,  0, 0, 0, 0]
     ,[ 0,  D,  E, 24, 22,  F,  0, 0, 0, 0]
     ,[ 0,  G,  H,  I, 21,  J,  K, 0, 0, 0]
     ,[ 0,  L, 26,  M, 13, 40, 11, 0, 0, 0]
     ,[ 0, 27,  N,  O,  P,  9,  Q, 1, 0, 0]
     ,[ 0,  0,  0,  R,  S, 18,  T, U, 0, 0]
     ,[ 0,  0,  0,  0,  0,  V,  7, W, X, 0]
     ,[ 0,  0,  0,  0,  0,  0,  0, 5, Y, 0]
     ,[ 0,  0,  0,  0,  0,  0,  0, 0, 0, 0]
     ]
     [ A, 33, 35,  B,  C
     , D,  E, 24, 22,  F
        , G,  H,  I, 21,  J,  K
        , L, 26,  M, 13, 40, 11
           , 27,  N,  O,  P,  9, Q, 1
           ,  R,  S, 18,  T,  U
               ,  V,  7,  W,  X
                       ,  5,  Y
     ] = success

main = unpack hidato
```

{{Output}}

```txt
Execution time: 1440 msec. / elapsed: 2270 msec.
[[0,0,0,0,0,0,0,0,0,0],[0,32,33,35,36,37,0,0,0,0],[0,31,34,24,22,38,0,0,0,0],[0,30,25,23,21,12,39,0,0,0],[0,29,26,20,13,40,11,0,0,0],[0,27,28,14,19,9,10,1,0,0],[0,0,0,15,16,18,8,2,0,0],[0,0,0,0,0,17,7,6,3,0],[0,0,0,0,0,0,0,5,4,0],[0,0,0,0,0,0,0,0,0,0]]
More values? [y(es)/N(o)/a(ll)]
```



## D

===More C-Style Version===
This version retains some of the characteristics of the original C version. It uses global variables, it doesn't enforce immutability and purity. This style is faster to write for prototypes, short programs or less important code, but in larger programs you usually want more strictness to avoid some bugs and increase long-term maintainability.
{{trans|C}}

```d
import std.stdio, std.array, std.conv, std.algorithm, std.string;

int[][] board;
int[] given, start;

void setup(string s) {
    auto lines = s.splitLines;
    auto cols = lines[0].split.length;
    auto rows = lines.length;
    given.length = 0;

    board = new int[][](rows + 2, cols + 2);
    foreach (row; board)
        row[] = -1;

    foreach (r, row; lines) {
        foreach (c, cell; row.split) {
            switch (cell) {
                case "__":
                    board[r + 1][c + 1] = 0;
                    break;
                case ".":
                    break;
                default:
                    int val = cell.to!int;
                    board[r + 1][c + 1] = val;
                    given ~= val;
                    if (val == 1)
                        start = [r + 1, c + 1];
            }
        }
    }
    given.sort();
}

bool solve(int r, int c, int n, int next = 0) {
    if (n > given.back)
        return true;

    if (board[r][c] && board[r][c] != n)
        return false;

    if (board[r][c] == 0 && given[next] == n)
        return false;

    int back = board[r][c];

    board[r][c] = n;
    foreach (i; -1 .. 2)
        foreach (j; -1 .. 2)
            if (solve(r + i, c + j, n + 1, next + (back == n)))
                return true;

    board[r][c] = back;
    return false;
}

void printBoard() {
    foreach (row; board) {
        foreach (c; row)
            writef(c == -1 ? " . " : c ? "%2d " : "__ ", c);
        writeln;
    }
}

void main() {
    auto hi = "__ 33 35 __ __  .  .  .
                __ __ 24 22 __  .  .  .
                __ __ __ 21 __ __  .  .
                __ 26 __ 13 40 11  .  .
                27 __ __ __  9 __  1  .
                 .  . __ __ 18 __ __  .
                 .  .  .  . __  7 __ __
                 .  .  .  .  .  .  5 __";

    hi.setup;
    printBoard;
    "\nFound:".writeln;
    solve(start[0], start[1], 1);
    printBoard;
}
```

{{out}}

```txt
 .  .  .  .  .  .  .  .  .  .
 . __ 33 35 __ __  .  .  .  .
 . __ __ 24 22 __  .  .  .  .
 . __ __ __ 21 __ __  .  .  .
 . __ 26 __ 13 40 11  .  .  .
 . 27 __ __ __  9 __  1  .  .
 .  .  . __ __ 18 __ __  .  .
 .  .  .  .  . __  7 __ __  .
 .  .  .  .  .  .  .  5 __  .
 .  .  .  .  .  .  .  .  .  .

Found:
 .  .  .  .  .  .  .  .  .  .
 . 32 33 35 36 37  .  .  .  .
 . 31 34 24 22 38  .  .  .  .
 . 30 25 23 21 12 39  .  .  .
 . 29 26 20 13 40 11  .  .  .
 . 27 28 14 19  9 10  1  .  .
 .  .  . 15 16 18  8  2  .  .
 .  .  .  .  . 17  7  6  3  .
 .  .  .  .  .  .  .  5  4  .
 .  .  .  .  .  .  .  .  .  .
```



### Stronger Version

{{trans|C}}
This version uses a little stronger typing, performs tests a run-time with contracts, it doesn't use global variables, it enforces immutability and purity where possible, and produces a correct text output for both larger ad small boards. This style is more fit for larger programs, or when you want the code to be less bug-prone or a little more efficient.

With this coding style the changes in the code become less bug-prone, but also more laborious. This version is also faster, its total runtime is about 0.02 seconds or less.

```d
import std.stdio, std.conv, std.ascii, std.array, std.string,
       std.algorithm, std.exception, std.range, std.typetuple;

struct Hidato {
    // alias Cell = RangedValue!(int, -1, int.max);
    alias Cell = int;
    alias Pos = size_t;
    enum : Cell { emptyCell = -1, unknownCell = 0 }

    immutable Cell boardMax;
    immutable size_t nCols, nRows;
    Cell[] board;
    Pos[] known;
    bool[] flood;

    this(in string input) pure @safe
    in {
        assert(!input.strip.empty);
    } out {
        assert(nCols > 0 && nRows > 0);
        immutable size = nCols * nRows;
        assert(board.length == size);
        assert(known.length == size + 1);
        assert(flood.length == size);
        assert(boardMax > 0 && boardMax <= size);
        assert(board.reduce!max == boardMax);
        assert(board.canFind(1) && board.canFind(boardMax));
        assert(flood.all!(f => f == 0));
        assert(known.all!(rc => rc >= 0 && rc < size));

        foreach (immutable i, immutable cell; board) {
            assert(cell == Hidato.emptyCell ||
                   cell == Hidato.unknownCell ||
                   (cell >= 1 && cell <= size));
            if (cell > 0)
                assert(i == known[size_t(cell)]);
        }
    } body {
        bool[Cell] pathSeen; // A set.
        immutable lines = input.splitLines;
        this.nRows = lines.length;
        this.nCols = lines[0].split.length;

        immutable size = nCols * nRows;
        this.board.length = size;
        this.board[] = emptyCell;
        this.known.length = size + 1;
        this.flood.length = size;

        auto boardMaxMutable = Cell.min;
        Pos i = 0;

        foreach (immutable row; lines) {
            assert(row.split.length == nCols,
                   text("Wrong cols n.: ", row.split.length));

            foreach (immutable cell; row.split) {
                switch (cell) {
                    case "_":
                        this.board[i] = Hidato.unknownCell;
                        break;
                    case ".":
                        this.board[i] = Hidato.emptyCell;
                        break;
                    default: // Known.
                        immutable val = cell.to!Cell;
                        enforce(val > 0, "Path numbers must be > 0.");
                        enforce(val !in pathSeen,
                                text("Duplicated path number: ", val));
                        pathSeen[val] = true;
                        this.board[i] = val;
                        this.known[val] = i;
                        boardMaxMutable = max(boardMaxMutable, val);
                }
                i++;
            }
        }

        this.boardMax = boardMaxMutable;
    }


    private Pos idx(in size_t r, in size_t c) const pure nothrow @safe @nogc {
        return r * nCols + c;
    }

    private uint nNeighbors(in Pos pos, ref Pos[8] neighbours)
    const pure nothrow @safe @nogc {
        immutable r = pos / nCols;
        immutable c = pos % nCols;
        typeof(return) n = 0;

        foreach (immutable sr; TypeTuple!(-1, 0, 1)) {
            immutable size_t i = r + sr; // Can wrap-around.
            if (i >= nRows)
                continue;
            foreach (immutable sc; TypeTuple!(-1, 0, 1)) {
                immutable size_t j = c + sc; // Can wrap-around.
                if ((sc != 0 || sr != 0) && j < nCols) {
                    immutable pos2 = idx(i, j);
                    neighbours[n] = pos2;
                    if (board[pos2] != Hidato.emptyCell)
                        n++;
                }
            }
        }

        return n;
    }

    /// Fill all free cells around 'cell' with true and write
    /// output to variable "flood".
    private void floodFill(in Pos pos) pure nothrow @safe @nogc {
        Pos[8] n = void;

        // For all neighbours.
        foreach (immutable i; 0 .. nNeighbors(pos, n)) {
            // If pos is not free, choose another neighbour.
            if (board[n[i]] || flood[n[i]])
                continue;
            flood[n[i]] = true;
            floodFill(n[i]);
        }
    }

    /// Check all empty cells are reachable from higher known cells.
    private bool checkConnectity(in uint lowerBound) pure nothrow @safe @nogc {
        flood[] = false;

        foreach (immutable i; lowerBound + 1 .. boardMax + 1)
            if (known[i])
                floodFill(known[i]);

        foreach (immutable i; 0 .. nCols * nRows)
            // If there are free cells which could not be
            // reached from floodFill.
            if (!board[i] && !flood[i])
                return false;
        return true;
    }

    private bool fill(in Pos pos, in uint n) pure nothrow @safe @nogc {
        if ((board[pos] && board[pos] != n) ||
            (known[n] && known[n] != pos))
            return false;

        if (n == boardMax)
            return true;

        immutable ko = known[n];
        immutable bo = board[pos];
        board[pos] = n;

        Pos[8] p = void;
        if (checkConnectity(n))
            foreach (immutable i; 0 .. nNeighbors(pos, p))
                if (fill(p[i], n + 1))
                    return true;

        board[pos] = bo;
        known[n] = ko;
        return false;
    }

    void solve() pure nothrow @safe @nogc
    in {
        assert(!known.empty);
    } body {
        fill(known[1], 1);
    }

    string toString() const pure {
        immutable d = [Hidato.emptyCell: ".",
                       Hidato.unknownCell: "_"];
        immutable form = "%" ~ text(boardMax.text.length + 1) ~ "s";

        string result;
        foreach (immutable r; 0 .. nRows) {
            foreach (immutable c; 0 .. nCols) {
                immutable cell = board[idx(r, c)];
                result ~= format(form, d.get(cell, cell.text));
            }
            result ~= "\n";
        }
        return result;
    }
}

void solveHidato(in string problem) {
    auto game = problem.Hidato;
    writeln("Problem:\n", game);
    game.solve;
    writeln("Solution:\n", game);
}

void main() {
    solveHidato(" _ 33 35  _  _  .  .  .
                  _  _ 24 22  _  .  .  .
                  _  _  _ 21  _  _  .  .
                  _ 26  _ 13 40 11  .  .
                 27  _  _  _  9  _  1  .
                  .  .  _  _ 18  _  _  .
                  .  .  .  .  _  7  _  _
                  .  .  .  .  .  .  5  _");

    solveHidato(". 4 .
                 _ 7 _
                 1 _ _");

    solveHidato(
"1 _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . 74
 . . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ .
 . . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ ."
    );
}
```

{{out}}

```txt
Problem:
  _ 33 35  _  _  .  .  .
  _  _ 24 22  _  .  .  .
  _  _  _ 21  _  _  .  .
  _ 26  _ 13 40 11  .  .
 27  _  _  _  9  _  1  .
  .  .  _  _ 18  _  _  .
  .  .  .  .  _  7  _  _
  .  .  .  .  .  .  5  _

Solution:
 32 33 35 36 37  .  .  .
 31 34 24 22 38  .  .  .
 30 25 23 21 12 39  .  .
 29 26 20 13 40 11  .  .
 27 28 14 19  9 10  1  .
  .  . 15 16 18  8  2  .
  .  .  .  . 17  7  6  3
  .  .  .  .  .  .  5  4

Problem:
 . 4 .
 _ 7 _
 1 _ _

Solution:
 . 4 .
 3 7 5
 1 2 6

Problem:
  1  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  . 74
  .  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .
  .  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .

Solution:
  1  2  3  .  .  8  9  .  . 14 15  .  . 20 21  .  . 26 27  .  . 32 33  .  . 38 39  .  . 44 45  .  . 50 51  .  . 56 57  .  . 62 63  .  . 68 69  .  . 74
  .  .  4  .  7  . 10  . 13  . 16  . 19  . 22  . 25  . 28  . 31  . 34  . 37  . 40  . 43  . 46  . 49  . 52  . 55  . 58  . 61  . 64  . 67  . 70  . 73  .
  .  .  .  5  6  .  . 11 12  .  . 17 18  .  . 23 24  .  . 29 30  .  . 35 36  .  . 41 42  .  . 47 48  .  . 53 54  .  . 59 60  .  . 65 66  .  . 71 72  .
```



## Elixir

{{trans|Ruby}}

```elixir
# Solve a Hidato Like Puzzle with Warnsdorff like logic applied
#
defmodule HLPsolver do
  defmodule Cell do
    defstruct value: -1, used: false, adj: []
  end

  def solve(str, adjacent, print_out\\true) do
    board = setup(str)
    if print_out, do: print(board, "Problem:")
    {start, _} = Enum.find(board, fn {_,cell} -> cell.value==1 end)
    board = set_adj(board, adjacent)
    zbl = for %Cell{value: n} <- Map.values(board), into: %{}, do: {n, true}
    try do
      solve(board, start, 1, zbl, map_size(board))
      IO.puts "No solution"
    catch
      {:ok, result} -> if print_out, do: print(result, "Solution:"),
                                   else: result
    end
  end

  defp solve(board, position, seq_num, zbl, goal) do
    value = board[position].value
    cond do
      value > 0 and value != seq_num -> nil
      value == 0 and zbl[seq_num] -> nil
      true ->
        cell = %Cell{board[position] | value: seq_num, used: true}
        board = %{board | position => cell}
        if seq_num == goal, do: throw({:ok, board})
        Enum.each(wdof(board, cell.adj), fn pos ->
          solve(board, pos, seq_num+1, zbl, goal)
        end)
    end
  end

  defp setup(str) do
    lines = String.strip(str) |> String.split(~r/(\n|\r\n|\r)/) |> Enum.with_index
    for {line,i} <- lines, {char,j} <- Enum.with_index(String.split(line)),
        :error != Integer.parse(char), into: %{} do
          {n,_} = Integer.parse(char)
          {{i,j}, %Cell{value: n}}
        end
  end

  defp set_adj(board, adjacent) do
    Enum.reduce(Map.keys(board), board, fn {x,y},map ->
      adj = Enum.map(adjacent, fn {i,j} -> {x+i, y+j} end)
            |> Enum.reduce([], fn pos,acc -> if board[pos], do: [pos | acc], else: acc end)
      Map.update!(map, {x,y}, fn cell -> %Cell{cell | adj: adj} end)
    end)
  end

  defp wdof(board, adj) do              # Warnsdorf's rule
    Enum.reject(adj, fn pos -> board[pos].used end)
    |> Enum.sort_by(fn pos ->
         Enum.count(board[pos].adj, fn p -> not board[p].used end)
       end)
  end

  def print(board, title) do
    IO.puts "\n#{title}"
    {xmin, xmax} = Map.keys(board) |> Enum.map(fn {x,_} -> x end) |> Enum.min_max
    {ymin, ymax} = Map.keys(board) |> Enum.map(fn {_,y} -> y end) |> Enum.min_max
    len = map_size(board) |> to_char_list |> length
    space = String.duplicate(" ", len)
    Enum.each(xmin..xmax, fn x ->
      Enum.map_join(ymin..ymax, " ", fn y ->
        case Map.get(board, {x,y}) do
          nil  -> space
          cell -> to_string(cell.value) |> String.rjust(len)
        end
      end)
      |> IO.puts
    end)
  end
end
```


'''Test:'''

```elixir
adjacent = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}]

"""
  .  4
  0  7  0
  1  0  0
"""
|> HLPsolver.solve(adjacent)

"""
  0 33 35  0  0
  0  0 24 22  0
  0  0  0 21  0  0
  0 26  0 13 40 11
 27  0  0  0  9  0  1
  .  .  0  0 18  0  0
  .  .  .  .  0  7  0  0
  .  .  .  .  .  .  5  0
"""
|> HLPsolver.solve(adjacent)

"""
  1  0  0  .  0  0  0  .  0  0  0  .  0  0  0  .  0  0  0  .  0  0  0  .  0  0  0
  .  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0
  .  .  0  0  0  .  0  0  0  .  0  0  0  .  0  0  0  .  0  0  0  .  0  0  0  .  0
"""
|> HLPsolver.solve(adjacent)
```


{{out}}

```txt

Problem:
  4
0 7 0
1 0 0

Solution:
  4
3 7 5
1 2 6

Problem:
 0 33 35  0  0
 0  0 24 22  0
 0  0  0 21  0  0
 0 26  0 13 40 11
27  0  0  0  9  0  1
       0  0 18  0  0
             0  7  0  0
                   5  0

Solution:
32 33 35 36 37
31 34 24 22 38
30 25 23 21 12 39
29 26 20 13 40 11
27 28 14 19  9 10  1
      15 16 18  8  2
            17  7  6  3
                   5  4

Problem:
 1  0  0     0  0  0     0  0  0     0  0  0     0  0  0     0  0  0     0  0  0
       0     0     0     0     0     0     0     0     0     0     0     0     0
       0  0  0     0  0  0     0  0  0     0  0  0     0  0  0     0  0  0     0

Solution:
 1  2  3     9 10 11    17 18 19    25 26 27    33 34 35    41 42 43    49 50 51
       4     8    12    16    20    24    28    32    36    40    44    48    52
       5  6  7    13 14 15    21 22 23    29 30 31    37 38 39    45 46 47    53

```



## Erlang

To simplify the code I start a new process for searching each potential path through the grid. This means that the default maximum number of processes had to be raised ("erl +P 50000" works for me). The task takes about 1-2 seconds on a low level Mac mini. If faster times are needed, or even less performing hardware is used, some optimisation should be done.

```Erlang

-module( solve_hidato_puzzle ).

-export( [create/2, solve/1, task/0] ).

-compile({no_auto_import,[max/2]}).

create( Grid_list, Number_list ) ->
        Squares = lists:flatten( [create_column(X, Y) || {X, Y} <- Grid_list] ),
	lists:foldl( fun store/2, dict:from_list(Squares), Number_list ).

print( Grid_list ) when is_list(Grid_list) -> print( create(Grid_list, []) );
print( Grid_dict ) ->
    Max_x = max_x( Grid_dict ),
    Max_y = max_y( Grid_dict ),
    Print_row = fun (Y) -> [print(X, Y, Grid_dict) || X <- lists:seq(1, Max_x)], io:nl() end,
    [Print_row(Y) || Y <- lists:seq(1, Max_y)].

solve( Dict ) ->
    {find_start, [Start]} = {find_start, dict:fold( fun start/3, [], Dict )},
    Max = dict:size( Dict ),
    {stop_ok, {Max, Max, [Stop]}} = {stop_ok, dict:fold( fun stop/3, {Max, 0, []}, Dict )},
    My_pid = erlang:self(),
    erlang:spawn( fun() -> path(Start, Stop, Dict, My_pid, []) end ),
    receive
    {grid, Grid, path, Path} -> {Grid, Path}
    end.

task() ->
    %% Square is {X, Y}, N}. N = 0 for empty square. These are created if not present.
    %% Leftmost column is X=1. Top row is Y=1.
    %% Optimised for the example, grid is a list of {X, {Y_min, Y_max}}.
    %% When there are holes, X is repeated as many times as needed with two new Y values each time.
    Start = {{7,5}, 1},
    Stop = {{5,4}, 40},
    Grid_list = [{1, {1,5}}, {2, {1,5}}, {3, {1,6}}, {4, {1,6}}, {5, {1,7}}, {6, {3,7}}, {7, {5,8}}, {8, {7,8}}],
    Number_list = [Start, Stop, {{1,5}, 27}, {{2,1}, 33}, {{2,4}, 26}, {{3,1}, 35}, {{3,2}, 24},
                {{4,2}, 22}, {{4,3}, 21}, {{4,4}, 13}, {{5,5}, 9}, {{5,6}, 18}, {{6,4}, 11}, {{6,7}, 7}, {{7,8}, 5}],
    Grid = create( Grid_list, Number_list ),
    io:fwrite( "Start grid~n" ),
    print( Grid ),
    {New_grid, Path} = solve( create(Grid_list, Number_list) ),
    io:fwrite( "Start square ~p, Stop square ~p.~nPath ~p~n", [Start, Stop, Path] ),
    print( New_grid ).


create_column( X, {Y_min, Y_max} ) -> [{{X, Y}, 0} || Y <- lists:seq(Y_min, Y_max)].

is_filled( Dict ) -> [] =:= dict:fold( fun keep_0_square/3, [], Dict ).

keep_0_square( Key, 0, Acc ) -> [Key | Acc];
keep_0_square(  _Key, _Value, Acc ) -> Acc.

max( Position, Keys ) ->
    [Square | _T] = lists:reverse( lists:keysort(Position, Keys) ),
    Square.

max_x( Dict ) ->
    {X, _Y} = max( 1, dict:fetch_keys(Dict) ),
    X.

max_y( Dict ) ->
    {_X, Y} = max( 2, dict:fetch_keys(Dict) ),
    Y.


neighbourhood( Square, Dict ) ->
        Potentials = neighbourhood_potential_squares( Square ),
	neighbourhood_squares( dict:find(Square, Dict), Potentials, Dict ).

neighbourhood_potential_squares( {X, Y} ) -> [{Sx, Sy} || Sx <- [X-1, X, X+1], Sy <- [Y-1, Y, Y+1], {X, Y} =/= {Sx, Sy}].

neighbourhood_squares( {ok, Value}, Potentials, Dict ) ->
        Square_values = lists:flatten( [neighbourhood_square_value(X, dict:find(X, Dict)) || X <- Potentials] ),
        Next_value = Value + 1,
        neighbourhood_squares_next_value( lists:keyfind(Next_value, 2, Square_values), Square_values, Next_value ).

neighbourhood_squares_next_value( {Square, Value}, _Square_values, Value ) -> [{Square, Value}];
neighbourhood_squares_next_value( false, Square_values, Value ) -> [{Square, Value} || {Square, Y} <- Square_values, Y =:= 0].

neighbourhood_square_value( Square, {ok, Value} ) -> [{Square, Value}];
neighbourhood_square_value( _Square, error ) -> [].

path( Square, Square, Dict, Pid, Path ) -> path_correct( is_filled(Dict), Pid, [Square | Path], Dict );
path( Square, Stop, Dict, Pid, Path ) ->
    Reversed_path = [Square | Path],
    Neighbours = neighbourhood( Square, Dict ),
    [erlang:spawn( fun() -> path(Next_square, Stop, dict:store(Next_square, Value, Dict), Pid, Reversed_path) end ) || {Next_square, Value} <- Neighbours].

path_correct( true, Pid, Path, Dict ) -> Pid ! {grid, Dict, path, lists:reverse( Path )};
path_correct( false, _Pid, _Path, _Dict ) -> dead_end.

print( X, Y, Dict ) -> print_number( dict:find({X, Y}, Dict) ).

print_number( {ok, 0} ) -> io:fwrite( "~3s", ["."] ); % . is less distracting than 0
print_number( {ok, Value} ) -> io:fwrite( "~3b", [Value] );
print_number( error ) -> io:fwrite( "~3s", [" "] ).

start( Key, 1, Acc ) -> [Key | Acc]; % Allow check that we only have one key with value 1.
start( _Key, _Value, Acc ) -> Acc.

stop( Key, Max, {Max, Max_found, Stops} ) -> {Max, erlang:max(Max, Max_found), [Key | Stops]}; % Allow check that we only have one key with value Max.
stop( _Key, Value, {Max, Max_found, Stops} ) -> {Max, erlang:max(Value, Max_found), Stops}. % Allow check that Max is Max.

store( {Key, Value}, Dict ) -> dict:store( Key, Value, Dict ).

```

{{out}}

```txt

2> solve_hidato_puzzle:task().
Start grid
  . 33 35  .  .
  .  . 24 22  .
  .  .  . 21  .  .
  . 26  . 13 40 11
 27  .  .  .  9  .  1
        .  . 18  .  .
              .  7  .  .
                    5  .
Start square {{7,5},1}, Stop square {{5,4},40}.
Path [{7,5}, {7,6}, {8,7}, {8,8}, {7,8}, {7,7}, {6,7}, {6,6}, {5,5}, {6,5}, {6,4}, {5,3}, {4,4}, {3,5}, {3,6}, {4,6}, {5,7}, {5,6}, {4,5}, {3,4},
      {4,3}, {4,2}, {3,3}, {3,2}, {2,3}, {2,4}, {1,5},{2,5}, {1,4}, {1,3}, {1,2}, {1,1}, {2,1}, {2,2}, {3,1}, {4,1}, {5,1}, {5,2}, {6,3}, {5,4}]
 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4

```



## Go

{{trans|Java}}

```go
package main

import (
    "fmt"
    "sort"
    "strconv"
    "strings"
)

var board [][]int
var start, given []int

func setup(input []string) {
    /* This task is not about input validation, so
       we're going to trust the input to be valid */
    puzzle := make([][]string, len(input))
    for i := 0; i < len(input); i++ {
        puzzle[i] = strings.Fields(input[i])
    }
    nCols := len(puzzle[0])
    nRows := len(puzzle)
    list := make([]int, nRows*nCols)
    board = make([][]int, nRows+2)
    for i := 0; i < nRows+2; i++ {
        board[i] = make([]int, nCols+2)
        for j := 0; j < nCols+2; j++ {
            board[i][j] = -1
        }
    }
    for r := 0; r < nRows; r++ {
        row := puzzle[r]
        for c := 0; c < nCols; c++ {
            switch cell := row[c]; cell {
            case "_":
                board[r+1][c+1] = 0
            case ".":
                break
            default:
                val, _ := strconv.Atoi(cell)
                board[r+1][c+1] = val
                list = append(list, val)
                if val == 1 {
                    start = append(start, r+1, c+1)
                }
            }
        }
    }
    sort.Ints(list)
    given = make([]int, len(list))
    for i := 0; i < len(given); i++ {
        given[i] = list[i]
    }
}

func solve(r, c, n, next int) bool {
    if n > given[len(given)-1] {
        return true
    }

    back := board[r][c]
    if back != 0 && back != n {
        return false
    }

    if back == 0 && given[next] == n {
        return false
    }

    if back == n {
        next++
    }

    board[r][c] = n
    for i := -1; i < 2; i++ {
        for j := -1; j < 2; j++ {
            if solve(r+i, c+j, n+1, next) {
                return true
            }
        }
    }

    board[r][c] = back
    return false
}

func printBoard() {
    for _, row := range board {
        for _, c := range row {
            switch {
            case c == -1:
                fmt.Print(" . ")
            case c > 0:
                fmt.Printf("%2d ", c)
            default:
                fmt.Print("__ ")
            }
        }
        fmt.Println()
    }
}

func main() {
    input := []string{
        "_ 33 35 _ _ . . .",
        "_ _ 24 22 _ . . .",
        "_ _ _ 21 _ _ . .",
        "_ 26 _ 13 40 11 . .",
        "27 _ _ _ 9 _ 1 .",
        ". . _ _ 18 _ _ .",
        ". . . . _ 7 _ _",
        ". . . . . . 5 _",
    }
    setup(input)
    printBoard()
    fmt.Println("\nFound:")
    solve(start[0], start[1], 1, 0)
    printBoard()
}
```


{{out}}

```txt

 .  .  .  .  .  .  .  .  .  .
 . __ 33 35 __ __  .  .  .  .
 . __ __ 24 22 __  .  .  .  .
 . __ __ __ 21 __ __  .  .  .
 . __ 26 __ 13 40 11  .  .  .
 . 27 __ __ __  9 __  1  .  .
 .  .  . __ __ 18 __ __  .  .
 .  .  .  .  . __  7 __ __  .
 .  .  .  .  .  .  .  5 __  .
 .  .  .  .  .  .  .  .  .  .

Found:
 .  .  .  .  .  .  .  .  .  .
 . 32 33 35 36 37  .  .  .  .
 . 31 34 24 22 38  .  .  .  .
 . 30 25 23 21 12 39  .  .  .
 . 29 26 20 13 40 11  .  .  .
 . 27 28 14 19  9 10  1  .  .
 .  .  . 15 16 18  8  2  .  .
 .  .  .  .  . 17  7  6  3  .
 .  .  .  .  .  .  .  5  4  .
 .  .  .  .  .  .  .  .  .  .

```



## Haskell


```haskell
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import Data.List
import Data.Maybe
import Data.Time.Clock

data BoardProblem = Board
  { cells :: IntMap (IntMap Int)
  , endVal :: Int
  , onePos :: (Int, Int)
  , givens :: [Int]
  } deriving (Show, Eq)

tupIns x y v m = I.insert x (I.insert y v (I.findWithDefault I.empty x m)) m

tupLookup x y m = I.lookup x m >>= I.lookup y

makeBoard =
  (\x ->
      x
      { givens = dropWhile (<= 1) $ sort $ givens x
      }) .
  foldl' --'
    f
    (Board I.empty 0 (0, 0) []) .
  concatMap (zip [0 ..]) . zipWith (\y w -> map (y, ) $ words w) [0 ..]
  where
    f bd (x, (y, v)) =
      if v == "."
        then bd
        else Board
               (tupIns x y (read v) (cells bd))
               (if read v > endVal bd
                  then read v
                  else endVal bd)
               (if v == "1"
                  then (x, y)
                  else onePos bd)
               (read v : givens bd)

hidato brd = listToMaybe $ h 2 (cells brd) (onePos brd) (givens brd)
  where
    h nval pmap (x, y) gs
      | nval == endVal brd = [pmap]
      | nval == head gs =
        if null nvalAdj
          then []
          else h (nval + 1) pmap (fst $ head nvalAdj) (tail gs)
      | not $ null nvalAdj = h (nval + 1) pmap (fst $ head nvalAdj) gs
      | otherwise = hEmptyAdj
      where
        around =
          [ (x - 1, y - 1)
          , (x, y - 1)
          , (x + 1, y - 1)
          , (x - 1, y)
          , (x + 1, y)
          , (x - 1, y + 1)
          , (x, y + 1)
          , (x + 1, y + 1)
          ]
        lkdUp = map (\(x, y) -> ((x, y), tupLookup x y pmap)) around
        nvalAdj = filter ((== Just nval) . snd) lkdUp
        hEmptyAdj =
          concatMap
            (\((nx, ny), _) -> h (nval + 1) (tupIns nx ny nval pmap) (nx, ny) gs) $
          filter ((== Just 0) . snd) lkdUp

printCellMap cellmap = putStrLn $ concat strings
  where
    maxPos = xyBy I.findMax maximum
    minPos = xyBy I.findMin minimum
    xyBy :: (forall a. IntMap a -> (Int, a)) -> ([Int] -> Int) -> (Int, Int)
    xyBy a b = (fst (a cellmap), b $ map (fst . a . snd) $ I.toList cellmap)
    strings =
      map
        f
        [ (x, y)
        | y <- [snd minPos .. snd maxPos]
        , x <- [fst minPos .. fst maxPos] ]
    f (x, y) =
      let z =
            if x == fst maxPos
              then "\n"
              else " "
      in case tupLookup x y cellmap of
           Nothing -> "  " ++ z
           Just n ->
             (if n < 10
                then ' ' : show n
                else show n) ++
             z

main = do
  let sampleBoard = makeBoard sample
  printCellMap $ cells sampleBoard
  printCellMap $ fromJust $ hidato sampleBoard

sample =
  [ " 0 33 35  0  0"
  , " 0  0 24 22  0"
  , " 0  0  0 21  0  0"
  , " 0 26  0 13 40 11"
  , "27  0  0  0  9  0  1"
  , ".  .   0  0 18  0  0"
  , ".  .  .  .   0  7  0  0"
  , ".  .  .  .  .  .   5  0"
  ]
```

{{Out}}

```txt
 0 33 35  0  0
 0  0 24 22  0
 0  0  0 21  0  0
 0 26  0 13 40 11
27  0  0  0  9  0  1
       0  0 18  0  0
             0  7  0  0
                   5  0

32 33 35 36 37
31 34 24 22 38
30 25 23 21 12 39
29 26 20 13 40 11
27 28 14 19  9 10  1
      15 16 18  8  2
            17  7  6  3
                   5  4

```


==Icon and {{header|Unicon}}==

This is an Unicon-specific solution but could easily be adjusted to work in Icon.

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
    p := [[-1]]
    nCells := maxCols := 0
    every line := !&input do {
        put(p,[: -1 | gencells(line) | -1 :])
        maxCols <:= *p[-1]
        }
    put(p, [-1])
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
    method atEnd(); return (nCells = val) = puzzle[loc.r][loc.c]; end
    method goNorth(); return visit(loc.r-1,loc.c);   end
    method goNE();    return visit(loc.r-1,loc.c+1); end
    method goEast();  return visit(loc.r,  loc.c+1); end
    method goSE();    return visit(loc.r+1,loc.c+1); end
    method goSouth(); return visit(loc.r+1,loc.c);   end
    method goSW();    return visit(loc.r+1,loc.c-1); end
    method goWest();  return visit(loc.r,  loc.c-1); end
    method goNW();    return visit(loc.r-1,loc.c-1); end

    method visit(r,c)
        if /best & validPos(r,c) then return Pos(r,c)
    end

    method validPos(r,c)
        xv := puzzle[r][c]
        if xv = (val+1) then return
        if xv = 0 then {  # make sure this path hasn't already gone there
            ancestor := self
            while xl := (ancestor := \ancestor.getParent()).getLoc() do
                if (xl.r = r) & (xl.c = c) then fail
            return
            }
    end

initially
    val +:= 1
    if atEnd() then return best := self
    QMouse(puzzle, goNorth(), self, val)
    QMouse(puzzle, goNE(),    self, val)
    QMouse(puzzle, goEast(),  self, val)
    QMouse(puzzle, goSE(),    self, val)
    QMouse(puzzle, goSouth(), self, val)
    QMouse(puzzle, goSW(),    self, val)
    QMouse(puzzle, goWest(),  self, val)
    QMouse(puzzle, goNW(),    self, val)
end
```


Sample run:


```txt

->hd <hd.in
Input with 40 cells:

     _ 33 35  _  _
     _  _ 24 22  _
     _  _  _ 21  _  _
     _ 26  _ 13 40 11
    27  _  _  _  9  _  1
           _  _ 18  _  _
                 _  7  _  _
                       5  _

Output with 40 cells:

    32 33 35 36 37
    31 34 24 22 38
    30 25 23 21 12 39
    29 26 20 13 40 11
    27 28 14 19  9 10  1
          15 16 18  8  2
                17  7  6  3
                       5  4

->

```



## Java

{{trans|D}}
{{works with|Java|7}}

```java
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Hidato {

    private static int[][] board;
    private static int[] given, start;

    public static void main(String[] args) {
        String[] input = {"_ 33 35 _ _ . . .",
            "_ _ 24 22 _ . . .",
            "_ _ _ 21 _ _ . .",
            "_ 26 _ 13 40 11 . .",
            "27 _ _ _ 9 _ 1 .",
            ". . _ _ 18 _ _ .",
            ". . . . _ 7 _ _",
            ". . . . . . 5 _"};

        setup(input);
        printBoard();
        System.out.println("\nFound:");
        solve(start[0], start[1], 1, 0);
        printBoard();
    }

    private static void setup(String[] input) {
        /* This task is not about input validation, so
           we're going to trust the input to be valid */

        String[][] puzzle = new String[input.length][];
        for (int i = 0; i < input.length; i++)
            puzzle[i] = input[i].split(" ");

        int nCols = puzzle[0].length;
        int nRows = puzzle.length;

        List<Integer> list = new ArrayList<>(nRows * nCols);

        board = new int[nRows + 2][nCols + 2];
        for (int[] row : board)
            for (int c = 0; c < nCols + 2; c++)
                row[c] = -1;

        for (int r = 0; r < nRows; r++) {
            String[] row = puzzle[r];
            for (int c = 0; c < nCols; c++) {
                String cell = row[c];
                switch (cell) {
                    case "_":
                        board[r + 1][c + 1] = 0;
                        break;
                    case ".":
                        break;
                    default:
                        int val = Integer.parseInt(cell);
                        board[r + 1][c + 1] = val;
                        list.add(val);
                        if (val == 1)
                            start = new int[]{r + 1, c + 1};
                }
            }
        }
        Collections.sort(list);
        given = new int[list.size()];
        for (int i = 0; i < given.length; i++)
            given[i] = list.get(i);
    }

    private static boolean solve(int r, int c, int n, int next) {
        if (n > given[given.length - 1])
            return true;

        if (board[r][c] != 0 && board[r][c] != n)
            return false;

        if (board[r][c] == 0 && given[next] == n)
            return false;

        int back = board[r][c];
        if (back == n)
            next++;

        board[r][c] = n;
        for (int i = -1; i < 2; i++)
            for (int j = -1; j < 2; j++)
                if (solve(r + i, c + j, n + 1, next))
                    return true;

        board[r][c] = back;
        return false;
    }

    private static void printBoard() {
        for (int[] row : board) {
            for (int c : row) {
                if (c == -1)
                    System.out.print(" . ");
                else
                    System.out.printf(c > 0 ? "%2d " : "__ ", c);
            }
            System.out.println();
        }
    }
}
```


Output:


```txt
 .  .  .  .  .  .  .  .  .  .
 . __ 33 35 __ __  .  .  .  .
 . __ __ 24 22 __  .  .  .  .
 . __ __ __ 21 __ __  .  .  .
 . __ 26 __ 13 40 11  .  .  .
 . 27 __ __ __  9 __  1  .  .
 .  .  . __ __ 18 __ __  .  .
 .  .  .  .  . __  7 __ __  .
 .  .  .  .  .  .  .  5 __  .
 .  .  .  .  .  .  .  .  .  .

Found:
 .  .  .  .  .  .  .  .  .  .
 . 32 33 35 36 37  .  .  .  .
 . 31 34 24 22 38  .  .  .  .
 . 30 25 23 21 12 39  .  .  .
 . 29 26 20 13 40 11  .  .  .
 . 27 28 14 19  9 10  1  .  .
 .  .  . 15 16 18  8  2  .  .
 .  .  .  .  . 17  7  6  3  .
 .  .  .  .  .  .  .  5  4  .
 .  .  .  .  .  .  .  .  .  .
```



## Julia

This solution utilizes a Hidato puzzle solver module which is also used for the Hopido and knight move tasks.

```julia
module Hidato

export hidatosolve, printboard, hidatoconfigure

function hidatoconfigure(str)
    lines = split(str, "\n")
    nrows, ncols = length(lines), length(split(lines[1], r"\s+"))
    board = fill(-1, (nrows, ncols))
    presets = Vector{Int}()
    starts = Vector{CartesianIndex{2}}()
    maxmoves = 0
    for (i, line) in enumerate(lines), (j, s) in enumerate(split(strip(line), r"\s+"))
        c = s[1]
        if c == '_' || (c == '0' && length(s) == 1)
            board[i, j] = 0
            maxmoves += 1
        elseif c == '.'
            continue
        else # numeral, get 2 digits
            board[i, j] = parse(Int, s)
            push!(presets, board[i, j])
            if board[i, j] == 1
                push!(starts, CartesianIndex(i, j))
            end
            maxmoves += 1
        end
    end
    board, maxmoves, sort!(presets), length(starts) == 1 ? starts : findall(x -> x == 0, board)
end

function hidatosolve(board, maxmoves, movematrix, fixed, row, col, sought)
    if sought > maxmoves
        return true
    elseif (0 != board[row, col] != sought) || (board[row, col] == 0 && sought in fixed)
        return false
    end
    backnum = board[row, col] == sought ? sought : 0
    board[row, col] = sought # try board with this cell set to next number
    for move in movematrix
        i, j = row + move[1], col + move[2]
        if (0 < i <= size(board)[1]) && (0 < j <= size(board)[2]) &&
            hidatosolve(board, maxmoves, movematrix, fixed, i, j, sought + 1)
            return true
        end
    end
    board[row, col] = backnum # return board to original state
    false
end

function printboard(board, emptysquare= "__ ", blocked = "   ")
    d = Dict(-1 => blocked, 0 => emptysquare, -2 => "\n")
    map(x -> d[x] = rpad(lpad(string(x), 2), 3), 1:maximum(board))
    println(join([d[i] for i in hcat(board, fill(-2, size(board)[1]))'], ""))
end

end  # module

```

```julia
using .Hidato

hidat = """
__ 33 35 __ __  .  .  .
__ __ 24 22 __  .  .  .
__ __ __ 21 __ __  .  .
__ 26 __ 13 40 11  .  .
27 __ __ __  9 __  1  .
 .  . __ __ 18 __ __  .
 .  .  .  . __  7 __ __
 .  .  .  .  .  .  5 __"""

const kingmoves = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

board, maxmoves, fixed, starts = hidatoconfigure(hidat)
printboard(board)
hidatosolve(board, maxmoves, kingmoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

```
{{output}}
```txt

 __ 33 35 __ __
 __ __ 24 22 __
 __ __ __ 21 __ __
 __ 26 __ 13 40 11
 27 __ __ __  9 __  1
       __ __ 18 __ __
             __  7 __ __
                    5 __

 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4

```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

lateinit var board: List<IntArray>
lateinit var given: IntArray
lateinit var start: IntArray

fun setUp(input: List<String>) {
    val nRows = input.size
    val puzzle = List(nRows) { input[it].split(" ") }
    val nCols = puzzle[0].size
    val list = mutableListOf<Int>()
    board = List(nRows + 2) { IntArray(nCols + 2) { -1 } }
    for (r in 0 until nRows) {
        val row = puzzle[r]
        for (c in 0 until nCols) {
            val cell = row[c]
            if (cell == "_") {
                board[r + 1][c + 1] = 0
            }
            else if (cell != ".") {
                val value = cell.toInt()
                board[r + 1][c + 1] = value
                list.add(value)
                if (value == 1) start = intArrayOf(r + 1, c + 1)
            }
        }
    }
    list.sort()
    given = list.toIntArray()
}

fun solve(r: Int, c: Int, n: Int, next: Int): Boolean {
    if (n > given[given.lastIndex]) return true
    val back = board[r][c]
    if (back != 0 && back != n) return false
    if (back == 0 && given[next] == n) return false
    var next2 = next
    if (back == n) next2++
    board[r][c] = n
    for (i in -1..1)
        for (j in -1..1)
            if (solve(r + i, c + j, n + 1, next2)) return true
    board[r][c] = back
    return false
}

fun printBoard() {
    for (row in board) {
        for (c in row) {
            if (c == -1)
                print(" . ")
            else
                print(if (c > 0) "%2d ".format(c) else "__ ")
        }
        println()
    }
}

fun main(args: Array<String>) {
    var input = listOf(
        "_ 33 35 _ _ . . .",
        "_ _ 24 22 _ . . .",
        "_ _ _ 21 _ _ . .",
        "_ 26 _ 13 40 11 . .",
        "27 _ _ _ 9 _ 1 .",
        ". . _ _ 18 _ _ .",
        ". . . . _ 7 _ _",
        ". . . . . . 5 _"
    )
    setUp(input)
    printBoard()
    println("\nFound:")
    solve(start[0], start[1], 1, 0)
    printBoard()
}
```


{{out}}

```txt

 .  .  .  .  .  .  .  .  .  .
 . __ 33 35 __ __  .  .  .  .
 . __ __ 24 22 __  .  .  .  .
 . __ __ __ 21 __ __  .  .  .
 . __ 26 __ 13 40 11  .  .  .
 . 27 __ __ __  9 __  1  .  .
 .  .  . __ __ 18 __ __  .  .
 .  .  .  .  . __  7 __ __  .
 .  .  .  .  .  .  .  5 __  .
 .  .  .  .  .  .  .  .  .  .

Found:
 .  .  .  .  .  .  .  .  .  .
 . 32 33 35 36 37  .  .  .  .
 . 31 34 24 22 38  .  .  .  .
 . 30 25 23 21 12 39  .  .  .
 . 29 26 20 13 40 11  .  .  .
 . 27 28 14 19  9 10  1  .  .
 .  .  . 15 16 18  8  2  .  .
 .  .  .  .  . 17  7  6  3  .
 .  .  .  .  .  .  .  5  4  .
 .  .  .  .  .  .  .  .  .  .

```



## Mathprog


```mathprog
/*Hidato.mathprog, part of KuKu by Nigel Galloway

  Find a solution to a Hidato problem

  Nigel_Galloway@operamail.com
  April 1st., 2011
*/

param ZBLS;
param ROWS;
param COLS;
param D := 1;
set ROWSR := 1..ROWS;
set COLSR := 1..COLS;
set ROWSV := (1-D)..(ROWS+D);
set COLSV := (1-D)..(COLS+D);
param Iz{ROWSR,COLSR}, integer, default 0;
set ZBLSV := 1..(ZBLS+1);
set ZBLSR := 1..ZBLS;

var BR{ROWSV,COLSV,ZBLSV}, binary;

void0{r in ROWSV, z in ZBLSR,c in (1-D)..0}: BR[r,c,z] = 0;
void1{r in ROWSV, z in ZBLSR,c in (COLS+1)..(COLS+D)}: BR[r,c,z] = 0;
void2{c in COLSV, z in ZBLSR,r in (1-D)..0}: BR[r,c,z] = 0;
void3{c in COLSV, z in ZBLSR,r in (ROWS+1)..(ROWS+D)}: BR[r,c,z] = 0;
void4{r in ROWSV,c in (1-D)..0}: BR[r,c,ZBLS+1] = 1;
void5{r in ROWSV,c in (COLS+1)..(COLS+D)}: BR[r,c,ZBLS+1] = 1;
void6{c in COLSV,r in (1-D)..0}: BR[r,c,ZBLS+1] = 1;
void7{c in COLSV,r in (ROWS+1)..(ROWS+D)}: BR[r,c,ZBLS+1] = 1;

Izfree{r in ROWSR, c in COLSR, z in ZBLSR : Iz[r,c] = -1}: BR[r,c,z] = 0;
Iz1{Izr in ROWSR, Izc in COLSR, r in ROWSR, c in COLSR, z in ZBLSR : Izr=r and Izc=c and Iz[Izr,Izc]=z}: BR[r,c,z] = 1;

rule1{z in ZBLSR}: sum{r in ROWSR, c in COLSR} BR[r,c,z] = 1;
rule2{r in ROWSR, c in COLSR}: sum{z in ZBLSV} BR[r,c,z] = 1;
rule3{r in ROWSR, c in COLSR, z in ZBLSR}: BR[0,0,z+1] + BR[r-1,c-1,z+1] + BR[r-1,c,z+1] + BR[r-1,c+1,z+1] + BR[r,c-1,z+1] + BR[r,c+1,z+1] + BR[r+1,c-1,z+1] + BR[r+1,c,z+1] + BR[r+1,c+1,z+1] - BR[r,c,z] >= 0;

solve;

for {r in ROWSR} {
    for {c in COLSR} {
        printf " %2d", sum{z in ZBLSR} BR[r,c,z]*z;
    }
    printf "\n";
}
data;

param ROWS := 8;
param COLS := 8;
param ZBLS := 40;
param
Iz: 1   2   3   4   5   6   7   8 :=
 1  .  33  35   .   .  -1  -1  -1
 2  .   .  24  22   .  -1  -1  -1
 3  .   .   .  21   .   .  -1  -1
 4  .  26   .  13  40  11  -1  -1
 5 27   .   .   .   9   .   1  -1
 6 -1  -1   .   .  18   .   .  -1
 7 -1  -1  -1  -1   .   7   .   .
 8 -1  -1  -1  -1  -1  -1   5   .
 ;

 end;
```

Using the data in the model produces the following:
{{out}}

```txt

>glpsol --minisat --math Hidato.mathprog
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --minisat --math Hidato.mathprog
Reading model section from Hidato.mathprog...
Reading data section from Hidato.mathprog...
64 lines were read
Generating void0...
Generating void1...
Generating void2...
Generating void3...
Generating void4...
Generating void5...
Generating void6...
Generating void7...
Generating Izfree...
Generating Iz1...
Generating rule1...
Generating rule2...
Generating rule3...
Model has been successfully generated
Will search for ANY feasible solution
Translating to CNF-SAT...
Original problem has 5279 rows, 4100 columns, and 33359 non-zeros
2520 covering inequalities
2719 partitioning equalities
Solving CNF-SAT problem...
Instance has 7076 variables, 24047 clauses, and 77735 literals

### ============================
[MINISAT]
### =============================

| Conflicts |     ORIGINAL     |              LEARNT              | Progress |
|           | Clauses Literals |   Limit Clauses Literals  Lit/Cl |          |

### ========================================================================

|         0 |   21432    75120 |    7144       0        0     0.0 |  0.000 % |

### ========================================================================

SATISFIABLE
Objective value =  0.000000000e+000
Time used:   0.0 secs
Memory used: 14.5 Mb (15192264 bytes)
 32 33 35 36 37  0  0  0
 31 34 24 22 38  0  0  0
 30 25 23 21 12 39  0  0
 29 26 20 13 40 11  0  0
 27 28 14 19  9 10  1  0
  0  0 15 16 18  8  2  0
  0  0  0  0 17  7  6  3
  0  0  0  0  0  0  5  4
Model has been successfully processed

```

Modelling Evil Case 1:

```txt

data;
param ROWS := 3;
param COLS := 3;
param ZBLS := 7;
param
Iz: 1   2   3 :=
 1 -1   4  -1
 2  .   7   .
 3  1   .   .
 ;
end;

```

Produces:

```txt

>glpsol --minisat --math Hidato.mathprog --data Evil1.data
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --minisat --math Hidato.mathprog --data Evil1.data
Reading model section from Hidato.mathprog...
Hidato.mathprog:47: warning: data section ignored
47 lines were read
Reading data section from Evil1.data...
11 lines were read
Generating void0...
Generating void1...
Generating void2...
Generating void3...
Generating void4...
Generating void5...
Generating void6...
Generating void7...
Generating Izfree...
Generating Iz1...
Generating rule1...
Generating rule2...
Generating rule3...
Model has been successfully generated
Will search for ANY feasible solution
Translating to CNF-SAT...
Original problem has 256 rows, 200 columns, and 935 non-zeros
56 covering inequalities
193 partitioning equalities
Solving CNF-SAT problem...
Instance has 337 variables, 1237 clauses, and 4094 literals

### ============================
[MINISAT]
### =============================

| Conflicts |     ORIGINAL     |              LEARNT              | Progress |
|           | Clauses Literals |   Limit Clauses Literals  Lit/Cl |          |

### ========================================================================

|         0 |    1060     3917 |     353       0        0     0.0 |  0.000 % |

### ========================================================================

SATISFIABLE
Objective value =  0.000000000e+000
Time used:   0.0 secs
Memory used: 0.8 Mb (861188 bytes)
  0  4  0
  3  7  5
  1  2  6
Model has been successfully processed

```

Modelling Evil Case 2 - The Snake in the Grass:

```txt

data;
param ROWS := 3;
param COLS := 50;
param ZBLS := 74;
param
Iz:  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 :=
 1   1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1 74
 2  -1 -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1  . -1
 3  -1 -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1 -1  .  . -1
;
end;

```

Produces:

```txt

G:\IAJAAR4.47>glpsol --minisat --math Hidato.mathprog --data Evil2.data
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --minisat --math Hidato.mathprog --data Evil2.data
Reading model section from Hidato.mathprog...
Hidato.mathprog:47: warning: data section ignored
47 lines were read
Reading data section from Evil2.data...
Evil2.data:11: warning: final NL missing before end of file
11 lines were read
Generating void0...
Generating void1...
Generating void2...
Generating void3...
Generating void4...
Generating void5...
Generating void6...
Generating void7...
Generating Izfree...
Generating Iz1...
Generating rule1...
Generating rule2...
Generating rule3...
Model has been successfully generated
Will search for ANY feasible solution
Translating to CNF-SAT...
Original problem has 25500 rows, 19500 columns, and 147452 non-zeros
11026 covering inequalities
14400 partitioning equalities
Solving CNF-SAT problem...
Instance has 31338 variables, 98310 clauses, and 305726 literals

### ============================
[MINISAT]
### =============================

| Conflicts |     ORIGINAL     |              LEARNT              | Progress |
|           | Clauses Literals |   Limit Clauses Literals  Lit/Cl |          |

### ========================================================================

|         0 |   84134   291550 |   28044       0        0     0.0 |  0.000 % |
|       101 |   31135   126809 |   30848      98     5496    56.1 | 65.521 % |
|       251 |   31135   126809 |   33933     244    12470    51.1 | 66.552 % |
|       476 |   27353   115512 |   37327     446    23819    53.4 | 68.160 % |
|       814 |   26574   113330 |   41059     770    42161    54.8 | 69.586 % |
|      1321 |   25432   110534 |   45165    1262    83658    66.3 | 70.056 % |

### ========================================================================

SATISFIABLE
Objective value =  0.000000000e+000
Time used:   1.0 secs
Memory used: 60.9 Mb (63862624 bytes)
  1  2  3  0  0  8  9  0  0 14 15  0  0 20 21  0  0 26 27  0  0 32 33  0  0 38 39  0  0 44 45  0  0 50 51  0  0 56 57  0  0 62 63  0  0 68 69  0  0 74
  0  0  4  0  7  0 10  0 13  0 16  0 19  0 22  0 25  0 28  0 31  0 34  0 37  0 40  0 43  0 46  0 49  0 52  0 55  0 58  0 61  0 64  0 67  0 70  0 73  0
  0  0  0  5  6  0  0 11 12  0  0 17 18  0  0 23 24  0  0 29 30  0  0 35 36  0  0 41 42  0  0 47 48  0  0 53 54  0  0 59 60  0  0 65 66  0  0 71 72  0
Model has been successfully processed

```

Modelling Evil Case 3 - A fatter snake in the Grass:

```txt

data;
param ROWS := 4;
param COLS := 46;
param ZBLS := 82;
param
Iz:  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 :=
 1   1  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1  0  0 -1 -1 -1 82
 2  -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1
 3  -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1  0 -1  0 -1 -1
 4   0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1  0  0  0 -1 -1 -1
 ;
end;

```

Produces:

```txt

>glpsol --minisat --math Hidato.mathprog --data Evil3.data
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --minisat --math Hidato.mathprog --data Evil3.data
Reading model section from Hidato.mathprog...
Hidato.mathprog:47: warning: data section ignored
47 lines were read
Reading data section from Evil3.data...
12 lines were read
Generating void0...
Generating void1...
Generating void2...
Generating void3...
Generating void4...
Generating void5...
Generating void6...
Generating void7...
Generating Izfree...
Generating Iz1...
Generating rule1...
Generating rule2...
Generating rule3...
Model has been successfully generated
Will search for ANY feasible solution
Translating to CNF-SAT...
Original problem has 32684 rows, 23904 columns, and 198488 non-zeros
15006 covering inequalities
17596 partitioning equalities
Solving CNF-SAT problem...
Instance has 39792 variables, 130040 clauses, and 407222 literals

### ============================
[MINISAT]
### =============================

| Conflicts |     ORIGINAL     |              LEARNT              | Progress |
|           | Clauses Literals |   Limit Clauses Literals  Lit/Cl |          |

### ========================================================================

|         0 |  112710   389892 |   37570       0        0     0.0 |  0.000 % |

### ========================================================================

SATISFIABLE
Objective value =  0.000000000e+000
Time used:   0.0 secs
Memory used: 80.2 Mb (84067912 bytes)
  1  2  0  0  0 10 11  0  0  0 19 20  0  0  0 28 29  0  0  0 37 38  0  0  0 46 47  0  0  0 55 56  0  0  0 64 65  0  0  0 73 74  0  0  0 82
  0  0  3  0  9  0  0 12  0 18  0  0 21  0 27  0  0 30  0 36  0  0 39  0 45  0  0 48  0 54  0  0 57  0 63  0  0 66  0 72  0  0 75  0 81  0
  0  4  0  8  0  0 13  0 17  0  0 22  0 26  0  0 31  0 35  0  0 40  0 44  0  0 49  0 53  0  0 58  0 62  0  0 67  0 71  0  0 76  0 80  0  0
  5  6  7  0  0 14 15 16  0  0 23 24 25  0  0 32 33 34  0  0 41 42 43  0  0 50 51 52  0  0 59 60 61  0  0 68 69 70  0  0 77 78 79  0  0  0
Model has been successfully processed

```



## Nim


```nim
import strutils, algorithm

var board: array[0..19, array[0..19, int]]
var given, start: seq[int] = @[]
var rows, cols: int = 0

proc setup(s: string) =
    var lines = s.splitLines()
    cols = lines[0].split().len()
    rows = lines.len()

    for i in 0 .. rows + 1:
        for j in 0 .. cols + 1:
           board[i][j] = -1

    for r, row in pairs(lines):
        for c, cell in pairs(row.split()):
            case cell
            of "__" :
                board[r + 1][c + 1] = 0
                continue
            of "." : continue
            else :
               var val = parseInt(cell)
               board[r + 1][c + 1] = val
               given.add(val)
               if (val == 1):
                   start.add(r + 1)
                   start.add(c + 1)
    given.sort(cmp[int], Ascending)

proc solve(r, c, n: int, next: int = 0): bool =
    if n > given[high(given)]:
       return true
    if board[r][c] < 0:
        return false
    if (board[r][c] > 0 and board[r][c] != n):
        return false
    if (board[r][c] == 0 and given[next] == n):
        return false

    var back = board[r][c]
    board[r][c] = n
    for i in -1 .. 1:
        for j in -1 .. 1:
            if back == n:
                if (solve(r + i, c + j, n + 1, next + 1)):  return true
            else:
                if (solve(r + i, c + j, n + 1, next)): return true
    board[r][c] = back
    result = false


proc printBoard() =
    for r in  0 .. rows + 1:
        for cellid,c in pairs(board[r]):
            if cellid > cols + 1: break
            if c == -1:
                write(stdout, " . ")
            elif c == 0:
                write(stdout, "__ ")
            else:
                write(stdout, "$# " % align($c,2))
        writeLine(stdout, "")

var hi: string = """__ 33 35 __ __  .  .  .
__ __ 24 22 __  .  .  .
__ __ __ 21 __ __  .  .
__ 26 __ 13 40 11  .  .
27 __ __ __  9 __  1  .
.  . __ __ 18 __ __  .
.  .  .  . __  7 __ __
.  .  .  .  .  .  5 __"""

setup(hi)
printBoard()
echo("")
echo("Found:")
discard solve(start[0], start[1], 1)
printBoard()
```

{{out}}

```txt
 .  .  .  .  .  .  .  .  .  .
 . __ 33 35 __ __  .  .  .  .
 . __ __ 24 22 __  .  .  .  .
 . __ __ __ 21 __ __  .  .  .
 . __ 26 __ 13 40 11  .  .  .
 . 27 __ __ __  9 __  1  .  .
 .  .  . __ __ 18 __ __  .  .
 .  .  .  .  . __  7 __ __  .
 .  .  .  .  .  .  .  5 __  .
 .  .  .  .  .  .  .  .  .  .

Found:
 .  .  .  .  .  .  .  .  .  .
 . 32 33 35 36 37  .  .  .  .
 . 31 34 24 22 38  .  .  .  .
 . 30 25 23 21 12 39  .  .  .
 . 29 26 20 13 40 11  .  .  .
 . 27 28 14 19  9 10  1  .  .
 .  .  . 15 16 18  8  2  .  .
 .  .  .  .  . 17  7  6  3  .
 .  .  .  .  .  .  .  5  4  .
 .  .  .  .  .  .  .  .  .  .
```



## Perl


```perl
use strict;
use List::Util 'max';

our (@grid, @known, $n);

sub show_board {
	for my $r (@grid) {
		print map(!defined($_)	? '   ' : $_
					? sprintf("%3d", $_)
					: ' __'
			, @$r), "\n"
	}
}

sub parse_board {
	@grid = map{[map(/^_/ ? 0 : /^\./ ? undef: $_, split ' ')]}
			split "\n", shift();
	for my $y (0 .. $#grid) {
		for my $x (0 .. $#{$grid[$y]}) {
			$grid[$y][$x] > 0
				and $known[$grid[$y][$x]] = "$y,$x";
		}
	}
	$n = max(map { max @$_ } @grid);
}

sub neighbors {
	my ($y, $x) = @_;
	my @out;
	for (	[-1, -1], [-1, 0], [-1, 1],
		[ 0, -1],	   [ 0, 1],
		[ 1, -1], [ 1, 0], [ 1, 1])
	{
		my $y1 = $y + $_->[0];
		my $x1 = $x + $_->[1];
		next if $x1 < 0 || $y1 < 0;
		next unless defined $grid[$y1][$x1];
		push @out, "$y1,$x1";
	}
	@out
}

sub try_fill {
	my ($v, $coord) = @_;
	return 1	if $v > $n;

	my ($y, $x) = split ',', $coord;
	my $old = $grid[$y][$x];

	return	if $old && $old != $v;
	return	if exists $known[$v] and $known[$v] ne $coord;

	$grid[$y][$x] = $v;
	print "\033[0H";
	show_board();

	try_fill($v + 1, $_) && return 1
			for neighbors($y, $x);

	$grid[$y][$x] = $old;
	return
}

parse_board
#	". 4 .
#	 _ 7 _
#	 1 _ _";

#	" 1 _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . 74
#	  . . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _
#	  . . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _
#	";

	"__ 33 35 __ __ .. .. .. .
	 __ __ 24 22 __ .. .. .. .
	 __ __ __ 21 __ __ .. .. .
	 __ 26 __ 13 40 11 .. .. .
	 27 __ __ __  9 __  1 .. .
	 .   . __ __ 18 __ __ .. .
	 .  ..  .  . __  7 __ __ .
	 .  .. .. ..  .  .  5 __ .";

print "\033[2J";
try_fill(1, $known[1]);
```
{{out}}
 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4


## Perl 6

This uses a Warnsdorff solver, which cuts down the number of tries by more than a factor of six over the brute force approach. This same solver is used in:

* [[Solve a Hidato puzzle#Perl_6|Solve a Hidato puzzle]]
* [[Solve a Hopido puzzle#Perl_6|Solve a Hopido puzzle]]
* [[Solve a Holy Knight's tour#Perl_6|Solve a Holy Knight's tour]]
* [[Solve a Numbrix puzzle#Perl_6|Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle#Perl_6|Solve the no connection puzzle]]


```perl6
my @adjacent = [-1, -1], [-1, 0], [-1, 1],
               [ 0, -1],          [ 0, 1],
               [ 1, -1], [ 1, 0], [ 1, 1];

solveboard q:to/END/;
        __ 33 35 __ __ .. .. ..
        __ __ 24 22 __ .. .. ..
        __ __ __ 21 __ __ .. ..
        __ 26 __ 13 40 11 .. ..
        27 __ __ __  9 __  1 ..
        .. .. __ __ 18 __ __ ..
        .. .. .. .. __  7 __ __
        .. .. .. .. .. ..  5 __
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



## Phix


```Phix
sequence board, warnsdorffs, knownx, knowny

integer width, height, limit, nchars, tries
string fmt, blank

constant ROW = 1, COL = 2
constant moves = {{-1,-1},{-1,0},{-1,1},{0,-1},{0,1},{1,-1},{1,0},{1,1}}

function onboard(integer row, integer col)
    return row>=1 and row<=height and col>=nchars and col<=nchars*width
end function

procedure init_warnsdorffs()
integer nrow,ncol
    for row=1 to height do
        for col=nchars to nchars*width by nchars do
            for move=1 to length(moves) do
                nrow = row+moves[move][ROW]
                ncol = col+moves[move][COL]*nchars
                if onboard(nrow,ncol)
                and board[nrow][ncol]='_' then
                    warnsdorffs[nrow][ncol] += 1
                end if
            end for
        end for
    end for
end procedure

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
        and board[nrow][ncol]='_' then
            wmoves = append(wmoves,{warnsdorffs[nrow][ncol],nrow,ncol})
        end if
    end for
    wmoves = sort(wmoves)
    -- avoid creating orphans
    if length(wmoves)<2 or wmoves[2][1]>1 then
        for m=1 to length(wmoves) do
            {?,nrow,ncol} = wmoves[m]
            warnsdorffs[nrow][ncol] -= 1
        end for
        for m=1 to length(wmoves) do
            {?,nrow,ncol} = wmoves[m]
            board[nrow][ncol-nchars+1..ncol] = sprintf(fmt,n)
            if solve(nrow,ncol,n+1) then return 1 end if
            board[nrow][ncol-nchars+1..ncol] = blank
        end for
        for m=1 to length(wmoves) do
            {?,nrow,ncol} = wmoves[m]
            warnsdorffs[nrow][ncol] += 1
        end for
    end if
    return 0
end function

procedure Hidato(sequence s, integer w, integer h, integer lim)
integer y, ch, ch2, k
atom t0 = time()
    s = split(s,'\n')
    width = w
    height = h
    nchars = length(sprintf(" %d",lim))
    fmt = sprintf(" %%%dd",nchars-1)
    blank = repeat('_',nchars)
    board = repeat(repeat(' ',width*nchars),height)
    knownx = repeat(0,lim)
    knowny = repeat(0,lim)
    limit = 0
    for x=1 to height do
        for y=nchars to width*nchars by nchars do
            if y>length(s[x]) then
                ch = '.'
            else
                ch = s[x][y]
            end if
            if ch='_' then
                limit += 1
            elsif ch!='.' then
                k = ch-'0'
                ch2 = s[x][y-1]
                if ch2!=' ' then
                    k += (ch2-'0')*10
                    board[x][y-1] = ch2
                end if
                knownx[k] = x
                knowny[k] = y
                limit += 1
            end if
            board[x][y] = ch
        end for
    end for
    warnsdorffs = repeat(repeat(0,width*nchars),height)
    init_warnsdorffs()
    tries = 0
    if solve(knownx[1],knowny[1],2) then
        puts(1,join(board,"\n"))
        printf(1,"\nsolution found in %d tries (%3.2fs)\n",{tries,time()-t0})
    else
        puts(1,"no solutions found\n")
    end if
end procedure

constant board1 = """
 __ 33 35 __ __ .. .. ..
 __ __ 24 22 __ .. .. ..
 __ __ __ 21 __ __ .. ..
 __ 26 __ 13 40 11 .. ..
 27 __ __ __  9 __  1 ..
 .. .. __ __ 18 __ __ ..
 .. .. .. .. __  7 __ __
 .. .. .. .. .. ..  5 __"""
Hidato(board1,8,8,40)

constant board2 = """
 . 4 .
 _ 7 _
 1 _ _"""
Hidato(board2,3,3,7)

constant board3 = """
  1  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  . 74
  .  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .  _  .
  .  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  .  .  _  _  ."""
Hidato(board3,50,3,74)

constant board4 = """
 54 __ 60 59 __ 67 __ 69 __
 __ 55 __ __ 63 65 __ 72 71
 51 50 56 62 __ .. .. .. ..
 __ __ __ 14 .. .. 17 __ ..
 48 10 11 .. 15 __ 18 __ 22
 __ 46 __ ..  3 __ 19 23 __
 __ 44 __  5 __  1 33 32 __
 __ 43  7 __ 36 __ 27 __ 31
 42 __ __ 38 __ 35 28 __ 30"""
Hidato(board4,9,9,72)

constant board5 = """
 __ 58 __ 60 __ __ 63 66 __
 57 55 59 53 49 __ 65 __ 68
 __  8 __ __ 50 __ 46 45 __
 10  6 __ .. .. .. __ 43 70
 __ 11 12 .. .. .. 72 71 __
 __ 14 __ .. .. .. 30 39 __
 15  3 17 __ 28 29 __ __ 40
 __ __ 19 22 __ __ 37 36 __
  1 20 __ 24 __ 26 __ 34 33"""
Hidato(board5,9,9,72)

constant board6 = """
  1 __ .. .. .. __ __ .. .. .. __ __ .. .. .. __ __ .. .. .. __ __ .. .. .. __ __ .. .. .. __ __ .. .. .. __ __ .. .. .. __ __ .. .. .. 82
 .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ ..
 .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. .. __ .. __ .. ..
 __ __ __ .. .. __ __ __ .. .. __ __ __ .. .. __ __ __ .. .. __ __ __ .. .. __ __ __ .. .. __ __ __ .. .. __ __ __ .. .. __ __ __ .. .. .."""
Hidato(board6,46,4,82)
```

{{out}}
<pre style="font-size: 8px">
 32 33 35 36 37  .  .  .
 31 34 24 22 38  .  .  .
 30 25 23 21 12 39  .  .
 29 26 20 13 40 11  .  .
 27 28 14 19  9 10  1  .
  .  . 15 16 18  8  2  .
  .  .  .  . 17  7  6  3
  .  .  .  .  .  .  5  4
solution found in 760 tries (0.00s)
 . 4 .
 3 7 5
 1 2 6
solution found in 10 tries (0.00s)
  1  2  3  .  .  8  9  .  . 14 15  .  . 20 21  .  . 26 27  .  . 32 33  .  . 38 39  .  . 44 45  .  . 50 51  .  . 56 57  .  . 62 63  .  . 68 69  .  . 74
  .  .  4  .  7  . 10  . 13  . 16  . 19  . 22  . 25  . 28  . 31  . 34  . 37  . 40  . 43  . 46  . 49  . 52  . 55  . 58  . 61  . 64  . 67  . 70  . 73  .
  .  .  .  5  6  .  . 11 12  .  . 17 18  .  . 23 24  .  . 29 30  .  . 35 36  .  . 41 42  .  . 47 48  .  . 53 54  .  . 59 60  .  . 65 66  .  . 71 72  .
solution found in 74 tries (0.00s)
 54 53 60 59 58 67 66 69 70
 52 55 61 57 63 65 68 72 71
 51 50 56 62 64  .  .  .  .
 49 12 13 14  .  . 17 21  .
 48 10 11  . 15 16 18 20 22
 47 46  9  .  3  2 19 23 24
 45 44  8  5  4  1 33 32 25
 41 43  7  6 36 34 27 26 31
 42 40 39 38 37 35 28 29 30
solution found in 106 tries (0.00s)
 56 58 54 60 61 62 63 66 67
 57 55 59 53 49 47 65 64 68
  9  8 52 51 50 48 46 45 69
 10  6  7  .  .  . 44 43 70
  5 11 12  .  .  . 72 71 42
  4 14 13  .  .  . 30 39 41
 15  3 17 18 28 29 38 31 40
  2 16 19 22 25 27 37 36 32
  1 20 21 24 23 26 35 34 33
solution found in 495 tries (0.00s)
  1  2  .  .  . 10 11  .  .  . 19 20  .  .  . 28 29  .  .  . 37 38  .  .  . 46 47  .  .  . 55 56  .  .  . 64 65  .  .  . 73 74  .  .  . 82
  .  .  3  .  9  .  . 12  . 18  .  . 21  . 27  .  . 30  . 36  .  . 39  . 45  .  . 48  . 54  .  . 57  . 63  .  . 66  . 72  .  . 75  . 81  .
  .  4  .  8  .  . 13  . 17  .  . 22  . 26  .  . 31  . 35  .  . 40  . 44  .  . 49  . 53  .  . 58  . 62  .  . 67  . 71  .  . 76  . 80  .  .
  5  6  7  .  . 14 15 16  .  . 23 24 25  .  . 32 33 34  .  . 41 42 43  .  . 50 51 52  .  . 59 60 61  .  . 68 69 70  .  . 77 78 79  .  .  .
solution found in 82 tries (0.02s)

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(de hidato (Lst)
   (let Grid (grid (length (maxi length Lst)) (length Lst))
      (mapc
         '((G L)
            (mapc
               '((This Val)
                  (nond
                     (Val
                        (with (: 0 1 1) (con (: 0 1)))    # Cut off west
                        (with (: 0 1 -1) (set (: 0 1)))   # east
                        (with (: 0 -1 1) (con (: 0 -1)))  # south
                        (with (: 0 -1 -1) (set (: 0 -1))) # north
                        (set This) )
                     ((=T Val) (=: val Val)) ) )
               G L ) )
         Grid
         (apply mapcar (reverse Lst) list) )
      (let Todo
         (by '((This) (: val)) sort
            (mapcan '((Col) (filter '((This) (: val)) Col))
               Grid ) )
         (let N 1
            (with (pop 'Todo)
               (recur (N Todo)
                  (unless (> (inc 'N) (; Todo 1 val))
                     (find
                        '((Dir)
                           (with (Dir This)
                              (cond
                                 ((= N (: val))
                                    (if (cdr Todo) (recurse N @) T) )
                                 ((not (: val))
                                    (=: val N)
                                    (or (recurse N Todo) (=: val NIL)) ) ) ) )
                        (quote
                           west east south north
                           ((X) (or (south (west X)) (west (south X))))
                           ((X) (or (north (west X)) (west (north X))))
                           ((X) (or (south (east X)) (east (south X))))
                           ((X) (or (north (east X)) (east (north X)))) ) ) ) ) ) ) )
      (disp Grid 0
         '((This)
            (if (: val) (align 3 @) "   ") ) ) ) )
```

Test:

```PicoLisp
(hidato
   (quote
      (T   33  35  T   T)
      (T   T   24  22  T)
      (T   T   T   21  T   T)
      (T   26  T   13  40  11)
      (27  T   T   T   9   T   1)
      (NIL NIL T   T   18  T   T)
      (NIL NIL NIL NIL T   7   T  T)
      (NIL NIL NIL NIL NIL NIL 5  T) ) )
```

Output:

```txt
   +---+---+---+---+---+---+---+---+
 8 | 32  33  35  36  37|   |   |   |
   +   +   +   +   +   +---+---+---+
 7 | 31  34  24  22  38|   |   |   |
   +   +   +   +   +   +---+---+---+
 6 | 30  25  23  21  12  39|   |   |
   +   +   +   +   +   +   +---+---+
 5 | 29  26  20  13  40  11|   |   |
   +   +   +   +   +   +   +---+---+
 4 | 27  28  14  19   9  10   1|   |
   +---+---+   +   +   +   +   +---+
 3 |   |   | 15  16  18   8   2|   |
   +---+---+---+---+   +   +   +---+
 2 |   |   |   |   | 17   7   6   3|
   +---+---+---+---+---+---+   +   +
 1 |   |   |   |   |   |   |  5   4|
   +---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h
```



## Prolog

Works with SWI-Prolog and library(clpfd) written by '''Markus Triska'''.

Puzzle solved is from the Wilkipedia page : http://en.wikipedia.org/wiki/Hidato

```Prolog
:- use_module(library(clpfd)).

hidato :-
	init1(Li),
	% skip first blank line
	init2(1, 1, 10, Li),
	my_write(Li).


init1(Li) :-
	Li = [  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
		0,  A, 33, 35,  B,  C,  0,  0,  0,  0,
	        0,  D,  E, 24, 22,  F,  0,  0,  0,  0,
	        0,  G,  H,  I, 21,  J,  K,  0,  0,  0,
	        0,  L, 26,  M, 13, 40, 11,  0,  0,  0,
	        0, 27,  N,  O,  P,  9,  Q,  1,  0,  0,
	        0,  0,  0,  R,  S, 18,  T,  U,  0,  0,
	        0,  0,  0,  0,  0,  V,  7,  W,  X,  0,
	        0,  0,  0,  0,  0,  0,  0,  5,  Y,  0,
	        0,  0,  0,  0,  0,  0,  0,  0,  0,  0],

	LV = [  A, 33, 35,  B,  C,
	        D,  E, 24, 22,  F,
	        G,  H,  I, 21,  J,  K,
	        L, 26,  M, 13, 40, 11,
	       27,  N,  O,  P,  9,  Q,  1,
	                R,  S, 18,  T,  U,
	                        V,  7,  W,  X,
					5,  Y],


	LV ins 1..40,
	all_distinct(LV).

% give the constraints
% Stop before the last line
init2(_N, Col, Max_Col, _L) :-
	Col is Max_Col - 1.

% skip zeros
init2(N, Lig, Col, L) :-
	I is N + Lig * Col,
	element(I, L, 0),
	!,
	V is N+1,
	(   V > Col -> N1 = 1, Lig1 is Lig + 1; N1 = V, Lig1 = Lig),
	init2(N1, Lig1, Col, L).


% skip first column
init2(1, Lig, Col, L) :-
	!,
	init2(2, Lig, Col, L) .

% skip last column
init2(Col, Lig, Col, L) :-
	!,
	Lig1 is Lig+1,
	init2(1, Lig1, Col, L).

% V5 V3 V6
% V1  V V2
% V7 V4 V8
% general case
init2(N, Lig, Col, L) :-
	I is N + Lig * Col,
	element(I, L, V),

	I1 is I  - 1, I2 is I  + 1, I3 is I  - Col, I4 is I  + Col,
	I5 is I3 - 1, I6 is I3 + 1, I7 is I4 - 1,   I8 is I4 + 1,

	maplist(compute_BI(L, V), [I1,I2,I3,I4,I5,I6,I7,I8], VI, BI),

	sum(BI, #=, SBI),

	(  ((V #=  1 #\/ V #=  40) #/\ SBI #= 1) #\/
	    (V #\= 1 #/\ V #\= 40  #/\ SBI #= 2)) #<==> 1,

	labeling([ffc, enum], [V | VI]),

	N1 is N+1,
	init2(N1, Lig, Col, L).

compute_BI(L, V, I, VI, BI) :-
	element(I, L, VI),
	VI #= 0 #==> BI #= 0,
	( VI #\= 0 #/\ (V - VI #= 1 #\/ VI - V #= 1))  #<==> BI.

% display the result
my_write([0, A, B, C, D, E, F, G, H, 0 | T]) :-
	maplist(my_write_1, [A, B, C, D, E, F, G, H]), nl,
	my_write(T).

my_write([]).

my_write_1(0) :-
	write('   ').

my_write_1(X) :-
	writef('%3r', [X]).
```

{{out}}

```txt
?- hidato.
 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4
true
```



## Python


```python
board = []
given = []
start = None

def setup(s):
    global board, given, start
    lines = s.splitlines()
    ncols = len(lines[0].split())
    nrows = len(lines)
    board = [[-1] * (ncols + 2) for _ in xrange(nrows + 2)]

    for r, row in enumerate(lines):
        for c, cell in enumerate(row.split()):
            if cell == "__" :
                board[r + 1][c + 1] = 0
                continue
            elif cell == ".":
                continue # -1
            else:
                val = int(cell)
                board[r + 1][c + 1] = val
                given.append(val)
                if val == 1:
                    start = (r + 1, c + 1)
    given.sort()

def solve(r, c, n, next=0):
    if n > given[-1]:
        return True
    if board[r][c] and board[r][c] != n:
        return False
    if board[r][c] == 0 and given[next] == n:
        return False

    back = 0
    if board[r][c] == n:
        next += 1
        back = n

    board[r][c] = n
    for i in xrange(-1, 2):
        for j in xrange(-1, 2):
            if solve(r + i, c + j, n + 1, next):
                return True
    board[r][c] = back
    return False

def print_board():
    d = {-1: "  ", 0: "__"}
    bmax = max(max(r) for r in board)
    form = "%" + str(len(str(bmax)) + 1) + "s"
    for r in board[1:-1]:
        print "".join(form % d.get(c, str(c)) for c in r[1:-1])

hi = """\
__ 33 35 __ __  .  .  .
__ __ 24 22 __  .  .  .
__ __ __ 21 __ __  .  .
__ 26 __ 13 40 11  .  .
27 __ __ __  9 __  1  .
 .  . __ __ 18 __ __  .
 .  .  .  . __  7 __ __
 .  .  .  .  .  .  5 __"""

setup(hi)
print_board()
solve(start[0], start[1], 1)
print
print_board()
```

{{out}}

```txt
 __ 33 35 __ __
 __ __ 24 22 __
 __ __ __ 21 __ __
 __ 26 __ 13 40 11
 27 __ __ __  9 __  1
       __ __ 18 __ __
             __  7 __ __
                    5 __

 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4
```



## Racket


### Standalone

Algorithm is depth first search for each number, repeating for all numbers in ascending order.
It currently runs slowish due to temporary shortcomings in untyped Racket's array indexing, but finished
immediately when tested with custom 2d vector library.


```Racket

#lang racket
(require math/array)

;#f = not a legal position, #t = blank position
(define board
  (array
   #[#[#t 33 35 #t #t #f #f #f]
     #[#t #t 24 22 #t #f #f #f]
     #[#t #t #t 21 #t #t #f #f]
     #[#t 26 #t 13 40 11 #f #f]
     #[27 #t #t #t  9 #t  1 #f]
     #[#f #f #t #t 18 #t #t #f]
     #[#f #f #f #f #t  7 #t #t]
     #[#f #f #f #f #f #f  5 #t]]))

;filters elements with the predicate, returning the element and its indices
(define (array-indices-of a f)
  (for*/list ([i (range 0 (vector-ref (array-shape a) 0))]
              [j (range 0 (vector-ref (array-shape a) 1))]
              #:when (f (array-ref a (vector i j))))
    (list (array-ref a (vector i j)) i j)))

;returns a list, each element is a list of the number followed by i and j indices
;sorted ascending by number
(define (goal-list v) (sort (array-indices-of v number?) (λ (a b) (< (car a) (car b)))))

;every direction + start position that's on the board
(define (legal-moves a i0 j0)
  (for*/list ([i (range (sub1 i0) (+ i0 2))]
              [j (range (sub1 j0) (+ j0 2))]
              ;cartesian product -1..1 and -1..1, except 0 0
              #:when (and (not (and (= i i0) (= j j0)))
                          ;make sure it's on the board
                          (<= 0 i (sub1 (vector-ref (array-shape a) 0)))
                          (<= 0 j (sub1 (vector-ref (array-shape a) 1)))
                          ;make sure it's an actual position too (the real board isn't square)
                          (array-ref a (vector i j))))
    (cons i j)))

;find path through array, returning list of coords from start to finish
(define (hidato-path a)
  ;get starting position as first goal
  (match-let ([(cons (list n i j) goals) (goal-list a)])
    (let hidato ([goals goals] [n n] [i i] [j j] [path '()])
      (match goals
        ;no more goals, return path
        ['() (reverse (cons (cons i j) path))]
        ;get next goal
        [(cons (list n-goal i-goal j-goal) _)
         (let ([move (cons i j)])
           ;already visiting a spot or taking too many moves to reach the next goal is no good
           (cond [(or (member move path) (> n n-goal)) #f]
                 ;taking the right number of moves to be at the goal square is good
                 ;so go to the next goal
                 [(and (= n n-goal) (= i i-goal) (= j j-goal))
                  (hidato (cdr goals) n i j path)]
                 ;depth first search using every legal move to find next goal
                 [else (ormap (λ (m) (hidato goals (add1 n) (car m) (cdr m) (cons move path)))
                              (legal-moves a i j))]))]))))

;take a path and insert it into the array
(define (put-path a path)
  (let ([a (array->mutable-array a)])
    (for ([n (range 1 (add1 (length path)))] [move path])
      (array-set! a (vector (car move) (cdr move)) n))
    a))

;main function
(define (hidato board) (put-path board (hidato-path board)))

```

{{out}}

```txt

> (hidato board)
(mutable-array
 #[#[32 33 35 36 37 #f #f #f]
   #[31 34 24 22 38 #f #f #f]
   #[30 25 23 21 12 39 #f #f]
   #[29 26 20 13 40 11 #f #f]
   #[27 28 14 19 9 10 1 #f]
   #[#f #f 15 16 18 8 2 #f]
   #[#f #f #f #f 17 7 6 3]
   #[#f #f #f #f #f #f 5 4]])

```


===Using Hidato Family Solver from [[Solve a Numbrix puzzle#Racket|Numbrix]]===

This solution uses the module "hidato-family-solver.rkt" from
[[Solve a Numbrix puzzle#Racket]]. The difference between the two is
essentially the neighbourhood function.


```racket
#lang racket
(require "hidato-family-solver.rkt")

(define moore-neighbour-offsets
  '((+1 0) (-1 0) (0 +1) (0 -1) (+1 +1) (-1 -1) (-1 +1) (+1 -1)))

(define solve-hidato (solve-hidato-family moore-neighbour-offsets))

(displayln
 (puzzle->string
  (solve-hidato
   #(#( 0 33 35  0  0)
     #( 0  0 24 22  0)
     #( 0  0  0 21  0  0)
     #( 0 26  0 13 40 11)
     #(27  0  0  0  9  0  1)
     #( _  _  0  0 18  0  0)
     #( _  _  _  _  0  7  0  0)
     #( _  _  _  _  _  _  5  0)))))

```


{{out}}

```txt
32 33 35 36 37  _  _  _
31 34 24 22 38  _  _  _
30 25 23 21 12 39  _  _
29 26 20 13 40 11  _  _
27 28 14 19  9 10  1  _
 _  _ 15 16 18  8  2  _
 _  _  _  _ 17  7  6  3
 _  _  _  _  _  _  5  4
```



## REXX

Programming note:   the coördinates for the cells used are the same as an   X Y   grid, that is,

the bottom left-most cell is   1 1   and the tenth cell on row 2 is   2 10

If ''any'' marker is negative, then it's assumed to be a Numbrix puzzle (and the absolute value is used).

Over half of the REXX program deals with validating the input and displaying the puzzle.

''Hidato''   and   ''Numbrix''   are registered trademarks.

```rexx
/*REXX program solves a  Numbrix (R) puzzle, it also displays the puzzle and solution.  */
maxR=0;    maxC=0;    maxX=0;     minR=9e9;      minC=9e9;    minX=9e9;    cells=0;    @.=
parse arg xxx;        PZ='Hidato puzzle'         /*get the cell definitions from the CL.*/
xxx=translate(xxx, , "/\;:_", ',')               /*also allow other characters as comma.*/

               do  while xxx\='';  parse var  xxx    r c   marks  ','  xxx
                   do  while marks\='';          _=@.r.c
                   parse var marks  x  marks
                   if datatype(x,'N')  then do;  x=x/1                     /*normalize X*/
                                                 if x<0  then PZ= 'Numbrix puzzle'
                                                 x=abs(x)                  /*use  │x│   */
                                            end
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
if \next(2,!r,!c)  then call err  'No solution possible for this'   PZ    "puzzle."
say 'A solution for the'   PZ    "exists.";      say;                     call show
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:  say;    say '***error*** (from' PZ"): "    arg(1);        say;          exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
next: procedure expose @. Nr. Nc. cells pMoves;  parse arg #,r,c;   ##=#+1
           do t=1  for pMoves                                   /* [↓]  try some moves. */
           parse value  r+Nr.t c+Nc.t  with nr nc               /*next move coördinates.*/
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

'''output'''   when using the following as input:

 <tt> 1 7 5 .\2 5 . 7 . .\3 3 . . 18 . .\4 1 27 . . . 9 . 1\5 1 . 26 . 13 40 11\6 1 . . . 21 . .\7 1 . . 24 22 .\8 1 . 33 35 . .</tt>

```txt

  . 33 35  .  .
  .  . 24 22  .
  .  .  . 21  .  .
  . 26  . 13 40 11
 27  .  .  .  9  .  1
        .  . 18  .  .
              .  7  .  .
                    5  .


A solution for the Hidato puzzle exists.

 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4

```



## Ruby


### Without Warnsdorff

The following class provides functionality for solving a hidato problem:

```ruby
# Solve a Hidato Puzzle
#
class Hidato
  Cell = Struct.new(:value, :used, :adj)
  ADJUST = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

  def initialize(board, pout=true)
    @board = []
    board.each_line do |line|
      @board << line.split.map{|n| Cell[Integer(n), false] rescue nil} + [nil]
    end
    @board << []                                # frame (Sentinel value : nil)
    @board.each_with_index do |row, x|
      row.each_with_index do |cell, y|
        if cell
          @sx, @sy = x, y  if cell.value==1     # start position
          cell.adj = ADJUST.map{|dx,dy| [x+dx,y+dy]}.select{|xx,yy| @board[xx][yy]}
        end
      end
    end
    @xmax = @board.size - 1
    @ymax = @board.map(&:size).max - 1
    @end  = @board.flatten.compact.size
    puts to_s('Problem:')  if pout
  end

  def solve
    @zbl = Array.new(@end+1, false)
    @board.flatten.compact.each{|cell| @zbl[cell.value] = true}
    puts (try(@board[@sx][@sy], 1) ? to_s('Solution:') : "No solution")
  end

  def try(cell, seq_num)
    return true  if seq_num > @end
    return false if cell.used
    value = cell.value
    return false if value > 0 and value != seq_num
    return false if value == 0 and @zbl[seq_num]
    cell.used = true
    cell.adj.each do |x, y|
      if try(@board[x][y], seq_num+1)
        cell.value = seq_num
        return true
      end
    end
    cell.used = false
  end

  def to_s(msg=nil)
    str = (0...@xmax).map do |x|
      (0...@ymax).map{|y| "%3s" % ((c=@board[x][y]) ? c.value : c)}.join
    end
    (msg ? [msg] : []) + str + [""]
  end
end
```


'''Test:'''

```ruby
# Which may be used as follows to solve Evil Case 1:
board1 = <<EOS
  .  4
  0  7  0
  1  0  0
EOS
Hidato.new(board1).solve

# Which may be used as follows to solve this tasks example:
board2 = <<EOS
  0 33 35  0  0
  0  0 24 22  0
  0  0  0 21  0  0
  0 26  0 13 40 11
 27  0  0  0  9  0  1
  .  .  0  0 18  0  0
  .  .  .  .  0  7  0  0
  .  .  .  .  .  .  5  0
EOS
Hidato.new(board2).solve

# Which may be used as follows to solve The Snake in the Grass:
board3 = <<EOS
  1  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  . 74
  .  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .
  .  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .
EOS
t0 = Time.now
Hidato.new(board3).solve
puts " #{Time.now - t0} sec"
```


{{out}}

```txt

Problem:
     4
  0  7  0
  1  0  0

Solution:
     4
  3  7  5
  1  2  6

Problem:
  0 33 35  0  0
  0  0 24 22  0
  0  0  0 21  0  0
  0 26  0 13 40 11
 27  0  0  0  9  0  1
        0  0 18  0  0
              0  7  0  0
                    5  0

Solution:
 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4

Problem:
  1  0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0       74
        0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
           0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0

Solution:
  1  2  3        8  9       14 15       20 21       26 27       32 33       38 39       44 45       50 51       56 57       62 63       68 69       74
        4     7    10    13    16    19    22    25    28    31    34    37    40    43    46    49    52    55    58    61    64    67    70    73
           5  6       11 12       17 18       23 24       29 30       35 36       41 42       47 48       53 54       59 60       65 66       71 72

 40.198299 sec

```



### With Warnsdorff

I modify method as follows to implement [[wp:Knight's_tour#Warnsdorff|Warnsdorff]] like

```ruby
# Solve a Hidato Like Puzzle with Warnsdorff like logic applied
#
class HLPsolver
  attr_reader :board
  Cell = Struct.new(:value, :used, :adj)

  def initialize(board, pout=true)
    @board = []
    frame = ADJACENT.flatten.map(&:abs).max
    board.each_line do |line|
      @board << line.split.map{|n| Cell[Integer(n), false] rescue nil} + [nil]*frame
    end
    frame.times {@board << []}                  # frame (Sentinel value : nil)
    @board.each_with_index do |row, x|
      row.each_with_index do |cell, y|
        if cell
          @sx, @sy = x, y  if cell.value==1     # start position
          cell.adj = ADJACENT.map{|dx,dy| [x+dx,y+dy]}.select{|xx,yy| @board[xx][yy]}
        end
      end
    end
    @xmax = @board.size - frame
    @ymax = @board.map(&:size).max - frame
    @end  = @board.flatten.compact.size
    @format = " %#{@end.to_s.size}s"
    puts to_s('Problem:')  if pout
  end

  def solve
    @zbl = Array.new(@end+1, false)
    @board.flatten.compact.each{|cell| @zbl[cell.value] = true}
    puts (try(@board[@sx][@sy], 1) ? to_s('Solution:') : "No solution")
  end

  def try(cell, seq_num)
    value = cell.value
    return false if value > 0 and value != seq_num
    return false if value == 0 and @zbl[seq_num]
    cell.used = true
    if seq_num == @end
      cell.value = seq_num
      return true
    end
    a = []
    cell.adj.each_with_index do |(x, y), n|
      cl = @board[x][y]
      a << [wdof(cl.adj)*10+n, x, y]  unless cl.used
    end
    a.sort.each do |key, x, y|
      if try(@board[x][y], seq_num+1)
        cell.value = seq_num
        return true
      end
    end
    cell.used = false
  end

  def wdof(adj)
    adj.count {|x,y| not @board[x][y].used}
  end

  def to_s(msg=nil)
    str = (0...@xmax).map do |x|
      (0...@ymax).map{|y| @format % ((c=@board[x][y]) ? c.value : c)}.join
    end
    (msg ? [msg] : []) + str + [""]
  end
end
```

Which may be used as follows to solve Hidato Puzzles:

```ruby
require 'HLPsolver'

ADJACENT = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

# solve Evil Case 1:
board1 = <<EOS
  .  4
  0  7  0
  1  0  0
EOS
HLPsolver.new(board1).solve

boardx = <<EOS
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  1  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
EOS
HLPsolver.new(boardx).solve

# solve this tasks example:
board2 = <<EOS
  0 33 35  0  0
  0  0 24 22  0
  0  0  0 21  0  0
  0 26  0 13 40 11
 27  0  0  0  9  0  1
  .  .  0  0 18  0  0
  .  .  .  .  0  7  0  0
  .  .  .  .  .  .  5  0
EOS
HLPsolver.new(board2).solve

#solve The Snake in the Grass:
board3 = <<EOS
  1  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  . 74
  .  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .  0  .
  .  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .  .  0  0  .
EOS
t0 = Time.now
HLPsolver.new(board3).solve
puts " #{Time.now - t0} sec"
```


Which produces:

```txt

Problem:
   4
 0 7 0
 1 0 0

Solution:
   4
 3 7 5
 1 2 6

Problem:
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  1  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0
  0  0  0  0  0  0  0  0

Solution:
 33 34 36 37 41 42 43 44
 32 35 38 40 56 55 46 45
  2 31 39 57 59 60 54 47
  3  1 30 58 61 62 53 48
  4  6 18 29 63 64 52 49
  5  7 17 19 28 51 50 25
  8 11 13 16 20 27 26 24
  9 10 12 14 15 21 22 23

Problem:
  0 33 35  0  0
  0  0 24 22  0
  0  0  0 21  0  0
  0 26  0 13 40 11
 27  0  0  0  9  0  1
        0  0 18  0  0
              0  7  0  0
                    5  0

Solution:
 32 33 35 36 37
 31 34 24 22 38
 30 25 23 21 12 39
 29 26 20 13 40 11
 27 28 14 19  9 10  1
       15 16 18  8  2
             17  7  6  3
                    5  4

Problem:
  1  0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0       74
        0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
           0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0        0  0

Solution:
  1  2  3        8  9       14 15       20 21       26 27       32 33       38 39       44 45       50 51       56 57       62 63       68 69       74
        4     7    10    13    16    19    22    25    28    31    34    37    40    43    46    49    52    55    58    61    64    67    70    73
           5  6       11 12       17 18       23 24       29 30       35 36       41 42       47 48       53 54       59 60       65 66       71 72

 0.003001 sec

```


HLPsolver may be used to solve [[Knight's tour#Ruby|Knight's tour]]:


## Seed7


```seed7
$ include "seed7_05.s7i";

var set of integer: given is {};
var array array integer: board is 0 times 0 times 0;
var integer: startRow is 0;
var integer: startColumn is 0;

const proc: setup (in array string: input) is func
  local
    var integer: r is 0;
    var integer: c is 0;
    var array string: row is 0 times "";
    var string: cell is "";
    var integer: value is 0;
  begin
    board := (length(input) + 2) times 0 times 0;
    for key r range input do
      row := split(input[r], " ");
      board[r + 1] := (length(row) + 2) times - 1;
      for key c range row do
        cell := row[c];
        if cell = "_" then
          board[r + 1][c + 1] := 0;
        elsif cell[1] in {'0' .. '9'} then
          value := integer parse cell;
          board[r + 1][c + 1] := value;
          incl(given, value);
          if value = 1 then
            startRow := r + 1;
            startColumn := c + 1;
          end if;
        end if;
      end for;
    end for;
    board[1] := (length(row) + 2) times - 1;
    board[length(input) + 2] := (length(row) + 2) times - 1;
  end func;

const func boolean: solve (in integer: r, in integer: c, in integer: n) is func
  result
    var boolean: solved is FALSE;
  local
    var integer: back is 0;
    var integer: i is 0;
    var integer: j is 0;
  begin
    if n > max(given) then
      solved := TRUE;
    elsif board[r][c] = 0 and n not in given or board[r][c] = n then
      back := board[r][c];
      board[r][c] := n;
      for i range -1 to 1 until solved do
        for j range -1 to 1 until solved do
          solved := solve(r + i, c + j, n + 1);
        end for;
      end for;
      if not solved then
        board[r][c] := back;
      end if;
    end if;
  end func;

const proc: printBoard is func
  local
    var integer: r is 0;
    var integer: c is 0;
  begin
    for key r range board do
      for c range board[r] do
        if c = -1 then
          write(" . ");
        elsif c > 0 then
          write(c lpad 2 <& " ");
        else
          write("__ ");
        end if;
      end for;
      writeln;
    end for;
  end func;

const proc: main is func
  local
    const array string: input is [] ("_ 33 35 _ _ . . .",
                                     "_ _ 24 22 _ . . .",
                                     "_ _ _ 21 _ _ . .",
                                     "_ 26 _ 13 40 11 . .",
                                     "27 _ _ _ 9 _ 1 .",
                                     ". . _ _ 18 _ _ .",
                                     ". . . . _ 7 _ _",
                                     ". . . . . . 5 _");
  begin
    setup(input);
    printBoard;
    writeln;
    if solve(startRow, startColumn, 1) then
      writeln("Found:");
      printBoard;
    end if;
  end func;
```


{{out}}

```txt

 .  .  .  .  .  .  .  .  .  .
 . __ 33 35 __ __  .  .  .  .
 . __ __ 24 22 __  .  .  .  .
 . __ __ __ 21 __ __  .  .  .
 . __ 26 __ 13 40 11  .  .  .
 . 27 __ __ __  9 __  1  .  .
 .  .  . __ __ 18 __ __  .  .
 .  .  .  .  . __  7 __ __  .
 .  .  .  .  .  .  .  5 __  .
 .  .  .  .  .  .  .  .  .  .

Found:
 .  .  .  .  .  .  .  .  .  .
 . 32 33 35 36 37  .  .  .  .
 . 31 34 24 22 38  .  .  .  .
 . 30 25 23 21 12 39  .  .  .
 . 29 26 20 13 40 11  .  .  .
 . 27 28 14 19  9 10  1  .  .
 .  .  . 15 16 18  8  2  .  .
 .  .  .  .  . 17  7  6  3  .
 .  .  .  .  .  .  .  5  4  .
 .  .  .  .  .  .  .  .  .  .

```



## Tailspin

{{trans|Java}}

```tailspin

def input:
'__ 33 35 __ __  .  .  .
 __ __ 24 22 __  .  .  .
 __ __ __ 21 __ __  .  .
 __ 26 __ 13 40 11  .  .
 27 __ __ __  9 __  1  .
  .  . __ __ 18 __ __  .
  .  .  .  . __  7 __ __
  .  .  .  .  .  .  5 __';

templates hidato
  composer setup
    @: {row: 1, col: 1, given:[]};
    { board: [ <line>+ ], given: $@.given -> [i](<{}> { n: $i, $...} !) }
    rule line: [ <cell>+ ] (<'\n '>?) (..|@: {row: $@.row + 1, col: 1};)
    rule cell: <open|blocked|given> (<' '>?) (@.col: $@.col + 1;)
    rule open: <'__'> -> 0
    rule blocked: <' \.'> -> -1
    rule given: (<' '>?) (def given: <INT>;)
      ($given -> ..|@.given: $@.given::length+1..$ -> [];)
      ($given -> @.given($): { row: $@.row, col: $@.col };)
      $given
  end setup

  templates solve
    <~{row: <1..$@hidato.board::length>, col: <1..$@hidato.board(1)::length>}> !VOID
    <{ n: <$@hidato.given(-1).n>, row: <$@hidato.given(-1).row>, col: <$@hidato.given(-1).col> }> $@hidato.board !
    <?($@hidato.board($.row; $.col) <~0|$.n>)> !VOID
    <?($@hidato.board($.row; $.col) <0>)?($@hidato.given($.next) <$.n>)> !VOID
    <>
      def guess: $;
      def back: $@hidato.board($.row; $.col);
      def next: $ -> (<{n: <$back>}> $.next + 1! <> $.next!);
      @hidato.board($.row; $.col): $.n;
      0..8 -> { next: $next, n: $guess.n + 1, row: $guess.row + $ / 3 - 1, col: $guess.col + $ mod 3 - 1 } -> #
      @hidato.board($.row; $.col): $back;
  end solve

  @: $ -> setup;
  { next: 1, $@.given(1)... } -> solve !
end hidato

$input -> hidato -> '$... -> '$... -> ' $ -> (<-1> ' .' ! <10..> '$;' ! <> ' $;' !);';
';
' ->!OUT::write

```

{{out}}

```txt

 32 33 35 36 37  .  .  .
 31 34 24 22 38  .  .  .
 30 25 23 21 12 39  .  .
 29 26 20 13 40 11  .  .
 27 28 14 19  9 10  1  .
  .  . 15 16 18  8  2  .
  .  .  .  . 17  7  6  3
  .  .  .  .  .  .  5  4

```



## Tcl


```tcl
proc init {initialConfiguration} {
    global grid max filled
    set max 1
    set y 0
    foreach row [split [string trim $initialConfiguration "\n"] "\n"] {
	set x 0
	set rowcontents {}
	foreach cell $row {
	    if {![string is integer -strict $cell]} {set cell -1}
	    lappend rowcontents $cell
	    set max [expr {max($max, $cell)}]
	    if {$cell > 0} {
		dict set filled $cell [list $y $x]
	    }
	    incr x
	}
	lappend grid $rowcontents
	incr y
    }
}

proc findseps {} {
    global max filled
    set result {}
    for {set i 1} {$i < $max-1} {incr i} {
	if {[dict exists $filled $i]} {
	    for {set j [expr {$i+1}]} {$j <= $max} {incr j} {
		if {[dict exists $filled $j]} {
		    if {$j-$i > 1} {
			lappend result [list $i $j [expr {$j-$i}]]
		    }
		    break
		}
	    }
	}
    }
    return [lsort -integer -index 2 $result]
}

proc makepaths {sep} {
    global grid filled
    lassign $sep from to len
    lassign [dict get $filled $from] y x
    set result {}
    foreach {dx dy} {-1 -1  -1 0  -1 1  0 -1  0 1  1 -1  1 0  1 1} {
	discover [expr {$x+$dx}] [expr {$y+$dy}] [expr {$from+1}] $to \
	    [list [list $from $x $y]] $grid
    }
    return $result
}
proc discover {x y n limit path model} {
    global filled
    # Check for illegal
    if {[lindex $model $y $x] != 0} return
    upvar 1 result result
    lassign [dict get $filled $limit] ly lx
    # Special case
    if {$n == $limit-1} {
	if {abs($x-$lx)<=1 && abs($y-$ly)<=1 && !($lx==$x && $ly==$y)} {
	    lappend result [lappend path [list $n $x $y] [list $limit $lx $ly]]
	}
	return
    }
    # Check for impossible
    if {abs($x-$lx) > $limit-$n || abs($y-$ly) > $limit-$n} return
    # Recursive search
    lappend path [list $n $x $y]
    lset model $y $x $n
    incr n
    foreach {dx dy} {-1 -1  -1 0  -1 1  0 -1  0 1  1 -1  1 0  1 1} {
	discover [expr {$x+$dx}] [expr {$y+$dy}] $n $limit $path $model
    }
}

proc applypath {path} {
    global grid filled
    puts "Found unique path for [lindex $path 0 0] -> [lindex $path end 0]"
    foreach cell [lrange $path 1 end-1] {
	lassign $cell n x y
	lset grid $y $x $n
	dict set filled $n [list $y $x]
    }
}

proc printgrid {} {
    global grid max
    foreach row $grid {
	foreach cell $row {
	    puts -nonewline [format " %*s" [string length $max] [expr {
		$cell==-1 ? "." : $cell
	    }]]
	}
	puts ""
    }
}

proc solveHidato {initialConfiguration} {
    init $initialConfiguration
    set limit [llength [findseps]]
    while {[llength [set seps [findseps]]] && [incr limit -1]>=0} {
	foreach sep $seps {
	    if {[llength [set paths [makepaths $sep]]] == 1} {
		applypath [lindex $paths 0]
		break
	    }
	}
    }
    puts ""
    printgrid
}
```

Demonstrating (dots are “outside” the grid, and zeroes are the cells to be filled in):

```tcl
solveHidato "
     0  33  35   0   0   .   .   .
     0   0  24  22   0   .   .   .
     0   0   0  21   0   0   .   .
     0  26   0  13  40  11   .   .
    27   0   0   0   9   0   1   .
     .   .   0   0  18   0   0   .
     .   .   .   .   0   7   0   0
     .   .   .   .   .   .   5   0
"
```

{{out}}

```txt

Found unique path for 5 -> 7
Found unique path for 7 -> 9
Found unique path for 9 -> 11
Found unique path for 11 -> 13
Found unique path for 33 -> 35
Found unique path for 18 -> 21
Found unique path for 1 -> 5
Found unique path for 35 -> 40
Found unique path for 22 -> 24
Found unique path for 24 -> 26
Found unique path for 27 -> 33
Found unique path for 13 -> 18

 32 33 35 36 37  .  .  .
 31 34 24 22 38  .  .  .
 30 25 23 21 12 39  .  .
 29 26 20 13 40 11  .  .
 27 28 14 19  9 10  1  .
  .  . 15 16 18  8  2  .
  .  .  .  . 17  7  6  3
  .  .  .  .  .  .  5  4

```

More complex cases are solvable with an [[/Extended Tcl solution|extended version of this code]], though that has more onerous version requirements.


## zkl

{{trans|Python}}

```zkl
hi:=  // 0==empty cell, X==not a cell
#<<<
 "0  33  35   0   0   X   X   X
  0   0  24  22   0   X   X   X
  0   0   0  21   0   0   X   X
  0  26   0  13  40  11   X   X
 27   0   0   0   9   0   1   X
  X   X   0   0  18   0   0   X
  X   X   X   X   0   7   0   0
  X   X   X   X   X   X   5   0";
#<<<

board,given,start:=setup(hi);
print_board(board);
solve(board,given, start.xplode(), 1);
println();
print_board(board);
```


```zkl
fcn print_board(board){
   d:=D(-1,"  ", 0,"__");
   foreach r in (board[1,-1]){
      r[1,-1].pump(String,'wrap(c){ "%2s ".fmt(d.find(c,c)) }).println();
   }
}
fcn setup(s){
   lines:=s.split("\n");
   ncols,nrows:=lines[0].split().len(),lines.len();
   board:=(nrows+2).pump(List(), (ncols+2).pump(List(),-1).copy);
   given,start:=List(),Void;
   foreach r,row in (lines.enumerate()){
      foreach c,cell in (row.split().enumerate()){
         if(cell=="X") continue;   // X == not in play, leave at -1
	 val:=cell.toInt();
	 board[r+1][c+1]=val;
	 given.append(val);
	 if(val==1) start=T(r+1,c+1);
      }
   }
   return(board,given.filter().sort(),start);
}
fcn solve(board,given, r,c,n, next=0){
   if(n>given[-1])                       return(True);
   if(board[r][c] and board[r][c]!=n)    return(False);
   if(board[r][c]==0 and given[next]==n) return(False);

   back:=0;
   if(board[r][c]==n){ next+=1; back=n; }

   board[r][c]=n;
   foreach i,j in ([-1..1],[-1..1]){
      if(solve(board,given, r+i,c+j,n+1, next)) return(True);
   }
   board[r][c]=back;
   False
}
```

{{out}}

```txt

__ 33 35 __ __
__ __ 24 22 __
__ __ __ 21 __ __
__ 26 __ 13 40 11
27 __ __ __  9 __  1
      __ __ 18 __ __
            __  7 __ __
                   5 __

32 33 35 36 37
31 34 24 22 38
30 25 23 21 12 39
29 26 20 13 40 11
27 28 14 19  9 10  1
      15 16 18  8  2
            17  7  6  3
                   5  4

```


[[Category:Puzzles]]
