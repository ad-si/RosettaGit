+++
title = "Solve a Holy Knight's tour"
description = ""
date = 2019-08-05T10:46:20Z
aliases = []
[extra]
id = 17663
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[File:chess_white_knight.jpg|200px||right]]

Chess coaches have been known to inflict a kind of torture on beginners by taking a chess board, placing pennies on some squares and requiring that a Knight's tour be constructed that avoids the squares with pennies. 

This kind of knight's tour puzzle is similar to   [[Solve a Hidato puzzle|Hidato]].

The present task is to produce a solution to such problems. At least demonstrate your program by solving the following:

;Example 1

```txt

  0 0 0 
  0   0 0 
  0 0 0 0 0 0 0
0 0 0     0   0
0   0     0 0 0
1 0 0 0 0 0 0
    0 0   0
      0 0 0

```


Note that the zeros represent the available squares, not the pennies.

Extra credit is available for other interesting examples.


;Related tasks:
* [[A* search algorithm]]
* [[Knight's tour]]
* [[N-queens problem]]
* [[Solve a Hidato puzzle]]
* [[Solve a Hopido puzzle]]
* [[Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle]]





## Ada


This solution uses the package Knights_Tour from [[Knight's Tour#Ada]]. The board is quadratic, the size of the board is read from the command line and the board itself is read from the standard input. For the board itself, Space and Minus indicate a no-go (i.e., a coin on the board), all other characters represent places the knight must visit. A '1' represents the start point. Ill-formatted input will crash the program. 


```Ada
with Knights_Tour, Ada.Text_IO, Ada.Command_Line;
 
procedure Holy_Knight is
   
   Size: Positive := Positive'Value(Ada.Command_Line.Argument(1));
   package KT is new Knights_Tour(Size => Size);
   Board: KT.Tour := (others => (others => Natural'Last));
   
   Start_X, Start_Y: KT.Index:= 1; -- default start place (1,1)
   S: String(KT.Index);
   I: Positive := KT.Index'First;
begin
   -- read the board from standard input
   while not Ada.Text_IO.End_Of_File and I <= Size loop
      S := Ada.Text_IO.Get_Line; 
      for J in KT.Index loop
         if S(J) = ' ' or S(J) = '-' then
            Board(I,J) := Natural'Last;
         elsif S(J) = '1' then 
              Start_X := I; Start_Y := J;  Board(I,J) := 1;
         else Board(I,J) := 0;
         end if;
      end loop;
      I := I + 1;
   end loop;

   -- print the board
   Ada.Text_IO.Put_Line("Start Configuration (Length:" 
                          & Natural'Image(KT.Count_Moves(Board)) & "):");
   KT.Tour_IO(Board, Width => 1);
   Ada.Text_IO.New_Line;

   -- search for the tour and print it
   Ada.Text_IO.Put_Line("Tour:");
   KT.Tour_IO(KT.Warnsdorff_Get_Tour(Start_X, Start_Y, Board));
end Holy_Knight;
```


{{out}}


```txt
>holy_knight 8 < standard_problem.txt
Start Configuration (Length: 36):
--000---
--0-00--
-0000000
000--0-0
0-0--000
1000000-
--00-0--
---000--

Tour:
   -   -  30  15  20   -   -   -
   -   -  21   -  29  16   -   -
   -  33  14  31  22  19   6  17
  13  36  23   -   -  28   -   8
  34   -  32   -   -   7  18   5
   1  12  35  24  27   4   9   -
   -   -   2  11   -  25   -   -
   -   -   -  26   3  10   -   -
```




###  Extra Credit 


The Holy_Knight program can immediately be used to tackle "more interesting" problems, such as those from [http://www.archimedes-lab.org/knight_tour.html New Knight's Tour Puzzles and Graphs]. Here is one sample solution:


```txt
>holy_knight 13 < problem10.txt
Start Configuration (Length: 56):
-----1-0-----
-----0-0-----
----00000----
-----000-----
--0--0-0--0--
00000---00000
--00-----00--
00000---00000
--0--0-0--0--
-----000-----
----00000----
-----0-0-----
-----0-0-----

Tour:
   -   -   -   -   -   1   -  27   -   -   -   -   -
   -   -   -   -   -  56   -   2   -   -   -   -   -
   -   -   -   -  24   3  28  55  26   -   -   -   -
   -   -   -   -   -  54  25   4   -   -   -   -   -
   -   -  50   -   -  23   -  29   -   -   6   -   -
  51  20  47  22  53   -   -   -   5  30   9  32   7
   -   -  52  49   -   -   -   -   -  33  36   -   -
  19  48  21  46  17   -   -   -  37  10  31   8  35
   -   -  18   -   -  45   -  11   -   -  34   -   -
   -   -   -   -   -  16  41  38   -   -   -   -   -
   -   -   -   -  42  39  44  15  12   -   -   -   -
   -   -   -   -   -  14   -  40   -   -   -   -   -
   -   -   -   -   -  43   -  13   -   -   -   -   -
```



## ALGOL 68

Uses a modified version of the [[Knight's Tour#ALGOL 68]] solution.

```algol68
# directions for moves #
INT nne = 1, ne  = 2, se = 3, sse = 4;
INT ssw = 5, sw  = 6, nw = 7, nnw = 8;

INT lowest move  = nne;
INT highest move = nnw;

# the vertical position changes of the moves                             #
[]INT offset v = ( -2, -1,  1,  2,  2,  1, -1, -2 );
# the horizontal position changes of the moves                           #
[]INT offset h = (  1,  2,  2,  1, -1, -2, -2, -1 );

MODE SQUARE = STRUCT( INT move      # the number of the move that caused #
                                    # the knight to reach this square    #
                    , INT direction # the direction of the move that     #
                                    # brought the knight here - one of   #
                                    # nne, ne, se, sse, ssw, sw, nw or   #
                                    # nnw                                #
                    );
# get the size of the board - must be between 4 and 8                    #
INT board size = 8;
# the board #
[ board size, board size ]SQUARE board;
# starting position #
INT start row := 1;
INT start col := 1;
# the tour will be complete when we have made as many moves              #
# as there are free squares in the initial board                         #
INT  final move    := 0;

# initialise the board setting the free squares from the supplied pttern #
# the pattern has the rows in revers order                               #
PROC initialise board = ( []STRING pattern )VOID:
     BEGIN
        INT pattern row := UPB board;
        FOR row FROM 1 LWB board TO 1 UPB board
        DO
            FOR col FROM 2 LWB board TO 2 UPB board
            DO
                IF pattern[ pattern row ][ col ] = "-"
                THEN
                    # can't use this square                              #
                    board[ row, col ] := ( -1, -1 )
                ELSE
                    # available square                                   #
                    board[ row, col ] := ( 0, 0 );
                    final move +:= 1;
                    IF pattern[ pattern row ][ col ] = "1"
                    THEN
                        # have the start position                        #
                        start row := row;
                        start col := col
                    FI
                FI
            OD;
            pattern row -:= 1
        OD
     END; # initialise board #
# statistics #
INT iterations := 0;
INT backtracks := 0;

# prints the board #
PROC print tour = VOID:
BEGIN
    # format "number" into at least two characters #
    PROC n2 = ( INT number )STRING:
        IF   number < 0
        THEN
            " -"
        ELIF number < 10 AND number >= 0
        THEN
            " " + whole( number, 0 )
        ELSE
            whole( number, 0 )
        FI; # n2 #
    print( ( "     a  b  c  d  e  f  g  h", newline ) );
    print( ( "   ________________________", newline ) );
    FOR row FROM 1 UPB board BY -1 TO 1 LWB board
    DO
        print( ( n2( row ) ) );
        print( ( "|" ) );

        FOR col FROM 2 LWB board TO 2 UPB board
        DO
            print( ( " " ) );
            print( ( n2( move OF board[ row, col ] ) ) )
        OD;
        print( ( newline ) )
    OD
END; # print tour #

# update the board to the first knight's tour found starting from       #
# "start row" and "start col".                                          #
# return TRUE if one was found, FALSE otherwise                         #
PROC find tour = BOOL:
BEGIN

    BOOL result       := TRUE;
    INT  move number  := 1;
    INT  row          := start row;
    INT  col          := start col;
    INT  direction    := lowest move - 1;
    # the first move is to place the knight on the starting square #
    board[ row, col ] := ( move number, lowest move - 1 );
    # attempt to find a sequence of moves that will reach each square once #
    WHILE
        move number < final move AND result
    DO
        IF direction < highest move
        THEN
            # try the next move from this position #
            direction +:= 1;
            INT new row = row + offset v[ direction ];
            INT new col = col + offset h[ direction ];
            IF  new row <= 1 UPB board
            AND new row >= 1 LWB board
            AND new col <= 2 UPB board
            AND new col >= 2 LWB board
            THEN
                # the move is legal, check the new square is unused #
                IF move OF board[ new row, new col ] = 0
                THEN
                    # can move here #
                    iterations       +:= 1;
                    row               := new row;
                    col               := new col;
                    move number      +:= 1;
                    board[ row, col ] := ( move number, direction );
                    direction         := lowest move - 1
                FI
            FI
        ELSE
            # no more moves from this position - backtrack #
            IF move number = 1
            THEN
                # at the starting position - no solution #
                result := FALSE
            ELSE
                # not at the starting position - undo the latest move #
                backtracks  +:= 1;
                move number -:= 1;
                INT curr row := row;
                INT curr col := col;
                row -:= offset v[ direction OF board[ curr row, curr col ] ];
                col -:= offset h[ direction OF board[ curr row, curr col ] ];
                # determine which direction to try next #
                direction := direction OF board[ curr row, curr col ];
                # reset the square we just backtracked from #
                board[ curr row, curr col ] := ( 0, 0 )
            FI
        FI
    OD;
    result
END; # find tour #

main:(
    initialise board( ( "-000----"
                      , "-0-00---"
                      , "-0000000"
                      , "000--0-0"
                      , "0-0--000"
                      , "1000000-"
                      , "--00-0--"
                      , "---000--"
                      )
                    );
    IF find tour
    THEN
        # found a solution #
        print tour
    ELSE
        # couldn't find a solution #
        print( ( "Solution not found", newline ) )
    FI;
    print( ( iterations, " iterations, ", backtracks, " backtracks", newline ) )
)
```

{{out}}

```txt

     a  b  c  d  e  f  g  h
   ________________________
 8|  - 21 34 25  -  -  -  -
 7|  - 24  - 20  7  -  -  -
 6|  - 35 22 33 26 11  6  9
 5| 23 32 19  -  -  8  - 12
 4| 36  - 16  -  - 27 10  5
 3|  1 18 31 28 15  4 13  -
 2|  -  -  2 17  - 29  -  -
 1|  -  -  - 30  3 14  -  -
    +578929 iterations,     +578894 backtracks

```



## Bracmat

This solution can handle different input formats: the widths of the first and the other columns are computed. The cell were to start from should have a unique value, but this value is not prescribed. Non-empty cells (such as the start cell) should contain a character that is different from '-', '.' or white space. 
The puzzle solver itself is only a few lines long.

```bracmat
( ( Holy-Knight
  =     begin colWidth crumbs non-empty pairs path parseLine
      , display isolateStartCell minDistance numberElementsAndSort
      , parseBoard reverseList rightAlign solve strlen
    .   "'non-empty' is a pattern that is used several times in bigger patterns."
      & ( non-empty
        = 
        =   %@
          : ~( "."
             | "-"
             | " "
             | \t
             | \r
             | \n
             )
        )
      & ( reverseList
        =   a L
          .   :?L
            & whl'(!arg:%?a ?arg&!a !L:?L)
            & !L
        )
      & (strlen=e.@(!arg:? [?e)&!e)
      & ( rightAlign
        =   string width
          .   !arg:(?width,?string)
            & !width+-1*strlen$!string:?width
            &   whl
              ' ( !width+-1:~<0:?width
                & " " !string:?string
                )
            & str$!string
        )
      & ( minDistance
        =   board pat1 pat2 minWidth pos1 pos2 pattern
          .   !arg:(?board,(=?pat1),(=?pat2))
            & -1:?minWidth
            & "Construct a pattern using a template.
            The pattern finds the smallest distance between any two columns in the input.
            Assumption: all columns have the same width and columns are separated by one or
            more spaces. The function can also be used to find the width of the first column
            by letting pat1 match a new line."
            &     
                ' ( ?
                    (   $pat1
                        [?pos1
                        (? " "|`)
                        ()$pat2
                        [?pos2
                        ?
                    &   !pos2+-1*!pos1
                      : ( <!minWidth
                        | ?&!minWidth:<0
                        )
                      : ?minWidth
                    & ~
                    )
                  )
              : (=?pattern)
            & "'pattern', by design, always fails. The interesting part is a side effect: 
               the column width."
            & (@(!board:!pattern)|!minWidth)
        )
      & ( numberElementsAndSort
        =   a sum n
          .   0:?sum:?n
            & "An evaluated sum is always sorted. The terms are structured so the sorting
               order is by row and then by column (both part of 'a')."
            &   whl
              ' ( !arg:%?a ?arg
                & 1+!n:?n
                & (!a,!n)+!sum:?sum
                )
            & "return the sorted list (sum) and also the size of a field that can contain
               the highest number."
            & (!sum.strlen$!n+1)
        )
      & ( parseLine
        =     line row columnWidth width col
            , bins val A M Z cell validPat
          .   !arg:(?line,?row,?width,?columnWidth,?bins)
            & 0:?col
            & "Find the cells and create a pair [row,col] for each. Put each pair in a bin.
               There are as many bins as there are different values in cells."
            &   '(? ($!non-empty:?val) ?)
              : (=?validPat)
            &   whl
              ' ( @(!line:?cell [!width ?line)
                & (   @(!cell:!validPat)
                    &   (   !bins:?A (!val.?M) ?Z
                          & !A (!val.(!row.!col) !M) !Z
                        | (!val.!row.!col) !bins
                        )
                      : ?bins
                  | 
                  )
                & !columnWidth:?width
                & 1+!col:?col
                )
            & !bins
        )
      & ( parseBoard
        =   board firstColumnWidth columnWidth,row bins line
          .   !arg:?board
            &   (   minDistance
                  $ (str$(\r \n !arg),(=\n),!non-empty)
                , minDistance$(!arg,!non-empty,!non-empty)
                )
              : (?firstColumnWidth,?columnWidth)
            & 0:?row
            & :?bins
            &   whl
              ' ( @(!board:?line \n ?board)
                &     parseLine
                    $ (!line,!row,!firstColumnWidth,!columnWidth,!bins)
                  : ?bins
                & (!bins:|1+!row:?row)
                )
            &     parseLine
                $ (!board,!row,!firstColumnWidth,!columnWidth,!bins)
              : ?bins
        )
      & "Find the first bin with only one pair. Return this pair and the combined pairs in
         all remaining bins."
      & ( isolateStartCell
        =   A begin Z valuedPairs pairs
          .   !arg:?A (?.? [1:?begin) ?Z
            & !A !Z:?arg
            & :?pairs
            &   whl
              ' ( !arg:(?.?valuedPairs) ?arg
                & !valuedPairs !pairs:?pairs
                )
            & (!begin.!pairs)
        )
      & ( display
        =   board solution row col x y n colWidth
          .   !arg:(?board,?solution,?colWidth)
            & out$!board
            & 0:?row
            & -1:?col
            &   whl
              ' ( !solution:((?y.?x),?n)+?solution
                &   whl
                  ' ( !row:<!y
                    & 1+!row:?row
                    & -1:?col
                    & put$\n
                    )
                &   whl
                  ' ( 1+!col:?col:<!x
                    & put$(rightAlign$(!colWidth,))
                    )
                & put$(rightAlign$(!colWidth,!n))
                )
            & put$\n
        )
      & ( solve
        =   A Z x y crumbs pairs X Y solution
          .   !arg:((?y.?x),?crumbs,?pairs)
            & ( !pairs:&(!y.!x) !crumbs
              |     !pairs
                  :   ?A
                      ( (?Y.?X) ?Z
                      &   (!x+-1*!X)*(!y+-1*!Y)
                        : (2|-2)
                      &     solve
                          $ ( (!Y.!X)
                            , (!y.!x) !crumbs
                            , !A !Z
                            )
                        : ?solution
                      )
                & !solution
              )
        )
      & ( isolateStartCell$(parseBoard$!arg):(?begin.?pairs)
        | out$"Sorry, I cannot identify a start cell."&~
        )
      & solve$(!begin,,!pairs):?crumbs
      &   numberElementsAndSort$(reverseList$!crumbs)
        : (?path.?colWidth)
      & display$(!arg,!path,!colWidth)
  )
&     "
  
      0 0 0
      0   0 0
      0 0 0 0 0 0 0
    0 0 0     0   0
    0   0     0 0 0
    1 0 0 0 0 0 0
        0 0   0
          0 0 0
          "
      "
-----1-0-----
-----0-0-----
----00000----
-----000-----
--0--0-0--0--
00000---00000
--00-----00--
00000---00000
--0--0-0--0--
-----000-----
----00000----
-----0-0-----
-----0-0-----"
  : ?boards
& whl'(!boards:%?board ?boards&Holy-Knight$!board)
& done
);
```

Output:

```txt


      0 0 0
      0   0 0
      0 0 0 0 0 0 0
    0 0 0     0   0
    0   0     0 0 0
    1 0 0 0 0 0 0
        0 0   0
          0 0 0

    21 30 19
    36    22 29
    31 20 35 18 23 28 25
 15 34 17       26     8
 32    14        9 24 27
  1 16 33 10 13  4  7
        2  5    11
          12  3  6

-----1-0-----
-----0-0-----
----00000----
-----000-----
--0--0-0--0--
00000---00000
--00-----00--
00000---00000
--0--0-0--0--
-----000-----
----00000----
-----0-0-----
-----0-0-----
                 1    27
                26    56
             30 55  2 25 28
                24 29 54
       36       31     3       50
 37 34 39 32 23          53  4 47  6 51
       22 35                49 52
 21 38 33 40 19           9 46  5 48  7
       20       41    45        8
                18 43 10
             42 11 14 17 44
                16    12
                13    15
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
	dx[0] = -1; dy[0] = -2; dx[1] = -1; dy[1] =  2;
	dx[2] =  1; dy[2] = -2; dx[3] =  1; dy[3] =  2;
	dx[4] = -2; dy[4] = -1; dx[5] = -2; dy[5] =  1; 
	dx[6] =  2; dy[6] = -1; dx[7] =  2; dy[7] =  1;
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
	z = 99999;
	for( int b = 0; b < hei; b++ )
	    for( int a = 0; a < wid; a++ )
		if( arr[a + wid * b].val > 0 && arr[a + wid * b].val < z ) 
		{ 
		    x = a; y = b;
		    z = arr[a + wid * b].val;
		}

    }

    int wid, hei, max, dx[8], dy[8];
    node* arr;
};

int main( int argc, char* argv[] )
{
    int wid; string p;
    //p = "* . . . * * * * * . * . . * * * * . . . . . . . . . . * * . * . . * . * * . . . 1 . . . . . . * * * . . * . * * * * * . . . * *"; wid = 8;
    p = "* * * * * 1 * . * * * * * * * * * * . * . * * * * * * * * * . . . . . * * * * * * * * * . . . * * * * * * * . * * . * . * * . * * . . . . . * * * . . . . . * * . . * * * * * . . * * . . . . . * * * . . . . . * * . * * . * . * * . * * * * * * * . . . * * * * * * * * * . . . . . * * * * * * * * * . * . * * * * * * * * * * . * . * * * * * "; wid = 13;
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


{{out}}

```txt

   17 14 29
   28    18 15
   13 16 27 30 19 32 07
25 02 11       06    20
12    26       31 08 33
01 24 03 10 05 34 21
      36 23    09
         04 35 22

              01    05
              10    12
           02 13 04 09 06
              08 11 14
     34       03    07       16
7 30 39 28 35          15 56 49 54 51
     36 33                17 52
1 38 29 40 27          19 48 55 50 53
     32       41    47       18
              26 23 20
           42 21 44 25 46
              24    22
              43    45

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
        knightMoves = {(1,-2),(2,-1),(2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2)};

    private (int dx, int dy)[] moves;
        
    public static void Main()
    {
        var knightSolver = new Solver(knightMoves);
        Print(knightSolver.Solve(true,
            ".000....",
            ".0.00...",
            ".0000000",
            "000..0.0",
            "0.0..000",
            "1000000.",
            "..00.0..",
            "...000.."));

        Print(knightSolver.Solve(true,
            ".....0.0.....",
            ".....0.0.....",
            "....00000....",
            ".....000.....",
            "..0..0.0..0..",
            "00000...00000",
            "..00.....00..",
            "00000...00000",
            "..0..0.0..0..",
            ".....000.....",
            "....00000....",
            ".....0.0.....",
            ".....0.0....." 
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

{{out}}

```txt

-- 23 32 21 -- -- -- --
-- 16 -- 24 31 -- -- --
-- 33 22 15 20 25 30 27
17 36 19 -- -- 28 --  8
34 -- 14 -- --  9 26 29
 1 18 35 10 13  4  7 --
-- --  2  5 -- 11 -- --
-- -- -- 12  3  6 -- --

-- -- -- -- --  1 -- 37 -- -- -- -- --
-- -- -- -- -- 34 -- 56 -- -- -- -- --
-- -- -- --  2 55 38 33 36 -- -- -- --
-- -- -- -- -- 32 35 54 -- -- -- -- --
-- -- 28 -- --  3 -- 39 -- -- 48 -- --
29  6 25  4 31 -- -- -- 53 40 51 42 47
-- -- 30 27 -- -- -- -- -- 49 46 -- --
 7 26  5 24  9 -- -- -- 45 52 41 50 43
-- --  8 -- -- 23 -- 15 -- -- 44 -- --
-- -- -- -- -- 10 19 22 -- -- -- -- --
-- -- -- -- 18 21 16 11 14 -- -- -- --
-- -- -- -- -- 12 -- 20 -- -- -- -- --
-- -- -- -- -- 17 -- 13 -- -- -- -- --

```



## D

{{trans|C++}}
From the refactored C++ version with more precise typing, and some optimizations. The HolyKnightPuzzle struct is created at compile-time, so its pre-conditions can catch most malformed puzzles at compile-time. 

```d
import std.stdio, std.conv, std.string, std.range, std.algorithm,
       std.typecons, std.typetuple;


struct HolyKnightPuzzle {
    private alias InputCellBaseType = char;
    private enum InputCell : InputCellBaseType { available = '#', unavailable = '.', start='1' }
    private alias Cell = uint;
    private enum : Cell { unknownCell = 0, unavailableCell = Cell.max, startCell=1 } // Special Cell values.

    // Neighbors, [shift row, shift column].
    static struct P { int x, y; }
    alias shifts = TypeTuple!(P(-2, -1), P(2, -1), P(-2, 1), P(2, 1),
                              P(-1, -2), P(1, -2), P(-1, 2), P(1, 2));

    immutable size_t gridWidth, gridHeight;
    private immutable Cell nAvailableCells;
    private /*immutable*/ const InputCell[] flatPuzzle;
    private Cell[] grid; // Flattened mutable game grid.

    @disable this();


    this(in string[] rawPuzzle) pure @safe
    in {
        assert(!rawPuzzle.empty);
        assert(!rawPuzzle[0].empty);
        assert(rawPuzzle.all!(row => row.length == rawPuzzle[0].length)); // Is rectangular.
        assert(rawPuzzle.join.count(InputCell.start) == 1); // Exactly one start point.
    } body {
        //immutable puzzle = rawPuzzle.to!(InputCell[][]);
        immutable puzzle = rawPuzzle.map!representation.array.to!(InputCell[][]);

        gridWidth = puzzle[0].length;
        gridHeight = puzzle.length;
        flatPuzzle = puzzle.join;

        // This counts the start cell too.
        nAvailableCells = flatPuzzle.representation.count!(ic => ic != InputCell.unavailable);

        grid = flatPuzzle
               .map!(ic => ic.predSwitch(InputCell.available,   unknownCell,
                                         InputCell.unavailable, unavailableCell,
                                         InputCell.start,       startCell))
               .array;
    }


    Nullable!(string[][]) solve(size_t width)() pure /*nothrow*/ @safe
    out(result) {
        if (!result.isNull)
            assert(!grid.canFind(unknownCell));
    } body {
        assert(width == gridWidth);

        // Find start position.
        foreach (immutable r; 0 ..  gridHeight)
            foreach (immutable c; 0 .. width)
                if (grid[r * width + c] == startCell &&
                    search!width(r, c, startCell + 1)) {
                    auto result = zip(flatPuzzle, grid) // Not nothrow.
                                  //.map!({p, c} => ...
                                  .map!(pc => (pc[0] == InputCell.available) ?
                                              pc[1].text :
                                              InputCellBaseType(pc[0]).text)
                                  .array
                                  .chunks(width)
                                  .array;
                    return typeof(return)(result);
                }

        return typeof(return)();
    }


    private bool search(size_t width)(in size_t r, in size_t c, in Cell cell) pure nothrow @safe @nogc {
        if (cell > nAvailableCells)
            return true; // One solution found.

        // This doesn't use the Warnsdorff rule.
        foreach (immutable sh; shifts) {
            immutable r2 = r + sh.x,
                      c2 = c + sh.y,
                      pos = r2 * width + c2;
            // No need to test for >= 0 because uint wraps around.
            if (c2 < width && r2 < gridHeight && grid[pos] == unknownCell) {
                grid[pos] = cell;        // Try.
                if (search!width(r2, c2, cell + 1))
                    return true;
                grid[pos] = unknownCell; // Restore.
            }
        }

        return false;
    }
}


void main() @safe {
    // Enum HolyKnightPuzzle to catch malformed puzzles at compile-time.
    enum puzzle1 = ".###....
                    .#.##...
                    .#######
                    ###..#.#
                    #.#..###
                    1######.
                    ..##.#..
                    ...###..".split.HolyKnightPuzzle;

    enum puzzle2 = ".....1.#.....
                    .....#.#.....
                    ....#####....
                    .....###.....
                    ..#..#.#..#..
                    #####...#####
                    ..##.....##..
                    #####...#####
                    ..#..#.#..#..
                    .....###.....
                    ....#####....
                    .....#.#.....
                    .....#.#.....".split.HolyKnightPuzzle;

    foreach (/*enum*/ puzzle; TypeTuple!(puzzle1, puzzle2)) {
        //immutable solution = puzzle.solve!(puzzle.gridWidth);
        enum width = puzzle.gridWidth;
        immutable solution = puzzle.solve!width; // Solved at run-time.
        if (solution.isNull)
            writeln("No solution found for puzzle.\n");
        else
            writefln("One solution:\n%(%-(%2s %)\n%)\n", solution);
    }
}
```

{{out}}

```txt
One solution:
 . 17 14 29  .  .  .  .
 . 28  . 18 15  .  .  .
 . 13 16 27 30 19 32  7
25  2 11  .  .  6  . 20
12  . 26  .  . 31  8 33
 1 24  3 10  5 34 21  .
 .  . 36 23  .  9  .  .
 .  .  .  4 35 22  .  .

One solution:
 .  .  .  .  .  1  .  5  .  .  .  .  .
 .  .  .  .  . 10  . 12  .  .  .  .  .
 .  .  .  .  2 13  4  9  6  .  .  .  .
 .  .  .  .  .  8 11 14  .  .  .  .  .
 .  . 34  .  .  3  .  7  .  . 16  .  .
37 30 39 28 35  .  .  . 15 56 49 54 51
 .  . 36 33  .  .  .  .  . 17 52  .  .
31 38 29 40 27  .  .  . 19 48 55 50 53
 .  . 32  .  . 41  . 47  .  . 18  .  .
 .  .  .  .  . 26 23 20  .  .  .  .  .
 .  .  .  . 42 21 44 25 46  .  .  .  .
 .  .  .  .  . 24  . 22  .  .  .  .  .
 .  .  .  .  . 43  . 45  .  .  .  .  .
```


Run-time about 0.58 seconds with ldc2 compiler (using a switch statement if you don't have the predSwitch yet in Phobos), about 23 times faster than the Haskell entry.


## Elixir

{{trans|Ruby}}
This solution uses HLPsolver from [[Solve_a_Hidato_puzzle#Elixir | here]]

```elixir
# require HLPsolver
 
adjacent = [{-1,-2},{-2,-1},{-2,1},{-1,2},{1,2},{2,1},{2,-1},{1,-2}]
 
"""
. . 0 0 0
. . 0 . 0 0
. 0 0 0 0 0 0 0
0 0 0 . . 0 . 0
0 . 0 . . 0 0 0
1 0 0 0 0 0 0
. . 0 0 . 0
. . . 0 0 0
"""
|> HLPsolver.solve(adjacent)

"""
 _ _ _ _ _ 1 _ 0          
 _ _ _ _ _ 0 _ 0          
 _ _ _ _ 0 0 0 0 0        
 _ _ _ _ _ 0 0 0          
 _ _ 0 _ _ 0 _ 0 _ _ 0    
 0 0 0 0 0 _ _ _ 0 0 0 0 0
 _ _ 0 0 _ _ _ _ _ 0 0    
 0 0 0 0 0 _ _ _ 0 0 0 0 0
 _ _ 0 _ _ 0 _ 0 _ _ 0    
 _ _ _ _ _ 0 0 0          
 _ _ _ _ 0 0 0 0 0        
 _ _ _ _ _ 0 _ 0          
 _ _ _ _ _ 0 _ 0          
"""
|> HLPsolver.solve(adjacent)
```


{{out}}

```txt

Problem:
       0  0  0
       0     0  0
    0  0  0  0  0  0  0
 0  0  0        0     0
 0     0        0  0  0
 1  0  0  0  0  0  0
       0  0     0
          0  0  0

Solution:
      18 21 36
      13    19 22
   17 20 35 14 25  6 23
31 12 15       34    26
16    32        7 24  5
 1 30 11  8 33  4 27
       2 29     9
         10  3 28

Problem:
                1     0
                0     0
             0  0  0  0  0
                0  0  0
       0        0     0        0
 0  0  0  0  0           0  0  0  0  0
       0  0                 0  0
 0  0  0  0  0           0  0  0  0  0
       0        0     0        0
                0  0  0
             0  0  0  0  0
                0     0
                0     0

Solution:
                1    55
               34    36
             2 37 56 33 54
               32 35 38
      28        3    53       44
29  6 25  4 31          39 52 41 50 43
      30 27                45 48
 7 26  5 24  9          47 40 51 42 49
       8       23    15       46
               10 19 22
            18 21 16 11 14
               12    20
               17    13

```



## Go

{{trans|Python}}

```go
package main

import "fmt"

var moves = [][2]int{
    {-1, -2}, {1, -2}, {-1, 2}, {1, 2}, {-2, -1}, {-2, 1}, {2, -1}, {2, 1},
}

var board1 = " xxx    " +
    " x xx   " +
    " xxxxxxx" +
    "xxx  x x" +
    "x x  xxx" +
    "sxxxxxx " +
    "  xx x  " +
    "   xxx  "

var board2 = ".....s.x....." +
    ".....x.x....." +
    "....xxxxx...." +
    ".....xxx....." +
    "..x..x.x..x.." +
    "xxxxx...xxxxx" +
    "..xx.....xx.." +
    "xxxxx...xxxxx" +
    "..x..x.x..x.." +
    ".....xxx....." +
    "....xxxxx...." +
    ".....x.x....." +
    ".....x.x....."

func solve(pz [][]int, sz, sx, sy, idx, cnt int) bool {
    if idx > cnt {
        return true
    }
    for i := 0; i < len(moves); i++ {
        x := sx + moves[i][0]
        y := sy + moves[i][1]
        if (x >= 0 && x < sz) && (y >= 0 && y < sz) && pz[x][y] == 0 {
            pz[x][y] = idx
            if solve(pz, sz, x, y, idx+1, cnt) {
                return true
            }
            pz[x][y] = 0
        }
    }
    return false
}

func findSolution(b string, sz int) {
    pz := make([][]int, sz)
    for i := 0; i < sz; i++ {
        pz[i] = make([]int, sz)
        for j := 0; j < sz; j++ {
            pz[i][j] = -1
        }
    }
    var x, y, idx, cnt int
    for j := 0; j < sz; j++ {
        for i := 0; i < sz; i++ {
            switch b[idx] {
            case 'x':
                pz[i][j] = 0
                cnt++
            case 's':
                pz[i][j] = 1
                cnt++
                x, y = i, j
            }
            idx++
        }
    }

    if solve(pz, sz, x, y, 2, cnt) {
        for j := 0; j < sz; j++ {
            for i := 0; i < sz; i++ {
                if pz[i][j] != -1 {
                    fmt.Printf("%02d  ", pz[i][j])
                } else {
                    fmt.Print("--  ")
                }
            }
            fmt.Println()
        }
    } else {
        fmt.Println("Cannot solve this puzzle!")
    }
}

func main() {
    findSolution(board1, 8)
    fmt.Println()
    findSolution(board2, 13)
}
```


{{out}}

```txt

--  17  14  29  --  --  --  --  
--  28  --  18  15  --  --  --  
--  13  16  27  30  19  32  07  
25  02  11  --  --  06  --  20  
12  --  26  --  --  31  08  33  
01  24  03  10  05  34  21  --  
--  --  36  23  --  09  --  --  
--  --  --  04  35  22  --  --  

--  --  --  --  --  01  --  05  --  --  --  --  --  
--  --  --  --  --  10  --  12  --  --  --  --  --  
--  --  --  --  02  13  04  09  06  --  --  --  --  
--  --  --  --  --  08  11  14  --  --  --  --  --  
--  --  36  --  --  03  --  07  --  --  16  --  --  
35  42  33  44  37  --  --  --  15  20  27  22  25  
--  --  38  41  --  --  --  --  --  17  24  --  --  
39  34  43  32  45  --  --  --  19  28  21  26  23  
--  --  40  --  --  31  --  29  --  --  18  --  --  
--  --  --  --  --  46  51  56  --  --  --  --  --  
--  --  --  --  52  55  30  47  50  --  --  --  --  
--  --  --  --  --  48  --  54  --  --  --  --  --  
--  --  --  --  --  53  --  49  --  --  --  --  --  

```



## Haskell


```Haskell
import Data.Array
       (Array, (//), (!), assocs, elems, bounds, listArray)
import Data.Foldable (forM_)
import Data.List (intercalate, transpose)
import Data.Maybe

type Position = (Int, Int)

type KnightBoard = Array Position (Maybe Int)

toSlot :: Char -> Maybe Int
toSlot '0' = Just 0
toSlot '1' = Just 1
toSlot _ = Nothing

toString :: Maybe Int -> String
toString Nothing = replicate 3 ' '
toString (Just n) = replicate (3 - length nn) ' ' ++ nn
  where
    nn = show n

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

showBoard :: KnightBoard -> String
showBoard board =
  intercalate "\n" . map concat . transpose . chunksOf (height + 1) . map toString $
  elems board
  where
    (_, (_, height)) = bounds board

toBoard :: [String] -> KnightBoard
toBoard strs = board
  where
    height = length strs
    width = minimum (length <$> strs)
    board =
      listArray ((0, 0), (width - 1, height - 1)) . map toSlot . concat . transpose $
      take width <$> strs

add
  :: Num a
  => (a, a) -> (a, a) -> (a, a)
add (a, b) (x, y) = (a + x, b + y)

within
  :: Ord a
  => ((a, a), (a, a)) -> (a, a) -> Bool
within ((a, b), (c, d)) (x, y) = a <= x && x <= c && b <= y && y <= d

-- Enumerate valid moves given a board and a knight's position.
validMoves :: KnightBoard -> Position -> [Position]
validMoves board position = filter isValid plausible
  where
    bound = bounds board
    plausible =
      add position <$>
      [(1, 2), (2, 1), (2, -1), (-1, 2), (-2, 1), (1, -2), (-1, -2), (-2, -1)]
    isValid pos = within bound pos && maybe False (== 0) (board ! pos)

isSolved :: KnightBoard -> Bool
isSolved = all (maybe True (0 /=))

-- Solve the knight's tour with a simple Depth First Search.
solveKnightTour :: KnightBoard -> Maybe KnightBoard
solveKnightTour board = solve board 1 initPosition
  where
    initPosition = fst $ head $ filter ((== Just 1) . snd) $ assocs board
    solve boardA depth position =
      let boardB = boardA // [(position, Just depth)]
      in if isSolved boardB
           then Just boardB
           else listToMaybe $
                mapMaybe (solve boardB $ depth + 1) $ validMoves boardB position

tourExA :: [String]
tourExA =
  [ " 000    "
  , " 0 00   "
  , " 0000000"
  , "000  0 0"
  , "0 0  000"
  , "1000000 "
  , "  00 0  "
  , "   000  "
  ]

tourExB :: [String]
tourExB =
  [ "-----1-0-----"
  , "-----0-0-----"
  , "----00000----"
  , "-----000-----"
  , "--0--0-0--0--"
  , "00000---00000"
  , "--00-----00--"
  , "00000---00000"
  , "--0--0-0--0--"
  , "-----000-----"
  , "----00000----"
  , "-----0-0-----"
  , "-----0-0-----"
  ]

main :: IO ()
main =
  forM_
    [tourExA, tourExB]
    (\board ->
        case solveKnightTour $ toBoard board of
          Nothing -> putStrLn "No solution.\n"
          Just solution -> putStrLn $ showBoard solution ++ "\n")
```

{{out}}

```txt
    19 26 17            
    36    20 25         
    31 18 27 16 21  6 23
 35 28 15       24     8
 30    32        7 22  5
  1 34 29 14 11  4  9   
        2 33    13      
          12  3 10      

                 1    31               
                32    28               
             56 27  2 33 30            
                34 29 26               
       48       55     3       24      
 47 52 45 54 35          25  4 11  6 23
       36 49                 9 22      
 51 46 53 44 37          21 12  5 10  7
       50       43    13        8      
                38 41 20               
             42 19 16 39 14            
                40    18               
                17    15               
```

As requested, in an attempt to make this solution faster, the following is a version that replaces the Array with an STUArray (unboxed and mutable), and yields a speedup of 4.2. No speedups were gained until move validation was inlined with the logic in `solve'. This seems to point to the list consing as the overhead for time and allocation, although profiling did show that about 25% of the time in the immutable version was spent creating arrays. Perhaps a more experienced Haskeller could provide insight on how to further optimize this or what optimizations were frivolous (barring a different algorithm or search heuristic, and jumping into C, unless those are the only way).

```Haskell
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (forM_)

import qualified Data.Array.Unboxed as AU

import Control.Monad.ST (ST, runST)

import Data.Array.Base (unsafeFreeze)

import Data.List (intercalate, transpose)

import Data.Array.ST
       (STUArray, readArray, writeArray, newListArray)

type Position = (Int, Int)

type KnightBoard = AU.UArray Position Int

toSlot :: Char -> Int
toSlot '0' = 0
toSlot '1' = 1
toSlot _ = -1

toString :: Int -> String
toString (-1) = replicate 3 ' '
toString n = replicate (3 - length nn) ' ' ++ nn
  where
    nn = show n

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = uncurry ((. chunksOf n) . (:)) (splitAt n xs)

showBoard :: KnightBoard -> String
showBoard board =
  intercalate "\n" . map concat . transpose . chunksOf (height + 1) . map toString $
  AU.elems board
  where
    (_, (_, height)) = AU.bounds board

toBoard :: [String] -> KnightBoard
toBoard strs = board
  where
    height = length strs
    width = minimum (length <$> strs)
    board =
      AU.listArray ((0, 0), (width - 1, height - 1)) . map toSlot . concat . transpose $
      take width <$> strs

add
  :: Num a
  => (a, a) -> (a, a) -> (a, a)
add (a, b) (x, y) = (a + x, b + y)

within
  :: Ord a
  => ((a, a), (a, a)) -> (a, a) -> Bool
within ((a, b), (c, d)) (x, y) = a <= x && x <= c && b <= y && y <= d

-- Solve the knight's tour with a simple Depth First Search.
solveKnightTour :: KnightBoard -> Maybe KnightBoard
solveKnightTour board =
  runST $
  do let assocs = AU.assocs board
         bounds = AU.bounds board
     array <-
       newListArray bounds (AU.elems board) :: ST s (STUArray s Position Int)
     let initPosition = fst $ head $ filter ((== 1) . snd) assocs
         maxDepth = fromIntegral $ 1 + length (filter ((== 0) . snd) assocs)
         offsets =
           [ (1, 2)
           , (2, 1)
           , (2, -1)
           , (-1, 2)
           , (-2, 1)
           , (1, -2)
           , (-1, -2)
           , (-2, -1)
           ]
         solve depth position =
           if within bounds position
             then do
               oldValue <- readArray array position
               if oldValue == 0
                 then do
                   writeArray array position depth
                   if depth == maxDepth
                     then return True
                          -- This mapM-any combo can be reduced to a string of ||'s
                          -- with the goal of removing the allocation overhead due to consing
                          -- which the compiler may not be able to optimize out.
                     else do
                       results <- mapM (solve (depth + 1) . add position) offsets
                       if or results
                         then return True
                         else do
                           writeArray array position oldValue
                           return False
                 else return False
             else return False
     writeArray array initPosition 0
     result <- solve 1 initPosition
     farray <- unsafeFreeze array
     return $
       if result
         then Just farray
         else Nothing

tourExA :: [String]
tourExA =
  [ " 000    "
  , " 0 00   "
  , " 0000000"
  , "000  0 0"
  , "0 0  000"
  , "1000000 "
  , "  00 0  "
  , "   000  "
  ]

tourExB :: [String]
tourExB =
  [ "-----1-0-----"
  , "-----0-0-----"
  , "----00000----"
  , "-----000-----"
  , "--0--0-0--0--"
  , "00000---00000"
  , "--00-----00--"
  , "00000---00000"
  , "--0--0-0--0--"
  , "-----000-----"
  , "----00000----"
  , "-----0-0-----"
  , "-----0-0-----"
  ]

main :: IO ()
main =
  forM_
    [tourExA, tourExB]
    (\board ->
        case solveKnightTour $ toBoard board of
          Nothing -> putStrLn "No solution.\n"
          Just solution -> putStrLn $ showBoard solution ++ "\n")
```


This version is similar to the previous one but:
* the working code is cleaned up slightly with minor optimisations here and there
* only valid board fields are taken into consideration: previously a huge amount of time was wasted on constantly verifying if moves were valid rather than building only valid moves to start with
* vector is used instead of array to take advantage of any fusion

This results in 117x speedup over the very first version. This speed up comes from a smarter traversal rather than from minor code optimisations.


```Haskell

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-stg -O2 -fforce-recomp #-}
module Main (main) where

import           Control.Monad.ST (runST)
import           Data.List (intercalate, transpose)
import qualified Data.Ix as Ix
import qualified Data.Vector as V
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           Data.Foldable (for_)

type Position = ( Int, Int )

type Bounds = (Position, Position)

type KnightBoard = (Bounds, Vector Int)

toSlot :: Char -> Int
toSlot '0' = 0
toSlot '1' = 1
toSlot _ = -1

toString :: Int -> String
toString (-1) = replicate 3 ' '
toString n = replicate (3 - length nn) ' ' ++ nn
 where
   nn = show n

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = uncurry ((. chunksOf n) . (:)) (splitAt n xs)

showBoard :: KnightBoard -> String
showBoard (bounds, board) =
 intercalate "\n" . map concat . transpose . chunksOf (height + 1) . map toString $
   U.toList board
 where
   (_, (_, height)) = bounds

toBoard :: [String] -> KnightBoard
toBoard strs = (((0,0),(width-1,height-1)), board)
 where
   height = length strs
   width = minimum (length <$> strs)
   board =
     U.fromListN (width*height) . map toSlot . concat . transpose $
     take width <$> strs

-- Solve the knight's tour with a simple Depth First Search.
solveKnightTour :: KnightBoard -> Maybe KnightBoard
solveKnightTour (bounds@(_,(_,yb)), board) = runST $ do
 array <- U.thaw board
 let maxDepth = U.length $ U.filter (/= (-1)) board
     Just iniIdx = U.findIndex (==1) board
     initPosition = mkPos iniIdx
     !hops = V.generate  (U.length board) $ \i ->
       if board `U.unsafeIndex` i == -1
       then U.empty
       else mkHops (mkPos i)

     solve !depth !position = MU.unsafeRead array position >>= \case
       0 -> do
         MU.unsafeWrite array position depth
         case depth == maxDepth of
           True -> return True
           False -> do
             results <- U.mapM (solve (depth + 1)) (hops `V.unsafeIndex` position)
             if U.or results
               then return True
               else do
                 MU.unsafeWrite array position 0
                 return False
       _ -> pure False

 MU.unsafeWrite array (Ix.index bounds initPosition) 0
 result <- solve 1 (Ix.index bounds initPosition)
 farray <- U.unsafeFreeze array
 return $ if result then Just (bounds, farray) else Nothing
 where
   offsets = U.fromListN 8 [ (1, 2), (2, 1), (2, -1), (-1, 2), (-2, 1), (1, -2), (-1, -2), (-2, -1) ]
   mkHops pos = U.filter (\i -> board `U.unsafeIndex` i == 0)
              $ U.map (Ix.index bounds)
              $ U.filter (Ix.inRange bounds)
              $ U.map (add pos) offsets
   add (x, y) (x', y') = (x + x', y + y')
   mkPos idx = idx `quotRem` (yb+1)


tourExA :: [String]
tourExA =
 [ " 000    "
 , " 0 00   "
 , " 0000000"
 , "000  0 0"
 , "0 0  000"
 , "1000000 "
 , "  00 0  "
 , "   000  "
 ]

tourExB :: [String]
tourExB =
 [ "-----1-0-----"
 , "-----0-0-----"
 , "----00000----"
 , "-----000-----"
 , "--0--0-0--0--"
 , "00000---00000"
 , "--00-----00--"
 , "00000---00000"
 , "--0--0-0--0--"
 , "-----000-----"
 , "----00000----"
 , "-----0-0-----"
 , "-----0-0-----"
 ]

main :: IO ()
main =
 for_
   [tourExA, tourExB]
   (\board -> do
       case solveKnightTour $ toBoard board of
         Nothing -> putStrLn "No solution.\n"
         Just solution -> putStrLn $ showBoard solution ++ "\n")


```


==Icon and {{header|Unicon}}==
This is a Unicon-specific solution:

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
    QMouse(puzzle, visit(loc.r-2,loc.c-1), self, val)
    QMouse(puzzle, visit(loc.r-2,loc.c+1), self, val)
    QMouse(puzzle, visit(loc.r-1,loc.c+2), self, val)
    QMouse(puzzle, visit(loc.r+1,loc.c+2), self, val)
    QMouse(puzzle, visit(loc.r+2,loc.c+1), self, val)
    QMouse(puzzle, visit(loc.r+2,loc.c-1), self, val)
    QMouse(puzzle, visit(loc.r+1,loc.c-2), self, val)
    QMouse(puzzle, visit(loc.r-1,loc.c-2), self, val)
end
```


Sample run:

```txt
->hkt <hkt.in
Input with 36 cells:
                                    
                                    
           _  _  _                  
           _     _  _               
           _  _  _  _  _  _  _      
        _  _  _        _     _      
        _     _        _  _  _      
        1  _  _  _  _  _  _         
              _  _     _            
                 _  _  _            
                                    
                                    
Output with 36 cells:
                                    
                                    
          19  4 13                  
          12    18  5               
          25 20  3 14 17  6 31      
       21  2 11       32    16      
       26    24       15 30  7      
        1 22 27 10 35  8 33         
             36 23    29            
                28  9 34            
                                    
                                    
->

```


## J


The simplest J implementation here uses a breadth first search - but that can be memory inefficient so it's worth representing the boards as characters (several orders of magnitude space improvement) and it's worth capping how much memory we allow J to use (2^34 is 16GB):


```J
9!:21]2^34

unpack=:verb define
  mask=. +./' '~:y
  board=. (255 0 1{a.) {~ {.@:>:@:"."0 mask#"1 y
)

ex1=:unpack ];._2]0 :0
  0 0 0 
  0   0 0 
  0 0 0 0 0 0 0
0 0 0     0   0
0   0     0 0 0
1 0 0 0 0 0 0
    0 0   0
      0 0 0
)

solve=:verb define
  board=.,:y
  for_move.1+i.+/({.a.)=,y do.
    board=. ;move <@knight"2 board
  end.
)

kmoves=: ,/(2 1,:1 2)*"1/_1^#:i.4

knight=:dyad define
  pos=. ($y)#:(,y)i.x{a.
  moves=. <"1(#~ 0&<: */"1@:* ($y)&>"1)pos+"1 kmoves
  moves=. (#~ (0{a.)={&y) moves
  moves y adverb def (':';'y x} m')"0 (x+1){a.
)
```


Letting that cook:


```J
   $~.sol
48422 8 8
```


That's 48422 solutions. Here's one of them:


```J
   (a.i.{.sol){(i.255),__
__ 11 28 13 __ __ __ __
__ 22 __ 10 29 __ __ __
__ 27 12 21 14  9 16 31
23  2 25 __ __ 30 __  8
26 __ 20 __ __ 15 32 17
 1 24  3 34  5 18  7 __
__ __ 36 19 __ 33 __ __
__ __ __  4 35  6 __ __
```


and here's a couple more:


```J
   (a.i.{:sol){(i.255),__
__  5  8 31 __ __ __ __
__ 32 __  6  9 __ __ __
__  7  4 33 30 23 10 21
 3 34 29 __ __ 20 __ 24
36 __  2 __ __ 11 22 19
 1 28 35 12 15 18 25 __
__ __ 16 27 __ 13 __ __
__ __ __ 14 17 26 __ __
   (a.i.24211{sol){(i.255),__
__ 11 14 33 __ __ __ __
__ 34 __ 10 13 __ __ __
__ 19 12 15 32  9  6 25
35 16 31 __ __ 24 __  8
18 __ 20 __ __  7 26  5
 1 36 17 30 27  4 23 __
__ __  2 21 __ 29 __ __
__ __ __ 28  3 22 __ __
```


This is something of a problem, however, because finding all those solutions is slow. And even having to be concerned about a 16GB memory limit for this small of a problem is troubling (and using 64 bit integers, instead of 8 bit characters, to represent board squares, would exceed that limit). Also, you'd get bored, inspecting 48422 boards.

So, let's just find one solution:


```J
unpack=:verb define
  mask=. +./' '~:y
  board=. __ 0 1 {~ {.@:>:@:"."0 mask#"1 y
)

ex1=:unpack ];._2]0 :0
  0 0 0 
  0   0 0 
  0 0 0 0 0 0 0
0 0 0     0   0
0   0     0 0 0
1 0 0 0 0 0 0
    0 0   0
      0 0 0
)

solve1=:verb define
 (1,+/0=,y) solve1 ,:y
:
  for_block._10 <\ y do.
    board=. ;({.x) <@knight"2 ;block
    if. #board do.
      if. =/x do.
        {.board return.
      else.
        board=. (1 0+x) solve1 board
        if. #board do.
          board return.
        end.
      end.
    end.
  end.
  i.0 0
)

kmoves=: ,/(2 1,:1 2)*"1/_1^#:i.4

knight=:dyad define
  pos=. ($y)#:(,y)i.x
  moves=. <"1(#~ 0&<: */"1@:* ($y)&>"1)pos+"1 kmoves
  moves=. (#~ 0={&y) moves
  moves y adverb def (':';'y x} m')"0 x+1
)
```


Here, we break our problem space up into blocks of no more than 10 boards each, and use recursion to investigate each batch of boards. When we find a solution, we stop there (for each iteration at each level of recursion):


```J
   solve1 ex1
__ 11 28 13 __ __ __ __
__ 22 __ 10 29 __ __ __
__ 27 12 21 14  9 16 31
23  2 25 __ __ 30 __  8
26 __ 20 __ __ 15 32 17
 1 24  3 34  5 18  7 __
__ __ 36 19 __ 33 __ __
__ __ __  4 35  6 __ __
```


[Why ten boards and not just one board? Because 10 is a nice compromise between amortizing the overhead of each attempt and not trying too much at one time. Most individual attempts will fail, but by splitting up the workload after exceeding 10 possibilities, instead of investigating each possibility individually, we increase the chances that we are investigating something useful. Also, J implementations penalize the performance of algorithms which are overly serial in structure.]

With this tool in hand, we can now attempt bigger problems:


```J
ex2=:unpack ];._2]0 :0
           1   0          
           0   0          
         0 0 0 0 0        
           0 0 0          
     0     0   0     0    
 0 0 0 0 0       0 0 0 0 0
     0 0           0 0    
 0 0 0 0 0       0 0 0 0 0
     0     0   0     0    
           0 0 0          
         0 0 0 0 0        
           0   0          
           0   0          
)
```


Finding a solution for this looks like:


```J
   solve1 ex2
__ __ __ __ __  1 __  5 __ __ __ __ __
__ __ __ __ __  6 __ 46 __ __ __ __ __
__ __ __ __ 48 45  2  7  4 __ __ __ __
__ __ __ __ __  8 47 44 __ __ __ __ __
__ __ 56 __ __ 49 __  3 __ __ 42 __ __
13 52 11 50  9 __ __ __ 43 38 31 36 33
__ __ 14 55 __ __ __ __ __ 41 34 __ __
53 12 51 10 15 __ __ __ 39 30 37 32 35
__ __ 54 __ __ 23 __ 29 __ __ 40 __ __
__ __ __ __ __ 16 19 22 __ __ __ __ __
__ __ __ __ 24 21 26 17 28 __ __ __ __
__ __ __ __ __ 18 __ 20 __ __ __ __ __
__ __ __ __ __ 25 __ 27 __ __ __ __ __
```



## Java

{{works with|Java|8}}

```java
import java.util.*;

public class HolyKnightsTour {

    final static String[] board = {
        " xxx    ",
        " x xx   ",
        " xxxxxxx",
        "xxx  x x",
        "x x  xxx",
        "1xxxxxx ",
        "  xx x  ",
        "   xxx  "};

    private final static int base = 12;
    private final static int[][] moves = {{1, -2}, {2, -1}, {2, 1}, {1, 2},
    {-1, 2}, {-2, 1}, {-2, -1}, {-1, -2}};
    private static int[][] grid;
    private static int total = 2;

    public static void main(String[] args) {
        int row = 0, col = 0;

        grid = new int[base][base];

        for (int r = 0; r < base; r++) {
            Arrays.fill(grid[r], -1);
            for (int c = 2; c < base - 2; c++) {
                if (r >= 2 && r < base - 2) {
                    if (board[r - 2].charAt(c - 2) == 'x') {
                        grid[r][c] = 0;
                        total++;
                    }
                    if (board[r - 2].charAt(c - 2) == '1') {
                        row = r;
                        col = c;
                    }
                }
            }
        }

        grid[row][col] = 1;

        if (solve(row, col, 2))
            printResult();
    }

    private static boolean solve(int r, int c, int count) {
        if (count == total)
            return true;

        List<int[]> nbrs = neighbors(r, c);

        if (nbrs.isEmpty() && count != total)
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

    private static List<int[]> neighbors(int r, int c) {
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

    private static int countNeighbors(int r, int c) {
        int num = 0;
        for (int[] m : moves)
            if (grid[r + m[1]][c + m[0]] == 0)
                num++;
        return num;
    }

    private static void printResult() {
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
         19 26 21                   
         28    18 25                
         33 20 27 22 17 24  7       
      29  2 35        8    16       
      34    32       23  6  9       
       1 30  3 36 13 10 15          
            12 31     5             
                4 11 14             
```



## JavaScript


### ES6

By composition of generic functions, cacheing degree-sorted moves for each node.

```JavaScript
(() => {
    'use strict';

    // problems :: [[String]]
    const problems = [
        [
              " 000    " //
            , " 0 00   " //
            , " 0000000" //
            , "000  0 0" //
            , "0 0  000" //
            , "1000000 " //
            , "  00 0  " //
            , "   000  " //
        ],
        [
              "-----1-0-----" //
            , "-----0-0-----" //
            , "----00000----" //
            , "-----000-----" //
            , "--0--0-0--0--" //
            , "00000---00000" //
            , "--00-----00--" //
            , "00000---00000" //
            , "--0--0-0--0--" //
            , "-----000-----" //
            , "----00000----" //
            , "-----0-0-----" //
            , "-----0-0-----" //
        ]
    ];

    // GENERIC FUNCTIONS ------------------------------------------------------

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : a > b ? 1 : 0
        };

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // charColRow :: Char -> [String] -> Maybe (Int, Int)
    const charColRow = (c, rows) =>
        foldr((a, xs, iRow) =>
            a.nothing ? (() => {
                const mbiCol = elemIndex(c, xs);
                return mbiCol.nothing ? mbiCol : {
                    just: [mbiCol.just, iRow],
                    nothing: false
                };
            })() : a, {
                nothing: true
            }, rows);

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat(Array.from(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.indexOf(x) !== -1;

    // elemIndex :: Eq a => a -> [a] -> Maybe Int
    const elemIndex = (x, xs) => {
        const i = xs.indexOf(x);
        return {
            nothing: i === -1,
            just: i
        };
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // findIndex :: (a -> Bool) -> [a] -> Maybe Int
    const findIndex = (f, xs) => {
        for (var i = 0, lng = xs.length; i < lng; i++) {
            if (f(xs[i])) return {
                nothing: false,
                just: i
            };
        }
        return {
            nothing: true
        };
    };

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // foldr (a -> b -> b) -> b -> [a] -> b
    const foldr = (f, a, xs) => xs.reduceRight(f, a);

    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const groupBy = (f, xs) => {
        const dct = xs.slice(1)
            .reduce((a, x) => {
                const
                    h = a.active.length > 0 ? a.active[0] : undefined,
                    blnGroup = h !== undefined && f(h, x);
                return {
                    active: blnGroup ? a.active.concat([x]) : [x],
                    sofar: blnGroup ? a.sofar : a.sofar.concat([a.active])
                };
            }, {
                active: xs.length > 0 ? [xs[0]] : [],
                sofar: []
            });
        return dct.sofar.concat(dct.active.length > 0 ? [dct.active] : []);
    };

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // intersectBy::(a - > a - > Bool) - > [a] - > [a] - > [a]
    const intersectBy = (eq, xs, ys) =>
        (xs.length > 0 && ys.length > 0) ?
        xs.filter(x => ys.some(curry(eq)(x))) : [];

    // justifyRight :: Int -> Char -> Text -> Text
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // mappendComparing :: [(a -> b)] -> (a -> a -> Ordering)
    const mappendComparing = fs => (x, y) =>
        fs.reduce((ord, f) => {
            if (ord !== 0) return ord;
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : a > b ? 1 : 0
        }, 0);

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        xs.reduce((a, x) => a === undefined ? x : (
            f(x, a) > 0 ? x : a
        ), undefined);

    // min :: Ord a => a -> a -> a
    const min = (a, b) => b < a ? b : a;

    // replicate :: Int -> a -> [a]
    const replicate = (n, a) => {
        let v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) => xs.slice()
        .sort(f);

    // splitOn :: String -> String -> [String]
    const splitOn = (s, xs) => xs.split(s);

    // take :: Int -> [a] -> [a]
    const take = (n, xs) => xs.slice(0, n);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // zip :: [a] -> [b] -> [(a,b)]
    const zip = (xs, ys) =>
        xs.slice(0, Math.min(xs.length, ys.length))
        .map((x, i) => [x, ys[i]]);

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        Array.from({
            length: min(xs.length, ys.length)
        }, (_, i) => f(xs[i], ys[i]));

    // HOLY KNIGHT's TOUR FUNCTIONS -------------------------------------------

    // kmoves :: (Int, Int) -> [(Int, Int)]
    const kmoves = ([x, y]) => map(
        ([a, b]) => [a + x, b + y], [
            [1, 2],
            [1, -2],
            [-1, 2],
            [-1, -2],
            [2, 1],
            [2, -1],
            [-2, 1],
            [-2, -1]
        ]);

    // rowPosns :: Int -> String -> [(Int, Int)]
    const rowPosns = (iRow, s) => {
        return foldl((a, x, i) => (elem(x, ['0', '1']) ? (
            a.concat([
                [i, iRow]
            ])
        ) : a), [], splitOn('', s));
    };

    // hash :: (Int, Int) -> String
    const hash = ([col, row]) => col.toString() + '.' + row.toString();

    // Start node, and degree-sorted cache of moves from each node
    // All node references are hash strings (for this cache)

    // problemModel :: [[String]] -> {cache: {nodeKey: [nodeKey], start:String}}
    const problemModel = boardLines => {
        const
            steps = foldl((a, xs, i) =>
                a.concat(rowPosns(i, xs)), [], boardLines),
            courseMoves = (xs, [x, y]) => intersectBy(
                ([a, b], [c, d]) => a === c && b === d, kmoves([x, y]), xs
            ),
            maybeStart = charColRow('1', boardLines);
        return {
            start: maybeStart.nothing ? '' : hash(maybeStart.just),
            boardWidth: boardLines.length > 0 ? boardLines[0].length : 0,
            stepCount: steps.length,
            cache: (() => {
                const moveCache = foldl((a, xy) => (
                        a[hash(xy)] = map(hash, courseMoves(steps, xy)),
                        a
                    ), {}, steps),
                    lstMoves = Object.keys(moveCache),
                    dctDegree = foldl((a, k) =>
                        (a[k] = moveCache[k].length,
                            a), {}, lstMoves);

                return foldl((a, k) => (
                    a[k] = sortBy(comparing(x => dctDegree[x]), moveCache[k]),
                    a
                ), {}, lstMoves);
            })()
        };
    };

    // firstSolution :: {nodeKey: [nodeKey]} -> Int ->
    //      nodeKey -> nodeKey -> [nodeKey] ->
    //      -> {path::[nodeKey], pathLen::Int, found::Bool}
    const firstSolution = (dctMoves, intTarget, strStart, strNodeKey, path) => {
        const
            intPath = path.length,
            moves = dctMoves[strNodeKey];

        if ((intTarget - intPath) < 2 && elem(strStart, moves)) {
            return {
                nothing: false,
                just: [strStart, strNodeKey].concat(path),
                pathLen: intTarget
            };
        }

        const
            nexts = filter(k => !elem(k, path), moves),
            intNexts = nexts.length,
            lstFullPath = [strNodeKey].concat(path);

        // Until we find a full path back to start
        return until(
            x => (x.nothing === false || x.i >= intNexts),
            x => {
                const
                    idx = x.i,
                    dctSoln = firstSolution(
                        dctMoves, intTarget, strStart, nexts[idx], lstFullPath
                    );
                return {
                    i: idx + 1,
                    nothing: dctSoln.nothing,
                    just: dctSoln.just,
                    pathLen: dctSoln.pathLen
                };
            }, {
                nothing: true,
                just: [],
                i: 0
            }
        );
    };

    // maybeTour :: [String] -> {
    //    nothing::Bool, Just::[nodeHash], i::Int: pathLen::Int }
    const maybeTour = trackLines => {
        const
            dctModel = problemModel(trackLines),
            strStart = dctModel.start;
        return strStart !== '' ? firstSolution(
            dctModel.cache, dctModel.stepCount, strStart, strStart, []
        ) : {
            nothing: true
        };
    };

    // showLine :: Int -> Int -> String -> Maybe (Int, Int) ->
    //              [(Int, Int, String)] -> String
    const showLine = curry((intCell, strFiller, maybeStart, xs) => {
        const
            blnSoln = maybeStart.nothing,
            [startCol, startRow] = blnSoln ? [0, 0] : maybeStart.just;
        return foldl((a, [iCol, iRow, sVal], i, xs) => ({
                    col: iCol + 1,
                    txt: a.txt +
                        concat(replicate((iCol - a.col) * intCell, strFiller)) +
                        justifyRight(
                            intCell, strFiller,
                            (blnSoln ? sVal : (
                                iRow === startRow &&
                                iCol === startCol ? '1' : '0')
                            )
                        )
                }), {
                    col: 0,
                    txt: ''
                },
                xs
            )
            .txt
    });

    // solutionString :: [String] -> Int -> String
    const solutionString = (boardLines, iProblem) => {
        const
            dtePre = Date.now(),
            intCols = boardLines.length > 0 ? boardLines[0].length : 0,
            soln = maybeTour(boardLines),
            intMSeconds = Date.now() - dtePre;

        if (soln.nothing) return 'No solution found …';

        const
            kCol = 0,
            kRow = 1,
            kSeq = 2,
            steps = soln.just,
            lstTriples = zipWith((h, n) => {
                    const [col, row] = map(
                        x => parseInt(x, 10), splitOn('.', h)
                    );
                    return [col, row, n.toString()];
                },
                steps,
                enumFromTo(1, soln.pathLen)),
            cellWidth = length(maximumBy(
                comparing(x => length(x[kSeq])), lstTriples
            )[kSeq]) + 1,
            lstGroups = groupBy(
                (a, b) => a[kRow] === b[kRow],
                sortBy(
                    mappendComparing([x => x[kRow], x => x[kCol]]),
                    lstTriples
                )),
            startXY = take(2, lstTriples[0]),
            strMap = 'PROBLEM ' + (parseInt(iProblem, 10) + 1) + '.\n\n' +
            unlines(map(showLine(cellWidth, ' ', {
                nothing: false,
                just: startXY
            }), lstGroups)),
            strSoln = 'First solution found in c. ' +
            intMSeconds + ' milliseconds:\n\n' +
            unlines(map(showLine(cellWidth, ' ', {
                nothing: true,
                just: startXY
            }), lstGroups)) + '\n\n';

        console.log(strSoln);
        return strMap + '\n\n' + strSoln;
    };

    // TEST -------------------------------------------------------------------
    return unlines(map(solutionString, problems));
})();
```

{{Out}}
(Executed in Atom editor, using 'Script' package).

```txt
PROBLEM 1.

     0  0  0
     0     0  0
     0  0  0  0  0  0  0
  0  0  0        0     0
  0     0        0  0  0
  1  0  0  0  0  0  0
        0  0     0
           0  0  0

First solution found in c. 21 milliseconds:

    25 14 23
     8    26 15
    13 24  7 22 27 16 31
  9 36 11       30    28
 12     6       21 32 17
  1 10 35 20  3 18 29
        2  5    33
          34 19  4


PROBLEM 2.

                 1     0
                 0     0
              0  0  0  0  0
                 0  0  0
        0        0     0        0
  0  0  0  0  0           0  0  0  0  0
        0  0                 0  0
  0  0  0  0  0           0  0  0  0  0
        0        0     0        0
                 0  0  0
              0  0  0  0  0
                 0     0
                 0     0

First solution found in c. 7084 milliseconds:

                 1     3
                50    52
             56 53  2 49  4
                48 51 54
       46       55     5       10
 45 42 35 40 47          11  6 13  8 15
       44 37                 9 16
 43 36 41 34 39          19 12  7 14 17
       38       33    27       18
                26 23 20
             32 21 30 25 28
                24    22
                31    29


[Finished in 7.2s]
```



## Julia

Uses the Hidato puzzle solver module, which has its source code listed [[Solve_a_Hidato_puzzle#Julia | here]]  in the Hadato task.

```julia
using .Hidato       # Note that the . here means to look locally for the module rather than in the libraries

const holyknight = """
 . 0 0 0 . . . . 
 . 0 . 0 0 . . . 
 . 0 0 0 0 0 0 0 
 0 0 0 . . 0 . 0
 0 . 0 . . 0 0 0
 1 0 0 0 0 0 0 . 
 . . 0 0 . 0 . . 
 . . . 0 0 0 . . """

const knightmoves = [[-2, -1], [-2, 1], [-1, -2], [-1, 2], [1, -2], [1, 2], [2, -1], [2, 1]]

board, maxmoves, fixed, starts = hidatoconfigure(holyknight)
printboard(board, " 0", "  ")
hidatosolve(board, maxmoves, knightmoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

```
{{output}}
```txt

   0 0 0
   0   0 0
   0 0 0 0 0 0 0
 0 0 0     0   0
 0   0     0 0 0
 1  0 0 0 0 0 0
     0 0   0
       0 0 0

     7  4 17
    16     8  5
     9  6  3 18 25 20 23
 31  2 15       22    26
 10    30       19 24 21
  1 32 11 14 29 34 27
       36 33    13
          12 35 28

```



## Kotlin

{{trans|Python}}

```scala
// version 1.1.3

val moves = arrayOf(
    intArrayOf(-1, -2), intArrayOf( 1, -2), intArrayOf(-1,  2), intArrayOf(1, 2),
    intArrayOf(-2, -1), intArrayOf(-2,  1), intArrayOf( 2, -1), intArrayOf(2, 1)
)

val board1 =
    " xxx    " +
    " x xx   " +
    " xxxxxxx" +
    "xxx  x x" +
    "x x  xxx" +
    "sxxxxxx " +
    "  xx x  " +
    "   xxx  "

val board2 =
    ".....s.x....." +
    ".....x.x....." +
    "....xxxxx...." +
    ".....xxx....." +
    "..x..x.x..x.." +
    "xxxxx...xxxxx" +
    "..xx.....xx.." +
    "xxxxx...xxxxx" +
    "..x..x.x..x.." +
    ".....xxx....." +
    "....xxxxx...." +
    ".....x.x....." +
    ".....x.x....."

fun solve(pz: Array<IntArray>, sz: Int, sx: Int, sy: Int, idx: Int, cnt: Int): Boolean {
    if (idx > cnt) return true
    for (i in 0 until moves.size) {
        val x = sx + moves[i][0]
        val y = sy + moves[i][1]
        if ((x in 0 until sz) && (y in 0 until sz) && pz[x][y] == 0) {
            pz[x][y] = idx
            if (solve(pz, sz, x, y, idx + 1, cnt)) return true
            pz[x][y] = 0
        }
    }
    return false
}

fun findSolution(b: String, sz: Int) {
    val pz = Array(sz) { IntArray(sz) { -1 } }
    var x = 0
    var y = 0
    var idx = 0
    var cnt = 0
    for (j in 0 until sz) {
        for (i in 0 until sz) {
            if (b[idx] == 'x') {
                pz[i][j] = 0
                cnt++
            }
            else if (b[idx] == 's') {
                pz[i][j] = 1
                cnt++
                x = i
                y = j
            }
            idx++
        }
    }

    if (solve(pz, sz, x, y, 2, cnt)) {
        for (j in 0 until sz) {
            for (i in 0 until sz) {
                if (pz[i][j] != -1)
                    print("%02d  ".format(pz[i][j]))
                else
                    print("--  ")
            }
            println()
        }
    }
    else println("Cannot solve this puzzle!")
}

fun main(args: Array<String>) {
    findSolution(board1,  8) 
    println()
    findSolution(board2, 13)
}
```


{{out}}

```txt

--  17  14  29  --  --  --  --  
--  28  --  18  15  --  --  --  
--  13  16  27  30  19  32  07  
25  02  11  --  --  06  --  20  
12  --  26  --  --  31  08  33  
01  24  03  10  05  34  21  --  
--  --  36  23  --  09  --  --  
--  --  --  04  35  22  --  --  

--  --  --  --  --  01  --  05  --  --  --  --  --  
--  --  --  --  --  10  --  12  --  --  --  --  --  
--  --  --  --  02  13  04  09  06  --  --  --  --  
--  --  --  --  --  08  11  14  --  --  --  --  --  
--  --  36  --  --  03  --  07  --  --  16  --  --  
35  42  33  44  37  --  --  --  15  20  27  22  25  
--  --  38  41  --  --  --  --  --  17  24  --  --  
39  34  43  32  45  --  --  --  19  28  21  26  23  
--  --  40  --  --  31  --  29  --  --  18  --  --  
--  --  --  --  --  46  51  56  --  --  --  --  --  
--  --  --  --  52  55  30  47  50  --  --  --  --  
--  --  --  --  --  48  --  54  --  --  --  --  --  
--  --  --  --  --  53  --  49  --  --  --  --  --

```



## Lua


```lua

local p1, p1W = ".xxx.....x.xx....xxxxxxxxxx..x.xx.x..xxxsxxxxxx...xx.x.....xxx..", 8
local p2, p2W = ".....s.x..........x.x.........xxxxx.........xxx.......x..x.x..x..xxxxx...xxxxx..xx.....xx..xxxxx...xxxxx..x..x.x..x.......xxx.........xxxxx.........x.x..........x.x.....", 13
local puzzle, movesCnt, wid = {}, 0, 0
local moves = { { -1, -2 }, {  1, -2 }, { -1,  2 }, {  1,  2 }, 
                { -2, -1 }, { -2,  1 }, {  2, -1 }, {  2,  1 } }

function isValid( x, y )
    return( x > 0 and x <= wid and y > 0 and y <= wid and puzzle[x + y * wid - wid] == 0 )
end
function solve( x, y, s )
    if s > movesCnt then return true end
    local test, a, b
    for i = 1, #moves do
        test = false
        a = x + moves[i][1]; b = y + moves[i][2]
        if isValid( a, b ) then
            puzzle[a + b * wid - wid] = s
            if solve( a, b, s + 1 ) then return true end
            puzzle[a + b * wid - wid] = 0
        end
    end
    return false
end
function printSolution()
    local lp
    for j = 1, wid do
        for i = 1, wid do
            lp = puzzle[i + j * wid - wid]
            if lp == -1 then io.write( "   " )
            else io.write( string.format( " %.2d", lp ) )
            end
        end
        print()
    end
    print( "\n" )
end
local sx, sy
function fill( pz, w )
    puzzle = {}; wid = w; movesCnt = #pz
    local lp
    for i = 1, #pz do
        lp = pz:sub( i, i )
        if lp == "x" then
            table.insert( puzzle, 0 )
        elseif lp == "." then
            table.insert( puzzle, -1 ); movesCnt = movesCnt - 1
        else 
            table.insert( puzzle, 1 )
            sx = 1 + ( i - 1 ) % wid; sy = math.floor( ( i + wid - 1 ) / wid )
        end
    end
end
-- [[ entry point ]] --
print( "\n\n" ); fill( p1, p1W );
if solve( sx, sy, 2 ) then printSolution() end
print( "\n\n" ); fill( p2, p2W );
if solve( sx, sy, 2 ) then printSolution() end

```

{{out}}

```txt


    17 14 29
    28    18 15
    13 16 27 30 19 32 07
 25 02 11       06    20
 12    26       31 08 33
 01 24 03 10 05 34 21
       36 23    09
          04 35 22

                01    05
                10    12
             02 13 04 09 06
                08 11 14
       36       03    07       16
 35 42 33 44 37          15 20 27 22 25
       38 41                17 24
 39 34 43 32 45          19 28 21 26 23
       40       31    29       18
                46 51 56
             52 55 30 47 50
                48    54
                53    49

```


## Perl

We perform a brute-force search. As an enhancement, we unroll the search by one level and use Parallel::ForkManager to search the top-level sub-trees concurrently, subject to the number of cores of course. We implement the search with explicit recursion, which impacts performance but improves readability and provides a use case for the "local" keyword.

```perl
package KT_Locations;
# A sequence of locations on a 2-D board whose order might or might not
# matter. Suitable for representing a partial tour, a complete tour, or the
# required locations to visit.
use strict;
use overload '""' => "as_string";
use English;
# 'locations' must be a reference to an array of 2-element array references,
# where the first element is the rank index and the second is the file index.
use Class::Tiny qw(N locations);
use List::Util qw(all);

sub BUILD {
    my $self = shift;
    $self->{N} //= 8;
    $self->{N} >= 3 or die "N must be at least 3";
    all {ref($ARG) eq 'ARRAY' && scalar(@{$ARG}) == 2} @{$self->{locations}}
        or die "At least one element of 'locations' is invalid";
    return;
}

sub as_string {
    my $self = shift;
    my %idxs;
    my $idx = 1;
    foreach my $loc (@{$self->locations}) {
        $idxs{join(q{K},@{$loc})} = $idx++;
    }
    my $str;
    {
        my $w = int(log(scalar(@{$self->locations}))/log(10.)) + 2;
        my $fmt = "%${w}d";
        my $N = $self->N;
        my $non_tour = q{ } x ($w-1) . q{-};
        for (my $r=0; $r<$N; $r++) {
            for (my $f=0; $f<$N; $f++) {
                my $k = join(q{K}, $r, $f);
                $str .= exists($idxs{$k}) ? sprintf($fmt, $idxs{$k}) : $non_tour;
            }
            $str .= "\n";
        }
    }
    return $str;
}

sub as_idx_hash {
    my $self = shift;
    my $N = $self->N;
    my $result;
    foreach my $pair (@{$self->locations}) {
        my ($r, $f) = @{$pair};
        $result->{$r * $N + $f}++;
    }
    return $result;
}

package KnightsTour;
use strict;
# If supplied, 'str' is parsed to set 'N', 'start_location', and 
# 'locations_to_visit'.  'legal_move_idxs' is for improving performance.
use Class::Tiny qw( N start_location locations_to_visit str legal_move_idxs );
use English;
use Parallel::ForkManager;
use Time::HiRes qw( gettimeofday tv_interval );

sub BUILD {
    my $self = shift;
    if ($self->{str}) {
        my ($n, $sl, $ltv) = _parse_input_string($self->{str});
        $self->{N} = $n;
        $self->{start_location} = $sl;
        $self->{locations_to_visit} = $ltv;
    }
    $self->{N} //= 8;
    $self->{N} >= 3 or die "N must be at least 3";
    exists($self->{start_location}) or die "Must supply start_location";
    die "start_location is invalid"
        if ref($self->{start_location}) ne 'ARRAY' ||
           scalar(@{$self->{start_location}}) != 2;
    exists($self->{locations_to_visit}) or die "Must supply locations_to_visit";
    ref($self->{locations_to_visit}) eq 'KT_Locations'
        or die "locations_to_visit must be a KT_Locations instance";
    $self->{N} == $self->{locations_to_visit}->N
        or die "locations_to_visit has mismatched board size";
    $self->precompute_legal_moves();
    return;
}

sub _parse_input_string {
    my @rows = split(/[\r\n]+/s, shift);
    my $N = scalar(@rows);
    my ($start_location, @to_visit);
    for (my $r=0; $r<$N; $r++) {
        my $row_r = $rows[$r];
        for (my $f=0; $f<$N; $f++) {
            my $c = substr($row_r, $f, 1);
            if ($c eq '1') { $start_location = [$r, $f]; }
            elsif ($c eq '0') { push @to_visit, [$r, $f]; }
        }
    }
    $start_location or die "No starting location provided";
    return ($N,
            $start_location,
            KT_Locations->new(N => $N, locations => \@to_visit));
}

sub precompute_legal_moves {
    my $self = shift;
    my $N = $self->{N};
    my $ktl_ixs = $self->{locations_to_visit}->as_idx_hash();
    for (my $r=0; $r<$N; $r++) {
        for (my $f=0; $f<$N; $f++) {
            my $k = $r * $N + $f;
            $self->{legal_move_idxs}->{$k} =
                _precompute_legal_move_idxs($r, $f, $N, $ktl_ixs);
        }
    }
    return;
}

sub _precompute_legal_move_idxs {
    my ($r, $f, $N, $ktl_ixs) = @ARG;
    my $r_plus_1  = $r + 1;  my $r_plus_2 = $r + 2;
    my $r_minus_1 = $r - 1;  my $r_minus_2 = $r - 2;
    my $f_plus_1  = $f + 1;  my $f_plus_2 = $f + 2;
    my $f_minus_1 = $f - 1;  my $f_minus_2 = $f - 2;
    my @result = grep { exists($ktl_ixs->{$ARG}) }
                 map { $ARG->[0] * $N + $ARG->[1] }
                 grep {$ARG->[0] >= 0 && $ARG->[0] < $N &&
                       $ARG->[1] >= 0 && $ARG->[1] < $N}
                      ([$r_plus_2,  $f_minus_1], [$r_plus_2,  $f_plus_1],
                       [$r_minus_2, $f_minus_1], [$r_minus_2, $f_plus_1],
                       [$r_plus_1,  $f_plus_2],  [$r_plus_1,  $f_minus_2],
                       [$r_minus_1, $f_plus_2],  [$r_minus_1, $f_minus_2]);
    return \@result;
}

sub find_tour {
    my $self = shift;
    my $num_to_visit = scalar(@{$self->locations_to_visit->locations});
    my $N = $self->N;
    my $start_loc_idx =
        $self->start_location->[0] * $N + $self->start_location->[1];
    my $visited;  for (my $i=0; $i<$N*$N; $i++) { vec($visited, $i, 1) = 0; }
    vec($visited, $start_loc_idx, 1) = 1;
    # We unwind the search by one level and use Parallel::ForkManager to search
    # the top-level sub-trees concurrently, assuming there are enough cores.
    my @next_loc_idxs = @{$self->legal_move_idxs->{$start_loc_idx}};
    my $pm = new Parallel::ForkManager(scalar(@next_loc_idxs));
    foreach my $next_loc_idx (@next_loc_idxs) {
        $pm->start and next;  # Do the fork
        my $t0 = [gettimeofday];
        vec($visited, $next_loc_idx, 1) = 1;  # (The fork cloned $visited.)
        my $tour = _find_tour_helper($N,
                                     $num_to_visit - 1,
                                     $next_loc_idx,
                                     $visited,
                                     $self->legal_move_idxs);
        my $elapsed = tv_interval($t0);
        my ($r, $f) = _idx_to_rank_and_file($next_loc_idx, $N);
        if (defined $tour) {
            my @tour_locs =
                map { [_idx_to_rank_and_file($ARG, $N)] }
                    ($start_loc_idx, $next_loc_idx, split(/\s+/s, $tour));
            my $kt_locs = KT_Locations->new(N => $N, locations => \@tour_locs);
            print "Found a tour after first move ($r, $f) ",
                  "in $elapsed seconds:\n", $kt_locs, "\n";
        }
        else {
            print "No tour found after first move ($r, $f). ",
                  "Took $elapsed seconds.\n";
        }
        $pm->finish; # Do the exit in the child process
    }
    $pm->wait_all_children;
    return;
}

sub _idx_to_rank_and_file {
    my ($idx, $N) = @ARG;
    my $f = $idx % $N;
    my $r = ($idx - $f) / $N;
    return ($r, $f);
}

sub _find_tour_helper {
    my ($N, $num_to_visit, $current_loc_idx, $visited, $legal_move_idxs) = @ARG;

    # The performance hot spot.
    local *inner_helper = sub {
        my ($num_to_visit, $current_loc_idx, $visited) = @ARG;
        if ($num_to_visit == 0) {
            return q{ };  # Solution found.
        }
        my @next_loc_idxs = @{$legal_move_idxs->{$current_loc_idx}};
        my $num_to_visit2 = $num_to_visit - 1;
        foreach my $loc_idx2 (@next_loc_idxs) {
            next if vec($visited, $loc_idx2, 1);
            my $visited2 = $visited;
            vec($visited2, $loc_idx2, 1) = 1;
            my $recursion = inner_helper($num_to_visit2, $loc_idx2, $visited2);
            return $loc_idx2 . q{ } . $recursion if defined $recursion;
        }
        return;
    };

    return inner_helper($num_to_visit, $current_loc_idx, $visited);
}

package main;
use strict;

solve_size_8_problem();
solve_size_13_problem();
exit 0;

sub solve_size_8_problem {
    my $problem = <<"END_SIZE_8_PROBLEM";
--000---
--0-00--
-0000000
000--0-0
0-0--000
1000000-
--00-0--
---000--
END_SIZE_8_PROBLEM
    my $kt = KnightsTour->new(str => $problem);
    print "Finding a tour for an 8x8 problem...\n";
    $kt->find_tour();
    return;
}

sub solve_size_13_problem {
    my $problem = <<"END_SIZE_13_PROBLEM";
-----1-0-----
-----0-0-----
----00000----
-----000-----
--0--0-0--0--
00000---00000
--00-----00--
00000---00000
--0--0-0--0--
-----000-----
----00000----
-----0-0-----
-----0-0-----
END_SIZE_13_PROBLEM
    my $kt = KnightsTour->new(str => $problem);
    print "Finding a tour for a 13x13 problem...\n";
    $kt->find_tour();
    return;
}
```

{{out}}
The timings shown below were obtained on a Dell Optiplex 9020 with 4 cores.

```txt
...>holy_knights_tour.pl 
Finding a tour for an 8x8 problem... 
Found a tour after first move (6, 2) in 0.018372 seconds: 
  -  - 18 31 16  -  -  - 
  -  - 23  - 33 30  -  - 
  - 19 32 17 24 15 34 29 
  7 22  5  -  - 28  - 26 
 20  -  8  -  - 25 14 35 
  1  6 21  4 11 36 27  - 
  -  -  2  9  - 13  -  - 
  -  -  - 12  3 10  -  - 

Found a tour after first move (4, 2) in 0.010491 seconds: 
  -  - 30 23 20  -  -  - 
  -  -  9  - 31 22  -  - 
  - 29 24 21 10 19 32 15 
 25  8 27  -  - 16  - 18 
 28  -  2  -  - 11 14 33 
  1 26  7 12  5 34 17  - 
  -  - 36  3  - 13  -  - 
  -  -  -  6 35  4  -  - 

Found a tour after first move (3, 1) in 0.048164 seconds: 
  -  - 28 11 14  -  -  - 
  -  - 13  -  9 30  -  - 
  - 27 10 29 12 15 18 31 
 23  2 25  -  -  8  - 16 
 26  - 22  -  - 17 32 19 
  1 24  3 34  5 20  7  - 
  -  - 36 21  - 33  -  - 
  -  -  -  4 35  6  -  - 

Finding a tour for a 13x13 problem... 
Found a tour after first move (2, 6) in 78.827185 seconds: 
  -  -  -  -  -  1  - 21  -  -  -  -  - 
  -  -  -  -  - 22  -  6  -  -  -  -  - 
  -  -  -  -  4  7  2 23 20  -  -  -  - 
  -  -  -  -  - 24  5  8  -  -  -  -  - 
  -  - 34  -  -  3  - 19  -  - 56  -  - 
 35 30 37 28 25  -  -  -  9 18 11 16 13 
  -  - 26 33  -  -  -  -  - 55 14  -  - 
 31 36 29 38 27  -  -  - 53 10 17 12 15 
  -  - 32  -  - 39  - 45  -  - 54  -  - 
  -  -  -  -  - 46 49 52  -  -  -  -  - 
  -  -  -  - 40 51 42 47 44  -  -  -  - 
  -  -  -  -  - 48  - 50  -  -  -  -  - 
  -  -  -  -  - 41  - 43  -  -  -  -  - 

Found a tour after first move (2, 4) in 100.327934 seconds: 
  -  -  -  -  -  1  - 23  -  -  -  -  - 
  -  -  -  -  - 24  - 20  -  -  -  -  - 
  -  -  -  -  2 19  4 25 22  -  -  -  - 
  -  -  -  -  - 26 21 18  -  -  -  -  - 
  -  - 36  -  -  3  -  5  -  - 12  -  - 
 37 32 39 30 27  -  -  - 17  6 15  8 13 
  -  - 28 35  -  -  -  -  - 11 56  -  - 
 33 38 31 40 29  -  -  - 55 16  7 14  9 
  -  - 34  -  - 41  - 47  -  - 10  -  - 
  -  -  -  -  - 48 51 54  -  -  -  -  - 
  -  -  -  - 42 53 44 49 46  -  -  -  - 
  -  -  -  -  - 50  - 52  -  -  -  -  - 
  -  -  -  -  - 43  - 45  -  -  -  -  - 

Found a tour after first move (1, 7) in 1443.340089 seconds: 
  -  -  -  -  -  1  - 21  -  -  -  -  - 
  -  -  -  -  - 22  -  2  -  -  -  -  - 
  -  -  -  - 18  3 16 23 20  -  -  -  - 
  -  -  -  -  - 24 19  4  -  -  -  -  - 
  -  - 34  -  - 17  - 15  -  - 56  -  - 
 35 30 37 28 25  -  -  -  5 14  7 12  9 
  -  - 26 33  -  -  -  -  - 55 10  -  - 
 31 36 29 38 27  -  -  - 53  6 13  8 11 
  -  - 32  -  - 39  - 45  -  - 54  -  - 
  -  -  -  -  - 46 49 52  -  -  -  -  - 
  -  -  -  - 40 51 42 47 44  -  -  -  - 
  -  -  -  -  - 48  - 50  -  -  -  -  - 
  -  -  -  -  - 41  - 43  -  -  -  -  - 
```



## Perl 6

This uses a Warnsdorff solver, which cuts down the number of tries by more than a factor of six over the brute force approach. This same solver is used in:

* [[Solve a Hidato puzzle#Perl_6|Solve a Hidato puzzle]]
* [[Solve a Hopido puzzle#Perl_6|Solve a Hopido puzzle]]
* [[Solve a Holy Knight's tour#Perl_6|Solve a Holy Knight's tour]]
* [[Solve a Numbrix puzzle#Perl_6|Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle#Perl_6|Solve the no connection puzzle]]


```perl6
my @adjacent =
               [ -2, -1],  [ -2, 1],
      [-1,-2],                       [-1,+2],
      [+1,-2],                       [+1,+2],
               [ +2, -1],  [ +2, 1];

put "\n" xx 60;

solveboard q:to/END/;
    . 0 0 0
    . 0 . 0 0
    . 0 0 0 0 0 0 0
    0 0 0 . . 0 . 0
    0 . 0 . . 0 0 0
    1 0 0 0 0 0 0
    . . 0 0 . 0
    . . . 0 0 0
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


{{out}}

```txt
   25 14 27
   36    24 15
   31 26 13 28 23  6 17
35 12 29       16    22
30    32        7 18  5
 1 34 11  8 19  4 21
       2 33     9
         10  3 20
84 tries
```



## Phix

Tweaked the knights tour algorithm (to use a limit variable rather than size*size). Bit slow on the second one...

```Phix
sequence board, warnsdorffs

integer size, limit, nchars
string fmt, blank

constant ROW = 1, COL = 2
constant moves = {{-1,-2},{-2,-1},{-2,1},{-1,2},{1,2},{2,1},{2,-1},{1,-2}}

function onboard(integer row, integer col)
    return row>=1 and row<=size and col>=nchars and col<=nchars*size
end function

procedure init_warnsdorffs()
integer nrow,ncol
    for row=1 to size do
        for col=nchars to nchars*size by nchars do
            for move=1 to length(moves) do
                nrow = row+moves[move][ROW]
                ncol = col+moves[move][COL]*nchars
                if onboard(nrow,ncol) then
                    -- (either of these would work)
                    warnsdorffs[row][col] += 1
--                  warnsdorffs[nrow][ncol] += 1
                end if
            end for
        end for
    end for
end procedure

atom t0 = time()
integer tries = 0, backtracks = 0
atom t1 = time()+1
function solve(integer row, integer col, integer n)
integer nrow, ncol
    if time()>t1 then
        ?{row,floor(col/nchars),n,tries}
        puts(1,join(board,"\n"))
        t1 = time()+1
--      if wait_key()='!' then ?9/0 end if
    end if
    tries+= 1
    if n>limit then return 1 end if
    sequence wmoves = {}
    for move=1 to length(moves) do
        nrow = row+moves[move][ROW]
        ncol = col+moves[move][COL]*nchars
        if onboard(nrow,ncol)
        and board[nrow][ncol]=' ' then
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
            backtracks += 1
            board[nrow][ncol-nchars+1..ncol] = blank
        end for
        for m=1 to length(wmoves) do
            {?,nrow,ncol} = wmoves[m]
            warnsdorffs[nrow][ncol] += 1
        end for
    end if
    return 0
end function

procedure holyknight(sequence s)
integer y, ch, sx, sy
    s = split(s,'\n')
    size = length(s)
    nchars = length(sprintf(" %d",size*size))   
    fmt = sprintf(" %%%dd",nchars-1)
    blank = repeat(' ',nchars)
    board = repeat(repeat(' ',size*nchars),size)
    limit = 1
    for x=1 to size do
        y = nchars
        for j=1 to size do
            if j>length(s[x]) then
                ch = '-'
            else
                ch = s[x][j]
            end if
            if ch=' ' then
                ch = '-'
            elsif ch='0' then
                ch = ' '
                limit += 1
            elsif ch='1' then
                sx = x
                sy = y
            end if
            board[x][y] = ch
            y += nchars
        end for
    end for
    warnsdorffs = repeat(repeat(0,size*nchars),size)
    init_warnsdorffs()
    t0 = time()
    tries = 0
    backtracks = 0
    t1 = time()+1
    if solve(sx,sy,2) then
        puts(1,join(board,"\n"))
        printf(1,"\nsolution found in %d tries, %d backtracks (%3.2fs)\n",{tries,backtracks,time()-t0})
    else
        puts(1,"no solutions found\n")
    end if
end procedure

constant board1 = """
 000
 0 00
 0000000
000  0 0
0 0  000
1000000
  00 0
   000"""

holyknight(board1)

constant board2 = """
-----1-0-----
-----0-0-----
----00000----
-----000-----
--0--0-0--0--
00000---00000
--00-----00--
00000---00000
--0--0-0--0--
-----000-----
----00000----
-----0-0-----
-----0-0-----"""

holyknight(board2)

{} = wait_key()
```

{{out}}

```txt

  - 21  4 19  -  -  -  -
  - 18  - 22  5  -  -  -
  - 15 20  3 32 23  6  9
 17  2 33  -  -  8  - 24
 14  - 16  -  - 31 10  7
  1 34 13 30 27 36 25  -
  -  - 28 35  - 11  -  -
  -  -  - 12 29 26  -  -
solution found in 31718 tries, 31682 backtracks (0.11s)

   -   -   -   -   -   1   -  55   -   -   -   -   -
   -   -   -   -   -   8   -   2   -   -   -   -   -
   -   -   -   -   6   3  54   9  56   -   -   -   -
   -   -   -   -   -  10   7   4   -   -   -   -   -
   -   -  12   -   -   5   -  53   -   -  46   -   -
  13  16  23  18  11   -   -   -  45  52  43  50  41
   -   -  14  21   -   -   -   -   -  47  40   -   -
  15  22  17  24  19   -   -   -  39  44  51  42  49
   -   -  20   -   -  25   -  33   -   -  48   -   -
   -   -   -   -   -  32  35  38   -   -   -   -   -
   -   -   -   -  26  37  28  31  34   -   -   -   -
   -   -   -   -   -  30   -  36   -   -   -   -   -
   -   -   -   -   -  27   -  29   -   -   -   -   -
solution found in 61341542 tries, 61341486 backtracks (180.56s)

```



## Python


```python

from sys import stdout
moves = [
    [-1, -2], [1, -2], [-1, 2], [1, 2],
    [-2, -1], [-2, 1], [2, -1], [2, 1]
]


def solve(pz, sz, sx, sy, idx, cnt):
    if idx > cnt:
        return 1

    for i in range(len(moves)):
        x = sx + moves[i][0]
        y = sy + moves[i][1]
        if sz > x > -1 and sz > y > -1 and pz[x][y] == 0:
            pz[x][y] = idx
            if 1 == solve(pz, sz, x, y, idx + 1, cnt):
                return 1
            pz[x][y] = 0

    return 0


def find_solution(pz, sz):
    p = [[-1 for j in range(sz)] for i in range(sz)]
    idx = x = y = cnt = 0
    for j in range(sz):
        for i in range(sz):
            if pz[idx] == "x":
                p[i][j] = 0
                cnt += 1
            elif pz[idx] == "s":
                p[i][j] = 1
                cnt += 1
                x = i
                y = j
            idx += 1

    if 1 == solve(p, sz, x, y, 2, cnt):
        for j in range(sz):
            for i in range(sz):
                if p[i][j] != -1:
                    stdout.write(" {:0{}d}".format(p[i][j], 2))
                else:
                    stdout.write("   ")
            print()
    else:
        print("Cannot solve this puzzle!")


# entry point
find_solution(".xxx.....x.xx....xxxxxxxxxx..x.xx.x..xxxsxxxxxx...xx.x.....xxx..", 8)
print()
find_solution(".....s.x..........x.x.........xxxxx.........xxx.......x..x.x..x..xxxxx...xxxxx..xx.....xx..xxxxx...xxxxx..x..x.x..x.......xxx.........xxxxx.........x.x..........x.x.....", 13)

```

{{out}}
```txt

    17 14 29            
    28    18 15         
    13 16 27 30 19 32 07
 25 02 11       06    20
 12    26       31 08 33
 01 24 03 10 05 34 21   
       36 23    09      
          04 35 22 
     
                01    05               
                10    12               
             02 13 04 09 06            
                08 11 14               
       36       03    07       16      
 35 42 33 44 37          15 20 27 22 25
       38 41                17 24      
 39 34 43 32 45          19 28 21 26 23
       40       31    29       18      
                46 51 56               
             52 55 30 47 50            
                48    54               
                53    49               

```



## Racket


This solution uses the module "hidato-family-solver.rkt" from
[[Solve a Numbrix puzzle#Racket]]. The difference between the two is
essentially the neighbourhood function.

It solves the tasked problem, as well as the "extra credit" from [[#Ada]].


```racket
#lang racket
(require "hidato-family-solver.rkt")

(define knights-neighbour-offsets
  '((+1 +2) (-1 +2) (+1 -2) (-1 -2) (+2 +1) (-2 +1) (+2 -1) (-2 -1)))

(define solve-a-knights-tour (solve-hidato-family knights-neighbour-offsets))

(displayln
 (puzzle->string
  (solve-a-knights-tour
   #(#(_ 0 0 0 _ _ _ _)
     #(_ 0 _ 0 0 _ _ _)
     #(_ 0 0 0 0 0 0 0)
     #(0 0 0 _ _ 0 _ 0)
     #(0 _ 0 _ _ 0 0 0)
     #(1 0 0 0 0 0 0 _)
     #(_ _ 0 0 _ 0 _ _)
     #(_ _ _ 0 0 0 _ _)))))

(newline)

(displayln
 (puzzle->string
  (solve-a-knights-tour
   #(#(- - - - - 1 - 0 - - - - -)
     #(- - - - - 0 - 0 - - - - -)
     #(- - - - 0 0 0 0 0 - - - -)
     #(- - - - - 0 0 0 - - - - -)
     #(- - 0 - - 0 - 0 - - 0 - -)
     #(0 0 0 0 0 - - - 0 0 0 0 0)
     #(- - 0 0 - - - - - 0 0 - -)
     #(0 0 0 0 0 - - - 0 0 0 0 0)
     #(- - 0 - - 0 - 0 - - 0 - -)
     #(- - - - - 0 0 0 - - - - -)
     #(- - - - 0 0 0 0 0 - - - -)
     #(- - - - - 0 - 0 - - - - -)
     #(- - - - - 0 - 0 - - - - -)))))
```


{{out}}

```txt
 _ 13 30 23  _  _  _  _
 _ 24  _ 14 31  _  _  _
 _ 29 12 25 22 15 32  7
11 26 21  _  _  6  _ 16
28  _ 10  _  _ 33  8  5
 1 20 27 34  9  4 17  _
 _  _  2 19  _ 35  _  _
 _  _  _ 36  3 18  _  _

  _   _   _   _   _   1   _  51   _   _   _   _   _
  _   _   _   _   _  50   _   2   _   _   _   _   _
  _   _   _   _  56   3  52  49  54   _   _   _   _
  _   _   _   _   _  48  55   4   _   _   _   _   _
  _   _  46   _   _   5   _  53   _   _  24   _   _
 45   8  11   6  47   _   _   _  23  30  19  28  21
  _   _  44   9   _   _   _   _   _  25  22   _   _
 43  10   7  12  41   _   _   _  31  18  29  20  27
  _   _  42   _   _  13   _  17   _   _  26   _   _
  _   _   _   _   _  40  37  32   _   _   _   _   _
  _   _   _   _  36  33  14  39  16   _   _   _   _
  _   _   _   _   _  38   _  34   _   _   _   _   _
  _   _   _   _   _  35   _  15   _   _   _   _   _
```



## REXX

This REXX program is essentially a modified   ''knight's tour''   REXX program with support to place pennies on the chessboard. 

Also supported is the specification of the size of the chessboard and the placement of the knight (initial position).

This is an   ''open tour''   solution.   (See this task's   ''discussion''   page for an explanation   [in the first section]). 

```rexx
/*REXX program solves the  holy knight's tour  problem for a (general)  NxN  chessboard.*/
blank=pos('//', space(arg(1), 0))\==0            /*see if the pennies are to be shown.  */
parse arg  ops   '/'   cent                      /*obtain the options and the pennies.  */
parse var  ops  N  sRank  sFile .                /*boardsize, starting position, pennys.*/
if     N=='' |     N==","  then     N=8          /*no boardsize specified?  Use default.*/
if sRank=='' | sRank==","  then sRank=N          /*starting rank given?      "      "   */
if sFile=='' | sFile==","  then sFile=1          /*    "    file   "         "      "   */
NN=N**2;  NxN='a ' N"x"N ' chessboard'           /*file  [↓]          [↓]   r=rank      */
@.=;              do r=1  for N;  do f=1  for N;   @.r.f=.;    end /*f*/;        end /*r*/
                                                 /*[↑]  create an empty  NxN chessboard.*/
cent=space( translate( cent, , ',') )            /*allow use of comma (,) for separater.*/
cents=0                                          /*number of pennies on the chessboard. */
       do  while  cent\=''                       /* [↓]  possibly place the pennies.    */
       parse var  cent   cr  cf  x  '/'  cent    /*extract where to place the pennies.  */
       if x=''   then x=1                        /*if number not specified, use 1 penny.*/
       if cr=''  then iterate                    /*support the  "blanking"  option.     */
                               do cf=cf  for x   /*now, place  X  pennies on chessboard.*/
                               @.cr.cf= '¢'      /*mark chessboard position with a penny*/
                               end   /*cf*/      /* [↑]  places X pennies on chessboard.*/
       end   /*while*/                           /* [↑]  allows of the placing of  X  ¢s*/
                                                 /* [↓]  traipse through the chessboard.*/
    do r=1  for N;  do f=1  for N;  cents=cents + (@.r.f=='¢');   end /*f*/;     end /*r*/
                                                 /* [↑]  count the number of pennies.   */
if cents\==0  then say cents   'pennies placed on chessboard.'
target=NN - cents                                /*use this as the number of moves left.*/
                   Kr = '2 1 -1 -2 -2 -1  1  2'  /*the legal "rank"  moves for a knight.*/
                   Kf = '1 2  2  1 -1 -2 -2 -1'  /* "    "   "file"    "    "  "    "   */
kr.M=words(Kr)                                   /*number of possible moves for a Knight*/
parse var Kr  Kr.1 Kr.2 Kr.3 Kr.4 Kr.5 Kr.6 Kr.7 Kr.8   /*parse the legal moves by hand.*/
parse var Kf  Kf.1 Kf.2 Kf.3 Kf.4 Kf.5 Kf.6 Kf.7 Kf.8   /*  "    "    "     "    "   "  */
beg= '-1-'                                       /* [↑]   create the  NxN  chessboard.  */
if @.sRank.sFile ==.    then @.sRank.sFile=beg   /*the knight's starting position.      */
if @.sRank.sFile\==beg  then do    sRank=1  for N   /*find starting rank for the knight.*/
                                do sFile=1  for N   /*  "     "     file  "   "     "   */
                                if @.sRank.sFile\==.  then iterate
                                @.sRank.sFile=beg   /*the knight's starting position.   */
                                leave sRank         /*we have a spot, so leave all this.*/
                                end   /*sFile*/
                             end      /*sRank*/
@hkt= "holy knight's tour"                       /*a handy─dandy literal for the  SAYs. */
if \move(2,sRank,sFile)  &  \(N==1)  then say 'No'    @hkt    "solution for"        NxN'.'
                                     else say 'A solution for the'   @hkt   "on"    NxN':'

                                                 /*show chessboard with moves and coins.*/
!=left('', 9 * (n<18) );           say           /*used for indentation of chessboard.  */
_=substr( copies("┼───", N), 2);   say  ! translate('┌'_"┐", '┬', "┼")
     do   r=N  for N  by -1;       if r\==N      then say ! '├'_"┤";     L=@.
       do f=1  for N; ?=@.r.f;     if ?==target  then ?='end';           L=L'│'center(?,3)
       end      /*f*/
     if blank then L=translate(L,,'¢')           /*blank out the pennies on chessboard ?*/
     say !  translate(L'│', , .)                 /*display  a  rank of the  chessboard. */
     end        /*r*/                            /*19x19 chessboard can be shown 80 cols*/
say  !  translate('└'_"┘", '┴', "┼")             /*display the last rank of chessboard. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
move: procedure expose @. Kr. Kf. target; parse arg #,rank,file /*obtain move,rank,file.*/
        do t=1  for Kr.M;   nr=rank+Kr.t;         nf=file+Kf.t  /*position of the knight*/
        if @.nr.nf==.  then do;                   @.nr.nf=#     /*Empty? Knight can move*/
                               if #==target       then return 1 /*is this the last move?*/
                               if move(#+1,nr,nf) then return 1 /* "   "   "    "    "  */
                               @.nr.nf=.                        /*undo the above move.  */
                            end                                 /*try a different move. */
        end   /*t*/                                             /* [↑]  all moves tried.*/
     return 0                                                   /*tour isn't possible.  */
```

'''output'''   when the following is used for input: 

<tt> , 3 1 /1,1 3 /1,7 2 /2,1 2 /2,5 /2,7 2 /3,8 /4,2 /4,4 2 /5,4 2 /5,7 /6,1 /7,1 /7,3 /7,6 3 /8,1  /8,5 4 </tt>

```txt

28 pennies placed on chessboard.
A solution for the holy knight's tour on a  8x8  chessboard:

          ┌───┬───┬───┬───┬───┬───┬───┬───┐
          │ ¢ │25 │12 │27 │ ¢ │ ¢ │ ¢ │ ¢ │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │ ¢ │end│ ¢ │24 │13 │ ¢ │ ¢ │ ¢ │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │ ¢ │11 │26 │ 3 │28 │23 │14 │ 5 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │35 │ 2 │31 │ ¢ │ ¢ │ 4 │ ¢ │22 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │10 │ ¢ │34 │ ¢ │ ¢ │29 │ 6 │15 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │-1-│32 │ 9 │30 │19 │16 │21 │ ¢ │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │ ¢ │ ¢ │18 │33 │ ¢ │ 7 │ ¢ │ ¢ │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │ ¢ │ ¢ │ ¢ │ 8 │17 │20 │ ¢ │ ¢ │
          └───┴───┴───┴───┴───┴───┴───┴───┘

```

'''output'''   when the following   (for a "cleaner" chessboard, no pennies are shown)   is used for input: 

<tt> , 3 1 /1,1 3 /1,7 2 /2,1 2 /2,5 /2,7 2 /3,8 /4,2 /4,4 2 /5,4 2 /5,7 /6,1 /7,1 /7,3 /7,6 3 /8,1  /8,5 4 // </tt>

```txt

28 pennies placed on chessboard.
A solution for the holy knight's tour on a  8x8  chessboard:

          ┌───┬───┬───┬───┬───┬───┬───┬───┐
          │   │25 │12 │27 │   │   │   │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │end│   │24 │13 │   │   │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │11 │26 │ 3 │28 │23 │14 │ 5 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │35 │ 2 │31 │   │   │ 4 │   │22 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │10 │   │34 │   │   │29 │ 6 │15 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │-1-│32 │ 9 │30 │19 │16 │21 │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │   │18 │33 │   │ 7 │   │   │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │   │   │   │ 8 │17 │20 │   │   │
          └───┴───┴───┴───┴───┴───┴───┴───┘

```



## Ruby

This solution uses HLPsolver from [[Solve_a_Hidato_puzzle#With_Warnsdorff | here]]

```ruby
require 'HLPsolver'

ADJACENT = [[-1,-2],[-2,-1],[-2,1],[-1,2],[1,2],[2,1],[2,-1],[1,-2]]

boardy = <<EOS
. . 0 0 0
. . 0 . 0 0
. 0 0 0 0 0 0 0
0 0 0 . . 0 . 0
0 . 0 . . 0 0 0
1 0 0 0 0 0 0
. . 0 0 . 0
. . . 0 0 0
EOS
t0 = Time.now
HLPsolver.new(boardy).solve
puts " #{Time.now - t0} sec"
```


Which produces:


```txt

Problem:
        0  0  0         
        0     0  0      
     0  0  0  0  0  0  0
  0  0  0        0     0
  0     0        0  0  0
  1  0  0  0  0  0  0   
        0  0     0      
           0  0  0      

Solution:
        8 33 14         
       13     7 32      
     9 34 31 22 15  6 29
 35 12 21       30    16
 10    36       23 28  5
  1 20 11 24 27  4 17   
        2 19    25      
          26  3 18      

 0.005 sec

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

oo::class create HKTSolver {
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
	        -1 -2   1 -2
	    -2 -1          2 -1
	    -2  1          2 1
	        -1 2    1 2
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
	    string map {" " -1} $c
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
  0 0 0 
  0   0 0 
  0 0 0 0 0 0 0
0 0 0     0   0
0   0     0 0 0
1 0 0 0 0 0 0
    0 0   0
      0 0 0
}]
showPuzzle $puzzle "Input"
HKTSolver create hkt $puzzle
hkt solve
showPuzzle [hkt solution] "Output"
```

{{out}}

```txt

Input with 36 cells
     __ __ __            
     __    __ __         
     __ __ __ __ __ __ __
  __ __ __       __    __
  __    __       __ __ __
   1 __ __ __ __ __ __   
        __ __    __      
           __ __ __      
Output with 36 cells
     13  6 15            
      8    12 31         
      5 14  7 16 27 32 29
   9  2 11       30    26
   4    22       17 28 33
   1 10  3 18 21 34 25   
        36 23    19      
           20 35 24      

```

[[Category:Puzzles]]
