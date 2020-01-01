+++
title = "Generate random chess position"
description = ""
date = 2019-09-21T06:28:29Z
aliases = []
[extra]
id = 19820
[taxonomies]
categories = []
tags = []
+++

{{task}}

The purpose of this task is to generate a random chess position in [[wp:Forsyth–Edwards Notation|FEN format]].  The position does not have to be realistic or even balanced, but it must comply to the following rules:

* there is one and only one king of each color (one black king and one white king);
* the kings must not be placed on adjacent squares;
* there can not be any pawn in the promotion square (no white pawn in the eighth rank, and no black pawn in the first rank);
* including the kings, up to 32 pieces of either color can be placed.  There is no requirement for material balance between sides;  The picking of pieces does not have to comply to a regular chess set : there can be five knights, twenty rooks, whatever... as long as the total number of pieces do not exceed thirty-two.
* it is white's turn, it is assumed that both sides have lost castling rights and that there is no possibility for ''en passant'' (the FEN should thus end in <tt>w - - 0 1</tt>).


No requirement is made regarding the probability distribution of your method, but your program should be able to span a reasonably representative sample of all possible positions.  For instance, programs that would always generate positions with say five pieces on the board, or with kings on a corner, would not be considered truly random.



## C

{{trans|Java}}

```c
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define TRUE 1
#define FALSE 0

typedef int bool;

char grid[8][8];

void placeKings() {
    int r1, r2, c1, c2;
    for (;;) {
        r1 = rand() % 8;
        c1 = rand() % 8;
        r2 = rand() % 8;
        c2 = rand() % 8;
        if (r1 != r2 && abs(r1 - r2) > 1 && abs(c1 - c2) > 1) {
            grid[r1][c1] = 'K';
            grid[r2][c2] = 'k';
            return;
        }
    }
}

void placePieces(const char *pieces, bool isPawn) {
    int n, r, c;
    int numToPlace = rand() % strlen(pieces);
    for (n = 0; n < numToPlace; ++n) {
        do {
            r = rand() % 8;
            c = rand() % 8;
        }
        while (grid[r][c] != 0 || (isPawn && (r == 7 || r == 0)));
        grid[r][c] = pieces[n];
    }
}

void toFen() {
    char fen[80], ch;
    int r, c, countEmpty = 0, index = 0;
    for (r = 0; r < 8; ++r) {
        for (c = 0; c < 8; ++c) {
            ch = grid[r][c];
            printf("%2c ", ch == 0 ? '.' : ch);
            if (ch == 0) {
                countEmpty++;
            }
            else {
                if (countEmpty > 0) {
                    fen[index++] = countEmpty + 48;
                    countEmpty = 0;
                }
                fen[index++] = ch;
            }
        }
        if (countEmpty > 0) {
            fen[index++] = countEmpty + 48;
            countEmpty = 0;
        }
        fen[index++]= '/';
        printf("\n");
    }
    strcpy(fen + index, " w - - 0 1");
    printf("%s\n", fen);
}

char *createFen() {
    placeKings();
    placePieces("PPPPPPPP", TRUE);
    placePieces("pppppppp", TRUE);
    placePieces("RNBQBNR", FALSE);
    placePieces("rnbqbnr", FALSE);
    toFen();
}

int main() {
    srand(time(NULL));
    createFen();
    return 0;
}
```


{{out}}
Sample position:

```txt

 .  .  .  Q  .  .  b  .
 .  .  .  .  .  .  .  .
 R  .  .  b  .  .  r  .
 .  .  .  .  q  .  .  K
 .  .  .  .  P  .  .  .
 .  B  k  P  .  .  .  .
 .  n  .  .  .  .  .  .
 .  .  .  N  .  .  .  .
3Q2b1/8/R2b2r1/4q2K/4P3/1BkP4/1n6/3N4/ w - - 0 1

```



## C++



```cpp

#include <ctime>
#include <iostream>
#include <string>
#include <algorithm>

class chessBoard {
public:
    void generateRNDBoard( int brds ) {
        int a, b, i; char c;
        for( int cc = 0; cc < brds; cc++ ) {
            memset( brd, 0, 64 );
            std::string pieces = "PPPPPPPPNNBBRRQKppppppppnnbbrrqk";
            random_shuffle( pieces.begin(), pieces.end() );

            while( pieces.length() ) {
                i = rand() % pieces.length(); c = pieces.at( i );
                while( true ) {
                    a = rand() % 8; b = rand() % 8;
                    if( brd[a][b] == 0 ) {
                        if( c == 'P' && !b || c == 'p' && b == 7 ||
                          ( ( c == 'K' || c == 'k' ) && search( c == 'k' ? 'K' : 'k', a, b ) ) ) continue;
                        break;
                    }
                }
                brd[a][b] = c;
                pieces = pieces.substr( 0, i ) + pieces.substr( i + 1 );
            }
            print();
        }
    }
private:
    bool search( char c, int a, int b ) {
        for( int y = -1; y < 2; y++ ) {
            for( int x = -1; x < 2; x++ ) {
                if( !x && !y ) continue;
                if( a + x > -1 && a + x < 8 && b + y >-1 && b + y < 8 ) {
                    if( brd[a + x][b + y] == c ) return true;
                }
            }
        }
        return false;
    }
    void print() {
        int e = 0;
        for( int y = 0; y < 8; y++ ) {
            for( int x = 0; x < 8; x++ ) {
                if( brd[x][y] == 0 ) e++;
                else {
                    if( e > 0 ) { std::cout << e; e = 0; }
                    std::cout << brd[x][y];
                }
            }
            if( e > 0 ) { std::cout << e; e = 0; }
            if( y < 7 ) std::cout << "/";
        }
        std::cout << " w - - 0 1\n\n";

        for( int y = 0; y < 8; y++ ) {
            for( int x = 0; x < 8; x++ ) {
                if( brd[x][y] == 0 ) std::cout << ".";
                else std::cout << brd[x][y];
            }
            std::cout << "\n";
        }

        std::cout << "\n\n";
    }
    char brd[8][8];
};
int main( int argc, char* argv[] ) {
    srand( ( unsigned )time( 0 ) );
    chessBoard c;
    c.generateRNDBoard( 2 );
    return 0;
}

```

{{out}}

```txt

1R6/2bnQP1K/br1N1BP1/nPkp1P2/2p1P1P1/4Ppqp/p1r1ppp1/1PNR3B w - - 0 1
.R......
..bnQP.K
br.N.BP.
nPkp.P..
..p.P.P.
....Ppqp
p.r.ppp.
.PNR...B


1n1k2bp/1PppQpb1/N1p4p/1B2P1K1/1RB2P2/pPR1Np2/P1r1rP1P/P2q3n w - - 0 1
.n.k..bp
.PppQpb.
N.p....p
.B..P.K.
.RB..P..
pPR.Np..
P.r.rP.P
P..q...n

```



## Crystal

{{trans|Ruby}}

```ruby
def hasNK(board, a, b)
    (-1..1).each do |g|
        (-1..1).each do |f|
            aa = a + f; bb = b + g
            if (0..7).includes?(aa) && (0..7).includes?(bb)
                p = board[aa + 8 * bb]
                return true if p == "K" || p == "k"
            end
        end
    end
    return false
end

def generateBoard(board, pieces)
    pieces.each_char do |p|
        while true
            a = rand(8); b = rand(8)
            next  if ( (b == 0 || b == 7) && (p == "P" || p == "p") ) ||
               ( (p == "k" || p == "K") && hasNK(board, a, b) )
            break if board[a + b * 8] == '.'
        end
        board[a + b * 8] = p
    end
end

pieces = "ppppppppkqrrbbnnPPPPPPPPKQRRBBNN"
11.times do
    e = pieces.size - 1
    while e > 0
        p = rand(e); t = pieces[e]
        #pieces[e] = pieces[p]; pieces[p] = t; e -= 1 # in Ruby
        pieces = pieces.sub(e, pieces[p])             # in Crystal because
        pieces = pieces.sub(p, t); e -= 1             # strings immutable
    end
end

# No 'nil' for Crystal arrays; use '.' for blank value
board = Array.new(64, '.'); generateBoard(board, pieces)
puts
e = 0
8.times do |j| row_j = j * 8
    8.times do |i|
        board[row_j + i ] == '.' ? (e += 1) :
            ( (print(e); e = 0) if e > 0
            print board[row_j + i] )
    end
    (print(e); e = 0) if e > 0
    print("/") if j < 7
end

print(" w - - 0 1\n")
8.times do |j| row_j = j * 8
  8.times { |i| board[row_j + i] == '.' ? print(".") : print(board[row_j + i]) }
  puts
end

# Simpler for same output
8.times{ |row| puts board[row*8..row*8 + 7].join("") }
```

{{out}}
```txt
1n1P3p/1p1k2Pp/1ppPPprn/2rP4/KNRQ2b1/2Bp1PP1/3qPBN1/2Rp4 w - - 0 1
.n.P...p
.p.k..Pp
.ppPPprn
..rP....
KNRQ..b.
..Bp.PP.
...qPBN.
..Rp....

```



## Factor


```factor
USING: combinators.short-circuit grouping io kernel math
math.parser math.ranges math.vectors prettyprint random
sequences sets splitting.monotonic strings ;
IN: rosetta-code.random-chess-position

<PRIVATE

CONSTANT: pieces "RNBQBNRPPPPPPPPrnbqbnrpppppppp"
CONSTANT: empty CHAR: .

: <empty-board> ( -- seq ) 64 [ empty ] "" replicate-as ;
: empty-index ( seq -- n ) empty swap indices random ;
: place ( seq elt n -- seq' ) rot [ set-nth ] keep ;

! return a list of indices that are adjacent to n
: adj ( n -- seq )
    [ 1 - ] [ 1 + ] bi [a,b] { 8 8 8 } [ v- ] 2keep dupd v+
    append append ;

: rand-non-adjacent ( m -- n ) 64 <iota> swap adj diff random ;

: place-kings ( seq -- seq' )
    CHAR: K over empty-index [ place ] keep [ CHAR: k ] dip
    rand-non-adjacent place ;

: non-pawn ( seq elt -- seq' ) over empty-index place ;

! prevent placing of pawns in ranks 1 and 8
: pawn ( seq elt -- seq' )
    over empty swap indices
    [ { [ 7 > ] [ 56 < ] } 1&& ] filter random place ;

: place-piece ( seq -- seq' )
    pieces random dup "Pp" member? [ pawn ] [ non-pawn ] if ;

PRIVATE>


: position ( -- seq )
    <empty-board> place-kings 30 random [ place-piece ] times ;

: position. ( seq -- )
    [ 1string ] { } map-as 8 group simple-table. ;

: position>fen ( seq -- seq' )
    8 group [
        [ = ] monotonic-split
        [ dup first empty = [ length number>string ] when ]
        map concat
    ] map "/" join "/ w - - 0 1" append ;

: random-chess-position-demo ( -- )
    position [ position. ] [ position>fen print ] bi ;

MAIN: random-chess-position-demo
```

{{out}}

```txt

. . . . . . . .
. p . . . . . .
. . . Q R . . .
. . . . . . k .
. P . . b . . .
. . P . . . K .
. . . . . . b .
. . . . . . r .
8/1p6/3QR3/6k1/1P2b3/2P3K1/6b1/6r1/ w - - 0 1

```



## FreeBASIC

{{trans|Yabasic}}

```freebasic

Dim Shared As Byte grid(8, 8), r, c

Sub placeKings()
    Dim As Byte r1, r2, c1, c2

    Do
        r1 = Int(Rnd*8)
        c1 = Int(Rnd*8)
        r2 = Int(Rnd*8)
        c2 = Int(Rnd*8)
        If (r1 <> r2 And Abs(r1 - r2) > 1 And Abs(c1 - c2) > 1) Then
            grid(r1, c1) = Asc("K")
            grid(r2, c2) = Asc("k")
            Return
        End If
    Loop
End Sub

Sub placePieces(pieces As String, isPawn As Byte)
    Dim numToPlace As Byte = Int(Rnd*(Len(pieces)))

    For n As Byte = 0 To numToPlace-1
        Do
            r = Int(Rnd*8)
            c = Int(Rnd*8)
        Loop Until(Not(grid(r, c) Or (isPawn And (r = 7 Or r = 0))))
        grid(r, c) = Asc(Mid(pieces, n, 1))
    Next n
End Sub

Sub toFen()
    Dim As Byte ch, countEmpty = 0
    Dim As String fen

    For r = 0 To 8-1
        For c = 0 To 8-1
            ch = grid(r, c)
            If ch <> 0 Then
                Print " " & Chr(ch);
            Else
                Print " .";
            End If
            If ch = 0 Then
                countEmpty += 1
            Else
                If countEmpty > 0 Then
                    fen += Chr(countEmpty + 48)
                    countEmpty = 0
                End If
                fen += Chr(ch)
            End If
        Next c
        If countEmpty > 0 Then
            fen += Chr(countEmpty + 48)
            countEmpty = 0
        End If
        fen += "/"
        Print
    Next r
    fen += " w - - 0 1"
    Print fen
End Sub

Randomize Timer
placeKings()
placePieces("PPPPPPPP", True)
placePieces("pppppppp", True)
placePieces("RNBQBNR", False)
placePieces("rnbqbnr", False)
toFen()
Sleep

```

{{out}}

```txt

 . . . . . . . .
 . . . . . . p .
 . . k P . . . .
 . . . . . b . .
 p . p . . p K .
 . Q . . p . r .
 . q . . . . . .
 . . . N . B n .
8/6p1/2kP4/5b2/p1p2pK1/1Q2p1r1/1q6/3N1Bn1/ w - - 0 1

```



## Go

{{trans|Java}}

```go
package main

import (
    "fmt"
    "math/rand"
    "strconv"
    "strings"
    "time"
)

var grid [8][8]byte

func abs(i int) int {
    if i >= 0 {
        return i
    } else {
        return -i
    }
}

func createFen() string {
    placeKings()
    placePieces("PPPPPPPP", true)
    placePieces("pppppppp", true)
    placePieces("RNBQBNR", false)
    placePieces("rnbqbnr", false)
    return toFen()
}

func placeKings() {
    for {
        r1 := rand.Intn(8)
        c1 := rand.Intn(8)
        r2 := rand.Intn(8)
        c2 := rand.Intn(8)
        if r1 != r2 && abs(r1-r2) > 1 && abs(c1-c2) > 1 {
            grid[r1][c1] = 'K'
            grid[r2][c2] = 'k'
            return
        }
    }
}

func placePieces(pieces string, isPawn bool) {
    numToPlace := rand.Intn(len(pieces))
    for n := 0; n < numToPlace; n++ {
        var r, c int
        for {
            r = rand.Intn(8)
            c = rand.Intn(8)
            if grid[r][c] == '\000' && !(isPawn && (r == 7 || r == 0)) {
                break
            }
        }
        grid[r][c] = pieces[n]
    }
}

func toFen() string {
    var fen strings.Builder
    countEmpty := 0
    for r := 0; r < 8; r++ {
        for c := 0; c < 8; c++ {
            ch := grid[r][c]
            if ch == '\000' {
                ch = '.'
            }
            fmt.Printf("%2c ", ch)
            if ch == '.' {
                countEmpty++
            } else {
                if countEmpty > 0 {
                    fen.WriteString(strconv.Itoa(countEmpty))
                    countEmpty = 0
                }
                fen.WriteByte(ch)
            }
        }
        if countEmpty > 0 {
            fen.WriteString(strconv.Itoa(countEmpty))
            countEmpty = 0
        }
        fen.WriteString("/")
        fmt.Println()
    }
    fen.WriteString(" w - - 0 1")
    return fen.String()
}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println(createFen())
}
```


{{out}}
Sample position:

```txt

 .  q  .  .  .  .  .  .
 .  .  .  p  .  .  .  K
 .  .  .  .  .  .  .  b
 .  r  .  b  .  .  .  .
 .  .  .  k  .  .  .  .
 .  .  .  .  .  .  p  .
 .  .  .  .  .  .  .  .
 .  n  n  .  .  .  .  .
1q6/3p3K/7b/1r1b4/3k4/6p1/8/1nn5/ w - - 0 1

```



## J


Implementation:


```J
getlayout=:3 :0
  whilst. NB. first two positions are non-adjacent kings
    (0{pos) e. (1{pos)+(,-)1 7 8 9
  do.
    pos=: y?64
  end.
)

randboard=:3 :0
  n=: ?30  NB. number of non-king pieces on board
  layout=: getlayout 2+n   NB. where they go
  white=: 0 1,?n#2         NB. which ones are white?
  pawns=: 0 0,?n#2             NB. where are the pawns?
  pawns=: pawns * 1- white*layout e.56+i.8
  pawns=: pawns * 1-(1-white)*layout e.i.8
  ptyp=: 'pkqbjnPKQBJN'{~(6*white)+1 1,(1-2}.pawns)*2+?n#4
  8 8$ptyp layout}64#'.'
)

NB. fen compress a line
fen1=:3 :0
  for_n.8-i.8 do.
    y=. y rplc (#&'.';":) n
  end.
)

NB. translate 8x8 board to fen notation
NB. (just the task specific crippled fen)
b2fen=:3 :0
  (}.;|.<@(('/',fen1)"1) y),' w - - 0 1'
)

randfen=:b2fen@randboard
```


Example use:


```J
   randfen''
q6J/1pb1p1p1/1Jq2k2/4p1Pp/1pP5/1K2Bp2/5p2/1P4P1 w - - 0 1
   randfen''
1Q3Q2/1Kn5/1PP5/2P5/2kNq1J1/7J/1P6/8 w - - 0 1
   randfen''
8/8/7K/8/8/8/k7/8 w - - 0 1
   randfen''
p4n2/1Q6/8/8/8/p4k1K/8/P5Qn w - - 0 1
   randfen''
bk1q3J/8/1N4K1/N3P3/3BQ3/j1P3P1/7P/5jq1 w - - 0 1
   randfen''
b1Q1Bb1J/j1N1PpK1/PpB1P1Pp/2pp2PN/3nnk2/3J4/P6P/1N2Qb1J w - - 0 1

```



## Java


```java
import static java.lang.Math.abs;
import java.util.Random;

public class Fen {
    static Random rand = new Random();

    public static void main(String[] args) {
        System.out.println(createFen());
    }

    static String createFen() {
        char[][] grid = new char[8][8];

        placeKings(grid);
        placePieces(grid, "PPPPPPPP", true);
        placePieces(grid, "pppppppp", true);
        placePieces(grid, "RNBQBNR", false);
        placePieces(grid, "rnbqbnr", false);

        return toFen(grid);
    }

    static void placeKings(char[][] grid) {
        int r1, c1, r2, c2;
        while (true) {
            r1 = rand.nextInt(8);
            c1 = rand.nextInt(8);
            r2 = rand.nextInt(8);
            c2 = rand.nextInt(8);
            if (r1 != r2 && abs(r1 - r2) > 1 && abs(c1 - c2) > 1)
                break;
        }
        grid[r1][c1] = 'K';
        grid[r2][c2] = 'k';
    }

    static void placePieces(char[][] grid, String pieces, boolean isPawn) {
        int numToPlace = rand.nextInt(pieces.length());
        for (int n = 0; n < numToPlace; n++) {
            int r, c;
            do {
                r = rand.nextInt(8);
                c = rand.nextInt(8);

            } while (grid[r][c] != 0 || (isPawn && (r == 7 || r == 0)));

            grid[r][c] = pieces.charAt(n);
        }
    }

    static String toFen(char[][] grid) {
        StringBuilder fen = new StringBuilder();
        int countEmpty = 0;
        for (int r = 0; r < 8; r++) {
            for (int c = 0; c < 8; c++) {
                char ch = grid[r][c];
                System.out.printf("%2c ", ch == 0 ? '.' : ch);
                if (ch == 0) {
                    countEmpty++;
                } else {
                    if (countEmpty > 0) {
                        fen.append(countEmpty);
                        countEmpty = 0;
                    }
                    fen.append(ch);
                }
            }
            if (countEmpty > 0) {
                fen.append(countEmpty);
                countEmpty = 0;
            }
            fen.append("/");
            System.out.println();
        }
        return fen.append(" w - - 0 1").toString();
    }
}
```


```txt
(lol, the black king is in check, it couldn't possibly be white's turn)
 .  .  .  .  .  B  .  .
 P  .  P  .  .  .  .  .
 .  .  k  .  R  .  .  .
 p  .  P  p  .  .  .  .
 .  .  .  .  .  .  .  Q
 P  .  .  .  .  .  .  .
 .  N  .  .  r  N  P  .
 .  .  .  .  .  .  B  K
5B2/P1P5/2k1R3/p1Pp4/7Q/P7/1N2rNP1/6BK/ w - - 0 1
```



## Julia

{{trans|Kotlin}}

'''Module''':

```julia
module Chess

using Printf

struct King end
struct Pawn end

function placepieces!(grid, ::King)
    axis = axes(grid, 1)
    while true
        r1, c1, r2, c2 = rand(axis, 4)
        if r1 != r2 && abs(r1 - r2) > 1 && abs(c1 - c2) > 1
            grid[r1, c1] = '♚'
            grid[r2, c2] = '♔'
            return grid
        end
    end
end

function placepieces!(grid, ch)
    axis = axes(grid, 1)
    while true
        r, c = rand(axis, 2)
        if grid[r, c] == ' '
            grid[r, c] = ch
            return grid
        end
    end
end

function placepieces!(grid, ch, ::Pawn)
    axis = axes(grid, 1)
    while true
        r, c = rand(axis, 2)
        if grid[r, c] == ' ' && r ∉ extrema(axis)
            grid[r, c] = ch
            return grid
        end
    end
end

function randposition!(grid)
    placepieces!(grid, King())
    foreach("♙♙♙♙♙♙♙♙♙♟♟♟♟♟♟♟♟") do ch
        placepieces!(grid, ch, Pawn())
    end
    foreach("♖♘♗♕♗♘♖♜♞♝♛♝♞♜") do ch
        placepieces!(grid, ch)
    end
    return grid
end

printgrid(grid) = println(join((join(grid[r, :], ' ') for r in 1:size(grid, 1)), '\n'))

end  # module Chess
```


'''Main''':

```julia
grid = fill(' ', 8, 8)
Chess.randposition!(grid)
Chess.printgrid(grid)
```


{{out}}

```txt
    ♕     ♞ ♗
♖ ♙ ♔ ♙ ♖ ♜   ♙
      ♟   ♙ ♙ ♜
♘ ♝ ♙ ♙ ♟
  ♙     ♚   ♝ ♟
    ♟ ♟     ♗ ♟
♙     ♞   ♟   ♟
        ♘   ♛
```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

import java.util.Random
import kotlin.math.abs

val rand = Random()

val grid = List(8) { CharArray(8) }

const val NUL = '\u0000'

fun createFen(): String {
    placeKings()
    placePieces("PPPPPPPP", true)
    placePieces("pppppppp", true)
    placePieces("RNBQBNR", false)
    placePieces("rnbqbnr", false)
    return toFen()
}

fun placeKings() {
    while (true) {
        val r1 = rand.nextInt(8)
        val c1 = rand.nextInt(8)
        val r2 = rand.nextInt(8)
        val c2 = rand.nextInt(8)
        if (r1 != r2 && abs(r1 - r2) > 1 && abs(c1 - c2) > 1) {
            grid[r1][c1] = 'K'
            grid[r2][c2] = 'k'
            return
        }
    }
}

fun placePieces(pieces: String, isPawn: Boolean) {
    val numToPlace = rand.nextInt(pieces.length)
    for (n in 0 until numToPlace) {
        var r: Int
        var c: Int
        do {
            r = rand.nextInt(8)
            c = rand.nextInt(8)
        }
        while (grid[r][c] != NUL || (isPawn && (r == 7 || r == 0)))
        grid[r][c] = pieces[n]
    }
}

fun toFen(): String {
    val fen = StringBuilder()
    var countEmpty = 0
    for (r in 0..7) {
        for (c in 0..7) {
            val ch = grid[r][c]
            print ("%2c ".format(if (ch == NUL) '.' else ch))
            if (ch == NUL) {
                countEmpty++
            }
            else {
                if (countEmpty > 0) {
                    fen.append(countEmpty)
                    countEmpty = 0
                }
                fen.append(ch)
            }
        }
        if (countEmpty > 0) {
            fen.append(countEmpty)
            countEmpty = 0
        }
        fen.append("/")
        println()
    }
    return fen.append(" w - - 0 1").toString()
}

fun main(args: Array<String>) {
    println(createFen())
}
```


Sample output:

```txt

 .  .  .  .  .  .  .  .
 K  .  .  .  .  .  .  .
 .  P  p  .  b  .  P  P
 .  p  P  .  .  .  P  .
 .  p  .  q  .  p  P  P
 .  .  .  k  .  .  .  b
 .  .  .  n  p  .  .  .
 .  .  r  .  .  .  .  .
8/K7/1Pp1b1PP/1pP3P1/1p1q1pPP/3k3b/3np3/2r5/ w - - 0 1

```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';
use utf8;
use List::AllUtils <shuffle any natatime>;

sub pick1 { return @_[rand @_] }

sub gen_FEN {
    my $n = 1 + int rand 31;
    my @n = (shuffle(0 .. 63))[1 .. $n];

    my @kings;

  KINGS: {
    for my $a (@n) {
        for my $b (@n) {
            next unless $a != $b && abs(int($a/8) - int($b/8)) > 1 || abs($a%8 - $b%8) > 1;
            @kings = ($a, $b);
            last KINGS;
        }
        die 'No good place for kings!';
    }
    }

    my ($row, @pp);
    my @pieces = <p P n N b B r R q Q>;
    my @k      = rand() < .5 ? <K k> : <k K>;

    for my $sq (0 .. 63) {
        if (any { $_ == $sq } @kings) {
            push @pp, shift @k;
        }
        elsif (any { $_ == $sq } @n) {
            $row = 7 - int $sq / 8;
            push @pp,
                $row == 0 ? pick1(grep { $_ ne 'P' } @pieces)
              : $row == 7 ? pick1(grep { $_ ne 'P' } @pieces)
              :             pick1(@pieces);
        }
        else {
            push @pp, 'ø';
        }
    }

    my @qq;
    my $iter = natatime 8, @pp;
    while (my $row = join '', $iter->()) {
        $row =~ s/((ø)\2*)/length($1)/eg;
        push @qq, $row;
    }
    return join('/', @qq) . ' w - - 0 1';
}

say gen_FEN();
```

{{out}}

```txt
p5Nq/1nn5/3N2bp/PRBkQr2/1QB1Pn2/Q5pK/1NRb2rN/p1R2r1N w - - 0 1
```



## Perl 6


```perl6
sub pick-FEN {
    # First we chose how many pieces to place
    my $n = (2..32).pick;

    # Then we pick $n squares
    my @n = (^64).pick($n);

    # We try to find suitable king positions on non-adjacent squares.
    # If we could not find any, we return recursively
    return pick-FEN() unless
    my @kings[2] = first -> [$a, $b] {
        $a !== $b && abs($a div 8 - $b div 8) | abs($a mod 8 - $b mod 8) > 1
    }, (@n X @n);

    # We make a list of pieces we can pick (apart from the kings)
    my @pieces = <p P n N b B r R q Q>;

    # We make a list of two king symbols to pick randomly a black or white king
    my @k = <K k>.pick(*);

    return (gather for ^64 -> $sq {
        if $sq == @kings.any { take @k.shift }
        elsif $sq == @n.any {
            my $row = 7 - $sq div 8;
            take
            $row == 7 ?? @pieces.grep(none('P')).pick !!
            $row == 0 ?? @pieces.grep(none('p')).pick !!
            @pieces.pick;
        }
        else { take 'ø' }
    }).rotor(8)».join».subst(/ø+/,{ .chars }, :g).join('/') ~ ' w - - 0 1';
}

say pick-FEN();
```

{{out}}

```txt
q2n1n2/1Qpk3Q/1r3bP1/1b1b4/2pRBR2/4P1bN/2R3K1/N1r2rPB w - - 0 1
```



## Phix


```Phix
constant show_bad_boards = false

string board

function fen()
    string res = ""
    for i=1 to 64 by 8 do
        integer empty = 0
        for j=i to i+7 do
            if board[j]='.' then
                empty += 1
            else
                if empty then
                    res &= empty+'0'
                    empty = 0
                end if
                res &= board[j]
            end if
        end for
        if empty then
            res &= empty+'0'
        end if
        if i<57 then res &= '/' end if
    end for
    res &= " w - - 0 1"
    return res
end function

function p15()
    string res = "pppppppprrnnbbq"
    -- promote 0..8 pawns
    for i=1 to rand(9)-1 do
        res[i] = res[rand(7)+8]
    end for
    res = shuffle(res)
    return res
end function

function kings_adjacent(sequence p12)
    integer {p1,p2} = sq_sub(p12,1),
            row_diff = abs(floor(p1/8)-floor(p2/8)),
            col_diff = abs(mod(p1,8)-mod(p2,8))
    return row_diff<=1 and col_diff<=1
end function

integer lp
procedure show_board()
    printf(1,"%d pieces\n%s",{lp,join_by(board,1,8,"")})
end procedure

while true do
    string pieces = "Kk"&                        -- kings
                    upper(p15())[1..rand(16)-1]& -- white
                    lower(p15())[1..rand(16)-1]  -- black
    lp = length(pieces)
    sequence p = tagset(64)
    p = shuffle(p)
    board = repeat('.',64)
    for i=1 to lp do board[p[i]] = pieces[i] end for

    if kings_adjacent(p[1..2]) then
        if show_bad_boards then show_board() end if
        puts(1,"kings adjacent - reshuffle\n\n")
    else
        -- check no pawn will be on a promotion square,
        -- and (above spec) no pawn has gone backwards:
        if find('p',lower(board[1..8]&board[57..64]))=0 then exit end if
        if show_bad_boards then show_board() end if
        puts(1,"pawn on rank 1/8 - reshuffle\n\n")
    end if
end while
show_board()
printf(1,"\n%s\n",{fen()})
```

To allow pawns that have "moved backwards", replace the inner test with
<code>if not find('P',board[1..8]) and not find('p',board[57..64]) then exit end if</code>

{{out}}
with show_bad_boards set to true

```txt

28 pieces
nR..rprR
kp....Rp
BQ..B.B.
b..B...b
.Kq.B.b.
....r.n.
ppN..r..
.P......
pawn on rank 1/8 - reshuffle

28 pieces
pnp...K.
.q.....k
b.P....p
P..P..bp
pPN.P.P.
.BP..rRN
QR....bN
....n...
kings adjacent - reshuffle

10 pieces
......K.
b......q
...nn...
.....k..
........
..Q..n..
..b..r..
........

6K1/b6q/3nn3/5k2/8/2Q2n2/2b2r2/8 w - - 0 1

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")
(seed (in "/dev/urandom" (rd 8)))
(de pieceN (P)
   (case P
      ("p" 8)
      ("q" 1)
      (T 2) ) )
(de pieceset ()
   (let C 0
      (make
         (for P (conc (cons "p") (shuffle '("r" "n" "b" "q")))
            (let (X (pieceN P)  N (rand 0 (+ X C)))
               (do N
                  (link P) )
               (if (= P "p")
                  (and
                     (> X N)
                     (inc 'C (- X N)) )
                  (and
                     (> N X)
                     (dec 'C (- N X)) ) ) ) ) ) ) )
(de neib (A)
   (let
      (X (/ (dec A) 8)
         Y (% (dec A) 8) )
      (filter
         '((N)
            (and
               (<= 1 N 64)
               (<=
                  (abs (- (/ (dec N) 8) X))
                  1 )
               (<=
                  (abs (- (% (dec N) 8) Y))
                  1 ) ) )
         (mapcar
            '((B) (+ B A))
            (-9 -8 -7 -1 1 7 8 9) ) ) ) )
(setq *B (need 64 "."))
(setq *P (conc (pieceset) (mapcar uppc (pieceset)) (cons "K")))
(for P *P
   (loop
      (T
         (=
            "."
            (car
               (setq @@
                  (nth
                     *B
                     (apply
                        rand
                        (if (or (= "p" P) (= "P" P))
                           (9 56)
                           (1 64) ) ) ) ) ) )
         (set @@ P) ) ) )
(loop
   (T
      (and
         (=
            "."
            (car
               (setq @@ (nth *B (setq @@@ (rand 1 64)))) ) )
         (fully
            '((N) (<> "K" (get *B N)))
            (neib @@@) ) )
      (set @@ "k") ) )
(setq *FEN
   (make
      (while *B
         (let (C 0  Lst (cut 8 '*B))
            (prinl Lst)
            (link
               (make
                  (for L Lst
                     (if (= L ".")
                        (inc 'C)
                        (and
                           (gt0 C)
                           (link (swap 'C 0)) )
                        (link L) ) )
                  (and
                     (gt0 C)
                     (link C) ) ) ) ) ) ) )
(println (pack (glue "/" *FEN) " w - - 0 1"))
```

{{out}}

```txt

...n...B
.p.BQ.B.
B..P..Br
p......p
r.PpBrrk
...B.P..
...r..K.
......r.
"3n3B/1p1BQ1B1/B2P2Br/p6p/r1PpBrrk/3B1P2/3r2K1/6r1 w - - 0 1"

```



## Python


```python

import random

board = [[" " for x in range(8)] for y in range(8)]
piece_list = ["R", "N", "B", "Q", "P"]


def place_kings(brd):
	while True:
		rank_white, file_white, rank_black, file_black = random.randint(0,7), random.randint(0,7), random.randint(0,7), random.randint(0,7)
		diff_list = [abs(rank_white - rank_black),  abs(file_white - file_black)]
		if sum(diff_list) > 2 or set(diff_list) == set([0, 2]):
			brd[rank_white][file_white], brd[rank_black][file_black] = "K", "k"
			break

def populate_board(brd, wp, bp):
	for x in range(2):
		if x == 0:
			piece_amount = wp
			pieces = piece_list
		else:
			piece_amount = bp
			pieces = [s.lower() for s in piece_list]
		while piece_amount != 0:
			piece_rank, piece_file = random.randint(0, 7), random.randint(0, 7)
			piece = random.choice(pieces)
			if brd[piece_rank][piece_file] == " " and pawn_on_promotion_square(piece, piece_rank) == False:
				brd[piece_rank][piece_file] = piece
				piece_amount -= 1

def fen_from_board(brd):
	fen = ""
	for x in brd:
		n = 0
		for y in x:
			if y == " ":
				n += 1
			else:
				if n != 0:
					fen += str(n)
				fen += y
				n = 0
		if n != 0:
			fen += str(n)
		fen += "/" if fen.count("/") < 7 else ""
	fen += " w - - 0 1\n"
	return fen

def pawn_on_promotion_square(pc, pr):
	if pc == "P" and pr == 0:
		return True
	elif pc == "p" and pr == 7:
		return True
	return False


def start():
	piece_amount_white, piece_amount_black = random.randint(0, 15), random.randint(0, 15)
	place_kings(board)
	populate_board(board, piece_amount_white, piece_amount_black)
	print(fen_from_board(board))
	for x in board:
		print(x)

#entry point
start()

```

{{out}}
```txt

1p5k/1B4P1/1KN4R/nR3Nrb/Q1NrnpP1/7R/3PP3/1P5P w - - 0 1

[' ', 'p', ' ', ' ', ' ', ' ', ' ', 'k']
[' ', 'B', ' ', ' ', ' ', ' ', 'P', ' ']
[' ', 'K', 'N', ' ', ' ', ' ', ' ', 'R']
['n', 'R', ' ', ' ', ' ', 'N', 'r', 'b']
['Q', ' ', 'N', 'r', 'n', 'p', 'P', ' ']
[' ', ' ', ' ', ' ', ' ', ' ', ' ', 'R']
[' ', ' ', ' ', 'P', 'P', ' ', ' ', ' ']
[' ', 'P', ' ', ' ', ' ', ' ', ' ', 'P']

```



## REXX


This REXX version normally generates balanced number of pieces   (both sides have an equal number of total pieces,

but not necessarily of the same kind).

If the number of chessboards specified is negative, then the number of chess pieces for each side will be random.

This version also allows any number of chessboards to be displayed.

```rexx
/*REXX program generates a  chess position (random pieces & positions)  in a FEN format.*/
parse arg seed CBs .                             /*obtain optional arguments from the CL*/
if datatype(seed,'W')  then call random ,,seed   /*SEED given for RANDOM repeatability? */
if CBs=='' | CBs==","  then CBs=1                /*CBs:  number of generated ChessBoards*/
                                                 /* [↓]  maybe display any # of boards. */
      do boards=1  for abs(CBs)                  /* [↓]  maybe display separator & title*/
      if sign(CBs)\==CBs  then do;   say;   say center(' board' boards" ", 79, '▒');   end
      @.=.;  !.=.                                /*initialize the chessboard to be empty*/
                  do p=1  for random(2, 32)      /*generate a random number of chessmen.*/
                  if p<3  then call piece 'k'    /*a   king   of each color.            */
                          else call piece substr('bnpqr', random(1, 5), 1)
                  end  /*p*/                     /* [↑]  place a piece on the chessboard*/
      call cb                                    /*display the ChessBoard  and  its FEN.*/
      end          /*boards*/                    /* [↑]  CB ≡  ─    ─                   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cb: fen=;  do   r=8  for 8  by -1; $=                        /*the board rank  (so far).*/
             do f=8  for 8  by -1; $=$ || @.r.f;  end /*f*/  /*append the board file.   */
           say $                                             /*display the board rank.  */
             do e=8  for 8  by -1; $=changestr(copies(.,e),$,e); end  /*e*/   /*.≡filler*/
           fen=fen || $ || left('/', r\==1)                  /*append / if not 1st rank.*/
           end   /*r*/                                       /* [↑]  append $ str to FEN*/
    say                                                      /*display a blank sep. line*/
    say 'FEN='fen   "w - - 0 1"                              /*Forsyth─Edwards Notation.*/
    return                                                   /* [↑]  display chessboard.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
piece: parse arg x;    if p//2  then upper x;     arg ux         /*use white if  odd  P.*/
       if CBs<0 & p>2  then if random(1)  then upper x           /*CBs>0?  Use balanced.*/
                                                                 /*[↓]  # isn't changed.*/
         do #=0  by 0;  r=random(1, 8);   f=random(1, 8)         /*random rank and file.*/
         if @.r.f\==.   then iterate                             /*is position occupied?*/
         if (x=='p' & r==1) | (x=='P' & r==8)  then iterate      /*any promoting pawn?  */
                                                                 /*[↑]  skip these pawns*/
         if ux=='K'  then do    rr=r-1 for 3                     /*[↓]  neighbor ≡ king?*/
                             do ff=f-1 for 3;  if !.rr.ff=='K'  then iterate #   /*king?*/
                             end   /*rr*/                        /*[↑]  neighbor ≡ king?*/
                          end      /*ff*/                        /*[↑]  we're all done. */
         @.r.f= x                                                /*put random piece.    */
         !.r.f=ux;  return                                       /* "     "     "  upper*/
         end   /*#*/                                             /*#: isn't incremented.*/
```

Some older REXXes don't have a   '''changestr'''   BIF,   so one is included here:   ───►   [[CHANGESTR.REX]].


'''output'''   showing five chess positions (starting with a specific position by seeding the   '''random'''   BIF with   '''96'''),

specifying the arguments (for Regina REXX under Windows):   96   5

```txt

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ board 1 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
........
........
....K...
........
....k...
........
........
........

FEN=8/8/4K3/8/4k3/8/8/8 w - - 0 1

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ board 2 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
...Q..r.
r.kbn...
...pqn.p
..N.N.qQ
p....N.n
..N.....
...NB...
.R.Q.KBP

FEN=3Q2r1/r1kbn3/3pqn1p/2N1N1qQ/p4N1n/2N5/3NB3/1R1Q1KBP w - - 0 1

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ board 3 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
.....B.n
N...p.p.
.q..n...
.K..R...
.BP...P.
q....k..
........
R.N..n..

FEN=5B1n/N3p1p1/1q2n3/1K2R3/1BP3P1/q4k2/8/R1N2n2 w - - 0 1

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ board 4 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
......K.
pN..q...
...qN...
....n...
.....Q..
........
....k...
P.r....B

FEN=6K1/pN2q3/3qN3/4n3/5Q2/8/4k3/P1r4B w - - 0 1

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ board 5 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
qnqN...N
...B...N
n....P.Q
PP..b.Rn
KqB.....
pB.knp..
q.n.qB..
.......N

FEN=qnqN3N/3B3N/n4P1Q/PP2b1Rn/KqB5/pB1knp2/q1n1qB2/7N w - - 0 1

```



## Ruby


```ruby

def hasNK( board, a, b )
    for g in -1 .. 1
        for f in -1 .. 1
            aa = a + f; bb = b + g
            if aa.between?( 0, 7 ) && bb.between?( 0, 7 )
                p = board[aa + 8 * bb]
                if p == "K" || p == "k"; return true; end
            end
        end
    end
    return false
end
def generateBoard(  board, pieces  )
    while( pieces.length > 1 )
        p = pieces[pieces.length - 1]
        pieces = pieces[0...-1]
        while( true )
            a = rand( 8 ); b = rand( 8 )
            if ( ( b == 0 || b == 7 ) && ( p == "P" || p == "p" ) ) ||
               ( ( p == "k" || p == "K" ) && hasNK( board, a, b ) ); next; end
            if board[a + b * 8] == nil; break;end
        end
        board[a + b * 8] = p
    end
end
pieces = "ppppppppkqrrbbnnPPPPPPPPKQRRBBNN"
for i in 0 .. 10
    e = pieces.length - 1
    while e > 0
        p = rand( e ); t = pieces[e];
        pieces[e] = pieces[p]; pieces[p] = t; e -= 1
    end
end
board = Array.new( 64 ); generateBoard( board, pieces )
puts
e = 0
for j in 0 .. 7
    for i in 0 .. 7
        if board[i + 8 * j] == nil; e += 1
        else
            if e > 0; print( e ); e = 0; end
            print( board[i + 8 * j] )
        end
    end
    if e > 0; print( e ); e = 0; end
    if j < 7; print( "/" ); end
end
print( " w - - 0 1\n" )
for j in 0 .. 7
    for i in 0 .. 7
        if board[i + j * 8] == nil; print( "." )
        else print( board[i + j * 8] ); end
    end
    puts
end

```

{{out}}
```txt

1bN1RK2/1Pp1pQbP/p2PpP2/8/1pkNP3/P1r1Pp1p/Bn1RPp1r/3qB3 w - - 0 1
.bN.RK..
.Pp.pQbP
p..PpP..
........
.pkNP...
P.r.Pp.p
Bn.RPp.r
...qB...

```



## Rust


```Rust
use std::fmt::Write;

use rand::{Rng, distributions::{Distribution, Standard}};

const EMPTY: u8 = b'.';

#[derive(Clone, Debug)]
struct Board {
    grid: [[u8; 8]; 8],
}

impl Distribution<Board> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Board {
        let mut board = Board::empty();
        board.place_kings(rng);
        board.place_pieces(rng, b"PPPPPPPP", true);
        board.place_pieces(rng, b"pppppppp", true);
        board.place_pieces(rng, b"RNBQBNR", false);
        board.place_pieces(rng, b"rnbqbnr", false);
        board
    }
}

impl Board {
    fn empty() -> Self {
        Board { grid: [[EMPTY; 8]; 8] }
    }

    fn fen(&self) -> String {
        let mut fen = String::new();
        let mut count_empty = 0;
        for row in &self.grid {
            for &ch in row {
                print!("{} ", ch as char);
                if ch == EMPTY {
                    count_empty += 1;
                } else {
                    if count_empty > 0 {
                        write!(fen, "{}", count_empty).unwrap();
                        count_empty = 0;
                    }
                    fen.push(ch as char);
                }
            }
            if count_empty > 0 {
                write!(fen, "{}", count_empty).unwrap();
                count_empty = 0;
            }
            fen.push('/');
            println!();
        }
        fen.push_str(" w - - 0 1");
        fen
    }

    fn place_kings<R: Rng + ?Sized>(&mut self, rng: &mut R) {
        loop {
            let r1: i8 = rng.gen_range(0, 8);
            let c1: i8 = rng.gen_range(0, 8);
            let r2: i8 = rng.gen_range(0, 8);
            let c2: i8 = rng.gen_range(0, 8);
            if r1 != r2 && (r1 - r2).abs() > 1 && (c1 - c2).abs() > 1 {
                self.grid[r1 as usize][c1 as usize] = b'K';
                self.grid[r2 as usize][c2 as usize] = b'k';
                return;
            }
        }
    }

    fn place_pieces<R: Rng + ?Sized>(&mut self, rng: &mut R, pieces: &[u8], is_pawn: bool) {
        let num_to_place = rng.gen_range(0, pieces.len());
        for &piece in pieces.iter().take(num_to_place) {
            let mut r = rng.gen_range(0, 8);
            let mut c = rng.gen_range(0, 8);
            while self.grid[r][c] != EMPTY || (is_pawn && (r == 7 || r == 0)) {
                r = rng.gen_range(0, 8);
                c = rng.gen_range(0, 8);
            }
            self.grid[r][c] = piece;
        }
    }
}

fn main() {
    let b: Board = rand::random();
    println!("{}", b.fen());
}
```


{{out}}

```txt
b . . . . . . .
. R P . . . . .
n . . P . . . k
p . . . P r . .
P p . b . . . n
. . P . . . . .
K . . . . . . .
. . . q . . . .
b7/1RP5/n2P3k/p3Pr2/Pp1b3n/2P5/K7/3q4/ w - - 0 1
```



## Scala

{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/zdG9NQ4YSaShFRU7AS31GA Scastie (remote JVM)].

```Scala
import scala.math.abs
import scala.util.Random

object RandomFen extends App {
  val rand = new Random

  private def createFen = {
    val grid = Array.ofDim[Char](8, 8)

    def placeKings(grid: Array[Array[Char]]): Unit = {
      var r1, c1, r2, c2 = 0
      do {
        r1 = rand.nextInt(8)
        c1 = rand.nextInt(8)
        r2 = rand.nextInt(8)
        c2 = rand.nextInt(8)
      } while (r1 == r2 || abs(r1 - r2) <= 1 || abs(c1 - c2) <= 1)
      grid(r1)(c1) = 'K'
      grid(r2)(c2) = 'k'
    }

    def placePieces(grid: Array[Array[Char]],
                    pieces: String,
                    isPawn: Boolean): Unit = {
      val numToPlace = rand.nextInt(pieces.length)

      for (n <- 0 until numToPlace) {
        var r, c = 0
        do {
          r = rand.nextInt(8)
          c = rand.nextInt(8)
        } while (grid(r)(c) != 0 || (isPawn && (r == 7 || r == 0)))
        grid(r)(c) = pieces(n)
      }
    }

    def toFen(grid: Array[Array[Char]]) = {
      val fen = new StringBuilder
      var countEmpty = 0
      for (r <- grid.indices) {
        for (c <- grid.indices) {
          val ch = grid(r)(c)
          print(f"${if (ch == 0) '.' else ch}%2c ")
          if (ch == 0) countEmpty += 1
          else {
            if (countEmpty > 0) {
              fen.append(countEmpty)
              countEmpty = 0
            }
            fen.append(ch)
          }
        }
        if (countEmpty > 0) {
          fen.append(countEmpty)
          countEmpty = 0
        }
        fen.append("/")
        println()
      }
      fen.append(" w - - 0 1").toString
    }

    placeKings(grid)
    placePieces(grid, "PPPPPPPP", isPawn = true)
    placePieces(grid, "pppppppp", isPawn = true)
    placePieces(grid, "RNBQBNR", isPawn = false)
    placePieces(grid, "rnbqbnr", isPawn = false)
    toFen(grid)
  }

  println(createFen)

}
```



## Yabasic

{{trans|C}}

```Yabasic
dim grid(8, 8)

sub placeKings()
    local r1, r2, c1, c2

    do
        r1 = int(ran(8))
        c1 = int(ran(8))
        r2 = int(ran(8))
        c2 = int(ran(8))
        if (r1 <> r2 and abs(r1 - r2) > 1 and abs(c1 - c2) > 1) then
            grid(r1, c1) = asc("K")
            grid(r2, c2) = asc("k")
            return
        end if
    loop
end sub

sub placePieces(pieces$, isPawn)
    local n, r, c, numToPlace

    numToPlace = int(ran(len(pieces$)))
    for n = 0 to numToPlace-1
        repeat
            r = int(ran(8))
            c = int(ran(8))
        until(not(grid(r, c) or (isPawn and (r = 7 or r = 0))))
        grid(r, c) = asc(mid$(pieces$, n, 1))
    next
end sub

sub toFen()
    local fen$, ch, r, c, countEmpty

    for r = 0 to 8-1
        for c = 0 to 8-1
            ch = grid(r, c)
            if ch then print chr$(ch); else print "."; end if
            if not ch then
                countEmpty = countEmpty + 1
            else
                if countEmpty then
                    fen$ = fen$ + chr$(countEmpty + 48)
                    countEmpty = 0
                end if
                fen$ = fen$ + chr$(ch)
            end if
        next
        if countEmpty then
            fen$ = fen$ + chr$(countEmpty + 48)
            countEmpty = 0
        end if
        fen$ = fen$ + "/"
        print
    next
    fen$ = fen$ + " w - - 0 1"
    print fen$
end sub

sub createFen()
    placeKings()
    placePieces("PPPPPPPP", TRUE)
    placePieces("pppppppp", TRUE)
    placePieces("RNBQBNR", FALSE)
    placePieces("rnbqbnr", FALSE)
    toFen()
end sub

createFen()

```



## zkl

{{trans|perl6}}

```zkl
fcn pickFEN{
   # First we chose how many pieces to place: 2 to 32
   n := (0).random(2,33);

   # Then we pick $n squares: first n of shuffle (0,1,2,3...63)
   n = [0..63].walk().shuffle()[0,n];

   # We try to find suitable king positions on non-adjacent squares.
   # If we could not find any, we return recursively
   kings := Walker.cproduct(n,n).filter1(fcn([(a,b)]){  // Cartesian product
      a!=b and (a/8 - b/8).abs() or (a%8 - b%8).abs()>1
   }); # (a,b) on success, False on fail
   if(not kings) return(pickFEN());    // tail recursion

   # We make a list of pieces we can pick (apart from the kings)
   pieces,pnp,pnP := "p P n N b B r R q Q".split(" "), pieces-"p", pieces-"P";

   # We make a list of two king symbols to pick randomly a black or white king
   k := "K k".split(" ").shuffle();

   [0..63].apply('wrap(sq){  # look at each square
      if(kings.holds(sq)) k.pop();
      else if(n.holds(sq)){
         row,n,n2 := 7 - sq/8, (0).random(pieces.len()), (0).random(pnp.len());
	 if(     row == 7) pnP[n2]  // no white pawn in the promotion square
	 else if(row == 0) pnp[n2]  // no black pawn in the promotion square
	 else 		   pieces[n] // otherwise, any ole random piece
      }
      else "#"  // empty square
   })
   .pump(List,T(Void.Read,7),"".append,subst)  // chunkize into groups of 8 chars (1 row)
   .concat("/") + " w - - 0 1"
}
fcn subst(str){  // replace "#" with count of #s
   re :=RegExp("#+");
   while(re.search(str,1)){ n,m:=re.matched[0]; str=String(str[0,n],m,str[n+m,*]) }
   str
}
```


```zkl
do(5){ pickFEN().println() }
```

{{out}}

```txt

b3rQBr/n2b1Q1q/1R6/1BQRQ2n/RnB2r2/q3b1nQ/1BqB1bBQ/2q1Q3 w - - 0 1
b7/5qqB/qb3B2/8/3b4/Q1n3r1/3B2Q1/n6r w - - 0 1
r2R3N/Q1n5/1N6/1N1R1RBB/8/Rn1b1r2/3QbNN1/2n3rQ w - - 0 1
5b2/1R6/r4b1q/3Q4/8/8/8/5b2 w - - 0 1
8/1BQ5/8/1q6/1q6/3B1Rq1/5b2/3N3R w - - 0 1
```

