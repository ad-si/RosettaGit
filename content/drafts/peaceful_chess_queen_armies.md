+++
title = "Peaceful chess queen armies"
description = ""
date = 2019-08-28T00:06:00Z
aliases = []
[extra]
id = 22237
[taxonomies]
categories = []
tags = []
+++

{{task}}
In chess, a queen attacks positions from where it is, in straight lines up-down and left-right as well as on both its diagonals. It attacks only pieces ''not'' of its own colour.


<table style="font-weight:bold">
   <tr align="center" valign="middle">
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#8662;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x21d1;</td>
    <td style="width:16pt; height:16pt;">&#8663;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr align="center" valign="middle">
    <td style="width:16pt; height:16pt;">&#8656;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#8656;</td>
    <td style="width:16pt; height:16pt;"><font color="lime">&#x265b;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#8658;</td>
    <td style="width:16pt; height:16pt;">&#8658;</td>
</tr>
  <tr align="center" valign="middle">
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#8665;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#8659;</td>
    <td style="width:16pt; height:16pt;">&#8664;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr align="center" valign="middle">
    <td style="width:16pt; height:16pt;">&#8665;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#8659;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#8664;</td>
</tr>
  <tr align="center" valign="middle">
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#8659;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



The goal of Peaceful chess queen armies is to arrange <code>m</code> black queens and <code>m</code> white queens on an <code>n-by-n</code> square grid, (the board), so that ''no queen attacks another of a different colour''.


;Task:
# Create a routine to represent two-colour queens on a 2-D board. (Alternating black/white background colours, Unicode chess pieces and other embellishments are not necessary, but may be used at your discretion).
# Create a routine to generate at least one solution to placing <code>m</code> equal numbers of black and white queens on an <code>n</code> square board.
# Display here results for the <code>m=4, n=5</code> case.


;References:
* [http://www.mathopt.org/Optima-Issues/optima62.pdf Peaceably Coexisting Armies of Queens] (Pdf) by Robert A. Bosch. Optima, the Mathematical Programming Socity newsletter, issue 62.
* [https://oeis.org/A250000 A250000] OEIS





## D

{{trans|Go}}

```d
import std.array;
import std.math;
import std.stdio;
import std.typecons;

enum Piece {
    empty,
    black,
    white,
}

alias position = Tuple!(int, "i", int, "j");

bool place(int m, int n, ref position[] pBlackQueens, ref position[] pWhiteQueens) {
    if (m == 0) {
        return true;
    }
    bool placingBlack = true;
    foreach (i; 0..n) {
        inner:
        foreach (j; 0..n) {
            auto pos = position(i, j);
            foreach (queen; pBlackQueens) {
                if (queen == pos || !placingBlack && isAttacking(queen, pos)) {
                    continue inner;
                }
            }
            foreach (queen; pWhiteQueens) {
                if (queen == pos || placingBlack && isAttacking(queen, pos)) {
                    continue inner;
                }
            }
            if (placingBlack) {
                pBlackQueens ~= pos;
                placingBlack = false;
            } else {
                pWhiteQueens ~= pos;
                if (place(m - 1, n, pBlackQueens, pWhiteQueens)) {
                    return true;
                }
                pBlackQueens.length--;
                pWhiteQueens.length--;
                placingBlack = true;
            }
        }
    }
    if (!placingBlack) {
        pBlackQueens.length--;
    }
    return false;
}

bool isAttacking(position queen, position pos) {
    return queen.i == pos.i
        || queen.j == pos.j
        || abs(queen.i - pos.i) == abs(queen.j - pos.j);
}

void printBoard(int n, position[] blackQueens, position[] whiteQueens) {
    auto board = uninitializedArray!(Piece[])(n * n);
    board[] = Piece.empty;

    foreach (queen; blackQueens) {
        board[queen.i * n + queen.j] = Piece.black;
    }
    foreach (queen; whiteQueens) {
        board[queen.i * n + queen.j] = Piece.white;
    }
    foreach (i,b; board) {
        if (i != 0 && i % n == 0) {
            writeln;
        }
        final switch (b) {
            case Piece.black:
                write("B ");
                break;
            case Piece.white:
                write("W ");
                break;
            case Piece.empty:
                int j = i / n;
                int k = i - j * n;

                if (j % 2 == k % 2) {
                    write("• "w);
                } else {
                    write("◦ "w);
                }
                break;
        }
    }
    writeln('\n');
}

void main() {
    auto nms = [
        [2, 1], [3, 1], [3, 2], [4, 1], [4, 2], [4, 3],
        [5, 1], [5, 2], [5, 3], [5, 4], [5, 5],
        [6, 1], [6, 2], [6, 3], [6, 4], [6, 5], [6, 6],
        [7, 1], [7, 2], [7, 3], [7, 4], [7, 5], [7, 6], [7, 7],
    ];
    foreach (nm; nms) {
        writefln("%d black and %d white queens on a %d x %d board:", nm[1], nm[1], nm[0], nm[0]);
        position[] blackQueens;
        position[] whiteQueens;
        if (place(nm[1], nm[0], blackQueens, whiteQueens)) {
            printBoard(nm[0], blackQueens, whiteQueens);
        } else {
            writeln("No solution exists.\n");
        }
    }
}
```

{{out}}

```txt
1 black and 1 white queens on a 2 x 2 board:
No solution exists.

1 black and 1 white queens on a 3 x 3 board:
B ◦ • 
◦ • W 
• ◦ • 

2 black and 2 white queens on a 3 x 3 board:
No solution exists.

1 black and 1 white queens on a 4 x 4 board:
B ◦ • ◦ 
◦ • W • 
• ◦ • ◦ 
◦ • ◦ • 

2 black and 2 white queens on a 4 x 4 board:
B ◦ • ◦ 
◦ • W • 
B ◦ • ◦ 
◦ • W • 

3 black and 3 white queens on a 4 x 4 board:
No solution exists.

1 black and 1 white queens on a 5 x 5 board:
B ◦ • ◦ • 
◦ • W • ◦ 
• ◦ • ◦ • 
◦ • ◦ • ◦ 
• ◦ • ◦ • 

2 black and 2 white queens on a 5 x 5 board:
B ◦ • ◦ B 
◦ • W • ◦ 
• W • ◦ • 
◦ • ◦ • ◦ 
• ◦ • ◦ • 

3 black and 3 white queens on a 5 x 5 board:
B ◦ • ◦ B 
◦ • W • ◦ 
• W • ◦ • 
◦ • ◦ B ◦ 
• W • ◦ • 

4 black and 4 white queens on a 5 x 5 board:
• B • B • 
◦ • ◦ • B 
W ◦ W ◦ • 
◦ • ◦ • B 
W ◦ W ◦ • 

5 black and 5 white queens on a 5 x 5 board:
No solution exists.

1 black and 1 white queens on a 6 x 6 board:
B ◦ • ◦ • ◦ 
◦ • W • ◦ • 
• ◦ • ◦ • ◦ 
◦ • ◦ • ◦ • 
• ◦ • ◦ • ◦ 
◦ • ◦ • ◦ • 

2 black and 2 white queens on a 6 x 6 board:
B ◦ • ◦ B ◦ 
◦ • W • ◦ • 
• W • ◦ • ◦ 
◦ • ◦ • ◦ • 
• ◦ • ◦ • ◦ 
◦ • ◦ • ◦ • 

3 black and 3 white queens on a 6 x 6 board:
B ◦ • ◦ B B 
◦ • W • ◦ • 
• W • ◦ • ◦ 
◦ • ◦ • ◦ • 
• ◦ W ◦ • ◦ 
◦ • ◦ • ◦ • 

4 black and 4 white queens on a 6 x 6 board:
B ◦ • ◦ B B 
◦ • W • ◦ • 
• W • ◦ • ◦ 
◦ • ◦ • ◦ B 
• ◦ W W • ◦ 
◦ • ◦ • ◦ • 

5 black and 5 white queens on a 6 x 6 board:
• B • ◦ B ◦ 
◦ • ◦ B ◦ B 
W ◦ • ◦ • ◦ 
W • W • ◦ • 
• ◦ • ◦ • B 
W • W • ◦ • 

6 black and 6 white queens on a 6 x 6 board:
No solution exists.

1 black and 1 white queens on a 7 x 7 board:
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

2 black and 2 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

3 black and 3 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

4 black and 4 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

5 black and 5 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

6 black and 6 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 

7 black and 7 white queens on a 7 x 7 board:
• B • ◦ • B • 
◦ B ◦ • B • ◦ 
• B • ◦ • B • 
◦ • ◦ • B • ◦ 
W ◦ W ◦ • ◦ W 
◦ • ◦ W ◦ • ◦ 
W ◦ W W • ◦ • 
```



## Go

This is based on the C# code [https://www.reddit.com/r/coding/comments/b5eu7w/peaceful_chess_queen_armies_new_draft_rosetta/ here].

Textual rather than HTML output. Whilst the unicode symbols for the black and white queens are recognized by the Ubuntu 16.04 terminal, I found it hard to visually distinguish between them so I've used 'B' and 'W' instead.

```go
package main

import "fmt"

const (
    empty = iota
    black
    white
)

const (
    bqueen  = 'B'
    wqueen  = 'W'
    bbullet = '•'
    wbullet = '◦'
)

type position struct{ i, j int }

func iabs(i int) int {
    if i < 0 {
        return -i
    }
    return i
}

func place(m, n int, pBlackQueens, pWhiteQueens *[]position) bool {
    if m == 0 {
        return true
    }
    placingBlack := true
    for i := 0; i < n; i++ {
    inner:
        for j := 0; j < n; j++ {
            pos := position{i, j}
            for _, queen := range *pBlackQueens {
                if queen == pos || !placingBlack && isAttacking(queen, pos) {
                    continue inner
                }
            }
            for _, queen := range *pWhiteQueens {
                if queen == pos || placingBlack && isAttacking(queen, pos) {
                    continue inner
                }
            }
            if placingBlack {
                *pBlackQueens = append(*pBlackQueens, pos)
                placingBlack = false
            } else {
                *pWhiteQueens = append(*pWhiteQueens, pos)
                if place(m-1, n, pBlackQueens, pWhiteQueens) {
                    return true
                }
                *pBlackQueens = (*pBlackQueens)[0 : len(*pBlackQueens)-1]
                *pWhiteQueens = (*pWhiteQueens)[0 : len(*pWhiteQueens)-1]
                placingBlack = true
            }
        }
    }
    if !placingBlack {
        *pBlackQueens = (*pBlackQueens)[0 : len(*pBlackQueens)-1]
    }
    return false
}

func isAttacking(queen, pos position) bool {
    if queen.i == pos.i {
        return true
    }
    if queen.j == pos.j {
        return true
    }
    if iabs(queen.i-pos.i) == iabs(queen.j-pos.j) {
        return true
    }
    return false
}

func printBoard(n int, blackQueens, whiteQueens []position) {
    board := make([]int, n*n)
    for _, queen := range blackQueens {
        board[queen.i*n+queen.j] = black
    }
    for _, queen := range whiteQueens {
        board[queen.i*n+queen.j] = white
    }

    for i, b := range board {
        if i != 0 && i%n == 0 {
            fmt.Println()
        }
        switch b {
        case black:
            fmt.Printf("%c ", bqueen)
        case white:
            fmt.Printf("%c ", wqueen)
        case empty:
            if i%2 == 0 {
                fmt.Printf("%c ", bbullet)
            } else {
                fmt.Printf("%c ", wbullet)
            }
        }
    }
    fmt.Println("\n")
}

func main() {
    nms := [][2]int{
        {2, 1}, {3, 1}, {3, 2}, {4, 1}, {4, 2}, {4, 3},
        {5, 1}, {5, 2}, {5, 3}, {5, 4}, {5, 5},
        {6, 1}, {6, 2}, {6, 3}, {6, 4}, {6, 5}, {6, 6},
        {7, 1}, {7, 2}, {7, 3}, {7, 4}, {7, 5}, {7, 6}, {7, 7},
    }
    for _, nm := range nms {
        n, m := nm[0], nm[1]
        fmt.Printf("%d black and %d white queens on a %d x %d board:\n", m, m, n, n)
        var blackQueens, whiteQueens []position
        if place(m, n, &blackQueens, &whiteQueens) {
            printBoard(n, blackQueens, whiteQueens)
        } else {
            fmt.Println("No solution exists.\n")
        }
    }
}
```


{{out}}
<div style="overflow:scroll; height:250px;">

```txt

1 black and 1 white queens on a 2 x 2 board:
No solution exists.

1 black and 1 white queens on a 3 x 3 board:
B ◦ • 
◦ • W 
• ◦ • 

2 black and 2 white queens on a 3 x 3 board:
No solution exists.

1 black and 1 white queens on a 4 x 4 board:
B ◦ • ◦ 
• ◦ W ◦ 
• ◦ • ◦ 
• ◦ • ◦ 

2 black and 2 white queens on a 4 x 4 board:
B ◦ • ◦ 
• ◦ W ◦ 
B ◦ • ◦ 
• ◦ W ◦ 

3 black and 3 white queens on a 4 x 4 board:
No solution exists.

1 black and 1 white queens on a 5 x 5 board:
B ◦ • ◦ • 
◦ • W • ◦ 
• ◦ • ◦ • 
◦ • ◦ • ◦ 
• ◦ • ◦ • 

2 black and 2 white queens on a 5 x 5 board:
B ◦ • ◦ B 
◦ • W • ◦ 
• W • ◦ • 
◦ • ◦ • ◦ 
• ◦ • ◦ • 

3 black and 3 white queens on a 5 x 5 board:
B ◦ • ◦ B 
◦ • W • ◦ 
• W • ◦ • 
◦ • ◦ B ◦ 
• W • ◦ • 

4 black and 4 white queens on a 5 x 5 board:
• B • B • 
◦ • ◦ • B 
W ◦ W ◦ • 
◦ • ◦ • B 
W ◦ W ◦ • 

5 black and 5 white queens on a 5 x 5 board:
No solution exists.

1 black and 1 white queens on a 6 x 6 board:
B ◦ • ◦ • ◦ 
• ◦ W ◦ • ◦ 
• ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ 

2 black and 2 white queens on a 6 x 6 board:
B ◦ • ◦ B ◦ 
• ◦ W ◦ • ◦ 
• W • ◦ • ◦ 
• ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ 

3 black and 3 white queens on a 6 x 6 board:
B ◦ • ◦ B B 
• ◦ W ◦ • ◦ 
• W • ◦ • ◦ 
• ◦ • ◦ • ◦ 
• ◦ W ◦ • ◦ 
• ◦ • ◦ • ◦ 

4 black and 4 white queens on a 6 x 6 board:
B ◦ • ◦ B B 
• ◦ W ◦ • ◦ 
• W • ◦ • ◦ 
• ◦ • ◦ • B 
• ◦ W W • ◦ 
• ◦ • ◦ • ◦ 

5 black and 5 white queens on a 6 x 6 board:
• B • ◦ B ◦ 
• ◦ • B • B 
W ◦ • ◦ • ◦ 
W ◦ W ◦ • ◦ 
• ◦ • ◦ • B 
W ◦ W ◦ • ◦ 

6 black and 6 white queens on a 6 x 6 board:
No solution exists.

1 black and 1 white queens on a 7 x 7 board:
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

2 black and 2 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

3 black and 3 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

4 black and 4 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

5 black and 5 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

6 black and 6 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 

7 black and 7 white queens on a 7 x 7 board:
• B • ◦ • B • 
◦ B ◦ • B • ◦ 
• B • ◦ • B • 
◦ • ◦ • B • ◦ 
W ◦ W ◦ • ◦ W 
◦ • ◦ W ◦ • ◦ 
W ◦ W W • ◦ • 

```

</div>



## Julia

GUI version, uses the Gtk library. The place! function is condensed from the C# example. 

```julia
using Gtk

struct Position
    row::Int
    col::Int
end

function place!(numeach, bsize, bqueens, wqueens)
    isattack(q, pos) = (q.row == pos.row || q.col == pos.col ||
                        abs(q.row - pos.row) == abs(q.col - pos.col))
    noattack(qs, pos) = !any(x -> isattack(x, pos), qs)
    positionopen(bqs, wqs, p) = !any(x -> x == p, bqs) && !any(x -> x == p, wqs)

    placingbqueens = true
    if numeach < 1
        return true
    end
    for i in 1:bsize, j in 1:bsize
        bpos = Position(i, j)
        if positionopen(bqueens, wqueens, bpos)
            if placingbqueens && noattack(wqueens, bpos)
                push!(bqueens, bpos)
                placingbqueens = false
            elseif !placingbqueens && noattack(bqueens, bpos)
                push!(wqueens, bpos)
                if place!(numeach - 1, bsize, bqueens, wqueens)
                    return true
                end
                pop!(bqueens)
                pop!(wqueens)
                placingbqueens = true
            end
        end
    end
    if !placingbqueens
        pop!(bqueens)
    end
    false
end

function peacefulqueenapp()
    win = GtkWindow("Peaceful Chess Queen Armies", 800, 800) |> (GtkFrame() |> (box = GtkBox(:v)))
    boardsize = 5
    numqueenseach = 4
    hbox = GtkBox(:h)
    boardscale = GtkScale(false, 2:16)
    set_gtk_property!(boardscale, :hexpand, true)
    blabel = GtkLabel("Choose Board Size")
    nqueenscale = GtkScale(false, 1:24)
    set_gtk_property!(nqueenscale, :hexpand, true)
    qlabel = GtkLabel("Choose Number of Queens Per Side")
    solveit = GtkButton("Solve")
    set_gtk_property!(solveit, :label, "   Solve   ")
    solvequeens(wid) = (boardsize = Int(GAccessor.value(boardscale));
        numqueenseach = Int(GAccessor.value(nqueenscale)); update!())
    signal_connect(solvequeens, solveit, :clicked)
    map(w->push!(hbox, w),[blabel, boardscale, qlabel, nqueenscale, solveit])
    scrwin = GtkScrolledWindow()
    grid = GtkGrid()
    push!(scrwin, grid)
    map(w -> push!(box, w),[hbox, scrwin])
    piece = (white = "\u2655", black = "\u265B", blank = "   ")
    stylist = GtkStyleProvider(Gtk.CssProviderLeaf(data="""
        label {background-image: image(cornsilk); font-size: 48px;}
        button {background-image: image(tan); font-size: 48px;}"""))

    function update!()
        bqueens, wqueens = Vector{Position}(), Vector{Position}()
        place!(numqueenseach, boardsize, bqueens, wqueens)
        if length(bqueens) == 0
            warn_dialog("No solution for board size $boardsize and $numqueenseach queens each.", win)
            return
        end
        empty!(grid)
        labels = Array{Gtk.GtkLabelLeaf, 2}(undef, (boardsize, boardsize))
        buttons = Array{GtkButtonLeaf, 2}(undef, (boardsize, boardsize))
        for i in 1:boardsize, j in 1:boardsize
            if isodd(i + j)
                grid[i, j] = buttons[i, j] = GtkButton(piece.blank)
                set_gtk_property!(buttons[i, j], :expand, true)
                push!(Gtk.GAccessor.style_context(buttons[i, j]), stylist, 600)
            else
                grid[i, j] = labels[i, j] = GtkLabel(piece.blank)
                set_gtk_property!(labels[i, j], :expand, true)
                push!(Gtk.GAccessor.style_context(labels[i, j]), stylist, 600)
            end
            pos = Position(i, j)
            if pos in bqueens
                set_gtk_property!(grid[i, j], :label, piece.black)
            elseif pos in wqueens
                set_gtk_property!(grid[i, j], :label, piece.white)
            end
        end
        showall(win)
    end

    update!()
    cond = Condition()
    endit(w) = notify(cond)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(cond)
end

peacefulqueenapp()

```



## Kotlin

{{trans|D}}

```scala
import kotlin.math.abs

enum class Piece {
    Empty,
    Black,
    White,
}

typealias Position = Pair<Int, Int>

fun place(m: Int, n: Int, pBlackQueens: MutableList<Position>, pWhiteQueens: MutableList<Position>): Boolean {
    if (m == 0) {
        return true
    }
    var placingBlack = true
    for (i in 0 until n) {
        inner@
        for (j in 0 until n) {
            val pos = Position(i, j)
            for (queen in pBlackQueens) {
                if (queen == pos || !placingBlack && isAttacking(queen, pos)) {
                    continue@inner
                }
            }
            for (queen in pWhiteQueens) {
                if (queen == pos || placingBlack && isAttacking(queen, pos)) {
                    continue@inner
                }
            }
            placingBlack = if (placingBlack) {
                pBlackQueens.add(pos)
                false
            } else {
                pWhiteQueens.add(pos)
                if (place(m - 1, n, pBlackQueens, pWhiteQueens)) {
                    return true
                }
                pBlackQueens.removeAt(pBlackQueens.lastIndex)
                pWhiteQueens.removeAt(pWhiteQueens.lastIndex)
                true
            }
        }
    }
    if (!placingBlack) {
        pBlackQueens.removeAt(pBlackQueens.lastIndex)
    }
    return false
}

fun isAttacking(queen: Position, pos: Position): Boolean {
    return queen.first == pos.first
            || queen.second == pos.second
            || abs(queen.first - pos.first) == abs(queen.second - pos.second)
}

fun printBoard(n: Int, blackQueens: List<Position>, whiteQueens: List<Position>) {
    val board = MutableList(n * n) { Piece.Empty }

    for (queen in blackQueens) {
        board[queen.first * n + queen.second] = Piece.Black
    }
    for (queen in whiteQueens) {
        board[queen.first * n + queen.second] = Piece.White
    }
    for ((i, b) in board.withIndex()) {
        if (i != 0 && i % n == 0) {
            println()
        }
        if (b == Piece.Black) {
            print("B ")
        } else if (b == Piece.White) {
            print("W ")
        } else {
            val j = i / n
            val k = i - j * n
            if (j % 2 == k % 2) {
                print("• ")
            } else {
                print("◦ ")
            }
        }
    }
    println('\n')
}

fun main() {
    val nms = listOf(
        Pair(2, 1), Pair(3, 1), Pair(3, 2), Pair(4, 1), Pair(4, 2), Pair(4, 3),
        Pair(5, 1), Pair(5, 2), Pair(5, 3), Pair(5, 4), Pair(5, 5),
        Pair(6, 1), Pair(6, 2), Pair(6, 3), Pair(6, 4), Pair(6, 5), Pair(6, 6),
        Pair(7, 1), Pair(7, 2), Pair(7, 3), Pair(7, 4), Pair(7, 5), Pair(7, 6), Pair(7, 7)
    )
    for ((n, m) in nms) {
        println("$m black and $m white queens on a $n x $n board:")
        val blackQueens = mutableListOf<Position>()
        val whiteQueens = mutableListOf<Position>()
        if (place(m, n, blackQueens, whiteQueens)) {
            printBoard(n, blackQueens, whiteQueens)
        } else {
            println("No solution exists.\n")
        }
    }
}
```

{{out}}

```txt
1 black and 1 white queens on a 2 x 2 board:
No solution exists.

1 black and 1 white queens on a 3 x 3 board:
B ◦ • 
◦ • W 
• ◦ • 

2 black and 2 white queens on a 3 x 3 board:
No solution exists.

1 black and 1 white queens on a 4 x 4 board:
B ◦ • ◦ 
◦ • W • 
• ◦ • ◦ 
◦ • ◦ • 

2 black and 2 white queens on a 4 x 4 board:
B ◦ • ◦ 
◦ • W • 
B ◦ • ◦ 
◦ • W • 

3 black and 3 white queens on a 4 x 4 board:
No solution exists.

1 black and 1 white queens on a 5 x 5 board:
B ◦ • ◦ • 
◦ • W • ◦ 
• ◦ • ◦ • 
◦ • ◦ • ◦ 
• ◦ • ◦ • 

2 black and 2 white queens on a 5 x 5 board:
B ◦ • ◦ B 
◦ • W • ◦ 
• W • ◦ • 
◦ • ◦ • ◦ 
• ◦ • ◦ • 

3 black and 3 white queens on a 5 x 5 board:
B ◦ • ◦ B 
◦ • W • ◦ 
• W • ◦ • 
◦ • ◦ B ◦ 
• W • ◦ • 

4 black and 4 white queens on a 5 x 5 board:
• B • B • 
◦ • ◦ • B 
W ◦ W ◦ • 
◦ • ◦ • B 
W ◦ W ◦ • 

5 black and 5 white queens on a 5 x 5 board:
No solution exists.

1 black and 1 white queens on a 6 x 6 board:
B ◦ • ◦ • ◦ 
◦ • W • ◦ • 
• ◦ • ◦ • ◦ 
◦ • ◦ • ◦ • 
• ◦ • ◦ • ◦ 
◦ • ◦ • ◦ • 

2 black and 2 white queens on a 6 x 6 board:
B ◦ • ◦ B ◦ 
◦ • W • ◦ • 
• W • ◦ • ◦ 
◦ • ◦ • ◦ • 
• ◦ • ◦ • ◦ 
◦ • ◦ • ◦ • 

3 black and 3 white queens on a 6 x 6 board:
B ◦ • ◦ B B 
◦ • W • ◦ • 
• W • ◦ • ◦ 
◦ • ◦ • ◦ • 
• ◦ W ◦ • ◦ 
◦ • ◦ • ◦ • 

4 black and 4 white queens on a 6 x 6 board:
B ◦ • ◦ B B 
◦ • W • ◦ • 
• W • ◦ • ◦ 
◦ • ◦ • ◦ B 
• ◦ W W • ◦ 
◦ • ◦ • ◦ • 

5 black and 5 white queens on a 6 x 6 board:
• B • ◦ B ◦ 
◦ • ◦ B ◦ B 
W ◦ • ◦ • ◦ 
W • W • ◦ • 
• ◦ • ◦ • B 
W • W • ◦ • 

6 black and 6 white queens on a 6 x 6 board:
No solution exists.

1 black and 1 white queens on a 7 x 7 board:
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

2 black and 2 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

3 black and 3 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

4 black and 4 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 
◦ • ◦ • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

5 black and 5 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ • ◦ • 
◦ • W • ◦ • ◦ 
• ◦ • ◦ • ◦ • 

6 black and 6 white queens on a 7 x 7 board:
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
B ◦ • ◦ B ◦ • 
◦ • W • ◦ • W 
• ◦ • ◦ • ◦ • 

7 black and 7 white queens on a 7 x 7 board:
• B • ◦ • B • 
◦ B ◦ • B • ◦ 
• B • ◦ • B • 
◦ • ◦ • B • ◦ 
W ◦ W ◦ • ◦ W 
◦ • ◦ W ◦ • ◦ 
W ◦ W W • ◦ • 
```



## Perl


```perl
#!/usr/bin/perl

use strict;       # http://www.rosettacode.org/wiki/Peaceful_chess_queen_armies
use warnings;

my $m = shift // 4;
my $n = shift // 5;
my %seen;
my $gaps = join '|', qr/-*/, map qr/.{$_}(?:-.{$_})*/sx, $n-1, $n, $n+1;
my $attack = qr/(\w)(?:$gaps)(?!\1)\w/;

place( scalar +('-' x $n . "\n") x $n );
print "No solution to $m $n\n";

sub place
  {
  local $_ = shift;
  $seen{$_}++ || /$attack/ and return; # previously or attack
  (my $have = tr/WB//) < $m * 2 or exit !print "Solution to $m $n\n\n$_";
  place( s/-\G/ qw(W B)[$have % 2] /er ) while /-/g; # place next queen
  }
```

{{out}}

```txt

Solution to 4 5

W---W
--B--
-B-B-
--B--
W---W

```



## Phix

{{trans|Go}}
{{trans|Python}}

```Phix
-- demo\rosetta\Queen_Armies.exw
string html = ""
constant as_html = true
constant queens = {``,
                   `&#x265b;`, 
                   `<font color="green">&#x2655;</font>`,
                   `<span style="color:red">?</span>`}

procedure showboard(integer n, sequence blackqueens, whitequeens)
    sequence board = repeat(repeat('-',n),n)
    for i=1 to length(blackqueens) do
        integer {qi,qj} = blackqueens[i]
        board[qi,qj] = 'B'
        {qi,qj} = whitequeens[i]
        board[qi,qj] = 'W'
    end for
    if as_html then
        string out = sprintf("
<b>## %d black and %d white queens on a %d-by-%d board</b>
\n",
                             {length(blackqueens),length(whitequeens),n,n}),
               tbl = ""
        out &= "<table style=\"font-weight:bold\">\n  "
        for x=1 to n do
            for y=1 to n do
                if y=1 then tbl &= "  </tr>\n  <tr valign=\"middle\" align=\"center\">\n" end if
                integer xw = find({x,y},blackqueens)!=0,
                        xb = find({x,y},whitequeens)!=0,
                        dx = xw+xb*2+1
                string ch = queens[dx],
                       bg = iff(mod(x+y,2)?"":` bgcolor="silver"`)
                tbl &= sprintf("    <td style=\"width:14pt; height:14pt;\"%s>%s</td>\n",{bg,ch})
            end for
        end for
        out &= tbl[11..$]
        out &= "  </tr>\n</table>\n
\n"
        html &= out
    else
        integer b = length(blackqueens),
                w = length(whitequeens)
        printf(1,"%d black and %d white queens on a %d x %d board:\n", {b, w, n, n})
        puts(1,join(board,"\n")&"\n")
--      ?{n,blackqueens, whitequeens}
    end if
end procedure 

function isAttacking(sequence queen, pos)
    integer {qi,qj} = queen, {pi,pj} = pos
    return qi=pi or qj=pj or abs(qi-pi)=abs(qj-pj)
end function

function place(integer m, n, sequence blackqueens = {}, whitequeens = {})
    if m == 0 then showboard(n,blackqueens,whitequeens) return true end if
    bool placingBlack := true
    for i=1 to n do
        for j=1 to n do
            sequence pos := {i, j}
            for q=1 to length(blackqueens) do
                sequence queen := blackqueens[q]
                if queen == pos or ((not placingBlack) and isAttacking(queen, pos)) then
                    pos = {}
                    exit
                end if
            end for
            if pos!={} then
                for q=1 to length(whitequeens) do
                    sequence queen := whitequeens[q]
                    if queen == pos or (placingBlack and isAttacking(queen, pos)) then
                        pos = {}
                        exit
                    end if
                end for
                if pos!={} then
                    if placingBlack then
                        blackqueens = append(blackqueens, pos)
                        placingBlack = false
                    else
                        whitequeens = append(whitequeens, pos)
                        if place(m-1, n, blackqueens, whitequeens) then return true end if
                        blackqueens = blackqueens[1..$-1]
                        whitequeens = whitequeens[1..$-1]
                        placingBlack = true
                    end if
                end if
            end if
        end for
    end for
    return false
end function

for n=2 to 7 do
    for m=1 to n-(n<5) do
        if not place(m,n) then
            string no = sprintf("Cannot place %d+ queen armies on a %d-by-%d board",{m,n,n})
            if as_html then
                html &= sprintf("<b># %s</b>

\n\n",{no})
            else
                printf(1,"%s.\n", {no})
            end if
        end if
    end for
end for

constant html_header = """
<!DOCTYPE html>
<html lang="en">
 <head>
  <meta charset="utf-8" />
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title>Rosettacode Rank Languages by popularity</title>
 </head>
 <body>
  <h2>queen armies</h2>
""", -- or <div style="overflow:scroll; height:250px;">
         html_footer = """
 </body>
</html>
""" -- or </div>

if as_html then
    integer fn = open("queen_armies.html","w")
    puts(fn,html_header)
    puts(fn,html)
    puts(fn,html_footer)
    close(fn)
    printf(1,"See queen_armies.html\n")
end if

?"done"
{} = wait_key()
```

{{out}}
with as_html = false

```txt

Cannot place 1+ queen armies on a 2-by-2 board.
1 black and 1 white queens on a 3 x 3 board:
B--
--W
---
Cannot place 2+ queen armies on a 3-by-3 board.
&lt;snip&gt;
7 black and 7 white queens on a 7 x 7 board:
-B---B-
-B--B--
-B---B-
----B--
W-W---W
---W---
W-WW---

```


{{out}}
with as_html = true
<div style="overflow:scroll; height:250px;">
<b># Cannot place 1+ queen armies on a 2-by-2 board</b>




<b>## 1 black and 1 white queens on a 3-by-3 board</b>

<table style="font-weight:bold">
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>


<b># Cannot place 2+ queen armies on a 3-by-3 board</b>


&lt;snip&gt;



<b>## 7 black and 7 white queens on a 7-by-7 board</b>

<table style="font-weight:bold">
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>
</div>


## Python


### Python: Textual output


```python
from itertools import combinations, product, count
from functools import lru_cache, reduce


_bbullet, _wbullet = '\u2022\u25E6'
_or = set.__or__

def place(m, n):
    "Place m black and white queens, peacefully, on an n-by-n board"
    board = set(product(range(n), repeat=2))  # (x, y) tuples
    placements = {frozenset(c) for c in combinations(board, m)}
    for blacks in placements:
        black_attacks = reduce(_or, 
                               (queen_attacks_from(pos, n) for pos in blacks), 
                               set())
        for whites in {frozenset(c)     # Never on blsck attacking squares
                       for c in combinations(board - black_attacks, m)}:
            if not black_attacks & whites:
                return blacks, whites
    return set(), set()

@lru_cache(maxsize=None)
def queen_attacks_from(pos, n):
    x0, y0 = pos
    a = set([pos])    # Its position
    a.update((x, y0) for x in range(n))    # Its row
    a.update((x0, y) for y in range(n))    # Its column
    # Diagonals
    for x1 in range(n):
        # l-to-r diag
        y1 = y0 -x0 +x1
        if 0 <= y1 < n: 
            a.add((x1, y1))
        # r-to-l diag
        y1 = y0 +x0 -x1
        if 0 <= y1 < n: 
            a.add((x1, y1))
    return a

def pboard(black_white, n):
    "Print board"
    if black_white is None: 
        blk, wht = set(), set()
    else:
        blk, wht = black_white
    print(f"## {len(blk)} black and {len(wht)} white queens "
          f"on a {n}-by-{n} board:", end='')
    for x, y in product(range(n), repeat=2):
        if y == 0:
            print()
        xy = (x, y)
        ch = ('?' if xy in blk and xy in wht 
              else 'B' if xy in blk
              else 'W' if xy in wht
              else _bbullet if (x + y)%2 else _wbullet)
        print('%s' % ch, end='')
    print()

if __name__ == '__main__':
    n=2
    for n in range(2, 7):
        print()
        for m in count(1):
            ans = place(m, n)
            if ans[0]:
                pboard(ans, n)
            else:
                print (f"# Can't place {m}+ queens on a {n}-by-{n} board")
                break
    #
    print('\n')
    m, n = 5, 7
    ans = place(m, n)
    pboard(ans, n)
```


{{out}}
<div style="overflow:scroll; height:250px;">

```txt
# Can't place 1+ queens on a 2-by-2 board

## 1 black and 1 white queens on a 3-by-3 board:
◦•◦
B◦•
◦•W
# Can't place 2+ queens on a 3-by-3 board

## 1 black and 1 white queens on a 4-by-4 board:
◦•W•
B◦•◦
◦•◦•
•◦•◦
## 2 black and 2 white queens on a 4-by-4 board:
◦B◦•
•B•◦
◦•◦•
W◦W◦
# Can't place 3+ queens on a 4-by-4 board

## 1 black and 1 white queens on a 5-by-5 board:
◦•◦•◦
W◦•◦•
◦•◦•◦
•◦•◦B
◦•◦•◦
## 2 black and 2 white queens on a 5-by-5 board:
◦•◦•W
•◦B◦•
◦•◦•◦
•◦•B•
◦W◦•◦
## 3 black and 3 white queens on a 5-by-5 board:
◦W◦•◦
•◦•◦W
B•B•◦
B◦•◦•
◦•◦W◦
## 4 black and 4 white queens on a 5-by-5 board:
◦•B•B
W◦•◦•
◦W◦W◦
W◦•◦•
◦•B•B
# Can't place 5+ queens on a 5-by-5 board

## 1 black and 1 white queens on a 6-by-6 board:
◦•◦•◦•
W◦•◦•◦
◦•◦•◦•
•◦•◦B◦
◦•◦•◦•
•◦•◦•◦
## 2 black and 2 white queens on a 6-by-6 board:
◦•◦•◦•
•◦B◦•◦
◦•◦•◦•
•◦•B•◦
◦•◦•◦•
W◦•◦W◦
## 3 black and 3 white queens on a 6-by-6 board:
◦•B•◦•
•B•◦•◦
◦•◦W◦W
•◦•◦•◦
W•◦•◦•
•◦•◦B◦
## 4 black and 4 white queens on a 6-by-6 board:
WW◦•W•
•W•◦•◦
◦•◦•◦B
•◦B◦•◦
◦•◦B◦•
•◦•B•◦
## 5 black and 5 white queens on a 6-by-6 board:
◦•W•W•
B◦•◦•◦
◦•W•◦W
B◦•◦•◦
◦•◦•◦W
BB•B•◦
# Can't place 6+ queens on a 6-by-6 board


## 5 black and 5 white queens on a 7-by-7 board:
◦•◦•B•◦
•W•◦•◦W
◦•◦•B•◦
B◦•◦•◦•
◦•B•◦•◦
•◦•B•◦•
◦W◦•◦WW
```

</div>


### Python: HTML output

Uses the solver function <code>place</code> from the above textual output case.

```python
from peaceful_queen_armies_simpler import place
from itertools import product, count

_bqueenh, _wqueenh = '&#x265b;', '<font color="green">&#x2655;</font>'

def hboard(black_white, n):
    "HTML board generator"
    if black_white is None: 
        blk, wht = set(), set()
    else:
        blk, wht = black_white
    out = (f"
<b>## {len(blk)} black and {len(wht)} white queens "
           f"on a {n}-by-{n} board</b>
\n")
    out += '<table style="font-weight:bold">\n  '
    tbl = ''
    for x, y in product(range(n), repeat=2):
        if y == 0:
            tbl += '  </tr>\n  <tr valign="middle" align="center">\n'
        xy = (x, y)
        ch = ('<span style="color:red">?</span>' if xy in blk and xy in wht 
              else _bqueenh if xy in blk
              else _wqueenh if xy in wht
              else "")
        bg = "" if (x + y)%2 else ' bgcolor="silver"'
        tbl += f'    <td style="width:14pt; height:14pt;"{bg}>{ch}</td>\n'
    out += tbl[7:]
    out += '  </tr>\n</table>\n
\n'
    return out

if __name__ == '__main__':
    n=2
    html = ''
    for n in range(2, 7):
        print()
        for m in count(1):
            ans = place(m, n)
            if ans[0]:
                html += hboard(ans, n)
            else:
                html += (f"<b># Can't place {m}+ queen armies on a "
                         f"{n}-by-{n} board</b>

\n\n" )
                break
    #
    html += '
\n'
    m, n = 6, 7
    ans = place(m, n)
    html += hboard(ans, n)
    with open('peaceful_queen_armies.htm', 'w') as f:
        f.write(html)
```


{{out}}

<div style="overflow:scroll; height:250px;">

<b># Can't place 1+ queen armies on a 2-by-2 board</b>




<b>## 1 black and 1 white queens on a 3-by-3 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
  </tr>
</table>


<b># Can't place 2+ queen armies on a 3-by-3 board</b>




<b>## 1 black and 1 white queens on a 4-by-4 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 2 black and 2 white queens on a 4-by-4 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>


<b># Can't place 3+ queen armies on a 4-by-4 board</b>




<b>## 1 black and 1 white queens on a 5-by-5 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 2 black and 2 white queens on a 5-by-5 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 3 black and 3 white queens on a 5-by-5 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 4 black and 4 white queens on a 5-by-5 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
  </tr>
</table>


<b># Can't place 5+ queen armies on a 5-by-5 board</b>




<b>## 1 black and 1 white queens on a 6-by-6 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 2 black and 2 white queens on a 6-by-6 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 3 black and 3 white queens on a 6-by-6 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 4 black and 4 white queens on a 6-by-6 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



<b>## 5 black and 5 white queens on a 6-by-6 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;">&#x265b;</td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>


<b># Can't place 6+ queen armies on a 6-by-6 board</b>






<b>## 6 black and 6 white queens on a 7-by-7 board</b>

<table style="font-weight:bold">
  
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"><font color="green">&#x2655;</font></td>
  </tr>
  <tr valign="middle" align="center">
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:14pt; height:14pt;"></td>
    <td style="width:14pt; height:14pt;" bgcolor="silver"></td>
  </tr>
</table>



</div>


## zkl


```zkl
fcn isAttacked(q, x,y) // ( (r,c), x,y ) : is queen at r,c attacked by q@(x,y)?
   { r,c:=q; (r==x or c==y or r+c==x+y or r-c==x-y) }
fcn isSafe(r,c,qs) // queen safe at (r,c)?, qs=( (r,c),(r,c)..)
   { ( not qs.filter1(isAttacked,r,c) ) }
fcn isEmpty(r,c,qs){ (not (qs and qs.filter1('wrap([(x,y)]){ r==x and c==y })) ) }
fcn _peacefulQueens(N,M,qa,qb){  //--> False | (True,((r,c)..),((r,c)..) )
   // qa,qb -->  // ( (r,c),(r,c).. ), solution so far to last good spot
   if(qa.len()==M==qb.len()) return(True,qa,qb);
   n, x,y := N, 0,0;
   if(qa) x,y = qa[-1]; else n=(N+1)/2;  // first queen, first quadrant only
   foreach r in ([x..n-1]){
      foreach c in ([y..n-1]){
	 if(isEmpty(r,c,qa) and isSafe(r,c,qb)){
	    qc,qd := qa.append(T(r,c)), self.fcn(N,M, qb,qc);
	    if(qd) return( if(qd[0]==True) qd else T(qc,qd) );
	 }
      }
      y=0
   }
   False
}

fcn peacefulQueens(N=5,M=4){ # NxN board, M white and black queens
   qs:=_peacefulQueens(N,M, T,T);
   println("Solution for %dx%d board with %d black and %d white queens:".fmt(N,N,M,M));
   if(not qs)println("None");
   else{
      z:=Data(Void,"-"*N*N);
      foreach r,c in (qs[1]){ z[r*N + c]="W" }
      foreach r,c in (qs[2]){ z[r*N + c]="B" }
      z.text.pump(Void,T(Void.Read,N-1),"println");
   }   
}
```


```zkl
peacefulQueens();
foreach n in ([4..10]){ peacefulQueens(n,n) }
```

{{out}}

```txt

Solution for 5x5 board with 4 black and 4 white queens:
W---W
--B--
-B-B-
--B--
W---W
Solution for 4x4 board with 4 black and 4 white queens:
None
Solution for 5x5 board with 5 black and 5 white queens:
None
Solution for 6x6 board with 6 black and 6 white queens:
None
Solution for 7x7 board with 7 black and 7 white queens:
W---W-W
--B----
-B-B-B-
--B----
W-----W
--BB---
W-----W
Solution for 8x8 board with 8 black and 8 white queens:
W---W---
--B---BB
W---W---
--B---B-
---B---B
-W---W--
W---W---
--B-----
Solution for 9x9 board with 9 black and 9 white queens:
W---W---W
--B---B--
-B---B---
---W---W-
-B---B---
---W---W-
-B---B---
---W---W-
-B-------
Solution for 10x10 board with 10 black and 10 white queens:
W---W---WW
--B---B---
-B-B------
-----W-W-W
-BBB------
-----W-W-W
-B--------
------B---
---B------
----------

```

