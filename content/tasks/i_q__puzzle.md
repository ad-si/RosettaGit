+++
title = "I.Q. Puzzle"
description = ""
date = 2019-09-08T09:00:06Z
aliases = []
[extra]
id = 18092
[taxonomies]
categories = ["task"]
tags = []
+++

An   IQ Puzzle   is a triangle of 15 golf tee's. 


This puzzle is typically seen at Cracker Barrel   (a USA sales store)   where one tee is missing and the remaining tees jump over each other   (with removal of the jumped tee, like checkers)   until one tee is left. 

The fewer tees left,   the higher the IQ score. 

Peg   #1   is the top centre through to the bottom row which are pegs 11 through to 15.

```txt

         ^
        / \        
       /   \
      /     \
     /   1   \     
    /  2   3  \
   / 4   5  6  \ 
  / 7  8  9  10 \
 /11 12 13 14  15\
/_________________\

```


Reference picture:   http://www.joenord.com/puzzles/peggame/


## Task

Print a solution to solve the puzzle leaving one peg not implemented variations.

Start with empty peg in   '''X'''   and solve with one peg in position   '''Y'''.





## D

```d
import std.stdio, std.array, std.string, std.range, std.algorithm;

immutable N = [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1];
immutable G = [[0,1,3],[0,2,5],[1,3,6],[1,4,8],[2,4,7],[2,5,9],
    [3,4,5],[3,6,10],[3,7,12],[4,7,11],[4,8,13],[5,8,12],
    [5,9,14],[6,7,8],[7,8,9],[10,11,12],[11,12,13],[12,13,14]];

string b2s(in int[] n) pure @safe {
    static immutable fmt = 6.iota
                           .map!(i => " ".replicate(5 - i) ~ "%d ".replicate(i))
                           .join('\n');
    return fmt.format(n[0], n[1], n[2],  n[3],  n[4],  n[5],  n[6],
                      n[7], n[8], n[9], n[10], n[11], n[12], n[13], n[14]);
}

string solve(in int[] n, in int i, in int[] g) pure @safe {
    if (i == N.length - 1)
        return "\nSolved";
    if (n[g[1]] == 0)
        return null;
    string s;
    if (n[g[0]] == 0) {
        if (n[g[2]] == 0)
            return null;
        s = "\n%d to %d\n".format(g[2], g[0]);
    } else {
        if (n[g[2]] == 1)
            return null;
        s = "\n%d to %d\n".format(g[0], g[2]);
    }

    auto a = n.dup;
    foreach (const gi; g)
        a[gi] = 1 - a[gi];
    string l;
    foreach (const gi; G) {
        l = solve(a, i + 1, gi);
        if (!l.empty)
            break;
    }
    return l.empty ? l : (s ~ b2s(a) ~ l);
}

void main() @safe {
    b2s(N).write;
    string l;
    foreach (const g; G) {
        l = solve(N, 1, g);
        if (!l.empty)
            break;
    }
    writeln(l.empty ? "No solution found." : l);
}
```

```txt
     
    0 
   1 1 
  1 1 1 
 1 1 1 1 
1 1 1 1 1 
3 to 0
     
    1 
   0 1 
  0 1 1 
 1 1 1 1 
1 1 1 1 1 
8 to 1
     
    1 
   1 1 
  0 0 1 
 1 1 0 1 
1 1 1 1 1 
10 to 3
     
    1 
   1 1 
  1 0 1 
 0 1 0 1 
0 1 1 1 1 
1 to 6
     
    1 
   0 1 
  0 0 1 
 1 1 0 1 
0 1 1 1 1 
11 to 4
     
    1 
   0 1 
  0 1 1 
 1 0 0 1 
0 0 1 1 1 
2 to 7
     
    1 
   0 0 
  0 0 1 
 1 1 0 1 
0 0 1 1 1 
9 to 2
     
    1 
   0 1 
  0 0 0 
 1 1 0 0 
0 0 1 1 1 
0 to 5
     
    0 
   0 0 
  0 0 1 
 1 1 0 0 
0 0 1 1 1 
6 to 8
     
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 0 1 1 1 
13 to 11
     
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 1 0 0 1 
5 to 12
     
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 1 1 0 1 
11 to 13
     
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 0 1 1 
14 to 12
     
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 1 0 0 
Solved
```



## Elixir

Inspired by Ruby

```elixir
defmodule IQ_Puzzle do
  def task(i \\ 0, n \\ 5) do
    fmt = Enum.map_join(1..n, fn i ->
            String.duplicate(" ", n-i) <> String.duplicate("~w ", i) <> "~n"
          end)
    pegs = Tuple.duplicate(1, div(n*(n+1),2)) |> put_elem(i, 0)
    rest = tuple_size(pegs) - 1
    next = next_list(n)
    :io.format fmt, Tuple.to_list(pegs)
    result = Enum.find_value(next, fn nxt -> solve(pegs, rest, nxt, next, fmt) end)
    IO.puts  if result, do: result, else: "No solution found"
  end
  
  defp solve(_,1,_,_,_), do: "Solved"
  defp solve(pegs,rest,{g0,g1,g2},next,fmt) do
    if s = jump(pegs, g0, g1, g2) do
      peg2 = Enum.reduce([g0,g1,g2], pegs, fn g,acc ->
               put_elem(acc, g, 1-elem(acc, g))
             end)
      result = Enum.find_value(next, fn g -> solve(peg2, rest-1, g, next, fmt) end)
      if result do
        [(:io_lib.format "~n~s~n", [s]), (:io_lib.format fmt, Tuple.to_list(peg2)) | result]
      end
    end
  end
  
  defp jump(pegs, _0, g1, _2) when elem(pegs,g1)==0, do: nil
  defp jump(pegs, g0, _1, g2) when elem(pegs,g0)==0, do: if elem(pegs, g2)==1, do: "#{g2} to #{g0}"
  defp jump(pegs, g0, _1, g2)                      , do: if elem(pegs, g2)==0, do: "#{g0} to #{g2}"
  
  defp next_list(n) do
    points = for x <- 1..n, y <- 1..x, do: {x,y}
    board = points |> Enum.with_index |> Enum.into(Map.new)
    Enum.flat_map(points, fn {x,y} ->
      [ {board[{x,y}], board[{x,  y+1}], board[{x,  y+2}]},
        {board[{x,y}], board[{x+1,y  }], board[{x+2,y  }]},
        {board[{x,y}], board[{x+1,y+1}], board[{x+2,y+2}]} ]
    end)
    |> Enum.filter(fn {_,_,p} -> p end)
  end
end

IQ_Puzzle.task
```


<pre style="height:80ex;overflow:scroll">
    0 
   1 1 
  1 1 1 
 1 1 1 1 
1 1 1 1 1 

3 to 0
    1 
   0 1 
  0 1 1 
 1 1 1 1 
1 1 1 1 1 

8 to 1
    1 
   1 1 
  0 0 1 
 1 1 0 1 
1 1 1 1 1 

10 to 3
    1 
   1 1 
  1 0 1 
 0 1 0 1 
0 1 1 1 1 

1 to 6
    1 
   0 1 
  0 0 1 
 1 1 0 1 
0 1 1 1 1 

11 to 4
    1 
   0 1 
  0 1 1 
 1 0 0 1 
0 0 1 1 1 

2 to 7
    1 
   0 0 
  0 0 1 
 1 1 0 1 
0 0 1 1 1 

9 to 2
    1 
   0 1 
  0 0 0 
 1 1 0 0 
0 0 1 1 1 

0 to 5
    0 
   0 0 
  0 0 1 
 1 1 0 0 
0 0 1 1 1 

6 to 8
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 0 1 1 1 

13 to 11
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 1 0 0 1 

5 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 1 1 0 1 

11 to 13
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 0 1 1 

14 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 1 0 0 
Solved

```



## Go

```go
package main

import "fmt"

type solution struct{ peg, over, land int }

type move struct{ from, to int }

var emptyStart = 1

var board [16]bool

var jumpMoves = [16][]move{
    {},
    {{2, 4}, {3, 6}},
    {{4, 7}, {5, 9}},
    {{5, 8}, {6, 10}},
    {{2, 1}, {5, 6}, {7, 11}, {8, 13}},
    {{8, 12}, {9, 14}},
    {{3, 1}, {5, 4}, {9, 13}, {10, 15}},
    {{4, 2}, {8, 9}},
    {{5, 3}, {9, 10}},
    {{5, 2}, {8, 7}},
    {{9, 8}},
    {{12, 13}},
    {{8, 5}, {13, 14}},
    {{8, 4}, {9, 6}, {12, 11}, {14, 15}},
    {{9, 5}, {13, 12}},
    {{10, 6}, {14, 13}},
}

var solutions []solution

func initBoard() {
    for i := 1; i < 16; i++ {
        board[i] = true
    }
    board[emptyStart] = false
}

func (sol solution) split() (int, int, int) {
    return sol.peg, sol.over, sol.land
}

func (mv move) split() (int, int) {
    return mv.from, mv.to
}

func drawBoard() {
    var pegs [16]byte
    for i := 1; i < 16; i++ {
        if board[i] {
            pegs[i] = fmt.Sprintf("%X", i)[0]
        } else {
            pegs[i] = '-'
        }
    }
    fmt.Printf("       %c\n", pegs[1])
    fmt.Printf("      %c %c\n", pegs[2], pegs[3])
    fmt.Printf("     %c %c %c\n", pegs[4], pegs[5], pegs[6])
    fmt.Printf("    %c %c %c %c\n", pegs[7], pegs[8], pegs[9], pegs[10])
    fmt.Printf("   %c %c %c %c %c\n", pegs[11], pegs[12], pegs[13], pegs[14], pegs[15])
}

func solved() bool {
    count := 0
    for _, b := range board {
        if b {
            count++
        }
    }
    return count == 1 // just one peg left
}

func solve() {
    if solved() {
        return
    }
    for peg := 1; peg < 16; peg++ {
        if board[peg] {
            for _, mv := range jumpMoves[peg] {
                over, land := mv.split()
                if board[over] && !board[land] {
                    saveBoard := board
                    board[peg] = false
                    board[over] = false
                    board[land] = true
                    solutions = append(solutions, solution{peg, over, land})
                    solve()
                    if solved() {
                        return // otherwise back-track
                    }
                    board = saveBoard
                    solutions = solutions[:len(solutions)-1]
                }
            }
        }
    }
}

func main() {
    initBoard()
    solve()
    initBoard()
    drawBoard()
    fmt.Printf("Starting with peg %X removed\n\n", emptyStart)
    for _, solution := range solutions {
        peg, over, land := solution.split()
        board[peg] = false
        board[over] = false
        board[land] = true
        drawBoard()
        fmt.Printf("Peg %X jumped over %X to land on %X\n\n", peg, over, land)
    }
}
```


```txt

Same as Kotlin entry

```



## J


```J

NB. This is a direct translation of the python program,
NB. except for the display which by move is horizontal.

PEGS =: >:i.15

move =: 4 : 0       NB. move should have been factored in the 2014-NOV-29 python version
 board =. x
 'peg over land' =. y
 board =. board RemovePeg peg
 board =. board RemovePeg over
 board =. board AddPeg land
)

NB.# Draw board triangle in ascii
NB.#
NB.def DrawBoard(board):
NB.  peg = [0,]*16
NB.  for n in xrange(1,16):
NB.    peg[n] = '.'
NB.    if n in board:
NB.      peg[n] = "%X" % n
NB.  print "     %s" % peg[1]
NB.  print "    %s %s" % (peg[2],peg[3])
NB.  print "   %s %s %s" % (peg[4],peg[5],peg[6])
NB.  print "  %s %s %s %s" % (peg[7],peg[8],peg[9],peg[10])
NB.  print " %s %s %s %s %s" % (peg[11],peg[12],peg[13],peg[14],peg[15])

HEXCHARS =: Num_j_ , Alpha_j_

DrawBoard =: 3 : 0
 NB. observe 1 1 0 1 0 0 1 0 0 0 1 0 0 0 0 -: 2#.inv 26896  (== 6910 in base 16)
 board =. y
 < (-i._5) (|."0 1) 1j1 (#"1) (2#.inv 16b6910)[;.1 }. (board { HEXCHARS) board } 16 # '.'
)


NB.# remove peg n from board
NB.def RemovePeg(board,n):
NB.  board.remove(n)
NB.  return board

RemovePeg =: i. ({. , (}.~ >:)~) [


NB.# Add peg n on board
NB.def AddPeg(board,n):
NB.  board.append(n)
NB.  return board

AddPeg =: ,


NB.# return true if peg N is on board else false is empty position
NB.def IsPeg(board,n):
NB.  return n in board

IsPeg =: e.~


NB.# A dictionary of valid jump moves index by jumping peg
NB.# then a list of moves where move has jumpOver and LandAt positions
NB.JumpMoves = { 1: [ (2,4),(3,6) ],  # 1 can jump over 2 to land on 4, or jumper over 3 to land on 6
NB.              2: [ (4,7),(5,9)  ],
NB.              3: [ (5,8),(6,10) ],
NB.                 ...
NB.             14: [ (9,5),(13,12)  ],
NB.             15: [ (10,6),(14,13) ]
NB.            }

JumpMoves =: a:,(<@:([\~ _2:)@:".;._2) 0 :0  NB. 1 can jump over 2 to land on 4, or jump over 3 to land on 6
   (2,4),(3,6)
   (4,7),(5,9)
   (5,8),(6,10)
   (2,1),(5,6),(7,11),(8,13)
   (8,12),(9,14)
   (3,1),(5,4),(9,13),(10,15)
   (4,2),(8,9)
   (5,3),(9,10)
   (5,2),(8,7)
   (9,8)
   (12,13)
   (8,5),(13,14)
   (8,4),(9,6),(12,11),(14,15)
   (9,5),(13,12)
   (10,6),(14,13)
)


NB.Solution = []
NB.#
NB.# Recursively solve the problem
NB.#
NB.def Solve(board):
NB.  #DrawBoard(board)
NB.  if len(board) == 1:
NB.    return board # Solved one peg left
NB.  # try a move for each peg on the board
NB.  for peg in xrange(1,16): # try in numeric order not board order
NB.    if IsPeg(board,peg):
NB.      movelist = JumpMoves[peg]
NB.      for over,land in movelist:
NB.        if IsPeg(board,over) and not IsPeg(board,land):
NB.          saveboard = board[:] # for back tracking
NB.          board = RemovePeg(board,peg)
NB.          board = RemovePeg(board,over)
NB.          board = AddPeg(board,land) # board order changes!
NB.          Solution.append((peg,over,land))
NB.          board = Solve(board)
NB.          if len(board) == 1:
NB.            return board
NB.        ## undo move and back track when stuck!
NB.          board = saveboard[:] # back track
NB.          del Solution[-1] # remove last move
NB.  return board

Solution =: 0 3 $ 0

Solve =: 3 : 0
 board =. y
 if. 1 = # board do. return. end.
 for_peg. PEGS do.
  if. board IsPeg peg do.
   movelist =: peg {:: JumpMoves
   for_OL. movelist do.
    'over land' =. OL
    if. (board IsPeg over) (*. -.) (board IsPeg land) do.
     saveboard =. board          NB. for back tracking
     board =. board move peg,over,land
     Solution =: Solution , peg, over, land
     board =. Solve board
     if. 1 = # board do. return. end.
     board =. saveboard
     Solution =: }: Solution
    end.
   end.
  end.
 end.
 board
)


NB.#
NB.# Remove one peg and start solving
NB.#
NB.def InitSolve(empty):
NB.  board = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
NB.  RemovePeg(board,empty_start)
NB.  Solve(board)

InitSolve =: [: Solve PEGS RemovePeg ]


NB.#
NB.empty_start = 1
NB.InitSolve(empty_start)

InitSolve empty_start =: 1


NB.board = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
NB.RemovePeg(board,empty_start)
NB.for peg,over,land in Solution:
NB.  RemovePeg(board,peg)
NB.  RemovePeg(board,over)
NB.  AddPeg(board,land) # board order changes!
NB.  DrawBoard(board)
NB.  print "Peg %X jumped over %X to land on %X\n" % (peg,over,land)


(3 : 0) PEGS RemovePeg empty_start
 board =. y
 horizontal =. DrawBoard board
 for_POL. Solution do.
  'peg over land' =. POL
  board =. board move POL
  horizontal =. horizontal , DrawBoard board
  smoutput 'Peg ',(":peg),' jumped over ',(":over),' to land on ',(":land)
 end.
 smoutput horizontal
 NB. Solution NB. return Solution however Solution is global.
)

```

Example linux session with program in file CrackerBarrel.ijs

```txt

ubuntu$ ijconsole CrackerBarrel.ijs
Peg 4 jumped over 2 to land on 1
Peg 6 jumped over 5 to land on 4
Peg 1 jumped over 3 to land on 6
Peg 7 jumped over 4 to land on 2
Peg 12 jumped over 8 to land on 5
Peg 14 jumped over 13 to land on 12
Peg 6 jumped over 9 to land on 13
Peg 2 jumped over 5 to land on 9
Peg 12 jumped over 13 to land on 14
Peg 15 jumped over 10 to land on 6
Peg 6 jumped over 9 to land on 13
Peg 14 jumped over 13 to land on 12
Peg 11 jumped over 12 to land on 13
┌──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┐
│    .     │    1     │    1     │    .     │    .     │    .     │    .     │    .     │    .     │    .     │    .     │    .     │    .     │    .     │
│   2 3    │   . 3    │   . 3    │   . .    │   2 .    │   2 .    │   2 .    │   2 .    │   . .    │   . .    │   . .    │   . .    │   . .    │   . .    │
│  4 5 6   │  . 5 6   │  4 . .   │  4 . 6   │  . . 6   │  . 5 6   │  . 5 6   │  . 5 .   │  . . .   │  . . .   │  . . 6   │  . . .   │  . . .   │  . . .   │
│ 7 8 9 A  │ 7 8 9 A  │ 7 8 9 A  │ 7 8 9 A  │ . 8 9 A  │ . . 9 A  │ . . 9 A  │ . . . A  │ . . 9 A  │ . . 9 A  │ . . 9 .  │ . . . .  │ . . . .  │ . . . .  │
│B C D E F │B C D E F │B C D E F │B C D E F │B C D E F │B . D E F │B C . . F │B C D . F │B C D . F │B . . E F │B . . E . │B . D E . │B C . . . │. . D . . │
└──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┘
   JVERSION
Engine: j701/2011-01-10/11:25
Library: 8.02.12
Platform: Linux 64
Installer: unknown
InstallPath: /usr/share/j/8.0.2
   exit 0
ubuntu$ 

```




## Julia

```julia
moves = [[1, 2, 4], [1, 3, 6], [2, 4, 7], [2, 5, 9], [3, 5, 8], [3, 6, 10], [4, 5, 6],
         [4, 7, 11], [4, 8, 13], [5, 8, 12], [5, 9, 14], [6, 9, 13], [6, 10, 15],
         [7, 8, 9], [8, 9, 10],  [11, 12, 13], [12, 13, 14], [13, 14, 15]]
    
triangletext(v) = join(map(i -> " "^([6,4,3,1,0][i]) * join(map(x -> rpad(x, 3), 
    v[div(i*i-i+2,2):div(i*(i+1),2)]), ""), 1:5), "\n")

const solutiontext = ["Starting board:\n" * triangletext([0; ones(Int, 14)]) * "\n"]

function solve(mv, turns=1, bd=[0; ones(Int, 14)])
    if turns + 1 == length(bd)
        return true
    elseif bd[mv[2]] == 0 || (bd[mv[1]] == 0 && bd[mv[3]] == 0) || (bd[mv[3]] == 1 && bd[mv[1]] == 1)
        return false
    else
        movetext = "\nmove " * (bd[mv[1]] == 0 ? "$(mv[3]) to $(mv[1])" : "$(mv[1]) to $(mv[3])")
        newboard = deepcopy(bd)
        map(i -> newboard[i] = 1 - newboard[i], mv)
        for move in moves
            if solve(move, turns + 1, newboard)
                push!(solutiontext, (movetext * "\n" * triangletext(newboard) * "\n"))
                return true
            end
        end
    end
    false
end

for (i, move) in enumerate(moves)
    if solve(move)
        println(join([solutiontext[1]; reverse(solutiontext[2:end])], ""))
        break
    elseif i == length(moves) 
        println("No solution found.")
    end
end

```
```txt

Starting board:
      0
    1  1
   1  1  1
 1  1  1  1
1  1  1  1  1

move 4 to 1
      1
    0  1
   0  1  1
 1  1  1  1
1  1  1  1  1

move 9 to 2
      1
    1  1
   0  0  1
 1  1  0  1
1  1  1  1  1

move 11 to 4
      1
    1  1
   1  0  1
 0  1  0  1
0  1  1  1  1

move 2 to 7
      1
    0  1
   0  0  1
 1  1  0  1
0  1  1  1  1

move 12 to 5
      1
    0  1
   0  1  1
 1  0  0  1
0  0  1  1  1

move 3 to 8
      1
    0  0
   0  0  1
 1  1  0  1
0  0  1  1  1

move 10 to 3
      1
    0  1
   0  0  0
 1  1  0  0
0  0  1  1  1

move 1 to 6
      0
    0  0
   0  0  1
 1  1  0  0
0  0  1  1  1

move 7 to 9
      0
    0  0
   0  0  1
 0  0  1  0
0  0  1  1  1

move 14 to 12
      0
    0  0
   0  0  1
 0  0  1  0
0  1  0  0  1

move 6 to 13
      0
    0  0
   0  0  0
 0  0  0  0
0  1  1  0  1

move 12 to 14
      0
    0  0
   0  0  0
 0  0  0  0
0  0  0  1  1

move 15 to 13
      0
    0  0
   0  0  0
 0  0  0  0
0  0  1  0  0

```



## Kotlin

```scala
// version 1.1.3

data class Solution(val peg: Int, val over: Int, val land: Int)

var board = BooleanArray(16) { if (it == 0) false else true }

val jumpMoves = listOf(
    listOf(),
    listOf( 2 to  4,  3 to  6),
    listOf( 4 to  7,  5 to  9),
    listOf( 5 to  8,  6 to 10),
    listOf( 2 to  1,  5 to  6,  7 to 11,  8 to 13),
    listOf( 8 to 12,  9 to 14),
    listOf( 3 to  1,  5 to  4,  9 to 13, 10 to 15),
    listOf( 4 to  2,  8 to  9),
    listOf( 5 to  3,  9 to 10),
    listOf( 5 to  2,  8 to  7),
    listOf( 9 to  8),
    listOf(12 to 13),
    listOf( 8 to  5, 13 to 14),
    listOf( 8 to  4,  9 to  6, 12 to 11, 14 to 15),
    listOf( 9 to  5, 13 to 12),
    listOf(10 to  6, 14 to 13)
)

val solutions = mutableListOf<Solution>()

fun drawBoard() {
    val pegs = CharArray(16) { '-' }
    for (i in 1..15) if (board[i]) pegs[i] = "%X".format(i)[0]
    println("       %c".format(pegs[1]))
    println("      %c %c".format(pegs[2], pegs[3]))
    println("     %c %c %c".format(pegs[4], pegs[5], pegs[6]))
    println("    %c %c %c %c".format(pegs[7], pegs[8], pegs[9], pegs[10]))
    println("   %c %c %c %c %c".format(pegs[11], pegs[12], pegs[13], pegs[14], pegs[15])) 
}

val solved get() = board.count { it } == 1  // just one peg left

fun solve() {
    if (solved) return
    for (peg in 1..15) {
        if (board[peg]) {
            for ((over, land) in jumpMoves[peg]) {
                if (board[over] && !board[land]) {
                    val saveBoard = board.copyOf()
                    board[peg]  = false
                    board[over] = false
                    board[land] = true 
                    solutions.add(Solution(peg, over, land))
                    solve()
                    if (solved) return // otherwise back-track
                    board = saveBoard 
                    solutions.removeAt(solutions.lastIndex)
                }           
            }
        }
    }
} 
 
fun main(args: Array<String>) {
    val emptyStart = 1
    board[emptyStart] = false
    solve()
    board = BooleanArray(16) { if (it == 0) false else true }
    board[emptyStart] = false 
    drawBoard()
    println("Starting with peg %X removed\n".format(emptyStart)) 
    for ((peg, over, land) in solutions) {
        board[peg]  = false
        board[over] = false
        board[land] = true
        drawBoard()
        println("Peg %X jumped over %X to land on %X\n".format(peg, over, land))
    }
}
```


```txt

       -
      2 3
     4 5 6
    7 8 9 A
   B C D E F
Starting with peg 1 removed

       1
      - 3
     - 5 6
    7 8 9 A
   B C D E F
Peg 4 jumped over 2 to land on 1

       1
      - 3
     4 - -
    7 8 9 A
   B C D E F
Peg 6 jumped over 5 to land on 4

       -
      - -
     4 - 6
    7 8 9 A
   B C D E F
Peg 1 jumped over 3 to land on 6

       -
      2 -
     - - 6
    - 8 9 A
   B C D E F
Peg 7 jumped over 4 to land on 2

       -
      2 -
     - 5 6
    - - 9 A
   B - D E F
Peg C jumped over 8 to land on 5

       -
      2 -
     - 5 6
    - - 9 A
   B C - - F
Peg E jumped over D to land on C

       -
      2 -
     - 5 -
    - - - A
   B C D - F
Peg 6 jumped over 9 to land on D

       -
      - -
     - - -
    - - 9 A
   B C D - F
Peg 2 jumped over 5 to land on 9

       -
      - -
     - - -
    - - 9 A
   B - - E F
Peg C jumped over D to land on E

       -
      - -
     - - 6
    - - 9 -
   B - - E -
Peg F jumped over A to land on 6

       -
      - -
     - - -
    - - - -
   B - D E -
Peg 6 jumped over 9 to land on D

       -
      - -
     - - -
    - - - -
   B C - - -
Peg E jumped over D to land on C

       -
      - -
     - - -
    - - - -
   - - D - -
Peg B jumped over C to land on D

```



## Perl

```perl
@start = qw<
        0
       1 1
      1 1 1
     1 1 1 1
    1 1 1 1 1
>;

@moves = (
    [ 0, 1, 3], [ 0, 2, 5], [ 1, 3, 6],
    [ 1, 4, 8], [ 2, 4, 7], [ 2, 5, 9],
    [ 3, 4, 5], [ 3, 6,10], [ 3, 7,12],
    [ 4, 7,11], [ 4, 8,13], [ 5, 8,12],
    [ 5, 9,14], [ 6, 7, 8], [ 7, 8, 9],
    [10,11,12], [11,12,13], [12,13,14]
);

$format .= (" " x (5-$_)) . ("%d " x $_) . "\n" for 1..5;

sub solve {
    my ($move, $turns, @board) = @_;
    $turns = 1 unless $turns;
    return "\nSolved" if $turns + 1 == @board;
    return undef if $board[$$move[1]] == 0;
    my $valid = do  {
        if ($board[$$move[0]] == 0) {
            return undef if $board[$$move[2]] == 0;
            "\nmove $$move[2] to $$move[0]\n";
        } else {
            return undef if $board[$$move[2]] == 1;
            "\nmove $$move[0] to $$move[2]\n";
        }
    };

    my $new_result;
    my @new_layout = @board;
    @new_layout[$_] = 1 - @new_layout[$_] for @$move;
    for $this_move (@moves) {
        $new_result = solve(\@$this_move, $turns + 1, @new_layout);
        last if $new_result
    }
    $new_result ? "$valid\n" . sprintf($format, @new_layout) . $new_result : $new_result}

$result = "Starting with\n\n" . sprintf($format, @start), "\n";

for $this_move (@moves) {
    $result .= solve(\@$this_move, 1, @start);
    last if $result
}

print $result ? $result : "No solution found";

```

<pre style="height:60ex;overflow:scroll;">Starting with

    0
   1 1
  1 1 1
 1 1 1 1
1 1 1 1 1

move 3 to 0

    1
   0 1
  0 1 1
 1 1 1 1
1 1 1 1 1

move 8 to 1

    1
   1 1
  0 0 1
 1 1 0 1
1 1 1 1 1

move 10 to 3

    1
   1 1
  1 0 1
 0 1 0 1
0 1 1 1 1

move 1 to 6

    1
   0 1
  0 0 1
 1 1 0 1
0 1 1 1 1

move 11 to 4

    1
   0 1
  0 1 1
 1 0 0 1
0 0 1 1 1

move 2 to 7

    1
   0 0
  0 0 1
 1 1 0 1
0 0 1 1 1

move 9 to 2

    1
   0 1
  0 0 0
 1 1 0 0
0 0 1 1 1

move 0 to 5

    0
   0 0
  0 0 1
 1 1 0 0
0 0 1 1 1

move 6 to 8

    0
   0 0
  0 0 1
 0 0 1 0
0 0 1 1 1

move 13 to 11

    0
   0 0
  0 0 1
 0 0 1 0
0 1 0 0 1

move 5 to 12

    0
   0 0
  0 0 0
 0 0 0 0
0 1 1 0 1

move 11 to 13

    0
   0 0
  0 0 0
 0 0 0 0
0 0 0 1 1

move 14 to 12

    0
   0 0
  0 0 0
 0 0 0 0
0 0 1 0 0

Solved
```


## Perl 6

```perl6

constant @start =  <
        0
       1 1
      1 1 1
     1 1 1 1
    1 1 1 1 1
>».Int;

constant @moves =
    [ 0, 1, 3],[ 0, 2, 5],[ 1, 3, 6],
    [ 1, 4, 8],[ 2, 4, 7],[ 2, 5, 9],
    [ 3, 4, 5],[ 3, 6,10],[ 3, 7,12],
    [ 4, 7,11],[ 4, 8,13],[ 5, 8,12],
    [ 5, 9,14],[ 6, 7, 8],[ 7, 8, 9],
    [10,11,12],[11,12,13],[12,13,14];

my $format = (1..5).map: {' ' x 5-$_, "%d " x $_, "\n"};

sub solve(@board, @move) {
    return "   Solved" if @board.sum == 1;
    return Nil if @board[@move[1]] == 0;
    my $valid = do given @board[@move[0]] {
        when 0 {
            return Nil if @board[@move[2]] == 0;
            "move {@move[2]} to {@move[0]}\n ";
        }
        default {
            return Nil if @board[@move[2]] == 1;
            "move {@move[0]} to {@move[2]}\n ";
        }
    }

    my @new-layout = @board;
    @new-layout[$_] = 1 - @new-layout[$_] for @move;
    my $result;
    for @moves -> @this-move {
        $result = solve(@new-layout, @this-move);
        last if $result
    }
    $result ?? "$valid\n " ~ sprintf($format, |@new-layout) ~ $result !! $result
}

print "Starting with\n ", sprintf($format, |@start);

my $result;
for @moves -> @this-move {
    $result = solve(@start, @this-move);
    last if $result
};
say $result ?? $result !! "No solution found";
```

<pre style="height:60ex;overflow:scroll;">Starting with
      0  
     1 1  
    1 1 1  
   1 1 1 1  
  1 1 1 1 1  
move 3 to 0
 
      1  
     0 1  
    0 1 1  
   1 1 1 1  
  1 1 1 1 1  
move 8 to 1
 
      1  
     1 1  
    0 0 1  
   1 1 0 1  
  1 1 1 1 1  
move 10 to 3
 
      1  
     1 1  
    1 0 1  
   0 1 0 1  
  0 1 1 1 1  
move 1 to 6
 
      1  
     0 1  
    0 0 1  
   1 1 0 1  
  0 1 1 1 1  
move 11 to 4
 
      1  
     0 1  
    0 1 1  
   1 0 0 1  
  0 0 1 1 1  
move 2 to 7
 
      1  
     0 0  
    0 0 1  
   1 1 0 1  
  0 0 1 1 1  
move 9 to 2
 
      1  
     0 1  
    0 0 0  
   1 1 0 0  
  0 0 1 1 1  
move 0 to 5
 
      0  
     0 0  
    0 0 1  
   1 1 0 0  
  0 0 1 1 1  
move 6 to 8
 
      0  
     0 0  
    0 0 1  
   0 0 1 0  
  0 0 1 1 1  
move 13 to 11
 
      0  
     0 0  
    0 0 1  
   0 0 1 0  
  0 1 0 0 1  
move 5 to 12
 
      0  
     0 0  
    0 0 0  
   0 0 0 0  
  0 1 1 0 1  
move 11 to 13
 
      0  
     0 0  
    0 0 0  
   0 0 0 0  
  0 0 0 1 1  
move 14 to 12
 
      0  
     0 0  
    0 0 0  
   0 0 0 0  
  0 0 1 0 0  
   Solved

```



## Phix

Twee brute-force string-based solution. Backtracks a mere 366 times, whereas starting with the 5th peg missing backtracks 19388 times (all in 0s, obvs).

```Phix
--
-- demo\rosetta\IQpuzzle.exw
--
constant moves = {-11,-9,2,11,9,-2}
function solve(string board, integer left)
    if left=1 then return "" end if
    for i=1 to length(board) do
        if board[i]='1' then
            for j=1 to length(moves) do
                integer mj = moves[j], over = i+mj, tgt = i+2*mj
                if tgt>=1 and tgt<=length(board) 
                and board[tgt]='0' and board[over]='1' then
                    {board[i],board[over],board[tgt]} = "001"
                    string res = solve(board,left-1)
                    if length(res)!=4 then return board&res end if
                    {board[i],board[over],board[tgt]} = "110"
                end if
            end for
        end if
    end for
    return "oops"
end function
 
sequence start = """
----0----
---1-1---
--1-1-1--
-1-1-1-1-
1-1-1-1-1
"""
puts(1,substitute(join_by(split(start&solve(start,14),'\n'),5,7),"-"," "))
```

```txt

    0           1           1           0           0           0           0
   1 1         0 1         0 1         0 0         1 0         1 1         1 1
  1 1 1       0 1 1       1 0 0       1 0 1       0 0 1       0 0 0       0 1 0
 1 1 1 1     1 1 1 1     1 1 1 1     1 1 1 1     0 1 1 1     0 1 1 0     0 0 1 0
1 1 1 1 1   1 1 1 1 1   1 1 1 1 1   1 1 1 1 1   1 1 1 1 1   1 1 1 1 1   1 0 1 1 1

    0           0           0           0           0           0           0
   1 1         0 1         0 0         0 0         0 0         0 0         0 0
  0 1 1       0 0 1       0 0 0       0 0 1       0 0 0       0 0 0       0 0 0
 0 0 0 0     0 0 1 0     0 0 1 1     0 0 1 0     0 0 0 0     0 0 0 0     0 0 0 0
1 0 0 1 1   1 0 0 1 1   1 0 0 1 1   1 0 0 1 0   1 0 1 1 0   1 1 0 0 0   0 0 1 0 0

```

Adapted to the English game:

```Phix
constant moves = {-2,15,2,-15}
function solve(string board, integer left)
    if left=1 then
--      return ""   -- (leaves it on the edge)
        if board[3*15+8]='.' then return "" end if
        return "oops"
    end if
    for i=1 to length(board) do
        if board[i]='.' then
            for j=1 to length(moves) do
                integer mj = moves[j], over = i+mj, tgt = i+2*mj
                if tgt>=1 and tgt<=length(board) 
                and board[tgt]='o' and board[over]='.' then
                    {board[i],board[over],board[tgt]} = "oo."
                    string res = solve(board,left-1)
                    if length(res)!=4 then return board&res end if
                    {board[i],board[over],board[tgt]} = "..o"
                end if
            end for
        end if
    end for
    return "oops"
end function
 
sequence start = """
-----.-.-.----
-----.-.-.----
-.-.-.-.-.-.-.
-.-.-.-o-.-.-.
-.-.-.-.-.-.-.
-----.-.-.----
-----.-.-.----
"""
puts(1,substitute(join_by(split(start&solve(start,32),'\n'),7,8),"-"," "))
```

```txt

     . . .            . . .            . . .            o . .            . o o            . o o            . o o            . o .   
     . . .            . o .            . o .            o o .            o o .            o o .            o o .            o o o   
 . . . . . . .    . . . o . . .    . o o . . . .    . o . . . . .    . o . . . . .    . . o o . . .    o o . o . . .    o o . o o . .
 . . . o . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .
 . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .
     . . .            . . .            . . .            . . .            . . .            . . .            . . .            . . .   
     . . .            . . .            . . .            . . .            . . .            . . .            . . .            . . .   

     . o .            . o .            o o .            o o .            o o .            o o .            o o .            o o .   
     o o o            . o o            o o o            o o o            . o o            . o o            . o o            . o .   
 o o . o . o o    o o o o . o o    o o . o . o o    o o . o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o o o o
 . . . . . . .    . . o . . . .    . . o . . . .    o o . . . . .    o o o . . . .    o o . o o . .    o o . o . o o    o o . o o o o
 . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .    . . . . . . .
     . . .            . . .            . . .            . . .            . . .            . . .            . . .            . . .   
     . . .            . . .            . . .            . . .            . . .            . . .            . . .            . . .   

     o o o            o o o            o o o            o o o            o o o            o o o            o o o            o o o   
     . o o            . o o            o o o            o o o            o o o            o o o            o o o            o o o   
 o o o o . o o    o o . o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o . o o
 o o . o o o o    o o o o o o o    o o . o o o o    o o . o o o o    o o . o o o o    o o . o . o o    o o . o . o o    o o . o . o o
 . . . . . . .    . . o . . . .    . . o . . . .    o o . . . . .    o . o o . . .    o . o o o . .    o . o o . o o    o . . o . o o
     . . .            . . .            . . .            . . .            . . .            . . o            . . o            o . o   
     . . .            . . .            . . .            . . .            . . .            . . .            . . .            o . .   

     o o o            o o o            o o o            o o o            o o o            o o o            o o o            o o o   
     o o o            o o o            o o o            o o o            o o o            o o o            o o o            o o o   
 o o o o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o o o o    o o o o o o o    o o o o o o o
 o o o o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o . o o    o o o o o o o    o o o o o o o    o o o . o o o
 o . o o . o o    o . o o . o o    o . . o . o o    o o o . . o o    o o o o o . o    o o o o . . o    o o o . o o o    o o o o o o o
     . . o            . . o            o . o            o . o            o . o            o . o            o . o            o o o   
     o . .            . o o            o o o            o o o            o o o            o o o            o o o            o o o   

```



## Prolog

Works with SWI-Prolog and module(lambda).


```Prolog
:- use_module(library(lambda)).

iq_puzzle :-
	iq_puzzle(Moves),
	display(Moves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compute solution
%
iq_puzzle(Moves) :-
	play([1], [2,3,4,5,6,7,8,9,10,11,12,13,14,15], [], Moves).

play(_, [_], Lst, Moves) :-
	reverse(Lst, Moves).

play(Free, Occupied, Lst, Moves) :-
	select(S, Occupied, Oc1),
	select(O, Oc1, Oc2),
	select(E, Free, F1),
	move(S, O, E),
	play([S, O | F1], [E | Oc2], [move(S,O,E) | Lst], Moves).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% allowed moves
%
move(S,2,E) :-
	member([S,E], [[1,4], [4,1]]).
move(S,3,E) :-
	member([S,E], [[1,6], [6,1]]).
move(S,4,E):-
	member([S,E], [[2,7], [7,2]]).
move(S,5,E):-
	member([S,E], [[2,9], [9,2]]).
move(S,5,E):-
	member([S,E], [[3,8], [8,3]]).
move(S,6,E):-
	member([S,E], [[3,10], [10,3]]).
move(S,5,E):-
	member([S,E], [[4,6], [6,4]]).
move(S,7,E):-
	member([S,E], [[4,11], [11,4]]).
move(S,8,E):-
	member([S,E], [[4,13], [13,4]]).
move(S,8,E):-
	member([S,E], [[5,12], [12,5]]).
move(S,9,E):-
	member([S,E], [[5,14], [14,5]]).
move(S,9,E):-
	member([S,E], [[6,13], [13,6]]).
move(S,10,E):-
	member([S,E], [[6,15], [15,6]]).
move(S,8,E):-
	member([S,E], [[9,7], [7,9]]).
move(S,9,E):-
	member([S,E], [[10,8], [8,10]]).
move(S,12,E):-
	member([S,E], [[11,13], [13,11]]).
move(S,13,E):-
	member([S,E], [[12,14], [14,12]]).
move(S,14,E):-
	member([S,E], [[15,13], [13,15]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% display soluce
%
display(Sol) :-
	display(Sol, [1]).

display([], Free) :-
	numlist(1,15, Lst),
	maplist(\X^I^(member(X, Free) -> I = 0; I = 1),
		Lst,
		[I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15]),
	format('    ~w        ~n', [I1]),
	format('   ~w ~w      ~n', [I2,I3]),
	format('  ~w ~w ~w    ~n', [I4,I5,I6]),
	format(' ~w ~w ~w ~w  ~n', [I7,I8,I9,I10]),
	format('~w ~w ~w ~w ~w~n', [I11,I12,I13,I14,I15]),
	writeln(solved).


display([move(Start, Middle, End) | Tail], Free) :-
	numlist(1,15, Lst),
	maplist(\X^I^(member(X, Free) -> I = 0; I = 1),
		Lst,
		[I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15]),
	format('    ~w        ~n', [I1]),
	format('   ~w ~w      ~n', [I2,I3]),
	format('  ~w ~w ~w    ~n', [I4,I5,I6]),
	format(' ~w ~w ~w ~w  ~n', [I7,I8,I9,I10]),
	format('~w ~w ~w ~w ~w~n', [I11,I12,I13,I14,I15]),
	format('From ~w to ~w over ~w~n~n~n', [Start, End, Middle]),
	select(End, Free, F1),
	display(Tail,  [Start, Middle | F1]).

```

Output :

```txt
 ?- iq_puzzle.
    0        
   1 1      
  1 1 1    
 1 1 1 1  
1 1 1 1 1
From 4 to 1 over 2


    1        
   0 1      
  0 1 1    
 1 1 1 1  
1 1 1 1 1
From 6 to 4 over 5


    1        
   0 1      
  1 0 0    
 1 1 1 1  
1 1 1 1 1
From 1 to 6 over 3


    0        
   0 0      
  1 0 1    
 1 1 1 1  
1 1 1 1 1
From 7 to 2 over 4


    0        
   1 0      
  0 0 1    
 0 1 1 1  
1 1 1 1 1
From 10 to 3 over 6


    0        
   1 1      
  0 0 0    
 0 1 1 0  
1 1 1 1 1
From 12 to 5 over 8


    0        
   1 1      
  0 1 0    
 0 0 1 0  
1 0 1 1 1
From 13 to 6 over 9


    0        
   1 1      
  0 1 1    
 0 0 0 0  
1 0 0 1 1
From 3 to 10 over 6


    0        
   1 0      
  0 1 0    
 0 0 0 1  
1 0 0 1 1
From 2 to 9 over 5


    0        
   0 0      
  0 0 0    
 0 0 1 1  
1 0 0 1 1
From 15 to 6 over 10


    0        
   0 0      
  0 0 1    
 0 0 1 0  
1 0 0 1 0
From 6 to 13 over 9


    0        
   0 0      
  0 0 0    
 0 0 0 0  
1 0 1 1 0
From 14 to 12 over 13


    0        
   0 0      
  0 0 0    
 0 0 0 0  
1 1 0 0 0
From 11 to 13 over 12


    0        
   0 0      
  0 0 0    
 0 0 0 0  
0 0 1 0 0
solved

```

Bonus : number of solutions :

```txt
 ?- setof(L, iq_puzzle(L), LL), length(LL, Len).
LL = [[move(4, 2, 1), move(6, 5, 4), move(1, 3, 6), move(7, 4, 2), move(10, 6, 3), move(12, 8, 5), move(13, 9, 6), move(..., ..., ...)|...], [move(4, 2, 1), move(6, 5, 4), move(1, 3, 6), move(7, 4, 2), move(10, 6, 3), move(12, 8, 5), move(..., ..., ...)|...], [move(4, 2, 1), move(6, 5, 4), move(1, 3, 6), move(7, 4, 2), move(10, 6, 3), move(..., ..., ...)|...], [move(4, 2, 1), move(6, 5, 4), move(1, 3, 6), move(7, 4, 2), move(..., ..., ...)|...], [move(4, 2, 1), move(6, 5, 4), move(1, 3, 6), move(..., ..., ...)|...], [move(4, 2, 1), move(6, 5, 4), move(..., ..., ...)|...], [move(4, 2, 1), move(..., ..., ...)|...], [move(..., ..., ...)|...], [...|...]|...],
Len = 29760.

```



## Python



```Python
#
# Draw board triangle in ascii
#
def DrawBoard(board):
  peg = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  for n in xrange(1,16):
    peg[n] = '.'
    if n in board:
      peg[n] = "%X" % n
  print "     %s" % peg[1]
  print "    %s %s" % (peg[2],peg[3])
  print "   %s %s %s" % (peg[4],peg[5],peg[6])
  print "  %s %s %s %s" % (peg[7],peg[8],peg[9],peg[10])
  print " %s %s %s %s %s" % (peg[11],peg[12],peg[13],peg[14],peg[15])
#

# remove peg n from board
def RemovePeg(board,n):
  board.remove(n)

# Add peg n on board
def AddPeg(board,n):
  board.append(n)

# return true if peg N is on board else false is empty position
def IsPeg(board,n):
  return n in board

# A dictionary of valid jump moves index by jumping peg
# then a list of moves where move has jumpOver and LandAt positions
JumpMoves = { 1: [ (2,4),(3,6) ],  # 1 can jump over 2 to land on 4, or jumper over 3 to land on 6
              2: [ (4,7),(5,9)  ],
              3: [ (5,8),(6,10) ],
              4: [ (2,1),(5,6),(7,11),(8,13) ],
              5: [ (8,12),(9,14) ],
              6: [ (3,1),(5,4),(9,13),(10,15) ],
              7: [ (4,2),(8,9)  ],
              8: [ (5,3),(9,10) ],
              9: [ (5,2),(8,7)  ],
             10: [ (9,8) ],
             11: [ (12,13) ],
             12: [ (8,5),(13,14) ],
             13: [ (8,4),(9,6),(12,11),(14,15) ],
             14: [ (9,5),(13,12)  ],
             15: [ (10,6),(14,13) ]
            }

Solution = []
#
# Recursively solve the problem
#
def Solve(board):
  #DrawBoard(board)
  if len(board) == 1:
    return board # Solved one peg left
  # try a move for each peg on the board
  for peg in xrange(1,16): # try in numeric order not board order
    if IsPeg(board,peg):
      movelist = JumpMoves[peg]
      for over,land in movelist:
        if IsPeg(board,over) and not IsPeg(board,land):
          saveboard = board[:] # for back tracking
          RemovePeg(board,peg)
          RemovePeg(board,over)
          AddPeg(board,land) # board order changes!

          Solution.append((peg,over,land))

          board = Solve(board)
          if len(board) == 1:
            return board
        ## undo move and back track when stuck!
          board = saveboard[:] # back track
          del Solution[-1] # remove last move
  return board

#
# Remove one peg and start solving
#
def InitSolve(empty):
  board = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
  RemovePeg(board,empty_start)
  Solve(board)

#
empty_start = 1
InitSolve(empty_start)

board = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
RemovePeg(board,empty_start)
for peg,over,land in Solution:
  RemovePeg(board,peg)
  RemovePeg(board,over)
  AddPeg(board,land) # board order changes!
  DrawBoard(board)
  print "Peg %X jumped over %X to land on %X\n" % (peg,over,land)
```


```txt

     1
    . 3
   . 5 6
  7 8 9 A
 B C D E F
Peg 4 jumped over 2 to land on 1

     1
    . 3
   4 . .
  7 8 9 A
 B C D E F
Peg 6 jumped over 5 to land on 4

     .
    . .
   4 . 6
  7 8 9 A
 B C D E F
Peg 1 jumped over 3 to land on 6

     .
    2 .
   . . 6
  . 8 9 A
 B C D E F
Peg 7 jumped over 4 to land on 2

     .
    2 .
   . 5 6
  . . 9 A
 B . D E F
Peg C jumped over 8 to land on 5

     .
    2 .
   . 5 6
  . . 9 A
 B C . . F
Peg E jumped over D to land on C

     .
    2 .
   . 5 .
  . . . A
 B C D . F
Peg 6 jumped over 9 to land on D

     .
    . .
   . . .
  . . 9 A
 B C D . F
Peg 2 jumped over 5 to land on 9

     .
    . .
   . . .
  . . 9 A
 B . . E F
Peg C jumped over D to land on E

     .
    . .
   . . 6
  . . 9 .
 B . . E .
Peg F jumped over A to land on 6

     .
    . .
   . . .
  . . . .
 B . D E .
Peg 6 jumped over 9 to land on D

     .
    . .
   . . .
  . . . .
 B C . . .
Peg E jumped over D to land on C

     .
    . .
   . . .
  . . . .
 . . D . .
Peg B jumped over C to land on D

```



## Racket

{{incorrect|Racket|Should the output start 6 jumps 3, then 15 jumps 10 ... rather than 1 jumps 3, then 6 jumps 10 ... ?
<br/>
Not so fast... The output is correct if one reads the statement differently.  The first number is the arrival<br/>position, the second number is the position where the peg is "jumped over" and is to be removed.<br/><br/>The position of where the peg jumps <b><i>from</i></b> is not indicated - but it can only be a single possibility in each case.}}
* This includes the code to generate the list of available hops (other implementations seem to have the table built in)
* It produces a full has containing all the possible results from all possible start positions (including ones without valid hops, and unusual starts). It takes no time... and once this is pre-calculated then some of the questions you might want answered about this puzzle can be more easily answered!

Oh and there are some useful triangle numbers functions thrown in for free!


```racket
#lang racket
(define << arithmetic-shift)
(define bwbs? bitwise-bit-set?)
;; 1,2,2,3,3,3,4,4,4,4,5,5,5,5,5
;; OEIS: A002024: n appears n times
(define (A002024 n) (exact-floor (+ 1/2 (sqrt (* n 2)))))
;; 1, 1, 2, 1, 2, 3, 1, 2, 3, 4
;; OEIS: A002260: Triangle T(n,k) = k for k = 1..n.
(define (A002260 n) (+ 1 (A002262 (sub1 n))))
;; OEIS: A000217: Triangular numbers: a(n) = C(n+1,2) = n(n+1)/2 = 0+1+2+...+n. 
(define (tri n) (* n (sub1 n) 1/2))
;; OEIS: A002262: Triangle read by rows: T(n,k)
(define (A002262 n)
  (define trinv (exact-floor (/ (+ 1 (sqrt (+ 1 (* n 8)))) 2)))
  (- n (/ (* trinv (- trinv 1)) 2)))
(define row-number A002024)
(define col-number A002260)
(define (r.c->n r c) (and (<= 1 r 5) (<= 1 c r) (+ 1 (tri r) (- c 1))))

(define (available-jumps n) ; takes a peg number, and returns a list of (jumped-peg . landing-site)
  (define r (row-number n))
  (define c (col-number n))
  ;; Six possible directions - although noone gets all six: "J" - landing site, "j" jumped peg
  ;;   Triangle   Row/column (square edge)
  ;;    A . B     A.B
  ;;   . a b      .ab
  ;;  C c X d D   CcXdD
  ;; . . e f      ..ef
  ;;. . E . F     ..E.F
  (define (N+.n+ r+ c+) (cons (r.c->n (+ r (* 2 r+)) (+ c (* 2 c+))) (r.c->n (+ r r+) (+ c c+))))
  (define-values (A.a B.b C.c D.d E.e F.f)
    (values (N+.n+ -1 -1) (N+.n+ -1 0) (N+.n+ 0 -1) (N+.n+ 0 1) (N+.n+ 1 0) (N+.n+ 1 1)))
  (filter car (list A.a B.b C.c D.d E.e F.f)))

(define (available-jumps/bits n0)
  (for/list ((A.a (available-jumps (add1 n0))))
    (match-define (cons (app sub1 A) (app sub1 a)) A.a)
    (list A a (bitwise-ior (<< 1 n0) (<< 1 A) (<< 1 a))))) ; on a hop, these three bits will flip

(define avalable-jumps-list/bits (for/vector #:length 15 ((bit 15)) (available-jumps/bits bit)))

;; OK -- we'll be complete about this (so it might take a little longer)
;;
;; There are 2^15 possible start configurations; so we'll just systematically go though them, and
;; build an hash of what can go where. Bits are numbered from 0 - peg#1 to 14 - peg#15.
;; It's overkill for finding a single solution, but it seems that Joe Nord needs a lot of questions
;; answered (which should be herein).
(define paths# (make-hash))
(for* ((board (in-range 0 (expt 2 15)))
       (peg (in-range 15))
       #:when (bwbs? board peg)
       (Jjf (in-list (vector-ref avalable-jumps-list/bits peg)))
       #:when (bwbs? board (second Jjf)) ; need something to jump
       #:unless (bwbs? board (first Jjf))) ; need a clear landing space
  (define board- (bitwise-xor board (third Jjf)))
  (hash-update! paths# board (λ (old) (cons (cons board- Jjf) old)) null))

(define (find-path start end (acc null))
  (if (= start end) (reverse acc)
      (for*/first
          ((hop (hash-ref paths# start null))
           (inr (in-value (find-path (car hop) end (cons hop acc)))) #:when inr) inr)))

(define (display-board board.Jjf)
  (match-define (list board (app add1 J) (app add1 j) _) board.Jjf)
  (printf "~a jumps ~a ->" J j)
  (for* ((r (in-range 1 6))
         (c (in-range 1 (add1 r)))
         (n (in-value (r.c->n r c))))
    (when (= c 1) (printf "~%~a" (make-string (quotient (* 5 (- 5 r)) 2) #\space)))
    (printf "[~a] " (~a #:width 2 #:pad-string " " #:align 'right (if (bwbs? board (sub1 n)) n ""))))
  (newline))

(define (flip-peg p b) (bitwise-xor (<< 1 (sub1 p)) b))
(define empty-board #b000000000000000)
(define full-board  #b111111111111111)

;; Solve #1 missing -> #13 left alone
(for-each display-board (find-path (flip-peg 1 full-board) (flip-peg 13 empty-board)))
```


```txt
1 jumps 3 ->
          [ 1] 
       [ 2] [  ] 
     [ 4] [ 5] [  ] 
  [ 7] [ 8] [ 9] [10] 
[11] [12] [13] [14] [15] 
6 jumps 10 ->
          [ 1] 
       [ 2] [  ] 
     [ 4] [ 5] [ 6] 
  [ 7] [ 8] [ 9] [  ] 
[11] [12] [13] [14] [  ] 
10 jumps 9 ->
          [ 1] 
       [ 2] [  ] 
     [ 4] [ 5] [ 6] 
  [ 7] [  ] [  ] [10] 
[11] [12] [13] [14] [  ] 
3 jumps 6 ->
          [ 1] 
       [ 2] [ 3] 
     [ 4] [ 5] [  ] 
  [ 7] [  ] [  ] [  ] 
[11] [12] [13] [14] [  ] 
9 jumps 5 ->
          [ 1] 
       [  ] [ 3] 
     [ 4] [  ] [  ] 
  [ 7] [  ] [ 9] [  ] 
[11] [12] [13] [14] [  ] 
5 jumps 9 ->
          [ 1] 
       [  ] [ 3] 
     [ 4] [ 5] [  ] 
  [ 7] [  ] [  ] [  ] 
[11] [12] [13] [  ] [  ] 
14 jumps 13 ->
          [ 1] 
       [  ] [ 3] 
     [ 4] [ 5] [  ] 
  [ 7] [  ] [  ] [  ] 
[11] [  ] [  ] [14] [  ] 
2 jumps 4 ->
          [ 1] 
       [ 2] [ 3] 
     [  ] [ 5] [  ] 
  [  ] [  ] [  ] [  ] 
[11] [  ] [  ] [14] [  ] 
8 jumps 5 ->
          [ 1] 
       [ 2] [  ] 
     [  ] [  ] [  ] 
  [  ] [ 8] [  ] [  ] 
[11] [  ] [  ] [14] [  ] 
4 jumps 2 ->
          [  ] 
       [  ] [  ] 
     [ 4] [  ] [  ] 
  [  ] [ 8] [  ] [  ] 
[11] [  ] [  ] [14] [  ] 
13 jumps 8 ->
          [  ] 
       [  ] [  ] 
     [  ] [  ] [  ] 
  [  ] [  ] [  ] [  ] 
[11] [  ] [13] [14] [  ] 
12 jumps 13 ->
          [  ] 
       [  ] [  ] 
     [  ] [  ] [  ] 
  [  ] [  ] [  ] [  ] 
[11] [12] [  ] [  ] [  ] 
13 jumps 12 ->
          [  ] 
       [  ] [  ] 
     [  ] [  ] [  ] 
  [  ] [  ] [  ] [  ] 
[  ] [  ] [13] [  ] [  ]
```



## Ruby



```ruby
# Solitaire Like Puzzle Solver - Nigel Galloway: October 18th., 2014
G = [[0,1,3],[0,2,5],[1,3,6],[1,4,8],[2,4,7],[2,5,9],[3,4,5],[3,6,10],[3,7,12],[4,7,11],[4,8,13],[5,8,12],[5,9,14],[6,7,8],[7,8,9],[10,11,12],[11,12,13],[12,13,14],
     [3,1,0],[5,2,0],[6,3,1],[8,4,1],[7,4,2],[9,5,2],[5,4,3],[10,6,3],[12,7,3],[11,7,4],[13,8,4],[12,8,5],[14,9,5],[8,7,6],[9,8,7],[12,11,10],[13,12,11],[14,13,12]]
FORMAT = (1..5).map{|i| " "*(5-i)+"%d "*i+"\n"}.join+"\n"
def solve n,i,g
  return "Solved" if i == 1
  return false unless n[g[0]]==0 and n[g[1]]==1 and n[g[2]]==1
    e = n.clone; g.each{|n| e[n] = 1 - e[n]}
    l=false; G.each{|g| l=solve(e,i-1,g); break if l}
  return l ? "#{g[0]} to #{g[2]}\n" + FORMAT % e + l : l
end
puts FORMAT % (N=[0,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
l=false; G.each{|g| l=solve(N,N.inject(:+),g); break if l}
puts l ? l : "No solution found"

```

<pre style="height:64ex;overflow:scroll">
    0 
   1 1 
  1 1 1 
 1 1 1 1 
1 1 1 1 1 

3 to 0
    1 
   0 1 
  0 1 1 
 1 1 1 1 
1 1 1 1 1 

8 to 1
    1 
   1 1 
  0 0 1 
 1 1 0 1 
1 1 1 1 1 

10 to 3
    1 
   1 1 
  1 0 1 
 0 1 0 1 
0 1 1 1 1 

1 to 6
    1 
   0 1 
  0 0 1 
 1 1 0 1 
0 1 1 1 1 

11 to 4
    1 
   0 1 
  0 1 1 
 1 0 0 1 
0 0 1 1 1 

2 to 7
    1 
   0 0 
  0 0 1 
 1 1 0 1 
0 0 1 1 1 

9 to 2
    1 
   0 1 
  0 0 0 
 1 1 0 0 
0 0 1 1 1 

0 to 5
    0 
   0 0 
  0 0 1 
 1 1 0 0 
0 0 1 1 1 

6 to 8
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 0 1 1 1 

13 to 11
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 1 0 0 1 

5 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 1 1 0 1 

11 to 13
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 0 1 1 

14 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 1 0 0 

Solved

```



## Sidef

```ruby
const N = [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

const G = [
    [ 0, 1, 3],[ 0, 2, 5],[ 1, 3, 6],
    [ 1, 4, 8],[ 2, 4, 7],[ 2, 5, 9],
    [ 3, 4, 5],[ 3, 6,10],[ 3, 7,12],
    [ 4, 7,11],[ 4, 8,13],[ 5, 8,12],
    [ 5, 9,14],[ 6, 7, 8],[ 7, 8, 9],
    [10,11,12],[11,12,13],[12,13,14],
]

const format = ({"#{' '*(5-_)}#{'%d '*_}\n"}.map(1..5).join + "\n")

func solve(n, i, g) is cached {
    i == N.end && return "Solved"
    n[g[1]] == 0 && return nil

    var s = given(n[g[0]]) {
        when(0) {
            n[g[2]] == 0 && return nil
            "#{g[2]} to #{g[0]}\n"
        }
        default {
            n[g[2]] == 1 && return nil
            "#{g[0]} to #{g[2]}\n"
        }
    }

    var a = n.clone
    g.each {|n| a[n] = 1-a[n] }
    var r = ''
    G.each {|g| (r = solve(a, i+1, g)) && break }
    r ? (s + (format % (a...)) + r) : r
}

format.printf(N...)

var r = ''
G.each {|g| (r = solve(N, 1, g)) && break }
say (r ? r : "No solution found")
```


<pre style="height:64ex;overflow:scroll">
    0 
   1 1 
  1 1 1 
 1 1 1 1 
1 1 1 1 1 

3 to 0
    1 
   0 1 
  0 1 1 
 1 1 1 1 
1 1 1 1 1 

8 to 1
    1 
   1 1 
  0 0 1 
 1 1 0 1 
1 1 1 1 1 

10 to 3
    1 
   1 1 
  1 0 1 
 0 1 0 1 
0 1 1 1 1 

1 to 6
    1 
   0 1 
  0 0 1 
 1 1 0 1 
0 1 1 1 1 

11 to 4
    1 
   0 1 
  0 1 1 
 1 0 0 1 
0 0 1 1 1 

2 to 7
    1 
   0 0 
  0 0 1 
 1 1 0 1 
0 0 1 1 1 

9 to 2
    1 
   0 1 
  0 0 0 
 1 1 0 0 
0 0 1 1 1 

0 to 5
    0 
   0 0 
  0 0 1 
 1 1 0 0 
0 0 1 1 1 

6 to 8
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 0 1 1 1 

13 to 11
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 1 0 0 1 

5 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 1 1 0 1 

11 to 13
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 0 1 1 

14 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 1 0 0 

Solved

```



## Visual Basic .NET

'''Notes:'''
This program uses a brute-force method with a string of 25 characters to internally represent the 15 spots on the peg board.  One can set the starting removed peg and intended last remaining peg by editing the header variable declarations named '''''Starting''''' and '''''Target'''''.  If  one doesn't care which spot the last peg lands on, the '''''Target''''' variable can be set to 0.  The constant '''''n''''' can be changed for different sized peg boards, for example with '''''n = 6''''' the peg board would have 21 positions.

```vbnet

Imports System, Microsoft.VisualBasic.DateAndTime

Public Module Module1
    Const n As Integer = 5 ' extent of board
    Dim Board As String ' the peg board
    Dim Starting As Integer = 1 ' position on board where first peg has been removed
    Dim Target As Integer = 13 ' final peg position, use 0 to solve for any postion
    Dim Moves As Integer() ' possible offset moves on grid
    Dim bi() As Integer ' string position to peg location index
    Dim ib() As Integer ' string position to peg location reverse index
    Dim nl As Char = Convert.ToChar(10) ' newline character

    ' expands each line of the board properly
    Public Function Dou(s As String) As String
        Dou = "" : Dim b As Boolean = True
        For Each ch As Char In s
            If b Then b = ch <> " "
            If b Then Dou &= ch & " " Else Dou = " " & Dou
        Next : Dou = Dou.TrimEnd()
    End Function

    ' formats the string representaion of a board into a viewable item
    Public Function Fmt(s As String) As String
        If s.Length < Board.Length Then Return s
        Fmt = "" : For i As Integer = 1 To n : Fmt &= Dou(s.Substring(i * n - n, n)) &
                If(i = n, s.Substring(Board.Length), "") & nl
        Next
    End Function

    ' returns triangular number of n
    Public Function Triangle(n As Integer) As Integer
        Return (n * (n + 1)) / 2
    End Function

    ' returns an initialized board with one peg missing
    Public Function Init(s As String, pos As Integer) As String
        Init = s : Mid(Init, pos, 1) = "0"
    End Function

    ' initializes string-to-board position indices			
    Public Sub InitIndex()
        ReDim bi(Triangle(n)), ib(n * n) : Dim j As Integer = 0
        For i As Integer = 0 To ib.Length - 1
            If i = 0 Then
                ib(i) = 0 : bi(j) = 0 : j += 1
            Else
                If Board(i - 1) = "1" Then ib(i) = j : bi(j) = i : j += 1
            End If
        Next
    End Sub

    ' brute-force solver, returns either the steps of a solution, or the string "fail"
    Public Function solve(brd As String, pegsLeft As Integer) As String
        If pegsLeft = 1 Then ' down to the last one, see if it's the correct one
            If Target = 0 Then Return "Completed" ' don't care where the last one is
            If brd(bi(Target) - 1) = "1" Then Return "Completed" Else Return "fail"
        End If
        For i = 1 To Board.Length ' for each possible position...
            If brd(i - 1) = "1" Then ' that still has a peg...
                For Each mj In Moves ' for each possible move
                    Dim over As Integer = i + mj ' the position to jump over
                    Dim land As Integer = i + 2 * mj ' the landing spot
                    ' ensure landing spot is on the board, then check for a valid pattern
                    If land >= 1 AndAlso land <= brd.Length _
                                AndAlso brd(land - 1) = "0" _
                                AndAlso brd(over - 1) = "1" Then
                        setPegs(brd, "001", i, over, land) ' make a move
                        ' recursively send it out to test
                        Dim Res As String = solve(brd.Substring(0, Board.Length), pegsLeft - 1)
                        ' check result, returing if OK
                        If Res.Length <> 4 Then _
                            Return brd & info(i, over, land) & nl & Res
                        setPegs(brd, "110", i, over, land) ' not OK, so undo the move
                    End If
                Next
            End If
        Next
        Return "fail"
    End Function

    ' returns a text representation of peg movement for each turn
    Function info(frm As Integer, over As Integer, dest As Integer) As String
        Return "  Peg from " & ib(frm).ToString() & " goes to " & ib(dest).ToString() &
            ", removing peg at " & ib(over).ToString()
    End Function

    ' sets three pegs as once, used for making and un-doing moves
    Sub setPegs(ByRef board As String, pat As String, a As Integer, b As Integer, c As Integer)
        Mid(board, a, 1) = pat(0) : Mid(board, b, 1) = pat(1) : Mid(board, c, 1) = pat(2)
    End Sub

    ' limit an integer to a range
    Sub LimitIt(ByRef x As Integer, lo As Integer, hi As Integer)
        x = Math.Max(Math.Min(x, hi), lo)
    End Sub

    Public Sub Main()
        Dim t As Integer = Triangle(n) ' use the nth triangular number for bounds
        LimitIt(Starting, 1, t) ' ensure valid parameters for staring and ending positions
        LimitIt(Target, 0, t)
        Dim stime As Date = Now() ' keep track of start time for performance result
        Moves = {-n - 1, -n, -1, 1, n, n + 1} ' possible offset moves on a nxn grid
        Board = New String("1", n * n) ' init string representation of board
        For i As Integer = 0 To n - 2 ' and declare non-existent spots
            Mid(Board, i * (n + 1) + 2, n - 1 - i) = New String(" ", n - 1 - i)
        Next
        InitIndex() ' create indicies from board's pattern
        Dim B As String = Init(Board, bi(Starting)) ' remove first peg
        Console.WriteLine(Fmt(B & "  Starting with peg removed from " & Starting.ToString()))
        Dim res As String() = solve(B.Substring(0, B.Length), t - 1).Split(nl)
        Dim ts As String = (Now() - stime).TotalMilliseconds.ToString() & " ms."
        If res(0).Length = 4 Then
            If Target = 0 Then
                Console.WriteLine("Unable to find a solution with last peg left anywhere.")
            Else
                Console.WriteLine("Unable to find a solution with last peg left at " &
                                  Target.ToString() & ".")
            End If
            Console.WriteLine("Computation time: " & ts)
        Else
            For Each Sol As String In res : Console.WriteLine(Fmt(Sol)) : Next
            Console.WriteLine("Computation time to first found solution: " & ts)
        End If
        If Diagnostics.Debugger.IsAttached Then Console.ReadLine()
    End Sub
End Module
```

'''A full solution:'''
<pre style="height:64ex;overflow:scroll">
    0
   1 1
  1 1 1
 1 1 1 1
1 1 1 1 1  Starting with peg removed from 1

    1
   0 1
  0 1 1
 1 1 1 1
1 1 1 1 1  Peg from 4 goes to 1, removing peg at 2

    1
   0 1
  1 0 0
 1 1 1 1
1 1 1 1 1  Peg from 6 goes to 4, removing peg at 5

    0
   0 0
  1 0 1
 1 1 1 1
1 1 1 1 1  Peg from 1 goes to 6, removing peg at 3

    0
   1 0
  0 0 1
 0 1 1 1
1 1 1 1 1  Peg from 7 goes to 2, removing peg at 4

    0
   1 1
  0 0 0
 0 1 1 0
1 1 1 1 1  Peg from 10 goes to 3, removing peg at 6

    0
   1 1
  0 1 0
 0 0 1 0
1 0 1 1 1  Peg from 12 goes to 5, removing peg at 8

    0
   1 1
  0 1 1
 0 0 0 0
1 0 0 1 1  Peg from 13 goes to 6, removing peg at 9

    0
   0 1
  0 0 1
 0 0 1 0
1 0 0 1 1  Peg from 2 goes to 9, removing peg at 5

    0
   0 0
  0 0 0
 0 0 1 1
1 0 0 1 1  Peg from 3 goes to 10, removing peg at 6

    0
   0 0
  0 0 1
 0 0 1 0
1 0 0 1 0  Peg from 15 goes to 6, removing peg at 10

    0
   0 0
  0 0 0
 0 0 0 0
1 0 1 1 0  Peg from 6 goes to 13, removing peg at 9

    0
   0 0
  0 0 0
 0 0 0 0
1 1 0 0 0  Peg from 14 goes to 12, removing peg at 13

    0
   0 0
  0 0 0
 0 0 0 0
0 0 1 0 0  Peg from 11 goes to 13, removing peg at 12

Completed
Computation time to first found solution: 15.6086 ms.

```

'''A failed solution:'''

```txt

    1
   0 1
  1 1 1
 1 1 1 1
1 1 1 1 1  Starting with peg removed from 2

Unable to find a solution with last peg left at 13.
Computation time: 1656.2754 ms.

```



## zkl

```zkl
var N=T(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1);
var G=T( T(0,1, 3), T(0,2, 5), T(1,3, 6), T( 1, 4, 8), T( 2, 4, 7), T( 2, 5, 9),
	 T(3,4, 5), T(3,6,10), T(3,7,12), T( 4, 7,11), T( 4, 8,13), T( 5, 8,12),
	 T(5,9,14), T(6,7, 8), T(7,8, 9), T(10,11,12), T(11,12,13), T(12,13,14));
 
fcn b2s(n){
   var fmt=[1..5].pump(String,fcn(i){ String(" "*(5 - i),"%d "*i,"\n") });
   fmt.fmt(n.xplode())
}
 
fcn solve(n,i,g){  // --> False|String
   if (i==N.len() - 1) return("\nSolved");
   if (n[g[1]]==0)     return(False);

   reg s;
   if (n[g[0]]==0){
      if(n[g[2]]==0) return(False);
      s="\n%d to %d\n".fmt(g[2],g[0]);
   } else {
      if(n[g[2]]==1) return(False);
      s="\n%d to %d\n".fmt(g[0],g[2]);
   }
 
   a:=n.copy();
   foreach gi in (g){ a[gi]=1 - a[gi]; }
   reg l;  // auto sets to Void
   foreach gi in (G){ if(l=solve(a,i + 1,gi)) break; }
   l and String(s,b2s(a),l)
}

b2s(N).print();

reg l;
foreach g in (G){ if(l=solve(N,1,g)) break; }
println(l and l or "No solution found.");
```

<pre style="height:32ex;overflow:scroll">
    0 
   1 1 
  1 1 1 
 1 1 1 1 
1 1 1 1 1 

3 to 0
    1 
   0 1 
  0 1 1 
 1 1 1 1 
1 1 1 1 1 

8 to 1
    1 
   1 1 
  0 0 1 
 1 1 0 1 
1 1 1 1 1 

10 to 3
    1 
   1 1 
  1 0 1 
 0 1 0 1 
0 1 1 1 1 

1 to 6
    1 
   0 1 
  0 0 1 
 1 1 0 1 
0 1 1 1 1 

11 to 4
    1 
   0 1 
  0 1 1 
 1 0 0 1 
0 0 1 1 1 

2 to 7
    1 
   0 0 
  0 0 1 
 1 1 0 1 
0 0 1 1 1 

9 to 2
    1 
   0 1 
  0 0 0 
 1 1 0 0 
0 0 1 1 1 

0 to 5
    0 
   0 0 
  0 0 1 
 1 1 0 0 
0 0 1 1 1 

6 to 8
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 0 1 1 1 

13 to 11
    0 
   0 0 
  0 0 1 
 0 0 1 0 
0 1 0 0 1 

5 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 1 1 0 1 

11 to 13
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 0 1 1 

14 to 12
    0 
   0 0 
  0 0 0 
 0 0 0 0 
0 0 1 0 0 

Solved

```

