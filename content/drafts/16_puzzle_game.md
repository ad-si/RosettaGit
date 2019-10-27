+++
title = "16 Puzzle Game"
description = ""
date = 2019-08-01T03:00:24Z
aliases = []
[extra]
id = 21790
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
[[Category:Puzzles]]
[[Category:Games]]


;Task:
Create 16 Puzzle Game. 


See details: [http://www.bsswebsite.me.uk/Puzzlewebsite/Fifteenpuzzle/sixteen.htm 16 Puzzle Game]


Video: [https://www.google.com/url?q=https%3A%2F%2F1drv.ms%2Fv%2Fs!AqDUIunCqVnIg0bsohQX8b5_dc5G&sa=D&sntz=1&usg=AFQjCNH_eKBfk_1m88kte3Wo7MIii8hn1A 16 Puzzle Game]


;Related task:
:*   [[15_Puzzle_Game|15 Puzzle Game]]





## Go


```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "math/rand"
    "os"
    "strings"
    "time"
)

const (
    easy = 1
    hard = 4
)

var n [16]int

func initGrid() {
    for i := 0; i < 16; i++ {
        n[i] = i + 1
    }
}

func setDiff(level int) {
    moves := 3
    if level == hard {
        moves = 12
    }
    rc := make([]int, 0, 4)
    for i := 0; i < moves; i++ {
        rc = rc[:0]
        r := rand.Intn(2)
        s := rand.Intn(4)
        if r == 0 { // rotate random row
            for j := s * 4; j < (s+1)*4; j++ {
                rc = append(rc, j)
            }
        } else { // rotate random column
            for j := s; j < s+16; j += 4 {
                rc = append(rc, j)
            }
        }
        var rca [4]int
        copy(rca[:], rc)
        rotate(rca)
        if hasWon() { // do it again
            i = -1
        }
    }
    fmt.Println("Target is", moves, "moves.")
}

func drawGrid() {
    fmt.Println()
    fmt.Println("     D1   D2   D3   D4")
    fmt.Println("   ╔════╦════╦════╦════╗")
    fmt.Printf("R1 ║ %2d ║ %2d ║ %2d ║ %2d ║ L1\n", n[0], n[1], n[2], n[3])
    fmt.Println("   ╠════╬════╬════╬════╣")
    fmt.Printf("R2 ║ %2d ║ %2d ║ %2d ║ %2d ║ L2\n", n[4], n[5], n[6], n[7])
    fmt.Println("   ╠════╬════╬════╬════╣")
    fmt.Printf("R3 ║ %2d ║ %2d ║ %2d ║ %2d ║ L3\n", n[8], n[9], n[10], n[11])
    fmt.Println("   ╠════╬════╬════╬════╣")
    fmt.Printf("R4 ║ %2d ║ %2d ║ %2d ║ %2d ║ L4\n", n[12], n[13], n[14], n[15])
    fmt.Println("   ╚════╩════╩════╩════╝")
    fmt.Println("     U1   U2   U3   U4\n")
}

func rotate(ix [4]int) {
    last := n[ix[3]]
    for i := 3; i >= 1; i-- {
        n[ix[i]] = n[ix[i-1]]
    }
    n[ix[0]] = last
}

func hasWon() bool {
    for i := 0; i < 16; i++ {
        if n[i] != i+1 {
            return false
        }
    }
    return true
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    initGrid()
    rand.Seed(time.Now().UnixNano())
    level := easy
    scanner := bufio.NewScanner(os.Stdin)
    for {
        fmt.Print("Enter difficulty level easy or hard E/H : ")
        scanner.Scan()
        diff := strings.ToUpper(strings.TrimSpace(scanner.Text()))
        if diff != "E" && diff != "H" {
            fmt.Println("Invalid response, try again.")
        } else {
            if diff == "H" {
                level = hard
            }
            break
        }
    }
    check(scanner.Err())
    setDiff(level)
    var ix [4]int
    fmt.Println("When entering moves, you can also enter Q to quit or S to start again.")
    moves := 0
outer:
    for {
        drawGrid()
        if hasWon() {
            fmt.Println("Congratulations, you have won the game in", moves, "moves!!")
            return
        }
        for {
            fmt.Println("Moves so far =", moves, "\n")
            fmt.Print("Enter move : ")
            scanner.Scan()
            move := strings.ToUpper(strings.TrimSpace(scanner.Text()))
            check(scanner.Err())
            switch move {
            case "D1", "D2", "D3", "D4":
                c := int(move[1] - 49)
                ix[0] = 0 + c
                ix[1] = 4 + c
                ix[2] = 8 + c
                ix[3] = 12 + c
                rotate(ix)
                moves++
                continue outer
            case "L1", "L2", "L3", "L4":
                c := int(move[1] - 49)
                ix[0] = 3 + 4*c
                ix[1] = 2 + 4*c
                ix[2] = 1 + 4*c
                ix[3] = 0 + 4*c
                rotate(ix)
                moves++
                continue outer
            case "U1", "U2", "U3", "U4":
                c := int(move[1] - 49)
                ix[0] = 12 + c
                ix[1] = 8 + c
                ix[2] = 4 + c
                ix[3] = 0 + c
                rotate(ix)
                moves++
                continue outer
            case "R1", "R2", "R3", "R4":
                c := int(move[1] - 49)
                ix[0] = 0 + 4*c
                ix[1] = 1 + 4*c
                ix[2] = 2 + 4*c
                ix[3] = 3 + 4*c
                rotate(ix)
                moves++
                continue outer
            case "Q":
                return
            case "S":
                initGrid()
                setDiff(level)
                moves = 0
                continue outer
            default:
                fmt.Println("Invalid move, try again.")
            }
        }
    }
}
```


{{out}}
Sample game:

```txt

Enter difficulty level easy or hard E/H : e
Target is 3 moves.
When entering moves, you can also enter Q to quit or S to start again.

     D1   D2   D3   D4
   ╔════╦════╦════╦════╗
R1 ║  1 ║  2 ║  3 ║  4 ║ L1
   ╠════╬════╬════╬════╣
R2 ║  7 ║  8 ║  5 ║  6 ║ L2
   ╠════╬════╬════╬════╣
R3 ║  9 ║ 10 ║ 11 ║ 12 ║ L3
   ╠════╬════╬════╬════╣
R4 ║ 16 ║ 13 ║ 14 ║ 15 ║ L4
   ╚════╩════╩════╩════╝
     U1   U2   U3   U4

Moves so far = 0 

Enter move : l4

     D1   D2   D3   D4
   ╔════╦════╦════╦════╗
R1 ║  1 ║  2 ║  3 ║  4 ║ L1
   ╠════╬════╬════╬════╣
R2 ║  7 ║  8 ║  5 ║  6 ║ L2
   ╠════╬════╬════╬════╣
R3 ║  9 ║ 10 ║ 11 ║ 12 ║ L3
   ╠════╬════╬════╬════╣
R4 ║ 13 ║ 14 ║ 15 ║ 16 ║ L4
   ╚════╩════╩════╩════╝
     U1   U2   U3   U4

Moves so far = 1 

Enter move : l2

     D1   D2   D3   D4
   ╔════╦════╦════╦════╗
R1 ║  1 ║  2 ║  3 ║  4 ║ L1
   ╠════╬════╬════╬════╣
R2 ║  8 ║  5 ║  6 ║  7 ║ L2
   ╠════╬════╬════╬════╣
R3 ║  9 ║ 10 ║ 11 ║ 12 ║ L3
   ╠════╬════╬════╬════╣
R4 ║ 13 ║ 14 ║ 15 ║ 16 ║ L4
   ╚════╩════╩════╩════╝
     U1   U2   U3   U4

Moves so far = 2 

Enter move : l2

     D1   D2   D3   D4
   ╔════╦════╦════╦════╗
R1 ║  1 ║  2 ║  3 ║  4 ║ L1
   ╠════╬════╬════╬════╣
R2 ║  5 ║  6 ║  7 ║  8 ║ L2
   ╠════╬════╬════╬════╣
R3 ║  9 ║ 10 ║ 11 ║ 12 ║ L3
   ╠════╬════╬════╬════╣
R4 ║ 13 ║ 14 ║ 15 ║ 16 ║ L4
   ╚════╩════╩════╩════╝
     U1   U2   U3   U4

Congratulations, you have won the game in 3 moves!!

```



## Javascript

Try it [http://paulo-jorente.de/webgames/repos/16Puzzle/ here].

You'll also need a html file:

```txt

<!DOCTYPE html>
<html lang="en">
<head>
 <meta charset="UTF-8">
 <meta name="viewport" content="width=device-width, initial-scale=1.0">
 <meta http-equiv="X-UA-Compatible" content="ie=edge">
 <link href="https://fonts.googleapis.com/css?family=Ubuntu+Mono&display=swap" rel="stylesheet">
 <link rel="stylesheet" type="text/css" media="screen" href="./css/main.css">
 <title>16 Puzzle</title>
</head>
<body>
 <div id="done">WELL DONE!</div>
 <div id="board"></div>
 <div id="moves"></div>
 <button id="shuffle">SHUFFLE</button>
 <script src="./src/index.js" type="module"></script>
</body>
</html>

```

And css file:

```txt

* {
 margin: 0;
 border: 0;
 text-align: center;
 font-family: 'Ubuntu Mono', monospace;
 user-select: none;
}
button {
 border-radius: 5px;
 width: 300px;
 height: 80px;
 font-size: 40px;
 margin-top: 60px;
}
#board {
 width: 410px;
 height: 410px;
 margin: 120px auto 30px auto;
}
#done {
 font-size: 140px;
 padding: 20px;
 color: #fff;
 background-color: rgba(0, 23, 56, .5);
 border: 1px solid rgb(0, 90, 220);
 width: 700px;
 position: absolute;
 top: 250px;
 left: calc(50% - 380px);
}
#moves {
 font-size: 40px;
 line-height: 80px;
 height: 80px;
 width: 300px;
 margin: auto;
 border: 1px solid #000;
 border-radius: 5px;
}
.btn,
.numbers,
.hide {
 float: left;
 width: 64px;
 height: 64px;
 line-height: 65px;
 font-size: 40px;
 border: 1px solid black;
 color: black;
 background-color: white;
 cursor: none;
 margin: 1px;
 transition: all .3s;
}
.btn:hover {
 background-color: rgba(71, 231, 71, 0.5);
 cursor: pointer;
}
.hide {
 border: 1px solid white;
 cursor: none;
}

```


```javascript

class Puzzle {
 constructor() {
  this.moves;
  this.started;

  this.board = document.getElementById("board");
  document.getElementById("shuffle").addEventListener("click", () => {
   this.shuffle();
  });
  this.reset();
 }

 reset() {
  while (this.board.hasChildNodes()) {
   this.board.removeChild(this.board.firstChild);
  }

  this.moves = 0;
  this.started = false;
  document.getElementById("moves").innerText = this.moves;
  document.getElementById("done").style.visibility = "hidden";

  let t = 1;
  for (let y = 0; y < 6; y++) {
   for (let x = 0; x < 6; x++) {
    const d = document.createElement("div");
    d.id = `${x}_${y}`;
    if (y === 0 || x === 0 || y === 5 || x === 5) {
     if (((y === 0 || y === 5) && (x === 0 || x === 5))) {
      d.className = "hide";
     } else {
      d.className = "btn";
      if (y === 0) {
       d.innerText = "🡇";
       d.func = () => {
        this.rollDownRight(x, true);
       };
      } else if (y === 5) {
       d.innerText = "🡅";
       d.func = () => {
        this.rollUpLeft(x, true);
       };
      }
      if (x === 0) {
       d.innerText = "🡆";
       d.func = () => {
        this.rollDownRight(y, false);
       };
      } else if (x === 5) {
       d.innerText = "🡄";
       d.func = () => {
        this.rollUpLeft(y, false);
       };
      }
      d.addEventListener("click", (ev) => {
       ev.srcElement.func();
      })
     }
    } else {
     d.className = "numbers";
     d.innerText = `${t}`;
     t++;
    }
    this.board.appendChild(d);
   }
  }
  document.getElementById("shuffle").innerText = "SHUFFLE";
 }

 shuffle() {
  if (this.started) {
   this.reset();
  } else {
   this.started = true;
   const e = Math.floor(Math.random() * 30) + 30;
   for (let z = 0; z < e; z++) {
    switch (Math.floor(Math.random() * 4)) {
     case 0:
      this.rollDownRight(Math.floor(Math.random() * 4) + 1, false);
      break;
     case 1:
      this.rollUpLeft(Math.floor(Math.random() * 4) + 1, true);
      break;
     case 2:
      this.rollUpLeft(Math.floor(Math.random() * 4) + 1, false);
      break;
     case 3:
      this.rollDownRight(Math.floor(Math.random() * 4) + 1, true);
      break;
    }
   }
   this.moves = 0;
   document.getElementById("moves").innerText = this.moves;
   document.getElementById("shuffle").innerText = "RESTART";
  }
 }

 getElements(l, col) {
  const z = Array.from(document.querySelectorAll(".numbers"));
  for (let e = 15; e > -1; e--) {
   if (z[e].id[(col ? 0 : 2)] != l) {
    z.splice(e, 1)
   }
  }
  return z;
 }

 rollDownRight(x, col) {
  if (!this.started) return;
  const z = this.getElements(x, col),
   a = z[3].innerText;
  for (let r = 3; r > 0; r--) {
   z[r].innerText = z[r - 1].innerText;
  }
  z[0].innerText = a;
  this.updateMoves();
  this.checkSolved();
 }

 rollUpLeft(x, col) {
  if (!this.started) return;
  const z = this.getElements(x, col),
   a = z[0].innerText;
  for (let r = 0; r < 3; r++) {
   z[r].innerText = z[r + 1].innerText;
  }
  z[3].innerText = a;
  this.updateMoves();
  this.checkSolved();
 }

 updateMoves() {
  this.moves++;
  document.getElementById("moves").innerText = this.moves;
 }

 checkSolved() {
  function check() {
   const z = document.querySelectorAll(".numbers");
   let u = 1;
   for (let r of z) {
    if (r.innerText != u) return false;
    u++;
   }
   return true;
  }
  if (this.started && check()) {
   document.getElementById("done").style.visibility = "visible";
  }
 }
}

new Puzzle();

```



## Julia


```julia
using Gtk, Random

function puzzle16app(bsize)
    aclock, clock = "\u27f2", "\u27f3"
    win = GtkWindow("16 Game", 300, 300) |> (GtkFrame() |> (box = GtkBox(:v)))
    toolbar = GtkToolbar()
    newgame = GtkToolButton("New Game")
    set_gtk_property!(newgame, :label, "New Game")
    set_gtk_property!(newgame, :is_important, true)
    push!(toolbar, newgame)
    grid = GtkGrid()
    map(w -> push!(box, w),[toolbar, grid])
    buttons = Array{Gtk.GtkButtonLeaf,2}(undef, bsize + 2, bsize + 2)
    for i in 1:bsize+2, j in 1:bsize+2
        grid[i,j] = buttons[i,j] = GtkButton()
        set_gtk_property!(buttons[i,j], :expand, true)
    end

    inorder = string.(reshape(1:bsize*bsize, bsize, bsize))
    puzzle = shuffle(inorder)
    rotatecol(puzzle, col, n) = puzzle[:, col] .= circshift(puzzle[:, col], n)
    rotaterow(puzzle, row, n) = puzzle[row, :] .= circshift(puzzle[row, :], n)
    iswon() = puzzle == inorder
    won = false

    function findrowcol(button)
        for i in 1:bsize+2, j in 1:bsize+2
            if buttons[i, j] == button
                return i, j
            end
        end
        return 0, 0
    end

    function playerclicked(button)
        if !won
        i, j = findrowcol(button)
            if i == 1
                rotatecol(puzzle, j - 1, 1)
            elseif i == bsize + 2
                rotatecol(puzzle, j - 1, -1)
            elseif j == 1
                rotaterow(puzzle, i - 1, 1)
            elseif j == bsize + 2
                rotaterow(puzzle, i - 1, -1)
            end
        end
        update!()
    end

    function setup!()
        for i in 1:bsize+2, j in 1:bsize+2
            if 1 < j < bsize + 2
                if i == 1
                    signal_connect(playerclicked, buttons[i, j], "clicked")
                elseif i == bsize + 2
                    signal_connect(playerclicked, buttons[i, j], "clicked")
                end
            elseif 1 < i < bsize + 2
                if j == 1
                    signal_connect(playerclicked, buttons[i, j], "clicked")
                elseif j == bsize + 2
                    signal_connect(playerclicked, buttons[i, j], "clicked")
                end
            end
        end
    end

    function update!()
        for i in 1:bsize+2, j in 1:bsize+2
            if 1 < j < bsize + 2
                if i == 1
                    set_gtk_property!(buttons[i, j], :label, clock)
                elseif i == bsize + 2
                    set_gtk_property!(buttons[i, j], :label, aclock)
                else
                    set_gtk_property!(buttons[i, j], :label, puzzle[i-1, j-1])
                end
            elseif 1 < i < bsize + 2
                if j == 1
                    set_gtk_property!(buttons[i, j], :label, clock)
                elseif j == bsize + 2
                    set_gtk_property!(buttons[i, j], :label, aclock)
                end
            end
        end
        if iswon()
            won = true
            info_dialog("Game over.\nScore: $score", win)
        end
        showall(win)
    end

    function initialize!(w)
        puzzle = shuffle(inorder)
        won = false
        update!()
    end
    
    setup!()
    condition = Condition()
    endit(w) = notify(condition)
    signal_connect(initialize!, newgame, :clicked)
    signal_connect(endit, win, :destroy)
    initialize!(win)
    showall(win)
    wait(condition)
end

puzzle16app(4)

```



## Phix

NB arrow keys not tested on linux, but "UDLR" should work...

```Phix
constant level = 5,
         ESC=27, UP=328, DOWN=336, LEFT=331, RIGHT=333

sequence board = tagset(16), solve = board
 
procedure print_board()
    printf(1,"    1  2  3  4\n")
    for r=1 to 4 do
        printf(1,"%d: %2d %2d %2d %2d\n",r&board[r*4-3..r*4])
    end for
    puts(1,"\n")
end procedure
 
procedure move(integer d,rc)
    -- d is 1..4 for up/down/left/right
    -- rc is 1..4 for row(d>=3)/column(d<=2)
    sequence idx = repeat(0,4),
             tiles = repeat(0,4)
    for i=1 to 4 do
        idx[i] = iff(d<=2?rc+(i-1)*4:(rc-1)*4+i)
        tiles[i] = board[idx[i]]
    end for
--  ?{d,rc,idx}
    idx = iff(mod(d,2)?idx[4]&idx[1..3]:idx[2..4]&idx[1])
    for i=1 to 4 do
        board[idx[i]] = tiles[i]
    end for
end procedure
 
for i=1 to level do move(rand(4),rand(4)) end for
while 1 do
    print_board()
    if board=solve then
        puts(1,"Solved!\n")
        exit
    end if
    puts(1,"Your move (escape|Up/Down|Left/Right & 1/2/3/4):")
    integer d, rc
    while true do
        while true do
            d = find(upper(wait_key()),{ESC,UP,DOWN,LEFT,RIGHT}&"UDLR")-1
            if d!=-1 then exit end if
        end while
        if d=0 then
            puts(1,"\n\nYou gave up!\n")
            exit
        end if
        if d>4 then d-=4 end if
        puts(1,"UDLR"[d])
        while true do
            rc = find(upper(wait_key()),ESC&"1234UDLR"&{UP,DOWN,LEFT,RIGHT})-1
            if rc>4 then
                if rc>8 then rc -= 4 end if
                d = rc-4
                puts(1,"\b \b"&"UDLR"[d])
            else
                if rc!=-1 then exit end if
            end if
        end while
        if rc=0 then
            puts(1,"\b \b")
        else
            printf(1,"%d\n\n",rc)
            move(d,rc)
            exit
        end if
    end while
    if d=0 then exit end if
end while
```

{{out}}
(a level 2 game)

```txt

    1  2  3  4
1:  1  2  7  4
2:  5  6 11  8
3:  9 10 16 12
4: 14 15  3 13

Your move (escape|Up/Down|Left/Right & 1/2/3/4):D3

    1  2  3  4
1:  1  2  3  4
2:  5  6  7  8
3:  9 10 11 12
4: 14 15 16 13

Your move (escape|Up/Down|Left/Right & 1/2/3/4):R4

    1  2  3  4
1:  1  2  3  4
2:  5  6  7  8
3:  9 10 11 12
4: 13 14 15 16

Solved!

```



## REXX

This REXX version allows the user to choose the grid size for the   '''16'''   game (puzzle),   as
well as the difficulty of the puzzle.

The user's responses may have optional whitespace in the answer, 
and the answer can be in any case (lower or uppercase). 

Not all errors are checked so as to keep the program simpler.

```rexx
/*REXX pgm implements the  16  game;  displays game grid, prompts for a move, game won? */
sep= copies("─",8);  pad=left('',1+length(sep) ) /*pad=9 blanks.   SEP is used for msgs.*/
parse arg N hard seed .                          /*obtain optional arguments from the CL*/
er= '***error***'                                /*literal used to indicate an error msg*/
if    N=='' |    N==","  then    N= 4            /*Not specified?  Then use the default.*/
if hard=='' | hard==","  then hard= 2            /* "      "         "   "   "     "    */
if \isInt(N)  then do;  say sep  er  "grid size isn't an integer: "   N;    exit 1;    end
if N<2 | N>9  then do;  say sep  er  "grid size is out of range: "    N;    exit 1;    end
if isInt(seed)  then call random , , seed        /*use repeatability seed for RANDOM BIF*/
say sep 'Playing a '      N*N       " game with a difficulty level of: "     hard
#=0
         do   r=1  for N                         /* [◄]  build a solution for testing.  */
           do c=1  for N;   #= #+1;    @.r.c= #  /*bump number (count), define a cell.  */
           end   /*c*/
         end     /*r*/
                                                 /* [↓]  HARD  is the puzzle difficulty.*/
     do hard;     row= random(1)                 /*scramble the grid  HARD   # of times.*/
     if row  then call move random(1,N)substr('LR', random(1, 2), 1)   /* ◄── move row. */
             else call move substr('abcdefghi',random(1,N), 1)substr("+-",random(1,2),1)
     end   /*hard*/                                                    /* [↓]  move col.*/
                                                 /*play 16─game until  solved  or  quit.*/
   do  until done()                              /*perform moves until puzzle is solved.*/
   call move                                     /*get user's move(s)  and  validate it.*/
   if errMsg\==''  then do;  say sep er errMsg".";  iterate; end   /*possible error msg?*/
   end   /*until*/

call show;     say sep  'Congratulations!   The'      N**2"─puzzle is solved."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
done: #=0; do r=1 to N; do c=1 to N; #=#+1; if @.r.c\==# then return 0; end; end; return 1
isInt: return datatype( arg(1), 'W')             /*return 1 if arg is a whole number.   */
ghost: do r=1  for n;   do c=1  for n;    !.r.c= @.r.c;  end  /*r*/;   end  /*c*/;  return
/*──────────────────────────────────────────────────────────────────────────────────────*/
move: arg x                                      /*obtain optional move from ARG or ask.*/
      ask1= sep 'Please enter a     row  number    followed by a   L   or   R,       or'
      ask2= sep '       enter a   column letter    followed by a   +   or   -'   @quit
      @quit= '      (or Quit):'
      if x==''  then do
                     if queued()==0  then do;   say;   call show;    say ask1;    say ask2
                                          end
                     pull x;  x= space(x, 0)     /*obtain a response;  elide whitespace.*/
                     end
      y= left(x, 1);          d= right(x, 1)     /*get a number or letter, and also a ± */
      num= isInt(d);        let= datatype(y,'U') /*get direction to shift, based on type*/
      if abbrev('QUIT', x, 1)  then do;  say;  say;  say sep  "quitting.";    exit;    end
               select
               when x == ''                    then errMsg= "nothing entered"
               when length(x)>2                then errMsg= "improper response:  "       x
               when num  &  (y <1   | y >N  )  then errMsg= "row isn't in range: "       y
               when num  &  (d\="L" & d\='R')  then errMsg= "row shift isn't L or R: "   d
               when let  &  (y <"A" | y >HI )  then errMsg= "col isn't in range: "       y
               when let  &  (d\="+" & d\='-')  then errMsg= "col shift isn't + or -: "   d
               otherwise                            errMsg=
               end   /*select*/                  /* [↑]  verify the human entered data. */
      call ghost;    yn= pos(y, 'ABCDEFGHI')     /*create a ghost grid for easy moving. */
      if isInt(y)  then if d=='R'  then  do c=1  for N;  cm= c-1;  if c==1  then cm= c+N-1
                                                         @.y.c= !.y.cm
                                         end
                                   else  do c=1  for N;  cp= c+1;  if c==N  then cp= 1
                                                         @.y.c= !.y.cp
                                         end
                   else if d=='-'  then  do r=1  for N;  rm= r-1;  if r==1  then rm= r+N-1
                                                         @.r.yn= !.rm.yn
                                         end
                                   else  do r=1  for N;  rp= r+1;  if r==N  then rp= 1
                                                         @.r.yn= !.rp.yn
                                         end
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: top= '╔'copies( copies("═", 2)'╦', N);           top= left( top, length(top) - 1)"╗"
      bar= '╠'copies( copies("═", 2)'╬', N);           bar= left( bar, length(bar) - 1)"╣"
      bot= '╚'copies( copies("═", 2)'╩', N);           bot= left( bot, length(bot) - 1)"╝"
      ind= left('',  3 + length(N) )                              /*compute indentation.*/
      col= ind  ind  ind' '   subword("a- b- c- d- e- f- g- h- i-",  1,  n)
      HI= substr('abcdefghi', N, 1);    upper HI
      say col  ind  ind  ind  '-  means shift a column down';            say pad  ind  top
              do    r=1  for N;   z= r'R'    "  ║"                 /*build NxN game grid*/
                 do c=1  for N;   z= z || right(@.r.c, 2)'║'       /*build  row by row. */
                 end   /*c*/
              z= z   ' '   r"L"                                    /*add right-side info*/
              if r==1  then z= z  pad'L  means shift a row left'   /* "   1st help info.*/
              if r==2  then z= z  pad'R  means shift a row right'  /* "   2nd   "    "  */
              say pad z;             if r\==N  then say pad  ind  bar
              end     /*r*/
      say pad  ind  bot;             say;
      say translate(col, '+', "-")   ind  ind  ind  "+  means shift a column up";     say
      return
```

{{out|output|text=  when using the default inputs:}}

```txt

──────── Playing a  16  game with a difficulty level of:  2

                a- b- c- d-                -  means shift a column down
               ╔══╦══╦══╦══╗
          1R   ║ 1║ 2║15║ 4║   1L          L  means shift a row left
               ╠══╬══╬══╬══╣
          2R   ║ 5║ 6║ 3║ 8║   2L          R  means shift a row right
               ╠══╬══╬══╬══╣
          3R   ║12║ 9║ 7║11║   3L
               ╠══╬══╬══╬══╣
          4R   ║13║14║10║16║   4L
               ╚══╩══╩══╩══╝

                a+ b+ c+ d+                +  means shift a column up

──────── Please enter a     row  number    followed by a   L   or   R,       or
────────        enter a   column letter    followed by a   +   or   -       (or Quit):
c +                             ◄■■■■■■■■ user input                                                                                

                a- b- c- d-                -  means shift a column down
               ╔══╦══╦══╦══╗
          1R   ║ 1║ 2║ 3║ 4║   1L          L  means shift a row left
               ╠══╬══╬══╬══╣
          2R   ║ 5║ 6║ 7║ 8║   2L          R  means shift a row right
               ╠══╬══╬══╬══╣
          3R   ║12║ 9║10║11║   3L
               ╠══╬══╬══╬══╣
          4R   ║13║14║15║16║   4L
               ╚══╩══╩══╩══╝

                a+ b+ c+ d+                +  means shift a column up

──────── Please enter a     row  number    followed by a   L   or   R,       or
────────        enter a   column letter    followed by a   +   or   -       (or Quit):
3L                              ◄■■■■■■■■ user input
                a- b- c- d-                -  means shift a column down
               ╔══╦══╦══╦══╗
          1R   ║ 1║ 2║ 3║ 4║   1L          L  means shift a row left
               ╠══╬══╬══╬══╣
          2R   ║ 5║ 6║ 7║ 8║   2L          R  means shift a row right
               ╠══╬══╬══╬══╣
          3R   ║ 9║10║11║12║   3L
               ╠══╬══╬══╬══╣
          4R   ║13║14║15║16║   4L
               ╚══╩══╩══╩══╝

                a+ b+ c+ d+                +  means shift a column up

──────── Congratulations!   The 16─puzzle is solved.

```



## Ring


```ring

# Project : Sixteen Puzzle Game

load "guilib.ring"
load "stdlib.ring"

app1 = new qapp {

        t1 = 0
        temp = ""
        table = [][]
        movesnr = 0
        button = list(16) 
        begintiles = list(16)
        pReturn = list(4)
        CounterMan = 0
        saveflag = 0

        stylefusionblack()  
 
        win1 = new qwidget() {
                   move(0,0)
                   resize(360, 600)
                   setwindowtitle("Calmosoft Sixteen Puzzle Game")

                   for n = 1 to 16
                         col = n%4
                         if col = 0 col = 4 ok
                         row = ceil(n/4)
                         button[n] = new qpushbutton(win1)
                         {
                                            setgeometry(60+col*40,60+row*40,40,40)                                            
                                            setstylesheet("color:white")    
                                            setstylesheet("background-color:blue")                                        
                                            settext(string(n))                                                                                        
                          } 
                   next

                  buttonup1 = new qpushbutton(win1)
                  {
                                      setgeometry(100, 60, 40, 40)
                                      settext("up")
                                      setclickevent("up1()")   
                  }

                  buttonup2 = new qpushbutton(win1)
                  {
                                      setgeometry(140, 60, 40, 40)
                                      settext("up")
                                      setclickevent("up2()")   
                  }

                 buttonup3 = new qpushbutton(win1)
                  {
                                      setgeometry(180, 60, 40, 40)
                                      settext("up")
                                      setclickevent("up3()")   
                  }

                 buttonup4 = new qpushbutton(win1)
                  {
                                      setgeometry(220, 60, 40, 40)
                                      settext("up")
                                      setclickevent("up4()")   
                  }

                  buttondown1 = new qpushbutton(win1)
                  {
                                          setgeometry(100, 260, 40, 40)
                                          settext("down")
                                          setclickevent("down1()")   
                  }

                  buttondown2 = new qpushbutton(win1)
                  {
                                          setgeometry(140, 260, 40, 40)
                                          settext("down")
                                          setclickevent("down2()")   
                  }

                  buttondown3 = new qpushbutton(win1)
                  {
                                          setgeometry(180, 260, 40, 40)
                                          settext("down")
                                          setclickevent("down3()")   
                  }

                 buttondown4 = new qpushbutton(win1)
                  {
                                          setgeometry(220, 260, 40, 40)
                                          settext("down")
                                          setclickevent("down4()")   
                  }

                  buttonleft1 = new qpushbutton(win1)
                  {
                                          setgeometry(60, 100, 40, 40)
                                          settext("<<<")
                                          setclickevent("left1()")   
                  }

                  buttonleft2 = new qpushbutton(win1)
                  {
                                          setgeometry(60, 140, 40, 40)
                                          settext("<<<")
                                          setclickevent("left2()")   
                  }

                  buttonleft3 = new qpushbutton(win1)
                  {
                                          setgeometry(60, 180, 40, 40)
                                          settext("<<<")
                                          setclickevent("left3()")   
                  }

                  buttonleft4 = new qpushbutton(win1)
                  {
                                          setgeometry(60, 220, 40, 40)
                                          settext("<<<")
                                          setclickevent("left4()")   
                  }

                  buttonright1 = new qpushbutton(win1)
                  {
                                          setgeometry(260, 100, 40, 40)
                                          settext(">>>")
                                          setclickevent("right1()")   
                  }

                  buttonright2 = new qpushbutton(win1)
                  {
                                          setgeometry(260, 140, 40, 40)
                                          settext(">>>")
                                          setclickevent("right2()")   
                  }

                  buttonright3 = new qpushbutton(win1)
                  {
                                          setgeometry(260, 180, 40, 40)
                                          settext(">>>")
                                          setclickevent("right3()")   
                  }

                  buttonright4 = new qpushbutton(win1)
                  {
                                          setgeometry(260, 220, 40, 40)
                                          settext(">>>")
                                          setclickevent("right4()")   
                  }

                  buttonscramble = new qpushbutton(win1)
                  {
                                             setgeometry(100, 300, 160, 40)
                                             settext("Scarmble")
                                             setclickevent("scramble()")   
                  }

                  buttonreset = new qpushbutton(win1)
                  {
                                        setgeometry(100, 340, 160, 40)
                                        settext("Reset")
                                        setclickevent("reset()")   
                  }

                  buttonsave = new qpushbutton(win1)
                  {
                                   setgeometry(100, 380, 160, 40)
                                   settext("Save Game")
                                   setclickevent("psaveEmpty()")
                  }

                  buttonplay = new qpushbutton(win1)   
                  {
                                 setgeometry(100,420,160,40)  
                                 settext("Replay Game")  
                                 setclickevent("pPlay()")
                  }

                  buttonnr = new qpushbutton(win1)
                  {
                                   setgeometry(100, 460, 160, 40)
                                   settext("Moves : ")
                  }

                  timebtn = new qpushbutton(win1)   
                  {
                                 setgeometry(100,500,160,40)  
                                 settext("Elapsed Time : ")  
                  }
                  t1 = clock()

                  for i = 1 to 16
                       begintiles[i] = string(i)
                  next

                  TimerMan = new qtimer(win1)
                  {
                                    setinterval(0.5)
                                    settimeoutevent("pTime()")
                                    stop()
                  }
                  show()
        }
        exec()
}

func scramble
       reset()
       empty = 16
       movesnr = 0
       buttonnr.settext("Moves : " + movesnr)

       for n= 1 to 1000  
            nr=random(15)+1
            up = (empty = (nr - 4))
            down = (empty = (nr + 4))
            left = ((empty = (nr - 1)) and ((nr % 4) != 1))
            right = ((empty = (nr + 1)) and ((nr % 4) != 0))
            move = up or down or left  or right
            if move = 1 
               temp1 = button[nr].text()
               temp2 = button[empty].text()
               button[empty].settext(temp1)
               button[nr].settext(temp2)
               empty = nr
            ok
       next
       timebtn.settext("Elapsed Time : ")
       t1 = clock()
       table = []
       saveflag = 0
       for n= 1 to 16
             if isstring(button[n].text())
                begintiles[n] = button[n].text()
             else
                begintiles[n] = string(button[n].text())
             ok 
       next

func reset
        movesnr = 0
        buttonnr.settext("Moves : " + movesnr)
        for i = 1 to 16
             button[i] {settext(string(i))}
             button[i].setstylesheet("background-color:blue")
             begintiles[i] = string(i)
        next
        timebtn.settext("Elapsed Time : ")
        t1 = clock()
        table = []
        saveflag = 0
        return

func pClock
        t2 = (clock() - t1)/1000
        timebtn.settext("Elapsed Time : " + t2 + " s")

func psaveEmpty
        timebtn.settext("Elapsed Time : ")
        t1 = clock()
        return

func psave
        if saveflag = 1
           for n = 1 to 4
                add(table, [pReturn[n], button[pReturn[n]].text()])
           next
        ok

func pPlay
        if saveflag = 1
           for n=1 to 16
                 button[n]{settext(begintiles[n])}
                 button[n].setstylesheet("background-color:blue")
           next
           timebtn.settext("Elapsed Time : ")
           movesnr = 0
           buttonnr.settext("Moves : " + movesnr)
           t1 = clock()
           CounterMan = 0
           TimerMan.start()
        ok

func pTime()
        if saveflag = 1
           CounterMan = CounterMan + 1
           if CounterMan > 1
              temp.setstylesheet("background-color:blue")
           ok
           pPlaySleep()
           sleep(1) 
           if CounterMan = len(table)
              TimerMan.stop()
           ok
        ok

func pPlaySleep
        pClock()
        button[table[CounterMan][1]].setstylesheet("background-color:orange") 
        temp =  button[table[CounterMan][1]]
        button[table[CounterMan][1]].settext(table[CounterMan][2])
        movesnr = movesnr + 1
        buttonnr.settext("Moves : " + movesnr)
        return

func up1
        temp = button[1].text()
        button[1].settext(button[5].text())
        button[5].settext(button[9].text())
        button[9].settext(button[13].text())
        button[13].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [1,5,9,13]
        saveflag = 1
        psave()
        return

func up2
        temp = button[2].text()
        button[2].settext(button[6].text())
        button[6].settext(button[10].text())
        button[10].settext(button[14].text())
        button[14].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [2,6,10,14]
        saveflag = 1
        psave()
        return

func up3
        temp = button[3].text()
        button[3].settext(button[7].text())
        button[7].settext(button[11].text())
        button[11].settext(button[15].text())
        button[15].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [3,7,11,15]
        saveflag = 1
        psave()
        return

func up4
        temp = button[4].text()
        button[4].settext(button[8].text())
        button[8].settext(button[12].text())
        button[12].settext(button[16].text())
        button[16].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [4,8,12,16]
        saveflag = 1
        psave()
        return

func down1
        temp = button[13].text()
        button[13].settext(button[9].text())
        button[9].settext(button[5].text())
        button[5].settext(button[1].text())
        button[1].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [13,9,5,1]
        saveflag = 1
        psave()
        return

func down2
        temp = button[14].text()
        button[14].settext(button[10].text())
        button[10].settext(button[6].text())
        button[6].settext(button[2].text())
        button[2].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [14,10,6,2]
        saveflag = 1
        psave()
        return

func down3
        temp = button[15].text()
        button[15].settext(button[11].text())
        button[11].settext(button[7].text())
        button[7].settext(button[3].text())
        button[3].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [15,11,7,3]
        saveflag = 1
        psave()
        return

func down4
        temp = button[16].text()
        button[16].settext(button[12].text())
        button[12].settext(button[8].text())
        button[8].settext(button[4].text())
        button[4].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [16,12,8,4]
        saveflag = 1
        psave()
        return

func left1
        temp = button[1].text()
        button[1].settext(button[2].text())
        button[2].settext(button[3].text())
        button[3].settext(button[4].text())
        button[4].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [1,2,3,4]
        saveflag = 1
        psave()
        return

func left2
        temp = button[5].text()
        button[5].settext(button[6].text())
        button[6].settext(button[7].text())
        button[7].settext(button[8].text())
        button[8].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [5,6,7,8]
        saveflag = 1
        psave()
        return

func left3
        temp = button[9].text()
        button[9].settext(button[10].text())
        button[10].settext(button[11].text())
        button[11].settext(button[12].text())
        button[12].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [9,10,11,12]
        saveflag = 1
        psave()
        return

func left4
        temp = button[13].text()
        button[13].settext(button[14].text())
        button[14].settext(button[15].text())
        button[15].settext(button[16].text())
        button[16].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [13,14,15,16]
        saveflag = 1
        psave()
        return

func right1
        temp = button[4].text()
        button[4].settext(button[3].text())
        button[3].settext(button[2].text())
        button[2].settext(button[1].text())
        button[1].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [4,3,2,1]
        saveflag = 1
        psave()
        return

func right2
        temp = button[8].text()
        button[8].settext(button[7].text())
        button[7].settext(button[6].text())
        button[6].settext(button[5].text())
        button[5].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [8,7,6,5]
        saveflag = 1
        psave()
        return

func right3
        temp = button[12].text()
        button[12].settext(button[11].text())
        button[11].settext(button[10].text())
        button[10].settext(button[9].text())
        button[9].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [12,11,10,9]
        saveflag = 1
        psave()
        return

func right4
        temp = button[16].text()
        button[16].settext(button[15].text())
        button[15].settext(button[14].text())
        button[14].settext(button[13].text())
        button[13].settext(temp)
        movesnr =movesnr + 1
        buttonnr.settext("Moves : " + string(movesnr))
        pClock()
        pReturn = [16,15,14,13]
        saveflag = 1
        psave()
        return

```

Output image:

[https://www.dropbox.com/s/dy1zmw6xpo2o55v/CalmoSoftSixteenPuzzleGame.jpg?dl=0 16 Puzzle Game]
