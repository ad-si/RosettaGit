+++
title = "Hexapawn"
description = ""
date = 2019-04-28T10:12:39Z
aliases = []
[extra]
id = 19227
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "javascript",
  "julia",
  "pascal",
  "phix",
]
+++

{{draft task|Games}}Implement Martin Gardner’s game [http://en.wikipedia.org/wiki/Hexapawn Hexapawn] so that one can play against the computer.
Hexapawn is played on a 3 x 3 square board. The near rank contains your three pawns, and the far rank contains those of the computer.

It should “memorize” good moves and discard bad ones, after enough rounds, it will have learned from its mistakes and will become unbeatable.

[http://cs.williams.edu/~freund/cs136-073/GardnerHexapawn.pdf Description].

Extras:

* Make the board size variable. The player should decide it
* It would be cool to have a trainer, so you don’t need to keep playing until the computer learns it


## See also

* [https://www.youtube.com/watch?v=c7oc3EemqQk| ATARI ST Hexapawn 19xxErictronics (youtube)]



## Go

This implements '''HER''', '''H'''exapawn '''E'''ducable '''R'''obot, as specified in the paper rather than more efficiently
(e.g. no attempt is made to see if a possible move can immediately win).
However, unlike the paper this does ''not'' handle mirror games as the same.
I.e. this implementation will need to learn how to respond to white's opening "3 6" move independently from the mirror white opening move of "1 4".

```go
package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"math/rand"
	"os"
	"time"
)

// In the following `her*` is H.E.R. or Hexapawn Educable Robot

const (
	Rows = 3
	Cols = 3
)

var vlog *log.Logger

func main() {
	verbose := flag.Bool("v", false, "verbose")
	flag.Parse()
	if flag.NArg() != 0 {
		flag.Usage()
		os.Exit(2)
	}
	logOutput := ioutil.Discard
	if *verbose {
		logOutput = os.Stderr
	}
	vlog = log.New(logOutput, "hexapawn: ", 0)

	rand.Seed(time.Now().UnixNano())
	wins := make(map[spot]int, 2)
	for {
		h := New()
		var s herGameState
		for c := false; h[stateIdx] == empty; c = !c {
			if c {
				h = s.Move(h)
			} else {
				h = h.HumanMove()
			}
		}
		fmt.Printf("Board:\n%v is a win for %v\n", h, h[stateIdx])
		s.Result(h[stateIdx])
		wins[h[stateIdx]]++
		fmt.Printf("Wins: Black=%d, White=%d\n", wins[black], wins[white])
		fmt.Println()
	}
}

func (h Hexapawn) HumanMove() Hexapawn {
	fmt.Print("Board:\n", h, "\n")
	var from, to int
	for {
		fmt.Print("Your move: ")
		_, err := fmt.Scanln(&from, &to)
		if err != nil {
			fmt.Println(err)
			if err == io.EOF {
				os.Exit(0) // ick, exiting from here
			}
			continue
		}
		if err := h.doMove(white, from-1, to-1); err != nil {
			fmt.Println(err)
			continue
		}
		return h
	}
}

var herNextMove = make(map[Hexapawn][]move)

type herGameState struct {
	// Last "unknown" move was herNextMove[h][i]
	h Hexapawn
	i int
}

func (s *herGameState) Move(h Hexapawn) Hexapawn {
	known := false
	moves := herNextMove[h]
	if moves == nil { // Lazy init
		moves = possibleMoves(black, h)
		herNextMove[h] = moves
	} else if len(moves) == 0 {
		// From here all possibilities can lose
		vlog.Println("no good moves left to black, picking a random looser")
		known = true
		moves = possibleMoves(black, h)
	}
	vlog.Println("considering", moves)
	i := rand.Intn(len(moves))
	if !known {
		s.h = h
		s.i = i
	}
	fmt.Println("Computer moves", moves[i])
	if err := h.doMove(black, moves[i].from, moves[i].to); err != nil {
		panic(err)
	}
	return h
}

func (s herGameState) Result(winner spot) {
	if winner == black {
		return // Do nothing
	}
	// Throw out the last "unknown" move H.E.R. made
	moves := herNextMove[s.h]
	vlog.Printf("Training:\n%v will no longer do %v\n", s.h, moves[s.i])
	herNextMove[s.h] = append(moves[:s.i], moves[s.i+1:]...)
	vlog.Println("will instead do one of:", herNextMove[s.h])
}

type move struct{ from, to int }

func (m move) String() string { return fmt.Sprintf("%d→%d", m.from+1, m.to+1) }

var cachedMoves = []map[Hexapawn][]move{
	black: make(map[Hexapawn][]move),
	white: make(map[Hexapawn][]move),
}

func possibleMoves(s spot, h Hexapawn) []move {
	m := cachedMoves[s][h]
	if m != nil {
		return m
	}
	//vlog.Printf("calculating possible moves for %v\n%v\n", s, h)
	// These are cached so no effort at optimization is made
	// (e.g. skipping from==to or continuing the outer loop when h[from]!=s)
	m = make([]move, 0)
	for from := 0; from < Rows*Cols; from++ {
		for to := 0; to < Rows*Cols; to++ {
			if err := h.checkMove(s, from, to); err == nil {
				m = append(m, move{from, to})
			}
		}
	}
	cachedMoves[s][h] = m
	vlog.Printf("caclulated possible moves for %v\n%v as %v\n", s, h, m)
	return m
}

func (h *Hexapawn) doMove(p spot, from, to int) error {
	if err := h.checkMove(p, from, to); err != nil {
		return err
	}
	h[from] = empty
	h[to] = p
	if (p == white && to/Rows == Rows-1) || (p == black && to/Rows == 0) {
		h[stateIdx] = p
	} else if len(possibleMoves(p.Other(), *h)) == 0 {
		h[stateIdx] = p
	}
	return nil
}

func (h *Hexapawn) checkMove(p spot, from, to int) error {
	if h[from] != p {
		return fmt.Errorf("No %v located at spot %v", p, from+1)
	}
	if h[to] == p {
		return fmt.Errorf("%v already occupies spot %v", p, to+1)
	}
	Δr := from/Rows - to/Rows
	if (p == white && Δr != -1) || (p == black && Δr != 1) {
		return errors.New("must move forward one row")
	}
	Δc := from%Rows - to%Rows
	capture := h[to] != empty
	if (capture || Δc != 0) && (!capture || (Δc != 1 && Δc != -1)) {
		return errors.New("ilegal move")
	}
	return nil
}

type Hexapawn [Rows*Cols + 1]spot

func New() Hexapawn {
	// TODO for Rows,Cols != 3,3
	return Hexapawn{
		white, white, white,
		empty, empty, empty,
		black, black, black,
	}
}

func idx(r, c int) int { return r*Cols + c }

// The game winner (or empty) is stored at this index
const stateIdx = Rows * Cols

func (h Hexapawn) String() string {
	var b bytes.Buffer
	for r := Rows - 1; r >= 0; r-- {
		for c := 0; c < Cols; c++ {
			b.WriteByte(h[idx(r, c)].Byte())
		}
		b.WriteByte('\n')
	}
	// b.String() contains an extra newline
	return string(b.Next(Rows*(Cols+1) - 1))
}

type spot uint8

const (
	empty spot = iota
	black
	white
)

func (s spot) String() string {
	switch s {
	case black:
		return "Black"
	case white:
		return "White"
	}
	panic(s)
}

func (s spot) Byte() byte {
	switch s {
	case empty:
		return '.'
	case black:
		return 'B'
	case white:
		return 'W'
	}
	panic(s)
}

func (s spot) Other() spot {
	if s == black {
		return white
	}
	return black
}
```

```txt

Board:
BBB
...
WWW
Your move: 2 5
Computer moves 9→5
Board:
BB.
.B.
W.W
Your move: 3 5
Computer moves 7→5
Board:
.B.
.B.
W..
Your move: 1 5
Board:
.B.
.W.
... is a win for White
Wins: Black=0, White=1
[…]

```

Since H.E.R. won't make the same mistake twice and quickly becomes unbeatable the "game" becomes how many total wins can you achieve in a single session.
Note that you can get verbose output about possible moves, considered moves, and training by running with the <tt>-v</tt> command line flag.


## JavaScript

You can try it [http://paulo-jorente.de/tests/hexapawn/ here].

```javascript

var board, playBtn, turn, memory = [], lastMove = {brd: "", mvi: 0},
    clicks = {first: null, second: null}, win = {c: 0, p: 0}, score;

function getPossibles() {
    var pos = [], tp = turn == 0 ? "W" : "B", gp = turn == 0 ? "B" : "W";
	
    for( var j = 0; j < 3; j++ ) {
        var jj = j + (turn == 0 ? -1 : 1);
        if( jj < 3 && jj > -1 ) {
            for( var i = 0; i < 3; i++ ) {
                if( board[i][j] == tp ) {
                    for( var k = -1; k < 2; k++ ) {
                        if(i + k > -1 && i + k < 3 &&
                            ((board[i + k][jj] == " " && i + k == i) || (board[i + k][jj] == gp && i + k != i))) {
                            pos.push( {f: i + j * 3, t: i + k + jj * 3});
                        }
                    }
                }
            }
        }
    }
    return pos;
}
function computerMoves() {
    var brd = getBoard(), mvs, needSave = false;

	for( var i = 0; i < memory.length; i++ ) {
        if( memory[i].board == brd ) {
            mvs = memory[i].moves;
            break;
        }
    }
	
    if( !mvs ) {
        mvs = getPossibles();
        needSave = true;    
    }
    if( mvs.length == 0 ) return 0;

	var idx = Math.floor( Math.random() * mvs.length );
	lastMove.brd = brd;
	lastMove.mvi = idx;
	var i = mvs[idx].f % 3, j = Math.floor( mvs[idx].f / 3 ),
		ii = mvs[idx].t % 3, jj = Math.floor( mvs[idx].t / 3 );
	board[i][j] = " "; board[ii][jj] = "B";

    if( needSave ) {
        memory.push( {board: brd, moves: mvs} );
    }
	updateBtns();
    return -1;
}
function getBoard() {
    var str = "";
    for( var j = 0; j < 3; j++ ) {
        for( var i = 0; i < 3; i++ ) {
            str += board[i][j];
        }
    }
    return str;
}
function updateScore() {
	score.innerHTML = "Player: " + win.p + " Computer: " + win.c;
}
function finish( r ) {
	var str = "The Computer wins!";
	if( r == 0 ) {
		str = "You win!";
		win.p++;
		for( var i = 0; i < memory.length; i++ ) {
			if( memory[i].board == lastMove.brd ) {
				memory[i].moves.splice( lastMove.mvi, 1 );
				break;
			}
		}
	} else {
		win.c++;
	}
	playBtn.innerHTML = str + "<br />Click to play.";
	playBtn.className = "button long"
	updateScore();
}
function checkFinished() {
    if( getPossibles().length < 1 ) return turn == 0 ? 1 : 0;
    
    for( var i = 0; i < 3; i++ ) {
        if( board[i][0] == "W" ) return 0;
        if( board[i][2] == "B" ) return 1;
    }
	
    var w = 0, b = 0;
    for( var j = 0; j < 3; j++ ) {
        for( var i = 0; i < 3; i++ ) {
            if( board[i][j] == "W" ) w++;
            if( board[i][j] == "B" ) b++;
        }
    }
    if( w == 0 ) return 1;
    if( b == 0 ) return 0;
    return -1;
}
function nextPlayer() {
	var r;
	updateBtns();
	turn = turn == 0 ? 1 : 0;
	r = checkFinished();
	
	if( r > -1 ) {
		finish( r );
	} else {
		if( turn == 1 ) {
			r = computerMoves();
			if( r < 0 ) nextPlayer();
			else finish( r );
		}
	}
}
function search( o, arr ) {
	for( var i = 0; i < arr.length; i++ ) {
		if( o.f == arr[i].f && o.t == arr[i].t ) return i;
	}
	return -1;
}
function btnHandle( e ) {
    if( turn > 0 ) return;
    if( clicks.first == null && e.target.firstChild.data == "W" ) {
        clicks.first = e.target;
        clicks.first.className += " marked"
    } else if( clicks.first != null && e.target.firstChild.data == "W" ) {
		clicks.first.className = clicks.first.className.split(" ")[0];
        clicks.first = clicks.second = null;
	} else if( clicks.first != null && ( e.target.firstChild.data == " " ||
                                         e.target.firstChild.data == "B" ) ) {
        clicks.second = e.target;
        var moves = getPossibles( turn ),
            i = clicks.first.i, ii = clicks.second.i,
            j = clicks.first.j, jj = clicks.second.j,
            obj = {f: i + j * 3, t: ii + jj * 3};
        if( search( obj, moves ) > -1 ) {
            board[i][j] = " "; board[ii][jj] = "W";
			clicks.first.className = clicks.first.className.split(" ")[0];
			clicks.first = clicks.second = null;
			nextPlayer();
		}
    }
}
function updateBtns() {
    var b, v;
    for( var j = 0; j < 3; j++ ) {
        for( var i = 0; i < 3; i++ ) {
            b = document.getElementById( "btn" + ( i + j * 3 ) );
            b.innerHTML = ( board[i][j] );
        }
    }
}
function restart() {
    turn = 0;
    createBoard();
    updateBtns();
	playBtn.className += " hide";
}
function createBoard() {
    board = new Array( 3 );
    for( var i = 0; i < 3; i++ ) {
        board[i] = new Array( 3 );
    }
    for( var j = 0; j < 3; j++ ) {
        for( var i = 0; i < 3; i++ ) {
            board[i][j] = j == 0 ? "B" : j == 2 ? "W" : " ";
        }
    }
}
function createBtns() {
    var b, d = document.createElement( "div" ), v = false;
    d.className += "board";
    document.body.appendChild( d );
    for( var j = 0; j < 3; j++ ) {
        for( var i = 0; i < 3; i++ ) {
            b = document.createElement( "button" );
            b.id = "btn" + ( i + j * 3 );
            b.i = i; b.j = j;
            b.addEventListener( "click", btnHandle, false );
            b.appendChild( document.createTextNode( "" ) );
            d.appendChild( b );
            if( v ) b.className = "button"
            else  b.className = "empty";
            v = !v;
        }
    }
    playBtn = document.createElement( "button" );
    playBtn.className = "button long hide";
	playBtn.addEventListener( "click", restart, false );
	score = document.createElement( "p" );
    score.className = "txt";
	d.appendChild( score );
    d.appendChild( playBtn );
    updateScore();
}
function init() {
    createBtns();
    restart();
}

```

HTML (testing)

```txt
<!DOCTYPE html>
<html><head><meta charset="UTF-8">
<title>Hexapawn</title>
<script src="hexa.js"></script>
<style>
    html,body{padding:0; margin:0;padding-top:6vh;background:#222;color:#111}
    .txt{color:#fff;text-align:center;font-size:6vh}
    .board{padding:0;margin:auto;width:33vh;height:33vh}
    .long, .button, .empty{border:1px solid #000;font-size:9vh;margin:0;padding:0;height:11vh;width:11vh;line-height:5vh;
	vertical-align:middle;background:#fff;text-align:center;border-radius:3px;cursor:pointer;float:left}
    .empty{background:#ccc}
	.hide{display:none}
    .marked{border:#f60 3px solid}
	.long{width:100%;font-size:3vh}
</style>
</head><body onLoad="init()"></body></html>

```




## Julia

Graphical versions, using Gtk. Unicode has chess symbols! See https://www.compart.com/en/unicode/block/U+2600.

### Learning Version


```julia
using Gtk, Base

const whitepawn = UInt8('w')
const blackpawn = UInt8('b')
const space = UInt8(' ')
const unipawns = Dict(space => " ", whitepawn => "\u2659", blackpawn => "\u265f")
ispawn(c) = (c == whitepawn || c == blackpawn)
oppositepawn(c1, c2) = ispawn(c1) && ispawn(c2) && c1 != c2

mutable struct HState
    board::Matrix{UInt8}
    whitetomove::Bool
    HState(arr, iswhite) = new(reshape(UInt8.(arr), 3, 3), iswhite)
end
string(h::HState) = join([Char(c) for c in h.board], "") * (h.whitetomove ? "|t" : "|f")

const avoided = Vector{String}()

function legalmoves(board, px, py)
    moves = Vector{Pair{Int, Int}}()
    c = board[py, px]
    newrow = px + (c == whitepawn ? +1 : -1)
    if ispawn(c) && 0 < newrow < 4
        if py > 1 && oppositepawn(c, board[py - 1, newrow])
            push!(moves, Pair(newrow, py - 1))
        end
        if board[py, newrow] == UInt8(' ')
            push!(moves, Pair(newrow, py))
        end
        if py < 3 && oppositepawn(c, board[py + 1, newrow])
            push!(moves, Pair(newrow, py + 1))
        end
    end
    moves
end

islegalmove(board, px, py, i, j) = Pair(i, j) in legalmoves(board, px, py)

function allavailablemoves(board, forwhite)
    allret = Vector{Pair{Vector{Int}, Vector{Pair{Int, Int}}}}()
    for i in 1:3, j in 1:3
        if (board[j, i] == whitepawn && forwhite) || (board[j, i] == blackpawn && !forwhite)
            legmov = legalmoves(board, i, j)
            if !isempty(legmov)
                push!(allret, [i, j] => legmov)
            end
        end
    end
    allret
end

function checkforwin(hstate)
    if any(x -> hstate.board[x] == whitepawn, 7:9)
        return whitepawn # white win
    elseif any(x -> hstate.board[x] == blackpawn, 1:3)
        return blackpawn # black win
    else
        if length(allavailablemoves(hstate.board, hstate.whitetomove)) == 0
            return hstate.whitetomove ? blackpawn : whitepawn
        end
    end
    UInt8(' ') # hstate is not a winning position
end

function hexapawnapp()
    win = GtkWindow("Hexapawn Game", 425, 425) |> (GtkFrame() |> (box = GtkBox(:v)))
    toolbar = GtkToolbar()
    newWgame = GtkToolButton("New Game, Play as White")
    set_gtk_property!(newWgame, :label, "New Game, Play as White")
    set_gtk_property!(newWgame, :is_important, true)
    newBgame = GtkToolButton("New Game, Play as Black")
    set_gtk_property!(newBgame, :label, "New Game, Play as Black")
    set_gtk_property!(newBgame, :is_important, true)
    map(w->push!(toolbar,w),[newWgame, newBgame])
    scrwin = GtkScrolledWindow()
    grid = GtkGrid()
    map(w -> push!(box, w),[toolbar, scrwin])
    push!(scrwin, grid)
    buttons = Array{Gtk.GtkButtonLeaf, 2}(undef, 3, 3)
    stylist = GtkStyleProvider(Gtk.CssProviderLeaf(data="button {font-size:64px;}"))
    for i in 1:3, j in 1:3
        grid[i, 4-j] = buttons[i, j] = GtkButton()
        set_gtk_property!(buttons[i, j], :expand, true)
        push!(Gtk.GAccessor.style_context(buttons[i, j]), stylist, 600)
    end

    state = HState(b"www   bbb", true)
    won = ""
    pwhite = true
    ptomove = false
    ctomove = false
    pselected = false
    xsel, ysel = 0, 0
    laststate = ""

    function update!()
        for i in 1:3, j in 1:3
            set_gtk_property!(buttons[i, j], :label, unipawns[state.board[i, j]])
        end
        if (w = checkforwin(state)) != UInt8(' ')
            if pwhite == (w == whitepawn)
                push!(avoided, laststate)
            end
            won = (w == whitepawn) ? "White Has Won" : "Black Has Won"
            ptomove, ctomove = false, false
        else
            won = ""
        end
        set_gtk_property!(win, :title, "$won Hexapawn Game")
    end

    function initialize!()
        state = HState(b"www   bbb", true)
        won = ""
        pselected = false
        update!()
    end

    function newgame!(p)
        initialize!()
        if p == whitepawn
            pwhite = true
            ptomove, ctomove = true, false
        else
            pwhite = false
            ptomove, ctomove = false, true
        end
    end

    function domove!(board, m)
        board[m[4], m[3]], board[m[2], m[1]] = board[m[2], m[1]], UInt8(' ')
        update!()
    end

    function findrowcol(button)
        for i in 1:3, j in 1:3
            if buttons[i, j] == button
                return i, j
            end
        end
        return 0, 0
    end

    function playerclicked(button)
        update!()
        if won == "" && ptomove
            j, i = findrowcol(button)
            if !pselected && i > 0 &&
                state.board[j, i] == (pwhite ? whitepawn : blackpawn)
                xsel, ysel = i, j
                pselected = true
            elseif pselected
                if islegalmove(state.board, xsel, ysel, i, j)
                    domove!(state.board, [xsel, ysel, i, j])
                    xsel, ysel = 0, 0
                    pselected = false
                    ptomove = false
                    ctomove = true
                    state.whitetomove = !state.whitetomove
                else
                    pselected = false
                    xsel, ysel = 0, 0
                end
            end
        end
        update!()
    end

    function computerplay!()
        while true
            if won == "" && ctomove
                cmoves = Vector{Vector{Int}}()
                update!()
                if string(state) == "www   bbb|t"
                    push!(cmoves, rand([[1, 1, 2, 1], [1, 2, 2, 2], [1, 3, 2, 3]]))
                else
                    for p in allavailablemoves(state.board, state.whitetomove), m in p[2]
                        b = deepcopy(state.board)
                        i1, j1, i2, j2 = p[1][1], p[1][2], m[1], m[2]
                        b[j1, i1], b[j2, i2] = b[j2, i2], b[j1, i1]
                        newstate = HState(b, !state.whitetomove)
                        x = checkforwin(newstate)
                        if x != space && state.whitetomove == (x == whitepawn)
                            empty!(cmoves)
                            push!(cmoves, [i1, j1, i2, j2])
                            break
                        elseif !(string(newstate) in avoided)
                            push!(cmoves, [i1, j1, i2, j2])
                        end
                    end
                end
                cmove = rand(cmoves)
                ptomove, ctomove = true, false
                state.whitetomove = !state.whitetomove
                domove!(state.board, cmove)
                laststate = string(state)
            end
            yield()
            sleep(0.2)
        end
    end

    for i in 1:3, j in 1:3
        signal_connect(playerclicked, buttons[i, j], "clicked")
    end
    newplayerwhitegame!(w) = newgame!(whitepawn)
    newplayerblackgame!(w) = newgame!(blackpawn)
    signal_connect(newplayerwhitegame!, newWgame, :clicked)
    signal_connect(newplayerblackgame!, newBgame, :clicked)
    newplayerwhitegame!(win)
    condition = Condition()
    endit(w) = notify(condition)
    signal_connect(endit, win, :destroy)
    showall(win)
    @async computerplay!()
    wait(condition)
end

hexapawnapp()

```


### Pretaught Version

Unlike full chess, the play here is simple enough to specify perfectly played games with just a 17 move table and one-move lookahead.

```julia
using Gtk, Base

const whitepawn = UInt8('w')
const blackpawn = UInt8('b')
const space = UInt8(' ')
const unipawns = Dict(space => " ", whitepawn => "\u2659", blackpawn => "\u265f")
ispawn(c) = (c == whitepawn || c == blackpawn)
oppositepawn(c1, c2) = ispawn(c1) && ispawn(c2) && c1 != c2

mutable struct HState
    board::Matrix{UInt8}
    whitetomove::Bool
    HState(arr, iswhite) = new(reshape(UInt8.(arr), 3, 3), iswhite)
end
string(h::HState) = join([Char(c) for c in h.board], "") * (h.whitetomove ? "|t" : "|f")

const positionmoves = Dict(
    "w w b  bb|t" => [1, 1, 1, 2], "w w b bb |t" => [1, 3, 2, 3], "w   bbb  |t" => [1, 1, 2, 2],
    "  wbb   b|t" => [1, 3, 2, 2], "w  bbw b |t" => [2, 3, 3, 3], "w   bb  b|t" => [1, 1, 2, 1],
    " wwb  b b|t" => [1, 2, 2, 2], "  wbb   b|t" => [1, 3, 2, 2], "  wbb b  |t" => [1, 3, 2, 2],
    "ww   bb b|t" => [1, 2, 2, 2], "w   b   b|t" => [1, 1, 2, 2], "w    bb  |t" => [1, 1, 1, 2],
    " www  bbb|f" => [3, 2, 2, 1], "ww   wbbb|f" => [3, 2, 2, 3], "w w w bbb|f" => [3, 1, 2, 2],
    "  ww  b b|f" => [3, 3, 2, 3], "w   w  bb|f" => [3, 3, 2, 3])

function legalmoves(board, px, py)
    moves = Vector{Pair{Int, Int}}()
    c = board[py, px]
    newrow = px + (c == whitepawn ? +1 : -1)
    if ispawn(c) && 0 < newrow < 4
        if py > 1 && oppositepawn(c, board[py - 1, newrow])
            push!(moves, Pair(newrow, py - 1))
        end
        if board[py, newrow] == UInt8(' ')
            push!(moves, Pair(newrow, py))
        end
        if py < 3 && oppositepawn(c, board[py + 1, newrow])
            push!(moves, Pair(newrow, py + 1))
        end
    end
    moves
end

islegalmove(board, px, py, i, j) = Pair(i, j) in legalmoves(board, px, py)

function allavailablemoves(board, forwhite)
    allret = Vector{Pair{Vector{Int}, Vector{Pair{Int, Int}}}}()
    for i in 1:3, j in 1:3
        if (board[j, i] == whitepawn && forwhite) || (board[j, i] == blackpawn && !forwhite)
            legmov = legalmoves(board, i, j)
            if !isempty(legmov)
                push!(allret, [i, j] => legmov)
            end
        end
    end
    allret
end

function checkforwin(hstate)
    if any(x -> hstate.board[x] == whitepawn, 7:9)
        return whitepawn # white win
    elseif any(x -> hstate.board[x] == blackpawn, 1:3)
        return blackpawn # black win
    else
        if length(allavailablemoves(hstate.board, hstate.whitetomove)) == 0
            return hstate.whitetomove ? blackpawn : whitepawn
        end
    end
    UInt8(' ') # hstate is not a winning position
end

function hexapawnapp()
    win = GtkWindow("Hexapawn Game", 425, 425) |> (GtkFrame() |> (box = GtkBox(:v)))
    toolbar = GtkToolbar()
    newWgame = GtkToolButton("New Game, Play as White")
    set_gtk_property!(newWgame, :label, "New Game, Play as White")
    set_gtk_property!(newWgame, :is_important, true)
    newBgame = GtkToolButton("New Game, Play as Black")
    set_gtk_property!(newBgame, :label, "New Game, Play as Black")
    set_gtk_property!(newBgame, :is_important, true)
    map(w->push!(toolbar,w),[newWgame, newBgame])
    scrwin = GtkScrolledWindow()
    grid = GtkGrid()
    map(w -> push!(box, w),[toolbar, scrwin])
    push!(scrwin, grid)
    buttons = Array{Gtk.GtkButtonLeaf, 2}(undef, 3, 3)
    stylist = GtkStyleProvider(Gtk.CssProviderLeaf(data="button {font-size:64px;}"))
    for i in 1:3, j in 1:3
        grid[i, 4-j] = buttons[i, j] = GtkButton()
        set_gtk_property!(buttons[i, j], :expand, true)
        push!(Gtk.GAccessor.style_context(buttons[i, j]), stylist, 600)
    end

    state = HState(b"www   bbb", true)
    won = ""
    pwhite = true
    ptomove = false
    ctomove = false
    pselected = false
    xsel, ysel = 0, 0

    function update!()
        for i in 1:3, j in 1:3
            set_gtk_property!(buttons[i, j], :label, unipawns[state.board[i, j]])
        end
        if (w = checkforwin(state)) != UInt8(' ')
            won = (w == whitepawn) ? "White Has Won" : "Black Has Won"
            ptomove, ctomove = false, false
        else
            won = ""
        end
        set_gtk_property!(win, :title, "$won Hexapawn Game")
    end

    function initialize!()
        state = HState(b"www   bbb", true)
        won = ""
        pselected = false
        update!()
    end

    function newgame!(p)
        initialize!()
        if p == whitepawn
            pwhite = true
            ptomove, ctomove = true, false
        else
            pwhite = false
            ptomove, ctomove = false, true
        end
    end

    function domove!(board, m)
        board[m[4], m[3]], board[m[2], m[1]] = board[m[2], m[1]], UInt8(' ')
        update!()
    end

    function findrowcol(button)
        for i in 1:3, j in 1:3
            if buttons[i, j] == button
                return i, j
            end
        end
        return 0, 0
    end

    function playerclicked(button)
        update!()
        if won == "" && ptomove
            j, i = findrowcol(button)
            if !pselected && i > 0 &&
                state.board[j, i] == (pwhite ? whitepawn : blackpawn)
                xsel, ysel = i, j
                pselected = true
            elseif pselected
                if islegalmove(state.board, xsel, ysel, i, j)
                    domove!(state.board, [xsel, ysel, i, j])
                    xsel, ysel = 0, 0
                    pselected = false
                    ptomove = false
                    ctomove = true
                    state.whitetomove = !state.whitetomove
                else
                    pselected = false
                    xsel, ysel = 0, 0
                end
            end
        end
        update!()
    end

    function computerplay!()
        while true
            if won == "" && ctomove
                cmove = [0, 0, 0, 0]
                update!()
                if string(state) == "www   bbb|t"
                    cmove = rand([[1, 1, 2, 1], [1, 2, 2, 2], [1, 3, 2, 3]])
                elseif haskey(positionmoves, string(state))
                    cmove = positionmoves[string(state)]
                else
                    for p in allavailablemoves(state.board, state.whitetomove), m in p[2]
                        b = deepcopy(state.board)
                        i1, j1, i2, j2 = p[1][1], p[1][2], m[1], m[2]
                        b[j1, i1], b[j2, i2] = b[j2, i2], b[j1, i1]
                        newstate = HState(b, !state.whitetomove)
                        x = checkforwin(newstate)
                         if state.whitetomove == (x == whitepawn)
                            cmove = [i1, j1, i2, j2]
                            break
                        end
                    end
                end
                if cmove[1] == 0
                    throw("No known move for position $state")
                else
                    ptomove, ctomove = true, false
                    state.whitetomove = !state.whitetomove
                    domove!(state.board, cmove)
                end
            end
            yield()
            sleep(0.2)
        end
    end

    for i in 1:3, j in 1:3
        signal_connect(playerclicked, buttons[i, j], "clicked")
    end
    newplayerwhitegame!(w) = newgame!(whitepawn)
    newplayerblackgame!(w) = newgame!(blackpawn)
    signal_connect(newplayerwhitegame!, newWgame, :clicked)
    signal_connect(newplayerblackgame!, newBgame, :clicked)
    newplayerwhitegame!(win)
    condition = Condition()
    endit(w) = notify(condition)
    signal_connect(endit, win, :destroy)
    showall(win)
    @async computerplay!()
    wait(condition)
end

hexapawnapp()

```



## Pascal

This was written in 1995 for Turbo Pascal which was designed when windows were known only at Xerox then Apple, and file names were all in capital letters. As a result, it is set up to be started with parameters, from a command line interface. Err... DOS, like. If invoked with a ? as parameter (as in <code>PawnPlex.exe ?</code>) then a lengthy description will be rolled forth. Otherwise, the first two parameters are NR and NC for the number of rows and columns. If no parameters, the assumptions are for an 8×8 board. With suitable parameters, and also key strokes as the run proceeds you can elect to play one side or the other, both, or neither. Players are known as O and X for visual distinction, and are coloured as well: white and yellow. Additional parameters (and key strokes) allow various aspects of the process to be displayed or not.

On initiation, a file directory PAWNPLEX is created. As play proceeds, moves and their options are displayed on the screen along with the board, and details are kept in two files, one for O and one for X, with names according to the size of the board so file R8C8O.PPX would be for O and having Rows eight, and Columns eight. These files (via a hash function) store a description of every board position encountered along with the possible moves from that position, and, which moves are known to be bad. This last is established when a game is won, and thus, the losing side thereby knows that its last move was bad and should never be made again. Another game can then be started from the beginning, or, the last game resumed with the bad move not made. If from that position there were no non-bad moves remaining, then the losing side knows that its previous move is bad, because it leads to a position from which the enemy could choose a move to a position from which no possible move avoids a loss.

Eventually, it will be discovered that at the start position, either the first or the second player has no initial move available that evades a losing position. Thus, one player has a certain win plan known, either O or X. Accordingly, either an O or an X will be written to special text files WINSO.TXT, WINSX.TXT and WINSWHO.TXT (don't edit them! Their exact layout is important!) at a position corresponding to the number of rows and columns of the board shape for which the result has just been decided.

Option "witless" involves ''no'' position analysis at all, not even detecting that a single move amongst those possible would win. In that case, all is determined via the analysis of the game tree. Each player selects its move from its list of all possible moves without ratiocination, and the process is such that one player prefers to advance on a broad front while the other advances in a single thrust. Then they encounter each other and the battle begins. Otherwise, with "witless" suppressed, should one of those possible moves deliver a win, it will be selected. Not a deep analysis at all. Such an analysis would involve preparing a game tree of future moves and counter moves, possibly decorated by position evaluations according to some ad-hoc scheme (even if given the fancy name of "heuristic") that on average (or something) enables trimming the branch explosion, but this is not what is desired. Pawnplex is ''already'' developing a game tree, and it is decorating it with ''certain'' knowledge (namely, led to a loss) with a view to delivering a final report of certain knowledge, that X (or O) has a sure win.

One reason for writing this programme was to support an essay I had planned on the difference between thinking and computation. For instance, for some board shapes it is apparent (via thinking) that the forced winner is known, even obvious, without playing any game. But, via computation, the programme attains this only via slogging through the computation.

### Source


```Pascal

{$N- No serious use of floating-point stuff.}
{$B- Early and safe resolution of  If x <> 0 and 1/x...}
{$Q- No overflow checking, as in the hash computation.}
{$M 24000,300000,655360} {The stackIndex is 65521 words (last prime before 65536).}
Program Splash; Uses CRT, DOS {Ugh. I'd rather not!};
{Perpetrated by R.N.McLean (whom God preserve), Victoria University, December VMM.}

{  The following games are special, and demonstrate that a move can be lost.
In other words, a certain board position can be attained by both players
so that when considering the list of possible moves attached to a board
position, two lists must be kept for such board positions. This is most
easily done by considering that an indication of which player is to move
forms part of the description of the position so that two states are
recorded, each with one list of possible moves. In still other words,
although the layout of the pieces is the same, the board position is different.

                     Start
               OOOO          OOOO
               ++++          ++++
               ++++          ++++
               XXXX          XXXX
         11|21         11|21
               +OOO          +OOO
               O+++          O+++
               ++++          ++++
               XXXX          XXXX
         41|31         41|31
               +OOO          +OOO
               O+++          O+++
               X+++          X+++
               +XXX          +XXX
         12|22         12|22
               ++OO          ++OO
               OO++          OO++
               X+++          X+++
               +XXX          +XXX
         31/22         42|32           Deviation...
               ++OO          ++OO
               OX++          OO++
               ++++          XX++
               +XXX          ++XX
         13/22         21\32
               +++O          ++OO
               OO++          +O++
               ++++          XO++
               +XXX          ++XX
         43|33         43\32
               +++O          ++OO
               OO++          +O++
               ++X+          XX++
               +X+X          +++X
         21|31         22/31
               +++O          ++OO
               +O++          ++++
               O+X+          OX++
               +X+X          +++X
         33|23         44|34
               +++O          ++OO
               +OX+          ++++
               O+++          OX+X
               +X+X          ++++
         14/23         13|23
               ++++          +++O
               +OO+          ++O+
               O+++          OX+X
               +X+X          ++++
         42\31         32/23
               ++++          +++O
               +OO+          ++X+
               X+++          O++X
               +++X          ++++
         22/31         14/23
               ++++          ++++   NB! This is exactly the same board
               ++O+          ++O+   position as was attained in the first
               O+++          O++X   game, but one move further on.
               +++X          ++++   The boot is on the other foot.
         44|34         34\23        So the kick goes the other way.
               ++++          ++++
               ++O+          ++X+
               O++X          O+++   (O wins: attains final row.)
               ++++          ++++
         23\34
               ++++
               ++++  (X loses: no moves)
               O++O
               ++++

}

 Function Max(i,j: integer): integer; Begin if i > j then Max:=i else max:=j; End;
 Function Min(i,j: integer): integer; Begin if i < j then min:=i else min:=j; End;
 Function Ifmt(i: longint): string;
  Var s: string[11];
  Begin
   Str(i,s);
   Ifmt:=s;
  End;

 Var AsItWas: record mode: word; ta: word; end;
 Procedure Croak(Gasp: string);        {A lethal word.}
  Begin
   WriteLn;
   WriteLn(Gasp);
   AsItWas.Mode:=LastMode; {Trick to avoid a screen scrub during the Egress!}
   HALT;                   {This way to the egress...}
  End;

 Const ESC = #27; CR=#13; LF=#10; BS=#8;
 Var   Trace: boolean;     {Some confusion may arise.}

{                 Screen and board layout stuff.}
 Var   TrailColumns: byte;
 Const RowLimit = 29;      {Because of ShowStyle..}
 Const ColumnLimit = 38;   {Max of 38 columns for an eighty-column screen.}
 Const ColumnIdLimit = 62; {Identity list.}
 Const NumberCode: array[0..ColumnIdLimit] of char =
  '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
 Var   NR,NC: integer;     {Number of rows and columns in use, this run.}

 Const                        {Colours for the board.}
  Bright = Lightgray;         {The chequer.}
  Dark = black;               {The background.}
  Border = brown;             {For the playing board.}
  Scale = brown;              {Obtrusive, but prevents confusion.}

 Const PlayerColour: array[0..2] of byte = (Bright,White,Yellow);
 Const PlayerSymbol: array[0..2] of char = (' ','O','X');

 Var Board: array[0..RowLimit + 1,0..ColumnLimit + 1] of byte; {Constants!@##%$%!!}
 Var BoardSync: boolean;      {It can be left behind.}
 Var ListSync: boolean;       {Options may be flipped inconveniently.}

 Const left = -1; ahead = 0; right = +1;
 Const MoveMark: array[left..right] of char = ('/','|','\');
 Const MoveMask: array[left..right] of byte = ( 4,  2,  1);
 Const MoveFan: array[0..7] of byte = (0,1,1,2,1,2,2,3); {Bit counts.}
 Type  MoveBag = byte; {I really want a packed array of bits...}
 Var   AMove: array[1..ColumnLimit] of MoveBag;

 Const gameword: array[-1..+1] of string[7] = (' lost.',' flags.',' wins.');

 Var ShowBoardMoves,ListMoves,ShowResult,
     StepWise,Continual,WithRetraction,WitLess,QuitRun: boolean;
 Var FullRecall,Apotheosis: boolean;

 Procedure DealWith(Key: char); Forward;
 Function KeyFondle: char;             {Equivalent to ReadKey, except...}
  Var ticker: integer;                 { after a delay, it gives a hint.}
  Var cx,cy,ta: byte;                  {Who knows what was happening.}
  Begin                                {Screen and keyboard are connected by a computer...}
   Ticker:=666;                        {A delay counter.}
   While (ticker > 0) and not keypressed do
    begin                              { twiddle my thumbs.}
     ticker:=ticker - 1;               {My patience is being exhausted.}
     Delay(60)                         {Another irretrievable loss.}
    end;                               {Why no proper "Wait" feature?}
   if ticker <= 0 then                 {Had we run out of patience?}
    begin                              {Perhaps there is doubt at the keyboard.}
     cx:=wherex; cy:=wherey;           {So, where is the cursor?}
     ta:=TextAttr;                     {Save the style too.}
     TextAttr:=Blink + Blue*16 + LightRed;
     Write('Press a key!');            {Hullo sailor!}
    end;                               {Perhaps a provocation.}
   KeyFondle:=ReadKey;                 {This shouldn't waste cpu time.}
   if ticker <= 0 then                 {But it seems to, more tightly than Delay.}
    begin                              {Anyway, a key has now been pressed.}
     GoToXY(cx,cy);                    {So, scrub the hint that was given.}
     TextAttr:=Black; Write('            ');
     GoToXY(cx,cy);                    {Back to where I was.}
     TextAttr:=ta;                     {And how I was writing.}
    end;                               {So much for hints.}
  End; {of KeyFondle}

 Const LastPane = 4;                   {See ChooseALayout.}
 Type WindowPain = record col1,row1,col2,row2, CursorCol,CursorRow,style: byte; end;
 Var pane: array[1..LastPane] of WindowPain;
 const TheBoard = 1; TheScore = 2; TheFlags = 3; TheCommentary = 4;
 Var CurrentWindow: byte;              {The one of the moment.}
 Procedure LookTo(x: byte);            {Display areas.}
  Begin
   with pane[CurrentWindow] do         {Save the current state.}
    begin                              {Mickey mouse about.}
     CursorCol:=WhereX; CursorRow:=wherey; style:=TextAttr;
    end;                               {Piecemeal.}
   with pane[x] do                     {Now switch to the other.}
    begin                              {It would be nice to be able }
     Window(col1,row1,col2,row2);      { to do all this with just one }
     GoToXY(CursorCol,CursorRow);      { simple expression.}
     TextAttr:=style;                  {Like, if a thing called }
    end;                               { WindowState }
   CurrentWindow:=x;                   { was a known structure.}
  End;                                 {Ah well.}

{
### ==========Storage of information developed during the games.==========
}
 Const StashDirectory = 'PAWNPLEX';    {Just looking for a home... Gotta have a home...}
 Var   StashRecordSize: integer;       {A board description.}
 Const StashBranchLimit = 666;         {Forest foliage fan following.}
 var   StashBranch,StashPrevBranch: array[1..StashBranchLimit] of word;
 Const StashBranchCountLimit = 65535;  {Branches at a given stack level.}
 Const StashIndexLimit = 65521;        {Last prime before 65536.}
 Const StashChunkLast = 16383;         {Serious addressing problems.}
 Type  StashFingers = array[0..StashChunkLast] of word;{So, vexation!}
 type  StashBitHand = byte;            {Was Word, when one stash, not two.}
 const SBSHR = 3; SBAND = 7;           {Was 4, 15 for a 16-bit word.}
 type  StashBitFingers = array[0..StashChunkLast] of StashBitHand;
 Const StashMod = (SBAND + 1)*StashIndexLimit;  {8*=524186, 16*=1048336 A byte's worth of bits times that.}
 Const StashChunks = 3;                {Four in 0:3. Thus use SHR 2, AND 3. Be careful!}
 const SSHR = 2; SAND = 3;             {Thus these constants.}
 Const StashComfy = 30000;             {Many StashInBit bits will be zero.}
 Const StashFullish = 64200;           {Getting close to the edge.}
 Type  SRecNum = word;                 {I'm sticking with 16 bits as long as possible...}
 Var   StashRetract: boolean;          {See Burp.}
{I would like a bit array for StashBitFingers, but turbid Pascal disregards
"packed", despite the claim that this occurs "automatically", and uses one
bit per byte. So, I have to do the packing. Sigh. Previously, StashBitHand
was of type "word", but with the advent of a separate stash for each player,
storage limitations have reduced that to byte.}
 Const BitNum: array [0..15] of word = (1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768);

 Type  RowAndColumn = record col,row: byte; end; {Accursed byte swapping!}
 Var   Player: array[1..2] of          {Some games could have N players...}
        record                         {But only two here. Four if left&right as well.}
         header: record                {Maintained from one run to the next.}
          VictoryCount: longint;       {First things first!}
          StashNewPosition: longint;   {Keep some statistics }
          StashOldPosition: longint;   { of the stashes usefulness.}
          StashCount: word;            {There could be rather more than 64k, alas.}
          Paranoia: byte;              {This really is important information...}
         end;                          {So much for retained info.}
         PreviousVCount: longint;      {Previously shown Victory count.}
         StashFile: file;              {There was one for both players.}
         StashName: string;            {But now two lots of 64k limits, not one.}
         StashGrows: boolean;          {Additions overwrite the filed StashInRec.}
         StashAche: boolean;           {Storage problems impend?}
         StashEarly: word;             {Earliest (lowest) prune level.}
         StashInRec: array[0..StashChunks] of ^StashFingers;
         StashInBit: array[0..StashChunks] of ^StashBitFingers;
         Piece: array[1..ColumnLimit] of RowAndColumn;
         Keyboard: boolean;            {There may be dirigisme from outside.}
        end;
 Const StashRecordBase = 0             {A header at the front adds to the fun.}
     + SizeOf(Player[1].Header);       {So the file doesn't have fixed-size records.}
 Const Empty = 0; NoEntry=255;         {Neither empty nor 1 or 2, the player numbers.}

 Procedure ScrubStashFingers(who: byte);
  var i: word;
  Begin                                {Tiresome drivel.}
   with Player[who] do
    for i:=0 to StashChunks do         {Why can't I just put StashBranch:=0?}
     begin                             {Or, Clear StashInRec[*]^,StashInBit[*]^?}
      FillChar(StashInRec[i]^,SizeOf(StashFingers),0);
      FillChar(StashInBit[i]^,SizeOf(StashBitFingers),0);
     end;
   i:=who;
   Repeat
    StashBranch[i]:=0;
    StashPrevBranch[i]:=0;
    i:=i + 2;
   until i > StashBranchLimit;
  End;

 Var SP: integer;        {Enthusiastic stepping back can make this negative.}
 Var MirrorLevel: integer;
 Var MirrorPlane: byte;
 Var Mirror: boolean;
 Type
  zash = record          {Describes the current game position, in core memory.}
   prev: pointer;        {It's going to be a stack...}
   zHashIndex: longint;  {Save the stash hash index for deferred StashBloat.}
   zStashRec: srecnum;   {Where the stuff will be stashed in the file.}
   zMoveMade: shortint;  {The move's column shift: -1,0,+1}
   zPieceMoved: byte;    {The piece number in my position list.}
   zChanged: boolean;    {If Re-educate had its way with this state.}
   zRecLink: srecnum;    {A linked list in the stash file.}
   zSP: word;            {For pruning level control, when stashed.}
   zOrigMoveCount: byte; {Help with a discouraging display.}
   zstuff: array[1..5*ColumnLimit] of byte; {NB! not actually full sized when used...}
  end;     {place[2,NC],move[NC], is what I want, damnit!}
  zpointer=^zash;        {A finger type.}
 var zptr: zpointer;     {Sigh. Not abstract enough.}
 var zSize,zMoveOffset: word;{Like, *I* have to do the indexing for cut-to-fit work areas.}
 {  The Zash records might be regarded as an array, indexed by the stack pointer,
 thus Zash(SP) stashes information of interest during a game. However, rather than an
 array (and turbid Pascal is not flexible about array sizes and shapes), each Zash
 is a blob of memory of a size determined by the number of columns, NC,
 and a stack of them is allocated and released as the games are played.
    When a record is to be stashed on disc, because the stack is being cut back
and the Zash contains something worth keeping (that a move is found to be bad),
what is written is from zRecLink on, not the whole Zash. This is the information
about the board position that may be of use in a later game, however it also
includes some extra information for convenience even though this increases the
record size and thus slows the whole process.}

 Function LiveMoves(p: zpointer): word;
  var i,hit: word;
  Begin
   hit:=0;
   for i:=1 to NC do hit:=hit + MoveFan[p^.zstuff[zMoveOffset + i]];
   LiveMoves:=hit;
  End;

 Function ZGrab(Var p:zpointer): boolean;
  var t: zpointer;
  Begin
   t:=nil; GetMem(t,zSize);  {Grab only enough for this run's board size.}
   if t <> nil then          {Success?}
    begin                    {Yes.}
     t^.prev:=p;             {Finger the predecessor.}
     p:=t;                   {This is the new work place.}
    end;                     {Thus prepare a linked stash of work places.}
   ZGrab:=t<>nil;            {Announce results.}
  End;

 Procedure StashBloat(who: byte); Forward;
 Function WriteZStash(who: byte): boolean; Forward;

 Procedure ZDrop;            {Return a work space to the memory manager.}
  var t: zpointer;           {But it may need to be saved on disc.}
  var who,ta: byte;          {So we have to figure out which player}
  Begin
   ta:=TextAttr;
   who:=2 - (SP and 1);      {Player one strikes first, at level one.}
   if Trace then
    begin
     textcolor(PlayerColour[who]);
     Write('ZDrop:      SP=',SP:2,',who=',who);
    end;
   if (zptr <> nil) then               {General suspicion.}
    begin                              {Well, we have something.}
     if Trace then WriteLn(',z^.zChanged=',zptr^.zChanged,',z^.zStashRec=',zptr^.zStashRec);
     if zptr^.zChanged then            {Has something been learnt?}
      if zptr^.zStashRec = 0 then stashbloat(who) {Yes: add to the stash.}
       else if not WriteZStash(who) then croak('ZDrop: can''t update the stash!');
     t:=zptr^.prev;                    {Recall the predecessor.}
     FreeMem(zptr,zSize);              {Hand the storage back.}
     zptr:=t;                          {Now finger the predecessor as the current.}
    end;                               {So much for retreats.}
   TextAttr:=ta;
   SP:=SP - 1;                         {Humm, not all callers care for this.}
  End;

 Procedure ShowZStashPlaces;
  var i,j,l: integer;
  var ta: byte;
  var flip: boolean;
  Begin
   if zptr = nil then begin Write('-+Nil+-'); EXIT; end;
   ta:=TextAttr;
   for i:=1 to 2 do
    begin
     TextColor(PlayerColour[i]);
     flip:=false;
     for j:=1 to NC do
      Begin
       if flip then TextBackground(red) else TextBackground(green);
       flip:=not flip;
       l:=(i - 1)*NC*2 + (j - 1)*2 + 1; {Beware byte swapping!}
       Write(NumberCode[zptr^.zstuff[l+1]],NumberCode[zptr^.zstuff[l]]);
      end;
     TextBackground(Black);
     if i < 2 then Write(';');
    end;
   TextAttr:=ta;
  End;
 Procedure ShowZStashMoves;
  Var i,j,l,p,w,n: integer;
  var ta: byte;
  var flip: boolean;
  Begin
   if zptr = nil then begin Write('=#Nil#='); EXIT; end;
   ta:=TextAttr;                       {Colour changes allow tighter packing.}
   p:=2 - (zptr^.zSP mod 2);           {SP=1 Player 1, SP=2 Player 2, etc.}
   w:=3 - 2*p;                         {= +1 or -1.}
   p:=(p - 1)*2*NC;                    {Player's place list pointer.}
   Write(':');                         {The move display should be recognisable.}
   n:=0;                               {No scribbles as yet.}
   flip:=false;                        {Instead of spaces, colour changes.}
   for i:=1 to NC do                   {Standardise banishes empty players to the end.}
    if (zptr^.zstuff[p+i*2-1] <> 0) or (zptr^.zstuff[p+i*2] <> 0) then
     begin
      if flip then TextBackground(red) else TextBackground(green);
      flip:=not flip;
      l:=zMoveOffset + i;              {Locate the i'th MoveBag.}
      if zptr^.zstuff[l] = empty then begin Write('-'); n:=n + 1; end
       else for j:=left to right do
        if (MoveMask[j] and zptr^.zstuff[l] <> 0) then
         begin
          Write(MoveMark[w*j]);
          n:=n + 1;
         end;
     end;
   TextAttr:=ta;                       {Back to whatever.}
   for i:=n+1 to NC+2 do Write(' ');   {Encourage alignment.}
   ClrEol;                             {Prepare for what follows.}
  End; {of ShowZStashMoves.}

 Procedure ShowZStash;
  Begin
   if zptr = nil then begin write('=Z.nil='); EXIT; end;
   ShowZStashPlaces;
   ShowZStashMoves;
  End; {of ShowZStash.}

 Function HashedPlaceBit: longint;
  Var Hash: longint;
  var i,rc: integer;
  Begin
   Hash:=666;                          {Might as well be something strange.}
   rc:=NR*NC;                          {Crunch once.}
   for i:=1 to NC do                   {Step through the place list.}
    hash:=hash*1327                    {Make a real mess.}
     + Integer(Player[1].Piece[i])*rc  {These calculations have not been extensively tested.}
     + Integer(Player[2].Piece[i]);    {Perhaps some other would be more than marginally better.}
   hash:=hash AND $7FFFFFFF;           {Zap any negativeness.}
   if Trace then Write(' H=',hash);
   HashedPlaceBit:=Hash mod StashMod;  {Thus in 0:StashMod-1.}
  End;

 Procedure ShowWorkPlaces;
  var i,j: integer;
  var ta: byte;
  var flip: boolean;
   Begin
    ta:=TextAttr;
    for i:=1 to 2 do
     begin
      TextColor(PlayerColour[i]);
      flip:=false;
      for j:=1 to NC do
       Begin
        if flip then TextBackground(red) else TextBackground(green);
        flip:=not flip;
        Write(NumberCode[Player[i].Piece[j].row],NumberCode[Player[i].Piece[j].col]);
       end;
      TextBackground(Black);
      if i < 2 then Write(';');
     end;
    TextAttr:=ta; ClrEol;
   End;

 Var Flabby: boolean;    {Either NR or NC is too big for 4 bits.}
 Var CramRec:            {But if not, a squeeze.}
  record
   sRecLink: srecnum;    {A linked list in the stash file.}
   sSP: word;            {For pruning level control.}
   sOrigMoveCount: byte; {Help with a discouraging display.}
   sstuff: array[1..5*ColumnLimit] of byte; {NB! As with zstuff, not actually full-sized when used...}
  end;
 Const CramBite = SizeOf(CramRec.sRecLink) + SizeOf(CramRec.sSP) + SizeOf(CramRec.sOrigMoveCount);
 Procedure SqueezeRec;                 {When the number of rows and columns }
  var i,j,l,p: integer;                { both are less than 16, some space can be saved.}
  var b,f: byte;                       {There are many possible board positions to be stored.}
  Begin                                {And disc file buffering works better on smaller files...}
   Move(zptr^.zRecLink,CramRec,CramBite);{Copy across the fixed part.}
   for i:=1 to 2*NC do                 {The squeeze begins.}
     CramRec.sstuff[i]:=(zptr^.zstuff[i*2-1] shl 4) or (zptr^.zstuff[i*2]);
   l:=2*NC + 1;                        {Now for the real fun.}
   p:=8; b:=0;                         {For each piece, there are three move bits.}
   for i:=1 to NC do                   {Squeeze them into a sequence of eight-bit holders.}
    begin                              {Rather than one set in each byte.}
     f:=zptr^.zstuff[2*NC*2 + i];      {Grab the i'th piece's move flags.}
     for j:=left to right do           {Step through them.}
      begin                            {Spitting out any completed bytes on the way.}
       p:=p - 1; if p < 0 then begin CramRec.sstuff[l]:=b; l:=l + 1; b:=0; p:=7; end;
       if (f and MoveMask[j]) <> 0 then b:=b or bitnum[p];
      end;                             {On to the next move possibility.}
    end;                               {On to the next piece.}
   CramRec.sstuff[l]:=b;               {The last is probably only part full.}
  End; {of SqueezeRec.}
 Procedure UnSqueezeRec;               {Oh for palindromic procedures.}
  var i,j,l,p: integer;
  var b,f: byte;
  Begin
   Move(CramRec,zptr^.zRecLink,CramBite);
   for i:=1 to 2*NC do                 {Fish out the place lists.}
    begin                              {Two on one run.}
     zptr^.zstuff[i*2-1]:=Cramrec.sstuff[i] shr 4;
     zptr^.zstuff[i*2  ]:=CramRec.sstuff[i] and 15;
    end;                               {On to the next place.}
   l:=2*NC;                            {Syncopation.}
   p:=0;                               {The first extraction attempt advances one.}
   for i:=1 to NC do                   {Righto, step through the pieces.}
    begin                              {Extracting their moves from the bit string.}
     f:=0;                             {So far, no move flags.}
     for j:=left to right do           {Chase after them.}
      begin                            {One by one.}
       p:=p - 1; if p < 0 then begin l:=l + 1; b:=CramRec.sstuff[l]; p:=7; end;
       if (b and bitnum[p]) <> 0 then f:=f or MoveMask[j];
      end;                             {On to the next.}
     zptr^.zstuff[2*NC*2 + i]:=f;      {Place the stuff.}
    end;                               {On to the next piece.}
  End; {of UnSqueezeRec.}

 Function ReadZStash(who: byte; rec: word): boolean;
  var ulp: word;
  Begin
   if zptr = nil then begin WriteLn('ReadZStash: nil!'); ReadZStash:=false; EXIT; end;
   if rec <= 0 then begin WriteLn('ReadZ: rec=',rec); ReadZStash:=false; EXIT; end;
   if Trace then Write('RdS',who,'@',rec:5,': ');
   With Player[who] do
    begin
     {$I-}Seek(StashFile,(rec - 1)*StashRecordSize + StashRecordBase);{$I+}
     ulp:=IOResult; if ulp <> 0 then Croak('ReadZStash'+Ifmt(who)+' sought '+ifmt(rec)
      +', and failed with ioresult='+ifmt(ulp));
     if Flabby then BlockRead(StashFile,zptr^.zRecLink,StashRecordSize,ulp)
      else
       begin
        BlockRead(StashFile,CramRec,StashRecordSize,ulp);
        UnsqueezeRec;
       end;
     if ulp = StashRecordSize then ReadZStash:=true{Why isn't BlockRead a FUNCTION?!!}
      else
       begin
        WriteLn; WriteLn('ReadZStash fails. Ulp=',ulp);
        ReadZStash:=false;
        Trace:=true; QuitRun:=true; StepWise:=true;
       end;
     if Trace then
      begin
       ShowZStash;
       Write('zL=',zptr^.zRecLink,',zSP=',zptr^.zSP:2,',zOMC=',zptr^.zOrigMoveCount);
       WriteLn;
      end;
    end;
  End; {of ReadZStash.}

 Function WriteZStash(who: byte): boolean;        {Roll a board position to disc.}
  var ulp: word;
  Begin
   if zptr = nil then begin WriteLn('WriteZStash: nil!'); WriteZStash:=false; EXIT; end;
   if Trace then
    begin
     Write('WrS',who:1,'@',zptr^.zStashRec:5,': ');
     ShowZStash;
     Write('zL=',zptr^.zRecLink,',zSP=',zptr^.zSP:2,',zOMC=',zptr^.zOrigMoveCount);
     WriteLn;
    end;
   With Player[who] do
    begin
     Seek(StashFile,(zptr^.zStashRec - 1)*StashRecordSize + StashRecordBase);
     if Flabby then BlockWrite(StashFile,zptr^.zRecLink,StashRecordSize,ulp)
      else
       begin
        SqueezeRec;
        BlockWrite(StashFile,CramRec,StashRecordSize,ulp);
       end ;
     if ulp = StashRecordSize then WriteZStash:=true {If (result:=blockRead()) <> OK then..}
      else
       begin
        WriteLn;WriteLn('WriteZStash fails. Ulp=',ulp);
        WriteZStash:=false;
       end;
    end;
   zptr^.zChanged:=false;              {In agreement now.}
  End; {of WriteZStash.}

 Procedure CopyWorkToStash;
  var i,j,l: integer;
  Begin
   if Trace then Write('Work>Stash: ');
   Move(Player[1].Piece[1],zptr^.zstuff[       1],     2*NC);
   Move(Player[2].Piece[1],zptr^.zstuff[2*NC + 1],     2*NC);
   Move(Amove[1],          zptr^.zstuff[zMoveOffset + 1],NC);
   if Trace then begin ShowZStash; WriteLn; end;
  End;
 Procedure CopyStashToWork;
  var i,j,l: integer;
  Begin
   if Trace then Write('Stash>Work: ');
   Move(zptr^.zstuff[1],       Player[1].Piece[1],2*NC);
   Move(zptr^.zstuff[2*NC + 1],Player[2].Piece[1],2*NC);
   Move(zptr^.zstuff[zMoveOffset + 1],AMove[1],NC);
   if Trace then begin ShowZStash; WriteLn; end;
  End; {of CopyWorkToStash.}

 Function StashMatchesPlace: boolean;
{  Watch out. For reasons of speed, the Move trick is used in rolling the
data between the Player.Piece lists and the zstuff. This has to be done
in two goes, since the Player.Piece array can't be sized to suit.
For further reasons of speed, the array is of a RowAndColumn, two bytes
in one word, and it is convenient to be able to deal with both at a go,
except when one at a time is needed.
   The difficulty that arises is that Move is a byte-based operation,
but RowAndColumn amounts to two bytes, a word. This wouldn't matter, were it
not for the crazed business of byte-swapping, engaged in by fwibmpcs and
others, so that the bytes within a word are not as one might think.
This "endian" issue thus could invalidate the comparison below on a different
cpu, though it is hardly likely that a Pascal source file could be transferred
without change anyway.
   Even more annoying, all that is of interest is whether the two runs of
bytes match or not, so if there were a Match function comparable to the Move
process, all could be dealt with, and swiftly. But no.}
  var i,j,l: integer;
  Begin
   if Trace then Write('Stash=Work? ');{Quite.}
   StashMatchesPlace:=false;             {Might as well be ready for a getaway.}
   l:=1;                               {Poke the finger.}
   for i:=1 to 2 do                    {Then follow Row,Col pairs.}
    With Player[i] do                  {Why should I suggest this to the compiler?}
     For j:=1 to NC do                 {Beware of byte-swapping!!}
      begin                            {I regard the Row as the first, but...}
       if (zptr^.zstuff[l+1] <> Piece[j].row)
        or (zptr^.zstuff[l] <> piece[j].col) then
         begin                         {A difference.}
          if Trace then WriteLn('nope.');
          EXIT;                        {Escape swiftly.}
         end;                          {But if no mismatch,}
       l:=l + 2;                       {On to the next row,col pair.}
      end;                             {Ah, indices.}
   if Trace then WriteLn('yeah.');     {If all else fails,}
   StashMatchesPlace:=true;              {There is a match.}
  End; {of StashMatchesPlace.}

 Procedure CopyStashToMoves;
  var i: integer;
  Begin
   if Trace then Write('Stash>Move: ');
   Move(zptr^.zstuff[zMoveOffset + 1],Amove[1],NC);
   if Trace then begin ShowZStash; WriteLn; end;
  End;

 Procedure SaveStashHeader(who: byte);
{   Note that on the fwibmpc a datum is written and retrieved from store with
its low-order byte first (as in the numbering of bits) rather than left to right.
This "endian" madness means that a four-byte datum (such as the Victorycount)
will appear as bytes 4321 (low-order bits first) as revealed in a hex dump,
which is strictly byte order. Thus, a value of 291 will appear not as hex
00,00,01,23 but as 23,01,00,00, and if read as two two-byte data, further
confusion arises.}
  var ugly: array[1..SizeOf(Player[1].Header)] of byte;
  var i: integer;
  Begin
   with Player[who] do
    begin
     if Trace then WriteLn('SaveStashHeader',who,'.');
     Seek(StashFile,0);              {In principle, this should happen in parallel, as in furrytran.}
     Move(Header,Ugly,SizeOf(Ugly)); {Why can't I just use "equivalence", as in furrytran?}
     Header.Paranoia:=0;             {Known to be the last entry, thus the -1 below.}
     for i:=SizeOf(Ugly) - 1 downto 1 do Header.Paranoia:=Header.Paranoia xor ugly[i];
     BlockWrite(StashFile,Header,SizeOf(Header));
    end;
  End; {of SaveStashHeader.}

 Function GrabStashHeader(who: byte): boolean;
  var ugly: array[1..SizeOf(Player[1].Header)] of byte;
  var hic: byte;
  var i: integer;
  Begin
   with Player[who] do
    begin
     if trace then Write('GrabStashHeader',who);
     Reset(StashFile,1); {Like, OPEN.}
     BlockRead(StashFile,Header,SizeOf(Header));
     Move(Header,Ugly,SizeOf(Ugly));
     Hic:=0; for i:=SizeOf(Ugly) downto 1 do hic:=hic xor ugly[i];
     if hic <> 0 then FillChar(Header,Sizeof(Header),0); {Crud! Scrub.}
     GrabStashHeader:=hic = 0; {Relies on (X xor X) = 0.}
    end;
  End;

 Procedure PrepareStash(who: byte);
  Begin
   Write('PrepareStash',who,': ');
   ReWrite(Player[who].StashFile,1);
   SaveStashHeader(who);               {Values are zeroed at start up.}
   if Trace then WriteLn('FilePos=',FilePos(Player[who].stashfile),',StashRecordBase=',stashrecordbase);
  End; {of PrepareStash.}

 Type StashStep = record Value: srecnum; Bits: StashBitHand; gap: word; end;
 Procedure ShowBits(w: StashBitHand);
  var i: byte;
  Begin
   for i:=0 to SBAND do      {Bits in a StashBitHand.}
    if (w and bitnum[i]) = 0 then write('-')
     else write('+');
  End;

 Procedure SaveStash(who: byte);       {All that is needed for LoadStash to recover.}
{  The stash contains a header, which has the value of StashCount,
 followed by StashCount records (of a size probably different from the header)
 followed by the stashfinger stuff, in two possible formats,
 followed by the StashBranch counts.
   As soon as StashBloat adds a record during a run, the stashfinger stuff
starts being overwritten by the added board positions. However, SaveStash
will follow the board positions by a fresh writing, ready for a restart.
Put another way, the StashCount board position records are already in place.
   There are various checks as to the validity of these data, and procedure
UnmangleStash stands ready to attempt a recovery of the board positions,
based on the value of StashCount in the header (but also checking), then
reconstitutes the stashfinger stuff. To reduce the amount exposed to loss,
the header is written at intervals. Only board position records written
after the value of StashCount that has been written to the file will be
lost when the stash file is re-read, as during a restart. Obviously, a normal
stop run is arranged to save everything properly, but something may go wrong,
and to reduce the amount of a possible loss, the header is updated occasionally.}
  Var i,it,l1,l2: word;
  Var Gulp: word;
  Var x: StashStep;                    {This may provoke a crash (by running out of space?)...}
  var nb: integer;                     {This may attain -1.}
  Var check: word;
  Begin
   if not Apotheosis then WriteLn('Just a moment; saving fingers for player ',who);
   with Player[who] do
    begin
     if Trace then WriteLn('SaveStash',who,': StashCount=',Header.StashCount);
     SaveStashHeader(who);             {That was easy.}
     Seek(StashFile,Header.StashCount*StashRecordSize + StashRecordBase);
     l2:=0;                            {Now prepare to scan the fingers.}
     if Header.StashCount > StashIndexLimit div 2 then {Worth compacting?}
       for i:=0 to StashChunks do      {No. Except that addressing problems force }
        begin                          { a piecemeal approach, rather than a big write.}
         BlockWrite(StashFile,StashInRec[i]^,SizeOf(StashFingers));
         BlockWrite(StashFile,StashInBit[i]^,SizeOf(StashBitFingers));
      end                              {So much for mass directness.}
      else                             {A sparse collection wastes space that way.}
       Repeat                          {So look for gaps between fingers.}
        L1:=l2;                        {Finger the start of the run.}
        While (l2 <= StashIndexLimit) and (StashInRec[l2 and SAND]^[l2 shr SSHR] = 0) do l2:=l2 + 1;
        x.gap:=l2 - l1;                {L2 fingers the value to save.}
        if l2 <= StashIndexLimit then  {Off the end yet?}
         begin                         {Nope.}
          x.value:=StashInRec[l2 and SAND]^[l2 shr SSHR];
          x.bits:=StashInBit[l2 and SAND]^[l2 shr SSHR];
         end                           {So much for another piece of grit.}
         else                          {But when we get to the end, }
          begin                        { roll a doorstop.}
           x.value:=0;                 {Like, the point is to save }
           x.bits:=0;                  { the non-zero entries.}
          end;                         {So much for a stopper.}
        if Trace then                  {Logorrhoea?}
         begin                         {Yep.}
          Write('L=',L2:5,',Gap=',x.gap:5);
          if x.value > 0 then          {The last takes us to the end of the array }
           begin                       {And LoadStash likes a positive statement.}
            Write(',Rec=',x.value:5,',Bits=');
            ShowBits(x.bits);          {Just for fun.}
           end;
          WriteLn;
         end;
        BlockWrite(StashFile,X,SizeOf(x));   {Splot!}
        l2:=l2 + 1;                    {The next candidate.}
        if KeyPressed then DealWith(ReadKey);
       Until x.value = 0;              {I'm sick of multiple tests.}
     check:=0; for i:=0 to StashIndexLimit do
      check:=check xor StashInRec[i and SAND]^[i shr SSHR] xor StashInBit[i and SAND]^[i shr SSHR];
     BlockWrite(StashFile,check,SizeOf(check));
     nb:=StashBranchLimit;             {Ignore the tail.}
     if (nb mod 2) <> (who mod 2) then nb:=nb - 1; {Players alternate...}
     While (nb >= who) and (StashBranch[nb] = 0) do nb:=nb - 2;
     if nb < 0 then nb:=0;             {We may have stepped back two from 1.}
     BlockWrite(StashFile,nb,SizeOf(nb));
     check:=0;
     it:=who;
     for i:=1 to nb do
      begin
       BlockWrite(StashFile,StashBranch[it],SizeOf(StashBranch[1]));
       check:=check xor StashBranch[it];
       it:=it + 2;
      end;
     BlockWrite(StashFile,check,SizeOf(check));
     StashGrows:=false;                {Since the last save, it hasn't.}
    end; {So much for Player[who].}
  End; {of SaveStash.}

 Procedure UnMangleStash(who: byte; why: string); {The supposition is that the board positions are safe.}
  Var i: word;                         {Risks are mostly at the end of a file, }
  var whomod: word;                    { where growth }
  Var Squawk: string;                  { may be rudely terminated.}
  Function Tasty: boolean;             {A token check on the position.}
   Procedure SquawkOn(more: string);   {Some complaint is raised.}
    Begin                              {So, add it to the litany.}
     if length(Squawk) <= 0 then Squawk:=more
      else Squawk:=Squawk + ' ' + more;{Some attempt at neatness.}
    End;
   var bad: boolean;                   {Gross file damage should be spotted.}
   var l,m,r,c: byte;                  {Even buffer tangles should show up }
   var hit: word;                      { as my records are odd sizes.}
   Begin
    Squawk:='';                        {None as yet.}
    if zptr^.zSP mod 2 <> whomod then SquawkOn('Player ' + Ifmt(who) + ' but SP='+Ifmt(zptr^.zSP));
    if zptr^.zRecLink >= i then SquawkOn('Linked to a following record.');
    if zptr^.zOrigMoveCount > 3*NC - 2 then SquawkOn('To have '+ifmt(zptr^.zOrigMoveCount) + ' moves is impossible.');
    bad:=false;                        {Perhaps the place list?}
    for l:=1 to 2*NC do                {Both players in one pass.}
     begin                             {Could check for order, but...}
      r:=zptr^.zstuff[l*2]; c:=zptr^.zstuff[l*2 - 1];
      if (r > NR) or (c > NC)          {Row or column number too big?}
       or ((r = 0) and (c <> 0))       {Invalid row?}
       or ((c = 0) and (r <> 0))       {Invalid column?}
       then bad:=true;                 {Both zero means no piece there.}
     end;                              {On to the next position.}
    if bad then SquawkOn('Damaged place list.')
     else                              {If the place list seems good, }
      begin                            {Check the move list.}
       hit:=0;                         {How many moves survive?}
       for l:=1 to NC do               {Step through the list.}
        begin                          {Each piece has a move opportunity.}
         m:=zptr^.zstuff[zMoveOffset + l];  {Grab it.}
         if m > 7 then bad:=true       {Three bits at most.}
          else hit:=hit + MoveFan[m];  {So, count its possibilities.}
        end;                           {On to the next move.}
       if bad then SquawkOn('Damaged move list.')
        else if hit > zptr^.zOrigMoveCount then SquawkOn('Extravagant move list.');
      end;                             {So much for the move list.}
    tasty:=(not bad) and (squawk = '');{Could compare the moves to the piece places, but...}
   End; {of function Tasty.}
  var it: longint;                     {Wot the hash function offers.}
  var i1,i2: word;                     {Annoyances due to inadequate addressing.}
  var j1,j2: word;                     {To two levels.}
  Begin {of UnMangleStash.}
   whomod:=who mod 2;                  {Which player?}
   With Player[who] do
    begin
     WriteLn('Whoops! Trouble with stash ',who,': ',why,'.');
     Write(Header.StashCount,' board position');{Have faith in this number...}
     if Header.StashCount <> 1 then write('s'); {Ah, the horde.}
     WriteLn(' languish.');            {They await readmission to the fold.}
     if not zgrab(zptr) then croak('Not enough memory!');
     ScrubStashFingers(who);           {In case of partial fillings.}
     i:=1; While (i <= Header.StashCount) and ReadZStash(who,i) and Tasty do
      begin                            {Grab a record.}
       CopyStashToWork;                {Expose it to others.}
       it:=HashedPlaceBit;             {Compute a hash index.}
       i1:=it shr SBSHR;               {Sigh. Can't have large arrays.}
       i2:=it and SBAND;               {Not even bit arrays...}
       j1:=i1 and SAND; j2:=i1 shr SSHR;
       if Trace then writeln(' S[',i1:5,']=',StashInRec[j1]^[j2]:5,',zSP=',zptr^.zSP:2);
       if zptr^.zRecLink <> StashInRec[j1]^[j2] then
        begin                          {Connection alterations?}
         zptr^.zRecLink:=StashInRec[j1]^[j2];   {The hash function may have changed.}
         zptr^.zStashRec:=i;           {Back to where it came from.}
         if not WriteZStash(who) then croak('Can''t re-write record '+ifmt(i));
        end;                           {So much for re-linking.}
       StashInRec[j1]^[j2]:=i;         {The last (if more than one) is the head.}
       StashInBit[j1]^[j2]:=StashInBit[j1]^[j2] or bitnum[i2];
       StashBranch[zptr^.zSP]:=StashBranch[zptr^.zSP] + 1;
       if i mod 1000 = 0 then WriteLn(i:5,' and counting.');
       if KeyPressed then DealWith(ReadKey);
       i:=i + 1;                       {Ridiculous drivel.}
      end;                             {For i:=1:StashCount While read & tasty do...}
     if i <= Header.StashCount then WriteLn('Humm. Position ',i,' is unhealthy. (',squawk,') Any followers are ignored.');
     Header.StashCount:=i - 1;         {The survivors.}
     WriteLn(Header.StashCount,' drew no suspicion.');
     WriteLn('Righto, all should be well.');
     zptr^.zChanged:=false; ZDrop;     {Cast aside the work area, preventing a write.}
     SaveStash(who);                   {Return to health, though perhaps rather thin health.}
    end;
  End; {of UnmangleStash.}

 Procedure LoadStash(who: byte);       {Retrieve my fingers, and stuff.}
  var i,it,n,gulp,ulp: word;           {The workers.}
  var urp: boolean;                    {Disc problems...}
  var x: StashStep;                    {Small finger collections are handled compactly.}
  var check,stashcheck: word;          {Checksum stuff.}
  Label 666;                           {Oh dear.}
  Begin
   WriteLn('Just a moment; grabbing fingers...');
   if Trace then Write('LoadStash',who);
   With Player[who] do
    begin
     if not GrabStashHeader(who) then WriteLn('Garbled header! Scrubbed!');
     if Trace then WriteLn(': StashCount=',Header.StashCount,',FilePos=',FilePos(StashFile));
     if Header.StashCount > 0 then     {Have we work?}
      begin                            {Yep.}
       urp:=false;                     {No problems, yet.}
       Seek(StashFile,Header.StashCount*StashRecordSize + StashRecordBase);
       if Header.StashCount > StashIndexLimit div 2 then
        begin                          {So many fingers, so few gaps.}
         for i:=0 to StashChunks do    {Addressing problems.}
          begin                        {So not one big bag, alas.}
           gulp:=SizeOf(StashFingers); {So grab them by the barrow load.}
           BlockRead(StashFile,StashInRec[i]^,gulp,ulp);
           if ulp <> gulp then urp:=true;
           gulp:=SizeOf(StashBitFingers);
           BlockRead(StashFile,StashInBit[i]^,gulp,ulp);
           if ulp <> gulp then urp:=true;
          end;                         {On to the next barrow load.}
         i:=StashIndexLimit + 1;       {In one big lump.}
        end                            {But otherwise, piecemeal.}
        else                           {It was saved with gap counting.}
         begin                         {Because there were lots of gaps.}
          i:=0;                        {So, start at the start.}
      666:BlockRead(StashFile,X,SizeOf(X),ulp);  {And grab a step.}
          if sizeof(x) <> ulp then WriteLn('Reached finger ',i,', then the disc file ended.')
           else                        {But this is the normal case.}
            begin                      {We have a step.}
             i:=i + x.gap;             {Pass over the gap.}
             if Trace then             {Logorrhoea?}
              begin                    {Yep.}
               Write('i=',i:5,',Gap=',x.gap:5);
               if x.value > 0 then     {Zero only for the last.}
                begin                  {Whose step should takes us to the array's end.}
                 Write(',Rec=',x.value:5,',bits=');
                 ShowBits(x.bits);     {Ah, why not.}
                end;
               WriteLn;
              end;
             if (x.value <> 0) and (i <= StashIndexLimit) then
              begin                    {Another one.}
               StashInRec[i and SAND]^[i shr SSHR]:=X.value;  {Place the value.}
               StashInBit[i and SAND]^[i shr SSHR]:=X.bits;   {Along with occupancy flags.}
               i:=i + 1;               {It occupied one slot.}
               if KeyPressed then DealWith(ReadKey);
               goto 666;               {To hell with multiple tests, etc.}
              end;                     {So much for a live one.}
            end;                       {So much for compaction.}
           end;                        {So much for that read.}
       if i <= StashIndexLimit then UnmangleStash(who,'finger shortage')
        else                           {We have a full complement.}
         begin                         {So inspect them.}
          check:=0;                    {If there was a disc file zap, }
          for i:=0 to StashIndexLimit do { this correlation is unlikely to survive.}
           check:=check xor StashInRec[i and SAND]^[i shr SSHR] xor StashInBit[i and SAND]^[i shr SSHR];
          gulp:=SizeOf(Check);         {Idiotic drivel!}
          BlockRead(StashFile,stashcheck,gulp,ulp);
          if ulp <> gulp then urp:=true;
          if urp then UnmangleStash(who,'blurred fingers')
           else if check <> stashcheck then UnMangleStash(who,'mangled fingers')
            else                       {Righto, the fingers are fine.}
             begin                     {Grab the branch counts.}
              BlockRead(StashFile,N,SizeOf(n)); {How many?}
              urp:=false;              {No trouble seen.}
              check:=0;                {But, look for it.}
              it:=who;                 {My twostepper started.}
              i:=1;                    {My counter started.}
              While (i <= n) and (it <= StashBranchLimit) and not urp do
               begin                   {Proceed with some caution.}
                BlockRead(StashFile,StashBranch[it],SizeOf(StashBranch[1]),ulp);
                if ulp <> SizeOf(StashBranch[1]) then urp:=true;
                check:=check xor StashBranch[it];
                StashPrevBranch[it]:=StashBranch[it]; {Thus follow this run's gains.}
                it:=it + 2;            {Dealing only with Player[who].}
                i:=i + 1;              {Advance to the next stashed count.}
               end;                    {More?}
              BlockRead(StashFile,stashcheck,SizeOf(check),ulp);
              if ulp <> SizeOf(Check) then urp:=true;
              if urp or (check <> stashcheck) then UnMangleStash(who,'Tangled branch counts');
             end;                      {So much for the branches.}
         end;                          {So much for the fingers.}
      end;                             {So much for StashCount > 0.}
    end;           {So much for Player[who].}
  End; {of LoadStash.}

 Procedure DescribeTheJungle; {Talk about the branches.}
  var i,j,last,lm,w: integer;
  var ns,np,d,no,nn,n: longint;
  var m: word;
  Var ta: word;
  Begin
   ta:=TextAttr;                       {Whatever.}
   w:=pane[CurrentWindow].col2 - pane[CurrentWindow].col1 + 1;
   TextBackground(Black); TextColor(LightGray);
   ClrEol;                             {Prepare to state the stashes' population.}
   Write('Stashed: ');                 {Start talking.}
   ns:=0;                              {There are two, now.}
   for i:=1 to 2 do                    {So, step through them.}
    begin
     TextColor(PlayerColour[i]);
     ns:=ns + Player[i].Header.StashCount;
     Write(Player[i].Header.StashCount);
     TextColor(LightGray);
     if i = 1 then Write(' + ');
    end;                               {On to the next stash.}
   Write(' = ',ns,' node');            {State total stash population.}
   if ns <> 1 then write('s');         {Ah, grammar!}
   if ns <= 0 then WriteLn             {If no nodes, no more to say.}
    else                               {But more likely,}
     begin                             {There are some.}
      last:=StashBranchLimit;          {Scan the branch counts.}
      While (last > 0) and (StashBranch[last] = 0) do last:=last - 1;
      m:=0;lm:=0;                      {Maximum count, and its level.}
      for i:=1 to last do if StashBranch[i] > m then begin m:=StashBranch[i]; lm:=i; end;
      WriteLn(', highest at level ',last);{The summit.}
      Write('Stash hits: ');           {How much use has been all the effort }
      for i:=1 to 2 do                 { that went into the stash?}
       begin                           {How has this run done?}
        TextColor(PlayerColour[i]);    {Strongly affected by the state of WithRetraction.}
        no:=Player[i].Header.StashOldPosition;{Save on some typing.}
        nn:=Player[i].Header.StashNewPosition;
        n:=no + nn;                    {Stash references.}
        if n <= 0 then write('none')   {Too eager for news?}
         else write(no,'/',n,' =',100.0*no/n:6:2,'%');
        if i = 1 then write(', ');     {Ah, details.}
       end;                            {On to the other player.}
      WriteLn;                         {So much for stash hit statistics.}
      TextColor(LightGray);            {Back to normal text.}
      Write('Counts: '); ClrEol;       {Now reveal the population counts.}
      for i:=1 to last do              {From the start.}
       begin                           {Even if zero.}
        TextColor(PlayerColour[2 - (i mod 2)]);
        Write(StashBranch[i]);         {Might as well have the player's colour.}
        TextColor(LightGray);          {But the punctuation is plain.}
        if i < last then               {More to come?}
         begin                         {Yep.}
          write(',');                  {So, punctuate.}
          if w - wherex < 6 then begin WriteLn; write('   '); ClrEol; end;
         end;                          {I hate this multiple testing!}
       end;                            {But test the next index, anyway, for the loop.}
      WriteLn('.');                    {Might as well terminate neatly.}
      ClrEol;
      if m > 1 then WriteLn('Thickest level is ',lm,', with ',m);
      if (Player[1].StashEarly < maxint) or (Player[2].StashEarly < maxint) then
       begin
        ClrEol; Write('Earliest change was at level');
        for i:=1 to 2 do
         begin
          TextColor(PlayerColour[i]);
          if Player[i].StashEarly < maxint then Write(' ',Player[i].StashEarly);
         end;
        WriteLn;
        TextColor(LightGray);
       end;
      while (last > 0) and (StashBranch[last] = StashPrevBranch[last]) do last:=last - 1;
      i:=1; while (i <= last) and (StashBranch[i] = StashPrevBranch[i]) do i:=i + 1;
      ClrEol;                          {Scrub any previous text.}
      Write('Added ');                 {Start explaining.}
      if i = 1 then WriteLn('no fresh branches.')
       else                            {But except at the start of stash additions, }
        begin                          { there will be added branches as games terminate.}
         np:=0; for j:=1 to last do np:=np + StashPrevBranch[j];
         d:=ns - np;                   {Differences.}
         if d <> 0 then                {Still in the first layer?}
          begin                        {Nope, additions.}
           Write(d,' node'); if d <> 1 then write('s');
           Write(' to level');         {Where to?}
           if i = last then write(' ',i){Perhaps just the one level.}
            else                       {Or maybe, to many.}
             begin                     {So name them.}
              write('s ',i,-last,': ');{And they're plural.}
              for j:=i to last do      {Skip the initial zero counts.}
               begin                   {If there were any.}
                TextColor(PlayerColour[2 - (j mod 2)]);
                d:=StashBranch[j]; d:=d - StashPrevBranch[j]; {Surely a better way exists!}
                Write(d);  {Perhaps negative: note promotion opportunity in SniffTheState.}
                TextColor(LightGray);  {Simple announcements.}
                if j < last then       {A successor awaits?}
                 begin                 {If so, make a nice list.}
                  write(',');          {With commas.}
                  if w - wherex < 6 then begin WriteLn; write('   '); ClrEol; end;
                 end;                  {And avoid line overflows.}
               end;                    {On to the next in the count list.}
             end;                      {So much for more than just one level.}
           WriteLn('.');               {Finish the line.}
          end;                         {So much for getting past the first layer.}
        end;                           {So much for there being some added branches.}
     end;                              {So much for there being nodes.}
   TextAttr:=ta;                       {Back to whatever.}
  End; {of DescribeTheJungle.}

 Procedure SpecialReport; {Some words about the state of the stash.}
  var cc,cr: byte;
  Begin
   cr:=wherey; cc:=wherex;
   GoToXY(1,pane[TheCommentary].row2 - 12);
   DescribeTheJungle;
   GoToXY(cc,cr);
  End;

 Var PruneLevel: integer;
 Procedure PruneAttack; {Ad-hoc adjustment of stash trimming.}
  var cc,cr: byte;
  var ta: word;
  Begin
   cr:=wherey; cc:=wherex; ta:=TextAttr;
   TextColor(Green); TextBackground(Black);
   repeat
    GoToXY(1,cr+1); ClrEol; Write('Nominate a pre-emptive prune level: ');
    {$I-} ReadLn(PruneLevel);{$I+}
   until IOResult = 0;
   GoToXY(1,cr+1); ClrEol;
   TextAttr:=ta; GoToXY(cc,cr);
  End;

 Procedure Burp(who: byte);            {Too much stuff in the stash!}
{   The obvious procedure is to step along the StashInRec array, copying
across each chain of records and dropping those that are not wanted.
Although the output file will be written to sequentially, the input
records will come from all over the stash file. In other words, the
random-access memory cells of the StashInRec will be read sequentially
while the preferably sequentially accessed disc file will be read randomly.
   Instead, the stash file can be read sequentially, and rather than
search the StashInRec array for any finger to the current record, the
hash function will reveal which equivalence class a record belongs to
by direct computation, since each record contains a copy of the place
list and mover code that was grist to the hash function in the first
place. As the first encountered record of an equivalence class will be
the first that was written, the new stash will grow in the same order
as the old did, merely skipping certain records that lack sufficient value.}
  Var i,j,p: integer;
  var n,blobcount,shead,bhead: word;
  var rec: srecnum;
  var it: longint;
  var i1,i2,j1,j2: word;
  var ta: byte;
  var mytrace: boolean;                {Local control.}
  Var bowl: file;                      {A temporary storage area.}
  Begin                                {Update in place risks a crash during the update.}
   ta:=TextAttr;                       {Who knows what was being written, how.}
   TextBackground(Black);
   if not trace then Clrscr;           {Clear for action.}
   TextColor(LightGreen);        WriteLn('Urrgh!!! I don''t feel so good...');
   TextColor(Red);               Write('It''s time to purge stash ');
   TextColor(PlayerColour[who]); WriteLn(who);
   TextColor(LightGray);               {Normal murmuring.}
   if trace then dealwith(keyfondle);
   SaveStash(who);                     {Thus allow recovery.}
   if trace then dealwith(keyfondle);
   n:=0; p:=who;                       {Now decide on a prune level.}
   repeat                              {Scan my branch count table.}
    n:=n + StashBranch[p];             {So many at this level.}
    p:=p + 2;                          {Finger the next level.}
   until (p > StashBranchLimit) or (n >= StashComfy);
   WriteLn('Prune level is ',p);  {One too far, and chop!}
   if (PruneLevel > 0) and (PruneLevel < p) then
    begin                              {A hack attack is desired.}
     p:=PruneLevel;                    {More brutal than that chosen.}
     WriteLn('But set to ',p,', by special command.');
    end;                               {Let the bodies fall where they may.}
   PruneLevel:=0;                      {Use once only, unless reactivated.}
   With Player[who] do
    begin
     if Trace then WriteLn('StashBurp',who,
      ':  Scomfy=',StashComfy,',Sfullish=',StashFullish,
      ',StashCount=',Header.StashCount,',SRSz=',StashRecordSize);
     Assign(bowl,'\' + StashDirectory + '\Delete.tmp');
     ReWrite(Bowl,1);                  {Prepare a work area.}
     BlockWrite(Bowl,Header,SizeOf(Header));
     BlobCount:=0;                     {Header written, but no content yet.}
     Shead:=0;                         {Count the worms.}
     for i:=0 to StashChunks do        {Twisting through the stash.}
      for j:=0 to StashChunkLast do    {Those beyond StashIndexLimit will be zero.}
       if StashInRec[i]^[j] <> 0 then shead:=shead + 1;
     BHead:=0;                         {No new worms.}
     if not zgrab(zptr) then Croak('Burp: memory shortage!');
     ScrubStashFingers(who);           {Eeek!!}
     WriteLn('Finger, Kept,Heads.');   {A heading for the progress messages.}
     for rec:=1 to Header.StashCount do{Step through the stash records.}
      begin                            {The hash function finds its entry }
       if Trace then Write('Burp@',rec:5);    { in the StashInRec! No search!!}
       mytrace:=Trace; Trace:=false;   {Take control.}
       if not ReadZStash(who,rec) then Croak('Burp: can''t read record '+ifmt(rec))
        else if (zptr^.zSP < p) and (LiveMoves(zptr) < zptr^.zOrigMoveCount) then     {A survivor?}
         begin                         {Yes, copy across.}
          CopyStashToWork;             {A bit wasteful, but simple.}
          it:=HashedPlaceBit;          {Where might I have stashed it?}
          i1:=it shr SBSHR; i2:=it and SBAND;
          j1:=i1 and SAND;  j2:=i1 shr SSHR;
          if mytrace then              {Am I raving?}
           begin                       {Reveal the content.}
            ShowZStashPlaces;          {The moves will be carried along, I hope.}
            write(' S[',i1:5,']=',StashInRec[j1]^[j2]:5,',zSP=',zptr^.zSP:2);
           end;                        {Enough talk. Deeds follow.}
          zptr^.zRecLink:=StashInRec[j1]^[j2];{Put it at the head of the new worm.}
          if zptr^.zRecLink = 0 then BHead:=BHead + 1;
          BlobCount:=BlobCount + 1;    {Secure another segment in the output file.}
          StashInRec[j1]^[j2]:=BlobCount;    {Point the head to it.}
          StashInBit[j1]^[j2]:=StashInBit[j1]^[j2] or bitnum[i2];
          if Flabby then BlockWrite(Bowl,zptr^.zRecLink,StashRecordSize)
           else                        {Perhaps we're not wasting so much space.}
            begin                      {At some cost in compaction.}
             CramRec.sRecLink:=zptr^.zRecLink;{A full SqueezeRec is not needed.}
             BlockWrite(Bowl,CramRec,StashRecordSize);
            end;                       {So much for that.}
          if zptr^.zSP <= StashBranchLimit then StashBranch[zptr^.zSP]:=StashBranch[zptr^.zSP] + 1;
          if mytrace then Write(';N=',blobcount);
         end else if mytrace then Write(': zSP=',zptr^.zSP:2);
       if mytrace then WriteLn;        {Close off any remark.}
       if rec mod 10000 = 0 then WriteLn(rec:6,BlobCount:6,Bhead:6);
       Trace:=mytrace;                 {Revert to the global.}
       if KeyPressed then DealWith(KeyFondle);
      end;                             {And on to the next record.}
     zptr^.zChanged:=false; ZDrop;     {SP invalid here. Get rid of the hired help, with no write.}
     if blobcount mod 10000 <> 0 then WriteLn(BlobCount:5,' segments in ',Bhead,' worms.');
     WriteLn('Segments ejected ',Header.StashCount - BlobCount:5);
     WriteLn('Worms extracted  ',Shead - Bhead:5);
     Write('Average length was ',(Header.StashCount+0.0)/shead:6:3,', now ');
     if bhead <= 0 then Write('none remain.')
      else Write((blobcount+0.0)/bhead:6:3);
     WriteLn;
     Header.StashCount:=BlobCount;     {The survivors huddle.}
     DescribeTheJungle;                {Under the spreading branches...}
     i:=who;  {Pathetic! for i:=who:StashBranchLimit:2 do...}
     repeat   {But no, the step has to be one, only.}
      StashPrevBranch[i]:=StashBranch[i];
      i:=i + 2;
     until i > StashBranchLimit;
     StashEarly:=maxint;               {Refreshed scrutiny.}
     Close (StashFile); Close(Bowl);   {StashInRec and StashBranch unsaved.}
     Erase(StashFile);                 {I will surely have enough space now.}
     Rename(Bowl,StashName);           {Cast off the old, reach for the fresh.}
     Assign(StashFile,Stashname); Reset(StashFile,1);
     SaveStash(who);                   {Roll the new info to what was the copy.}
     StashAche:=false;                 {Quite.}
    end;                               {So much for Player[who].}
   TextAttr:=ta;                       {Back to healthier colours.}
   WithRetraction:=StashRetract;       {Revert to what I was doing.}
   WriteLn('Arrr. I feel better now!');{Having burped.}
   if StepWise or not continual or KeyPressed then DealWith(KeyFondle);
   if not trace then ClrScr;           {Clear the table.}
  End; {of Burp.}

 Procedure StashBloat(who: byte);      {Add a segment to some worm.}
{  The stash file grows sequentially as records are added, so disc file
buffering has a better chance. The hash number fingers an entry in array
StashInRec that gives the record number. As ever, more than one record
may have the same hash number, so the fingered record is the start of a
linked list of records, a "worm", though it is to be hoped that all worms
are short. Thus, the segment being saved may lengthen an existing worm
or start its own.}
  var i1,i2: word;
  var j1,j2: word;
  Begin
   With Player[who] do
    begin
     Header.StashCount:=Header.StashCount + 1; {Onwards!}
     i2:=zptr^.zHashIndex and SBAND;
     i1:=zptr^.zHashIndex shr SBSHR;   {Convert from the bit index.}
     j1:=i1 and SAND; j2:=i1 shr SSHR; {Really, want StashInRec[i1], but no.}
     if Trace then WriteLn('StashBloat: Who=',who,',St.Ct=',Header.StashCount,
      ',H=',zptr^.zHashIndex,',SInRec^[',i1,']=',StashInRec[j1]^[j2]);
     if SP <= StashBranchLimit then zptr^.zSP:=SP else zptr^.zSP:=StashBranchLimit;
     if StashBranch[zptr^.zSP] < StashBranchCountLimit then StashBranch[zptr^.zSP]:=StashBranch[Zptr^.zSP] + 1;
     zptr^.zRecLink:=StashInRec[j1]^[j2];   {The latest goes to the head of the linked list.}
     StashInRec[j1]^[j2]:=Header.StashCount;{Finger the new head.}
     StashInBit[j1]^[j2]:=StashInBit[j1]^[j2] or bitnum[i2];
     zptr^.zStashRec:=Header.StashCount;    {Grab a new record.}
     if not WriteZStash(who) then WriteLn('eek.');   {Thus were no params for WriteZStash.}
     if Header.StashCount mod 1000 = 0 then SaveStashHeader(who); {Thus, recoverable.}
     StashGrows:=true;                 {The index table in the file has been damaged.}
     if not StashAche and (Header.StashCount > StashFullish) then
      begin                            {Excessive occupancy.}
       StashAche:=true;                {Leading to extra work.}
       StashRetract:=WithRetraction;   {Dare not allow any deferred use }
       WithRetraction:=false;          { saved in the zash^ stack because }
      end;                             { after a Burp, much is rearranged!}
    end;
  End; {of StashBloat.}                {So left and right hand knowingness.}

 Procedure GrabAPlayPen;               {Get hold of the stash files.}
  Var Snarl: integer;
  Var i: word;
  Var dirinfo: SearchRec;
  Var aname: string;
  Var who: byte;   {The two stashes were once combined into one playpen.}
  Begin
   if Trace then WriteLn('Grabaplaypen:');
   FindFirst('\' + StashDirectory + '\*.*',anyfile,dirinfo);
   if doserror = 3 then
    begin
     WriteLn('No sign of directory ' + StashDirectory + ' on the current drive. One moment...');
     GetDir(0,aname);
     {$I-}ChDir('\'); MkDir(StashDirectory); snarl:=IOResult;
     if snarl <> 0 then Croak('Agh! IOResult ' + ifmt(snarl));{$I-}
     chdir(aname);
    end;
   StashRecordSize:=0                  {Record size stuff.}
     + SizeOf(zptr^.zRecLink)          {Needed to store a board position.}
     + SizeOf(zptr^.zSP)               {With auxiliary information.}
     + SizeOf(Zptr^.zOrigMoveCount);   {To assist in pruning and display embellishment.}
   if Flabby then StashRecordSize:=StashRecordSize + 2*NC*2 + NC*SizeOf(MoveBag)
    else StashRecordSize:=StashRecordSize + 2*NC*1 + (NC*3 - 1) div 8 + 1;
   if Trace then Write('StRcSz=',StashRecordSize,',StRcBase=',stashrecordbase);
   for i:=1 to StashBranchLimit do StashPrevBranch[i]:=0;
   for who:=1 to 2 do With Player[who] do
    begin
     StashGrows:=false;                {No stash to hand.}
     Header.StashCount:=0;             {No entries.}
     Header.VictoryCount:=0;           {No victories.}
     Header.StashNewPosition:=0;       {No new entries made,}
     Header.StashOldPosition:=0;       {No old entries there to have been found.}
     ScrubStashFingers(who);           {So clean my fingers to them.}
     Stashname:='\' + StashDirectory + '\' + 'r' + Ifmt(NR) + 'c' + Ifmt(NC)
      + PlayerSymbol[Who] + '.ppx';
     FindFirst(Stashname,0,dirinfo);   {This drivel is a right pain.}
     if Trace then
      if doserror = 18 then WriteLn(', new file needed')
       else WriteLn(', DosError=',doserror);
     Assign(StashFile,Stashname);      {This is tiresome stuff!}
     if doserror = 0 then LoadStash(who) else PrepareStash(who);
     if Trace then DealWith(KeyFondle);
   end;
  End; {of GrabAPlayPen.}

{---------------------------------The Board----------------------------------}
 Const OffsetCol = 2; OffsetRow = 2;   {One for annotation, another one for a border.}
 Procedure DrawBorder;                 {Invoke once only! (Adjusts the bounds)}
  Var r,c: byte;                       {Row and column.}
  Begin
   LookTo(TheBoard);                   {Just so.}
   ClrScr;
   GoToXY(1,1);                        {No matter what may have gone before.}
   TextBackground(Dark); Write('  ');  {Here we go.}
   TextColor(Scale);
   Board[0,0]:=NoEntry;                {Place a barrier, into which }
   Board[NR+1,0]:=NoEntry;             { pieces cannot move.}
   for c:=1 to NC do
    begin
     Write(' ',NumberCode[c]);
     Board[0,c]:=NoEntry;
     Board[NR+1,c]:=NoEntry;
    end;
   Board[0,NC+1]:=NoEntry;
   Board[NR+1,NC+1]:=NoEntry;
   GoToXY(1,2); Write('  '); TextColor(Border); for c:=1 to 2*NC+1 do Write('Ü');
   for r:=1 to NR do                   {Step down the rows.}
    begin                              {Just the two sides.}
     Board[r,0]:=NoEntry;
     Board[r,NC+1]:=noentry;
     TextColor(Scale); GoToXY(1,OffsetRow + r); Write(' ',NumberCode[r]);
     if r mod 2 = 0 then TextBackground(dark) else TextBackground(bright);
     TextColor(Border); Write('Ý');    {The left side.}
     if r mod 2 <> NC mod 2 then TextBackground(dark) else TextBackground(bright);
     GoToXY(OffsetCol + NC*2 + 1,OffsetRow+r); Write('Þ'); TextBackground(dark);
    end;                               {On to the next row.}
   GoToXY(1,OffsetRow + NR + 1); Write('  '); for c:=1 to 2*NC + 1 do Write('ß');
   with pane[TheBoard] do              {Adjust the bounds of the windowpane.}
    begin                              {Thereby saving some additions.}
     row1:=row1 + OffsetRow; col1:=col1 + OffsetCol;
     row2:=row2 - 1;         col2:=col2 - 1;
    end;                               {Every time a board square is twiddled.}
  End; {of DrawBorder.}

 Procedure PlaceSquare(a: char; mark: byte; r,c: byte);
  Begin;
   if c mod 2 = r mod 2 then TextBackground(Bright) else TextBackground(Dark);
   TextColor(mark); GoToXY(c*2,r); Write(a);
  End;

 Procedure Standardise(w: byte);       {Reverse order, for cunning reasons.}
  Var i,j: byte;                       {See usage of Movable.}
  Var stool: RowAndColumn;             {Damnit, why no Swap construct?!%$#@!}
  Begin
   with Player[w] do
    for i:=2 to NC do                  {Crank up an InsertionSort.}
     if word(piece[i]) > word(piece[i - 1]) then {REVERSE order...}
      begin
       word(stool):=word(piece[i]);
       j:=i - 1;
       repeat
        word(piece[j + 1]):=word(piece[j]);
        j:=j - 1;
       until (j <= 0) or (word(piece[j]) >= word(stool));
       word(piece[j + 1]):=word(stool);
      end;
  End; {of Standardise.}

 Procedure InitialPlaces;              {Clear for action.}
  Var r,c: byte;
  Begin
   for r:=1 to NR do                   {Idiotic drivel! Board:=Empty;!!}
    for c:=1 to NC do board[r,c]:=Empty;
   for c:=1 to NC do                   {Now place the pieces in their rows.}
    begin                              {Top and bottom.}
     Board[ 1,c]:=1;                   {Occupied by player 1.}
     Board[NR,c]:=2;                   {Occupied by player 2.}
     Player[1].Piece[c].row:= 1; Player[1].Piece[c].col:=c;
     Player[2].Piece[c].row:=NR; Player[2].Piece[c].col:=c;
     if ShowBoardMoves then            {Are we demonstrative?}
      begin                            {Yep. Show on screen.}
       PlaceSquare(PlayerSymbol[1],PlayerColour[1], 1,c);
       PlaceSquare(PlayerSymbol[2],PlayerColour[2],NR,c);
      end;
    end;
   BoardSync:=ShowBoardMoves;
   Standardise(1); {A specified order for the locations in the piece lists }
   Standardise(2); { prevents equivalent lists from appearing to be different.}
  End;

 Procedure FreshBoard;
 {Drawing a border all around means that the window size has to be a bit strange...}
  Var Flip: boolean;
  Procedure PutLine(a: char;mark: byte);
   var i: byte;
   Begin
    if flip then TextBackground(Bright) else TextBackground(Dark);
    for i:=1 to NC do
     begin
      if flip then TextBackground(Bright) else TextBackground(Dark);
      TextColor(Mark); Write(a);
      if flip then TextColor(Dark) else TextColor(Bright);
      if i < NC then Write('Þ');
      flip:=not flip;
     end;
   End; {of PutLine.}
  Var r,c: byte;
  Begin
   for r:=1 to NR do
    begin
     Flip:=r mod 2 = 1;
     GoToXY(2,r); PutLine(' ',Bright);
    end;
  End; {of FreshBoard.}

 Procedure BrandSquare(a: char; mark: byte; r,c: byte);
  var w: byte;     {Whence we came.}
  Begin;
   w:=CurrentWindow;                   {Where was I?}
   LookTo(TheBoard);                   {Just so.}
   if mark and $70 = 0 then            {No specified background colour.}
    if c mod 2 = r mod 2 then mark:=mark + Bright*16; {Thus use the board's.}
   TextAttr:=mark;                     {The style.}
   GoToXY(c*2,r);                      {The location.}
   Write(a);                           {The splot.}
   LookTo(w);                          {Others will decide on the TextAttr.}
  End; {of BrandSquare.}

 Procedure RedrawBoard; {The board display needs to be rewritten because }
  var r,c,it: byte;     { it has not been updated as the game progresses.}
  Begin                 {Although there are NR*NC squares and at most 2*NC }
   FreshBoard;          { non-blank squares, the Place array may not be in }
   for r:=1 to NR do    { agreement with the state of play because a retraction }
    for c:=1 to NC do   { might be in progress (with ShowBoardMoves set true }
     begin              { at a delicate moment).}
      it:=Board[r,c];   {The Board array is however kept correct }
      PlaceSquare(PlayerSymbol[it],PlayerColour[it],r,c);
     end;               { because it is not saved in a file, }
   BoardSync:=true;     { unlike the Place and Move lists }
  End;                  { as the board state is NR*NC whereas the place list is 2*NC only.}

 Procedure ShowMove(who,r1,c1,r2,c2,ohw:byte);
  var w: byte;
  Begin
   w:=CurrentWindow; LookTo(TheBoard);
   if not boardsync then RedrawBoard;
   PlaceSquare(PlayerSymbol[ohw],PlayerColour[ohw],r1,c1);
   PlaceSquare(PlayerSymbol[who],PlayerColour[who],r2,c2);
   LookTo(w);
  End;


 Const ScoreLines = 3;
 Const MinScoreWidth = 5;
 Var   ScoreBefore: boolean;
 Var PVtext: array[1..2] of string[12];{Retain previous victory text.}
 Procedure ShowScores;                 {Thus alter only those that differ.}
  var w: byte;
  var c,l: integer;
  var vtext: string[12];               {Text version of the VictoryCount.}
  Begin
   w:=CurrentWindow; LookTo(TheScore); {Just so.}
   if not ScoreBefore then             {First time around?}
    begin                              {Yep, a heading is needed.}
     ClrScr;                           {Just scrub my windowpane.}
     GoToXY(1,1);                      {Since screen updating is very slow, }
     TextBackground(Black);            { it is worth avoiding any detail.}
     TextColor(Cyan); Write('Score');  {The heading, once.}
     ScoreBefore:=true;                {And one scrub, per run.}
    end;                               {So much for the heading.}
   for c:=1 to 2 do With Player[c] do  {Step through the scores.}
    if Header.VictoryCount <> PreviousVCount then
     begin                             {Alas, this number has changed.}
      str(Header.VictoryCount:5,Vtext);       {The latest count.}
      l:=1; while (l <= length(vtext)) and (Vtext[l] = pvtext[c,l]) do l:=l + 1;
      GoToXY(l,c+1);                   {Its first deviating character.}
      TextColor(PlayerColour[c]);      {So, here goes.}
      Write(Copy(Vtext,l,12));         {Only the different.}
      PreviousVCount:=Header.VictoryCount;     {Remember for the next time.}
      pvtext[c]:=vtext;                {All effort to reduce screen update quantities.}
     end;                              {So much for changes.}
   LookTo(w);                          {Back to where we were.}
  End; {of ShowScores}

 Const StyleLines = 13;      {Space for the splots.}
 Const MinStyleWidth = 5;
 Procedure ShowStyle;
{  Particular vexation awaits anyone who attempts to write text for the
full length of a line without causing scrolling. The last character of a
line can be written except when it is the last line. This final cell is
not available, so if you want to draw a box outline, then you're stuck.
   Notice how the list below ends with a truncated line... The previous
lines go to the end but rather than fuss with Write vs WriteLn vexation
the subsequent line's text is positioned directly.
   Scowl.}
  Var l: integer;
  Procedure splot(Msg: string; on: boolean);
   Begin
    GoToXY(1,l);
    if on then Msg[1]:=UpCase(msg[1]);
    TextColor(LightGreen); Write(Msg[1]);
    TextColor(Green);      Write(Copy(Msg,2,Lo(WindMax)));
    l:=l + 1;
   End;
  Procedure HowGo(i: integer);
   Begin
    GoToXY(1,l);
    TextColor(PlayerColour[i]); Write(PlayerSymbol[i]);
    TextColor(Green);
    if Player[i].keyboard then Write(' you') else Write(' me.');
    l:=l + 1;
   End;
  var w: byte;
  Begin
   w:=CurrentWindow; LookTo(TheFlags); ClrScr;
   l:=1;
   GoToXY(1,l); TextColor(Cyan); Write('Style');
   l:=l + 1;
   splot('board show',ShowBoardMoves);
   splot('list moves',ListMoves);
   splot('quiet run',not ShowResult);
   splot('stepWise',StepWise);
   splot('continual',continual);
   splot('retract',WithRetraction);
   splot('witless',witless);
   splot('every board',FullRecall);
   splot('trace',trace);
   HowGo(1); HowGo(2);
   GoToXY(1,l); TextColor(cyan);
   with Pane[TheFlags] do Write(Copy('ESC quits.',1,col2 - col1 + 1));
   LookTo(w);
  End; {of ShowStyle.}

 Procedure SplotHint;
  var cc,cr: byte;
  var ta: word;
  Begin
   cr:=wherey; cc:=wherex; ta:=TextAttr;
   TextColor(Green); TextBackground(Black);
   GoToXY(1,pane[TheCommentary].row2 - 3);
   WriteLn('A poke of keys BLQSC etc. alters the style of operation,');
   WriteLn('A poke of the ESC key will stop the current run,');
   Write('PAWNPLEX ?    evokes a description.');
   TextAttr:=ta; GoToXY(cc,cr);
  End;

 Var UnknownKey: boolean;
 Procedure DealWith(Key: char);
  Begin
   UnknownKey:=true;
   if key = #0 then key:=ReadKey {No actions for the special keys. Just swallow.}
    else
     begin
      UnknownKey:=false;
      case upcase(key) of
       ESC:QuitRun:=true;
       'C':Continual:=not Continual;
       'B':ShowBoardMoves:=not ShowBoardMoves;
       'E':FullRecall:=not FullRecall;
       'L':ListMoves:=not ListMoves;
       'Q':ShowResult:=not ShowResult;
       'R':WithRetraction:=not WithRetraction;
       'S':StepWise:=not StepWise;
       'T':Trace:=not Trace;
       'W':Witless:=not witless;
       'O':Player[1].Keyboard:=not Player[1].Keyboard;
       'X':Player[2].Keyboard:=not Player[2].Keyboard;
       ' ':Specialreport;
       'P':PruneAttack;
       '?':SplotHint;
      else UnknownKey:=true; end;
      if QuitRun then begin StepWise:=false; Continual:=true; end;
      if not UnknownKey then ShowStyle;{Presumably, changed.}
      if not ShowBoardMoves then boardsync:=false;
      if not ListMoves or Trace then listsync:=false;
     end;
  End; {of DealWith.}

 Procedure AdjustEscutcheon(Unbeatable:Char); {I keep records...}
{  Various records are kept of the results, once a conclusion as to which
player has an unbeatable set of moves. Three files are maintained. To enable
random access for updating, their format is fixed, so be careful with them.}
  var NewResult: boolean;
  Procedure Poke(Which,blob,subtitle:string); {Update a file with something.}
   Var NoteBook: file;
   var i,j,lslot,zap: integer;
   Var snarl: integer;
   var wad,eol,curse: string;
   Begin
    Lslot:=length(blob);     {Length of a slot.}
    Assign(NoteBook,'\' + StashDirectory + '\' + Which + '.txt');
    {$I-}Reset(NoteBook,1); Snarl:=IOResult;{$I+}
    If Snarl <> 0 then       {So, what happened.}
     begin                   {No existing file, so prepare one.}
      Rewrite(NoteBook,1);
      eol:=cr + lf;          {For DOS, anyway.}
      wad:=copy('            ',1,lslot - 1) + '?';
      curse:='|';            {No concurrence on vert. bar character codes, e.g. 179.}
      for i:=RowLimit downto 2 do {Can't have one row for a game.}
       begin
        BlockWrite(NoteBook,NumberCode[i],1);
        BlockWrite(NoteBook,curse[1],1);
        for j:=1 to ColumnLimit do BlockWrite(Notebook,wad[1],lslot);
        BlockWrite(NoteBook,Eol[1],length(eol));
       end;
      Curse:='  '; BlockWrite(Notebook,curse[1],2);
      For j:=1 to ColumnLimit do
       begin
        BlockWrite(notebook,wad[1],lslot - 1);
        BlockWrite(notebook,NumberCode[j],1);
       end;
      BlockWrite(NoteBook,eol[1],length(eol));   {What a load of annoyance!}
      BlockWrite(NoteBook,eol[1],length(eol));
      BlockWrite(NoteBook,subtitle[1],length(subtitle));
      BlockWrite(NoteBook,eol[1],length(eol));
     end;                    {Righto, a file is ready.}
    zap:=(RowLimit - NR)*(2 + ColumnLimit*lslot + 2) + 2 + (NC - 1)*lslot;
    if Trace then WriteLn('    require rec ',zap);
    Seek(NoteBook,zap); BlockRead(NoteBook,Wad[1],Lslot);
    if lslot = 1 then                  {Is this the victory table?}
     begin                             {Yes.}
      NewResult:=wad[lslot] = '?';     {Previously, no decision was recorded?}
      if not NewResult then            {No, a decision had been filed.}
       if wad[lslot] <> Unbeatable then{Concurrence?}
        begin                          {No!?!!???}
         WriteLn('Revisionism! ',Unbeatable,' replaces ',wad[lslot]);
         NewResult:=true;              {It surely is.}
        end;                           {And it shouldn't happen!}
     end;                              {So much for the certain-win table.}
    if NewResult then begin Seek(NoteBook,zap); BlockWrite(NoteBook,blob[1],lslot); end;
    Close(NoteBook);                   {Done with this one.}
   End; {of Poke}
  Var text: string;
  Var i: integer;
  Const cw = 10;         {Width for the counts.}
  Begin
   if Trace then WriteLn('Escutcheon: Winner=',Unbeatable,',NR=',NR,',NC=',NC);
   NewResult:=false;                   {Perhaps not.}
   Poke('WinsWho',Unbeatable,'Unbeatable');{Consider the blot in my escutcheon.}
   if NewResult then                   {Any subsequent games are supererogatory.}
    begin                              {So only record the counts needed for decision.}
     for i:=1 to 2 do with Player[i] do{Other move selection styles would produce different counts.}
      begin                            {But these will be of interest.}
       Str(Header.VictoryCount:cw,text);{Damn this! a) not a function, b) non-parameter syntax!!}
       Poke('Wins' + PlayerSymbol[i],text,'Victories for '+playersymbol[i]);
      end;                             {Only two iterations, but enough code.}
     Str(Player[1].Header.VictoryCount + Player[2].Header.VictoryCount:cw,text);
     Poke('Games',text,'Game counts.');{Might as well.}
    end;                               {So much for the first flush of victory.}
  End; {of AdjustEscutcheon.}

{------------------------------The game's afoot!-----------------------------}
 const TrailNumWidth = 6;    {123456:NnRC/RC}
 const TrailWidth = TrailNumWidth + 1+2+2+1+2; {See DescribeOptions.}
 const FreshBlood = LightRed;{Quite.}
 const DriedBlood = Red;     {A memento of earlier conflict.}

 Procedure PlayAGame;        {Ah, but the details...}
  var who,which: byte;       {Who is playing (= 1 or 2), Which piece.}
  var way,how: shortint;     {What Way(+-1), and How(-1,0,+1).}
  var r,c,r2,c2: byte;       {From row r, column c to r2,c2.}
  Var Result: shortint;      {-1,0,+1: Lose, undecided, win.}
  Var Movable: byte;         {Fingers a movable piece.}
  Var Lunge: byte;           {If non-zero, fingers a finisher.}
  Var MoveCount: byte;       {3*ColumnLimit is not too much.}

  Procedure AllPossibleMoves;
{  Because the position lists are sorted in reverse order, when the possible
moves are considered and Movable left fingering the last piece that was found
movable, the first player will advance in whole ranks whereas the second
player will repeatedly advance its first mover, thus two diametrically
opposite strategies will contend. The non-witless choice of move means
that if a piece is one row from the final row (so by advancing one row
it would win), then variable Lunge will finger one such piece.
  It may appear that selecting a most advanced piece for further advance
would be likely to force a conclusion earlier, and test runs with small
boards showed no or slight advantage but then a 5x5 was very much the
other way. Game counts for these two styles were:          max lunge
                  m  l         m   l         m    l  5x5 73599 18026
        m  l 4x3 60 70   4x4 458 232  4x5 4349 4888
   3x4 10 10

  Likewise, a trial in which when a game is restarted, the piece selected
after a retreat is in the most advanced row resulted in 18222 games being
required for the 5x5 case, so again a possible ploy does not prove to be
a startling improvement.
  Selection of the first encountered most advanced piece meant that both
players advanced their rightmost pawn; other sorting and scanning schemes
result in other patterns and without exhaustive testing, I see no reason
to prefer any over the rank vs. file pattern, so it will suffice to Lunge
only from the penultimate row, and allow the two strategies to fight it out.
  One could engage in a deeper analysis of the possible moves, but the whole
point of this programme is to demonstrate the (slow) development of good
play without built-in ploys. Further, such analysis sooner or later
involves a game tree of its own, secretly used within the black-box move
selector, and another objective is to have it all out in the open.}

   Var wmove: MoveBag;                 {The current piece's move collection.}
   Var i,r,c,v,ohw: byte;
   Begin
    if Trace then Write('AllPossMov: who=',who,',Witless=',witless);
    MoveCount:=0;                      {No moves discovered.}
    Movable:=0;                        {No piece is known as movable.}
    Lunge:=0;                          {Nor is any immediate win known.}
    if who = 1 then v:=NR else v:=1;   {Victory row.}
    ohW:=3 - who;                      {The enemy. ohW = 2 or 1}
    with Player[who] do                {Quite.}
     for i:=1 to NC do                 {Consider every piece.}
      begin                            {One by one.}
       AMove[i]:=Empty;                {No moves identified as yet.}
       if word(piece[i]) <> 0 then     {Have we a piece to play with?}
        begin                          {There may have been captures.}
         wmove:=empty;                 {A fresh field of opportunity.}
         c:=piece[i].col;              {We may go to the left or right.}
         r:=piece[i].row + way;        {But every move advances one row.}
         if board[r,c] = empty then   wmove:=MoveMask[ahead];
         if board[r,c - 1] = ohW then wmove:=wmove or MoveMask[left];
         if board[r,c + 1] = ohW then wmove:=wmove or MoveMask[right];
         AMove[i]:=wmove;              {Splot the actual options.}
         if wmove <> empty then        {Is there movement?}
          begin                        {Yes.}
           MoveCount:=MoveCount + MoveFan[wmove];
           Movable:=i;                 {Just so.}
           if (r = v) and not witless then lunge:=i;
          end;                         {So much for advances.}
        end;                           {So much for extant pieces.}
      end;                             {On to the next.}
    if Trace then WriteLn(',Movable=',movable,',Lunge=',lunge,',OMC=',movecount);
    if lunge <> 0 then movable:=lunge; {Leap forward to victory.}
   End; {of AllPossibleMoves.}

  Procedure PickSomeStashedMove;       {NB! Does *not* use the Move array.}
   var i,l,v: byte;                    {Nor the Player.Place array.}
   label 6;                            {I would prefer text labels.}
   Begin                               {This schizophrenia allows KeyboardChoice }
    Lunge:=0;                          { to accept requests for losing moves.}
    Movable:=NC; while (Movable > 0) and (zptr^.zstuff[zMoveOffset + Movable] = Empty) do Movable:=Movable - 1;
    if not Witless and (Movable > 1) then   {If only one move, then it is the one.}
     begin                             {We can move at least one piece.}
      i:=Movable;                      {Maybe others too.}
      l:=(who - 1)*NC*2;               {And don't want to be stupid about it.}
      if who = 1 then v:=NR-1 else v:=1+1; {Victory in reach row.}
      repeat                           {So see if an immediate win is possible.}
       if zptr^.zstuff[zMoveOffset + i] <> empty then
        if v = zptr^.zstuff[i*2 + l] then begin lunge:=i; goto 6; end;
       i:=i - 1;                       {Step back through the list.}
      until i <= 0;                    {On to another possible.}
     end;                              {So much for minimal intelligence.}
  6:if Trace then WriteLn('PickSomeMv: Witless=',witless,',Movable=',movable,',lunge=',lunge,',way=',way);
    if lunge > 0 then Movable:=Lunge;  {Only if not Witless.}
   End; {of PickSomeStashedMove.}

  Procedure SniffTheState;
   Procedure FindAStash;
{  Board positions are saved in a disc file so that subsequent runs may
have the benefit of whatever was learnt and not repeat the same losing
moves. Since further, a board position may be reached by more than one
sequence of moves (moves A,B,C for player O and U,V,W for X result in
the same position whether they are played as AUBVCW or BUCVAW, or etc.
provided that the pieces do not interact), it would be worthwhile to
play out the consequences of that position once only.
   But there may be many board positions... So, a hash function with an
index table allows the appropriate record to be found in one probe and
also means that the file can grow sequentially, making life easier for
the disc buffering arrangements.
   So long as variable FullRecall is false, positions will only be saved
in the disc file if something has been learnt, which means that one of
its moves has been found bad and suppressed, never to be attempted again,
and the record is written only when its stack level is finished with,
which will often mean that all its moves have been found wanting, if the
Retract option is activated so that each game is not restarted from scratch.
   Because most probes will be for positions not previously encountered,
a two-level hash index is used. The primary hash value indexes an array
of bits, which merely states if there is an entry present for that hash;
if not, no further probing is needed. If it is present, then the entries
fingered by the second, 16-bit index array element need to be compared
for a match (more than one position can have the same hash value).
In other words, if bit StashInBit[h] is 'on', then the linked list of
records must be searched, starting with the one fingered by StashInRec[h].
There are sixteen bits in StashInBit, corresponding to the one (16-bit word)
StashInRec[h], and as the total number of records does not exceed 64k,
there will be only one bit on (on average, when the stash approaches fullness)
corresponding to a linked list of only one element, on average. This means
that 'absent' will be determined without disc accesses (usually), while
if present, only one disc access will be needed, most times.
   Well, that was the situation when one hash file was used for both players.
Now, with each having its own stash file, the hash table requirement is thus
doubled, and there is a memory constraint. Sigh. So the bit array is now of
8-bit data instead of 16-bit data, requiring less room. Sigh.
   A possible extension would be to arrange that only the first record in
a linked-list need be amongst the first 64k (that can be named with 16 bits),
second and subsequent records could be further along, with larger variables
used for the links in the disc records. This would lead to a rather messy
file structure, though, and would slowly eat up the advantage of sparse
presence on the bit array. One could extend the bit array, perhaps foregoing
the StashInRec array and its advantage of sequential file growth.
   This picture is confused by addressing problems in turbo pascal whereby
no array may occupy more than 64k bytes of store (actually about 65,520,
so if you want a power of two, you're stuck with a size of 32,768),
so a 2-D array is used, with an array of pointers for the first dimension. Ugh.}
    Var HashIndex: LongInt;
    Var i1,i2: word;
    var j1,j2: word;
    Var i,rc: integer;
    var ta: byte;
    Begin {of FindAStash.}
     if Trace then Write('FindaStash:');
     HashIndex:=HashedPlaceBit;        {Somewhere.}
     zptr^.zHashIndex:=hashindex;      {Save for StashBloat.}
     i2:=hashindex and SBAND;          {Thus finger the bit within the word.}
     i1:=hashindex shr SBSHR;          {Bits packed into a StashInRec datum.}
     j1:=i1 and SAND;                  {I want StashInRec[i1], but can't have that.}
     j2:=i1 shr SSHR;
     With Player[who] do               {Selecting the player's stash,}
      begin                            { develop knowledge of this board position.}
       zptr^.zStashRec:=StashInRec[j1]^[j2];
       if Trace then
        begin
         ta:=TextAttr;                 {We may want to boast.}
         Write(',Bit(',hashindex,')=');
         if (StashInBit[j1]^[j2] and bitnum[i2]) = 0 then Write('-')
          else begin Textcolor(LightGreen); Write('+'); end;
         WriteLn(',StashIn(',i1,')Rec=',zptr^.zStashRec);
         TextAttr:=ta;
        end;
       if (StashInBit[j1]^[j2] and bitnum[i2]) = 0 then zptr^.zStashRec:=0 {Definitely unknown.}
        else                           {There is a position with this hash.}
         while (zptr^.zStashRec > 0)   {There is a linked-list of such.}
          and ReadZStash(who,zptr^.zStashRec)  {Grab the currently-fingered one.}
          and not StashMatchesPlace            {And if it isn't the right one, }
         do zptr^.zStashRec:=zptr^.zRecLink;   { advance to the next one.}
       if zptr^.zStashRec = 0 then     {This position is unknown.}
        begin                          {So prepare the details.}
         Header.StashNewPosition:=Header.StashNewPosition + 1; {Variety the spice of life.}
         AllPossibleMoves;             {From the Places, list the Moves.}
         CopyWorkToStash;              {To the I/O area.}
         zptr^.zOrigMoveCount:=MoveCount;  {Retain the original value for display embellishment.}
         if FullRecall then StashBloat(who);  {Even though we may never learn anything here.}
        end                            {The search always succeeds!}
        else                           {But if something really was known, }
         begin                         { we can take advantage of it.}
          Header.StashOldPosition:=Header.StashOldPosition + 1; {Hopefully, some moves have been rejected.}
          if SP < zptr^.zSP then       {When this position was earlier encountered, }
           begin                       { it was higher up in the game tree.}
            StashBranch[zptr^.zSP]:=StashBranch[zptr^.zSP] - 1;
            StashBranch[SP]:=StashBranch[SP] + 1;
            zptr^.zSP:=SP;             {Promote its encounter level.}
            zptr^.zChanged:=true;      {In case of later pruning.}
           end;                        {Now consider the moves.}
          CopyStashToMoves;            {Load my scratchpad from the stash.}
          PickSomeStashedMove;         {And see what lies on the slab.}
          if Keyboard then             {Keyboard selection of moves?}
           if (movable > 0) or witless then AllPossibleMoves   {Perhaps half ignore findings!}
         end;                          {So much for known encounters.}
      end; {of Player[who]}            {Known or until now unknown, }
    End; {of FindAStash.}              { we always find a stash.}
   Begin {SniffTheState}   {Who = 1 or 2}
    way:=3 - 2*who;        {way =+1 or-1}
    if Trace then
     begin
      Write('SniffState: ');
      ShowWorkPlaces;
      WriteLn(' who=',who,',way=',way);
     end;
    FindAStash;                        {A stash is always discovered to be found.}
    if Trace then WriteLn('SniffState: Movable=',movable,',StashRec=',zptr^.zStashRec);
   End; {of SniffTheState.}

  Procedure DescribeStore(it: word);   {First part of a move description.}
   Begin                               {Identifies storage.}
    if it <> 0 then Write(it:TrailNumWidth,':') {A stash record number.}
     else Write(SP:trailnumwidth,'#'); {Working memory only.}
   End;                                {TrailNumWidth + 1 characters.}
  Procedure DescribeMove; {A start row and column, a move symbol, the arrival row and column.}
   Begin
    TextColor(PlayerColour[who]);      {Back to the player's colour.}
    Write(NumberCode[r],NumberCode[c]);{Start position.}
    if how <> 0 then TextColor(FreshBlood); {A kill?}
    Write(MoveMark[way*how]);          {Move symbol.}
    TextColor(PlayerColour[who]);      {Back to the player's colour.}
    Write(NumberCode[r2],NumberCode[c2]);
   End; {of DescribeMove}              {2 + 1 + 2 characters.}
  Procedure DescribeOptions;           {Summary form only.}
   var hit: word;                      {Counts of possible moves, surviving moves.}
   Begin
    TextColor(Green);                  {The go counts should be Green for Go...}
    if zptr^.zOrigMoveCount > ColumnIdLimit then Write('!')
     else Write(NumberCode[zptr^.zOrigMoveCount]);
    hit:=LiveMoves(zptr);              {So, how many possibilities lurk unrejected?}
    if hit = zptr^.zOrigMoveCount then Write(' ')
     else if hit > ColumnIdLimit then Write('!')
      else Write(NumberCode[hit]);     {Cram into one column.}
    DescribeMove;                      {The move of the moment.}
   End; {of DescribeOptions.}          {2 characters for the count encodements.}

  Procedure RelistGameStack;           {The screen trail may have been damaged.}
   var rway,rhow: shortint;            {Those who use global variables }
   var rr,rc,rr2,rc2: byte;            { rather than passing parameters }
   var rwho,rwhich: byte;              { in order to save effort during normal operation}
   var rp: integer;                    { must pay the price in special cases.}
   var rptr: zpointer;                 {By saving and restoring }
   var i,l,hit: integer;               { the state variables.}
   var ta: byte;                       {Which can be messy.}
   Begin                               {The Player and Move arrays are for the top of stack.}
    ta:=TextAttr;                      {Whatever.}
    TextBackground(Black); ClrScr;     {Simplify with extreme prejudice.}
    rp:=SP; rptr:=zptr;                {Take a copy of the current situation.}
    rwho:=who; rwhich:=which;          {So I can fiddle things for DescribeOptions.}
    rway:=way; rhow:=how;              {It changes as I clamber down the stack.}
    zptr:=zptr^.prev;                  {"While (zptr:=zptr^.prev) <> nil do"!}
    While zptr <> nil do               {But no, we must kiddytalk.}
     begin                             {Righto, we're at some level.}
      SP:=SP - 1; way:=-way; who:=3 - who;   {They take turns.}
      l:=(SP - 1) div TrailColumns;    {What line?}
      i:=(SP - 1) mod TrailColumns;    {Starting column.}
      GoToXY(i*TrailWidth + 1,l + 1);  {Position the pen.}
      TextColor(PlayerColour[who]);    {Prepare the pigment.}
      how:=zptr^.zMoveMade;            {Now consider the move that was made.}
      which:=zptr^.zPieceMoved;        {And the mover.}
      l:=2*NC*(who - 1) + (which - 1)*2;  {Beware byteswap horror!}
      r:=zptr^.zstuff[l + 2]; c:=zptr^.zstuff[l + 1];
      r2:=r + way;           c2:=c + how;
      DescribeStore(zptr^.zStashRec);  {Show where the info comes from.}
      DescribeOptions;                 {Show the info's summary.}
      zptr:=zptr^.prev;                {A plague on this two-step test!}
     end;                              {Perhaps another level remains.}
    SP:=rp; zptr:=rptr;                {Righto, revert to what was }
    who:=rwho; which:=rwhich;          { the current situation.}
    way:=rway; how:=rhow;              { before clambering began.}
    TextAttr:=ta;                      {Revert to whatever.}
    ListSync:=true;                    {But for how much longer?}
   End; {of RelistGameStack.}          {Non-retraction re-lists moves the hard way.}

  Procedure TrimStackDisplay;          {Chops abandoned output from DescribeOptions.}
   var i,l: integer;                   {As when retiring moves.}
   Begin                               {After a game is over.}
    l:=(SP - 1) div TrailColumns + 1;  {What line ought we be on?}
    TextBackground(Black);             {Prevent confusion.}
    for i:=wherey downto l + 1 do      {Scrub back to it.}
     begin                             {Clobbering trace output, }
      GoToXY(1,i); ClrEol;             { or win/loss bumf, }
     end;                              { move retraction, whatever.}
    if not listsync then RelistGameStack; {The front may be messed about too.}
    i:=(SP - 1) mod TrailColumns;      {Possible partial line scrubs.}
    GoToXY(i*TrailWidth + 1,l); ClrEol;{Splot.}
   End;

  Procedure MovePlayer;                {Adjust my various tables.}
   var i,ohw: byte;
   var hit: word;
   Begin
    if Trace then WriteLn('MovePlayer: who=',who,',way=',way,',which=',which,',how=',how);
    Zptr^.zPieceMoved:=which;          {ReEducate will want to know this.}
    Zptr^.zMoveMade:=how;              {In order to cast out losing moves.}
    r:=Player[who].Piece[which].row; c:=Player[who].Piece[which].col;
    r2:=r + way; c2:=c + how;          {The destination.}
    if mirror then                     {Symmetry still unsplit?}
     if (c <> MirrorPlane) or (c2 <> MirrorPlane) then
      begin                            {A piece has deviated.}
       mirror:=false;                  {So the mirror has broken.}
       MirrorLevel:=SP;                {This level was the last with itself as a reflection.}
      end;                             {And thus, double learning.}
    if Trace then WriteLn('MovePlayer: r=',r,',c=',c,',r2=',r2,',c2=',c2,',Mirrored=',mirror);
    if ShowBoardMoves then ShowMove(who,r,c,r2,c2,Empty);
    Board[r2,c2]:=who;                 {I arrive!}
    Board[r,c]:=Empty;                 {I left!}
    if ListMoves then                  {Talkative?}
     begin                             {Yeah, prepare a remark.}
      DescribeOptions;                 {Cramped gibberish.}
      if SP mod TrailColumns = 0 then  {A new line needed?}
       if wherex > 1 then WriteLn;     {Damn this flow-on!!!}
     end else ListSync:=false;         {So much for descriptions.}
    if ((way > 0) and (r2 = NR))       {What of the result?}
     or ((way < 0) and (r2 = 1)) then  {Are we on a final row?}
     begin                             {Yes!}
      result:=+1;                      {So we've won.}
      if ListMoves then                {Are we talkative?}
       if SP mod TrailColumns <> 0 then WriteLn;
     end;                              {So much for surprises.}
    Player[who].Piece[which].row:=r2;  {Adjust my moved piece's position.}
    Player[who].Piece[which].col:=c2;  {In my list of positions.}
    hit:=Word(Player[who].Piece[which]);
    Standardise(who);                  {Canonical order prevents duplicates.}
    if how <> 0 then                   {If a capture was effected, }
     begin                             { the other fellow has suffered.}
      ohw:=3 - who;                    {Whoever you is, I is the other.}
      for i:=1 to NC do                {Find the victim.}
       if Word(Player[ohw].Piece[i]) = hit then
        begin                          {This one!}
         Word(Player[ohw].Piece[i]):=empty;
         Standardise(ohw);             {Prevent multiple descriptions of the same board.}
         EXIT;                         {Cut and run.}
        end;                           {So much for a victim.}
     end;                              {Otherwise, search on.}
   End; {of MovePlayer.}

  Procedure NameThatMove;              {It is about to be remarked upon.}
   Begin                               {This is not part of the game stack display.}
    DescribeStore(zptr^.zStashRec);    {But when a game has finished.}
    Write(' ');                        {So, no move count stuff.}
    DescribeMove;                      {As is provided via DescribeOptions.}
   End;

  Procedure StepBack;                  {Retreats may be in order.}
   var l: integer;
   Begin
    Zdrop;                             {Don't bother updating Place and Move...}
    way:=-way;                         {The other player goes the other way.}
    who:=3 - who;                      {Switch player identity.}
    TextColor(PlayerColour[who]);      {Convert to his colour.}
    if Trace then
     begin
      Write('SteppdBack: ');
      ShowZStash;
      WriteLn(' SP=',SP:2,',who=',who,',way=',way);
     end;
    if SP <= 0 then EXIT;              {Whoops!}
    which:=Zptr^.zPieceMoved;          {The piece that moved.}
    how:=Zptr^.zMoveMade;              {And how it moved.}
    l:=(who - 1)*NC*2 + (which - 1)*2; {Why must *I* do the indexing!}
    r:=zptr^.zstuff[l + 2]; c:=zptr^.zstuff[l + 1];    {Where from, byteswapped.}
    r2:=r + way; c2:=c + how;          {And of the destination.}
   End; {of StepBack.}
  Procedure evoMAekaM(described: boolean; shade: byte);
{  The Board array is kept in sync but Place and Move are ignored during retreat
because their states can be recovered from the stored state at the end of the retreat}
   var socket: byte;
   Begin
    TextColor(PlayerColour[who]);
    if Trace then Write('evoMAekaM:  SP=',SP:2,',which=',which,',d.=',described);
    if SP <= 0 then begin if Trace then WriteLn; EXIT; end;
    if Trace and (which <> 0) then Write(',r=',r,',c=',c,',r2=',r2,',c2=',c2);
    if which > 0 then
     begin
      if how = 0 then socket:=0 else socket:=3-who;  {An empty socket?}
      if Trace then WriteLn(',how=',how,',s.=',socket);
      if ShowBoardMoves and StepWise then BrandSquare(MoveMark[way*how],shade,r,c);
      if StepWise then DealWith(KeyFondle);
      if ShowBoardMoves then ShowMove(who,r2,c2,r,c,Socket);
      board[r2,c2]:=socket;            {What used to be there.}
      board[r,c]:=who;                 {The place departed from is re-occupied.}
      if ListMoves or Trace then
       begin
        if not described then NameThatMove;
        WriteLn(' retracted.');
       end;
     end else if Trace then WriteLn;
   End; {of evoMAekaM.}

  Procedure ChooseMove;                {Me or you?}
   Procedure KeyboardChoice;           {DealWith may change PlayerKeyboard[who]...}
    Procedure KeyboardHint;            {There may be confusion.}
     Var tx,ty,i,j: byte;              {Or forgetfulness.}
     Begin                             {So try provocation.}
      tx:=wherex;ty:=wherey;           {Where on the screen.}
      TextBackground(black); WriteLn; TextBackground(green);
      WriteLn('To step amongst your possible moves');
      WriteLn('press the left or right arrow keys.');
      WriteLn('Press enter to have the move made, ');
      WriteLn('or press the BackSpace to retreat. ');
      Write  ('A few other keys change the style. ');
      while not KeyPressed do delay(60); {Damnit, this wastes CPU time!}
      TextBackground(Black);           {We have a key at last.}
      j:=wherey;                       {So scrub the hint.}
      for i:=1 to 5 do begin GoToXY(1,j); ClrEol; j:=j - 1; end;
      GoToXY(tx,ty);                   {Where we started.}
      UnknownKey:=false;               {It has been remarked upon, if it was.}
     End; {of KeyboardHint.}
    Var r2: byte;                      {Destination row.}
    Procedure MarkMoves;               {Move How is the current choice.}
     var l: shortint;                  {Steps amongst the Piece's moves.}
     var zot: char;                    {The mark that will be made.}
     var hit,c2: byte;                 {Style and placement.}
     Begin
      r:=Player[who].Piece[which].row; {The mover's starting row, }
      c:=Player[who].Piece[which].col; { and column.}
      BrandSquare(Playersymbol[who],Green,r,c); {Departing. ("Blink" just blurs now.)}
      r2:=r + way;                     {The row it is heading for.}
      for l:=left to right do          {Scan its possible moves.}
       if (AMove[which] and MoveMask[l]) <> 0 then
        begin                          {It can make move l.}
         c2:=c + l;                    {The destination column.}
         zot:=PlayerSymbol[Board[r2,c2]];    {What is at the destination?}
         if zot = ' ' then zot:='+';   {Perhaps an empty square.}
         if (zptr^.zstuff[zMoveOffset + which] and MoveMask[l]) <> 0 then hit:=FreshBlood
          else hit:=DriedBlood;        {From earlier battles.}
         if how = l then hit:=hit + green*16; {Is this the current choice of How?}
         BrandSquare(zot,hit,r2,c2);   {Perhaps blood will be shed.}
        end;                           {On to the next potential move.}
    End;  {of MarkMoves.}
   Procedure UnMarkMoves;         {Undecorate the board.}
    var l: shortint;              {Steps through the moves.}
    Begin
     BrandSquare(Playersymbol[who],PlayerColour[who],r,c); {Back to proper,}
     for l:=left to right do      {Repair the destinations also.}
      if (AMove[which] and MoveMask[l]) <> 0 then
       BrandSquare(PlayerSymbol[Board[r2,c + l]],PlayerColour[Board[r2,c + l]],r2,c + l);
    End; {of UnMarkMoves.}
   Procedure Hop(skip: shortint); {Jump from the currently fingered move.}
    Begin
     UnMarkMoves;                 {Abandon the unwanted.}
     Repeat                       {Chase after some move.}
      how:=how + skip;            {Go somewhere.}
      if abs(how) > 1 then        {We've run out of moves for this piece.}
      begin                       {So, advance to the next piece, in my peculiar ordering.}
       repeat                     {Except that it may not have any moves.}
        which:=which - skip;      {My piece list is backwardishly ordered.}
        if which < 1 then which:=NC else if which > NC then which:=1; {Grr.}
       until Amove[which] <> Empty;{Possibly wrapping around to the one only extant.}
       how:=-skip;                {Thus skip right through this move fan.}
      end;                        {So much for a new piece.}
     until Amove[which] and MoveMask[how] <> 0;
     MarkMoves;                   {Possibly the same piece again.}
    End; {of Hop.}
   Var cx,cy: byte;               {Screen state annoyances.}
   Procedure Retire;              {The keyboard thumper doesn't like the prospects.}
    Begin                         {So go back to the previous move for that player.}
     if SP <= Who then exit;      {Can't retire from a player's first move.}
     UnmarkMoves;                 {Discard attention to a move.}
     StepBack;                    {Abandon where I was about to move from.}
     evoMAekaM(false,Green);      {The other player acted in good faith.}
     StepBack;                    {To this player's previous choice.}
     evoMAekaM(True,Yellow);      {Retire from that move in cowardice.}
     CopyStashToWork;             {Reload the state, ready to choose a move.}
     AllPossibleMoves;            {Consider it afresh.}
     MarkMoves;                   {Indicate a possible move, as before.}
     if ListMoves then            {Have we been messing on the screen?}
      begin                       {Yes. prepare to unmess a little.}
       if not Trace then TrimStackDisplay; {With trace, keep the mess.}
       DescribeStore(zptr^.zStashRec);
      end;                        {Righto, ready to ask for the move choice.}
     cx:=wherex; cy:=wherey;      {The new write point.}
     TextBackground(Red);         {Continue to echo KeyboardChoice's start up.}
     Write('Your retry');         {But with a different text.}
    End; {of Retire}              {So much for trepidation.}
    Var ch: char;                      {Keyboard action.}
    Var ItIsChosen: boolean;           {Our hero decides...}
    Var ticker: integer;               {Or dawdles. A countdown to a hint.}
    Begin {of KeyboardChoice.}         {Prepare for a key fondle.}
     AllPossibleMoves;                 {Irrespective of any hard-won earlier knowledge, in zstash.}
     if MoveCount = 1 then EXIT;       {Hobson's choice?}
     cx:=wherex; cy:=wherey;           {Be ready to unwrite.}
     TextBackground(red); Write('Your move');
     MarkMoves;                        {Currently, Piece[which] moves How.}
     UnknownKey:=false;                {Clarify the state.}
     ItIsChosen:=false;                {Just so.}
     repeat                            {Thump until a decision is made.}
      if UnknownKey then KeyboardHint; {There may be confusion lying around...}
      ticker:=666;                     {Oh for proper event handling...}
      while not KeyPressed do          {If WaitFor(KeyPressed,60) then...}
       begin                           {Instead, this drivel.}
        ticker:=ticker - 1;            {Count down.}
        if ticker <= 0 then KeyboardHint
         else Delay(60);               {Waste cpu time, damnit!}
       end;                            {Perhaps a key has been pressed now.}
      Ch:=ReadKey;                     {So, what key has been pressed?}
      Case(ch) of                      {Taste and see.}
       #0:Case(ReadKey) of             {Accursed double-bangers.}
    {Left}#75:Hop(-1);                 {The current move is disliked.}
   {Right}#77:Hop(+1);                 {So step to another.}
          else KeyboardHint end;       {Ignoring all other double keystroke codes.}
       CR:ItIsChosen:=true;            {Aha! A decision!}
       BS:Retire;                      {Oho! An undecision!}
      else DealWith(ch); end;          {Perhaps a state change, by the way.}
     until ItIsChosen or not Player[who].keyboard or QuitRun;
     UnMarkMoves;                      {Clear up the mess.}
     Move(zptr^.zstuff[zMoveOffset + 1],AMove[1],NC); {Revert to the stashed move set.}
     GoToXY(cx,cy); TextBackground(Black); ClrEol; {Scrub the invitation.}
    End; {of KeyboardChoice.}
   Begin {of ChooseMove.}              {One or more moves are available.}
    If Trace then WriteLn('ChooseMove: who=',who,',way=',way,
     ',Move[',which,']=',AMove[which],',Lunge=',lunge,',Kbd=',Player[who].Keyboard);
    if AMove[which] and MoveMask[left] <> 0 then how:=left {My choice is without }
     else if AMove[which] and MoveMask[right] <> 0 then how:=right { the slightest attempt }
      else if AMove[which] and MoveMask[ahead] <> 0 then how:=ahead; { at tactics, besides Lunge.}
    if Player[who].Keyboard and (lunge = 0) then KeyboardChoice; {Await a choice.}
    MovePlayer;                        {At last: Player[who], Piece[Which], Way, How.}
   End; {of ChooseMove.}

  Procedure MakeAMove;                 {A necessary detail.}
   Procedure Epitaph(how: word; what: string);
    var ta: word;
    Begin
     ta:=TextAttr; {Will be reverting to this.}
     TextAttr:=how;{Style of the epitaph.}
     Write(what);  {Inscribe it.}
     TextAttr:=ta; {Thus the text background is black, for wiping to the end of the line.}
     WriteLn;      {For subsequent NewLine actions too.}
    End; {of Epitaph.}
   Begin {of MakeAMove}
    TextColor(PlayerColour[who]);      {The current player.}
    if Trace then WriteLn('Makeamove:  SP=',SP:2,',who=',who,',way=',way);
    SP:=SP + 1;                        {Here we go again.}
    if not ZGrab(zptr) then Croak('MakeAMove: Out of memory!');
    Zptr^.zChanged:=false;             {No re-education as yet.}
    SniffTheState;                     {So, what's the situation?}
    which:=Movable;                    {At least one piece that can.}
    if ListMoves then DescribeStore(zptr^.zStashRec);
    if which > 0 then ChooseMove       {At least one move available, but not How.}
     else                              {But if not, the game is over.}
      begin                            {The current player loses.}
       if ListMoves then               {Perhaps a funeral oration.}
        if zptr^.zOrigMoveCount <> 0 then Epitaph(DriedBlood*16+FreshBlood,' Empty!')
         else if zptr^.zstuff[(who - 1)*NC*2 + 1] <> 0 then Epitaph(Cyan*16+FreshBlood,' Block!')
          else Epitaph(Blue*16+FreshBlood,' Dead!');
       result:=-1;                     {Possible moves may have been quashed }
      end;                             { because they led to a loss.}
   End; {of MakeAMove.}

  Procedure ReEducate;       {This is the heart of the method.}
  { Result = +1 means that the current player made a winning move.
    Bad move--> The previous move led to a position that allowed this.
    Result = -1 means that the current player could not move and so lost.
                The previous move was therefore a winning move.
    Bad move--> The move before it led to a position that allowed this.}
   var i,l: integer;
   const loser = LightBlue; winner = LightRed;
   Procedure StrangleThatLosingMove;   {I won't do it again.}
    Begin
     if Trace then WriteLn('Strangle:   how=',how,',which=',which,',r=',r,',c=',c);
     l:=zMoveOffset + which;           {Locate the MoveBag.}
     if ListMoves then                 {Jabbering as we play?}
      begin                            {Yeah.}
       NameThatMove;                   {The talking point of the moment.}
       if zptr^.zstuff[l] and MoveMask[how] = 0 then Write(' still bad.')
        else Write(' quashed...');     {A retraction impends.}
       If Trace then WriteLn;          {But there will be intervening bumf.}
      end;                             {Enough talk. Now for the action!}
     if (zptr^.zstuff[l] and MoveMask[how]) <> 0 then {A new discovery?}
      begin                            {Yes. There is something to zap.}
       zptr^.zChanged:=true;           {An admission.}
       if SP < Player[who].StashEarly then Player[who].StashEarly:=SP;         {Monitor change levels.}
      end;                             {Enough of internal talk.}
     zptr^.zstuff[l]:=zptr^.zstuff[l] and not MoveMask[how];    {THE DEED!}
    End; {of StrangleThatLosingMove.}  {From little bits do larger results grow.}
   Begin {of ReEducate.}
    if Trace then WriteLn('ReEducate:  SP=',SP:2,',Result=',result,',MirrorLevel=',MirrorLevel);
    if result < 0 then begin evoMAekaM(false,loser); StepBack; end; {Finger the winner.}
    evoMAekaM(false,winner); StepBack; {Finger the loser that allowed a win.}
    While SP > 0 do                    {Claw back, but not necessarily to zero.}
     begin                             {Perhaps an alternate branch remains.}
      mirror:=SP <= MirrorLevel;       {Perhaps two-handed.}
      if Trace then WriteLn('Reeducate:  SP=',SP:2,',who=',who,',way=',way,',how=',how,',which=',which,',mirror=',mirror);
      StrangleThatLosingMove;          {Belated population control.}
      evoMAekaM(True,loser);           {Just so.}
      if mirror then                   {We have a doppelganger.}
       begin                           {Through the looking-glass.}
        how:=-how;                     {Mirrored move.}
        c:=NC - c + 1;                 {Mirrored column, same row.}
        l:=(who - 1)*NC*2;             {Find the piece in my list of places.}
        i:=NC; while (i > 0) and ((r <> zptr^.zstuff[l + (i-1)*2 + 2]) or (c <> zptr^.zstuff[l + (i-1)*2 + 1])) do i:=i - 1;
        if (i > 0) and (i<>which) then {Thanks to Standardise, the list of places }
         begin                         { may be rearranged so that the mirrored index }
          which:=i;                    { may not finger the mirrored piece.}
          StrangleThatLosingMove;      {This applies if mirroring persists beyond the first move.}
          if ListMoves then WriteLn('(reflected)');   {Fling a hint in passing.}
         end;                          {So much for the other hand.}
       end;                            {Halving the effort is worth some pain.}
      PickSomeStashedMove;             {Scan for any alternative moves.}
      if movable > 0 then EXIT;        {Aha! All is not yet lost.}
      StepBack;                        {Otherwise, retreat from this hopeless position.}
      evoMAekaM(false,winner);         {He won't be a winner for long...}
      StepBack;                        {Unmake the other side's move that led to it.}
     end;                              {And consider the move that allowed him to.}
   End; {of ReEducate.}

  var winner: byte;                    {To be determined.}
  Procedure FinalVictory;              {Sound the trumpets' final call.}
   var renniw,ta: byte;
   Begin
    ta:=TextAttr;
    renniw:=3 - winner;                {The other player.}
    ShowScores;                        {Unconditional, now that the games are done.}
    WriteLn;                           {Final report impends, after all the floundering.}
    TextColor(PlayerColour[winner]);Write(PlayerSymbol[winner]);
    TextColor(LightRed);            WriteLn(' can force a win.');
    TextColor(PlayerColour[renniw]);Write(PlayerSymbol[renniw]);
    TextColor(LightBlue);           WriteLn(' hopes for error.');
    TextColor(PlayerColour[renniw]);WriteLn('Abandon all hope.');      {Quite.}
    AdjustEscutcheon(PlayerSymbol[winner]);
    TextColor(Green); TextBackground(Blue);
    Write('No moves to try.');         {All counter moves have led to loss.}
    Apotheosis:=true;                  {I have achieved it.}
    TextColor(White); TextBackground(Black); WriteLn;
    if not(Player[1].keyboard or Player[2].keyboard) then QuitRun:=true;
    if QuitRun then                    {I know when to give in.}
     begin                             {When one player cannot avoid defeat.}
      TextBackground(Black); TextColor(LightGray);
      WriteLn;
      Write('    The End.');
     end;
    TextAttr:=ta;
   End; {of FinalVictory.}

  var i,l: integer;
  Label 6,7;
  Begin {of Playagame.}
   LookTo(TheBoard);                   {Just so.}
   if ShowBoardMoves then Freshboard;  {Here we go.}
   InitialPlaces;                      {Fresh army.}
   ListSync:=true;                     {No confusions, yet.}
   SP:=0;                              {Soon we begin.}
   zptr:=nil;                          {And the stack will arise.}
   mirror:=true;                       {Symmetry as yet unbroken.}
   MirrorLevel:=0;                     {We haven't carried it far enough yet.}
   who:=2;                             {Syncopation for the first player.}
   way:=-1;                            {A turn flip before moving, like.}
   LookTo(TheCommentary); TextBackground(Black);
   Result:=0;                          {The game awaits resolution.}
 6:if not WithRetraction then          {Restarting a game from the start?}
    if ListMoves or Trace then ClrScr; {Yes, is there clobber to prepare for?}
   if Trace then WriteLn('PlayAGame:  MemAvail=',memavail,',MaxAvail=',Maxavail,',zSize=',zSize);
   repeat                              {Fight through to the end.}
    Who:=3 - who;                      {Switch to the other player.}
    way:=-way;                         {Who goes the other way.}
    MakeAMove;                         {Any move... Just decide How.}
    if KeyPressed or (StepWise and (result = 0)) then DealWith(KeyFondle);
   until (Result <> 0) or QuitRun;     {Game over yet?}

 7:if ListMoves then WriteLn(PlayerSymbol[who],GameWord[Result]);
   if not StepWise and not continual then DealWith(keyfondle);
   if result <> 0 then                 {Do we have a decision?}
    begin                              {Yes. A loser and a winner.}
     winner:=(1 - result)*3 div 2 + result*who; {Confusingly discovered.}
     Player[winner].Header.VictoryCount:=Player[winner].Header.VictoryCount + 1;
     if result > 0 then TextColor(PlayerColour[3 - who]);
     if ShowResult then ShowScores;    {Update a side show.}
     ReEducate;                        {The loser takes note...}
     if WithRetraction and (SP > 0) then   {Retraction rather than restart?}
      begin                            {Yes, resume with a different move.}
       if Trace then WriteLn('Retraction: movable=',movable);
       CopyStashToWork;                {Recover earlier work state.}
       if ListMoves or Trace then
        begin
         DescribeStore(zptr^.zStashRec);
         Write(' resumed.');
         if Trace then WriteLn;
        end;
       if StepWise or not continual then DealWith(KeyFondle);
       if ListMoves and not Trace then TrimStackDisplay;
       Result:=0;                      {We can hope for a different result.}
       Which:=Movable;                 {Re-Educate stopped when a piece was movable.}
       if Trace then WriteLn('Retry:      SP=',SP:2,',who=',who,',way=',way,',which=',which);
       Zptr^.zPieceMoved:=which;       {The new choice.}
       if ListMoves then DescribeStore(zptr^.zStashRec);
       ChooseMove;                     {Move away from the resumed position.}
       if StepWise then DealWith(KeyFondle);
       if result = 0 then goto 6 else goto 7;   {Beware of sudden results.}
      end;         {So much for move retraction and resuming a game.}
     if SP <= 0 then FinalVictory;     {Has re-education reached the end?}
    end;                               {So much for games ending with a result.}
   while zptr <> nil do Zdrop;         {Hack back the stack.}
   if not QuitRun then                 {Are we continuing?}
    begin                              {Yes. Consider the stash.}
     if Player[1].StashAche then Burp(1);
     if Player[2].StashAche then Burp(2);
     if not continual or KeyPressed then DealWith(KeyFondle);
    end;                               {So much for the stash.}
  End; {of PlayAGame.}

{############################ Enough of Game Playing.########################}
 Procedure SquawkOut;             {Reveal an explanation, and quit.}
  Var ScreenLine: integer;
  Var Unflushed: boolean;
  Procedure Z(Text: string);      {Roll some text.}
   Begin                          {With screen pauses.}
    if Unflushed then ClrEol;     {Perhaps bumf lurks on this line.}
    WriteLn(Text); ScreenLine:=ScreenLine + 1;      {Writes only to the end of text, not eol.}
    if ScreenLine >= Hi(WindMax) then      {Have we reached the bottom?}
     begin                        {Yes, the display would soon scroll up.}
      if Unflushed then ClrEol;   {A last remnant.}
      Unflushed:=false;           {Once scrolling starts, new lines are blank.}
      Write('(Press a key)');     {A hint, offering out-by-one possibilities.}
      if ReadKey = #0 then if ReadKey = ESC then;      {Ignore a key.}
      GoToXY(1,wherey); ClrEol;   {Scrub the hint.}
      ScreenLine:=0;              {Restart the count.}
     end;                         {So much for a screen full.}
   End;                           {So much for that line.}
  Begin                   {Just a wad of text.}
   ScreenLine:=0; Unflushed:=true;{Prepare the roll.}
   Z('                                    PawnPlex');
   Z('                               Egalitarian Chess');
   Z('                        (The only pieces are the pawns)');
   Z('                    On a game board of N rows by M columns.');
   Z('');
   Z('   Long ago, the Reader''s Digest Young People''s Annual for 1963 had an article');
   Z('by Martin Gardner on a simplified form of chess called Hexapawn to be played on');
   Z('a three by three board, and how to build a computer out of two dozen matchboxes');
   Z('that learns how to play the game. Each matchbox bears a diagram corresponding');
   Z('to a possible board position and has coloured arrows for the moves that can be');
   Z('made from that position. Within are smarties corresponding to the arrows, and');
   Z('when a particular move is determined to be bad, its smarty is devoured...');
   Z('   The pieces advance and attack just like pawns, though without the fancy');
   Z('options of a two step first move or capture en passant. Victory is gained by');
   Z('winning through to the last row or by leaving the enemy with no possible move');
   Z('either because all pieces are blocked, or else by capturing all pieces.');
   Z('   The programme plays permissible moves on a purely witless basis. It makes');
   Z('no attempt either to win or to avoid losing even with its current move, and');
   Z('it employs no ''look ahead'' analysis nor any position evaluation function...');
   Z('Indeed, if it makes a move that happens to block the enemy pieces, it doesn''t');
   Z('recognise that it has won until the other player discovers that it is blocked!');
   Z('However, whenever a player loses a game, the move that was made that led to');
   Z('the losing situation is removed from the schedule of possible moves, and in');
   Z('subsequent games, other moves will be tried instead. This analysis eventually');
   Z('ends with one of the players having no non-losing first move left...');
   Z('   Put another way, the idea is to "act out" the lookahead analysis on screen,');
   Z('rather than conduct it invisibly in order to select a move to show. Further,');
   Z('this analysis relies on the only true basis, the actual and definite win/lose');
   Z('result of a game''s termination, rather than on some sort of guess at a move''s');
   Z('merit by calculating at that stage an ad-hoc evaluation function that sort of');
   Z('works in trial runs but has no proof for the general case.');
   Z('   When a piece''s moves are being considered, there is a slight bias in that');
   Z('the capture of an enemy''s pawn will be preferred over a simple advance if');
   Z('there is a choice, but this is just because the tests have to be performed in');
   Z('some order, as I didn''t want to employ the apparatus for random choice, but');
   Z('also, corpses don''t move so there are thus fewer moves to consider further.');
   Z('Likewise, the piece to move is selected on the basis of it being the first');
   Z('movable piece in the list of current piece locations, and as the lists are');
   Z('ordered from top left to bottom right, this has the consequence that the first');
   Z('player''s army advances en mass by rank, whereas the second player''s army');
   Z('sends the pawn in the first file rushing forwards alone, unless you take over');
   Z('control of movement.');
   Z('   For each board size, a separate file will be kept under directory ' + StashDirectory);
   Z('(on the current disc drive) and it will grow as more and more positions are');
   Z('encountered. The file is accessed at random, depending on the board positions');
   Z('involved in a game, and so would not be at its best on a floppy disc.');
   Z('   You have the option of operating both, either or neither player, with the');
   Z('programme enforcing the rules and directing the other player(s). When your');
   Z('turn is awaited, one of your pieces on the board will be flashing to signify');
   Z('that it can move. You can step amongst your movable pieces by pressing the');
   Z('space or the backspace key and indicate the move by pressing an arrow key.');
   Z('Up (or down) to advance one row, and left or right to move the pawn to the');
   Z('screen''s left or right, as seen by you, not as from the pawn''s viewpoint.');
   Z('(I''d use the arrow keys, and Shift-arrow to signify the move, but... no go.)');
   Z('The squares to which your current piece can move are marked in red, but if in');
   Z('brown, that way led to defeat and dried blood from an earlier battle...');
   Z('   If you do surrender control of both players, you will be confronted by the');
   Z('spectacle of ignorant armies of pawns clashing by night as the total lack of');
   Z('strategy and tactics is demonstrated in a battlefield frenzy. Yet the idiot');
   Z('commander does learn, in the sense that losing moves are not made twice...');
   Z('');
   Z('   During a run, you have a variety of options which may be flipped by pressing');
   Z('the appropriate key, as in the following list (the capitals mean "On"):');
   Z(' B: Board action will be shown.');
   Z(' L: List the moves being made.');
   Z(' q: Quiet running (don''t show scores).');
   Z(' t: Trace the programme''s internal activities.');
   Z(' S: StepWise execution: press a key to advance a step.');
   Z(' c: Continual running- no pause at the end of each game.');
   Z(' r: Retract unwinning moves and Resume play without restarting.');
   Z(' W: Witless choice of moves, not even spotting an immediate win.');
   Z(' e: Every board position is recorded even if nothing was learnt of it.');
   Z(' O: swap controllers for player 1.');
   Z(' X: swap controllers for player 2.');
   Z('');
   Z('   Moves are listed in a compact form, using a single symbol for the row and');
   Z('the column number as shown along the side of the game board. The start square');
   Z('and the destination square are named with a symbol in between that describes');
   Z('the move made, so 12|22 would mean that a piece has moved from row one, column');
   Z('two to row two, column two, and this is a vertical move. A / or a \ signify a');
   Z('capturing move, and so are shown in red, whereas otherwise all appears in the');
   Z('colour of the moving piece. And there''s more. To the left appear one or two');
   Z('codes in green; these note how many moves could be made from the current');
   Z('position. Following the original count code is a blank, or else a lesser count,');
   Z('to signify that some possible moves have been removed from the list because');
   Z('they led to a loss that cannot be avoided.');
   Z('   Game positions and possible moves are held in the computer''s memory, and the');
   Z('storage cell is identified to the left of the move information. 666: means that');
   Z('disc record 666 contains the details, whereas 65# means that the details are');
   Z('held temporarily in the stack at level 65, and will only be saved somewhere in');
   Z('the disc file if something is learnt for that position, which is to say that');
   Z('one of its moves proves to be a bad one because it allowed the enemy to win...');
   Z('Level 65 may be used for many different positions as the battle proceeds, and');
   Z('many are not worth recording on disc, but option "Every board" will cause all');
   Z('to be saved.');
   Z('   The .ppx file holding the information on the positions so far encountered is');
   Z('not allowed to grow indefinitely. After about '+Ifmt(StashFullish)+' entries, a purge is done.');
   Z('Preference is given to entries low in the game tree, but some battles may end');
   Z('up being re-fought because the records that disallowed the moves that led to');
   Z('them have been ejected. Not recording every position helps...');
   Z('   A run finishes when the suppression of losing moves results in one of the');
   Z('players having no non-losing move left to make. When this happens, a special');
   Z('file called OutLook.txt will be updated by the placing of the winner''s symbol');
   Z('(an O or an X) in the location corresponding to the number of rows and columns');
   Z('of the current size board. For simplicity''s sake, this file has a fixed layout,');
   Z('so be careful if you want to alter anything before the bottom line. Similar');
   Z('files contain counts of victories and games played to reach this decision');
   Z('and likewise ought not be rearranged.');
   Z('');
   Z('   To activate:');
   Z('');
   Z('PAWNPLEX         plays on 8 x 8');
   Z('PAWNPLEX n       plays on n x n');
   Z('PAWNPLEX n m     plays on n x m: n rows, m columns.');
   Z('');
   Z('   Add a trailing collection of options, if desired.');
   Z('');
   Z('PAWNPLEX 4 SCR   plays on a 4 x 4 board, in a hurry.');
   Z('PAWNPLEX 4 5 XO  plays on a 4 x 5 board, with you directing both players.');
   Z('');
   Z('PAWNPLEX ?       offers this description and stops.');
   Z('');
   Z('   To quit, press the ESC key.');
   if AsItWas.mode <> Lastmode then    {Had we twiddled the screen mode?}
    if KeyFondle = ESC then;           {Yes, delay. Reverting will blank the screen.}
   HALT; {Actually, via the egress.}
  End;


 Var EgressSave:Pointer;               {Turbo pascal ritual }
 {$F+}Procedure Egress;{$F-}           { for cleaning-up on exit.}
  Begin
   if LastMode <> AsItWas.mode then TextMode(AsItWas.mode); {Plus, screen scrub.}
   TextAttr:=AsItWas.ta;               {Back to the original colours.}
   NoSound;                            {Just in case someone had started screaming.}
   ExitProc:=EgressSave;               {I'm done.}
  End;

{Damnit, Turbo pascal's pointer using procedures don't check for null pointers!}
{$F+} Function HeapFull(Size: word): integer; {$F-}
       Begin HeapFull:=1; End; {Sez "If full, return a null pointer" to GetMem.}

 Function EatInt(t:string; var i2: integer): boolean;
  var n,c: integer;
  Begin
   Val(t,n,c); {Why WHY  W H Y  a procedure, not a function!!!}
   if c = 0 then i2:=n;
   Eatint:=c = 0;
  End;

 Procedure ChooseALayout;    {Based on the screen's character dimensions, and the board.}
  Var LastLine,LastCol: byte;
  Var br,bc: byte;
  Procedure Zonk(ac1, br1,bc1,bc2, cr1,cc1,cc2, dr1,dc1: byte);
   Begin
    with pane[TheBoard] do
     begin
      col1:=ac1; row1:=1;
      col2:=col1 + bc - 1; row2:=row1 + br; {-1 omitted to prevent scrolling.}
     end;
    with pane[TheScore] do
     begin
      col1:=bc1; row1:=br1;
      col2:=bc2; row2:=row1 + ScoreLines;   {-1 omitted to prevent scrolling.}
     end;
    with pane[TheFlags] do
     begin
      col1:=cc1; row1:=cr1;
      col2:=cc2; row2:=row1 + StyleLines;   {-1 omitted to prevent scrolling.}
     end;
    with pane[TheCommentary] do
     begin
      col1:=dc1; row1:=dr1;
      col2:=LastCol; row2:=LastLine;
     end;
   End; {of Zonk.}
  Const LastLayout = 8;                {Surely enough.}
  Var StorySpace,sr,sc: array[1..LastLayout] of word;
  Var i,it: integer;
  var s1,s2,s3,s4,s5,s6: integer;
  var ch: char;
  Begin
   TextMode(C80+Font8x8);     {Crazed gibberish gives less unsquare character cells.}
   LastLine:=Hi(WindMax)+1;  LastCol:=Lo(WindMax)+1; {Demented!}
   bc:=OffsetCol + 2*NC + 1; br:=OffsetRow + NR + 1;
   if Trace then WriteLn('LastLine=',LastLine,', LastCol=',LastCol);
   if Trace then WriteLn('NR=',NR,', NC=',NC,', br=',br,', bc=',bc);
   CurrentWindow:=TheBoard;            {Initial confusion.}
   for i:=1 to LastPane do             {Its usage is always ab initio.}
    with pane[i] do                    {So previous positions are of no interest.}
     begin                             {Meanwhile, set all to something.}
      CursorCol:=1; CursorRow:=1;      {Might as well.}
      style:=TextAttr;                 {It will do.}
     end;                              {And on to the next.}
{  Consider various arrangements of the windowpanes, and select that which allows
the maximum space for the commentary... There is a complication in that the
description of a move requires TrailWidth columns, so that only TrailColumns
of them can fit on to one line and these numbers confuse the picture.
In principle, certain layouts could accommodate more move descriptions than
others that have more space, but on the other hand, trace output needs as
much space as possible. So space maximisation will suffice.
The layouts:

   Style Board Score      Board Comm           Score Board
   Commentary             Score ent            Style Commentary
                          Style ary

   Score Board            Style Board          Style Board
   Style                        Score                Score
   Commentary             Commentary                 Commentary

   B o a r d              B o a r d
   Score Commentary       Style Score Commentary
   Style

   It may well be that some layouts will never be preferred, but I can't be
bothered trying to decide this, given the variety of board sizes that may
be requested, not to mention the possible screen sizes. One might even
encounter a screen that is higher than it is wide, just like a sheet of
paper... So, stuff it.}

   sr[1]:=max(br,max(ScoreLines,StyleLines)); sc[1]:=0;
   sr[2]:=0;                                  sc[2]:=max(bc,max(MinStyleWidth,MinScoreWidth));
   sr[3]:=br;                                 sc[3]:=max(MinStyleWidth,MinScoreWidth);
   sr[4]:=max(br,ScoreLines + StyleLines);    sc[4]:=0;
   sr[5]:=max(StyleLines,br + ScoreLines);    sc[5]:=0;
   sr[6]:=br + ScoreLines;                    sc[6]:=MinStyleWidth;
   sr[7]:=br;                                 sc[7]:=max(MinStyleWidth,MinScoreWidth);
   sr[8]:=br;                                 sc[8]:=MinStyleWidth+MinScoreWidth;
   for i:=1 to LastLayout do StorySpace[i]:=(LastLine - sr[i])*(LastCol - sc[i]);
   if bc + MinScoreWidth + MinStyleWidth > LastCol    then StorySpace[1]:=0;
   if br + ScoreLines + 1 + StyleLines > LastLine     then StorySpace[2]:=0;
   if bc + max(MinStyleWidth,MinScoreWidth) > LastCol then StorySpace[3]:=0;
   if bc + max(MinStyleWidth,MinScoreWidth) > LastCol then StorySpace[4]:=0;
   if bc + MinStyleWidth > LastCol                    then StorySpace[5]:=0;
   if bc + MinStyleWidth > LastCol                    then StorySpace[6]:=0;
   if br + ScoreLines + 1 + StyleLines > LastLine     then StorySpace[7]:=0;
   if br + max(StyleLines,ScoreLines) > LastLine      then StorySpace[8]:=0;
   s1:=(LastCol - bc) div 2;           {Even Stevens.}
   s4:=min(LastCol - bc,3*MinScoreWidth);
   s5:=s4;
   s6:=sc[6];
   if Trace then begin for i:=1 to LastLayout do Write(StorySpace[i]:5); WriteLn; end;
   it:=1; for i:=2 to LastLayout do if StorySpace[i] > StorySpace[it] then it:=i;
   if Trace then WriteLn('it=',it);
   if StorySpace[it] <= 0 then         {Trepidation.}
    begin                              {Confirmed.}
     WriteLn('   Argh!');
     WriteLn('With a screen size of ',LastLine,' by ',LastCol,' offered on this system');
     WriteLn('and the board size of ',NR,' by ',NC,' requested for this run,');
     WriteLn('(two screen columns for each board column are needed)');
     WriteLn('none of my possible layouts can fit everything in.');
     WriteLn('(like, the Board, plus Score, plus Style, plus Commentary)');
     WriteLn('   If you can''t find a computer that this programme can recognise');
     WriteLn('as having a larger screen, then you will have to be content with');
     WriteLn(' a smaller board, or give up.');
     Croak('ChooseALayout regrets...');
    end;
   case it of                          {But otherwise...}
     {     Board,   Score,               Style,                     Story.}
    1:Zonk(s1+1,    1,s1+bc+1,LastCol,   1,1,s1,                    sr[1]+1,1);
    2:Zonk(1,       br+1,1,sc[2],        br+ScoreLines+1,1,sc[2],   sr[2]+1,sc[2]+1);
    3:Zonk(sc[3]+1, 1,1,sc[3],           ScoreLines+1,1,sc[3],      sr[3]+1,sc[3]+1);
    4:Zonk(s4+1,    1,1,s4,              ScoreLines+1,1,s4,         sr[4]+1,1);
    5:Zonk(s5+1,    br+1,s5+1,s5+s5,     1,1,s5,                    sr[5]+1,1);
    6:Zonk(s6+1,    br+1,s6+1,s6+s6,     1,1,s6,                    sr[6]+1,sc[6]+1);
    7:Zonk(1,       br+1,1,sc[7],        br+ScoreLines+1,1,sc[7],   sr[7]+1,sc[7]+1);
    8:Zonk(1,       br+1,1,MinScoreWidth,br+1,MinScoreWidth+1,sc[8],sr[8]+1,sc[8]+1);
   end;                                {Draw a diagram!}
   with pane[TheCommentary] do TrailColumns:=(col2 - col1 + 1) div TrailWidth;
   if Trace then
    begin
     WriteLn('tw=',TrailWidth,', tc=',TrailColumns);
     for i:=1 to LastPane do
      with pane[i] do
       WriteLn(i,': ',row1:3,col1:3,' - ',row2:3,col2:3,' Width=',col2-col1+1:3);
     ch:=KeyFondle;   {Pause. Window selection will damage the above output.}
    end;
   LookTo(TheCommentary);              {All is in readiness.}
  End; {of ChooseALayout.}

 var i,j,LastParam: integer;
 var z1: string[1];
 var ch: char;
 var who: byte;
 BEGIN
  Trace:=false;              {Desperation before the windowpanes are prepared?}
  AsItWas.mode:=LastMode;    {Grr. I might want to save the display content too!}
  AsItWas.ta:=TextAttr;      {Not just its colour and style.}
  EgressSave:=ExitProc; ExitProc:=@Egress;
  HeapError:=@HeapFull;      {I'd prefer functions to procedures...}

  ScoreBefore:=false;        {No scores shown yet.}
  for i:=1 to 2 do with Player[i] do
   begin                     {Prepare some stuff for this run.}
    Keyboard:=false;         {Parameters may adjust this.}
    StashAche:=false;        {No stashes to hand, so no aches.}
    StashEarly:=maxint;      {Track this run's lowest move quash.}
    PreviousVCount:=0;       {Spot changes to VictoryCount.}
    pvtext[i]:='x';          {To speed updating its screen display.}
   end;
  StepWise:=true;            {Might as well start of slowly.}
  ListMoves:=true;           {And admit activities.}
  ShowBoardMoves:=true;      {Even displaying the action on the board.}
  BoardSync:=false;          {But we haven't shown it yet.}
  ShowResult:=true;          {And showing the running score.}
  Continual:=false;          {But not running flat out.}
  Witless:=true;             {Manifest ignorance.}
  WithRetraction:=false;     {And re-fight every folly from the start.}
  FullRecall:=false;         {But not recording every board position attained.}
  PruneLevel:=0;             {Pre-emptive value not activated.}
  QuitRun:=false;            {We have not yet begun to fight!}
  Apotheosis:=false;         {Nor have we reached the conclusion.}

  if (ParamStr(1) = '?') or (ParamCount > 3) then SquawkOut;
  NR:=8; NC:=8;              {Default...}
  LastParam:=1;              {I'd prefer ParamStr(0) being the full text.}
  if ParamCount >= 1 then    {But instead, must deal with unwanted help.}
   begin                     {Anyway, we have a thingy.}
    if EatInt(ParamStr(1),NR) then
     begin                   {It was a number, so maybe another.}
      NC:=NR;                {In case there was only one number.}
      LastParam:=2;          {Thus the one past the last number.}
      if EatInt(ParamStr(2),NC) then LastParam:=3;    {Another number.}
     end;                    {So much for ad-hoc swallows.}
   end;                      {Enough of the numbers.}
  if LastParam > ParamCount then LastParam:=0;   {See later...}
  if NR < 2 then NR:=2;      {NR:=max(2,min(NR,RowLimit)), damnit.}
  if NC < 1 then NC:=1;      {Likewise.}
  if NR > RowLimit then NR:=RowLimit;
  if NC > ColumnLimit then NC:=ColumnLimit;
  if NC mod 2 = 0 then MirrorPlane:=0 else MirrorPlane:=NC div 2 + 1;
  Flabby:=(NR > 15) or (NC > 15);
  zSize:=SizeOf(zash)        {The tail end is cut to fit in use...}
   - (ColumnLimit - NC)*(2*SizeOf(RowAndColumn) + SizeOf(MoveBag));
  zMoveOffset:=4*NC;         {Mumblemumblemumble.}

  ChooseALayout;             {Messy stuff, but no talking until it is decided.}

  UnknownKey:=false;         {No unknown characters noticed.}
  if LastParam > 0 then      {Sniff for some possibilities.}
   for i:=1 to length(ParamStr(LastParam)) do
    begin                    {Crazed drivel thanks to nuisance type checking.}
     z1:=copy(ParamStr(LastParam),i,1);
     ch:=char(hi(integer(z1)));
     DealWith(ch);           {At last! (This will cause the Style box to appear)}
     if UnknownKey then WriteLn(ch,' unknown.');
    end;                     {Perhaps another.}

  for who:= 1 to 2 do with Player[who] do
   for i:=0 to StashChunks do
    begin
     if trace then WriteLn('who=',who,',i=',i,', MemAvail=',memavail);
     New(StashInRec[i]);
     if StashInRec[i] = nil then Croak('Insufficient memory for player '
      + ifmt(who) + '''s StashInRec['+ifmt(i)+'] array!');
     New(StashInBit[i]);
     if StashInBit[i] = nil then Croak('Insufficient memory for player '
      + ifmt(who) + '''s StashInBit['+ifmt(i)+'] array!');
    end;
  if trace then WriteLn('Stash finger stuff grabbed. MemAvail=',MemAvail);
  GrabAPlayPen;              {Get hold of a work file.}
  if trace then DealWith(KeyFondle);
  ClrScr;                    {Here we go.}
  ShowStyle;                 {Initial state. (May not have been poked by DealWith)}
  DrawBorder;                {Once only! It adjusts the dimensions too.}

  Repeat PlayAGame until QuitRun; {Just so.}

  for who:=1 to 2 do with Player[who] do
   begin
    if StashGrows then SaveStash(who) else SaveStashHeader(who);
    Close(StashFile);
   end;
  LookTo(TheCommentary); GoToXY(1,hi(WindMax) - 1);
  Write('Done.');
  if AsItWas.mode <> lastmode then
   begin
    TextBackground(Red);
    Write(' Press a key.');
    ch:=ReadKey;
   end;
 END.

```


### Some results

It is called Pawnplex because it is not for just a three by three board. Invoke from the command line with <code>Pawnplex.exe ?</code> to obtain a description or <code>Pawnplex.exe 3</code> to run for the original 3×3 problem. A windows interface may allow the specification of parameters to a run (say via a "shortcut") but otherwise, ... open a DOS window... As for one player or the other having a winning strategy, some partial results are shown in the following:

```txt

T|O?????????????????????????????????????
S|X?????????????????????????????????????
R|O?????????????????????????????????????
Q|X?????????????????????????????????????
P|O?????????????????????????????????????
O|X?????????????????????????????????????
N|O?????????????????????????????????????
M|X?????????????????????????????????????
L|OX????????????????????????????????????
K|XX????????????????????????????????????
J|OX????????????????????????????????????
I|XX????????????????????????????????????
H|OX????????????????????????????????????
G|XXX???????????????????????????????????
F|OXO???????????????????????????????????
E|XXX???????????????????????????????????
D|OXO???????????????????????????????????
C|XXX???????????????????????????????????
B|OXO???????????????????????????????????
A|XXXX??????????????????????????????????
9|OXOX??????????????????????????????????
8|XXXO??????????????????????????????????
7|OXOXO?????????????????????????????????
6|XXXOO?????????????????????????????????
5|OXOXOXO???????????????????????????????
4|XXXOXXOX??????????????????????????????
3|OXXOOXOOXXOXXOO???????????????????????
2|XOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
  123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabc

```

Which is an array of results, one for each size board - a board having only one row is impossible as the pieces can't be placed. Notice that row 3, column 3 has X, meaning that X has an unbeatable strategy, no matter what O tries, and so on for the other worked-through board sizes. The number of possible games increases very rapidly as the board size increases...


## Phix

OTT GUI version.
From reading the 1962 pdf, I felt the only thing that would do it justice would be to show a graphic 
representation of the 24 matchboxes, with grey arrows for still-eligible and red for eliminated.
Displays full training data (for black and white), and last game history.
Mouse-operated: Hover on an appropriate sector of one of your pieces until an arrow shows, and click.

```Phix
-- demo\rosetta\hexapawn.exw
include pGUI.e

-- A board is held as eg {"WWW","...","BBB"} (nb displayed upside down)
-- A move is held as {{{y1,x1},{y2,x2}},colour} where x/y are 1..3.

enum BOARD, MOVES, BLACK, WHITE 
-- The above correspond to board_canvas .. white_canvas.
-- BOARD|MOVES also apply within each of the above, that is
-- data[BOARD][BOARD], data[BOARD][MOVES], data[MOVES][BOARD],
-- etc are all valid uses, however data[BOARD][BLACK] isn't.

sequence data = {{{},{}},   -- BOARD    -- (current)
                 {{},{}},   -- MOVES    -- (game log)
                 {{},{}},   -- BLACK    -- (training data)
                 {{},{}}}   -- WHITE    -- (training data)

-- data[BOARD][BOARD] is always a sequence of length 1, for consistency,
--                    whereas [other][BOARD] can contain 0..19 of them,
--                    where that 19 is just from the highest yet seen.
-- data[BOARD][MOVES] is {} or one move, depending on the mouse hover.
-- data[MOVES][MOVES] has one move per board, except last which is {}.
-- data[BLACK/WHITE][MOVES] can contain 0-3 possible moves, obviously.

bool game_over = false
integer bw = 0, ww = 0  -- win counts

enum HUMAN, COMPUTER

-- The next five values flip on every move:
integer player, opponent,           -- 'W' <==> 'B'
        player_type, opponent_type, -- HUMAN/COMPUTER
        traindx = WHITE             -- or BLACK, top-level index to data

function possible_moves(sequence board)
    sequence moves = {}
    integer height = length(board),
            width = length(board[1]),
            direction = iff(player='W'?+1:-1)
    for row=1 to height do
        for col=1 to width do
            if board[row][col]=player then
                integer rd = row+direction
                if rd<1 or rd>height then ?9/0 end if -- game_over??
                for tgt=max(1,col-1) to min(col+1,width) do
                    if board[rd][tgt]==iff(tgt==col?'.':opponent) then
                        moves = append(moves,{{{row,col},{rd,tgt}},CD_DARK_GREY})
                    end if
                end for
            end if
        end for
    end for
    return moves
end function

-- We use mirroring to approximately halve the training data set sizes.
-- (every possible board/move has an equivalent mirror, some idential)

function mirror(sequence board)
-- flip each row, eg {".WW","W..","BBB"} <=> {"WW.","..W","BBB"}
    for i=1 to length(board) do
        board[i] = reverse(board[i])
    end for
    return board
end function

function mirror_move(sequence move)
-- from/to x := 4-x; y's and colour unchanged.
    integer {{{y1,x1},{y2,x2}},colour} = move
    move = {{{y1,4-x1},{y2,4-x2}},colour}
    return move
end function

function find_board(sequence board)
--
-- retrieve an index to the training data (created if rqd)
-- also returns bools flip (move must flip too then), and
-- is_mirror (ie the training data deserves two updates)
--
-- (Also used to init training data, ie show the 
--  starting board, with all moves valid/grey.)
--
    sequence mb = mirror(board)
    bool is_mirror = (mb==board), flip = false
    if mb<board then
        board = mb
        flip = true
    end if
    integer k = find(board,data[traindx][BOARD])
    if k=0 then
        sequence moves = possible_moves(board)
        data[traindx][BOARD] &= {board}
        data[traindx][MOVES] &= {moves}
        k = length(data[traindx][BOARD])
    end if
    return {k,flip,is_mirror}
end function

procedure train(integer mdx)
--
-- Learn something from losing a game.
-- player, traindx should already be set.
--
-- nb learn both/propagate are technically not in the original spec,
--    which confiscates max one bead per round. Obviously you could
--    instead hunt for one earlier move when moves[m][2]==CD_RED,
--    and simply play more games to complete the training.
--
    data[MOVES][MOVES][mdx][1][2] = CD_RED  -- (just the game log)
    sequence board = data[MOVES][BOARD][mdx],
             move = data[MOVES][MOVES][mdx][1]
    {integer k, bool flip, bool is_mirror} = find_board(board)
    sequence moves = data[traindx][MOVES][k]
    if flip then move = mirror_move(move) end if
    integer m = find(move[1],vslice(moves,1))
    if moves[m][2]!=CD_RED then
        moves[m][2] = CD_RED
        if is_mirror then -- (learn both)
            m = find(mirror_move(move)[1],vslice(moves,1))
            moves[m][2] = CD_RED
        end if
        data[traindx][MOVES][k] = moves
        if mdx>2 and sum(sq_ne(vslice(moves,2),CD_RED))=0 then
            train(mdx-2) -- (propagate)
        end if
    end if
end procedure

procedure play_move(sequence move)
    if length(move) then
        move[2] = CD_DARK_GREY
        integer {{{y1,x1},{y2,x2}}/*,colour*/} = move
        data[BOARD][BOARD][1][y1][x1] = '.'
        data[BOARD][BOARD][1][y2][x2] = player
        data[MOVES][MOVES][$] = {move}  -- (fill in game log)
        data[MOVES][BOARD] &= data[BOARD][BOARD]
        data[MOVES][MOVES] &= {{{}}}
        {player, opponent} = {opponent, player}
        {player_type, opponent_type} = {opponent_type, player_type}
        traindx = WHITE+BLACK - traindx -- (toggle b/w)
        sequence board = data[BOARD][BOARD][1]
        if y2!=2 -- (must be "third row" then, ie 1|3)
        or not find(player,flatten(board))
        or possible_moves(board)={} then
            game_over = true
            if player='W' then bw += 1 else ww +=1 end if
            train(length(data[MOVES][MOVES])-2)
        else
            {} = find_board(board)
        end if
    end if
end procedure

procedure opponent_play()
--
-- does nothing if opponent is HUMAN
--
    if not game_over
    and player_type=COMPUTER then
        sequence board = data[BOARD][BOARD][1]
        {integer k, bool flip, bool is_mirror} = find_board(board)
        sequence moves = data[traindx][MOVES][k], not_red = {}
        for m=1 to length(moves) do
            if moves[m][2]!=CD_RED then
                not_red = append(not_red,moves[m])
            end if
        end for
        if length(not_red) then moves = not_red end if
        sequence move = moves[rand(length(moves))]
        if flip then move = mirror_move(move) end if
        play_move(move)
    end if
end procedure

procedure new_game()
    data[BOARD] = {{{"WWW","...","BBB"}},{{}}}
    data[MOVES] = data[BOARD]
    game_over = false
    {player,opponent} = "WB"
    {player_type,opponent_type} = {HUMAN,COMPUTER} -- (or any)
    traindx = WHITE
    {} = find_board(data[BOARD][BOARD][1])  -- (init training data)
end procedure

procedure train_rec()
--
-- Note this might require some more looping if train() is changed
-- to respect max-one-per-round original rules, as mentioned above
--
    sequence board = data[BOARD][BOARD][1],
             moves = possible_moves(board),
             plopt = {player, opponent, traindx, data[MOVES]},
             bwww = {bw,ww}
    for i=1 to length(moves) do
        play_move(moves[i])
        if not game_over then train_rec() end if
        game_over = false
        data[BOARD][BOARD][1] = board
        {player, opponent, traindx, data[MOVES]} = plopt
    end for
    {bw,ww} = bwww
end procedure

Ihandle dlg

-- saved from draw for mouse hover checks:
sequence board_centres = {}
integer board_radius2 = 0

procedure draw_board(cdCanvas cddbuffer, integer dx, i, px, py, mx)
    sequence board = data[dx][BOARD][i],
             centres = board    -- (fully overwritten in draw pieces step)
    integer diameter = floor(mx/5),
            gap = floor(mx/8),  -- (squares are twice the gap)
            rhs = 7*gap         -- (lhs==gap, rhs/lhs==top/btm)
    --
    -- draw grid lines
    --
    cdCanvasSetForeground(cddbuffer,CD_GREY)
    cdCanvasLineWidth(cddbuffer,1)
    for l=1 to 7 by 2 do
        integer l3 = l*gap
        cdCanvasLine(cddbuffer, px+gap, py+l3, px+rhs, py+l3)
        cdCanvasLine(cddbuffer, px+l3, py+gap, px+l3, py+rhs)
    end for
    --
    -- draw pieces
    --
    for r=1 to length(board) do
        for c=1 to length(board[r]) do
            integer cx = px+2*c*gap,
                    cy = py+2*r*gap
            centres[r][c] = {cx,cy}
            integer piece = board[r][c]
            if piece!='.' then
                cdCanvasSetForeground(cddbuffer,iff(piece='B'?CD_BLACK:CD_WHITE))
                cdCanvasSector(cddbuffer, cx, cy, diameter, diameter, 0, 360)
                cdCanvasSetForeground(cddbuffer,CD_BLACK)
                cdCanvasArc(cddbuffer, cx, cy, diameter, diameter, 0, 360)
            end if
        end for
    end for
    if dx=BOARD then
        -- save for motion_cb
        board_centres = centres
        board_radius2 = gap*gap
    end if
    --
    -- draw arrows
    --
    cdCanvasLineWidth(cddbuffer,2)
    sequence arrows = data[dx][MOVES][i]
    for i=1 to length(arrows) do
        if length(arrows[i])=0 then exit end if
        integer {{{x1,y1},{x2,y2}},colour} = arrows[i]
        {x1,y1} = centres[x1][y1]
        {x2,y2} = centres[x2][y2]
        if y1=y2 then ?9/0 end if   -- horizontal??
        integer ax = iff(x1<x2?-1:+1),
                ay = iff(y1<y2?-1:+1)
        cdCanvasSetForeground(cddbuffer,colour)
        if x1!=x2 then -- diagonal:
            cdCanvasLine(cddbuffer,x1,y1,x2+ax,y2+ay)
            cdCanvasLine(cddbuffer,x2+ax,y2,x2+ax,y2+ay*6)
            cdCanvasLine(cddbuffer,x2,y2+ay,x2+ax*6,y2+ay)
        else          -- vertical:
            cdCanvasLine(cddbuffer,x1,y1,x2,y2+ay)
            cdCanvasLine(cddbuffer,x2-ax,y2+ay,x2+ax*4,y2+ay*6)
            cdCanvasLine(cddbuffer,x2+ax,y2+ay,x2-ax*4,y2+ay*6)
        end if
    end for
end procedure
 
function redraw_cb(Ihandle ih, integer /*posx*/, integer /*posy*/)
    integer dx = IupGetInt(ih,"DATA"),
            {cw,ch} = IupGetIntInt(ih, "DRAWSIZE")
    --
    -- first, calculate the best nw*nh way to fit them all in:
    --
    integer nw = 1, nh = 1
    while nw*nh<length(data[dx][1]) do
        if (cw/(nw+1))>(ch/(nh+1)) then
            nw += 1
        else
            nh += 1
        end if
    end while
    integer mx = min(floor(cw/nw), floor(ch/nh))
    --
    -- now draw all the boards
    --
    cdCanvas cddbuffer = IupGetAttributePtr(ih,"DBUFFER")
    IupGLMakeCurrent(ih)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    integer px = floor((cw-nw*mx)/2), -- (set margin)
            py = floor((ch-nh*mx)/2), --      ""
            this_row = 0
    py = ch-mx-py -- (start at the top)
    for i=1 to length(data[dx][1]) do
        draw_board(cddbuffer,dx,i,px,py,mx)
        px += mx
        this_row += 1
        if this_row>=nw then
            this_row = 0
            px = floor((cw-nw*mx)/2)
            py -= mx
        end if
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    IupGLMakeCurrent(ih)
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvas cddbuffer = cdCreateCanvas(CD_GL, "10x10 %g", {res})
    IupSetAttributePtr(ih,"DBUFFER",cddbuffer)
    cdCanvasSetBackground(cddbuffer, CD_PARCHMENT)
    return IUP_DEFAULT
end function

function canvas_resize_cb(Ihandle canvas)
    cdCanvas cddbuffer = IupGetAttributePtr(canvas,"DBUFFER")
    integer {canvas_width, canvas_height} = IupGetIntInt(canvas, "DRAWSIZE")
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvasSetAttribute(cddbuffer, "SIZE", "%dx%d %g", {canvas_width, canvas_height, res})
    return IUP_DEFAULT
end function

function check_position(integer px, py)
--
-- check if x,y is on (a 45 degree sector of player's piece of) a legal move.
-- notes: a result of {{1,1},{2,2},CD_GREEN} means bottom left takes centre.
--        perfectly fine to invoke for either the black or white player.
--        this can get invoked when board_centres still=={} (returns {}).
--
    if not game_over 
    and player_type=HUMAN
    and board_centres!={} then -- (when started with mouse cursor on-board)
        integer height = length(board_centres),
                width = length(board_centres[1]),
                direction = iff(player='W'?+1:-1)
        for y=1 to height do
            integer yd = y+direction
            if yd>=1 and yd<=height then
                for x=1 to width do
                    sequence c = sq_sub(board_centres[4-y][x],{px,py})
                    integer {cx,cy} = sq_mul(c,{1,direction})
                    if cy>0 and (cx*cx+cy*cy)<=board_radius2 then
                        atom angle = -arctan(cx/cy)*CD_RAD2DEG
                        if angle>=-67.5 and angle<=67.5 then
                            -- within one of three 45 degree sectors:
                            -- >22.5 is a move right,
                            -- 22.5..-22.5 is a move up,
                            -- <-22.5 is a move left
                            sequence board = data[BOARD][BOARD][1]
                            integer move = floor((angle+22.5)/45), -- (-1/0/+1)
                                    tgt = iff(move=0?'.':opponent),
                                    xm = x+move
                            if xm>=1 and xm<=width
                            and board[y][x]=player
                            and board[yd][xm]==tgt then
                                return {{{y,x},{yd,xm}},CD_GREEN}
                            end if
                        end if
                        return {}   -- (we may as well quit now)
                    end if
                end for
            end if
        end for
    end if
    return {}
end function

function motion_cb(Ihandle board_canvas, integer x, y, atom /*pStatus*/)
    data[BOARD][MOVES][1] = {check_position(x,y)}
    IupUpdate(board_canvas) 
    return IUP_CONTINUE
end function

procedure redraw_all()
    Ihandle frame = IupGetAttributePtr(dlg,"BOARD_FRAME")
    string title = iff(game_over?sprintf("%c wins (Black:%d, White:%d)",{opponent,bw,ww}):"Board")
    IupSetAttribute(frame, "TITLE", title)
    IupUpdate(IupGetAttributePtr(dlg,"BOARD_CANVAS"))
    IupUpdate(IupGetAttributePtr(dlg,"MOVES_CANVAS"))
    IupUpdate(IupGetAttributePtr(dlg,"WHITE_CANVAS"))
    IupUpdate(IupGetAttributePtr(dlg,"BLACK_CANVAS"))
end procedure

function button_cb(Ihandle /*board_canvas*/, integer button, pressed, x, y, atom /*pStatus*/)
    if button=IUP_BUTTON1 and not pressed then      -- (left button released)
        play_move(check_position(x,y))
        opponent_play()
        redraw_all()
    end if
    return IUP_CONTINUE
end function

function new_game_cb(Ihandle /*ih*/)
    new_game()
    redraw_all()
    return IUP_DEFAULT
end function

function train_cb(Ihandle /*ih*/)
    sequence pott = {player_type, opponent_type}
    new_game()
    train_rec()
    {player_type, opponent_type} = pott -- (restore)
    redraw_all()
    return IUP_DEFAULT
end function

function exit_cb(Ihandle /*ih*/)
    return IUP_CLOSE
end function

constant help_text = """
Hexapawn may be the simplest game for which a self-learning game AI 
is not completely and utterly trivial, or at least non-obvious.

Pieces can move forward into an empty square or diagonally to take
an opponent's piece (no initial double or en passant moves). 
The game is over if a piece reaches the third row, all opponent 
pieces are captured, or the opponent has no valid move. 
White always goes first.

Hover the mouse over a (45 degree sector of) a piece until an arrow 
appears (if that's valid), then click.

Either or both players can be computer or human (the machine learns
from both). Note that when both are computer, mouse clicks (anywhere
on the board canvas) are still needed before it makes a move.

The "Train" option performs an exhaustive search of all possible moves.
Note the results may turn out slightly different if you manually 
teach it a few moves first. Once fully trained, white cannot possibly win.
Interestingly, it never learns anything about winning moves, it just 
avoids losing moves, which is enough to make it (/black) unbeatable.

In the training data, red arrows indicate losing moves, and grey ones
still potential winners. Red arrows in the game log indicate what has
just been learnt from playing(/losing) the last game.

The human-player games quickly become a test to see how fast you 
can decipher/fill the training data, which is made slightly more 
difficult (for a human) by board/move mirroring. How many games can
you win, and how few to prove that white will (eventually) always 
lose? My records are 14 and 3 respectively, however on the latter
you will get/need a few more wins on the way to finishing the full
training of it.

Enhancing this game to handle larger boards and other games such as 
noughts and crosses/tic-tac-toe and/or checkers/draughts (etc) is 
left as an exercise for the reader.
"""

function help_cb(Ihandln /*ih*/)
    IupMessage("Hexapawn",help_text)
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*dlg*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=K_F1 then return help_cb(NULL) end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
    Ihandle board_canvas = IupGLCanvas("DATA=%d,RASTERSIZE=50x50",{BOARD}),
            moves_canvas = IupGLCanvas("DATA=%d,RASTERSIZE=50x50",{MOVES}),
            black_canvas = IupGLCanvas("DATA=%d,RASTERSIZE=50x50",{BLACK}),
            white_canvas = IupGLCanvas("DATA=%d,RASTERSIZE=50x50",{WHITE}),
            board_frame = IupFrame(board_canvas,"TITLE=Board"),
            moves_frame = IupFrame(moves_canvas,"TITLE=\"Game log\""),
            black_frame = IupFrame(black_canvas,"TITLE=Black"),
            white_frame = IupFrame(white_canvas,"TITLE=White"),
            left = IupSplit(board_frame,moves_frame,"ORIENTATION=HORIZONTAL,MINMAX=100:900"),
            right = IupVbox({black_frame,white_frame}),
            full = IupSplit(left,right,"ORIENTATION=VERTICAL,MINMAX=100:900")
    dlg = IupDialog(full,"TITLE=Hexapawn,RASTERSIZE=800x800")
    IupSetAttributePtr(dlg,"BOARD_FRAME",board_frame)
    IupSetAttributePtr(dlg,"BOARD_CANVAS",board_canvas)
    IupSetAttributePtr(dlg,"MOVES_CANVAS",moves_canvas)
    IupSetAttributePtr(dlg,"WHITE_CANVAS",white_canvas)
    IupSetAttributePtr(dlg,"BLACK_CANVAS",black_canvas)
    Ihandles c4 = {board_canvas, moves_canvas, black_canvas, white_canvas}
    IupSetCallbacks(c4,{"ACTION",Icallback("redraw_cb"),
                        "MAP_CB",Icallback("map_cb"),
                        "RESIZE_CB", Icallback("canvas_resize_cb")})
    IupSetCallbacks(board_canvas, {"MOTION_CB", Icallback("motion_cb"),
                                   "BUTTON_CB", Icallback("button_cb")})
    
    Ihandle main_menu = IupMenu({IupMenuItem("&New game",Icallback("new_game_cb")),
                                 IupMenuItem("&Train",Icallback("train_cb")),
                                 IupMenuItem("&Help (F1)",Icallback("help_cb")),
                                 IupMenuItem("E&xit", Icallback("exit_cb"))})
    IupSetAttributeHandle(dlg,"MENU",main_menu)
    IupSetCallback(dlg, "K_ANY", Icallback("key_cb"))
    new_game()
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure
 
main()
```

There is also a straightforward translation of Go in demo\rosetta\heaxpawn_go.exw if anyone is interested...
