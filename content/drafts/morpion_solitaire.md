+++
title = "Morpion solitaire"
description = ""
date = 2019-07-19T09:44:12Z
aliases = []
[extra]
id = 9890
[taxonomies]
categories = []
tags = []
+++

{{draft task|Games}}

'''Task Requirements'''

The task is to get the computer to play a game of [[wp:Morpion solitaire|Morpion solitaire]]. For this task, it is acceptable for the computer to make randomly selected valid moves, rather than playing the strongest move each time. Use the standard version, with 5 point lines that allows parallel lines to touch at the endpoints.

[[File:Morpion 5T67-typical.png|thumb|right|A typical random game of "touching" (5T) morpion as rendered by Pentasol]]
(Proposed additional requirement):  Provide sample output. Preferably the output should be a record of moves suitable for replay using Pentasol.  Alternately, ASCII art can be used; however, games rendered in this manner may not provide enough information to replay the game.  Please see [[Talk:Morpion_solitaire#Game_Notation|the discussion on game notation and rendering]] for more information.

'''Playing Morpion Solitaire'''

There are several variations of the game, this task deals with the 5 point "touching" version also known as "5T".

Morpian solitaire is played on a (theoretically) infinite grid. It begins with 36 points marked in a Greek cross:

```txt

...XXXX...
...X..X...
...X..X...
XXXX..XXXX
X........X
X........X
XXXX..XXXX
...X..X...
...X..X...
...XXXX...

```


* A move is made by adding one point anywhere that creates a new line of 5 points (without spaces) and drawing a line through them. (Moves are commonly marked with the number of the move for visual clarity. Creating a record of the game in game notation is a better way to validate a game.) 
* Any two lines not running in the same direction may cross. 
* Any two lines running in the same direction are allowed to touch at the ends but not overlap (i.e. share at most a single point).
* The game ends when you run out of legal moves.  (The game score is the number of legal moves played.)

The rules to morpion solitaire are [http://www.morpionsolitaire.com/English/Rules.htm here].  

'''Background'''

A short history of the 5T game:

* 170 - Bruneau, by hand in 1976
* 117 and 122 - Juillé in 1995 and 1999
* 143 - Zimmer in 2003
* 144 - Cazenave in 2007  
* 172 - Rosin in 2010
* 171 and 172 - Tishchenko in 2011
* 177 and 178 - Rosin in 2011  

For an up to date list of [http://www.morpionsolitaire.com/English/RecordsGrids5T.htm Morpion 5T Records] see here.
The shortest game possible is [http://www.morpionsolitaire.com/English/Limits.htm 20 moves].

The game is NP-hard in the general case and has a huge search space and is a test case for research into searching methods.

Theoretical bounds have been placed on the longest 5T game.  The lower bound of 170 and upper bound of either 324 or 704 according to two different papers (see talk page).

__TOC__


## C

Console play with ncurses.  Length and touching rules can be changed at the begining of source code.  'q' key to quit, space key to toggle auto move, anykey for next move.  Play is random.  I got nowhere near the record 177 moves, but did approach the worst-possible (20) quite often.

```C>#include <ncurses.h

#include <stdlib.h>
#include <unistd.h>
#include <time.h>

/* option: how long a line is. Options probably should have been made into
* commandline args, if I were not lazy.  Also, if line_len is set to 3,
* the game may keep going indefinitely: best use auto mode. */
int line_len = 5;

/* option: whether two lines are allowed to be in the same direction and
* connected end to end.  Note: two lines crossing are always ok. */
int disjoint = 0;

int **board = 0, width, height;

#define for_i for(i = 0; i < height; i++)
#define for_j for(j = 0; j < width; j++)
enum {
	s_blank		= 0,
	s_occupied	= 1 << 0,
	s_dir_ns	= 1 << 1,
	s_dir_ew	= 1 << 2,
	s_dir_ne_sw	= 1 << 3,
	s_dir_nw_se	= 1 << 4,
	s_newly_added	= 1 << 5,
	s_current	= 1 << 6,
};

int irand(int n)
{
	int r, rand_max = RAND_MAX - (RAND_MAX % n);
	while ((r = rand()) >= rand_max);
	return r / (rand_max / n);
}

int** alloc_board(int w, int h)
{
	int i;
	int **buf = calloc(1, sizeof(int *) * h + sizeof(int) * h * w);

	buf[0] = (int*)(buf + h);
	for (i = 1; i < h; i++)
		buf[i] = buf[i - 1] + w;
	return buf;
}

/* -1: expand low index end; 1: exten high index end */
void expand_board(int dw, int dh)
{
	int i, j;
	int nw = width + !!dw, nh = height + !!dh;

	/* garanteed to fragment heap: not realloc because copying elements
	 * is a bit tricky */
	int **nbuf = alloc_board(nw, nh);

	dw = -(dw < 0), dh = -(dh < 0);

	for (i = 0; i < nh; i++) {
		if (i + dh < 0 || i + dh >= height) continue;
		for (j = 0; j < nw; j++) {
			if (j + dw < 0 || j + dw >= width) continue;
			nbuf[i][j] = board[i + dh][j + dw];
		}
	}
	free(board);

	board = nbuf;
	width = nw;
	height = nh;
}

void array_set(int **buf, int v, int x0, int y0, int x1, int y1)
{
	int i, j;
	for (i = y0; i <= y1; i++)
		for (j = x0; j <= x1; j++)
			buf[i][j] = v;
}

void show_board()
{
	int i, j;
	for_i for_j mvprintw(i + 1, j * 2,
			(board[i][j] & s_current) ? "X "
			: (board[i][j] & s_newly_added) ? "O "
			: (board[i][j] & s_occupied) ? "+ " : "  ");
	refresh();
}

void init_board()
{
	width = height = 3 * (line_len - 1);
	board = alloc_board(width, height);

	array_set(board, s_occupied, line_len - 1, 1, 2 * line_len - 3, height - 2);
	array_set(board, s_occupied, 1, line_len - 1, width - 2, 2 * line_len - 3);

	array_set(board, s_blank, line_len, 2, 2 * line_len - 4, height - 3);
	array_set(board, s_blank, 2, line_len, width - 3, 2 * line_len - 4);
}

int ofs[4][3] = {
	{0, 1, s_dir_ns},
	{1, 0, s_dir_ew},
	{1, -1, s_dir_ne_sw},
	{1, 1, s_dir_nw_se}
};

typedef struct { int m, s, seq, x, y; } move_t;

/* test if a point can complete a line, or take that point */
void test_postion(int y, int x, move_t * rec)
{
	int m, k, s, dx, dy, xx, yy, dir;
	if (board[y][x] & s_occupied) return;

	for (m = 0; m < 4; m++) { /* 4 directions */
		dx = ofs[m][0];
		dy = ofs[m][1];
		dir = ofs[m][2];

		for (s = 1 - line_len; s <= 0; s++) { /* offset line */
			for (k = 0; k < line_len; k++) {
				if (s + k == 0) continue;

				xx = x + dx * (s + k);
				yy = y + dy * (s + k);
				if (xx < 0 || xx >= width || yy < 0 || yy >= height)
					break;

				/* no piece at position */
				if (!(board[yy][xx] & s_occupied)) break;

				/* this direction taken */
				if ((board[yy][xx] & dir)) break;
			}
			if (k != line_len) continue;

			/* position ok; irand() to even each option's chance of
			   being picked */
			if (! irand(++rec->seq))
				rec->m = m, rec->s = s, rec->x = x, rec->y = y;
		}
	}
}

void add_piece(move_t *rec) {
	int dx = ofs[rec->m][0];
	int dy = ofs[rec->m][1];
	int dir= ofs[rec->m][2];
	int xx, yy, k;

	board[rec->y][rec->x] |= (s_current | s_occupied);

	for (k = 0; k < line_len; k++) {
		xx = rec->x + dx * (k + rec->s);
		yy = rec->y + dy * (k + rec->s);
		board[yy][xx] |= s_newly_added;
		if (k >= disjoint || k < line_len - disjoint)
			board[yy][xx] |= dir;
	}
}

int next_move()
{
	int i, j;
	move_t rec;
	rec.seq = 0;

	/* wipe last iteration's new line markers */
	for_i for_j board[i][j] &= ~(s_newly_added | s_current);

	/* randomly pick one of next legal moves */
	for_i for_j test_postion(i, j, &rec);

	/* didn't find any move, game over */
	if (!rec.seq) return 0;

	add_piece(&rec);

	rec.x = (rec.x == width  - 1) ? 1 : rec.x ? 0 : -1;
	rec.y = (rec.y == height - 1) ? 1 : rec.y ? 0 : -1;

	if (rec.x || rec.y) expand_board(rec.x, rec.y);
	return 1;
}

int main()
{
	int ch = 0;
	int move = 0;
	int wait_key = 1;

	init_board();
	srand(time(0));

	initscr();
	noecho();
	cbreak();

	do  {
		mvprintw(0, 0, "Move %d", move++);
		show_board();
		if (!next_move()) {
			next_move();
			show_board();
			break;
		}
		if (!wait_key) usleep(100000);
		if ((ch = getch()) == ' ') {
			wait_key = !wait_key;
			if (wait_key) timeout(-1);
			else timeout(0);
		}
	} while (ch != 'q');

	timeout(-1);
	nocbreak();
	echo();

	endwin();
	return 0;
}
```



## Go

{{libheader|goncurses}}
{{trans|C}}

```go
package main

import (
    gc "github.com/rthornton128/goncurses"
    "log"
    "math/rand"
    "time"
)

// optional settings
const (
    lineLen  = 5
    disjoint = 0
)

var (
    board  [][]int
    width  int
    height int
)

const (
    blank    = 0
    occupied = 1 << (iota - 1)
    dirNS
    dirEW
    dirNESW
    dirNWSE
    newlyAdded
    current
)

var ofs = [4][3]int{
    {0, 1, dirNS},
    {1, 0, dirEW},
    {1, -1, dirNESW},
    {1, 1, dirNWSE},
}

type move struct{ m, s, seq, x, y int }

func btoi(b bool) int {
    if b {
        return 1
    }
    return 0
}

func allocBoard(w, h int) [][]int {
    buf := make([][]int, h)
    for i := 0; i < h; i++ {
        buf[i] = make([]int, w)
    }
    return buf
}

func boardSet(v, x0, y0, x1, y1 int) {
    for i := y0; i <= y1; i++ {
        for j := x0; j <= x1; j++ {
            board[i][j] = v
        }
    }
}

func initBoard() {
    height = 3 * (lineLen - 1)
    width = height
    board = allocBoard(width, height)

    boardSet(occupied, lineLen-1, 1, 2*lineLen-3, height-2)
    boardSet(occupied, 1, lineLen-1, width-2, 2*lineLen-3)
    boardSet(blank, lineLen, 2, 2*lineLen-4, height-3)
    boardSet(blank, 2, lineLen, width-3, 2*lineLen-4)
}

// -1: expand low index end; 1: expand high index end
func expandBoard(dw, dh int) {
    dw2, dh2 := 1, 1
    if dw == 0 {
        dw2 = 0
    }
    if dh == 0 {
        dh2 = 0
    }
    nw, nh := width+dw2, height+dh2
    nbuf := allocBoard(nw, nh)
    dw, dh = -btoi(dw < 0), -btoi(dh < 0)
    for i := 0; i < nh; i++ {
        if i+dh < 0 || i+dh >= height {
            continue
        }
        for j := 0; j < nw; j++ {
            if j+dw < 0 || j+dw >= width {
                continue
            }
            nbuf[i][j] = board[i+dh][j+dw]
        }
    }
    board = nbuf
    width, height = nw, nh
}

func showBoard(scr *gc.Window) {
    for i := 0; i < height; i++ {
        for j := 0; j < width; j++ {
            var temp string
            switch {
            case (board[i][j] & current) != 0:
                temp = "X "
            case (board[i][j] & newlyAdded) != 0:
                temp = "0 "
            case (board[i][j] & occupied) != 0:
                temp = "+ "
            default:
                temp = "  "
            }
            scr.MovePrintf(i+1, j*2, temp)
        }
    }
    scr.Refresh()
}

// test if a point can complete a line, or take that point
func testPosition(y, x int, rec *move) {
    if (board[y][x] & occupied) != 0 {
        return
    }
    for m := 0; m < 4; m++ { // 4 directions
        dx := ofs[m][0]
        dy := ofs[m][1]
        dir := ofs[m][2]
        var k int
        for s := 1 - lineLen; s <= 0; s++ { // offset line
            for k = 0; k < lineLen; k++ {
                if s+k == 0 {
                    continue
                }
                xx := x + dx*(s+k)
                yy := y + dy*(s+k)
                if xx < 0 || xx >= width || yy < 0 || yy >= height {
                    break
                }

                // no piece at position
                if (board[yy][xx] & occupied) == 0 {
                    break
                }

                // this direction taken
                if (board[yy][xx] & dir) != 0 {
                    break
                }
            }
            if k != lineLen {
                continue
            }

            // position ok
            // rand.Intn to even each option's chance of being picked
            rec.seq++
            if rand.Intn(rec.seq) == 0 {
                rec.m, rec.s, rec.x, rec.y = m, s, x, y
            }
        }
    }
}

func addPiece(rec *move) {
    dx := ofs[rec.m][0]
    dy := ofs[rec.m][1]
    dir := ofs[rec.m][2]
    board[rec.y][rec.x] |= current | occupied
    for k := 0; k < lineLen; k++ {
        xx := rec.x + dx*(k+rec.s)
        yy := rec.y + dy*(k+rec.s)
        board[yy][xx] |= newlyAdded
        if k >= disjoint || k < lineLen-disjoint {
            board[yy][xx] |= dir
        }
    }
}

func nextMove() bool {
    var rec move
    // wipe last iteration's new line markers
    for i := 0; i < height; i++ {
        for j := 0; j < width; j++ {
            board[i][j] &^= newlyAdded | current
        }
    }

    // randomly pick one of next legal moves
    for i := 0; i < height; i++ {
        for j := 0; j < width; j++ {
            testPosition(i, j, &rec)
        }
    }

    // didn't find any move, game over
    if rec.seq == 0 {
        return false
    }

    addPiece(&rec)

    if rec.x == width-1 {
        rec.x = 1
    } else if rec.x != 0 {
        rec.x = 0
    } else {
        rec.x = -1
    }

    if rec.y == height-1 {
        rec.y = 1
    } else if rec.y != 0 {
        rec.y = 0
    } else {
        rec.y = -1
    }

    if rec.x != 0 || rec.y != 0 {
        expandBoard(rec.x, rec.y)
    }
    return true
}

func main() {
    rand.Seed(time.Now().UnixNano())
    initBoard()
    scr, err := gc.Init()
    if err != nil {
        log.Fatal("init", err)
    }
    defer gc.End()
    gc.Echo(false)
    gc.CBreak(true)
    ch := gc.Key(0)
    move := 0
    waitKey := true
    for {
        scr.MovePrintf(0, 0, "Move %d", move)
        move++
        showBoard(scr)
        if !nextMove() {
            nextMove()
            showBoard(scr)
            break
        }
        if !waitKey {
            time.Sleep(100000 * time.Microsecond)
        }
        if ch = scr.GetChar(); ch == ' ' {
            waitKey = !waitKey
            if waitKey {
                scr.Timeout(-1)
            } else {
                scr.Timeout(0)
            }
        }
        if ch == 'q' {
            break
        }
    }
    scr.Timeout(-1)
    gc.CBreak(false)
    gc.Echo(true)
}
```


=={{header|Icon}} and {{header|Unicon}}==
[[File:Morpion 5T92 unicon.PNG|thumb|right|Example of the longest random game produced by this program (92 moves) and displayed using the Pentasol player.]]

The example provided goes beyond the basic requirement to play out a random game.  It provides a flexible framework to explore the challenge of morpion solitaire. 
 
See [[Morpion_solitaire/Unicon]]


## J

With this program as the file m.ijs

```J

NB. turn will be a verb with GRID as y, returning GRID.  Therefor:
NB. morpion is move to the power of infinity---convergence.
morpion =: turn ^: _

NB. Detail:

NB. bitwise manipulation definitions for bit masks.
bit_and =: 2b10001 b.
bit_or =: 2b10111 b.

assert 0 0 0 1 -: 0 0 1 1 bit_and 0 1 0 1
assert 0 1 1 1 -: 0 0 1 1 bit_or  0 1 0 1

diagonal =: (<i.2)&|:  NB. verb to extract the major diagonal of a matrix.
assert 0 3 -: diagonal i. 2 2

NB. choose to pack bits into groups of 3.  3 bits can store 0 through 5.
MASKS =: 'MARKED M000 M045 M090 M135'
(MASKS) =: 2b111 * 2b1000 ^ i. # ;: MASKS

bit_to_set =: 2&}.&.#:

MARK =: bit_to_set MARKED

GREEK_CROSS =: MARK * 10 10 $ 'x' = LF -.~ 0 :0
   xxxx   
   x  x   
   x  x   
xxxx  xxxx
x        x
x        x
xxxx  xxxx
   x  x   
   x  x   
   xxxx   
)

NB. frame pads the marked edges of the GRID
frame_top =: 0&,^:(0 ~: +/@:{.)
frame_bot =: frame_top&.:|.
frame_lef=:frame_top&.:|:
frame_rig=: frame_bot&.:|:
frame =: frame_top@:frame_bot@:frame_lef@:frame_rig
assert (-: frame) 1 1 $ 0
assert (3 3 $ _5 {. 1) (-: frame) 1 1 $ 1

odometer =: (4 $. $.)@:($&1) NB. http://www.jsoftware.com/jwiki/Essays/Odometer
index_matrix =: ($ <"1@:odometer)@:$ NB. the computations work with indexes
assert (1 1 ($ <) 0 0) (-: index_matrix) (i. 1 1)

Note 'adverb Line'
 m is the directional bit mask.
 produces the bitmask with a list of index vectors to make a new line.
 use Line:   (1,:1 5) M000 Line ;._3 index_matrix GRID
 Line is a Boolean take of the result.
 Cuts apply Line to each group of 5.
 However I did not figure out how to make this work without a global variable.
)

NB.         the middle 3 are not
NB.         used in this direction       and   4 are already marked
Line =: 1 :'((((0 = m bit_and +/@:}.@:}:) *. (4 = MARKED bit_and +/))@:,@:({&GRID))y){.<(bit_to_set m)(;,)y'

l000 =: (1,:1 5)&(M000 Line;._3)
l045 =: (1,:5 5) M045 Line@:diagonal;._3 |.
l090 =: (1,:5 1)&(M090 Line;._3)
l135 =: (1,:5 5)&(M135 Line@:diagonal;._3)

NB. find all possible moves
compute_marks =: (l135 , l090 , l045 , l000)@:index_matrix  NB. compute_marks GRID

choose_randomly =: {~ ?@:#
apply =: (({~ }.)~ bit_or (MARK bit_or 0&{::@:[))`(}.@:[)`]}
save =: 4 : '(x) =: y'
move =: (apply~ 'CHOICE' save choose_randomly)~

turn =: 3 : 0
 TURN =: >: TURN
 FI =. GRID =: frame y
 MOVES =: _6[\;compute_marks GRID
 GRID =: MOVES move :: ] GRID
 if. TURN e. OUTPUT do.
  smoutput (":TURN),' TURN {'
  smoutput '  choose among'  ; < MOVES
  smoutput '  selected' ; CHOICE
  smoutput '  framed input & ouput' ; FI ; GRID
  smoutput '}'
 end.
 GRID
)

NB. argument y is a vector of TURN numbers to report detailed output.
play =: 3 : 0
 OUTPUT =: y
 NB. save the random state to replay a fantastic game.
 RANDOM_STATE =: '(9!:42 ; 9!:44 ; 9!:0)' ; (9!:42 ; 9!:44 ; 9!:0)''
 if. 0 < # OUTPUT do.
  smoutput 'line angle bit keys for MARK 000 045 090 135: ',":bit_to_set"0 MARKED,M000,M045,M090,M135
  smoutput 'RANDOM_STATE begins as' ; RANDOM_STATE
 end.
 TURN =: _1 NB. count the number of plays.  Convergence requires 1 extra attempt.
 GRID =: morpion GREEK_CROSS           NB. play the game
 TURN
)

NB. example
smoutput play''

```

load the file into a j session to play an initial game and report the number of turns.  We can play a game providing a vector of move numbers at which to report the output.

```txt

   load'/tmp/m.ijs'
60

   play 3
line angle bit keys for MARK 000 045 090 135: 1 8 64 512 4096
┌──────────────────────┬──────────────────────┬─┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────...
│RANDOM_STATE begins as│(9!:42 ; 9!:44 ; 9!:0)│2│┌─┬──┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────...
│                      │                      │ ││2│73│_1823777002250993298 _6838471509779976446 _8601563932981981704 _9084675764771521463 _513205540226054792 8272574653743672083 _9008275520901665952 542248839568947423 _149618965119662441 _7363052629138270...
│                      │                      │ │└─┴──┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────...
└──────────────────────┴──────────────────────┴─┴───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────...
3 TURN {
┌──────────────┬───────────────────────────────┐
│  choose among│┌────┬────┬────┬────┬────┬────┐│
│              ││4096│1 6 │2 7 │3 8 │4 9 │5 10││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││4096│3 7 │4 8 │5 9 │6 10│7 11││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││4096│6 1 │7 2 │8 3 │9 4 │10 5││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││4096│7 3 │8 4 │9 5 │10 6│11 7││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │0 4 │1 4 │2 4 │3 4 │4 4 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │0 7 │1 7 │2 7 │3 7 │4 7 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │1 4 │2 4 │3 4 │4 4 │5 4 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │1 7 │2 7 │3 7 │4 7 │5 7 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │3 1 │4 1 │5 1 │6 1 │7 1 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │3 10│4 10│5 10│6 10│7 10││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │4 1 │5 1 │6 1 │7 1 │8 1 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │4 10│5 10│6 10│7 10│8 10││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │6 4 │7 4 │8 4 │9 4 │10 4││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││512 │7 4 │8 4 │9 4 │10 4│11 4││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││64  │10 6│9 7 │8 8 │7 9 │6 10││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││64  │5 1 │4 2 │3 3 │2 4 │1 5 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │1 3 │1 4 │1 5 │1 6 │1 7 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │1 4 │1 5 │1 6 │1 7 │1 8 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │4 0 │4 1 │4 2 │4 3 │4 4 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │4 1 │4 2 │4 3 │4 4 │4 5 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │4 6 │4 7 │4 8 │4 9 │4 10││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │4 7 │4 8 │4 9 │4 10│4 11││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │7 0 │7 1 │7 2 │7 3 │7 4 ││
│              │├────┼────┼────┼────┼────┼────┤│
│              ││8   │7 1 │7 2 │7 3 │7 4 │7 5 ││
│              │└────┴────┴────┴────┴────┴────┘│
└──────────────┴───────────────────────────────┘
┌──────────┬───┬───┬───┬───┬───┬───┐
│  selected│512│1 4│2 4│3 4│4 4│5 4│
└──────────┴───┴───┴───┴───┴───┴───┘
┌──────────────────────┬───────────────────────────┬─────────────────────────────┐
│  framed input & ouput│0 0 0 0 0 0 0   0 0 0 0 0 0│0 0 0 0   0 0 0   0 0 0 0 0 0│
│                      │0 0 0 0 1 1 1   1 0 0 0 0 0│0 0 0 0 513 1 1   1 0 0 0 0 0│
│                      │0 0 0 0 1 0 0   1 0 0 0 0 0│0 0 0 0 513 0 0   1 0 0 0 0 0│
│                      │0 0 0 0 1 0 0   1 0 0 0 0 0│0 0 0 0 513 0 0   1 0 0 0 0 0│
│                      │0 1 1 1 1 0 0   1 1 1 1 0 0│0 1 1 1 513 0 0   1 1 1 1 0 0│
│                      │0 1 0 0 0 0 0   0 0 0 1 0 0│0 1 0 0 513 0 0   0 0 0 1 0 0│
│                      │0 1 0 0 0 0 0   0 0 0 1 0 0│0 1 0 0   0 0 0   0 0 0 1 0 0│
│                      │0 1 1 1 1 0 0 521 9 9 9 9 0│0 1 1 1   1 0 0 521 9 9 9 9 0│
│                      │0 0 0 0 1 0 0 513 0 0 0 0 0│0 0 0 0   1 0 0 513 0 0 0 0 0│
│                      │0 0 0 0 1 0 0 513 0 0 0 0 0│0 0 0 0   1 0 0 513 0 0 0 0 0│
│                      │0 0 0 0 9 9 9 521 9 0 0 0 0│0 0 0 0   9 9 9 521 9 0 0 0 0│
│                      │0 0 0 0 0 0 0 513 0 0 0 0 0│0 0 0 0   0 0 0 513 0 0 0 0 0│
│                      │0 0 0 0 0 0 0   0 0 0 0 0 0│0 0 0 0   0 0 0   0 0 0 0 0 0│
└──────────────────────┴───────────────────────────┴─────────────────────────────┘
}
62

```

Explanation.

load'/tmp/m.ijs'  Load the file played an initial game.  This one played 60 moves.

play 3  Shows the state of the random generator at the start of the game, and then information about turn 3.
The pseudo-random generator can be reconstructed from information in the RANDOM_STATE variable, hence one can replay with full output superior games.

Curly braces enclose output pertaining to the move transitioning from given state to the next.

```txt
3 TURN {
  ...
}

```

A list of the possible moves follows, along with the selection.  Let's decode the selected move.
Given the key from first output line the move 512 is a 90 degree (vertical) line.  The list of index origin 0 row column coordinates indeed shows 5 constant column with sequential rows.  From the framed input and output grids shown, near the top of the fifth column, the 1 1 1 1 0 became 513 513 513 513 513.  513 is the number corresponding to one bits of MARK and 90 degrees.  On a prior move, the 521's shows that thes marked points were used for 0 and 90 degree lines, included in the (difficult to see) 9's and 513's in proper direction.  The final 62 shows the length of the game.  Display the value of final grid with the sentence GRID .  GRID is a pronoun.


```txt

line angle bit keys for MARK 000 045 090 135: 1 8 64 512 4096

┌──────────┬───┬───┬───┬───┬───┬───┐
│  selected│512│1 4│2 4│3 4│4 4│5 4│
└──────────┴───┴───┴───┴───┴───┴───┘
┌──────────────────────┬───────────────────────────┬─────────────────────────────┐
│  framed input & ouput│0 0 0 0 0 0 0   0 0 0 0 0 0│0 0 0 0   0 0 0   0 0 0 0 0 0│
│                      │0 0 0 0 1 1 1   1 0 0 0 0 0│0 0 0 0 513 1 1   1 0 0 0 0 0│
│                      │0 0 0 0 1 0 0   1 0 0 0 0 0│0 0 0 0 513 0 0   1 0 0 0 0 0│
│                      │0 0 0 0 1 0 0   1 0 0 0 0 0│0 0 0 0 513 0 0   1 0 0 0 0 0│
│                      │0 1 1 1 1 0 0   1 1 1 1 0 0│0 1 1 1 513 0 0   1 1 1 1 0 0│
│                      │0 1 0 0 0 0 0   0 0 0 1 0 0│0 1 0 0 513 0 0   0 0 0 1 0 0│
│                      │0 1 0 0 0 0 0   0 0 0 1 0 0│0 1 0 0   0 0 0   0 0 0 1 0 0│
│                      │0 1 1 1 1 0 0 521 9 9 9 9 0│0 1 1 1   1 0 0 521 9 9 9 9 0│
│                      │0 0 0 0 1 0 0 513 0 0 0 0 0│0 0 0 0   1 0 0 513 0 0 0 0 0│
│                      │0 0 0 0 1 0 0 513 0 0 0 0 0│0 0 0 0   1 0 0 513 0 0 0 0 0│
│                      │0 0 0 0 9 9 9 521 9 0 0 0 0│0 0 0 0   9 9 9 521 9 0 0 0 0│
│                      │0 0 0 0 0 0 0 513 0 0 0 0 0│0 0 0 0   0 0 0 513 0 0 0 0 0│
│                      │0 0 0 0 0 0 0   0 0 0 0 0 0│0 0 0 0   0 0 0   0 0 0 0 0 0│
└──────────────────────┴───────────────────────────┴─────────────────────────────┘
}

```

The distribution of 4444 games is strongly bimodal with a narrow peak around 22 moves, and a broader peak of same count at 65 moves.  The longest game scored 81, and 120 minimum 20 move games found.


## Java

See: [[Morpion solitaire/Java]]


## Phix

Focuses on playing back the 178-record, see: [[Morpion solitaire/Phix]]


## Racket



```racket
#lang racket
(module rules racket/base
  (require racket/match)
  
  (provide game-cross
           available-lines
           add-line
           line-dx.dy)
  
  (define (add-points points# x y . more)
    (define p+ (hash-set points# (cons x y) #t))
    (if (null? more) p+ (apply add-points p+ more)))
  
  ;; original cross
  (define (game-cross)
    (let ((x1 (for/fold ((x (hash))) ((i (in-range 3 7)))
                (add-points x 0 i i 0 9 i i 9))))
      (for/fold ((x x1)) ((i (in-sequences (in-range 0 4) (in-range 6 10))))
        (add-points x 3 i i 3 6 i i 6))))
  
  ;; add an edge
  (define (make-edge points#)
    (for*/hash ((k (in-hash-keys points#))
                (dx (in-range -1 2))
                (dy (in-range -1 2))
                (x (in-value (+ (car k) dx)))
                (y (in-value (+ (cdr k) dy)))
                (e (in-value (cons x y)))
                #:unless (hash-has-key? points# e))
      (values e #t)))
  
  (define (line-dx.dy d)
    (values (match d ['w -1] ['nw -1] ['n 0] [ne 1])
            (match d ['n -1] ['ne -1] ['nw -1] ['w 0])))
  
  (define (line-points e d)
    (define-values (dx dy) (line-dx.dy d))
    (match-define (cons x y) e)
    (for/list ((i (in-range 5)))
      (cons (+ x (* dx i))
            (+ y (* dy i)))))
  
  (define (line-overlaps? lp d l#)
    (for/first ((i (in-range 3))
                (p (in-list (cdr lp)))
                #:when (hash-has-key? l# (cons d p)))
      #t))
  
  (define (four-points? lp p#)
    (= 4 (for/sum ((p (in-list lp)) #:when (hash-has-key? p# p)) 1)))
  
  ;; returns a list of lines that can be applied to the game
  (define (available-lines p# l# (e# (make-edge p#)))
    (for*/list ((ep (in-sequences (in-hash-keys e#) (in-hash-keys p#)))
                (d (in-list '(n w ne nw)))
                (lp (in-value (line-points ep d)))
                #:unless (line-overlaps? lp d l#)
                #:when (four-points? lp p#))
      (define new-edge-point (for/first ((p (in-list lp)) #:when (hash-ref e# p #f)) p))
      (list ep d lp new-edge-point)))
  
  ;; adds a new line to points# lines# returns (values [new points#] [new lines#])
  (define (add-line line points# lines#)
    (match-define (list _ dir ps _) line)
    (for/fold ((p# points#) (l# lines#)) ((p (in-list ps)))
      (values (hash-set p# p #t) (hash-set l# (cons dir p) #t)))))

(module player racket/base
  (require racket/match
           (submod ".." rules))

  (provide play-game
           random-line-chooser)
  
  (define (random-line-chooser p# l# options)
    (list-ref options (random (length options))))
  
  ;; line-chooser (points lines (Listof line) -> line)
  (define (play-game line-chooser (o# (game-cross)))
    (let loop ((points# o#)
               (lines# (hash))
               (rv null))
      (match (available-lines points# lines#)
        [(list) (values points# (reverse rv) o#)]
        [options
         (match-define (and chosen-one (list (cons x y) d _ new-edge-point))
           (line-chooser points# lines# options))
         (define-values (p# l#) (add-line chosen-one points# lines#))
         (loop p# l# (cons (vector x y d new-edge-point) rv))]))))

;; [Render module code goes here]

(module main racket/base
  (require (submod ".." render)
           (submod ".." player)
           pict
           racket/class)
  (define p (call-with-values (λ () (play-game random-line-chooser)) render-state))
  p
  (define bmp (pict->bitmap p))
  (send bmp save-file "images/morpion.png" 'png))
```



'''Intermission:''' The <code>render</code> submodule just does drawing, and is not part of the solving. But the <code>main</code> module uses it, so we put it in here:


```racket
(module render racket
  (require racket/match
           racket/draw
           pict
          (submod ".." rules))
  (provide display-state
           render-state)
  
  (define (min/max-point-coords p#)
    (for/fold ((min-x #f) (min-y #f) (max-x #f) (max-y #f))
              ((k (in-hash-keys p#)))
      (match-define (cons x y) k)
      (if min-x
          (values (min min-x x) (min min-y y) (max max-x x) (max max-y y))
          (values x y x y))))
  
  (define (draw-text/centered dc x y t ->x ->y)
    (define-values (w h b v) (send dc get-text-extent t))
    (send dc draw-text t (- (->x x) (* w 1/2)) (- (->y y) (* h 1/2))))

  (define ((with-stored-dc-context draw-fn) dc w h)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    (define old-font (send dc get-font))
    (draw-fn dc w h)
    (send* dc (set-brush old-brush) (set-pen old-pen) (set-font old-font)))

  (define red-brush (new brush% [style 'solid] [color "red"]))
  (define white-brush (new brush% [style 'solid] [color "white"]))
  (define cyan-brush (new brush% [style 'solid] [color "cyan"]))
  (define cyan-pen (new pen% [color "cyan"]))
  (define black-pen (new pen% [color "black"]))
  (define green-pen (new pen% [color "green"] [width 3]))
  (define black-brush (new brush% [style 'solid] [color "black"]))
      
  (define (render-state p# ls (o# (hash)))
    (define-values (min-x min-y max-x max-y) (min/max-point-coords p#))    
    (define C 24)
    (define R  8)
    (define D (* R 2))
    (define Rp 4)

    (define (draw dc w h)
      (define (->x x) (* C (- x min-x -1/2)))
      (define (->y y) (* C (- y min-y -1/2 )))
      (send dc set-brush cyan-brush)
      (send dc set-pen cyan-pen)
      (send dc set-font (make-object font% R 'default))
      
      (for ((y (in-range min-y (add1 max-y))))
        (send dc draw-line (->x min-x) (->y y) (->x max-x) (->y y))
        (for ((x (in-range min-x (add1 max-x))))
          (send dc draw-line (->x x) (->y min-y) (->x x) (->y max-y))))
      
      (send dc set-pen black-pen)
      (for ((l (in-list ls)))
        (match-define (vector x y d (cons ex ey)) l)
        (define-values (dx dy) (line-dx.dy d))
        (define x1 (+ x (* 4 dx)))
        (define y1 (+ y (* 4 dy)))
        (send* dc (draw-line (->x x) (->y y) (->x x1) (->y y1))))
      
      (for* ((y (in-range min-y (add1 max-y)))
             (x (in-range min-x (add1 max-x))))
        (define k (cons x y))
        (cond [(hash-has-key? o# k)
               (send dc set-brush red-brush)
               (send dc draw-ellipse (- (->x x) R) (- (->y y) R) D D)]
              [(hash-has-key? p# k)
               (send dc set-brush white-brush)
               (send dc draw-ellipse (- (->x x) R) (- (->y y) R) D D)]))
      
      (send dc set-brush black-brush)
      (for ((l (in-list ls))
            (i (in-naturals 1)))
        (match-define (vector _ _ d (cons ex ey)) l)
        (define-values (dx dy) (line-dx.dy d))
        (define R.dx (* R dx 0.6))
        (define R.dy (* R dy 0.6))
        (send* dc
          (set-pen green-pen)
          (draw-line (- (->x ex) R.dx) (- (->y ey) R.dy) (+ (->x ex) R.dx) (+ (->y ey) R.dy))
          (set-pen black-pen))
        (draw-text/centered dc ex ey (~a i) ->x ->y)))
    
    (define P (dc (with-stored-dc-context draw) (* C (- max-x min-x -1)) (* C (- max-y min-y -1))))
    (printf "~s~%~a points ~a lines~%" ls (hash-count p#) (length ls))
    P)
  
  (define (display-state p# l (o# (hash)))
    (define-values (min-x min-y max-x max-y) (min/max-point-coords p#))
    (for ((y (in-range min-y (add1 max-y)))
          #:when (unless (= y min-y) (newline))
          (x (in-range min-x (add1 max-x))))
      (define k (cons x y))
      (write-char
       (cond [(hash-has-key? o# k) #\+]
             [(hash-has-key? p# k) #\.]
             [else #\space])))
    (printf "~s~%~a points ~a lines~%" l (hash-count p#) (length l))))
```

{{out}}

[[File:Morpion racket.png|thumb|right|The Racket rendition of the output solution]]

Here is the text output of one run, and if you're (I'm) lucky, there's a picture attached:

```txt

(#(9 6 n (9 . 2)) #(4 3 w (4 . 3)) #(7 9 w (7 . 9)) #(8 3 w (5 . 3)) #(3 9 n (3 . 5))
 #(0 7 n (0 . 7)) #(6 3 n (6 . -1)) #(7 0 w (7 . 0)) #(3 3 n (3 . -1)) #(4 6 w (4 . 6))
 #(2 6 ne (4 . 4)) #(6 9 n (6 . 5)) #(0 4 ne (2 . 2)) #(9 4 nw (7 . 2)) #(8 6 w (5 . 6))
 #(4 9 nw (2 . 7)) #(7 9 nw (5 . 7)) #(7 6 nw (5 . 4)) #(2 7 ne (4 . 5)) #(7 3 nw (5 . 1))
 #(5 7 n (5 . 5)) #(7 5 w (7 . 5)) #(5 6 ne (7 . 4)) #(6 7 nw (3 . 4)) #(0 7 ne (2 . 5))
 #(7 7 nw (7 . 7)) #(6 8 ne (10 . 4)) #(2 6 n (2 . 4)) #(5 7 ne (8 . 4)) #(5 4 w (1 . 4))
 #(1 4 ne (4 . 1)) #(7 7 w (4 . 7)) #(4 9 n (4 . 8)) #(7 4 n (7 . 1)) #(7 4 nw (5 . 2))
 #(11 4 w (11 . 4)) #(7 9 n (7 . 8)) #(5 3 n (5 . -1)) #(7 2 w (4 . 2)) #(8 6 nw (6 . 4))
 #(7 8 w (5 . 8)) #(3 10 ne (3 . 10)) #(5 9 nw (1 . 5)) #(4 3 ne (8 . -1))
 #(-1 7 ne (-1 . 7)) #(1 6 n (1 . 2)) #(6 1 w (2 . 1)) #(10 4 nw (8 . 2)) #(3 5 w (-1 . 5))
 #(8 6 n (8 . 5)) #(-1 4 ne (-1 . 4)) #(5 5 ne (9 . 1)) #(3 6 nw (-1 . 2)) #(3 3 ne (7 . -1))
 #(7 -1 w (4 . -1)) #(7 10 nw (7 . 10)) #(3 2 w (0 . 2)) #(3 5 nw (-1 . 1)) #(-1 5 n (-1 . 3))
 #(3 7 w (1 . 7)) #(3 9 nw (2 . 8)) #(1 9 ne (1 . 9)) #(4 2 n (4 . -2)))
99 points 63 lines

```



## REXX

This REXX program is an attempt to play (badly, and with random moves) the game of Morpion solitaire by a computer.


The program also allows a carbon-based life form (er, that is, a human) to play.


This is a work in progress and currently doesn't log the moves in the manner asked for by this task.

The moves are marked by ''' 0123456789ABC...XYZabc...xyz()[]{}<>«» ''' and thereafter by a plus sign (+) on the board which is shown in 2D.

This allows 73 unique moves to be shown on the board (or grid), but all moves are also logged to a file.

Currently, the computer tries to start the game (with sixteen moves) by the assumptions I made, which clearly aren't worth a tinker's dam.

This program allows the <tt> D </tt> or <tt> T </tt> forms of the game, and allows any board size (grid size) of three or higher. 

The default games is <tt> 5T </tt> 


```rexx
/*REXX program to play Morpion solitaire, the default is the 5T version.*/
signal on syntax; signal on novalue    /*handle REXX program errors.    */
quiet=0; oFID='MORPION'
arg game player .                      /*see if a person wants to play. */
if game=='' | game==',' then game='5T' /*Not specified? Then use default*/
prompt=                                /*null string is used for ERR ret*/
TorD='T (touching) ───or─── D (disjoint).' /*valid games types  (T | D).*/
gT=right(game,1)                       /*T = touching ─or─ D = disjoint.*/
if \datatype(gT,'U') | verify(gT,gT)\==0 then call err 'game gT not' gT
gS=left(game,length(game)-1)           /*gS=Game Size (line len for win)*/
if \datatype(gS,'W') then call err "game size isn't numeric:" gS
gS=gS/1
if gS<3 then call err "grid size is too small:" gS
sw=linesize()-1
indent=left('',max(0,sw-gS-10)%2)      /*indentation used board display.*/
empty='fa'x                            /*the empty grid point symbol.   */
@.=empty                               /*field  (grid) is infinite.     */
gC=                                    /*GreeK cross character or null. */
CBLF=player\==''                       /*carbon-based lifeform ?        */
if CBLF then oFID=player               /*oFID is used for the game log. */
oFID=oFID'.LOG'                        /*fulltype for the LOG's filename*/
prompt='enter X,Y point and an optional character for placing on board',
       '(or Quit):';  prompt=right(prompt,sw,'─')    /*right justify it.*/
call GreekCross
jshots=Gshots

  do turns=1 for 1000
  if CBLF then do
               call t prompt;   pull stuff;   stuff=translate(stuff,,',')
               parse var stuff px py p
               _=px;  upper _;   if abbrev('QUIT',_,1) then exit
               if stuff=='' then do;   call display;   iterate;   end
               call mark px,py
               end   /*if CBLF*/
          else do;  quiet=1
               shot=translate(word(Gshots,turn),,',')
               if shot=='' then do 50
                                xr=loX-1+random(0,hiX-loX+2)
                                yr=loY-1+random(0,hiY-loY+2)
                                if @.xr.yr\==empty then iterate
                                if \neighbor(xr,yr) then iterate
                                shot=xr yr
                                end
               call mark word(shot,1),word(shot,2)
               end
   end   /*turns*/

call t '* number of wins =' wins
exit wins                              /*stick a fork in it, we're done.*/
/*───────────────────────────────error handling subroutines and others.─*/
err: if \quiet then do;   call t;   call t
                    call t center(' error! ',max(40,linesize()%2),"*"); call t
                       do j=1 for arg(); call t arg(j); call t; end; call t
                    end
     if prompt=='' then exit 13;   return

novalue: syntax:   prompt=;   quiet=0
         call err 'REXX program' condition('C') "error",,
             condition('D'),'REXX source statement (line' sigl"):",,
             sourceline(sigl)

t:  say arg(1);   call lineout oFID,arg(1);   return
Gshot:   Gshots=Gshots arg(1)','arg(2);       return
tranGC: if gC=='' then return arg(1); return translate(arg(1),copies(gC,12),'┌┐└┘│─╔╗╚╝║═')
/*─────────────────────────────────────GREEKCROSS subroutine────────────*/
GreekCross:    wins=0;     loX=-1;      hiX=0;   LB=gS-1       /*Low Bar*/
 lintel=LB-2;  turn=1;     loY=-1;      hiY=0;   ht=4+3*(LB-2) /*─   ─  */
 Gshots=;      nook=gS-2;     Hnook=ht-nook+1;   TB=ht-LB+1    /*Top Bar*/
                                                               /*─   ─  */
  do y=1 for ht; _top='╔'copies('═',lintel)'╗'                          ; _top=tranGC(_top)
                 _bot='╚'copies('═',lintel)'╝'                          ; _bot=tranGC(_bot)
                 _hib='╔'copies('═',lintel)'╝'left('',lintel)'╚'copies('═',lintel)'╗' ; _hib=tranGC(_hib)
                 _lob='╚'copies('═',lintel)'╗'left('',lintel)'╔'copies('═',lintel)'╝' ; _lob=tranGC(_lob)
                 _sid='║'                                               ; _sid=tranGC(_sid)
    select
    when y==1  then do x=1  for LB; call place x+LB-1,y,substr(_bot,x,1); end
    when y==ht then do x=1  for LB; call place x+LB-1,y,substr(_top,x,1); end
    when y==LB then do x=1  for ht; if x>LB & x<TB then iterate; call place x,y,substr(_lob,x,1); end
    when y==TB then do x=1  for ht; if x>LB & x<TB then iterate; call place x,y,substr(_hib,x,1); end
    when y>LB & y<TB then do x=1  by ht-1  for 2; call place x,y,_sid; end
    otherwise             do x=LB by TB-LB for 2; call place x,y,_sid; end
    end   /*select*/
  end     /*y*/

@abc='abcdefghijklmnopqrstuvwxyz'; @chars='0123456789'translate(@abc)||@abc
@chars=@chars'()[]{}<>«»'              /*can't contain "empty", ?, blank*/

call display
call Gshot  nook  , nook  ;      call Gshot  nook  , Hnook
call Gshot  Hnook , nook  ;      call Gshot  Hnook , Hnook
call Gshot     gS , LB    ;      call Gshot     gS ,  TB
call Gshot  ht-LB , LB    ;      call Gshot  ht-LB ,  TB
call Gshot     LB , gS    ;      call Gshot     TB ,  gS
call Gshot     LB , TB-1  ;      call Gshot     TB ,  TB-1
call Gshot      1 , TB+1  ;      call Gshot     ht ,  TB+1
call Gshot   TB+1 , 1     ;      call Gshot   TB+1 ,  ht
return
/*─────────────────────────────────────DISPLAY subroutine───────────────*/
display: call t;  do y=hiY to loY by -1; _=indent   /*start at a high Y.*/
                              do x=loX to hiX       /*build an "X" line.*/
                              !=@.x.y;  xo=x==0;  yo=y==0
                              if !==empty then do  /*grid transformation*/
                                               if xo            then !='|'
                                               if xo & y//5 ==0 then !='├'
                                               if xo & y//10==0 then !='╞'
                                               if yo            then !='─'
                                               if yo & x//5 ==0 then !='┴'
                                               if yo & x//10==0 then !='╨'
                                               if xo & yo       then !='┼'
                                               end
                              _=_ || !
                              end  /*x*/
                  call t _                          /*...and display it.*/
                  end              /*y*/

if wins==0 then call t copies('═',sw)
           else call t right('count of (above) wins =' wins,sw,'═')
call t
return
/*─────────────────────────────────────PLACE subroutine─────────────────*/
place: parse arg xxp,yyp               /*place a marker (point) on grid.*/
loX=min(loX,xxp);   hiX=max(hiX,xxp)
loY=min(loY,yyp);   hiY=max(hiY,yyp);      @.xxp.yyp=arg(3)
return
/*─────────────────────────────────────MARK subroutine──────────────────*/
mark: parse arg xx,yy,pointChar        /*place marker, check for errors.*/
if pointChar=='' then pointChar=word(substr(@chars,turn,1) '+',1)
xxcyy=xx','yy;   _.1=xx;   _.2=yy

  do j=1 for 2;  XorY=substr('XY',j,1) /*make sure X and Y are integers.*/
  if _.j==''            then do;  call err XorY "wasn't specified."    ; return 0; end
  if \datatype(_.j,'N') then do;  call err XorY "isn't numeric:" _.j   ; return 0; end
  if \datatype(_.j,'W') then do;  call err XorY "isn't an integer:" _.j; return 0; end
  end

xx=xx/1;   yy=yy/1                     /*normalize integers:  + 7 or 5.0*/

if pointChar==empty |,
   pointChar=='?'   then do;  call err 'illegal point character:' pointChar; return 0; end
if @.xx.yy\==empty  then do;  call err 'point' xxcyy 'is already occupied.'; return 0; end
if \neighbor(xx,yy) then do;  call err "point" xxcyy "is a bad move."      ; return 0; end
call place xx,yy,'?'
newWins=countWins()
if newWins==0 then do;        call err "point" xxcyy "isn't a good move."
                   @.xx.yy=empty
                   return 0
                   end
call t "move" turn '  ('xx","yy')   with "'pointChar'"'
wins=wins+newWins;   @.xx.yy=pointChar;   call display;   turn=turn+1
return 1
/*─────────────────────────────────────NEIGHBOR subroutine──────────────*/
neighbor:  parse arg a,b;    am=a-1;   ap=a+1
                             bm=b-1;   bp=b+1
return   @.am.b  \== empty   |   @.am.bm \== empty |,
         @.ap.b  \== empty   |   @.am.bp \== empty |,
         @.a.bm  \== empty   |   @.ap.bm \== empty |,
         @.a.bp  \== empty   |   @.ap.bp \== empty
/*─────────────────────────────────────COUNTALINE subroutine────────────*/
countAline:  arg z  ; L=length(z)

if L>gS then do;   if gT=='D' then return 0   /*longlines ¬ kosker for D*/
             parse var z z1 '?' z2            /*could be   xxxxx?xxxx   */
             return length(z1)==4 | length(z2)==4
             end
return L==gS
/*─────────────────────────────────────COUNTWINS subroutine─────────────*/
countWins: eureka=0;   y=yy   /*count horizontal/vertical/diagonal wins.*/
z=@.xx.yy
     do x=xx+1;              if @.x.y==empty then leave;   z=z||@.x.y;   end
     do x=xx-1 by -1;        if @.x.y==empty then leave;   z=@.x.y||z;   end
eureka=eureka+countAline(z)   /*─────────count wins in horizontal line. */

x=xx
z=@.xx.yy
     do y=yy+1;              if @.x.y==empty then leave;   z=z||@.x.y;   end
     do y=yy-1 by -1;        if @.x.y==empty then leave;   z=@.x.y||z;   end
eureka=eureka+countAline(z)   /*─────────count wins in vertical line.   */

x=xx
z=@.xx.yy
     do y=yy+1; x=x+1;       if @.x.y==empty then leave;   z=z||@.x.y;   end
x=xx
     do y=yy-1 by -1; x=x-1; if @.x.y==empty then leave;   z=@.x.y||z;   end
eureka=eureka+countAline(z)   /*───────count diag wins:  up&>,  down&<  */

x=xx
z=@.xx.yy
     do y=yy+1; x=x-1;       if @.x.y==empty then leave;   z=z||@.x.y;   end
x=xx
     do y=yy-1 by -1; x=x+1; if @.x.y==empty then leave;   z=z||@.x.y;   end
return eureka+countAline(z)   /*───────count diag wins:  up&<, down&>   */
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console). 

The   '''LINESIZE.REX'''   REXX program is included here ──► [[LINESIZE.REX]].


'''output''' when running 1,500 trials, the highest win was a meager 44 (four games, all different), and 
one of them is shown below.
<pre style="height:100ex">
                                ·╞···╔══╗···
                                ·|···║··║···
                                ·|···║··║···
                                ·|╔══╝··╚══╗
                                ·|║········║
                                ·├║········║
                                ·|╚══╗··╔══╝
                                ·|···║··║···
                                ·|···║··║···
                                ·|···╚══╝···
                                ─┼────┴────╨
                                ·|··········
═══════════════════════════════════════════════════════════════════════════════

move 1   (3,3)   with "0"
                                  ...  previous 46 moves elided  ...  above is the initial board (grid)  ...
                                  ---  the next line means: 47th move,   position=9,9    marked with an "k"  ---
move 47   (9,9)   with "k"

                               ·|············
                               ·|··iQagP·····
                               ·╞j·d╔══╗F····
                               ·|·hO║NL║ck···
                               ·|CZ1║bK║3MD··
                               ·X╔══╝57╚══╗f·
                               ·|║YHASGBJR║··
                               ·├║UT8I·9·e║··
                               ·|╚══╗46╔══╝··
                               ·V··0║W·║2····
                               ·|···║··║·····
                               ·|···╚══╝E····
                               ─┼────┴────╨──
                               ·|············ 
             
═════════════════════════════════════════════════════ count of (above) wins = 47
 
* number of wins = 47

```

