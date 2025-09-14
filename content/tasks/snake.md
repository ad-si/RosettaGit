+++
title = "Snake"
description = ""
date = 2019-05-01T22:19:45Z
aliases = []
[extra]
id = 20698
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "basic",
  "cpp",
  "go",
  "haskell",
  "java",
  "javascript",
  "kotlin",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "sidef",
  "zx_spectrum_basic",
]
+++

[[wp:Snake_(video_game)|Snake]] is a game where the player maneuvers a line which grows in length every time the snake reaches a food source.

## Task

Implement a variant of the Snake game, in any interactive environment, in which a sole player attempts to eat items by running into them with the head of the snake. Each item eaten makes the snake longer and a new item is randomly generated somewhere else on the plane. The game ends when the snake attempts to eat himself.


## BASIC

=
## ZX Spectrum Basic
=
By ancient tradition, the controls are Q for up, A for down, O for left, and P for right.

A screenshot is [http://www.edmundgriffiths.com/spectrumsnake.jpg here].

Note that lines <code>10</code> to <code>210</code> and <code>580</code> to <code>890</code>—more than half the program—define graphics characters for the snake's head (facing in different directions) and for its food. If you're happy to make do with characters from the standard character set, you can easily adapt lines <code>220</code> to <code>570</code> to work on their own. The things the snake eats are supposed to be apples, although they don't look too much like them.

```zxbasic
 10 FOR i=0 TO 7
 20 READ bits
 30 POKE USR "L"+i,bits
 40 NEXT i
 50 FOR i=0 TO 7
 60 READ bits
 70 POKE USR "R"+i,bits
 80 NEXT i
 90 FOR i=0 TO 7
100 READ bits
110 POKE USR "P"+i,bits
120 NEXT i
130 RESTORE 740
140 FOR i=7 TO 0 STEP -1
150 READ bits
160 POKE USR "D"+i,bits
170 NEXT i
180 FOR i=0 TO 7
190 READ bits
200 POKE USR "F"+i, bits
210 NEXT i
220 PAPER 0
230 CLS
240 LET snakex=19
250 LET snakey=15
260 LET dx=-1
270 LET dy=0
280 LET s$=CHR$ 15+CHR$ 20+CHR$ 15+CHR$ 21
290 LET foodx=INT (RND*32)
300 LET foody=INT (RND*22)
310 IF SCREEN$ (foody,foodx)<>" " THEN GO TO 290
320 INK 2
330 PRINT AT foody,foodx;CHR$ 149
340 INK 4
350 INVERSE 1
360 PRINT AT CODE s$,CODE s$(1);"#"
370 INVERSE 0
380 IF INKEY$="q" AND dy=0 THEN LET dx=0: LET dy=-1
390 IF INKEY$="a" AND dy=0 THEN LET dx=0: LET dy=1
400 IF INKEY$="o" AND dx=0 THEN LET dx=-1: LET dy=0
410 IF INKEY$="p" AND dx=0 THEN LET dx=1: LET dy=0
420 IF dx=-1 THEN PRINT AT snakey,snakex;CHR$ 155
430 IF dx=1 THEN PRINT AT snakey,snakex;CHR$ 161
440 IF dy=1 THEN PRINT AT snakey,snakex;CHR$ 159
450 IF dy=-1 THEN PRINT AT snakey,snakex;CHR$ 147
460 LET s$=CHR$ snakey+CHR$ snakex+s$
470 IF snakex=foodx AND snakey=foody THEN GO TO 290
480 PRINT AT CODE s$(LEN s$-1),CODE s$(LEN s$);" "
490 LET s$=s$( TO LEN s$-2)
500 LET snakex=snakex+dx
510 LET snakey=snakey+dy
520 IF snakex=-1 THEN LET snakex=31
530 IF snakex=32 THEN LET snakex=0
540 IF snakey=-1 THEN LET snakey=21
550 IF snakey=22 THEN LET snakey=0
560 IF SCREEN$ (snakey,snakex)="#" THEN STOP
570 GO TO 340
580 DATA BIN 00001111
590 DATA BIN 00111111
600 DATA BIN 01110011
610 DATA BIN 11110011
620 DATA BIN 11111111
630 DATA BIN 01111111
640 DATA BIN 00000111
650 DATA BIN 00011111
660 DATA BIN 11110000
670 DATA BIN 11111100
680 DATA BIN 11001110
690 DATA BIN 11001111
700 DATA BIN 11111111
710 DATA BIN 11111110
720 DATA BIN 11100000
730 DATA BIN 11111000
740 DATA BIN 00011000
750 DATA BIN 00111100
760 DATA BIN 01111100
770 DATA BIN 01111101
780 DATA BIN 11001101
790 DATA BIN 11001111
800 DATA BIN 11111111
810 DATA BIN 11111111
820 DATA BIN 00000100
830 DATA BIN 00001000
840 DATA BIN 01101011
850 DATA BIN 11111100
860 DATA BIN 11111100
870 DATA BIN 11111100
880 DATA BIN 01111111
890 DATA BIN 00110110
```



## C++

Simple Windows console implementation.
[[File:SnakeCpp.png|200px|thumb|right]]

```cpp

#include <windows.h>
#include <ctime>
#include <iostream>
#include <string>

const int WID = 60, HEI = 30, MAX_LEN = 600;
enum DIR { NORTH, EAST, SOUTH, WEST };

class snake {
public:
    snake() {
        console = GetStdHandle( STD_OUTPUT_HANDLE ); SetConsoleTitle( "Snake" ); 
        COORD coord = { WID + 1, HEI + 2 }; SetConsoleScreenBufferSize( console, coord );
        SMALL_RECT rc = { 0, 0, WID, HEI + 1 }; SetConsoleWindowInfo( console, TRUE, &rc );
        CONSOLE_CURSOR_INFO ci = { 1, false }; SetConsoleCursorInfo( console, &ci );
    }
    void play() {
        std::string a;
        while( 1 ) {
            createField(); alive = true;
            while( alive ) { drawField(); readKey(); moveSnake(); Sleep( 50 ); }
            COORD c = { 0, HEI + 1 }; SetConsoleCursorPosition( console, c );
            SetConsoleTextAttribute( console, 0x000b );
            std::cout << "Play again [Y/N]? "; std::cin >> a;
            if( a.at( 0 ) != 'Y' && a.at( 0 ) != 'y' ) return;
        }
    }
private:
    void createField() {
        COORD coord = { 0, 0 }; DWORD c;
        FillConsoleOutputCharacter( console, ' ', ( HEI + 2 ) * 80, coord, &c );
        FillConsoleOutputAttribute( console, 0x0000, ( HEI + 2 ) * 80, coord, &c );
        SetConsoleCursorPosition( console, coord );
        int x = 0, y = 1; for( ; x < WID * HEI; x++ ) brd[x] = 0;
        for( x = 0; x < WID; x++ ) {
            brd[x] = brd[x + WID * ( HEI - 1 )] = '+';
        }
        for( ; y < HEI; y++ ) {
            brd[0 + WID * y] = brd[WID - 1 + WID * y] = '+';
        }
        do {
            x = rand() % WID; y = rand() % ( HEI >> 1 ) + ( HEI >> 1 );
        } while( brd[x + WID * y] );
        brd[x + WID * y] = '@';
        tailIdx = 0; headIdx = 4; x = 3; y = 2;
        for( int c = tailIdx; c < headIdx; c++ ) {
            brd[x + WID * y] = '#';
            snk[c].X = 3 + c; snk[c].Y = 2;
        }
        head = snk[3]; dir = EAST; points = 0;
    }
    void readKey() {
        if( GetAsyncKeyState( 39 ) & 0x8000 ) dir = EAST;
        if( GetAsyncKeyState( 37 ) & 0x8000 ) dir = WEST;
        if( GetAsyncKeyState( 38 ) & 0x8000 ) dir = NORTH;
        if( GetAsyncKeyState( 40 ) & 0x8000 ) dir = SOUTH;
    }
    void drawField() {
        COORD coord; char t;
        for( int y = 0; y < HEI; y++ ) {
            coord.Y = y;
            for( int x = 0; x < WID; x++ ) {
                t = brd[x + WID * y]; if( !t ) continue;
                coord.X = x; SetConsoleCursorPosition( console, coord );
                if( coord.X == head.X && coord.Y == head.Y ) {
                    SetConsoleTextAttribute( console, 0x002e );
                    std::cout << 'O'; SetConsoleTextAttribute( console, 0x0000 );
                    continue;
                }
                switch( t ) {
                    case '#': SetConsoleTextAttribute( console, 0x002a ); break;
                    case '+': SetConsoleTextAttribute( console, 0x0019 ); break;
                    case '@': SetConsoleTextAttribute( console, 0x004c ); break;
                }
                std::cout << t; SetConsoleTextAttribute( console, 0x0000 );
            }
        }
        std::cout << t; SetConsoleTextAttribute( console, 0x0007 );
        COORD c = { 0, HEI }; SetConsoleCursorPosition( console, c );
        std::cout << "Points: " << points;
    }
    void moveSnake() {
        switch( dir ) {
            case NORTH: head.Y--; break;
            case EAST: head.X++; break;
            case SOUTH: head.Y++; break;
            case WEST: head.X--; break;
        }
        char t = brd[head.X + WID * head.Y];
        if( t && t != '@' ) { alive = false; return; }
        brd[head.X + WID * head.Y] = '#';
        snk[headIdx].X = head.X; snk[headIdx].Y = head.Y;
        if( ++headIdx >= MAX_LEN ) headIdx = 0;
        if( t == '@' ) {
            points++; int x, y;
            do {
                x = rand() % WID; y = rand() % ( HEI >> 1 ) + ( HEI >> 1 );
            } while( brd[x + WID * y] );
            brd[x + WID * y] = '@'; return;
        }
        SetConsoleCursorPosition( console, snk[tailIdx] ); std::cout << ' ';
        brd[snk[tailIdx].X + WID * snk[tailIdx].Y] = 0;
        if( ++tailIdx >= MAX_LEN ) tailIdx = 0;
    }
    bool alive; char brd[WID * HEI]; 
    HANDLE console; DIR dir; COORD snk[MAX_LEN];
    COORD head; int tailIdx, headIdx, points;
};
int main( int argc, char* argv[] ) {
    srand( static_cast<unsigned>( time( NULL ) ) );
    snake s; s.play(); return 0;
}

```



## Go

This uses the [https://github.com/nsf/termbox-go termbox] package
for terminal input and output.
This makes the code fairly cross-platform, it successfully built for
FreeBSD, OpenBSD, NetBSD, DragonFly BSD, Linux, MS Windows, and MacOS
(tested on FreeBSD and MS Windows).

```Go
package main

import (
	"errors"
	"fmt"
	"log"
	"math/rand"
	"time"

	termbox "github.com/nsf/termbox-go"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	score, err := playSnake()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("Final score:", score)
}

type snake struct {
	body          []position // tail to head positions of the snake
	heading       direction
	width, height int
	cells         []termbox.Cell
}

type position struct {
	X int
	Y int
}

type direction int

const (
	North direction = iota
	East
	South
	West
)

func (p position) next(d direction) position {
	switch d {
	case North:
		p.Y--
	case East:
		p.X++
	case South:
		p.Y++
	case West:
		p.X--
	}
	return p
}

func playSnake() (int, error) {
	err := termbox.Init()
	if err != nil {
		return 0, err
	}
	defer termbox.Close()

	termbox.Clear(fg, bg)
	termbox.HideCursor()
	s := &snake{
		// It would be more efficient to use a circular
		// buffer instead of a plain slice for s.body.
		body:  make([]position, 0, 32),
		cells: termbox.CellBuffer(),
	}
	s.width, s.height = termbox.Size()
	s.drawBorder()
	s.startSnake()
	s.placeFood()
	s.flush()

	moveCh, errCh := s.startEventLoop()
	const delay = 125 * time.Millisecond
	for t := time.NewTimer(delay); ; t.Reset(delay) {
		var move direction
		select {
		case err = <-errCh:
			return len(s.body), err
		case move = <-moveCh:
			if !t.Stop() {
				<-t.C // handles race between moveCh and t.C
			}
		case <-t.C:
			move = s.heading
		}
		if s.doMove(move) {
			time.Sleep(1 * time.Second)
			break
		}
	}

	return len(s.body), err
}

func (s *snake) startEventLoop() (<-chan direction, <-chan error) {
	moveCh := make(chan direction)
	errCh := make(chan error, 1)
	go func() {
		defer close(errCh)
		for {
			switch ev := termbox.PollEvent(); ev.Type {
			case termbox.EventKey:
				switch ev.Ch { // WSAD and HJKL movement
				case 'w', 'W', 'k', 'K':
					moveCh <- North
				case 'a', 'A', 'h', 'H':
					moveCh <- West
				case 's', 'S', 'j', 'J':
					moveCh <- South
				case 'd', 'D', 'l', 'L':
					moveCh <- East
				case 0:
					switch ev.Key { // Cursor key movement
					case termbox.KeyArrowUp:
						moveCh <- North
					case termbox.KeyArrowDown:
						moveCh <- South
					case termbox.KeyArrowLeft:
						moveCh <- West
					case termbox.KeyArrowRight:
						moveCh <- East
					case termbox.KeyEsc: // Quit
						return
					}
				}
			case termbox.EventResize:
				// TODO
				errCh <- errors.New("terminal resizing unsupported")
				return
			case termbox.EventError:
				errCh <- ev.Err
				return
			case termbox.EventInterrupt:
				return
			}
		}
	}()
	return moveCh, errCh
}

func (s *snake) flush() {
	termbox.Flush()
	s.cells = termbox.CellBuffer()
}

func (s *snake) getCellRune(p position) rune {
	i := p.Y*s.width + p.X
	return s.cells[i].Ch
}
func (s *snake) setCell(p position, c termbox.Cell) {
	i := p.Y*s.width + p.X
	s.cells[i] = c
}

func (s *snake) drawBorder() {
	for x := 0; x < s.width; x++ {
		s.setCell(position{x, 0}, border)
		s.setCell(position{x, s.height - 1}, border)
	}
	for y := 0; y < s.height-1; y++ {
		s.setCell(position{0, y}, border)
		s.setCell(position{s.width - 1, y}, border)
	}
}

func (s *snake) placeFood() {
	for {
		// a random empty cell
		x := rand.Intn(s.width-2) + 1
		y := rand.Intn(s.height-2) + 1
		foodp := position{x, y}
		r := s.getCellRune(foodp)
		if r != ' ' {
			continue
		}
		s.setCell(foodp, food)
		return
	}
}

func (s *snake) startSnake() {
	// a random cell somewhat near the center
	x := rand.Intn(s.width/2) + s.width/4
	y := rand.Intn(s.height/2) + s.height/4
	head := position{x, y}
	s.setCell(head, snakeHead)
	s.body = append(s.body[:0], head)
	s.heading = direction(rand.Intn(4))
}

func (s *snake) doMove(move direction) bool {
	head := s.body[len(s.body)-1]
	s.setCell(head, snakeBody)
	head = head.next(move)
	s.heading = move
	s.body = append(s.body, head)
	r := s.getCellRune(head)
	s.setCell(head, snakeHead)
	gameOver := false
	switch r {
	case food.Ch:
		s.placeFood()
	case border.Ch, snakeBody.Ch:
		gameOver = true
		fallthrough
	case empty.Ch:
		s.setCell(s.body[0], empty)
		s.body = s.body[1:]
	default:
		panic(r)
	}
	s.flush()
	return gameOver
}

const (
	fg = termbox.ColorWhite
	bg = termbox.ColorBlack
)

// Symbols to use.
// Could use Unicode instead of simple ASCII.
var (
	empty     = termbox.Cell{Ch: ' ', Bg: bg, Fg: fg}
	border    = termbox.Cell{Ch: '+', Bg: bg, Fg: termbox.ColorBlue}
	snakeBody = termbox.Cell{Ch: '#', Bg: bg, Fg: termbox.ColorGreen}
	snakeHead = termbox.Cell{Ch: 'O', Bg: bg, Fg: termbox.ColorYellow | termbox.AttrBold}
	food      = termbox.Cell{Ch: '@', Bg: bg, Fg: termbox.ColorRed}
)
```



## Haskell


```haskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.Random (getRandomRs)
import Graphics.Gloss.Interface.Pure.Game
import Lens.Micro ((%~), (^.), (&), set)
import Lens.Micro.TH (makeLenses)

--------------------------------------------------------------------------------
-- all data types

data Snake = Snake { _body :: [Point], _direction :: Point }
makeLenses ''Snake

data World = World { _snake :: Snake , _food :: [Point]
                   , _score :: Int , _maxScore :: Int }
makeLenses ''World

--------------------------------------------------------------------------------
-- everything snake can do

moves (Snake b d) = Snake (step b d : init b) d
eats  (Snake b d) = Snake (step b d : b) d
bites (Snake b _) = any (== head b)
step ((x,y):_) (a,b) = (x+a, y+b)

turn (x',y') (Snake b (x,y)) | (x+x',y+y') == (0,0)  = Snake b (x,y)
                             | otherwise             = Snake b (x',y')

--------------------------------------------------------------------------------
-- all randomness

createWorld = do xs <- map fromIntegral <$> getRandomRs (2, 38 :: Int)
                 ys <- map fromIntegral <$> getRandomRs (2, 38 :: Int)
                 return (Ok, World snake (zip xs ys) 0 0)
                 where
                   snake = Snake [(20, 20)] (1,0)
                 
-------------------------------------------------------------------------------
-- A tyny DSL for declarative description of business logic 

data Status = Ok | Fail | Stop

continue = \x -> (Ok, x)
stop     = \x -> (Stop, x)
f >>> g  = \x -> case f x of { (Ok, y) -> g y; b -> b }    -- chain composition
f <|> g  = \x -> case f x of { (Fail, _) -> g x; b -> b }  -- alternative
p ==> f  = \x -> if p x then f x else (Fail, x)            -- condition
l .& f   = continue . (l %~ f)                             -- modification
l .= y   = continue . set l y                              -- setting

--------------------------------------------------------------------------------
-- all business logic

updateWorld _ =  id >>> (snakeEats <|> snakeMoves) 
  where
    snakeEats  = (snakeFindsFood ==> (snake .& eats)) >>>
                 (score .& (+1)) >>> (food .& tail)

    snakeMoves = (snakeBitesTail ==> stop) <|>
                 (snakeHitsWall ==> stop) <|>
                 (snake .& moves)

    snakeFindsFood w = (w^.snake & moves) `bites` (w^.food & take 1)
    snakeBitesTail w = (w^.snake) `bites` (w^.snake.body & tail)
    snakeHitsWall w  = (w^.snake.body) & head & isOutside
    isOutside (x,y) = or [x <= 0, 40 <= x, y <= 0, 40 <= y]

--------------------------------------------------------------------------------
-- all event handing

handleEvents e (s,w) = f w
  where f = case s of
          Ok -> case e of
            EventKey (SpecialKey k) _ _ _ -> case k of
              KeyRight -> snake .& turn (1,0)
              KeyLeft  -> snake .& turn (-1,0)
              KeyUp    -> snake .& turn (0,1)
              KeyDown  -> snake .& turn (0,-1)
              _-> continue
            _-> continue
          _-> \w -> w & ((snake.body) .= [(20,20)]) >>>
                         (maxScore .& max (w^.score)) >>> (score .= 0)
                         
--------------------------------------------------------------------------------
-- all graphics

renderWorld (s, w) = pictures [frame, color c drawSnake, drawFood, showScore]
  where c = case s of { Ok -> orange; _-> red }
        drawSnake = foldMap (rectangleSolid 10 10 `at`) (w^.snake.body)
        drawFood = color blue $ circleSolid 5 `at` (w^.food & head)
        frame = color black $ rectangleWire 400 400
        showScore = color orange $ scale 0.2 0.2 $ txt `at` (-80,130)
        txt = Text $ mconcat ["Score: ", w^.score & show
                             ,"   Maximal score: ", w^.maxScore & show]
        at p (x,y) = Translate (10*x-200) (10*y-200) p

--------------------------------------------------------------------------------

main = do world <- createWorld
          play inW white 7 world renderWorld handleEvents updateWorld
  where inW = InWindow "The Snake" (400, 400) (10, 10)
```


'''Extra credit'''

It is easy to make snake to seek food automatically. Just change the first line of the <code>updateWorld</code> definition:


```haskell
updateWorld _ =  id >>> snakeSeeksFood >>> (snakeEats <|> snakeMoves) 
```


and add local definition:


```haskell
    snakeSeeksFood w = w & snake .& turns optimalDirection
      where
        optimalDirection = minimumBy (comparing distanceToFood) safeTurns
        
        safeTurns = filter safe [(x,y),(-y,x),(y,-x)] `ifEmpty` [(x,y)]
          where (x,y) = w^.snake.direction
                safe d = let w'' = w & snake %~ moves . turns d
                         in not (snakeBitesTail w'' || snakeHitsWall w'')
                lst `ifEmpty` x = if null lst then x else lst
        
        distanceToFood d = let (a,b) = w^.snake & turns d & moves & (^.body) & head
                               (x,y) = w^.food & head
                           in (a-x)^2 + (b-y)^2
```



## Java


See [[Snake/Java]].


## JavaScript

You need the P5 Library to run this code!

```javascript

const L = 1, R = 2, D = 4, U = 8;
var block = 24, wid = 30, hei = 20, frameR = 7, fruit, snake;
function Snake() {
    this.length = 1;
    this.alive = true;
    this.pos = createVector( 1, 1 );
    this.posArray = [];
    this.posArray.push( createVector( 1, 1 ) );
    this.dir = R;
    this.draw = function() {
        fill( 130, 190, 0 );
        var pos, i = this.posArray.length - 1, l = this.length;
        while( true ){
            pos = this.posArray[i--];
            rect( pos.x * block, pos.y * block, block, block );
            if( --l == 0 ) break;
        }
    }
    this.eat = function( frut ) {
        var b = this.pos.x == frut.x && this.pos.y == frut.y;
        if( b ) this.length++;
        return b;
    }
    this.overlap = function() {
        var len = this.posArray.length - 1;
        for( var i = len; i > len - this.length; i-- ) {
            tp = this.posArray[i];
            if( tp.x === this.pos.x && tp.y === this.pos.y ) return true;
        }
        return false;
    }
    this.update = function() {
        if( !this.alive ) return;
        switch( this.dir ) {
            case L:
                this.pos.x--; if( this.pos.x < 1 ) this.pos.x = wid - 2;
            break;
            case R:
                this.pos.x++; if( this.pos.x > wid - 2 ) this.pos.x = 1;
            break;
            case U:
                this.pos.y--; if( this.pos.y < 1 ) this.pos.y = hei - 2;
            break;
            case D:
                this.pos.y++; if( this.pos.y > hei - 2 ) this.pos.y = 1;
            break;
        }
        if( this.overlap() ) { this.alive = false; } else {
            this.posArray.push( createVector( this.pos.x, this.pos.y ) );
            if( this.posArray.length > 5000 ) { this.posArray.splice( 0, 1 ); }
        }
    }
}
function Fruit() {
    this.fruitTime = true;
    this.pos = createVector();
    this.draw = function() {
        fill( 200, 50, 20 );
        rect( this.pos.x * block, this.pos.y * block, block, block );
    }

    this.setFruit = function() {
        this.pos.x = floor( random( 1, wid - 1 ) );
        this.pos.y = floor( random( 1, hei - 1 ) );
        this.fruitTime = false;
    }
}
function setup() {
    createCanvas( block * wid, block * hei );
    noStroke(); frameRate( frameR );
    snake = new Snake();fruit = new Fruit();
}
function keyPressed() {
    switch( keyCode ) {
        case LEFT_ARROW: snake.dir = L; break;
        case RIGHT_ARROW: snake.dir = R; break;
        case UP_ARROW: snake.dir = U; break;
        case DOWN_ARROW: snake.dir = D;
    }
}
function draw() {
    background( color( 0, 0x22, 0 ) );
    fill( 20, 50, 120 );
    for( var i = 0; i < wid; i++ ) {
        rect( i * block, 0, block, block );
        rect( i * block, height - block, block, block );
    }
    for( var i = 1; i < hei - 1; i++ ) {
        rect( 1, i * block, block, block );
        rect( width - block, i * block, block, block );
    }
    if( fruit.fruitTime ) {
        fruit.setFruit();
        frameR += .2;
        frameRate( frameR );
    }
    fruit.draw();
    snake.update();
    if( snake.eat( fruit.pos ) ) {
        fruit.fruitTime = true;
    }
    snake.draw();
    fill( 200 );
    textStyle( BOLD ); textAlign( RIGHT ); textSize( 120 );
    text( ""+( snake.length - 1 ), 690, 440 );
    if( !snake.alive ) text( "THE END", 630, 250 );
}

```



## Kotlin

```scala
// Kotlin Native v0.5

import kotlinx.cinterop.*
import platform.posix.*
import platform.windows.*

const val WID = 60
const val HEI = 30
const val MAX_LEN = 600
const val NUL = '\u0000'

enum class Dir { NORTH, EAST, SOUTH, WEST }

class Snake {
    val console: HANDLE
    var alive = false
    val brd = CharArray(WID * HEI)
    var dir = Dir.NORTH
    val snk = nativeHeap.allocArray<COORD>(MAX_LEN)
    lateinit var head: COORD
    var tailIdx = 0
    var headIdx = 0
    var points = 0

    init {
        console = GetStdHandle(STD_OUTPUT_HANDLE)!!
        SetConsoleTitleW("Snake")
        memScoped {
            val coord = alloc<COORD>().apply { X = (WID + 1).toShort(); Y = (HEI + 2).toShort() }
            SetConsoleScreenBufferSize(console, coord.readValue())
            val rc = alloc<SMALL_RECT>().apply {
                Left = 0; Top = 0; Right = WID.toShort(); Bottom = (HEI + 1).toShort()
            }
            SetConsoleWindowInfo(console, TRUE, rc.ptr)
            val ci = alloc<CONSOLE_CURSOR_INFO>().apply { dwSize = 1; bVisible = FALSE }
            SetConsoleCursorInfo(console, ci.ptr)
        }
    }

    fun play() {
        while (true) {
            createfield()
            alive = true
            while (alive) {
                drawfield()
                readKey()
                moveSnake()
                Sleep(50)
            }
            memScoped {
                val c = alloc<COORD>().apply { X = 0; Y = (HEI + 1).toShort() }
                SetConsoleCursorPosition(console, c.readValue())
            }
            SetConsoleTextAttribute(console, 0x000b)
            print("Play again [Y/N]? ")
            val a = readLine()!!.toLowerCase()
            if (a.length > 0 && a[0] != 'y') {
                nativeHeap.free(snk)
                return
            }
        }
    }

    private fun createfield() {
        memScoped {
            val coord = alloc<COORD>().apply { X = 0; Y = 0 }
            val c = alloc<DWORDVar>()
            FillConsoleOutputCharacterW(console, 32, (HEI + 2) * 80, coord.readValue(), c.ptr)
            FillConsoleOutputAttribute(console, 0x0000, (HEI + 2) * 80, coord.readValue(), c.ptr)
            SetConsoleCursorPosition(console, coord.readValue())
        }
        for (x in 0 until WID * HEI) brd[x] = NUL
        for (x in 0 until WID) {
            brd[x + WID * (HEI - 1)] = '+'
            brd[x] = '+'
        }
        for (y in 1 until HEI) {
            brd[WID - 1 + WID * y] = '+' 
            brd[WID * y] = '+'
        }
        var xx: Int
        var yy: Int
        do {
            xx = rand() % WID
            yy = rand() % (HEI shr 1) + (HEI shr 1)
        }
        while (brd[xx + WID * yy] != NUL)
        brd[xx + WID * yy] = '@'
        tailIdx = 0
        headIdx = 4
        xx = 3
        yy = 2
        for (cc in tailIdx until headIdx) {
            brd[xx + WID * yy] = '#'
            snk[cc].X = (3 + cc).toShort()
            snk[cc].Y = 2
        }
        head = snk[3]
        dir = Dir.EAST
        points = 0
    }

    private fun readKey() {
        if ((GetAsyncKeyState(39).toInt() and 0x8000) != 0) dir = Dir.EAST
        if ((GetAsyncKeyState(37).toInt() and 0x8000) != 0) dir = Dir.WEST
        if ((GetAsyncKeyState(38).toInt() and 0x8000) != 0) dir = Dir.NORTH
        if ((GetAsyncKeyState(40).toInt() and 0x8000) != 0) dir = Dir.SOUTH
    }

    private fun drawfield() {
        memScoped {
            val coord = alloc<COORD>()
            var t = NUL
            for (y in 0 until HEI) {
                coord.Y = y.toShort()
                for (x in 0 until WID) {
                    t = brd[x + WID * y]
                    if (t == NUL) continue
                    coord.X = x.toShort()
                    SetConsoleCursorPosition(console, coord.readValue())
                    if (coord.X == head.X && coord.Y == head.Y) {
                        SetConsoleTextAttribute(console, 0x002e)
                        print('O')
                        SetConsoleTextAttribute(console, 0x0000)
                        continue
                    }
                    when (t) {
                        '#' ->  SetConsoleTextAttribute(console, 0x002a)
                        '+' ->  SetConsoleTextAttribute(console, 0x0019)
                        '@' ->  SetConsoleTextAttribute(console, 0x004c)
                    }
                    print(t)
                    SetConsoleTextAttribute(console, 0x0000)
                }
            }
            print(t)
            SetConsoleTextAttribute(console, 0x0007)
            val c = alloc<COORD>().apply { X = 0; Y = HEI.toShort() }
            SetConsoleCursorPosition(console, c.readValue())
            print("Points: $points")
        }
    }

    private fun moveSnake() {
        when (dir) {
            Dir.NORTH -> head.Y--
            Dir.EAST  -> head.X++
            Dir.SOUTH -> head.Y++
            Dir.WEST  -> head.X--
        }
        val t = brd[head.X + WID * head.Y]
        if (t != NUL && t != '@') {
            alive = false
            return
        }
        brd[head.X + WID * head.Y] = '#'
        snk[headIdx].X = head.X
        snk[headIdx].Y = head.Y
        if (++headIdx >= MAX_LEN) headIdx = 0
        if (t == '@') {
            points++
            var x: Int
            var y: Int
            do {
                x = rand() % WID
                y = rand() % (HEI shr 1) + (HEI shr 1)
            }
            while (brd[x + WID * y] != NUL)
            brd[x + WID * y] = '@'
            return
        }
        SetConsoleCursorPosition(console, snk[tailIdx].readValue())
        print(' ')
        brd[snk[tailIdx].X + WID * snk[tailIdx].Y] = NUL
        if (++tailIdx >= MAX_LEN) tailIdx = 0
    }
}

fun main(args: Array<String>) {
    srand(time(null).toInt())
    Snake().play()
}
```


```txt

Similar to C++ entry

```




## OCaml


```ocaml
(* A simple Snake Game *)
open Sdl

let width, height = (640, 480)

type pos = int * int

type game_state = {
  pos_snake: pos;
  seg_snake: pos list;
  dir_snake: [`left | `right | `up | `down];
  pos_fruit: pos;
  sleep_time: int;
  game_over: bool;
}

let red   = (255, 0, 0)
let blue  = (0, 0, 255)
let green = (0, 255, 0)
let black = (0, 0, 0)
let alpha = 255

let fill_rect renderer (x, y) =
  let rect = Rect.make4 x y 20 20 in
  Render.fill_rect renderer rect;
;;


let display_game renderer state =
  let bg_color, snake_color, fruit_color =
    if state.game_over
    then (red, black, green)
    else (black, blue, red)
  in
  Render.set_draw_color renderer bg_color alpha;
  Render.clear renderer;
  Render.set_draw_color renderer fruit_color alpha;
  fill_rect renderer state.pos_fruit;
  Render.set_draw_color renderer snake_color alpha;
  List.iter (fill_rect renderer) state.seg_snake;
  Render.render_present renderer;
;;


let proc_events dir_snake = function
  | Event.KeyDown { Event.keycode = Keycode.Left } -> `left
  | Event.KeyDown { Event.keycode = Keycode.Right } -> `right
  | Event.KeyDown { Event.keycode = Keycode.Up } -> `up
  | Event.KeyDown { Event.keycode = Keycode.Down } -> `down
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0
  | _ -> (dir_snake)


let rec event_loop dir_snake =
  match Event.poll_event () with
  | None -> (dir_snake)
  | Some ev ->
      let dir = proc_events dir_snake ev in
      event_loop dir


let rec pop = function
  | [_] -> []
  | hd :: tl -> hd :: (pop tl)
  | [] -> invalid_arg "pop"


let rec new_pos_fruit seg_snake =
  let new_pos =
    (20 * Random.int 32,
     20 * Random.int 24)
  in
  if List.mem new_pos seg_snake
  then new_pos_fruit seg_snake
  else (new_pos)


let update_state req_dir ({
    pos_snake;
    seg_snake;
    pos_fruit;
    dir_snake;
    sleep_time;
    game_over;
  } as state) =
  if game_over then state else
  let dir_snake =
    match dir_snake, req_dir with
    | `left, `right -> dir_snake
    | `right, `left -> dir_snake
    | `up, `down -> dir_snake
    | `down, `up -> dir_snake
    | _ -> req_dir
  in
  let pos_snake =
    let x, y = pos_snake in
    match dir_snake with
    | `left  -> (x - 20, y)
    | `right -> (x + 20, y)
    | `up    -> (x, y - 20)
    | `down  -> (x, y + 20)
  in
  let game_over =
    let x, y = pos_snake in
    List.mem pos_snake seg_snake
    || x < 0 || y < 0
    || x >= width
    || y >= height
  in
  let seg_snake = pos_snake :: seg_snake in
  let seg_snake, pos_fruit, sleep_time =
    if pos_snake = pos_fruit
    then (seg_snake, new_pos_fruit seg_snake, sleep_time - 1)
    else (pop seg_snake, pos_fruit, sleep_time)
  in
  { pos_snake;
    seg_snake;
    pos_fruit;
    dir_snake;
    sleep_time;
    game_over;
  }


let () =
  Random.self_init ();
  Sdl.init [`VIDEO];
  let window, renderer =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  Window.set_title ~window ~title:"Snake OCaml-SDL2";
  let initial_state = {
    pos_snake = (100, 100);
    seg_snake = [
      (100, 100);
      ( 80, 100);
      ( 60, 100);
    ];
    pos_fruit = (200, 200);
    dir_snake = `right;
    sleep_time = 120;
    game_over = false;
  } in

  let rec main_loop state =
    let req_dir = event_loop state.dir_snake in
    let state = update_state req_dir state in
    display_game renderer state;
    Timer.delay state.sleep_time;
    main_loop state
  in
  main_loop initial_state
```



## Perl

[[File:Snake_game_perl.png|200px|thumb|right]]

```perl
use utf8;
use Time::HiRes qw(sleep);
use Term::ANSIColor qw(colored);
use Term::ReadKey qw(ReadMode ReadLine);

binmode(STDOUT, ':utf8');

use constant {
              VOID => 0,
              HEAD => 1,
              BODY => 2,
              TAIL => 3,
              FOOD => 4,
             };

use constant {
              LEFT  => [+0, -1],
              RIGHT => [+0, +1],
              UP    => [-1, +0],
              DOWN  => [+1, +0],
             };

use constant {
              BG_COLOR  => "on_black",
              SLEEP_SEC => 0.05,
             };

use constant {
              SNAKE_COLOR => ('bold green' . ' ' . BG_COLOR),
              FOOD_COLOR  => ('red'        . ' ' . BG_COLOR),
             };

use constant {
    U_HEAD => colored('▲', SNAKE_COLOR),
    D_HEAD => colored('▼', SNAKE_COLOR),
    L_HEAD => colored('◀', SNAKE_COLOR),
    R_HEAD => colored('▶', SNAKE_COLOR),

    U_BODY => colored('╹', SNAKE_COLOR),
    D_BODY => colored('╻', SNAKE_COLOR),
    L_BODY => colored('╴', SNAKE_COLOR),
    R_BODY => colored('╶', SNAKE_COLOR),

    U_TAIL => colored('╽', SNAKE_COLOR),
    D_TAIL => colored('╿', SNAKE_COLOR),
    L_TAIL => colored('╼', SNAKE_COLOR),
    R_TAIL => colored('╾', SNAKE_COLOR),

    A_VOID => colored(' ',   BG_COLOR),
    A_FOOD => colored('❇', FOOD_COLOR),
             };

local $| = 1;

my $w = eval { `tput cols` }  || 80;
my $h = eval { `tput lines` } || 24;
my $r = "\033[H";

my @grid = map {
    [map { [VOID] } 1 .. $w]
} 1 .. $h;

my $dir      = LEFT;
my @head_pos = ($h / 2, $w / 2);
my @tail_pos = ($head_pos[0], $head_pos[1] + 1);

$grid[$head_pos[0]][$head_pos[1]] = [HEAD, $dir];    # head
$grid[$tail_pos[0]][$tail_pos[1]] = [TAIL, $dir];    # tail

sub create_food {
    my ($food_x, $food_y);

    do {
        $food_x = rand($w);
        $food_y = rand($h);
    } while ($grid[$food_y][$food_x][0] != VOID);

    $grid[$food_y][$food_x][0] = FOOD;
}

create_food();

sub display {
    my $i = 0;

    print $r, join("\n",
        map {
            join("",
                map {
                    my $t = $_->[0];
                    if ($t != FOOD and $t != VOID) {
                        my $p = $_->[1];
                        $i =
                            $p eq UP   ? 0
                          : $p eq DOWN ? 1
                          : $p eq LEFT ? 2
                          :              3;
                    }
                        $t == HEAD ? (U_HEAD, D_HEAD, L_HEAD, R_HEAD)[$i]
                      : $t == BODY ? (U_BODY, D_BODY, L_BODY, R_BODY)[$i]
                      : $t == TAIL ? (U_TAIL, D_TAIL, L_TAIL, R_TAIL)[$i]
                      : $t == FOOD ? (A_FOOD)
                      :              (A_VOID);

                  } @{$_}
                )
          } @grid
    );
}

sub move {
    my $grew = 0;

    # Move the head
    {
        my ($y, $x) = @head_pos;

        my $new_y = ($y + $dir->[0]) % $h;
        my $new_x = ($x + $dir->[1]) % $w;

        my $cell = $grid[$new_y][$new_x];
        my $t    = $cell->[0];

        if ($t == BODY or $t == TAIL) {
            die "Game over!\n";
        }
        elsif ($t == FOOD) {
            create_food();
            $grew = 1;
        }

        # Create a new head
        $grid[$new_y][$new_x] = [HEAD, $dir];

        # Replace the current head with body
        $grid[$y][$x] = [BODY, $dir];

        # Save the position of the head
        @head_pos = ($new_y, $new_x);
    }

    # Move the tail
    if (not $grew) {
        my ($y, $x) = @tail_pos;

        my $pos   = $grid[$y][$x][1];
        my $new_y = ($y + $pos->[0]) % $h;
        my $new_x = ($x + $pos->[1]) % $w;

        $grid[$y][$x][0]         = VOID;    # erase the current tail
        $grid[$new_y][$new_x][0] = TAIL;    # create a new tail

        # Save the position of the tail
        @tail_pos = ($new_y, $new_x);
    }
}

ReadMode(3);
while (1) {
    my $key;
    until (defined($key = ReadLine(-1))) {
        move();
        display();
        sleep(SLEEP_SEC);
    }

    if    ($key eq "\e[A" and $dir ne DOWN ) { $dir = UP    }
    elsif ($key eq "\e[B" and $dir ne UP   ) { $dir = DOWN  }
    elsif ($key eq "\e[C" and $dir ne LEFT ) { $dir = RIGHT }
    elsif ($key eq "\e[D" and $dir ne RIGHT) { $dir = LEFT  }
}
```



## Perl 6

This is a variation of a demo script included in the examples folder for the Perl 6 SDL2::Raw library bindings.


```perl6
use SDL2::Raw;
use Cairo;

constant W = 1280;
constant H = 960;

constant FIELDW = W div 32;
constant FIELDH = H div 32;

SDL_Init(VIDEO);

my $window = SDL_CreateWindow(
    'Snake',
    SDL_WINDOWPOS_CENTERED_MASK,
    SDL_WINDOWPOS_CENTERED_MASK,
    W, H,
    OPENGL
);

my $render = SDL_CreateRenderer($window, -1, ACCELERATED +| PRESENTVSYNC);

my $snake_image = Cairo::Image.record(
    -> $_ {
        .save;
        .rectangle: 0, 0, 64, 64;
        .clip;
        .rgb: 0, 1, 0;
        .rectangle: 0, 0, 64, 64;
        .fill :preserve;
        .rgb: 0, 0, 0;
        .stroke;
        .restore;

        .save;
        .translate: 64, 0;
        .rectangle: 0, 0, 64, 64;
        .clip;
        .rgb: 1, 0, 0;
        .arc: 32, 32, 30, 0, 2 * pi;
        .fill :preserve;
        .rgb: 0, 0, 0;
        .stroke;
        .restore;
    }, 128, 128, Cairo::FORMAT_ARGB32);

my $snake_texture = SDL_CreateTexture(
    $render,
    %PIXELFORMAT<ARGB8888>,
    STATIC,
    128,
    128
);

SDL_UpdateTexture(
    $snake_texture,
    SDL_Rect.new(
        :x(0),
        :y(0),
        :w(128),
        :h(128)
    ),
    $snake_image.data,
    $snake_image.stride // 128 * 4
);

SDL_SetTextureBlendMode($snake_texture, 1);

SDL_SetRenderDrawBlendMode($render, 1);

my $snakepiece_srcrect = SDL_Rect.new(:w(64), :h(64));
my $nompiece_srcrect   = SDL_Rect.new(:w(64), :h(64), :x(64));

my $event = SDL_Event.new;

enum GAME_KEYS (
    K_UP    => 82,
    K_DOWN  => 81,
    K_LEFT  => 80,
    K_RIGHT => 79,
);

my Complex @snakepieces = 10+10i;
my Complex @noms;
my Complex $snakedir = 1+0i;
my $nomspawn   = 0;
my $snakespeed = 0.1;
my $snakestep  = 0;
my $nom        = 4;

my $last_frame_start = now;
main: loop {
    my $start = now;
    my $dt = $start - $last_frame_start // 0.00001;
    while SDL_PollEvent($event) {
        my $casted_event = SDL_CastEvent($event);
        given $casted_event {
            when *.type == QUIT    { last main }
            when *.type == KEYDOWN {
                if GAME_KEYS(.scancode) -> $comm {
                    given $comm {
                        when 'K_LEFT'  { $snakedir = -1+0i unless $snakedir ==  1+0i }
                        when 'K_RIGHT' { $snakedir =  1+0i unless $snakedir == -1+0i }
                        when 'K_UP'    { $snakedir =  0-1i unless $snakedir ==  0+1i }
                        when 'K_DOWN'  { $snakedir =  0+1i unless $snakedir ==  0-1i }
                    }
                }
            }
        }
    }

    if ($nomspawn -= $dt) < 0 {
        $nomspawn += 1;
        @noms.push: (^FIELDW).pick + (^FIELDH).pick * i unless @noms > 3;
        @noms.pop if @noms[*-1] == any(@snakepieces);
    }

    if ($snakestep -= $dt) < 0 {
        $snakestep += $snakespeed;

        @snakepieces.unshift: do given @snakepieces[0] {
            ($_.re + $snakedir.re) % FIELDW
            + (($_.im + $snakedir.im) % FIELDH) * i
        }

        if @snakepieces[2..*].first( * == @snakepieces[0], :k ) -> $idx {
            @snakepieces = @snakepieces[0..($idx + 1)];
        }

        @noms .= grep(
            { $^piece == @snakepieces[0] ?? ($nom += 1) && False !! True }
        );

        if $nom == 0 {
            @snakepieces.pop;
        } else {
            $nom = $nom - 1;
        }
    }

    for @snakepieces {
        SDL_SetTextureColorMod(
            $snake_texture,
            255,
            (cos((++$) / 2) * 100 + 155).round,
            255
        );

        SDL_RenderCopy(
            $render,
            $snake_texture,
            $snakepiece_srcrect,
            SDL_Rect.new(.re * 32, .im * 32, 32, 32)
        );
    }

    SDL_SetTextureColorMod($snake_texture, 255, 255, 255);

    for @noms {
        SDL_RenderCopy(
            $render,
            $snake_texture,
            $nompiece_srcrect,
            SDL_Rect.new(.re * 32, .im * 32, 32, 32)
        )
    }

    SDL_RenderPresent($render);
    SDL_SetRenderDrawColor($render, 0, 0, 0, 0);
    SDL_RenderClear($render);

    $last_frame_start = $start;
    sleep(1 / 50);
}

SDL_Quit();
```



## Phix

```Phix
constant W = 60, H = 30, MAX_LEN = 600
enum NORTH, EAST, SOUTH, WEST

sequence board, snake
bool alive
integer tailIdx, headIdx, hdX, hdY, d, points
 
procedure createField()
    clear_screen()
    board = repeat("+"&repeat(' ',W-2)&'+',H)
    for x=1 to W do
        board[1,x] = '+'
    end for
    board[H] = board[1]
    board[1+rand(H-2),1+rand(W-2)] = '@';
    snake = repeat(0,MAX_LEN)
    board[3,4] = '#'; tailIdx = 1; headIdx = 5;
    for c=tailIdx to headIdx do
        snake[c] = {3,3+c}
    end for
    {hdY,hdX} = snake[headIdx-1]; d = EAST; points = 0;
end procedure

procedure drawField()
    for y=1 to H do
        for x=1 to W do
            integer t = board[y,x]
            if t!=' ' then
                position(y,x)
                if x=hdX and y=hdY then
                    text_color(14); puts(1,'O');
                else
                    text_color({10,9,12}[find(t,"#+@")]); puts(1,t);
                end if
            end if
        end for
    end for
    position(H+1,1); text_color(7); printf(1,"Points: %d",points)
end procedure

procedure readKey()
    integer k = find(get_key(),{333,331,328,336})
    if k then d = {EAST,WEST,NORTH,SOUTH}[k] end if
end procedure

procedure moveSnake()
integer x,y
    switch d do
        case NORTH: hdY -= 1
        case EAST:  hdX += 1
        case SOUTH: hdY += 1
        case WEST:  hdX -= 1
    end switch
    integer t = board[hdY,hdX];
    if t!=' ' and t!='@' then alive = false; return; end if
    board[hdY,hdX] = '#'; snake[headIdx] = {hdY,hdX};
    headIdx += 1; if headIdx>MAX_LEN then headIdx = 1 end if
    if t=='@' then
        points += 1
        while 1 do
            x = 1+rand(W-2); y = 1+rand(H-2);
            if board[y,x]=' ' then
                board[y,x] = '@'
                return
            end if
        end while
    end if
    {y,x} = snake[tailIdx]; position(y,x); puts(1,' '); board[y,x] = ' ';
    tailIdx += 1; if tailIdx>MAX_LEN then tailIdx = 1 end if
end procedure

procedure play()
    while true do
        createField(); alive = true; cursor(NO_CURSOR)
        while alive do drawField(); readKey(); moveSnake(); sleep(0.05) end while
        cursor(BLOCK_CURSOR); position(H+2,1); bk_color(0); text_color(11);
        puts(1,"Play again [Y/N]? ")
        if upper(wait_key())!='Y' then return end if
    end while
end procedure
play()
```



## Sidef


```ruby
class SnakeGame(w, h) {
    const readkey = frequire('Term::ReadKey')
    const ansi    = frequire('Term::ANSIColor')

    enum (VOID, HEAD, BODY, TAIL, FOOD)

    define (
        LEFT  = [+0, -1],
        RIGHT = [+0, +1],
        UP    = [-1, +0],
        DOWN  = [+1, +0],
    )

    define BG_COLOR    = "on_black"
    define FOOD_COLOR  = ("red"        + " " + BG_COLOR)
    define SNAKE_COLOR = ("bold green" + " " + BG_COLOR)
    define SLEEP_SEC   = 0.02

    const (
        A_VOID  = ansi.colored(' ', BG_COLOR),
        A_FOOD  = ansi.colored('❇', FOOD_COLOR),
        A_BLOCK = ansi.colored('■', SNAKE_COLOR),
    )

    has dir = LEFT
    has grid = [[]]
    has head_pos = [0, 0]
    has tail_pos = [0, 0]

    method init {
        grid = h.of { w.of { [VOID] } }

        head_pos = [h//2, w//2]
        tail_pos = [head_pos[0], head_pos[1]+1]

        grid[head_pos[0]][head_pos[1]] = [HEAD, dir]    # head
        grid[tail_pos[0]][tail_pos[1]] = [TAIL, dir]    # tail

        self.make_food()
    }

    method make_food {
        var (food_x, food_y)

        do {
            food_x = w.rand.int
            food_y = h.rand.int
        } while (grid[food_y][food_x][0] != VOID)

        grid[food_y][food_x][0] = FOOD
    }

    method display {
        print("\033[H", grid.map { |row|
            row.map { |cell|
                given (cell[0]) {
                    when (VOID) { A_VOID }
                    when (FOOD) { A_FOOD }
                    default     { A_BLOCK }
                }
              }.join('')
            }.join("\n")
        )
    }

    method move {
        var grew = false

        # Move the head
        var (y, x) = head_pos...

        var new_y = (y+dir[0] % h)
        var new_x = (x+dir[1] % w)

        var cell = grid[new_y][new_x]

        given (cell[0]) {
            when (BODY) { die "\nYou just bit your own body!\n" }
            when (TAIL) { die "\nYou just bit your own tail!\n" }
            when (FOOD) { grew = true; self.make_food()         }
        }

        # Create a new head
        grid[new_y][new_x] = [HEAD, dir]

        # Replace the current head with body
        grid[y][x] = [BODY, dir]

        # Update the head position
        head_pos = [new_y, new_x]

        # Move the tail
        if (!grew) {
            var (y, x) = tail_pos...

            var pos   = grid[y][x][1]
            var new_y = (y+pos[0] % h)
            var new_x = (x+pos[1] % w)

            grid[y][x][0]         = VOID    # erase the current tail
            grid[new_y][new_x][0] = TAIL    # create a new tail

            tail_pos = [new_y, new_x]
        }
    }

    method play {
        STDOUT.autoflush(true)
        readkey.ReadMode(3)

        try {
            loop {
                var key
                while (!defined(key = readkey.ReadLine(-1))) {
                    self.move()
                    self.display()
                    Sys.sleep(SLEEP_SEC)
                }

                given (key) {
                    when ("\e[A") { if (dir != DOWN ) { dir = UP    } }
                    when ("\e[B") { if (dir != UP   ) { dir = DOWN  } }
                    when ("\e[C") { if (dir != LEFT ) { dir = RIGHT } }
                    when ("\e[D") { if (dir != RIGHT) { dir = LEFT  } }
                }
            }
        }
        catch {
            readkey.ReadMode(0)
        }
    }
}

var w = `tput cols`.to_i
var h = `tput lines`.to_i

SnakeGame(w || 80, h || 24).play
```

