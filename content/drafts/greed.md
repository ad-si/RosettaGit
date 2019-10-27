+++
title = "Greed"
description = ""
date = 2019-10-07T18:35:28Z
aliases = []
[extra]
id = 20780
[taxonomies]
categories = []
tags = []
+++

{{draft task|Games}}
This task is about making a clone of the game "GREED" by Matthew Day.

This game is played on a grid of 79 column by 22 rows of random numbers from 1 to 9. The player location is signified by the '@' symbol.

The object of Greed is to erase as much of the screen as possible by moving around (all 8 directions are allowed) in this grid. When you move in a direction, you erase N number of grid squares in that direction, N being the first number in that direction. Your score reflects the total number of squares eaten.

You may not make a move that places you off the grid or over a previously eaten square. 

The game is over if there is no more valid moves.

[https://www.youtube.com/watch?v=XQHq6tdxylk&list=PLdvB7n7RN2UDkjHAWCmbQ8okmgSrjWcvE Video on YouTube]



## C++

Windows console version.
[[File:newGreedCpp.png|200px|thumb|right]]

```cpp

#include <windows.h>
#include <iostream>
#include <ctime>

const int WID = 79, HEI = 22;
const float NCOUNT = ( float )( WID * HEI );

class coord : public COORD {
public:
    coord( short x = 0, short y = 0 ) { set( x, y ); }
    void set( short x, short y ) { X = x; Y = y; }
};
class winConsole {
public:
    static winConsole* getInstamnce() { if( 0 == inst ) { inst = new winConsole(); } return inst; }
    void showCursor( bool s ) { CONSOLE_CURSOR_INFO ci = { 1, s }; SetConsoleCursorInfo( conOut, &ci ); }
    void setColor( WORD clr ) { SetConsoleTextAttribute( conOut, clr ); }
    void setCursor( coord p ) { SetConsoleCursorPosition( conOut, p ); }
    void flush() { FlushConsoleInputBuffer( conIn ); }
    void kill() { delete inst; }
private:
    winConsole() { conOut = GetStdHandle( STD_OUTPUT_HANDLE ); 
                   conIn  = GetStdHandle( STD_INPUT_HANDLE ); showCursor( false ); }
    static winConsole* inst;
    HANDLE conOut, conIn;
};
class greed {
public:
    greed() { console = winConsole::getInstamnce(); }
    ~greed() { console->kill(); }
    void play() {
        char g; do {
            console->showCursor( false ); createBoard();
            do { displayBoard(); getInput(); } while( existsMoves() );
            displayBoard(); console->setCursor( coord( 0, 24 ) ); console->setColor( 0x07 );
            console->setCursor( coord( 19,  8 ) ); std::cout << "+----------------------------------------+";
            console->setCursor( coord( 19,  9 ) ); std::cout << "|               GAME OVER                |";
            console->setCursor( coord( 19, 10 ) ); std::cout << "|            PLAY AGAIN(Y/N)?            |";
            console->setCursor( coord( 19, 11 ) ); std::cout << "+----------------------------------------+";
            console->setCursor( coord( 48, 10 ) ); console->showCursor( true ); console->flush(); std::cin >> g;
        } while( g == 'Y' || g == 'y' );
    }
private:
    void createBoard() {
        for( int y = 0; y < HEI; y++ ) {
            for( int x = 0; x < WID; x++ ) {
                brd[x + WID * y] = rand() % 9 + 1;
            }
        }
        cursor.set( rand() % WID, rand() % HEI );
        brd[cursor.X + WID * cursor.Y] = 0; score = 0;
        printScore();
    }
    void displayBoard() {
        console->setCursor( coord() ); int i;
		for( int y = 0; y < HEI; y++ ) {
            for( int x = 0; x < WID; x++ ) {
                i = brd[x + WID * y]; console->setColor( 6 + i );
                if( !i ) std::cout << " "; else std::cout << i;
            }
            std::cout << "\n";
        }
        console->setColor( 15 ); console->setCursor( cursor ); std::cout << "@";
    }
    void getInput() { 
        while( 1 ) {
            if( ( GetAsyncKeyState( 'Q' ) & 0x8000 ) && cursor.X > 0 && cursor.Y > 0 ) { execute( -1, -1 ); break; }
            if( ( GetAsyncKeyState( 'W' ) & 0x8000 ) &&  cursor.Y > 0 ) { execute( 0, -1 ); break; }
            if( ( GetAsyncKeyState( 'E' ) & 0x8000 ) && cursor.X < WID - 1 && cursor.Y > 0 ) { execute( 1, -1 ); break; }
            if( ( GetAsyncKeyState( 'A' ) & 0x8000 ) && cursor.X > 0 ) { execute( -1, 0 ); break; }
            if( ( GetAsyncKeyState( 'D' ) & 0x8000 ) && cursor.X < WID - 1 ) { execute( 1, 0 ); break; }
            if( ( GetAsyncKeyState( 'Y' ) & 0x8000 ) && cursor.X > 0 && cursor.Y < HEI - 1 ) { execute( -1, 1 ); break; }
            if( ( GetAsyncKeyState( 'X' ) & 0x8000 ) && cursor.Y < HEI - 1 ) { execute( 0, 1 ); break; }
            if( ( GetAsyncKeyState( 'C' ) & 0x8000 ) && cursor.X < WID - 1 && cursor.Y < HEI - 1 ) { execute( 1, 1 ); break; }
        }
        console->flush(); printScore();
    }
    void printScore() {
        console->setCursor( coord( 0, 24 ) ); console->setColor( 0x2a );
        std::cout << "      SCORE: " << score << " : " << score * 100.f / NCOUNT << "%      ";
    }
    void execute( int x, int y ) {
        int i = brd[cursor.X + x + WID * ( cursor.Y + y )];
        if( countSteps( i, x, y ) ) {
            score += i;
            while( i ) {
                --i; cursor.X += x; cursor.Y += y;
                brd[cursor.X + WID * cursor.Y] = 0;
            }
        }
    }
    bool countSteps( int i, int x, int y ) {
        coord t( cursor.X, cursor.Y );
        while( i ) {
            --i; t.X += x; t.Y += y;
            if( t.X < 0 || t.Y < 0 || t.X >= WID || t.Y >= HEI || !brd[t.X + WID * t.Y] ) return false;
        }
        return true;
    }
    bool existsMoves() {
        int i;
        for( int y = -1; y < 2; y++ ) {
            for( int x = -1; x < 2; x++ ) {
                if( !x && !y ) continue;
                i = brd[cursor.X + x + WID * ( cursor.Y + y )];
                if( i > 0 && countSteps( i, x, y ) ) return true;
            }
        }
        return false;
    }
    winConsole* console;
    int brd[WID * HEI];
    float score; coord cursor;
};
winConsole* winConsole::inst = 0;
int main( int argc, char* argv[] ) {
    srand( ( unsigned )time( 0 ) );
    SetConsoleTitle( "Greed" );
    greed g; g.play(); return 0;
}

```



## Factor

This uses Factor's own user interface vocabularies. Use hjkl-bnyu (vi-keys) to move.

```factor
USING: accessors arrays colors combinators
combinators.short-circuit fry grouping io io.styles kernel lexer
literals make math math.matrices math.parser math.vectors random
sequences strings ui ui.commands ui.gadgets.panes
ui.gadgets.status-bar ui.gadgets.worlds ui.gestures
ui.pens.solid ;
IN: rosetta-code.greed

<<
  SYNTAX: RGB: scan-token 2 cut 2 cut [ hex> 255 /f ] tri@ 1
  <rgba> suffix! ;
>>

CONSTANT: cells-width 79
CONSTANT: cells-height 22
CONSTANT: size 24
CONSTANT: bg-color RGB: 000000

CONSTANT: player-format {
    { font-size $ size }
    { foreground RGB: 5990C8 }
    { background RGB: B96646 }
}

CONSTANT: normal-format { { font-size $ size } }

CONSTANT: colors {
    RGB: 40B4A4
    RGB: 40B3B7
    RGB: 40A2B9
    RGB: 408FBC
    RGB: 407CBF
    RGB: 4268C0
    RGB: 4355C2
    RGB: 4845C3
    RGB: 5F46C4
}

CONSTANT: neighbors {
    { -1 -1 } { -1  0 } { -1  1 }
    {  0 -1 }           {  0  1 }
    {  1 -1 } {  1  0 } {  1  1 }
}

TUPLE: greed < pane cells x y score ;

: set-player ( greed elt -- )
    '[ y>> _ swap ] [ x>> 2array ] [ cells>> ] tri set-index ;

: place-player ( greed -- ) 0 set-player ;

: remove-player ( greed -- ) f set-player ;

: make-cells ( -- cells )
    cells-width cells-height * [ 9 random 1 + ] replicate
    cells-width group ;

: write-number ( n/f -- )
    [ >digit 1string normal-format first foreground ]
    [ 1 - colors nth 2array ] bi 2array format ;

: write-cell ( n/f -- )
    {
        { f [ " " normal-format format ] }
        { 0 [ "@" player-format format ] }
        [ write-number ]
    } case ;

: write-cells ( cells -- ) [ [ write-cell ] each nl ] each ;

: update-cells ( greed -- )
    dup cells>> [ write-cells ] curry with-pane ;

: init-greed ( greed -- greed' )
    make-cells >>cells cells-width random >>x cells-height
    random >>y 0 >>score dup place-player dup update-cells dup
    "Score: 0" swap show-status ;

: <greed> ( -- greed )
    f greed new-pane bg-color <solid> >>interior init-greed ;

: ?r,c ( r c matrix -- elt/f ) swapd ?nth ?nth ;
 
: ?r,cths ( seq matrix -- newseq )
    [ [ first2 ] dip ?r,c ] curry map ;

: (ray) ( start-loc dir length -- seq )
    1 + [ [ [ v+ ] keep over , ] times ] { } make 2nip ;

: ray ( start-loc dir length -- seq/f )
    dup [ (ray) ] [ 2nip ] if ;

: ?r,c-dir ( r c dir matrix -- n )
    [ 2array ] [ v+ first2 ] [ ?r,c ] tri* ;

: move-length ( greed dir -- n )
    [ [ y>> ] [ x>> ] [ ] tri ] dip swap cells>> ?r,c-dir ;

: y,x>loc ( greed -- loc ) [ y>> ] [ x>> ] bi 2array ;

: ray-dir ( greed dir -- seq )
    [ [ y,x>loc ] dip ] [ move-length ] 2bi ray ;

: in-bounds? ( dim loc -- ? )
    { [ nip [ 0 >= ] all? ] [ v- [ 0 > ] all? ] } 2&& ;

: endpoint-in-bounds? ( greed dir -- ? )
    ray-dir dup [
        last ${ cells-height cells-width } swap in-bounds?
    ] when ;

: gapless? ( greed dir -- ? )
    [ ray-dir ] [ drop cells>> ?r,cths ] 2bi [ integer? ] all? ;

: can-move? ( greed dir -- ? )
    { [ endpoint-in-bounds? ] [ gapless? ] } 2&& ;

: can-move-any? ( greed -- ? )
    neighbors [ can-move? ] with map [ t = ] any? ;

: setup-move ( greed dir -- seq ) over remove-player ray-dir ;

: update-score ( greed dir -- greed dir )
    2dup move-length pick swap [ + ] curry change-score dup
    score>> number>string "Score: " prepend swap show-status ;

: (move) ( greed dir -- )
    update-score [ drop f ] [ setup-move dup last ]
    [ drop cells>> swap [ set-indices ] dip ] 2tri first2
    [ >>y ] dip >>x place-player ;

: game-over ( greed -- )
    [
        score>> number>string "Game over! Final score: "
        prepend " Press <space> for new game." append
    ] [ show-status ] bi ;

: ?game-over ( greed -- )
    dup can-move-any? [ drop ] [ game-over ] if ;

: move ( greed dir -- )
    dupd 2dup can-move? [ (move) ] [ 2drop ] if
    [ update-cells ] [ ?game-over ] bi ;

: ?new-game ( greed -- )
    dup can-move-any? [ drop ] [ init-greed drop ] if ;

: e  ( greed -- ) {  0  1 } move ;
: se ( greed -- ) {  1  1 } move ;
: s  ( greed -- ) {  1  0 } move ;
: sw ( greed -- ) {  1 -1 } move ;
: w  ( greed -- ) {  0 -1 } move ;
: nw ( greed -- ) { -1 -1 } move ;
: n  ( greed -- ) { -1  0 } move ;
: ne ( greed -- ) { -1  1 } move ;

greed "gestures" f {
    { T{ key-down { sym "l" } } e  }
    { T{ key-down { sym "n" } } se }
    { T{ key-down { sym "j" } } s  }
    { T{ key-down { sym "b" } } sw }
    { T{ key-down { sym "h" } } w  }
    { T{ key-down { sym "y" } } nw }
    { T{ key-down { sym "k" } } n  }
    { T{ key-down { sym "u" } } ne }
    { T{ key-down { sym " " } } ?new-game }
} define-command-map

: greed-window ( -- )
    [
        <greed> <world-attributes> "Greed" >>title
        open-status-window
    ] with-ui ;

MAIN: greed-window
```

{{out}}
[https://i.imgur.com/3IEo8cC.png Screenshot of the game after a loss]


## Go

{{trans|C++}}
{{libheader|termbox-go}}
{{works with|Ubuntu 16.04}}


This hasn't been tested on Windows 10 but should work.

Note that this version uses the Z key (rather than the Y key) to move diagonally downwards to the left. A leave key, L, has also been added in case one wants to end the game prematurely.

```go
package main

import (
    "fmt"
    "github.com/nsf/termbox-go"
    "log"
    "math/rand"
    "strconv"
    "time"
)

type coord struct{ x, y int }

const (
    width  = 79
    height = 22
    nCount = float64(width * height)
)

var (
    board  [width * height]int
    score  = 0
    bold   = termbox.AttrBold
    cursor coord
)

var colors = [10]termbox.Attribute{
    termbox.ColorDefault,
    termbox.ColorWhite,
    termbox.ColorBlack | bold,
    termbox.ColorBlue | bold,
    termbox.ColorGreen | bold,
    termbox.ColorCyan | bold,
    termbox.ColorRed | bold,
    termbox.ColorMagenta | bold,
    termbox.ColorYellow | bold,
    termbox.ColorWhite | bold,
}

func printAt(x, y int, s string, fg, bg termbox.Attribute) {
    for _, r := range s {
        termbox.SetCell(x, y, r, fg, bg)
        x++
    }
}

func createBoard() {
    for y := 0; y < height; y++ {
        for x := 0; x < width; x++ {
            board[x+width*y] = rand.Intn(9) + 1
        }
    }
    cursor = coord{rand.Intn(width), rand.Intn(height)}
    board[cursor.x+width*cursor.y] = 0
    score = 0
    printScore()
}

func displayBoard() {
    termbox.SetCursor(0, 0)
    bg := colors[0]
    for y := 0; y < height; y++ {
        for x := 0; x < width; x++ {
            i := board[x+width*y]
            fg := colors[i]
            s := " "
            if i > 0 {
                s = strconv.Itoa(i)
            }
            printAt(x, y, s, fg, bg)
        }
    }
    fg := colors[9]
    termbox.SetCursor(cursor.x, cursor.y)
    printAt(cursor.x, cursor.y, "@", fg, bg)
    termbox.Flush()
}

func printScore() {
    termbox.SetCursor(0, 24)
    fg := colors[4]
    bg := termbox.ColorGreen
    s := fmt.Sprintf("      SCORE: %d : %.3f%%      ", score, float64(score)*100.0/nCount)
    printAt(0, 24, s, fg, bg)
    termbox.Flush()
}

func execute(x, y int) {
    i := board[cursor.x+x+width*(cursor.y+y)]
    if countSteps(i, x, y) {
        score += i
        for i != 0 {
            i--
            cursor.x += x
            cursor.y += y
            board[cursor.x+width*cursor.y] = 0
        }
    }
}

func countSteps(i, x, y int) bool {
    t := cursor
    for i != 0 {
        i--
        t.x += x
        t.y += y
        if t.x < 0 || t.y < 0 || t.x >= width || t.y >= height || board[t.x+width*t.y] == 0 {
            return false
        }
    }
    return true
}

func existsMoves() bool {
    for y := -1; y < 2; y++ {
        for x := -1; x < 2; x++ {
            if x == 0 && y == 0 {
                continue
            }
            ix := cursor.x + x + width*(cursor.y+y)
            i := 0
            if ix >= 0 && ix < len(board) {
                i = board[ix]
            }
            if i > 0 && countSteps(i, x, y) {
                return true
            }
        }
    }
    return false
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    err := termbox.Init()
    check(err)
    defer termbox.Close()

    eventQueue := make(chan termbox.Event)
    go func() {
        for {
            eventQueue <- termbox.PollEvent()
        }
    }()

    for {
        termbox.HideCursor()
        createBoard()
        for {
            displayBoard()
            select {
            case ev := <-eventQueue:
                if ev.Type == termbox.EventKey {
                    switch ev.Ch {
                    case 'q', 'Q':
                        if cursor.x > 0 && cursor.y > 0 {
                            execute(-1, -1)
                        }
                    case 'w', 'W':
                        if cursor.y > 0 {
                            execute(0, -1)
                        }
                    case 'e', 'E':
                        if cursor.x < width-1 && cursor.y > 0 {
                            execute(1, -1)
                        }
                    case 'a', 'A':
                        if cursor.x > 0 {
                            execute(-1, 0)
                        }
                    case 'd', 'D':
                        if cursor.x < width-1 {
                            execute(1, 0)
                        }
                    case 'z', 'Z':
                        if cursor.x > 0 && cursor.y < height-1 {
                            execute(-1, 1)
                        }
                    case 'x', 'X':
                        if cursor.y < height-1 {
                            execute(0, 1)
                        }
                    case 'c', 'C':
                        if cursor.x < width-1 && cursor.y < height-1 {
                            execute(1, 1)
                        }
                    case 'l', 'L': // leave key
                        return
                    }
                } else if ev.Type == termbox.EventResize {
                    termbox.Flush()
                }
            }
            printScore()
            if !existsMoves() {
                break
            }
        }
        displayBoard()
        fg := colors[7]
        bg := colors[0]
        printAt(19, 8, "+----------------------------------------+", fg, bg)
        printAt(19, 9, "|               GAME OVER                |", fg, bg)
        printAt(19, 10, "|            PLAY AGAIN(Y/N)?            |", fg, bg)
        printAt(19, 11, "+----------------------------------------+", fg, bg)
        termbox.SetCursor(48, 10)
        termbox.Flush()
        select {
        case ev := <-eventQueue:
            if ev.Type == termbox.EventKey {
                if ev.Ch == 'y' || ev.Ch == 'Y' {
                    break
                } else {
                    return
                }
            }
        }
    }
}
```



## Kotlin

{{trans|C++}}
{{works with|Windows 10}}
Note that this version uses the Z key (rather than the Y key) to move diagonally downwards to the left.

```scala
// Kotlin Native v0.5

import kotlinx.cinterop.*
import platform.posix.*
import platform.windows.*

const val WID = 79
const val HEI = 22
const val NCOUNT = (WID * HEI).toFloat()

class WinConsole {
    val conOut: HANDLE
    val conIn: HANDLE

    private constructor() {
        conOut = GetStdHandle(STD_OUTPUT_HANDLE)!!
        conIn  = GetStdHandle(STD_INPUT_HANDLE)!!
        showCursor(FALSE)
    }

    fun showCursor(s: WINBOOL) {
        memScoped {
            val ci = alloc<CONSOLE_CURSOR_INFO>().apply { dwSize = 1; bVisible = s }
            SetConsoleCursorInfo(conOut, ci.ptr)
        }
    }

    fun setColor(clr: WORD) = SetConsoleTextAttribute(conOut, clr)

    fun setCursor(p: COORD) = SetConsoleCursorPosition(conOut, p.readValue())

    fun flush() =  FlushConsoleInputBuffer(conIn)

    fun kill() {
        inst = null
    }

    companion object {
        val instance: WinConsole 
            get() {
                if (inst == null) inst = WinConsole()
                return inst!!
            }

        private var inst: WinConsole? = null
    }
}

class Greed {
    private val console: WinConsole
    private val brd = IntArray(WID * HEI)
    private var score = 0
    private lateinit var cursor: COORD

    init {
        console = WinConsole.instance
        SetConsoleTitleW("Greed")
    }

    fun destroy() {
        nativeHeap.free(cursor)
        console.kill()
    }

    fun play() {
        memScoped {
            val coord1 = alloc<COORD>().apply { X = 0;  Y = 24 }
            val coord2 = alloc<COORD>().apply { X = 19; Y = 8  }
            val coord3 = alloc<COORD>().apply { X = 19; Y = 9  }
            val coord4 = alloc<COORD>().apply { X = 19; Y = 10  }
            val coord5 = alloc<COORD>().apply { X = 19; Y = 11  }
            val coord6 = alloc<COORD>().apply { X = 48; Y = 10  }
            do {
                console.showCursor(FALSE)
                createBoard()
                do {
                    displayBoard()
                    getInput()
                }
                while (existsMoves())
                displayBoard()
                with (console) {
                    setCursor(coord1)
                    setColor(0x07)
                    setCursor(coord2);  print("+----------------------------------------+")
                    setCursor(coord3);  print("|               GAME OVER                |")
                    setCursor(coord4);  print("|            PLAY AGAIN(Y/N)?            |")
                    setCursor(coord5);  print("+----------------------------------------+")
                    setCursor(coord6)
                    showCursor(TRUE)
                    flush()
                }
                val g = readLine()!!.toUpperCase()
            }
            while (g.length >= 1 && g[0] == 'Y')
        }
        destroy()
    }

    private fun createBoard() {
        for (y in 0 until HEI) {
            for (x in 0 until WID) {
                brd[x + WID * y] = rand() % 9 + 1
            }
        }
        cursor = nativeHeap.alloc<COORD>().apply {
            X = (rand() % WID).toShort(); Y = (rand() % HEI).toShort()
        }
        brd[cursor.X + WID * cursor.Y] = 0
        score = 0
        printScore()
    }

    private fun displayBoard() {
        memScoped {
            val coord = alloc<COORD>().apply { X = 0; Y = 0 }
            console.setCursor(coord)
        }
        for (y in 0 until HEI) {
            for (x in 0 until WID) {
                val i = brd[x + WID * y]
                console.setColor((6 + i).toShort())
                print(if (i == 0) " " else "$i")
            }
            println()
        }
        console.setColor(15)
        console.setCursor(cursor)
        print("@")
    }

    private fun checkKey(k: Char) = (GetAsyncKeyState(k.toInt()).toInt() and 0x8000) != 0

    private fun getInput() {
        while (true) {
            if (checkKey('Q') && cursor.X > 0 && cursor.Y > 0) { execute(-1, -1); break }
            if (checkKey('W') && cursor.Y > 0) { execute(0, -1); break }
            if (checkKey('E') && cursor.X < WID - 1 && cursor.Y > 0) { execute(1, -1); break }
            if (checkKey('A') && cursor.X > 0) { execute(-1, 0); break }
            if (checkKey('D') && cursor.X < WID - 1) { execute(1, 0); break }
            if (checkKey('Z') && cursor.X > 0 && cursor.Y < HEI - 1) { execute(-1, 1); break }
            if (checkKey('X') && cursor.Y < HEI - 1) { execute(0, 1); break }
            if (checkKey('C') && cursor.X < WID - 1 && cursor.Y < HEI - 1) { execute(1, 1); break }
        }
        console.flush()
        printScore()
    }

    private fun printScore() {
        memScoped {
            val coord = alloc<COORD>().apply { X = 0; Y = 24 }
            console.setCursor(coord)
        }
        console.setColor(0x2a)
        print("      SCORE: $score :  ${score * 100.0f / NCOUNT}%      ")
    }

    private fun execute(x: Int, y: Int) {
        var i = brd[cursor.X + x + WID * ( cursor.Y + y )]
        if (countSteps(i, x, y)) {
            score += i
            while (i-- != 0) {
                cursor.X = (cursor.X + x).toShort()
                cursor.Y = (cursor.Y + y).toShort()
                brd[cursor.X + WID * cursor.Y] = 0
            }
        }
    }

    private fun countSteps(i: Int, x: Int, y: Int): Boolean {
        var ii = i
        memScoped {
            val t = alloc<COORD>().apply { X = cursor.X; Y = cursor.Y }
            while (ii-- != 0) {
                t.X = (t.X + x).toShort()
                t.Y = (t.Y + y).toShort()
                if (t.X < 0 || t.Y < 0 || t.X >= WID || t.Y >= HEI || brd[t.X + WID * t.Y] == 0 ) return false
            }
        }
        return true
    }

    private fun existsMoves(): Boolean {
        for (y in -1..1) {
            for (x in -1..1) {
                if (x == 0 && y == 0) continue
                val i = brd[cursor.X + x + WID * ( cursor.Y + y )]
                if (i > 0 && countSteps(i, x, y)) return true
            }
        }
        return false
    }
}

fun main(args: Array<String>) {
    srand(time(null).toInt())
    Greed().play()
}
```



## Java


See [[Greed/Java]].






## Julia

GUI version. Click a square adjacent to the "@" symbol to move.

```julia
using Gtk

struct BState
    board::Matrix{Int}
    row::Int
    col::Int
end

function greedapp(r, c)
    rows, cols = c, r  # gtk rotates grid 90 degrees
    win = GtkWindow("Greed Game", 1200, 400) |> (GtkFrame() |> (box = GtkBox(:v)))
    toolbar = GtkToolbar()
    newgame = GtkToolButton("New Game")
    set_gtk_property!(newgame, :label, "New Game")
    set_gtk_property!(newgame, :is_important, true)
    undomove = GtkToolButton("Undo Move")
    set_gtk_property!(undomove, :label, "Undo Move")
    set_gtk_property!(undomove, :is_important, true)
    map(w->push!(toolbar,w),[newgame,undomove])
    scrwin = GtkScrolledWindow()
    grid = GtkGrid()
    map(w -> push!(box, w),[toolbar, scrwin])
    push!(scrwin, grid)
    buttons = Array{Gtk.GtkButtonLeaf, 2}(undef, rows, cols)
    for i in 1:rows, j in 1:cols
        grid[i,j] = buttons[i,j] = GtkButton()
        set_gtk_property!(buttons[i,j], :expand, true)
    end
    function findrowcol(button)
        for i in 1:rows, j in 1:cols
            if buttons[i, j] == button
                return i, j
            end
        end
        return 0, 0
    end
    board = zeros(Int, rows, cols)
    pastboardstates = Vector{BState}()
    score = 0
    condition = Condition()
    won = ""
    myrow, mycol = 1, 1
    function update!()
        for i in 1:rows, j in 1:cols
            label = (board[i, j] > 0) ? board[i, j] : " "
            set_gtk_property!(buttons[i, j], :label, label)
        end
        set_gtk_property!(buttons[myrow, mycol], :label, "@")
        won = all(iszero, board) ? "WINNING" : ""
        set_gtk_property!(win, :title, "$won Greed Game  (Score: $score)")
    end
    function erasefromtile!(moverow, movecol)
        xdir, ydir = moverow - myrow, movecol - mycol
        if abs(xdir) > 1 || abs(ydir) > 1 || 0 == xdir == ydir || board[moverow, movecol] == 0
            return
        end
        push!(pastboardstates, BState(deepcopy(board), myrow, mycol))
        for i in 1:board[moverow, movecol]
            x, y = myrow + xdir * i, mycol + ydir * i
            if 0 < x <= rows && 0 < y <= cols
                board[x, y] = 0
                score += 1
            end
        end
        board[myrow, mycol] = 0
        myrow = moverow
        mycol = movecol
        update!()
    end
    clicked(button) = begin x, y = findrowcol(button); erasefromtile!(x, y)  end
    function initialize!(w)
        won = ""
        possiblevals = collect(1:9)
        for i in 1:rows, j in 1:cols
            board[i, j] = rand(possiblevals)
            set_gtk_property!(buttons[i,j], :label, board[i, j])
            signal_connect(clicked, buttons[i, j], "clicked")
        end
        myrow = rand(1:rows)
        mycol = rand(1:cols)
        board[myrow, mycol] = 0
        update!()
    end
    function undo!(w)
        if won == "" && length(pastboardstates) > 0
            bst = pop!(pastboardstates)
            board, myrow, mycol = bst.board, bst.row, bst.col
            update!()
        end
    end
    endit(w) = notify(condition)
    initialize!(win)
    signal_connect(initialize!, newgame, :clicked)
    signal_connect(undo!, undomove, :clicked)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(condition)
end

# greedapp(22, 79)  # This would be per task, though a smaller game board is nicer
greedapp(12, 29)

```



## Phix

{{trans|C++}}

```Phix
constant W = 79, H = 22, NCOUNT = W*H

sequence board 
integer X, Y, score

procedure printScore()
    position(25,1); bk_color(2); text_color(10)
    printf(1,"      SCORE: %d : %f%%      ",{score,score*100/NCOUNT});
end procedure

procedure createBoard()
    board = repeat(repeat('0',W),H)
    for y=1 to H do
        for x=1 to W do
            board[y,x] = '0'+rand(9)
        end for
    end for
    X = rand(W); Y = rand(H);
    board[Y,X] = '0'; score = 0;
    printScore();
end procedure

procedure displayBoard()
    position(1,1)
    bk_color(2)
    for y=1 to H do
        for x=1 to W do
            integer ch = board[y,x]; 
            text_color(iff(ch=' '?6:6+ch-'0'))
            puts(1,ch)
        end for
        puts(1,"\n")
    end for
    bk_color(4); text_color(15); position(Y,X); puts(1,"@")
end procedure

function countSteps(integer i, x, y)
    integer tX = X, tY = Y
    while i do
        i -= 1; tX += x; tY += y;
        if tX<1 or tY<1 or tX>W or tY>H or board[tY,tX]=' ' then return false end if
    end while
    return true;
end function

procedure execute(integer x, y)
    integer ch = board[Y+y,X+x],
            i = iff(ch=' '?0:ch-'0')
    if countSteps(i, x, y) then
        score += i
        while i do
            i -= 1; X += x; Y += y;
            board[Y,X] = ' ';
        end while
    end if
end procedure

procedure getInput()
    while true do
        integer k = upper(wait_key())
        if    k='Q' and X > 1 and Y > 1 then execute(-1,-1) exit
        elsif k='W'           and Y > 1 then execute( 0,-1) exit
        elsif k='E' and X < W and Y > 1 then execute( 1,-1) exit
        elsif k='A' and X > 1           then execute(-1, 0) exit
        elsif k='D' and X < W           then execute( 1, 0) exit
        elsif k='Z' and X > 1 and Y < H then execute(-1, 1) exit
        elsif k='X'           and Y < H then execute( 0, 1) exit
        elsif k='C' and X < W and Y < H then execute( 1, 1) exit
        end if
    end while
    printScore();
end procedure

function existsMoves()
    for y=-1 to +1 do
        for x=-1 to +1 do
            if (x or y)
            and X+x>=1 and X+x<=W
            and Y+y>=1 and Y+y<=H then
                integer ch = board[Y+y,X+x];
                if ch!=' ' and countSteps(ch-'0', x, y) then
                    return true
                end if
            end if
        end for
    end for
    return false;
end function

procedure play()
    while true do
        cursor(NO_CURSOR); createBoard();
        while true do 
            displayBoard(); getInput() 
            if not existsMoves() then exit end if 
        end while
        displayBoard(); text_color(7);
        position( 8,19); puts(1,"+----------------------------------------+");
        position( 9,19); puts(1,"|               GAME OVER                |");
        position(10,19); puts(1,"|            PLAY AGAIN(Y/N)?            |");
        position(11,19); puts(1,"+----------------------------------------+");
        position(10,48); cursor(BLOCK_CURSOR);
        if upper(wait_key())!='Y' then return end if
    end while
end procedure
play()
```



## REXX

This REXX version's only dependency is that the DOS command   '''cls'''   is used to clear the terminal screen. 

No attempt was made to validate the input the input arguments (parameters) for this REXX program.

Pointers (above and to the right of) the grid are included to help identify where the current location is.

```rexx
/*REXX program lets a user play the game of  GREED  (by Matthew Day)  from the console. */
parse arg sw sd @ b ?r .                         /*obtain optional argumenst from the CL*/
if sw=='' | sw==","   then sw= 79                /*cols specified?  Then use the default*/
if sd=='' | sd==","   then sd= 22                /*rows     "         "   "   "     "   */
if  @=='' |  @==","   then @= '@'                /*here     "         "   "   "     "   */
if  b=='' |  b==","   then b= ' '                /*blank    "         "   "   "     "   */
if datatype(?r, 'W')  then call random ,,?r      /*maybe use a  seed  for the RANDOM BIF*/
if length(@)==2 & datatype(@,'X')  then @=x2c(@) /*maybe use @  char for current pos.   */
if length(b)==2 & datatype(b,'X')  then b=x2c(b) /*  "    "  B  char for background.    */
signal on halt                                   /*handle pressing of  Ctrl-Break  key. */
call init                                        /* [↓]  CLR  is reset if there's an err*/
clr=1;    do  until  # == sw*sd;        ??=      /*keep playing until the grid is blank.*/
          call show clr                          /*show the playing field (grid) to term*/
          call ask;                  clr= 1      /*obtain user's move, validate, or quit*/
          if \move()  then do;       clr= 0      /*perform the user's move per @ loc.*/
                           if ??==@. then say ____  "invalid move:  moving out of bounds."
                           if ??==b  then say ____  "invalid move:  moving into a blank."
                           end
          call show 0
          end   /*until*/                        /* [↑]  Also, if out─of─bounds, LEAVE. */
      if show(1)==sw*sd  then say ____ "You've won, the grid is blank,  your score is: " #
      exit  2
exit 0                                           /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ask:      do forever                             /*play 'til done, or no possible moves.*/
          say  ____  'moves:'  ____  '   Q= ◄↑   W= ↑    E= ►↑'
          say  ____  'moves:'  ____  '   A= ◄            D= ►'
          say  ____  'moves:'  ____  '   Z= ◄↓   X= ↓    C= ►↓'
          say  ____
          say  ____ 'enter a move     ──or──     QUIT          (the score is: '   #")"
          parse pull  z  2  1  what  .  1  oz;                   upper z what
          if abbrev('QUIT', what, 2) | abbrev("QQUIT", what, 2)  then leave
          if length( space(oz) )==1  &  pos(z, 'QWEADZXC')\==0   then return
          say ____ '***error*** invalid direction for a move:'  space(oz);            say
          end   /*forever*/
halt: say;      say ____ 'quitting.';           exit 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
init: @.= 'ff'x;  $.=.;     ____= copies("─", 8) /*out─of─bounds literal; fence for SAYs*/
          do   r=1  for sd
            do c=1  for sw;  @.r.c= random(1, 9) /*assign grid area to random digs (1►9)*/
            end   /*c*/
          end     /*r*/
      !r= random(1, sd);  !c= random(1, sw);  @.!r.!c= @;   return /*assign 1st position*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
move: @.!r.!c= '≤';   $r= !r;   $c= !c;    ??=   /*blank out this  "start"  position.   */
      @@=.                                       /*nullify the count of move positions. */
          do until @@==0;       select
                                when z== 'Q'  then do;   !r= !r - 1;    !c= !c - 1;    end
                                when z== 'W'  then       !r= !r - 1
                                when z== 'E'  then do;   !r= !r - 1;    !c= !c + 1;    end
                                when z== 'A'  then                      !c= !c - 1
                                when z== 'D'  then                      !c= !c + 1
                                when z== 'Z'  then do;   !r= !r + 1;    !c= !c - 1;    end
                                when z== 'X'  then       !r= !r + 1
                                when z== 'C'  then do;   !r= !r + 1;    !c= !c + 1;    end
                                end   /*select*/
          ?= @.!r.!c;    if ?==@. | ?==b  then do;  !r= $r;   !c= $c;   ??= ?;   return 0
                                               end
          if @@==.  then @@=?;   if datatype(@@, 'W')  then @@= @@ - 1   /*diminish cnt.*/
          @.!r.!c= '±'                           /*nullify  (later, a blank)  position. */
          end   /*until*/
      @.!r.!c= @;                    return 1    /*signify current grid position with @ */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: arg tell;     #=0;         if tell  then do;   "CLS";    say left('', !c)"↓";    end
          do   r=1  for sd;            _= ' '    /* [↑]  DOS cmd  CLS  clears the screen*/
            do c=1  for sw                       /*construct row of the grid for display*/
            if @.r.c=="±" & ??\==''  then @.r.c= $.r.c  /*Is this a temp blank?  Restore*/
            if @.r.c=="±" & ?? ==''  then @.r.c= b      /*Is this a temp blank?  Blank. */
            if @.r.c=="≤" & ??\==''  then @.r.c= $.r.c  /*Is this a temp a  @ ?  Restore*/
            if @.r.c=="≤" & ?? ==''  then @.r.c= b      /*Is this a temp a  @ ?  Blank. */
            ?= @.r.c;                _= _ || ?   /*construct a line of the grid for term*/
            if ?==b | ?==@  then #= # + 1        /*Position==b ─or─ @?  Then bump score.*/
            if tell         then $.r.c= @.r.c    /*create a backup grid for re─instating*/
            end   /*c*/
          if r==!r  then _= _ '◄'                /*indicate   row  of current position. */
          if tell   then say _                   /*display   a row of grid to screen.   */
          end     /*r*/; say;        return #    /*SHOW also counts # of blanks (score).*/
```

A note on the OUTPUT sections:   each (cleared) screen displayed is shown below as a separate OUTPUT section.

The following are the screen shots when inputs used   (size of the grid)   are:     <tt> 22   10 </tt>

{{out|output|text=:     the 1<sup>st</sup> screen shown.}}

```txt

                                                          ↓
 1636166561333644938615925878672969839136949348125385742112849651343354296271245
 4935939188413836477495369151362748736256329449564639583731265554747438655579797
 2761827294343258918258167935625127433626644177165772453435474591949917695547965
 5336784646373682676398688475989972451499776252164989899239191733912697265898925
 4952948995581413589577455233495962736898536553933712711747529619371573895413265
 5328643745672485468516645326176482571162377128958669252244431799914145324756787
 9682648416475828434376154259111596818112819626518754715385939211764235211148126
 4771918124154627513339665771138169237888886368882335865655526894655352121961215
 794644718989445262471866768299551827168758297323537929749@815519895387457566428 ◄
 9347969832617624113866732722842121521854745888458198852913265875445986923272597

──────── moves: ────────    Q= ◄↑   W= ↑    E= ►↑
──────── moves: ────────    A= ◄            D= ►
──────── moves: ────────    Z= ◄↓   X= ↓    C= ►↓
────────
──────── enter a move     ──or──  enter   QUIT   to quit.  (score is:  1)
e                       ◄■■■■■■■■■■■■■ user input

```

{{out|output|text=:     the 2<sup>nd</sup> screen shown.}}

```txt

                                                               ↓
 1636166561333644938615925878672969839136949348125385742112849651343354296271245
 4935939188413836477495369151362748736256329449564639583731265554747438655579797
 2761827294343258918258167935625127433626644177165772453435474591949917695547965
 53367846463736826763986884759899724514997762521649898992391917@3912697265898925 ◄
 4952948995581413589577455233495962736898536553933712711747529 19371573895413265
 532864374567248546851664532617648257116237712895866925224443 799914145324756787
 96826484164758284343761542591115968181128196265187547153859 9211764235211148126
 4771918124154627513339665771138169237888886368882335865655 26894655352121961215
 794644718989445262471866768299551827168758297323537929749 815519895387457566428
 9347969832617624113866732722842121521854745888458198852913265875445986923272597 

──────── moves: ────────    Q= ◄↑   W= ↑    E= ►↑
──────── moves: ────────    A= ◄            D= ►
──────── moves: ────────    Z= ◄↓   X= ↓    C= ►↓
────────
──────── enter a move     ──or──  enter   QUIT   to quit.  (score is:  6)
quit                    ◄■■■■■■■■■■■■■ user input

──────── quitting. 

```

