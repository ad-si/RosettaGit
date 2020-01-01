+++
title = "Terminal control/Cursor movement"
description = ""
date = 2019-08-29T16:47:12Z
aliases = []
[extra]
id = 10052
[taxonomies]
categories = []
tags = []
+++

{{task|Terminal control}}

;Task:
Demonstrate how to achieve movement of the terminal cursor:
:* how to move the cursor one position to the left
:* how to move the cursor one position to the right
:* how to move the cursor up one line (without affecting its horizontal position)
:* how to move the cursor down one line (without affecting its horizontal position)
:* how to move the cursor to the beginning of the line
:* how to move the cursor to the end of the line
:* how to move the cursor to the top left corner of the screen
:* how to move the cursor to the bottom right corner of the screen



For the purpose of this task, it is not permitted to overwrite any characters or attributes on any part of the screen (so outputting a space is not a suitable solution to achieve a movement to the right).


;Handling of out of bounds locomotion
This task has no specific requirements to trap or correct cursor movement beyond the terminal boundaries, so the implementer should decide what behavior fits best in terms of the chosen language.   Explanatory notes may be added to clarify how an out of bounds action would behave and the generation of error messages relating to an out of bounds cursor position is permitted.




## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program cursorMove.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

/* Initialized data */
.data
szMessStartPgm:            .asciz "Program start \n"
szMessEndPgm:              .asciz "Program normal end.\n"
szMessColorRed:            .asciz "Color red.\n"
szCodeInit:                .asciz "\033[0m"                    @ color reinit
szCodeRed:                 .asciz "\033[31m"                   @ color red
szCodeBlue:                .asciz "\033[34m"                   @ color blue
szMessMove:                .asciz "\033[A\033[6CBlue Message up and 6 location right."
szMessMoveDown:            .asciz "\033[31m\033[BRed text location down"
szMessTopLeft:             .asciz "\033[;HTOP LEFT"
szCarriageReturn:          .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:

    ldr r0,iAdrszMessStartPgm                   @ display start message
    bl affichageMess
    ldr r0,iAdrszCodeRed                        @ color red
    bl affichageMess
    ldr r0,iAdrszMessColorRed
    bl affichageMess
    ldr r0,iAdrszCodeBlue
    bl affichageMess
    ldr r0,iAdrszMessMove
    bl affichageMess
    ldr r0,iAdrszMessMoveDown                   @ move pointer down
    bl affichageMess
    ldr r0,iAdrszMessTopLeft
    bl affichageMess
    ldr r0,iAdrszCarriageReturn                 @ start next line
    bl affichageMess
    ldr r0,iAdrszCodeInit                       @ color reinitialize
    bl affichageMess
    ldr r0,iAdrszMessEndPgm                     @ display end message
    bl affichageMess

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessStartPgm:        .int szMessStartPgm
iAdrszMessEndPgm:          .int szMessEndPgm
iAdrszCodeInit:            .int szCodeInit
iAdrszCodeRed:             .int szCodeRed
iAdrszCodeBlue:            .int szCodeBlue
iAdrszMessColorRed:        .int szMessColorRed
iAdrszMessMove:            .int szMessMove
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrszMessMoveDown:        .int szMessMoveDown
iAdrszMessTopLeft:         .int szMessTopLeft

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return


```


## AutoHotkey



```autohotkey
DllCall("AllocConsole")
hConsole:=DllCall("GetConsoleWindow","UPtr")
Stdout:=FileOpen(DllCall("GetStdHandle", "int", -11, "ptr"), "h `n")
Stdin:=FileOpen(DllCall("GetStdHandle", "int", -10, "ptr"), "h `n")

;move the cursor one position to the left
GetPos(x,y)
SetPos(x-1)

;move the cursor one position to the right
GetPos(x,y)
SetPos(x+1)

;move the cursor up one line (without affecting its horizontal position)
GetPos(x,y)
SetPos(x,y-1)

;move the cursor down one line (without affecting its horizontal position)
GetPos(x,y)
SetPos(x,y+1)

;move the cursor to the beginning of the line
GetPos(x,y)
SetPos(0,y)

;move the cursor to the end of the line
;requires previous knowledge of screen width -- typically 80
SetPos(79) ;minus 1 because origin is (0,0)

;move the cursor to the top left corner of the screen
SetPos(0,0)

;move the cursor to the bottom right corner of the screen
GetConsoleSize(w,h)
SetPos(w-1,h-1) ;minus 1 because origin is (0,0)

GetPos(ByRef x, ByRef y) {
	global Stdout
	VarSetCapacity(struct,22,0)
	e:=DllCall("GetConsoleScreenBufferInfo","UPtr",Stdout.__Handle,"Ptr",&struct)
	if (!e) or (ErrorLevel)
			return 0 ;Failure
	x:=NumGet(&struct,4,"UShort")
	y:=NumGet(&struct,6,"UShort")
	return 1
}

SetPos(x="",y="") {
	global Stdout
	GetPos(ox,oy)
	if x is not Integer
			x:=ox
	if y is not Integer
			y:=oy
	VarSetCapacity(struct,4,0)
	Numput(x,struct,"UShort")
	Numput(y,struct,2,"UShort")
	e:=DllCall("SetConsoleCursorPosition","Ptr",Stdout.__Handle,"uint",Numget(struct,"uint"))
	if (!e) or (ErrorLevel)
			return 0 ;Failure
	return 1
}

GetConsoleSize(ByRef bufferwidth, ByRef bufferheight) {
	global Stdout
	VarSetCapacity(struct,22,0)
	x:=DllCall("GetConsoleScreenBufferInfo","UPtr",Stdout.__Handle,"Ptr",&struct)
	if (!x) or (ErrorLevel)
		return 0 ;Failure
	bufferwidth:=NumGet(&struct,"UShort")
	bufferheight:=NumGet(&struct,2,"UShort")
	return 1
}
```



## Axe

Axe does not allow relative movement of the cursor. However, if the current position is known in the X and Y variables, the behavior can be simulated.

```axe
Output(X-1,Y)
Output(X+1,Y)
Output(X,Y-1)
Output(X,Y+1)
Output(0,Y)
Output(15,Y)
Output(0,0)
Output(15,7)
```



## BASIC

{{works with|GW-BASIC}}
{{works with|QBasic}}


```qbasic
10 'move left
20 LOCATE , POS(0) - 1
30 'move right
40 LOCATE , POS(0) + 1
50 'move up
60 LOCATE CSRLIN - 1
70 'move down
80 LOCATE CSRLIN + 1
900 'beginning of line
100 LOCATE , 1
110 'end of line; requires previous knowledge of screen width -- typically 80
120 LOCATE , 80
130 'top left corner
140 LOCATE 1, 1
150 'bottom right corner; requires knowledge of screen dimensions (80x25 here)
160 LOCATE 25, 80
```

=
## Applesoft BASIC
=
80-Column Text Card: Applesoft Control Codes
http://support.apple.com/kb/TA33130

Apple II Family Identification
http://www.umich.edu/~archive/apple2/technotes/tn/misc/TN.MISC.007

```ApplesoftBasic
REM APPLE II GS ?
100 DATA56,32,31,254,160,0
110 DATA176,1,136,140,13,3,96
120 FOR I = 768 TO 780
130     READ B: POKE I,B
140 NEXT : CALL 768
150 IF PEEK (781) THEN 190

160 B =  PEEK (64435) : T4 = 1

REM APPLE II ?
170 IF B =  56 THEN 200

REM APPLE II PLUS ?
180 IF B = 234 THEN 200

REM NOT 80 COLUMN MODE ?
190 T4 =  PEEK (49183) < 128

REM START HERE
200 VTAB 12: HTAB 21
210 PRINT ">"; : UP = -998

REM LEFT
220 PRINT CHR$(8); : GET A$

REM RIGHT
230 IF T4 THEN CALL  -1036
240 IF NOT T4 THEN PRINT CHR$(28);
250 GET A$

REM UP
260 CALL UP : GET A$

REM DOWN / LINE FEED
270 PRINT CHR$(10); : GET A$

REM BEGINNING OF LINE
300 HTAB 1 : ON T4 GOTO 350
310 IT = PEEK(37)
320 IF IT THEN CALL UP
330 PRINT
340 IF NOT IT THEN CALL UP
350 GET A$

REM END OF LINE
370 HTAB PEEK(33)
380 GET A$

REM TOP LEFT
400 IF T4 THEN VTAB 1 : HTAB 1
410 PRINT CHR$(25); : GET A$

REM BOTTOM RIGHT
420 CALL  -1233
430 HTAB PEEK(33)
440 GET A$

```



## BBC BASIC


```bbcbasic
      VDU 8    : REM Move one position to the left
      VDU 9    : REM Move one position to the right
      VDU 11   : REM Move up one line
      VDU 10   : REM Move down one line
      VDU 13   : REM Move to the beginning of the line
      VDU 30   : REM Move to the top left corner
      VDU 23,16,16;0;0;0; : REM Disable scrolling
      VDU 13,8,10 : REM Move to the end of the line
      VDU 30,8 : REM Move to the bottom right corner
      VDU 23,16,0;0;0;0; : REM Enable scrolling
```



## Befunge


Assuming a terminal with support for ANSI escape sequences, you can move the cursor position with the code fragments below.

{|style="text-align: left;"
!Move left one column:
|<code>"D["39*,,,</code>
|-
!Move right one column:
|<code>"C["39*,,,</code>
|-
!Move up one line:
|<code>"A["39*,,,</code>
|-
!Move down one line:
|<code>"B["39*,,,</code>
|-
!Move to start of line:
|<code>94+,</code>
|-
!Move to end of line:
|<code>"C999["39*,,,,,,</code>
|-
!Move to top left:
|<code>"H["39*,,,</code>
|-
!Move to bottom right:
|<code>0"H999;999["39*>:#,_$</code>
|}

Note that the ''end of line'' movement is achieved by moving right 999 columns, and relies on the fact that the terminal will clamp the movement range to the width of the screen. Similarly, the ''bottom right'' movement is achieved by setting the cursor position to the 999th column of the 999th row, which again is clamped to the maximum width and height of the screen.

The ''start of line'' movement is simply a carriage return (ASCII 13).


## C

The conio.h header file in Borland's Turbo C makes keyboard interaction very simple. The following is an interactive program which has been tested with Turbo C, the delay function takes milliseconds and has been used to animate the involved cases.


```C

#include<conio.h>
#include<dos.h>

char *strings[] = {"The cursor will move one position to the left",
  		   "The cursor will move one position to the right",
 		   "The cursor will move vetically up one line",
 		   "The cursor will move vertically down one line",
 		   "The cursor will move to the beginning of the line",
 		   "The cursor will move to the end of the line",
 		   "The cursor will move to the top left corner of the screen",
 		   "The cursor will move to the bottom right corner of the screen"};

int main()
{
	int i,j,MAXROW,MAXCOL;
	struct text_info tInfo;
	gettextinfo(&tInfo);
	MAXROW = tInfo.screenheight;
	MAXCOL = tInfo.screenwidth;

	clrscr();
	cprintf("This is a demonstration of cursor control using gotoxy(). Press any key to continue.");
	getch();

	for(i=0;i<8;i++)
	{
		clrscr();
		gotoxy(5,MAXROW/2);

		cprintf("%s",strings[i]);
		getch();

		switch(i){
			case 0:gotoxy(wherex()-1,wherey());
			break;
			case 1:gotoxy(wherex()+1,wherey());
			break;
			case 2:gotoxy(wherex(),wherey()-1);
			break;
			case 3:gotoxy(wherex(),wherey()+1);
			break;
			case 4:for(j=0;j<strlen(strings[i]);j++){
				   gotoxy(wherex()-1,wherey());
				   delay(100);
			       }
			break;
			case 5:gotoxy(wherex()-strlen(strings[i]),wherey());
			       for(j=0;j<strlen(strings[i]);j++){
				   gotoxy(wherex()+1,wherey());
				   delay(100);
			       }
			break;
			case 6:while(wherex()!=1)
			       {
				     gotoxy(wherex()-1,wherey());
				     delay(100);
		               }
			       while(wherey()!=1)
			       {
			             gotoxy(wherex(),wherey()-1);
			             delay(100);
			       }
			break;
			case 7:while(wherex()!=MAXCOL)
			       {
				     gotoxy(wherex()+1,wherey());
				     delay(100);
			       }
			       while(wherey()!=MAXROW)
			       {
				     gotoxy(wherex(),wherey()+1);
				     delay(100);
			       }
			break;
			};
			getch();
	}

	clrscr();
	cprintf("End of demonstration.");
	getch();
	return 0;
}

```


## C#
{{works with|Mono|1.2}}
{{works with|Visual C sharp|Visual C#|2003}}

```c#
static void Main(string[] args)
{
    //There will be a 3 second pause between each cursor movement.
    Console.Write("\n\n\n\n     Cursor is here -->   ");
    System.Threading.Thread.Sleep(3000);
    Console.CursorLeft = Console.CursorLeft - 1; //Console.CursorLeft += -1 is an alternative.
    System.Threading.Thread.Sleep(3000);
    Console.CursorLeft = Console.CursorLeft + 1;
    System.Threading.Thread.Sleep(3000);
    Console.CursorTop = Console.CursorTop - 1;
    System.Threading.Thread.Sleep(3000);
    Console.CursorTop = Console.CursorTop + 1;
    System.Threading.Thread.Sleep(3000);
    Console.CursorLeft = 0; //Move the cursor far left.
    System.Threading.Thread.Sleep(3000);
    Console.CursorLeft = Console.BufferWidth - 1;
    /* BufferWidth represents the number of characters wide the console area is.
        * The exact value may vary on different systems.
        * As the cursor position is a 0 based index we must subtract 1 from buffer width or we move the cursor out of bounds.
        * In some cases WindowWidth may be preferable (however in this demonstration window and buffer should be the same).
        */
    System.Threading.Thread.Sleep(3000);
    Console.SetCursorPosition(0,0); //I have used an alternative method for moving the cursor here which I feel is cleaner for the task at hand.
    System.Threading.Thread.Sleep(3000);
    Console.SetCursorPosition(Console.BufferWidth-1, Console.WindowHeight-1); //Buffer height is usually longer than the window so window has been used instead.
    System.Threading.Thread.Sleep(3000);
}

```




## Forth

ANS/ISO Forth has a cursor positioning function called AT-XY. The standard language does not define how the cursor is managed however most systems give the programmer direct access to a pair of variables for cursor row and column position.

The following example assumes we are using a terminal that accepts ANSI escape codes. It defines the ANSI codes as Forth words with a markup language look.  With this code compiled into the Forth system, the commands are used like native Forth commands.
<LANG FORTH>( ANSI terminal control lexicon )
DECIMAL

( support routines)
 27 CONSTANT ESC
: <##>    ( n -- ) ( sends n, radix 10, no spaces)
          BASE @ >R DECIMAL  0 <# #S #> TYPE   R> BASE ! ;

: ESC[   ( -- )   ESC EMIT ." [" ;

( ANSI terminal commands as Forth words)
: <CUU>  ( row --) ESC[ <##> ." A" ;
: <CUD>  ( row --) ESC[ <##> ." B" ;
: <CUF>  ( col --) ESC[ <##> ." C" ;
: <CUB>  ( col --) ESC[ <##> ." D" ;
: <CPL>  ( -- )    ESC[ <##> ." F" ;
: <CHA>  ( n --)   ESC[ <##> ." G" ;
: <EL>   ( -- )    ESC[ ." K" ;
: <ED>   ( -- )    ESC[ ." 2J" ;
: <CUP>  ( row col -- ) SWAP ESC[ <##> ." ;" <##> ." H"  ;

( Define ANSI Forth names for these functions using our markup words)
: AT-XY ( col row  -- ) SWAP <CUP> ;
: PAGE  ( -- ) <ED>  1 1 <CUP> ;</LANG>
Rosetta Task

```forth
( move the cursor one position to the left)  1 <CUB>
( move the cursor one position to the right) 1 <CUF>
( move the cursor up one line )              1 <CUU>
( move the cursor down one line)             1 <CUD>
( move the cursor to the beginning of the line) 1 <CHA>
( move the cursor to the end of the line      ) 80 <CHA>
( move the cursor to the top left corner of the screen)  1 1 <CUP>
( move the cursor to the bottom right corner of the screen)  80 24 <CUP>
</LANG>


## Go


### External commands


```go
package main

import (
    "fmt"
    "time"
    "os"
    "os/exec"
    "strconv"
)

func main() {
    tput("clear") // clear screen
    tput("cup", "6", "3") // an initial position
    time.Sleep(1 * time.Second)
    tput("cub1") // left
    time.Sleep(1 * time.Second)
    tput("cuf1") // right
    time.Sleep(1 * time.Second)
    tput("cuu1") // up
    time.Sleep(1 * time.Second)
    // cud1 seems broken for me.  cud 1 works fine though.
    tput("cud", "1") // down
    time.Sleep(1 * time.Second)
    tput("cr") // begining of line
    time.Sleep(1 * time.Second)
    // get screen size here
    var h, w int
    cmd := exec.Command("stty", "size")
    cmd.Stdin = os.Stdin
    d, _ := cmd.Output()
    fmt.Sscan(string(d), &h, &w)
    // end of line
    tput("hpa", strconv.Itoa(w-1))
    time.Sleep(2 * time.Second)
    // top left
    tput("home")
    time.Sleep(2 * time.Second)
    // bottom right
    tput("cup", strconv.Itoa(h-1), strconv.Itoa(w-1))
    time.Sleep(3 * time.Second)
}

func tput(args ...string) error {
    cmd := exec.Command("tput", args...)
    cmd.Stdout = os.Stdout
    return cmd.Run()
}
```


### ANSI escape codes

Not meeting all task requirements.  Some of the movements are awkward with ANSI escape codes alone and are best done with one of the other techniques shown.

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    fmt.Print("\033[2J\033[6;3H") // clear screen, move to an initial position
    time.Sleep(1 * time.Second)   // pause to let cursor blink
    fmt.Print("\033[D") // left
    time.Sleep(1 * time.Second)
    fmt.Print("\033[C") // right
    time.Sleep(1 * time.Second)
    fmt.Print("\033[A") // up
    time.Sleep(1 * time.Second)
    fmt.Print("\033[B") // down
    time.Sleep(1 * time.Second)
    fmt.Print("\033[;H") // top left
    time.Sleep(1 * time.Second)
}
```


### Ncurses

{{libheader|curses}}

```go
package main

import (
    "log"
    "time"

    gc "code.google.com/p/goncurses"
)

func main() {
    s, err := gc.Init()
    if err != nil {
        log.Fatal("init:", err)
    }
    defer gc.End()
    // an initial position
    s.Move(6, 3)
    s.Refresh()                 // update screen
    time.Sleep(1 * time.Second) // allow time for cursor to blink
    // left
    y, x := s.CursorYX()
    s.Move(y, x-1)
    s.Refresh()
    time.Sleep(1 * time.Second)
    // right
    y, x = s.CursorYX()
    s.Move(y, x+1)
    s.Refresh()
    time.Sleep(1 * time.Second)
    // up
    y, x = s.CursorYX()
    s.Move(y-1, x)
    s.Refresh()
    time.Sleep(1 * time.Second)
    // down
    y, x = s.CursorYX()
    s.Move(y+1, x)
    s.Refresh()
    time.Sleep(1 * time.Second)
    // beginning of line
    y, x = s.CursorYX()
    s.Move(y, 0)
    s.Refresh()
    time.Sleep(1 * time.Second)
    // get window size for moves to edges
    my, mx := s.MaxYX()
    // end of line
    y, x = s.CursorYX()
    s.Move(y, mx-1)
    s.Refresh()
    time.Sleep(2 * time.Second)
    // top left
    s.Move(0, 0)
    s.Refresh()
    time.Sleep(2 * time.Second)
    // bottom right
    s.Move(my-1, mx-1)
    s.Refresh()
    s.GetChar()
}
```



## Julia

{{trans|Kotlin}}

```julia
const ESC = "\u001B" # escape code
const moves = Dict( "left" => "[1D", "right" => "[1C", "up" => "[1A", "down" => "[1B",
                    "linestart" => "[9D", "topleft" => "[H", "bottomright" => "[24;79H")

print("$ESC[2J")     # clear terminal first
print("$ESC[10;10H") # move cursor to (10, 10) say
const count = [0]
for d in ["left", "right", "up", "down", "linestart", "bottomright"]
    sleep(3) # three second pause for display between cursor movements
    print("$ESC$(moves[d])")
    print(count[1] += 1)
end
println()
println()

```




## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
// version 1.1.2

const val ESC = "\u001B"  // escape code

fun main(args: Array<String>) {
    print("$ESC[2J")     // clear terminal first
    print("$ESC[10;10H") // move cursor to (10, 10) say
    val aecs = arrayOf(
        "[1D",    // left
        "[1C",    // right
        "[1A",    // up
        "[1B",    // down
        "[9D",    // line start
        "[H",     // top left
        "[24;79H" // bottom right - assuming 80 x 24 terminal
    )
    for (aec in aecs) {
        Thread.sleep(3000) // three second display between cursor movements
        print("$ESC$aec")
    }
    Thread.sleep(3000)
    println()
}
```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(esc		= decode_base64('Gw=='))

stdoutnl('Demonstrate how to move the cursor one position to the left
Demonstrate how to move the cursor one position to the right
Demonstrate how to move the cursor up one line (without affecting its horizontal position)
Demonstrate how to move the cursor down one line (without affecting its horizontal position)
Demonstrate how to move the cursor to the beginning of the line
Demonstrate how to move the cursor to the end of the line
Demonstrate how to move the cursor to the top left corner of the screen
Demonstrate how to move the cursor to the bottom right corner of the screen
')

// place cursor in a suitable place before exercise
stdout(#esc + '[5;10H')
sleep(2000)


// move the cursor one position to the left
stdout(#esc + '[1D')
sleep(2000)

// move the cursor one position to the right
stdout(#esc + '[1C')
sleep(2000)

// move the cursor up one line
stdout(#esc + '[1A')
sleep(2000)

// move the cursor down one line
stdout(#esc + '[1B')
sleep(2000)

// move the cursor to the beginning of the line
stdout(#esc + '[100D')
sleep(2000)

// move the cursor to the top left corner of the screen
stdout(#esc + '[H')
sleep(2000)

// move the cursor to the bottom right corner of the screen
stdout(#esc + '[500;500H')
sleep(2000)
```



## Mathematica


```Mathematica
Run["tput cub1"]                  (* one position to the left *)
Run["tput cuf1" ]                 (* one position to the right *)
Run["tput cuu1" ]                 (* up one line *)
Run["tput cud1"]                  (* down one line *)
Run["tput cr"]                    (* beginning of line *)
Run["tput home"]                  (* top left corner *)


WIDTH=RunThrough["tput cols", ""];
HEIGHT=RunThrough["tput lines", ""];

Run["tput hpa "<>WIDTH]            (* end of line *)
Run["tput cup "<>HEIGHT<>"  "<> WIDTH]    (* bottom right corner *)
```



## Perl

{{trans|Perl 6}}

```perl
system "tput cub1"; sleep 1;  # one position to the left
system "tput cuf1"; sleep 1;  # one position to the right
system "tput cuu1"; sleep 1;  # up one line
system "tput cud1"; sleep 1;  # down one line
system "tput cr";   sleep 1;  # beginning of line
system "tput home"; sleep 1;  # top left corner

$_ = qx[stty -a </dev/tty 2>&1];
my($rows,$cols) = /(\d+) rows; (\d+) col/;
$rows--; $cols--;

system "tput cup $rows $cols"; # bottom right corner
sleep 1;
```



## Perl 6


```perl6
shell "tput cub1";                  # one position to the left
shell "tput cuf1";                  # one position to the right
shell "tput cuu1";                  # up one line
shell "tput cud1";                  # down one line
shell "tput cr";                    # beginning of line
shell "tput home";                  # top left corner

$_ = qx[stty -a </dev/tty 2>&1];
my $rows = +m/'rows '    <(\d+)>/;
my $cols = +m/'columns ' <(\d+)>/;

shell "tput hpa $cols";             # end of line
shell "tput cup $rows $cols";       # bottom right corner
```



## Phix


```Phix
--
-- demo\rosetta\Cursor_movement.exw
--
### ==========================

--
-- These may vary by platform/hardware... (this program is ideal for sorting such things out)
--
constant HOME  = 327,
         END   = 335,
         UP    = 328,
         DOWN  = 336,
         LEFT  = 331,
         RIGHT = 333,
         PGUP  = 329,   -- (goto top left)
         PGDN  = 337    -- (goto bottom right)

constant {maxl,maxc} = video_config()[VC_SCRNLINES..VC_SCRNCOLS]

procedure move_cursor(integer dy, integer dx)
    integer {l,c} = sq_add(get_position(),{dy,dx})
    if l>=1 and l<=maxl
    and c>=1 and c<=maxc then
        position(l,c)
    end if
end procedure

procedure move_to(integer ny=-1, integer nx=-1)
    integer {l,c} = get_position()
    if ny!=-1 then l = ny end if
    if nx!=-1 then c = nx end if
    position(l,c)
end procedure

procedure showkey(integer key)
    integer {l,c} = get_position()
    position(2,maxc-5)
    ?key
    position(l,c)
end procedure

while 1 do
    integer key = wait_key()
    if key=#1B then exit end if -- escape quits
    showkey(key)
    if    key=HOME  then move_to(nx:=1)     -- home
    elsif key=END   then move_to(nx:=maxc)  -- end
    elsif key=UP    then move_cursor(-1, 0) -- up
    elsif key=DOWN  then move_cursor(+1, 0) -- down
    elsif key=LEFT  then move_cursor( 0,-1) -- left
    elsif key=RIGHT then move_cursor( 0,+1) -- right
    elsif key=PGUP  then move_to(1,1)       -- page_up
    elsif key=PGDN  then move_to(maxl,maxc) -- page_down
    end if
end while
```



## PicoLisp


```PicoLisp
(call 'tput "cub1")                                # one position to the left
(call 'tput "cuf1")                                # one position to the right
(call 'tput "cuu1")                                # up one line
(call 'tput "cud1")                                # down one line
(call 'tput "cr")                                  # beginning of the line
(call 'tput "hpa" (sys "COLUMNS"))                 # end of the line
(call 'tput "home")                                # top left corner
(call 'tput "cup" (sys "LINES") (sys "COLUMNS"))   # bottom right corner
```



## Python

{{libheader|curses}}

```Python
import curses

scr = curses.initscr()
#     Demonstrate how to move the cursor one position to the left
def move_left():
	y,x = curses.getyx()
	curses.move(y,x-1)

#    Demonstrate how to move the cursor one position to the right
def move_right():
	y,x = curses.getyx()
	curses.move(y,x+1)

#    Demonstrate how to move the cursor up one line (without affecting its horizontal position)
def move_up():
	y,x = curses.getyx()
	curses.move(y-1,x)

#    Demonstrate how to move the cursor down one line (without affecting its horizontal position)
def move_down():
	y,x = curses.getyx()
	curses.move(y+1,x)

#    Demonstrate how to move the cursor to the beginning of the line
def move_line_home()
	y,x = curses.getyx()
	curses.move(y,0)

#    Demonstrate how to move the cursor to the end of the line
def move_line_end()
	y,x = curses.getyx()
	maxy,maxx = scr.getmaxyx()
	curses.move(y,maxx)

#    Demonstrate how to move the cursor to the top left corner of the screen
def move_page_home():
	curses.move(0,0)

#    Demonstrate how to move the cursor to the bottom right corner of the screen
def move_page_end():
	y,x = scr.getmaxyx()
	curses.move(y,x)

```



## Racket


```racket

#lang racket
(require (planet neil/charterm:3:0))
(define x 0)
(define y 0)

(define (on-key k)
  (match k
    ['down   (move  0 -1)]
    ['up     (move  0 +1)]
    ['right  (move +1  0)]
    ['left   (move -1  0)]
    [else    #f]))

(define (move dx dy)
  (set! x (+ x dx))
  (set! y (+ y dy))
  (charterm-cursor x y))

(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 0 0)
 (let loop ([continue? #t])
   (when continue?
     (loop (on-key (charterm-read-key))))))

```




## REXX

{{works with|PC/REXX}}
{{works with|Personal REXX}}

This version   ''only''   works with PC/REXX or Personal REXX.

```rexx
/*REXX pgm demonstrates how to achieve movement of the terminal cursor. */

parse value scrsize() with sd sw       /*find the display screen size.  */
parse value cursor() with row col      /*find where the cursor is now.  */

colL=col-1;  if colL==0 then colL=sw   /*prepare to move cursor to left.*/
call cursor row,colL                   /*move cursor to the left (wrap).*/

colR=col+1;  if colR>sw then colL=1    /*prepare to move cursor to right*/
call cursor row,colR                   /*move cursor to the right (wrap)*/

rowU=row-1;  if rowU==0 then rowU=sd   /*prepare to move cursor up.     */
call cursor rowU,col                   /*move cursor  up  (with wrap).  */

rowD=row+1;  if rowD>sd then rowD=1    /*prepare to move cursor down.   */
call cursor rowD,col                   /*move cursor down (with wrap).  */

call cursor row,1                      /*move cursor to beginning of row*/
call cursor row,sw                     /*move cursor to    end    of row*/
call cursor 1,1                        /*move cursor to top left corner.*/
call cursor sd,sw                      /*move cursor to bot right corner*/

                                       /*stick a fork in it, we're done.*/
```



## Scala

{{Works with|Ubuntu|14.04}}

```scala
object CursorMovement extends App {
  val ESC = "\u001B" // escape code

  print(s"$ESC[2J$ESC[10;10H") // clear terminal first, move cursor to (10, 10) say
  val aecs = Seq(
    "[1D", // left
    "[1C", // right
    "[1A", // up
    "[1B", // down
    "[9D", // line start
    "[H", // top left
    "[24;79H" // bottom right - assuming 80 x 24 terminal
  )
  for (aec <- aecs) {
    Thread.sleep(3000) // three second display between cursor movements
    print(s"$ESC$aec")
  }

}
```


## Tcl

{{trans|UNIX Shell}}

```tcl
# Simplification wrapper for when we're actually affecting the terminal
proc tput args {
    exec tput {*}$args >@stdout <@stdin
}

tput cub1;                 # one position to the left
tput cuf1;                 # one position to the right
tput cuu1;                 # up one line
tput cud1;                 # down one line
tput cr;                   # beginning of line
tput home;                 # top left corner

# For line ends and bottom, we need to determine size of terminal
set width [exec tput cols]
set height [exec tput lines]

tput hpa $width;	   # end of line
tput cpu $height $width;   # bottom right corner
```



## UNIX Shell


```bash
tput cub1                  # one position to the left
tput cuf1                  # one position to the right
tput cuu1                  # up one line
tput cud1                  # down one line
tput cr                    # beginning of line
tput home                  # top left corner

# For line ends and bottom, we need to determine size
# of terminal
WIDTH=`tput cols`
HEIGHT=`tput lines`

tput hpa $WIDTH            # end of line
tput cup $HEIGHT $WIDTH    # bottom right corner
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
int I, X, Y, W, H;
[Cursor(10, 13);        \set cursor to arbitrary location on screen
I:= ChIn(1);            \wait for keystroke (no echo to display)
ChOut(0, $08\BS\);      \backspace moves one position right
I:= ChIn(1);
X:= Peek($40, $50);     \get cursor location from BIOS data
Y:= Peek($40, $51);
Cursor(X+1, Y);         \move one position right
I:= ChIn(1);
Cursor(X+1, Y-1);       \move up one line
I:= ChIn(1);
ChOut(0, $0A\LF\);      \line feed moves down one line
I:= ChIn(1);
ChOut(0, $0D\CR\);      \carriage return moves to beginning of current line
I:= ChIn(1);
W:= Peek($40, $4A);     \get width of display (standard = 80; mine = 94)
Cursor(W-1, Y);         \move to end of current line
I:= ChIn(1);
Cursor(0, 0);           \move to top left corner
I:= ChIn(1);
H:= Peek($40, $84) + 1; \get height of display (standard = 25; mine = 50)
Cursor(W-1, H-1);       \move to bottom right corner
I:= ChIn(1);
]
```


Moving the cursor position beyond the terminal boundaries simply makes
the flashing cursor disappear. This is sometimes useful.


## zkl

{{trans|Go}}
zkl doesn't know anything about terminals but can print Ansi terminal codes:

```zkl
print("\e[2J\e[6;3H");	// clear screen, move to an initial position
   Atomic.sleep(1);	// pause to let cursor blink
print("\e[D");		// left
   Atomic.sleep(1);
print("\e[C");		// right
   Atomic.sleep(1);
print("\e[A");		// up
   Atomic.sleep(1);
print("\e[B");		// down
   Atomic.sleep(1);
print("\e[;H");		// top left
   Atomic.sleep(1);
```



## ZX Spectrum Basic



```zxbasic
10 PRINT CHR$(8);:REM cursor one position left
20 PRINT CHR$(9);:REM cursor one position right
30 GO SUB 500: REM get cursor position
40 IF cr>0 THEN LET cr=cr-1: GO SUB 550: REM cursor up one line
50 IF cr<22 THEN LET cr=cr+1: GO SUB 550: REM cursor down one line
60 POKE 23688,33: REM cursor to beginning of the line
70 POKE 23688,0: REM cursor to end of line
80 POKE 23688,33:POKE 23689,24: REM cursor to top left
90 REM bottom two rows are reserved for input and errors
100 REM so we reserve those lines here
110 POKE 23688,0: POKE 23689,2: REM bottom right

499 STOP: REM do not overrun into subroutines

500 REM get cursor position
510 LET cc=33-PEEK 23688:REM current column
520 LET cr=24-PEEK 23689:REM current row
530 RETURN

550 REM set cursor position
560 PRINT AT cr,cc;
570 RETURN

600 REM alternative set cursor position
610 POKE 23688,33-cc
620 POKE 23689,24-cr
630 RETURN
```

