+++
title = "Terminal control/Coloured text"
description = ""
date = 2019-10-09T10:31:08Z
aliases = []
[extra]
id = 10458
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "arm_assembly",
  "arturo",
  "autohotkey",
  "basic",
  "bbc_basic",
  "befunge",
  "c",
  "cobol",
  "csharp",
  "d",
  "forth",
  "fortran",
  "funl",
  "go",
  "golo",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "lasso",
  "locomotive_basic",
  "mathematica",
  "nim",
  "ocaml",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "xpl0",
  "zkl",
]
+++

## Task

Display a word in various colours on the terminal.

The system palette, or colours such as Red, Green, Blue, Magenta, Cyan, and Yellow can be used.


Optionally demonstrate:
* How the system should determine if the terminal supports colour
* Setting of the background colour
* How to cause blinking or flashing (if supported by the terminal)





## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program colorText.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ BUFFERSIZE,          100

/* Initialized data */
.data
szMessStartPgm:            .asciz "Program start \n"
szMessEndPgm:              .asciz "Program normal end.\n"
szMessColorRed:            .asciz "Color red.\n"
szCodeInit:                .asciz "\033[0m"                    @ color reinit
szCodeRed:                 .asciz "\033[31m"                   @ color red
szMessBlue:                .asciz "\033[34mColor Blue\n"       @ color blue
szMessTwoColor:            .asciz "\033[32mColor Green \033[35m Color Velvet\n"
szMessTest:                .asciz "\033[33m\033[1mMessage yellow bold\n"

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
    ldr r0,iAdrszMessBlue                       @ message color blue
    bl affichageMess
    ldr r0,iAdrszMessTwoColor                   @ message two colors
    bl affichageMess
    ldr r0,iAdrszMessTest
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
iAdrszMessBlue:            .int szMessBlue
iAdrszMessColorRed:        .int szMessColorRed
iAdrszMessTwoColor:        .int szMessTwoColor
iAdrszMessTest:            .int szMessTest
iAdrszCarriageReturn:      .int szCarriageReturn

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



## Arturo



```arturo
str "Hello World"

print $(color str "red")
print $(color str "green")
print $(color str "blue")
print $(color str "magenta")
print $(color str "yellow")
print $(color str "cyan")
print $(color str "black")
print $(color str "white")
```



## AutoHotkey

[[Image:AutoHotkey_terminal_control_coloured_text.jpeg|thumb|right]]
AutoHotkey is not written for the command line, so we need to use the WinAPI directly. For simplicity, this example demonstrates only the foreground colours.

```AHK
DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )
Loop 15
	 SetConsoleTextAttribute(hConsole, A_Index)
	,WriteConsole(hConsole, "AutoHotkey`n")

MsgBox

SetConsoleTextAttribute(hConsole, Attributes){
	return DllCall( "SetConsoleTextAttribute", UPtr, hConsole, UShort, Attributes)
}
WriteConsole(hConsole, text){
	VarSetCapacity(out, 16)
	If DllCall( "WriteConsole", UPtr, hConsole, Str, text, UInt, StrLen(text)
				  , UPtrP, out, uint, 0 )
		return out
	return 0
}
```



## BASIC

[[File:QBasic colored text.png|right]]

```qbasic
FOR n = 1 TO 15
    COLOR n
    PRINT "Rosetta Code"
NEXT
```



## BBC BASIC


```bbcbasic
      FOR col% = 0 TO 14
        COLOUR col% : REM foreground
        COLOUR 128+(15-col%) : REM background
        PRINT "Rosetta Code"
      NEXT
```

[[File:coloured_text_bbc.gif]]


## Befunge

Assuming a terminal with support for ANSI escape sequences, this displays the words ''Red'', ''Green'', ''Blue'', ''Magenta'', ''Cyan'' and ''Yellow'', using the corresponding text colour and a "complementary" background colour.

```befunge
<v0"1Red"0"2Green"0"4Blue"0"5Magenta"0"6Cyan"0"3Yellow"00
,_:!#@_:"m3["39*,,,\,,"m4["39*,,,\"g"\->:#,_55+"m["39*,,,
```



## C

On a terminal that understands ANSI escape sequences, such as color xterm, this shows you some annoyingly huge, annoyingly colorful tables.

```c
#include <stdio.h>

void table(const char *title, const char *mode)
{
	int f, b;
	printf("\n\033[1m%s\033[m\n bg\t fg\n", title);
	for (b = 40; b <= 107; b++) {
		if (b == 48) b = 100;
		printf("%3d\t\033[%s%dm", b, mode, b);
		for (f = 30; f <= 97; f++) {
			if (f == 38) f = 90;
			printf("\033[%dm%3d ", f, f);
		}
		puts("\033[m");
	}
}

int main(void)
{
	int fg, bg, blink, inverse;

	table("normal ( ESC[22m or ESC[m )", "22;");
	table("bold ( ESC[1m )", "1;");
	table("faint ( ESC[2m ), not well supported", "2;");
	table("italic ( ESC[3m ), not well supported", "3;");
	table("underline ( ESC[4m ), support varies", "4;");
	table("blink ( ESC[5m )", "5;");
	table("inverted ( ESC[7m )", "7;");
	return 0;
}
```


## C#
Visual Studios Intellisense will list all available colours.

```c#

static void Main(string[] args)
{
    Console.ForegroundColor = ConsoleColor.Red;
    Console.BackgroundColor = ConsoleColor.Yellow;
    Console.WriteLine("Red on Yellow");
    Console.ForegroundColor = ConsoleColor.White;
    Console.BackgroundColor = ConsoleColor.Black;
    Console.WriteLine("White on black");
    Console.ResetColor();
    Console.WriteLine("Back to normal");
    Console.ReadKey();
}

```



## COBOL

Note: <code>LOWLIGHT</code> and <code>BLINK</code> are not well supported on modern terminals, and may have no effect.
<!-- I couldn't find a way to upload a picture of the output. The 'Upload File' option isn't there for me. Would someone else mind uploading it? -->

```cobol
*> Apologies for the repetitiveness.
       IDENTIFICATION  DIVISION.
       PROGRAM-ID. coloured-text.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  example-str VALUE "COBOL".

       01  fore-colour PIC 9.
       01  back-colour PIC 9.

       01  line-num    PIC 99 VALUE 1.
       01  col-num     PIC 99 VALUE 1.

       01  pause       PIC X.

       PROCEDURE DIVISION.
           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "With HIGHLIGHT:" AT LINE line-num, COLUMN 1
           ADD 1 TO line-num

           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour HIGHLIGHT

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "With LOWLIGHT: (has no effect on many terminals)"
               AT LINE line-num, COLUMN 1
           ADD 1 TO line-num

           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour LOWLIGHT

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "With BLINK:" AT LINE line-num, COLUMN 1
           ADD 1 TO line-num

           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour BLINK

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "Press enter to continue."
               AT LINE line-num, COLUMN 1
           ACCEPT pause AT LINE line-num, COLUMN 40

           GOBACK
           .
```



## D


For terminals that understand color escape sequences:

```d
import
    std.conv,
    std.stdio;

enum Color {
    fgBlack = 30,
    fgRed,
    fgGreen,
    fgYellow,
    fgBlue,
    fgMagenta,
    fgCyan,
    fgWhite,

    bgBlack = 40,
    bgRed,
    bgGreen,
    bgYellow,
    bgBlue,
    bgMagenta,
    bgCyan,
    bgWhite
}

string color(string text, Color ink) {
    return "\033["
        ~ ink.to!int.to!string
        ~ "m"
        ~ text
        ~ "\033[0m";
}

void main() {
    auto colors = [
        Color.fgBlack,
        Color.fgRed,
        Color.fgGreen,
        Color.fgYellow,
        Color.fgBlue,
        Color.fgMagenta,
        Color.fgCyan,
        Color.fgWhite
    ];

    foreach (c; colors) {
        // Print the color name, in white.
        c.to!string.color(Color.fgWhite).writeln;

        // Print some text in the color.
        "Hello, world!".color(c).writeln;
    }
}
```


=={{header|F_Sharp|F#}}==
```fsharp
open System

Console.ForegroundColor <- ConsoleColor.Red
Console.BackgroundColor <- ConsoleColor.Yellow
Console.WriteLine("Red on Yellow")

Console.ForegroundColor <- ConsoleColor.White
Console.BackgroundColor <- ConsoleColor.Black
Console.WriteLine("White on Black")

Console.ForegroundColor <- ConsoleColor.Green
Console.BackgroundColor <- ConsoleColor.Blue
Console.WriteLine("Green on Blue")

Console.ResetColor()
Console.WriteLine("Back to normal")
Console.ReadKey()
```



## Forth

ANS/ISO Forth does not specify how screen color is handled. This demonstration creates a set of commands for an ANSI terminal that give the programmer control of text color.
<LANG FORTH>( ANSI terminal control lexicon Colored Text)
DECIMAL
( support routines)
 27 CONSTANT ESC
: <##>    ( n -- ) ( sends n, radix 10, no spaces)
          BASE @ >R   0 <# #S #> TYPE   R> BASE ! ;

: ESC[   ( -- )   ESC EMIT ." [" ;
( Attributes )
1 CONSTANT BOLD    2 CONSTANT DIM    3 CONSTANT ITALIC
5 CONSTANT BLINK   7 CONSTANT REV    8 CONSTANT BLANK

( Colors )
0 CONSTANT BLACK   1 CONSTANT RED    2 CONSTANT GREEN
3 CONSTANT YELLOW  4 CONSTANT BLUE   5 CONSTANT MAGENTA
6 CONSTANT CYAN    7 CONSTANT WHITE

: ATTR   ( attribute ) ESC[ <##> ." m" ;  ( use:  BOLD ATTR       )
: TEXT       ( color ) 30 + ATTR ;        ( use:  YELLOW TEXT     )
: BACKGROUND ( color ) 40 + ATTR ;        ( use:  BLUE BACKGROUND )

```
With the code loaded into Forth, color control is a part of the language

```forth
WHITE TEXT BLUE BACKGROUND  ok
BLUE TEXT  BOLD ATTR ok
CYAN TEXT ok
```


## Fortran


### Intel Fortran on Windows

Using Windows API functions, see for instance '''[https://msdn.microsoft.com/en-us/library/ms686047.aspx SetConsoleTextAttribute]''' in MSDN. On can set foreground and background colors, available attributes are [https://msdn.microsoft.com/en-us/library/ms682088.aspx here]. It's not possible to cause blinking without using a thread to change attributes at time intervals. The program reverts the console attributes to the preceding values. Failing to do that, it is still possible to reset console colors with the '''color''' command, without arguments.


```fortran
program textcolor
    use kernel32
    implicit none
    integer(HANDLE) :: hConsole
    integer(BOOL) :: q
    type(T_CONSOLE_SCREEN_BUFFER_INFO) :: csbi

    hConsole = GetStdHandle(STD_OUTPUT_HANDLE)

    if (GetConsoleScreenBufferInfo(hConsole, csbi) == 0) then
        error stop "GetConsoleScreenBufferInfo failed."
    end if

    q = SetConsoleTextAttribute(hConsole, int(FOREGROUND_RED .or. &
                                              FOREGROUND_INTENSITY .or. &
                                              BACKGROUND_BLUE .or. &
                                              BACKGROUND_RED, WORD))
    print "(A)", "This is a red string."
    q = SetConsoleTextAttribute(hConsole, csbi%wAttributes)
end program
```



## FunL


```funl
import console.*

bold()
blink()

if $os.toLowerCase().startsWith( 'win' )
  println( 'not supported' )
else
  println( 'good to go' )

reset()

println( RED + 'Red', GREEN + 'Green', BLUE + 'Blue', MAGENTA + 'Magenta', CYAN + 'Cyan', YELLOW + 'Yellow' + RESET )
```



## Go


### External command


```go
package main

import (
    "fmt"
    "os"
    "os/exec"
)

func main() {
    color(red)
    fmt.Println("Red")
    color(green)
    fmt.Println("Green")
    color(blue)
    fmt.Println("Blue")
}

const (
    blue  = "1"
    green = "2"
    red   = "4"
)

func color(c string) {
    cmd := exec.Command("tput", "setf", c)
    cmd.Stdout = os.Stdout
    cmd.Run()
}
```

Optional tasks

```go
package main

import (
    "fmt"
    "log"
    "os"
    "os/exec"
)

func main() {
    // set background color to blue, log error message if impossible.
    if err := tput("setb", blue); err != nil {
        log.Fatal("no color capability")
    }
    // clearing the screen will fill screen with background color
    // on most terminals.
    tput("clear")

    tput("blink")     // set blink attribute
    tput("setb", red) // new background color
    fmt.Println("  Blinking Red  ")
    tput("sgr0") // clear blink (and all other attributes)
}

const (
    blue  = "1"
    green = "2"
    red   = "4"
)

func tput(args ...string) error {
    cmd := exec.Command("tput", args...)
    cmd.Stdout = os.Stdout
    return cmd.Run()
}
```


### ANSI escape codes


```go
package main

import "fmt"

func main() {
    fmt.Println("\033[31mRed")
    fmt.Println("\033[32mGreen")
    fmt.Println("\033[34mBlue")
}
```

Optional tasks

```go
package main

import "fmt"

func main() {
    fmt.Print("\033[44m")   // set background color
    fmt.Print("\033[2J")    // clear screen to paint new background color
    fmt.Print("\033[5;41m") // blink on, red background
    fmt.Println("   Blinking Red   ")
    fmt.Print("\033[25;40m") // blink off, black background
}
```


### Ncurses

```go
package main

import (
    "log"

    gc "code.google.com/p/goncurses"
)

func main() {
    s, err := gc.Init()
    if err != nil {
        log.Fatal("init:", err)
    }
    defer gc.End()
    gc.StartColor()
    const (
        red   = 1
        green = 2
        blue  = 3
    )
    gc.InitPair(red, gc.C_RED, gc.C_BLACK)
    gc.InitPair(green, gc.C_GREEN, gc.C_BLACK)
    gc.InitPair(blue, gc.C_BLUE, gc.C_BLACK)
    s.ColorOn(red)
    s.Println("Red")
    s.ColorOn(green)
    s.Println("Green")
    s.ColorOn(blue)
    s.Println("Blue")
    s.GetChar()
}
```

Optional tasks

```go
package main

import (
    "log"

    gc "code.google.com/p/goncurses"
)

func main() {
    s, err := gc.Init()
    if err != nil {
        log.Fatal("init:", err)
    }
    defer gc.End()
    // determine color support
    if !gc.HasColors() {
        log.Fatal("no color support")
    }
    // set background color
    gc.StartColor()
    gc.InitPair(1, gc.C_WHITE, gc.C_BLUE)
    s.ColorOn(1)
    s.SetBackground(gc.Char(' ') | gc.ColorPair(1))
    // blinking, different background color
    s.AttrOn(gc.A_BLINK)
    gc.InitPair(2, gc.C_WHITE, gc.C_RED)
    s.ColorOn(2)
    s.Print("   Blinking Red   ")
    s.GetChar()
}
```



## Golo


```golo
#!/usr/bin/env golosh
----
This module demonstrates terminal colours.
----
module Terminalcontrolcoloredtext

import gololang.AnsiCodes

function main = |args| {

  # these are lists of pointers to the ansi functions in the golo library.
  # {} doesn't do anything so it's got no effect on the text.

  let foregrounds = vector[
    ^fg_red, ^fg_blue, ^fg_magenta, ^fg_white, ^fg_black, ^fg_cyan, ^fg_green, ^fg_yellow
  ]
  let backgrounds = vector[
    ^bg_red, ^bg_blue, ^bg_magenta, ^bg_white, ^bg_black, ^bg_cyan, ^bg_green, ^bg_yellow
  ]
  let effects = vector[
    {}, ^bold, ^blink, ^underscore, ^concealed, ^reverse_video
  ]

  println("Terminal supports ansi code: " + likelySupported())

  foreach fg in foregrounds {
    foreach bg in backgrounds {
      foreach effect in effects {
        fg()
        bg()
        effect()
        print("Rosetta Code")
        reset()
      }
    }
  }
  println("")
}
```



## Haskell


```haskell
#!/usr/bin/runhaskell

import System.Console.ANSI

colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []
  putStrLn ""

main = do
  colorStrLn Vivid White Vivid Red "This is red on white."
  colorStrLn Vivid White Dull Blue "This is white on blue."
  colorStrLn Vivid Green Dull Black "This is green on black."
  colorStrLn Vivid Yellow Dull Black "This is yellow on black."
  colorStrLn Dull Black Vivid Blue "This is black on light blue."
```



## J

<!--WARNING-->
<!--Warning, other RC-J terminal control pages link here for definitions-->
<!--WARNING-->
Quite different from the fixed c solution, we flexibly construct character vectors that combine various functions.  This code constructs two such sequences, DB is useful to write vertical text, and the noun J output to the terminal draws to the extent of my artistic ability the J icon at the relative position.

```J
NB. relies on an vt100 terminal

CSI=: 27 91 { a.
'BLACK BLUE CYAN WHITE'=: 0 4 6 7
'OFF REVERSEVIDEO'=: 0 7

HIDECURSOR=: CSI,'?25l'
SHOWCURSOR=: CSI,'?25h'

csi=: (,~ (CSI , (' '&=)`(,:&';')}@:":))~
clear=: csi&'J'
attributes=: csi&'m'
color=: BLACK&$: : (attributes@:(40 30 + ,)) NB. BACKGROUND color FOREGROUND
move=: csi&'H'

upward=: csi&'A'
downward=: csi&'B'
foreward=: csi&'C'
backward=: csi&'D'

DB=: (downward , backward) ''

NB. J is character vector to simulate the J icon.
J=: (BLUE color WHITE[CYAN)
J=: J , (backward 1),' T ',(backward 1),DB,,3#,:'|',DB
J=: J , (backward 5),'*    |',DB
J=: J , (backward 5),'\____/'
smoutput(color BLACK),(clear 2),(move 8 22),J,(WHITE color BLACK),(downward 2)

```



## Julia

Julia has rudimentary color terminal support built-in.  Slightly more elaborate color and effect support is available with the <code>AnsiColor</code> package.

```Julia

using AnsiColor

function showbasecolors()
    for color in keys(Base.text_colors)
        print_with_color(color, " ", string(color), " ")
    end
    println()
end

function showansicolors()
    for fore in keys(AnsiColor.COLORS)
        print(@sprintf("%15s ", fore))
        for back in keys(AnsiColor.COLORS)
            print(" ", colorize(fore, "RC", background=back), " ")
        end
        println()
    end
    println()
    for eff in keys(AnsiColor.MODES)
        print(@sprintf(" %s ", eff), colorize("default", "RC", mode=eff))
    end
    println()
end

if Base.have_color
    println()
    println("Base Colors")
    showbasecolors()
    println("\nusing AnsiColor")
    showansicolors()
    println()
else
    println("This terminal appears not to support color.")
end

```


```txt

$ julia --color=yes terminal_control_color.jl

```

[https://raw.githubusercontent.com/MichaeLeroy/rosetta-code/master/julia/completed/terminal_control_color.png Output Image]

```txt

$ julia --color=no terminal_control_color.jl
This terminal appears not to support color.

```



## Kotlin

```scala
// version 1.1.2

const val ESC = "\u001B"
const val NORMAL = ESC + "[0"
const val BOLD   = ESC + "[1"
const val BLINK  = ESC + "[5"      // not working on my machine
const val BLACK  = ESC + "[0;40m"  // black background
const val WHITE  = ESC + "[0;37m"  // normal white foreground

fun main(args: Array<String>) {
    print("${ESC}c") // clear terminal first
    print(BLACK)     // set background color to black
    val foreColors = listOf(
        ";31m" to "red",
        ";32m" to "green",
        ";33m" to "yellow",
        ";34m" to "blue",
        ";35m" to "magenta",
        ";36m" to "cyan",
        ";37m" to "white"
    )
    for (attr in listOf(NORMAL, BOLD, BLINK)) {
        for (color in foreColors) println("$attr${color.first}${color.second}")
    }
    println(WHITE)  // set foreground color to normal white
}
```



## Lasso


```Lasso
#!/usr/bin/lasso9

define ec(code::string) => {

	local(esc		= decode_base64('Gw=='))
	local(codes		= map('esc' = #esc,
		'normal'	= #esc + '[0m',
		'blink'		= #esc + '[5;31;49m',
		'red'		= #esc + '[31;49m',
		'blue'		= #esc + '[34;49m',
		'green'		= #esc + '[32;49m',
		'magenta'	= #esc + '[35;49m',
		'yellowred'	= #esc + '[33;41m'
	))

	return #codes -> find(#code)
}

stdout( ec('red'))
stdoutnl('So this is the Rosetta Code!')
stdout( ec('blue'))
stdoutnl('So this is the Rosetta Code!')
stdout( ec('green'))
stdoutnl('So this is the Rosetta Code!')
stdout( ec('magenta'))
stdoutnl('So this is the Rosetta Code!')
stdout( ec('yellowred'))
stdout('So this is the Rosetta Code!')
stdoutnl( ec('blink'))
stdoutnl('So this is the Rosetta Code!')
stdout( ec('normal'))

```



## Locomotive Basic



```locobasic
10 mode 1:defint a-z
20 print "Mode 1 (4 colors):"
30 for y=0 to 3
40 for x=0 to 3
50 pen x:paper y:print "Test";
60 next
70 print
80 next
90 pen 1:paper 0
100 locate 1,25:print "<Press any key>";:call &bb06
110 ink 1,8,26
120 ink 2,21,17
130 locate 1,25:print "Flashing inks --- <Press any key>";:call &bb06
140 speed ink 8,3
150 locate 1,25:print "Different flashing --- <Press any key>";:call &bb06
160 ink 1,24:ink 2,20  ' back to defaults -- see chapter 1, page 50 in CPC manual
170 pen 1:paper 0:mode 0:speed ink 50,50
180 print "Mode 0 (16 colors):"
190 for i=0 to 15
200 pen i
210 if i=0 then paper 1 else paper 0
220 print using "##";i;
230 for j=1 to 18
240 print chr$(143);
250 next
260 next
270 pen 1:paper 0
280 print "Paper/pen 14 and 15"
290 print "are set to";
300 pen 14:print " flashing":pen 1
310 print "by default."
320 print
330 print "*End of color demo*"
340 locate 1,25:print "<Press any key>";:call &bb06
350 mode 1
```



## Mathematica

Delegating to tput on terminal enabled OS(Mac Os, Linux)

```Mathematica
Run["tput setaf 1"]; Print["Coloured Text"];
Run["tput setaf 2"]; Print["Coloured Text"];
Run["tput setaf 3"]; Print["Coloured Text"]
```

[[File:colouredtextmma.png]]


## Nim


```nim
import Terminal
setForegroundColor(fgRed)
echo "FATAL ERROR! Cannot write to /boot/vmlinuz-3.2.0-33-generic"

setBackgroundColor(bgBlue)
setForegroundColor(fgYellow)
stdout.write "This is an "
writeStyled  "important"
stdout.write " word"
resetAttributes()
stdout.write "\n"

setForegroundColor(fgYellow)
echo "RosettaCode!"

setForegroundColor(fgCyan)
echo "RosettaCode!"

setForegroundColor(fgGreen)
echo "RosettaCode!"

setForegroundColor(fgMagenta)
echo "RosettaCode!"
```



## OCaml


Using the library [http://forge.ocamlcore.org/projects/ansiterminal/ ANSITerminal] in the interactive loop:


```ocaml
$ ocaml unix.cma -I +ANSITerminal ANSITerminal.cma

# open ANSITerminal ;;
# print_string [cyan; on_blue] "Hello\n" ;;
Hello
- : unit = ()
```




## ooRexx

This program is based on the shell script in the Bash Prompt HowTo at
http://www.tldp.org/, by Giles Orr.  It uses object-oriented features of Open Object Rexx.

```REXX

#!/usr/bin/rexx
/*.----------------------------------------------------------------------.*/
/*|bashcolours: Display a table showing all of the possible colours that |*/
/*|             can be generated using ANSI escapes in bash in an xterm  |*/
/*|             terminal session.                                        |*/
/*|                                                                      |*/
/*|Usage:                                                                |*/
/*|                                                                      |*/
/*|>>-bashcolours-.----------.-----------------------------------------><|*/
/*|               |-- -? ----|                                           |*/
/*|               |-- -h ----|                                           |*/
/*|               '- --help -'                                           |*/
/*|                                                                      |*/
/*|where                                                                 |*/
/*|  -?, -h or --help                                                    |*/
/*|         display this documentation.                                  |*/
/*|                                                                      |*/
/*|This program is based on the shell script in the Bash Prompt HowTo at |*/
/*|http://www.tldp.org/, by Giles Orr.                                   |*/
/*|                                                                      |*/
/*|This program writes the various colour codes to the terminal to       |*/
/*|demonstrate what's available.  Each line showshe colour code of one   |*/
/*|forground colour, out of 17 (default + 16 escapes), followed by a test|*/
/*|use of that colour on all nine background colours (default + 8        |*/
/*|escapes).  Additional highlighting escapes are also demonstrated.     |*/
/*|                                                                      |*/
/*|This program uses object-oriented features of Open Object Rexx.       |*/
/*|The lineout method is used instead of say for consistency with use of |*/
/*|the charout method.                                                   |*/
/*'----------------------------------------------------------------------'*/
  call usage arg(1)
  trace normal

/* See if escapes work on the kind of terminal in use. */
  if value('TERM',,'ENVIRONMENT') = 'LINUX' then
    do
      say 'The Linux console does not support ANSI escape sequences for',
          'changing text colours or highlighting.'
      exit 4
    end

/* Set up the escape sequences. */
  ! = '1B'x                                                  -- ASCII escape
  bg = .array~of('[40m','[41m','[42m','[43m','[44m','[45m','[46m','[47m')
  fg = .array~of('[0m',   '[1m',   '[0;30m','[1;30m','[0;31m','[1;31m',,
                 '[0;32m','[1;32m','[0;33m','[1;33m','[0;34m','[1;34m',,
                 '[0;35m','[1;35m','[0;36m','[1;36m','[0;37m','[1;37m')
  hi = .array~of('[4m','[5m','[7m','[8m')
  text = 'gYw'                                              -- The test text
  .OUTPUT~lineout(' ')
  .OUTPUT~lineout('Foreground  |       Background Codes')
  .OUTPUT~lineout(!'[4mCodes       '!'[0m|'||,
                  !'[4m~[40m ~[41m ~[42m ~[43m ~[44m ~[45m ~[46m ~[47m'!'[0m')

  do f = 1 to fg~size                          -- write the foreground info.
    prefix = '~'fg[f]~left(6)' '!||fg[f]~strip' 'text
    .OUTPUT~charout(prefix)

    do b = 1 to bg~size                        -- write the background info.
      segment = !||fg[f]~strip !||bg[b]' 'text' '!||fg[1]
      .OUTPUT~charout(segment)
    end

    .OUTPUT~lineout(' ')
  end

/* Write the various highlighting escape sequences. */
  prefix = '~[4m'~left(6)'   '!||hi[1]'Underlined'!||fg[1]
  .OUTPUT~lineout(prefix)
  prefix = '~[5m'~left(6)'   '!||hi[2]'Blinking'!||fg[1]
  .OUTPUT~lineout(prefix)
  prefix = '~[7m'~left(6)'   '!||hi[3]'Inverted'!||fg[1]
  .OUTPUT~lineout(prefix)
  prefix = '~[8m'~left(6)'   '!||hi[4]'Concealed'!||fg[1],
           "(Doesn't seem to work in my xterm; might in Windows?)"
  .OUTPUT~lineout(prefix)
  .OUTPUT~lineout(' ')
  .OUTPUT~lineout("Where ~ denotes the ASCII escape character ('1B'x).")
  .OUTPUT~lineout(' ')
exit

/*.--------------------------------------------------------.*/
/*|One might expect to be able to use directory collections|*/
/*|as below instead of array collections, but there is no  |*/
/*|way to guarantee that a directory's indices will be     |*/
/*|iterated over in a consistent sequence, since directory |*/
/*|objects are not ordered.  Oh, well...                   |*/
/*'--------------------------------------------------------'*/
  fg = .directory~new
  fg[Default]   = '[0m';    fg[DefaultBold] = '[1m'
  fg[Black]     = '[0;30m'; fg[DarkGray]    = '[1;30m'
  fg[Blue]      = '[0;34m'; fg[LightBlue]   = '[1;34m'
  fg[Green]     = '[0;32m'; fg[LightGreen]  = '[1;32m'
  fg[Cyan]      = '[0;36m'; fg[LightCyan]   = '[1;36m'
  fg[Red]       = '[0;31m'; fg[LightRed]    = '[1;31m'
  fg[Purple]    = '[0;35m'; fg[LightPurple] = '[1;35m'
  fg[Brown]     = '[0;33m'; fg[Yellow]      = '[1;33m'
  fg[LightGray] = '[0;37m'; fg[White]       = '[1;37m'
  bg = .directory~new;      hi = .directory~new
  bg[Black]     = '[0;40m'; hi[Underlined]  = '[4m'
  bg[Blue]      = '[0;44m'; hi[Blinking]    = '[5m'
  bg[Green]     = '[0;42m'; hi[Inverted]    = '[7m'
  bg[Cyan]      = '[0;46m'; hi[Concealed]   = '[8m'
  bg[Red]       = '[0;41m'
  bg[Purple]    = '[0;45m'
  bg[Brown]     = '[0;43m'
  bg[LightGray] = '[0;47m'

usage: procedure
  trace normal

  if arg(1) = '-h',
   | arg(1) = '-?',
   | arg(1) = '--help'
  then
    do
      line = '/*|'
      say

      do l = 3 by 1 while line~left(3) = '/*|'
        line = sourceline(l)
        parse var line . '/*|' text '|*/' .
        .OUTPUT~lineout(text)
      end

      say
      exit 0
    end
return

```

This is what the output looks like:

[[File:BashColours.png]]

--[[User:Jlturriff|Leslie]] 23:10, 23 September 2012 (UTC)


## PARI/GP



```parigp
for(b=40, 47, for(c=30, 37, printf("\e[%d;%d;1mRosetta Code\e[0m\n", c, b)))
```



## Pascal


The CRT unit allows us to play with the console window, since at least the old Turbo Pascal days. We can clear the screen and specify colors by number or by name, among other tricks.


```pascal
program Colorizer;

uses CRT;

const SampleText = 'Lorem ipsum dolor sit amet';

var fg, bg: 0..15;

begin
    ClrScr;
    for fg := 0 to 7 do begin
        bg := 15 - fg;
        TextBackground(bg);
        TextColor(fg);
        writeln(SampleText)
    end;
    TextBackground(White);
    TextColor(Black);
end.
```



## Perl


```perl
my %colors = (
    red     => "\e[1;31m",
    green   => "\e[1;32m",
    yellow  => "\e[1;33m",
    blue    => "\e[1;34m",
    magenta => "\e[1;35m",
    cyan    => "\e[1;36m"
);
$clr = "\e[0m";

print "$colors{$_}$_ text $clr\n" for sort keys %colors;

# the Perl 6 code also works
use feature 'say';
use Term::ANSIColor;

say colored('RED ON WHITE', 'bold red on_white');
say colored('GREEN', 'bold green');
say colored('BLUE ON YELLOW', 'bold blue on_yellow');
say colored('MAGENTA', 'bold magenta');
say colored('CYAN ON RED', 'bold cyan on_red');
say colored('YELLOW', 'bold yellow');
```



## Perl 6


```perl6
use Terminal::ANSIColor;

say colored('RED ON WHITE', 'bold red on_white');
say colored('GREEN', 'bold green');
say colored('BLUE ON YELLOW', 'bold blue on_yellow');
say colored('MAGENTA', 'bold magenta');
say colored('CYAN ON RED', 'bold cyan on_red');
say colored('YELLOW', 'bold yellow');
```



## Phix

The following builtin constants (0..15) may be used:
BLACK, BLUE, BRIGHT_BLUE, BROWN, CYAN, BRIGHT_CYAN, GRAY, GREEN, BRIGHT_GREEN, MAGENTA, BRIGHT_MAGENTA, RED, BRIGHT_RED, WHITE, BRIGHT_WHITE, YELLOW

```Phix
--
-- demo\rosetta\Coloured_text.exw
--
### ==========================

--
text_color(GRAY)
bk_color(BLACK)
printf(1,"Background color#     00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15\n")
printf(1,"                      -----------------------------------------------\n")
for foreground=0 to 15 do
    printf(1,"Foreground color# %02d  ",foreground)
    for background=0 to 15 do
        text_color(foreground)
        bk_color(background)
        printf(1,"%02d",foreground)
        text_color(GRAY)
        bk_color(BLACK)
        printf(1," ")
    end for
    printf(1,"\n")
end for
printf(1,"\n\npress enter to exit")
{} = wait_key()
```

Output matches PureBasic


## PicoLisp

```PicoLisp
(unless (member (sys "TERM") '("linux" "xterm" "xterm-color" "xterm-256color" "rxvt"))
   (quit "This application requires a colour terminal") )

# Coloured text
(for X '((1 . "Red") (4 . "Blue") (3 . "Yellow"))
   (call 'tput "setaf" (car X))
   (prinl (cdr X)) )

# Blinking
(out '(tput "-S")
   (prinl "setab 1^Jsetaf 3^Jblink") )
(prin "Flashing text")

(call 'tput 'sgr0)   # reset
(prinl)
```



## PowerShell


```PowerShell

foreach ($color in [enum]::GetValues([System.ConsoleColor])) {Write-Host "$color color." -ForegroundColor $color}

```



## Python

```python

from colorama import init, Fore, Back, Style
init(autoreset=True)

print Fore.RED + "FATAL ERROR! Cannot write to /boot/vmlinuz-3.2.0-33-generic"
print Back.BLUE + Fore.YELLOW + "What a cute console!"
print "This is an %simportant%s word" % (Style.BRIGHT, Style.NORMAL)
print Fore.YELLOW  + "Rosetta Code!"
print Fore.CYAN    + "Rosetta Code!"
print Fore.GREEN   + "Rosetta Code!"
print Fore.MAGENTA + "Rosetta Code!"
print Back.YELLOW + Fore.BLUE + Style.BRIGHT + " " * 40 + " == Good Bye!"

```



This is a windows only solution without colorama

```python

from ctypes import *

windll.Kernel32.GetStdHandle.restype = c_ulong
h = windll.Kernel32.GetStdHandle(c_ulong(0xfffffff5))
#Default CMD colour = 7
def color(colour):
    windll.Kernel32.SetConsoleTextAttribute(h, colour)

for count in range (0, 16):
    color(count)
    print "This Colour Is #" + str(count)

print ""
color(7)
raw_input("holding cmd")

```



## PureBasic


```purebasic
If OpenConsole()
  PrintN("Background color#     00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15")
  PrintN("                      -----------------------------------------------")
  Define Foreground, Background
  For Foreground = 0 To 15
    ConsoleColor(7, 0) ;grey foreground, black background
    Print("Foreground color# " + RSet(Str(Foreground), 2, "0") + "  ")
    For Background = 0 To 15
      ConsoleColor(Foreground, Background)
      Print(RSet(Str(Foreground), 2, "0"))
      ConsoleColor(7, 0) ;grey foreground, black background
      Print(" ")
    Next
    PrintN("")
  Next

  ConsoleColor(7, 0) ;grey foreground, black background
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

[[Image:terminal_control,colored_text.png]]


## Racket

```racket

#lang racket

;; Utility interfaces to the low-level command
(define (capability? cap) (system (~a "tput "cap" > /dev/null 2>&1")))
(define (tput . xs) (system (apply ~a 'tput " " (add-between xs " "))) (void))
(define (colorterm?) (and (capability? 'setaf) (capability? 'setab)))
(define color-map '([black 0] [red 1] [green 2] [yellow 3]
                    [blue 4] [magenta 5] [cyan 6] [white 7]))
(define (foreground color) (tput 'setaf (cadr (assq color color-map))))
(define (background color) (tput 'setab (cadr (assq color color-map))))
(define (reset) (tput 'sgr0) (void))

;; Demonstration of use
(if (colorterm?)
  (begin (foreground 'blue)
         (background 'yellow)
         (displayln "Color output")
         (reset))
  (displayln "Monochrome only"))
(if (capability? 'blink)
  (begin (tput 'blink)
         (displayln "Blinking output")
         (reset))
  (displayln "Steady only"))

```



## REXX


### PC/REXX or Personal REXX

[[Image:REXX_terminal_control_coloured_text.JPG|thumb|right]]
This REXX program only works under PC/REXX (also called Personal REXX).

PC/REXX can execute under MSDOS (or a Windows DOS window), or OS/2.

Only Windows ''older'' than Windows 7 supports RC/REXX, newer versions of Windows won't run 16-bit code.


The prologue code (at the bottom of the program) is a collection of some general-purpose subroutines which determine:
* which environment (operating system) the REXX interpreter is running under
* if Windows/NT/XP/Vista/7/8 (the NT family) is running
* which REXX is being executed
* what literal to use to obtain the environmental variables (for the '''value''' bif)
* what the fileName, fileType/fileExt, fileMode/path is of the REXX program
* which command to use to clear the terminal screen
* invokes $H to show general documentation (1st and only arg = ?)
* invokes $H to show a flow diagram (1st and only arg = ?FLOW)
* invokes $H to show sample uses (1st and only arg = ?SAMPLE)
* invokes $H to show the author &amp; contact info (1st and only arg = ?AUTHOR)

All the prologue was left intact to give a general feel of the scope of the boilerplate code.

The prologue code is in many REXX programs and it's easier to keep them on one line for copying purposes and sorting.


The program displays 16 lines, each of a different color with text stating the color of the text.

(The black text, of course, is essentially invisible as the background is also black.)

```rexx
/*REXX program to display sixteen lines,  each of a different color.    */
parse arg !;  if !all()  then exit     /*exit if documentation specified*/
if \!dos  &  \!os2       then exit     /*if this isn't DOS,  then exit. */
if \!pcrexx              then exit     /*if this isn't PC/REXX,   exit. */

color.0  = 'black'                     /*┌─────────────────────────────┐*/
color.1  = 'dark blue'                 /*│ Normally, all programs issue│*/
color.2  = 'dark green'                /*│ the  (above) error messages │*/
color.3  = 'dark cyan/turquois'        /*│ through another REXX program│*/
color.4  = 'dark red'                  /*│ ($ERR)  which has more      │*/
color.5  = 'dark pink/magenta'         /*│ verbiage and  explanations, │*/
color.6  = 'dark yellow (orange)'      /*│ and issues the error text in│*/
color.7  = 'dark white'                /*│ red (if color is available).│*/
color.8  = 'brite black (grey/gray)'   /*└─────────────────────────────┘*/
color.9  = 'bright blue'
color.10 = 'bright green'
color.11 = 'bright cyan/turquois'
color.12 = 'bright red'
color.13 = 'bright pink/magenta'
color.14 = 'bright yellow'
color.15 = 'bright white'

         do j=0  to 15                 /*show all sixteen color codes.  */
         call scrwrite ,,'color code=['right(j,2)"]" color.j,,,j;   say
         end   /*j*/                   /*the  "SAY"  forces a  NEWLINE. */
exit                                   /*stick a fork in it, we're done.*/
/*══════════════════════════════════general 1-line subs═════════════════*/
!all:!!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:if symbol('!CALL')\=="VAR" then !call=;return !call
!env:!env='ENVIRONMENT';if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM';if !os2 then !env='OS2'!env;!ebcdic=1=='f0'x;return
!fid:parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;call !sys;if !dos then do;_=lastpos('\',!fn);!fm=left(!fn,_);!fn=substr(!fn,_+1);parse var !fn !fn '.' !ft;end;return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:parse upper version !ver !vernum !verdate .;!brexx='BY'==!vernum;!kexx='KEXX'==!ver;!pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver;!r4='REXX-R4'==!ver;!regina='REXX-REGINA'==left(!ver,11);!roo='REXX-ROO'==!ver;call !env;return
!sys:!cms=!sys=='CMS';!os2=!sys=='OS2';!tso=!sys=='TSO'|!sys=='MVS';!vse=!sys=='VSE';!dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD';call !rex;return
!var:call !fid;if !kexx then return space(dosenv(arg(1)));return space(value(arg(1),,!env))
```



## Ring


```ring

# Project : Terminal control/Coloured text

load "consolecolors.ring"

forecolors = [CC_FG_BLACK,CC_FG_RED,CC_FG_GREEN,CC_FG_YELLOW,
                   CC_FG_BLUE,CC_FG_MAGENTA,CC_FG_CYAN,CC_FG_GRAY,CC_BG_WHITE]

for n = 1 to len(forecolors)
     forecolor = forecolors[n]
     cc_print(forecolor | CC_BG_WHITE, "Rosetta Code" + nl)
next

```

Output image:

https://www.dropbox.com/s/d313jguinhr6jyl/Colors.jpg?dl=0


## Ruby


```Ruby
#!/usr/bin/ruby -w
require 'rubygems'
require 'colored'

print 'Colors are'.bold
print ' black'.black
print ' blue'.blue
print ' cyan'.cyan
print ' green'.green
print ' magenta'.magenta
print ' red'.red
print ' white '.white
print 'and'.underline, ' yellow'.yellow, "\n"
puts 'black on blue'.black_on_blue
puts 'black on cyan'.black_on_cyan
puts 'black on green'.black_on_green
puts 'black on magenta'.black_on_magenta
puts 'black on red'.black_on_red
puts 'white on black'.white_on_black
puts 'white on blue'.white_on_blue
puts 'white on cyan'.white_on_cyan
puts 'white on green'.white_on_green
puts 'white on magenta'.white_on_magenta
puts 'white on red'.white_on_red
```


[[File:Colored-text-ruby.png]]


## Scala

===Scala idiom (Functional Programming)===
```scala
object ColouredText extends App {
  val ESC = "\u001B"
  val (normal, bold, blink, black, white) =
    (ESC + "[0", ESC + "[1"
      , ESC + "[5" // not working on my machine
      , ESC + "[0;40m" // black background
      , ESC + "[0;37m" // normal white foreground
    )

  print(s"${ESC}c") // clear terminal first
  print(black) // set background color to black
  def foreColors = Map(
    ";31m" -> "red",
    ";32m" -> "green",
    ";33m" -> "yellow",
    ";34m" -> "blue",
    ";35m" -> "magenta",
    ";36m" -> "cyan",
    ";37m" -> "white")

  Seq(normal, bold, blink).flatMap(attr => foreColors.map(color => (attr, color)))
    .foreach { case (attr, (seq, text)) => println(s"$attr${seq}${text}") }
  println(white) // set foreground color to normal white
}

```


## Sidef

```ruby
var a = frequire('Term::ANSIColor');

say a.colored('RED ON WHITE', 'bold red on_white');
say a.colored('GREEN', 'bold green');
say a.colored('BLUE ON YELLOW', 'bold blue on_yellow');
say a.colored('MAGENTA', 'bold magenta');
say a.colored('CYAN ON RED', 'bold cyan on_red');
say a.colored('YELLOW', 'bold yellow');
```



## Tcl

This only works on Unix terminals as it delegates to the system <tt>tput</tt> command.

```tcl
# Utility interfaces to the low-level command
proc capability cap {expr {![catch {exec tput -S << $cap}]}}
proc colorterm {} {expr {[capability setaf] && [capability setab]}}
proc tput args {exec tput -S << $args >/dev/tty}
array set color {black 0 red 1 green 2 yellow 3 blue 4 magenta 5 cyan 6 white 7}
proc foreground x {exec tput -S << "setaf $::color($x)" > /dev/tty}
proc background x {exec tput -S << "setab $::color($x)" > /dev/tty}
proc reset {} {exec tput sgr0 > /dev/tty}

# Demonstration of use
if {[colorterm]} {
    foreground blue
    background yellow
    puts "Color output"
    reset
} else {
    puts "Monochrome only"
}

if {[capability blink]} {
    tput blink
    puts "Blinking output"
    reset
} else {
    puts "Steady only"
}
```


== {{header|TPP}} ==

```tpp
--color red
This is red
--color green
This is green
--color blue
This is blue
--color cyan
This is cyan
--color magenta
This is magenta
--color yellow
This is yellow
```


== {{header|UNIX Shell}} ==

```sh
#!/bin/sh
# Check if the terminal supports colour

# We should know from the TERM evironment variable whether the system
# is comfigured for a colour terminal or not, but we can also check the
# tput utility to check the terminal capability records.

COLORS=8    # Assume initially that the system supports eight colours
case $TERM in
  linux)
    ;;      # We know this is a colour terminal
  rxvt)
    ;;      # We know this is a colour terminal
  *)
    COLORS=`tput colors 2> /dev/null`    # Get the number of colours from the termcap file
esac
if [ -z $COLORS ] ; then
  COLORS=1    # Watch out for an empty returned value
fi

if [ $COLORS -le 2 ] ; then
  # The terminal is not colour
  echo "HW65000 This application requires a colour terminal" >&2
  exit 252    #ERLHW incompatible hardware
fi

# We know at this point that the terminal is colour

# Coloured text
tput setaf 1    #red
echo "Red"
tput setaf 4    #blue
echo "Blue"
tput setaf 3    # yellow
echo "Yellow"

# Blinking
tput setab 1    # red background
tput setaf 3    # yellow foreground
#tput blink     # enable blinking (but does not work on some terminals)
echo "Flashing text"

tput sgr0    # reset everything before exiting
```



## XPL0

Device 6 is similar to the standard console output device 0, but it
supports color. When light colors are used for background (in a text
rather than graphic display mode) they are displayed as standard (dim)
colors and the foreground color flashes. A BIOS call (int 10h, func 10h,
sub 03h) can be used to disable flashing and enable bright backgrounds.

It's possible to detect monochrome displays with a BIOS call, but
monochrome is so ancient it's not worth demonstrating. Actually the older
16-bit versions of the language made it easy to detect monochrome using
the Equip(ment) intrinsic, but the newer 32-bit version doesn't provide
the Equip intrinsic.

Of course these features are provided by the hardware of IBM-compatible
PCs and by simulators, such as DOSBox, on other computers.


```XPL0
code ChOut=8, Attrib=69;
def Black, Blue, Green, Cyan, Red, Magenta, Brown, White,  \attribute colors
    Gray, LBlue, LGreen, LCyan, LRed, LMagenta, Yellow, BWhite; \EGA palette
[ChOut(6,^C);           \default white on black background
Attrib(Red<<4+White);   \white on red
ChOut(6,^o);
Attrib(Green<<4+Red);   \red on green
ChOut(6,^l);
Attrib(Blue<<4+LGreen); \light green on blue
ChOut(6,^o);
Attrib(LRed<<4+White);  \flashing white on (standard/dim) red
ChOut(6,^u);
Attrib(Cyan<<4+Black);  \black on cyan
ChOut(6,^r);
]
```



## zkl

Terminal window on Linux, prints tables of colored glyphs.

```zkl
fcn table(title,mode){
   println("\n\e[1m%s\e[m\n bg\t fg".fmt(title));
   foreach b in ([40..48].chain([100..107])){
      print("%3d\t\e[%s%dm".fmt(b,mode,b));
      foreach f in ([30..38].chain([90..97])){ print("\e[%dm%3d ".fmt(f,f)) }
      println("\e[m");
   }
}

table("normal ( ESC[22m or ESC[m )", 		"22;");
table("bold ( ESC[1m )",			"1;");
table("faint ( ESC[2m ), not well supported",	"2;");
table("italic ( ESC[3m ), not well supported",	"3;");
table("underline ( ESC[4m ), support varies",	"4;");
table("blink ( ESC[5m )",			"5;");
table("inverted ( ESC[7m )",			"7;");
```


== {{header|ZX Spectrum Basic}} ==
The ZX Spectrum will always output colour. However if the television is black and white, these will show as various levels of luminence corresponding to the numerical colour value.

```zxbasic
10 FOR l=0 TO 7
20 READ c$: REM get our text for display
30 INK l: REM set the text colour
40 PRINT c$
50 NEXT l
60 PAPER 2: REM red background
70 INK 6: REM yellow forground
80 FLASH 1: REM activate flashing
90 PRINT "Flashing!": REM this will flash red and yellow (alternating inverse)
100 PAPER 7: INK 0: FLASH 0: REM normalize colours before exit
110 STOP

900 DATA "Black","Blue","Red","Magenta","Green","Cyan","Yellow","White"
```


