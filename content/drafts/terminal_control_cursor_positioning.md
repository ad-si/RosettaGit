+++
title = "Terminal control/Cursor positioning"
description = ""
date = 2019-08-29T16:29:42Z
aliases = []
[extra]
id = 8413
[taxonomies]
categories = []
tags = []
+++

{{task|Terminal control}}

Move the cursor to column 3, row 6 and display the word "Hello", so that the letter H is in column 3 on row 6.
[[Terminal Control::task| ]]

## Ada



```Ada
with Ada.Text_IO;

procedure Cursor_Pos is

begin
   Ada.Text_IO.Set_Line(6);
   Ada.Text_IO.Set_Col(3);
   Ada.Text_IO.Put("Hello");
end Cursor_Pos;
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program cursorPos.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall


/* Initialized data */
.data
szMessStartPgm:            .asciz "Program start \n"
szMessEndPgm:              .asciz "Program normal end.\n"
szMessMovePos:             .asciz "\033[6;3HHello\n"
szCarriageReturn:          .asciz "\n"
szClear1:                  .byte 0x1B
                           .byte 'c'           @ other console clear
                           .byte 0
/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:

    ldr r0,iAdrszMessStartPgm                   @ display start message
    bl affichageMess
    ldr r0,iAdrszClear1
    bl affichageMess
    ldr r0,iAdrszMessMovePos
    bl affichageMess

    ldr r0,iAdrszMessEndPgm                     @ display end message
    bl affichageMess

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessStartPgm:        .int szMessStartPgm
iAdrszMessEndPgm:          .int szMessEndPgm
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrszClear1:              .int szClear1
iAdrszMessMovePos:         .int szMessMovePos

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

{{works with|AutoHotkey_L}}
Remember that AHK is not built for the console, so we must call the WinAPI directly.

```AHK
DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )

DllCall("SetConsoleCursorPosition", UPtr, hConsole, UInt, (6 << 16) | 3)
WriteConsole(hConsole, "Hello")

MsgBox

WriteConsole(hConsole, text){
	VarSetCapacity(out, 16)
	If DllCall( "WriteConsole", UPtr, hConsole, Str, text, UInt, StrLen(text)
				  , UPtrP, out, uint, 0 )
		return out
	return 0
}
```



## Axe

Since the rows and columns are zero-indexed, we must subtract 1 from both.

```axe
Output(2,5,"HELLO")
```



## BASIC


=== {{header|Applesoft BASIC}} ===

```Applesoft BASIC
 10  VTAB 6: HTAB 3
 20  PRINT "HELLO"
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PRINT AT 6,3:"Hello"
```


=== {{header|Locomotive Basic}} ===

```locobasic
 10 LOCATE 3,6
 20 PRINT "Hello"
```


=== {{header|ZX Spectrum Basic}} ===

```zxbasic
 10 REM The top left corner is at position 0,0
 20 REM So we subtract one from the coordinates
 30 PRINT AT 5,2 "Hello"
```


=== {{header|BBC BASIC}} ===

```bbcbasic
PRINT TAB(2,5);"Hello"
```


=== {{header|Commodore BASIC}} ===

```basic
 100 print chr$(19) :rem change to lowercase set
 110 print chr$(14) :rem go to position 1,1
 120 print:print:print:print
 130 print tab(2) "Hello"
```



## Befunge

Assuming a terminal with support for ANSI escape sequences.

```befunge
0"olleHH3;6["39*>:#,_$@
```



## Blast


```blast
# This will display a message at a specific position on the terminal screen
.begin
cursor 6,3
display "Hello!"
return
# This is the end of the script
```


=={{header|C}}/{{header|C++}}==
Using ANSI escape sequence, where ESC[y;xH moves curser to row y, col x:

```c
#include <stdio.h>
int main()
{
	printf("\033[6;3HHello\n");
	return 0;
}
```


The C version of the minesweeper game uses curses.
[[Minesweeper_game#C]]

On Windows, using console API:

```c
#include <windows.h>
```


int main() {
    HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    COORD pos = {3, 6};
    SetConsoleCursorPosition(hConsole, pos);
    WriteConsole(hConsole, "Hello", 5, NULL, NULL);
    return 0;
}
```


=={{header|C sharp|C#}}==
{{works with|Mono|1.2}}
{{works with|Visual C sharp|Visual C#|2003}}

```csharp
static void Main(string[] args)
{
    Console.SetCursorPosition(3, 6);
    Console.Write("Hello");
}
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cursor-positioning.

       PROCEDURE DIVISION.
           DISPLAY "Hello" AT LINE 6, COL 3

           GOBACK
           .
```




## D

ANSI escape sequences allow you to move the cursor anywhere on the screen. See more at: [http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x361.html Bash Prompt HowTo - Chapter 6. ANSI Escape Sequences: Colours and Cursor Movement]

 Position the Cursor:
  \033[<L>;<C>H
     or
  \033[<L>;<C>f
 puts the cursor at line L and column C.


```D

import std.stdio;

void main()
{
  writef("\033[6;3fHello");
}

```


'''Output:'''
 0123456789
 1
 2
 3
 4
 5
 6  Hello
 9
 8
 9


## Elena

ELENA 4.x :

```elena
public program()
{
    console.setCursorPosition(3,6).write("Hello")
}
```



## Euphoria


```Euphoria
position(6,3)
puts(1,"Hello")
```


=={{header|F_Sharp|F#}}==

```fsharp
open System

Console.SetCursorPosition(3, 6)
Console.Write("Hello")
```



## Forth


```forth
2 5 at-xy ." Hello"
```



## Fortran


### Intel Fortran on Windows


```fortran
program textposition
    use kernel32
    implicit none
    integer(HANDLE) :: hConsole
    integer(BOOL) :: q

    hConsole = GetStdHandle(STD_OUTPUT_HANDLE)
    q = SetConsoleCursorPosition(hConsole, T_COORD(3, 6))
    q = WriteConsole(hConsole, loc("Hello"), 5, NULL, NULL)
end program
```



## Go


### External command


```go
package main

import (
    "bytes"
    "fmt"
    "os"
    "os/exec"
)

func main() {
    cmd := exec.Command("tput", "-S")
    cmd.Stdin = bytes.NewBufferString("clear\ncup 5 2")
    cmd.Stdout = os.Stdout
    cmd.Run()
    fmt.Println("Hello")
}
```


### ANSI escape codes


```go
package main

import "fmt"

func main() {
    fmt.Println("\033[2J\033[6;3HHello")
}
```


### Ncurses

{{libheader|curses}}

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
    s.Move(5, 2)
    s.Println("Hello")
    s.GetChar()
}
```


=={{header|Icon}} and {{header|Unicon}}==
If the OS has older termcap files, CUP is included with <tt>link ansi</tt>

```unicon
procedure main()
    writes(CUP(6,3), "Hello")
end

procedure CUP(i,j)
    writes("\^[[",i,";",j,"H")
    return
end
```



## J

Using terminal positioning verbs of [[Terminal_control/Coloured_text#J]]

```J
'Hello',~move 6 3
```




## Julia


```julia
const ESC = "\u001B"

gotoANSI(x, y) = print("$ESC[$(y);$(x)H")

gotoANSI(3, 6)
println("Hello")

```



## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
// version 1.1.2

fun main(args: Array<String>) {
    print("\u001Bc") // clear screen first
    println("\u001B[6;3HHello")
}
```



## Lasso


```Lasso
local(esc = decode_base64('Gw=='))

stdout( #esc + '[6;3HHello')
```



## Liberty BASIC


```lb
locate 3, 6
print "Hello"

```



## Logo


```logo
setcursor [2 5]
type "Hello
```

You can also draw positioned text on the turtle graphics window.

```logo
setpos [20 50]
setxy 20 30   ; alternate way to set position
label "Hello
```



## Mathematica


```Mathematica
Run["tput cup 6 3"]
Print["Hello"]
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 LOCATE 3,6
20 PRINT "HELLO"
```



## OCaml


Using the library [http://forge.ocamlcore.org/projects/ansiterminal/ ANSITerminal]:


```ocaml
#load "unix.cma"
#directory "+ANSITerminal"
#load "ANSITerminal.cma"

module Trm = ANSITerminal

let () =
  Trm.erase Trm.Screen;
  Trm.set_cursor 3 6;
  Trm.print_string [] "Hello";
;;
```



## Pascal


```Pascal

program cursor_pos;
uses crt;
begin
  gotoxy(6,3);
  write('Hello');
end.

```



## Perl

Using the Term::Cap module:

```perl

use Term::Cap;

my $t = Term::Cap->Tgetent;
print $t->Tgoto("cm", 2, 5); # 0-based
print "Hello";

```



## Nim


```nim
import terminal
setCursorPos(3, 6)
echo "Hello"
```



## Perl 6

Assuming an ANSI terminal:

```perl6
print "\e[6;3H";
print 'Hello';
```



## Phix


```Phix
position(6,3)
puts(1,"Hello")
```



## PicoLisp


```PicoLisp
(call 'tput "cup" 6 3)
(prin "Hello")
```



## PowerShell

The following will only work in the PowerShell console host. Most notably it will not work in the PowerShell ISE.

```powershell
$Host.UI.RawUI.CursorPosition = New-Object System.Management.Automation.Host.Coordinates 2,5
$Host.UI.Write('Hello')
```

Alternatively, in any PowerShell host that uses the Windows console, one can directly use the .NET <code>Console</code> class:

```powershell
[Console]::SetCursorPosition(2,5)
[Console]::Write('Hello')
```



## PureBasic


```PureBasic
EnableGraphicalConsole(#True)
ConsoleLocate(3,6)
Print("Hello")
```



## Python

Using ANSI escape sequence, where ESC[y;xH moves curser to row y, col x:
```Python
print("\033[6;3HHello")
```

On Windows it needs to import and init the [http://code.google.com/p/colorama/ colorama] module first.

ANSI sequences are not recognized in Windows console, here is a program using Windows API:


```python
from ctypes import *

STD_OUTPUT_HANDLE = -11

class COORD(Structure):
    pass

COORD._fields_ = [("X", c_short), ("Y", c_short)]

def print_at(r, c, s):
    h = windll.kernel32.GetStdHandle(STD_OUTPUT_HANDLE)
    windll.kernel32.SetConsoleCursorPosition(h, COORD(c, r))

    c = s.encode("windows-1252")
    windll.kernel32.WriteConsoleA(h, c_char_p(c), len(c), None, None)

print_at(6, 3, "Hello")
```



## Racket


```racket

#lang racket
(require (planet neil/charterm:3:0))
(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 3 6)
 (displayln "Hello World"))

```



## REXX

The REXX language doesn't have any cursor or screen management tools, but some REXX interpreters have added the functionality via different methods.
{{works with|PC/REXX, Personal REXX}}

```rexx
/*REXX program demonstrates cursor position and writing of text to same.*/

call cursor  3,6                       /*move the cursor to row 3, col 6*/
say 'Hello'                            /*write the text at that location*/

call scrwrite 30,50,'Hello.'           /*another method.       */

call scrwrite 40,60,'Hello.',,,14      /*another ... in yellow.*/
```



## Retro


```Retro
with console'
: hello 3 6 at-xy "Hello" puts ;
```



## Ring


```ring

# Project : Terminal control/Cursor positioning

for n = 1 to 5
     see nl
next
see "  Hello"

```

Output:

```txt






   Hello

```



## Ruby

{{libheader|curses}}

```ruby
require 'curses'

Curses.init_screen
begin
  Curses.setpos(6, 3)  # column 6, row 3
  Curses.addstr("Hello")

  Curses.getch  # Wait until user presses some key.
ensure
  Curses.close_screen
end
```



## Scala

{{Works with|Ubuntu|14.04}}

```scala
object Main extends App {
    print("\u001Bc") // clear screen first
    println("\u001B[6;3HHello")
}
```


## Seed7

The function [http://seed7.sourceforge.net/libraries/console.htm#setPos%28ref_console_file,__ref_integer,ref_integer%29 setPos] is portable and
positions the cursor on the [http://seed7.sourceforge.net/libraries/console.htm console window].
''SetPos'' is based on terminfo respectively the Windows console API.


```seed7
$ include "seed7_05.s7i";
  include "console.s7i";

const proc: main is func
  local
    var text: console is STD_NULL;
  begin
    console := open(CONSOLE);
    setPos(console, 6, 3);
    write(console, "Hello");
    # Terminal windows often restore the previous
    # content, when a program is terminated. Therefore
    # the program waits until Return/Enter is pressed.
    readln;
  end func;
```



## Tcl


```tcl>exec tput cup 5 2
/dev/tty
puts "Hello"
```



## UNIX Shell



```sh
# The tput utility numbers from zero, so we have subtracted 1 from row and column
# number to obtain correct positioning.
tput cup 5 2
```



## Whitespace

Using ANSI escape sequence, where ESC[y;xH moves curser to row y, col x (see below):

```whitespace



































```


This solution was generated from the following pseudo-Assembly.

```asm
push "Hello"	;The characters are pushed onto the stack in reverse order
push "[6;3H"
push 27		;ESC

push 11		;Number of characters to print
call 0		;Calls print-string function
exit

0:
  dup jumpz 1	;Return if counter is zero
  exch prtc	;Swap counter with the next character and print it
  push 1 sub	;Subtract one from counter
  jump 0	;Loop back to print next character

1:
  pop ret	;Pop counter and return
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

[Cursor(2, 5);          \3rd column, 6th row
Text(0, "Hello");       \upper-left corner is coordinate 0, 0
]
```



## zkl

{{trans|C/C++}}
Using ANSI escape sequence, where ESC[y;xH moves curser to row y, col x:

```zkl
print("\e[6;3H" "Hello");
```


{{omit from|ACL2}}
{{omit from|GUISS}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|Scratch|no concept of a terminal window}}
