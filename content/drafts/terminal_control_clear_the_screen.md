+++
title = "Terminal control/Clear the screen"
description = ""
date = 2019-10-20T17:53:46Z
aliases = []
[extra]
id = 8412
[taxonomies]
categories = []
tags = []
+++

{{task|Terminal control}}[[Category:Initialization]]
Clear the terminal window.
[[Terminal Control::task| ]]

## 6502 Assembly

{{works with|http://vice-emu.sourceforge.net/ VICE}}
This example has been written for the C64 and uses the CHROUT KERNEL routine.
Compile with the [http://turbo.style64.org/ Turbo Macro Pro cross assembler]:

```txt

tmpx -i clrscr.s -o bin/clrscr.prg

```

Run with:

```txt

SYS680

```


```6502asm
; C64 - Terminal control: Clear the screen

; *** labels ***

chrout          = $ffd2

; *** main ***

                *=$02a8         ; sys 680

                lda clr         ; A = {CLR}
                jsr chrout      ; Output a character in A to the current
                                ; output device (default: screen).
                rts

; *** data ***

clr             .byte $93       ; the CLR control code
                                ; see https://en.wikipedia.org/wiki/PETSCII

```


{{works with|http://www.6502asm.com/ 6502asm.com|1.2}}
{{works with|http://www.6502asm.com/beta/index.html 6502asm.com|1.5 beta}}
The 6502asm.com emulator has a 32x32 pixel screen. First we fill this screen with random colored pixels, wait for a keypress and then "clear" the screen (fill it with black pixels).

```6502asm
; 6502asm.com - Clear the screen

                lda #$00        ; store the start address of the screen ($200)
                sta $00         ; at $00 and $01 (high byte in $01)
                lda #$02
                sta $01

                ldy #$00        ; Y = 0
fillscreen:
                lda $fe         ; A = random number from $fe
                sta ($00),y     ; put pixel (random color) to the screen
                iny             ; Y++
                bne fillscreen  ; loop if Y!=0
                inc $01         ; increase address high byte
                lda $01
                cmp #$06        ; A==6? (screen ends at $05ff)
                bne fillscreen  ; no -> loop

waitforkeypress:
                lda $ff         ; $ff is 0 if no key has been pressed
                beq waitforkeypress

                ldx #$00
                lda #$00        ; black
clearscreen:
                sta $0200,x
                sta $0300,x
                sta $0400,x
                sta $0500,x
                inx
                bne clearscreen

```



## Ada


For systems with ANSI terminal handling:


```Ada
with Ada.Text_IO;
procedure CLS is
begin
   Ada.Text_IO.Put(ASCII.ESC & "[2J");
end CLS;
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program clearScreen.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ BUFFERSIZE,          100

/* Initialized data */
.data
szMessStartPgm:            .asciz "Program start \n"
szMessEndPgm:              .asciz "Program normal end.\n"
szClear:                   .asciz "\33[2J"     @ console clear (id language C)
szClear1:                  .byte 0x1B
                           .byte 'c'           @ other console clear
                           .byte 0
szCarriageReturn:          .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:

    ldr r0,iAdrszMessStartPgm                   @ display start message
    bl affichageMess
    //ldr r0,iAdrszClear                        @ clear screen
    ldr r0,iAdrszClear1                         @ change for other clear screen
    bl affichageMess
    ldr r0,iAdrszMessEndPgm                     @ display end message
    bl affichageMess

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessStartPgm:       .int szMessStartPgm
iAdrszMessEndPgm:         .int szMessEndPgm
iAdrszClear:              .int szClear
iAdrszClear1:             .int szClear1
iAdrszCarriageReturn:     .int szCarriageReturn

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

Reference: http://www.autohotkey.com/forum/topic76532.html

```AHK
RunWait %comspec% /c cls
```


## AWK


```awk
system("clear")
```



## Axe


```axe>ClrHome</lang



## BASIC


{{works with|QBasic}}
{{works with|Locomotive Basic}}
{{works with|ZX Spectrum Basic}}
{{works with|BBC BASIC}}

```qbasic>CLS</lang


=
## Applesoft BASIC
=

```ApplesoftBasic>HOME</lang


=
## Aquarius BASIC
=

```Aquarius Basic
PRINT CHR$(11);
```


=
## Atari BASIC
=

```Atari Basic
PRINT CHR$(125);
```


=
## BBC BASIC
=

```bbcbasic>      CLS</lang

or

```bbcbasic>      VDU 12</lang

or

```bbcbasic
      PRINT CHR$(12);
```


=
## Commodore BASIC
=

```Commodore Basic
PRINT CHR$(147);
```


==={{header|GW-BASIC}}===

```qbasic>10 CLS</lang


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 CLEAR SCREEN
```


=
## PureBasic
=
Clears the whole console content using the current background color.

```PureBasic
ClearConsole()
```



## Batch File



```command>CLS</lang



## beeswax


Using the ANSI escape sequence <code>Esc[2J</code>.


```beeswax
_3F..}`[2J`
```



## Befunge

Assuming a terminal with support for ANSI escape sequences.

```befunge
"J2["39*,,,,@
```



## Blast



```blast>clear</lang



## Bracmat


```bracmat
sys$cls&
```



## C

The [[Minesweeper game#C|C version of the Minesweeper game]] uses curses.

If perhaps clear screen isn't used, call the function <code>cls</code> to do the trick.


```C
void cls(void) {
    printf("\33[2J");
}
```


Here is the cheaty way no one likes, only works on Windows.


```c
#include <stdio.h>
#include <stdlib.h>

void main() {
    printf ("clearing screen");
    getchar();
    system("cls");
}
```


For Unix-likes, changing the above <code>system("cls");</code> to <code>system("clear");</code> usually works, however the <code>getchar();</code> perhaps doesn't always work as expected if you press anything other than return. This is because of the raw vs. cooked terminal mode thing.

=={{header|C sharp|C#}}==

```csharp
System.Console.Clear();
```

Works on all .NET Core platforms. Throws an exception if output has been redirected to a file.


## COBOL


```cobol
       PROGRAM-ID. blank-terminal.

       DATA DIVISION.
       SCREEN SECTION.
       01  blank-screen BLANK SCREEN.

       PROCEDURE DIVISION.
           DISPLAY blank-screen

           GOBACK
           .
```


## Comal


```Comal>PAGE</lang



## Common Lisp


```lisp

(format t "~C[2J" #\Esc)

```

or it could be done passing the 'clear' command to the shell

```lisp

(defun sh (cmd)
   "A multi-implementation function equivalent for the C function system"
   #+clisp (shell cmd)
   #+ecl (si:system cmd)
   #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*)
   #+clozure (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))
(sh "clear")

```



## D


```d
extern (C) nothrow {
    void disp_open();
    void disp_move(int, int);
    void disp_eeop();
    void disp_close();
}

void main() {
    disp_open();
    disp_move(0, 0);
    disp_eeop();
    disp_close();
}
```



## Dc

===Using external "clear" binary===
Dc's command to execute shell commands can only be the last command of a line. That's no problem with multi line Dc programs but not very helpful in Dc oneliners:

```dc
!clear
```

Luckily there is a loophole:

```dc
[ !clear ] x
```



### Using a terminal control sequence

A common way to clear the screen with a terminal (assuming XTerm here) control sequence
could be to home the cursor ("ESC[H", "1B 5B 48") and then clear to the end of the
screen ("ESC[J", "1B 5B 4A").

```dc>16i 1B5B481B5B4A P</lang



## Elena

ELENA 3.4 :

```elena
public program()
{
   console.clear()
}
```



## Erlang


```Erlang

clear()->io:format(os:cmd("clear")).

```



## Euphoria


```Euphoria
clear_screen()
```


=={{header|F_Sharp|F#}}==

```fsharp
open System

Console.Clear()
```



## Forth


```forth>page</lang



## Fortran

'''Fortran 2008''':

```fortran
program clear
    character(len=:), allocatable :: clear_command
    clear_command = "clear" !"cls" on Windows, "clear" on Linux and alike
    call execute_command_line(clear_command)
end program
```



###  Intel Fortran on Windows

Using console functions, one can also clear the screen without using a system command. See also ''[https://msdn.microsoft.com/en-us/library/ms682022.aspx Clearing the Screen]'' on MSDN.


```fortran
program clear
    use kernel32
    implicit none
    integer(HANDLE) :: hStdout
    hStdout = GetStdHandle(STD_OUTPUT_HANDLE)
    call clear_console(hStdout)
contains
    subroutine clear_console(hConsole)
        integer(HANDLE) :: hConsole
        type(T_COORD) :: coordScreen = T_COORD(0, 0)
        integer(DWORD) :: cCharsWritten
        type(T_CONSOLE_SCREEN_BUFFER_INFO) :: csbi
        integer(DWORD) :: dwConSize

        if (GetConsoleScreenBufferInfo(hConsole, csbi) == 0) return
        dwConSize = csbi%dwSize%X * csbi%dwSize%Y

        if (FillConsoleOutputCharacter(hConsole, SCHAR_" ", dwConSize, &
            coordScreen, loc(cCharsWritten)) == 0) return

        if (GetConsoleScreenBufferInfo(hConsole, csbi) == 0) return

        if (FillConsoleOutputAttribute(hConsole, csbi%wAttributes, &
            dwConSize, coordScreen, loc(cCharsWritten)) == 0) return

        if (SetConsoleCursorPosition(hConsole, coordScreen) == 0) return
    end subroutine
end program
```



###  GNU Fortran on Windows

The preceding program can be compiled with GNU Fortran, with the following interface module for Windows API.


```fortran
module kernel32
    use iso_c_binding
    implicit none
    integer, parameter :: HANDLE = C_INTPTR_T
    integer, parameter :: PVOID = C_INTPTR_T
    integer, parameter :: LPDWORD = C_INTPTR_T
    integer, parameter :: BOOL = C_INT
    integer, parameter :: SHORT = C_INT16_T
    integer, parameter :: WORD = C_INT16_T
    integer, parameter :: DWORD = C_INT32_T
    integer, parameter :: SCHAR = C_CHAR
    integer(DWORD), parameter :: STD_INPUT_HANDLE = -10
    integer(DWORD), parameter :: STD_OUTPUT_HANDLE = -11
    integer(DWORD), parameter :: STD_ERROR_HANDLE = -12

    type, bind(C) :: T_COORD
        integer(SHORT) :: X, Y
    end type

    type, bind(C) :: T_SMALL_RECT
        integer(SHORT) :: Left
        integer(SHORT) :: Top
        integer(SHORT) :: Right
        integer(SHORT) :: Bottom
    end type

    type, bind(C) :: T_CONSOLE_SCREEN_BUFFER_INFO
        type(T_COORD) :: dwSize
        type(T_COORD) :: dwCursorPosition
        integer(WORD) :: wAttributes
        type(T_SMALL_RECT) :: srWindow
        type(T_COORD) :: dwMaximumWindowSize
    end type

    interface
        function FillConsoleOutputCharacter(hConsoleOutput, cCharacter, &
                nLength, dwWriteCoord, lpNumberOfCharsWritten) &
                bind(C, name="FillConsoleOutputCharacterA")
            import BOOL, C_CHAR, SCHAR, HANDLE, DWORD, T_COORD, LPDWORD
            !GCC$ ATTRIBUTES STDCALL :: FillConsoleOutputCharacter
            integer(BOOL) :: FillConsoleOutputCharacter
            integer(HANDLE), value :: hConsoleOutput
            character(kind=SCHAR), value :: cCharacter
            integer(DWORD), value :: nLength
            type(T_COORD), value :: dwWriteCoord
            integer(LPDWORD), value :: lpNumberOfCharsWritten
        end function
    end interface

    interface
        function FillConsoleOutputAttribute(hConsoleOutput, wAttribute, &
                nLength, dwWriteCoord, lpNumberOfAttrsWritten) &
                bind(C, name="FillConsoleOutputAttribute")
            import BOOL, HANDLE, WORD, DWORD, T_COORD, LPDWORD
            !GCC$ ATTRIBUTES STDCALL :: FillConsoleOutputAttribute
            integer(BOOL) :: FillConsoleOutputAttribute
            integer(HANDLE), value :: hConsoleOutput
            integer(WORD), value :: wAttribute
            integer(DWORD), value :: nLength
            type(T_COORD), value :: dwWriteCoord
            integer(LPDWORD), value :: lpNumberOfAttrsWritten
        end function
    end interface

    interface
        function GetConsoleScreenBufferInfo(hConsoleOutput, &
                lpConsoleScreenBufferInfo) &
                bind(C, name="GetConsoleScreenBufferInfo")
            import BOOL, HANDLE, T_CONSOLE_SCREEN_BUFFER_INFO
            !GCC$ ATTRIBUTES STDCALL :: GetConsoleScreenBufferInfo
            integer(BOOL) :: GetConsoleScreenBufferInfo
            integer(HANDLE), value :: hConsoleOutput
            type(T_CONSOLE_SCREEN_BUFFER_INFO) :: lpConsoleScreenBufferInfo
        end function
    end interface

    interface
        function SetConsoleCursorPosition(hConsoleOutput, dwCursorPosition) &
                bind(C, name="SetConsoleCursorPosition")
            import BOOL, HANDLE, T_COORD
            !GCC$ ATTRIBUTES STDCALL :: SetConsoleCursorPosition
            integer(BOOL) :: SetConsoleCursorPosition
            integer(HANDLE), value :: hConsoleOutput
            type(T_COORD), value :: dwCursorPosition
        end function
    end interface

    interface
        function GetStdHandle(nStdHandle) bind(C, name="GetStdHandle")
            import HANDLE, DWORD
            !GCC$ ATTRIBUTES STDCALL :: GetStdHandle
            integer(HANDLE) :: GetStdHandle
            integer(DWORD), value :: nStdHandle
        end function
    end interface
end module

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' FreeBASIC has a built in Cls command which clears the console on Windows
' but it may still be possible to scroll the console to view its
' previous contents. The following command prevents this.

Shell("Cls")
Sleep
```



## Go


### External command

Probably most reliable way to clear the screen.

```go
package main

import (
    "os"
    "os/exec"
)

func main() {
    c := exec.Command("clear")
    c.Stdout = os.Stdout
    c.Run()
}
```



### ANSI escape code

Simplest, if your terminal supports the ANSI code you want.

```go
package main

import "fmt"

func main() {
    fmt.Print("\033[2J")
}
```


### Ncurses

More complex, but works across multiple terminal types.
{{libheader|curses}}

```go
package main

import (
    "log"
    "time"

    "code.google.com/p/goncurses"
)

func main() {
    s, err := goncurses.Init()
    if err != nil {
        log.Fatal("goncurses:", err)
    }
    defer goncurses.End()
    s.Println("Clearing screen...")
    s.Refresh()
    time.Sleep(1 * time.Second)

    s.Clear() // clear screen

    // Goncurses saves the screen on Init and restores it on End.  This
    // GetChar() allows you to see the effect of the program before it exits.
    s.GetChar() // press any key to continue
}
```



## GUISS

This will only work if the terminal is sitting at a prompt.

```guiss
Window:Terminal,Type:clear[enter]
```



## Haskell



```Haskell

import System.Console.ANSI

main = clearScreen

```


=={{header|Icon}} and {{header|Unicon}}==
Example works for both Icon and Unicon.  Determine which system command to call by querying &features at run time.  Alternately, the related preprocessor symbols can be used to select the operating system.

```Icon
procedure main ()
  if &features == "MS Windows" then system("cls")  # Windows
  else if &features == "UNIX" then system("clear") # Unix
end
```



## J

Note: this is specific the java+gdi based J ide.

```j
smwrite_jijs_ ''
```


=={{Header|Java}}==
Using the ANSI escape sequence:

```java
public class Clear
{
    public static void main (String[] args)
    {
        System.out.print("\033[2J");
    }
}
```

An alternative sequence:

```java
public class Clear
{
    public static void main (String[] args)
    {
        System.out.print("\033\143");
    }
}
```


=={{Header|jq}}==

```jq
"\u001B[2J"
```

'''Example''':

```sh
$ jq -n '"\u001B[2J"'
```



## Jsish

Using ANSI terminal control codes.

```javascript
/* Terminal Control, clear the screen, in Jsish */
function cls() { printf('\u001b[2J'); }

;cls();

/*
=!EXPECTSTART!=
cls() ==> ^[[2Jundefined
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u terminalControlClear.jsi
[PASS] terminalControlClear.jsi
```



## Julia


```julia

println("\33[2J")

```



## Kotlin

{{works with|Ubuntu|14.04}}

```scala
// version 1.1.2

fun main(args: Array<String>) {
    println("\u001Bc")  // Esc + c
}
```



## Lasso


```Lasso
local(
	esc = decode_base64('Gw==')
)

stdout(#esc + '[2J')
```



## Logo


```logo>cleartext</lang

There is a separate command to reset the turtle graphics window.

```logo
clearscreen
cs      ; abbreviation for clearscreen
clean   ; like cs, but doesn't reset turtle position
```



## Lua


===Unix, Linux===

```lua
os.execute( "clear" )
```



### Windows


```lua
os.execute( "cls" )
```



## M2000 Interpreter

We can clear the screen with Form, Cls, Window statements. Each one perform something on the console form except clear it. Form change the size of letters using the size of window. Window change all. Cls change the background color, or use the same (is optional) and optional can set the line for the split screen (from that line we have scrolling). Also we can use Linespace statement (in twips) to set the line space. Statement Form set linespace automatic to give space between lines to full use of the window.


```M2000 Interpreter

Module Checkit {
      Pen 14 ' yellow
      \\ using form we set characters by rows
      \\ this clear the screen
      Form 80, 40
      \\ magenta for background, all form for vertical scrolling
      Cls 5, 0
      Print "wait... half second"
      Wait 500
      \\ clear using background color
      Cls
      \\ set the background (using html number for color), and set 4th line as top
      \\ for scrolling
      Cls #11bb22, 3
      Print "This is in 4th line"
      Wait  1000
      \\ now we center the form, using 12000 twips by 8000twips as border
      \\ form inside maybe smaller
      \\ font size is 16pt of current font
      Font "Courier New"
      Window 16, 12000, 8000;
      Print "This is first line"
      Wait 1000
      Font "Arial"
      \\ set the console form to screen 0, maximized
      Window 16, 0
      Cls 5   ' magenta
      Back {
            Cls 15 ' white border
      }
}
checkit

```




## Mathematica

Delegating to clear on terminal enabled OS(Mac Os, Linux)

```Mathematica
Run["clear"];
```



## Nanoquery


```nanoquery>cls</lang



## Nemerle

Exactly as [[Terminal_control/Clear_the_screen#C.23|C#]]. Because of possible (probable) ambiguity, this is one time it may be prudent to use:

```Nemerle
Console.Clear();
```

rather than importing the <tt>Console</tt> class with <tt>using System.Console;</tt> and calling as:

```Nemerle
Clear();
```



## NewLISP


```NewLISP

(! "clear")

```

In the newLISP command shell, this syntax is also proper:

```NewLISP

!clear

```



## Nim


```nim
import osproc
discard execCmd "clear"
```



## OCaml


Using the library [http://forge.ocamlcore.org/projects/ansiterminal/ ANSITerminal]:


```ocaml
#load "unix.cma"
#directory "+ANSITerminal"
#load "ANSITerminal.cma"
open ANSITerminal

let () =
  erase Screen
```



## Octave


```Octave> system clear;</lang


```Octave
 system('clear');
```



## Pascal



```Pascal>clrscr;</lang



## Perl

Assuming some ANSI terminal, easiest way is call your system's clear command:

```perl
system('clear')
```


If it's needed often:

```perl
$clear = `clear`; # clear simply prints some escape sequence, cache it
#... later:
print $clear;
```


We can also obtain the sequence using the Term::Cap module:


```perl
use Term::Cap;

$terminal = Term::Cap->Tgetent();
$clear = $terminal->Tputs('cl');
print $clear;
```



## Perl 6


```perl6
sub clear { print qx[clear] }
clear;
```



## Phix


```Phix
clear_screen()
```



## PicoLisp


```PicoLisp
(call 'clear)
```



## PowerShell


```powershell
Clear-Host
```



## ProDOS


```ProDOS>clearscurrentscreentext</lang



## Python

{{works with|Python|2.6}}
{{works with|Ubuntu|10.10}}

To clear the screen on Windows, replace 'clear' with 'cls'


```python
import os
os.system("clear")
```


Or similar to C example (won't work in Winsows console, since it does not recognize ANSI sequences):


```python
print "\33[2J"
```


On Windows, using functions from the kernel32 DLL:


```python
from ctypes import *

STD_OUTPUT_HANDLE = -11

class COORD(Structure):
    pass

COORD._fields_ = [("X", c_short), ("Y", c_short)]

class SMALL_RECT(Structure):
    pass

SMALL_RECT._fields_ = [("Left", c_short), ("Top", c_short), ("Right", c_short), ("Bottom", c_short)]

class CONSOLE_SCREEN_BUFFER_INFO(Structure):
    pass

CONSOLE_SCREEN_BUFFER_INFO._fields_ = [
    ("dwSize", COORD),
    ("dwCursorPosition", COORD),
    ("wAttributes", c_ushort),
    ("srWindow", SMALL_RECT),
    ("dwMaximumWindowSize", COORD)
]

def clear_console():
    h = windll.kernel32.GetStdHandle(STD_OUTPUT_HANDLE)

    csbi = CONSOLE_SCREEN_BUFFER_INFO()
    windll.kernel32.GetConsoleScreenBufferInfo(h, pointer(csbi))
    dwConSize = csbi.dwSize.X * csbi.dwSize.Y

    scr = COORD(0, 0)
    windll.kernel32.FillConsoleOutputCharacterA(h, c_char(b" "), dwConSize, scr, pointer(c_ulong()))
    windll.kernel32.FillConsoleOutputAttribute(h, csbi.wAttributes, dwConSize, scr, pointer(c_ulong()))
    windll.kernel32.SetConsoleCursorPosition(h, scr)

clear_console()
```



## Racket


```racket

#lang racket
(require (planet neil/charterm:3:0))
(with-charterm
 (void (charterm-clear-screen)))

```



## Retro


```Retro>clear</lang



## REXX


### generic

The [[REXX]] programming language does not include a facility to clear the screen natively.

However, it is possile to execute an external [[system command]] to achieve this task.

Below is some generic-type boilerplate (REXX) code which (possibly) determines:
::::* which REXX is being used
::::* which operating system is being used
::::* which (external) program to clear the screen


Also, not germane to this Rosetta Code task, the boilerplate code also possibly determines (among other things):
::::* if a particular documentation is to be shown
::::* which system pool name is to be used for system environmental variables
::::* which version of REXX is being used
::::* if the program is being invoked as a function, command, or subroutine


The following code works for:
::::* PC/REXX
::::* Personal REXX
::::* CMS REXX
::::* TSO REXX
::::* R4 REXX
::::* ROO REXX
::::* KEXX
::::* REXX compiler
::::* Regina REXX


The intent of the program's boilerplate code is to be able to be executed under most REXXes under most operating systems without changing the boilerplate REXX code.

```rexx
/*REXX boilerplate determines how to clear screen (under various REXXes)*/
trace off;      parse arg !            /*turn off tracing; get C.L. args*/
if !all(arg())  then exit              /*Doc request?   Show, then exit.*/
if !cms then address ''                /*Is this CMS?  Use this address.*/

!cls                                   /*clear the (terminal) screen.   */            /* ◄═══   this is where  "it"  happens.*/

exit                                   /*stick a fork in it, we're done.*/
/*═════════════════════════════general 1-line subs══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:  if symbol('!CALL')\=="VAR" then !call=; return !call
!env:  !env='ENVIRONMENT'; if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM'; if !os2 then !env='OS2'!env; !ebcdic=1=='f0'x; if !crx then !env='DOS'; return
!fid:  parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .; call !sys; if !dos then do; _=lastpos('\',!fn); !fm=left(!fn,_); !fn=substr(!fn,_+1); parse var !fn !fn '.' !ft; end; return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:  parse upper version !ver !vernum !verdate .; !brexx='BY'==!vernum; !kexx='KEXX'==!ver; !pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver; !r4='REXX-R4'==!ver; !regina='REXX-REGINA'==left(!ver,11); !roo='REXX-ROO'==!ver; call !env; return
!sys:  !cms=!sys=='CMS'; !os2=!sys=='OS2'; !tso=!sys=='TSO'|!sys=='MVS'; !vse=!sys=='VSE'; !dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD'; !crx=left(!sys,6)=='DOSCRX'; call !rex; return
!var:  call !fid; if !kexx then return space(dosenv(arg(1))); return space(value(arg(1),,!env))
```



### Regina

The [[regina]] interpreter supports the [[rexxcurses]] plugin, which provides a facility to clear the screen
(not shown here).


## Scala

{{libheader|Scala}}

```Scala
object Cls extends App {print("\033[2J")}
```



## Ring


```Ring
system('clear')
```



## Ruby


```Ruby
system 'clear'
```


Or, without reliance on the command line:
(equivalent to <code>`clear`</code>)

```Ruby
puts "\e[H\e[2J"
```



## Rust


```rust
print!("\x1B[2J");
```


Or using casting:


```rust
print!("{}[2J", 27 as char);
```



## Seed7

The function [http://seed7.sourceforge.net/libraries/console.htm#clear(in_console_file) clear] is portable and
clears the [http://seed7.sourceforge.net/libraries/console.htm console window].
''Clear'' is based on terminfo respectively the Windows console API.
A portable function to clear cannot rely on shell respectively cmd.exe commands,
because Windows uses ''CLS'' and Unix shells use ''CLEAR'', to clear a screen.
ANSI terminal escape sequences are also not 100% portable,
since not all terminals accept them.


```seed7
$ include "seed7_05.s7i";
  include "console.s7i";

const proc: main is func
  local
    var text: console is STD_NULL;
  begin
    console := open(CONSOLE);
    clear(console);
    # Terminal windows often restore the previous
    # content, when a program is terminated. Therefore
    # the program waits until Return/Enter is pressed.
    readln;
  end func;
```



## Sidef

Using a cached-function:

```ruby
func clear { print(static x = `clear`) };
clear();
```


Directly invoking the `clear` command:

```ruby
Sys.run('clear');
```


Alternatively, without reliance on the command line:

```ruby
print "\e[3J\e[H\e[2J";
```



## Smalltalk


```smalltalk>Transcript clear.</lang



## SmileBASIC

SmileBASIC's text screen is mixed in with its graphics screen, background screen, and sprites screen.

### Text screen only

To clear just the text screen:

```smilebasic>CLS</lang


### All screens

Clearing all of the screens, and resetting display options can be done with:

```smilebasic>ACLS</lang



## SPL


```spl
#.clear()
```



## Stata


The '''[https://www.stata.com/help.cgi?cls cls]''' command clears the Results window, which is the closest to a ''terminal'' in Stata.


## Tcl

This only works on systems with ANSI terminal handling, i.e., Unix platforms.

```tcl
puts -nonewline "\033\[2J"
flush stdout
```



## UNIX Shell


The clear command can be used to clear the terminal screen:

{{works with|Bourne Shell}}


```bash
clear

# Alternative method using tput
tput clear
```



## Visual Basic .NET


```vbnet
System.Console.Clear()
```

Works on all .NET Core platforms. Throws an exception if output has been redirected to a file.


## XPL0


```XPL0
code Clear=40;
Clear;
```



## zkl


```zkl
System.cmd(System.isWindows and "cls" or "clear");
// or, for ANSI terminals: print("\e[2J")
```


{{omit from|ACL2}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|Inform 7}}
