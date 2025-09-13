+++
title = "Terminal control/Positional read"
description = ""
date = 2019-02-25T04:46:50Z
aliases = []
[extra]
id = 8418
[taxonomies]
categories = ["task", "Terminal control"]
tags = []
+++

## Task

{{task|Terminal control}}[[Terminal Control::task| ]]
Determine the character displayed on the screen at column 3, row 6 and store that character in a variable. Note that it is permissible to utilize system or language provided methods or system provided facilities, system maintained records or available buffers or system maintained display records to achieve this task, rather than query the terminal directly, if those methods are more usual for the system type or language.


## AutoHotkey

<p>AutoHotkey is not built for the command line, so we need call the WinAPI directly.</p><p>For fun, this writes random characters to the command window so that it has something to retrieve. </p>

```AHK
DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )
Loop 10
{
	Loop 10
	{
		Random, asc, % asc("A"), % Asc("Z")
		WriteConsole(hConsole, Chr(asc))
	}
	WriteConsole(hConsole, "`n")
}

MsgBox % ReadConsoleOutputCharacter(hConsole, 1, 3, 6)

;
###  The below simply wraps part of the WinAPI


WriteConsole(hConsole, text){
	VarSetCapacity(out, 16)
	If DllCall( "WriteConsole", UPtr, hConsole, Str, text, UInt, StrLen(text)
				  , UPtrP, out, uint, 0 )
		return out
	return 0
}
ReadConsoleOutputCharacter(hConsole, length, x, y){
	VarSetCapacity(out, length * (1 << !!A_IsUnicode))
	VarSetCapacity(n, 16)
	if DllCall( "ReadConsoleOutputCharacter"
		, UPtr, hConsole
		, Str, out
		, UInt, length
		, UInt, x | (y << 16)
		, UPtrP, n )

		&& VarSetCapacity(out, -1)
				return out
	return 0
}
```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
 10  DEF  FN C(H) =  SCRN( H - 1,(V - 1) * 2) +  SCRN( H - 1,(V - 1) * 2 + 1) * 16
 20  LET V = 6:C$ =  CHR$ ( FN C(3))
```


=
## Locomotive Basic
=


```locobasic
10 LOCATE 3,6
20 a$=COPYCHR$(#0)
```


Amstrad CPC screen memory only stores pixels but no character information (as opposed to e.g. the C64), so the firmware routine (TXT_UNWRITE) called by BASIC works by trying to find a match between screen pixels and the shape of a currently defined character. If the character table or screen pixels in the area of the character are changed between writing and reading, COPYCHR$ will therefore fail.

===[[QuickBASIC#QBasic|QBasic]]===

The top left corner is (1, 1).


```qbasic
c$ = CHR$(SCREEN(6, 3))
```


=== {{header|ZX Spectrum Basic}} ===

```basic
 10 REM The top left corner is at position 0,0
 20 REM So we subtract one from the coordinates
 30 LET c$ = SCREEN$(5,2)
```



## BBC BASIC

```bbcbasic
      PRINT TAB(2,5) "Here"
      char$ = GET$(2,5)
      PRINT ''"Character at column 3 row 6 was " char$
```



## C

With the Windows console, call <code>GetConsoleScreenBufferInfo()</code> to find the top-left corner of the display screen. Then add (3, 6) to the top-left corner and call <code>ReadConsoleOutputCharacterW()</code> to read character. This program reckons that the top-left corner is (0, 0).

```c
#include <windows.h>
#include <wchar.h>

int
main()
{
	CONSOLE_SCREEN_BUFFER_INFO info;
	COORD pos;
	HANDLE conout;
	long len;
	wchar_t c;

	/* Create a handle to the console screen. */
	conout = CreateFileW(L"CONOUT$", GENERIC_READ | GENERIC_WRITE,
	    FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
	    0, NULL);
	if (conout == INVALID_HANDLE_VALUE)
		return 1;

	/* Where is the display window? */
	if (GetConsoleScreenBufferInfo(conout, &info) == 0)
		return 1;

	/* c = character at position. */
	pos.X = info.srWindow.Left + 3;  /* Column */
	pos.Y = info.srWindow.Top  + 6;  /* Row */
	if (ReadConsoleOutputCharacterW(conout, &c, 1, pos, &len) == 0 ||
	    len <= 0)
		return 1;

	wprintf(L"Character at (3, 6) had been '%lc'\n", c);
	return 0;
}
```



## Go

```go
package main

/*
#include <windows.h>
*/
import "C"
import "fmt"

func main() {
    for i := 0; i < 80*25; i++ {
        fmt.Print("A")  // fill 80 x 25 console with 'A's
    }
    fmt.Println()
    conOut := C.GetStdHandle(C.STD_OUTPUT_HANDLE)
    info := C.CONSOLE_SCREEN_BUFFER_INFO{}
    pos := C.COORD{}
    C.GetConsoleScreenBufferInfo(conOut, &info)
    pos.X = info.srWindow.Left + 3 // column number 3 of display window
    pos.Y = info.srWindow.Top + 6  // row number 6 of display window
    var c C.wchar_t
    var le C.ulong
    ret := C.ReadConsoleOutputCharacterW(conOut, &c, 1, pos, &le)
    if ret == 0 || le <= 0 {
        fmt.Println("Something went wrong!")
        return
    }
    fmt.Printf("The character at column 3, row 6 is '%c'\n", c)
}
```


```txt

The character at column 3, row 6 is 'A'

```



## Julia

```julia
using LibNCurses

randtxt(n) = foldl(*, rand(split("1234567890abcdefghijklmnopqrstuvwxyz", ""), n))

initscr()

for i in 1:20
    LibNCurses.mvwaddstr(i, 1, randtxt(50))
end

row = rand(1:20)
col = rand(1:50)
ch = LibNCurses.winch(row, col)
LibNCurses.mvwaddstr(col, 52, "The character at ($row, $col) is $ch.") )

```



## Kotlin

This is based on the C entry and works on Windows 10:

```scala
// Kotlin Native version 0.3

import kotlinx.cinterop.*
import win32.*

fun main(args: Array<String>) {
    for (i in 0 until (80 * 25)) print("A")  // fill 80 x 25 console with 'A's
    println()
    memScoped {
        val conOut = GetStdHandle(-11)
        val info = alloc<CONSOLE_SCREEN_BUFFER_INFO>()
        val pos = alloc<COORD>()
        GetConsoleScreenBufferInfo(conOut, info.ptr)
        pos.X = (info.srWindow.Left + 3).toShort()  // column number 3 of display window
        pos.Y = (info.srWindow.Top + 6).toShort()   // row number 6 of display window
        val c = alloc<wchar_tVar>()
        val len = alloc<IntVar>()
        ReadConsoleOutputCharacterW(conOut, c.ptr, 1, pos.readValue(), len.ptr)
        if (len.value == 1) {
            val ch = c.value.toChar()
            println("The character at column 3, row 6 is '$ch'")
        }
        else println("Something went wrong!")
    }
}
```


```txt

The character at column 3, row 6 is 'A'

```



## Perl 6

```perl6
#!/usr/bin/env perl6

use v6;
use NCurses;

# Reference:
# https://github.com/azawawi/perl6-ncurses

# Initialize curses window
my $win = initscr() or die "Failed to initialize ncurses\n";

# Print random text in a 10x10 grid

for ^10 { mvaddstr($_ , 0, (for ^10 {(41 .. 90).roll.chr}).join )};

# Read

my $icol = 3 - 1;
my $irow = 6 - 1;

my $ch = mvinch($irow,$icol);

# Show result

mvaddstr($irow, $icol+10, 'Character at column 3, row 6 = ' ~ $ch.chr);

mvaddstr( LINES() - 2, 2, "Press any key to exit..." );

# Refresh (this is needed)
nc_refresh;

# Wait for a keypress
getch;

# Cleanup
LEAVE {
    delwin($win) if $win;
    endwin;
}
```


```txt

+W18:5I<1N
N-I.HG45SK
BFJY8:AK)8
J+4U<H1++:
RP>BX-/19Y
URDESVX;HX  Character at column 3, row 6 = D
J7+X3@E<BG
M;?2U<8+FI
)@BG,:D)O1
)>A-=LDY-.












  Press any key to exit...

```




## Phix


```Phix
--
-- demo\rosetta\Positional_read.exw
--
### ==========================

--
position(6,1) -- line 6 column 1 (1-based)
puts(1,"abcdef")
integer {ch,attr} = get_screen_char(6,3)
printf(1,"\n\n=>%c",ch)
{} = wait_key()
```

```txt






abcdef

=>c

```



## Python


```python
import curses
from random import randint


# Print random text in a 10x10 grid
stdscr = curses.initscr()
for rows in range(10):
    line = ''.join([chr(randint(41, 90)) for i in range(10)])
    stdscr.addstr(line + '\n')

# Read
icol = 3 - 1
irow = 6 - 1
ch = stdscr.instr(irow, icol, 1).decode(encoding="utf-8")

# Show result
stdscr.move(irow, icol + 10)
stdscr.addstr('Character at column 3, row 6 = ' + ch + '\n')
stdscr.getch()

curses.endwin()

```


```txt
T@4;4G,XIJ
>C+PE0)RM;
JEV6B/8E?H
FSC>41UIGR
V>41JMXMOW
IY0*KH6M;B  Character at column 3, row 6 = 0
-6<UL*>DU7
MZ))<5D:B8
.@UB/P6UQ)
<9HYH)<ZJF

```


## Racket

Works in a CMD box on Windows:

```racket

#lang racket
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer defwin #f)
(defwin GetStdHandle (_fun _int -> _pointer))
(defwin ReadConsoleOutputCharacterA
  (_fun _pointer _pointer _uint _uint [len : (_ptr o _uint)] -> _bool))

(define b (make-bytes 1 32))
(and (ReadConsoleOutputCharacterA (GetStdHandle -11) b 1 #x50002)
     (printf "The character at 3x6 is <~a>\n" b))

```



## REXX

The REXX doesn't have any cursor or screen management tools, but some REXX interpreters have added the functionality via different methods.
```rexx
/*REXX program demonstrates reading a char at  specific screen location.*/
row = 6                                /*point to row six.              */
col = 3                                /*point to column three.         */
howMany = 1                            /*read one character.            */

stuff = scrRead(row, col, howMany)     /*this'll do it.                 */

other = scrRead(40, 6, 1)              /*same thing, but for row forty. */
                                       /*stick a fork in it, we're done.*/
```



## TXR


```txrlisp
;;; Type definitions and constants

(typedef BOOL (enum BOOL FALSE TRUE))
(typedef HANDLE cptr)
(typedef WCHAR wchar)
(typedef DWORD uint32)
(typedef WORD uint16)
(typedef SHORT short)

(typedef COORD
         (struct COORD
           (X SHORT)
           (Y SHORT)))

(typedef SMALL_RECT
         (struct SMALL_RECT
           (Left SHORT)
           (Top SHORT)
           (Right SHORT)
           (Bottom SHORT)))

(typedef CONSOLE_SCREEN_BUFFER_INFO
         (struct CONSOLE_SCREEN_BUFFER_INFO
           (dwSize COORD)
           (dwCursorPosition COORD)
           (wAttributes WORD)
           (srWindow SMALL_RECT)
           (dwMaximumWindowSize COORD)))

;;; Various constants

(defvarl STD_INPUT_HANDLE (- #x100000000 10))
(defvarl STD_OUTPUT_HANDLE (- #x100000000 11))
(defvarl STD_ERROR_HANDLE (- #x100000000 12))

(defvarl NULL cptr-null)
(defvarl INVALID_HANDLE_VALUE (cptr-int -1))

;;; Foreign Function Bindings

(with-dyn-lib "kernel32.dll"
  (deffi GetStdHandle "GetStdHandle" HANDLE (DWORD))
  (deffi GetConsoleScreenBufferInfo "GetConsoleScreenBufferInfo"
         BOOL (HANDLE (ptr-out CONSOLE_SCREEN_BUFFER_INFO)))
  (deffi ReadConsoleOutputCharacter "ReadConsoleOutputCharacterW"
         BOOL (HANDLE (ptr-out (array 1 WCHAR))
                       DWORD COORD (ptr-out (array 1 DWORD)))))

;;; Now the character at <2, 5> -- column 3, row 6.

(let ((console-handle (GetStdHandle STD_OUTPUT_HANDLE)))
  (when (equal console-handle INVALID_HANDLE_VALUE)
    (error "couldn't get console handle"))

  (let* ((cinfo (new CONSOLE_SCREEN_BUFFER_INFO))
         (getinfo-ok (GetConsoleScreenBufferInfo console-handle cinfo))
         (coord (if getinfo-ok
                  ^#S(COORD X ,(+ 2 cinfo.srWindow.Left)
                            Y ,(+ 5 cinfo.srWindow.Top))
                  #S(COORD X 0 Y 0)))
         (chars (vector 1))
         (nread (vector 1))
         (read-ok (ReadConsoleOutputCharacter console-handle chars
                                              1 coord nread)))
    (when (eq getinfo-ok 'FALSE)
      (error "GetConsoleScreenBufferInfo failed"))
    (prinl cinfo)
    (when (eq read-ok 'FALSE)
      (error "ReadConsoleOutputCharacter failed"))
    (unless (plusp [nread 0])
      (error "ReadConsoleOutputCharacter read zero characters"))
    (format t "character is ~s\n" [chars 0])))
```


Notes:
* An <code>ptr-out</code> to an <code>array</code> of 1 <code>DWORD</code> is used for the number of characters out parameter. The FFI type <code>(ptr-out DWORD)</code> cannot work as a function argument, because integer objects are not mutable, and there isn't any concept of taking the address of a variable. A vector of 1 integer is mutable, and by making such a vector correspond with the FFI type <code>(array 1 DWORD)</code>, the necessary semantics is achieved.
* The quasiquote expression <code>^#S(COORD X ,(+ 2 cinfo.srWindow.Left) Y ,(+ 5 cinfo.srWindow.Top))</code> is equivalent to <code>(new COORD X (+ 2 cinfo.srWindow.Left) Y (+ 5 cinfo.srWindow.Top))</code>. It is done this way to demonstrate support for structure quasiquoting.


## XPL0


```XPL0
include c:\cxpl\stdlib;
int C;
[Cursor(3, 6);                  \move cursor to column 3, row 6 (top left = 0,0)
\Call BIOS interrupt routine to read character (& attribute) at cursor position
C:= CallInt($10, $0800, 0) & $00FF; \mask off attribute, leaving the character
]
```


