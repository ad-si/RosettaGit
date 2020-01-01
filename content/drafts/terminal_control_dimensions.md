+++
title = "Terminal control/Dimensions"
description = ""
date = 2019-02-24T14:42:55Z
aliases = []
[extra]
id = 8423
[taxonomies]
categories = []
tags = []
+++

{{task|Terminal control}}[[Category:Initialization]]
Determine the height and width of the terminal, and store this information into variables for subsequent use.
[[Terminal Control::task| ]]


## Applesoft BASIC


```ApplesoftBasic
WIDTH = PEEK(33)
HEIGHT = PEEK(35) - PEEK(34)
```



## AutoHotkey

{{works with|AutoHotkey_L}}
{{trans|C}}
AutoHotkey is not built for the console (it is GUI oriented) so we must call the WinAPI directly.

```AHK
DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )

MsgBox Resize the console...

VarSetCapacity(csbi, 22) ; CONSOLE_SCREEN_BUFFER_INFO structure
DllCall("GetConsoleScreenBufferInfo", UPtr, hConsole, UPtr, &csbi)
Left   := NumGet(csbi, 10, "short")
Top    := NumGet(csbi, 12, "short")
Right  := NumGet(csbi, 14, "short")
Bottom := NumGet(csbi, 16, "short")

columns	:= right - left + 1
rows	:= bottom - top + 1
MsgBox %columns% columns and %rows% rows
```



## Axe

Since Axe currently only supports the TI-83/84, the home screen dimensions are fixed at 16 columns by 8 rows.


## Batch File

'''Screen Buffer Size:'''

```dos
@echo off

for /f "tokens=1,2 delims= " %%A in ('mode con') do (
	if "%%A"=="Lines:" set line=%%B
	if "%%A"=="Columns:" set cols=%%B
)

echo Lines: %line%
echo Columns: %cols%
exit /b 0
```

{{Out}}

```txt
>Size.Bat
Lines: 300
Columns: 80

>
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      dx% = @vdu.tr%-@vdu.tl% : REM Width of text viewport in pixels
      dy% = @vdu.tb%-@vdu.tt% : REM Height of text viewport in pixels
```



## C

C provides no standard way to find the size of a terminal.

=== {{libheader|BSD libc}} ===
[[BSD]] systems (and some other [[Unix]] clones) have TIOCGWINSZ. This ioctl(2) call gets the "window size" of a tty(4) device.

Almost all terminal devices can do NAWS (Negotiate About Window Size). A terminal emulator like xterm(1) should set the size. A network server like sshd(1) should copy the size from its client. Other devices, such as plain serial ports, might not know the window size.

{{works with|BSD|4.4}}

```c
#include <sys/ioctl.h>  /* ioctl, TIOCGWINSZ */
#include <err.h>	/* err */
#include <fcntl.h>	/* open */
#include <stdio.h>	/* printf */
#include <unistd.h>	/* close */

int
main()
{
	struct winsize ws;
	int fd;

	/* Open the controlling terminal. */
	fd = open("/dev/tty", O_RDWR);
	if (fd < 0)
		err(1, "/dev/tty");

	/* Get window size of terminal. */
	if (ioctl(fd, TIOCGWINSZ, &ws) < 0)
		err(1, "/dev/tty");

	printf("%d rows by %d columns\n", ws.ws_row, ws.ws_col);
	printf("(%d by %d pixels)\n", ws.ws_xpixel, ws.ws_ypixel);

	close(fd);
	return 0;
}
```


=== [[Windows]] ===
Grab a console screen handle, then call <code>GetConsoleScreenBufferInfo()</code> to get the information. Most consoles have a scroll bar and hold hundreds of lines, but the window shows only 25 or 50 lines. Use the window coordinates to calculate the window size.

{{works with|MinGW}}

```c
#include <windows.h>
#include <wchar.h>

int
main()
{
	HANDLE console;
	CONSOLE_SCREEN_BUFFER_INFO info;
	short rows;
	short columns;
	/* Create a handle to the console screen. */
	console = CreateFileW(L"CONOUT$", GENERIC_READ | GENERIC_WRITE,
	    FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
	    0, NULL);
	if (console == INVALID_HANDLE_VALUE)
		return 1;

	/* Calculate the size of the console window. */
	if (GetConsoleScreenBufferInfo(console, &info) == 0)
		return 1;
	CloseHandle(console);
	columns = info.srWindow.Right - info.srWindow.Left + 1;
	rows = info.srWindow.Bottom - info.srWindow.Top + 1;

	wprintf(L"%d columns by %d rows\n", columns, rows);

	return 0;
}
```


=={{header|C sharp|C#}}==
The C# console has several properties to take note of.
BufferHeight and BufferWidth are the valid writing area, the cursor can print anywhere within these bounds. This can be considered the actual terminal.
WindowHeight and WindowWidth are simply the size of the window, it only represents the active viewing area which may be larger or more commonly smaller than the size of the buffer.


```csharp

static void Main(string[] args)
{
    int bufferHeight = Console.BufferHeight;
    int bufferWidth = Console.BufferWidth;
    int windowHeight = Console.WindowHeight;
    int windowWidth = Console.WindowWidth;

    Console.Write("Buffer Height: ");
    Console.WriteLine(bufferHeight);
    Console.Write("Buffer Width: ");
    Console.WriteLine(bufferWidth);
    Console.Write("Window Height: ");
    Console.WriteLine(windowHeight);
    Console.Write("Window Width: ");
    Console.WriteLine(windowWidth);
    Console.ReadLine();
}

```


On the author's system this results in the following output:

```txt

Buffer Height: 300
Buffer Width: 80
Window Height: 25
Window Width: 80

```

This perfectly demonstrates that the buffer may not be the same size as the window.


## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. terminal-dimensions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  num-lines PIC 9(3).
       01  num-cols  PIC 9(3).

       SCREEN SECTION.
       01  display-screen.
           03  LINE 01 COL 01 PIC 9(3) FROM num-lines.
           03  LINE 01 COL 05 VALUE "rows by " .
           03  LINE 01 COL 13 PIC 9(3) FROM num-cols.
           03  LINE 01 COL 16 VALUE " columns.".

       PROCEDURE DIVISION.
           ACCEPT num-lines FROM LINES
           ACCEPT num-cols FROM COLUMNS

           DISPLAY display-screen

      *    This pauses the program, as ncurses will immediately revert
      *    back to the console when the program ends.
           CALL "C$SLEEP" USING BY CONTENT 3

           GOBACK
           .
```



## Euphoria


```Euphoria
include graphics.e

sequence vc
integer term_height, term_width

vc = video_config()

term_height = vc[VC_LINES]
term_width  = vc[VC_COLUMNS]

printf(1,"Terminal height is %d\n",term_height)
printf(1,"Terminal width is %d\n",term_width)
```


=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System

let bufferHeight = Console.BufferHeight
let bufferWidth = Console.BufferWidth
let windowHeight = Console.WindowHeight
let windowWidth = Console.WindowWidth

Console.Write("Buffer Height: ")
Console.WriteLine(bufferHeight)
Console.Write("Buffer Width: ")
Console.WriteLine(bufferWidth)
Console.Write("Window Height: ")
Console.WriteLine(windowHeight)
Console.Write("Window Width: ")
Console.WriteLine(windowWidth)
Console.ReadLine()
```



## Forth

{{works with|GNU Forth}} {{works with|SwiftForth}}


```forth
variable term-width
variable term-height

s" gforth" environment? [if]
  2drop form ( height width )
[else]  \ SwiftForth
  get-size ( width height ) swap
[then]
term-width ! term-height !
```



## Go

===Sub-repository===
{{libheader|Go sub-repositories}}

```go
package main

import (
    "fmt"
    "os"

    "golang.org/x/crypto/ssh/terminal"
)

func main() {
    w, h, err := terminal.GetSize(int(os.Stdout.Fd()))
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println(h, w)
}
```



### External command


```go
package main

import (
    "fmt"
    "os"
    "os/exec"
)

func main() {
    var h, w int
    cmd := exec.Command("stty", "size")
    cmd.Stdin = os.Stdin
    d, _ := cmd.Output()
    fmt.Sscan(string(d), &h, &w)
    fmt.Println(h, w)
}
```


### Ncurses

{{libheader|curses}}

```go
package main

import (
    "fmt"
    "log"

    "code.google.com/p/goncurses"
)

func main() {
    s, err := goncurses.Init()
    if err != nil {
        log.Fatal("init:", err)
    }
    defer goncurses.End()
    height, width := s.MaxYX()
    fmt.Println(height, width)
}
```



## Nim



```nim
import terminal

echo "Terminal width: " & $terminalWidth()
echo "Terminal height: " & $terminalHeight()

```



## J


This is not well supported in J, but since the terminal window can be resized at any time and can have its font changed and so on, good design generally dictates that this kind of information be ignored.

Nevertheless, assuming J version 6 in its usual environment, to determine its width and height, in pixels, you can use:


```j
_2 {.qsmsize_jijs_''
```


Note also that this will typically include 37 extra pixels horizontally and 79 extra pixels vertically, which are not available to display text.  In other words, if the result was 700 500 you would really have 663 pixels of width and 421 pixels of height.



## Julia


```julia

julia> using Gtk

julia> screen_size()
(3840, 1080)

julia>

```



## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
// version 1.1.2

/*
    I needed to execute the terminal command: 'export COLUMNS LINES'
    before running this program for it to work (returned 'null' sizes otherwise).
*/

fun main(args: Array<String>) {
    val lines = System.getenv("LINES")
    val columns = System.getenv("COLUMNS")
    println("Lines   = $lines")
    println("Columns = $columns")
}
```


{{out}}

```txt

Lines   = 24
Columns = 80

```



## Locomotive Basic


Locomotive BASIC has no built-in command to get window dimensions, but there is a firmware call to &bb69 (TXT_GET_WINDOW) for this. So we have to use a snippet of Z80 machine code to call the firmware and copy the results from the DE and HL registers to RAM. It looks like this when disassembled:


```z80
4000 d5          push de
4001 e5          push hl
4002 cd 69 bb    call &bb69
4005 ed 53 20 40 ld (&4020),de
4009 22 22 40    ld (&4022),hl
400c e1          pop hl
400d d1          pop de
400e c9          ret
```


This routine gets POKEd into RAM (starting at address &4000) and CALLed from Locomotive BASIC, then the results are retrieved with PEEK:


```locobasic
10 s=&4000:SYMBOL AFTER 256:MEMORY s-1
20 FOR i=0 to 14:READ a:POKE s+i,a:NEXT
30 DATA &d5,&e5,&cd,&69,&bb,&ed,&53,&20,&40,&22,&22,&40,&e1,&d1,&c9
40 CALL s
50 h=PEEK(&4020)-PEEK(&4022)+1
60 w=PEEK(&4021)-PEEK(&4023)+1
70 PRINT "window width:"; w; ", height:"; h
```


In practice, one would prefer to write the machine code routine as a slightly more elaborate RSX ('''r'''esident '''s'''ystem e'''x'''tension) which is a freely relocatable and therefore more reusable Locomotive BASIC extension. The RSX routine might be called "getwh" and accept pointers to integers, which would simplify the BASIC code to:


```locobasic
10 w%=0:h%=0      ' initialize and force integer type
20 |getwh,@w%,@h% ' call RSX and pass variables as pointers
30 PRINT "window width:"; w%; ", height:"; h%
```



## Mathematica


```Mathematica
WIDTH=RunThrough["tput cols", ""];
HEIGHT=RunThrough["tput lines", ""];
```



## OCaml


Using the library [http://forge.ocamlcore.org/projects/ansiterminal/ ANSITerminal] in the interactive loop:


```ocaml
$ ocaml unix.cma -I +ANSITerminal ANSITerminal.cma

# let width, height = ANSITerminal.size () ;;
val width : int = 126
val height : int = 47
```



## Perl



```perl
use Term::Size;

($cols, $rows) = Term::Size::chars;
print "The terminal has $cols columns and $rows lines\n";
```



## Perl 6

Using <i>stty</i> just for the heck of it.

```perl6
my $stty = qx[stty -a];
my $lines = $stty.match(/ 'rows '    <( \d+/);
my $cols  = $stty.match(/ 'columns ' <( \d+/);
say "$lines $cols";
```


## Phix

The buffer is usually somewhat larger (and never smaller) than the current physical screen size. I would guess that most applications are more interested in the latter.

```Phix
sequence vc = video_config()
printf(1,"Terminal buffer height is %d\n",vc[VC_LINES])
printf(1,"Terminal buffer width is %d\n",vc[VC_COLUMNS])
printf(1,"Terminal screen height is %d\n",vc[VC_SCRNLINES])
printf(1,"Terminal screen width is %d\n",vc[VC_SCRNCOLS])
```

{{out}}

```txt

Terminal buffer height is 196
Terminal buffer width is 132
Terminal screen height is 25
Terminal screen width is 80

```



## PicoLisp


```PicoLisp
(setq
   Width (in '(tput cols) (read))
   Height (in '(tput lines) (read)) )
```



## PureBasic

PureBasic does not have native functions for reading the size of this window, but supports API-functions that allows this.

This code is for Windows only.

```PureBasic
Macro ConsoleHandle()
  GetStdHandle_( #STD_OUTPUT_HANDLE )
EndMacro

Procedure ConsoleWidth()
  Protected CBI.CONSOLE_SCREEN_BUFFER_INFO
  Protected hConsole = ConsoleHandle()
  GetConsoleScreenBufferInfo_( hConsole, @CBI )
  ProcedureReturn CBI\srWindow\right - CBI\srWindow\left + 1
EndProcedure

Procedure ConsoleHeight()
  Protected CBI.CONSOLE_SCREEN_BUFFER_INFO
  Protected hConsole = ConsoleHandle()
  GetConsoleScreenBufferInfo_( hConsole, @CBI )
  ProcedureReturn CBI\srWindow\bottom - CBI\srWindow\top + 1
EndProcedure

If OpenConsole()
  x$=Str(ConsoleWidth())
  y$=Str(ConsoleHeight())
  PrintN("This window is "+x$+"x"+y$+ " chars.")
  ;
  Print(#CRLF$+"Press ENTER to exit"):Input()
EndIf
```



## Python

{{works with|Python|2.6}}
{{libheader|ctypes}}

This uses the [http://python.net/crew/theller/ctypes/ ctypes library] in order to get the console dimensions on Windows. This code is a slight refactoring of an [http://code.activestate.com/recipes/440694-determine-size-of-console-window-on-windows/ ActiveState Recipe]. For Linux, the tput utility is used.


```python
import os

def get_windows_terminal():
    from ctypes import windll, create_string_buffer
    h = windll.kernel32.GetStdHandle(-12)
    csbi = create_string_buffer(22)
    res = windll.kernel32.GetConsoleScreenBufferInfo(h, csbi)

    #return default size if actual size can't be determined
    if not res: return 80, 25

    import struct
    (bufx, bufy, curx, cury, wattr, left, top, right, bottom, maxx, maxy)\
    = struct.unpack("hhhhHhhhhhh", csbi.raw)
    width = right - left + 1
    height = bottom - top + 1

    return width, height

def get_linux_terminal():
    width = os.popen('tput cols', 'r').readline()
    height = os.popen('tput lines', 'r').readline()

    return int(width), int(height)

print get_linux_terminal() if os.name == 'posix' else get_windows_terminal()

```



## Racket


```racket

#lang racket
(require (planet neil/charterm:3:0))
(with-charterm
 (charterm-screen-size))

```



## Retro

This information is provided by Retro in the '''ch''' (height) and '''cw''' (width) variables. You can manually obtain it using the io ports.


```Retro
-3 5 out wait 5 in !cw
-4 5 out wait 5 in !ch
```



## REXX


### Using TPUT under Linux/Unix

{{works with|brexx}}
{{works with|regina}}
{{works with|rexximc}}

Some REXX interpreters don't provide basic [[terminal control]] as part of the language. However, it's possible to determine the size of the terminal window by using external system commands:

```rexx
width = 'tput'( 'cols' )
height = 'tput'( 'lines' )

say 'The terminal is' width 'characters wide'
say 'and has' height 'lines'
```



### LINESIZE

The <code>LINESIZE</code> built-in function returns the (terminal) screen's width.   It is supported by most (classic) REXX interpreters (and some others) such as: CMS REXX, TSO REXX, VSE REXX, the IBM REXX compiler, PC/REXX, Personal REXX, REXX/imc, R4 and ROO.   A sample usage of it is:

```rexx
width=linesize()
```

The above example makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here ──► [[LINESIZE.REX]].


### SCRSIZE

<code>SCRSIZE</code> is another built-in function, and returns two integers: the screen depth and the screen width.   A few classic REXX interpreters support it:   PC/REXX, Personal REXX, R4 and ROO.

```rexx
 parse value scrsize() with sd sw
```

The above example makes use of   '''SCRSIZE'''   REXX program (of BIF) which is used to determine the screen size of the terminal (console).

The   '''SCRSIZE.REX'''   REXX program is included here ──► [[SCRSIZE.REX]].



## Ring


```ring

system("mode 50,20")

```



## Ruby


```ruby
def winsize
  # Ruby 1.9.3 added 'io/console' to the standard library.
  require 'io/console'
  IO.console.winsize
rescue LoadError
  # This works with older Ruby, but only with systems
  # that have a tput(1) command, such as Unix clones.
  [Integer(`tput li`), Integer(`tput co`)]
end

rows, cols = winsize
printf "%d rows by %d columns\n", rows, cols
```


==={{libheader|curses}}===
<code>Curses.lines</code> and <code>Curses.cols</code> return the size of the terminal. The program ''must'' call <code>Curses.init_screen</code>, because without this call, Curses might report 0 lines and 0 columns. Beware that <code>Curses.init_screen</code> also switches the terminal to screen-oriented mode, and fails on those terminals that cannot support curses.


```ruby
require 'curses'

begin
  Curses.init_screen

  r, c = Curses.lines, Curses.cols

  Curses.setpos r / 2, 0
  Curses.addstr "#{r} rows by #{c} columns".center(c)
  Curses.getch
ensure
  Curses.close_screen
end
```



## Scala

{{Works with|Ubuntu|14.04}}

```Scala
  /*
     First execute the terminal command: 'export COLUMNS LINES'
     before running this program for it to work (returned 'null' sizes otherwise).
 */

    val (lines, columns)  = (System.getenv("LINES"), System.getenv("COLUMNS"))
    println(s"Lines   = $lines, Columns = $columns")
```


## Seed7

The functions [http://seed7.sourceforge.net/libraries/console.htm#height%28ref_console_file%29 height] and
[http://seed7.sourceforge.net/libraries/console.htm#width%28ref_console_file%29 width] are portable and
determine the dimensions of the [http://seed7.sourceforge.net/libraries/console.htm console window].
''Height'' and ''width'' are based on terminfo respectively the Windows console API.


```seed7
$ include "seed7_05.s7i";
  include "console.s7i";

const proc: main is func
  local
    var text: console is STD_NULL;
  begin
    console := open(CONSOLE);
    writeln(console, "height: " <& height(console) lpad 3);
    writeln(console, "width:  " <& width(console) lpad 3);
    # Terminal windows often restore the previous
    # content, when a program is terminated. Therefore
    # the program waits until Return/Enter is pressed.
    readln;
  end func;
```



## Sidef

{{trans|Perl 6}}

```ruby
var stty = `stty -a`;
var lines = stty.match(/\brows\h+(\d+)/);
var cols  = stty.match(/\bcolumns\h+(\d+)/);
say "#{lines} #{cols}";
```

{{out}}

```txt

24 80

```



## Tcl

{{trans|UNIX Shell}}

```tcl
set width [exec tput cols]
set height [exec tput lines]
puts "The terminal is $width characters wide and has $height lines"
```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
#!/bin/sh
WIDTH=`tput cols`
HEIGHT=`tput lines`
echo "The terminal is $WIDTH characters wide and has $HEIGHT lines."
```


==={{libheader|termcap}}===
<div style="background-color: #ffc;">termcap is obsolete.</div>

```bash
#!/bin/sh
WIDTH=`tput co`
HEIGHT=`tput li`
echo "The terminal is $WIDTH characters wide and has $HEIGHT lines."
```


=
## C Shell
=

```csh
#!/bin/csh -f
set WIDTH=`tput cols`
set HEIGHT=`tput lines`
echo "The terminal is $WIDTH characters wide and has $HEIGHT lines."
```



## Visual Basic

{{trans|C#}}

```vb
Module Module1

    Sub Main()
        Dim bufferHeight = Console.BufferHeight
        Dim bufferWidth = Console.BufferWidth
        Dim windowHeight = Console.WindowHeight
        Dim windowWidth = Console.WindowWidth

        Console.Write("Buffer Height: ")
        Console.WriteLine(bufferHeight)
        Console.Write("Buffer Width: ")
        Console.WriteLine(bufferWidth)
        Console.Write("Window Height: ")
        Console.WriteLine(windowHeight)
        Console.Write("Window Width: ")
        Console.WriteLine(windowWidth)
    End Sub

End Module
```

{{Out}}
I put the built application in Desktop:

```txt
\Desktop>ConsoleApplication1
Buffer Height: 300
Buffer Width: 80
Window Height: 25
Window Width: 80

\Desktop>
```

After resizing the console:

```txt
\Desktop>ConsoleApplication1
Buffer Height: 300
Buffer Width: 80
Window Height: 14
Window Width: 49

\Desktop>
```



## XPL0


```XPL0
include c:\cxpl\codes;
int W, H;
[W:= Peek($40, $4A);            \IBM-PC BIOS data
 H:= Peek($40, $84) + 1;
Text(0, "Terminal width and height = ");
IntOut(0, W);  ChOut(0, ^x);  IntOut(0, H);
]
```


Output:

```txt

80x25

```



## zkl

Unix specific solution:
{{trans|GO}}

```zkl
h,w:=System.popen("stty size","r").readln().split();
println(w," x ",h);
```

{{out}}
```txt
91 x 24
```


{{omit from|ACL2}}
{{omit from|GUISS}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|TI-83 BASIC|The terminal dimensions are constant.}}
