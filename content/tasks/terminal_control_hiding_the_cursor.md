+++
title = "Terminal control/Hiding the cursor"
description = ""
date = 2019-01-30T06:57:07Z
aliases = []
[extra]
id = 8595
[taxonomies]
categories = ["task", "Terminal control"]
tags = []
+++

## Task

{{task|Terminal control}}[[Category:Terminal control]]

The task is to hide the cursor and show it again.


## AutoHotkey

Keep in mind that AHK is not built for the command line, so we must call the WinAPI directly.

```AHK
DllCall("AllocConsole") ; Create a console if not launched from one
hConsole := DllCall("GetStdHandle", UInt, STDOUT := -11)

VarSetCapacity(cci, 8)  ; CONSOLE_CURSOR_INFO structure
DllCall("GetConsoleCursorInfo", UPtr, hConsole, UPtr, &cci)
NumPut(0, cci, 4)
DllCall("SetConsoleCursorInfo", UPtr, hConsole, UPtr, &cci)

FileAppend, Cursor hidden for 3 seconds..., CONOUT$ ; Prints to stdout
Sleep 3000

NumPut(1, cci, 4)
DllCall("SetConsoleCursorInfo", UPtr, hConsole, UPtr, &cci)

FileAppend, `nCursor shown, CONOUT$
MsgBox
```



## BASIC

```qbasic
'hide the cursor:
LOCATE , , 0
'wait for a keypress...
SLEEP
'show the cursor:
LOCATE , , 1
```


=
## Applesoft BASIC
=

```ApplesoftBasic
WAIT 49152,128
```



## BBC BASIC


```bbcbasic
      OFF : REM Hide the cursor (caret)
      WAIT 400
      ON  : REM Show the cursor again
```



## Befunge

Assuming a terminal with support for ANSI escape sequences, this hides the cursor, waits for the user to press ''Enter'', and then shows the cursor again.

```befunge
"l52?["39*,,,,,,  >v
"retnE sserP">:#,_v>
"h52?["39*,,,,,,@ >~
```



## C


```c

/* Please note that curs_set is terminal dependent. */

#include<curses.h>
#include<stdio.h>

int
main ()
{
  printf
    ("At the end of this line you will see the cursor, process will sleep for 5 seconds.");
  napms (5000);
  curs_set (0);
  printf
    ("\nAt the end of this line you will NOT see the cursor, process will again sleep for 5 seconds.");
  napms (5000);
  printf ("\nGoodbye.");
  return 0;
}

```



## C++


```cpp

#include <Windows.h>
int main()
{
  bool showCursor = false;

  HANDLE std_out = GetStdHandle(STD_OUTPUT_HANDLE); // Get standard output
  CONSOLE_CURSOR_INFO cursorInfo;                   //
  GetConsoleCursorInfo(out, &cursorInfo);           // Get cursorinfo from output
  cursorInfo.bVisible = showCursor;                 // Set flag visible.
  SetConsoleCursorInfo(out, &cursorInfo);           // Apply changes
}

```


## C#
```c#
static void Main(string[] args)
{
    Console.Write("At the end of this line you will see the cursor, process will sleep for 5 seconds.");
    System.Threading.Thread.Sleep(5000);
    Console.CursorVisible = false;
    Console.WriteLine();
    Console.Write("At the end of this line you will not see the cursor, process will sleep for 5 seconds.");
    System.Threading.Thread.Sleep(5000);
}
```



## Common Lisp


```lisp

(defun sh (cmd)
  #+clisp (shell cmd)
  #+ecl (si:system cmd)
  #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*)
  #+clozure (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))

(defun show-cursor (x)
  (if x (sh "tput cvvis") (sh "tput civis")))

(show-cursor nil)
(sleep 3)
(show-cursor t)
(sleep 3)

```




## FunL


```funl
import time.*
import console.*

hide()
sleep( 2 Second )
show()
```



## Go


### External command


```go
package main

import (
    "os"
    "os/exec"
    "time"
)

func main() {
    tput("civis") // hide
    time.Sleep(3 * time.Second)
    tput("cvvis") // show
    time.Sleep(3 * time.Second)
}

func tput(arg string) error {
    cmd := exec.Command("tput", arg)
    cmd.Stdout = os.Stdout
    return cmd.Run()
}
```


### Escape code

(Not sure if this is ANSI, but it worked for me.)

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    fmt.Print("\033[?25l")
    time.Sleep(3 * time.Second)
    fmt.Print("\033[?25h")
    time.Sleep(3 * time.Second)
}
```


### Ncurses

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
    gc.Cursor(0)
    time.Sleep(3 * time.Second)
    gc.Cursor(1)
    s.GetChar()
}
```



## J

With the definitions of [[Terminal_control/Coloured_text#J]]

```J
smoutput HIDECURSOR
usleep(4e6) NB. wait 4 seconds
smoutput SHOWCURSOR

```



## Julia

```julia
const ESC = "\u001B" # escape code
print("$ESC[?25l")                       # hide the cursor
print("Enter anything, press RETURN: ")  # prompt shown
input = readline()                       # but no cursor
print("$ESC[0H$ESC[0J$ESC[?25h")         # reset, visible again
sleep(3)
println()

```



## Kotlin

```scala
// version 1.1.2

fun main(args: Array<String>) {
    print("\u001B[?25l")      // hide cursor
    Thread.sleep(2000)        // wait 2 seconds before redisplaying cursor
    print("\u001B[?25h")      // display cursor
    Thread.sleep(2000)        // wait 2 more seconds before exiting
}
```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(
	esc = decode_base64('Gw==')
)

// hide the cursor
stdout(#esc + '[?25l')

// wait for 4 seconds to give time discover the cursor is gone
sleep(4000)

// show the cursor
stdout(#esc + '[?25h')

// wait for 4 seconds to give time discover the cursor is back
sleep(4000)
```



## Locomotive Basic


```locobasic
10 CURSOR 0: REM hide cursor
20 FOR l = 1 TO 2000: REM delay
30 NEXT l
40 CURSOR 1: REM show cursor
```



## Mathematica


```Mathematica
Run["tput civis"]  (* Cursor hidden *)
Pause[2]
Run["tput cvvis"]  (* Cursor Visible *)
```


## Nemerle


```Nemerle
using System.Console;
using System.Threading.Thread;

module CursorVisibility
{
    Main() : void
    {
        repeat(3) {
            CursorVisible = !CursorVisible;
            Sleep(5000);
        }
    }
}
```



## Nim



```nim
import terminal

echo "Cursor hidden.  Press a key to show the cursor and exit."
stdout.hideCursor()
discard getCh()
stdout.showCursor()

```



## Perl


```perl
print "\e[?25l";                        # hide the cursor
print "Enter anything, press RETURN: "; # prompt shown
$input = <>;                            # but no cursor
print "\e[0H\e[0J\e[?25h";              # reset, visible again
```



## Perl 6


```perl6
say 'Hiding the cursor for 5 seconds...';
run 'tput', 'civis';
sleep 5;
run 'tput', 'cvvis';
```



## Phix


```Phix
cursor(NO_CURSOR)
sleep(1)
cursor(UNDERLINE_CURSOR)
```



## PicoLisp


```PicoLisp
(call "tput" "civis")  # Invisible
(wait 1000)
(call "tput" "cvvis")  # Visible
```



## PureBasic


```PureBasic
#cursorSize = 10 ;use a full sized cursor

If OpenConsole()
  Print("Press any key to toggle cursor: ")
  EnableGraphicalConsole(1)
  height = #cursorSize
  ConsoleCursor(height)
  Repeat
    If Inkey()
      height ! #cursorSize
      ConsoleCursor(height)
    EndIf
  ForEver
EndIf
```


Tested with <b>PB v5.60</b>


```PureBasic
Procedure HideCursor ()
 ConsoleCursor(0)
EndProcedure

Procedure ShowCursor (CursorHeight.b = 1)
 If CursorHeight > 10 : CursorHeight = 10 : EndIf
 If CursorHeight < 1  : CursorHeight = 1  : EndIf
 ConsoleCursor(CursorHeight)
EndProcedure

Procedure NL (NoL.b = 1)
 For i = 1 To NoL : PrintN("") : Next
EndProcedure

If OpenConsole()
 EnableGraphicalConsole(1)
 Print("   full cursor > ")
 ShowCursor(11) : Delay(4000) : NL()
 Print(" hidden cursor > ")
 HideCursor()   : Delay(4000) : NL()
 Print("minimal cursor > ")
 ShowCursor(-0.5)
 Print("press [Enter] to continue ... ") : Input()
EndIf
```




## Python


```python
import curses
import time

stdscr = curses.initscr()
curses.curs_set(1)  # visible
time.sleep(2)
curses.curs_set(0)  # invisible
time.sleep(2)
curses.curs_set(1)  # visible
time.sleep(2)
curses.endwin()

```



## Racket


```racket

#lang racket
(void (system "tput civis")) (sleep 2) (void (system "tput cvvis"))

```



## REXX

```rexx
/*REXX pgm calls a function in a shared library (regutil) to hide/show cursor.*/
z=rxfuncadd('sysloadfuncs', "regutil", 'sysloadfuncs')   /*add a function lib.*/
if z\==0  then do                                        /*test the return cod*/
               say 'return code'  z  "from rxfuncadd"    /*tell about bad RC. */
               exit z                                    /*exit this program. */
               end

call sysloadfuncs                                        /*load the functions.*/

                                       /* [↓]   call a particular function.   */
call syscurstate 'off'                 /*hide the displaying of the cursor.   */
say 'showing of the cursor is now off' /*inform that the cursor is now hidden.*/

                                       /* ··· and perform some stuff here ··· */
say 'sleeping for three seconds ...'   /*inform the user of what we're doing. */
call sleep 3                           /*might as well sleep for three seconds*/

call syscurstate 'on'                  /*(unhide) the displaying of the cursor*/
say 'showing of the cursor is now on'  /*inform that the cursor is now showing*/
                                       /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

showing of the cursor is now off
sleeping for three seconds ...
showing of the cursor is now on

```



## Ring


```ring

# Project : Terminal control/Hiding the cursor

load "stdlib.ring"
# Linux
? "Hide Cursor using tput utility"
system("tput civis")     # Invisible
sleep(10)
? "Show Cursor using tput utility"
system("tput cnorm")   # Normal

```



## Ruby


```ruby
require "curses"
include Curses

init_screen
begin
  curs_set(1) #visible cursor
  sleep 3
  curs_set(0) #invisible cursor
  sleep 3
  curs_set(1) #visible cursor
  sleep 3
ensure
  close_screen
end
```



## Scala


```scala
object Main extends App {
  print("\u001B[?25l") // hide cursor
  Thread.sleep(2000) // wait 2 seconds before redisplaying cursor
  print("\u001B[?25h") // display cursor
  Thread.sleep(2000) // wait 2 more seconds before exiting
}
```


## Tcl


```tcl
proc cursor {{state "normal"}} {
    switch -- $state {
	"normal"    {set op "cnorm"}
	"invisible" {set op "civis"}
	"visible"   {set op "cvvis"}
    }
    # Should be just: “exec tput $op” but it's not actually supported on my terminal...
    exec sh -c "tput $op || true"
}
```

Demonstration code:

```tcl
cursor normal
puts "normal cursor"
after 3000
cursor invisible
puts "invisible cursor"
after 3000
cursor visible
puts "very visible cursor"
after 3000
cursor normal
puts "back to normal"
after 1000
```



## UNIX Shell



```sh
tput civis    # Hide the cursor
sleep 5       # Sleep for 5 seconds
tput cnorm    # Show the cursor
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

proc ShowCur(On);       \Turn flashing cursor on or off
int On;                 \true = cursor on; false = cursor off
int CpuReg;
[CpuReg:= GetReg;       \access CPU registers
CpuReg(0):= $0100;      \AX:= $0100
CpuReg(2):= if On then $0007 else $2000;
SoftInt($10);           \Call BIOS interrupt $10
]; \ShowCur

[ShowCur(false);        \turn off flashing cursor
if ChIn(1) then [];     \wait for keystroke
ShowCur(true);          \turn on flashing cursor
]
```



## zkl

Hide the cursor for three seconds on an ANSI terminal

```zkl
print("\e[?25l");
Atomic.sleep(3);
print("\e[?25h");
```


