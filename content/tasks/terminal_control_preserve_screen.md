+++
title = "Terminal control/Preserve screen"
description = ""
date = 2019-01-30T11:48:31Z
aliases = []
[extra]
id = 9380
[taxonomies]
categories = ["task", "Terminal control"]
tags = []
+++

[[Terminal Control::task| ]]


## Task

Clear the screen, output something on the display, and then restore the screen to the preserved state that it was in before the task was carried out.

There is no requirement to change the font or kerning in this task, however character decorations and attributes are expected to be preserved.   If the implementer decides to change the font or kerning during the display of the temporary screen, then these settings need to be restored prior to exit.





## Applesoft BASIC

Restores 40 x 24 TEXT screen, cursor position, display mode, and speed.  Adjusts HIMEM to make room to store 1024 bytes aligned to the 256 byte page boundary.  POKEs a machine language "copy 4 pages of memory" routine into page 3.

```ApplesoftBasic
 10  LET FF = 255:FE = FF - 1
 11  LET FD = 253:FC = FD - 1
 12  POKE FC, 0 : POKE FE, 0
 13  LET R = 768:H =  PEEK (116)
 14  IF  PEEK (R) = 162 GOTO 40

 15  LET L = PEEK (115) > 0
 16  LET H = H - 4 - L
 17  HIMEM:H*256
 18  LET A = 10:B = 11:C = 12
 19  LET D = 13:E = 14:Z = 256
 20  POKE R + 0,162: REMLDX
 21  POKE R + 1,004: REM #$04
 22  POKE R + 2,160: REMLDY
 23  POKE R + 3,000: REM #$00
 24  LET L = R + 4: REMLOOP
 25  POKE L + 0,177: REMLDA
 26  POKE L + 1,FC:: REM($FC),Y
 27  POKE L + 2,145: REMSTA
 28  POKE L + 3,FE:: REM($FE),Y
 29  POKE L + 4,200: REMINY
 30  POKE L + 5,208: REMBNE
 31  POKE L + 6,Z - 7: REMLOOP
 32  POKE L + 7,230: REMINC
 33  POKE L + 8,FD:: REM  $FD
 34  POKE L + 9,230: REMINC
 35  POKE L + A,FF:: REM  $FF
 36  POKE L + B,202: REMDEX
 37  POKE L + C,208: REMBNE
 38  POKE L + D,Z - E: REMLOOP
 39  POKE L + E,096: REMRTS

 40  POKE FD, 4 : POKE FF, H
 41  CALL R : S = PEEK(241)
 42  LET V = PEEK(37)
 43  LET C = PEEK(36)
 44  LET M = PEEK(50)
 45  LET F = PEEK(243)

 50  HOME : INVERSE
 51  PRINT "ALTERNATE BUFFER"
 52  FLASH : SPEED = 125
 53  FOR I = 5 TO 1 STEP -1
 54      PRINT "GOING BACK IN: "I
 55  NEXT I

 60  POKE FD, H : POKE FF, 4
 61  CALL R : POKE 241, S
 62  VTAB V + 1 : HTAB C + 1
 63  POKE 50, M : POKE 243, F
```



## BBC BASIC

The screen is saved as a bitmap:

```bbcbasic
      PRINT "This is the original screen"
      OSCLI "GSAVE """ + @tmp$ + "bbcsave"""
      WAIT 200
      CLS
      PRINT "This is the new screen, following a CLS"
      WAIT 200
      OSCLI "DISPLAY """ + @tmp$ + "bbcsave"""
```



## Befunge


Assuming a terminal with support for Xterm's [http://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-The-Alternate-Screen-Buffer ''alternate screen buffer''] escape sequences (which I believe is fairly standard these days), this example will switch to the alternate screen buffer, output "Press <Enter> to restore..." in the top left corner, and then restore the original screen.


```befunge
"h9401?["39*>:#,_"...erotser ot >retnE< sserPH["39*>:#,_~$"l9401?["39*>:#,_@
```



## C

For Xterm.  "Allow alternate screen buffer" must be enabled by the popup menu.
```c
#include <stdio.h>
#include <unistd.h>

int main()
{
	int i;
	printf("\033[?1049h\033[H");
	printf("Alternate screen buffer\n");
	for (i = 5; i; i--) {
		printf("\rgoing back in %d...", i);
		fflush(stdout);
		sleep(1);
	}
	printf("\033[?1049l");

	return 0;
}
```


## Common Lisp

```lisp

(format t "~C[?1049h~C[H" (code-char #O33) (code-char #O33))
(format t "Alternate screen buffer~%")
(loop for i from 5 downto 1 do (progn
                             (format t "~%going back in ~a" i)
                             (sleep 1)
                             ))
(format t "~C[?1049l" (code-char #O33))

```



## Emacs Lisp



```lisp
#!/usr/local/bin/emacs --script
;; -*- lexical-binding: t; -*-

;; "ESC [ ? 1049 h" - Enable alternative screen buffer
(princ "\033[?1049h")
(princ "Alternate screen buffer\n")

(let ((i 5))
  (while (> i 0)
    (princ (format "\rgoing back in %d..." i))
    ;; flush stdout
    (set-binary-mode 'stdout t)
    (sleep-for 1)
    (setq i (1- i))))

;; "ESC [ ? 1049 l" - Disable alternative screen buffer
(princ "\033[?1049l")
```



## Go

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    fmt.Print("\033[?1049h\033[H")
    fmt.Println("Alternate screen buffer\n")
    s := "s"
    for i := 5; i > 0; i-- {
        if i == 1 {
            s = ""
        }
        fmt.Printf("\rgoing back in %d second%s...", i, s)
        time.Sleep(time.Second)
    }
    fmt.Print("\033[?1049l")
}
```



## Java

```java
public class PreserveScreen
{
    public static void main(String[] args) throws InterruptedException {
        System.out.print("\033[?1049h\033[H");
        System.out.println("Alternate screen buffer\n");
        for (int i = 5; i > 0; i--) {
            String s = (i > 1) ? "s" : "";
            System.out.printf("\rgoing back in %d second%s...", i, s);
            Thread.sleep(1000);
        }
        System.out.print("\033[?1049l");
    }
}
```



## JavaScript


```javascript
(function() {
	var orig= document.body.innerHTML
	document.body.innerHTML= '';
	setTimeout(function() {
		document.body.innerHTML= 'something';
		setTimeout(function() {
			document.body.innerHTML= orig;
		}, 1000);
	}, 1000);
})();
```


This implementation assumes that Javascript is running in the browser.

This task does not admit sample output, but you can demonstrate this solution for yourself using the chrome browser:  control-shift-J then copy and paste the above into the command line, and hit enter.


## Julia

```julia
const ESC = "\u001B" # escape code

print("$ESC[?1049h$ESC[H")
print("\n\nNow using an alternate screen buffer. Returning after count of: ")
foreach(x -> (sleep(1); print("  $x")), 5:-1:0)
print("$ESC[?1049l\n\n\n")


```



## Kotlin

```scala
// version 1.1.2

const val ESC = "\u001B"

fun main(args: Array<String>) {
    print("$ESC[?1049h$ESC[H")
    println("Alternate screen buffer")
    for(i in 5 downTo 1) {
        print("\rGoing back in $i second${if (i != 1) "s" else ""}...")
        Thread.sleep(1000)
    }
    print("$ESC[?1049l")
}
```


## M2000 Interpreter

M2000 Console can used for graphics also. Here is a small example how we can preserve attributes. We use Hold to save temporary console bitmap, and Release to restore old console bitmap. These statements used for animation too.
We can set the refresh rate using refresh statement, so we can make drawings before next refresh.

If we change Mode (size of font), or Window size (console witdh/height), or use a Form statement to set character resolution (number characters in a row by row number) which automatic calculate size and line spacing, then saved consoled bitmap erased. To preserve screen from this situation we have to preserve last form's arguments, or window's arguments or mode's argument.


```M2000 Interpreter

Module PreserveScreen {
      Bold 1
      Font "Arial"
      Paper=#225511
      SplitScreenRow=0
      Cls Paper, SplitScreenRow
      Print "Test"
      Gosub GetState
      Font "Tahoma"
      Bold 1 : Italic 1: Pen 15
      cls 0, 5
      For i=1 to 100 : Print i: Next i
      Move 6000,6000
      For i=1000 to 6000 step 1000 : Circle i : Next i
      WaitKey$=Key$
      Gosub RestoreState
      Print "End"
      End

      GetState:
            prevfont$=fontname$
            prevbold=bold
            previtalic=italic
            prevpen=pen
            posx=pos
            posy=row
            graphicx=pos.x
            graphicy=pos.y
            OldPaper=Paper
            OldSplit=SplitScreenRow
            Hold
      Return
      RestoreState:
            Paper=OldPaper
            SplitScreenRow=OldSplit
            Cls Paper, SplitScreenRow
            Release
            font prevfont$
            bold prevbold
            italic previtalic
            pen prevpen
            cursor posx, posy
            move graphicx, graphicy
      Return
}
PreserveScreen


```




## Mathematica


```Mathematica
Run["tput smcup"]    (* Save the display *)
Run["echo Hello"]
Pause[5]       (* Wait five seconds *)
Run["tput rmcup"]
```



## Nim

```nim
import os

echo "\e[?1049h\e[H"
echo "Alternate buffer!"

for i in countdown(5, 1):
  echo "Going back in: ", i
  sleep 1000

echo "\e[?1049l"
```



## Perl

```perl
print "\033[?1049h\033[H";
print "Alternate screen buffer\n";

for (my $i = 5; $i > 0; --$i) {
    print "going back in $i...\n";
    sleep(1);
}

print "\033[?1049l";
```



## Perl 6


```perl6
print "\e[?1049h\e[H";
say "Alternate buffer!";

for 5,4...1 {
    print "\rGoing back in: $_";
    sleep 1;
}

print "\e[?1049l";
```



## Phix


```Phix
sequence s = save_text_image({1,1}, {25,80})
clear_screen()
puts(1,"\n\n *** hello ***\n")
sleep(5)
display_text_image({1,1}, s)
sleep(3)
```

The following also works fine on linux (but not windows)

```Phix
puts(1,"\e[?1049h\e[H")
puts(1,"Alternate buffer!\n")

for i=5 to 0 by -1 do
    printf(1,"Going back in:%d\r", i)
    sleep(1)
end for
puts(1,"\e[?1049l")
```



## PicoLisp


```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(call 'tput "smcup")
(prinl "something")
(wait 3000)
(call 'tput "rmcup")

(bye)
```



## Python

Similar to the C example above:


```Python
#!/usr/bin/env python

import time

print "\033[?1049h\033[H"
print "Alternate buffer!"

for i in xrange(5, 0, -1):
    print "Going back in:", i
    time.sleep(1)

print "\033[?1049l"
```



## Racket


```Racket

#lang racket

(require racket/system)
(define (flash str)
  (system "tput smcup")
  (displayln str)
  (sleep 2)
  (system "tput rmcup")
  (void))

(flash "Hello world.")

```



## REXX

This version <u>only</u> works with PC/REXX and Personal REXX.

The   '''CLS'''   (DOS) command is used to clear the terminal screen.

```rexx
/*REXX program saves the screen contents and also the cursor location,  then clears the */
/*──── screen, writes a half screen of ~~~ lines, and then restores the original screen.*/

parse value  scrsize()   with  sd  sw  .         /*determine the size of terminal screen*/
parse value cursor(1,1)  with  curRow  curCol .  /*also, find the location of the cursor*/

          do original=1  for sd                  /*obtain the original screen contents. */
          @line.original=scrRead(original,1, sw) /*obtain a line of the terminal screen.*/
          end   /*original*/                     /* [↑]  obtains  SD  number of lines.  */
'CLS'                                            /*start with a clean slate on terminal.*/
          do sd % 2                              /*write a line of ~~~ for half of scr. */
          say '~~~'                              /*writes ~~~ starting at top of screen.*/
          end   /*sd % 2*/                       /* [↑]  this shows ~~~ will be overlaid*/
                                                 /*no need to clear the screen here.    */
          do restore=1  for sd                   /*restore original screen from  @line. */
          call scrWrite restore,1, @line.restore /*write to terminal the original lines.*/
          end   /*restore*/                      /* [↑]  writes (restores)  SD  lines.  */
                                                 /*stick a fork in it,  we're all done. */
call cursor  curRow, curCol                      /*restore the original cursor position.*/
```

This REXX program makes use of   '''scrsize'''   BIF which is used to determine the screen size of the terminal (console).

For those REXXes that don't have the   '''scrsize'''   BIF, the   '''SCRSIZE.REX'''   REXX program is included here   ──►   [[SCRSIZE.REX]].




## Scala

Similar to the C example above:


```Scala
print("\033[?1049h\033[H")
println("Alternate buffer!")

for (i <- 5 to 0 by -1) {
    println(s"Going back in: $i")
    Thread.sleep(1000)
}

print("\033[?1049l")
```



## Sidef

```ruby
print "\e[?1049h\e[H";
say "Alternate buffer!";

3.downto(1).each { |i|
    say "Going back in: #{i}";
    Sys.sleep(1);
}

print "\e[?1049l";
```



## Tcl

On Unix terminals only, with the help of <tt>tput</tt>:

```tcl
# A helper to make code more readable
proc terminal {args} {
    exec /usr/bin/tput {*}$args >/dev/tty
}

# Save the screen with the "enter_ca_mode" capability, a.k.a. 'smcup'
terminal smcup
# Some indication to users what is happening...
puts "This is the top of a blank screen. Press Return/Enter to continue..."
gets stdin
# Restore the screen with the "exit_ca_mode" capability, a.k.a. 'rmcup'
terminal rmcup
```



## UNIX Shell


```sh
#!/bin/sh
tput smcup    # Save the display
echo 'Hello'
sleep 5       # Wait five seconds
tput rmcup    # Restore the display
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

proc SetPage(P);        \Select active display page for video screen
int  P;
int  CpuReg;
[CpuReg:= GetReg;       \access CPU registers
CpuReg(0):= $0500 + P;  \call BIOS interrupt $10, function 5
SoftInt($10);
]; \SetPage

[SetPage(1);            \enable page 1 text display screen
Clear;                  \clear screen and output something
Text(0, "Hit any key to restore original screen. ");
if ChIn(1) then [];     \wait for keystroke
SetPage(0);             \restore original, default text screen, page 0
]
```



## Z80 Assembly


Using the Amstrad CPC firmware:


```z80
		org	$3000

txt_output:	equ	$bb5a
scr_clear:	equ	$bc14
wait_char:	equ	$bb06
scr_get_loc:	equ	$bc0b
scr_set_off:	equ	$bc05

		push	bc
		push	de
		push	hl
		push	af

		call	scr_get_loc ; save this value just in case the
		push	hl          ;   original screen has been scrolled vertically

		ld	hl,$c000 ; copy screen to block 1
		ld	de,$4000
		ld	bc,$4000
		ldir

		call	scr_clear
		ld	hl,text

print:		ld	a,(hl)
		cp	0
		jr	z,key
		call	txt_output
		inc	hl
		jr	print

key:		call	wait_char
		pop	hl
		call	scr_set_off
		ld	hl,$4000 ; restore screen
		ld	de,$c000
		ld	bc,$4000
		ldir
		pop	af
		pop	hl
		pop	de
		pop	bc
		ret

text:		defm	"This is some text. Please press a key.\0"
```



## zkl

Works in a Mint Linux terminal, switching to the alternate screen buffer, printing a count down message and then switching back.

```zkl
print("\e[?1049h\e[H");
println("Alternate screen buffer");
foreach i in ([5..1,-1]){
   print("\rgoing back in %d...".fmt(i));
   Atomic.sleep(1);
}
print("\e[?1049l");
```



