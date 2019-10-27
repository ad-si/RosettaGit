+++
title = "Terminal control/Inverse video"
description = ""
date = 2019-01-30T22:42:46Z
aliases = []
[extra]
id = 8597
[taxonomies]
categories = []
tags = []
+++

{{task|Terminal control}}[[Category:Terminal control]]
The task is to display a word in inverse video (or reverse video) followed by a word in normal video.


## 6502 Assembly
 
{{works with|http://vice-emu.sourceforge.net/ VICE}}
This example has been written for the C64 and uses the STROUT BASIC routine.
Compile with the [http://turbo.style64.org/ Turbo Macro Pro cross assembler]:

```txt

tmpx -i inverse-video.s -o inverse-video.prg

```

Run with:

```txt

SYS680

```


```6502asm
; C64 - Terminal control: Inverse Video

; *** labels ***

strout          = $ab1e

; *** main ***

                *=$02a8         ; sys 680
                
                lda #<str       ; Address of the message to print - low byte
                ldy #>str       ; Address high byte
                jsr strout      ; Print a null terminated string.
                rts    
                
; *** data ***

str             .byte $12       ; the REVERSE ON control code
                                ; see https://en.wikipedia.org/wiki/PETSCII
                .text "reversed"
                .byte $92       ; the REVERSE OFF control code
                .null " normal" ; null terminated string                

```



## Ada


```Ada
with Ada.Text_IO; use  Ada.Text_IO;

procedure Reverse_Video is 

   Rev_Video  : String := Ascii.ESC & "[7m";
   Norm_Video : String := Ascii.ESC & "[m";

begin
   Put (Rev_Video & "Reversed");
   Put (Norm_Video & " Normal");
end Reverse_Video;

```



## AutoHotkey

Call SetConsoleTextAttribute() to change foreground and background colors.

```AHK
DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )

SetConsoleTextAttribute(hConsole, 0x70) ; gray background, black foreground
FileAppend, Reversed`n, CONOUT$ ; print to stdout

SetConsoleTextAttribute(hConsole, 0x07) ; black background, gray foreground
FileAppend, Normal, CONOUT$

MsgBox

SetConsoleTextAttribute(hConsole, Attributes){
	return DllCall( "SetConsoleTextAttribute", UPtr, hConsole, UShort, Attributes)
}
```


## AWK


```awk
BEGIN {
system ("tput rev")
print "foo"
system ("tput sgr0")
print "bar"
}
```



## Axe

A delay is added because the screen redraws with the normal font after the program exits.


```axe
Fix 3
Disp "INVERTED"
Fix 2
Disp "REGULAR",i
Pause 4500
```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
INVERSE:?"ROSETTA";:NORMAL:?" CODE"
```


=
## BBC BASIC
=

```bbcbasic
      COLOUR 128        : REM Black background
      COLOUR 15         : REM White foreground
      PRINT "Inverse";
      COLOUR 128+15     : REM White background
      COLOUR 0          : REM Black foreground
      PRINT " video"
```

Alternative method using 'VDU code' strings:

```bbcbasic
      inverse$ = CHR$(17)+CHR$(128)+CHR$(17)+CHR$(15)
      normal$ = CHR$(17)+CHR$(128+15)+CHR$(17)+CHR$(0)
      PRINT inverse$ + "Inverse" + normal$ + " video"
```


=
## Locomotive Basic
=
The firmware routine at &bb9c (TXT INVERSE) swaps the current Locomotive BASIC PEN and PAPER colors:


```locobasic
10 CALL &bb9c:PRINT "inverse";
20 CALL &bb9c:PRINT "normal"
```


=
## PureBasic
=

```PureBasic
If OpenConsole()
  ConsoleColor(0, 15) ;use the colors black (background) and white (forground)
  PrintN("Inverse Video")
  ConsoleColor(15, 0) ;use the colors white (background) and black (forground)
  PrintN("Normal Video")
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


=
## Run BASIC
=

```runbasic
' ---------- foo is reverse --------------
x$ = shell$("tput mr
echo 'foo'")

' ---------- bar is normal --------------
x$ = shell$("tput me     
echo 'bar'")
wait
```


=
## Sinclair ZX81 BASIC
=
Inverse video is available from the keyboard (accessed with <code>SHIFT</code><code>9</code>), so the normal way to do this would be just

```basic
PRINT "FOOBAR"
```

but with the 'foo' in inverse video and the 'bar' in normal video.

If this won't work (say, if we may want to use inverse video with string variables rather than string literals), we can use a small subroutine—relying on the fact that the ZX81 character set uses the high bit of each character code to select normal or inverse video.

```basic
10 LET S$="FOO"
20 GOSUB 50
30 PRINT S$;"BAR"
40 STOP
50 FOR I=1 TO LEN S$
60 LET S$(I)=CHR$ (128+CODE S$(I))
70 NEXT I
80 RETURN
```

Note that this subroutine assumes the source string is not already in inverse video: if it could be, you will need to test each character before you attempt to convert it.

=== {{header|ZX Spectrum Basic}} ===

```basic
10 INVERSE 1
20 PRINT "FOO";
30 INVERSE 0
40 PRINT "BAR"
```



## Befunge

Assuming a terminal with support for ANSI escape sequences.

```befunge
0"lamroNm["39*"esrevnIm7["39*>:#,_$@
```



## C


```C>#include <stdio.h


int main()
{
	printf("\033[7mReversed\033[m Normal\n");

	return 0;
}
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. terminal-reverse-video.

       PROCEDURE DIVISION.
           DISPLAY "Reverse-Video" WITH REVERSE-VIDEO
           DISPLAY "Normal"

           GOBACK
           .
```



## FunL


```funl
import console.*

println( "${REVERSED}This is reversed.$RESET This is normal." )
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
    tput("rev")
    fmt.Print("Rosetta")
    tput("sgr0")
    fmt.Println(" Code")
}

func tput(arg string) error {
    cmd := exec.Command("tput", arg)
    cmd.Stdout = os.Stdout
    return cmd.Run()
}
```


### ANSI escape codes


```go
package main

import "fmt"

func main() {
    fmt.Println("\033[7mRosetta\033[m Code")
}
```


### Ncurses

{{libheader|Curses}}

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
    s.AttrOn(gc.A_REVERSE)
    s.Print("Rosetta")
    s.AttrOff(gc.A_REVERSE)
    s.Println(" Code")
    s.GetChar()
}
```



## J

Use the definitions given in [[Terminal_control/Coloured_text#J]]

```J

   ;:';:,#.*."3,(C.A.)/\/&.:;:' NB. some output beforehand
   attributes REVERSEVIDEO      NB. does as it says
   2 o.^:a:0                    NB. solve the fixed point equation cos(x) == x
   attributes OFF               NB. no more blinky flashy
   parseFrench=:;:,#.*."3,(C.A.)/\/&.:;:  NB. just kidding!  More output.

```




## Julia

Use within the Julia REPL command line.

```julia
using Crayons.Box

println(WHITE_FG, BLACK_BG, "Normal")
println(WHITE_BG, BLACK_FG, "Reversed")
println(WHITE_FG, BLACK_BG, "Normal")

```



## Kotlin

{{Works with|Ubuntu|14.04}}

```scala
// version 1.1.2

fun main(args: Array<String>) {
    println("\u001B[7mInverse\u001B[m Normal")
}
```



## Lasso


```Lasso
local(esc = decode_base64('Gw=='))

stdout( #esc + '[7m Reversed Video ' + #esc + '[0m Normal Video ')
```



## Mathematica


```Mathematica
Run["tput mr"]
Run["echo foo"] (* is displayed in reverse mode *)
Run["tput me"]
Run["echo bar"]
```



## Nim


```nim
echo "\e[7mReversed\e[m Normal"
```



## OCaml


Using the library [http://forge.ocamlcore.org/projects/ansiterminal/ ANSITerminal] in the interactive loop:


```ocaml
$ ocaml unix.cma -I +ANSITerminal ANSITerminal.cma

# open ANSITerminal ;;
# print_string [Inverse] "Hello\n" ;;
Hello
- : unit = ()
```



## Pascal

{{works with|Free_Pascal}}
{{libheader|Curses}}
Using Free Pascal and ncurses. On some systems linking to the libtinfo library may be necessary.

```pascal
program InverseVideo;
{$LINKLIB tinfo}
uses
  ncurses;
begin
  initscr;
  attron(A_REVERSE);
  printw('reversed');
  attroff(A_REVERSE);
  printw(' normal');
  refresh;
  getch;
  endwin;
end.

```



## Perl

Like Perl 6.

```perl
print "normal\n";
system "tput rev";
print "reversed\n";
system "tput sgr0";
print "normal\n";
```



## Perl 6


```perl6
say "normal";
run "tput", "rev";
say "reversed";
run "tput", "sgr0";
say "normal";
```


## Phix


```Phix
--
-- demo\rosetta\Inverse_Video.exw
-- 
### ==========================

--
text_color(BLACK)
bk_color(WHITE)
printf(1,"Inverse")
text_color(WHITE)
bk_color(BLACK)
printf(1," Video")
printf(1,"\n\npress enter to exit")
{} = wait_key()

```



## PicoLisp


```PicoLisp
(prin "abc")
(call "tput" "rev")
(prin "def")  # These three chars are displayed in reverse video
(call "tput" "sgr0")
(prinl "ghi")
```



## Python


```Python
#!/usr/bin/env python

print "\033[7mReversed\033[m Normal"
```




## Racket


```racket

#lang racket
(require (planet neil/charterm:3:0))
 
(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 0 0)
 (charterm-inverse)
 (charterm-display "Hello")
 (charterm-normal)
 (charterm-display "World"))

```



## REXX

This version only works with PC/REXX and Personal Rexx.

```rexx
/*REXX program to demonstrate reverse video.                            */
@day   = 'day'
@night = 'night'
call scrwrite , 1, @day, , , 7                         /*white on black.*/
call scrwrite , 1+length(@day), @night, , , 112        /*black on white.*/
                                       /*stick a fork in it, we're done.*/
```



## Ring


```ring

nverse = char(17)+char(128)+char(17)+char(15)
normal = char(17)+char(128+15)+char(17)+char(0)
see  inverse + " inverse " + normal + " video"

```



## Scala

{{Works with|Ubuntu|14.04}}

```scala
object Main extends App {
    println("\u001B[7mInverse\u001B[m Normal")
}
```


## Tcl

This only works on Unix terminals.

```tcl
# Get how the terminal wants to do things...
set videoSeq(reverse) [exec tput rev]
set videoSeq(normal) [exec tput rmso]
proc reverseVideo str {
    global videoSeq
    return "$videoSeq(reverse)${str}$videoSeq(normal)"
}

# The things to print
set inReverse "foo"
set inNormal "bar"

# Print those words
puts "[reverseVideo $inReverse] $inNormal"
```



## TPP


```tpp
--revon
This is inverse
--revoff
This is normal
```



## UNIX Shell

Use the [http://invisible-island.net/ncurses/man/tput.1.html tput(1)] utility to write the escape sequences that enable or disable reverse video.

{{works with|Bourne Shell}}


```bash
#!/bin/sh
tput mr     # foo is reversed
echo 'foo'
tput me     # bar is normal video
echo 'bar'
```


If the system supports terminfo, then <code>tput rev</code> and <code>tput sgr0</code> also work. (All recent systems have terminfo, except NetBSD, but [http://blog.netbsd.org/tnf/entry/terminfo_has_replaced_termcap NetBSD 6 will have terminfo].) The shorter names <code>mr</code> and <code>me</code> are the backward-compatible names from termcap.

If the terminal cannot do reverse video, then ''tput'' will fail with a message to standard error.


```bash
$ TERM=dumb tput mr
tput: Unknown terminfo capability `mr'
```


Some programs use the ''standout mode'', which might look exactly like reverse video. (The escape sequences might be identical!)


```bash
tput so     # enter standout mode
echo 'foo'
tput se     # exit standout mode
echo 'bar'
```


If the system supports terminfo, then <code>tput smso</code> and <code>tput rmso</code> also work.

=
## C Shell
=

```csh
tput mr
echo 'foo'
tput me
echo 'bar'
```



## XPL0

Output device 6 is similar to the normal console screen (device 0), but
it provides many combinations of foreground and background colors.


```XPL0
include c:\cxpl\codes;
[Attrib($70);
Text(6, "Inverse");
Attrib($07);
Text(6, " Video");
CrLf(6);
]
```



## zkl

There is no explicit support for terminals/video. But, assuming an ANSI terminal:

```zkl
println("\e[7mReversed\e[m Normal");
```


{{omit from|ACL2}}
{{omit from|Maxima}}
