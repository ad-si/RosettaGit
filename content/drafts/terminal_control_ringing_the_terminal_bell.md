+++
title = "Terminal control/Ringing the terminal bell"
description = ""
date = 2019-08-29T18:46:56Z
aliases = []
[extra]
id = 8507
[taxonomies]
categories = []
tags = []
+++

{{task|Terminal control}}
[[Terminal Control::task| ]]

;Task:
Make the terminal running the program ring its "bell". 


On modern terminal emulators, this may be done by playing some other sound which might or might not be configurable, or by flashing the title bar or inverting the colors of the screen, but was classically a physical bell within the terminal.   It is usually used to indicate a problem where a wrong character has been typed.

In most terminals, if the   [[wp:Bell character|Bell character]]   (ASCII code '''7''',   <big><code> \a </code></big> in C)   is printed by the program, it will cause the terminal to ring its bell.   This is a function of the terminal, and is independent of the programming language of the program, other than the ability to print a particular character to standard out.





## 6800 Assembly


```m68k
        .cr  6800
        .tf  bel6800.obj,AP1
        .lf  bel6800
;
### ===============================================
;
;         Ring the Bell for the Motorola 6800         ;
;                 by barrym 2013-03-31                ;
;-----------------------------------------------------;
; Rings the bell of an ascii terminal (console)       ;
;   connected to a 1970s vintage SWTPC 6800 system,   ;
;   which is the target device for this assembly.     ;
; Many thanks to:                                     ;
;   swtpc.com for hosting Michael Holley's documents! ;
;   sbprojects.com for a very nice assembler!         ;
;   swtpcemu.com for a very capable emulator!         ;
; reg a holds the ascii char to be output             ;
;-----------------------------------------------------;
outeee   =   $e1d1      ;ROM: console putchar routine
        .or  $0f00
;-----------------------------------------------------;
main    ldaa #7         ;Load the ascii BEL char
        jsr  outeee     ;  and print it
        swi             ;Return to the monitor
        .en
```



## Ada



```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Bell is
begin
   Put(Ada.Characters.Latin_1.BEL);
end Bell;
```



## Applescript


```applescript>beep</lang



## Asymptote


```Asymptote
beep()
```

See [http://asymptote.sourceforge.net/doc/Data-types.html#index-g_t_0040code_007bbeep_007d-287 beep() in the Asymptote manual].


## AutoHotkey


```AutoHotkey

fileappend, `a, *

```


This requires that you compile the exe in console mode (see Lexikos script to change this) or pipe the file 
through more: autohotkey bell.ahk |more


## AWK



```awk
BEGIN {
print "\a" # Ring the bell
}
```



## BASIC


=
## Applesoft BASIC
=


```Applesoft BASIC
 10  PRINT  CHR$ (7);
```


=
## Integer BASIC
=

You can't see it, but the bell character (Control G) is embedded in what looks like an empty string on line 10.

```Integer BASIC
  10 PRINT "";: REM ^G IN QUOTES
  20 END
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>PING
```


=
## Locomotive Basic
=


```locobasic
10 PRINT CHR$(7)
```


=== {{header|ZX Spectrum Basic}} ===

The ZX Spectrum had a speaker, rather than a bell. Here we use middle C as a bell tone, but we could produce a different note by changing the final zero to a different value.


```basic
BEEP 0.2,0
```



## Batch File

Source: [http://www.dostips.com/forum/viewtopic.php?f=3&t=5860 Here]

```dos
@echo off
for /f %%. in ('forfiles /m "%~nx0" /c "cmd /c echo 0x07"') do set bell=%%.
echo %bell%
```



## BBC BASIC

Assuming that the platform the program is running on rings the bell when CHR$7 is sent to the VDU driver:


```bbcbasic>VDU 7</lang



## Bc


```Bc
print "\a"
```



## beeswax



```beeswax
_7}
```



## Befunge


```befunge
7,@
```



## Bracmat

Run Bracmat in interactive mode (start Bracmat without command line arguments) and enter the following after the Bracmat prompt <code>{?}</code>:

```bracmat>\a</lang

Alternatively, run Bracmat non-interactively. In DOS, you write

```txt
bracmat "put$\a"
```

In Linux, you do

```txt
bracmat 'put$\a'
```


=={{header|Brainf***}}==
Assuming the output stream is connected to a TTY, printing BEL should ring its bell.


```brainfuck
  I
  +
 + +
 +++
+-+-+
  .
```



## C



```c>#include <stdio.h

int main() {
  printf("\a");
  return 0;
}
```



## C++


```cpp>#include <iostream


int main() {
  std::cout << "\a";
  return 0;
}
```


=={{header|C sharp|C#}}==
Inside a function:

```csharp
// the simple version:
System.Console.Write("\a"); // will beep
System.Threading.Thread.Sleep(1000); // will wait for 1 second
System.Console.Beep(); // will beep a second time
System.Threading.Thread.Sleep(1000);

// System.Console.Beep() also accepts (int)hertz and (int)duration in milliseconds:
System.Console.Beep(440, 2000); // default "concert pitch" for 2 seconds

```



## Clojure


```clojure
(println (char 7))
```



## COBOL

Standard compliant:

```cobol>DISPLAY SPACE WITH BELL</lang


{{works with|Visual COBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mf-bell.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  bell-code              PIC X USAGE COMP-X VALUE 22.
       01  dummy-param            PIC X.
       
       PROCEDURE DIVISION.
           CALL X"AF" USING bell-code, dummy-param
       
           GOBACK
           .
```



## Common Lisp


```lisp

(format t "~C" (code-char 7))

```



## D


```d
void main() {
    import std.stdio;
    writeln('\a');
}
```



## Delphi


```Delphi
program TerminalBell;

{$APPTYPE CONSOLE}

begin
  Writeln(#7);
end.
```



## E


```e
print("\u0007")
```



## Emacs Lisp


```lisp
(ding)    ;; ring the bell
(beep)    ;; the same thing
```

On a tty or in <code>-batch</code> mode this emits a BEL character.  In a GUI it does whatever suits the window system.  Variables <code>visible-bell</code> and <code>ring-bell-function</code> can control the behaviour.

<code>beep</code> was originally called <code>feep</code>, but that changed, recently :-)


```txt
Fri Dec 13 00:52:16 1985  Richard M. Stallman  (rms at prep)
	* subr.el: Rename feep to beep, a more traditional name.
```


=={{header|F_Sharp|F#}}==

```fsharp
open System

Console.Beep()
```



## Forth


```forth>7 emit</lang


{{works with|GNU Forth}}


```forth>#bell emit</lang


{{works with|iForth}}


```forth
^G emit
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Print !"\a"
Sleep
```



## gnuplot


```gnuplot
print "\007"
```



## Go


```go
package main

import "fmt"

func main() {
  fmt.Print("\a")
}
```



## Groovy


```groovy
println '\7'
```



## Haskell


```haskell
main = putStr "\a"
```


=={{header|Icon}} and {{header|Unicon}}==

Works on both Icon and Unicon.


```Icon

procedure main ()
  write ("\7") # ASCII 7 rings the bell under Bash
end

```



## J

This j sentence reads "Seven from alphabet."

```J
   7{a.  NB. noun a. is a complete ASCII ordered character vector.
```



## Java


```java
public class Bell{
    public static void main(String[] args){
        java.awt.Toolkit.getDefaultToolkit().beep();

        //or

        System.out.println((char)7);
    }
}
```



## Julia

{{works with|Linux}}

```Julia

println("This should ring a bell.\a")

```


{{out}}

```txt

This should ring a bell.

```


And it does, provided that the bell is enabled on your terminal.


## Kotlin

{{works with|Windows|10}}

```scala
// version 1.1.2

fun main(args: Array<String>) {
    println("\u0007")
}
```



## Lasso


```Lasso
stdoutnl('\a')
```



## Logo


```logo>type char 7</lang



## Lua


```lua
print("\a")
```



## M2000 Interpreter

M2000 Environment has own console (not the one provided from system). Console used for graphics, and has 32 layers for text or and graphics and as sprites too. We can alter the console by code, moving to any monitor, changing font, font size and, line space. Also there is a split function, where the lower part can scroll, and the upper part used as header (we can write/draw in the upper part also, but CLS - clear screen- statement clear only the lower part).


### Using Windows Bell

Async beep. If another start while beeps (it is a bell), then stop

```M2000 Interpreter

Module CheckIt {
      After 300 {beep}
      Print "Begin"
      for i=0 to 100 {
            wait 10
            Print i
      }
      Print "End"
}
CheckIt

```




### Play tone at 1khz or specific hz

Execution stop to play tone

```txt

Tone (1khz)
Tone 200 (1 kgz 200 ms)
Tone 200, 5000 (5khz. 200ms)

```



```M2000 Interpreter

Module CheckIt {
      After 300 {Tone 200}
      Print "Begin"
      for i=0 to 100 {
            wait 10
            Print i
      }
      Print "End"
}
CheckIt

```





### Play melody with beeper

Execution stop to play tune

```txt

Tune melody$
Tune duration_per_note, melody$

```



```M2000 Interpreter

Module CheckIt {
      After 300 {Tune 300, "C3BC#"}
      Print "Begin"
      for i=0 to 100 {
            wait 10
            Print i
      }
      Print "End"
}
CheckIt

```




### using midi to send music scores

Play a score in each of 16 voices (async, programming internal midi, problem with async in Wine Linux). We can make a piano using keyboard and play/score commands.


```M2000 Interpreter

Module CheckIt {
      Score 1, 500, "c@2dc @2ef"
      Play 1, 19  ' attach a music score to an organ
      Print "Begin"
      for i=0 to 100 {
            wait 10
            Print i
      }
      Print "End"
      \\ stop play, remove this and music continue, in console prompt
      Play 0
}
CheckIt

```


There are other statements like Sound, and Background filename$ to play background music.


## Mathematica


```Mathematica
Print["\007"]
```



## MUMPS


```MUMPS
write $char(7)
```



## Nemerle


```Nemerle
using System.Console;

module Beep
{
    Main() : void
    {
        Write("\a");
        System.Threading.Thread.Sleep(1000);
        Beep();
        System.Threading.Thread.Sleep(1000);
        Beep(2600, 1000); // limited OS support
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  do
    BEL = 8x07
    jtk = java.awt.toolkit.getDefaultToolkit()
    say 'Bing!'(Rexx BEL).d2c
    Thread.sleep(500)
    say 'Ding\x07-ding\u0007!'
    Thread.sleep(500)
    say 'Beep!'
    jtk.beep()
  catch ex = Exception
    ex.printStackTrace()
  end
  return

```



## Nim


```nim
echo "\a"
```



## Objeck


```objeck
7->As(Char)->PrintLine();
```



## PARI/GP

{{Works with|PARI/GP|2.7.4 and above}}


```parigp
\\ Ringing the terminal bell.
\\ 8/14/2016 aev
Strchr(7) \\ press <Enter>
```
 
;or:

```parigp
print(Strchr(7)); \\ press <Enter>
```


{{Output}}

```txt

(11:12) gp > Strchr(7) \\ press <Enter>
%6 = ""
(11:13) gp > print(Strchr(7)); \\ press <Enter>

(11:14) gp >

```



## Pascal

See [[Terminal_control/Ringing_the_terminal_bell#Delphi | Delphi]]


## Perl


```perl
print "\a";
```



## Perl 6


```perl6>print 7.chr;</lang



## Phix


```Phix
puts(1,"\x07")
```



## PHP


```php
<?php
echo "\007";
```



## PicoLisp


```PicoLisp
(beep)
```



## PL/I


```pli
   declare bell character (1);
   unspec (bell) = '00000111'b;
   put edit (bell) (a);
```



## PostScript

The following will only work in a PostScript interpreter that sends output to a terminal. It will very likely not make a printer beep.

```postscript
(\007) print
```



## PowerShell

One can either use the ASCII <code>BEL</code> character which only works in a console (i.e. not in a graphical PowerShell host such as PowerShell ISE):

```powershell
"`a"
```

or use the .NET <code>Console</code> class which works independent of the host application:

```powershell
[Console]::Beep()
```



## PureBasic


```PureBasic
Print(#BEL$)
```


## Python

In Python 2.7.x:

```python
print "\a"
```

In Python 3.x:

```python
print("\a")
```



## R


```R
alarm()
```



## Racket


```racket

#lang racket
(require (planet neil/charterm:3:0))
(with-charterm
 (void (charterm-bell)))

```



## Retro


```Retro>7 putc</lang



## REXX

There is no standard REXX built-in function to handle the sounding of the bell or a PC's speaker.

However, some REXX interpreters have added a non-standard BIF.

```rexx
/*REXX program illustrates methods to  ring the terminal bell  or  use the PC speaker.  */
                     /*╔═══════════════════════════════════════════════════════════════╗
                       ║                                                               ║
                       ║  Note that the  hexadecimal code  to ring the  terminal bell  ║
                       ║  is different on an ASCII machine than an EBCDIC machine.     ║
                       ║                                                               ║
                       ║  On an  ASCII machine,  it is  (hexadecimal)  '07'x.          ║
                       ║   "  " EBCDIC    "       "  "        "        '2F'x.          ║
                       ║                                                               ║
                       ╚═══════════════════════════════════════════════════════════════╝*/

if 3=='F3'  then bell= '2f'x                     /*we are running on an EBCDIC machine. */
            else bell= '07'x                     /* "  "     "     "  "  ASCII    "     */

say bell                                         /*sound the  bell  on the terminal.    */
say copies(bell, 20)                             /*as above,  but much more annoying.   */

                     /*╔═══════════════════════════════════════════════════════════════╗
                       ║                                                               ║
                       ║  Some REXX interpreters have a  built-in function  (BIF)  to  ║
                       ║  to produce a sound on the PC speaker, the sound is specified ║
                       ║  by frequency  and  an optional  duration.                    ║
                       ║                                                               ║
                       ╚═══════════════════════════════════════════════════════════════╝*/

                                         /* [↓]  supported by Regina REXX:              */
freq= 1200                               /*frequency in  (nearest)  cycles per second.  */
call  beep freq                          /*sounds the PC speaker, duration=  1   second.*/
ms=   500                                /*duration in milliseconds.                    */
call  beep freq, ms                      /*  "     "   "    "         "     1/2     "   */


                                         /* [↓]  supported by PC/REXX  &  Personal REXX:*/
freq= 2000                               /*frequency in  (nearest)  cycles per second.  */
call  sound freq                         /*sounds PC speaker, duration=   .2   second.  */
secs= .333                               /*duration in seconds (round to nearest tenth).*/
call  sound freq, secs                   /*  "     "    "         "      3/10     "     */

                                         /*stick a fork in it, we're done making noises.*/
```



## Ring


```ring

see char(7)

```



## Ruby


```ruby
print "\a"
```



## Scala


```scala
java.awt.Toolkit.getDefaultToolkit().beep()
```


## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    write("\a");
  end func;
```



## Sidef


```ruby
print "\a";
```



## SNUSP


```snusp
$+++++++.#
```



## Tcl


```tcl
puts -nonewline "\a";flush stdout
```



## UNIX Shell

{{works with|Bourne Shell}} {{works with|bash}}


```sh
#!/bin/sh
# Ring the terminal bell
# echo "\a" # does not work in some shells
tput bel
```



## X86 Assembly


```X86 Assembly
;Assemble with: tasm; tlink /t
        .model  tiny
        .code
        org     100h            ;.com files start here
start:  mov     ah, 02h         ;character output
        mov     dl, 07h         ;bell code
        int     21h             ;call MS-DOS
        ret                     ;return to MS-DOS
        end     start
```



## XPL0


```XPL0
code ChOut=8;
ChOut(0,7)
```



## zkl


```zkl
print("\x07");
```



{{omit from|ACL2}}
{{omit from|Axe}}
{{omit from|Inform 7}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
