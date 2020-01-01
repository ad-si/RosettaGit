+++
title = "Hello world/Text"
description = ""
date = 2019-10-04T03:16:11Z
aliases = []
[extra]
id = 1514
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
{{selection|Short Circuit|Console Program Basics}}
[[Category:Simple]]

;Task:
Display the string '''Hello world!''' on a text console.

;Related tasks:
*   [[Hello world/Graphical]]
*   [[Hello world/Line Printer]]
*   [[Hello world/Newline omission]]
*   [[Hello world/Standard error]]
*   [[Hello world/Web server]]





## 0815


```0815

<:48:x<:65:=<:6C:$=$=$$~<:03:+
$~<:ffffffffffffffb1:+$<:77:~$
~<:fffffffffffff8:x+$~<:03:+$~
<:06:x-$x<:0e:x-$=x<:43:x-$


```



## 360 Assembly

Using native SVC (Supervisor Call) to write to system console:

```360 Assembly

HELLO    CSECT
         USING HELLO,15
         LA    1,MSGAREA     Point Register 1 to message area
         SVC   35            Invoke SVC 35 (Write to Operator)
         BR    14            Return
MSGAREA  EQU   *             Message Area
         DC    AL2(19)       Total area length = 19 (Prefix length:4 + Data Length:15)
         DC    XL2'00'       2 bytes binary of zeros
         DC    C'Hello world!'  Text to be written to system console
         END
```

Using WTO Macro to generate SVC 35 and message area:

```360 Assembly
         WTO   'Hello world!'
         BR    14            Return
         END

```



## 4DOS Batch


```4dos
echo Hello world!
```



## 6502 Assembly


```asm
; goodbyeworld.s for C= 8-bit machines, ca65 assembler format.
; String printing limited to strings of 256 characters or less.

a_cr	= $0d		; Carriage return.
bsout	= $ffd2		; C64 KERNEL ROM, output a character to current device.
			; use $fded for Apple 2
	.code

	ldx #0		; Starting index 0 in X register.
printnext:
	lda text,x	; Get character from string.
	beq done	; If we read a 0 we're done.
	jsr bsout	; Output character.
	inx		; Increment index to next character.
	bne printnext	; Repeat if index doesn't overflow to 0.
done:
	rts		; Return from subroutine.

	.rodata

text:
	.byte	"Hello world!", a_cr, 0
```



## 6800 Assembly

<lang>        .cr  6800
        .tf  gbye6800.obj,AP1
        .lf  gbye6800
;
### ===============================================
;
;        Hello world! for the Motorola 6800        ;
;                 by barrym 2013-03-17                ;
;-----------------------------------------------------;
; Prints the message "Hello world!" to an ascii    ;
;   terminal (console) connected to a 1970s vintage   ;
;   SWTPC 6800 system, which is the target device for ;
;   this assembly.                                    ;
; Many thanks to:                                     ;
;   swtpc.com for hosting Michael Holley's documents! ;
;   sbprojects.com for a very nice assembler!         ;
;   swtpcemu.com for a very capable emulator!         ;
; reg x is the string pointer                         ;
; reg a holds the ascii char to be output             ;
;-----------------------------------------------------;
outeee   =   $e1d1      ;ROM: console putchar routine
        .or  $0f00
;-----------------------------------------------------;
main    ldx  #string    ;Point to the string
        bra  puts       ;  and print it
outs    jsr  outeee     ;Emit a as ascii
        inx             ;Advance the string pointer
puts    ldaa ,x         ;Load a string character
        bne  outs       ;Print it if non-null
        swi             ;  else return to the monitor
;
### ===============================================
;
string  .as  "Hello world!",#13,#10,#0
        .en
```



## 8086 Assembly



```masm
DOSSEG
.MODEL TINY
.DATA
TXT DB "Hello world!$"
.CODE
START:
	MOV ax, @DATA
	MOV ds, ax

	MOV ah, 09h		; prepare output function
	MOV dx, OFFSET TXT	; set offset
	INT 21h			; output string TXT

	MOV AX, 4C00h 		; go back to DOS
	INT 21h
END START
```


With A86 or NASM syntax:

```txt
  org 100h

  mov dx, msg
  mov ah, 9
  int 21h

  mov ax, 4c00h
  int 21h

msg:
  db "Hello world!$"
```



## 8th


```forth
"Hello world!\n" . bye
```



## AArch64 Assembly

<lang ARM_Assembly>.equ STDOUT, 1
.equ SVC_WRITE, 64
.equ SVC_EXIT, 93

.text
.global _start

_start:
	stp x29, x30, [sp, -16]!
	mov x0, #STDOUT
	ldr x1, =msg
	mov x2, 13
	mov x8, #SVC_WRITE
	mov x29, sp
	svc #0 // write(stdout, msg, 13);
	ldp x29, x30, [sp], 16
	mov x0, #0
	mov x8, #SVC_EXIT
	svc #0 // exit(0);

msg:	.ascii "Hello World!\n"
.align 4
```



## ABAP


```ABAP
REPORT zgoodbyeworld.
  WRITE 'Hello world!'.
```



## ACL2


```lisp
(cw "Hello world!~%")
```



## ActionScript


```ActionScript
trace("Hello world!");
```



## Ada

{{works with|GCC|4.1.2}}

```ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
  Put_Line ("Hello world!");
end Main;
```



## Agena


```agena
print( "Hello world!" )
```



## Aime


```aime
o_text("Hello world!\n");
```


or:


```aime
integer
main(void)
{
    o_text("Hello world!\n");

    return 0;
}
```



## Algae


```algae
printf("Hello world!\n");
```



## ALGOL 60


```algol60
'BEGIN'
    OUTSTRING(1,'('Hello world!')');
    SYSACT(1,14,1)
'END'
```



## ALGOL 68


```algol68
main: (
  printf($"Hello world!"l$)
)
```


=={{header|ALGOL-M}}==

```algol
BEGIN
    WRITE( "Hello world!" );
END
```



## ALGOL W


```algolw
begin
    write( "Hello world!" )
end.
```



## Alore


```alore
Print('Hello world!')
```



## AmbientTalk


```ambienttalk
system.println("Hello world!")
```



## AmigaE


```amigae
PROC main()
  WriteF('Hello world!\n')
ENDPROC
```



## AntLang

Note, that "Hello, World!" prints twice in interactive mode.
One time as side-effect and one as the return value of echo.

```AntLang
echo["Hello, World!"]
```



## Anyways



```Anyways
There was a guy called Hello World
"Ow!" it said.
That's all folks!
```



## APL


```apl
'Hello world!'
```



## AppleScript

To show in Script Editor Result pane:

```applescript
"Hello world!"
```


To show in Script Editor Event Log pane:

```applescript
log "Hello world!"
```



## Applesoft BASIC

<!-- {{incorrect|Applesoft BASIC|output isn't consistent with the task's requirements: mixed case.}} -->

Important Note: Although Applesoft BASIC allowed the storage and output of mixed-case strings, the ability to enter mixed-case via the keyboard and to output mixed-case on the default display was not offered as standard equipment on the original Apple II/II+.  Since Applesoft WAS the default programming language for the Apple II+, perhaps some flexibility in the task specification could be offered, for this and for other systems that lacked proper mixed-case I/O capabilities in at least one popular configuration.


```Applesoft BASIC
 PRINT "Hello world!"
```



## Apricot


```apricot
(puts "Hello world!")
```



## Arc


```arc
(prn "Hello world!")
```



## Arendelle


```txt
"Hello world!"
```



## Argile


```Argile
use std
print "Hello world!"
```

compile with: arc hello_world.arg -o hello_world.c && gcc -o hello_world hello_world.c


## ARM Assembly


<lang ARM_Assembly>.global main

message:
    .asciz "Hello world!\n"
    .align 4

main:
    ldr r0, =message
    bl printf

    mov r7, #1
    swi 0
```



## ArnoldC


```ArnoldC
IT'S SHOWTIME
TALK TO THE HAND "Hello world!"
YOU HAVE BEEN TERMINATED
```



## Arturo


```arturo
"Hello world!"
```

or:

```arturo
print "Hello world!"
```



## Astro


```python
print "Hello world!"
```



## Asymptote



```asymptote
write('Hello world!');
```



## AsciiDots



```AsciiDots

.-$'Hello, World!'

```



## ATS


```ATS
implement main0 () = print "Hello world!\n"
```



## AutoHotkey

script launched from windows explorer

```AutoHotkey
DllCall("AllocConsole")
FileAppend, Goodbye`, World!, CONOUT$
FileReadLine, _, CONIN$, 1
```

scripts run from shell
[requires Windows XP or higher; older Versions of Windows don´t have the "AttachConsole" function]

```AutoHotkey
DllCall("AttachConsole", "int", -1)
FileAppend, Goodbye`, World!, CONOUT$
```


```AutoHotkey
SendInput Hello world!{!}
```



## AutoIt


```AutoIt
ConsoleWrite("Hello world!" & @CRLF)
```



## AutoLISP


```cadlisp
(printc "Hello World!")
```



## AWK


```awk
BEGIN{print "Hello world!"}
```



"BEGIN" is a "special pattern" - code within "{}" is executed before the input file is read, even if there is no input. "END" is a similar pattern, for after completion of main processing.

```awk

END {
     print "Hello world!"
    }

```


For a file containing data, the work can be done in the "body". The "//" is "match anything" so gets the first data, the "exit" halts processing the file (any "END" would then be executed).  Or instead of //, simply 1 is true.

```awk

//  {
    print "Hello world!"
    exit
    }

```



For a "single record" file.

```awk

//  {
    print "Hello world!"
    }

```


For a "single record" file containing - Hello world! -. The "default" action for a "pattern match" (the "/" and "/" define a "pattern" to match data) is to "print" the record.

```awk

//

```



## Axe

Note that the i here is the imaginary ''i'', not the lowercase letter i.

```axe
Disp "Hello world!",i
```



## B

{{works with|The Amsterdam Compiler Kit - B|V6.1pre1}}

```B
main()
{
    putstr("Hello world!*n");
    return(0);
}
```



## Babel



```babel
"Hello world!" <<
```



## bash


```bash
echo "Hello world!"
```



## BASIC

{{works with|BASICA}}
{{works with|Commodore BASIC}}
{{works with|Locomotive Basic}}
{{works with|M2000 Interpreter}}

```qbasic
10 print "Hello world!"
```


{{works with|7Basic}}
{{works with|BaCon}} [[Category:BaCon]]
{{works with|QBasic}}
{{works with|M2000 Interpreter}}

```qbasic
PRINT "Hello world!"
```



## BASIC256


```BASIC256
PRINT "Hello world!"
```



## Basic Casio


```Basic Casio
Locate 1,1,"Hello World!"
```

or just

```Basic Casio
"Hello World!"
```



## Batch File

'''Under normal circumstances, when delayed expansion is disabled'''

```dos
echo Hello world!
```


'''If delayed expansion is enabled, then the ! must be escaped twice'''

```dos
setlocal enableDelayedExpansion
echo Hello world!^^!
```



## Battlestar

<!--- supports C syntax highlighting --->

```c
const hello = "Hello world!\n"

print(hello)
```



## BBC BASIC


```bbcbasic
      PRINT "Hello world!"
```



## bc


```bc
"Hello world!
"
```



## BCPL


```BCPL
GET "libhdr"

LET start() = VALOF
{ writef("Hello world!")
  RESULTIS 0
}
```



## beeswax

Straightforward:


```Beeswax
*`Hello, World!
```


Less obvious way:


```beeswax>
`ld!
`
 r
  o
   W
    `
     b` ,olleH`_
```


Even less obvious, demonstrating the creation and execution order of instruction pointers, and the hexagonal layout of beeswax programs:


```beeswax
r  l
 l o
  ``
ol`*`,d!
   ``
   e H
   W
```



## Befunge


```befunge
52*"!dlroW ,eybdooG">:#,_@
```



## Bird

It's not possible to print exclamation marks in [[Bird]] which is why it is not used in this example.

```Bird
use Console

define Main
    Console.Println "Hello world"
end
```



## Blast


```blast
# This will display a goodbye message on the terminal screen
.begin
display "Hello world!"
return
# This is the end of the script.
```



## blz


```blz
print("Hello world!")
```



## BML


```bml
display "Hello world!"
```



## Boo


```boo
print "Hello world!"
```



## Brace


```brace
#!/usr/bin/env bx
use b
Main:
	say("Hello world!")
```



## Bracmat


```bracmat
put$"Hello world!"
```


=={{header|Brainfuck}}==
To print text, we need the ascii-value of each character to output.

So, we wanna make a series of round numbers going like:

 10	close to newline and carriage return
 30	close to ! and SPACE
 40	close to COMMA
 70	close to G
 80	close to W
 90	close to b
 100	is d and close to e and l
 110	close to o
 120	close to y

forming all the letters we need if we just add up a bit

Commented version:

```bf
+++++ +++++		First cell 10 (its a counter and we will be "multiplying")

[
>+			10 times 1 is 10
>+++			10 times 3 is 30
>++++			etc etc
>+++++ ++
>+++++ +++
>+++++ ++++
>+++++ +++++
>+++++ ++++++
>+++++ +++++++
<<<<<<<<< -		go back to counter and subtract 1
]

printing G
>>>> + .

o twice
>>>> + ..

d
< .

b
< +++++ +++ .

y
>>> + .

e
<< + .

COMMA
<<<< ++++ .

SPACE
< ++ .

W
>>> +++++ ++ .

o
>>> .

r
+++ .

l
< +++++ ++ .

d
----- --- .

!
<<<<< + .

CRLF
< +++ . --- .
```


Uncommented:

```bf>++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++
++
++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>
>+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.
<+++++++.--------.<<<<<+.<+++.---.
```

It can most likely be optimized, but this is a nice way to show
how character printing works in Brainfuck :)


## Brat


```brat
p "Hello world!"
```



## Brlcad


The mged utility can output text to the terminal:


```brlcad

echo Hello world!

```



## Burlesque



```burlesque

"Hello world!"sh

```


Although please note that ''sh'' actually does not print anything.


## C

{{works with|gcc|4.0.1}}

```cpp
#include <iostream>
#include <stdio.h>

int main(void)
{
  printf("Hello world!\n");
  return EXIT_SUCCESS;
}
```

Or:

```cpp
#include <iostream>
#include <stdio.h>

int main(void)
{
  puts("Hello world!");
  return EXIT_SUCCESS;
}
```

Or, the eternal favourite :)

```c

#include<stdio.h>

int main()
{
  printf("\nHello world!");
  return 0;
}
```


or better yet...

```C

#include<stdio.h>

int main()
{
	return printf("\nHello World!");
}

```


## C#
{{works with|Mono|1.2}}
{{works with|Visual C sharp|Visual C#|2003}}

```c#
namespace HelloWorld
{
    class Program
    {
        static void Main(string[] args)
        {
            System.Console.WriteLine("Hello world!");
        }
    }
}
```



## C++


```cpp
#include <iostream>

int main () {
  std::cout << "Hello world!" << std::endl;
}
```



## C++/CLI


```cpp
using namespace System;
int main()
{
  Console::WriteLine("Hello world!");
}
```



## C1R


```C0H
Hello_world/Text
```

{{out}}

```txt

$ echo Hello_world/Text >hw.c1r
$ ./c1r hw.c1r
$ ./a.out
Hello world!

```



## Cat


```Cat
"Hello world!" writeln
```



## Cduce


```Cduce
print "Hello world!";;
```



## Chef


```Chef
Goodbye World Souffle.

Ingredients.
71 g green beans
111 cups oil
98 g butter
121 ml yogurt
101 eggs
44 g wheat flour
32 zucchinis
119 ml water
114 g red salmon
108 g lard
100 g dijon mustard
33 potatoes

Method.
Put potatoes into the mixing bowl.
Put dijon mustard into the mixing bowl.
Put lard into the mixing bowl.
Put red salmon into the mixing bowl.
Put oil into the mixing bowl.
Put water into the mixing bowl.
Put zucchinis into the mixing bowl.
Put wheat flour into the mixing bowl.
Put eggs into the mixing bowl.
Put yogurt into the mixing bowl.
Put butter into the mixing bowl.
Put dijon mustard into the mixing bowl.
Put oil into the mixing bowl.
Put oil into the mixing bowl.
Put green beans into the mixing bowl.
Liquefy contents of the mixing bowl.
Pour contents of the mixing bowl into the baking dish.

Serves 1.
```



## ChucK

<lang><<< "Hello world!">>>;
```



## Cind



```cind

execute() {
    host.println("Hello world!");
}

```




## Clay


```clay
main() {
    println("Hello world!");
}
```



## Clean


```clean
Start = "Hello world!"
```



## Clipper


```Clipper
? "Hello world!"
```



## CLIPS


```clips
(printout t "Hello world!" crlf)
```



## Clio


```clio
'hello world!' -> print
```



## Clojure


```lisp
(println "Hello world!")
```



## CMake


```cmake
message(STATUS "Hello world!")
```


This outputs


```txt
-- Hello world!
```



## COBOL

Using fixed format.
{{works with|OpenCOBOL}}
{{works with|Dell Enterprise COBOL}}


```cobol
	program-id. hello.
	procedure division.
		display "Hello world!".
		stop run.
```


Using relaxed compilation rules, the hello program can become a single DISPLAY statement.
{{works with|GnuCOBOL}}


```cobol
display"Hello, world".
```



```txt
prompt$ cobc -x -frelax-syntax -free hello.cob
hello.cob: 1: Warning: PROGRAM-ID header missing - assumed
hello.cob: 1: Warning: PROCEDURE DIVISION header missing - assumed

prompt$ ./hello
Hello, world
```


''Note how COBOL can handle the DISPLAY reserved word without a space before the quoted string, the quote being a compile time scan delimiter.  The full stop period after the single statement is still mandatory, at least for GnuCOBOL and a clean compile to executable.''


## Cobra


```cobra
class Hello
    def main
        print 'Hello world!'
```



## CoffeeScript

{{works with|Node.js}}

```coffeescript
console.log "Hello world!"
```

{{works with|Rhino engine}}

```coffeescript
print "Hello world!"
```



## ColdFusion


```coldfusion><cfoutput
Hello world!</cfoutput>
```



## Comal


```Comal
PRINT "Hello world!"
```



## Comefrom0x10


```cf0x10
'Hello world!'
```



```cf0x10
"Hello world!"
```




## Commodore BASIC

By default some Commodore computers boot into uppercase/graphics mode (C64, C128, VIC-20, Plus 4, etc.) while others (PET, CBM etc.) boot into lowercase/uppercase mode. Therefore, depending on machine used, the CHR$(14) may or may not be required to switch into mixed-case mode.

```GWBasic
10 print chr$(147);chr$(14);:REM 147=clear screen, 14=switch to lowercase mode
20 print "Hello world!"
30 end

```


{{Out}}
```txt
Hello world!
```



## Common Lisp


```lisp
(format t "Hello world!~%")
```


Or


```lisp
(print "Hello world!")
```



### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : Hello world/Text

(format t "~a" "Hello world!")

```

Output:

```txt

Hello world!

```



## Component Pascal


```oberon2

MODULE Hello;
	IMPORT Out;

	PROCEDURE Do*;
	BEGIN
		Out.String("Hello world!"); Out.Ln
	END Do;
END Hello.
```

Run command ''Hello.Do'' by commander.


## Crack


```crack

import crack.io cout;
cout `Hello world!\n`;

```



## Creative Basic


```Creative Basic

OPENCONSOLE

PRINT"Hello world!"

'This line could be left out.
PRINT:PRINT:PRINT"Press any key to end."

'Keep the console from closing right away so the text can be read.
DO:UNTIL INKEY$<>""

CLOSECONSOLE

END

```



## Crystal


```ruby
puts "Hello world!"
```



## D

{{works with|D|2.0}}

```D
import std.stdio;

void main() {
    writeln("Hello world!");
}
```



## Dafny


```dafny

method Main() {
  print "hello, world!\n";
  assert 10 < 2;
}

```



## Dao


```dao
io.writeln( 'Hello world!' )
```



## Dart


```dart
main() {
    var bye = 'Hello world!';
    print("$bye");
}
```



## DataWeave


```DataWeave
"Hello world!"
```



## Dc


```dc
[Hello world!]p
```

...or print a numerically represented string.

```dc>5735816763073014741799356604682 P</lang



## DCL


```DCL
$ write sys$output "Hello world!"
```



## DDNC


DDNC can only output to a single 7-segment LED display digit, so first we must convert each character into its 7-segment equivalent numerical value.

The three horizontal bars are assigned bits 6, 3, and 0 from top to bottom. The top two vertical bars are assigned bits 5 and 4 while the bottom two vertical bars are assigned bits 2 and 1 from left to right.

Because DDNC can only interpret literals in decimal, each binary number was converted and stored in consecutive memory cells starting at cell 10.

The code can be divided into three sections. The first stores the character numbers in order in an array. The second sets up the loop by loading a delay of 500 milliseconds to slot 3, the start address of the character array in memory to slot 2, and the number of times to loop (14) plus one to slot 5. The third section starts the loop of displaying the characters, waiting for the delay time, incrementing the pointer, decrementing the counter, and checking if the counter is negative to know whether to continue the loop.


```ddnc

0 111 10
0 15 11
0 15 12
0 31 13
0 47 14
0 59 15
0 125 16
0 3 17
0 0 18
0 63 19
0 15 20
0 12 21
0 36 22
0 31 23
0 17 24

0 500 3
0 10 2
0 15 5

60 4
2 2 1
80 1
72 3
30 2
31 5
62 5
61 4
64

```


=={{header|Déjà Vu}}==

```dejavu
!print "Hello world!"
```



## DeviousYarn


```deviousyarn
o:"Hello world!
```



## Delphi


```delphi

program ProjectGoodbye;
{$APPTYPE CONSOLE}
begin
  WriteLn('Hello world!');
end.

```



## DIV Games Studio


```div

PROGRAM HELLOWORLD;

BEGIN

    WRITE_TEXT(0,160,100,4,"HELLO WORLD!");
    LOOP
        FRAME;
    END
END


```



## DM


```DM

/client/New()
    ..()
    src << "Hello world!"

```



## Dragon


```dragon

showln "Hello world!"

```



## DWScript


```delphi

PrintLn('Hello world!');

```



## Dyalect


```Dyalect
print("Hello world!")
```



## Dylan


```Dylan

module: hello-world

format-out("%s\n", "Hello world!");

```



## Dylan.NET

{{works with|Mono|2.6.7}}
{{works with|Mono|2.10.x}}
{{works with|Mono|3.x.y}}
{{works with|.NET|3.5}}
{{works with|.NET|4.0}}
{{works with|.NET|4.5}}
One Line version:

```Dylan.NET
Console::WriteLine("Hello world!")
```


Hello World Program:

```Dylan.NET

//compile using the new dylan.NET v, 11.5.1.2 or later
//use mono to run the compiler
#refstdasm mscorlib.dll

import System

assembly helloworld exe
ver 1.2.0.0

class public Program

   method public static void main()
      Console::WriteLine("Hello world!")
   end method

end class

```



## E


```e
println("Hello world!")

stdout.println("Hello world!")
```



## EasyLang


<lang>print "Hello world!"
```



## eC


```ec
class GoodByeApp : Application
{
   void Main()
   {
      PrintLn("Hello world!");
   }
}
```



## EchoLisp


```lisp

(display "Hello world!" "color:blue")

```



## ECL


```ECL

OUTPUT('Hello world!');

```



## EDSAC order code

The EDSAC did not support lower-case letters. The method used here is to include a separate <code>O</code> order to print each character: for short messages and labels this is quite adequate. A more general (though slightly more involved) solution for printing strings is given at [[Hello world/Line printer#EDSAC order code]].

```edsac
[ Print HELLO WORLD ]
[ A program for the EDSAC ]
[ Works with Initial Orders 2 ]

T64K  [ Set load point: address 64 ]
GK    [ Set base address ]
O13@  [ Each O order outputs one ]
O14@  [ character. The numerical ]
O15@  [ parameter gives the offset ]
O16@  [ (from the base address) where ]
O17@  [ the character to print is ]
O18@  [ stored ]
O19@
O20@
O21@
O22@
O23@
O24@
ZF    [ Stop ]
*F    [ Shift to print letters ]
HF    [ Character literals ]
EF
LF
LF
OF
!F    [ Space character ]
WF
OF
RF
LF
DF
EZPF  [ Start program beginning at
        the load point ]
```

{{out}}

```txt
HELLO WORLD
```



## Efene

short version (without a function)


```efene
io.format("Hello world!~n")
```


complete version (put this in a file and compile it)


```efene
@public
run = fn () {
    io.format("Hello world!~n")
}
```



## Egel


```Egel

def main = "Hello World!"

```



## Egison



```egison

(define $main
  (lambda [$argv]
    (write-string "Hello world!\n")))

```



## EGL

{{works with|EDT}}
{{works with|RBD}}

```EGL

program HelloWorld
    function main()
        SysLib.writeStdout("Hello world!");
    end
end

```



## Eiffel

{{wikipedia|Eiffel (programming language)}}

```eiffel
class
    HELLO_WORLD
create
    make
feature
    make
        do
            print ("Hello world!%N")
        end
end
```



## Ela


```ela
open monad io
do putStrLn "Hello world!" ::: IO
```



## elastiC

From the [http://www.elasticworld.org/man/elastic.html elastiC Manual].


```elastic
package hello;

    // Import the `basic' package
    import basic;

    // Define a simple function
    function hello()
    {
        // Print hello world
        basic.print( "Hello world!\n" );
    }

    /*
     *  Here we start to execute package code
     */

    // Invoke the `hello' function
    hello();
```



## Elena

ELENA 4.x:

```elena
public program()
{
    console.writeLine:"Hello world!"
}
```



## Elisa


```elisa
 "Hello world!"?
```



## Elixir


```elixir

IO.puts "Hello world!"

```



## Elm


```haskell
main = text "Goodbye World!"
```



## Emacs Lisp


```lisp
(insert "Hello world!")
```



## Emojicode


```emojicode
🏁 🍇
  😀 🔤Hello world!🔤
🍉
```



## Erlang


```erlang
io:format("Hello world!~n").
```



## ERRE


```ERRE

! Hello World in ERRE language
PROGRAM HELLO
BEGIN
  PRINT("Hello world!")
END PROGRAM

```



## Euler Math Toolbox



```txt

"Hello world!"

```


=={{header|Extended Brainfuck}}==


```bf>[.
]@Hello world!
```



## Ezhil


<span style="color:#FF0000">பதிப்பி</span><span style="color:#CD5C5C">"வணக்கம் உலகம்!"</span><br />
<span style="color:#FF0000">பதிப்பி</span><span style="color:#CD5C5C"> "Hello world!" </span><br />
<span style="color:#FF0000">பதிப்பி</span><span style="color:#CD5C5C">"******* வணக்கம்! மீண்டும் சந்திப்போம் *******"</span><br />
<span style="color:#8B0000">exit()</span>

=={{header|F_Sharp|F#}}==

```fsharp
printfn "%s" "Hello world!"
```

or using .Net classes directly

```fsharp
System.Console.WriteLine("Hello world!")
```



## Factor


```factor
"Hello world!" print
```



## Falcon

With the printl() function:

```falcon
printl("Hello world!")
```

Or via "fast print":

```falcon>
 "Hello world!"
```



## FALSE


```false
"Hello world!
"
```



## Fantom



```fantom

class HelloText
{
  public static Void main ()
  {
    echo ("Hello world!")
  }
}

```



## ferite

word.}}

```ferite
uses "console";
Console.println( "Goodby, World!" );
```



## Fexl


```Fexl
say "Hello world!"
```



## Fish

Standard Hello, world example, modified for this task:

```Fish
!v"Hello world!"r!
 >l?!;o
```

Explanation of the code:<br/>
<tt>!v"</tt> jumps over the <tt>v</tt> character with the <tt>!</tt> sign, then starts the string mode with <tt>"</tt> .<br/>
Then the characters <tt>Hello world!</tt> are added, and string mode is closed with <tt>"</tt>.<br/>
The stack is reversed for printing (<tt>r</tt>), and a jump (<tt>!</tt>) is executed to jump over the <tt>!</tt> at the beginning of the line and execute the <tt>v</tt>. ([[Fish]] is torical)<br/>
After going down by <tt>v</tt>, it goes rightwards again by <tt>></tt> and this line is being executed.<br/>
This line pushes the stack size (<tt>l</tt>), and stops (<tt>;</tt>) if the top item on the stack is equal to 0 (<tt>?</tt>). Else it executes the <tt>!</tt> directly after it and jumps to the <tt>o</tt>, which outputs the top item in [http://en.wikipedia.org/wiki/Ascii ASCII]. Then the line is executed again. It effectively prints the stack until it's empty, then it terminates.


## FOCAL


```focal
TYPE "Hello, world" !
```



## Forth


```forth
." Hello world!"
```


Or as a whole program:


```forth
: goodbye ( -- )   ." Hello world!" CR ;
```



## Fortran

{{works with|F77}}
Simplest case - display using default formatting:


```fortran
print *,"Hello world!"
```


Use explicit output format:


```fortran
100   format (5X,A,"!")
      print 100,"Hello world!"
```


Output to channels other than stdout goes like this:


```fortran
write (89,100) "Hello world!"
```


uses the format given at label 100 to output to unit 89. If output unit with this number exists yet (no "<tt>OPEN</tt>" statement or processor-specific external unit setting), a new file will be created and the output sent there. On most UNIX/Linux systems that file will be named "<tt>fort.89</tt>".
{{7*7}}


## Fortress


```fortress
export Executable

run() = println("Hello world!")
```



## FreeBASIC


```FreeBASIC
? "Hello world!"
sleep
```



## Frege


{{Works with|Frege|3.20.113}}


```frege
module HelloWorld where
main _ = println "Hello world!"
```



## friendly interactive shell

Unlike other [[UNIX shell]] languages, fish doesn't support history substitution, so <code>!</code> is safe to use without quoting.

```fishshell
echo Hello world!
```



## Frink


```frink

println["Hello world!"]

```



## FunL


```funl
println( 'Hello world!' )
```



## FUZE BASIC


```qbasic
PRINT "Hello world!"
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=d3a51f670e4eb0f793b513b14329be51 Click this link to run this code]'''

```gambas
Public Sub Main()

PRINT "Hello world!"

End
```



## GAP


```gap
# Several ways to do it
"Hello world!";

Print("Hello world!\n"); # No EOL appended

Display("Hello world!");

f := OutputTextUser();
WriteLine(f, "Hello world!\n");
CloseStream(f);
```



## GB BASIC


```GB BASIC
10 print "Hello world!"
```



## gecho


```gecho
'Hello, <> 'World! print
```



## Gema

Gema ia a preprocessor that reads an input file and writes an output file.
This code will write "Hello world!' no matter what input is given.


```gema
*= ! ignore off content of input
\B=Hello world!\! ! Start output with this text.
```



## Genie


```Genie

init
    print "Hello world!"

```



## Gentee


```gentee>func hello <main

{
   print("Hello world!")
}
```



## GFA Basic


```gfabasic
PRINT "Hello World"
```



## GLBasic


```GLBasic
STDOUT "Hello world!"
```



## Glee


```glee
"Hello world!"
```


or


```glee
'Hello world!'
```


or to display with double quotes


```glee
 '"Goodbye,World!"'
```


or to display with single quotes


```glee
 "'Goodbye,World!'"
```



## Global Script


This uses the <code>gsio</code> I/O operations, which are designed to be simple to implement on top of Haskell and simple to use.

```Global Script
λ _. print qq{Hello world!\n}
```



## GlovePIE


```glovepie
debug="Hello world!"
```



## GML


```C
show_message("Hello world!"); // displays a pop-up message
show_debug_message("Hello world!"); // sends text to the debug log or IDE
```



## Go



```go
package main

import "fmt"

func main() { fmt.Println("Hello world!") }
```



## Golfscript


```golfscript
"Hello world!"
```



## Gosu


```gosu
print("Hello world!")
```



## Groovy


```groovy
println "Hello world!"
```


=={{header|GW-BASIC}}==

```qbasic
10 PRINT "Hello world!"
```



## Hack


```hack
<?hh echo 'Hello world!'; ?>
```



## Halon

If the code in run in the REPL the output will be to stdout otherwise syslog LOG_DEBUG will be used.


```halon
echo "Hello world!";
```



## Harbour


```visualfoxpro
? "Hello world!"
```



## Haskell


```haskell

main = putStrLn "Hello world!"

```



## Haxe


```ActionScript
trace("Hello world!");
```



## hexiscript


```hexiscript
println "Hello world!"
```



## HicEst


```hicest
WRITE() 'Hello world!'
```



## HLA


```HLA
program goodbyeWorld;
#include("stdlib.hhf")
begin goodbyeWorld;

  stdout.put( "Hello world!" nl );

end goodbyeWorld;
```



## HolyC


```holyc
"Hello world!\n";
```



## Hoon


```Hoon
~&  "Hello world!"  ~
```



## HPPPL


```HPPPL
PRINT("Hello world!");
```



## HQ9+

{{incorrect|HQ9+|output isn't consistent with the task's requirements (and is probably incapable of solving the task).}}

```hq9plus>H</lang

*Technically, HQ9+ can't print "Hello world!" text because of its specification.
- H : Print 'Hello World!'

- Q : Quine

- 9 : Print '99 Bottles of Beer'

- + : Increase Pointer (useless!)


## Huginn


```huginn
#! /bin/sh
exec huginn --no-argv -E "${0}" "${@}"
#! huginn

main() {
	print( "Hello World!\n" );
	return ( 0 );
}
```



## Hy


```clojure
(print "Hello world!")
```



## i


```i
software {
    print("Hello world!")
}
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
  write( "Hello world!" )
end
```



## IDL


```idl
print,'Hello world!'
```



## Inform 6


```Inform 6
[Main;
  print "Hello world!^";
];
```



## Integer BASIC

NOTE:  Integer BASIC was written (and hand-assembled by Woz himself) for the Apple 1 and original Apple 2.  The Apple 1 has NO support for lower-case letters, and it was an expensive (and later) option on the Apple 2.  This example accurately represents the only reasonable solution for those target devices, and therefore cannot be "fixed", only deleted.


```Integer BASIC
   10 PRINT "Hello world!"
   20 END
```



## Io


```Io
"Hello world!" println
```



## Ioke


```ioke
"Hello world!" println
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>PRINT "Hello world!"
```



## IWBASIC


```IWBASIC

OPENCONSOLE

PRINT"Hello world!"

'This line could be left out.
PRINT:PRINT:PRINT"Press any key to end."

'Keep the console from closing right away so the text can be read.
DO:UNTIL INKEY$<>""

CLOSECONSOLE

END

```



## J


```j
   'Hello world!'
Hello world!
```


Here are some redundant alternatives:

```J
   [data=. 'Hello world!'
Hello world!
   data
Hello world!
   smoutput data
Hello world!

   NB. unassigned names are verbs of infinite rank awaiting definition.
   NB. j pretty prints the train.
   Hello World!
Hello World !


   NB. j is glorious, and you should know this!

   i. 2 3   NB. an array of integers
0 1 2
3 4 5

   verb_with_infinite_rank =: 'Hello world!'"_

   verb_with_infinite_rank i. 2 3
Hello world!


   verb_with_atomic_rank =: 'Hello world!'"0

   verb_with_atomic_rank i. 2 3
Hello world!
Hello world!
Hello world!

Hello world!
Hello world!
Hello world!

```



## Jack


```jack
class Main {
  function void main () {
    do Output.printString("Hello world!");
    do Output.println();
    return;
  }
}
```



## Jacquard Loom

This weaves the string "Hello world!"

```jacquard
+---------------+
|               |
|    *    *     |
|*   *    *  *  |
|*           * *|
|*           * *|
|*  *         * |
|   *     *   * |
|         *     |
+---------------+

+---------------+
|               |
|*   *    *     |
|*   *    *     |
|            * *|
|            * *|
|*  *         * |
|*  *     *   * |
|         *     |
+---------------+

+---------------+
|               |
|*   **   * *   |
|*******  *** * |
| **** *   * ***|
| **** *  ******|
| ******   ** * |
|   * *   *   * |
|         *     |
+---------------+

+---------------+
|               |
|*******  *** * |
|*******  *** * |
|           ** *|
|*        *  * *|
|*******   ** * |
|*******  *** * |
|         *     |
+---------------+

+---------------+
|               |
|*******  *** * |
|*******  *** * |
|      *  *  * *|
|      *  *  * *|
|*******  **  * |
|*******  **  * |
|         *     |
+---------------+

+---------------+
|               |
|***** *  *** * |
|*******  *** * |
|     * * *  *  |
|     * *    *  |
|******   **  * |
|******   **  * |
|         *     |
+---------------+

+---------------+
|               |
|    *    * *   |
|***** *  ***** |
|***** **  * ***|
|***** **  * ***|
|*******   * ** |
|   * *   *   * |
|         *     |
+---------------+

+---------------+
|               |
|               |
|     * *       |
|     * *       |
|     *         |
|     *         |
|               |
|               |
+---------------+
```



## Java


```java
public class HelloWorld
{
 public static void main(String[] args)
 {
  System.out.println("Hello world!");
 }
}
```



## JavaScript


```javascript
document.write("Hello world!");
```


{{works with|NJS|0.2.5}}
{{works with|Rhino}}
{{works with|SpiderMonkey}}

```javascript
print('Hello world!');
```


{{works with|JScript}}

```javascript
WScript.Echo("Hello world!");
```


{{works with|Node.js}}

```javascript
console.log("Hello world!")
```



## JCL


```JCL
/*MESSAGE Hello world!
```



## Joy


```joy
"Hello world!" putchars.
```



## jq


```jq
"Hello world!"
```



## Jsish


```javascript
puts("Hello world!")
```



## Julia


```Julia
println("Hello world!")
```



## K


```K

"Hello world!"

```

Some of the other ways this task can be attached are:

```K

`0: "Hello world!\n"

```


```K

s: "Hello world!"
s

```


```K

\echo "Hello world!"

```



## Kabap


```Kabap
return = "Hello world!";
```



## Kaya


```kaya
program hello;

Void main() {
    // My first program!
    putStrLn("Hello world!");
}
```



## Kdf9 Usercode

{{incorrect|Kdf9|output isn't consistent with the task's requirements: wording, punctuation.}}

```joy


V2; W0;
RESTART; J999; J999;
PROGRAM;                   (main program);
   V0 = Q0/AV1/AV2;
   V1 = B0750064554545700; ("Hello" in Flexowriter code);
   V2 = B0767065762544477; ("World" in Flexowriter code);
   V0; =Q9; POAQ9;         (write "Hello World" to Flexowriter);
999;  OUT;
   FINISH;

```



## Kite

simply a single line

```Kite
"#!/usr/local/bin/kite

"Hello world!"|print;
```



## Kitten



```Kitten
"Hello world!" say
```



## KonsolScript

Displays it in a text file or console/terminal.

```KonsolScript
function main() {
  Konsol:Log("Hello world!")
}
```



## Kotlin


```Kotlin
fun main(args: Array<String>) {
    println("Hello world!")
}
```



## KQL



```KQL
print 'Hello world!'
```



## Lambdatalk


```scheme

Hello world!
{h1 Hello world!}
_h1 Hello world!\n

```




## Lang5


```Lang5
"Hello world!\n" .
```



## Langur


```Langur
writeln "yo, peeps"
```



## LaTeX


```LaTeX

\documentclass{scrartcl}

\begin{document}
Hello World!
\end{document}

```



## Latitude


```Latitude
putln "Hello world!".
```



## LDPL


```LDPL

procedure:
display "Hello World!" crlf

```



## Lasso

A plain string is output automatically.

```Lasso
'Hello world!'
```



## LFE


```lisp

(: io format '"Hello world!~n")

```



## LC3 Assembly



```lc3asm
.orig x3000
LEA R0, hello    ; R0 = &hello
TRAP x22         ; PUTS (print char array at addr in R0)
HALT
hello .stringz "Hello World!"
.end
```

Or (without PUTS)

```lc3asm
.orig x3000
LEA R1, hello        ; R1 = &hello
TOP LDR R0, R1, #0   ; R0 = R1[0]
BRz END              ; if R0 is string terminator (x0000) go to END
TRAP x21             ; else OUT (write char in R0)
ADD R1, R1, #1       ;      increment R1
BR TOP               ;      go to TOP
END HALT
hello .stringz "Hello World!"
.end
```



## Liberty BASIC


```lb
print "Hello world!"
```



## LIL


```tcl
#
# Hello world in lil
#

print "Hello, world!"
```



## Lily

There are two ways to do this. First, with the builtin print:


```lily
print("Hello world!")
```


Second, by using stdout directly:


```lily
stdout.print("Hello world!\n")
```



## Lilypond


```lilypond
\version "2.18.2"
global = {
  \time 4/4
  \key c \major
  \tempo 4=100
}
\relative c''{ g e e( g2)
}
\addlyrics {
  Hel -- lo,   World!
}
```



## Limbo


```limbo
implement Command;

 include "sys.m";
     sys: Sys;

 include "draw.m";

 include "sh.m";

 init(nil: ref Draw->Context, nil: list of string)
 {
     sys = load Sys Sys->PATH;
     sys->print("Hello world!\n");
 }
```



## Lingo


```lingo
put "Hello world!"
```


or:


```lingo
trace("Hello world!")
```



## Lisaac

{{works with|Lisaac|0.13.1}}
You can print to standard output in Lisaac by calling STRING.print or INTEGER.print:


```lisaac
Section Header          // The Header section is required.
  + name := GOODBYE;    // Define the name of this object.

Section Public
  - main <- ("Hello world!\n".print;);
```


However, it may be more straightforward to use IO.print_string instead:


```lisaac
Section Header          // The Header section is required.
  + name := GOODBYE2;   // Define the name of this object.

Section Public
  - main <- (IO.put_string "Hello world!\n";);
```



## Little

Output to terminal:

```c
puts("Hello world!");
```


Without the newline terminator:


```c
puts(nonewline: "Hello world!");
```


Output to arbitrary open, writable file, for example the standard error channel:

```c
puts(stderr, "Hello world!");
```



## LiveCode

Example on OS X using livecode-server in shell script

```LiveCode
#! /usr/local/bin/livecode-server
set the outputLineEndings to "lf"
put "Hello world!" & return
```


Livecode also supports stdout as a device to write to
```LiveCode
write "Hello world!" & return to stdout
```



## LLVM



```llvm

; const char str[14] = "Hello World!\00"
@.str = private unnamed_addr constant  [14 x i8] c"Hello, world!\00"

; declare extern `puts` method
declare i32 @puts(i8*) nounwind

define i32 @main()
{
  call i32 @puts( i8* getelementptr ([14 x i8]* @str, i32 0,i32 0))
  ret i32 0
}
```



## Lobster


```lobster
print "Hello world!"
```



## Logo

Print includes a line feed:

```logo
print [Hello world!]
```

Type does not:

```logo
type [Hello world!]
```



## Logtalk


```logtalk
:- object(hello_world).

    % the initialization/1 directive argument is automatically executed
    % when the object is loaded into memory:
    :- initialization(write('Hello world!\n')).

:- end_object.
```



## LOLCODE


```LOLCODE

HAI
CAN HAS STDIO?
VISIBLE "Hello world!"
KTHXBYE

```



## LotusScript


```lotusscript
:- object(hello_world).
    'This will send the output to the status bar at the bottom of the Notes client screen
    print "Hello world!"

:- end_object.
```



## LSE64


```lse64
"Hello world!" ,t nl
```



## Lua

Function calls with either a string literal or a table constructor passed as their only argument do not require parentheses.

```lua
print "Hello world!"
```


Harder way with a table:

```lua

local chars = {"G","o","o","d","b","y","e",","," ","W","o","r","l","d","!"}
for i = 1, #chars do
write(chars[i])
end

```




## Luna


```luna
def main:
    hello = "Hello, World!"
    print hello
```



## M2000 Interpreter


```M2000 Interpreter

Print "Hello World!" \\ printing on columns, in various ways defined by last $() for specific layer
Print $(4),"Hello World!" \\ proportional printing using columns, expanded to a number of columns as the length of string indicates.
Report "Hello World!"  \\ proportional printing with word wrap, for text, can apply justification and rendering a range of text lines

```



## M4

For the particular nature of m4, this is simply:

```m4
`Hello world!'
```



## Maclisp


```lisp
(format t "Hello world!~%")
```

Or

```lisp
(print "Hello world!")
```



## make

Makefile contents:

```make

all:
$(info Hello world!)

```

Running make produces:

Hello world!

make: Nothing to be done for `all'.


## Malbolge


'''Long version:'''

```malbolge
('&%:9]!~}|z2Vxwv-,POqponl$Hjig%eB@@>}=<M:9wv6WsU2T|nm-,jcL(I&%$#"
`CB]V?Tx<uVtT`Rpo3NlF.Jh++FdbCBA@?]!~|4XzyTT43Qsqq(Lnmkj"Fhg${z@>
```


'''Short version:'''

```malbolge
(=<`#9]~6ZY32Vx/4Rs+0No-&Jk)"Fh}|Bcy?`=*z]Kw%oG4UUS0/@-ejc(:'8dc
```


{{Out}}

```txt
HELLO WORLD!

```



## MANOOL

In &ldquo;applicative&rdquo; notation:

```MANOOL
{{extern "manool.org.18/std/0.3/all"} in WriteLine[Out; "Hello world!"]}
```

OOPish notation (equivalent to the above, up to Abstract Syntax Tree):

```MANOOL
{{extern "manool.org.18/std/0.3/all"} in Out.WriteLine["Hello world!"]}
```

LISPish notation (ditto):

```MANOOL
{{extern "manool.org.18/std/0.3/all"} in {WriteLine Out "Hello world!"}}
```

Using a colon punctuator (ditto):

```MANOOL
{{extern "manool.org.18/std/0.3/all"} in: WriteLine Out "Hello world!"}
```

Note that all semicolons, wherever allowed, are optional. The above example with all possible semicolons:

```MANOOL
{{extern; "manool.org.18/std/0.3/all"} in: WriteLine; Out; "Hello world!"}
```



## Maple


```Maple

> printf( "Hello world!\n" ): # print without quotes
Hello world!

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
Print["Hello world!"]
```



## MATLAB


```MATLAB>>
 disp('Hello world!')
```



## Maude


```Maude

fmod BYE-WORLD is

	protecting STRING .

	op sayBye : -> String .

	eq sayBye = "Hello world!" .

endfm

red sayBye .

```



## Maxima


```maxima
print("Hello world!");
```



## MAXScript


```maxscript
print "Hello world!"
```

or:

```maxscript
format "%" "Hello world!"
```



## MDL


```mdl
<PRINC "Hello world!">
<CRLF>
```



## Mercury


```mecury
:- module hello.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
main(!IO) :-
    io.write_string("Hello world!\n", !IO).
```



## Metafont


```metafont
message "Hello world!"; end
```



## min


```min
"Hello world!" puts
```



## MiniScript


```MiniScript
print "Hello world!"
```



## MIPS Assembly

{{works with|MARS}} and {{works with|SPIM}}

```mips
   .data #section for declaring variables
hello:  .asciiz "Hello world!" #asciiz automatically adds the null terminator. If it's .ascii it doesn't have it.

   .text # beginning of code
main: # a label, which can be used with jump and branching instructions.
   la $a0, hello # load the address of hello into $a0
   li $v0, 4 # set the syscall to print the string at the address $a0
   syscall # make the system call

   li $v0, 10 # set the syscall to exit
   syscall # make the system call
```



## mIRC Scripting Language


```mirc
echo -ag Hello world!
```



## ML/I


```ML/I
Hello world!
```


=={{header|Modula-2}}==

```modula2
MODULE Hello;
IMPORT InOut;

BEGIN
  InOut.WriteString('Hello world!');
  InOut.WriteLn
END Hello.
```


=={{header|Modula-3}}==

```modula3
MODULE Goodbye EXPORTS Main;

IMPORT IO;

BEGIN
  IO.Put("Hello world!\n");
END Goodbye.
```



## MontiLang


```MontiLang
|Hello, World!| PRINT .
```



## Morfa


```morfa

import morfa.io.print;
func main(): void
{
    println("Hello world!");
}

```



## MUF


```muf
: main[ -- ]
me @ "Hello world!" notify
exit
;
```



## MUMPS


```MUMPS
Write "Hello world!",!
```



## MyDef

Run with:
```txt
mydef_run hello.def
```

Perl:

```MyDef
$print Hello world
```


C:

```MyDef

module: c
$print Hello world

```


python:

```MyDef

module: python
$print Hello world

```


JavaScript

```MyDef

module: js
$print "Hello world"

```


go:

```MyDef

module: go
$print Hello world

```



## MyrtleScript


```MyrtleScript
script HelloWorld {
    func main returns: int {
        print("Hello World!")
    }
}

```



## MySQL



```MySQL
SELECT 'Hello world!';
```



## Mythryl



```Mythryl
print "Hello world!";
```



## N/t/roff


To get text output, compile the source file using NROFF and set output to the text terminal.  If you compile using TROFF, you will get graphical output suitable for typesetting on a graphical typesetter/printer instead.

Because /.ROFF/ is a document formatting language, the majority of input is expected to be text to output onto a medium.  Therefore, there are no routines to explicitly call to print text.


```N/t/roff
Hello world!
```



## Nanoquery


```nanoquery
println "Hello world!"
```



## Neat



```Neat
void main() writeln "Hello world!";
```



## Neko


```neko
$print("Hello world!");
```



## Nemerle


```Nemerle

class Hello
{
  static Main () : void
  {
    System.Console.WriteLine ("Hello world!");
  }
}

```

Easier method:

```Nemerle

System.Console.WriteLine("Hello world!");

```



## NetRexx


```NetRexx
say  'Hello world!'
```



## Never


```fsharp
func main() -> int {
    prints("Hello world!\n");
    0
}
```


{{out}}

```txt
prompt$ never -f hello.nev
Hello world!
```



## newLISP

{{works with|newLisp|6.1 and after}}

```lisp
(println "Hello world!")
```



## Nickle


```c
printf("Hello world!\n")
```



## Nim


```python
echo("Hello world!")
```



## Nit


```nit
print "Hello world!"
```


=={{header|NS-HUBASIC}}==
<p>As lowercase characters are not offered in NS-HUBASIC, perhaps some flexibility in the task specification could be offered.</p>
Using <code>?</code>:
<lang NS-HUBASIC>10 ? "HELLO WORLD!"
```


Using <code>PRINT</code>:
<lang NS-HUBASIC>10 PRINT "HELLO WORLD!"
```



## Nyquist

'''Interpreter:''' [[Nyquist]] (3.15)
[[Category:Nyquist Version 3.15]]

### LISP syntax


```lisp
(format t "Hello world!")
```


Or


```lisp
(print "Hello world!")
```



### SAL syntax


```SAL
print "Hello World!"
```


Or


```SAL
exec format(t, "Hello World!")
```


=={{header|Oberon-2}}==

```oberon2

MODULE Goodbye;
IMPORT Out;
  PROCEDURE World*;
  BEGIN
    Out.String("Hello world!");Out.Ln
  END World;
BEGIN
  World;
END Goodbye.

```



## Objeck


```objeck

class Hello {
  function : Main(args : String[]) ~ Nil {
    "Hello world!"->PrintLine();
  }
}
```


=={{header|Objective-C}}==
{{works with|clang-602.0.53}}

The de facto Objective-C "Hello, World!" program is most commonly illustrated as the following, using the NSLog() function:


```objc

#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSLog(@"Hello, World!");
    }
}

```


However the purpose of the NSLog() function is to print a message to standard error prefixed with a timestamp, which does not meet the most common criteria of a "Hello, World!" program of displaying only the requested message to standard output.

The following code prints the message to standard output without a timestamp using exclusively Objective-C messages:


```objc

#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSFileHandle *standardOutput = [NSFileHandle fileHandleWithStandardOutput];
        NSString *message = @"Hello, World!\n";
        [standardOutput writeData:[message dataUsingEncoding:NSUTF8StringEncoding]];
    }
}

```


Objective-C also supports functions contained within the C standard library. However, Objective-C's NSString objects must be converted into a UTF-8 string in order to be supported by the C language's I/O functions.


```objc

#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSString *message = @"Hello, World!\n";
        printf("%s", message.UTF8String);
    }
}

```



## OCaml


```ocaml
print_endline "Hello world!"
```



## Occam

{{works with|kroc}}

```occam
#USE "course.lib"
PROC main (CHAN BYTE screen!)
  out.string("Hello world!*c*n", 0, screen)
:
```



## Octave


```octave
disp("Hello world!");
```


Or, using C-style function printf:


```octave
printf("Hello world!");
```



## Oforth


```Oforth
"Hello world!" .
```



## Onyx


```onyx
`Hello world!\n' print flush
```



## OOC

To print a String, either call its println() method:

```ooc
main: func {
  "Hello world!" println()
}
```

Or call the free println() function with the String as the argument.

```ooc
main: func {
  println("Hello world!")
}
```



## ooRexx

Refer also to the [[#REXX|Rexx]] and [[#NetRexx|NetRexx]] solutions.  Simple output is common to most Rexx dialects.

```ooRexx
/* Rexx */
say 'Hello world!'

```



## OpenLisp

We can use the same code as the Common Lisp example, but as a shell script.

```openlisp

#!/openlisp/uxlisp -shell
(format t "Hello world!~%")
(print "Hello world!")

```


Output:
Hello world!
"Hello world!"


## Openscad


```openscad

echo("Hello world!");  // writes to the console
text("Hello world!");  // creates 2D text in the object space
linear_extrude(height=10) text("Hello world!"); // creates 3D text in the object space

```



## Oxygene

From [[wp:Oxygene (programming language)]]

```oxygene

namespace HelloWorld;

interface

type
  HelloClass = class
  public
    class method Main;
  end;

implementation

class method HelloClass.Main;
begin
  System.Console.WriteLine('Hello world!');
end;

end.

```


```txt

>HelloWorld.exe
Hello world!

```



## Oz


```oz
{Show "Hello world!"}
```



## PARI/GP



```parigp
print("Hello world!")
```



## Pascal

{{works with|Free Pascal}}

```pascal
program byeworld;
begin
 writeln('Hello world!');
end.
```



## PASM


```pasm
print "Hello world!\n"
end
```


=={{header|PDP-1 Assembly}}==
This can be assembled with macro1.c distributed with SIMH and then run on the SIMH PDP-1 simulator.

```assembly

hello
/ above: title line - was punched in human readable letters on paper tape
/ below: location specifier - told assembler what address to assemble to
100/
lup,	lac i ptr		/ load ac from address stored in pointer
	cli			/ clear io register
lu2,	rcl 6s			/ rotate combined ac + io reg 6 bits to the left
				/ left 6 bits in ac move into right 6 bits of io reg
	tyo			/ type out character in 6 right-most bits of io reg
	sza			/ skip next instr if accumulator is zero
	jmp lu2			/ otherwise do next character in current word
	idx ptr			/ increment pointer to next word in message
	sas end			/ skip next instr if pointer passes the end of message
	jmp lup			/ otherwise do next word in message
	hlt			/ halt machine
ptr,	msg			/ pointer to current word in message
msg,	text "hello, world"	/ 3 6-bit fiodec chars packed into each 18-bit word
end,	.         		/ sentinel for end of message
start 100			/ tells assembler where program starts

```


=={{header|PDP-11 Assembly}}==
This is Dennis Ritchie's Unix Assembly ("as"). Other PDP-11 assembly languages include PAL-11R, PAL-11S and MACRO-11.

{{works with|UNIX|6}}
{{works with|UNIX|7}}


```assembly
.globl  start
	.text
start:
        mov	$1,r0
	sys	4; outtext; outlen
	sys	1
	rts	pc

	.data
outtext: <Hello world!\n>
outlen = . - outtext
```



## Perl

{{works with|Perl|5.8.8}}

```perl
print "Hello world!\n";
```


{{works with|Perl|5.10.x}}
Backported from Perl 6:

```perl
use feature 'say';
say 'Hello world!';
```


or:

```perl
use 5.010;
say 'Hello world!';
```



## Perl 6


```perl6
say 'Hello world!';
```

In an object-oriented approach, the string is treated as an object calling its '''say()''' method:

```perl6
"Hello, World!".say();
```



## Phix


```phix
puts(1,"Hello world!")
```



## PHL


```phl
module helloworld;
extern printf;

@Integer main [
    printf("Hello world!");
    return 0;
]
```



## PHP


```php
<?php
echo "Hello world!\n";
?>
```

Alternatively, any text outside of the <code><?php ?></code> tags will be automatically echoed:

```php
Hello world!
```



## PicoLisp


```PicoLisp
(prinl "Hello world!")
```



## Pict

Using the syntax sugared version:

```pict
(prNL "Hello World!");
```

Using the channel syntax:

```pict
new done: ^[]
run ( prNL!["Hello World!" (rchan done)]
    | done?_ = () )
```



## Pikachu


```pict
pikachu pika pikachu pika pika pi pi pika pikachu pika pikachu pi pikachu pi pikachu pi pika pi pikachu pikachu pi pi pika pika pikachu pika pikachu pikachu pi pika pi pika pika pi pikachu pikachu pi pikachu pi pika pikachu pi pikachu pika pikachu pi pikachu pikachu pi pikachu pika pika pikachu pi pikachu pi pi pikachu pikachu pika pikachu pi pika pi pi pika pika pikachu pikachu pi pi pikachu pi pikachu
pikachu pikachu pi pikachu
pikachu pika pika pikachu pika pikachu pikachu pika pika pikachu pikachu pi pi pikachu pika pikachu pika pika pi pika pikachu pikachu pi pika pika pikachu pi pika pi pika pi pikachu pi pikachu pika pika pi pi pika pi pika pika pikachu pikachu pika pikachu pikachu pika pi pikachu pika pi pikachu pi pika pika pi pikachu pika pi pika pikachu pi pi pikachu pika pika pi pika pi pikachu
pikachu pikachu pi pikachu
pikachu pika pi pika pika pikachu pika pikachu pi pikachu pi pi pika pi pikachu pika pi pi pika pikachu pi pikachu pi pi pikachu pikachu pika pikachu pikachu pika pi pikachu pi pika pikachu pi pikachu pika pika pikachu pika pi pi pikachu pikachu pika pika pikachu pi pika pikachu pikachu pi pika pikachu pikachu pika pi pi pikachu pikachu pi pikachu pi pikachu pi pikachu pi pika pikachu pi pikachu pika pikachu pi pika pi pikachu
pi pika
pikachu pikachu pi pikachu
pika pi
pikachu pikachu pi pikachu
pikachu pi pikachu pi pi pikachu pi pikachu pika pikachu pikachu pi pikachu pikachu pika pi pi pika pikachu pika pikachu pi pi pikachu pika pi pi pikachu pika pika pi pika pika pikachu pika pikachu pi pi pika pikachu pika pi pikachu pikachu pi pikachu pika pikachu pikachu pika pi pi pikachu pikachu pi pika pikachu pi pikachu pika pikachu pikachu pika pi pikachu pikachu pika pikachu pi pikachu pika pika pi pikachu pi pika pi pikachu pikachu pi pikachu
pi pika
pikachu pikachu pi pikachu
pikachu pikachu pi pika pikachu pi pika pika pi pi pika pi pikachu pi pika pi pika pi pika pikachu pika pi pi pikachu pi pikachu pi pika pi pika pika pikachu pi pikachu
pikachu pikachu pi pikachu
pikachu pi pikachu pika pikachu pi pika pi pikachu pikachu pika pika pi pi pikachu pi pika pi pikachu pi pika pikachu pi pika pi pi pikachu pikachu pika pika pikachu pikachu pi pi pikachu pi pikachu pi pikachu pi pi pikachu pikachu pi pikachu pi pikachu pi pika pika pikachu pikachu pika pi pika pikachu pi pikachu pi pi pika pikachu pika pi pikachu pi pika pi pi pikachu pikachu pika pika pikachu pika pika pikachu pi pika pi pika pikachu pi pika pikachu pika pi pika pikachu
pikachu pikachu pika pikachu
pikachu pikachu pika pikachu
pi pi pikachu pi pikachu pika pika pi pikachu pika pika pi pi pika pika pikachu pi pi pikachu pi pika pi pika pikachu pi pikachu pi pikachu pikachu pi pi pika pika pi pika pika pi pika pikachu pikachu pi pikachu pika pi pi pika pi pi pikachu pikachu pika pi pi pika pika pi pika pikachu pi pikachu pi pi pika pi pika pika pikachu pika pi pika pikachu pi pikachu pikachu pi pi pika pi pika pika pikachu pikachu pi pikachu
pikachu pikachu pi pikachu
pikachu pi pikachu pikachu pika pikachu pikachu pika pika pikachu pikachu pika pikachu pi pika pikachu pika pika pi pikachu pi pi pika pi pi pikachu pika pika pikachu pikachu pika pikachu pikachu pi pika pi pi pikachu pikachu pika pi pi pikachu pikachu pika pikachu pika pi pikachu pi pika pi pika pikachu pika pi pikachu pi pikachu pikachu pi pika pikachu pi pikachu pikachu pi pika pi pikachu pikachu pi pikachu pika pika pi pi pikachu
pikachu pi pi pika pi pi pikachu pika pikachu pikachu pika pika pi pi pika pikachu pi pikachu pi pi pika pi pika pi pi pika pikachu pi pika pi pikachu pika pikachu pika pi pi pika pi pi pikachu pi pikachu pikachu pika pi pikachu pi pi pika pi pikachu pi pi pika pi pi pikachu pika pikachu pika pikachu pika pi pikachu pikachu pi pi pika pika pikachu
pikachu pikachu pi pikachu
pikachu pikachu pika pikachu
```



## Pike


```pike
int main(){
   write("Hello world!\n");
}
```



## PILOT


```pilot
T:Hello world!
```



## PIR


```pir
.sub hello_world_text :main
	print "Hello world!\n"
.end
```



## PL/I


```pli
goodbye:proc options(main);
     put list('Hello world!');
end goodbye;
```



## PL/M

Assuming the existence of a WRITE$STRING library routine.

```plm
HELLO_WORLD: DO;
    /* external I/O routines */
    WRITE$STRING: PROCEDURE( S ) EXTERNAL; DECLARE S POINTER; END WRITE$STRING;
    /* end external routines */
    MAIN: PROCEDURE;
        CALL WRITE$STRING( @( 'Hello world!', 0AH, 0 ) );
    END MAIN;
END HELLO_WORLD;

```



## PL/SQL

{{works with|Oracle}}

```plsql

set serveroutput on

BEGIN
  DBMS_OUTPUT.PUT_LINE('Hello world!');
END;
/

```



```txt

SQL> set serveroutput on
SQL>
SQL> BEGIN
  2    DBMS_OUTPUT.PUT_LINE('Hello world!');
  3  END;
  4  /
Hello world!

PL/SQL procedure successfully completed.

```



## Pony


```pony
actor Main
  new create(env: Env) =>
    env.out.print("Hello world!")
```



## Pop11


```pop11
printf('Hello world!\n');
```



## PostScript


To generate a document that shows the text "Hello world!":


```postscript
%!PS
/Helvetica 20 selectfont
70 700 moveto
(Hello world!) show
showpage
```


If the viewer has a console, then there are the following ways to display the topmost element of the stack:


```postscript
(Hello world!) ==
```


will display the <i>string</i> "(Hello world!)";


```postscript
(Hello world!) =
```


will display the <i>content</i> of the string "(Hello world!)"; that is, "Hello world!";


```postscript
(Hello world!) print
```


will do the same, without printing a newline. It may be necessary to provoke an error message to make the console pop up. The following program combines all four above variants:


```postscript
%!PS
/Helvetica 20 selectfont
70 700 moveto
(Hello world!) dup dup dup
= print == % prints three times to the console
show % prints to document
1 0 div % provokes error message
showpage
```



## Potion


```potion
"Hello world!\n" print
```



## PowerBASIC


```powerbasic
#COMPILE EXE
#COMPILER PBCC 6

FUNCTION PBMAIN () AS LONG
  CON.PRINT "Hello world!"
  CON.WAITKEY$
END FUNCTION
```



## PowerShell


```powershell
'Hello world!'

#It's considered good practice to use Write-Host, although it works just fine without too
Write-Host 'Hello world!'

# For extra flair, you can specify colored output
Write-Host 'Hello world!' -foregroundcolor red
```



## Processing


```processing
println("Hello world!");
```



## ProDOS


```ProDOS
printline Hello world!
```



## Prolog


```prolog
:- write('Hello world!'), nl.
```



## PSQL

  EXECUTE BLOCK
    RETURNS(S VARCHAR(40))
  AS
  BEGIN
    S = 'Hello world!';
    SUSPEND;
  END


## Pure


```pure

using system;

puts "Hello world!\n" ;

```



## PureBasic


```PureBasic
OpenConsole()
PrintN("Hello world!")
Input() ; Wait for enter
```



## Python

{{works with|Python|2.4}}

```python
print "Hello world!"
```


The same using sys.stdout

```python
import sys
sys.stdout.write("Hello world!\n")
```


In Python 3.0, print is changed from a statement to a function.

{{works with|Python|3.0}} (And version 2.X too).

```python
print("Hello world!")
```


'''An easter egg'''

```python
import __hello__
```



```python
import __phello__
```



```python
import __phello__.spam
```



## Quill


```quill
"Hello world!" print
```



## Quite BASIC

  'See Quite BASIC web application http://www.quitebasic.com/prj/basics/helloworld/

```Quite BASIC
10 PRINT "Hello World!"
20 PRINT "Hello" ; " World!"
30 PRINT "Hello" ;
40 PRINT " World!
```



## R


```R
 cat("Hello world!\n")
```

or

```R
 message("Hello world!")
```

or

```R
 print("Hello world!")
```



## Ra


```Ra

class HelloWorld
	**Prints "Hello world!"**

	on start

		print "Hello world!"

```



## Racket


```Racket

(printf "Hello world!\n")

```



## Raven


```raven
'Hello world!' print
```



## REALbasic

{{works with|REALbasic|5.5}}
This requires a console application.


```realbasic
Function Run(args() as String) As Integer
  Print "Hello world!"
  Quit
End Function
```



## REBOL


```REBOL
print "Hello world!"
```



## RED


```RED
print "Hello world!"
```



## Retro



```Retro

'Hello_world! s:put

```



## REXX


### using SAY


```rexx
/*REXX program to show a line of text.  */
say 'Hello world!'
```



### using SAY variable


```rexx
/*REXX program to show a line of text.  */
yyy = 'Hello world!'
say yyy
```



### using LINEOUT


```rexx
/*REXX program to show a line of text.  */

call lineout ,"Hello world!"
```



## RTL/2


```RTL/2
TITLE Goodbye World;

LET NL=10;

EXT PROC(REF ARRAY BYTE) TWRT;

ENT PROC INT RRJOB();

    TWRT("Hello world!#NL#");
    RETURN(1);

ENDPROC;
```



## Ring


```ring
See "Hello world!"
```

=={{header|Risc-V}}==
<lang Risc-V>.data
hello:
.string "Hello World!\n\0"
.text
main:
la a0, hello
li a7, 4
ecall
li a7, 10
ecall

```



## Ruby

{{works with|Ruby|1.8.4}}

```ruby
puts "Hello world!"
```

or

```ruby
$stdout.puts "Hello world!"
```

or even

```ruby
 STDOUT.write "Hello world!\n"
```



'''Using the > global'''

```ruby
$>.puts "Hello world!"
```


```ruby
$>.write "Hello world!\n"
```



## Run BASIC


```Runbasic
print "Hello world!"
```



## Rust


```rust

fn main() {
   print!("Hello world!");
}

```


or


```rust

fn main() {
   println!("Hello world!");
}

```



## Salmon


```Salmon
"Hello world!"!
```


or


```Salmon
print("Hello world!\n");
```


or


```Salmon
standard_output.print("Hello world!\n");
```



## SAS


```sas
/* Using a data step. Will print the string in the log window */
data _null_;
put "Hello world!";
run;
```



## SASL

Note that a string starts with a single and ends with a double quote

```SASL

'Hello World!",nl

```



## Sather


```sather
class GOODBYE_WORLD is
 main is
  #OUT+"Hello world!\n";
 end;
end;
```



## Scala

{{libheader|Console}}

### Ad hoc REPL solution

Ad hoc solution as [http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop REPL] script. Type this in a REPL session:

```Scala
println("Hello world!")
```


### Via Java runtime

This is a call to the Java run-time library. '''Not recommended'''.

```Scala
System.out.println("Hello world!")
```



### Via Scala Console API

This is a call to the Scala run-time library. '''Recommended'''.

```Scala
println("Hello world!")
```



### Short term deviation to out


```Scala
Console.withErr(Console.out) { Console.err.println("This goes to default _out_") }
```


### Long term deviation to out


```Scala
  Console.err.println ("Err not deviated")
  Console.setErr(Console.out)
  Console.err.println ("Err deviated")
  Console.setErr(Console.err) // Reset to normal
```



## Scheme

{{works with|Gauche}}
{{works with|Guile}}
{{works with|Chicken Scheme}}

```scheme
(display "Hello world!")
(newline)
```

{{works with|Gauche}}
{{works with|Chicken Scheme}}

```scheme
(print "Hello world!")
```

or just:

```scheme
"Hello world!"
```

(should work on any scheme)


###  R7RS Scheme


```scheme
(import (scheme base)
        (scheme write))
(display "Hello world!")
(newline)
```



## Scilab


```Scilab
disp("Hello world!");
```



## sed


```sed
i\
Hello world!
q
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln("Hello world!");
  end func;
```



## Self


```self
'Hello world!' printLine.
```



## Set lang

<lang set_lang>set ! H
set ! E
set ! L
set ! L
set ! O
set ! 32
set ! W
set ! O
set ! R
set ! L
set ! D
set ! 33
```



## SETL


```SETL
print("Hello world!");
```



## SETL4


```SETL4
out("Hello world!");end
```



## Shen


```Shen
(output "Hello world!~%")
```



## Shiny


```shiny
say 'Hello world!'
```



## Sidef


```sidef
„Hello world!”.say;
```



## SIMPOL


```simpol
function main()
end function "Hello world!{d}{a}"
```



## Simula

{{works with|SIMULA-67}}

```simula
BEGIN
   OUTTEXT("Hello world!");
   OUTIMAGE
END
```



## Sisal


```sisal
define main

% Sisal doesn't yet have a string built-in.
% Let's define one as an array of characters.

type string = array[character];

function main(returns string)
  "Hello world!"
end function
```



## SkookumScript


```javascript
print("Hello world!")
```

Alternatively if just typing in the SkookumIDE [http://skookumscript.com/docs/v3.0/ide/console/workspace/ REPL]:

```javascript
"Hello world!"
```



## Slate


```slate
inform: 'Hello world!'.
```



## Smalltalk


```smalltalk
Transcript show: 'Hello world!'; cr.
```


{{works with|GNU Smalltalk}} (as does the above code)

```smalltalk
'Hello world!' printNl.
```



## smart BASIC


```qbasic
PRINT "Hello world!"
```



## SmileBASIC


```smilebasic
PRINT "Hello world!"
```



## SNOBOL4

Using CSnobol4 dialect

```snobol4
    OUTPUT = "Hello world!"
END
```



## SNUSP


### Core SNUSP


```snusp
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/
```



### Modular SNUSP


```snusp
@\G.@\o.o.@\d.--b.@\y.@\e.>@\comma.@\.<-@\W.+@\o.+++r.------l.@\d.>+.! #
 |   |     \@------|#  |    \@@+@@++|+++#-    \\               -
 |   \@@@@=+++++#  |   \===--------!\===!\-----|-------#-------/
 \@@+@@@+++++#     \!#+++++++++++++++++++++++#!/
```



## SoneKing Assembly


```soneking assembly

extern print

dv Msg Goodbye,World!

mov eax Msg
push
call print
pop

```



## SPARC Assembly


```sparc

	.section	".text"
	.global		_start
_start:
	mov	4,%g1			! 4 is SYS_write
	mov	1,%o0			! 1 is stdout
	set	.msg,%o1		! pointer to buffer
	mov	(.msgend-.msg),%o2	! length
	ta	8

	mov	1,%g1			! 1 is SYS_exit
	clr	%o0			! return status is 0
	ta	8

.msg:
	.ascii	"Hello world!\n"
.msgend:

```



## Sparkling


```sparkling
print("Hello world!");
```



## SPL


```spl
#.output("Hello world!")
```



## SQL

{{works with|Oracle}}
{{works with|Db2 LUW}}

```sql

select 'Hello world!' text from dual;

```



```txt

SQL>select 'Hello world!' text from dual;
TEXT
------------
Hello world!

```



## SQL PL

{{works with|Db2 LUW}}
With SQL only:

```sql pl

SELECT 'Hello world!' AS text FROM sysibm.sysdummy1;

```

Output:

```txt

db2 -t
db2 => SELECT 'Hello world!' AS text FROM sysibm.sysdummy1;

TEXT
------------
Hello world!

  1 record(s) selected.

```


{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

SET SERVEROUTPUT ON;

CALL DBMS_OUTPUT.PUT_LINE('Hello world!');

```

Output:

```txt

db2 -t
db2 => SET SERVEROUTPUT ON
DB20000I  The SET SERVEROUTPUT command completed successfully.
db2 => CALL DBMS_OUTPUT.PUT_LINE('Hello world!')

  Return Status = 0

Hello world!

```



## Standard ML


```sml
print "Hello world!\n"
```



## Stata


```stata
display "Hello world!"
```



## Suneido


```Suneido
Print("Hello world!")
```



## Swift

{{works with|Swift|2.x+}}

```swift
print("Hello world!")
```

{{works with|Swift|1.x}}

```swift
println("Hello world!")
```



## Symsyn


```symsyn

 'hello world' []

```



## Tailspin


```tailspin
'Hello World' -> !OUT::write
```



## Tcl

Output to terminal:

```tcl
puts "Hello world!"
```


Output to arbitrary open, writable file:

```tcl
puts $fileID "Hello world!"
```



## Teco

Outputting to terminal. Please note that ^A means control-A, not a caret followed by 'A', and that $ represent the ESC key.

```teco
^AHello world!^A$$
```



## Tern


```tern
println("Hello world!");
```



## Terra


```terra
C = terralib.includec("stdio.h")

terra hello(argc : int, argv : &rawstring)
  C.printf("Hello world!\n")
  return 0
end
```



## TestML


```TestML
%TestML 0.1.0
Print("Hello world!")
```


=={{header|TI-83 BASIC}}==

```ti83b
Disp "Hello world!
```

(Lowercase letters DO exist in TI-BASIC, though you need an assembly program to enable them.)

=={{header|TI-89 BASIC}}==

```ti89b
Disp "Hello world!"
```



## TorqueScript


```tqs
echo("Hello world!");
```



## TPP


```tpp
Hello world!
```


=={{header|Transact-SQL}}==


```sql
PRINT "Hello world!"
```



## TransFORTH


```forth
PRINT " Hello world! "
```



## Trith


```trith
"Hello world!" print
```



## True BASIC


```truebasic

! In True BASIC all programs run in their own window. So this is almost a graphical version.
PRINT "Hello world!"
END

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
PRINT "Hello world!"

```

Output:

```txt

Hello world!

```



## Uniface


```Uniface

message "Hello world!"

```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
#!/bin/sh
echo "Hello world!"
```


=
## C Shell
=

```csh
#!/bin/csh -f
echo "Hello world!\!"
```


We use \! to prevent history substitution. Plain ! at end of string seems to be safe, but we use \! to be sure.


## Unlambda


```unlambda
`r```````````````.G.o.o.d.b.y.e.,. .W.o.r.l.d.!i
```



## Ursa


```ursa
out "hello world!" endl console
```



## Ursala

output as a side effect of compilation

```Ursala
#show+

main = -[Hello world!]-
```

output by a compiled executable

```Ursala
#import std

#executable ('parameterized','')

main = <file[contents: -[Hello world!]-]>!
```


=={{header|உயிர்/Uyir}}==
<lang உயிர்/Uyir>முதன்மை என்பதின் வகை எண் பணி {{
         ("உலகத்தோருக்கு வணக்கம்") என்பதை திரை.இடு;

         முதன்மை = 0;
}};
```



## V


```v
"Hello world!" puts
```



## Vala


```vala
void main(){
	stdout.printf("Hello world!\n");
}
```


## VAX Assembly



```VAX Assembly

desc:  .ascid "Hello World!"      ;descriptor (len+addr) and text
.entry hello, ^m<>                ;register save mask
       pushaq desc                ;address of descriptor
       calls #1, g^lib$put_output ;call with one argument on stack
       ret                        ;restore registers, clean stack & return
.end hello                        ;transfer address for linker

```


## VBScript

{{works with|Windows Script Host|5.7}}

```VBScript
WScript.Echo "Hello world!"
```



## Vedit macro language


```vedit
Message("Hello world!")
```



## Verbexx


```Verbexx
@SAY "Hello world!";
```



## VHDL


```VHDL
LIBRARY std;
USE std.TEXTIO.all;

entity test is
end entity test;

architecture beh of test is
begin
  process
    variable line_out : line;
  begin
    write(line_out, string'("Hello world!"));
    writeline(OUTPUT, line_out);
    wait; -- needed to stop the execution
  end process;
end architecture beh;
```



## Vim Script


```vim
echo "Hello world!\n"
```



## Visual Basic

{{Libheader|Microsoft.Scripting}}
{{works with|Visual Basic|VB6 Standard}}
Visual Basic 6 is actually designed to create GUI applications, however with a little help from the Microsoft.Scripting Library it is fairly easy to write a simple console application.

```vb
Option Explicit

Private Declare Function AllocConsole Lib "kernel32.dll" () As Long
Private Declare Function FreeConsole Lib "kernel32.dll" () As Long
'needs a reference set to "Microsoft Scripting Runtime" (scrrun.dll)

Sub Main()
  Call AllocConsole
  Dim mFSO As Scripting.FileSystemObject
  Dim mStdIn As Scripting.TextStream
  Dim mStdOut As Scripting.TextStream
  Set mFSO = New Scripting.FileSystemObject
  Set mStdIn = mFSO.GetStandardStream(StdIn)
  Set mStdOut = mFSO.GetStandardStream(StdOut)
  mStdOut.Write "Hello world!" & vbNewLine
  mStdOut.Write "press enter to quit program."
  mStdIn.Read 1
  Call FreeConsole
End Sub
```



## Visual Basic .NET


```vb
Imports System

Module HelloWorld
    Sub Main()
        Console.WriteLine("Hello world!")
    End Sub
End Module
```



## Viua VM assembly

<lang>.function: main/0
    text %1 local "Hello World!"
    print %1 local
    izero %0 local
    return
.end
```



## Vlang


```vlang
println('Hello World!')
```



## Wart



```wart
prn "Hello world!"
```



## WDTE


```WDTE
io.writeln io.stdout 'Hello world!';
```



## Wee Basic


```Wee Basic
print 1 "Hello world!"
end
```



## Whenever



```whenever
1 print("Hello world!");
```



## Whiley



```whiley
import whiley.lang.System

method main(System.Console console):
    console.out.println("Hello world!")
```



## Whitespace

There is a "Hello World" - example-program on the [http://compsoc.dur.ac.uk/whitespace/hworld.ws Whitespace-website]


## Wolfram Language


```wolfram
Print["Hello world!"]
```



## Wren


```wren
System.print("Hello world!")
```



## X86 Assembly

{{works with|nasm|2.05.01}}

This is known to work on Linux, it may or may not work on other Unix-like systems

Prints "Hello world!" to stdout (and there is probably an even simpler version):

```asm
section .data
msg     db      'Hello world!', 0AH
len     equ     $-msg

section .text
global  _start
_start: mov     edx, len
        mov     ecx, msg
        mov     ebx, 1
        mov     eax, 4
        int     80h

        mov     ebx, 0
        mov     eax, 1
        int     80h
```


'''AT&T syntax:''' works with gcc (version 4.9.2) and gas (version 2.5):


```asm
.section .text

.globl main

main:
	movl $4,%eax	#syscall number 4
	movl $1,%ebx	#number 1 for stdout
	movl $str,%ecx	#string pointer
	movl $16,%edx	#number of bytes
	int $0x80	#syscall interrupt
	ret

.section .data
str: .ascii "Hello world!\12"
```


'''AT&T syntax (x64):'''
<lang>// No "main" used
// compile with `gcc -nostdlib`
#define SYS_WRITE   $1
#define STDOUT      $1
#define SYS_EXIT    $60
#define MSGLEN      $14

.global _start
.text

_start:
    movq    $message, %rsi          // char *
    movq    SYS_WRITE, %rax
    movq    STDOUT, %rdi
    movq    MSGLEN, %rdx
    syscall                         // sys_write(message, stdout, 0x14);

    movq    SYS_EXIT, %rax
    xorq    %rdi, %rdi              // The exit code.
    syscall                         // exit(0)

.data
message:    .ascii "Hello, world!\n"
```



## XBasic

{{works with|Windows XBasic}}

```xbasic

PROGRAM "hello"
VERSION "0.0003"

DECLARE FUNCTION Entry()

FUNCTION Entry()
  PRINT "Hello World"
END FUNCTION
END PROGRAM

```



## xEec


```xEec

h#10 h$! h$d h$l h$r h$o h$w h#32
h$o h$l h$l h$e h$H >o o$ p jno

```



## XL


```XL
use XL.UI.CONSOLE
WriteLn "Hello world!"
```



## XLISP


```xlisp
(DISPLAY "Hello world!")
(NEWLINE)
```



## XPL0


```XPL0
code Text=12;
Text(0, "Hello world!
")
```



## XSLT


```xml><xsl:text
Hello world!
</xsl:text>
```



## Yorick


```yorick
write, "Hello world!"
```



## Z80 Assembly


Using the Amstrad CPC firmware:


```z80
org		$4000

txt_output:	equ	$bb5a

		push	hl
		ld	hl,world

print:		ld	a,(hl)
		cp	0
		jr	z,end
		call	txt_output
		inc	hl
		jr	print

end:		pop	hl
		ret

world:		defm	"Hello world!\r\n\0"
```



## zkl


```zkl
println("Hello world!");
```

{{omit from|VBA|VBA can't write or output to console}}


## ZX Spectrum Basic


```zxbasic
10 print "Hello world!"
```

