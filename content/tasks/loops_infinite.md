+++
title = "Loops/Infinite"
description = ""
date = 2019-10-10T19:11:59Z
aliases = []
[extra]
id = 2817
[taxonomies]
categories = ["task", "Iteration"]
tags = []
languages = [
  "360_assembly",
  "4dos_batch",
  "6502_assembly",
  "6800_assembly",
  "8th",
  "acl2",
  "actionscript",
  "ada",
  "agena",
  "aime",
  "algol_60",
  "algol_68",
  "algol_w",
  "amigae",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "arnoldc",
  "arturo",
  "autohotkey",
  "awk",
  "axe",
  "basic",
  "batch_file",
  "bbc_basic",
  "bc",
  "beeswax",
  "befunge",
  "blz",
  "bracmat",
  "brat",
  "c",
  "c_shell",
  "chapel",
  "chuck",
  "clojure",
  "cobol",
  "coffeescript",
  "coldfusion",
  "comal",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dc",
  "dcl",
  "delphi",
  "dwscript",
  "dyalect",
  "e",
  "edsac_order_code",
  "ela",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "es",
  "euphoria",
  "factor",
  "false",
  "fantom",
  "fish",
  "forth",
  "fortran",
  "fortress",
  "freebasic",
  "frink",
  "futurebasic",
  "gambas",
  "gap",
  "gb_basic",
  "gml",
  "go",
  "groovy",
  "halon",
  "haskell",
  "hexiscript",
  "hicest",
  "holyc",
  "idl",
  "intercal",
  "io",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "labview",
  "lang5",
  "lasso",
  "liberty_basic",
  "lily",
  "lingo",
  "lisaac",
  "livecode",
  "logo",
  "lolcode",
  "lua",
  "m2000_interpreter",
  "m4",
  "make",
  "maple",
  "maxima",
  "maxscript",
  "metafont",
  "microsoft_small_basic",
  "min",
  "monte",
  "montilang",
  "moo",
  "mumps",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "occam",
  "octave",
  "oforth",
  "ol",
  "opl",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pilot",
  "pl_i",
  "plaintex",
  "pop11",
  "postscript",
  "powershell",
  "prolog",
  "pure_data",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "red",
  "related_tasks",
  "retro",
  "rexx",
  "ring",
  "robotic",
  "ruby",
  "run_basic",
  "rust",
  "salmon",
  "sather",
  "scala",
  "scheme",
  "scilab",
  "sed",
  "seed7",
  "self",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "snusp",
  "sparkling",
  "spin",
  "spl",
  "sql_pl",
  "standard_ml",
  "stata",
  "swift",
  "systemverilog",
  "tcl",
  "torquescript",
  "trith",
  "tuscript",
  "unix_shell",
  "unixpipes",
  "unlambda",
  "ursa",
  "v",
  "vala",
  "vax_assembly",
  "vedit_macro_language",
  "visual_basic",
  "visual_basic_dotnet",
  "wart",
  "wee_basic",
  "x86_assembly",
  "xlisp",
  "xpl0",
  "z80_assembly",
  "zkl",
]
+++

## Task

Print out       <big> '''SPAM''' </big>       followed by a   ''newline''   in an infinite loop.


## Related tasks

*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## 360 Assembly

This for sure will result in a severe WTO buffer shortage.

```360 Assembly

INFINITE CSECT ,                       this PGM control section
INFINITE AMODE 31                      addressing mode 31 bit
INFINITE RMODE ANY                     loader can load either 24 or 31
         BAKR  14,0                    stack caller's register contents
         LR    12,15                   establish base
         LA    13,0                    no savearea
         USING INFINITE,12             base to assembler
         LA    10,1                    1 in reg 10
         LA    11,2                    2 in reg 11
LOOP     EQU   *
         CR    10,11                   1==2?
         BE    RETURN                  Yes, exit.
        WTO    'SPAM',ROUTCDE=11       print SPAM to syslog
         B     LOOP                    No, check again.
RETURN   PR    ,                       return to caller
         END   INFINITE

```



## 4DOS Batch



```4dos
@echo off
do forever
  echo SPAM
enddo
```



## 6502 Assembly

Specific OS/hardware routines for printing are left unimplemented.

```6502asm
InfiniteLoop	LDX #0
PrintLoop:	LDA MSG,x
		JSR PrintAccumulator	;routine not implemented
		INX
		CPX #5
		BNE PrintLoop
		BEQ InfiniteLoop

MSG		.byte "SPAM", $0A
```



## 6800 Assembly

<lang>        .cr  6800
        .tf  spam6800.obj,AP1
        .lf  spam6800
;
### ===============================================
;
;       Infinite SPAM loop for the Motorola 6800      ;
;                 by barrym 2013-04-10                ;
;-----------------------------------------------------;
; Prints the message "SPAM" repeatedly to an ascii    ;
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
        bra  main       ;else restart
;
### ===============================================
;
string  .as  "SPAM",#13,#10,#0
        .en
```



## 8th

One way:

```forth

: inf "SPAM\n" . recurse ;

```

Another way:

```forth

: inf repeat "SPAM\n" . again ;

```


## ACL2


```Lisp
(defun spam ()
   (declare (xargs :mode :program))
   (if nil
       nil
       (prog2$ (cw "SPAM~%")
               (spam))))
```



## ActionScript


```actionscript
while (true) {
    trace("SPAM");
}
```



## Ada


```ada
loop
   Put_Line("SPAM");
end loop;
```



## Agena

Tested with Agena 2.9.5 Win32

```agena
do
    print( "SPAM" )
od
```



## Aime


```aime
while (1) {
    o_text("SPAM\n");
}
```



## ALGOL 60

'''Based on the 1962 Revised Repport on ALGOL''':
  '''begin'''
    '''integer''' i;
    '''for''' i:=1 '''step''' 0 '''until''' 2 '''do'''
      outtext("spam")
  '''end'''
```algol60
'BEGIN' 'COMMENT' Loops/Infinite - Algol60 - 23/06/2018;
  'INTEGER' I;
  'FOR' I := 1 'STEP' 0 'UNTIL' 2 'DO'
    OUTSTRING(1,'('SPAM')')
'END'
```



## ALGOL 68


```algol68
DO
  printf($"SPAM"l$)
OD
```

Or the classic "dynamic halt":

```algol68
loop x:
   printf($"SPAM"l$);
loop x
```



## ALGOL W


```algolw
begin
    for i := 1 step 0 until 2 do write( "SPAM" )
end.
```



## AmigaE


```amigae
PROC main()
  LOOP
    WriteF('SPAM')
  ENDLOOP
ENDPROC
```


## AppleScript


```applescript
repeat
  log "SPAM"
end repeat
```



## ARM Assembly

<lang ARM_Assembly>
.global main

main:

loop:
    ldr r0, =message
    bl printf
    b loop

message:
    .asciz "SPAM\n"

```



## ArnoldC


```ArnoldC
IT'S SHOWTIME
STICK AROUND @NO PROBLEMO
TALK TO THE HAND "SPAM"
CHILL
YOU HAVE BEEN TERMINATED
```



## Arturo


```arturo
loop true {
	print "SPAM"
}
```



## AutoHotkey


```autohotkey
Loop
  MsgBox SPAM `n
```



## AWK


```awk
BEGIN {
  while(1) {
    print "SPAM"
  }
}
```



## Axe

Warning: running this program will cause you to need to reset your calculator, thereby losing any user data stored in RAM.


```axe
While 1
 Disp "SPAM",i
End
```



## BASIC

Old-fashioned syntax:

```qbasic
while 1
  print "SPAM"
wend
```


Standard BASIC:

```qbasic
do
  print "SPAM"
loop
```


Also

```qbasic
for i = 1 to 10 step 0
  print "SPAM"
next i
```


```txt

10 PRINT "SPAM"
20 GOTO 10

```

Rather than a GOTO, instead we can use a FOR statement:

```txt

10 FOR I = 1 TO 10 STEP 0: REM A zero step makes the loop infinite
20 PRINT "SPAM"
30 NEXT I

```


=
## Applesoft BASIC
=

```ApplesoftBasic
FOR I = 0 TO 1 STEP 0 : PRINT "SPAM" : NEXT
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 DO
110   PRINT "SPAM"
120 LOOP
```



## Batch File

Using <code>goto</code>:

```dos
@echo off
:loop
echo SPAM
goto loop
```

Another variant which uses Windows NT's <code>for</code> statement:

```dos
for /l %%x in (1,0,2) do @echo SPAM
```

This essentially is a counted loop which starts at <code>1</code>, increments by <code>0</code> and stops when the counter reaches <code>2</code>.


## BBC BASIC


```bbcbasic
      REPEAT
        PRINT "SPAM"
      UNTIL FALSE
```



## bc


```bc
while (1) "SPAM
"
```



## beeswax


```beeswax
_>`SPA`p
  bN`M`<
```



## Befunge

Because the 2-D code space is toroidal, all loops are infinite
unless explicitly stopped with '''@'''.

```befunge
55+"MAPS",,,,,
```



## blz


```blz
while true
    print("SPAM")
end
```


=={{header|Brainfuck}}==
Optimized for code size:

```bf
++++++++++[->++++++>++++++++>+<<<]>+++++>
[+++.---.<.>---.+++>.<]
```


Optimized for execution speed:

```bf
10++++++++++
[-> 8++++++++ > 8++++++++ > 6++++++ > 8++++++++ > 1+ <<<<<]>
83+++ > 80 > 65+++++ > 77--- <<<
[.>.>.>.>.<<<<]
```



## Bracmat


```bracmat
whl'out$SPAM
```



## Brat


```brat
loop { p "SPAM" }
```



## C


```c
while(1) puts("SPAM");
```

or

```c
 for(;;) puts("SPAM");
```

or

```c
do { puts("SPAM"); } while(1);
```

or

```c
while(puts("SPAM"));
```

or

```c

spam: puts("SPAM");
goto spam;

```



## ChucK

<lang>
while(true) <<<"SPAM">>>;

```



## C++

```cpp
while (true)
  std::cout << "SPAM\n";
```

or

```cpp
for (;;)
  std::cout << "SPAM\n";
```

or

```cpp
do
  std::cout << "SPAM\n";
while (true);
```


## C#


```c#
while (true)
{
    Console.WriteLine("SPAM");
}
```



## Chapel


```chapel
while true do writeln("SPAM");
```



## ColdFusion

This will result in a JRun Servlet Error and heap dump.

With tags:

```cfm
<cfloop condition = "true NEQ false">
  SPAM
</cfloop>
```

With script:

```cfm><cfscript

  while( true != false )
  {
    writeOutput( "SPAM" );
  }
</cfscript>
```



## Clojure


```lisp
(loop [] (println "SPAM") (recur))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Spam.

       PROCEDURE DIVISION.
           PERFORM UNTIL 1 <> 1
               DISPLAY "SPAM"
           END-PERFORM

           GOBACK
           .
```

[[OpenCOBOL]] supports a <code>FOREVER</code> clause for <code>PERFORM</code> which will have the same effect.


## CoffeeScript


```coffeescript
loop
  console.log 'SPAM'

```




## Comal


```Comal
LOOP
   PRINT "SPAM"
ENDLOOP
```



## Common Lisp


```lisp
(loop (write-line "SPAM"))
```



## D

Some common ways to create an infinite printing loop:

```d
import std.stdio;

void main() {
    while (true)
        writeln("SPAM");
}
```



```d
import std.stdio;

void main() {
    do
        writeln("SPAM");
    while (true);
}
```



```d
import std.stdio;

void main() {
    for ( ; ; )
        writeln("SPAM");
}
```



```d
import std.stdio;

void main() {
    LOOP:
    writeln("SPAM");
    goto LOOP;
}
```



## Dart

<lang>
main() {
  while(true) {
    print("SPAM");
  }
}

```



## dc


```dc
[[SPAM
]P dx]dx
```


This loop is a tail-recursive function.
The program pushes the function on the stack,
the outer ''dx'' makes the first call,
and the inner ''dx'' makes each recursive call.

## DCL


```DCL
$ loop:
$  write sys$output "SPAM"
$  goto loop
```


=={{header|Déjà Vu}}==

```dejavu
while true:
	!print "SPAM"
```

Infinite recursion thanks to tail calls:

```dejavu
labda:
	!print "SPAM"
	recurse
call
```



## Delphi



```Delphi
while True do Writeln('SPAM');
```



## DWScript



```Delphi
while True do
   PrintLn('SPAM');
```



## Dyalect



```Dyalect
while true {
    print("SPAM")
}
```



## E



```e
while (true) {
    println("SPAM")
}
```



```e
def f() {
    println("SPAM")
    f <- ()
}
f <- ()
```


The difference between these is that in the second,
other activities can be interleaved with the loop;
in the first, no other processing will occur in this vat.


## EDSAC order code

The EDSAC instruction set does not include an unconditional jump: it is necessary to synthesize it by using either an <code>E</code> "branch on accumulator sign bit clear" or <code>F</code> "branch on accumulator sign bit set" order, in circumstances where the condition is guaranteed to be met. For this specific task, guaranteeing it is trivial: printing characters does not change the contents of the accumulator at all. The solution presented here, however, is more general. We use a <code>T</code> "transfer and clear" order to store the accumulator's contents in storage address <i>θ</i>+17, then jump back to the beginning of the loop and reload the accumulator with an <code>A</code> "add" order. Note that the storage address used as a temporary variable should be set to zero on entry to the loop.

```edsac
[ Infinite loop

### =======


  A program for the EDSAC

  Works with Initial Orders 2 ]

        T56K
        GK

        O10@  [ letter shift ]

[  1 ]  A17@  [ a += C(17@) ]
        O11@
        O12@
        O13@
        O14@
        O15@
        O16@
        T17@  [ C(17@) = a; a = 0 ]
        E1@   [ if a >= 0 goto 1@ ]

[ 10 ]  *F
[ 11 ]  SF
[ 12 ]  PF
[ 13 ]  AF
[ 14 ]  MF
[ 15 ]  @F    [ carriage return ]
[ 16 ]  &F    [ line feed ]

[ 17 ]  PF

        EZPF
```



## Ela



### Direct Approach



```ela
open monad io

loop () = do
  putStrLn "SPAM"
  loop ()

loop () ::: IO
```


===Non-strict version===


```ela
open monad io

xs = "SPAM"::xs

takeit 0 _ = do return ()
takeit num (x::xs) = do
  putStrLn x
  takeit (num - 1) xs

_ = takeit 10 xs ::: IO
```



## Elena

ELENA 4.x:

```elena
public program()
{
    while (true)
    {
        console.writeLine:"spam"
    }
}
```



## Elixir


```elixir
defmodule Loops do
  def infinite do
    IO.puts "SPAM"
    infinite
  end
end

Loops.infinite
```

or

```elixir
Stream.cycle(["SPAM"]) |> Enum.each(&IO.puts &1)
```



## Erlang


```erlang

-module (main).
-export ([main/0]).

main() ->
  io:fwrite( "SPAM~n" ),
  main().

```



## Emacs Lisp

This is run in an external file.

```elisp

#!/usr/bin/env emacs --script

(while (princ "SPAM\n"))

```



## ERRE


```ERRE

LOOP
  PRINT("SPAM")
END LOOP

```

You can use also WHILE TRUE..END WHILE or REPEAT...UNTIL FALSE loops.


## Euphoria


```Euphoria

while 1 do
    puts(1, "SPAM\n")
end while

```


=={{header|F Sharp|F#}}==

```fsharp

// Imperative Solution
while true do
    printfn "SPAM"

// Functional solution
let rec forever () : unit =
    printfn "SPAM"
    forever ()

```



## Factor

Tail recursion:

```factor
: spam ( -- ) "SPAM" print spam ;
```

Looping combinators:

```factor
[ "SPAM" print t ] loop
```


```factor
USE: combinators.extras
[ "SPAM" print ] forever
```



## FALSE


```false
[1]["SPAM
"]#
```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    while (true)
    {
      echo ("SPAM")
    }
  }
}

```



## Fish


```fish
a"MAPS"ooooo
```



## Forth


```forth
: email   begin ." SPAM" cr again ;
```



## Fortran

FORTRAN 77

```fortran

   10 WRITE(*,*) 'SPAM'
      GO TO 10
      END

```


Fortran 90

```fortran

program spam
  implicit none
  do
    write(*,*) 'SPAM'
  end do
end program spam

```



## Fortress


```fortress

component loops_infinite
  export Executable
  run() = while true do
    println("SPAM")
  end
end

```



## FreeBASIC


```freebasic
' FB 1.05.0

Do
  Print "SPAM"
Loop
```



## Frink


```frink

while true
   println["SPAM"]

```



## FutureBasic

Loop de loop -- whose great idea was this?

```futurebasic

include "ConsoleWindow"

while 1
print "Spam"
wend

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=e63dcdc939f8a7dcfd3194d884f8d4a5 Click this link to run this code]'''

```gambas
Public Sub Main()

Do
  Print "SPAM"
Loop

End
```



## GAP


```gap
while true do
    Print("SPAM\n");
od;
```



## GB BASIC


```GB BASIC
10 print "SPAM"
20 goto10
```



## GML


```GML
while(1)
    show_message("SPAM")
```



## Go


```go
package main

import "fmt"

func main() {
	for {
		fmt.Printf("SPAM\n")
	}
}
```



## Groovy


```groovy
while (true) {
 println 'SPAM'
}
```



## Halon


```halon
forever {
    echo "SPAM";
}
```

or (due to optimizations, these are equally fast)

```halon
while (true) {
    echo "SPAM";
}
```



## Haskell


```haskell
forever (putStrLn "SPAM")
```

or

```haskell
import Control.Monad.Fix (fix)
fix (putStrLn "SPAM" >>)
```



## hexiscript


```hexiscript
while true; println "SPAM"; endwhile
```



## HicEst


```hicest
DO i = 1, 1E20 ! for i with 16 or more digits:  i == i + 1 == loop infinite
    WRITE() "SPAM"
ENDDO
```



## HolyC


```holyc
while(1) Print("SPAM\n");
```


=={{header|Icon}} and {{header|Unicon}}==
There are several ways to write infinite loops in Icon.  The  most straightforward would be with repeat.

```icon
procedure main()
   repeat write("SPAM")
end
```


Alternately one could use one of these:

```icon
until &fail do write("SPAM")   # always fails, needs succeed to break
...
while write("SPAM")            # always succeeds, needs failure to break
...
every write(|"SPAM")           # generator always succeeds, needs failure to break
...
while write(|"SPAM")           # this is a common mistake that results in an endless loop
...
while write(1 to 5)            # a clearer version of the same mistake that generates endless 1's
```



## IDL


```IDL
while 1 do print,'SPAM'
```



## Intercal

Assuming Turing Text I/O with 8-bit ASCII-compatible character set, using COME FROM:


```intercal
       NOTE THIS IS INTERCAL
       PLEASE ,1 <- #5
       DO ,1 SUB #1 <- #54
       DO ,1 SUB #2 <- #192
       DO ,1 SUB #3 <- #136
       PLEASE ,1 SUB #4 <- #208
       DO ,1 SUB #5 <- #98
       DO COME FROM (1)
       DO READ OUT ,1
(2)    DO ,1 SUB #1 <- #134
(1)    PLEASE ABSTAIN FROM (2)
```



## Io


```io
loop("SPAM" println)
```



## J



```j
(-[smoutput bind 'SPAM')^:_(1)
```


Alternatively,


```j
smoutput bind 'SPAM'^:1e99 ''
```


This second implementation relies on numeric inaccuracies in IEEE floating point notation.
For example, 1+1e98 is exactly equal to 1e98.
That said, 1e98 iterations would still be significantly longer than the practical life of any machine anyone would care to dedicate to this task.


## Java


```java
while (true) {
   System.out.println("SPAM");
}
```



```java
for (;;) {
   System.out.println("SPAM");
}
```



## JavaScript


```javascript
for (;;) console.log("SPAM");
```


```javascript
while (true) console.log("SPAM");
```



## Joy


```joy
DEFINE loop == [true []] dip while.

["SPAM\n" putchars] loop.
```



## jq


```jq
recurse("SPAM")
```

 "SPAM"
 "SPAM"
 ...

To suppress the quotation marks, invoke jq with the -r option.


## Jsish


```javascript
for (;;) puts('SPAM');
```



## Julia


```Julia

while true
    println("SPAM")
end

```

```txt

SPAM
SPAM
SPAM
SPAM
SPAM
SPAM
SPAM

```

and so on until ^C


## K


```K
   while[1; `0:"SPAM\n"]
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    while (true) println("SPAM")
}
```



## LabVIEW

## Lang5


```lang5
do "SPAM\n" . loop
```



## Lasso


```Lasso
// not wise to run this!
while(1 > 0) => {^
	'SPAM\r'
^}
```



## Liberty BASIC

<CTRL><Break> is used to terminate such loops.

```lb

while 1
  print "SPAM"
wend
end

```



## Lily


```lily

while 1: print("SPAM")

```



## Lingo


```lingo
repeat while TRUE
  put "SPAM"
end repeat
```



## Lisaac

<pre lang=Lisaac>
{ "SPAM\n".print; }.endless_loop;

```



## LiveCode


```LiveCode
repeat forever
  put "SPAM" & return
end repeat
```



## Logo


```logo
forever [print "SPAM]
```



## LOLCODE


```lolcode
HAI
  CAN HAS STDIO?
  IM IN YR LOOP
    VISIBLE "SPAM"
  IM OUTTA YR LOOP
KTHXBYE
```



## Lua


```lua

while true do
  print("SPAM")
end

--Another solution
repeat
  print("SPAM")
until false

```



## M2000 Interpreter

All loops can stop using Esc or Ctrl+C or Break (the last two open dialog box to stop or continue). Using Escape Off we make Esc not work for breaking execution.
If Esc works then Ctrl + Y (and other letters except C, A, Z, X, N, M. F, L), open Control form, which we can do: Next Step, Slow Flow, Stop, and we can show code,current stack, variables, or execute immediate statements. This works only in console, not in M2000 forms.


```M2000 Interpreter

Module CheckIt {
      Print "SPAM"
      loop
}
Checkit

```

Using a Repeat (or Do) - Always block

```M2000 Interpreter

Module CheckIt {
      Repeat {
            Print "SPAM"
      } Always
}
Checkit

```


Printing text rendering using Report.

```M2000 Interpreter

Module CheckIt {
      \\ stop in every 2/3 of cosole lines
      \\ press spacebar or mouse button to continue
      Report Format$("Spam\n")
      Loop
}
Checkit
\\ using multiline string, replace report from module above
Report {SPAM
            }

```




## M4


```M4
define(`spam',`SPAM
spam')
spam
```



## Make


```make
spam:
   @echo SPAM
   $(MAKE)
```



## Maple


```Maple

> do print(SPAM) end;

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
While[True,
 Print@"SPAM";
 ]
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
while true
    fprintf('SPAM\n')
end
```



## Maxima


```maxima
do(disp("SPAM"));
```


## MAXScript


```maxscript
while true do print "SPAM\n"
```



## Metafont



```metafont
forever: message "SPAM"; endfor end
```



## Microsoft Small Basic

With <code>While</code>.

```microsoftsmallbasic

While "True"
  TextWindow.WriteLine("SPAM")
EndWhile

```

With <code>Goto</code>.

```microsoftsmallbasic

loopStart:
TextWindow.WriteLine("SPAM")
Goto loopStart

```



## min

```min
(true) ("SPAM" puts!) while
```


=={{header|MK-61/52}}==
<lang>1	2	3	4	С/П	БП	00
```


''Note'': because this device has no text output instead of "SPAM" was used the number (1234).

=={{header|Modula-2}}==

```modula2
LOOP
  InOut.WriteString ("SPAM");
  InOut.WriteLn
END;
```


=={{header|Modula-3}}==

```modula3
LOOP
  IO.Put("SPAM\n");
END;
```



## Monte



```Monte

while (true):
    traceln("SPAM")

```



## MOO


```moo
while (1)
  player:tell("SPAM");
endwhile
```


## MUMPS


```MUMPS

 FOR  WRITE "SPAM",!

```



## MontiLang


```MontiLang
WHILE TRUE
    |SPAM| PRINT .
ENDWHILE
```

Note that <code>TRUE</code> is simply a variable equal to 1. <code>WHILE 1</code>, any number larger than 0 or any string with a length more than 0 would also work


## Nemerle


```Nemerle
while (true) WriteLine("SPAM");
```

Or, using recursion:

```Nemerle
def loop() : void
{
    WriteLine("SPAM");
    loop();
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/Infinite'

  loop label spam forever
    say 'SPAM'
    end spam

```



## NewLISP


```NewLISP
(while (println "SPAM"))
```



## Nim


```nim
while true:
  echo "SPAM"
```


=={{header|NS-HUBASIC}}==
Using <code>FOR</code>:
<lang NS-HUBASIC>10 FOR I=0 TO 1 STEP 0
20 PRINT "SPAM"
30 NEXT
```


Using <code>GOTO</code>:
<lang NS-HUBASIC>10 PRINT "SPAM"
20 GOTO 10
```


Using <code>RUN</code>:
<lang NS-HUBASIC>10 PRINT "SPAM"
20 RUN
```


=={{header|Oberon-2}}==

```oberon2

MODULE InfiniteLoop;
IMPORT
  Out;
BEGIN
  LOOP
    Out.String("SPAM");Out.Ln
  END
END InfiniteLoop.

```



## Objeck


```objeck

while(true) {
  "SPAM"->PrintLine();
};

```



## OCaml


```ocaml
while true do
  print_endline "SPAM"
done
```


or


```ocaml
let rec inf_loop() =
  print_endline "SPAM";
  inf_loop()
in
inf_loop()
```


Seen like this it looks like the "too much functional" danger when a "while" loop looks far simpler, but the functional loop may be useful to provide data to the next loop without using mutable variable.


## Occam


```occam
#USE "course.lib"
PROC main (CHAN BYTE screen!)
  WHILE TRUE
    out.string("SPAM*c*n", 0, screen)
:
```



## Octave


```octave
while(1)
  disp("SPAM")
endwhile
```



## Oforth



```Oforth
begin "SPAM" . again
```



## Ol


```scheme

(let loop ()
   (display "SPAM")
   (loop))

```



## OPL


```opl
PROC main:
  LOCAL loop%
  loop%=1
  while loop%=1
  PRINT "SPAM"
  ENDWH
ENDP
```



## Oz


```oz
for do
   {Show 'SPAM'}
end
```



## PARI/GP


```parigp
while(1,
  print("SPAM")
);
```


For a shorter version, note that <code>print</code> returns <code>gnil</code> which is evaluated as <code>false</code>.
A 'cheating' solution might use <code>print(SPAM)</code> on the hope that the variable SPAM is uninitialized and hence prints as the monomial in itself.
But with the <code>'</code> operator that evaluation can be forced, regardless of the current value (if any) of that variable:

```parigp
until(print('SPAM),)
```



## Pascal


```pascal
while true do
  writeln('SPAM');
```

Alternatively:

```pascal
repeat
  writeln('SPAM')
until false;
```



## Perl


```perl
while(1){
    print "SPAM\n";
}
```


or equivalently


```perl
print "SPAM\n" while 1;
```



## Perl 6

```perl6
loop {
    say 'SPAM';
}
```

In addition, there are various ways of writing lazy, infinite lists in Perl 6:

```perl6
print "SPAM\n" xx *;      # repetition operator
print "SPAM\n", ~* ... *; # sequence operator
map {say "SPAM"}, ^Inf;   # upto operator
```



## Phix


```Phix
while 1 do
    puts(1,"SPAM\n")
end while
```



## PHP


```php
while(1)
    echo "SPAM\n";
```



## PicoLisp


```PicoLisp
(loop (prinl "SPAM"))
```



## Pike


```pike
int main(){
   while(1) write("SPAM\n");
}
```



## PILOT


```pilot
*TypeSpam
type:SPAM
jump:*TypeSpam
```



## PL/I


```PL/I

do forever;
   put list ('SPAM'); put skip;
end;

```



## plainTeX

Compile in console mode, with, e.g. "pdftex <file name>".

```tex
\newlinechar`\^^J
\def\spam{\message{SPAM^^J}\spam}%
\spam
```



## Pop11


```pop11
while true do
    printf('SPAM', '%p\n');
endwhile;
```



## PostScript

simple infinite loop:

```postscript
{}loop
```


A bit more complex infinite loop:

```postscript
/go {
  /spam
     { (SPAM\n) print flush }
  bind def % bind and define spam

  { spam } % procedure that will be executed by loop and will call spam to print
  loop % the loop
}

%start spamming!
go
```



## PowerShell


```powershell
for () {
    "SPAM"
}
```



## Prolog


```prolog
repeat, write('SPAM'), nl, fail.
```



## Pure Data


Screenshot: https://i.imgur.com/IrwaafZ.png


```Pure Data
#N canvas 426 88 450 300 10;
#X obj 17 75 print;
#X msg 17 55 SPAM;
#X obj 17 35 metro 1;
#X msg 17 15 1;
#X connect 1 0 0 0;
#X connect 2 0 1 0;
#X connect 3 0 2 0;
```


Notes: the loop is started by clicking the |1(, a [loadbang] could additionally be used. An [until] object, sent a bang, will loop forever, but will hang Pure Data, whereas a high-speed metro will function perfectly.


## PureBasic


### Repeat/Forever


```PureBasic
Repeat
  PrintN("SPAM")
ForEver
```



### Goto


```PureBasic
PrintIt:
PrintN("SPAM")
Goto PrintIt
```



## Python

In Python 2:

```python
while 1:
   print "SPAM"
```


In python 3:

```python
while 1:
   print("SPAM")
```


Note: one can also use: "True" or any other non-false value.
In Python the following values are false: 0, "" (empty string), (,) and {} and [] (empty tuples, dictionaries or lists), ''None'' (the special object), and the ''False'' object.
Any non-empty collection or string or non-zero numeric value is considered "True".
However, according to [http://wiki.python.org/moin/PythonSpeed#Takeadvantageofinterpreteroptimizations Python Wiki], for Python versions 2.3+ this variant is optimized by the interpreter and thus is the fastest.


## R

Note that the default R Gui buffers outputs before pushing them to the screen.
To see this run either run in terminal mode, right click on the GUI window and deselect "Buffered Output" prior to execution, or add a call to flush.console() in the loop.


```R
repeat print("SPAM")
```



## Racket



```racket

#lang racket

;; Using recursion
(define (loop)
  (displayln "SPAM")
  (loop))

(loop)

;; Using a for loop
(for ([i (in-naturals)])
  (displayln "SPAM"))

```



## REBOL


```REBOL
forever [print "SPAM"]
```



## Red


```Red
forever [
print "SPAM"
]
```



## Retro


```Retro
[ "SPAM\n" puts -1 ] while
```



## REXX


### simple


```rexx
/*REXX program displays the  word      SPAM      forever.               */

  do forever
  say 'SPAM'
  end   /*DO forever*/
                                       /*control will never reach here. */
                                       /*don't stick a fork in it.      */
```



### esoteric


```rexx
/*REXX program displays the  word      SPAM      forever.               */

   do  while  1==1                      /*esoteric  "forever"  clause.   */
   say 'SPAM'
   end   /*DO while 1==1*/
                                        /*control will never reach here. */
                                        /*don't stick a fork in it.      */
```



### GO TO version


```rexx
/*REXX program displays the  word      SPAM      forever.               */

tell_it:    say 'SPAM'
signal tell_it                         /*REXX's version of a  GO TO     */

                                       /*control will never reach here. */
                                       /*don't stick a fork in it.      */
```


### too clever by half


```rexx
/*REXX program displays the  word      SPAM      forever.               */

  do  until  0>1                       /*too-clever-by-half forever loop*/
  say 'SPAM'
  end   /*DO until 0>1*/
                                       /*control will never reach here. */
                                       /*don't stick a fork in it.      */
```



## Ring


```ring

while true
      see "Spam"
end

```



## Robotic

This will display the word '''SPAM''' at the bottom of the screen indefinitely:

```robotic

: "infinite_loop"
* "SPAM"
goto "infinite_loop"

```



## Ruby


```ruby
loop {puts "SPAM"}

```



## Rust


```rust
fn main() {
    loop {
        println!("SPAM");
    }
}
```



## Run BASIC


```runbasic
[loop] print "Spam" :goto [loop]

while 1
print "Spam"
wend
```



=={{header|S-lang}}==
<lang S-lang>forever print("SPAM");
```



## Salmon


```Salmon
while (true)
    "SPAM"!;
```



## Sather


```sather
class MAIN is
  main is
    loop
      #OUT + "Spam\n";
    end;
  end;
end;
```



## Scala


```scala
while (true)
  println("SPAM")
```



## Scheme


```scheme
((lambda (x) (display "SPAM") (newline) (x x))
 (lambda (x) (display "SPAM") (newline) (x x)))

```


or, less Schemishly but with less redundancy:


```scheme
(do () (#f) (display "SPAM") (newline))
```



## Scilab

<lang>while %T
    printf("SPAM\n")
end
```

```txt
SPAM
SPAM
SPAM
SPAM
...
```



## sed


```sed
:loop
s/.*/SPAM/
p
t loop
```

Sed requires at least one line of input to execute, so run as follows:

```txt
echo | sed ':loop;s/.*/SPAM/;p;t loop'
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    while TRUE do
      writeln("SPAM");
    end while;
  end func;
```



## Self


```self
['SPAM' printLine] loop
```



## Sidef


```ruby
loop { say "SPAM!" };
```



## Slate


```slate
[inform: 'SPAM'] loop
```



## Smalltalk


```smalltalk
[ true ] whileTrue: [ 'SPAM' displayNl ]
```



## SNOBOL4


```snobol
loop output = "SPAM" :(loop)
end
```



## SNUSP


```snusp>@\>@\>@\>@\
++++++++++===!/ < < < < \
 |  |  |  \M=@@@@+@+++++# \.>.>.>.>./
 |  |  \A=@@+@@@@+++#
 |  \P=@@+@@+@@+++#
 \S=@@+@+@@@+++#
```



## Sparkling


```sparkling
while true {
    print("SPAM");
}
```


or


```sparkling
do {
    print("SPAM");
} while true;
```


or


```sparkling
for var b = true; b; b = true {
    printf("SPAM\n");
}
```


etc.


## Spin

```spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main
  ser.start(31, 30, 0, 115200)

  repeat
    ser.str(string("SPAM",13,10))

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```



## SPL


```spl>

  #.output("SPAM")
<
```



## Standard ML


```sml
while true do
  print "SPAM\n";
```


or


```sml
let
  fun inf_loop () = (
    print "SPAM\n";
    inf_loop ()
  )
in
  inf_loop ()
end
```


Seen like this it looks like the "too much functional" danger when a "while" loop looks far simpler, but the functional loop may be useful to provide data to the next loop without using mutable variable.


## Stata



```stata
while 1 {
        display "SPAM"
}
```



###  Mata


```stata
while (1) printf("SPAM\n")
```


Also possible with a '''[https://www.stata.com/help.cgi?m2_for for]''' loop, but unlike C, the middle expression is not optional:


```stata
for (;1;) printf("SPAM\n")
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON@

BEGIN
 DECLARE I SMALLINT DEFAULT 1;
 WHILE (I = I) DO
  CALL DBMS_OUTPUT.PUT_LINE('SPAM');
 END WHILE;
END @

```

Output:

```txt

db2 -td@
db2 => SET SERVEROUTPUT ON@

db2 => BEGIN
...
db2 (cont.) => END @
DB21034E  The command was processed as an SQL statement because it was not a
valid Command Line Processor command.  During SQL processing it returned:
SQL20511N  There is not enough available space in the "DBMS_OUTPUT" message
buffer.  SQLSTATE=54035

SPAM
SPAM
SPAM
SPAM
...

```



## Swift


```swift
while true {
    println("SPAM")
}
```



## SystemVerilog



```SystemVerilog
program main;
  initial forever $display("SPAM");
endprogram

```


=={{header|Transact-SQL}}==


```sql
WHILE 1=1 BEGIN
 PRINT "SPAM"
END
```



## Tcl


```tcl
while true {
    puts SPAM
}
# or
for {} 1 {} {
    puts SPAM
}
```


=={{header|TI-83 BASIC}}==

There are a few ways to achieve this in TI-83 BASIC


```ti83b

  :Lbl 1
  :Disp "SPAM
  :Goto 1

```


Another way is by using a While loop


```ti83b

  :While 1
  :Disp "SPAM
  :End

```


=={{header|TI-89 BASIC}}==


```ti89b
Loop
  Disp "SPAM"
EndLoop
```



## TorqueScript


```Torque
While(1)
    echo("SPAM");
```



## Trith


```trith
["SPAM" print] loop
```



## TUSCRIPT

TUSCRIPT has no infinite loop. 999999999 loops are the limit.

```tuscript

$$ MODE TUSCRIPT
LOOP/999999999
print "spam"
ENDLOOP

```



## UNIX Shell

Use any of these loops:

```bash>while :; do echo SPAM; done</lang



```bash
while true; do echo "SPAM"; done
```



```bash
until false; do echo "SPAM"; done
```


```bash
for ((;;)); do echo "SPAM"; done
```


=
## C Shell
=

```bash
while (1)
	echo SPAM
end
```


=
## es
=

```es
forever {echo SPAM}
```



## UnixPipes


```bash>yes SPAM</lang



## Unlambda


```unlambda
 ``ci``s``s`kr``s``s``s``s`k.S`k.P`k.A`k.Mii
```



## Ursa

```ursa
while true
	out "SPAM" endl console
end while
```



## V


```v
true [
   'SPAM' puts
] while
```



## Vala


```vala
for(;;) stdout.printf("SPAM\n");
```


```vala
while(true) stdout.printf("SPAM\n");
```


```vala
do stdout.printf("SPAM\n"); while(true);
```


## VAX Assembly


```VAX Assembly
                               0000  0000     1 .entry	main,0
                   4D415053 8F   DD  0002     2 	pushl	#^a"SPAM"		;string on stack
                            5E   DD  0008     3 	pushl	sp			;reference to string
                            04   DD  000A     4 	pushl	#4			;+length = descriptor
                                     000C     5 loop:
                            5E   DD  000C     6 	pushl	sp			;descriptor by reference
              00000000'GF   01   FB  000E     7 	calls	#1, g^lib$put_output	;show message
                            F5   11  0015     8 	brb	loop			;forever
                                     0017     9
                                     0017    10 .end	main
```



## Vedit macro language


```vedit
while (1) {
    Message("Spam\n")
}
```

or:

```vedit
do {
    Message("Spam\n")
} while (1)
```

or:

```vedit
for (;1;) {
    Message("Spam\n")
}
```

"Nearly infinite" loop can be done by using constant ALL (=1073741824) as repeat count:

```vedit
Repeat (ALL) {
    Message("Spam\n")
}
```



## Visual Basic


```vb
Do
    Debug.Print("SPAM")
Loop
```



## Visual Basic .NET

'''Platform:''' [[.NET]]
```vbnet
Do
    Console.WriteLine("SPAM")
Loop
```



## Wart


```wart
repeat :forever
  prn "spam"
```



## Wee Basic


```Wee Basic
let loop=1
while loop=1
print 1 "SPAM"
wend
end
```



## X86 Assembly

```asm

section .text
	global _start

	_start:
		mov edx, len
		mov ecx, msg
		mov ebx, 1
		mov eax, 4
		int 0x80
		jmp _start

section .data
msg		db "SPAM",0xa
len		equ $-msg

```



## XLISP

It is of course possible to use a <code>WHILE</code> loop with a condition that will always evaluate to true:

```lisp
(defun keep-printing-spam ()
    (while t
        (display "SPAM")
        (newline) ) )
```

Although this idiom is very common (in many programming languages), however, it feels a bit like a misuse of a looping construct that is meant to be conditional. If an unconditional jump is really what we want, then that is what we have <tt>goto</tt> for; or rather, in XLISP we do not have <tt>goto</tt> (in so many words) but we can achieve the effect of it using tail recursion.

```lisp
(defun keep-printing-spam ()
    (display "SPAM")
    (newline)
    (keep-printing-spam) )
```



## XPL0


```XPL0
code Text=12;
loop Text(0, "SPAM
")
```



## Z80 Assembly


Using the Amstrad CPC firmware:


```z80
org		$4000

txt_output:	equ	$bb5a

start:		ld	hl,spam

print:		ld	a,(hl)
		cp	0
		jr	z,start
		call	txt_output
		inc	hl
		jr	print

spam:		defm	"SPAM\r\n\0"
```



## zkl


```zkl
while(1)   { println("SPAM") }
while(True){ println("SPAM") }
foreach _ in ([0..]){ println("SPAM") }
[0..].pump(Console.println,T(Void,"SPAM"));
[0..].pump(fcn{ println("SPAM") });
fcn{ println("SPAM"); return(self.fcn()) }(); // tail recursive lambda
```


