+++
title = "Loops/N plus one half"
description = ""
date = 2019-10-08T08:42:37Z
aliases = []
[extra]
id = 2829
[taxonomies]
categories = ["task", "Iteration"]
tags = []
languages = [
  "360_assembly",
  "acl2",
  "ada",
  "aime",
  "algol_60",
  "algol_68",
  "algol_w",
  "amigae",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "autoit",
  "awk",
  "axe",
  "basic",
  "bbc_basic",
  "bc",
  "befunge",
  "bracmat",
  "c",
  "chapel",
  "clojure",
  "cobol",
  "coffeescript",
  "coldfusion",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "delphi",
  "dwscript",
  "e",
  "echolisp",
  "edsac_order_code",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "falcon",
  "false",
  "fantom",
  "fbsl",
  "forth",
  "fortran",
  "freebasic",
  "futurebasic",
  "gambas",
  "gap",
  "gml",
  "go",
  "gosu",
  "groovy",
  "haskell",
  "hexiscript",
  "hicest",
  "holyc",
  "idl",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "labview",
  "lang5",
  "lasso",
  "lhogho",
  "liberty_basic",
  "lisaac",
  "livecode",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "make",
  "maple",
  "mathematica",
  "maxscript",
  "metafont",
  "microsoft_small_basic",
  "mumps",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oz",
  "panda",
  "pari_gp",
  "pascal",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pl_i",
  "pop11",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "salmon",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "smalltalk",
  "snobol4",
  "snusp",
  "spin",
  "stata",
  "swift",
  "tcl",
  "tuscript",
  "unix_shell",
  "unixpipes",
  "ursa",
  "v",
  "vba",
  "vedit_macro_language",
  "vim_script",
  "visual_basic_dotnet",
  "wart",
  "wee_basic",
  "xpl0",
  "zkl",
]
+++

Quite often one needs loops which, in the last iteration, execute only part of the loop body.


;Goal:
Demonstrate the best way to do this.


## Task

Write a loop which writes the comma-separated list
 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
using separate output statements for the number
and the comma from within the body of the loop.


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
*   [[Loops/Wrong ranges]]





## 360 Assembly


```360asm

*        Loops/N plus one half     13/08/2015
LOOPHALF CSECT         USING  LOOPHALF,R12
         LR     R12,R15
BEGIN    LA     R3,MVC
         SR     R5,R5
         LA     R6,1
         LA     R7,10
LOOPI    BXH    R5,R6,ELOOPI       for i=1 to 10
         XDECO  R5,XDEC
         MVC    0(4,R3),XDEC+8
         LA     R3,4(R3)
         CH     R5,=H'10'
         BNL    NEXTI
         MVC    0(2,R3),=C', '
         LA     R3,2(R3)
NEXTI    B      LOOPI              next i
ELOOPI   XPRNT  MVC,80
         XR     R15,R15
         BR     R14
MVC      DC     CL80' '
XDEC     DS     CL12
         YREGS
         END    LOOPHALF

```

```txt

   1,    2,    3,    4,    5,    6,    7,    8,    9,   10

```




## ACL2

ACL2 does not have loops, but this is close:


```Lisp
(defun print-list (xs)
   (progn$ (cw "~x0" (first xs))
           (if (endp (rest xs))
               (cw (coerce '(#\Newline) 'string))
               (progn$ (cw ", ")
                       (print-list (rest xs))))))
```



## Ada


```ada
with Ada.Text_IO;

procedure LoopsAndHalf is
begin
  for i in 1 .. 10 loop
    Ada.Text_IO.put (i'Img);
    exit when i = 10;
    Ada.Text_IO.put (",");
  end loop;
  Ada.Text_IO.new_line;
end LoopsAndHalf;
```



## Aime


```aime
integer i;

i = 0;
while (1) {
    i += 1;
    o_integer(i);
    if (i == 10) {
        break;
    }
    o_text(", ");
}
o_text("\n");
```



## ALGOL 60

```algol60
'BEGIN'
  'COMMENT' Loops N plus one half - Algol60 - 20/06/2018;
  'INTEGER' I;
  'FOR' I:=1 'STEP' 1 'UNTIL' 10 'DO' 'BEGIN'
    OUTINTEGER(1,I);
    'IF' I 'NOTEQUAL' 10 'THEN' OUTSTRING(1,'(', ')')
  'END'
'END'
```

```txt

         +1  ,          +2  ,          +3  ,          +4  ,          +5  ,          +6  ,          +7  ,          +8  ,         +9  ,         +10

```



## ALGOL 68

There are three common ways of achieving n+½ loops:
{|border="1" style="border-collapse: collapse;"
||
```algol68
  FOR i WHILE
    print(whole(i, -2));
# WHILE # i < 10 DO
    print(", ")

  OD;

  print(new line)
```

||
```algol68
FOR i TO 10 DO
  print(whole(i, -2));
  IF i < 10 THEN
    print(", ")
  FI
OD;

print(new line)
```

||
```algol68
FOR i DO
  print(whole(i, -2));
  IF i >= 10 THEN GO TO done FI;
  print(", ")

OD;
done:
print(new line)
```

|}
Output for all cases above:

```txt

 1,  2,  3,  4,  5,  6,  7,  8,  9, 10

```



## ALGOL W


```algolw
begin
    integer i;
    i := 0;
    while
        begin
           i := i + 1;
           writeon( i );
           i < 10
        end
    do
    begin
       writeon( "," )
    end
end.
```



## AmigaE


```amigae
PROC main()
  DEF i
  FOR i := 1 TO 10
    WriteF('\d', i)
    EXIT i = 10
    WriteF(', ')
  ENDFOR
ENDPROC
```



## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program loopnplusone.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessResult:      .ascii ""                    @ message result
sMessValeur:       .fill 11, 1, ' '
szMessComma:       .asciz ","
szCarriageReturn:  .asciz "\n"
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                       @ entry of program
    mov r4,#1                               @ loop counter
1:                                          @ begin loop
    mov r0,r4
    ldr r1,iAdrsMessValeur                  @ display value
    bl conversion10                         @ decimal conversion
    ldr r0,iAdrszMessResult
    bl affichageMess                        @ display message
    ldr r0,iAdrszMessComma
    bl affichageMess                        @ display comma
    add r4,#1                               @ increment counter
    cmp r4,#10                              @ end ?
    blt 1b                                  @ no ->begin loop one
    mov r0,r4
    ldr r1,iAdrsMessValeur                  @ display value
    bl conversion10                         @ decimal conversion
    ldr r0,iAdrszMessResult
    bl affichageMess                        @ display message
    ldr r0,iAdrszCarriageReturn
    bl affichageMess                        @ display return line

100:                                        @ standard end of the program
    mov r0, #0                              @ return code
    mov r7, #EXIT                           @ request to exit program
    svc #0                                  @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszMessResult:         .int szMessResult
iAdrszMessComma:          .int szMessComma
iAdrszCarriageReturn:     .int szCarriageReturn
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                   @ save  registres
    mov r2,#0                               @ counter length
1:                                          @ loop length calculation
    ldrb r1,[r0,r2]                         @ read octet start position + index
    cmp r1,#0                               @ if 0 its over
    addne r2,r2,#1                          @ else add 1 in the length
    bne 1b                                  @ and loop
                                            @ so here r2 contains the length of the message
    mov r1,r0                               @ address message in r1
    mov r0,#STDOUT                          @ code to write to the standard output Linux
    mov r7, #WRITE                          @ code call system "write"
    svc #0                                  @ call systeme
    pop {r0,r1,r2,r7,lr}                    @ restaur registers */
    bx lr                                   @ return
/******************************************************************/
/*     Converting a register to a decimal                                 */
/******************************************************************/
/* r0 contains value and r1 address area   */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                         @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:                                          @ start loop
    bl divisionpar10                        @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                              @ digit
    strb r1,[r3,r2]                         @ store digit on area
    sub r2,#1                               @ previous position
    cmp r0,#0                               @ stop if quotient = 0
    bne 1b                                  @ else loop
                                            @ end replaces digit in front of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]                         @ store in area begin
    add r4,#1
    add r2,#1                               @ previous position
    cmp r2,#LGZONECAL                       @ end
    ble 2b                                  @ loop
    mov r1,#0                               @ final zero
    strb r1,[r3,r4]
100:
    pop {r1-r4,lr}                          @ restaur registres
    bx lr                                   @return
/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10:
  /* r0 contains the argument to be divided by 10 */
    push {r2-r4}                           @ save registers  */
    mov r4,r0
    mov r3,#0x6667                         @ r3 <- magic_number  lower
    movt r3,#0x6666                        @ r3 <- magic_number  upper
    smull r1, r2, r3, r0                   @ r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0)
    mov r2, r2, ASR #2                     @ r2 <- r2 >> 2
    mov r1, r0, LSR #31                    @ r1 <- r0 >> 31
    add r0, r2, r1                         @ r0 <- r2 + r1
    add r2,r0,r0, lsl #2                   @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                   @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2-r4}
    bx lr                                  @ return


```



## Arturo


### Functional


```arturo
print $(join $(map $(range 1 10) { toString(&) }) ", ")
```

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## AutoHotkey


```AutoHotkey
Loop, 9                ; loop 9 times
{
  output .= A_Index    ; append the index of the current loop to the output var
  If (A_Index <> 9)    ; if it isn't the 9th iteration of the loop
    output .= ", "     ; append ", " to the output var
}
MsgBox, %output%
```



## AutoIt


```autoit
#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.8.1
 Author:         Alexander Alvonellos

 Script Function:
	Output a comma separated list from 1 to 10, and on the tenth iteration of the
	output loop, only perform half of the loop.

#ce ----------------------------------------------------------------------------

Func doLoopIterative()
		Dim $list = ""
		For $i = 1 To 10 Step 1
			$list = $list & $i
			If($i = 10) Then ExitLoop
			$list = $list & ", "
		Next
		return $list & @CRLF
EndFunc

Func main()
	ConsoleWrite(doLoopIterative())
EndFunc

main()
```



## AWK

'''One-liner:'''

```awk
$ awk 'BEGIN{for(i=1;i<=10;i++){printf i;if(i<10)printf ", "};print}'
```

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


'''Readable version:'''

```awk

BEGIN {
  n=10
  for(i=1;i<=n;i++) {
    printf i;
    if(i<n) printf ", "
  }
  print
}
```

Same output.


## Axe


```axe
For(I,1,10)
 Disp I▶Dec
 If I=10
  Disp i
 Else
  Disp ","
 End
End
```



## BASIC

```qbasic
DIM i AS Integer

FOR i=1 TO 10
    PRINT i;
    IF i=10 THEN EXIT FOR
    PRINT ", ";
NEXT i
```


=== {{header|Applesoft BASIC}} ===
The [[#ZX_Spectrum_Basic|ZX Spectrum Basic]] code will work just fine in Applesoft BASIC.  The following is a more structured approach which avoids the use of GOTO.


```ApplesoftBasic
10 FOR I = 1 TO 10
20     PRINT I;
30     IF I < 10 THEN PRINT ", "; : NEXT I
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 FOR I=1 TO 10
110   PRINT I;
120   IF I=10 THEN EXIT FOR
130   PRINT ",";
140 NEXT
```


=
## Sinclair ZX81 BASIC
=
The [[#ZX_Spectrum_Basic|ZX Spectrum Basic]] program will work on the ZX81. Depending on the context, the programmer's intention may be clearer if we do it all with <code>GOTO</code>s instead of a <code>FOR</code> loop.

```basic
10 LET I=1
20 PRINT I;
30 IF I=10 THEN GOTO 70
40 PRINT ", ";
50 LET I=I+1
60 GOTO 20
```


=== {{header|ZX Spectrum Basic}} ===
To terminate a loop on the ZX Spectrum, set the loop counter to a value that will exit the loop, before jumping to the NEXT statement.


```zxbasic
10 FOR i=1 TO 10
20 PRINT i;
30 IF i=10 THEN GOTO 50
40 PRINT ", ";
50 NEXT i
```


=
## BBC BASIC
=

```bbcbasic
      FOR i% = 1 TO 10
        PRINT ; i% ;
        IF i% <> 10 PRINT ", ";
      NEXT
      PRINT
```



## bc

The <code>print</code> extension is necessary to get the required output.

```bc
while (1) {
    print ++i
    if (i == 10) {
        print "\n"
        break
    }
    print ", "
}
```



## Befunge


```befunge
1+>
::.9`#@_" ,",,
```

This code is a good answer. However, most Befunge implementations print a " " after using . (output number), so this program prints "1 , 2 , 3 ..." with extra spaces. A bypass for this is possible, by adding 48 and printing the ascii character, but does not work with 10::

```befunge
1+>
::68*+,8`#v_" ,",,
  @,,,,", 10"<
```



## Bracmat


```bracmat
  1:?i
&   whl
  ' ( put$!i
    & !i+1:~>10:?i
    & put$", "
    )
```



## C

```c
#include <stdio.h>

int main()
{
  int i;
  for (i = 1; i <= 10; i++) {
    printf("%d", i);
    printf(i == 10 ? "\n" : ", ");
  }
  return 0;
}
```



## C++


```cpp
#include <iostream>

int main()
{
  int i;
  for (i = 1; i<10 ; i++)
    std::cout << i << ", ";
  std::cout<<i;
  return 0;
}
```


## C#

```c#
using System;

class Program
{
    static void Main(string[] args)
    {
        for (int i = 1; ; i++)
        {
            Console.Write(i);
            if (i == 10) break;
            Console.Write(", ");
        }
        Console.WriteLine();
    }
}
```



## Chapel


```chapel
for i in 1..10 do
        write(i, if i % 10 > 0 then ", " else "\n")
```



## Clojure


```clojure

; Functional version
(apply str (interpose ", " (range 1 11)))

; Imperative version
(loop [n 1]
   (printf "%d" n)
   (if (< n 10)
       (do
	(print ", ")
	(recur (inc n)))))

```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Loop-N-And-Half.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I    PIC 99.
       01  List PIC X(45).

       PROCEDURE DIVISION.
           PERFORM FOREVER
               *> The list to display must be built up because using
               *> DISPLAY adds an endline at the end automatically.
               STRING FUNCTION TRIM(List) " "  I  INTO List

               IF I = 10
                   EXIT PERFORM
               END-IF

               STRING FUNCTION TRIM(List) "," INTO List

               ADD 1 TO I
           END-PERFORM

           DISPLAY List

           GOBACK
           .
```

Free-form, 'List'-free version, using DISPLAY NO ADVANCING.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. LOOP-1p5-NOADV.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  I    PIC 99 VALUE 1.
01	IDISP	PIC Z9.
PROCEDURE DIVISION.
	PERFORM FOREVER
		MOVE I TO IDISP
		DISPLAY FUNCTION TRIM(IDISP) WITH NO ADVANCING
		IF I = 10
			EXIT PERFORM
		END-IF
		DISPLAY ", " WITH NO ADVANCING
		ADD 1 TO I
	END-PERFORM.
	STOP RUN.
	END-PROGRAM.
```

Free-form, GO TO, 88-level. Paragraphs in PROCEDURE DIVISION.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. LOOP-1p5-NOADV-GOTO.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  I	PIC 99	VALUE 1.
	88	END-LIST	VALUE 10.
01	I-OUT	PIC Z9.
PROCEDURE DIVISION.
01-LOOP.
	MOVE I TO I-OUT.
	DISPLAY FUNCTION TRIM(I-OUT) WITH NO ADVANCING.
	IF END-LIST GO TO 02-DONE.
	DISPLAY ", " WITH NO ADVANCING.
	ADD 1 TO I.
	GO TO 01-LOOP.
02-DONE.
	STOP RUN.
	END-PROGRAM.
```

Using 'PERFORM VARYING'

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. LOOP-1p5-NOADV-VARY.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  I    	PIC 99  VALUE 1.
	88	END-LIST	VALUE 10.
01	I-OUT	PIC Z9.
PROCEDURE DIVISION.
	PERFORM WITH TEST AFTER VARYING I FROM 1 BY 1 UNTIL END-LIST
		MOVE I TO I-OUT
		DISPLAY FUNCTION TRIM(I-OUT) WITH NO ADVANCING
		IF NOT END-LIST
			DISPLAY ", " WITH NO ADVANCING
		END-IF
	END-PERFORM.
	STOP RUN.
	END-PROGRAM.
```



## CoffeeScript


```coffeescript

# Loop plus half.  This code shows how to break out of a loop early
# on the last iteration.  For the contrived example, there are better
# ways to generate a comma-separated list, of course.
start = 1
end = 10
s = ''
for i in [start..end]
  # the top half of the loop executes every time
  s += i
  break if i == end
  # the bottom half of the loop is skipped for the last value
  s += ', '
console.log s

```



## ColdFusion

With tags:

```cfm
<cfloop index = "i" from = "1" to = "10">
  #i#
  <cfif i EQ 10>
    <cfbreak />
  </cfif>
  ,
</cfloop>
```


With script:

```cfm
<cfscript>

  for( i = 1; i <= 10; i++ ) //note: the ++ notation works only on version 8 up, otherwise use i=i+1
  {
    writeOutput( i );

    if( i == 10 )
    {
      break;
    }
    writeOutput( ", " );
  }
</cfscript>
```



## Common Lisp


```lisp

(loop for i from 1 below 10 do
        (princ i) (princ ", ")
        finally (princ i))

```


or


```lisp
(loop for i from 1 upto 10 do
  (princ i)
  (if (= i 10) (return))
  (princ ", "))
```


but for such simple tasks we can use format's powers:


```lisp

(format t "~{~a~^, ~}" (loop for i from 1 to 10 collect i))

```



## D


### Iterative


```d
import std.stdio;

void main() {
    for (int i = 1; ; i++) {
        write(i);
        if (i >= 10)
            break;
        write(", ");
    }

    writeln();
}
```

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


### Functional Style


```d
void main() {
    import std.stdio, std.range, std.algorithm, std.conv, std.string;
    iota(1, 11).map!text.join(", ").writeln;

    // A simpler solution:
    writefln("%(%d, %)", iota(1, 11));
}
```

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## Dart


```Dart
String loopPlusHalf(start, end) {
  var result = '';
  for(int i = start; i <= end; i++) {
    result += '$i';
    if(i == end) {
      break;
    }
    result += ', ';
  }
  return result;
}

void main() {
  print(loopPlusHalf(1, 10));
}
```



## Delphi



```Delphi
program LoopsNPlusOneHalf;

{$APPTYPE CONSOLE}

var
  i: integer;
const
  MAXVAL = 10;
begin
  for i := 1 to MAXVAL do
  begin
    Write(i);
    if i < MAXVAL then
      Write(', ');
  end;
  Writeln;
end.
```



## DWScript



```Delphi
var i : Integer;

for i := 1 to 10 do begin
   Print(i);
   if i < 10 then
      Print(', ');
end;
```



## E


A typical loop+break solution:

```e
var i := 1
while (true) {
    print(i)
    if (i >= 10) { break }
    print(", ")
    i += 1
}
```


Using the loop primitive in a semi-functional style:


```e
var i := 1
__loop(fn {
    print(i)
    if (i >= 10) {
        false
    } else {
        print(", ")
        i += 1
        true
    }
})
```



## EchoLisp


```lisp

(string-delimiter "")

(for ((i (in-range 1 11))) (write i) #:break (= i 10) (write ","))
 → 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10

;; or

(string-join (range 1 11) ",")
   → 1,2,3,4,5,6,7,8,9,10

```



## EDSAC order code


```edsac
[ N and a half times loop

### =================


  A program for the EDSAC

  Works with Initial Orders 2 ]

        T56K
        GK

        O16@
[  1 ]  T24@
        A25@
        A18@
        U25@
        S23@
        E11@

        O25@
        O19@
        O20@
        G1@

[ 11 ]  O18@
        O17@
        O21@
        O22@
        ZF

[ 16 ]  #F
[ 17 ]  PF
[ 18 ]  QF
[ 19 ]  NF
[ 20 ]  !F
[ 21 ]  @F
[ 22 ]  &F
[ 23 ]  JF
[ 24 ]  PF
[ 25 ]  PF

        EZPF
```

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## Elixir


```elixir
defmodule Loops do
  def n_plus_one_half([]), do: IO.puts ""
  def n_plus_one_half([x]), do: IO.puts x
  def n_plus_one_half([h|t]) do
    IO.write "#{h}, "
    n_plus_one_half(t)
  end
end

Enum.to_list(1..10) |> Loops.n_plus_one_half
```



## Erlang


```erlang
%% Implemented by Arjun Sunel
-module(loop).
-export([main/0]).

main() ->
	for_loop(1).

 for_loop(N) ->
 	if N < 10 ->
		io:format("~p, ",[N] ),
		for_loop(N+1);
	true ->
		io:format("~p\n",[N])
	end.

```




## ERRE


```ERRE

FOR I=1 TO 10 DO
    PRINT(I;)
    EXIT IF I=10
    PRINT(", ";)
END FOR

```





## Euphoria


```Euphoria

for i = 1 to 10 do
    printf(1, "%g", {i})
    if i < 10 then
        puts(1, ", ")
    end if
end for

```


While, yes, use of <code>exit</code> would also work here, it is slightly faster to code it this way, if only the last iteration has something different.

=={{header|F Sharp|F#}}==
Functional version that works for lists of any length

```fsharp

let rec print (lst : int list) =
    match lst with
    | hd :: [] ->
        printf "%i " hd
    | hd :: tl ->
        printf "%i, " hd
        print tl
    | [] -> printf "\n"

print [1..10]

```



## Factor


```factor
: print-comma-list ( n -- )
    [ [1,b] ] keep '[
        [ number>string write ]
        [ _ = [ ", " write ] unless ] bi
    ] each nl ;
```



## Falcon


```falcon
for value = 1 to 10
    formiddle
        >> value
        >> ", "
    end
    forlast: > value
end
```


```txt
prompt$ falcon forto.fal
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## FALSE


```false
1[$9>~][$.", "1+]#.
```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    for (Int i := 1; i <= 10; i++)
    {
      Env.cur.out.writeObj (i)
      if (i == 10) break
      Env.cur.out.writeChars (", ")
    }
    Env.cur.out.printLine ("")
  }
}

```



## FBSL


```qbasic

#APPTYPE CONSOLE
For Dim i = 1 To 10
    PRINT i;
    IF i < 10 THEN PRINT ", ";
Next
PAUSE
```

Output

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
Press any key to continue...
```



## Forth


```forth
: comma-list ( n -- )
  dup 1 ?do  i 1 .r ." , " loop
  . ;
```



```forth
: comma-list ( n -- )
  dup 1+ 1 do
    i 1 .r
    dup i = if leave then   \ or DROP UNLOOP EXIT to exit loop and the function
    [char] , emit space
  loop drop ;
```



```forth
: comma-list ( n -- )
  1
  begin  dup 1 .r
         2dup <>
  while  ." , " 1+
  repeat 2drop ;
```



## Fortran

```fortran
C     Loops N plus one half - Fortran IV (1964)
      INTEGER I
      WRITE(6,301) (I,I=1,10)
  301 FORMAT((I3,','))
      END
```


```fortran
C     WARNING: This program is not valid ANSI FORTRAN 77 code. It uses
C     two nonstandard characters on the lines labelled 5001 and 5002.
C     Many F77 compilers should be okay with it, but it is *not*
C     standard.
      PROGRAM LOOPPLUSONEHALF
        INTEGER I, TEN
C       I'm setting a parameter to distinguish from the label 10.
        PARAMETER (TEN = 10)

        DO 10 I = 1, TEN
C         Write the number only.
          WRITE (*,5001) I

C         If we are on the last one, stop here. This will make this test
C         every iteration, which can slow your program down a little. If
C         you want to speed this up at the cost of your own convenience,
C         you could loop only to nine, and handle ten on its own after
C         the loop is finished. If you don't care, power to you.
          IF (I .EQ. TEN) GOTO 10

C         Append a comma to the number.
          WRITE (*,5002) ','
   10   CONTINUE

C       Always finish with a newline. This programmer hates it when a
C       program does not end its output with a newline.
        WRITE (*,5000) ''
        STOP

 5000   FORMAT (A)

C       Standard FORTRAN 77 is completely incapable of completing a
C       WRITE statement without printing a newline. This program would
C       be much more difficult (i.e. impossible) to write in the ANSI
C       standard, without cheating and saying something like:
C
C           WRITE (*,*) '1, 2, 3, 4, 5, 6, 7, 8, 9, 10'
C
C       The dollar sign at the end of the format is a nonstandard
C       character. It tells the compiler not to print a newline. If you
C       are actually using FORTRAN 77, you should figure out what your
C       particular compiler accepts. If you are actually using Fortran
C       90 or later, you should replace this line with the commented
C       line that follows it.
 5001   FORMAT (I3, $)
 5002   FORMAT (A, $)
C5001   FORMAT (T3, ADVANCE='NO')
C5001   FORMAT (A, ADVANCE='NO')
      END
```


```fortran
i = 1
do
  write(*, '(I0)', advance='no') i
  if ( i == 10 ) exit
  write(*, '(A)', advance='no') ', '
  i = i + 1
end do
write(*,*)
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

For i As Integer = 1 To 10
  Print Str(i);
  If i < 10 Then Print ", ";
Next

Print
Sleep
```


```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long i, num : num = 10

for i = 1 to num
print i;
if i = num then exit for
print ",";
next i

```

Output:

```txt

 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=c43cc581e5f93e70c5dc82733f609a7e Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siLoop As Short

For siLoop = 1 To 10
  Print siLoop;
  If siLoop <> 10 Then Print ", ";
Next

End
```

Output:

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## GAP


```gap
n := 10;
for i in [1 .. n] do
    Print(i);
    if i < n then
        Print(", ");
    else
        Print("\n");
    fi;
od;
```



## GML


```GML
str = ""
for(i = 1; i <= 10; i += 1)
    {
    str += string(i)
    if(i != 10)
        str += ", "
    }
show_message(str)
```



## Go


```go
package main

import "fmt"

func main() {
    for i := 1; ; i++ {
        fmt.Print(i)
        if i == 10 {
            fmt.Println()
            break
        }
        fmt.Print(", ")
    }
}
```



## Gosu


```java
var out = System.out
for(i in 1..10) {
  if(i > 1) out.print(", ")
  out.print(i)
}
```



## Groovy

Solution:

```groovy
for(i in (1..10)) {
    print i
    if (i == 10) break
    print ', '
}
```


Output:

```txt
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## Haskell


```haskell
main :: IO ()
main = forM_ [1 .. 10] $ \n -> do
            putStr $ show n
            putStr $ if n == 10 then "\n" else ", "
```


You can also use intersperse :: a -> [a] -> [a]

```haskell
intercalate ", " (map show [1..10])
```



## hexiscript


```hexiscript
for let i 1; i <= 10; i++
  print i
  if i = 10; break; endif
  print ", "
endfor
println ""
```



## HicEst


```hicest
DO i = 1, 10
    WRITE(APPend) i
    IF(i < 10) WRITE(APPend) ", "
ENDDO
```



## HolyC


```holyc
U8 i, max = 10;
for (i = 1; i <= max; i++) {
  Print("%d", i);
  if (i == max) break;
  Print(", ");
}
Print("\n");
```



## IDL


Nobody would ever use a loop in IDL to output a vector of numbers - the requisite output would be generated something like this:


```idl
print,indgen(10)+1,format='(10(i,:,","))'
```


However if a loop had to be used it could be done like this:


```idl
for i=1,10 do begin
 print,i,format='($,i)'
 if i lt 10 then print,",",format='($,a)'
endfor
```


(which merely suppresses the printing of the comma in the last iteration);

or like this:


```idl
for i=1,10 do begin
 print,i,format='($,i)'
 if i eq 10 then break
 print,",",format='($,a)'
end
```


(which terminates the loop early if the last element is reached).

=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
every writes(i := 1 to 10) do
   if i = 10 then break write()
   else writes(", ")
end
```


The above can be written more succinctly as:

```Icon
every writes(c := "",1 to 10) do c := ","

```



## J


```j
output=: verb define
  buffer=: buffer,y
)

loopy=: verb define
  buffer=: ''
  for_n. 1+i.10 do.
    output ":n
    if. n<10 do.
      output ', '
    end.
  end.
  smoutput buffer
)
```


Example use:

```j
   loopy 0
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


That said, note that neither loops nor output statements are necessary:


```j
   ;}.,(', ' ; ":)&> 1+i.10
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


And, note also that this sort of data driven approach can also deal with more complex issues:


```j
   commaAnd=: ":&.> ;@,. -@# {. (<;._1 '/ and /') ,~ (<', ') #~ #
   commaAnd i.5
0, 1, 2, 3 and 4
```



## Java


```java
public static void main(String[] args) {
    for (int i = 1; ; i++) {
        System.out.print(i);
        if (i == 10)
            break;
        System.out.print(", ");
    }
    System.out.println();
}
```



## JavaScript


```javascript
function loop_plus_half(start, end) {
    var str = '',
        i;
    for (i = start; i <= end; i += 1) {
        str += i;
        if (i === end) {
          break;
        }
        str += ', ';
    }
    return str;
}

alert(loop_plus_half(1, 10));
```


Alternatively, if we stand back for a moment from imperative assumptions about the nature and structure of computing tasks, it becomes clear that the problem of special transitional cases as a pattern terminates has no necessary connection with loops. (See the comments accompanying the ACL2, Haskell, IDL, J and R examples above and below, and see also some of the approaches taken in languages like Clojure and Scala.

If a JavaScript expression evaluates to an array [1 .. 10] of integers, for example, we can map that array directly to a comma-delimited string by using the '''Array.join()''' function, writing something like:


```JavaScript
function range(m, n) {
  return Array.apply(null, Array(n - m + 1)).map(
    function (x, i) {
      return m + i;
    }
  );
}

console.log(
  range(1, 10).join(', ')
);
```


Output:

```JavaScript
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


Otherwise, any special transitional case at the end of a pattern can be handled by defining conditional values for one or more sub-expressions:


```JavaScript
function range(m, n) {
  return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
    return m + i;
  });
}

console.log(
  (function (nFrom, nTo) {
    var iLast = nTo - 1;

    return range(nFrom, nTo).reduce(
      function (accumulator, n, i) {
        return accumulator +
          n.toString() +

          (i < iLast ? ', ' : ''); // conditional sub-expression

      }, ''
    )
  })(1, 10)
);
```


Output:

```JavaScript
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


### = Otherwise =


```JavaScript
var s=1, e=10
for (var i=s; i<=e; i+=1) {
	document.write( i==s ? '' : ', ', i )
}
```

or

```JavaScript
var s=1, e=10
for (;; s+=1) {
	document.write( s )
	if (s==e) break
	document.write( ', ' )
}
```

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## jq

In jq, it is idiomatic to view a range of integers with boundaries m and n as [m, n), i.e. including m but excluding n.

```jq
One approach is to construct the answer incrementally:
def loop_plus_half(m;n):
  if m<n then reduce range(m+1;n) as $i (m|tostring; . +  ", " + ($i|tostring))
  else empty
  end;

# An alternative that is shorter and perhaps closer to the task description because it uses range(m;n) is as follows:
def loop_plus_half2(m;n):
  [range(m;n) | if . == m then . else  ", ", . end | tostring] | join("");
```

 loop_plus_half2(1;11)
 # "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"


## Julia

The short-circuiting && is idiomatic to Julia - the second expression will only be evaluated if the first one is true.

```Julia

for i = 1:10
  print(i)
  i == 10 && break
  print(", ")
end

```

Output:

```Julia

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## Lasso


```Lasso
local(out) = ''
loop(10) => {
    #out->append(loop_count)
    loop_count == 10 ? loop_abort
    #out->append(', ')
}
#out
```



## K


```K
   p:{`0:$x} / output
   i:1;do[10;p[i];p[:[i<10;", "]];i+:1];p@"\n"
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


Alternative solutions:

```K
   10 {p@x;p@:[x<10;", ";"\n"];x+1}\1;
   {p@x;p@:[x<10;", ";"\n"];x+1}'1+!10; /variant
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    for (i in 1 .. 10) {
        print(i)
        if (i < 10) print(", ")
    }
}
```


```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## LabVIEW

{{VI snippet}}<br/>[[File:LabVIEW_Loops_N_plus_one_half.png]]


## Lang5


```lang5
: ,  dup ", " 2 compress "" join ;
1 do dup 10 != if dup , . 1 + else . break then loop
```

Word: [[Loops/For#Lang5]]

```lang5
: 2string  2 compress "" join ;
: ,  dup 10 != if ", " 2string then ;
1 10 "dup , . 1+" times
```



## Lhogho

<code>type</code> doesn't output a newline. The <code>print</code> outputs one.

```logo
for "i [1 10]
[
	type :i
	if :i < 10
	[
		type "|, |
	]
]
print
```


A more list-y way of doing it


```logo
to join :lst :sep
	if list? :lst
	[
		ifelse count :lst > 1
		[
			op (word first :lst :sep joinWith butfirst :lst :sep)
		]
		[
			op (word last :lst)
		]
	]
	op :lst
end

make "aList [1 2 3 4 5 6 7 8 9 10]
print join :aList "|, |
```



## Liberty BASIC

Keyword 'exit' allows the termination.

```lb

for i =1 to 10
    print i;
    if i =10 then exit for
    print ", ";
next i
end

```



## Lisaac


```Lisaac
Section Header

+ name := LOOP_AND_HALF;

Section Public

- main <- (
  + i : INTEGER;

  i := 1;
  {
    i.print;
    i = 10
  }.until_do {
    ", ".print;
    i := i + 1;
  };
  '\n'.print;
);
```



## LiveCode


```LiveCode
repeat with n = 1 to 10
    put n after loopn
    if n is not 10 then put comma after loopn
end repeat
put loopn
```



## Logo


```logo
to comma.list :n
  repeat :n-1 [type repcount type "|, |]
  print :n
end

comma.list 10
```



## Lua

Translation of C:

```lua
for i = 1, 10 do
  io.write(i)
  if i == 10 then break end
  io.write", "
end
```




## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      \\ old type loop
      For i=1 to 10
            Print i;
            If i=10 Then Exit For
            Print ", ";
      Next i
      Print
      \\ fast type loop. Continue exit block, without breaking loop.
      For i=1 to 10 {
                  Print i;
                  If i=10 Then Continue
                  Print ", ";
      }
      Print
      Print
      i=0
      {
                  loop  \\ this mark block for loop, each time need to mark
                  i++
                  Print i;
                  If i=10 Then Exit  ' so now we use exit to break loop
                  Print ", ";
      }
      Print
}
Checkit

```




## M4


```M4
define(`break',
   `define(`ulim',llim)')
define(`for',
   `ifelse($#,0,``$0'',
   `define(`ulim',$3)`'define(`llim',$2)`'ifelse(ifelse($3,`',1,
         `eval($2<=$3)'),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),ulim,`$4')')')')

for(`x',`1',`',
   `x`'ifelse(x,10,`break',`, ')')
```



## Maple


```Maple>
 for i to 10 do printf( "%d%s", i, `if`( i = 10, "\n", ", " ) ) end:
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## Mathematica


```Mathematica
i = 1; s = "";
While[True,
 s = s <> ToString@i;
 If[i == 10, Break[]];
 s = s <> ",";
 i++;
 ]
s
```


=={{header|MATLAB}} / {{header|Octave}}==
Vectorized form:

```MATLAB
 	printf('%i, ',1:9); printf('%i\n',10);
```


Explicite loop:

```Matlab
   for k=1:10,
      printf('%i', k);
   if k==10, break; end;
      printf(', ');
   end;
   printf('\n');
```


Output:

```txt
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## MAXScript


```maxscript
for i in 1 to 10 do
(
    format "%" i
    if i == 10 then exit
    format "%" ", "
)
```



## Make


```make
NEXT=`expr $* + 1`
MAX=10
RES=1

all: 1-n;

$(MAX)-n:
       @echo $(RES)

%-n:
       @-make -f loop.mk $(NEXT)-n MAX=$(MAX) RES=$(RES),$(NEXT)
```


Invoking it
 |make -f loop.mk MAX=10
 1,2,3,4,5,6,7,8,9,10


## Metafont


Since <tt>message</tt> append always a newline, we need building the output inside a string,
and then we output it.


```metafont
last := 10;
string s; s := "";
for i = 1 upto last:
  s := s & decimal i;
  if i <> last: s := s & ", " fi;
endfor
message s;
end
```


=={{header|Modula-3}}==

```modula3
MODULE Loop EXPORTS Main;

IMPORT IO, Fmt;

VAR i := 1;

BEGIN
  LOOP
    IO.Put(Fmt.Int(i));
    IF i = 10 THEN EXIT; END;
    IO.Put(", ");
    i := i + 1;
  END;
  IO.Put("\n");
END Loop.
```



## Nemerle


```Nemerle
foreach (i in [1 .. 10])
{
    Write(i);
    unless (i == 10) Write(", ");
}
```



## MUMPS


```MUMPS
LOOPHALF
 NEW I
 FOR I=1:1:10 DO
 .WRITE I
 .IF I'=10 WRITE ", "
 QUIT
 ;Alternate
 NEW I FOR I=1:1:10 WRITE I WRITE:I'=10 ", "
 KILL I QUIT
```

Output:
```txt
USER>D LOOPHALF^ROSETTA
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
USER>D LOOPHALF+7^ROSETTA
1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## NetRexx


```NetRexx
/* NetRexx */
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/N plus one half'

  rs = ''
  istart = 1
  iend   = 10
  loop i_ = istart to iend
    rs = rs || ' ' || i_
    if i_ < iend then do
      rs = rs','
      end
    end i_
  say rs.strip()
```

'''Output'''

```txt
Loops/N plus one half
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```



## NewLISP


```NewLISP
(for (i 0 10)
  (print i)
  (unless (= i 10)
    (print ", ")))
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 FOR I=1 TO 10
20 PRINT I;
30 IF I=10 THEN GOTO 50
40 PRINT ",";
50 NEXT
```



## Microsoft Small Basic


```smallbasic
For i = 1 To 10
  TextWindow.Write(i)
  If i <> 10 Then
    TextWindow.Write(", ")
  EndIf
EndFor
TextWindow.WriteLine("")
```




## Nim


```nim
var s = ""

for i in 1..10:
  if s.len > 0: s.add(", ")
  s.add($i)

echo s
```



## Objeck


```objeck

bundle Default {
  class Hello {
    function : Main(args : String[]) ~ Nil {
      for(i := 1; true; i += 1;) {
        i->Print();
        if(i = 10) {
          break;
        };
        ", "->Print();
      };
      '\n'->Print();
    }
  }
}

```



## OCaml


```ocaml
let last = 10 in
for i = 1 to last do
  print_int i;
  if i <> last then
    print_string ", ";
done;
print_newline();
```



```ocaml
let ints = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
let str_ints = List.map string_of_int ints in
print_endline (String.concat ", " str_ints);
```



## Oforth



```Oforth
: loopn
| i |
   10 loop: i [ i dup print 10 ifEq: [ break ] "," . ] printcr ;
```



## Oz

Using a for-loop:

```oz

for N in {List.number 1 10 1} break:Break do
   {System.printInfo N}
   if N == 10 then {Break} end
   {System.printInfo ", "}
end
```


However, it seems more natural to use a left fold:

```oz
declare
  fun {CommaSep Xs}
     case Xs of nil then nil
     [] X|Xr then
	{FoldL Xr
	 fun {$ Z X} Z#", "#X end
	 X}
     end
  end
in
  {System.showInfo {CommaSep {List.number 1 10 1}}}
```



## PARI/GP


```parigp
n=0;
while(1,
  print1(n++);
  if(n>9, break);
  print1(", ")
);
```



## Panda

Panda is stream based. To know if there is no more values you need to know it's the last. You can only do that if you get all the values. So this is functional style; We accumulate all the values from the stream. Then join them together as strings with a comma.

```panda
array{{1..10}}.join(',')
```



## Pascal


```pascal
program numlist(output);

const MAXNUM: integer = 10;
var
  i: integer;

begin
  { loop 1: w/ if branching }
  for i := 1 to MAXNUM do
    begin
      write(i);
      if i <> MAXNUM then
        write(', ')
    end;
  writeln;
  { loop 2: w/o if branching }
  for i := 1 to MAXNUM-1 do
    write(i, ', ');
  writeln(MAXNUM);
end.
```



## Peloton


```sgml
<@ FORLITLIT>
10|<@ SAYPOSFOR>...</@><@ ABF>,</@></@>
```



## Perl


```perl
for my $i(1..10) {
    print $i;
    last if $i == 10;
    print ', ';
}
print "\n";
```


In perl one would solve the task via <code>join</code>.

```perl
print join(', ', 1..10), "\n";
```



## Perl 6


```perl6
for 1 .. 10 {
    .print;
    last when 10;
    print ', ';
}

print "\n";
```



## Phix


```Phix
for i=1 to 10 do
    printf(1,"%d",i)
    if i=10 then exit end if
    printf(1,", ")
end for
```



## PHP


```php
for ($i = 1; $i <= 11; $i++) {
    echo $i;
    if ($i == 10)
        break;
    echo ', ';
}
echo "\n";
```



## PicoLisp


```PicoLisp
(for N 10
   (prin N)
   (T (= N 10))
   (prin ", ") )
```



## Pike


```pike

int main(){
   for(int i = 1; i <= 11; i++){
      write(sprintf("%d",i));
      if(i == 10){
         break;
      }
      write(", ");
   }
   write("\n");
}
```



## PL/I


```PL/I

do i = 1 to 10;
   put edit (trim(i)) (a);
   if i < 10 then put edit (', ') (a);
end;

```



## Pop11


```pop11
lvars i;
for i from 1 to 10 do
    printf(i, '%p');
    quitif(i = 10);
    printf(', ', '%p');
endfor;
printf('\n', '%p');
```



## PowerShell

```powershell
for ($i = 1; $i -le 10; $i++) {
    Write-Host -NoNewLine $i
    if ($i -eq 10) {
        Write-Host
        break
    }
    Write-Host -NoNewLine ", "
}
```

An interesting alternative solution, although not ''strictly'' a loop, even though <code>switch</code> certainly loops over the given range.

```powershell
switch (1..10) {
    { $true }     { Write-Host -NoNewLine $_ }
    { $_ -lt 10 } { Write-Host -NoNewLine ", " }
    { $_ -eq 10 } { Write-Host }
}
```



## PureBasic



```PureBasic
x=1
Repeat
  Print(Str(x))
  x+1
  If x>10: Break: EndIf
  Print(", ")
ForEver
```



## Prolog


```prolog
example :-
  between(1,10,Val), write(Val), Val<10, write(', '), fail.
example.
```


```txt
?- example.
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
true.
```


## Python

The particular pattern and example chosen in the task description is recognised by the Python language and there are more idiomatic ways to achieve the result that don't even require an explicit conditional test such as:

```python
print ( ', '.join(str(i+1) for i in range(10)) )
```

But the [http://academicearth.org/lectures/the-loop-and-half-problem named pattern] is shown by code such as the following:

```python
>>>
 from sys import stdout
>>> write = stdout.write
>>> n, i = 10, 1
>>> while True:
    write(i)
    i += 1
    if i > n:
        break
    write(', ')


1, 2, 3, 4, 5, 6, 7, 8, 9, 10
>>>
```



## R

The natural way to solve this task in R is:

```R
paste(1:10, collapse=", ")
```

The task specifies that we should use a loop however, so this more verbose code is needed.

```R
for(i in 1:10)
{
   cat(i)
   if(i==10)
   {
      cat("\n")
      break
   }
   cat(", ")
}
```



## Racket


```Racket
#lang racket
(for ((i (in-range 1 15)))
  (display i)
  #:break (= 10 i)
  (display ", "))
```


Gives the desired output.


## REBOL


```REBOL
REBOL [
    Title: "Loop Plus Half"
    URL: http://rosettacode.org/wiki/Loop/n_plus_one_half
]

repeat i 10 [
	prin i
	if 10 = i [break]
	prin ", "
]
print ""
```



## REXX


### two CHAROUTs


```rexx
/*REXX program displays:                 1,2,3,4,5,6,7,8,9,10                           */

     do j=1  to 10
     call charout ,j                             /*write the  DO loop  index  (no LF).  */
     if j<10  then call charout ,","             /*append a comma for one-digit numbers.*/
     end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



### one CHAROUT


```rexx
/*REXX program displays:                 1,2,3,4,5,6,7,8,9,10                           */

     do j=1  for 10                              /*using   FOR   is faster than    TO.  */
     call charout ,j || left(',',j<10)           /*display  J, maybe append a comma (,).*/
     end   /*j*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



### version 3 if the number of items is not known


```rexx
list='aa bb cc dd'
sep=', '
Do i=1 By 1 While list<>''
  If i>1 Then Call charout ,sep
  Parse Var list item list
  Call charout ,item
  End
```

```txt
aa, bb, cc, dd
```



## Ring


```ring

for x = 1 to 10 see x if x=10 exit ok see ", " next see nl

```



## Ruby


```ruby

(1..10).each do |i|
  print i
  break if i == 10
  print ", "
end
puts
```

More idiomatic Ruby to obtain the same result is:

```ruby

puts (1..10).join(", ")
```


## Rust


```rust
fn main() {
    for i in 1..=10 {
        print!("{}{}", i, if i < 10 { ", " } else { "\n" });
    }
}
```


More like the problem description:

```rust
fn main() {
    for i in 1..=10 {
        print!("{}", i);
        if i == 10 {
            break;
        }
        print!(", ");
    }
    println!();
}
```


Alternative solution using join.

```rust
fn main() {
    println!(
        "{}",
        (1..=10)
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    );
}
```



## Run BASIC


```runbasic

FOR i = 1 TO 10
  PRINT cma$;i;
  cma$ = " , "
NEXT i

```



=={{header|S-lang}}==
This may constitute not following directions, but I've always felt
the most readable and general way to code this is to move the "optional"
part from the bottom to the top of the loop, then NOT include it on the
FIRST pass:
<lang S-lang>variable more = 0, i;
foreach i ([1:10]) {
  if (more) () = printf(", ");
  printf("%d", i);
  more = 1;
}
```



## Salmon


```Salmon
iterate (x; [1...10])
  {
    print(x);
    if (x == 10)
        break;;
    print(", ");
  };
print("\n");
```



## Scala

```Scala
object LoopAndHalf extends App {
  println((1 to 10).mkString(", "))
}
```



## Scheme

It is possible to use continuations:

```scheme
(call-with-current-continuation
 (lambda (esc)
   (do ((i 1 (+ 1 i))) (#f)
     (display i)
     (if (= i 10) (esc (newline)))
     (display ", "))))
```


But usually making the tail recursion explicit is enough:

```scheme
(let loop ((i 0))
  (display i)
  (if (= i 10)
      (newline)
      (begin
        (display ", ")
        (loop (+ 1 i)))))
```



## Scilab

<lang>for i=1:10
    printf("%2d ",i)
    if i<10 then printf(", "); end
end
printf("\n")
```

```txt
 1 ,  2 ,  3 ,  4 ,  5 ,  6 ,  7 ,  8 ,  9 , 10
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range 1 to 10 do
      write(number);
      if number < 10 then
        write(", ")
      end if;
    end for;
    writeln;
  end func;
```



## Sidef


```ruby
for (1..10) { |i|
    print i;
    i == 10 && break;
    print ', ';
}

print "\n";
```



## Smalltalk


```smalltalk
1 to: 10 do: [:n |
    Transcript show: n asString.
    n < 10 ifTrue: [ Transcript show: ', ' ]
]
```



## SNOBOL4


It's idiomatic in Snobol to accumulate the result in a string buffer for
line output, and to use the same statement for loop control and the comma.


```SNOBOL4
loop    str = str lt(i,10) (i = i + 1) :f(out)
        str = str ne(i,10) ',' :s(loop)
out     output = str
end
```


For the task description, it's possible (implementation dependent) to
set an output variable to raw mode for character output within the loop.

This example also breaks the loop explicitly:

```SNOBOL4
        output('out',1,'-[-r1]')
loop    i = lt(i,10) i + 1 :f(end)
        out = i
        eq(i,10) :s(end)
        out = ',' :(loop)
end
```


```txt
1,2,3,4,5,6,7,8,9,10
```



## SNUSP


```snusp
@\>@\>@\>
+++++++++<!/+.  >-?\#  digit and loop test
 |  |  \@@@+@+++++# \>>.<.<</    comma and space
 |  \@@+@@+++++#
 \@@@@=++++#
```



## Spin

```spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main | n
  ser.start(31, 30, 0, 115200)

  repeat n from 1 to 10
    ser.dec(n)
    if n<10
      ser.str(string(", "))
  ser.str(string(13, 10))

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10

```



## Stata


```stata
forv i=1/10 {
	di `i' _continue
	if `i'<10 {
		di ", " _continue
	}
	else {
		di
	}
}
```



###  Mata


```stata
mata
for (i=1; i<=10; i++) {
	printf("%f",i)
	if (i<10) {
		printf(", ")
	} else {
		printf("\n")
	}
}
end
```



## Swift

```swift
for var i = 1; ; i++ {
    print(i)
    if i == 10 {
        println()
        break
    }
    print(", ")
}
```

{{works with|Swift|2}} The usual way to do this with Swift 2 is:

```swift

for i in 1...10 {

    print(i, terminator: i == 10 ? "\n" : ", ")
}

```

To satisfy the specification, we have to do something similar to Swift 1.x and other C-like languages:

```swift
for var i = 1; ; i++ {
    print(i, terminator: "")
    if i == 10 {
        print("")
        break
    }
    print(", ", terminator: "")
}
```



## Tcl


```tcl
for {set i 1; set end 10} true {incr i} {
    puts -nonewline $i
    if {$i >= $end} break
    puts -nonewline ", "
}
puts ""
```

However, that's not how the specific task (printing 1..10 with comma separators) would normally be done. (Note, the solution below is ''not'' a solution to the half-looping problem.)

```tcl
proc range {from to} {
    set result {}
    for {set i $from} {$i <= $to} {incr i} {
        lappend result $i
    }
    return $i
}

puts [join [range 1 10] ", "] ;# ==> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```


=={{header|TI-89 BASIC}}==

There is no horizontal cursor position on the program IO screen, so we concatenate strings instead.


```ti89b
Local str
"" → str
For i,1,10
  str & string(i) → str
  If i < 10 Then
    str & "," → str
  EndIf
EndFor
Disp str
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
line=""
LOOP n=1,10
 line=CONCAT (line,n)
 IF (n!=10) line=CONCAT (line,", ")
ENDLOOP
PRINT line

```

Output:

```txt

1,  2,  3,  4,  5,  6,  7,  8,  9,  10

```



## UnixPipes

The last iteration is handled automatically for us
when there is no element in one of the pipes.

```bash
yes \ | cat -n | head -n 10 | paste -d\  - <(yes , | head -n 9) | xargs echo
```



## UNIX Shell


```bash
for(( Z=1; Z<=10; Z++ )); do
    echo -e "$Z\c"
    if (( Z != 10 )); then
        echo -e ", \c"
    fi
done
```


```bash
for ((i=1;i<=$((last=10));i++)); do
  echo -n $i
  [ $i -eq $last ] && break
  echo -n ", "
done
```



## Ursa

```ursa
decl int i
for (set i 1) (< i 11) (inc i)
        out i console
        if (= i 10)
                break
        end if
        out ", " console
end for
out endl console
```



## V


```v
[loop
   [ [10 =] [puts]
     [true] [dup put ',' put succ loop]
   ] when].
```

Using it
 |1 loop
 =1,2,3,4,5,6,7,8,9,10


## VBA


```VB
Public Sub WriteACommaSeparatedList()
    Dim i As Integer
    Dim a(1 To 10) As String
    For i = 1 To 10
        a(i) = CStr(i)
    Next i
    Debug.Print Join(a, ", ")
End Sub
```


## Vedit macro language

This example writes the output into current edit buffer.

```vedit
for (#1 = 1; 1; #1++) {
    Num_Ins(#1, LEFT+NOCR)
    if (#1 == 10) { Break }
    Ins_Text(", ")
}
Ins_Newline
```



## Vim Script


```vim
for i in range(1, 10)
   echon i
   if (i != 10)
      echon ", "
   endif
endfor
```



## Visual Basic .NET


```vbnet
For i = 1 To 10
    Console.Write(i)
    If i = 10 Then Exit For
    Console.Write(", ")
Next
```



## Wart


```wart
for i 1 (i <= 10) ++i
  pr i
  if (i < 10)
    pr ", "
(prn)
```



## Wee Basic

print 1 "" ensures the end of program text is separate from the list of numbers.

```Wee Basic
print 1 ""
for numbers=1 to 10
print 1 at numbers*3-2,0 numbers
if numbers<>10
print 1 at numbers*3-1,0 ","
endif
end
```



## XPL0


```XPL0
codes CrLf=9, IntOut=11, Text=12;
int  N;
[for N:= 1 to 10 do     \best way to do this
        [IntOut(0, N);  if N#10 then Text(0, ", ")];
CrLf(0);

N:= 1;                  \way suggested by task statement
loop    [IntOut(0, N);
        if N=10 then quit;
        Text(0, ", ");
        N:= N+1;
        ];
CrLf(0);
]
```



## zkl


```zkl
foreach n in ([1..10]){ print(n); if (n!=10) print(",") }
```

Or, using a state machine:

```zkl
[1..10].pump(Console.print, Void.Drop, T(Void.Write,",",Void.Drop));
```

where pump is (sink, action, action ...). The first Drop writes the
first object from the source (1) to the sink and drops out (and that
iteration of the loop is done).  For the rest of the loop, Write
collects things to write to the sink:  a comma and the number, eg ",2".
Or:

```zkl
[1..10].pump(Console.print, Void.Drop, fcn(n){ String(",",n) });
```

