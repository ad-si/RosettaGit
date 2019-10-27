+++
title = "Loops/For with a specified step"
description = ""
date = 2019-10-08T16:58:57Z
aliases = []
[extra]
id = 4491
[taxonomies]
categories = []
tags = []
+++

{{task|Iteration}}
[[Category:Simple]]

Demonstrate a for-loop where the step-value is greater than one.


;Related tasks:
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

;Basic - Algol style
The opcode BXH uses 3 registers, one for index one for step and one for limit.

```360asm
*        Loops/For with a specified step     12/08/2015
LOOPFORS CSECT
         USING  LOOPFORS,R12
         LR     R12,R15
*     == Algol style 
### ==========
 test at the beginning
         LA     R3,BUF             idx=0
         LA     R5,0               from 5 (from-step=0)
         LA     R6,5               step 5
         LA     R7,25              to 25
LOOPI    BXH    R5,R6,ELOOPI       for i=5 to 25 step 5
         XDECO  R5,XDEC              edit i
         MVC    0(4,R3),XDEC+8       output i
         LA     R3,4(R3)             idx=idx+4
         B      LOOPI              next i
ELOOPI   XPRNT  BUF,80             print buffer
         BR     R14
BUF      DC     CL80' '            buffer
XDEC     DS     CL12               temp for edit
         YREGS  
         END    LOOPFORS
```

{{out}}

```txt

   5  10  15  20  25

```

;Basic - Fortran style
The opcode BXLE uses 3 registers, one for index one for step and one for limit.

```360asm
*     == Fortran style 
### ========
 test at the end
         LA     R3,BUF             idx=0
         LA     R5,5               from 5
         LA     R6,5               step 5
         LA     R7,25              to 25
LOOPJ    XDECO  R5,XDEC            for j=5 to 25 step 5;  edit j
         MVC    0(4,R3),XDEC+8       output j
         LA     R3,4(R3)             idx=idx+4
         BXLE   R5,R6,LOOPJ        next j
         XPRNT  BUF,80             print buffer
```

;Structured Macros

```360asm
*     == Algol style 
### ==========
 test at the beginning
         LA     R3,BUF             idx=0
         LA     R5,5               from 5 
         LA     R6,5               step 5
         LA     R7,25              to 25
         DO WHILE=(CR,R5,LE,R7)    for i=5 to 25 step 5
           XDECO  R5,XDEC            edit i 
           MVC    0(4,R3),XDEC+8     output i
           LA     R3,4(R3)           idx=idx+4
           AR     R5,R6              i=i+step
         ENDDO  ,                  next i
         XPRNT  BUF,80             print buffer
```

;Structured Macros HLASM

```360asm
*     == Fortran style 
### ========
 test at the end
         LA     R3,BUF             idx=0
         DO FROM=(R5,5),TO=(R7,25),BY=(R6,5)  for i=5 to 25 step 5
           XDECO  R5,XDEC            edit i 
           MVC    0(4,R3),XDEC+8     output i
           LA     R3,4(R3)           idx=idx+4
         ENDDO  ,                  next i
         XPRNT  BUF,80             print buffer
```



## Ada

The FOR loop construct in Ada does not give the programmer the ability to directly modify the loop control variable during the execution of the loop.  
Instead, a valid range must always be provided before entering a loop. 
Because exact adherence to the task is impossible, we have three versions to approximate a solution. Looper_1 goes through a range of values which are even. 
Looper_2 multiples each value by two. 
Looper_3 most closely adheres to the requirements of this task, and achieves this by using a second range for the indices.


```ada
with Loopers;
use Loopers;


procedure For_Main is
begin
        Looper_1;
        Looper_2;
        Looper_3;
end For_Main;


package Loopers is
        procedure Looper_1;
        procedure Looper_2;
        procedure Looper_3;
end Loopers;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

package body Loopers is
        procedure Looper_1 is
                Values : array(1..5) of Integer := (2,4,6,8,10);
        begin
                for I in Values'Range loop
                        Put(Values(I),0);
                        if I = Values'Last then
                                Put_Line(".");
                        else
                                Put(",");
                        end if;
                end loop;
        end Looper_1;

        procedure Looper_2 is
                E : Integer := 5;
        begin
                for I in 1..E loop
                        Put(I*2,0);
                        if I = E then
                                Put_Line(".");
                        else
                                Put(",");
                        end if;
                end loop;
        end Looper_2;

        procedure Looper_3 is
                Values : array(1..10) of Integer := (1,2,3,4,5,6,7,8,9,10);
                Indices : array(1..5) of Integer := (2,4,6,8,10);
        begin
                for I in Indices'Range loop
                        Put(Values(Indices(I)),0);
                        if I = Indices'Last then
                                Put_Line(".");
                        else
                                Put(",");
                        end if;
                end loop;
        end Looper_3;

end Loopers;


```



## Agena

Tested with Agena 2.9.5 Win32

```agena
for i from 2 to 8 by 2 do
    print( i )
od
```



## Aime


```aime
integer i;

i = 0;
while (i < 10) {
    o_winteger(2, i);
    i += 2;
}

o_newline();
```



## ALGOL 60

 '''for''' i:=5 '''step''' 5 '''until''' 25 '''do'''
   OUTINTEGER(i)


## ALGOL 68

The ALGOL 68 "''universal''" '''for'''/'''while''' loop:
  [ '''for''' index ] [ '''from''' first ] [ '''by''' increment ] [ '''to''' last ] [ '''while''' condition ] '''do''' statements '''od'''
  The minimum form of a "loop clause" is thus: '''do''' statements '''od''' # an infinite loop #

The formal specification of ALGOL 68 states:
 '''for''' i '''from''' u1 '''by''' u2 '''to''' u3 '''while''' condition '''do''' action '''od'''
"is thus equivalent to the following void-closed-clause:"
 '''begin''' '''int''' f:= u1, '''int''' b = u2, t = u3;
    step2:
      '''if''' (b > 0 &and; f &le; t) &or; (b < 0 &and; f &ge; t) &or; b = 0
      '''then''' '''int''' i = f;
          '''if''' condition
          '''then''' action; f +:= b; '''go''' '''to''' step2
          '''fi'''
      '''fi'''
 '''end'''
Note: Highlighting is as per the formal specification, 
c.f. [[:Category:ALGOL 68#Example of different program representations]].

There are several unusual aspects of the construct:
** only the ''''''do''' ~ '''od'''''' portion was compulsory, in which case the loop will iterate indefinitely.
** thus the clause ''''''to''' 100 '''do''' ~ '''od'''''', will iterate only 100 times.
** the '''while''' "syntactic element" allowed a programmer to break from a '''for''' loop early. eg
 '''int''' sum sq:=0;
 '''for''' i '''while'''
   sum sq &ne; 70 &times; 70
 '''do'''
   sum sq +:= i &uarr; 2
 '''od''' 

Subsequent "extensions" to the standard Algol68 allowed the '''to''' syntactic element to be replaced with '''upto''' and '''downto''' to achieve a small optimisation.  
The same compilers also incorporated:
* '''until'''<sup>(C)</sup> - for late loop termination.
* '''foreach'''<sup>(S)</sup> - for working on arrays in [[wp:Parallel computing|parallel]].

=={{header|ALGOL-M}}==

```algol
BEGIN
    INTEGER I;
    FOR I := 1 STEP 3 UNTIL 19 DO
        WRITE( I );
END
```



## ALGOL W


```algolw
begin
    for i := 3 step 2 until 9 do write( i )
end.
```


=={{header|AppleScript|}}==


```AppleScript
repeat with i from 2 to 10 by 2
	log i
end repeat
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program loopstep2.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
.equ MAXI,   20
/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessResult:  .ascii "Counter = "      @ message result
sMessValeur:   .fill 12, 1, ' '
                   .asciz "\n"
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss 
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main 
main:                @ entry of program 
    push {fp,lr}      @ saves 2 registers 
    mov r4,#0
1:    @ begin loop 
    mov r0,r4
    ldr r1,iAdrsMessValeur     @ display value
    bl conversion10             @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    add r4,#2                   @ increment counter by 2
    cmp r4,#MAXI              @
    ble 1b                @ loop

100:   @ standard end of the program 
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    svc #0                       @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszMessResult:         .int szMessResult
/******************************************************************/
/*     display text with size calculation                         */ 
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}      @ save  registres
    mov r2,#0                  @ counter length 
1:      @ loop length calculation 
    ldrb r1,[r0,r2]           @ read octet start position + index 
    cmp r1,#0                  @ if 0 its over 
    addne r2,r2,#1            @ else add 1 in the length 
    bne 1b                    @ and loop 
                                @ so here r2 contains the length of the message 
    mov r1,r0        			@ address message in r1 
    mov r0,#STDOUT      		@ code to write to the standard output Linux 
    mov r7, #WRITE             @ code call system "write" 
    svc #0                      @ call systeme 
    pop {r0,r1,r2,r7,lr}        @ restaur des  2 registres */ 
    bx lr                       @ return  
/******************************************************************/
/*     Converting a register to a decimal                                 */ 
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion10:
    push {r1-r4,lr}    @ save registers 
    mov r3,r1
    mov r2,#10

1:	   @ start loop
    bl divisionpar10 @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48        @ digit	
    strb r1,[r3,r2]  @ store digit on area
    sub r2,#1         @ previous position
    cmp r0,#0         @ stop if quotient = 0 */
    bne 1b	          @ else loop
    @ and move spaces in first on area
    mov r1,#' '   @ space	
2:	
    strb r1,[r3,r2]  @ store space in area
    subs r2,#1       @ @ previous position
    bge 2b           @ loop if r2 >= zéro 

100:	
    pop {r1-r4,lr}    @ restaur registres 
    bx lr	          @return
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
    push {r2-r4}   /* save registers  */
    mov r4,r0 
    mov r3,#0x6667   @ r3 <- magic_number  lower
    movt r3,#0x6666  @ r3 <- magic_number  upper
    smull r1, r2, r3, r0   @ r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0) 
    mov r2, r2, ASR #2     /* r2 <- r2 >> 2 */
    mov r1, r0, LSR #31    /* r1 <- r0 >> 31 */
    add r0, r2, r1         /* r0 <- r2 + r1 */
    add r2,r0,r0, lsl #2   /* r2 <- r0 * 5 */
    sub r1,r4,r2, lsl #1   /* r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10) */
    pop {r2-r4}
    bx lr                  /* leave function */
/***************************************************/
/* integer division unsigned                       */
/***************************************************/
division:
    /* r0 contains dividend */
    /* r1 contains divisor */
    /* r2 returns quotient */
    /* r3 returns remainder */
    push {r4, lr}
    mov r2, #0                @ init quotient
    mov r3, #0                @ init remainder
    mov r4, #32               @ init counter bits
    b 2f
1:          @ loop 
    movs r0, r0, LSL #1     @ r0 <- r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1)
    adc r3, r3, r3           @ r3 <- r3 + r3 + C. This is equivalent to r3 ? (r3 << 1) + C 
    cmp r3, r1               @ compute r3 - r1 and update cpsr 
    subhs r3, r3, r1        @ if r3 >= r1 (C=1) then r3 ? r3 - r1 
    adc r2, r2, r2           @ r2 <- r2 + r2 + C. This is equivalent to r2 <- (r2 << 1) + C 
2:
    subs r4, r4, #1          @ r4 <- r4 - 1 
    bpl 1b                  @ if r4 >= 0 (N=0) then loop
    pop {r4, lr}
    bx lr



```



## Arturo


```arturo
loop $(range 0 10 2) {
	print &
}
```



## AutoHotkey


```AutoHotkey
SetBatchLines, -1
iterations := 5
step := 10
iterations *= step
Loop,  % iterations
{
   If Mod(A_Index, step)
      Continue
   MsgBox, % A_Index
}
ExitApp
```



## AWK



```awk
BEGIN {
  for (i= 2; i <= 8; i = i + 2) {
    print i
  }
  print "Ain't never too late!"
}
```



## Axe

Axe does not support a step size other than 1. However, one can modify the increment variable inside the loop to accomplish the same task.

This example increments by 2:

```axe
For(I,0,10)
 Disp I▶Dec,i
 I++
End
```



## BASIC


=
## Applesoft BASIC
=

```qbasic
FOR I = 2 TO 8 STEP 2 : PRINT I; ", "; : NEXT I : PRINT "WHO DO WE APPRECIATE?"
```


=
## BaCon
=
This prints all odd digits:

```freebasic

FOR i = 1 TO 10 STEP 2
    PRINT i
NEXT
```


==={{header|Basic|QuickBasic}}===
{{works with|QuickBasic|4.5}}

```qbasic
for i = 2 to 8 step 2
   print i; ", ";
next i
print "who do we appreciate?"
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 FOR I=1 TO 10 STEP 2
110   PRINT I
120 NEXT
```


=
## smart BASIC
=

Notice how the ampersand (&) is used to concatenate the variable with the text instead of a semicolon.


```qbasic
FOR n = 2 TO 8 STEP 2
   PRINT n & "..";
NEXT n
PRINT "who do we appreciate?"
END
```


=
## Commodore BASIC
=

```qbasic
10 FOR I = 1 TO 10 STEP 2
20 PRINT I
30 NEXT
```



## Batch File


```dos
@echo off
for /l %%A in (1,2,10) do (
     echo %%A
)
```

{{Out}}

```txt
>Sample.BAT
1
3
5
7
9

>
```



## BBC BASIC


```bbcbasic
      FOR n = 2 TO 8 STEP 1.5
        PRINT n
      NEXT
```

{{out}}

```txt

         2
       3.5
         5
       6.5
         8

```



## bc


```bc
for (i = 2; i <= 10; i += 2) {
    i
}
```



## Befunge

{{trans|C}}

```befunge>1 
:.55+,v
@_^#`9:+2<
```



## C

This prints all odd digits:

```c
int i;
for(i = 1; i < 10; i += 2)
  printf("%d\n", i);
```



## ChucK

Chuck style

```c

SinOsc s => dac;

for (0 => int i; i < 2000; 5 +=> i )
{
    i => s.freq;
    100::ms => now;
}

```

General purpose style:

```c

for (0 => int i; i < 2000; 5 +=> i )
{
    <<< i >>>;
}

```



## C++

This prints all odd digits:

```cpp
for (int i = 1; i < 10; i += 2)
  std::cout << i << std::endl;
```


=={{header|C sharp|C#}}==


```csharp
using System;
 
class Program {
    static void Main(string[] args) {    
        for (int i = 2; i <= 8; i+= 2) {        
            Console.Write("{0}, ", i);
        }

        Console.WriteLine("who do we appreciate?");
    }
}
```



## Ceylon


```ceylon
shared void run() {
	
	for(i in (2..8).by(2)) {
		process.write("``i`` ");
	}
	print("who do we appreciate?");
}
```



## Clojure

The first example here is following the literal specification, but is not idiomatic Clojure code. 
The second example achieves the same effect without explicit looping, and would (I think) be viewed as better code by the Clojure community.

```Clojure
(loop [i 0]
  (println i)
  (when (< i 10)
    (recur (+ 2 i))))

(doseq [i (range 0 12 2)] 
  (println i))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Display-Odd-Nums.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I PIC 99.

       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 2 UNTIL 10 < I
               DISPLAY I
           END-PERFORM

           GOBACK
           .
```



## ColdFusion


```cfm

<cfloop from="0" to="99" step="3" index="i">
  <Cfoutput>#i#</Cfoutput>
</cfloop>

```



## Common Lisp


```lisp

(format t "~{~S, ~}who do we appreciate?~%" (loop for i from 2 to 8 by 2 collect i))

```


{{out}}

```txt

2, 4, 6, 8, who do we appreciate?

```



## Chapel


```chapel

// Can be set on commandline via --N=x
config const N = 3; 

for i in 1 .. 10 by N {
  writeln(i);
}

```

{{out}}

```txt

$ ./loopby
1
4
7
10

$ ./loopby --N=4
1
5
9

```



## D


```d
import std.stdio, std.range;

void main() {
    // Print odd numbers up to 9.
    for (int i = 1; i < 10; i += 2)
        writeln(i);

    // Alternative way.
    foreach (i; iota(1, 10, 2))
        writeln(i);
}
```

{{out}}

```txt
1
3
5
7
9
1
3
5
7
9
```



## Dao


```dao
# first value: 1
# max value: 9
# step: 2
for( i = 1 : 2 : 9 ) io.writeln( i )
```



## Delphi

Delphi's For loop doesn't support a step value.  
It would have to be simulated using something like a While loop.


```Delphi
program LoopWithStep;

{$APPTYPE CONSOLE}

var
  i: Integer;
begin
  i:=2;
  while i <= 8 do begin
    WriteLn(i);
    Inc(i, 2);
  end;
end.
```


{{out}}

```txt
2
4
6
8
```



## Dragon


```dragon
for(i = 2, i <= 8,i += 2){
   show i + ", "
}
showln "who do we appreciate?"
```




## DWScript


```Delphi
var i : Integer;

for i := 2 to 8 step 2 do
   PrintLn(i);
```


{{out}}

```txt
2
4
6
8
```



## E


There is no step in the standard numeric range object (a..b and a..!b) in E, which is typically used for numeric iteration. 
An ordinary while loop can of course be used:


```e
var i := 2
while (i <= 8) {
    print(`$i, `)
    i += 2
}
println("who do we appreciate?")
```


A programmer frequently in need of iteration with an arbitrary step should define an appropriate range object:


```e
def stepRange(low, high, step) {
    def range {
        to iterate(f) {
            var i := low
            while (i <= high) {
                f(null, i)
                i += step
            }
        }
    }
    return range
}

for i in stepRange(2, 9, 2) {
  print(`$i, `)
}
println("who do we appreciate?")
```


The least efficient, but perhaps convenient, solution is to iterate over successive integers and discard undesired ones:


```e
for i ? (i %% 2 <=> 0) in 2..8 {
    print(`$i, `)
}
println("who do we appreciate?")
```



## EchoLisp

Steps may be integers, float, rationals.

```lisp

(for ((i (in-range 0 15 2))) (write i))
    0 2 4 6 8 10 12 14

(for ((q (in-range 0 15 14/8))) (write q))
    0 7/4 7/2 21/4 7 35/4 21/2 49/4 14 

(for ((x (in-range 0 15 PI))) (write x))
    0 3.141592653589793 6.283185307179586 9.42477796076938 12.566370614359172 

```



## Ela



```ela
open monad io
 
for m s n | n > m = do return ()
          | else = do
              putStrLn (show n) 
              for m s (n+s)
 
_  = for 10 2 0 ::: IO
```


{{out}}

```txt
0
2
4
6
8
10
```



## Elena

ELENA 4.x

```elena
public program()
{
    for(int i := 2, i <= 8, i += 2  )
    {
        console.writeLine:i
    }
}
```



## Elixir


```elixir
defmodule Loops do
  def for_step(n, step) do
    IO.inspect Enum.take_every(1..n, step)
  end
end

Loops.for_step(20, 3)
```


{{out}}

```txt

[1, 4, 7, 10, 13, 16, 19]

```

or

```elixir
iex(1)> Stream.iterate(1, &(&1+2)) |> Enum.take(10)
[1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
```



## ERRE


```ERRE

      FOR N=2 TO 8 STEP 1.5 DO
        PRINT(N)
      END FOR

```

{{out}}

```txt

         2
       3.5
         5
       6.5
         8

```



## Erlang


```erlang
%% Implemented by Arjun Sunel
%% for_loop/4 by Bengt Kleberg.
-module(loop_step).
-export([main/0, for_loop/1, for_loop/4]).
 
 % This Erlang code for "For Loop" is equivalent to: " for (i=start;  i<end ; i=i+2){ printf("* ");} " in C language. 
 
main() ->
	for_loop(1).    
  
 for_loop( N ) ->
	for_loop( N, 4, 2, fun() -> io:fwrite("* ") end ).

for_loop( I, End, Step, Do ) when N < End ->
    Do(),
    for_loop( I+Step, End, Step, Do );
for_loop( _I, _End, _Step, _Do ) -> ok.

```


{{out}}

```txt

* * ok

```



## Euphoria



```Euphoria

for i = 1 to 10 by 2 do
    ? i
end for

```

As a note, <code>? something</code> is shorthand for:

```Euphoria

print(1, something)
puts(1, "\n")

```


<code>print()</code> differs from <code>puts()</code> in that <code>print()</code> will print out the actual <code>sequence</code> it is given.  
If it is given an <code>integer</code>, or an <code>atom</code> 
(Any number that is not an <code>integer</code>), it will print those out as-is.


## Factor

Prints odd digits.

```factor>1 10 2 <range
 [ . ] each
```



## FALSE


```false
2[$9\>][$.", "2+]#"who do we appreciate!"
```



## Fantom



```fantom

class Main
{
  public static Void main ()
  {
    Int step := 5
    for (Int i := 0; i < 100; i += step)
    {
      echo (i)
    }
  }
}

```



## FBSL


```qbasic
#APPTYPE CONSOLE

DIM n AS INTEGER
FOR n = 2 TO 8 STEP 2
    PRINT n;
    IF n < 8 THEN PRINT " ";
NEXT
PRINT ", who will we obliterate?"
PAUSE

```



## FOCAL

If a <tt>FOR</tt> statement has three parameters, they are (in order) the start, the step, and the end; if only two parameters are supplied, they are taken to be the start and the end. The step is then set to 1.

```focal
FOR I = 1,3,10; TYPE I, !
```



## Forth


```forth
: test
  9 2 do
    i .
  2 +loop
  ." who do we appreciate?" cr ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
do i = 1,10,2
   print *, i
end do
```


{{works with|Fortran|77 and later}}

```fortran
      PROGRAM STEPFOR
        INTEGER I

C       This will print all even numbers from -10 to +10, inclusive.
        DO 10 I = -10, 10, 2
          WRITE (*,*) I
   10   CONTINUE

        STOP
      END
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

For i As Integer = 1 To 21 Step 2
  Print i; " ";
Next
Print
Sleep
```


{{out}}

```txt

 1  3  5  7  9  11  13  15  17  19  21

```


=={{header|F_Sharp|F#}}==

```fsharp
for i in 2..2..8 do
   printf "%d, " i
printfn "done"
```


{{out}}

```txt
2, 4, 6, 8, done

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as Str15 s(11)
dim as long i

s(0) = "Somewhere"
s(2) = " over"
s(4) = " the"
s(6) = " rainbow" + chr$(13)
s(8) = "Bluebirds"
s(10) = " fly."

for i = 0 to 10 step 2
print s(i);
next

```

Output:

```txt

Somewhere over the rainbow
Bluebirds fly.

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=cdd9b10b64ac4d78b75c364061f25641 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short

For siCount = 1 To 50 Step 5
  Print "Gambas is great!"
Next

End
```


```txt

Gambas is great!
Gambas is great!
Gambas is great!
Gambas is great!
Gambas is great!
Gambas is great!
Gambas is great!
Gambas is great!
Gambas is great!
Gambas is great!

```



## GAP

# Use a range [a, b .. c], where the step is b-a (b is the value following a), and c-a must be a multiple of the step.

```gap
for i in [1, 3 .. 11] do
    Print(i, "\n");
od;

1
3
5
7
9
11

```



## GML


```GML
for(i = 0; i < 10; i += 2)
    show_message(string(i))
```



## Go

This prints all odd digits:

```go
for i := 1; i < 10; i += 2 {
  fmt.Printf("%d\n", i)
}
```



## Groovy

"for" loop:

```groovy
for(i in (2..9).step(2)) {
    print "${i} "
}
println "Who do we appreciate?"
```


"each() method:
Though technically not a loop, most Groovy programmers would use the slightly more terse "each()" method on the collection itself, instead of a "for" loop.

```groovy
(2..9).step(2).each {
    print "${it} "
}
println "Who do we appreciate?"
```


{{out}}

```txt
2 4 6 8 Who do we appreciate?
```

Go Team!


## Haskell


```haskell
import Control.Monad (forM_)
main = do forM_ [2,4..8] (\x -> putStr (show x ++ ", "))
          putStrLn "who do we appreciate?"
```



## hexiscript


```hexiscript
for let i 0; i <= 50; let i (i + 5)
  println i
endfor
```



## HicEst


```hicest
DO i = 1, 6, 1.25 ! from 1 to 6 step 1.25
   WRITE() i
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon accomplish loop stepping through the use of a generator, the ternary operator to-by, and the every clause which forces a generator to consume all of its results.  
Because to-by is an operator it has precedence (just higher than assignments) and associativity (left) and can be combined with other operators.

```Icon

   every 1 to 10 by 2                       # the simplest case that satisfies the task, step by 2

   every 1 to 10                            # no to, step is by 1 by default
   every EXPR1 to EXPR2 by EXPR3 do EXPR4   # general case - EXPRn can be complete expressions including other generators such as to-by, every's do is optional
   steps := [2,3,5,7]                       # a list
   every i := 1 to 100 by !steps            # . more complex, several passes with each step in the list steps, also we might want to know what value we are at
   every L[1 to 100 by 2]                   # as a list index
   every i := 1 to 100 by (k := !steps)     # . need () otherwise := generates an error
   every 1 to 5 to 10                       # simple case of combined to-by - 1,..,10, 2,..10, ..., 5,..,10
   every 1 to 15 by 2 to 5                  # combined to-by
   every (1 to 15 by 2) to 5                # . made explicit

   every writes( (TO_BY_EXPR) | "\n", " " ) # if you want to see how any of these work

```

The ability to combine to-by arbitrarily is quite powerful.  
Yet it can lead to unexpected results.  In cases of combined to-by operators the left associativity seems natural where the by is omitted.  
In cases where the by is used it might seem more natural to be right associative.  
If in doubt parenthesize.


## HolyC

This prints all odd digits:

```holyc
U8 i;
for (i = 1; i < 10; i += 2)
  Print("%d\n", i);
```



## Io


```Io
for(i,2,8,2,
    write(i,", ")
)
write("who do we appreciate?")
```



## J


```J
    ' who do we appreciate?' ,~ ":  2 * >: i.4
2 4 6 8 who do we appreciate?
```


Or, using an actual for loop:


```J
   3 :0''
  r=.$0
  for_n. 2 * >: i.4 do.
    r=.r,n
  end.
  ' who do we appreciate?' ,~ ":n
)
2 4 6 8 who do we appreciate?
```


That said, note also that J's '''steps''' verb lets us specify how many steps to take:


```J
   i:8
_8 _7 _6 _5 _4 _3 _2 _1 0 1 2 3 4 5 6 7 8
   i:8j8
_8 _6 _4 _2 0 2 4 6 8
```


Or, if we prefer, we could borrow the definition of <code>thru</code> from the [[Loops/Downward_for#J|Downward for]] task and then filter for the desired values:


```J
   thru=: <./ + i.@(+*)@-~
```


Example use:


```J
   (#~ 0 = 2&|) 1 thru 20
2 4 6 8 10 12 14 16 18 20
   (#~ 0 = 3&|) 1 thru 20
3 6 9 12 15 18
   (#~ 1 = 3&|) 1 thru 20
1 4 7 10 13 16 19
```


And, of course, like filtering in any language, this approach supports non-constant step sizes:


```J
   (#~ 1&p:) 1 thru 20
2 3 5 7 11 13 17 19
```



## Java


```java
for(int i = 2; i <= 8;i += 2){
   System.out.print(i + ", ");
}
System.out.println("who do we appreciate?");
```



## JavaScript


```javascript
var output = '',
    i;
for (i = 2; i <= 8; i += 2) {
   output += i + ', ';
}
output += 'who do we appreciate?';
document.write(output);
```


In a functional idiom of JavaScript, however, we will only be able to compose this computation within the superordinate expressions of our program if it has the the form of an expression returning a value, rather than that of a statement which fires off side-effects but returns no value.

Following the example of languages like Haskell and J on this page, we can begin by generating the stepped series as an expression. In functional JavaScript we will typically replace a state-changing loop with a non-mutating map or fold, writing, for example, something like:


```JavaScript
// range(iMax)
// range(iMin, iMax)
// range(iMin, iMax, dI)
function range() {
  var lngArgs = arguments.length,
    lngMore = lngArgs - 1;

  iMin = lngMore ? arguments[0] : 1;
  iMax = arguments[lngMore ? 1 : 0];
  dI = lngMore > 1 ? arguments[2] : 1;

  return lngArgs ? Array.apply(null, Array(
    Math.floor((iMax - iMin) / dI) + 1
  )).map(function (_, i) {
    return iMin + (dI * i);
  }) : [];
}

console.log(
  range(2, 8, 2).join(', ') + ', who do we appreciate ?'
);
```


Output:

```txt
2, 4, 6, 8, who do we appreciate ?
```



## jq

To generate the stream: 2,4,6,8:
```jq
# If your version of jq does not have range/3, use this:
def range(m;n;step): range(0; ((n-m)/step) ) | m + (. * step);

range(2;9;2)
```

'''Example''':

```jq
reduce range(2;9;2) as $i
   (""; . + "\($i), ") +
   "whom do we appreciate?"
```



## Julia


```julia
for i in 2:2:8
    print(i, ", ")
end
println("whom do we appreciate?")
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    for (i in 1 .. 21 step 2) print("$i ")
}
```


{{out}}

```txt

1 3 5 7 9 11 13 15 17 19 21

```



## LabVIEW

{{VI solution|LabVIEW_Loops_For_with_a_specified_step.png}}


## Lang5


```lang5>: <range
  over iota swap * rot + tuck swap <= select ; : tuck  swap over ;
: >>say.(*)  . ;
1 10 2 <range> >>say.
```




## Lasso


```Lasso
loop(-to=100, -from=1, -by=2) => {^
    loop_count
    '\r' // for formatting
^}
```



## Liberty BASIC


```lb

for i = 2 to 8 step 2
   print i; ", ";
next i
print "who do we appreciate?"
end

```



## LIL

The '''inc''' command accepts a value to add to the variable, 1 if not specified.

```tcl
for {set i 1} {$i < 15} {inc i 3} {print $i}
```

{{out}}

```txt
# for {set i 1} {$i < 15} {inc i 3} {print $i}
1
4
7
10
13
#
```



## Lingo

Lingo loops don't support a "step" parameter, so it has to be implemented manually:

```lingo
step = 3
repeat with i = 0 to 10
  put i
  i = i + (step-1)
end repeat
```

{{out}}

```txt

-- 0
-- 3
-- 6
-- 9

```



## Lisaac


```Lisaac
1.to 9 by 2 do { i : INTEGER;
  i.print;
  '\n'.print;
};
```



## LiveCode


```LiveCode
repeat with n = 0 to 10 step 2
    put n after loopn
    if n is not 10 then put comma after loopn
end repeat
put loopn
```

Output
```LiveCode
0,2,4,6,8,10
```



## Logo


```logo
for [i 2 8 2] [type :i type "|, |] print [who do we appreciate?]
```



## Lua


```lua

for i=2,9,2 do
  print(i)
end

```


{{out}}

```txt

2
4
6
8

```


## M2000 Interpreter


### A for loop

Str$(i) always return decimal separator as "." 
format$() use same as Locale number specify. So here we use "," from Locale 1036. Str$() place a space before number if it is positive. We can use str$(i, "") to trim lead space. Here we use a space in format$ before number (and for negative numbers)

For this task we use single float numbers, and we make the loop one time from lower to higher value, and one time form higher to lower value.


```M2000 Interpreter

Module LoopFor {
      Locale 1036
      Document doc$
      \\ define i as a single
      def single i
      for i=1 to 21 step 5/3
            Print i
            doc$=format$(" {0}", i)
      next i
      doc$={
      }
      \\ make i as a single
      for i=21 to 1 step 5/3
            Print i
            doc$=format$(" {0}", i)
      next i
      clipboard doc$
      report doc$
}
LoopFor

```

{{Out}}

```txt

 1 2,66667 4,33333 6 7,66667 9,33333 11 12,66667 14,33333 16 17,66667 19,33333 21
 21 19,33333 17,66667 16 14,33334 12,66667 11 9,33333 7,66667 6 4,33333 2,66667 1

```


### Iterator step 2


```M2000 Interpreter

a=("A", "B", "C", "D", "E", "F", "Z")
k=Each(a)
While k {
      Print Array$(k),
      k=Each(a, k^+2)   ' set start again
}
Print
\\ a list of keys (unique keys allowed)
Inventory b="A", "B", "C", "D", "E", "F", "Z"
k=Each(b)
While k {
      Print Eval$(k), ' return keys as values, because no value exist yet for each key.
      k=Each(b, k^+2)
}
Print

```

{{out}}

```txt

A   C   E   Z
A   C   E   Z

```



## M4


```M4
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$5`'popdef(`$1')$0(`$1',eval($2+$4),$3,$4,`$5')')')')dnl

for(`x',`1',`5',`3',`x
')

```


{{out}}

```txt

1
4

```



## Maple


```Maple
for i from 2 to 8 by 2 do
  i;
end do;
```

{{out}}

```txt

                               2
                               4
                               6
                               8

```



## Mathematica


```Mathematica
Do[
 Print@i,
 {i, 1, 20, 4}]
```


{{out}}

```txt
1
5
9
13
17

```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
    for k = 0:10:100,
        printf('%d\n',k)
    end; 
```


A vectorized version of the code is


```Matlab
  printf('%d\n',0:10:100);  
```



## Maxima


```maxima
for i: 1 step 2 thru 10 do print(i);
/* 1
   3
   5
   7 */
```


## MAXScript


```MAXScript
for i = 0 to 10 by 2 do format "%\n" i
```

Output:

```MAXScript
 
0
2
4
6
8
10
OK

```



## Microsoft Small Basic


```microsoftsmallbasic

For i = 0 To 100 Step 2
  TextWindow.WriteLine(i)
EndFor

```



## MiniScript


```MiniScript
for i in range(1,20,4)
    print i
end for
```


{{out}}

```txt
1
5
9
13
17
```


=={{header|МК-61/52}}==
<lang>1	П0	ИП0	3	+	П0	1	0	-	x#0
02	С/П
```


In this example, the step is 3, the lowest value is 1 and the upper limit is 10.

=={{header|Modula-2}}==

```modula2
MODULE ForBy;
  IMPORT InOut;

  VAR
    i: INTEGER;

BEGIN
  FOR i := 0 TO 100 BY 2 DO
    InOut.WriteInt(i, 3);
    InOut.WriteLn
  END
END ForBy.
```


=={{header|Modula-3}}==

```modula3
FOR i := 1 TO 100 BY 2 DO
  IO.Put(Fmt.Int(i) & " ");
END;
```



## MUMPS


```MUMPS
FOR I=65:3:122 DO
 .WRITE $CHAR(I)," "
```

{{out}}

```txt
A D G J M P S V Y \ _ b e h k n q t w z
```



## NewLISP


```NewLISP
(for (i 0 10 2)
  (println i))
```



## Nim


```nim
for x in countdown(10,0,3): echo(x)
```

{{out}}

```txt
10
7
4
1
```



## Nemerle


```Nemerle
for (i = 2; i <= 8; i +=2)
```


```Nemerle
foreach (i in [2, 4 .. 8])
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

say
  say 'Loops/For with a specified step'

  loop i_ = -1.4 to 10.6 by 1.7
    say i_.format(3, 1) || '\0'
    end i_
  say
```

{{out}}

```txt
D:\>java lst

Loops/For with a specified step
 -1.4  0.3  2.0  3.7  5.4  7.1  8.8 10.5
```



## Never


The increment step of the Never ''for'' expression can be simple or complex and need not be contiguous.


```fsharp
for (i = 0; i < 10; i += 3)
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 FOR I=1 TO 10 STEP 2
20 PRINT I
30 NEXT
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE LoopForStep;
IMPORT 
  Out;
VAR
  i: INTEGER;
 
BEGIN
  FOR i := 0 TO 10 BY 3 DO
    Out.LongInt(i,0);Out.Ln
  END;
  FOR i := 10 TO 0 BY -3 DO
    Out.LongInt(i,0);Out.Ln
  END
END LoopForStep.

```

Output:

```txt

0
3
6
9
10
7
4
1

```


## Objeck


```objeck

for(i := 0; i < 10; i += 2;) {
  i->PrintLine();
};

```



## OCaml


```ocaml
# let for_step a b step fn =
    let rec aux i =
      if i <= b then begin
        fn i;
        aux (i+step)
      end
    in
    aux a
  ;;
val for_step : int -> int -> int -> (int -> 'a) -> unit = <fun>

# for_step 0 8 2  (fun i -> Printf.printf " %d\n" i) ;;
 0
 2
 4
 6
 8
- : unit = ()
```



## Octave


```octave
for i = 1:2:10
  disp(i)
endfor
```



## Oforth



```Oforth
 1 100 2 step: i [ i println ]
```



## Openscad



```openscad
/* Loop from 3 to 9 in steps of 2 */

for ( l = [3:2:9] )  {
  echo (l);
}
echo ("on a double white line.");
```



## Oz


```oz
for I in 2..8;2 do
   {System.show I}
end
{System.show done}

```



## PARI/GP


```parigp
forstep(n=1,10,2,print(n))
```


The <code>forstep</code> construct is actually more powerful.  
For example, to print numbers with last digit relatively prime to 10:

```parigp
forstep(n=1,100,[2,4,2,2],print(n))
```



## Panda

Panda doesn't nativly have a number generator with steps, so let's add it.

```panda
fun for(from,to,step) type integer,integer,integer->integer
  t=to.minus(from).divide(step)
  0..t.times(step).plus(from)
  /test it for(1 6 2) -> 1 3 5

for(1 3 5)
```



## Pascal

See [[Loops/For_with_a_specified_step#Delphi | Delphi]]


## Perl


```perl
for($i=2; $i <= 8; $i += 2) {
  print "$i, ";
}
print "who do we appreciate?\n";
```



## Perl 6

{{works with|Rakudo|2010.07}}

With at least two values on the left-hand side, the sequence operator (<code>...</code>) can infer an arithmetic series. (With at least three values, it can infer a geometric sequence, too.)


```perl6
for 2, 4 ... 8 {
    print "$_, ";
}
 
say 'whom do we appreciate?';
```
<!-- "Whom" is infinitely more amusing. -->


## Phix


```Phix
for i=2 to 8 by 2 do
    printf(1,"%d, ",i)
end for
printf(1,"who do we appreciate?\n")
```



## PHP


```php
<?php
foreach (range(2, 8, 2) as $i)
    echo "$i, ";
echo "who do we appreciate?\n";
?>
```

{{out}}

```txt
2, 4, 6, 8, who do we appreciate?
```



## PicoLisp


```PicoLisp
(for (N 1 (> 10 N) (+ N 2))
   (printsp N) )
```



## Pike


```pike
int main() {
   for(int i = 2; i <= 16; i=i+2) {
      write(i + "\n");
   }
}
```



## PILOT

One of the advantages of needing to create loops manually by using conditional jumps is that a step of any integer is just as easy as a step of one.

```pilot
R  : Prints the odd numbers less than 10.
C  :i = 1
*Loop
T  :#i
C  :i = i + 2
J ( i < 10 )  :*Loop
END:
```



## PL/I


```PL/I

declare (n, i) fixed binary;

get list (n);
do i = 1 to n by 4;
   put skip list (i);
end;

```



## PowerShell


```powershell
for ($i = 0; $i -lt 10; $i += 2) {
    $i
}
```



## PureBasic


```PureBasic
For i=-15 To 25 Step 5
  Debug i
Next i
```



## Prolog

If you need a stepping iterator, write one:

```prolog
for(Lo,Hi,Step,Lo)  :- Step>0, Lo=<Hi.
for(Lo,Hi,Step,Val) :- Step>0, plus(Lo,Step,V), V=<Hi, !, for(V,Hi,Step,Val).

example :- 
  for(0,10,2,Val), write(Val), write(' '), fail.
example.
```


```txt
?- example.
0 2 4 6 8 10 
true.
```

Adding the following two rules lets you go backwards too:

```prolog
for(Hi,Lo,Step,Hi)  :- Step<0, Lo=<Hi.
for(Hi,Lo,Step,Val) :- Step<0, plus(Hi,Step,V), Lo=<V, !, for(V,Lo,Step,Val).
```



## Python

{{works with|Python|2.x}}

```python
for i in xrange(2, 9, 2):
    print "%d," % i,
print "who do we appreciate?"
```


{{works with|Python|3.x}}

```python
for i in range(2, 9, 2):
    print("%d, " % i, end="")
print("who do we appreciate?")
```


{{out}}

```txt
2, 4, 6, 8, who do we appreciate?
```



## R


```R
for(a in seq(2,8,2)) {
  cat(a, ", ")
}
cat("who do we appreciate?\n")
```


Here the loop may be done implicitly by first concatenating the string and then printing:


```R
cat(paste(c(seq(2, 8, by=2), "who do we appreciate?\n"), collapse=", "))
```



## Racket



```racket

#lang racket

(for ([i (in-range 2 9 2)])
  (printf "~a, " i))
(printf "who do we appreciate?~n")

```



## Raven

List of numbers:

```Raven
[ 2 4 6 8 ] each "%d, " print
"who do we appreciate?\n" print
```


Range:

```Raven
2 10 2 range each "%d, " print
"who do we appreciate?\n" print
```

{{out}}

```txt
2, 4, 6, 8, who do we appreciate?

```



## REBOL


```REBOL
for i 2 8 2 [
	prin rejoin [i ", "]]
print "who do we appreciate?"
```


{{out}}

```txt
2, 4, 6, 8, who do we appreciate?
```




## REXX


### version 1


```rexx
  do x=1  to 10  by 1.5
  say x
  end
```

{{out}}

```txt

1
2.5
4.0
5.5
7.0
8.5
10.0

```



### version 2


```rexx
  do thing=1  by  3/2  to 10 
  say  thing
  end
```

'''output''' is the same as above.





### version 3


```rexx
Do v=1  by  3/2  While v**2<30 
  Say  v
  End
Say '('v'**2) is greater than 30 (30.25)'
```

{{output}}

```txt
1
2.5
4.0
(5.5**2) is greater than 30 (30.25)
```



## Ring

we use step keyword to define step length
in this example we print Even numbers between 0 and 10

```ring

for i = 0 to 10 step 2 see i + nl next

```


{{out}}

```txt

2
4
6
8
10

```


we can use step with double values as well:

```ring

for i = 0 to 10 step 0.5 see i + nl next

```

{{out}}

```txt

0
0.50
1
1.50
2
2.50
3
3.50
4
4.50
5
5.50
6
6.50
7
7.50
8
8.50
9
9.50
10

```



## Ruby


```ruby
2.step(8,2) {|n| print "#{n}, "}
puts "who do we appreciate?"
```

or:

```ruby
(2..8).step(2) {|n| print "#{n}, "}
puts "who do we appreciate?"
```

or:

```ruby
for n in (2..8).step(2)
  print "#{n}, "
end
puts "who do we appreciate?"
```

{{out}}

```txt

2, 4, 6, 8, who do we appreciate?

```



## Run BASIC


```runbasic
for i = 2 to 8 step 2
   print i; ", ";
next i
print "who do we appreciate?"
```

{{out}}

```txt
2, 4, 6, 8, who do we appreciate?
```



## Rust


For Rust 1.28 and later:

```rust
fn main() {
  for i in (2..=8).step_by(2) {
    print!("{}", i);
  }
  println!("who do we appreciate?!");
}

```


An alternative which also works in earlier versions of Rust:

```rust
fn main() {
    let mut i = 2;
    while i <= 8 {
        print!("{}, ", i);
        i += 2;
    }
    println!("who do we appreciate?!");
}
```



## Salmon


```Salmon
for (x; 2; x <= 8; 2)
    print(x, ", ");;
print("who do we appreciate?\n");
```



## SAS


```sas
data _null_;
do i=1 to 10 by 2;
put i;
end;
run;
```



## Sather

See [[Loops/For#Sather]]: the implementation for <code>for!</code> allows to specify a step, even though the built-in <code>stepto!</code> can be used; an example of usage could be simply:

```sather
    i :INT;
    loop
      i := for!(1, 50, 2);
      -- OR
      -- i := 1.stepto!(50, 2);
      #OUT + i + "\n";
    end;
```


(Print all odd numbers from 1 to 50)


## Scala


```scala
for (i <- 2 to 8 by 2) println(i)
```


Alternatively:

```scala
(2 to 8 by 2) foreach println
```



## Scheme

The built-in ''for''-like form in Scheme is the ''do'' form:


```scheme
(do ((i 2 (+ i 2)))  ; list of variables, initials and steps -- you can iterate over several at once
  ((>= i 9))         ; exit condition
  (display i)        ; body
  (newline))
```


Some people prefer to use the recursive-style and more flexible _named let_ form:


```scheme
(let loop ((i 2))            ; function name, parameters and starting values
  (cond ((< i 9)
         (display i)
         (newline)
         (loop (+ i 2))))))  ; tail-recursive call, won't create a new stack frame
```


You can add to the language by wrapping the loop in a function:


```scheme
(define (for-loop start end step func)
  (let loop ((i start))
    (cond ((< i end)
	   (func i)
	   (loop (+ i step))))))

(for-loop 2 9 2
  (lambda (i)
    (display i)
    (newline)))
```


... or in a macro, which allows for making the <code>(lambda)</code> implicit:


```scheme
(define-syntax for-loop
  (syntax-rules () 
    ((for-loop index start end step body ...)
     (let ((evaluated-end end) (evaluated-step step))
       (let loop ((i start))
         (if (< i evaluated-end)
           ((lambda (index) body ... (loop (+ i evaluated-step))) i)))))))

(for-loop i 2 9 2
  (display i)
  (newline))
```


{{out}}

```txt
2
4
6
8
```



## Scilab

{{works with|Scilab|5.5.1}}
<lang>for i=1:2:10
    printf("%d\n",i)
end
```

{{out}}

```txt
1
3
5
7
8
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range 1 to 10 step 2 do
      writeln(number);
    end for;
  end func;
```



## Sidef


'''for(;;)''' loop:

```ruby
for (var i = 2; i <= 8; i += 2) {
    say i
}
```


'''for-in''' loop:

```ruby
for i in (2 .. (8, 2)) {
    say i
}
```


'''.each''' method:

```ruby
2.to(8).by(2).each { |i|
    say i
}
```



## Simula


```simula
begin
    integer i;
    for i:=5 step 5 until 25 do outint(i, 5)
end
```



## Slate


```slate
2 to: 8 by: 2 do: [| :i | Console ; i printString ; ', '].
inform: 'enough with the cheering already!'.
```



## Smalltalk


```smalltalk
2 to: 8 by: 2 do: [ :i |
  Transcript show: i; show ', '
].
Transcript showCr: 'enough with the cheering already!'
```



## Spin

{{works with|BST/BSTC}}
{{works with|FastSpin/FlexSpin}}
{{works with|HomeSpun}}
{{works with|OpenSpin}}

```spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main | n
  ser.start(31, 30, 0, 115200)

  repeat n from 0 to 19 step 3
    ser.dec(n)
    ser.tx(32)

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

{{out}}

```txt

0 3 6 9 12 15 18

```



## SPL


```spl>
 n, 1..10,2
  #.output(n)
<
```



## SSEM

Implementing loops with a step other than one is precisely as easy (or as fiddly) as implementing loops with a step equal to one. This example program uses a loop to perform integer division. It should be run with the dividend in storage location 21 and the divisor in storage location 22. To show that it works, we shall ask the machine to count from 387 in steps of -5 and to halt with the accumulator showing the number of times it has done so before producing a negative result.

```ssem
10101000000000100000000000000000   0. -21 to c
00101000000001100000000000000000   1. c to 20
00101000000000100000000000000000   2. -20 to c
01101000000000010000000000000000   3. Sub. 22
10101000000001100000000000000000   4. c to 21
00000000000000110000000000000000   5. Test
01001000000001000000000000000000   6. Add 18 to CI
00011000000000000000000000000000   7. 24 to CI
11101000000000100000000000000000   8. -23 to CI
01001000000000010000000000000000   9. Sub. 18
00101000000001100000000000000000  10. c to 20
00101000000000100000000000000000  11. -20 to c
11101000000001100000000000000000  12. c to 23
11001000000000000000000000000000  13. 19 to CI
11101000000000100000000000000000  14. -23 to c
00101000000001100000000000000000  15. c to 20
00101000000000100000000000000000  16. -20 to c
00000000000001110000000000000000  17. Stop
10000000000000000000000000000000  18. 1
11111111111111111111111111111111  19. -1
00000000000000000000000000000000  20. 0
11000001100000000000000000000000  21. 387
10100000000000000000000000000000  22. 5
00000000000000000000000000000000  23. 0
10110000000000000000000000000000  24. 13
```

After executing 1,012 instructions, the computer halts with the correct quotient—77—in the accumulator.


## Stata


```stata
forvalues i=1(2)10 {
	display "`i'"
}

1
3
5
7
9
```


## Swift

This prints all odd digits:

```swift
for i in 1.stride(to: 10, by: 2) {
  print(i)
}
```

Alternately (removed in Swift 3):

```swift
for var i = 1; i < 10; i += 2 {
  print(i)
}
```



## Tcl


```tcl
for {set i 2} {$i <= 8} {incr i 2} {
    puts -nonewline "$i, "
}
puts "enough with the cheering already!"
```


=={{header|TI-83 BASIC}}==
Prints numbers from 0 to 100 stepping by 5.

```ti83b
:For(I,0,100,5
:Disp I
:End
```


=={{header|TI-89 BASIC}}==

```ti89b
Local i
For i, 0, 100, 5
    Disp i
EndFor
```



## TorqueScript



```TorqueScript
for(%i = 0; %i < 201; %i += 2)
{
	echo(%i);
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
LOOP i=2,9,2
PRINT i
ENDLOOP

```

{{out}}

```txt

2
4
6
8

```



## UNIX Shell

All these loops iterate 2, 4, 6, 8.
=
## Bourne Shell
=
{{works with|Bourne Shell}}

```bash
x=2
while test $x -le 8; do
	echo $x
	x=`expr $x + 2` || exit $?
done
```


{{works with|Bourne Shell}}
{{libheader|jot}}

```bash
for x in `jot - 2 8 2`; do echo $x; done
```


=
## Korn Shell
=
{{works with|Korn Shell}}

```bash
x=2
while [[$x -le 8]]; do
	echo $x
	((x=x+2))
done
```

{{works with|Korn Shell}}

```bash
x=2
while ((x<=8)); do
	echo $x
	((x+=2))
done
```



### Bourne Again Shell

{{works with|Bourne Again SHell|3}}

```bash
for (( x=2; $x<=8; x=$x+2 )); do
  printf "%d, " $x
done
```


{{works with|Bourne Again SHell|4}}
Bash v4.0+ has inbuilt support for setting up a step value

```bash
for x in {2..8..2} 
do
  echo $x
done
```


=
## C Shell
=
{{libheader|jot}}

```csh
foreach x (`jot - 2 8 2`)
	echo $x
end
```



## Ursa

{{trans|Python}}

```ursa
decl int i
for (set i 2) (< i 9) (set i (int (+ i 2)))
	out i ", " console
end for
out "who do we appreciate?" endl console
```



## VAX Assembly


```VAX Assembly
                               0000  0000     1 .entry	main,0
                            50   D4  0002     2 	clrf	r0				;init to 0.0
                                     0004     3 loop:
                                 01  0004     4 	nop					;do nothing
             FFF9 50   0A   3E   4F  0005     5 	acbf	#112.0, #1.25, r0, loop		;limit, step
                                     000B     6 
                                 04  000B     7 	ret
                                     000C     8 .end	main
```



## Vedit macro language

This prints all odd digits in range 1 to 9: 

```vedit
for (#1 = 1; #1 < 10; #1 += 2) {
    Num_Type(#1)
}
```



## VBA


```vb
Sub MyLoop()
    For i = 2 To 8 Step 2
        Debug.Print i;
    Next i
    Debug.Print
End Sub
```

{{out}}

```txt

 2  4  6  8 

```



## VBScript


```vb
buffer = ""
For i = 2 To 8 Step 2
    buffer = buffer & i & " "
Next
WScript.Echo buffer
```

{{out}}

```txt
2 4 6 8
```



## Vim Script


```vim
for i in range(2, 10, 2)
    echo i
endfor
```


{{Out}}

```txt
2                                                                                                                 
4                                                                                                                 
6                                                                                                                 
8                                                                                                                 
10
```




## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb
Sub MyLoop()
    For i = 2 To 8 Step 2
        Debug.Print i;
    Next i
    Debug.Print
End Sub
```

{{out}}

```txt

 2  4  6  8 

```



## Visual Basic .NET

{{works with|Visual Basic .NET|2011}}

```vbnet
Public Class FormPG
    Private Sub FormPG_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim i As Integer, buffer As String
        buffer = ""
        For i = 2 To 8 Step 2
            buffer = buffer & i & " "
        Next i
        Debug.Print(buffer)
    End Sub
End Class
```

{{out}}

```txt

2 4 6 8 

```



## Vorpal


```vorpal
for(i = 2, i <= 8, i = i + 2){
   i.print()
}
```



## Wart


```wart
for i 2 (i <= 8) (i <- i+2)
  prn i
```



## XPL0

The 'for' loop always steps by 1 (or -1 for 'downto'). However there is
no restriction on how the control variable can be used or manipulated,
thus a step by 2 can be implemented like this:


```XPL0
include c:\cxpl\codes;
int I;
[for I:= 2 to 8 do
        [IntOut(0, I);  Text(0, ", ");
        I:= I+1;
        ];
Text(0, "who do we appreciate?");
]
```


{{out}}

```txt

2, 4, 6, 8, who do we appreciate?

```



## zkl


```zkl
foreach n in ([1..10,4]) {println(n)}
[1..10,3].pump(Console.println)
```

{{out}}

```txt

1
5
9

1
4
7
10

```

A few others:

```zkl
fcn loop(i=0){println(i); if(i<10)return(self.fcn(i+2))}
(0).pump(10,Console.println,fcn(n){if(n%2)return(Void.Skip); n})
```




## ZX Spectrum Basic



```basic
10 FOR l = 2 TO 8 STEP 2
20 PRINT l; ", ";
30 NEXT l
40 PRINT "Who do we appreciate?"
```


{{omit from|GUISS}}
