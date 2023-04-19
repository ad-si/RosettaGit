+++
title = "Loops/Do-while"
description = ""
date = 2019-10-20T17:13:45Z
aliases = []
[extra]
id = 2824
[taxonomies]
categories = []
tags = []
+++

{{task|Iteration}}
[[Category:Conditional loops]]
[[Category:Simple]]

Start with a value at 0. Loop while value mod 6 is not equal to 0.
Each time through the loop, add 1 to the value then print it.
The loop must execute at least once.


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


;Reference:
* [[wp:Do while loop|Do while loop]] Wikipedia.





## 360 Assembly

;Basic
The WTO macro is in SYS1.MACLIB, which needs to be in the SYSLIB concatenation at assembly.

```360asm
*        Do-While
DOWHILE CSECT  ,                  This program's control section
         BAKR  14,0               Caller's registers to linkage stack
         LR    12,15              load entry point address into Reg 12
        USING  DOWHILE,12         tell assembler we use Reg 12 as base
         XR    9,9                clear Reg 9 - divident value
         LA    6,6                load divisor value 6 in Reg 6
         LA    8,WTOLEN           address of WTO area in Reg 8
LOOP     DS    0H
         LA    9,1(,9)            add 1 to divident Reg 9
         ST    9,FW2              store it
         LM    4,5,FDOUBLE        load into even/odd register pair
         STH   9,WTOTXT           store divident in text area
         MVI   WTOTXT,X'F0'       first of two bytes zero
         OI    WTOTXT+1,X'F0'     make second byte printable
        WTO    TEXT=(8)           print it (Write To Operator macro)
         DR    4,6                divide Reg pair 4,5 by Reg 6
         LTR   5,5                test quotient (remainder in Reg 4)
         BNZ   RETURN             if one: 6 iterations, exit loop.
         B     LOOP               if zero: loop again.
RETURN   PR    ,                  return to caller.
FDOUBLE  DC    0FD
         DC    F'0'
FW2      DC    F'0'
WTOLEN   DC    H'2'               fixed WTO length of two
WTOTXT   DC    CL2' '
         END   DOWHILE
```

;Structured Macros
Although specified at the beginning (DO UNTIL), the test is done at the end of the loop (ENDDO).
Structured macros (DO ENDDO) weren't in the 1963 standard of Assembler 360, but there are part of it since since 1998.

```360asm
*        Do-While                  27/06/2016
DOWHILE  CSECT
         USING DOWHILE,12          set base register
         LR    12,15               init base register
         SR    6,6                 v=0
         LA    4,1                 init reg 4
         DO UNTIL=(LTR,4,Z,4)      do until v mod 6=0
         LA    6,1(6)                v=v+1
         STC   6,WTOTXT              v
         OI    WTOTXT,X'F0'          make editable
         WTO   MF=(E,WTOMSG)         display v
         LR    4,6                   v
         SRDA  4,32                  shift dividend to reg 5
         D     4,=F'6'               v/6  so r4=remain & r5=quotient
         ENDDO ,                   end do
         BR    14                  return to caller
WTOMSG   DS    0F                  full word alignment for wto
WTOLEN   DC    AL2(L'WTOTXT+4)     length of WTO buffer
         DC    H'0'                must be zero
WTOTXT   DS    C                   one char
         END   DOWHILE
```



## 6502 Assembly

Code is called as a subroutine (i.e. JSR DoWhileSub).  Specific OS/hardware routines for printing are left unimplemented.

```6502asm
DoWhileSub:	PHA
		TYA
		PHA			;push accumulator and Y register onto stack

		LDY #0
DoWhileLoop:	INY
		JSR DisplayValue	;routine not implemented
		TYA
		SEC
Modulus:	SBC #6
		BCS Modulus
		ADC #6
		BNE DoWhileLoop

		PLA
		TAY
		PLA			;restore Y register and accumulator from stack
		RTS			;return from subroutine
```



## ActionScript


```actionscript
var val:int = 0;
do
{
    trace(++val);
} while (val % 6);
```



## Ada


```ada
loop
   Value := Value + 1;
   Put (Value);
   exit when Value mod 6 = 0;
end loop;
```

Here is an alternative version:

```ada
for Value in 0..Integer'Last loop
   Put (Value);
   exit when Value mod 6 = 0;
end loop;
```



## Agena

Tested with Agena 2.9.5 Win32

```agena
scope
    local i := 0;
    do
        inc i, 1;
        print( i )
    as ( i % 6 ) <> 0
epocs
```



## Aime


```aime
integer a;

a = 0;
do {
   a += 1;
   o_integer(a);
   o_byte('\n');
} while (a % 6 != 0);
```



## ALGOL 60

{{works with|ALGOL 60|OS/360}}
No stuctured control instructions in Algol 60 to perform this task. Use of 2 harmful GOTOs. I agree Edsger Dijkstra  communication "Go To Statement Considered Harmful", ACM 1968.

```algol60
'BEGIN' 'COMMENT' Loops DoWhile  - Algol60 - 22/06/2018;
  'INTEGER' I;
  I:=0;
LOOP:
    I:=I+1;
    'IF' I=I'/'6*6 'THEN' 'GOTO' LAB;
    OUTINTEGER(1,I);
  'GOTO' LOOP;
LAB:
'END'
```

{{out}}

```txt

         +1           +2           +3           +4           +5

```




## ALGOL 68


```algol68
FOR value WHILE
  print(value);
# WHILE # value MOD 6 /= 0 DO
  SKIP
OD
```



## ALGOL W


```algolw
begin
    integer i;
    i := 0;
    while
        begin
            i := i + 1;
            write( i );
            ( i rem 6 ) not = 0
        end
    do begin end
end.
```



## AmigaE


```amigae
PROC main()
  DEF i = 0
  REPEAT
    i := i + 1
    WriteF('\d\n', i)
  UNTIL Mod(i, 6) = 0
ENDPROC
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program loopdowhile.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

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
    add r4,#1                   @ increment counter
    mov r0,r4
    mov r1,#6              @ division conuter by 6
    bl division
    cmp r3,#0              @ remainder = zéro ?
    bne 1b                @ no ->begin loop one

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



## AutoHotkey


```AutoHotkey
While mod(A_Index, 6) ;comment:everything but 0 is considered true
  output = %output%`n%A_Index%
MsgBox % output
```



## AWK


```awk
BEGIN {
  val = 0
  do {
    val++
    print val
  } while( val % 6 != 0)
}
```



## Axe

While Axe does not have explicit do-while loops, they can be easily emulated using an infinite loop with a conditional terminator:

```axe
0→A
While 1
 A++
 Disp A▶Dec,i
End!If A^6
```



## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
a = 0
do
  a = a + 1
  print a
loop while a mod 6 <> 0
```


=
## BaCon
=


```freebasic


a=0
REPEAT
    INCR a
    PRINT a
UNTIL  MOD(a,6)  == 0


```


=
## BBC BASIC
=

```bbcbasic
a = 0
REPEAT
  a = a + 1
  PRINT a
UNTIL a MOD 6 = 0
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET I=0
110 DO
120   LET I=I+1
130   PRINT I
140 LOOP UNTIL MOD(I,6)=0
```


=
## Sinclair ZX81 BASIC
=

```basic
10 LET X=0
20 LET X=X+1
30 PRINT X
40 IF X/6<>INT (X/6) THEN GOTO 20
```



## bc


```bc
i = 0
for (;;) {
	++i	/* increments then prints i */
	if (i % 6 == 0) break
}
quit
```



## Befunge


```befunge>0
1+:.v
 |%6: <
 @
```



## C


```c
int val = 0;
do{
   val++;
   printf("%d\n",val);
}while(val % 6 != 0);
```



## ChucK

<lang>
0 => int value;
do
{
    value++;
    <<<value>>>;
}
while(value % 6 != 0);

```



## C++


```cpp
int val = 0;
do{
   val++;
   std::cout << val << std::endl;
}while(val % 6 != 0);
```


## C#


```c#
int a = 0;

do
{
    a += 1;
    Console.WriteLine(a);
} while (a % 6 != 0);
```



## Chapel


```chapel
var val = 0;
do {
        val += 1;
        writeln(val);
} while val % 6 > 0;
```



## Clipper


```clipper
   Local n := 0
   DO WHILE .T.
      ? ++n
      IF n % 6 == 0
         EXIT
      ENDIF
   ENDDO
```



## COBOL

The COBOL equivalent of a do-while loop is <code>PERFORM WITH TEST AFTER UNTIL some-condition</code>.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. loop-do-while.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i PIC 99 VALUE 0.

       PROCEDURE DIVISION.
           PERFORM WITH TEST AFTER UNTIL FUNCTION MOD(i, 6) = 0
               ADD 1 TO i
               DISPLAY i
           END-PERFORM

           GOBACK
           .
```



## Coco

Do-while loops are a JavaScript feature removed in CoffeeScript but re-added in Coco.


```coco
v = 0
do
   console.log ++v
while v % 6
```



## CoffeeScript

CoffeeScript doesn't have <code>do {} while ()</code> loop, but it can be emulated using <code>loop</code> statement and <code>break unless</code> statement.

```coffeescript
val = 0
loop
  console.log ++val
  break unless val % 6
```



## ColdFusion


```cfm><cfscript

  value = 0;
  do
  {
    value += 1;
    writeOutput( value );
  } while( value % 6 != 0 );
</cfscript>
```



## Common Lisp


```lisp
(let ((val 0))
  (loop do
        (incf val)
        (print val)
        while (/= 0 (mod val 6))))
```


loop can set up temporary values, and incf returns a value, so it's also possible to do


```lisp
(loop with val = 0
      do (print (incf val))
      until (= 0 (mod val 6)))
```



## Clojure


```Clojure
(loop [i 0]
  (let [i* (inc i)]
    (println i*)
    (when-not (zero? (mod i* 6))
      (recur i*))))
```



## D


```d
import std.stdio;

void main() {
    int val;
    do {
        val++;
        write(val, " ");
    } while (val % 6 != 0);
}
```

{{out}}

```txt
1 2 3 4 5 6
```



## dc

{{trans|bc}}

```dc
0 si		[i = 0]sz
[2Q]sA		[A = code to break loop]sz
[
 li 1 + p	[print it = i + 1]sz
 d si		[i = it, leave it on stack]sz
 6 % 0 =A	[call A if 0 == it % 6]sz
 0 0 =B		[continue loop]sz
]sB 0 0 =B
```



## Delphi


```Delphi
program Loop;

{$APPTYPE CONSOLE}

var
  I: Integer;

begin
  I:= 0;
  repeat
    Inc(I);
    Write(I:2);
  until I mod 6 = 0;
  Writeln;
  Readln;
end.
```



## Dragon



```dragon
val = 0
do{
   val++
   showln val
}while(val % 6 != 0)
```



## DUP


DUP only provides a while loop in the form of <code DUP>[condition][block]#</code>, where the block is executed in a loop as long as the condition is nonzero/true.
A do-while loop is technically nothing more than executing the block once before running an ordinary while loop, so we simply define an operator or function that contains the block (comments in curly braces):


```DUP
[1+$.' ,]⇒A   {operator definition: PUSH 1, ADD, DUP, print top of stack to SDTOUT, print whitespace}
[1+$.' ,]a:    {function definition}
```


and put the defined block in front of the while loop, and inside the while loop itself:

If the block was defined as an operator, the whole program would look like this (comments in curly braces):


```DUP
[1+$.' ,]⇒A
0 A[$6/%][A]#    {PUSH 0, execute operator A, [DUP, PUSH 6, MOD/DIV, POP][execute operator A]#}
```


And if the block is defined as a named function:


```DUP
[1+$.' ,]a:
0 a;![$6/%][a;!]#
```


Result:


```DUP>1 2 3 4 5 6</lang



## DWScript


```Delphi

var i := 0;

repeat
   Inc(i);
   PrintLn(i);
until i mod 6 = 0;

```

'''Bold text'''


## E

E does not have an official do-while construct, but the primitive which loops are built out of (which calls a function which returns a boolean indicating whether it should be called again) can be used to construct a do-while.

```e
var x := 0
__loop(fn {
    x += 1
    println(x)
    x % 6 != 0   # this is the return value of the function
})
```


<!-- XXX we should have an example of lambda-args sugar here -->


## Ela



```ela
open monad io

loop n | n % 6 == 0 = do return ()
       | else = do
          putStrLn (show n)
          loop (n+1)

_ = loop 10 ::: IO
```



## Elixir


```elixir
defmodule Loops do
  def do_while(n) do
    n1 = n + 1
    IO.puts n1
    if rem(n1, 6) == 0, do: :ok,
                      else: do_while(n1)
  end
end

Loops.do_while(0)
```



## Emacs Lisp

The condition form for <code>while</code> can be a <code>progn</code> to evaluate arbitrary code before the loop condition.  The body of a <code>while</code> can be empty.


```Lisp
(let ((val 0))
  (while (progn
           (setq val (1+ val))
           (message "%d" val)
           (/= 0 (mod val 6)))))
```



## Erlang



```Erlang

do() ->
	do(0).

do(0) ->
	io:fwrite( "0 " ),
        do( 1 );
do(N) when N rem 6 =:= 0 ->
	io:format("~w~n", [N]);
do(N) ->
	io:fwrite( "~p ", [N] ),
	do(N+1).

```



## ERRE


```ERRE

A=0
REPEAT
  A=A+1
  PRINT(A)
UNTIL A MOD 6=0  !UNTIL A-6*INT(A/6)=0 for C-64

```



## Euphoria

{{works with|Open Euphoria}}

```euphoria

include std/console.e
include std/math.e

atom x = 0

loop do
	x += 1
	?x
	until(mod(x,6)) = 0
end loop

if getc(0) then end if

```



## F#

If you must have a loop then this is acceptable F#

```fsharp

let rec loop n =
  printfn "%d " n
  if (n+1)%6 > 0 then loop (n+1)
loop 0

```


But I prefer this way:

```fsharp

Seq.initInfinite id |> Seq.takeWhile(fun n->n=0 || n%6>0) |> Seq.iter (fun n-> printfn "%d" n)

```


Either produces:
{{out}}

```txt

0
1
2
3
4
5

```



## Factor


```factor
0 [ dup 6 mod 0 = not ] [ [ . ] [ 1 + ] bi ] do while drop
```



## Fantom


There is no do-while statement in Fantom, so instead use an infinite while loop with a break statement:


```fantom

class Main
{
  public static Void main ()
  {
    i := 0
    while (true)
    {
      i += 1
      echo (i)
      if (i % 6 == 0) break // end loop on condition
    }
  }
}

```



## Forth


```forth
: do-until
  0
  begin 1+
        dup .
        dup 6 mod 0=
  until
  drop ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
INTEGER :: i = 0
DO
  i = i + 1
  WRITE(*, *) i
  IF (MOD(i, 6) == 0) EXIT
END DO
```


{{works with|Fortran|77 and later}}

```fortran
      PROGRAM DOWHILE
C Initialize modulus and value.
        INTEGER MODLUS, IVALUE
        PARAMETER (MODLUS = 6)
        IVALUE = 0

C FORTRAN 77 has no do-while structure -- not semantically. It is not
C difficult to simulate it using GOTO, however:
   10   CONTINUE
          IVALUE = IVALUE + 1
          WRITE (*,*) IVALUE
        IF (.NOT. (MOD(IVALUE, MODLUS) .EQ. 0)) GOTO 10

        STOP
      END
```


{{works with|Fortran|IV and later}}

```fortran
      IVALUE = 0
   10 CONTINUE
        IVALUE=IVALUE+1
        WRITE(6,301) IVALUE
  301   FORMAT(I5)
      IF(MOD(IVALUE,6).NE.0) GOTO 10
      END
```


{{works with|Fortran|I and later}}

```fortran
      IVALUE = 0
   10 IVALUE=IVALUE+1
      WRITE 301,IVALUE
  301 FORMAT(I5)
      IF(IVALUE-IVALUE/6*6) 10,20,10
   20 STOP
      END
```



## Fortress


Due to the way that Fortress works, you have to use a label to escape a loop upon a specified condition being met. There is no traditional <code>break</code> equivalent.


```fortress

component loops_do_while
  export Executable

  var x:ZZ32 = 0
  run() = label loop
    while true do
      x += 1
      println(x)

      if (x MOD 6) = 0
      then exit loop
      end
    end
  end loop
end

```


{{out}}

```txt

1
2
3
4
5
6

```



## FreeBASIC


```freebasic
' FB 1.05. 0 Win64

Dim i As Integer = 0
Do
  i += 1
  Print i; " ";
Loop While i Mod 6 <> 0
Print
Sleep
```


{{out}}

```txt

 1  2  3  4  5  6

```



## Frink


```frink
n = 0
do
{
   n = n + 1
   println[n]
} while n mod 6 != 0
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long i

do
  i++
  print i
until ( i mod 6 == 0 )

```

Output:

```txt

 1
 2
 3
 4
 5
 6

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=57e91eab60baf7e39df9b6d16a0deddd Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short

Repeat
  Inc siCount
  Print siCount;;
Until siCount Mod 6 = 0

End
```

Output:

```txt

1 2 3 4 5 6

```



## GAP


```gap
n := 0;
repeat
    n := n + 1;
    Print(n, "\n");
until RemInt(n, 6) = 0;
```



## Go

There is no explicit do-while in Go, but it can be simulated with a range-based for loop and the break statement.

```go
package main

import "fmt"

func main() {
	var value int
	for {
		value++
		fmt.Println(value)
                if value%6 != 0 {
                        break
                }
	}
}
```

{{out}}

```txt
1
2
3
4
5
6
```

It can also be simulated ''without'' using a break statement as follows:

```go
package main

import "fmt"

func main() {
	var value int
	for ok := true; ok; ok = value%6 != 0 {
		value++
		fmt.Println(value)
	}
}
```


{{out}}

```txt

Same as before.

```



## GML


```GML
i = 0
do
    {
    i += 1
    show_message(string(i))
    }
until (i mod 6 = 0)
```



## Groovy

Groovy does not have a bottom-checking loop construct! So use an "infinite" while loop with a conditional break as the last statement

```groovy
def i = 0
while (true) {
    i++
    println i
    if ( i % 6 == 0) break
}
```


{{out}}

```txt
1
2
3
4
5
6
```


=={{header|GW-BASIC}}==
GW-BASIC does not have a <code>do .. while</code> construct.
Equivalent using <code>WHILE</code>:
{{works with|PC-BASIC|any}}

```qbasic

10 LET I% = 0
20 ' first iteration - before the WHILE
30 PRINT I%
40 LET I% = I% + 1
50 WHILE I% MOD 6 <> 0
60  PRINT I%
70  LET I% = I% + 1
80 WEND

```

Equivalent using <code>GOTO</code>:
{{works with|PC-BASIC|any}}

```qbasic

10 LET I% = 0
20  PRINT I%
30  LET I% = I% + 1
40  IF I% MOD 6 <> 0 THEN GOTO 20

```



## Harbour


```visualfoxpro
LOCAL n := 0

DO WHILE .T.
   ? ++n
   IF n % 6 == 0
      EXIT
   ENDIF
ENDDO
```



## Haskell



```haskell
import Data.List
import Control.Monad
import Control.Arrow

doWhile p f n = (n:) $ takeWhile p $ unfoldr (Just.(id &&& f)) $ succ n
```

Example executed in GHCi:

```haskell
*Main> mapM_ print $ doWhile ((/=0).(`mod`6)) succ 0
0
1
2
3
4
5
```


The standard Prelude also includes, without further import or definition, an '''until''' function, which takes three arguments – a predicate function, a transformation function, and an initial value.


```haskell
main :: IO ()
main =
  mapM_ print . reverse $
  until
    (\(x:_) -> (x > 0) && (mod x 6 == 0))
    (\xs@(x:_) -> succ x : xs)
    [0]
```


{{Out}}

```txt
0
1
2
3
4
5
6

```


###  With mutable references

Using iterateWhile from monad-loops package

```haskell
import Data.IORef
import Control.Monad.Loops

main = do
  x <- newIORef 0;
  iterateWhile (\val -> val `mod` 6 /= 0 ) $ do
    modifyIORef x (+1)
    val <- readIORef x
    print val
    return val
```



## HolyC


```holyc
U8 i = 0;
do {
   i++;
   Print("%d\n", i);
} while (i % 6 != 0);
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon do not have a do-while looping control with end of loop checking.  There are four looping controls 'every', 'repeat', 'until', and 'while' (see [[Icon%2BUnicon/Intro#Looping_Controls|Introduction to Icon and Unicon/Looping Controls]] for more information.)

```Icon
procedure main()

i := 0
repeat {
   write(i +:= 1)
   if i % 6 = 0 then break
   }
end
```



## J

J is array-oriented, so there is very little need for loops.  For example, one could satisfy this task this way:

   ,. ([^:(0=6|])>:)^:a: 0

J does support loops for those times they can't be avoided (just like many languages support gotos for those time they can't be avoided).

```j
3 : 0 ] 0

         NB.  The 'st' in 'whilst' stands for 'skip test'

         whilst. 0 ~: 6 | y do.
             y 1!:2 ]2
             y =. y+1
         end.

        i.0 0
   )
```


Though it's rare to see J code like this.


## Java



```java
int val = 0;
do{
   val++;
   System.out.println(val);
}while(val % 6 != 0);
```



## JavaScript



### Javascript: Imperative


```javascript
var val = 0;
do {
  print(++val);
} while (val % 6);
```



### Javascript: Functional


### =ES5=

In a functional idiom of JavaScript we cannot use a Do While '''statement''', as it returns no value and is not a composable expression. We can, however achieve the same effect with a composable doWhile '''function''', which takes three arguments, and returns the output series as a value.

:#An initial value,
:#a Do function which transforms that value repetitively, corresponding to the body of the loop,
:#and a conditional While function.


```JavaScript
function doWhile(varValue, fnBody, fnTest) {
  'use strict';
  var d = fnBody(varValue); // a transformed value

  return fnTest(d) ? [d].concat(
    doWhile(d, fnBody, fnTest)
  ) : [d];
}

console.log(
  doWhile(0,           // initial value
    function (x) {     // Do body, returning transformed value
      return x + 1;
    },
    function (x) {     // While condition
      return x % 6;
    }
  ).join('\n')
);
```


Output:

```JavaScript
1
2
3
4
5
6
```


Alternatively, if we assume instead that the unstated problem was not to produce repetitive computation, but to derive the '''membership of a set''' we could interpret the task as a request for a JavaScript implementation of the '''takeWhile''' function – a familiar staple of functional list processing.

So, for example, something like:


```JavaScript
function range(m, n) {
  'use strict';
  return Array.apply(null, Array(n - m + 1)).map(
    function (x, i) {
      return m + i;
    }
  );
}

function takeWhile(lst, fnTest) {
 'use strict';
  var varHead = lst.length ? lst[0] : null;

  return varHead ? (
    fnTest(varHead) ? [varHead].concat(
      takeWhile(lst.slice(1), fnTest)
    ) : []
  ) : []
}

console.log(
  takeWhile(
    range(1, 100),
    function (x) {
      return x % 6;
    }
  ).join('\n')
);
```


Output:

```JavaScript
1
2
3
4
5
```



### =ES6=


A process or value of this kind might be better expressed (in functionally composed JavaScript) with an '''unfold''' or '''until''' function, returning a list.


```JavaScript
(() => {
    'use strict';

    // unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    function unfoldr(mf, v) {
        for (var lst = [], a = v, m;
            (m = mf(a)) && m.valid;) {
            lst.push(m.value), a = m.new;
        }
        return lst;
    }

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    function until(p, f, x) {
        let v = x;
        while(!p(v)) v = f(v);
        return v;
    }

    let result1 = unfoldr(
        x => {
            return {
                value: x,
                valid: (x % 6) !== 0,
                new: x + 1
            }
        },
        1
    );

    let result2 = until(
        m => (m.n % 6) === 0,
        m => {
            return {
                n : m.n + 1,
                xs : m.xs.concat(m.n)
            };
        },
        {
            n: 1,
            xs: []
        }
    ).xs;

    return [result1, result2];
})();

```




```JavaScript
[[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]]
```


ES6 is a superset of Javascript so the Javascript and ES5 solution is valid.  An example of a do-while loop in a generator follows that produces correct output:


```JavaScript

// generator with the do while loop
function* getValue(stop) {
    var i = 0;
    do {
        yield ++i;
    } while (i % stop != 0);
}

// function to print the value and invoke next
function printVal(g, v) {
    if (!v.done) {
        console.log(v.value);
        setImmediate(printVal, g, g.next());
    }
}

(() => {
    var gen = getValue(6);
    printVal(gen, gen.next());
})();

```



```JavaScript

1
2
3
4
5
6

```



## jq

{{works with|jq|1.4}}
In jq 1.4, the "recurse" built-in always emits the input value, and so to accomplish the task specified here,
we shall define a control structure: "do_while(action; condition)" as follows:

```jq
# Perform the action, then check the condition, etc
def do_while( action; condition ):
  def w: action | if (condition | not) then empty else ., w end;
  w;
```

'''The task:'''

```jq
0 | do_while( .+1; . % 6 != 0 )
```

{{out}}
 1
 2
 3
 4
 5


## Julia

Julia has no do-while construct.  Here is one of several ways to implement do-while behavior.


```Julia

julia> i = 0
0

julia> while true
           println(i)
           i += 1
           i % 6 == 0 && break
       end
0
1
2
3
4
5

```


Using a macro that mimics the classic C style do-while.

Notice that the symbol <code>while</code> cannot be used as it is a keyword, which is why <code>when</code> is used instead, also the macro definition is wrapped in a <code>@eval</code> macro invocation since <code>do</code> is also a keyword, but in Julia macro calls are prefixed by <code>@</code> so this is only an issue during the macro definition, not when invoked, ie. <code>@do block when condition</code>).


```Julia

julia> @eval macro $(:do)(block, when::Symbol, condition)
           when ≠ :when && error("@do expected `when` got `$s`")
           quote
               let
                   $block
                   while $condition
                       $block
                   end
               end
           end |> esc
       end
@do (macro with 1 method)

julia> i = 0
0

julia> @do begin
           @show i
           i += 1
       end when i % 6 ≠ 0
i = 0
i = 1
i = 2
i = 3
i = 4
i = 5

```


Here is mostly the same macro, but with the conditional clause used first, which is arguably more readable.


```Julia

julia> macro do_while(condition, block)
           quote
               let
                   $block
                   while $condition
                       $block
                   end
               end
           end |> esc
       end
@do_while (macro with 1 method)

julia> i = 0
0

julia> @do_while i % 6 ≠ 0 begin
           @show i
           i += 1
       end
i = 0
i = 1
i = 2
i = 3
i = 4
i = 5

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    var value = 0
    do {
        println(++value)
    }
    while (value % 6 != 0)
}
```


{{out}}

```txt

1
2
3
4
5
6

```



## LabVIEW

{{VI snippet}}<br/>[[File:LabVIEW_Loops_Do-while.png]]


## Lasso


```Lasso
local(x = 0)
while(#x % 6 > 0 || #x == 0) => {^
	++#x
	'\r' // for formatting
^}
```



## Liberty BASIC


```lb

a = 0
do
  a =a +1
  print a
loop until ( a mod 6) = 0

```



## Lingo

Lingo has no do..while, but here how this behavior can be implemented:

```lingo
i = 0
repeat while TRUE
  i = i+1
  put i
  if i mod 6 = 0 then exit repeat
end
```



## Lisaac


```Lisaac
+ val : INTEGER;
{
  val := val + 1;
  val.print;
  '\n'.print;
  val % 6 != 0
}.while_do { };
```



## LiveCode


```LiveCode
repeat while n mod 6 is not 0 or n is 0
    add 1 to n
    put n
end repeat
```



## Lua


Lua doesn't have a <code>do .. while</code> construct.


```lua

i=0
repeat
  i=i+1
  print(i)
until i%6 == 0

```



## Logo


```logo
make "val 0
do.while [make "val :val + 1  print :val] [notequal? 0 modulo :val 6]
do.until [make "val :val + 1  print :val] [equal? 0 modulo :val 6]

to my.loop :n
  make "n :n + 1
  print :n
  if notequal? 0 modulo :n 6 [my.loop :n]
end
my.loop 0
```



## M2000 Interpreter


```M2000 Interpreter

Module checkit {
      x=0
      \\ Do or Repeat
      Do {
            x++
            Print x
      } Until x mod 6=0

      x=0
      {
            \\ when enter to block the loop flag change to false
            x++
            if x mod 6<>0 Then loop   ' set loop flag of current block to true
            \\ when block end check Loop flag and if true execute block again
            Print X
      }
}
Checkit

```


{{out}}

```txt

1
2
3
4
5
6
1
2
3
4
5
6

```



## Maple


```Maple
val := 0:
do
        val := 1 + val;
        print( val );
        if irem( val, 6 ) = 0 then
                break
        end if;
end do:
```



## Mathematica

Because everything is an expression in Mathematica, <code>While[body;condition]</code> tests <code>condition</code> after <code>body</code> has been executed at least once.

```Mathematica
value = 0;
While[
 value++;
 Print[value];
 Mod[value,6]!=0
]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
   a=0;
   while (1)
      a = a+1;
      disp(a);
   if (~mod(a,6)) break; end;
   end;
```



## Maxima


```maxima
block([n: 0], do (ldisp(n: n + 1), if mod(n, 6) = 0 then return('done)))$
```



## MAXScript


```maxscript
a = 0
do
(
    print a
    a += 1
)
while mod a 6 != 0
```



## Metafont


Metafont has no a do-while construct; the same thing can be done using a forever loop and exitif.


```metafont
a := 0;
forever: show a; a := a + 1; exitif a mod 6 = 0; endfor
end
```



## Microsoft Small Basic

Microsoft Small Basic does not have a <code>do .. while</code> construct.
Equivalent using <code>While</code>:

```microsoftsmallbasic

i = 0
' first iteration - before the While
TextWindow.WriteLine(i)
i = i + 1
While Math.Remainder(i, 6) <> 0
  TextWindow.WriteLine(i)
  i = i + 1
EndWhile

```

Equivalent using <code>Goto</code>:

```microsoftsmallbasic

i = 0
loopStart:
TextWindow.WriteLine(i)
i = i + 1
If Math.Remainder(i, 6) <> 0 Then
  Goto loopStart
EndIf

```



## MIPS Assembly



```mips

	.text
main:	li 	$s0, 0		# start at 0.
	li	$s1, 6
loop:	addi	$s0, $s0, 1	# add 1 to $s0
	div	$s0, $s1	# divide $s0 by $s1. Result is in the multiplication/division registers
	mfhi	$s3		# copy the remainder from the higher multiplication register to $s3
	move	$a0, $s0	# variable must be in $a0 to print
	li	$v0, 1		# 1 must be in $v0 to tell the assembler to print an integer
	syscall			# print the integer in $a0
	bnez	$s3, loop	# if $s3 is not 0, jump to loop

	li	$v0, 10
	syscall			# syscall to end the program

```


=={{header|MK-61/52}}==
<lang>0	П4	КИП4	ИП4	6	/	{x}	x=0	02	С/П
```


=={{header|Modula-2}}==

```modula2
MODULE DoWhile;
  IMPORT InOut;

  VAR
    i: INTEGER;

BEGIN
  i := 0
  REPEAT
    InOut.WriteInt(i, 1);
    InOut.WriteLn;
    INC(i)
  UNTIL i MOD 6 = 0;
END DoWhile.
```


=={{header|Modula-3}}==
This is very similar to the [[Modula-2]] code above.

```modula3
REPEAT
  i := i + 1;
  IO.Put(Fmt.Int(i));
UNTIL i MOD 6 = 0;
```



## Monicelli

The do-while loop is the only kind of loop available in Monicelli

```monicelli

stuzzica
    ...     # loop body
e brematura anche, se <expr> # exit if <expr> is false

```



## MUMPS

{{works with|Caché ObjectScript}}

```MUMPS
DOWHILELOOP
    set val = 0
    do {
        set val = val + 1
        write val,!
    } while ((val # 6) '= 0)

    quit
```


{{out}}
```txt

SAMPLES>do ^DOWHILELOOP
1
2
3
4
5
6

```



## Neko


```ActionScript
/**
 Loops/Do-while in Neko
 Tectonics:
   nekoc loops-do-while.neko
   neko loops-do-while
*/

var index = 0;
do {
  index += 1;
  $print(index, "\n");
} while (index % 6) != 0
```


{{out}}

```txt
prompt$ nekoc loops-do-while.neko
prompt$ neko loops-do-while
1
2
3
4
5
6
```



## Nemerle


```Nemerle
mutable x = 0;
do
{
    x++;
    WriteLine($"$x");
} while (x % 6 != 0)
```



## NetRexx

In NetRexx the '''do&ndash;while''' construct is implemented via the <code>until ''expru''</code> conditional clause of the <code>loop</code> instruction.  The expression ''expru'' in the <code>until ''expru''</code> clause is evaluated at the end of the loop, guaranteeing that the loop will be executed at least once.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/Do-while'

  i_ = 0
  loop until i_ // 6 = 0
    i_ = i_ + 1
    say i_
    end

```



## NewLISP


```NewLISP
(let ((i 0))
  (do-until (= 0 (% i 6))
	    (println (++ i))))
```



## Nim

Nim does not have a do-while loop, but it's easy to write your own:

```nim
template doWhile(a, b: untyped): untyped =
  b
  while a:
    b

var val = 1
doWhile val mod 6 != 0:
  val += 1
  echo val
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE LoopDoWhile;
IMPORT
  Out;

PROCEDURE Do();
VAR
  i: INTEGER;
BEGIN
  i := 0;
  REPEAT
    Out.LongInt(i,0);Out.Ln;
    INC(i)
  UNTIL (i MOD 6 = 0);
END Do;

BEGIN
  Do
END LoopDoWhile.

```



## OCaml

OCaml doesn't have a do-while loop, so we can just make a local loop:

```ocaml
let rec loop i =
  let i = succ i in
  Printf.printf "%d\n" i;
  if i mod 6 <> 0 then
    loop i
  in
  loop 0
```


or implementing a generic do-while iterator with higher order function:


```ocaml
let do_while f p =
  let rec loop() =
    f();
    if p() then loop()
  in
  loop()
(** val do_while : (unit -> 'a) -> (unit -> bool) -> unit *)
```



```ocaml
let v = ref 0 in
do_while (fun () -> incr v; Printf.printf "%d\n" !v)
         (fun () -> !v mod 6 <> 0)
```


The example above is the an imperative form, below is its functional counterpart:

```ocaml
let do_while f p ~init =
  let rec loop v =
    let v = f v in
    if p v then loop v
  in
  loop init

do_while (fun v ->
            let v = succ v in
            Printf.printf "%d\n" v;
            (v))
         (fun v -> v mod 6 <> 0)
         ~init:0
```


Or in a very poor OCaml style, we can use an exception to exit a while loop:

```ocaml
let v = ref 0
exception Exit_loop
try while true do
  incr v;
  Printf.printf "%d\n" !v;
  if not(!v mod 6 <> 0) then
    raise Exit_loop;
done
with Exit_loop -> ()
```



## Objeck


```objeck

i := 0;
do {
   i += 1;
   i->PrintLine();
}
while (i % 6 <> 0);

```



## Octave

The do-while can be changed into a do-until, just negating the condition of the while.

```octave
val = 0;
do
  val++;
  disp(val)
until( mod(val, 6) == 0 )
```



## Oforth



```Oforth>0 doWhile: [ 1+ dup . dup 6 rem 0 <
 ] drop
```



## OpenEdge/Progress


```progress
DEFINE VARIABLE ii AS INTEGER.

DO WHILE ii MODULO 6 <> 0 OR ii = 0:
   ii = ii + 1.
   MESSAGE ii VIEW-AS ALERT-BOX.
END.
```



## Oz

Normal  Oz variables are single-assignment only. So we use a "cell", which is a one-element mutable container.

```oz
declare
  I = {NewCell 0}
in
  for until:@I mod 6 == 0 do
     I := @I + 1
     {Show @I}
  end
```



## PARI/GP

The generic Pari loops (<code>while</code>, <code>until</code>) test at the beginning, so just use an infinite loop with a break.

```parigp
x = 0;
while(1,
  print(x++);
 if(x % 6 == 0, break)
)
```


If the loop body is something simple then it might be worked into the loop condition.  This is obscure but compact.


```parigp
x = 0;
while (print(x++) || x % 6, )
```


The condition in <code>while</code> and <code>until</code> is an expression, not a sequence, so <code>;</code> for multiple statements cannot be used there.


## Pascal


```pascal
program countto6(output);

var
  i: integer;

begin
  i := 0;
  repeat
    i := i + 1;
    writeln(i)
  until i mod 6 = 0
end.
```



## Perl


```perl
my $val = 0;
do {
   $val++;
   print "$val\n";
} while ($val % 6);
```

<code>do ... until (''condition'')</code> is equivalent to <code>do ... while (not ''condition'')</code>.

```perl
my $val = 0;
do {
   $val++;
   print "$val\n";
} until ($val % 6 == 0);
```



## Perl 6

{{works with|Rakudo Star|2010.08}}


```perl6
my $val = 0;
repeat {
    say ++$val;
} while $val % 6;
```


<code>repeat ... until ''condition''</code> is equivalent to <code>do ... while not ''condition''</code>.


```perl6
my $val = 0;
repeat {
    say ++$val;
} until $val %% 6;
```

(Here we've used <code>%%</code>, the "divisible-by" operator.)
<p>
You can also put the condition before the block, without changing the order of evaluation.


```perl6
my $val = 0;
repeat while $val % 6 {
    say ++$val;
}
```



## Phix


```Phix
integer x = 0
while 1 do
    x += 1
    ?x
    if mod(x,6)=0 then exit end if
end while
```



## PHL



```phl
var i = 0;
do {
	i = i::inc;
	printf("%i\n", i);
} while (i%6 != 0);
```



## PHP


```php
$val = 0;
do {
   $val++;
   print "$val\n";
} while ($val % 6 != 0);
```



## PicoLisp

Literally:

```PicoLisp
(let Val 0
   (loop
      (println (inc 'Val))
      (T (=0 (% Val 6))) ) )
```

Shorter:

```PicoLisp
(let Val 0
   (until (=0 (% (println (inc 'Val)) 6))) )
```

or:

```PicoLisp
(for (Val 0  (n0 (% (println (inc 'Val)) 6))))
```



## Pike


```pike
int main(){
   int value = 0;
   do {
      value++;
      write(value + "\n");
   } while (value % 6);
}
```



## PL/I


```pli

dcl value fixed bin (31) init (0);
do forever;
  value = value + 1;

  if mod(value, 6) = 0 then
    leave;

  put list (value);
end;

```

or shorter:

```pli

 dcl value fixed bin(31) init(0);
 do Until(value=6);
   value+=1;
   put Skip list(value);
 end;
```

{{out}}

```txt

             1
             2
             3
             4
             5
             6

```



## Pop11


```pop11
lvars val = 0;
while true do
   val + 1 -> val;
   printf(val, '%p\n');
   quitif(val rem 6 = 0);
endwhile;
```



## PowerShell


```powershell
$n = 0
do {
    $n++
    $n
} while ($n % 6 -ne 0)
```



## Prolog


```prolog

% initial condition
do(0):- write(0),nl,do(1).

% control condition
do(V):- 0 is mod(V,6), !, fail.

% loop
do(V) :-
    write(V),nl,
    Y is V + 1,
    do(Y).

wloop :-
   do(0).


```



## PureBasic

{{works with|PureBasic|4.41}}

```PureBasic
x=0
Repeat
  x+1
  Debug x
Until x%6=0
```



## Python

Python doesn't have a do-while loop.

```python
val = 0
while True:
   val +=1
   print val
   if val % 6 == 0: break
```

or repeat the body of the loop before a standard while.

```python
val = 1
print val
while val % 6 != 0:
   val += 1
   print val
```



## R


```R
i <- 0
repeat
{
   i <- i + 1
   print(i)
   if(i %% 6 == 0) break
}
```



## Racket


Idiomatic Racket code is functional:

```racket

#lang racket
(let loop ([n 0])
  (let ([n (add1 n)])
    (displayln n)
    (unless (zero? (modulo n 6)) (loop n))))

```


But an imperative version is easy to do too:

```racket

#lang racket
(define n 0)
(let loop ()
  (set! n (add1 n))
  (displayln n)
  (unless (zero? (modulo n 6)) (loop)))

```



## REBOL


```REBOL
REBOL [
	Title: "Loop/While"
	URL: http://rosettacode.org/wiki/Loop/Do_While
]

; REBOL doesn't have a specific 'do/while' construct, but 'until' can
; be used to provide the same effect.

value: 0
until [
	value: value + 1
	print value

	0 = mod value 6
]
```


{{out}}

```txt
1
2
3
4
5
6
```



## Red


```Red
Red []
i: 0
until [
  ?? i
  i: i + 1
  i % 6 = 0 ;; loop , until this is true...
]

```
{{out}}

```txt
i: 0
i: 1
i: 2
i: 3
i: 4
i: 5

```


## REXX

In the '''DO UNTIL''' construct, the expression is evaluated at the end of the '''DO''' loop,

even though it is written at the beginning.

This insures that the '''DO UNTIL''' loop will execute at least once (as coded below).



In contrast, a '''DO WHILE''' construct, the expression would be evaluated at the beginning of the '''DO''' loop, and

may cause the '''DO WHILE''' loop to not execute at all.

This necessitates the use of '''DO UNTIL''' instead of '''DO WHILE'''.

### version 1


```rexx
/*REXX program demonstrates a     DO  UNTIL     construction.           */
v=0
          do  until  v//6==0           /*REXX   //   is the ÷ remainder.*/
          v=v+1
          say v
          end
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

1
2
3
4
5
6

```



### version 2


```rexx
/*REXX program demonstrates a     DO  UNTIL     construction.           */

          do v=1  until  v//6==0       /*REXX   //   is the ÷ remainder.*/
          say v
          end
                                       /*stick a fork in it, we're done.*/
```

'''output''' is the same as the 1<sup>st</sup> version.



## Ring


```ring

   n = 0
   While True
      n++  See n + nl
      if n % 6 = 0  exit ok
   end

```



## Ruby

The <tt>while</tt> statement modifier normally checks the condition before entering the loop. But if the <tt>while</tt> statement modifier is on a <tt>begin ... end</tt> statement, then it loops at least once. Same with the <tt>until</tt> statement modifier.

{| class="wikitable"
! while
! until
|-
|
```ruby
val = 0
begin
   val += 1
   puts val
end while val % 6 != 0
```

|
```ruby
val = 0
begin
   val += 1
   puts val
end until val % 6 == 0
```

|}

During November 2005, Yukihiro Matsumoto, the creator of Ruby, [http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-core/6741 regretted this loop feature] and [http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-core/6745 suggested using Kernel#loop].

{| class="wikitable"
! break unless
! break if
|-
|
```ruby
val = 0
loop do
   val += 1
   puts val
   break unless val %6 != 0
end
```

|
```ruby
val = 0
loop do
   val += 1
   puts val
   break if val %6 == 0
end
```

|}

All four of these examples print the numbers 1, 2, 3, 4, 5, 6.


## Rust


Rust does not have a <tt>do...while</tt> loop. Instead, the keyword <tt>loop</tt> is used with a termination condition.


```rust
let mut x = 0;

loop {
    x += 1;
    println!("{}", x);

    if x % 6 == 0 { break; }
}
```



## Salmon


```Salmon
variable x := 0;
do
  {
    ++x;
    x!
  }
while (x % 6 != 0);
```



## SAS


```sas
/* using DO UNTIL so that the loop executes at least once */
data _null_;
n=0;
do until(mod(n,6)=0);
    n+1;
    put n;
end;
run;
```



## Sather

{{trans|C}}

```sather
class MAIN is
  main is
    val ::= 0;
    loop
      val := val + 1;
      #OUT + val + "\n";
      while!(val % 6 /= 0)
    end;
  end;
end;
```



## Scala

{{libheader|Scala}}


### Imperative


```scala
  {
    var (x, l) = (0, List[Int]())
    do {
      x += 1
      l :+= x // A new copy of this list with List(x) appended.
    } while (x % 6 != 0)
    l
  }.foreach(println(_))

```



### Tail recursive


```scala
	def loop(iter: Int, cond: (Int) => Boolean, accu: List[Int]): List[Int] = {
	  val succ = iter + 1
	  val temp = accu :+ succ
	  if (cond(succ)) loop(succ, cond, temp) else temp
	}
	println(loop(0, (_ % 6 != 0), Nil))
```



### Stream


```scala
  def loop(i: Int, cond: (Int) => Boolean): Stream[Int] = {
    val succ = i + 1;
    succ #:: (if (cond(succ)) loop(succ, cond) else Stream.empty)
  }
  loop(0, (_ % 6 != 0)).foreach(println(_))
```



## Scheme


```scheme
(let loop ((i 1))
  (display i)
  (if (positive? (modulo i 6))
      (loop (+ i 1))))
```



## Scilab

{{works with|Scilab|5.5.1}}
<lang>v=0
while %T
    v=v+1
    printf("%2d ",v)
    if modulo(v,6)==0 then break; end
end
printf("\n")
```

{{out}}

```txt
 1  2  3  4  5  6
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: number is 0;
  begin
    repeat
      incr(number);
      writeln(number)
    until number rem 6 = 0
  end func;
```



## Sidef


```ruby
var value = 0;
do {
    say ++value;
} while (value % 6);
```



## Slate


```slate
[| val |
  val: 0.
  [val: val + 1.
   print: val.
   val \\ 6 ~= 0] whileTrue
] do.
```



## Smalltalk


{{works with|Smalltalk/X}}
{{works with|VisualWorks Smalltalk}}

```smalltalk
|val|
val := 0.
[
  val := val + 1.
  val displayNl.
] doWhile: [ (val rem: 6) ~= 0 ]
```



```smalltalk
|val|
val := 0.
[
  val := val + 1.
  val displayNl.
] doUntil: [ (val rem: 6) == 0 ]
```


{{works with|GNU Smalltalk}}
To simulate the do-while construct, we can use the
<tt>whileTrue:</tt> method of a block with a void while block.

```smalltalk
|val|
val := 0.
[
  val := val + 1.
  val displayNl.
  (val rem: 6) ~= 0
] whileTrue: [ ]
```



## Sparkling


```sparkling
var i = 0;
do {
    print(++i);
} while (i % 6 != 0);
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

  n := 0
  repeat
    n += 1
    ser.dec(n)
    ser.tx(32)
  while n // 6

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

{{out}}

```txt
1 2 3 4 5 6
```



## SPL


```spl
n = 0
>
  n += 1
  #.output(n)
< n%6
```

{{out}}

```txt

1
2
3
4
5
6

```



## Stata

Stata macro language has no do/while loop, but it's possible to achieve this with a '''[https://www.stata.com/help.cgi?while while]''' loop.

Use a flag to force the first loop. It's changed in the loop so that it will have no effect after the first loop.


```stata
local n 0
local q 1
while `q' | mod(`n',6) {
	local q 0
	di `++n'
}
```


Use an infinite while loop and do the test with an ''[https://www.stata.com/help.cgi?if if]''' at the end of the loop.


```stata
local n 0
while 1 {
	di `++n'
	if mod(`n',6)==0 continue, break
}
```



###  Mata

Mata has a '''[https://www.stata.com/help.cgi?m2_do do/while]''' loop:

<lang>mata
n=0
do {
	printf("%f\n",++n)
} while (mod(n,6))
end
```



## Suneido


```Suneido
val = 0
do
    {
    Print(++val)
    } while (val % 6 isnt 0)
```


{{out}}

```txt
1
2
3
4
5
6
```



## Swift

{{works with|Swift|3.x+}}

```swift
var val = 0
repeat {
  val += 1
  print(val)
} while val % 6 != 0
```

{{works with|Swift|2.x}}

```swift
var val = 0
repeat {
  val++
  print(val)
} while val % 6 != 0
```

{{works with|Swift|1.x}}

```swift
var val = 0
do {
   val++
   println(val)
} while val % 6 != 0
```



## Tcl

Tcl does not have a built-in <code>do...while</code> construct.  This example demonstrates the ease  of creating new looping contructs in plain Tcl.  <code>do</code> procedure taken from [http://wiki.tcl.tk/3603 Tcler's wiki]

```tcl
proc do {body keyword expression} {
    if {$keyword eq "while"} {
       set expression "!($expression)"
    } elseif {$keyword ne "until"} {
       return -code error "unknown keyword \"$keyword\": must be until or while"
    }
    set condition [list expr $expression]
    while 1 {
       uplevel 1 $body
       if {[uplevel 1 $condition]} {
          break
       }
    }
    return
}

set i 0
do {puts [incr i]} while {$i % 6 != 0}
```

{{tcllib|control}}

```tcl
package require control
set i 0; control::do {puts [incr i]} while {$i % 6 != 0}
set i 0; control::do {puts [incr i]} until {$i % 6 == 0}
```


Mind you, it is also normal to write this task using a normal <code>while</code> as:

```tcl
set i 0
while true {
    puts [incr i]
    if {$i % 6 == 0} break
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
var=0
LOOP
var=var+1, rest=var%6
PRINT var
IF (rest==0) EXIT
ENDLOOP

```

{{out}}

```txt

1
2
3
4
5
6

```



## UNIX Shell

{{works with|bash}}
{{works with|pdksh}}
{{works with|zsh}}

```bash
val=0
while true; do
  echo $((++val))
  [ $((val%6)) -eq 0 ] && break
done
```


{{works with|Bourne Shell}}

```bash
val=0
while true; do
  val=`expr $val + 1`
  echo $val
  expr $val % 6 = 0 >/dev/null && break
done
```


{{works with|zsh}}

```bash
for ((val=1;;val++)) {
  print $val
  (( val % 6 )) || break
}
```



## VBA


```VB
Public Sub LoopDoWhile()
    Dim value As Integer
    value = 0
    Do
        value = value + 1
        Debug.Print value;
    Loop While value Mod 6 <> 0
End Sub
```
{{out}}
```txt
 1  2  3  4  5  6
```


## Vedit macro language


```vedit
#1 = 0
do {
    #1++
    Num_Type(#1)
} while (#1 % 6 != 0);
```



## Verbexx


```verbexx
//  Basic @LOOP until: verb

@LOOP init:{@VAR n = 0} until:(n % 6 == 0)
{
     n++;
     @SAY n;
};
```



## Visual Basic .NET


```vbnet
Dim i = 0
Do
    i += 1
    Console.WriteLine(i)
Loop Until i Mod 6 = 0
```



## XPL0


```XPL0
code CrLf=9, IntOut=11;
int V;
[V:= 0;
repeat  V:= V+1;
        IntOut(0, V);  CrLf(0);
until   rem(V/6) = 0;
]
```



## Yorick


```yorick
val = 0;
do {
    val++;
    write, val;
} while(val % 6 != 0);
```



## zkl

{{trans|Yorick}}

```zkl
val := 0;
do {
    val+=1;
    val.print(" ");
} while(val % 6 != 0);
```

{{out}}

```txt

1 2 3 4 5 6

```


{{omit from|GUISS}}
{{omit from|Commodore BASIC}}
