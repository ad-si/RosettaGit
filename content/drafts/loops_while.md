+++
title = "Loops/While"
description = ""
date = 2019-10-08T08:25:02Z
aliases = []
[extra]
id = 2821
[taxonomies]
categories = []
tags = []
+++

{{task|Iteration}}
[[Category:Conditional loops]]
[[Category:Simple]]
{{omit from|GUISS|No loops and we cannot read values}}

;Task:
Start an integer value at   '''1024'''.

Loop while it is greater than zero.

Print the value (with a newline) and divide it by two each time through the loop.


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





## 0815


```0815
<:400:~}:_:%<:a:~$=<:2:=/^:_:
```



## 360 Assembly

;Basic
Using binary arithmetic. Convert results to EBCDIC printable output.

```360asm
*        While                     27/06/2016
WHILELOO CSECT                     program's control section
         USING WHILELOO,12         set base register
         LR    12,15               load base register
         LA    6,1024              v=1024
LOOP     LTR   6,6                 while v>0
         BNP   ENDLOOP             .
         CVD   6,PACKED              convert v to packed decimal
         OI    PACKED+7,X'0F'        prepare unpack
         UNPK  WTOTXT,PACKED         packed decimal to zoned printable
         WTO   MF=(E,WTOMSG)         display v
         SRA   6,1                   v=v/2   by right shift
         B     LOOP                end while
ENDLOOP  BR    14                  return to caller
PACKED   DS    PL8                 packed decimal
WTOMSG   DS    0F                  full word alignment for wto
WTOLEN   DC    AL2(8),H'0'         length of wto buffer (4+1)
WTOTXT   DC    CL4' '              wto text
         END   WHILELOO
```

{{out}} (+ sign indicates "problem state" (non system key) issued WTO's
<pre style="height:16ex">
+1024
+0512
+0256
+0128
+0064
+0032
+0016
+0008
+0004
+0002
+0001

```

;Structured Macros

```360asm
*        While                     27/06/2016
WHILELOO CSECT
         USING WHILELOO,12         set base register
         LR    12,15               load base register
         LA    6,1024              v=1024
         DO WHILE=(LTR,6,P,6)      do while v>0
         CVD   6,PACKED              convert v to packed decimal
         OI    PACKED+7,X'0F'        prepare unpack
         UNPK  WTOTXT,PACKED         packed decimal to zoned printable
         WTO   MF=(E,WTOMSG)         display
         SRA   6,1                   v=v/2   by right shift
         ENDDO ,                   end while
         BR    14                  return to caller
PACKED   DS    PL8                 packed decimal
WTOMSG   DS    0F                  full word alignment for wto
WTOLEN   DC    AL2(8),H'0'         length of wto buffer (4+1)
WTOTXT   DC    CL4' '              wto text
         END   WHILELOO
```

{{out}}
Same as above


## 6502 Assembly

Code is called as a subroutine (i.e. JSR LoopsWhile).  Specific OS/hardware routines for printing are left unimplemented.

```6502asm
LoopsWhile:	PHA			;push accumulator onto stack

		LDA #$00		;the 6502 is an 8-bit processor
		STA Ilow		;and so 1024 ($0400) must be stored in two memory locations
		LDA #$04
		STA Ihigh
WhileLoop:	LDA Ilow
		BNE NotZero
		LDA Ihigh
		BEQ EndLoop
NotZero:	JSR PrintI		;routine not implemented
		LSR Ihigh		;shift right
		ROR Ilow		;rotate right
		JMP WhileLoop

EndLoop:	PLA			;restore accumulator from stack
		RTS			;return from subroutine
```



## ActionScript


```actionscript
var i:int = 1024;
while (i > 0) {
    trace(i);
    i /= 2;
}
```



## Ada


```ada
declare
   I : Integer := 1024;
begin
   while I > 0 loop
      Put_Line(Integer'Image(I));
      I := I / 2;
   end loop;
end;
```



## Agena

Tested with Agena 2.9.5 Win32

```agena
scope
    local i := 1024;
    while i > 0 do
        print( i );
        i := i \ 2
    od
epocs
```



## Aime


```aime
integer i;

i = 1024;
while (i) {
    o_plan(i, "\n");
    i /= 2;
}
```



## ALGOL 60


```algol60
INTEGER I;
I:=1024;
WHILE I>0 DO
BEGIN
   OUTINT(I);
   I:=I DIV 2
END
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
INT i := 1024;
WHILE i > 0 DO
   print(i);
   i := i OVER 2
OD
```

{{Out}}

```txt

      +1024       +512       +256       +128        +64        +32        +16         +8         +4         +2         +1

```


=={{header|ALGOL-M}}==

```algol
begin
    integer i;
    i := 1024;
    while i > 0 do begin
        write( i );
        i := i / 2;
    end;
end
```



## ALGOL W


```algolw
begin
    integer i;
    i := 1024;
    while i > 0 do
    begin
        write( i );
        i := i div 2
    end
end.
```



## AmbientTalk

Note: in AmbientTalk, while:do: is a keyworded message (as in Smalltalk).
Both arguments to this message must be blocks (aka anonymous functions or thunks).


```ambienttalk
// print 1024 512 etc
def i := 1024;
while: { i > 0 } do: {
  system.print(" "+i);
  i := i/2;
}
```



## AmigaE


```amigae
PROC main()
  DEF i = 1024
  WHILE i > 0
    WriteF('\d\n', i)
    i := i / 2
  ENDWHILE
ENDPROC
```



## AppleScript

AppleScript does not natively support a standard out.
Use the Script Editor's Event Log as the output.

```AppleScript
set i to 1024
repeat while i > 0
	log i
	set i to i / 2
end repeat
```



## Applesoft BASIC


```Applesoft BASIC
 10 I% = 1024
 20  IF I% > 0 THEN  PRINT I%:I% = I% / 2: GOTO 20
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program loopwhile.s   */

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
    mov r4,#1024                            @ loop counter
1:                                          @ begin loop
    mov r0,r4
    ldr r1,iAdrsMessValeur                  @ display value
    bl conversion10                         @ decimal conversion
    ldr r0,iAdrszMessResult
    bl affichageMess                        @ display message
    ldr r0,iAdrszCarriageReturn
    bl affichageMess                        @ display return line
    lsr r4,#1                               @ division by 2
    cmp r4,#0                               @ end ?
    bgt 1b                                  @ no ->begin loop one


100:                                        @ standard end of the program
    mov r0, #0                              @ return code
    mov r7, #EXIT                           @ request to exit program
    svc #0                                  @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszMessResult:         .int szMessResult
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
    cmp r0,#0                               @ stop if quotient = 0
    subne r2,#1                               @ previous position
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


```arturo
i 1024

loop i>0 {
	print i
	i i/2
}
```


{{out}}


```txt
1024
512
256
128
64
32
16
8
4
2
1
```



## AutoHotkey


```AutoHotkey
i = 1024
While (i > 0)
{
  output = %output%`n%i%
  i := Floor(i / 2)
}
MsgBox % output
```



## AWK


```awk
BEGIN {
  v = 1024
  while(v > 0) {
    print v
    v = int(v/2)
  }
}
```



## Axe


```axe
1024→A
While A>0
 Disp A▶Dec,i
 A/2→A
End
```



## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
i = 1024
while i > 0
   print i
   i = i / 2
wend
```


=
## BaCon
=

```freebasic

i = 1024
WHILE i > 0
   PRINT i
   i = i / 2
WEND
```


=
## Commodore BASIC
=
There is no WHILE construct in Commodore BASIC. A GOTO construct is used instead. Also, an integer variable name has a % sign as its suffix.

```gwbasic
10 N% = 1024
20 IF N% = 0 THEN 60
30 PRINT N%
40 N% = N%/2
50 GOTO 20
60 END
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      i% = 1024
      WHILE i%
        PRINT i%
        i% DIV= 2
      ENDWHILE
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET I=1024
110 DO WHILE I>0
120   PRINT I
130   LET I=IP(I/2)
140 LOOP
```



## bc


```bc
i = 1024
while (i > 0) {
    i
    i /= 2
}
```



## Befunge


```befunge
84*:*>       :v
     ^/2,*25.:_@
```



## blz


```blz
num = 1024
while num > 1 # blz will automatically cast num to a fraction when dividing 1/2, so this is necessary to stop an infinite loop
    print(num)
    num = num / 2
end
```



## Bracmat


```bracmat
1024:?n & whl'(!n:>0 & out$!n & div$(!n.2):?n)
```



## Brat

Converts to integers so output is a little bit shorter and neater.


```brat
i = 1024
while { i > 0 } {
    p i
    i = (i / 2).to_i
}
```



## C


```c
int i = 1024;
while(i > 0) {
  printf("%d\n", i);
  i /= 2;
}
```

In for loop fashion:

```c
int i;
for(i = 1024;i > 0; i/=2){
   printf("%d\n", i);
}
```



## ChucK

<lang>
1024 => int value;

while(value > 0)
{
    <<<value>>>;
    value / 2 => value;
}

```



## C++


```cpp
int i = 1024;
while(i > 0){
  std::cout << i << std::endl;
  i /= 2;
}
```

Alternatively, it can be done with <code>for</code>:

```cpp
for(int i = 1024; i > 0; i /= 2)
  std::cout << i << std::endl;
```


Instead of <code>i /= 2</code> one can also use the bit shift operator <code>i >>= 1</code> on integer variables.

Indeed, in C++,

```cpp
for(init; cond; update){
  statement;
}
```

is equivalent to

```cpp
{
  init;
  while(cond){
    statement;
    update;
  }
}
```


## C#

```c#
int i = 1024;
while(i > 0){
   System.Console.WriteLine(i);
   i /= 2;
}
```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>WHILELOOP
    set x = 1024
    while (x > 0) {
        write x,!
        set x = (x \ 2)    ; using non-integer division will never get to 0
    }

    quit
```


{{out}}
```txt
SAMPLES>DO ^WHILELOOP
1024
512
256
128
64
32
16
8
4
2
1


```



## Chapel


```chapel
var val = 1024;
while val > 0 {
        writeln(val);
        val /= 2;
}
```



## Clojure


```lisp
(def i (ref 1024))

(while (> @i 0)
  (println @i)
  (dosync (ref-set i (quot @i 2))))
```


2 ways without mutability:


```Clojure
(loop [i 1024]
  (when (pos? i)
    (println i)
    (recur (quot i 2))))


(doseq [i (take-while pos? (iterate #(quot % 2) 1024))]
  (println i))
```



## COBOL

COBOL does not have a while loop construct, but it is does have a <code>PERFORM UNTIL</code> structure, which means that the normal condition used in a while loop must be negated.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Loop-While.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I PIC 9999 VALUE 1024.

       PROCEDURE DIVISION.
           PERFORM UNTIL NOT 0 < I
               DISPLAY I
               DIVIDE 2 INTO I
           END-PERFORM

           GOBACK
           .
```



## ColdFusion

Remove the leading space from the line break tag.

With tags:

```cfm><cfset i = 1024 /
<cfloop condition="i GT 0">  #i#< br />
  <cfset i /= 2 />
</cfloop>
```

With script:

```cfm><cfscript
  i = 1024;
  while( i > 0 )
  {
    writeOutput( i + "< br/ >" );
  }
</cfscript>
```



## Common Lisp


```lisp
(let ((i 1024))
  (loop while (plusp i) do
        (print i)
        (setf i (floor i 2))))

(loop with i = 1024
      while (plusp i) do
      (print i)
      (setf i (floor i 2)))

(defparameter *i* 1024)
(loop while (plusp *i*) do
      (print *i*)
      (setf *i* (floor *i* 2)))

```



## Crack


```crack
i = 1024;
while( i > 0 ) {
  cout ` $i\n`;
  i = i/2;
}
```


## Creative Basic


```Creative  Basic
DEF X:INT

X=1024

OPENCONSOLE

WHILE X>0

   PRINT X
   X=X/2

ENDWHILE
'Output starts with 1024 and ends with 1.

'Putting the following in the loop will produce output starting with 512 and ending with 0:
'X=X/2
'PRINT X

PRINT:PRINT"Press any key to end."

'Keep console from closing right away so the figures can be read.
WHILE INKEY$="":ENDWHILE

CLOSECONSOLE

'Since this is, in fact, a Creative Basic console program.
END
```

Note: Spacing is not an issue. I just find the code to be more readable with spaces.


## D


```d
import std.stdio;

void main() {
    int i = 1024;

    while (i > 0) {
        writeln(i);
        i >>= 1;
    }
}
```

{{out}}

```txt
1024
512
256
128
64
32
16
8
4
2
1
```



## Dc


```Dc
[ q ] sQ [ d 0!<Q p 2 / lW x ] sW 1024 lW x
```



## Dao


```dao
i = 1024;
while( i > 0 ) i = i / 2;
```



## DCL


DCL is quite primitive in terms of "control statements", no WHILE, REPEAT, UNLESS or FOR,
so must make do with IF/THEN/ELSE and GOTO statements.


```DCL
$ i = 1024
$Loop:
$ IF ( i .LE. 0 ) THEN GOTO LoopEnd
$ WRITE sys$output F$FAO( "  i = !4UL", i )  ! formatted ASCII output, fixed-width field
$ ! Output alternatives:
$ !   WRITE sys$output F$STRING( i )         ! explicit integer-to-string conversion
$ !   WRITE sys$output i                     ! implicit conversion to string/output
$ i = i / 2
$ GOTO Loop
$LoopEnd:
```



## Delphi



```Delphi
var
  i : Integer;
begin
  i := 1024;

  while i > 0 do
  begin
    Writeln(i);
    i := i div 2;
  end;
end;
```



## Dragon


```dragon
i = 1024
while(i > 0){
   showln i
   i >>= 1 //also acceptable: i /= 2
}
```



## DUP



```dup
1024[$][$.10,2/\%]# {Short form}
```


Explanation:

```dup
1024                {push 1024 on stack}
    [ ][         ]# {while[condition>0][do]}
     $              {DUP}
        $.          {DUP, print top of stack to STDOUT}
          10,       {print newline}
             2/\%   {2 DIV/MOD SWAP POP}
```


Alternative, if the interpreter allows using the shift operator:


```dup
1024[$][$.10,1»]#
```


Output:


```dup
1024
512
256
128
64
32
16
8
4
2
1
```



## DWScript



```Delphi
var i := 1024;

while i > 0 do begin
   PrintLn(i);
   i := i div 2;
end;
```



## Dyalect


{{trans|Swift}}


```Dyalect
var i = 1024
while i > 0 {
  print(i)
  i /= 2
}
```



## E



```e
var i := 1024
while (i > 0) {
    println(i)
    i //= 2
}
```



## EasyLang


<lang>i = 1024
while i > 0
  print i
  i = i / 2
.
```



## EchoLisp


```lisp

(set! n 1024)
(while (> n 0) (write n) (set! n (quotient n 2)))
1024 512 256 128 64 32 16 8 4 2 1

```



## EGL



```EGL
x int = 1024;
while ( x > 0 )
   SysLib.writeStdout( x );
   x = MathLib.floor( x / 2 );
end
```



## Elena

ELENA 4.x:

```elena
public program()
{
    int i := 1024;
    while (i > 0)
    {
        console.writeLine:i;

        i /= 2
    }
}
```



## Elixir


```elixir
defmodule Loops do
  def while(0), do: :ok
  def while(n) do
    IO.puts n
    while( div(n,2) )
  end
end

Loops.while(1024)
```



## Emacs Lisp


```Lisp
(let ((i 1024))
  (while (> i 0)
    (message "%d" i)
    (setq i (/ i 2))))
```



## Erlang


```erlang
-module(while).
-export([loop/0]).

loop() ->
	loop(1024).

loop(N) when N div 2 =:= 0 ->
	io:format("~w~n", [N]);

loop(N) when N >0 ->
	io:format("~w~n", [N]),
	loop(N div 2).
```




## ERRE


```ERRE

   I%=1024
   WHILE I%>0 DO  ! you can leave out >0
     PRINT(I%)
     I%=I% DIV 2  ! I%=INT(I%/2) for C-64 version
   END WHILE

```




## Euphoria


```Euphoria
integer i
i = 1024

while i > 0 do
    printf(1, "%g\n", {i})
    i = floor(i/2) --Euphoria does NOT use integer division.  1/2 = 0.5
end while
```

Even without the <code>floor()</code> the code will in fact end.  But it's FAR beyond 1.

=={{header|F_Sharp|F#}}==

```fsharp>let rec loop n = if n
 0 then printf "%d " n; loop (n / 2)
loop 1024
```



## Factor


```factor>1024 [ dup 0
 ] [ dup . 2 /i ] while drop
```



## FALSE


```false
1024[$0>][$."
"2/]#%
```



## Fantom



```fantom
class Main
{
  public static Void main ()
  {
    Int i := 1024
    while (i > 0)
    {
      echo (i)
      i /= 2
    }
  }
}
```



## Forth


```forth
: halving ( n -- )
  begin  dup 0 >
  while  cr dup .  2/
  repeat drop ;
1024 halving
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
INTEGER :: i = 1024
DO WHILE (i > 0)
  WRITE(*,*) i
  i = i / 2
END DO
```


{{works with|Fortran|77 and later}}

```fortran
      PROGRAM LOOPWHILE
        INTEGER I

C       FORTRAN 77 does not have a while loop, so we use GOTO statements
C       with conditions instead. This is one of two easy ways to do it.
        I = 1024
   10   CONTINUE
C       Check condition.
        IF (I .GT. 0) THEN
C         Handle I.
          WRITE (*,*) I
          I = I / 2
C         Jump back to before the IF block.
          GOTO 10
        ENDIF
        STOP
      END
```


{{works with|Fortran|IV and 66 and later}}

```fortran
      PROGRAM LOOPWHILE
      INTEGER I
C     FORTRAN 66 does not have IF block.
      I = 1024
 10   CONTINUE
      IF (I .LE. 0) GOTO 20
      WRITE (*,*) I
      I = I / 2
      GOTO 10
 20   CONTINUE
      STOP
      END
```



## Fortress


```fortress

component loops_while
  export Executable

  var i:ZZ32 = 1024
  run() = while i > 0 do
    println(i)
    i := i DIV 2
  end
end

```


{{out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim i As Integer = 1024

While i > 0
  Print i
  i Shr= 1
Wend

Sleep
```


{{out}}

```txt

 1024
 512
 256
 128
 64
 32
 16
 8
 4
 2
 1

```



## Frink


```frink
i=1024
while i>0
{
   i = i/1
}
```




## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long i : i = 1024

while i > 0
print i
i = int( i / 2 )
wend

```

Output:

```txt

 1024
 512
 256
 128
 64
 32
 16
 8
 4
 2
 1

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=4e992013e4e7dc69a82477299a5ce23a Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short = 1024

While siCount > 0
  Print siCount;;
  siCount /= 2
Wend

End
```

Output:

```txt

1024 512 256 128 64 32 16 8 4 2 1

```



## GAP


```gap
n := 1024;
while n > 0 do
    Print(n, "\n");
    n := QuoInt(n, 2);
od;
```



## GML


```GML
i = 1024
while(i > 0)
    {
    show_message(string(i))
    i /= 2
    }
```



## Go


```go
i := 1024
for i > 0 {
  fmt.Printf("%d\n", i)
  i /= 2
}
```



## Groovy

Solution:

```groovy
int i = 1024
while (i > 0) {
    println i
    i /= 2
}
```


{{Out}}

```txt
1024
512
256
128
64
32
16
8
4
2
1
```



## Haskell


```haskell
import Control.Monad (when)

main = loop 1024
  where loop n = when (n > 0)
                      (do print n
                          loop (n `div` 2))
```


You can use whileM_ function from monad-loops package that operates on monads:


```haskell
import Data.IORef
import Control.Monad.Loops

main :: IO ()
main = do r <- newIORef 1024
          whileM_ (do n <- readIORef r
                     return (n > 0))
                  (do n <- readIORef r
                     print n
                     modifyIORef r (`div` 2))
```


With MonadComprehensions extension you can write it a little bit more readable:

```haskell
{-# LANGUAGE MonadComprehensions #-}
import Data.IORef
import Control.Monad.Loops

main :: IO ()
main = do
   r <- newIORef 1024
   whileM_ [n > 0 | n <- readIORef r] $ do
        n <- readIORef r
        print n
        modifyIORef r (`div` 2)
```



## hexiscript


```hexiscript
let i 1024
while i > 0
  println i
  let i (i / 2)
endwhile
```



## HolyC


```holyc
U16 i = 1024;
while (i > 0) {
  Print("%d\n", i);
  i /= 2;
}
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
   local i
   i := 1024
   while write(0 < (i := i / 2))
end
```



## Inform 7


```inform7
let N be 1024;
while N > 0:
	say "[N][line break]";
	let N be N / 2;
```


## IWBASIC


```IWBASIC

DEF X:INT

X=1024

OPENCONSOLE

WHILE X>0

    PRINT X
    X=X/2

ENDWHILE
'Output starts with 1024 and ends with 1.

'Putting the following in the loop will produce output starting with 512 and ending with 0:
'X=X/2
'PRINT X

'When compiled as a console only program, a press any key to continue message is automatic.
'I presume code is added by the compiler.
CLOSECONSOLE

'Since this is, in fact, an IWBASIC console program, which compiles and runs.
END
```

Note: Spacing is not an issue. I just find the code to be more readable with spaces.


## J

J is array-oriented, so there is very little need for loops.  For example, one could satisfy this task this way:


```j
,. <.@-:^:*^:a: 1024
```


J does support loops for those times they can't be avoided (just like many languages support gotos for those time they can't be avoided).


```j
monad define 1024
  while. 0 < y do.
    smoutput y
    y =. <. -: y
  end.
  i.0 0
)
```


Note: this defines an anonymous function (monad define, and the subsequent lines) and passes it the argument 1024, which means it will be executed as soon as the full definition is available.


## Java


```java5
int i = 1024;
while(i > 0){
   System.out.println(i);
   i >>= 1; //also acceptable: i /= 2;
}
```

With a for loop:

```java5
for(int i = 1024; i > 0;i /= 2 /*or i>>= 1*/){
   System.out.println(i);
}
```



## JavaScript


```javascript
var n = 1024;
while (n > 0) {
  print(n);
  n /= 2;
}
```


In a functional idiom of JavaScript, however, we can not use a While '''statement''' to achieve this task, as statements return no value, mutate state, and can not be composed within other functional expressions.

Instead, we can define a composable loopWhile() '''function''' which has no side effects, and takes 3 arguments:
:#An initial value
:#A function which returns some derived value, corresponding to the body of the While loop
:#A conditional function, corresponding to the While test


```JavaScript
function loopWhile(varValue, fnDelta, fnTest) {
  'use strict';
  var d = fnDelta(varValue);

  return fnTest(d) ? [d].concat(
    loopWhile(d, fnDelta, fnTest)
  ) : [];
}

console.log(
  loopWhile(
    1024,
    function (x) {
      return Math.floor(x/2);
    },
    function (x) {
      return x > 0;
    }
  ).join('\n')
);
```


If we assume integer division here (Math.floor(x/2)) rather than the floating point division (x/2) used in the imperative example, we obtain the output:


```JavaScript
512
256
128
64
32
16
8
4
2
1
```



## Joy


```joy
DEFINE putln == put '\n putch.

1024 [] [dup putln 2 /] while.
```



## jq

'''Using recurse/1'''
```jq
# To avoid printing 0, test if the input is greater than 1
1024 | recurse( if . > 1 then ./2 | floor else empty end)
```

'''Using recurse/2''' (requires jq >1.4)

```jq
1024 | recurse( ./2 | floor; . > 0)
```

'''Using a filter'''

```jq>def task: if .
 0 then ., (./2 | floor | task) else empty end;
1024|task
```

'''Using while/2'''

If your jq does not include while/2 as a builtin, here is its definition:

```jq
def while(cond; update):
  def _while: if cond then ., (update | _while) else empty end;
  _while;
```

For example:

```jq
1024|while(. > 0; ./2|floor)
```



## Jsish


```javascript
#!/usr/bin/env jsish
/* Loops/While in Jsish */
var i = 1024;

while (i > 0) { puts(i); i = i / 2 | 0; }

/*
=!EXPECTSTART!=
1024
512
256
128
64
32
16
8
4
2
1
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u loopsWhile.jsi
[PASS] loopsWhile.jsi
```



## Julia


```Julia

n = 1024

while n > 0
    println(n)
    n >>= 1
end

```

{{out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```



## K

Implementation of the task using anonymous function is
given below

```K

{while[x>0; \echo x; x%:2]} 1024

```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    var value = 1024
    while (value > 0) {
        println(value)
        value /= 2
    }
}
```


{{out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```



## LabVIEW

Use Round Towards -Inf to prevent the integer becoming a float.<br/>{{VI snippet}}<br/>
[[File:LabVIEW_Loops_While.png]]


## Lang5

{{trans|Factor}}

```lang5
: /i  / int ; : 0=  0 == ;
: dip  swap '_ set execute _ ; : dupd  'dup dip ;
: 2dip  swap '_x set swap '_y set execute _y _x ;
: while
    do  dupd 'execute 2dip
        rot 0= if break else dup 2dip then
    loop ;

1024 "dup 0 >" "dup . 2 /i" while
```





## Lasso


```Lasso
local(i = 1024)
while(#i > 0) => {^
	#i + '\r'
	#i /= 2
^}
```



## Liberty BASIC

All integers are changed to floats if an operation creates a non-integer result.
Without using int() the program keeps going until erroring because accuracy was lost.

```lb
i = 1024
while i > 0
   print i
   i = int( i / 2)
wend
end
```



## LIL


```tcl
set num 1024; while {$num > 0} {print $num; set num [expr $num \ 2]}
```


Backslash is integer division, otherwise LIL would allow the division to go floating point.


## Lingo


```lingo
n = 1024
repeat while n>0
  put n
  n = n/2 -- integer division implicitely returns floor: 1/2 -> 0
end repeat
```



## Lisaac


```Lisaac
+ i : INTEGER;
i := 1024;
{ i > 0 }.while_do {
  i.println;

  i := i / 2;
};
```



## LiveCode


```LiveCode
put 1024 into n
repeat while n > 0
    put n & cr
    divide n by 2
end repeat
```



## Logo


```logo
make "n 1024
while [:n > 0] [print :n  make "n :n / 2]
```



## LOLCODE


LOLCODE's loop semantics require an afterthought if a condition is used, thus the <tt>nop</tt> in the following example. The more idiomatic approach would have been to <tt>GTFO</tt> of the loop once <tt>n</tt> had reached 0.


```LOLCODE
HAI 1.3

I HAS A n ITZ 1024

IM IN YR loop UPPIN YR nop WILE n
    VISIBLE n
    n R QUOSHUNT OF n AN 2
IM OUTTA YR loop

KTHXBYE
```



## Lua


```lua
n = 1024
while n>0 do
  print(n)
  n = math.floor(n/2)
end
```


## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Def long A=1024
      While A>0 {
            Print A
            A/=2
      }
}
Checkit

```

One line

```M2000 Interpreter

Module Online { A=1024&: While A>0 {Print A: A/=2}} : OnLine

```




## Maple

To avoid generating an infinite sequence (1/2, 1/4, 1/8, 1/16, etc.) of fractions after n takes the value 1, we use integer division (iquo) rather than the solidus operation (/).

```Maple>> n := 1024: while n
 0 do print(n); n := iquo(n,2) end:
                                  1024
                                  512
                                  256
                                  128
                                   64
                                   32
                                   16
                                   8
                                   4
                                   2
                                   1
```



## Mathematica

Mathematica does not support integer-rounding, it would result in getting fractions: 1/2, 1/4 , 1/8 and so on; the loop would take infinite time without using the Floor function:

```Mathematica
i = 1024;
While[i > 0,
 Print[i];
 i = Floor[i/2];
]
```


=={{header|MATLAB}} / {{header|Octave}}==
In Matlab (like Octave) the math is done floating point, then rounding to integer, so that 1/2 will be always 1 and never 0. A 'floor' is used to round the number.

```Matlab
i = 1024;
while (i > 0)
    disp(i);
    i = floor(i/2);
end
```


A vectorized version of the code is


```Matlab
  printf('%d\n', 2.^[log2(1024):-1:0]);
```



## Maxima


```maxima
block([n], n: 1024, while n > 0 do (print(n), n: quotient(n, 2)));

/* using a C-like loop: divide control variable by two instead of incrementing it */
for n: 1024 next quotient(n, 2) while n > 0 do print(n);
```



## MAXScript


```maxscript
a = 1024
while a > 0 do
(
    print a
    a /= 2
)
```



## Make


```make
NEXT=`expr $* / 2`
MAX=10

all: $(MAX)-n;

0-n:;

%-n: %-echo
       @-make -f while.mk $(NEXT)-n MAX=$(MAX)

%-echo:
       @echo $*
```


Invoking it

```make
|make -f while.mk MAX=1024
```



## Metafont


Metafont has no <tt>while</tt> loop, but it can be "simulated" easily.


```metafont
a := 1024;
forever: exitif not (a > 0);
  show a;
  a := a div 2;
endfor
```



## min

{{works with|min|0.19.3}}

```min
1024 :n (n 0 >) (n puts 2 div @n) while
```



## MiniScript


```MiniScript
i = 1024
while i > 0
    print i
    i = floor(i/2)
end while
```


{{out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```



## MIRC Scripting Language



```mirc
alias while_loop {
  var %n = 10
  while (%n >= 0) {
    echo -a Countdown: %n
    dec %n
  }
}
```



## Microsoft Small Basic


```microsoftsmallbasic

i = 1024
While i > 0
  TextWindow.WriteLine(i)
  i = Math.Floor(i / 2)
EndWhile

```


=={{header|MK-61/52}}==
<lang>1	0	2	4	П0	ИП0	/-/	x<0	15	ИП0
2	/	П0	БП	05	С/П
```



## MIXAL


```MIXAL

******************************************
* X = M / N WHILE X > 0
* STORE EACH X IN NUMERIC ARRAY
* PRINT ARRAY
*******************************************
M	EQU	1024
N	EQU	2
LPR	EQU	18
BUF0	EQU	100
MSG	EQU	2000
LENGTH	EQU	500
	ORIG	3000
START	IOC	0(LPR)
	ENTX	M
CALC	STX	BUF0,1
	DIV	=N=
	SRAX	5
	INC1	1
	JXP	CALC
	ST1	LENGTH
PRINT	LDA	BUF0,2
	CHAR
	STX	MSG
	OUT	MSG(LPR)
	INC2	1
	CMP2	LENGTH
	JNE	PRINT
	HLT
	END	START

```


=={{header|Modula-2}}==

```modula2
MODULE DivBy2;
  IMPORT InOut;

  VAR
    i: INTEGER;
BEGIN
  i := 1024;
  WHILE i > 0 DO
    InOut.WriteInt(i, 4);
    InOut.WriteLn;
    i := i DIV 2
  END
END DivBy2.
```


=={{header|Modula-3}}==
The usual module code and imports are omitted.

```modula3
PROCEDURE DivBy2() =
  VAR i: INTEGER := 1024;
  BEGIN
    WHILE i > 0 DO
      IO.PutInt(i);
      IO.Put("\n");
      i := i DIV 2;
    END;
  END DivBy2;
```



## Monte



```Monte

var i := 1024
while (i > 0):
    traceln(i)
    i //= 2

```



## MOO


```moo
i = 1024;
while (i > 0)
  player:tell(i);
  i /= 2;
endwhile
```



## Morfa


```morfa

import morfa.io.print;

var i = 1024;
while(i > 0)
{
    println(i);
    i /= 2;
}

```



## Nanoquery


```nanoquery
$n = 1024
while ($n > 0)
    println $n
    $n = $n/2
end while
```



## Nemerle


```Nemerle
mutable x = 1024;
while (x > 0)
{
    WriteLine($"$x");
    x /= 2;
}
```

Or, with immutable types, after Haskell:

```Nemerle
// within another function, eg Main()
def loop(n : int) : void
{
    when (n > 0)
    {
        WriteLine($"$n");
        loop(n / 2);
    }
}

loop(1024)
```



## Neko


```Neko

var i = 1024

while(i > 0) {
    $print(i + "\n");
    i = $idiv(i, 2)
}

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/While'

  x_ = 1024
  loop while x_ > 0
    say x_.right(6)
    x_ = x_ % 2 -- integer division
    end
```



## NewLISP


```NewLISP
(let (i 1024)
  (while (> i 0)
    (println i)
    (setq i (/ i 2))))
```



## Nim


```nim
var n: int = 1024
while n > 0:
  echo(n)
  n = n div 2
```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 I=1024
20 IF I=0 THEN END
30 PRINT I
40 I=I/2
50 GOTO 20
```



=={{header|Oberon-2}}==
The usual module code and imports are ommited.

```oberon2
PROCEDURE DivBy2*();
  VAR i: INTEGER;
BEGIN
  i := 1024;
  WHILE i > 0 DO
    Out.Int(i,0);
    Out.Ln;
    i := i DIV 2;
  END;
END DivBy2;
```



## Objeck


```objeck
i := 1024;
while(i > 0) {
   i->PrintLine();
   i /= 2;
};
```



## OCaml


```ocaml
let n = ref 1024;;
while !n > 0 do
  Printf.printf "%d\n" !n;
  n := !n / 2
done;;
```


But it is more common to write it in a tail-recursive functional style:

```ocaml
let rec loop n =
  if n > 0 then begin
    Printf.printf "%d\n" n;
    loop (n / 2)
  end
in loop 1024
```



## Octave


```octave
i = 1024;
while (i > 0)
  disp(i)
  i = floor(i/2);
endwhile
```


The usage of the type int32 is not convenient, since the math is done floating point, then rounding to integer, so that 1/2 will be always 1 and never 0.


## Oforth



```Oforth
1024 while ( dup ) [ dup println 2 / ]
```



## OOC


```ooc

main: func {
  value := 1024
  while (value > 0) {
    value toString() println()
    value /= 2
  }
}

```



## Oz

Oz' for-loop can be used in a C-like manner:

```oz>for I in 1024; I
0; I div 2 do
   {Show I}
end
```


Alternatively, we can use the <code>while</code> feature of the for-loop with a mutable variable:

```oz
declare
  I = {NewCell 1024}
in
  for while:@I > 0 do
     {Show @I}
     I := @I div 2
  end
```


## Panoramic


```Panoramic
dim x%:rem an integer

x%=1024

while x%>0

     print x%
     x%=x%/2

end_while

rem output starts with 1024 and ends with 1.

terminate
```



## PARI/GP


```parigp
n=1024;
while(n,
  print(n);
  n/=2
);
```



## Panda

Panda doesn't have explicit loops, instead we solve it by using the transitive closure operator. It applies a function to each successive value, each unique value is outputted. Our function halves, we make sure that the result is greater than 0 and add newline.

```panda
fun half(a) type integer->integer a.divide(2)
1024.trans(func:half).gt(0) nl

```



## Pascal


```pascal
program divby2(output);

var
  i: integer;

begin
  i := 1024;
  while i > 0 do
    begin
      writeln(i);
      i := i div 2
    end
end.
```



## PeopleCode


```PeopleCode

Local string &CRLF;
Local number &LoopNumber;
&LoopNumber = 1024;
&CRLF = Char(10) | Char(13);

While &LoopNumber > 0;
 WinMessage(&LoopNumber | &CRLF);
 &LoopNumber = &LoopNumber / 2;
End-While;

```



## Perl


```perl
my $n = 1024;
while($n){
    print "$n\n";
    $n = int $n / 2;
}
```


or written as a for-loop and using the bit-shift operator


```perl
for(my $n = 1024; $n > 0; $n >>= 1){
    print "$n\n";
}
```


<code>until (''condition'')</code> is equivalent to <code>while (not ''condition'')</code>.


```perl
my $n = 1024;
until($n == 0){
    print "$n\n";
    $n = int $n / 2;
}
```



## Perl 6


Here is a straightforward translation of the task description:

```perl6
my $n = 1024; while $n > 0 { say $n; $n div= 2 }
```


The same thing with a C-style loop and a bitwise shift operator:

```perl6
loop (my $n = 1024; $n > 0; $n +>= 1) { say $n }
```


And here's how you'd <em>really</em> write it, using a sequence operator that intuits the division for you:


```perl6
.say for 1024, 512, 256 ... 1
```



## Phix


```Phix
integer i = 1024
while i!=0 do
    ?i
    i = floor(i/2)  -- (see note)
end while
```

note: using i=i/2 would iterate over 1000 times until i is 4.94e-324 before the final division made it 0, if it didn't typecheck when it got set to 0.5


## PHL



```phl
var i = 1024;
while (i > 0) {
	printf("%i\n", i);
	i = i/2;
}
```



## PHP


```php
$i = 1024;
while ($i > 0) {
   echo "$i\n";
   $i >>= 1;
}
```



## PicoLisp


```PicoLisp
(let N 1024
   (while (gt0 N)
      (println N)
      (setq N (/ N 2)) ) )
```



## Pike


```pike
int main(){
   int i = 1024;
   while(i > 0){
      write(i + "\n");
      i = i / 2;
   }
}
```



## PL/I


```PL/I
declare i fixed binary initial (1024);

do while (i>0);
   put skip list (i);
   i = i / 2;
end;
```



## PL/SQL

{{works with|Oracle}}

```plsql

set serveroutput on
declare
  n number := 1024;
begin
  while n > 0 loop
    dbms_output.put_line(n);
    n := trunc(n / 2);
  end loop;
end;
/

```



## Pop11


```pop11
lvars i = 1024;
while i > 0 do
    printf(i, '%p\n');
    i div 2 -> i;
endwhile;
```



## PostScript

PostScript has no real <code>while</code> loop,
but it can easily be created with an endless loop and a check at the beginning:

```postscript
1024
{
    dup 0 le     % check whether still greater than 0
    { pop exit } % if not, exit the loop
    if
    dup =        % print the number
    2 idiv       % divide by two
}
loop
```



## PowerShell


```powershell
[int]$i = 1024
while ($i -gt 0) {
    $i
    $i /= 2
}
```



## Prolog


```prolog
while(0) :- !.
while(X) :-
    writeln(X),
    X1 is X // 2,
    while(X1).
```


Start the calculation at a top-level like this:


```prolog
?- while(1024).
```



## PureBasic


```PureBasic
If OpenConsole()

  x.i = 1024
  While x > 0
    PrintN(Str(x))
    x / 2
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python


```python
n = 1024
while n > 0:
    print n
    n //= 2
```



## R


```R
i <- 1024L
while(i > 0)
{
   print(i)
   i <- i %/% 2
}
```



## REBOL


```REBOL
REBOL [
	Title: "Loop/While"
	URL: http://rosettacode.org/wiki/Loop/While
]

value: 1024
while [value > 0][
	print value
	value: to-integer value / 2
]
```



## Racket


### Loop/When


```racket
#lang racket
(let loop ([n 1024])
  (when (positive? n)
    (displayln n)
    (loop (quotient n 2))))
```



### Macro


```racket
#lang racket
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define n 1024)
(while (positive? n)
  (displayln n)
  (set! n (sub1 n)))
```



## Retro


```Retro
1024 [ cr &putn sip 2 / dup ] while
```



## REXX

===version 1, simple===

```rexx
/*REXX program demonstrates a  DO WHILE  with index reduction construct.*/
j=1024                                 /*define the initial value of  J.*/
        do  while  j>0                 /*test if made at the top of  DO.*/
        say j
        j=j%2                          /*in REXX, % is integer division.*/
        end
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```


===version 2, right justified===
Note that a faster version could be implemented with


::::: '''DO WHILE x\==0'''
but that wouldn't be compliant with the wording of the task.

```rexx
/*REXX program demonstrates a  DO WHILE  with index reduction construct.*/
x=1024                                 /*define the initial value of  X.*/
        do  while  x>0                 /*test if made at the top of  DO.*/
        say right(x,10)                /*pretty output by aligning right*/
        x=x%2                          /*in REXX, % is integer division.*/
        end
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

       1024
        512
        256
        128
         64
         32
         16
          8
          4
          2
          1

```


===version 3, faster WHILE comparison===

```rexx
/*REXX program demonstrates a  DO WHILE  with index reduction construct.*/
x=1024                                 /*define the initial value of  X.*/
        do  while  x>>0                /*this is an  exact  comparison. */
        say right(x,10)                /*pretty output by aligning right*/
        x=x%2                          /*in REXX, % is integer division.*/
        end
                                       /*stick a fork in it, we're done.*/
```

'''output''' is the same as version 2.



===version 4, index reduction===

```rexx
/*REXX program demonstrates a  DO WHILE  with index reduction construct.*/
                                       /* [↓] note:   BY   defaults to 1*/
        do j=1024  by 0  while  j>>0   /*this is an  exact  comparison. */
        say right(j,10)                /*pretty output by aligning right*/
        j=j%2                          /*in REXX, % is integer division.*/
        end
                                       /*stick a fork in it, we're done.*/
```

'''output''' is the same as version 2.





## Ring


```ring

i = 1024
while i > 0
      see i + nl
      i = floor(i / 2)
end

```



## Ruby


```ruby
i = 1024
while i > 0 do
   puts i
   i /= 2
end
```

The above can be written in one statement:

```ruby
puts i = 1024
puts i /= 2 while i > 0
```


<code>until ''condition''</code> is equivalent to <code>while not ''condition''</code>.


```ruby
i = 1024
until i <= 0 do
   puts i
   i /= 2
end
```



## Run BASIC


```runbasic
i = 1024
while i > 0
   print i
   i = int(i / 2)
wend
end
```




## Rust


```rust
fn main() {
    let mut n: i32 = 1024;
    while n > 0 {
        println!("{}", n);
        n /= 2;
    }
}
```



## SAS


```sas
data _null_;
n=1024;
do while(n>0);
  put n;
  n=int(n/2);
end;
run;
```



## Sather


```sather
class MAIN is
  main is
    i ::= 1024;
    loop while!(i > 0);
      #OUT + i + "\n";
      i := i / 2;
    end;
  end;
end;
```



## Scala

{{libheader|Scala}}

### Imperative


```scala
var i = 1024
while (i > 0) {
  println(i)
  i /= 2
}
```



### Tail recursive


```scala
  @tailrec
  def loop(iter: Int) {
    if (iter > 0) {
      println(iter)
      loop(iter / 2)
    }
  }
  loop(1024)
```



### Iterator


```scala
  def loop = new Iterator[Int] {
    var i = 1024
    def hasNext = i > 0
    def next(): Int = { val tmp = i; i = i / 2; tmp }
  }
  loop.foreach(println(_))
```



### Stream

Finite stream (1024..0) filtered by takeWhile (1024..1).

```scala
  def loop(i: Int): Stream[Int] = i #:: (if (i > 0) loop(i / 2) else Stream.empty)
  loop(1024).takeWhile(_ > 0).foreach(println(_))
```



## Scheme


```scheme
(do ((n 1024 (quotient n 2)))
    ((<= n 0))
    (display n)
    (newline))
```



## Scilab

{{works with|Scilab|5.5.1}}
<lang>i=1024
while i>0
    printf("%4d\n",i)
    i=int(i/2)
end
```

{{out}}

```txt
1024
 512
 256
 128
  64
  32
  16
   8
   4
   2
   1
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: i is 1024;
  begin
    while i > 0 do
      writeln(i);
      i := i div 2
    end while;
  end func;
```



## SETL


```ada
n := 1024;
while n > 0 loop
    print( n );
    n := n div 2;
end loop;
```



## Sidef


```ruby
var i = 1024
while (i > 0) {
    say i
    i //= 2
}
```



## Simula

{{works with|SIMULA-67}}

```simula
begin
  integer i;
  i:=1024;
  while i>0 do
  begin
     outint(i,5);
     i:=i//2-1
  end
end
```

{{out}}

```txt

 1024  511  254  126   62   30   14    6    2 

```



## Sinclair ZX81 BASIC

The distinctive thing about a <code>while</code> loop is that the conditional test happens before the loop body, not after—so that the code in the loop may be executed zero times.

Since we have no integer type, we floor the result of the division each time.

```basic
10 LET I=1024
20 IF I=0 THEN GOTO 60
30 PRINT I
40 LET I=INT (I/2)
50 GOTO 20
```



## Slate


```slate
#n := 1024.
[n isPositive] whileTrue:
  [inform: number printString.
   n := n // 2]
```



## Smalltalk


```smalltalk
number := 1024.
[ number > 0 ] whileTrue:
  [ Transcript print: number; nl.
  number := number // 2 ]
```



```smalltalk
number := 1024.
[ number <= 0 ] whileFalse:
  [ Transcript print: number; nl.
  number := number // 2 ]
```



## Sparkling


```sparkling
var i = 1024;
while i > 0 {
    print(i);
    i /= 2;
}
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

  n := 1024
  repeat while n > 0
    ser.dec(n)
    ser.tx(32)
    n /= 2

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

{{out}}

```txt

1024 512 256 128 64 32 16 8 4 2 1

```



## SPL


```spl
n = 1024
>
  #.output(n)
  n /= 2
< n!<1
```

{{out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

BEGIN
 DECLARE I SMALLINT DEFAULT 1024;

 Loop: WHILE (I > 0) DO
  CALL DBMS_OUTPUT.PUT_LINE(I);
  SET I = I / 2;
 END WHILE Loop;
END @

```

Output:

```txt

db2 -td@
db2 => SET SERVEROUTPUT ON @
DB20000I  The SET SERVEROUTPUT command completed successfully.
db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

1024
512
256
128
64
32
16
8
4
2
1

```



## Standard ML


```sml
val n = ref 1024;
while !n > 0 do (
  print (Int.toString (!n) ^ "\n");
  n := !n div 2
)
```


But it is more common to write it in a tail-recursive functional style:

```sml
let
  fun loop n =
    if n > 0 then (
      print (Int.toString n ^ "\n");
      loop (n div 2)
    ) else ()
in
  loop 1024
end
```



## Stata


```stata
local n=1024
while `n'>0 {
	display `n'
	local n=floor(`n'/2)
}
```



## Suneido


```Suneido
i = 1024
while (i > 0)
    {
    Print(i)
    i = (i / 2).Floor()
    }
```

{{Out}}

```txt
1024
512
256
128
64
32
16
8
4
2
1
```



## Swift


```swift
var i = 1024
while i > 0 {
  println(i)
  i /= 2
}
```



## Tcl


```tcl
set i 1024
while {$i > 0} {
    puts $i
    set i [expr {$i / 2}]
}
```



## TeX



```TeX

\newcount\rosetta
\rosetta=1024
\loop
    \the\rosetta\endgraf
    \divide\rosetta by 2
    \ifnum\rosetta > 0
\repeat
\bye

```


=={{header|TI-83 BASIC}}==


```ti83b
1024→I
While I>0
Disp I
I/2→I
End

```


=={{header|TI-89 BASIC}}==


```ti89b
Local i
1024 → i
While i > 0
  Disp i
  intDiv(i, 2) → i
EndWhile
```



## TorqueScript

This has to make use of mFloor because torque has automatic type shuffling,
causing an infiniteloop.

```Torque
%num = 1024;
while(%num > 0)
{
    echo(%num);
    %num = mFloor(%num / 2);
}
```


=={{header|Transact-SQL}}==
<lang Transact-SQL>
DECLARE @i INT = 1024;
WHILE @i >0
BEGIN
    PRINT @i;
    SET @i = @i / 2;
END;

```



## Trith


```trith
1024 [dup print 2 / floor] [dup 0 >] while drop
```


```trith
1024 [dup print 1 shr] [dup 0 >] while drop
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
i=1024
LOOP
   PRINT i
   i=i/2
   IF (i==0) EXIT
ENDLOOP
```

{{Out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```



## Unicon

See [[#Icon|Icon]].


## Uniface



```Uniface
variables
	numeric I
endvariables

I = 1024
while (I > 0)
	putmess I
	I = (I/2)[trunc]
endwhile
```



## UNIX Shell

{{works with|Bourne Again SHell}}

```bash
x=1024
while [[ $x -gt 0 ]]; do
  echo $x
  x=$(( $x/2 ))
done
```



## UnixPipes


```bash
(echo 1024>p.res;tail -f p.res) | while read a ; do
   test $a -gt 0 && (expr $a / 2  >> p.res ; echo $a) || exit 0
done
```



## Ursa


```ursa
decl int n
set n 1024

while (> n 0)
    out n endl console
    set n (int (/ n 2))
end while
```



## Ursala

Unbounded iteration is expressed with the -> operator.
An expression (p-> f) x, where p is a predicate and f is a function,
evaluates to x, f(x), or f(f(x)), etc. as far as necessary to falsify p.

Printing an intermediate result on each iteration is a bigger problem
because side effects are awkward.
Instead, the function g in this example iteratively constructs a list of results,
which is displayed on termination.

The argument to g is the unit list <1024>.
The predicate p is ~&h, the function that tests whether
the head of a list is non-null (equivalent to non-zero).
The iterated function f is that which conses the
truncated half of the head of its argument with a copy of the whole argument.
The main program takes care of list reversal and formatting.

```Ursala
#import nat

g = ~&h-> ^C/half@h ~&

#show+

main = %nP*=tx g <1024>
```

{{Out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```

Explicit iteration has its uses but there are always alternatives.
The same output is produced by the following main program
using bit manipulation.

```Ursala
main = %nP*=tK33 1024
```



## V


```v>1024 [0
] [
   dup puts
   2 / >int
] while
```



## VBA


```VB
Public Sub LoopsWhile()
    Dim value As Integer
    value = 1024
    Do While value > 0
        Debug.Print value
        value = value / 2
    Loop
End Sub
```


## Vedit macro language


```vedit
#1 = 1024
while (#1 > 0) {
    Num_Type(#1)
    #1 /= 2
}
```

or with for loop:

```vedit
for (#1 = 1024; #1 > 0; #1 /= 2) {
    Num_Type(#1)
}
```



## Verbexx


```verbexx
//  Basic @LOOP while: verb

@LOOP init:{@VAR n = 1024} while:(n > 0) next:{n /= 2}
{
     @SAY n;
};
```



## Vim Script


```vim
let i = 1024
while i > 0
    echo i
    let i = i / 2
endwhile
```



## Visual Basic .NET


```vbnet
Dim x = 1024
Do
    Console.WriteLine(x)
    x = x \ 2
Loop While x > 0
```



## Wart


```wart
i <- 1024
while (i > 0)
  prn i
  i <- (int i/2)
```



## Wee Basic


```Wee Basic
let number=1024
while number>0.5
print 1 number
let number=number/2
wend
end
```



## Whitespace


```Whitespace

















```

Pseudo-assembly equivalent:

```asm
push 1024

0:
    dup onum push 10 ochr
    push 2 div dup
    push 0 swap sub
        jn 0
        pop exit
```



## X86 Assembly


```asm

; NASM 64 bit X86-64 assembly on Linux

global main
extern printf

segment .data

printffmt db `%ld\n`,0

segment .text

main:
    push rbp
    mov rbp,rsp

; used rbx and r12 because printf preserves these values

    mov rbx,1024                 ; start with 1024
    mov r12,2                    ; load 2 as divisor

.toploop                         ; top of while loop
    cmp rbx,0                    ; compare to 0
    jle .done                    ; exit 0 or less

    lea rdi,[printffmt]          ; print number in rsi
    mov rsi,rbx                  ; mov to rsi as argument
    call printf

; calculate n/2 and save
    xor rdx,rdx                  ; clear rdx for division
    mov rax,rbx                  ; mov number to rax for division
    idiv r12                     ; divide by 2
    mov rbx,rax                  ; save n/2

    jmp .toploop                 ; next loop

.done
    xor rax,rax                  ; return code 0
    leave                        ; fix stack
    ret                          ; return

```



## XLISP

The specification calls for an integer value and for the loop to run <tt>WHILE</tt> that value is greater than zero. In a dynamically typed language like XLISP, variables cannot be declared as integer or real; but the same result is obtained by looping <tt>WHILE</tt> the value of the variable <i>i</i> is greater than or equal to one.

```xlisp
(DEFINE I 1024)

(WHILE (>= I 1)
    (PRINT I)
    (DEFINE I (/ I 2)))
```



## XPL0


```XPL0
code CrLf=9, IntOut=11;
int I;
[I:= 1024;
while I>0 do
        [IntOut(0, I);  CrLf(0);
        I:= I>>1;       \(same as I/2 for positive I)
        ];
]
```



## zkl


```zkl
n:=1024; while(n>0){println(n); n/=2;}
```

{{out}}

```txt

1024
512
256
128
64
32
16
8
4
2
1

```

