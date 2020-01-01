+++
title = "Loops/Downward for"
description = ""
date = 2019-10-12T23:11:19Z
aliases = []
[extra]
id = 2830
[taxonomies]
categories = []
tags = []
+++

{{task|Iteration}}
[[Category:Simple]]

;Task:
Write a   <big><big> ''for'' </big></big>   loop which writes a countdown from   '''10'''   to   '''0'''.


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

Use of BXLE and BCT opcodes.

```360asm
*        Loops/Downward for        27/07/2015
LOOPDOWN CSECT
         USING  LOOPDOWN,R12
         LR     R12,R15            set base register
BEGIN    EQU    *
*        fisrt loop with a BXLE    BXLE: Branch on indeX Low or Equal
         LH     R2,=H'11'          from 10 (R2=11) index
         LH     R4,=H'-1'          step -1 (R4=-1)
         LH     R5,=H'-1'          to 0    (R5=-1)
LOOPI    BXLE   R2,R4,ELOOPI       R2=R2+R4 if R2<=R5 goto ELOOPI
         XDECO  R2,BUFFER          edit R2
         XPRNT  BUFFER,L'BUFFER    print
         B      LOOPI
ELOOPI   EQU    *
*        second loop with a BCT    BCT: Branch on CounT
         LA     R2,10              index   R2=10
         LA     R3,11              counter R3=11
LOOPJ    XDECO  R2,BUFFER          edit R2
         XPRNT  BUFFER,L'BUFFER    print
         BCTR   R2,0               R2=R2-1
ELOOPJ   BCT    R3,LOOPJ           R3=R3-1 if R3<>0 goto LOOPI
RETURN   XR     R15,R15            set return code
         BR     R14                return to caller
BUFFER   DC     CL80' '
         YREGS
         END    LOOPDOWN
```



## 6502 Assembly

Code is called as a subroutine (i.e. JSR Start).
Printing routines are only partially coded here, specific OS/hardware routines for printing are left unimplemented.

```6502asm
;An OS/hardware specific routine that is setup to display the Ascii character
;value contained in the Accumulator
Send 		= 	$9000		;routine not implemented here
PrintNewLine	=	$9050		;routine not implemented here

  		*= 	$8000		;set base address
Start		PHA			;push Accumulator and Y register onto stack
		TYA
		PHA
		LDY 	#10		;set Y register to loop start value
		TYA			;place loop value in the Accumulator
Loop		JSR	PrintTwoDigits
		JSR   PrintNewLine
		DEY			;decrement loop value
		BPL	Loop		;continue loop if sign flag is clear
		PLA			;pop Y register and Accumulator off of stack
		TAY
		PLA
		RTS			;exit

;Print value in Accumulator as two hex digits
PrintTwoDigits
		PHA
		LSR
		LSR
		LSR
		LSR
		JSR     PrintDigit
		PLA
		AND     #$0F
		JSR     PrintDigit
		RTS

;Convert value in Accumulator to an Ascii hex digit
PrintDigit
		ORA	#$30
		JSR	Send		;routine not implemented here
		RTS
```



## Ada


```ada
for I in reverse 0..10 loop
   Put_Line(Integer'Image(I));
end loop;
```



## Agena

Tested with Agena 2.9.5 Win32

```agena
for i from 10 downto 0 do
    print( i )
od
```



## ALGOL 60

'''Based on the 1962 Revised Repport on ALGOL''':
  '''begin'''
    '''integer''' i;
    '''for''' i:=10 '''step''' -1 '''until''' 0 '''do'''
      outinteger(i)
  '''end'''
{{works with|ALGOL 60|OS/360}}

```algol60
'BEGIN' 'COMMENT' Loops/Downward for - Algol60 - 23/06/2018;
  'INTEGER' I;
  'FOR' I := 10 'STEP' -1 'UNTIL' 0 'DO'
    OUTINTEGER(1,I)
'END'
```




## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
FOR i FROM 10 BY -1 TO 0 DO
    print((i,new line))
OD
```

As a common extension the DOWNTO is sometimes included to optimise
the loop termination logic.  The DOWNTO is available in Marcel's
[[ALGOL 68G]] and [[Cambridge ALGOL 68C]].

```algol68
FOR i FROM 10 DOWNTO 0 DO
    print((i,new line))
OD
```



## ALGOL W


```algolw
begin
    for i := 10 step -1 until 0 do
    begin
        write( i )
    end
end.
```



## AmigaE


```amigae
PROC main()
  DEF i
  FOR i := 10 TO 0 STEP -1
    WriteF('\d\n', i)
  ENDFOR
ENDPROC
```



## AppleScript


```AppleScript
repeat with i from 10 to 0 by -1
  log i
end repeat
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program loopdownward.s   */

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
    mov r4,#10
1:    @ begin loop
    mov r0,r4
    ldr r1,iAdrsMessValeur     @ display value
    bl conversion10             @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    subs r4,#1                   @ decrement counter
    bge 1b                      @ loop if greather

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



```



## Arturo


```arturo
loop $(range 10 0) {
	print &
}
```


{{out}}


```txt
10
9
8
7
6
5
4
3
2
1
0
```



## AutoHotkey


```AutoHotkey
x := 10
While (x >= 0)
{
  output .= "`n" . x
  x--
}
MsgBox % output

```



## AWK


```awk
BEGIN {
  for(i=10; i>=0; i--) {
     print i
  }
}
```



## Axe

Axe does not support for loops with step sizes other than 1.

```axe
For(I,0,10)
 Disp 10-I▶Dec,i
End
```



## BASIC


```qbasic
for i = 10 to 0 step -1
   print i
next i
```


=
## Applesoft BASIC
=

```ApplesoftBasic
FOR I = 10 TO 0 STEP -1 : PRINT I : NEXT I
```


=
## BaCon
=

```freebasic
' Downward for
FOR i = 10 DOWNTO 0 : PRINT i : NEXT
```


=
## Commodore BASIC
=

```basic
10 FOR I = 10 TO 0 STEP -1
20 PRINT I
30 NEXT
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 FOR I=10 TO 0 STEP-1
110   PRINT I
120 NEXT
```



## Batch File


```dos
@echo off
for /l %%D in (10,-1,0) do echo %%D
```



## BBC BASIC


```bbcbasic
      FOR i% = 10 TO 0 STEP -1
        PRINT i%
      NEXT
```



## bc


```bc
for (i = 10; i >= 0; i--) i
quit
```



## Befunge


```befunge>55+
:.:v
@  ^ -1_
```



## Bracmat


```bracmat
  10:?i
& whl'(out$!i&!i+-1:~<0:?i)
```



## Brat


```brat
10.to 0 { n | p n }
```



## C


```c
int i;
for(i = 10; i >= 0; --i)
  printf("%d\n",i);
```



## C++


```cpp
for(int i = 10; i >= 0; --i)
  std::cout << i << "\n";
```


## C#


```c#
for (int i = 10; i >= 0; i--)
{
   Console.WriteLine(i);
}
```



## Ceylon


```ceylon
for (i in 10..0) {
    print(i);
}
```



## Clojure


```c#
(doseq [x (range 10 -1 -1)] (println x))
```



## COBOL

free-form

```cobol
identification division.
program-id. countdown.
environment division.
data division.
working-storage section.
01	counter 		pic 99.
	88	counter-done	value 0.
01	counter-disp	pic Z9.
procedure division.
	perform with test after varying counter from 10 by -1 until counter-done
		move counter to counter-disp
		display counter-disp
	end-perform
	stop run.
```

{{out}}

```txt
10
 9
 8
 7
 6
 5
 4
 3
 2
 1
 0
```



## CoffeeScript

This could be written either in the array comprehension style,
or in "regular" for loop style.

```coffeescript
# The more compact "array comprehension" style
console.log i for i in [10..0]

# The "regular" for loop style.
for i in [10..0]
	console.log i

# More compact version of the above
for i in [10..0] then console.log i
```


```txt
10
9
8
7
6
5
4
3
2
1
0
```

(the output is repeated three times; once for each loop)


## ColdFusion

With tags:

```cfm
<cfloop index = "i" from = "10" to = "0" step = "-1">
  #i#
</cfloop>
```

With script:

```cfm><cfscript

  for( i = 10; i <= 0; i-- )
  {
    writeOutput( i );
  }
</cfscript>
```



## Common Lisp


```lisp
(loop for i from 10 downto 1 do
  (print i))
```



## Chapel



```chapel
for i in 1..10 by -1 do
	writeln(i);
```


In case you wonder why it is not written as <tt>10..1 by -1</tt>: <tt>by</tt> is an operator that works on ranges, and it should work the same when the range was defined earlier, like in


```chapel
var r = 1..10;
for i in r by -1 do { ... }
```



## Clipper


```clipper
   FOR i := 10 TO 0 STEP -1
      ? i
   NEXT
```



## D


```d
import std.stdio: writeln;

void main() {
    for (int i = 10; i >= 0; --i)
        writeln(i);
    writeln();

    foreach_reverse (i ; 0 .. 10 + 1)
        writeln(i);
}
```

{{out}}

```txt
10
9
8
7
6
5
4
3
2
1
0

10
9
8
7
6
5
4
3
2
1
0
```



## dc


does not use GNU extensions

[]s. is a comment

c clears the stack

[~...]p s. to print strings

l<register>x executes the macro

uses the macro f - [p] to print, this can be replaced by any complex expressions.


```dc
c

[macro s(swap) - (a b : b a)]s.
[Sa Sb La Lb] ss

[macro d(2dup) - (a b : a b a b)]s.
[Sa d Sb La d Lb lsx] sd

[macro m(for) - ]s.
[lfx 1 - ldx !<m ] sm

0 10 ldx [p] sf !<m
q
```


Using it

```dc
|dc < ./for.dc
10
9
...
0
```



## Delphi

:''See [[#Pascal|Pascal]]''


## DWScript


```pascal
for i := 10 downto 0 do
  PrintLn(i);
```



## E



```e
for i in (0..10).descending() { println(i) }
```



## EasyLang


<lang>for i = 10 downto 0
  print i
.
```



## EchoLisp


```scheme

(for ((longtemps-je-me-suis-couché-de-bonne-heure (in-range 10 -1 -1)))
     (write longtemps-je-me-suis-couché-de-bonne-heure))
    → 10 9 8 7 6 5 4 3 2 1 0

```



## EDSAC order code

Including a full routine to print integers in decimal would probably be overkill; at least, it would obscure what is essentially a simple program. We therefore cheat slightly by printing "10\r\n" manually, and using the loop only to print "9\r\n" down to "0\r\n". Note that character codes are stored in the high 5 bits of the 17-bit EDSAC word: so we actually count down from 36,864 to 0 in steps of 4,096.

```edsac
[ Loop with downward counter

### ====================


  A program for the EDSAC

  Prints the integers 10 down to 0

  The counter is stored at address 20@

  Its initial value is 9 * 2^12
  (9 in the high 5 bits, representing
  the character '9') and it counts
  down in steps of 2^12

  Works with Initial Orders 2 ]

        T56K    [ set load point ]
        GK      [ set base address ]

[ orders ]

        O14@    [ print figure shift ]
        O15@    [ print '1' ]
        O16@    [ print '0' ]
        O17@    [ print CR ]
        O18@    [ print LF ]

[ 5 ]   O20@    [ print c ]
        O17@    [ print CR ]
        O18@    [ print LF ]

        T19@    [ acc := 0 ]
        A20@    [ acc += c ]
        S15@    [ acc -:= character '1' ]
        U20@    [ c := acc ]

        E5@     [ branch on non-negative ]

        ZF      [ stop ]

[ constants ]

[ 14 ]  #F      [ πF -- figure shift ]
[ 15 ]  QF      [ character '1' ]
[ 16 ]  PF      [ character '0' ]
[ 17 ]  @F      [ θF -- CR ]
[ 18 ]  &F      [ ΔF -- LF ]

[ variables ]

[ 19 ]  P0F     [ used to clear acc ]
[ 20 ]  OF      [ character c = '9' ]

        EZPF    [ start when loaded ]
```



## EGL


```EGL
for ( i int from 10 to 0 decrement by 1 )
   SysLib.writeStdout( i );
end
```



## Elixir


```elixir
iex(1)> Enum.each(10..0, fn i -> IO.puts i end)
10
9
8
7
6
5
4
3
2
1
0
:ok
```



## Erlang


```erlang
%% Implemented by Arjun Sunel
-module(downward_loop).
-export([main/0]).

main() ->
	for_loop(10).

 for_loop(N) ->
 	if N > 0 ->
		io:format("~p~n",[N] ),
		for_loop(N-1);
	true ->
		io:format("~p~n",[N])
	end.

```

{{out}}

```txt
10
9
8
7
6
5
4
3
2
1
0
ok
```



## ERRE


```ERRE

   FOR I%=10 TO 0 STEP -1 DO
     PRINT(I%)
   END FOR

```



## Euphoria


```euphoria
for i = 10 to 0 by -1 do
    ? i
end for
```



## Ela



### Standard Approach



```ela
open monad io

each [] = do return ()
each (x::xs) = do
  putStrLn $ show x
  each xs

each [10,9..0] ::: IO
```



### Alternative Approach



```ela
open monad io

countDown m n | n < m = do return ()
              | else = do
                  putStrLn $ show n
                  countDown m (n - 1)

_ = countDown 0 10 ::: IO
```



## Factor


```factor>11 iota <reversed
 [ . ] each
```



## FALSE


```false
10[$0>][$." "1-]#.
```



## Fantom



```fantom

class DownwardFor
{
  public static Void main ()
  {
    for (Int i := 10; i >= 0; i--)
    {
      echo (i)
    }
  }
}

```



## FBSL


```qbasic
#APPTYPE CONSOLE

FOR DIM i = 10 DOWNTO 0
    PRINT i
NEXT

PAUSE

```



## Forth

Unlike the incrementing 10 0 DO-LOOP, this will print eleven numbers. The LOOP words detect crossing the floor of the end limit.

```forth
: loop-down  0 10 do  i .  -1 +loop ;
```



## Fortran

{{Works with|Fortran|90 and later}}

```fortran
DO i = 10, 0, -1
  WRITE(*, *) i
END DO
```


{{works with|Fortran|77 and later}}

```fortran
      PROGRAM DOWNWARDFOR
C Initialize the loop parameters.
        INTEGER I, START, FINISH, STEP
        PARAMETER (START = 10, FINISH = 0, STEP = -1)

C If you were to leave off STEP, it would default to positive one.
        DO 10 I = START, FINISH, STEP
          WRITE (*,*) I
   10   CONTINUE

        STOP
      END
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

For i As Integer = 10 To 0 Step -1
  Print i; " ";
Next
Print
Sleep
```


{{out}}

```txt

 10  9  8  7  6  5  4  3  2  1  0

```



## Frink


```frink

for i = 10 to 0 step -1
   println[i]

```


=={{header|F_Sharp|F#}}==
Using an enumerable expression:

```fsharp
for i in 10..-1..0 do
  printfn "%d" i
```


Using the 'downto' keyword:

```fsharp
for i = 10 downto 0 do
  printfn "%d" i
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long i

for i = 10 to 0 step -1
print i
next

```

Output:

```txt

 10
 9
 8
 7
 6
 5
 4
 3
 2
 1
 0

```



## GAP


```gap
for i in [10, 9 .. 0] do
    Print(i, "\n");
od;
```



## GML


```GML
for(i = 10; i >= 0; i -= 1)
    show_message(string(i))
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=b236db5bdb1087fa90e934a5a8210e1f Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount As Short

For siCount = 10 DownTo 0
  Print siCount;;
Next

End
```

Output:

```txt

10 9 8 7 6 5 4 3 2 1 0

```



## Go


```go>for i := 10; i
= 0; i-- {
  fmt.Println(i)
}
```



## Groovy


```groovy
for (i in (10..0)) {
    println i
}
```


=={{header|GW-BASIC}}==
{{works with|PC-BASIC|any}}

```qbasic

10 FOR I% = 10 TO 0 STEP -1
20  PRINT I%
30 NEXT I%

```



## Harbour


```visualfoxpro
FOR i := 10 TO 0 STEP -1
   ? i
NEXT
```



## Haskell


```haskell
import Control.Monad

main :: IO ()
main = forM_ [10,9 .. 0] print
```



## hexiscript


```hexiscript>for let i 10; i
= 0; i--
  println i
endfor
```



## HicEst


```hicest
DO i = 10, 0, -1
  WRITE() i
ENDDO
```



## HolyC


```holyc
I8 i;
for (i = 10; i >= 0; --i)
  Print("%d\n", i);
```



## IDL


Using a loop (with an "increment of minus one" ):


```idl
for i=10,0,-1 do print,i
```


But in IDL one would rarely use loops (for anything) since practically everything can be done with vectors/arrays.

The "IDL way of doing things" for the countdown requested in the task would probably be this:


```idl
print,10-indgen(11)
```


=={{header|Icon}} and {{header|Unicon}}==
There are four looping controls 'every', 'repeat', 'until', and 'while' (see [[Icon%2BUnicon/Intro#Looping_Controls|Introduction to Icon and Unicon/Looping Controls]] for more information.)  The closest to a 'for' loop is 'every'.

```Icon
every i := 10 to 0 by -1 do {
   # things to do within the loop
   }

```



## Inform 6


```Inform 6
for(i = 10: i >= 0: i--)
    print i, "^";
```



## Io


```Io
for(i,10,0,-1,
    i println
)
```



## J

J is array-oriented, so there is very little need for loops.  For example, one could satisfy this task this way:

   ,. i. -11

J does support loops for those times they can't be avoided (just like many languages support gotos for those time they can't be avoided).

```j
3 : 0 ] 11
  for_i. i. - y do.
    smoutput i
  end.
)
```


Though it's rare to see J code like this.

That said, a convenient routine for generating intervals in J might be:


```J
thru=: <. + i.@(+*)@-~
```


For example:


```J
   10 thru 0
10 9 8 7 6 5 4 3 2 1 0
```


(or <code>,.10 thru 0</code> if you want each number on a line by itself)

This verb "thru" will count up or down, starting and stop at the indicated left and right ending points.


## Java


```java
for(i = 10; i >= 0; --i){
   System.out.println(i);
}
```



## JavaScript


```javascript
for (var i=10; i>=0; --i) print(i);
```


Alternatively, remaining for the moment within an imperative idiom of JavaScript, in which programs are composed of statements, we could trim the computational costs over longer reversed iterations by moving the mutation into the test, and dropping the third term of a for() statement:


```JavaScript
for (var i = 11; i--;) console.log(i);
```


and it sometimes might be more natural, especially at scales at which optimisation becomes an issue, to go one step further and express the same computation with the more economical while statement.


```JavaScript
var i = 11;
while (i--) console.log(i);
```


In a functional idiom of JavaScript, however, we need an expression with a value (which can be composed within superordinate expressions), rather than a statement, which produces a side-effect but returns no information-bearing value.

If we have grown over-attached to the English morpheme 'for', we might think first of turning to '''Array.forEach()''',  and write something like:


```JavaScript
function range(m, n) {
  return Array.apply(null, Array(n - m + 1)).map(
    function (x, i) {
      return m + i;
    }
  );
}

range(0, 10).reverse().forEach(
  function (x) {
    console.log(x);
  }
);
```



but this is still a statement with side-effects, rather than a composable expression with a value.

We can get an expression (assuming that the range() function (above) is defined) but replacing Array.forEach with '''Array.map()'''


```JavaScript
console.log(
  range(0, 10).reverse().map(
    function (x) {
      return x;
    }
  ).join('\n')
);
```


but in this case, we are simply mapping an identity function over the values, so the expression simplifies down to:


```JavaScript
console.log(
    range(0, 10).reverse().join('\n')
);
```



## jq

If range/3 is available in your jq:

```jq
range(10;-1;-1)
```

Otherwise:
 range(-10;1) | -.


## Julia


```julia
for i in 10:-1:0
    println(i)
end
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    for (i in 10 downTo 0) print("$i ")
}
```


{{out}}

```txt

10 9 8 7 6 5 4 3 2 1 0

```



## Lasso


```Lasso
loop(-from=10, -to=0, -by=-1) => {^ loop_count + ' ' ^}
```



## Lhogho

Slightly different syntax for <code>for</code> compared to Logo.

```logo
for "i [10 0] [print :i]
```



## Liberty BASIC


```lb

for i = 10 to 0 step -1
   print i
next i
end

```



## Lingo


```lingo
repeat with i = 10 down to 0
  put i
end repeat
```



## Lisaac


```Lisaac
10.downto 0 do { i : INTEGER;
  i.println;

};
```



## LiveCode

Livecode's repeat "for" variant does not have a "down to" form, in a function you would need to manually decrement a counter

```LiveCode
local x=10
repeat for 10 times
  put x & return
  add -1 to x
end repeat
```


A more idiomatic approach using "with" variant of repeat which does have a "down to" form

```LiveCode
repeat with n=10 down to 1
  put n
end repeat
```



## Logo

If the limit is less than the start, then FOR decrements the control variable. Otherwise, a fourth parameter could be given as a custom increment.

```logo
for [i 10 0] [print :i]
```



## Lua


```lua

for i=10,0,-1 do
  print(i)
end

```




## M2000 Interpreter

M2000 can operate a For like in BASIC or Like M2000. In M2000 mode, a For always execute at least one time the block inside. This FOR use absolute value of step, except when we have start value and end value the same value, so from sign of step, interpreter calculate the exit value.

We can change the iterator variable of a For, but this variable is a copy of actual iterator, and next step get the proper value. So we can't change the numbers of steps, but we can use continue to skip rest of code and execute next step, or exit to exit block and stop loop. Also we can use Goto to stop loop and continue from a label.

There is a slower For, the For Next style:


```M2000 Interpreter

For i=1 to 10 step 2 : Print i : Next i

```

We have to use Exit For to exit from that type of For.

This is not an error (each for has private counter value):

for i=1 to 10 :for i=1 to 2:Print i:Next i:Next i

We get 10 times two values: 1 2



```M2000 Interpreter

Form 80, 50
Module Checkit {
      set switches "+For"
      For i=10 to 1 step -1 {
            Print i
      }
      Print i=0
      \\ this For switch make it like For in BASIC
      \\ block skipped
      For i=1 to 10 step -1 {
            Print i
      }
      print i=1
      \\ but this is the default behavior
      \\
      set switches "-For"
      \\ sign of step used when start is same as end to calculate the exit value of i
      \\ This is the standard, and a For always execute at least one time the block.
      \\ use absulute step_Value. Because 10>1 direction is downward.
      For i=10 to 1 step -1 {
            Print i
      }
      Print i=0
      \\  loop from 1 to 10, using abs(step_value)
      For i=1 to 10 step -1 {
            Print i
      }
      print i=11
      For i=1 to 1 step -1 {
            Print i
      }
      Print i=0
}
CheckIt

```



## M4


```M4
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2 $3),1,
   `pushdef(`$1',$2)$5`'popdef(`$1')$0(`$1',eval($2+$4),$3,$4,`$5')')')')dnl

for(`x',`10',`>=0',`-1',`x
')
```



## Maple

Using an explicit loop:

```Maple
for i from 10 to 0 by -1 do print(i) end:
```

Pushing the loop into the kernel:

```Maple
seq(print(i),i=10..0,-1)
```



## Mathematica

Mathematica provides several ways to iterate over a range of numbers,
small subtle differences are amongst them.
3 possible implementations are (exactly the same output):

Using For:

```Mathematica
For[i = 10, i >= 0, i--, Print[i]]
```

Using Do:

```Mathematica
Do[Print[i], {i, 10, 0, -1}]
```

Using Scan:

```Mathematica
Scan[Print, Range[10, 0, -1]]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
    for k = 10:-1:0,
        printf('%d\n',k)
    end;
```


A vectorized version of the code is


```Matlab
  printf('%d\n',10:-1:0);
```



## Maxima


```maxima
for i from 10 thru 0 step -1 do print(i);
```



## MAXScript


```maxscript
for i in 10 to 0 by -1 do print i
```



## Mercury

<lang>:- module loops_downward_for.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
   Print = (pred(I::in, !.IO::di, !:IO::uo) is det :-
       io.write_int(I, !IO), io.nl(!IO)
   ),
   int.fold_down(Print, 1, 10, !IO).
```



## Metafont



```metafont
for i = 10 step -1 until 0: show i; endfor
end
```


The basic set of macros for Metafont defines <tt>downto</tt>, so that we can write


```metafont>for i = 10 downto 0: show i; endfor end</lang



## Microsoft Small Basic


```microsoftsmallbasic

For i = 10 To 0 Step -1
  TextWindow.WriteLine(i)
EndFor

```


=={{header|МК-61/52}}==
<lang>1	0	П0	ИП0	L0	03	С/П
```


=={{header|Modula-2}}==

```modula2
MODULE Downward;
  IMPORT InOut;

  VAR
    i: INTEGER;

BEGIN
  FOR i := 10 TO 0 BY -1 DO
    InOut.WriteInt(i, 2);
    InOut.WriteLn
  END
END Downward.
```


=={{header|Modula-3}}==

```modula3
FOR i := 10 TO 0 BY -1 DO
  IO.PutInt(i);
END;
```



## MUMPS


```MUMPS
LOOPDOWN
 NEW I FOR I=10:-1:1 WRITE I WRITE:I'=1 ", "
 KILL I QUIT
```



## NewLISP


```NewLISP
(for (i 10 0)
  (println i))
```



## Nim


```nim
for x in countdown(10,0): echo(x)
```

{{out}}

```txt
10
9
8
7
6
5
4
3
2
1
0
```



## Nemerle


```Nemerle
for (i = 10; i >= 0; i--) {WriteLine($"$i")}
```


```Nemerle
foreach (i in [10, 9 .. 0]) {WriteLine($"$i")}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

  say
  say 'Loops/Downward for'

  loop i_ = 10 to 0 by -1
    say i_.right(2)
    end i_

```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 FOR 1=10 TO 0 STEP -1
20 PRINT I
30 NEXT
```


=={{header|Oberon-2}}==

```oberon2
FOR i := 10 TO 0 BY -1 DO
  Out.Int(i,0);
END;
```



## Objeck


```objeck

for(i := 10; i >= 0; i--;) {
   i->PrintLine();
};

```



## OCaml


```ocaml
for i = 10 downto 0 do
  Printf.printf "%d\n" i
done
```



## Octave



```octave
for i = 10:-1:0
  % ...
endfor
```



## Oforth



```Oforth
10 0 -1 step: i [ i println ]
```



## Oz


```oz
for I in 10..0;~1 do
   {Show I}
end
```



## PARI/GP


```parigp
forstep(n=10,0,-1,print(n))
```



## Pascal


```pascal
for i := 10 downto 0 do
  writeln(i);
```



## Peloton

English fixed-length opcodes

```sgml><@ ITEFORLITLITLITLIT
0|<@ SAYVALFOR>...</@>|10|-1</@>
```


Simplified Chinese variable-length opcodes

```sgml
<# 迭代迭代次数字串字串字串字串>0|<# 显示值迭代次数>...</#>|10|-1</#>
```



## Perl


```perl
foreach (reverse 0..10) {
  print "$_\n";
}
```



## Perl 6

{{works with|Rakudo Star|2010.08}}


```perl6
for 10 ... 0 {
    .say;
}
```



## Phix


```Phix
for i=10 to 0 by -1 do
    ?i
end for
```



## PHP


```php
for ($i = 10; $i >= 0; $i--)
  echo "$i\n";
```

or

```php
foreach (range(10, 0) as $i)
  echo "$i\n";
```



## PicoLisp


```PicoLisp
(for (I 10 (ge0 I) (dec I))
   (println I) )
```

or:

```PicoLisp
(mapc println (range 10 0))
```



## Pike


```pike
int main(){
   for(int i = 10; i >= 0; i--){
      write(i + "\n");
   }
}
```



## PL/I


```PL/I

do i = 10 to 0 by -1;
   put skip list (i);
end;

```



## Pop11


```pop11
lvars i;
for i from 10 by -1 to 0 do
   printf(i, '%p\n');
endfor;
```



## PowerShell


```powershell
for ($i = 10; $i -ge 0; $i--) {
    $i
}
```

Alternatively, the range operator might be used as well which simply returns a contiguous range of integers:

```powershell>10..0</lang



## PureBasic


```PureBasic
For i=10 To 0 Step -1
  Debug i
Next
```



## Prolog

Although Prolog has a between(Lo,Hi,Value) iterator, there is no built in equivalent for iterating descending values.  This is not a show stopper, as it's easy enough to write one.

```Prolog
rfor(Hi,Lo,Hi) :- Hi >= Lo.
rfor(Hi,Lo,Val) :- Hi > Lo, H is Hi - 1, !, rfor(H,Lo,Val).

reverse_iter :-
  rfor(10,0,Val), write(Val), nl, fail.
reverse_iter.
```


```txt
?- reverse_iter.
10
9
8
7
6
5
4
3
2
1
0
true.

```



## Python


```python
for i in xrange(10, -1, -1):
    print i
```



###  List comprehension



```python
[i for i in xrange(10, -1, -1)]
```


```python
import pprint
pprint.pprint([i for i in xrange(10, -1, -1)])

```



## R


```R
for(i in 10:0) {print(i)}
```



## Racket


```racket

#lang racket

(for ([i (in-range 10 -1 -1)])
  (displayln i))

```



## REBOL


```REBOL
for i 10 0 -1 [print i]
```



## Retro


```Retro
11 [ putn space ] iterd
```



## REXX


### version 1

(equivalent to version 2 and version 3)

```rexx
  do j=10  to 0  by -1
  say j
  end
```



### version 2

(equivalent to version 1 and version 3)

```rexx
  do j=10  by -1  to 0
  say j
  end
```



### version 3

(equivalent to version 1 and version 2)


Anybody who programs like this should be hunted down and shot like dogs!


Hurrumph!  Hurrumph!

```rexx
  do j=10  by -2  to 0
  say j
  j=j+1     /*this increments the  DO  index.   Do NOT program like this! */
  end
```



### version 4

This example isn't compliant to the task,
but it shows that the increment/decrement can be a non-integer:

```rexx
  do j=30  to 1  by -.25
  say j
  end
```


## Ring

count from 10 to 0 by -1 step:

```ring

for i = 10 to 0 step -1 see i + nl next

```



## Ruby


```ruby
10.downto(0) do |i|
   puts i
end
```



## Rust


```rust
fn main() {
    for i in (0..=10).rev() {
        println!("{}", i);
    }
}
```



## Salmon


```Salmon
for (x; 10; x >= 0; -1)
    x!;
```



## Sather


```sather
class MAIN is
  main is
    i:INT;
    loop i := 10.downto!(0);
       #OUT  + i + "\n";
    end;
  end;
end;
```



## Scala


```scala
for(i <- 10 to 0 by -1) println(i)
//or
10 to 0 by -1 foreach println
```



## Scheme


```scheme
(do ((i 10 (- i 1)))
    ((< i 0))
    (display i)
    (newline))
```



## Seed7


```seed7
for i range 10 downto 0 do
  writeln(i);
end for;
```



## Scilab

{{works with|Scilab|5.5.1}}
<lang>for i=10:-1:0
    printf("%d\n",i)
end
```

{{out}}
<pre style="height:20ex">
10
9
8
7
6
5
4
3
2
1
0

```



## Sidef

'''for(;;)''' loop:

```ruby
for (var i = 10; i >= 0; i--) {
    say i
}
```


'''for-in''' loop:

```ruby
for i in (11 ^.. 0) {
    say i
}
```


'''.each''' method:

```ruby
10.downto(0).each { |i|
    say i
}
```



## Simula


```simula
BEGIN
    Integer i;
    for i := 10 step -1 until 0 do
    BEGIN
        OutInt(i, 2);
        OutImage
    END
END
```



## Slate


```slate
10 downTo: 1 do: [| :n | print: n]
```



## Smalltalk


```smalltalk
10 to: 1 by: -1 do:[:aNumber |
  aNumber display.
  Character space display.
]
```



## SNOBOL4


```snobol4
        COUNT = 10
LOOP    OUTPUT = COUNT
        COUNT = COUNT - 1
        GE(COUNT, 0)     :S(LOOP)
END
```



## SNUSP


```snusp>++++++++++
++++++++++!/- @!\=@\.@@@-@-----#   atoi
    \n      counter  #\?>.</  \ @@@+@+++++#   itoa
                       loop
```



## Sparkling


```sparkling>for var i = 10; i
= 0; i-- {
    print(i);
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

  repeat n from 10 to 0
    ser.dec(n)
    ser.tx(32)

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)
```

{{out}}

```txt
10 9 8 7 6 5 4 3 2 1 0
```



## SPL


```spl>
 i, 10..0,-1
  #.output(i)
<
```



## SSEM

The SSEM can't print, so the results are stored in an array at addresses 22 to 31. Array access is done using self-modifying code: on each iteration we subtract the current value of <tt>n</tt> (stored at address 18) from the illegal instruction <tt>c to 32</tt>, yielding the actual instruction we use to store <tt>n</tt> into the array.

```ssem
10001000000000100000000000000000   0. -17 to c
11001000000001100000000000000000   1. c to 19
11001000000000100000000000000000   2. -19 to c
01001000000000010000000000000000   3. Sub. 18
00010000000001100000000000000000   4. c to 8
01001000000000100000000000000000   5. -18 to c
11001000000001100000000000000000   6. c to 19
11001000000000100000000000000000   7. -19 to c
00000000000000000000000000000000   8. generated at run time
11110000000000010000000000000000   9. Sub. 15
01001000000001100000000000000000  10. c to 18
11110000000000010000000000000000  11. Sub. 15
00000000000000110000000000000000  12. Test
00001000000000000000000000000000  13. 16 to CI
00000000000001110000000000000000  14. Stop
10000000000000000000000000000000  15. 1
11111111111111111111111111111111  16. -1
00000100000001100000000000000000  17. c to 32
01010000000000000000000000000000  18. 10
```



## Stata

See '''[https://www.stata.com/help.cgi?forvalues forvalues]''' and '''[https://www.stata.com/help.cgi?foreach foreach]''' in Stata help.


```stata
forvalues n=10(-1)0 {
        display `n'
}

forvalues n=10 9 to 0 {
        display `n'
}

foreach n of numlist 10/0 {
        display `n'
}
```



## Swift


```swift
for i in stride(from: 10, through: 0, by: -1) {
  println(i)
}
```

Alternately:

```swift
for i in lazy(0...10).reverse() {
  println(i)
}
```

In Swift 1.2 Alternately:

```swift
for i in reverse(0 ... 10) {
  println(i)
}
```

Alternately (removed in Swift 3):

```swift>for var i = 10; i
= 0; i-- {
  println(i)
}
```

Swift 3:

```swift
for i in (0...10).reversed() {
    print(i)
}
```



## Tcl


```tcl
for {set i 10} {$i >= 0} {incr i -1} {
    puts $i
}
# puts "We have liftoff!"
```


=={{header|TI-83 BASIC}}==


```ti83b

:For(I,10,0,-1
:Disp I
:End

```


=={{header|TI-89 BASIC}}==


```ti89b
Local i
For i, 10, 0, –1
  Disp i
EndFor
```



## Trith


```trith
10 inc iota reverse [print] each
```


```trith
10 [dup print dec] [dup 0 >=] while drop
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
LOOP n=10,0,-1
 PRINT n
ENDLOOP

```



## UnixPipes

{{works with|OpenBSD|4.9}}

```bash
yes '' | cat -n | head -n 11 | while read n; do
	expr $n - 1
done | tail -r
```


This pipe uses several nonstandard commands: <code>cat -n</code> and <code>tail -r</code> might not work with some systems.
If there is no <code>tail -r</code>, try <code>tac</code>.


## UNIX Shell

{{works with|Bourne Shell}}

```bash
i=10
while test $i -ge 0; do
	echo $i
	i=`expr $i - 1`
done

# or

jot - 10 0 -1

# or

seq 10 -1 0
```


----
{{works with|bash}}

```bash
for(( Z=10; Z>=0; Z-- )); do
    echo $Z
done

#or

for Z in {10..0}; do
    echo $Z
done

```



## Ursa


```ursa
decl int i
for (set i 10) (> i -1) (dec i)
	out i endl console
end for
```



## V


```v
10
[0 >]
  [dup puts pred]
while
```



## VBA


```VB
For i = 10 To 0 Step -1
   Debug.Print i
Next i
```


## Vedit macro language


```vedit
for (#1 = 10; #1 >= 0; #1--) {
    Num_Type(#1)
}
```



## Visual Basic .NET


```vbnet
For i = 10 To 0 Step -1
    Console.WriteLine(i)
Next
```



## Wart

<lang>for i 10 (i >= 0) --i
  prn i
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int I;
for I:= 10 downto 0 do
        [IntOut(0, I); CrLf(0)]
```



## zkl


```zkl
foreach n in ([10..0,-1]){ println(n) }
[10..0,-1].apply() //-->L(10,9,8,7,6,5,4,3,2,1,0)
   // tail recursion
fcn(n){ n.println(); if(n==0)return(); return(self.fcn(n-1)) }(10)
```



## ZX Spectrum Basic



```zxbasic
10 FOR l = 10 TO 0 STEP -1
20 PRINT l
30 NEXT l
```

