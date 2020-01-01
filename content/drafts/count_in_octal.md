+++
title = "Count in octal"
description = ""
date = 2019-10-08T05:09:31Z
aliases = []
[extra]
id = 9879
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Radices]]
[[Category:Iteration]]

;Task:
Produce a sequential count in octal,   starting at zero,   and using an increment of a one for each consecutive number.

Each number should appear on a single line,   and the program should count until terminated,   or until the maximum value of the numeric type in use is reached.


;Related task:
*   [[Integer sequence]]   is a similar task without the use of octal numbers.





## 0815


```0815
}:l:>     Start loop, enqueue Z (initially 0).
  }:o:    Treat the queue as a stack and
    <:8:= accumulate the octal digits
    /=>&~ of the current number.
  ^:o:

  <:0:-   Get a sentinel negative 1.
  &>@     Enqueue it between the digits and the current number.
  {       Dequeue the first octal digit.

  }:p:
    ~%={+ Rotate each octal digit into place and print it.
  ^:p:

  <:a:~$  Output a newline.
  <:1:x{+ Dequeue the current number and increment it.
^:l:
```



## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        Octal                     04/07/2016
OCTAL    CSECT
         USING  OCTAL,R13          base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R6,0               i=0
LOOPI    LR     R2,R6              x=i
         LA     R9,10              j=10
         LA     R4,PG+23           @pg
LOOP     LR     R3,R2              save x
         SLL    R2,29              shift left  32-3
         SRL    R2,29              shift right 32-3
         CVD    R2,DW              convert octal(j) to pack decimal
         OI     DW+7,X'0F'         prepare unpack
         UNPK   0(1,R4),DW         packed decimal to zoned printable
         LR     R2,R3              restore x
         SRL    R2,3               shift right 3
         BCTR   R4,0               @pg=@pg-1
         BCT    R9,LOOP            j=j-1
         CVD    R2,DW              binary to pack decimal
         OI     DW+7,X'0F'         prepare unpack
         UNPK   0(1,R4),DW         packed decimal to zoned printable
         CVD    R6,DW              convert i to pack decimal
         MVC    ZN12,EM12          load mask
         ED     ZN12,DW+2          packed decimal (PL6) to char (CL12)
         MVC    PG(12),ZN12        output i
         XPRNT  PG,80              print buffer
         C      R6,=F'2147483647'  if i>2**31-1 (integer max)
         BE     ELOOPI             then exit loop on i
         LA     R6,1(R6)           i=i+1
         B      LOOPI              loop on i
ELOOPI   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
         LTORG
PG       DC     CL80' '            buffer
DW       DS     0D,PL8             15num
ZN12     DS     CL12
EM12     DC     X'40',9X'20',X'2120'  mask CL12 11num
         YREGS
         END    OCTAL
```

{{out}}
<pre style="height:20ex">
           0 00000000000
           1 00000000001
           2 00000000002
           3 00000000003
           4 00000000004
           5 00000000005
           6 00000000006
           7 00000000007
           8 00000000010
           9 00000000011
          10 00000000012
          10 00000000012
          11 00000000013
...
  2147483640 17777777770
  2147483641 17777777771
  2147483642 17777777772
  2147483643 17777777773
  2147483644 17777777774
  2147483645 17777777775
  2147483646 17777777776
  2147483647 17777777777

```



## Ada


```Ada
with Ada.Text_IO;

procedure Octal is
   package IIO is new Ada.Text_IO.Integer_IO(Integer);
begin
   for I in 0 .. Integer'Last loop
      IIO.Put(I, Base => 8);
      Ada.Text_IO.New_Line;
   end loop;
end Octal;
```

First few lines of Output:

```txt
       8#0#
       8#1#
       8#2#
       8#3#
       8#4#
       8#5#
       8#6#
       8#7#
      8#10#
      8#11#
      8#12#
      8#13#
      8#14#
      8#15#
      8#16#
      8#17#
      8#20#
```



## Aime


```aime
integer o;

o = 0;
do {
    o_xinteger(8, o);
    o_byte('\n');
    o += 1;
} while (0 < o);
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #

INT oct width = (bits width-1) OVER 3 + 1;
main:
(
  FOR i TO 17 # max int # DO
    printf(($"8r"8r n(oct width)dl$, BIN i))
  OD
)
```

Output:

```txt

8r00000000001
8r00000000002
8r00000000003
8r00000000004
8r00000000005
8r00000000006
8r00000000007
8r00000000010
8r00000000011
8r00000000012
8r00000000013
8r00000000014
8r00000000015
8r00000000016
8r00000000017
8r00000000020
8r00000000021

```



## ALGOL W

Algol W has built-in hexadecimal and decimal output, this implements octal output.

```algolw
begin
    string(12) r;
    string(8)  octDigits;
    integer    number;
    octDigits := "01234567";
    number    := -1;
    while number < MAXINTEGER do begin
        integer    v, cPos;
        number := number + 1;
        v      := number;
        % build a string of octal digits in r, representing number %
        % Algol W uses 32 bit integers, so r should be big enough  %
        % the most significant digit is on the right               %
        cPos   := 0;
        while begin
            r( cPos // 1 ) := octDigits( v rem 8 // 1 );
            v :=  v div 8;
            ( v > 0 )
        end do begin
            cPos := cPos + 1
        end while_v_gt_0;
        % show most significant digit on a newline %
        write( r( cPos // 1 ) );
        % continue the line with the remaining digits (if any) %
        for c := cPos - 1 step -1 until 0 do writeon( r( c // 1 ) )
    end while_r_lt_MAXINTEGER
end.
```

{{out}}

```txt

0
1
2
3
4
5
6
7
10
11
12
...

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program countoctal.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:        .ascii "Count : "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"


/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program
    mov r4,#0                                     @ loop indice
1:                                                @ begin loop
    mov r0,r4
    ldr r1,iAdrsMessValeur
    bl conversion8                                @ call conversion octal
    ldr r0,iAdrsMessResult
    bl affichageMess                              @ display message
    add r4,#1
    cmp r4,#64
    ble 1b


100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return
/******************************************************************/
/*     Converting a register to octal                             */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion8:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:                                                  @ start loop
    mov r1,r0
    lsr r0,#3                                       @ / by 8
    sub r1,r0,lsl #3                                @ compute remainder r1 - (r0 * 8)
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                      @ and move spaces in end on area
    mov r0,r4                                         @ result length
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                    @ restaur registres
    bx lr                                             @return


```


## AutoHotkey


```AHK
DllCall("AllocConsole")
Octal(int){
	While int
		out := Mod(int, 8) . out, int := int//8
	return out
}
Loop
{
	FileAppend, % Octal(A_Index) "`n", CONOUT$
	Sleep 200
}
```


## AWK

The awk extraction and reporting language uses the underlying C library to provide support for the printf command. This enables us to use that function to output the counter value as octal:


```awk
BEGIN {
  for (l = 0; l <= 2147483647; l++) {
    printf("%o\n", l);
  }
}
```



## BASIC


Some BASICs provide a built-in function to convert a number to octal, typically called <code>OCT$</code>.

{{works with|QBasic}}


```qbasic
DIM n AS LONG
FOR n = 0 TO &h7FFFFFFF
    PRINT OCT$(n)
NEXT
```


However, many do not. For those BASICs, we need to write our own function.

{{works with|Chipmunk Basic}}


```qbasic
WHILE ("" = INKEY$)
    PRINT Octal$(n)
    n = n + 1
WEND
END
FUNCTION Octal$(what)
    outp$ = ""
    w = what
    WHILE ABS(w) > 0
        o = w AND 7
        w = INT(w / 8)
        outp$ = STR$(o) + outp$
    WEND
    Octal$ = outp$
END FUNCTION
```


See also: [[#BBC BASIC|BBC BASIC]], [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]], [[#Run BASIC|Run BASIC]]

=
## Applesoft BASIC
=

```ApplesoftBasic
10 N$ = "0"

100 O$ = N$
110 PRINT O$
120 N$ = ""
130 C = 1
140 FOR I = LEN(O$) TO 1 STEP -1
150     N = VAL(MID$(O$, I, 1)) + C
160     C = N >= 8
170     N$ = STR$(N - C * 8) + N$
180 NEXT I
190 IF C THEN N$ = "1" + N$
200 GOTO 100
```


=
## Sinclair ZX81 BASIC
=
The octal number is stored and manipulated as a string, meaning that even with only 1k of RAM the program shouldn't stop until the number gets to a couple of hundred digits long. I have <i>not</i> left it running long enough to find out exactly when it does run out of memory. The <code>SCROLL</code> statement is necessary: the ZX81 halts when the screen is full unless it is positively told to scroll instead.

```basic
 10 LET N$="0"
 20 SCROLL
 30 PRINT N$
 40 LET L=LEN N$
 50 LET N=VAL N$(L)+1
 60 IF N=8 THEN GOTO 90
 70 LET N$(L)=STR$ N
 80 GOTO 20
 90 LET N$(L)="0"
100 IF L=1 THEN GOTO 130
110 LET L=L-1
120 GOTO 50
130 LET N$="1"+N$
140 GOTO 20
```



## Batch File


```dos

@echo off
:: {CTRL + C} to exit the batch file

:: Send incrementing decimal values to the :to_Oct function
set loop=0
:loop1
call:to_Oct %loop%
set /a loop+=1
goto loop1

:: Convert the decimal values parsed [%1] to octal and output them on a new line
:to_Oct
set todivide=%1
set "fulloct="

:loop2
set tomod=%todivide%
set /a appendmod=%tomod% %% 8
set fulloct=%appendmod%%fulloct%
if %todivide% lss 8 (
  echo %fulloct%
  exit /b
)
set /a todivide/=8
goto loop2

```

{{out}}

```txt

0
1
2
3
4
5
6
7
10
...

```



## BBC BASIC

Terminate by pressing ESCape.

```bbcbasic
      N% = 0
      REPEAT
        PRINT FN_tobase(N%, 8, 0)
        N% += 1
      UNTIL FALSE
      END

      REM Convert N% to string in base B% with minimum M% digits:
      DEF FN_tobase(N%, B%, M%)
      LOCAL D%, A$
      REPEAT
        D% = N% MOD B%
        N% DIV= B%
        IF D%<0 D% += B% : N% -= 1
        A$ = CHR$(48 + D% - 7*(D%>9)) + A$
        M% -= 1
      UNTIL (N%=FALSE OR N%=TRUE) AND M%<=0
      =A$

```



## bc


```bc
obase = 8			/* Output base is octal. */
for (num = 0; 1; num++) num	/* Loop forever, printing counter. */
```


The loop never stops at a maximum value, because bc uses [[arbitrary-precision integers (included)|arbitrary-precision integers]].


## Befunge

This is almost identical to the [[Binary digits#Befunge|Binary digits]] sample, except for the change of base and the source coming from a loop rather than a single input.

```befunge
:0\55+\:8%68>*#<+#8\#68#%/#8:_$>:#,_$1+:0`!#@_
```



## Bracmat

Stops when the user presses Ctrl-C or when the stack overflows. The solution is not elegant, and so is octal counting.

```bracmat

  ( oct
  =
    .     !arg:<8
        & (!arg:~<0|ERROR)
      | str$(oct$(div$(!arg.8)) mod$(!arg.8))
  )
& -1:?n
& whl'(1+!n:?n&out$(!n oct$!n));

```


=={{header|Brainfuck}}==


```bf
+[            Start with n=1 to kick off the loop
[>>++++++++<< Set up {n 0 8} for divmod magic
[->+>-        Then
[>+>>]>       do
[+[-<+>]>+>>] the
<<<<<<]       magic
>>>+          Increment n % 8 so that 0s don't break things
>]            Move into n / 8 and divmod that unless it's 0
-<            Set up sentinel ‑1 then move into the first octal digit
[++++++++ ++++++++ ++++++++ Add 47 to get it to ASCII
 ++++++++ ++++++++ +++++++. and print it
[<]<]         Get to a 0; the cell to the left is the next octal digit
>>[<+>-]      Tape is {0 n}; make it {n 0}
>[>+]         Get to the ‑1
<[[-]<]       Zero the tape for the next iteration
++++++++++.   Print a newline
[-]<+]        Zero it then increment n and go again
```



## C


```c
#include <stdio.h>

int main()
{
        unsigned int i = 0;
        do { printf("%o\n", i++); } while(i);
        return 0;
}
```

## C#

```c#
using System;

class Program
{
    static void Main()
    {
        var number = 0;
        do
        {
            Console.WriteLine(Convert.ToString(number, 8));
        } while (++number > 0);
    }
}
```


## C++

This prevents an infinite loop by counting until the counter overflows and produces a 0 again. This could also be done with a for or while loop, but you'd have to print 0 (or the last number) outside the loop.


```cpp
#include <iostream>

int main()
{
  unsigned i = 0;
  do
  {
    std::cout << std::oct << i << std::endl;
    ++i;
  } while(i != 0);
  return 0;
}
```



## Clojure


```clojure
(doseq [i (range)] (println (format "%o" i)))
```



## COBOL

{{trans|Delphi}}
{{works with|GNU Cobol|2.0}}

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. count-in-octal.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION dec-to-oct
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  i                                   PIC 9(18).

PROCEDURE DIVISION.
    PERFORM VARYING i FROM 1 BY 1 UNTIL i = 0
        DISPLAY FUNCTION dec-to-oct(i)
    END-PERFORM
    .
END PROGRAM count-in-octal.


IDENTIFICATION DIVISION.
FUNCTION-ID. dec-to-oct.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  rem                                 PIC 9.

01  dec                                 PIC 9(18).

LINKAGE SECTION.
01  dec-arg                             PIC 9(18).

01  oct                                 PIC 9(18).

PROCEDURE DIVISION USING dec-arg RETURNING oct.
    MOVE dec-arg TO dec *> Copy is made to avoid modifying reference arg.
    PERFORM WITH TEST AFTER UNTIL dec = 0
        MOVE FUNCTION REM(dec, 8) TO rem
        STRING rem, oct DELIMITED BY SPACES INTO oct
        DIVIDE 8 INTO dec
    END-PERFORM
    .
END FUNCTION dec-to-oct.
```



## CoffeeScript


```coffeescript

n = 0

while true
  console.log n.toString(8)
  n += 1

```



## Common Lisp


```lisp
(loop for i from 0 do (format t "~o~%" i))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE CountOctal;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	i: INTEGER;
	resp: ARRAY 32 OF CHAR;
BEGIN
	FOR i := 0 TO 1000 DO
		Strings.IntToStringForm(i,8,12,' ',TRUE,resp);
		StdLog.String(resp);StdLog.Ln
	END
END Do;
END CountOctal.


```

Execute: ^Q CountOctal.Do<br/>
Output:

```txt

         0%8
         1%8
         2%8
         3%8
         4%8
         5%8
         6%8
         7%8
        10%8
        11%8
        12%8
        13%8
        14%8
        15%8
        16%8
        17%8
        20%8
        21%8
        22%8

```



## Crystal


```ruby
# version 0.21.1
# using unsigned 8 bit integer, range 0 to 255

(0_u8..255_u8).each { |i| puts i.to_s(8) }
```


{{out}}

```txt

0
1
2
3
4
5
6
7
10
11
12
...
374
375
376
377

```



## D


```d
void main() {
    import std.stdio;

    ubyte i;
    do writefln("%o", i++);
    while(i);
}
```



## Dc


###  Named Macro

A simple infinite loop and octal output will do.

```Dc
8o0[p1+lpx]dspx
```



###  Anonymous Macro

Needs <code>r</code> (swap TOS and NOS):

```Dc
8 o 0 [ r p 1 + r dx ] dx
```

Pushing/poping TOS to a named stack can be used instead of swaping:

```Dc
8 o 0 [ S@ p 1 + L@ dx ] dx
```



## DCL


```DCL
$ i = 0
$ loop:
$  write sys$output f$fao( "!OL", i )
$  i = i + 1
$  goto loop
```

{{out}}

```txt
00000000000
00000000001
00000000002
...
17777777777
20000000000
20000000001
...
37777777777
00000000000
00000000001
...
```



## Delphi


```Delphi
program CountingInOctal;

{$APPTYPE CONSOLE}

uses SysUtils;

function DecToOct(aValue: Integer): string;
var
  lRemainder: Integer;
begin
  Result := '';
  repeat
    lRemainder := aValue mod 8;
    Result := IntToStr(lRemainder) + Result;
    aValue := aValue div 8;
  until aValue = 0;
end;

var
  i: Integer;
begin
  for i := 0 to 20 do
    WriteLn(DecToOct(i));
end.
```



## Elixir


```elixir
Stream.iterate(0,&(&1+1)) |> Enum.each(&IO.puts Integer.to_string(&1,8))
```

or

```elixir
Stream.unfold(0, fn n ->
  IO.puts Integer.to_string(n,8)
  {n,n+1}
end) |> Stream.run
```

or

```elixir
f = fn ff,i -> :io.fwrite "~.8b~n", [i]; ff.(ff, i+1) end
f.(f, 0)
```



## Emacs Lisp

Displays in the message area interactively, or to standard output under <code>-batch</code>.


```lisp
(dotimes (i most-positive-fixnum) ;; starting from 0
  (message "%o" i))
```



## Erlang

The fun is copied from [[Integer sequence#Erlang]]. I changed the display format.

```Erlang

F = fun(FF, I) -> io:fwrite("~.8B~n", [I]), FF(FF, I + 1) end.

```

Use like this:

```txt

F( F, 0 ).

```



## Euphoria


```euphoria
integer i
i = 0
while 1 do
    printf(1,"%o\n",i)
    i += 1
end while
```


Output:

```txt
...
6326
6327
6330
6331
6332
6333
6334
6335
6336
6337

```


=={{header|F Sharp|F#}}==

```fsharp
let rec countInOctal num : unit =
  printfn "%o" num
  countInOctal (num + 1)

countInOctal 1
```



## Factor


```factor
USING: kernel math prettyprint ;
0 [ dup .o 1 + t ] loop
```



## Forth

Using INTS from [[Integer sequence#Forth]]

```forth
: octal ( -- )  8 base ! ;  \ where unavailable

octal ints
```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program Octal
  implicit none

  integer, parameter :: i64 = selected_int_kind(18)
  integer(i64) :: n = 0

! Will stop when n overflows from
! 9223372036854775807 to -92233720368547758078 (1000000000000000000000 octal)
  do while(n >= 0)
    write(*, "(o0)") n
    n = n + 1
  end do
end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim ub As UByte = 0 ' only has a range of 0 to 255
Do
   Print Oct(ub, 3)
   ub += 1
Loop Until ub = 0 ' wraps around to 0 when reaches 256
Print
Print "Press any key to quit"
Sleep
```



## Futhark


Futhark cannot print.  Instead we produce an array of integers that
look like octal numbers when printed in decimal.


```Futhark

fun octal(x: int): int =
  loop ((out,mult,x) = (0,1,x)) = while x > 0 do
    let digit = x % 8
    let out = out + digit * mult
    in (out, mult * 10, x / 8)
  in out

fun main(n: int): [n]int =
  map octal (iota n)

```



## FutureBasic


```futurebasic

include "ConsoleWindow
defstr word

dim as short i

for i = &o000000 to &o000031      // 0 to 25 in decimal
   print oct$(i); " in octal ="; i
next

```


Output:

```txt

000000 in octal = 0
000001 in octal = 1
000002 in octal = 2
000003 in octal = 3
000004 in octal = 4
000005 in octal = 5
000006 in octal = 6
000007 in octal = 7
000010 in octal = 8
000011 in octal = 9
000012 in octal = 10
000013 in octal = 11
000014 in octal = 12
000015 in octal = 13
000016 in octal = 14
000017 in octal = 15
000020 in octal = 16
000021 in octal = 17
000022 in octal = 18
000023 in octal = 19
000024 in octal = 20
000025 in octal = 21
000026 in octal = 22
000027 in octal = 23
000030 in octal = 24
000031 in octal = 25

```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    for i := int8(0); ; i++ {
        fmt.Printf("%o\n", i)
        if i == math.MaxInt8 {
            break
        }
    }
}
```

Output:

```txt

0
1
2
3
4
5
6
7
10
11
12
...
175
176
177

```

Note that to use a different integer type, code must be changed in two places.  Go has no way to query a type for its maximum value.  Example:

```go
func main() {
    for i := uint16(0); ; i++ {  // type specified here
        fmt.Printf("%o\n", i)
        if i == math.MaxUint16 { // maximum value for type specified here
            break
        }
    }
}
```

Output:

```txt

...
177775
177776
177777

```

Note also that if floating point types are used for the counter, loss of precision will prevent the program from from ever reaching the maximum value.  If you stretch interpretation of the task wording "maximum value" to mean "maximum value of contiguous integers" then the following will work:

```go
import "fmt"

func main() {
    for i := 0.; ; {
        fmt.Printf("%o\n", int64(i))
        /* uncomment to produce example output
        if i == 3 {
            i = float64(1<<53 - 4) // skip to near the end
            fmt.Println("...")
        } */
        next := i + 1
        if next == i {
            break
        }
        i = next
    }
}
```

Output, with skip uncommented:

```txt

0
1
2
3
...
377777777777777775
377777777777777776
377777777777777777
400000000000000000

```

Big integers have no maximum value, but the Go runtime will panic when memory allocation fails.  The deferred recover here allows the program to terminate silently should the program run until this happens.

```go
import (
    "big"
    "fmt"
)

func main() {
    defer func() {
        recover()
    }()
    one := big.NewInt(1)
    for i := big.NewInt(0); ; i.Add(i, one) {
        fmt.Printf("%o\n", i)
    }
}
```

Output:

```txt

0
1
2
3
4
5
6
7
10
11
12
13
14
...

```



## Groovy

Size-limited solution:

```groovy
println 'decimal  octal'
for (def i = 0; i <= Integer.MAX_VALUE; i++) {
    printf ('%7d  %#5o\n', i, i)
}
```


Unbounded solution:

```groovy
println 'decimal  octal'
for (def i = 0g; true; i += 1g) {
    printf ('%7d  %#5o\n', i, i)
}
```


Output:

```txt
decimal  octal
      0     00
      1     01
      2     02
      3     03
      4     04
      5     05
      6     06
      7     07
      8    010
      9    011
     10    012
     11    013
     12    014
     13    015
     14    016
     15    017
     16    020
     17    021
...
```



## Haskell


```haskell
import Numeric

main = mapM_ (putStrLn . flip showOct "") [1..]
```


=={{header|Icon}} and {{header|Unicon}}==

```unicon
link convert   # To get exbase10 method

procedure main()
    limit := 8r37777777777
    every write(exbase10(seq(0)\limit, 8))
end
```



## J

'''Solution:'''

```J
   disp=.([smoutput) ' '(-.~":)8&#.inv
   (1+disp)^:_]0x
```


The full result is not displayable, by design.  This could be considered a bug, but is an essential part of this task.  Here's how it starts:


```j
   (1+disp)^:_]0x
0
1
2
3
4
5
6
7
10
11
...
```


The important part of this code is 8&#.inv which converts numbers from internal representation to a sequence of base 8 digits. (We then convert this sequence to characters and remove the delimiting spaces - this gives us the octal values we want to display.)

So then we define disp as a word which displays its argument in octal and returns its argument as its result (unchanged).

Finally, the <code>^:_</code> clause tells J to repeat this function forever, with <code>(1+disp)</code>adding 1 to the result each time it is displayed (or at least tha clause tells J to keep repeating that operation until it gives the same value back twice in a row - which won't happen - or to stop when the machine stops - like if the power is turned off - or if J is shut down - or...).

We use arbitrary precision numbers, not because there's any likelihood that fixed width numbers would ever overflow, but just to emphasize that this thing is going to have to be shut down by some mechanism outside the program.


## Java


```java
public class Count{
    public static void main(String[] args){
        for(int i = 0;i >= 0;i++){
            System.out.println(Integer.toOctalString(i)); //optionally use "Integer.toString(i, 8)"
        }
    }
}
```



## JavaScript


```javascript
for (var n = 0; n < 1e14; n++) { // arbitrary limit that's not too big
    document.writeln(n.toString(8)); // not sure what's the best way to output it in JavaScript
}
```



## Julia


```Julia

for i in one(Int64):typemax(Int64)
    print(oct(i), " ")
    sleep(0.1)
end

```

I slowed the loop down with a <code>sleep</code> to make it possible to see the result without being swamped.

{{out}}

```txt

1 2 3 4 5 6 7 10 11 12 13 14 15 16 17 20 21 22 23 24 25 26 27 30 31 32 33 34 35 36 ^C

```



## Kotlin


```scala
//  version 1.1

//  counts up to 177 octal i.e. 127 decimal
fun main(args: Array<String>) {
    (0..Byte.MAX_VALUE).forEach { println("%03o".format(it)) }
}
```


{{out}}
First ten lines:

```txt

000
001
002
003
004
005
006
007
010
011

```



## LabVIEW

LabVIEW contains a Number to Octal String function. The following image shows the front panel and block diagram.<br/>
[[file:Count_in_octal.png]]


## Lang5


```lang5
'%4o '__number_format set
0 do dup 1 compress . "\n" . 1 + loop
```



## LFE


```lisp
(: lists foreach
  (lambda (x)
    (: io format '"~p~n" (list (: erlang integer_to_list x 8))))
  (: lists seq 0 2000))

```



## Liberty BASIC

Terminate these ( essentially, practically) infinite loops by hitting <CTRL<BRK>

```lb

    'the method used here uses the base-conversion from RC Non-decimal radices/Convert
    'to terminate hit <CTRL<BRK>

    global      alphanum$
    alphanum$   ="01234567"

    i =0

    while 1
        print toBase$( 8, i)
        i =i +1
    wend

    end

    function toBase$( base, number) '   Convert decimal variable to number string.
        maxIntegerBitSize   =len( str$( number))
        toBase$             =""
        for i =10 to 1 step -1
            remainder   =number mod base
            toBase$     =mid$( alphanum$, remainder +1, 1) +toBase$
            number      =int( number /base)
            if number <1 then exit for
        next i
        toBase$ =right$( "             " +toBase$, 10)
    end function

```

As suggested in LOGO, it is easy to work on a string representation too.

```lb

 op$ = "00000000000000000000"
L   =len( op$)

while 1
    started =0

    for i =1 to L
        m$ =mid$( op$, i, 1)
        if started =0 and m$ ="0" then print " "; else print m$;: started =1
    next i
    print

    for i =L to 1 step -1
        p$ =mid$( op$, i, 1)
        if p$ =" " then v =0 else v =val( p$)
        incDigit  = v +carry
        if i =L then incDigit =incDigit +1
        if incDigit >=8 then
            replDigit =incDigit -8
            carry     =1
        else
            replDigit =incDigit
            carry     =0
        end if
        op$ =left$( op$, i -1) +chr$( 48 +replDigit) +right$( op$, L -i)
    next i

wend

end

```

Or use a recursive listing of permutations with the exception that the first digit is not 0 (unless listing single-digit numbers). For each digit-place, list numbers with 0-7 in the next digit-place.

```lb

 i = 0
while 1
    call CountOctal 0, i, i > 0
    i = i + 1
wend

sub CountOctal value, depth, startValue
    value = value * 10
    for i = startValue to 7
        if depth > 0 then
            call CountOctal value + i, depth - 1, 0
        else
            print value + i
        end if
    next i
end sub

```



## Logo

No built-in octal-formatting, so it's probably more efficient to just manually increment a string than to increment a number and then convert the whole thing to octal every time we print.  This also lets us keep counting as long as we have room for the string.


```logo
to increment_octal :n
  ifelse [empty? :n] [
    output 1
  ] [
    local "last
    make "last last :n
    local "butlast
    make "butlast butlast :n
    make "last sum :last 1
    ifelse [:last < 8] [
      output word :butlast :last
    ] [
      output word (increment_octal :butlast) 0
    ]
  ]
end

make "oct 0
while ["true] [
  print :oct
  make "oct increment_octal :oct
]
```



## LOLCODE

LOLCODE has no conception of octal numbers, but we can use string concatenation (<tt>SMOOSH</tt>) and basic arithmetic to accomplish the task.

```LOLCODE
HAI 1.3

HOW IZ I octal YR num
    I HAS A digit, I HAS A oct ITZ ""
    IM IN YR octalizer
        digit R MOD OF num AN 8
        oct R SMOOSH digit oct MKAY
        num R QUOSHUNT OF num AN 8
        NOT num, O RLY?
            YA RLY, FOUND YR oct
        OIC
    IM OUTTA YR octalizer
IF U SAY SO

IM IN YR printer UPPIN YR num
    VISIBLE I IZ octal YR num MKAY
IM OUTTA YR printer

KTHXBYE
```



## Lua



```lua
for l=1,2147483647 do
  print(string.format("%o",l))
end
```



## M4



```M4
define(`forever',
   `ifelse($#,0,``$0'',
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',eval($2+$3),$3,`$4')')')dnl
forever(`y',0,1, `eval(y,8)
')
```



## Maple


```Maple

octcount := proc (n)
 seq(printf("%a \n", convert(i, octal)), i = 1 .. n);
 end proc;

```



## Mathematica



```Mathematica
x=0;
While[True,Print[BaseForm[x,8];x++]
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
    n = 0;
    while (1)
        dec2base(n,8)
        n = n+1;
    end;
```

Or use printf:

```Matlab
    n = 0;
    while (1)
        printf('%o\n',n);
        n = n+1;
    end;
```


If a predefined sequence should be displayed, one can use

```Matlab
    seq = 1:100;
    dec2base(seq,8)
```

or

```Matlab
    printf('%o\n',seq);
```



## Mercury

<lang>
:- module count_in_octal.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    count_in_octal(0, !IO).

:- pred count_in_octal(int::in, io::di, io::uo) is det.

count_in_octal(N, !IO) :-
    io.format("%o\n", [i(N)], !IO),
    count_in_octal(N + 1, !IO).

```



## min

{{works with|min|0.19.3}}
min has no support for octal or base conversion (it is a minimalistic language, after all) so we need to do that ourselves.

```min
(
  (dup 0 ==) (pop () 0 shorten)
  (((8 mod) (8 div)) cleave) 'cons linrec
  reverse 'print! foreach newline
) :octal

0 (dup octal succ)
9.223e18 int times ; close to max int value
```


=={{header|МК-61/52}}==
<lang>ИП0	П1	1	0	/	[x]	П1	Вx	{x}	1
0	*	7	-	x=0	21	ИП1	x#0	28	БП
02	ИП0	1	+	П0	С/П	БП	00	ИП0	lg
[x]	1	+	10^x	П0	С/П	БП	00
```


=={{header|Modula-2}}==

```modula2
MODULE octal;

IMPORT  InOut;

VAR     num             : CARDINAL;

BEGIN
  num := 0;
  REPEAT
    InOut.WriteOct (num, 12);           InOut.WriteLn;
    INC (num)
  UNTIL num = 0
END octal.
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.math.BigInteger

-- allow an option to change the output radix.
parse arg radix .
if radix.length() == 0 then radix = 8
k_ = BigInteger
k_ = BigInteger.ZERO

loop forever
  say k_.toString(int radix)
  k_ = k_.add(BigInteger.ONE)
  end

```



## NewLISP


```NewLISP
; file:   ocount.lsp
; url:    http://rosettacode.org/wiki/Count_in_octal
; author: oofoe 2012-01-29

; Although NewLISP itself uses a 64-bit integer representation, the
; format function relies on underlying C library's printf function,
; which can only handle a 32-bit octal number on this implementation.

(for (i 0 (pow 2 32)) (println (format "%o" i)))

(exit)
```


Sample output:


```txt
0
1
2
3
4
5
6
7
10
11
12
...

```



## Nim


```nim
import strutils
for i in 0 ..< int.high:
  echo toOct(i, 16)
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE CountInOctal;
IMPORT
  NPCT:Tools,
  Out := NPCT:Console;
VAR
  i: INTEGER;

BEGIN
  FOR i := 0 TO MAX(INTEGER) DO;
    Out.String(Tools.IntToOct(i));Out.Ln
  END
END CountInOctal.

```

{{out}}

```txt

00000000000
00000000001
00000000002
00000000003
00000000004
00000000005
00000000006
00000000007
00000000010
00000000011
00000000012
00000000013
00000000014
00000000015
00000000016
00000000017
00000000020
00000000021
...
00000077757
00000077760
00000077761
00000077762
00000077763
00000077764
00000077765
00000077766
00000077767
00000077770
00000077771
00000077772
00000077773
00000077774
00000077775
00000077776
00000077777


```



## OCaml



```ocaml
let () =
  for i = 0 to max_int do
    Printf.printf "%o\n" i
  done
```


{{out}}

```txt

0
1
2
3
4
5
6
7
10
11
12
...
7777777775
7777777776
7777777777

```



## PARI/GP

Both versions will count essentially forever; the universe will succumb to [[wp:Proton decay|proton decay]] long before the counter rolls over even in the 32-bit version.

Manual:

```parigp
oct(n)=n=binary(n);if(#n%3,n=concat([[0,0],[0]][#n%3],n));forstep(i=1,#n,3,print1(4*n[i]+2*n[i+1]+n[i+2]));print;
n=0;while(1,oct(n);n++)
```


Automatic:
{{works with|PARI/GP|2.4.3 and above}}

```parigp
n=0;while(1,printf("%o\n",n);n++)
```



## Pascal

See [[Count_in_octal#Delphi | Delphi]] or {{works with|Free Pascal}}
old string incrementer for Turbo Pascal transformed, same as in http://rosettacode.org/wiki/Count_in_octal#Logo, about 100x times faster than Dephi-Version, with the abilty to used preformated strings leading zeroes.
Added a Bit fiddling Version IntToOctString, nearly as fast.

```pascal
program StrAdd;
{$Mode Delphi}
{$Optimization ON}
uses
  sysutils;//IntToStr

const
  maxCntOct = (SizeOf(NativeUint)*8+(3-1)) DIV 3;

procedure IntToOctString(i: NativeUint;var res:Ansistring);
var
  p : array[0..maxCntOct] of byte;
  c,cnt: LongInt;
begin
  cnt := maxCntOct;
  repeat
    c := i AND 7;
    p[cnt] := (c+Ord('0'));
    dec(cnt);
    i := i shr 3;
  until (i = 0);
  i := cnt+1;
  cnt := maxCntOct-cnt;
  //most time consuming with Ansistring
  //call fpc_ansistr_unique
  setlength(res,cnt);
  move(p[i],res[1],cnt);
end;

procedure IncStr(var s:String;base:NativeInt);
var
  le,c,dg:nativeInt;
begin
  le := length(s);
  IF le = 0 then
  Begin
    s := '1';
    EXIT;
  end;

  repeat
    dg := ord(s[le])-ord('0') +1;
    c  := ord(dg>=base);
    dg := dg-(base AND (-c));
    s[le] := chr(dg+ord('0'));
    dec(le);
  until (c = 0) or (le<=0);

  if (c = 1) then
  begin
    le := length(s);
    setlength(s,le+1);
    move(s[1],s[2],le);
    s[1] := '1';
  end;
end;

const
  MAX = 8*8*8*8*8*8*8*8*8;//8^9
var
  sOct,
  s  : AnsiString;
  i : nativeInt;
  T1,T0: TDateTime;
Begin
  sOct := '';
  For i := 1 to 16 do
  Begin
    IncStr(sOct,8);
    writeln(i:10,sOct:10);
  end;
  writeln;

  For i := 1 to 16 do
  Begin
    IntToOctString(i,s);
    writeln(i:10,s:10);
  end;

  sOct := '';
  T0 := time;
  For i := 1 to MAX do
    IncStr(sOct,8);
  T0 := (time-T0)*86400;
  writeln(sOct);

  T1 := time;
  For i := 1 to MAX do
    IntToOctString(i,s);
  T1 := (time-T1)*86400;
  writeln(s);
  writeln;
  writeln(MAX);
  writeln('IncStr         ',T0:8:3);
  writeln('IntToOctString ',T1:8:3);
end.

```

{{out}}

```txt
         1         1
         2         2
         3         3
         4         4
         5         5
         6         6
         7         7
         8        10
         9        11
        10        12
        11        13
        12        14
        13        15
        14        16
        15        17
        16        20

         1         1
         2         2
         3         3
         4         4
         5         5
         6         6
         7         7
         8        10
         9        11
        10        12
        11        13
        12        14
        13        15
        14        16
        15        17
        16        20

1000000000
1000000000

134217728
IncStr            0.944 secs
IntToOctString    2.218 secs
```



## Perl

Since task says "system register", I take it to mean "no larger than machine native integer limit":

```perl
use POSIX;
printf "%o\n", $_ for (0 .. POSIX::UINT_MAX);
```

Otherwise:

```perl
use bigint;
my $i = 0;
printf "%o\n", $i++ while 1
```



## Perl 6


```perl6
say .base(8) for ^Inf;
```

{{out}}

```txt
0
```

Here we arbitrarily show as many lines of output as there are lines in the program. <tt>:-)</tt>


## Phix


```Phix
integer i = 0
constant ESC = #1B
while not find(get_key(),{ESC,'q','Q'}) do
    printf(1,"%o\n",i)
    i += 1
end while

```



## PHP


```php
<?php
for ($n = 0; is_int($n); $n++) {
  echo decoct($n), "\n";
}
?>
```



## PicoLisp


```PicoLisp
(for (N 0  T  (inc N))
   (prinl (oct N)) )
```



## PL/I

Version 1:

```pli
/* Do the actual counting in octal. */
count: procedure options (main);
   declare v(5) fixed(1) static initial ((5)0);
   declare (i, k) fixed;

   do k = 1 to 999;
      call inc;
      put skip edit ( (v(i) do i = 1 to 5) ) (f(1));
   end;

inc: proc;
   declare (carry, i) fixed binary;

   carry = 1;
   do i = 5 to 1 by -1;
      v(i) = v(i) + carry;
      if v(i) > 7 then
         do; v(i) = v(i) - 8; if i = 1 then stop; carry = 1; end;
      else
         carry = 0;
   end;
end inc;

end count;
```

Version 2:

```pli
count: procedure options (main); /* 12 Jan. 2014 */
   declare (i, j) fixed binary;

   do i = 0 upthru 2147483647;
      do j = 30 to 0 by -3;
         put edit (iand(isrl(i, j), 7) ) (f(1));
      end;
      put skip;
   end;

end count;
```


{{out}}

```txt
(End of) Output of version 1
00000001173
00000001174
00000001175
00000001176
00000001177
00000001200
00000001201
00000001202
00000001203
00000001204
00000001205
00000001206
00000001207
00000001210
00000001211
00000001212
00000001213
00000001214
00000001215
00000001216

```



## PowerShell


```PowerShell
[int64]$i = 0
While ( $True )
    {
    [Convert]::ToString( ++$i, 8 )
    }
```




## Prolog

Rather than just printing out a list of octal numbers, this code will generate a sequence.
octal/1 can also be used to tell if a number is a valid octal number or not.
octalize will keep producing and printing octal number, there is no limit.


```Prolog
o(O) :- member(O, [0,1,2,3,4,5,6,7]).

octal([O]) :- o(O).
octal([A|B]) :-
	octal(O),
	o(T),
	append(O, [T], [A|B]),
	dif(A, 0).

octalize :-
	forall(
		octal(X),
		(maplist(write, X), nl)
	).
```



## PureBasic


```PureBasic
Procedure.s octal(n.q)
  Static Dim digits(20)
  Protected i, j, result.s
  For i = 0 To 20
    digits(i) = n % 8
    n / 8
    If n < 1
      For j = i To 0 Step -1
        result + Str(digits(j))
      Next
      Break
    EndIf
  Next

  ProcedureReturn result
EndProcedure

Define n.q
If OpenConsole()
  While n >= 0
    PrintN(octal(n))
    n + 1
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf

```

Sample output:

```txt
0
1
2
3
4
5
6
7
10
11
12
...
777777777777777777767
777777777777777777770
777777777777777777771
777777777777777777772
777777777777777777773
777777777777777777774
777777777777777777775
777777777777777777776
777777777777777777777
```



## Python


```Python
import sys
for n in xrange(sys.maxint):
    print oct(n)
```



## Racket


```racket

#lang racket
(for ([i (in-naturals)])
  (displayln (number->string i 8)))

```

(Racket has bignums, so this loop will never end.)


## Retro

Integers in Retro are signed.


```Retro
octal
17777777777 [ putn cr ] iter
```



## REXX

If this REXX program wouldn't be stopped, it would count ''forever''.

```rexx
/*REXX program counts in octal until the number exceeds #pgm statements.*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ Count all the protons  (and electrons!)  in the universe.          │
  │                                                                    │
  │ According to Sir Arthur Eddington in 1938 at his Tamer Lecture at  │
  │ Trinity College (Cambridge), he postulated that there are exactly  │
  │                                                                    │
  │                              136 ∙ 2^256                           │
  │                                                                    │
  │ protons in the universe,  and the same number of electrons,  which │
  │ is equal to around  1.57477e+79.                                   │
  │                                                                    │
  │ [Although, a modern estimate is around  10^80.]                    │
  └────────────────────────────────────────────────────────────────────┘*/
numeric digits 100000                  /*handle almost all big numbers. */
numIn=right('number in', 20)           /*used for indentation of output.*/
w=length(sourceline())                 /*used for formatting width of #s*/

  do #=0  to 136 * (2**256)            /*Sir Eddington, here we come !  */
  !=x2b( d2x(#) )
  _=right(!,  3 * (length(_) % 3 + 1),  0)
  o=
                do k=1  to length(_)  by 3
                o=o'0'substr(_,k,3)
                end   /*k*/

  say numIn 'base ten = ' right(#,w) numIn  "octal = " right(b2x(o)+0,w+w)
  if #>sourceline()  then leave        /*stop if #protons>pgm statements*/
  end   /*#*/
                                       /*stick a fork in it, we're done.*/
```

'''output'''
<pre style="height:30ex">
           number in base ten =   0            number in octal =     0
           number in base ten =   1            number in octal =     1
           number in base ten =   2            number in octal =     2
           number in base ten =   3            number in octal =     3
           number in base ten =   4            number in octal =     4
           number in base ten =   5            number in octal =     5
           number in base ten =   6            number in octal =     6
           number in base ten =   7            number in octal =     7
           number in base ten =   8            number in octal =    10
           number in base ten =   9            number in octal =    11
           number in base ten =  10            number in octal =    12
           number in base ten =  11            number in octal =    13
           number in base ten =  12            number in octal =    14
           number in base ten =  13            number in octal =    15
           number in base ten =  14            number in octal =    16
           number in base ten =  15            number in octal =    17
           number in base ten =  16            number in octal =    20
           number in base ten =  17            number in octal =    21
           number in base ten =  18            number in octal =    22
           number in base ten =  19            number in octal =    23
           number in base ten =  20            number in octal =    24
           number in base ten =  21            number in octal =    25
           number in base ten =  22            number in octal =    26
           number in base ten =  23            number in octal =    27
           number in base ten =  24            number in octal =    30
           number in base ten =  25            number in octal =    31
           number in base ten =  26            number in octal =    32
           number in base ten =  27            number in octal =    33
           number in base ten =  28            number in octal =    34
           number in base ten =  29            number in octal =    35
           number in base ten =  30            number in octal =    36
           number in base ten =  31            number in octal =    37

```



## Ring


```ring

size = 30
for n = 1 to size
    see octal(n) + nl
next

func octal m
     output = ""
     w = m
     while fabs(w) > 0
           oct = w & 7
           w = floor(w / 8)
           output = string(oct) + output
     end
     return output

```



## Ruby

From the [http://www.ruby-doc.org/core/Fixnum.html documentation]: "A Fixnum holds Integer values that can be represented in a native machine word (minus 1 bit). If any operation on a Fixnum exceeds this range, the value is automatically converted to a Bignum."


```ruby
n = 0
loop do
  puts "%o" % n
  n += 1
end

# or
for n in 0..Float::INFINITY
  puts n.to_s(8)
end

# or
0.upto(1/0.0) do |n|
  printf "%o\n", n
end

# version 2.1 later
0.step do |n|
  puts format("%o", n)
end
```



## Run BASIC


```runbasic
input "Begin number:";b
input "  End number:";e

for i = b to e
  print i;" ";toBase$(8,i)
next i
end

function toBase$(base,base10)
for i = 10 to 1 step -1
  toBase$   = str$(base10 mod base) +toBase$
  base10    = int(base10 / base)
  if base10 < 1 then exit for
next i
end function
```



## Rust


```rust
fn main() {
    for i in 0..std::usize::MAX {
        println!("{:o}", i);
    }
}
```



## Salmon


Salmon has built-in unlimited-precision integer arithmetic, so these examples will all continue printing octal values indefinitely, limited only by the amount of memory available (it requires O(log(n)) bits to store an integer n, so if your computer has 1 GB of memory, it will count to a number with on the order of <math>2^{80}</math> octal digits).


```Salmon
iterate (i; [0...+oo])
    printf("%o%\n", i);;
```


or


```Salmon
for (i; 0; true)
    printf("%o%\n", i);;
```


or


```Salmon
variable i := 0;
while (true)
  {
    printf("%o%\n", i);
    ++i;
  };
```



## Scala


```scala
Stream from 0 foreach (i => println(i.toOctalString))
```



## Scheme


```scheme
(do ((i 0 (+ i 1))) (#f) (display (number->string i 8)) (newline))
```



## Scratch

[[File:ScratchCountInOctal.png]]


## Seed7

This example uses the [http://seed7.sourceforge.net/libraries/integer.htm#%28in_integer%29radix%28in_integer%29 radix] operator to write a number in octal.


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: i is 0;
  begin
    repeat
      writeln(i radix 8);
      incr(i);
    until FALSE;
  end func;
```



## Sidef


```ruby
var i = 0;
loop { say i++.as_oct }
```



## Sparkling


```sparkling
for (var i = 0; true; i++) {
    printf("%o\n", i);
}
```



## Standard ML


```sml
local
  fun count n = (print (Int.fmt StringCvt.OCT n ^ "\n"); count (n+1))
in
  val _ = count 0
end
```



## Swift


```swift
import Foundation

func octalSuccessor(value: String) -> String {
   if value.isEmpty {
        return "1"
   } else {
     let i = value.startIndex, j = value.endIndex.predecessor()
     switch (value[j]) {
       case "0": return value[i..<j] + "1"
       case "1": return value[i..<j] + "2"
       case "2": return value[i..<j] + "3"
       case "3": return value[i..<j] + "4"
       case "4": return value[i..<j] + "5"
       case "5": return value[i..<j] + "6"
       case "6": return value[i..<j] + "7"
       case "7": return octalSuccessor(value[i..<j]) + "0"
       default:
         NSException(name:"InvalidDigit", reason: "InvalidOctalDigit", userInfo: nil).raise();
         return ""
     }
  }
}

var n = "0"
while strtoul(n, nil, 8) < UInt.max {
  println(n)
  n = octalSuccessor(n)
}
```


{{Output}}
The first few lines. anyway:

```txt
0
1
2
3
4
5
6
7
10
11
12
13
14
15
16
17
20
21
22
23
```



## Tcl


```tcl
package require Tcl 8.5;   # arbitrary precision integers; we can count until we run out of memory!
while 1 {
    puts [format "%llo" [incr counter]]
}
```



## UNIX Shell

We use the bc calculator to increment our octal counter:


```sh
#!/bin/sh
num=0
while true; do
  echo $num
  num=`echo "obase=8;ibase=8;$num+1"|bc`
done
```



### Using printf

Increment a decimal counter and use <code>printf(1)</code> to print it in octal. Our loop stops when the counter overflows to negative.


```sh
#!/bin/sh
num=0
while test 0 -le $num; do
  printf '%o\n' $num
  num=`expr $num + 1`
done
```


Various recent shells have a bultin <code>$(( ... ))</code> for arithmetic rather than running <code>expr</code>, in which case

{{works with|bash}}
{{works with|pdksh|5.2.14}}

```sh
num=0
while test 0 -le $num; do
  printf '%o\n' $num
  num=$((num + 1))
done
```



## VBA


With i defined as an Integer, the loop will count to 77777 (32767 decimal). Error handling added to terminate nicely on integer overflow.


```VBA

Sub CountOctal()
Dim i As Integer
i = 0
On Error GoTo OctEnd
Do
    Debug.Print Oct(i)
    i = i + 1
Loop
OctEnd:
Debug.Print "Integer overflow - count terminated"
End Sub

```



## VBScript


```vb

For i = 0 To 20
	WScript.StdOut.WriteLine Oct(i)
Next

```



## Vim Script


```vim
let counter = 0
while counter >= 0
    echon printf("%o\n", counter)
    let counter += 1
endwhile
```



## Whitespace


This program prints octal numbers until the internal representation of the current integer overflows to -1; it will never do so on some interpreters.


```Whitespace








































```


It was generated from the following pseudo-Assembly.


```asm
push 0
; Increment indefinitely.
0:
    push -1 ; Sentinel value so the printer knows when to stop.
    copy 1
    call 1
    push 10
    ochr
    push 1
    add
    jump 0
; Get the octal digits on the stack in reverse order.
1:
    dup
    push 8
    mod
    swap
    push 8
    div
    push 0
    copy 1
    sub
    jn 1
    pop
; Print them.
2:
    dup
    jn 3 ; Stop at the sentinel.
    onum
    jump 2
3:
    pop
    ret
```



## XPL0

XPL0 doesn't have built-in routines to handle octal; instead it uses hex.

```XPL0
include c:\cxpl\codes;     \intrinsic code declarations

proc OctOut(N);    \Output N in octal
int N;
int R;
[R:= N&7;
N:= N>>3;
if N then OctOut(N);
ChOut(0, R+^0);
];

int I;
[I:= 0;
repeat  OctOut(I); CrLf(0);
        I:= I+1;
until KeyHit or I=0;
]
```


Example output:

```txt

0
1
2
3
4
5
6
7
10
11
12
13
14
15
16
17
20
21

```



## zig


```zig
const std = @import("std");
const fmt = std.fmt;
const warn = std.debug.warn;

pub fn main() void {
    var i: u8 = 0;
    var buf: [3]u8 = undefined;

    while (i < 255) : (i += 1) {
        _ = fmt.formatIntBuf(buf[0..], i, 8, false, 0); // buffer, value, base, uppercase, width
        warn("{}\n", buf);
    }
}
```



## zkl


```zkl
foreach n in ([0..]){println("%.8B".fmt(n))}
```

{{out}}

```txt

0
1
2
3
4
5
6
7
10
11
12

```



## ZX Spectrum Basic


```zxbasic
10 PRINT "DEC.  OCT."
20 FOR i=0 TO 20
30 LET o$="": LET n=i
40 LET o$=STR$ FN m(n,8)+o$
50 LET n=INT (n/8)
60 IF n>0 THEN GO TO 40
70 PRINT i;TAB 3;" = ";o$
80 NEXT i
90 STOP
100 DEF FN m(a,b)=a-INT (a/b)*b
```

