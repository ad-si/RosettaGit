+++
title = "Integer sequence"
description = ""
date = 2019-10-18T19:31:11Z
aliases = []
[extra]
id = 9249
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Create a program that, when run, would display all integers from   '''1'''   to   <big><big> ''' <b> &infin; </b> ''' </big></big>   (or any relevant implementation limit),   in sequence   (i.e.   1, 2, 3, 4, etc)   if given enough time.


An example may not be able to reach arbitrarily-large numbers based on implementations limits.   For example, if integers are represented as a 32-bit unsigned value with 0 as the smallest representable value, the largest representable value would be 4,294,967,295.   Some languages support arbitrarily-large numbers as a built-in feature, while others make use of a module or library.

If appropriate, provide an example which reflect the language implementation's common built-in limits as well as an example which supports arbitrarily large numbers, and describe the nature of such limitations—or lack thereof.





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360).

```360asm
*        Integer sequence      06/05/2016
INTSEQED CSECT
         USING  INTSEQED,12
         LR    12,15
         LA    6,1             i=1
LOOP     CVD   6,DW            binary to pack decimal
         MVC   WTOMSG+4(12),EM12 load mask
         ED    WTOMSG+4(12),DW+2 packed dec to char
         WTO   MF=(E,WTOMSG)   write to console
         LA    6,1(6)          i=i+1
         B     LOOP            goto loop
WTOMSG   DC    0F,H'80',H'0',CL80' '
DW       DS    0D,PL8          pack dec 15num
EM12     DC    X'402020202020202020202120'  mask CL12 11num
         END   INTSEQED
```

{{out}}

```txt

...
      314090
      314091
      314092
      314093
      314094
      314095
      314096
      314097
      314098
      314099
...

```



## 0815


```0815
}:_:<:1:+%<:a:~$^:_:
```



## 8080 Assembly

Actually printing the numbers out would depend on the hardware and operating system.

```8080asm
        ORG     0100H
        MVI     A,    0   ; move immediate
LOOP:   INR     A         ; increment
   ; call 'PRINT' subroutine, if required
        JMP     LOOP      ; jump unconditionally

        END
```


A more complex, arbitrary precision version that can count as high as you have free bytes of memory to use. (This does assemble with CP/M's MAC assembler, but since it doesn't implement PRBUFR, it's only useful for exposition purposes, or for loading into DDT.)


```8080asm

        ORG     0100H
BITS    EQU     128       ; 128 bits of precision
BYTES   EQU     BITS / 8  ; Number of bytes we store those bits in

        ; Zero out the storage for our number
        LXI     H,BUFR    ; HL points at BUFR. (HL is idiomatically used for pointers)
        MVI     C,BYTES   ; C holds the number of bytes we'll use
        XRA     A         ; XOR with A is a 1-byte instruction to set A to zero
INIT:   MOV     M,A       ; Store 0 to address pointed to by HL
        INX     H         ; Advance HL to the next byte
        DCR     C         ; Count down
        JNZ     INIT      ; Keep looping if we're not done

        ; The "very long integer" is zeroed, so start the loop
LOOP:   CALL    PRBUFR    ; Output our number
        LXI     H,BUFR    ; HL Points to BUFR
        MVI     C,BYTES   ; Count down (assume fewer than 256 bytes in our integer)
NEXT:   INR     M         ; Increment the byte pointed to by HL. Sets the zero flag
        JNZ     LOOP      ; If the increment didn't overflow A, start the loop over
                          ; This byte overflowed, so we need to advance to the next byte in our number
        INX     H         ; We store our byes in order of increasing significance
        DCR     C         ; Count down to make sure we don't overflow our buffer
        JNZ     NEXT      ; jump to process the next, more significant byte

        ; If we get here, we have overflowed our integer!
        HALT              ; TODO: probably something other than "halt the CPU"

PRBUFR: ; TODO, a subroutine that shows all of the digits in BUFR on the console
        ; Assume that this code trashes all our registers...
        RET

BUFR:   ; This space will hold our number
        ; We zero this memory before the loop
        END
```



## Ada


```Ada
with Ada.Text_IO;
procedure Integers is
   Value : Integer := 1;
begin
   loop
      Ada.Text_IO.Put_Line (Integer'Image (Value));
      Value := Value + 1;  -- raises exception Constraint_Error on overflow
   end loop;
end Integers;
```

Alternative (iterating through all values of Positive (positive part of Integer) without endless loop):

```Ada
with Ada.Text_IO;
procedure Positives is
begin
   for Value in Positive'Range loop
      Ada.Text_IO.Put_Line (Positive'Image (Value));
   end loop;
end Positives;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
The upper limit of the loop variable ''i'' is ''max int'' currently ''+2147483647'' for [[ALGOL 68G]].

```algol68
main:
(
  FOR i DO
    printf(($g(0)","$,i))
  OD
)
```

Partial output:

```txt

1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,...

```



## ALGOL W


```algolw
begin
    % print the integers from 1 onwards                                       %
    % Algol W only has 32-bit integers. When i reaches 2^32,                  %
    % an integer overflow event would be raised which by default,             %
    % should terminate the program                                            %
    integer i;
    i := 1;
    while true do begin
        write( i );
        i := i + 1
    end loop_forever ;
end.
```



## Applesoft BASIC

Integer variables can be within the range of -32767 to 32767.

```Applesoft BASIC
 10 I% = 1
 20  PRINT I%;
 30 I% = I% + 1
 40  PRINT ", ";
 50  GOTO 20
```

Last screen of scrolled output:

```Applesoft BASIC
, 32646, 32647, 32648, 32649, 32650, 326
51, 32652, 32653, 32654, 32655, 32656, 3
2657, 32658, 32659, 32660, 32661, 32662,
 32663, 32664, 32665, 32666, 32667, 3266
8, 32669, 32670, 32671, 32672, 32673, 32
674, 32675, 32676, 32677, 32678, 32679,
32680, 32681, 32682, 32683, 32684, 32685
, 32686, 32687, 32688, 32689, 32690, 326
91, 32692, 32693, 32694, 32695, 32696, 3
2697, 32698, 32699, 32700, 32701, 32702,
 32703, 32704, 32705, 32706, 32707, 3270
8, 32709, 32710, 32711, 32712, 32713, 32
714, 32715, 32716, 32717, 32718, 32719,
32720, 32721, 32722, 32723, 32724, 32725
, 32726, 32727, 32728, 32729, 32730, 327
31, 32732, 32733, 32734, 32735, 32736, 3
2737, 32738, 32739, 32740, 32741, 32742,
 32743, 32744, 32745, 32746, 32747, 3274
8, 32749, 32750, 32751, 32752, 32753, 32
754, 32755, 32756, 32757, 32758, 32759,
32760, 32761, 32762, 32763, 32764, 32765
, 32766, 32767
?ILLEGAL QUANTITY ERROR IN 30
]
```



## ARM Assembly


```armasm
.text
.global main

@ An ARM program that keeps incrementing R0 forever
@
@ If desired, a call to some 'PRINT' routine --
@ which would depend on the OS -- could be included

main:
        mov   r0,   #0          @ start with R0 = 0

repeat:
        @ call to 'PRINT' routine
        add   r0,   r0,   #1    @ increment R0
        b     repeat            @ unconditional branch
```



## AutoHotkey

This uses traytip to show the results. A msgbox, tooltip, or fileappend could also be used.

```AutoHotkey
x=0
Loop
    TrayTip, Count, % ++x
```



## AWK


```awk
BEGIN {
    for( i=0; i != i + 1; i++ )
        print( i )
}
```


Awk uses floating-point numbers. This loop terminates when <code>i</code> becomes too large for integer precision. With IEEE doubles, this loop terminates when <code>i</code> reaches <code>2 ^ 53</code>.


## Axe

Integers in Axe are limited to 16 bits, or a maximum of 65535. This script will run infinitely until either the variable overflows or a key is pressed.


```axe
While getKey(0)
End
0→I
Repeat getKey(0)
 Disp I▶Dec,i
 I++
EndIf I=0
```



## BASIC

{{works with|ZX Spectrum Basic}}

```zxbasic
10 LET A = 0
20 LET A = A + 1
30 PRINT A
40 GO TO 20
```

{{works with|QBasic}}

```qbasic
A = 0
DO: A = A + 1: PRINT A: LOOP 1
```



## Batch File

Variables are limited to 32bit integer, capable of a maximum value of <code>2,147,483,647</code>

```dos

@echo off
set number=0
:loop
set /a number+=1
echo %number%
goto loop

```

{{out}}

```txt

...
2147483644
2147483645
2147483646
2147483647
-2147483648
-2147483647
-2147483646
-2147483645
...

```




## BBC BASIC

{{works with|BBC BASIC for Windows}}
Native version, limited to 53-bit integers (maximum output 9007199254740992):

```bbcbasic
      *FLOAT 64
      REPEAT
        i += 1
        PRINT TAB(0,0) i;
      UNTIL FALSE
```

Version using Huge Integer Math and Encryption library (up to 2^31 bits, but this program limited to 65535 decimal digits because of maximum string length):

```bbcbasic
      INSTALL @lib$+"HIMELIB"
      PROC_himeinit("")
      reg% = 1

      PROC_hiputdec(reg%, "0")
      REPEAT
        SYS `hi_Incr`, ^reg%, ^reg%
        PRINT TAB(0,0) FN_higetdec(reg%);
      UNTIL FALSE
```



## bc


```bc
while (++i) i
```



## beeswax

Using an ordinary loop structure:

```beeswax
 qNP<
_1>{d
```


Using a jump instruction:

```beeswax
_1F6~@{PN@J
```


Numbers in beeswax are unsigned 64-bit integers, so after reaching 2^64-1 the counter wraps around to 0.


## Befunge

The range of a numeric value in Befunge is implementation dependent, but is commonly 32 bit signed integers for the stack, so a maximum value of 2147483647. However, note that some implementations have a smaller range for ''displayed'' values, so the sequence may appear to wrap to negative numbers while the internal value is in fact still increasing.

Also note that the range of values written to the code page or 'playfield' is often much smaller - frequently only supporting 8 bits, sometimes signed, sometimes unsigned.


```befunge
1+:0`!#@_:.55+,
```



## Bracmat

{{trans|Ruby}}
Bracmat uses big numbers. Numbers are stored with a radix 10, each decimal digit occupying one byte. When multiplying or dividing, numbers are temporarily converted to radix 10000 (32-bit systems: 1 digit occupies two bytes) or radix 100000000 (64-bit systems: 1 digit occupies four bytes) to speed up the computation.
<lang>0:?n&whl'out$(1+!n:?n)
```


=={{header|Brainfuck}}==
This program assumes that decrementing past zero wraps around, but it doesn't rely on cell size, other than that a cell can hold at least six bits. It also assumes the ASCII character set. This is an arbitrarily large number implementation.
<lang Brainfuck>++++++++++>>>+[[->>+<[+>->+<<---------------------------------------
-------------------[>>-<++++++++++<[+>-<]]>[-<+>]<++++++++++++++++++
++++++++++++++++++++++++++++++>]<[<]>>[-<+++++++++++++++++++++++++++
++++++++++++++++++++++>]>]>[>>>]<<<[.<<<]<.>>>+]
```


This modification of the previous program will print out 1 to the maximum cell value, still assuming wrapping. On many implementations, this will print out 1-255.
<lang Brainfuck>++++++++++>>-[>+[->>+<[+>->+<<--------------------------------------
--------------------[>>-<++++++++++<[+>-<]]>[-<+>]<+++++++++++++++++
+++++++++++++++++++++++++++++++>]<[<]>>[-<++++++++++++++++++++++++++
+++++++++++++++++++++++>]>]>[>>>]<<<[.<<<]<.>>-]
```


This program can count in any base counting system under 256. '''Note:''' Change the characters in quotes equal to the base counting system you want to use.
<lang Brainfuck>+[<<+>>[[<<"-----------"["+++++++++++"<]>]>[<<<<+>>+>>[>>]<]<]>>[>>]<<]
```



## Brat


```brat
i = 1

loop {
  p i
  i = i + 1
}
```



## Burlesque



```burlesque

1R@

```



## C

Prints from 1 to max unsigned integer (usually 2**32 -1), then stops.

```c
#include <stdio.h>

int main()
{
	unsigned int i = 0;
	while (++i) printf("%u\n", i);

	return 0;
}
```


==={{libheader|GMP}}===

This one never stops.
It's not even likely that you'll run out of memory before you run out of patience.

```c
#include <gmp.h>

int main()
{
	mpz_t i;
	mpz_init(i); /* zero now */

	while (1) {
		mpz_add_ui(i, i, 1); /* i = i + 1 */
		gmp_printf("%Zd\n", i);
	}

	return 0;
}
```


==={{libheader|OpenSSL}}===
OpenSSL provides arbitrarily large integers.


```c
#include <openssl/bn.h>
		/* BN_*() */
#include <openssl/err.h>	/* ERR_*() */
#include <stdio.h>		/* fprintf(), puts() */

void
fail(const char *message)
{
	fprintf(stderr, "%s: error 0x%08lx\n", ERR_get_error());
	exit(1);
}

int
main()
{
	BIGNUM i;
	char *s;

	BN_init(&i);
	for (;;) {
		if (BN_add_word(&i, 1) == 0)
			fail("BN_add_word");
		s = BN_bn2dec(&i);
		if (s == NULL)
			fail("BN_bn2dec");
		puts(s);
		OPENSSL_free(s);
	}
	/* NOTREACHED */
}
```


## C#

```c#
using System;
using System.Numerics;

class Program
{
    static void Main()
    {
        BigInteger i = 1;
        while (true)
        {
            Console.WriteLine(i++);
        }
    }
}
```


## ChucK

Math.INT_MAX is a constant value that represents the greater integer, 32 bit , 64 bit systems.

```chuck
for(1 => int i; i < Math.INT_MAX; i ++)
{
    <<< i >>>;
}
```


## C++

```cpp
#include <cstdint>
#include <iostream>
#include <limits>

int main()
{
  auto i = std::uintmax_t{};

  while (i < std::numeric_limits<decltype(i)>::max())
    std::cout << ++i << '\n';
}
```

<!--

```cpp
// Using the proposed unbounded integer library

#include <iostream>
#include <seminumeric>

int main()
{
  try
  {
    auto i = std::experimental::seminumeric::integer{};

    while (true)
      std::cout << ++i << '\n';
  }
  catch (...)
  {
    // Do nothing
  }
}
```

-->


## Clean

In Clean this example has a limit of basically 2147483648.

```Clean
module IntegerSequence

import StdEnv

Start = [x \\ x <- [1..]]
```


Output:

```txt
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,..
```



## Clojure


```Clojure
(map println (next (range)))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Int-Sequence.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
*      *> 36 digits is the largest size a numeric field can have.
       01  I PIC 9(36).

       PROCEDURE DIVISION.
*          *> Display numbers until I overflows.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = 0
               DISPLAY I
           END-PERFORM

           GOBACK
           .
```



## CoffeeScript

Like with most languages, counting is straightforward in CoffeeScript, so the program below tries to handle very large numbers.  See the comments for starting the sequence from 1.


```coffeescript

# This very limited BCD-based collection of functions
# makes it easy to count very large numbers.  All arrays
# start off with the ones columns in position zero.
# Using arrays of decimal-based digits to model integers
# doesn't make much sense for most tasks, but if you
# want to keep counting forever, this does the trick.

BcdInteger =
  from_string: (s) ->
    arr = []
    for c in s
      arr.unshift parseInt(c)
    arr

  render: (arr) ->
    s = ''
    for elem in arr
      s = elem.toString() + s
    s

  succ: (arr) ->
    arr = (elem for elem in arr)
    i = 0
    while arr[i] == 9
      arr[i] = 0
      i += 1
    arr[i] ||= 0
    arr[i] += 1
    arr

# To start counting from 1, change the next line!
big_int = BcdInteger.from_string "199999999999999999999999999999999999999999999999999999"
while true
  console.log BcdInteger.render big_int
  big_int = BcdInteger.succ big_int

```


output
<lang>
> coffee foo.coffee | head -5
199999999999999999999999999999999999999999999999999999
200000000000000000000000000000000000000000000000000000
200000000000000000000000000000000000000000000000000001
200000000000000000000000000000000000000000000000000002
200000000000000000000000000000000000000000000000000003

```




## Common Lisp



```lisp
(loop for i from 1 do (print i))
```


If your compiler does tail call elimination (note: this has absolutely no advantage over normal loops):

```lisp
(defun pp (x) (pp (1+ (print x))))
(funcall (compile 'pp) 1) ; it's less likely interpreted mode will eliminate tails
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE IntegerSequence;
IMPORT StdLog;

PROCEDURE Do*;
VAR
	i: INTEGER;
BEGIN
	FOR i := 0 TO MAX(INTEGER) DO;
		StdLog.Int(i)
	END;
	StdLog.Ln
END Do;

END IntegerSequence.

```

Execute: ^Q IntegerSequence.Do<br/>
Output:

```txt

 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 ...

```



## Computer/zero Assembly

This program counts up to 255 in the accumulator, after which it starts again from zero.

```czasm
start:  ADD  one
        JMP  start
one:         1
```



## D


```d
import std.stdio, std.bigint;

void main() {
    BigInt i;
    while (true)
        writeln(++i);
}
```

Alternative:

```d
import std.stdio, std.traits, std.bigint, std.string;

void integerSequence(T)() if (isIntegral!T || is(T == BigInt)) {
    T now = 1;
    T max = 0;
    static if (!is(T == BigInt))
        max = T.max;

    do
        write(now, " ");
    while (now++ != max);

    writeln("\nDone!");
}

void main() {
    writeln("How much time do you have?");
    writeln(" 0. I'm in hurry.");
    writeln(" 1. I've some time.");
    writeln(" 2. I'm on vacation.");
    writeln(" 3. I'm unemployed...");
    writeln(" 4. I'm immortal!");
    write("Enter 0-4 or nothing to quit: ");

    string answer;
    readf("%s\n", &answer);

    switch (answer.toLower()) {
        case "0": integerSequence!ubyte();  break;
        case "1": integerSequence!short();  break;
        case "2": integerSequence!uint();   break;
        case "3": integerSequence!long();   break;
        case "4": integerSequence!BigInt(); break;
        default: writeln("\nBye bye!");     break;
    }
}
```



## Dc


```Dc
1[p1+lpx]dspx
```


## DCL


```DCL
$ i = 1
$ loop:
$  write sys$output i
$  i = i + 1
$  goto loop
```

{{out}}

```txt
1
2
3
...
2147483646
2147483647
-2147483648
-2147483647
...
-1
0
1
...
```



## Delphi


```Delphi
program IntegerSequence;

{$APPTYPE CONSOLE}

var
  i: Integer;
begin
  for i := 1 to High(i) do
    WriteLn(i);
end.
```


=={{header|Déjà Vu}}==

```dejavu
1

while /= -- dup dup:
	!. dup
	++

drop
```


This continues to print numbers until double precision IEEE 754 cannot represent adjacent integers any more (9007199254740992, to be exact).

In the future, the implementation may switch to arbitrary precision, so it will keep running until memory fills up.


## DWScript

High(i) returns the maximum supported value, typically, it is the highest signed 64 bit integer.

```delphi

var i: Integer;

for i:=1 to High(i) do
   PrintLn(i);

```



## Dyalect



```dyalect
var n = 0
while true {
    n += 1
    print(n)
}
```



## E



```e>for i in int
 0 { println(i) }
```



## EchoLisp


```scheme

(lib 'bigint) ;; arbitrary length integers
(for ((n (in-naturals))) (writeln n))

```



## EDSAC order code


```edsac
[ Integer sequence

### ==========


  A program for the EDSAC

  Displays integers 1,2,3...
  in binary form in the first
  word of storage tank 2
  until stopped

  Works with Initial Orders 2  ]

T56K  [ set load point         ]
GK    [ set base address       ]

A3@   [ increment accumulator  ]
U64F  [ copy accumulator to 64 ]
E@    [ jump to base address   ]

P0D   [ constant: 1            ]

EZPF  [ begin at load point    ]
```



## Eiffel


```eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make
feature {NONE} -- Initialization
	make
			-- Run application.
		do
			from
				number := 0
			until
				number = number.max_value
			loop
				print(number)
				print(", ")
				number := number + 1
			end
		end
	number:INTEGER_64
end

```


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    var i := 0u;
    while (true)
    {
        console.printLine(i);

        i += 1u
    }
}
```



## Elixir


```elixir
Stream.iterate(1, &(&1+1)) |> Enum.each(&(IO.puts &1))
```



## Emacs Lisp

Displays in the message area interactively, or to standard output under <code>-batch</code>.


```lisp
(dotimes (i most-positive-fixnum)
  (message "%d" (1+ i)))
```



## Erlang



```erlang
 F = fun(FF, I) -> io:format("~p~n", [I]), FF(FF, I + 1) end, F(F,0).
```



## ERRE

<lang>
.............
A%=0
LOOP
  A%=A%+1
  PRINT(A%;)
END LOOP
.............

```

% is integer-type specificator. Integer type works on 16-bit signed numbers (reserved constant MAXINT is 32767). Beyond this limit execution will give Runtime error #6 (overflow).


## Euphoria


```euphoria
integer i
i = 0
while 1 do
    ? i
    i += 1
end while
```


=={{header|F_Sharp|F#}}==


```fsharp
// lazy sequence of integers starting with i
let rec integers i =
  seq { yield i
        yield! integers (i+1) }

Seq.iter (printfn "%d") (integers 1)
```


lazy sequence of int32 starting from 0

```fsharp>let integers = Seq.initInfinite id</lang


lazy sequence of int32 starting from n

```fsharp
let integers n = Seq.initInfinite ((+) n)
```


lazy sequence (not necessarily of int32) starting from n (using unfold anamorphism)

```fsharp
let inline numbers n =
    Seq.unfold (fun n -> Some (n, n + LanguagePrimitives.GenericOne)) n
```

<div>
 > numbers 0 |> Seq.take 10;;
 val it : seq<int> = seq [0; 1; 2; 3; ...]
 > let bignumber = 12345678901234567890123456789012345678901234567890;;
 val bignumber : System.Numerics.BigInteger =
   12345678901234567890123456789012345678901234567890
 > numbers bignumber |> Seq.take 10;;
 val it : seq<System.Numerics.BigInteger> =
  seq
    [12345678901234567890123456789012345678901234567890 {IsEven = true;
                                                         IsOne = false;
                                                         IsPowerOfTwo = false;
                                                         IsZero = false;
                                                         Sign = 1;};
     12345678901234567890123456789012345678901234567891 {IsEven = false;
                                                         IsOne = false;
                                                         IsPowerOfTwo = false;
                                                         IsZero = false;
                                                         Sign = 1;};
     12345678901234567890123456789012345678901234567892 {IsEven = true;
                                                         IsOne = false;
                                                         IsPowerOfTwo = false;
                                                         IsZero = false;
                                                         Sign = 1;};
     12345678901234567890123456789012345678901234567893 {IsEven = false;
                                                         IsOne = false;
                                                         IsPowerOfTwo = false;
                                                         IsZero = false;
                                                         Sign = 1;}; ...]
 > numbers 42.42 |> Seq.take 10;;
 val it : seq<float> = seq [42.42; 43.42; 44.42; 45.42; ...]
</div>

## Factor


```factor
USE: lists.lazy
1 lfrom [ . ] leach
```



## Fantom



```fantom

class Main
{
  public static Void main()
  {
    i := 1
    while (true)
    {
      echo (i)
      i += 1
    }
  }
}

```


Fantom's integers are 64-bit signed, and so the numbers will return to 0 and continue again, if you wait long enough!
You can use Java BigInteger via FFI


## Fish

Since there aren't really libraries in Fish and I wouldn't know how to program arbitarily large integers, so here's an example that just goes on until the interpreter's number limit:

```Fish>0
:n1+v
 ^o" "<
```



## Forth


```forth
: ints ( -- )
  0 begin 1+ dup cr u. dup -1 = until drop ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Intseq
  implicit none

  integer, parameter :: i64 = selected_int_kind(18)
  integer(i64) :: n = 1

! n is declared as a 64 bit signed integer so the program will display up to
! 9223372036854775807 before overflowing to -9223372036854775808
  do
    print*, n
    n = n + 1
  end do
end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' FB does not natively support arbitrarily large integers though support can be added
' by using an external library such as GMP. For now we will just use an unsigned integer (32bit).

Print "Press Ctrl + C to stop the program at any time"
Dim i As UInteger = 1

Do
  Print i
  i += 1
Loop Until i = 0 ' will wrap back to 0 when it reaches 4,294,967,296

Sleep
```



## Frink

All of Frink's numbers can be arbitrarily-sized:

```Frink

i=0
while true
{
   println[i]
   i = i + 1
}

```



## FunL

The following has no limit since FunL has arbitrary size integers.


```funl
for i <- 1.. do println( i )
```



## Futhark


Infinite loops cannot produce results in Futhark, so this program
accepts an input indicating how many integers to generate.  It encodes the size of the returned array in its type.


```Futhark

fun main(n: int): [n]int = iota n

```



## GAP


```gap
InfiniteLoop := function()
	local n;
	n := 1;
	while true do
		Display(n);
		n := n + 1;
	od;
end;

# Prepare some coffee
InfiniteLoop();
```



## Go

Size of <tt>int</tt> type is implementation dependent.  After the maximum positive value, it rolls over to maximum negative, without error. Type <tt>uint</tt> will roll over to zero.

```go
package main

import "fmt"

func main() {
    for i := 1;; i++ {
        fmt.Println(i)
    }
}
```

The <tt>big.Int</tt> type does not roll over and is limited only by available memory, or practically, by whatever external factor halts CPU execution:  human operator, lightning storm, CPU fan failure, heat death of universe, etc.

```go
package main

import (
    "big"
    "fmt"
)

func main() {
    one := big.NewInt(1)
    for i := big.NewInt(1);; i.Add(i, one) {
        fmt.Println(i)
    }
}
```


## Gridscript


```gridscript

#INTEGER SEQUENCE.

@width
@height 1

(1,1):START
(3,1):STORE 1
(5,1):CHECKPOINT 0
(7,1):PRINT
(9,1):INCREMENT
(11,1):GOTO 0

```



## Groovy


```groovy
// 32-bit 2's-complement signed integer (int/Integer)
for (def i = 1; i > 0; i++) { println i }

// 64-bit 2's-complement signed integer (long/Long)
for (def i = 1L; i > 0; i+=1L) { println i }

// Arbitrarily-long binary signed integer (BigInteger)
for (def i = 1g; ; i+=1g) { println i }
```



## GUISS


Graphical User Interface Support Script makes use of installed programs. There are no variables, no loop structures and no jumps within the language so iteration is achieved by repetative instructions. In this example, we will just use the desktop calculator and keep adding one to get a counter. We stop after counting to ten in this example.


```guiss
Start,Programs,Accessories,Calculator,
Button:[plus],Button:1,Button:[equals],Button:[plus],Button:1,Button:[equals],
Button:[plus],Button:1,Button:[equals],Button:[plus],Button:1,Button:[equals],
Button:[plus],Button:1,Button:[equals],Button:[plus],Button:1,Button:[equals],
Button:[plus],Button:1,Button:[equals],Button:[plus],Button:1,Button:[equals],
Button:[plus],Button:1,Button:[equals],Button:[plus],Button:1,Button:[equals]
```



## Haskell


```haskell
mapM_ print [1..]
```


Or less imperatively:


```haskell
putStr $ unlines $ map show [1..]
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon support large integers by default.  The built-in generator seq(i,j) yields the infinite sequence i, i+j, i+2*j, etc.  Converting the results to strings for display will likely eat your lunch before the sequence will take its toll.


```Icon
procedure main()
every write(seq(1))        # the most concise way
end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 FOR I=1 TO INF
110   PRINT I;
120 NEXT
```


INF = 9.999999999E62


## HolyC

Prints from 1 to max unsigned 64 bit integer (2**64 -1), then stops.

```holyc
U64 i = 0;
while (++i) Print("%d\n", i);

```



## J

The following will count indefinitely but once the 32-bit (or 64-bit depending on J engine version) limit is reached, the results will be reported as floating point values (which would immediately halt on 64 bit J and halt with the 53 bit precision limit is exceeded on 32 bit J).  Since that could take many, many centuries, even on a 32 bit machine, more likely problems include the user dying of old age and failing to pay the electric bill resulting in the machine being powered off.


```j
 count=: (smoutput ] >:)^:_
```


The above works with both fixed sized integers and floating point numbers (fixed sized integers are automatically promoted to floating point, if they overflow), but also works with extended precision integers (which will not overflow, unless they get so large that they cannot be represented in memory, but that should exceed lifetime of the universe, let alone lifetime of the computer).

This adds support for extended precision (in that it converts non-extended precision arguments to extended precision arguments) and will display integers to ∞ (or at least until the machine is turned off or interrupted or crashes).

```j
 count=: (smoutput ] >:)@x:^:_
```



## Java

Long limit:

```java
public class Count{
    public static void main(String[] args){
        for(long i = 1; ;i++) System.out.println(i);
    }
}
```

"Forever":

```java
import java.math.BigInteger;

public class Count{
    public static void main(String[] args){
        for(BigInteger i = BigInteger.ONE; ;i = i.add(BigInteger.ONE)) System.out.println(i);
    }
}
```



## JavaScript


```javascript
var i = 0;

while (true)
    document.write(++i + ' ');
```



## Joy


```joy

1 [0 >] [dup put succ] while pop.
```


Counting stops at <code>maxint</code>, which is 2147483647


## jq

Currently, julia does not support infinite-precision arithmetic, but very large integers are converted to floating-point numbers, so the following will continue to generate integers (beginning with 0) indefinitely in recent versions of jq that have tail recursion optimization:

```jq
def iota: ., (. + 1 | iota);
0 | iota
```
In versions of jq which have <tt>while</tt>, one could also write:
```jq
0 | while(true;. + 1)
```
This idiom is likely to be more useful as <tt>while</tt> supports <tt>break</tt>.

Another technique would be to use <tt>recurse</tt>:

```jq
0 | recurse(. + 1)
```
For generating integers, the generator, <tt>range(m;n)</tt>, is more likely to be useful in practice; if m and n are integers, it generates integers from m to n-1, inclusive.


## Julia


```julia
i = zero(BigInt)    # or i = big(0)
while true
  println(i += 1)
end
```

The built-in <code>BigInt</code> type is an arbitrary precision integer (based on the GMP library), so the value of <code>i</code> is limited only by available memory.  To use (much faster) hardware fixed-width integer types, use e.g. <code>zero(Int32)</code> or <code>zero(Int64)</code>.  (Initializing <code>i = 0</code> will use fixed-width integers that are the same size as the hardware address width, e.g. 64-bit on a 64-bit machine.)


## K


```k
  {`0:"\n",$x+:1;x}/1
```


Using a <code>while</code> loop:


```k
  i:0; while[1;`0:"\n",$i+:1]
```



## Kotlin


```scala
import java.math.BigInteger

// version 1.0.5-2

fun main(args: Array<String>) {
    // print until 2147483647
    (0..Int.MAX_VALUE).forEach { println(it) }

    // print forever
    var n = BigInteger.ZERO
    while (true) {
        println(n)
        n += BigInteger.ONE
    }
}
```



## Lang5


```lang5>0 do dup . 1 + loop</lang



## Lasso


```Lasso
local(number = 1)
while(#number > 0) => {^
	#number++
	' '
	//#number > 100 ? #number = -2 // uncomment this row if you want to halt the run after proving concept
^}
```

This will run until you exhaust the system resources it's run under.


## Liberty BASIC

Liberty BASIC handles extremely large integers. The following code was halted by user at 10,000,000 in test run.

```lb
 while 1
    i=i+1
    locate 1,1
    print i
    scan
wend

```



## Limbo

The int (32 bits) and big (64 bits) types are both signed, so they wrap around.  This version uses the infinite precision integer library:


```Limbo
implement CountToInfinity;

include "sys.m"; sys: Sys;
include "draw.m";
include "ipints.m"; ipints: IPints;
	IPint: import ipints;

CountToInfinity: module {
	init: fn(nil: ref Draw->Context, nil: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	ipints = load IPints IPints->PATH;

	i := IPint.inttoip(0);
	one := IPint.inttoip(1);
	for(;;) {
		sys->print("%s\n", i.iptostr(10));
		i = i.add(one);
	}
}

```



## Lingo


```lingo
i = 1
repeat while i>0
  put i
  i = i+1
end repeat
```


Lingo uses signed 32 bit integers, so max. supported integer value is 2147483647:

```lingo
put the maxInteger
-- 2147483647
```


Beyond this limit values behave like negative numbers:

```lingo
put the maxInteger+1
-- -2147483648
put the maxInteger+2
-- -2147483647
```


Up to the (quite high) number where floats (double-precission) start rounding, floats can be used to exceed the integer limit:

```lingo
the floatPrecision = 0 -- forces floats to be printed without fractional digits

put float(the maxInteger)+1
-- 2147483648

-- max. whole value that can be stored as 8-byte-float precisely
maxFloat = power(2,53) -- 9007199254740992.0

i = 1.0
repeat while i<=maxFloat
  put i
  i = i+1
end repeat
-- 1
-- 2
-- 3
-- ...
```



## LLVM

{{trans|C}}

```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

;--- The declarations for the external C functions
declare i32 @printf(i8*, ...)

$"FORMAT_STR" = comdat any
@"FORMAT_STR" = linkonce_odr unnamed_addr constant [4 x i8] c"%u\0A\00", comdat, align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4          ;-- allocate i
  store i32 0, i32* %1, align 4     ;-- store i as 0
  br label %loop

loop:
  %2 = load i32, i32* %1, align 4   ;-- load i
  %3 = add i32 %2, 1                ;-- increment i
  store i32 %3, i32* %1, align 4    ;-- store i
  %4 = icmp ne i32 %3, 0            ;-- i != 0
  br i1 %4, label %loop_body, label %exit

loop_body:
  %5 = load i32, i32* %1, align 4   ;-- load i
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"FORMAT_STR", i32 0, i32 0), i32 %5)
  br label %loop

exit:
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
```



## Lua


```lua

i = 1

-- in the event that the number inadvertently wraps around,
-- stop looping - this is unlikely with Lua's default underlying
-- number type (double), but on platform without double
-- the C type used for numbers can be changed
while i > 0 do
    print( i )
    i = i + 1
end

```



## M2000 Interpreter


```M2000 Interpreter

\\ easy way
a=1@
\\ Def statement defines one time (second pass produce error)
Rem : Def Decimal a=1
Rem : Def a as decimal=1
\\ Global shadow any global with same name, but not local
\\ globals can change type, local can't change
\\ to assign value to global need <=
\\ Symbol = always make local variables (and shadows globals)
Rem : Global a as decimal =1
\\Local make a new local and shadow one with same name
Rem : Local a as decimal=1
\\ we can create an "auto rounding" variable
\\ an integer with any type (double, single, decimal, currency, long, integer)
\\ rounding to .5 : up for positive numbers and down to negative
\\ 1.5 round to 2 and -1.5 round to -2
a%=1@

\\ variables a, a%, a$, arrays/functions a(), a$(), sub a() and the module a can exist together
\\ A block may act as loop structure using an internal flag
\\ A Loop statement mark a flag in the block, so can be anywhere inside,
\\ this flag reset to false before restart.
{loop : Print a : a++}

```



## Maple

Maple has arbitrary-precision integers so there are no built-in limits on the size of the integers represented.


```Maple
for n do
   print(n)
end do;
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Built in arbitrary precision support meanst the following will not overflow.

```Mathematica

x = 1;
Monitor[While[True, x++], x]

```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
 a = 1; while (1) printf('%i\n',a); a=a+1; end;
```


Typically, numbers are stored as double precision floating point numbers, giving accurate integer values up to about 2^53=bitmax('double')=9.0072e+15. Above this limit, round off errors occur. This limitation can be overcome by defining the numeric value as type int64 or uint64


```Matlab
 a = uint64(1); while (1) printf('%i\n',a); a=a+1; end;
```


This will run up to 2^64 and then stop increasing, there will be no overflow.


```txt

>> a=uint64(10e16+1)    % 10e16 is first converted into a double precision number causing some round-off error.
a = 100000000000000000
>> a=uint64(10e16)+1
a = 100000000000000001

```


The above limitations can be overcome with additional toolboxes for symbolic computation or multiprecision computing.

Matlab and Octave recommend vectorizing the code, one might pre-allocate the sequence up to a specific N.


```Matlab
  N = 2^30; printf('%d\n', 1:N);
```


The main limitation is the available memory on your machine. The standard version of Octave has a limit that a single data structure can hold at most 2^31 elements. In order to overcome this limit, Octave must be compiled with "./configure --enable-64", but this is currently not well supported.


## Maxima


```maxima
for i do disp(i);
```



## min

{{works with|min|0.19.3}}
min's integers are 64-bit signed. This will eventually overflow.

```min
0 (dup) () (puts succ) () linrec
```


=={{header|MK-61/52}}==
<lang>1	П4	ИП4	С/П	КИП4	БП	02
```



## ML/I


```ML/I
MCSKIP "WITH" NL
"" Integer sequence
"" Will overflow when it reaches implementation-defined signed integer limit
MCSKIP MT,<>
MCINS %.
MCDEF DEMO WITHS NL AS <MCSET T1=1
%L1.%T1.
MCSET T1=T1+1
MCGO L1
>
DEMO
```


=={{header|Modula-2}}==

```modula2
MODULE Sequence;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i : CARDINAL;
BEGIN
    i := 1;
    WHILE i>0 DO
        FormatString("%c ", buf, i);
        WriteString(buf);
        INC(i)
    END;
    ReadChar
END Sequence.
```



## NetRexx


### Rexx Built In

NetRexx provides built-in support for very large precision arithmetic via the <tt>Rexx</tt> class.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

k_ = Rexx
bigDigits = 999999999 -- Maximum setting for digits allowed by NetRexx
numeric digits bigDigits

loop k_ = 1
  say k_
  end k_

```



### Using BigInteger

Java's <tt>BigInteger</tt> class is also available for very large precision arithmetic.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import java.math.BigInteger

-- allow an option to change the output radix.
parse arg radix .
if radix.length() == 0 then radix = 10 -- default to decimal
k_ = BigInteger
k_ = BigInteger.ZERO

loop forever
  k_ = k_.add(BigInteger.ONE)
  say k_.toString(int radix)
  end

```



## NewLISP


```NewLISP
(while (println (++ i)))
```



## Nim


```nim
var i:int64 = 0
while true:
    inc i
    echo i
```


Using BigInts:

```nim
import bigints

var i = 0.initBigInt
while true:
  i += 1
  echo i
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE IntegerSeq;
IMPORT
  Out,
  Object:BigInt;

  PROCEDURE IntegerSequence*;
  VAR
    i: LONGINT;
  BEGIN
    FOR i := 0 TO MAX(LONGINT) DO
      Out.LongInt(i,0);Out.String(", ")
    END;
    Out.Ln
  END IntegerSequence;

  PROCEDURE BigIntSequence*;
  VAR
    i: BigInt.BigInt;
  BEGIN
    i := BigInt.zero;
    LOOP
      Out.Object(i.ToString() + ", ");
      i := i.Add(BigInt.one);
    END
  END BigIntSequence;

END IntegerSeq.

```



## Objeck


```objeck

bundle Default {
  class Count {
    function : Main(args : String[]) ~ Nil {
      i := 0;
      do {
        i->PrintLine();
        i += 1;
      } while(i <> 0);
    }
  }
}

```



## OCaml

with an imperative style:

```ocaml
let () =
  let i = ref 0 in
  while true do
    print_int !i;
    print_newline ();
    incr i;
  done
```


with a functional style:

```ocaml
let () =
  let rec aux i =
    print_int i;
    print_newline ();
    aux (succ i)
  in
  aux 0
```



## Oforth


Oforth handles arbitrary integer precision.

The loop will stop when out of memory


```Oforth
: integers  1 while( true ) [ dup . 1+ ] ;
```



## Ol

Ol does not limit the size of numbers. So maximal number depends only on available system memory.

```scheme

(let loop ((n 1))
   (print n)
   (loop (+ 1 n)))

```


Sample sequence with break for large numbers:

```scheme

(let loop ((n 2))
   (print n)
   (unless (> n 100000000000000000000000000000000)
      (loop (* n n))))

```

Output:

```txt

2
4
16
256
65536
4294967296
18446744073709551616
340282366920938463463374607431768211456

```



## OpenEdge/Progress

OpenEdge has three data types that can be used for this task:
<ol><li>INTEGER (32-bit signed integer)

```progress
DEF VAR ii AS INTEGER FORMAT "->>>>>>>>9" NO-UNDO.

DO WHILE TRUE:
   ii = ii + 1.
   DISPLAY ii.
END.
```

When an integer rolls over its maximum of 2147483647 error 15747 is raised (Value # too large to fit in INTEGER.).
</li>
<li>INT64 (64-bit signed integer)

```progress
DEF VAR ii AS INT64 FORMAT "->>>>>>>>>>>>>>>>>>9" NO-UNDO.

DO WHILE TRUE:
   ii = ii + 1.
   DISPLAY ii.
END.
```

When a 64-bit integer overflows no error is raised and the signed integer becomes negative.
</li>
<li>DECIMAL (50 digits)

```progress
DEF VAR de AS DECIMAL FORMAT "->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>9" NO-UNDO.

DO WHILE TRUE:
   de = de + 1.
   DISPLAY de.
END.
```

When a decimal requires mores than 50 digits error 536 is raised (Decimal number is too large.).
</li>
</ol>


## Order

Order supports arbitrarily-large positive integers natively. However, the simple version:

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8printloop ORDER_PP_FN( \
8fn(8N,                                      \
    8do(8print(8to_lit(8N) 8comma 8space),   \
        8printloop(8inc(8N)))) )

ORDER_PP( 8printloop(1) )
```

... while technically fulfilling the task, will probably never display anything, as most C Preprocessor implementations won't print their output until the file is done processing. Since the C Preprocessor is not technically Turing-complete, the Order interpreter has a maximum number of steps it can execute - but this number is very, very large (from the documentation: "the Order interpreter could easily be extended with a couple of hundred macros to prolong the wait well beyond the estimated lifetime of the sun"), so the compiler is rather more likely to simply run out of memory.

To actually see anything with GCC, add a maximum limit so that the task can complete:

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8printloop ORDER_PP_FN( \
8fn(8N,                                      \
    8do(8print(8to_lit(8N) 8comma 8space),   \
        8when(8less(8N, 99), 8printloop(8inc(8N))))) )

ORDER_PP( 8printloop(1) )   // 1, ..., 99,
```



## PARI/GP


```parigp
n=0; while(1,print(++n))
```



## Pascal

See also [[Integer_sequence#Delphi | Delphi]]
{{works with|Free_Pascal}}
Quad word has the largest positive range of all ordinal types

```pascal
Program IntegerSequenceLimited;
var
  Number: QWord = 0; // 8 bytes, unsigned: 0 .. 18446744073709551615
begin
  repeat
    writeln(Number);
    inc(Number);
  until false;
end.
```

{{libheader|GMP}}
With the gmp library your patience is probably the limit :-)

```pascal
Program IntegerSequenceUnlimited;

uses
  gmp;

var
  Number: mpz_t;

begin
  mpz_init(Number); //* zero now *//
  repeat
    mp_printf('%Zd' + chr(13) + chr(10), @Number);
    mpz_add_ui(Number, Number, 1); //* increase Number *//
  until false;
end.
```



## Perl


```perl
my $i = 0;
print ++$i, "\n" while 1;
```


On 64-bit Perls this will get to <tt>2^64-1</tt> then print <tt>1.84467440737096e+19</tt> forever.  On 32-bit Perls using standard doubles this will get to <tt>999999999999999</tt> then start incrementing and printing floats until they lose precision.  This behavior can be changed by adding something like:

```perl
use bigint;
my $i = 0;  print ++$i, "\n" while 1;
```

which makes almost all integers large (ranges are excluded).  Faster alternatives exist with non-core modules, e.g.
* <tt>use bigint lib=>"GMP";</tt>
* <tt>use Math::Pari qw/:int/;</tt>
* <tt>use Math::GMP qw/:constant/;</tt>


## Perl 6


```perl6
.say for 1..*
```



## Phix

This will crash at 1,073,741,824 on 32 bit, 4,611,686,018,427,387,904 on 64-bit:

```Phix
integer i = 0
while 1 do
    ?i
    i += 1
end while
```

This will stall at 9,007,199,254,740,992 on 32-bit, and about twice the above on 64-bit.
(after ~15 or 19 digits of precision, adding 1 will simply cease to have any effect)

```Phix
atom a = 0
while 1 do
    ?a
    a += 1
end while
```

{{libheader|mpfr}}
This will probably carry on until the number has over 300 million digits (32-bit, you can
square that on 64-bit) which would probably take zillions of times longer than the
universe has already existed, if your hardware/OS/power grid kept going that long.

```Phix
include mpfr.e
mpz b = mpz_init(0)
while true do
    mpz_add_ui(b,b,1)
    mpfr_printf(1,"%Zd\n",b)
end while
```



## PicoLisp


```PicoLisp
(for (I 1 T (inc I))
   (printsp I) )
```



## Piet


Rendered as a Wiki table because uploading images is not possible.

{| style="border-collapse: collapse; border-spacing: 0; font-family: courier-new,courier,monospace; font-size: 18px; line-height: 1.2em; padding: 0px"
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#c0ffc0; color:#c0ffc0;" | ww
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#c000c0; color:#c000c0;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#00ffff; color:#00ffff;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww

|}

Program explanation on my user page: [http://rosettacode.org/wiki/User:Albedo#Integer_Sequence]


## PILOT


```pilot
C  :n = 1
*InfiniteLoop
T  :#n
C  :n = n + 1
J  :*InfiniteLoop
```



## PL/I


```PL/I

infinity: procedure options (main);
   declare k fixed decimal (30);
   put skip edit
      ((k do k = 1 to 999999999999999999999999999998))(f(31));
end infinity;

```



## PostScript

{{libheader|initlib}}

```postscript

1 {succ dup =} loop

```



## Prolog


```Prolog
loop(I) :-
	writeln(I),
	I1 is I+1,
	loop(I1).

```



### Constraint Handling Rules

Works with SWI-Prolog and library '''CHR''' written by  '''Tom Schrijvers''' and '''Jan Wielemaker'''

```Prolog
:- use_module(library(chr)).

:- chr_constraint loop/1.

loop(N) <=> writeln(N), N1 is N+1, loop(N1).

```



## PowerShell


```Powershell
try
{
    for ([int]$i = 0;;$i++)
    {
        $i
    }
}
catch {break}
```



## PureBasic


```PureBasic
OpenConsole()
Repeat
  a.q+1
  PrintN(Str(a))
ForEver
```



## Python


```python
i=1
while i:
    print(i)
    i += 1
```


Or, alternatively:

```python
from itertools import count

for i in count():
    print(i)
```


Pythons integers are of arbitrary large precision and so programs would probably keep going until OS or hardware system failure.


## Q

{{trans|K}}

Using converge (the <tt>\</tt> adverb):

```q
({-1 string x; x+1}\) 1
```


Using <tt>while</tt>:

```q
i:0; while[1;-1 string (i+:1)]
```



## R


```r
z <- 0
repeat {
	print(z)
	z <- z + 1
}
```



## Racket


Racket uses bignums, so counting should continue up to very large numbers. Naturally, printing these numbers will consume quite a bit of power.


```Racket
#lang racket
(for ([i (in-naturals)]) (displayln i))

```



## Raven

Raven uses signed 32 bit integer values.

```Raven
1 as $i
repeat TRUE while
   $i "%d\n" print   $i 1000 +  as $i
```



## Retro

Retro uses signed integer values.


```Retro
#0 [ [ n:put spa ] sip n:inc dup n:-zero? ] while drop
```



## REXX


```rexx
/*count all the protons, electrons, & whatnot in the universe, and then */
/*keep counting.  According to some pundits in-the-know, one version of */
/*the big-bang theory is that the universe will collapse back to where  */
/*it started, and this computer program will be still counting.         */
/*┌────────────────────────────────────────────────────────────────────┐
  │ Count all the protons  (and electrons!)  in the universe, and then │
  │ keep counting.  According to some pundits in-the-know, one version │
  │ of the big-bang theory is that the universe will collapse back to  │
  │ where it started, and this computer program will still be counting.│
  │                                                                    │
  │                                                                    │
  │ According to Sir Arthur Eddington in 1938 at his Tamer Lecture at  │
  │ Trinity College (Cambridge), he postulated that there are exactly  │
  │                                                                    │
  │                              136 ∙ 2^256                           │
  │                                                                    │
  │ protons in the universe and the same number of electrons, which is │
  │ equal to around  1.57477e+79.                                      │
  │                                                                    │
  │ Although, a modern estimate is around  10^80.                      │
  │                                                                    │
  │                                                                    │
  │ One estimate of the age of the universe is  13.7  billion years,   │
  │ or  4.32e+17 seconds.    This'll be a piece of cake.               │
  └────────────────────────────────────────────────────────────────────┘*/
numeric digits 1000000000       /*just in case the universe slows down. */

                                /*this version of a DO loop increments J*/
         do j=1                 /*Sir Eddington's number, then a googol.*/
         say j                  /*first, destroy some electrons.        */
         end
say 42                          /*(see below for explanation of 42.)    */
exit

/*This REXX program (as it will be limited to the NUMERIC DIGITS above, */
/*will only count up to  1000000000000000000000000000000000000000000... */
/*000000000000000000000000000000000000000000000000000000000000000000000 */
/*  ... for another (almost) one billion more zeroes  (then subtract 1).*/

/*if we can count  1,000  times faster than the fastest PeeCee, and we  */
/*started at the moment of the big-bang, we'd be at only  1.72e+28,  so */
/*we still have a little ways to go, eh?                                */

/*To clarify, we'd be  28 zeroes  into a million zeroes.   If PC's get  */
/*1,000  times faster again,  that would be  31  zeroes into a million. */

/*It only took   Deep Thought  7.5  million years  to come up with the  */
/*answer to everything  (and it double-checked the answer).  It was  42.*/
```



## Ring


```ring

size = 10

for n = 1 to size
    see n + nl
next
see nl

for n in [1:size]
    see n + nl
next
see nl

i = n
while n <= size
      see n + nl
      n = n + 1
end

```



## Ruby


```ruby
1.step{|n| puts n}
```


The step method of Numeric takes two optional arguments. The limit defaults to infinity, the step size to 1.

Ruby does not limit the size of integers.


## Run BASIC


```runbasic
while 1
i = i + 1
print i
wend
```

Eventually as it gets larger it becomes a floating point.


## Rust

{{works with|Rust 1.2}}

```rust
fn main() {
    for i in 0.. {
        println!("{}", i);
    }
}
```



Looping endlessly:

```rust
extern crate num;

use num::bigint::BigUint;
use num::traits::{One,Zero};

fn main() {
    let mut i: BigUint = BigUint::one();
    loop {
        println!("{}", i);
        i = i + BigUint::one();
    }
}
```



## Salmon


Salmon has built-in unlimited-precision integer arithmetic, so these examples will all continue printing decimal values indefinitely, limited only by the amount of memory available (it requires O(log(n)) bits to store an integer n, so if your computer has 1 GB of memory, it will count to a number with on the order of <math>2^{80}</math> digits).


```Salmon
iterate (i; [0...+oo])
    i!;
```


or


```Salmon
for (i; 0; true)
    i!;
```


or


```Salmon
variable i := 0;
while (true)
  {
    i!;
    ++i;
  };
```



## Scala


```scala>Stream from 1 foreach println</lang



## Scheme



```scheme

(let loop ((i 1))
  (display i) (newline)
  (loop (+ 1 i)))

```


Scheme does not limit the size of numbers.


## Seed7

Limit 2147483647:

```seed7
$ include "seed7_05.s7i";

  const proc: main is func
    local
      var integer: number is 0;
    begin
      repeat
        incr(number);
        writeln(number);
      until number = 2147483647;
    end func;
```

"Forever":

```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

  const proc: main is func
    local
      var bigInteger: number is 1_;
    begin
      repeat
        writeln(number);
        incr(number);
      until FALSE;
    end func;
```



## Sidef

No limit:

```ruby
{|i| say i } * Math.inf;
```



## Smalltalk


```smalltalk
i := 0.
[
   Stdout print:i; cr.
   i := i + 1
] loop
```

will run forever.


## SSEM

Since we have no Add instruction, we subtract -1 on each iteration instead of adding 1. The same -1 also serves as a jump target, taking advantage of a quirk of the SSEM architecture (the Current Instruction counter is incremented after the instruction has been executed, not before—so <tt>GOTO address</tt> has to be coded as <tt>GOTO address - 1</tt>).

```ssem
01000000000000010000000000000000   0. Sub. 2     acc -= -1
01000000000000000000000000000000   1. 2 to CI    goto -1 + 1
11111111111111111111111111111111   2. -1
```



## SuperCollider

The SuperCollider language has a 32-bit signed int, and a 64 bit signed float. Instead of locking the interpreter with an infinite loop, we post the values over time.

```SuperCollider

i = Routine { inf.do { |i| i.yield } }; // return all integers, represented by a 64 bit signed float.
j = { inf.do { i.next.postln; 0.01.wait } }; // this prints them incrementally
j.play;

```




## Swift


```Swift
var i = 0
while true {
    println(i++)
}
```



## Standard ML


This will print up to Int.maxInt and then raise an Overflow exception.  On a 32 bit machine
the max is 1073741823.  Alternatively you could use Int64.int (64 bit) or
IntInf.int (arbitrary precision).


```sml
let
  fun printInts(n) =
    (
      print(Int.toString(n) ^ "\n");
      printInts(n+1)
    )
in
  printInts(1)
end;
```


{{out}}

```txt
1
2
3
...
1073741821
1073741822
1073741823

uncaught exception Overflow [overflow]
  raised at: <file intSeq.sml>
```



## Tcl


```tcl
package require Tcl 8.5
while true {puts [incr i]}
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
LOOP n=0,999999999
n=n+1
ENDLOOP
```



## UNIX Shell



```sh
#!/bin/sh
num=0
while true; do
  echo $num
  num=`expr $num + 1`
done
```



## Ursa


```ursa
#
# integer sequence
#

# declare an int and loop until it overflows
decl int i
set i 1
while true
        out i endl console
        inc i
end while
```



## Vala


```vala

uint i = 0;
while (++i < uint.MAX)
	stdout.printf("%u\n", i);

```



## Visual Basic .NET

Visual Basic .NET supports an unsigned, 64 bit Integer (maxing out at a <i>whopping</i> 9 223 372 036 854 775 807), however, this is not an intrinsic type, it is a structure that is <i><b>not</b></i> supported by the CLS (Common Language Specification).

The CLS supported type (also a structure) is <i>Decimal</i> (an even more impressive range from positive 79 228 162 514 264 337 593 543 950 335 to negative 79 228 162 514 264 337 593 543 950 335), I have used a standard CLS <i>Integer</i> intrinsic type (from -2 147 483 648 through 2 147 483 647).

Note that attempting to store any value larger than the maximum value of any given type (say 2 147 483 648 for an Integer) will result in an OverflowException being thrown (<i>"Arithmetic operation resulted in an overflow."</i>)


```vbnet
    For i As Integer = 0 To Integer.MaxValue
      Console.WriteLine(i)
    Next
```



### Arbitrarily large numbers

One could use the '''System.Numerics''' library as the C# example did, or one can do the following.<br/>A list of Long Integers is maintained as the incremented number.  As the incremented value approaches the maximum allowed (''base'') in the first element of ''ar'', a new item is inserted at the beginning of the list to extend the incremented number.  The process has the limitation of when the ''ar'' array is enlarged to the point where the program exhausts the available memory, it ought to indicate failure and terminate.  It is my understanding that a '''List''' count is backed by an '''Integer.MaxValue''' limitation and there may also be a 2 GB per object limitation involved.  Since writing to the Console is such a slow process, I lack the patience to wait for the program (as written) to fail.  If the program is tweaked to fail early, the practical limit seems to be a number 2,415,919,086 digits in length.

```vbnet
Imports System.Console

Module Module1

    Dim base, b1 As Long, digits As Integer, sf As String, st As DateTime,
        ar As List(Of Long) = {0L}.ToList, c As Integer = ar.Count - 1

    Sub Increment(n As Integer)
        If ar(n) < b1 Then
            ar(n) += 1
        Else
            ar(n) = 0 : If n > 0 Then
                Increment(n - 1)
            Else
                Try
                    ar.Insert(0, 1L) : c += 1
                Catch ex As Exception
                    WriteLine("Failure when trying to increase beyond {0} digits", CDbl(c) * digits)
                    TimeStamp("error")
                    Stop
                End Try
            End If
        End If
    End Sub

    Sub TimeStamp(cause As String)
        With DateTime.Now - st
            WriteLine("Terminated by {5} at {0} days, {1} hours, {2} minutes, {3}.{4} seconds",
                      .Days, .Hours, .Minutes, .Seconds, .Milliseconds, cause)
        End With
    End Sub

    Sub Main(args As String())
        digits = Long.MaxValue.ToString.Length - 1
        base = CLng(Math.Pow(10, digits)) : b1 = base - 1
        base = 10 : b1 = 9
        sf = "{" & base.ToString.Replace("1", "0:") & "}"
        st = DateTime.Now
        While Not KeyAvailable
            Increment(c) : Write(ar.First)
            For Each item In ar.Skip(1) : Write(sf, item) : Next : WriteLine()
        End While
        TimeStamp("keypress")
    End Sub
End Module
```

{{out}}

```txt
1
2
3
...
10267873
10267874
10267875
Terminated by keypress at 0 days, 0 hours, 30 minutes, 12.980 seconds
```



## WDTE


```WDTE>let s =
 import 'stream';

s.new 0 (+ 1)
-> s.map (io.writeln io.stdout)
-> s.drain
;
```


WDTE's number type is, at the time of writing, backed by Go's <code>float64</code> type, so all of the same limitations that apply there apply here. Also, this should '''not''' be run in the WDTE playground, as it will run with no output until the browser crashes or is killed.


## XLISP


```lisp
(defun integer-sequence-from (x)
	(print x)
	(integer-sequence-from (+ x 1)) )

(integer-sequence-from 1)
```



## XPL0


```XPL0
\Displays integers up to 2^31-1 = 2,147,483,647
code CrLf=9, IntOut=11;
int N;
[N:= 1;
repeat  IntOut(0, N);  CrLf(0);
        N:= N+1;
until   N<0;
]
```



## zkl


```zkl
[1..].pump(Console.println)  // eager
m:=(1).MAX; [1..m].pump(Console.println)  // (1).MAX is 9223372036854775807
[1..].pump(100,Console.println)  // lazy
```



[[Category:Iteration]]
