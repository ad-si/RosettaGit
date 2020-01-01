+++
title = "Formatted numeric output"
description = ""
date = 2019-10-10T16:05:49Z
aliases = []
[extra]
id = 1934
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Text processing]]
[[Category:Simple]]

;Task:
Express a number in decimal as a fixed-length string with leading zeros.


For example, the number   '''7.125'''   could be expressed as   '''00007.125'''.





## 8th


```forth

7.125 "%09.3f" s:strfmt
. cr

```

{{out}}

```txt

 00007.125

```



## Ada


```ada
with Ada.Text_Io.Editing; use Ada.Text_Io.Editing;
with Ada.Text_Io; use Ada.Text_Io;

procedure Zero_Fill is
   Pic_String: String := "<999999.99>";
   Pic : Picture := To_Picture(Pic_String);
   type Money is delta 0.01 digits 8;
   package Money_Output is new Decimal_Output(Money);
   use Money_Output;

   Value : Money := 37.25;
begin
   Put(Item => Value, Pic => Pic);
end Zero_Fill;
```

{{out}}

```txt

 000037.25

```



## Aime


```aime
o_form("/w9s0/\n", 7.125);
o_form("/w12d6p6/\n", -12.0625);
o_form("/w12d6p6/\n", 7.125);
```

{{out}}

```txt
00007.125
  -12.0625
    7.125
```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
main:(
  REAL r=exp(pi)-pi;
  print((r,newline));
  printf(($g(-16,4)l$,-r));
  printf(($g(-16,4)l$,r));
  printf(($g( 16,4)l$,r));
  printf(($g( 16,4,1)l$,r));
  printf(($-dddd.ddddl$,-r));
  printf(($-dddd.ddddl$,r));
  printf(($+dddd.ddddl$,r));
  printf(($ddddd.ddddl$,r));
  printf(($zzzzd.ddddl$,r));
  printf(($zzzz-d.ddddl$,r));
  printf(($zzzz-d.ddddedl$,r));
  printf(($zzzz-d.ddddeddl$,r));
  printf(($4z-d.4de4dl$,r))
)
```

{{out}}

```txt

+1.99990999791895e  +1
        -19.9991
         19.9991
        +19.9991
+19999099.979e-6
-0019.9991
 0019.9991
+0019.9991
00019.9991
00019.9991
    19.9991
     1.9999e1
     1.9999e01
     1.9999e0001

```



## AmigaE

The function RealF can be used to convert a floating point value into a string, with a specified number of decimal digits.
But to fit the string into a greater container prepending 0 we must write our own function.
(The one here proposed has no a flag for the alignment of the result inside the containing string)

```amigae
PROC newRealF(es, fl, digit, len=0, zeros=TRUE)
  DEF s, t, i
  IF (len = 0) OR (len < (digit+3))
    RETURN RealF(es, fl, digit)
  ELSE
    s := String(len)
    t := RealF(es, fl, digit)
    FOR i := 0 TO len-EstrLen(t)-1 DO StrAdd(s, IF zeros THEN '0' ELSE ' ')
    StrAdd(s, t)
    StrCopy(es, s)
    DisposeLink(s)
    DisposeLink(t)
  ENDIF
ENDPROC es

PROC main()
  DEF s[100] : STRING
  WriteF('\s\n', newRealF(s, 7.125, 3,9))
ENDPROC
```


== {{header|APL}} ==


```apl
      'ZF15.9' ⎕FMT 7.125
00007.125000000
```


APL's <tt>⎕FMT</tt> is similar to C's <tt>printf</tt> (only it operates on arrays).


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program formatNum.s   */
/* use C library printf  ha, ha, ha !!! */
/* Constantes               */
.equ EXIT,   1                         @ Linux syscall
/* Initialized data */
.data
szFormat1:         .asciz " %09.3f\n"
.align 4
sfNumber:          .double  0f-7125E-3
sfNumber1:         .double  0f7125E-3

/* UnInitialized data */
.bss
.align 4

/*  code section */
.text
.global main
main:                                   @ entry of program
    push {fp,lr}                        @ saves registers

    ldr r0,iAdrszFormat1                @ format
    ldr r1,iAdrsfNumber                 @ number address
    ldr r2,[r1]                         @ load first 4 bytes
    ldr r3,[r1,#4]                      @ load last 4 bytes
    bl printf                           @ call C function !!!
    ldr r0,iAdrszFormat1
    ldr r1,iAdrsfNumber1
    ldr r2,[r1]
    ldr r3,[r1,#4]
    bl printf



100:                                    @ standard end of the program
    mov r0, #0                          @ return code
    pop {fp,lr}                         @restaur  registers
    mov r7, #EXIT                       @ request to exit program
    swi 0                               @ perform the system call

iAdrszFormat1:           .int szFormat1
iAdrsfNumber:            .int sfNumber
iAdrsfNumber1:           .int sfNumber1



```



## AWK


```awk
BEGIN {
  r=7.125
  printf " %9.3f\n",-r
  printf " %9.3f\n",r
  printf " %-9.3f\n",r
  printf " %09.3f\n",-r
  printf " %09.3f\n",r
  printf " %-09.3f\n",r
}
```


Same output as the C code.


## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276467.html#276467 forum]

```AutoHotkey
MsgBox % pad(7.25,7)  ; 0007.25
MsgBox % pad(-7.25,7) ; -007.25

pad(x,len) { ; pad with 0's from left to len chars
   IfLess x,0, Return "-" pad(SubStr(x,2),len-1)
   VarSetCapacity(p,len,Asc("0"))
   Return SubStr(p x,1-len)
}
```



## BaCon

BaCon can use C style <tt>printf</tt> format specifiers.


```freebasic
' Formatted numeric output
n = 7.125
PRINT n FORMAT "%09.3f\n"
```


{{out}}

```txt
prompt$ ./formatted
00007.125
```



## BBC BASIC


```bbcbasic
      PRINT FNformat(PI, 9, 3)
      PRINT FNformat(-PI, 9, 3)
      END

      DEF FNformat(n, sl%, dp%)
      LOCAL @%
      @% = &1020000 OR dp% << 8
      IF n >= 0 THEN
        = RIGHT$(STRING$(sl%,"0") + STR$(n), sl%)
      ENDIF
      = "-" + RIGHT$(STRING$(sl%,"0") + STR$(-n), sl%-1)
```

{{out}}

```txt

00003.142
-0003.142

```



## bc

First define a custom function for numeric output.


```bc
/*
 * Print number n, using at least c characters.
 *
 * Different from normal, this function:
 *  1. Uses the current ibase (not the obase) to print the number.
 *  2. Prunes "0" digits from the right, so p(1.500, 1) prints "1.5".
 *  3. Pads "0" digits to the left, so p(-1.5, 6) prints "-001.5".
 *  4. Never prints a newline.
 *
 * Use an assignment, as t = p(1.5, 1), to discard the return value
 * from this function so that bc not prints the return value.
 */
define p(n, c) {
	auto d, d[], f, f[], i, m, r, s, v
	s = scale	/* Save original scale. */

	if (n < 0) {
		"-"	/* Print negative sign. */
		c -= 1
		n = -n	/* Remove negative sign from n. */
	}

	/* d[] takes digits before the radix point. */
	scale = 0
	for (m = n / 1; m != 0; m /= 10) d[d++] = m % 10

	/* f[] takes digits after the radix point. */
	r = n - (n / 1)		/* r is these digits. */
	scale = scale(n)
	f = -1			/* f counts the digits of r. */
	for (m = r + 1; m != 0; m /= 10) f += 1
	scale = 0
	r = r * (10 ^ f) / 1	/* Remove radix point from r. */
	if (r != 0) {
		while (r % 10 == 0) {	/* Prune digits. */
			f -= 1
			r /= 10
		}
		for (i = 0; i < f; i++) {
			f[i] = r % 10
			r /= 10
		}
	}

	/* Pad "0" digits to reach c characters. */
	c -= d
	if (f > 0) c -= 1 + f
	for (1; c > 0; c--) "0"		/* Print "0". */

	/* i = index, m = maximum index, r = digit to print. */
	m = d + f
	for (i = 1; i <= m; i++) {
		if (i <= d) r = d[d - i]
		if (i > d) r = f[m - i]
		if (i == d + 1) "."	/* Print radix point. */

		v = 0
		if (r == v++) "0"	/* Print digit. */
		if (r == v++) "1"
		if (r == v++) "2"	/* r == 2 might not work, */
		if (r == v++) "3"	/* unless ibase is ten.   */
		if (r == v++) "4"
		if (r == v++) "5"
		if (r == v++) "6"
		if (r == v++) "7"
		if (r == v++) "8"
		if (r == v++) "9"
		if (r == v++) "A"
		if (r == v++) "B"
		if (r == v++) "C"
		if (r == v++) "D"
		if (r == v++) "E"
		if (r == v++) "F"
	}

	scale = s  /* Restore original scale. */
}
```


Then use this function to print 7.125 with 9 characters.


```bc
x = 7.125
"Decimal: "; t = p(x, 9); "
"
ibase = 16
"Hexadecimal: "; t = p(x, 9); "
"
ibase = 2
"Binary: "; t = p(x, 1001); "
"
quit
```


{{out}}

```txt
Decimal: 00007.125
Hexadecimal: 0000007.2
Binary: 00111.001
```


## C#

```c#

class Program
    {


        static void Main(string[] args)
        {

            float myNumbers = 7.125F;

            string strnumber = Convert.ToString(myNumbers);

            Console.WriteLine(strnumber.PadLeft(9, '0'));

            Console.ReadLine();
        }




    }

```



## C


```c
#include <stdio.h>
main(){
  float r=7.125;
  printf(" %9.3f\n",-r);
  printf(" %9.3f\n",r);
  printf(" %-9.3f\n",r);
  printf(" %09.3f\n",-r);
  printf(" %09.3f\n",r);
  printf(" %-09.3f\n",r);
  return 0;
}
```

{{out}}
    -7.125
     7.125
 7.125
 -0007.125
 00007.125
 7.125


## C++


```cpp
#include <iostream>
#include <iomanip>

int main()
{
  std::cout << std::setfill('0') << std::setw(9) << std::fixed << std::setprecision(3) << 7.125 << std::endl;
  return 0;
}
```



## Clojure


{{trans|Common Lisp}} Using cl format strings

```lisp
(cl-format true "~9,3,,,'0F" 7.125)
```


{{trans|java}} Using java format strings

```lisp
(printf "%09.3f" 7.125) ; format works the same way (without side the effect of printing)
```



## COBOL

This is actually the easiest kind of numeric output to achieve in COBOL, because it requires no adjustments from the way numbers are stored internally (in fixed-point decimal). Each variable declaration requires a <tt>PIC</tt> or <tt>PICTURE</tt> clause describing the kind of data that will be stored there. In this case, we have <tt>9</tt> (a decimal digit), repeated five times; then <tt>V</tt>, the decimal point (cf. French <i>virgule</i>); and then three more decimal digits. Other terms that can appear in <tt>PICTURE</tt> clauses include <tt>A</tt> (a letter of the alphabet), <tt>X</tt> (a character), and <tt>Z</tt> (a decimal digit to be printed with leading spaces instead of leading zeros).

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. NUMERIC-OUTPUT-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-EXAMPLE.
    05 X            PIC  9(5)V9(3).
PROCEDURE DIVISION.
    MOVE     7.125  TO   X.
    DISPLAY  X      UPON CONSOLE.
    STOP RUN.
```

{{out}}

```txt
00007.125
```



## Common Lisp


```lisp
(format t "~9,3,,,'0F" 7.125)
```



## D


```d
import std.stdio;

void main() {
    immutable r = 7.125;
    writefln(" %9.3f",  -r);
    writefln(" %9.3f",   r);
    writefln(" %-9.3f",  r);
    writefln(" %09.3f", -r);
    writefln(" %09.3f",  r);
    writefln(" %-09.3f", r);
}
```

{{out}}

```txt
    -7.125
     7.125
 7.125
 -0007.125
 00007.125
 7.125
```



## dc

{{trans|bc}}

First define a custom function for numeric output.


```dc
[*
 * (n) (c) lpx
 * Print number n, using at least c characters.
 *
 * Different from normal, this function:
 *  1. Uses the current ibase (not the obase) to print the number.
 *  2. Prunes "0" digits from the right, so [1.500 1 lxp] prints "1.5".
 *  3. Pads "0" digits to the left, so [_1.5 6 lxp] prints "-001.5".
 *  4. Never prints a newline.
 *]sz
[
 Sc Sn          [Local n, c = from stack.]sz
 K Ss           [Local s = original scale.]sz
 [Reserve local variables D, F, I, L.]sz
 0 SD 0 SF 0 SI 0 SL

 [              [If n < 0:]sz
  [-]P           [Print negative sign.]sz
  lc 1 - sc      [Decrement c.]sz
  0 ln - sn      [Negate n.]sz
 ]sI 0 ln <I

 [*
  * Array D[] takes digits before the radix point.
  *]sz
 0 k            [scale = 0]sz
 0 Sd           [Local d = 0]sz
 ln 1 /         [Push digits before radix point.]sz
 [              [Loop to fill D[]:]sz
  d 10 % ld :D   [D[d] = next digit.]sz
  ld 1 + sd      [Increment d.]sz
  10 /           [Remove digit.]sz
  d 0 !=L        [Loop until no digits.]sz
 ]sL d 0 !=L
 sz             [Pop digits.]sz

 [*
  * Array F[] takes digits after the radix point.
  *]sz
 ln ln 1 / -    [Push digits after radix point.]sz
 d X k          [scale = enough.]sz
 _1 Sf          [Local f = -1]sz
 d 1 +          [Push 1 + digits after radix point.]sz
 [              [Loop to count digits:]sz
  lf 1 + sf      [Increment f.]sz
  10 /           [Remove digit.]sz
  d 0 !=L        [Loop until no digits.]sz
 ]sL d 0 !=L
 sz             [Pop 1 + digits.]sz
 0 k            [scale = 0]sz
 10 lf ^ * 1 /  [Remove radix point from digits.]sz
 [              [Loop to prune digits:]sz
  lf 1 - sf      [Decrement f.]sz
  10 /           [Remove digit.]sz
  d 10 % 0 =L    [Loop while last digit is 0.]sz
 ]sL d 10 % 0 =L
 0 Si           [Local i = 0]sz
 [              [Loop to fill F[]:]sz
  d 10 % li :F   [F[i] = next digit.]sz
  10 /           [Remove digit.]sz
  li 1 + si      [Increment i.]sz
  lf li <L       [Loop while i < f.]sz
 ]sL lf li <L
 sz             [Pop digits.]sz

 lc ld -        [Push count = c - d.]sz
 [              [If f > 0:]sz
  1 lf + -       [Subtract 1 radix point + f from count.]sz
 ]sI 0 lf >I
 [              [Loop:]sz
  [0]P           [Print a padding "0".]sz
  1 -            [Decrement count.]sz
  d 0 <L         [Loop while count > 0.]sz
 ]sL d 0 <L
 sz             [Pop count.]sz

 [              [Local function (digit) lPx:]sz
  [              [Execute:]sz
   [*
    * Push the string that matches the digit.
    *]sz
   [[0] 2Q]sI d 0 =I  [[1] 2Q]sI d 1 =I  [[2] 2Q]sI d 2 =I  [[3] 2Q]sI d 3 =I
   [[4] 2Q]sI d 4 =I  [[5] 2Q]sI d 5 =I  [[6] 2Q]sI d 6 =I  [[7] 2Q]sI d 7 =I
   [[8] 2Q]sI d 8 =I  [[9] 2Q]sI d 9 =I  [[A] 2Q]sI d A =I  [[B] 2Q]sI d B =I
   [[C] 2Q]sI d C =I  [[D] 2Q]sI d D =I  [[E] 2Q]sI d E =I  [[F] 2Q]sI d F =I
   [?]            [Else push "?".]sz
  ]x
  P              [Print the string.]sz
  sz             [Pop the digit.]sz
 ]SP
 ld             [Push counter = d.]sz
 [              [Loop:]sz
  1 -            [Decrement counter.]sz
  d ;D lPx       [Print digit D[counter].]sz
  d 0 <L         [Loop while counter > 0.]sz
 ]sL d 0 <L
 sz             [Pop counter.]sz
 [              [If f > 0:]sz
  [.]P           [Print radix point.]sz
  lf              [Push counter = f.]sz
  [              [Loop:]sz
   1 -            [Decrement counter.]sz
   d ;F lPx       [Print digit F[counter].]sz
   d 0 <L         [Loop while counter > 0.]sz
  ]sL d 0 <L
  sz             [Pop counter.]sz
 ]sI 0 lf >I

 [Restore variables n, c, d, f, D, F, L, I, P.]sz
 Lnsz Lcsz Ldsz Lfsz LDsz LFsz LLsz LIsz LPsz
 Ls k           [Restore variable s. Restore original scale.]sz
]sp
```


Then use this function to print 7.125 with 9 characters:


```dc
7.125 sx
[Decimal: ]P lx 9 lpx [
]P 16 i [Hexadecimal: ]P lx 9 lpx [
]P 2 i [Binary: ]P lx 9 lpx [
]P
```


{{out}}

```txt
Decimal: 00007.125
Hexadecimal: 0000007.2
Binary: 00111.001
```



## Delphi



```Delphi

program FormattedNumericOutput;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
  fVal = 7.125;

begin
  Writeln(FormatFloat('0000#.000',fVal));
  Writeln(FormatFloat('0000#.0000000',fVal));
  Writeln(FormatFloat('##.0000000',fVal));
  Writeln(FormatFloat('0',fVal));
  Writeln(FormatFloat('#.#E-0',fVal));
  Writeln(FormatFloat('#,##0.00;;Zero',fVal));
  Readln;
end.

```


{{out}}

```txt

00007.125
00007.1250000
7.1250000
7
7.1E0
7.13

```



## Eiffel

{{works with|Eiffel Studio|6.6}}


```Eiffel

note
	description : "{
2 Examples are given.
The first example uses the standard library's FORMAT_DOUBLE class.
The second example uses the AEL_PRINTF class from the freely available
Amalasoft Eiffel Library (AEL).

See additional comments in the code.
}"

class APPLICATION

inherit
	AEL_PRINTF -- Optional, see below

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		do
			print_formatted_std (7.125)
			print_formatted_ael (7.125)
		end

	--|--------------------------------------------------------------

	print_formatted_std (v: REAL_64)
			-- Print the value 'v' as a zero-padded string in a fixed
			-- overall width of 9 places and, with a precision of
			-- to 3 places to the right of the decimal point.
			-- Use the FORMAT_DOUBLE class from the standard library
		local
			fmt: FORMAT_DOUBLE
		do
			create fmt.make (9, 3)
			fmt.zero_fill
			print (fmt.formatted (v) + "%N")
		end

	--|--------------------------------------------------------------

	print_formatted_ael (v: REAL_64)
			-- Print the value 'v' as a zero-padded string in a fixed
			-- overall width of 9 places and, with a precision of
			-- to 3 places to the right of the decimal point.
			-- Use the AEL_PRINTF class from the Amalasoft Eiffel Library
			-- freely available from www.amalasoft.com
		do
			-- printf accepts a format string and an argument list
			-- The argument list is a container (often a manifest
			-- array) of values corresponding to the type of the format
			-- specified in the format string argument.
			-- When only one argument is needed, then there is also the
			-- option to use just the value, without the container.
			-- In this example, the line would be:
			--   printf ("%%09.3f%N", v)
			-- The more deliberate form is used in the actual example,
			-- as it is more representative of common usage, when there
			-- are multiple value arguments.

			printf ("%%09.3f%N", << v >>)
		end

end

```



## Elixir



```elixir
n = 7.125
:io.fwrite "~f~n", [n]
:io.fwrite "~.3f~n", [n]
:io.fwrite "~9f~n", [n]
:io.fwrite "~9.3f~n", [n]
:io.fwrite "~9..0f~n", [n]
:io.fwrite "~9.3.0f~n", [n]
:io.fwrite "~9.3._f~n", [n]
:io.fwrite "~f~n", [-n]
:io.fwrite "~9.3f~n", [-n]
:io.fwrite "~9.3.0f~n", [-n]
:io.fwrite "~e~n", [n]
:io.fwrite "~12.4e~n", [n]
:io.fwrite "~12.4.0e~n", [n]
```


{{out}}

```txt

7.125000
7.125
 7.125000
    7.125
07.125000
00007.125
____7.125
-7.125000
   -7.125
000-7.125
7.12500e+0
    7.125e+0
00007.125e+0

```



## Emacs Lisp


```Lisp
(format "%09.3f" 7.125)
=>
"00007.125"
```


<code>format</code> is similar to C <code>sprintf</code>.  See [http://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html GNU Elisp manual on Formatting Strings].


## Erlang

Built in
{{out}}

```txt

14> io:fwrite("~9.3.0f~n", [7.125]).
00007.125

```



## ERRE


```ERRE
PROGRAM FORMATTED

PROCEDURE FORMATTED_PRINT(N,LENGTH,DEC_PLACES->FP$)

    LOCAL I,C$,NN$

    FORMAT$=STRING$(LENGTH,"#")+"."

    FOR I=1 TO DEC_PLACES DO
       FORMAT$=FORMAT$+"#"
    END FOR

    OPEN("O",1,"FORMAT.$$$")
       WRITE(#1,FORMAT$;N)
    CLOSE(1)

    OPEN("I",1,"FORMAT.$$$")
       INPUT(LINE,#1,N$)
    CLOSE(1)

    ! add leading zeros
    FOR I=1 TO LEN(N$) DO
       C$=MID$(N$,I,1)
       IF C$=" " OR C$="%" THEN NN$=NN$+"0" ELSE NN$=NN$+C$
    END FOR

    FP$=RIGHT$("000000000000"+NN$,LENGTH) ! chop to required length

END PROCEDURE

BEGIN

   PRINT(CHR$(12);) ! CLS

   FOR I=1 TO 10 DO
     N=RND(1)*10^(INT(10*RND(1))-2)
     FORMATTED_PRINT(N,16,5->FP$)
     PRINT("Raw number =";N;TAB(30);"Using custom function =";FP$)
   END FOR

END PROGRAM
```

{{out}}

```txt

Raw number = 1213.501        Using custom function =0000001213.50100
Raw number = 86886.11        Using custom function =0000086886.11000
Raw number = 7.98853E-03     Using custom function =0000000000.00799
Raw number = 49.03128        Using custom function =0000000049.03128
Raw number = 1072496         Using custom function =0001072496.00000
Raw number = 703.8703        Using custom function =0000000703.87030
Raw number = 9.711614        Using custom function =0000000009.71161
Raw number = 9561278         Using custom function =0009561278.00000
Raw number = 534.9367        Using custom function =0000000534.93670
Raw number = 67121.88        Using custom function =0000067121.88000

```



## Euphoria


```euphoria
constant r = 7.125
printf(1,"%9.3f\n",-r)
printf(1,"%9.3f\n",r)
printf(1,"%-9.3f\n",r)
printf(1,"%09.3f\n",-r)
printf(1,"%09.3f\n",r)
printf(1,"%-09.3f\n",r)
```


{{out}}

```txt

    -7.125
     7.125
 7.125
 -0007.125
 00007.125
 7.125

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

#Include "vbcompat.bi"

Dim s As String  = Format(7.125, "00000.0##")
Print s
Sleep
```


{{out}}

```txt

00007.125

```



## Free Pascal

''See [[#Pascal|Pascal]]''

=={{header|F_Sharp|F#}}==

```fsharp
printfn "%09.3f" 7.125f
```



## Factor


```factor
USE: formatting
7.125 "%09.3f\n" printf
```

{{out}}

```txt

00007.125

```



## Fantom



```fantom

class Main
{
  public static Void main()
  {
    echo (7.125.toStr.padl(9, '0'))
  }
}

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Formatted_numeric_output this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

Forth has a rather rich set of number formatting words,
which makes formatted output very flexible but sometime cumbersome.

Here one way to generate the required output.
Note that the number generated is NOT truncated to the field width.
If you wish to truncate the number, remove #s and 1- from the definition.
(The 1- is necessary because #s always generates at least one digit, even if it's zero.)


```forth
\ format 'n' digits of the double word 'd'
: #n ( d n -- d )  0 ?do # loop ;

\ ud.0 prints an unsigned double
: ud.0 ( d n -- )  <# 1- #n #s #> type ;

\ d.0 prints a signed double
: d.0 ( d n -- )  >r tuck dabs <# r> 1- #n #s rot sign #> type ;
```


Usage example:


```forth>Type:    123 s
d  8 ud.0
Result:  00000123 ok
Type:    -123 s>d 8 d.0
Result:  -00000123 ok
```

==
## Detail
==
Forth's number formatting words are different than many other languages because they are active code rather than using a pattern string.  This small set of seven routines ( >DIGIT <#   #>  #  #S  HOLD SIGN )  allow arbitrary number formatting of double precision and single precision numbers. The number is created in a "hold' buffer the output is typically a Forth style stack-string consisting of an address and a length.

Typical of Forth the using the formatting routines means putting things in reverse order.
We are also free to create a mnemonic name that gives a reminder at how numbers will appear.

To replicate the example for this task we could write:

```txt
: '.'   [CHAR] . HOLD ; \ HOLD inserts a character into the number string
\                                    right side .  left side
: 0000#.###  ( d -- addr len) DABS <#    # # # '.' # # # # #    #> ;
```


At the console we can input a double number, execute the format routine and type the resulting string.

```txt
7.125 0000#.### TYPE 000007.125 ok
```



## Fortran

{{works with|Fortran|90 and later}}
Using standard data edit descriptors it is only possible to precede Integer data with leading zeros.

```fortran
INTEGER :: number = 7125
WRITE(*,"(I8.8)") number   ! Prints 00007125
```


### On the other hand

One can engage in trickery via FORMAT statements, in particular the T format option. Unlike actual tab settings which on a typewriter go to a particular column following, T''n'' means go to column ''n''.

```Fortran

      INTEGER IV
      REAL V
      DATA V/7.125/	!A positive number.
      IV = V		!Grab the integer part.
      WRITE (6,1) V,IV
    1 FORMAT (F9.3,T1,I5.5)
      END

```

Output is
 00007.125
This would need adjustment for other sizes, but works as follows: The value part is printed (in the format system's working area) as "bbbb7.125" (b's standing for spaces), then the T1 moves the finger back to column one, and the I5.5 writes out "00007", the .5 addendum to I5 meaning print leading zeroes rather than leading spaces. It does not overwrite the subsequent ".125", and as no further output items appear the deed is done. Only later Fortran offers the addendum feature, but the Tab feature is much older.

Another approach would be to write forth a literal "0000" instead of the integer, but this is less flexible. In the absence of the .5 addendum, write the output to a character string (or equivalent), replace leading spaces by zeroes (watching out for negative numbers), and print the result.


## FutureBasic


```futurebasic

include "ConsoleWindow"

print using "0000#.###"; 7.125

```

Output:

```txt

00007.125

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=4f64ab96d0f97df5876b76ab0431b302 Click this link to run this code]'''

```gambas
Public Sub Main()

Print Format("7.125", "00000.000")

End
```

Output:

```txt

00007.125

```



## gnuplot


```gnuplot
print sprintf("%09.3f", 7.125)
```



## Go


```go
fmt.Printf("%09.3f", 7.125)
```



## Groovy

Solution:

```groovy
printf ("%09.3f", 7.125)
```


{{out}}

```txt
00007.125
```



## Haskell


```haskell
import Text.Printf
main =
  printf "%09.3f" 7.125
```



## hexiscript


```hexiscript
fun format n length
  let n tostr n
  while len n < length
    let n 0 + n
  endwhile
  println n
endfun

format 7.125 9
```



## HicEst


```hicest
WRITE(ClipBoard, Format='i5.5, F4.3') INT(7.125), MOD(7.125, 1)    ! 00007.125
```



## i


```i

concept FixedLengthFormat(value, length) {
	string = text(abs(value))
	prefix = ""
	sign = ""

	if value < 0
		sign = "-"
	end

	if #string < length
		prefix = "0"*(length-#sign-#string-#prefix)
	end

	return sign+prefix+string
}

software {
	d = 7.125
	print(FixedLengthFormat(d, 9))
	print(FixedLengthFormat(-d, 9))
}

```



## IDL

[[Category:IDL]]


```idl
n = 7.125
print, n, format='(f08.3)'
;==> 0007.125
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link printf

procedure main()

every  r := &pi | -r | 100-r do {
  write(r," <=== no printf")
  every p := "|%r|" | "|%9.3r|" | "|%-9.3r|" | "|%0.3r|" | "|%e|" | "|%d|" do
     write(sprintf(p,r)," <=== sprintf ",p)
}
end
```


{{out}} Abbreviated

```txt
3.141592653589793 <=== no printf
|3.141593| <=== sprintf |%r|
|    3.142| <=== sprintf |%9.3r|
|3.142    | <=== sprintf |%-9.3r|
|3.142| <=== sprintf |%0.3r|
|   3.141593e0| <=== sprintf |%e|
|3| <=== sprintf |%d|
```


{{libheader|Icon Programming Library}} provides [http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf]

=={{header|IS-BASIC}}==
<lang IS-BASIC>100 LET F=7.125
110 PRINT USING "-%%%%%.###":F
```



## J


```j
   'r<0>9.3' (8!:2) 7.125
00007.125
```


[http://www.jsoftware.com/help/dictionary/dx008.htm Documentation on 8!:]


## Java

{{works with|Java|1.5+}}
Stealing printf from C/C++:

```java5
public class Printing{
	public static void main(String[] args){
		double printer = 7.125;
		System.out.printf("%09.3f",printer);//System.out.format works the same way
	}
}
```

{{out}}

```txt
000000007.125
```

Using <code>NumberFormat</code>:

```java5
import java.text.DecimalFormat;
import java.text.NumberFormat;

public class Format {
	public static void main(String[] args){
		NumberFormat numForm = new DecimalFormat();
		numForm.setMinimumIntegerDigits(9);
		//Maximum also available for Integer digits and Fraction digits
		numForm.setGroupingUsed(false);//stops it from inserting commas
		System.out.println(numForm.format(7.125));

		//example of Fraction digit options
		numForm.setMinimumIntegerDigits(5);
		numForm.setMinimumFractionDigits(5);
		System.out.println(numForm.format(7.125));
		numForm.setMinimumFractionDigits(0);
		numForm.setMaximumFractionDigits(2);
		System.out.println(numForm.format(7.125));
		System.out.println(numForm.format(7.135));//rounds to even
	}
}
```

{{out}}

```txt
000000007.125
00007.12500
00007.12
00007.14
```



## JavaScript


```javascript
var n = 123;
var str = ("00000" + n).slice(-5);
alert(str);
```


or, put in browser URL: javascript:n=123;alert(("00000"+n).slice(-5));

Also, a 60-line implementation of <code>sprintf</code> can be found [http://code.google.com/p/sprintf/ here].


## jq

The jq function pp0/1 as defined below is written in accordance with the task
requirements, but no truncation occurs; pp/1 is similar but
is likely to be more useful as the decimal point is aligned if possible.

```jq
def pp0(width):
  tostring
  | if width > length then (width - length) * "0" + . else . end;

# pp(left; right) formats a decimal number to occupy
# (left+right+1) positions if possible,
# where "left" is the number of characters to the left of
# the decimal point, and similarly for "right".
def pp(left; right):
  def lpad: if (left > length) then ((left - length) * "0") + . else . end;
  tostring as $s
  | $s
  | index(".") as $ix
  | ((if $ix then $s[0:$ix] else $s end) | lpad) + "." +
    (if $ix then $s[$ix+1:] | .[0:right] else "" end);
```

'''Examples''':

```jq
(1.0, 12.3, 333.333, 1e6) | pp0(10)
```

produces

```sh
0000000001
00000012.3
000333.333
0001000000
```



```jq
(1.0, 12.3, 333.333, 1e6) | pp(4;2)
```

produces

```sh
0001.
0012.3
0333.33
1000000.
```



## Julia

Julia's <tt>@sprintf</tt> macro provides string formatting that is similar to that of the c function of the same name.  Though easy to use and efficient, <tt>@sprintf</tt> has limited flexibility, as its format specification must be a string literal, precluding its use in dynamic formatting.  Greater flexibility is available via the <tt>Formatting</tt> package, which provides an implementation of Python's format specification mini-language.  This solution demonstrates both of these techniques to provide the leading zero padded floating point format suggested in the task description ("%09.3f").

```Julia
using Printf
test = [7.125, [rand()*10^rand(0:4) for i in 1:9]]

println("Formatting some numbers with the @sprintf macro (using \"%09.3f\"):")
for i in test
    println(@sprintf "    %09.3f" i)
end

using Formatting
println()
println("The same thing using the Formatting package:")
fe = FormatExpr("    {1:09.3f}")
for i in test
    printfmtln(fe, i)
end

```


{{out}}

```txt

Formatting some numbers with the @sprintf macro (using "%09.3f"):
    00007.125
    00001.734
    00903.432
    00000.980
    00002.271
    00559.864
    00105.497
    00069.955
    00046.107
    04970.430

The same thing using the Formatting package:
    00007.125
    00001.734
    00903.432
    00000.980
    00002.271
    00559.864
    00105.497
    00069.955
    00046.107
    04970.430

```



## Kotlin


```scala
// version 1.0.5-2

fun main(args: Array<String>) {
    val num = 7.125
    println("%09.3f".format(num))
}
```


{{out}}

```txt

00007.125

```



## Lasso


```Lasso
7.125 -> asstring(-precision = 3, -padding = 9, -padchar = '0')
```

{{out}}

```txt
00007.125
```



## Liberty BASIC

Custom function builds on the supplied 'print using( "###.###", n)'.

NB no check that this does not truncate high-order digits...
and remember LB calculates with more figures than its normal 'print' displays.

```lb

for i =1 to 10
    n =rnd( 1) *10^( int( 10 *rnd(1)) -2)
    print "Raw number ="; n; "Using custom function ="; FormattedPrint$( n, 16, 5)
next i
end

function FormattedPrint$( n, length, decPlaces)
    format$ ="#."
    for i =1 to decPlaces
        format$ =format$ +"#"
    next i

    n$ =using( format$, n)            '   remove leading spaces if less than 3 figs left of decimal
                                        '   add leading zeros
    for i =1 to len( n$)
        c$ =mid$( n$, i, 1)
        if c$ =" " or c$ ="%" then nn$ =nn$ +"0" else nn$ =nn$ +c$
    next i
    FormattedPrint$ =right$( "000000000000" +nn$, length) '   chop to required length
end function

```

{{out}}

```txt

Raw number =0.16045274      Using custom function =0000000000.16045
Raw number =13221.2247      Using custom function =0000013221.22474
Raw number =738.134167      Using custom function =0000000738.13417
Raw number =5.07495908      Using custom function =0000000005.07496
Raw number =4471738.93      Using custom function =0004471738.92920
Raw number =48.7531874      Using custom function =0000000048.75319
Raw number =0.26086972e-1   Using custom function =0000000000.02609
Raw number =0.86559862      Using custom function =0000000000.86560
Raw number =818579.045      Using custom function =0000818579.04498
Raw number =81.460946       Using custom function =0000000081.46095

```



## Logo

Various collection functions, such as MAP and FILTER,
will work on individual characters of a string when given a word instead of a list.

```logo
to zpad :num :width :precision
  output map [ifelse ? = "| | ["0] [?]] form :num :width :precision
end
print zpad 7.125 9 3  ; 00007.125
```


{{works with|UCB Logo}}
As a debugging feature, you can drop down to [[C]] language printf formatting by giving -1 for the width and a format string for the precision.

```logo
print form 7.125 -1 "|%09.3f|    ; 00007.125
```



## Lua


```lua
function digits(n) return math.floor(math.log(n) / math.log(10))+1 end
function fixedprint(num, digs) --digs = number of digits before decimal point
  for i = 1, digs - digits(num) do
    io.write"0"
  end
  print(num)
end

fixedprint(7.125, 5) --> 00007.125
```



An easier way to do that would be


```Lua

 print(string.format("%09.3d",7.125))

```


## M2000 Interpreter

We can use ? as Print

```M2000 Interpreter

Print str$(7.125,"00000.000")

```



## Maple


```maple
printf("%f", Pi);
    3.141593
printf("%.0f", Pi);
    3
printf("%.2f", Pi);
    3.14
printf("%08.2f", Pi);
    00003.14
printf("%8.2f", Pi);
        3.14
printf("%-8.2f|", Pi);
    3.14    |
printf("%+08.2f", Pi);
    +0003.14
printf("%+0*.*f",8, 2, Pi);
    +0003.14
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
StringTake["000000" <> ToString[7.125], -9]
00007.125
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB>>
 disp(sprintf('%09.3f',7.125))
00007.125
```



## Mercury

<lang>
:- module formatted_numeric_output.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
    io.format("%09.3f\n", [f(7.125)], !IO).

```


=={{header|Modula-3}}==
Modules <tt>IO</tt> and <tt>Fmt</tt> must be imported before use.

```modula3
IO.Put(Fmt.Pad("7.125\n", length := 10, padChar := '0'));
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

import java.text.MessageFormat

sevenPointOneTwoFive = double 7.125

-- using NetRexx Built-In Functions (BIFs)
say Rexx(sevenPointOneTwoFive).format(5, 3).changestr(' ', '0')

-- using Java library constructs
System.out.printf('%09.3f\n', [Double(sevenPointOneTwoFive)])
say MessageFormat.format('{0,number,#00000.###}', [Double(sevenPointOneTwoFive)])

return

```

{{out}}

```txt

00007.125
00007.125
00007.125

```



## Nim


```nim
import strfmt
const r = 7.125
echo r
echo((-r).format("9.3f"))
echo(r.format("9.3f"))
echo((-r).format("09.3f"))
echo(r.format("09.3f"))
```

{{out}}

```txt
7.1250000000000000e+00
   -7.125
    7.125
-0007.125
00007.125
```


=={{header|Oberon-2}}==
Module <code>Out</code> must be imported before use.


```oberon2
Out.Real(7.125, 9, 0);
```


=={{header|Objective-C}}==

```objc
NSLog(@"%09.3f", 7.125);
```

or

```objc
NSLog(@"%@", [NSString stringWithFormat:@"%09.3f", 7.125]);
```



## OCaml


```ocaml
Printf.printf "%09.3f\n" 7.125
```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>MESSAGE
   STRING( 7.125, "99999.999" )
VIEW-AS ALERT-BOX.
```



## Oz

It is possible to set the precision used for float printing
(where "precision" means the total number of digits used).

It doesn't seem to be possible to use leading zeros for printing,
so we implement this manually:

```oz
declare
  fun {PrintFloat X Prec}
     {Property.put 'print.floatPrecision' Prec}
     S = {Float.toString X}
  in
     {Append
      for I in 1..Prec-{Length S}+1 collect:C do {C &0} end
      S}
  end
in
  {System.showInfo {PrintFloat 7.125 8}}
```



## PARI/GP

{{works with|PARI/GP|2.4.3 and above}}

```parigp
printf("%09.4f\n", Pi)
```



## Pascal


```pascal
procedure writeInFixedFormat(n: real);
const
	wholeNumberPlaces = 5;
	fractionalPlaces = 3;
	zeroDigit = '0';
	negative = '-';
var
	signPresent: boolean;
	i: integer;
begin
	// NOTE: This does not catch “negative” zero.
	signPresent := n < 0.0;
	if signPresent then
	begin
		write(negative);
		n := abs(n);
	end;

	// determine number of leading zeros
	i := wholeNumberPlaces;
	if n > 0 then
	begin
		i := i - trunc(ln(n) / ln(10));
	end;

	for i := i - 1 downto succ(ord(signPresent)) do
	begin
		write(zeroDigit);
	end;

	// writes n with
	// - at least 0 characters in total
	// - exactly fractionalPlaces post-radix digits
	// rounded
	write(n:0:fractionalPlaces);
end;
```



## Perl

{{works with|Perl|5.x}}

```perl
printf "%09.3f\n", 7.125;
```



## Perl 6


```perl6
say 7.125.fmt('%09.3f');
```



## Phix


```Phix
printf(1,"%09.3f\n",7.125)
```

{{out}}

```txt

 00007.125

```



## PHP


```php
echo str_pad(7.125, 9, '0', STR_PAD_LEFT);
```

or

```php
printf("%09.3f\n", 7.125);
```



## PicoLisp


```PicoLisp
(pad 9 (format 7125 3))
(pad 9 (format 7125 3 ","))  # European format
```



## PL/I


```PL/I

put edit (X) (p'999999.V999'); /* Western format. */

put edit (X) (p'999999,V999'); /* In European format. */


```


<lang> lz: Proc Options(main);
 /*********************************************************************
 * 10.09.2013 Walter Pachl  one way to treat negative numbers
 * another would be using a Picture of 'S(9)9.V(3)9' or '-(9)9.V(3)9'
 *********************************************************************/
 Call z2lz(1.2);
 Call z2lz(-1.32);
 Call z2lz(123456789.012);
 Call z2lz(-23456789.012);
 Call z2lz(-123456789.012);

 z2lz: Proc(z);
 Dcl z Dec Fixed(15,3); ;
 Dcl s Char(13) Based(addr(p));
 Dcl p  Pic'(9)9.V(3)9';
 p=z;
 If z<0 Then
   If left(s,1)='0' Then substr(s,1,1)='-';
   Else Do;
     Put Skip List(z,'can''t be formatted that way');
     Return;
     End;
 Put Skip List(z,s);
 End;
 End;
```

{{out}}

```txt

             1.200      000000001.200
            -1.320      -00000001.320
     123456789.012      123456789.012
     -23456789.012      -23456789.012
    -123456789.012      can't be formatted that way

```



## Pop11

The task is underspecified, so we present a few alternatives.


```pop11
;;; field of length 12, 3 digits after decimal place
format_print('~12,3,0,`*,`0F', [1299.19]);
;;; prints "00001299.190"
format_print('~12,3,0,`*,`0F', [100000000000000000]);
;;; Since the number does not fit into the field prints "************"
;;; that is stars instead of the number
format_print('~12,3,0,`*,`0F', [-1299.19]);
;;; prints "000-1299.190"
;;; that is _leading zeros_ before sign

format_print('~3,1,12,`0:$', [1299.19]);
;;; prints "00001299.190"
format_print('~3,1,12,`0:$', [-1299.19]);
;;; prints "-0001299.190"
;;; that is sign before leading zeros
format_print('~3,1,12,`0:$', [100000000000000000]);
;;; prints "100000000000000000.000"
;;; that is uses more space if the number does not fit into
;;; fixed width
```



## PowerShell

Using the <code>-f</code> formatting operator and a custom format string:

```powershell
'{0:00000.000}' -f 7.125
```

or by invoking <code>ToString</code> on the number:

```powershell
7.125.ToString('00000.000')
```



## PureBasic

Using RSet() to pad 7.125 with 3 decimals converted to a string, to 8 char length.

```PureBasic
RSet(StrF(7.125,3),8,"0")    ; Will be 0007.125
```



## Python

{{works with|Python|2.5}}
Python has 3 different floating point formatting methods: "%e","%f" & "%g".
The "%g" format is a beautified hybrid of "%e" and "%f".  There is no way of
specifying how many digits appear in the exponent when printed with a format.


```python
from math import pi, exp
r = exp(pi)-pi
print r
print "e=%e f=%f g=%g G=%G s=%s r=%r!"%(r,r,r,r,r,r)
print "e=%9.4e f=%9.4f g=%9.4g!"%(-r,-r,-r)
print "e=%9.4e f=%9.4f g=%9.4g!"%(r,r,r)
print "e=%-9.4e f=%-9.4f g=%-9.4g!"%(r,r,r)
print "e=%09.4e f=%09.4f g=%09.4g!"%(-r,-r,-r)
print "e=%09.4e f=%09.4f g=%09.4g!"%(r,r,r)
print "e=%-09.4e f=%-09.4f g=%-09.4g!"%(r,r,r)
```

{{out}}

```txt

19.9990999792
e=1.999910e+01 f=19.999100 g=19.9991 G=19.9991 s=19.9990999792 r=19.999099979189474!
e=-1.9999e+01 f= -19.9991 g=      -20!
e=1.9999e+01 f=  19.9991 g=       20!
e=1.9999e+01 f=19.9991   g=20       !
e=-1.9999e+01 f=-019.9991 g=-00000020!
e=1.9999e+01 f=0019.9991 g=000000020!
e=1.9999e+01 f=19.9991   g=20       !

```


{{works with|Python|3}}

```python
from math import pi, exp
r = exp(pi)-pi
print(r)
print("e={0:e} f={0:f} g={0:g} G={0:G} s={0!s} r={0!r}!".format(r))
print("e={0:9.4e} f={0:9.4f} g={0:9.4g}!".format(-r))
print("e={0:9.4e} f={0:9.4f} g={0:9.4g}!".format(r))
print("e={0:-9.4e} f={0:-9.4f} g={0:-9.4g}!".format(r))
print("e={0:09.4e} f={0:09.4f} g={0:09.4g}!".format(-r))
print("e={0:09.4e} f={0:09.4f} g={0:09.4g}!".format(r))
print("e={0:-09.4e} f={0:-09.4f} g={0:-09.4g}!".format(r))
```

{{out}}

```txt

19.9990999792
e=1.999910e+01 f=19.999100 g=19.9991 G=19.9991 s=19.9990999792 r=19.999099979189474!
e=-1.9999e+01 f= -19.9991 g=      -20!
e=1.9999e+01 f=  19.9991 g=       20!
e=1.9999e+01 f=19.9991   g=20       !
e=-1.9999e+01 f=-019.9991 g=-00000020!
e=1.9999e+01 f=0019.9991 g=000000020!
e=1.9999e+01 f=19.9991   g=20       !

```



## R


[http://www.stat.ucl.ac.be/ISdidactique/Rhelp/library/base/html/sprintf.html sprintf] brings the printf goodness one expects:


```R>
 sprintf("%f", pi)
[1] "3.141593"
> sprintf("%.3f", pi)
[1] "3.142"
> sprintf("%1.0f", pi)
[1] "3"
> sprintf("%5.1f", pi)
[1] "  3.1"
> sprintf("%05.1f", pi)
[1] "003.1"
> sprintf("%+f", pi)
[1] "+3.141593"
> sprintf("% f", pi)
[1] " 3.141593"
> sprintf("%-10f", pi)# left justified
[1] "3.141593  "
> sprintf("%e", pi)
[1] "3.141593e+00"
> sprintf("%E", pi)
[1] "3.141593E+00"
> sprintf("%g", pi)
[1] "3.14159"
> sprintf("%g",   1e6 * pi) # -> exponential
[1] "3.14159e+06"
> sprintf("%.9g", 1e6 * pi) # -> "fixed"
[1] "3141592.65"
> sprintf("%G", 1e-6 * pi)
[1] "3.14159E-06"
```


formatC also provides C-style string formatting.

```R
formatC(x, width=9, flag="0")
# "00007.125"
```


Other string formatting functions include
 format, prettynum


## Racket



```Racket

-> (displayln (~a 7.125 #:width 9 #:align 'right #:pad-string "0"))
00007.125

```



## REBOL


```REBOL
REBOL [
	Title: "Formatted Numeric Output"
	URL: http://rosettacode.org/wiki/Formatted_Numeric_Output
]

; REBOL has no built-in facilities for printing pictured output.
; However, it's not too hard to cook something up using the
; string manipulation facilities.

zeropad: func [
	"Pad number with zeros or spaces. Works on entire number."
	pad "Number of characters to pad to."
	n "Number to pad."
	/space "Pad with spaces instead."
	/local nn c s
][
	n: to-string n  c: " "  s: ""

	if not space [
		c: "0"
		if #"-" = n/1 [pad: pad - 1  n: copy skip n 1  s: "-"]
	]

        insert/dup n c (pad - length? n)
	insert n s
    n
]

; These tests replicate the C example output.

print [zeropad/space 9 negate 7.125]
print [zeropad/space 9 7.125]
print 7.125
print [zeropad 9 negate 7.125]
print [zeropad 9 7.125]
print 7.125
```


{{out}}

```txt
   -7.125
    7.125
7.125
-0007.125
00007.125
7.125
```



## Raven


```raven
7.125 "%09.3f" print

00007.125
```

{{trans|Python}}

```raven
define PI
   -1 acos

PI exp PI - as r
r print "\n" print
r "" prefer "s=%s!\n" print
r dup dup dup dup "e=%e f=%f g=%g G=%G!\n" print
-1 r * dup dup "e=%9.4e f=%9.4f g=%9.4g!\n"  print
r dup dup "e=%9.4e f=%9.4f g=%9.4g!\n" print
r dup dup "e=%-9.4e f=%-9.4f g=%-9.4g!\n" print
r -1 * dup dup "e=%09.4e f=%09.4f g=%09.4g!\n" print
r dup dup "e=%09.4e f=%09.4f g=%09.4g!\n" print
r dup dup "e=%-09.4e f=%-09.4f g=%-09.4g!\n" print
```


```raven
19.9991
s=19.999100!
e=1.999910e+01 f=19.999100 g=19.9991 G=19.9991!
e=-1.9999e+01 f= -19.9991 g=      -20!
e=1.9999e+01 f=  19.9991 g=       20!
e=1.9999e+01 f=19.9991   g=20       !
e=-1.9999e+01 f=-019.9991 g=-00000020!
e=1.9999e+01 f=0019.9991 g=000000020!
e=1.9999e+01 f=19.9991   g=20       !
```



## REXX


```rexx
/*REXX program shows various ways to  add leading zeroes  to numbers.   */
a=7.125
b=translate(format(a,10),0,' ')
say 'a=' a
say 'b=' b
say

c=8.37
d=right(c,20,0)
say 'c=' c
say 'd=' d
say

e=19.46
f='000000'e
say 'e=' e
say 'f=' f
say

g=18.25e+1
h=000000||g
say 'g=' g
say 'h=' h
say

i=45.2
j=translate('      'i,0," ")
say 'i=' i
say 'j=' j
say

k=36.007
l=insert(00000000,k,0)
say 'k=' k
say 'l=' l
say

m=.10055
n=copies(0,20)m
say 'm=' m
say 'n=' n
say

p=4.060
q=0000000000000||p
say 'p=' p
say 'q=' q
say

r=876
s=substr(r+10000000,2)
say 'r=' r
say 's=' s
say

t=13.02
u=reverse(reverse(t)000000000)
say 't=' t
say 'u=' u
                                      /*stick a fork in it, we're done.*/
```

{{out}}
<pre style="height:30ex">
a= 7.125
b= 0000000007.125

c= 8.37
d= 00000000000000008.37

e= 19.46
f= 00000019.46

g= 18.25E+1
h= 00000018.25E+1

i= 45.2
j= 00000045.2

k= 36.007
l= 0000000036.007

m= .10055
n= 00000000000000000000.10055

p= 4.060
q= 00000000000004.060

r= 876
s= 0000876

t= 13.02
u= 00000000013.02

```



## Ring


```ring

decimals(3)
see fixedprint(7.125, 5) + nl

func fixedprint num, digs
     for i = 1 to digs - len(string(floor(num)))
         see "0"
     next
     see num + nl

```



## Ruby


```ruby
r = 7.125
printf " %9.3f\n",   r          #=>      7.125
printf " %09.3f\n",  r          #=>  00007.125
printf " %09.3f\n", -r          #=>  -0007.125
printf " %+09.3f\n", r          #=>  +0007.125
puts " %9.3f"  %  r             #=>      7.125
puts " %09.3f" %  r             #=>  00007.125
puts " %09.3f" % -r             #=>  -0007.125
puts " %+09.3f" % r             #=>  +0007.125
```



## Run BASIC


```runbasic
print right$("00000";using("#####.##",7.125),8) ' => 00007.13
```



## Rust


```rust

fn main() {
    let x = 7.125;

    println!("{:9}", x);
    println!("{:09}", x);
    println!("{:9}", -x);
    println!("{:09}", -x);
}

```

{{out}}

```txt

    7.125
00007.125
   -7.125
-0007.125

```



## Sather

The Fill options should fill with any character, but it is still (!) not implemented; according to [http://www.icsi.berkeley.edu/~sather/Documentation/Library/LibraryBrowser/short-FMT.html ICSI Sather library documentation] (GNU Sather library documentation is missing) works only for string, bools and characters, but a test has revealed it does not work in either way (yet) (GNU Sather v1.2.3).


```sather
class MAIN is
  main is
    #OUT + #FMT("<F0 #####.###>", 7.1257) + "\n";
    #OUT + #FMT("<F0 #####.###>", 7.1254) + "\n";
  end;
end;
```


Luckly the C-like formats are supported too:


```sather
    #OUT + #FMT("%09.3f", 7.125) + "\n";
```



## Scala

{{libheader|Scala}}
{{works with|Scala|2.10.2}}
As shown in a [https://github.com/scala-ide/scala-worksheet/wiki/Getting-Started Scala Worksheet]:

```Scala
object FormattedNumeric {
  val r = 7.125                                   //> r  : Double = 7.125
  println(f" ${-r}%9.3f");                        //>     -7,125
  println(f" $r%9.3f");                           //>      7,125
  println(f" $r%-9.3f");                          //>  7,125
  println(f" ${-r}%09.3f");                       //>  -0007,125
  println(f" $r%09.3f");                          //>  00007,125
  println(f" $r%-9.3f");                          //>  7,125
  println(f" $r%+09.3f");                         //>  +0007,125
}
```



## Scheme

{{works with|Gauche Scheme}}
Obtain the implementation of SRFI 54 from
http://srfi.schemers.org/srfi-54/srfi-54.html
and save it as "srfi-54.scm" in directory
Gauche/share/gauche/site/lib/


```Scheme
(load "srfi-54.scm")
(load "srfi-54.scm") ;; Don't ask.

(define x 295643087.65432)

(dotimes (i 4)
  (print (cat x 25 3.0 #\0 (list #\, (- 4 i)))))

```

{{output}}

```txt

00000000002,9564,3087.654
0000000000295,643,087.654
00000002,95,64,30,87.65,4
002,9,5,6,4,3,0,8,7.6,5,4

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const proc: main is func
  local
    const float: r is 7.125;
  begin
    writeln( r digits 3 lpad 9);
    writeln(-r digits 3 lpad 9);
    writeln( r digits 3 lpad0 9);
    writeln(-r digits 3 lpad0 9);
    writeln( r digits 3);
    writeln(-r digits 3);
  end func;
```


{{out}}

```txt

    7.125
   -7.125
00007.125
-0007.125
7.125
-7.125

```



## Sidef


```ruby
printf("%09.3f\n", 7.125);
```

or

```ruby
say ("%09.3f" % 7.125);
```

{{out}}

```txt

00007.125

```



## Smalltalk

{{works with|Pharo 1.1.1}}

```smalltalk
Transcript show: (7.125 printPaddedWith: $0 to: 3.6); cr.
"output: 007.125000"
```


{{works with|Smalltalk/X}}

```smalltalk
(7.123 asFixedPoint:3)  printOn: Transcript leftPaddedTo: 9 with: $0
"output: 00007.125"
```

notice that printOn:* is implemented in Object;thus any object can be printed with padding this way.

Using the PrintfScanf utility:

```smalltalk
PrintfScanf new printf:'%08.3f' arguments: { 7.125 }
```



## SQL

{{works with|MS SQL|2005}}

```sql
declare @n int
select @n=123
select substring(convert(char(5), 10000+@n),2,4) as FourDigits

set @n=5
print "TwoDigits: " + substring(convert(char(3), 100+@n),2,2)
--Output: 05
```



## Standard ML


```sml
print (StringCvt.padLeft #"0" 9 (Real.fmt (StringCvt.FIX (SOME 3)) 7.125) ^ "\n")
```

{{works with|SML/NJ}}

```sml
print (Format.format "%09.3f\n" [Format.REAL 7.125])
```



## Stata

See '''[https://www.stata.com/help.cgi?format format]''' in Stata help.


```stata
. display %010.3f (57/8)
000007.125
```



## Suneido


```Suneido
Print(7.125.Pad(9))
```


{{out}}

```txt
00007.125
```



## Tcl


```tcl
set number 7.342
format "%08.3f" $number
```

Use with <tt>puts</tt> if output is desired to go to a channel.

=={{header|TI-89 BASIC}}==

{{improve|TI-89 BASIC|It does not handle negative numbers.}}


```ti89b
right("00000" & format(7.12511, "f3"), 9)
```



## Toka


```toka
needs values
value n
123 to n

2 import printf
" %08d" n printf
```



## Ursala

The library function printf calls the host system's C
library function by that name and can cope with any of
the same numeric formats.

```Ursala
#import flo

x = 7.125

#show+

t = <printf/'%09.3f' x>
```

{{out}}

```txt
00007.125
```



## VBA


```vb
Option Explicit

Sub Main()
Debug.Print fFormat(13, 2, 1230.3333)
Debug.Print fFormat(2, 13, 1230.3333)
Debug.Print fFormat(10, 5, 0.3333)
Debug.Print fFormat(13, 2, 1230)
End Sub

Private Function fFormat(NbInt As Integer, NbDec As Integer, Nb As Double) As String
'NbInt : Lenght of integral part
'NbDec : Lenght of decimal part
'Nb : decimal on integer number
Dim u As String, v As String, i As Integer
   u = CStr(Nb)
   i = InStr(u, Application.DecimalSeparator)
   If i > 0 Then
      v = Mid(u, i + 1)
      u = Left(u, i - 1)
      fFormat = Right(String(NbInt, "0") & u, NbInt) & Application.DecimalSeparator & Left(v & String(NbDec, "0"), NbDec)
   Else
      fFormat = Right(String(NbInt, "0") & u, NbInt) & Application.DecimalSeparator & String(NbDec, "0")
   End If
End Function

```

{{out}}

```txt
0000000001230.33
30.3333000000000
0000000000.33330
0000000001230.00
```



## VBScript

{{works with|Windows Script Host|*}}

```VBScript

a = 1234.5678

' Round to three decimal places. Groups by default. Output = "1,234.568".
WScript.Echo FormatNumber(a, 3)

' Truncate to three decimal places. Output = "1234.567".
WScript.Echo Left(a, InStr(a, ".") + 3)

' Round to a whole number. Grouping disabled. Output = "1235".
WScript.Echo FormatNumber(a, 0, , , False)

' Use integer portion only and pad with zeroes to fill 8 chars. Output = "00001234".
WScript.Echo Right("00000000" & Int(a), 8)

```



## Vedit macro language

Vedit only supports integers, but fixed point calculations can be used for decimal values.

The following example uses 3 decimal places (value scaled by 1000).
The output is inserted at current edit position.

```vedit
#1 = 7125
Num_Ins(#1, FILL+COUNT, 9) Char(-3) Ins_Char('.')
```


{{out}}

```txt

 00007.125

```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb

Debug.Print Format$(7.125, "00000.000")

```

Output (the decimal separator used depends on the system's language settings):

```vb

00007.125

```



## XSLT

 <xsl:value-of select="format-number(7.125, '00000000.#############')" />


## zkl


```zkl
"%09.3f".fmt(7.125)   //-->"00007.125"
"%09.3e".fmt(7.125)   //-->"7.125e+00"
"%09.3g".fmt(7.125)   //-->"000007.12"
"%09d".fmt(7.125)     //-->"000000007"
"%09,d".fmt(78901.125)//-->"00078,901"
```



## ZX Spectrum Basic


```zxbasic
10 LET n=7.125
20 LET width=9
30 GO SUB 1000
40 PRINT AT 10,10;n$
50 STOP
1000 REM Formatted fixed-length
1010 LET n$=STR$ n
1020 FOR i=1 TO width-LEN n$
1030 LET n$="0"+n$
1040 NEXT i
1050 RETURN
```

