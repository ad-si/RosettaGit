+++
title = "Literals/Integer"
description = ""
date = 2019-10-08T18:01:41Z
aliases = []
[extra]
id = 3335
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Simple]]

Some programming languages have ways of expressing integer literals in bases other than the normal base ten.


;Task:
Show how integer literals can be expressed in as many bases as your language allows.


Note:   this should '''not''' involve the calling of any functions/methods, but should be interpreted by the compiler or interpreter as an integer written to a given base.

Also show any other ways of expressing literals, e.g. for different types of integers.


;Related task:
*   [[Literals/Floating point]]





## AArch64 Assembly

Supported integer literals may differ across assemblers.

GNU assembler supports decimal, binary (prefix 0b), octal (prefix 0), hexadecimal (prefix 0x), and ASCII value of a given character (a single quote followed by an ASCII character, no closing quote).

{{works with|aarch64-linux-gnu-as/qemu-aarch64}}
<lang ARM_Assembly>.equ STDOUT, 1
.equ SVC_WRITE, 64
.equ SVC_EXIT, 93

.text
.global _start

_start:
	stp x29, x30, [sp, -16]!
	mov x29, sp
	mov x0, #123 // decimal
	bl print_uint64
	mov x0, #0b01111011 // binary
	bl print_uint64
	mov x0, #0173 // octal
	bl print_uint64
	mov x0, #0x7b // hexadecimal
	bl print_uint64
	mov x0, #'{ // ascii value
	bl print_uint64
	mov x0, #'\{ // ascii value in another way
	bl print_uint64
	ldp x29, x30, [sp], 16
	mov x0, #0
	b _exit // exit(0);

// void print_uint64(uint64_t x) - print an unsigned integer in base 10.
print_uint64:
	// x0 = remaining number to convert
	// x1 = pointer to most significant digit
	// x2 = 10
	// x3 = x0 / 10
	// x4 = x0 % 10
	// compute x0 divmod 10, store a digit, repeat if x0 > 0
	ldr x1, =strbuf_end
	mov x2, #10
1:	udiv x3, x0, x2
	msub x4, x3, x2, x0
	add x4, x4, #48
	mov x0, x3
	strb w4, [x1, #-1]!
	cbnz x0, 1b
	// compute the number of digits to print, then call write()
	ldr x3, =strbuf_end_newline
	sub x2, x3, x1
	mov x0, #STDOUT
	b _write

.data
strbuf:
	.space 31
strbuf_end:
	.ascii "\n"
strbuf_end_newline:
.align 4

.text
//////////////// system call wrappers
// ssize_t _write(int fd, void *buf, size_t count)
_write:
	stp x29, x30, [sp, -16]!
	mov x8, #SVC_WRITE
	mov x29, sp
	svc #0
	ldp x29, x30, [sp], 16
	ret

// void _exit(int retval)
_exit:
	mov x8, #SVC_EXIT
	svc #0
```



## Ada

In [[Ada]] integer literals may have the form <base>#<numeral>#.
Here <base> can be from the range 2..16.
For example:

```ada
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure Test_Literals is
begin
   Put (16#2D7#);
   Put (10#727#);
   Put (8#1_327#);
   Put (2#10_1101_0111#);
end Test_Literals;
```

{{out}}

```txt

        727        727        727        727

```


## Aime


```aime
if ((727 == 0x2d7) && (727 == 01327)) {
    o_text("true\n");
} else {
    o_text("false\n");
}
```


## ALGOL 68


{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

Binary literals are of type BITS, and need to be converted
to INT using the operator ABS.

```algol68
main:(

  SHORT SHORT INT ssdec = SHORT SHORT 727,
            sshex = ABS SHORT SHORT 16r2d7,
            ssoct = ABS SHORT SHORT 8r1327,
            ssbin = ABS SHORT SHORT 2r1011010111;

  SHORT INT sdec = SHORT 727,
            shex = ABS SHORT 16r2d7,
            soct = ABS SHORT 8r1327,
            sbin = ABS SHORT 2r1011010111;

  INT dec = 727,
      hex = ABS 16r2d7,
      oct = ABS 8r1327,
      bin = ABS 2r1011010111;

  LONG INT ldec = LONG 727,
           lhex = ABS LONG 16r2d7,
           loct = ABS LONG 8r1327,
           lbin = ABS LONG 2r1011010111;

CO
  LONG LONG INT lldec = LONG LONG 727,
           llhex = ABS LONG LONG 16r2d7,
           lloct = ABS LONG LONG 8r1327,
           llbin = ABS LONG LONG 2r1011010111
# etc ... #
END CO

  print(("SHORT SHORT INT:", ssdec, sshex, ssoct, ssbin, new line));
  print(("      SHORT INT:", sdec, shex, soct, sbin, new line));
  print(("            INT:", dec, hex, oct, bin, new line));
  print(("       LONG INT:", ldec, lhex, loct, lbin, new line))
CO LONG LONG INT not supported by ELLA ALGOL 68RS
  print(("LONG LONG INT:", new line, lldec, new line, llhex, new line, lloct, new line, llbin, new line))
# etc ... #
END CO

)
```

[http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download algol68g] output:

```txt

SHORT SHORT INT:       +727       +727       +727       +727
      SHORT INT:       +727       +727       +727       +727
            INT:       +727       +727       +727       +727
       LONG INT:                                +727                                +727                                +727                                +727

```

[http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download algol68toc] output:

```txt

SHORT SHORT INT:  -41  -41  -41  -41
      SHORT INT:   +727   +727   +727   +727
            INT:        +727        +727        +727        +727
       LONG INT:                 +727                 +727                 +727                 +727

```



## ALGOL W

Algol W has only decimal integer literals. Hexadecimal values can be written (prefixed with #) but these are of type "bits" and the
standard number function must be used to "convert" them to an integer.

```algolw
begin
    write( 16, number( #10 ) )
end.
```

{{out}}

```txt

            16              16

```



## AmigaE


```amigae
PROC main()
  IF ($2d7 = 727) AND (%001011010111 = 727) THEN WriteF('true\n')
ENDPROC
```


## AutoHotkey


```AutoHotkey
If (727 == 0x2d7)
MsgBox true
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program integer.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
iNumberBinaire: .int 0b1100100
iNumberOctal:    .int  0144
iNumberDecimal: .int 100
iNumberHexa:     .int 0x64


szMessResult:  .ascii "Resultat = "      @ message result
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
    ldr r0,iAdriNumberBinaire   @ number address
    ldr r0,[r0]                     @ load number
    ldr r1,iAdrsMessValeur
    bl conversion10       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    ldr r0,iAdriNumberOctal
    ldr r0,[r0]
    ldr r1,iAdrsMessValeur
    bl conversion10       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
   ldr r0,iAdriNumberDecimal
    ldr r0,[r0]
    ldr r1,iAdrsMessValeur
    bl conversion10       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    ldr r0,iAdriNumberHexa
    ldr r0,[r0]
    ldr r1,iAdrsMessValeur
    bl conversion10       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message

100:   @ standard end of the program
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    svc #0                       @ perform the system call
iAdriNumberBinaire:	.int iNumberBinaire
iAdriNumberOctal:		.int iNumberOctal
iAdriNumberDecimal:	.int iNumberDecimal
iAdriNumberHexa:		.int iNumberHexa
iAdrsMessValeur:		.int sMessValeur
iAdrszMessResult:		.int szMessResult

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}    			/* save  registres */
    mov r2,#0   				/* counter length */
1:      	/* loop length calculation */
    ldrb r1,[r0,r2]  			/* read octet start position + index */
    cmp r1,#0       			/* if 0 its over */
    addne r2,r2,#1   			/* else add 1 in the length */
    bne 1b          			/* and loop */
                                /* so here r2 contains the length of the message */
    mov r1,r0        			/* address message in r1 */
    mov r0,#STDOUT      		/* code to write to the standard output Linux */
    mov r7, #WRITE             /* code call system "write" */
    svc #0                      /* call systeme */
    pop {r0,r1,r2,r7,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */
/******************************************************************/
/*     Converting a register to a decimal                                 */
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion10:
    push {r1-r4,lr}    /* save registers */
    mov r3,r1
    mov r2,#10

1:	   @ start loop
    bl divisionpar10 @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48        @ digit
    strb r1,[r3,r2]  @ store digit on area
    sub r2,#1         @ previous position
    cmp r0,#0         @ stop if quotient = 0 */
    bne 1b	          @ else loop
    @ and move spaves in first on area
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
   ldr r3, .Ls_magic_number_10 /* r1 <- magic_number */
   smull r1, r2, r3, r0   /* r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0) */
   mov r2, r2, ASR #2     /* r2 <- r2 >> 2 */
   mov r1, r0, LSR #31    /* r1 <- r0 >> 31 */
   add r0, r2, r1         /* r0 <- r2 + r1 */
   add r2,r0,r0, lsl #2   /* r2 <- r0 * 5 */
   sub r1,r4,r2, lsl #1   /* r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10) */
   pop {r2-r4}
   bx lr                  /* leave function */
   .align 4
.Ls_magic_number_10: .word 0x66666667



```



## Arturo


```arturo>num 18966</lang


## AWK

Awk has decimal literals, using the digits from <tt>0</tt> to <tt>9</tt>. [[Literals/Floating point#AWK]] describes the format of these literals.

As an extension to the language, some Awk implementations also have octal or hexadecimal literals. GNU awk ([[gawk]]) has both octal and hexadecimal literals, like C. The One True Awk ([[nawk]]) only has decimal literals.

{{works with|gawk|3.1.7}}

```awk
BEGIN {
    if ( (0x2d7 == 727) &&
         (01327 == 727) ) {
        print "true with GNU awk"
    }
}
```


nawk parses <tt>01327</tt> as <tt>1327</tt>, and parses <tt>0x2d7</tt> as <tt>0 x2d7</tt> (which is the string concatentation of <tt>"0"</tt> and variable <tt>x2d7</tt>).


```awk
BEGIN {
    x2d7 = "Goodbye, world!"
    print 0x2d7  # gawk prints "727", nawk prints "0Goodbye, world!"
    print 01327  # gawk prints "727", nawk prints "1327"
}
```



## Axe

In addition to decimal integer literals, Axe supports hexadecimal and binary integers using a leading exponent operator or pi, respectively. Note that the leading E below is the small-caps E.

```axe
123
ᴇFACE
π101010
```



## BASIC

&O = octal; &H = hexadecimal. Some flavors of BASIC also support &B = binary, but they're somewhat rare.


```qbasic
PRINT 17
PRINT &O21
PRINT &H11
```

Output:

```txt
17
17
17
```


=
## BaCon
=
BaCon allows (as it converts to C) C style integer literals.  zero prefix Octal, 0x prefix Hexadecimal, no prefix Decimal, and if supported by the underlying compiler, 0b prefix for Binary.  0x and 0b can be upper case 0X and 0B.

```freebasic
' literal integers
PRINT 10
PRINT 010
PRINT 0x10
' C compiler dependent, GCC extension
PRINT 0b10
```


{{out}}

```txt
prompt$ bacon literal-integer.bac
Converting 'literal-integer.bac'... done, 6 lines were processed in 0.002 seconds.
Compiling 'literal-integer.bac'... cc  -c literal-integer.bac.c
cc -o literal-integer literal-integer.bac.o -lbacon -lm
Done, program 'literal-integer' ready.
prompt$ ./literal-integer
10
8
16
2
```


=
## BBC BASIC
=

```bbcbasic
      PRINT 1234 : REM Decimal
      PRINT &4D2 : REM Hexadecimal
      PRINT %10011010010 : REM Binary
```

'''Output:'''

```txt

      1234
      1234
      1234

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>PRINT 17
PRINT BIN(10001)
PRINT ORD(HEX$("11"))
```



## bc

Numeric literals use the digits 0-9 and A-F (only the uppercase letters). The minus sign '-' and radix point '.' are optional. When the program encounters a numeric literal, it uses the current value of <tt>ibase</tt>.

This example shows the literal -727 in all bases from 2 to 16. (It never prints "Impossible!")


```bc
ibase = 2
b[10] = -1011010111
ibase = 11 /* 3 */
b[10] = -222221
ibase = 11 /* 4 */
b[10] = -23113
ibase = 11 /* 5 */
b[10] = -10402
ibase = 11 /* 6 */
b[10] = -3211
ibase = 11 /* 7 */
b[10] = -2056
ibase = 11 /* 8 */
b[10] = -1327
ibase = 11 /* 9 */
b[10] = -887
ibase = 11 /* 10 */
b[10] = -727
ibase = 11 /* 11 */
b[10] = -601
ibase = 11 /* 12 */
b[10] = -507
ibase = 11 /* 13 */
b[10] = -43C
ibase = 11 /* 14 */
b[10] = -39D
ibase = 11 /* 15 */
b[10] = -337
ibase = 11 /* 16 */
b[10] = -2D7

ibase = A
for (i = 2; i <= 16; i++) if (b[i] != -727) "Impossible!
"
quit
```


The digits 0-9 and A-F are valid with all input bases. For example, FF from base 2 is 45 (because 15 * 2 + 15 is 45), and FF from base 10 is 165 (because 15 * 10 + 15 is 45). Most importantly, <tt>ibase = A</tt> always switches to base ten.


## Befunge


While Befunge doesn't directly support numbers aside from 0-9 (base 10), characters in strings are essentially treated as base-256 numbers.


```befunge
" ~"..@
```


Output:
 126 32


## Bracmat

Bracmat only supports specification of numbers in base ten.


## C


Leading 0 means octal, 0x or 0X means hexadecimal. Otherwise, it is just decimal.


```c
#include <stdio.h>

int main(void)
{
  printf("%s\n",
         ( (727 == 0x2d7) &&
           (727 == 01327)    ) ? "true" : "false");

  return 0;
}
```


GCC supports specifying integers in binary using the [http://gcc.gnu.org/onlinedocs/gcc/Binary-constants.html 0b prefix] syntax, but it's not standard. Standard C has no way of specifying integers in binary.

To specify a literal of an unsigned integer, you add the suffix "u" or "U". To specify a literal of a "long" integer, you add the suffix "l" or "L". In C99, to specify a literal of a "long long" integer, you add the suffix "ll" or "LL". (The "l" and "ll" forms are discouraged as "l" looks like the digit "1"). The "u" suffixes can be combined with "l" or "ll" suffixes for unsigned long or unsigned long long integers.

## C#
C# has decimal and hexadecimal integer literals, the latter of which are prefixed with <code>0x</code>:

```c#
int a = 42;
int b = 0x2a;
```

Literals of either form can be suffixed with <code>U</code> and/or <code>L</code>. <code>U</code> will cause the literal to be interpreted as an unsigned type (necessary for numbers exceeding 2<sup>31</sup> or hex literals that have a first digit larger than <code>7</code>) and <code>L</code> signifies the use of a <code>long</code> type – using <code>UL</code> or <code>LU</code> as suffix will then use <code>ulong</code>. C# has no syntactic notion of expressing integer literals of smaller types than <code>Int32</code>; it is a compile-time error to have an assignment such as

```csharp>byte x = 500;</lang

'''Update'''<br/>
As of C#7, integer literals can be written in binary with the prefix <code>0b</code>. Furthermore, underscores can be used as separators:

```c#

int x = 0b1100_1001_1111_0000;

```



## C++


The same comments apply as to the [[#C|C example]].


```cpp
#include <iostream>

int main()
{
  std::cout << ( (727 == 0x2d7) &&
                 (727 == 01327)     ? "true" : "false")
            << std::endl;

  return 0;
}
```



## Clojure


Clojure uses the Java octal (0...) and hexadecimal (0x...) notation; for any other base, nR... is used, 2 <= n <= 36.


```lisp>user=
 2r1001
9
user=> 8r64
52
user=> 064
52
user=> 16r4b
75
user=> 0x4b
75
user=>
```



## COBOL

Standard COBOL accepts signed base 10 integer literals, but does allow for <tt>BOOLEAN</tt> and <tt>Hexadecimal</tt> alphanumeric literals, that can be treated as numeric values in code.

ACUCOBOL added extensions that allow base-2 (B#), base-8 (O#), base-16 (with both H# and X# prefix) integer literals.

With GnuCOBOL these extensions are allowed by configuration

```txt

prompt$ cobc -x -cb_conf=acucobol-literals:ok

```



```COBOL

display B#10 ", " O#01234567 ", " -0123456789 ", "
        H#0123456789ABCDEF ", " X#0123456789ABCDEF ", " 1;2;3;4

```


{{out}}

```txt

2, 342391, 0123456789, 81985529216486895, 81985529216486895, 1234

```


Some characters are removed by the COBOL text manipulation facility, and are allowed in numeric literals. These symbols are stripped out, along with comment lines, before seen by the compiler proper.


```cobol

if 1234 = 1,2,3,4 then display "Decimal point is not comma" end-if
if 1234 = 1;2;3;4 then display "literals are equal, semi-colons ignored" end-if

```


Comma is a special case, as COBOL can be compiled with <code>DECIMAL POINT IS COMMA</code> in the <code>CONFIGURATION SECTION</code>.  The <tt>1,2,3,4</tt> comparison test above would cause a compile time syntax error when <code>DECIMAL POINT IS COMMA</code> is in effect.


## Comal


```Comal
IF 37=$25 THEN PRINT "True"
IF 37=%00100101 THEN PRINT "True"

```



## Common Lisp


(This is an interactive common lisp session)

binary: #b, octal: #o, hexadecimal: #x, any base from 2 to 36: #Nr

```lisp>
(= 727 #b1011010111)
T
>(= 727 #o1327)
T
>(= 727 #x2d7)
T
>(= 727 #20r1g7)
T
```



## D


D besides hexadecimal, has also binary base. Additionally you can use '''_''' to separate digits in integer (and FP) literals. Octal number literals are library-based to avoid bugs caused by the leading zero.

```d
import std.stdio, std.conv;

void main() {
    writeln("oct: ", octal!777);
    writeln("bin: ", 0b01011010);
    writeln("hex: ", 0xBADF00D);
    writeln("dec: ", 1000000000);
    writeln("dec: ", 1_000_000_000);
    writeln();

    writeln(typeid(typeof(0)));
    writeln(typeid(typeof(0u)));
    // writeln(typeid(typeof(0l))); // 'l' suffix is deprecated
    writeln(typeid(typeof(0L)));
    writeln(typeid(typeof(0uL)));
    writeln(typeid(typeof(0LU)));
    writeln();

    writefln("%x", 0xFEE1_BAD_CAFE_BABEuL);
}
```

{{out}}

```txt
oct: 511
bin: 90
hex: 195948557
dec: 1000000000
dec: 1000000000

int
uint
long
ulong
ulong

fee1badcafebabe
```


## DCL


```DCL
$ decimal1 = 123490
$ decimal2 = %D123490
$ octal = %O12370
$ hex = %X1234AF0
```



## Delphi


```Delphi
const
  INT_VALUE = 256;
  HEX_VALUE = $100;
```



## DWScript


DWScript has decimal and hexadecimal integer literals, the latter of which are prefixed with <code>$</code>:

```delphi
var a : Integer := 42;
var b : Integer := $2a;
```

Both notations can also be used for character codes (when prefixed by <code>#</code>).


## Dyalect


Dyalect has decimal and hexadecimal integer literals, the latter of which are prefixed with 0x:


```Dyalect
var a = 42
var b = 0x2a
```



## Dylan


```Dylan
42        // a decimal integer
#x2A      // a hexadecimal integer
#o52      // an octal integer
#b101010  // a binary integer
```



## E



```e
? 256
# value: 256

? 0x100
# value: 256

? 0123
# syntax error: Octal is no longer supported: 0123
```



## Efene



```efene
@public
run = fn () {
    io.format("0xff  : ~B~n", [0xff])
    io.format("0xFF  : ~B~n", [0xFF])
    io.format("0o777 : ~B~n", [0o777])
    io.format("0b1011: ~B~n", [0b1011])
}

```



## Eiffel

Integer literals can be specified in decimal, hexadecimal, octal and binary. Only decimal literals can have an optional sign. Underscores may also be used as separators, but cannot begin or end the literal. Literals are case insensitive.
```Eiffel

123		-- decimal
-1_2_3		-- decimal
0x7b		-- hexadecimal
0c173		-- octal
0b111_1011	-- binary

```


Literals are by default interpreted as type INTEGER, where INTEGER is a synonym for either INTEGER_32 or INTEGER_64 (depending on the compiler option) but can be explicitly converted to another type.
```Eiffel

{NATURAL_8}	255
{INTEGER_64}	2_147_483_648

```


## Elena


```elena

   var n := 1234; // decimal number
   var x := 1234h; // hexadecimal number

```



## Elixir


```elixir>1234            #=
 1234
1_000_000       #=> 1000000
0010            #=> 10
0b111           #=> 7
0o10            #=> 8
0x1f            #=> 31

0B10            #=> syntax error before: B10
0X10            #=> syntax error before: X10
0xFF            #=> 255
```



## Emacs Lisp


```Lisp
123      ;; decimal           all Emacs
#b101    ;; binary            Emacs 21 up, XEmacs 21
#o77     ;; octal             Emacs 21 up, XEmacs 21
#xFF     ;; hex               Emacs 21 up, XEmacs 21
#3r210   ;; any radix 2-36    Emacs 21 up (but not XEmacs 21.4)
```


The digits and the radix character can both be any mixture of upper and lower case.  See [http://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html GNU Elisp reference manual "Integer Basics"].


## Erlang

Erlang allows integer literals in bases 2 through 36. The format is Base#Number. For bases greater than 10, the values 10-35 are represented by A-Z or a-z.

```erlang

> 2#101.
5
> 101.
101
> 16#F.
15
> 36#3z.
143

```



## ERRE

% = binary, & = octal; $ = hexadecimal.

```ERRE

PRINT(17)
PRINT(&21)
PRINT($11)
PRINT(%1001)

```

Output:

```txt

17
17
17
17
```



## Euphoria


```euphoria

printf(1,"Decimal:\t%d, %d, %d, %d\n",{-10,10,16,64})
printf(1,"Hex:\t%x, %x, %x, %x\n",{-10,10,16,64})
printf(1,"Octal:\t%o, %o, %o, %o\n",{-10,10,16,64})
printf(1,"Exponential:\t%e, %e, %e, %e\n",{-10,10,16,64.12})
printf(1,"Floating Point\t%3.3f, %3.3f, %+3.3f\n",{-10,10.2,16.25,64.12625})
printf(1,"Floating Point or Exponential:  %g, %g, %g, %g\n",{10,16,64,123456789.123})

```

{{out}}

```txt

Decimal:    -10, 10, 16, 64
Hex:    FFFFFFFFFFFFFFF6, A, 10, 40
Octal:  1777777777777777777766, 12, 20, 100
Exponential:    -1.000000e+001, 1.000000e+001, 1.600000e+001, 6.412000e+001
Floating Point  -10.000, 10.000, +16.250, 64.126
Floating Point or Exponential:  10, 16, 64, 1.23457e+008

```


=={{header|F Sharp|F#}}==

### Base prefixes

Binary numbers begin with 0b, octal numbers with 0o, and hexadecimal numbers with 0x. The hexadecimal digits A-F may be in any case.

```fsharp
0b101 // = 5
0o12  // = 10
0xF   // = 16
```



### Type suffixes

Most type suffixes can be preceded with a 'u', which indicates the type is unisgned.

```fsharp
10y  // 8-bit
'g'B // Character literals can be turned into unsigned 8-bit literals
10s  // 16-bit
10l  // 32-bit (suffix is optional)
10L  // 64-bit
10I  // Bigint (cannot be preceded by a 'u')

10un // Unsigned native int (used to represent pointers)
```



## Factor


```factor
10 . ! decimal
0b10 . ! binary
-0o10 . ! octal
0x10 . ! hexadecimal
```

{{out}}

```txt

10
2
-8
16

```

Factor also supports the arbitrary use of commas in integer literals:

```factor
1,234,567 .
1,23,4,567 .
```

{{out}}

```txt

1234567
1234567

```



## Forth

The standard method for entering numbers of a particular base is to set the user variable BASE to the desired radix from 2 to 36. There are also convenience words for setting the base to DECIMAL and HEX.

```forth
HEX
FEEDFACE
2 BASE !
1011001
DECIMAL
1234
: mask  var @ [ base @ hex ] 3fff and [ base ! ] var ! ;
```

The Forth numeric parser will look for symbols embedded within the stream of digits to determine whether to interpret it as a single cell, double cell, or floating point literal ('e').

```forth
1234   ( n )
123.4  ( l h )
123e4  ( F: n )
```



### Base prefixes

{{works with|GNU Forth}}
In addition, many Forths have extensions for using a prefix to temporarily override BASE when entering an integer literal. These are the prefixes supported by GNU Forth.

```forth
$feedface   \ hexadecimal
&1234       \ decimal
%1001101    \ binary
'a          \ base 256  (ASCII literal)
```

Some Forths also support "0xABCD" hex literals for compatibility with C-like languages.


## Fortran



```fortran
program IntegerLiteral

  implicit none
  integer, parameter   :: dec = 727
  integer, parameter   :: hex = Z'2d7'
  integer, parameter   :: oct = O'1327'
  integer, parameter   :: bin = B'1011010111'

  print *, dec, hex, oct, bin

end program IntegerLiteral
```


Outputs:


```txt

         727         727         727         727

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' The following all print 64 to the console

' integer literals of unspecified type - actual type is inferred from size or context (8, 16, 32 or 64 bit signed/unsigned)
Print 64        '' Decimal literal
Print &H40      '' Hexadecimal literal
Print &O100     '' Octal Literal
Print &B1000000 '' Binary literal

' integer literals of specific types
' Integer type is 4 bytes on 32 bit and 8 bytes on 64 bit platform
Print 64%       '' Decimal signed 4/8 byte integer (Integer)
Print 64L       '' Decimal signed 4 byte integer   (Long)
Print 64&       '' Decimal signed 4 byte integer   (Long) - alternative suffix
Print 64LL      '' Decimal unsigned 4 byte integer (ULong)
Print 64LL      '' Decimal signed 8 byte integer   (LongInt)
Print 64ULL     '' Decimal unsigned 8 byte integer (ULongInt)

Sleep
```



## Frink

Bases from 2 to 36 are allowed in Frink.  All literals can be arbitrarily large.  Frink does not subscribe to the insanity that a leading 0 implies octal.

```frink

123456789123456789               // (a number in base 10)
123_456_789_123_456_789          // (the same number in base 10 with underscores for readability)
1 quadrillion                    // (named numbers are fine in Frink.)
1ee39                            // (exact exponent, an integer with exact value 10^39)
100001000101111111101101\\2      // (a number in base 2)
1000_0100_0101_1111_1110_1101\\2 // (a number in base 2 with underscores for readability)
845FED\\16                       // (a number in base 16... bases from 2 to 36 are allowed)
845fed\\16                       // (The same number in base 16... upper or lowercase are allowed.)
845_fed\\16                      // (a number in base 16 with underscores for readability)
FrinkRulesYou\\36                // (a number in base 36)
0x845fed                         // (Common hexadecimal notation)
0x845FED                         // (Common hexadecimal notation)
0xFEED_FACE                      // (Hexadecimal with underscores for readability)
0b100001000101111111101101       // (Common binary notation)
0b1000_0100_0101_1111_1110_1101  // (Binary with underscores for readability)

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

def tab 2

print "      Decimal 100:", 100
print " Hexadecimal &h64:", &h64,      hex$(100)
print "      Octal &o144:", &o144,     oct$(100)
print " Binary &x1100100:", &x1100100, bin$(100)

```

Output:

```txt

      Decimal 100:   100
 Hexadecimal &h64:   100  00000064
      Octal &o144:   100  00000000144
 Binary &x1100100:   100  00000000000000000000000001100100

```



## GAP



```gap
# Only decimal integers, but of any length
31415926
1606938044258990275541962092341162602522202993782792835301376
```



## Go

For integer literals, leading <code>0</code> means octal, <code>0x</code> or <code>0X</code> means hexadecimal.
Otherwise, it is just decimal. There is no binary.

Character literals though, also specify integer values.  Go source is specified to be UTF-8 encoded.  The value of a character literal is the Unicode value of the UTF-8 encoded character.

There is no size or type specification with an integer literal, they are of arbitrary precision and do not overflow (compilers are required to represent integer constants with at least 256 bits and give an error if unable to represent an integer constant precisely).
Constant expressions are evaluated at compile time at an arbitrary precision.
It is only when a constant is assigned to a variable that it is given a type and an error produced if the constant value cannot be represented as a value of the respective type.

```go
package main

import "fmt"

func main() {
    fmt.Println(727 == 0x2d7) // prints true
    fmt.Println(727 == 01327) // prints true
    fmt.Println(727 == '˗')   // prints true
}
```



## Groovy

Solution:

```groovy
println 025    // octal
println 25     // decimal integer
println 25l    // decimal long
println 25g    // decimal BigInteger
println 0x25   // hexadecimal
```


Output:

```txt
21
25
25
25
37
```



## Harbour

Hexademical integer literals are supported - the leading symbols must be 0x or 0X:

```visualfoxpro>? 0x1f</lang

Output:

```txt
31
```



## Haskell


(This is an interactive ghci session)

Oct(leading 0o or 0O), Hex(leading 0x or 0X)

```haskell>Prelude
 727 == 0o1327
True
Prelude> 727 == 0x2d7
True
```



## hexiscript


```hexiscript
# All equal to 15
println 15
println 000015 # Leading zeros are ignored
println 0b1111
println 0o17
println 0xf
```



## HicEst

HicEst only supports decimal integer literals.


## HolyC

HolyC supports various integer sizes.


```holyc
U8 i; // 8 bit integer
U16 i; // 16 bit integer
U32 i; // 32 bit integer
U64 i; // 64 bit integer
```


By default all integers are decimal. Leading "0x" implies hexadecimal.

```holyc
U16 i = 727; // decimal
U16 i = 0x2d7; // hexadecimal
```


=={{header|Icon}} and {{header|Unicon}}==
Icon/Unicon supports digit literals of the form <base>r<value> with base being from 2-36 and the digits being from 0..9 and a..z.

```Icon
procedure main()
L := [1, 2r10, 3r10, 4r10, 5r10, 6r10, 7r10, 8r10, 9r10, 10r10, 11r10, 12r10, 13r10, 14r10,
      15r10, 16r10, 17r10, 18r10,19r10, 20r10, 21r10, 22r10, 23r10, 24r10, 25r10, 26r10, 27r10,
      28r10, 29r10, 30r10, 31r10, 32r10, 33r10, 34r10, 35r10, 36r10]

every write(!L)
end
```



## J


J's numeric [http://www.jsoftware.com/help/dictionary/dcons.htm mini-language] allows spaces, underlines, dots and lower case alphabetic characters in its numeric literals.

Arbitrary base numbers begin with a base ten literal (which represents the base of this number), and then the letter 'b' and then an arbitrary sequence of digits and letters which represents the number in that base.  Letters a..z represent digits in the range 10..35.  Each numeric item in a numeric constant must have its base specified independently.


```j
   10b123 16b123 8b123 20b123 2b123 1b123 0b123 100b123 99 0 0bsilliness
1
123 291 83 443 11 6 3 10203 99 0 1 28
```


This may be used to enter hexadecimal or octal or binary numbers.  However, note also that J's primitives support a variety of binary operations on numbers represented as sequences of 0s and 1s, like this:


```j>0 1 0 0 0 1 0 0 0 1 1 1 1</lang



J also supports extended precision integers, if one member of a list ends with an 'x' when they are parsed.  Extended precision literals can not be combined, in the same constant, with arbitrary base literals. (The notation supports no way of indicating that extra precision in an arbitrary base literal should be preserved and the extra complexity to let this attribute bleed from any member of a list to any other member was deemed not worth implementing.)


```j
   123456789123456789123456789 100000000000x
123456789123456789123456789 100000000000

   16b100 10x
|ill-formed number
```


J also allows integers to be entered using other notations, such as scientific or rational.


```j
   1e2 100r5
100 20
```


Internally, J freely [http://www.jsoftware.com/help/dictionary/dictg.htm converts] fixed precision integers to floating point numbers when they overflow, and numbers (including integers) of any type may be combined using any operation where they would individually be valid arguments.

Internally, J represents numeric constants in their simplest type, regardless of how they were specified.  In other words 9r1, although it is "specified as a rational" is represented as an extended precision integer.  Similarly, 2.0, although it is "specified as a floating point value" is represented as an integer, and 1.0 is represented as a boolean.

That said, note that "type" is a property of the array, and not a property of the value.  And, code that modifies the structure of an array leaves its type alone.  So, if you need an array of a type different than that specified by J's "simplest type for constants" rule, you can extract the constant you need from an array which contains it and has the type you need.  For example <code>{.1 2</code> would give you an integer 1 instead of a boolean 1.


## Java


A leading 0 means octal, 0x or 0X means hexadecimal. Otherwise, it is just decimal.


```java5
public class IntegerLiterals {
    public static void main(String[] args) {
        System.out.println( 727 == 0x2d7 &&
                            727 == 01327   );
    }
}
```


You may also specify a <tt>long</tt> literal by adding an <tt>l</tt> or <tt>L</tt> (uppercase is preferred as the lowercase looks like a "1" in some fonts) to the end (ex: <tt>long a = 574298540721727L</tt>). This is required for numbers that are too large to be expressed as an <tt>int</tt>.

{{works with|Java|7}}
Java 7 has added binary literals to the language. A leading 0b means binary. You may also use underscores as separators in all bases.

```java5
public class BinaryLiteral {
    public static void main(String[] args) {
        System.out.println( 727 == 0b10_1101_0111 );
    }
}
```



## JavaScript



```javascript
if ( 727 == 0x2d7 &&
     727 == 01327 )
    window.alert("true");
```



## jq

jq only supports JSON data types, and thus the only supported integer literals are decimals, which may, however, be expressed using digits in the conventional way, or using the "e" notation, e.g. 10 == 1e1.  Other ways to express 10 include 1e+1, 10e0, 10E-0, etc.


## Julia

Julia has binary, octal and hexadecimal literals. We check that they give the same value.

```julia>julia
 0b1011010111 == 0o1327 == 0x2d7 == 727
true
```



## Kotlin

Kotlin supports 3 types of integer literal (signed 4 byte), namely : decimal, hexadecimal and binary.

These can be converted to long integer literals (signed 8 byte) by appending the suffix 'L' (lower case 'l' is not allowed as it is easily confused with the digit '1').

It is also possible to assign integer literals to variables of type Short (signed 2 byte) or Byte (signed 1 byte). They will be automatically converted by the compiler provided they are within the range of the variable concerned.

```scala
// version 1.0.6

fun main(args: Array<String>) {
    val d = 255                  // decimal integer literal
    val h = 0xff                 // hexadecimal integer literal
    val b = 0b11111111           // binary integer literal

    val ld = 255L                // decimal long integer literal (can't use l instead of L)
    val lh = 0xffL               // hexadecimal long integer literal (could use 0X rather than 0x)
    val lb = 0b11111111L         // binary long integer literal (could use 0B rather than 0b)

    val sd : Short = 127         // decimal integer literal automatically converted to Short
    val sh : Short = 0x7f        // hexadecimal integer literal automatically converted to Short
    val bd : Byte  = 0b01111111  // binary integer literal automatically converted to Byte

    println("$d $h $b $ld $lh $lb $sd $sh $bd")
}
```


{{out}}

```txt

255 255 255 255 255 255 127 127 127

```



## Lasso


```Lasso
42
0x2a
```


## Limbo

Integer literals in Limbo can be written in any base from 2 to 36 by putting the base (or radix), then 'r' or 'R', and the digits of the number. If no base is explicitly given then the number will be in base 10.

```Limbo
implement Command;

include "sys.m";
sys: Sys;

include "draw.m";

include "sh.m";

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;

	sys->print("%d\n", 2r1111); # binary
	sys->print("%d\n", 8r17);   # octal
	sys->print("%d\n", 15);     # decimal
	sys->print("%d\n", 16rF);   # hexadecimal
}
```



## LiveCode

LiveCode supports hexadecimal literals, and if "convertOctals" is set to true, then integer literals with leading zeroes are interpreted as octal and not base 10.

Hex example
```LiveCode>put 0x1 + 0xff</lang



## Logo

Logo only supports decimal integer literals.


## Logtalk

Built-in support for bases 2, 8, 10, and 16:

```logtalk

:- object(integers).

    :- public(show/0).

    show :-
        write('Binary      0b11110101101 = '), write(0b11110101101), nl,
        write('Octal       0o3655 =        '), write(0o3655), nl,
        write('Decimal     1965 =          '), write(1965), nl,
        write('Hexadecimal 0x7AD =         '), write(0x7AD), nl.

:- end_object.

```

Sample output:

```text

| ?- integers::show.
Binary      0b11110101101 = 1965
Octal       0o3655 =        1965
Decimal     1965 =          1965
Hexadecimal 0x7AD =         1965
yes

```



## Lua

Lua supports either base ten or hex

```Lua

45, 0x45

```



## M2000 Interpreter


```M2000 Interpreter

Def ExpType$(x)=Type$(x)
Print ExpType$(12345678912345#)="Currency", 12345678912345#
Print ExpType$(123456789123456789123456@)="Decimal", 123456789123456789123456@
Print ExpType$(12&)="Long", 12&, 0xFFFFFFFF&=-1
Print ExpType$(12%)="Integer", 12%, 0xFFFF%=-1
\\ used for unsigned integers (but it is double)
Print ExpType$(0xFFFFFFFF)="Double", 0xFFFFFFFF=4294967295

```



## M4

m4 has decimal, octal and hexadecimal literals like C.


```M4
eval(10)        # base 10
eval(010)       # base 8
eval(0x10)      # base 16
```


Output:
```txt
10        # base 10
8       # base 8
16      # base 16
```


As an extension, GNU m4 provides "0b" and "0r" literals.

{{works with|GNU m4}}


```M4
eval(0b10)      # base 2
eval(`0r2:10')  # base 2
 ...
eval(`0r36:10') # base 36
```


Output:
```txt
2      # base 2
2  # base 2
 ...
36 # base 36
```



## Mathematica


```Mathematica
b^^nnnn is a valid number in base b (with b ranging from 2 to 36) :
2^^1011
-> 11

36^^1011
-> 46693
```


=={{header|MATLAB}} / {{header|Octave}}==
Matlab uses only base 10 integers.

```MATLAB>
 11
ans =  11
```


Octave allows also a hexadecimal representation

```Octave>
 0x11
ans =  17
```


Other representation of other bases need to be converted by functions

```MATLAB
hex2dec(s)
bin2dec(s)
base2dec(s,base)
```


Different integer types can be defined by casting.

```MATLAB
int8(8)
uint8(8)
int16(8)
uint16(8)
int32(8)
uint32(8)
int64(8)
uint64(8)
```



## Maxima


```maxima
/* Maxima has integers of arbitrary length */
170141183460469231731687303715884105727
```



## Mercury



```Mercury
Bin = 0b010101,
Octal = 0o666,
Hex = 0x1fa,
CharCode = 0'a.
```


An integer is either a decimal, binary, octal, hexadecimal, or character-code literal. A decimal literal is any sequence of decimal digits. A binary literal is <tt>0b</tt> followed by any sequence of binary digits. An octal literal is <tt>0o</tt> followed by any sequence of octal digits. A hexadecimal literal is <tt>0x</tt> followed by any sequence of hexadecimal digits. A character-code literal is <tt>0'</tt> followed by any single character.


## Metafont



```metafont
num1 := oct"100";
num2 := hex"100";
```


Metafont numbers can't be greater than 4096, so that the maximum octal and hexadecimal legal values are <tt>7777</tt> and <tt>FFF</tt> respectively. To be honest, <tt>"100"</tt> is a string, and <tt>oct</tt> is an "internal" "''macro''"; but this is the way Metafont specifies numbers in base 8 and 16.

=={{header|Modula-3}}==
All numbers 2 to 16 are allowed to be bases.

```modula3
MODULE Literals EXPORTS Main;

IMPORT IO;

BEGIN
  IO.PutInt(16_2D7);
  IO.Put(" ");
  IO.PutInt(10_727);
  IO.Put(" ");
  IO.PutInt(8_1327);
  IO.Put(" ");
  IO.PutInt(2_1011010111);
  IO.Put("\n");
END Literals.
```



## Neko

Neko supports base 10 and 0x prefixed base 16 integer literals.  Leading zero is NOT octal.


```ActionScript
/**
 Integer literals, in Neko
 Base 10 and Base 16, no leading zero octal in Neko
*/

var num = 2730
if (num == 02730) $print("base 10, even with leading zero\n")
if (num == 0xAAA) $print("base 16, with leading 0x or 0X\n")
```




## Nemerle


```Nemerle
42                            // integer literal
1_000_000                     // _ can be used for readability
1_42_00                       // or unreadability...
0x2a                          // hexadecimal integer literal
0o52                          // octal integer literal
0b101010                      // binary integer literal
10u                           // unsigned int
10b, 10sb, 10bs               // signed byte
10ub, 10bu                    // unsigned byte
10L                           // long
10UL, 10LU                    // unsigned long
```


Formally (adapted from [http://nemerle.org/wiki/index.php?title=Lexical_structure_%28ref%29 Reference Manual]):

```txt
<decimal_literal> ::=
    [ <prefix> ] <digits> [ { '_' <digits> } ] [ <suffix> ]
<prefix> ::=
    '0x'
|   '0o'
|   '0b'
<digits> ::=
    { <decimal_digit> }
<suffix> ::=
    'b'
|   'sb'
|   'ub'
|   's'
|   'us'
|   'u'
|   'l'
|   'lu'
```



## NetRexx

Along with decimal notation NetRexx accepts numeric literals in hexadecimal and binary formats.

The NetRexx documentation describes hexadecimal and binary literal symbol notation in more detail; a summary follows:

A ''hexadecimal numeric symbol'' describes a whole number, and is of the form ''nXstring'' where, ''n'' is a
simple number which describes the effective length of the hexadecimal string and ''string'' is a string of one or more hexadecimal characters.

A ''binary numeric symbol'' describes a whole number using the same rules, except that the identifying
character is <tt>B</tt> or <tt>b</tt>, and the digits of ''string'' must be either <tt>0</tt> or <tt>1</tt>, each representing a single bit.


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols

iv =                   8; say                   '8'.right(20) '==' iv.right(8) --     8
iv =                  -8; say                  '-8'.right(20) '==' iv.right(8) --    -8
iv =                 1x8; say                 '1x8'.right(20) '==' iv.right(8) --    -8
iv =                 2x8; say                 '2x8'.right(20) '==' iv.right(8) --     8
iv =                2x08; say                '2x08'.right(20) '==' iv.right(8) --     8
iv =                0x08; say                '0x08'.right(20) '==' iv.right(8) --     8
iv =                0x10; say                '0x10'.right(20) '==' iv.right(8) --    16
iv =                0x81; say                '0x81'.right(20) '==' iv.right(8) --   129
iv =                2x81; say                '2x81'.right(20) '==' iv.right(8) --  -127
iv =                3x81; say                '3x81'.right(20) '==' iv.right(8) --   129
iv =                4x81; say                '4x81'.right(20) '==' iv.right(8) --   129
iv =               04x81; say               '04x81'.right(20) '==' iv.right(8) --   129
iv =               16x81; say               '16x81'.right(20) '==' iv.right(8) --   129
iv =              4xF081; say              '4xF081'.right(20) '==' iv.right(8) -- -3967
iv =              8xF081; say              '8xF081'.right(20) '==' iv.right(8) -- 61569
iv =              0Xf081; say              '0Xf081'.right(20) '==' iv.right(8) -- 61569
iv =              0xffff; say              '0xffff'.right(20) '==' iv.right(8) -- 65535
iv =              4xffff; say              '4xffff'.right(20) '==' iv.right(8) --    -1
iv =              8xffff; say              '8xffff'.right(20) '==' iv.right(8) -- 65535
iv =                 1b0; say                 '1b0'.right(20) '==' iv.right(8) --     0
iv =                 1b1; say                 '1b1'.right(20) '==' iv.right(8) --    -1
iv =                 2b1; say                 '2b1'.right(20) '==' iv.right(8) --     1
iv =                0b10; say                '0b10'.right(20) '==' iv.right(8) --     2
iv =                2b10; say                '2b10'.right(20) '==' iv.right(8) --    -2
iv =                3b10; say                '3b10'.right(20) '==' iv.right(8) --     2
iv =               0b100; say               '0b100'.right(20) '==' iv.right(8) --     4
iv =               3b100; say               '3b100'.right(20) '==' iv.right(8) --    -4
iv =               4b100; say               '4b100'.right(20) '==' iv.right(8) --     4
iv =              4b1000; say              '4b1000'.right(20) '==' iv.right(8) --    -8
iv =              8B1000; say              '8B1000'.right(20) '==' iv.right(8) --     8
iv = 00B1111111111111111; say '00B1111111111111111'.right(20) '==' iv.right(8) -- 65535
iv = 16B1111111111111111; say '16B1111111111111111'.right(20) '==' iv.right(8) --    -1
iv = 32B1111111111111111; say '32B1111111111111111'.right(20) '==' iv.right(8) -- 65535

return
```

'''Output:'''

```txt

                   8 ==        8
                  -8 ==       -8
                 1x8 ==       -8
                 2x8 ==        8
                2x08 ==        8
                0x08 ==        8
                0x10 ==       16
                0x81 ==      129
                2x81 ==     -127
                3x81 ==      129
                4x81 ==      129
               04x81 ==      129
               16x81 ==      129
              4xF081 ==    -3967
              8xF081 ==    61569
              0Xf081 ==    61569
              0xffff ==    65535
              4xffff ==       -1
              8xffff ==    65535
                 1b0 ==        0
                 1b1 ==       -1
                 2b1 ==        1
                0b10 ==        2
                2b10 ==       -2
                3b10 ==        2
               0b100 ==        4
               3b100 ==       -4
               4b100 ==        4
              4b1000 ==       -8
              8B1000 ==        8
 00B1111111111111111 ==    65535
 16B1111111111111111 ==       -1
 32B1111111111111111 ==    65535

```



## Nim


```nim
var x: int
x = 0b1011010111
x = 0b10_1101_0111
x = 0o1327
x = 0o13_27
x = 727
x = 727_000_000
x = 0x2d7
x = 0x2d7_2d7

# Literals of specific size:
var a = -127'i8 # 8 bit Integer
var b = -128'i16
var c = -129'i32
var d = -129'i64
var e = 126'u # Unsigned Integer
var f = 127'u8 # 8 bit uint
var g = 128'u16
var h = 129'u32
var i = 130'u64
```



## Objeck

As of v1.1, Objeck only supports hexadecimal and decimal literals.

```objeck

bundle Default {
  class Literal {
    function : Main(args : String[]) ~ Nil {
      (727 = 0x2d7)->PrintLine();
    }
  }
}

```



## OCaml


(This is an interactive ocaml session)

Bin(leading 0b or 0B), Oct(leading 0o or 0O), Hex(leading 0x or 0X)

```ocaml
# 727 = 0b1011010111;;
- : bool = true
# 727 = 0o1327;;
- : bool = true
# 727 = 0x2d7;;
- : bool = true
# 12345 = 12_345 (* underscores are ignored; useful for keeping track of places *);;
- : bool = true
```


Literals for the other built-in integer types:
* <tt>727l</tt> - int32
* <tt>727L</tt> - int64
* <tt>727n</tt> - nativeint


## Oforth


Integers can be expressed into base 10 (default), base 16 (using 0x prefix) or base 2 (using 0b prefix).


Those prefixes can be used for arbitrary precision integers :

{{out}}

```txt

>0b100000000000000000000000000 println
67108864
ok
>0xFFFFFFFFFFFFFFFFFFFFFFFFFFF println
324518553658426726783156020576255
ok

```



## Oz

To demonstrate the different numerical bases, we unify the identical values:

```oz
try
   %% binary      octal   dec.  hexadecimal
   0b1011010111 = 01327 = 727 = 0x2d7
   {Show success}
catch _ then
   {Show unexpectedError}
end
```


Negative integers start with "~":

```oz
X = ~42
```



## PARI/GP

GP allows input in binary <code>0b11</code> and hexadecimal <code>0xff</code>. PARI of course supports precisely those bases supported by [[#C|C]].


## Pascal

See [[Literals/Integer#Delphi | Delphi]]

FreePascal also supports octal (with leading ampersand) and binary (with leading percent sigh) literals:


```Pascal
const
  INT_VALUE = 15;
  OCTAL_VALUE = &017;
  BINARY_VALUE = %1111;
```



## Perl



```perl
print "true\n" if ( 727 == 0x2d7 &&
                    727 == 01327 &&
                    727 == 0b1011010111 &&
                    12345 == 12_345   # underscores are ignored; useful for keeping track of places
                  );
```



## Perl 6

These all print 255.

```perl6
say 255;
say 0d255;
say 0xff;
say 0o377;
say 0b1111_1111;

say :10<255>;
say :16<ff>;
say :8<377>;
say :2<1111_1111>;
say :3<100110>;
say :4<3333>;
say :12<193>;
say :36<73>;
```

There is a specced form for bases above 36, but rakudo does not yet implement it.


## Phix

Phix supports more bases and number formats than average. Standard decimals and hexadecimals are eg 255=#FF.
For hexadecimal numbers you can use upper or lower case for digits above 9 (A..F or a..f).

Phix also supports 0b01, 0o07, (0t07,) 0d09, and 0x0F for binary, octal, (octal,) decimal, and hexadecimal values.
(The only difference between 0o07 and 0t07 is personal preference.) There is no difference whatsoever between 1 and 1.0.

Given the need for 2, 8, 10, and 16, rather than four routines I wrote one that could handle all of them, and trivially
extended it to cope up to base 36. Thus Phix (also) allows any base between 2 and 36, using the notation o(<base>)digits,
eg o(7)16 is the base 7 representation of the decimal 13 (ie 1*7^1 + 6*7^0).
Phix does not however support "leading 0 is octal", or "trailing h is hex" or any other trailing qualifiers.
There is also a specialist "bytewise octal" that I personally wanted for x86 opcodes/listing files, eg 0ob377377377377==#FFFFFFFF.

An integer literal representing a character code can also be expressed by surrounding the character with single quotes, for example the statement <code>for i='A' to 'Z'</code> is/behaves exactly the same as <code>for i=65 to 90</code>.

Elements (8-bit characters) of an ansi string can likewise be treated as integers. Strings representing a number can/must be converted using eg scanf().

In the 32-bit version, integers outside -1,073,741,824 to +1,073,741,823 must be stored as atoms, which [ie a 64-bit float] can (accurately) store integers up to 9,007,199,254,740,992:
between 9,007,199,254,740,992 and 18,014,398,509,481,984 you can only store even numbers, and between 18,014,398,509,481,984 and 36,028,797,018,963,968, you can only store numbers divisible by 4, and so on. (ie as you need more and more bits on the front, eventually bits must start falling off the end)

In the 64-bit version the limits of integers are -4,611,686,018,427,387,904 to +4,611,686,018,427,387,903. Offhand I don't know
the exact highest integer an 80-bit float can hold, but the limits should all be 512* the values given above for 32-bit.

The included bigatom library allows working with extremely large integers with arbitrary precision, but obviously will be somewhat slower than using native integers/atoms.

```Phix
?{65,#41,'A',scanf("55","%d"),0o10,0(7)11}
```

{{out}}

```txt

{65,65,65,{{55}},8,8}

```



## PHP



```php
<?php
if ( 727 == 0x2d7 &&
     727 == 01327 )
    echo "true\n";
?>
```



## PicoLisp

In the strict sense of this task, PicoLisp reads only integers at bases which are a power of ten (scaled fixpoint numbers). This is controlled via the global variable '[http://software-lab.de/doc/refS.html#*Scl *Scl]':

```PicoLisp
: (setq *Scl 4)
-> 4

: 123.456789
-> 1234568
```

However, the reader is normally augmented by read macros, which can read any
base or any desired format. Read macros are not executed at runtime, but
intially when the sources are read.

```PicoLisp
: '(a `(hex "7F") b `(oct "377") c)
-> (a 127 b 255 c)
```

In addition to standard formats like
'[http://software-lab.de/doc/refH.html#hex hex]' (hexadecimal) and
'[http://software-lab.de/doc/refO.html#oct oct]' (octal),
there are also more esoteric formats like
'[http://software-lab.de/doc/refF.html#fmt64 fmt64]' (base 64) and
'[http://software-lab.de/doc/refH.html#hax hax]' (hexadecimal numbers
coded with alphabetic characters).


## PL/I


```PL/I

12345
'b4'xn           /* a hexadecimal literal integer.            */
'ffff_ffff'xn    /* a longer hexadecimal hexadecimal integer. */
1101b             /* a binary integer, of value decimal 13.   */

```



## PostScript

Integer literals in PostScript can be either standard decimal literals or in the form ''base''<code>#</code>''number''. ''base'' can be any decimal integer between 2 and 36, ''number'' can then use digits from <code>0</code> to ''base'' − 1. Digits above <code>9</code> are replaced by <code>A</code> through <code>Z</code> and case does not matter.

```postscript
123      % 123
8#1777   % 1023
16#FFFE  % 65534
2#11011  % 27
5#44     % 24
```



## PowerShell

PowerShell only supports base 10 and 16 directly:

```powershell
727     # base 10
0x2d7   # base 16
```

Furthermore there are special suffices which treat the integer as a multiple of a specific power of two, intended to simplify file size operations:

```powershell
3KB  # 3072
3MB  # 3145728
3GB  # 3221225472
3TB  # 3298534883328
```

A number can be suffixed with <code>d</code> to make it a <code>decimal</code>. This doesn't work in conjunction with above suffixes, though:

```txt
PS> 4d.GetType().ToString()
System.Decimal
```



## PureBasic

PureBasic allows integer literals to be specified in base 10, base 2 by using the prefix '%', or base 16 by using the prefix '$'.

```PureBasic
x = 15     ;15 in base 10
x = %1111  ;15 in base 2
x = $f     ;15 in base 16
```

An integer literal representing a character code can also be expressed by surrounding the character with single quotes. More than one character can be included in the single quotes (i.e. 'abc').  Depending on whether code is compiled in Ascii or Unicode mode this will result in the integer value being specified in base 256 or base 65536 respectively.


```PureBasic
x = 'a'     ;129
```



## Python

{{works with|Python|3.0}}
Python 3.0 brought in the binary literal and uses 0o or 0O exclusively for octal.

```python>>>
 # Bin(leading 0b or 0B), Oct(leading 0o or 0O), Dec, Hex(leading 0x or 0X), in order:
>>> 0b1011010111 == 0o1327 == 727 == 0x2d7
True
>>>
```

{{works with|Python|2.6}}
Python 2.6 has the binary and new octal formats of 3.0, as well as keeping the earlier leading 0 octal format of previous 2.X versions for compatability.

```python>>>
 # Bin(leading 0b or 0B), Oct(leading 0o or 0O, or just 0), Dec, Hex(leading 0x or 0X), in order:
>>> 0b1011010111 == 0o1327 == 01327 == 727 == 0x2d7
True
>>>
```

{{works with|Python|2.5}}

```python>>>
 # Oct(leading 0), Dec, Hex(leading 0x or 0X), in order:
>>> 01327 == 727 == 0x2d7
True
>>>
```


In Python 2.x you may also specify a <tt>long</tt> literal by adding an <tt>l</tt> or <tt>L</tt> (the latter form is preferred as the former looks like a "1") to the end (ex: <tt>574298540721727L</tt>), but this is optional, as integer literals that are too large for an <tt>int</tt> will be interpreted as a <tt>long</tt>.


## R

0x or 0X followed by digits or the letters a-f denotes a hexadecimal number.  The suffix L means that the number should be stored as an integer rather than numeric (floating point).

```R
0x2d7==727            # TRUE
identical(0x2d7, 727) # TRUE
is.numeric(727)       # TRUE
is.integer(727)       # FALSE
is.integer(727L)      # TRUE
is.numeric(0x2d7)     # TRUE
is.integer(0x2d7)     # FALSE
is.integer(0x2d7L)    # TRUE
```

For more information, see [http://cran.r-project.org/doc/manuals/R-lang.pdf Section 10.3.1 of the R Language definition] (PDF).


## Racket



```racket

#lang racket
#b1011010111
#o1327
#d727
#x2d7

```


Output:

```txt

727
727
727
727

```



## REBOL


```rebol>1</lang



## Retro


```Retro
#100 ( decimal )
%100 ( binary  )
$100 ( hex     )
'c   ( ascii character )
100  ( number in current base )
```


Numbers without a prefix are interpreted using the current '''base''', which is a variable Valid characters are stored in a string called '''numbers''', which can also be altered to allow for larger bases.


## REXX


```rexx
/*REXX pgm displays an  integer  (expressed in the pgm as a literal)  in different bases*/
                                      /*────────── expressing decimal numbers ──────────*/
ddd =  123                            /*a decimal number  (expressed as a literal).     */
ddd = '123'                           /*this is exactly the same as above.              */
ddd = "123"                           /*this is exactly the same as above also.         */
                                      /*────────── expressing hexadecimal numbers ──────*/
hhh = '7b'x                           /*a value,  expressed as a hexadecimal literal.   */
hhh = '7B'x                           /* (same as above)  using a capital  "B".         */
hhh = '7B'X                           /* (same as above)  using a capital  "X".         */
cow = 'dead beef'x                    /*another value,    with a blank for the eyeballs.*/
cow = 'de ad be ef'x                  /* (same as above)  with  blanks for the eyeballs.*/
                                      /*────────── expressing binary numbers ───────────*/
bbb =  '1111011'b                     /*a value,  expressed as a binary literal.        */
bbb = '01111011'b                     /* (same as above)  with a full 8 binary digits.  */
bbb = '0111 1011'b                    /* (same as above)  with a blank for the eyeballs.*/

say '    base  10='            ddd
say '    base   2='  x2b( d2x( ddd ) )
say '    base  16='       d2x( ddd )
say '    base 256='       d2c( ddd )  /*the output displayed is ASCII (or maybe EBCDIC).*/

thingy1=  +123                        /*╔══════════════════════════════════════════════╗*/
thingy2= '+123'                       /*║ All of the THINGYs variables aren't strictly ║*/
thingy3= ' 123'                       /*║ (exactly)  equal to the  DDD  variable,  but ║*/
thingy4=   123.                       /*║ they do compare numerically equal.   When    ║*/
thingy5=    12.3e+1                   /*║ compared numerically, numbers are rounded to ║*/
thingy6=  1230e-1                     /*║ the current setting of  NUMERIC DIGITS.  The ║*/
thingy7=  1230E-0001                  /*║ default for  (decimal)  NUMERIC DIGITS is  9 ║*/
thingy8= ' +     123  '               /*╚══════════════════════════════════════════════╝*/

                                                 /*stick a fork in it,  we're all done. */
```

{{out|output}}

```txt

    base  10= 123
    base   2= 01111011
    base  16= 7B
    base 256= {

```


On TSO d2c(37) does not result in a displayable character.
With thing=c2d('A') I see:

```txt

base  10= 193
base   2= 11000001
base  16= C1
base 256= A

```

The first three lines are platform-independent.


## Ring


```ring

see "Decimal literal = " + 1234 + nl
see "Hexadecimal literal = " + dec("4D2") + nl
see "Octal Literal = " + octal(668) + nl
see "Binary literal = " + bintodec("10011010010")

func bintodec(bin)
     binsum = 0
     for n=1  to len(bin)
         binsum = binsum + number(bin[n]) *pow(2, len(bin)-n)
     next
     return binsum

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

Output:

```txt

Decimal literal = 1234
Hexadecimal literal = 1234
Octal Literal = 1234
Binary literal = 1234

```



## Ruby



```ruby>727 == 0b1011010111  # =
 true, binary
727 == 0x2d7   # => true, hex
727 == 0o1327  # => true, octal
727 == 01327   # => true, octal

12345 == 12_345 # => true underscores are ignored; useful for keeping track of places

```



## Rust


```rust
10     // Decimal
0b10   // Binary
0x10   // Hexadecimal
0o10   // Octal
1_000  // Underscores may appear anywhere in the numeric literal for clarity
10_i32 // The type (in this case i32, a 32-bit signed integer) may also be appended.
10i32  // With or without underscores
```



## Scala

Scala has signed integers of 8, 16, 32 and 64 bits. They can be represented in decimal, octal by prefixing
<code>0</code>, or hexadecimal by prefixing <code>0x</code> or <code>0X</code>. Without any other type hint,
it defaults to 32 bits integers, or an <code>Int</code>. An <code>l</code> or <code>L</code> suffix will
indicate a 64 bits integer, or a <code>Long</code>. The other two types, <code>Byte</code> and <code>Short</code>,
can be represented using type ascription, as shown below.


```txt

scala> 16
res10: Int = 16

scala> 020L
res11: Long = 16

scala> 0x10 : Byte
res12: Byte = 16

scala> 16 : Short
res13: Short = 16

scala> 020 : Int
res14: Int = 16

scala> 0x10 : Long
res15: Long = 16

```



## Scheme


(This is an interactive scheme session)

binary: #b, octal: #o, decimal: #d (optional obviously), hex: #x

```scheme>
 (= 727 #b1011010111)
#t
> (= 727 #o1327)
#t
> (= 727 #d727)
#t
> (= 727 #x2d7)
#t
```



## Seed7

In [[Seed7]] integer literals may have the form <base>#<numeral>. Here <base> can be from the range 2..36. For example:

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln(727);
    writeln(32#MN);
    writeln(16#2D7);
    writeln(10#727);
    writeln(8#1327);
    writeln(2#1011010111);
  end func;

```

Sample output:

```txt

727
727
727
727
727
727

```


## Sidef


```ruby
say 255;
say 0xff;
say 0377;
say 0b1111_1111;
```

{{out}}

```txt
255
255
255
255
```



## Slate



```slate>2r1011010111 + 8r1327 + 10r727 + 16r2d7 / 4</lang



## Smalltalk



```smalltalk>2r1011010111 + 5r100 + 8r1327 + 10r727 + 16r2d7 / 4</lang

binary, base-5, octal, decimal, binary, decimal (default). The radix is any between 2 and 32 (although only 2, 8, 10 and 16 are typically needed).

There is no size limit (except memory constraints), the runtime chooses an appropriate representation automatically:

```smalltalk
16r1B30964EC395DC24069528D54BBDA40D16E966EF9A70EB21B5B2943A321CDF10391745570CCA9420C6ECB3B72ED2EE8B02EA2735C61A000000000000000000000000 = 100 factorial
"evaluates to true"
```



## Standard ML

(This is an interactive SML/NJ session)

Hex(leading 0x), Word (unsigned ints, leading 0w), Word Hex (leading 0wx)

```sml
- 727 = 0x2d7;
val it = true : bool
- 727 = Word.toInt 0w727;
val it = true : bool
- 0w727 = 0wx2d7;
val it = true : bool
- ~727; (* negative number;
         * ~ is the unary negation operator for all numbers, including reals and ints;
         * worth mentioning because it's unusual
         *)
val it = ~727 : int
```


## Stata


Stata does not have an integer type, except for dataset storage, in order to reduce data size in memory or on disk. Computations are done with floating-point doubles, which can hold exact integers in the range -9007199254740992 to 9007199254740992 (that is, -2^53 to 2^53). Only decimal literals are supported.


## Swift


```Swift
let hex = 0x2F // Hexadecimal
let bin = 0b101111 // Binary
let oct = 0o57 // Octal
```



## Tcl

{{works with|Tcl|8.5}}
(This is an interactive tclsh session; <tt>expr</tt> is only called to evaluate the equality test.)

```tcl
% expr 727 == 0x2d7
1
% expr 727 == 0o1327
1
% expr 727 == 01327
1
% expr 727 == 0b1011010111
1
```


=={{header|TI-89 BASIC}}==

Binary, decimal, and hexadecimal are supported. The system base mode sets the default output base, but does not affect input; unmarked digits are always decimal.


```ti89b>0b10000001 = 129 = 0h81</lang



## UNIX Shell

The <tt>expr</tt> command accepts only decimal literals.


```bash
$ expr 700 - 1
699
$ expr 0700 - 01
699
```


Some shells have arithmetic expansion. These shells may accept literals in other bases. This syntax only works in places that do arithmetic expansion, such as in <tt>$(( ))</tt>, or in Bash's <tt>let</tt> command.

Quoting the manual page of [[pdksh]]:

 Integer constants may be specified with arbitrary bases using the
 notation <u>base</u>#<u>number</u>, where <u>base</u> is a decimal integer specifying the
 base, and <u>number</u> is a number in the specified base.  Additionally,
 integers may be prefixed with `0X' or `0x' (specifying base 16) or `0'
 (base 8) in all forms of arithmetic expressions, except as numeric
 arguments to the '''test''' command.

[[pdksh]] allows bases from 2 to 36. The letters a-z or A-Z represent numbers 10 to 35.

[[Bash]] allows the same syntax as pdksh. In addition, Bash can handle bases as high as 64: the symbols used are digits, lowercase letters, uppercase letters, @ and _ <cite>in that order</cite>; <cite>if the BASE is less than or equal to 36, lowercase and uppercase letters can be used interchangeably to represent number from 10 and 35.</cite> (From the info manual of the Bash).

{{works with|bash}}

```bash
dec=727
oct=$(( 01327 ))
bin=$(( 2#1011010111 ))
hex=$(( 0x2d7 ))
# or e.g.
let bin=2#1011010111
let "baseXX = 20#1g7"
```


{{works with|pdksh|5.2.14}}

```bash
dec=727
oct=$(( 01327 ))
bin=$(( 2#1011010111 ))
hex=$(( 0x2d7 ))
# or e.g.
(( bin = 2#1011010111 ))
(( baseXX = 20#1g7 ))
```



## Ursa

Ursa supports signed, base-10 integers.

```ursa
decl int i
set i 123
set i -456
```



## Ursala


Natural numbers (i.e., unsigned integers) of any size are supported. Only decimal integer literals are recognized by the compiler, as in a declaration such as the following.

```Ursala>n = 724</lang

Signed integers are also recognized and are considered a separate type from natural numbers, but non-negative integers and natural numbers have compatible binary representations.

```Ursala
z = -35
```

Signed rational numbers of unlimited precision are yet another primitive type and can be expressed
in conventional decimal form.

```Ursala
m = -2/3
```

The forward slash in a rational literal is part of the syntax and not a division operator. Finally, a signed or unsigned integer with a trailing underscore, like this

```Usala
t = 4534934521_
```

is used for numbers stored in binary converted decimal format, also with unlimited precision, which may perform better in applications involving very large decimal numbers.


## Verbexx


```verbexx
//    Integer Literals:
//
//    If present, base prefix must be:    0b 0B (binary) 0o 0O (octal)
//                                        0x 0X (hex)
//
//    If present, length suffix must be:  i I i64 I64 (INT64_T)
//                                        u U u64 U64 (UINT64_T)
//                                        i32 I32 (INT32_T) u32 U32 (UINT32_T)
//                                        i16 I16 (INT16_T) u16 U16 (UINT16_T)
//                                        i8  I8  (INT8_T)  u8  U8  (UINT8_T)
//                                        u1  U1  (BOOL_T)  u0  U0  (UNIT_T)
//                                        iV  iv  Iv IV             (INTV_T)

//     Binary       Octal         Decimal       Hexadecimal
//     ------------ ----------    ------------  --------------
@SAY  0b1101        0o016         19999999      0xFfBBcC0088   ; // INT64_T
@SAY  0B0101        0O777        -12345678      0X0a2B4c6D8eA  ; // INT64_T
@SAY  0b1101I64     0o707I64      12345678i64   0xFfBBcC00i64  ; // INT64_T
@SAY  0b1101I       0o57707i     -2345678I      0xfafbbCc99i   ; // INT64_T
@SAY  0b1001U64     0o555u64      33345678u64   0xFfaBcC00U64  ; // UINT64_T
@SAY  0b10010100U   0o1234567u    3338u         0x99faBcC0EU   ; // UINT64_T
@SAY  0B0101i32     0O753I32      987654i32     0XAAb4cCeeI32  ; // INT32_T
@SAY  0B0101u32     0O573u32      987654U32     0X0BAb4cCeeU32 ; // UINT32_T
@SAY  0B0101i16     0O753i16     -017654I16     0X000cCffi16   ; // INT16_T
@SAY  0B0101u16     0O633U16      27654U16      0X000dDbBu16   ; // UINT16_T
@SAY  0B0101i8      0O153i8      -000114I8      0X000ffi8      ; // INT8_T
@SAY  0B0101u8      0O132U8       00094U8       0X0000bu8      ; // UINT8_T
@SAY  0b0u1         0o0u1         00u1 0U1      0x000u1        ; // BOOL_T (FALSE)
@SAY  0B001u1       0O1u1         1u1 01U1      0X1u1 0x001U1  ; // BOOL_T (TRUE )
@SAY  0b0u0         0o000u0       00u0 0U0      0x0u0 0X000U0  ; // UNIT_T
@SAY                             -1234iV                       ; // INTV_T (cpp_int)
@SAY                              56781234Iv                   ; // INTV_T (cpp_int)

//  note: _ (underscores) can appear in the main numeric part of the literal,
//        after any base prefix, and before any length suffix.  If there is
//        no prefix, the numeric literal cannot begin with underscore:

@SAY 100_000  1_u1  0x_FFFF_u16  1__0__  0x__7890_ABCD_EFAB_CDEF__u64;
```



## Visual Basic

{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|Access 97}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}
Integer literals can be expressed in octal, decimal and hexadecimal form.

```vb
Sub Main()

'Long:    4 Bytes (signed), type specifier = &
Dim l1 As Long, l2 As Long, l3 As Long
'Integer: 2 Bytes (signed), type specifier = %
Dim i1 As Integer, i2 As Integer, i3 As Integer
'Byte:    1 Byte (unsigned), no type specifier
Dim b1 As Byte, b2 As Byte, b3 As Byte

  l1 = 1024&
  l2 = &H400&
  l3 = &O2000&
  Debug.Assert l1 = l2
  Debug.Assert l2 = l3

  i1 = 1024
  i2 = &H400
  i3 = &O2000
  Debug.Assert i1 = i2
  Debug.Assert i2 = i3

  b1 = 255
  b2 = &O377
  b3 = &HFF
  Debug.Assert b1 = b2
  Debug.Assert b2 = b3

End Sub
```



## XPL0


```XPL0
code CrLf=9, IntOut=11;
def A=123, B=$123, C=%11_0011, D=^A;
[IntOut(0, A);  CrLf(0);        \decimal
 IntOut(0, B);  CrLf(0);        \hex
 IntOut(0, C);  CrLf(0);        \binary
 IntOut(0, D);  CrLf(0);        \ASCII
]
```


Output:

```txt

123
291
51
65

```



## zkl

Three int types the compiler understands: decimal, hex, binary. Other bases (2-36) require a method call.

```zkl
123, 0d1_000
0x123, 0x12|34
0b1111|0000
```



{{omit from|ML/I}}
