+++
title = "Character codes"
description = ""
date = 2019-09-10T10:31:22Z
aliases = []
[extra]
id = 3118
[taxonomies]
categories = ["Text processing", "Basic language learning", "String manipulation", "task"]
tags = []
+++

## Task
[[Category:Simple]]


;Task:
Given a character value in your language, print its code   (could be ASCII code, Unicode code, or whatever your language uses).


;Example:
The character   'a'   (lowercase letter A)   has a code of 97 in ASCII   (as well as Unicode, as ASCII forms the beginning of Unicode).

Conversely, given a code, print out the corresponding character.





## 11l


```11l
print(‘a’.code)       // prints "97"
print(Char(code' 97)) // prints "a"
```



## 360 Assembly

S/360 architecture and EBCDIC was born together.
In EBCDIC, the character 'a' (lowercase letter A) has a code of 129 in decimal and '81'x in hexadecimal.
To perform conversion, we use IC (insert character) and STC (store character) opcodes.

```360asm
*        Character codes EBCDIC    15/02/2017
CHARCODE CSECT
         USING  CHARCODE,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
* Character to Decimal
         SR     R1,R1              r1=0
         IC     R1,=C'a'           insert character 'a'
         XDECO  R1,PG
         XPRNT  PG,L'PG            print -> 129
* Hexadecimal to character
         SR     R1,R1              r1=0
         IC     R1,=X'81'          insert character X'81'
         STC    R1,CHAR            store character r1
         XPRNT  CHAR,L'CHAR        print -> 'a'
* Decimal to character
         LH     R1,=H'129'         r1=129
         STC    R1,CHAR            store character r1
         XPRNT  CHAR,L'CHAR        print -> 'a'
*
         XDUMP  CHAR,L'CHAR        dump -> X'81'
*
RETURN   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
PG       DS     CL12
CHAR     DS     CL1
         YREGS
         END    CHARCODE
```

{{out}}

```txt

         129
a
a

```



## ABAP

In ABAP you must first cast the character to a byte field and back to a number in order to get its ASCII value.

```ABAP
report zcharcode
data: c value 'A', n type i.
field-symbols <n> type x.

assign c to <n> casting.
move <n> to n.
write: c, '=', n left-justified.
```

{{Out}}
```txt
A = 65
```



## ACL2

Similar to Common Lisp:

```Lisp
(cw "~x0" (char-code #\a))
(cw "~x0" (code-char 97))
```



## ActionScript

In ActionScript, you cannot take the character code of a character directly. Instead you must create a string and call charCodeAt with the character's position in the string as a parameter.

```ActionScipt
trace(String.fromCharCode(97)); //prints 'a'
trace("a".charCodeAt(0));//prints '97'
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Char_Code is
begin
   Put_Line (Character'Val (97) & " =" & Integer'Image (Character'Pos ('a')));
end Char_Code;
```

The predefined language attributes S'Pos and S'Val for every discrete subtype, and Character is such a type, yield the position of a value and value by its position correspondingly.
{{out}}

```txt
a = 97
```



## Aime


```aime
# prints "97"
o_integer('a');
o_byte('\n');
# prints "a"
o_byte(97);
o_byte('\n');
```



## ALGOL 68

In ALGOL 68 the '''format''' $g$ is type aware, hence the type conversion operators '''abs''' & '''repr''' are used to set the type.

```algol68
main:(
  printf(($gl$, ABS "a")); # for ASCII this prints "+97" EBCDIC prints "+129" #
  printf(($gl$, REPR 97))  # for ASCII this prints "a"; EBCDIC prints "/" #
)
```

''Character conversions'' may be available in the ''standard prelude'' so that when
a foreign tape is mounted, the characters will be converted transparently as the tape's
records are read.

```algol68
FILE tape;
INT errno = open(tape, "/dev/tape1", stand out channel)
make conv(tape, ebcdic conv);
FOR record DO getf(tape, ( ~ )) OD; ~ # etc ... #
```

Every '''channel''' has an associated standard character conversion that can be determined
using the ''stand conv'' query routine and then the conversion applied to a particular
file/tape. eg.

```algol68
 make conv(tape, stand conv(stand out channel))
```



## ALGOL W


```algolw
begin
    % display the character code of "a" (97 in ASCII)                        %
    write( decode( "a" ) );
    % display the character corresponding to 97 ("a" in ASCII)               %
    write( code( 97 ) );
end.
```



## APL

{{works with|Dyalog APL}}
In Dyalog, <tt>⎕UCS</tt> with an integer returns the corresponding Unicode character:

```apl
      ⎕UCS 97
a
```

and <tt>⎕UCS</tt> with a character returns the corresponding code:

```apl
      ⎕UCS 'a'
97
```

Like most things in APL, <tt>⎕UCS</tt> can also be used with an array or with a string (which is an array of characters):

```apl
      ⎕UCS 65 80 76
APL
      ⎕UCS 'Hello, world!'
72 101 108 108 111 44 32 119 111 114 108 100 33
```



## AppleScript


```AppleScript
log(id of "a")
log(id of "aA")
```

{{out}}

```txt
(*97*)
(*97, 65*)
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program character.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessCodeChar: .ascii "The code of character is :"
sZoneconv:		 .fill 12,1,' '
szCarriageReturn:  .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    mov r0,#'A'
    ldr r1,iAdrsZoneconv
    bl conversion10S
    ldr r0,iAdrszMessCodeChar
    bl affichageMess
    mov r0,#'a'
    ldr r1,iAdrsZoneconv
    bl conversion10S
    ldr r0,iAdrszMessCodeChar
    bl affichageMess
    mov r0,#'1'
    ldr r1,iAdrsZoneconv
    bl conversion10S
    ldr r0,iAdrszMessCodeChar
    bl affichageMess

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrsZoneconv:		.int  sZoneconv
iAdrszMessCodeChar:		.int szMessCodeChar
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save  registres */
    push {r0,r1,r2,r7}    		/* save others registers */
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
    swi #0                      /* call systeme */
    pop {r0,r1,r2,r7}     		/* restaur others registers */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */

/***************************************************/
/*   conversion register signed décimal     */
/***************************************************/
/* r0 contient le registre   */
/* r1 contient l adresse de la zone de conversion */
conversion10S:
    push {r0-r5,lr}    /* save des registres */
    mov r2,r1       /* debut zone stockage */
    mov r5,#'+'     /* par defaut le signe est + */
    cmp r0,#0       /* nombre négatif ? */
    movlt r5,#'-'     /* oui le signe est - */
    mvnlt r0,r0       /* et inversion en valeur positive */
    addlt r0,#1
    mov r4,#10   /* longueur de la zone */
1: /* debut de boucle de conversion */
    bl divisionpar10 /* division  */
    add r1,#48        /* ajout de 48 au reste pour conversion ascii */
    strb r1,[r2,r4]  /* stockage du byte en début de zone r5 + la position r4 */
    sub r4,r4,#1      /* position précedente */
    cmp r0,#0
    bne 1b	       /* boucle si quotient different de zéro */
    strb r5,[r2,r4]  /* stockage du signe à la position courante */
    subs r4,r4,#1   /* position précedente */
    blt  100f         /* si r4 < 0  fin  */
    /* sinon il faut completer le debut de la zone avec des blancs */
    mov r3,#' '   /* caractere espace */
2:
    strb r3,[r2,r4]  /* stockage du byte  */
    subs r4,r4,#1   /* position précedente */
    bge 2b        /* boucle si r4 plus grand ou egal a zero */
100:  /* fin standard de la fonction  */
    pop {r0-r5,lr}   /*restaur desregistres */
    bx lr

/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 contient le dividende   */
/* r0 retourne le quotient */
/* r1 retourne le reste  */
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
   bx lr                  /* leave function */
.Ls_magic_number_10: .word 0x66666667



```



## AutoHotkey


```AutoHotkey
MsgBox % Chr(97)
MsgBox % Asc("a")
```



## AWK

AWK has no built-in way to convert a character into ASCII (or whatever) code;
but a function that does so can be easily built using an associative array (where the keys are the characters).
The opposite can be done using <tt>printf</tt> (or <tt>sprintf</tt>) with <tt>%c</tt>

```awk
function ord(c)
{
  return chmap[c]
}
BEGIN {
  for(i=0; i < 256; i++) {
    chmap[sprintf("%c", i)] = i
  }
  print ord("a"), ord("b")
  printf "%c %c\n", 97, 98
  s = sprintf("%c%c", 97, 98)
  print s
}
```



## Axe


```axe
Disp 'a'▶Dec,i
Disp 97▶Char,i
```



## Babel



```babel
'abcdefg' str2ar
{%d nl <<} eachar
```


{{Out}}
```txt

97
98
99
100
101
102
103

```



```babel
(98 97 98 101 108) ls2lf ar2str nl <<

```

{{out}}
 babel


## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
charCode = 97
char = "a"
PRINT CHR$(charCode) 'prints a
PRINT ASC(char) 'prints 97
```


On the ZX Spectrum string variable names must be a single letter but numeric variables can be multiple characters:
{{works with|ZX Spectrum Basic}}

```zxbasic
10 LET c = 97: REM c is a character code
20 LET d$ = "b": REM d$ holds the character
30 PRINT CHR$(c): REM this prints a
40 PRINT CODE(d$): REM this prints 98
```


=
## Applesoft BASIC
=
CHR$(97) is used in place of "a" because on the older model Apple II, lower case is difficult to input.

```qbasic
?CHR$(97)"="ASC(CHR$(97))
```

{{Out}}
```txt
a=97
```


Output as it appears on the text display on the Apple II and Apple II plus, with the original text character ROM:

```txt
!=97
```


=
## BaCon
=

```qbasic
' ASCII
c$ = "$"
PRINT c$, ": ", ASC(c$)

' UTF-8
uc$ = "€"
PRINT uc$, ": ", UCS(uc$), ", ", UCS(c$)
```


{{out}}

```txt

$: 36
€: 8364, 36
```


=
## Sinclair ZX81 BASIC
=

```basic
10 REM THE ZX81 USES ITS OWN NON-ASCII CHARACTER SET
20 REM WHICH DOES NOT INCLUDE LOWER-CASE LETTERS
30 PRINT CODE "A"
40 PRINT CHR$ 38
```

{{out}}

```txt
38
A
```


=
## Commodore BASIC
=
Commodore BASIC uses PETSCII code for its character set.

```gwbasic
10 CH = 65:        REM IN PETSCII CODE FOR 'A' IS 65
20 D$ = "B":       REM D$ HOLDS THE CHARACTER 'B'
30 PRINT CHR$(CH): REM THIS PRINTS 'A'
40 PRINT ASC(D$):  REM THIS PRINTS 66
```

{{Out}}
```txt
A
 66
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PRINT ORD("A")
110 PRINT CHR$(65)
```



## Batch File


```dos

@echo off

:: Supports all ASCII characters and codes from 34-126 with the exceptions of:
:: 38  &
:: 60  <
:: 62  >
:: 94  ^
:: 124 |

:_main
call:_toCode a
call:_toChar 97
pause>nul
exit /b

:_toCode
setlocal enabledelayedexpansion
set codecount=32

for /l %%i in (33,1,126) do (
  set /a codecount+=1
  cmd /c exit %%i
  if %1==!=exitcodeAscii! (
    echo !codecount!
    exit /b
  )
)

:_toChar
setlocal
cmd /c exit %1
echo %=exitcodeAscii%
exit /b

```

{{in}}

```txt

toCode a
toChar 97

```

{{out}}

```txt

97
a

```



## BBC BASIC


```bbcbasic
      charCode = 97
      char$ = "a"
      PRINT CHR$(charCode) : REM prints a
      PRINT ASC(char$) : REM prints 97
```



## Befunge

The instruction <tt>.</tt> will output as an integer. <tt>,</tt> will output as ASCII character.

```befunge
"a". 99*44*+, @
```



## Bracmat


```bracmat
( put
$ ( str
  $ ( "\nLatin a
        ISO-9959-1: "
      asc$a
      " = "
      chr$97
      "
             UTF-8: "
      utf$a
      " = "
      chu$97
      \n
      "Cyrillic а (UTF-8): "
      utf$а
      " = "
      chu$1072
      \n
    )
  )
)
```

{{Out}}
```txt
Latin a
        ISO-9959-1: 97 = a
             UTF-8: 97 = a
Cyrillic а (UTF-8): 1072 = а
```



## C

<tt>char</tt> is already an integer type in C, and it gets automatically promoted to <tt>int</tt>. So you can use a character where you would otherwise use an integer. Conversely, you can use an integer where you would normally use a character, except you may need to cast it, as <tt>char</tt> is smaller.


```c
#include <stdio.h>

int main() {
  printf("%d\n", 'a'); /* prints "97" */
  printf("%c\n", 97); /* prints "a"; we don't have to cast because printf is type agnostic */
  return 0;
}
```



## C++

<tt>char</tt> is already an integer type in C++, and it gets automatically promoted to <tt>int</tt>. So you can use a character where you would otherwise use an integer. Conversely, you can use an integer where you would normally use a character, except you may need to cast it, as <tt>char</tt> is smaller.

In this case, the output operator <tt><<</tt> is overloaded to handle integer (outputs the decimal representation) and character (outputs just the character) types differently, so we need to cast it in both cases.

```cpp
#include <iostream>

int main() {
  std::cout << (int)'a' << std::endl; // prints "97"
  std::cout << (char)97 << std::endl; // prints "a"
  return 0;
}
```


## C#
C# represents strings and characters internally as Unicode,
so casting a char to an int returns its Unicode character encoding.

```c#
using System;

namespace RosettaCode.CharacterCode
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine((int) 'a');   //Prints "97"
            Console.WriteLine((char) 97);   //Prints "a"
        }
    }
}
```



## Clojure


```clojure
(print (int \a)) ; prints "97"
(print (char 97)) ; prints \a

; Unicode is also available, as Clojure uses the underlying java Strings & chars
(print (int \π))  ; prints 960
(print (char 960)) ; prints \π

; use String because char in Java can't represent characters outside Basic Multilingual Plane
(print (.codePointAt "𝅘𝅥𝅮" 0)) ; prints 119136
(print (String. (int-array 1 119136) 0 1)) ; prints 𝅘𝅥𝅮
```



## COBOL

Tested with GnuCOBOL on an ASCII based GNU/Linux system.
Running this code on EBCDIC native hardware would display a control code and 000000093.

```COBOL
       identification division.
       program-id. character-codes.
       remarks. COBOL is an ordinal language, first is 1.
       remarks. 42nd ASCII code is ")" not, "*".
       procedure division.
       display function char(42)
       display function ord('*')
       goback.
       end program character-codes.
```


{{out}}

```txt
prompt$ cobc -xj character-codes.cob
)
000000043
```



## CoffeeScript

CoffeeScript transcompiles to JavaScript, so it uses the JS standard library.

```coffeescript
console.log 'a'.charCodeAt 0 # 97
console.log String.fromCharCode 97 # a
```



## Common Lisp


```lisp
(princ (char-code #\a)) ; prints "97"
(princ (code-char 97)) ; prints "a"
```



## Component Pascal

BlackBox Component Builder

```oberon2
PROCEDURE CharCodes*;
VAR
	c : CHAR;
BEGIN
	c := 'A';
	StdLog.Char(c);StdLog.String(":> ");StdLog.Int(ORD(c));StdLog.Ln;
	c := CHR(3A9H);
	StdLog.Char(c);StdLog.String(":> ");StdLog.Int(ORD(c));StdLog.Ln
END CharCodes;
```

{{Out}}

```txt
A:>  65
Ω:>  937
```



## D


```d
void main() {
    import std.stdio, std.utf;

    string test = "a";
    size_t index = 0;

    // Get four-byte utf32 value for index 0.
    writefln("%d", test.decode(index));

    // 'index' has moved to next character input position.
    assert(index == 1);
}
```

{{out}}

```txt
97
```



## Delphi

Example from Studio 2006.

```delphi
program Project1;

{$APPTYPE CONSOLE}

uses
  SysUtils;
var
  aChar:Char;
  aCode:Byte;
  uChar:WideChar;
  uCode:Word;
begin
  aChar := Chr(97);       Writeln(aChar);
  aCode := Ord(aChar);    Writeln(aCode);
  uChar := WideChar(97);  Writeln(uChar);
  uCode := Ord(uChar);    Writeln(uCode);

  Readln;
end.
```



## DWScript


```delphi
PrintLn(Ord('a'));
PrintLn(Chr(97));
```



## E


```e
? 'a'.asInteger()
# value: 97

? <import:java.lang.makeCharacter>.asChar(97)
# value: 'a'
```



## Eiffel

All characters are of the type CHARACTER_8 (ASCII encoding) or CHARACTER_32 (Unicode encoding). CHARACTER is a synonym for either of these two (depending on the compiler option). Characters can be assigned using character literals (a single character enclosed in single quotes) or code value notation (of the form '%/value/' where value is an integer literal of any of the recognized forms).

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
		local
			c8: CHARACTER_8
			c32: CHARACTER_32
		do
			c8 := '%/97/'			-- using code value notation
			c8 := '%/0x61/'			-- same as above, but using hexadecimal literal
			print(c8.natural_32_code)	-- prints "97"
			print(c8)			-- prints the character "a"

			c32 := 'a'			-- using character literal
			print(c32.natural_32_code)	-- prints "97"
			print(c32)			-- prints "U+00000061"

			--c8 := 'π'			-- compile-time error (c8 does not have enough range)
			c32 := 'π'			-- assigns Unicode value 960
		end
end

```


Limitations: There is no "put_character_32" feature for standard io (FILE class), so there appears to be no way to print Unicode characters.


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    var ch := $97;

    console.printLine:ch;
    console.printLine(ch.toInt())
}
```

{{out}}

```txt

a
97

```



## Elixir

A String in Elixir is a UTF-8 encoded binary.

```elixir
iex(1)> code = ?a
97
iex(2)> to_string([code])
"a"
```



## Emacs Lisp


```Lisp

(string-to-char "a")
(message "%c" 97)

```




## Erlang

In Erlang, lists and strings are the same, only the representation changes. Thus:

```erlang>1
 F = fun([X]) -> X end.
#Fun<erl_eval.6.13229925>
2> F("a").
97
```

If entered manually, one can also get ASCII codes by prefixing characters with <tt>$</tt>:

```erlang>3
 $a.
97
```

Unicode is fully supported since release R13A only.


## Euphoria


```Euphoria
printf(1,"%d\n", 'a') -- prints "97"
printf(1,"%s\n", 97) -- prints "a"
```


=={{header|F Sharp|F#}}==

```fsharp
let c = 'A'
let n = 65
printfn "%d" (int c)
printfn "%c" (char n)
```

{{Out}}
```txt
65
A
```



## Factor


```factor
CHAR: katakana-letter-a .
"ア" first .

12450 1string print
```



## FALSE


```false
'A."
"65,
```



## Fantom

A character is represented in single quotes: the 'toInt' method returns the code for the character.  The 'toChar' method converts an integer into its respective character.

```fantom>fansh
 97.toChar
a
fansh> 'a'.toInt
97
```



## Forth

As with C, characters are just integers on the stack which are treated as ASCII.

```forth
char a
dup .    \ 97
emit     \ a
```



## Fortran

Functions ACHAR and IACHAR specifically work with the ASCII character set, while the results of CHAR and ICHAR will depend on the default character set being used.

```fortran
WRITE(*,*) ACHAR(97), IACHAR("a")
WRITE(*,*) CHAR(97), ICHAR("a")
```



## FreeBASIC


```freebasic

' FreeBASIC v1.05.0 win64
Print "a - > "; Asc("a")
Print "98 -> "; Chr(98)
Print
Print "Press any key to exit the program"
Sleep
End

```


{{out}}

```txt

a - > 97
98 -> b

```



## Free Pascal

''See [[#Pascal|Pascal]]''


## Frink

The function <code>char[x]</code> in Frink returns the numerical Unicode codepoints for a string or character, or returns the Unicode string for an integer value or array of integer values.  The <code>chars[x]</code> returns an array even if the string is a single character.  These functions also correctly handle upper-plane Unicode characters as a single codepoint.

```frink
println[char["a"]]              // prints 97
println[chars["a"]]             // prints [97] (an array)
println[char[97]]               // prints a
println[char["Frink rules!"]]   // prints [70, 114, 105, 110, 107, 32, 114, 117, 108, 101, 115, 33]
println[[70, 114, 105, 110, 107, 32, 114, 117, 108, 101, 115, 33]]  // prints "Frink rules!"
```



## Gambas


```gambas
Public Sub Form_Open()
Dim sChar As String

sChar = InputBox("Enter a character")
Print "Character " & sChar & " = ASCII " & Str(Asc(sChar))

sChar = InputBox("Enter a ASCII code")
Print "ASCII code " & sChar & " represents " & Chr(Val(sChar))

End
```

Output:

```txt

Character W = ASCII 87
ASCII code 35 represents #

```



## GAP


```gap
# Code must be in 0 .. 255.
CharInt(65);
# 'A'
IntChar('Z');
# 90
```



## Go

In Go, a character literal ''is'' simply an integer constant of the character code:

```go
fmt.Println('a') // prints "97"
fmt.Println('π') // prints "960"
```

Literal constants in Go are not typed (named constants can be).
The variable and constant types most commonly used for character data are <code>byte</code>, <code>rune</code>, and <code>string</code>.
This example program shows character codes (as literals) stored in typed variables, and printed out with default formatting.  Note that since byte and rune are integer types, the default formatting is a printable base 10 number.  String is not numeric, and a little extra work must be done to print the character codes.

```go
package main

import "fmt"

func main() {
	// yes, there is more concise syntax, but this makes
	// the data types very clear.
	var b byte = 'a'
	var r rune = 'π'
	var s string = "aπ"

	fmt.Println(b, r, s)
	fmt.Println("string cast to []rune:", []rune(s))
	// A range loop over a string gives runes, not bytes
	fmt.Print("    string range loop: ")
	for _, c := range s {
		fmt.Print(c, " ") // c is type rune
	}
	// We can also print the bytes of a string without an explicit loop
	fmt.Printf("\n         string bytes: % #x\n", s)
}
```

{{out}}

```txt

97 960 aπ
string cast to []rune: [97 960]
    string range loop: 97 960
         string bytes: 0x61 0xcf 0x80

```

For the second part of the task, printing the character of a given code, the <code>%c</code> verb of <code>fmt.Printf</code> will do this directly from integer values, emitting the UTF-8 encoding of the code, (which will typically print the character depending on your hardware and operating system configuration).

```go
b := byte(97)
r := rune(960)
fmt.Printf("%c %c\n%c %c\n", 97, 960, b, r)
```

{{out}}

```txt

a π
a π
```

You can think of the default formatting of strings as being the printable characters of the string.  In fact however, it is even simpler.
Since we expect our output device to interpret UTF-8, and we expect our string to contain UTF-8, the default formatting simply dumps the bytes of the string to the output.

Examples showing strings constructed from integer constants and then printed:

```go
fmt.Println(string(97)) // prints "a"
fmt.Println(string(960)) // prints "π"
fmt.Println(string([]rune{97, 960})) // prints "aπ"
```



## Golfscript

To convert a number to a string, we use the array to string coercion.

```golfscript
97[]+''+p
```

To convert a string to a number, we have a many options, of which the simplest and shortest are:

```golfscript
'a')\;p
'a'(\;p
'a'0=p
'a'{}/p
```



## Groovy

Groovy does not have a character literal at all, so one-character strings have to be ''coerced'' to '''char'''. Groovy '''printf''' (like Java, but unlike C) is ''not type-agnostic'', so the cast or coercion from '''char''' to '''int''' is also required. The reverse direction is considerably simpler.

```groovy
printf ("%d\n", ('a' as char) as int)
printf ("%c\n", 97)
```

{{Out}}

```txt
97
a
```



## Haskell


```haskell
import Data.Char

main = do
  print (ord 'a') -- prints "97"
  print (chr 97) -- prints "'a'"
  print (ord 'π') -- prints "960"
  print (chr 960) -- prints "'\960'"
```



## HicEst


```hicest
WRITE(Messagebox) ICHAR('a'), CHAR(97)
```



## i


```i
software {
	print(number('a'))
	print(text([97]))
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
if *arglist > 0 then L := arglist else L := [97, "a"]

every x := !L do
   write(x, " ==> ", char(integer(x)) | ord(x) )  # char produces a character, ord produces a number
end
```

Icon and Unicon do not currently support double byte character sets.
{{Out}}
```txt
97 ==> a
a ==> 97
```



## HolyC


```holyc
Print("%d\n", 'a'); /* prints "97" */
Print("%c\n", 97);  /* prints "a" */
```



## Io

Here character is a sequence (string) of length one.

```Io
"a" at(0) println       // --> 97
97 asCharacter println  // --> a

"π" at(0) println       // --> 960
960 asCharacter println // --> π
```



## J


```j
   4 u: 97 98 99 9786
abc☺

   3 u: 7 u: 'abc☺'
97 98 99 9786
```


<code>7 u:</code> converts from utf-8, <code>3 u:</code> by itself would give us:


```j
   3 u: 'abc☺'
97 98 99 226 152 186
```


Also, if we limit ourselves to ascii, we have other ways of accomplishing the same thing. <code>a.</code> is a list of the 8 bit character codes and we can index from it, or search it (though that's mostly a notational convenience, since the underlying type already gives us all we need to know).


```j
   97 98 99{a.
abc
   a.i.'abc'
97 98 99
```



## Java

<tt>char</tt> is already an integer type in Java, and it gets automatically promoted to <tt>int</tt>. So you can use a character where you would otherwise use an integer. Conversely, you can use an integer where you would normally use a character, except you may need to cast it, as <tt>char</tt> is smaller.

In this case, the <tt>println</tt> method is overloaded to handle integer (outputs the decimal representation) and character (outputs just the character) types differently, so we need to cast it in both cases.

```java
public class Foo {
    public static void main(String[] args) {
        System.out.println((int)'a'); // prints "97"
        System.out.println((char)97); // prints "a"
    }
}
```

Java characters support Unicode:

```java
public class Bar {
    public static void main(String[] args) {
        System.out.println((int)'π'); // prints "960"
        System.out.println((char)960); // prints "π"
    }
}
```



## JavaScript

Here character is just a string of length 1

```javascript
console.log('a'.charCodeAt(0)); // prints "97"
console.log(String.fromCharCode(97)); // prints "a"
```


ES6 brings '''String.codePointAt()''' and '''String.fromCodePoint()''', which provide access to 4-byte unicode characters,
in addition to the usual 2-byte unicode characters.


```JavaScript
['字'.codePointAt(0), '🐘'.codePointAt(0)]
```


{{Out}}


```JavaScript
[23383, 128024]
```


and


```JavaScript
[23383, 128024].map(function (x) {
	return String.fromCodePoint(x);
})
```


{{Out}}


```JavaScript
["字", "🐘"]
```



## Joy


```joy
'a ord.
97 chr.
```



## jq

jq data strings are JSON strings, which can be "explode"d into an array of integers, each representing a Unicode codepoint. The inverse of the <tt>explode</tt> filter is <tt>implode</tt>.  <tt>explode</tt> can of course be used for single-character strings, and so for example:

```jq
"a" | explode  # => [ 97 ]
[97] | implode # => "a"
```

Here is a filter which can be used to convert an integer to the corresponding
character:
```jq
def chr: [.] | implode;

```

Example:
1024 | chr # => "Ѐ"


## Julia

Julia character constants (of type <code>Char</code>) are treated as an integer type representing the Unicode codepoint of the character, and can easily be converted to and from other integer types.


```julia
println(Int('a'))
println(Char(97))
```


{{out}}
```txt
97
a
```



## K


```K
  _ic "abcABC"
97 98 99 65 66 67

  _ci 97 98 99 65 66 67
"abcABC"
```



## Kotlin


```scala
fun main(args: Array<String>) {
    var c = 'a'
    var i = c.toInt()
    println("$c  <-> $i")
    i += 2
    c = i.toChar()
    println("$i <-> $c")
}
```


{{out}}

```txt

a  <-> 97
99 <-> c

```



## LabVIEW

{{VI snippet}}<br/>
[[File:LabVIEW_Character_codes.png]]


## Lang5


```lang5
: CHAR  "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[" comb
        '\\ comb -1 remove append "]^_`abcdefghijklmnopqrstuvwxyz{|}~" comb append ;
: CODE  95 iota 33 + ;                : comb  "" split ;
: extract'  rot 1 compress index subscript expand drop ;
: chr  CHAR CODE extract' ;
: ord  CODE CHAR extract' ;

'a ord .    # 97
97 chr .    # a
```



## Lasso


```Lasso
'a'->integer
'A'->integer
97->bytes
65->bytes
```

{{out}}
```txt
97
65
a
A
```



## LFE

In LFE/Erlang, lists and strings are the same, only the representation changes. For example:

```lisp>
 (list 68 111 110 39 116 32 80 97 110 105 99 46)
"Don't Panic."
```


As for this exercise, here's how you could print out the ASCII code for a letter, and a letter from the ASCII code:

```lisp>
 (: io format '"~w~n" '"a")
97
ok
> (: io format '"~p~n" (list '(97)))
"a"
ok
```



## Liberty BASIC


```lb
charCode = 97
char$ = "a"
print chr$(charCode) 'prints a
print asc(char$) 'prints 97
```



## LIL

LIL does not handle NUL bytes in character strings, char 0 returns an empty string.

```tcl
print [char 97]
print [codeat "a" 0]
```


{{out}}

```txt
a
97
```



## Lingo


```lingo
-- returns Unicode code point (=ASCII code for ASCII characters) for character
put chartonum("a")
-- 97

-- returns character for Unicode code point (=ASCII code for ASCII characters)
put numtochar(934)
-- Φ
```



## Little


```C
puts("Unicode value of ñ is ${scan("ñ", "%c")}");
printf("The code 241 in Unicode is the letter: %c.\n", 241);

```



## LiveCode


```LiveCode
Since 7.0.x works with unicode
put charToNum("") && numToChar(240)
```



## Logo

Logo characters are words of length 1.

```logo
print ascii "a    ; 97
print char 97     ; a
```



## Logtalk


```logtalk
|?- char_code(Char, 97), write(Char).
a
Char = a
yes
```


```logtalk
|?- char_code(a, Code), write(Code).
97
Code = 97
yes
```



## Lua


```lua
print(string.byte("a")) -- prints "97"
print(string.char(97)) -- prints "a"
```



## M2000 Interpreter


```M2000 Interpreter

\\ ANSI
Print Asc("a")
Print Chr$(Asc("a"))
\\ Utf16-Le
Print ChrCode("a")
Print ChrCode$(ChrCode("a"))

\\ (,) is an empty array.

Function Codes(a$) {
      If Len(A$)=0 then =(,) : Exit
      Buffer Mem as byte*Len(a$)
      \\ Str$(string) return one byte character
      Return Mem, 0:=Str$(a$)
           Inventory Codes
      For i=0 to len(Mem)-1
      Append Codes, i:=Eval(Mem, i)
      Next i
      =Codes
}
Print Codes("abcd")
\\ 97 98 99 100

```



## Maple

There are two ways to do this in Maple.  First, there are procedures in StringTools for this purpose.

```Maple>
 use StringTools in Ord( "A" ); Char( 65 ) end;
                                   65

                                  "A"

```

Second, the procedure convert handles conversions to and from byte values.

```Maple>
 convert( "A", bytes );
                                  [65]

> convert( [65], bytes );
                                  "A"

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Use the FromCharacterCode and ToCharacterCode functions:

```Mathematica
ToCharacterCode["abcd"]
FromCharacterCode[{97}]
```

{{Out}}
```txt
{97, 98, 99, 100}

"a"
```


=={{header|MATLAB}} / {{header|Octave}}==
There are two built-in function that perform these tasks.
To convert from a number to a character use:

```MATLAB
character = char(asciiNumber)
```


To convert from a character to its corresponding ascii character use:

```MATLAB
asciiNumber = double(character)
```


or if you need this number as an integer not a double use:

```MATLAB
asciiNumber = uint16(character)
asciiNumber = uint32(character)
asciiNumber = uint64(character)
```


Sample Usage:

```MATLAB>>
 char(87)

ans =

W

>> double('W')

ans =

    87

>> uint16('W')

ans =

     87
```



## Maxima


```maxima
ascii(65);
"A"

cint("A");
65
```



## Metafont

Metafont handles only ''ASCII'' (even though codes beyond 127 can be given and used as real ASCII codes)

```metafont
message "enter a letter: ";
string a;
a := readstring;
message decimal (ASCII a); % writes the decimal number of the first character
                           % of the string a
message "enter a number: ";
num := scantokens readstring;
message char num;   % num can be anything between 0 and 255; what will be seen
                    % on output depends on the encoding used by the "terminal"; e.g.
                    % any code beyond 127 when UTF-8 encoding is in use will give
                    % a bad encoding; e.g. to see correctly an "è", we should write
message char10;  % (this add a newline...)
message char hex"c3" & char hex"a8";  % since C3 A8 is the UTF-8 encoding for "è"
end
```



## Microsoft Small Basic


```vb
TextWindow.WriteLine("The ascii code for 'A' is: " + Text.GetCharacterCode("A") + ".")
TextWindow.WriteLine("The character for '65' is: " + Text.GetCharacter(65) + ".")
```


{{out}}

```basic
The ascii code for 'A' is: 65.
The character for '65' is: A.
Press any key to continue...
```




=={{header|Modula-2}}==

```modula2
MODULE asc;

IMPORT  InOut;

VAR     letter          : CHAR;
        ascii           : CARDINAL;

BEGIN
  letter := 'a';
  InOut.Write (letter);
  ascii := ORD (letter);
  InOut.Write (11C);            (*  ASCII TAB   *)
  InOut.WriteCard (ascii, 8);
  ascii := ascii - ORD ('0');
  InOut.Write (11C);            (*  ASCII TAB   *)
  InOut.Write (CHR (ascii));
  InOut.WriteLn
END asc.
```

{{out}}
<lang Modula-2>jan@Beryllium:~/modula/rosetta$ ./asc
a             97        1
```


=={{header|Modula-3}}==
The built in functions <code>ORD</code> and <code>VAL</code> work on characters, among other things.

```modula3
ORD('a') (* Returns 97 *)
VAL(97, CHAR); (* Returns 'a' *)
```




## MUMPS


```MUMPS
WRITE $ASCII("M")
WRITE $CHAR(77)
```




## Neko

Neko treats strings as an array of bytes


```neko
// An 'a' and a 'b'
var s = "a";
var c = 98;
var h = " ";

$print("Character code for 'a': ", $sget(s, 0), "\n");

$sset(h, 0, c);
$print("Character code ", c,  ": ", h, "\n");
```


{{out}}

```txt
Character code for 'a': 97
Character code 98: b
```


Neko also has standard primitives for handling the byte array as UTF-8


```neko
// While Neko also includes some UTF-8 operations,
//  native strings are just arrays of bytes
var us = "¥·£·€·$·¢·₡·₢·₣·₤·₥·₦·₧·₨·₩·₪·₫·₭·₮·₯·₹";

// load some Std lib primitives
utfGet = $loader.loadprim("std@utf8_get", 2);
utfSub = $loader.loadprim("std@utf8_sub", 3);
utfAlloc = $loader.loadprim("std@utf8_buf_alloc", 1);
utfAdd = $loader.loadprim("std@utf8_buf_add", 2);
utfContent = $loader.loadprim("std@utf8_buf_content", 1);

// Pull out the Euro currency symbol from the UTF-8 currency sampler
var uc = utfGet(us, 4);
$print("UFT-8 code for '", utfSub(us, 4, 1), "': ", uc, "\n");

// Build a UTF-8 buffer
var buf = utfAlloc(4);

// Add a Pound Sterling symbol
uc = 8356;
utfAdd(buf, uc);
$print("UTF-8 code ", uc, ": ", utfContent(buf), "\n");
```


{{out}}

```txt
UFT-8 code for '€': 8364
UTF-8 code 8356: ₤
```



## NESL

In NESL, character literals are prefixed with a backtick. The functions <tt>char_code</tt> and <tt>code_char</tt> convert between characters and integer character codes.

```nesl
char_code(`a);

it = 97 : int
```


```nesl
code_char(97);

it = `a : char
```



## NetRexx

NetRexx provides built-in functions to convert between character and decimal/hexadecimal.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  -- create some sample data: character, hex and unicode
  samp = ' ' || 'a'.sequence('e') || '$' || '\xa2'.sequence('\xa5') || '\u20a0'.sequence('\u20b5')
  -- use the C2D C2X D2C and X2C built-in functions
  say "'"samp"'"
  say '   | Chr    C2D  C2X D2C X2C'
  say '---+ --- ------ ---- --- ---'
  loop ci = 1 to samp.length
    cc = samp.substr(ci, 1)
    cd = cc.c2d -- char to decimal
    cx = cc.c2x -- char to hexadecimal
    dc = cd.d2c -- decimal to char
    xc = cx.x2c -- hexadecimal to char
    say ci.right(3)"| '"cc"'" cd.right(6) cx.right(4, 0) "'"dc"' '"xc"'"
    end ci
  return
```

{{Out}}
<pre style="height:20ex; overflow:scroll">' abcde$¢£¤¥₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵'
   | Chr    C2D  C2X D2C X2C
---+ --- ------ ---- --- ---
  1| ' '     32 0020 ' ' ' '
  2| 'a'     97 0061 'a' 'a'
  3| 'b'     98 0062 'b' 'b'
  4| 'c'     99 0063 'c' 'c'
  5| 'd'    100 0064 'd' 'd'
  6| 'e'    101 0065 'e' 'e'
  7| '$'     36 0024 '$' '$'
  8| '¢'    162 00A2 '¢' '¢'
  9| '£'    163 00A3 '£' '£'
 10| '¤'    164 00A4 '¤' '¤'
 11| '¥'    165 00A5 '¥' '¥'
 12| '₠'   8352 20A0 '₠' '₠'
 13| '₡'   8353 20A1 '₡' '₡'
 14| '₢'   8354 20A2 '₢' '₢'
 15| '₣'   8355 20A3 '₣' '₣'
 16| '₤'   8356 20A4 '₤' '₤'
 17| '₥'   8357 20A5 '₥' '₥'
 18| '₦'   8358 20A6 '₦' '₦'
 19| '₧'   8359 20A7 '₧' '₧'
 20| '₨'   8360 20A8 '₨' '₨'
 21| '₩'   8361 20A9 '₩' '₩'
 22| '₪'   8362 20AA '₪' '₪'
 23| '₫'   8363 20AB '₫' '₫'
 24| '€'   8364 20AC '€' '€'
 25| '₭'   8365 20AD '₭' '₭'
 26| '₮'   8366 20AE '₮' '₮'
 27| '₯'   8367 20AF '₯' '₯'
 28| '₰'   8368 20B0 '₰' '₰'
 29| '₱'   8369 20B1 '₱' '₱'
 30| '₲'   8370 20B2 '₲' '₲'
 31| '₳'   8371 20B3 '₳' '₳'
 32| '₴'   8372 20B4 '₴' '₴'
 33| '₵'   8373 20B5 '₵' '₵'
```



## Nim


```nim
echo ord('a') # echoes 97
echo chr(97) # echoes a

import unicode

echo int("π".runeAt(0)) # echoes 960
echo Rune(960) # echoes π
```


=={{header|NS-HUBASIC}}==
NS-HUBASIC uses a non-ASCII character set that doesn't include letters in lowercase.
<lang NS-HUBASIC>10 PRINT CODE "A"
20 PRINT CHR$(38)
```

{{Out}}

```txt
 0A
&
```


=={{header|Oberon-2}}==

```oberon2
MODULE Ascii;
IMPORT Out;
VAR
	c: CHAR;
	d: INTEGER;
BEGIN
	c := CHR(97);
	d := ORD("a");
	Out.Int(d,3);Out.Ln;
	Out.Char(c);Out.Ln
END Ascii.
```

{{Out}}
```txt

97
a
```


## Objeck


```objeck
'a'->As(Int)->PrintLine();
97->As(Char)->PrintLine();
```



## Object Pascal

''See [[#Pascal|Pascal]]''


## OCaml


```ocaml
Printf.printf "%d\n" (int_of_char 'a'); (* prints "97" *)
Printf.printf "%c\n" (char_of_int 97); (* prints "a" *)
```


The following are aliases for the above functions:

```ocaml
# Char.code ;;
- : char -> int = <fun>
# Char.chr;;
- : int -> char = <fun>
```



## Oforth


Oforth has not type or class for characters. A character is an integer which value is its unicode code.


```Oforth
'a' println
```


{{out}}

```txt

97

```



## OpenEdge/Progress

<lang Progress (Openedge ABL)>MESSAGE
   CHR(97) SKIP
   ASC("a")
VIEW-AS ALERT-BOX.
```



## Oz

Characters in Oz are the same as integers in the range 0-255 (ISO 8859-1 encoding). To print a number as a character, we need to use it as a string (i.e. a list of integers from 0 to 255):

```oz
{System.show &a}  %% prints "97"
{System.showInfo [97]}  %% prints "a"
```



## PARI/GP


```parigp
print(Vecsmall("a")[1]);
print(Strchr([72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33]))
```



## Pascal


```pascal
writeln(ord('a'));
writeln(chr(97));
```



## Perl

Here character is just a string of length 1

```perl
print ord('a'), "\n"; # prints "97"
print chr(97), "\n"; # prints "a"
```



## Perl 6

Both Perl 5 and Perl 6 have good Unicode support.  Note that even characters outside the BMP are considered single characters, not a surrogate pair.  Here we use the character "four dragons" (with 64 strokes!) to demonstrate that.

```perl6
say ord('𪚥').fmt('0x%04x');
say chr(0x2a6a5);
```

{{out}}
```txt
0x2a6a5
𪚥
```



## Phix

Characters and their ascii codes are one and the same. (See also printf, %d / %s / %c.)

```Phix
?'A'
puts(1,65)
```

{{out}}

```txt

65
A

```



## PHP

Here character is just a string of length 1

```php
echo ord('a'), "\n"; // prints "97"
echo chr(97), "\n"; // prints "a"
```



## PicoLisp


```PicoLisp
: (char "a")
-> 97
: (char "字")
-> 23383
: (char 23383)
-> "字"
: (chop "文字")
-> ("文" "字")
: (mapcar char @)
-> (25991 23383)
```



## PL/I


```PL/I
declare 1 u union,
          2 c character (1),
          2 i fixed binary (8) unsigned;
c = 'a'; put skip list (i); /* prints 97  */
i = 97;  put skip list (c); /* prints 'a' */
```



## PowerShell

PowerShell does not allow for character literals directly, so to get a character one first needs to convert a single-character string to a char:

```powershell
$char = [char] 'a'
```

Then a simple cast to int yields the character code:

```powershell
$charcode = [int] $char   # => 97
```

This also works with Unicode:

```powershell
[int] [char] '☺'          # => 9786
```

For converting an integral character code into the actual character, a cast to char suffices:

```powershell
[char] 97    # a
[char] 9786  # ☺
```



## Prolog

SWI-Prolog has predefined predicate char_code/2.

```txt
?- char_code(a, X).
X = 97.

?- char_code(X, 97).
X = a.
```



## PureBasic

PureBasic allows compiling code so that it will use either Ascii or a Unicode (UCS-2) encoding for representing its string content.
It also allows for the source code that is being compiled to be in either Ascii or UTF-8 encoding.
A one-character string is used here to hold the character and a numerical character type is used to hold the character code.
The character type is either one or two bytes in size, depending on whether compiling for Ascii or Unicode respectively.

```PureBasic
If OpenConsole()
  ;Results are the same when compiled for Ascii or Unicode
  charCode.c = 97
  Char.s = "a"
  PrintN(Chr(charCode))   ;prints a
  PrintN(Str(Asc(Char)))  ;prints 97

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```


This version should be compiled with Unicode setting and the source code to be encoded using UTF-8.

```PureBasic
If OpenConsole()
  ;UTF-8 encoding compiled for Unicode (UCS-2)
  charCode.c = 960
  Char.s = "π"
  PrintN(Chr(charCode))   ;prints π
  PrintN(Str(Asc(Char)))  ;prints 960

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```



## Python

{{works with|Python|2.x}}
Here character is just a string of length 1

8-bit characters:

```python
print ord('a') # prints "97"
print chr(97)  # prints "a"
```


Unicode characters:

```python
print ord(u'π')   # prints "960"
print unichr(960) # prints "π"
```


{{works with|Python|3.x}}
Here character is just a string of length 1

```python
print(ord('a')) # prints "97" (will also work in 2.x)
print(ord('π')) # prints "960"
print(chr(97))  # prints "a" (will also work in 2.x)
print(chr(960)) # prints "π"
```



## R


```R
ascii <- as.integer(charToRaw("hello world")); ascii
text <- rawToChar(as.raw(ascii)); text
```



## Racket


```Racket
#lang racket

(define (code ch)
  (printf "The unicode number for ~s is ~a\n" ch (char->integer ch)))
(code #\a)
(code #\λ)

(define (char n)
  (printf "The unicode number ~a is the character ~s\n" n (integer->char n)))
(char 97)
(char 955)
```



## RapidQ


```vb

Print Chr$(97)
Print Asc("a")

```



## Red


```Red
Red []
print to-integer first "a" ;; -> 97
print to-integer #"a"      ;; -> 97
print to-binary "a"        ;; -> #{61}
print to-char 97           ;; -> a

```



## Retro


```Retro
'c putc
```



## REXX

REXX supports handling of characters with built-in functions (BIFs), whether it be hexadecimal, binary (bits), or decimal code(s).

### ASCII


```rexx
/*REXX program displays a char's ASCII code/value (or EBCDIC if run on an EBCDIC system)*/
yyy= 'c'                               /*assign a lowercase       c        to   YYY.    */
yyy= "c"                               /* (same as above)                               */
say  'from char, yyy code=' yyy

yyy= '63'x                             /*assign hexadecimal      63        to   YYY.    */
yyy= '63'X                             /* (same as above)                               */
say  'from  hex, yyy code=' yyy

yyy= x2c(63)                           /*assign hexadecimal      63        to   YYY.    */
say  'from  hex, yyy code=' yyy

yyy= '01100011'b                       /*assign a binary      0011 0100    to   YYY.    */
yyy= '0110 0011'b                      /* (same as above)                               */
yyy= '0110 0011'B                      /*   "   "    "                                  */
say  'from  bin, yyy code=' yyy

yyy= d2c(99)                           /*assign decimal code     99        to   YYY.    */
say  'from  dec, yyy code=' yyy

say                                    /*     [↓]    displays the value of  YYY  in ··· */
say  'char code: '   yyy               /* character code  (as an 8-bit ASCII character).*/
say  ' hex code: '   c2x(yyy)          /*    hexadecimal                                */
say  ' dec code: '   c2d(yyy)          /*        decimal                                */
say  ' bin code: '   x2b( c2x(yyy) )   /*         binary  (as a bit string)             */
                                       /*stick a fork in it, we're all done with display*/
```

'''output'''

```txt

from char, yyy code= c
from  hex, yyy code= c
from  hex, yyy code= c
from  bin, yyy code= c
from  dec, yyy code= c

char code:  c
 hex code:  63
 dec code:  99
 bin code:  01100011

```



### EBCDIC


```rexx
/* REXX */
yyy='c'               /*assign a lowercase   c to  YYY */
yyy='83'x             /*assign hexadecimal  83 to  YYY */
                      /*the  X  can be upper/lowercase.*/
yyy=x2c(83)           /* (same as above)               */
yyy='10000011'b       /* (same as above)               */
yyy='1000 0011'b      /* (same as above)               */
                      /*the  B  can be upper/lowercase.*/
yyy=d2c(129)          /*assign decimal code 129 to YYY */

say yyy               /*displays the value of  YYY                   */
say c2x(yyy)          /*displays the value of  YYY in hexadecimal.   */
say c2d(yyy)          /*displays the value of  YYY in decimal.       */
say x2b(c2x(yyy))/*displays the value of YYY in binary (bit string). */
```

{{out}}

```txt
a
81
129
10000001
```



## Ring


```ring

see ascii("a") + nl
see char(97) + nl

```



## Ruby


### 1.8

In Ruby 1.8 characters are usually represented directly as their integer character code. Ruby has a syntax for "character literal" which evaluates directly to the integer code: <tt>?a</tt> evaluates to the integer 97. Subscripting a string also gives just the integer code for the character.

```ruby>
 ?a
=> 97
> "a"[0]
=> 97
> 97.chr
=> "a"
```



### 1.9

In Ruby 1.9 characters are represented as length-1 strings; same as in Python. The previous "character literal" syntax <tt>?a</tt> is now the same as <tt>"a"</tt>. Subscripting a string also gives a length-1 string. There is now an "ord" method of strings to convert a character into its integer code.


```ruby>
 "a".ord
=> 97
> 97.chr
=> "a"
```



## Run BASIC


```runbasic
print chr$(97) 'prints a
print asc("a") 'prints 97
```



## Rust


```rust
use std::char::from_u32;

fn main() {
    //ascii char
    println!("{}", 'a' as u8);
    println!("{}", 97 as char);

    //unicode char
    println!("{}", 'π' as u32);
    println!("{}", from_u32(960).unwrap());
}
```

{{out}}

```txt
97
a
960
π
```



## Sather


```sather
class MAIN is
  main is
    #OUT + 'a'.int + "\n"; -- or
    #OUT + 'a'.ascii_int + "\n";
    #OUT + CHAR::from_ascii_int(97) + "\n";
  end;
end;
```



## Scala

{{libheader|Scala}}
Scala supports unicode characters, but each character is UTF-16, so there is not a 1-to-1 relationship for supplementary character sets.

### In a REPL session


```scala>scala
 'a' toInt
res2: Int = 97

scala> 97 toChar
res3: Char = a

scala> '\u0061'
res4: Char = a

scala> "\uD869\uDEA5"
res5: String = 𪚥
```


### Full swing workout

Taken the supplemental character sets in account.

```scala
import java.lang.Character._; import scala.annotation.tailrec

object CharacterCode extends App {
  def intToChars(n: Int): Array[Char] = java.lang.Character.toChars(n)

  def UnicodeToList(UTFstring: String) = {
    @tailrec
    def inner(str: List[Char], acc: List[String], surrogateHalf: Option[Char]): List[String] = {
      (str, surrogateHalf) match {
        case (Nil, _) => acc
        case (ch :: rest, None) => if (ch.isSurrogate) inner(rest, acc, Some(ch))
        else inner(rest, acc :+ ch.toString, None)
        case (ch :: rest, Some(f)) => inner(rest, (acc :+ (f.toString + ch)), None)
      }
    }
    inner(UTFstring.toList, Nil, None)
  }

  def UnicodeToInt(utf: String) = {
    def charToInt(high: Char, low: Char) =
      { if (isSurrogatePair(high, low)) toCodePoint(high, low) else high.toInt }
    charToInt(utf(0), if (utf.size > 1) utf(1) else 0)
  }

  def UTFtoHexString(utf: String) = { utf.map(ch => f"${ch.toInt}%04X").mkString("\"\\u", "\\u", "\"") }

  def flags(ch: String) = { // Testing Unicode character properties
    (if (ch matches "\\p{M}") "Y" else "N") + (if (ch matches "\\p{Mn}") "Y" else "N")
  }

  val str = '\uFEFF' /*big-endian BOM*/ + "\u0301a" +
    "$áabcde¢£¤¥©ÇßĲĳŁłʒλπक्तु•₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵℃←→⇒∙⌘☃☹☺☻ア字文𠀀" + intToChars(173733).mkString

  println(s"Example string: $str")
  println("""    | Chr C/C++/Java source  Code Point Hex      Dec Mn Name
		  	!----+ --- ------------------------- ------- -------- -- """.stripMargin('!') + "-" * 27)

  (UnicodeToList(str)).zipWithIndex.map {
    case (coll, nr) =>
      f"$nr%4d: $coll\t${UTFtoHexString(coll)}%27s U+${UnicodeToInt(coll)}%05X" +
        f"${"(" + UnicodeToInt(coll).toString}%8s) ${flags(coll)}  ${getName(coll(0).toInt)} "
  }.foreach(println)
}
```

{{Out}}
<pre style="height:20ex; overflow:scroll">
Example string: ﻿́a$áabcde¢£¤¥©ÇßĲĳŁłʒλπक्तु•₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵℃←→⇒∙⌘☃☹☺☻ア字文𠀀𪚥
    | Chr C/C++/Java source  Code Point Hex      Dec Mn Name
----+ --- ------------------------- ------- -------- -- ---------------------------
   0: ﻿	                   "\uFEFF" U+0FEFF  (65279) NN  ZERO WIDTH NO-BREAK SPACE
   1: ́	                   "\u0301" U+00301    (769) YY  COMBINING ACUTE ACCENT
   2: a	                   "\u0061" U+00061     (97) NN  LATIN SMALL LETTER A
   3: $	                   "\u0024" U+00024     (36) NN  DOLLAR SIGN
   4: á	                   "\u00E1" U+000E1    (225) NN  LATIN SMALL LETTER A WITH ACUTE
   5: a	                   "\u0061" U+00061     (97) NN  LATIN SMALL LETTER A
   6: b	                   "\u0062" U+00062     (98) NN  LATIN SMALL LETTER B
   7: c	                   "\u0063" U+00063     (99) NN  LATIN SMALL LETTER C
   8: d	                   "\u0064" U+00064    (100) NN  LATIN SMALL LETTER D
   9: e	                   "\u0065" U+00065    (101) NN  LATIN SMALL LETTER E
  10: ¢	                   "\u00A2" U+000A2    (162) NN  CENT SIGN
  11: £	                   "\u00A3" U+000A3    (163) NN  POUND SIGN
  12: ¤	                   "\u00A4" U+000A4    (164) NN  CURRENCY SIGN
  13: ¥	                   "\u00A5" U+000A5    (165) NN  YEN SIGN
  14: ©	                   "\u00A9" U+000A9    (169) NN  COPYRIGHT SIGN
  15: Ç	                   "\u00C7" U+000C7    (199) NN  LATIN CAPITAL LETTER C WITH CEDILLA
  16: ß	                   "\u00DF" U+000DF    (223) NN  LATIN SMALL LETTER SHARP S
  17: Ĳ	                   "\u0132" U+00132    (306) NN  LATIN CAPITAL LIGATURE IJ
  18: ĳ	                   "\u0133" U+00133    (307) NN  LATIN SMALL LIGATURE IJ
  19: Ł	                   "\u0141" U+00141    (321) NN  LATIN CAPITAL LETTER L WITH STROKE
  20: ł	                   "\u0142" U+00142    (322) NN  LATIN SMALL LETTER L WITH STROKE
  21: ʒ	                   "\u0292" U+00292    (658) NN  LATIN SMALL LETTER EZH
  22: λ	                   "\u03BB" U+003BB    (955) NN  GREEK SMALL LETTER LAMDA
  23: π	                   "\u03C0" U+003C0    (960) NN  GREEK SMALL LETTER PI
  24: क	                   "\u0915" U+00915   (2325) NN  DEVANAGARI LETTER KA
  25: ्	                   "\u094D" U+0094D   (2381) YY  DEVANAGARI SIGN VIRAMA
  26: त	                   "\u0924" U+00924   (2340) NN  DEVANAGARI LETTER TA
  27: ु	                   "\u0941" U+00941   (2369) YY  DEVANAGARI VOWEL SIGN U
  28: •	                   "\u2022" U+02022   (8226) NN  BULLET
  29: ₠	                   "\u20A0" U+020A0   (8352) NN  EURO-CURRENCY SIGN
  30: ₡	                   "\u20A1" U+020A1   (8353) NN  COLON SIGN
  31: ₢	                   "\u20A2" U+020A2   (8354) NN  CRUZEIRO SIGN
  32: ₣	                   "\u20A3" U+020A3   (8355) NN  FRENCH FRANC SIGN
  33: ₤	                   "\u20A4" U+020A4   (8356) NN  LIRA SIGN
  34: ₥	                   "\u20A5" U+020A5   (8357) NN  MILL SIGN
  35: ₦	                   "\u20A6" U+020A6   (8358) NN  NAIRA SIGN
  36: ₧	                   "\u20A7" U+020A7   (8359) NN  PESETA SIGN
  37: ₨	                   "\u20A8" U+020A8   (8360) NN  RUPEE SIGN
  38: ₩	                   "\u20A9" U+020A9   (8361) NN  WON SIGN
  39: ₪	                   "\u20AA" U+020AA   (8362) NN  NEW SHEQEL SIGN
  40: ₫	                   "\u20AB" U+020AB   (8363) NN  DONG SIGN
  41: €	                   "\u20AC" U+020AC   (8364) NN  EURO SIGN
  42: ₭	                   "\u20AD" U+020AD   (8365) NN  KIP SIGN
  43: ₮	                   "\u20AE" U+020AE   (8366) NN  TUGRIK SIGN
  44: ₯	                   "\u20AF" U+020AF   (8367) NN  DRACHMA SIGN
  45: ₰	                   "\u20B0" U+020B0   (8368) NN  GERMAN PENNY SIGN
  46: ₱	                   "\u20B1" U+020B1   (8369) NN  PESO SIGN
  47: ₲	                   "\u20B2" U+020B2   (8370) NN  GUARANI SIGN
  48: ₳	                   "\u20B3" U+020B3   (8371) NN  AUSTRAL SIGN
  49: ₴	                   "\u20B4" U+020B4   (8372) NN  HRYVNIA SIGN
  50: ₵	                   "\u20B5" U+020B5   (8373) NN  CEDI SIGN
  51: ℃	                   "\u2103" U+02103   (8451) NN  DEGREE CELSIUS
  52: ←	                   "\u2190" U+02190   (8592) NN  LEFTWARDS ARROW
  53: →	                   "\u2192" U+02192   (8594) NN  RIGHTWARDS ARROW
  54: ⇒	                   "\u21D2" U+021D2   (8658) NN  RIGHTWARDS DOUBLE ARROW
  55: ∙	                   "\u2219" U+02219   (8729) NN  BULLET OPERATOR
  56: ⌘	                   "\u2318" U+02318   (8984) NN  PLACE OF INTEREST SIGN
  57: ☃	                   "\u2603" U+02603   (9731) NN  SNOWMAN
  58: ☹	                   "\u2639" U+02639   (9785) NN  WHITE FROWNING FACE
  59: ☺	                   "\u263A" U+0263A   (9786) NN  WHITE SMILING FACE
  60: ☻	                   "\u263B" U+0263B   (9787) NN  BLACK SMILING FACE
  61: ア	                   "\u30A2" U+030A2  (12450) NN  KATAKANA LETTER A
  62: 字	                   "\u5B57" U+05B57  (23383) NN  CJK UNIFIED IDEOGRAPHS 5B57
  63: 文	                   "\u6587" U+06587  (25991) NN  CJK UNIFIED IDEOGRAPHS 6587
  64: 	                   "\uF8FF" U+0F8FF  (63743) NN  PRIVATE USE AREA F8FF
  65: 𠀀	             "\uD840\uDC00" U+20000 (131072) NN  HIGH SURROGATES D840
  66: 𪚥	             "\uD869\uDEA5" U+2A6A5 (173733) NN  HIGH SURROGATES D869
```
[http://illegalargumentexception.blogspot.nl/2009/05/java-rough-guide-to-character-encoding.html More background info: "Java: a rough guide to character encoding"]


## Scheme


```scheme
(display (char->integer #\a)) (newline) ; prints "97"
(display (integer->char 97)) (newline) ; prints "a"
```



## Seed7


```seed7
writeln(ord('a'));
writeln(chr(97));
```



## Sidef


```ruby
say 'a'.ord;    # => 97
say 97.chr;     # => 'a'
```



## SequenceL

SequenceL natively supports ASCII characters.

'''SequenceL Interpreter Session:'''

```sequencel>cmd:
asciiToInt('a')
97
cmd:>intToAscii(97)
'a'
```



## Slate


```slate
$a code.
97 as: String Character.
```



## Smalltalk


```smalltalk
($a asInteger) displayNl. "output 97"
(Character value: 97) displayNl. "output a"
```



## SmileBASIC


```smilebasic
PRINT CHR$(97) 'a
PRINT ASC("a") '97
```



## SNOBOL4

Snobol implementations may or may not have built-in char( ) and ord ( ) or asc( ).
These are based on examples in the Snobol4+ tutorial and work with the native (1-byte) charset.

```SNOBOL4
        define('chr(n)') :(chr_end)
chr     &alphabet tab(n) len(1) . chr :s(return)f(freturn)
chr_end

        define('asc(str)c') :(asc_end)
asc     str len(1) . c
        &alphabet break(c) @asc :s(return)f(freturn)
asc_end

*       # Test and display
        output = char(65) ;* Built-in
        output = chr(65)
        output = asc('A')
end
```

{{Out}}

```txt
A
A
65
```



## SPL

In SPL all characters are used in UTF-16LE encoding.

```spl
x = #.array("a")
#.output("a -> ",x[1]," ",x[2])
x = [98,0]
#.output("98 0 -> ",#.str(x))
```

{{out}}

```txt

a -> 97 0
98 0 -> b

```



## Standard ML


```sml
print (Int.toString (ord #"a") ^ "\n"); (* prints "97" *)
print (Char.toString (chr 97) ^ "\n"); (* prints "a" *)
```



## Stata

The Mata '''ascii''' function transforms a string into a numeric vector of UTF-8 bytes. For instance:


```stata
: ascii("α")
         1     2
    +-------------+
  1 |  206   177  |
    +-------------+
```


Where 206, 177 is the UTF-8 encoding of Unicode character 945 (GREEK SMALL LETTER ALPHA).

ASCII characters are mapped to single bytes:


```stata
: ascii("We the People")
         1     2     3     4     5     6     7     8     9    10    11    12    13
    +-------------------------------------------------------------------------------+
  1 |   87   101    32   116   104   101    32    80   101   111   112   108   101  |
    +-------------------------------------------------------------------------------+
```


Conversely, the '''char''' function transforms a byte vector into a string:


```stata
: char((73,32,115,116,97,110,100,32,104,101,114,101))
  I stand here
```



## Swift

The type that represent a Unicode code point is <code>UnicodeScalar</code>.
You can initialize it with a string literal:

```swift
let c1: UnicodeScalar = "a"
println(c1.value) // prints "97"
let c2: UnicodeScalar = "π"
println(c2.value) // prints "960"
```

Or, you can get it by iterating a string's unicode scalars view:

```swift
let s1 = "a"
for c in s1.unicodeScalars {
  println(c.value) // prints "97"
}
let s2 = "π"
for c in s2.unicodeScalars {
  println(c.value) // prints "960"
}
```


You can also initialize it from a <code>UInt32</code> integer:

```swift
let i1: UInt32 = 97
println(UnicodeScalar(i1)) // prints "a"
let i2: UInt32 = 960
println(UnicodeScalar(i2)) // prints "π"
```



## Tcl


```tcl
# ASCII
puts [scan "a" %c]   ;# ==> 97
puts [format %c 97]  ;# ==> a
# Unicode is the same
puts [scan "π" %c]   ;# ==> 960
puts [format %c 960] ;# ==> π
```


=={{header|TI-83 BASIC}}==
TI-83 BASIC provides no built in way to do this, so in all String<-->List routines and anything else which requires character codes, a workaround using inString( and sub( is used.
In this example, the code of 'A' is displayed, and then the character matching a user-defined code is displayed.

```ti83b
"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789→Str1
Disp inString(Str1,"A
Input "CODE? ",A
Disp sub(Str1,A,1
```


=={{header|TI-89 BASIC}}==
The TI-89 uses an 8-bit charset/encoding which is similar to ISO-8859-1, but with more mathematical symbols and Greek letters.
At least codes 14-31, 128-160, 180 differ.
The ASCII region is unmodified. (TODO: Give a complete list.)

The TI Connect X desktop software converts between this unique character set and Unicode characters, though sometimes in a consistent but inappropriate fashion. <!-- Only Mac experience went into this statement; info for other platforms? -->

The below program will display the character and code for any key pressed. Some keys do not correspond to characters and have codes greater than 255.
The portion of the program actually implementing the task is marked with a line of “©”s.

```ti89b
Prgm
  Local k, s
  ClrIO
  Loop
    Disp "Press a key, or ON to exit."
    getKey() © clear buffer
    0 → k : While k = 0 : getKey() → k : EndWhile
    ClrIO
    If k ≥ 256 Then
      Disp "Not a character."
      Disp "Code: " & string(k)
    Else

      char(k) → s                           ©
      © char() and ord() are inverses.      ©
      Disp "Character: " & s                ©
      Disp "Code: " & string(ord(s))        ©

    EndIf
  EndLoop
EndPrgm
```



## Trith

Characters are Unicode code points, so the solution is the same for Unicode characters as it is for ASCII characters:

```trith
"a" ord print
97 chr print
```


```trith
"π" ord print
960 chr print
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
SET character ="a", code=DECODE (character,byte)
PRINT character,"=",code
```

{{Out}}
```txt
a=97
```



## uBasic/4tH

uBasic/4tH is an integer BASIC, just like Tiny BASIC. However, the function ORD() is supported, just as CHR(). The latter is only allowed within a PRINT statement.
<lang>z = ORD("a") : PRINT CHR(z)    ' Prints "a"
```



## Ursa


```ursa
# outputs the character value for 'a'
out (ord "a") endl console
# outputs the character 'a' given its value
out (chr 97) endl console
```



## Ursala

Character code functions are not built in but easily defined as reifications of
the character table.

```Ursala
#import std
#import nat

chr = -: num characters
asc = -:@rlXS num characters

#cast %cnX

test = (chr97,asc`a)
```

{{Out}}
```txt
(`a,97)
```



## VBA


```vba
Debug.Print Chr(97) 'Prints a
Debug.Print [Code("a")] ' Prints 97
```



## VBScript


```vb

'prints a
WScript.StdOut.WriteLine Chr(97)

'prints 97
WScript.StdOut.WriteLine Asc("a")

```



## Vim Script

The behavior of the two functions depends on the value of the option <code>encoding</code>.

```vim
"encoding is set to utf-8
echo char2nr("a")
"Prints 97

echo nr2char(97)
"Prints a
```




## Visual Basic .NET


```vbnet
Console.WriteLine(Chr(97)) 'Prints a
Console.WriteLine(Asc("a")) 'Prints 97
```



## XLISP

In a REPL:

```scheme
[1] (INTEGER->CHAR 97)

#\a
[2] (CHAR->INTEGER #\a)

97
```



## XPL0

A character is represented by an integer value equal to its ASCII code.
The up-arrow character is used to convert the immediately following
character to an integer equal to its ASCII code.


```XPL0
IntOut(0, ^a);  \(Integer Out)  displays "97" on the console (device 0)
ChOut(0, 97);   \(Character Out) displays "a" on the console (device 0)
```



## zkl

The character set is 8 bit ASCII (but doesn't care if you use UTF-8 or unicode characters).

```zkl
 "a".toAsc()  //-->97
(97).toChar() //-->"a"
```



## ZX Spectrum Basic


```zxbasic
10 PRINT CHR$ 97: REM prints a
20 PRINT CODE "a": REM prints 97
```


{{omit from|bc}}
{{omit from|GUISS}}
