+++
title = "Increment a numerical string"
description = ""
date = 2019-10-15T12:31:54Z
aliases = []
[extra]
id = 1966
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}
[[Category:Simple]]

;Task:
Increment a numerical string.





## ABAP


```ABAP
report zz_incstring
perform test using: '0', '1', '-1', '10000000', '-10000000'.

form test using iv_string type string.
  data: lv_int  type i,
        lv_string type string.
  lv_int = iv_string + 1.
  lv_string = lv_int.
  concatenate '"' iv_string '" + 1 = "' lv_string '"' into lv_string.
  write / lv_string.
endform.

```


{{Out}}

```txt

"0" + 1 = "1 "
"1" + 1 = "2 "
"-1" + 1 = "0 "
"10000000" + 1 = "10000001 "
"-10000000" + 1 = "9999999-"

```



## ActionScript


```ActionScript
function incrementString(str:String):String
{
	return String(Number(str)+1);
}
```



## Ada

The standard Ada package Ada.Strings.Fixed provides a function for trimming blanks from a string.

```ada
S : String := "12345";
S := Ada.Strings.Fixed.Trim(Source => Integer'Image(Integer'Value(S) + 1), Side => Ada.Strings.Both);
```



## Aime


```aime

o_text(itoa(atoi("2047") + 1));
o_byte('\n');

```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
STRING s := "12345"; FILE f; INT i;
associate(f, s); get(f,i);
i+:=1;
s:=""; reset(f); put(f,i);
print((s, new line))
```

{{Out}}

```txt

+12346

```



## Apex


```apex

string count = '12345';
count = String.valueOf(integer.valueOf(count)+1);
system.debug('Incremental Value : '+count);

```

{{Out}}

```txt
12346
```



## AppleScript

Preserving the distinction between real and integer strings, and allowing for strings containing non-numeric tokens and/or multiple numeric expressions. Provides an option to either retain or prune out any non-numeric tokens in the string:
{{Trans|Python}}
{{Trans|Haskell}}
{{Trans|JavaScript}}

```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

-- succString :: Bool -> String -> String
on succString(blnPruned, s)
    script go
        on |λ|(w)
            try
                if w contains "." then
                    set v to w as real
                else
                    set v to w as integer
                end if
                {(1 + v) as string}
            on error
                if blnPruned then
                    {}
                else
                    {w}
                end if
            end try
        end |λ|
    end script
    unwords(concatMap(go, |words|(s)))
end succString


-- TEST ---------------------------------------------------
on run
    script test
        on |λ|(bln)
            succString(bln, ¬
                "41 pine martens in 1491.3 -1.5 mushrooms ≠ 136")
        end |λ|
    end script
    unlines(map(test, {true, false}))
end run

--> 42 1492.3 -0.5 137
--> 42 pine martens in 1492.3 -0.5 mushrooms ≠ 137

-- GENERIC ------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- unwords :: [String] -> String
on unwords(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, space}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end unwords

-- words :: String -> [String]
on |words|(s)
    set ca to current application
    (((ca's NSString's stringWithString:(s))'s ¬
        componentsSeparatedByCharactersInSet:(ca's ¬
            NSCharacterSet's whitespaceAndNewlineCharacterSet()))'s ¬
        filteredArrayUsingPredicate:(ca's ¬
            NSPredicate's predicateWithFormat:"0 < length")) as list
end |words|
```

{{Out}}

```txt
42 1492.3 -0.5 137
42 pine martens in 1492.3 -0.5 mushrooms ≠ 137
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program incstring.s   */

/* Constantes    */
.equ BUFFERSIZE,   100
.equ STDIN,  0     @ Linux input console
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ READ,   3     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessNum:           .asciz "Enter number : \n"
szCarriageReturn:  .asciz "\n"
szMessResult:       .ascii "Increment number is = "      @ message result
sMessValeur:        .fill 12, 1, ' '
                       .asciz "\n"

/* UnInitialized data */
.bss
sBuffer:    .skip    BUFFERSIZE

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    ldr r0,iAdrszMessNum
    bl affichageMess
    mov r0,#STDIN         @ Linux input console
    ldr r1,iAdrsBuffer   @ buffer address
    mov r2,#BUFFERSIZE   @ buffer size
    mov r7, #READ         @ request to read datas
    swi 0                  @ call system
    ldr r1,iAdrsBuffer    @ buffer address
    mov r2,#0                @ end of string
    strb r2,[r1,r0]         @ store byte at the end of input string (r0
    @
    ldr r0,iAdrsBuffer    @ buffer address
    bl conversionAtoD    @ conversion string in number in r0
    @ increment r0
    add r0,#1
    @ conversion register to string
    ldr r1,iAdrsMessValeur
    bl conversion10S       @ call conversion
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call

iAdrsMessValeur:        .int sMessValeur
iAdrszMessNum:           .int szMessNum
iAdrsBuffer:             .int sBuffer
iAdrszMessResult:       .int szMessResult
iAdrszCarriageReturn:  .int szCarriageReturn
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save  registres */
    push {r0,r1,r2,r7}    		/* save others registers */
    mov r2,#0   				/* counter length */
1:      /* loop length calculation */
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

 /******************************************************************/
/*     Convert a string to a number stored in a registry          */
/******************************************************************/
/* r0 contains the address of the area terminated by 0 or 0A */
/* r0 returns a number                           */
conversionAtoD:
    push {fp,lr}         @ save 2 registers
    push {r1-r7}         @ save others registers
    mov r1,#0
    mov r2,#10           @ factor
    mov r3,#0            @ counter
    mov r4,r0            @ save address string -> r4
    mov r6,#0            @ positive sign by default
    mov r0,#0            @ initialization to 0
1:     /* early space elimination loop */
    ldrb r5,[r4,r3]     @ loading in r5 of the byte located at the beginning + the position
    cmp r5,#0            @ end of string -> end routine
    beq 100f
    cmp r5,#0x0A        @ end of string -> end routine
    beq 100f
    cmp r5,#' '          @ space ?
    addeq r3,r3,#1      @ yes we loop by moving one byte
    beq 1b
    cmp r5,#'-'          @ first character is -
    moveq r6,#1         @  1 -> r6
    beq 3f              @ then move on to the next position
2:   /* beginning of digit processing loop */
    cmp r5,#'0'          @ character is not a number
    blt 3f
    cmp r5,#'9'          @ character is not a number
    bgt 3f
    /* character is a number */
    sub r5,#48
    umull r0,r1,r2,r0         @ multiply par factor 10
	cmp r1,#0           @ overflow ?
    bgt 99f            @ overflow error
    add r0,r5            @ add to  r0
3:
    add r3,r3,#1         @ advance to the next position
    ldrb r5,[r4,r3]     @ load byte
    cmp r5,#0            @ end of string -> end routine
    beq 4f
    cmp r5,#0x0A            @ end of string -> end routine
    beq 4f
    b 2b                 @ loop
4:
    cmp r6,#1            @ test r6 for sign
    moveq r1,#-1
    muleq r0,r1,r0       @ if negatif, multiply par -1
    b 100f
99:  /* overflow error */
    ldr r0,=szMessErrDep
    bl   affichageMess
    mov r0,#0      @ return  zero  if error
100:
    pop {r1-r7}          @ restaur other registers
    pop {fp,lr}          @ restaur   2 registers
    bx lr                 @return procedure
/* constante program */
szMessErrDep:  .asciz  "Too large: overflow 32 bits.\n"
.align 4

/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}    @ save registers
    mov r2,r1       /* debut zone stockage */
    mov r3,#'+'     /* par defaut le signe est + */
    cmp r0,#0       @ negative number ?
    movlt r3,#'-'   @ yes
    mvnlt r0,r0     @ number inversion
    addlt r0,#1
    mov r4,#10       @ length area
1:  @ start loop
    bl divisionpar10
    add r1,#48   @ digit
    strb r1,[r2,r4]  @ store digit on area
    sub r4,r4,#1      @ previous position
    cmp r0,#0          @ stop if quotient = 0
    bne 1b

    strb r3,[r2,r4]  @ store signe
    subs r4,r4,#1    @ previous position
    blt  100f        @ if r4 < 0 -> end

    mov r1,#' '   @ space
2:
    strb r1,[r2,r4]  @store byte space
    subs r4,r4,#1    @ previous position
    bge 2b           @ loop if r4 > 0
100:
    pop {r0-r4,lr}   @ restaur registers
    bx lr


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



```arturo
num "12349"

print "The next number is: " + ($(toNumber num)+1)
```


{{out}}


```txt
The next number is: 12350
```



## AutoHotkey


```autohotkey
str = 12345
MsgBox % str += 1
```

{{Out}}

```txt
12346
```



## AutoIt


```autoIt
Global $x = "12345"
$x += 1
MsgBox(0,"",$x)
```

{{Out}}

```txt
12346
```



## AWK

The example shows that the string ''s'' can be incremented,
but after that still is a string of length 2.

```awk
$ awk 'BEGIN{s="42"; s++; print s"("length(s)")" }'
43(2)
```



## BASIC

{{works with|BaCon}}

{{works with|BBC BASIC}}

{{works with|QBasic}}

{{works with|PowerBASIC}}

{{works with|Visual Basic}}

{{works with|Liberty BASIC}}


```qbasic
s$ = "12345"
s$ = STR$(VAL(s$) + 1)
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET S$="12345"
110 LET S$=STR$(VAL(S$)+1)
```


=== {{header|ZX Spectrum Basic}} ===

The ZX Spectrum needs line numbers and a let statement,
but the same technique can be used:


```zxbasic
10 LET s$ = "12345"
20 LET s$ = STR$(VAL(s$) + 1)
```



## Batch File

Since environment variables have no type distinction all numbers are simply numeric strings:

{{works with|Windows NT|4}}

```dos
set s=12345
set /a s+=1
```



## BBC BASIC

This assumes the task is about incrementing an ''arbitrary-length'' decimal string.

```bbcbasic
      num$ = "567"
      REPEAT
        PRINT num$
        PROCinc$(num$)
      UNTIL FALSE
      END

      DEF PROCinc$(RETURN n$)
      LOCAL A$, I%
      I% = LEN(n$)
      REPEAT
        A$ = CHR$(ASCMID$(n$,I%) + 1)
        IF A$=":" A$ = "0"
        MID$(n$,I%,1) = A$
        I% -= 1
      UNTIL A$<>"0" OR I%=0
      IF A$="0" n$ = "1" + n$
      ENDPROC
```



## Boo


```boo
s = "1234"
s = (int.Parse(s) + 1).ToString()
```



## Bracmat

Numbers are strings. Bracmat supports rational numbers, including integers, using arbitrary-precision arithmetic. Pure imaginary numbers are formed using a factor <code>i</code> (or <code>-i</code>). There is no support for floating point arithmetics. (Historically, because the ARM 2 processor in the Archimedes computer didn't sport an FPU.)

```bracmat
(n=35664871829866234762187538073934873121878/6172839450617283945)
&!n+1:?n
&out$!n

   35664871829866234762193710913385490405823/6172839450617283945

```



## Brat


```brat
#Convert to integer, increment, then back to string
p ("100".to_i + 1).to_s  #Prints 101
```



## Burlesque



```burlesque

ri?ish

```



## C

Handling strings of arbitrary sizes:
```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*  Constraints: input is in the form of (\+|-)?[0-9]+
 *  and without leading zero (0 itself can be as "0" or "+0", but not "-0");
 *  input pointer is realloc'able and may change;
 *  if input has leading + sign, return may or may not keep it.
 *  The constranits conform to sprintf("%+d") and this function's own output.
 */
char * incr(char *s)
{
	int i, begin, tail, len;
	int neg = (*s == '-');
	char tgt = neg ? '0' : '9';

	/* special case: "-1" */
	if (!strcmp(s, "-1")) {
		s[0] = '0', s[1] = '\0';
		return s;
	}

	len = strlen(s);
	begin = (*s == '-' || *s == '+') ? 1 : 0;

	/* find out how many digits need to be changed */
	for (tail = len - 1; tail >= begin && s[tail] == tgt; tail--);

	if (tail < begin && !neg) {
		/* special case: all 9s, string will grow */
		if (!begin) s = realloc(s, len + 2);
		s[0] = '1';
		for (i = 1; i <= len - begin; i++) s[i] = '0';
		s[len + 1] = '\0';
	} else if (tail == begin && neg && s[1] == '1') {
		/* special case: -1000..., so string will shrink */
		for (i = 1; i < len - begin; i++) s[i] = '9';
		s[len - 1] = '\0';
	} else { /* normal case; change tail to all 0 or 9, change prev digit by 1*/
		for (i = len - 1; i > tail; i--)
			s[i] = neg ? '9' : '0';
		s[tail] += neg ? -1 : 1;
	}

	return s;
}

void string_test(const char *s)
{
	char *ret = malloc(strlen(s));
	strcpy(ret, s);

	printf("text: %s\n", ret);
	printf("  ->: %s\n", ret = incr(ret));
	free(ret);
}

int main()
{
	string_test("+0");
	string_test("-1");
	string_test("-41");
	string_test("+41");
	string_test("999");
	string_test("+999");
	string_test("109999999999999999999999999999999999999999");
	string_test("-100000000000000000000000000000000000000000000");

	return 0;
}
```

{{Out}}
```txt

text: +0
  ->: +1
text: -1
  ->: 0
text: -41
  ->: -40
text: +41
  ->: +42
text: 999
  ->: 1000
text: +999
  ->: 1000
text: 109999999999999999999999999999999999999999
  ->: 110000000000000000000000000000000000000000
text: -100000000000000000000000000000000000000000000
  ->: -99999999999999999999999999999999999999999999
```



## C++


```cpp
// standard C++ string stream operators
#include <cstdlib>
#include <string>
#include <sstream>

// inside a function or method...
std::string s = "12345";

int i;
std::istringstream(s) >> i;
i++;
//or:
//int i = std::atoi(s.c_str()) + 1;

std::ostringstream oss;
if (oss << i) s = oss.str();
```


{{works with|C++11}}

```cpp
#include <string>

std::string s = "12345";
s = std::to_string(1+std::stoi(s));
```


{{libheader|Boost}}

```cpp
// Boost
#include <cstdlib>
#include <string>
#include <boost/lexical_cast.hpp>

// inside a function or method...
std::string s = "12345";
int i = boost::lexical_cast<int>(s) + 1;
s = boost::lexical_cast<std::string>(i);
```


{{libheader|Qt}}
{{uses from|Library|Qt|component1=QString}}

```cpp
// Qt
QString num1 = "12345";
QString num2 = QString("%1").arg(v1.toInt()+1);
```


{{Libheader|MFC}}
{{uses from|Library|Microsoft Foundation Classes|component1=CString}}
{{uses from|Library|C Runtime|component1=_ttoi|component2=_tcstoul}} <!-- They're Microsoft-specific extensions exported in tchar.h. That'll be noted on their relevant pages.-->


```cpp
// MFC
CString s = "12345";
int i = _ttoi(s) + 1;
int i = _tcstoul(s, NULL, 10) + 1;
s.Format("%d", i);
```


All of the above solutions only work for numbers <= INT_MAX. The following works for an (almost) arbitrary large number:

{{works with|g++|4.0.2}}

```cpp
#include <string>
#include <iostream>
#include <ostream>

void increment_numerical_string(std::string& s)
{
    std::string::reverse_iterator iter = s.rbegin(), end = s.rend();
    int carry = 1;
    while (carry && iter != end)
    {
        int value = (*iter - '0') + carry;
        carry = (value / 10);
        *iter = '0' + (value % 10);
        ++iter;
    }
    if (carry)
        s.insert(0, "1");
}

int main()
{
    std::string big_number = "123456789012345678901234567899";
    std::cout << "before increment: " << big_number << "\n";
    increment_numerical_string(big_number);
    std::cout << "after increment:  " << big_number << "\n";
}
```


## C#

```c#
string s = "12345";
s = (int.Parse(s) + 1).ToString();
```



## Ceylon


```ceylon
shared void run() {

	"Increments a numeric string by 1. Returns a float or integer depending on the string.
	 Returns null if the string isn't a number."
	function inc(String string) =>
			if(exists integer = parseInteger(string)) then integer + 1
			else if(exists float = parseFloat(string)) then float + 1.0
			else null;

	value a = "1";
	print(a);
	value b = inc(a);
	print(b);
	value c = "1.0";
	print(c);
	value d = inc(c);
	print(d);
}
```



## Clojure


```lisp
(str (inc (Integer/parseInt "1234")))
```



## CMake

CMake performs all arithmetic with numeric strings, through its math() command.


```cmake
set(string "1599")
math(EXPR string "${string} + 1")
message(STATUS "${string}")
```



```txt
-- 1600
```



## COBOL


```cobol
       PROGRAM-ID. increment-num-str.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  str                    PIC X(5) VALUE "12345".
       01  num                    REDEFINES str PIC 9(5).

       PROCEDURE DIVISION.
           DISPLAY str
           ADD 1 TO num
           DISPLAY str

           GOBACK
           .
```


The following example also increments a numerical string, although it does not apear to. num-str is implicitly defined as <code>USAGE DISPLAY</code> which means its contents will be stored as characters. This means num-str is effectively a string of (numeric) characters.

```cobol
       PROGRAM-ID. increment-num-str.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  num-str                PIC 9(5) VALUE 12345.

       PROCEDURE DIVISION.
           DISPLAY num-str
           ADD 1 TO num-str
           DISPLAY num-str

           GOBACK
           .
```



## Common Lisp


```lisp
(princ-to-string (1+ (parse-integer "1234")))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Operations;
IMPORT StdLog,Args,Strings;

PROCEDURE IncString(s: ARRAY OF CHAR): LONGINT;
VAR
	resp: LONGINT;
	done: INTEGER;
BEGIN
	Strings.StringToLInt(s,resp,done);
	INC(resp);
	RETURN resp
END IncString;

PROCEDURE DoIncString*;
VAR
	p: Args.Params;
BEGIN
	Args.Get(p);
	IF p.argc > 0 THEN
		StdLog.String(p.args[0] + " + 1= ");StdLog.Int(IncString(p.args[0]));StdLog.Ln
	END
END DoIncString;

END Operations.

```

Execute: ^Q Operatiosn.DoIncString 124343~<br/>
{{Out}}

```txt

124343 + 1=  124344

```


## D


```d
void main() {
    import std.string;

    immutable s = "12349".succ;
    assert(s == "12350");
}
```



## Delphi


```Delphi
program IncrementNumericalString;

{$APPTYPE CONSOLE}

uses SysUtils;

const
  STRING_VALUE = '12345';
begin
  WriteLn(Format('"%s" + 1 = %d', [STRING_VALUE, StrToInt(STRING_VALUE) + 1]));

  Readln;
end.
```


{{Out}}

```txt
"12345" + 1 = 123456
```


=={{header|Déjà Vu}}==

```dejavu
 !. to-str ++ to-num "100"
```


{{out}}

```txt
"101"
```



## DWScript


```Delphi
var value : String = "1234";
value := IntToStr(StrToInt(value) + 1);
PrintLn(value);
```



## E


```e
__makeInt("1234", 10).next().toString(10)
```



## EasyLang


<lang>a$ = "12"
a$ = number a$ + 1
print a$
```



## EchoLisp


```scheme

(number->string (1+ (string->number "665")))
    → "666"

```



## Eero


```objc>#import <Foundation/Foundation.h


int main()

  i := (int)'123' + 1
  s := @(i).description

  Log( '%@', s )
  return 0
```



## EGL


```EGL
s string = "12345";
s = 1 + s;  // Note: s + 1 is a string concatenation.
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE}

	make
		do
			io.put_string (increment_numerical_string ("7"))
			io.new_line
			io.put_string (increment_numerical_string ("99"))
		end

	increment_numerical_string (s: STRING): STRING
			-- String 's' incremented by one.
		do
			Result := s.to_integer.plus (1).out
		end

end

```

Output:

```txt

8

100

```


## Elena

ELENA 4.x:

```elena
import extensions;

public program()
{
    var s := "12345";
    s := (s.toInt() + 1).toString();

    console.printLine(s)
}
```

{{out}}

```txt

12346

```



## Elixir

Values can be converted to integers then converted back after incrementing

```Elixir
increment1 = fn n -> to_string(String.to_integer(n) + 1) end
# Or piped
increment2 = fn n -> n |> String.to_integer |> +1 |> to_string end

increment1.("1")
increment2.("100")
```

{{out}}

```txt

"2"
"101"

```



'''Case of char list:'''

```Elixir
iex(1)> (List.to_integer('12345') + 1) |> to_char_list
'12346'
```



## Emacs Lisp



```lisp
(1+ (string-to-number "12345"))
```



## Erlang



```erlang
integer_to_list(list_to_integer("1336")+1).
```



## ERRE

<lang>
   ....................
    s$="12345"
    s$=STR$(VAL(s$)+1)
   ....................

```



## Euphoria


```euphoria
include get.e

function val(sequence s)
    sequence x
    x = value(s)
    return x[2]
end function

sequence s

s = "12345"
s = sprintf("%d",{val(s)+1})
```



## Factor


```factor
"1234" string>number 1 + number>string
```



## Fantom


Within 'fansh':


```fantom

fansh> a := "123"
123
fansh> (a.toInt + 1).toStr
124

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Increment_a_numerical_string this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

This word causes the number whose string value is stored at the given location to be incremented. The address passed must contain enough space to hold the string representation of the new number. Error handling is rudimentary, and consists of aborting when the string does not contain a numerical value.

The word ">string" takes and integer and returns the string representation of that integer. I factored it out of the definitions below to keep the example simpler.


```forth>:
string ( d -- addr n )
  dup >r dabs <# #s r> sign #> ;

: inc-string ( addr -- )
  dup count number? not abort" invalid number"
  1 s>d d+ >string rot place ;
```


Here is a version that can increment by any value


```forth
: inc-string ( addr n -- )
  over count number? not abort" invalid number"
  rot s>d d+  >string rot place ;
```


Test the first version like this:


```forth
s" 123" pad place
pad inc-string
pad count type
```


And the second one like this:


```forth
s" 123" pad place
pad 1 inc-string
pad count type
```



## Fortran

{{works with|Fortran|90 and later}}
Using 'internal' files you can increment both integer and real strings

```fortran
CHARACTER(10) :: intstr = "12345", realstr = "1234.5"
INTEGER :: i
REAL :: r

READ(intstr, "(I10)") i        ! Read numeric string into integer i
i = i + 1                      ! increment i
WRITE(intstr, "(I10)") i       ! Write i back to string

READ(realstr, "(F10.1)") r
r = r + 1.0
WRITE(realstr, "(F10.1)") r
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function Increment (num As String) As String
  Dim d As Double = Val(num)
  Return Str(d + 1.0)
End Function

Dim num(5) As String = {"1", "5", "81", "123.45", "777.77", "1000"}
For i As Integer = 0 To 5
  Print num(i); " + 1", " = "; Increment(num(i))
Next

Print
Print "Press any key to exit"
Sleep
```


{{out}}

```txt

1 + 1          = 2
5 + 1          = 6
81 + 1         = 82
123.45 + 1     = 124.45
777.77 + 1     = 778.77
1000 + 1       = 1001

```


=={{header|F_Sharp|F#}}==

```fsharp
let next = string( int( "1234" ) + 1 )
```



## Frink

The following works for integers, rational numbers, complex numbers, floating-point, etc.

```frink

a = input["Enter number: "]
toString[eval[a] + 1]

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as Str255 numStr
dim as long   numeric, i

numStr = "12345"
numeric = val(numStr)
numeric++
numStr = str$(numeric)
print numStr

print

for i = 0 to 10
   numeric++
   numStr = str$( numeric )
   print numStr
next

```


Output:

```txt

 12346

 12347
 12348
 12349
 12350
 12351
 12352
 12353
 12354
 12355
 12356
 12357

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=e31b63f444c2c09c8b1436407020e216 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim vInput As Variant = "12345"

Inc vInput
Print vInput

End
```

Output:

```txt

12346

```



## GAP


```gap
# Using built-in functions
Incr := s -> String(Int(s) + 1);

# Implementing addition
# (but here 9...9 + 1 = 0...0 since the string length is fixed)
Increment := function(s)
  local c, n, carry, digits;
  digits := "0123456789";
  n := Length(s);
  carry := true;
  while n > 0 and carry do
    c := Position(digits, s[n]) - 1;
    if carry then
      c := c + 1;
    fi;
    if c > 9 then
      carry := true;
      c := c - 10;
    else
      carry := false;
    fi;
    s[n] := digits[c + 1];
    n := n - 1;
  od;
end;

s := "2399";
Increment(s);
s;
# "2400"
```



## Go

Concise:

```go
package main
import "fmt"
import "strconv"
func main() {
  i, _ := strconv.Atoi("1234")
  fmt.Println(strconv.Itoa(i + 1))
}
```

More:

```go
package main

import (
    "math/big"
    "fmt"
    "strconv"
)

func main() {
    // integer
    is := "1234"
    fmt.Println("original:   ", is)
    i, err := strconv.Atoi(is)
    if err != nil {
        fmt.Println(err)
        return
    }
    // assignment back to original variable shows result is the same type.
    is = strconv.Itoa(i + 1)
    fmt.Println("incremented:", is)

    // error checking worthwhile
    fmt.Println()
    _, err = strconv.Atoi(" 1234") // whitespace not allowed
    fmt.Println(err)
    _, err = strconv.Atoi("12345678901")
    fmt.Println(err)
    _, err = strconv.Atoi("_1234")
    fmt.Println(err)
    _, err = strconv.ParseFloat("12.D34", 64)
    fmt.Println(err)

    // float
    fmt.Println()
    fs := "12.34"
    fmt.Println("original:   ", fs)
    f, err := strconv.ParseFloat(fs, 64)
    if err != nil {
        fmt.Println(err)
        return
    }
    // various options on FormatFloat produce different formats.  All are valid
    // input to ParseFloat, so result format does not have to match original
    // format.  (Matching original format would take more code.)
    fs = strconv.FormatFloat(f+1, 'g', -1, 64)
    fmt.Println("incremented:", fs)
    fs = strconv.FormatFloat(f+1, 'e', 4, 64)
    fmt.Println("what format?", fs)

    // complex
    // strconv package doesn't handle complex types, but fmt does.
    // (fmt can be used on ints and floats too, but strconv is more efficient.)
    fmt.Println()
    cs := "(12+34i)"
    fmt.Println("original:   ", cs)
    var c complex128
    _, err = fmt.Sscan(cs, &c)
    if err != nil {
        fmt.Println(err)
        return
    }
    cs = fmt.Sprint(c + 1)
    fmt.Println("incremented:", cs)

    // big integers have their own functions
    fmt.Println()
    bs := "170141183460469231731687303715884105728"
    fmt.Println("original:   ", bs)
    var b, one big.Int
    _, ok := b.SetString(bs, 10)
    if !ok {
        fmt.Println("big.SetString fail")
        return
    }
    one.SetInt64(1)
    bs = b.Add(&b, &one).String()
    fmt.Println("incremented:", bs)
}
```

{{out}}

```txt

original:    1234
incremented: 1235

strconv.ParseInt: parsing " 1234": invalid syntax
strconv.ParseInt: parsing "12345678901": value out of range
strconv.ParseInt: parsing "_1234": invalid syntax
strconv.ParseFloat: parsing "12.D34": invalid syntax

original:    12.34
incremented: 13.34
what format? 1.3340e+01

original:    (12+34i)
incremented: (13+34i)

original:    170141183460469231731687303715884105728
incremented: 170141183460469231731687303715884105729

```



## Golfscript


```golfscript
~)`
```

With a test framework to supply a number:

```golfscript
"1234" ~)` p
```



## Groovy

Solution:

```groovy
println ((("23455" as BigDecimal) + 1) as String)
println ((("23455.78" as BigDecimal) + 1) as String)
```


{{Out}}

```txt
23456
23456.78
```



## Haskell

<!--
```haskell
((show :: Integer -> String) . succ . read) "1234"
```


At least one type signature somewhere in the code is necessary, because otherwise there is no specification of what kind of value should be incremented; the operation is equally applicable to any instance of <tt>Enum</tt>:


```haskell
((show :: Bool -> String) . succ . read) "False"
```
-->


```haskell
(show . (+1) . read) "1234"
```


or, for Integral values, we can use the Prelude's '''succ''' function:


```haskell
(show . succ) (read "1234" :: Int)
```


and to extend the range of a succString function to allow for both floating point and integral numeric strings, for non-numeric noise, for multiple numeric expressions within a single string, and for an option to retain or prune any non-numeric noise, we could write things like:

{{Trans|Python}}

```haskell
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

succString :: Bool -> String -> String
succString pruned s =
  let succs
        :: (Num a, Show a)
        => a -> Maybe String
      succs = Just . show . (1 +)
      go w
        | elem '.' w = (readMaybe w :: Maybe Double) >>= succs
        | otherwise = (readMaybe w :: Maybe Integer) >>= succs
      opt w
        | pruned = Nothing
        | otherwise = Just w
  in unwords $
     mapMaybe
       (\w ->
           case go w of
             Just s -> Just s
             _ -> opt w)
       (words s)


-- TEST ---------------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines) $
  succString <$> [True, False] <*>
  pure "41.0 pine martens in 1491 -1.5 mushrooms ≠ 136"
```

{{Out}}

```txt
42.0 1492 -0.5 137
42.0 pine martens in 1492 -0.5 mushrooms ≠ 137
```



## HicEst


```hicest
CHARACTER string = "123     -4567.89"

   READ( Text=string) a,   b
   WRITE(Text=string) a+1, b+1 ! 124 -4566.89
```




## HolyC


```holyc
I8 *s;

s = "10";
s = Str2I64(s) + 1;
Print("%d\n", s);

s = "-10";
s = Str2I64(s) + 1;
Print("%d\n", s);
```



## Hy


```clojure
(str (inc (int "123")))
```

Alternatively, with the "threading" macro:

```clojure
(-> "123" (int) (inc) (str))
```



## HyperTalk


```hypertalk
put 0 into someVar
add 1 to someVar
-- without "into [field reference]" the value will appear
-- in the message box
put someVar -- into cd fld 1
```



## i


```i
software {
	string = "1"
	string += 1
	print(string)
}
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon will automatically coerce type conversions where they make sense.  Where a conversion can't be made to a required type a run time error is produced.


```Icon
s := "123"  # s is a string
s +:= 1     # s is now an integer
```



## IDL


```idl
str = '1234'
print, string(fix(str)+1)
;==>   1235
```


In fact, IDL tries to convert types cleverly. That works, too:


```idl
print, '1234' + 1
;==>    1235
```



## Inform 7

This solution works for numbers that fit into a single word (16-bit signed int for [[Z-machine]], 32-bit signed int for [[Glulx virtual machine]]).

```inform7
Home is a room.

To decide which indexed text is incremented (T - indexed text):
	let temp be indexed text;
	let temp be the player's command;
	change the text of the player's command to T;
	let N be a number;
	if the player's command matches "[number]":
		let N be the number understood;
	change the text of the player's command to temp;
	decide on "[N + 1]".

When play begins:
	say incremented "12345";
	end the story.
```



## Io

<lang>str := ("123" asNumber + 1) asString
```



## J


```j>incrTextNum=:
:&.".
```


Note that in addition to working for a single numeric value, this will increment multiple values provided within the same string, on a variety of number types and formats including rational and complex numbers.

```j
   incrTextNum '34.5'
35.5
   incrTextNum '7 0.2 3r5 2j4 5.7e_4'
8 1.2 1.6 3j4 1.00057
```


Note also that the result here is a list of characters, and not a list of integers, which becomes obvious when you manipulate the result.  For example, consider the effect of reversing the contents of the list:


```j
   |.incrTextNum'123 456'
754 421
   |.1+123 456
457 124
```



## Java

When using <tt>Integer.parseInt</tt> in other places, it may be beneficial to call <tt>trim</tt> on the String, since <tt>parseInt</tt> will throw an Exception if there are spaces in the String.

```java
String s = "12345";
s = String.valueOf(Integer.parseInt(s) + 1);
```


Another solution that works with big decimal numbers:

```java
String s = "123456789012345678901234567890.12345";
s = new BigDecimal(s).add(BigDecimal.ONE).toString();
```



## JavaScript


### ES6


Using implicit coercion:


```javascript
let s = '9999';
let splusplus = (+s+1)+""

console.log([splusplus, typeof splusplus]) // 10000,string
```


Or, expanding the range of a '''stringSucc''' function to allow for non-numeric noise, and also for multiple numeric expressions in a single string:

{{Trans|Python}}
{{Trans|Haskell}}

```javascript
(() => {
    'use strict';

    // succString :: Bool -> String -> String
    const succString = blnPruned => s => {
        const go = w => {
            const
                v = w.includes('.') ? (
                    parseFloat(w)
                ) : parseInt(w);
            return isNaN(v) ? (
                blnPruned ? [] : [w]
            ) : [(1 + v).toString()];
        };
        return unwords(concatMap(go, words(s)));
    };

    // TEST -----------------------------------------------
    const main = () =>
        console.log(
            unlines(
                ap(
                    map(succString, [true, false]),
                    ['41 pine martens in 1491.3 -1.5 mushrooms ≠ 136']
                )
            )
        );


    // GENERIC FUNCTIONS ----------------------------------

    // Each member of a list of functions applied to each
    // of a list of arguments, deriving a list of new values.

    // ap (<*>) :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        fs.reduce((a, f) => a.concat(
            xs.reduce((a, x) => a.concat([f(x)]), [])
        ), []);

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // MAIN ---
    return main();
})();
```


{{Out}}

```txt
42 1492.3 -0.5 137
42 pine martens in 1492.3 -0.5 mushrooms ≠ 137
```



## jq


### =tonumber=

jq's string-to-number filter is called <tt>tonumber</tt>.  For example, if we have a file named input.txt consisting of string representations of numbers, one per line, we could compute the sum as follows:

```sh
$ jq -n -M -s 'map(tonumber) | add' input.txt
```


More precisely, <tt>tonumber</tt> will convert string representations of JSON numbers (integers and decimals) to numbers, but very large integers will be converted to decimals with possible loss of precision, and similar problems will be noticeable for very small and very large decimals.

<tt>tostring</tt> can be used to convert numbers to strings.
====long_add====

```jq
# This function assumes its string arguments represent non-negative decimal integers.
def long_add(num1; num2):
  if (num1|length) < (num2|length) then long_add(num2; num1)
  else  (num1 | explode | map(.-48) | reverse) as $a1
      | (num2 | explode | map(.-48) | reverse) as $a2
      | reduce range(0; num1|length) as $ix
          ($a2;  # result
           ( $a1[$ix] + .[$ix] ) as $r
           | if $r > 9 # carrying
             then
               .[$ix + 1] = ($r / 10 | floor) +
                 (if $ix + 1 >= length then 0 else .[$ix + 1] end )
               | .[$ix] = $r - ( $r / 10 | floor ) * 10
             else
               .[$ix] = $r
             end )
      | reverse | map(.+48) | implode
  end ;
```

'''Example'''

```jq

long_add("9" * 100; "1")
```

{{Out}}
 "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"


## Jsish


```javascript
var a = "1"
a = String(Number(a) + 1)
```



## Julia

{{works with|Julia|0.6}}


```julia
Base.:+(s::AbstractString, n::Real) = string(parse(s) + n)

@show "125" + 1
@show "125.15" + 1
@show "1234567890987654321" + 1
```


{{out}}

```txt
"125" + 1 = "126"
"125.15" + 1 = "126.15"
"1234567890987654321" + 1 = "1234567890987654322"
```



## K

"." is a built-in function that evaluates a valid K expression.


```K
   1 + ."1234"
1235
   1 + ."1234.56"
1235.56

   / As a function
   inc:{1 + . x}
   inc "1234"
1235
```


Some other examples.

```K
   1 + .:' ("1";"2";"3";"4")
2 3 4 5

   1 + . "123 456"
124 457

   . "1","+","-10"
-9
```



## Kotlin


```scala
// version 1.0.5-2

/** overload ++ operator to increment a numeric string */
operator fun String.inc(): String =
    try {
        val num = this.toInt()
        (num + 1).toString()
    }
    catch(e: NumberFormatException) {
        this  // return string unaltered
    }

fun main(args: Array<String>) {
    var ns = "12345"
    println(++ns)
    ns = "ghijk"  // not numeric, so won't be changed by increment operator
    println(++ns)
}
```


{{out}}

```txt

12346
ghijk

```



## LabVIEW

{{VI snippet}}<br/>
[[File:LabVIEW_Increment_a_numerical_string.png]]


## Lasso


```Lasso
(integer('123') + 1) -> asstring
```

-> 124


## LaTeX


```latex
\documentclass{article}

% numbers are stored in counters
\newcounter{tmpnum}

% macro to increment a string (given as argument)
\newcommand{\stringinc}[1]{%
\setcounter{tmpnum}{#1}%     setcounter effectively converts the string to a number
\stepcounter{tmpnum}%        increment the counter; alternatively: \addtocounter{tmpnum}{1}
\arabic{tmpnum}%             convert counter value to arabic (i.e. decimal) number string
}

%example usage
\begin{document}
The number 12345 is followed by \stringinc{12345}.
\end{document}
```



## Liberty BASIC


```lb
'   [RC] Increment a numerical string.

o$ ="12345"
print o$

v  =val( o$)
o$ =str$( v +1)
print o$

end
```



## LIL


```tcl
##
   Increment a numerical string, in LIL
##
set a "41"
inc a
print $a
```


{{out}}

```txt
prompt$ lil incrementNumericalString.lil
42
```



## Lingo


```lingo
put integer("123")+1
-- 124
```



## LiveCode

LiveCode casts types transparently. When storing a number in a variable, the internal representation is numeric (a double, I think), and if the variable is used as a number, there is no type conversion.
If the variable is used as a string, the conversion is automatic; likewise if a string variable containing a number is used as a number:

```LiveCode
put "0" & "1234" into myString -- I think this will result in an internal string representation
add 1 to myString -- automatically converts to a number
put "The number is:" && myString
-- outputs "The number is: 1235"
```



## Logo

Logo is weakly typed, so numeric strings can be treated as numbers and numbers can be treated as strings.

```logo
show "123 + 1  ; 124
show word? ("123 + 1) ;  true
```



## Logtalk


```logtalk
number_chars(Number, "123"), Number2 is Number+1, number_chars(Number2, String2)
```



## LOLCODE


LOLCODE is weakly typed, so the arithmetic operators work "as expected" on strings.


```LOLCODE
HAI 1.3

I HAS A foo ITZ "1234"
foo R SUM OF foo AN 1
VISIBLE foo BTW, prints 1235

KTHXBYE
```



## LSL

To test it yourself; rez a box on the ground, and add the following as a New Script.

```LSL
default {
	state_entry() {
		llListen(PUBLIC_CHANNEL, "", llGetOwner(), "");
		llOwnerSay("Say a Number and I'll Increment it.");
	}
	listen(integer iChannel, string sName, key kId, string sMessage) {
		llOwnerSay("You said '"+sMessage+"' + 1 = "+(string)(((integer)sMessage)+1));
	}
}
```

{{Out}}

```txt
Increment_a_Numerical_String: You said '99999999' + 1 = 100000000
Increment_a_Numerical_String: You said '-100000000' + 1 = -99999999
```



## Lua

Lua will attempt an implicit type conversion if an arithmetic operator is used with a string. This is illustrated by the following interactive session:

```txt
> s = "2345"
> =type(s)
string
> n = s + 1
> =n
2346
> =type(n)
number
> s = "This string won't convert to a number."
> n = s + 1
stdin:1: attempt to perform arithmetic on global 's' (a string value)
stack traceback:
        stdin:1: in main chunk
        [C]: at 0x01271f20
```



## M2000 Interpreter

Using Str$( stringexpr, "") we trim leading space for positive numbers. We can use a number as locale id, 1032 for Greece, so Str$(12.1212, 1032) return without leading space "12,1212".

Str$(mumberexpression) return always dot for decimal point
Val(stringexpression) read dot as decimal point
Val(stringexpression, ",") use "," as decimal point
Val(stringexpression, 1032) use Locale 1032 (maybe this can change by OS user), so we get "." or "," (the later is by default the decimal point for 1032).

We can use   Eval("1212211212122112215@") which evaluate expressions in strings (among other duties when we use objects), and we can use letters to indicate the type of number
Val can return type using a special form, using a numeric expression as first parameter: Local m=Val(112+1.12->Decimal) make a new variable m type of decimal (96-bit integer with a variable power of 10).


```M2000 Interpreter

Module CheckIt {
      s$ = "12345"
      s$ = STR$(VAL(s$) + 1,"")
      Print S$
      \\ using , for decimal point like in Locale 1032 for Greece
      s$ = "123,45"
      \\ we get value in Locale 1032
      S$=Str$(VAL(s$,",") + 1, 1032)
}
CheckIt

```



## M4

M4 can handle only integer signed 32 bit numbers, and they can be only written as strings

```m4
define(`V',`123')dnl
define(`VN',`-123')dnl
eval(V+1)
eval(VN+1)
```


If the expansion of any macro in the argument of eval gives something that can't be interpreted as an expression, an error is raised (but the ''interpretation'' of the whole file is not stopped)


## Maple


```maple
s := "12345";
s := convert(parse(s)+1, string);
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
FromDigits["1234"] + 1
```



## MATLAB


```MATLAB
function numStr = incrementNumStr(numStr)
    numStr = num2str(str2double(numStr) + 1);
end
```




## MAXScript


```maxscript
str = "12345"
str = ((str as integer) + 1) as string
```



## Metafont


```metafont
string s;
s := "1234";
s := decimal(scantokens(s)+1);
message s;
```



## min

{{works with|min|0.19.3}}

```min
(int succ string) :next
```



## mIRC Scripting Language


```mirc
var %n = 12345
inc %n
echo -ag %n
```



## ML

=
## mLite
=

```ocaml
ntos ` ston "1234" + 1;

```


=
## Standard ML
=

```sml
Int.toString (1 + valOf (Int.fromString "1234"))
```


=={{header|Modula-2}}==

```modula2
MODULE addstr;

IMPORT  InOut, NumConv, Strings;

VAR     str1, str2      : Strings.String;
        num             : CARDINAL;
        ok              : BOOLEAN;

BEGIN
  str1 := "12345";
  InOut.Write ('"');    InOut.WriteString (str1);       InOut.WriteString ('" + 1 = ');
  NumConv.Str2Num (num, 10, str1, ok);
  INC (num);
  NumConv.Num2Str (num, 10, str2,  ok);
  InOut.WriteString (str2);
  InOut.WriteLn
END addstr.
```


```txt
"12345" + 1 = 12346
```


=={{header|Modula-3}}==
Modula-3 provides the module <tt>Scan</tt> for lexing.

```modula3
MODULE StringInt EXPORTS Main;

IMPORT IO, Fmt, Scan;

VAR string: TEXT := "1234";
    num: INTEGER := 0;

BEGIN
  num := Scan.Int(string);
  IO.Put(string & " + 1 = " & Fmt.Int(num + 1) & "\n");
END StringInt.
```

{{Out}}

```txt

1234 + 1 = 1235

```



## MUMPS

Just add. MUMPS has strings of characters as its native datatype. The "+" (plus) binary operator interprets its two arguments as numbers, so the MUMPS system does incrementing a string naturally. MUMPS portability standards require that the result must have at least 15 significant digits. Some implementations use Binary Coded Digits (BCD) and long fixed point (64 bit) integers to accomplish this.

```MUMPS

 SET STR="123"
 WRITE STR+1

```



## Neko


```Neko
var str = "123";
var str = $string($int(str) + 1);

$print(str);
```



## Nemerle


```Nemerle
mutable str = "12345";
str = $"$(Int32.Parse(str)+1)";
```



## NetRexx

In concert with [[REXX|Rexx]], NetRexx can use typeless variables. Typeless variable support is provided through the default NetRexx <code>'''Rexx'''</code> object.  Values are stored as variable length character strings and can be treated as either a string or a numeric value, depending on the context in which they are used.

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

numbers = '12345'
say numbers
numbers = numbers + 1
say numbers

return

```

{{Out}}

```txt

12345
12346

```



## NewLISP


```NewLISP
(string (++ (int "123")))
```



## Nim


```nim
import strutils
let next = $(parseInt("123") + 1)
```


=={{header|Oberon-2}}==

```oberon2
MODULE addstr;

IMPORT  Out, Strings;

VAR     str1, str2      : ARRAY 9 OF CHAR;
        num, pos        : INTEGER;
        carry           : BOOLEAN;
        ch              : CHAR;

BEGIN
  str1 := "9999";
  Out.Char ('"');       Out.String (str1);      Out.String ('" + 1 = ');
  num := Strings.Length (str1) - 1;
  pos := num;
  IF  str1 [0] = '9'  THEN  INC (pos)  END;
  str2 [pos + 1] := 0X;
  carry := TRUE;
  REPEAT
    ch := str1 [num];
    IF  carry  THEN
      ch := CHR (ORD (ch) + 1)
    END;
    IF  ch > '9'  THEN
      carry := TRUE;
      ch := '0'
    ELSE
      carry := FALSE
    END;
    str2 [pos] := ch;
    DEC (num);
    DEC (pos)
  UNTIL num < 0;
  IF  carry  THEN  str2 [0] := '1'  END;
  Out.String (str2);
  Out.Ln
END addstr.
```

Producing:

```txt
jan@Beryllium:~/Oberon/obc$ Add
"12345" + 1 = 12346
"9999" + 1 = 10000
```



## Objeck


```objeck

s := "12345";
i := int->ToInt(s) + 1;
s := i->ToString();

```


=={{header|Objective-C}}==

```objc
NSString *s = @"12345";
int i = [s intValue] + 1;
s = [NSString stringWithFormat:@"%i", i]
```



## OCaml


```ocaml
string_of_int (succ (int_of_string "1234"))
```



## Octave


We convert the string to a number, increment it, and convert it back to a string.


```octave
nstring = "123";
nstring = sprintf("%d", str2num(nstring) + 1);
disp(nstring);
```




## Oforth

+ on strings is a concatenation, not an addition. To increment, the string is converted as integer then as string again.


```Oforth
"999" 1 + println
"999" asInteger 1 + asString println
```


{{out}}

```txt

9991
1000

```



## OoRexx

ooRexx supports the += etc. operators:

```oorexx
i=1
i+=1
Say i
```

{{Out}}

```txt

2

```



## OpenEdge/Progress


```progress
DEFINE VARIABLE cc AS CHARACTER INITIAL "12345".

MESSAGE
   INTEGER( cc ) + 1
VIEW-AS ALERT-BOX.
```



## Oz


```oz
{Int.toString {String.toInt "12345"} + 1}
```



## PARI/GP


```parigp
foo(s)=Str(eval(s)+1);
```



## Pascal

{{works with| Free Pascal}} not like [[Increment_a_numerical_string#Delphi | Delphi]] doing two conversion, but increment a string by 1 is much faster.
Here with different bases upto 10.After this there must be a correction to convert values > '9' to 'A'...
Only for positive integer strings as high speed counter.

```pascal
program StrInc;
{$IFDEF FPC}
  {$Mode Delphi}
  {$Optimization ON}{$Align 16}{$Codealign proc=16,loop=4}
{$ENDIF}

uses
  sysutils;

type
  myString =  AnsiString; // string[32];//

function InCLoop(ps: pChar;le,Base: NativeInt):NativeInt;
//Add 1 and correct carry
//returns 0, if no overflow, else -1
var
  dg: nativeInt;
Begin
  dec(le);//ps is 0-based
  repeat
    dg := ord(ps[le])+(-ord('0')+1);
    result  := -ord(dg>=base);// -1 or 0 -> $FF...FF or $00...00
    ps[le] := chr(-(result AND base)+dg+ord('0'));
    dec(le);
  until (result = 0) or (le<0);
end;

procedure IncIntStr(base:NativeInt;var s:myString);
var
  le :nativeInt;
begin
  le := length(s);
  IF le > 0 then
  Begin
    if (InCLoop(pChar(@s[1]),le,base) <>0) then
    begin
      setlength(s,le+1);
      move(s[1],s[2],le);
      s[1] := '1';
    end
  end
  else
  Begin
    setlength(s,1);
    s[1] := '1';
  end;
end;

const
  strLen = 26;
  MAX = 1 shl strLen -1;

var
  s  : myString;
  i,base : nativeInt;
  T1,T0: TDateTime;
Begin
  For base := 2 to 10 do
  Begin
    s:= '';
{
    //Zero pad string
    //s:= '0';// doesn't work for AnsiString for FPC 3.0 but for 2.6.4?
    //This works for all Ansi-string
    setlength(s,strLen);fillchar(s[1],strLen,'0');
}
    T0 := time;
    For i := 1 to MAX do
      IncIntStr(Base,s);
    T0 := (time-T0)*86400;
    writeln(s:strLen,' base ',base:2,T0:8:3,' s');
//One Billion Digits
  setlength(s,1000*1000*1000+1);
  s[1]:= '0';//don't measure setlength in IncIntStr
  fillchar(s[2],length(s)-1,'9');
  writeln('first 5 digits ',s[1],s[2],s[3],s[4],s[5]);
  T0 := time;
  IncIntStr(s,10);
  T0 := (time-T0)*86400;
  writeln(length(s):10,T0:8:3,' s');
  writeln('first 5 digits ',s[1],s[2],s[3],s[4],s[5]);
  end;
end.
```

Output:

```txt
 ppc386 aka 32-Bit fpc 3.0.1
11111111111111111111111111 base  2   0.423 s
         11200021111001110 base  3   0.364 s
             3333333333333 base  4   0.331 s
              114134440423 base  5   0.345 s
               10354213103 base  6   0.325 s
                1443262443 base  7   0.318 s
                 377777777 base  8   0.312 s
                 150244043 base  9   0.308 s
                  67108863 base 10   0.305 s
first 5 digits 09999
1000000001   1.299 s
//4.55 cpu-cycles per digit == 3.5 Ghz [cycle/s]*1.3[s]/1e9[digits]  (IPC = 17/4.55 = 3.73 wow)
first 5 digits 10000

```



## Perl


```perl
my $s = "12345";
$s++;
```



## Perl 6

{{works with|Rakudo|#22 "Thousand Oaks"}}


```perl6
my $s = "12345";
$s++;
```



## Phix


```Phix
integer {{n}} = scanf("2047","%d")
printf(1,"%d\n",{n+1})
```

{{out}}

```txt

2048

```



## PHP


```php
$s = "12345";
$s++;
```



## PicoLisp


```PicoLisp
(format (inc (format "123456")))
```



## Pike


```Pike
string number = "0";
number = (string)((int)number+1);
Result: "1"
```



## PL/I


```pli
declare s picture '999999999';
s = '123456789';
s = s + 1;
put skip list (s);
```


```txt

Warning:
With s='999999999'
the result shown would be
000000000
It is advisable to enable the SIZE condition
(size):
 s = s + 1;
which will diagnose this problem:
IBM0342I  ONCODE=0340  The SIZE condition was raised.
   At offset +000000B9 in procedure with entry IB1

```



## plainTeX



```tex
\newcount\acounter
\def\stringinc#1{\acounter=#1\relax%
\advance\acounter by 1\relax%
\number\acounter}
The number 12345 is followed by \stringinc{12345}.
\bye
```


The generated page will contain the text:


```txt

The number 12345 is followed by 12346.

```



## Pop11


```pop11
lvars s = '123456789012123456789999999999';
(strnumber(s) + 1) >< '' -> s;
```



## PowerShell

The easiest way is to cast the string to int, incrementing it and casting back to string:

```powershell
$s = "12345"
$t = [string] ([int] $s + 1)
```

One can also take advantage of the fact that PowerShell casts automatically according to the left-most operand to save one cast:

```powershell
$t = [string] (1 + $s)
```



## Prolog

Works with SWI-Prolog.

```Prolog
incr_numerical_string(S1, S2) :-
	string_to_atom(S1, A1),
	atom_number(A1, N1),
	N2 is N1+1,
	atom_number(A2, N2),
	string_to_atom(S2, A2).

```

{{Out}}

```Prolog
 ?- incr_numerical_string("123", S2).
S2 = "124".

```



## PureBasic


```PureBasic
string$="12345"
string$=Str(Val(string$)+1)
Debug string$
```



## Python

{{works with|Python|2.3 through 3.4}}

```python
next = str(int('123') + 1)
```


Or, preserving the distinction between integer and floating point numeric values, while also allowing for noisy or multi-number numerical strings, and providing the option of retaining or pruning out any non-numeric parts of the string.

```python
# Dropping or keeping any non-numerics in the string


# succString :: Bool -> String -> String
def succString(blnPruned):
    def go(x):
        try:
            return [str(1 + (float(x) if '.' in x else int(x)))]
        except ValueError:
            return [] if blnPruned else [x]
    return lambda s: ' '.join(concatMap(go)(s.split()))


# TEST ----------------------------------------------------
def main():
    print(
        '\n'.join(
            [succString(bln)(
                '41.0 pine martens in 1491 -1.5 mushrooms ≠ 136'
            ) for bln in [False, True]]
        )
    )


# GENERIC ---------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    return lambda xs: (
        [ys[0] for ys in [f(x) for x in xs] if ys]
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
42.0 pine martens in 1492 -0.5 mushrooms ≠ 137
42.0 1492 -0.5 137
```



## R


```r
s = "12345"
s <- as.character(as.numeric(s) + 1)
```



## Racket


```racket

#lang racket
(define next (compose number->string add1 string->number))

```



## Rascal


```rascal

import String;

public str IncrNumStr(str s) = "<toInt(s) + 1>";

```

{{Out}}

```txt

rascal>IncrNumStr("123")
str: "124"

```



## REBOL


```REBOL
REBOL [
	Title: "Increment Numerical String"
	URL: http://rosettacode.org/wiki/Increment_numerical_string
]

; Note the use of unusual characters in function name. Also note that
; because REBOL collects terms from right to left, I convert the
; string argument (s) to integer first, then add that result to one.

s++: func [s][to-string 1 + to-integer s]

; Examples. Because the 'print' word actually evaluates the block
; (it's effectively a 'reduce' that gets printed space separated),
; it's possible for me to assign the test string to 'x' and have it
; printed as a side effect. At the end, 'x' is available to submit to
; the 's++' function. I 'mold' the return value of s++ to make it
; obvious that it's still a string.

print [x: "-99" "plus one equals" mold s++ x]
print [x: "42" "plus one equals" mold s++ x]
print [x: "12345" "plus one equals" mold s++ x]
```


{{Out}}

```txt
-99 plus one equals "-98"
42 plus one equals "43"
12345 plus one equals "12346"
```



## Retro



```Retro
'123 s:to-number n:inc n:to-string
```



## REXX


REXX, like many other scripting languages, uses typeless variables.

Typeless variables are stored as variable length character strings and can be treated as

either a string or a numeric value, depending on the context in which they are used.

### version 1


```rexx
/*REXX program demonstrates a method how to increment a numerical string*/
count = "3"      /*REXX variables (and constants) are character strings.*/
count =  3       /*(identical to the above statement.)                  */
count = count+1  /*strings in a numerical context are treated as numbers*/
say 'sum=' count /*display the value of  COUNT  to the terminal (screen)*/

/*────────────────── The default numeric digits for REXX is  9  digits. */
/*────────────────── However, that can be increased with NUMERIC DIGITS.*/

numeric digits 15000   /*let's go ka-razy with fifteen thousand digits. */

count=copies(2,15000)  /*stressing REXX's brains with lots of  two's,   */
                       /*the above is considered a number in REXX.      */
count=count+3          /*make that last digit of  COUNT  a  "5".        */

if 1==0  then          /*let's not display this gihugeic number to term,*/
say  'count='  count   /*ya most likely don't want to display this thing*/

                       /* [↓]  show the six leftmost and rightmost digs.*/
say  'count='  left(count,6)'···'right(count,6)
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

sum= 4
count= 222222···222225

```



### version 2

Looking at the PL/I code I started investigating this situation in Rexx.
These are my findings:

```rexx
/* REXX ************************************************************
* There is no equivalent to PL/I's SIZE condition in REXX.
* The result of an arithmetic expression is rounded
* according to the current setting of Numeric Digits.
* ooRexx introduced, however, a LOSTDIGITS condition that checks
* if any of the OPERANDS exceeds this number of digits.
*      Unfortunately this check is currently a little too weak
*      and will not recognise a 10-digit number to be too large.
*      This little bug will be fixed in the next release of ooRexx.
**********************************************************************/
Parse Version v .
Say v
z=999999998
Do i=1 To 3
  z=z+1
  Say z
  End
Numeric Digits 20
z=999999998
Do i=1 To 3
  z=z+1
  Say z
  End
Numeric Digits 9
If left(v,11)='REXX-ooRexx' Then
  Signal On Lostdigits
z=100000000012
Say z
z=z+1
Say z
Exit
lostdigits:
  Say 'LOSTDIGITS condition raised in line' sigl
  Say 'sourceline='sourceline(sigl)
  Say "condition('D')="condition('D')
```

{{Out}}

```txt

REXX-ooRexx_4.1.2(MT)
999999999
1.00000000E+9
1.00000000E+9
999999999
1000000000
1000000001
100000000012
LOSTDIGITS condition raised in line 30
sourceline=z=z+1
condition('D')= 100000000012

```



## Ring


```ring

x = "1234"  See 1+x  # print 1235

```



## Ruby

If a string represents a number, the succ method will increment the number:

```ruby
'1234'.succ #=> '1235'
'99'.succ #=> '100'
```



## Rust


```rust
fn next_string(input: &str) -> String {
    (input.parse::<i64>().unwrap() + 1).to_string()
}

fn main() {
    let s = "-1";
    let s2 = next_string(s);
    println!("{:?}", s2);
}
```

{{out}}

```txt
"0"
```



## Run BASIC

Run BASIC has trim command for left and right

```runbasic
string$ = "12345"
numeric = val(string$)
numeric = numeric + 1
string$ = str$(numeric)
print string$

```


```txt
12346
```



## Scala

The string needs to be converted to a numeric type. <code>BigDecimal</code> should
handle most numeric strings. We define a method to do it.


```scala
implicit def toSucc(s: String) = new { def succ = BigDecimal(s) + 1 toString }
```


Usage:


```txt

scala> "123".succ
res5: String = 124

```



## Scheme


```scheme
(number->string (+ 1 (string->number "1234")))
```



## Sed


Reads a decimal integer from stdin and outputs the same with the magnitude incremented by one.

(TODO: Since it deals only with the magnitude, the result is incorrect for negative numbers—though adding this support is definitely possible.)

The routine starts by suffixing the input number with a carry mark (a <code>:</code> in this case) indicating that the digit to its left still needs to be incremented. In a loop, the following happens:

* If there is a carry mark on the far left, replace it with a 1.
* If there are no more carry marks, exit the loop.
* Hold the current number. (<code>h</code>)
* Extract the digit to the left of the first carry mark. (<code>s</code>)
* Replace the digit with the same digit incremented by one, with 9 incrementing to a carry mark (i.e. 10). (<code>y</code>)
* If the result of such replacement was a carry mark, suffix the mark with a 0, indicating that the digit has rolled over and the digit to the left must be incremented. (<code>s</code>)
* Retrieve the held number (<code>G</code>) and replace the first carry mark and the digit to its left with the result of the computation. (<code>s</code>)
* Repeat. (<code>b</code>)


```sed
s/^.*$/&:/
:bubble
s/^:/1/
/.:/ {
    h
    s/^.*\(.\):.*$/\1/
    y/0123456789/123456789:/
    s/:/:0/
    G
    s/\(.*\)\n\(.*\).:\(.*\)$/\2\1\3/
    b bubble
}
```



## Seed7


```seed7
var string: s is "12345";

s := str(succ(integer parse s));
```



## SequenceL


```sequencel>import <Utilities/Conversion.sl
;

increment(input(1)) := intToString(stringToInt(input) + 1);
```



## Sidef


```ruby
say '1234'.inc;    #=> '1235'
say '99'.inc;      #=> '100'
```



## Slate


```slate
((Integer readFrom: '123') + 1) printString
```



## Smalltalk



```smalltalk
('123' asInteger + 1) printString
```

(a note to non-smalltalkers: "printString" does not print, but return the "printString")


## SNOBOL4


```snobol4

     output = trim(input) + 1
     output = "123" + 1
end
```



```txt

Input:
 123

Output:
 124
 124

```



## Sparkling


```sparkling
function numStrIncmt(s) {
    return fmtstr("%d", toint(s) + 1);
}

spn:1> numStrIncmt("12345")
= 12346
```



## SuperTalk


```supertalk
put 0 into someVar
add 1 to someVar
-- without "into [field reference]" the value will appear
-- in the message box
put someVar -- into cd fld 1
```



## Swift

{{works with|Swift|2.x+}}

```swift
let s = "1234"
if let x = Int(s) {
  print("\(x + 1)")
}
```

{{works with|Swift|1.x}}

```swift
let s = "1234"
if let x = s.toInt() {
  println("\(x + 1)")
}
```



## Tcl

In the end, all variables are strings in Tcl. A "number" is merely a particular interpretation of a string of bytes.

```tcl
set str 1234
incr str
```


=={{header|TI-89 BASIC}}==


```ti89b
string(expr(str)+1)
```


=={{header|TI-83 BASIC}}==
There is no single command to convert a number to a string; you have to store it to one of the Function variables which acts as both a number and a string.

```ti83b
:"1"→Str1
:expr(Str1)+1→A
:{0,1}→L₁
:{0,A}→L₂
:LinReg(ax+b) Y₁
:Equ►String(Y₁,Str1)
:sub(Str1,1,length(Str1)-3)→Str1
```



## Toka


```toka
" 100" >number drop 1 + >string
```



## TorqueScript

To increment by 1:
  $string = "12345";
  $string++;
$string is now 12346.

To increment by more than 1:
  $string = "12345";
  $string += 10;
$string is now 12355.


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
teststring="0'1'-1'12345'10000000'-10000000"
LOOP/CLEAR n=teststring
n=n+1
PRINT n
ENDLOOP

```

{{Out}}

```txt

1
2
0
12346
10000001
-9999999

```



## TXR


Two implementations of what the task says: incrementing a numerical string. (Not: converting a string to a number, then incrementing the number, then converting back to string.)


### = TXR Lisp =



```txr
@(do (defun inc-num-str (str-in)
       (let ((len (length str-in))
             (str (copy-str str-in)))
         (for ((i (- len 1)))
              ((>= i 0) `1@str`)
              ((dec i))
           (if (<= (inc [str i]) #\9)
             (return str)
             (set [str i] #\0))))))
@(bind a @(inc-num-str "9999"))
@(bind b @(inc-num-str "1234"))
```



```txt
$ ./txr -B incnum.txr
a="10000"
b="1235"
```



### = No TXR Lisp =



```txr
@(deffilter incdig ("0" "1") ("1" "2") ("2" "3") ("3" "4") ("4" "5")
                   ("5" "6") ("6" "7") ("7" "8") ("8" "9"))
@(define increment (num out))
@  (local prefix dig junk)
@  (next :string num)
@  (cases)
9
@    (bind out "10")
@  (or)
@*{prefix}@{dig /[0-8]/}
@    (bind out `@prefix@{dig :filter incdig}`)
@  (or)
@*{prefix}9
@    (bind out `@{prefix :filter (:fun increment)}0`)
@  (or)
@junk
@    (throw error `bad input: @junk`)
@  (end)
@(end)
@in
@(increment in out)
```



```txt
$ echo 1 | ./txr -B incnum.txr -
input="1"
result="2"
$ echo 123 | ./txr -B incnum.txr -
input="123"
result="124"
$ echo 899999 | ./txr -B incnum.txr -
input="899999"
result="900000"
$ echo 999998 | ./txr -B incnum.txr -
input="999998"
result="999999"
$ echo 999999 | ./txr -B incnum.txr -
input="999999"
result="1000000"
```



## UNIX Shell

Traditional Unix shell does not directly support arithmetic operations, so external tools, such as expr are used to perform arithmetic calculations when required. The following example demonstrates how a variable can be incremented by using the expr function:

{{works with|Bourne Shell}}

```bash
# All variables are strings within the shell
# Although num look like a number, it is in fact a numerical string
num=5
num=`expr $num + 1`    # Increment the number
```


The [[Korn Shell]] and some newer shells do support arithmetic operations directly, and several syntax options are available:

{{works with|bash}}
{{works with|ksh93}}
{{works with|pdksh}}
{{works with|zsh}}

```bash
num=5
let num=num+1          # Increment the number
let "num = num + 1"    # Increment again. (We can use spaces inside quotes)
((num = num + 1))      # This time we use doublebrackets
let num+=1             # This time we use +=
let "num += 1"
((num += 1))
```


{{works with|ksh93}}
{{works with|pdksh}}
{{works with|zsh}}

```bash
integer num=5          # Declare an integer...
num=$num+1             # ...then increment without the let keyword.
```


=
## C Shell
=
The <code>@</code> assignment command uses strings as integers.

```csh
@ num = 5
@ num += 1
```



## Ursa


```ursa
decl string num
set num "123"
set num (int (+ (int num) 1))
```



## Ursala



```Ursala
#import nat

instring = ~&h+ %nP+ successor+ %np@iNC      # convert, do the math, convert back
```

test program:

```Ursala
#cast %sL

tests = instring* <'22435','4','125','77','325'>
```

{{Out}}

```txt

<'22436','5','126','78','326'>

```



## VBA

The easy method assumes that the number can be represented as a Long integer:

```VBA

Public Function incr(astring As String) As String
'simple function to increment a number string
   incr = CStr(CLng(astring) + 1)
End Function

```

Examples:

```txt

print incr("345343434")
345343435
print incr("-10000000")
-9999999

```


The long version handles arbitrary-length strings:

```VBA

Public Function Lincr(astring As String) As String
'increment a number string, of whatever length
'calls function "increment" or "decrement"
Dim result As String

'see if it is a negative number
If left$(astring, 1) = "-" Then
  'negative x: decrease |x| by 1, then add "-"
  '(except if the result is zero)
  result = decrement(Mid$(astring, 2))
  If result <> "0" Then result = "-" & result
Else
  '0 or positive x: increase x by 1
  If left$(astring, 1) = "+" Then  'allow a + before the number
    result = increment(Mid$(astring, 2))
  Else
    result = increment(astring)
  End If
End If
Lincr = result
End Function

Public Function increment(astring) As String
Dim result As String
'increment a string representing a positive number
'does not work with negative numbers
carry = 1
L = Len(astring)
result = ""
For j = L To 1 Step -1
  digit = Val(Mid$(astring, j, 1)) + carry
  If digit > 9 Then
    digit = digit - 10
    carry = 1
  Else
    carry = 0
  End If
  result = CStr(digit) & result
Next
If carry = 1 Then result = CStr(carry) & result
increment = result
End Function

Public Function decrement(astring) As String
Dim result As String
'decrement a string representing a positive number
'does not work with zero or negative numbers
borrow = 1
L = Len(astring)
result = ""
For j = L To 1 Step -1
  digit = Val(Mid$(astring, j, 1)) - borrow
  If digit < 0 Then
    digit = digit + 10
    borrow = 1
  Else
    borrow = 0
  End If
  result = CStr(digit) & result
Next
'remove leading zero, if necessary
If (Len(result) > 1) And (left$(result, 1) = "0") Then result = Mid$(result, 2)
decrement = result
End Function

```

Examples:

```txt

print Lincr("99999999999999999")
100000000000000000
print Lincr("-10000000000000000")
-9999999999999999
print Lincr("-1")
0
print Lincr("0")
1
print Lincr("+1234567890987654321009")
1234567890987654321010


```



## Vedit macro language

This example increments numeric string by converting it into numeric value, as most other language examples do.
The string is located in text register 10.

```vedit
itoa(atoi(10)+1, 10)
```


The following example increments unsigned numeric string of unlimited length.
The current line in the edit buffer contains the string.

```vedit
EOL
do {
    if (At_BOL) {
	Ins_Char('1')		// add new digit
	Break
    }
    Char(-1)
    #1 = Cur_Char+1		// digit
    #2 = 0			// carry bit
    if (#1 > '9') {
	#1 = '0'
	#2 = 1
    }
    Ins_Char(#1, OVERWRITE)
    Char(-1)
} while (#2)			// repeat until no carry
```



## Visual Basic .NET


```vbnet
    Dim s As String = "123"

    s = CStr(CInt("123") + 1)
    ' or
    s = (CInt("123") + 1).ToString
```



## XLISP


```lisp
(DEFUN INCREMENT-STRING (X)
    (NUMBER->STRING (+ (STRING->NUMBER X) 1)))
```



## XPL0


```XPL0
string 0;               \use zero-terminated string convention
code Text=12;

func StrLen(A);         \Return number of characters in an ASCIIZ string
char A;
int  I;
for I:= 0 to -1>>1-1 do
        if A(I) = 0 then return I;

proc IncStr(S);         \Increment a numeric string
char S;
int  I;
[for I:= StrLen(S)-1 downto 0 do
        [S(I):= S(I)+1;
        if S(I) > ^9 then S(I):= S(I)-10 else return;
        ];
];

char Str;
[Str:= "0123999999999"; \MSD first (big endian)
IncStr(Str);  IncStr(Str);
Text(0, Str);
]
```


{{out}}

```txt

0124000000001

```



## zkl

In zkl, the first operand "wins" and transforms the second. So 5+"1"-->6

```zkl
fcn numStringPlusOne(s){1+s}
numStringPlusOne("123") //-->124
```

