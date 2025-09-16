+++
title = "Array length"
description = ""
date = 2019-10-17T04:16:06Z
aliases = []
[extra]
id = 19599
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "8th",
  "abap",
  "ada",
  "algol_68",
  "antlang",
  "apex",
  "apl",
  "applescript",
  "arm_assembly",
  "arturo",
  "ats",
  "autohotkey",
  "autoit",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "brat",
  "c",
  "ceylon",
  "clipper_xbasepp",
  "clojure",
  "cobol",
  "coldfusion",
  "commodore_basic",
  "common_lisp",
  "component_pascal",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "dart",
  "dataweave",
  "dragon",
  "dwscript",
  "dyalect",
  "easylang",
  "echolisp",
  "ela",
  "elena",
  "elixir",
  "elm",
  "emacs_lisp",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "furryscript",
  "futhark",
  "futurebasic",
  "gambas",
  "genie",
  "go",
  "groovy",
  "haskell",
  "hexiscript",
  "i",
  "idris",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "klong",
  "kotlin",
  "latitude",
  "liberty_basic",
  "lil",
  "limbo",
  "lingo",
  "little",
  "livecode",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "min",
  "miniscript",
  "nanoquery",
  "neko",
  "newlisp",
  "ngs",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "onyx",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pl_i",
  "plorth",
  "pony",
  "potion",
  "powershell",
  "processing",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "red",
  "rexx",
  "ring",
  "robotic",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "setl",
  "sidef",
  "simula",
  "smalltalk",
  "snobol4",
  "spl",
  "sql",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "unix_shell",
  "ursa",
  "vba",
  "vbscript",
  "visual_basic",
  "visual_basic_.net",
  "wdte",
  "wren",
  "xlisp",
  "zkl",
  "zonnon",
]
+++

## Task

Determine the amount of elements in an array.


As an example use an array holding the strings 'apple' and 'orange'.


## Related tasks

*   [[String length]]





## 11l


```11l
print([‘apple’, ‘orange’].len)
```

```txt

2

```



## 360 Assembly

Array length is computed at compilation time with the formula : (AEND-A)/L'A

```360asm
*        Array length              22/02/2017
ARRAYLEN START
         USING ARRAYLEN,12
         LR    12,15               end of prolog
         LA    1,(AEND-A)/L'A      hbound(a)
         XDECO 1,PG+13             edit
         XPRNT PG,L'PG             print
         BR    14                  exit
A        DC    CL6'apple',CL6'orange'   array
AEND     DC    0C
PG       DC    CL25'Array length='      buffer
         END   ARRAYLEN
```

```txt

Array length=           2

```



## 8th


```forth

["apples", "oranges"] a:len . cr

```

```txt

2

```



## ABAP

The concept of arrays does not exist in ABAP, instead internal tables are used. Since ABAP Version 7.40 they can be accessed with the common index notation. Note that the index starts at 1 and out of bound access raises an exception. The built-in function "lines" returns the number of records.


```ABAP

report z_array_length.

data(internal_table) = value stringtab( ( `apple` ) ( `orange` ) ).

write: internal_table[ 1 ] , internal_table[ 2 ] , lines( internal_table ).

```


```txt

apple orange          2

```



## Ada



```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Array_Length is
   Fruits : constant array (Positive range <>) of access constant String
      := (new String'("orange"),
          new String'("apple"));

begin
   for Fruit of Fruits loop
      Ada.Text_IO.Put (Integer'Image (Fruit'Length));
   end loop;

   Ada.Text_IO.Put_Line ("  Array Size : " & Integer'Image (Fruits'Length));
end Array_Length;
```


```txt
  6 5  Array Size: 2
```



## ALGOL 68


```algol68
# UPB returns the upper bound of an array, LWB the lower bound #
[]STRING fruits = ( "apple", "orange" );
print( ( ( UPB fruits - LWB fruits ) + 1, newline ) ) # prints 2 #
```



## AntLang


```AntLang
array: seq["apple"; "orange"]
length[array]
/Works as a one liner: length[seq["apple"; "orange"]]
```



## Apex


```apex
System.debug(new String[] { 'apple', 'banana' }.size()); // Prints 2
```



## APL


```apl
⍴'apple' 'orange'
```

Output:

```txt
2
```



## AppleScript


```AppleScript

set theList to {"apple", "orange"}
count theList
-- or
length of theList
-- or
number of items in theList

```

```txt
2
```


No context or goal is provided for this task – sometimes for example, we may simply want to take the last member of an array, and counting the length to derive an index might well not be the best route.

More generally, we may learn more about AppleScript by defining '''length()''' ourselves. There are two basic functional approaches to doing that – we can write a simple recursive definition, or, if we have a higher order '''fold'''/'''reduce''' function (see [[Catamorphism]]) we can derive length() as:


```txt
fold (λx n -> 1 + n)  0
```



```AppleScript
on run

    set xs to ["alpha", "beta", "gamma", "delta", "epsilon", ¬
        "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu"]

    {_length(xs), fold(xs, succ, 0), item 12 of xs, item -1 of xs}

    --> {12, 12, "mu", "mu"}

end run

-- TWO FUNCTIONAL DEFINITIONS OF LENGTH

-- 1. Recursive definition

on _length(xs)
    if xs is [] then
        0
    else
        1 + _length(rest of xs)
    end if
end _length


-- 2. fold (λx n -> 1 + n)  0

on succ(x)
    1 + x
end succ

--[a] - > (a - > b) - > b - > [b]
on fold(xs, f, startValue)
    script mf
        property lambda : f
    end script

    set v to startValue
    repeat with x in xs
        set v to mf's lambda(v, x)
    end repeat
end fold

```


```txt
{12, 12, "mu", "mu"}
```



## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program lenAreaString.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessLenArea: .ascii "The length of area is : "
sZoneconv:		 .fill 12,1,' '
szCarriageReturn:  .asciz "\n"

/* areas strings  */
szString1:  .asciz "Apples"
szString2:  .asciz "Oranges"
/* pointer items area */
tablesPoi:
ptApples:		.int szString1
ptOranges:   .int szString2
ptVoid:		.int 0

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    ldr r1,iAdrtablesPoi  @ begin pointer table
    mov r0,#0    @ counter
1:              @ begin loop
    ldr r2,[r1,r0,lsl #2]    @ read string pointer address item r0 (4 bytes by pointer)
    cmp r2,#0                @ is null ?
    addne r0,#1             @ no increment counter
    bne 1b                  @ and loop

    ldr r1,iAdrsZoneconv   @ conversion decimal
    bl conversion10S
    ldr r0,iAdrszMessLenArea
    bl affichageMess

2:

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrtablesPoi:		.int tablesPoi
iAdrszMessLenArea:  .int szMessLenArea
iAdrsZoneconv:		.int  sZoneconv
iAdrszCarriageReturn:  .int  szCarriageReturn
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



## Arturo



```arturo
fruit #("apple" "orange")

print "array length = " + $(size fruit)
```


```txt
array length = 2
```



## ATS


```ATS

#include
"share/atspre_staload.hats"
#include
"share/atspre_staload_libats_ML.hats"

val A0 =
array0_tuple<string>
( "apple", "orange" )
val () =
println!("length(A0) = ", length(A0))

implement main0((*void*)) = ((*void*))

```



## AutoHotkey


```AutoHotkey
MsgBox % ["apple","orange"].MaxIndex()
```

```txt
2
```



## AutoIt


```AutoIt
Opt('MustDeclareVars',1)	 	; 1 = Variables must be pre-declared.

Local $aArray[2] = ["Apple", "Orange"]
Local $Max = UBound($aArray)
ConsoleWrite("Elements in array: " & $Max & @CRLF)

For $i = 0 To $Max - 1
	ConsoleWrite("aArray[" & $i & "] = '" & $aArray[$i] & "'" & @CRLF)
Next

```

```txt
Elements in array: 2
aArray[0] = 'Apple'
aArray[1] = 'Orange'
```



## AWK

<!-- http://ideone.com/qhJQ6g -->

The main use of the length()-function is to determine the length of a string.

When used on an array, it returns the number of elements.

Another method to count the elements of the array is by using a variant of for().


```awk
# usage:  awk -f arraylen.awk
#
function countElements(array) {
  for( e in array ) {c++}
  return c
}

BEGIN {
  array[1] = "apple"
  array[2] = "orange"

  print "Array length :", length(array), countElements(array)

  print "String length:", array[1], length(array[1])
}
```


```txt

Array length : 2 2
String length: apple 5

```



## BASIC


```basic
DIM X$(1 TO 2)
X$(1) = "apple"
X$(2) = "orange"
PRINT UBOUND(X$) - LBOUND(X$) + 1
```


=
## Commodore BASIC
=

```basic
10 DIM A$(2)
20 A$(1) = "ORANGE"
30 A$(2) = "APPLE"
40 PRINT LEN(A$(1))+LEN(A$(2))
```



## Batch File

While batch files don't support arrays in the traditional sense, sets of variables forming somewhat of a pseudo-array are extremely useful. They are usually in the form of <code>%name{number}%</code>. The below code gives an example of how to create an array from a list stored in a variable, and how to acquire the amount of entries in the array.


```dos

@echo off

:_main
setlocal enabledelayedexpansion

:: This block of code is putting a list delimitered by spaces into an pseudo-array
:: In practice, this could be its own function _createArray however for the demonstration, it is built in
set colour_list=red yellow blue orange green
set array_entry=0
for %%i in (%colour_list%) do (
  set /a array_entry+=1
  set colours[!array_entry!]=%%i
)

call:_arrayLength colours
echo _arrayLength returned %errorlevel%
pause>nul
exit /b

:: _arrayLength returns the length of the array parsed to it in the errorcode
:_arrayLength
setlocal enabledelayedexpansion

:loop
set /a arrayentry=%arraylength%+1
if "!%1[%arrayentry%]!"=="" exit /b %arraylength%
set /a arraylength+=1
goto loop

```

```txt

red yellow blue orange green

```

```txt

_arrayLength returned 5

```



## BBC BASIC

```bbcbasic
      DIM array$(1)
      array$() = "apple", "orange"
      PRINT "Number of elements in array = "; DIM(array$(), 1) + 1
      PRINT "Number of bytes in all elements combined = "; SUMLEN(array$())
      END
```

```txt
Number of elements in array = 2
Number of bytes in all elements combined = 11
```



## Brat


```brat

p ["apple", "orange"].length

```



## C


C features two kinds of arrays: static (compile-time, fixed size) and dynamic (allocated at runtime).

The length of a dynamic array cannot be acquired from the array itself - its length must be stored elsewhere.

For static arrays:


```c

#include <stdio.h>

int main()
{
    const char *fruit[2] = { "apples", "oranges" };

    // Acquire the length of the array by dividing the size of all elements (found
    // with sizeof(fruit)) by the size of the first element.

    // Note that since the array elements are pointers to null-terminated character
    // arrays, the size of the first element is actually the size of the pointer
    // type - not the length of the string.

    // This size, regardless of the type being pointed to, is 8 bytes, 4 bytes, or
    // 2 bytes on 64-bit, 32-bit, or 16-bit platforms respectively.
    int length = sizeof(fruit) / sizeof(fruit[0]);

    printf("%d\n", length);

    return 0;
}

```


A C pre-processor macro may be created for ease of use:


```c

#define ARRAY_LENGTH(A) (sizeof(A) / sizeof(A[0]))

```


Note that these arrays become pointers when passed as a parameter to a function.
Thus, the length of an array parameter may not be required directly - a dedicated length parameter would be required.

## C#


```c#

using System;

class Program
{
    public static void Main()
    {
        var fruit = new[] { "apple", "orange" };
        Console.WriteLine(fruit.Length);
    }
}

```


Note that any of the following array declarations could be used:


```c#

var fruit = new[] { "apple", "orange" };
var fruit = new string[] { "apple", "orange" };
string[] fruit = new[] { "apple", "orange" };
string[] fruit = new string[] { "apple", "orange" };
string[] fruit = { "apple", "orange" };

```


A shorter variant could also have been used:


```c#

using static System.Console;

class Program
{
    public static void Main()
    {
        WriteLine(new[] { "apples", "oranges" }.Length);
    }
}

```



## C++


C++ follows the same rules as C regarding static and dynamic arrays.

However, C++ has an additional std::array type (amongst other collections) in its standard library:


```cpp

#include <array>
#include <iostream>
#include <string>

int main()
{
    std::array<std::string, 2> fruit { "apples", "oranges" };
    std::cout << fruit.size();
    return 0;
}

```


Note that char* or const char* could have been used instead of std::string.

In addition to the std::array type, the C++ standard library also provides dynamically-sized containers to hold arbitrary objects.
These all support similar interfaces, though their implementations have different performance characteristics.

```cpp

    std::vector<std::string> fruitV({ "apples", "oranges" });
    std::list<std::string> fruitL({ "apples", "oranges" });
    std::deque<std::string> fruitD({ "apples", "oranges" });
    std::cout << fruitV.size() << fruitL.size() << fruitD.size() << std::endl;

```


Of these, vector is probably the most widely used.


## Ceylon


```ceylon
shared void run() {
	value array = ["apple", "orange"];
	print(array.size);
}
```



## Clipper/XBase++



```Clipper/XBase++
/*
 * nizchka: March - 2016
 * This is a Clipper/XBase++ of RosettaCode Array_Length
 */

PROCEDURE MAIN()
        LOCAL FRUIT := { "apples","oranges" }

        ? LEN(FRUIT)
RETURN

```

Outputs:
```txt
2
```

[[User:nizchka|nizchka]] 23:27, 16 March 2016 (UTC)


## Clojure



```clojure
; using count:
(count ["apple" "orange"])

; OR alength if using Java arrays:
(alength (into-array ["apple" "orange"]))
```



## COBOL

Arrays in COBOL are usually referred to as tables.  Tables can have fixed or variable (with known maximum) allocations, using a syntax of OCCURS DEPENDING ON.  The value of the ODO identifier is the number of active elements in the table.


```COBOL
       identification division.
       program-id. array-length.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 table-one.
          05 str-field pic x(7) occurs 0 to 5 depending on t1.

       77 t1           pic 99.

       procedure division.
       array-length-main.
       perform initialize-table
       perform display-table-info
       goback.

       initialize-table.
           move 1 to t1
           move "apples" to str-field(t1)

           add 1 to t1
           move "oranges" to str-field(t1).

      *> add an extra element and then retract table size
           add 1 to t1
           move "bananas" to str-field(t1).
           subtract 1 from t1
       .

       display-table-info.
           display "Elements: " t1 ", using " length(table-one) " bytes"
           display table-one
       .

       end program array-length.
```

```txt
$ cobc -xjd array-length.cob
Elements: 02, using 000000014 bytes
apples oranges

```



## ColdFusion



```coldfusion

<cfset testArray = ["apple","orange"]>
<cfoutput>Array Length = #ArrayLen(testArray)#</cfoutput>

```

Outputs:
```txt
Array Length = 2
```

[[User:grandnegus|Mike Knapp]] 15:57, 26 May 2016 (UTC)



## Common Lisp


```Lisp

(print (length #("apple" "orange")))

```


### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : Array length

(setf my-array (make-array '(2)))
(setf (aref my-array 0) "apple")
(setf (aref my-array 1) "orange")
(format t "~a" "length of my-array: ")
(length my-array)
(terpri)

```

Output:

```txt

length of my-array: 2

```



## Component Pascal

```oberon2

MODULE AryLen;
IMPORT StdLog;

TYPE
	String = POINTER TO ARRAY OF CHAR;
VAR
	a: ARRAY 16 OF String;

PROCEDURE NewString(s: ARRAY OF CHAR): String;
VAR
	str: String;
BEGIN
	NEW(str,LEN(s$) + 1);str^ := s$; RETURN str
END NewString;

PROCEDURE Length(a: ARRAY OF String): INTEGER;
VAR
	i: INTEGER;
BEGIN
	i := 0;
	WHILE a[i] # NIL DO INC(i) END;
	RETURN i
END Length;

PROCEDURE Do*;
BEGIN
	a[0] := NewString("Apple");
	a[1] := NewString("Orange");
	StdLog.String("Length:> ");StdLog.Int(Length(a));StdLog.Ln
END Do;

END AryLen.

```

Execute: ^Q AryLen.Do
```txt

Length:>  2

```



## Crystal



```ruby

puts ["apple", "orange"].size

```


```txt

2

```



## D


```d

import std.stdio;

int main()
{
    auto fruit = ["apple", "orange"];
    fruit.length.writeln;
    return 0;
}

```


Or a somewhat shorter...

```d

import std.stdio;

void main()
{
    ["apple", "orange"].length.writeln;
}

```



## Dart


```dart

arrLength(arr) {
  return arr.length;
}

main() {
  var fruits = ['apple', 'orange'];
  print(arrLength(fruits));
}


```



## DataWeave


```DataWeave
var arr = ["apple", "orange"]
sizeOf(arr)

```



## Dragon


```dragon
select "std"

a = ["apple","orange"]
b = length(a)

show b

```



## DWScript


```delphi

var a := ['apple', 'orange'];
PrintLn(a.Length);

```



## Dyalect



```dyalect
var xs = ["apple", "orange"]
print(xs.len())
```



## EasyLang

<lang>fruit$[] = [ "apples" "oranges" ]
print len fruit$[]
```



## EchoLisp


```scheme

(length '("apple" "orange")) ;; list
   → 2
(vector-length #("apple" "orange")) ;; vector
   → 2

```



## Ela


```ela
length [1..10]
```



## Elena

ELENA 4.1 :

```elena

var array := new::("apple", "orange");
var length := array.Length;

```



## Elixir


```elixir
iex(1)> length( ["apple", "orange"] )          # List
2
iex(2)> tuple_size( {"apple", "orange"} )      # Tuple
2
```



## Elm


```elm

import Array
import Html

main : Html.Html
main =
    ["apple", "orange"]
      |> Array.fromList
      |> Array.length
      |> Basics.toString
      |> Html.text

```



## Emacs Lisp


```Lisp
(length ["apple" "orange"])
=> 2
```


<code>length</code> also accepts a list or a string.


## Erlang


```Erlang

1> length(["apple", "orange"]).     %using a list
2
1> tuple_size({"apple", "orange"}). %using a tuple
2

```


=={{header|F_Sharp|F#}}==

```fsharp
[|1;2;3|].Length |> printfn "%i"
```

Or:

```fsharp
[|1;2;3|] |> Array.length |> printfn "%i"
```



## Factor


```factor

{ "apple" "orange" } length

```



## Forth

The philosophy of Chuck Moore, the creator of Forth was that he did not want to write code for something he may never use. His solution was to distill his language into a large set of very simple routines that control the hardware directly.
This demonstration must build "arrays" from scratch. In Forth, like in Assembler, you can do this any way you want. This demonstration adds new words to Forth that make a syntax to create simple variable length string arrays. Each string is a counted string with no trailing zero.

The code is commented to explain what is going on for those unfamiliar with Forth.

<lang>: STRING,  ( caddr len -- ) \ Allocate space & compile string into memory
             HERE  OVER CHAR+  ALLOT  PLACE ;

: "     ( -- ) [CHAR] " PARSE  STRING, ; \ Parse input to " and compile to memory

\ Array delimiter words
: {  ALIGN 0 C, ;               \ Compile 0 byte start/end of array
: }  ALIGN 0 C,  ;

\ String array words
: {NEXT}    ( str -- next_str)       \ Iterate to next string
           COUNT + ;

: {NTH}    ( n array_addr -- str)   \ Returns address of the Nth item in the array
           SWAP 0 DO {NEXT} LOOP ;

: {LEN} ( array_addr -- n)  \ count strings in the array
          0 >R                      \ Counter on Rstack
          {NEXT}
          BEGIN
             DUP C@                 \ Fetch length byte
          WHILE                     \ While true
             R> 1+ >R               \ Inc. counter
             {NEXT}
          REPEAT
          DROP
          R> ;      \ return counter to data stack
```

Test code at Forth console

```forth
CREATE Q { " Apples" " Oranges" }   q {len} . 2  ok
```



## Fortran

Early fortrans offered no protocol for ascertaining the length (or dimensionality) of arrays, though the compiler has this information. Thus a statement such as <code>PRINT A</code> would print all the elements of a variable <code>A</code> according to its definition. A subprogram that received a parameter would have no access to these details, so its parameter might be declared as <code>A(12345)</code> simply to signify that it was an array (rather than an ordinary variable) and the programmer would rely on other data to know the upper bound to employ, for instance via an additional parameter. ''Any mistakes would cause crashes!'' On the other hand, with heavy computational tasks, it was common to take advantage of the opportunities. Thus, a subprogram might regard its array parameter as one-dimensional even though the actual parameter was not. Carefully-programmed routines might thusly process a sequence of elements via 1-D indexing, far faster than the 2-D or higher order indexing of the original. Success at this game required understanding how array elements were arranged in multidimensional arrays.

Later fortrans allowed <code>A(*)</code> to signify an array parameter of unstated upper bound, but there was still a problem with higher dimensions. All but the last dimension has to be stated correctly if a multi-dimensional array parameter is to be indexed correctly - Fortran stores array elements in [[Array#Fortran|column-major]] order.

With Fortran 90, a new protocol was introduced, whereby the parameter might be declared as <code>A(:)</code> signifying an array of one dimension, of bounds unstated. A 2-D array would have <code>A(:,:)</code> and so on. Further, arrays could have arbitrary lower bounds as well, as in <code>A(-7:12)</code> but if no colon appeared for a dimension, the lower bound would be assumed to be one so <code>A(2)</code> means an array of two elements, as before. And as before, in a subprogram a bound could be explicitly stated, perhaps via an explicit parameter such as <code>N</code>, but now with the <code>:</code> scheme, the compiler is passing secret additional parameters to the subprogram giving the bounds of the array, and these can be accessed via the library functions LBOUND and UBOUND. For multi-dimensional arrays there are multiple bounds, and an invocation might be <code>UBOUND(A,DIM = 2)</code> but in the example only a one-dimensional array is involved. These facilities are available only if the new MODULE protocol is employed.

The task is in terms of an array holding the texts "Apple" and "Orange", so a CHARACTER*6 element size will do; the subprogram receives yet another secret parameter specifying the size of CHARACTER parameters. This size can be accessed via the LEN function, and, since in principle the index span is arbitrary, no constant index is sure to be a valid index of some single element: thus the LBOUND function is used to finger one that is.

For a simple example, the WRITE(6,*) suffices: write to standard output (the 6), in free-format (the *).


```Fortran

      MODULE EXAMPLE
       CONTAINS
        SUBROUTINE ABOUND(A)
         CHARACTER*(*) A(:)	!One dimensional array, unspecified bounds.
          WRITE (6,*) "Lower bound",LBOUND(A),", Upper bound",UBOUND(A)
          WRITE (6,*) "Element size",LEN(A(LBOUND(A)))
          WRITE (6,*) A
        END SUBROUTINE ABOUND
      END MODULE EXAMPLE

      PROGRAM SHOWBOUNDS
       USE EXAMPLE
       CHARACTER*6 ARRAY(-1:1)
        ARRAY(-1) = "Apple"
        ARRAY(0) = "Orange"
        ARRAY(1) = ""
        CALL ABOUND(ARRAY)
        WRITE (6,*) "But, when it is at home..."
        WRITE (6,*) "L. bound",LBOUND(ARRAY),", U. bound",UBOUND(ARRAY)
      END

```


Output:
 Lower bound           1 , Upper bound           3
 Element size           6
 Apple Orange
 But, when it is at home...
 L. bound          -1 , U. bound           1

Notice that the subprogram sees the array as an old-style array starting with index one! If it is to work with a lower bound other than one, the declaration in the subprogram must state it, perhaps as A(-1:) or as A(START:), ''etc''. The upper bound remains unspecified, and with that declaration, UBOUND returns 1 instead of 3, corresponding to the shift. Thus, UBOUND returns not the actual upper bound of the array parameter (as supplied) but the upper bound relative to the lower bound in use ''in'' the subprogram so that UBOUND - LBOUND + 1 does give the number of elements.

If in the subprogram the bounds are fixed (say as <code>A(-1:6)</code>) then a CALL ABOUND(ARRAY) may provoke a compiler complaint if ARRAY is not a suitable size.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim fruit(1) As String = {"apple", "orange"}
Dim length As Integer = UBound(fruit) - LBound(fruit) + 1
Print "The length of the fruit array is"; length
Print
Print "Press any key to quit the program"
Sleep
```


```txt

The length of the fruit array is 2

```



## Frink


```frink

a = ["apple", "orange"]
println[length[a]]

```



## FurryScript


```furryscript
THE_LIST( <apple> <orange> )
COUNT[ 0 SW ~| COUNT_STEP# 0 SW SU ]
COUNT_STEP[ DR 1 SU ]

`THE_LIST COUNT# +<>
```



## Futhark

The <code>shape</code> builtin returns the shape of an array as an array of integers.  The length is element 0 of the shape:


```Futhark

fun length(as: []int): int = (shape as)[0]

```



## FutureBasic



```futurebasic

include "ConsoleWindow"

dim as CFArrayRef   array
dim as Str255       s
dim as CFStringRef  fruits, tempStr
dim as CFIndex ubound, i

fruits = @"apples,bananas,cherries,dates,grapes,lemon,lime,orange,peach,pear,pineapple,strawberries,watermelon"
array = fn CFStringCreateArrayBySeparatingStrings( _kCFAllocatorDefault, fruits, @"," )

ubound = fn CFArrayGetCount( array )
for i = 0 to ubound - 1
  tempStr = fn CFArrayGetValueAtIndex( array, i )
  fn CFStringGetPascalString( tempStr, @s, 256, _kCFStringEncodingMacRoman )
  CFRelease( tempStr )
  print "Fruit"; i; " = "; s
next

```

Output:

```txt

Fruit 0 = apples
Fruit 1 = bananas
Fruit 2 = cherries
Fruit 3 = dates
Fruit 4 = grapes
Fruit 5 = lemon
Fruit 6 = lime
Fruit 7 = orange
Fruit 8 = peach
Fruit 9 = pear
Fruit 10 = pineapple
Fruit 11 = strawberries
Fruit 12 = watermelon

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=2a452c807c7030eb64a2f1d60d31a830 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siList As Short[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

Print siList.Count

End
```

Output:

```txt

10

```



## Genie


```genie
[indent=4]
/* Array length, in Genie */
init
    arr:array of string = {"apple", "orange"}
    stdout.printf("%d ", arr.length)
    print arr[1]
```


```txt
prompt$ valac array_length.gs
prompt$ ./array_length
2 orange
```



## Go



```Go
package main

import "fmt"

func main() {
	arr := [...]string{"apple", "orange", "pear"}

	fmt.Printf("Length of %v is %v.\n", arr, len(arr))
}
```



## Groovy



```Groovy

def fruits = ['apple','orange']
println fruits.size()

```



## Haskell



```Haskell
-- [[Char]] -> Int
length ["apple", "orange"]
```



## hexiscript


```hexiscript
let a arr 2
let a[0] "apple"
let a[1] "orange"
println len a
```



## i


```i
main: print(#["apple", "orange"])
```



## Idris


```Idris
length ["apple", "orange"]
```



## J

Tally (<code>#</code>) returns the length of the leading dimension of an array (or 1 if the array has no dimensions). Shape Of (<code>$</code>) returns the length of each dimension of an array.

```j
   # 'apple';'orange'
2
   $ 'apple';'orange'
2
```

For the list array example given, the result appears to be the same. The difference is that the result of Tally is a scalar (array of 0 dimensions) whereas the result of Shape Of is a list (1 dimensional array), of length 1 in this case.

```j
   $#'apple';'orange'

   $$'apple';'orange'
1
```

This might be a clearer concept with a few more examples. Here's an array with two dimensions:

```j

'apple';'orange'
apple
orange
   $>'apple';'orange'
2 6
   #>'apple';'orange'
2
```

And, here's an array with no dimensions:
<lang>   9001
9001
   #9001
1
   $9001

```

You can count the number of dimensions of an array (the length of the list of lengths) using <code>#$array</code>:

```j

   #$9001
0
   #$'apple';'orange'
1
   #$>'apple';'orange'
2
```



## Java


```java
public class ArrayLength {
    public static void main(String[] args) {
        System.out.println(new String[]{"apple", "orange"}.length);
    }
}
```



## JavaScript



```javascript
console.log(['apple', 'orange'].length);
```


However, determining the length of a list, array, or collection may simply be the wrong thing to do.

If, for example, the actual task (undefined here, unfortunately) requires retrieving the final item, while it is perfectly possible to write '''last''' in terms of '''length'''


```JavaScript
function last(lst) {
    return lst[lst.length - 1];
}
```


using length has the disadvantage that it leaves ''last'' simply undefined for an empty list.

We might do better to drop the narrow focus on length, and instead use a fold (''reduce'', in JS terms) which can return a default value of some kind.


```JavaScript
function last(lst) {
    return lst.reduce(function (a, x) {
        return x;
    }, null);
}
```


Alternatively, rather than scanning the entire list to simply get the final value, it might sometimes be better to test the length:


```JavaScript
function last(list, defaultValue) {
   return list.length ?list[list.length-1] :defaultValue;
}
```


Or use other built-in functions – this, for example, seems fairly clear, and is already 100+ times faster than unoptimised tail recursion in ES5 (testing with a list of 1000 elements):


```JavaScript
function last(list, defaultValue) {
    return list.slice(-1)[0] || defaultValue;
}
```



## jq



```jq
["apple","orange"] | length
```

Output:

```sh
2
```


Note that the ''length'' filter is polymorphic, so for example the empty string (""), the empty list ([]), and ''null'' all have ''length'' 0.


## Jsish


```javascript
/* Array length, in jsish */
var arr = new Array('apple', 'orange');
puts(arr.length);
puts(arr[1]);
```


```txt
prompt$ jsish arrayLength.jsi
2
orange
```



## Julia



```Julia

a = ["apple","orange"]
length(a)

```



## Klong



```K

#["apple" "orange"]

```



## Kotlin



```scala
fun main(args: Array<String>) {
    println(arrayOf("apple", "orange").size)
}
```



## Latitude


In Latitude, <code>length</code> and <code>size</code> are synonymous and will both retrieve the size of a collection.


```latitude
println: ["apple", "orange"] length.
```



## Liberty BASIC


When a one or two dimensional array, A$, with subscript(s) of 10 or less is referenced (either by assigning or reading), the compiler does an implicit DIM A$(10) or DIM A$(10,10). Before referencing an array with any subscript numbered higher than 10, or arrays of three dimensions or more, the programmer must first issue an explicit DIM statement.

There is no function in Liberty Basic to directly read the size of an array. This program uses error trapping loops to, first, determine the number of dimensions of the array. Then, second, again uses error trapping loops to determine the number of elements in each dimension. Finally, it prints the DIM statement that was used to define the array.

I suppose the implicit DIM feature makes it a bit quicker to write short, simple programs. One or two dimension arrays may be resized with REDIM. Three dimension or more arrays can not be resized. All arrays may be cleared with REDIM. Keep in mind that A$(n) and A$(n,m) are the same array. You must refer to it with the correct arguments or get an error.

NOTE -- This program runs only under LB Booster version 3.05 or higher because of arrays with more than two dimensions, passed array names to functions and subroutines as a parameter, and structured error trapping syntax. Get the LBB compiler here: http://lbbooster.com/
```lb

FruitList$(0)="apple" 'assign 2 cells of a list array
FruitList$(1)="orange"
dimension=dimension(FruitList$()) 'first get the dimension of the array
if dimension>3 then
    print "Sorry, program only written for array dimensions of 3 or less."
    end
end if
call elements FruitList$(), dimension 'next get the size of each dimension
end

function dimension(array$())
    for dimension=1 to 4
        select case dimension
            case 1
                try: x$=array$(0)
                catch: goto [TryNext]
                end try
                exit for
            case 2
                try: x$=array$(0,0)
                catch: goto [TryNext]
                end try
                exit for
            case 3
                try: x$=array$(0,0,0)
                catch: goto [TryNext]
                end try
                exit for
            case 4
                exit for
        end select
    [TryNext]
    next dimension
    if dimension<4 then print "array dimension = "; dimension
    ArraySize(0)=dimension
end function

sub elements array$(), dimension
    select case dimension
        case 1
            try
                do
                    x$=array$(a)
                    a+=1
                loop
            catch: elements=a
            end try
            ArraySize(1)=elements-1
            print "dimension 1 has "; elements; " elements (cells), "_
                    "numbered 0 to "; ArraySize(1)
        case 2
            try
                do
                    x$=array$(a,0)
                    a+=1
                loop
            catch: elements=a
            end try
            ArraySize(1)=elements-1
            print "dimension 1 has "; elements; " elements (cells), "_
                    "numbered 0 to "; ArraySize(1)
            elements=0
            try
                do
                    x$=array$(0,b)
                    b+=1
                loop
            catch: elements=b
            end try            ArraySize(2)=elements-1
            print "dimension 2 has "; elements; " elements (cells), "_
                    "numbered 0 to "; ArraySize(2)
        case 3
            try
                do
                    x$=array$(a,0,0)
                    a+=1
                loop
            catch: elements=a
            end try
            ArraySize(1)=elements-1
            print "dimension 1 has "; elements; " elements (cells), "_
                    "numbered 0 to "; ArraySize(1)
            elements=0
            try
                do
                    x$=array$(0,b,0)
                    b+=1
                loop
            catch: elements=b
            end try
            ArraySize(2)=elements-1
            print "dimension 2 has "; elements; " elements (cells), "_
                    "numbered 0 to "; ArraySize(2)
            elements=0
            try
                do
                    x$=array$(0,0,c)
                    c+=1
                loop
            catch: elements=c
            end try
            ArraySize(3)=elements-1
            print "dimension 3 has "; elements; " elements (cells), "_
                    "numbered 0 to "; ArraySize(3)
    end select
   'print the explicit or implied DIMension statement for this array
    print "DIM array$("; a-1;
    if b>0 then print ","; b-1;
    if c>0 then print ","; c-1;
    print ")"
end sub

```

```txt

array dimension = 1
dimension 1 has 11 elements (cells), numbered 0 to 10
DIM array$(10)

```



## LIL

LIL does not use arrays, but indexed lists.  The builtin '''count''' command returns the item count in a list.  The '''length''' command returns the length of the list after string conversion.


```tcl
# Array length, in LIL
set a [list "apple"]
append a "orange"
print [count $a]
print [index $a 1]
```


```txt
prompt$ lil arrayLength.lil
2
orange
```



## Limbo


```Limbo
implement Command;

include "sys.m";
sys: Sys;

include "draw.m";

include "sh.m";

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;

	a := array[] of {"apple", "orange"};
	sys->print("length of a: %d\n", len a);
}
```



## Lingo


```lingo
fruits = ["apple", "orange"]
put fruits.count
-- 2
```



## Little


```C
string fruit[] = {"apples", "oranges"};
puts(length(fruit));
```



## LiveCode



```LiveCode
put "apple","orange" into fruit
split fruit using comma
answer the number of elements of fruit
```



## Lua


```Lua
-- For tables as simple arrays, use the # operator:
fruits = {"apple", "orange"}
print(#fruits)

-- Note the # symbol does not work for non-integer-indexed tables:
fruits = {fruit1 = "apple", fruit2 = "orange"}
print(#fruits)

-- For this you can use this short function:
function size (tab)
  local count = 0
  for k, v in pairs(tab) do
    count = count + 1
  end
  return count
end

print(size(fruits))
```

```txt

2
0
2

```


## M2000 Interpreter


```M2000 Interpreter

\\ A is a pointer to array
A=("Apple", "Orange")
Print  Len(A)=2  ' True
Print Dimension(A, 0)  ' LBound (0 or 1), here 0
Print Dimension(A)  ' No of Dimensions 1
Print Dimension(A, 1) ' for 1 dimension array this is also Length=2
\\ A$( ) is an Array (not a pointer to array)
Dim Base 1, A$(2)
A$(1)="Apple", "Orange"
Print Dimension(A$(), 0)  ' LBound (0 or 1), here 1
Print Dimension(A$())  ' No of Dimensions 1
Print Dimension(A$(), 1) ' for 1 dimension array this is also Length=2
Link A to B$()  ' B$() is a reference to A
Print B$(0)=A$(1)
Print B$(1)=A$(2)
Dim C$()
\\ C$() get a copy of B$()
C$()=B$()
Print C$()  ' prints Apple Orange
\\ An array can link to a new name as reference, and can change major type
\\ here A$() get A() so we can read/store numbers and read/store strings in same array
\\ using two names
Link A$() to A()
\\ An array pointer can point to another array
A=A()
Print Dimension(A, 0)  ' LBound (0 or 1), here 1 (was 0)
\\ Because B$() is reference of A:
Print Dimension(B$(), 0)  ' LBound (0 or 1), here 1 (was 0)
Print B$(1)=A$(1)
Print B$(2)=A$(2)
Print Dimension(C$(), 0)  ' LBound (0 or 1), here 0
\\ change base preserve items
Dim Base 1, C$(Dimension(C$(), 1))
Print Dimension(C$(), 0)  ' LBound (0 or 1), here 1 (was 0)
Print C$()  ' prints Apple Orange
Print Len(C$()) ' Len return all items of an array - can be 0
Dim K(1,1,1,1,1,1)  ' Maximum 10 dimensions
Print Len(K()=1 ' True

```




## Maple


```maple
a := Array(["apple", "orange"]);
numelems(a);
```

```txt

a := [ "apple" "orange" ]
2

```



## Mathematica


```Mathematica
Length[{"apple", "orange"}]
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
length({'apple', 'orange'})
```

For arrays with more than one dimension, length reports the length of the larges dimension. The number of elements in a multi-dimensional array can be obtained with numel.

```Matlab
numel({'apple', 'orange'; 'pear', 'banana'})
```



## min

```min
("apple" "orange") size print
```

```txt

2

```



## MiniScript


```MiniScript

fruits = ["apple", "orange"]
print fruits.len

```



## Nanoquery


```nanoquery
$fruit = array(2)
$fruit[0] = "apple"
$fruit[1] = "orange"
println len($fruit)

// outputs 2
```



## Neko


```neko
var fruit = $array("apple", "orange");

$print($asize(fruit));
```



## NewLISP


```newlisp
(println (length '("apple" "orange")))

; Nehal-Singhal 2018-05-25
(length '(apple orange))
```



## NGS


```NGS
echo(len(['apple', 'orange']))
# same
echo(['apple', 'orange'].len())
```



## Nim


```nim
let fruit = ["apple", "orange"]
echo "The length of the fruit array is ", len(fruit)
```


```txt

The length of the fruit array is 2

```


=={{header|Oberon-2}}==
```oberon2

MODULE ArrayLength;
IMPORT
  Strings,
  Out;
TYPE
  String = POINTER TO ARRAY OF CHAR;
VAR
  a: ARRAY 16 OF String;

PROCEDURE NewString(s: ARRAY OF CHAR): String;
VAR
  str: String;
BEGIN
  NEW(str,Strings.Length(s) + 1);COPY(s,str^);
  RETURN str
END NewString;

PROCEDURE Length(a: ARRAY OF String): LONGINT;
VAR
  i: LONGINT;
BEGIN
  i := 0;
  WHILE (a[i] # NIL) DO INC(i) END;
  RETURN i;
END Length;

BEGIN
  a[0] := NewString("apple");
  a[1] := NewString("orange");
  Out.String("length: ");Out.Int(Length(a),0);Out.Ln
END ArrayLength.

```

```txt

length: 2

```



## Objeck


```objeck

class Test {
  function : Main(args : String[]) ~ Nil {
    fruit := ["apples", "oranges"];
    fruit->Size()->PrintLine();
  }
}
```



## OCaml


```OCaml

Array.length [|"apple"; "orange"|];;

```



## Oforth


```Oforth
[ "apple", "orange" ] size
```



## Onyx



```onyx
[`apple' `orange'] length # leaves 2 on top of the stack
```



## ooRexx


```oorexx

/* REXX */
a = .array~of('apple','orange')
say a~size 'elements'
Do e over a
  say e
  End
Say "a[2]="a[2]
```

```txt
2 elements
apple
orange
a[2]=orange
```



## PARI/GP


```parigp
array = ["apple", "orange"]
length(array)       \\ == 2
#array              \\ == 2
```


The <code>#</code> syntax is a handy shorthand.  It usually looks best just on variables but it works on expressions too, possibly with parens to control precedence.

Both forms work on column vectors too, and also on strings and matrices.  (For a matrix it is the number of columns.)


## Pascal


```Pascal

#!/usr/bin/instantfpc
//program ArrayLength;

{$mode objfpc}{$H+}

uses SysUtils, Classes;

const
  Fruits : array[0..1] of String = ('apple', 'orange');

begin
   WriteLn('Length of Fruits by function : ', Length(Fruits));
   WriteLn('Length of Fruits by bounds : ', High(Fruits) - Low(Fruits) + 1);
END.

```


```txt

./ArrayLength.pas
Length of Fruits by function : 2
Length of Fruits by bounds : 2


```



## Perl


The way to get the number of elements of an array in Perl is to put the array in scalar context.


```perl
my @array = qw "apple orange banana", 4, 42;

scalar @array;      #  5
0 + @arrray;        #  5
'' . @array;        # "5"
my $elems = @array; # $elems = 5

scalar @{  [1,2,3]  }; # [1,2,3] is a reference which is already a scalar

my $array_ref = \@array; # a reference
scalar @$array_ref;


# using subroutine prototypes, not generally recommended
# and not usually what you think they are
sub takes_a_scalar ($) { my ($a) = @_; return $a }

takes_a_scalar @array;

# the built-ins can also act like they have prototypes
```


A common mistake is to use <code>length</code> which works on strings not arrays.
So using it on an array, as a side-effect, actually gives you a number which represents the order of magnitude.


```perl6
length '' . @array; # 1
length      @array; # 1

print '0.', scalar @array, 'e', length @array, "\n"; # 0.5e1

@array = 1..123;
print '0.', scalar @array, 'e', length @array, "\n"; # 0.123e3

print 'the length of @array is on the order of ';
print 10 ** (length( @array )-1); # 100
print " elements long\n";
```



## Perl 6

To get the number of elements of an array in Perl 6 you put the array in a coercing Numeric context, or call <code>elems</code> on it.


```perl6

my @array = <apple orange>;

say @array.elems;  # 2
say elems @array;  # 2
say + @array;      # 2
say @array + 0;    # 2

```


Watch out for infinite/lazy arrays though. You can't get the length of those.


```perl6
my @infinite = 1 .. Inf;  # 1, 2, 3, 4, ...

say @infinite[5000];  # 5001
say @infinite.elems;  # Throws exception "Cannot .elems a lazy list"

```



## Phix


```Phix
constant fruits = {"apple","orange"}
?length(fruits)
```

```txt

2

```



## PHP



```php
print count(['apple', 'orange']); // Returns 2
```



## PicoLisp


```PicoLisp
: (length '(apple orange))
-> 2
:
```



## Pike


```pike
void main()
{
    array fruit = ({ "apple", "orange" });
    write("%d\n", sizeof(fruit));
}
```


```txt
2
```



## PL/I


```pli
 p: Proc Options(main);
 Dcl a(2) Char(6) Varying Init('apple','orange');
 Put Edit('Array a has',(hbound(a)-lbound(a)+1),' elements.')
         (Skip,a,f(2),a);
 Put Skip Data(a);
 End;
```

```txt

Array a has 2 elements.
A(1)='apple'            A(2)='orange';
```



## Plorth


```plorth
["apple", "orange"] length println
```



## Pony


```pony

actor Main
    new create(env:Env)=>
        var c=Array[String](2)
        c.push("apple")
        c.push("orange")
        env.out.print("Array c is " + c.size().string() + " elements long!")

```



## Potion


```potion
("apple", "orange") length print
```



## PowerShell


```PowerShell

$Array = @( "Apple", "Orange" )
$Array.Count
$Array.Length
```

```txt

2
2
```



## Processing


```Processing

String[] arr = {"apple", "orange"};
void setup(){
  println(arr.length);
}

```

```txt

2

```



## Prolog


```Prolog
| ?- length(["apple", "orange"], X).

X = 2

yes
```



## PureBasic


```PureBasic

EnableExplicit
Define Dim fruit$(1); defines array with 2 elements at indices 0 and 1
fruit$(0) = "apple"
fruit$(1) = "orange"
Define length = ArraySize(fruit$()) + 1; including the element at index 0
If OpenConsole()
  PrintN("The length of the fruit array is " + length)
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```



## Python


```python
>>
 print(len(['apple', 'orange']))
2
>>>
```



## R


```R

a <- c('apple','orange')   # create a vector containing "apple" and "orange"
length(a)

```

```txt

[1] 2

```



## Racket

```racket
#lang racket/base
(length '("apple" "orange")) ;; list
(vector-length #("apple" "orange")) ;; vector
```

```txt
2
2
```



## Red


```Red
length? ["apples" "oranges"]
== 2
```



## REXX


```rexx
/* REXX ----------------------------------------------
* The compond variable a. implements an array
* By convention, a.0 contains the number of elements
*---------------------------------------------------*/
a.=0                   /* initialize the "array" */
call store 'apple'
Call store 'orange'
Say 'There are' a.0 'elements in the array:'
Do i=1 To a.0
  Say 'Element' i':' a.i
  End
Exit
store: Procedure Expose a.
z=a.0+1
a.z=arg(1)
a.0=z
Return
```

```txt
There are 2 elements in the array:
Element 1: apple
Element 2: orange
```



## Ring



```ring
See len(['apple', 'orange']) # output = 2
```



## Robotic

As stated before in the [[Arrays#Robotic|arrays]] section on this Wiki, there is no functions listed for the manipulation/status of arrays. The best way we can count for length in an array is to have a variable keep track of it.

Example 1:

```robotic

set "index" to 0
set "$array&index&" to "apple"
inc "index" by 1
set "$array&index&" to "orange"
* "Array length: ('index' + 1)"

```


Example 2:

```robotic

set "index" to 0
set "local1" to random 1 to 99
: "rand"
set "array&index&" to random 0 to 99
inc "index" 1
dec "local1" 1
if "local1" > 1 then "rand"
* "Array length: ('index')"

```



## Ruby



```ruby
puts ['apple', 'orange'].length  # or .size
```



## Rust


By default arrays are immutable in rust.


```rust

fn main() {
    let array = ["foo", "bar", "baz", "biff"];
    println!("the array has {} elements", array.len());
}

```



## Scala



```scala

println(Array("apple", "orange").length)

```



## Scheme


Using Scheme's vector type as an equivalent to an array:


```scheme

(display (vector-length #("apple" "orange")))

```



## Seed7

The function [http://seed7.sourceforge.net/libraries/array.htm#length(in_arrayType) length]
determines the length of an array.

```seed7
$ include "seed7_05.s7i";

const array string: anArray is [] ("apple", "orange");

const proc: main is func
  begin
    writeln(length(anArray));
  end func;
```



## SETL


```haskell

arr := ["apple", "orange"];
print(# arr); -- '#' is the cardinality operator. Works on strings, tuples, and sets.

```



## Sidef


```ruby
var arr = ['apple', 'orange'];
say arr.len;        #=> 2
say arr.end;        #=> 1 (zero based)
```


## Simula


```simula
COMMENT ARRAY-LENGTH;
BEGIN

    INTEGER PROCEDURE ARRAYLENGTH(A); TEXT ARRAY A;
    BEGIN
        ARRAYLENGTH := UPPERBOUND(A, 1) - LOWERBOUND(A, 1) + 1;
    END ARRAYLENGTH;

    TEXT ARRAY A(1:2);
    INTEGER L;
    A(1) :- "APPLE";
    A(2) :- "ORANGE";
    L := ARRAYLENGTH(A);
    OUTINT(L, 0);
    OUTIMAGE;
END

```

```txt

2

```



## Smalltalk


```smalltalk

a := #('apple' 'orange').
a size
```



## SNOBOL4


```SNOBOL4
    ar = ARRAY('2,2')
    ar<1,1> = 'apple'
    ar<1,2> = 'first'
    ar<2,1> = 'orange'
    ar<2,2> = 'second'
    OUTPUT = IDENT(DATATYPE(ar), 'ARRAY') PROTOTYPE(ar)
end
```

```txt
2,2
```



## SPL


```spl
a = ["apple","orange"]
#.output("Number of elements in array: ",#.size(a,1))
```

```txt

Number of elements in array: 2

```



## SQL



```sql
SELECT COUNT() FROM (VALUES ('apple'),('orange'));
```



## Standard ML


```sml
let
  val a = Array.fromList ["apple", "orange"]
in
  Array.length a
end;
```



## Stata

String data may be stored either in a Stata dataset or in a Mata matrix, not in a Stata matrix, which may hold only numeric data. A list of strings may also be stored in a Stata macro.


###  Dimensions of a dataset


```stata
clear
input str10 fruit
apple
orange
end

di _N
di c(N) " " c(k)
```



###  Length of a macro list


Use either the '''[https://www.stata.com/help.cgi?macrolists sizeof]''' macro list function or the '''[https://www.stata.com/help.cgi?extended_fcn word count]''' extended macro function. Notice that the argument of the former is the macro ''name'', while the argument of the latter is the macro ''contents''.


```stata
local fruits apple orange
di `: list sizeof fruits'
di `: word count `fruits''
```



###  Mata

For a Mata array, use '''[https://www.stata.com/help.cgi?mf_rows rows]''' and similar functions:


```stata
mata
a=st_sdata(.,"fruit")
rows(a)
cols(a)
length(a)
end
```



## Swift



```Swift
import Cocoa //include Cocoa library (standard in OS X)

let fruits = ["apple", "orange"] //declare constant array literal
let fruitsCount = fruits.count //declare constant array length (count)

print(fruitsCount) //print array length to output window
```

```txt
2
```



## Tcl

<!-- http://ideone.com/uotEvm -->


```tcl
;# not recommanded:
set mylistA {apple orange}   ;# actually a string
set mylistA "Apple Orange"   ;# same - this works only for simple cases

set lenA [llength $mylistA]
puts "$mylistA :  $lenA"

# better:  to build a list, use 'list' and/or 'lappend':
set mylistB [list apple orange "red wine" {green frog}]
lappend mylistB "blue bird"

set lenB [llength $mylistB]
puts "$mylistB :  $lenB"

```


```txt
Apple Orange :  2
apple orange {red wine} {green frog} {blue bird} :  5
```


=={{header|TI-83 BASIC}}==
Use function <code>dim()</code>.

```ti83b
{1,3,–5,4,–2,–1}→L1
dim(L1)
```

```txt

6

```



## UNIX Shell


```bash
#!/bin/bash
array=("orange" "apple")
echo "${#array[@]}"
```

```txt

2

```



## Ursa


```txt
> decl string<> stream
> append "two" "strings" stream
> out (size stream) endl console
2
> out (size "test string") endl console
11
>
```



## VBA


```vb
Debug.Print "Array Length: " & UBound(Array("apple", "orange")) + 1
```


```txt
Array Length: 2
```



## VBScript


```vb
arr = Array("apple","orange")
WScript.StdOut.WriteLine UBound(arr) - LBound(arr) + 1
```


```txt
2
```



## Visual Basic

The amount of elements in an array in Visual Basic is computed via the upper bound and lower bound indices. In Visual Basic the indices of arrays have to be numeric, but it is even possible to have negative values for them. Of course the element numbering is continuous.


```vb

' declared in a module
Public Function LengthOfArray(ByRef arr As Variant) As Long
  If IsArray(arr) Then
     LengthOfArray = UBound(arr) - LBound(arr) + 1
  Else
     LengthOfArray = -1
  End If
End Function

' somewhere in the programm
' example 1
  Dim arr As Variant

  arr = Array("apple", "orange")

  Debug.Print LengthOfArray(arr) ' prints 2 as result

' example 2
  Dim arr As Variant

  ReDim arr(-2 To -1)
  arr(-2) = "apple"
  arr(-1) = "orange"

  Debug.Print LengthOfArray(arr) ' prints 2 as result


```



## Visual Basic .NET

```vbnet
Module ArrayLength

    Sub Main()
        Dim array() As String = {"apple", "orange"}
        Console.WriteLine(array.Length)
    End Sub

End Module

```


```txt

2

```



## WDTE


```wdte
let io =
 import 'io';
let a => ['apple'; 'orange'];
len a -- io.writeln io.stdout;
```



## Wren


```wren
var arr = [1,2,3]
var length = arr.count

```



## XLISP


```lisp
(vector-length #("apple" "orange"))
```



## zkl

zkl doesn't support arrays natively, use lists instead.

```zkl
List("apple", "orange").len().println() //-->2, == L("apple", "orange")
T("apple", "orange").len().println() //-->2, read only list (ROList)
```



## zonnon


```zonnon

module AryLength;
type
	Vector = array 12 of string;
	Matrix = array *,* of string;
var
	a: Vector;
	b: Matrix;
begin
	writeln(len(a):4);		(* len(a) = len(a,0) *)

	b := new Matrix(10,11);
	writeln(len(b,0):4); 	(* first dimension *)
	writeln(len(b,1):4)		(* second dimension *)
end AryLength.

```

