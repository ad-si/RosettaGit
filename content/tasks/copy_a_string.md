+++
title = "Copy a string"
description = ""
date = 2019-10-17T04:17:11Z
aliases = []
[extra]
id = 1965
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

This task is about copying a string.


## Task

Where it is relevant, distinguish between copying the contents of a string
versus making an additional reference to an existing string.





## 11l


```11l
V src = ‘hello’
V dst = src
```



## 360 Assembly

To copy a string, we use an MVC (Move Character). To make a reference to a string, we use a LA (Load Address).

```360asm
*        Duplicate a string
         MVC    A,=CL64'Hello'     a='Hello'
         MVC    B,A                b=a          memory copy
         MVC    A,=CL64'Goodbye'   a='Goodbye'
         XPRNT  A,L'A              print a
         XPRNT  B,L'B              print b
         ...
*        Make reference to a string a string
         MVC    A,=CL64'Hi!'       a='Hi!'
         LA     R1,A               r1=@a        set pointer
         ST     R1,REFA            refa=@a      store pointer
         XPRNT  A,L'A              print a
         XPRNT  0(R1),L'A          print %refa
         ...
A        DS     CL64               a
B        DS     CL64               b
REFA     DS     A                  @a
```



## ABAP


```ABAP
data: lv_string1 type string value 'Test',
      lv_string2 type string.
lv_string2 = lv_string1.
```



### Inline Declaration

```ABAP
DATA(string1) = |Test|.
DATA(string2) = string1.
```



## ActionScript

Strings are immutable in ActionScript, and can safely be assigned with the assignment operator, much as they can in Java.[http://livedocs.adobe.com/flash/9.0/main/00000647.html]

```ActionScript
var str1:String = "Hello";
var str2:String = str1;
```



## Ada

Ada provides three different kinds of strings.
The String type is a fixed length string.
The Bounded_String type is a string with variable length up to a specified maximum size.
The Unbounded_String type is a variable length string with no specified maximum size.
The Bounded_String type behaves a lot like C strings, while the Unbounded_String type behaves a lot like the C++ String class.


### Fixed Length String Copying.


```ada
Src : String := "Hello";
Dest : String := Src;
```

Ada provides the ability to manipulate slices of strings.

```ada
Src : String := "Rosetta Stone";
Dest : String := Src(1..7); -- Assigns "Rosetta" to Dest
Dest2 : String := Src(9..13); -- Assigns "Stone" to Dest2
```



### Bounded Length String Copying


```ada
-- Instantiate the generic package Ada.Strings.Bounded.Generic_Bounded_Length with a maximum length of 80 characters
package Flexible_String is new Ada.Strings.Bounded.Generic_Bounded_Length(80);
use Flexible_String;

Src : Bounded_String := To_Bounded_String("Hello");
Dest : Bounded_String := Src;
```

Ada Bounded_String type provides a number of functions for dealing with slices.


###  Unbounded Length String Copying


```ada
-- The package Ada.Strings.Unbounded contains the definition of the Unbounded_String type and all its methods
Src : Unbounded_String := To_Unbounded_String("Hello");
Dest : Unbounded_String := Src;
```



## Aime

The intrinsic text type is immediate, immutable
and cannot be referred more than once.

Copying an intrinsic string:

```aime
text s, t;
t = "Rosetta";
s = t;
```

Data of the non intrinsic byte array type can be referred more than once.
Copying a binary array of bytes:

```aime
data s, t;
# Copy -t- into -s-
b_copy(s, t);
# Set -s- as a reference of the object -t- is pointing
b_set(s, t);
# or:
s = t;

```



## ALGOL 68

In ALGOL 68 strings are simply flexible length arrays of CHAR;


```algol68
(
  STRING src:="Hello", dest;
  dest:=src
)
```



## ALGOL W


```algolw
begin
    % strings are (fixed length) values in algol W. Assignment makes a copy   %
    string(10) a, copyOfA;
    a := "some text";
    copyOfA := a;
    % assignment to a will not change copyOfA                                 %
    a := "new value";
    write( a, copyOfA )
end.
```

```txt

new value some text

```



## Apex

In Apex, Strings are a primitive data type

```apex
String original = 'Test';
String cloned = original;
//"original == cloned" is true

cloned += ' more';
//"original == cloned" is false
```



## AppleScript


```AppleScript
set src to "Hello"
set dst to src
```



## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program copystr.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szString: .asciz "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n"

/* UnInitialized data */
.bss
.align 4
iPtString:   .skip 4
szString1:    .skip 80

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    @ display start string
    ldr r0,iAdrszString
    bl affichageMess
    @ copy pointer string
    ldr r0,iAdrszString
    ldr r1,iAdriPtString
    str r0,[r1]
    @ control
    ldr r1,iAdriPtString
    ldr r0,[r1]
    bl affichageMess
    @ copy string
    ldr r0,iAdrszString
    ldr r1,iAdrszString1
1:
    ldrb r2,[r0],#1   @ read one byte and increment pointer one byte
    strb r2,[r1],#1   @ store one byte and increment pointer one byte
    cmp r2,#0          @ end of string ?
    bne 1b            @ no -> loop
    @ control
    ldr r0,iAdrszString1
    bl affichageMess

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszString:		.int szString
iAdriPtString:		.int iPtString
iAdrszString1:		.int szString1

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



```



## AutoHotkey



```autohotkey
src := "Hello"
dst := src
```



## AutoIt



```autoit
$Src= "Hello"
$dest = $Src
```



## AWK



```awk
BEGIN {
  a = "a string"
  b = a
  sub(/a/, "X", a) # modify a
  print b  # b is a copy, not a reference to...
}
```



## Axe


```axe
Lbl STRCPY
r₁→S
While {r₂}
 {r₂}→{r₁}
 r₁++
 r₂++
End
0→{r₁}
S
Return
```



## Babel


To copy a string in Babel is the same as copying any other object. Use the cp operator to make a deep-copy.


```babel>babel
 "Hello, world\n" dup cp dup 0 "Y" 0 1 move8
babel> << <<
Yello, world
Hello, world

```



## BASIC

  src$ = "Hello"
  dst$ = src$

=
## Applesoft BASIC
=

```ApplesoftBasic
100 DEF  FN P(A) =  PEEK (A) +  PEEK(A + 1) * 256 : FOR I =  FN P(105) TO  FN P(107) - 1 STEP 7 : ON PEEK(I + 1) < 128 OR PEEK(I) > 127 GOTO 130 : ON LEFT$(P$, 1) <> CHR$(PEEK(I)) GOTO 130
110 IF LEN(P$) > 1 THEN ON PEEK(I + 1) = 128 GOTO 130 : IF MID$(P$, 2, 1) <> CHR$(PEEK(I + 1) - 128) GOTO 130
120 POKE I + 4, P / 256 : POKE I + 3, P - PEEK(I + 4) * 256 : RETURN
130 NEXT I : STOP
```



```ApplesoftBasic
S$ = "HELLO" : REM S$ IS THE ORIGINAL STRING
C$ = S$ : REM C$ IS THE COPY
```



```ApplesoftBasic
P$ = "S" : P = 53637 : GOSUB 100"POINT STRING S AT SOMETHING ELSE
?S$
?C$
```


=
## BaCon
=
Strings by value or by reference

Strings can be stored by value or by reference. By value means that a copy of the original string is stored in a variable. This happens automatically when when a string variable name ends with the '$' symbol.

Sometimes it may be necessary to refer to a string by reference. In such a case, simply declare a variable name as STRING but omit the '$' at the end. Such a variable will point to the same memory location as the original string. The following examples should show the difference between by value and by reference.

When using string variables by value:


```freebasic
a$ = "I am here"
b$ = a$
a$ = "Hello world..."
PRINT a$, b$
```


This will print "Hello world...I am here". The variables point to their individual memory areas so they contain different strings. Now consider the following code:


```freebasic
a$ = "Hello world..."
LOCAL b TYPE STRING
b = a$
a$ = "Goodbye..."
PRINT a$, b
```


This will print "Goodbye...Goodbye..." because the variable 'b' points to the same memory area as 'a$'.

=
## Commodore BASIC
=

```basic
10 A$ = "HELLO"
20 REM COPY CONTENTS OF A$ TO B$
30 B$ = A$
40 REM CHANGE CONTENTS OF A$
50 A$ = "HI"
60 REM DISPLAY CONTENTS
70 PRINT A$, B$
```

Commodore BASIC can't do pointers or 'reference to'

=
## Sinclair ZX81 BASIC
=
Creating a new reference to an existing string is not possible, or at least not easy. (You could probably do it with <code>PEEK</code>s and <code>POKE</code>s.) This program demonstrates that an assignment statement copies a string, by showing that the two strings can afterwards be independently modified.

```basic
10 LET A$="BECAUSE I DO NOT HOPE TO TURN AGAIN"
20 LET B$=A$
30 LET A$=A$( TO 21)
40 PRINT B$
50 PRINT A$
60 LET B$=A$+B$(22 TO 29)
70 PRINT B$
```

```txt
BECAUSE I DO NOT HOPE TO TURN AGAIN
BECAUSE I DO NOT HOPE
BECAUSE I DO NOT HOPE TO TURN
```



## Batch File

Since the only variables are environment variables,
creating a string copy is fairly straightforward:

```dos
set src=Hello
set dst=%src%
```



## BBC BASIC

```bbcbasic
      source$ = "Hello, world!"

      REM Copy the contents of a string:
      copy$ = source$
      PRINT copy$

      REM Make an additional reference to a string:
      !^same$ = !^source$
      ?(^same$+4) = ?(^source$+4)
      ?(^same$+5) = ?(^source$+5)
      PRINT same$
```



## Bracmat

Because in Bracmat strings are unalterable, you never want to copy a string.
Still, you will obtain a copy of a string by overflowing the reference counter of the string.
(Currently, reference counters on strings and on most operators are 10 bits wide.
The <code>=</code> operator has a much wider 'inexhaustible' reference counter, because it anchors alterable objects.)
Still, you won't be able to test whether you got the original or a copy other than by looking at overall memory usage of the Bracmat program at the OS-level or by closely timing comparison operations.
You obtain a new reference to a string or a copy of the string by simple assignment using the <code>=</code> or the <code>:</code> operator:

```bracmat
abcdef:?a;
!a:?b;

c=abcdef;
!c:?d;

!a:!b { variables a and b are the same and probably referencing the same string }
!a:!d { variables a and d are also the same but not referencing the same string }

```



## C


```cpp
#include <iostream>	/* exit(), free() */
#include <stdio.h>	/* fputs(), perror(), printf() */
#include <string.h>

int
main()
{
	size_t len;
	char src[] = "Hello";
	char dst1[80], dst2[80];
	char *dst3, *ref;

	/*
	 * Option 1. Use strcpy() from <string.h>.
	 *
	 * DANGER! strcpy() can overflow the destination buffer.
	 * strcpy() is only safe if the source string is shorter than
	 * the destination buffer. We know that "Hello" (6 characters
	 * with the final '\0') easily fits in dst1 (80 characters).
	 */
	strcpy(dst1, src);

	/*
	 * Option 2. Use strlen() and memcpy() from <string.h>, to copy
	 * strlen(src) + 1 bytes including the final '\0'.
	 */
	len = strlen(src);
	if (len >= sizeof dst2) {
		fputs("The buffer is too small!\n", stderr);
		exit(1);
	}
	memcpy(dst2, src, len + 1);

	/*
	 * Option 3. Use strdup() from <string.h>, to allocate a copy.
	 */
	dst3 = strdup(src);
	if (dst3 == NULL) {
		/* Failed to allocate memory! */
		perror("strdup");
		exit(1);
	}

	/* Create another reference to the source string. */
	ref = src;

	/* Modify the source string, not its copies. */
	memset(src, '-', 5);

	printf(" src: %s\n", src);   /*  src: ----- */
	printf("dst1: %s\n", dst1);  /* dst1: Hello */
	printf("dst2: %s\n", dst2);  /* dst2: Hello */
	printf("dst3: %s\n", dst3);  /* dst3: Hello */
	printf(" ref: %s\n", ref);   /*  ref: ----- */

	/* Free memory from strdup(). */
	free(dst3);

	return 0;
}
```


==={{libheader|BSD libc}}===

```cpp
#include <iostream>	/* exit() */
#include <stdio.h>	/* fputs(), printf() */
#include <string.h>

int
main()
{
	char src[] = "Hello";
	char dst[80];

	/* Use strlcpy() from <string.h>. */
	if (strlcpy(dst, src, sizeof dst) >= sizeof dst) {
		fputs("The buffer is too small!\n", stderr);
		exit(1);
	}

	memset(src, '-', 5);
	printf("src: %s\n", src);  /* src: ----- */
	printf("dst: %s\n", dst);  /* dst: Hello */

	return 0;
}
```



## C++


```cpp
#include <iostream>
#include <string>

int main( ) {
   std::string original ("This is the original");
   std::string my_copy = original;
   std::cout << "This is the copy: " << my_copy << std::endl;
   original = "Now we change the original! ";
   std::cout << "my_copy still is " << my_copy << std::endl;
}
```


## C#

```c#
string src = "Hello";
string dst = src;
```



## Clojure



```clojure
(let [s "hello"
      s1 s]
  (println s s1))
```



## COBOL

```cobol
MOVE "Hello" TO src
MOVE src TO dst
```



## ColdFusion

In ColdFusion, only complex data types (structs, objects, etc.)
are passed by reference.
Hence, any string copy operations are by value.


```coldfusion
<cfset stringOrig = "I am a string." />
<cfset stringCopy = stringOrig />
```



## Common Lisp



```lisp
(let* ((s1     "Hello")        ; s1 is a variable containing a string
       (s1-ref s1)             ; another variable with the same value
       (s2     (copy-seq s1))) ; s2 has a distinct string object with the same contents
  (assert (eq s1 s1-ref))      ; same object
  (assert (not (eq s1 s2)))    ; different object
  (assert (equal s1 s2))       ; same contents

  (fill s2 #\!)                ; overwrite s2
  (princ s1)
  (princ s2))                  ; will print "Hello!!!!!"
```



## Crystal


```crystal
s1 = "Hello"
s2 = s1
```



## Component Pascal


```oberon2

VAR
	str1: ARRAY 128 OF CHAR;
	str2: ARRAY 32 OF CHAR;
	str3: ARRAY 25 OF CHAR;

```

...

```oberon2

	str1 := "abcdefghijklmnopqrstuvwxyz";
        str3 := str1; (* don't compile, incompatible assignement *)
        str3 := str1$; (* runtime error, string too long *)
        str2 := str1$; (* OK *)

```



## Computer/zero Assembly

Assuming a string to be a zero-terminated array of bytes, this program takes a string beginning at address <tt>src</tt> and makes a copy of it beginning at address <tt>dest</tt>. As an example, we copy the string "Rosetta".

```czasm
ldsrc:  LDA  src
stdest: STA  dest
        BRZ  done  ; 0-terminated

        LDA  ldsrc
        ADD  one
        STA  ldsrc

        LDA  stdest
        ADD  one
        STA  stdest

        JMP  ldsrc

done:   STP

one:         1

src:         82    ; ASCII
             111
             115
             101
             116
             116
             97
             0

dest:
```



## D



```d
void main() {
    string src = "This is a string";

    // copy contents:
    auto dest1 = src.idup;

    // copy contents to mutable char array
    auto dest2 = src.dup;

    // copy just the fat reference of the string
    auto dest3 = src;
}
```



## dc


```dc
[a string]   # push "a string" on the main stack
d            # duplicate the top value
f            # show the current contents of the main stack
```


```txt
a string
a string
```



## Delphi


Delphi strings are reference counted with [[wp:Copy-on-write|copy on write]] semantics.


```Delphi
program CopyString;

{$APPTYPE CONSOLE}

var
  s1: string;
  s2: string;
begin
  s1 := 'Goodbye';
  s2 := s1; // S2 points at the same string as S1
  s2 := s2 + ', World!'; // A new string is created for S2

  Writeln(s1);
  Writeln(s2);
end.
```


```txt
Goodbye
Goodbye, World!
```


=={{header|Déjà Vu}}==
In Déjà Vu, strings are immutable,
so there really isn't a good reason to copy them.
As such, no standard way of doing so is provided.
However, one can still create a copy of a string
by concatenating it with an empty string.

```dejavu
local :orgininal "this is the original"
local :scopy concat( original "" )
!. scopy
```

```txt
"this is the original"
```



## DWScript

DWScript strings are value-type, from the language point of view,
you can't have a reference to a String,
no more than you can have a reference to an Integer or a Float
(unless you wrap in an object of course).

Internally they're transparently implemented
via either immutable reference or copy-on-write.


## Dyalect


Strings in Dyalect are immutable:


```dyalect
var src = "foobar"
var dst = src
```



## E


E is a [[pass-references-by-value]] object-oriented language, and strings are immutable, so there is never a need for or benefit from copying a string.
Various operations, such as taking the substring (run) from the beginning to the end (<code><var>someString</var>.run(0)</code>) might create a copy,
but this is not guaranteed.


## EasyLang


<lang>a$ = "hello"
b$ = a$
```



## EchoLisp

Strings are immutable. A copy will return the same object.

```scheme

(define-syntax-rule (string-copy s) (string-append s)) ;; copy = append nothing
    → #syntax:string-copy
(define s "abc")
(define t (string-copy s))
    t → "abc"
(eq? s t) → #t ;; same reference, same object

```



## EDSAC order code

Expects the final character of a string to be marked with a 1 in the least significant bit, as in [[Hello world/Line printer#EDSAC order code]]. The source string should be loaded at <i>θ</i>+34; it is copied into storage tank 6. The copy is then printed out.

```edsac
[ Copy a string

### =======


  A program for the EDSAC

  Copies the source string into storage
  tank 6, which is assumed to be free,
  and then prints it from there

  Works with Initial Orders 2 ]

        T56K
        GK

[  0 ]  A34@      [ copy the string ]
[  1 ]  T192F
[  2 ]  H34@
        C32@
        S32@
        E17@
        T31@
        A@
        A33@
        T@
        A1@
        A33@
        T1@
        A2@
        A33@
        T2@
        E@
[ 17 ]  O192F     [ print the copy  ]
[ 18 ]  H192F
        C32@
        S32@
        E30@
        T31@
        A17@
        A33@
        T17@
        A18@
        A33@
        T18@
        E17@
[ 30 ]  ZF
[ 31 ]  PF
[ 32 ]  PD
[ 33 ]  P1F
[ 34 ]  *F
        RF
        OF
        SF
        EF
        TF
        TF
        AF
        !F
        CF
        OF
        DF
        ED

        EZPF
```

```txt
ROSETTA CODE
```



## Elena


```elena

var src := "Hello";
var dst := src;          //  copying the reference
var copy := src.clone(); //  copying the content

```



## Elixir


```elixir
src = "Hello"
dst = src
```



## Erlang


```erlang
Src = "Hello".
Dst = Src.
```



## Emacs Lisp


```Lisp

(setq str1 "hi")
(setq str2 str1)
(eq str1 str2)
```



## Euphoria

Arrays in many languages are constrained to have a fixed number of elements,
and those elements must all be of the same type.
Euphoria eliminates both of those restrictions by defining all arrays (sequences) as a list of zero or more Euphoria objects whose element count can be changed at any time.
When you retrieve a sequence element, it is not guaranteed to be of any type.
You, as a programmer, need to check that the retrieved data is of the type
you'd expect, Euphoria will not.
The only thing it will check is whether an assignment is legal.
For example, if you try to assign a sequence to an integer variable,
Euphoria will complain at the time your code does the assignment.


```Euphoria
sequence first = "ABC"
sequence newOne = first
```


=={{header|F Sharp|F#}}==
.NET strings are immutable, so it is usually not useful to make a deep copy.
However if needed, it is possible using a static method of the <code>System.String</code> type:

```fsharp
let str = "hello"
let additionalReference = str
let deepCopy = System.String.Copy( str )

printfn "%b" <| System.Object.ReferenceEquals( str, additionalReference ) // prints true
printfn "%b" <| System.Object.ReferenceEquals( str, deepCopy )            // prints false
```



## Factor


Factor strings are mutable but not growable.
Strings will be immutable in a future release.


```factor
"This is a mutable string." dup ! reference
"Let's make a deal!" dup clone  ! copy
"New" " string" append .               ! new string
    "New string"
```


Factor string buffers (sbufs) are mutable and growable.


```factor
SBUF" Grow me!" dup "  OK." append
    SBUF" Grow me!  OK."
```


Convert a string buffer to a string.


```factor
SBUF" I'll be a string someday." >string .
    "I'll be a string someday."
```



## Forth


Forth strings are generally stored in memory as prefix counted string,
where the first byte contains the string length.
However, on the stack they are most often represented as <addr cnt> pairs.
Thus the way you copy a string depends on where the source string comes from:


```forth
\ Allocate two string buffers
create stringa 256 allot
create stringb 256 allot

\ Copy a constant string into a string buffer
s" Hello" stringa place

\ Copy the contents of one string buffer into another
stringa count  stringb place
```



## Fortran



```fortran>str2 = str1</lang


Because Fortran uses fixed length character strings if str1 is shorter than str2 then str2 is padded out with trailing spaces.
If str1 is longer than str2 it is truncated to fit.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim s As String = "This is a string"
Dim t As String = s
' a separate copy of the string contents has been made as can be seen from the addresses
Print s, StrPtr(s)
Print t, StrPtr(t)
' to refer to the same string a pointer needs to be used
Dim u As String Ptr = @s
Print
Print *u, StrPtr(*u)
Sleep
```


```txt

This is a string            10623504
This is a string            10623552

This is a string            10623504

```



## Frink

Strings are immutable after construction, so "copying" a string just creates a new reference to a string.  All string manipulation routines return a new string.

```frink

a = "Monkey"
b = a

```



## FutureBasic

<lang>
include "ConsoleWindow"

dim as Str15 s, c
s = "Hello!"
c = s
print c

```


Output:

```txt

"Hello!"

```



## Gambas


Note that the DIM statement is required in Gambas.

'''[https://gambas-playground.proko.eu/?gist=b88224f45b9b5be09eafdf069b059076 Click this link to run this code]'''

```gambas
Public Sub main()
Dim src As String
Dim dst As String

src = "Hello"
dst = src

Print src
Print dst
End
```



## GAP


```gap
#In GAP strings are lists of characters. An affectation simply copy references
a := "more";
b := a;
b{[1..4]} := "less";
a;
# "less"

# Here is a true copy
a := "more";
b := ShallowCopy(a);
b{[1..4]} := "less";
a;
# "more"
```



## GML


```GML
src = "string";
dest = src;
```



## Go

Just use assignment:

```go
src := "Hello"
dst := src
```

Strings in Go are immutable.  Because of this, there is no need to distinguish between copying the contents and making an additional reference.
Technically, Go strings are immutable byte slices.
A slice is an object that contains a reference to an underlying array.
In the assignment shown above, a new slice object is created for dst.
Its internal reference is likely to point to the same underlying array as src,
but the language does not specify this behavior or make any guarantees about it.


## Groovy

The dynamics of references and object creation are very much the same as in [[#Java|Java]].
However, the meaning of the equality (==) operator is different in Groovy, so we show those differences here, even though they are not relevant to the actual copying.

Example and counter-example:

```groovy
def string = 'Scooby-doo-bee-doo'    // assigns string object to a variable reference
def stringRef = string               // assigns another variable reference to the same object
def stringCopy = new String(string)  // copies string value into a new object, and assigns to a third variable reference
```


Test Program:

```groovy
assert string == stringRef           // they have equal values (like Java equals(), not like Java ==)
assert string.is(stringRef)          // they are references to the same objext (like Java ==)
assert string == stringCopy          // they have equal values
assert ! string.is(stringCopy)       // they are references to different objects (like Java !=)
```


'''Caveat Lector''': Strings are immutable objects in Groovy, so it is wasteful and utterly unnecessary to ever make copies of them within a Groovy program.


## GUISS



```guiss
Start.Programs,Accessories,Notepad,
Type:Hello world[pling],Highlight:Hello world[pling],
Menu,Edit,Copy,Menu,Edit,Paste
```



## Haskell


In Haskell, every value is immutable, including ''String''s.
So one never needs to copy them; references are shared.


## HicEst


```hicest
src = "Hello World"
dst = src
```



## i


```i
//Strings are immutable in 'i'.
software {
	a = "Hello World"
	b = a //This copies the string.

	a += "s"

	print(a)
	print(b)
}

```


=={{header|Icon}} and {{header|Unicon}}==
Strings in Icon are immutable.

```icon
procedure main()
    a := "qwerty"
    b := a
    b[2+:4] := "uarterl"
    write(a," -> ",b)
end
```


Under the covers 'b' is created as a reference to the same string as 'a';
the sub-string assignment creates a new copy of the string.
However, there is no way to tell this in the language.
While most of the time this is transparent, programs that create very long strings through repeated concatenation need to avoid generating intermediate strings.
Instead using a list and concatenating at the last minute can perform much better.

Note that strings are indicated using double quotes.
However, single quotes are another type called character sets or csets.


## J


```j
src  =: 'hello'
dest =: src
```


J has copy-on-write semantics.
So both <code>src</code> and <code>dest</code> are references to the same memory, until <code>src</code> changes, at which time <code>dest</code> retains a copy of the original value of <code>src</code>.


## Java

In Java, Strings are immutable, so it doesn't make that much difference to copy it.

```java
String src = "Hello";
String newAlias = src;
String strCopy = new String(src);

//"newAlias == src" is true
//"strCopy == src" is false
//"strCopy.equals(src)" is true
```


Instead, maybe you want to create a <code>StringBuffer</code> (mutable string) from an existing String or StringBuffer:

```java
StringBuffer srcCopy = new StringBuffer("Hello");
```



## JavaScript

Objects can be copied in JavaScript via simple reassignment.
Changes to the properties of one will be reflected in the other:

```javascript
var container = {myString: "Hello"};
var containerCopy = container; // Now both identifiers refer to the same object

containerCopy.myString = "Goodbye"; // container.myString will also return "Goodbye"
```


If you copy property values with reassignment, such as properties of the global object (<code>window</code> in browsers), only the value will be copied and not the reference

```javascript
var a = "Hello";
var b = a; // Same as saying window.b = window.a

b = "Goodbye" // b contains a copy of a's value and a will still return "Hello"
```



## jq

jq is a functional language and all data types, including strings, are immutable.  If a string were to be copied (e.g. by exploding and imploding it), the resultant string would be equal in all respects to the original, and from the jq programmer's perspective, the two would be identical.

jq does however have a type of variable, though their values actually don't change -- they are just context-dependent. For example, consider the sequence of steps in the following function:
```jq
def demo:
  "abc" as $s    # assignment of a string to a variable
  | $s as $t     # $t points to the same string as $s
  | "def" as $s  # This $s shadows the previous $s
  | $t           # $t still points to "abc"
;

demo

```

 "abc"


## Joy


```joy
"hello" dup
```


Strings are immutable.


## Julia

Strings are immutable in Julia.  Assignment of one string valued variable to another is effectively a copy, as subsequent changes to either variable have no effect on the other.

```Julia

s = "Rosetta Code"
t = s

println("s = \"", s, "\" and, after \"t = s\", t = \"", t, "\"")

s = "Julia at "*s

println("s = \"", s, "\" and, after this change, t = \"", t, "\"")

```


```txt

s = "Rosetta Code" and, after "t = s", t = "Rosetta Code"
s = "Julia at Rosetta Code" and, after this change, t = "Rosetta Code"

```



## KonsolScript


```KonsolScript
Var:String str1 = "Hello";
Var:String str2 = str1;
```



## Kotlin


```scala
val s = "Hello"
val alias = s      // alias === s
val copy = "" + s  // copy !== s
```



## LabVIEW

In LabVIEW, one can simply wire an input to more than one output.<br/>
{{VI snippet}}<br/>[[File:LabVIEW_Copy_a_string.png]]


## Lang5


```lang5
'hello dup
```



## Lasso

While other datatypes like arrays require ->asCopy & ->asCopyDeep methods,
assigning strings creates a copy, not a reference, as is seen below.

```Lasso
local(x = 'I saw a rhino!')
local(y = #x)

#x //I saw a rhino!
'\r'
#y //I saw a rhino!

'\r\r'
#x = 'I saw one too'
#x //I saw one too
'\r'
#y //I saw a rhino!

'\r\r'
#y = 'it was grey.'
#x //I saw one too
'\r'
#y //it was grey.
```



## LC3 Assembly

Copying a string is the same as copying any other zero-terminated array. This program copies the string at <tt>SRC</tt> to <tt>COPY</tt>, then prints the copy to show it has worked.

```lc3asm
        .ORIG      0x3000

        LEA        R1,SRC
        LEA        R2,COPY

LOOP    LDR        R3,R1,0
        STR        R3,R2,0
        BRZ        DONE
        ADD        R1,R1,1
        ADD        R2,R2,1
        BRNZP      LOOP

DONE    LEA        R0,COPY
        PUTS

        HALT

SRC     .STRINGZ   "What, has this thing appeared again tonight?"

COPY    .BLKW      128

        .END
```

```txt
What, has this thing appeared again tonight?
```



## LFE



```lisp
(let* ((a '"data assigned to a")
       (b a))
  (: io format '"Contents of 'b': ~s~n" (list b)))
```


```txt

Contents of 'b': data assigned to a

```


One can also use <code>set</code> to copy a sting when one is in the LFE REPL:


```lisp>
 (set a '"data")
"data"
> a
"data"
> (set b a)
"data"
> b
"data"
```



## Liberty BASIC


```lb
src$ = "Hello"
dest$ = src$
print src$
print dest$

```



## Lingo


```lingo
str = "Hello world!"
str2 = str
```


Syntax-wise strings are not immuatable in Lingo. You can alter an existing string without new assignment:


```lingo
put "X" before str
put "X" after str
put "X" into char 6 of str
put str
-- "XHellX world!X"
```


But memory-wise they are immutable: Lingo internally stores references to strings, and as soon as a string is altered, a new copy is created on the fly, so other references to the original string are not affected by the change.


## Lisaac


```Lisaac
+ scon : STRING_CONSTANT;
+ svar : STRING;

scon := "sample";
svar := STRING.create 20;
svar.copy scon;
svar.append "!\n";

svar.print;
```

STRING_CONSTANT is immutable, STRING is not.

## Little


```C
string a = "A string";
string b = a;
a =~ s/$/\./;
puts(a);
puts(b);
```



## LiveCode


```LiveCode
put "foo" into bar
put bar into baz
answer bar && baz
```


Copies are nearly always made, on function calls parameters may be passed by reference (pointer) by prepending @ to a parameter in the function definition, however this is the only case where it is usually performed.


## Logo

As a functional language, words are normally treated as symbols and cannot be modified. The EQUAL? predicate compares contents instead of identity. In [[UCB Logo]] the .EQ predicate tests for "thing" identity.

```logo
make "a "foo
make "b "foo
print .eq :a :b   ; true, identical symbols are reused

make "c :a
print .eq :a :c   ; true, copy a reference

make "c word :b "||  ; force a copy of the contents of a word by appending the empty word
print equal? :b :c   ; true
print .eq :b :c     ; false
```



## Lua

Lua strings are immutable, so only one reference to each string exists.

```lua

a = "string"
b = a
print(a == b) -->true
print(b) -->string
```



## Maple

In Maple, you cannot really copy a string in the sense that there can be two copies of the string in memory.  As soon as you create a second copy of a string that already exists, it get turned into a reference to the first copy.  However, you can copy a reference to a string by a simple assignment statement.

```Maple

> s := "some string";
                           s := "some string"

> t := "some string";
                           t := "some string"

> evalb( s = t ); # they are equal
                                  true

> addressof( s ) = addressof( t ); # not just equal data, but the same address in memory
                        3078334210 = 3078334210

> u := t: # copy reference

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a="Hello World"
b=a
```



## MATLAB


```MATLAB
string1 = 'Hello';
string2 = string1;
```



## Maxima


```maxima
/* It's possible in Maxima to access individual characters by subscripts, but it's not the usual way.
Also, the result is "Lisp character", which cannot be used by other Maxima functions except cunlisp. The usual
way to access characters is charat, returning a "Maxima character" (actually a one characte string). With the latter,
it's impossible to modify a string in place, thus scopy is of little use. */

a: "loners"$
b: scopy(a)$
c: a$

c[2]: c[5]$

a;
"losers"

b;
"loners"

c;
"losers"
```




## MAXScript


```maxscript
str1 = "Hello"
str2 = copy str1
```



## Metafont


Metafont will always copy a string (does not make references).


```metafont
string s, a;
s := "hello";
a := s;
s := s & " world";
message s;  % writes "hello world"
message a;  % writes "hello"
end
```




## MiniScript


```MiniScript
phrase = "hi"
copy = phrase
print phrase
print copy
```



## MIPS Assembly

This does a full copy of the string, not just copying the pointer to the string's contents.

```mips
.data
	ex_msg_og: .asciiz "Original string:\n"
	ex_msg_cpy: .asciiz "\nCopied string:\n"
	string: .asciiz "Nice string you got there!\n"

.text
	main:
		la $v1,string #load addr of string into $v0
		la $t1,($v1)  #copy addr into $t0 for later access
		lb $a1,($v1)  #load byte from string addr
	strlen_loop:
		beqz $a1,alloc_mem
		addi $a0,$a0,1 #increment strlen_counter
		addi $v1,$v1,1 #increment ptr
		lb $a1,($v1)   #load the byte
		j strlen_loop

	alloc_mem:
		li $v0,9 #alloc memory, $a0 is arg for how many bytes to allocate
		         #result is stored in $v0
		syscall
		la $t0,($v0) #$v0 is static, $t0 is the moving ptr
		la $v1,($t1) #get a copy we can increment
	copy_str:
		lb $a1,($t1) #copy first byte from source

	strcopy_loop:
		beqz $a1,exit_procedure #check if current byte is NULL
		sb $a1,($t0)            #store the byte at the target pointer
		addi $t0,$t0,1          #increment source ptr
		addi $t1,$t1,1          #decrement source ptr
		lb $a1,($t1)            #load next byte from source ptr
		j strcopy_loop


	exit_procedure:
		la $a1,($v0) #store our string at $v0 so it doesn't get overwritten
		li $v0,4 #set syscall to PRINT

		la $a0,ex_msg_og  #PRINT("original string:")
		syscall

		la $a0,($v1)      #PRINT(original string)
		syscall

		la $a0,ex_msg_cpy #PRINT("copied string:")
		syscall

		la $a0,($a1)      #PRINT(strcopy)
		syscall

		li $v0,10         #EXIT(0)
		syscall

```



## Mirah


```mirah
src = "Hello"
new_alias = src

puts 'interned strings are equal' if src == new_alias

str_copy = String.new(src)
puts 'non-interned strings are not equal' if str_copy != src
puts 'compare strings with equals()' if str_copy.equals(src)

```


=={{header|Modula-3}}==
Strings in Modula-3 have the type <code>TEXT</code>.

```modula3
VAR src: TEXT := "Foo";
VAR dst: TEXT := src;
```



## MUMPS

<lang>SET S1="Greetings, Planet"
SET S2=S1
```



## Nanoquery


```nanoquery
$a = "Hello"
$b = $a
```



## Neko


```Neko
var src = "Hello"
var dst = src
```



## Nemerle

Nemerle gives you the option of declaring a variable - even a string - as mutable, so the caveats of languages with only immutable strings don't necessarily apply.  However, Nemerle binds the value of the string to the new name when copying; to sort of emulate copying a reference you can use lazy evaluation.

```Nemerle
using System;
using System.Console;
using Nemerle;

module StrCopy
{
    Main() : void
    {
        mutable str1 = "I am not changed";      // str1 is bound to literal
        def str2 = lazy(str1);                  // str2 will be bound when evaluated
        def str3 = str1;                        // str3 is bound to value of str1
        str1 = "I am changed";                  // str1 is bound to new literal
        Write($"$(str1)\n$(str2)\n$(str3)\n");  // str2 is bound to value of str1
        // Output: I am changed
        //         I am changed
        //         I am not changed
    }
}
```



## NetRexx

In addition to the string capabilities provided by the Java String libraries (see [[#Java|Java]] for some examples) NetRexx provides comprehensive string capabilities through the built-in Rexx type.  Rexx strings can be copied by simple assignment; as follows:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

s1 = 'This is a Rexx string'
s2 = s1

s2 = s2.changestr(' ', '_')

say s1
say s2
```

In this example a string is created, the string is copied then the copy is modified with the <tt>changestr</tt> built-in function.  Finally both strings are displayed to confirm that the original string wasn't modified by the call to <tt>changestr</tt>.

```txt

This is a Rexx string
This_is_a_Rexx_string

```



## NewLISP


```NewLISP
(define (assert f msg) (if (not f) (println msg)))

(setq  s "Greetings!"  c (copy s))
(reverse c) ; Modifies c in place.

(assert (= s c) "Strings not equal.")

; another way
; Nehal-Singhal 2018-05-25

> (setq a "abcd")
"abcd"
> (setq b a)
"abcd"
> b
"abcd"
> (= a b)
true


```



## Nim


```nim
var
  c = "This is a string"
  d = c # Copy c into a new string
```


=={{header|Oberon-2}}==

```oberon2
MODULE CopyString;
TYPE
	String = ARRAY 128 OF CHAR;
VAR
	a,b: String;

BEGIN
	a := "plain string";
	COPY(a,b);
END CopyString.
```



## Objeck


```objeck
a := "GoodBye!";
b := a;
```


=={{header|Objective-C}}==
Immutable strings - since they are immutable, you may get the same instance with its references count increased. Or, you can get a copy which is mutable if you use <code>mutableCopy</code>. Remember that both <code>copy</code> and <code>mutableCopy</code> return a retained instance. You can also get a copy by doing <code>[NSString stringWithString:]</code> or <code>[[NSString alloc] initWithString:]</code>.

Note that both <code>copy</code> and <code>initWithString:</code>/<code>stringWithString:</code> are optimized to return the original string object (possibly retained) if it is immutable.


```objc
NSString *original = @"Literal String";
NSString *new = [original copy];
NSString *anotherNew = [NSString stringWithString:original];
NSString *newMutable = [original mutableCopy];
```


Mutable strings - you can get either new mutable (if you use <code>mutableCopy</code>) or immutable (if you use <code>copy</code>) string:


```objc
NSMutableString *original = [NSMutableString stringWithString:@"Literal String"];
NSString *immutable = [original copy];
NSString *anotherImmutable = [NSString stringWithString:original];
NSMutableString *mutable = [original mutableCopy];
```


Copying a CString into an NSString:


```objc
const char *cstring = "I'm a plain C string";
NSString *string = [NSString stringWithUTF8String:cstring];
```


Copying from data, possibly not null terminated:


```objc
char bytes[] = "some data";
NSString *string = [[NSString alloc] initWithBytes:bytes length:9 encoding:NSASCIIStringEncoding];
```


And of course, if a C string is needed, you can use standard functions like strcpy.


## OCaml


```ocaml>let dst = String.copy src</lang



## Octave


```octave>str2 = str1</lang



## Oforth

To make a copy of the reference, just dup the string

```Oforth
"abcde" dup
```


There is no need to copy a string content as strings are immutable. If really needed :

```Oforth
StringBuffer new "abcde" <<
```



## OxygenBasic


```oxygenbasic

string s, t="hello"
s=t

```



## ooRexx


```ooRexx
/* Rexx ***************************************************************
* 16.05.2013 Walter Pachl
**********************************************************************/

s1 = 'This is a Rexx string'
s2 = s1 /* does not copy the string */

Say 's1='s1
Say 's2='s2
i1=s1~identityhash; Say 's1~identityhash='i1
i2=s2~identityhash; Say 's2~identityhash='i2

s2 = s2~changestr('*', '*') /* creates a modified copy */

Say 's1='s1
Say 's2='s2
i1=s1~identityhash; Say 's1~identityhash='i1
i2=s2~identityhash; Say 's2~identityhash='i2
```

```txt
s1=This is a Rexx string
s2=This is a Rexx string
s1~identityhash=17587366586244
s2~identityhash=17587366586244
s1=This is a Rexx string
s2=This is a Rexx string
s1~identityhash=17587366586244
s2~identityhash=17587366588032
```



## PARI/GP

Assignment in GP always copies.

```parigp>s1=s</lang


In PARI, strings can be copied and references can be made.

```C
GEN string_copy = gcopy(string);
GEN string_ref = string;
```



## Pascal



```pascal
program in,out;

type

   pString = ^string;

var

   s1,s2 : string ;
   pStr  : pString ;

begin

   /* direct copy */
   s1 := 'Now is the time for all good men to come to the aid of their party.'
   s2 := s1 ;

   writeln(s1);
   writeln(s2);

   /* By Reference */
   pStr := @s1 ;
   writeln(pStr^);

   pStr := @s2 ;
   writeln(pStr^);

end;
```



## Perl


To copy a string, just use ordinary assignment:


```perl
my $original = 'Hello.';
my $new = $original;
$new = 'Goodbye.';
print "$original\n";   # prints "Hello."
```


To create a reference to an existing string, so that modifying the referent changes the original string, use a backslash:


```perl
my $original = 'Hello.';
my $ref = \$original;
$$ref = 'Goodbye.';
print "$original\n";   # prints "Goodbye."
```


If you want a new name for the same string, so that you can modify it without dereferencing a reference, assign a reference to a typeglob:


```perl
my $original = 'Hello.';
our $alias;
local *alias = \$original;
$alias = 'Good evening.';
print "$original\n";   # prints "Good evening."
```


Note that <tt>our $alias</tt>, though in most cases a no-op, is necessary under stricture. Beware that <tt>local</tt> binds dynamically, so any subroutines called in this scope will see (and possibly modify!) the value of <tt>$alias</tt> assigned here.

To make a lexical variable that is an alias of some other variable, the [http://search.cpan.org/perldoc?Lexical::Alias Lexical::Alias] module can be used:

```perl
use Lexical::Alias;
my $original = 'Hello.';
my $alias;
alias $alias, $original;
$alias = 'Good evening.';
print "$original\n";   # prints "Good evening."
```




## Perl 6


There is no special handling needed to copy a string; just assign it to a new variable:

```perl6
my $original = 'Hello.';
my $copy = $original;
say $copy;            # prints "Hello."
$copy = 'Goodbye.';
say $copy;            # prints "Goodbye."
say $original;        # prints "Hello."
```


You can also bind a new variable to an existing one so that each refers to, and can modify the same string.

```perl6
my $original = 'Hello.';
my $bound := $original;
say $bound;           # prints "Hello."
$bound = 'Goodbye.';
say $bound;           # prints "Goodbye."
say $original;        # prints "Goodbye."
```


<!-- SqrtNegInf 2016-01-16  This is NYI, so until such time as it is, leaving this section commented
You can also create a read-only binding which will allow read access to the string but prevent modification except through the original variable.

```perl6
# y $original = 'Hello.';
#my $bound-ro ::= $original;
#say $bound-ro;        # prints "Hello."
#try {
#  $bound-ro = 'Runtime error!';
#  CATCH {
#    say "$!";         # prints "Cannot modify readonly value"
#  };
#};
say $bound-ro;        # prints "Hello."
$original = 'Goodbye.';
say $bound-ro;        # prints "Goodbye."
```

-->


## Phix

Use of strings is utterly intuitive with no unexpected side effects. For example

```Phix
string this = "feed"
string that = this     -- (that becomes "feed", this remains "feed")
that[2..3] = "oo"      -- (that becomes "food", this remains "feed")
this[1] = 'n'          -- (that remains "food", this becomes "need")
?{this,that}

```

```txt

{"need","food"}

```

Phix variables are reference counted (except for integers). When a simple copy is made, it increases the reference count and shares the data, making it very fast on large sequences and long strings. Attempts to modify any data with a reference count greater than one cause a copy to be made, and all other variables are left unchanged. Strings <b><i>can</i></b> be modified "in situ", no problem.


## PHP



```php
$src = "Hello";
$dst = $src;
```



## PicoLisp


```PicoLisp
(setq Str1 "abcdef")
(setq Str2 Str1)                       # Create a reference to that symbol
(setq Str3 (name Str1))                # Create new symbol with name "abcdef"
```



## Pike


```pike
int main(){
   string hi = "Hello World.";
   string ih = hi;
}
```



## PL/I


```pli
   declare (s1, s2) character (20) varying;
   s1 = 'now is the time';
   s2 = s1;
```



## Pop11


In Pop11 normal data are represented by references, so plain assignment will copy references. To copy data one has to use copy procedure:


```pop11
vars src, dst;
'Hello' -> src;
copy(src) -> dst;
```


One can also combine assignment (initialization) with variable declarations:


```pop11
vars src='Hello';
vars dst=copy(src);
```



## PostScript

In PostScript,

```postscript
(hello) dup length string copy
```



## Prolog

Values in Prolog are immutable so unifying with a variable that already has the value of a string will effectively copy that string.
You cannot reassign a value once it has been unified, it is not logical to have a value equal more than one thing.

```prolog
?- A = "A test string", A = B.
A = B, B = "A test string".
```



## PowerShell

Since PowerShell uses .NET behind the scenes and .NET strings are immutable you can simply assign the same string to another variable without breaking anything:

```powershell
$str = "foo"
$dup = $str
```

To actually create a copy the <code>Clone()</code> method can be used:

```powershell
$dup = $str.Clone()
```


## PureBasic


```PureBasic
src$ = "Hello"
dst$ = src$
```



## ProDOS


```ProDOS
editvar /newvar /value=a /userinput=1 /title=Enter a string to be copied:
editvar /newvar /value=b /userinput=1 /title=Enter current directory of the string:
editvar /newvar /value=c /userinput=1 /title=Enter file to copy to:
copy -a- from -b- to -c-
```



## Python

Since strings are immutable, all copy operations return the same string. Probably the reference is increased.


```python>>>
 src = "hello"
>>> a = src
>>> b = src[:]
>>> import copy
>>> c = copy.copy(src)
>>> d = copy.deepcopy(src)
>>> src is a is b is c is d
True
```


To actually copy a string:


```python>>>
 a = 'hello'
>>> b = ''.join(a)
>>> a == b
True
>>> b is a  ### Might be True ... depends on "interning" implementation details!
False
```


As a result of object "interning" some strings such as the empty string and single character strings like 'a' may be references to the same object regardless of copying. This can potentially happen with any Python immutable object and should be of no consequence to any proper code.

Be careful with ''is'' - use it only when you want to compare the identity of the object. To compare string values, use the ''=='' operator.  For numbers and strings any given Python interpreter's implementation of "interning" may cause the object identities to coincide.  Thus any number of names to identical numbers or strings might become references to the same objects regardless of how those objects were derived (even if the contents were properly "copied" around).  The fact that these are immutable objects makes this a reasonable behavior.


## R

Copy a string by value:

```R
str1 <- "abc"
str2 <- str1
```



## Racket


```Racket

#lang racket

(let* ([s1 "Hey"]
       [s2 s1]
       [s3 (string-copy s1)]
       [s4 s3])
  (printf "s1 and s2 refer to ~a strings\n"
          (if (eq? s1 s2) "the same" "different")) ; same
  (printf "s1 and s3 refer to ~a strings\n"
          (if (eq? s1 s3) "the same" "different")) ; different
  (printf "s3 and s4 refer to ~a strings\n"
          (if (eq? s3 s4) "the same" "different")) ; same
  (string-fill! s3 #\!)
  (printf "~a~a~a~a\n" s1 s2 s3 s4)) ; outputs "HeyHey!!!!!!"

```



## Raven


Copy a string by reference:


```raven
'abc' as a
a as b
```


Copy a string by value:


```raven
'abc' as a
a copy as b
```



## REBOL


```REBOL
REBOL [
    Title: "String Copy"
    URL: http://rosettacode.org/wiki/Copy_a_string
]

x: y: "Testing."
y/2: #"X"
print ["Both variables reference same string:" mold x "," mold y]

x: "Slackeriffic!"
print ["Now reference different strings:" mold x "," mold y]

y: copy x        ; String copy here!
y/3: #"X"        ; Modify string.
print ["x copied to y, then modified:" mold x "," mold y]

y: copy/part x 7 ; Copy only the first part of y to x.
print ["Partial copy:" mold x "," mold y]

y: copy/part  skip x 2  3
print ["Partial copy from offset:" mold x "," mold y]
```


```txt
Script: "String Copy" (16-Dec-2009)
Both variables reference same string: "TXsting." , "TXsting."
Now reference different strings: "Slackeriffic!" , "TXsting."
x copied to y, then modified: "Slackeriffic!" , "SlXckeriffic!"
Partial copy: "Slackeriffic!" , "Slacker"
Partial copy from offset: "Slackeriffic!" , "ack"
```



## Red


```Red

Red[]
originalString: "hello wordl"
copiedString: originalString
; OR
copiedString2: copy originalString

```



## Retro


```Retro
"this is a string" dup tempString
```



## REXX

The example shows how to copy the contents of one string into another string.

Note that delimiters for literal strings, REXX accepts either of:
::*   ''' <big>'</big> '''     (an apostrophe)
::*   ''' <big>"</big> '''     (a double quote)

Also note that   ''all''   REXX values (variables) are
stored as (varying length)   ''character strings''.

```rexx
src = "this is a string"
dst = src
```



## RLaB


```RLaB>>
 s1 = "A string"
A string
>> s2 = s1
A string
```



## Ring


```ring

cStr1 = "Hello!"   # create original string
cStr2 = cStr1      # make new string from original

```



## Robotic


```robotic

set "$string1" to "This is a string"
set "$string2" to "$string1"
* "&$string2&"

```



## Ruby

In Ruby, String are mutable.

```ruby
original = "hello"
reference = original          # copies reference
copy1 = original.dup          # instance of original.class
copy2 = String.new(original)  # instance of String

original << " world!"         # append
p reference                   #=> "hello world!"
p copy1                       #=> "hello"
p copy2                       #=> "hello"
```


There is a method of Object#clone, too, in the copy of the object.

```ruby
original = "hello".freeze     # prevents further modifications
copy1 = original.dup          # copies contents (without status)
copy2 = original.clone        # copies contents (with status)
p copy1.frozen?               #=> false
p copy1 << " world!"          #=> "hello world!"
p copy2.frozen?               #=> true
p copy2 << " world!"          #=> can't modify frozen String (RuntimeError)
```



## Run BASIC


```runbasic
origString$ = "Hello!"     ' create original string
newString$  = origString$  ' make new strig from original
```



## Rust


```rust
fn main() {
    let s1 = "A String";
    let mut s2 = s1;

    s2 = "Another String";

    println!("s1 = {}, s2 = {}", s1, s2);
}
```


Output: <lang>s1 = A String, s2 = Another String
```



## Sather


```sather
class MAIN is
  main is
    s  ::= "a string";
    s1 ::= s;
    -- s1 is a copy
  end;
end;
```



## Scala


```scala
  val src = "Hello"
  // Its actually not a copy but a reference
  // That is not a problem because String is immutable
  // In fact its a feature
  val des = src
  assert(src eq des) // Proves the same reference is used.
  // To make a real copy makes no sense.
  // Actually its hard to make a copy, the compiler is too smart.
  // mkString, toString makes also not a real copy
  val cop = src.mkString.toString
  assert((src eq cop))                 // Still no copyed image
  val copy = src.reverse.reverse       // Finally double reverse makes a copy
  assert(src == copy && !(src eq copy))// Prove, but it really makes no sense.
```



## Scheme


```scheme
(define dst (string-copy src))
```



## Seed7



```seed7
var string: dest is "";

dest := "Hello";
```



## Shiny


```shiny
src: 'hello'
cpy: src
```



## Sidef


```ruby
var original = "hello";               # new String object
var reference = original;             # points at the original object
var copy1 = String.new(original);     # creates a new String object
var copy2 = original+'';              # ==//==
```



## Slate



```slate
[ | :s | s == s copy] applyTo: {'hello'}. "returns False"
```




## Smalltalk



```smalltalk
|s1 s2|
"bind the var s1 to the object string on the right"
s1 := 'i am a string'.
"bind the var s2 to the same object..."
s2 := s1.
"bind s2 to a copy of the object bound to s1"
s2 := (s1 copy).
```



## SNOBOL4


```snobol4

* copy a to b
          b = a = "test"
          output = a
          output = b
* change the copy
          b "t" = "T"
          output = b
end
```


```txt

  test
  test
  Test

```



## Standard ML

In Standard ML, <code>string</code>s are immutable, so you don't copy it.

Instead, maybe you want to create a <code>CharArray.array</code> (mutable string) from an existing <code>string</code>:

```sml
val src = "Hello";
val srcCopy = CharArray.array (size src, #"x"); (* 'x' is just dummy character *)
CharArray.copyVec {src = src, dst = srcCopy, di = 0};
src = CharArray.vector srcCopy; (* evaluates to true *)
```


or from another <code>CharArray.array</code>:

```sml
val srcCopy2 = CharArray.array (CharArray.length srcCopy, #"x"); (* 'x' is just dummy character *)
CharArray.copy {src = srcCopy, dst = srcCopy2, di = 0};
```



## Swift

Just use assignment:

```swift
var src = "Hello"
var dst = src
```

Strings in Swift are value types, so assigning copies the string.


## Tcl


```tcl
set src "Rosetta Code"
set dst $src
```

Tcl copies strings internally when needed.
To be exact, it uses a basic value model based on simple objects that are immutable when shared (i.e., when they have more than one effective reference to them); when unshared, they can be changed because the only holder of a reference has to be the code requesting the change.
At the script level, this looks like Tcl is making a copy when the variable is assigned as above, but is more efficient in the common case where a value is not actually modified.

=={{header|TI-83 BASIC}}==

```ti83b
:"Rosetta Code"→Str1
:Str1→Str2
```


=={{header|TI-89 BASIC}}==

```ti89b
:"Rosetta Code"→str1
:str1→str2
```



## Toka


```toka
" hello" is-data a
a string.clone is-data b
```



## Trith

Strings are immutable character sequences,
so copying a string just means duplicating the reference at the top of the stack:

```trith
"Hello" dup
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
str="Hello"
dst=str
```



## UNIX Shell



```bash
foo="Hello"
bar=$foo    # This is a copy of the string
```



## Ursa


```ursa
decl string a b
set a "hello"
set b a
```



## V

dup really makes a reference, but the language is functional,
so the string is immutable.


```v
"hello" dup
```



## VBA

This program copies string in variable a to variable b. Mutating variable a subsequently doesn't alter variable b. Variable b is not a reference.

```vb
Sub copystring()
    a = "Hello World!"
    b = a
    a = "I'm gone"
    Debug.Print b
    Debug.Print a
End Sub
```
```txt
Hello World!
I'm gone
```


## Vim Script


```vim
let str1 = "original string"
let str2 = str1
let str1 = "new string"

echo "String 1:" str1
echo "String 2:" str2
```


```txt
String 1: new string
String 2: original string
```



## Visual Basic .NET

'''Platform:''' [[.NET]]

```vbnet
'Immutable Strings
Dim a = "Test string"
Dim b = a 'reference to same string
Dim c = New String(a.ToCharArray) 'new string, normally not used

'Mutable Strings
Dim x As New Text.StringBuilder("Test string")
Dim y = x 'reference
Dim z = New Text.StringBuilder(x.ToString) 'new string
```


Alternatively, you can use, with all versions of the .NET framework:

```vbnet
Dim a As String = "Test String"
Dim b As String = String.Copy(a) ' New string
```



## XPL0

The default method of terminating strings is to set the most significant
bit of the last character.
An alternative is to use the 'string 0' command to specify zero-terminated strings.
The string copy routine from the standard library is shown.


```XPL0
proc StrCopy(A, B);     \Copy string: A --> B
char A, B;              \Strings: B must already have enough space "Reserved"
int  I;                 \Beware if strings overlap
for I:= 0 to -1>>1-1 do
    [B(I):= A(I);
    if A(I) >= $80 then return
    ];

char S1, S2, S3(13);
[S1:= "Hello, world!";  \S1 now points to the string
S2:= S1;                \S2 now also points to the string
StrCopy(S1, S3);        \S3 points to a separate copy of the string
]
```



## zkl

Strings are immutable so copy is just return the string:

```zkl
"abc".copy()  // noop
```



## zonnon


```zonnon

module Main;
var
	s,r: string;
	c: array 60 of char;
begin
	s := "plain string";r := s; writeln(s);
	(* copy string to array of char *)
	copy(s,c);c[0] := 'P';
	(* copy array of char to string *)
	copy(c,r);writeln(r);
end Main.

```


## ZX Spectrum Basic



```basic
10 LET a$ = "Hello": REM a$ is the original string
20 LET b$ = a$: REM b$ is the copy
```

