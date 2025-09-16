+++
title = "Address of a variable"
description = ""
date = 2019-10-07T04:08:47Z
aliases = []
[extra]
id = 1985
[taxonomies]
categories = ["Basic Data Operations", "task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "applesoft_basic",
  "argile",
  "arm_assembly",
  "astro",
  "autohotkey",
  "axe",
  "basic",
  "bbc_basic",
  "cobol",
  "common_lisp",
  "component_pascal",
  "creative_basic",
  "csharp",
  "d",
  "delphi",
  "erre",
  "fbsl",
  "forth",
  "fortran",
  "freebasic",
  "futurebasic",
  "go",
  "java",
  "julia",
  "kotlin",
  "maple",
  "newlisp",
  "nim",
  "ocaml",
  "oforth",
  "ol",
  "oorexx",
  "oxygenbasic",
  "panoramic",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powerbasic",
  "purebasic",
  "python",
  "racket",
  "rapidq",
  "retro",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "smalltalk",
  "stata",
  "swift",
  "tcl",
  "toka",
  "vba",
  "visual_basic_.net",
  "wart",
  "x86_assembly",
  "xlisp",
  "xpl0",
  "yorick",
]
+++

## Task

### Task:
Demonstrate how to get the address of a variable and how to set the address of a variable.





## 360 Assembly


### Get the Address

To get the address of a variable, use <code>LA</code> (load address) instead of <code>L</code> (load):

```360asm
         LA    R3,I                load address of I
...
I        DS    F
```



### Set the Address

To set a variable dynamically at the same address of an other variable, use a <code>DSECT</code> (dummy section):

```360asm
         USING MYDSECT,R12
         LA    R12,I               set @J=@I
         L     R2,J                now J is at the same location as I
...
I        DS    F
MYDSECT  DSECT
J        DS    F
```



## Ada


### Get The Address


```ada
The_Address : System.Address;
I : Integer;
The_Address := I'Address;
```


### Set The Address

Set the address of a variable to address A100 in hexadecimal

```ada
I : Integer;
for I'Address use 16#A100#;
```

Set the address of one variable to the address of another variable, creating an overlay.

```ada
I : Integer;
J : Integer;
for I'Address use J'Address;
```

{{omit from|Lua}}


## ALGOL 68

Basically ALGOL 68 refuses to let the programmer access the memory directly.
The language does  allow "references" any variables.
These references are effectively the address a particular variable.
But the value of the actual address is not available
for printing or any arithmetic.

```algol68
[4]INT test := (222,444,666,888);
REF INT reference := test[3];
REF INT(reference) := reference + 111;
print(("test value is now: ",test))
```

{{out}}

```txt
test value is now:        +222       +444       +777       +888
```


The other reason specific addresses are using in languages like [[C]]
to manipulate devices.
For this purpose site are expected to implement channels
for their programmers to use.
To quote the ALGOL 68 Revised Report: <i>A "channel" corresponds to one
or more physical devices (e.g., a card reader, a card punch or
a line printer, or even to a set up in nuclear physics
the results of which are collected by the computer),
or to a filestore maintained by the operating system</i>[http://www.xs4all.nl/~jmvdveer/report_5.html#A312aa].

To establish a channel with such a device there is a special standard procedure:
```algol68
PROC establish = (REF FILE file, STRING idf, CHANNEL chan, INT p, l, c) INT: ~
```

Where the <tt>idf</tt> string is text describing which device to open, and possibly
options.  And <tt>chan</tt> is the actual device type.  Standard CHANNELs in
ALGOL 68 are <tt>stand in channel</tt>, <tt>stand out channel</tt>, and <tt>stand
back channel</tt>.  These determine the type of the pre opened stdio FILEs <tt>stand in</tt>,
<tt>stand out</tt>, and <tt>stand back</tt>. A site would be expected to
implement their own CHANNELs for network connections, database queries and particle accelerators etc.


## Argile


###  Get the address

{{works with|Argile|1.0.0}}

```Argile
use std, array		(: array.arg also defines pointer operators :)
let var = 42
let ptr = &var		(: value of ptr is address of var :)
print var		(: prints 42 :)
(*ptr)++		(: increments value pointed by ptr :)
print var		(: prints 43 :)
```


###  Set the address

Since we cannot set address of a variable, we use a macro
that returns a reference.
{{works with|Argile|1.0.0}}

```Argile
use std, array
=:mac:= -> int& { * (0x400000 as int*) }
printf "%x\n" mac	(: may crash depending on operating system :)

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program adrvar.s   */

/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1
/* Initialized data */
.data
szMessage:      .asciz "Hello. \n "       @ message
szRetourLigne:  .asciz "\n"
iValDeb:        .int 5      @ value 5 in array of 4 bytes
/* No Initialized data */
.bss
iValeur:     .skip  4     @ reserve 4 bytes in memory

.text
.global main
main:
        ldr r0,=szMessage          @ adresse of message  short program
	bl affichageMess            @ call function
	                                @  or
	ldr r0,iAdrszMessage         @ adresse of message big program (size code > 4K)
        bl affichageMess            @ call function

	ldr r1,=iValDeb              @ adresse of variable -> r1  short program
	ldr r0,[r1]                    @ value of iValdeb  -> r0
	ldr r1,iAdriValDeb           @ adresse of variable -> r1  big program
	ldr r0,[r1]                    @ value of iValdeb  -> r0


	/* set variables  */
	ldr r1,=iValeur               @ adresse of variable -> r1  short program
	str r0,[r1]                     @ value of r0 ->  iValeur
	ldr r1,iAdriValeur           @ adresse of variable -> r1  big program
	str r0,[r1]                     @ value of r0 ->  iValeur


 /* end of  program */
    mov r0, #0                  @ return code
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdriValDeb:  .int iValDeb
iAdriValeur:  .int iValeur
iAdrszMessage: .int szMessage
iAdrszRetourLigne: .int szRetourLigne
/******************************************************************/
/*     affichage des messages   avec calcul longueur              */
/******************************************************************/
/* r0 contient l adresse du message */
affichageMess:
	push {fp,lr}    /* save des  2 registres */
	push {r0,r1,r2,r7}    /* save des autres registres */
	mov r2,#0   /* compteur longueur */
1:	      /*calcul de la longueur */
    ldrb r1,[r0,r2]  /* recup octet position debut + indice */
	cmp r1,#0       /* si 0 c est fini */
	beq 1f
	add r2,r2,#1   /* sinon on ajoute 1 */
	b 1b
1:	/* donc ici r2 contient la longueur du message */
	mov r1,r0        /* adresse du message en r1 */
	mov r0,#STDOUT      /* code pour écrire sur la sortie standard Linux */
    mov r7, #WRITE                  /* code de l appel systeme 'write' */
    swi #0                      /* appel systeme */
	pop {r0,r1,r2,r7}     /* restaur des autres registres */
	pop {fp,lr}    /* restaur des  2 registres */
    bx lr	        /* retour procedure */


```



## Astro


```python
var num = 12
var pointer = ptr(num) # get pointer

print pointer # print address

@unsafe # bad idea!
pointer.addr = 0xFFFE # set the address

```



## AutoHotkey

Getting or setting the address of a variable is not supported as a builtin function.
However, you can get the address of contents pointed to by the variable structure var

```AutoHotkey
msgbox % &var
```



## Axe


Axe supports getting the address of a variable using the degree symbol:

```axe
°A→B
.B now contains the address of A
```


Axe does not support setting the address of a variable directly. However, it does support setting the value of a variable and then dereferencing it:

```axe
1234→A
1→{A}
```

This should usually be avoided because TI-OS does not use virtual memory. Writing to arbitrary memory locations can affect the stability of the operating system.


## BASIC

Many BASICs, especially older flavors like [[QuickBASIC]], lack the ability to set a variable's address (and indeed, they pretty much all lack the ability to work with pointers in any fashion).

```qbasic
'get a variable's address:
DIM x AS INTEGER, y AS LONG
y = VARPTR(x)

'can't set the address, but can access a given memory location... 1 byte at a time
DIM z AS INTEGER
z = PEEK(y)
z = z + (PEEK(y) * 256)
```



## Applesoft BASIC



###  Get the address

Get the address of the last variable used.

```ApplesoftBasic
N = N : PRINT PEEK (131) + PEEK (132) * 256
```

Get (find) the address of a function.

```ApplesoftBasic
0 DEF FN F(X) = 0
1 FOR A = PEEK(105) + PEEK(106) * 256 TO PEEK(107) + PEEK(108) * 256 STEP 7 : IF PEEK(A) <> ASC("F") + 128 OR PEEK(A + 1) <> 0 THEN NEXT A : A = 0 : PRINT "FN F NOT FOUND"
2 IF A THEN PRINT A

```



###  Set the address

Set the address where variables are stored, but clears all variables.

```ApplesoftBasic
LOMEM: 4096
I% = I% : PRINT PEEK (131) + PEEK (132) * 256

```

Set the address and length of a string.

```ApplesoftBasic
S$ = "HELLO" : POKE 768, PEEK (131) : POKE 769, PEEK (132) : A = PEEK(768) + PEEK(769) * 256
PRINT S$ : PRINT A "- " PEEK(A) " " PEEK(A + 1) + PEEK(A + 2) * 256
POKE 768, ASC("H") : POKE 769, ASC("I") : POKE A, 2: POKE A + 1, 0 : POKE A + 2, 3
PRINT S$ : PRINT A "- " PEEK(A) " " PEEK(A + 1) + PEEK(A + 2) * 256

```

Set the address of a function.

```ApplesoftBasic
0 DEF FN F(X) = 1
1 DEF FN B(X) = 2
2 N$ = "F" : GOSUB 8 : FA = A
3 N$ = "B" : GOSUB 8 : BA = A
4 PRINT FN F(0)
5 POKE FA, PEEK(BA) : POKE FA + 1, PEEK(BA + 1)
6 PRINT FN F(0)
7 END
8 FOR A = PEEK(105) + PEEK(106) * 256 TO PEEK(107) + PEEK(108) * 256 STEP 7 : IF PEEK(A) = ASC(LEFT$(N$,1)) + 128 AND PEEK(A + 1) = ASC(MID$(N$ + CHR$(0), 2, 1)) THEN A = A + 2 : RETURN
9 NEXT A : PRINT "FN " N$ " NOT FOUND"

```



## BBC BASIC

The original BBC BASIC doesn't provide an address-of operator, but ''BBC BASIC for Windows'' does:

```bbcbasic
REM get a variable's address:
y% = ^x%

REM can't set a variable's address, but can access a given memory location (4 bytes):
x% = !y%
```


With BBC BASIC on other platforms the address of a variable can be found by calling a short piece of machine code, see [http://beebwiki.mdfs.net/Address_of_a_variable BeebWiki].


## C#


Use of pointers in C# is restricted to <code>unsafe</code> sections of code, which is enabled in Microsoft's C# compiler with the commandline parameter <code>/unsafe</code> or in Mono's C# compiler with <code>-unsafe</code> (or <code>--unsafe</code> in older versions).


###  Get the address

Note that void* is a "pure" address which doesn't carry the type information anymore. If you need the type information (e.g. to recover the variable itself in a type safe manner), use a pointer to the appropriate type instead; in this case int*.


```c#
unsafe
{
  int i = 5;
  void* address_of_i = &i;
}
```


=={{header|C}} / {{header|C++}}==

{{works with|gcc}} {{works with|g++}}

###  Get the address

Note that <code>void*</code> is a "pure" address which doesn't carry the type information anymore. If you need the type information (e.g. to recover the variable itself in a type safe manner), use a pointer to the appropriate type instead; in this case <code>int*</code>.

```cpp
int i;
void* address_of_i = &i;
```


'''C++ only:''' C++ allows overloading the <code>&</code> operator. To bypass this, for example in generic code, use the library function <code>addressof</code>:


```cpp
#include <memory>
int i;
auto address_of_i = std::addressof(i);
```



###  Set the address

While C++ doesn't directly support putting a variable at a given address, the same effect can be achieved by creating a reference to that address:

```cpp
int& i = *(int*)0xA100;
```


If the type of the variable requires initialization, it is necessary to use placement new:

```cpp
#include <new>

struct S { int i = 0; S() {} };
auto& s = *new (reinterpret_cast<void*>(0xa100)) S;
```


Overlaying of variables is done with anonymous unions; however at global/namespace scope such variables have to be static (i.e. local to the current file):

```cpp
static union
{
  int i;
  int j;
};
```

'''C++ only:''' An alternative (and cleaner) solution is to use references:

```cpp
int i;
int& j = i;
```

Note that in this case, the variables can be non-static.

If the type of two overlaid variables is not sufficiently similar, then writes to one may not be reflected in reads from the other, even though they have the same address. This is because the optimizer is free to assume that variables of different types do not alias. To read or write a variable as a different type, use <code>memcpy</code>:


```cpp
#include <cstring>
inline float read_as_float(int const& i) { float f; memcpy(&f, &i, sizeof(f)); return f; }
int i = 0x0a112233;
float f = read_as_float(i);
```


{{omit from|Clojure}}


## COBOL

Pointers were added in COBOL 2002.


### Get Address


```cobol
data division.
working-storage section.
01 ptr usage pointer.
01 var pic x(64).

procedure division.
set ptr to address of var.
```



### Set Address

Sets the address of a variable using the <code>BASED</code> clause. There are other methods, in particular <code>LINKAGE SECTION</code> variables.

```cobol

OCOBOL*> Rosetta Code set address example
      *> tectonics: cobc -x setaddr.cob && ./setaddr
       program-id. setaddr.
       data division.
       working-storage section.
       01 prealloc  pic x(8) value 'somedata'.
       01 var       pic x(8) based.
       procedure division.
       set address of var to address of prealloc
       display var end-display
       goback.
       end program setaddr.
```



## Common Lisp



### Simulated Address of Variable


Common Lisp has no means to take a simple address reference to a ''place'' (the Lisp term for any one of many types of assignable storage locations). For instance, to access the slot of a structure, we need the structure itself (which is essentially a pointer) and a symbol denoting the slot name. To access an array, we need that array object, and an index. To access a local variable, we need the closure object, which is only accessible to code within the lexical scope, which is a long-winded way of saying, we can only access a variable via an expression which is in the lexical scope of the variable.

Yet, thanks to Lisp macros and lexical closures, we can create reference values which behave like address of places. A tiny module for doing this is found in <strike>[http://paste.lisp.org/display/71952 Lisppaste #71952]</strike> (now it's available [http://www.kylheku.com/cgit/lisp-snippets/tree/refs.lisp here]), required by the following example:


```lisp
;;; Demonstration of references by swapping two variables using a function rather than a macro
;;; Needs http://paste.lisp.org/display/71952
(defun swap (ref-left ref-right)
  ;; without with-refs we would have to write this:
  ;; (psetf (deref ref-left) (deref ref-right)
  ;;        (deref ref-right) (deref ref-left))
  (with-refs ((l ref-left) (r ref-right))
    (psetf l r r l)))

(defvar *x* 42)
(defvar *y* 0)

(swap (ref *x*) (ref *y*))

;; *y* -> 42
;; *x* -> 0
```


These references are completely safe to use. There is no way that a place can disappear, leaving a reference dangling, because if a reference is a live object, it prevents the object which contains the referenced place from becoming garbage. Also note that if two references are taken to the same memory location, they are two distinct objects. A function could be provided to test two references for referential equality (do they point to the same object).

===Varible-Like Moniker for External Location===

{{works with|CLISP}} {{works with|Linux}} {{works with|Cygwin}}

What does it mean to "set the address of a variable?" One interpretation of this task is: rather than let the programming language compiler or run-time allocate a named storage location in its usual ways, force the allocation of a variable at some particular memory location, such as a hardware register, or a variable in a foreign library.
Lisp implementations have foreign function interfaces for doing this sort of thing.

Here is an example specific to CLISP running on either Linux or Cygwin. It creates a Lisp <code>errno</code> variable which is located in the C Library's <code>errno</code>.
The C Library's <code>errno</code> is actually thread-specific, whose location is retrieved by calling a hidden function.

We wrap this function with a Lisp foreign call which is properly annotated as returning a C pointer to int. When we call this function, we get an object that behaves like a reference to that location. All we need then is a macro which looks like a storage location.


```lisp
(use-package :ffi)

(defmacro def-libc-call-out (name &rest args)
  `(def-call-out ,name
     (:language :stdc)
     #-cygwin(:library "libc.so.6")
     #+cygwin (:library "cygwin1.dll")
     ,@args))

(progn
  (def-libc-call-out errno-location
    #-cygwin (:name "__errno_location")
    #+cygwin (:name "__errno")
    (:arguments)
    (:return-type (c-pointer int)))

  (defun get-errno ()
    (let ((loc (errno-location)))
      (foreign-value loc)))

  (defun set-errno (value)
    (let ((loc (errno-location)))
      (setf (foreign-value loc) value)))

  (defsetf get-errno set-errno)

  (define-symbol-macro errno (get-errno)))
```


Test:


```txt
[1]> (setf errno 42)
42
[2]> errno
0   ;; Oops! the REPL itself executed a bunch of code which cleared errno
[3]> (progn (delete-file "nonexistent") errno)
2   ;; Aha! 2 is ENOENT: No such file or directory
```



## Creative Basic


```Creative Basic

== Get ==

To get the address of a variable without using the Windows API:

DEF X:INT
DEF pPointer:POINTER
pPointer=X

----

To get the address of a variable using the Windows API Lstrcpy function called in Creative Basic:
(This may give users of another language without a native way to get the address of a variable to work around that problem.)

DEF Win:WINDOW
DEF Close:CHAR
DEF ScreenSizeX,ScreenSizeY,Col:INT

'***Map Function***
DECLARE "Kernel32",Lstrcpy(P1:POINTER,P2:POINTER),INT
'The pointers replace the VB3 variable type of Any.

'Note: This is translated from VB3 or earlier code, and "Ptr" is *not* a Creative Basic pointer.
DEF Ptr:INT
DEF X1:INT
DEF X2:STRING

X1=123

'***Call function***
Ptr=Lstrcpy(X1,X1)

GETSCREENSIZE(ScreenSizeX,ScreenSizeY)

WINDOW Win,0,0,ScreenSizeX,ScreenSizeY,@MINBOX|@MAXBOX|@SIZE|@MAXIMIZED,0,"Skel Win",MainHandler

'***Display address***
PRINT Win, "The address of x1 is: " + Hex$(Ptr)
      X2="X2"

WAITUNTIL Close=1

CLOSEWINDOW Win

END

SUB MainHandler

	SELECT @CLASS

	CASE @IDCLOSEWINDOW

	Close=1

	ENDSELECT

RETURN

Note: The Windows Dev Center (http://msdn.microsoft.com/en-us/library/windows/desktop/ms647490%28v=vs.85%29.aspx) says
improper use of the Lstrcpy function may compromise security. A person is advised to see the Windows Dev site before using
the Lstrcopy function.

== Set ==

It appears to the author the closest one can come to setting the address of a variable is to set which bytes will be
used to store a variable in a reserved block of memory:

DEF pMem as POINTER
pMem = NEW(CHAR,1000) : 'Get 1000 bytes to play with
#<STRING>pMem = "Copy a string into memory"
pMem += 100
#<UINT>pMem = 34234: 'Use bytes 100-103 to store a UINT
DELETE pMem

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE AddressVar;
IMPORT SYSTEM,StdLog;

VAR
	x: INTEGER;

PROCEDURE Do*;
BEGIN
	StdLog.String("ADR(x):> ");StdLog.IntForm(SYSTEM.ADR(x),StdLog.hexadecimal,8,'0',TRUE);StdLog.Ln
END Do;

BEGIN
	x := 10;
END AddressVar.

```

Execute: ^Q AddressVar.Do<br/>
{{out}}

```txt

ADR(x):> 653700D8H

```


## D


Take the address of a variable:

```d
int i;
int* ip = &i;
```


Using a numeric value:

```d
int* ip = cast(int*)0xdeadf00d;
```


Locating a "regular" variable at a specific address is not possible.

The closest thing is passing a dereferenced pointer to a reference parameter.


```d
void test(ref int i) {
    import std.stdio;
    writeln(&i);
}

void main() {
    test(* (cast(int*)0xdeadf00d) );
}
```



## Delphi


Turbo/Borland Pascal and Delphi (Object Pascal) support the <tt>@</tt> (address of) operator and the <tt>var: [type] absolute</tt> declaration.

To get the address of any variable, structure, procedure or function use the <tt>@</tt>-operator:

```pascal
var
	i: integer;
	p: ^integer;
begin
	p := @i;
	writeLn(p^);
end;
```

Note, (untyped) constants do not have an address in Pascal.

A variable can be declared as <tt>absolute</tt> i. e.: to reside at a specific address:

```pascal
var
	crtMode: integer absolute $0040;
	str: string[100];
	strLen: byte absolute str;

```



## ERRE

ERRE hasn't explicit pointers, but only a function that gets the address of a variable
<lang>
........
A%=100
ADDR=VARPTR(A%)
.......

```

ADDR contains the value of the address of variable A (from 0 to 65535 because every ERRE module has a 64K address space). ERRE data types is 2 bytes-long for integer, 4 (5 with C-64) for reals and 8 for double-real variables.
Using this address you can modify the variable's value without using an assignment statement:
<lang>
PROGRAM POINTER
BEGIN
   A%=100
   ADDR=VARPTR(A%)
   PRINT(A%)       ! prints 100
   POKE(ADDR,200)
   PRINT(A%)       ! prints 200
END PROGRAM

```

Note: With C-64 substitute POKE(ADDR,200) with POKE(ADDR+3,200).


## FBSL


Unlike in other BASICs, pointers are readily available in FBSL in several ways although C-style pointer arithmetics is not supported.

1. A simple Variant variable may be dereferenced to point to any other FBSL entity from simple variables through arrays to class instances and COM objects with a ReferenceOf operator, e.g.

    ReferenceOf a = b

whereby a is set to refer to the variable b.

2. Non-function entities' pointers can be both retrieved and set using a PointerOf operator which also has a shorthand alias @, e.g.

    @a = @b

whereby a is set to refer to the variable b. Assignment through the PointerOf operator also accepts integer literals to set the variable's absolute address to.

3. Function pointers can be both retrieved and set using an AddressOf operator as in the following sample script. Assignment via AddressOf also accepts integer literals to set a function pointer to an absolute address.

    #APPTYPE CONSOLE

    SUB Foo()
        PRINT "foo"
    END SUB

    SUB Bar()
        PRINT "bar"
    END SUB

    ADDRESSOF Foo = ADDRESSOF Bar

    Foo() // prints "bar" to the console

    PAUSE

4. Values at an absolute address or address read by a PointerOf operator plus a byte offset can be retrieved or set using the Peek()/Poke() and GetMem()/SetMem() functions.


## Forth

Variables and created memory blocks return their address when referenced. The "fetch" operator '''@''' could also be pronounced "dereference".
 variable foo
 foo .  \ some large number, an address
 8 foo !
 foo @ .  \ 8

You can define a constant or value with an address, which then acts like a variable.
This can be used to refer to fixed addresses (such as I/O ports), graphics buffers, or allocated memory.
 $3F8 constant LPT1:
 8 LPT1: !

 100 cells allocate throw value buffer
 42 buffer 20 cells + !

## Fortran

{{works with|Fortran|90 and later}}

```fortran
program test_loc

  implicit none
  integer :: i
  real :: r

  i = loc (r)
  write (*, '(i0)') i

end program test_loc
```

Note: <code>loc</code> is a common extension that is implemented
by e.g. the Intel Fortran Compiler, G95 and gfortran.


## FreeBASIC

One can get the address of a variable using the @ operator:

```freebasic
' FB 1.05.0 Win64
Dim a As Integer = 3
Dim p As Integer Ptr = @a
Print a, p
```

To my knowledge, it is not possible to set the address of a variable to a specific address in FB though (as in C/C++) you can do something like this as a workaround:

```freebasic
Var p = Cast(Integer Ptr, 1375832)
*p = 42
Print p, *p
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as short i : i = 575
dim as ptr j : j = NULL

j = @i

print "Adddress of i ="; j
print "Value of i ="; [j]

```


Output:

```txt

Adddress of i = 902164
Value of i = 575

```



## Go

Go has pointers. Just like in C, you can "take the address" of an addressable value by using the <code>&</code> operator, and access the value pointed to by a pointer using the <code>*</code> operator.

Unlike in C, pointers are not readily convertible to integers and
there is no direct pointer arithmetic.
You may print out the address of a pointer, either using the Print function
on the pointer, or using the <code>%p</code> format specifier
in formatted output (just like in C).

When rarely required, you can convert a pointer to an integer using the [https://godoc.org/pkg/unsafe unsafe package].

It is not possible in Go to set the address of a variable,
however you can assign an arbitrary value to a pointer and
use/deference that pointer.

The following demonstrates getting the address of a variable and storing/printing it various ways.
It also demonstrates accessing an arbitrary memory location (here the known address of a float) as an integer.

```go
package main

import (
	"fmt"
	"unsafe"
)

func main() {
	myVar := 3.14
	myPointer := &myVar
	fmt.Println("Address:", myPointer, &myVar)
	fmt.Printf("Address: %p %p\n", myPointer, &myVar)

	var addr64 int64
	var addr32 int32
	ptr := unsafe.Pointer(myPointer)
	if unsafe.Sizeof(ptr) <= unsafe.Sizeof(addr64) {
		addr64 = int64(uintptr(ptr))
		fmt.Printf("Pointer stored in   int64: %#016x\n", addr64)
	}
	if unsafe.Sizeof(ptr) <= unsafe.Sizeof(addr32) {
		// Only runs on architectures where a pointer is <= 32 bits
		addr32 = int32(uintptr(ptr))
		fmt.Printf("Pointer stored in   int32: %#08x\n", addr32)
	}
	addr := uintptr(ptr)
	fmt.Printf("Pointer stored in uintptr: %#08x\n", addr)

	fmt.Println("value as float:", myVar)
	i := (*int32)(unsafe.Pointer(&myVar))
	fmt.Printf("value as int32: %#08x\n", *i)
}
```

{{out}}
On a 32 bit architecture:

```txt
Address: 0x3826c020 0x3826c020
Address: 0x3826c020 0x3826c020
Pointer stored in   int64: 0x000000003826c020
Pointer stored in   int32: 0x3826c020
Pointer stored in uintptr: 0x3826c020
value as float: 3.14
value as int32: 0x51eb851f
```

On a 64 bit architecture:

```txt
Address: 0xc208000170 0xc208000170
Address: 0xc208000170 0xc208000170
Pointer stored in   int64: 0x000000c208000170
Pointer stored in uintptr: 0xc208000170
value as float: 3.14
value as int32: 0x51eb851f
```


==IWBASIC==

```IWBASIC

== Get ==

There are at least three ways to get the address of a variable in IWBASIC. The first is to use the address of operator:

DEF X:INT
PRINT &X
'This will print in the console window (after OPENCONSOLE is issued.)
'To Print in an open window the appropriate Window variable is specified, e.g., PRINT Win,&X.

The second is to use a pointer:

DEF X:INT
DEF pPointer:POINTER
pPointer=X

The third is to use the Windows API function Lstrcpy. That is done in the same way as the Creative Basic example;
however, the function would be declared as follows: DECLARE IMPORT,Lstrcpy(P1:POINTER,P2:POINTER),INT.

== Set ==

It appears to the author that the closest one can come to being able to assign an address to a variable is to set
which bytes will be used to store a variable in a block of reserved memory:

DEF pMem as POINTER
pMem = NEW(CHAR,1000) : 'Get 1000 bytes to play with
#<STRING>pMem = "Copy a string into memory"
pMem += 100
#<UINT>pMem = 34234: 'Use bytes 100-103 to store a UINT
DELETE pMem

```


== {{header|J}} ==

J hides the details of pointers and memory allocation from the programmer, so it is rarely, if ever, necessary to do this.  However, for those times when there is no better substitute, J provides access to these low-level details:

<lang>   var      =: 52                NB.  Any variable (including data, functions, operators etc)
   var_addr =: 15!:6<'var'       NB.  Get address
   new_var  =: 15!:7 var_addr    NB.  Set address
```



## Java


There is no way to access addresses of variables in Java. However, the [http://download.oracle.com/javase/6/docs/api/java/lang/Object.html#hashCode%28%29 default hashCode()] method defined in the Object class, "is typically implemented by converting the internal address of the object into an integer". Therefore, in some Java implementations at least, the hash code returned by <code>Object.hashCode()</code> reflects at least part of the address of an object. For objects whose classes have overridden the <code>hashCode()</code> method, you can still access the original hash code through the [http://download.oracle.com/javase/6/docs/api/java/lang/System.html#identityHashCode%28java.lang.Object%29 System.identityHashCode()] function.


## Julia


Julia provides a variety of ways to work with and manipulate raw address pointers (via the built-in <code>Ptr</code> type), which are mostly used in practice for calling C library functions.

Julia has both mutable and immutable objects.  Immutable objects are values, such as constants, bit-based values such as numeric 3, or immutable structs. Immutable objects may not have a single set location in memory, but instead might in some cases be created on demand by the compiled code. Mutable objects such as typical arrays and mutable structs, on the other hand, have addresses on the Julia heap that can be found with specific base Julia functions which use the <code>Ptr</code> type.

To get the memory address of a Julia object, one can use <code>pointer_from_objref(object)</code>, and the reverse is accomplished by <code>unsafe_pointer_to_objref(ptr)</code>:
```julia
julia
 x = [1, 2, 3]
julia> ptr = pointer_from_objref(x)
Ptr{Void} @0x000000010282e4a0
julia> unsafe_pointer_to_objref(ptr)
3-element Array{Int64,1}:
 1
 2
 3
```
  The latter is "unsafe" because it only works if <code>ptr</code> refers to a valid heap-allocated "boxed" Julia object, which can only be safely allocated by Julia itself.

<p>Another common use of pointers is for arrays of values, which are typically passed in low-level C-like libraries via pointers to contiguous sets of values in memory.  This is accomplished in Julia by the <code>pointer(A)</code> function, which returns a pointer to the data stored in a high-level Julia array <code>A</code>.  Given a pointer <code>p</code> to values of a given type, the <code>i</code>-th value (numbered starting at 1 for the value pointed to by <code>p</code>) can be read or written by the low-level <code>unsafe_load(p, i)</code> and <code>unsafe_store!(p, val, i)</code> functions, or it can be converted back to a high-level Julia array type by the <code>pointer_to_array(p, dimensions)</code> function:
```julia
julia
 A = [1, 2.3, 4]
3-element Array{Float64,1}:
 1.0
 2.3
 4.0

julia> p = pointer(A)
Ptr{Float64} @0x0000000113f70d60

julia> unsafe_load(p, 3)
4.0

julia> unsafe_store!(p, 3.14159, 3)
julia> A
3-element Array{Float64,1}:
 1.0
 2.3
 3.14149

julia> pointer_to_array(p, (3,))
3-element Array{Float64,1}:
 1.0
 2.3
 3.14149
```


Finally, an arbitrary integer can be converted to a pointer type with <code>convert</code>, which allows an arbitrary address to be converted into and viewed as an array of an arbitrary type and read or written (although this can easily result in a crash if an invalid address is used).   In the following example, we create a "new" length-two array <code>B</code> at an address offset by 8 bytes from the address of the data in <code>A</code> above, which will make it point to the second element of <code>A</code>:
```julia
julia

julia> q = convert(Ptr{Float64}, 0x0000000113f70d68)
Ptr{Float64} @0x0000000113f70d68

julia> B = pointer_to_array(q, (2,))
2-element Array{Float64,1}:
 2.3
 3.14149
```


{{omit from|K}}


## Kotlin

Kotlin/JVM does not support pointers and so (apart from some jiggery-pokery with the sun.misc.Unsafe class) there is no way to obtain a variable's address.

However, Kotlin/Native which is currently (January 2018) available as a pre-release version does support pointers to enable it to interoperate with C code. The following program shows how to obtain the address of a variable which has been allocated on the native heap. It does not appear to be possible to allocate a variable at a particular address.
{{works with|Ubuntu|14.04}}

```scala
// Kotlin Native v0.5

import kotlinx.cinterop.*

fun main(args: Array<String>) {
    val intVar = nativeHeap.alloc<IntVar>()
    intVar.value = 42
    with(intVar) { println("Value is $value, address is $rawPtr") }
    nativeHeap.free(intVar)
}
```


{{out}}
Sample output:

```txt

Value is 42, address is 0xc149f0

```



## Maple


To obtain the address of any expression in Maple, use the builtin function <code>addressof</code>.

```maple

 addressof( x );
                              18446884674469911422
```
The inverse operation is <code>pointto</code>:
```Maple

> pointto( 18446884674469911422 );
                                       x
```
This works for any expression, not just variables:
```maple

 addressof( sin( x )^2 + cos( x )^2 );
                              18446884674469972158

> pointto( 18446884674469972158 );
                                     2         2
                               sin(x)  + cos(x)
```


=={{header|Modula-2}}==

### Get Address


```modula2
MODULE  GetAddress;

FROM    SYSTEM IMPORT ADR;
FROM    InOut  IMPORT WriteInt, WriteLn;

VAR     var : INTEGER;
        adr : LONGINT;
BEGIN
    adr := ADR(var);    (*get the address*)
    WriteInt(adr, 0);
    WriteLn;
END GetAddress.
```



### Set Address


```modula2
MODULE  SetAddress;

CONST   adress  = 134664460;

VAR     var [adress] : INTEGER;

BEGIN
    (*do nothing*)
END SetAddress.
```


## NewLISP


### Get Address


```NewLISP

(set 'a '(1 2 3))
(address a)

```



## Nim


```nim
var x = 12
var xptr = addr(x) # Get address of variable
echo cast[int](xptr) # and print it
xptr = cast[ptr int](0xFFFE) # Set the address
```


=={{header|Oberon-2}}==

### Get Address


```txt

VAR a: LONGINT;
VAR b: INTEGER;

b := 10;
a := SYSTEM.ADR(b); (* Sets variable a to the address of variable b *)

```



### Set Address


```txt

SYSTEM.PUT(a, b); (* Sets the address of b to the address of a *)

```



## OCaml


OCaml is a high-level programming language, and thus does not expose the addresses of variables to the programmer.

However, it is kind of possible to get the address of the structure pointed to by a reference type. At runtime, every value in OCaml is a pointer to a "block" structure (i.e. boxed) except for <code>int</code> (and things that can fit inside an <code>int</code>, like <code>char</code>, <code>bool</code>, <code>None</code>, datatypes with all no-arg constructors, etc.) which is a direct value (unboxed). For boxed types, it is possible to get this address by some tricky hacks.

An OCaml value is distinguished between an unboxed integer and a boxed type by the last (least significant) bit. If the bit is set (1), then the value is an integer, and the rest of the bits are the bits of the integer, shifted one bit to the left (that's why <code>int</code> in OCaml can only represent 31 bits in 32-bit, and 63 bits in 64-bit). If the last bit is cleared (0), then the value is a pointer, and all the bits (including the last one) form the address.

To get the address, we re-interpret the boxed value as an integer; however, this will get the address divided by 2, since the integer only uses the upper 31 (or 63) bits. Therefore, we need to shift this number left by one to get the real address. However, <code>int</code> cannot hold all the bits of the address, so if we shift we will lose a bit, so we use the <code>nativeint</code> type to represent it instead:


```ocaml
let address_of (x:'a) : nativeint =
  if Obj.is_block (Obj.repr x) then
    Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1 (* magic *)
  else
    invalid_arg "Can only find address of boxed values.";;

let () =
  let a = 3.14 in
  Printf.printf "%nx\n" (address_of a);;
  let b = ref 42 in
  Printf.printf "%nx\n" (address_of b);;
  let c = 17 in
  Printf.printf "%nx\n" (address_of c);; (* error, because int is unboxed *)
```



## Ol

Otus Lisp has no means to take a simple address reference to a place (the Lisp term for any one of many types of assignable storage locations). It can be done only using the extension mechanism (ffi), but in any case this information can't be useful due to Garbage Collection (at any time the variable can be moved to other place that automatically makes invalid the got reference pointer).


## ooRexx

ooRexx is a high-level programming language, and thus does not expose the addresses of variables to the programmer.  Variables can be accessed by name using the VAR(), SYMBOL(), and VALUE() built-in functions.


## Oforth

In Oforth, addresses are not exposed.

Variables (like all Oforth metamodel) are objects.
It is possible to retrieve the object corresponding to a variable and use #at and #put to change its value.

For instance, here, after creating and setting a variable A, we store the corresponding word into a variable B :


```Oforth
tvar: A
10 to A

tvar: B
#A to B
B .s
[1] (Variable) #A
>ok

12 B put
A .s
[1] (Integer) 12
[2] (Variable) #A
>ok
```



## OxygenBasic


```txt


'GETTING ADDRESS OF VARIABLE

int a=1,b=2,c=3
print "Adrress of b: " @b


'SETTING ADDRESS OF INDIRECT (BYREF) VARIABLE

int *aa,*bb,*cc

@bb=@b 'setting address of bb to address of b

print "Value of bb: " bb 'result: 2


```


## Panoramic


```Panoramic

== Get ==

adr(variable)

Example:

dim a
print adr(a)

== Set ==

Whether Panoramic is able to set the value of a variable may depend on what is meant by that. Panoramic implements the
poke command to set a byte from a value of 0 to 255 (inclusive). Panoramic also implements the peek command to get
the value of a byte, so it is possible to the following:

(A)
dim a
rem a variable with no post-fix is a real.
poke adr(a),57
rem the value of a variable being set by setting an address, the address of a in this instance.

(B)
dim a%,b%
rem % means integer.
b%=57
poke adr(a%),b%
rem b% being assigned to the address of a%, in this instance.
rem it is even possible to free b%
free b%
print a%

(C)
dim a,b
b=57
poke adr(a),b
b=peek(adr(a))
print b
rem the address of b being, in effect, set to the address of a, the address of a, in this instance.

rem Observations and further insight welcome.

''Note:'' An attempt to poke a real or an integer (Panoramic's only numeric types) value of less than 0 or of more than
255 will cause an error.

```



## PARI/GP

In GP you can sent the address to built-in commands like issquare

```parigp
issquare(n, &m)
```

but you cannot directly compute with it.  You can view the address of a variable and other debugging information with the

```txt
\x
```

command.

In PARI you can use standard [[#C_.2F_C.2B.2B|C]] commands.


## Pascal

See [[Address_of_a_variable#Delphi | Delphi]]


## Perl

To get the address, get the reference to a variable, and either stringify it, or use Scalar::Util's refaddr() to get just the address. Also see Devel::Peek.

```perl
use Scalar::Util qw(refaddr);
print refaddr(\my $v), "\n";  # 140502490125712
```

Alternatively, the address (in hexadecimal) can be directly obtained with <code>printf</code>:

```perl
printf "%p", $v; # 7fc949039590
```

Use Devel::Pointer::PP if you want to dereference a certain address in memory.

Changing the address of a variable is not easily possible, but see perlapi. Wanting to go against the automatic memory management is a sign that this is only used to hack around the deficiencies of dafter languages. I can imagine address munging is commonly used to make variable aliasing possible, but Perl already has a higher level syntax for that.

Simple reference (address) manipulation.

```perl
my $a = 12;
my $b = \$a; # get reference
$$b = $$b + 30; # access referenced value
print $a; # prints 42
```


Example how to make variable overlay.

```perl
my $a = 12;
our $b; # you can overlay only global variables (this line is only for strictness)
*b = \$a;
print $b; # prints 12
$b++;
print $a; # prints 13
```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
my $x;
say $x.WHERE;

my $y := $x;   # alias
say $y.WHERE;  # same address as $x

say "Same variable" if $y =:= $x;
$x = 42;
say $y;  # 42

```

{{out}}

```txt
7857931379550584425
```

How you set the address of a variable (or any other object) is outside the purview of the Perl 6 language, but Perl 6 supports pluggable object representations, and any given representation scheme could conceivably allow an existing address to be treated as an object candidate where that makes sense.  Memory-mapped structs are not unreasonable and are likely to be supported on VMs that allow it.


## Phix

Phix does not natively support pointers, but there are methods to do whatever you need.

You cannot set the address of a variable, but you can save it and use that to modify things.

Things get more complicated for floats/strings/sequences, particularly wrt reference counts,
but there are examples aplently, for pretty much everything, in the builtins/VM sources.
Since all hll variables are dword-aligned or better, a shr 2 loses no information and the
result can be safely stored in an integer, avoiding some nasty int/float conversions for
anything above #3FFFFFFF (on 32 bit, add another 8Fs on 64 bit).
Obviously extreme caution must be exercised, in the example below if you save the address
of V which is local to the address() procedure, and then exit said, you have a pointer to
memory that will be re-used for something completely different almost immediately.
Example is 32 and 64 bit compatible, which at this level needs twice the code, however the
compiler only omits the appropriate binary for the currently selected target architecture.
You can also use allocate/free with peek/poke to obtain similar effects.

```Phix
procedure address()
object V
integer addr4   -- stored /4 (assuming dword aligned, which it will be)
#ilASM{
    [32]
        lea eax,[V]
        shr eax,2
        mov [addr4],eax
    [64]
        lea rax,[V]
        shr rax,2
        mov [addr4],rax
    []
      }
    if machine_bits()=32 then
        poke4(addr4*4,123)
    elsif machine_bits()=64 then
        poke8(addr4*4,123)
    end if
    ?V
    if getc(0) then end if
end procedure

address()
```

{{out}}

```txt

123

```



## PicoLisp

The PicoLisp function '[http://software-lab.de/doc/refA.html#adr adr]' returns
the address of a variable. A variable may be either a symbol or a cons pair in
PicoLisp.

The returned address is a number representing an encoded pointer. For symbols,
it is a negative number, and for cons pairs a positive number. The same function
'adr' can then be used to convert that pointer back to the original object.

```PicoLisp
: (setq X 7)
-> 7

: (adr 'X)
-> -2985527269106

: (val (adr -2985527269106))
-> 7

: (set (adr -2985527269106) '(a b c))
-> (a b c)

: X
-> (a b c)
```



## PL/I



```PL/I

declare addr   builtin; /* retrieve address of a variable               */
declare ptradd builtin; /* pointer addition                             */
declare cstg   builtin; /* retrieve length of the storage of a variable */
declare hbound builtin; /* retrieve the number of elements in an array  */

declare p pointer;
declare i bin fixed(31) init(42);
p = addr(i); /* Obtain address of variable, stored in integer variable k */

/* how to read a string bit by bit - example for pointerAdd           */
/* we built a pointer (movingPointer), which will move through the    */
/*   storage of a variable (exampleTxt). attached to the pointer is   */
/*   an array of bits (movingBit) - this means wherever the pointer   */
/*   is pointing to, this will also be the position of the array.     */
/* only whole bytes can be addressed. to get down to the single bits, */
/*   an array of 8 bits is used.                                      */

declare exampleTxt    char(16) init('Hello MainFrame!);
declare movingPointer pointer;
declare movingBit(8)  bit(01) based(movingPointer);

declare walkOffset    bin fixed(31);
declare walkBit       bin fixed(31);

do walkOffset = 0 to cstg(exampleTxt)-1;
  movingPointer = ptradd(addr(exampleTxt, walkOffset);

  do walkBit = 1 to hbound(movingBit);
    put skip list( 'bit at Byte '  !!walkOffset
                 !!' and position '!!walkBit
                 !!' is '          !!movingBit(walkBit));
  end;
end;

```



## PowerBASIC


```powerbasic
'get a variable's address:
DIM x AS INTEGER, y AS LONG
y = VARPTR(x)

'can't set the address of a single variable, but can access memory locations
DIM z AS INTEGER
z = PEEK(INTEGER, y)

'or can do it one byte at a time
DIM zz(1) AS BYTE
zz(0) = PEEK(BYTE, y)
zz(1) = PEEK(BYTE, y + 1)
'(MAK creates an INTEGER, LONG, or QUAD out of the next smaller type)
z = MAK(INTEGER, zz(0), zz(1))

'*can* set the address of an array
DIM zzz(1) AS BYTE AT y
'zzz(0) = low byte of x, zzz(1) = high byte of x
```



## PureBasic

Get the address of a variable using the '@' operator.

```PureBasic
a.i = 5
MessageRequester("Address",Str(@a))
```



Set the address of a structured pointer. The pointer can be dereferenced to interact with it's data.  Ensure that there is access to the memory address that is assigned to the pointer (i.e. part of allocated memory).

```PureBasic
a.i = 5
*b.Integer = @a    ;set *b equal to the address of variable a
*c.Integer = $A100 ;set *c to point at memory location $A100 (in hex)


MessageRequester("Address",Str(*b)) ;display the address being pointed at by *b
MessageRequester("Value",Str(*b\i)) ;de-reference the pointer *b to display the data being pointed at
```



## Python


Python traditionally doesn't support low-level operations on memory addresses, except in the limited sense that one can use the ''mmap'' module where it's available, and manipulate offsets into memory map objects...including serializing other objects into and out of the memory mapping. New versions of Python support a ''ctypes'' module which permits some low level address operations on C-type objects (see [http://docs.python.org/lib/ctypes-ctypes-reference.html C-types Reference] for details).

The Python ''id()'' function returns a unique ID for any object. This just happens to be implemented as the base address of the object in C Python[http://docs.python.org/library/functions.html#id]; but that is not guaranteed by the semantics of the language and should not be considered a standard, nor used as such. But for comparison purposes the ID can be used as an address, since different extant objects will have different IDs.


```python
foo = object()  # Create (instantiate) an empty object
address = id(foo)
```


In addition some folks have written binary Python modules which implement "peek" and "poke" operations, but these are non-standard.


## Racket


```racket
#lang racket

(require ffi/unsafe)

(define (madness v) ; i'm so sorry
   (cast v _racket _gcpointer))
```


To test that it is doing "sane" things, you can retrieve the value of the C short located at the pointer produced. Racket objects start with a 2-byte tag indicating their type. These calls should all produce fairly small numbers: the Racket source I'm looking at uses only the first 259 tag values. Small fixnums are stored directly in tagged pointers, so attempting this dereferencing on the pointer madness gives you from a fixnum will most likely segfault your process.


```racket

(ptr-ref (madness +) _short)
(ptr-ref (madness (/ 4 3)) _short)
(ptr-ref (madness 3.2) _short)
(ptr-ref (madness (sqrt -2)) _short)
(ptr-ref (madness #\a) _short)
(ptr-ref (madness 'foo) _short)

```



## RapidQ


```vb

Dim TheAddress as long
Dim SecVar as byte
Dim MyVar as byte
    MyVar = 10

'Get the address of MyVar
TheAddress = varptr(MyVar)

'Set a new value on the address
MEMSET(TheAddress, 102, SizeOf(byte))

'Myvar is now = 102
showmessage "MyVar = " + str$(MyVar)

'...or copy from one address to another using:
MEMCPY(VarPtr(SecVar), TheAddress, SizeOf(byte))

'SecVar is now also = 102
showmessage "SecVar = " + str$(SecVar)

```



## Retro

Retro is only able to directly access memory as 32-bit values within a linear address space.


### Get The Address


```Retro
'a var
&a
```



### Set The Address

Create variable '''b''' and point it to address '''100'''

```Retro
'b var
#100 @Dictionary d:xt store
```



### Byte Addressing


Retro includes a standard library allowing for creation and access of byte-level data. This is done using words which mask, unpack, and repack the bytes within the 32-bit cells.

To read the value at byte address 100:


```Retro
'example/ByteAddressing.forth include
#100 b:fetch
```


Or to alter the value at byte address 100:


```Retro
$e #100 b:store
```



## REXX

REXX has no easy way of getting the address of variables within the langage itself, but since each

REXX variable can be accessed by name and its name passed to (say) subroutines [PROCEDUREs],

with the use of the VALUE and SYMBOL built-in functions (BIFs), it's possible to determine

the state of any variable (defined or not defined, its value, length of the variable's value).


It is possible to use the BIF (shown below)  (at least, in the original REXX)

```rexx
zzz = storage(xxx)
```

(but only in '''some''' REXX interpreters)   to access the internal REXX pool of variables, but it

would depend on the (internal) REXX internal structure(s) and almost likely be not portable nor

useable across releases of REXX or the operating system.   It would be necessary to follow a

pretty complex chain of pointers to just find the REXX pool of variables and the internal structure

may be pretty complicated and somewhat obscure.   Going down this path is not for the faint of

heart.


## Ruby


You can't access the address of a "variable" in Ruby.
However, it may be possible to get the address of an object.

The Ruby <code>object_id</code> method returns an object ID that is unique among active objects. It turns out that for the official Ruby implementation, the object ID is based on the address. For non-immediate objects (i.e. anything other than a <code>Fixnum</code>, <code>Symbol</code>, <code>true</code>, <code>false</code>, or <code>nil</code>), the address can be obtained by shifting the object ID one to the left. For more information, see the source code for the <code>object_id</code> method:[http://www.ruby-doc.org/core/Object.html#method-i-object_id].

For classes that do not override the <code>to_s</code> method, the <code>to_s</code> method also shows the address.


```ruby
>foo = Object.new  # => #<Object:0x10ae32000

>id = foo.object_id  # => 2238812160
>"%x" % (id << 1)  # => "10ae32000"

```



## Rust


It is not possible to change the memory address of an existing variable in Rust directly. However, you could make a copy of the value and then write it to a specific address.


```rust
let v1 = vec![vec![1,2,3]; 10];
println!("Original address: {:p}", &v1);
let mut v2;
// Override rust protections on reading from uninitialized memory
unsafe {v2 = mem::uninitialized();}
let addr = &mut v2 as *mut _;

// ptr::write() though it takes v1 by value, v1s destructor is not run when it goes out of
// scope, which is good since then we'd have a vector of free'd vectors
unsafe {ptr::write(addr, v1)}
println!("New address: {:p}", &v2);
```


Get the memory address of a variable:

```rust
let var = 1;
println!("address of var: {:p}", &var);
```


Get the value at a certain memory address:

```rust
let address: usize = 0x7ffc8f303130;
unsafe {
    let val = *(address as *const usize);
    println!("value at {}: {:?}", address, val);
}
```


Set the value at a certain memory address:

```rust
unsafe {
    *(0x7ffc8f303130 as *mut usize) = 1;
    // Note that this invokes undefined behavior if 0x7ffc8f303130 is uninitialized. In that case, std::ptr::write should be used.
    std::ptr::write(0x7ffc8f303130 as *mut usize, 1);
}
```



## Scala

There is no way to access addresses in Scala. It's governed by the Memory Management of the JVM controlling or knowing the addresses makes absolutely no sense in Scala.


## Sidef


```ruby
var n = 42;
say Sys.refaddr(\n);        # prints the address of the variable
say Sys.refaddr(n);         # prints the address of the object at which the variable points to
```

{{out}}

```txt

42823224
37867184

```



## Smalltalk

This task does not really make sense in Smalltalk: for one, all we could ask for is the address of an object, not a variable, which is a binding of a name to a value in a lexical scoping (similar to Scheme, Common Lisp and other managed languages). Second, the underlying memory management (garbage collector) is usually free to move objects around, and most implementations do so when objects are tenured or memory areas are defragmented (also similar).
So its usefulness is limited to VM developers and debuggers ;-)

You asked for it, and here it is:
{{works with|Smalltalk/X}}

```smalltalk
|p|
p := Point x:10 y:20.
ObjectMemory addressOf:p.
ObjectMemory collectGarbage.
ObjectMemory addressOf:p "may return another value"
```

to deal with non-Smalltalk objects, all Smalltalks provide libraries to pass-in and out parameters to foreign function calls (FFI). The underlying memory block will not be moved by the garbage collector and the address can be passed to external (eg. C, C++, asm) functions.
For those, we can allocate a block of memory and fiddle around with its "address":
{{works with|Smalltalk/X}}{{works with|VisualWorks Smalltalk}}

```smalltalk
|ptr|
ptr := ExternalBytes new:10.
ptr address.
ptr byteAt:1 put: 16rFF.
```


However, there are "reference holders", similar to box-objects in scheme/lisp. In Smalltalk these are called "ValueHolder" and are heavily used in UI frameworks. Usually, they are used with the observer pattern as shown in the following example:
<lang>|holder|
holder := ValueHolder with:123.
holder onChangeSend:#someChange to:someone.
holder value: 234

```


{{omit from|Smart BASIC}}


## Stata

See '''[https://www.stata.com/help.cgi?m2_pointers pointers]''' in Stata help.
It's not possible to set the address of a variable, but on can get the address of a variable or a function with the & operator.


```stata
a = 1
&a

function f(x) {
	return(x+1)
}

&f()
```



## Swift


```swift

class MyClass { }

func printAddress<T>(of pointer: UnsafePointer<T>) {
    print(pointer)
}

func test() {
    var x = 42
    var y = 3.14
    var z = "foo"
    var obj = MyClass()

    // Use a pointer to a variable on the stack and print its address.
    withUnsafePointer(to: &x)   { print($0) }
    withUnsafePointer(to: &y)   { print($0) }
    withUnsafePointer(to: &z)   { print($0) }
    withUnsafePointer(to: &obj) { print($0) }

    // Alternately:
    printAddress(of: &x)
    printAddress(of: &y)
    printAddress(of: &z)
    printAddress(of: &obj)

    // Printing the address of an object that an object reference points to.
    print(Unmanaged.passUnretained(obj).toOpaque())
}

test()

```

{{out}}

```txt

0x00007fffe61bcac0
0x00007fffe61bcab8
0x00007fffe61bcaa0
0x00007fffe61bca98
0x00007fffe61bcac0
0x00007fffe61bcab8
0x00007fffe61bcaa0
0x00007fffe61bca98
0x000000000082e2f0

```



## Tcl

It is highly unusual to want to directly manipulate the address of a variable in Tcl, as it is a thoroughly unsafe operation. Indeed, Tcl does not expose any mechanism to do so at the script level. However, Tcl does contain a C-level API function, <tt>[http://www.tcl.tk/man/tcl8.6/TclLib/LinkVar.htm Tcl_LinkVar]</tt>, to arrange for a variable's value to always reflect the contents of a particular address in memory. (See [[Machine Address#Tcl|Machine Address]] for an example of how to do that.)

However, that's not the only way of doing it. You can also use the '''critcl''' library to put [[C]] code directly inside a Tcl script and so work with addresses directly that way.


{{libheader|critcl}}

```tcl
package require critcl
# This code assumes an ILP32 architecture, like classic x86 or VAX.
critcl::cproc peek {int addr} int {
    union {
       int i;
       int *a;
    } u;

    u.i = addr;
    return *u.a;
}
critcl::cproc poke {int addr int value} void {
    union {
        int i;
        int *a;
    } u;

    u.i = addr;
    *u.a = value;
}
package provide poker 1.0
```

Demonstrating:

```tcl
package require poker

# Increment a memory location; this will probably crash if you try for real.
# We don't define how to get a good address, but it's not usually a problem
# for embedded programming...
set where 0x12340
poke $where [expr {[peek $where] + 1}]
```

Have great care with this sort of code; the damage you can do by writing to random locations is considerable and being able to read from anywhere could allow information to flow to otherwise unauthorized programs.


## Toka



### Get the Address

The default behaviour of a data element in Toka is to return its address. This makes obtaining the address trivial:

 variable foo
 foo .


### Set the Address

You can manually assign a name to any memory address (or other number),
but you should make sure it's part of allocated memory first.

  hex abcdef is-data foo
  foo .

{{omit from|TorqueScript}}


## VBA

The '''VarPtr''' function allows one to get the address of a variable. There are also functions to peek/poke values at a given address.


```vb
Option Explicit
Declare Sub GetMem1 Lib "msvbvm60" (ByVal ptr As Long, ByRef x As Byte)
Declare Sub GetMem2 Lib "msvbvm60" (ByVal ptr As Long, ByRef x As Integer)
Declare Sub GetMem4 Lib "msvbvm60" (ByVal ptr As Long, ByRef x As Long)
Declare Sub PutMem1 Lib "msvbvm60" (ByVal ptr As Long, ByVal x As Byte)
Declare Sub PutMem2 Lib "msvbvm60" (ByVal ptr As Long, ByVal x As Integer)
Declare Sub PutMem4 Lib "msvbvm60" (ByVal ptr As Long, ByVal x As Long)

Sub Test()
    Dim a As Long, ptr As Long, s As Long
    a = 12345678

    'Get and print address
    ptr = VarPtr(a)
    Debug.Print ptr

    'Peek
    Call GetMem4(ptr, s)
    Debug.Print s

    'Poke
    Call PutMem4(ptr, 87654321)
    Debug.Print a
End Sub
```



## Visual Basic .NET


Visual Basic uses managed memory that can be moved around at any time. If a memory address for a variable is needed, the address is created first and then its contents copied.


### Get the Address

Allocates a stable address in unmanaged memory, copies a variable to it, then returns the address itself.

  Dim x = 5
  Dim ptrX As IntPtr
  ptrX = Marshal.AllocHGlobal(Marshal.SizeOf(GetType(Integer)))
  Marshal.StructureToPtr(5, ptrX, False)
  Dim addressX = ptrX.ToInt64


### Set the Address

Sets the pointer to the address A100 in hex.

  Dim ptrX As New IntPtr(&HA100)


## Wart


```wart
addr.x
=> 27975840
```


<code>addr</code> is guaranteed to provide a stable identifier ''for this session''. The address is just a number like any other and you can perform all the arithmetic you like on it. However, there's no way to dereference an address back into a value, so this is not pointer arithmetic. The primary use of <code>addr</code> is to check if two objects are the same and not just copies, like Common Lisp's <code>eq</code> operator.


```wart
if (addr.x = addr.y)
  ..
```


As a result, Wart has only one way to compare values: by default two objects are considered equal if they are structurally isomorphic. (You can override it if you want a different behavior.)


## X86 Assembly

For SVR4 Unix-like style assembler the address of a variable is its symbol.  (On some systems the names of C language variables have an extra leading underscore.)

```Assembler
        movl    my_variable, %eax
```


For SVR4 style code destined for a shared library it's necessary to fetch the address from the global offset table to ensure position independent code.  That table is found relative to the program counter using the special <code>_GLOBAL_OFFSET_TABLE_</code> (or on some systems extra leading underscore <code>__GLOBAL_OFFSET_TABLE_</code>).  The C compiler normally does this in <code>%ebx</code> but for hand-crafted assembler anything equivalent is possible.


```Assembler
        call    eip_to_eax
        addl    $_GLOBAL_OFFSET_TABLE_, %eax
        movl    my_variable@GOT(%eax), %eax
        ...
eip_to_eax:
        movl    (%esp), %eax
        ret
```



## XLISP

To get the address in the heap of a variable <code>X</code>, use:

```lisp
(%ADDRESS-OF X)
```

If by "setting the address" we mean compelling the system to store a variable at a particular address of our choosing, then there is no easy way to do that.


## XPL0

It is easy to get the address of a variable (relative to the beginning of
its data segment), but there is no way to set the address of a variable.
However, pointers can be used to access specific addresses. The example
shows how the pointer B(0) is used to access the variable A and set it to
hex 1234ABCD. The Peek and Poke intrinsics can be used to access specific
hardware addresses. The absolute hardware address of a program's data
segment can be obtained using the GetReg intrinsic. The '@' operator
works exactly like 'addr' for integers but returns a 'real' pointer,
instead of a 32-bit relative address, when used on a 'real' variable.


```XPL0
include c:\cxpl\codes;
int  A, B;
[B:= addr A;
HexOut(0, B);  CrLf(0);
B(0):= $1234ABCD;
HexOut(0, A);  CrLf(0);
]
```


{{out}}

```txt

00000F48
1234ABCD

```



## Yorick


Yorick has pointers, but they are typically used in an opaque fashion.
Pointer arithmetic is not supported, not is referencing arbitrary memory locations. However, a pointer address may be copied to other variables.
Here is an interactive example that illustrates some of this.

```txt
> foo = 1
> bar = &foo
> bar
0x15f42c18
> baz = bar
> *baz = 5
> *bar
5
> *baz
5
```


{{omit from|8th|Impossible to access address of a variable}}
{{omit from|AWK}}
{{omit from|bc|Impossible to access address of a variable}}
{{omit from|Brlcad}}
{{omit from|Commodore BASIC}}
{{omit from|dc|Impossible to access address of a variable}}
{{omit from|Déjà Vu}}
{{omit from|E}}
{{omit from|Eiffel}}
{{omit from|Erlang}}
{{omit from|Factor}}
{{omit from|Falcon|Falcon does not support direct access of variable pointers}}
{{omit from|gnuplot}}
{{omit from|Groovy}}
{{omit from|GUISS}}
{{omit from|Haskell}}
{{omit from|JavaScript}}
{{omit from|Joy}}
{{omit from|Icon}}{{omit from|Unicon}}
{{omit from|LaTeX}}
{{omit from|Lily}}
{{omit from|Lilypond}}
{{omit from|Logtalk}}
{{omit from|M4}}
{{omit from|Make}}
{{Omit From|MATLAB}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{Omit From|Metafont}}
{{omit from|ML/I}}
{{Omit From|MUMPS|The interpreter handles the addressing}}
{{omit from|NetRexx}}
{{Omit From|NSIS}}
{{omit from|OCaml}}
{{omit from|Octave}}
{{omit from|Openscad}}
{{omit from|Oz}}
{{omit from|PlainTeX}}
{{omit from|PHP}}
{{omit from|Scratch}}
{{omit from|TI-89 BASIC}}
{{omit from|TPP}}
{{omit from|Unlambda|Does not have variables.}}
{{omit from|Verilog}}
{{omit from|VHDL}}
{{omit from|XSLT}}
{{omit from|UNIX Shell}}
{{omit from|zkl}}
