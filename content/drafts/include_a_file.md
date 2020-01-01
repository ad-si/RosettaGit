+++
title = "Include a file"
description = ""
date = 2019-09-24T20:50:28Z
aliases = []
[extra]
id = 9814
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Demonstrate the language's ability to include source code from other files.





## 360 Assembly

The COPY instruction includes source statements from the SYSLIB library.

```360asm>         COPY  member</lang



## ACL2


For files containing only events (definitions and similar; no top-level function calls) which are admissible (note the lack of file extension):

```Lisp
(include-book "filename")
```

For all other files:

```Lisp
(ld "filename.lisp")
```



## Ada

Some remarks are necessary here.
Ada does not define how the source code is stored in files. The language rather talks about compilation units. A compilation unit "imports" another compilation unit by using context clauses - these have the syntax "with CU1, CU2, ...;". All compilers I know of require in their standard mode exactly one compilation unit per file; also file naming conventions vary. However GNAT e.g. has a mode that can deal with files holding several compilation units and any file name conventions.

```Ada
with Ada.Text_IO, Another_Package; use Ada.Text_IO;
  -- the with-clause tells the compiler to include the Text_IO package from the Ada standard
  -- and Another_Package. Subprograms from these packages may be called as follows:
  --               Ada.Text_IO.Put_Line("some text");
  --               Another_Package.Do_Something("some text");
  -- The use-clause allows the program author to write a subprogram call shortly as
  --               Put_Line("some text");
```



## ALGOL 68

The formal definition of Algol68 make numerous references to the standard '''prelude''' and '''postlude'''.

At the time the language was formally defined it was typical for code to be stored on decks of [[wp:Punched card|punched cards]] (or [[wp:Punched tape|paper tape]]).
Possibly because storing code on [[wp:Hard disk drive|disk]] (or [[wp:Drum memory|drum]]) was expensive.
Similarly card decks can be read sequentially from ''just'' one [[wp:Punched card input/output|card reader]].
It appears the Algol68 "standard" assumed all cards could be simply stacked before and after the actual ''source code'', hence the references "prelude" and "postlude" in the formal standard.

=
## ALGOL 68G
=
In the simplest case a file can be included as follows:

```algol68
PR read "file.a68" PR
```


But in the Algol68 formal reports - it appears - the intention was to have a more structure approach.
{{works with|ALGOL 68|Revision 1 - one extension to language used - PRAGMA READ - a non standard feature similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: prelude/test.a68'''
```algol68
# -*- coding: utf-8 -*- #
BEGIN
# Exception setup code: #
  on value error(stand out, (REF FILE f)BOOL: GOTO value error not mended);
# Block setup code: #
  printf(($"Prelude test:"l$))
```
'''File: postlude/test.a68'''
```algol68
# -*- coding: utf-8 -*- #
# Block teardown code:  #
  printf(($"Postlude test."l$))
EXIT
# Exception code: #
  value error not mended: SKIP
END
```
'''File: test/include.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

PR read "prelude/test.a68" PR;
printf($4x"Hello, world!"l$);
PR read "postlude/test.a68" PR
```

{{out}}

```txt

Prelude test:
    Hello, world!
Postlude test.

```


'''Other implementations: e.g. [[ALGOL 68RS]] and [[ALGOL 68G]]'''

Note that actual ''source code'' inclusion with parsing can be avoided because of a more generalised separate compilation method storing declaration specifications in a ''[[wp:data dictionary|data dictionary]]''.
Different to '''#include''' found in [[C]] where the include file needs to be parsed for each source file that includes it.

=
## ALGOL 68RS
=

This British implementation of the language has various ways to include it's own source code and and integrate with code compiled from other languages.

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d].}}
* [[wp:ALGOL 68RS#Separate compilation|Separate compilation]]
* [[wp:ALGOL 68RS#Declaration modules|Declaration modules]]
* [[wp:ALGOL 68RS#Nested modules|Nested modules]]
* [[wp:ALGOL 68RS#Code and Alien access|Code and Alien access]]

In order to support a top-down programming style ALGOL 68RS provided the '''here''' and '''context''' facilities.

A program could be written with parts to be filled in later marked by a '''here''' tag followed by a ''keeplist'' of declarations to be made available.

 '''program''' (pass1, pass2) compiler
 '''begin'''
    '''string''' source := ...;
    '''tree''' parsetree;
 ...
    '''here''' pass1 (source, parsetree);
 ...
    '''instructions''' insts;
    '''here''' pass2 (parsetree, insts);
 ...
 '''end'''
 '''finish'''

The code to be executed in the context of the '''here''' tags would be written as:

 '''program''' pass1 implementation
 '''context''' pass1 '''in''' compiler
 '''begin'''
   ...   { code using "source" and "parsetree" }
 '''end'''
 '''finish'''

'''here''' is similar to the ALGOL 68C '''environ''' and '''context''' is equivalent to the ALGOL 68C '''using'''.

=
## ALGOL 68C
=

Separate compilation in ALGOL 68C is done using the ENVIRON and USING clauses. The ENVIRON saves the complete environment at the point it appears. A separate module written starting with a USING clause is effectively inserted into the first module at the point the ENVIRON clause appears.
* [[wp:ALGOL 68C#The ENVIRON and USING clauses|The ENVIRON and USING clauses]]

''' Example of <code>ENVIRON</code> clause '''

A file called ''mylib.a68'':

```algol68
BEGIN
   INT dim = 3; # a constant #
   INT a number := 120; # a variable #
   ENVIRON EXAMPLE1;
   MODE MATRIX = [dim, dim]REAL; # a type definition #
   MATRIX m1;
   a number := ENVIRON EXAMPLE2;
   print((a number))
END
```


''' Example of <code>USING</code> clause '''

A file called ''usemylib.a68'':

```algol68
USING EXAMPLE2 FROM "mylib"
BEGIN
  MATRIX m2; # example only #
  print((a number)); # declared in mylib.a68 #
  print((2 UPB m1)); # also declared in mylib.a68 #
  ENVIRON EXAMPLE3;  # ENVIRONs can be nested #
  666
END
```



## AntLang

AntLang is made for interactive programming, but a way to load files exists.
Even if it is really primitive, i. e. file get's current environment and manipulates it.

```AntLang
load["script.ant"]
```



## Applesoft BASIC

Chain PROGRAM TWO to PROGRAM ONE.  First create and save PROGRAM TWO.  Then, create PROGRAM ONE and run it.  PROGRAM ONE runs and then "includes" PROGRAM TWO which is loaded and run using the Binary program CHAIN from the DOS 3.3 System Master.  Variables from PROGRAM ONE are not cleared so they can be used in PROGRAM TWO.  User defined functions should be redefined in PROGRAM TWO.  See "Applesoft: CHAIN and user-defined functions Issues" http://support.apple.com/kb/TA41069

<lang ApplesoftBASIC}> 10  REMPROGRAM TWO
 20  DEF  FN A(X) = X * Y
 30  PRINT  FN A(2)

SAVE PROGRAM TWO
```

<lang ApplesoftBASIC}> 10  REMPROGRAM ONE
 20 Y = 6
 30  DEF  FN A(X) = X * Y
 40  PRINT  FN A(2)
 50 D$ =  CHR$ (4)
 60  PRINT D$"BLOADCHAIN,A520"
 70  CALL 520"PROGRAM TWO"

SAVE PROGRAM ONE
RUN
```


{{out}}

```txt

12

12

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

'file constant include'
/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

'file source include '
/* file affichage.inc */
.data
/*************************************************/
szMessErr: .ascii "Error code hexa : "
sHexa:     .space 9,' '
           .ascii "  decimal :  "
sDeci:     .space 15,' '
           .asciz "\n"
.text
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                       @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                    @ so here r2 contains the length of the message
    mov r1,r0                                       @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                            @ restaur des  2 registres */
    bx lr                                           @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL
1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
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
    bx lr
/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}       @ save registers
    mov r2,r1             @ debut zone stockage
    mov r3,#'+'           @ par defaut le signe est +
    cmp r0,#0             @ negative number ?
    movlt r3,#'-'         @ yes
    mvnlt r0,r0           @ number inversion
    addlt r0,#1
    mov r4,#10            @ length area
1:                        @ start loop
    bl divisionpar10U
    add r1,#48            @ digit
    strb r1,[r2,r4]       @ store digit on area
    sub r4,r4,#1          @ previous position
    cmp r0,#0             @ stop if quotient = 0
    bne 1b

    strb r3,[r2,r4]       @ store signe
    subs r4,r4,#1         @ previous position
    blt  100f             @ if r4 < 0 -> end

    mov r1,#' '           @ space
2:
    strb r1,[r2,r4]       @store byte space
    subs r4,r4,#1         @ previous position
    bge 2b                @ loop if r4 > 0
100:
    pop {r0-r4,lr}        @ restaur registers
    bx lr

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD
/***************************************************/
/*   display error message                        */
/***************************************************/
/* r0 contains error code  r1 : message address */
displayError:
    push {r0-r2,lr}                         @ save registers
    mov r2,r0                               @ save error code
    mov r0,r1
    bl affichageMess
    mov r0,r2                               @ error code
    ldr r1,iAdrsHexa
    bl conversion16                         @ conversion hexa
    mov r0,r2                               @ error code
    ldr r1,iAdrsDeci                        @ result address
    bl conversion10S                        @ conversion decimale
    ldr r0,iAdrszMessErr                    @ display error message
    bl affichageMess
100:
    pop {r0-r2,lr}                          @ restaur registers
    bx lr                                   @ return
iAdrszMessErr:                 .int szMessErr
iAdrsHexa:                     .int sHexa
iAdrsDeci:                     .int sDeci
/******************************************************************/
/*     Converting a register to hexadecimal                      */
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion16:
    push {r1-r4,lr}                                    @ save registers
    mov r2,#28                                         @ start bit position
    mov r4,#0xF0000000                                 @ mask
    mov r3,r0                                          @ save entry value
1:                                                     @ start loop
    and r0,r3,r4                                       @value register and mask
    lsr r0,r2                                          @ move right
    cmp r0,#10                                         @ compare value
    addlt r0,#48                                       @ <10  ->digit
    addge r0,#55                                       @ >10  ->letter A-F
    strb r0,[r1],#1                                    @ store digit on area and + 1 in area address
    lsr r4,#4                                          @ shift mask 4 positions
    subs r2,#4                                         @  counter bits - 4 <= zero  ?
    bge 1b                                             @  no -> loop

100:
    pop {r1-r4,lr}                                     @ restaur registers
    bx lr                                              @return
/***************************************************/
/* integer division unsigned                       */
/***************************************************/
division:
    /* r0 contains dividend */
    /* r1 contains divisor */
    /* r2 returns quotient */
    /* r3 returns remainder */
    push {r4, lr}
    mov r2, #0                                         @ init quotient
    mov r3, #0                                         @ init remainder
    mov r4, #32                                        @ init counter bits
    b 2f
1:                                                     @ loop
    movs r0, r0, LSL #1                                @ r0 <- r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1)
    adc r3, r3, r3                                     @ r3 <- r3 + r3 + C. This is equivalent to r3 ? (r3 << 1) + C
    cmp r3, r1                                         @ compute r3 - r1 and update cpsr
    subhs r3, r3, r1                                   @ if r3 >= r1 (C=1) then r3 <- r3 - r1
    adc r2, r2, r2                                     @ r2 <- r2 + r2 + C. This is equivalent to r2 <- (r2 << 1) + C
2:
    subs r4, r4, #1                                    @ r4 <- r4 - 1
    bpl 1b                                             @ if r4 >= 0 (N=0) then loop
    pop {r4, lr}
    bx lr

'File Main program'

/* ARM assembly Raspberry PI  */
/*  program include.s   */

/* Constantes               */
.include "./constantes.inc"
/* Initialized data */
.data
szMessageOK:        .asciz "Hello \n"

/*  code section */
.text
.global main
main:                                   @ entry of program
    push {fp,lr}                        @ saves registers
    ldr r0,iAdrszMessageOK
    bl affichageMess


100:                                    @ standard end of the program
    mov r0, #0                          @ return code
    pop {fp,lr}                         @restaur  registers
    mov r7, #EXIT                       @ request to exit program
    swi 0                               @ perform the system call

iAdrszMessageOK:           .int szMessageOK

/*********************************/
/* include source display        */
/*********************************/
.include "./affichage.inc"


```



## AutoHotkey


```AutoHotkey

#Include FileOrDirName
#IncludeAgain FileOrDirName

```


## AWK


The awk extraction and reporting language does not support the use of include files. However, it is possible to provide the name of more than one source file at the command line:


```sh
awk -f one.awk -f two.awk
```


The functions defined in different source files will be visible from other scripts called from the same command line:


```awk
# one.awk
BEGIN {
  sayhello()
}

# two.awk
function sayhello() {
  print "Hello world"
}
```


However, it is not permissible to pass the name of additional source files through a hashbang line, so the following will will not work:

 #!/usr/bin/awk -f one.awk -f two.awk

{{Works with|Gawk}}
GNU Awk has an <code>@include</code> which can include another awk source file at that point in the code.


```awk
@include "filename.awk"
```


This is a parser-level construct and so must be a literal filename, not a variable or expression.  If the filename is not absolute then it's sought in an <code>$AWKPATH</code> list of directories.  See [http://www.gnu.org/software/gawk/manual/html_node/Include-Files.html the gawk manual] for more.


## Axe

This will cause the program called OTHER to be parsed as if it was contained in the source code instead of this line.

```axe>prgmOTHER</lang



## BaCon

''other.bac''

```freebasic>other = 42</lang

''including.bac''

```freebasic
' Include a file
INCLUDE "other.bac"
PRINT other
```


{{out}}

```txt
prompt$ bacon including.bac
Converting 'including.bac'... done, 4 lines were processed in 0.005 seconds.
Compiling 'including.bac'... cc  -c including.bac.c
cc -o including including.bac.o -lbacon -lm
Done, program 'including' ready.
prompt$ ./including
42
```



## BASIC

{{works with|QuickBASIC}}
The include directive must be in a comment and that the name of the file for inclusion is enclosed in single quotes (a.k.a. apostrophes).

Note that this will ''not'' work under QBasic.


```qbasic
REM $INCLUDE: 'file.bi'
'$INCLUDE: 'file.bi'
```


See also: [[#BBC BASIC|BBC BASIC]], [[#Gambas|Gambas]], [[#IWBASIC|IWBASIC]], [[#PowerBASIC|PowerBASIC]], [[#PureBasic|PureBasic]], [[#Run BASIC|Run BASIC]], [[#ZX Spectrum Basic|ZX Spectrum Basic]]


## Batch File


```dos

call file2.bat

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      CALL filepath$
```

The file is loaded into memory at run-time, executed, and then discarded.  It must be in 'tokenised' (internal) .BBC format.


## Bracmat


```bracmat
get$"<i>module</i>"
```



## ChucK

<lang>Machine.add(me.dir() + "/MyOwnClassesDefinitions.ck");
```


=={{header|C}} / {{header|C++}}==

In C and C++, inclusion of other files is achieved via a preprocessor. The <code>#include</code> preprocessor directive tells the compiler to incorporate code from the included file. This is normally used near the top of a source file and is usually used to tell the compiler to include header files for the function libraries.


```c
/* Standard and other library header names are enclosed between chevrons */
#include <stdlib.h>

/* User/in-project header names are usually enclosed between double-quotes */
#include "myutil.h"
```


Although it is often conventional and idiomatic for a project to use its own headers in the style described on the second line above, it's also possible to tell most compilers using various flags (e. g. GCC and Clang accept <tt>-I</tt>) to treat an arbitrary directory as a system/library include folder, thereby allowing any contained files to be included using the angle bracket syntax.


## C#


```c#
/* The C# language specification does not give a mechanism for 'including' one source file within another,
 * likely because there is no need - all code compiled within one 'assembly' (individual IDE projects
 * are usually compiled to separate assemblies) can 'see' all other code within that assembly.
 */
```



## Clipper

The inclusion of other files is achieved via a preprocessor. The <code>#include</code> preprocessor directive tells the compiler to incorporate code from the included file. This is normally used near the top of a source file and is usually used to tell the compiler to include header files for the function libraries.

```clipper
  #include "inkey.ch"
```



## Clojure

Just as in Common Lisp:

```clojure
(load "path/to/file")
```


This would rarely be used for loading code though, since Clojure supports modularisation (like most modern languages) through [http://blog.8thlight.com/colin-jones/2010/12/05/clojure-libs-and-namespaces-require-use-import-and-ns.html namespaces] and code is typically located/loaded via related abstractions.  It's probably more often used to load data or used for quick-and-dirty experiments in the [https://en.wikipedia.org/wiki/Read–eval–print_loop REPL].


## COBOL

In COBOL, code is included from other files by the <code>COPY</code> statement. The files are called copybooks, normally end with the file extension '.cpy' and may contain ''any'' valid COBOL syntax. The <code>COPY</code> statement takes an optional <code>REPLACING</code> clause allows any text within the copybook to be replaced with something else.

```cobol
COPY "copy.cpy". *> The full stop is mandatory, wherever the COPY is.
COPY "another-copy.cpy" REPLACING foo BY bar
                                  SPACE BY ZERO
                                  ==text to replace== BY ==replacement text==.
```



## Common Lisp


```lisp
(load "path/to/file")
```



## D

D has a module system, so usually there is no need of a textual inclusion of a text file:

```d>import std.stdio;</lang


To perform a textual inclusion:

```d
mixin(import("code.txt"));
```

=={{header|Déjà Vu}}==

```dejavu
#with the module system:
!import!foo

#passing a file name (only works with compiled bytecode files):
!run-file "/path/file.vu"
```



## Delphi


```Delphi
uses SysUtils;    // Lets you use the contents of SysUtils.pas from the current unit

{$Include Common} // Inserts the contents of Common.pas into the current unit
{$I Common}       // Same as the previous line, but in a shorter form
```



## Dragon

To include source code from another file, you simply need to use include keyword with file name.

Just this would be enough.
<lang>def my(){
  showln "hello"
  //this is program.dgn
}
```


<lang>
include "program.dgn"
my() // output : hello

```




## DWScript


In addition to straight inclusion, there is a filtered inclusion, in which the include file goes through a pre-processing filter.

```Delphi

{$INCLUDE Common}      // Inserts the contents of Common.pas into the current unit
{$I Common}            // Same as the previous line, but in a shorter form
{$INCLUDE_ONCE Common} // Inserts the contents of Common.pas into the current unit only if not included already
{$FILTER Common}       // Inserts the contents of Common.pas into the current unit after filtering
{$F Common}            // Same as the previous line, but in a shorter form

```



## Emacs Lisp

Write this code in: file1.el

```Emacs Lisp

(defun sum (ls)
  (apply '+ ls) )

```

In the directory of file1.el, we write this new code in: file2.el

```Emacs Lisp

(add-to-list 'load-path "./")
(load "./file1.el")
(insert (format "%d" (sum (number-sequence 1 100) )))

```

<b>Output:</b>

```txt

5050

```



## Erlang


```Erlang

-include("my_header.hrl"). % Includes the file at my_header.erl

```


## Euphoria


```Euphoria

include my_header.e

```



## Factor


```Factor

USING: vocaba vocabb... ;

```



## Forth


```forth>include matrix.fs</lang


Other Forth systems have a smarter word, which protects against multiple inclusion.  The name varies: '''USES''', '''REQUIRE''',  '''NEEDS'''.


## Fortran


```Fortran
include ''char-literal-constant''
```


"The interpretation of char-literal-constant is processor dependent. An example of a possible valid interpretation is that char-literal-constant is the name of a file that contains the source text to be included."
See section ''3.4 Including source text'' of the [http://www.j3-fortran.org/doc/year/04/04-007.pdf ISO standard working draft] (Fortran 2003).

Included content may itself involve further inclusions but should not start with any attempt at the continuation of a statement preceding the include line nor should there be any attempt at the line following the include being a continuation of whatever had been included. It is not considered to be a statement (and so should not have a statement label) in Fortran itself but something a Fortran compiler might recognise, however a trailing comment on that line may be accepted. The exact form (if supported at all) depends on the system and its file naming conventions, especially with regard to spaces in a file name. The file name might be completely specified, or, relative as in <code>INCLUDE "../Fortran/Library/InOutCom.for"</code> Further, Fortran allows text strings to be delimited by apostrophes as well as by quotes and there may be different behaviour for the two sorts, if recognised. For instance, the relative naming might be with reference to the initial file being compiled, or, with reference to the directory position of the file currently being compiled - it being the source of the current include line - as where file InOutCom.for contained an inclusion line specifying another file in the same library collection.

Different compilers behave differently, and standardisation attempts have not reached back to earlier compilers.


## FreeBASIC

File to be included :

```freebasic
' person.bi file
Type Person
  name As String
  age As UInteger
  Declare Operator Cast() As String
End Type

Operator Person.Cast() As String
  Return "[" + This.name + ", " + Str(This.age) + "]"
End Operator
```


Main file :

```freebasic
' FB 1.05.0 Win 64

' main.bas file
#include "person.bi"

Dim person1 As Person
person1.name = "Methuselah"
person1.age = 969
Print person1
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

[Methuselah, 969]

```



## Furryscript

Use a word with a slash at the end to import; how the file is found is based on the implementation (normally the "furinc" directory is looked at for include files).

The file is imported into a new namespace. Use the same name at the beginning and a slash, but now include something else afterward, and now means that name inside of that namespace.


## FutureBasic

FB has powerful tools to include files in a project. Its "include resources" statement allows you to specify any number of files for copying into the built application package's Contents/Resources/ directory.

```futurebasic

include resources "SomeImage.png"
include resources "SomeMovie.mpeg"
include resources "SomeSound.aiff"
include resources "SomeIcon.icns"
include resources "Info.plist"   //Custom preference file to replace FB's generic app preferences

```

Including C or Objective-C headers (i.e. files with the .h extension) or source files (files with the .c or .m extension) requires a different 'include' syntax:

```txt

include "HeaderName.h"  // do not use 'include resources' to include C/Objective-C headers
include "CSourceFile.c"
include "ObjectiveCImplementationFile.m"

```

Another special case are Objective-C .nib or .xib files. These are loaded with:

```txt

include resources "main.nib"

```

However, .nib files are copied to the built application's Contents/Resources/en.lproj/ directory.


Mac OS X frameworks may be specified with the 'include library' statement, which has two forms:

```txt

include library "Framework/Header.h"
include library "Framework" // optional short form, expanded internally to: include library "Framework/Framework.h"

```

After including a Framework, you must notify the compiler of specific functions in the Framework that your code will be using with FB's "toolbox fn" statement as shown in this example:

```txt

include library "AddressBook/AddressBookUI.h"
// tell FBtoC the functions
toolbox fn ABPickerCreate() = ABPickerRef

```

Special treatment for C static libraries (*.a): The include statement copies the library file to the build_temp folder; you must also place the name of the library file in the preferences 'More compiler options' field [this causes it to be linked]. The example below is for a library MyLib that exports one symbol (MyLibFunction).

```txt

include "MyLib.a"
BeginCDeclaration
// let the compiler know about the function
void MyLibFunction( void ); // in lieu of .h file
EndC
// let FBtoC know about the function
toolbox MyLibFunction()
MyLibFunction() // call the function

```

An include file can also contain executable source code. Example: Suppose we create a file "Assign.incl" which contains the following lines of text:

```txt

dim as long a, b, c

a = 3
b = 7
</Pre>
Now suppose we write a program like this:

```txt

include "Assign.incl"
c = a + b
print c

```

When we compile this program, the result will be identical to this:

```txt

dim as long a, b, c
a = 3
b = 7
c = a + b
print c

```

Other include cases are detailed in FB's Help Center.


## Gambas


In gambas, files are added to the project via the project explorer main window which is a component of the integrated development environment.


## Gambas


Here a file is loaded into a variable

```gambas
Public Sub Form_Open()
Dim sFile As String

sFile = File.Load("FileToLoad")

End

```



## GAP


```gap
Read("file");
```



## Gnuplot


```gnuplot
load "filename.gnuplot"
```


This is the same as done for each file named on the command line.  Special filename <code>"-"</code> reads from standard input.


```gnuplot
load "-"         # read standard input
```


If the system has <code>popen</code> then piped output from another program can be loaded,


```gnuplot
load "< myprogram"       # run myprogram, read its output
load "< echo print 123"
```


<code>call</code> is the same as <code>load</code> but takes parameters which are then available to the sub-script as <code>$0</code> through <code>$9</code>


```gnuplot
call "filename.gnuplot" 123 456 "arg3"
```



## Go

Go has a 'package' system and doesn't therefore need to include source code from other files via an #include directive (as found in C/C++) or similar.

Instead one can simply give the other source code file(s) the same package name as the 'main' file, copy them to the same directory and build them all together. For example:

```go
// main.go
package main

import "fmt"

func hello() {
    fmt.Println("Hello from main.go")
}

func main() {
    hello()
    hello2()
}
```



```go
// main2.go
package main

import "fmt"

func hello2() {
    fmt.Println("Hello from main2.go")
}
```


{{out}}

```txt

$ go build main.go main2.go
$ ./main
Hello from main.go
Hello from main2.go

```



## Harbour

The inclusion of other files is achieved via a preprocessor. The <code>#include</code> preprocessor directive tells the compiler to incorporate code from the included file. This is normally used near the top of a source file and is usually used to tell the compiler to include header files for the function libraries.

```visualfoxpro
#include "inkey.ch"
```



## Haskell



```Haskell
-- Due to Haskell's module system, textual includes are rarely needed. In
-- general, one will import a module, like so:
import SomeModule
-- For actual textual inclusion, alternate methods are available. The Glasgow
-- Haskell Compiler runs the C preprocessor on source code, so #include may be
-- used:
#include "SomeModule.hs"
```



## HTML


Current HTML specifications do not provide an include tag, Currently, in order to include content from another file, it is necessary to include content via an iframe. However, this is not supported in some browsers and looks very untidy in other browsers:


```html
<iframe src="foobar.html">
Sorry: Your browser cannot show the included content.</iframe>
```


There is an unofficial tag, but this will be ignored by most browsers:


```html><include>foobar.html</include></lang


=={{header|Icon}} and {{header|Unicon}}==

Include another file of source code using the preprocessor statement:
```Icon
$include "filename.icn"
```



## IWBASIC


```IWBASIC
$INCLUDE "ishelllink.inc"
```


Further, external library or object files can be specified with the $USE statement, which is a compiler preprocessor command:


```IWBASIC
$USE "libraries\\mylib.lib"
```


IWBASIC also allows resources, files and data that are compiled with an application and embedded in the executable. However, resources in IWBASIC may be used only for projects, i.e., programs that have more than one source file.

Various resources are loaded as follows:


```IWBASIC
Success=LOADRESOURCE(ID,Type,Variable)
```


<code>ID</code> is either a numeric or string identifier to the resource, <code>TYPE</code> is a numeric or string type and it stores the info in variable. The standard Windows resource types can be specified and loaded in raw form using the following constants:


```IWBASIC
@RESCURSOR
@RESBITMAP
@RESICON
@RESMENU
@RESDIALOG
@RESSTRING
@RESACCEL
@RESDATA
@RESMESSAGETABLE
@RESGROUPCURSOR
@RESGROUPICON
@RESVERSION
```



## J


The usual approach for a file named 'myheader.ijs' would be:


```j
require 'myheader.ijs'
```


However, this has "include once" semantics, and if the requirement is to include the file even if it has been included earlier you would instead use:


```j
load 'myheader.ijs'
```




## Java

To include source code from another file, you simply need to create an object of that other file, or 'extend' it using inheritance. The only requirement is that the other file also exists in the same directory, so that the classpath can lead to it. Since Java is quite particular about their "Class name is the same as file name" rule, if you want to use another file called Class2 in Class1, you don't need to be told a unique filename.

Just this would be enough.

```Java
public class Class1 extends Class2
{
	//code here
}
```


You could also consider creating an instance of Class2 within Class1, and then using the instance methods.

```Java
public class Class1
{
	Class2 c2=new Class2();
	static void main(String[] args)
	{
		c2.func1();
		c2.func2();
	}
}
```



## JavaScript



### Pure JavaScript in browsers with the DOM

Following example, if loaded in an HTML file, loads the [http://jquery.com/ jQuery] library from a remote site

```javascript
var s = document.createElement('script');
s.type = 'application/javascript';

// path to the desired file
s.src = 'http://code.jquery.com/jquery-1.6.2.js';
document.body.appendChild(s);
```

Most be noted that it can also request [[HTTP]] source and eval() the source


### With jQuery

{{libheader|jQuery}}

```javascript
$.getScript("http://example.com/script.js");
```


===With AMD (require.js)===

```javascript
require(["jquery"], function($) { /* ... */ });
```


===CommonJS style with node.js (or browserify)===
{{libheader|node.js}}

```javascript
var $ = require('$');
```



### ES6 Modules


```javascript
import $ from "jquery";
```



## jq


{{works with | jq | with "include" }}

jq 1.5 has two directives for including library files, "include" and "import". A library file here means one that contains jq function definitions, comments, and/or directives.

The main difference between the two types of directive is that included files are in effect textually included at the point of inclusion, whereas
imported files are imported into the namespace specified by the "import" directive.  The "import" directive can also be used to import data.

Here we illustrate the "include" directive on the assumption that there are two files:

'''Include_a_file.jq'''

```jq
include "gort";

hello
```


'''gort.jq'''
<lang>def hello: "Klaatu barada nikto";
```


{{ out }}

```sh
$ jq -n -c -f Include_a_file.jq
Klaatu barada nikto.
```



## Jsish

''jsish'' can include other source via '''System.source('filename');'''.  Versioned moduled can be included via '''System.require('module', version);'''.  Methods in the ''System'' object are automatically exported as top level globals (and the module version argument defaults to 1), so those can be shortened to


```javascript
source('file');
require('module');
```


Compiled code can also be included via '''System.load('shlib');''', but that feature requires a known named init function, Jsi_Init[shlib] to be an exported symbol in the Dynamic Shared Object file.


## Julia

Julia's <code>include</code> function executes code from an arbitrary file:

```Julia
include("foo.jl")
```

or alternatively <code>include_string</code> executes code in a string as if it were a file (and can optionally accept a filename to use in error messages etcetera).

Julia also has a module system:

```Julia>import MyModule</lang

imports the content of the module <code>MyModule.jl</code> (which should be of the form <code>module MyModule ... end</code>, whose symbols can be accessed as <code>MyModule.variable</code>, or alternatively

```Julia>using MyModule</lang

will import the module and all of its exported symbols


## Kotlin

The closest thing Kotlin has to an ''#include'' directive is its ''import'' directive. This doesn't import source code as such but makes available names defined in another accessible package as if such names were defined in the current file i.e. the names do not need to be fully qualified except to resolve a name clash.

Either a single name or all accessible names in a particular scope (package, class, object etc.) can be imported.

For example:

```scala
fun f() = println("f called")
```


We can now import and invoke this from code in the default package as follows:


```scala
// version 1.1.2

import package1.f // import f from package `package1`

fun main(args: Array<String>) {
    f()  // invoke f without qualification
}
```


{{out}}

```txt

f called

```



## LabVIEW

In LabVIEW, any VI can be used as a "SubVI" by changing the icon and wiring the terminals to the front panel. This cannot be explained concisely in code; instead, see the [http://zone.ni.com/reference/en-XX/help/371361E-01/lvconcepts/creating_subvis/ documentation].


## LabVIEW


```Lasso
web_response -> include('my_file.inc')
```



## Lasso


```Lasso
include('myfile.lasso')
```



## Lingo



```lingo
-- load Lingo code from file
fp = xtra("fileIO").new()
fp.openFile(_movie.path&"someinclude.ls", 1)
code = fp.readFile()
fp.closeFile()

-- create new script member, assign loaded code
m = new(#script)
m.name = "someinclude"
m.scriptText = code

-- use it instantly in the current script (i.e. the script that contained the above include code)
script("someinclude").foo()
```



## Logtalk


```logtalk

:- object(foo).

    :- include(bar).

:- end_object.

```



## Lua


To include a header file myheader.lua:


```lua
 require "myheader"
```



## M2000 Interpreter

Without use of New in Load we get any cached file with same name. Using load from M2000 command line we always load file, but from code interpreter use a cache to hold it for next load.

```M2000 Interpreter

Document A$={
Module Global Beta {
      Print "This is Beta"
      x=10
      Print x
      }
      Print "This is statement to execute"
      Beta  ' this call not happen
}
Save.Doc A$, "TestThis.Gsb"
Module checkit {
      \\ we can delete Global
      \\ usinf New Modules we get latest TestThis, excluding statements calling modules.
      Load New Modules TestThis
      \\ check if Beta exist
      Print Module(Beta)=True
      \\ so now we call Beta
      Beta
      Print Valid(x)=False ' x is local to beta
}
Checkit
\\ now Beta erased (after return form Checkit)
Print Module(Beta)=False

```


Running code of a module, as code is inline and  not in that module. Now X is a variable in CheckIt

```M2000 Interpreter

\\ we can delete global
Module Global alfa {
      Print "this is alfa"
      X=10
}
Module Checkit {
      Inline Code alfa
      Print X=10
}
Checkit

```



## m4



```m4
include(filename)
```



## Maple

For textual inclusion, analogous to the C preprocessor, use the "$include" preprocessor directive.  (The preprocessor is not a separate program, however.)  This is frequently useful for large project development.

```Maple
$include <somefile>
```

Or

```Maple
$include "somefile"
```

It is also possible to read a file, using the "read" statement.  This has rather different semantics.

```Maple
read "somefile":
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==



```Mathematica
 Get["myfile.m"]
```


=={{header|MATLAB}} / {{header|Octave}}==
MATLAB and Octave look for functions in *.m and *.mex included in the "path".
New functions can be included, either by storing a new function in an existing path, or by extending the existing path to a new directory. When two functions have the same name, the function found first in the path takes precedence. The later is shown here:


```MATLAB
  % add a new directory at the end of the path
  path(path,newdir);
  addpath(newdir,'-end');  % same as before

  % add a new directory at the beginning
  addpath(newdir);
  path(newdir,path);       % same as before
```



## Maxima


```maxima
load("c:/.../source.mac")$

/* or if source.mac is in Maxima search path (see ??file_search_maxima), simply */
load(source)$
```


=={{header|Modula-2}}==

```modula2
IMPORT  InOut, NumConv, Strings;
```


=={{header|Modula-3}}==

```modula3
IMPORT IO, Text AS Str;
FROM Str IMPORT T
```



## Nemerle

To include classes, static methods etc. from other namespaces, include those namespaces with the <tt>using</tt> keyword

```Nemerle>using System.Console;</lang

<tt>using</tt> is for accessing code that has already been compiled into libraries. Nemerle also allows for creating
<tt>partial</tt> classes (and structs), the source code of which may be split amongst several files as long as the class is
marked as <tt>partial</tt> in each place that part of it is defined. An interesting feature of partial classes in
Nemerle is that some parts of partial classes may be written in C# while others are written in Nemerle.

```Nemerle
public partial class Foo : Bar // all parts of a partial class must have same access modifier;
{                              // the class that a partial class inherits from only needs to
...                            // be specified in one location
}
```



## NewLISP


```NewLISP
;; local file
(load "file.lsp")

;; URLs (both http:// and file:// URLs are supported)
(load "http://www.newlisp.org/code/modules/ftp.lsp")
```



## Nim

After <code>import someModule</code> an exported symbol <code>x</code> can be accessed as <code>x</code> and as <code>someModule.x</code>.

```nim
import someModule
import strutils except parseInt
import strutils as su, sequtils as qu # su.x works
import lib.pure.strutils, lib/pure/os, "lib/pure/times" # still strutils.x
```



## OASYS Assembler

Use an equal sign at the beginning of a line to include a file: <code>=util.inc</code>


## OCaml


In script mode and in the interactive loop (the toplevel) we can use:

```ocaml
#use "some_file.ml"
```


In compile mode (compiled to bytecode or compiled to native code) we can use:

```ocaml
include Name_of_a_module
```



## Oforth


In order to load a file with name filename :

```Oforth
"filename" load
```


In order to load a package with name pack :

```Oforth>import: pack</lang



## ooRexx

ooRexx has a package system and no ability for textual inclusion of other text files.  Importing of other packages is done via the ::requires directive.

```ooRexx
   ::requires "regex.cls"
```



## OpenEdge/Progress

Curly braces indicate that a file should be included. The file is searched across all PROPATH directory entries.

```progress
{file.i}
```


Arguments can be passed to the file being included:


```progress
{file.i super}
```



## Openscad



```openscad
//Include and run the file foo.scad
include <foo.scad>;

//Import modules and functions, but do not execute
use <bar.scad>;
```



## PARI/GP

Files can be loaded in GP with the <code>read</code>, or directly in gp with the metacommand <code>\r</code>.

PARI can use the standard [[#C|C]] <code>#include</code>, but note that if using gp2c the embedded <code>GP;</code> commands must be in the original file.


## Pascal

See [[Include_a_file#Delphi | Delphi]]


## Perl


Here we include the file include.pl into our main program:

main.perl:


```perl
#!/usr/bin/perl
do "include.pl";        # Utilize source from another file
sayhello();
```


include.pl:

```perl
sub sayhello {
  print "Hello World!";
}
```


From documentation:
```txt

If "do" cannot read the file, it returns undef and sets $! to the error.
If "do" can read the file but cannot compile it, it returns undef and sets
an error message in $@.
If the file is successfully compiled, "do" returns the value of the last
expression evaluated.
```



## Perl 6

Perl 6 provides a module system that is based primarily on importation of symbols rather than
on inclusion of textual code:

```perl6>use MyModule;</lang

However, one can evaluate code from a file:

```perl6
require 'myfile.p6';
```

One can even do that at compile time:

```perl6
BEGIN require 'myfile.p6'
```

None of these are true inclusion, unless the <tt>require</tt> cheats and modifies the current input string of the parser.  To get a true textual inclusion, one could define an unhygienic textual macro like this:

```perl6
macro include(AST $file) { slurp $file.eval }
include('myfile.p6');
```



## Phix


```Phix>include arwen.ew</lang

Phix also supports relative directory includes, for instance if you include "..\demo\arwen\arwen.ew" then anything that arwen.ew includes will be looked for in the appropriate directory.


## PHP

There are different ways to do this in PHP.  You can use a basic include:

```PHP
include("file.php")
```

You can be safe about it and make sure it's not included more than once:

```PHP
include_once("file.php")
```

You can crash the code at this point if the include fails for any reason by using require:

```PHP
require("file.php")
```

And you can use the require statement, with the safe _once method:

```PHP
require_once("file.php")
```



## PicoLisp

The function '[http://software-lab.de/doc/refL.html#load load]' is used for recursively executing the contents of files.

```PicoLisp
(load "file1.l" "file2.l" "file3.l")
```



## PL/I


```pli
%include myfile;
```



## PowerBASIC

Note that PowerBASIC has the optional modifier <code>ONCE</code> which is meant to insure that no matter how many times the file may be included in code, it will only be inserted by the compiler once (the first time the compiler is told to include that particular file).

Note also that <code>#INCLUDE</code> and <code>$INCLUDE</code> function identically.


```powerbasic
#INCLUDE "Win32API.inc"
#INCLUDE ONCE "Win32API.inc"
```



## PowerShell


```PowerShell

<#
    A module is a set of related Windows PowerShell functionalities, grouped together as a convenient unit (usually saved in a
    single directory). By defining a set of related script files, assemblies, and related resources as a module, you can
    reference, load, persist, and share your code much easier than you would otherwise.
#>

Import-Module -Name MyModule


<#
    When you dot source a script (or scriptblock), all variables and functions defined in the script (or scriptblock) will
    persist in the shell when the script ends.
#>
. .\MyFunctions.ps1

```




## Processing

Processing sketches ''automatically'' include include any Processing .pde or Java .java files in the sketch directory. All .pde files are concatenated together and processed as if they were one big file. Each .java file is compiled to a Java class and the class is automatically imported.

Processing also supports the import keyword for explicitly including Processing / Java 8 libraries.


```processing
import java.util.Map;
import g4p_controls.*;
```




## Prolog



```Prolog
consult('filename').
```



## PureBasic

IncludeFile will include the named source file at the current place in the code.

```PureBasic
IncludeFile "Filename"
```

XIncludeFile is exactly the same except it avoids including the same file several times.

```PureBasic
XIncludeFile "Filename"
```


IncludeBinary will include a named file of any type at the current place in the code.
IncludeBinary don't have to, but should preferably be done inside a [http://www.purebasic.com/documentation/reference/data.html data block].

```PureBasic
IncludeBinary "Filename"
```



## Python

Python supports the use of [http://docs.python.org/library/functions.html#execfile execfile] to allow code from arbitrary files to be executed from a program (without using its modules system).


```Python>import mymodule</lang


includes the content of mymodule.py

Names in this module can be accessed as attributes:


```Python>mymodule.variable</lang



## R



```R
source("filename.R")
```



## Racket


Including files is usually discouraged in favor of using modules, but it is still possible:


```racket

#lang racket
(include "other-file.rkt")

```



## RapidQ


```vb

$INCLUDE "RAPIDQ.INC"

```



## Retro



```Retro
'filename include
```



## REXX

{{works with|CMS and TSO REXX compiler}}

The REXX language does not include any directives to include source code from other files. A workaround is to use a preprocessor that take the source and the included modules and builds a temporary file containing all the necessary code, which then gets run by the interpreter.

Some variants of REXX may provide implementation specific solutions.

The REXX Compiler for CMS and TSO supports a directive to include program text before compiling the program

```rexx
/*%INCLUDE member */
```



### Including a file at time of execution

On the other hand, since REXX is a dynamic language, you can (mis)use some file IO and the INTERPRET statement to include another source file:
{{works with|ARexx}}
{{works with|REGINA 3.8 and later, with options: AREXX_BIFS and AREXX_SEMANTICS}}

```rexx

/* Include a file and INTERPRET it; this code uses ARexx file IO BIFs */
say 'This is a program running.'
if Open(other,'SYS:Rexxc/otherprogram.rexx','READ') then do
   say "Now we opened a file with another chunk of code. Let's read it into a variable."
   othercode=''
   do until EOF(other)
      othercode=othercode || ReadLn(other) || ';'
      end
   call Close(other)
   say 'Now we run it as part of our program.'
   interpret othercode
   end
say 'The usual program resumes here.'
exit 0

```

Note:   due to the way most REXX interpreters work, functions and jumps (SIGNALs) inside an INTERPRETED program won't work.   Neither are   ''labels''   recognized, which would then exclude the use of those subroutines/functions.

There are also other restrictions such as multi-line statements and comments (more than one line).

Another possibility of errors is the creation of an extremely long value which may exceed the limit for a particular REXX interpreter.


### Calling an external program

Usually, including a file in another is not necessary with REXX, since any script can be called as a function:

'''Program1.rexx'''

```rexx

/* This is program 1 */
say 'This is program 1 writing on standard output.'
call Program2
say 'Thank you, program 1 is now ending.'
exit 0

```

'''Program2.rexx'''

```rexx

/* This is program 2 */
say 'This is program 2 writing on standard output.'
say 'We now return to the caller.'
return

```

If a REXX interpreter finds a function call, it first looks in the current program for a function or procedure by that name, then it looks in the standard function library (so you may replace the standard functions with your own versions inside a program), then it looks for a program by the same name in the standard paths. This means that including a file in your program is usually not necessary, unless you want them to share global variables.


## RPG


{{works with|ILE RPG}}


```rpg
      // fully qualified syntax:
      /include library/file,member

      // most sensible; file found on *libl:
      /include file,member

      // shortest one, the same library and file:
      /include member

      // and alternative:
      /copy library/file,member

      //... farther like "include"
```



## Ring


```Ring
Load 'file.ring'
```



## Ruby

Note that in Ruby, you don't use the file extension. Ruby will first check for a Ruby (.rb) file of the specified name and load it as a source file. If an .rb file is not found it will search for files in .so, .o, .dll or other shared-library formats and load them as Ruby extension. <code>require</code> will search in a series of pre-determined folders, while <code>require_relative</code> behaves the same way but searches in the current folder, or another specified folder.


```Ruby
require 'file'
```



## Run BASIC

You don't use the file extension.  .bas is assumed.

```runbasic
run SomeProgram.bas",#include         ' this gives it a handle of #include
render #include                       ' render will RUN the program with handle #include
```



## Rust

The compiler will include either a 'test.rs' or a 'test/mod.rs' (if the first one doesn't exist) file.

```rust
mod test;

fn main() {
    test::some_function();
}
```


Additionally, third-party libraries (called <code>crates</code> in rust) can be declared thusly:

```rust
extern crate foo;
fn main() {
    foo::some_function();
}
```



## Scala

Some remarks are necessary here. Scala does not define how the source code is stored in files. The language rather talks about compilation units.

In a Scala REPL[https://docs.scala-lang.org/overviews/repl/overview.html] it's possible to save and load source code.

## Seed7

The Seed7 language is defined in the include file seed7_05.s7i.
Therefore seed7_05.s7i must be included before other language features can be used (only comments can be used before).
The first include directive (the one which includes seed7_05.s7i) is special and it must be introduced with the $ character.

```seed7
$ include "seed7_05.s7i";
```

All following include directives don't need a $ to introduce them.
The [http://seed7.sourceforge.net/libraries/float.htm float.s7i]
[http://seed7.sourceforge.net/libraries library] can be included with:

```seed7
  include "float.s7i";
```



## Sidef

Include a file in the current namespace:

```ruby
include 'file.sf';
```


Include a file as module (file must exists in '''SIDEF_INC''' as '''Some/Name.sm'''):

```ruby
include Some::Name;
# variables are available here as: Some::Name::var_name
```



## Smalltalk

there is no such thing as source-file inclusion in Smalltalk. However, in a REPL or anywhere in code, source code can be loaded with:

```smalltalk>aFilename asFilename readStream fileIn</lang

or:

```smalltalk>Smalltalk fileIn: aFilename</lang

In Smalltalk/X, which supports binary code loading, aFilename may either be sourcecode or a dll containing a precompiled class library.


## SNOBOL4

{{Works with|SNOBOL4}}
{{Works with|Spitbol}}

```SNOBOL4
-INCLUDE "path/to/filename.inc"
```


## SPL


```spl
$include.txt
```



## Standard ML

{{Works with|SML/NJ}}

```sml
use "path/to/file";
```



## Tcl

The built-in <code>source</code> command does exactly inclusion of code into the currently executing scope, subject to minor requirements of being well-formed Tcl script that is sourced in the first place (and the ability to introspect via <code>info script</code>):

```tcl
source "foobar.tcl"
```


Note that it is more usually considered good practice to arrange code into ''packages'' that can be loaded in with more regular semantics (including version handling, only-once semantics, integration of code written in other languages such as [[C]], etc.)

```tcl>package require foobar 1.3</lang

In the case of packages that are implemented using Tcl code, these will actually be incorporated into the program using the <code>source</code> command, though this is formally an implementation detail of those packages.


## UNIX Shell


With Bourne-compatible shells, the ''dot operator'' includes another file.

{{works with|Bourne Shell}}

```bash>. myfile.sh    # Include the contents of myfile.sh </lang


=
## C Shell
=

```csh>source myfile.csh</lang


=
## Bash
=

```shell
. myfile.sh
source myfile.sh
```


GNU Bash has both  <code>.</code> and the C-Shell style <code>source</code>.  See [http://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-source Bash manual on <code>source</code>]


## Ursa

Ursa can read in and execute another file using the import statement, similar to Python.

```ursa
import "filename.u"
```



## Vala

Importing/including is done during compilation. For example, to compile the program called "maps.vala" with the package "gee":

```txt

valac maps.vala --pkg gee-1.0

```


Functions can be called then using Gee.<function> calls:

```vala
var map = new Gee.HashMap<string, int> ();
```


or with a using statement:

```vala
using Gee;

var map = new HashMap<string, int>();
```



## VBScript

VBScript doesn't come with an explicit include (unless you use the wsf form). Fortunately vbscript has the Execute and ExecuteGlobal commands which allow you to add code dynamically into the local (disappears when the code goes out of scope) or global namespaces. Thus, all you have to do to include code from a file is read the file into memory and ExecuteGlobal on that code. Just pass the filename to this sub and all is golden. If you want an error to occur if the file is not found then just remove the FileExists test.


```vb

Include "D:\include\pad.vbs"

Wscript.Echo lpad(12,14,"-")

Sub Include (file)
   dim fso: set fso = CreateObject("Scripting.FileSystemObject")
   if fso.FileExists(file) then ExecuteGlobal fso.OpenTextFile(file).ReadAll
End Sub

```

If you use the wsf form you can include a file by

```vbscript

<script id="Connections" language="VBScript" src="D:\include\ConnectionStrings.vbs"/>


```


If you use the following form then you can define an environment variable, %INCLUDE% and make your include library more portable as in


```vbscript

Include "%INCLUDE%\StrFuncs.vbs"

Function Include ( ByVal file )
    Dim wso: Set wso = CreateObject("Wscript.Shell")
    Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")
    ExecuteGlobal(fso.OpenTextFile(wso.ExpandEnvironmentStrings(file)).ReadAll)
End Function

```



## Verbexx


```verbexx
/*******************************************************************************
*   /#   @INCLUDE file:"filename.filetype"
*        - file: is just the filename
*        - actual full pathname is VERBEXX_INCLUDE_PATH\filename.filetype
*          where VERBEXX_INCLUDE_PATH is the contents of an environment variable
*
*   /#   @INCLUDE file:"E:\xxx\xxx\xxx\filename.filetype"
*        - file: specifies the complete pathname of file to include
*
*   @INCLUDE verb can appear only in pre-processor code (after /#  /{  etc.)
*******************************************************************************/

/{  //////////////////////////////////////////////// start of pre-processor code

    @IF (@IS_VAR include_counter)
        else:{@VAR include_counter global: = 0};  // global, so all code sees it

    include_counter++;
    @SAY "    In pre-processor -- include counter = " include_counter;

    @IF (include_counter < 3)
        then:{@INCLUDE file:"rosetta\include_a_file.txt"};     // include self

}/  ////////////////////////////////////////////////// end of pre-processor code

@SAY "Not in pre-processor -- include_counter = " include_counter;
/]
Output:      In preprocessor -- include_counter = 1
             In preprocessor -- include_counter = 2
             In preprocessor -- include_counter = 3
         Not in preprocessor -- include_counter = 3
         Not in preprocessor -- include_counter = 3
         Not in preprocessor -- include_counter = 3
```



## x86 Assembly


{{works with|FASM on Windows}}

```asm
include 'MyFile.INC'
```


{{works with|nasm}}

```asm
%include "MyFile.INC"
```



## XPL0


```XPL0
include c:\cxpl\stdlib;
DateOut(0, GetDate)
```


{{out}}

```txt

09-28-12

```



## zkl


```zkl
include(vm.h.zkl, compiler.h.zkl, zkl.h.zkl, opcode.h.zkl);
```



## ZX Spectrum Basic


It is possible to include the contents of another program using the merge command. However, line numbers that coincide with those of the original program shall be overwritten, so it is best to reserve a block of line numbers for merged code:


```zxbasic
10 GO TO 9950
5000 REM We reserve line numbers 5000 to 8999 for merged code
9000 STOP: REM In case our line numbers are wrong
9950 REM Merge in our module
9955 MERGE "MODULE"
9960 REM Jump to the merged code. Pray it has the right line numbers!
9965 GO TO 5000
```



{{omit from|F Sharp}}
{{omit from|GUISS}}
{{omit from|NetRexx}}
{{omit from|VBA}}
{{omit from|Visual Basic}}

[[Category:Basic language learning]]
[[Category:Initialization]]
