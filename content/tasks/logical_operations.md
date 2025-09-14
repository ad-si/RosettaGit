+++
title = "Logical operations"
description = ""
date = 2019-10-11T15:50:40Z
aliases = []
[extra]
id = 2250
[taxonomies]
categories = ["task", "Basic Data Operations"]
tags = []
languages = [
  "360_assembly",
  "acl2",
  "ada",
  "agda",
  "aikido",
  "aime",
  "algol_68",
  "algol_w",
  "apex",
  "apl",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "awk",
  "axe",
  "basic",
  "basic256",
  "bbc_basic",
  "bc",
  "bracmat",
  "brat",
  "c",
  "clipper",
  "clojure",
  "cobol",
  "coldfusion",
  "commodore_basic",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dc",
  "delphi",
  "dwscript",
  "dyalect",
  "e",
  "ecl",
  "efene",
  "elena",
  "elixir",
  "elm",
  "erlang",
  "euphoria",
  "excel",
  "factor",
  "false",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "funl",
  "futurebasic",
  "gap",
  "gecho",
  "genie",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "hexiscript",
  "hicest",
  "holyc",
  "hy",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lil",
  "livecode",
  "llvm",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "mathematica",
  "maxima",
  "maxscript",
  "metafont",
  "min",
  "mumps",
  "neko",
  "nemerle",
  "netrexx",
  "newlisp",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "ooc",
  "openedge_progress",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pop11",
  "postscript",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "quickbasic",
  "r",
  "racket",
  "rascal",
  "rebol",
  "retro",
  "rexx",
  "ring",
  "rlab",
  "robotic",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "self",
  "sidef",
  "skookumscript",
  "slate",
  "smalltalk",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "toka",
  "ubasic_4th",
  "v",
  "vala",
  "visual_basic_dotnet",
  "xlisp",
  "xpl0",
  "xslt",
  "zkl",
]
+++

## Task

Write a function that takes two logical (boolean) values, and outputs the result of "and" and "or" on both arguments as well as "not" on the first arguments.

If the programming language doesn't provide a separate type for logical values, use the type most commonly used for that purpose.

If the language supports additional logical operations on booleans such as XOR, list them as well.





## 360 Assembly

Assembler 360 offers a full set of opcodes for logical operations: or, and, xor (exclusive or).
The "not" can be done by inversing the branching: BNE (Branch Not Equal) instead of BE (Branch Equal).
An othe way to perform a not is to use a xor with the true value (X'FF').

```txt

 Op-codes
                     Or    And   Xor
                     ---   ---   ---
 Memory to memory    OC    NC    XC
 Memory to register  O     N     X
 Immediate           OI    NI    XI

```


An example:

```360asm
*        Logical operations       04/04/2017
LOGICAL  CSECT
         USING  LOGICAL,R15
*     -- C=A and B
         MVC    C,A                C=A
         NC     C,B                C=A and B
*     -- C=A or B
         MVC    C,A                C=A
         OC     C,B                C=A or B
*     -- C=not A
         MVC    C,A                C=A
         XI     C,X'01'            C=not A
*     -- if C then goto e
         CLI    C,X'01'            if C
         BE     E                  then goto e
         XPRNT  =C'FALSE',5
*
E        BR     R14
TRUE     DC     X'01'
FALSE    DC     X'00'
A        DC     X'01'
B        DC     X'00'
C        DS     X
PG       DC     CL80' '
         YREGS
         END    LOGICAL
```

```txt

FALSE

```



## ACL2



```lisp
(defun logical-ops (a b)
   (progn$ (cw "(and a b) = ~x0~%" (and a b))
           (cw "(or a b)  = ~x0~%" (or a b))
           (cw "(not a) =   ~x0~%" (not a))))
```






## Ada


I have also included logical xor because it is defined for Ada boolean types.
All the operators below work equally well on arrays of boolean types.
In fact, a packed array of boolean is an array of bits,
providing a direct link between logical and bitwise operations.


```ada
procedure Print_Logic(A : Boolean; B : Boolean) is
begin
   Put_Line("A and B is " & Boolean'Image(A and B));
   Put_Line("A or B  is " & Boolean'Image(A or B));
   Put_Line("A xor B is " & Boolean'Image(A xor B));
   Put_Line("not A   is " & Boolean'Image(not A));
end Print_Logic;
```



## Agda



```agda
module AndOrNot where

open import Data.Bool
open import Data.Product

test : Bool → Bool → Bool × Bool × Bool
test x y = x ∧ y , x ∨ y , not x
```


e.g.

 test true false ⇒ false , true , false


## Aikido



```aikido

function logic(a,b) {
  println("a AND b: " + (a && b))
  println("a OR b: " + (a || b))
  println("NOT a: " + (!a))
}

```



## Aime



```aime
void
out(integer a, integer b)
{
    o_integer(a && b);
    o_byte('\n');
    o_integer(a || b);
    o_byte('\n');
    o_integer(!a);
    o_byte('\n');
}
```



## ALGOL 68



```algol68
PROC print_logic = (BOOL a, b)VOID:
(
# for a 6-7 bit/byte compiler #
  printf(($"a and b is "gl$, a AND b);
  printf(($"a or b is "gl$, a OR b);
  printf(($"not a is "gl$, NOT a);
  printf(($"a equivalent to b is "gl$, a EQ b);
  printf(($"a not equivalent to b is "gl$, a NE b);

# Alternatively ASCII #
  printf(($"a and b is "gl$, a & b);
  printf(($"a and b is "gl$, a /\ b);  <!-- http://web.archive.org/web/20021207211127/http://www.bobbemer.com/BRACES.HTM -->
  printf(($"a or b is "gl$, a \/ b);
  printf(($"a equivalent to b "gl$, a = b);
  printf(($"a not equivalent to b "gl$, a /= b);

¢ for a European 8 bit/byte charcter set eg. ALCOR or GOST ¢
  printf(($"a and b is "gl$, a ∧ b);
  printf(($"a or b is "gl$, a ∨ b);
  printf(($"not a is "gl$, ¬ a)
  printf(($"a not equivalent to b is "gl$, a ≠ b)
)
```



## ALGOL W


```algolw
procedure booleanOperations( logical value a, b ) ;
    begin

        % algol W has the usual "and", "or" and "not" operators         %
        write( a,      " and ", b, ": ", a and   b );
        write( a,      "  or ", b, ": ", a  or   b );
        write( "         not ", a, ": ",   not   a );

        % logical values can be compared with the = and not = operators %
        %     a not = b can be used for a xor b                         %
        write( a,      " xor ", b, ": ", a not = b );
        write( a,      " equ ", b, ": ", a     = b );

    end booleanOperations ;
```



## Apex


```Java
boolean a = true;
boolean b = false;
System.Debug('a AND b: ' + (a && b));
System.Debug('a OR b: ' + (a || b));
System.Debug('NOT a: ' + (!a));
System.Debug('a XOR b: ' + (a ^ b));

```



## APL

APL represents Boolean values using 1 and 0. This function takes Boolean arguments before it and after it—which may be arrays of Booleans—and returns an array consisting of arg1 AND arg2, arg1 OR arg2, NOT arg1, arg1 NAND arg2, arg1 NOR arg2, and arg1 XOR arg2, in that order.

```apl
      LOGICALOPS←{(⍺∧⍵)(⍺∨⍵)(~⍺)(⍺⍲⍵)(⍺⍱⍵)(⍺≠⍵)}
```



## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program logicoper.s   */
/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1
/* Initialized data */
.data
szMessResultAnd:   .asciz "Result of And : \n"
szMessResultOr:    .asciz "Result of Or : \n"
szMessResultEor:   .asciz "Result of Exclusive Or : \n"
szMessResultNot:   .asciz "Result of Not : \n"
szMessResultClear: .asciz "Result of Bit Clear : \n"

sMessAffBin: .ascii "Register value : "
sZoneBin:    .space 36,' '
             .asciz "\n"

/* code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}     /* save 2 registers */

    mov r0,#0b1100      @ binary value 1
    mov r1,#0b0110      @ binary value 2
    bl logicfunc

100:   @ standard end of the program
    mov r0,#0                   @ return code
    pop {fp,lr}                 @ restore 2 registers
    mov r7,#EXIT                @ request to exit program
    swi 0                       @ perform the system call

/******************************************************************/
/*     logics functions                              */
/******************************************************************/
/* r0 contains the first value */
/* r1 contains the second value */
logicfunc:
    push {r2,lr}                     @ save  registers
    mov r2,r0                        @ save value 1 in r2
    ldr r0,iAdrszMessResultAnd       @ and
    bl affichageMess
    mov r0,r2                        @ load value 1 in r0
    and r0,r1
    bl affichage2
    ldr r0,iAdrszMessResultOr        @ or
    bl affichageMess
    mov r0,r2
    orr r0,r1
    bl affichage2
    ldr r0,iAdrszMessResultEor       @ exclusive or
    bl affichageMess
    mov r0,r2
    eor r0,r1
    bl affichage2
    ldr r0,iAdrszMessResultNot       @ not
    bl affichageMess
    mov r0,r2
    mvn r0,r1
    bl affichage2
    ldr r0,iAdrszMessResultClear     @ bit clear
    bl affichageMess
    mov r0,r2
    bic r0,r1
    bl affichage2
100:
    pop {r2,lr}                      @ restore registers
    bx lr
iAdrszMessResultAnd:    .int szMessResultAnd
iAdrszMessResultOr:     .int szMessResultOr
iAdrszMessResultEor:    .int szMessResultEor
iAdrszMessResultNot:    .int szMessResultNot
iAdrszMessResultClear:  .int szMessResultClear
/******************************************************************/
/*     register display in binary                              */
/******************************************************************/
/* r0 contains the register */
affichage2:
    push {r0,lr}     /* save registers */
    push {r1-r5}     /* save other registers */
    mrs r5,cpsr      /* saves state register in r5 */
    ldr r1,iAdrsZoneBin
    mov r2,#0         @ read bit position counter
    mov r3,#0         @ position counter of the written character
1:                @ loop
    lsls r0,#1        @ left shift  with flags
    movcc r4,#48      @ flag carry off   character '0'
    movcs r4,#49      @ flag carry on    character '1'
    strb r4,[r1,r3]   @ character ->   display zone
    add r2,r2,#1      @ + 1 read bit position counter
    add r3,r3,#1      @ + 1 position counter of the written character
    cmp r2,#8         @ 8 bits read
    addeq r3,r3,#1    @ + 1 position counter of the written character
    cmp r2,#16        @ etc
    addeq r3,r3,#1
    cmp r2,#24
    addeq r3,r3,#1
    cmp r2,#31        @ 32 bits shifted ?
    ble 1b            @ no -> loop

    ldr r0,iAdrsZoneMessBin    @ address of message result
    bl affichageMess           @ display result

100:
    msr cpsr,r5    /* restore state register */
    pop {r1-r5}    /* restore other registers */
    pop {r0,lr}
    bx lr
iAdrsZoneBin: .int sZoneBin
iAdrsZoneMessBin: .int sMessAffBin

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save registers */
    push {r0,r1,r2,r7}    		/* save others registers */
    mov r2,#0   				/* counter length */
1:      	            /* loop length calculation */
    ldrb r1,[r0,r2]  			/* read byte start position + index */
    cmp r1,#0       			/* if 0 it's over */
    addne r2,r2,#1   			/* else add 1 to the length */
    bne 1b          			/* and loop */
                                /* so here r2 contains the length of the message */
    mov r1,r0        			/* address message in r1 */
    mov r0,#STDOUT      		/* code to write to the standard output */
    mov r7,#WRITE               /* "write" system call */
    swi #0                      /* system call */
    pop {r0,r1,r2,r7}     		/* restore other registers */
    pop {fp,lr}    				/* restore 2 registers */
    bx lr	        			/* return */



```



## Arturo


```arturo
logic [a,b]{
	print "a AND b = " + $(and a b)
	print "a OR b = " + $(or a b)
	print "NOT a = " + $(not a)
}

logic true false
```

```txt
a AND b = false
a OR b = true
NOT a = false
```



## AutoHotkey


```AutoHotkey
a = 1
b = 0
msgbox % "a and b is " . (a && b)
msgbox % "a or b is " . (a || b)
msgbox % "not a is " . (!a)
```



## AWK


```awk
$ awk '{print "and:"($1&&$2),"or:"($1||$2),"not:"!$1}'
0 0
and:0 or:0 not:1
0 1
and:0 or:1 not:1
1 0
and:0 or:1 not:0
1 1
and:1 or:1 not:0
```



## Axe


```axe
Lbl LOGIC
r₁→A
r₂→B
Disp "AND:",(A?B)▶Dec,i
Disp "OR:",(A??B)▶Dec,i
Disp "NOT:",(A?0,1)▶Dec,i
Return
```


Note that unlike [[TI-83 BASIC]], the "and", "or", "xor", and "not(" tokens in Axe are bitwise operators, not logical operators.


## BASIC


=
## Commodore BASIC
=
In Commodore BASIC 'True' is -1 and 'False' is 0. There is no operation for 'exclusive-or'.

```qbasic
10 A = -1
20 B = 0
30 PRINT A AND B
40 PRINT A OR B
50 PRINT (A AND (NOT B)) OR ((NOT A) AND B)
60 PRINT NOT A
```


```txt
0
-1
-1
0
```


=
## BASIC256
=

```BASIC256
a = true
b = false
print a and b
print a or b
print a xor b
print not a
```


=
## BBC BASIC
=

```bbcbasic
      PROClogic(FALSE, FALSE)
      PROClogic(FALSE, TRUE)
      PROClogic(TRUE, FALSE)
      PROClogic(TRUE, TRUE)
      END

      DEF PROClogic(a%, b%)
      LOCAL @% : @% = 2 : REM Column width
      PRINT a% " AND " b% " = " a% AND b% TAB(20);
      PRINT a% " OR "  b% " = " a% OR b%  TAB(40);
      PRINT a% " EOR " b% " = " a% EOR b% TAB(60);
      PRINT " NOT " a% " = " NOT a%
      ENDPROC
```

```txt

 0 AND  0 =  0       0 OR  0 =  0        0 EOR  0 =  0       NOT  0 = -1
 0 AND -1 =  0       0 OR -1 = -1        0 EOR -1 = -1       NOT  0 = -1
-1 AND  0 =  0      -1 OR  0 = -1       -1 EOR  0 = -1       NOT -1 =  0
-1 AND -1 = -1      -1 OR -1 = -1       -1 EOR -1 =  0       NOT -1 =  0

```


=
## QuickBASIC
=
```qbasic
SUB logic (a%, b%) 'no booleans in BASIC...these are integers. 1 for true 0 for false.
  PRINT a AND b
  PRINT a OR b
  PRINT NOT a
END SUB
```


=
## FreeBASIC
=
In addition to And, Or and Not FreeBASIC supports several other logical operators:

* XOr - Exclusive Or : true if both operands are different, false if they're the same
* Eqv - Equivalence  : true if both operands are the same, false if they're different
* Imp - Implication  : true unless the first operand is true and the second operand is false when it is false


There are also 'short-circuiting' operators:

* AndAlso - Same as AND but the second operand is only evaluated if the first is true
* OrElse  - Same as OR but the second operand is only evaluated if the first is false


The following program illustrates the use of these operators:


```freebasic
' FB 1.05.0 Win64

Sub logicalDemo(b1 As Boolean, b2 As Boolean)
  Print "b1             = "; b1
  Print "b2             = "; b2
  Print "b1 And b2      = "; b1 And b2
  Print "b1 Or b2       = "; b1 Or b2
  Print "b1 XOr b2      = "; b1 Xor b2
  Print "b1 Eqv b2      = "; b1 Eqv b2
  Print "b1 Imp b2      = "; b1 Imp b2
  Print "Not b1         = "; Not b1
  Print "b1 AndAlso b2  = "; b1 AndAlso b2
  Print "b1 OrElse b2   = "; b1 OrElse b2
  Print
End Sub

Dim b1 As Boolean = True
Dim b2 As Boolean = True
logicalDemo b1, b2
b2 = False
logicalDemo b1, b2
b1 = False
logicalDemo b1, b2
b2 = True
logicalDemo b1, b2
Print "Press any key to quit"
Sleep
```


```txt

b1             = true
b2             = true
b1 And b2      = true
b1 Or b2       = true
b1 XOr b2      = false
b1 Eqv b2      = true
b1 Imp b2      = true
Not b1         = false
b1 AndAlso b2  = true
b1 OrElse b2   = true

b1             = true
b2             = false
b1 And b2      = false
b1 Or b2       = true
b1 XOr b2      = true
b1 Eqv b2      = false
b1 Imp b2      = false
Not b1         = false
b1 AndAlso b2  = false
b1 OrElse b2   = true

b1             = false
b2             = false
b1 And b2      = false
b1 Or b2       = false
b1 XOr b2      = false
b1 Eqv b2      = true
b1 Imp b2      = true
Not b1         = true
b1 AndAlso b2  = false
b1 OrElse b2   = false

b1             = false
b2             = true
b1 And b2      = false
b1 Or b2       = true
b1 XOr b2      = true
b1 Eqv b2      = false
b1 Imp b2      = true
Not b1         = true
b1 AndAlso b2  = false
b1 OrElse b2   = true

```



## bc

POSIX bc has neither Boolean values nor built-in logical operations.
Thus one has to write them oneself:

```bc
/* The following three functions assume 0 is false and 1 is true */

/* And */
define a(x, y) {
    return(x * y)
}

/* Or */
define o(x, y) {
    return(x + y - x * y)
}

/* Not */
define n(x) {
    return(1 - x)
}

define f(a, b) {
    "a and b: "
    a(a, b)
    "a or b: "
    o(a, b)
    "not a: "
    n(a)
}
```


GNU bc's extensions make this task much easier:

```bc
define logic_test(a, b) {
    print "a and b: ", a && b, "\n"
    print "a or b: ", a || b, "\n"
    print "not a: ", !a, "\n"
}
```



## Bracmat

Bracmat has no boolean values. Instead, each expression has, apart from its value, also a S/F/I (SUCCEEDED/FAILED/IGNORE) feature, where the latter is used in the exceptional case that the success or failure of an expression should not influence the program flow.

The expression <code>~</code> is special in that it always fails. Most expressions only fail in exceptional cases, such as when a file cannot be opened. Match expressions stand apart from the rest and can be compared to expressions with comparison operations in other languages.

In the example below, the empty string represents 'true' and <code>~</code> represents 'false'. The binary operators <code>&</code> and <code>|</code>, which normally are used as the glue between expressions such as match operations, function definitions and function calls, are used as the logical operators 'and' and 'or', respectively.


```bracmat
( ( Logic
  =   x y
    .   '$arg:(=?x,?y)
      &   str
        $ ( "\n(x,y)="
            !arg
            ( ":\n"
              "x and y -> "
              ( (!x&!y)&true
              | false
              )
            )
            ( \n
              "x or y -> "
              ( (!x|!y)&true
              | false
              )
            )
            "\nnot x -> "
            (~!x&true|false)
          )
  )
& out$(Logic$(,))
& out$(Logic$(~,))
& out$(Logic$(,~))
& out$(Logic$(~,~))
);
```

```txt
(x,y)=(,):
x and y -> true
x or y -> true
not x -> false

(x,y)=(~,):
x and y -> false
x or y -> true
not x -> true

(x,y)=(,~):
x and y -> false
x or y -> true
not x -> false

(x,y)=(~,~):
x and y -> false
x or y -> false
not x -> true
```



## Brat


```brat
logic = { a, b |
  p "a and b: #{ a && b }"
  p "a or b: #{ a || b }"
  p "not a: #{ not a }"
}
```



## C



```c
void print_logic(int a, int b)
{
  printf("a and b is %d\n", a && b);
  printf("a or b is %d\n", a || b);
  printf("not a is %d\n", !a);
}
```



## C++



```cpp
void print_logic(bool a, bool b)
{
  std::cout << std::boolalpha; // so that bools are written as "true" and "false"
  std::cout << "a and b is " << (a && b) << "\n";
  std::cout << "a or b is " << (a || b) << "\n";
  std::cout << "not a is " << (!a) << "\n";
}
```


## C#

```c#
using System;

namespace LogicalOperations
{
    class Program
    {
        static void Main(string[] args)
        {
            bool a = true, b = false;
            Console.WriteLine("a and b is {0}", a && b);
            Console.WriteLine("a or b is {0}", a || b);
            Console.WriteLine("Not a is {0}", !a);
            Console.WriteLine("a exclusive-or b is {0}", a ^ b);
        }
    }
}
```




## Clipper


```clipper
 Function Foo( a, b )
   // a and b was defined as .F. (false) or .T. (true)
   ? a .AND. b
   ? a .OR. b
   ? .NOT. a, .NOT. b
   Return Nil

```



## Clojure


```clojure

(defn logical [a b]
  (prn (str "a and b is " (and a b)))
  (prn (str "a or b is " (or a b)))
  (prn (str "not a is "  (not a))))

(logical true false)

```



## COBOL

Logical operations in COBOL are exactly the same as [[Bitwise operations#COBOL|bitwise operations]].

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. print-logic.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  result                  PIC 1 USAGE BIT.

       LINKAGE SECTION.
       01  a                       PIC 1 USAGE BIT.
       01  b                       PIC 1 USAGE BIT.

       PROCEDURE DIVISION USING a, b.
           COMPUTE result = a B-AND b
           DISPLAY "a and b is " result

           COMPUTE result = a B-OR b
           DISPLAY "a or b is " result

           COMPUTE result = B-NOT a
           DISPLAY "Not a is " result

           COMPUTE result = a B-XOR b
           DISPLAY "a exclusive-or b is " result

           GOBACK
           .
```



## ColdFusion


```cfm
<cffunction name = "logic" hint = "Performs basic logical operations">
  <cfargument name = "a" required = "yes" type = "boolean" />
  <cfargument name = "a" required = "yes" type = "boolean" />
  <cfoutput>
    'A' AND 'B' is #a AND b#< br />
    'A' OR  'B' is #a OR  b#< br />
    NOT 'A'     is #!a#
  </cfoutput>
</cffunction>
```



## Common Lisp



```lisp
(defun logic (a b)
  (print "a and b is") (write (and a b))
  (print "a or b is" ) (write (or a b))
  (print "not a is"  ) (write (not a)))
```



## D


```d
import std.stdio;

void logic(T, U)(T lhs, U rhs) {
    writefln("'%s' is of type '%s', '%s' is of type '%s';",
             lhs, typeid(typeof(lhs)), rhs,typeid(typeof(rhs)));
    writefln("\t'%s' AND '%s' is %s, ", lhs, rhs, lhs && rhs);
    writefln("\t'%s' OR '%s' is %s, ", lhs, rhs, lhs || rhs);
    writefln("\tNOT '%s' is %s.\n", lhs, !lhs);
}

class C { int value; }

void main() {
    bool theTruth = true;
    bool theLie = false;
    real zeroReal = 0.0L;
    real NaN; // D initializes floating point values to NaN
    int zeroInt  = 0;
    real[] nullArr = null;
    string emptyStr = "";
    string nullStr = null;
    C someC = new C;
    C nullC = null;

    // Note: Struct is value type in D, but composite
    //  so no default bool equivalent.

    logic(theTruth, theLie);
    logic(zeroReal, NaN);
    logic(zeroInt, nullArr);
    logic(nullStr, emptyStr);
    logic(someC, nullC);
}
```

```txt
'true' is of type 'bool', 'false' is of type 'bool';
    'true' AND 'false' is false,
    'true' OR 'false' is true,
    NOT 'true' is false.

'0' is of type 'real', 'nan' is of type 'real';
    '0' AND 'nan' is false,
    '0' OR 'nan' is true,
    NOT '0' is true.

'0' is of type 'int', '[]' is of type 'real[]';
    '0' AND '[]' is false,
    '0' OR '[]' is false,
    NOT '0' is true.

'' is of type 'immutable(char)[]', '' is of type 'immutable(char)[]';
    '' AND '' is false,
    '' OR '' is true,
    NOT '' is true.

'logical_operations.C' is of type 'logical_operations.C', 'null' is of type 'logical_operations.C';
    'logical_operations.C' AND 'null' is false,
    'logical_operations.C' OR 'null' is true,
    NOT 'logical_operations.C' is false.
```



## Dc


```dc
[ 1 q ] sT

[ 0=T 0 ] s!
[ l! x S@ l! x L@ + l! x ] s&
[ l! x S@ l! x L@ * l! x ] s|

[ 48 + P ] s.

[ Sb Sa
  la l. x [ ] P lb l. x [  ] P
  la lb l& x l. x [   ] P
  la Lb l| x l. x [   ] P
  La l! x l. x
  A P
] sF

[a b a&b a|b !a] P A P
0 0 lF x
0 1 lF x
1 0 lF x
1 1 lF x
```

```txt

a b a&b a|b !a
0 0  0   0   1
0 1  0   1   1
1 0  0   1   0
1 1  1   1   0

```



## Delphi


```Delphi
program LogicalOperations;

{$APPTYPE CONSOLE}

const
  a = True;
  b = False;
begin
  Write('a = ');
  Writeln(a);
  Write('b = ');
  Writeln(b);
  Writeln;

  Write('a AND b: ');
  Writeln(a AND b);

  Write('a OR b: ');
  Writeln(a OR b);

  Write('NOT a: ');
  Writeln(NOT a);

  Write('a XOR b: ');
  Writeln(a XOR b);
end.
```


```txt
a = TRUE
b = FALSE

a AND b: FALSE
a OR b: TRUE
NOT a: FALSE
a XOR b: TRUE
```


=={{header|Déjà Vu}}==

```dejavu
showbool a b:
    !.( a b or a b and a b xor a b not a )

for a in [ false true ]:
    for b in [ false true ]:
        showbool a b
```

```txt
true true true true false false
true false true false true false
false true true false true true
false false false false false true
```



## DWScript


```Delphi
var a := True;
var b := False;

Print('a = ');
PrintLn(a);
Print('b = ');
PrintLn(b);

Print('a AND b: ');
PrintLn(a AND b);

Print('a OR b: ');
PrintLn(a OR b);

Print('NOT a: ');
PrintLn(NOT a);

Print('a XOR b: ');
PrintLn(a XOR b);
```

```txt
a = True
b = False
a AND b: False
a OR b: True
NOT a: False
a XOR b: True
```



## Dyalect



```dyalect
var a = true
var b = false
print("a and b is \(a && b)")
print("a or b is \(a || b)")
print("Not a is \(!a)")
print("a exclusive-or b is \(a ^ b)")
```



## E



```e
def logicalOperations(a :boolean, b :boolean) {
    return ["and" => a & b,
            "or"  => a | b,
            "not" => !a,
            "xor" => a ^ b]
}
```


Each of these is a method on [http://wiki.erights.org/wiki/Boolean boolean objects]; the above is precisely equivalent to:


```e
def logicalOperations(a :boolean, b :boolean) {
    return ["and" => a.and(b),
            "or"  => a.or(b),
            "not" => a.not(),
            "xor" => a.xor(b)]
}
```


If the <code>:boolean</code> guards were removed, these operations would also work on other types, such as sets (&amp; is union and | is intersection; <code>not</code> is not supported).


## ECL


```ECL

LogicalOperations(BOOLEAN A,BOOLEAN B) := FUNCTION
  ANDit := A AND B;
  ORit  := A OR B;
  NOTA  := NOT A;
  XORit := (A OR B) AND NOT (A AND B);
  DS    := DATASET([{A,B,'A AND B is:',ANDit},
                    {A,B,'A OR B is:',ORit},
                    {A,B,'NOT A is:',NOTA},
                    {A,B,'A XOR B is:',XORit}],
                    {BOOLEAN AVal,BOOLEAN BVal,STRING11 valuetype,BOOLEAN val});
  RETURN DS;
END;

LogicalOperations(FALSE,FALSE);
LogicalOperations(FALSE,TRUE);
LogicalOperations(TRUE,FALSE);
LogicalOperations(TRUE,TRUE);
LogicalOperations(1>2,1=1); //Boolean expressions are also valid here

```



## Efene



```efene
compare_bool = fn (A, B) {
    io.format("~p and ~p = ~p~n", [A, B, A and B])
    io.format("~p or ~p = ~p~n", [A, B, A or B])
    io.format("not ~p = ~p~n", [A, not A])
    io.format("~p xor ~p = ~p~n", [A, B, A xor B])
    io.format("~n")
}

@public
run = fn () {
    compare_bool(true, true)
    compare_bool(true, false)
    compare_bool(false, true)
    compare_bool(false, false)
}

```


## Elena

ELENA 4.x:

```elena
import extensions;

public program()
{
    bool a := true;
    bool b := false;

    console.printLine("a and b is ", a && b);
    console.printLine("a or b is ", a || b);
    console.printLine("Not a is ", a.Inverted);
    console.printLine("a xor b is ", a ^^ b)
}
```

```txt

a and b is false
a or b is true
Not a is false
a xor b is true

```



## Elixir

Elixir also provides three boolean operators: <code>or</code>, <code>and</code> and <code>not</code>. These operators are strict in the sense that they expect a boolean (<code>true</code> or <code>false</code>) as their first argument:

```elixir
iex(1)> true and false
false
iex(2)> false or true
true
iex(3)> not false
true
```

<code>or</code> and <code>and</code> are short-circuit operators. They only execute the right side if the left side is not enough to determine the result:

Besides these boolean operators, Elixir also provides <code>||</code>, <code>&amp;&amp;</code> and <code>!</code> which accept arguments of any type. For these operators, all values except <code>false</code> and <code>nil</code> will evaluate to true:

```elixir
(28)> nil || 23
23
iex(29)> [] || false
[]
iex(30)> nil && true
nil
iex(31)> 0 && 15
15
iex(32)> ! true
false
iex(33)> ! nil
true
iex(34)> ! 3.14
false
```

As a rule of thumb, use <code>and</code>, <code>or</code> and <code>not</code> when you are expecting booleans. If any of the arguments are non-boolean, use <code>&amp;&amp;</code>, <code>||</code> and <code>!</code>.


## Elm


```Elm

--Open cmd and elm-repl and directly functions can be created

--Creating Functions
t=True
f=False
opand a b= a && b
opor a b= a || b
opnot a= not a

--Using the created Functions
opand t f
opor t f
opnot f

--Output will be False, True and True of type Boolean!
--end

```



## Erlang


```Erlang>1
 true and false.
false
2> false or true.
true
3> true xor false.
true
4> not false.
true
5> not (true and true).
false
```



## Euphoria


```euphoria
procedure print_logic(integer a, integer b)
    printf(1,"a and b is %d\n", a and b)
    printf(1,"a or b is %d\n", a or b)
    printf(1,"a xor b is %d\n", a xor b)
    printf(1,"not a is %d\n", not a)
end procedure
```



## Excel


If the values are typed in cells A1 and B1, type in the following in cell C1


```excel

=CONCATENATE($A1, " AND ", $B1, " is ", AND($A1,$B1))

```


In D1


```excel

=CONCATENATE($A1, " OR ", $B1, " is ", OR($A1,$B1))

```


In E1


```excel

=CONCATENATE(" NOT ", $A1, " is ", NOT($A1))

```


=={{header|F Sharp|F#}}==

```fsharp
let printLogic a b =
    printfn "a and b is %b" (a && b)
    printfn "a or b is %b" (a || b)
    printfn "Not a is %b" (not a)
    // The not-equals operator has the same effect as XOR on booleans.
    printfn "a exclusive-or b is %b" (a <> b)
```



## Factor


```factor
: logical-operators ( a b -- )
    {
        [ "xor is: " write xor . ]
        [ "and is: " write and . ]
        [ "or is:  " write or . ]
        [ "not is: " write drop not . ]
    } 2cleave ;
```



## FALSE

FALSE uses zero/non-zero for testing False and True. Comparison operators return -1 for True and 0 for False, which work with bitwise operators for logical operations.

```false
1 3=~["unequal, "]?
1 1= 1_=["true is -1, "]?
0~["false is 0, "]?
'm$'a>'z@>&["a < m < z"]?
```



## Fantom



```fantom

class Main
{
  static Void doOps (Bool arg1, Bool arg2)
  {
    echo ("$arg1 and $arg2 = ${arg1.and(arg2)}")
    echo ("$arg1 or $arg2 = ${arg1.or(arg2)}")
    echo ("not $arg1 = ${arg1.not}")
    echo ("$arg1 xor $arg2 = ${arg1.xor(arg2)}")
  }

  public static Void main ()
  {
    [true,false].each |Bool arg1|
    {
      [true,false].each |Bool arg2|
      {
        doOps (arg1, arg2)
      }
    }
  }
}

```



## Forth

Forth can use bitwise operators if the boolean values are well formed: TRUE (-1) and FALSE (0).  '''0<>''' converts an ill-formed flag (zero/non-zero) to a well-formed flag (false/true).

```forth
: .bool ( ? -- ) if ." true" else ." false" then ;
: logic ( a b -- ) 0<> swap 0<> swap
 cr ." a = " over .bool ."   b = " dup .bool
 cr ." a and b = " 2dup and .bool
 cr ." a  or b = " over  or .bool
 cr ." not a = " 0= .bool ;
```



## Fortran

In ANSI FORTRAN 66 or later, use LOGICAL data type:

```fortran
       SUBROUTINE PRNLOG(A, B)
       LOGICAL A, B
       PRINT *, 'a and b is ', A .AND. B
       PRINT *, 'a or b is ', A .OR. B
       PRINT *, 'not a is ', .NOT. A

C       You did not ask, but the following logical operators are also standard
C       since ANSI FORTRAN 66
C
### =================================================================


C       This yields the same results as .EQ., but has lower operator precedence
C       and only works with LOGICAL operands:
       PRINT *, 'a equivalent to b is ', A .EQV. B

C       This yields the same results as .NE., but has lower operator precedence
C       and only works with LOGICAL operands (this operation is also commonly
C       called "exclusive or"):
       PRINT *, 'a not equivalent to b is ', A .NEQV. B
       END
```




## FunL


```funl
def logical( a, b ) = println( """
a and b   = ${a and b}
a or b    = ${a or b}
not a     = ${not a}
a xor b   = ${a xor b}
""" )

for i <- [false, true], j <- [false, true] do logical( i, j )
```


```txt

a and b   = false
a or b    = false
not a     = true
a xor b   = false


a and b   = false
a or b    = true
not a     = true
a xor b   = true


a and b   = false
a or b    = true
not a     = false
a xor b   = true


a and b   = true
a or b    = true
not a     = false
a xor b   = false

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

def tab 6

dim as long a, b

print "In FB the Boolean constants _true = 1, _false = 0"
print string$( 39, "-" )
print " a", " b", "and",  "or", "xor", "nand", "nor"
print string$( 39, "-" )
a = _false: b = _false: print a, b, a and b, a or  b, a xor b, a nand b, a nor b
a = _false: b = _true:  print a, b, a and b, a or  b, a xor b, a nand b, a nor b
a = _true:  b = _false: print a, b, a and b, a or  b, a xor b, a nand b, a nor b
a = _true:  b = _true:  print a, b, a and b, a or  b, a xor b, a nand b, a nor b
print
print "FB also has shorthand operator expressions:
print string$( 39, "-" )
print " a", " b", "&&",  "||", "^^", "^&", "^|"
print string$( 39, "-" )
a = _false: b = _false: print a, b, a && b, a ||  b, a ^^ b, a ^& b, a ^| b
a = _false: b = _true:  print a, b, a && b, a ||  b, a ^^ b, a ^& b, a ^| b
a = _true:  b = _false: print a, b, a && b, a ||  b, a ^^ b, a ^& b, a ^| b
a = _true:  b = _true:  print a, b, a && b, a ||  b, a ^^ b, a ^& b, a ^| b

```


```txt

In FB the Boolean constants _true = 1, _false = 0
---------------------------------------
 a     b    and   or    xor   nand  nor
---------------------------------------
 0     0     0     0     0     0    -1
 0     1     0     1     1     0    -2
 1     0     0     1     1     1    -1
 1     1     1     1     0     0    -1

FB also has shorthand operator expressions:
---------------------------------------
 a     b    &&    ||    ^^    ^&    ^|
---------------------------------------
 0     0     0     0     0     0    -1
 0     1     0     1     1     0    -2
 1     0     0     1     1     1    -1
 1     1     1     1     0     0    -1

```



## GAP


```gap
Logical := function(a, b)
    return [ a or b, a and b, not a ];
end;

Logical(true, true);
# [ true, true, false ]

Logical(true, false);
# [ true, false, false ]

Logical(false, true);
# [ true, false, true ]

Logical(false, false);
# [ false, false, true ]
```



## gecho


```gecho>3 4 and</lang

3&&4

```gecho>1 2 or</lang

1||2


## Genie


```genie
[indent=4]
/*
   Logical operations in Genie
   valac logicals.gs
   ./logicals true false
*/

def logicals(a:bool, b:bool)
    print @"$a and $b is $(a and b)"
    print @"$a or $b is $(a or b)"
    print @"not $a is $(not a)"

init
    a:bool = bool.parse(args[1])
    b:bool = bool.parse(args[2])
    logicals(a, b)
```


```txt
prompt$ valac logicals.gs
prompt$ ./logicals true false
true and false is false
true or false is true
not true is false
```



## Go


```go
func printLogic(a, b bool) {
    fmt.Println("a and b is", a && b)
    fmt.Println("a or b is", a || b)
    fmt.Println("not a is", !a)
}
```

Other operators that work on type bool are == and !=.  == corresponds to the logical operation of equivalence.  != corresponds to exclusive or.


## Groovy


```groovy
def logical = { a, b ->
    println """
a AND b   = ${a} && ${b}   = ${a & b}
a OR b    = ${a} || ${b}   = ${a | b}
NOT a     = ! ${a}         = ${! a}
a XOR b   = ${a} != ${b}   = ${a != b}
a EQV b   = ${a} == ${b}   = ${a == b}
"""
}
```


Program:

```groovy
[true, false].each { a -> [true, false].each { b-> logical(a, b) } }
```


```txt
a AND b   = true && true   = true
a OR b    = true || true   = true
NOT a     = ! true         = false
a XOR b   = true != true   = false
a EQV b   = true == true   = true


a AND b   = true && false   = false
a OR b    = true || false   = true
NOT a     = ! true         = false
a XOR b   = true != false   = true
a EQV b   = true == false   = false


a AND b   = false && true   = false
a OR b    = false || true   = true
NOT a     = ! false         = true
a XOR b   = false != true   = true
a EQV b   = false == true   = false


a AND b   = false && false   = false
a OR b    = false || false   = false
NOT a     = ! false         = true
a XOR b   = false != false   = false
a EQV b   = false == false   = true
```



## Harbour


```visualfoxpro
PROCEDURE Foo( a, b )
   // a and b was defined as .F. (false) or .T. (true)
   ? a .AND. b
   ? a .OR. b
   ? ! a, ! b
   RETURN
```



## Haskell


Instead of a function and printing, which is unidiomatic for Haskell, here are the operations in the same style as in [[Bitwise operations]]:


```haskell
a = False
b = True

a_and_b = a && b
a_or_b  = a || b
not_a   = not a
a_xor_b  = a /= b
a_nxor_b = a == b
a_implies_b = a <= b -- sic!
```


(&&) and (||) are lazy on the second argument and therefore this operations are not symmetric:

```haskell
*Main > False && undefined
False
Prelude> undefined && False
*** Exception: Prelude.undefined
Prelude> True || undefined
True
Prelude> undefined || True
*** Exception: Prelude.undefined
```

(<=), (<), (>=) and (>) on the other hand are strict:

```haskell>Prelude
 False <= undefined
*** Exception: Prelude.undefined
Prelude> undefined <= True
*** Exception: Prelude.undefined
Prelude> True < undefined
*** Exception: Prelude.undefined
Prelude> undefined < False
*** Exception: Prelude.undefined
```


## hexiscript


```hexiscript
fun logic a b
  println "a and b = " + (a && b)
  println "a or  b = " + (a || b)
  println "  not a = " + (!a)
endfun
```



## HicEst

No logical variables. Nonzero is true, zero is false in logical expressions:

```hicest
  x     = value1 /= 0
  y     = value2 /= 0
  NOTx  = x == 0
  xANDy = x * y
  xORy  = x + y  /= 0
  EOR   = x /= y
```



## HolyC



```holyc
U0 PrintLogic(Bool a, Bool b) {
  Print("a and b is %d\n", a && b);
  Print("a or b is %d\n", a || b);
  Print("not a is %d\n", !a);
}

PrintLogic(TRUE, FALSE);
```



## Hy


```clojure
(defn logic [a b]
  (print "a and b:" (and a b))
  (print "a or b:" (or a b))
  (print "not a:" (not a)))
```



## Io


```io
printLogic := method(a,b,
  writeln("a and b is ", a and b)
  writeln("a or b is ", a or b)
  writeln("not a is ", a not)
)
```


=={{header|Icon}} and {{header|Unicon}}==
Icon/Unicon do not have a native logical or Boolean type; nor do they use Boolean values for flow control.  Instead for flow control they use the concept of success (a result is returned) or failure (a signal). For more on this see see [[Short-circuit_evaluation#Icon_and_Unicon|Short Circuit Evaluation]]. Because there is almost no need for Boolean values the concept is somewhat alien.

One likely situation where Boolean values could be encountered is working with an external array of bits/flags.  This example attempts to show a solution that would work in such a scenario.  Some characteristics would include:
* the ability to work with an entire array of bits
* the ability to test an individual bit for true/false
* need to be careful with automatic type conversions
Of course other characteristics and functionality might be desirable, examples include:
* shifting (based on ishift)
* rotation
* conversion to a (large) integer
* setting a specific bit in the array

Those are left as an exercise for the reader.

There are a couple of choices for implementation.  Briefly:
* use of &null and a non-null - this creates problems for negation as not &null can be any or all values
* use of large integers as bit arrays - only signed integers are supported and this complicates preserving array length
* use of strings - a bit wasteful of space

This implementation uses strings as packed arrays of bits.  This facilitates easy reading and writing from external sources.  While string length is variable it is controlled and doesn't change under negation.  The built-in integer bit operations (ior, ixor, iand, ishift) can be utilized under the covers.

```Icon
invocable all

procedure main()                      #: sample demonstrating boolean function use

limit := 4
char2 := char(2)||char(0)
every (i := char(1 to limit)|char2) do {
   write(iop := "bnot","( ",image(i)," ) = ",image(iop(i)))
   every k := 3 | 10 do {
     write("bistrue(",image(i),",",k,") - ", if bistrue(i,k) then "returns" else "fails")
     write("bisfalse(",image(i),",",k,") - ", if bisfalse(i,k) then "returns" else "fails")
     }
   every (j := char(1 to limit)) & (iop := "bor"|"band"|"bxor") do
      write(iop,"( ",image(i),", ",image(j)," ) = ",image(iop(i,j)))
   }
end


procedure bisfalse(b,p)                #: test if bit p (numbered right to left from 1) is false; return b or fails
return boolean_testbit(0,b,p)
end

procedure bistrue(b,p)                 #: test if bit p is true; return b or fails
return boolean_testbit(1,b,p)
end

procedure bnot(b)                      #: logical complement of b (not is a reserved word)
static cs,sc
initial sc := reverse(cs := string(&cset))
if type(b) ~== "string" then runerr(103,b)
return map(b,cs,sc)                    # en-mass inversion through remapping ordered cset
end

procedure bor(b1,b2)                   #: logical or
return boolean_op(ior,b1,b2)
end

procedure band(b1,b2)                  #: logical or
return boolean_op(iand,b1,b2)
end

procedure bxor(b1,b2)                  #: logical or
return boolean_op(ixor,b1,b2)
end

procedure boolean_testbit(v,b,p)       #: (internal) test if bit p is true/false; return b or fail
if not 0 <= integer(p) = p then runerr(101,p)
if type(b) ~== "string" then runerr(103,b)
if v = ishift(ord(b[-p/8-1]), -(p%8)+1) then return b
end

procedure boolean_op(iop,b1,b2)        #: boolean helper
local b3,i
static z
initial z := char(0)
if type(b1) ~== "string" then runerr(103,b1)
if type(b2) ~== "string" then runerr(103,b2)
b3 := ""
every i := -1 to -max(*b1,*b2) by -1 do
   b3 :=  char(iop(ord(b1[i]|z),ord(b2[i]|z))) || b3
return b3
end
```


```txt
...
bnot( "\x03" ) = "\xfc"
...
bor( "\x03", "\x01" ) = "\x03"
band( "\x03", "\x01" ) = "\x01"
bxor( "\x03", "\x01" ) = "\x02"
...
bnot( "\x02\x00" ) = "\xfd\xff"
bistrue("\x02\x00",3) - fails
bisfalse("\x02\x00",3) - returns
bistrue("\x02\x00",10) - returns
bisfalse("\x02\x00",10) - fails
bor( "\x02\x00", "\x01" ) = "\x02\x01"
band( "\x02\x00", "\x01" ) = "\x00\x00"
bxor( "\x02\x00", "\x01" ) = "\x02\x01"
...
```



## J


J uses 0 for logical false and 1 for logical true.

```j
   aon=: *.`+.`(-.@[)`:0
```

Given boolean arguments, <code>*.</code> is logical and, <code>+.</code> is logical or, and <code>-.</code>is logical not.

Additional primary logical operators include <code>*:</code> (not-and), <code>+:</code> (not-or), <code>~:</code> (exclusive-or) and <code><:</code> (logical implication).


```j

   a=: 0 0 1 1   NB. Work on vectors to show all possible
   b=: 0 1 0 1   NB. 2-bit combos at once.
   a aon b
0 0 0 1
0 1 1 1
1 1 0 0
```


An alternate approach, based on a probabilistic interpretation, uses <code>*</code> for logical and, <code>-.</code> for logical negation and derives the others: <code>(*&.-.)</code> for logical or, <code>(-.@*)</code> for not-and, <code>(-.@*&.-.)</code> for not-or, <code>(* *&.-. -.@*&.-.)</code> for exclusive or, and <code>(*&.-. -.)~</code> for logical implication. You get the same results for simple truth values this way, but you also get consistent treatment for values between 0 and 1.


## Java



```java
public static void logic(boolean a, boolean b){
  System.out.println("a AND b: " + (a && b));
  System.out.println("a OR b: " + (a || b));
  System.out.println("NOT a: " + (!a));
}
```


Additionally, ^ is used for XOR and == is used for "equal to" (a.k.a. bidirectional implication).


## JavaScript


```javascript
function logic(a,b) {
  print("a AND b: " + (a && b));
  print("a OR b: " + (a || b));
  print("NOT a: " + (!a));
}
```



## jq

In jq, <tt>and</tt> and <tt>or</tt> have short-circuit semantics, and can be used with non-boolean arguments.

In addition to the basic logical operators, jq has <tt>any</tt> and <tt>all</tt> filters. Versions of jq since 1.4 also have extended versions of these for working efficiently with streams.

```jq
def logic(a; b):
  "\(a) and \(b) => \(a and b)",
  "\(a) or \(b)  => \(a or  b)",
  "\(a) | not    => \(a | not)",
  "if \(a) then true else false end => \(if a then true else false end)" ;
```

'''Example''':

```jq
  (false, null, []) as $a
| (false, null, {}) as $b
| logic( $a; $b )
```

<div style="overflow:scroll; height:200px;">

```sh
$ jq -n -r -f logical_operations.jq
false and false => false
false or false  => false
false | not    => true
if false then true else false end => false
false and null => false
false or null  => false
false | not    => true
if false then true else false end => false
false and {} => false
false or {}  => true
false | not    => true
if false then true else false end => false
null and false => false
null or false  => false
null | not    => true
if null then true else false end => false
null and null => false
null or null  => false
null | not    => true
if null then true else false end => false
null and {} => false
null or {}  => true
null | not    => true
if null then true else false end => false
[] and false => false
[] or false  => true
[] | not    => false
if [] then true else false end => true
[] and null => false
[] or null  => true
[] | not    => false
if [] then true else false end => true
[] and {} => true
[] or {}  => true
[] | not    => false
if [] then true else false end => true
```
</div>


## Julia


```Julia

function exerciselogic(a::Bool, b::Bool)
    st = @sprintf " %5s" a
    st *= @sprintf " %5s" b
    st *= @sprintf " %5s" ~a
    st *= @sprintf " %5s" a | b
    st *= @sprintf " %5s" a & b
    st *= @sprintf " %5s" a $ b
end

println("Julia's logical operations on Bool:")
println("   a     b    not   or    and   xor")
for a in [true, false], b in [true, false]
    println(exerciselogic(a, b))
end

```


```txt

Julia's logical operations on Bool:
   a     b    not   or    and   xor
  true  true false  true  true false
  true false false  true false  true
 false  true  true  true false  true
 false false  true false false false

```


'''Notes'''

This solution shows the bitwise operators in action.  There are also short-circuiting or and and (<code>||</code>, <code>&&</code>).  In addition, there are updating versions of the three binary logical operators, <code>|=</code>, <code>&=</code> and <code>$=</code>.


## Kotlin

Similar style to FreeBASIC entry:

```scala
// version 1.0.6

fun logicalDemo(b1: Boolean, b2: Boolean) {
    println("b1             =  $b1")
    println("b2             =  $b2")
    println("b1 and b2      =  ${b1 and b2}")
    println("b1 or b2       =  ${b1 or b2}")
    println("b1 xor b2      =  ${b1 xor b2}")
    println("not b1         =  ${!b1}")
    println("b1 && b2       =  ${b1 && b2}")
    println("b1 || b2       =  ${b1 || b2}")
    println()
}

fun main(args: Array<String>) {
    logicalDemo(true, true)
    logicalDemo(true, false)
    logicalDemo(false, false)
    logicalDemo(false, true)
}
```


```txt

b1             =  true
b2             =  true
b1 and b2      =  true
b1 or b2       =  true
b1 xor b2      =  false
not b1         =  false
b1 && b2       =  true
b1 || b2       =  true

b1             =  true
b2             =  false
b1 and b2      =  false
b1 or b2       =  true
b1 xor b2      =  true
not b1         =  false
b1 && b2       =  false
b1 || b2       =  true

b1             =  false
b2             =  false
b1 and b2      =  false
b1 or b2       =  false
b1 xor b2      =  false
not b1         =  true
b1 && b2       =  false
b1 || b2       =  false

b1             =  false
b2             =  true
b1 and b2      =  false
b1 or b2       =  true
b1 xor b2      =  true
not b1         =  true
b1 && b2       =  false
b1 || b2       =  true

```



## Lasso


```Lasso
// br is just for formatting output here
define br => '\r'

// define vars
local(a = true, b = false)

// boolean comparators.
// note, not including comparison operators which would return boolean results
'a AND b: ' + (#a && #b)
br
'a OR b: ' + (#a || #b)
br
'NOT a: ' + !#a
br
'NOT a (using not): ' + not #a
```



## Liberty BASIC

There is no truly Boolean type.
0 = false, nonzero = true.
A true value is ANY value not zero, but is usually considered to be either "1" or "-1".

```lb

False =0
True  =not( False)

print " True ="; True, "False ="; False, "NB True here shown as -1"
print

print " a   b    AND  OR   XOR"
a =0: b =0: print " "; a; "   "; b; "     "; a and b; "    "; a or  b; "    "; a xor b
a =0: b =1: print " "; a; "   "; b; "     "; a and b; "    "; a or  b; "    "; a xor b
a =1: b =0: print " "; a; "   "; b; "     "; a and b; "    "; a or  b; "    "; a xor b
a =1: b =1: print " "; a; "   "; b; "     "; a and b; "    "; a or  b; "    "; a xor b

end

```

 True =-1     False =0      NB True here shown as -1
 .
 a   b    AND  OR   XOR
 0   0     0    0    0
 0   1     0    1    1
 1   0     0    1    1
 1   1     1    1    0


## LIL


```tcl
# Logical operations, in LIL
set first [expr 1 == 1]
set second [expr 1 == 0]

func and-or-not {a b} {
    print a $a b $b
    print "a AND b" [expr $a && $b]
    print "a OR b " [expr $a || $b]
    print "NOT a  " [expr !$a]
}

and-or-not $first $second
```


```txt
prompt$ lil logicalOperations.lil
a 1 b 0
a AND b 0
a OR b  1
NOT a   0
```



## LiveCode


```LiveCode
function boolOps p1, p2
    local boolOpsResult
    put p1 && "AND" && p2 && "=" && merge("[[p1 and p2]]") & cr after boolOpsResult
    put p1 && "OR" && p2 && "=" && merge("[[p1 or p2]]") & cr after boolOpsResult
    put "NOT" && p1 && "=" && merge("[[not p1]]")  & cr after boolOpsResult
    return boolOpsResult
end boolOps
```

Example

```LiveCode
repeat for each item bop in "true,false"
  put boolops(bop, bop) & cr after bopResult
  put boolops(bop, not bop) & cr after bopResult
end repeat
put bopResult

-- results
true AND true = true
true OR true = true
NOT true = false

true AND false = false
true OR false = true
NOT true = false

false AND false = false
false OR false = false
NOT false = true

false AND true = false
false OR true = true
NOT false = true
```



## LLVM


```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

;--- The declarations for the external C functions
declare i32 @printf(i8*, ...)

$"FORMAT_AND" = comdat any

$"FORMAT_OR" = comdat any

$"FORMAT_NOT" = comdat any

@"FORMAT_AND" = linkonce_odr unnamed_addr constant [15 x i8] c"a and b is %d\0A\00", comdat, align 1
@"FORMAT_OR" = linkonce_odr unnamed_addr constant [14 x i8] c"a or b is %d\0A\00", comdat, align 1
@"FORMAT_NOT" = linkonce_odr unnamed_addr constant [13 x i8] c"not a is %d\0A\00", comdat, align 1

; Function Attrs: noinline nounwind optnone uwtable
define void @print_logic(i32, i32) #0 {
  %3 = alloca i32, align 4          ;-- allocate b
  %4 = alloca i32, align 4          ;-- allocate a
  store i32 %1, i32* %3, align 4    ;-- copy parameter b
  store i32 %0, i32* %4, align 4    ;-- copy parameter a
  %5 = load i32, i32* %4, align 4   ;-- load a
  %6 = icmp ne i32 %5, 0            ;-- is a true?
  br i1 %6, label %and_true, label %and_false

and_true:
  %7 = load i32, i32* %3, align 4
  %8 = icmp ne i32 %7, 0
  br label %and_false

and_false:
  %9 = phi i1 [ false, %2 ], [ %8, %and_true ]
  %10 = zext i1 %9 to i32
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @"FORMAT_AND", i32 0, i32 0), i32 %10)
  %12 = load i32, i32* %4, align 4  ;-- load a
  %13 = icmp ne i32 %12, 0          ;-- is a true?
  br i1 %13, label %or_true, label %or_false

or_false:
  %14 = load i32, i32* %3, align 4  ;-- load b
  %15 = icmp ne i32 %14, 0          ;-- is b true?
  br label %or_true

or_true:
  %16 = phi i1 [ true, %and_false ], [ %15, %or_false ]
  %17 = zext i1 %16 to i32
  %18 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @"FORMAT_OR", i32 0, i32 0), i32 %17)

  %19 = load i32, i32* %4, align 4  ;-- load a
  %20 = icmp ne i32 %19, 0
  %21 = xor i1 %20, true
  %22 = zext i1 %21 to i32
  %23 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @"FORMAT_NOT", i32 0, i32 0), i32 %22)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4          ;-- allocate i
  %2 = alloca i32, align 4          ;-- allocate j
  store i32 0, i32* %1, align 4     ;-- store 0 in i
  br label %loop_i

loop_i:
  %3 = load i32, i32* %1, align 4   ;-- load i
  %4 = icmp slt i32 %3, 2           ;-- i < 2
  br i1 %4, label %loop_j_init, label %exit

loop_j_init:
  store i32 0, i32* %2, align 4     ;-- store 0 in j
  br label %loop_j

loop_j:
  %5 = load i32, i32* %2, align 4   ;-- load j
  %6 = icmp slt i32 %5, 2           ;-- j < 2
  br i1 %6, label %loop_body, label %loop_i_inc

loop_body:
  %7 = load i32, i32* %2, align 4   ;-- load j
  %8 = load i32, i32* %1, align 4   ;-- load i
  call void @print_logic(i32 %8, i32 %7)
  %9 = load i32, i32* %2, align 4   ;-- load j
  %10 = add nsw i32 %9, 1           ;-- increment j
  store i32 %10, i32* %2, align 4   ;-- store j
  br label %loop_j

loop_i_inc:
  %11 = load i32, i32* %1, align 4  ;-- load i
  %12 = add nsw i32 %11, 1          ;-- increment i
  store i32 %12, i32* %1, align 4   ;-- store i
  br label %loop_i

exit:
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
```

```txt
a and b is 0
a or b is 0
not a is 1
a and b is 0
a or b is 1
not a is 1
a and b is 0
a or b is 1
not a is 0
a and b is 1
a or b is 1
not a is 0
```



## Logo

The boolean literals are used as words ("true and "false) when used in a program.

```logo
to logic :a :b
  (print [a AND b =] and :a :b)
  (print [a OR b =] or :a :b)
  (print [NOT a =] not :a)
end
```


AND and OR may have arity greater than two if used in parentheses (and :a :b :c).


## Lua


```lua

function logic(a,b)
  return a and b, a or b, not a
end

```




## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Def Boolean A, B
      Document Rep$
      A=True
      B=False
      k=(A, B)
      And=Lambda (a as Boolean, b as Boolean)-> a and b
      Or=Lambda (a as Boolean, b as Boolean)-> a or b
      Xor=Lambda (a as Boolean, b as Boolean)-> a xor b
      Not=Lambda (a)->Not a
      func=((And, "And"), (Or, "Or"), (Xor, "Xor"))
      F1=Each(func)
      While F1 {
            M1=Each(k)
            M2=Each(k)
            While M1 {
                  While M2 {
                       A=Array(Array(F1), 0)
                       Rep$=Format$("{0} {1} {2} = {3}",Array(M1), Array$(Array(F1), 1),Array(M2), A(Array(M1), Array(M2)))+{
                                    }
                  }
            }
      }
      M1=Each(k)
      While M1 {
            Rep$=Format$("Not {0} = {1}",Array(M1),  Not Array(M1))+{
                        }
      }
      Report Rep$
      Clipboard Rep$
}
CheckIt

```

```txt

True And True = True
True And False = False
False And True = False
False And False = False
True Or True = True
True Or False = True
False Or True = True
False Or False = False
True Xor True = False
True Xor False = True
False Xor True = True
False Xor False = False
Not True = False
Not False = True

```



## M4


```m4
define(`logical',
   `and($1,$2)=eval($1&&$2)  or($1,$2)=eval($1||$2)  not($1)=eval(!$1)')
logical(1,0)
```


```txt

and(1,0)=0  or(1,0)=1  not(1)=0

```



## Maple

Infix and prefix operators are provided for each of <code>and</code>, <code>or</code>, <code>not</code> as well as <code>xor</code> and <code>implies</code>.

```Maple

f:=proc(a,b) a and b, a or b, not a; end proc:

f(true,true);
f(true,false);
f(false,true);
f(false,false);

```

```txt
                              true, true, false
                             false, true, false
                              false, true, true
                             false, false, true
```



## Mathematica


```Mathematica
And[a,b,...]
Or[a,b,...]
Not[a]
```

And can also be given using the infix operator &&, Or can also be used using the infix operator ||. Not[a] can also be written as !a.
Furthermore Mathematica supports:

```Mathematica
Xor[a, b,...]
Nand[a, b,...]
Nor[a, b,...]
Xnor[a, b,...]
```

Note that the functions are not restricted to 2 arguments; any number of arguments are allowed (except for the function Not).
All these functions can also be used with infix operators, the characters for that are: \[Xor], \[Nand], \[Nor], and \[Xnor]. Or by typing [escape] [name boolean operator] [escape].


## Maxima


```maxima
f(a, b) := [not a, a or b, a and b];

/* to use multiple arguments, use any of these */
a and b and c and d;
a or b or c or d;
"and"(a, b, c, d);
"or"(a, b, c, d);
apply("and", [a, b, c, d]);
apply("or", [a, b, c, d]);
```



## MAXScript


```maxscript
fn printLogic a b =
(
    format "a and b is %\n" (a and b)
    format "a or b is %\n" (a or b)
    format "not a is %\n" (not a)
)
```



## Metafont



```metafont
def tf(expr a) = if a: "true" else: "false" fi enddef;
def test(expr a, b) =
  for o = "and", "or":
    message tf(a) & " " & o & " " & tf(b);
    show a scantokens(o) b;
  endfor
  message "not " & tf(a);
  show not a enddef;
```



```metafont
test(true, true);
test(false, false);
test(true, false);
test(false, true);
end
```



## min

```min
(
  :b :a
  "xor is: " print! a b xor puts!
  "and is: " print! a b and puts!
  "or is: " print! a b or puts!
  "not is: " print! a not puts!
) :logical-operators
```


=={{header|Modula-2}}==

```modula2
MODULE LogicalOps;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Print(a,b : BOOLEAN);
VAR buf : ARRAY[0..31] OF CHAR;
BEGIN
    FormatString("a and b is %b\n", buf, a AND b);
    WriteString(buf);
    FormatString("a or b is %b\n", buf, a OR b);
    WriteString(buf);
    FormatString("not a is %b\n", buf, NOT a);
    WriteString(buf);
    WriteLn
END Print;

BEGIN
    Print(FALSE, FALSE);
    Print(FALSE, TRUE);
    Print(TRUE, TRUE);
    Print(TRUE, FALSE);

    ReadChar
END LogicalOps.
```


=={{header|Modula-3}}==

```modula3
MODULE Logical EXPORTS Main;

FROM IO IMPORT Put;
FROM Fmt IMPORT Bool;

PROCEDURE Test(a, b: BOOLEAN) =
  BEGIN
    Put("a AND b is " & Bool(a AND b) & "\n");
    Put("a OR b is " & Bool(a OR b) & "\n");
    Put("NOT a is " & Bool(NOT a) & "\n");
  END Test;

BEGIN
  Test(TRUE, FALSE);
END Logical.
```



## MUMPS


```MUMPS

LOGIC(A,B)
 WRITE !,A," AND ",B," IS ",A&B
 WRITE !,A," OR  ",B," IS ",A!B
 WRITE !,"NOT ",A," AND ",B," IS ",'(A)&B
 WRITE !,"NOT ",A," OR ",B," IS ",'(A)!B

```



## Neko


```ActionScript
/**
 Logical operations, in Neko
*/

/* For logical operations, values need to be explicitly treated as boolean */
/* Only null, false and 0 evaluate as false with $istrue() */

var logical = 1
if logical $print("literal 1 tests true\n") else $print("literal 1 tests false\n")
if $istrue(logical) $print("$istrue(1) tests true\n")

/* supports && logical AND, || logical OR, $not(value) the opposite of $istrue() */

if $istrue(logical) && logical > 0 $print("true path for logical AND\n")
if $istrue(logical) || logical > 1 $print("true path for logical OR\n")
if $not(logical) $print("true path for $not(1)\n") else $print("false path for $not(1)\n")
```


```txt
prompt$ nekoc logical-operations.neko
prompt$ neko logical-operations.n
literal 1 tests false
$istrue(1) tests true
true path for logical AND
true path for logical OR
false path for $not(1)
```



## Nemerle


```Nemerle
using System;
using System.Console;

module Logical
{
    WriteLogical(a : bool, b : bool) : void
    {
        WriteLine("{0} and {1} is {2}", a, b, a && b);
        WriteLine("{0} or {1} is {2}", a, b, a || b);
        WriteLine("not {0} is {1}", a, !a);
    }

    Main() : void {WriteLogical(true, false)}
}
```

Or, if you prefer keywords to operators import the Nemerle.English namespace to use '''and''', '''or''', and '''not'''.


## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method logicalOperation(xL = boolean, xR = boolean) public static
  say showBool(xL) 'AND' showBool(xR) '=' showBool(xL &  xR) -- AND
  say showBool(xL) 'OR ' showBool(xR) '=' showBool(xL |  xR) -- OR
  say showBool(xL) 'XOR' showBool(xR) '=' showBool(xL && xR) -- XOR
  say '     '      'NOT' showBool(xL) '=' showBool(\xL)      -- NOT
  say
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method showBool(bb = boolean) public static
  if bb then bt = 'true '
  else       bt = 'false'
  return bt

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  TRUE_  = (1 == 1)
  FALSE_ = \TRUE_
  lpairs = [ -
    [TRUE_,  TRUE_ ], -
    [TRUE_,  FALSE_], -
    [FALSE_, TRUE_ ], -
    [FALSE_, FALSE_]  -
  ]
  loop lx = 0 to lpairs.length - 1
    lpair = lpairs[lx]
    --say showBool(lpair[0]) showBool(lpair[1])
    logicalOperation(lpair[0], lpair[1])
    end lx
  return

```

```txt

true  AND true  = true
true  OR  true  = true
true  XOR true  = false
      NOT true  = false

true  AND false = false
true  OR  false = true
true  XOR false = true
      NOT true  = false

false AND true  = false
false OR  true  = true
false XOR true  = true
      NOT false = true

false AND false = false
false OR  false = false
false XOR false = false
      NOT false = true

```



## NewLISP


```newlisp

(define (logic a b)
		(print "a and b is: " (and a b) "\n a or b is: " (or a b))
		(print "\n not a is: " (not a)))


```



## Nim


```nim
proc logic(a, b) =
  echo "a and b: ", a and b
  echo "a or b: ", a or b
  echo "not a: ", not a
  echo "a xor b: ", a xor b
```



## Objeck


```objeck

bundle Default {
  class Logic {
    function : Main(args : String[]) ~ Nil {
      a := true;
      b := false;
      IO.Console->GetInstance()->Print("a and b is: ")->PrintLine(a & b);
      IO.Console->GetInstance()->Print("a or b is: ")->PrintLine(a | b);
      IO.Console->GetInstance()->Print("not a is: ")->PrintLine(a <> true);
    }
  }
}

```



## OCaml



```ocaml
let print_logic a b =
  Printf.printf "a and b is %B\n" (a && b);
  Printf.printf "a or b is %B\n" (a || b);
  Printf.printf "not a is %B\n" (not a)
```



## Octave



```octave
function test(a, b)
  s1 = num2str(a);
  s2 = num2str(b);
  disp(strcat(s1, " and ", s2, " = ", num2str(a&&b)));
  disp(strcat(s1, " or ", s2, " = ", num2str(a||b)));
  disp(strcat("not ", s1, " = ", num2str(!a)));
endfunction

% constant true is 1, false is 0
test(true, true);
test(false, false);
test(true, false);
test(false, true);
```



## Oforth



```Oforth
: logical(b1, b2)
   System.Out "and = " << b1 b2 and << cr
   System.Out "or  = " << b1 b2 or << cr
   System.Out "xor = " << b1 b2 xor << cr
   System.Out "not = " << b1 not << cr ;
```



## OOC

Bools in ooc are just covers for C's bools and respond to the same operators.

```ooc

logic: func (a: Bool, b: Bool) {
  println()
  "A=#{a}, B=#{b}:"  println()
  "AND:   #{a && b}" println()
  "OR:    #{a || b}" println()
  "NOT A: #{!a}"     println()
}

main: func {
  logic(true, false)
  logic(true, true)
  logic(false, false)
  logic(false, true)
}

```



## OpenEdge/Progress

The logical data type can have three values: true, false or unknown (represented by question mark).


```progress
FUNCTION testLogical RETURNS CHAR (
   i_l1 AS LOGICAL,
   i_l2 AS LOGICAL
):

   RETURN
      SUBSTITUTE( '&1 and &2:  &3', i_l1, i_l2, i_l1 AND i_l2 ) + '~n' +
      SUBSTITUTE( '&1 or &2:  &3', i_l1, i_l2, i_l1 OR i_l2 )  + '~n' +
      SUBSTITUTE( 'not &1:  &2', i_l1, NOT i_l1 )
      .

END FUNCTION.
```


```progress
MESSAGE
   testLogical( FALSE, FALSE ) SKIP(1)
   testLogical( FALSE, TRUE ) SKIP(1)
   testLogical( TRUE, FALSE ) SKIP(1)
   testLogical( TRUE, TRUE ) SKIP(2)

   testLogical( ?, ? ) SKIP(1)
   testLogical( ?, FALSE ) SKIP(1)
   testLogical( ?, TRUE ) SKIP(1)
VIEW-AS ALERT-BOX.
```


```txt
---------------------------
Message (Press HELP to view stack trace)
---------------------------
no and no:  no
no or no:  no
not no:  yes

no and yes:  no
no or yes:  yes
not no:  yes

yes and no:  no
yes or no:  yes
not yes:  no

yes and yes:  yes
yes or yes:  yes
not yes:  no


? and ?:  ?
? or ?:  ?
not ?:  ?

? and no:  no
? or no:  ?
not ?:  ?

? and yes:  ?
? or yes:  yes
not ?:  ?

---------------------------
OK   Help
---------------------------
```



## Oz


```oz
proc {PrintLogic A B}
   %% using not short-circuiting standard library functions
   {Show {And A B}}
   {Show {Or A B}}
   {Show {Not A}}

   %% using short-circuiting keywords
   {Show A andthen B}
   {Show A orelse B}
end
```



## PARI/GP

Note that the forms <code>bitand()</code>, <code>bitor()</code>, <code>bitneg()</code>, and <code>bitxor()</code> also exist. These apply the operator to each bit and do not short-circuit, unlike the below.

```parigp
logic(a,b)={
  print(a&b); \\ && is the same
  print(a|b); \\ || is the same
  print(!a);
};
```



## Pascal


```pascal
procedure printlogic(a, b: boolean);
 begin
  writeln('a and b is ', a and b);
  writeln('a or b is ', a or b);
  writeln('not a is', not a);
 end;
```



## Perl



```perl
sub show_bool
{
        return shift() ? 'true' : 'false', "\n";
}

sub test_logic
{
        my ($a, $b) = @_;
        print "a and b is ", show_bool $a && $b;
        print "a or b is ", show_bool $a || $b;
        print "not a is ", show_bool !$a;
        print "a xor b is ", show_bool($a xor $b);
}
```


There are also <code>and</code>, <code>or</code>, and <code>not</code> operators. These are just like <code>&&</code>, <code>||</code>, and <code>!</code> (respectively) except for their precedences, which are much lower.


## Perl 6


Perl 6 has an abundance of logical operators for various purposes.

```perl6
sub logic($a,$b) {
    say "$a && $b is ", $a && $b;     # short-circuiting
    say "$a || $b is ", $a || $b;     # short-circuiting
    say "$a ^^ $b is ", $a ^^ $b;
    say "!$a is ",     !$a;

    say "$a ?& $b is ", $a ?& $b;     # non-short-circuiting
    say "$a ?| $b is ", $a ?| $b;     # non-short-circuiting
    say "$a ?^ $b is ", $a ?^ $b;     # non-short-circuiting

    say "$a +& $b is ", $a +& $b;     # numeric bitwise
    say "$a +| $b is ", $a +| $b;     # numeric bitwise
    say "$a +^ $b is ", $a +^ $b;     # numeric bitwise

    say "$a ~& $b is ", $a ~& $b;     # buffer bitwise
    say "$a ~| $b is ", $a ~| $b;     # buffer bitwise
    say "$a ~^ $b is ", $a ~| $b;     # buffer bitwise

    say "$a & $b is ", $a & $b;       # junctional/autothreading
    say "$a | $b is ", $a | $b;       # junctional/autothreading
    say "$a ^ $b is ", $a ^ $b;       # junctional/autothreading

    say "$a and $b is ", ($a and $b); # loose short-circuiting
    say "$a or $b is ",  ($a or $b);  # loose short-circuiting
    say "$a xor $b is ", ($a xor $b);
    say "not $a is ",    (not $a);
}

logic(3,10);
```

```txt
3 && 10 is 10
3 || 10 is 3
3 ^^ 10 is Nil
!3 is False
3 ?& 10 is True
3 ?| 10 is True
3 ?^ 10 is False
3 +& 10 is 2
3 +| 10 is 11
3 +^ 10 is 9
3 ~& 10 is 1
3 ~| 10 is 30
3 ~^ 10 is 30
3 & 10 is all(3, 10)
3 | 10 is any(3, 10)
3 ^ 10 is one(3, 10)
3 and 10 is 10
3 or 10 is 3
3 xor 10 is Nil
not 3 is False
```



## Phix

There is no builtin boolean type, but you can either use integers or create one easily enough.

The operators always return 1(true) or 0(false), and treat operands of 0 as false and all other (atom) values as true.

Short-circuiting is always applied (to all "and"/"or" expressions)

Other relational operators and maths are also valid, if you wanna get clever.

```Phix
--constant TRUE = (1=1),        -- 1 internally     \ now pre-
--         FALSE = not TRUE     -- 0 internally     / defined
type boolean(object b)
    return integer(b) and find(b,{TRUE,FALSE})!=0
end type

function logicop(boolean a, boolean b)
    return {a, b, a and b, a or b, not a, a xor b, a=b, a!=b}
end function

function TF(sequence tf)
boolean tfi
    for i=1 to length(tf) do
        tfi = tf[i]
        tf[i] = iff(tfi?'T','F')
    end for
    return tf
end function

printf(1," a  b and or not xor = !=\n")
for a=FALSE to TRUE do  -- nb: TRUE to FALSE would need a "by -1".
    for b=FALSE to TRUE do
        printf(1,"%2c %2c  %c  %c   %c   %c  %c %c\n",TF(logicop(a,b)))
    end for
end for
```

```txt

 a  b and or not xor = !=
 F  F  F  F   T   F  T F
 F  T  F  T   T   T  F T
 T  F  F  T   F   T  F T
 T  T  T  T   F   F  T F

```

Simpler version using plain integer flags:

```Phix
function logiicop(integer a, integer b)
    return {a, b, a and b, a or b, not a, a xor b, a=b, a!=b}
end function

printf(1," a  b and or not xor = !=\n")
for a=0 to 1 do
    for b=0 to 1 do
        printf(1,"%2d %2d  %d  %d   %d   %d  %d %d\n",logiicop(a,b))
    end for
end for
```

```txt

 a  b and or not xor = !=
 0  0  0  0   1   0  1 0
 0  1  0  1   1   1  0 1
 1  0  0  1   0   1  0 1
 1  1  1  1   0   0  1 0

```



## PHP



```php
function print_logic($a, $b)
{
    echo "a and b is ", $a && $b ? 'True' : 'False', "\n";
    echo "a or b is ", $a || $b ? 'True' : 'False', "\n";
    echo "not a is ", ! $a ? 'True' : 'False', "\n";
}
```



## PicoLisp


```PicoLisp
(de logic (A B)
   (prin "A AND B is ")
   (println (and A B))
   (prin "A OR B is ")
   (println (or A B))
   (prin "A XOR B is ")
   (println (xor A B))
   (prin "NOT A is ")
   (println (not A)) )
```



## PL/I


```pli
logical_ops: procedure (t, u);
   declare (t, u) bit (1);

   put skip list (t & u);
   put skip list (t | u); /* logical or   */
   put skip list (^t);    /* logical not  */
   put skip list (t ^ u); /* exclusive or */
end logical_ops;
```



## Pop11



```pop11
define print_logic(a, b);
    printf(a and b, 'a and b is %p\n');
    printf(a or b, 'a or b is %p\n');
    printf(not(a), 'not a is %p\n');
enddefine;
```


Example usage is:

```pop11
print_logic(true, false);
```



## PostScript


```postscript

/logical{
/a exch def
/b exch def
a b and =
a b or =
a not =
}def

```



## PowerShell


```powershell
function Test-Boolean ([bool] $a, [bool] $b) {
    Write-Host "A and B:   " ($a -and $b)
    Write-Host "A or B:    " ($a -or $b)
    Write-Host "not A:     " (-not $a)
    Write-Host "not A:     " (!$a)
    Write-Host "A xor B:   " ($a -xor $b)
}
```



## Prolog

In Prolog, '''','''' is used for '''and''', '''';'''' for '''or''' and '''\+''' for '''not'''.

```txt
 ?- true,true.
true.

 ?- true,false.
false.

 ?- true;false.
true .

 ?- false;true.
true .

 ?- false;false.
false .

 ?- \+true.
false.

 ?- \+false.
true.

 ?- \+((true,false)).
true.


 ?- \+((true;false)).
false.



```



## PureBasic



```PureBasic
Procedure LogicDebug(a,b)
  Debug a And b
  Debug a Or b
  Debug Not a
  Debug a XOr b
EndProcedure
```



## Python


```python
def logic(a, b):
    print('a and b:', a and b)
    print('a or b:', a or b)
    print('not a:', not a)
```


Note: Any normal object can be treated as a Boolean in Python.  Numeric objects which evaluate to any non-zero value are "True" otherwise they are false.  Non-empty strings, lists, tuples and other sequences are "True" otherwise they are false.  The pre-defined ''None'' object is also treated as "False."  In Python 2.3 pre-defined objects named ''True'' and ''False'' were added to the language; prior to that it was a common convention to include a line: ''False, True = 0, 1'' to use these as names.  Custom classes which implement ''__nonzero__'' or ''__len__'' or some other special methods can be implicitly evaluated as Booleans based on those results.


## R


```R
logic <- function(a, b) {
  print(a && b)
  print(a || b)
  print(! a)
}

logic(TRUE, TRUE)
logic(TRUE, FALSE)
logic(FALSE, FALSE)
```



## Racket


```Racket
#lang racket

(define (logic a b)
  (displayln (format "a and b equals ~a" (and a b)))
  (displayln (format "a or b equals ~a" (or a b)))
  (displayln (format "not a equals ~a" (not a)))
  (displayln (format "a nand b equals ~a" (nand a b)))
  (displayln (format "a nor b equals ~a" (nor a b)))
  (displayln (format "a implies b equals ~a" (implies a b)))
  (displayln (format "a xor b equals ~a" (xor a b))))
```



## Rascal


```rascal
import IO;

public void logic(bool a, bool b){
	println("a and b, is <a && b>");
	println("a or b, is <a || b>");
	println("a equivalent to b, is <a <==> b>");
	println("a implies b, is <a ==> b>");
	println("not a", <!a>");
}
```


```txt
rascal>logic(false, false);

a and b, is false
a or b, is false
a equivalent to b, is true
a implies b, is true
not a, true
ok
```



## REBOL


```rebol
logics: func [a [logic!] b [logic!]] [
    print ['and tab a and b]
    print ['or  tab a or  b]
    print ['not tab   not a]
    print ['xor tab a xor b]

    print ['and~ tab and~ a b]
    print ['or~  tab or~  a b]
    print ['xor~ tab xor~ a b]

    print ['any tab any [a b]]
    print ['all tab all [a b]]
]
```


Example:

```txt

>> logics true false
and      false
or       true
not      false
xor      true
and~     false
or~      true
xor~     true
any      true
all      none

```



## Retro


```Retro
: .bool ( f- ) [ "true" ] [ "false" ] if puts cr ;
: logic ( ab- )
 "\na = "  puts over .bool "b = " puts dup .bool
 "\na and b = " puts 2dup and .bool
 "\na  or b = " puts over  or .bool
 "\nnot a = " puts not .bool ;
```



## REXX

The REXX language's boolean values are well formed:

:::*   '''1'''   <tt> (true)</tt>
:::*   '''0'''   <tt>(false)</tt>


Any other value will raise a REXX '''syntax''' error condition.

### basic boolean functions


```rexx
/*REXX program  demonstrates some  binary  (also known as  bit  or logical)  operations.*/
                        x= 1    ;     y= 0       /*set the initial values of  X  and Y. */
                       @x= ' x ';    @y= ' y '   /*define a couple of literals for HDRs.*/
                                                 /* [↓]  echo  the   X  and  Y   values.*/
call $ 'name', "value"                           /*display the  header  (title) line.   */
call $ 'x'   ,    x                              /*display "x"  and then the value of X.*/
call $ 'y'   ,    y                              /*   "    "y"   "    "   "    "    " Y */
                                                 /* [↓]  negate the X; then the Y value.*/
call $ 'name', "negated"                         /*some REXXes support the  ¬  character*/
call $ 'x'   ,   \x                              /*display "x"  and then the value of ¬X*/
call $ 'y'   ,   \y                              /*   "    "y"   "    "   "    "    " ¬Y*/
say
say
call $ @x, @y, 'AND';    do x=0  to 1;   do y=0  to 1;   call $ x, y, x  & y;    end;  end
call $ @x, @y, 'OR' ;    do x=0  to 1;   do y=0  to 1;   call $ x, y, x  | y;    end;  end
call $ @x, @y, 'XOR';    do x=0  to 1;   do y=0  to 1;   call $ x, y, x && y;    end;  end
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$: parse arg @.1, @.2, @.3, @.4;         hdr= length(@.1) \== 1;        if hdr  then say
             do j=0  to hdr;             _=
                   do k=1  for arg();    _=_  center(@.k, 7)
                   end   /*k*/
             say _
             @.=copies('═', 7)                   /*define a new header separator line.  */
             end         /*j*/
   return
```

```txt

  name    value
 ═══════ ═══════
    x       1
    y       0

  name   negated
 ═══════ ═══════
    x       0
    y       1



    x       y      AND
 ═══════ ═══════ ═══════
    0       0       0
    0       1       0
    1       0       0
    1       1       1

    x       y      OR
 ═══════ ═══════ ═══════
    0       0       0
    0       1       1
    1       0       1
    1       1       1

    x       y      XOR
 ═══════ ═══════ ═══════
    0       0       0
    0       1       1
    1       0       1
    1       1       0

```



### extended boolean functions

All sixteen boolean functions could easily be shown.

```rexx
/*REXX pgm demonstrates some binary (also known as bit or logical)  extended operations.*/
                        x= 1    ;     y= 0       /*set the initial values of  X  and Y. */
                       @x= ' x ';    @y= ' y '   /*define a couple of literals for HDRs.*/
                                                 /* [↓]  echo  the   X  and  Y   values.*/
call $ 'name', "value"                           /*display the  header  (title) line.   */
call $ 'x'   ,    x                              /*display "x"  and then the value of X.*/
call $ 'y'   ,    y                              /*   "    "y"   "    "   "    "    " Y */
                                                 /* [↓]  negate the X; then the Y value.*/
call $ 'name', "negated"                         /*some REXXes support the  ¬  character*/
call $ 'x'   ,   \x                              /*display "x"  and then the value of ¬X*/
call $ 'y'   ,   \y                              /*   "    "y"   "    "   "    "    " ¬Y*/
say                                              /*note:  NXOR  is also known as  XNOR. */
say                                              /*all 16 bit operations could be shown.*/
call $ @x, @y, 'AND' ;   do x=0  to 1;   do y=0  to 1;   call $ x, y,   x  & y ;  end; end
call $ @x, @y, 'NAND';   do x=0  to 1;   do y=0  to 1;   call $ x, y, \(x  & y);  end; end
call $ @x, @y, 'OR'  ;   do x=0  to 1;   do y=0  to 1;   call $ x, y,   x  | y ;  end; end
call $ @x, @y, 'NOR' ;   do x=0  to 1;   do y=0  to 1;   call $ x, y, \(x  | y);  end; end
call $ @x, @y, 'XOR' ;   do x=0  to 1;   do y=0  to 1;   call $ x, y,   x && y ;  end; end
call $ @x, @y, 'NXOR';   do x=0  to 1;   do y=0  to 1;   call $ x, y, \(x && y);  end; end
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$: parse arg @.1, @.2, @.3, @.4;            hdr= length(@.1) \== 1;     if hdr  then say
              do j=0  to hdr;               _=
                    do k=1  for arg();      _=_  center(@.k, 7)
                    end   /*k*/
              say _
              @.= copies('═', 7)                 /*define a new separator (header) line.*/
              end         /*j*/
   return
```

```txt

  name    value
 ═══════ ═══════
    x       1
    y       0

  name   negated
 ═══════ ═══════
    x       0
    y       1



    x       y      AND
 ═══════ ═══════ ═══════
    0       0       0
    0       1       0
    1       0       0
    1       1       1

    x       y     NAND
 ═══════ ═══════ ═══════
    0       0       1
    0       1       1
    1       0       1
    1       1       0

    x       y      OR
 ═══════ ═══════ ═══════
    0       0       0
    0       1       1
    1       0       1
    1       1       1

    x       y      NOR
 ═══════ ═══════ ═══════
    0       0       1
    0       1       0
    1       0       0
    1       1       0

    x       y      XOR
 ═══════ ═══════ ═══════
    0       0       0
    0       1       1
    1       0       1
    1       1       0

    x       y     NXOR
 ═══════ ═══════ ═══════
    0       0       1
    0       1       0
    1       0       0
    1       1       1

```



## Ring


```ring

x = true
y = false

see "x and y = " + (x and y) + nl
see "x or y = " + (x or y) + nl
see  "not x = " + (not x) + nl

```



## RLaB

RLaB allows for standard logic operations.
<code>and/or/not</code> are synonymous with <code>&&/||/!</code>. In the case when the argument is a real number (default type of argument) the default statement in the absence of ''if'' command is ''is the argument non-zero''.
Therefore

```RLaB

>> x = 5
5
>> y = 0
0
>> !x
0
>> !y
1
>> x && y
0

```


However, if arguments to the functions are of the type ''integer'' then the functions operate bit-wise.

```RLaB

>> x = int(5)
5
>> y = int(0)
0
>> !x
-6
>> !y
-1
>> x && y
0

```



## Robotic

Due to the lack of booleans, there is no way to perform logical operations in Robotic.
However, [[Bitwise_operations|bitwise operators]] can be used.


## Ruby


```ruby
def logic(a, b)
  print 'a and b: ', a && b, "\n"
  print 'a or b: ' , a || b, "\n"
  print 'not a: '  , !a    , "\n"
  print 'a xor b: ' , a ^ b, "\n"
end
```

<code>and/or/not</code> are synonymous with <code>&&/||/!</code> albeit with lower precedence.


## Rust

```Rust

fn boolean_ops(a: bool, b: bool) {
    println!("{} and {} -> {}", a, b, a && b);
    println!("{} or {} -> {}", a, b, a || b);
    println!("{} xor {} -> {}", a, b, a ^ b);
    println!("not {} -> {}\n", a, !a);
}

fn main() {
    boolean_ops(true, true);
    boolean_ops(true, false);
    boolean_ops(false, true);
    boolean_ops(false, false)
}

```

The Boolean operators || and && are more efficient versions of | and & in that the right-hand operand is only evaluated when the left-hand operand does not already determine the result of the expression.


## Scala

In vanilla Scala:

```scala
def logical(a: Boolean, b: Boolean): Unit = {
  println("and: " + (a && b))
  println("or:  " + (a || b))
  println("not: " + !a)
}

logical(true, false)
```


With Scalaz:

```scala
def logical(a: Boolean, b: Boolean): IO[Unit] = for {
  _ <- putStrLn("and: " ++ (a && b).shows)
  _ <- putStrLn("or:  " ++ (a || b).shows)
  _ <- putStrLn("not: " ++ (!a).shows)
} yield ()

logical(true, false).unsafePerformIO
```



## Scheme


```scheme
(define (logic a b)
  (display "a and b is ")
  (display (and a b))
  (newline)
  (display "a or b is ")
  (display (or a b))
  (newline)
  (display "not a is ")
  (display (not a))
  (newline))
```



## Seed7


```seed7
const proc: writeLogic (in boolean: a, in boolean: b) is func
  begin
    writeln("a and b is " <& a and b);
    writeln("a or b is " <& a or b);
    writeln("not a is " <& not a);
  end func;
```



## Self



```self
true not = false.
( true && false ) = false.
( true ^^ false ) = true. "xor"
( true || false ) = true. "or"

```



## Sidef


```ruby
func logic(a, b) {
    say ("a and b: ", a && b);
    say ("a  or b: ", a || b);
    say ("a xor b: ", a ^ b);
    say ("  not a: ", !a);
}

logic(false, true);
```

```txt
a and b: false
a  or b: true
a xor b: true
  not a: true
```



## Slate

```slate
{#/\. #\/. #not} do: [ |:func|
  func arity = 1 ifTrue: [inform: 'True ' ; (func as: String) ; ' = ' ; (func sendTo: {True}) printString.
                          inform: 'False ' ; (func as: String) ; ' = ' ; (func sendTo: {False}) printString.].

  func arity = 2
    ifTrue: [{{True. True}. {True. False}. {False. True}. {False. False}} do:
              [ |:each| inform: each first printString ; (func as: String) ; each second printString ; ' = ' ; (func sendTo: each) printString]]

].
```


```txt
True/\True = True
True/\False = False
False/\True = False
False/\False = False
True\/True = True
True\/False = True
False\/True = True
False\/False = False
True not = False
False not = True
```




## SkookumScript


SkookumScript has a <code>Boolean</code> class with two possible values: <code>true</code> or <code>false</code>. Conditionals such as <code>if</code> expect a <code>Boolean</code> type and no other types can be implicitly coerced to a <code>Boolean</code> though they can be explicitly converted. Likewise <code>Boolean</code> cannot be implicitly coerced to an <code>Integer</code> value.

This makes a closure that takes two Boolean values. Booleans can be indicated by predicate identifier names that end with a question mark <code>?</code>.


```javascript
!logic:
  (a? b?)
    [
    println("a and b: " a and b)
    println("a or b: "  a or b)
    println("not a: "   not a)
    println("a xor b: " a xor b)
    println("a nand b: " a nand b)
    println("a nor b: " a nor b)
    println("a not xor b: " a nxor b)
    ]

```


Example call:


```javascript
logic(true false)
```




## Smalltalk

```smalltalk
|test|
test := [ :a :b |
  ('%1 %2 %3 = %4' % { a. 'and'. b. (a & b) }) displayNl.
  ('%1 %2 %3 = %4' % { a. 'or'. b. (a | b) }) displayNl.
  ('%1 %2 = %3' % {'not'. a. (a not) }) displayNl
].

test value: true value: true.
test value: false value: false.
test value: true value: false.
test value: false value: true.
```


```smalltalk
a implies: b
a xor: b
```



## Standard ML



```sml
fun print_logic (a, b) = (
  print ("a and b is " ^ Bool.toString (a andalso b) ^ "\n");
  print ("a or b is " ^ Bool.toString (a orelse b) ^ "\n");
  print ("not a is " ^ Bool.toString (not a) ^ "\n")
)
```



## Stata


Stata does not have a boolean type, and uses instead 0 and 1 to denote resp. false and true.


```stata
prog def bool
	args a b
	di `a'&`b'
	di `a'|`b'
	di !`a'
end
```


Likewise in Mata:


```stata
function bool(a,b) {
	printf("%f\n",a&b)
	printf("%f\n",a|b)
	printf("%f\n",!a)
}
```



## Swift



```swift
func logic(a: Bool, b: Bool) {
  println("a AND b: \(a && b)");
  println("a OR b: \(a || b)");
  println("NOT a: \(!a)");
}
```


Additionally, ^ is used for XOR and == is used for "equal to" (a.k.a. bidirectional implication).


## Tcl


```tcl
proc logic {a b} {
    puts "a and b: [expr {$a && $b}]"
    puts "a or b:  [expr {$a || $b}]"
    puts "not a:   [expr {!$a}]"
}
```



## Toka

This is an adaption of the code from the Forth example. Toka provides TRUE/FALSE flags
that are the same as the well-formed flags in Forth.


```toka>[ 0 <
 [ ." true" ] [ ." false"] ifTrueFalse ] is .bool
[ ( a b -- )
  cr ." a = " over .bool ."   b = " dup .bool
  cr ." a and b = " 2dup and .bool
  cr ." a  or b = " over  or .bool
  cr ." not a = " 0 = .bool
] is logic
```



## uBasic/4tH

uBasic/4tH does not have logical operators, but every non-zero value will be considered ''TRUE'' in conditional statements. However, comparison operators (like =, #, < and >) can be used in expressions and will return fully qualified booleans. Hence, simple arithmetic operators will do the trick just fine.
<lang>Proc _Boolean(4, 2)
Proc _Boolean(0, 2)
Proc _Boolean(2, 0)

End


_Boolean Param(2)
  a@ = a@ # 0                          ' Transform to true booleans
  b@ = b@ # 0

  print "A and B is "; a@ * b@         ' Multiplication will now do AND
  print "A or B is "; a@ + b@          ' Addition will now do OR
  print "not A is "; a@ = 0            ' This will invert the boolean value
  print
Return
```

```txt
A and B is 1
A or B is 2
not A is 0

A and B is 0
A or B is 1
not A is 1

A and B is 0
A or B is 1
not A is 0


0 OK, 0:63
```


## V

Using stack shuffles.


```v
[mylogic
  [get2 [dup] dip swap [dup] dip].
   get2 and puts
   get2 or puts
   swap not puts
   pop
 ].
```


Using view.

```v
[mylogic
   [get2 [a b : a b a b] view].
   get2 and puts
   get2 or puts
   swap not puts
   pop
 ].
```


Using internal defines


```v
[mylogic [a b] let
  a b and puts
  a b or puts
  a not puts
].
```



## Vala



```vala
public class Program {
    private static void print_logic (bool a, bool b) {
        print ("a and b is %s\n", (a && b).to_string ());
        print ("a or b is %s\n", (a || b).to_string ());
        print ("not a %s\n", (!a).to_string ());
    }
    public static int main (string[] args) {
        if (args.length < 3) error ("Provide 2 arguments!");
        bool a = bool.parse (args[1]);
        bool b = bool.parse (args[2]);
        print_logic (a, b);
        return 0;
    }
}
```



## Visual Basic .NET



```vbnet
Function Test(ByVal a As Boolean, ByVal b As Boolean)
    Console.WriteLine("And " & a And b)
    Console.WriteLine("Or " & a Or b)
    Console.WriteLine("Not " & Not a)
    Console.WriteLine("Xor " & a Xor b)
    Console.WriteLine("And, short-circuited " & a AndAlso b)
    Console.WriteLine("Or, short-circuited " & a OrElse b)
End Function
```



## XLISP


```lisp
(defun logical-functions (a b)
    (print `(a and b = ,(and a b)))
    (print `(a or b = ,(or a b)))
    (print `(not a = ,(not a))) )
```



## XPL0

Logical operations and bitwise operations are the same. The command word
'false' = 0 and 'true' = -1. These values are produced by comparison
operations, such as A>=B. Any integer not equal to zero is considered
true. Real numbers cannot be used as booleans. Symbols can be used
instead: & = and, ! = or, ~ = not, | = xor. Note that not 1, which is
true, is $FFFFFFFE, which is also true. Despite this, it's simple and
convenient to combine logical and bitwise operations.


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

func Logic(A, B);
int  A, B;
[HexOut(0, A and B);  ChOut(0, ^ );
 HexOut(0, A or  B);  ChOut(0, ^ );
 HexOut(0, not A);    ChOut(0, ^ );
 HexOut(0, A xor B);
]; \Logic

[Logic(false, false); CrLf(0);
 Logic(true, false);  CrLf(0);
 Logic(true, true);   CrLf(0);
 Logic(1, 1);   CrLf(0);
 Logic(1, 2);   CrLf(0);
]
```


```txt

00000000 00000000 FFFFFFFF 00000000
00000000 FFFFFFFF 00000000 FFFFFFFF
FFFFFFFF FFFFFFFF 00000000 00000000
00000001 00000001 FFFFFFFE 00000000
00000000 00000003 FFFFFFFE 00000003

```



## XSLT


```xml
<xsl:template name="logic">
  <xsl:param name="a" select="true()"/>
  <xsl:param name="b" select="false()"/>
  <fo:block>a and b = <xsl:value-of select="$a and $b"/></fo:block>
  <fo:block>a or b = <xsl:value-of select="$a or $b"/></fo:block>
  <fo:block>not a = <xsl:value-of select="not($a)"/></fo:block>
 </xsl:template>
```



## zkl

Logical and/or do the logic and returns one of the ops.

```zkl
fcn f(a,b){a and b}
fcn g(a,b){a or b}
fcn h(a){(not a)}
```


```txt

f(0,1) //-->0
f(1,2) //-->2
f(True,True)  //-->True
f(True,False) //-->False

g(0,1) //-->1
g(1,2) //-->1
g(True,True)  //-->True
g(True,False) //-->True

h(0) //-->True
h(1) //-->False
h(True)  //-->False
h(False) //-->True

```

