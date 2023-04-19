+++
title = "Function definition"
description = ""
date = 2019-10-11T15:49:36Z
aliases = []
[extra]
id = 2139
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}[[Category:Functions and subroutines]] [[Category:Simple]]
A function is a body of code that returns a value.

The value returned may depend on arguments provided to the function.


;Task:
Write a definition of a function called "multiply" that takes two arguments and returns their product.

(Argument types should be chosen so as not to distract from showing how functions are created and values returned).


;Related task:
*   [[Function prototype]]





## 360 Assembly

Linkage conventions are: register 1 : the parameter list, register 0 : the return value,
and register 14 : the return address.

```360asm
DEFFUN   CSECT
         USING  DEFFUN,R13
SAVEAREA B      PROLOG-SAVEAREA(R15)
         DC     17F'0'
PROLOG   STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15            set base register
BEGIN    L      R2,=F'13'
         ST     R2,X               X=13
         L      R2,=F'17'
         ST     R2,Y               Y=17
         LA     R1,PARMLIST        R1->PARMLIST
         B      SKIPPARM
PARMLIST DS     0F
         DC     A(X)
         DC     A(Y)
SKIPPARM BAL    R14,MULTPLIC       call MULTPLIC
         ST     R0,Z               Z=MULTPLIC(X,Y)
RETURN   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)
         XR     R15,R15            set return code
         BR     R14                return to caller
*
MULTPLIC EQU    *                  function MULTPLIC(X,Y)
         L      R2,0(R1)           R2=(A(X),A(Y))
         XR     R4,R4              R4=0
         L      R5,0(R2)           R5=X
         L      R6,4(R2)           R6=Y
         MR     R4,R6              R4R5=R4R5*R6
         LR     R0,R5              R0=X*Y   (R0 return value)
         BR     R14                end function MULTPLIC
*
X        DS     F
Y        DS     F
Z        DS     F
         YREGS
         END    DEFFUN
```



## 6502 Assembly

As with other low-level languages, 6502 assembler has subroutines rather than functions in the strict sense. This implementation of <tt>MULTIPLY</tt> behaves rather like a function, however: it expects two 'parameters' to be passed in the index registers <tt>X</tt> and <tt>Y</tt> and it returns the answer in the accumulator. Note that the 6502 has no <tt>MUL</tt> instruction, so multiplication is carried out by repeated addition.

```asm6502
MULTIPLY: STX   MULN      ; 6502 has no "acc += xreg" instruction,
          TXA             ; so use a memory address
MULLOOP:  DEY
          CLC             ; remember to clear the carry flag before
          ADC   MULN      ; doing addition or subtraction
          CPY   #$01
          BNE   MULLOOP
          RTS
```

An alternative implementation that multiplies A by X and checks if A/X is zero.

```asm6502
; https://skilldrick.github.io/easy6502/
; Multiplies A by X

define    memory  1040

          JMP MAIN

MULTIPLY: STA memory   ; memory = A
          BEQ MUL_END  ; A = 0
          TXA          ; A = X
          BEQ MUL_END  ; X = 0 -> A = 0
          LDA memory
          CLC
MUL_LOOP: DEX          ; X -= 1
          BEQ MUL_END  ; X = 0 -> A = A * X
          ADC memory   ; A += memory
          JMP MUL_LOOP
MUL_END:  RTS

MAIN:     LDA #50
          LDX #5
          JSR MULTIPLY
```



## 8051 Assembly

Like other assembly languages, 8051 doesn't have functions but instead has symbolic references to code. Function arguments are passed via registers decided on beforehand.

```asm
ORG RESET
mov a, #100
mov b, #10
call multiply
; at this point, the result of 100*10 = 1000 = 03e8h is stored in registers a and b
; a = e8
; b = 03
jmp $

multiply:
mul ab
ret
```



## ACL2


```Lisp
(defun multiply (a b) (* a b))
```



## ActionScript


```actionscript
function multiply(a:Number, b:Number):Number {
    return a * b;
}
```



## Ada


```ada
function Multiply (A, B : Float) return Float;
```

and an implementation of:

```ada
function Multiply (A, B : Float) return Float is
begin
   return A * B;
end Multiply;
```



The Ada 2012 standard provides an even simpler way to define and implement functions:


```Ada
function Multiply(A, B: Float) return Float is (A * B);
```



Ada supports generic functions which can take generic formal parameters like the numeric type to use:

```ada
generic
   type Number is digits <>;
function Multiply (A, B : Number) return Number;
```

implemented as:

```ada
function Multiply (A, B : Number) return Number is
begin
   return A * B;
end Multiply;
```

To use this, you need to instantiate the function for each type e.g.

```ada

with Multiply;
...
function Multiply_Integer is new Multiply(Number => Integer);
use Multiply_Integer; -- If you must

type My_Integer is Range -100..100;
function Multiply_My_Integer is new Multiply(My_Integer);

```



## Aime


```aime
real
multiply(real a, real b)
{
    return a * b;
}
```



## ALGOL 60

 '''begin'''
     '''comment''' Function definition;

     '''integer''' '''procedure''' multiply(a,b);
     '''integer''' a,b;
     '''begin'''
         multiply:=a*b;
     '''end''';

     '''integer''' c;
     c:=multiply(2,2);
     outinteger(1,c)
 '''end'''
{{out}}

```txt

 4

```




## ALGOL 68


```algol68
PROC multiply = ( LONG REAL a, b ) LONG REAL:
(
  a * b
)
```


=={{header|ALGOL-M}}==
This implementation takes two integers and returns an integer. Note that a function is distinguished from a procedure, which does not return a value.

```algol
INTEGER FUNCTION MULTIPLY( A, B );
INTEGER A, B;
BEGIN
    MULTIPLY := A * B;
END;
```



## ALGOL W


```algolw
long real procedure multiply( long real value a, b );
begin
    a * b
end
```



## AmigaE


```amigae
PROC my_molt(a,b)
-> other statements if needed... here they are not
ENDPROC a*b    -> return value

-> or simplier

PROC molt(a,b) IS a*b

PROC main()
  WriteF('\d\n', my_molt(10,20))
ENDPROC
```



## AntLang


```AntLang
multiply: * /`*' is a normal function
multiply: {x * y}
```

Explicit definition has the syntax:

```AntLang
{expr-or-def1; expr-or-def2; ..; return-expr}
```

Inside functions, the variable args contains the sequence of arguments.
x, y and z contain the first, second and third argument.

## APL


```apl
       multiply  ←  ×
```

Works on arrays of any rank (any number of dimensions): atoms, lists, tables, etc.


## AppleScript


```AppleScript
on multiply(a, b)
    return a * b
end
```



## Applesoft BASIC

Applesoft BASIC functions are unary meaning they only take one argument.  As the task asks for a multiply function which takes two arguments this poses a problem.  To get around this, the multiply function MU takes one argument as the offset into an array of parameters.

Function names in Applesoft BASIC can be longer than two characters but only the first two characters are significant. Function names cannot contain any keywords.


```basic
10  DEF  FN MULTIPLY(P) =  P(P) * P(P+1)
20  P(1) = 611 : P(2) = 78 : PRINT  FN MULTIPLY(1)
```



```basic>47658</lang



## Argile


```Argile
use std
.: multiply <real a, real b> :. -> real {a * b}
```

with a macro and a variable number of parameters:

```Argile
use std
=: multiply <real a> [<real b>...] := -> real {Cgen a (@@1 (Cgen " * " b))}
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program functMul.s   */
/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1

/***********************/
/* Initialized data */
/***********************/
.data
szRetourLigne: .asciz "\n"
szMessResult:  .ascii "Resultat : "      @ message result
sMessValeur:   .fill 12, 1, ' '
                   .asciz "\n"
/***********************
/* No Initialized data */
/***********************/
.bss

.text
.global main
main:
    push {fp,lr}    /* save  2 registers */

    @ function multiply
	mov r0,#8
	mov r1,#50
	bl multiply             @ call function
    ldr r1,iAdrsMessValeur
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message

    mov r0, #0                  @ return code

100: /* end of  program */
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrsMessValeur: .int sMessValeur
iAdrszMessResult: .int szMessResult
/******************************************************************/
/*   Function multiply              */
/******************************************************************/
/* r0 contains value 1 */
/* r1 contains value 2 */
/* r0 return résult   */
multiply:
    mul r0,r1,r0
    bx lr	        /* return function */

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
/*   conversion register in string décimal signed  */
/***************************************************/
/* r0 contains the register   */
/* r1 contains address of conversion area */
conversion10S:
    push {fp,lr}    /* save registers frame and return */
    push {r0-r5}   /* save other registers  */
    mov r2,r1       /* early storage area */
    mov r5,#'+'     /* default sign is + */
    cmp r0,#0       /* négatif number ? */
    movlt r5,#'-'     /* yes sign is - */
    mvnlt r0,r0       /* and inverse in positive value */
    addlt r0,#1
    mov r4,#10   /* area length */
1: /* conversion loop */
    bl divisionpar10 /* division  */
    add r1,#48        /* add 48 at remainder for conversion ascii */
    strb r1,[r2,r4]  /* store byte area r5 + position r4 */
    sub r4,r4,#1      /* previous position */
    cmp r0,#0
    bne 1b	       /* loop if quotient not equal zéro */
    strb r5,[r2,r4]  /* store sign at current position  */
    subs r4,r4,#1   /* previous position */
    blt  100f         /* if r4 < 0  end  */
    /* else complete area with space */
    mov r3,#' '   /* character space */
2:
    strb r3,[r2,r4]  /* store  byte  */
    subs r4,r4,#1   /* previous position */
    bge 2b        /* loop if r4 greather or equal zero */
100:  /*  standard end of function  */
    pop {r0-r5}   /*restaur others registers */
    pop {fp,lr}   /* restaur des  2 registers frame et return  */
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
   push {r2-r4}   /* save autres registres  */
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
multiply {
	&0 * &1
}

print $(multiply 3 7)
```

Or:

```arturo
multiply [x,y]{
	return x*y
}

print $(multiply 3 7)
```

{{out}}

```txt
21
```




## AutoHotkey


```autohotkey
MsgBox % multiply(10,2)

multiply(multiplicand, multiplier) {
  Return (multiplicand * multiplier)
}
```



## AutoIt


```AutoIt
#AutoIt Version: 3.2.10.0
$I=11
$J=12
MsgBox(0,"Multiply", $I &" * "& $J &" = " & product($I,$J))
Func product($a,$b)
   Return $a * $b
EndFunc
```



## AWK


```awk
function multiply(a, b)
{
  return a*b
}
BEGIN {
  print multiply(5, 6)
}
```



## Axe


```axe
Lbl MULT
r₁*r₂
Return
```



## BASIC

{{works with|QBasic}}

```qbasic
DECLARE FUNCTION multiply% (a AS INTEGER, b AS INTEGER)

FUNCTION multiply% (a AS INTEGER, b AS INTEGER)
    multiply = a * b
END FUNCTION
```


=
## Commodore BASIC
=
In Commodore BASIC function definition can consist of any mathematical operation other functions or commands which result in a numeric expression. The definition is limited to single statement, and it accepts only a single argument. When using the function, keyword fn must precede the function name, which itself must be uniquely distinguishable by its first two characters.

```basic
10 DEF FN MULT(X) = X*Y
20 Y = 4 : REM VALUE OF SECOND ARGUMENT MUST BE ASSIGNED SEPARATELY
30 PRINT FN MULT(3)
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 DEF MULTIPLY(A,B)=A*B
```



## Batch File

Windows batch files only have procedures, not functions. Instead, environmental variables can be used as a global shared state.
<lang>@ECHO OFF
SET /A result = 0
CALL :multiply 2 3
ECHO %result%
GOTO :eof

:multiply
    SET /A result = %1 * %2
    GOTO :eof

:eof
```



## BBC BASIC

BBC BASIC supports both single-line and multi-line function definitions. Note that the function name ''must'' begin with '''FN'''.

Single-line function:

```bbcbasic
PRINT FNmultiply(6,7)
END

DEF FNmultiply(a,b) = a * b
```

Multiline function:

```bbcbasic
DEF FNmultiply(a,b)
LOCAL c
c = a * b
= c
```



## bc

{{Works with|GNU bc}}

```bc
define multiply(a, b) { return a*b }

print multiply(2, 3)
```



## Boo


```boo
def multiply(x as int, y as int):
    return x * y

print multiply(3, 2)
```



## Bracmat


```bracmat
multiply=a b.!arg:(?a.?b)&!a*!b;
out$multiply$(123456789.987654321); { writes 121932631112635269 to standard output }
```



## Brat


```brat
multiply = { x, y | x * y }

p multiply 3 14  #Prints 42
```



## C


```c
double multiply(double a, double b)
{
   return a * b;
}
```


### Macros

Macros can be defined at the top of a program and the compiler will replace the function calls with the function itself before compiling the program (the source file will not change).

```c
#define MULTIPLY(X, Y) ((X) * (Y))
```

Parentheses should be added around parameters in the function definition to avoid order of operations errors when someone uses the macro as such:

```c
x = MULTIPLY(x + z, y);
```

A program with that call would be compiled as if this were coded instead:

```c
x = ((x + z) * (y));
```

Another advantage of macros is that they work with all types alike. For example, the above macro can be used both to multiply double values (like the function above), and to multiply int values (giving an int, which the function doesn't).

## C#

```c#
static double multiply(double a, double b)
{
    return a * b;
}
```

Anonymous function:

```c#
Func<double, double, double> multiply = ((a,b) => a*b);
```



## ChucK

<lang>
fun float multiply (float a, float b)
{
    return a * b;
}
// uncomment next line and change values to test
//<<< multiply(16,4) >>>;

```



## C++

C++ functions basically are the same as in C. Also macros exist, however they are discouraged in C++ in favour of inline functions and function templates.

An inline function differs from the normal function by the keyword inline and the fact that it has to be included in every translation unit which uses it (i.e. it normally is written directly in the header). It allows the compiler to eliminate the function without having the disadvantages of macros (like unintended double evaluation and not respecting scope), because the substitution doesn't happen at source level, but during compilation. An inline version of the above function is:

```cpp
inline double multiply(double a, double b)
{
   return a*b;
}
```

If not only doubles, but numbers of arbitrary types are to be multiplied, a function template can be used:

```cpp
template<typename Number>

Number multiply(Number a, Number b)
{
   return a*b;
}
```

Of course, both inline and template may be combined (the <tt>inline</tt> then has to follow the <tt>template&lt;...&gt;</tt>), but since templates have to be in the header anyway (while the standard allows them to be compiled separately using the keyword <tt>export</tt>, almost no compiler implements that), the compiler usually can inline the template even without the keyword.


## Clay


```Clay
multiply(x,y) = x * y;
```




## Clojure


```lisp
(defn multiply [x y]
  (* x y))

(multiply 4 5)
```

Or with multiple arities (in the manner of the actual <tt>*</tt> function):

```lisp
(defn multiply
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
    (reduce * (* x y) more)))

(multiply 2 3 4 5)  ; 120
```



## COBOL

In COBOL, ''multiply'' is a reserved word, so the requirements must be relaxed to allow a different function name. The following uses a program:
{{works with|OpenCOBOL}}

```COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. myTest.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  x   PIC 9(3) VALUE 3.
       01  y   PIC 9(3) VALUE 2.
       01  z   PIC 9(9).
       PROCEDURE DIVISION.
           CALL "myMultiply" USING
               BY CONTENT x, BY CONTENT y,
               BY REFERENCE z.
           DISPLAY z.
           STOP RUN.
       END PROGRAM myTest.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. myMultiply.
       DATA DIVISION.
       LINKAGE SECTION.
       01  x   PIC 9(3).
       01  y   PIC 9(3).
       01  z   PIC 9(9).
       PROCEDURE DIVISION USING x, y, z.
           MULTIPLY x BY y GIVING z.
           EXIT PROGRAM.
       END PROGRAM myMultiply.
```


This example uses user-defined functions, which were added in COBOL 2002.
{{works with|GNU Cobol|2.0}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. myTest.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION myMultiply.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  x   PIC 9(3) VALUE 3.
       01  y   PIC 9(3) VALUE 2.
       PROCEDURE DIVISION.
           DISPLAY myMultiply(x, y).
           STOP RUN.
       END PROGRAM myTest.

       IDENTIFICATION DIVISION.
       FUNCTION-ID. myMultiply.
       DATA DIVISION.
       LINKAGE SECTION.
       01  x   PIC 9(3).
       01  y   PIC 9(3).
       01  z   pic 9(9).
       PROCEDURE DIVISION USING x, y RETURNING z.
           MULTIPLY x BY y GIVING z.
           EXIT FUNCTION.
       END FUNCTION myMultiply.
```



## Coco


As CoffeeScript. In addition, Coco provides some syntactic sugar for accessing the <code>arguments</code> array reminiscent of Perl's <code>@_</code>:


```coco
multiply = -> @@0 * @@1
```


Furthermore, when no parameter list is defined, the first argument is available as <code>it</code>:


```coco
double = -> 2 * it
```



## CoffeeScript


```coffeescript
multiply = (a, b) -> a * b
```



## ColdFusion


```coldfusion
<cffunction name="multiply" returntype="numeric">
	<cfargument name="a" type="numeric">
	<cfargument name="b" type="numeric">
	<cfreturn a * b>
</cffunction>
```



## Common Lisp

Common Lisp has ordinary functions and generic functions.

### Ordinary Functions

Ordinary functions operate on the values of argument expressions. Lisp functions terminate by returning one or more values, or by executing a non-local dynamic control transfer, in which case values are not returned.

```lisp
(defun multiply (a b)
  (* a b))

(multiply 2 3)
```

====User-Defined Compiler Optimization of Functions====
In Lisp we can express optimizations of calls to a function using compiler macros. For instance, suppose we know that the multiply function, which may be in another module, simply multiplies numbers together. We can replace a call to multiply by a constant, if the arguments are constant expressions. Like the usual kind of Lisp macro, the compiler macro takes the argument forms as arguments, not the argument values. The special keyword &whole gives the macro access to the entire expression, which is convenient for the unhandled cases, whereby no transformation takes place:

```lisp
(define-compiler-macro multiply (&whole expr a b)
  (if (and (constantp a) (constantp b))
    (* (eval a) (eval b))
    expr)) ;; no macro recursion if we just return expr; the job is done!
```

Lisp implementations do not have to honor compiler macros. Usually compilers make use of them, but evaluators do not.

Here is test of the macro using a CLISP interactive session. Note that the multiply function is not actually defined, yet it compiles and executes anyway, which shows that the macro provided the translation something.

```txt
$ clisp -q
[1]> (define-compiler-macro multiply (&whole expr a b)
  (if (and (constantp a) (constantp b))
    (* (eval a) (eval b))
    expr))
MULTIPLY
[2]> (defun test1 () (multiply 2 3))
TEST1
[3]> (compile 'test1)
TEST1 ;
NIL ;
NIL
[4]> (disassemble 'test1)

Disassembly of function TEST1
(CONST 0) = 6
[ ... ]
2 byte-code instructions:
0     (CONST 0)                           ; 6
1     (SKIP&RET 1)
NIL
[5]> (test1)
6
```


### Generic Functions

Lisp's generic functions are part of the object system. Generic functions are compiled to ordinary functions, and so are called in the ordinary way. Internally, however, they have the special behavior of dispatching one or more methods based on specializable parameters.

Methods can be defined right inside the DEFGENERIC construct, but usually are written with separate DEFMETHODS.

Also, the DEFGENERIC is optional, since the first DEFMETHOD will define the generic function, but good practice.

```lisp

;;; terrific example coming

```


## Creative Basic


```Creative Basic

DECLARE Multiply(N1:INT,N2:INT)

DEF A,B:INT

A=2:B=2

OPENCONSOLE

PRINT Multiply(A,B)

PRINT:PRINT"Press any key to close."

DO:UNTIL INKEY$<>""

CLOSECONSOLE

END

SUB Multiply(N1:INT,N2:INT)

     DEF Product:INT

     Product=N1*N2

RETURN Product

'Can also be written with no code in the subroutine and just RETURN N1*N2.

```



## D


```d
// A function:
int multiply1(int a, int b) {
    return a * b;
}

// Functions like "multiply1" can be evaluated at compile time if
// they are called where a compile-time constant result is asked for:
enum result = multiply1(2, 3); // Evaluated at compile time.
int[multiply1(2, 4)] array;    // Evaluated at compile time.

// A templated function:
T multiply2(T)(T a, T b) {
    return a * b;
}

// Compile-time multiplication can also be done using templates:
enum multiply3(int a, int b) = a * b;

pragma(msg, multiply3!(2, 3)); // Prints "6" during compilation.

void main() {
    import std.stdio;
    writeln("2 * 3 = ", result);
}
```

Both the compile-time and run-time output:

```txt
6
2 * 3 = 6
```



## dc

For dc, the functions (called macros) are limited to names from 'a' to 'z'
Create a function called 'm'

```dc
[*] sm
```

Use it (lm loads the function in 'm',x executes it, f shows the the stack.)

```dc
3 4 lm x f
= 12
```



## Delphi


```Delphi
function Multiply(a, b: Integer): Integer;
begin
  Result := a * b;
end;
```


=={{header|Déjà Vu}}==

```dejavu
multiply a b:
    * a b
```



## Dragon


```dragon
def multiply(a, b) {
  return a*b
}
```



## DWScript


```Delphi
function Multiply(a, b : Integer) : Integer;
begin
   Result := a * b;
end;
```



## Dyalect


```Dyalect
func multiply(a, b) {
    a * b
}
```


Using lambda syntax:


```Dyalect
const multiply = (a, b) => a * b
```


Using shorthand function syntax:


```Dyalect
const multiply = $0 * $1
```



## E


```e
def multiply(a, b) {
    return a * b
}
```

(This does not necessarily return a product, but whatever the "multiply" method of <var>a</var> returns. The parameters could be guarded to only accept standard numbers.)

It is also possible to write short anonymous function definitions which do not need explicit returns:

```e
def multiply := fn a, b { a * b }
```

This definition is identical to the previous except that the function object will not know its own name.


## EasyLang

<lang>func multiply a b . r .
  r = a * b
.
call multiply 7 5 res
print res
```



## EchoLisp


```lisp

(define (multiply a b) (* a b)) → multiply ;; (1)
(multiply 1/3 666) → 222

;; a function is a lambda definition :
multiply
     → (λ (_a _b) (#* _a _b))

;; The following is the same as (1) :
(define multiply (lambda(a b) (* a b)))
multiply
    → (🔒 λ (_a _b) (#* _a _b)) ;; a closure


;; a function may be compiled
(lib 'compile)
(compile 'multiply "-float-verbose")
    →
💡 [0]     compiling _🔶_multiply ((#* _a _b))
;; object code (javascript) :
var ref,top = _blocks[_topblock];
/* */return (
/* */(_stack[top] *_stack[1 + top])
/* */);

multiply  → (λ (_a _b) (#🔶_multiply)) ;; compiled function

```



## Efene


```efene
multiply = fn (A, B) {
    A * B
}

@public
run = fn () {
    io.format("~p~n", [multiply(2, 5)])
}
```



## Eiffel


```Eiffel

multiply(a, b: INTEGER): INTEGER
	do
		Result := a*b
	end

```



## Ela


```Ela
multiply x y = x * y
```

Anonymous function:

```Ela
\x y -> x * y
```


## Elena


```elena
real multiply(real a, real b)
        = a * b;
```

Anonymous function / closure:

```elena
symbol f := (x,y => x * y);
```

Root closure:

```elena
f(x,y){ ^ x * y }
```



## Elixir


```elixir
defmodule RosettaCode do
  def multiply(x,y) do
    x * y
  end

  def task, do: IO.puts multiply(3,5)
end

RosettaCode.task
```


{{out}}

```txt

15

```



## Elm


```Elm

--There are multiple ways to create a function in Elm

--This is a named function
multiply x y = x*y

--This is an anonymous function
\x y -> x*y

```


## Emacs Lisp


```Lisp
(defun multiply (x y)
  (* x y))
```


A "docstring" can be added as follows.  This is shown by the Emacs help system and is good for human users.  It has no effect on execution.


```Lisp
(defun multiply (x y)
  "Return the product of X and Y."
  (* x y))
```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(func_definition).
-export([main/0]).

main() ->
	K=multiply(3,4),
	io :format("~p~n",[K]).

multiply(A,B) ->
	case {A,B} of
		{A, B} -> A * B
	end.
```

{{out}}

```txt
12
ok

```



## ERRE

A statement function in ERRE is a '''single line''' function definition as in Fortran 77 or BASIC. These are useful in defining functions that can be expressed with a single formula. A statement function should appear in declaration part of the program. The format is simple - just type
 FUNCTION f(x,y,z,…)
    f=formula
 END FUNCTION

The main features of function statement are:

1) You can use relational operators, so it's possible to "compact" an IF THEN ELSE statement but not loop statements: you must use a procedure for these.

2) Functions can have their own identifier (integer, string, real,double).

3) It's possible to declare function with no parameter: use FUNCTION f()........

4) Functions always return '''one''' value.

5) ERRE for C-64 admits only real with one parameter functions.

 FUNCTION MULTIPLY(A,B)
    MULTIPLY=A*B
 END FUNCTION

Usage:

  IF MULTIPLY(A,B)>10 THEN ......

or

  S=MULTIPLY(22,11)


## Euphoria


```Euphoria
function multiply( atom a, atom b )
    return a * b
end function
```

If you declare the arguments as <code>object</code> then sequence comprehension kicks in:

```Euphoria
function multiply( object a, object b )
    return a * b
end function

sequence a = {1,2,3,4}
sequence b = {5,6,7,8}

? multiply( 9, 9 )
? multiply( 3.14159, 3.14159 )
? multiply( a, b )
? multiply( a, 7 )
? multiply( 10.39564, b )
```

{{out}}

```txt
81
9.869587728
{5,12,21,32}
{7,14,21,28}
{51.9782,62.37384,72.76948,83.16512}
```


=={{header|F Sharp|F#}}==
The default will be an integer function but you can specify other types as shown:

```fsharp
let multiply x y = x * y // integer
let fmultiply (x : float) (y : float) = x * y
```



## Factor


```factor
: multiply ( a b -- a*b ) * ;
```



## Falcon


```falcon
function sayHiTo( name )
 > "Hi ", name
end
```



## FALSE


```false
[*]     {anonymous function to multiply the top two items on the stack}
m:      {binding the function to one of the 26 available symbol names}
2 3m;!  {executing the function, yielding 6}
```



## Fantom


```fantom
class FunctionDefinition
{
  public static Void main ()
  {
    multiply := |Int a, Int b -> Int| { a * b }
    echo ("Multiply 2 and 4: ${multiply(2, 4)}")
  }
}
```



## Fexl


```fexl
\multiply=(\x\y * x y)
```

Or if I'm being cheeky:

```fexl
\multiply=*
```



## Fish

Functions cannot be named in Fish. However, they can be defined as new stacks that pull a certain number of arguments off the stack that came before. <code>2[</code> says pull 2 values off the stack and put them in a new, separate stack. <code>]</code> says put all remaining values in the current stack onto the top of the stack below (the old stack).

```fish
2[*]
```



## Forth


```forth
: fmultiply ( F: a b -- F: c )  F* ;
: multiply ( a b -- c )  * ;
```



## Fortran

In FORTRAN I (1957), inline function could be defined at the beginning of the program. Let's note than to specify a floating point real the name of the statement function begins with an X (no type declaration) and to specify this is a function the name ends with a F.

```fortran
     XMULTF(X,Y)=X*Y
```

And for interger multiplication:

```fortran
     MULTF(I,J)=I*J
```


In FORTRAN IV, FORTRAN 66 or later, define a function:

```fortran
FUNCTION MULTIPLY(X,Y)
REAL MULTIPLY, X, Y
MULTIPLY = X * Y
END
```

And for integer multiplication:

```fortran
FUNCTION MULTINT(X,Y)
INTEGER MULTINT, X, Y
MULTINT = X * Y
END
```


In Fortran 95 or later, define an elemental function, so that this function can be applied to whole arrays as well as to scalar variables:

```fortran
module elemFunc
contains
    elemental function multiply(x, y)
        real, intent(in) :: x, y
        real :: multiply
        multiply = x * y
    end function multiply
end module elemFunc
```


```fortran
program funcDemo
    use elemFunc

    real :: a = 20.0, b = 30.0, c
    real, dimension(5) :: x = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /), y = (/ 32.0, 16.0, 8.0, 4.0, 2.0 /), z

    c = multiply(a,b)     ! works with either function definition above

    z = multiply(x,y)     ! element-wise invocation only works with elemental function
end program funcDemo
```

It is worth noting that Fortran can call functions (and subroutines) using named arguments; e.g. we can call multiply in the following way:

```fortran
c = multiply(y=b, x=a)   ! the same as multiply(a, b)
z = multiply(y=x, x=y)   ! the same as multiply(y, x)
```

(Because of commutativity property of the multiplication, the difference between <code>multiply(x,y)</code> and <code>multiply(y,x)</code> is not evident)

Also note that the function result can be declared with a different name within the routine:

```fortran
module elemFunc
contains
    elemental function multiply(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        z = x * y
    end function multiply
end module elemFunc
```


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function multiply(d1 As Double, d2 As Double) As Double
  Return d1 * d2
End Function
```

This function could either be used for all numeric types (as they are implicitly convertible to Double)
or could be overloaded to deal with each such type (there are 12 of them).

Alternatively, one could write a macro though this wouldn't be type-safe:


```freebasic
#Define multiply(d1, d2) (d1) * (d2)
```



## Frink


```frink
multiply[x,y] := x*y
```



## Futhark

{{incorrect|Futhark|Futhark's syntax has changed, so this example will not compile}}


```Futhark

fun multiply(x: int, y: int): int = x * y

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

local fn multiply( a as long, b as long ) as long
end fn = a * b

print fn multiply( 3, 9 )

```

Output:

```txt

27

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=bc93236474d9937217dd4117026f7441 Click this link to run this code]'''

```gambas
Public Sub Main()

Print Multiply(56, 4.66)

End

Public Sub Multiply(f1 As Float, f2 As Float) As Float

Return f1 * f2

End
```

Output:

```txt

260.96

```



## GAP


```gap
multiply := function(a, b)
    return a*b;
end;
```



## GML

In GML one can not define a function but in [[Game Maker]] there is a ''script'' resource, which is the equivalent of a function as defined here. Scripts can be exported to or imported from a text file with the following format:

```GML
#define multiply
a = argument0
b = argument1
return(a * b)
```



## Gnuplot


```Gnuplot
multiply(x,y) = x*y

# then for example
print multiply(123,456)
```



## Go

Function return types in Go are statically typed and never depend on argument types.

The return statement can contain an expression of the function return type:

```go
func multiply(a, b float64) float64 {
   return a * b
}
```

Alternatively, if the return value is named, the return statement does not require an expression:

```go
func multiply(a, b float64) (z float64) {
   z = a * b
   return
}
```



## Golfscript


```golfscript
{*}:multiply;
```



## Groovy


```groovy
def multiply = { x, y -> x * y }
```

Test Program:

```groovy
println "x * y = 20 * 50 = ${multiply 20, 50}"
```

{{out}}

```txt
x * y = 20 * 50 = 1000
```



## Halon


```halon
function multiply( $a, $b )
{
    return $a * $b;
}
```



## Haskell


```haskell
multiply x y = x * y
```

Alternatively, with help of auto-currying,

```haskell
multiply = (*)
```

You can use [[lambda-function]]

```haskell
multiply = \ x y -> x*y
```



## Haxe


```haxe
function multiply(x:Float, y:Float):Float{
   return x * y;
}
```



## hexiscript


```hexiscript
fun multiply a b
  return a * b
endfun
```



## HicEst


```hicest
FUNCTION multiply(a, b)
   multiply = a * b
END
```



## HolyC


```holyc
F64 Multiply(F64 a, F64 b) {
  return a * b;
}

F64 x;
x = Multiply(42, 13.37);
Print("%5.2f\n", x);
```



## Hy

Function definition:

```clojure
(defn multiply [a b]
  (* a b))
```

Lambda definition:

```clojure
(def multiply (fn [a b] (* a b)))
```



## i


```i

concept multiply(a, b) {
	return a*b
}

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure multiply(a,b)
return a * b
end
```



## IDL

The task description is unclear on what to do when the arguments to the function are non-scalar, so here's multiple versions:

```idl
function multiply ,a,b
  return, a* b
end
```

If "a" and "b" are scalar, this will return a scalar. If they are arrays of the same dimensions, the result is an array of the same dimensions where each element is the product of the corresponding elements in "a" and "b".

Alternatively, there's this possibility:

```idl
function multiply ,a,b
  return, product([a, b])
end
```

This will yield the same result for scalars, but if "a" and "b" are arrays it will return the product of all the elements in both arrays.

Finally, there's this option:

```idl
function multiply ,a,b
  return, a # b
end
```

This will return a scalar if given scalars, if given one- or two-dimensional arrays it will return the matrix-product of these arrays. E.g. if given two three-element one-dimensional arrays (i.e. vectors), this will return a 3x3 matrix.


## Inform 6


```inform6
[ multiply a b;
  return a * b;
];
```



## Inform 7


```inform7
To decide which number is (A - number) multiplied by (B - number):
	decide on A * B.
```



## Io


```io
multiply := method(a,b,a*b)
```


## IWBASIC


```IWBASIC

'1. Not Object Oriented Program

DECLARE Multiply(N1:INT,N2:INT),INT

DEF A,B:INT

A=2:B=2

OPENCONSOLE

PRINT Multiply(A,B)

PRINT

'When compiled as a console only program, a press any key to continue is automatic.
CLOSECONSOLE

END

SUB Multiply(N1:INT,N2:INT),INT

     DEF Product:INT

     Product=N1*N2

RETURN Product
ENDSUB

'Can also be written with no code in the subroutine and just RETURN N1*N2.

----

'2. Not Object Oriented Program Using A Macro

$MACRO Multiply (N1,N2) (N1*N2)

DEF A,B:INT

A=5:B=5

OPENCONSOLE

PRINT Multiply (A,B)

PRINT

'When compiled as a console only program, a press any key to continue is automatic.
CLOSECONSOLE

END

----

'3. In An Object Oriented Program

CLASS Associate
'functions/methods
DECLARE Associate:'object constructor
DECLARE _Associate:'object destructor
'***Multiply declared***
DECLARE Multiply(UnitsSold:UINT),UINT
'members
DEF m_Price:UINT
DEF m_UnitsSold:UINT
DEF m_SalesTotal:UINT
ENDCLASS

DEF Emp:Associate

m_UnitsSold=10

Ass.Multiply(m_UnitsSold)

OPENCONSOLE

PRINT"Sales total: ",:PRINT"$"+LTRIM$(STR$(Emp.m_SalesTotal))

PRINT

CLOSECONSOLE

END

'm_price is set in constructor
SUB Associate::Multiply(UnitsSold:UINT),UINT
     m_SalesTotal=m_Price*UnitsSold
     RETURN m_SalesTotal
ENDSUB

SUB Associate::Associate()
     m_Price=10
ENDSUB

SUB Associate::_Associate()
'Nothing to cleanup
ENDSUB


```



## J


```j
multiply=: *
```

Works on conforming arrays of any rank (any number of dimensions, as long as the dimensions of one are a prefix of the dimensions of the other): atoms, lists, tables, etc.

Or, more verbosely (and a bit slower, though the speed difference should be unnoticeable in most contexts):

```J
multiply=: dyad define
  x * y
)
```

Here we use an [http://www.jsoftware.com/help/dictionary/intro18.htm explicit] definition (where the arguments are named) rather than a [http://www.jsoftware.com/help/dictionary/intro19.htm tacit] version (where the arguments are implied).  In explicit J verbs, x is the left argument and y is the right argument.

(Note, by the way, that explicit definitions are a subset of tacit definitions -- when the arguments are explicitly named they are still implied in the larger context containing the definition.)


## Java

There are no global functions in Java. The equivalent is to define static methods in a class (here invoked as "Math.multiply(a,b)"). Overloading allows us to define the method for multiple types.

```java
public class Math
{
     public static    int multiply(   int a,    int b) { return a*b; }
     public static double multiply(double a, double b) { return a*b; }
}
```



## JavaScript

===ES1-*===
Function Declaration

```javascript
function multiply(a, b) {
  return a*b;
}
```


===ES3-*===
Function Expression

```javascript
var multiply = function(a, b) {
    return a * b;
};
```


Named Function Expression

```javascript
var multiply = function multiply(a, b) {
    return a * b;
};
```


Method Definition

```javascript
var o = {
  multiply: function(a, b) {
    return a * b;
  }
};
```


===ES5-*===
Accessors

```javascript
var o = {
  get foo() {
    return 1;
  },
  set bar(value) {
    // do things with value
  }
};
```



===ES6-*===
Arrow Function

```javascript
var multiply = (a, b) => a * b;
var multiply = (a, b) => { return a * b };

```


Concise Body Method Definition

```javascript
var o = {
  multiply(a, b) {
    return a * b;
  }
};
```


Generator Functions

```javascript
function * generator() {
  yield 1;
}
```



## Joy


```joy
DEFINE multiply == * .
```




## jq

Example of a simple function definition:
```jq
def multiply(a; b): a*b;
```

Example of the definition of an inner function:
```jq
# 2 | generate(. * .) will generate 2, 4, 16, 256, ...
def generate(f): def r: ., (f | r); r;
```

The previous example (generate/1) also illustrates that a function argument can be a function or composition of functions.  Here is another example:
```jq
def summation(f): reduce .[] as $x (0; . + ($x|f));
```

<tt>summation/1</tt> expects an array as its input and takes a function, f, as its argument. For example, if the input array consists of JSON objects with attributes "h" and "w", then to compute SIGMA (h * w) we could simply write:
```jq
summation( .h * .w)
```



## Julia

{{works with|Julia|0.6}}
General function definition:


```julia
function multiply(a::Number, b::Number)
  return a * b
end
```


Julia also supports `assignment` definition as shorthand:


```julia
multiply(a, b) = a * b
```


And lambda calculus:


```julia
multiply = (a, b) -> a * b
```



## Kaya


```kaya
program test;

// A function definition in Kaya:
Int multiply(Int a, Int b) {
    return a * b;
}

// And calling a function:
Void main() {
    putStrLn(string( multiply(2, 3) ));
}
```



## Kotlin


```kotlin
// One-liner
fun multiply(a: Int, b: Int) = a * b

// Proper function definition
fun multiplyProper(a: Int, b: Int): Int {
    return a * b
}
```



## Lasso


Lasso supports multiple dispatch — signature definitions determine which method will be invoked.


```Lasso
define multiply(a,b) => {
	return #a * #b
}
```


As this function is so simple it can also be represented like so:


```Lasso
define multiply(a,b) => #a * #b
```


Using multiple dispatch, different functions will be invoked depending on the functions input.


```Lasso
// Signatures that convert second input to match first input
define multiply(a::integer,b::any) => #a * integer(#b)
define multiply(a::decimal,b::any) => #a * decimal(#b)

// Catch all signature
define multiply(a::any,b::any) => decimal(#a) * decimal(#b)
```



## LFE


```lisp

(defun mutiply (a b)
  (* a b))

```



## Liberty BASIC


```lb
'     define & call a function

print multiply( 3, 1.23456)

wait

function multiply( m1, m2)
    multiply =m1 *m2
end function

end
```



## Lily


```Lily
define multiply(a: Integer, b: Integer): Integer
{
  return a * b
}
```



## Lingo


```lingo
on multiply (a, b)
  return a * b
end
```



## LiveCode

LiveCode has a built-in method called multiply, so there is an extra y to avoid an error.

```LiveCode
function multiplyy n1 n2
    return n1 * n2
end multiplyy

put multiplyy(2,5) -- = 10
```



## Locomotive Basic


```locobasic
10 DEF FNmultiply(x,y)=x*y
20 PRINT FNmultiply(2,PI)
```

Function names are always preceded by "FN" in Locomotive BASIC. Also, PI is predefined by the interpreter as 3.14159265.


## Logo


```logo
to multiply :x :y
  output :x * :y
end
```



## LSE64


```lse64
multiply  : *
multiply. : *.  # floating point
```



## Lua


```Lua
function multiply( a, b )
    return a * b
end
```



## Lucid


```lucid
multiply(x,y) = x * y
```



## M2000 Interpreter


### A Module can return value

A module can return value to stack of values. Calling a module we place parent stack to module, so we can read any value.

```M2000 Interpreter

Module Checkit {
      Module Multiply (a, b) {
            Push a*b
      }
      Multiply 10, 5
      Print Number=50

      Module Multiply {
            Push Number*Number
      }

      Multiply 10, 5
      Print Number=50
      \\ push before call
      Push 10, 5
      Multiply
      Read A
      Print A=50
      Push 10, 2,3 : Multiply : Multiply: Print Number=60
      Module Multiply {
            If not match("NN") Then Error "I nead two numbers"
            Read a, b
            Push a*b
      }
      Call Multiply 10, 5
      Print Number=50
      \\ now there are two values in stack 20 and 50
      Multiply
}
Call Checkit, 20, 50
Print Number=1000

```



### A Local Function Definition


There are two types of function, the normal and the lambda. If a Function return string then we have to use $ at the end of function name.

```M2000 Interpreter

Module Checkit {
      \\ functions can shange by using a newer definition
      \\ function Multiply is local, and at the exit of Checkit, erased.
      Function Multiply (a, b) {
            =a*b
      }
      Print Multiply(10, 5)=50

      Function Multiply {
            =Number*Number
      }

      Print Multiply(10, 5)=50

      Function Multiply {
            If not match("NN") Then Error "I nead two numbers"
            Read a, b
            =a*b
      }
      Print Multiply(10, 5)=50
      Function Multiply {
            Read a as long, b as long
            =a*b
      }
      Z=Multiply(10, 5)
      Print Z=50, Type$(Z)="Long"
      Function Multiply(a as decimal=1, b as decimal=2) {
            =a*b
      }
      D=Multiply(10, 5)
      Print D=50, Type$(D)="Decimal"
      D=Multiply( , 50)
      Print D=50, Type$(D)="Decimal"
      D=Multiply( 50)
      Print D=100, Type$(D)="Decimal"
      \\ by reference plus using type
      Function Multiply(&a as decimal, &b as decimal) {
            =a*b
            a++
            b--
      }
      alfa=10@
      beta=20@
      D=Multiply(&alfa, &beta)
      Print D=200, alfa=11,beta=19, Type$(D)="Decimal"
      \\ Using Match() to identify type of items at the top of stack
      Function MultiplyALot {
            M=Stack
            While Match("NN") {
                  mul=Number*Number
                  Stack M {
                        Data mul  ' at the bottom
                  }
            }
            =Array(M)
      }

      K=MultiplyALot(1,2,3,4,5,6,7,8,9,10)
      N=Each(K)
      While N {
            Print Array(N),     ' we get 2  12   30   56   90
      }
      Print
}
Checkit

```



### A Lambda Function

Lambda function is first citizen. We can push it to stack and make another reading from stack. Lambda can use closures as static variables, some of them are pointers so if we copy a lambda we just copy the pointer. Pointers are containers like pointer to array, inventory and stack. Here we define string lambda function (there is a numeric also)


```M2000 Interpreter

Module CheckIt {
      A$=Lambda$ N$="Hello There" (x) ->{
            =Mid$(N$, x)
      }
      Print A$(4)="lo There"
      Push A$
}
CheckIt
Read B$
Print B$(1)="Hello There"
Function List$ {
      Dim Base 1,   A$()
      A$()=Array$([])  ' make an array from stack items
      =lambda$ A$() (x) -> {
            =A$(x)
      }

}
\\ change definition/closures
B$=List$("Hello", "Rosetta", "Function")
Print B$(1)="Hello"

```



## M4


```M4
define(`multiply',`eval($1*$2)')

multiply(2,3)
```



## Make

In makefile, a function may be defined as a rule, with recursive make used to retrieve the returned value.

```make
A=1
B=1

multiply:
   @expr $(A) \* $(B)
```

Invoking it

```make
make -f mul.mk multiply A=100 B=3
> 300
```

Using gmake, the define syntax is used to define a new function
{{works with|gmake}}

```make
A=1
B=1

define multiply
   expr $(1) \* $(2)
endef

do:
   @$(call multiply, $(A), $(B))

|gmake -f mul.mk do A=5 B=3
```



## Maple


```maple
multiply:= (a, b) -> a * b;
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
There are two ways to define a function in Mathematica.

Defining a function as a transformation rule:

```Mathematica
multiply[a_,b_]:=a*b
```

Defining a pure function:

```Mathematica
multiply=#1*#2&
```



## Maxima


```Maxima
f(a, b):= a*b;
```



## MAXScript


```maxscript
fn multiply a b =
(
    a * b
)
```



## Mercury


```Mercury
% Module ceremony elided...
:- func multiply(integer, integer) = integer.
multiply(A, B) = A * B.
```



## Metafont

Metafont has macros, rather than functions; through those the language can be expanded. According to the kind of macro we are going to define, Metafont has different ways of doing it. The one suitable for this task is called <code>primarydef</code>.

```metafont
primarydef a mult b = a * b enddef;
```


```metafont>t := 3 mult 5; show t; end</lang

The '''primarydef''' allows to build binary operators with the same priority as *. For a more generic macro, we can use instead

```metafont
def mult(expr a, b) = (a * b) enddef;
t := mult(2,3);
show t;
end
```



## min

<code>'*</code> is syntax sugar for <code>(*)</code>, which is an anonymous function that takes two numbers from the data stack, multiplies them, and leaves the result on the data stack. To give it a name, we can use the <code>:</code> sigil which is syntax sugar for <code>define</code>.

```min
'* :multiply
```


=={{header|МiniZinc}}==

```txt

function var int:multiply(a: var int,b: var int) =
    a*b;

```

=={{header|MK-61/52}}==

```txt

ИП0 ИП1 * В/О

```


Function (subprogram) that multiplies two numbers. Parameters in registers Р0 and Р1, the result (return value) in register X. Commands ''ИП0'' and ''ИП1'' cause the contents of the corresponding registers in the stack, the more they multiplied (command ''*'') and then code execution goes to the address from which the call subprogram (command ''В/О'').

=={{header|Modula-2}}==

```modula2
PROCEDURE Multiply(a, b: INTEGER): INTEGER;
BEGIN
  RETURN a * b
END Multiply;
```


=={{header|Modula-3}}==

```modula3
PROCEDURE Multiply(a, b: INTEGER): INTEGER =
BEGIN
  RETURN a * b;
END Multiply;
```



## MUMPS


```MUMPS
MULTIPLY(A,B);Returns the product of A and B
 QUIT A*B
```



## Nanoquery


```nanoquery
def multiply($a, $b)
    return ($a * $b)
end
```



## Neko


```Neko
var multiply = function(a, b) {
    a * b
}

$print(multiply(2, 3))
```


'''Output:'''
6


## Nemerle


```Nemerle
public Multiply (a : int, b : int) : int  // this is either a class or module method
{
    def multiply(a, b) { return a * b }   // this is a local function, can take advantage of type inference
    return multiply(a, b)
}
```



## NESL


```nesl
function multiply(x, y) = x * y;
```

The NESL system responds by reporting the type it has inferred for the function:

```txt
multiply = fn : (a, a) -> a :: (a in number)
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

pi      = 3.14159265358979323846264338327950
radiusY = 10
in2ft   = 12
ft2yds  = 3
in2mm   = 25.4
mm2m    = 1 / 1000
radiusM = multiply(multiply(radiusY, multiply(multiply(ft2yds, in2ft), in2mm)), mm2m)

say "Area of a circle" radiusY "yds radius: " multiply(multiply(radiusY, radiusY), pi).format(3, 3) "sq. yds"
say radiusY "yds =" radiusM.format(3, 3)  "metres"
say "Area of a circle" radiusM.format(3, 3)"m radius:" multiply(multiply(radiusM, radiusM), pi).format(3, 3)"m**2"


/**
 * Multiplication function
 */
method multiply(multiplicand, multiplier) public static returns Rexx

  product = multiplicand * multiplier
  return product
```

{{out}}

```txt

Area of a circle 10 yds radius:  314.159 sq. yds
10 yds =   9.144 metres
Area of a circle   9.144m radius: 262.677m**2

```



## NewLISP


```NewLISP>
 (define (my-multiply a b) (* a b))
(lambda (a b) (* a b))
> (my-multiply 2 3)
6
```



## Nial

Using variables

```nial
multiply is operation a b {a * b}
```

Using it

```nial
|multiply 2 3
=6
```

Point free form

```nial
mul is *
```

Using it

```nial
|mul 3 4
=12
```

Nial also allows creation of operators

```nial
multiply is op a b {a * b}
```

Using it.

```nial
|2 multiply 3
=6
|multiply 2 3
=6
```

Since this is an array programming language, any parameters can be arrays too

```nial
|mul 3 [1,2]
=3 6
|mul [1,2] [10,20]
=10 40
```



## Nim

Nim has a magic variable, `result`, which can be used as a substitute for `return`. The `result` variable will be returned implicitly.

```nim
proc multiply(a, b: Int): Int =
  result = a * b
```

Here is the same function but with the use of the `return` keyword.

```nim
proc multiply(a, b: Int): Int =
  return a * b
```

The last statement in a function implicitly is the result value:

```nim
proc multiply(a, b: Int): Int = a * b
```



## OASYS

<lang oasys_oac>method int multiply int x int y {
  return x * y
}
```



## OASYS Assembler

OASYS Assembler requires a prefix and suffix on names to indicate their types (an omitted suffix means a void type).
<lang oasys_oaa>[&MULTIPLY#,A#,B#],A#<,B#<MUL RF
```


=={{header|Oberon-2}}==
Oberon-2 uses procedures, and has a special procedure called a "Function Procedure" used to return a value.

```oberon2
PROCEDURE Multiply(a, b: INTEGER): INTEGER;
 BEGIN
    RETURN a * b;
 END Multiply;
```



## Objeck


```objeck
function : Multiply(a : Float, b : Float) ~, Float {
   return a * b;
}
```



## OCaml



```ocaml
let int_multiply x y = x * y
let float_multiply x y = x *. y
```



## Octave


```octave
function r = mult(a, b)
  r = a .* b;
endfunction
```




## Oforth


Function #* is already defined : it removes 2 objects from the stack and returns on the stack the product of them.

If necessary, we can create a function with name multiply, but, it will just call *


```Oforth
: multiply  * ;
```


It is also possible to create a function with declared paramaters. In this case, if we define n parameters, n objects will be removed from the stack and stored into those parameters :


```Oforth
: multiply2(a, b)   a b * ;
```


A function return value (or values) is always what remains on the stack when the function ends. There is no syntax to define explicitely what is the return value(s) of a function.


## Ol

Function creation implemented using keyword 'lambda'. This created anonymous function can be saved into local or global variable for further use.

```scheme

(lambda (x y)
   (* x y))

```


Ol has two fully equal definitions of global named function (second one is syntactic sugar for first one). In fact both of them is saving the created lambda in global variable.

```scheme

(define multiply (lambda (x y) (* x y)))

(define (multiply x y) (* x y))

```


And only one definition of local named functions (with immediate calculation). This type of definition helps to implement local recursions.

```scheme

(let multiply ((x n) (y m))
   (* x y))

; example of naive multiplication function implementation using local recursion:
(define (multiply x y)
   (let loop ((y y) (n 0))
      (if (= y 0)
         n
         (loop (- y 1) (+ n x)))))

(print (multiply 7 8))
; ==> 56

```



## OOC


```ooc

multiply: func (a: Double, b: Double) -> Double {
  a * b
}

```



## ooRexx


### Internal Procedure


```rexx
SAY multiply(5, 6)
EXIT
multiply:
    PROCEDURE
    PARSE ARG x, y
    RETURN x*y
```


### ::Routine Directive


```oorexx

say multiply(5, 6)
::routine multiply
    use arg x, y
    return x *y
```


### Accomodate large factors


```oorexx
say multiply(123456789,987654321)
say multiply_long(123456789,987654321)
::routine multiply
    use arg x, y
    return x *y
::routine multiply_long
    use arg x, y
    Numeric Digits (length(x)+length(y))
    return x *y
```

{{out}}

```txt
1.21932631E+17
121932631112635269
```



## OpenEdge/Progress

<lang Progress (Openedge ABL)>function multiply returns dec (a as dec , b as dec ):
  return a * b .
end.
```



## Oz


```oz
fun {Multiply X Y}
   X * Y
end
```

Or by exploiting first-class functions:

```oz
Multiply = Number.'*'
```



## PARI/GP


```parigp
multiply(a,b)=a*b;
```

or

```parigp
multiply=(a,b)->a*b;
```

Note that in both cases the <code>;</code> is part of the definition of the function, not of the function itself: it suppresses the output of the function body, but does not suppress the output of the function when called.  To do that, either double the semicolon (which will suppress the output of both) or wrap in braces:

```parigp
multiply={(a,b)->a*b;}
```

which will return a function which calculates but does not return the product.


## Pascal

(all versions and dialects)

```pascal
function multiply(a,b: real): real;
begin
  multiply := a*b;
end;
```



## Perl

The most basic form:

```perl
sub multiply { return $_[0] * $_[1] }
```

or simply:

```perl
sub multiply { $_[0] * $_[1] }
```

Arguments in Perl subroutines are passed in the <code>@_</code> array, and they can be accessed directly, first one as <code>$_[0]</code>, second one as <code>$_[1]</code>, etc. When the above function is called with only one or no arguments then the missing ones have an undefined value which is converted to 0 in multiplication.

This is an example using [http://perldoc.perl.org/perlsub.html#Prototypes subroutine prototypes]:

```perl
sub multiply( $$ )
{
   my ($a, $b) = @_;
   return $a * $b;
}
```

The above subroutine can only be called with exactly two [http://perldoc.perl.org/perldata.html#Scalar-values scalar values] (two dollar signs in the signature) but those values may be not numbers or not even defined. The <code>@_</code> array is unpacked into <code>$a</code> and <code>$b</code> lexical variables, which are used later.

The arguments can be automatically unpacked into lexical variables using the experimental signatures feature (in core as of 5.20):

```perl
use experimental 'signatures';
sub multiply ($x, $y) {
    return $x * $y;
}
```



## Perl 6

Without a signature:

```perl6
sub multiply { return @_[0] * @_[1]; }
```

The return is optional on the final statement, since the last expression would return its value anyway.  The final semicolon in a block is also optional.
(Beware that a subroutine without an explicit signature, like this one, magically becomes variadic (rather than nullary) only if <code>@_</code> or <code>%_</code> appear in the body.)  In fact, we can define the variadic version explicitly, which still works for two arguments:

```perl6
sub multiply { [*] @_ }
```

With formal parameters and a return type:

```perl6
sub multiply (Rat $a, Rat $b --> Rat) { $a * $b }
```

Same thing:

```perl6
my Rat sub multiply (Rat $a, Rat $b) { $a * $b }
```

It is possible to define a function in "lambda" notation and then bind that into a scope, in which case it works like any function:

```perl6
my &multiply := -> $a, $b { $a * $b };
```

Another way to write a lambda is with internal placeholder parameters:

```perl6
my &multiply := { $^a * $^b };
```

(And, in fact, our original <tt>@_</tt> above is just a variadic self-declaring placeholder argument.  And the famous Perl "topic", <tt>$_</tt>, is just a self-declared parameter to a unary block.)

You may also curry both built-in and user-defined operators by supplying a <tt>*</tt> (known as "whatever") in place of the argument that is <i>not</i> to be curried:

```perl6
my &multiply := * * *;
```

This is not terribly readable in this case due to the visual confusion between the whatever star and the multiplication operator, but Perl knows when it's expecting terms instead of infixes, so only the middle star is multiplication.
It tends to work out much better with other operators.  In particular, you may
curry a cascade of methods with only the original invocant missing:

```perl6
@list.grep( *.substr(0,1).lc.match(/<[0..9 a..f]>/) )
```

This is equivalent to:

```perl6
@list.grep( -> $obj { $obj.substr(0,1).lc.match(/<[0..9 a..f]>/) } )
```



## Phix


```Phix
function multiply(atom a, atom b)
    return a*b
end function
```



## PHL



```phl
@Integer multiply(@Integer a, @Integer b) [
	return a * b;
]
```



## PHP


```php
function multiply( $a, $b )
{
    return $a * $b;
}
```



## Picat


```php
multiply(A, B) = A*B.

```



## PicoLisp


```PicoLisp
(de multiply (A B)
   (* A B) )
```



## Pike


```pike
int multiply(int a, int b){
   return a * b;
}
```



## PL/I


```pli
PRODUCT: procedure (a, b) returns (float);
   declare (a, b) float;
   return (a*b);
end PRODUCT;
```



## PL/SQL


```plsql
FUNCTION multiply(p_arg1 NUMBER, p_arg2 NUMBER) RETURN NUMBER
IS
  v_product NUMBER;
BEGIN
  v_product := p_arg1 * p_arg2;
  RETURN v_product;
END;
```



## Pop11


```pop11
define multiply(a, b);
    a * b
enddefine;
```



## PostScript

Inbuilt:

```postscript>3 4 mul</lang

Function would be:

```postscript
/multiply{
    /x exch def
    /y exch def
    x y mul =
}def
```



## PowerShell

The most basic variant of function definition would be the kind which uses positional parameters and therefore doesn't need to declare much:

```powershell
function multiply {
    return $args[0] * $args[1]
}
```

Also, the return statement can be omitted in many cases in PowerShell, since every value that "drops" out of a function can be used as a "return value":

```powershell
function multiply {
    $args[0] * $args[1]
}
```

Furthermore, the function arguments can be stated and named explicitly:

```powershell
function multiply ($a, $b) {
    return $a * $b
}
```

There is also an alternative style for declaring parameters. The choice is mostly a matter of personal preference:

```powershell
function multiply {
    param ($a, $b)
    return $a * $b
}
```

And the arguments can have an explicit type:

```powershell
function multiply ([int] $a, [int] $b) {
    return $a * $b
}
```



## Processing

Processing is based on Java, and thus uses a familiar C-style syntax for function definition—as it does for much else. For the sake of argument, this implementation of <tt>multiply</tt> uses single-precision floats: other numeral types are available.

```processing
float multiply(float x, float y)
{
    return x * y;
}
```



## Prolog

Prolog, as a logic programming languages, does not have user-supplied functions available.  It has only predicates; statements which are "true" or "false".  In cases where values have to be "returned" a parameter is passed in that is unified with the result.  In the following predicate the parameter "P" (for "Product") is used in this role.  The following code will work in any normal Prolog environment (but not in things like Turbo Prolog or Visual Prolog or their ilk):

```Prolog
multiply(A, B, P) :- P is A * B.
```

This is what it looks like in use:

```Prolog
go :-
  multiply(5, 2, P),
  format("The product is ~d.~n", [P]).
```

This can be a little bit jarring for those used to languages with implicit return values, but it has its advantages.  For example unit testing of such a predicate doesn't require special frameworks to wrap the code:

```Prolog
test_multiply :-
  multiply(5, 2, 10),  % this will pass
  multiply(3, 4, 11).  % this will not pass
```

Still, the lack of user-defined functions remains an annoyance.

Prolog, however, is a remarkably malleable language and through its term re-writing capabilities the function-style approach could be emulated.  The following code relies on the [http://packs.ndrix.com/function_expansion/index.html function_expansion] pack (separately installed through the packs system) for SWI-Prolog.  Similar code could be made in any Prolog implementation, however.

```Prolog
:- use_module(library(function_expansion)).

user:function_expansion(multiply(A, B), P, P is A * B).  % "function" definition

go :-
  format("The product is ~d.~n", [multiply(5, 2)]).
```


While the function '''definition''' is perhaps a bit more involved, the function '''use''' is now pretty much the same as any other language people are used to.  The "magic" is accomplished by the compiler rewriting the <code>go/0</code> term into the following code:

```Prolog
go :-
  A is 5*2,
  format('The product is ~d.~n', [A]).
```



## PureBasic


```PureBasic
Procedure multiply(a,b)
  ProcedureReturn a*b
EndProcedure
```



## Python

Function definition:

```python
def multiply(a, b):
    return a * b
```

Lambda function definition:

```python
multiply = lambda a, b: a * b
```

A callable class definition allows functions and classes to use the same interface:

```python
class Multiply:
    def __init__(self):
        pass
    def __call__(self, a, b):
        return a * b

multiply = Multiply()
print multiply(2, 4)    # prints 8
```

(No extra functionality is shown in ''this'' class definition).


## Q


```q
multiply:{[a;b] a*b}
```

or

```q
multiply:{x*y}
```

or

```q
multiply:*
```

Using it

```q
multiply[2;3]
 6
```



## Quack

You have several ways to define a function in Quack. You can do it by the classic way:

```quack
fn multiply[ a; b ]
  ^ a * b
end
```


Using lambda-expressions:

```quack
let multiply :- fn { a; b | a * b }
```


And using partial anonymous functions:
```quack
let multiply :- &(*)
```



## R


```rsplus
mult <- function(a,b) a*b
```

In general:

```rsplus
mult <- function(a,b) {
  a*b
  # or:
  # return(a*b)
}
```



## Racket

A simple function definition that takes 2 arguments.


```racket
(define (multiply a b) (* a b))
```


Using an explicit <code>lambda</code> or <code>λ</code> is completely equivalent:

```racket
(define multiply (lambda (a b) (* a b)))
```



```racket
(define multiply (λ (a b) (* a b)))
```


Note that <code>*</code> is a function value, so the following code also works (although <code>multiply</code> will now be variadic function).


```racket
(define multiply *)
```



## Raven


```raven
define multiply use a, b
    a b *
```

Or optional infix:

```raven
define multiply use a, b
    (a * b)
```

Or skip named vars:

```raven
define multiply *
```



## REALbasic


```vb

Function Multiply(a As Integer, b As Integer) As Integer
  Return a * b
End Function

```



## REBOL

REBOL actually already has a function called 'multiply', which is a native compiled function. However, since it's not protected, I can easily override it:

```REBOL
multiply: func [a b][a * b]
```



## Retro


```Retro
: multiply ( nn-n ) * ;
```



## REXX


### exactitudeness


```rexx
multiply: return arg(1) * arg(2)    /*return the product of the two arguments.*/
```



### cleaner display

Because REXX will return the same precision as the multiplicands, we can do some beautification with the resultant product.


I.E.:             ''' 3.0 * 4.00 '''     yields the product:     '''12.000'''


This version eliminates the   '''.000'''   from the product.

```rexx
multiply: return arg(1) * arg(2) / 1    /*return with a normalized product of 2 args. */
```



## RLaB

In RLaB the functions can be built-in (compiled within RLaB, or part of the shared object library that is loaded per request of user), or user (written in RLaB script). Consider an example:

```RLaB>>
 class(sin)
function
>> type(sin)
builtin
```

Functions are a data class on their own, or they can be member of a list (associative array).

1. user function specified from built-in functions, here basic addition

```RLaB
f = function(x, y)
{
  return x + y;
};

>> class(f)
function
>> type(f)
user
```


2. function can be member of a list (associative array)

```RLaB>somelist = <<>
;
somelist.f = function(x, y)
{
  rval = x + y;
  return rval;
};
```


3. user function which uses a function that is specified as a member of some list, here we use ''somelist'' from above:

```RLaB
g = function(x, y)
{
  global(somelist);
  rval = x * somelist.f(x, 2*y);
  return rval;
};
```



## Ring


```ring

func multiply x,y return x*y

```



## Ruby


```ruby
def multiply(a, b)
    a * b
end
```



## Rust


```rust
fn multiply(a: i32, b: i32) -> i32 {
    a * b
}
```



## Sather


```sather
class MAIN is
  -- we cannot have "functions" (methods) outside classes
  mult(a, b:FLT):FLT is return a*b; end;

  main is
    #OUT + mult(5.2, 3.4) + "\n";
  end;
end;
```


=={{header|S-BASIC}}==
S-BASIC is unusual in that the function return value is assigned to the END statement that terminates the function.

```basic

function multiply(a, b = real) = real
end = a * b

```



## Scala


```scala
def multiply(a: Int, b: Int) = a * b
```



## Scheme


```scheme
(define multiply *)
```

Alternately,

```scheme
(define (multiply a b)
  (* a b))
```



## Seed7


```seed7
const func float: multiply (in float: a, in float: b) is
  return a * b;
```



## SETL


```setl
proc multiply( a, b );
    return a * b;
end proc;
```



## Sidef


```ruby
func multiply(a, b) {
    a * b;
}
```



## Simula

Simula uses the term <tt>procedure</tt> for subroutines/methods whether they return a value or not. A procedure that does return a value is declared with a data type (e.g. <tt>integer procedure</tt>), whereas one that does not is declared simply as <tt>procedure</tt>. This program defines <tt>multiply</tt> as an integer procedure and illustrates its use. Note that the second argument provided to <tt>Outint</tt> gives the width of the integer to be printed.

```simula
BEGIN
    INTEGER PROCEDURE multiply(x, y);
    INTEGER x, y;
    BEGIN
        multiply := x * y
    END;
    Outint(multiply(7,8), 2);
    Outimage
END
```



## Slate


```slate
define: #multiply -> [| :a :b | a * b].
```

or using a macro:

```slate
define: #multiply -> #* `er.
```

The block may also be installed as a method like so:

```slate
a@(Number traits) multiplyBy: b@(Number traits) [a * b].
```

or more explicitly (without sugar):

```slate
[| :a :b | a * b] asMethod: #multipleBy: on: {Number traits. Number traits}.
```



## Smalltalk


```smalltalk
|mul|
mul := [ :a :b | a * b ].
```



## SNOBOL4


```snobol4
          define('multiply(a,b)') :(mul_end)
multiply  multiply = a * b        :(return)
mul_end
* Test
          output = multiply(10.1,12.2)
          output = multiply(10,12)
end
```

{{out}}
    123.22
    120


## SNUSP

For expediency, the function is adding three values, instead of multiplying two values. Another function, atoi (+48) is called before printing the result.

```snusp>+1>++2=@\=
+++3=@\==@\=.=#  prints '6'
        |        |   \=itoa=@@@+@+++++#
        \
### =
!\==!/===?\<#
                     \>+<-/
```



## SPARK

The function definition (multiplies two standard Integer):

```Ada
package Functions is
   function Multiply (A, B : Integer) return Integer;
   --# pre A * B in Integer; -- See note below
   --# return A * B; -- Implies commutativity on Multiply arguments
end Functions;
```

Note: how do you ensure then “A * B in Integer” ? Either with a proof prior to Multiply invokation or using another form of Multiply where input A and B would be restricted to a range which ensures the resulting product is always valid. Exemple :

```Ada
type Input_Type is range 0 .. 10;
type Result_Type is range 0 .. 100;
```

and had a version of Multiply using these types. On the other hand, if arguments of Multiply are constants, this is provable straight away.

The Multiply's implementation:

```Ada
package body Functions is
   function Multiply (A, B : Integer) return Integer is
   begin
      return A * B;
   end Multiply;
end Functions;
```



## SPL

Single-line function definition:

```spl
multiply(a,b) <= a*b
```

Multi-line function definition:

```spl
multiply(a,b)=
  x = a*b
  <= x
.
```



## SSEM

The SSEM instruction set makes no explicit provision for subroutines, and indeed its storage space is too small for them to be of much use; but something like a subroutine can be created using a modified form of Wheeler jump. In this technique, the jump to the subroutine is accomplished with the return address loaded in the accumulator. The first action by the subroutine is to store this address in a place where it will be found by its own final jump instruction. In principle, therefore, the subroutine can be called multiple times from different points in the program without the calling routine needing to modify it at all (or even to know anything about it beyond where it begins, where it expects to find its parameters, and where it will store its result or results).

In this example, the main routine does nothing at all beyond calling the subroutine and halting after it has returned. The values <tt>A</tt> and <tt>B</tt> are passed in the two addresses located immediately before the subroutine begins; their product is returned in the address that formerly stored <tt>A</tt>. Given that the <tt>multiply</tt> subroutine begins at address 8, the calling routine looks like this:

```ssem
01000000000000100000000000000000   0. -2 to c
00100000000000000000000000000000   1. 4 to CI
01111111111111111111111111111111   2. -2
00000000000001110000000000000000   3. Stop
11100000000000000000000000000000   4. 7
```

or in pseudocode:

```txt
          load       &here
          jump       multiply
here:     halt
```

Implementing <tt>multiply</tt> on the SSEM requires the use of repeated negation and subtraction. For the sake of example, the values 8 and 7 are provided for <tt>A</tt> and <tt>B</tt>.

```ssem
00010000000000000000000000000000   6. 8
11100000000000000000000000000000   7. 7
11111000000001100000000000000000   8. c to 31
01100000000000100000000000000000   9. -6 to c
01111000000001100000000000000000  10. c to 30
01111000000000100000000000000000  11. -30 to c
01111000000001100000000000000000  12. c to 30
11100000000000100000000000000000  13. -7 to c
11100000000001100000000000000000  14. c to 7
11100000000000100000000000000000  15. -7 to c
00111000000000010000000000000000  16. Sub. 28
11100000000001100000000000000000  17. c to 7
00111000000000010000000000000000  18. Sub. 28
00000000000000110000000000000000  19. Test
00111000000001000000000000000000  20. Add 28 to CI
11111000000000000000000000000000  21. 31 to CI
01100000000000100000000000000000  22. -6 to c
01111000000000010000000000000000  23. Sub. 30
01100000000001100000000000000000  24. c to 6
01100000000000100000000000000000  25. -6 to c
01100000000001100000000000000000  26. c to 6
10111000000000000000000000000000  27. 29 to CI
10000000000000000000000000000000  28. 1
00110000000000000000000000000000  29. 12
00000000000000000000000000000000  30. 0
00000000000000000000000000000000  31. 0
```

The pseudocode equivalent clarifies how the subroutine works, or how it would work on an architecture that supported <tt>load</tt> and <tt>add</tt>:

```txt
a:        equals     #8
b:        equals     #7
multiply: store      ret
          load       a
          store      n
loop:     load       b
          sub        #1
          store      b
          sub        #1
          ifNegative done
          load       a
          add        n
          store      a
          jump       loop
done:     jump       *ret
n:        reserve    1 word
ret:      reserve    1 word
```



## Standard ML


```ocaml
val multiply = op *
```

Equivalently,

```ocaml
fun multiply (x, y) = x * y
```

Curried form:

```ocaml
fun multiply x y = x * y
```



## Stata



###  Ado

Stata's macro language does not have functions, but commands. Output is usually saved as a "stored result" (but could also be saved in a global macro variable, in a scalar or matrix, in a dataset or simply printed to the Results window). See '''[https://www.stata.com/help.cgi?program program]''' and '''[https://www.stata.com/help.cgi?return]''' in Stata documentation.


```stata
prog def multiply, return
	args a b
	return sca product=`a'*`b'
end

multiply 77 13
di r(product)
```


'''Output'''


```txt
1001
```



###  Mata

Mata is the matrix language of Stata. Here is how to define a function


```stata
mata
scalar multiply(scalar x, scalar y) {
	return(x*y)
}

multiply(77,13)
end
```


'''Output'''


```txt
1001
```



## Swift


```swift
func multiply(a: Double, b: Double) -> Double {
   return a * b
}
```



## Tcl

Strictly as described in the task:

```tcl
proc multiply { arg1 arg2 } {
    return [expr {$arg1 * $arg2}]
}
```

{{works with|Tcl|8.5}}
You can also create functions that work directly inside expressions. This is done by creating the command with the correct name (that is, in the ''tcl::mathfunc'' namespace):

```tcl
proc tcl::mathfunc::multiply {arg1 arg2} {
    return [expr {$arg1 * $arg2}]
}

# Demonstrating...
if {multiply(6, 9) == 42} {
    puts "Welcome, Citizens of Golgafrincham from the B-Ark!"
}
```


=={{header|TI-89 BASIC}}==

```ti89b
multiply(a, b)
Func
  Return a * b
EndFunc
```



## Toka


```toka
[ ( ab-c ) * ] is multiply
```



## TXR

In TXR, there are pattern functions which are predicates that perform pattern matching and variable capture. A call to this type of  function call can specify unbound variables. If the function succeeds, it can establish bindings for those variables.

Here is how to make a pattern function that multiplies, and call it. To multiply the numbers, we break out of the pattern language and invoke Lisp evaluation: <code>@(* a b)</code>

```txr
@(define multiply (a b out))
@(bind out @(* a b))
@(end)
@(multiply 3 4 result)
```


```txt
$ txr -B multiply.txr
result="12"
```

In the embedded Lisp dialect, it is possible to write an ordinary function that returns a value:

```txrlisp
(defun mult (a b) (* a b))
  (put-line `3 * 4 = @(mult 3 4)`)
```


```txt
$ txr multiply.tl
3 * 4 = 12
```



## uBasic/4tH

In uBasic you can turn any subroutine into a function with the '''FUNC()''' function. It takes one argument, which is the label. Arguments are optional.
<lang>PRINT FUNC (_Multiply (2,3))
END

_Multiply PARAM (2)
RETURN (a@ * b@)
```




## UNIX Shell

Note that in the Unix shell, function definitions do not include any argument specifications within the parentheses. Instead arguments to functions are obtained using the positional parameters.
{{works with|Bourne Shell}}

```bash
multiply() {
  # There is never anything between the parentheses after the function name
  # Arguments are obtained using the positional parameters $1, and $2
  # The return is given as a parameter to the return command
  return `expr "$1" \* "$2"`    # The backslash is required to suppress interpolation
}

# Call the function
multiply 3 4    # The function is invoked in statement context
echo $?        # The dollarhook special variable gives the return value
```

{{works with|Bash}}
return an exit code

```bash
multiply() {
  return $(($1 * $2))
}

multiply 5 6
echo $?
```

echo the result

```bash
multiply() {
  echo -n $(($1 * $2))
}

echo $(multiply 5 6)
```



## Ursa


```ursa
# multiply is a built-in in ursa, so the function is called mult instead
def mult (int a, int b)
	return (* a b)
end
```



## Ursala

Functions are declared with an equals sign like constants of any other type.
They may be specified by lambda abstraction, with dummy variables in double quotes, or in point-free form, or any combination. The way multiplication is defined depends on the type of numbers being multiplied. For this example, numbers in standard IEEE double precision are assumed, and the multiply function is defined in terms of the system library function, called using the syntax <code>math..mul</code>.
This is the definition in point free form,

```Ursala>multiply = math..mul</lang

this is the definition using lambda abstraction

```Ursala
multiply = ("a","b"). math..mul ("a","b")
```

and this is the definition using pattern matching.

```Ursala
multiply("a","b") = math..mul ("a","b")
```



## V

V uses stack for input arguments and '.' is a word that takes a quote and binds the first word to the sequence of actions supplied in the quote.

```v
[multiply *].
```

Using it

```v
2 3 multiply
=6
```

V also allows internal bindings.

```v
[multiply
  [a b] let
  a b *].
```



## VBA


```vb
Function Multiply(lngMcand As Long, lngMplier As Long) As Long
    Multiply = lngMcand * lngMplier
End Function
```

To use this function :

```vb
Sub Main()
Dim Result As Long
    Result = Multiply(564231, 897)
End Sub
```



## VBScript


```vb
function multiply( multiplicand, multiplier )
    multiply = multiplicand * multiplier
end function
```

Usage:

```vb
dim twosquared
twosquared = multiply(2, 2)
```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb

Function multiply(a As Integer, b As Integer) As Integer
    multiply = a * b
End Function

```

Call the function

```vb
Multiply(6, 111)
```



## Visual Basic .NET


```vbnet
Function Multiply(ByVal a As Integer, ByVal b As Integer) As Integer
    Return a * b
End Function
```

Call the function

```vbnet
Multiply(1, 1)
```



## Wart

A straightforward way to say how calls of the form <code>(multiply a b)</code> are translated:

```python
def (multiply a b)
  a*b
```



```python
(multiply 3 4)
=> 12
```


Functions can also use keyword args.


```python
(multiply 3 :a 4)  # arg order doesn't matter here, but try subtract instead
=> 12
```


Finally, we can give parameters better keyword args using <em>aliases</em>:


```python
def (multiply a b|by)
  (* a b)
```



```python
multiply 3 :by 4
=> 12
```



## X86 Assembly

X86 Assembly doesn't really have functions. Instead, it has labels that are called. Function arguments can be pushed onto the stack prior to calling or passed to the function in registers.  The system will usually have some sort of calling conventions to facilitate inter-operation between languages.


### Unix

Function definition and calling conventions on a Unix-like system are specified in the book "System V Application Binary Interface: Intel 386 Architecture Processor Supplement" ([https://web.archive.org/web/20000818171113/http://www.sco.com/developer/devspecs/abi386-4.pdf from SCO at archive.org]).  These are the conventions used by the C language and also most other languages.

The stack, for two 32-bit integer parameters, is
* <code>[esp+8]</code> second parameter
* <code>[esp+4]</code> first parameter
* <code>[esp]</code> return address
The return value is left in the <code>eax</code> register.  <code>ecx</code> and <code>edx</code> are "scratch" registers meaning the called routine doesn't need to preserve their values.  (In the code below edx is clobbered.)

The following is Unix-style "as" assembler syntax (including GNU as).  The resulting function can be called from C with <code>multiply(123,456)</code>.


```asm
        .text
        .globl  multiply
        .type   multiply,@function
multiply:
        movl    4(%esp), %eax
        mull    8(%esp)
        ret
```


The <code>.type</code> directive is important for code which will go into a shared library.  You can get away without it for a static link.  It ensures the linker knows to dispatch calls from the mainline to the function via a PLT entry.  (If omitted the code is copied at runtime into some mainline space.  Without a <code>.size</code> directive only 4 bytes will be copied.)


### NASM

{{works with|NASM}}

```asm
section .text
global _start

_multiply_regs:
  mul ebx
  mov eax, ebx
  ret

_multiply_stack:
  enter 2,0
  mov eax, [esp+4]
  mov ebx, [esp+8]
  mul ebx
  mov eax, ebx
  leave
  ret

_start:
  mov ax, 6  ;The number to multiply by
  mov ebx, 16 ;base number to multiply.
  call _multiply_regs
  push 6
  push 16
  call _multiply_stack
```



### MASM

However, in MASM we do have function statements due to the preprocessor.
{{works with|MASM}}

```asm
multiply proc arg1:dword, arg2:dword
  mov eax, arg1
  mov ebx, arg2
  mul ebx
  mov eax, ebx
  ret
multiply endp
```

Then to call it.

```asm
invoke multiply, 6, 16
;or..
push 16
push 6
call multiply
```

Return values are usually put into the register EAX. This, of course is not a must it's simply that it's somewhat of a unofficial standard. For example, C/C++ preprocessors/compilers will translate "return value" into "mov eax, value" followed by the return to caller instruction "ret".


## XLISP

Functions can be defined using either 'classic' Lisp syntax:

```lisp
(defun multiply (x y)
    (* x y))
```

or Scheme-style syntax:

```scheme
(define (multiply x y)
    (* x y))
```

or, if you prefer, with <tt>LAMBDA</tt>:

```scheme
(define multiply
    (lambda (x y) (* x y)))
```



## Xojo


```vbnet
Function Multiply(ByVal a As Integer, ByVal b As Integer) As Integer
    Return a * b
End Function
```

Call the function

```vbnet
Dim I As Integer = Multiply(7, 6)
```



## XPL0


```XPL0
func Multiply(A, B);    \the characters in parentheses are only a comment
int  A, B;              \the arguments are actually declared here, as integers
return A*B;            \the default (undeclared) function type is integer
                        \no need to enclose a single statement in brackets

func real FloatMul(A, B); \floating point version
real A, B;              \arguments are declared here as floating point (doubles)
return A*B;
```



## XSLT

Templates are the closest things XSLT has to user defined functions. They can be declared to be called by name and/or to be applied to all nodes in a matching set and given "mode". Both types of template can take named parameters with default values. Templates also have a "context" node used as the base of XPath expressions (kind of like an implied "this" of an object's method).

```xslt
<xsl:template name="product">
  <xsl:param name="a" select="2"/>
  <xsl:param name="b" select="3"/>
  <fo:block>product = <xsl:value-of select="$a * $b"/></fo:block>
</xsl:template>

<xsl:call-template name="product">
  <xsl:with-param name="a">4</xsl:with-param>
  <xsl:with-param name="b">5</xsl:with-param>
</xsl:call-template>
```


<xsl:call-template name="product"/>    &lt;-- using default parameters of 2 and 3 -->


## Yorick


```yorick
func multiply(x, y) {
    return x * y;
}
```

Example of interactive usage:

```txt
> multiply(2, 4.5)
9
```



## zkl


```zkl
fcn multiply(x,y){x*y}
```


```zkl
fcn(x,y){x*y}(4.5,3) // --> 13.5
```

Since all functions are vararg:
```zkl
fcn multiply{vm.arglist.reduce('*)}
multiply(1,2,3,4,5) //--> 120
```

Operators are first class objects so:
```zkl
var mul=Op("*"); mul(4,5) //-->20
```



## ZX Spectrum Basic

On the ZX Spectrum, function names are limited to one letter. Note that the function becomes effective as soon as it is entered into the program, and does not need to be run

```zxbasic
10 PRINT FN m(3,4): REM call our function to produce a value of 12
20 STOP
9950 DEF FN m(a,b)=a*b
```


{{omit from|GUISS}}
{{omit from|TI-83 BASIC|Cannot define functions.}}
