+++
title = "Call a function"
description = ""
date = 2019-10-17T14:52:05Z
aliases = []
[extra]
id = 10098
[taxonomies]
categories = ["Functions and subroutines", "Simple", "task"]
tags = []
+++

## Task

;Task:
Demonstrate the different syntax and semantics provided for calling a function.


This may include:
:*   Calling a function that requires no arguments
:*   Calling a function with a fixed number of arguments
:*   Calling a function with [[Optional parameters|optional arguments]]
:*   Calling a function with a [[Variadic function|variable number of arguments]]
:*   Calling a function with [[Named parameters|named arguments]]
:*   Using a function in statement context
:*   Using a function in [[First-class functions|first-class context]] within an expression
:*   Obtaining the return value of a function
:*   Distinguishing built-in functions and user-defined functions
:*   Distinguishing subroutines and functions
;*   Stating whether arguments are [[:Category:Parameter passing|passed]] by value or by reference
;*   Is partial application possible and how



This task is ''not'' about [[Function definition|defining functions]].

<bR>


## 360 Assembly

Due to assembler, argument are passed by reference.

With:

```360asm
X        DS     F
Y        DS     F
Z        DS     F
```

If you do not want to use the CALL macro instruction and for a link-edited object-module:

```360asm
         L      R15,=V(MULTPLIC)
         LA     R1,PARMLIST        address of the paramter list
         BALR   R14,R15            branch and link
         ST     R0,Z               Z=MULTPLIC(X,Y)
* ...
PARMLIST DC     A(X)
         DC     A(Y)
```

If you call a link-edited object-module:

```360asm
         CALL   MULTPLIC,(X,Y)     call MULTPLIC(X,Y)
         ST     R0,Z               Z=MULTPLIC(X,Y)
```

If you call an load-module at execution time:

```360asm
         LOAD   EP=MULTPLIC        load load-module
         LR     R15,R0             retrieve entry address
         CALL   (R15),(X,Y)        call MULTPLIC(X,Y)
         ST     R0,Z               Z=MULTPLIC(X,Y)
```



## ActionScript



```actionscript
  myfunction();       /* function with no arguments in statement context */
  myfunction(6,b);    // function with two arguments in statement context
  stringit("apples");    //function with a string argument
```



## Ada


* Ada provides two kinds of subroutines: procedures without return values and functions with return values. The return values of functions must be used by the callers. If you don't want to deal with the return value, call a procedure instead.

* As a rule of thumb, an Ada compiler is free to pass arguments either by value or by reference. Parameters have a mode however: either 'in' or 'out' or 'in out'. It is prohibited to write anything to an 'in' parameter. The next language Standard, Ada 2012, will support functions with 'out' and 'in out' mode parameters, so far, only procedures could have parameters with non-'in' modes. So any of the following statements for Ada functions holds for Ada procedures as well.

* There are no differences between calling built-in vs. user defined functions.

* Functions without parameters can be called by omitting the parameter list (no empty brackets!):
```Ada
S: String := Ada.Text_IO.Get_Line;
```


* Ada supports functions with optional parameters:
```Ada
function F(X: Integer; Y: Integer := 0) return Integer; -- Y is optional
...
A : Integer := F(12);
B : Integer := F(12, 0); -- the same as A
C : Integer := F(12, 1); -- something different
```


* If the number of parameters of F were fixed to two (by omitting the ":= 0" in the specification), then B and C would be OK, but A wouldn't.

* Ada does not support functions with a variable number of arguments. But a function argument can be an unconstrained array with as many values as you want:
```Ada
type Integer_Array is array (Positive range <>) of Integer;
function Sum(A: Integer_Array) return Integer is
   S: Integer := 0;
begin
   for I in A'Range loop
      S := S + A(I);
   end loop;
   return S;
end Sum;
...
A := Sum((1,2,3));     -- A = 6
B := Sum((1,2,3,4));   -- B = 10
```


* One can realize first-class functions by defining an access to a function as a parameter:
```Ada
function H (Int: Integer;
            Fun: not null access function (X: Integer; Y: Integer)
              return Integer);
           return Integer;

...

X := H(A, F'Access) -- assuming X and A are Integers, and F is a function
                     -- taking two Integers and returning an Integer.
```


* The caller is free to use either positional parameters or named parameters, or a mixture of both (with positional parameters first)
```Ada
Positional := H(A, F'Access);
Named      := H(Int => A, Fun => F'Access);
Mixed      := H(A, Fun=>F'Access);
```



## ALGOL 68


```algol68
# Note functions and subroutines are called procedures (or PROCs) in Algol 68 #
# A function called without arguments: #
f;
# Algol 68 does not expect an empty parameter list for calls with no arguments, "f()" is a syntax error #
# A function with a fixed number of arguments: #
f(1, x);

# variable number of arguments: #
# functions that accept an array as a parameter can effectively provide variable numbers of arguments #
# a "literal array" (called a row-display in Algol 68) can be passed, as is often the case for the I/O #
# functions - e.g.: #
print( ( "the result is: ", r, " after ", n, " iterations", newline ) );
# the outer brackets indicate the parameters of print, the inner brackets indicates the contents are a "literal array" #

# ALGOL 68 does not support optional arguments, though in some cases an empty array could be passed to a function #
# expecting an array, e.g.: #
f( () );

# named arguments - see the Algol 68 sample in: http://rosettacode.org/wiki/Named_parameters #

# In "Talk:Call a function" a statement context is explained as
"The function is used as an instruction (with a void context),
rather than used within an expression."
Based on that, the examples above are already in a statement context.
Technically, when a function that returns other than VOID (i.e. is not a subroutine)
is called in a statement context, the result of the call is "voided" i.e. discarded.
If desired, this can be made explicit using a cast, e.g.: #
VOID(f);

# A function's return value being used: #
x := f(y);

# There is no distinction between built-in functions and user-defined functions. #

# A subroutine is simply a function that returns VOID. #

# If the function is declared with argument(s) of mode REF MODE,
then those arguments are being passed by reference. #
# Technically, all parameters are passed by value, however the value of a REF MODE is a reference... #
```


See [http://rosettacode.org/wiki/First-class_functions#ALGOL_68 First-Class Functions] for an example of first-class functions in ALGOL 68.

See [http://rosettacode.org/wiki/Partial_function_application#ALGOL_68 Partial Function Application] for an example of partial function application in ALGOL 68.

See [http://rosettacode.org/wiki/Optional_parameters#ALGOL_68 Optional Parameters] for an example of optional parameters in Algol 68.

See [http://rosettacode.org/wiki/Named_parameters#ALGOL_68 Named Parameters] for an example of named parameters in Algol 68.


## ALGOL W


```algolw
% Note, in Algol W, functions are called procedures %
% calling a function with no parameters: %
f;

% calling a function with a fixed number of parameters %
g( 1, 2.3, "4" );

% Algol W does not support optional parameters in general, however constructors for records can %
% be called wither with parameters (one for each field in the record) or no parameters #

% Algol W does not support variable numbers of parameters, except for the built-in I/O functions #
% Algol W does not support named arguments %

% A function can be used in a statement context by calling it, as in the examples above %

% First class context: A function can be passed as a parameter to another procedure, e.g.: %
v := integrate( sin, 0, 1 )
% assuming a suitable definition of integrate %
% Algol W does not support functions returning functions %

% obtaining the return value of a function: e.g.: %
v := g( x, y, z );

% There is no syntactic distinction between user-defined and built-in functions %

% Subroutines and functions are both procedures, a subroutine is a procedure with no return type %
% (called a proper procedure in Algol W) %
% There is no syntactic distinction between a call to a function and a call to a subroutine %
% other than the context %

% In Algol W, parameters are passed by value, result or value result. This must be stated in the %
% definition of the function/subroutine. Value parameters are passed by value, result and value result %
% are effectively passed by reference and assigned on function exit. Result parameters are "out" parameters %
% and value result parameters are "in out". %
% Algol W also has "name" parameters (not to be confused with named parameters). Functions with name %
% parameters are somewhat like macros %

% Partial application is not possible in Algol W %
```



## AntLang

AntLang provides two ways to apply a function.
One way is infix application.

```AntLang
2*2+9
```

Infix application is right associative, so x f y g z means x f (y g z) and not (x f y) g z.
You can break this rule using parenthesis.
The other way is prefix application.

```AntLang
*[2;+[2;9]]
echo["Hello!"]
time[]
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program callfonct.s   */

/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1

/***********************/
/* Initialized data */
/***********************/
.data
szMessage:      .asciz "Hello. \n"       @ message
szRetourLigne: .asciz "\n"
szMessResult:  .ascii "Resultat : "      @ message result
sMessValeur:   .fill 12, 1, ' '
                   .asciz "\n"
/***********************/
/* No Initialized data */
/***********************/
.bss
iValeur:  .skip  4     @ reserve 4 bytes in memory

.text
.global main
main:
    ldr r0,=szMessage          @ adresse of message  short program
    bl affichageMess            @ call function with 1 parameter (r0)

    @ call function with parameters in register
    mov r0,#5
    mov r1,#10
    bl fonction1            @ call function with 2 parameters (r0,r1)
    ldr r1,=sMessValeur                           @ result in r0
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,=szMessResult
    bl affichageMess            @ call function with 1 parameter (r0)

    @ call function with parameters on stack
    mov r0,#5
    mov r1,#10
    push {r0,r1}
    bl fonction2            @ call function with 2 parameters on the stack
                              @ result in r0
    ldr r1,=sMessValeur
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,=szMessResult
    bl affichageMess            @ call function with 1 parameter (r0)


 /* end of  program */
    mov r0, #0                  @ return code
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call

/******************************************************************/
/*     call function parameter in register             */
/******************************************************************/
/* r0 value one */
/* r1 value two */
/* return in r0 */
fonction1:
    push {fp,lr}    /* save des  2 registres */
    push {r1,r2}    /* save des autres registres */
    mov r2,#20
    mul r0,r2
    add r0,r0,r1
    pop {r1,r2}     /* restaur des autres registres */
    pop {fp,lr}    /* restaur des  2 registres */
    bx lr           /* retour procedure */

/******************************************************************/
/*     call function parameter in the stack             */
/******************************************************************/
/* return in r0 */
fonction2:
    push {fp,lr}    /* save des  2 registres */
    add fp,sp,#8    /* address parameters in the stack*/
    push {r1,r2}    /* save des autres registres */
    ldr r0,[fp]
    ldr r1,[fp,#4]
    mov r2,#-20
    mul r0,r2
    add r0,r0,r1
    pop {r1,r2}     /* restaur des autres registres */
    pop {fp,lr}    /* restaur des  2 registres */
    add sp,#8      /* very important, for stack aligned */
    bx lr          /* retour procedure */

/******************************************************************/
/*     affichage des messages   avec calcul longueur              */
/******************************************************************/
/* r0 contient l adresse du message */
affichageMess:
    push {fp,lr}    /* save des  2 registres */
    push {r0,r1,r2,r7}    /* save des autres registres */
    mov r2,#0   /* compteur longueur */
1:       /*calcul de la longueur */
    ldrb r1,[r0,r2]  /* recup octet position debut + indice */
    cmp r1,#0       /* si 0 c est fini */
    beq 1f
    add r2,r2,#1   /* sinon on ajoute 1 */
    b 1b
1:  /* donc ici r2 contient la longueur du message */
    mov r1,r0        /* adresse du message en r1 */
    mov r0,#STDOUT      /* code pour écrire sur la sortie standard Linux */
    mov r7, #WRITE                  /* code de l appel systeme 'write' */
    swi #0                      /* appel systeme */
    pop {r0,r1,r2,r7}     /* restaur des autres registres */
    pop {fp,lr}    /* restaur des  2 registres */
    bx lr	        /* retour procedure */
/***************************************************/
/*   conversion registre en décimal   signé  */
/***************************************************/
/* r0 contient le registre   */
/* r1 contient l adresse de la zone de conversion */
conversion10S:
    push {fp,lr}    /* save des  2 registres frame et retour */
    push {r0-r5}   /* save autres registres  */
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
    pop {r0-r5}   /*restaur des autres registres */
    pop {fp,lr}   /* restaur des  2 registres frame et retour  */
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
printHello {
	print "Hello World!"
}

sayHello [to]{
	print "Hello " + to + "!"
}

printAll [args]{
	loop args {
		print &
	}
}

getNumber { 3 }

// Calling a function that requires no arguments
printHello

// Calling a function with a fixed number of arguments
sayHello "John"

// Calling a function with a variable number of arguments
printAll #("one" "two" "three")

// Using a function in statement context
if true { printHello }

// Using a function in first-class context within an expression
if $(getNumber)=3 { "yep, it worked" }

// Obtaining the return value of a function:
num $(getNumber)
```


{{out}}


```txt
Hello World!
Hello John!
one
two
three
Hello World!
yep, it worked
```



## AutoHotkey


```AHK
; Call a function without arguments:
f()

; Call a function with a fixed number of arguments:
f("string", var, 15.5)

; Call a function with optional arguments:
f("string", var, 15.5)

; Call a function with a variable number of arguments:
f("string", var, 15.5)

; Call a function with named arguments:
    ; AutoHotkey does not have named arguments. However, in v1.1+,
    ; we can pass an object to the function:
f({named: "string", otherName: var, thirdName: 15.5})

; Use a function in statement context:
f(1), f(2) ; What is statement context?

; No first-class functions in AHK

; Obtaining the return value of a function:
varThatGetsReturnValue := f(1, "a")

; Cannot distinguish built-in functions

; Subroutines are called with GoSub; functions are called as above.
; Subroutines cannot be passed variables

; Stating whether arguments are passed by value or by reference:
; [v1.1.01+]: The IsByRef() function can be used to determine
;     whether the caller supplied a variable for a given ByRef parameter.
; A variable cannot be passed by value to a byRef parameter. Instead, do this:
f(tmp := varIdoNotWantChanged)
; the function f will receive the value of varIdoNotWantChanged, but any
; modifications will be made to the variable tmp.

; Partial application is impossible.

```



## AWK


The awk interpreter reads the entire script prior to processing, so functions can be called from sections of code appearing before the definition.


```awk
BEGIN {
  sayhello()       # Call a function with no parameters in statement context
  b=squareit(3)    # Obtain the return value from a function with a single parameter in first class context
}
```


In awk, scalar values are passed by value, but arrays are passed by reference. Note that if a function has no arguments, then empty parentheses are required.

The awk extraction and reporting language does not support the use of named parameters.


## Axe

In Axe, up to six arguments are passed as the variables r₁ through r₆. As with all variables in Axe, these exist in the global scope, which makes nested function calls and recursion quite difficult.

```axe
NOARG()
ARGS(1,5,42)
```


Since arguments are simply global variables, they are always optional and can be omitted from right to left.

```axe
OPARG(1,2,3,4,5,6)
OPARG(1,2,3)
OPARG()
```


Somewhat similar to [[TI-83 BASIC]], the last evaluated expression becomes the return value of the function. However, this is distinct from the Ans variable. Return values can be captured just like any other expression.

```axe
MATHS(2,4)→A
Disp GETSTR()
```


User-defined functions can be distinguished from language-defined functions by the fact that language-defined function names are composed of atomic tokens (usually with built-in parentheses) whereas user-defined function names are composed of individual characters. Also, because only uppercase letters are available by default in the OS, most user-defined names are all uppercase while language-defined names are mixed case.

```axe
USER()
axeFunc()
```



## Batch File


Batch files do not have a traditional "function" system like OOP languages, however this is the closest thing to it. The only difference between a block of code and a function is the way method you choose to invoke it. It's also worth noting that all batch files can be called from any other batch file, performing a function. A function should be put somewhere in the code where it will not be parsed unless the script is redirected there.


```dos

:: http://rosettacode.org/wiki/Call_a_function
:: Demonstrate the different syntax and semantics provided for calling a function.

@echo off

echo Calling myFunction1
call:myFunction1
echo.

echo Calling myFunction2 11 8
call:myFunction2 11 8
echo.

echo Calling myFunction3 /fi and saving the output into %%filecount%%
call:myFunction3 /fi
echo.%filecount%
echo.

echo Calling myFunction4 1 2 3 4 5
call:myFunction4 1 2 3 4 5
echo.

echo Calling myFunction5 "filename=test.file" "filepath=C:\Test Directory\"
call:myFunction5 "filename=test.file" "filepath=C:\Test Directory\"
echo.
echo Calling myFunction5 "filepath=C:\Test Directory\" "filename=test.file"
call:myFunction5 "filepath=C:\Test Directory\" "filename=test.file"
echo.

pause>nul
exit

:: Requires no arguments
:myFunction1
	echo myFunction1 has been called.
	goto:eof

:: Fixed number of arguments (%a% & %b%)
:myFunction2
	:: Returns %a% + %b%
	setlocal
	set /a c=%~1+%~2
	endlocal & echo %c%
	goto:eof

:: Optional arguments
:myFunction3
	:: Returns the amount of folders + files in the current directory
	:: /fi Returns only file count
	:: /fo Returns only folder count

	setlocal
	set count=0

	if "%~1"=="" set "command=dir /b"
	if "%~1"=="/fi" set "command=dir /b /A-d"
	if "%~1"=="/fo" set "command=dir /b /Ad"

	for /f "usebackq" %%i in (`%command%`) do set /a count+=1

	endlocal & set filecount=%count%
	goto:eof

:: Variable number of arguments
:myFunction4
	:: Returns sum of arguments
	setlocal
	:myFunction4loop
	set sum=0
	for %%i in (%*) do set /a sum+=%%i
	endlocal & echo %sum%
	goto:eof

:: Named Arguments (filepath=[path] & filename=[name])
:myFunction5
	:: Returns the complete path based off the 2 arguments
	if "%~1"=="" then goto:eof
	setlocal enabledelayedexpansion
	set "param=%~1"

	for /l %%i in (1,1,2) do (
		for /f "tokens=1,2 delims==" %%j in ("!param!") do set %%j=%%k
		set "param=%~2"
	)
	endlocal & echo.%filepath%%filename%
	goto:eof

```

Output:

```txt

Calling myFunction1
myFunction1 has been called.

Calling myFunction2 11 8
19

Calling myFunction3 /fi and saving the output into %filecount%
1

Calling myFunction4 1 2 3 4 5
15

Calling myFunction5 "filename=test.file" "filepath=C:\Test Directory\"
C:\Test Directory\test.file

Calling myFunction5 "filepath=C:\Test Directory\" "filename=test.file"
C:\Test Directory\test.file

```



## BBC BASIC

BBC BASIC distinguishes between functions (which return one value), procedures (which may return an arbitrary number of values including zero), and subroutines. Functions can be built-in or user-defined.
A call to a <b>built-in function</b> (for example, the square root function) is an expression:

```bbcbasic
PRINT SQR(2)
```

The parentheses can often be omitted:

```bbcbasic>PRINT SQR 2</lang

The name of a <b>user-defined function</b> must begin with <tt>FN</tt>. A call to it is also an expression:

```bbcbasic
PRINT FN_foo(bar$, baz%)
```

(The sigils <tt>$</tt> and <tt>%</tt> identify the variables' types.)
A function that takes no arguments can be called omitting the parentheses:

```bbcbasic
PRINT FN_foo
```

The name of a <b>procedure</b> must begin with <tt>PROC</tt>. A call to it is a statement, not an expression:

```bbcbasic
PROC_foo
```

If it has arguments, they come in parentheses just as with a function:

```bbcbasic
PROC_foo(bar$, baz%, quux)
```

Note that you <i>cannot tell from this syntax</i> which of the variables <tt>bar$</tt>, <tt>baz%</tt>, and <tt>quux</tt> are arguments provided to the procedure and which of them are return values from it. You have to look at where it is defined:

```bbcbasic
DEF PROC_foo(a$, RETURN b%, RETURN c)
```

<b>Subroutines</b> are provided for compatibility with older, unstructured dialects of BASIC; otherwise they are never really used. They require statements to be numbered, and they can neither receive arguments nor return values: they can only manipulate global variables. The <tt>GOSUB</tt> and <tt>RETURN</tt> statements in fact mirror assembly language 'jump to subroutine' and 'return from subroutine' instructions quite closely.

```bbcbasic>200 GOSUB 30050</lang



## Bracmat


* Calling a function that requires no arguments:

Strictly speaking, all Bracmat functions receive at least one argument. But empty strings are valid expressions, so you can do


```bracmat
aFunctionWithoutArguments$
```

or

```bracmat
aFunctionWithoutArguments'
```


Both function calls pass the right and side of the <code>$</code> or <code>'</code> operator. This is in fact still something: an empty string.

The <code>$</code> operator always evaluates its right hand side before passing it to the function, while the <code>'</code> does not. Therefore it is slightly faster to use the <code>functionName'</code> variant.

* Calling a function with a fixed number of arguments:

Bracmat passes exactly one argument to a function, called <code>arg</code>. The argument can be any Bracmat expression. In patterns, if a function call expression is a pattern component, a second argument <code>sjt</code> is added, the part of the subject that the pattern component is going trying to match.

* Calling a function with optional arguments

There is no special syntax for that. It is up to the programmer to define a datastructure with a variable part, e.g., a list.

* Calling a function with a variable number of arguments

Same answer.

* Calling a function with named arguments

There is no special syntax for that. You could pass a list of (name.value) pairs.

* Using a function in statement context

A f...

You can do

```bracmat
func$!myargument;
```

The <code>;</code> marks the end of a Bracmat statement.

* Using a function in first-class context within an expression

(Copied from JavaScript:) Bracmat functions are first-class citizens; they can be stored in variables and passed as arguments. Assigning to a variable <code>yourfunc</code> can be done in a few ways. The most common one is


```bracmat
(yourfunc=local vars.function body)
```

If there is already a function <code>myfunc</code> that you want to assign to <code>yourfunc</code> as well, do

```bracmat
('$myfunc:(=?yourfunc))
```


* Obtaining the return value of a function


```bracmat
myfunc$!myarg:?myresult
```


Notice that the returned value can be any evaluated expression.

* Distinguishing built-in functions and user-defined functions

You cannot list built-in functions that are implemented directly in C. Nor can such functions be passed as arguments or assigned to variables. There are also a number of built-in functions that are written in Bracmat. They are nothing special and can be deleted or redefined. You can see a list of all currently defined functions that are written in Bracmat with the function call <code>cat'</code>.

* Distinguishing subroutines and functions

You can ignore the return value of a function <code>myfunc</code> as follows:


```bracmat
myfunc$!myarg&yourfunc$!yourarg
```


But notice that if <code>myfunc</code> fails, the above expression returns the value produced by <code>myfunc</code>! To also ignore the success/failure of a function, do


```bracmat
`(myfunc$!myarg)&yourfunc$!yourarg
```


* Stating whether arguments are passed by value or by reference

Values are passed by reference, or by value if the reference counter, which is a very small integer, overflows. Most values are immutable, so for those there is no practical difference between passing by reference or value. The single exception of a mutable value is always passed by reference, and has an enormous reference counter. (The binary operator <code>=</code> introduces a mutable value and can be used for an object oriented style of programming.)

* Is partial application possible and how

There is no special syntax for that, but you can write a function that e.g., can take a list with one or with two elements and that returns a function in the first case.


```bracmat
( ( plus
  =   a b
    .     !arg:%?a ?b
        & !b:
        & '(.!arg+$a)
      | !a+!b
  )
& out$("1+2, not partial:" plus$(1 2))
& out$("1+2,     partial:" (plus$1)$2)
);
```


Output:


```txt
1+2, not partial: 3
1+2,     partial: 3
```



## C


```c
/* function with no argument */
f();

/* fix number of arguments */
g(1, 2, 3);

/* Optional arguments: err...
   Feel free to make sense of the following.  I can't. */
int op_arg();
int main()
{
	op_arg(1);
	op_arg(1, 2);
	op_arg(1, 2, 3);
	return 0;
}
int op_arg(int a, int b)
{
	printf("%d %d %d\n", a, b, (&b)[1]);
	return a;
}  /* end of sensible code */

/* Variadic function: how the args list is handled solely depends on the function */
void h(int a, ...)
{
	va_list ap;
	va_start(ap);
	...
}
/* call it as: (if you feed it something it doesn't expect, don't count on it working) */
h(1, 2, 3, 4, "abcd", (void*)0);

/* named arguments: this is only possible through some pre-processor abuse
*/
struct v_args {
    int arg1;
    int arg2;
    char _sentinel;
};

void _v(struct v_args args)
{
    printf("%d, %d\n", args.arg1, args.arg2);
}

#define v(...) _v((struct v_args){__VA_ARGS__})

v(.arg2 = 5, .arg1 = 17); // prints "17,5"
/* NOTE the above implementation gives us optional typesafe optional arguments as well (unspecified arguments are initialized to zero)*/
v(.arg2=1); // prints "0,1"
v();  // prints "0,0"

/* as a first-class object (i.e. function pointer) */
printf("%p", f); /* that's the f() above */

/* return value */
double a = asin(1);

/* built-in functions: no such thing. Compiler may interally give special treatment
   to bread-and-butter functions such as memcpy(), but that's not a C built-in per se */

/* subroutines: no such thing. You can goto places, but I doubt that counts. */

/* Scalar values are passed by value by default. However, arrays are passed by reference. */
/* Pointers *sort of* work like references, though. */
```



## C#


```c sharp

/* a function that has no argument */
	public int MyFunction();

	/* a function with a fixed number of arguments */
	FunctionWithArguments(4, 3, 2);

	/* a function with optional arguments */
	public void OptArg();

	public static void Main()
	{
		OptArg(1);
		OptArg(1, 2);
		OptArg(1, 2, 3);
	}
	public void ExampleMethod(int required,
        string optionalstr = "default string",
		int optionalint = 10)
	/* If you know the first and the last parameter */
	ExampleMethod(3, optionalint: 4);

	/* If you know all the parameter */
	ExampleMethod(3, "Hello World", 4);

	/* Variable number of arguments use array */
	public static void UseVariableParameters(params int[] list)

	/* Obtain return value from function */
	public internal MyFunction();
	int returnValue = MyFunction();

```


## C++


```C++


/* function with no arguments */
foo();

```



```C++

/* passing arguments by value*/
/* function with one argument */
bar(arg1);
/* function with multiple arguments */
baz(arg1, arg2);

```



```C++

/* get return value of a function */
variable = function(args);

```



```C++

#include <iostream>
using namespace std;
/* passing arguments by reference */
void f(int &y) /* variable is now passed by reference */
{
y++;
}
int main()
{
int x = 0;
cout<<"x = "<<x<<endl; /* should produce result "x = 0" */
f(x);                  /* call function f */
cout<<"x = "<<x<<endl; /* should produce result "x = 1" */
}

```



## COBOL


```cobol
CALL "No-Arguments"

*> Fixed number of arguments.
CALL "2-Arguments" USING Foo Bar

CALL "Optional-Arguments" USING Foo
CALL "Optional-Arguments" USING Foo Bar
*> If an optional argument is omitted and replaced with OMITTED, any following
*> arguments can still be specified.
CALL "Optional-Arguments" USING Foo OMITTED Bar
*> Interestingly, even arguments not marked as optional can be omitted without
*> a compiler warning. It is highly unlikely the function will still work,
*> however.
CALL "2-Arguments" USING Foo

*> COBOL does not support a variable number of arguments, or named arguments.

*> Values to return can be put in either one of the arguments or, in OpenCOBOL,
*> the RETURN-CODE register.
*> A standard function call cannot be done in another statement.
CALL "Some-Func" USING Foo
MOVE Return-Code TO Bar

*> Intrinsic functions can be used in any place a literal value may go (i.e. in
*> statements) and are optionally preceded by FUNCTION.
*> Intrinsic functions that do not take arguments may optionally have a pair of
*> empty parentheses.
*> Intrinsic functions cannot be defined by the user.
MOVE FUNCTION PI TO Bar
MOVE FUNCTION MEDIAN(4, 5, 6) TO Bar

*> Built-in functions/subroutines typically have prefixes indicating which
*> compiler originally incorporated it:
*>  - C$      - ACUCOBOL-GT
*>  - CBL_    - Micro Focus
*>  - CBL_OC_ - OpenCOBOL
*> Note: The user could name their functions similarly if they wanted to.
CALL "C$MAKEDIR" USING Foo
CALL "CBL_CREATE_DIR" USING Foo
CALL "CBL_OC_NANOSLEEP" USING Bar
*> Although some built-in functions identified by numbers.
CALL X"F4" USING Foo Bar

*> Parameters can be passed in 3 different ways:
*>  - BY REFERENCE - this is the default way in OpenCOBOL and this clause may
*>       be omitted. The address of the argument is passed to the function.
*>       The function is allowed to modify the variable.
*>  - BY CONTENT - a copy is made and the function is passed the address
*>      of the copy, which it can then modify. This is recomended when
*>      passing a literal to a function.
*>  - BY VALUE - the function is passed the address of the argument (like a
*>      pointer). This is mostly used to provide compatibility with other
*>      languages, such as C.
CALL "Modify-Arg" USING BY REFERENCE Foo *> Foo is modified.
CALL "Modify-Arg" USING BY CONTENT Foo   *> Foo is unchanged.
CALL "C-Func" USING BY VALUE Bar

*> Partial application is impossible as COBOL does not support first-class
*> functions.
*> However, as functions are called using a string of their PROGRAM-ID,
*> you could pass a 'function' as an argument to another function, or store
*> it in a variable, or get it at runtime.
ACCEPT Foo *> Get a PROGRAM-ID from the user.
CALL "Use-Func" USING Foo
CALL Foo USING Bar
```



## CoffeeScript


```coffeescript

# Calling a function that requires no arguments
foo()

# Calling a function with a fixed number of arguments
foo 1

# Calling a function with optional arguments
# (Optional arguments are done using an object with named keys)
foo 1, optionalBar: 1, optionalBaz: 'bax'

# Calling a function with a variable number of arguments
# for a function `foo` defined as `foo = ( args... ) ->`
foo 1, 2, 3, 4

# Calling a function with named arguments
# (Named arguments are done using an object with named keys)
foo bar: 1, bax: 'baz'

# Using a function in statement context
x = foo 1

# Using a function in first-class context within an expression
# (For `foo` defined as `foo = ( x ) -> x + 1`
x = [ 1, 2, 3 ].map foo

# Obtaining the return value of a function
x = foo 1

# Arguments are passed by value, even objects. Objects
# are passed as the _value_ of the reference to an object.
# Example:
bar = ( person ) ->
    # Since `person` is a reference
    # to the person passed in, we can assign
    # a new value to its `name` key.
    person.name = 'Bob'

    # Since `person` is just the value of
    # the original reference, assigning to it
    # does not modify the original reference.
    person = new Person 'Frank'

# Partial application is only possible manually through closures
curry = ( f, fixedArgs... ) ->
    ( args... ) -> f fixedArgs..., args...

# Example usage
add = ( x, y ) -> x + y

add2 = curry add, 2

add2 1 #=> 3

```



## Common Lisp


```lisp

;Calling a function that requires no arguments
(defun a () "This is the 'A' function")
(a)
;Calling a function with a fixed number of arguments
(defun b (x y) (list x y))
(b 1 2)
;Calling a function with optional arguments
(defun c (&optional x y) (list x y))
(c 1)
;Calling a function with a variable number of arguments
(defun d (&rest args) args)
(d 1 2 3 4 5 6 7 8)
;Calling a function with named arguments
(defun e (&key (x 1) (y 2)) (list x y))
(e :x 10 :y 20)
;Using a function in first-class context within an expression
(defun f (func) (funcall func))
(f #'a)
;Obtaining the return value of a function
(defvar return-of-a (a))
;Is partial application possible and how
(defun curry (function &rest args-1)
  (lambda (&rest args-2)
    (apply function (append args-1 args-2))))
(funcall (curry #'+ 1) 2)

```



## D


```d
import std.traits;

enum isSubroutine(alias F) = is(ReturnType!F == void);

void main() {
    void foo1() {}

    // Calling a function that requires no arguments:
    foo1();
    foo1; // Alternative syntax.


    void foo2(int x, int y) {}

    immutable lambda = function int(int x) => x ^^ 2;

    // Calling a function with a fixed number of arguments:
    foo2(1, 2);
    foo2(1, 2);
    cast(void)lambda(1);


    void foo3(int x, int y=2) {}

    // Calling a function with optional arguments:
    foo3(1);
    foo3(1, 3);

    int sum(int[] arr...) {
        int tot = 0;
        foreach (immutable x; arr)
            tot += x;
        return tot;
    }

    real sum2(Args...)(Args arr) {
        typeof(return) tot = 0;
        foreach (immutable x; arr)
            tot += x;
        return tot;
    }

    // Calling a function with a variable number of arguments:
    assert(sum(1, 2, 3) == 6);
    assert(sum(1, 2, 3, 4) == 10);
    assert(sum2(1, 2.5, 3.5) == 7);

    // Calling a function with named arguments:
    // Various struct or tuple-based tricks can be used for this,
    // but currently D doesn't have named arguments.


    // Using a function in statement context (?):
    if (1)
        foo1;

    // Using a function in first-class context within an expression:
    assert(sum(1) == 1);


    auto foo4() { return 1; }

    // Obtaining the return value of a function:
    immutable x = foo4;


    // Distinguishing built-in functions and user-defined functions:
    // There are no built-in functions, beside the operators, and
    // pseudo-functions like assert().


    int myFynction(int x) { return x; }
    void mySubroutine(int x) {}

    // Distinguishing subroutines and functions:
    // (A subroutine is merely a function that has no explicit
    // return statement and will return void).
    pragma(msg, isSubroutine!mySubroutine); // Prints: true
    pragma(msg, isSubroutine!myFynction);   // Prints: false


    void foo5(int a, in int b, ref int c, out int d, lazy int e, scope int f) {}

    // Stating whether arguments are passed by value, by reference, etc:
    alias STC = ParameterStorageClass;
    alias psct = ParameterStorageClassTuple!foo5;
    static assert(psct.length == 6); // Six parameters.
    static assert(psct[0] == STC.none);
    static assert(psct[1] == STC.none);
    static assert(psct[2] == STC.ref_);
    static assert(psct[3] == STC.out_);
    static assert(psct[4] == STC.lazy_);
    static assert(psct[5] == STC.scope_);
    // There are also inout and auto ref.


    int foo6(int a, int b) { return a + b; }

    // Is partial application possible and how:
    import std.functional;
    alias foo6b = partial!(foo6, 5);
    assert(foo6b(6) == 11);
}
```

{{out}}

```txt
true
false
```


=={{header|Déjà Vu}}==

```dejavu
# all functions used are from the standard library
# calling a function with no arguments:
random-int
# calling a function with a fixed number of arguments:
+ 1 2
# calling a function with optional arguments:
# optional arguments are not really possible as such
# generally differently named functions are used:
sort [ 3 2 1 ]
sort-by @len [ "Hello" "World" "Bob" ]
# calling a function with a variable number of arguments:
# generally with a special terminator value, which one depends
# on the function called
concat( 1 2 3 )
[ 1 2 3 ]
set{ :foo :bar :spam }
# calling a function with named arguments: not possible
# using a function in first-class context within an expression
$ @-- @len # $ is "compose", so the function returned is "one less than the length"
# obtaining the return value of a function
# return values are always pushed on the stack, so you don't need anything special
random-int
# discarding the return value of a function
drop random-int
# method call:
local :do { :nuthin @pass }
do!nuthin
!import!fooModule # same as eva!import :fooModule
# arguments are passed by object-identity, like in Python and Lua
# partial application is not possible, due to the fact that
# a function's arity is a property of its behavior and not
# of its definition
```



## Dragon


* Calling a function that requires no arguments

```dragon
myMethod()
```


* Calling a function with a fixed number of arguments

```dragon
myMethod(97, 3.14)
```



## Dyalect


Calling a function that requires no arguments:


```Dyalect
func foo() { }
foo()
```


Calling a function with a fixed number of arguments:


```Dyalect
func foo(x, y, z) { }
foo(1, 2, 3)
```


Calling a function with optional arguments:


```Dyalect
func foo(x, y = 0, z = 1) { }
foo(1)
```


Calling a function with a variable number of arguments:


```Dyalect
func foo(args...) { }
foo(1, 2, 3)
```


Calling a function with named arguments:


```Dyalect
func foo(x, y, z) { }
foo(z: 3, x: 1, y: 2)
```


Using a function in statement context:


```Dyalect
func foo() { }
if true {
    foo()
}
```


Using a function in first-class context within an expression:


```Dyalect
func foo() { }
var x = if foo() {
    1
} else {
    2
}
```


Obtaining the return value of a function:


```Dyalect
func foo(x) { x * 2 }
var x = 2
var y = foo(x)
```


Distinguishing built-in functions and user-defined functions:


```Dyalect>//Not possible in Dyalect at the moment</lang


Distinguishing subroutines and functions:


```Dyalect
//There is no difference between subroutines and functions:
func foo() { } //doesn't explicitely return something (but in fact returns nil)
func bar(x) { return x * 2 } //explicitely returns value (keyword "return" can be omitted)
```


Stating whether arguments are passed by value or by reference:


```Dyalect>//All arguments are passed by reference</lang


Is partial application possible and how:


```Dyalect
//Using a closure:
func apply(fun, fst) { snd => fun(fst, snd) }

//Usage:
func sum(x, y) { x + y }

var sum2 = apply(sum, 2)
var x = sum2(3) //x is 5

//By second argument
func flip(fun) { (y, x) => fun(x, y) }
func sub(x, y) { x - y }

var sub3 = apply(flip(sub), 3)
x = sub3(9) //x is 6
```



## Elena

ELENA 4.1:
Declaring closures

```elena

var c0 := { console.writeLine("No argument provided") };
var c2 := (int a, int b){ console.printLine("Arguments ",a," and ",b," provided") };

```

Calling a closure without arguments

```elena

   c0();

```

Calling a closure with arguments

```elena

   c2(2,4);

```

Passing arguments by reference:

```elena

    var exch := (ref object x){ x := 2 };
    var a := 1;
    exch(ref a);

```



## Elixir



```elixir

# Anonymous function

foo = fn() ->
  IO.puts("foo")
end

foo()  #=> undefined function foo/0
foo.() #=> "foo"

# Using `def`

defmodule Foo do
  def foo do
    IO.puts("foo")
  end
end

Foo.foo    #=> "foo"
Foo.foo()  #=> "foo"


# Calling a function with a fixed number of arguments

defmodule Foo do
  def foo(x) do
    IO.puts(x)
  end
end

Foo.foo("foo") #=> "foo"

# Calling a function with a default argument

defmodule Foo do
  def foo(x \\ "foo") do
    IO.puts(x)
  end
end

Foo.foo()      #=> "foo"
Foo.foo("bar") #=> "bar"

# There is no such thing as a function with a variable number of arguments. So in Elixir, you'd call the function with a list

defmodule Foo do
  def foo(args) when is_list(args) do
    Enum.each(args, &(IO.puts(&1)))
  end
end

# Calling a function with named arguments

defmodule Foo do
  def foo([x: x]) do
    IO.inspect(x)
  end
end

```



## Erlang


```erlang

no_argument()
one_argument( Arg )
optional_arguments( Arg, [{opt1, Opt1}, {another_opt, Another}] )
variable_arguments( [Arg1, Arg2 | Rest] )
names_arguments([{name1, Arg1}, {another_name, Another}] )
% Statement context?
% First class context?
Result = obtain_result( Arg1 )
% No way to distinguish builtin/user functions
% Subroutines?
% Arguments are passed by reference, but you can not change them.
% Partial application is possible (a function returns a function that has one argument bound)

```


=={{header|F Sharp|F#}}==

```fsharp
// No arguments
noArgs()

// Fixed number of arguments
oneArg x

// Optional arguments
// In a normal function:
optionalArgs <| Some(5) <| None
// In a function taking a tuple:
optionalArgsInTuple(Some(5), None)
// In a function in a type:
foo.optionalArgs 5;;
// However, if you want to pass more than one paramter, the arguments must be
// passed in a tuple:
foo.optionalArgs(5, 6)

// Function with a variable number of arguments
variableArgs 5 6 7 // etc...

// Named arguments can only be used in type methods taking a tuple. The
// arguments can appear in any order.
foo.namedArgs(x = 5, y = 6)

// Using a function in a statement
for i = 0 to someFunc() do
    printfn "Something"

// Using a function in a first-class context
funcArgs someFunc

// Obtaining a return value
let x = someFunc()

// Built-in functions: do functions like (+) or (-) count?

// Parameters are normally passed by value (as shown in the previous examples),
// but they can be passed by reference.
// Passing by reference:
refArgs &mutableVal

// Partial application example
let add2 = (+) 2
```



## Factor

* Calling a word with no arguments:

```Factor>foo</lang


* Calling a word with a fixed number of arguments. This will pull as many objects as it needs from the stack. If there are not enough, it will result in a stack underflow.

```Factor>foo</lang


* No special support for optional arguments.

* Variable arguments are achieved by defining a word that takes an integer, and operates on that many items at the top of the stack:

```Factor
"a" "b" "c" 3 narray
! { "a" "b" "c" }
```


* The named arguments idiom is to define a tuple, set its slots, and pass it to a word:

```Factor><email

    "jack@aol.com" >>from
    { "jill@aol.com" } >>to
    "Hello there" >>subject
    body >>body
send-email
```


* First-class context: this pushes a word to the stack. Use execute to evaluate.

```Factor>\ foo</lang

Additionally, you can put words directly inside sequences and quotations for deferred execution:

```Factor
{ foo } [ foo ]
```


* Obtaining the return value, which will be placed on the stack:

```Factor>foo</lang


* Returns true if the word is defined in the Factor VM as opposed to in a vocabulary. It should be noted that there are very few primitives.

```Factor>\ foo primitive?</lang


* Factor makes no distinction between subroutines and functions.

* It's not perfectly accurate to think of words as passing arguments by reference (or at all), since all words simply operate on the data stack. However, it is still important for the programmer to understand that words which make duplicates of objects such as <tt>dup</tt> and <tt>over</tt> do so by creating references. If one wishes for a unique copy, one may use <tt>clone</tt>.

* Partial application is possible by use of curry. Here, the object 2 is curried into the left side of the quotation (anonymous function) <tt>[ - ]</tt>:

```Factor
{ 1 2 3 } 2 [ - ] curry map .
! { -1 0 1 }
```



## Forth


```forth
a-function         \ requiring no arguments
a-function         \ with a fixed number of arguents
a-function         \ having optional arguments
a-function         \ having a variable number of arguments
a-function         \ having such named arguments as we have in Forth
' a-function var ! \ using a function in a first-class context (here: storing it in a variable)
a-function         \ in which we obtain a function's return value

                   \ forth lacks 'statement contenxt'
                   \ forth doesn't distinguish between built-in and user-defined functions
                   \ forth doesn't distinguish between functions and subroutines
                   \ forth doesn't care about by-value or by-reference

\ partial application is achieved by creating functions and manipulating stacks
: curried  0 a-function ;
: only-want-third-argument 1 2 rot a-function ;

\ Realistic example:
: move ( delta-x delta-y -- )
  y +!  x +! ;

: down ( n -- )  0 swap move ;
: up ( n -- )    negate down ;
: right ( n -- ) 0 move ;
: left ( n -- )  negate right ;
```



## Fortran


### Examples


```Fortran
program main
implicit none
integer :: a
integer :: f, g
logical :: lresult
interface
  integer function h(a,b,c)
    integer :: a, b
    integer, optional :: c
  end function
end interface
write(*,*) 'no arguments: ', f()
write(*,*) '-----------------'
write(*,*) 'fixed arguments: ', g(5,8,lresult)
write(*,*) '-----------------'
write(*,*) 'optional arguments: ', h(5,8), h(5,8,4)
write(*,*) '-----------------'
write(*,*) 'function with variable arguments: Does not apply!'
write(*,*) 'An option is to pass arrays of variable lengths.'
write(*,*) '-----------------'
write(*,*) 'named arguments: ', h(c=4,b=8,a=5)
write(*,*) '-----------------'
write(*,*) 'function in statement context: Does not apply!'
write(*,*) '-----------------'
write(*,*) 'Fortran passes memory location of variables as arguments.'
write(*,*) 'So an argument can hold the return value.'
write(*,*) 'function result: ', g(5,8,lresult) , ' function successful? ', lresult
write(*,*) '-----------------'
write(*,*) 'Distinguish between built-in and user-defined functions: Does not apply!'
write(*,*) '-----------------'
write(*,*) 'Calling a subroutine: '
a = 30
call sub(a)
write(*,*) 'Function call: ', f()
write(*,*) '-----------------'
write(*,*) 'All variables are passed as pointers.'
write(*,*) 'Problems can arise if instead of sub(a), one uses sub(10).'
write(*,*) '-----------------'
end program

!no argument
integer function f()
f = 10
end function

!fixed number of arguments
integer function g(a, b, lresult)
integer :: a, b
logical :: lresult
g = a+b
lresult = .TRUE.
end function

!optional arguments
integer function h(a, b, c)
integer :: a, b
integer, optional :: c

h = a+b
if(present(c)) then
  h = h+10*c
end if
end function

!subroutine
subroutine sub(a)
integer :: a
a = a*100
write(*,*) 'Output of subroutine: ', a
end subroutine

```



```txt

 no arguments:           10
 -----------------
 fixed arguments:           13
 -----------------
 optional arguments:           13          53
 -----------------
 function with variable arguments: Does not apply!
 An option is to pass arrays of variable lengths.
 -----------------
 named arguments:           53
 -----------------
 function in statement context: Does not apply!
 -----------------
 Fortran passes memory location of variables as arguments.
 So an argument can hold the return value.
 function result:           13  function successful?  T
 -----------------
 Distinguish between built-in and user-defined functions: Does not apply!
 -----------------
 Calling a subroutine:
 Output of subroutine:         3000
 Function call:           10
 -----------------
 All variables are passed as pointers.
 Problems can arise if instead of sub(a), one uses sub(10).
 -----------------

```


### In other words

As described in [[Naming_conventions#Fortran|Naming Conventions]], First Fortran (1958) allowed user-written functions but with restrictions on the names so that an ordinary variable called SIN would be disallowed because it was deemed to be in conflict with the library function SINF. These constraints were eased with Fortran II, and the rule became that a user could employ any correct-form name, such as SQRT, for a variable's name (simple or array) but then the library function SQRT would become inaccessible in such a routine. Similarly, there would be no point in the user writing a function called SQRT, because it could not be invoked - the compiler would take any invocation as being for the library routine SQRT. Thus, a user-written function could perhaps chance to have the name of an obscure (i.e. one forgotten about) library function, but if you were lucky it would have conflicting parameters and the compiler will complain.

A special case is provided by the "arithmetic statement function" that is defined after declarations but before executable statements in a routine and which has access to all the variables of the routine. Consider
```Fortran
      REAL this,that
      DIST(X,Y,Z) = SQRT(X**2 + Y**2 + Z**2) + this/that !One arithmetic statement, possibly lengthy.
      ...
      D = 3 + DIST(X1 - X2,YDIFF,SQRT(ZD2))              !Invoke local function DIST.
```

In this case, even if "DIST" happened to be the name of some library function, invocations within the routine defining it would not be of the library function.

This flexibility in naming can be turned the other way around. For example, some compilers offer the intrinsic function SIND which calculates ''sine'' in degrees. Simply defining an array <code>REAL SIND(0:360)</code> (and properly initialising it) enables the slowish SIND function to be approximated by the faster indexing of an array. Put another way, an array is a function of a limited span of integer-valued arguments and is called in arithmetic expressions with the same syntax as is used for functions, be they intrinsic or user-written. Those writing in Pascal would be blocked by its insistence that arrays employ [] rather than (). Similarly, when testing, an array's declaration might be commented out and a function of that name defined, which function could check its arguments, write to a log file, note time stamps, or whatever else comes to mind. But alas, there is no "palindromic" or reverse-entry facility whereby a function could handle the assignment of a value ''to'' an array that would make this fully flexible.

Within a function there are some delicacies. The usual form is to assign the desired result to the name of the variable as in <code>H = A + B</code> where <code>H</code> is the name of the function. However, during evaluation the desired result might be developed over many stages and with reference to prior values. Suppose function H is to combine results from separate statements and it is not convenient to achieve this via one lengthy expression, perhaps because of conditional tests. Something like
```Fortran
      H = A + B
      IF (blah) H = 3*H - 7
```

As written, the appearance of <code>H</code> on the right-hand side of an expression does ''not'' constitute a call of function <code>H</code> at all. Some compilers fail to deal with this as hoped, and so one must use a scratch variable such as <code>FH</code> to develop the value, then remember to ensure that the assignment <code>H = FH</code> is executed before exiting the function, by whatever route. If the result is a large datum (a long character variable, say) this is annoying.

With the belated recognition of recursive possibilities (introduced by Algol in the 1960s) comes the possibility of a function invoking itself. In the above example, <code>H(3.7,5.5,6.6)</code> would clearly be a function invocation (because of the parentheses) whereas <code>H</code> would not be. Actually, Fortran routines have always been able to engage in recursion, it is just the returns that will fail - except on a stack-based system such as the Burroughs 6700 in the 1970s.

Fortran also offers the ability to pass a function as a parameter such that the recipient routine can call it, as in
```Fortran
      REAL FUNCTION INTG8(F,A,B,DX)	!Integrate function F.
       EXTERNAL F	!Some function of one parameter.
       REAL A,B		!Bounds.
       REAL DX		!Step.
       INTEGER N	!A counter.
        INTG8 = F(A) + F(B)	!Get the ends exactly.
        N = (B - A)/DX		!Truncates. Ignore A + N*DX = B chances.
        DO I = 1,N		!Step along the interior.
          INTG8 = INTG8 + F(A + I*DX)	!Evaluate the function.
        END DO			!On to the next.
        INTG8 = INTG8/(N + 2)*(B - A)	!Average value times interval width.
      END FUNCTION INTG8	!This is not a good calculation!

      FUNCTION TRIAL(X)		!Some user-written function.
       REAL X
        TRIAL = 1 + X		!This will do.
      END FUNCTION TRIAL	!Not the name of a library function.

      PROGRAM POKE
      INTRINSIC SIN	!Thus, not an (undeclared) ordinary variable.
      EXTERNAL TRIAL	!Likewise, but also, not an intrinsic function.
      REAL INTG8	!Don't look for the result in an integer place.
       WRITE (6,*) "Result=",INTG8(SIN,  0.0,8*ATAN(1.0),0.01)
       WRITE (6,*) "Linear=",INTG8(TRIAL,0.0,1.0,        0.01)
      END
```

This involves a fair amount of juggling special declarations so that the compiler will make the desired assumptions that a function is being called upon, rather than the value of some variable. This is eased somewhat with F90 onwards if the MODULE protocol is used so that at least you do not have to remember to declare INTG8 as REAL. Certain library functions are not allowed as candidates for passing to INTG8 (for instance, the compiler may render them as in-line code, bypassing the protocol used for functions) and arithmetic statement functions are usually rejected, as would be an array masquerading as a function. Arithmetic expressions are not allowable as possible "functions" either - how might something like <code>sin(x) + 3*sqrt(x) + 7</code> be recognised as a function instead? As <code>INTG8(SIN + 3*SQRT + 7,''etc...''</code>? Unlike Algol, Fortran does not offer the call-by-name facility as used in [[Jensen's_Device|Jensen's Device]], which would be something like <code>INTG8(SIN(X) + 3*SQRT(X) + 7,''etc...''</code> and would also require passing variable X. Perhaps a keyword BYNAME might be introduced one day. Until then a properly-named function must be declared and its name only be given. And of course, candidate functions must have the correct number and type of parameters, or else...

This works because Fortran passes parameters by reference (i.e. by giving the machine address of the entity), so that for functions, the code's entry point for the function is passed. With normal variables this means that a function (or subroutine) might modify the value of a parameter, as well as returning the function's result - and also mess with any COMMON data or other available storage, so a function EATACARD(IN) might read a line of data into a shared work area (called say ACARD) from I/O unit number IN and return ''true'', otherwise ''false'' should it hit end-of-file.

But it is also possible that parameters are passed via copy-in copy-out instead of by reference, with subtle changes in behaviour. This may also be done even on systems that do employ passing by reference. For instance, with
```Fortran
      TYPE MIXED
       CHARACTER*12 NAME
       INTEGER STUFF
      END TYPE MIXED
      TYPE(MIXED) LOTS(12000)
```

One might hope to try <code>IT = BCHOP(LOTS.NAME,"Fred")</code> where BCHOP is a well-tested function for performing a binary search that should run swiftly. Alas, no. The successive values of NAME are not contiguous while BCHOP expects to receive an array of values that are contiguous - that is, with a "stride" of one. So, the compiler inserts code to copy all the LOTS.NAME elements into such a work area and passes the location of that to BCHOP (which searches it swiftly), then on return, the work area is copied back to LOTS.NAME just in case there had been a change. This latter can be avoided if within BCHOP its array is given the attribute INTENT(IN) for read-only but the incoming copy still means an effort of order N, while for the search the effort is just Log(N). This can have a less-than-subtle effect if large arrays are involved.


## Fortress


```fortress

component call_a_function
  export Executable
  (* Declaring test functions that allow the various ways to call functions in Fortress to be demonstrated. *)
  addition(i:ZZ32, j:ZZ32): ZZ32 = i+j
  addition(i:ZZ32): ZZ32 = i+1

  (* Strings are concatenated by using a space as an infix operator. *)
  addition(i:String, j:String): String = i j

  printAString(s:String): () = println(s)

  (* Functions can be passed to other functions as arguments. When passing a function as an argument, the argument's type should be
  represented as follows: "typeOfArgument(s)->returnType," which, in this case, is "String->()." You could also technically use the
  "Any" type, but that isn't type-safe. *)
  printAString(s:String, f:String->()) = f(s)

  (* Defined functions can then be called as follows. *)
  var x:ZZ32 = addition(1, 2)
  var str:String = addition("This is ", "another string.")

  run() = do
    (* You can call built-in functions the same way that you call functions that you define. *)
    println("x at start: " x)

    x := addition(x, 2)

    println("x at middle: " x)

    printAString("This " "is " "a " "string.")
    printAString(str)
    printAString("\nThis is a string that is being printed by a function of the same name \nthat takes a function as an argument.\n",
      printAString)

    x := addition(4)

    println("x at end: " x)
  end
end

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=1bbbeb240f6fbca4b893271f1a19833b Click this link to run this code]'''

Some of the uses of Procedures/Functions in Gambas

```gambas
Public Sub Main()

Hello
Print CopyIt("Hello ", 6)
Print CopyIt("Hello ", 3, "!!")

End
'_____________________________________________________________________________________
Public Sub CopyIt(sString As String, siNo As Short, Optional sEnd As String) As String
Dim siCount As Short
Dim sNewString As String

For siCount = 1 To siNo
  sNewString &= sString
Next

Return Trim(sNewString) & sEnd

End
'_____________________________________________________________________________________
Public Sub Hello()

Print "Hello world!"

End
```

Output:

```txt

Hello world!
Hello Hello Hello Hello Hello Hello
Hello Hello Hello!!

```



## Go

The following examples use functions from the standard packages
plus a few dummy local functions:
::
```go
import (
	"image"
	"image/gif"
	"io/ioutil"
	"strings"
	"unicode"
)

func f() (int, float64)  { return 0, 0 }
func g(int, float64) int { return 0 }
func h(string, ...int)   {}
```

* Calling with no arguments and calling with a fixed number of arguments:
::
```go
	f()
	g(1, 2.0)
	// If f() is defined to return exactly the number and type of
	// arguments that g() accepts than they can be used in place:
	g(f())
	// But only without other arguments, this won't compile:
	//h("fail", f())
	// But this will:
	g(g(1, 2.0), 3.0)
```

* Calling with a variable number of arguments:
::This is only possible with functions defined with a trailing optional/variable length argument of a single type (as <code>h</code> above).
```go
	h("ex1")
	h("ex2", 1, 2)
	h("ex3", 1, 2, 3, 4)
	// such functions can also be called by expanding a slice:
	list := []int{1,2,3,4}
	h("ex4", list...)
	// but again, not mixed with other arguments, this won't compile:
	//h("fail", 2, list...)
```

* Optional arguments and named arguments are not supported.
::However, it is reasonably common to see a structure used for this. In this example <code>gif.Options</code> is a structure with multiple members which can initialized/assigned by name or omitted (or the whole third argument can just be <code>nil</code>).
```go
	gif.Encode(ioutil.Discard, image.Black, &gif.Options{NumColors: 16})
```

* Within a statement context.
::Assignment statements are shown later. Only functions returning a single value can be used in a single value context:
```go
	if 2*g(1, 3.0)+4 > 0 {}
```

* In a first-class context:
::
```go
	fn := func(r rune) rune {
		if unicode.IsSpace(r) {
			return -1
		}
		return r
	}
	strings.Map(fn, "Spaces removed")
	strings.Map(unicode.ToLower, "Test")
	strings.Map(func(r rune) rune { return r + 1 }, "shift")
```

* Obtaining the value:
::<lang>	a, b := f()              // multivalue return
	_, c := f()              // only some of a multivalue return
	d := g(a, c)             // single return value
	e, i := g(d, b), g(d, 2) // multiple assignment
```

* Built-in functions and user defined functions can not be distinguished.
::Functions from the standard packages look like any other. The few truly built-in functions are only different in that they have no package specifier like local functions (and they sometimes have extra capabilities).
```go
	list = append(list, a, d, e, i)
	i = len(list)
```

* Go has no subroutines, just functions and methods.
* Go arguments are passed by value.
::As with C, a pointer can be used to achieve the effect of reference passing. (Like pointers, slice arguments have their contents passed by reference, it's the slice header that is passed by value).
* Partial application is not directly supported.
::However something similar can be done, see [[Partial function application#Go]]


## Groovy

There are two types of first-class functions in Groovy.

# The first are functions defined in scripts, although they behave as if they are methods of the script "class".
# The second are closures, which are similar to lambdas in Java, except that they are defined as their own class type and must be explicitly converted to single-method "functional" interfaces. There are many methods within the Groovy API that accept closures as arguments.


* Calling a function that requires no arguments

```groovy
noArgs()
```


* Calling a function with a fixed number of arguments

```groovy
fixedArgs(1, "Zing", Color.BLUE, ZonedDateTime.now(), true)
```


* Calling a function with optional arguments

```groovy
optArgs("It's", "a", "beautiful", "day")
optArgs("It's", "a", "beautiful")
optArgs("It's", "a")
optArgs("It's")
```


* Calling a function with a variable number of arguments

```groovy
varArgs("It's", "a", "beautiful", "day")
varArgs("It's", "a", "beautiful")
varArgs("It's", "a")
varArgs("It's")
```


* Calling a function with named arguments
It's complicated

* Using a function in statement context

```groovy
def mean = calcAverage(1.2, 4.5, 3, 8.9, 22, 3)
```


* Using a function in first-class context within an expression
** Create new functions from preexisting functions at run-time

```groovy
def oldFunc = { arg1, arg2 -> arg1 + arg2 }
def newFunc = oldFunc.curry(30)
assert newFunc(12) == 42
```

** Store functions in collections

```groovy
def funcList = [func1, func2, func3]
```

** Use functions as arguments to other functions

```groovy
def eltChangeFunc = { it * 3 - 1 }
def changedList = list.collect(eltChangeFunc)
```

** Use functions as return values of other functions

```groovy
def funcMaker = { String s, int reps, boolean caps ->
    caps ? { String transString -> ((transString + s) * reps).toUpperCase() }
         : { String transString -> (transString + s) * reps }
}
def func = funcMaker("a", 2, true)
assert func("pook") == "POOKAPOOKA"
```


* Obtaining the return value of a function

```groovy
def retVal = func(x, y, z)
```


* Distinguishing built-in functions and user-defined functions
There are no "built-in" functions. All is illusion.

* Stating whether arguments are passed by value or by reference
As with Java everything is passed by value, but object values are actually references (pointers). So,
** if the argument is a primative it is passed by value and changes are not manifested in the caller's context.
** if the argument is an object reference, the reference is passed by value and changes to the reference (re-assignment, for example) are not manifested in the caller's context, but changes in the object are.


* Is partial application possible and how
Partial application in Groovy is performed via currying (demonstrated above)


## Haskell



```haskell

-- Calling a function with a fixed number of arguments
multiply x y = x * y
multiply 10 20 -- returns 200

-- Calling a function that requires no arguments
-- Normally, you use constant instead of function without arguments:
twopi = 6.28
-- But you can also pass special value as the first argument indicating function call:
twopi () = 6.28 -- definition
twopi :: Num a => () -> a -- its type
twopi () -- returns 6.28

-- Partial application and auto-currying is built-in.
multiply_by_10 = (10 * )
map multiply_by_10 [1, 2, 3] -- [10, 20, 30]
multiply_all_by_10 = map multiply_by_10
multiply_all_by_10 [1, 2, 3] -- [10, 20, 30]

-- TODO:
-- Calling a function with optional arguments
-- Calling a function with a variable number of arguments
-- Calling a function with named arguments
-- Using a function in statement context
-- Using a function in first-class context within an expression
-- Obtaining the return value of a function
-- Distinguishing built-in functions and user-defined functions
-- Distinguishing subroutines and functions
-- Stating whether arguments are passed by value or by reference

```



## i


```i
//The type of the function argument determines whether or not the value is passed by reference or not.
//Eg. numbers are passed by value and lists/arrays are passed by reference.

software {
	print() 					//Calling a function with no arguments.
	print("Input a number!")	//Calling a function with fixed arguments.
	print(1,2,3,4,5,6,7,8,9,0) 	//Calling a function with variable arguments.
	input = read() 				//Obtaining the return value of a function.
	myprint = print
	myprint("It was: ", input)	//Calling first class functions, the same as calling ordinary functions.

	//The only distinction that can be made between two functions is if they are 'real' or not.
	if type(myprint) = concept
		print("myprint is a not a real function")
	else if type(myprint) = function
		print("myprint is a real function")
	end

	//Partial functions can be created with static parts.
	DebugPrint = print["[DEBUG] ", text]
	DebugPrint("partial function!")		//This would output '[DEBUG] partial function!'

	if type(DebugPrint) = concept
		print("DebugPrint is a not a real function")
	else if type(DebugPrint) = function
		print("DebugPrint is a real function")
	end
}

```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon have generalized procedures and syntax that are used to implement functions, subroutines and generators.
* Procedures can return values or not and callers may use the returned values or not.
* Procedures in Icon and Unicon are first class values and can be assigned to variables which can then be used to call procedures.  This also facilitates some additional calling syntax.
* Additionally, co-expressions are supported which allow for co-routine like transfers of control between two or more procedures.  There are some differences in syntax for co-expression calls.
* There are no differences between calling built-in vs. user defined functions
* Named arguments is not natively supported; however, they can be supported using a user defined procedure as shown in [[Named_parameters#Icon_and_Unicon|Named parameters]]
* Method calling is similar with some extended syntax
* Arguments are basically passed by value or reference based on their type.  Immutable values like strings, and numbers are passed by value.  Mutable data types like structures are essentially references and although these are passed by value the effective behavior is like a call by reference.

For more information see [[Icon%2BUnicon/Intro|Icon and Unicon Introduction on Rosetta]]


```Icon
procedure main()  # demonstrate and describe function calling syntax and semantics

   # normal procedure/function calling

   f()                      # no arguments, also command context
   f(x)                     # fixed number of arguments
   f(x,h,w)                 # variable number of arguments (varargs)
   y := f(x)                # Obtaining the returned value of a function

   # procedures as first class values and string invocation

   f!L                      # Alternate calling syntax using a list as args
   (if \x then f else g)()  # call (f or g)()
   f := write               # assign a procedure
   f("Write is now called") # ... and call it
   "f"()                    # string invocation, procedure
   "-"(1)                   # string invocation, operator

   # Co-expressions

   f{e1,e2}                 # parallel evaluation co-expression call
                            # equivalent to f([create e1, create e2])
   expr @ coexp             # transmission of a single value to a coexpression
   [e1,e2]@coexp            # ... of multiple values (list) to a coexpression
   coexp(e1,e2)             # ... same as above but only in Unicon

   # Other

   f("x:=",1,"y:=",2)       # named parameters (user defined)
end
```



## J


A function in J is typically represented by a verb.  Under the right circumstances other syntactic entities (nouns, adverbs, conjunctions) can represent functions, but let's focus on the typical case.

A verb, in J, typically supports two syntactic variants:


```j
        verb noun
   noun verb noun
```


And a noun, in J, is an array.

An argument list can be represented by an array.  Thus, when dealing with multiple arguments, a typical form is:


```j>  function argumentList</lang


Here, <code>function</code> is a verb and <code>argumentList</code> is a noun.

For example:


```j
  sum(1,2,3)
```


Here <code>sum</code> is a verb and <code>(1,2,3)</code> is a noun.

Thus:

''A function that requires no arguments'' can be simulated by calling a function with empty argument list:
```j
f''
```
  Note that an empty list of characters is not the only constant in the language which is an empty list.  That said, most operations in the language do not care what type of data is not present, in an array which contains nothing.


''A function with a fixed number of arguments'' gets special treatment in J when the fixed number is 1 or 2.
```j
f 'one argument'
```
and
```j
'this example has two arguments' f 'the other argument'
```
  Alternatively, the function can be written such that an argument list is an error when it's the wrong length.

''A function with a variable number of arguments (varargs)'':  See above.

If argument types conflict they will need to be put in boxes and the function will have to take its arguments out of the boxes.  Here's an unboxed example with five arguments:
```j
 f 1,2,3,4,5
```
 and here's a boxed example with five arguments:
```j
f (<1),(<2),(<3),(<4),(<5)
```
 Note that the last set of parenthesis is unnecessary
```j
f (<1),(<2),(<3),(<4),<5
```
  Note also that J offers some syntactic sugar for this kind of list
```j>f 1; 2; 3; 4; <5</lang
.  Note also that if the last argument in a semicolon list is not boxed there is no need to explicitly box it, since that is unambiguous (it must be boxed so that it conforms with the other members of the list).
```j>f 1; 2; 3; 4; 5</lang


''A function with named arguments'' can be accomplished by calling a function with the names of the arguments.
```j
f 'george';'tom';'howard'
```
  Other interpretations of this concept are also possible.  For example, the right argument for a verb might be a list of argument names and the left argument might be a corresponding list of argument values:
```j
1 2 3 f 'george';'tom';'howard'
```
  Or, for example a function which requires an object could be thought of as a function with named arguments since an object's members have names:
```j
   obj=: conew'blank'
   george__obj=: 1
   tom__obj=: 2
   howard__obj=: 3
   f obj
   coerase obj
```
  Name/value pairs can also be used for this purpose and can be implemented in various ways, including passing names followed by values
```j
f 'george';1;'tom';2;'howard';3
```
 and passing a structure of pairs
```j
f ('george';1),('tom';2),:(howard';3)
```
  Or, for example, the pairs could be individually boxed:
```j
f ('george';1);('tom';2);<howard';3
```


''Using a function in command context'' is no different from using a function in any other context, in J.  ''Using a function in first class context within an expression'' is no different from using a function in any other context, in J.

''Obtaining the return value of a function'' is no different from using a function in j.  For example, here we add 1 to the result of a function:
```j>1 + f 2</lang


The only ''differences that apply to calling builtin functions rather than user defined functions'' is spelling of the function names.

There are no ''differences between calling subroutines and functions'' because J defines neither <code>subroutines</code> nor <code>functions</code>.  Instead, J defines <code>verbs</code>, <code>adverbs</code>, and <code>conjunctions</code> which for the purpose of this task are treated as functions. (All of the above examples used verbs. J's adverbs and conjunctions have stronger [[wp:Valence|valence]] than its verbs.)


## Java

Java does not have functions, but Java classes have "methods" which are equivalent.

* Calling a function that requires no arguments

```java
myMethod()
```

We didn't specify an object (or a class) as the location of the method, so <tt>this.myMethod()</tt> is assumed. This applies to all the following examples.

* Calling a function with a fixed number of arguments

```java
myMethod(97, 3.14)
```


* Calling a function with optional arguments
This is possible if the method name is overloaded with different argument lists. For example:

```java
int myMethod(int a, double b){
    // return result of doing sums with a and b
}

int myMethod(int a){
    return f(a, 1.414);
}
```


The compiler figures out which method to call based on the types of the arguments, so in this case the second argument appears to be optional. If you omit it, the value <tt>1.414</tt> is used.

```java
System.out.println( myMethod( 97, 3.14 ) );
System.out.println( myMethod( 97 ) );
```


* Calling a function with a variable number of arguments
This is possible if the method is defined with varargs syntax. For example:

```java
void printAll(String... strings){
    for ( String s : strings )
        System.out.println( s );
}
```


The type of <tt>strings</tt> is actually a string array, but the caller just passes strings:

```java
printAll( "Freeman" );
printAll( "Freeman", "Hardy", "Willis" );
```


To avoid ambiguity, only the last argument to a function can have varargs.

* Calling a function with named arguments
Not directly possible, but you could simulate this (somewhat verbosely):

```java
int myMethod( Map<String,Object> params ){
    return
       ((Integer)params.get("x")).intValue()
       + ((Integer)params.get("y")).intValue();
}
```


Called like this:

```java
System.out.println( myMethod(new HashMap<String,Object>(){{put("x",27);put("y",52);}}) );
```


Yuk.

* Using a function in statement context
If this means "use a function where a statement is expected", see all the other examples

* Using a function in first-class context within an expression
Not possible - must be wrapped in a class

* Obtaining the return value of a function

```java
int i = myMethod(x);
```


* Distinguishing built-in functions and user-defined functions
No distinction - all methods belong to classes, and there is no real distinction between built-in and user-defined classes.

* Distinguishing subroutines and functions
If the return type is void, you might consider a method as a subroutine rather than a function.

* Stating whether arguments are passed by value or by reference
All arguments are passed by value, but since object variables contain a reference to an object (not the object itself), objects appear to be passed by reference. For example:

```java
myMethod(List<String> list){
    // If I change the contents of the list here, the caller will see the change
}
```


* Is partial application possible and how
Don't know


## JavaScript


The arguments to a JavaScript function are stored in a special array-like object which does not enforce arity in any way; a function declared to take ''n'' arguments may be called with none‒and vice versa‒without raising an error.


```JavaScript
var foo = function() { return arguments.length };
foo() // 0
foo(1, 2, 3) // 3
```


Neither optional (see above) nor named arguments are supported, though the latter (and the inverse of the former) may be simulated with the use of a helper object to be queried for the existence and/or values of relevant keys. <span style="color: transparent;">Seriously, what is "statement context"?</span>

JavaScript functions are first-class citizens; they can be stored in variables (see above) and passed as arguments.

```JavaScript
var squares = [1, 2, 3].map(function (n) { return n * n }); // [1, 4, 9]
```


Naturally, they can also be returned, thus partial application is supported.

```JavaScript

var make_adder = function(m) {
    return function(n) { return m + n }
};
var add42 = make_adder(42);
add42(10) // 52
```


Calling a user-defined function's <tt>toString()</tt> method returns its source verbatim; that the implementation is elided for built-ins provides a mechanism for distinguishing between the two.


```JavaScript
foo.toString()
"function () { return arguments.length }"
alert.toString()
"function alert() { [native code] }"
```


Arguments are passed by value, but the members of collections are essentially passed by reference and thus propagate modification.

```JavaScript
var mutate = function(victim) {
    victim[0] = null;
    victim = 42;
};
var foo = [1, 2, 3];
mutate(foo) // foo is now [null, 2, 3], not 42
```



## jq

jq functions are pure functions that are somewhat unusual in two respects:

* They are like commands in modern operating systems, in that they are <tt>parameterized filters</tt> that can accept input from the previous command and provide output to the next command if there is one in the pipeline of commands.

* Functions can not only process a stream of inputs, one at a time, but each argument can also accept a stream of inputs. The outputs are then a Cartesian product of the various inputs.

In this section, we use the notation fn/N to refer to a function named "fn" with N formal parameters.

'''Calling a function that requires no arguments'''

0-arity jq functions are invoked simply by specifying their name.
* Example: .
(Yes, "." is a 0-arity jq function.)

'''Calling a function with a fixed number of arguments'''

The conventional syntax is used except that ";" is the parameter separator. "," is used to construct a stream of values.
* Example: range(0;100;2)

'''Calling a function with optional arguments'''

Recent versions of jq allow one to define functions with the same name but different arities, and therefore if both fn/1 and fn/2 are defined, we may say that fn requires one parameter but accepts 2.  In all cases, the  syntax for function invocation is the same.
* Example: range(0; 10) and range(0; 10; 2)

Since jq functions can accept JSON arrays and objects, there are other ways to simulate optional arguments.

'''Calling a function with a variable number of arguments'''

See above.

'''Calling a function with named arguments'''

This is not directly supported but can be simulated by defining the function to accept JSON objects.  For example, if fn were such a function,
we might invoke fn like so: <tt>fn( {"required": 1, "optional": 2} )</tt>.

'''Using a function in statement context'''

The assignment to a local variable (e.g. <tt>(2*2) as $two<tt>) is similar to a statement context in that the expression as a whole does nothing to the flow of values from its input to its output.

'''Using a function in first-class context within an expression'''

jq functions cannot be assigned to variables but are otherwise "first-class" in that the composition of functions can be passed as arguments to other functions.  No special syntax is required.
* Example: <tt>2 | recurse(. * .)</tt> # generate the sequence 2, 4, 16, 256, ...

'''Obtaining the return value of a function'''

The value (or stream of values) returned by a function is (or are) automatically available to the next function in the pipeline (e.g. sin | cos); the returned value(s) can also be assigned to a local variable (e.g. sin as $v).

'''Distinguishing built-in functions and user-defined functions'''

Currently there is no such distinction, but user-defined functions always have the conventional form, whereas some built-in functions have special syntax.

'''Distinguishing subroutines and functions'''

A jq function can be written so as never to return anything (either because it returns without generating a value or because it generates an infinite stream of values), but there is no distinctive marker associated with such functions.

'''Stating whether arguments are passed by value or by reference'''

Arguments are in effect passed by value.

'''Is partial application possible and how'''

See [[Currying#jq]].


## Julia


```Julia

#  Calling a function that requires no arguments:
f() = print("Hello world!")
f()


#  Calling a function with a fixed number of arguments:
function f(x, y, z)
    x*y - z^2
end

f(3, 4, 2)


#  Calling a function with optional arguments:
#  Note Julia uses multiple dispatch based on argument number and type, so
# f() is always different from f(x) unless default arguments are used, as in:

pimultiple(mult=1.0) = pi * mult # so pimultiple() defaults to pi * (1.0) or pi


#  Calling a function with a variable number of arguments:

f(a,b,x...) = reduce(+, 0, x) - a - b


# here a and b are single arguments, but x is a tuple of x plus whatever follows x, so:
a = b = c = d = e = 3
f(a,b,c)           # x within the function is (c) so == 0 + c - a - b
f(a,b,c,d,e)      # x is a tuple == (c,d,e) so == (0 + c + d + e) - a - b
f(a,b)             # x is () so == 0 - a - b


#  Calling a function with named arguments:
# Functions with keyword arguments are defined using a semicolon in the function signature,
#  as in
#             function plot(x, y; style="solid", width=1, color="black")
#
# When the function is called, the semicolon is optional, so plot here can be
# either called with plot(x, y, width=2) or less commonly as plot(x, y; width=2).


#  Using a function in statement context:
#  Any function can be used as a variable by its name.

circlearea(x) = x^2 * pi
map(circlearea, [r1, r2, r3, r4])


#  Using a function in first-class context within an expression:
cylindervolume = circlearea(r) * h


#  Obtaining the return value of a function:
radius = 2.5
area = circlearea(2.5)


#  Distinguishing built-in functions and user-defined functions:
#  Julia does not attempt to distinguish these in any special way,
#  but at the REPL command line there is ? help available for builtin
#  functions that would not generally be available for the user-defined ones.


#  Distinguishing subroutines and functions:
#  All subroutines are called functions in Julia, regardless of whether they return values.


#  Stating whether arguments are passed by value or by reference:
#  As in Python, all arguments are passed by pointer reference, but assignment to a passed argument
#  only changes the variable within the function. Assignment to the values referenced by the argument
## DOES however change those values. For instance:

a = 3
b = [3]
c = [3]

function f(x, y)
    a = 0
    b[1] = 0
    c = [0]
end # a and c are now unchanged but b = [0]


#  Is partial application possible and how:
#  In Julia, there are many different ways to compose functions. In particular,
#  Julia has an "arrow" operator -> that may be used to curry other functions.

f(a, b) = a^2 + a + b
v = [4, 6, 8]
map(x -> f(x, 10), v)  # v = [30, 52, 82]

```



## Kotlin

In Kotlin parameters are always passed by value though, apart from the (unboxed) primitive types, the value passed is actually a reference to an object.

```scala
// version 1.0.6

fun fun1() = println("No arguments")

fun fun2(i: Int) = println("One argument = $i")

fun fun3(i: Int, j: Int = 0) = println("One required argument = $i, one optional argument = $j")

fun fun4(vararg v: Int) = println("Variable number of arguments = ${v.asList()}")

fun fun5(i: Int) = i * i

fun fun6(i: Int, f: (Int) -> Int) = f(i)

fun fun7(i: Int): Double = i / 2.0

fun fun8(x: String) = { y: String -> x + " " + y }

fun main(args: Array<String>) {
    fun1()              // no arguments
    fun2(2)             // fixed number of arguments, one here
    fun3(3)             // optional argument, default value used here
    fun4(4, 5, 6)       // variable number of arguments
    fun3(j = 8, i = 7)  // using named arguments, order unimportant
    val b = false
    if (b) fun1() else fun2(9)        // statement context
    println(1 + fun6(4, ::fun5) + 3)  // first class context within an expression
    println(fun5(5))    // obtaining return value
    println(Math.round(2.5)) // no distinction between built-in and user-defined functions, though former usually have a receiver
    fun1()              // calling sub-routine which has a Unit return type by default
    println(fun7(11))   // calling function with a return type of Double (here explicit but can be implicit)
    println(fun8("Hello")("world"))   // partial application isn't supported though you can do this
}
```


{{out}}

```txt

No arguments
One argument = 2
One required argument = 3, one optional argument = 0
Variable number of arguments = [4, 5, 6]
One required argument = 7, one optional argument = 8
One argument = 9
20
25
3
No arguments
5.5
Hello world

```



## LFE


'''Calling a function that requires no arguments:'''

In some module, define the following:

```lisp

(defun my-func()
  (: io format '"I get called with NOTHING!~n"))

```


Then you use it like so (depending upon how you import it):

```lisp

> (my-func)
I get called with NOTHING!
ok

```


'''Calling a function with a fixed number of arguments:'''
In some module, define the following:

```lisp

(defun my-func(a b)
  (: io format '"I got called with ~p and ~p~n" (list a b)))

```


Then you use it like so:

```lisp

> (my-func '"bread" '"cheese")
I got called with "bread" and "cheese"
ok

```


'''Calling a function with optional arguments or calling a function with a variable number of arguments:'''

* Arguments are fixed in LFE/Erlang functions.
* One can have a dictionary, record, or list be the function argument, and use that to achieve something like variable/optional (and named) arguments.
* One can define multiple functions so that it ''appears'' that one is calling a function with optional or a variable number of arguments:


```lisp

(defmodule args
  (export all))

(defun my-func ()
  (my-func () () ()))

(defun my-func (a)
  (my-func a () ()))

(defun my-func (a b)
  (my-func a b ()))

(defun my-func (a b c)
  (: io format '"~p ~p ~p~n" (list a b c)))

```


Here is some example usage:

```lisp

> (slurp '"args.lfe")
#(ok args)
> (my-func)
[] [] []
ok
> (my-func '"apple")
"apple" [] []
ok
> (my-func '"apple" '"banana")
"apple" "banana" []
ok
> (my-func '"apple" '"banana" '"cranberry")
"apple" "banana" "cranberry"
ok
> (my-func '"apple" '"banana" '"cranberry" '"bad arg")
exception error: #(unbound_func #(my-func 4))

```


'''Calling a function with named arguments:'''

* LFE/Erlang doesn't support named arguments, per se.
* However, by using atoms in function argument patterns (a fairly common pattern), one can achieve similar effects.
* One may also use records or dicts as arguments to achieve similar effects.


'''Using a function in statement context:'''

```lisp

...
  (cond ((== count limit) (hit-limit-func arg-1 arg-2))
        ((/= count limit) (keep-going-func count)))
  ...

```


'''Using a function in first-class context within an expression:'''

From the LFE REPL:

```lisp

> (>= 0.5 (: math sin 0.5))
true

```


'''Obtaining the return value of a function:'''

There are many, many ways to assign function outputs to variables in LFE. One fairly standard way is with the <code>(let ...)</code> form:

```lisp

(let ((x (: math sin 0.5)))
  ...)

```


'''Distinguishing built-in functions and user-defined functions:'''

* There is no distinction made in LFE/Erlang between functions that are built-in and those that are not.
* "Built-in" for LFE/Erlang usually can be figured out: if a function has the module name <code>erlang</code>, e.g., <code>(: erlang list_to_integer ... )</cod>, then it's built-in.
* Most of the functions that come with LFE/Erlang are not even in the <code>erlang</code> module, but exist in other modules (e.g., <code>io</code>, <code>math</code>, etc.) and in OTP.
* One uses user/third-party modules in exactly the same way as one uses built-ins and modules that come with the Erlang distribution.


'''Distinguishing subroutines and functions:'''

* One commonly made distinction between functions and subroutines is that functions return a value (or reference, etc.) and subroutines do not.
* By this definition, LFE/Erlang does not support the concept of a subroutine; all functions return something.


'''Stating whether arguments are passed by value or by reference:'''

* Arguments and returns values are passed by reference in LFE/Erlang.


'''Is partial application possible?'''

* Not explicitly.
* However, one can use <code>lambda</code>s to achieve the same effect.


## Liberty BASIC


```lb

'Call a function - Liberty BASIC

'First, function result could not be discarded
' that is, you cannot do "f(x)" as a separate statement

'Calling a function that requires no arguments
res = f()   'brackets required

'Calling a function with a fixed number of arguments
res = g(x)
res = h(x,y)
'Calling a function with optional arguments
    'impossible for user-defined functions
    'Some build-in functions ex. INSTR and MID$ could be called with last argument omitted
'Calling a function with a variable number of arguments
    'impossible
'Calling a function with named arguments
    'impossible
'Using a function in statement context
    'impossible (see starting notice)
'Using a function in first-class context within an expression
    'impossible
'Obtaining the return value of a function
res = g(x)
'Distinguishing built-in functions and user-defined functions
    'I would say impossible. Though built-in functions could be EVAL'ed,
    'while user-defined would not be called (tries address array instead).
    'Still cannot distinguish user-defined function from array.
'Distinguishing subroutines and functions
    'then defined, subroutines and functions defined with words
    'SUB and FUNCTION (case incensitive)
    'Then used, function used as expression (with return value),
    res = g(x)
    'while subroutines called with special keyword CALL and without brackets
    call test x, y
'Stating whether arguments are passed by value or by reference
    'Variables passed as arguments into functions and subs are passed "by value" by default
    'parameters could be passed "by reference" if formal parameter in sub/function definition uses the "byref" specifier
    'Then calling a function, you can prevent pass by reference by changing variable to expression
    '   like x+0, x$+"" or just (x), (x$)
'Is partial application possible and how
    'impossible

```



## Lingo


*Calling a function that requires no arguments

```lingo
foo()
-- or alternatively:
call(#foo, _movie)
```


*Calling a function with a fixed number of arguments

```lingo
foo(1,2,3)
-- or alternatively:
call(#foo, _movie, 1, 2, 3)
```


*Calling a function with optional arguments

```lingo
on foo (a, b)
  if voidP(b) then b = 1
  return a * b
end
```


```lingo
put foo(23, 2)
-- 46
put foo(23)
-- 23
```


*Calling a function with a variable number of arguments

```lingo
on sum ()
  res = 0
  repeat with i = 1 to the paramCount
    res = res + param(i)
  end repeat
  return res
end
```


```lingo
put sum (1,2,3)
-- 6
```


*Calling a function with named arguments
Not directly supported, but you can of course re-write any function to only accept a single property list (hash) as argument, which can make sense e.g. for functions that have a lot of optional aruments.

*Using a function in statement context
*Using a function in first-class context within an expression
Lingo has no first-class functions, but the call(...) syntax (see above) allows to identify and use functions specified as "symbols" (e.g. #foo). This allows some "first-class alike" features:

```lingo
----------------------------------------
-- One of the five native iterative methods defined in ECMAScript 5
-- @param {list} tList
-- @param {symbol} cbFunc
-- @param {object} [cbObj=_movie]
-- @return {list}
----------------------------------------
on map (tList, cbFunc, cbObj)
  if voidP(cbObj) then cbObj = _movie
  res = []
  cnt = tList.count
  repeat with i = 1 to cnt
    res[i] = call(cbFunc, cbObj, tList[i], i, tList)
  end repeat
  return res
end

on doubleInt (n)
  return n*2
end
```


```lingo
l = [1,2,3]
put map(l, #doubleInt)
-- [2, 4, 6]
```


*Obtaining the return value of a function

```lingo
x = foo(1,2)
```


*Distinguishing built-in functions and user-defined functions
In Lingo all user-defined (global) functions are 'methods' of the _movie object, and there is AFAIK no direct way to distinguish those from _movie's built-in functions. But by iterating over of all movie scripts in all castlibs you can get a complete list of all user-defined (global) functions, and then any function not in this list is a built-in function:

```lingo
on getAllUserFunctions ()
  res = []
  repeat with i = 1 to _movie.castlib.count
    c = _movie.castlib(i)
    repeat with j = 1 to c.member.count
      m = c.member[j]
      if m.type<>#script then next repeat
      if m.scripttype=#movie then
        functions = m.script.handlers()
        repeat with f in functions
          res.append(f)
        end repeat
      end if
    end repeat
  end repeat
  return res
end
```


```lingo
put getAllUserFunctions()
-- [#sum, #double, #getAllUserFunctions]
```


*Distinguishing subroutines and functions
In Lingo functions (also called "handlers") don't have to return anything, so there is no distinction. The return value of a function without a "return ..." line is VOID, and not distinguishable from a function with the explicit line "return VOID".

*Stating whether arguments are passed by value or by reference
In lingo 'objects' are always passed by reference, all other types (e.g. strings, integers, floats) by value. 'Objects' are e.g. lists (arrays), property lists (hashes), images and script instances. The built-in function  objectP() returns TRUE (1) for objects and FALSE (0) for non-objects. To prevent the effects of call-by-reference, some object types (lists, property lists and images) support the method duplicate() to clone the object before passing it to a function:

```lingo
on double (someList)
  cnt = someList.count
  repeat with i = 1 to cnt
    someList[i] = someList[i] * 2
  end repeat
end
```


```lingo
l = [1,2,3]
double(l)
put l
-- [2, 4, 6]

l = [1,2,3]
double(l.duplicate())
put l
-- [1, 2, 3]
```



## Little


The following examples use buldin functions, standard Tcl commands and some
local fuctions.


```C
// Calling a function that requires no arguments
void foo() {puts("Calling a function with no arguments");}
foo();

// Calling a function with a fixed number of arguments
abs(-36);

// Calling a function with optional arguments
puts(nonewline: "nonewline is an optional argument");
puts("\n");

// Calling a function with a variable number of arguments
void var_arg_func(...args) {
    puts(length(args));
}
var_arg_func(1, 2);
var_arg_func(1, 2, 3);

// Obtaining the return value of a function
int s = clock("seconds"); //current time in seconds
// Calling a function with named arguments
// format is a named argument in Clock_format
int str = Clock_format(s, format: "%B");
puts(str);

// Stating whether arguments are passed by value or by reference
void f(int a, int &b) { a++; b++; }
{
int a = 0;
int b = 0;

f(a, &b);
puts (a);
puts (b);
}
```



## Lua


```lua
-- Lua functions accept any number of arguments; missing arguments are nil-padded, extras are dropped.
function fixed (a, b, c) print(a, b, c) end
fixed() --> nil nil nil
fixed(1, 2, 3, 4, 5) --> 1 2 3

-- True vararg functions include a trailing ... parameter, which captures all additional arguments as a group of values.
function vararg (...) print(...) end
vararg(1, 2, 3, 4, 5) -- 1 2 3 4 5

-- Lua also allows dropping the parentheses if table or string literals are used as the sole argument
print "some string"
print { foo = "bar" } -- also serves as a form of named arguments

-- First-class functions in expression context
print(("this is backwards uppercase"):gsub("%w+", function (s) return s:upper():reverse() end))

-- Functions can return multiple values (including none), which can be counted via select()
local iter, obj, start = ipairs { 1, 2, 3 }
print(select("#", (function () end)())) --> 0
print(select("#", unpack { 1, 2, 3, 4 })) --> 4

-- Partial application
function prefix (pre)
    return function (suf) return pre .. suf end
end

local prefixed = prefix "foo"
print(prefixed "bar", prefixed "baz", prefixed "quux")

-- nil, booleans, and numbers are always passed by value. Everything else is always passed by reference.
-- There is no separate notion of subroutines
-- Built-in functions are not easily distinguishable from user-defined functions

```



## Luck


```luck
/* Calling a function that requires no arguments */
f();;

/* Calling a function with a fixed number of arguments */
f(1,2);;

/* Calling a function with optional arguments
   Note: defining the function is cumbersome but will get easier in future versions. */
f(1,2,new {default with x=3, y=4});;

/* Calling a function with a variable number of arguments */
printf("%d %d %d %d":char*,2,3,4,5);;

/* Calling a function with named arguments
   Note: may get syntax sugar in future versions */
f(1,2,new {default with x=3, y=4});;

/* Using a function in statement context (what?) */
f();f();f();;

/* Using a function in first-class context within an expression */
[1,2,3].map(string);;

/* Obtaining the return value of a function */
let x:int = f();;

/* Distinguishing built-in functions and user-defined functions */
/* Builtin function i.e. custom calling convention: */
(@ binop "==" l r);;
/* User defined function i.e. normal function */
f(l)(r);;

/* Distinguishing subroutines and functions: both are supported, but compiler is not aware of difference */
sub();;
fun();;

/* Stating whether arguments are passed by value or by reference */
f(value);; /* by value */
f(&value);; /* by pointer reference */
f(ref(value));; /* by managed reference */

/* Is partial application possible and how */
tasty_curry(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)(m)(n)(o)(p)(q)(r)(s)(t)(u)(v)(w)(x)(y)(z);;
```



## M2000 Interpreter


### Standard Call of Modules/Functions

In M2000 we can use arrays, functions and subroutines with same name. Using @ we can direct interpreter to use function. Using Gosub we direct interpreter to call a subroutine. These happen at module/function level.
A Subroutine is code inside Modules/Functions where all definitions in module/function are visible. Modules and functions can call own modules/functions or global modules/functions. Functions can use recursion. Module with standard calling can't use recursion (need to use Call nameOfModule to call itself).



```M2000 Interpreter

Module CheckIt {
            Dim a(4)=100
            Def a(x)=x**2
            Print a(2), a(@2)
            Gosub a(4)
            Print "End"
            Sub a(x)
                  Print "This is sub a()", x
            End Sub
}
CheckIt
Call CheckIt
\\ both module and function can exist with same name
Function CheckIt {
            Dim a(4)=100
            Def a(x)=x**2
            Print a(2), a(@2)
            Gosub a(4)
            Print "End"
            Sub a(x)
                  Print "This is sub a()", x
            End Sub
}
Call CheckIt()
Call Function Checkit
\\ if a function return a non zero number then we get error with value the returned number. Using Void we drop any return value, so no error happen.
Call Void CheckIt()
Call Void Function Checkit
\\ subs are part of modules/functions (there are no global subs, but there is a way to share definitions modules from parent module).
Module CheckSub {
      M=1
      a(100)   ' 400
      a(100)  ' 800
      Module Child {
            M=1
            a(100) ' 400
            a(100) ' 800
      }
      Child
      Sub a(x)
            b(x*4)
            M++
      End Sub
      Sub b(x)
            Print x*M
      End Sub
}
CheckSub


```



### Call Local

Using standard call, a call to a module or a function open a new namespace, so we can use local variables. But what if we want to call a function as a subroutine, where the namespace is the same as the module/function where we define it.
So we have to ''read new'' to make K new, not reading the same K in module Checkit. We have to use Local N to shadow N in module Checkit. After the call to Alfa, all new definitions erased. The sane hold for call Checkit, anything defined there erased after the call. Only Stack return. A Call Local use new stack, the same as the calling of function in expression (a standard Call to module or function, using Call statement use parent stack)


```M2000 Interpreter

Module CheckIt {
      M=100
      K=5
      N=200
      Function Alfa {
            Read New K
            Local N=3
            M++
            Print M*K/3
      }
      Call Local Alfa(30)   ' print 1010
      Print M=101, K=5, N=200
}
CheckIt

```



### Using Event Object

We have to define the type of arguments to event object. We can add functions to event and we can remove them, except one internal (optional defined using Function {} without a read command)

```M2000 Interpreter

Module CheckIt  (&P){
      Event Alfa {
            Read X, &M
            Function {
                  Print X, M
            }
      }
      Function Other (a, &b) {
            b++
            Print a*b
      }
      Event Alfa New &Other()
      Call Event Alfa, 3, &P
      \\ Print  3 10    \\ form internal function in Event Alfa
      \\ Print 33  \\ from Other()
      Print P=11
      Push ALfa
}
M=10
Checkit &M
Read ReturnedEventObject
Call Event ReturnedEventObject, 2, &M
\\ Print  2 11    \\ form internal function in Event Alfa
\\ Print 24  \\ from Other(), which is a closure to event
Print M=12

```




### Call a reference to Function


```M2000 Interpreter

Module Checkit {
      Group Alfa {
            x=10
            Function Beta {
                  =.x
                  .x++
            }
      }
      Module PassRef (&n()) {
            Print n()
      }
      PassRef &Alfa.Beta()
      Print Alfa.x=11
}
Call Checkit

```



### Light Event Function


```M2000 Interpreter

Module Checkit {
      Group WithEvents Alfa {
            Event "AddOne"
            x=10
            Function Beta {
                  =.x
                  .PrintIt .x+1
                  .x++
            }
            Module PrintIt (x) {
                  Call Event "AddOne", x
            }
      }
      Module PassRef (&n()) {
            z=n()
      }
      z=500
      k=0
      Function Alfa_AddOne (new z) {
      \\ interpreter make a line: Read New Z
            k+=z
            Print z
      }
      PassRef &Alfa.Beta()
      Print k=11, z=500
}
Call Checkit

```



## Maple

Calling a function with no arguments:
```Maple
 f()
```

Calling a function with a fixed number of arguments:
```Maple
f(1,sin(x), g -> int(g(t),t=0..1)
```

Calling a function with optional arguments:
```Maple
f(1, sin(x), g -> int(g(t),t=0..1)
```

Calling a function with a variable number of arguments:
```Maple
f(1, sin(x), g -> int(g(t),t=0..1)
```

Calling a function with named arguments:
```Maple
f(a,b,method = foo)
```

Calling a function in a statements context:
```Maple
f(a); f(b);
```

Using a function in first-class context within an expression:
```Maple
f(a) + g(b)
```

Obtaining the return value of a function:
```Maple
 x := f(1)
```

Distinguishing built-in functions and user-defined functions:

```Maple>
 type( op, 'builtin' );
                       true

```

Distinguishing subroutines and functions: There is no distinction.

Stating whether arguments are passed by value or by reference: All values are passed by value.
However, if an argument is a name, then it can be assigned to and, if a value is mutable (such as an array), then it can be modified by the function.  This is implemented by function definition; there is no distinction when calling the function.

Partial application is supported by the <code>curry</code> and <code>rcurry</code> commands.

=={{header|Mathematica}} / {{header|Wolfram Language}}==
Calling a function that requires no arguments:

```Mathematica
f[]
```


Calling a function with a fixed number of arguments:

```Mathematica
f[1,2]
```


Calling a function with optional arguments:

```Mathematica
f[1,Option1->True]
```


Calling a function with a variable number of arguments:

```Mathematica
f[1,Option1->True]
f[1,Option1->True,Option2->False]
```


Calling a function with named arguments:

```Mathematica
f[Option1->True,Option2->False]
```


Using a function in statement context:

```Mathematica
f[1,2];f[2,3]
```


Using a function in first-class context within an expression:

```Mathematica
(#^2)&[3];
```


The return value of a function can be formally extracted using Return[]
Built-in functions names by convention start with a capital letter.
No formal distinction between subroutines and functions.
Arguments can be passed by value or by reference.

=={{header|MATLAB}} / {{header|Octave}}==

```Matlab

    % Calling a function that requires no arguments
       function a=foo();
         a=4;
       end;
       x = foo();
    % Calling a function with a fixed number of arguments
       function foo(a,b,c);
         %% function definition;
       end;
       foo(x,y,z);
    % Calling a function with optional arguments
       function foo(a,b,c);
	if nargin<2, b=0; end;
	if nargin<3, c=0; end;
         %% function definition;
       end;
       foo(x,y);
    % Calling a function with a variable number of arguments
       function foo(varargin);
	  for k=1:length(varargin)
            arg{k} = varargin{k};
       end;
       foo(x,y);
    % Calling a function with named arguments
	%% does not apply
    % Using a function in statement context
	%% does not apply
    % Using a function in first-class context within an expression
    % Obtaining the return value of a function
       function [a,b]=foo();
         a=4;
         b='result string';
       end;
       [x,y] = foo();
    % Distinguishing built-in functions and user-defined functions
	fun = 'foo';
	if (exist(fun,'builtin'))
 		printf('function %s is a builtin\n');
        elseif (exist(fun,'file'))
 		printf('function %s is user-defined\n');
        elseif (exist(fun,'var'))
 		printf('function %s is a variable\n');
        else
 		printf('%s is not a function or variable.\n');
        end
    % Distinguishing subroutines and functions
        % there are only scripts and functions, any function declaration starts with the keyword function, otherwise it is a script that runs in the workspace
    % Stating whether arguments are passed by value or by reference
      % arguments are passed by value, however Matlab has delayed evaluation, such that a copy of large data structures are done only when an element is written to.

```



## Nemerle


```Nemerle
// no arguments
f()

// fixed arguments
def f(a, b) { ... } // as an aside, functions defined with 'def' use type inference for parameters and return types
f(1, 'a')

// optional arguments
def f(a, b = 0) { ... }
f("hello")
f("goodbye", 2)
f("hey", b = 2) // using the name makes more sense if there's more than one optional argument, obviously

// variable number of arguments
def f(params args) { ... }
def g(a, b, params rest) { ... }
f(1, 2, 3) // arguments should all have the same type or may be coerced to a supertype
g(1.0, 2, "a", "hello")

// named arguments
f(a = 'a', b = 0)
f(b = 0, a = 'a')
f('a', b = 0) // if mixing named and unnamed args, unnamed must be first and in correct order

// statement context
if (f(foo) == 42)
    WriteLine($"$foo is the meaning to life, the universe and everything.")
else WriteLine($"$foo is meaningless.")

// first class function in an expression
def a = numList.FoldLeft(f)

// obtaining return value
def a = f(3)

// distinguishing built-in from user functions
//   N/A?

// distinguishing subroutines from functions
//   N/A

// stating whether passed by value or by reference
//   .NET distinguishes between value types and reference types; if a reference type is passed by reference (using ref or out),
//   the reference is passed by reference, which would allow a method to modify the object to which the reference refers
def f(a, ref b) { ... }
mutable someVar = "hey there" // doesn't make sense to pass immutable value by ref
f(2, ref someVar)
def g(a, out b) { ... }
mutable someOtherVar // if passed by ref using 'out', the variable needn't be initialized
g(2, out someOtherVar)

// partial application
def f(a, b) { ... }
def g = f(2, _)
def h = f(_, 2)
def a = g(3) // equivalent to: def a = f(2, 3)
def b = h(3) // equivalent to: def b = f(3, 2)
```



## Nim

Translated from Python, when possible:

```nim
proc no_args() =
  discard
# call
no_args()

proc fixed_args(x, y) =
  echo x
  echo y
# calls
fixed_args(1, 2)        # x=1, y=2
fixed_args 1, 2         # same call
1.fixed_args(2)         # same call


proc opt_args(x=1.0) =
  echo x
# calls
opt_args()              # 1
opt_args(3.141)         # 3.141

proc var_args(v: varargs[string, `$`]) =
  for x in v: echo x
# calls
var_args(1, 2, 3)       # (1, 2, 3)
var_args(1, (2,3))      # (1, (2, 3))
var_args()              # ()

## Named arguments
fixed_args(y=2, x=1)    # x=1, y=2

## As a statement
if true:
  no_args()

proc return_something(x): int =
  x + 1

var a = return_something(2)

## First-class within an expression
let x = return_something(19) + 10
let y = 19.return_something() + 10
let z = 19.return_something + 10
```



## OCaml


* Calling a function that requires no arguments:


```ocaml
f ()
```


(In fact it is impossible to call a function without arguments, when there are no particular arguments we provide the type <code>unit</code> which is a type that has only one possible value. This type is mainly made for this use.)

* Calling a function with a fixed number of arguments:


```ocaml>f 1 2 3</lang


* Calling a function with optional arguments:

For a function that has this signature:


```ocaml
val f : ?a:int -> int -> unit
```


here is how to call it with or without the first argument omited:


```ocaml
f 10
f ~a:6 10
```


Due to partial application, an optional argument always has to be followed by a non-optional argument. If the function needs no additional arguments then we use the type <code>unit</code>:


```ocaml
g ()
g ~b:1.0 ()
```


* Calling a function with a variable number of arguments:

This is not possible. The strong OCaml type system does not allow this.
The OCaml programmer will instead provide the variable number of arguments in a list, an array, an enumeration, a set or any structure of this kind.
(But if we really need this for a good reason, it is still possible to use a hack, like it has been done for the function <code>Printf.printf</code>.)

* Calling a function with named arguments:

Named arguments are called '''labels'''.


```ocaml
f ~arg:3
```


If a variable has the same name than the label we can use this simpler syntax:


```ocaml
let arg = 3 in
f ~arg
```


* Using a function in statement context:


```ocaml
(* TODO *)
```


* Using a function in first-class context within an expression:

functions in OCaml are first-class citizen.

* Obtaining the return value of a function:


```ocaml
let ret = f ()
let a, b, c = f ()  (* if there are several returned values given as a tuple *)
let _ = f ()        (* if we want to ignore the returned value *)
let v, _ = f ()     (* if we want to ignore one of the returned value *)
```


* Distinguishing built-in functions and user-defined functions:

There is no difference.

* Distinguishing subroutines and functions:

OCaml only provides functions.

* Stating whether arguments are passed by value or by reference:

OCaml arguments are always passed by reference. OCaml is an impure functional language, for immutable variables there is no difference if the argument is passed by value or by reference, but for mutable variables the programmer should know that a function is able to modify it.

* How to use partial application:

Just apply less arguments than the total number of arguments.

With partial application, the arguments are applied in the same order than they are defined in the signature of the function, except if there are labeled arguments, then it is possible to use these labels to partially apply the arguments in any order.


## Oforth


Oforth provides functions (global prodecure, without receiver) and methods (need a receiver).

Oforth uses RPN notation. Arguments must be on the stack before calling a function or method. So, the same syntax is use for calling a function with or without prameters.

If f is a function and c b a ares objects :

```Oforth>a b c f</lang

will push c then b then a on the stack then call f. Calling f does not describe if f will use 1, 2 or 3 arguments (or none).

Oforth adds a notation to describe parameters used by a function. It is only a way to add information about which parameters will be used by f :

```Oforth
f(a, b, c)
```


Intepreter will replace this second syntax by the first one. It is only "sugar"...


```Oforth
a b c f
a b f(c)
a f(b, c)
f(a, b, c)
```


are the same call to function f and the interpreter will translate all of them into the first one. Which parameters are really used by f will depend on f implementation.

Methods need a receiver (the object on which the method will apply and the object that will pushed on th stack when self is used into the method body).
The receiver must be on the top of the stack before calling the method. If a, b, c and r are objects and m a method :

```Oforth>a b c r m</lang

will call m with r as its receiver.
It is also possible to use the same "sugar" notation used by functions :

```Oforth
r m(a, b, c)
```



## Ol


```scheme

; note: sign "==>" indicates expected output

;;; Calling a function that requires no arguments
(define (no-args-function)
   (print "ok."))

(no-args-function)
; ==> ok.


;;; Calling a function with a fixed number of arguments
(define (two-args-function a b)
   (print "a: " a)
   (print "b: " b))

(two-args-function 8 13)
; ==> a: 8
; ==> b: 13


;;; Calling a function with optional arguments
(define (optional-args-function a . args)
   (print "a: " a)
   (if (null? args)
      (print "no optional arguments"))
   (if (less? 0 (length args))
      (print "b: " (car args)))
   (if (less? 1 (length args))
      (print "c: " (cadr args)))
   ; etc...
)

(optional-args-function 3)
; ==> a: 3
; ==> no optional arguments
(optional-args-function 3 8)
; ==> a: 3
; ==> b: 8
(optional-args-function 3 8 13)
; ==> a: 3
; ==> b: 8
; ==> c: 13
(optional-args-function 3 8 13 77)
; ==> a: 3
; ==> b: 8
; ==> c: 13


;;; Calling a function with a variable number of arguments
; /same as optional arguments


;;; Calling a function with named arguments
; /no named arguments "from the box" is provided, but it can be easily simulated using builtin associative arrays (named "ff")
(define (named-args-function args)
   (print "a: " (get args 'a 8)) ; 8 is default value if no variable value given
   (print "b: " (get args 'b 13)); same as above
)

(named-args-function #empty)
; ==> a: 8
; ==> b: 13
(named-args-function (list->ff '((a . 3))))
; ==> a: 3
; ==> b: 13
; or nicer (and shorter) form available from ol version 2.1
(named-args-function '{(a . 3)})
; ==> a: 3
; ==> b: 13
(named-args-function '{(b . 7)})
; ==> a: 8
; ==> b: 7
(named-args-function '{(a . 3) (b . 7)})
; ==> a: 3
; ==> b: 7


;;; Using a function in first-class context within an expression
(define (first-class-arg-function arg a b)
   (print (arg a b))
)

(first-class-arg-function + 2 3)
; ==> 5
(first-class-arg-function - 2 3)
; ==> -1

;;; Using a function in statement context
(let ((function (lambda (x) (* x x))))
   (print (function 4))
; ==> 16
;(print (function 4))
; ==> What is 'function'?

;;; Obtaining the return value of a function
(define (return-value-function)
   (print "ok.")
   123)

(let ((result (return-value-function)))
   (print result))
; ==> ok.
; ==> 123

;;; Obtaining the return value of a function while breaking the function execution (for example infinite loop)
(print
   (call/cc (lambda (return)
      (let loop ((n 0))
         (if (eq? n 100)
            (return (* n n)))
         (loop (+ n 1))))))) ; this is infinite loop
; ==> 10000


;;; Is partial application possible and how
(define (make-partial-function n)
   (lambda (x y)
      (print (n x y)))
)

(define plus (make-partial-function +))
(define minus (make-partial-function -))

(plus 2 3)
; ==> 5
(minus 2 3)
; ==> -1

;;; Distinguishing built-in functions and user-defined functions
; ol has no builtin functions but only eight builtin forms: quote, values, lambda, setq, letq, ifeq, either, values-apply.
; all other functions is "user-defined", and some of them defined in base library, for example (scheme core) defines if, or, and, zero?, length, append...

;;; Distinguishing subroutines and functions
; Both subroutines and functions is a functions in Ol.
; Btw, the "subroutine" has a different meaning in Ol - the special function that executes simultaneously in own context. The intersubroutine messaging mechanism is provided, sure.

;;; Stating whether arguments are passed by value or by reference
; The values in Ol always passed as values and objects always passed as references. If you want to pass an object copy - make a copy by yourself.

```



## PARI/GP

Calling a function is done in GP by writing the name of the function and the arguments, if any, in parentheses.  As of version 2.5.0, function calls must use parentheses; some earlier versions allowed functions with an arity of 0 to be called without parentheses.  However built-in constants (which are implicit functions of the current precision) can still be called without parentheses.

Optional arguments can be skipped, leaving commas in place.  Trailing commas can be dropped.

Functions can be used when statements would be expected without change.

```parigp
f(); \\ zero arguments
sin(Pi/2); \\ fixed number of arguments
vecsort([5,6]) != vecsort([5,6],,4) \\ optional arguments
Str("gg", 1, "hh") \\ variable number of arguments
call(Str, ["gg", 1, "hh"]) \\ variable number of arguments in a vector
(x->x^2)(3); \\ first-class
x = sin(0); \\ get function value
```


Built-in functions are like user-defined functions in current versions.  In older versions built-in functions cannot be passed as closures.

Most arguments are passed by reference.  Some built-in functions accept arguments (e.g., flags) that are not <code>GEN</code>s; these are passed by value or reference depending on their [[C]] type.  See the User's Guide to the PARI Library section 5.7.3, "Parser Codes".

=={{Header|Perl}}==
The most common syntax; simply calls the function foo on the argument(s) provided.

```perl
foo();              # Call foo on the null list
&foo();             # Ditto
foo($arg1, $arg2);  # Call foo on $arg1 and $arg2
&foo($arg1, $arg2); # Ditto; ignores prototypes
```

Call foo() as a bareword. Only works after the function has been declared, which
can be done normally or with the use subs pragma.

```perl>foo;</lang

Call foo() with the current values of @_
```perl
&foo;
```

Call foo() with the current values of @_, discarding the previous stack frame. Not your grandfather's (harmful) goto, although the keyword can do both.
```perl
goto &foo;
```

For subroutines stored in references (anonymous subroutines).
```perl
&$fooref('foo', 'bar');
&{$fooref}('foo', 'bar');
$fooref->('foo', 'bar');
```



## Perl 6


### Theory

Fundamentally, nearly everything you do in Perl 6 is a function call if you look hard enough.
At the lowest level, a function call merely requires a reference to any
kind of invokable object, and a call to its <tt>postcircumfix:&lt;( )&gt;</tt> method.
However, there are various forms of sugar and indirection that you
can use to express these function calls differently.  In particular,
operators are all just sugar for function calls.

Calling a function that requires no arguments:


```perl6
foo               # as list operator
foo()             # as function
foo.()            # as function, explicit postfix form
$ref()            # as object invocation
$ref.()           # as object invocation, explicit postfix
&foo()            # as object invocation
&foo.()           # as object invocation, explicit postfix
::($name)()       # as symbolic ref
```


Calling a function with exactly one argument:


```perl6
foo 1             # as list operator
foo(1)            # as named function
foo.(1)           # as named function, explicit postfix
$ref(1)           # as object invocation (must be hard ref)
$ref.(1)          # as object invocation, explicit postfix
1.$foo            # as pseudo-method meaning $foo(1) (hard ref only)
1.$foo()          # as pseudo-method meaning $foo(1) (hard ref only)
1.&foo            # as pseudo-method meaning &foo(1) (is hard foo)
1.&foo()          # as pseudo-method meaning &foo(1) (is hard foo)
1.foo             # as method via dispatcher
1.foo()           # as method via dispatcher
1."$name"()       # as method via dispatcher, symbolic
+1                # as operator to prefix:<+> function
```


Method calls are included here because they do eventually dispatch to a true
function via a dispatcher.  However, the dispatcher in question is not going
to dispatch to the same set of functions that a function call of that name
would invoke.  That's why there's a dispatcher, after all.  Methods are declared
with a different keyword, <tt>method</tt>, in Perl 6, but all that does is
install the actual function into a metaclass.  Once it's there, it's merely
a function that expects its first argument to be the invocant object.  Hence we
feel justified in including method call syntax as a form of indirect function call.

Operators like <tt>+</tt> also go through a dispatcher, but in this case it is
multiply dispatched to all lexically scoped candidates for the function.  Hence
the candidate list is bound early, and the function itself can be bound early
if the type is known.  Perl 6 maintains a clear distinction between early-bound
linguistic constructs that force Perlish semantics, and late-bound OO dispatch
that puts the objects and/or classes in charge of semantics.  (In any case, <tt>&foo</tt>,
though being a hard ref to the function named "foo", may actually be a ref to
a dispatcher to a list of candidates that, when called, makes all the candidates behave as a single unit.)

Calling a function with exactly two arguments:


```perl6
foo 1,2           # as list operator
foo(1,2)          # as named function
foo.(1,2)         # as named function, explicit postfix
$ref(1,2)         # as object invocation (must be hard ref)
$ref.(1,2)        # as object invocation, explicit postfix
1.$foo: 2         # as pseudo-method meaning $foo(1,2) (hard ref only)
1.$foo(2)         # as pseudo-method meaning $foo(1,2) (hard ref only)
1.&foo: 2         # as pseudo-method meaning &foo(1,2) (is hard foo)
1.&foo(2)         # as pseudo-method meaning &foo(1,2) (is hard foo)
1.foo: 2          # as method via dispatcher
1.foo(2)          # as method via dispatcher
1."$name"(2)      # as method via dispatcher, symbolic
1 + 2             # as operator to infix:<+> function
```


Optional arguments don't look any different from normal arguments.
The optionality is all on the binding end.

Calling a function with a variable number of arguments (varargs):


```perl6
foo @args         # as list operator
foo(@args)        # as named function
foo.(@args)       # as named function, explicit postfix
$ref(@args)       # as object invocation (must be hard ref)
$ref.(@args)      # as object invocation, explicit postfix
1.$foo: @args     # as pseudo-method meaning $foo(1,@args) (hard ref)
1.$foo(@args)     # as pseudo-method meaning $foo(1,@args) (hard ref)
1.&foo: @args     # as pseudo-method meaning &foo(1,@args)
1.&foo(@args)     # as pseudo-method meaning &foo(1,@args)
1.foo: @args      # as method via dispatcher
1.foo(@args)      # as method via dispatcher
1."$name"(@args)  # as method via dispatcher, symbolic
@args X @blargs   # as list infix operator to infix:<X>
```

Note: whether a function may actually be called with a variable number of arguments depends entirely
on whether a signature accepts a list at that position in the argument list, but
describing that is not the purpose of this task.  Suffice to say that we assume here that the
foo function is declared with a signature of the form (*@params).  The calls above might be interpreted as having a single array argument if the signature indicates a normal parameter instead of a variadic one.  What you cannot do in Perl 6 (unlike Perl 5) is pass an array as several fixed arguments.  By default it must either represent a single argument, or be part of a variadic list.  You can force the extra level of argument list interpolation using a prefix <tt>|</tt> however:


```perl6
my @args = 1,2,3;
foo(|@args);  # equivalent to foo(1,2,3)
```


Calling a function with named arguments:


```perl6
foo :a, :b(4), :!c, d => "stuff"
foo(:a, :b(4), :!c, d => "stuff")
```


...and so on.  Operators may also be called with named arguments, but only
colon adverbials are allowed:


```perl6
1 + 1 :a :b(4) :!c :d("stuff")   # calls infix:<+>(1,1,:a, :b(4), :!c, d => "stuff")
```


Using a function in statement context:


```perl6
foo(); bar(); baz();    # evaluate for side effects
```


Using a function in first class context within an expression:


```perl6
1 / find-a-func(1,2,3)(4,5,6) ** 2;
```


Obtaining the return value of a function:


```perl6
my $result = somefunc(1,2,3) + 2;
```


There is no difference between calling builtins and user-defined functions and operators (or
even control stuctures).  This was a major design goal of Perl 6, and apart from a very few
low-level primitives, all of Perl 6 can be written in Perl 6.

There is no difference between calling subroutines and functions in Perl 6, other than that
calling a function in void context that has no side effects is likely to get you a "Useless use of..." warning.
And, of course, the fact that pure functions can participate in more optimizations such as constant folding.

By default, arguments are passed readonly, which allows the implementation to decide whether pass-by-reference or pass-by-value is more efficient on a case-by-case basis.  Explicit lvalue, reference, or copy semantics may be requested on a parameter-by-parameter basis, and the entire argument list may be processed raw if that level of control is needed.

### Practice

Demonstrating each of the above-mentioned function calls with actual running code, along with the various extra definitions required to make them work (in certain cases).  Arguments are checked, and function name / run-sequence number are displayed upon success.

```perl6
{
state $n;

multi f ()          {                                    print ' f' ~ ++$n }
multi f ($a)        { die if 1  != $a;                   print ' f' ~ ++$n }
multi f ($a,$b)     { die if 3  != $a+$b;                print ' f' ~ ++$n }
multi f (@a)        { die if @a != [2,3,4];              print ' f' ~ ++$n }
multi f ($a,$b,$c)  { die if 2  != $a || 4 != $c;        print ' f' ~ ++$n }
sub   g ($a,*@b)    { die if @b != [2,3,4] || 1 != $a;   print ' g' ~ ++$n }

my \i = ->          {                                    print ' i' ~ ++$n }
my \l = -> $a       { die if 1 != $a;                    print ' l' ~ ++$n }
my \m = -> $a,$b    { die if 1 != $a || 2 != $b;         print ' m' ~ ++$n }
my \n = -> @a       { die if @a != [2,3,4];              print ' n' ~ ++$n }

Int.^add_method( 'j', method ()
                    { die if 1 != self;                  print ' j' ~ ++$n } );
Int.^add_method( 'k', method ($a)
                    { die if 1 != self || 2 != $a;       print ' k' ~ ++$n } );
Int.^add_method( 'h', method (@a)
                    { die if @a != [2,3,4] || 1 != self; print ' h' ~ ++$n } );

my $ref   =  &f;  # soft ref
my $f    :=  &f;  # hard ref
my $g    :=  &g;  # hard ref
my $f-sym = '&f'; # symbolic ref
my $g-sym = '&g'; # symbolic ref
my $j-sym =  'j'; # symbolic ref
my $k-sym =  'k'; # symbolic ref
my $h-sym =  'h'; # symbolic ref

# Calling a function with no arguments:

f;            #  1  as list operator
f();          #  2  as function
i.();         #  3  as function, explicit postfix form  # defined via pointy-block
$ref();       #  4  as object invocation
$ref.();      #  5  as object invocation, explicit postfix
&f();         #  6  as object invocation
&f.();        #  7  as object invocation, explicit postfix
::($f-sym)(); #  8  as symbolic ref

# Calling a function with exactly one argument:

f 1;          #   9  as list operator
f(1);         #  10  as named function
l.(1);        #  11  as named function, explicit postfix  # defined via pointy-block
$f(1);        #  12  as object invocation (must be hard ref)
$ref.(1);     #  13  as object invocation, explicit postfix
1.$f;         #  14  as pseudo-method meaning $f(1) (hard ref only)
1.$f();       #  15  as pseudo-method meaning $f(1) (hard ref only)
1.&f;         #  16  as pseudo-method meaning &f(1) (is hard f)
1.&f();       #  17  as pseudo-method meaning &f(1) (is hard f)
1.j;          #  18  as method via dispatcher             # requires custom method, via 'Int.^add_method'
1.j();        #  19  as method via dispatcher
1."$j-sym"(); #  20  as method via dispatcher, symbolic

# Calling a function with exactly two arguments:

f 1,2;         #  21  as list operator
f(1,2);        #  22  as named function
m.(1,2);       #  23  as named function, explicit postfix  # defined via pointy-block
$ref(1,2);     #  24  as object invocation (must be hard ref)
$ref.(1,2);    #  25  as object invocation, explicit postfix
1.$f: 2;       #  26  as pseudo-method meaning $f(1,2) (hard ref only)
1.$f(2);       #  27  as pseudo-method meaning $f(1,2) (hard ref only)
1.&f: 2;       #  28  as pseudo-method meaning &f(1,2) (is hard f)
1.&f(2);       #  29  as pseudo-method meaning &f(1,2) (is hard f)
1.k: 2;        #  30  as method via dispatcher             # requires custom method, via 'Int.^add_method'
1.k(2);        #  31  as method via dispatcher
1."$k-sym"(2); #  32  as method via dispatcher, symbolic

# Calling a function with a variable number of arguments (varargs):

my @args = 2,3,4;

f @args;           #  33  as list operator
f(@args);          #  34  as named function
n.(@args);         #  35  as named function, explicit postfix   # defined via pointy-block
$ref(@args);       #  36  as object invocation (must be hard ref)
$ref.(@args);      #  37  as object invocation, explicit postfix
1.$g: @args;       #  38  as pseudo-method meaning $f(1,@args) (hard ref)
1.$g(@args);       #  39  as pseudo-method meaning $f(1,@args) (hard ref)
1.&g: @args;       #  40  as pseudo-method meaning &f(1,@args)
1.&g(@args);       #  41  as pseudo-method meaning &f(1,@args)
1.h: @args;        #  42  as method via dispatcher              # requires custom method, via 'Int.^add_method'
1.h(@args);        #  43  as method via dispatcher
1."$h-sym"(@args); #  44  as method via dispatcher, symbolic
f(|@args);         #  45  equivalent to f(1,2,3)

}
```

{{out}}

```txt
f1 f2 i3 f4 f5 f6 f7 f8 f9 f10 l11 f12 f13 f14 f15 f16 f17 j18 j19 j20 f21 f22 m23 f24 f25 f26 f27 f28 f29 k30 k31 k32 f33 f34 n35 f36 f37 g38 g39 g40 g41 h42 h43 h44 f45
```



## Phix

Phix has three kinds of routines: procedure, function, and type. A procedure does not return a value, whereas a function does.
A type is a specialised kind of function that permits declarations of instances which are automatically validated whenever they are changed, further a type routine always returns either true or false.
* Phix does not allow implicit discard of function results. The explicit discard statement takes the form

```Phix
{} = myfunction()
```

* This is in fact a simple contraction of standard multiple assigment (which can be nested as deeply as you like):

```Phix
{cities,populations} = columize(muncipalities)
{{},populations} = columize(muncipalities) -- discard result[1]
{cities,{}} = columize(muncipalities)   -- discard result[2]
{cities} = columize(muncipalities)  -- ""
```

* Calling a function with no parameters still requires the "()" empty argument list.
* Optional arguments are denoted simply by the presence of a default, and must be grouped on the right:

```Phix
function myfunction(integer a, string b="default")
    return {a,b}
end function
--? myfunction() -- illegal, compile-time error
?myfunction(1) -- displays {1,"default"}
?myfunction(2,"that") -- displays {2,"that"}
```

* Sequence parameters can be of any length, which is another way to implement optional/variable number of arguments.
* Named arguments can be specified in any order, with an error if any non-optional parameters are missing:

```Phix
?myfunction(b:="then",a:=3) -- displays {3,"then"}
--?myfunction(b:="though") -- compile-time error
```

* The programmer is free to use either positional parameters or named parameters, or a mixture of both (with positional parameters first).
* Phix does not support first-class functions directly, but instead uses an integer routine_id mechanism (and obviously integers are first-class):

```Phix
constant integer r_my_func = routine_id("myroutine")
?call_func(r_my_func,{1}) -- displays {1,"default"}
```

The value of r_my_func can be passed as an argument to any routine, or stored in a table, and invoked in a similar fashion.

Note however that for performance reasons some builtins do not have a proper routine_id; if you need one you must write a trivial one-line wrapper.

(For a full list, see psym.e/syminit() calls to AutoAsm(), whereas calls to initialAutoEntry() therein indicate builtins that can have routine_ids.)

(Routines that do not have a proper routine_id do not support named parameters either.)

(One day the compiler may be enhanced to automatically create one-line wrappers as needed, but that is quite near the end of a fairly long to-do list.)
* Partial application is usually achieved through a single variable-length "user_data" parameter within a call_func() expression.
* All arguments are passed by reference with copy-on-write semantics: to modify the value of a parameter you must both return and assign it, as in:

```Phix
s = append(s,item)
```

* Implicit forward calls are supported, as are optional explicit forward declarations, which can occasionally cure compilation error messages.


## PicoLisp

When calling a funcion in PicoLisp directly (does this mean "in a statement context"?), it is always surrounded by parentheses, with or without arguments, and for any kind of arguments (evaluated or not):

```PicoLisp
(foo)
(bar 1 'arg 2 'mumble)
```

When a function is used in a "first class context" (e.g. passed to another function), then it is not yet '''called'''. It is simply '''used'''. Technically, a function can be either a '''number''' (a built-in function) or a '''list''' (a Lisp-level function) in PicoLisp):

```PicoLisp
(mapc println Lst)  # The value of 'printlin' is a number
(apply '((A B C) (foo (+ A (* B C)))) (3 5 7))  # A list is passed
```

Any argument to a function may be evaluated or not, depending on the function. For example, 'setq' evaluates every second argument

```PicoLisp
(setq A (+ 3 4)  B (* 3 4))
```

i.e. the first argument 'A' is not evaluated, the second evaluates to 7, 'B' is not evaluated, then the fourth evaluates to 12.


## Python

Under the hood all Python function/method parameters are named. All arguments can be passed as ''name=value'' pairs or as a dictionary containing such pairs using the ''myfunc('''**key_args''')'' (apply over dictionary) syntax).  One can also "apply" a function over a sequence of arguments using the syntax: ''myfunc('''*args''')'' as noted in comments below.  Parameters can be mixed so long parameters with default values (optional arguments) follow any "positional" (required) parameters, and catchall parameter ('''''*args''''') follow those, and any "keyword arguments' parameter" is last. (Any function can only have up to one "catchall" or '''''*args'''' parameter and up to one "keyword args" '''''**kwargs''''' parameter).

```python
def no_args():
    pass
# call
no_args()

def fixed_args(x, y):
    print('x=%r, y=%r' % (x, y))
# call
fixed_args(1, 2)        # x=1, y=2

## Can also called them using the parameter names, in either order:
fixed_args(y=2, x=1)

## Can also "apply" fixed_args() to a sequence:
myargs=(1,2) # tuple
fixed_args(*myargs)

def opt_args(x=1):
    print(x)
# calls
opt_args()              # 1
opt_args(3.141)         # 3.141

def var_args(*v):
    print(v)
# calls
var_args(1, 2, 3)       # (1, 2, 3)
var_args(1, (2,3))      # (1, (2, 3))
var_args()              # ()

## Named arguments
fixed_args(y=2, x=1)    # x=1, y=2

## As a statement
if 1:
    no_args()

## First-class within an expression
assert no_args() is None

def return_something():
    return 1
x = return_something()

def is_builtin(x):
	print(x.__name__ in dir(__builtins__))
# calls
is_builtin(pow)         # True
is_builtin(is_builtin)  # False

# Very liberal function definition

def takes_anything(*args, **kwargs):
    for each in args:
        print(each)
    for key, value in sorted(kwargs.items()):
        print("%s:%s" % (key, value))
    # Passing those to another, wrapped, function:
    wrapped_fn(*args, **kwargs)
    # (Function being wrapped can have any parameter list
    # ... that doesn't have to match this prototype)

## A subroutine is merely a function that has no explicit
## return statement and will return None.

## Python uses "Call by Object Reference".
## See, for example, http://www.python-course.eu/passing_arguments.php

## For partial function application see:
##   http://rosettacode.org/wiki/Partial_function_application#Python
```



## R

Translated from Python, when possible.

```rsplus
### Calling a function that requires no arguments
no_args <- function() NULL
no_args()


### Calling a function with a fixed number of arguments
fixed_args <- function(x, y) print(paste("x=", x, ", y=", y, sep=""))
fixed_args(1, 2)        # x=1, y=2
fixed_args(y=2, x=1)    # y=1, x=2


### Calling a function with optional arguments
opt_args <- function(x=1) x
opt_args()              # x=1
opt_args(3.141)         # x=3.141


### Calling a function with a variable number of arguments
var_args <- function(...) print(list(...))
var_args(1, 2, 3)
var_args(1, c(2,3))
var_args()


### Calling a function with named arguments
fixed_args(y=2, x=1)    # x=1, y=2


### Using a function in statement context
if (TRUE) no_args()


### Using a function in first-class context within an expression
print(no_args)


### Obtaining the return value of a function
return_something <- function() 1
x <- return_something()
x


### Distinguishing built-in functions and user-defined functions
# Not easily possible. See
# http://cran.r-project.org/doc/manuals/R-ints.html#g_t_002eInternal-vs-_002ePrimitive
# for details.


### Distinguishing subroutines and functions
# No such distinction.


### Stating whether arguments are passed by value or by reference
# Pass by value.


### Is partial application possible and how
# Yes, see http://rosettacode.org/wiki/Partial_function_application#R
```





## Racket



```Racket

#lang racket

;; Calling a function that requires no arguments
(foo)

;; Calling a function with a fixed number of arguments
(foo 1 2 3)

;; Calling a function with optional arguments
;; Calling a function with a variable number of arguments
(foo 1 2 3) ; same in both cases

;; Calling a function with named arguments
(foo 1 2 #:x 3) ; using #:keywords for the names

;; Using a function in statement context
;; Using a function in first-class context within an expression
;; Obtaining the return value of a function
;; -> Makes no sense for Racket, as well as most other functional PLs

;; Distinguishing built-in functions and user-defined functions
(primitive? foo)
;; but this is mostly useless, since most of Racket is implemented in
;; itself

;; Distinguishing subroutines and functions
;; -> No difference, though `!' is an idiomatic suffix for names of
;;    side-effect functions, and they usually return (void)

;; Stating whether arguments are passed by value or by reference

;; -> Always by value, but it's possible to implement languages with
;;    other argument passing styles, including passing arguments by
;;    reference (eg, there is "#lang algol60")

;; Is partial application possible and how
(curry foo 1 2)    ; later apply this on 3
(λ(x) (foo 1 2 x)) ; a direct way of doing the same

```



## REXX


### version 1


```rexx
/*REXX program to demonstrate various methods of calling a REXX function*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ Calling a function that REQUIRES no arguments.                     │
  │                                                                    │
  │ In the REXX language, there is no way to require the caller to not │
  │ pass arguments, but the programmer can check if any arguments were │
  │ (or weren't) passed.                                               │
  └────────────────────────────────────────────────────────────────────┘*/
yr=yearFunc()
say 'year=' yr
exit

yearFunc: procedure
if arg()\==0 then call sayErr "SomeFunc function won't accept arguments."
return left(date('Sorted'),3)
/*┌────────────────────────────────────────────────────────────────────┐
  │ Calling a function with a fixed number of arguments.               │
  │                                                                    │
  │ I take this to mean that the function requires a fixed number of   │
  │ arguments.   As above, REXX doesn't enforce calling (or invoking)  │
  │ a (any) function with a certain number of arguments,  but the      │
  │ programmer can check if the correct number of arguments have been  │
  │ specified (or not).                                                │
  └────────────────────────────────────────────────────────────────────┘*/
ggg=FourFunc(12,abc,6+q,zz%2,'da 5th disagreement')
say 'ggg squared=' ggg**2
exit

FourFunc: procedure; parse arg a1,a2,a3; a4=arg(4)  /*another way get a4*/

if arg()\==4 then do
                  call sayErr "FourFunc function requires 4 arguments,"
                  call sayErr "but instead it found" arg() 'arguments.'
                  exit 13
                  end
return a1+a2+a3+a4
/*┌────────────────────────────────────────────────────────────────────┐
  │ Calling a function with optional arguments.                        │
  │                                                                    │
  │ Note that not passing an argument isn't the same as passing a null │
  │ argument  (a REXX variable whose value is length zero).            │
  └────────────────────────────────────────────────────────────────────┘*/
x=12;  w=x/2;  y=x**2;  z=x//7                /* z  is  x  modulo seven.*/
say 'sum of w, x, y, & z=' SumIt(w,x,y,,z)    /*pass 5 args, 4th is null*/
exit

SumIt: procedure; sum=0

  do j=1 for arg()
  if arg(j,'E') then sum=sum+arg(j)  /*the Jth arg may have been omitted*/
  end

return sum
/*┌────────────────────────────────────────────────────────────────────┐
  │ Calling a function with a variable number of arguments.            │
  │                                                                    │
  │ This situation isn't any different then the previous example.      │
  │ It's up to the programmer to code how to utilize the arguments.    │
  └────────────────────────────────────────────────────────────────────┘*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ Calling a function with named arguments.                           │
  │                                                                    │
  │ REXX allows almost anything to be passed, so the following is one  │
  │ way this can be accomplished.                                      │
  └────────────────────────────────────────────────────────────────────┘*/
what=parserFunc('name=Luna',"gravity=.1654",'moon=yes')
say 'name=' common.name
gr=common.gr
say 'gravity=' gr
exit

parseFunc: procedure expose common.
      do j=1 for arg()
      parse var arg(j) name '=' val
      upper name
      call value 'COMMON.'name,val
      end
return arg()
/*┌────────────────────────────────────────────────────────────────────┐
  │ Calling a function in statement context.                           │
  │                                                                    │
  │ REXX allows functions to be called (invoked) two ways, the first   │
  │ example (above) is calling a function in statement context.        │
  └────────────────────────────────────────────────────────────────────┘*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ Calling a function in within an expression.                        │
  │                                                                    │
  │ This is a variant of the first example.                            │
  └────────────────────────────────────────────────────────────────────┘*/
yr=yearFunc()+20
say 'two decades from now, the year will be:' yr
exit
/*┌────────────────────────────────────────────────────────────────────┐
  │ Obtaining the return value of a function.                          │
  │                                                                    │
  │ There are two ways to get the (return) value of a function.        │
  └────────────────────────────────────────────────────────────────────┘*/
currYear=yearFunc()
say 'the current year is' currYear

call yearFunc
say 'the current year is' result
/*┌────────────────────────────────────────────────────────────────────┐
  │ Distinguishing built-in functions and user-defined functions.      │
  │                                                                    │
  │ One objective of the REXX language is to allow the user to use any │
  │ function (or subroutine) name whether or not there is a built-in   │
  │ function with the same name  (there isn't a penality for this).    │
  └────────────────────────────────────────────────────────────────────┘*/
qqq=date()                      /*number of real dates that Bob was on. */
say "Bob's been out" qqq 'times.'
www='DATE'('USA')               /*returns date in format mm/dd/yyy      */
exit                            /*any function in quotes is external.   */

date: return 4
/*┌────────────────────────────────────────────────────────────────────┐
  │ Distinguishing subroutines and functions.                          │
  │                                                                    │
  │ There is no programatic difference between subroutines and         │
  │ functions if the subroutine returns a value  (which effectively    │
  │ makes it a function).   REXX allows you to call a function as if   │
  │ it were a subroutine.                                              │
  └────────────────────────────────────────────────────────────────────┘*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ In REXX, all arguments are passed by value, never by name,  but it │
  │ is possible to accomplish this if the variable's name is passed    │
  │ and the subroutine/function could use the built-in-function VALUE  │
  │ to retrieve the variable's value.                                  │
  └────────────────────────────────────────────────────────────────────┘*/
/*┌────────────────────────────────────────────────────────────────────┐
  │ In the REXX language, partial application is possible, depending   │
  │ how partial application is defined; I prefer the 1st definition (as│
  │ (as per the "discussion" for "Partial Function Application" task:  │
  │   1.  The "syntactic sugar" that allows one to write (some examples│
  │       are:      map (f 7 9)  [1..9]                                │
  │        or:      map(f(7,_,9),{1,...,9})                            │
  └────────────────────────────────────────────────────────────────────┘*/
```



### version 2


```rexx
/* REXX ***************************************************************
* 29.07.2013 Walter Pachl trying to address the task concisely
***********************************************************************
* f1 Calling a function that requires no arguments
* f2 Calling a function with a fixed number of arguments
* f3 Calling a function with optional arguments
* f4 Calling a function with a variable number of arguments
* f5 Calling a function with named arguments
* f6 Using a function in statement context
* f7 Using a function within an expression
* f8 Obtaining the return value of a function
*       f8(...) is replaced by the returned value
*       call f8 ...  returned value is in special vatiable RESULT
* f9 Distinguishing built-in functions and user-defined functions
*       bif is enforced by using its name quoted in uppercase
* fa,fb Distinguishing subroutines and functions
* Stating whether arguments are passed by value or by reference
*       Arguments are passed by value
*       ooRexx supports passing by reference (Use Arg instruction)
* Is partial application possible and how
*       no ideas
**********************************************************************/
say f1()
Say f2(1,2,3)
say f2(1,2,3,4)
say f3(1,,,4)
Say f4(1,2)
Say f4(1,2,3)
a=4700; b=11;
Say f5('A','B')
f6()  /* returned value is used as command */
x=f7()**2
call f8 1,2; Say result '=' f8(1,2)
f9: Say 'DATE'('S') date()
call fa 11,22; Say result '=' fa(1,,
                                   2) /* the second comma above is for line continuation */
Signal On Syntax
Call fb 1,2
x=fb(1,2)
Exit
f1: Return 'f1 doesn''t need an argument'
f2: If arg()=3 Then
      Return 'f2: Sum of 3 arguments:' arg(1)+arg(2)+arg(3)
    Else
      Return 'f2: Invalid invocation:' arg() 'arguments. Needed: 3'
f3: sum=0
    do i=1 To arg()
      If arg(i,'E')=0 Then Say 'f3: Argument' i 'omitted'
                      Else sum=sum+arg(i)
      End
    Return 'f3 sum=' sum
f4: sum=0; Do i=1 To arg(); sum=sum+arg(i); End
    Return 'f4: Sum of' arg() 'arguments is' sum
f5: Parse Arg p1,p2
    Say 'f5: Argument 1 ('p1') contains' value(p1)
    Say 'f5: Argument 2 ('p2') contains' value(p2)
    Return 'f5: sum='value(p1)+value(p2)
f6: Say 'f6: dir ft.rex'
    Return 'dir ft.rex'
f7: Say 'f7 returns 7'
    Return 7
f8: Say 'f8 returns arg(1)+arg(2)'
    Return arg(1)+arg(2)
date: Say 'date is my date function'
    Return translate('ef/gh/abcd','DATE'('S'),'abcdefgh')
fa: Say 'fa returns arg(1)+arg(2)'
    Return arg(1)+arg(2)
fb: Say 'fb:' arg(1)','arg(2)
    Return

Syntax:
  Say 'Syntax raised in line' sigl
  Say sourceline(sigl)
  Say 'rc='rc '('errortext(rc)')'
  If sigl=39 Then
    Say 'fb cannot be invoked as function (it does not return a value'
  Exit
```

{{out}}

```txt

f1 doesn't need an argument
f2: Sum of 3 arguments: 6
f2: Invalid invocation: 4 arguments. Needed: 3
f3: Argument 2 omitted
f3: Argument 3 omitted
f3 sum= 5
f4: Sum of 2 arguments is 3
f4: Sum of 3 arguments is 6
f5: Argument 1 (A) contains 4700
f5: Argument 2 (B) contains 11
f5: sum=4711
f6: dir ft.rex
 Datenträger in Laufwerk D: ist DATA
 Volumeseriennummer: B0F8-F2C3

 Verzeichnis von D:\

29.07.2013  00:33             2.661 ft.rex
               1 Datei(en),          2.661 Bytes
               0 Verzeichnis(se), 251.050.979.328 Bytes frei
f7 returns 7
f8 returns arg(1)+arg(2)
f8 returns arg(1)+arg(2)
3 = 3
date is my date function
20130729 07/29/2013
fa returns arg(1)+arg(2)
fa returns arg(1)+arg(2)
33 = 3
fb: 1,2
fb: 1,2
Syntax raised in line 39
x=fb(1,2)
rc=44 (Function or message did not return data)
fb cannot be invoked as function (it does not return a value)
```



## Ring


```ring

hello()
func hello
        see "Hello from function" + nl

```


```ring

first()  second()
func first   see "message from the first function" + nl
func second  see "message from the second function" + nl

```


```ring

sum(3,5) sum(1000,2000)
func sum x,y see x+y+nl

```


```ring

# this program will print the hello world message first then execute the main function
See "Hello World!" + nl
func main
        see "Message from the main function" + nl

```



## Ruby

Ruby does not have functions, but Ruby classes have "methods" which are equivalent.
The parentheses around the arguments are optional (definition and call).
A method returns the last expression that was evaluated in the body of the method.
The return keyword can be used to make it explicit that a method returns a value.

*Calling a function that requires no arguments
:
```ruby
def foo() p "foo" end

foo                             #=> "foo"
foo()                           #=> "foo"
```


*Calling a function with a fixed number of arguments
:
```ruby
def foo arg; p arg end          # one argument

foo(1)                          #=> 1
foo "1"                         #=> "1"
foo [0,1,2]                     #=> [0, 1, 2]   (one Array)
```


*Calling a function with optional arguments
:
```ruby
def foo(x=0, y=x, flag=true) p [x,y,flag] end

foo                             #=> [0, 0, true]
foo(1)                          #=> [1, 1, true]
foo(1,2)                        #=> [1, 2, true]
foo 1,2,false                   #=> [1, 2, false]
```


*Calling a function with a variable number of arguments
:
```ruby
def foo(*args) p args end

foo                             #=> []
foo(1,2,3,4,5)                  #=> [1, 2, 3, 4, 5]
```


*Calling a function with named arguments
:
```ruby
def foo(id:0, name:"", age:0) p [id, name, age] end

foo(age:22, name:"Tom")         #=> [0, "Tom", 22]
```


*Using a function in statement context
::?

*Using a function in first-class context within an expression
:The method is not a first-class function. However, there is '''Proc''' object.
::See [[First-class functions#Ruby]]

*Obtaining the return value of a function
:
```ruby
def foo(a,b) a + b end

bar = foo 10,20
p bar                           #=> 30
p foo("abc","def")              #=> "abcdef"

# return multiple values
def sum_and_product(a,b) return a+b,a*b end

x,y = sum_and_product(3,5)
p x                             #=> 8
p y                             #=> 15
```


*Distinguishing built-in functions and user-defined functions
::There is no distinction.

*Distinguishing subroutines and functions
:Subroutine and function don't exist at ruby. It is only a method that there is.
::The Kernel module is included by class Object, so its methods are available in every Ruby object.
::These methods are called without a receiver and thus can be called in functional form.

::
```ruby
puts "OK!"                      # Kernel#puts
raise "Error input"             # Kernel#raise
Integer("123")                  # Kernel#Integer
rand(6)                         # Kernel#rand
throw(:exit)                    # Kernel#throw

# method which can be seen like a reserved word.
attr_accessor                   # Module#attr_accessor
include                         # Module#include
private                         # Module#private
require                         # Kernel#require
loop { }                        # Kernel#loop
```


*Stating whether arguments are passed by value or by reference

::passed by value of object reference.

*Is partial application possible and how
::However something similar can be done, see [[Partial function application#Ruby]]


*Others
:Block Argument:
::The block argument sends a closure from the calling scope to the method.
::The block argument is always last when sending a message to a method. A block is sent to a method using <code>do ... end</code> or <code>{ ... }</code>.
::
```ruby
class Array
  def sum(init=0, &blk)
    if blk
      inject(init){|s, n| s + blk.call(n)}
    else
      inject(init){|s, n| s + n}
    end
  end
end

ary = [1,2,3,4,5]
p ary.sum                               #=> 15
p ary.sum(''){|n| (-n).to_s}            #=> "-1-2-3-4-5"
p (ary.sum do |n| n * n end)            #=> 55
```


:Splat operator:
::You can turn an Array into an argument list with * (or splat) operator.
::
```ruby
def foo(a,b,c) p [a,b,c] end

args = [1,2,3]
foo *args                       #=> [1, 2, 3]
args = [1,2]
foo(0,*args)                    #=> [0, 1, 2]
```


:Syntax sugar:
::In Ruby, many operators are actually method calls.
::
```ruby
#                                   return value        substance
i = 3
p 1 + i                         #=> 4                   1.+(i)
p i < 5                         #=> true                i.<(5)
p 2 ** i                        #=> 8                   2.**(i)
p -i                            #=> -3                  i.-@()
a = [1,2,3]
p a[0]                          #=> 1                   a.[](0)
a[2] = "0"                      #                       a.[]=(2,"0")
p a << 5                        #=> [1, 2, "0", 5]      a.<<(5)
p a & [4,2]                     #=> [2]                 a.&([4,2])
p "abcde"[1..3]                 #=> "bcd"               "abcde".[](1..3)
p "%2d %4s" % [1,"xyz"]         #=> " 1  xyz"           "%2d %4s".%([1,"xyz"])
```

::Method call which was displayed in the comment is usable actually.

## Rust


```rust
fn main() {
    // Rust has a lot of neat things you can do with functions: let's go over the basics first
    fn no_args() {}
    // Run function with no arguments
    no_args();

    // Calling a function with fixed number of arguments.
    // adds_one takes a 32-bit signed integer and returns a 32-bit signed integer
    fn adds_one(num: i32) -> i32 {
        // the final expression is used as the return value, though `return` may be used for early returns
        num + 1
    }
    adds_one(1);

    // Optional arguments
    // The language itself does not support optional arguments, however, you can take advantage of
    // Rust's algebraic types for this purpose
    fn prints_argument(maybe: Option<i32>) {
        match maybe {
            Some(num) => println!("{}", num),
            None => println!("No value given"),
        };
    }
    prints_argument(Some(3));
    prints_argument(None);

    // You could make this a bit more ergonomic by using Rust's Into trait
    fn prints_argument_into<I>(maybe: I)
        where I: Into<Option<i32>>
    {
        match maybe.into() {
            Some(num) => println!("{}", num),
            None => println!("No value given"),
        };
    }
    prints_argument_into(3);
    prints_argument_into(None);

    // Rust does not support functions with variable numbers of arguments. Macros fill this niche
    // (println! as used above is a macro for example)

    // Rust does not support named arguments

    // We used the no_args function above in a no-statement context

    // Using a function in an expression context
    adds_one(1) + adds_one(5); // evaluates to eight

    // Obtain the return value of a function.
    let two = adds_one(1);

    // In Rust there are no real built-in functions (save compiler intrinsics but these must be
    // manually imported)

    // In rust there are no such thing as subroutines

    // In Rust, there are three ways to pass an object to a function each of which have very important
    // distinctions when it comes to Rust's ownership model and move semantics. We may pass by
    // value, by immutable reference, or mutable reference.

    let mut v = vec![1, 2, 3, 4, 5, 6];

    // By mutable reference
    fn add_one_to_first_element(vector: &mut Vec<i32>) {
        vector[0] += 1;
    }
    add_one_to_first_element(&mut v);
    // By immutable reference
    fn print_first_element(vector: &Vec<i32>) {
        println!("{}", vector[0]);
    }
    print_first_element(&v);

    // By value
    fn consume_vector(vector: Vec<i32>) {
        // We can do whatever we want to vector here
    }
    consume_vector(v);
    // Due to Rust's move semantics, v is now inaccessible because it was moved into consume_vector
    // and was then dropped when it went out of scope

    // Partial application is not possible in rust without wrapping the function in another
    // function/closure e.g.:
    fn average(x: f64, y: f64) -> f64 {
        (x + y) / 2.0
    }
    let average_with_four = |y| average(4.0, y);
    average_with_four(2.0);


}
```



## Scala

{{libheader|Scala}}

```Scala
def ??? = throw new NotImplementedError // placeholder for implementation of hypothetical methods
def myFunction0() = ???
myFunction0() // function invoked with empty parameter list
myFunction0   // function invoked with empty parameter list omitted

def myFunction = ???
myFunction          // function invoked with no arguments or empty arg list
/* myFunction() */  // error: does not take parameters

def myFunction1(x: String) = ???
myFunction1("foobar")     // function invoked with single argument
myFunction1 { "foobar" }  // function invoked with single argument provided by a block
                          // (a block of code within {}'s' evaluates to the result of its last expression)

def myFunction2(first: Int, second: String) = ???
val b = "foobar"
myFunction2(6, b) // function with two arguments

def multipleArgLists(first: Int)(second: Int, third: String) = ???
multipleArgLists(42)(17, "foobar")  // function with three arguments in two argument lists

def myOptionalParam(required: Int, optional: Int = 42) = ???
myOptionalParam(1)    // function with optional param
myOptionalParam(1, 2) // function with optional param provided

def allParamsOptional(firstOpt: Int = 42, secondOpt: String = "foobar") = ???
allParamsOptional()     // function with all optional args
/* allParamsOptional */ // error: missing arguments for method allParamsOptional;
                        //        follow with `_' if you want to treat it as a partially applied function

def sum[Int](values: Int*) = values.foldLeft(0)((a, b) => a + b)
sum(1, 2, 3)                // function accepting variable arguments as literal

val values = List(1, 2, 3)
sum(values: _*)             // function acception variable arguments from collection
sum()                       // function accepting empty variable arguments

def mult(firstValue: Int, otherValues: Int*) = otherValues.foldLeft(firstValue)((a, b) => a * b)
mult(1, 2, 3)                             // function with non-empty variable arguments
myOptionalParam(required = 1)             // function called with named arguments (all functions have named arguments)
myFunction2(second = "foo", first = 1)    // function with re-ordered named arguments
mult(firstValue = 1, otherValues = 2, 3)  // function with named variable argument as literal

val otherValues = Seq(2, 3)
mult(1, otherValues = otherValues: _*)  // function with named variable argument from collection
val result = myFunction0()              // function called in an expression context
myFunction0()                           // function called in statement context
/* myOptionalParam(optional = 1, 2) */  // error: positional after named argument.

def transform[In, Out](initial: In)(transformation: In => Out) = transformation(initial)
val result = transform(42)(x => x * x)  // function in first-class context within an expression

def divide(top: Double, bottom: Double) = top / bottom
val div = (divide _)              // partial application -- defer application of entire arg list
val halve = divide(_: Double, 2)  // partial application -- defer application of some arguments

class Foo(var value: Int)
def incFoo(foo: Foo) = foo.value += 1 // function showing AnyRef's are passed by reference
/* def incInt(i: Int) = i += 1 */     // error: += is not a member of Int
                                      // (All arguments are passed by reference, but reassignment
                                      // or setter must be defined on a type or a field
                                      // (respectively) in order to modify its value.)

// No distinction between built-in functions and user-defined functions
// No distinction between subroutines and functions
```



## Seed7

* Seed7 provides two kinds of subroutines: ''proc'', which has no return value, and ''func'', which has a return value. The return value of a ''func'' must be used by the caller (e.g. assigned to a variable). If you don't want do deal with the return value, use a ''proc'' instead.

* Seed7 supports call-by-value, call-by-reference, and call-by-name parameters. Programmers are free to specify the desired parameter passing mechanism. The most used parameter passing mechanism is 'in'. Depending on the type 'in' specifies call-by-value (for integer, float, ...) or call-by-reference (for string, array, ...). It is prohibited to write something to an 'in' parameter.

* All parameters are positional.

* There are no differences between between calling built-in vs. user defined functions.
```seed7
env := environment;     # Call a function that requires no arguments.
env := environment();   # Alternative possibility to call of a function with no arguments.
cmp := compare(i, j);   # Call a function with a fixed number of arguments.
```


* There are no optional arguments, but a similar effect can be achieved with overloading.
```seed7
write(aFile, "asdf");   # Variant of write with a parameter to specify a file.
write("asdf");          # Variant of write which writes to the file OUT.
```


* Seed7 does not support functions with a variable number of arguments. But a function argument can be an array with as many values as you want:
```seed7
const func integer: sum (in array integer: intElems) is func
  result
    var integer: sum is 0;
  local
    var integer: element is 0;
  begin
    for element range intElems do
      sum +:= element;
    end for;
  end func;

s := sum([] (1, 2, 3)); # Use an aggregate to generate an array.
t := sum([] (2, 3, 5, 7));
```


* Concatenation operators can be used to concatenate arguments. This solution is used to provide the write function:
```seed7
write("Nr: " <& num);   # Use operators to concatenate arguments.
```


* The procedure ignore can be used to ignore a return value.
```seed7
ignore(getln(IN));      # Using a function in statement context (ignore the result).
```


* Call-by-name parameters use a function in first-class context. The function [http://seed7.sourceforge.net/examples/map.htm doMap] from the examples section of the Seed7 homepage uses a given expression to modify the elements of an array:
```seed7
seq := doMap([](1, 2, 4, 6, 10, 12, 16), x, succ(x));
```



## Sidef

All functions in Sidef are first-class closures

```ruby
foo();                       # without arguments
foo(1, 2);                   # with two arguments
foo(args...);                # with a variable number of arguments
foo(name: 'Bar', age: 42);   # with named arguments

var f = foo;                 # store the function foo inside 'f'
var result = f();            # obtain the return value of a function

var arr = [1,2,3];
foo(arr);                    # the arguments are passed by object-reference
```


Partial application is possible by using a curry function:


```ruby
func curry(f, *args1) {
    func (*args2) {
        f(args1..., args2...);
    }
}

func add(a, b) {
    a + b
}

var adder = curry(add, 1);
say adder(3);                 #=>4
```



## Smalltalk

Where f is a closure and arguments is an array of values for f to operate on.

```smalltalk>f valueWithArguments: arguments.</lang



## SSEM

Assuming the subroutine has been set up in accordance with the Wheeler jump technique as described in the SSEM [[Function definition]] entry, calling it requires simply loading the return address into the accumulator and jumping out to the subroutine. Parameters must be passed using "global variables", i.e. storage locations; results may be passed the same way, although it is also possible to pass a return value in the accumulator.

This code fragment, beginning (for the sake of argument) at address 10, performs a Wheeler jump to a subroutine beginning at address 20. The return address is coded in negative (two's complement) form because the SSEM negates values in the process of loading them into the accumulator. As always on the SSEM, jump targets are one less than the actual intended target: this is because the CI ("Current Instruction") register is incremented after an instruction has been executed rather than before.

```ssem
00110000000000100000000000000000  10. -12 to c
10110000000000000000000000000000  11. 13 to CI
11001111111111111111111111111111  12. -13
11001000000000000000000000000000  13. 19
```



## Swift


```Swift
// call a function with no args
noArgs()

// call a function with one arg with no external name
oneArgUnnamed(1)

// call a function with one arg with external name
oneArgNamed(arg: 1)

// call a function with two args with no external names
twoArgsUnnamed(1, 2)

// call a function with two args and external names
twoArgsNamed(arg1: 1, arg2: 2)

// call a function with an optional arg
// with arg
optionalArguments(arg: 1)
// without
optionalArguments() // defaults to 0

// function that takes another function as arg
funcArg(noArgs)

// variadic function
variadic(opts: "foo", "bar")

// getting a return value
let foo = returnString()

// getting a bunch of return values
let (foo, bar, baz) = returnSomeValues()

// getting a bunch of return values, discarding second returned value
let (foo, _, baz) = returnSomeValues()
```



## Tcl


```tcl
aCallToACommandWithNoArguments
aCallToACommandWithOne argument
aCallToACommandWith arbitrarily many arguments
aCallToACommandWith {*}$manyArgumentsComingFromAListInAVariable
aCallToACommandWith -oneNamed argument -andAnother namedArgument
aCallToACommandWith theNameOfAnotherCommand
aCallToOneCommand [withTheResultOfAnother]
```

Tcl does differentiate between functions and other types of commands in expressions:

```tcl
expr {func() + [cmd]}
expr {func(1,2,3} + [cmd a b c]}
```

However, there are no deep differences between the two: functions are translated into commands that are called in a particular namespace (thus <tt>foo()</tt> becomes <tt>tcl::mathfunc::foo</tt>).
There are no differences in usage between built-in commands and user-defined ones, and parameters are passed to commands by value conceptually (and read-only reference in the implementation).


## UNIX Shell


In the shell, there are no argument specifications for functions. Functions obtain their arguments using the positional parameter facilities and functions are simply called by name followed by any arguments that are to be passed:


```sh
sayhello    # Call a function in statement context with no arguments
multiply 3 4    # Call a function in statement context with two arguments
```


The shell does not support the use of named parameters. There is no lookahead in the shell, so functions cannot be called until their definition has been run.


## VBA


```vb
'definitions/declarations

'Calling a function that requires no arguments
Function no_arguments() As String
    no_arguments = "ok"
End Function

'Calling a function with a fixed number of arguments
Function fixed_number(argument1 As Integer, argument2 As Integer)
    fixed_number = argument1 + argument2
End Function

'Calling a function with optional arguments
Function optional_parameter(Optional argument1 = 1) As Integer
    'Optional parameters come at the end of the parameter list
    optional_parameter = argument1
End Function

'Calling a function with a variable number of arguments
Function variable_number(arguments As Variant) As Integer
    variable_number = UBound(arguments)
End Function

'Calling a function with named arguments
Function named_arguments(argument1 As Integer, argument2 As Integer) As Integer
    named_arguments = argument1 + argument2
End Function

'Using a function in statement context
Function statement() As String
    Debug.Print "function called as statement"
    statement = "ok"
End Function

'Using a function in first-class context within an expression
'see call the functions

'Obtaining the return value of a function
Function return_value() As String
    return_value = "ok"
End Function

'Distinguishing built-in functions and user-defined functions
'There is no way to distinguish built-in function and user-defined functions

'Distinguishing subroutines And functions
'subroutines are declared with the reserved word "sub" and have no return value
Sub foo()
    Debug.Print "subroutine",
End Sub
'functions are declared with the reserved word "function" and can have a return value
Function bar() As String
    bar = "function"
End Function

'Stating whether arguments are passed by value or by reference
Function passed_by_value(ByVal s As String) As String
    s = "written over"
    passed_by_value = "passed by value"
End Function
'By default, parameters in VBA are by reference
Function passed_by_reference(ByRef s As String) As String
    s = "written over"
    passed_by_reference = "passed by reference"
End Function

'Is partial application possible and how
'I don't know

'calling a subroutine with arguments does not require parentheses
Sub no_parentheses(myargument As String)
    Debug.Print myargument,
End Sub

'call the functions
Public Sub calling_a_function()
    'Calling a function that requires no arguments
    Debug.Print "no arguments", , no_arguments
    Debug.Print "no arguments", , no_arguments()
    'Parentheses are not required

    'Calling a function with a fixed number of arguments
    Debug.Print "fixed_number", , fixed_number(1, 1)

    'Calling a function with optional arguments
    Debug.Print "optional parameter", optional_parameter
    Debug.Print "optional parameter", optional_parameter(2)

    'Calling a function with a variable number of arguments
    Debug.Print "variable number", variable_number([{"hello", "there"}])
    'The variable number of arguments have to be passed as an array

    'Calling a function with named arguments
    Debug.Print "named arguments", named_arguments(argument2:=1, argument1:=1)

    'Using a function in statement context
    statement

    'Using a function in first-class context within an expression
    s = "no_arguments"
    Debug.Print "first-class context", Application.Run(s)
    'A function name can be passed as argument in a string

    'Obtaining the return value of a function
    returnvalue = return_value
    Debug.Print "obtained return value", returnvalue

    'Distinguishing built-in functions and user-defined functions

    'Distinguishing subroutines And functions
    foo
    Debug.Print , bar

    'Stating whether arguments are passed by value or by reference
    Dim t As String
    t = "unaltered"
    Debug.Print passed_by_value(t), t
    Debug.Print passed_by_reference(t), t

    'Is partial application possible and how
    'I don 't know

    'calling a subroutine with arguments does not require parentheses
    no_parentheses "calling a subroutine"
    Debug.Print "does not require parentheses"
    Call no_parentheses("deprecated use")
    Debug.Print "of parentheses"

End Sub

```
{{out}}

```txt
no arguments                ok
no arguments                ok
fixed_number                 2
optional parameter           1
optional parameter           2
variable number              2
named arguments              2
function called as statement
first-class context         ok
obtained return value       ok
subroutine                  function
passed by value             unaltered
passed by reference         written over
calling a subroutine        does not require parentheses
deprecated use              of parentheses
```


## WDTE


```wdte>let noargs =
 + 2 5;
noargs -- print;

let fixedargs a b => + a b;
fixedargs 3 5 -- print;

let m => import 'math';
m.cos 3 -- print;

# WDTE only has expressions, not statements, so statement vs.
# first-class context doesn't make sense.

# Arguments in WDTE are technically passed by reference, in a way, but
# because it's a functional language and everything's immutable
# there's no real usability difference from that.

# Partial application is possible. For example, the following
# evaluates `+ 3` and then passes 7 to the resulting partially applied
# function.
(+ 3) 7 -- print;
```



## XLISP


```lisp
; call a function (procedure) with no arguments:
(foo)

; call a function (procedure) with arguments:
(foo bar baz)
; the first symbol after "(" is the name of the function
; the other symbols are the arguments

; call a function on a list of arguments formed at run time:
(apply foo bar)

; In a REPL, the return value will be printed.
; In other contexts, it can be fed as argument into a further function:
(foo (bar baz))
; this calls bar on the argument baz and then calls foo on the return value

; or it can simply be discarded
(foo bar)
; nothing is done with the return value
```



## XSLT



```xml
<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml" indent="yes"/>
    <xsl:template match="/">
        <demo>
            <!--
                XSLT 1.0 actually defines two function-like constructs that
                are used variously depending on the context.
            -->
            <xsl:call-template name="xpath-function-demos"/>
            <xsl:call-template name="xslt-template-demos"/>
        </demo>
    </xsl:template>

    <xsl:template name="xpath-function-demos">
        <!--
            A 'function' in XSLT 1.0 is a function that can be called from
            an XPath 1.0 expression (such as from "select" or "test"
            attribute of several XSLT elements). The following demos apply
            to these functions.
        -->

        <!-- Calling function that requires no arguments -->
        <!-- false() always returns a boolean false value -->
        <line>This test is <xsl:if test="false()">NOT</xsl:if> OK.</line>

        <!-- Calling a function with a fixed number of arguments -->
        <!-- not() takes exactly 1 argument. starts-with() takes exactly 2 arguments. -->
        <line>'haystack' does <xsl:if test="not(starts-with('haystack', 'hay'))">NOT</xsl:if> start with 'hay'.</line>

        <!-- Calling a function with optional arguments -->
        <!-- If the third argument of substring() is omitted, the length of the string is assumed. -->
        <line>'<xsl:value-of select="substring('haystack', 1, 3)"/>' = 'hay'</line>
        <line>'<xsl:value-of select="substring('haystack', 4)"/>' = 'stack'</line>

        <!-- Calling a function with a variable number of arguments -->
        <!-- concat() accepts two or more arguments. -->
        <line>'<xsl:value-of select="concat('abcd', 'efgh')"/>' = 'abcdefgh'</line>
        <line>'<xsl:value-of select="concat('ij', 'kl', 'mn', 'op')"/>' = 'ijklmnop'</line>
        <!--
            Aggregate functions such as sum() and count() accept nodesets.
            This isn't quite the same as varargs but are probably worth
            mentioning.
        -->
        <line>The number of root elements in the input document is <xsl:value-of select="count(/*)"/> (should be 1).</line>

        <!-- Calling a function with named arguments -->
        <!-- XPath 1.0 uses only positional parameters. -->

        <!-- Using a function in statement context -->
        <!--
            In general, XPath 1.0 functions have no side effects, so calling
            them as statements is useless. While implementations often allow
            writing extensions in imperative languages, the semantics of
            calling a function with side effects are, at the very least,
            implementation-dependent.
        -->

        <!-- Using a function in first-class context within an expression -->
        <!-- Functions are not natively first-class values in XPath 1.0. -->

        <!-- Obtaining the return value of a function -->
        <!--
            The return value of the function is handled as specified by the
            various contexts in which an XPath expression is used. The
            return value can be stored in a "variable" (no destructive
            assignment is allowed), passed as a parameter to a function or a
            template, used as a conditional in an <xsl:if/> or <xsl:when/>,
            interpolated into text using <xsl:value-of/> or into an
            attribute value using brace syntax, and so forth.
        -->
        <!-- Here, concat() is interpolated into an attribute value using braces ({}). -->
        <line foo="{concat('Hello, ', 'Hello, ', 'Hello')}!">See attribute.</line>

        <!-- Distinguishing built-in functions and user-defined functions -->
        <!--
            Given that functions aren't first-class here, the origin of any
            given function is known before run time. Incidentally, functions
            defined by the standard are generally unprefixed while
            implementation-specific extensions (and user extensions, if
            available) must be defined within a separate namespace and
            prefixed.
        -->

        <!-- Distinguishing subroutines and functions -->
        <!--
            There are no "subroutines" in this sense—everything that looks
            like a subroutine has some sort of return or result value.
        -->

        <!-- Stating whether arguments are passed by value or by reference -->
        <!-- There is no meaningful distinction since there is no mechanism by which to mutate values. -->

        <!-- Is partial application possible and how -->
        <!-- Not natively. -->
    </xsl:template>

    <xsl:template name="xslt-template-demos">
        <!--
            A 'template' in XSLT 1.0 is a subroutine-like construct. When
            given a name (and, optionally, parameters), it can be called
            from within another template using the <xsl:call-template/>
            element. (An unnamed template is instead called according to its
            match and mode attributes.) The following demos apply to named
            templates.
        -->
        <!--
            Unlike with functions, there are no built-in named templates to
            speak of. The ones used here are defined later in this
            transform.
        -->

        <!--
            Answers for these prompts are the same as with XPath functions (above):
                Using a function in statement context
                Distinguishing subroutines and functions
                Stating whether arguments are passed by value or by reference
                Is partial application possible and how
        -->

        <!-- Calling function that requires no arguments -->
        <xsl:call-template name="nullary-demo"/>
        <!--
            Note that even if a template has no parameters, it has access to
            the current node (.) as of the time of the call. This
            <xsl:apply-templates/> runs a matching template above that calls
            the template "nullary-context-demo" with no parameters. Another
            way to manipulate a template's idea of which node is current is
            by calling from inside a <xsl:for-each/> loop.
        -->
        <xsl:apply-templates select="/*" mode="nullary-context-demo-mode"/>

        <!--
            A template parameter is made optional in the definition of the
            template by supplying an expression as its select attribute,
            which is evaluated and used as its value if the parameter is
            omitted. Note, though, that all template parameters have an
            implicit default value, the empty string, if the select
            attribute is not specified. Therefore, all template parameters
            are always optional, even when semantically they should not be.
        -->

        <!-- Calling a function with a fixed number of arguments -->
        <working note="When all parameters are supplied">
            <xsl:call-template name="ternary-demo">
                <xsl:with-param name="a" select="4"/>
                <xsl:with-param name="b">3</xsl:with-param>
                <xsl:with-param name="c" select="2 + 3"/>
            </xsl:call-template>
        </working>
        <broken note="When the third parameter 'c' is omitted">
            <xsl:call-template name="ternary-demo">
                <xsl:with-param name="a" select="4"/>
                <xsl:with-param name="b">3</xsl:with-param>
            </xsl:call-template>
        </broken>

        <!-- Calling a function with optional arguments -->
        <!-- With the optional third parameter -->
        <working name="When all parameters are supplied">
            <xsl:call-template name="binary-or-ternary-demo">
                <xsl:with-param name="a" select="4"/>
                <xsl:with-param name="b" select="3"/>
                <xsl:with-param name="c" select="5"/>
            </xsl:call-template>
        </working>
        <!-- Without the optional third parameter (which defaults to 0) -->
        <working name="When 'a' and 'b' are supplied but 'c' is defaulted to 0">
            <xsl:call-template name="binary-or-ternary-demo">
                <xsl:with-param name="a" select="4"/>
                <xsl:with-param name="b" select="3"/>
            </xsl:call-template>
        </working>

        <!-- Calling a function with a variable number of arguments -->
        <!--
            Templates are not varargs-capable. Variable numbers of arguments
            usually appear in the form of a nodeset which is then bound to a
            single parameter name.
        -->

        <!-- Calling a function with named arguments -->
        <!--
            Other than what comes with the current context, template
            arguments are always named and can be supplied in any order.
            Templates do not support positional arguments. Additionally,
            even arguments not specified by the template may be passed; they
            are silently ignored.
        -->

        <!-- Using a function in first-class context within an expression -->
        <!-- Templates are not first-class values in XSLT 1.0. -->

        <!-- Obtaining the return value of a function -->
        <!--
            The output of a template is interpolated into the place of the
            call. Often, this is directly into the output of the transform,
            as with most of the above examples. However, it is also possible
            to bind the output as a variable or parameter. This is useful
            for using templates to compute parameters for other templates or
            for XPath functions.
        -->
        <!-- Which is the least of 34, 78, 12, 56? -->
        <xsl:variable name="lesser-demo-result">
            <!-- The variable is bound to the output of this call -->
            <xsl:call-template name="lesser-value">
                <xsl:with-param name="a">
                    <!-- A call as a parameter to another call -->
                    <xsl:call-template name="lesser-value">
                        <xsl:with-param name="a" select="34"/>
                        <xsl:with-param name="b" select="78"/>
                    </xsl:call-template>
                </xsl:with-param>
                <xsl:with-param name="b">
                    <!-- and again -->
                    <xsl:call-template name="lesser-value">
                        <xsl:with-param name="a" select="12"/>
                        <xsl:with-param name="b" select="56"/>
                    </xsl:call-template>
                </xsl:with-param>
            </xsl:call-template>
        </xsl:variable>
        <!-- The variable is used here in an XPath expression -->
        <line>
            <xsl:value-of select="concat('And the answer, which should be 12, is ', $lesser-demo-result, ', of course.')"/>
        </line>

        <!-- Distinguishing built-in functions and user-defined functions -->
        <!-- Virtually all templates are user-defined. -->

    </xsl:template>

    <!-- Templates supporting template demos above -->
    <xsl:template match="/*" mode="nullary-context-demo-mode">
        <xsl:call-template name="nullary-context-demo"/>
    </xsl:template>

    <xsl:template name="nullary-demo">
        <line>No parameters needed here!</line>
    </xsl:template>

    <xsl:template name="nullary-context-demo">
        <!-- When a template is called it has access to the current node of the caller -->
        <xsl:for-each select="self::*">
            <line>The context element here is named "<xsl:value-of select="local-name()"/>"</line>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="ternary-demo">
        <!-- This demo requires, at least semantically, all three parameters. -->
        <xsl:param name="a"/>
        <xsl:param name="b"/>
        <xsl:param name="c"/>
        <line>(<xsl:value-of select="$a"/> * <xsl:value-of select="$b"/>) + <xsl:value-of select="$c"/> = <xsl:value-of select="($a * $b) + $c"/></line>
    </xsl:template>

    <xsl:template name="binary-or-ternary-demo">
        <!-- This demo requires the first two parameters, but defaults the third to 0 if it is not supplied. -->
        <xsl:param name="a"/>
        <xsl:param name="b"/>
        <xsl:param name="c" select="0"/>
        <line>(<xsl:value-of select="$a"/> * <xsl:value-of select="$b"/>) + <xsl:value-of select="$c"/> = <xsl:value-of select="($a * $b) + $c"/></line>
    </xsl:template>

    <xsl:template name="lesser-value">
        <xsl:param name="a"/>
        <xsl:param name="b"/>
        <xsl:choose>
            <xsl:when test="number($a) &lt; number($b)">
                <xsl:value-of select="$a"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$b"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
</xsl:stylesheet>

```



## Yabasic


```Yabasic

sub test(a, b, c) :	print a, b, c : end sub

test(1, 2, 3)	// show 1 2 3
test(1, 2)	// show 1 2 0

execute("test", 1, 2, 3)	// show 1 2 3

sub test$(a$)	// show all members of a "list"
	local n, i, t$(1)

	n = token(a$, t$(), ", ")
	for i = 1 to n
		print t$(i), " ";
	next
end sub

test$("1, 2, 3, 4, text, 6, 7, 8, \"include text\"")
print
```



## zkl

The syntax and semantics of function calls is the always the same: name/object(parameters). All calls are varargs, it is up to the callee to do default/optional parameter handling (but that is hidden from the programmer). No named parameters. Pass by reference or value, depending.

Using f has a function, method or object:

```zkl
f(); f(1,2,3,4);
fcn f(a=1){}()  // define and call f, which gets a set to 1
fcn{vm.arglist}(1,2,3,4) // arglist is L(1,2,3,4)
fcn{a1:=vm.nthArg(1)}(1,2,3)  // a1 == 2
(f() == True); (f() and 1 or 2)
if (f()) println()
f(f) // pass f to itself
s:=f()
fcn{}.isType(self.fcn)  //True
fcn{}.len.isType(self.fcn)  //False, len is a Method
```

Partial application is done with the .fp* methods or the 'wrap keyword

```zkl

fcn(a,b,c).fp(1)()   // call function with a always set to 1
fcn(a,b,c).fp1(2,3)() // call function with b & c always set to 2 & 3
fcn(a,b,c,d).fpN(3,5)() // call function with d always set to 5
fcn{vm.arglist}.fpN(3,66)(1,2,3,4,5) //-->L(1,2,3,66,4,5)
fcn{}.fpM("01-",5) // use a mask to select parameters
   // 1 is supplied, 0 is get upon call, - is chop arglist
fcn{vm.arglist}.fpM("01-",66)(1,2,3,4) //-->L(1,66)

a:=5; f('wrap(b){a+b}) // 'wrap is syntactic sugar for .fpN
   // to create a lexical closure --> f(fcn(b,a){a+b}.fpN(1,a))
```



## zonnon


```zonnon

module CallingProcs;
	type
		{public} Vector = array {math} * of integer;

	var
		nums: array {math} 4 of integer;
		ints: Vector;
		total: integer;

		procedure Init(): boolean; (* private by default *)
		begin
			nums := [1,2,3,4];
			ints := new Vector(5);
			ints := [2,4,6,8,10];
			return true;
		end Init;

		(* function *)
		procedure Sum(v: Vector): integer;
		var
			i,s: integer;
		begin
			s := 0;
			for i := 0 to len(v) - 1 do
				(* inc is a predefined subroutine *)
				inc(s,v[i])
			end;
			return s
		end Sum;

		(* subroutine
		 * @param v: by value
		 * @param t: by reference
		 *)
		procedure Sum2(v: array {math} * of integer; var t: integer);
		var
			i: integer;
		begin
			t := 0;
			for i := 0 to len(v) - 1 do
				inc(t,v[i])
			end
		end Sum2;
	begin
		Init; (* calling a function without parameters *)
		total := Sum(nums);
		writeln(total);
		(* optional arguments not supported *)
		(* variable arguments through open arrays *)
		writeln(Sum(ints));
		(* named arguments not supported *)
		ints := [1,3,5,7,9];
		Sum2(ints,total);
		writeln(total);
	end CallingProcs.

```



## ZX Spectrum Basic


On the ZX Spectrum, functions and subroutines are separate entities. A function is limited to being a single expression that generates a return value. Statements are not allowed within a function. A subroutine can perform input and output and can contain statements.


```zxbasic
10 REM functions cannot be called in statement context
20 PRINT FN a(5): REM The function is used in first class context. Arguments are not named
30 PRINT FN b(): REM Here we call a function that has no arguments
40 REM subroutines cannot be passed parameters, however variables are global
50 LET n=1: REM This variable will be visible to the called subroutine
60 GO SUB 1000: REM subroutines are called by line number and do not have names
70 REM subroutines do not return a value, but we can see any variables it defined
80 REM subroutines cannot be used in first class context
90 REM builtin functions are used in first class context, and do not need the FN keyword prefix
100 PRINT SIN(50): REM here we pass a parameter to a builtin function
110 PRINT RND(): REM here we use a builtin function without parameters
120 RANDOMIZE: REM statements are not functions and cannot be used in first class context.
```


{{omit from|GUISS}}
