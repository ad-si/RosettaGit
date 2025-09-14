+++
title = "Compile-time calculation"
description = ""
date = 2019-08-09T15:14:27Z
aliases = []
[extra]
id = 5380
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dwscript",
  "echolisp",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "lingo",
  "m4",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oxygenbasic",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "racket",
  "rexx",
  "ring",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "ursala",
  "visual_basic_.net",
  "xlisp",
  "xpl0",
  "zkl",
]
+++

Some programming languages allow calculation of values at compile time.


## Task

Calculate   <big> 10! </big>   (ten factorial)   at compile time.

Print the result when the program is run.

Discuss what limitations apply to compile-time calculations in your language.





## 360 Assembly

First example with the assembler equivalence pseudo instruction (EQU):

```360asm
COMPCALA CSECT
         L      R1,=A(FACT10)      r1=10!
         XDECO  R1,PG
         XPRNT  PG,L'PG            print buffer
         BR     R14                exit
FACT10   EQU    10*9*8*7*6*5*4*3*2*1   factorial computation
PG       DS     CL12
```

{{out}} in the assembler listing ( 375F00 hexadecimal of 10!)

```txt

.... 00375F00 .... FACT10   EQU    10*9*8*7*6*5*4*3*2*1   factorial computation

```

{{out}} at execution time

```txt

     3628800

```

Second example with an assembler macro instruction:

```360asm
         MACRO
&LAB     FACT   &REG,&N            parameters
&F       SETA   1                  f=1
&I       SETA   1                  i=1
.EA      AIF    (&I GT &N).EB      ea: if i>n then goto eb
&F       SETA   &F*&I              f=f*i
&I       SETA   &I+1               i=i+1
         AGO    .EA                goto ea
.EB      ANOP                      eb:
         MNOTE  0,'Load &REG with &N! = &F'   macro note
&LAB     L      &REG,=A(&F)        load reg with factorial
         MEND                      macro end
COMPCALB CSECT
         USING  COMPCALB,R12       base register
         LR     R12,R15            set base register
         FACT   R1,10              macro call
         XDECO  R1,PG
         XPRNT  PG,L'PG            print buffer
         BR     R14                exit
PG       DS     CL12
         YREGS
         END    COMPCALB
```

{{out}} in the assembler listing

```txt

          FACT   R1,10              macro call
+         MNOTE 'Load R1 with 10! = 3628800'   macro note
+         L      R1,=A(3628800)        load reg with factorial

```



## Ada

Here's a hardcoded version:

```ada

with Ada.Text_Io;
procedure CompileTimeCalculation is
   Factorial : constant Integer := 10*9*8*7*6*5*4*3*2*1;

begin
   Ada.Text_Io.Put(Integer'Image(Factorial));
end CompileTimeCalculation;

```

And here's a recursive function version that prints the exact same thing.

```ada

with Ada.Text_Io;
procedure CompileTimeCalculation is

   function Factorial (Int : in Integer) return Integer is
   begin
      if Int > 1 then
         return Int * Factorial(Int-1);
      else
         return 1;
      end if;
   end;

     Fact10 : Integer := Factorial(10);
begin
   Ada.Text_Io.Put(Integer'Image(Fact10));
end CompileTimeCalculation;
```


===Unbounded Compile-Time Calculation===

An interesting property of Ada is that such calculations at compile time are performed with mathematical (i.e., unbounded) integers for intermediate results. On a compiler with 32-bit integers (gcc), the following code prints the value of '20 choose 10' = 184756:


```Ada
with Ada.Text_IO;

procedure Unbounded_Compile_Time_Calculation is
   F_10 : constant Integer := 10*9*8*7*6*5*4*3*2*1;
   A_11_15 : constant Integer := 15*14*13*12*11;
   A_16_20 : constant Integer := 20*19*18*17*16;
begin
   Ada.Text_IO.Put_Line -- prints out
     ("20 choose 10 =" & Integer'Image((A_11_15 * A_16_20 * F_10) / (F_10 * F_10)));
--   Ada.Text_IO.Put_Line -- would not compile
--     ("Factorial(20) =" & Integer'Image(A_11_15 * A_16_20 * F_10));
end Unbounded_Compile_Time_Calculation;
```


The same compiler refuses to compile the two two lines


```Ada
   Ada.Text_IO.Put_Line -- would not compile
      ("Factorial(20) =" & Integer'Image(A_11_15 * A_16_20 * F_10));
```


because the final result A_11_15 * A_16_20 * F_10 is a '''value not in range of type "Standard.Integer"''' -- the same intermediate value that was used above to compute '20 choose 10'.


## BASIC

Most BASICs perform compile-time calculation on anything they can determine is a constant. This can either be done explicitly:

```qbasic
CONST factorial10 = 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
```


or implicitly:


```qbasic
DIM factorial10 AS LONG
factorial10 = 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
```


In both cases, the identifier <code>factorial10</code> is given the value 3628800 without any runtime calculations, although in many (or perhaps most) BASICs the first one is handled similarly to C's <code>#define</code>: if it isn't used elsewhere in the code, it doesn't appear at all in the final executable.

## C

C includes a macro processor that runs at compile-time. With the use of suitably imaginative macro library includes, it can be used in a similar way to C++ template metaprogramming or Lisp macros. Like C++, and unlike Lisp, the language used is usually very different from the C language used to write runtime code.

The Order macro library [[Order|implements a full virtual machine and high-level, functional programming language]] available to C programs at compile-time:

```c
#include <stdio.h>
#include <order/interpreter.h>

#define ORDER_PP_DEF_8fac ORDER_PP_FN( \
8fn(8X, 8seq_fold(8times, 1, 8seq_iota(1, 8inc(8X)))) )

int main(void) {
	printf("10! = %d\n", ORDER_PP( 8to_lit( 8fac(10) ) ) );
	return 0;
}
```


In this example, the <code>8fac</code> function computes the factorial by folding <code>8times</code> (the binary multiplication primitive) over a numeric range created by <code>8seq_iota</code> (which requires <code>8X</code> to be incremented, as it creates lists from L to R-1), a familiar way of computing this in functional languages. The result of the factorial calculation is an internal, "native" number which need to be converted to decimal by the <code>8to_lit</code> function.

If the compiler allows, run the preprocessor only (the <code>-E</code> option with GCC) to see the result in place.

```txt
3628800
```


This and similar macro libraries are very demanding on the preprocessor, and will require a standards-compliant implementation such as GCC.

===C (simpler version)===
This is a simple version, showing that 10! was computed at compile time:

```c
#include <stdio.h>
const int val = 2*3*4*5*6*7*8*9*10;
int main(void) {
	printf("10! = %d\n", val );
	return 0;
}
```


```txt
10! = 3628800
```


asm from compiler

```txt
$ gcc 10fact.c -S
$ cat 10fact.s
        .file   "10fact.c"
        .globl  val
        .section .rdata,"dr"
        .align 4
val:
        .long   3628800
        .def    __main; .scl    2;      .type   32;     .endef
.LC0:
        .ascii "10! = %d\12\0"
        .text
        .globl  main
        .def    main;   .scl    2;      .type   32;     .endef
        .seh_proc       main
main:
        pushq   %rbp
        .seh_pushreg    %rbp
        movq    %rsp, %rbp
        .seh_setframe   %rbp, 0
        subq    $32, %rsp
        .seh_stackalloc 32
        .seh_endprologue
        call    __main
        movl    $3628800, %eax   # critical line showing the compiler computed the result.
        movl    %eax, %edx
        leaq    .LC0(%rip), %rcx
        call    printf
        movl    $0, %eax
        addq    $32, %rsp
        popq    %rbp
        ret
        .seh_endproc
        .ident  "GCC: (GNU) 4.9.3"
        .def    printf; .scl    2;      .type   32;     .endef

```



## C++

This is called [[wp:Template metaprogramming|Template metaprogramming]]. In fact, templates in C++ are Turing-complete, making deciding whether a program will compile undecidable.

```cpp
#include <iostream>

template<int i> struct Fac
{
    static const int result = i * Fac<i-1>::result;
};

template<> struct Fac<1>
{
    static const int result = 1;
};


int main()
{
    std::cout << "10! = " << Fac<10>::result << "\n";
    return 0;
}
```


Compile-time calculations in C++ look quite different from normal code. We can only use templates, type definitions and a subset of integer arithmetic. It is not possible to use iteration. C++ compile-time programs are similar to programs in pure functional programming languages, albeit with a peculiar syntax.

Alternative version, using constexpr in C++11:


```cpp
#include <stdio.h>

constexpr int factorial(int n) {
    return n ? (n * factorial(n - 1)) : 1;
}

constexpr int f10 = factorial(10);

int main() {
    printf("%d\n", f10);
    return 0;
}
```

Output:

```txt
3628800
```

The asm produced by G++ 4.6.0 32 bit (-std=c++0x -S), shows the computation is done at compile-time:

```asm
_main:
	pushl	%ebp
	movl	%esp, %ebp
	andl	$-16, %esp
	subl	$16, %esp
	call	___main
	movl	$3628800, 4(%esp)
	movl	$LC0, (%esp)
	call	_printf
	movl	$0, %eax
	leave
	ret
```



## C#

'''Compiler:''' Roslyn C#, language version 7.3

The Roslyn compiler performs constant folding at compile-time and emits IL that contains the result.


```c#
using System;

public static class Program
{
    public const int FACTORIAL_10 = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1;
    static void Main()
    {
        Console.WriteLine(FACTORIAL_10);
    }
}
```


<!--CIL assembly has a similar overall syntax to C#, so colorize it as such.-->

```c#
.class public auto ansi abstract sealed beforefieldinit Program
	extends [System.Runtime]System.Object
{
	// Fields
	.field public static literal int32 FACTORIAL_10 = int32(3628800)

	// Methods
	.method private hidebysig static
		void Main () cil managed
	{
		// Method begins at RVA 0x2050
		// Code size 11 (0xb)
		.maxstack 8
		.entrypoint

		IL_0000: ldc.i4 3628800
		IL_0005: call void [System.Console]System.Console::WriteLine(int32)
		IL_000a: ret
	} // end of method Program::Main

} // end of class Program
```


Note that the constant field is generated only when both it and the containing class are visible outside of the assembly.

Constant expressions that appear outside of constant declarations are also folded, so


```c#
using System;

static class Program
{
    static void Main()
    {
        Console.WriteLine(10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1);
    }
}
```


and


```c#
using System;

static class Program
{
    static void Main()
    {
        int factorial;
        factorial = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1;
        Console.WriteLine(factorial);
    }
}
```


produce the same IL, except without the field.

```c#
.class private auto ansi abstract sealed beforefieldinit Program
	extends [System.Runtime]System.Object
{
	// Methods
	.method private hidebysig static
		void Main () cil managed
	{
		// Method begins at RVA 0x2050
		// Code size 11 (0xb)
		.maxstack 8
		.entrypoint

		IL_0000: ldc.i4 3628800
		IL_0005: call void [System.Console]System.Console::WriteLine(int32)
		IL_000a: ret
	} // end of method Program::Main

} // end of class Program
```


```txt
3628800
```



## Clojure


```clojure

(defn fac [n] (apply * (range 1 (inc n))))
(defmacro ct-factorial [n] (fac n))
```



## Common Lisp


Assuming a definition from [[Factorial function#Common Lisp]], we first have to make a small adjustment so that the function is available at compile time. Common Lisp does not have a single image building and deployment model. For instance, Common Lisp implementations can support a "C like" model whereby a compiler is invoked as a separate process to handle individual files, which are then loaded to form an image (analogous to linking).   A Lisp compiler will not make available to itself the functions in a source file which it happens to be compiling, unless told to do so:


```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun factorial ...))
```


With that, here are ways to do compile-time evaluation:


```lisp
(defmacro ct-factorial (n)
  (factorial n))

...

(print (ct-factorial 10))
```


The <code>factorial</code> function must be defined before any use of the <code>ct-factorial</code> macro is evaluated or compiled.

If the data resulting from the compile-time calculation is not necessarily a number or other [http://www.lispworks.com/documentation/HyperSpec/Body/03_abac.htm self-evaluating object], as it is in the factorial case, then the macro must quote it to avoid it being interpreted as code (a form):


```lisp
(defmacro ct-factorial (n)
  `(quote ,(factorial n)))

; or, equivalently,
(defmacro ct-factorial (n)
  `',(factorial n))
```


It is also possible to have a value [http://www.lispworks.com/documentation/HyperSpec/Body/s_ld_tim.htm computed at ''load time''], when the code is loaded into the process, rather than at compile time; this is useful if the value to be computed contains objects that do not yet exist at compile time, or the value might vary due to properties which might be different while yet using the same compiled program (e.g. pathnames), but it is still constant for one execution of the program:


```lisp
(print (load-time-value (factorial 10)))
```


Further it's also possible to have the value computed at read time using the read macro <code>#.</code> .


```lisp
(print (#. (factorial 10)))
```


Lastly, Common Lisp has "compiler macros" which are user-defined handlers for function call optimization. A compiler macro is defined which has the same name as some user-defined function. When calls to that function are being compiled, they pass through the macro. The macro must analyze the arguments and rewrite the function call into something else, or return the original form.


```lisp
(define-compiler-macro factorial (&whole form arg)
  (if (constantp arg)
    (factorial arg)
    form))
```


Test with CLISP (taking advantage of its <code>!</code> function) showing how a factorial call with a constant argument of 10 ends up compiled to the constant 3268800, but a factorial call with the argument a is compiled to a variable access and function call:


```txt
[1]> (defun factorial (x) (! x))
FACTORIAL
[2]> (define-compiler-macro factorial (&whole form arg)
  (if (constantp arg)
    (factorial arg)
    form))
FACTORIAL
[3]> (defun test-constant () (factorial 10))
TEST-CONSTANT
[4]> (disassemble 'test-constant)

Disassembly of function TEST-CONSTANT
(CONST 0) = 3628800
[ .. snip ... ]
0     (CONST 0)                           ; 3628800
1     (SKIP&RET 1)
NIL
[5]> (defun test-nonconstant () (factorial a))
TEST-NONCONSTANT
[6]> (disassemble 'test-nonconstant)
WARNING in TEST-NONCONSTANT :
A is neither declared nor bound,
it will be treated as if it were declared SPECIAL.

Disassembly of function TEST-NONCONSTANT
(CONST 0) = A
(CONST 1) = FACTORIAL
[ .. snip ... ]
reads special variable: A
3 byte-code instructions:
0     (GETVALUE&PUSH 0)                   ; A
2     (CALL1 1)                           ; FACTORIAL
4     (SKIP&RET 1)
NIL
```



## D

The D compiler is able to run many functions at compile-time [http://dlang.org/function.html#interpretation Compile Time Function Execution (CTFE)]:

```d
long fact(in long x) pure nothrow @nogc {
    long result = 1;
    foreach (immutable i; 2 .. x + 1)
        result *= i;
    return result;
}

void main() {
    // enum means "compile-time constant", it forces CTFE.
    enum fact10 = fact(10);

    import core.stdc.stdio;

    printf("%ld\n", fact10);
}
```


The 32-bit asm generated by DMD shows the computation is done at compile-time:

```asm
__Dmain
    push EAX
    mov  EAX,offset FLAT:_DATA
    push 0
    push 0375F00h
    push EAX
    call near ptr _printf
    add  ESP,0Ch
    xor  EAX,EAX
    pop  ECX
    ret
```



## Delphi

:''See [[#Pascal|Pascal]]''


## DWScript

In DWScript, constant expressions and referentially-transparent built-in functions, such as ''Factorial'', are evaluated at compile time.


```delphi
const fact10 = Factorial(10);
```



## EchoLisp

'''define-constant''' may be used to compute data, which in turn may be used in other define-constant, or in the final code.

```scheme

(define-constant DIX! (factorial 10))
(define-constant DIX!+1 (1+ DIX!))

(writeln DIX!+1)
3628801

```



## Erlang

This is a placeholder since to do something more complex than text substitution macros Erlang offers parse transformations. This is a quote from their documentation: "Programmers are strongly advised not to engage in parse transformations". Somebody can do this task, but not I.


## Factor

Technically, this calculation happens at parse-time, before any compilation takes place.  Calculating factorial at compile-time is not useful in Factor.


```factor
: factorial ( n -- n! ) [1,b] product ;

CONSTANT: 10-factorial $[ 10 factorial ]
```



## Forth

During a word definition, you can drop out of the compilation state with '''[''' and go back in with ''']'''. (This is where the naming conventions for '''[CHAR]''' and '''[']''' come from.) There are several flavors of '''LITERAL''' for compiling the result into the word.


```forth
: fac ( n -- n! ) 1 swap 1+ 2 max 2 ?do i * loop ;

: main  ." 10! = " [ 10 fac ] literal . ;

see main
: main
  .\" 10! = " 3628800 . ; ok
```


Outside of a word definition, it's fuzzy.  If the following code is itself followed by a test and output, and is run in a script, then the construction of the '''bignum''' array (and the perhaps native-code compilation of '''more''') happens at runtime.  If the following code is followed by a command that creates an executable, the array will not be rebuilt on each run.


```forth

: more  ( "digits" -- )  \ store "1234" as 1 c, 2 c, 3 c, 4 c,
  parse-word bounds ?do
    i c@ [char] 0 - c,
  loop ;

create bignum
more 73167176531330624919225119674426574742355349194934
more 96983520312774506326239578318016984801869478851843
...
```



## Fortran

In Fortran, parameters can be defined where the value is computed at compile time:

```fortran
 program test

   implicit none
   integer,parameter :: t = 10*9*8*7*6*5*4*3*2  !computed at compile time

   write(*,*) t  !write the value the console.

 end program test

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Calculations can be done in a Const declaration at compile time
' provided only literals or other constant expressions are used

Const factorial As Integer = 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10
Print factorial ' 3628800
Sleep
```



## Go

Constant expressions are evaluated at compile time.  A constant expression though, is pretty simple and can't have much more than literals, operators, and a special thing called iota.  There is no way to loop in a constant expression and so the expanded expression below is about the simplest way of completing this task.

```go
package main

import "fmt"

func main() {
    fmt.Println(2*3*4*5*6*7*8*9*10)
}
```



## Haskell


With Template Haskell, it is quite easy to do compile time embedding. The functions used at compile-time need to be already compiled. Therefore, you generally need two modules.


```haskell
module Factorial where
import Language.Haskell.TH.Syntax

fact n = product [1..n]

factQ :: Integer -> Q Exp
factQ = lift . fact
```



```haskell
{-# LANGUAGE TemplateHaskell #-}
import Factorial

main = print $(factQ 10)
```


Note: Doing <code>$([|fact 10|])</code> is the same than doing <code>fact 10</code>. <code>[|something|]</code> returns the abstract syntax tree of <code>something</code>. Thus <code>[|fact 10|]</code> returns the AST of the call to the <code>fact</code> function with 10 as argument. <code>$(something)</code> waits for an AST from a call to <code>something</code>.


## J


J is an interpreter, and not a compiler, so could be said to not have any "compile time".  Nevertheless, J tacit programs are stored using an internal representation -- the program is parsed once, well before it is used.

Thus, a program which prints 10 factorial:


```J
pf10=: smoutput bind (!10)
```


When the definition of pf10 is examined, it contains the value 3628800.  J has several ways of representing tacit programs.  Here all five of them are presented for this program (the last two happen to look identical for this trivial case):


```J
   9!:3]1 2 4 5 6

   pf10
┌───────────────────────────────────────┐
│┌─┬───────────────────────────────────┐│
││@│┌────────┬────────────────────────┐││
││ ││smoutput│┌─┬────────────────────┐│││
││ ││        ││"│┌────────────┬─────┐││││
││ ││        ││ ││┌─┬────────┐│┌─┬─┐│││││
││ ││        ││ │││0│3.6288e6│││0│_││││││
││ ││        ││ ││└─┴────────┘│└─┴─┘│││││
││ ││        ││ │└────────────┴─────┘││││
││ ││        │└─┴────────────────────┘│││
││ │└────────┴────────────────────────┘││
│└─┴───────────────────────────────────┘│
└───────────────────────────────────────┘
┌────────┬─┬──────────────┐
│smoutput│@│┌────────┬─┬─┐│
│        │ ││3.6288e6│"│_││
│        │ │└────────┴─┴─┘│
└────────┴─┴──────────────┘
      ┌─ smoutput
── @ ─┤          ┌─ 3628800
      └─ " ──────┴─ _
smoutput@(3628800"_)
smoutput@(3628800"_)
```


Finally, when this program is run, it displays this number:


```J
   pf10 ''
3628800
```


Note: Currently, the mediawiki implementation is corrupting the above display due to a cascading sequence of bad design decisions and mis-interpreted specifications on the part of someone "contributing" to that implementation. To work around this issue, and see the original display, you can currently use either the "Edit" or "View Source" option, depending on whether you are logged in to rosettacode with an account that has edit rights here. (Please don't actually save changes though.) If you are using View Source, you might want to do that in a new tab (so you also stay here with this view) and use your browser's search capability to quickly scroll to this location in the source view.


## Julia

Julia includes a powerful macro feature that can perform arbitrary code transformations at compile-time (or technically at parse-time), and can also execute arbitrary Julia code.  For example, the following macro computes the factorial of <code>n</code> (a literal constant) and returns the value (e.g. to inline it in the resulting source code)

```julia
macro fact(n)
  factorial(n)
end
```

If we now use this in a function, e.g.

```julia
foo() = @fact 10
```

then the value of 10! = 3628800 is computed at parse-time and is inlined in the compiled function <code>foo</code>, as can be verified by inspecting the assembly code via the built-in function <code>code_native(foo, ())</code>.


## Kotlin

Compile time calculations are possible in Kotlin using the 'const' modifier provided one sticks to literals or other constants when specifying the calculation to be performed:

```scala
// version 1.0.6
const val TEN_FACTORIAL = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2

fun main(args: Array<String>) {
    println("10! = $TEN_FACTORIAL")
}
```


```txt

10! = 3628800

```



## Lingo

As an interpreted language with the interpreter always being present, Lingo has no clear separation of compile-time and runtime. Whenever you change the code of a script at runtime, it's immediately (re)compiled to bytecode (in memory). You can also create new scripts at runtime:

```lingo
-- create new (movie) script at runtime
m = new(#script)

-- the following line triggers compilation to bytecode
m.scriptText = "on fac10"&RETURN&"return "&(10*9*8*7*6*5*4*3*2)&RETURN&"end"

put fac10()
-- 3628800
```



## m4

m4 expands macros at run time, not compile time. If m4 is a front end to some other langugage, then m4's run time is part of other language's compile time.

This example uses m4 as a front end to [[AWK]]. m4 calculates factorial of 10, where AWK program calls macro.


```m4
define(`factorial',
`ifelse($1, 0, 1, `eval($1 * factorial(eval($1 - 1)))')')dnl
dnl
BEGIN {
	print "10! is factorial(10)"
}
```


One runs <code>m4 program.m4 > program.awk</code> to make this valid AWK program.


```awk
BEGIN {
	print "10! is 3628800"
}
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica is not a compiled language, you can construct compiled functions in Mathematica by the build-in function "Compile". Constants are calculated at "compile-time".
<lang>f = Compile[{}, 10!]
```

```txt
CompiledFunction[{},3628800,-CompiledCode-]
```

<lang>f[]
```

```txt
3628800
```



## Nim

Nim can evaluate procedures at compile-time, this can be forced by calling a procedure with a const keyword like so:


```Nim
proc fact(x: int): int =
  result = 1
  for i in 2..x:
    result = result * i

const fact10 = fact(10)
echo(fact10)
```


We can see that this is evaluated at compile-time by looking at the generated C code:


```C
...
STRING_LITERAL(TMP122, "3628800", 7);
...
```


The Nim compiler can also be told to try to evaluate procedures at compile-time even for variables by using the --implicitStatic:on command line switch.
The Nim compiler performs a side effect analysis to make sure that the procedure is side effect free, if it is not; a compile-time error is raised.

=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE CompileTime;
IMPORT
  Out;
CONST
    tenfac = 10*9*8*7*6*5*4*3*2;
BEGIN
  Out.String("10! =");Out.LongInt(tenfac,0);Out.Ln
END CompileTime.

```



## Objeck

Objeck will fold constants at compiler time as long as the -s2 or -s3 compiler switches are enabled.


```objeck

bundle Default {
  class CompileTime {
    function : Main(args : String[]) ~ Nil {
      (10*9*8*7*6*5*4*3*2*1)->PrintLine();
    }
  }
}

```



## OCaml


OCaml does not calculate operations that involve functions calls, as for example factorial 10, but OCaml does calculate simple mathematical operations at compile-time, for example in the code below <code>(24 * 60 * 60)</code> will be replaced by its result <code>86400</code>.


```ocaml
let days_to_seconds n =
  let conv = 24 * 60 * 60 in
  (n * conv)
;;
```


It is easy to verify this using the argument <code>-S</code> to keep the intermediate assembly file:
 ocamlopt -S sec.ml
 grep 86400 sec.s
         imull   $<span style="color:red">86400</span>, %eax

If you wish to verify this property in your own projects, you have to know that integer values most often have their OCaml internal representation in the assembly, which for an integer <code>x</code> its OCaml internal representation will be <code>(((x) << 1) + 1)</code>. So for example if we modify the previous code for:


```ocaml
let conv = 24 * 60 * 60

let days_to_seconds n =
  (n * conv)
;;
```


 # (24 * 60 * 60) lsl 1 + 1 ;;
 - : int = 172801

 grep 172801 sec.s
         movl    $<span style="color:red">172801</span>, camlSec



## Oforth

Not easy to define "compile time" with Oforth : oforth interpreter read input and perform it. If that intput creates function or methods, it creates and compile them.

You can do any calculation you want before or after, create constants, ...


```Oforth
10 seq reduce(#*) Constant new: FACT10
: newFunction  FACT10 . ;
```


You can also calculate all factorials for 1 to 20 before defining fact method :

```Oforth
20 seq map(#[ seq reduce(#*) ]) Constant new: ALLFACTS
: fact(n)  n ifZero: [ 1 ] else: [ ALLFACTS at(n) ] ;

ALLFACTS println
```


```txt

[1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600, 6227020800, 871
78291200, 1307674368000, 20922789888000, 355687428096000, 6402373705728000, 12164510040883
2000, 2432902008176640000]

```



## OxygenBasic

To demonstrate compiler timing, A custom compiler is created here with the system performance counter to measure lapsed time. The source code is embedded for brevity.

To dimension a static  array, the macro Pling10 is resolved at compile time. The overall compile time (ready to execute) was around 23 milliseconds.


```oxygenbasic

'LIBRARY CALLS
'
### =======


extern lib "../../oxygen.dll"

declare o2_basic (string src)
declare o2_exec  (optional sys p) as sys
declare o2_errno () as sys
declare o2_error () as string

extern lib "kernel32.dll"

declare QueryPerformanceFrequency(quad*freq)
declare QueryPerformanceCounter(quad*count)

end extern

'EMBEDDED SOURCE CODE
'
### ==============


src=quote


### Source


def  Pling10 2*3*4*5*6*7*8*9*10

byte a[pling10] 'Pling10 is resolved to a number here at compile time

print pling10


### Source



'TIMER
'=====

quad ts,tc,freq
QueryPerformanceFrequency freq
QueryPerformanceCounter ts

'COMPILE/EXECUTE
'
### =========


o2_basic src

if o2_errno then
  print o2_error
else
  QueryPerformanceCounter tc
  print "Compile time: " str((tc-ts)*1000/freq, 1) " MilliSeconds"
  o2_exec 'Run the program
end if

```



## Oz


```oz
functor
import
   System Application
prepare
   fun {Fac N}
      {FoldL {List.number 1 N 1} Number.'*' 1}
   end
   Fac10 = {Fac 10}
define
   {System.showInfo "10! = "#Fac10}
   {Application.exit 0}
end
```


Code in the <code>prepare</code> section of a functor is executed at compile time. External modules that are used in this code must be imported with a <code>require</code> statement (not shown in this example). Such external functors must have been compiled before the current functor is compiled (<code>ozmake</code> will automatically take care of this).

It is possible to export variables that are defined in the <code>prepare</code> statement. However, such variables must not be stateful entities, e.g. it is not possible to export a cell that was defined at compile time.


## Pascal


All the variants of pascal have always been able to calculate the values of constants at compile time as long
as the values can be resolved.


```pascal

program in out;

const

X = 10*9*8*7*6*5*4*3*2*1 ;

begin

writeln(x);

end;

```



## Perl


There are few limits on code you can put in <code>BEGIN</code> blocks, which are executed at compile-time. Unfortunately, you can't in general save the compiled form of a program to run later. Instead, <code>perl</code> recompiles your program every time you run it.


```perl
my $tenfactorial;
print "$tenfactorial\n";

BEGIN
   {$tenfactorial = 1;
    $tenfactorial *= $_ foreach 1 .. 10;}
```


Note however that all constant folding is done at compile time, so this actually does the factorial at compile time.


```perl
my $tenfactorial = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2;
```




## Perl 6



```perl6
constant $tenfact = [*] 2..10;
say $tenfact;
```


Like Perl 5, we also have a BEGIN block, but it also works to introduce a blockless statement,
the value of which will be stored up to be used in the surrounding expression at run time:


```perl6
 say(BEGIN [*] 2..10);
```



## Phix

The Phix compiler uses constant folding/propagation, so running p -d on the following snippet

```Phix
integer a,b
a = 10*9*8*7*6*5*4*3*2*1
b = factorial(10)
?{a,b}
```

produces a listing file containing

```txt

;     1 integer a,b
;     2 a = 10*9*8*7*6*5*4*3*2*1
    mov [#0040278C] (a), dword 3628800    ;#0042904E: 307005 8C274000 005F3700   uv 00 00  1  15
;     3 b = factorial(10)
    mov ecx,5                             ;#00429058: 271 05000000               vu 02 00  1  15
    mov edx,85                            ;#0042905D: 272 55000000               uv 04 00  1  16
    call :%opFrame (factorial)            ;#00429062: 350 0BE80000               v  00 00  1  16
    ...

```

```txt

{3628800,3628800}

```



## PicoLisp

The PicoLisp "compiler" is the so-called "reader", which converts the
human-readable source code into nested internal pointer structures. When it
runs, arbitrary expressions can be executed with the backqoute and tilde
operators ([http://software-lab.de/doc/ref.html#macro-io read macros]).

```PicoLisp
(de fact (N)
   (apply * (range 1 N)) )

(de foo ()
   (prinl "The value of fact(10) is " `(fact 10)) )
```

Output:

```txt
: (pp 'foo)  # Pretty-print the function
(de foo NIL
   (prinl "The value of fact(10) is " 3628800) )
-> foo

: (foo)  # Execute it
The value of fact(10) is 3628800
-> 3628800
```



## PL/I


```PL/I

/* Factorials using the pre-processor. */
test: procedure options (main);



%factorial: procedure (N) returns (fixed);
   declare N fixed;
   declare (i, k) fixed;

   k = 1;
   do i = 2 to N;
      k = k*i;
   end;
   return (k);

%end factorial;

%activate factorial;

   declare (x, y) fixed decimal;
   x = factorial (4);
   put ('factorial 4  is ', x);
   y = factorial (6);
   put skip list ('factorial 6 is ', y);

end test;

```


Output from the pre-processor:

<lang>
/* Factorials using the pre-processor. */
test: procedure options (main);
   declare (x, y) fixed decimal;
   x =       24;
   put ('factorial 4  is ', x);
   y =      720;
   put skip list ('factorial 6 is ', y);
end test;

```


Execution results:

<lang>
factorial 4  is               24
factorial 6 is               720

```



## PowerShell


```PowerShell

function fact([BigInt]$n){
    if($n -ge ([BigInt]::Zero)) {
        $fact = [BigInt]::One
        ([BigInt]::One)..$n | foreach{
            $fact = [BigInt]::Multiply($fact, $_)
        }
        $fact

    } else {
        Write-Error "$n is lower than 0"
    }
}
"$((Measure-Command {$fact = fact 10}).TotalSeconds) Seconds"
$fact

```

<b>Output:</b>

```txt

0.0030411 Seconds
3628800

```



## PureBasic

PureBasic will do most calculation during compiling, e.g.

```PureBasic
a=1*2*3*4*5*6*7*8*9*10
```

could on a x86 be complied to

```txt
MOV    dword [v_a],3628800
```



## Racket


Racket, like most Lisp descendants, allows arbitrary code to be executed at compile-time.


```racket

#lang racket

;; Import the math library for compile-time
;; Note: included in Racket v5.3.2
(require (for-syntax math))

;; In versions older than v5.3.2, just define the function
;; for compile-time
;;
;; (begin-for-syntax
;;   (define (factorial n)
;;     (if (zero? n)
;;         1
;;         (factorial (- n 1)))))

;; define a macro that calls factorial at compile-time
(define-syntax (fact10 stx)
  #`#,(factorial 10))

;; use the macro defined above
(fact10)

```



## REXX


<!--

There is no mention of ''non-eligibility'' for this task in this Rosetta Code task requirements or prologue.

This Rosetta Code task did NOT restrict any language from solving this task, nor did it mention what wasn't
eligible.  Rosetta Code is a place for solutions, not exclusions. This is a place to show various languages solve (programming) problems and to compare their approaches and solutions.

REXX does have a compiler (for the OS and VM mainframes, as well as "compile" Personal REXX [the /C option], along with some other REXXes).

This REXX example accomplished the task of computing '''10!''' at compile time (or interpreted time), exactly as this task requested.

There is no need to fix the REXX code, it works as intended.

There are plenty other interpreted programs that are entered for this Rosetta Code problem, as well as
Mathematica, most BASICs, OCaml and Oforth seem to fall in the middle.  I'm sure that there are other interpreters as well.

-- ~~~~

-->

Since REXX is an interpreted language   (as are other languages entered for this Rosetta Code task),   run time is compile time.

```rexx
/*REXX program computes 10! (ten factorial) during REXX's equivalent of "compile─time". */

say '10! ='    !(10)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!: procedure;  !=1;            do j=2  to arg(1);    !=!*j;    end  /*j*/;        return !
```

'''output'''

```txt

10! = 3628800

```



## Ring


```ring

a = 10*9*8*7*6*5*4*3*2*1
b = factorial(10)
see a + nl
see b + nl

func factorial nr if nr = 1 return 1 else return nr * factorial(nr-1) ok

```


## Rust

The Rust compiler can automatically do optimizations in the code to calculate the factorial.


```rust
fn factorial(n: i64) -> i64 {
    let mut total = 1;
    for i in 1..n+1 {
        total *= i;
    }
    return total;
}

fn main() {
    println!("Factorial of 10 is {}.", factorial(10));
}
```


If we compile this with <code>rustc factorial.rs -O --emit asm</code> and inspect the outputted assembly, we can see <code>movq	$3628800, (%rsp)</code>. This means the result of 3628800 was calculated in compile-time rather than run-time.


## Scala


```Scala
object Main extends {
  val tenFactorial = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2

  def tenFac = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2

  println(s"10! = $tenFactorial", tenFac)
}
```

As it can been seen in the always heavily optimized run-time code the calculations are already computed for the field constant and function method.

```txt

  public int tenFac();
    descriptor: ()I
    flags: (0x0001) ACC_PUBLIC
    Code:
      stack=1, locals=1, args_size=1
         0: ldc           #28                 // int 3628800
         2: ireturn
      LocalVariableTable:
        Start  Length  Slot  Name   Signature
            0       3     0  this   L$line2/$read$$iw$$iw$Main$;
      LineNumberTable:
        line 14: 0

  public $line2.$read$$iw$$iw$Main$();
    descriptor: ()V
    flags: (0x0001) ACC_PUBLIC
    Code:
      stack=6, locals=1, args_size=1
         0: aload_0
         1: invokespecial #29                 // Method java/lang/Object."<init>":()V
         4: aload_0
         5: putstatic     #31                 // Field MODULE$:L$line2/$read$$iw$$iw$Main$;
         8: aload_0
         9: ldc           #28                 // int 3628800
        11: putfield      #25                 // Field tenFactorial:I
        14: getstatic     #36                 // Field scala/Predef$.MODULE$:Lscala/Predef$;
        17: new           #38                 // class scala/Tuple2
        20: dup
        21: new           #40                 // class java/lang/StringBuilder
```


## Seed7


Seed7 allows predefined and user defined initialisation expressions.
The ! operator is predefined, so no user defined function is necessary.


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const integer: factorial is !10;
  begin
    writeln(factorial);
  end func;
```



## Sidef

The compile-time evaluation is limited at a constant expression, which cannot refer at any other user-defined data, such as variables or functions.

```ruby
define n = (10!);
say n;
```


or:

```ruby
define n = (func(n){ n > 0 ? __FUNC__(n-1)*n : 1 }(10));
say n;
```



## Tcl

In Tcl, compilation happens dynamically when required rather than being a separate step. That said, it is possible to use the language's introspection engine to discover what code has been compiled to, making it easy to show that known-constant expressions are compiled to their results. Generating the expression to compile is then simple enough.

```tcl
proc makeFacExpr n {
    set exp 1
    for {set i 2} {$i <= $n} {incr i} {
        append exp " * $i"
    }
    return "expr \{$exp\}"
}
eval [makeFacExpr 10]
```

How to show that the results were compiled? Like this:

```tcl
% tcl::unsupported::disassemble script [makeFacExpr 10]
ByteCode 0x0x4de10, refCt 1, epoch 3, interp 0x0x31c10 (epoch 3)
  Source "expr {1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10}"
  Cmds 1, src 45, inst 3, litObjs 1, aux 0, stkDepth 1, code/src 0.00
  Commands 1:
      1: pc 0-1, src 0-44
  Command 1: "expr {1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10}"
    (0) push1 0 	# "3628800"
    (2) done

```

As you can see, that expression was transformed into just a push of the results (and an instruction to mark the end of the bytecode segment).


## Ursala

Any user-defined or library function callable at run time can also be called at compile time
and evaluated with no unusual ceremony involved.

```Ursala
#import nat

x = factorial 10

#executable&

comcal = ! (%nP x)--<''>
```

some notes:
* <code>x</code> is declared as a constant equal to ten factorial using the <code>factorial</code> function imported from the <code>nat</code> library.
* <code>%nP</code> is a function derived from the type expression <code>%n</code>, for natural numbers, which takes a natural number as an argument and maps it to a list of character strings suitable for printing
* The <code>#executable&</code> directive causes the function following to be compiled as a free standing executable transforming standard input to standard output thereby.
* The <code>--</code> operator represents list concatenation.
* The list containing the empty string is concatenated with <code>(%nP x)</code> so that the output will be terminated with a line break.
* The <code>!</code> operator makes a constant function of its operand, so that the compiled program will ignore its input and print <code>x</code> regardless.
Here is a bash session showing compilation of the above code into
a simple command line filter, and running it as the next command.

```txt

$ fun comcal.fun
fun: writing `comcal'
$ comcal < /dev/null
3628800

```

Similarly to the Ocaml and Tcl solutions, we can confirm that the calculation
has been performed at compile time by inspecting the object code.

```txt

$ fun comcal --decompile
main = constant <'3628800',''>

```



## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic, language version 15.8

The Roslyn compiler performs constant folding at compile-time and emits IL that contains the result.


```vbnet
Module Program
    Const FACTORIAL_10 = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1

    Sub Main()
        Console.WriteLine(FACTORIAL_10)
    End Sub
End Module
```


<!--CIL assembly has a similar overall syntax to C#, so colorize it as such.-->

```c#
.class private auto ansi sealed Program
	extends [System.Runtime]System.Object
{
	.custom instance void Microsoft.VisualBasic.CompilerServices.StandardModuleAttribute::.ctor() = (
		01 00 00 00
	)
	// Fields
	.field private static literal int32 FACTORIAL_10 = int32(3628800)

	// Methods
	.method public static
		void Main () cil managed
	{
		.custom instance void [System.Runtime]System.STAThreadAttribute::.ctor() = (
			01 00 00 00
		)
		// Method begins at RVA 0x2060
		// Code size 11 (0xb)
		.maxstack 8
		.entrypoint

		IL_0000: ldc.i4 3628800
		IL_0005: call void [System.Console]System.Console::WriteLine(int32)
		IL_000a: ret
	} // end of method Program::Main

} // end of class Program
```


Constant expressions that appear outside of constant declarations are also folded, so


```vbnet
Module Program
    Sub Main()
        Console.WriteLine(10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1)
    End Sub
End Module
```


and


```vbnet
Module Program
    Sub Main()
        Dim factorial As Integer
        factorial = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1
        Console.WriteLine(factorial)
    End Sub
End Module
```


produce the same IL, albeit without the constant field that other assemblies can reference.

```c#
.class private auto ansi sealed Program
	extends [System.Runtime]System.Object
{
	.custom instance void Microsoft.VisualBasic.CompilerServices.StandardModuleAttribute::.ctor() = (
		01 00 00 00
	)
	// Methods
	.method public static
		void Main () cil managed
	{
		.custom instance void [System.Runtime]System.STAThreadAttribute::.ctor() = (
			01 00 00 00
		)
		// Method begins at RVA 0x2060
		// Code size 11 (0xb)
		.maxstack 8
		.entrypoint

		IL_0000: ldc.i4 3628800
		IL_0005: call void [System.Console]System.Console::WriteLine(int32)
		IL_000a: ret
	} // end of method Program::Main

} // end of class Program
```


```txt
3628800
```



## XLISP

Macros can be used to evaluate expressions at compile time:

```lisp
(defmacro f10-at-compile-time () (* 2 3 4 5 6 7 8 9 10))
```

If the expression is <i>quoted</i>, however, it is <i>not</i> evaluated—it is inserted 'as is', and will be evaluated at run time:

```lisp
(defmacro f10-at-run-time () '(* 2 3 4 5 6 7 8 9 10))
```

To show what is going on, first start a REPL and define little functions that just invoke each macro:

```lisp
[1] (defun test-f10-ct () (f10-at-compile-time))

TEST-F10-CT
[2] (defun test-f10-rt () (f10-at-run-time))

TEST-F10-RT
```

Then use <tt>DECOMPILE</tt> to examine the bytecode generated for each function. First, the one where the calculation was performed at compile time:

```txt
[3] (decompile test-f10-ct)

TEST-F10-CT:0000 12 00       ARGSEQ 00 ; ()
TEST-F10-CT:0002 04 03       LIT 03 ; 3628800
TEST-F10-CT:0004 0d          RETURN
()
```

Here, 10! is included as the literal number 3628800. By contrast, if we decompile the function that uses the <tt>F10-AT-RUN-TIME</tt> macro:

```txt
[4] (decompile test-f10-rt)

TEST-F10-RT:0000 12 00       ARGSEQ 00 ; ()
TEST-F10-RT:0002 04 03       LIT 03 ; 10
TEST-F10-RT:0004 10          PUSH
TEST-F10-RT:0005 04 04       LIT 04 ; 9
TEST-F10-RT:0007 10          PUSH
TEST-F10-RT:0008 04 05       LIT 05 ; 8
TEST-F10-RT:000a 10          PUSH
TEST-F10-RT:000b 04 06       LIT 06 ; 7
TEST-F10-RT:000d 10          PUSH
TEST-F10-RT:000e 04 07       LIT 07 ; 6
TEST-F10-RT:0010 10          PUSH
TEST-F10-RT:0011 04 08       LIT 08 ; 5
TEST-F10-RT:0013 10          PUSH
TEST-F10-RT:0014 04 09       LIT 09 ; 4
TEST-F10-RT:0016 10          PUSH
TEST-F10-RT:0017 04 0a       LIT 0a ; 3
TEST-F10-RT:0019 10          PUSH
TEST-F10-RT:001a 04 0b       LIT 0b ; 2
TEST-F10-RT:001c 10          PUSH
TEST-F10-RT:001d 05 0c       GREF 0c ; *
TEST-F10-RT:001f 0c 09       TCALL 09
()
```

we see that it includes the instructions necessary to find the answer but not the answer itself.


## XPL0


```XPL0
code IntOut=11;
IntOut(0, 10*9*8*7*6*5*4*3*2);

```

Generates this 80386 assembly code:

```txt

        XOR     EAX,EAX
        PUSH    EAX
        MOV     EAX,3628800
        CALL    INTR11
        RET

```



## zkl

zkl has two ways to do compile time calculations: a variant of C's macros and "parse time" calculations (since the compiler is written in zkl, the parser just recurses).
File foo.zkl:

```zkl
const { [1..10].reduce('*).println(" parse time") }

#fcn fact(N) { [1..N].reduce('*).println(" tokenize time"); ""}
   // paste output of fact into source
#tokenize fact(10)

println("compiled program running.");
```

Run the program: zkl foo:
```txt

3628800 tokenize time
3628800 parse time
compiled program running.

```

Tokenize time can paste text into the source, parse time can inject a limited set of objects into the parse tree (and is used for things like __DATE__, __FILE__ constants).

