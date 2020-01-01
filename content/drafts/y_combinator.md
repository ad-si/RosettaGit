+++
title = "Y combinator"
description = ""
date = 2019-09-15T04:47:33Z
aliases = []
[extra]
id = 3988
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}
{{requires|First class functions}}
[[Category:Recursion]]

In strict [[wp:Functional programming|functional programming]] and the [[wp:lambda calculus|lambda calculus]], functions (lambda expressions) don't have state and are only allowed to refer to arguments of enclosing functions.
This rules out the usual definition of a recursive function wherein a function is associated with the state of a variable and this variable's state is used in the body of the function.

The [http://mvanier.livejournal.com/2897.html Y combinator] is itself a stateless function that, when applied to another stateless function, returns a recursive version of the function. The Y combinator is the simplest of the class of such functions, called [[wp:Fixed-point combinator|fixed-point combinators]].


;Task:
Define the stateless Y combinator and use it to compute [[wp:Factorial|factorials]] and [[wp:Fibonacci number|Fibonacci numbers]] from other stateless functions or lambda expressions.


;Cf:
* [http://vimeo.com/45140590 Jim Weirich: Adventures in Functional Programming]





## ALGOL 68

{{trans|Python}} Note: This specimen retains the original [[#Python|Python]] coding style.
{{wont work with|ALGOL 68|Revision 1 - scoping extensions to language used.}}
{{works with|ALGOL 68S|from Amsterdam Compiler Kit ( [[wp:Guido van Rossum|Guido van Rossum]]'s teething ring) with runtime scope checking turned off.}}
<!--
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - compile not currently available on my system.}}
{{wont work with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny] - correctly detects a scope violation}} -->

```algol68
BEGIN
  MODE F = PROC(INT)INT;
  MODE Y = PROC(Y)F;

# compare python Y = lambda f: (lambda x: x(x)) (lambda y: f( lambda *args: y(y)(*args)))#
  PROC y =      (PROC(F)F f)F: (  (Y x)F: x(x)) (  (Y z)F: f((INT arg )INT: z(z)( arg )));

  PROC fib = (F f)F: (INT n)INT: CASE n IN n,n OUT f(n-1) + f(n-2) ESAC;

  FOR i TO 10 DO print(y(fib)(i)) OD
END
```

<!--
ALGOL 68G Output demonstrating the runtime scope violation:

```txt

5     PROC y = (PROC(F)F f)F: ( (Y y1)F: y1(y1) ) ( (Y y2)F: (INT args)INT: y2(y2)(args) );
                                         1
a68g: runtime error: 1: F value from procedure is exported out of its scope (detected in Y closed-clause starting at "(" in this line).

```

-->


## AppleScript

AppleScript is not particularly "functional" friendly. It can, however, support the Y combinator.

AppleScript does not have anonymous functions, but it does have anonymous objects. The code below implements the latter with the former (using a handler (i.e. function) named 'lambda' in each anonymous object).

Unfortunately, an anonymous object can only be created in its own statement ('script'...'end script' can not be in an expression). Thus, we have to apply Y to the automatic 'result' variable that holds the value of the previous statement.

The identifier used for Y uses "pipe quoting" to make it obviously distinct from the y used inside the definition.

```AppleScript
-- Y COMBINATOR ---------------------------------------------------------------

on |Y|(f)
    script
        on |λ|(y)
            script
                on |λ|(x)
                    y's |λ|(y)'s |λ|(x)
                end |λ|
            end script

            f's |λ|(result)
        end |λ|
    end script

    result's |λ|(result)
end |Y|


-- TEST -----------------------------------------------------------------------
on run

    -- Factorial
    script fact
        on |λ|(f)
            script
                on |λ|(n)
                    if n = 0 then return 1
                    n * (f's |λ|(n - 1))
                end |λ|
            end script
        end |λ|
    end script


    -- Fibonacci
    script fib
        on |λ|(f)
            script
                on |λ|(n)
                    if n = 0 then return 0
                    if n = 1 then return 1
                    (f's |λ|(n - 2)) + (f's |λ|(n - 1))
                end |λ|
            end script
        end |λ|
    end script

    {facts:map(|Y|(fact), enumFromTo(0, 11)), fibs:map(|Y|(fib), enumFromTo(0, 20))}

    --> {facts:{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800},

    --> fibs:{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987,
    --           1597, 2584, 4181, 6765}}

end run


-- GENERIC FUNCTIONS FOR TEST -------------------------------------------------

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

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn
```

{{Out}}

```AppleScript
{facts:{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800},
fibs:{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765}}
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program Ycombi.s   */

/* REMARK 1 : this program use routines in a include file
   see task Include a file language arm assembly
   for the routine affichageMess conversion10
   see at end of this program the instruction include */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall


/*******************************************/
/* Structures                               */
/********************************************/
/* structure function*/
    .struct  0
func_fn:                    @ next element
    .struct  func_fn + 4
func_f_:                    @ next element
    .struct  func_f_ + 4
func_num:
    .struct  func_num + 4
func_fin:

/* Initialized data */
.data
szMessStartPgm:            .asciz "Program start \n"
szMessEndPgm:              .asciz "Program normal end.\n"
szMessError:               .asciz "\033[31mError Allocation !!!\n"

szFactorielle:             .asciz "Function factorielle : \n"
szFibonacci:               .asciz "Function Fibonacci : \n"
szCarriageReturn:          .asciz "\n"

/* datas message display */
szMessResult:            .ascii "Result value :"
sValue:                  .space 12,' '
                         .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                                           @ program start
    ldr r0,iAdrszMessStartPgm                   @ display start message
    bl affichageMess
    adr r0,facFunc                              @ function factorielle address
    bl YFunc                                    @ create Ycombinator
    mov r5,r0                                   @ save Ycombinator
    ldr r0,iAdrszFactorielle                    @ display message
    bl affichageMess
    mov r4,#1                                   @ loop counter
1:  @ start loop
    mov r0,r4
    bl numFunc                                  @ create number structure
    cmp r0,#-1                                  @ allocation error ?
    beq 99f
    mov r1,r0                                   @ structure number address
    mov r0,r5                                   @ Ycombinator address
    bl callFunc                                 @ call
    ldr r0,[r0,#func_num]                       @ load result
    ldr r1,iAdrsValue                           @ and convert ascii string
    bl conversion10
    ldr r0,iAdrszMessResult                     @ display result message
    bl affichageMess
    add r4,#1                                   @ increment loop counter
    cmp r4,#10                                  @ end ?
    ble 1b                                      @ no -> loop
/*********Fibonacci  *************/
    adr r0,fibFunc                              @ function factorielle address
    bl YFunc                                    @ create Ycombinator
    mov r5,r0                                   @ save Ycombinator
    ldr r0,iAdrszFibonacci                      @ display message
    bl affichageMess
    mov r4,#1                                   @ loop counter
2:  @ start loop
    mov r0,r4
    bl numFunc                                  @ create number structure
    cmp r0,#-1                                  @ allocation error ?
    beq 99f
    mov r1,r0                                   @ structure number address
    mov r0,r5                                   @ Ycombinator address
    bl callFunc                                 @ call
    ldr r0,[r0,#func_num]                       @ load result
    ldr r1,iAdrsValue                           @ and convert ascii string
    bl conversion10
    ldr r0,iAdrszMessResult                     @ display result message
    bl affichageMess
    add r4,#1                                   @ increment loop counter
    cmp r4,#10                                  @ end ?
    ble 2b                                      @ no -> loop
    ldr r0,iAdrszMessEndPgm                     @ display end message
    bl affichageMess
    b 100f
99:                                             @ display error message
    ldr r0,iAdrszMessError
    bl affichageMess
100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessStartPgm:        .int szMessStartPgm
iAdrszMessEndPgm:          .int szMessEndPgm
iAdrszFactorielle:         .int szFactorielle
iAdrszFibonacci:           .int szFibonacci
iAdrszMessError:           .int szMessError
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrszMessResult:          .int szMessResult
iAdrsValue:                .int sValue
/******************************************************************/
/*     factorielle function                         */
/******************************************************************/
/* r0 contains the Y combinator address  */
/* r1 contains the number structure  */
facFunc:
    push {r1-r3,lr}             @ save  registers
    mov r2,r0                   @ save Y combinator address
    ldr r0,[r1,#func_num]       @ load number
    cmp r0,#1                   @ > 1 ?
    bgt 1f                      @ yes
    mov r0,#1                   @ create structure number value 1
    bl numFunc
    b 100f
1:
    mov r3,r0                   @ save number
    sub r0,#1                   @ decrement number
    bl numFunc                  @ and create new structure number
    cmp r0,#-1                  @ allocation error ?
    beq 100f
    mov r1,r0                   @ new structure number -> param 1
    ldr r0,[r2,#func_f_]        @ load function address to execute
    bl callFunc                 @ call
    ldr r1,[r0,#func_num]       @ load new result
    mul r0,r1,r3                @ and multiply by precedent
    bl numFunc                  @ and create new structure number
                                @ and return her address in r0
100:
    pop {r1-r3,lr}              @ restaur registers
    bx lr                       @ return
/******************************************************************/
/*     fibonacci function                         */
/******************************************************************/
/* r0 contains the Y combinator address  */
/* r1 contains the number structure  */
fibFunc:
    push {r1-r4,lr}             @ save  registers
    mov r2,r0                   @ save Y combinator address
    ldr r0,[r1,#func_num]       @ load number
    cmp r0,#1                   @ > 1 ?
    bgt 1f                      @ yes
    mov r0,#1                   @ create structure number value 1
    bl numFunc
    b 100f
1:
    mov r3,r0                   @ save number
    sub r0,#1                   @ decrement number
    bl numFunc                  @ and create new structure number
    cmp r0,#-1                  @ allocation error ?
    beq 100f
    mov r1,r0                   @ new structure number -> param 1
    ldr r0,[r2,#func_f_]        @ load function address to execute
    bl callFunc                 @ call
    ldr r4,[r0,#func_num]       @ load new result
    sub r0,r3,#2                @ new number - 2
    bl numFunc                  @ and create new structure number
    cmp r0,#-1                  @ allocation error ?
    beq 100f
    mov r1,r0                   @ new structure number -> param 1
    ldr r0,[r2,#func_f_]        @ load function address to execute
    bl callFunc                 @ call
    ldr r1,[r0,#func_num]       @ load new result
    add r0,r1,r4                @ add two results
    bl numFunc                  @ and create new structure number
                                @ and return her address in r0
100:
    pop {r1-r4,lr}              @ restaur registers
    bx lr                       @ return
/******************************************************************/
/*     call function                         */
/******************************************************************/
/* r0 contains the address of the function  */
/* r1 contains the address of the function 1 */
callFunc:
    push {r2,lr}                                @ save  registers
    ldr r2,[r0,#func_fn]                        @ load function address to execute
    blx r2                                      @ and call it
    pop {r2,lr}                                 @ restaur registers
    bx lr                                       @ return
/******************************************************************/
/*     create Y combinator function                         */
/******************************************************************/
/* r0 contains the address of the function  */
YFunc:
    push {r1,lr}                                @ save  registers
    mov r1,#0
    bl newFunc
    cmp r0,#-1                                  @ allocation error ?
    strne r0,[r0,#func_f_]                      @ store function and return in r0
    pop {r1,lr}                                 @ restaur registers
    bx lr                                       @ return
/******************************************************************/
/*     create structure number function                         */
/******************************************************************/
/* r0 contains the number  */
numFunc:
    push {r1,r2,lr}                             @ save  registers
    mov r2,r0                                   @ save number
    mov r0,#0                                   @ function null
    mov r1,#0                                   @ function null
    bl newFunc
    cmp r0,#-1                                  @ allocation error ?
    strne r2,[r0,#func_num]                     @ store number in new structure
    pop {r1,r2,lr}                              @ restaur registers
    bx lr                                       @ return
/******************************************************************/
/*     new function                                               */
/******************************************************************/
/* r0 contains the function address   */
/* r1 contains the function address 1   */
newFunc:
    push {r2-r7,lr}                             @ save  registers
    mov r4,r0                                   @ save address
    mov r5,r1                                   @ save adresse 1
    @ allocation place on the heap
    mov r0,#0                                   @ allocation place heap
    mov r7,#0x2D                                @ call system 'brk'
    svc #0
    mov r3,r0                                   @ save address heap for output string
    add r0,#func_fin                            @ reservation place one element
    mov r7,#0x2D                                @ call system 'brk'
    svc #0
    cmp r0,#-1                                  @ allocation error
    beq 100f
    mov r0,r3
    str r4,[r0,#func_fn]                        @ store address
    str r5,[r0,#func_f_]
    mov r2,#0
    str r2,[r0,#func_num]                       @ store zero to number
100:
    pop {r2-r7,lr}                              @ restaur registers
    bx lr                                       @ return
/***************************************************/
/*      ROUTINES INCLUDE                 */
/***************************************************/
.include "../affichage.inc"


```

{{output}}

```txt

Program start
Function factorielle :
Result value :1
Result value :2
Result value :6
Result value :24
Result value :120
Result value :720
Result value :5040
Result value :40320
Result value :362880
Result value :3628800
Function Fibonacci :
Result value :1
Result value :2
Result value :3
Result value :5
Result value :8
Result value :13
Result value :21
Result value :34
Result value :55
Result value :89
Program normal end.

```


## ATS


```ATS

(* ****** ****** *)
//
#include "share/atspre_staload.hats"
//
(* ****** ****** *)
//
fun
myfix
{a:type}
(
 f: lazy(a) -<cloref1> a
) : lazy(a) = $delay(f(myfix(f)))
//
val
fact =
myfix{int-<cloref1>int}
(
lam(ff) => lam(x) => if x > 0 then x * !ff(x-1) else 1
)
(* ****** ****** *)
//
implement main0 () = println! ("fact(10) = ", !fact(10))
//
(* ****** ****** *)

```



## BlitzMax

BlitzMax doesn't support anonymous functions or classes, so everything needs to be explicitly named.

```blitzmax
SuperStrict

'Boxed type so we can just use object arrays for argument lists
Type Integer
	Field val:Int
	Function Make:Integer(_val:Int)
		Local i:Integer = New Integer
		i.val = _val
		Return i
	End Function
End Type


'Higher-order function type - just a procedure attached to a scope
Type Func Abstract
	Method apply:Object(args:Object[]) Abstract
End Type

'Function definitions - extend with fields as locals and implement apply as body
Type Scope Extends Func Abstract
	Field env:Scope

	'Constructor - bind an environment to a procedure
	Function lambda:Scope(env:Scope) Abstract

	Method _init:Scope(_env:Scope)	'Helper to keep constructors small
		env = _env ; Return Self
	End Method
End Type


'Based on the following definition:
'(define (Y f)
'    (let ((_r (lambda (r) (f (lambda a (apply (r r) a))))))
'      (_r _r)))

'Y (outer)
Type Y Extends Scope
	Field f:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)	'Necessary due to highly limited constructor syntax
		Return (New Y)._init(env)
	End Function

	Method apply:Func(args:Object[])
		f = Func(args[0])
		Local _r:Func = YInner1.lambda(Self)
		Return Func(_r.apply([_r]))
	End Method
End Type

'First lambda within Y
Type YInner1 Extends Scope
	Field r:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)
		Return (New YInner1)._init(env)
	End Function

	Method apply:Func(args:Object[])
		r = Func(args[0])
		Return Func(Y(env).f.apply([YInner2.lambda(Self)]))
	End Method
End Type

'Second lambda within Y
Type YInner2 Extends Scope
	Field a:Object[]	'Parameter - not really needed, but good for clarity

	Function lambda:Scope(env:Scope)
		Return (New YInner2)._init(env)
	End Function

	Method apply:Object(args:Object[])
		a = args
		Local r:Func = YInner1(env).r
		Return Func(r.apply([r])).apply(a)
	End Method
End Type


'Based on the following definition:
'(define fac (Y (lambda (f)
'                 (lambda (x)
'                   (if (<= x 0) 1 (* x (f (- x 1)))))))

Type FacL1 Extends Scope
	Field f:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)
		Return (New FacL1)._init(env)
	End Function

	Method apply:Object(args:Object[])
		f = Func(args[0])
		Return FacL2.lambda(Self)
	End Method
End Type

Type FacL2 Extends Scope
	Function lambda:Scope(env:Scope)
		Return (New FacL2)._init(env)
	End Function

	Method apply:Object(args:Object[])
		Local x:Int = Integer(args[0]).val
		If x <= 0 Then Return Integer.Make(1) ; Else Return Integer.Make(x * Integer(FacL1(env).f.apply([Integer.Make(x - 1)])).val)
	End Method
End Type


'Based on the following definition:
'(define fib (Y (lambda (f)
'                 (lambda (x)
'                   (if (< x 2) x (+ (f (- x 1)) (f (- x 2)))))))

Type FibL1 Extends Scope
	Field f:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)
		Return (New FibL1)._init(env)
	End Function

	Method apply:Object(args:Object[])
		f = Func(args[0])
		Return FibL2.lambda(Self)
	End Method
End Type

Type FibL2 Extends Scope
	Function lambda:Scope(env:Scope)
		Return (New FibL2)._init(env)
	End Function

	Method apply:Object(args:Object[])
		Local x:Int = Integer(args[0]).val
		If x < 2
			Return Integer.Make(x)
		Else
			Local f:Func = FibL1(env).f
			Local x1:Int = Integer(f.apply([Integer.Make(x - 1)])).val
			Local x2:Int = Integer(f.apply([Integer.Make(x - 2)])).val
			Return Integer.Make(x1 + x2)
		EndIf
	End Method
End Type


'Now test
Local _Y:Func = Y.lambda(Null)

Local fac:Func = Func(_Y.apply([FacL1.lambda(Null)]))
Print Integer(fac.apply([Integer.Make(10)])).val

Local fib:Func = Func(_Y.apply([FibL1.lambda(Null)]))
Print Integer(fib.apply([Integer.Make(10)])).val
```



## Bracmat

The lambda abstraction

```txt
 (λx.x)y
```

translates to

```txt
 /('(x.$x))$y
```

in Bracmat code. Likewise, the fixed point combinator

```txt
Y := λg.(λx.g (x x)) (λx.g (x x))
```

the factorial

```txt
G := λr. λn.(1, if n = 0; else n × (r (n−1)))
```

the Fibonacci function

```txt
H := λr. λn.(1, if n = 1 or n = 2; else (r (n−1)) + (r (n−2)))
```

and the calls

```txt
(Y G) i
```

and

```txt
(Y H) i
```

where i varies between 1 and 10, are translated into Bracmat as shown below

```bracmat
(   ( Y
    = /(
       ' ( g
         .   /('(x.$g'($x'$x)))
           $ /('(x.$g'($x'$x)))
         )
       )
    )
  & ( G
    = /(
       ' ( r
         . /(
            ' ( n
              .   $n:~>0&1
                | $n*($r)$($n+-1)
              )
            )
         )
       )
    )
  & ( H
    = /(
       ' ( r
         . /(
            ' ( n
              .   $n:(1|2)&1
                | ($r)$($n+-1)+($r)$($n+-2)
              )
            )
         )
       )
    )
  & 0:?i
  &   whl
    ' ( 1+!i:~>10:?i
      & out$(str$(!i "!=" (!Y$!G)$!i))
      )
  & 0:?i
  &   whl
    ' ( 1+!i:~>10:?i
      & out$(str$("fib(" !i ")=" (!Y$!H)$!i))
      )
  &
)
```

{{out}}

```txt
1!=1
2!=2
3!=6
4!=24
5!=120
6!=720
7!=5040
8!=40320
9!=362880
10!=3628800
fib(1)=1
fib(2)=1
fib(3)=2
fib(4)=3
fib(5)=5
fib(6)=8
fib(7)=13
fib(8)=21
fib(9)=34
fib(10)=55
```



## C

C doesn't have first class functions, so we demote everything to second class to match.
```c
#include <stdio.h>
#include <stdlib.h>

/* func: our one and only data type; it holds either a pointer to
   a function call, or an integer.  Also carry a func pointer to
   a potential parameter, to simulate closure                   */
typedef struct func_t *func;
typedef struct func_t {
        func (*fn) (func, func);
        func _;
        int num;
} func_t;

func new(func(*f)(func, func), func _) {
        func x = malloc(sizeof(func_t));
        x->fn = f;
        x->_ = _;       /* closure, sort of */
        x->num = 0;
        return x;
}

func call(func f, func n) {
        return f->fn(f, n);
}

func Y(func(*f)(func, func)) {
        func g = new(f, 0);
        g->_ = g;
        return g;
}

func num(int n) {
        func x = new(0, 0);
        x->num = n;
        return x;
}


func fac(func self, func n) {
        int nn = n->num;
        return nn > 1   ? num(nn * call(self->_, num(nn - 1))->num)
                        : num(1);
}

func fib(func self, func n) {
        int nn = n->num;
        return nn > 1
                ? num(  call(self->_, num(nn - 1))->num +
                        call(self->_, num(nn - 2))->num )
                : num(1);
}

void show(func n) { printf(" %d", n->num); }

int main() {
        int i;
        func f = Y(fac);
        printf("fac: ");
        for (i = 1; i < 10; i++)
                show( call(f, num(i)) );
        printf("\n");

        f = Y(fib);
        printf("fib: ");
        for (i = 1; i < 10; i++)
                show( call(f, num(i)) );
        printf("\n");

        return 0;
}

```


{{out}}

```txt
fac:  1 2 6 24 120 720 5040 40320 362880
fib:  1 2 3 5 8 13 21 34 55
```



## C#


Like many other statically typed languages, this involves a recursive type, and like other strict languages, it is the Z-combinator instead.

The combinator here is expressed entirely as a lambda expression and is a static property of the generic <code>YCombinator</code> class. Both it and the <code>RecursiveFunc</code> type thus "inherit" the type parameters of the containing class—there effectively exists a separate specialized copy of both for each generic instantiation of <code>YCombinator</code>.

''Note: in the code, <code>Func<T, TResult></code> is a delegate type (the CLR equivalent of a function pointer) that has a parameter of type <code>T</code> and return type of <code>TResult</code>. See [[Higher-order functions#C#]] or [https://docs.microsoft.com/en-us/dotnet/standard/delegates-lambdas the documentation] for more information.''


```c#
using System;

static class YCombinator<T, TResult>
{
    // RecursiveFunc is not needed to call Fix() and so can be private.
    private delegate Func<T, TResult> RecursiveFunc(RecursiveFunc r);

    public static Func<Func<Func<T, TResult>, Func<T, TResult>>, Func<T, TResult>> Fix { get; } =
        f => ((RecursiveFunc)(g => f(x => g(g)(x))))(g => f(x => g(g)(x)));
}

static class Program
{
    static void Main()
    {
        var fac = YCombinator<int, int>.Fix(f => x => x < 2 ? 1 : x * f(x - 1));
        var fib = YCombinator<int, int>.Fix(f => x => x < 2 ? x : f(x - 1) + f(x - 2));

        Console.WriteLine(fac(10));
        Console.WriteLine(fib(10));
    }
}

```

{{out}}

```txt
3628800
55
```


Alternatively, with a non-generic holder class (note that <code>Fix</code> is now a method, as properties cannot be generic):

```c#
static class YCombinator
{
    private delegate Func<T, TResult> RecursiveFunc<T, TResult>(RecursiveFunc<T, TResult> r);

    public static Func<T, TResult> Fix<T, TResult>(Func<Func<T, TResult>, Func<T, TResult>> f)
        => ((RecursiveFunc<T, TResult>)(g => f(x => g(g)(x))))(g => f(x => g(g)(x)));
}
```


Using the late-binding offered by <code>dynamic</code> to eliminate the recursive type:

```c#
static class YCombinator<T, TResult>
{
    public static Func<Func<Func<T, TResult>, Func<T, TResult>>, Func<T, TResult>> Fix { get; } =
        f => ((Func<dynamic, Func<T, TResult>>)(g => f(x => g(g)(x))))((Func<dynamic, Func<T, TResult>>)(g => f(x => g(g)(x))));
}
```


The usual version using recursion, disallowed by the task (implemented as a generic method):

```c#
static class YCombinator
{
    static Func<T, TResult> Fix<T, TResult>(Func<Func<T, TResult>, Func<T, TResult>> f) => x => f(Fix(f))(x);
}
```



### Translations

To compare differences in language and runtime instead of in approaches to the task, the following are translations of solutions from other languages. Two versions of each translation are provided, one seeking to resemble the original as closely as possible, and another that is identical in program control flow but syntactically closer to idiomatic C#.

====[http://rosettacode.org/mw/index.php?oldid=287744#C++ C++]====
<code>std::function<TResult(T)></code> in C++ corresponds to <code>Func<T, TResult></code> in C#.

'''Verbatim'''

```c#
using Func = System.Func<int, int>;
using FuncFunc = System.Func<System.Func<int, int>, System.Func<int, int>>;

static class Program {
    struct RecursiveFunc<F> {
        public System.Func<RecursiveFunc<F>, F> o;
    }

    static System.Func<A, B> Y<A, B>(System.Func<System.Func<A, B>, System.Func<A, B>> f) {
        var r = new RecursiveFunc<System.Func<A, B>>() {
            o = new System.Func<RecursiveFunc<System.Func<A, B>>, System.Func<A, B>>((RecursiveFunc<System.Func<A, B>> w) => {
                return f(new System.Func<A, B>((A x) => {
                    return w.o(w)(x);
                }));
            })
        };
        return r.o(r);
    }

    static FuncFunc almost_fac = (Func f) => {
        return new Func((int n) => {
            if (n <= 1) return 1;
            return n * f(n - 1);
        });
    };

    static FuncFunc almost_fib = (Func f) => {
        return new Func((int n) => {
            if (n <= 2) return 1;
            return f(n - 1) + f(n - 2);
        });
    };

    static int Main() {
        var fib = Y(almost_fib);
        var fac = Y(almost_fac);
        System.Console.WriteLine("fib(10) = " + fib(10));
        System.Console.WriteLine("fac(10) = " + fac(10));
        return 0;
    }
}
```


'''Semi-idiomatic'''

```c#
using System;

using FuncFunc = System.Func<System.Func<int, int>, System.Func<int, int>>;

static class Program {
    struct RecursiveFunc<F> {
        public Func<RecursiveFunc<F>, F> o;
    }

    static Func<A, B> Y<A, B>(Func<Func<A, B>, Func<A, B>> f) {
        var r = new RecursiveFunc<Func<A, B>> {
            o = w => f(x => w.o(w)(x))
        };
        return r.o(r);
    }

    static FuncFunc almost_fac = f => n => n <= 1 ? 1 : n * f(n - 1);

    static FuncFunc almost_fib = f => n => n <= 2 ? 1 : f(n - 1) + f(n - 2);

    static void Main() {
        var fib = Y(almost_fib);
        var fac = Y(almost_fac);
        Console.WriteLine("fib(10) = " + fib(10));
        Console.WriteLine("fac(10) = " + fac(10));
    }
}
```


====[http://rosettacode.org/mw/index.php?oldid=287744#Ceylon Ceylon]====
<code>TResult(T)</code> in Ceylon corresponds to <code>Func<T, TResult></code> in C#.

Since C# does not have local classes, <code>RecursiveFunc</code> and <code>y1</code> are declared in a class of their own. Moving the type parameters to the class also prevents type parameter inference.

'''Verbatim'''

```c#
using System;
using System.Diagnostics;

class Program {
    public delegate TResult ParamsFunc<T, TResult>(params T[] args);

    static class Y<Result, Args> {
        class RecursiveFunction {
            public Func<RecursiveFunction, ParamsFunc<Args, Result>> o;
            public RecursiveFunction(Func<RecursiveFunction, ParamsFunc<Args, Result>> o) => this.o = o;
        }

        public static ParamsFunc<Args, Result> y1(
                Func<ParamsFunc<Args, Result>, ParamsFunc<Args, Result>> f) {

            var r = new RecursiveFunction((RecursiveFunction w)
                => f((Args[] args) => w.o(w)(args)));

            return r.o(r);
        }
    }

    static ParamsFunc<Args, Result> y2<Args, Result>(
            Func<ParamsFunc<Args, Result>, ParamsFunc<Args, Result>> f) {

        Func<dynamic, ParamsFunc<Args, Result>> r = w => {
            Debug.Assert(w is Func<dynamic, ParamsFunc<Args, Result>>);
            return f((Args[] args) => w(w)(args));
        };

        return r(r);
    }

    static ParamsFunc<Args, Result> y3<Args, Result>(
            Func<ParamsFunc<Args, Result>, ParamsFunc<Args, Result>> f)
        => (Args[] args) => f(y3(f))(args);

    static void Main() {
        var factorialY1 = Y<int, int>.y1((ParamsFunc<int, int> fact) => (int[] x)
            => (x[0] > 1) ? x[0] * fact(x[0] - 1) : 1);

        var fibY1 = Y<int, int>.y1((ParamsFunc<int, int> fib) => (int[] x)
            => (x[0] > 2) ? fib(x[0] - 1) + fib(x[0] - 2) : 2);

        Console.WriteLine(factorialY1(10)); // 362880
        Console.WriteLine(fibY1(10));       // 110
    }
}
```


'''Semi-idiomatic'''

```c#
using System;
using System.Diagnostics;

static class Program {
    delegate TResult ParamsFunc<T, TResult>(params T[] args);

    static class Y<Result, Args> {
        class RecursiveFunction {
            public Func<RecursiveFunction, ParamsFunc<Args, Result>> o;
            public RecursiveFunction(Func<RecursiveFunction, ParamsFunc<Args, Result>> o) => this.o = o;
        }

        public static ParamsFunc<Args, Result> y1(
                Func<ParamsFunc<Args, Result>, ParamsFunc<Args, Result>> f) {

            var r = new RecursiveFunction(w => f(args => w.o(w)(args)));

            return r.o(r);
        }
    }

    static ParamsFunc<Args, Result> y2<Args, Result>(
            Func<ParamsFunc<Args, Result>, ParamsFunc<Args, Result>> f) {

        Func<dynamic, ParamsFunc<Args, Result>> r = w => {
            Debug.Assert(w is Func<dynamic, ParamsFunc<Args, Result>>);
            return f(args => w(w)(args));
        };

        return r(r);
    }

    static ParamsFunc<Args, Result> y3<Args, Result>(
            Func<ParamsFunc<Args, Result>, ParamsFunc<Args, Result>> f)
        => args => f(y3(f))(args);

    static void Main() {
        var factorialY1 = Y<int, int>.y1(fact => x => (x[0] > 1) ? x[0] * fact(x[0] - 1) : 1);
        var fibY1 = Y<int, int>.y1(fib => x => (x[0] > 2) ? fib(x[0] - 1) + fib(x[0] - 2) : 2);

        Console.WriteLine(factorialY1(10));
        Console.WriteLine(fibY1(10));
    }
}
```


====[http://rosettacode.org/mw/index.php?oldid=287744#Go Go]====
<code>func(T) TResult</code> in Go corresponds to <code>Func<T, TResult></code> in C#.

'''Verbatim'''

```c#
using System;

// Func and FuncFunc can be defined using using aliases and the System.Func<T, TReult> type, but RecursiveFunc must be a delegate type of its own.
using Func = System.Func<int, int>;
using FuncFunc = System.Func<System.Func<int, int>, System.Func<int, int>>;

delegate Func RecursiveFunc(RecursiveFunc f);

static class Program {
    static void Main() {
        var fac = Y(almost_fac);
        var fib = Y(almost_fib);
        Console.WriteLine("fac(10) = " + fac(10));
        Console.WriteLine("fib(10) = " + fib(10));
    }

    static Func Y(FuncFunc f) {
        RecursiveFunc g = delegate (RecursiveFunc r) {
            return f(delegate (int x) {
                return r(r)(x);
            });
        };
        return g(g);
    }

    static Func almost_fac(Func f) {
        return delegate (int x) {
            if (x <= 1) {
                return 1;
            }
            return x * f(x-1);
        };
    }

    static Func almost_fib(Func f) {
        return delegate (int x) {
            if (x <= 2) {
                return 1;
            }
            return f(x-1)+f(x-2);
        };
    }
}
```


Recursive:

```c#
    static Func Y(FuncFunc f) {
        return delegate (int x) {
            return f(Y(f))(x);
        };
    }
```


'''Semi-idiomatic'''

```c#
using System;

delegate int Func(int i);
delegate Func FuncFunc(Func f);
delegate Func RecursiveFunc(RecursiveFunc f);

static class Program {
    static void Main() {
        var fac = Y(almost_fac);
        var fib = Y(almost_fib);
        Console.WriteLine("fac(10) = " + fac(10));
        Console.WriteLine("fib(10) = " + fib(10));
    }

    static Func Y(FuncFunc f) {
        RecursiveFunc g = r => f(x => r(r)(x));
        return g(g);
    }

    static Func almost_fac(Func f) => x => x <= 1 ? 1 : x * f(x - 1);

    static Func almost_fib(Func f) => x => x <= 2 ? 1 : f(x - 1) + f(x - 2);
}
```


Recursive:

```c#
    static Func Y(FuncFunc f) => x => f(Y(f))(x);
```


====[http://rosettacode.org/mw/index.php?oldid=287744#Java Java]====

'''Verbatim'''

Since Java uses interfaces and C# uses delegates, which are the only type that the C# compiler will coerce lambda expressions to, this code declares a <code>Functions</code> class for providing a means of converting CLR delegates to objects that implement the <code>Function</code> and <code>RecursiveFunction</code> interfaces.

```c#
using System;

static class Program {
    interface Function<T, R> {
        R apply(T t);
    }

    interface RecursiveFunction<F> : Function<RecursiveFunction<F>, F> {
    }

    static class Functions {
        class Function<T, R> : Program.Function<T, R> {
            readonly Func<T, R> _inner;

            public Function(Func<T, R> inner) => this._inner = inner;

            public R apply(T t) => this._inner(t);
        }

        class RecursiveFunction<F> : Function<Program.RecursiveFunction<F>, F>, Program.RecursiveFunction<F> {
            public RecursiveFunction(Func<Program.RecursiveFunction<F>, F> inner) : base(inner) {
            }
        }

        public static Program.Function<T, R> Create<T, R>(Func<T, R> inner) => new Function<T, R>(inner);
        public static Program.RecursiveFunction<F> Create<F>(Func<Program.RecursiveFunction<F>, F> inner) => new RecursiveFunction<F>(inner);
    }

    static Function<A, B> Y<A, B>(Function<Function<A, B>, Function<A, B>> f) {
        var r = Functions.Create<Function<A, B>>(w => f.apply(Functions.Create<A, B>(x => w.apply(w).apply(x))));
        return r.apply(r);
    }

    static void Main(params String[] arguments) {
        Function<int, int> fib = Y(Functions.Create<Function<int, int>, Function<int, int>>(f => Functions.Create<int, int>(n =>
            (n <= 2)
              ? 1
              : (f.apply(n - 1) + f.apply(n - 2))))
        );
        Function<int, int> fac = Y(Functions.Create<Function<int, int>, Function<int, int>>(f => Functions.Create<int, int>(n =>
            (n <= 1)
              ? 1
              : (n * f.apply(n - 1))))
        );

        Console.WriteLine("fib(10) = " + fib.apply(10));
        Console.WriteLine("fac(10) = " + fac.apply(10));
    }
}
```


'''"Idiomatic"'''

For demonstrative purposes, to completely avoid using CLR delegates, lambda expressions can be replaced with explicit types that implement the functional interfaces. Closures are thus implemented by replacing all usages of the original local variable with a field of the type that represents the lambda expression; this process, called "hoisting" is actually how variable capturing is implemented by the C# compiler (for more information, see [https://blogs.msdn.microsoft.com/abhinaba/2005/10/18/c-anonymous-methods-are-not-closures/ this Microsoft blog post].

```c#
using System;

static class YCombinator {
    interface Function<T, R> {
        R apply(T t);
    }

    interface RecursiveFunction<F> : Function<RecursiveFunction<F>, F> {
    }

    static class Y<A, B> {
        class __1 : RecursiveFunction<Function<A, B>> {
            class __2 : Function<A, B> {
                readonly RecursiveFunction<Function<A, B>> w;

                public __2(RecursiveFunction<Function<A, B>> w) {
                    this.w = w;
                }

                public B apply(A x) {
                    return w.apply(w).apply(x);
                }
            }

            Function<Function<A, B>, Function<A, B>> f;

            public __1(Function<Function<A, B>, Function<A, B>> f) {
                this.f = f;
            }

            public Function<A, B> apply(RecursiveFunction<Function<A, B>> w) {
                return f.apply(new __2(w));
            }
        }

        public static Function<A, B> _(Function<Function<A, B>, Function<A, B>> f) {
            var r = new __1(f);
            return r.apply(r);
        }
    }

    class __1 : Function<Function<int, int>, Function<int, int>> {
        class __2 : Function<int, int> {
            readonly Function<int, int> f;

            public __2(Function<int, int> f) {
                this.f = f;
            }

            public int apply(int n) {
                return
                    (n <= 2)
                  ? 1
                  : (f.apply(n - 1) + f.apply(n - 2));
            }
        }

        public Function<int, int> apply(Function<int, int> f) {
            return new __2(f);
        }
    }

    class __2 : Function<Function<int, int>, Function<int, int>> {
        class __3 : Function<int, int> {
            readonly Function<int, int> f;

            public __3(Function<int, int> f) {
                this.f = f;
            }

            public int apply(int n) {
                return
                    (n <= 1)
                  ? 1
                  : (n * f.apply(n - 1));
            }
        }

        public Function<int, int> apply(Function<int, int> f) {
            return new __3(f);
        }
    }

    static void Main(params String[] arguments) {
        Function<int, int> fib = Y<int, int>._(new __1());
        Function<int, int> fac = Y<int, int>._(new __2());

        Console.WriteLine("fib(10) = " + fib.apply(10));
        Console.WriteLine("fac(10) = " + fac.apply(10));
    }
}
```


'''C# 1.0'''

To conclude this chain of decreasing reliance on language features, here is above code translated to C# 1.0. The largest change is the replacement of the generic interfaces with the results of manually substituting their type parameters.

```c#
using System;

class Program {
    interface Func {
        int apply(int i);
    }

    interface FuncFunc {
        Func apply(Func f);
    }

    interface RecursiveFunc {
        Func apply(RecursiveFunc f);
    }

    class Y {
        class __1 : RecursiveFunc {
            class __2 : Func {
                readonly RecursiveFunc w;

                public __2(RecursiveFunc w) {
                    this.w = w;
                }

                public int apply(int x) {
                    return w.apply(w).apply(x);
                }
            }

            readonly FuncFunc f;

            public __1(FuncFunc f) {
                this.f = f;
            }

            public Func apply(RecursiveFunc w) {
                return f.apply(new __2(w));
            }
        }

        public static Func _(FuncFunc f) {
            __1 r = new __1(f);
            return r.apply(r);
        }
    }

    class __fib : FuncFunc {
        class __1 : Func {
            readonly Func f;

            public __1(Func f) {
                this.f = f;
            }

            public int apply(int n) {
                return
                    (n <= 2)
                  ? 1
                  : (f.apply(n - 1) + f.apply(n - 2));
            }

        }

        public Func apply(Func f) {
            return new __1(f);
        }
    }

    class __fac : FuncFunc {
        class __1 : Func {
            readonly Func f;

            public __1(Func f) {
                this.f = f;
            }

            public int apply(int n) {
                return
                    (n <= 1)
                  ? 1
                  : (n * f.apply(n - 1));
            }
        }

        public Func apply(Func f) {
            return new __1(f);
        }
    }

    static void Main(params String[] arguments) {
        Func fib = Y._(new __fib());
        Func fac = Y._(new __fac());

        Console.WriteLine("fib(10) = " + fib.apply(10));
        Console.WriteLine("fac(10) = " + fac.apply(10));
    }
}
```


'''Modified/varargs (the last implementation in the Java section)'''

Since C# delegates cannot declare members, extension methods are used to simulate doing so.


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

static class Func {
    public static Func<T, TResult2> andThen<T, TResult, TResult2>(
            this Func<T, TResult> @this,
            Func<TResult, TResult2> after)
        => _ => after(@this(_));
}

delegate OUTPUT SelfApplicable<OUTPUT>(SelfApplicable<OUTPUT> s);
static class SelfApplicable {
    public static OUTPUT selfApply<OUTPUT>(this SelfApplicable<OUTPUT> @this) => @this(@this);
}

delegate FUNCTION FixedPoint<FUNCTION>(Func<FUNCTION, FUNCTION> f);

delegate OUTPUT VarargsFunction<INPUTS, OUTPUT>(params INPUTS[] inputs);
static class VarargsFunction {
    public static VarargsFunction<INPUTS, OUTPUT> from<INPUTS, OUTPUT>(
            Func<INPUTS[], OUTPUT> function)
        => function.Invoke;

    public static VarargsFunction<INPUTS, OUTPUT> upgrade<INPUTS, OUTPUT>(
            Func<INPUTS, OUTPUT> function) {
        return inputs => function(inputs[0]);
    }

    public static VarargsFunction<INPUTS, OUTPUT> upgrade<INPUTS, OUTPUT>(
            Func<INPUTS, INPUTS, OUTPUT> function) {
        return inputs => function(inputs[0], inputs[1]);
    }

    public static VarargsFunction<INPUTS, POST_OUTPUT> andThen<INPUTS, OUTPUT, POST_OUTPUT>(
            this VarargsFunction<INPUTS, OUTPUT> @this,
            VarargsFunction<OUTPUT, POST_OUTPUT> after) {
        return inputs => after(@this(inputs));
    }

    public static Func<INPUTS, OUTPUT> toFunction<INPUTS, OUTPUT>(
            this VarargsFunction<INPUTS, OUTPUT> @this) {
        return input => @this(input);
    }

    public static Func<INPUTS, INPUTS, OUTPUT> toBiFunction<INPUTS, OUTPUT>(
            this VarargsFunction<INPUTS, OUTPUT> @this) {
        return (input, input2) => @this(input, input2);
    }

    public static VarargsFunction<PRE_INPUTS, OUTPUT> transformArguments<PRE_INPUTS, INPUTS, OUTPUT>(
            this VarargsFunction<INPUTS, OUTPUT> @this,
            Func<PRE_INPUTS, INPUTS> transformer) {
        return inputs => @this(inputs.AsParallel().AsOrdered().Select(transformer).ToArray());
    }
}

delegate FixedPoint<FUNCTION> Y<FUNCTION>(SelfApplicable<FixedPoint<FUNCTION>> y);

static class Program {
    static TResult Cast<TResult>(this Delegate @this) where TResult : Delegate {
        return (TResult)Delegate.CreateDelegate(typeof(TResult), @this.Target, @this.Method);
    }

    static void Main(params String[] arguments) {
        BigInteger TWO = BigInteger.One + BigInteger.One;

        Func<IFormattable, long> toLong = x => long.Parse(x.ToString());
        Func<IFormattable, BigInteger> toBigInteger = x => new BigInteger(toLong(x));

        /* Based on https://gist.github.com/aruld/3965968/#comment-604392 */
        Y<VarargsFunction<IFormattable, IFormattable>> combinator = y => f => x => f(y.selfApply()(f))(x);
        FixedPoint<VarargsFunction<IFormattable, IFormattable>> fixedPoint =
            combinator.Cast<SelfApplicable<FixedPoint<VarargsFunction<IFormattable, IFormattable>>>>().selfApply();

        VarargsFunction<IFormattable, IFormattable> fibonacci = fixedPoint(
            f => VarargsFunction.upgrade(
                toBigInteger.andThen(
                    n => (IFormattable)(
                        (n.CompareTo(TWO) <= 0)
                        ? 1
                        : BigInteger.Parse(f(n - BigInteger.One).ToString())
                            + BigInteger.Parse(f(n - TWO).ToString()))
                )
            )
        );

        VarargsFunction<IFormattable, IFormattable> factorial = fixedPoint(
            f => VarargsFunction.upgrade(
                toBigInteger.andThen(
                    n => (IFormattable)((n.CompareTo(BigInteger.One) <= 0)
                        ? 1
                        : n * BigInteger.Parse(f(n - BigInteger.One).ToString()))
                )
            )
        );

        VarargsFunction<IFormattable, IFormattable> ackermann = fixedPoint(
            f => VarargsFunction.upgrade(
                (BigInteger m, BigInteger n) => m.Equals(BigInteger.Zero)
                    ? n + BigInteger.One
                    : f(
                        m - BigInteger.One,
                        n.Equals(BigInteger.Zero)
                            ? BigInteger.One
                            : f(m, n - BigInteger.One)
                    )
            ).transformArguments(toBigInteger)
        );

        var functions = new Dictionary<String, VarargsFunction<IFormattable, IFormattable>>();
        functions.Add("fibonacci", fibonacci);
        functions.Add("factorial", factorial);
        functions.Add("ackermann", ackermann);

        var parameters = new Dictionary<VarargsFunction<IFormattable, IFormattable>, IFormattable[]>();
        parameters.Add(functions["fibonacci"], new IFormattable[] { 20 });
        parameters.Add(functions["factorial"], new IFormattable[] { 10 });
        parameters.Add(functions["ackermann"], new IFormattable[] { 3, 2 });

        functions.AsParallel().Select(
            entry => entry.Key
                + "[" + String.Join(", ", parameters[entry.Value].Select(x => x.ToString())) + "]"
                + " = "
                + entry.Value(parameters[entry.Value])
        ).ForAll(Console.WriteLine);
    }
}
```


====[http://rosettacode.org/mw/index.php?oldid=287744#Swift Swift]====
<code>T -> TResult</code> in Swift corresponds to <code>Func<T, TResult></code> in C#.

'''Verbatim'''

The more idiomatic version doesn't look much different.

```c#
using System;

static class Program {
    struct RecursiveFunc<F> {
        public Func<RecursiveFunc<F>, F> o;
    }

    static Func<A, B> Y<A, B>(Func<Func<A, B>, Func<A, B>> f) {
        var r = new RecursiveFunc<Func<A, B>> { o = w => f(_0 => w.o(w)(_0)) };
        return r.o(r);
    }

    static void Main() {
        // C# can't infer the type arguments to Y either; either it or f must be explicitly typed.
        var fac = Y((Func<int, int> f) => _0 => _0 <= 1 ? 1 : _0 * f(_0 - 1));
        var fib = Y((Func<int, int> f) => _0 => _0 <= 2 ? 1 : f(_0 - 1) + f(_0 - 2));

        Console.WriteLine($"fac(5) = {fac(5)}");
        Console.WriteLine($"fib(9) = {fib(9)}");
    }
}
```


Without recursive type:

```c#
    public static Func<A, B> Y<A, B>(Func<Func<A, B>, Func<A, B>> f) {
        Func<dynamic, Func<A, B>> r = z => { var w = (Func<dynamic, Func<A, B>>)z; return f(_0 => w(w)(_0)); };
        return r(r);
    }
```


Recursive:

```c#
    public static Func<In, Out> Y<In, Out>(Func<Func<In, Out>, Func<In, Out>> f) {
        return x => f(Y(f))(x);
    }
```



## C++

{{works with|C++11}}
Known to work with GCC 4.7.2. Compile with
 g++ --std=c++11 ycomb.cc

```cpp
#include <iostream>
#include <functional>

template <typename F>
struct RecursiveFunc {
	std::function<F(RecursiveFunc)> o;
};

template <typename A, typename B>
std::function<B(A)> Y (std::function<std::function<B(A)>(std::function<B(A)>)> f) {
	RecursiveFunc<std::function<B(A)>> r = {
		std::function<std::function<B(A)>(RecursiveFunc<std::function<B(A)>>)>([f](RecursiveFunc<std::function<B(A)>> w) {
			return f(std::function<B(A)>([w](A x) {
				return w.o(w)(x);
			}));
		})
	};
	return r.o(r);
}

typedef std::function<int(int)> Func;
typedef std::function<Func(Func)> FuncFunc;
FuncFunc almost_fac = [](Func f) {
	return Func([f](int n) {
		if (n <= 1) return 1;
		return n * f(n - 1);
	});
};

FuncFunc almost_fib = [](Func f) {
	return Func([f](int n) {
	 	if (n <= 2) return 1;
		return  f(n - 1) + f(n - 2);
	});
};

int main() {
	auto fib = Y(almost_fib);
	auto fac = Y(almost_fac);
	std::cout << "fib(10) = " << fib(10) << std::endl;
	std::cout << "fac(10) = " << fac(10) << std::endl;
	return 0;
}
```

{{out}}


```txt

fib(10) = 55
fac(10) = 3628800

```


{{works with|C++14}}
A shorter version, taking advantage of generic lambdas.  Known to work with GCC 5.2.0, but likely some earlier versions as well. Compile with
 g++ --std=c++14 ycomb.cc

```cpp
#include <iostream>
#include <functional>
int main () {
  auto y = ([] (auto f) { return
              ([] (auto x) { return x (x); }
                 ([=] (auto y) -> std:: function <int (int)> { return
                    f ([=] (auto a) { return
                          (y (y)) (a) ;});}));});

  auto almost_fib = [] (auto f) { return
                       [=] (auto n) { return
                         n < 2? n: f (n - 1) + f (n - 2) ;};};
  auto almost_fac = [] (auto f) { return
                       [=] (auto n) { return
                         n <= 1? n: n * f (n - 1); };};

  auto fib = y (almost_fib);
  auto fac = y (almost_fac);
  std:: cout << fib (10) << '\n'
             << fac (10) << '\n';
}
```

{{out}}


```txt

fib(10) = 55
fac(10) = 3628800

```


The usual version using recursion, disallowed by the task:

```cpp
template <typename A, typename B>
std::function<B(A)> Y (std::function<std::function<B(A)>(std::function<B(A)>)> f) {
	return [f](A x) {
		return f(Y(f))(x);
	};
}
```


Another version which is disallowed because a function passes itself, which is also a kind of recursion:

```cpp
template <typename A, typename B>
struct YFunctor {
  const std::function<std::function<B(A)>(std::function<B(A)>)> f;
  YFunctor(std::function<std::function<B(A)>(std::function<B(A)>)> _f) : f(_f) {}
  B operator()(A x) const {
    return f(*this)(x);
  }
};

template <typename A, typename B>
std::function<B(A)> Y (std::function<std::function<B(A)>(std::function<B(A)>)> f) {
  return YFunctor<A,B>(f);
}
```



## Ceylon

Using a class for the recursive type:

```ceylon
Result(*Args) y1<Result,Args>(
        Result(*Args)(Result(*Args)) f)
        given Args satisfies Anything[] {

    class RecursiveFunction(o) {
        shared Result(*Args)(RecursiveFunction) o;
    }

    value r = RecursiveFunction((RecursiveFunction w)
        =>  f(flatten((Args args) => w.o(w)(*args))));

    return r.o(r);
}

value factorialY1 = y1((Integer(Integer) fact)(Integer x)
    =>  if (x > 1) then x * fact(x - 1) else 1);

value fibY1 = y1((Integer(Integer) fib)(Integer x)
    =>  if (x > 2) then fib(x - 1) + fib(x - 2) else 2);

print(factorialY1(10)); // 3628800
print(fibY1(10));       // 110
```


Using Anything to erase the function type:

```ceylon
Result(*Args) y2<Result,Args>(
        Result(*Args)(Result(*Args)) f)
        given Args satisfies Anything[] {

    function r(Anything w) {
        assert (is Result(*Args)(Anything) w);
        return f(flatten((Args args) => w(w)(*args)));
    }

    return r(r);
}
```


Using recursion, this does not satisfy the task requirements, but is included here for illustrative purposes:

```ceylon
Result(*Args) y3<Result, Args>(
        Result(*Args)(Result(*Args)) f)
        given Args satisfies Anything[]
    =>  flatten((Args args) => f(y3(f))(*args));
```



## Clojure


```lisp
(defn Y [f]
  ((fn [x] (x x))
   (fn [x]
     (f (fn [& args]
          (apply (x x) args))))))

(def fac
     (fn [f]
       (fn [n]
         (if (zero? n) 1 (* n (f (dec n)))))))

(def fib
     (fn [f]
       (fn [n]
         (condp = n
           0 0
           1 1
           (+ (f (dec n))
              (f (dec (dec n))))))))
```


{{out}}

```txt
user> ((Y fac) 10)
3628800
user> ((Y fib) 10)
55
```


<code>Y</code> can be written slightly more concisely via syntax sugar:


```lisp
(defn Y [f]
  (#(% %) #(f (fn [& args] (apply (% %) args)))))
```



## Common Lisp


```lisp
(defun Y (f)
  ((lambda (g) (funcall g g))
   (lambda (g)
     (funcall f (lambda (&rest a)
		  (apply (funcall g g) a))))))

(defun fac (n)
  (funcall
   (Y (lambda (f)
       (lambda (n)
         (if (zerop n)
	   1
	   (* n (funcall f (1- n)))))))
   n))

(defun fib (n)
  (funcall
   (Y (lambda (f)
       (lambda (n a b)
         (if (< n 1)
           a
           (funcall f (1- n) b (+ a b))))))
   n 0 1))

? (mapcar #'fac '(1 2 3 4 5 6 7 8 9 10))
(1 2 6 24 120 720 5040 40320 362880 3628800))

? (mapcar #'fib '(1 2 3 4 5 6 7 8 9 10))
(1 1 2 3 5 8 13 21 34 55)
```



## CoffeeScript


```coffeescript
Y = (f) -> g = f( (t...) -> g(t...) )
```

or

```coffeescript
Y = (f) -> ((h)->h(h))((h)->f((t...)->h(h)(t...)))
```


```coffeescript
fac = Y( (f) -> (n) -> if n > 1 then n * f(n-1) else 1 )
fib = Y( (f) -> (n) -> if n > 1 then f(n-1) + f(n-2) else n )

```



## D

A stateless generic Y combinator:

```d
import std.stdio, std.traits, std.algorithm, std.range;

auto Y(S, T...)(S delegate(T) delegate(S delegate(T)) f) {
    static struct F {
        S delegate(T) delegate(F) f;
        alias f this;
    }
    return (x => x(x))(F(x => f((T v) => x(x)(v))));
}

void main() { // Demo code:
    auto factorial = Y((int delegate(int) self) =>
        (int n) => 0 == n ? 1 : n * self(n - 1)
    );

    auto ackermann = Y((ulong delegate(ulong, ulong) self) =>
        (ulong m, ulong n) {
            if (m == 0) return n + 1;
            if (n == 0) return self(m - 1, 1);
            return self(m - 1, self(m, n - 1));
    });

    writeln("factorial: ", 10.iota.map!factorial);
    writeln("ackermann(3, 5): ", ackermann(3, 5));
}
```

{{out}}

```txt
factorial: [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
ackermann(3, 5): 253
```


=={{header|Déjà Vu}}==
{{trans|Python}}

```dejavu
Y f:
	labda y:
		labda:
			call y @y
		f
	labda x:
		x @x
	call

labda f:
	labda n:
		if < 1 n:
			* n f -- n
		else:
			1
set :fac Y

labda f:
	labda n:
		if < 1 n:
			+ f - n 2 f -- n
		else:
			1
set :fib Y

!. fac 6
!. fib 6
```

{{out}}

```txt
720
13
```



## Delphi

{{works with|Delphi XE and higher}}
May work with Delphi 2009 and 2010 too.
{{trans|C++}}
(The translation is not literal; e.g. the function argument type is left unspecified to increase generality.)

```delphi
program Y;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  YCombinator = class sealed
    class function Fix<T> (F: TFunc<TFunc<T, T>, TFunc<T, T>>): TFunc<T, T>; static;
  end;

  TRecursiveFuncWrapper<T> = record // workaround required because of QC #101272 (http://qc.embarcadero.com/wc/qcmain.aspx?d=101272)
    type
      TRecursiveFunc = reference to function (R: TRecursiveFuncWrapper<T>): TFunc<T, T>;
    var
      O: TRecursiveFunc;
  end;

class function YCombinator.Fix<T> (F: TFunc<TFunc<T, T>, TFunc<T, T>>): TFunc<T, T>;
var
  R: TRecursiveFuncWrapper<T>;
begin
  R.O := function (W: TRecursiveFuncWrapper<T>): TFunc<T, T>
    begin
      Result := F (function (I: T): T
        begin
          Result := W.O (W) (I);
        end);
    end;
  Result := R.O (R);
end;


type
  IntFunc = TFunc<Integer, Integer>;

function AlmostFac (F: IntFunc): IntFunc;
begin
  Result := function (N: Integer): Integer
    begin
      if N <= 1 then
        Result := 1
      else
        Result := N * F (N - 1);
    end;
end;

function AlmostFib (F: TFunc<Integer, Integer>): TFunc<Integer, Integer>;
begin
  Result := function (N: Integer): Integer
    begin
      if N <= 2 then
        Result := 1
      else
        Result := F (N - 1) + F (N - 2);
    end;
end;

var
  Fib, Fac: IntFunc;
begin
  Fib := YCombinator.Fix<Integer> (AlmostFib);
  Fac := YCombinator.Fix<Integer> (AlmostFac);
  Writeln ('Fib(10) = ', Fib (10));
  Writeln ('Fac(10) = ', Fac (10));
end.
```



## E

{{trans|Python}}

```e
def y := fn f { fn x { x(x) }(fn y { f(fn a { y(y)(a) }) }) }
def fac := fn f { fn n { if (n<2) {1} else { n*f(n-1) } }}
def fib := fn f { fn n { if (n == 0) {0} else if (n == 1) {1} else { f(n-1) + f(n-2) } }}
```



```e
? pragma.enable("accumulator")
? accum [] for i in 0..!10 { _.with(y(fac)(i)) }
[1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

? accum [] for i in 0..!10 { _.with(y(fib)(i)) }
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```



## EchoLisp


```scheme

;; Ref : http://www.ece.uc.edu/~franco/C511/html/Scheme/ycomb.html

 (define Y
    (lambda (X)
      ((lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg))))
       (lambda (procedure)
         (X (lambda (arg) ((procedure procedure) arg)))))))

; Fib
(define Fib* (lambda (func-arg)
    (lambda (n) (if (< n 2) n (+ (func-arg (- n 1)) (func-arg (- n 2)))))))
(define fib (Y Fib*))
(fib 6)
    → 8

; Fact
(define F*
   (lambda (func-arg) (lambda (n) (if (zero? n) 1 (* n (func-arg (- n 1)))))))
(define fact (Y F*))

(fact 10)
    → 3628800

```



## Eero

Translated from Objective-C example on this page.

```objc>#import <Foundation/Foundation.h


typedef int (^Func)(int)
typedef Func (^FuncFunc)(Func)
typedef Func (^RecursiveFunc)(id) // hide recursive typing behind dynamic typing

Func fix(FuncFunc f)
  Func r(RecursiveFunc g)
    int s(int x)
      return g(g)(x)
    return f(s)
  return r(r)

int main(int argc, const char *argv[])
  autoreleasepool

    Func almost_fac(Func f)
      return (int n | return n <= 1 ? 1 : n * f(n - 1))

    Func almost_fib(Func f)
      return (int n | return n <= 2 ? 1 : f(n - 1) + f(n - 2))

    fib := fix(almost_fib)
    fac := fix(almost_fac)

    Log('fib(10) = %d', fib(10))
    Log('fac(10) = %d', fac(10))

  return 0
```



## Ela


```ela
fix = \f -> (\x -> & f (x x)) (\x -> & f (x x))

fac _ 0 = 1
fac f n = n * f (n - 1)

fib _ 0 = 0
fib _ 1 = 1
fib f n = f (n - 1) + f (n - 2)

(fix fac 12, fix fib 12)
```


{{out}}

```txt
(479001600,144)
```


## Elena

{{trans|Smalltalk}}
ELENA 4.x :

```elena
import extensions;

singleton YCombinator
{
    fix(func)
        = (f){(x){ x(x) }((g){ f((x){ (g(g))(x) })})}(func);
}

public program()
{
    var fib := YCombinator.fix:(f => (i => (i <= 1) ? i : (f(i-1) + f(i-2)) ));
    var fact := YCombinator.fix:(f => (i => (i == 0) ? 1 : (f(i-1) * i) ));

    console.printLine("fib(10)=",fib(10));
    console.printLine("fact(10)=",fact(10));
}
```

{{out}}

```txt

fib(10)=55
fact(10)=3628800

```



## Elixir

{{trans|Python}}

```elixir

iex(1)> yc = fn f -> (fn x -> x.(x) end).(fn y -> f.(fn arg -> y.(y).(arg) end) end) end
#Function<6.90072148/1 in :erl_eval.expr/5>
iex(2)> fac = fn f -> fn n -> if n < 2 do 1 else n * f.(n-1) end end end
#Function<6.90072148/1 in :erl_eval.expr/5>
iex(3)> for i <- 0..9, do: yc.(fac).(i)
[1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
iex(4)> fib = fn f -> fn n -> if n == 0 do 0 else (if n == 1 do 1 else f.(n-1) + f.(n-2) end) end end end
#Function<6.90072148/1 in :erl_eval.expr/5>
iex(5)> for i <- 0..9, do: yc.(fib).(i)
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

```



## Elm


This is similar to the Haskell solution below, but uses a strict fixed-point combinator since Elm lacks lazy evaluation.


```elm

import Html exposing (text)

type Mu a b = Roll (Mu a b -> a -> b)

unroll : Mu a b -> (Mu a b -> a -> b)
unroll (Roll x) = x

fix : ((a -> b) -> (a -> b)) -> (a -> b)
fix f = let g r = f (\v -> unroll r r v)
        in g (Roll g)

fac : Int -> Int
fac = fix <|
    \f n -> if n <= 0
            then 1
            else n * f (n - 1)

main = text <| toString <| fac 5

```



## Erlang


```erlang
Y = fun(M) -> (fun(X) -> X(X) end)(fun (F) -> M(fun(A) -> (F(F))(A) end) end) end.

Fac = fun (F) ->
          fun (0) -> 1;
              (N) -> N * F(N-1)
          end
      end.
Fib = fun(F) ->
          fun(0) -> 0;
             (1) -> 1;
             (N) -> F(N-1) + F(N-2)
          end
      end.
(Y(Fac))(5). %% 120
(Y(Fib))(8). %% 21
```


=={{header|F Sharp|F#}}==

```fsharp
type 'a mu = Roll of ('a mu -> 'a)  // ' fixes ease syntax colouring confusion with

let unroll (Roll x) = x
// val unroll : 'a mu -> ('a mu -> 'a)

// As with most of the strict (non-deferred or non-lazy) languages,
// this is the Z-combinator with the additional 'a' parameter...
let fix f = let g = fun x a -> f (unroll x x) a in g (Roll g)
// val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>

// Although true to the factorial definition, the
// recursive call is not in tail call position, so can't be optimized
// and will overflow the call stack for the recursive calls for large ranges...
//let fac = fix (fun f n -> if n < 2 then 1I else bigint n * f (n - 1))
// val fac : (int -> BigInteger) = <fun>

// much better progressive calculation in tail call position...
let fac = fix (fun f n i -> if i < 2 then n else f () (bigint i * n) (i - 1)) <| 1I
// val fac : (int -> BigInteger) = <fun>

// Although true to the definition of Fibonacci numbers,
// this can't be tail call optimized and recursively repeats calculations
// for a horrendously inefficient exponential performance fib function...
// let fib = fix (fun fnc n -> if n < 2 then n else fnc (n - 1) + fnc (n - 2))
// val fib : (int -> BigInteger) = <fun>

// much better progressive calculation in tail call position...
let fib = fix (fun fnc f s i -> if i < 2 then f else fnc s (f + s) (i - 1)) 1I 1I
// val fib : (int -> BigInteger) = <fun>

[<EntryPoint>]
let main argv =
  fac 10 |> printfn "%A" // prints 3628800
  fib 10 |> printfn "%A" // prints 55
  0 // return an integer exit code
```

{{output}}

```txt
3628800
55
```


Note that the first `fac` definition isn't really very good as the recursion is not in tail call position and thus will build stack, although for these functions one will likely never use it to stack overflow as the result would be exceedingly large; it is better defined as per the second definition as a steadily increasing function controlled by an `int` indexing argument and thus be in tail call position as is done for the `fib` function.

Also note that the above isn't the true fix point Y-combinator which would race without the beta conversion to the Z-combinator with the included `a` argument; the Z-combinator can't be used in all cases that require a true Y-combinator such as in the formation of deferred execution sequences in the last example, as follows:


```fsharp
// same as previous...
type 'a mu = Roll of ('a mu -> 'a)  // ' fixes ease syntax colouring confusion with

// same as previous...
let unroll (Roll x) = x
// val unroll : 'a mu -> ('a mu -> 'a)

// break race condition with some deferred execution - laziness...
let fix f = let g = fun x -> f <| fun() -> (unroll x x) in g (Roll g)
// val fix : ((unit -> 'a) -> 'a -> 'a) = <fun>

// same efficient version of factorial with added deferred execution...
let fac = fix (fun f n i -> if i < 2 then n else f () (bigint i * n) (i - 1)) <| 1I
// val fac : (int -> BigInteger) = <fun>

// same efficient version of factorial with added deferred execution...
let fib = fix (fun fnc f s i -> if i < 2 then f else fnc () s (f + s) (i - 1)) 1I 1I
// val fib : (int -> BigInteger) = <fun>

// given the following definition for an infinite Co-Inductive Stream (CIS)...
type CIS<'a> = CIS of 'a * (unit -> CIS<'a>) // ' fix formatting

// define a continuous stream of Fibonacci numbers; there are other simpler ways,
// this way does not use recursion at all by using the Y-combinator, although it is
// much slower than other ways due to the many additional function calls and memory allocations,
// it demonstrates something that can't be done with the Z-combinator...
let fibs() =
  let fbsgen : (CIS<bigint> -> CIS<bigint>) =
    fix (fun fnc f (CIS(s, rest)) ->
      CIS(f + s, fun() -> fnc () s <| rest())) 1I
  Seq.unfold
    (fun (CIS(hd, rest)) -> Some(hd, rest()))
    <| fix (fun cis -> fbsgen (CIS(0I, cis)))

[<EntryPoint>]
let main argv =
  fac 10 |> printfn "%A" // prints 3628800
  fib 10 |> printfn "%A" // prints 55
  fibs() |> Seq.take 20 |> Seq.iter (printf "%A ") // prints 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765
  printfn ""
  0 // return an integer exit code
```

{{output}}

```txt
3628800
55
1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765
```


The above would be useful if F# did not have recursive functions (functions that can call themselves in their own definition), but as for most modern languages, F# does have function recursion by the use of the `rec` keyword before the function name, thus the above `fac` and `fib` functions can be written much more simply (and to run faster using tail recursion) with a recursion definition for the `fix` Y-combinator as follows, with a simple injected deferred execution to prevent race:

```fsharp
let rec fix f = f <| fun() -> fix f
// val fix : f:((unit -> 'a) -> 'a) -> 'a

// the application of this true Y-combinator is the same as for the above non function recursive version.
```


Using the Y-combinator (or Z-combinator) as expressed here is pointless as in unnecessary and makes the code slower due to the extra function calls through the call stack, with the first non-function recursive implementation even slower than the second function recursion one; a non Y-combinator version can use function recursion with tail call optimization to simplify looping for about 100 times the speed in the actual loop overhead; thus, this is primarily an intellectual exercise.

Also note that these Y-combinators/Z-combinator are the non sharing kind; for certain types of algorithms that require that the input and output recursive values be the same (such as the same sequence or lazy list but made reference at difference stages), these will work but may be many times slower as in over 10 times slower than using binding recursion if the language allows it; F# allows binding recursion with a warning.


## Factor

In rosettacode/Y.factor

```factor
USING: fry kernel math ;
IN: rosettacode.Y
: Y ( quot -- quot )
    '[ [ dup call call ] curry @ ] dup call ; inline

: almost-fac ( quot -- quot )
    '[ dup zero? [ drop 1 ] [ dup 1 - @ * ] if ] ;

: almost-fib ( quot -- quot )
    '[ dup 2 >= [ 1 2 [ - @ ] bi-curry@ bi + ] when ] ;
```

In rosettacode/Y-tests.factor

```factor
USING: kernel tools.test rosettacode.Y ;
IN: rosettacode.Y.tests

[ 120 ] [ 5 [ almost-fac ] Y call ] unit-test
[ 8 ]   [ 6 [ almost-fib ] Y call ] unit-test
```

running the tests :

```txt
 ( scratchpad - auto ) "rosettacode.Y" test
Loading resource:work/rosettacode/Y/Y-tests.factor
Unit Test: { [ 120 ] [ 5 [ almost-fac ] Y call ] }
Unit Test: { [ 8 ]   [ 6 [ almost-fib ] Y call ] }
```



## Forth


```Forth
\ Address of an xt.
variable 'xt
\ Make room for an xt.
: xt, ( -- ) here 'xt !  1 cells allot ;
\ Store xt.
: !xt ( xt -- ) 'xt @ ! ;
\ Compile fetching the xt.
: @xt, ( -- ) 'xt @ postpone literal postpone @ ;
\ Compile the Y combinator.
: y, ( xt1 -- xt2 ) >r :noname @xt, r> compile, postpone ; ;
\ Make a new instance of the Y combinator.
: y ( xt1 -- xt2 ) xt, y, dup !xt ;
```


Samples:

```Forth
\ Factorial
10 :noname ( u1 xt -- u2 ) over ?dup if 1- swap execute * else 2drop 1 then ;
y execute . 3628800 ok

\ Fibonacci
10 :noname ( u1 xt -- u2 ) over 2 < if drop else >r 1- dup r@ execute swap 1- r> execute + then ;
y execute . 55 ok

```



## Falcon


```Falcon

Y = { f => {x=> {n => f(x(x))(n)}} ({x=> {n => f(x(x))(n)}}) }
facStep = { f => {x => x < 1 ? 1 : x*f(x-1) }}
fibStep = { f => {x => x == 0 ? 0 : (x == 1 ? 1 : f(x-1) + f(x-2))}}

YFac = Y(facStep)
YFib = Y(fibStep)

> "Factorial 10: ", YFac(10)
> "Fibonacci 10: ", YFib(10)

```



## GAP


```gap
Y := function(f)
    local u;
    u := x -> x(x);
    return u(y -> f(a -> y(y)(a)));
end;

fib := function(f)
    local u;
    u := function(n)
        if n < 2 then
            return n;
        else
            return f(n-1) + f(n-2);
        fi;
    end;
    return u;
end;

Y(fib)(10);
# 55

fac := function(f)
    local u;
    u := function(n)
        if n < 2 then
            return 1;
        else
            return n*f(n-1);
        fi;
    end;
    return u;
end;

Y(fac)(8);
# 40320
```



## Genyris

{{trans|Scheme}}

```genyris
def fac (f)
    function (n)
      if (equal? n 0) 1
        * n (f (- n 1))
def fib (f)
  function (n)
    cond
      (equal? n 0) 0
      (equal? n 1) 1
      else (+ (f (- n 1)) (f (- n 2)))

def Y (f)
  (function (x) (x x))
      function (y)
          f
             function (&rest args) (apply (y y) args)

assertEqual ((Y fac) 5) 120
assertEqual ((Y fib) 8) 21
```



## Go


```go
package main

import "fmt"

type Func func(int) int
type FuncFunc func(Func) Func
type RecursiveFunc func (RecursiveFunc) Func

func main() {
	fac := Y(almost_fac)
	fib := Y(almost_fib)
	fmt.Println("fac(10) = ", fac(10))
	fmt.Println("fib(10) = ", fib(10))
}

func Y(f FuncFunc) Func {
	g := func(r RecursiveFunc) Func {
		return f(func(x int) int {
			return r(r)(x)
		})
	}
	return g(g)
}

func almost_fac(f Func) Func {
	return func(x int) int {
		if x <= 1 {
			return 1
		}
		return x * f(x-1)
	}
}

func almost_fib(f Func) Func {
	return func(x int) int {
		if x <= 2 {
			return 1
		}
		return f(x-1)+f(x-2)
	}
}
```

{{out}}

```txt

fac(10) =  3628800
fib(10) =  55

```


The usual version using recursion, disallowed by the task:

```go
func Y(f FuncFunc) Func {
	return func(x int) int {
		return f(Y(f))(x)
	}
}
```



## Groovy

Here is the simplest (unary) form of applicative order Y:

```groovy
def Y = { le -> ({ f -> f(f) })({ f -> le { x -> f(f)(x) } }) }

def factorial = Y { fac ->
    { n -> n <= 2 ? n : n * fac(n - 1) }
}

assert 2432902008176640000 == factorial(20G)

def fib = Y { fibStar ->
    { n -> n <= 1 ? n : fibStar(n - 1) + fibStar(n - 2) }
}

assert fib(10) == 55
```

This version was translated from the one in ''The Little Schemer'' by Friedman and Felleisen. The use of the variable name ''le'' is due to the fact that the authors derive Y from an ordinary recursive '''''le'''ngth'' function.

A variadic version of Y in Groovy looks like this:

```groovy
def Y = { le -> ({ f -> f(f) })({ f -> le { Object[] args -> f(f)(*args) } }) }

def mul = Y { mulStar -> { a, b -> a ? b + mulStar(a - 1, b) : 0 } }

1.upto(10) {
    assert mul(it, 10) == it * 10
}
```



## Haskell

The obvious definition of the Y combinator <code>(\f-> (\x -> f (x x)) (\x-> f (x x)))</code> cannot be used in Haskell because it contains an infinite recursive type (<code>a = a -> b</code>). Defining a data type (Mu) allows this recursion to be broken.

```haskell
newtype Mu a = Roll
  { unroll :: Mu a -> a }

fix :: (a -> a) -> a
fix = g <*> (Roll . g)
  where
    g = (. (>>= id) unroll)

- this version is not in tail call position...
-- fac :: Integer -> Integer
-- fac =
--   fix $ \f n -> if n <= 0 then 1 else n * f (n - 1)

-- this version builds a progression from tail call position and is more efficient...
fac :: Integer -> Integer
fac =
  (fix $ \f n i -> if i <= 0 then n else f (i * n) (i - 1)) 1

-- make fibs a function, else memory leak as
-- head of the list can never be released as per:
--   https://wiki.haskell.org/Memory_leak, type 1.1
-- overly complex version...
{--
fibs :: () -> [Integer]
fibs() =
  fix $
    (0 :) . (1 :) .
      (fix
        (\f (x:xs) (y:ys) ->
          case x + y of n -> n `seq` n : f xs ys) <*> tail)
--}

-- easier to read, simpler (faster) version...
fibs :: () -> [Integer]
fibs() = 0 : 1 : fix fibs_ 0 1
  where
    fibs_ fnc f s =
      case f + s of n -> n `seq` n : fnc s n

main :: IO ()
main =
  mapM_
    print
    [ map fac [1 .. 20]
    , take 20 $ fibs()
    ]
```


The usual version uses recursion on a binding, disallowed by the task, to define the <code>fix</code> itself; but the definitions produced by this <code>fix</code> does ''not'' use recursion on value bindings although it does use recursion when defining a function (not possible in all languages), so it can be viewed as a true Y-combinator too:


```haskell
-- note that this version of fix uses function recursion in its own definition;
-- thus its use just means that the recursion has been "pulled" into the "fix" function,
-- instead of the function that uses it...
fix :: (a -> a) -> a
fix f = f (fix f) -- _not_ the {fix f = x where x = f x}

fac :: Integer -> Integer
fac =
  (fix $
    \f n i ->
      if i <= 0 then n
      else f (i * n) (i - 1)) 1

fib :: Integer -> Integer
fib =
  (fix $
    \fnc f s i ->
      if i <= 1 then f
      else case f + s of n -> n `seq` fnc s n (i - 1)) 0 1

{--
-- compute a lazy infinite list. This is
-- a Y-combinator version of: fibs() = 0:1:zipWith (+) fibs (tail fibs)
-- which is the same as the above version but easier to read...
fibs :: () -> [Integer]
fibs() = fix fibs_
  where
    zipP f (x:xs) (y:ys) =
      case x + y of n -> n `seq` n : f xs ys
    fibs_ a = 0 : 1 : fix zipP a (tail a)
--}

-- easier to read, simpler (faster) version...
fibs :: () -> [Integer]
fibs() = 0 : 1 : fix fibs_ 0 1
  where
    fibs_ fnc f s =
      case f + s of n -> n `seq` n : fnc s n

-- This code shows how the functions can be used:
main :: IO ()
main =
  mapM_
    print
    [ map fac [1 .. 20]
    , map fib [1 .. 20]
    , take 20 fibs()
    ]
```


Now just because something is possible using the Y-combinator doesn't mean that it is practical:  the above implementations can't compute much past the 1000th number in the Fibonacci list sequence and is quite slow at doing so; using direct function recursive routines compute about 100 times faster and don't hang for large ranges, nor give problems compiling as the first version does (GHC version 8.4.3 at -O1 optimization level).

If one has recursive functions as Haskell does and as used by the second `fix`, there is no need to use `fix`/the Y-combinator at all since one may as well just write the recursion directly.  The Y-combinator may be interesting mathematically, but it isn't very practical when one has any other choice.


## J


===Non-tacit version===
Unfortunately, in principle, J functions cannot take functions of the same type as arguments.  In other words, verbs (functions) cannot take verbs, and adverbs or conjunctions (higher-order functions) cannot take adverbs or conjunctions.  This implementation uses the body, a literal (string), of an explicit adverb (a higher-order function with a left argument) as the argument for Y, to represent the adverb for which the product of Y is a fixed-point verb; Y itself is also an adverb.

```j
Y=. '('':''<@;(1;~":0)<@;<@((":0)&;))'(2 : 0 '')
  (1 : (m,'u'))(1 : (m,'''u u`:6('',(5!:5<''u''),'')`:6 y'''))(1 :'u u`:6')
)

```

This Y combinator follows the standard method:  it produces a fixed-point which reproduces and transforms itself anonymously according to the adverb represented by Y's argument.  All names (variables) refer to arguments of the enclosing adverbs and there are no assignments.

The factorial and Fibonacci examples follow:

```j
   'if. * y do. y * u <: y else. 1 end.' Y 10 NB. Factorial
3628800
          '(u@:<:@:<: + u@:<:)^:(1 < ])' Y 10 NB. Fibonacci
55
```

The names u, x, and y are J's standard names for arguments; the name y represents the argument of u and the name u represents the verb argument of the adverb for which Y produces a fixed-point.  Any verb can also be expressed tacitly, without any reference to its argument(s), as in the Fibonacci example.

A structured derivation of a Y with states follows (the stateless version can be produced by replacing all the names by its referents):

```j
   arb=. ':'<@;(1;~":0)<@;<@((":0)&;)                     NB. AR of an explicit adverb from its body

   ara=. 1 :'arb u'                                       NB. The verb arb as an adverb
   srt=. 1 :'arb ''u u`:6('' , (5!:5<''u'') , '')`:6 y''' NB. AR of the self-replication and transformation adverb
   gab=. 1 :'u u`:6'                                      NB. The AR of the adverb and the adverb itself as a train

   Y=. ara srt gab                                        NB. Train of adverbs
```

The adverb Y, apart from using a representation as Y's argument, satisfies the task's requirements.  However, it only works for monadic verbs (functions with a right argument).  J's verbs can also be dyadic (functions with a left and right arguments) and ambivalent (almost all J's primitive verbs are ambivalent; for example - can be used as in - 1 and 2 - 1).  The following adverb (XY) implements anonymous recursion of monadic, dyadic, and ambivalent verbs (the name x represents the left argument of u),

```j
XY=. (1 :'('':''<@;(1;~":0)<@;<@((":0)&;))u')(1 :'('':''<@;(1;~":0)<@;<@((":0)&;))((''u u`:6('',(5!:5<''u''),'')`:6 y''),(10{a.),'':'',(10{a.),''x(u u`:6('',(5!:5<''u''),'')`:6)y'')')(1 :'u u`:6')
```

The following are examples of anonymous dyadic and ambivalent recursions,

```j
   1 2 3 '([:`(>:@:])`(<:@:[ u 1:)`(<:@[ u [ u <:@:])@.(#.@,&*))'XY"0/  1 2 3 4 5 NB. Ackermann function...
 3  4  5   6   7
 5  7  9  11  13
13 29 61 125 253
                               '1:`(<: u <:)@.* : (+ + 2 * u@:])'XY"0/~ i.7       NB. Ambivalent recursion...
2  5 14 35 80 173 362
3  6 15 36 81 174 363
4  7 16 37 82 175 364
5  8 17 38 83 176 365
6  9 18 39 84 177 366
7 10 19 40 85 178 367
8 11 20 41 86 179 368
   NB. OEIS A097813 - main diagonal
   NB. OEIS A050488 = A097813 - 1 - adyacent upper off-diagonal
```


J supports directly anonymous tacit recursion via the verb $: and for tacit recursions, XY is equivalent to the adverb,

```j
YX=. (1 :'('':''<@;(1;~":0)<@;<@((":0)&;))u')($:`)(`:6)
```



### Tacit version

The Y combinator can be implemented indirectly using, for example, the linear representations of verbs (Y becomes a wrapper which takes an ad hoc verb as an argument and serializes it; the underlying self-referring system interprets the serialized representation of a verb as the corresponding verb):

```j
Y=. ((((&>)/)((((^:_1)b.)(`(<'0';_1)))(`:6)))(&([ 128!:2 ,&<)))
```

The factorial and Fibonacci examples:

```j
   u=. [ NB. Function (left)
   n=. ] NB. Argument (right)
   sr=. [ apply f. ,&< NB. Self referring

   fac=. (1:`(n * u sr n - 1:)) @. (0 < n)
   fac f. Y 10
3628800

   Fib=. ((u sr n - 2:) + u sr n - 1:) ^: (1 < n)
   Fib f. Y 10
55
```

The stateless functions are shown next (the f. adverb replaces all embedded names by its referents):

```j
   fac f. Y NB. Factorial...
'1:`(] * [ ([ 128!:2 ,&<) ] - 1:)@.(0 < ])&>/'&([ 128!:2 ,&<)

   fac f.   NB. Factorial step...
1:`(] * [ ([ 128!:2 ,&<) ] - 1:)@.(0 < ])


   Fib f. Y NB. Fibonacci...
'(([ ([ 128!:2 ,&<) ] - 2:) + [ ([ 128!:2 ,&<) ] - 1:)^:(1 < ])&>/'&([ 128!:2 ,&<)

   Fib f.   NB. Fibonacci step...
(([ ([ 128!:2 ,&<) ] - 2:) + [ ([ 128!:2 ,&<) ] - 1:)^:(1 < ])
```

A structured derivation of Y follows:

```j
   sr=. [ apply f.,&<                 NB. Self referring
   lv=. (((^:_1)b.)(`(<'0';_1)))(`:6) NB. Linear representation of a verb argument
   Y=. (&>)/lv(&sr)                   NB. Y with embedded states
   Y=. 'Y'f.                          NB. Fixing it...
   Y                                  NB. ... To make it stateless (i.e., a combinator)
((((&>)/)((((^:_1)b.)(`_1))(`:6)))(&([ 128!:2 ,&<)))
```



### Explicit alternate implementation


Another approach:


```j
Y=:1 :0
  f=. u Defer
  (5!:1<'f') f y
)

Defer=: 1 :0
:
  g=. x&(x`:6)
  (5!:1<'g') u y
)

almost_factorial=: 4 :0
  if. 0 >: y do. 1
  else. y * x`:6 y-1 end.
)

almost_fibonacci=: 4 :0
  if. 2 > y do. y
  else. (x`:6 y-1) + x`:6 y-2 end.
)
```


Example use:


```J
   almost_factorial Y 9
362880
   almost_fibonacci Y 9
34
   almost_fibonacci Y"0 i. 10
0 1 1 2 3 5 8 13 21 34
```


Or, if you would prefer to not have a dependency on the definition of Defer, an equivalent expression would be:


```J
Y=:2 :0(0 :0)
NB. this block will be n in the second part
:
  g=. x&(x`:6)
  (5!:1<'g') u y
)
  f=. u (1 :n)
  (5!:1<'f') f y
)
```


That said, if you think of association with a name as state (because in different contexts the association may not exist, or may be different) you might also want to remove that association in the context of the Y combinator.

For example:


```J
   almost_factorial f. Y 10
3628800
```



## Java


{{works with|Java|8+}}

```java5
import java.util.function.Function;

public interface YCombinator {
  interface RecursiveFunction<F> extends Function<RecursiveFunction<F>, F> { }
  public static <A,B> Function<A,B> Y(Function<Function<A,B>, Function<A,B>> f) {
    RecursiveFunction<Function<A,B>> r = w -> f.apply(x -> w.apply(w).apply(x));
    return r.apply(r);
  }

  public static void main(String... arguments) {
    Function<Integer,Integer> fib = Y(f -> n ->
      (n <= 2)
        ? 1
        : (f.apply(n - 1) + f.apply(n - 2))
    );
    Function<Integer,Integer> fac = Y(f -> n ->
      (n <= 1)
        ? 1
        : (n * f.apply(n - 1))
    );

    System.out.println("fib(10) = " + fib.apply(10));
    System.out.println("fac(10) = " + fac.apply(10));
  }
}
```

{{out}}

```txt

fib(10) = 55
fac(10) = 3628800

```

The usual version using recursion, disallowed by the task:

```java5
    public static <A,B> Function<A,B> Y(Function<Function<A,B>, Function<A,B>> f) {
        return x -> f.apply(Y(f)).apply(x);
    }
```


Another version which is disallowed because a function passes itself, which is also a kind of recursion:

```java5
    public static <A,B> Function<A,B> Y(Function<Function<A,B>, Function<A,B>> f) {
        return new Function<A,B>() {
	    public B apply(A x) {
		return f.apply(this).apply(x);
	    }
	};
    }
```


{{works with|Java|pre-8}}
We define a generic function interface like Java 8's <code>Function</code>.

```java5
interface Function<A, B> {
    public B call(A x);
}

public class YCombinator {
    interface RecursiveFunc<F> extends Function<RecursiveFunc<F>, F> { }

    public static <A,B> Function<A,B> fix(final Function<Function<A,B>, Function<A,B>> f) {
        RecursiveFunc<Function<A,B>> r =
            new RecursiveFunc<Function<A,B>>() {
            public Function<A,B> call(final RecursiveFunc<Function<A,B>> w) {
                return f.call(new Function<A,B>() {
                        public B call(A x) {
                            return w.call(w).call(x);
                        }
                    });
            }
        };
        return r.call(r);
    }

    public static void main(String[] args) {
        Function<Function<Integer,Integer>, Function<Integer,Integer>> almost_fib =
            new Function<Function<Integer,Integer>, Function<Integer,Integer>>() {
            public Function<Integer,Integer> call(final Function<Integer,Integer> f) {
                return new Function<Integer,Integer>() {
                    public Integer call(Integer n) {
                        if (n <= 2) return 1;
                        return f.call(n - 1) + f.call(n - 2);
                    }
                };
            }
        };

        Function<Function<Integer,Integer>, Function<Integer,Integer>> almost_fac =
            new Function<Function<Integer,Integer>, Function<Integer,Integer>>() {
            public Function<Integer,Integer> call(final Function<Integer,Integer> f) {
                return new Function<Integer,Integer>() {
                    public Integer call(Integer n) {
                        if (n <= 1) return 1;
                        return n * f.call(n - 1);
                    }
                };
            }
        };

        Function<Integer,Integer> fib = fix(almost_fib);
        Function<Integer,Integer> fac = fix(almost_fac);

        System.out.println("fib(10) = " + fib.call(10));
        System.out.println("fac(10) = " + fac.call(10));
    }
}
```


The following code modifies the Function interface such that multiple parameters (via varargs) are supported, simplifies the y function considerably, and the [[Ackermann function#Java|Ackermann function]] has been included in this implementation (mostly because both [[Y combinator#D|D]] and [[Y combinator#PicoLisp|PicoLisp]] include it in their own implementations).


```java5
import java.util.function.Function;

@FunctionalInterface
public interface SelfApplicable<OUTPUT> extends Function<SelfApplicable<OUTPUT>, OUTPUT> {
  public default OUTPUT selfApply() {
    return apply(this);
  }
}
```



```java5
import java.util.function.Function;
import java.util.function.UnaryOperator;

@FunctionalInterface
public interface FixedPoint<FUNCTION> extends Function<UnaryOperator<FUNCTION>, FUNCTION> {}
```



```java5
import java.util.Arrays;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.BiFunction;

@FunctionalInterface
public interface VarargsFunction<INPUTS, OUTPUT> extends Function<INPUTS[], OUTPUT> {
  @SuppressWarnings("unchecked")
  public OUTPUT apply(INPUTS... inputs);

  public static <INPUTS, OUTPUT> VarargsFunction<INPUTS, OUTPUT> from(Function<INPUTS[], OUTPUT> function) {
    return function::apply;
  }

  public static <INPUTS, OUTPUT> VarargsFunction<INPUTS, OUTPUT> upgrade(Function<INPUTS, OUTPUT> function) {
    return inputs -> function.apply(inputs[0]);
  }

  public static <INPUTS, OUTPUT> VarargsFunction<INPUTS, OUTPUT> upgrade(BiFunction<INPUTS, INPUTS, OUTPUT> function) {
    return inputs -> function.apply(inputs[0], inputs[1]);
  }

  @SuppressWarnings("unchecked")
  public default <POST_OUTPUT> VarargsFunction<INPUTS, POST_OUTPUT> andThen(
      VarargsFunction<OUTPUT, POST_OUTPUT> after) {
    return inputs -> after.apply(apply(inputs));
  }

  @SuppressWarnings("unchecked")
  public default Function<INPUTS, OUTPUT> toFunction() {
    return input -> apply(input);
  }

  @SuppressWarnings("unchecked")
  public default BiFunction<INPUTS, INPUTS, OUTPUT> toBiFunction() {
    return (input, input2) -> apply(input, input2);
  }

  @SuppressWarnings("unchecked")
  public default <PRE_INPUTS> VarargsFunction<PRE_INPUTS, OUTPUT> transformArguments(Function<PRE_INPUTS, INPUTS> transformer) {
    return inputs -> apply((INPUTS[]) Arrays.stream(inputs).parallel().map(transformer).toArray());
  }
}
```



```java5
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

@FunctionalInterface
public interface Y<FUNCTION> extends SelfApplicable<FixedPoint<FUNCTION>> {
  public static void main(String... arguments) {
    BigInteger TWO = BigInteger.ONE.add(BigInteger.ONE);

    Function<Number, Long> toLong = Number::longValue;
    Function<Number, BigInteger> toBigInteger = toLong.andThen(BigInteger::valueOf);

    /* Based on https://gist.github.com/aruld/3965968/#comment-604392 */
    Y<VarargsFunction<Number, Number>> combinator = y -> f -> x -> f.apply(y.selfApply().apply(f)).apply(x);
    FixedPoint<VarargsFunction<Number, Number>> fixedPoint = combinator.selfApply();

    VarargsFunction<Number, Number> fibonacci = fixedPoint.apply(
      f -> VarargsFunction.upgrade(
        toBigInteger.andThen(
          n -> (n.compareTo(TWO) <= 0)
            ? 1
            : new BigInteger(f.apply(n.subtract(BigInteger.ONE)).toString())
              .add(new BigInteger(f.apply(n.subtract(TWO)).toString()))
        )
      )
    );

    VarargsFunction<Number, Number> factorial = fixedPoint.apply(
      f -> VarargsFunction.upgrade(
        toBigInteger.andThen(
          n -> (n.compareTo(BigInteger.ONE) <= 0)
            ? 1
            : n.multiply(new BigInteger(f.apply(n.subtract(BigInteger.ONE)).toString()))
        )
      )
    );

    VarargsFunction<Number, Number> ackermann = fixedPoint.apply(
      f -> VarargsFunction.upgrade(
        (BigInteger m, BigInteger n) -> m.equals(BigInteger.ZERO)
          ? n.add(BigInteger.ONE)
          : f.apply(
              m.subtract(BigInteger.ONE),
              n.equals(BigInteger.ZERO)
                ? BigInteger.ONE
                  : f.apply(m, n.subtract(BigInteger.ONE))
            )
      ).transformArguments(toBigInteger)
    );

    Map<String, VarargsFunction<Number, Number>> functions = new HashMap<>();
    functions.put("fibonacci", fibonacci);
    functions.put("factorial", factorial);
    functions.put("ackermann", ackermann);

    Map<VarargsFunction<Number, Number>, Number[]> parameters = new HashMap<>();
    parameters.put(functions.get("fibonacci"), new Number[]{20});
    parameters.put(functions.get("factorial"), new Number[]{10});
    parameters.put(functions.get("ackermann"), new Number[]{3, 2});

    functions.entrySet().stream().parallel().map(
      entry -> entry.getKey()
        + Arrays.toString(parameters.get(entry.getValue()))
        + " = "
        + entry.getValue().apply(parameters.get(entry.getValue()))
    ).forEach(System.out::println);
  }
}
```


{{out}} (may depend on which function gets processed first):
<lang>factorial[10] = 3628800
ackermann[3, 2] = 29
fibonacci[20] = 6765
```



## JavaScript

The standard version of the Y combinator does not use lexically bound local variables (or any local variables at all), which necessitates adding a wrapper function and some code duplication - the remaining locale variables are only there to make the relationship to the previous implementation more explicit:

```javascript
function Y(f) {
    var g = f((function(h) {
        return function() {
            var g = f(h(h));
            return g.apply(this, arguments);
        }
    })(function(h) {
        return function() {
            var g = f(h(h));
            return g.apply(this, arguments);
        }
    }));
    return g;
}

var fac = Y(function(f) {
    return function (n) {
        return n > 1 ? n * f(n - 1) : 1;
    };
});

var fib = Y(function(f) {
    return function(n) {
        return n > 1 ? f(n - 1) + f(n - 2) : n;
    };
});
```

Changing the order of function application (i.e. the place where <code>f</code> gets called) and making use of the fact that we're generating a fixed-point, this can be reduced to

```javascript
function Y(f) {
    return (function(h) {
        return h(h);
    })(function(h) {
        return f(function() {
            return h(h).apply(this, arguments);
        });
    });
}
```

A functionally equivalent version using the implicit <code>this</code> parameter is also possible:

```javascript
function pseudoY(f) {
    return (function(h) {
        return h(h);
    })(function(h) {
        return f.bind(function() {
            return h(h).apply(null, arguments);
        });
    });
}

var fac = pseudoY(function(n) {
    return n > 1 ? n * this(n - 1) : 1;
});

var fib = pseudoY(function(n) {
    return n > 1 ? this(n - 1) + this(n - 2) : n;
});
```

However, <code>pseudoY()</code> is not a fixed-point combinator.

The usual version using recursion, disallowed by the task:

```javascript
function Y(f) {
    return function() {
    	return f(Y(f)).apply(this, arguments);
    };
}
```


Another version which is disallowed because it uses <code>arguments.callee</code> for a function to get itself recursively:

```javascript
function Y(f) {
    return function() {
    	return f(arguments.callee).apply(this, arguments);
    };
}
```

===ECMAScript 2015 (ES6) variants===
Since ECMAScript 2015 (ES6) just reached final draft, there are new ways to encode the applicative order Y combinator.
These use the new fat arrow function expression syntax, and are made to allow functions of more than one argument through the use of new rest parameters syntax and the corresponding new spread operator syntax. Also showcases new default parameter value syntax:

```javascript
let
    Y= // Except for the η-abstraction necessary for applicative order languages, this is the formal Y combinator.
        f=>((g=>(f((...x)=>g(g)(...x))))
            (g=>(f((...x)=>g(g)(...x))))),
    Y2= // Using β-abstraction to eliminate code repetition.
        f=>((f=>f(f))
            (g=>(f((...x)=>g(g)(...x))))),
    Y3= // Using β-abstraction to separate out the self application combinator δ.
        ((δ=>f=>δ(g=>(f((...x)=>g(g)(...x)))))
         ((f=>f(f)))),
    fix= // β/η-equivalent fix point combinator. Easier to convert to memoise than the Y combinator.
        (((f)=>(g)=>(h)=>(f(h)(g(h)))) // The Substitute combinator out of SKI calculus
         ((f)=>(g)=>(...x)=>(f(g(g)))(...x)) // S((S(KS)K)S(S(KS)K))(KI)
         ((f)=>(g)=>(...x)=>(f(g(g)))(...x))),
    fix2= // β/η-converted form of fix above into a more compact form
        f=>(f=>f(f))(g=>(...x)=>f(g(g))(...x)),
    opentailfact= // Open version of the tail call variant of the factorial function
        fact=>(n,m=1)=>n<2?m:fact(n-1,n*m);
    tailfact= // Tail call version of factorial function
        Y(opentailfact);
```

ECMAScript 2015 (ES6) also permits a really compact polyvariadic variant for mutually recursive functions:

```javascript
let
    polyfix= // A version that takes an array instead of multiple arguments would simply use l instead of (...l) for parameter
        (...l)=>(
            (f=>f(f))
            (g=>l.map(f=>(...x)=>f(...g(g))(...x)))),
    [even,odd]= // The new destructive assignment syntax for arrays
        polyfix(
            (even,odd)=>n=>(n===0)||odd(n-1),
            (even,odd)=>n=>(n!==0)&&even(n-1));
```


A minimalist version:


```javascript>var Y = f =
 (x => x(x))(y => f(x => y(y)(x)));
var fac = Y(f => n => n > 1 ? n * f(n-1) : 1);
```



## Joy


```joy
DEFINE y == [dup cons] swap concat dup cons i;

     fac == [ [pop null] [pop succ] [[dup pred] dip i *] ifte ] y.
```



## Julia


```julia

julia> """
       # Y combinator

       * `λf. (λx. f (x x)) (λx. f (x x))`
       """
       Y = f -> (x -> x(x))(y -> f((t...) -> y(y)(t...)))

```


Usage:


```julia

julia> "# Factorial"
       fac = f -> (n -> n < 2 ? 1 : n * f(n - 1))

julia> "# Fibonacci"
       fib = f -> (n -> n == 0 ? 0 : (n == 1 ? 1 : f(n - 1) + f(n - 2)))

julia> [Y(fac)(i) for i = 1:10]
10-element Array{Any,1}:
       1
       2
       6
      24
     120
     720
    5040
   40320
  362880
 3628800

julia> [Y(fib)(i) for i = 1:10]
10-element Array{Any,1}:
  1
  1
  2
  3
  5
  8
 13
 21
 34
 55

```



## Kitten



```kitten
define y<S..., T...> (S..., (S..., (S... -> T...) -> T...) -> T...):
  -> f; { f y } f call

define fac (Int32, (Int32 -> Int32) -> Int32):
  -> x, rec;
  if (x <= 1) { 1 } else { (x - 1) rec call * x }

define fib (Int32, (Int32 -> Int32) -> Int32):
  -> x, rec;
  if (x <= 2):
    1
  else:
    (x - 1) rec call -> a;
    (x - 2) rec call -> b;
    a + b

5  \fac y say  // 120
10 \fib y say  // 55

```



## Kotlin


```scala
// version 1.1.2

typealias Func<T, R> = (T) -> R

class RecursiveFunc<T, R>(val p: (RecursiveFunc<T, R>) -> Func<T, R>)

fun <T, R> y(f: (Func<T, R>) -> Func<T, R>): Func<T, R> {
    val rec = RecursiveFunc<T, R> { r -> f { r.p(r)(it) } }
    return rec.p(rec)
}

fun fac(f: Func<Int, Int>) = { x: Int -> if (x <= 1) 1 else x * f(x - 1) }

fun fib(f: Func<Int, Int>) = { x: Int -> if (x <= 2) 1 else f(x - 1) + f(x - 2) }

fun main(args: Array<String>) {
    print("Factorial(1..10)   : ")
    for (i in 1..10) print("${y(::fac)(i)}  ")
    print("\nFibonacci(1..10)   : ")
    for (i in 1..10) print("${y(::fib)(i)}  ")
    println()
}
```


{{out}}

```txt

Factorial(1..10)   : 1  2  6  24  120  720  5040  40320  362880  3628800
Fibonacci(1..10)   : 1  1  2  3  5  8  13  21  34  55

```



## Lambdatalk

Tested in http://epsilonwiki.free.fr/lambdaway/?view=Ycombinator


```Scheme

1) defining the Ycombinator
{def Y
 {lambda {:f :n}
  {:f :f :n}}}

2) defining non recursive functions
2.1) factorial
{def almost-fac
 {lambda {:f :n}
  {if {= :n 1}
   then 1
   else {* :n {:f :f {- :n 1}}}}}}

2.2) fibonacci
{def almost-fibo
 {lambda {:f :n}
  {if {<   :n 2}
   then 1
   else {+ {:f :f {- :n 1}} {:f :f {- :n 2}}}}}}

3) testing
{Y almost-fac 6}
-> 720
{Y almost-fibo 8}
-> 34

We could also forget the Ycombinator and names:

1) fac:
{{lambda {:f :n} {:f :f :n}}
 {lambda {:f :n}
  {if {= :n 1}
   then 1
   else {* :n {:f :f {- :n 1}}}}} 6}
-> 720

2) fibo:
{{lambda {:f :n} {:f :f :n}}
 {{lambda {:f :n}
  {if {<   :n 2} then 1
   else {+ {:f :f {- :n 1}} {:f :f {- :n 2}}}}}} 8}
-> 34


```



## Lua


```lua
Y = function (f)
   return function(...)
      return (function(x) return x(x) end)(function(x) return f(function(y) return x(x)(y) end) end)(...)
   end
end

```


Usage:


```lua
almostfactorial = function(f) return function(n) return n > 0 and n * f(n-1) or 1 end end
almostfibs = function(f) return function(n) return n < 2 and n or f(n-1) + f(n-2) end end
factorial, fibs = Y(almostfactorial), Y(almostfibs)
print(factorial(7))
```




## M2000 Interpreter

Lambda functions in M2000 are value types. They have a list of closures, but closures are copies, except for those closures which are reference types. Lambdas can keep state in closures (they are mutable). But here we didn't do that. Y combinator is a lambda which return a lambda with a closure as f function. This function called passing as first argument itself by value.

```M2000 Interpreter

Module Ycombinator {
      \\ y() return value. no use of closure
      y=lambda (g, x)->g(g, x)
      Print y(lambda (g, n)->if(n=0->1, n*g(g, n-1)), 10)
      Print y(lambda (g, n)->if(n<=1->n,g(g, n-1)+g(g, n-2)), 10)

      \\ Using closure in y, y() return function
      y=lambda (g)->lambda g (x) -> g(g, x)
      fact=y((lambda (g, n)-> if(n=0->1, n*g(g, n-1))))
      Print fact(6), fact(24)
      fib=y(lambda (g, n)->if(n<=1->n,g(g, n-1)+g(g, n-2)))
      Print  fib(10)
}
Ycombinator

```



```M2000 Interpreter

Module Checkit {
      \\ all lambda arguments passed by value in this example
      \\ There is no recursion in these lambdas
      \\ Y combinator make  argument f as closure, as a copy of f
      \\ m(m, argument) pass as first argument a copy of m
      \\ so never a function, here, call itself, only call a copy who get it as argument before the call.
      Y=lambda (f)-> {
            =lambda f (x)->f(f,x)
      }
      fac_step=lambda (m, n)-> {
            if n<2 then {
                  =1
            } else {
                  =n*m(m, n-1)
            }
      }
      fac=Y(fac_step)
      fib_step=lambda (m, n)-> {
            if n<=1 then {
                  =n
            } else {
                  =m(m, n-1)+m(m, n-2)
            }
      }
      fib=Y(fib_step)
      For i=1 to 10
            Print fib(i), fac(i)
      Next i
}
Checkit
Module CheckRecursion {
    fac=lambda (n) -> {
             if n<2 then {
                  =1
            } else {
                  =n*Lambda(n-1)
            }
      }
      fib=lambda (n) -> {
            if n<=1 then {
                  =n
            } else {
                  =lambda(n-1)+lambda(n-2)
            }
      }
      For i=1 to 10
            Print fib(i), fac(i)
      Next i
}
CheckRecursion

```



## MANOOL

Here one additional technique is demonstrated: the Y combinator is applied to a function ''during compilation'' due to the <code>$</code> operator, which is optional:

```MANOOL

{ {extern "manool.org.18/std/0.3/all"} in
: let { Y = {proc {F} as {proc {X} as X[X]}[{proc {X} with {F} as F[{proc {Y} with {X} as X[X][Y]}]}]} } in
  { for { N = Range[10] } do
  : (WriteLine) Out; N "! = "
    {Y: proc {Rec} as {proc {N} with {Rec} as: if N == 0 then 1 else N * Rec[N - 1]}}$[N]
  }
  { for { N = Range[10] } do
  : (WriteLine) Out; "Fib " N " = "
    {Y: proc {Rec} as {proc {N} with {Rec} as: if N == 0 then 0 else: if N == 1 then 1 else Rec[N - 2] + Rec[N - 1]}}$[N]
  }
}

```

Using less syntactic sugar:

```MANOOL

{ {extern "manool.org.18/std/0.3/all"} in
: let { Y = {proc {F} as {proc {X} as X[X]}[{proc {F; X} as F[{proc {X; Y} as X[X][Y]}.Bind[X]]}.Bind[F]]} } in
  { for { N = Range[10] } do
  : (WriteLine) Out; N "! = "
    {Y: proc {Rec} as {proc {Rec; N} as: if N == 0 then 1 else N * Rec[N - 1]}.Bind[Rec]}$[N]
  }
  { for { N = Range[10] } do
  : (WriteLine) Out; "Fib " N " = "
    {Y: proc {Rec} as {proc {Rec; N} as: if N == 0 then 0 else: if N == 1 then 1 else Rec[N - 2] + Rec[N - 1]}.Bind[Rec]}$[N]
  }
}

```

{{output}}

```txt

0! = 1
1! = 1
2! = 2
3! = 6
4! = 24
5! = 120
6! = 720
7! = 5040
8! = 40320
9! = 362880
Fib 0 = 0
Fib 1 = 1
Fib 2 = 1
Fib 3 = 2
Fib 4 = 3
Fib 5 = 5
Fib 6 = 8
Fib 7 = 13
Fib 8 = 21
Fib 9 = 34

```



## Maple


```Maple

> Y:=f->(x->x(x))(g->f((()->g(g)(args)))):
> Yfac:=Y(f->(x->`if`(x<2,1,x*f(x-1)))):
> seq( Yfac( i ), i = 1 .. 10 );
          1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800
> Yfib:=Y(f->(x->`if`(x<2,x,f(x-1)+f(x-2)))):
> seq( Yfib( i ), i = 1 .. 10 );
                    1, 1, 2, 3, 5, 8, 13, 21, 34, 55

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Y = Function[f, #[#] &[Function[g, f[g[g][##] &]]]];
factorial = Y[Function[f, If[# < 1, 1, # f[# - 1]] &]];
fibonacci = Y[Function[f, If[# < 2, #, f[# - 1] + f[# - 2]] &]];
```



## Moonscript


```Moonscript
Z = (f using nil) -> ((x) -> x x) (x) -> f (...) -> (x x) ...
factorial = Z (f using nil) -> (n) -> if n == 0 then 1 else n * f n - 1
```


=={{header|Objective-C}}==
{{works with|Mac OS X|10.6+}}{{works with|iOS|4.0+}}

```objc>#import <Foundation/Foundation.h


typedef int (^Func)(int);
typedef Func (^FuncFunc)(Func);
typedef Func (^RecursiveFunc)(id); // hide recursive typing behind dynamic typing

Func Y(FuncFunc f) {
  RecursiveFunc r =
  ^(id y) {
    RecursiveFunc w = y; // cast value back into desired type
    return f(^(int x) {
      return w(w)(x);
    });
  };
  return r(r);
}

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    Func fib = Y(^Func(Func f) {
      return ^(int n) {
        if (n <= 2) return 1;
        return  f(n - 1) + f(n - 2);
      };
    });
    Func fac = Y(^Func(Func f) {
      return ^(int n) {
        if (n <= 1) return 1;
        return n * f(n - 1);
      };
    });

    Func fib = fix(almost_fib);
    Func fac = fix(almost_fac);
    NSLog(@"fib(10) = %d", fib(10));
    NSLog(@"fac(10) = %d", fac(10));

  }
  return 0;
}
```


The usual version using recursion, disallowed by the task:

```objc
Func Y(FuncFunc f) {
  return ^(int x) {
    return f(Y(f))(x);
  };
}
```



## OCaml

The Y-combinator over functions may be written directly in OCaml provided rectypes are enabled:

```ocaml
let fix f g = (fun x a -> f (x x) a) (fun x a -> f (x x) a) g
```

Polymorphic variants are the simplest workaround in the absence of rectypes:

```ocaml
let fix f = (fun (`X x) -> f(x (`X x))) (`X(fun (`X x) y -> f(x (`X x)) y));;
```

Otherwise, an ordinary variant can be defined and used:

```ocaml
type 'a mu = Roll of ('a mu -> 'a);;

let unroll (Roll x) = x;;

let fix f = (fun x a -> f (unroll x x) a) (Roll (fun x a -> f (unroll x x) a));;

let fac f = function
    0 -> 1
  | n -> n * f (n-1)
;;

let fib f = function
    0 -> 0
  | 1 -> 1
  | n -> f (n-1) + f (n-2)
;;

(* val unroll : 'a mu -> 'a mu -> 'a = <fun>
val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
val fac : (int -> int) -> int -> int = <fun>
val fib : (int -> int) -> int -> int = <fun> *)

fix fac 5;;
(* - : int = 120 *)

fix fib 8;;
(* - : int = 21 *)
```


The usual version using recursion, disallowed by the task:

```ocaml
let rec fix f x = f (fix f) x;;
```



## Oforth

These combinators work for any number of parameters (see Ackermann usage)

With recursion into Y definition (so non stateless Y) :

```Oforth
: Y(f)   #[ f Y f perform ] ;
```


Without recursion into Y definition (stateless Y).

```Oforth
: X(me, f)   #[ me f me perform f perform ] ;
: Y(f)       #X f X ;
```


Usage :

```Oforth
: almost-fact(n, f)   n ifZero: [ 1 ] else: [ n n 1 - f perform * ] ;
#almost-fact Y => fact

: almost-fib(n, f)   n 1 <= ifTrue: [ n ] else: [ n 1 - f perform n 2 - f perform + ] ;
#almost-fib Y => fib

: almost-Ackermann(m, n, f)
   m 0 == ifTrue: [ n 1 + return ]
   n 0 == ifTrue: [ 1 m 1 - f perform return ]
   n 1 - m f perform m 1 - f perform ;
#almost-Ackermann Y => Ackermann
```



## Order


```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8y                                             \
ORDER_PP_FN(8fn(8F,                                                 \
            8let((8R, 8fn(8G,                                       \
                          8ap(8F, 8fn(8A, 8ap(8ap(8G, 8G), 8A))))), \
                 8ap(8R, 8R))))

#define ORDER_PP_DEF_8fac \
ORDER_PP_FN(8fn(8F, 8X,   \
                8if(8less_eq(8X, 0), 1, 8times(8X, 8ap(8F, 8minus(8X, 1))))))

#define ORDER_PP_DEF_8fib                                           \
ORDER_PP_FN(8fn(8F, 8X,                                             \
                8if(8less(8X, 2), 8X, 8plus(8ap(8F, 8minus(8X, 1)), \
                                            8ap(8F, 8minus(8X, 2))))))

ORDER_PP(8to_lit(8ap(8y(8fac), 10)))    // 3628800
ORDER_PP(8ap(8y(8fib), 10))             // 55
```



## Oz


```oz
declare
  Y = fun {$ F}
         {fun {$ X} {X X} end
          fun {$ X} {F fun {$ Z} {{X X} Z} end} end}
      end

  Fac = {Y fun {$ F}
              fun {$ N}
                 if N == 0 then 1 else N*{F N-1} end
              end
           end}

  Fib = {Y fun {$ F}
              fun {$ N}
                 case N of 0 then 0
                 [] 1 then 1
                 else {F N-1} + {F N-2}
                 end
              end
           end}
in
  {Show {Fac 5}}
  {Show {Fib 8}}
```



## PARI/GP

As of 2.8.0, GP cannot make general self-references in closures declared inline, so the Y combinator is required to implement these functions recursively in that environment, e.g., for use in parallel processing.

```parigp
Y(f)=x->f(f,x);
fact=Y((f,n)->if(n,n*f(f,n-1),1));
fib=Y((f,n)->if(n>1,f(f,n-1)+f(f,n-2),n));
apply(fact, [1..10])
apply(fib, [1..10])
```

{{out}}

```txt
%1 = [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
%2 = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```



## Perl


```perl
sub Y { my $f = shift;                                # λf.
    sub { my $x = shift; $x->($x) }->(                #   (λx.x x)
	sub {my $y = shift; $f->(sub {$y->($y)(@_)})} #   λy.f λz.y y z
    )
}
my $fac = sub {my $f = shift;
    sub {my $n = shift; $n < 2 ? 1 : $n * $f->($n-1)}
};
my $fib = sub {my $f = shift;
    sub {my $n = shift; $n == 0 ? 0 : $n == 1 ? 1 : $f->($n-1) + $f->($n-2)}
};
for my $f ($fac, $fib) {
    print join(' ', map Y($f)->($_), 0..9), "\n";
}
```

{{out}}

```txt
1 1 2 6 24 120 720 5040 40320 362880
0 1 1 2 3 5 8 13 21 34
```


The usual version using recursion, disallowed by the task:

```perl
sub Y { my $f = shift;
    sub {$f->(Y($f))->(@_)}
}
```



## Perl 6


```perl6
sub Y (&f) { sub (&x) { x(&x) }( sub (&y) { f(sub ($x) { y(&y)($x) }) } ) }
sub fac (&f) { sub ($n) { $n < 2 ?? 1 !! $n * f($n - 1) } }
sub fib (&f) { sub ($n) { $n < 2 ?? $n !! f($n - 1) + f($n - 2) } }
say map Y($_), ^10 for &fac, &fib;
```

{{out}}

```txt
(1 1 2 6 24 120 720 5040 40320 362880)
(0 1 1 2 3 5 8 13 21 34)
```


Note that Perl 6 doesn't actually need a Y combinator because you can name anonymous functions from the inside:


```perl6
say .(10) given sub (Int $x) { $x < 2 ?? 1 !! $x * &?ROUTINE($x - 1); }
```



## Phix

{{trans|C}}
After (over) simplifying things, the Y function has become a bit of a joke, but at least the recursion has been shifted out of fib/fac

Before saying anything too derogatory about Y(f)=f, it is clearly a fixed-point combinator, and I feel compelled to quote from the Mike Vanier link above:

"It doesn't matter whether you use cos or (lambda (x) (cos x)) as your cosine function; they will both do the same thing."

Anyone thinking they can do better may find some inspiration at
[[Currying#Phix|Currying]],
[[Closures/Value_capture#Phix|Closures/Value_capture]],
[[Partial_function_application#Phix|Partial_function_application]],
and/or [[Function_composition#Phix|Function_composition]]

```Phix
function call_fn(integer f, n)
    return call_func(f,{f,n})
end function

function Y(integer f)
    return f
end function

function fac(integer self, integer n)
    return iff(n>1?n*call_fn(self,n-1):1)
end function

function fib(integer self, integer n)
    return iff(n>1?call_fn(self,n-1)+call_fn(self,n-2):n)
end function

procedure test(string name, integer rid=routine_id(name))
    integer f = Y(rid)
    printf(1,"%s: ",{name})
    for i=1 to 10 do
        printf(1," %d",call_fn(f,i))
    end for
    printf(1,"\n");
end procedure
test("fac")
test("fib")
```

{{out}}

```txt

fac:  1 2 6 24 120 720 5040 40320 362880 3628800
fib:  1 1 2 3 5 8 13 21 34 55

```



## PHP

{{works with|PHP|5.3+}}

```php
<?php
function Y($f) {
  $g = function($w) use($f) {
    return $f(function() use($w) {
      return call_user_func_array($w($w), func_get_args());
    });
  };
  return $g($g);
}

$fibonacci = Y(function($f) {
  return function($i) use($f) { return ($i <= 1) ? $i : ($f($i-1) + $f($i-2)); };
});

echo $fibonacci(10), "\n";

$factorial = Y(function($f) {
  return function($i) use($f) { return ($i <= 1) ? 1 : ($f($i - 1) * $i); };
});

echo $factorial(10), "\n";
?>
```

The usual version using recursion, disallowed by the task:

```php
function Y($f) {
  return function() use($f) {
    return call_user_func_array($f(Y($f)), func_get_args());
  };
}
```


{{works with|PHP|pre-5.3 and 5.3+}}
with <tt>create_function</tt> instead of real closures. A little far-fetched, but...

```php
<?php
function Y($f) {
  $g = create_function('$w', '$f = '.var_export($f,true).';
    return $f(create_function(\'\', \'$w = \'.var_export($w,true).\';
      return call_user_func_array($w($w), func_get_args());
    \'));
  ');
  return $g($g);
}

function almost_fib($f) {
  return create_function('$i', '$f = '.var_export($f,true).';
    return ($i <= 1) ? $i : ($f($i-1) + $f($i-2));
  ');
};
$fibonacci = Y('almost_fib');
echo $fibonacci(10), "\n";

function almost_fac($f) {
  return create_function('$i', '$f = '.var_export($f,true).';
    return ($i <= 1) ? 1 : ($f($i - 1) * $i);
  ');
};
$factorial = Y('almost_fac');
echo $factorial(10), "\n";
?>
```


A functionally equivalent version using the <code>$this</code> parameter in closures is also possible:
{{works with|PHP|5.4+}}

```php
<?php
function pseudoY($f) {
    $g = function($w) use ($f) {
        return $f->bindTo(function() use ($w) {
            return call_user_func_array($w($w), func_get_args());
        });
    };
    return $g($g);
}

$factorial = pseudoY(function($n) {
    return $n > 1 ? $n * $this($n - 1) : 1;
});
echo $factorial(10), "\n";

$fibonacci = pseudoY(function($n) {
    return $n > 1 ? $this($n - 1) + $this($n - 2) : $n;
});
echo $fibonacci(10), "\n";
?>
```

However, <code>pseudoY()</code> is not a fixed-point combinator.


## PicoLisp

{{trans|Common Lisp}}

```PicoLisp
(de Y (F)
   (let X (curry (F) (Y) (F (curry (Y) @ (pass (Y Y)))))
      (X X) ) )
```


### Factorial


```PicoLisp
# Factorial
(de fact (F)
   (curry (F) (N)
      (if (=0 N)
         1
         (* N (F (dec N))) ) ) )

: ((Y fact) 6)
-> 720
```



### Fibonacci sequence


```PicoLisp
# Fibonacci
(de fibo (F)
   (curry (F) (N)
      (if (> 2 N)
         1
         (+ (F (dec N)) (F (- N 2))) ) ) )

: ((Y fibo) 22)
-> 28657
```



### Ackermann function


```PicoLisp
# Ackermann
(de ack (F)
   (curry (F) (X Y)
      (cond
         ((=0 X) (inc Y))
         ((=0 Y) (F (dec X) 1))
         (T (F (dec X) (F X (dec Y)))) ) ) )

: ((Y ack) 3 4)
-> 125
```



## Pop11


```pop11
define Y(f);
    procedure (x); x(x) endprocedure(
        procedure (y);
            f(procedure(z); (y(y))(z) endprocedure)
        endprocedure
    )
enddefine;

define fac(h);
    procedure (n);
       if n = 0 then 1 else n * h(n - 1) endif
    endprocedure
enddefine;

define fib(h);
    procedure (n);
        if n < 2 then 1 else h(n - 1) + h(n - 2) endif
    endprocedure
enddefine;

Y(fac)(5) =>
Y(fib)(5) =>
```

{{out}}

```txt

** 120
** 8

```



## PostScript

{{trans|Joy}}
{{libheader|initlib}}

```postscript
y {
    {dup cons} exch concat dup cons i
}.

/fac {
    { {pop zero?} {pop succ} {{dup pred} dip i *} ifte }
    y
}.
```



## PowerShell

{{trans|Python}}
PowerShell Doesn't have true closure, in order to fake it, the script-block is converted to text and inserted whole into the next function using variable expansion in double-quoted strings.
For simple translation of lambda calculus, <math>lambda</math> translates as param inside of a ScriptBlock, <math>(\ldots)</math> translates as Invoke-Expression "{}", invocation (written as a space) translates to InvokeReturnAsIs.
<math>\begin{array}{lcl}
fac & := & \lambda f.(\lambda n.\mbox{if }n\leq0\mbox{ then }1\mbox{ else }n*(f\ n-1)) \\
fib & := & \lambda f.(\lambda n. \mbox{if }n=0\mbox{ or }n=1\mbox{ then }1\mbox{ else }(f\ n-1)+(f\ n-2)) \\
Z & := & \lambda f.(\lambda x.f\ (\lambda y.x\ x\ y))\ (\lambda x.f\ (\lambda y.x\ x\ y)) \\
\end{array}</math>

```PowerShell
$fac = {
    	param([ScriptBlock] $f)
    	invoke-expression @"
    	{
    		param([int] `$n)
    		if (`$n -le 0) {1}
    		else {`$n * {$f}.InvokeReturnAsIs(`$n - 1)}
    	}
"@
    }

$fib = {
	param([ScriptBlock] $f)
	invoke-expression @"
	{
		param([int] `$n)
		switch (`$n)
        {
        0 {1}
        1 {1}
        default {{$f}.InvokeReturnAsIs(`$n-1)+{$f}.InvokeReturnAsIs(`$n-2)}
        }
	}
"@
}

$Z = {
    param([ScriptBlock] $f)
    invoke-expression @"
    {
        param([ScriptBlock] `$x)
        {$f}.InvokeReturnAsIs(`$(invoke-expression @`"
        {
            param(```$y)
            {`$x}.InvokeReturnAsIs({`$x}).InvokeReturnAsIs(```$y)
        }
`"@))
    }.InvokeReturnAsIs({
        param([ScriptBlock] `$x)
        {$f}.InvokeReturnAsIs(`$(invoke-expression @`"
        {
            param(```$y)
            {`$x}.InvokeReturnAsIs({`$x}).InvokeReturnAsIs(```$y)
        }
`"@))
    })
"@
}

$Z.InvokeReturnAsIs($fac).InvokeReturnAsIs(5)
$Z.InvokeReturnAsIs($fib).InvokeReturnAsIs(5)
```



GetNewClosure() was added in Powershell 2, allowing for an implementation without metaprogramming.  The following was tested with Powershell 4.


```PowerShell
$Y = {
    param ($f)

    {
        param ($x)

        $f.InvokeReturnAsIs({
            param ($y)

            $x.InvokeReturnAsIs($x).InvokeReturnAsIs($y)
        }.GetNewClosure())

    }.InvokeReturnAsIs({
        param ($x)

        $f.InvokeReturnAsIs({
            param ($y)

            $x.InvokeReturnAsIs($x).InvokeReturnAsIs($y)
        }.GetNewClosure())

    }.GetNewClosure())
}

$fact = {
    param ($f)

    {
        param ($n)

        if ($n -eq 0) { 1 } else { $n * $f.InvokeReturnAsIs($n - 1) }

    }.GetNewClosure()
}

$fib = {
    param ($f)

    {
        param ($n)

        if ($n -lt 2) { 1 } else { $f.InvokeReturnAsIs($n - 1) + $f.InvokeReturnAsIs($n - 2) }

    }.GetNewClosure()
}

$Y.invoke($fact).invoke(5)
$Y.invoke($fib).invoke(5)
```



## Prolog

Works with SWI-Prolog and module lambda, written by <b>Ulrich Neumerkel</b> found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl.

The code is inspired from this page : http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/ISO-Hiord#Hiord (p 106).

Original code is from <b>Hermenegildo</b> and al : <b>Hiord: A Type-Free Higher-Order Logic Programming Language with Predicate Abstraction</b>, pdf accessible here http://www.stups.uni-duesseldorf.de/asap/?id=129.

```Prolog
:- use_module(lambda).

% The Y combinator
y(P, Arg, R) :-
	Pred = P +\Nb2^F2^call(P,Nb2,F2,P),
	call(Pred, Arg, R).


test_y_combinator :-
    % code for Fibonacci function
    Fib   = \NFib^RFib^RFibr1^(NFib < 2 ->
			         RFib = NFib
			      ;
			         NFib1 is NFib - 1,
			         NFib2 is NFib - 2,
			         call(RFibr1,NFib1,RFib1,RFibr1),
			         call(RFibr1,NFib2,RFib2,RFibr1),
			         RFib is RFib1 + RFib2
			      ),

    y(Fib, 10, FR), format('Fib(~w) = ~w~n', [10, FR]),

    % code for Factorial function
    Fact =  \NFact^RFact^RFactr1^(NFact = 1 ->
			            RFact = NFact
                                 ;
			            NFact1 is NFact - 1,
			            call(RFactr1,NFact1,RFact1,RFactr1),
			            RFact is NFact * RFact1
			         ),

    y(Fact, 10, FF), format('Fact(~w) = ~w~n', [10, FF]).
```

{{out}}

```txt

 ?- test_y_combinator.
Fib(10) = 55
Fact(10) = 3628800
true.
```



## Python


```python>>>
 Y = lambda f: (lambda x: x(x))(lambda y: f(lambda *args: y(y)(*args)))
>>> fac = lambda f: lambda n: (1 if n<2 else n*f(n-1))
>>> [ Y(fac)(i) for i in range(10) ]
[1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
>>> fib = lambda f: lambda n: 0 if n == 0 else (1 if n == 1 else f(n-1) + f(n-2))
>>> [ Y(fib)(i) for i in range(10) ]
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```


The usual version using recursion, disallowed by the task:

```python
Y = lambda f: lambda *args: f(Y(f))(*args)
```



```python
Y = lambda b: ((lambda f: b(lambda *x: f(f)(*x)))((lambda f: b(lambda *x: f(f)(*x)))))
```



## R


```R
Y <- function(f) {
  (function(x) { (x)(x) })( function(y) { f( (function(a) {y(y)})(a) ) } )
}
```



```R
fac <- function(f) {
  function(n) {
    if (n<2)
      1
    else
      n*f(n-1)
  }
}

fib <- function(f) {
  function(n) {
    if (n <= 1)
      n
    else
      f(n-1) + f(n-2)
  }
}
```



```R
for(i in 1:9) print(Y(fac)(i))
for(i in 1:9) print(Y(fib)(i))
```



## Racket


The lazy implementation

```racket
#lang lazy

(define Y (λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))))

(define Fact
  (Y (λ (fact) (λ (n) (if (zero? n) 1 (* n (fact (- n 1))))))))
(define Fib
  (Y (λ (fib) (λ (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))))
```


{{out}}

```txt

> (!! (map Fact '(1 2 4 8 16)))
'(1 2 24 40320 20922789888000)
> (!! (map Fib '(1 2 4 8 16)))
'(0 1 2 13 610)

```


Strict realization:

```racket
#lang racket
(define Y (λ (b) ((λ (f) (b (λ (x) ((f f) x))))
                  (λ (f) (b (λ (x) ((f f) x)))))))
```


Definitions of <tt>Fact</tt> and <tt>Fib</tt> functions will be the same as in Lazy Racket.

Finally, a definition in Typed Racket is a little difficult as in other statically typed languages:

```racket
#lang typed/racket

(: make-recursive : (All (S T) ((S -> T) -> (S -> T)) -> (S -> T)))
(define-type Tau (All (S T) (Rec this (this -> (S -> T)))))
(define (make-recursive f)
  ((lambda: ([x : (Tau S T)]) (f (lambda (z) ((x x) z))))
   (lambda: ([x : (Tau S T)]) (f (lambda (z) ((x x) z))))))

(: fact : Number -> Number)
(define fact (make-recursive
              (lambda: ([fact : (Number -> Number)])
                (lambda: ([n : Number])
                  (if (zero? n)
                    1
                    (* n (fact (- n 1))))))))

(fact 5)
```



## REBOL


```rebol
Y: closure [g] [do func [f] [f :f] closure [f] [g func [x] [do f :f :x]]]
```

;usage example

```rebol
fact*: closure [h] [func [n] [either n <= 1 [1] [n * h n - 1]]]
fact: Y :fact*
```



## REXX

Programming note:   '''length''',   '''reverse''',   and   '''trunc'''   are REXX BIFs   ('''B'''uilt '''I'''n '''F'''unctions).

```rexx
/*REXX program implements and displays  a  stateless   Y   combinator.        */
numeric digits 1000                                      /*allow big numbers. */
say '    fib' Y(fib     (50))                            /*Fibonacci series.  */
say '    fib' Y(fib     (12 11 10 9 8 7 6 5 4 3 2 1 0))  /*Fibonacci series.  */
say '   fact' Y(fact    (60))                            /*single    factorial*/
say '   fact' Y(fact    (0 1 2 3 4 5 6 7 8 9 10 11))     /*single    factorial*/
say '  Dfact' Y(dfact   (4 5 6 7 8 9 10 11 12 13))       /*double    factorial*/
say '  Tfact' Y(tfact   (4 5 6 7 8 9 10 11 12 13))       /*triple    factorial*/
say '  Qfact' Y(qfact   (4 5 6 7 8 40))                  /*quadruple factorial*/
say ' length' Y(length  (when for to where whenceforth)) /*lengths   of words.*/
say 'reverse' Y(reverse (23 678 1007 45 MAS I MA))       /*reverses  strings. */
say '  trunc' Y(trunc   (-7.0005 12 3.14159 6.4 78.999)) /*truncates numbers. */
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
    Y: parse arg Y _; $=                                 /*the  Y  combinator.*/
        do j=1  for words(_); interpret '$=$' Y"("word(_,j)')'; end;    return $
  fib: procedure; parse arg x;  if x<2  then return x;   s=0;   a=0;    b=1
       s=0;  a=0;  b=1;             do j=2  to x; s=a+b; a=b; b=s; end; return s
dfact: procedure; parse arg x; !=1; do j=x  to 2  by -2; !=!*j; end;    return !
tfact: procedure; parse arg x; !=1; do j=x  to 2  by -3; !=!*j; end;    return !
qfact: procedure; parse arg x; !=1; do j=x  to 2  by -4; !=!*j; end;    return !
 fact: procedure; parse arg x; !=1; do j=2  to x       ; !=!*j; end;    return !
```

'''output'''

```txt

    fib  12586269025
    fib  144 89 55 34 21 13 8 5 3 2 1 1 0
   fact  8320987112741390144276341183223364380754172606361245952449277696409600000000000000
   fact  1 1 2 6 24 120 720 5040 40320 362880 3628800 39916800
  Dfact  8 15 48 105 384 945 3840 10395 46080 135135
  Tfact  4 10 18 28 80 162 280 880 1944 3640
  Qfact  4 5 12 21 32 3805072588800
 length  4 3 2 5 11
reverse  32 876 7001 54 SAM I AM
  trunc  -7 12 3 6 78

```



## Ruby

Using a lambda:


```ruby
y = lambda do |f|
  lambda {|g| g[g]}[lambda do |g|
      f[lambda {|*args| g[g][*args]}]
    end]
end

fac = lambda{|f| lambda{|n| n < 2 ? 1 : n * f[n-1]}}
p Array.new(10) {|i| y[fac][i]}   #=> [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

fib = lambda{|f| lambda{|n| n < 2 ? n : f[n-1] + f[n-2]}}
p Array.new(10) {|i| y[fib][i]}   #=> [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```


Same as the above, using the new short lambda syntax:
{{works with|Ruby|1.9}}

```ruby
y = ->(f) {->(g) {g.(g)}.(->(g) { f.(->(*args) {g.(g).(*args)})})}

fac = ->(f) { ->(n) { n < 2 ? 1 : n * f.(n-1) } }

p 10.times.map {|i| y.(fac).(i)}

fib = ->(f) { ->(n) { n < 2 ? n : f.(n-2) + f.(n-1) } }

p 10.times.map {|i| y.(fib).(i)}
```


Using a method:

{{works with|Ruby|1.9}}

```ruby
def y(&f)
  lambda do |g|
    f.call {|*args| g[g][*args]}
  end.tap {|g| break g[g]}
end

fac = y {|&f| lambda {|n| n < 2 ? 1 : n * f[n - 1]}}
fib = y {|&f| lambda {|n| n < 2 ? n : f[n - 1] + f[n - 2]}}

p Array.new(10) {|i| fac[i]}
# => [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
p Array.new(10) {|i| fib[i]}
# => [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```


The usual version using recursion, disallowed by the task:

```ruby
y = lambda do |f|
  lambda {|*args| f[y[f]][*args]}
end
```



## Rust


{{works with|Rust|1.35.0 stable}}

```rust
//! A simple implementation of the Y Combinator
// λf.(λx.xx)(λx.f(xx))
// <=> λf.(λx.f(xx))(λx.f(xx))

// CREDITS: A better version of the previous code that was posted here, with detailed explanation.
// See <y> and also <y_apply>.

// A function type that takes its own type as an input is an infinite recursive type.
// We introduce a trait that will allow us to have an input with the same type as self, and break the recursion.
// The input is going to be a trait object that implements the desired function in the interface.
// NOTE: We will be coercing a reference to a closure into this trait object.

trait Apply<T, R> {
  fn apply(
    &self,
    &Apply<T, R>,
    T
  ) -> R;
}

// In Rust, closures fall into three kinds: FnOnce, FnMut and Fn.
// FnOnce assumed to be able to be called just once if it is not Clone. It is impossible to
// write recursive FnOnce that is not Clone.
// All FnMut are also FnOnce, although you can call them multiple times, they are not allow to
// have a reference to themselves. So it is also not possible to write recursive FnMut closures
// that is not Clone.
// All Fn are also FnMut, and all closures of Fn are also Clone. However, programmers can create
// Fn objects that are not Clone

// This will work for all Fn objects, not just closures
// And it is a little bit more efficient for Fn closures as it do not clone itself.
impl<T, R, F> Apply<T, R> for F where F:
  Fn(&Apply<T, R>, T) -> R
{
  fn apply(
    &self,
    f: &Apply<T, R>,
    t: T
  ) -> R {
    self(f, t)

    // NOTE: Each letter is an individual symbol.
    // (λx.(λy.xxy))(λx.(λy.f(λz.xxz)y))t
    // => (λx.xx)(λx.f(xx))t
    // => (Yf)t
  }
}

// This works for all closures that is Clone, and those are Fn.
// impl<T, R, F> Apply<T, R> for F where F: FnOnce( &Apply<T, R>, T ) -> R + Clone {
//     fn apply( &self, f: &Apply<T, R>, t: T ) -> R {
//         (self.clone())( f, t )

//         // If we were to pass in self as f, we get -
//         // NOTE: Each letter is an individual symbol.
//         // λf.λt.sft
//         // => λs.λt.sst [s/f]
//         // => λs.ss
//     }
// }

// Before 1.26 we have some limitations and so we need some workarounds. But now impl Trait is stable and we can
// write the following:

fn y<T,R>(f:impl Fn(&Fn(T) -> R, T) -> R) -> impl Fn(T) -> R {
  move |t| (
    |x: &Apply<T,R>, y| x.apply(x, y)
  ) (
    &|x: &Apply<T,R>, y| f(
      &|z| x.apply(x,z),
      y
    ),
    t
  )
}

// fn y<T,R>(f:impl FnOnce(&Fn(T) -> R, T) -> R + Clone) -> impl FnOnce(T) -> R {
//    |t| (|x: &Apply<T,R>,y| x.apply(x,y))
//        (&move |x:&Apply<T,R>,y| f(&|z| x.apply(x,z), y), t)

//     // NOTE: Each letter is an individual symbol.
//     // (λx.(λy.xxy))(λx.(λy.f(λz.xxz)y))t
//     // => (λx.xx)(λx.f(xx))t
//     // => (Yf)t
// }

// Previous version removed as they are just hacks when impl Trait is not available.

fn fac(n: usize) -> usize {
  let almost_fac = |f: &Fn(usize) -> usize, x|
    if x == 0 {
      1
    } else {
      x * f(x - 1)
    }
  ;
  let fac = y( almost_fac );
  fac(n)
}

fn fib( n: usize ) -> usize {
  let almost_fib = |f: &Fn(usize) -> usize, x|
    if x < 2 {
      1
    } else {
      f(x - 2) + f(x - 1)
    };
  let fib = y(almost_fib);
  fib(n)
}

fn optimal_fib( n: usize ) -> usize {
  let almost_fib = |f: &Fn((usize,usize,usize)) -> usize, (i0,i1,x)|
    match x {
      0 => i0,
      1 => i1,
      x => f((i1,i0+i1, x-1))
    }
  ;
  let fib = |x| y(almost_fib)((1,1,x));
  fib(n)
}

fn main() {
  println!("{}", fac(10));
  println!("{}", fib(10));
  println!("{}", optimal_fib(10));
}
```

{{output}}

```txt
3628800
89
89
```



## Scala

Credit goes to the thread in [http://scala-blogs.org/2008/09/y-combinator-in-scala.html scala blog]

```scala
def Y[A,B](f: (A=>B)=>(A=>B)) = {
  case class W(wf: W=>A=>B) {
    def apply(w: W) = wf(w)
  }
  val g: W=>A=>B = w => f(w(w))(_)
  g(W(g))
}
```

Example

```scala
val fac = Y[Int, Int](f => i => if (i <= 0) 1 else f(i - 1) * i)
fac(6)  //> res0: Int = 720

val fib = Y[Int, Int](f => i => if (i < 2) i else f(i - 1) + f(i - 2))
fib(6)  //> res1: Int = 8
```



## Scheme


```scheme
(define Y                 ; (Y f) = (g g) where
  (lambda (f)             ;         (g g) = (f  (lambda a (apply (g g) a)))
    ((lambda (g) (g g))   ; (Y f) ==        (f  (lambda a (apply (Y f) a)))
     (lambda (g)
       (f  (lambda a (apply (g g) a)))))))

;; head-recursive factorial
(define fac                ; fac = (Y f) = (f      (lambda a (apply (Y f) a)))
  (Y (lambda (r)           ;     = (lambda (x) ... (r     (- x 1)) ... )
       (lambda (x)         ;        where   r    = (lambda a (apply (Y f) a))
         (if (< x 2)       ;               (r ... ) == ((Y f) ... )
             1             ;     == (lambda (x) ... (fac  (- x 1)) ... )
             (* x (r (- x 1))))))))

;; tail-recursive factorial
(define fac2
  (lambda (x)
    ((Y (lambda (r)        ;       (Y f) == (f     (lambda a (apply (Y f) a)))
          (lambda (x acc)  ;          r         == (lambda a (apply (Y f) a))
            (if (< x 2)    ;         (r ... )   == ((Y f) ... )
                acc
                (r (- x 1) (* x acc))))))
     x 1)))

; double-recursive Fibonacci
(define fib
  (Y (lambda (f)
       (lambda (x)
         (if (< x 2)
             x
             (+ (f (- x 1)) (f (- x 2))))))))

; tail-recursive Fibonacci
(define fib2
  (lambda (x)
    ((Y (lambda (f)
          (lambda (x a b)
            (if (< x 1)
                a
                (f (- x 1) b (+ a b))))))
     x 0 1)))

(display (fac 6))
(newline)

(display (fib2 134))
(newline)
```

{{out}}

```txt
720
4517090495650391871408712937
```


If we were allowed to use recursion (with <code>Y</code> referring to itself by name in its body) we could define the equivalent to the above as


```scheme
(define Yr        ; (Y f) == (f  (lambda a (apply (Y f) a)))
  (lambda (f)
    (f  (lambda a (apply (Yr f) a)))))
```


And another way is:

```scheme
(define Y2r
  (lambda (f)
    (lambda a (apply (f (Y2r f)) a))))
```


Which, non-recursively, is

```scheme
(define Y2                ; (Y2 f) = (g g) where
  (lambda (f)             ;          (g g) = (lambda a (apply (f (g g)) a))
    ((lambda (g) (g g))   ; (Y2 f) ==       (lambda a (apply (f (Y2 f)) a))
     (lambda (g)
       (lambda a (apply (f (g g)) a))))))
```



## Shen


```shen
(define y
  F -> ((/. X (X X))
        (/. X (F (/. Z ((X X) Z))))))

(let Fac (y (/. F N (if (= 0 N)
                      1
                      (* N (F (- N 1))))))
  (output "~A~%~A~%~A~%"
    (Fac 0)
    (Fac 5)
    (Fac 10)))
```

{{out}}

```txt
1
120
3628800
```



## Sidef


```ruby
var y = ->(f) {->(g) {g(g)}(->(g) { f(->(*args) {g(g)(args...)})})}

var fac = ->(f) { ->(n) { n < 2 ? 1 : (n * f(n-1)) } }
say 10.of { |i| y(fac)(i) }

var fib = ->(f) { ->(n) { n < 2 ? n : (f(n-2) + f(n-1)) } }
say 10.of { |i| y(fib)(i) }
```

{{out}}

```txt

[1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

```



## Slate

The Y combinator is already defined in slate as:

```slate
Method traits define: #Y &builder:
  [[| :f | [| :x | f applyWith: (x applyWith: x)]
	   applyWith: [| :x | f applyWith: (x applyWith: x)]]].
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
Y := [:f| [:x| x value: x] value: [:g| f value: [:x| (g value: g) value: x] ] ].

fib := Y value: [:f| [:i| i <= 1 ifTrue: [i] ifFalse: [(f value: i-1) + (f value: i-2)] ] ].

(fib value: 10) displayNl.

fact := Y value: [:f| [:i| i = 0 ifTrue: [1] ifFalse: [(f value: i-1) * i] ] ].

(fact value: 10) displayNl.
```

{{out}}

```txt
55
3628800
```


The usual version using recursion, disallowed by the task:

```smalltalk
Y := [:f| [:x| (f value: (Y value: f)) value: x] ].
```



## Standard ML


```sml
- datatype 'a mu = Roll of ('a mu -> 'a)
  fun unroll (Roll x) = x

  fun fix f = (fn x => fn a => f (unroll x x) a) (Roll (fn x => fn a => f (unroll x x) a))

  fun fac f 0 = 1
    | fac f n = n * f (n-1)

  fun fib f 0 = 0
    | fib f 1 = 1
    | fib f n = f (n-1) + f (n-2)
;
datatype 'a mu = Roll of 'a mu -> 'a
val unroll = fn : 'a mu -> 'a mu -> 'a
val fix = fn : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
val fac = fn : (int -> int) -> int -> int
val fib = fn : (int -> int) -> int -> int
- List.tabulate (10, fix fac);
val it = [1,1,2,6,24,120,720,5040,40320,362880] : int list
- List.tabulate (10, fix fib);
val it = [0,1,1,2,3,5,8,13,21,34] : int list
```


The usual version using recursion, disallowed by the task:

```sml
fun fix f x = f (fix f) x
```




## SuperCollider

Like Ruby, SuperCollider needs an extra level of lambda-abstraction to implement the y-combinator. The z-combinator is straightforward:

```SuperCollider
// z-combinator
(
z = { |f|
	{ |x| x.(x) }.(
		{ |y|
			f.({ |args| y.(y).(args) })
		}
	)
};
)

// the same in a shorter form

(
r = { |x| x.(x) };
z = { |f| r.({ |y| f.(r.(y).(_)) }) };
)


// factorial
k = { |f| { |x| if(x < 2, 1, { x * f.(x - 1) }) } };

g = z.(k);

g.(5) // 120

(1..10).collect(g) // [ 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800 ]



// fibonacci

k = { |f| { |x| if(x <= 2, 1, { f.(x - 1) + f.(x - 2) }) } };

g = z.(k);

g.(3)

(1..10).collect(g) // [ 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 ]



```



## Swift

Using a recursive type:

```swift>struct RecursiveFunc<F
 {
  let o : RecursiveFunc<F> -> F
}

func Y<A, B>(f: (A -> B) -> A -> B) -> A -> B {
  let r = RecursiveFunc<A -> B> { w in f { w.o(w)($0) } }
  return r.o(r)
}

let fac = Y { (f: Int -> Int) in
  { $0 <= 1 ? 1 : $0 * f($0-1) }
}
let fib = Y { (f: Int -> Int) in
  { $0 <= 2 ? 1 : f($0-1)+f($0-2) }
}
println("fac(5) = \(fac(5))")
println("fib(9) = \(fib(9))")
```

{{out}}

```txt

fac(5) = 120
fib(9) = 34

```


Without a recursive type, and instead using <code>Any</code> to erase the type:
{{works with|Swift|1.2+}} (for Swift 1.1 replace <code>as!</code> with <code>as</code>)

```swift
func Y<A, B>(f: (A -> B) -> A -> B) -> A -> B {
  typealias RecursiveFunc = Any -> A -> B
  let r : RecursiveFunc = { (z: Any) in let w = z as! RecursiveFunc; return f { w(w)($0) } }
  return r(r)
}
```


The usual version using recursion, disallowed by the task:

```swift
func Y<In, Out>( f: (In->Out) -> (In->Out) ) -> (In->Out) {
    return { x in f(Y(f))(x) }
}
```



## Tailspin


```tailspin

// YCombinator is not needed since tailspin supports recursion readily, but this demonstrates passing functions as parameters

templates combinator@{stepper:}
  templates makeStep@{rec:}
    $ -> stepper@{next: rec@{rec: rec}} !
  end makeStep
  $ -> makeStep@{rec: makeStep} !
end combinator

templates factorial
  templates seed@{next:}
    <0> 1 !
    <>
      $ * ($ - 1 -> next) !
  end seed
  $ -> combinator@{stepper: seed} !
end factorial

5 -> factorial -> 'factorial 5: $;
' -> !OUT::write

templates fibonacci
  templates seed@{next:}
    <..1> $ !
    <>
      ($ -2 -> next) + ($ - 1 -> next) !
  end seed
  $ -> combinator@{stepper: seed} !
end fibonacci

5 -> fibonacci -> 'fibonacci 5: $;
' -> !OUT::write

```

{{out}}

```txt

factorial 5: 120
fibonacci 5: 5

```



## Tcl

Y combinator is derived in great detail [http://wiki.tcl.tk/4833 here].


## TXR

This prints out 24, the factorial of 4:


```txrlisp
;; The Y combinator:
(defun y (f)
  [(op @1 @1)
   (op f (op [@@1 @@1]))])

;; The Y-combinator-based factorial:
(defun fac (f)
  (do if (zerop @1)
         1
         (* @1 [f (- @1 1)])))

;; Test:
(format t "~s\n" [[y fac] 4])
```


Both the <code>op</code> and <code>do</code> operators are a syntactic sugar for currying, in two different flavors. The forms within <code>do</code> that are symbols are evaluated in the normal Lisp-2 style and the first symbol can be an operator. Under <code>op</code>, any forms that are symbols are evaluated in the Lisp-2 style, and the first form is expected to evaluate to a function. The name <code>do</code> stems from the fact that the operator is used for currying over special forms like <code>if</code> in the above example, where there is evaluation control. Operators can have side effects: they can "do" something. Consider <code>(do set a @1)</code> which yields a function of one argument which assigns that argument to <code>a</code>.

The compounded <code>@@...</code> notation allows for inner functions to refer to outer parameters, when the notation is nested. Consider
```txrlisp
(op foo @1 (op bar @2 @@2))
```
. Here the <code>@2</code> refers to the second argument of the anonymous function denoted by the inner <code>op</code>. The <code>@@2</code> refers to the second argument of the outer <code>op</code>.


## Ursala

The standard y combinator doesn't work in Ursala due to eager
evaluation, but an alternative is easily defined as shown

```Ursala
(r "f") "x" = "f"("f","x")
my_fix "h"  = r ("f","x"). ("h" r "f") "x"
```

or by this shorter expression for the same thing in point free form.

```Ursala
my_fix = //~&R+ ^|H\~&+ ; //~&R
```

Normally you'd like to define a function recursively by writing
<math>f = h(f)</math>, where <math>h(f)</math> is just the body of the
function with recursive calls to <math>f</math> in it. With a fixed point
combinator such as <code>my_fix</code> as defined above, you do almost the same thing, except it's <math>f =</math><code>my_fix
"f".</code> <math>h</math><code>("f")</code>, where the dot represents lambda abstraction and the
quotes signify a dummy variable. Using this
method, the definition of the factorial function becomes

```Ursala
#import nat

fact = my_fix "f". ~&?\1! product^/~& "f"+ predecessor
```

To make it easier, the compiler has a directive to let you install
your own fixed point combinator for it to use, which looks like
this,

```Ursala
#fix my_fix
```

with your choice of function to be used in place of <code>my_fix</code>.
Having done that, you may express recursive functions per convention by circular definitions,
as in this example of a Fibonacci function.

```Ursala
fib = {0,1}?</1! sum+ fib~~+ predecessor^~/~& predecessor
```

Note that this way is only syntactic sugar for the for explicit way
shown above. Without a fixed point combinator given in the <code>#fix</code>
directive, this definition of fib
would ''not'' have compiled. (Ursala allows user defined fixed point
combinators because they're good for other things besides
functions.)
To confirm that all this works, here is a test program applying
both of the functions defined above to the numbers from 1 to 8.

```Ursala
#cast %nLW

examples = (fact* <1,2,3,4,5,6,7,8>,fib* <1,2,3,4,5,6,7,8>)
```

{{out}}

```txt

(
   <1,2,6,24,120,720,5040,40320>,
   <1,2,3,5,8,13,21,34>)
```

The fixed point combinator defined above is theoretically correct
but inefficient and limited to first order functions,
whereas the standard distribution includes a library (<code>sol</code>)
providing a hierarchy of fixed point combinators
suitable for production use and with higher order functions.
A more efficient alternative implementation of <code>my_fix</code>
would be <code>general_function_fixer 0</code>
(with 0 signifying the lowest order of fixed point combinators),
or if that's too easy, then by this definition.

```Ursala
#import sol

#fix general_function_fixer 1

my_fix "h" = "h" my_fix "h"
```

Note that this equation is solved using the next fixed point combinator in the hierarchy.


## VBA

{{trans|Phix}}
The IIf as translation of Iff can not be used as IIf executes both true and false parts and will cause a stack overflow.

```vb
Private Function call_fn(f As String, n As Long) As Long
    call_fn = Application.Run(f, f, n)
End Function

Private Function Y(f As String) As String
    Y = f
End Function

Private Function fac(self As String, n As Long) As Long
    If n > 1 Then
        fac = n * call_fn(self, n - 1)
    Else
        fac = 1
    End If
End Function

Private Function fib(self As String, n As Long) As Long
    If n > 1 Then
        fib = call_fn(self, n - 1) + call_fn(self, n - 2)
    Else
        fib = n
    End If
End Function

Private Sub test(name As String)
    Dim f As String: f = Y(name)
    Dim i As Long
    Debug.Print name
    For i = 1 To 10
        Debug.Print call_fn(f, i);
    Next i
    Debug.Print
End Sub

Public Sub main()
    test "fac"
    test "fib"
End Sub
```
{{out}}

```txt
fac
 1  2  6  24  120  720  5040  40320  362880  3628800
fib
 1  1  2  3  5  8  13  21  34  55
```


## Verbexx


```verbexx
/////// Y-combinator function (for single-argument lambdas) ///////

y @FN [f]
{ @( x -> { @f (z -> {@(@x x) z}) } )   // output of this expression is treated as a verb, due to outer @(  )
   ( x -> { @f (z -> {@(@x x) z}) } )   // this is the argument supplied to the above verb expression
};


/////// Function to generate an anonymous factorial function as the return value -- (not tail-recursive) ///////

fact_gen @FN [f]
{ n -> { (n<=0) ? {1} {n * (@f n-1)}
       }
};


/////// Function to generate an anonymous fibonacci function as the return value -- (not tail-recursive) ///////

fib_gen @FN [f]
{ n -> { (n<=0) ? { 0                                    }
                  { (n<=2) ? {1} { (@f n-1) + (@f n-2) } }
       }
};


/////// loops to test the above functions ///////

@VAR factorial = @y fact_gen;
@VAR fibonacci = @y fib_gen;

@LOOP init:{@VAR i = -1} while:(i <= 20) next:{i++}
{ @SAY  i "factorial =" (@factorial i) };

@LOOP init:{     i = -1} while:(i <= 16) next:{i++}
{ @SAY "fibonacci<" i "> =" (@fibonacci i) };
```



## Vim Script

There is no lambda in Vim (yet?), so here is a way to fake it using a Dictionary.  This also provides garbage collection.

```vim
" Translated from Python.  Works with: Vim 7.0

func! Lambx(sig, expr, dict)
    let fanon = {'d': a:dict}
    exec printf("
	\func fanon.f(%s) dict\n
	\  return %s\n
	\endfunc",
	\ a:sig, a:expr)
    return fanon
endfunc

func! Callx(fanon, arglist)
    return call(a:fanon.f, a:arglist, a:fanon.d)
endfunc

let g:Y = Lambx('f', 'Callx(Lambx("x", "Callx(a:x, [a:x])", {}), [Lambx("y", ''Callx(self.f, [Lambx("...", "Callx(Callx(self.y, [self.y]), a:000)", {"y": a:y})])'', {"f": a:f})])', {})

let g:fac = Lambx('f', 'Lambx("n", "a:n<2 ? 1 : a:n * Callx(self.f, [a:n-1])", {"f": a:f})', {})

echo Callx(Callx(g:Y, [g:fac]), [5])
echo map(range(10), 'Callx(Callx(Y, [fac]), [v:val])')

```

Update: since Vim 7.4.2044 (or so...), the following can be used (the feature check was added with 7.4.2121):

```vim

if !has("lambda")
    echoerr 'Lambda feature required'
    finish
endif
let Y = {f -> {x -> x(x)}({y -> f({... -> call(y(y), a:000)})})}
let Fac = {f -> {n -> n<2 ? 1 : n * f(n-1)}}

echo Y(Fac)(5)
echo map(range(10), 'Y(Fac)(v:val)')

```

Output:

```txt
120
[1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
```



## Wart


```python
# Better names due to Jim Weirich: http://vimeo.com/45140590
def (Y improver)
  ((fn(gen) gen.gen)
   (fn(gen)
     (fn(n)
       ((improver gen.gen) n))))

factorial <- (Y (fn(f)
                  (fn(n)
                    (if zero?.n
                      1
                      (n * (f n-1))))))

prn factorial.5
```


{{omit from|ACL2}}
{{omit from|Ada}}
{{omit from|PureBasic}}
{{omit from|TI-89 BASIC}} <!-- no lambdas, no first-class functions except by name string -->


## XQuery


Version 3.0 of the [http://www.w3.org/TR/xpath-30/ XPath] and [http://www.w3.org/TR/xquery-30/ XQuery] specifications added support for function items.


```XQuery
let $Y := function($f) {
    (function($x) { ($x)($x) })( function($g) { $f( (function($a) { $g($g) ($a)})  ) } )
  }
let $fac := $Y(function($f) { function($n) { if($n <  2) then 1  else $n * $f($n - 1) } })
let $fib := $Y(function($f) { function($n) { if($n <= 1) then $n else $f($n - 1) + $f($n - 2) } })
return (
    $fac(6),
    $fib(6)
)

```

{{out}}

```XQuery>720 8</lang



## Yabasic


```Yabasic
sub fac(self$, n)
    if n > 1 then
        return n * execute(self$, self$, n - 1)
    else
        return 1
    end if
end sub

sub fib(self$, n)
    if n > 1 then
        return execute(self$, self$, n - 1) + execute(self$, self$, n - 2)
    else
        return n
    end if
end sub

sub test(name$)
    local i

    print name$, ": ";
    for i = 1 to 10
        print execute(name$, name$, i);
    next
    print
end sub

test("fac")
test("fib")
```



## zkl


```zkl
fcn Y(f){ fcn(g){ g(g) }( 'wrap(h){ f( 'wrap(a){ h(h)(a) }) }) }
```

Functions don't get to look outside of their scope so data in enclosing scopes needs to be bound to a function, the fp (function application/cheap currying) method does this. 'wrap is syntactic sugar for fp.

```zkl
fcn almost_factorial(f){ fcn(n,f){ if(n<=1) 1 else n*f(n-1) }.fp1(f) }
Y(almost_factorial)(6).println();
[0..10].apply(Y(almost_factorial)).println();
```

{{out}}

```txt

720
L(1,1,2,6,24,120,720,5040,40320,362880,3628800)

```


```zkl
fcn almost_fib(f){ fcn(n,f){ if(n<2) 1 else f(n-1)+f(n-2) }.fp1(f) }
Y(almost_fib)(9).println();
[0..10].apply(Y(almost_fib)).println();
```

{{out}}

```txt

55
L(1,1,2,3,5,8,13,21,34,55,89)

```

