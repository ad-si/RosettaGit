+++
title = "Repeat"
description = ""
date = 2019-10-12T09:26:18Z
aliases = []
[extra]
id = 17634
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Task:
Write a procedure which accepts as arguments another procedure and a positive integer. 

The latter procedure is executed a number of times equal to the accepted integer.





## Ada



```Ada
with Ada.Text_IO;

procedure Repeat_Example is
   
   procedure Repeat(P: access Procedure; Reps: Natural) is
   begin
      for I in 1 .. Reps loop
	 P.all; -- P points to a procedure, and P.all actually calls that procedure
      end loop;
   end Repeat;
   
   procedure Hello is
   begin
      Ada.Text_IO.Put("Hello! ");
   end Hello;
   
begin
   Repeat(Hello'Access, 3); -- Hello'Access points to the procedure Hello
end Repeat_Example;
```


Output:

```txt
Hello! Hello! Hello! 
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68

# operator that executes a procedure the specified number of times            #
OP REPEAT = ( INT count, PROC VOID routine )VOID:
    TO count DO routine OD;

# make REPEAT a low priority operater                                         #
PRIO REPEAT = 1;


# can also create variant that passes the iteration count as a parameter      #
OP REPEAT = ( INT count, PROC( INT )VOID routine )VOID:
    FOR iteration TO count DO routine( iteration ) OD;

main: (

    # PROC to test the REPEAT operator with                                   #
    PROC say something = VOID: print( ( "something", newline ) );

    3 REPEAT say something;

    # PROC to test the variant                                                #
    PROC show squares = ( INT n )VOID: print( ( n, n * n, newline ) );

    3 REPEAT show squares

)

```

Output:

```txt

something
something
something
         +1         +1
         +2         +4
         +3         +9

```



## Applesoft BASIC

http://hoop-la.ca/apple2/2016/winterwarmup/#repeat.bas


## Arturo



```arturo
proc {
	print "I'm in proc!"
}

repeat [func,times]{
	loop $(range 0 times-1) {
		func
	}
}

repeat proc 3
```


{{out}}


```txt
I'm in proc!
I'm in proc!
I'm in proc!
```



## AutoHotkey


```AutoHotkey
repeat("fMsgBox",3)
return

repeat(f, n){
	loop % n
		%f%()
}

fMsgBox(){
	MsgBox hello
}
```



## AWK


```AWK

# syntax: GAWK -f REPEAT.AWK
BEGIN {
    for (i=0; i<=3; i++) {
      f = (i % 2 == 0) ? "even" : "odd"
      @f(i) # indirect function call
    }
    exit(0)
}
function even(n,  i) {
    for (i=1; i<=n; i++) {
      printf("inside even %d\n",n)
    }
}
function odd(n,  i) {
    for (i=1; i<=n; i++) {
      printf("inside odd %d\n",n)
    }
}

```

<p>output:</p>

```txt

inside odd 1
inside even 2
inside even 2
inside odd 3
inside odd 3
inside odd 3

```


## Batch File


```dos

@echo off

:_main
setlocal
call:_func1 _func2 3
pause>nul
exit/b

:_func1
setlocal enabledelayedexpansion
for /l %%i in (1,1,%2) do call:%1
exit /b

:_func2
setlocal
echo _func2 has been executed
exit /b

```



## C


```c>#include <stdio.h


void repeat(void (*f)(void), unsigned int n) {
 while (n-->0)
  (*f)(); //or just f()
}

void example() {
 printf("Example\n");
}

int main(int argc, char *argv[]) {
 repeat(example, 4);
 return 0;
}

```



## C++


```cpp>template <typename Function

void repeat(Function f, unsigned int n) {
 for(unsigned int i=n; 0<i; i--)
  f();
}
```

usage:

```cpp>#include <iostream

void example() {
 std::cout << "Example\n";
}

repeat(example, 4);
```

{{works with|C++11}}

```cpp
 repeat([]{std::cout << "Example\n";}, 4);
```

=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;

namespace Repeat {
    class Program {
        static void Repeat(int count, Action<int> fn) {
            if (null == fn) {
                throw new ArgumentNullException("fn");
            }
            for (int i = 0; i < count; i++) {
                fn.Invoke(i + 1);
            }
        }

        static void Main(string[] args) {
            Repeat(3, x => Console.WriteLine("Example {0}", x));
        }
    }
}
```

{{out}}

```txt
Example 1
Example 2
Example 3
```



## Clojure


```clojure
(defn repeat-function [f n] 
  (dotimes [i n] (f)))
```


{{out}}

```txt
user=> (repeat-function #(println "bork") 3)
bork
bork
bork
```



## Common Lisp


```lisp
(defun repeat (f n)
  (dotimes (i n) (funcall f)))

(repeat (lambda () (format T "Example~%")) 5)
```



## D


```d
void repeat(void function() fun, in uint times) {
    foreach (immutable _; 0 .. times)
        fun();
}

void procedure() {
    import std.stdio;
    "Example".writeln;
}

void main() {
    repeat(&procedure, 3);
}
```

{{out}}

```txt
Example
Example
Example
```



## EchoLisp


```lisp

(define (repeat f n) (for ((i n)) (f)))

(repeat (lambda () (write (random 1000))) 5)
    → 287 798 930 989 794 

;; Remark
;; It is also possible to iterate a function : f(f(f(f( ..(f x)))))
(define cos10 (iterate cos 10)
(define cos100 (iterate cos10 10))
(cos100 0.6)
    →  0.7390851332151605
(cos 0.7390851332151605)
    → 0.7390851332151608 ;; fixed point found

```


=={{header|F#|F sharp}}==

```fsharp
open System

let Repeat c f =
    for _ in 1 .. c do
        f()

let Hello _ = 
    printfn "Hello world"

[<EntryPoint>]
let main _ =
    Repeat 3 Hello

    0 // return an integer exit code
```



## Factor

Factor comes with the <tt>times</tt> word which does exactly this. For example, 

```factor

3 [ "Hello!" print ] times

```

{{out}}

```txt

Hello!
Hello!
Hello!

```


The implementation of <tt>times</tt>:

```factor

: times ( ... n quot: ( ... -- ... ) -- ... )
    [ drop ] prepose each-integer ; inline

```



## Forth


```forth
: times ( xt n -- )
  0 ?do dup execute loop drop ;
```


Or, taking care to keep the data stack clean for the XT's use, as is often desired:


```forth
: times { xt n -- }
  n 0 ?do xt execute loop ;
```


Or as a novel control structure, which is not demanded by this task but which is just as idiomatic in Forth as the XT-consuming alternatives above:


```forth
: times[  ]] 0 ?do [[ ; immediate compile-only
: ]times  postpone loop ;  immediate compile-only
```


Usage:


```forth
[: cr ." Hello" ;] 3 times

: 3-byes ( -- )  3 times[ cr ." Bye" ]times ;
3-byes
```


{{out}}
```txt
Hello
Hello
Hello
Bye
Bye
Bye
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub proc()
  Print " proc called"
End Sub

Sub repeat(s As Sub, n As UInteger)
  For i As Integer = 1 To n
    Print Using "##"; i;
    s()
  Next
End Sub

repeat(@proc, 5)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 1 proc called
 2 proc called
 3 proc called
 4 proc called
 5 proc called

```



## Go


```go
package main

import "fmt"

func repeat(n int, f func()) {
  for i := 0; i < n; i++ {
    f()
  }
}

func fn() {
  fmt.Println("Example")
}

func main() {
  repeat(4, fn)
}
```



## Haskell

Such a function already exists

```Haskell
import Control.Monad (replicateM_)

sampleFunction :: IO ()
sampleFunction = putStrLn "a"

main = replicateM_ 5 sampleFunction
```


And if the requirement is for something like a Church numeral, compounding the application of a given function '''n''' times (rather than repeating the same IO event '''n''' times) then we could also write something like '''applyN''' below: 

```Haskell
applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

main :: IO ()
main = print $ applyN 10 (\x -> 2 * x) 1
```

{{Out}}

```txt
1024
```



## J


```J

   NB. ^: (J's power conjunction) repeatedly evaluates a verb.

   NB. Appending to a vector the sum of the most recent
   NB. 2 items can generate the Fibonacci sequence.

   (, [: +/ _2&{.)  (^:4)  0 1
0 1 1 2 3 5
   

   NB. Repeat an infinite number of times
   NB. computes the stable point at convergence

   cosine =: 2&o.

   cosine (^:_ ) 2    NB. 2 is the initial value
0.739085
   
   cosine 0.739085  NB. demonstrate the stable point x==Cos(x)
0.739085
   

   cosine^:(<_) 2  NB. show the convergence
2 _0.416147 0.914653 0.610065 0.819611 0.682506 0.775995 0.713725 0.755929 0.727635 0.74675 0.733901 0.742568 0.736735 0.740666 0.738019 0.739803 0.738602 0.739411 0.738866 0.739233 0.738986 0.739152 0.73904 0.739116 0.739065 0.739099 0.739076 0.739091 0.7...


   # cosine^:(<_) 2  NB. iteration tallyft
78

   f =: 3 :'smoutput ''hi'''

   f''
hi
   
   NB. pass verbs via a gerund
   repeat =: dyad def 'for_i. i.y do. (x`:0)0 end. EMPTY'

   (f`'')repeat 4
hi
hi
hi
hi
   
   

   NB. pass a verb directly to an adverb

   Repeat =: adverb def 'for_i. i.y do. u 0 end. EMPTY'

   f Repeat 4
hi
hi
hi
hi

```



## Java

{{works with|Java|8}}

```java
import java.util.function.Consumer;
import java.util.stream.IntStream;

public class Repeat {

    public static void main(String[] args) {
        repeat(3, (x) -> System.out.println("Example " + x));
    }

    static void repeat (int n, Consumer<Integer> fun) {
        IntStream.range(0, n).forEach(i -> fun.accept(i + 1));
    }
}
```


Output:


```txt
Example 1
Example 2
Example 3
```


## jq

{{works with|jq|1.4}}

We first define "repeat" naively but in accordance with the task
specification; we then define an optimized version that illustrates
a general technique for taking advantage of jq's support for
tail-call optimization (TCO).

Since jq is a purely functional language, repeat(f; n) is unlikely
to be very useful so we define a similar filter, repeatedly(f; n), which
generates n+1 terms: . (the input), f, f|f, ... ; that is, using
conventional functional notation, it generates: x, f(x), f(f(x)), ...

'''Unoptimized version''':

```jq
def unoptimized_repeat(f; n):
  if n <= 0 then empty
  else f, repeat(f; n-1)
  end;
```


'''Optimized for TCO''':

```jq
def repeat(f; n):
  # state: [count, in]
  def r:
    if .[0] >= n then empty else (.[1] | f), (.[0] += 1 | r) end;
  [0, .] | r;
```

'''Variant''':

```jq
# If n is a non-negative integer,
# then emit a stream of (n + 1) terms: ., f, f|f, f|f|f, ...
def repeatedly(f; n):
  # state: [count, in]
  def r:
    if .[0] < 0 then empty
    else .[1], ([.[0] - 1, (.[1] | f)] | r)
    end;
  [n, .] | r;
```


'''Examples''':

```jq
0 | [ repeat(.+1; 3) ]
```

produces:
[1,1,1]

```jq
0 | repeatedly(.+1; 3)
```

produces:
 0 
 1 
 2
 3


## Julia


```julia
function sayHi()
	println("Hi")
end

function rep(f, n)
	for i = 1:n f() end
end

rep(sayHi, 3)
```

{{out}}

```txt
Hi
Hi
Hi
```



## Kotlin


```scala
// version 1.0.6

fun repeat(n: Int, f: () -> Unit) {
    for (i in 1..n) {
        f()
        println(i)
    }
}

fun main(args: Array<String>) {
    repeat(5) { print("Example ") }
}
```


{{out}}

```txt

Example 1
Example 2
Example 3
Example 4
Example 5

```



## LiveCode


```LiveCode
rep "answer",3

command rep x,n
    repeat n times
        do merge("[[x]] [[n]]")
    end repeat
end rep
```



## Lua

No particular magic required as Lua allows functions to be passed as arguments.

```Lua
function myFunc ()
    print("Sure looks like a function in here...")
end

function rep (func, times)
    for count = 1, times do
        func()
    end
end

rep(myFunc, 4)

```

{{out}}

```txt
Sure looks like a function in here...
Sure looks like a function in here...
Sure looks like a function in here...
Sure looks like a function in here...
```



## Mathematica

Note that anything of this form is not considered good practice.

```Mathematica
repeat[f_, n_] := Do[f[], {n}];
repeat[Print["Hello, world!"] &, 5];
```

{{out}}

```txt
Hello, world!
Hello, world!
Hello, world!
Hello, world!
Hello, world!
```


=={{header|МК-61/52}}==

<lang>1	П4

3	^	1	6	ПП	09	С/П

П7	<->	П0	КПП7	L0	12	В/О

ИП4	С/П	КИП4	В/О
```



## MiniScript



```MiniScript
sayHi = function()
    print "Hi!"
end function

rep = function(f, n)
    for i in range(1, n)
        f
    end for
end function

rep @sayHi, 3
```

{{out}}

```txt
Hi!
Hi!
Hi!
```


=={{header|Modula-2}}==

```modula2
MODULE Repeat;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE F = PROCEDURE;

PROCEDURE Repeat(fun : F; c : INTEGER);
VAR i : INTEGER;
BEGIN
    FOR i:=1 TO c DO
        fun
    END
END Repeat;

PROCEDURE Print;
BEGIN
    WriteString("Hello");
    WriteLn
END Print;

BEGIN
    Repeat(Print, 3);

    ReadChar
END Repeat.
```



## Objeck



```objeck
class Repeat {
  function : Main(args : String[]) ~ Nil {
    Repeat(Example() ~ Nil, 3);
  }
  
  function : Repeat(e : () ~ Nil, i : Int) ~ Nil {
    while(i-- > 0) {
      e();
    };
  }
  
  function : Example() ~ Nil {
    "Example"->PrintLine();
  }
}
```



## OCaml



```ocaml
let repeat ~f ~n =
  for i = 1 to n do
    f ()
  done

let func () =
  print_endline "Example"

let () =
  repeat ~n:4 ~f:func

```



## Oforth


This method is already defined : times. This method can be used on all runnables (functions, methods, blocks, ...).


```Oforth
: hello "Hello, World!" println ;
10 #hello times
```


{{out}}

```txt

Hello, World!
Hello, World!
Hello, World!
Hello, World!
Hello, World!
Hello, World!
Hello, World!
Hello, World!
Hello, World!
Hello, World!

```



## Ol


```scheme

; sample function
(define (function) (display "+"))

; simple case for 80 times
(for-each (lambda (unused) (function)) (iota 80))
(print) ; print newline
; ==> ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; detailed case for 80 times
(let loop ((fnc function) (n 80))
   (unless (zero? n)
      (begin
         (fnc)
         (loop fnc (- n 1)))))
(print) ; print newline
; ==> ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

```



## PARI/GP


```parigp
repeat(f, n)=for(i=1,n,f());
repeat( ()->print("Hi!"), 2);
```

{{out}}

```txt
Hi!
Hi!
```



## Perl


{{trans|C}}


```perl
sub repeat {
    my ($sub, $n) = @_;
    $sub->() for 1..$n;
}

sub example {
    print "Example\n";
}

repeat(\&example, 4);
```



## Perl 6



```perl6
sub repeat (&f, $n) { f() xx $n };

sub example { say rand }

repeat(&example, 3);
```


{{Output}}

```txt

0.435249779778396
0.647701200726486
0.279289335968417

```


Of course, we could have just written <pre style="display:inline; padding:0.3em">example() xx 3;
```
 or even <pre style="display:inline; padding:0.3em">(say rand) xx 3;
```
 directly &ndash; the custom <code>repeat</code> subroutine is just here to satisfy the task description.

Notes on the [http://doc.perl6.org/language/operators#infix_xx <code>xx</code>] operator:

* Unlike other operators, it evaluates its left-hand-side argument lazily - that's why we can simply call <code>f()</code> there rather than passing it as a function object.
* The operator has a return value: A list consisting of the return values of the left-hand-side ''(and building lists is in fact what <code>xx</code> is usually used for)''.

General notes:
* The <code>&</code> sigil in the <code>repeat</code> subroutine signature restricts that parameter to types that implement the <code>Callable</code> role, and makes it available inside the <code>repeat</code> subroutine body as if it were a lexically scoped sub.
* The parentheses in the last line are necessary to disambiguate it as a call to our custom subroutine, rather than an attempt to use the built-in <code>repeat { ... } while ...</code> construct.


## Phix


```Phix
procedure Repeat(integer rid, integer n)
    for i=1 to n do
        call_proc(rid,{})
    end for
end procedure

procedure Hello()
    ?"Hello"
end procedure

Repeat(routine_id("Hello"),5)
```



## PicoLisp



```PicoLisp
# The built-in function "do" can be used to achieve our goal,
# however, it has a slightly different syntax than what the
# problem specifies.

# Native solution.
(do 10 (version))

# Our solution.
(de dofn (Fn N)
   (do N (Fn)) )

(dofn version 10)
```



## PowerShell

{{trans|Python}} (Made more PowerShelly.)

```PowerShell

function Out-Example
{
    "Example"
}

function Step-Function ([string]$Function, [int]$Repeat)
{
    for ($i = 1; $i -le $Repeat; $i++)
    { 
        "$(Invoke-Expression -Command $Function) $i"
    }
}

Step-Function Out-Example -Repeat 3

```

{{Out}}

```txt

Example 1
Example 2
Example 3

```



## Prolog



```prolog
repeat(_, 0).
repeat(Callable, Times) :-
	succ(TimesLess1, Times),
	Callable,
	repeat(Callable, TimesLess1).

test :- write('Hello, World'), nl.	
test(Name) :- format('Hello, ~w~n', Name).
```

{{out}}

```txt

?- repeat(test, 3).
Hello, World
Hello, World
Hello, World
true ;
false.

?- repeat(test('Fred'), 3).
Hello, Fred
Hello, Fred
Hello, Fred
true ;
false.

```



## Python



### Procedural


```Python
#!/usr/bin/python
def repeat(f,n):
  for i in range(n):
    f();

def procedure():
  print("Example");

repeat(procedure,3); #prints "Example" (without quotes) three times, separated by newlines.
```



### Functional

Repeated function application:
{{Works with|Python|3.7}}

```python
'''Application of a given function, repeated N times'''

from itertools import repeat
from functools import reduce
from inspect import getsource


# applyN :: Int -> (a -> a) -> a -> a
def applyN(n):
    '''n compounding applications of the supplied
       function f. Equivalent to Church numeral n.
    '''
    def go(f):
        return lambda x: reduce(
            lambda a, g: g(a), repeat(f, n), x
        )
    return lambda f: go(f)


# MAIN ----------------------------------------------------
def main():
    '''Tests - compounding repetition
       of function application.
    '''
    def f(x):
        return x + 'Example\n'

    def g(x):
        return 2 * x

    def h(x):
        return 1.05 * x

    print(
        fTable(__doc__ + ':')(
            lambda fx: '\nRepeated * 3:\n (' + (
                getsource(fst(fx)).strip() + ')(' +
                repr(snd(fx)) + ')'
            )
        )(str)(
            liftA2(applyN(3))(fst)(snd)
        )([(f, '\n'), (g, 1), (h, 100)])
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# liftA2 :: (a0 -> b -> c) -> (a -> a0) -> (a -> b) -> a -> c
def liftA2(op):
    '''Lift a binary function to a composition
       over two other functions.
       liftA2 (*) (+ 2) (+ 3) 7 == 90
    '''
    def go(f, g):
        return lambda x: op(
            f(x)
        )(g(x))
    return lambda f: lambda g: go(f, g)


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Application of a given function, repeated N times:

Repeated * 3:
 (def f(x):
        return x + 'Example\n')('\n') -> 
Example
Example
Example

             
Repeated * 3:
 (def g(x):
        return 2 * x)(1) -> 8
        
Repeated * 3:
 (def h(x):
        return 1.05 * x)(100) -> 115.7625
```



## R


```R

f1 <- function(...){print("coucou")}

f2 <-function(f,n){
lapply(seq_len(n),eval(f))
}

f2(f1,4)

```



## Racket

The racket guide has a section called [http://docs.racket-lang.org/guide/for.html?q=iterators "Iterators and Comprehensions"], which shows that ''for'' isn't just for repeating n times!


```Racket
#lang racket/base
(define (repeat f n) ; the for loop is idiomatic of (although not exclusive to) racket
  (for ((_ n)) (f)))

(define (repeat2 f n) ; This is a bit more "functional programmingy"
  (when (positive? n) (f) (repeat2 f (sub1 n))))

(display "...")
(repeat (λ () (display " and over")) 5)
(display "...")
(repeat2 (λ () (display " & over")) 5)
(newline)
```

{{out}}

```txt
... and over and over and over and over and over... & over & over & over & over & over
```



## REXX

The procedure name (that is being repeatedly executed) isn't restricted to an   ''internal''   REXX subroutine (procedure),

it may be an   ''external''   program (procedure) written in any language.

```rexx
/*REXX program   executes   a  named procedure  a specified number of times.            */
parse arg pN # .                                 /*obtain optional arguments from the CL*/
if #=='' | #==","   then #=1                     /*assume  once  if not specified.      */
if pN\==''          then call repeats pN, #      /*invoke the REPEATS procedure for  pN.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
repeats: procedure;  parse arg x,n               /*obtain the procedureName & # of times*/
                do n;  interpret 'CALL' x;  end  /*repeat the invocation    N    times. */
         return                                  /*return to invoker of the REPEATS proc*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
yabba:   say 'Yabba, yabba do!';          return /*simple code;  no need for  PROCEDURE.*/
```

'''output''' when the input is:   <tt> yabba 4 </tt>

```txt

Yabba, yabba do!
Yabba, yabba do!
Yabba, yabba do!
Yabba, yabba do!

```

'''output''' when the input is:   <tt> $date 3 </tt>

[The (external)   $DATE.REX   program isn't supplied here.]

```txt

day-of-year= 159                Gregorian date= 06/08/2014               Sunday
day-of-year= 159                Gregorian date= 06/08/2014               Sunday
day-of-year= 159                Gregorian date= 06/08/2014               Sunday

```



## Ring


```ring

Func Main
     times(5,:test)

Func Test
     see "Message from the test function!" + nl

Func Times nCount, F
     for x = 1 to nCount
         Call F()
     next

```



## Ruby


```ruby
4.times{ puts "Example" }  # idiomatic way

def repeat(proc,num)
  num.times{ proc.call }
end

repeat(->{ puts "Example" }, 4)
```



## Rust


```rust
// Repeat the function f, n times.
fn repeat<F>(f: &F, n: u32)
    where F: Fn() {
    for _ in 0..n {
        f();
    }
}

fn static_fn() {
    print!("Static ");
}

fn main() {
    // Repeat a static function.
    repeat(&static_fn, 4);

    println!("");

    // Repeat an anonymous closure.
    repeat(&|| print!("Closure "), 5);
}
```

{{out}}

```txt
Static Static Static Static
Closure Closure Closure Closure Closure
```



## Scala


### Intuitive solution

# Call by name
# Type parameterization
# Higher order function

```scala
  def repeat[A](n:Int)(f: => A)= ( 0 until n).foreach(_ => f)

  repeat(3) { println("Example") }
```

===Advanced Scala-ish ===
# Call by name
# Type parameterization
# Implicit method
# Tail recursion
# Infix notation

```scala
object Repeat2 extends App {
  
   implicit class IntWithTimes(x: Int) {
      def times[A](f: => A):Unit = {
    @tailrec
      def loop( current: Int): Unit =
        if (current > 0) {
          f
          loop(current - 1)
        }
      loop(x)
    }
  }

  5 times println("ha") // Not recommended infix for 5.times(println("ha")) aka dot notation
}
```


===Most Scala-ish ===
# Call by name
# Type parameterization
# Implicit method
# Tail recursion
# Infix notation
# Operator overloading

```scala
import scala.annotation.tailrec

object Repeat3 extends App {

  implicit class UnitWithNtimes(f: => Unit) {
    def *[A](n: Int): Unit = { // Symbol * used instead of literal method name
      @tailrec
      def loop(current: Int): Unit =
        if (current > 0) {
          f
          loop(current - 1)
        }
      loop(n)
    }
  }

  print("ha") * 5 // * is the method, effective should be A.*(5) 
}
```



## Scheme


Scheme is mostly made up from expressions which return values.  However some functions, such as <tt>display</tt>, return 
an unspecified value.  The actual value returned varies depending on the Scheme implementation itself.  


```scheme

(import (scheme base)
        (scheme write))

(define (repeat proc n)
  (do ((i 0 (+ 1 i))
       (res '() (cons (proc) res)))
    ((= i n) res)))

;; example returning an unspecified value
(display (repeat (lambda () (display "hi\n")) 4)) (newline)

;; example returning a number
(display (repeat (lambda () (+ 1 2)) 5)) (newline)

```


{{out}}

(Using chibi-scheme: returns #<undef> from <tt>display</tt>.)

```txt

hi
hi
hi
hi
(#<undef> #<undef> #<undef> #<undef>)
(3 3 3 3 3)

```



## Sidef


```ruby
func repeat(f, n) {
    { f() } * n;
}

func example {
    say "Example";
}

repeat(example, 4);
```



## Stata



```stata
function repeat(f,n) {
	for (i=1; i<=n; i++) (*f)()
}

function hello() {
	printf("Hello\n")
}

repeat(&hello(),3)
```



## Swift


```swift
func repeat(n: Int, f: () -> ()) {
  for _ in 0..<n {
    f()
  }
}

repeat(4) { println("Example") }
```



## Tcl

The usual way of doing a repeat would be:

```tcl
proc repeat {command count} {
    for {set i 0} {$i < $count} {incr i} {
        uplevel 1 $command
    }
}

proc example {} {puts "This is an example"}
repeat example 4
```

However, the <code>time</code> command can be used as long as the return value (the report on the timing information) is ignored.

```tcl>time example 4</lang

It should be noted that the “command” can be an arbitrary script, not just a call to a procedure:

```tcl
repeat {puts "hello world"} 3
```



## Ursa


```ursa
def repeat (function f, int n)
	for (set n n) (> n 0) (dec n)
		f
	end for
end repeat

def procedure ()
	out "Hello! " console
end procedure

# outputs "Hello! " 5 times
repeat procedure 5
```



## VBA

{{trans|Phix}}
```vb
Private Sub Repeat(rid As String, n As Integer)
    For i = 1 To n
        Application.Run rid
    Next i
End Sub
 
Private Sub Hello()
    Debug.Print "Hello"
End Sub
 
Public Sub main()
    Repeat "Hello", 5
End Sub
```


## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Sub Repeat(count As Integer, fn As Action(Of Integer))
        If IsNothing(fn) Then
            Throw New ArgumentNullException("fn")
        End If

        For i = 1 To count
            fn.Invoke(i)
        Next
    End Sub

    Sub Main()
        Repeat(3, Sub(x) Console.WriteLine("Example {0}", x))
    End Sub

End Module
```

{{out}}

```txt
Example 1
Example 2
Example 3
```



## XLISP


```lisp
(defun repeat (f n)
    (f)
    (if (> n 1)
        (repeat f (- n 1)) ) )

;; an example to test it:
(repeat (lambda () (print '(hello rosetta code))) 5)
```

{{out}}

```txt
(HELLO ROSETTA CODE) 
(HELLO ROSETTA CODE) 
(HELLO ROSETTA CODE) 
(HELLO ROSETTA CODE) 
(HELLO ROSETTA CODE)
```



## Yabasic

{{trans|Lua}}

```Yabasic
sub myFunc ()
    print "Sure looks like a function in here..."
end sub
 
sub rep (func$, times)
    for count = 1 to times
        execute(func$)
    next
end sub
 
rep("myFunc", 4)
```



## zkl


```zkl
fcn repeat(f,n){ do(n){ f() } }
repeat("ho ".print,3);
```

{{out}}
```txt
ho ho ho 
```

