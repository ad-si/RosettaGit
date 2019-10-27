+++
title = "Function composition"
description = ""
date = 2019-08-31T08:50:02Z
aliases = []
[extra]
id = 3991
[taxonomies]
categories = []
tags = []
+++

{{task|Higher-order functions}}

;Task:
Create a function, <span style="font-family:serif">compose</span>,   whose two arguments   <span style="font-family:serif">''f''</span>   and   <span style="font-family:serif">''g''</span>,   are both functions with one argument. 


The result of <span style="font-family:serif">compose</span> is to be a function of one argument, (lets call the argument   <span style="font-family:serif">''x''</span>),   which works like applying function   <span style="font-family:serif"> ''f'' </span>   to the result of applying function   <span style="font-family:serif"> ''g'' </span>   to   <span style="font-family:serif"> ''x''</span>.


;Example:
  <span style="font-family:serif">compose(''f'', ''g'') (''x'') = ''f''(''g''(''x''))</span>


Reference: [[wp:Function composition (computer science)|Function composition]]

Hint: In some languages, implementing <span style="font-family:serif">compose</span> correctly requires creating a [[wp:Closure (computer science)|closure]].





## ActionScript

ActionScript supports closures, making function composition very straightforward.

```ActionScript
function compose(f:Function, g:Function):Function {
	return function(x:Object) {return f(g(x));};
}
function test() {
	trace(compose(Math.atan, Math.tan)(0.5));
}
```



## Ada

The interface of a generic functions package. The package can be instantiated with any type that has value semantics. Functions are composed using the operation '*'. The same operation applied to an argument evaluates it there: f * x. Functions can be composed with pointers to [[Ada]] functions. (In [[Ada]] functions are not first-class):

```ada
generic
   type Argument is private;      
package Functions is
   type Primitive_Operation is not null
      access function (Value : Argument) return Argument;
   type Func (<>) is private;
   function "*" (Left : Func; Right : Argument) return Argument;
   function "*" (Left : Func; Right : Primitive_Operation) return Func;
   function "*" (Left, Right : Primitive_Operation) return Func;
   function "*" (Left, Right : Func) return Func;
private
   type Func is array (Positive range <>) of Primitive_Operation;
end Functions;
```

Here is an implementation;

```ada
package body Functions is
   function "*" (Left : Func; Right : Argument) return Argument is
   Result : Argument := Right;
   begin
      for I in reverse Left'Range loop
         Result := Left (I) (Result);
      end loop;
      return Result;
   end "*";

   function "*" (Left, Right : Func) return Func is
   begin
      return Left & Right;
   end "*";

   function "*" (Left : Func; Right : Primitive_Operation) return Func is
   begin
      return Left & (1 => Right);
   end "*";
   
   function "*" (Left, Right : Primitive_Operation) return Func is
   begin
      return (Left, Right);
   end "*";
end Functions;
```

The following is an example of use:

```ada
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;
with Functions;

procedure Test_Compose is
   package Float_Functions is new Functions (Float);
   use Float_Functions;

   Sin_Arcsin : Func := Sin'Access * Arcsin'Access;
begin
   Put_Line (Float'Image (Sin_Arcsin * 0.5));
end Test_Compose;
```

{{out}}

```txt

 5.00000E-01

```



## Agda


```Agda
compose : ∀ {a b c} {A : Set a} {B : Set b} {C : Set c}
        → (B → C)
        → (A → B)
        → A → C
compose f g x = f (g x)
```



## Aikido


```aikido

import math

function compose (f, g) {
    return function (x) { return f(g(x)) }
}

var func = compose(Math.sin, Math.asin)
println (func(0.5))   //  0.5


```



## Aime


```aime
compose_i(,,)
{
    ($0)(($1)($2));
}

compose(,)
{
    compose_i.apply($0, $1);
}

double(real a)
{
    2 * a;
}

square(real a)
{
    a * a;
}

main(void)
{
    o_(compose(square, double)(40), "\n");

    0;
}
```

{{Out}}

```txt
6400
```



## ALGOL 68

{{trans|Python}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}
Note: Returning <code>PROC (REAL x)REAL: f1(f2(x))</code> from a function apparently
violates standard '''ALGOL 68''''s scoping rules.  [[ALGOL 68G]] warns about this during 
parsing, and then rejects during runtime.

```algol68
MODE F = PROC(REAL)REAL; # ALGOL 68 is strong typed #

# As a procedure for real to real functions #
PROC compose = (F f, g)F: (REAL x)REAL: f(g(x));

OP (F,F)F O = compose; # or an OPerator that can be overloaded #

# Example use: #
F sin arc sin = compose(sin, arc sin);
print((sin arc sin(0.5), (sin O arc sin)(0.5), new line))
```

{{out}}

```txt

+.500000000000000e +0 +.500000000000000e +0

```

ALGOL 68 is a stack based language, and the following apparently does not violate it's scoping rules.

{{works with|ALGOL 68|Standard - Jan 1975 Boston SC allowed Partial Parametrization. }}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

```algol68
MODE F = PROC(REAL)REAL; # ALGOL 68 is strong typed #

# As a procedure for real to real functions #
PROC compose = (F f, g)F: ((F f2, g2, REAL x)REAL: f2(g2(x)))(f, g, ); # Curry #

PRIO O = 7;
OP (F,F)F O = compose; # or an OPerator that can be overloaded #

# Example use: #
F sin arc sin = compose(sin, arc sin);
print((sin arc sin(0.5), (sin O arc sin)(0.5), new line))
```



## AntLang


```AntLang
/Apply g to exactly one argument
compose1: {f: x; g: y; {f[g[x]]}}
/Extra: apply to multiple arguments
compose: {f: x; g: y; {f[g apply args]}}
```



## AppleScript


```applescript
-- Compose two functions where each function is
-- a script object with a call(x) handler.
on compose(f, g)
    script
        on call(x)
            f's call(g's call(x))
        end call
    end script
end compose

script sqrt
    on call(x)
        x ^ 0.5
    end call
end script

script twice
    on call(x)
        2 * x
    end call
end script

compose(sqrt, twice)'s call(32)
-- Result: 8.0
```


A limitation of AppleScript's handlers (functions), which can be seen in the example above, is that they are not in themselves composable first class objects, and have to be lifted into script objects before they can be composed or passed as arguments.

We can generalise this lifting with an '''mReturn''' or '''mInject''' function, which injects a handler into a script for us. This allows use to write higher-order composition and pipelining functions which take a pair (or sequence of) ordinary handlers as arguments, and return a first class script object. (We can also use mReturn to equip AppleScript with '''map''' and '''fold''' functions which take a list and an ordinary handler as arguments).


```applescript
-- FUNCTIONS TO COMPOSE -------------------------------------------------------

on root(x)
    x ^ 0.5
end root

on succ(x)
    x + 1
end succ

on half(x)
    x / 2
end half

-- TEST -----------------------------------------------------------------------
on run
    
    compose([half, succ, root])'s |λ|(5)
    
    --> 1.61803398875
end run

-- GENERIC FUNCTIONS ----------------------------------------------------------

-- compose :: [(a -> a)] -> (a -> a)
on compose(fs)
    script
        on |λ|(x)
            script
                on |λ|(a, f)
                    mReturn(f)'s |λ|(a)
                end |λ|
            end script
            
            foldr(result, x, fs)
        end |λ|
    end script
end compose

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

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

```txt
1.61803398875
```



## Applesoft BASIC


```ApplesoftBasic
10 F$ = "SIN"
20 DEF FN A(P) = ATN(P/SQR(-P*P+1))
30 G$ = "FN A"
40 GOSUB 100"COMPOSE
50 SA$ = E$

60 X = .5 : E$ = SA$
70 GOSUB 200"EXEC
80 PRINT R
90 END

100 E$ = F$ + "(" + G$ + "(X))" : RETURN : REMCOMPOSE F$ G$

200 D$ = CHR$(4) : FI$ = "TEMPORARY.EX" : M$ = CHR$(13)
210 PRINT D$"OPEN"FI$M$D$"CLOSE"FI$M$D$"DELETE"FI$
220 PRINT D$"OPEN"FI$M$D$"WRITE"FI$
230 PRINT "CALL-998:CALL-958:R="E$":CONT"
240 PRINT D$"CLOSE"FI$M$D$"EXEC"FI$:CALL-998:END:RETURN
```



## Argile

Only works for functions taking real and returning real (double precision, 64 bits)
{{works with|Argile|1.0.0}}

```Argile
use std, math

let my_asin = new Function (.:<any,real x>:. -> real {asin x})
let my__sin = new Function (.:<any,real x>:. -> real { sin x})
let sinasin = my__sin o my_asin
print sin asin 0.5
print *my__sin 0.0
print *sinasin 0.5
~my_asin
~my__sin
~sinasin

=: <Function f> o <Function g> := -> Function {compose f g}

.:compose <Function f, Function g>:. -> Function
  use array
  let d = (new array of 2 Function)
  (d[0]) = f ; (d[1]) = g
  let c = new Function (.:<array of Function fg, real x>:. -> real {
    *fg[0]( *fg[1](x) )
  }) (d)
  c.del = .:<any>:.{free any}
  c

class Function
  function(any)(real)->(real)	func
  any				data
  function(any)			del

=: * <Function f> <real x> := -> real
   Cgen "(*("(f.func)"))("(f.data)", "(x)")"

.: del Function <Function f> :.
   unless f.del is nil
     call f.del with f.data
   free f
=: ~ <Function f> := {del Function f}

.: new Function <function(any)(real)-\>real func> (<any data>):. -> Function
   let f = new Function
   f.func = func
   f.data = data
   f
```




## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276379.html#276379 forum]

```AutoHotkey
MsgBox % compose("sin","cos",1.5)

compose(f,g,x) { ; function composition
   Return %f%(%g%(x))
}
```
 


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      REM Create some functions for testing:
      DEF FNsqr(a) = SQR(a)
      DEF FNabs(a) = ABS(a)
      
      REM Create the function composition:
      SqrAbs = FNcompose(FNsqr(), FNabs())
      
      REM Test calling the composition:
      x = -2 : PRINT ; x, FN(SqrAbs)(x)
      END
      
      DEF FNcompose(RETURN f%, RETURN g%)
      LOCAL f$, p% : DIM p% 7 : p%!0 = f% : p%!4 = g%
      f$ = "(x)=" + CHR$&A4 + "(&" + STR$~p% + ")(" + \
      \             CHR$&A4 + "(&" + STR$~(p%+4) + ")(x))"
      DIM p% LEN(f$) + 4 : $(p%+4) = f$ : !p% = p%+4
      = p%
```

{{out}}

```txt

-2        1.41421356

```



## Bori


```bori
double sin (double v)	{ return Math.sin(v); }
double asin (double v)	{ return Math.asin(v); }
Var compose (Func f, Func g, double d)	{ return f(g(d)); }

void button1_onClick (Widget widget)
{
	double d = compose(sin, asin, 0.5);
	label1.setText(d.toString(9));
}
```

{{out}} on Android phone:

```bori>0.500000000</lang



## Brat


```brat
compose = { f, g | { x | f g x } }
  
#Test
add1 = { x | x + 1 }
double = { x | x * 2 }
b = compose(->double ->add1)
p b 1 #should print 4
```



## C

Only works for functions taking a double and returning a double:

```c>#include <stdlib.h


/* generic interface for functors from double to double */
typedef struct double_to_double {
  double (*fn)(struct double_to_double *, double);
} double_to_double;

#define CALL(f, x) f->fn(f, x)


/* functor returned by compose */
typedef struct compose_functor {
  double (*fn)(struct compose_functor *, double);
  double_to_double *f;
  double_to_double *g;
} compose_functor;
/* function to be used in "fn" in preceding functor */
double compose_call(compose_functor *this, double x) {
  return CALL(this->f, CALL(this->g, x));
}
/* returns functor that is the composition of functors
   f & g. caller is responsible for deallocating memory */
double_to_double *compose(double_to_double *f,
                          double_to_double *g) {
  compose_functor *result = malloc(sizeof(compose_functor));
  result->fn = &compose_call;
  result->f = f;
  result->g = g;
  return (double_to_double *)result;
}



#include <math.h>

/* we can make functors for sin and asin by using 
   the following as "fn" in a functor */
double sin_call(double_to_double *this, double x) {
  return sin(x);
}
double asin_call(double_to_double *this, double x) {
  return asin(x);
}



#include <stdio.h>

int main() {
  double_to_double *my_sin = malloc(sizeof(double_to_double));
  my_sin->fn = &sin_call;
  double_to_double *my_asin = malloc(sizeof(double_to_double));
  my_asin->fn = &asin_call;

  double_to_double *sin_asin = compose(my_sin, my_asin);

  printf("%f\n", CALL(sin_asin, 0.5)); /* prints "0.500000" */

  free(sin_asin);
  free(my_sin);
  free(my_asin);

  return 0;
}
```



## C++


```cpp>#include <functional

#include <cmath>
#include <iostream>

// functor class to be returned by compose function
template <class Fun1, class Fun2>
class compose_functor :
  public std::unary_function<typename Fun2::argument_type,
                             typename Fun1::result_type>
{
protected:
  Fun1 f;
  Fun2 g;

public:
  compose_functor(const Fun1& _f, const Fun2& _g)
    : f(_f), g(_g) { }

  typename Fun1::result_type
  operator()(const typename Fun2::argument_type& x) const
  { return f(g(x)); }
};

// we wrap it in a function so the compiler infers the template arguments
// whereas if we used the class directly we would have to specify them explicitly
template <class Fun1, class Fun2>
inline compose_functor<Fun1, Fun2>
compose(const Fun1& f, const Fun2& g)
{ return compose_functor<Fun1,Fun2>(f, g); }

int main() {
  std::cout << compose(std::ptr_fun(::sin), std::ptr_fun(::asin))(0.5) << std::endl;

  return 0;
}
```


{{works with|C++11}} composing <code>std::function</code>

```cpp>#include <iostream

#include <functional>
#include <cmath>

template <typename A, typename B, typename C>
std::function<C(A)> compose(std::function<C(B)> f, std::function<B(A)> g) {
  return [f,g](A x) { return f(g(x)); };
}

int main() {
  std::function<double(double)> f = sin;
  std::function<double(double)> g = asin;
  std::cout << compose(f, g)(0.5) << std::endl;

  return 0;
}
```


{{works with|C++14}}
This much simpler version uses <code>decltype(auto)</code>.


```cpp

#include <iostream>
#include <math.h>
 
template <class F, class G>
decltype(auto) compose(F&& f, G&& g)
{
    return [=](auto x) { return f(g(x)); };
}
 
int main() {
  std::cout << compose(sin, asin)(0.5) << "\n";
  return 0;
}

```


{{works with|GCC}} GCC's C++ library has a built-in compose function

```cpp>#include <iostream

#include <cmath>
#include <ext/functional>

int main() {
  std::cout << __gnu_cxx::compose1(std::ptr_fun(::sin), std::ptr_fun(::asin))(0.5) << std::endl;

  return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
using System;
class Program
{
    static void Main(string[] args)
    {
        Func<int, int> outfunc = Composer<int, int, int>.Compose(functA, functB);
        Console.WriteLine(outfunc(5)); //Prints 100
    }
    static int functA(int i) { return i * 10; }
    static int functB(int i) { return i + 5; }
    class Composer<A, B, C>
    {
        public static Func<C, A> Compose(Func<B, A> a, Func<C, B> b)
        {
            return delegate(C i) { return a(b(i)); };
        }
    }
}
```



## Clojure

Function composition is built in to Clojure.  Simply call the <code>comp</code> function.

A manual implementation could look like this:

```clojure
(defn compose [f g]
  (fn [x]
    (f (g x))))

; Example
(def inc2 (compose inc inc))
(println (inc2 5)) ; prints 7
```



## CoffeeScript


```coffeescript

compose = ( f, g ) -> ( x ) -> f g x

# Example
add2 = ( x ) -> x + 2
mul2 = ( x ) -> x * 2

mulFirst = compose add2, mul2
addFirst = compose mul2, add2
multiple = compose mul2, compose add2, mul2

console.log "add2 2 #=> #{ add2 2 }"
console.log "mul2 2 #=> #{ mul2 2 }"
console.log "mulFirst 2 #=> #{ mulFirst 2 }"
console.log "addFirst 2 #=> #{ addFirst 2 }"
console.log "multiple 2 #=> #{ multiple 2 }"

```


{{out}}

```txt

add2 2 #=> 4
mul2 2 #=> 4
mulFirst 2 #=> 6
addFirst 2 #=> 8
multiple 2 #=> 12

```


Or, extending the <code>Function</code> prototype.


```coffeescript

Function::of = (f) -> (args...) => @ f args...

# Example
add2 = (x) -> x + 2
mul2 = (x) -> x * 2

mulFirst = add2.of mul2
addFirst = mul2.of add2
multiple = mul2.of add2.of mul2

console.log "add2 2 #=> #{ add2 2 }"
console.log "mul2 2 #=> #{ mul2 2 }"
console.log "mulFirst 2 #=> #{ mulFirst 2 }"
console.log "addFirst 2 #=> #{ addFirst 2 }"
console.log "multiple 2 #=> #{ multiple 2 }"

```


Output is identical.


## Common Lisp

<code>compose</code> returns a function that closes on the lexical variables f and g.

```lisp
(defun compose (f g) (lambda (x) (funcall f (funcall g x))))
```


Example use:

```lisp>
(defun compose (f g) (lambda (x) (funcall f (funcall g x))))
COMPOSE
>(let ((sin-asin (compose #'sin #'asin)))
   (funcall sin-asin 0.5))
0.5
```


This alternate solution, more ugly and more difficult, never closes on any lexical variables. Instead, it uses [[runtime evaluation]] to insert the values of f and g into new code. This is just a different way to create a closure.


```lisp
(defun compose (f g)
  (eval `(lambda (x) (funcall ',f (funcall ',g x)))))
```


----

In this last example, a macro is used to compose any number of single parameter functions.  

```lisp
CL-USER> (defmacro compose (fn-name &rest args)
	   (labels ((rec1 (args)
		      (if (= (length args) 1)
			  `(funcall ,@args x)
			  `(funcall ,(first args) ,(rec1 (rest args))))))
	     `(defun ,fn-name (x) ,(rec1 args))))
```

Because this macro expands into a defun form, the function returned by compose is in the function namespace and the use of funcall is not necessary.

```txt
CL-USER> (compose f #'ceiling #'sin #'sqrt)
F
CL-USER> (compose g #'1+ #'abs #'cos)
G
CL-USER> (compose h #'f #'g)
H
CL-USER> (values (f pi) (g pi) (h pi))
1
2.0L0
1
CL-USER> 
```



## D


```d
import std.stdio;

T delegate(S) compose(T, U, S)(in T delegate(U) f,
                               in U delegate(S) g) {
    return s => f(g(s));
}

void main() {
    writeln(compose((int x) => x + 15, (int x) => x ^^ 2)(10));
    writeln(compose((int x) => x ^^ 2, (int x) => x + 15)(10));
}
```

{{out}}

```txt
115
625
```



## Delphi


Anonymous methods were introduced in Delphi 2009, so next code works with Delphi 2009 and above:


```Delphi
program AnonCompose;

{$APPTYPE CONSOLE}

type
  TFunc = reference to function(Value: Integer): Integer;

function Compose(F, G: TFunc): TFunc;
begin
  Result:= function(Value: Integer): Integer
  begin
    Result:= F(G(Value));
  end
end;

var
  Func1, Func2, Func3: TFunc;

begin
  Func1:=
    function(Value: Integer): Integer
    begin
      Result:= Value * 2;
    end;

  Func2:=
    function(Value: Integer): Integer
    begin
      Result:= Value * 3;
    end;

  Func3:= Compose(Func1, Func2);

  Writeln(Func3(6));    // 36 = 6 * 3 * 2
  Readln;
end.
```


=={{header|Déjà Vu}}==
It is already defined in the standard library as <code>$</code>.


```dejavu
compose f g:
	labda:
		f g
```



## Dylan


compose[https://opendylan.org/books/drm/Functional_Operations#compose] is already part of the language standard, with a more complete definition than this.


```dylan

define method compose(f,g)
   method(x) f(g(x)) end
end;

```



## Ela

It is already defined in standard prelude as (<<) operator.


```ela
let compose f g x = f (g x)
```



## E



```e
def compose(f, g) {
  return fn x { return f(g(x)) }
}
```



## EchoLisp


```lisp

;; By decreasing order of performance
;; 1) user definition : lambda and closure

(define (ucompose f g ) (lambda (x) ( f ( g x))))
(ucompose sin cos)
   → (🔒 λ (_x) (f (g _x)))

;; 2) built-in compose : lambda

(compose sin cos)
   → (λ (_#:g1002) (#apply-compose (#list #cos #sin) _#:g1002))

;; 3) compiled composition

(define (sincos x) (sin (cos x)))
sincos → (λ (_x) (⭕️ #sin (#cos _x))) 

```

{{out}}

```lisp

((ucompose sin cos) 3) → -0.8360218615377305
((compose sin cos) 3) → -0.8360218615377305
(sincos 3) → -0.8360218615377305

```



## Ela

It is already defined in standard prelude as (<<) operator.


```ela
compose f g x = f (g x)
```


## Elena

ELENA 4.x :

```elena
import extensions;
 
extension op : Func1
{
    compose(Func1 f)
        = (x => self(f(x)));
}
 
public program()
{
    var fg := (x => x + 1).compose:(x => x * x);
 
    console.printLine(fg(3))
}
```

{{out}}

```txt

10

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def compose(f, g), do: fn(x) -> f.(g.(x)) end
  
  def multicompose(fs), do: List.foldl(fs, fn(x) -> x end, &compose/2)
end

sin_asin = RC.compose(&:math.sin/1, &:math.asin/1)
IO.puts sin_asin.(0.5)

IO.puts RC.multicompose([&:math.sin/1, &:math.asin/1, fn x->1/x end]).(0.5)
IO.puts RC.multicompose([&(&1*&1), &(1/&1), &(&1*&1)]).(0.5)
```


{{out}}

```txt

0.5
2.0
16.0

```



## Emacs Lisp

A <code>lambda</code> form can be constructed with the desired <code>f</code> and <code>g</code> inserted.  The result is simply a list.  A list starting with <code>lambda</code> is a function.


```Lisp
(defun compose (f g)
  `(lambda (x) (,f (,g x))))

(let ((func (compose '1+ '1+)))
  (funcall func 5))
=>
7
```


A similar thing can be done with a macro like the following.  It differs in that the arguments should be unquoted symbols, and if they're expressions then they're evaluated on every call to the resulting <code>lambda</code>.


```Lisp
(defmacro compose (f g)
  `(lambda (x) (,f (,g x))))

(let ((func (compose 1+ 1+)))
  (funcall func 5))
=>
7
```


Another possibility is the <code>cl.el</code> <code>lexical-let</code> to hold <code>f</code> and <code>g</code> for use in a new <code>lambda</code>.


```Lisp
(eval-when-compile (require 'cl)) ;; for `lexical-let' macro
(defun compose (f g)
  (lexical-let ((f f)
                (g g))
    (lambda (x)
      (funcall f (funcall g x)))))

(let ((func (compose '1+ '1+)))
  (funcall func 5))
=>
7
```



## Erlang


```erlang
-module(fn).
-export([compose/2, multicompose/1]).

compose(F,G) -> fun(X) -> F(G(X)) end.

multicompose(Fs) -> 
    lists:foldl(fun compose/2, fun(X) -> X end, Fs).
```


Using them:

```erlang>1
 (fn:compose(fun math:sin/1, fun math:asin/1))(0.5).
0.5
2> Sin_asin_plus1 = fn:multicompose([fun math:sin/1, fun math:asin/1, fun(X) -> X + 1 end]). 
#Fun<tests.0.59446746>
82> Sin_asin_plus1(0.5).
1.5
```


{{omit from|Euphoria}}

=={{header|F Sharp|F#}}==
The most-used composition operator in F# is <code>>></code>. It implements ''forward'' composition, i.e. <code>f >> g</code> is a function which calls f first and then calls g on the result.

The ''reverse'' composition operator <code><<</code>, on the other hand, exactly fulfills the requirements of the compose function described in this task. 

We can implement composition manually like this (F# Interactive session):

```fsharp>
 let compose f g x = f (g x);;

val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
```

Usage:

```fsharp>
 let sin_asin = compose sin asin;;

val sin_asin : (float -> float)

> sin_asin 0.5;;
val it : float = 0.5
```



## Factor

Factor is a concatenative language, meaning its inherent method of data flow is function composition. <code>foo bar baz</code> in Factor is equivalent to <code>baz(bar(foo()))</code> in an applicative language.

When passing functions around and creating anonymous functions, Factor uses so-called quotations. There is already a word (<code>compose</code>) that provides composition of quotations.

```factor
( scratchpad ) [ 2 * ] [ 1 + ] compose .
[ 2 * 1 + ]
( scratchpad ) 4 [ 2 * ] [ 1 + ] compose call .
9
```



## Fantom


```fantom

class Compose
{
  static |Obj -> Obj| compose (|Obj -> Obj| fn1, |Obj -> Obj| fn2)
  {
    return |Obj x -> Obj| { fn2 (fn1 (x)) }
  }

  public static Void main ()
  {
    double := |Int x -> Int| { 2 * x }
    |Int -> Int| quad := compose(double, double)
    echo ("Double 3 = ${double(3)}")
    echo ("Quadruple 3 = ${quad (3)}")
  }
}

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Function_composition this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: compose ( xt1 xt2 -- xt3 )
  >r >r :noname
     r> compile,
     r> compile,
     postpone ;
;

' 2* ' 1+ compose  ( xt )
3 swap execute .   \ 7
```



## Fortran

Modern Fortran standard has (limited) kind of higher-order functions (as result, argument, and with one level of nested functions) and optional arguments, and this enables to compose the following function (it is impure because Fortran ''has no closures''). For simple cases function calls may be just nested to achieve the effect of function composition, because in fortran nested calls f(g(d(x))) generate a hierarchic set of function calls and the result of each function is transmitted to its calling function in a standard way for all functions.


```fortran

module functions_module
   implicit none
   private ! all by default
   public :: f,g

contains

   pure function  f(x)
      implicit none
      real, intent(in) :: x
      real :: f
      f = sin(x)
   end function f

   pure function  g(x)
      implicit none
      real, intent(in) :: x
      real :: g
      g = cos(x)
   end function g

end module functions_module

module compose_module
   implicit none
   private ! all by default
   public :: compose

   interface
      pure function  f(x)
         implicit none
         real, intent(in) :: x
         real :: f
      end function f

      pure function  g(x)
         implicit none
         real, intent(in) :: x
         real :: g
      end function g
   end interface

contains

   impure function  compose(x, fi, gi)
      implicit none
      real, intent(in) :: x
      procedure(f), optional :: fi
      procedure(g), optional :: gi
      real :: compose

      procedure (f), pointer, save :: fpi => null()
      procedure (g), pointer, save :: gpi => null()

      if(present(fi) .and. present(gi))then
         fpi => fi
         gpi => gi
         compose = 0
         return
      endif

      if(.not. associated(fpi)) error stop "fpi"
      if(.not. associated(gpi)) error stop "gpi"

      compose = fpi(gpi(x))

   contains

   end function compose

end module compose_module

program test_compose
   use functions_module
   use compose_module
   implicit none
   write(*,*) "prepare compose:", compose(0.0, f,g)
   write(*,*) "run compose:", compose(0.5)
end program test_compose

```



## Fortress

In Fortress, there are two ways that you can compose functions.

1. You can compose functions manually by writing your own composition function.

In this version, we allow any type of function to be used by defining our own types in the function definition and using those types to define how the composed function should behave. This version operates very similarly to the way that the COMPOSE operator, explained below, operates.


```fortress

  compose[\A, B, C\](f:A->B, g:B->C, i:Any): A->C = do
    f(g(i))
  end

  composed(i:RR64): RR64 = compose(sin, cos, i)

```


Alternatively, you could explicitly define each type for improved type safety.

Due to the fact that alt_compose() is built around the idea that it is being used to compose two trigonometric functions, these will return identical functions. However, if you were to pass alt_composed() any other type of function, the interpreter would throw an error.

```fortress

  alt_compose(f:Number->RR64, g:Number->RR64, i:RR64): ()->RR64 = do
    f(g(i))
  end

  alt_composed(i:RR64): RR64 = compose(sin, cos, i)

```


2. You can use the COMPOSE operator (or CIRC or RING). Because COMPOSE returns an anonymous function, it is necessary to wrap it in parentheses if you want to be able to use it in this manner.


```fortress

  opr_composed(i:Number): Number->RR64 = (sin COMPOSE cos)(i)

```


Should you need to, you could also mix both methods by overloading the COMPOSE operator.


## FunL


```funl
import math.{sin, asin}

def compose( f, g ) = x -> f( g(x) )

sin_asin = compose( sin, asin )

println( sin_asin(0.5) )
```


{{out}}


```txt

0.5

```



## GAP


```gap
Composition := function(f, g)
    return x -> f(g(x));
end;

h := Composition(x -> x+1, x -> x*x);
h(5);
# 26
```



## Go


```go
// Go doesn't have generics, but sometimes a type definition helps
// readability and maintainability.   This example is written to
// the following function type, which uses float64.
type ffType func(float64) float64

// compose function requested by task
func compose(f, g ffType) ffType {
    return func(x float64) float64 {
        return f(g(x))
    }
}
```

Example use:

```go
package main

import "math"
import "fmt"

type ffType func(float64) float64

func compose(f, g ffType) ffType {
    return func(x float64) float64 {
        return f(g(x))
    }
}

func main() {
    sin_asin := compose(math.Sin, math.Asin)
    fmt.Println(sin_asin(.5))
}
```

{{out}}

```txt

0.5

```



## Groovy

Test program:

```groovy
final times2 = { it * 2 }
final plus1 = { it + 1 }

final plus1_then_times2 = times2 << plus1
final times2_then_plus1 = times2 >> plus1

assert plus1_then_times2(3) == 8
assert times2_then_plus1(3) == 7
```



## Haskell

This is already defined as the '''.''' (dot) operator in Haskell.

```haskell
compose f g x = f (g x)
```

Example use:

```haskell>Prelude
 let compose f g x = f (g x)
Prelude> let sin_asin = compose sin asin
Prelude> sin_asin 0.5
0.5
```


Right to left composition of a list of functions could be defined as ''flip (foldr id)'':


```haskell
composeList :: [a -> a] -> a -> a
composeList = flip (foldr id)


main :: IO ()
main = print $ composeList [(/ 2), succ, sqrt] 5
```

{{Out}}

```txt
1.618033988749895
```



## Hy


```clojure
(defn compose [f g]
  (fn [x]
    (f (g x))))
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon don't have a lambda function or native closure; however, they do have co-expressions which are extremely versatile and can be used to achieve the same effect.  The list of functions to compose can be a 'procedure', 'co-expression", or an invocable string (i.e. procedure name or unary operator). It will correctly handle compose(compose(...),..).

There are a few limitations to be aware of:
*  type(compose(f,g)) returns a co-expression not a procedure
*  this construction only handles functions of 1 argument (a closure construct is better for the general case)


The solution below can be adapted to work in Icon by reverting to the old syntax for invoking co-expressions.

```Icon
   x @ f                      # use this syntax in Icon instead of the Unicon f(x) to call co-expressions
   every push(fL := [],!rfL)  # use this instead of reverse(fL) as the Icon reverse applies only to strings
```

See [[Icon%2BUnicon/Intro#Minor_Differences|Icon and Unicon Introduction:Minor Differences]] for more information


```Unicon
procedure main(arglist)
    h := compose(sqrt,abs)
    k := compose(integer,"sqrt",ord)
    m := compose("-",k)
    every write(i := -2 to 2, " h=(sqrt,abs)-> ", h(i))
    every write(c :=  !"1@Q", " k=(integer,\"sqrt\",ord)-> ", k(c))
    write(c := "1"," m=(\"-\",k) -> ",m(c))
end

invocable all                                            # permit string invocations

procedure compose(fL[])   #: compose(f1,f2,...) returns the functional composition of f1,f2,... as a co-expression
    local x,f,saveSource

    every case type(x := !fL) of { 
       "procedure"|"co-expression": &null                # procedures and co-expressions are fine
       "string" : if not proc(x,1) then runnerr(123,fL)  # as are invocable strings (unary operators, and procedures)
       default: runerr(123,fL)
       }

    fL := reverse(fL)                                    # reverse and isolate from mutable side-effects 
    cf := create {  saveSource := &source                # don't forget where we came from
                    repeat {
                        x := (x@saveSource)[1]           # return result and resume here
                        saveSource := &source            # ...
                        every f := !fL do x := f(x)      # apply the list of 'functions'
                        }
                 }
    return (@cf, cf)                                     # 'prime' the co-expr before returning it

end
```


{{out}}

```txt
-2 h=(sqrt,abs)-> 1.414213562373095
-1 h=(sqrt,abs)-> 1.0
0 h=(sqrt,abs)-> 0.0
1 h=(sqrt,abs)-> 1.0
2 h=(sqrt,abs)-> 1.414213562373095
1 k=(integer,"sqrt",ord)-> 7
@ k=(integer,"sqrt",ord)-> 8
Q k=(integer,"sqrt",ord)-> 9
1 m=("-",k) -> -7

```



## J


'''Solution''':

```j>compose =: @</lang


'''Example''':

```j>f compose g</lang


Of course, given that <code>@</code> is only one character long and is a built-in primitive, there is no need for the cover function <code>compose</code>.  And <code>@</code> is not the only composition primitive; composition is a very important concept in J.  For more details, see the [[Talk:Functional Composition#J|talk page]].

Tentative new example:


```j>f=: 
.@(1&o.)@%:
g=: 1&+@|@(2&o.)
h=: f@g
```


Example use:

```j
   (f, g, h) 1p1
1 2 1
```


Note: <code>1&o.</code> is sine (mnemonic: sine is an odd circular function), <code>2&o.</code> is cosine (cosine is an even circular function), <code>%:</code> is square root, <code>>.</code> is ceiling, <code>|</code> is absolute value and <code>1&+</code> adds 1.


## Java


```java
public class Compose {

    // Java doesn't have function type so we define an interface
    // of function objects instead
    public interface Fun<A,B> {
        B call(A x);
    }

    public static <A,B,C> Fun<A,C> compose(final Fun<B,C> f, final Fun<A,B> g) {
        return new Fun<A,C>() {
            public C call(A x) {
                return f.call(g.call(x));
            }
        };
    }

    public static void main(String[] args) {
        Fun<Double,Double> sin = new Fun<Double,Double>() {
            public Double call(Double x) {
                return Math.sin(x);
            }
        };
        Fun<Double,Double> asin = new Fun<Double,Double>() {
            public Double call(Double x) {
                return Math.asin(x);
            }
        };

        Fun<Double,Double> sin_asin = compose(sin, asin);

        System.out.println(sin_asin.call(0.5)); // prints "0.5"
    }
}
```



### Java 8


Java 8's <code>Function</code> interface already has a <code>.compose()</code> default method:
{{works with|Java|8+}}

```java
import java.util.function.Function;

public class Compose {
    public static void main(String[] args) {
        Function<Double,Double> sin_asin = ((Function<Double,Double>)Math::sin).compose(Math::asin);

        System.out.println(sin_asin.apply(0.5)); // prints "0.5"
    }
}
```


Implementing it yourself as a static method:
{{works with|Java|8+}}

```java
import java.util.function.Function;

public class Compose {
    public static <A,B,C> Function<A,C> compose(Function<B,C> f, Function<A,B> g) {
        return x -> f.apply(g.apply(x));
    }

    public static void main(String[] args) {
        Function<Double,Double> sin_asin = compose(Math::sin, Math::asin);

        System.out.println(sin_asin.apply(0.5)); // prints "0.5"
    }
}
```



## JavaScript


### ES5


### =Simple composition of two functions=


```javascript
function compose(f, g) {
  return function(x) {
    return f(g(x));
  };
}
```

Example:

```javascript
var id = compose(Math.sin, Math.asin);
print(id(0.5)); // 0.5
```




### =Multiple composition=


Recursion apart, multiple composition can be written in at least two general ways in JS:

# Iteratively (faster to run, perhaps more fiddly to write)
# With a fold / reduction (see http://rosettacode.org/wiki/Catamorphism). The fold is arguably simpler to write and reason about, though not quite as fast to execute.


```JavaScript
(function () {
    'use strict';


    // iterativeComposed :: [f] -> f
    function iterativeComposed(fs) {

        return function (x) {
            var i = fs.length,
                e = x;

            while (i--) e = fs[i](e);
            return e;
        }
    }

    // foldComposed :: [f] -> f
    function foldComposed(fs) {

        return function (x) {
            return fs
                .reduceRight(function (a, f) {
                    return f(a);
                }, x);
        };
    }


    var sqrt = Math.sqrt,

        succ = function (x) {
            return x + 1;
        },

        half = function (x) {
            return x / 2;
        };


    // Testing two different multiple composition ([f] -> f) functions

    return [iterativeComposed, foldComposed]
        .map(function (compose) {

            // both functions compose from right to left
            return compose([half, succ, sqrt])(5);

        });
})();

```


{{Out}}

```txt
[1.618033988749895, 1.618033988749895]
```



### ES6


```JavaScript
(() => {
    'use strict';

    // compose :: [(a -> a)] -> (a -> a)
    const compose = (...fs) =>
        x => fs.reduceRight(
            (a, f) => f(a),
            x
        );

    // Test a composition of 3 functions (right to left)
    const
        sqrt = Math.sqrt,
        succ = x => x + 1,
        half = x => x / 2;

    return compose(half, succ, sqrt)(5);
    
    // --> 1.618033988749895
})();
```

{{Out}}

```txt
1.618033988749895
```



## Joy

Composition is the default operation in Joy. The composition of two functions is the concatenation of those functions, in the order in which they are to be applied.

```joy>g f</lang



## jq

The equivalent in jq of a function with one argument is a 0-arity filter. For example, in jq, exp is the exponential function and can be evaluated like so: <tt>0.5 | exp</tt>. 

We therefore illustrate here how a function that composes two 0-arity filters can be written:

```jq

# apply g first and then f
def compose(f; g): g | f;

```

Example: 0.5 | compose(asin; sin)

In practice, "compose" is rarely used since, given two 0-arity filters, f and g, the expression "g|f" can be passed as an argument to other functions.


## Julia

{{works with|Julia|0.6}}
'''Built-in''':

```julia
@show (asin ∘ sin)(0.5)
```


'''Alternative''':

```julia
compose(f::Function, g::Function) = (x) -> g(f(x))
@show compose(sin, asin)(0.5)
```



## K

The K syntax for APL tacit (point free) function composition is the dyadic form of apostrophe ('), here is a cover function

```k
compose:{'[x;y]}
```


An equivalent explicit definition would be

```k
compose:{x[y[z]]}
```


'''Example:'''

```k
  sin_asin:compose[sin;asin] // or compose . (sin;asin)
  sin_asin 0.5
0.5
```



## Kotlin


```scala
// version 1.0.6

fun f(x: Int): Int = x * x

fun g(x: Int): Int = x + 2

fun compose(f: (Int) -> Int,  g: (Int) -> Int): (Int) -> Int  = { f(g(it)) }

fun main(args: Array<String>) {
   val x  = 10
   println(compose(::f, ::g)(x))
}
```


{{out}}

```txt

144

```



## LFE


```lisp

(defun compose (f g)
  (lambda (x)
    (funcall f
      (funcall g x))))

(defun compose (funcs)
  (lists:foldl #'compose/2
               (lambda (x) x)
               funcs))

(defun check ()
  (let* ((sin-asin (compose #'math:sin/1 #'math:asin/1))
         (expected (math:sin (math:asin 0.5)))
         (compose-result (funcall sin-asin 0.5)))
    (io:format '"Expected answer: ~p~n" (list expected))
    (io:format '"Answer with compose: ~p~n" (list compose-result))))

```


If you pasted those into the LFE REPL, you can do the following:

```lisp

> (funcall (compose #'math:sin/1 #'math:asin/1)
           0.5)
0.49999999999999994
> (funcall (compose `(,#'math:sin/1
                      ,#'math:asin/1
                      ,(lambda (x) (+ x 1))))
           0.5)
1.5
> (check)
Expected answer: 0.49999999999999994
Answer with compose: 0.49999999999999994
ok
>

```



## Lingo

Lingo does not support functions as first-class objects. However, there is a way to achieve something similar:

In Lingo global functions (i.e. either built-in functions or custom functions defined in movie scripts) are methods of the _movie object. There are 2 ways to call such functions:

*a) foo (1,2,3)
*b) call (#foo, _movie, 1, 2, 3)
<br />
If we ignore the standard way a) and only concentrate on b), we can define a "call-function" (arbitrary word coining) as:

: ''"Anything that supports the syntax 'call(<func>, _movie [, comma-separated arg list])' and might return a value."''

As described above, this "call-function" definition includes all built-in and global user-defined functions.

For such "call-functions", function composition can be implemented using the following global (i.e. movie script) function compose() and the following parent script "Composer":


```lingo
-- in some movie script
----------------------------------------
-- Composes 2 call-functions, returns a new call-function
-- @param {symbol|instance} f
-- @param {symbol|instance} g
-- @return {instance}
----------------------------------------
on compose (f, g)
  return script("Composer").new(f, g)
end
```



```lingo
-- parent script "Composer"

property _f
property _g

----------------------------------------
-- @constructor
-- @param {symbol|instance} f
-- @param {symbol|instance} g
----------------------------------------
on new (me, f, g)
  me._f = f
  me._g = g
  return me
end

on call (me)
  if ilk(me._g)=#instance then
    cmd = "_movie.call(#call,me._g,VOID"
  else
    cmd = "_movie.call(me._g,_movie"
  end if
  a = [] -- local args list
  repeat with i = 1 to the paramCount-2
    a[i] = param(i+2)
    put ",a["&i&"]" after cmd
  end repeat
  put ")" after cmd
  if ilk(me._f)=#instance then
    return _movie.call(#call, me._f, VOID, value(cmd))
  else
    return _movie.call(me._f, _movie, value(cmd))
  end if
end
```


Usage:

```lingo
-- compose new function based on built-in function 'sin' and user-defined function 'asin'
f1 = compose(#asin, #sin)
put call(f1, _movie, 0.5)
-- 0.5000

-- compose new function based on previously composed function 'f1' and user-defined function 'double'
f2 = compose(#double, f1)
put call(f2, _movie, 0.5)
-- 1.0000

-- compose new function based on 2 composed functions
f1 = compose(#asin, #sin)
f2 = compose(#double, #triple)
f3 = compose(f2, f1)
put call(f3, _movie, 0.5)
-- 3.0000
```


User-defined custom functions used in demo code above:

```lingo
-- in some movie script
on asin (x)
  res = atan(sqrt(x*x/(1-x*x)))
  if x<0 then res = -res
  return res
end

on double (x)
  return x*2
end

on triple (x)
  return x*3
end
```



## LOLCODE

LOLCODE supports first-class functions only insofar as they may be stored in variables and returned from other functions. Alas, given the current lack of support for either lambdas or closures, function composition can only be reasonably simulated with the help of a few global variables.

```LOLCODE
HAI 1.3

I HAS A fx, I HAS A gx

HOW IZ I composin YR f AN YR g
    fx R f, gx R g
    HOW IZ I composed YR x
        FOUND YR I IZ fx YR I IZ gx YR x MKAY MKAY
    IF U SAY SO
    FOUND YR composed
IF U SAY SO

HOW IZ I incin YR num
    FOUND YR SUM OF num AN 1
IF U SAY SO

HOW IZ I sqrin YR num
    FOUND YR PRODUKT OF num AN num
IF U SAY SO

I HAS A incsqrin ITZ I IZ composin YR incin AN YR sqrin MKAY
VISIBLE I IZ incsqrin YR 10 MKAY BTW, prints 101

I HAS A sqrincin ITZ I IZ composin YR sqrin AN YR incin MKAY
VISIBLE I IZ sqrincin YR 10 MKAY BTW, prints 121

KTHXBYE
```



## Lua


```lua
function compose(f, g) return function(...) return f(g(...)) end end
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Compose = lambda (f, g)->{
            =lambda f, g (x)->f(g(x))
      }
      Add5=lambda (x)->x+5
      Division2=lambda (x)->x/2
      Add5Div2=compose(Division2, Add5)
      Print Add5Div2(15)=10  ' True
}
CheckIt

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Built-in function that takes any amount of function-arguments:

```Mathematica
Composition[f, g][x]
Composition[f, g, h, i][x]
```

gives back:

```Mathematica
f[g[x]]
f[g[h[i[x]]]]
```

Custom function:

```Mathematica
compose[f_, g_][x_] := f[g[x]]
compose[Sin, Cos][r]
```

gives back:

```Mathematica
Sin[Cos[r]]
```

Composition can be done in more than 1 way:

```Mathematica
Composition[f,g,h][x]
f@g@h@x
x//h//g//f
```

all give back:

```Mathematica
f[g[h[x]]]
```

The built-in function has a couple of automatic simplifications:

```Mathematica
Composition[f, Identity, g]
Composition[f, InverseFunction[f], h][x]
```

becomes:

```Mathematica
f[g[x]]
h[x]
```



## Maxima


```maxima
/* built-in */
load(to_poly_solver);

compose_functions([sin, cos]);
/* lambda([%g0],sin(cos(%g0)))*/

/* An implementation, to show a use of buildq */
compose(f, g) := buildq([f, g], lambda([x], f(g(x))));
```



## min

{{works with|min|0.19.3}}
Since min is both [http://concatenative.org/wiki/view/Concatenative%20language concatenative] and homoiconic, function composition is equivalent to list concatenation. Example:

```min
(1 +) (2 *) concat print
```

{{out}}

```txt

(1 + 2 *)

```



## Nemerle


```Nemerle
using System;
using System.Console;
using System.Math;

module Composition
{
    Compose[T](f : T -> T, g : T -> T, x : T) : T
    {
        f(g(x))
    }
    
    Main() : void
    {
        def SinAsin = Compose(Sin, Asin, _);
        WriteLine(SinAsin(0.5));
    }
}
```



## NewLISP



```NewLISP>
 (define (compose f g) (expand (lambda (x) (f (g x))) 'f 'g))
(lambda (f g) (expand (lambda (x) (f (g x))) 'f 'g))
> ((compose sin asin) 0.5)
0.5

```



## Nim


```nim
import future

proc compose[A,B,C](f: A -> B, g: B -> C): A -> C = (x: A) => f(g(x))

proc plustwo(x: int): int = x + 2
proc minustwo(x: int): int = x - 2

var plusminustwo = compose(plustwo, minustwo)
echo plusminustwo(10)
```


=={{header|Objective-C}}==
{{works with|Mac OS X|10.6+}}
We restrict ourselves to functions that take and return one object.


```objc>#include <Foundation/Foundation.h


typedef id (^Function)(id);

// a commodity for "encapsulating" double f(double)
typedef double (*func_t)(double);
Function encapsulate(func_t f) {
  return ^(id x) { return @(f([x doubleValue])); };
}

Function compose(Function a, Function b) {
  return ^(id x) { return a(b(x)); };
}

// functions outside...
double my_f(double x)
{
  return x+1.0;
}

double my_g(double x)
{
  return x*x;
}


int main()
{
  @autoreleasepool {

    Function f = encapsulate(my_f);
    Function g = encapsulate(my_g);
  
    Function composed = compose(f, g);
  
    printf("g(2.0) = %lf\n", [g(@2.0) doubleValue]);
    printf("f(2.0) = %lf\n", [f(@2.0) doubleValue]);
    printf("f(g(2.0)) = %lf\n", [composed(@2.0) doubleValue]);

  }
  return 0;
}
```



## Objeck


```objeck

bundle Default {
  class Test {
    @f : static : (Int) ~ Int;
    @g : static : (Int) ~ Int;
    
    function : Main(args : String[]) ~ Nil {
      compose := Composer(F(Int) ~ Int, G(Int) ~ Int);
      compose(13)->PrintLine();
    }
    
    function : F(a : Int) ~ Int {
      return a + 14;
    }

    function : G(a : Int) ~ Int {
      return a + 15;
    }
    
    function : Compose(x : Int) ~ Int {
      return @f(@g(x));
    }
    
    function : Composer(f : (Int) ~ Int, g : (Int) ~ Int) ~ (Int) ~ Int {
      @f := f;
      @g := g;
      return Compose(Int) ~ Int;
    }
  }
}

```

prints: 42


## OCaml


```ocaml
let compose f g x = f (g x)
```

Example use:

```ocaml
# let compose f g x = f (g x);;
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>
# let sin_asin = compose sin asin;;
val sin_asin : float -> float = <fun>
# sin_asin 0.5;;
- : float = 0.5
```



## Octave



```octave
function r = compose(f, g)
  r = @(x) f(g(x));
endfunction

r = compose(@exp, @sin);
r(pi/3)
```



## Oforth


Oforth uses RPN notation. Function composition of f and g is just calling : 

```Oforth>g f</lang

If a block is needed, a compose function can be implemented : 

```Oforth
: compose(f, g)  #[ g perform f perform ] ;
```

Usage : 

```Oforth
1.2 compose(#asin, #sin) perform
[ 1, 2, 3, 4, 5 ] compose(#[ map(#sqrt) ], #[ filter(#isEven) ]) perform
```

The last line returns : [1.4142135623731, 2]


## Order

Order supplies the built-in function <code>8compose</code> for this purpose. However, a manual implementation might be:

```c>#include <order/interpreter.h


#define ORDER_PP_DEF_8comp ORDER_PP_FN( \
8fn(8F, 8G, 8fn(8X, 8ap(8F, 8ap(8G, 8X)))) )
```

Interpreter limitations mean that local variables containing functions must be called with the <code>8ap</code> operator, but the functions themselves are still first-class values.


## Oz


```oz
declare
  fun {Compose F G}
     fun {$ X}
        {F {G X}}
     end
  end

  SinAsin = {Compose Float.sin Float.asin}
in
  {Show {SinAsin 0.5}}
```



## PARI/GP

{{works with|PARI/GP|2.4.2 and above}}

```parigp
compose(f, g)={
  x -> f(g(x))
};

compose(x->sin(x),x->cos(x)(1)
```


Usage note: In Pari/GP 2.4.3, this can be expressed more succinctly:

```parigp
compose(sin,cos)(1)
```



## Pascal

See [[Function_composition#Delphi | Delphi]]


## Perl


```perl
sub compose {
    my ($f, $g) = @_;

    sub {
        $f -> ($g -> (@_))
    };
}

use Math::Trig;
print compose(sub {sin $_[0]}, \&asin)->(0.5), "\n";
```



## Perl 6

{{works with|rakudo|2018.03}}
The function composition operator is <tt>∘</tt>, U+2218 RING OPERATOR (with a "Texas" version <tt>o</tt> for the Unicode challenged). Here we compose a routine, an operator, and a lambda:

```perl6
sub triple($n) { 3 * $n }
my &f = &triple ∘ &prefix:<-> ∘ { $^n + 2 };
say &f(5); # prints "-21".
```



## Phix

There is not really any direct support for this sort of thing in Phix, but it is all pretty trivial to manage explicitly.

In the following, as it stands, you cannot use constant m in the same way as a routine_id, or pass a standard routine_id to call_composite(), but tagging the ctable entries so that you know precisely what to do with each entry does not sound the least bit difficult to me.

```Phix
sequence ctable = {}

function compose(integer f, integer g)
    ctable = append(ctable,{f,g})
    return length(ctable)   
end function

function call_composite(integer f, atom x)
integer g
    {f,g} = ctable[f]
    return call_func(f,{call_func(g,{x})})
end function

function plus1(atom x)
    return x+1
end function

function halve(atom x)
    return x/2
end function

constant m = compose(routine_id("halve"),routine_id("plus1"))

?call_composite(m,1)    -- displays 1
?call_composite(m,4)    -- displays 2.5
```



## PHP

{{works with|PHP|5.3+}}

```php
<?php
function compose($f, $g) {
  return function($x) use ($f, $g) { return $f($g($x)); };
}

$trim_strlen = compose('strlen', 'trim');
echo $result = $trim_strlen(' Test '), "\n"; // prints 4
?>
```


{{works with|PHP|pre-5.3 and 5.3+}}
works with regular functions as well as functions created by <tt>create_function()</tt>

```php
<?php
function compose($f, $g) {
  return create_function('$x', 'return '.var_export($f,true).'('.var_export($g,true).'($x));');
}

$trim_strlen = compose('strlen', 'trim');
echo $result = $trim_strlen(' Test '), "\n"; // prints 4
?>
```



## PicoLisp


```PicoLisp
(de compose (F G)
   (curry (F G) (X)
      (F (G X)) ) )
```


```PicoLisp
(def 'a (compose inc dec))
(def 'b (compose 'inc 'dec))
(def 'c (compose '((A) (inc A)) '((B) (dec B))))
```


```PicoLisp
: (a 7)
-> 7

: (b 7)
-> 7

: (c 7)
-> 7
```



## PostScript


```PostScript

/compose { % f g -> { g f }
  [ 3 1 roll exch
  % procedures are not executed when encountered directly
  % insert an 'exec' after procedures, but not after operators
  1 index type /operatortype ne { /exec cvx exch } if
  dup type /operatortype ne { /exec cvx } if
  ] cvx
} def

/square { dup mul } def
/plus1  { 1 add } def
/sqPlus1 /square load /plus1 load compose def

```



## Prolog

Works with SWI-Prolog and module lambda, written by <b>Ulrich Neumerkel</b> found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```Prolog
:- use_module(lambda).

compose(F,G, FG) :-
	FG =  \X^Z^(call(G,X,Y), call(F,Y,Z)).

```

{{out}}

```txt
 ?- compose(sin, asin, F), call(F, 0.5, Y).
F = \_G4586^_G4589^ (call(asin,_G4586,_G4597),call(sin,_G4597,_G4589)),
Y = 0.5.

```



## PowerShell

You can simply call g inside f like this:

```PowerShell

function g ($x) {
    $x + $x
}
function f ($x) {
    $x*$x*$x
}  
f (g 1)

```


Or g and f can become paramaters of a new function fg


```PowerShell

function fg (${function:f}, ${function:g}, $x) {
    f (g $x)
}
fg f g 1

```


In both cases the answer is:

```txt
 8 
```



## PureBasic


```PureBasic
;Declare how our function looks like
Prototype.i Func(Arg.i)  

; Make a procedure that composes any functions of type "Func"
Procedure Compose(*a.Func,*b.Func, x)
  ProcedureReturn *a(*b(x))
EndProcedure

; Just a procedure fitting "Func"
Procedure f(n)
  ProcedureReturn 2*n
EndProcedure

; Yet another procedure fitting "Func"
Procedure g(n)
  ProcedureReturn n+1
EndProcedure

;- Test it
X=Random(100)
Title$="With x="+Str(x)
Body$="Compose(f(),g(), x) ="+Str(Compose(@f(),@g(),X))
MessageRequester(Title$,Body$)
```



## Purity


```Purity

data compose = f => g => $f . $g

```



## Python


### Simple composition of two functions


```python
compose = lambda f, g: lambda x: f( g(x) )
```

Example use:

```python>>>
 compose = lambda f, g: lambda x: f( g(x) )
>>> from math import sin, asin
>>> sin_asin = compose(sin, asin)
>>> sin_asin(0.5)
0.5
>>>
```



Or, expanding slightly:
{{Works with|Python|3}}

```python
from math import (acos, cos, asin, sin)


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g, f):
    '''Right to left function composition.'''
    return lambda x: g(f(x))


# main :: IO ()
def main():
    '''Test'''

    print(list(map(
        lambda f: f(0.5),
        zipWith(compose)(
            [sin, cos, lambda x: x ** 3.0]
        )([asin, acos, lambda x: x ** (1 / 3.0)])
    )))


# GENERIC FUNCTIONS ---------------------------------------


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    '''A list constructed by zipping with a
       custom function, rather than with the
       default tuple constructor.'''
    return lambda xs: lambda ys: (
        map(f, xs, ys)
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[0.49999999999999994, 0.5000000000000001, 0.5000000000000001]
```



### Multiple composition

Nested composition of several functions can be streamlined by using '''functools.reduce'''.

{{Works with|Python|3}}

```python
from functools import reduce
from numbers import Number
import math


def main():
    '''Test'''

    f = composeList([
        lambda x: x / 2,
        succ,
        math.sqrt
    ])

    print(
        f(5)
    )


# GENERIC FUNCTIONS ---------------------------------------


# composeList :: [(a -> a)] -> (a -> a)
def composeList(fs):
    '''Composition, from right to left,
       of a series of functions.'''
    return lambda x: reduce(
        lambda a, f: f(a),
        fs[::-1],
        x
    )


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value. For numeric types, (1 +).'''
    return 1 + x if isinstance(x, Number) else (
        chr(1 + ord(x))
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
1.618033988749895
```



## Qi

Qi supports partial applications, but only when calling a function with one argument.

```qi

(define compose
  F G -> (/. X
             (F (G X))))

((compose (+ 1) (+ 2)) 3)   \ (Outputs 6) \

```


Alternatively, it can be done like this:


```qi

(define compose F G X -> (F (G X)))

(((compose (+ 1)) (+ 2)) 3)  \ (Outputs 6) \

```



## R


```R
compose <- function(f,g) function(x) { f(g(x)) }
r <- compose(sin, cos)
print(r(.5))
```



## Racket


```racket

(define (compose f g)
  (lambda (x) (f (g x))))

```


Also available as a <tt>compose1</tt> builtin, and a more general <tt>compose</tt> where one function can produce multiple arguments that are sent the the next function in the chain.  (Note however that this is rarely desired.)


## REBOL


```REBOL
REBOL [
	Title: "Functional Composition"
	URL: http://rosettacode.org/wiki/Functional_Composition
]

; "compose" means something else in REBOL, therefore I use a 'compose-functions name. 

compose-functions: func [
    {compose the given functions F and G}
    f [any-function!]
    g [any-function!]
] [
    func [x] compose [(:f) (:g) x]
]
```


Functions "foo" and "bar" are used to prove that composition
actually took place by attaching their signatures to the result.


```REBOL
foo: func [x] [reform ["foo:" x]]
bar: func [x] [reform ["bar:" x]]

foo-bar: compose-functions :foo :bar
print ["Composition of foo and bar:"  mold foo-bar "test"]

sin-asin: compose-functions :sine :arcsine
print [crlf "Composition of sine and arcsine:" sin-asin 0.5]
```


{{out}}

```txt
Composition of foo and bar: "foo: bar: test"

Composition of sine and arcsine: 0.5
```



## REXX


```rexx
compose: procedure;  parse arg f,g,x;    interpret  'return'  f"("  g'('  x  "))"

exit        /*control should never gets here,  but this was added just in case.*/
```



## Ring


```ring

# Project : Function composition

sumprod = func1(:func2,2,3)
see sumprod + nl

func func1(func2,x,y)
        temp = call func2(x,y)
        res = temp + x + y
        return res

func func2(x,y)
        res = x * y
        return res

```

Output:

```txt

11

```



## Ruby

This <tt>compose</tt> method gets passed two Method objects or Proc objects

```ruby
def compose(f,g)
  lambda {|x| f[g[x]]}
end
s = compose(Math.method(:sin), Math.method(:cos))
p s[0.5]  # => 0.769196354841008

# verify
p Math.sin(Math.cos(0.5))  # => 0.769196354841008
```



## Rust

In order to return a closure (anonymous function) in Stable Rust, it must be wrapped in a layer of indirection via a heap allocation. However, there is a feature coming down the pipeline (currently available in Nightly) which makes this possible. Both of the versions below are in the most general form i.e. their arguments may be functions or closures with the only restriction being that the output of <code>g</code> is the same type as the input of <code>f</code>.


### Stable

Function is allocated on the heap and is called via dynamic dispatch

```rust
fn compose<'a,F,G,T,U,V>(f: F, g: G) -> Box<Fn(T) -> V + 'a>
    where F: Fn(U) -> V + 'a,
          G: Fn(T) -> U + 'a,
{
   Box::new(move |x| f(g(x)))
}
```



### Nightly

Function is returned on the stack and is called via static dispatch (monomorphized)

```rust
#![feature(conservative_impl_trait)]
fn compose<'a,F,G,T,U,V>(f: F, g: G) -> impl Fn(T) -> V + 'a
    where F: Fn(U) -> V + 'a,
          G: Fn(T) -> U + 'a,
{
   move |x| f(g(x))
}
```



## Scala


```scala
def compose[A](f: A => A, g: A => A) = { x: A => f(g(x)) }

def add1(x: Int) = x+1
val add2 = compose(add1, add1)
```


We can achieve a more natural style by creating a container class for composable functions, which provides 
the compose method 'o':


```scala
class Composable[A](f: A => A) {
  def o (g: A => A) = compose(f, g)
}

implicit def toComposable[A](f: A => A) = new Composable(f)

val add3 = (add1 _) o add2
```



```txt

> (add2 o add3)(37)
res0: Int = 42

```



## Scheme


```scheme
(define (compose f g) (lambda (x) (f (g x))))

;; or:

(define ((compose f g) x) (f (g x)))

;; or to compose an arbitrary list of 1 argument functions:

(define-syntax compose
  (lambda (x)
    (syntax-case x ()
      ((_) #'(lambda (y) y))
      ((_ f) #'f)
      ((_ f g h ...)  #'(lambda (y) (f ((compose g h ...) y)))))))


```

Example:

```scheme

(display ((compose sin asin) 0.5))
(newline)
```

{{out}}
<lang>0.5
```



## Sidef


```ruby
func compose(f, g) {
    func(x) { f(g(x)) }
}

var fg = compose(func(x){ sin(x) }, func(x){ cos(x) })
say fg(0.5)      # => 0.76919635484100842185251475805107
```



## Slate

Function (method) composition is standard:

```slate
[| :x | x + 1] ** [| :x | x squared] applyTo: {3}
```



## Smalltalk


```smalltalk
| composer fg |
composer := [ :f :g | [ :x | f value: (g value: x) ] ].
fg := composer value: [ :x | x + 1 ]
               value: [ :x | x * x ].

(fg value:3) displayNl.
```



## Standard ML

This is already defined as the '''o''' operator in Standard ML.

```sml
fun compose (f, g) x = f (g x)
```

Example use:

```sml
- fun compose (f, g) x = f (g x);
val compose = fn : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
- val sin_asin = compose (Math.sin, Math.asin);
val sin_asin = fn : real -> real
- sin_asin 0.5;
val it = 0.5 : real
```



## SuperCollider

has a function composition operator (the message `<>`):

```SuperCollider

f = { |x| x + 1 };
g = { |x| x * 2 };
h = g <> f;
h.(8); // returns 18

```




## Swift


```swift
func compose<A,B,C>(f: (B) -> C, g: (A) -> B) -> (A) -> C {
  return { f(g($0)) }
}

let sin_asin = compose(sin, asin)
println(sin_asin(0.5))
```

{{out}}

```txt

0.5

```



## Tcl

{{works with|Tcl|8.5}}
This creates a <code>compose</code> procedure that returns an anonymous function term that should be expanded as part of application to its argument.

```tcl
package require Tcl 8.5
namespace path {::tcl::mathfunc}

proc compose {f g} {
    list apply [list {f g x} {{*}$f [{*}$g $x]}] $f $g]
}

set sin_asin [compose sin asin]
{*}$sin_asin 0.5 ;# ==> 0.5
{*}[compose abs int] -3.14 ;# ==> 3
```




## TypeScript


```TypeScript

function compose<T, U, V> (fn1: (input: T) => U, fn2: (input: U) => V){
    return function(value: T) {
        return fn2(fn1(value))
    } 
}

function size (s: string): number { return s.length; }

function isEven(x: number): boolean { return x % 2 === 0; }

const evenSize = compose(size, isEven);

console.log(evenSize("ABCD")) // true
console.log(evenSize("ABC")) // false

```



## UNIX Shell

Each function takes its argument from standard input, 
and puts its result to standard output. 
Then the composition of ''f'' and ''g'' is a shell pipeline, <code>c() { g | f; }</code>.

{{works with|Bourne Shell}}

```bash
compose() {
	eval "$1() { $3 | $2; }"
}

downvowel() { tr AEIOU aeiou; }
upcase() { tr a-z A-Z; }
compose c downvowel upcase
echo 'Cozy lummox gives smart squid who asks for job pen.' | c
# => CoZY LuMMoX GiVeS SMaRT SQuiD WHo aSKS FoR JoB PeN.
```


{{works with|Bourne Again SHell}}
This solution uses no external tools, just Bash itself.


```bash
  
#compose a new function consisting of the application of 2 unary functions

             compose () { f="$1"; g="$2"; x="$3"; "$f" "$("$g" "$x")";} 


chartolowervowel() 
# Usage:  chartolowervowel "A" --> "a"

#Based on a to_upper script in Chris F. A. Johnson's book Pro Bash Programming Ch7. String Manipulation
#(with minor tweaks to use local variables and return the value of the converted character
#http://cfajohnson.com/books/cfajohnson/pbp/
#highly recommended I have a copy and have bought another for a friend
{  
 
   local LWR="";
     
	   case $1  in
                          A*) _LWR=a ;;
#                         B*) _LWR=b ;;
#			  C*) _LWR=c ;;
#			  D*) _LWR=d ;;
			  E*) _LWR=e ;;
#			  F*) _LWR=f ;;
#			  G*) _LWR=g ;;
#			  H*) _LWR=h ;;
			  I*) _LWR=i ;;
#			  J*) _LWR=j ;;
#			  K*) _LWR=k ;;
#			  L*) _LWR=L ;;
#			  M*) _LWR=m ;;
#			  N*) _LWR=n ;;
			  O*) _LWR=o ;;
#			  P*) _LWR=p ;;
#			  Q*) _LWR=q ;;
#			  R*) _LWR=r ;;
#			  S*) _LWR=s ;;
#			  T*) _LWR=t ;;
			  U*) _LWR=u ;;
#			  V*) _LWR=v ;;
#			  W*) _LWR=w ;;
#			  X*) _LWR=x ;;
#			  Y*) _LWR=y ;;
#			  Z*) _LWR=z ;;
			   *) _LWR=${1%${1#?}} ;;      
		  esac;
		echo "$_LWR";
                               }   

strdownvowel() 
# Usage:  strdownvowel "STRING" --> "STRiNG"

#Based on an upword script in Chris F. A. Johnson's book Pro Bash Programming Ch7. String Manipulation
#(with minor tweaks to use local variables and return the value of the converted string
#http://cfajohnson.com/books/cfajohnson/pbp/
#highly recommended I have a copy and have bought another for a friend

{
  local _DWNWORD=""
  local word="$1"
  while [ -n "$word" ] ## loop until nothing is left in $word
  do
     chartolowervowel "$word" >> /dev/null
     _DWNWORD=$_DWNWORD$_LWR
     word=${word#?}  ## remove the first character from $word
	  
  done
  Echo "$_DWNWORD"
}
 



chartoupper() 
# Usage:  chartoupper "s" --> "S"

#From Chris F. A. Johnson's book Pro Bash Programming Ch7. String Manipulation
#(with minor tweaks to use local variables and return the value of the converted character
#http://cfajohnson.com/books/cfajohnson/pbp/
#highly recommended I have a copy and have bought another for a friend
 { 
     local UPR="";
	 
       case $1  in
                          a*) _UPR=A ;;
                          b*) _UPR=B ;;
			  c*) _UPR=C ;;
			  d*) _UPR=D ;;
			  e*) _UPR=E ;;
			  f*) _UPR=F ;;
			  g*) _UPR=G ;;
			  h*) _UPR=H ;;
			  i*) _UPR=I ;;
			  j*) _UPR=J ;;
			  k*) _UPR=K ;;
			  l*) _UPR=L ;;
			  m*) _UPR=M ;;
			  n*) _UPR=N ;;
			  o*) _UPR=O ;;
			  p*) _UPR=P ;;
			  q*) _UPR=Q ;;
			  r*) _UPR=R ;;
			  s*) _UPR=S ;;
			  t*) _UPR=T ;;
			  u*) _UPR=U ;;
			  v*) _UPR=V ;;
			  w*) _UPR=W ;;
			  x*) _UPR=X ;;
			  y*) _UPR=Y ;;
			  z*) _UPR=Z ;;
			   *) _UPR=${1%${1#?}} ;;      
		  esac;
		echo "$_UPR";
		              } 

strupcase() 
# Usage:  strupcase "string" --> "STRING"

#Based on an upword script in Chris F. A. Johnson's book Pro Bash Programming Ch7. String Manipulation
#(with minor tweaks to use local variables and return the value of the converted string
#http://cfajohnson.com/books/cfajohnson/pbp/
#highly recommended I have a copy and have bought another for a friend

{
  local _UPWORD=""
  local word="$1"
  while [ -n "$word" ] ## loop until nothing is left in $word
  do
     chartoupper "$word" >> /dev/null
     _UPWORD=$_UPWORD$_UPR
     word=${word#?}  ## remove the first character from $word
	  
  done
  Echo "$_UPWORD"
}

compose  strdownvowel strupcase "Cozy lummox gives smart squid who asks for job pen." 
# --> CoZY LuMMoX GiVeS SMaRT SQuiD WHo aSKS FoR JoB PeN.
```



=
## es
=
With shell pipelines:


```es
fn compose f g {
	result @ {$g | $f}
}

fn downvowel {tr AEIOU aeiou}
fn upcase {tr a-z A-Z}
fn-c = <={compose $fn-downvowel $fn-upcase}
echo 'Cozy lummox gives smart squid who asks for job pen.' | c
# => CoZY LuMMoX GiVeS SMaRT SQuiD WHo aSKS FoR JoB PeN.
```


With function arguments:


```es
fn compose f g {
	result @ x {result <={$f <={$g $x}}}
}

fn downvowel x {result `` '' {tr AEIOU aeiou <<< $x}}
fn upcase x {result `` '' {tr a-z A-Z <<< $x}}
fn-c = <={compose $fn-downvowel $fn-upcase}
echo <={c 'Cozy lummox gives smart squid who asks for job pen.'}
# => CoZY LuMMoX GiVeS SMaRT SQuiD WHo aSKS FoR JoB PeN.
```



## Unlambda


```txt

``s`ksk

```



## Ursala

Functional composition is a built in operation expressible as f+g
for functions f and g, hence hardly worth defining. However, it could
be defined without using the operator like this.

```Ursala
compose("f","g") "x" = "f" "g" "x"
```

test program:

```Ursala
#import nat
#cast %n

test =  compose(successor,double) 3
```

{{out}}

```txt
7
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Cannot do function composition. Function definitions are dynamic, but functions cannot be passed as values. No closures. -->
{{omit from|Fortran}}


## VBScript

I'm not convinced that this is really a 'closure'. It looks to me more like a cute trick with Eval().

'''Implementation'''

```vb

option explicit
class closure

	private composition
	
	sub compose( f1, f2 )
		composition = f2 & "(" & f1 & "(p1))"
	end sub
	
	public default function apply( p1 )
		apply = eval( composition )
	end function
	
	public property get formula
		formula = composition
	end property
	
end class 

```


'''Invocation'''

```vb

dim c
set c = new closure

c.compose "ucase", "lcase"
wscript.echo c.formula
wscript.echo c("dog")

c.compose "log", "exp"
wscript.echo c.formula
wscript.echo c(12.3)

function inc( n )
	inc = n + 1
end function

c.compose "inc", "inc"
wscript.echo c.formula
wscript.echo c(12.3)

function twice( n )
	twice = n * 2
end function

c.compose "twice", "inc"
wscript.echo c.formula
wscript.echo c(12.3)

```


{{out}}

```txt

lcase(ucase(p1))
dog
exp(log(p1))
12.3
inc(inc(p1))
14.3
inc(twice(p1))
25.6

```



## WDTE


The simplest way is with a lambda:


```WDTE>let compose f g =
 (@ c x => g x -> f);
```


Alternatively, you can take advantage of partial function calls:


```WDTE>let compose f g x =
 g x -> f;
```


Both can be used as follows:


```WDTE
(compose (io.writeln io.stdout) !) true;
```


Output:


```WDTE>false</lang



## Wortel

The <code>@</code> operator applied to a array literal will compose the functions in the array and <code>^</code> with a group literal will do the same, but also quotes operators.

```wortel
! @[f g] x ; f(g(x))
```


```wortel
! ^(f g) x ; f(g(x))
```

Defining the <code>compose</code> function

```wortel
@var compose &[f g] &x !f!g x
```



## zkl


```zkl
Utils.Helpers.fcomp('+(1),'*(2))(5) //-->11
```

Which is implemented with a closure (.fp1), which fixes the second paramter

```zkl
fcn fcomp(f,g,h,etc){
   { fcn(x,hgf){ T(x).pump(Void,hgf.xplode()) }.fp1(vm.arglist.reverse()); }
```



## ZX Spectrum Basic

DEF FN commands can be nested, making this appear trivial:

```zxbasic
10 DEF FN f(x)=SQR x
20 DEF FN g(x)=ABS x
30 DEF FN c(x)=FN f(FN g(x))
40 PRINT FN c(-4)
```

Which gets you f(g(x)), for sure. But if you want g(f(x)) you need to DEF a whole new FN. Instead we can pass the function names as strings to a new function and numerically evaluate the string:

```zxbasic
10 DEF FN f(x)=SQR x
20 DEF FN g(x)=ABS x
30 DEF FN c(a$,b$,x)=VAL ("FN "+a$+"(FN "+b$+"(x))")
40 PRINT FN c("f","g",-4)
50 PRINT FN c("g","f",-4)
```

{{out}}

```txt
2

A Invalid argument, 50:1
```

