+++
title = "Currying"
description = ""
date = 2019-08-12T14:37:08Z
aliases = []
[extra]
id = 13238
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Create a simple demonstrative example of [[wp:Currying|Currying]] in a specific language.

Add any historic details as to how the feature made its way into the language.
<!-- from: http://en.wikipedia.org/w/index.php?title=Currying&direction=prev&oldid=142127294 -->
## Aime

Curry a function printing an integer, on a given number of characters, with commas inserted every given number of digits, with a given number of digits, in a given base:

```aime
ri(list l)
{
    l[0] = apply.apply(l[0]);
}
curry(object o)
{
    (o.__count - 1).times(ri, list(o));
}
main(void)
{
    o_wbfxinteger.curry()(16)(3)(12)(16)(1 << 30);
    0;
}

```

```txt
 000,040,000,000
```



## ALGOL 68

In 1968 [[wp:Charles H. Lindsey|C.H. Lindsey]] proposed for '''partial parametrisation''' for ALGOL 68, this is implemented as an extension in [[wp:ALGOL 68G]].

```algol68
# Raising a function to a power #

MODE FUN = PROC (REAL) REAL;
PROC pow = (FUN f, INT n, REAL x) REAL: f(x) ** n;
OP ** = (FUN f, INT n) FUN: pow (f, n, );

# Example: sin (3 x) = 3 sin (x) - 4 sin^3 (x) (follows from DeMoivre's theorem) #

REAL x = read real;
print ((new line, sin (3 * x),  3 *  sin (x) - 4 * (sin ** 3) (x)))
```




## AppleScript


The nearest thing to a first-class function in AppleScript is a 'script' in which a 'handler' (with some default or vanilla name like 'call' or 'lambda' is embedded). First class use of an ordinary 2nd class 'handler' function requires 'lifting' it into an enclosing script – a process which can be abstracted to a general mReturn function.


```AppleScript
-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry


-- TESTS ----------------------------------------------------------------------

-- add :: Num -> Num -> Num
on add(a, b)
    a + b
end add

-- product :: Num -> Num -> Num
on product(a, b)
    a * b
end product

-- Test 1.
curry(add)

-->  «script»


-- Test 2.
curry(add)'s |λ|(2)

--> «script»


-- Test 3.
curry(add)'s |λ|(2)'s |λ|(3)

--> 5


-- Test 4.
map(curry(product)'s |λ|(7), enumFromTo(1, 10))

--> {7, 14, 21, 28, 35, 42, 49, 56, 63, 70}


-- Combined:
{curry(add), ¬
    curry(add)'s |λ|(2), ¬
    curry(add)'s |λ|(2)'s |λ|(3), ¬
    map(curry(product)'s |λ|(7), enumFromTo(1, 10))}

--> {«script», «script», 5, {7, 14, 21, 28, 35, 42, 49, 56, 63, 70}}


-- GENERIC FUNCTIONS ----------------------------------------------------------

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

```txt
{«script», «script», 5, {7, 14, 21, 28, 35, 42, 49, 56, 63, 70}}
```



## C


```C

#include<stdarg.h>
#include<stdio.h>

long int factorial(int n){
	if(n>1)
		return n*factorial(n-1);
	return 1;
}

long int sumOfFactorials(int num,...){
	va_list vaList;
	long int sum = 0;

	va_start(vaList,num);

	while(num--)
		sum += factorial(va_arg(vaList,int));

	va_end(vaList);

	return sum;
}

int main()
{
	printf("\nSum of factorials of [1,5] : %ld",sumOfFactorials(5,1,2,3,4,5));
	printf("\nSum of factorials of [3,5] : %ld",sumOfFactorials(3,3,4,5));
	printf("\nSum of factorials of [1,3] : %ld",sumOfFactorials(3,1,2,3));

	return 0;
}

```

Output:

```txt

C:\rosettaCode>curry.exe

Sum of factorials of [1,5] : 153
Sum of factorials of [3,5] : 150
Sum of factorials of [1,3] : 9

```



## C#

This shows how to create syntactically natural currying functions in [[C sharp|C#]].

```c#
public delegate int Plus(int y);
public delegate Plus CurriedPlus(int x);
public static CurriedPlus plus =
      delegate(int x) {return delegate(int y) {return x + y;};};
static void Main()
{
    int sum = plus(3)(4); // sum = 7
    int sum2= plus(2)(plus(3)(4)) // sum2 = 9
}
```



## C++

Currying may be achieved in [[C++]] using the [[wp:Standard Template Library|Standard Template Library]] function object adapters (<code>binder1st</code> and <code>binder2nd</code>), and more generically using the [[wp:Boost library|Boost]] <code>bind</code> mechanism.


## Ceylon

```ceylon
shared void run() {

    function divide(Integer x, Integer y) => x / y;

    value partsOf120 = curry(divide)(120);

    print("half of 120 is ``partsOf120(2)``
           a third is ``partsOf120(3)``
           and a quarter is ``partsOf120(4)``");
}
```



## Clojure


```clojure
(def plus-a-hundred (partial + 100))
(assert (=
           (plus-a-hundred 1)
           101))

```



## Common Lisp


```lisp
(defun curry (function &rest args-1)
  (lambda (&rest args-2)
    (apply function (append args-1 args-2))))

```


Usage:

```lisp

(funcall (curry #'+ 10) 10)

20

```



## D


```d
void main() {
    import std.stdio, std.functional;

    int add(int a, int b) {
        return a + b;
    }

    alias add2 = partial!(add, 2);
    writeln("Add 2 to 3: ", add(2, 3));
    writeln("Add 2 to 3 (curried): ", add2(3));
}
```

```txt
Add 2 to 3: 5
Add 2 to 3 (curried): 5
```



## EchoLisp

[[EchoLisp]] has native support for curry, which is implemented thru closures, as shown in [[CommonLisp]] .
<lang>
;;
;; curry functional definition
;; (define (curry proc . left-args) (lambda right-args (apply proc (append left-args right-args))))
;;
;; right-curry
;; (define (rcurry proc . right-args) (lambda left-args (apply proc (append left-args right-args))))
;;

(define add42 (curry + 42))
(add42 666) → 708

(map (curry cons 'simon) '( gallubert garfunkel et-merveilles))
   →   ((simon . gallubert) (simon . garfunkel) (simon . et-merveilles))
(map (rcurry cons 'simon) '( gallubert garfunkel et-merveilles))
   →   ((gallubert . simon) (garfunkel . simon) (et-merveilles . simon))

;Implementation : result of currying :
(curry * 2 3 (+ 2 2))
    → (λ _#:g1004 (#apply-curry #* (2 3 4)  _#:g1004))

```



## Eero


```objc>#import <stdio.h


int main()

  addN := (int n)
    int adder(int x)
      return x + n
    return adder

  add2 := addN(2)

  printf( "Result = %d\n", add2(7) )

  return 0

```

Alternative implementation (there are a few ways to express blocks/lambdas):

```objc>#import <stdio.h


int main()

  addN := (int n)
    return (int x | return x + n)

  add2 := addN(2)

  printf( "Result = %d\n", add2(7) )

  return 0

```





## Eiffel

[[Eiffel]] has direct support for lambda expressions and hence currying through "inline agents". If f is a function with two arguments, of signature (''X'' × ''Y'') → ''Z'' then its curried version is obtained by simply writing

    g (x: X): FUNCTION [ANY, TUPLE [Y], Z]
        '''do'''
            '''Result''' := '''agent''' (closed_x: X; y: Y): Z
               '''do'''
                  '''Result''' := f (closed_x, y)
               '''end''' (x, ?)
        '''end'''

where FUNCTION [ANY, TUPLE [Y], Z] denotes the type ''Y'' → ''Z'' (agents taking as argument a tuple with a single argument of type Y and returning a result of type Z), which is indeed the type of the agent expression used on the next-to-last line to define the "Result" of g.


## Erlang


There are three solutions provided for this problem. The simple version is using anonymous functions as other examples of other languages do. The second solution corresponds to the definition of currying. It takes a function of a arity ''n'' and applies a given argument, returning then a function of arity ''n-1''. The solution provided uses metaprogramming facilities to create the new function. Finally, the third solution is a generalization that allows to curry any number of parameters and in a given order.


```erlang

-module(currying).

-compile(export_all).

% Function that curry the first or the second argument of a given function of arity 2

curry_first(F,X) ->
    fun(Y) -> F(X,Y) end.

curry_second(F,Y) ->
    fun(X) -> F(X,Y) end.

% Usual curry

curry(Fun,Arg) ->
	case erlang:fun_info(Fun,arity) of
		{arity,0} ->
			erlang:error(badarg);
		{arity,ArityFun} ->
			create_ano_fun(ArityFun,Fun,Arg);
		_ ->
			erlang:error(badarg)
	end.

create_ano_fun(Arity,Fun,Arg) ->
	Pars =
		[{var,1,list_to_atom(lists:flatten(io_lib:format("X~p", [N])))}
		 || N <- lists:seq(2,Arity)],
	Ano =
		{'fun',1,
			{clauses,[{clause,1,Pars,[],
				[{call,1,{var,1,'Fun'},[{var,1,'Arg'}] ++ Pars}]}]}},
	{_,Result,_} = erl_eval:expr(Ano, [{'Arg',Arg},{'Fun',Fun}]),
	Result.

% Generalization of the currying

curry_gen(Fun,GivenArgs,PosGivenArgs,PosParArgs) ->
    Pos = PosGivenArgs ++ PosParArgs,
    case erlang:fun_info(Fun,arity) of
        {arity,ArityFun} ->
            case ((length(GivenArgs) + length(PosParArgs)) == ArityFun) and
                 (length(GivenArgs) == length(PosGivenArgs)) and
                 (length(Pos) == sets:size(sets:from_list(Pos))) of
                true ->
                    fun(ParArgs) ->
                        case length(ParArgs) == length(PosParArgs) of
                            true ->
                                Given = lists:zip(PosGivenArgs,GivenArgs),
                                Pars = lists:zip(PosParArgs,ParArgs),
                                {_,Args} = lists:unzip(lists:sort(Given ++ Pars)),
                                erlang:apply(Fun,Args);
                            false ->
                                erlang:error(badarg)
                        end
                    end;
                false ->
                    erlang:error(badarg)
            end;
        _ ->
            erlang:error(badarg)
    end.

```



Output (simple version):

```txt

> (currying:curry_first(fun(X,Y) -> X + Y end,3))(2).
5
> (currying:curry_first(fun(X,Y) -> X - Y end,3))(2).
1
> (currying:curry_second(fun(X,Y) -> X - Y end,3))(2).
-1

```


Output (usual curry):

```txt

> G = fun(A,B,C)-> (A + B) * C end.
#Fun<erl_eval.18.90072148>
> (currying:curry(G,3))(2,1).
5
> (currying:curry(currying:curry(G,3),2))(1).
5
> (currying:curry(currying:curry(currying:curry(G,3),2),1))().
5

```


Output (generalized version):

```txt

> (currying:curry_gen(fun(A,B,C,D) -> (A + B) * (C - D) end,[1.0,0.0],[1,2],[3,4]))([2.0,5.0]).
-3.0
> (currying:curry_gen(fun(A,B,C,D) -> (A + B) * (C - D) end,[1.0,0.0],[4,2],[1,3]))([2.0,5.0]).
8.0
> (currying:curry_gen(fun(A,B,C) -> (A + B) * C end,[1.0,0.0],[3,2],[1]))([5.0]).
5.0

```


=={{header|F Sharp|F#}}==
F# is largely based on ML and has a built-in natural method of defining functions that are curried:


```fsharp
let addN n = (+) n
```



```fsharp>
 let add2 = addN 2;;

val add2 : (int -> int)

> add2;;
val it : (int -> int) = <fun:addN@1>
> add2 7;;
val it : int = 9
```



## Factor


```factor
IN: scratchpad 2 [ 3 + ] curry

--- Data stack:
[ 2 3 + ]
IN: scratchpad call

--- Data stack:
5
```

Currying doesn't need to be an atomic operation. <tt>compose</tt> lets you combine quotations.

```factor
IN: scratchpad [ 3 4 ] [ 5 + ] compose

--- Data stack:
[ 3 4 5 + ]
IN: scratchpad call

--- Data stack:
3
9
```


You can even treat quotations as sequences.

```factor
IN: scratchpad { 1 2 3 4 5 } [ 1 + ] { 2 / } append map

--- Data stack:
{ 1 1+1/2 2 2+1/2 3 }
```


Finally, fried quotations are often clearer than using <tt>curry</tt> and <tt>compose</tt>. Use <tt>_</tt> to take objects from the stack and slot them into the quotation.

```factor
USE: fry
IN: scratchpad 2 3 '[ _ _ + ]

--- Data stack:
[ 2 3 + ]
```


Use <tt>@</tt> to insert the contents of a quotation into another quotation.

```factor
IN: scratchpad { 1 2 3 4 5 } [ 1 + ] '[ 2 + @ ] map

--- Data stack:
{ 4 5 6 7 8 }
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Currying this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

```forth
: curry ( x xt1 -- xt2 )
  swap 2>r :noname r> postpone literal r> compile, postpone ; ;

5 ' + curry constant +5
5 +5 execute .
7 +5 execute .
```


```txt
10 12
```



## FreeBASIC

FreeBASIC is not a functional language and does not support either currying or nested functions/lambdas which are typically used by otherwise imperative languages to implement the former. The nearest I could get to currying using the features which the language does support is the following:

```freebasic
' FB 1.05.0 Win64

Type CurriedAdd
  As Integer i
  Declare Function add(As Integer) As Integer
End Type

Function CurriedAdd.add(j As Integer) As Integer
  Return i + j
End Function

Function add (i As Integer) as CurriedAdd
   Return Type<CurriedAdd>(i)
End Function

Print "3 + 4 ="; add(3).add(4)
Print "2 + 6 ="; add(2).add(6)
Sleep
```


```txt

3 + 4 = 7
2 + 6 = 8

```



## Go

Go has had [http://golang.org/ref/spec#Function_literals function literals]
and [http://golang.org/ref/spec#Method_expressions method expressions]
since before Go 1.0.
[http://golang.org/ref/spec#Method_values Method values] were added
in [http://golang.org/doc/go1.1#method_values Go 1.1].

```go
package main

import (
        "fmt"
        "math"
)

func PowN(b float64) func(float64) float64 {
        return func(e float64) float64 { return math.Pow(b, e) }
}

func PowE(e float64) func(float64) float64 {
        return func(b float64) float64 { return math.Pow(b, e) }
}

type Foo int

func (f Foo) Method(b int) int {
        return int(f) + b
}

func main() {
        pow2 := PowN(2)
        cube := PowE(3)

        fmt.Println("2^8 =", pow2(8))
        fmt.Println("4³ =", cube(4))

        var a Foo = 2
        fn1 := a.Method   // A "method value", like currying 'a'
        fn2 := Foo.Method // A "method expression", like uncurrying

        fmt.Println("2 + 2 =", a.Method(2)) // regular method call
        fmt.Println("2 + 3 =", fn1(3))
        fmt.Println("2 + 4 =", fn2(a, 4))
        fmt.Println("3 + 5 =", fn2(Foo(3), 5))
}
```

[http://play.golang.org/p/0YL9YTe-9V Run on the Go Playground.]


## Groovy

=== curry()  ===
This method can be applied to any Groovy closure or method reference (demonstrated here with closures). The arguments given to the ''curry()'' method are applied to the original (invoking) method/closure. The "curry()" method returns a closure as it's result. The arguments on the "curry()" method are passed, in their specified order, as the first (left-most) arguments of the original method/closure. The remaining, as yet unspecified arguments of the original method/closure, form the argument list of the resulting closure.

Example:

```groovy
def divide = { Number x, Number y ->
  x / y
}

def partsOf120 = divide.curry(120)

println "120: half: ${partsOf120(2)}, third: ${partsOf120(3)}, quarter: ${partsOf120(4)}"
```


Results:

```txt
120: half: 60, third: 40, quarter: 30
```


=== rcurry()  ===
This method can be applied to any Groovy closure or method reference. The arguments given to the ''rcurry()'' method are applied to the original (invoking) method/closure. The "rcurry()" method returns a closure as it's result. The arguments on the "rcurry()" method are passed, in their specified order, as the last (right-most) arguments of the original method/closure. The remaining, as yet unspecified arguments of the original method/closure, form the argument list of the resulting closure.

Example (using the same "divide()" closure as before):

```groovy
def half = divide.rcurry(2)
def third = divide.rcurry(3)
def quarter = divide.rcurry(4)

println "30: half: ${half(30)}; third: ${third(30)}, quarter: ${quarter(30)}"
```


Results:

```txt
30: half: 15; third: 10, quarter: 7.5
```



###  History

I invite any expert on the history of the Groovy language to correct this if necessary. Groovy is a relatively recent language, with alphas and betas first appearing on the scene in 2003 and a 1.0 release in 2007. To the best of my understanding currying has been a part of the language from the outset.


## Haskell

Likewise in Haskell, function type signatures show the currying-based structure of functions (note: "
```haskell
\ ->
```
" is Haskell's syntax for anonymous functions, in which the sign
```haskell>\</lang
 has been chosen for its resemblance to the Greek letter λ (lambda); it is followed by a list of space-separated arguments, and the arrow
```haskell
->
```
 separates the arguments list from the function body)

    Prelude> let plus = \x y -> x + y
    Prelude> :type plus
    plus :: Integer -> Integer -> Integer
    Prelude> plus 3 5
    8

and currying functions is trivial

    Prelude> let plus5 = plus 5
    Prelude> :type plus5
    plus5 :: Integer -> Integer
    Prelude> plus5 3
    8

In fact, the Haskell definition
```haskell
\x y -> x + y
```
 is merely [[wp:syntactic sugar|syntactic sugar]] for
```haskell
\x -> \y -> x + y
```
, which has exactly the same type signature:

    Prelude> let nested_plus = \x -> \y -> x + y
    Prelude> :type nested_plus
    nested_plus :: Integer -> Integer -> Integer


## Hy


```hy
(defn addN [n]
  (fn [x]
    (+ x n)))
```


```hy>=
 (setv add2 (addN 2))
=> (add2 7)
9

=> ((addN 3) 4)
7
```


==Icon and {{header|Unicon}}==

This version only works in Unicon because of the coexpression calling syntax
used.


```unicon
procedure main(A)
    add2 := addN(2)
    write("add2(7) = ",add2(7))
    write("add2(1) = ",add2(1))
end

procedure addN(n)
    return makeProc{ repeat { (x := (x@&source)[1], x +:= n) } }
end

procedure makeProc(A)
    return (@A[1], A[1])
end
```


```txt

->curry
add2(7) = 9
add2(1) = 3
->

```



## Io

A general currying function written in the [[Io]] programming language:

```io
curry := method(fn,
	a := call evalArgs slice(1)
	block(
		b := a clone appendSeq(call evalArgs)
		performWithArgList("fn", b)
	)
)

// example:
increment := curry( method(a,b,a+b), 1 )
increment call(5)
// result => 6
```



## J


'''Solution''':Use <tt>&</tt> (bond). This primitive conjunction accepts two arguments: a function (verb) and an object (noun) and binds the object to the function, deriving a new function.
'''Example''':
```j
   threePlus=: 3&+
   threePlus 7
10
   halve =: %&2  NB.  % means divide
   halve 20
10
   someParabola =: _2 3 1 &p. NB. x^2 + 3x - 2
```


'''Note''': The final example (<tt>someParabola</tt>) shows the single currying primitive (&) combined with J's array oriented nature, permits partial application of a function of any number of arguments.


## Java


```java5
    public class Currier<ARG1, ARG2, RET> {
        public interface CurriableFunctor<ARG1, ARG2, RET> {
            RET evaluate(ARG1 arg1, ARG2 arg2);
        }

        public interface CurriedFunctor<ARG2, RET> {
            RET evaluate(ARG2 arg);
        }

        final CurriableFunctor<ARG1, ARG2, RET> functor;

        public Currier(CurriableFunctor<ARG1, ARG2, RET> fn) { functor = fn; }

        public CurriedFunctor<ARG2, RET> curry(final ARG1 arg1) {
            return new CurriedFunctor<ARG2, RET>() {
                public RET evaluate(ARG2 arg2) {
                    return functor.evaluate(arg1, arg2);
                }
            };
        }

        public static void main(String[] args) {
            Currier.CurriableFunctor<Integer, Integer, Integer> add
                = new Currier.CurriableFunctor<Integer, Integer, Integer>() {
                    public Integer evaluate(Integer arg1, Integer arg2) {
                        return new Integer(arg1.intValue() + arg2.intValue());
                    }
            };

            Currier<Integer, Integer, Integer> currier
                = new Currier<Integer, Integer, Integer>(add);

            Currier.CurriedFunctor<Integer, Integer> add5
                = currier.curry(new Integer(5));

            System.out.println(add5.evaluate(new Integer(2)));
        }
    }
```



### Java 8



```java

import java.util.function.BiFunction;
import java.util.function.Function;

public class Curry {

	//Curry a method
	public static <T, U, R> Function<T, Function<U, R>> curry(BiFunction<T, U, R> biFunction) {
		return t -> u -> biFunction.apply(t, u);
	}

	public static int add(int x, int y) {
		return x + y;
	}

	public static void curryMethod() {
		BiFunction<Integer, Integer, Integer> bif = Curry::add;
		Function<Integer, Function<Integer, Integer>> add = curry(bif);
		Function<Integer, Integer> add5 = add.apply(5);
		System.out.println(add5.apply(2));
	}

	//Or declare the curried function in one line
	public static void curryDirectly() {
		Function<Integer, Function<Integer, Integer>> add = x -> y -> x + y;
		Function<Integer, Integer> add5 = add.apply(5);
		System.out.println(add5.apply(2));
	}

	//prints 7 and 7
	public static void main(String[] args) {
		curryMethod();
		curryDirectly();
	}
}

```



## JavaScript



### ES5



### =Partial application=


```javascript
 function addN(n) {
    var curry = function(x) {
        return x + n;
    };
    return curry;
 }

 add2 = addN(2);
 alert(add2);
 alert(add2(7));
```



### =Generic currying=


Basic case - returning a curried version of a function of two arguments


```JavaScript
(function () {

    // curry :: ((a, b) -> c) -> a -> b -> c
    function curry(f) {
        return function (a) {
            return function (b) {
                return f(a, b);
            };
        };
    }


    // TESTS

    // product :: Num -> Num -> Num
    function product(a, b) {
        return a * b;
    }

    // return typeof curry(product);
    // --> function

    // return typeof curry(product)(7)
    // --> function

    //return typeof curry(product)(7)(9)
    // --> number

    return [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        .map(curry(product)(7))

    // [7, 14, 21, 28, 35, 42, 49, 56, 63, 70]

})();

```


```JavaScript
[7, 14, 21, 28, 35, 42, 49, 56, 63, 70]
```



Functions of arbitrary arity can also be curried:


```JavaScript
(function () {

    // (arbitrary arity to fully curried)
    // extraCurry :: Function -> Function
    function extraCurry(f) {

        // Recursive currying
        function _curry(xs) {
            return xs.length >= intArgs ? (
                f.apply(null, xs)
            ) : function () {
                return _curry(xs.concat([].slice.apply(arguments)));
            };
        }

        var intArgs = f.length;

        return _curry([].slice.call(arguments, 1));
    }


    // TEST

    // product3:: Num -> Num -> Num -> Num
    function product3(a, b, c) {
        return a * b * c;
    }

    return [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        .map(extraCurry(product3)(7)(2))

    // [14, 28, 42, 56, 70, 84, 98, 112, 126, 140]

})();
```


```JavaScript
[14, 28, 42, 56, 70, 84, 98, 112, 126, 140]
```



### ES6



### =Y combinator=

Using a definition of currying that does not imply partial application, only conversion of a function of multiple arguments, e.g.:
```javascript
(a,b) => expr_using_a_and_b
```
into a function that takes a series of as many function applications as that function took arguments, e.g.:
```javascript>a => b =
 expr_using_a_and_b
```


One version for functions of a set amount of arguments that takes no rest arguments, and one version for functions with rest argument. The caveat being that if the rest argument would be empty, it still requires a separate application, and multiple rest arguments cannot be curried into multiple applications, since we have to figure out the number of applications from the function signature, not the amount of arguments the user might want to send it.

```javascript
let
  fix = // This is a variant of the Applicative order Y combinator
    f => (f => f(f))(g => f((...a) => g(g)(...a))),
  curry =
    f => (
      fix(
        z => (n,...a) => (
          n>0
          ?b => z(n-1,...a,b)
          :f(...a)))
      (f.length)),
  curryrest =
    f => (
      fix(
        z => (n,...a) => (
          n>0
          ?b => z(n-1,...a,b)
          :(...b) => f(...a,...b)))
      (f.length)),
  curriedmax=curry(Math.max),
  curryrestedmax=curryrest(Math.max);
print(curriedmax(8)(4),curryrestedmax(8)(4)(),curryrestedmax(8)(4)(9,7,2));
// 8,8,9

```

Neither of these handle propagation of the this value for methods, as ECMAScript 2015 (ES6) fat arrow syntax doesn't allow for this value propagation. Versions could easily be written for those cases using an outer regular function expression and use of Function.prototype.call or Function.prototype.apply. Use of Y combinator could also be removed through use of an inner named function expression instead of the anonymous fat arrow function syntax.


### =Simple 2 and N argument versions=


In the most rudimentary form, for example for mapping a two-argument function over an array:


```JavaScript
(() => {

    // curry :: ((a, b) -> c) -> a -> b -> c
    let curry = f => a => b => f(a, b);


    // TEST

    // product :: Num -> Num -> Num
    let product = (a, b) => a * b,

        // Int -> Int -> Maybe Int -> [Int]
        range = (m, n, step) => {
            let d = (step || 1) * (n >= m ? 1 : -1);

            return Array.from({
                length: Math.floor((n - m) / d) + 1
            }, (_, i) => m + (i * d));
        }


    return range(1, 10)
        .map(curry(product)(7))

    // [7, 14, 21, 28, 35, 42, 49, 56, 63, 70]

})();
```


```JavaScript
[7, 14, 21, 28, 35, 42, 49, 56, 63, 70]
```



Or, recursively currying functions of arbitrary arity:


```JavaScript
(() => {

    // (arbitrary arity to fully curried)
    // extraCurry :: Function -> Function
    let extraCurry = (f, ...args) => {
        let intArgs = f.length;

        // Recursive currying
        let _curry = (xs, ...arguments) =>
            xs.length >= intArgs ? (
                f.apply(null, xs)
            ) : function () {
                return _curry(xs.concat([].slice.apply(arguments)));
            };

        return _curry([].slice.call(args, 1));
    };

    // TEST

    // product3:: Num -> Num -> Num -> Num
    let product3 = (a, b, c) => a * b * c;

    return [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        .map(extraCurry(product3)(7)(2))

    // [14, 28, 42, 56, 70, 84, 98, 112, 126, 140]

})();
```


```JavaScript
[14, 28, 42, 56, 70, 84, 98, 112, 126, 140]
```



## jq


In jq, functions are filters.  Accordingly, we illustrate currying by defining plus(x) to be a filter that adds x to its input, and then define plus5 as plus(5):

```jq

def plus(x): . + x;

def plus5: plus(5);

```


We can now use plus5 as a filter, e.g.
```jq
3 | plus5
```
 produces 8.


## Julia


```julia

function addN(n::Number)::Function
  adder(x::Number) = n + x
  return adder
end

```


```txt

julia> add2 = addN(2)
(::adder) (generic function with 1 method)

julia> add2(1)
3


```



## Kotlin


```scala
// version 1.1.2

fun curriedAdd(x: Int) = { y: Int -> x + y }

fun main(args: Array<String>) {
    val a = 2
    val b = 3
    val sum = curriedAdd(a)(b)
    println("$a + $b = $sum")
}
```


```txt

2 + 3 = 5

```



## Latitude


<lang>addN := {
  takes '[n].
  {
    $1 + n.
  }.
}.

add3 := addN 3.
add3 (4). ;; 7
```


Note that, because of the syntax of the language, it is not possible to call <code>addN</code> in one line the naive way.

```latitude
;; addN (3) (4). ;; Syntax error!
;; (addN (3)) (4). ;; Syntax error!
addN (3) call (4). ;; Works as expected.
```


As a consequence, it is more common in Latitude to return new objects whose methods have meaningful names, rather than returning a curried function.

```latitude
addN := {
  takes '[n].
  Object clone tap {
    self do := {
      $1 + n.
    }.
  }.
}.

addN 3 do 4. ;; 7
```



## LFE


```lisp
(defun curry (f arg)
  (lambda (x)
    (apply f
      (list arg x))))

```

Usage:

```lisp

(funcall (curry #'+/2 10) 10)

```



## Logtalk


```logtalk

| ?- logtalk << call([Z]>>(call([X,Y]>>(Y is X*X), 5, R), Z is R*R), T).
T = 625
yes

```


Logtalk support for lambda expressions and currying was introduced in version 2.38.0, released in December 2009.


## Lua


```lua

function curry2(f)
   return function(x)
      return function(y)
         return f(x,y)
      end
   end
end

function add(x,y)
   return x+y
end

local adder = curry2(add)
assert(adder(3)(4) == 3+4)
local add2 = adder(2)
assert(add2(3) == 2+3)
assert(add2(5) == 2+5)

```



## M2000 Interpreter


```M2000 Interpreter

Module LikeCpp {
      divide=lambda (x, y)->x/y
      partsof120=lambda divide ->divide(![], 120)
      Print "half of 120 is ";partsof120(2)
      Print "a third is ";partsof120(3)
      Print "and a quarter is ";partsof120(4)
}
LikeCpp

Module Joke {
      \\ we can call F1(),  with any number of arguments, and always read one and then
      \\ call itself passing the remain arguments
      \\ ![] take stack of values and place it in the next call.
      F1=lambda -> {
            if empty then exit
            Read x
            =x+lambda(![])
      }

      Print F1(F1(2),2,F1(-4))=0
      Print F1(-4,F1(2),2)=0
      Print F1(2, F1(F1(2),2))=F1(F1(F1(2),2),2)
      Print F1(F1(F1(2),2),2)=6
      Print F1(2, F1(2, F1(2),2))=F1(F1(F1(2),2, F1(2)),2)
      Print F1(F1(F1(2),2, F1(2)),2)=8
      Print F1(2, F1(10, F1(2, F1(2),2)))=F1(F1(F1(2),2, F1(2)),2, 10)
      Print F1(F1(F1(2),2, F1(2)),2, 10)=18
      Print F1(2,2,2,2,10)=18
      Print F1()=0

      Group F2 {
            Sum=0
            Function Add  (x){
                  .Sum+=x
                  =x
            }
      }
      Link F2.Add() to F2()
      Print F1(F1(F1(F2(2)),F2(2), F1(F2(2))),F2(2))=8
      Print F2.Sum=8
}
Joke

```

Without joke, can anyone answer this puzzle?

```M2000 Interpreter

Module Puzzle {
            Global Group F2 {
                  Sum=0
                  Sum2=0
                  Function Add  (x){
                        .Sum+=x
                        =x
                  }
            }
            F1=lambda -> {
                  if empty then exit
                  Read x
                  Print ">>>", F2.Sum
                  F2.Sum2++   ' add one each time we read x
                  =x+lambda(![])
            }
            Link F2.Add() to F2()
            P=F1(F1(F1(F2(2)),F2(2), F1(F2(2))),F2(2))=8
            Print F2.Sum=8
            Print F2.Sum2=7
            \\  We read 7 times x, but we get 8, 2+2+2+2
            \\  So 3 times x was zero, or not?
            \\  but where we pass zero?
            \\  zero return from F1 if no argument pass, so how x  get zero??
}
Puzzle

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

Currying can be implemented by nesting the <code>Function</code> function. The following method curries the <code>Plus</code> function.

    In[1]:=   plusFC = Function[{x},Function[{y},Plus[x,y]]];

A higher currying function can be implemented straightforwardly.

    In[2]:=   curry = Function[{x}, Function[{y}, Function[{z}, x[y, z]]]];

```txt

    In[3]:=   Plus[2,3]
    Out[3]:=  5

    In[4]:=   plusFC[2][3]
    Out[4]:=  5

    In[5]:=   curry[Plus][2][3]
    Out[5]:=  5

```



## Nemerle

Currying isn't built in to Nemerle, but is relatively straightforward to define.

```Nemerle
using System;
using System.Console;

module Curry
{
    Curry[T, U, R](f : T * U -> R) : T -> U -> R
    {
        fun (x) { fun (y) { f(x, y) } }
    }

    Main() : void
    {
        def f(x, y) { x + y }
	def g = Curry(f);
	def h = Curry(f)(12);              // partial application
	WriteLine($"$(Curry(f)(20)(22))");
	WriteLine($"$(g(21)(21))");
	WriteLine($"$(h(30))")
    }
}
```



## Nim


```nim
proc addN[T](n: T): auto = (proc(x: T): T = x + n)

let add2 = addN(2)
echo add2(7)
```

Alternative syntax:

```nim
import future

proc addM[T](n: T): auto = (x: T) => x + n

let add3 = addM(3)
echo add3(7)
```



## OCaml

OCaml has a built-in natural method of defining functions that are curried:

```ocaml
let addnums x y = x+y        (* declare a curried function *)

let add1 = addnums 1         (* bind the first argument to get another function *)
add1 42                      (* apply to actually compute a result, 43 *)
```

The type of <code>addnums</code> above will be <tt>int -> int -> int</tt>.

Note that <code>fun addnums x y = ...</code>, or, equivalently, <code>let addnums = fun x y -> ...</code>, is really just syntactic sugar for <code>let addnums = function x -> function y -> ...</code>.

You can also define a general currying higher-ordered function:

```ocaml
let curry f x y = f (x,y)
(* Type signature: ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
```

This is a function that takes a function as a parameter and returns a function that takes one of the parameters and returns ''another'' function that takes the other parameter and returns the result of applying the parameter function to the pair of arguments.


## Oforth



```Oforth>2 #+ curry =
 2+
5 2+ .
7 ok
```



## PARI/GP

Simple currying example with closures.

```parigp
curriedPlus(x)=y->x+y;
curriedPlus(1)(2)
```

```txt
3
```



## Perl

This is a [[Perl|Perl 5]] example of a general curry function and curried plus using [[wp:closure (computer science)|closures]]:

```perl
sub curry{
  my ($func, @args) = @_;

  sub {
    #This @_ is later
    &$func(@args, @_);
  }
}

sub plusXY{
  $_[0] + $_[1];
}

my $plusXOne = curry(\&plusXY, 1);
print &$plusXOne(3), "\n";
```



## Perl 6

All callable objects have an "assuming" method that can do partial application of either positional or named arguments.  Here we curry the built-in subtraction operator.

```perl6
my &negative = &infix:<->.assuming(0);
say negative 1;
```

```txt
-1
```



## Phix

Phix does not support currying. The closest I can manage is very similar to my solution for closures

```Phix
sequence curries = {}
function create_curried(integer rid, sequence partial_args)
    curries = append(curries,{rid,partial_args})
    return length(curries) -- (return an integer id)
end function

function call_curried(integer id, sequence args)
    {integer rid, sequence partial_args} = curries[id]
    return call_func(rid,partial_args&args)
end function

function add(atom a, b)
    return a+b
end function

integer curried = create_curried(routine_id("add"),{2})
printf(1,"2+5=%d\n",call_curried(curried,{5}))
```

```txt

2+5=7

```

<small>(Of course you would probably not have to try too much harder to make it say 2+2=5 instead.)</small>


## PicoLisp


```txt
: (de multiplier (@X)
   (curry (@X) (N) (* @X N)) )
-> multiplier
: (multiplier 7)
-> ((N) (* 7 N))
: ((multiplier 7) 3)
-> 21
```



## PowerShell


```PowerShell

function Add($x) { return { param($y) return $y + $x }.GetNewClosure() }

```


```PowerShell

& (Add 1) 2

```

```txt

3

```

Add each number in list to its square root:

```PowerShell

(4,9,16,25 | ForEach-Object { & (add $_) ([Math]::Sqrt($_)) }) -join ", "

```

```txt

6, 12, 20, 30

```



## Prolog

Works with SWI-Prolog and module '''lambda.pl'''

Module lambda.pl can be found at http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl .

```txt
 ?- [library('lambda.pl')].
% library(lambda.pl) compiled into lambda 0,00 sec, 28 clauses
true.

 ?- N = 5, F = \X^Y^(Y is X+N), maplist(F, [1,2,3], L).
N = 5,
F = \X^Y^ (Y is X+5),
L = [6,7,8].


```



## Python


### Nested defs and functools.partial

Since Python has had local functions with closures since around 1.0, it's always been possible to create curried functions manually:

```python
 def addN(n):
     def adder(x):
         return x + n
     return adder
```



```python> >>
 add2 = addN(2)
 >>> add2
 <function adder at 0x009F1E30>
 >>> add2(7)
 9
```


But Python also comes with a function to build partial functions (with any number of positional or keyword arguments bound in) for you. This was originally in a third-party model called functional, but was added to the stdlib functools module in 2.5. Every year or so, someone suggests either moving it into builtins because it's so useful or removing it from the stdlib entirely because it's so easy to write yourself, but it's been in the functools module since 2.5 and will probably always be there.

```python>>>
 from functools import partial
>>> from operator import add
>>> add2 = partial(add, 2)
>>> add2
functools.partial(<built-in function add>, 2)
>>> add2(7)
9
>>> double = partial(map, lambda x: x*2)
>>> print(*double(range(5)))
0 2 4 6 8
```


But for a true curried function that can take arguments one at a time via normal function calls, you have to do a bit of wrapper work to build a callable object that defers to partial until all of the arguments are available. Because of the Python's dynamic nature and flexible calling syntax, there's no way to do this in a way that works for every conceivable valid function, but there are a variety of ways that work for different large subsets. Or just use a third-party library like [https://toolz.readthedocs.io toolz] that's already done it for you:

```python>>>
 from toolz import curry
>>> import operator
>>> add = curry(operator.add)
>>> add2 = add(2)
>>> add2
<built-in function add>
>>> add2(7)
9
>>> # Toolz also has pre-curried versions of most HOFs from builtins, stdlib, and toolz
>>>from toolz.curried import map
>>> double = map(lambda x: x*2)
>>> print(*double(range(5)))
0 2 4 6 8
```



### Automatic curry and uncurry functions using lambdas


As an alternative to nesting defs, we can also define curried functions, perhaps more directly, in terms of lambdas.
We can also write a general '''curry''' function, and a corresponding '''uncurry''' function, for automatic derivation of curried and uncurried functions at run-time, without needing to import ''functools.partial'':


```python
# AUTOMATIC CURRYING AND UNCURRYING OF EXISTING FUNCTIONS


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    return lambda a: lambda b: f(a, b)


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    return lambda x, y: f(x)(y)


# EXAMPLES --------------------------------------

# A plain uncurried function with 2 arguments,

# justifyLeft :: Int -> String -> String
def justifyLeft(n, s):
    return (s + (n * ' '))[:n]


# and a similar, but manually curried, function.

# justifyRight :: Int -> String -> String
def justifyRight(n):
    return lambda s: (
        ((n * ' ') + s)[-n:]
    )


# CURRYING and UNCURRYING at run-time:

def main():
    for s in [
        'Manually curried using a lambda:',
        '\n'.join(map(
            justifyRight(5),
            ['1', '9', '10', '99', '100', '1000']
        )),

        '\nAutomatically uncurried:',
        uncurry(justifyRight)(5, '10000'),

        '\nAutomatically curried',
        '\n'.join(map(
            curry(justifyLeft)(10),
            ['1', '9', '10', '99', '100', '1000']
        ))
    ]:
        print (s)


main()
```

```txt
Manually curried using a lambda:
    1
    9
   10
   99
  100
 1000

Automatically uncurried:
10000

Automatically curried
1
9
10
99
100
1000
```



## Racket

The simplest way to make a curried functions is to use curry:


```racket

#lang racket
(((curry +) 3) 2) ; =>5

```


As an alternative, one can use the following syntax:

```racket

#lang racket

(define ((curried+ a) b)
  (+ a b))

((curried+ 3) 2)  ; => 5

```



## REXX

This example is modeled after the   '''D'''   example.

### specific version


```ress
/*REXX program demonstrates a REXX    currying method    to perform addition.           */
say 'add 2 to 3:          '   add(2, 3)
say 'add 2 to 3 (curried):'   add2(3)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add:  procedure;  $= arg(1);       do j=2  to arg();   $= $ + arg(j);   end;      return $
add2: procedure;  return add( arg(1), 2)
```

```txt

add 2 to 3:           5
add 2 to 3 (curried): 5

```



### generic version


```rexx
/*REXX program demonstrates a REXX    currying method    to perform addition.           */
say 'add 2 to 3:          '   add(2, 3)
say 'add 2 to 3 (curried):'   add2(3)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add:  procedure;  $= 0;       do    j=1  for arg()
                                 do k=1  for words( arg(j) );      $= $ + word( arg(j), k)
                                 end   /*k*/
                              end      /*j*/
      return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
add2: procedure;  return add( arg(1), 2)
```

## Ruby

The curry method was added in Ruby 1.9.1. It takes an optional arity argument, which determines the number of arguments to be passed to the proc.
If that number is not reached, the curry method returns a new curried  method for the rest of the arguments. (Examples taken from the documentation).

```ruby

b = proc {|x, y, z| (x||0) + (y||0) + (z||0) }
p b.curry[1][2][3]           #=> 6
p b.curry[1, 2][3, 4]        #=> 6
p b.curry(5)[1][2][3][4][5]  #=> 6
p b.curry(5)[1, 2][3, 4][5]  #=> 6
p b.curry(1)[1]              #=> 1

b = proc {|x, y, z, *w| (x||0) + (y||0) + (z||0) + w.inject(0, &:+) }
p b.curry[1][2][3]           #=> 6
p b.curry[1, 2][3, 4]        #=> 10
p b.curry(5)[1][2][3][4][5]  #=> 15
p b.curry(5)[1, 2][3, 4][5]  #=> 15
p b.curry(1)[1]              #=> 1

```



## Rust


This is a simple currying function written in [[Rust]]:

```rust
fn add_n(n : i32) -> impl Fn(i32) -> i32 {
    move |x| n + x
}

fn main() {
    let adder = add_n(40);
    println!("The answer to life is {}.", adder(2));
}
```



## Scala


```Scala

def add(a: Int)(b: Int) = a + b
val add5 = add(5) _
add5(2)

```



## Sidef

This can be done by using lazy methods:

```ruby
var adder = 1.method(:add);
say adder(3);                #=> 4
```


Or by using a generic curry function:

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



## Swift

You can return a closure (or nested function):

```Swift
func addN(n:Int)->Int->Int { return {$0 + n} }

var add2 = addN(2)
println(add2) // (Function)
println(add2(7)) // 9
```


Prior to Swift 3, there was a curried function definition syntax:

```Swift
func addN(n:Int)(x:Int) -> Int { return x + n }

var add2 = addN(2)
println(add2) // (Function)
println(add2(x:7)) // 9
```

However, there was a bug in the above syntax which forces the second parameter to always be labeled. As of Swift 1.2, you could explicitly make the second parameter not labeled:

```Swift
func addN(n:Int)(_ x:Int) -> Int { return x + n }

var add2 = addN(2)
println(add2) // (Function)
println(add2(7)) // 9
```



## Standard ML

Standard ML has a built-in natural method of defining functions that are curried:

```sml
fun addnums (x:int) y = x+y  (* declare a curried function *)

val add1 = addnums 1         (* bind the first argument to get another function *)
add1 42                      (* apply to actually compute a result, 43 *)
```

The type of <code>addnums</code> above will be <tt>int -> int -> int</tt> (the type constraint in the declaration only being necessary because of the polymorphic nature of the <code>+</code> operator).

Note that <code>fun addnums x y = ...</code> is really just syntactic sugar for <code>val addnums = fn x => fn y => ...</code>.

You can also define a general currying higher-ordered function:

```sml
fun curry f x y = f(x,y)
(* Type signature: ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
```

This is a function that takes a function as a parameter and returns a function that takes one of the parameters and returns ''another'' function that takes the other parameter and returns the result of applying the parameter function to the pair of arguments.


## Tcl

The simplest way to do currying in Tcl is via an interpreter alias:

```tcl
interp alias {} addone {} ::tcl::mathop::+ 1
puts [addone 6]; # => 7
```

Tcl doesn't support automatic creation of curried functions though; the general variadic nature of a large proportion of Tcl commands makes that impractical.

### History

The type of aliases used here are a simple restriction of general inter-interpreter aliases to the case where both the source and target interpreter are the current one; these aliases are a key component of the secure interpreter mechanism introduced in Tcl 7.6, and are the mechanism used to allow access to otherwise-insecure behavior from a secure context (e.g., to write to a ''particular'' file, but not any old file).


## TXR


TXR Lisp has an operator called <code>op</code> for currying. Of course, currying is done with lambdas under the hood; the operator generates lambdas. Its name is inspired by the same-named operators featured in the Goo language, and in the Common Lisp library <i>cl-op</i>.

References:
Goo <code>op</code>: [http://people.csail.mit.edu/jrb/goo/manual.46/goomanual_15.html]
<i>cl-op</i>: [https://code.google.com/p/cl-op/]

TXR's <code>op</code> is quite different in that it uses numbered arguments, has some additional features, and is accompanied by a "zoo" of related operators which share its currying syntax, providing various useful derived behaviors.

A two-argument function which subtracts is arguments from 10, and then subtracts five:


```txrlisp
(op - 10 @1 @2 5)
```



## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >=15.3)

Functions are not curried in VB.NET, so this entry details functions that take a function and return functions that act as if the original function were curried (i.e. each takes one parameter and returns another function that takes one parameter, with the function for which all parameters of the original function are supplied calling the original function with those arguments.

===Fixed-arity approach===

Uses generics and lambdas returning lambdas.


```vbnet
Option Explicit On
Option Infer On
Option Strict On

Module Currying
    ' The trivial curry.
    Function Curry(Of T1, TResult)(func As Func(Of T1, TResult)) As Func(Of T1, TResult)
        ' At least satisfy the implicit contract that the result isn't reference-equal to the original function.
        Return Function(a) func(a)
    End Function

    Function Curry(Of T1, T2, TResult)(func As Func(Of T1, T2, TResult)) As Func(Of T1, Func(Of T2, TResult))
        Return Function(a) Function(b) func(a, b)
    End Function

    Function Curry(Of T1, T2, T3, TResult)(func As Func(Of T1, T2, T3, TResult)) As Func(Of T1, Func(Of T2, Func(Of T3, TResult)))
        Return Function(a) Function(b) Function(c) func(a, b, c)
    End Function

    ' And so on.
End Module
```


Test code:

```vbnet
Module Main
    ' An example binary function.
    Function Add(a As Integer, b As Integer) As Integer
        Return a + b
    End Function

    Sub Main()
        Dim curriedAdd = Curry(Of Integer, Integer, Integer)(AddressOf Add)
        Dim add2To = curriedAdd(2)

        Console.WriteLine(Add(2, 3))
        Console.WriteLine(add2To(3))
        Console.WriteLine(curriedAdd(2)(3))

        ' An example ternary function.
        Dim substring = Function(s As String, startIndex As Integer, length As Integer) s.Substring(startIndex, length)
        Dim curriedSubstring = Curry(substring)

        Console.WriteLine(substring("abcdefg", 2, 3))
        Console.WriteLine(curriedSubstring("abcdefg")(2)(3))

        ' The above is just syntax sugar for this (a call to the Invoke() method of System.Delegate):
        Console.WriteLine(curriedSubstring.Invoke("abcdefg").Invoke(2).Invoke(3))

        Dim substringStartingAt1 = curriedSubstring("abcdefg")(1)
        Console.WriteLine(substringStartingAt1(2))
        Console.WriteLine(substringStartingAt1(4))
    End Sub
End Module
```


===Late-binding approach===

or both
and
Due to VB's syntax, with indexers using parentheses, late-bound invocation expressions are compiled as invocations of the default property of the receiver. Thus, it is not possible to perform a late-bound delegate invocation. This limitation can, however, be circumvented, by declaring a type that wraps a delegate and defines a default property that invokes the delegate. Furthermore, by making this type what is essentially a discriminated union of a delegate and a result and guaranteeing that all invocations return another instance of this type, it is possible for the entire system to work with Option Strict on.


```vbnet
Option Explicit On
Option Infer On
Option Strict On

Module CurryingDynamic
    ' Cheat visual basic's syntax by defining a type that can be the receiver of what appears to be a method call.
    ' Needless to say, this is not idiomatic VB.
    Class CurryDelegate
        ReadOnly Property Value As Object
        ReadOnly Property Target As [Delegate]

        Sub New(value As Object)
            Dim curry = TryCast(value, CurryDelegate)
            If curry IsNot Nothing Then
                Me.Value = curry.Value
                Me.Target = curry.Target
            ElseIf TypeOf value Is [Delegate] Then
                Me.Target = DirectCast(value, [Delegate])
            Else
                Me.Value = value
            End If
        End Sub

        ' CurryDelegate could also work as a dynamic n-ary function delegate, if an additional ParamArray argument were to be added.
        Default ReadOnly Property Invoke(arg As Object) As CurryDelegate
            Get
                If Me.Target Is Nothing Then Throw New InvalidOperationException("All curried parameters have already been supplied")

                Return New CurryDelegate(Me.Target.DynamicInvoke({arg}))
            End Get
        End Property

        ' A syntactically natural way to assert that the currying is complete and that the result is of the specified type.
        Function Unwrap(Of T)() As T
            If Me.Target IsNot Nothing Then Throw New InvalidOperationException("Some curried parameters have not yet been supplied.")
            Return DirectCast(Me.Value, T)
        End Function
    End Class

    Function DynamicCurry(func As [Delegate]) As CurryDelegate
        Return DynamicCurry(func, ImmutableList(Of Object).Empty)
    End Function

    ' Use ImmutableList to create a new list every time any curried subfunction is called avoiding multiple or repeated
    ' calls interfering with each other.
    Private Function DynamicCurry(func As [Delegate], collectedArgs As ImmutableList(Of Object)) As CurryDelegate
        Return If(collectedArgs.Count = func.Method.GetParameters().Length,
            New CurryDelegate(func.DynamicInvoke(collectedArgs.ToArray())),
            New CurryDelegate(Function(arg As Object) DynamicCurry(func, collectedArgs.Add(arg))))
    End Function
End Module
```


Test code:

```vbnet
Module Program
    Function Add(a As Integer, b As Integer) As Integer
        Return a + b
    End Function

    Sub Main()
        ' A delegate for the function must be created in order to eagerly perform overload resolution.
        Dim curriedAdd = DynamicCurry(New Func(Of Integer, Integer, Integer)(AddressOf Add))
        Dim add2To = curriedAdd(2)

        Console.WriteLine(add2To(3).Unwrap(Of Integer))
        Console.WriteLine(curriedAdd(2)(3).Unwrap(Of Integer))

        Dim substring = Function(s As String, i1 As Integer, i2 As Integer) s.Substring(i1, i2)
        Dim curriedSubstring = DynamicCurry(substring)

        Console.WriteLine(substring("abcdefg", 2, 3))
        Console.WriteLine(curriedSubstring("abcdefg")(2)(3).Unwrap(Of String))

        ' The trickery of using a parameterized default property also makes it appear that the "delegate" has an Invoke() method.
        Console.WriteLine(curriedSubstring.Invoke("abcdefg").Invoke(2).Invoke(3).Unwrap(Of String))

        Dim substringStartingAt1 = curriedSubstring("abcdefg")(1)
        Console.WriteLine(substringStartingAt1(2).Unwrap(Of String))
        Console.WriteLine(substringStartingAt1(4).Unwrap(Of String))
    End Sub
End Module

```


```txt
5
5
5
cde
cde
cde
bc
bcde
```



## Wortel

The <code>\</code> operator takes a function and an argument and partial applies the argument to the function. The <code>&\</code> works like the <code>\</code> operator but can also take an array literal and partial applies all the arguments in the array.

```wortel
@let {
  addOne \+ 1
  subtractFrom1 \- 1
  subtract1 \~- 1

  subtract1_2 &\- [. 1]

  add ^+
  ; partial apply to named functions
  addOne_2 \add 1

  ; testing
  [[
    !addOne 5 ; returns 6
    !subtractFrom1 5 ; returns -4
    !subtract1 5 ; returns 4
    !subtract1_2 5 ; returns 4
    !addOne_2 5 ; returns 6
  ]]
}
```



## zkl

zkl doesn't support currying per se (recompilation of f with fixed input to create a new function), it does support partial application, for all objects, for any [number of] positional parameters to create an object of reduced arity.

```zkl
addOne:= Op("+").fp(1); addOne(5) //-->6
minusOne:=Op("-").fp1(1); minusOne(5) //-->4, note that this fixed 1 as the second parameter
   // fix first and third parameters:
foo:=String.fpM("101","<foo>","</foo>"); foo("zkl"); //-->"<foo>zkl</foo>"
fcn g(x){x+1} f:=fcn(f,x){f(x)+x}.fp(g); f(5); //-->11
f:=fcn(f,x){f(x)+x}.fp(fcn(x){x+1}); // above with lambdas all the way down
```

