+++
title = "Anonymous recursion"
description = ""
date = 2019-09-29T06:29:48Z
aliases = []
[extra]
id = 8846
[taxonomies]
categories = []
tags = []
+++

{{task|Recursion}}

While implementing a recursive function, it often happens that we must resort to a separate   ''helper function''   to handle the actual recursion.

This is usually the case when directly calling the current function would waste too many resources (stack space, execution time), causing unwanted side-effects,   and/or the function doesn't have the right arguments and/or return values.

So we end up inventing some silly name like   '''foo2'''   or   '''foo_helper'''.   I have always found it painful to come up with a proper name, and see some disadvantages:

::*   You have to think up a name, which then pollutes the namespace
::*   Function is created which is called from nowhere else
::*   The program flow in the source code is interrupted

Some languages allow you to embed recursion directly in-place.   This might work via a label, a local ''gosub'' instruction, or some special keyword.

Anonymous recursion can also be accomplished using the   [[Y combinator]].


;Task:
If possible, demonstrate this by writing the recursive version of the fibonacci function   (see [[Fibonacci sequence]])   which checks for a negative argument before doing the actual recursion.





## 11l

{{trans|C++}}

```11l
F fib(n)
   F f(Int n) -> Int
      I n < 2
         R n
      R @f(n - 1) + @f(n - 2)
   R f(n)

L(i) 0..20
   print(fib(i), end' ‘ ’)
```

{{out}}

```txt

0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765

```



## Ada

In Ada you can define functions local to other functions/procedures. This makes it invisible to outside and prevents namespace pollution.

Better would be to use type Natural instead of Integer, which lets Ada do the magic of checking the valid range.

```Ada
   function Fib (X: in Integer) return Integer is
      function Actual_Fib (N: in Integer) return Integer is
      begin
         if N < 2 then
            return N;
         else
            return Actual_Fib (N-1) + Actual_Fib (N-2);
         end if;
      end Actual_Fib;
   begin
      if X < 0 then
         raise Constraint_Error;
      else
         return Actual_Fib (X);
      end if;
   end Fib;
```



## ALGOL 68

{{Trans|Ada}}

```algol68
PROC fibonacci = ( INT x )INT:
     IF x < 0
     THEN
         print( ( "negative parameter to fibonacci", newline ) );
         stop
     ELSE
         PROC actual fibonacci = ( INT n )INT:
             IF n < 2
             THEN
                 n
             ELSE
                 actual fibonacci( n - 1 ) + actual fibonacci( n - 2 )
             FI;
         actual fibonacci( x )
     FI;

```



## AutoHotkey


```autohotkey
Fib(n) {
	nold1 := 1
	nold2 := 0
	If n < 0
	{
		MsgBox, Positive argument required!
		Return
	}
	Else If n = 0
		Return nold2
	Else If n = 1
		Return nold1
	Fib_Label:
	t := nold2+nold1
	If n > 2
	{
		n--
		nold2:=nold1
		nold1:=t
		GoSub Fib_Label
	}
	Return t
}
```



## AutoIt


```autoit

ConsoleWrite(Fibonacci(10) & @CRLF)						; ## USAGE EXAMPLE
ConsoleWrite(Fibonacci(20) & @CRLF)						; ## USAGE EXAMPLE
ConsoleWrite(Fibonacci(30))						        ; ## USAGE EXAMPLE

Func Fibonacci($number)

	If $number < 0 Then Return "Invalid argument" 				; No negative numbers

	If $number < 2 Then 							; If $number equals 0 or 1
		Return $number  						; then return that $number
	Else									; Else $number equals 2 or more
		Return Fibonacci($number - 1) + Fibonacci($number - 2) 		; FIBONACCI!
	EndIf

EndFunc

```

{{out}}

```txt

        55
        6765
        832040

```



## Axiom

Using the Aldor compiler in Axiom/Fricas:

```axiom
#include "axiom"
Z ==> Integer;
fib(x:Z):Z == {
	x <= 0 => error "argument outside of range";
	f(n:Z,v1:Z,v2:Z):Z == if n<2 then v2 else f(n-1,v2,v1+v2);
	f(x,1,1);
}
```
The old Axiom compiler has scope issues with calling a local function recursively. One solution is to use the Reference (pointer) domain and initialise the local function with a dummy value:

```axiom
)abbrev package TESTP TestPackage
Z ==> Integer
TestPackage : with
    fib : Z -> Z
  == add
    fib x ==
      x <= 0 => error "argument outside of range"
      f : Reference((Z,Z,Z) -> Z) := ref((n, v1, v2) +-> 0)
      f() := (n, v1, v2) +-> if n<2 then v2 else f()(n-1,v2,v1+v2)
      f()(x,1,1)
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This works by finding a pointer to the 'anonymous' function and calling it indirectly:

```bbcbasic
      PRINT FNfib(10)
      END

      DEF FNfib(n%) IF n%<0 THEN ERROR 100, "Must not be negative"
      LOCAL P% : P% = !384 + LEN$!384 + 4 : REM Function pointer
      (n%) IF n%<2 THEN = n% ELSE = FN(^P%)(n%-1) + FN(^P%)(n%-2)
```

{{out}}

```txt

        55

```



## Bracmat

===lambda 'light'===
The first solution uses macro substitution. In an expression headed by an apostrophe operator with an empty lhs all subexpressions headed by a dollar operator with empty lhs are replaced by the values that the rhs are bound to, without otherwise evaluating the expression. Example: if <code>(x=7) & (y=4)</code> then <code>'($x+3+$y)</code> becomes <code>=7+3+4</code>. Notice that the solution below utilises no other names than <code>arg</code>, the keyword that always denotes a function's argument. The test for negative or non-numeric arguments is outside the recursive part. The function fails if given negative input.

```bracmat
( (
  =
    .   !arg:#:~<0
      &   ( (=.!arg$!arg)
          $ (
            =
              .
                ' (
                  .   !arg:<2
                    |   (($arg)$($arg))$(!arg+-2)
                      + (($arg)$($arg))$(!arg+-1)
                  )
            )
          )
        $ !arg
  )
$ 30
)

```

Answer:

```txt
832040
```


### pure lambda calculus

(See http://en.wikipedia.org/wiki/Lambda_calculus). The following solution works almost the same way as the previous solution, but uses lambda calculus

```bracmat
( /(
   ' ( x
     .   $x:#:~<0
       &   ( /('(f.($f)$($f)))
           $ /(
              ' ( r
                . /(
                   ' ( n
                     .   $n:<2
                       |   (($r)$($r))$($n+-2)
                         + (($r)$($r))$($n+-1)
                     )
                   )
                )
              )
           )
         $ ($x)
     )
   )
$ 30
)
```

Answer:

```txt
832040
```



## C

Using scoped function fib_i inside fib, with GCC (required version 3.2 or higher):

```c
#include <stdio.h>

long fib(long x)
{
        long fib_i(long n) { return n < 2 ? n : fib_i(n - 2) + fib_i(n - 1); };
        if (x < 0) {
                printf("Bad argument: fib(%ld)\n", x);
                return -1;
        }
        return fib_i(x);
}

long fib_i(long n) /* just to show the fib_i() inside fib() has no bearing outside it */
{
        printf("This is not the fib you are looking for\n");
        return -1;
}

int main()
{
        long x;
        for (x = -1; x < 4; x ++)
                printf("fib %ld = %ld\n", x, fib(x));

        printf("calling fib_i from outside fib:\n");
        fib_i(3);

        return 0;
}
```

{{out}}

```txt
Bad argument: fib(-1)
fib -1 = -1
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib 3 = 2
calling fib_i from outside fib:
This is not the fib you are looking for
```



## C++

In C++ (as of the 2003 version of the standard, possibly earlier), we can declare class within a function scope. By giving that class a public static member function, we can create a function whose symbol name is only known to the function in which the class was derived.

```cpp
double fib(double n)
{
  if(n < 0)
  {
    throw "Invalid argument passed to fib";
  }
  else
  {
    struct actual_fib
    {
        static double calc(double n)
        {
          if(n < 2)
          {
            return n;
          }
          else
          {
            return calc(n-1) + calc(n-2);
          }
        }
    };

    return actual_fib::calc(n);
  }
}
```

{{works with|C++11}}

```cpp
#include <functional>
using namespace std;

double fib(double n)
{
  if(n < 0)
    throw "Invalid argument";

  function<double(double)> actual_fib = [&](double n)
  {
    if(n < 2) return n;
    return actual_fib(n-1) + actual_fib(n-2);
  };

  return actual_fib(n);
}
```


Using a local function object that calls itself using <code>this</code>:


```cpp
double fib(double n)
{
  if(n < 0)
  {
    throw "Invalid argument passed to fib";
  }
  else
  {
    struct actual_fib
    {
      double operator()(double n)
      {
        if(n < 2)
        {
          return n;
        }
        else
        {
          return (*this)(n-1) + (*this)(n-2);
        }
      }
    };

    return actual_fib()(n);
  }
}
```


## C#
The inner recursive function (delegate/lambda) has to be named.

```c#

static int Fib(int n)
{
    if (n < 0) throw new ArgumentException("Must be non negativ", "n");

    Func<int, int> fib = null; // Must be known, before we can assign recursively to it.
    fib = p => p > 1 ? fib(p - 2) + fib(p - 1) : p;
    return fib(n);
}

```



## Clio

Simple anonymous recursion to print from 9 to 0.

```clio
10 -> (@eager) fn n:
  if n:
    n - 1 -> print -> recall
```



## Clojure

The JVM as of now has no Tail call optimization so the default way of looping in Clojure uses anonymous recursion so not to be confusing.

```clojure

(defn fib [n]
  (when (neg? n)
    (throw (new IllegalArgumentException "n should be > 0")))
  (loop [n n, v1 1, v2 1]
    (if (< n 2)
      v2
      (recur (dec n) v2 (+ v1 v2)))))

```

Using an anonymous function


## CoffeeScript


```coffeescript
# This is a rather obscure technique to have an anonymous
# function call itself.
fibonacci = (n) ->
  throw "Argument cannot be negative" if n < 0
  do (n) ->
      return n if n <= 1
      arguments.callee(n-2) + arguments.callee(n-1)

# Since it's pretty lightweight to assign an anonymous
# function to a local variable, the idiom below might be
# more preferred.
fibonacci2 = (n) ->
  throw "Argument cannot be negative" if n < 0
  recurse = (n) ->
      return n if n <= 1
      recurse(n-2) + recurse(n-1)
  recurse(n)

```



## Common Lisp



### Using Anaphora

This version uses the anaphoric <code>lambda</code> from [http://dunsmor.com/lisp/onlisp/onlisp_18.html Paul Graham's On Lisp].


```lisp
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))
```


The Fibonacci function can then be defined as


```lisp
(defun fib (n)
  (assert (>= n 0) nil "'~a' is a negative number" n)
  (funcall
   (alambda (n)
     (if (>= 1 n)
	 n
	 (+ (self (- n 1)) (self (- n 2)))))
   n))
```



### Using labels


This puts a function in a local label. The function is not anonymous, but not only is it local, so that its name does not pollute the global namespace, but the name can be chosen to be identical to that of the surrounding function, so it is not a newly invented name


```lisp
(defun fib (number)
  "Fibonacci sequence function."
  (if (< number 0)
      (error "Error. The number entered: ~A is negative" number)
      (labels ((fib (n a b)
                 (if (= n 0)
                     a
                     (fib (- n 1) b (+ a b)))))
        (fib number 0 1))))
```

Although name space polution isn't an issue, in recognition of the obvious convenience of anonymous recursive helpers, here is another solution: add the language feature for anonymously recursive blocks: the operator RECURSIVE, with a built-in local operator RECURSE to make recursive calls.

Here is <code>fib</code> rewritten to use RECURSIVE:

```lisp
(defun fib (number)
  "Fibonacci sequence function."
  (if (< number 0)
      (error "Error. The number entered: ~A is negative" number)
      (recursive ((n number) (a 0) (b 1))
         (if (= n 0)
            a
            (recurse (- n 1) b (+ a b))))))
```

Implementation of RECURSIVE:

```lisp
(defmacro recursive ((&rest parm-init-pairs) &body body)
  (let ((hidden-name (gensym "RECURSIVE-")))
    `(macrolet ((recurse (&rest args) `(,',hidden-name ,@args)))
       (labels ((,hidden-name (,@(mapcar #'first parm-init-pairs)) ,@body))
         (,hidden-name ,@(mapcar #'second parm-init-pairs))))))
```

RECURSIVE works by generating a local function with LABELS, but with a machine-generated unique name. Furthermore, it provides syntactic sugar so that the initial call to the recursive function takes place implicitly, and the initial values are specified using LET-like syntax.  Of course, if RECURSIVE blocks are nested, each RECURSE refers to its own function. There is no way for an inner RECURSIVE to specify recursion to an other RECURSIVE. That is what names are for!

Exercises for reader:
# In the original <code>fib</code>, the recursive local function can obtain a reference to itself using <code>#'fib</code>. This would allow it to, for instance, <code>(apply #'fib list-of-args)</code>. Add a way for RECURSIVE blocks to obtain a reference to themselves.
# Add support for &optional and &rest parameters. Optional: also &key and &aux.
# Add LOOPBACK operator whose syntax resembles RECURSE, but which simply assigns the variables and performs a branch back to the top rather than a recursive call.
# Tail recursion optimization is compiler-dependent in Lisp. Modify RECURSIVE so that it walks the expressions and identifies tail-recursive RECURSE calls, rewriting these to use looping code. Be careful that unevaluated literal lists which resemble RECURSE calls are not rewritten, and that RECURSE calls belonging to any nested RECURSIVE invocation are not accidentally treated.


### Using the Y combinator



```lisp
(setf (symbol-function '!)  (symbol-function 'funcall)
      (symbol-function '!!) (symbol-function 'apply))

(defmacro ? (args &body body)
  `(lambda ,args ,@body))

(defstruct combinator
  (name     nil :type symbol)
  (function nil :type function))

(defmethod print-object ((combinator combinator) stream)
  (print-unreadable-object (combinator stream :type t)
    (format stream "~A" (combinator-name combinator))))

(defconstant +y-combinator+
  (make-combinator
   :name     'y-combinator
   :function (? (f) (! (? (g) (! g g))
                       (? (g) (! f (? (&rest a)
                                     (!! (! g g) a))))))))

(defconstant +z-combinator+
  (make-combinator
   :name     'z-combinator
   :function (? (f) (! (? (g) (! f (? (x) (! (! g g) x))))
                       (? (g) (! f (? (x) (! (! g g) x))))))))

(defparameter *default-combinator* +y-combinator+)

(defmacro with-y-combinator (&body body)
  `(let ((*default-combinator* +y-combinator+))
     ,@body))

(defmacro with-z-combinator (&body body)
  `(let ((*default-combinator* +z-combinator+))
     ,@body))

(defun x-call (x-function &rest args)
  (apply (funcall (combinator-function *default-combinator*) x-function) args))

(defmacro x-function ((name &rest args) &body body)
  `(lambda (,name)
     (lambda ,args
       (macrolet ((,name (&rest args)
                    `(funcall ,',name ,@args)))
         ,@body))))

(defmacro x-defun (name args &body body)
  `(defun ,name ,args
     (x-call (x-function (,name ,@args) ,@body) ,@args)))

;;;; examples

(x-defun factorial (n)
  (if (zerop n)
      1
      (* n (factorial (1- n)))))

(x-defun fib (n)
  (case n
    (0 0)
    (1 1)
    (otherwise (+ (fib (- n 1))
                  (fib (- n 2))))))
```



## D


```d
int fib(in uint arg) pure nothrow @safe @nogc {
    assert(arg >= 0);

    return function uint(in uint n) pure nothrow @safe @nogc {
        static immutable self = &__traits(parent, {});
        return (n < 2) ? n : self(n - 1) + self(n - 2);
    }(arg);
}

void main() {
    import std.stdio;

    39.fib.writeln;
}
```

{{out}}

```txt
63245986
```



### With Anonymous Class

In this version anonymous class is created, and by using opCall member function, the anonymous class object can take arguments and act like an anonymous function. The recursion is done by calling opCall inside itself.

```d
import std.stdio;

int fib(in int n) pure nothrow {
    assert(n >= 0);

    return (new class {
        static int opCall(in int m) pure nothrow {
            if (m < 2)
                return m;
            else
                return opCall(m - 1) + opCall(m - 2);
        }
    })(n);
}

void main() {
    writeln(fib(39));
}
```

The output is the same.

=={{header|Déjà Vu}}==

### With Y combinator


```dejavu
Y f:
	labda y:
		labda:
			f y @y
	call dup

labda fib n:
	if <= n 1:
		1
	else:
		fib - n 1
		fib - n 2
		+
Y
set :fibo

for j range 0 10:
	!print fibo j
```


### With <code>recurse</code>


```dejavu
fibo-2 n:
	n 0 1
	labda times back-2 back-1:
		if = times 0:
			back-2
		elseif = times 1:
			back-1
		elseif = times 2:
			+ back-1 back-2
		else:
			recurse -- times back-1 + back-1 back-2
	call

for j range 0 10:
	!print fibo-2 j
```


Note that this method starts from 0, while the previous starts from 1.


## Dylan

This puts a function in a local method binding. The function is not anonymous, but the name fib1 is local and never pollutes the outside namespace.


```dylan

define function fib (n)
  when (n < 0)
    error("Can't take fibonacci of negative integer: %d\n", n)
  end;
  local method fib1 (n, a, b)
    if (n = 0)
      a
    else
      fib1(n - 1, b, a + b)
    end
  end;
  fib1(n, 0, 1)
end

```



## EchoLisp

A '''named let''' provides a local lambda via a label.

```scheme

(define (fib n)
(let _fib ((a 1) (b 1) (n n))
		(if
		(<= n 1) a
		(_fib b (+ a b) (1- n)))))

```



## Ela

Using fix-point combinator:

```ela
fib n | n < 0 = fail "Negative n"
      | else = fix (\f n -> if n < 2 then n else f (n - 1) + f (n - 2)) n
```

Function 'fix' is defined in standard Prelude as follows:

```ela
fix f = f (& fix f)
```



## Elixir

With Y-Combinator:

```Elixir

fib = fn f -> (
      fn x -> if x == 0, do: 0, else: (if x == 1, do: 1, else: f.(x - 1) + f.(x - 2))	end
	)
end

y = fn x -> (
    fn f -> f.(f)
  end).(
    fn g -> x.(fn z ->(g.(g)).(z) end)
  end)
end

IO.inspect y.(&(fib.(&1))).(40)

```


{{out}}
102334155


## Elena

ELENA 4.x:

```elena
import extensions;

fib(n)
{
    if (n < 0)
        { InvalidArgumentException.raise() };

    ^ (n)
        {
            if (n > 1)
            {
                ^ this self(n - 2) + (this self(n - 1))
            }
            else
            {
                ^ n
            }
        }(n)
}

public program()
{
    for (int i := -1, i <= 10, i += 1)
    {
        console.print("fib(",i,")=");
        try
        {
            console.printLine(fib(i))
        }
        catch(Exception e)
        {
            console.printLine:"invalid"
        }
    };

    console.readChar()
}
```

{{out}}

```txt

fib(-1)=invalid
fib(0)=0
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



## Erlang

Two solutions. First fib that use the module to hide its helper. The helper also is called fib so there is no naming problem. Then fib_internal which has the helper function inside itself.


```Erlang

-module( anonymous_recursion ).
-export( [fib/1, fib_internal/1] ).

fib( N ) when N >= 0 ->
	fib( N, 1, 0 ).

fib_internal( N ) when N >= 0 ->
	Fun = fun (_F, 0, _Next, Acc ) -> Acc;
		(F, N, Next, Acc) -> F( F, N - 1, Acc+Next, Next )
		end,
	Fun( Fun, N, 1, 0 ).


fib( 0, _Next, Acc ) -> Acc;
fib( N, Next, Acc ) -> fib( N - 1, Acc+Next, Next ).


```


=={{header|F Sharp|F#}}==
'''Using a nested function:'''

The function 'fib2' is only visible inside the 'fib' function.

```fsharp
let fib = function
    | n when n < 0 -> None
    | n -> let rec fib2 = function
               | 0 | 1 -> 1
               | n -> fib2 (n-1) + fib2 (n-2)
            in Some (fib2 n)
```

'''Using a fixed point combinator:'''

```fsharp
let rec fix f x = f (fix f) x

let fib = function
    | n when n < 0 -> None
    | n -> Some (fix (fun f -> (function | 0 | 1 -> 1 | n -> f (n-1) + f (n-2))) n)
```

{{out}}
Both functions have the same output.

```fsharp
[-1..5] |> List.map fib |> printfn "%A"
[null; Some 1; Some 1; Some 2; Some 3; Some 5; Some 8]
```



## FBSL


```qbasic
#APPTYPE CONSOLE

FUNCTION Fibonacci(n)
	IF n < 0 THEN
		RETURN "Nuts!"
	ELSE
		RETURN Fib(n)
	END IF
	FUNCTION Fib(m)
		IF m < 2 THEN
			Fib = m
		ELSE
			Fib = Fib(m - 1) + Fib(m - 2)
		END IF
	END FUNCTION
END FUNCTION

PRINT Fibonacci(-1.5)
PRINT Fibonacci(1.5)
PRINT Fibonacci(13.666)

PAUSE
```

'''Output:'''
 Nuts!
 1.5
 484.082

 Press any key to continue...


## Factor

One would never use anonymous recursion. The better way defines a private word, like <code>fib2</code>, and recurse by name. This private word would pollute the namespace of one source file.

To achieve anonymous recursion, this solution has a recursive quotation.

```factor
USING: kernel math ;
IN: rosettacode.fibonacci.ar

: fib ( n -- m )
    dup 0 < [ "fib of negative" throw ] when
    [
        ! If n < 2, then drop q, else find q(n - 1) + q(n - 2).
        [ dup 2 < ] dip swap [ drop ] [
            [ [ 1 - ] dip dup call ]
            [ [ 2 - ] dip dup call ] 2bi +
        ] if
    ] dup call( n q -- m ) ;
```

The name ''q'' in the stack effect has no significance; <code>call( x x -- x )</code> would still work.

The recursive quotation has 2 significant disadvantages:

# To enable the recursion, a reference to the quotation stays on the stack. This ''q'' impedes access to other things on the stack. This solution must use <code>dip</code> and <code>swap</code> to move ''q'' out of the way. To simplify the code, one might move ''q'' to a local variable, but then the recursion would not be anonymous.
# Factor cannot infer the stack effect of a recursive quotation. The last line must have <code>call( n q -- m )</code> instead of plain <code>call</code>; but <code>call( n q -- m )</code> defers the stack effect check until runtime. So if the quotation has a wrong stack effect, the compiler would miss the error; only a run of <code>fib</code> would detect the error.


## Falcon

Falcon allows a function to refer to itself by use of the fself keyword which is always set to the currently executing function.

```falcon
function fib(x)
   if x < 0
      raise ParamError(description|"Negative argument invalid", extra|"Fibbonacci sequence is undefined for negative numbers")
   else
      return (function(y)
         if y == 0
            return 0
         elif y == 1
            return 1
         else
            return fself(y-1) + fself(y-2)
         end
      end)(x)
   end
end


try
>fib(2)
>fib(3)
>fib(4)
>fib(-1)
catch in e
> e
end
```

{{out}}

```txt

1
2
3
ParamError SS0000 at falcon.core.ParamError._init:(PC:ext.c): Negative argument invalid (Fibbonacci sequence is undefined for negative numbers)
  Traceback:
   falcon.core.ParamError._init:0(PC:ext.c)
   "/home/uDTVwo/prog.fam" prog.fib:3(PC:56)
   "/home/uDTVwo/prog.fam" prog.__main__:22(PC:132)

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Anonymous_recursion this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

Recursion is always anonymous in Forth, allowing it to be used in anonymous functions.  However, definitions can't be defined during a definition (there are no 'local functions'), and the data stack can't be portably used to get data into a definition being defined.
{{works with|SwiftForth}} - and any Forth in which colon-sys consumes zero cells on the data stack.

```forth
:noname ( n -- n' )
  dup 2 < ?exit
  1- dup recurse swap 1- recurse + ; ( xt )

: fib ( +n -- n' )
  dup 0< abort" Negative numbers don't exist."
  [ ( xt from the :NONAME above ) compile, ] ;
```

Portability is achieved with a once-off variable (or any temporary-use space with a constant address - i.e., not PAD):

```forth
( xt from :noname in the previous example )
variable pocket  pocket !
: fib ( +n -- n' )
  dup 0< abort" Negative numbers don't exist."
  [ pocket @ compile, ] ;
```

Currently, most Forths have started to support embedded definitions (shown here for iForth):

```forth
: fib ( +n -- )
	dup 0< abort" Negative numbers don't exist"
	[: dup 2 < ?exit  1- dup MYSELF swap 1- MYSELF + ;] execute . ;
```



## Fortran

Since a hidden named function instead of an anonymous one seems to be ok with implementors, here is the Fortran version:

```Fortran
integer function fib(n)
  integer, intent(in) :: n
  if (n < 0 ) then
    write (*,*) 'Bad argument: fib(',n,')'
    stop
  else
    fib = purefib(n)
  end if
contains
  recursive pure integer function purefib(n) result(f)
    integer, intent(in) :: n
    if (n < 2 ) then
      f = n
    else
      f = purefib(n-1) + purefib(n-2)
    end if
  end function purefib
end function fib
```



## FreeBASIC

FreeBASIC does not support nested functions, lambda expressions, functions inside nested types or even (in the default dialect) gosub.

However, for compatibility with old QB code, gosub can be used if one specifies the 'fblite', 'qb' or 'deprecated dialects:

```freebasic
' FB 1.05.0 Win64

#Lang "fblite"

Option Gosub  '' enables Gosub to be used

' Using gosub to simulate a nested function
Function fib(n As UInteger) As UInteger
  Gosub nestedFib
  Exit Function

  nestedFib:
  fib = IIf(n < 2, n, fib(n - 1) + fib(n - 2))
  Return
End Function

' This function simulates (rather messily) gosub by using 2 gotos and would therefore work
' even in the default dialect
Function fib2(n As UInteger) As UInteger
  Goto nestedFib

  exitFib:
  Exit Function

  nestedFib:
  fib2 = IIf(n < 2, n, fib2(n - 1) + fib2(n - 2))
  Goto exitFib
End Function

For i As Integer = 1 To 12
  Print fib(i); " ";
Next

Print

For j As Integer = 1 To 12
  Print fib2(j); " ";
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

1 1 2 3 5 8 13 21 34 55 89 144
1 1 2 3 5 8 13 21 34 55 89 144

```



## Go

Y combinator solution.  Go has no special support for anonymous recursion.

```go
package main

import "fmt"

func main() {
    for _, n := range []int{0, 1, 2, 3, 4, 5, 10, 40, -1} {
        f, ok := arFib(n)
        if ok {
            fmt.Printf("fib %d = %d\n", n, f)
        } else {
            fmt.Println("fib undefined for negative numbers")
        }
    }
}

func arFib(n int) (int, bool) {
    switch {
    case n < 0:
        return 0, false
    case n < 2:
        return n, true
    }
    return yc(func(recurse fn) fn {
        return func(left, term1, term2 int) int {
            if left == 0 {
                return term1+term2
            }
            return recurse(left-1, term1+term2, term1)
        }
    })(n-2, 1, 0), true
}

type fn func(int, int, int) int
type ff func(fn) fn
type fx func(fx) fn

func yc(f ff) fn {
    return func(x fx) fn {
        return x(x)
    }(func(x fx) fn {
        return f(func(a1, a2, a3 int) int {
            return x(x)(a1, a2, a3)
        })
    })
}
```

{{out}}

```txt

fib 0 = 0
fib 1 = 1
fib 2 = 1
fib 3 = 2
fib 4 = 3
fib 5 = 5
fib 10 = 55
fib 40 = 102334155
fib undefined for negative numbers

```



## Groovy

Groovy does not explicitly support anonymous recursion. This solution is a kludgy trick that takes advantage of the "owner" scoping variable (reserved word) for closures.

```groovy
def fib = {
    assert it > -1
    {i -> i < 2 ? i : {j -> owner.call(j)}(i-1) + {k -> owner.call(k)}(i-2)}(it)
}
```

Test:

```groovy
def fib0to20 = (0..20).collect(fib)
println fib0to20

try {
    println fib(-25)
} catch (Throwable e) {
    println "KABOOM!!"
    println e.message
}
```

{{out}}

```txt
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765]
KABOOM!!
assert it > -1
       |  |
       |  false
       -25
```



## Haskell

Haskell has two ways to use anonymous recursion. Both methods hide the 'anonymous' function from the containing module, however the first method is actually using a named function.

'''Named function:'''

We're defining a function 'real' which is only available from within the fib function.

```haskell
fib :: Integer -> Maybe Integer
fib n
  | n < 0 = Nothing
  | otherwise = Just $ real n
              where real 0 = 1
                    real 1 = 1
                    real n = real (n-1) + real (n-2)
```


'''Anonymous function:'''

This uses the 'fix' function to find the fixed point of the anonymous function.

```haskell
import Data.Function (fix)

fib :: Integer -> Maybe Integer
fib n
  | n < 0 = Nothing
  | otherwise = Just $ fix (\f -> (\n -> if n > 1 then f (n-1) + f (n-2) else 1)) n
```

{{out}}
Both functions provide the same output when run in GHCI.

```haskell>ghci
 map fib [-4..10]
[Nothing,Nothing,Nothing,Nothing,Just 1,Just 1,Just 2,Just 3,Just 5,Just 8,Just 13,Just 21,Just 34,Just 55,Just 89]
```


Or, without imports (inlining an anonymous fix)


```haskell
fib :: Integer -> Maybe Integer
fib n
  | n < 0 = Nothing
  | otherwise =
    Just $
    (\f ->
        let x = f x
        in x)
      (\f n ->
          if n > 1
            then f (n - 1) + f (n - 2)
            else 1)
      n

-- TEST ----------------------------------------------------------------------
main :: IO ()
main =
  print $
  fib <$> [-4 .. 10] >>=
  \m ->
     case m of
       Just x -> [x]
       _ -> []
```

{{Out}}

```txt
[1,1,2,3,5,8,13,21,34,55,89]
```


=={{header|Icon}} and {{header|Unicon}}==
The following solution works in both languages.  A cache is used to improve performance.

This example is more a case of can it even be done, and just because we CAN do something - doesn't mean we should do it.  The use of co-expressions for this purpose was probably never intended by the language designers and is more than a little bit intensive and definitely NOT recommended.

This example does accomplish the goals of hiding the procedure inside ''fib'' so that the type and value checking is outside the recursion.  It also does not require an identifier to reference the inner procedure; but, it requires a local variable to remember our return point.  Also, each recursion will result in the current co-expression being refreshed, essentially copied, placing a heavy demand on co-expression resources.

```Icon
procedure main(A)
   every write("fib(",a := numeric(!A),")=",fib(a))
end

procedure fib(n)
   local  source, i
   static cache
   initial {
      cache := table()
      cache[0] := 0
      cache[1] := 1
      }
   if type(n) == "integer" & n >= 0 then
      return n @ makeProc {{
         i := @(source := &source)                                          # 1
         /cache[i] := ((i-1)@makeProc(^&current)+(i-2)@makeProc(^&current)) # 2
         cache[i] @ source                                                  # 3
         }}
end

procedure makeProc(A)
   A := if type(A) == "list" then A[1]
   return (@A, A)                    # prime and return
end
```

Some of the code requires some explaining:
* The double curly brace syntax after ''makeProc'' serves two different purposes, the outer set is used in the call to create a co-expression.  The inner one binds all the expressions together as a single unit.
* At #1 we remember where we came from and receive ''n'' from our caller
* At #2 we transmit the new parameters to refreshed copies of the current co-expression setup to act as a normal procedure and cache the result.
* At #3 we transmit the result back to our caller.
* The procedure ''makeProc'' consumes the the first transmission to the co-expression which is ignored.  Essentially this primes the co-expression to behave like a regular procedure.

For reference, here is the non-cached version:

```Icon
procedure fib(n)
   local  source, i
   if type(n) == "integer" & n >= 0 then
      return n @ makeProc {{
         i := @(source := &source)
         if i = (0|1) then i@source
         ((i-1)@makeProc(^&current) + (i-2)@makeProc(^&current)) @ source
         }}
end
```

The performance of this second version is 'truly impressive'.  And I mean that in a really bad way.  By way of example, using default memory settings on a current laptop, a simple recursive non-cached ''fib'' out distanced the non-cached ''fib'' above by a factor of 20,000.  And a simple recursive cached version out distanced the cached version above by a factor of 2,000.


## Io

The most natural way to solve this task is to use a nested function whose scope is limited to the helper function.

```Io
fib := method(x,
    if(x < 0, Exception raise("Negative argument not allowed!"))
    fib2 := method(n,
        if(n < 2, n, fib2(n-1) + fib2(n-2))
    )
    fib2(x floor)
)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Fibonacc.bas"
110 FOR I=0 TO 10
120   PRINT FIB(I);
130 NEXT
140 DEF FIB(K)
150   SELECT CASE K
160   CASE IS<0
170     PRINT "Negative parameter to Fibonacci.":STOP
180   CASE 0
190     LET FIB=0
200   CASE 1
210     LET FIB=1
220   CASE ELSE
230     LET FIB=FIB(K-1)+FIB(K-2)
240   END SELECT
250 END DEF
```



## J

Copied directly from the [[Fibonacci_sequence#J|fibonacci sequence]] task, which in turn copied from one of several implementations in an [[j:Essays/Fibonacci_Sequence|essay]] on the J Wiki:

```j
   fibN=: (-&2 +&$: -&1)^:(1&<) M."0
```

Note that this is an identity function for arguments less than 1 (and 1 (and 5)).

'''Examples:'''

```j
   fibN 12
144
   fibN  i.31
0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040
```

(This implementation is doubly recursive except that results are cached across function calls.)

<code>$:</code> is an anonymous reference to the largest containing verb in the sentence.

-------------

Note also http://www.jsoftware.com/pipermail/general/2003-August/015571.html which points out that the form


```j
basis ` ($: @: g) @. test
```
 which is an anonymous form matches the "tail recursion" pattern is not automatically transformed to satisfy the classic "tail recursion optimization". That optimization would be implemented as transforming this particular example of recursion to the non-recursive
```j
basis @: (g^:test^:_)
```


Of course, that won't work here, because we are adding two recursively obtained results where tail recursion requires that the recursive result is the final result.


## Java

Creates an anonymous inner class to do the dirty work. While it does keep the recursive function out of the namespace of the class, it does seem to violate the spirit of the task in that the function is still named.


```java
public static long fib(int n) {
    if (n < 0)
        throw new IllegalArgumentException("n can not be a negative number");

    return new Object() {
        private long fibInner(int n) {
            return (n < 2) ? n : (fibInner(n - 1) + fibInner(n - 2));
        }
    }.fibInner(n);
}
```


Another way is to use the Java Y combinator implementation (the following uses the Java 8 version for better readability).
Note that the fib method below is practically the same as that of the version above, with less fibInner.


```java5
import java.util.function.Function;

@FunctionalInterface
interface SelfApplicable<OUTPUT> {
    OUTPUT apply(SelfApplicable<OUTPUT> input);
}

class Utils {
    public static <INPUT, OUTPUT> SelfApplicable<Function<Function<Function<INPUT, OUTPUT>, Function<INPUT, OUTPUT>>, Function<INPUT, OUTPUT>>> y() {
        return y -> f -> x -> f.apply(y.apply(y).apply(f)).apply(x);
    }

    public static <INPUT, OUTPUT> Function<Function<Function<INPUT, OUTPUT>, Function<INPUT, OUTPUT>>, Function<INPUT, OUTPUT>> fix() {
        return Utils.<INPUT, OUTPUT>y().apply(Utils.<INPUT, OUTPUT>y());
    }

    public static long fib(int m) {
        if (m < 0)
            throw new IllegalArgumentException("n can not be a negative number");

        return Utils.<Integer, Long>fix().apply(
                f -> n -> (n < 2) ? n : (f.apply(n - 1) + f.apply(n - 2))
        ).apply(m);
    }
}
```



## JavaScript


```javascript
function fibo(n) {
  if (n < 0) { throw "Argument cannot be negative"; }

  return (function(n) {
    return (n < 2) ? 1 : arguments.callee(n-1) + arguments.callee(n-2);
  })(n);
}
```

Note that <code>arguments.callee</code> will not be available in ES5 Strict mode. Instead, you are expected to "name" function (the name is only visible inside function however).

```javascript
function fibo(n) {
  if (n < 0) { throw "Argument cannot be negative"; }

  return (function fib(n) {
    return (n < 2) ? 1 : fib(n-1) + fib(n-2);
  })(n);
}
```



## jq

The "recurse" filter supports a type of anonymous recursion, e.g. to generate a stream of integers starting at 0:

```jq
0 | recurse(. + 1)
```


Also, as is the case for example with Julia, jq allows you to define an inner/nested function (in the follow example, <code>aux</code>) that is only defined within the scope of the surrounding function (here <code>fib</code>).  It is thus invisible outside the function:

```jq
def fib(n):
  def aux: if   . == 0 then 0
           elif . == 1 then 1
           else (. - 1 | aux) + (. - 2 | aux)
           end;
  if n < 0 then error("negative arguments not allowed")
  else n | aux
  end ;
```



## Julia

Julia allows you to define an inner/nested function (here, <code>aux</code>) that is only defined within the surrounding function <code>fib</code> scope.

```julia
function fib(n)
    if n < 0
        throw(ArgumentError("negative arguments not allowed"))
    end
    aux(m) = m < 2 ? one(m) : aux(m-1) + aux(m-2)
    aux(n)
end
```



## K


```k
fib: {:[x<0; "Error Negative Number"; {:[x<2;x;_f[x-2]+_f[x-1]]}x]}
```

'''Examples:'''

```k
  fib'!10
0 1 1 2 3 5 8 13 21 34
  fib -1
"Error Negative Number"
```



## Klong


```K

fib::{:[x<0;"error: negative":|x<2;x;.f(x-1)+.f(x-2)]}

```



## Kotlin

{{trans|Dylan}}

```scala
fun fib(n: Int): Int {
   require(n >= 0)
   fun fib1(k: Int, a: Int, b: Int): Int =
       if (k == 0) a else fib1(k - 1, b, a + b)
   return fib1(n, 0, 1)
}

fun main(args: Array<String>) {
    for (i in 0..20) print("${fib(i)} ")
    println()
}
```


{{out}}

```txt

0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765

```



## Lambdatalk


```scheme

1) defining a tail-recursive function:
{def fibo {lambda {:n}
 {{lambda {:f :n :a :b} {:f :f :n :a :b}}
  {lambda {:f :n :a :b}
   {if {< :n 0}
    then the number must be positive!
    else {if {<  :n 1}
    then :a
    else {:f :f {- :n 1} {+ :a :b} :a}}}} :n 1 0}}}

2) testing:
{fibo -1} -> the number must be positive!
{fibo 0} -> 1
{fibo 8} -> 34
{fibo 1000} -> 7.0330367711422765e+208
{map fibo {serie 1 20}}
-> 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946

We could also avoid any name and write an IIFE

{{lambda {:n}
 {{lambda {:f :n :a :b} {:f :f :n :a :b}}
  {lambda {:f :n :a :b}
   {if {< :n 0}
    then the number must be positive!
    else {if {<  :n 1}
    then :a
    else {:f :f {- :n 1} {+ :a :b} :a}}}} :n 1 0}}
 8}
-> 34


```



## Lingo

Lingo does not support anonymous functions. But what comes close: you can create and instantiate an "anonymous class":

```lingo
on fib (n)
  if n<0 then return _player.alert("negative arguments not allowed")

  -- create instance of unnamed class in memory only (does not pollute namespace)
  m = new(#script)
  r = RETURN
  m.scriptText = "on fib (me,n)"&r&"if n<2 then return n"&r&"return me.fib(n-1)+me.fib(n-2)"&r&"end"
  aux = m.script.new()
  m.erase()

  return aux.fib(n)
end
```



```lingo
put fib(10)
-- 55
```



## LOLCODE

{{trans|C}}

```LOLCODE
HAI 1.3

HOW IZ I fib YR x
    DIFFRINT x AN BIGGR OF x AN 0, O RLY?
        YA RLY, FOUND YR "ERROR"
    OIC

    HOW IZ I fib_i YR n
        DIFFRINT n AN BIGGR OF n AN 2, O RLY?
            YA RLY, FOUND YR n
        OIC

        FOUND YR SUM OF...
        I IZ fib_i YR DIFF OF n AN 2 MKAY AN...
        I IZ fib_i YR DIFF OF n AN 1 MKAY
    IF U SAY SO

    FOUND YR I IZ fib_i YR x MKAY
IF U SAY SO

HOW IZ I fib_i YR n
    VISIBLE "SRY U CANT HAS FIBS DIS TIEM"
IF U SAY SO

IM IN YR fibber UPPIN YR i TIL BOTH SAEM i AN 5
    I HAS A i ITZ DIFF OF i AN 1
    VISIBLE "fib(:{i}) = " I IZ fib YR i MKAY
IM OUTTA YR fibber

I IZ fib_i YR 3 MKAY

KTHXBYE
```



## Lua

Using a [[Y combinator]].

```lua
local function Y(x) return (function (f) return f(f) end)(function(y) return x(function(z) return y(y)(z) end) end) end

return Y(function(fibs)
  return function(n)
    return n < 2 and 1 or fibs(n - 1) + fibs(n - 2)
  end
end)
```

using a metatable (also achieves memoization)

```lua
return setmetatable({1,1},{__index = function(self, n)
  self[n] = self[n-1] + self[n-2]
  return self[n]
end})
```



## M2000 Interpreter

We can use a function in string. We can named it so the error say about "Fibonacci"
To exclude first check for negative we have to declare a function in anonymous function, which may have a name (a local name)

```M2000 Interpreter

A$={{ Module "Fibonacci" : Read X  :If X<0 then {Error {X<0}} Else  Fib=Lambda (x)->if(x>1->fib(x-1)+fib(x-2), x) : =fib(x)}}
Try Ok {
      Print Function(A$, -12)
}
If Error or Not Ok Then Print Error$
Print Function(A$, 12)=144 ' true

```


For recursion we can use Lambda() or Lambda$() (for functions which return string) and not name of function so we can use it in a referenced function. Here in k() if we have the fib() we get an error, but with lambda(), interpreter use current function's name.


```M2000 Interpreter

Function fib(x) {
      If x<0 then Error "argument outside of range"
      If x<2 then =x : exit
      Def fib1(x)=If(x>1->lambda(x-1)+lambda(x-2), x)
      =fib1(x)
}
Module CheckIt (&k()) {
      Print k(12)
}
CheckIt &Fib()
Print fib(-2)  ' error

```


Using lambda function


```M2000 Interpreter

fib=lambda -> {
       fib1=lambda (x)->If(x>1->lambda(x-1)+lambda(x-2), x)
      =lambda fib1 (x) -> {
            If x<0 then Error "argument outside of range"
            If x<2 then =x : exit
            =fib1(x)
      }
}()  ' using () execute this lambda so fib get the returned lambda
Module  CheckIt (&k()) {
      Print k(12)
}
CheckIt &Fib()
Try {
      Print fib(-2)
}
Print Error$
Z=Fib
Print Z(12)
Dim a(10)
a(3)=Z
Print a(3)(12)=144
Inventory Alfa = "key1":=Z
Print Alfa("key1")(12)=144

```


Using a Group (object in M2000) like a function

A Group may have a name like k (which hold a unique group), or can be unnamed like item in A(4), or can be pointed by a variable (or an array item) (we can use many pointers for the same group)



```M2000 Interpreter

Class Something {
\\ this class is a global function
\\ return a group with a value with one parameter
private:
      \\ we can use lambda(), but here we use .fib1() as This.fib1()
       fib1=lambda (x)->If(x>1->.fib1(x-1)+.fib1(x-2), x)
public:
      Value (x) {
            If x<0 then Error "argument outside of range"
            If x<2 then =x : exit
            =This.fib1(x)            \\ we can omit This using .fib1(x)
      }
}
K=Something()     ' K is a static group here
Print k(12)=144
Dim a(10)
a(4)=Group(K)
Print a(4)(12)=144
pk->Something()   ' pk is a pointer to group (object in M2000)
\\ pointers need Eval to process arguments
Print Eval(pk, 12)=144
Inventory Alfa = "Key2":=Group(k), 10*10:=pk
Print Alfa("Key2")(12)=144
Print Eval(Alfa("100"),12)=144, Eval(Alfa(100),12)=144

```



## Maple

In Maple, the keyword thisproc refers to the currently executing procedure (closure), which need not be named.  The following defines a procedure Fib, which uses a recursive, anonymous (unnamed) procedure to implement the Fibonacci sequence.   For better efficiency, we use Maple's facility for automatic memoisation ("option remember").

```Maple

Fib := proc( n :: nonnegint )
        proc( k )
                option  remember; # automatically memoise
                if k = 0 then
                        0
                elif k = 1 then
                        1
                else
                        # Recurse, anonymously
                        thisproc( k - 1 ) + thisproc( k - 2 )
                end
        end( n )
end proc:

```

For example:

```Maple

> seq( Fib( i ), i = 0 .. 10 );
                  0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55

> Fib( -1 );
Error, invalid input: Fib expects its 1st argument, n, to be of type
nonnegint, but received -1

```

The check for a negative argument could be put either on the outer Fib procedure, or the anonymous inner procedure (or both).  As it wasn't completely clear what was intended, I put it on Fib, which results in a slightly better error message in that it does not reveal how the procedure was actually implemented.

=={{header|Mathematica}} / {{header|Wolfram Language}}==
An anonymous reference to a function from within itself is named #0, arguments to that function are named #1,#2..#n, n being the position of the argument. The first argument may also be referenced as a # without a following number, the list of all arguments is referenced with ##. Anonymous functions are also known as [http://reference.wolfram.com/mathematica/tutorial/PureFunctions.html pure functions] in Mathematica.

```Mathematica
check := #<0&
fib := If[check[#],Throw["Negative Argument"],If[#<=1,1,#0[#-2]+#0[#-1]]&[#]]&
fib /@ Range[0,10]

{1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89}
```

Making sure that the check is only performed once.

```Mathematica
check := (Print[#];#<0)&
fib /@ Range[0,4]
0
1
2
3
4

{1, 1, 2, 3, 5}
```



## Nemerle

Not anonymous exactly, but using inner function solves all problems stated in task description.
* name is basically the same as outer function and doesn't pollute the namespace
* inner function not expected to be called from anywhere else
* nesting maintains program flow in source code

```Nemerle
using System;
using System.Console;

module Fib
{
    Fib(n : long) : long
    {
        def fib(m : long)
        {
            |0 => 1
            |1 => 1
            |_ => fib(m - 1) + fib(m - 2)
        }

        match(n)
        {
            |n when (n < 0) => throw ArgumentException("Fib() not defined on negative numbers")
            |_ => fib(n)
        }
    }

    Main() : void
    {
        foreach (i in [-2 .. 10])
        {
            try {WriteLine("{0}", Fib(i));}
            catch {|e is ArgumentException => WriteLine(e.Message)}
        }
    }
}
```



## Nim


```nim
# Using scoped function fibI inside fib
proc fib(x: int): int =
  proc fibI(n: int): int =
    if n < 2: n else: fibI(n-2) + fibI(n-1)
  if x < 0:
    raise newException(ValueError, "Invalid argument")
  return fibI(x)

for i in 0..4:
  echo fib(i)

# fibI(10) # undeclared identifier 'fibI'
```

Output:

```txt
0
1
1
2
3
```


=={{header|Objective-C}}==
This shows how a method (not regular function) can recursively call itself without explicitly putting its name in the code.

```objc>#import <Foundation/Foundation.h


@interface AnonymousRecursion : NSObject { }
- (NSNumber *)fibonacci:(NSNumber *)n;
@end

@implementation AnonymousRecursion
- (NSNumber *)fibonacci:(NSNumber *)n {
  int i = [n intValue];
  if (i < 0)
    @throw [NSException exceptionWithName:NSInvalidArgumentException
                                 reason:@"fibonacci: no negative numbers"
                               userInfo:nil];
  int result;
  if (i < 2)
    result = 1;
  else
    result = [[self performSelector:_cmd withObject:@(i-1)] intValue]
           + [[self performSelector:_cmd withObject:@(i-2)] intValue];
  return @(result);
}
@end

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    AnonymousRecursion *dummy = [[AnonymousRecursion alloc] init];
    NSLog(@"%@", [dummy fibonacci:@8]);

  }
  return 0;
}
```


;With internal named recursive block:
{{works with|Mac OS X|10.6+}}

```objc>#import <Foundation/Foundation.h


int fib(int n) {
    if (n < 0)
        @throw [NSException exceptionWithName:NSInvalidArgumentException
                                 reason:@"fib: no negative numbers"
                               userInfo:nil];
    int (^f)(int);
    __block __weak int (^weak_f)(int); // block cannot capture strong reference to itself
    weak_f = f = ^(int n) {
        if (n < 2)
            return 1;
        else
            return weak_f(n-1) + weak_f(n-2);
    };
    return f(n);
}

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    NSLog(@"%d", fib(8));

  }
  return 0;
}
```


When ARC is disabled, the above should be:

```objc>#import <Foundation/Foundation.h


int fib(int n) {
    if (n < 0)
        @throw [NSException exceptionWithName:NSInvalidArgumentException
                                 reason:@"fib: no negative numbers"
                               userInfo:nil];
    __block int (^f)(int);
    f = ^(int n) {
        if (n < 2)
            return 1;
        else
            return f(n-1) + f(n-2);
    };
    return f(n);
}

int main (int argc, const char *argv[]) {
  @autoreleasepool {

    NSLog(@"%d", fib(8));

  }
  return 0;
}
```



## OCaml

{{trans|Haskell}}
OCaml has two ways to use anonymous recursion. Both methods hide the 'anonymous' function from the containing module, however the first method is actually using a named function.

'''Named function:'''

We're defining a function 'real' which is only available from within the fib function.

```ocaml
let fib n =
  let rec real = function
      0 -> 1
    | 1 -> 1
    | n -> real (n-1) + real (n-2)
  in
  if n < 0 then
    None
  else
    Some (real n)
```


'''Anonymous function:'''

This uses the 'fix' function to find the fixed point of the anonymous function.

```ocaml
let rec fix f x = f (fix f) x

let fib n =
  if n < 0 then
    None
  else
    Some (fix (fun f -> fun n -> if n <= 1 then 1 else f (n-1) + f (n-2)) n)
```

{{out}}

```txt
# fib 8;;
- : int option = Some 34
```



## Ol

This uses named let to create a local function (loop) that only exists inside of function fibonacci.

```scheme

(define (fibonacci n)
   (if (> 0 n)
      "error: negative argument."
      (let loop ((a 1) (b 0) (count n))
         (if (= count 0)
            b
            (loop (+ a b) a (- count 1))))))

(print
   (map fibonacci '(1 2 3 4 5 6 7 8 9 10)))

```

{{out}}

```txt
'(1 1 2 3 5 8 13 21 34 55)
```



## OxygenBasic

An inner function keeps the name-space clean:

```oxygenbasic

function fiboRatio() as double
    function fibo( double i, j ) as double
        if j > 2e12 then return j / i
        return fibo j, i + j
    end function
    return fibo 1, 1
end function

print fiboRatio


```



## PARI/GP

This version uses a Y combinator to get a self-reference.

```parigp
Fib(n)={
  my(F=(k,f)->if(k<2,k,f(k-1,f)+f(k-2,f)));
  if(n<0,(-1)^(n+1),1)*F(abs(n),F)
};
```


{{works with|PARI/GP|2.8.1+}}
This version gets a self-reference from <code>self()</code>.

```parigp
Fib(n)={
  my(F=k->my(f=self());if(k<2,k,f(k-1)+f(k-2)));
  if(n<0,(-1)^(n+1),1)*F(abs(n))
};
```



## Perl

{{trans|PicoLisp}}
<code>recur</code> isn't built into Perl, but it's easy to implement.

```perl
sub recur (&@) {
    my $f = shift;
    local *recurse = $f;
    $f->(@_);
}

sub fibo {
    my $n = shift;
    $n < 0 and die 'Negative argument';
    recur {
        my $m = shift;
        $m < 3 ? 1 : recurse($m - 1) + recurse($m - 2);
    } $n;
}
```

Although for this task, it would be fine to use a lexical variable (closure) to hold an anonymous sub reference, we can also just push it onto the args stack and use it from there:

```perl
sub fib {
	my ($n) = @_;
	die "negative arg $n" if $n < 0;
	# put anon sub on stack and do a magic goto to it
	@_ = ($n, sub {
		my ($n, $f) = @_;
		# anon sub recurs with the sub ref on stack
		$n < 2 ? $n : $f->($n - 1, $f) + $f->($n - 2, $f)
	});
	goto $_[1];
}

print(fib($_), " ") for (0 .. 10);
```

One can also use <code>caller</code> to get the name of the current subroutine as a string, then call the sub with that string. But this won't work if the current subroutine is anonymous: <code>caller</code> will just return <code>'__ANON__'</code> for the name of the subroutine. Thus, the below program must check the sign of the argument every call, failing the task. Note that under stricture, the line <code>no strict 'refs';</code> is needed to permit using a string as a subroutine.

```perl
sub fibo {
    my $n = shift;
    $n < 0 and die 'Negative argument';
    no strict 'refs';
    $n < 3 ? 1 : (caller(0))[3]->($n - 1) + (caller(0))[3]->($n - 2);
}
```

===Perl 5.16 and __SUB__===
Perl 5.16 introduced __SUB__ which refers to the current subroutine.

```Perl
use v5.16;
say sub {
  my $n = shift;
  $n < 2 ? $n : __SUB__->($n-2) + __SUB__->($n-1)
}->($_) for 0..10
```



## Perl 6

{{works with|Rakudo|2015.12}}

In addition to the methods in the [[Perl]] entry above, and the Y-combinator described in [[Y_combinator]], you may also refer to an anonymous block or function from the inside:

```perl6
sub fib($n) {
    die "Naughty fib" if $n < 0;
    return {
        $_ < 2
            ?? $_
            !!  &?BLOCK($_-1) + &?BLOCK($_-2);
    }($n);
}

say fib(10);
```

However, using any of these methods is insane, when Perl 6 provides a sort of inside-out combinator that lets you define lazy infinite constants, where the demand for a particular value is divorced from dependencies on more primitive values.  This operator, known as the sequence operator, does in a sense provide anonymous recursion to a closure that refers to more primitive values.

```perl6
constant @fib = 0, 1, *+* ... *;
say @fib[10];
```

Here the closure, <tt>*+*</tt>, is just a quick way to write a lambda, <tt>-> $a, $b { $a + $b }</tt>.  The sequence operator implicitly maps the two arguments to the -2nd and -1st elements of the sequence.  So the sequence operator certainly applies an anonymous lambda, but whether it's recursion or not depends on whether you view a sequence as iteration or as simply a convenient way of memoizing a recursion.  Either view is justifiable.

At this point someone may complain that the solution is doesn't fit the specified task because the sequence operator doesn't do the check for negative.  True, but the sequence operator is not the whole of the solution; this check is supplied by the subscripting operator itself when you ask for <tt>@fib[-1]</tt>.  Instead of scattering all kinds of arbitrary boundary conditions throughout your functions, the sequence operator maps them quite naturally to the boundary of definedness at the start of a list.


## PHP

In this solution, the function is always called using <code>call_user_func()</code> rather than using function call syntax directly. Inside the function, we get the function itself (without having to refer to the function by name) by relying on the fact that this function must have been passed as the first argument to <code>call_user_func()</code> one call up on the call stack. We can then use <code>debug_backtrace()</code> to get this out.
{{works with|PHP|5.3+}}

```php
<?php
function fib($n) {
    if ($n < 0)
        throw new Exception('Negative numbers not allowed');
    $f = function($n) { // This function must be called using call_user_func() only
        if ($n < 2)
            return 1;
        else {
            $g = debug_backtrace()[1]['args'][0];
            return call_user_func($g, $n-1) + call_user_func($g, $n-2);
        }
    };
    return call_user_func($f, $n);
}
echo fib(8), "\n";
?>
```


;With internal named recursive function:
{{works with|PHP|5.3+}}

```php
<?php
function fib($n) {
    if ($n < 0)
        throw new Exception('Negative numbers not allowed');
    $f = function($n) use (&$f) {
        if ($n < 2)
            return 1;
        else
            return $f($n-1) + $f($n-2);
    };
    return $f($n);
}
echo fib(8), "\n";
?>
```


;With a function object that can call itself using <code>$this</code>:
{{works with|PHP|5.3+}}

```php
<?php
class fib_helper {
    function __invoke($n) {
        if ($n < 2)
            return 1;
        else
            return $this($n-1) + $this($n-2);
    }
}

function fib($n) {
    if ($n < 0)
        throw new Exception('Negative numbers not allowed');
    $f = new fib_helper();
    return $f($n);
}
echo fib(8), "\n";
?>
```



## PicoLisp


```PicoLisp
(de fibo (N)
   (if (lt0 N)
      (quit "Illegal argument" N) )
   (recur (N)
      (if (> 2 N)
         1
         (+ (recurse (dec N)) (recurse (- N 2))) ) ) )
```

Explanation: The above uses the '[http://software-lab.de/doc/refR.html#recur recur]' / '[http://software-lab.de/doc/refR.html#recurse recurse]' function pair, which is defined as a standard language extensions as

```PicoLisp
(de recur recurse
   (run (cdr recurse)) )
```

Note how 'recur' dynamically defines the function 'recurse' at runtime, by binding the rest of the expression (i.e. the body of the 'recur' statement) to the symbol 'recurse'.


## PostScript

{{libheader|initlib}}
Postscript can make use of the higher order combinators to provide recursion.

```postscript
% primitive recursion
/pfact {
  {1} {*} primrec}.

%linear recursion
/lfact {
   {dup 0 eq}
   {pop 1}
   {dup pred}
   {*}
   linrec}.

% general recursion
/gfact {
    {0 eq}
    {succ}
    {dup pred}
    {i *}
    genrec}.

% binary recursion
/fib {
    {2 lt} {} {pred dup pred} {+} binrec}.
```



## Prolog

Works with SWI-Prolog and module <b>lambda</b>, written by <b>Ulrich Neumerkel</b> found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl
The code is inspired from this page : http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/ISO-Hiord#Hiord (p 106). It uses the Y combinator.

```prolog
:- use_module(lambda).

fib(N, _F) :-
	N < 0, !,
	write('fib is undefined for negative numbers.'), nl.

fib(N, F) :-
    % code of Fibonacci
    PF     = \Nb^R^Rr1^(Nb < 2 ->
			  R = Nb
                        ;
			  N1 is Nb - 1,
			  N2 is Nb - 2,
			  call(Rr1,N1,R1,Rr1),
			  call(Rr1,N2,R2,Rr1),
			  R is R1 + R2
			),

    % The Y combinator.

    Pred = PF +\Nb2^F2^call(PF,Nb2,F2,PF),

    call(Pred,N,F).
```



## Python


```python>>>
 Y = lambda f: (lambda x: x(x))(lambda y: f(lambda *args: y(y)(*args)))
>>> fib = lambda f: lambda n: None if n < 0 else (0 if n == 0 else (1 if n == 1 else f(n-1) + f(n-2)))
>>> [ Y(fib)(i) for i in range(-2, 10) ]
[None, None, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```


Same thing as the above, but modified so that the function is uncurried:

```python>>>
from functools import partial
>>> Y = lambda f: (lambda x: x(x))(lambda y: partial(f, lambda *args: y(y)(*args)))
>>> fib = lambda f, n: None if n < 0 else (0 if n == 0 else (1 if n == 1 else f(n-1) + f(n-2)))
>>> [ Y(fib)(i) for i in range(-2, 10) ]
[None, None, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```


A different approach: the function always receives itself as the first argument, and when recursing, makes sure to pass the called function as the first argument also

```python>>>
 from functools import partial
>>> Y = lambda f: partial(f, f)
>>> fib = lambda f, n: None if n < 0 else (0 if n == 0 else (1 if n == 1 else f(f, n-1) + f(f, n-2)))
>>> [ Y(fib)(i) for i in range(-2, 10) ]
[None, None, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```


An interesting approach using introspection  (from http://metapython.blogspot.com/2010/11/recursive-lambda-functions.html)

```python

>>> from inspect import currentframe
>>> from types import FunctionType
>>> def myself (*args, **kw):
...    caller_frame = currentframe(1)
...    code = caller_frame.f_code
...    return FunctionType(code, caller_frame.f_globals)(*args, **kw)
...
>>> print "factorial(5) =",
>>> print (lambda n:1 if n<=1 else n*myself(n-1)) ( 5 )

```


Another way of implementing the "Y" function is given in this post: https://stackoverflow.com/questions/481692/can-a-lambda-function-call-itself-recursively-in-python. The main problem to solve is that the function "fib" can't call itself. Therefore, the function "Y" is used to help "fib" call itself.


```python

>>> Y = lambda f: lambda n: f(f,n)
>>> fib = lambda f, n: None if n < 0 else (0 if n == 0 else (1 if n == 1 else f(f,n-1) + f(f,n-2))) #same as the first three implementations
>>> [ Y(fib)(i) for i in range(-2, 10) ]
[None, None, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

```


All in one line:

```python

>>> fib_func = (lambda f: lambda n: f(f,n))(lambda f, n: None if n < 0 else (0 if n == 0 else (1 if n == 1 else f(f,n-1) + f(f,n-2))))
>>> [ fib_func(i) for i in range(-2, 10) ]
[None, None, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

```



## Qi


Use of anonymous recursive functions is not common in Qi. The philosophy of Qi
seems to be that using a "silly name" like "foo2" or "foo_helper" makes the code clearer than using anonymous recursive functions.

However, it can be done, for instance like this:


```Qi

(define fib
  N -> (let A (/. A N
                  (if (< N 2)
                      N
                      (+ (A A (- N 2))
                         (A A (- N 1)))))
         (A A N)))

```



## R

R provides Recall() as a wrapper which finds the calling function, with limitations; Recall will not work if passed to another function as an argument.

```R
fib2 <- function(n) {
  (n >= 0) || stop("bad argument")
  ( function(n) if (n <= 1) 1 else Recall(n-1)+Recall(n-2) )(n)
}
```



## Racket


In Racket, local helper function definitions inside of a function are only visible locally and do not pollute the module or global scope.


```racket

#lang racket

;; Natural -> Natural
;; Calculate factorial
(define (fact n)
  (define (fact-helper n acc)
    (if (= n 0)
        acc
        (fact-helper (sub1 n) (* n acc))))
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'fact "natural" n))
  (fact-helper n 1))

;; Unit tests, works in v5.3 and newer
(module+ test
  (require rackunit)
  (check-equal? (fact 0) 1)
  (check-equal? (fact 5) 120))

```


This calculates the slightly more complex Fibonacci funciton:

```racket

#lang racket
;; Natural -> Natural
;; Calculate fibonacci
(define (fibb n)
  (define (fibb-helper n fibb_n-1 fibb_n-2)
    (if (= 1 n)
        fibb_n-1
        (fibb-helper (sub1 n) (+ fibb_n-1 fibb_n-2) fibb_n-1)))
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'fibb "natural" n))
  (if (zero? n) 0 (fibb-helper n 1 0)))

;; Unit tests, works in v5.3 and newer
(module+ test
  (require rackunit)
  (check-exn exn:fail? (lambda () (fibb -2)))
  (check-equal?
   (for/list ([i (in-range 21)]) (fibb i))
   '(0 1 1 2 3 5 8 13 21 34 55 89 144 233
       377 610 987 1597 2584 4181 6765)))

```


Also with the help of first-class functions in Racket, anonymous recursion can be implemented using fixed-points operators:


```racket

#lang racket
;; We use Z combinator (applicative order fixed-point operator)
(define Z
  (λ (f)
    ((λ (x) (f (λ (g) ((x x) g))))
     (λ (x) (f (λ (g) ((x x) g)))))))

(define fibonacci
  (Z (λ (fibo)
       (λ (n)
         (if (<= n 2)
             1
             (+ (fibo (- n 1))
                (fibo (- n 2))))))))

```



```txt

> (fibonacci -2)
1
> (fibonacci 5)
5
> (fibonacci 10)
55

```



## REBOL



```rebol

fib: func [n /f][ do f: func [m] [ either m < 2 [m][(f m - 1) + f m - 2]] n]

```



## REXX

[Modeled after the Fortran example.]


Since a hidden named function (instead of an anonymous function) seems
to be OK with the implementers, here are the REXX versions.

### simplistic


```rexx
/*REXX program to show anonymous recursion  (of a function or subroutine).              */
numeric digits 1e6                               /*in case the user goes ka-razy with X.*/
parse arg x .                                    /*obtain the optional argument from CL.*/
if x=='' | x==","  then x=12                     /*Not specified?  Then use the default.*/
w=length(x)                                      /*W:  used for formatting the output.  */
                   do j=0  for x+1               /*use the  argument  as an upper limit.*/
                   say 'fibonacci('right(j, w)") ="   fib(j)
                   end  /*j*/                    /* [↑] show Fibonacci sequence: 0 ──► X*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fib: procedure; parse arg z;  if z>=0  then return .(z)
                              say "***error***  argument can't be negative.";   exit
.:   procedure; parse arg #;  if #<2  then return #;              return .(#-1)  +  .(#-2)
```

'''output'''   when using the default input of:   <tt> 12 </tt>

```txt

fibonacci( 0) = 0
fibonacci( 1) = 1
fibonacci( 2) = 1
fibonacci( 3) = 2
fibonacci( 4) = 3
fibonacci( 5) = 5
fibonacci( 6) = 8
fibonacci( 7) = 13
fibonacci( 8) = 21
fibonacci( 9) = 34
fibonacci(10) = 55
fibonacci(11) = 89
fibonacci(12) = 144

```



### memoization

Since the above REXX version is   ''very''   slow for larger numbers, the following version was added that incorporates memoization.

It's many orders of magnitude faster for larger values.

```rexx
/*REXX program to show anonymous recursion of a function or subroutine with memoization.*/
numeric digits 1e6                               /*in case the user goes ka-razy with X.*/
parse arg x .                                    /*obtain the optional argument from CL.*/
if x=='' | x==","  then x=12                     /*Not specified?  Then use the default.*/
@.=.;    @.0=0;    @.1=1                         /*used to implement memoization for FIB*/
w=length(x)                                      /*W:  used for formatting the output.  */
                   do j=0  for x+1               /*use the  argument  as an upper limit.*/
                   say 'fibonacci('right(j, w)") ="   fib(j)
                   end  /*j*/                    /* [↑] show Fibonacci sequence: 0 ──► X*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fib: procedure expose @.; arg z;  if z>=0  then return .(z)
                          say "***error***  argument can't be negative.";   exit
.: procedure expose @.; arg #; if @.#\==.  then return @.#;  @.#=.(#-1)+.(#-2); return @.#
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.




## Ring


```ring

# Project : Anonymous recursion

t=0
for x = -2 to 12
     n = x
     recursion()
     if x > -1
        see t + nl
     ok
next

func recursion()
        nold1=1
        nold2=0
        if n < 0
           see "positive argument required!" + nl
           return
        ok
        if n=0
           t=nold2
           return t
        ok
        if n=1
           t=nold1
           return  t
        ok
        while n
                  t=nold2+nold1
                  if n>2
                     n=n-1
                     nold2=nold1
                     nold1=t
                     loop
                  ok
                  return t
        end
        return t

```

Output:

```txt

positive argument required!
positive argument required!
0
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
89
144

```



## Ruby

Ruby has no keyword for anonymous recursion.

We can recurse a block of code, but we must provide the block with a reference to itself. The easiest solution is to use a local variable.

### Ruby with local variable


```ruby
def fib(n)
  raise RangeError, "fib of negative" if n < 0
  (fib2 = proc { |m| m < 2 ? m : fib2[m - 1] + fib2[m - 2] })[n]
end
```



```ruby
(-2..12).map { |i| fib i rescue :error }
=> [:error, :error, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
```


Here 'fib2' is a local variable of the fib() method. Only the fib() method, or a block inside the fib() method, can call this 'fib2'. The rest of this program cannot call this 'fib2', but it can use the name 'fib2' for other things.

* The fib() method has two local variables 'fib2' and 'n'.
* The block has a local variable 'm' and closes on both 'fib2' and 'n'.

'''Caution!''' The recursive block has a difference from Ruby 1.8 to Ruby 1.9. Here is the same method, except changing the block parameter from 'm' to 'n', so that block 'n' and method 'n' have the same name.

```ruby
def fib(n)
  raise RangeError, "fib of negative" if n < 0
  (fib2 = proc { |n| n < 2 ? n : fib2[n - 1] + fib2[n - 2] })[n]
end
```


```ruby
# Ruby 1.9
(-2..12).map { |i| fib i rescue :error }
=> [:error, :error, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]

# Ruby 1.8
(-2..12).map { |i| fib i rescue :error }
=> [:error, :error, 0, 1, 0, -3, -8, -15, -24, -35, -48, -63, -80, -99, -120]
```

Ruby 1.9 still shows the correct answer, but Ruby 1.8 shows the wrong answer. With Ruby 1.9, 'n' is still a local variable of the block. With Ruby 1.8, 'n' of the block closes on 'n' of the fib() method. All calls to the block share the 'n' of one call to the method. So <tt>fib2[n - 1]</tt> changes the value of 'n', and <tt>fib2[n - 2]</tt> uses the wrong value of 'n', thus the wrong answer.

### Ruby with Hash


```ruby
def fib(n)
  raise RangeError, "fib of negative" if n < 0
  Hash.new { |fib2, m|
    fib2[m] = (m < 2 ? m : fib2[m - 1] + fib2[m - 2]) }[n]
end
```

This uses a Hash to memoize the recursion. After <tt>fib2[m - 1]</tt> returns, <tt>fib2[m - 2]</tt> uses the value in the Hash, without redoing the calculations.

* The fib() method has one local variable 'n'.
* The block has two local variables 'fib2' and 'm', and closes on 'n'.

### Ruby with recur/recurse

{{trans|PicoLisp}}
{{libheader|continuation}}

```ruby
require 'continuation' unless defined? Continuation

module Kernel
  module_function

  def recur(*args, &block)
    cont = catch(:recur) { return block[*args] }
    cont[block]
  end

  def recurse(*args)
    block = callcc { |cont| throw(:recur, cont) }
    block[*args]
  end
end

def fib(n)
  raise RangeError, "fib of negative" if n < 0
  recur(n) { |m| m < 2 ? m : (recurse m - 1) + (recurse m - 2) }
end
```


Our recursive block now lives in the 'block' variable of the Kernel#recur method.

To start, Kernel#recur calls the block once. From inside the block, Kernel#recurse calls the block again. To find the block, recurse() plays a trick. First, Kernel#callcc creates a Continuation. Second, throw(:recur, cont) unwinds the call stack until it finds a matching Kernel#catch(:recur), which returns our Continuation. Third, Kernel#recur uses our Continuation to continue the matching Kernel#callcc, which returns our recursive block.


### Ruby with arguments.callee

{{trans|JavaScript}}
{{libheader|continuation}}

```ruby
require 'continuation' unless defined? Continuation

module Kernel
  module_function

  def function(&block)
    f = (proc do |*args|
           (class << args; self; end).class_eval do
             define_method(:callee) { f }
           end
           ret = nil
           cont = catch(:function) { ret = block.call(*args); nil }
           cont[args] if cont
           ret
         end)
  end

  def arguments
    callcc { |cont| throw(:function, cont) }
  end
end

def fib(n)
  raise RangeError, "fib of negative" if n < 0
  function { |m|
    if m < 2
      m
    else
      arguments.callee[m - 1] + arguments.callee[m - 2]
    end
  }[n]
end
```

Our recursive block now lives in the 'block' variable of the Kernel#function method. Another block 'f' wraps our original block and sets up the 'arguments' array. Kernel#function returns this wrapper block. Kernel#arguments plays a trick to get the array of arguments from 'f'; this array has an extra singleton method #callee which returns 'f'.

## Rust


```rust
fn fib(n: i64) -> Option<i64> {
    // A function declared inside another function does not pollute the outer namespace.
    fn actual_fib(n: i64) -> i64 {
        if n < 2 {
            n
        } else {
            actual_fib(n - 1) + actual_fib(n - 2)
        }
    }

    if n < 0 {
        None
    } else {
        Some(actual_fib(n))
    }
}

fn main() {
    println!("Fib(-1) = {:?}", fib(-1));
    println!("Fib(0) = {:?}", fib(0));
    println!("Fib(1) = {:?}", fib(1));
    println!("Fib(2) = {:?}", fib(2));
    println!("Fib(3) = {:?}", fib(3));
    println!("Fib(4) = {:?}", fib(4));
    println!("Fib(5) = {:?}", fib(5));
    println!("Fib(10) = {:?}", fib(10));
}

#[test]
fn test_fib() {
    assert_eq!(fib(0).unwrap(), 0);
    assert_eq!(fib(1).unwrap(), 1);
    assert_eq!(fib(2).unwrap(), 1);
    assert_eq!(fib(3).unwrap(), 2);
    assert_eq!(fib(4).unwrap(), 3);
    assert_eq!(fib(5).unwrap(), 5);
    assert_eq!(fib(10).unwrap(), 55);
}

#[test]
fn test_invalid_argument() {
    assert_eq!(fib(-1), None);
}
```


## Scala

Using a Y-combinator:

```scala
def Y[A, B](f: (A ⇒ B) ⇒ (A ⇒ B)): A ⇒ B = f(Y(f))(_)

def fib(n: Int): Option[Int] =
  if (n < 0) None
  else Some(Y[Int, Int](f ⇒ i ⇒
    if (i < 2) 1
    else f(i - 1) + f(i - 2))(n))

-2 to 5 map (n ⇒ (n, fib(n))) foreach println
```

{{out}}

```txt

(-2,None)
(-1,None)
(0,Some(1))
(1,Some(1))
(2,Some(2))
(3,Some(3))
(4,Some(5))
(5,Some(8))

```



## Scheme

This uses named let to create a function (aux) that only exists inside of fibonacci:

```scheme
(define (fibonacci n)
  (if (> 0 n)
      "Error: argument must not be negative."
      (let aux ((a 1) (b 0) (count n))
        (if (= count 0)
            b
            (aux (+ a b) a (- count 1))))))

(map fibonacci '(1 2 3 4 5 6 7 8 9 10))
```

{{out}}

```txt
'(1 1 2 3 5 8 13 21 34 55)
```



## Seed7

Uses a local function to do the dirty work. The local function has a name, but it is not in the global namespace.

```seed7
$ include "seed7_05.s7i";

const func integer: fib (in integer: x) is func
  result
    var integer: fib is 0;
  local
    const func integer: fib1 (in integer: n) is func
      result
        var integer: fib1 is 0;
      begin
        if n < 2 then
          fib1 := n;
        else
          fib1 := fib1(n-2) + fib1(n-1);
        end if;
      end func;
  begin
    if x < 0 then
      raise RANGE_ERROR;
    else
      fib := fib1(x);
    end if;
  end func;

const proc: main is func
  local
    var integer: i is 0;
  begin
    for i range 0 to 4 do
      writeln(fib(i));
    end for;
  end func;
```


{{out}}

```txt

0
1
1
2
3

```



## Sidef

__FUNC__ refers to the current function.

```ruby
func fib(n) {
    return NaN if (n < 0)

    func (n) {
        n < 2 ? n
              : (__FUNC__(n-1) + __FUNC__(n-2))
    }(n)
}
```


__BLOCK__ refers to the current block.


```ruby
func fib(n) {
    return NaN if (n < 0)

    {|n|
        n < 2 ? n
              : (__BLOCK__(n-1) + __BLOCK__(n-2))
    }(n)
}
```



## Sparkling

As a function expression:

```Sparkling
function(n, f) {
    return f(n, f);
}(10, function(n, f) {
    return n < 2 ? 1 : f(n - 1, f) + f(n - 2, f);
})

```


When typed into the REPL:

```Sparkling>spn:1
 function(n, f) { return f(n, f); }(10, function(n, f) { return n < 2 ? 1 : f(n - 1, f) + f(n - 2, f); })
= 89
```



## Standard ML

ML does not have a built-in construct for anonymous recursion, but you can easily write your own fix-point combinator:

```sml
fun fix f x = f (fix f) x

fun fib n =
    if n < 0 then raise Fail "Negative"
    else
        fix (fn fib =>
                (fn 0 => 0
                | 1 => 1
                | n => fib (n-1) + fib (n-2))) n
```


Instead of using a fix-point, the more common approach is to locally define a recursive function and call it once:

```sml
fun fib n =
    let
        fun fib 0 = 0
          | fib 1 = 1
          | fib n = fib (n-1) + fib (n-2)
    in
        if n < 0 then
            raise Fail "Negative"
        else
            fib n
    end
```


In this example the local function has the same name as the outer function. This is fine: the local definition shadows
the outer definition, so the line "fib n" will refer to our helper function.

Another variation is possible. Instead, we could define the recursive "fib" at top-level, then shadow it with a non-recursive wrapper. To force the wrapper to be non-recursive, we use the "val" syntax instead of "fun":

```sml
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

val fib = fn n => if n < 0 then raise Fail "Negative"
                  else fib n
```



## Swift

;With internal named recursive closure:
{{works with|Swift|2.x}}

```swift
let fib: Int -> Int = {
  func f(n: Int) -> Int {
    assert(n >= 0, "fib: no negative numbers")
    return n < 2 ? 1 : f(n-1) + f(n-2)
  }
  return f
}()

print(fib(8))
```

{{works with|Swift|1.x}}

```swift
let fib: Int -> Int = {
  var f: (Int -> Int)!
  f = { n in
    assert(n >= 0, "fib: no negative numbers")
    return n < 2 ? 1 : f(n-1) + f(n-2)
  }
  return f
}()

println(fib(8))
```


;Using Y combinator:

```swift>struct RecursiveFunc<F
 {
  let o : RecursiveFunc<F> -> F
}

func y<A, B>(f: (A -> B) -> A -> B) -> A -> B {
  let r = RecursiveFunc<A -> B> { w in f { w.o(w)($0) } }
  return r.o(r)
}

func fib(n: Int) -> Int {
  assert(n >= 0, "fib: no negative numbers")
  return y {f in {n in n < 2 ? 1 : f(n-1) + f(n-2)}} (n)
}

println(fib(8))
```



## Tcl

This solution uses Tcl 8.5's lambda terms, extracting the current term from the call stack using introspection (storing it in a local variable only for convenience, with that ''not'' in any way being the name of the lambda term; just what it is stored in, and only as a convenience that keeps the code shorter). The lambda terms are applied with the <code>apply</code> command.

```tcl
proc fib n {
    # sanity checks
    if {[incr n 0] < 0} {error "argument may not be negative"}
    apply {x {
	if {$x < 2} {return $x}
	# Extract the lambda term from the stack introspector for brevity
	set f [lindex [info level 0] 1]
	expr {[apply $f [incr x -1]] + [apply $f [incr x -1]]}
    }} $n
}
```

Demonstrating:

```tcl
puts [fib 12]
```

{{out}}}

```txt
144
```

The code above can be written without even using a local variable to hold the lambda term, though this is generally less idiomatic because the code ends up longer and clumsier:

```tcl
proc fib n {
    if {[incr n 0] < 0} {error "argument may not be negative"}
    apply {x {expr {
        $x < 2
          ? $x
          : [apply [lindex [info level 0] 1] [incr x -1]]
            + [apply [lindex [info level 0] 1] [incr x -1]]
    }}} $n
}
```

However, we can create a <code>recurse</code> function that makes this much more straight-forward:

```tcl
# Pick the lambda term out of the introspected caller's stack frame
proc tcl::mathfunc::recurse args {apply [lindex [info level -1] 1] {*}$args}
proc fib n {
    if {[incr n 0] < 0} {error "argument may not be negative"}
    apply {x {expr {
        $x < 2 ? $x : recurse([incr x -1]) + recurse([incr x -1])
    }}} $n
}
```



## TXR


For the Y combinator approach in TXR, see the Y combinator task.

The following easy transliteration of one of the Common Lisp solutions shows the conceptual and cultural compatibility between TXR Lisp macros and CL macros:

{{trans|Common_Lisp}}


```txrlisp
(defmacro recursive ((. parm-init-pairs) . body)
  (let ((hidden-name (gensym "RECURSIVE-")))
    ^(macrolet ((recurse (. args) ^(,',hidden-name ,*args)))
       (labels ((,hidden-name (,*[mapcar first parm-init-pairs]) ,*body))
         (,hidden-name ,*[mapcar second parm-init-pairs])))))

(defun fib (number)
  (if (< number 0)
    (error "Error. The number entered: ~a is negative" number)
    (recursive ((n number) (a 0) (b 1))
      (if (= n 0)
        a
        (recurse (- n 1) b (+ a b))))))

(put-line `fib(10) = @(fib 10)`)
(put-line `fib(-1) = @(fib -1)`))
```


{{out}}


```txt
$ txr anonymous-recursion.txr
fib(10) = 55
txr: unhandled exception of type error:
txr: possibly triggered by anonymous-recursion.txr:9
txr: Error. The number entered: -1 is negative
Aborted (core dumped)
```



## UNIX Shell

The shell does not have anonymous functions. Every function must have a name. However, one can create a subshell such that some function, which has a name in the subshell, is effectively anonymous to the parent shell.

```bash
fib() {
  if test 0 -gt "$1"; then
    echo "fib: fib of negative" 1>&2
    return 1
  else
    (
      fib2() {
        if test 2 -gt "$1"; then
          echo "$1"
        else
          echo $(( $(fib2 $(($1 - 1)) ) + $(fib2 $(($1 - 2)) ) ))
        fi
      }
      fib2 "$1"
    )
  fi
}
```


```bash
$ for i in -2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12; do
>   fib $i
> done
fib: fib of negative
fib: fib of negative
0
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
89
144
```



## Ursala


```Ursala
#import nat

fib =

~&izZB?(                    # test the sign bit of the argument
   <'fib of negative'>!%,   # throw an exception if it's negative
   {0,1}^?<a(               # test the argument to a recursively defined function
      ~&a,                  # if the argument was a member of {0,1}, return it
      sum^|W(               # otherwise return the sum of two recursive calls
         ~&,                # to the function thus defined
         predecessor^~(     # with the respective predecessors of
            ~&,             # the given argument
            predecessor)))) # and the predecessor thereof
```

Anonymous recursion is often achieved using the recursive conditional operator, <code>( _ )^?( _ , _ )</code>, which takes a predicate on the left and a pair of functions on the right, typically one for the base and one for the inductive case in a recursive definition. The form <code>^?<</code> can be used if the relevant predicate is given by membership of the argument in a constant set, in which case only the set needs to be specified rather than the whole predicate.

The recursive conditional operator <code>^?</code> differs from the ordinary conditional <code>?</code> seen at the outermost level by arranging for its predicate and component functions to be given an input of the form <math>(f,a)</math> where <math>a</math> is the original argument, and <math>f</math> is a copy of the whole function. Code within the function body may then access itself anonymously according to all the usual language idioms pertaining to deconstruction of tuples, and call itself by any of several recursion combinators, such as the pairwise recursion form <code>W</code> seen above.


## UTFool


'''Solution with anonymous class'''


```UTFool

···
http://rosettacode.org/wiki/Anonymous_recursion
···
⟦import java.util.function.UnaryOperator;⟧

■ AnonymousRecursion
  § static
    ▶ main
    • args⦂ String[]
      if 0 > Integer.valueOf args[0]
         System.out.println "negative argument"
      else
         System.out.println *UnaryOperator⟨Integer⟩° ■
           ▶ apply⦂ Integer
           • n⦂ Integer
             ⏎ n ≤ 1 ? n ! (apply n - 1) + (apply n - 2)
         °.apply Integer.valueOf args[0]

```



## VBA


```vb

Sub Main()
Debug.Print F(-10)
Debug.Print F(10)
End Sub

Private Function F(N As Long) As Variant
    If N < 0 Then
        F = "Error. Negative argument"
    ElseIf N <= 1 Then
        F = N
    Else
        F = F(N - 1) + F(N - 2)
    End If
End Function
```

{{out}}

```txt
Error. Negative argument
 55
```



## Wart


```wart
def (fib n)
  if (n >= 0)
    (transform n :thru (afn (n)
                         (if (n < 2)
                           n
                           (+ (self n-1)
                              (self n-2)))))
```


<code>afn</code> creates an anonymous function that can be recursed by calling <code>self</code>.


## WDTE


```WDTE>let str =
 'strings';

let fib n => switch n {
  < 0 => str.format 'Bad argument: {q}' n;
  default => n -> (@ memo s n => switch n {
    == 0 => 0; == 1 => 1;
    default => + (s (- n 1)) (s (- n 2));
  });
};
```


In WDTE, a lambda, defined in a block delineated by <code>(@)</code>, gets passed itself as its first argument, allowing for recursion.


## Wren


```wren
class Fibonacci {
    static compute(n) {
        var fib
        fib = Fn.new {|n|
            if (n < 2) return n
            return fib.call(n - 1) + fib.call(n - 2)
        }

        if (n < 0) return null
        return fib.call(n)
    }
}

```



## XPL0

In XPL0 you can nest functions/procedures inside other
functions/procedures up to eight levels deep.
This makes those nested functions invisible to the outside, thus preventing namespace pollution.


```XPL0
include c:\cxpl\codes;

func Fib(X);
int X;
        func ActualFib(N);
        int N;
        [if N<2 then return N
        else return ActualFib(N-1) + ActualFib(N-2);
        ]; \ActualFib;

[if X<0 then [Text(0, "Error "); return 0]
else return ActualFib(X);
]; \Fib;

[IntOut(0, Fib(8));  CrLf(0);
 IntOut(0, Fib(-2)); CrLf(0);
]
```


{{out}}

```txt

21
Error 0

```



## Yabasic

{{trans|AutoIt}}

```Yabasic
print Fibonacci(-10)
print Fibonacci(10)


sub Fibonacci(number)

    If number < 0 print "Invalid argument: "; : return number

    If number < 2 Then
        Return number
    Else
        Return Fibonacci(number - 1) + Fibonacci(number - 2)
    EndIf

end sub
```



## zkl


```zkl
fcn fib(n){
   if (n<0) throw(Exception.ValueError);
   fcn(n){
      if (n < 2) return(1);
      else       return(self.fcn(n-1) + self.fcn(n-2));
   }(n);
}
fib(8) .println();
fib(-8).println();

```

{{out}}

```txt

34
ValueError thrown

```



## ZX Spectrum Basic

{{trans|AutoHotkey}}

```zxbasic
10 INPUT "Enter a number: ";n
20 LET t=0
30 GO SUB 60
40 PRINT t
50 STOP
60 LET nold1=1: LET nold2=0
70 IF n<0 THEN PRINT "Positive argument required!": RETURN
80 IF n=0 THEN LET t=nold2: RETURN
90 IF n=1 THEN LET t=nold1: RETURN
100 LET t=nold2+nold1
110 IF n>2 THEN LET n=n-1: LET nold2=nold1: LET nold1=t: GO SUB 100
120 RETURN

```


{{omit from|ACL2}}
{{omit from|Euphoria}}
{{omit from|PureBasic}}
