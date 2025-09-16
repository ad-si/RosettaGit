+++
title = "Inverted syntax"
description = ""
date = 2019-09-09T20:22:08Z
aliases = []
[extra]
id = 9828
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "bracmat",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "d",
  "echolisp",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "m4",
  "mathematica",
  "mercury",
  "metafont",
  "nim",
  "oforth",
  "oxygenbasic",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "prolog",
  "python",
  "qi",
  "r",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "swift",
  "tcl",
  "wortel",
  "zkl",
]
+++

'''Inverted syntax with conditional expressions'''

In traditional syntax conditional expressions are usually shown before the action within a statement or code block:


```pseudocode
 IF raining=true THEN needumbrella=true
```


In inverted syntax, the action is listed before the conditional expression in the statement or code block:


```pseudocode
 needumbrella=true IF raining=true
```


'''Inverted syntax with assignment'''

In traditional syntax, assignments are usually expressed with the variable appearing before the expression:


```pseudocode
 a = 6
```


In inverted syntax, the expression appears before the variable:

```pseudocode
 6 = a
```


'''Task'''

The task is to demonstrate support for inverted syntax forms within the language by showing both the traditional and inverted forms.


## Ada


The only place where syntax is kind of inverted is <code>exit when</code> to exit loops:


```Ada
Foo := 1;
loop
   exit when Foo = 10;
   Foo := Foo + 1;
end loop;
```



## ALGOL 68

```algol68
# Inverted assignment                                                                       #
# Assignment in Algol 68 is via ":=" which is automaically provided for all modes (types)   #
# However we could define e.g. "=:" as an inverted assignment operator but we would need to #
# define a separate operator for each mode, e.g. for integers and strings:                  #
PRIO =: = 1;
OP   =: = ( INT a,    REF INT    b )REF INT:    b := a;
OP   =: = ( STRING a, REF STRING b )REF STRING: b := a;
OP   =: = ( CHAR   a, REF STRING b )REF STRING: b := a;
INT a, b; STRING s;
    1 =: a;
a + 1 =: b;
  "?" =: s;
print( ( a, b, s, newline ) );

# There is one standard inverted assignment operator: +=: or PLUSTO which prepends a string #
# to another:                                                                               #
"bc"  =: s;
"b"  +=: s;
print( ( s, newline ) );

# Inverted Conditional Expressions                                                          #
# We could define an operator called WHEN perhaps, that would execute its left operand if   #
# the right operand was TRUE. However the left operand would need to be a PROC VOID so the  #
# syntax would not be as convientent as the standard IF-THEN-FI construct. E.g.:            #
PRIO WHEN = 1;
OP   WHEN = ( PROC VOID code, BOOL test )VOID: IF test THEN code FI;

( VOID: print( ( "NO",  newline ) ) ) WHEN a = b;  # the anonymous PROC VOID is not called  #
( VOID: print( ( "yes", newline ) ) ) WHEN a /= b  # the anonymous PROC VOID is called      #
```

```txt

         +1         +2?
bbc
yes

```



## Bracmat

The match operator <code>:</code> can play the role of an inverted assignment operator <code>=</code>. The following two definitions of a function <code>double</code> are equivalent.

```bracmat

double=.!arg+!arg;

(=.!arg+!arg):(=?double); { inverted assignment syntax, same result as above. }

```

Bracmat evaluates all left hand sides of all binary operators. How right hand sides are treated depends on the operator. The <code>=</code> operator does not evaluate its right hand side at all. The right hand side of the <code>:</code> operator is evaluated by a dedicated match evaluator, which for example sees the expression <code>?double</code> as a variable to be bound to some part of the left hand side of the <code>:</code> operator.

The following two assignments are not equivalent:

```bracmat
foo=3+7  { assigns 3+7 to foo }
3+7:?foo { assigns 10 to foo }

```



## C

C doesn't have a way to do this cleanly.  You need to drop some code before the statements you want to invert.
I'll use CPP to make it look as close as I think you can make it. -- ksb, Jan 2013

The original would be "if (foo()) a = 4;"   Here is the inverted if logic using "otherwise ... given () ;"


```c

#include <stdio.h>
#include <stdlib.h>
#define otherwise       do { register int _o = 2; do { switch (_o) {  case 1:
#define given(Mc)       ;case 0: break; case 2: _o = !!(Mc); continue; } break; } while (1); } while (0)


int foo() { return 1; }

main()
{
        int a = 0;

        otherwise  a = 4 given (foo());
        printf("%d\n", a);
        exit(0);
}

```


Which actually makes the main program look like:


```C

main()
{
        int a = 0;

        do {
                register int _o = 2;
                do {
                        switch (_o) {
                        case 1:
                                a = 4;
                        case 0:
                                break;
                        case 2:
                                _o = !!(foo());
                                continue;
                        } break;
                } while (1);
        } while (0);
        printf("%d\n", a);
        exit(0);
}

```


To make lint happy you need a /*FALLTHROUGH*/ before the case 0 (in the macro).


## C++

Though rarely, if ever, used in practice, user-defined class types can have inverted syntax with assignment.

```cpp
class invertedAssign {
  int data;
public:
  invertedAssign(int data):data(data){}
  int getData(){return data;}
  void operator=(invertedAssign& other) const {
    other.data = this->data;
  }
};


#include <iostream>

int main(){
  invertedAssign a = 0;
  invertedAssign b = 42;
  std::cout << a.getData() << ' ' << b.getData() << '\n';

  b = a;

  std::cout << a.getData() << ' ' << b.getData() << '\n';
}
```


It doesn't work if the left operand is not of the type <tt>invertedAssign</tt>.


## Clojure


The "thread last" macro permits inversion of syntax in virtually all contexts. Any form for which the construction of a new list from consecutive elements doesn't change the semantics may be turned "inside-out"; this is to the exclusion of those containing function definitions and little else.


```Clojure
; normal
(if (= 1 1)
  (print "Math works."))

; inverted
(->> (print "Math still works.")
     (if (= 1 1)))

; a la Haskell
(->> (print a " is " b)
     (let [a 'homoiconicity
           b 'awesome]))
```


Expanding the macro reveals the nature of the aforementioned limitation.

```Clojure
((fn [x] (* x x) 5) ; Define a lambda and call it with 5.

(macroexpand-1 '(->> 5 (fn [x] (* x x))))
(fn [x] (* x x) 5)  ; Define a lambda that returns 5 regardless.
```


The "thread first" macro (colloquially, the Thrush) provides a similar facility, wherein each expression becomes the first argument to the next.

```Clojure
(= (mod (inc 42) 7)
   (-> 42 inc (mod 7)))
```



## CoffeeScript

CoffeeScript allows loop constructs and conditionals to be written in a suffix manner. Loop constructs evaluate to an array containing the result of each iteration; conditionals evaluates either the true-case expression or the false-case one.


```coffeescript
alert "hello" if true
alert "hello again" unless false # the same as the above; unless is a negated if.

idx = 0
arr = (++idx while idx < 10) # arr is [1,2,3,4,5,6,7,8,9,10]

idx = 0
arr = (++idx until idx is 10) # same as above; until is an inverted while.
```



## Common Lisp


Lisp has a PROGN macro for evaluating a bunch of forms, such that the value(s) of the last one is yielded as the result of the PROGN. We can create a reversed analog of PROGN, inside which, we must write the syntax backwards: arguments first, then the operator. Furthermore, this rule is recursively applied to expressions.

''Note: unfortunately this is completely naive. To do this 100% right, we need a code walker, which is more complicated. However, code walkers are used in practice for hairy language transformation jobs, just not commonly so in daily Lisp programming. A code walker would let us imlement a more smarter version which would apply the transformation to forms which are evaluated, and not things like literal data. As it is, our macro also transforms literal data, making it impossible, for instance, to use the quote shorthand 'FORM.  This stands for (QUOTE FORM) and of course, the macro will treat it as reversed syntax, transforming it to (FORM QUOTE).  We could hack in a special case which recognized QUOTE, but then we do not know whether or not (QUOTE FORM) is in a context where it is being evaluated. That is why we need the code walker. It's best to use someone's well-tested, portable code-walker, since they are not trivial to write, and they have some implementation-specific parts (such as recognition of compiler-specific forms that come out of system macros or perhaps out of user-written nonportable code).''



```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unrev-syntax (form)
    (cond
      ((atom form) form)
      ((null (cddr form)) form)
      (t (destructuring-bind (oper &rest args) (reverse form)
           `(,oper ,@(mapcar #'unrev-syntax args)))))))

(defmacro rprogn (&body forms)
  `(progn ,@(mapcar #'unrev-syntax forms)))
```


Interactive test run:


```txt
$ clisp -q -i reverse.lisp
;; Loading file reverse.lisp ...
;; Loaded file reverse.lisp
[1]> (rprogn ((1 2 +) (3 4 +) *))
21
[2]> (rprogn (("not greater" print) ("greater" print) (1 2 >) if))

"greater"
"greater"
[3]> (macroexpand '(rprogn (("not greater" print) ("greater" print) (1 2 >) if)))
(PROGN (IF (> 2 1) (PRINT "greater") (PRINT "not greater"))) ;
T

```


Now here is a slightly more complicated version which more closely conforms to the idea given in this task. The operator is assumed to be the second-last element of a form. The last element of the form is the first argument, and the forms prior to the operator are the remaining arguments (in reverse order).   These rules are recursively applied to each of the arguments, but not to the operator.  A two element form is assumed to be (argument operator), and is rewritten to
(operator argument*) where argument* is the result of applying the unreversal to the argument:


```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unrev-syntax (form)
    (cond
      ((atom form) form) ;; atom: leave alone
      ((null (cdr form)) form) ;; one-element form: leave alone
      ((null (cddr form)) ;; two-element form: swap
       (destructuring-bind (arg oper) form
         `(,oper ,(unrev-syntax arg))))
      (t    ;; two or more args: swap last two, add others in reverse
       (destructuring-bind (arg1 oper &rest args) (reverse form)
        `(,oper ,(unrev-syntax arg1) ,@(mapcar #'unrev-syntax args)))))))

(defmacro rprogn (&body forms)
  `(progn ,@(mapcar #'unrev-syntax forms)))
```



```txt
[1]> (rprogn ((1 + 2) * (3 + 4)))
21
[2]> (rprogn (("not equal" print) ("equal" print) if (1 = 2)))

"not equal"
"not equal"
[3]> (rprogn (("not equal" print) ("equal" print) if (1 = 1)))

"equal"
"equal"
[4]> (macroexpand '(rprogn (("not equal" print) ("equal" print) if (1 = 1))))
(PROGN (IF (= 1 1) (PRINT "equal") (PRINT "not equal"))) ;
T
[5]> (macroexpand '(rprogn ((1 + 2) * (3 + 4))))
(PROGN (* (+ 4 3) (+ 2 1))) ;
T
```



## D


D enables a function to be called as if it were a method of its first argument.  This feature often leads to natural syntax and readable, left-to-right expressions:


```d
#!/usr/bin/rdmd

import std.algorithm;

void main() {
    assert("Hello, World".length == 12);
    assert("Cleanliness".startsWith("Clean"));

    auto r = [1, 4, 2, 8, 5, 7]
        .filter!(n => n > 2)
        .map!(n => n * 2);

    assert(r.equal([8, 16, 10, 14]));
}
```



## EchoLisp


```scheme

;; use reader macros to transform (a OP b) into (OP b a)

(lib 'match)
(define-macro invert-= (a <- b) (set! b a))
(define-macro invert-IF (a 'IF b) (when b a))

(define raining #f)

(#t <- raining)
raining
    → #t
('umbrella-need IF raining)
    → umbrella-need

(#f <- raining)
('umbrella-need IF raining)
    → #f

;; debug mode
(debug 3)
('umbrella-need IF raining)
💡 [0]     invert-IF → ('umbrella-need IF raining)
compiled :: (#when raining 'umbrella-need)


```



## Factor

Since code is data in Factor, we can simply reverse it, transforming postfix into "prefix."

```Factor
1 1 + ! 2
[ + 1 1 ] reverse call ! 2
{ 1 2 3 4 5 } [ sq ] map ! { 1 4 9 16 25 }
[ map [ sq ] { 1 2 3 4 5 } ] reverse call ! { 1 4 9 16 25 }
```


In fact, using a Lisp-style macro, we can perform this transformation at parse time:


```factor
MACRO: pre ( quot -- quot ) reverse ;

[ + 2 2 ] pre ! 4
```


Of course, this isn't true prefix because <code>+</code> retains its arity of <tt>2</tt>:


```factor
[ + 3 + 2 2 ] pre ! 7
```


We can define a more accurate prefix macro for addition and subtraction in terms of <code>reduce</code>:


```factor
MACRO: pre ( quot -- quot ) 1 cut swap [ 0 ] dip reduce 1quotation ;

[ + 1 2 3 4 5 ] pre ! 15
```


Additionally, using parsing words, we can add any syntax we like. The <code>infix</code> vocabulary is an example of this:


```factor
USE: infix
[infix
    5*(1+1) ! 10
infix]
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Inverted_syntax this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

Leaving aside those who think that Fortran is inherently backward, assignment in Fortran is firmly right-to-left in the form <code>''variable'' = ''expression''</code> where the ''expression'' is computed in accordance with the precedence rules and the resulting value is assigned to the ''variable''.

However, assignment-style constructions appear inside the INQUIRE statement as in
```Fortran
      INQUIRE(FILE = FILENAME(1:L), EXIST = MAYBE, ERR = 666, IOSTAT = RESULT)
```

Here, a logical variable MAYBE is to receive the value of whether or not the disc file named in FILENAME(1:L) exists, and integer variable RESULT contains an error code, zero if all went well - the file name may not be correctly formed, for example. Thus, FILE is receiving a value right-to-left on entry to the statement, while MAYBE and RESULT receive values left-to-right on exit from the statement. Despite appearances, <code>ERR = 666</code> is not an assignment statement; 666 is not an integer, it is a statement label to which execution will jump should an error arise. Similar arrangements apply for the file OPEN and CLOSE statements.

There is no deviationism with the workings of IF-statements.


## FreeBASIC


FreeBASIC has nothing like this built into the language.
The nearest we can get is to define macros which reverse the order of the arguments:

```freebasic
' FB 1.05.0 Win64

#Define ThenIf(a, b) If b Then a
#Define InvertAssign(a, b) b = a

Dim As Boolean needUmbrella = False, raining = True
ThenIf(needUmbrella = True, raining = True)
Print "needUmbrella = "; needUmbrella

Dim As Integer b = 0, a = 3
InvertAssign(a, b)
Print "b is"; b
Sleep
```


```txt

needUmbrella = true
b is 3

```



## Go

The closest Go can get to inverted syntax for conditionals is to define a new type ('ibool' say) based on the built-in 'bool' type and then define a method ('iif' say) on the new type which takes the place of the traditional 'if'.

Simulating inverted syntax with assignment is not possible.

```go
package main

import "fmt"

type ibool bool

const itrue ibool = true

func (ib ibool) iif(cond bool) bool {
    if cond {
        return bool(ib)
    }
    return bool(!ib)
}

func main() {
    var needUmbrella bool
    raining := true

    // normal syntax
    if raining {
        needUmbrella = true
    }
    fmt.Printf("Is it raining? %t. Do I need an umbrella? %t\n", raining, needUmbrella)

    // inverted syntax
    raining = false
    needUmbrella = itrue.iif(raining)
    fmt.Printf("Is it raining? %t. Do I need an umbrella? %t\n", raining, needUmbrella)
}
```


```txt

Is it raining? true. Do I need an umbrella? true
Is it raining? false. Do I need an umbrella? false

```



## Haskell

Because Haskell is an expression-based pure functional language, this cannot be defined in the general case, because not every expression has a type with a null-like default value that can be inferred.

However, for the common case where you want to perform a certain action only when some condition holds, you can define a simple binary operator:


```haskell
when :: Monad m =
 m () -> Bool -> m ()
action `when` condition = if condition then action else return ()

```


Example usage:


```txt

Prelude> putStrLn "It's true." `when` False
Prelude> putStrLn "It's true." `when` True
It's true.
Prelude>

```


----

Haskell has a feature that can be considered "inverted" syntax (although it is not one of the types listed in the description of the task): The definition of local variables to be used in a function can be placed in a <code>where</code> clause that comes ''after'' the function body:


```haskell
func a b x = (x + y) / y
  where y = a * b
```


This is in contrast to <code>let</code> expressions, where the definition of local variables comes before the scope that uses them:


```haskell
func a b x =
  let y = a * b in
    (x + y) / y
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon can use [[Icon%2BUnicon/Intro#Conjunction.2C_yielding_a_different_result|expression conjunctions that select different sub-expression results]] to create this effect.

```Icon
procedure main()
raining := TRUE := 1                         # there is no true/false null/non-null will do
if \raining then needumbrella := TRUE        # normal
needumbrella := 1(TRUE, \raining)            # inverted (choose sub-expression 1)
end
```



## J


J tries to minimize syntax and control structures.  However, they are present.

For control structures, "traditional conditional structures" are traditional (control structures implemented using keywords such as <code>if.</code> are not inverted).  But J offers some alternatives which are inverted (operations such as <code>^:</code>) or both (operations such as <code>*</code> (multiplication) can be used as inverted conditional structures or as traditional conditional structures, depending on whether you place the boolean test on the left or the right side).  See [[Conditional_Structures/J]] for some examples.

Note also that simple, regular iteration without control structures is fundamental to the language (and turns data selection operations, including indexing, into conditional structures).  But no special syntax is present for these cases so it can be neither traditional nor inverted.


## Java

The closest Java comes to placing an action before a condition is with
```txt
do ... while(condition);
```



## jq

jq's syntax for associating a value with a variable is "inverted": the expression for associating a value, v, with a variable, $x, is:

```jq
v as $x
```

Note, however, that there is limited support for the conventional "target = value" syntax in the context of JSON objects and arrays, but the semantics is purely functional.

For example, if o is {"a": 1}, then the expression:

```jq
o["a"] = 2
# or equivalently: o.a = 2
```
 emits another object equal to {"a": 2}.


## Julia

Can be easily implemented as a macro:


```julia
macro inv(expr, cond)
    cond isa Expr && cond.head == :if || throw(ArgumentError("$cond is not an if expression"))
    cond.args[2] = expr
    return cond
end

@inv println("Wow! Lucky Guess!") if true else println("Not!") end
```



## Kotlin

Kotlin can get close to an inverted 'if' syntax by defining an infix function - 'iif' say. However, there doesn't appear to be a way to mimic inverted assignment.

```scala
// version 1.0.6

infix fun Boolean.iif(cond: Boolean) = if (cond) this else !this

fun main(args: Array<String>) {
    val raining = true
    val needUmbrella = true iif (raining)
    println("Do I need an umbrella?  ${if(needUmbrella) "Yes" else "No"}")
}
```


```txt

Do I need an umbrella?  Yes

```



## M2000 Interpreter

There is no way to have inverted syntax for conditionals, except for using lambda functions. Three more things we can do. One to invert the way we call a module, passing statements before calling the module. The Second by using Let we make a Push and then a Read, so first evaluated the right expression and then the left. Here we change a global variable in the right expression and we change the index of array where we set the returned value from function.
The third one is more complicated. We make a callback function which act as code of a module, with same scope, and use a module to check condition before we call the callback function


```M2000 Interpreter

expr=lambda ->{
      Print "ok"
}
ifrev=lambda (dothis, cond) ->{
      if cond then call dothis()
}
a=1
call ifrev(expr, a=1)

\\ on module call
Module Subtract (a, b) {
      Push a-b
}
Module PrintTop {
      Print Number
}
Subtract 10, 3 : PrintTop
\\ pushing before calling in reverse order
Push 3, 10 : Subtract : PrintTop
\\ Before call PrintTop any parameter send to stack
\\ So this works ok
PrintTop 1000
\\ on assignment
Dim A(5)=1
Global n=2
Function AddOne (x) {
      n++
      =x
}
\\ Execution of left expression, then right expression
A(n)=AddOne(5)
Print A(n-1)=5, n=3
\\ Execution of right expression, then left expression
Let A(n)=AddOne(15)
Print A(n)=15, n=4
\\ This statement..
Let X=1, Y=2
\\ executed like these
Push 2, 1 : Read X, Y

\\ This is the CallBack way
Module ExecCond {
      Read &callback(), cond
      if cond then call callback()
}
x=1
\\ this aa() is a function but when we call it after transforming from Lazy$()
\\ act as part of module so we see x, and alter it
Function aa {
      x++
}
a=1
ExecCond Lazy$(&aa()), A=1
Print x=2

```



## m4

We [[extend your language|extend our language]] with a new macro, <code>thenif</code>,
to invert the arguments to the builtin macro, <code>ifelse</code>.


```m4
define(`thenif', `ifelse($2, $3, `$1')')dnl
dnl
ifelse(eval(23 > 5), 1, 23 is greater than 5)
ifelse(eval(23 > 5), 0, math is broken)
thenif(23 is greater than 5, eval(23 > 5), 1)
thenif(math is broken, eval(23 > 5), 0)
```


This example outputs these four lines.
Math was not broken, so two lines are empty.


```txt
23 is greater than 5

23 is greater than 5

```



## Mathematica

Traditional form:

```Mathematica
a = 4
->4

b = 5
->5

If[1<2,
Print["This was expected"]
]
->This was expected
```


Inversion of syntax:


```txt
Unprotect["="]; SetAttributes[Set2, HoldAll]; Set2[a_, b_] := Set[b, a]
<< Notation`; Notation[ParsedBoxWrapper[RowBox[{"x_", "=","y_"}]]
\[DoubleLongLeftRightArrow]
ParsedBoxWrapper[RowBox[{" ", RowBox[{"Set2", "[", RowBox[{"x_", ",", "y_"}], "]"}]}]]]

2 = c
->2

3 = d
->3

Unprotect["If"];SetAttributes[If2,HoldAll];If2[a_,b_]:=If[b,a]
Notation[If[x_,y_] \[DoubleLongLeftRightArrow]  If2[x_,y_]]

If[Print["This was expected"],
1<2
]
->This was expected
```



## Metafont

Although there is an assignment operator, you can also define values of variables by equations, such as:

```metafont
x=6;
7=y;
```

Therefore it can be done both ways.


## Mercury


For Mercury, order rarely matters.
Just as programmers in most languages take care to order their function definitions or their method definitions for clarity and emphasis, while knowing that it's all the same to the compiler, Mercury programmers can do this also in the bodies of functions.

These two clauses are exactly the same:


```Mercury
:- pred progress(int::in, int::in, int::out, int::out) is det.
progress(Past, Future, At, Total) :-
	At = Past + 1,
	Total = Past + Future.

progress(Past, Future, At, Total) :-
        Past + Future = Total,
        Past + 1 = At.
```


Order doesn't matter even when a data dependency tells you (and Mercury's compiler) what the order of evaluation must be:


```Mercury
:- func example(int) = string.
example(N) = S :-
        from_int(N) = S0,
        pad_left(S0, '0', 3, S).

example(N) = S :-
        pad_left(S0, '0', 3, S),
        from_int(N) = S0.
```


Data dependencies are most obvious when state is threaded through a clause:


```Mercury
main(IO0, IO) :-
        io.write_string("Hello, ", IO0, IO1),
        io.write_string("world!\n", IO1, IO).

main(!IO) :-
        io.write_string("Hello, ", !IO),
        io.write_string("world!\n", !IO).
```


The io.write_string/2's in the first example could be written in either order and the result would be the same, as the "world!\n" can't be written until the "Hello, " provides the IO1.
Order matters in the second example, however, as it uses [http://www.mercury.csse.unimelb.edu.au/information/doc-release/mercury_ref/State-variables.html#State-variables state variables].
The order is still enforced by a data dependency, so


```Mercury
main(!IO) :-
        io.write_string(X, !IO), io.nl(!IO),
        some_long_uninteresting_thing(X).

% this is the same:
main(!IO) :-
        some_long_uninteresting_thing(X),
        io.write_string(X, !IO), io.nl(!IO).

% but this is different!
main(!IO) :-
        io.nl(!IO), io.write_string(X, !IO),
        some_long_uninteresting_thing(X).
```



## Nim


Inverted syntax in Nim can be achieved through the use of templates as operators:


```nim
#--
# if statements
#--

template `?`(expression, condition) =
  if condition:
    expression

let raining = true
var needUmbrella: bool

# Normal syntax
if raining: needUmbrella = true

# Inverted syntax
(needUmbrella = true) ? (raining == true)

#--
# Assignments
#--

template `~=`(right, left) =
  left = right

var a = 3

# Normal syntax
a = 6

# Inverted syntax
6 ~= a
```



## Oforth


Oforth uses RPN so some forms a partially (or totally) inverted  :
   6 -> a : Push 6 on the stack and set the top of the stack as value of local variable a (top of the stack is consumed)
   6 := a : Push 6 on the stack and set the top of the satck as value of  attribute a (top of the stack is consumed).
   raining ifTrue: [ true ->needumbrella ]


## OxygenBasic

Macros may have localised scope, so they can be safely deployed as HumptyDumpty words.

```oxygenbasic

macro cond(a,c)  {c then a}

macro store(b,a) {a=b}

sys a,c=10

if c>4 then a=4

'INVERTED SYNTAX FORMS:

cond a=40, if c>4

store 4,a

'COMBINED:

cond store(5,a), if c>4

```



## PARI/GP

GP does not include a syntax-inverted if, but that can be defined using closures.

```parigp
fi(f, condition)=if(condition,f());

if(raining, print("Umbrella needed"))
fi(->print("Umbrella needed"), raining)
```


PARI can also be used to implement it more directly in GP.


## Perl


Perl already has that:

```perl
if ($guess == 6) { print "Wow! Lucky Guess!"; };    # Traditional syntax
print 'Wow! Lucky Guess!' if $guess == 6;           # Inverted syntax (note missing braces and parens)
unless ($guess == 6) { print "Sorry, your guess was wrong!"; }   # Traditional syntax
print 'Huh! You Guessed Wrong!' unless $guess == 6;              # Inverted syntax
```


Inverted syntax can also be used with the ternary operator.
However this may produce different results to the traditional syntax form because when inverted syntax is used, we are effectively making an assignment to a ternary expression. so in the following example code, instead of the assignment being made to variable a (as it is in the traditional syntax form), the inverted syntax form will cause assignment to be made to either b or c, depending on value of the ok variable:


```perl
# Note that the results obtained by the inverted syntax form
# may produce differing results from the traditional syntax form
$a = $ok ? $b : $c;     # Traditional syntax
($ok ? $b : $c) = $a;   # Inverted syntax
```


We can also emulate inverted syntax for scalar assignment by creating a function as follows:


```perl
sub assign { $_[1] = $_[0] }
$a = $b;       # Traditional syntax
assign $b, $a; # Inverted syntax
```


Inverted list assignment is not possible because it prevents arrays from flattening.


## Perl 6

Like all Perls, Perl 6 has statement modifiers:

```perl6
if $guess == 6 { say "Wow! Lucky Guess!" }          # Traditional
say 'Wow! Lucky Guess!' if $guess == 6;             # Inverted
unless $guess == 6 { say "Huh! You Guessed Rong!" } # Traditional
say 'Huh! You Guessed Rong!' unless $guess == 6;    # Inverted
```


Perl also inverts the syntax of loops:

```perl6
while $i { --$i }
--$i while $i;

until $x > 10 { $x++ }
$x++ until $x > 10;

for 1..10 { .say if $_ %% 2 }
.say if $_ %% 2 for 1..10;  # list comprehension
```


Perl 6 has a system of metaoperators that modify the characteristics of normal operators.  Among these is the <tt>R</tt> metaoperator, which is able to reverse the arguments of most infix operators (including user-defined ones).
So a reversed assignment is easy to write:

```perl6
42 R= $_; say $_;   # prints 42
```


Since, much like list operators, assignment loosens the precedence of the following expression to allow comma lists, reverse assignment of a list requires parens where the normal assignment would not:

```perl6
my @a = 1,2,3;
(1,2,3) R= my @a;
```

However, generally in that case you'd use a feed operator anyway, which is like an object pipe, but unlike Unix pipes works in either direction:

```perl6
my @a <== 1,2,3;
1,2,3 ==> my @a;
```

We think this is much more readable than a reversed assignment.

One other interesting inversion is the ability to put the conditional of a repeat loop at either end, with identical test-after semantics:

```perl6
repeat {
    $_ = prompt "Gimme a number: ";
} until /^\d+$/;

repeat until /^\d+$/ {
    $_ = prompt "Gimme a number: ";
}
```

This might seem relatively useless, but it allows a variable to be declared in the conditional that isn't actually set until the loop body:

```perl6
repeat until my $answer ~~ 42 {
    $answer = prompt "Gimme an answer: ";
}
```

This would require a prior declaration (and two extra semicolons, horrors)
if written in the non-inverted form with the conditional at the bottom:

```perl6
my $answer;
repeat {
    $answer = prompt "Gimme an answer: ";
} until $answer ~~ 42;
```

You can't just put the <tt>my</tt> on the <tt>$answer</tt> in the block because the conditional is outside the scope of the block, and would not see the declaration.


## Phix

original... the got still you've as long as ,itself compile/run to used be can This

```Phix
if end
(&"test.exw"[1]cl)system
then >2(cl)length if
(&"\n"(pgm)mung,"test.exw")write_file = {}
write_file.e include
([$]cl)get_text = pgm string
()command_line = cl sequence
function end
("\n",lines)join return
for end (nup,rip,((([i]lines)split)reverse)join)substitute_all = [i]lines do (lines)length to 1=i for
(("\r\n",(rip,pun,pgm)substitute_all)split)reverse=lines sequence
(pgm string)mung function

(true,"*",("*","",3,7,"       -<>{}@!       ")join_by)split = rip constant
(true,"*",("*","",1,7,"-,=][)(")join_by)split = nup constant
(true,"*",("*","",1,7,"-,=[]()")join_by)split = pun constant
demo\rosetta\inverted_syntax.exw --
```


I should note that "if length(cl)>2 then" gets away by the skin of its teeth and would break were it written "if length(cl) > 2 then".


## PicoLisp

We define a read macro for reverted syntax

```PicoLisp
(de rv Prg
   (append (last Prg) (head -1 Prg)) )
```

Test:

```txt
(de needUmbrella (Raining)
   `(rv                                # Inverted syntax
      (on *NeedUmbrella)
      (println 'Need 'an 'umbrella)
      (when Raining) ) )

(de keepUmbrella (Raining)
   `(rv                                # Inverted syntax
      (on *KeepUmbrella)
      (println 'Still 'need 'an 'umbrella)
      (while Raining) ) )
```

```txt
: (pp 'needUmbrella)
(de needUmbrella (Raining)
   (when Raining                       # Traditional syntax
      (on *NeedUmbrella)
      (println 'Need 'an 'umbrella) ) )

: (pp 'keepUmbrella)
(de keepUmbrella (Raining)
   (while Raining                      # Traditional syntax
      (on *KeepUmbrella)
      (println 'Still 'need 'an 'umbrella) ) )
```



## PowerShell

The PowerShell syntax for an 'if' statement is very normal:

```PowerShell

if ((Get-Date 5/27/2016).DayOfWeek -eq "Friday") {"Thank God it's Friday!"}

```

```txt

Thank God it's Friday!

```

The order of the condition and expression can be easily reversed (with a slight change in syntax):

```PowerShell

function Action ([scriptblock]$Expression, [Alias("if")][bool]$Test)
{
    if ($Test) {&$Expression}
}

Set-Alias -Name say -Value Action

say {"Thank God it's Friday!"} -if (Get-Date 5/27/2016).DayOfWeek -eq "Friday"

```

```txt

Thank God it's Friday!

```



## Prolog

Prolog programs are made up of facts and rules. Facts are considered "true" if they are provable from the source code and "false" otherwise. The rules for proving new facts are described using "horn clauses" which are backwards "if" statements: the truth value of the second part becomes the truth value of the first part.

The facts themselves are usually expressed using "functors" involving parentheses, with the constant in front of the parentheses naming some property which applies to one or more items inside. As a result, they sometimes kind of look like backwards "is" statements.


```prolog
% Dracula is a vampire.
% Also, you become a vampire if someone who is a vampire bites you.
vampire(dracula).
vampire(You) :- bites(Someone, You), vampire(Someone).

% Oh no! Dracula just bit Bob...
bites(dracula, bob).
```


We load the source code into the interpreter and ask whether Alice and Bob are vampires...

```txt

?- vampire(alice).
false.

?- vampire(bob).
true .

```



## Python


```python
x = truevalue if condition else falsevalue
```



```python
with open("file.txt") as f:
    something(f)
```



## Qi


```qi

(define set-needumbrella
  Raining -> (set needumbrella true) where (= true Raining)
  Raining -> (set needumbrella false) where (= false Raining))

(define set-needumbrella
  Raining -> (if (= true Raining)
                 (set needumbrella true)
                 (set needumbrella false)))


Alternatives:

(define set-needumbrella
  Raining -> (set needumbrella true) where Raining
  Raining -> (set needumbrella false))

(define set-needumbrella
  Raining -> (if Raining
                 (set needumbrella true)
                 (set needumbrella false)))

(define set-needumbrella
  true  -> (set needumbrella true)
  false -> (set needumbrella false))

(define set-needumbrella
  A -> (set needumbrella A))

```



## R


This can be done with a simple function.


```R
do.if <- function(expr, cond) if(cond) expr
```


Because R evaluates function arguments lazily, "expr" is never evaluated unless "cond" evaluates to true.


```R
do.if(print("Wow! Lucky Guess!"), guess==6)
```


If you do not want to state "do.if" first, you can define an infix operator (any function whose name is bracketed in percent signs is treated as an infix operator), however custom infix operators have higher precedence than comparisons, so will usually require putting parentheses around the test.


```r
`%if%` <- function(expr, cond) if(cond) expr

print("Wow! Lucky Guess!") %if% (guess==6)
```



## Racket


Normally, all syntactic forms and functions are in prefix notation.
However, two <tt>.</tt>s allow infix notation in some cases.


```racket

#lang racket
(when #t (displayln "true"))
((displayln "true") . when . #t)

(define a 6)
(set! a 5)
(a . set! . 6)

```



## REXX

The closest thing that the REXX language has as far as inverted syntax
would be the use of the special case of the   '''signal'''   instruction:

```txt

        SIGNAL   {ON|OFF}   someCondition   {name}

```


```rexx
/*REXX program demonstrates a use of a special case of inverted syntax  (via SIGNAL ON).*/
signal on syntax
a=7
zz=444 / (7-a)
return zz
/*──────────────────────────────────────────────────────────────────────────────────────*/
syntax:  say  '***error***  program is attempting to do division by zero,'
         say  'the REXX statement number is: '  sigL  " and the REXX source is:"
         say  sourceLine(sigL)
         exit 13
```

'''output'''

```txt

***error***  program is attempting to do division by zero,
the REXX statement number is:  4  and the REXX source is:
zz=444 / (7-a)

```



## Ruby

Ruby takes, from Perl, the idea of a ''statement modifier''.
This looks like <code>''statement'' if ''condition''</code> and appends a condition to some statement.
This example shows how to invert <code>if</code>, <code>unless</code>, <code>while</code> and <code>until</code>.
These always check the condition ''before'' running the statement.


```ruby
# Raise ArgumentError if n is negative.
if n < 0 then raise ArgumentError, "negative n" end
raise ArgumentError, "negative n" if n < 0

# Exit 1 unless we can call Process.fork.
unless Process.respond_to? :fork then exit 1 end
exit 1 unless Process.respond_to? :fork

# Empty an array, printing each element.
while ary.length > 0 do puts ary.shift end
puts ary.shift while ary.length > 0

# Another way to empty an array, printing each element.
until ary.empty? do puts ary.shift end
puts ary.shift until ary.empty?
```


One can also modify a compound statement, as in <code>(warn "cannot fork"; exit 1) unless Process.respond_to? :fork</code>.

''Beware:'' The forms <code>begin ... end while ...</code> and <code>begin ... end until ...</code> have a different meaning to Ruby.
These forms check the condition ''after'' each iteration, so they run the loop at least once. [[Loops/Do-while#Ruby]] has more information.


## Scala


```scala
object Main extends App {

  val raining = true
  val needUmbrella = raining
  println(s"Do I need an umbrella?  ${if (needUmbrella) "Yes" else "No"}")
}
```



## Sidef


```ruby
# Inverted syntax with assignment
var raining = true;
[false]»(\var needumbrella);

# Inverted syntax with conditional expressions
if (raining==true) {needumbrella=true};
{needumbrella=true} -> if (raining==true);
(needumbrella=true) if (raining==true);
```



## Swift

Inverted syntax can be done with custom operators

```Swift
infix operator ~= {}
infix operator ! {}

func ~=(lhs:Int, inout rhs:Int) {
    rhs = lhs
}

func !(lhs:(() -> Void), rhs:Bool) {
    if (rhs) {
        lhs()
    }
}

// Traditional assignment
var a = 0

// Inverted using a custom operator
20 ~= a

let raining = true
let tornado = true
var needUmbrella = false
var stayInside = false

// Traditional conditional expression
if raining {needUmbrella = true}

// Inverted using a custom operator
_ = {stayInside = true} ! tornado
```



## Tcl

Copied verbatim from do.tcl, a part of tcllib's control package.

```tcl

# do.tcl --
#
#        Tcl implementation of a "do ... while|until" loop.
#
# Originally written for the "Texas Tcl Shootout" programming contest
# at the 2000 Tcl Conference in Austin/Texas.
#
# Copyright (c) 2001 by Reinhard Max <Reinhard.Max@gmx.de>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id: do.tcl,v 1.6 2004/01/15 06:36:12 andreas_kupries Exp $
#
namespace eval ::control {

    proc do {body args} {

	#
	# Implements a "do body while|until test" loop
	#
	# It is almost as fast as builtin "while" command for loops with
	# more than just a few iterations.
	#

	set len [llength $args]
	if {$len !=2 && $len != 0} {
	    set proc [namespace current]::[lindex [info level 0] 0]
	    return -code error "wrong # args: should be \"$proc body\" or \"$proc body \[until|while\] test\""
	}
	set test 0
	foreach {whileOrUntil test} $args {
	    switch -exact -- $whileOrUntil {
		"while" {}
		"until" { set test !($test) }
		default {
		    return -code error \
			"bad option \"$whileOrUntil\": must be until, or while"
		}
	    }
	    break
	}

	# the first invocation of the body
	set code [catch { uplevel 1 $body } result]

	# decide what to do upon the return code:
	#
	#               0 - the body executed successfully
	#               1 - the body raised an error
	#               2 - the body invoked [return]
	#               3 - the body invoked [break]
	#               4 - the body invoked [continue]
	# everything else - return and pass on the results
	#
	switch -exact -- $code {
	    0 {}
	    1 {
		return -errorinfo [ErrorInfoAsCaller uplevel do]  \
		    -errorcode $::errorCode -code error $result
	    }
	    3 {
		# FRINK: nocheck
		return
	    }
	    4 {}
	    default {
		return -code $code $result
	    }
	}
	# the rest of the loop
	set code [catch {uplevel 1 [list while $test $body]} result]
	if {$code == 1} {
	    return -errorinfo [ErrorInfoAsCaller while do] \
		-errorcode $::errorCode -code error $result
	}
	return -code $code $result

    }
}

#usage:
package require control
control::do {set i 0; puts "hello world"; incr i} until {$i > 0}

```


A more radical and probably ill-advised approach is to use the above
and modify the default tcl unknown procedure along these lines:


```tcl

rename unknown __unknown
proc unknown {args} {
   if {3 == [llength $args]} {
      package require control
      return [control::do {*}$args]
   } else {
      return [__unknown {*}$args]
   }
}
#usage
% {set i 0; puts "hello world"; incr i} until {$i > 0}
hello world

```


=={{header|TI-83 BASIC}}==
Assignment uses inverted syntax.

```ti83b
536→N
```




## Wortel

The <code>~</code> operator reverse the arguments of the next operator.

```wortel
; a = expr
:a expr
; expr = a
~:expr a
; if cond expr
@if cond expr
; if expr cond
~@if expr cond
```



## zkl

zkl is a OO language so the preferred style is object.method().method()...
When a computation needs to be done first, the compose op (colon) can be used maintain the "noun/verb/verb" flow: f() : ...
It is also useful to "unwind" computations so they are easier to read
but not require any temp vars.

```zkl
if (raining==True) needumbrella:=True;
(raining==True) : if (_) needumbrella:=True;
```


```zkl
a := 6
6 : a:=_
```


```zkl
key.sort(fcn(kv,kv2){kv[0] < kv2[0]}) : listUnzip(_) :
D.SD((_).xplode()) : return(_);
```




{{omit from|C}} <!-- doesn't have any -->
