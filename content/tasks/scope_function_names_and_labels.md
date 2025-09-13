+++
title = "Scope/Function names and labels"
description = ""
date = 2019-09-11T18:55:46Z
aliases = []
[extra]
id = 12940
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

{{task|Basic language learning}}[[Category:Scope]]
## Task

Explain or demonstrate the levels of visibility of function names and labels within the language.


## See also

* [[Variables]] for levels of scope relating to visibility of program variables
* [[Scope modifiers]] for general scope modification facilities





## ALGOL 68

Algol 68 follows the traditional block structure scoping rules introduced by Algol 60.


A label, function (PROC), variable etc. declared in an outer block is only visible within that block and all inner blocks,
unless another label, function, etc. is declared with the same name in an inner block.


In addition, functions etc. declared between IF and THEN are in scope in the THEN and ELSE parts, but
declarations in the THEN part and not visible in the ELSE part (and vice versa), so the following is invalid:

```algol68
IF PROC x = ...;
   ...
THEN
    # can call x here #
    PROC y = ...;
    ...
    GO TO l1 # invalid!! #
ELSE
    # can call x here, but not y #
    ...
    l1: ...
FI
```

Similarly, declarations between WHILE and DO are in scope between DO and OD and those declared between CASE and IN are visible in the IN and OUT parts of a CASE.


Note that labels cannot be defined between IF and THEN, between WHILE and DO and between CASE and IN.


## ALGOL W

Algol W follows the scoping rules of Algol 60 - a function (called a procedure) or a label is only in scope within the block it is declared in and in blocks nested within that block. If a procedure, label, variable etc. is declared with same name in an inner block,
the outer definition is inaccessible.


## AWK

In awk, function names are always global and can be referenced in sections of code appearing before the definition:

```awk
# This program outputs a greeting
BEGIN {
  sayhello()    # Call the function defined below
  exit
}

function sayhello {
   print "Hello World!"    # Outputs a message to the terminal
}
```

Note that the awk extraction and reporting language is data driven and does not support arbitary line labels.


## Axe

Function names (which are also labels) are global in Axe. They can be accessed from anywhere in any source file that gets compiled together.


## BASIC

Line numbers are used instead of labels. These are also immediately accessible as soon as the line is entered, even if the program is not run:

```basic>GOTO 50: REM THIS WILL WORK IMMEDIATELY</lang

The visibility of functions depends on the implementation. Most versions of basic will allow a function to be referenced from a point in the code prior to its definition. However, other implementations may require the function definition to be run, before it can be used:

```basic
10 DEF FN S(A)=A*A
20 PRINT FN S(2): REM THIS WILL WORK
30 PRINT FN C(2): REM CALLING A FUNCTION PRIOR TO DEFINITION MAY NOT WORK
40 GOSUB 9000
50 PRINT FN C(2): REM THIS WILL WORK
60 END
9000 DEF FN C(A)=A*A*A
9999 RETURN
```



## bc

Functions have global scope and must be defined before usage. 
A defined function can be replaced with a newer definition later in the file. 
Recursive calls are possible because function are considered as defined within their own body.

There are no labels in bc.


```bc
f(1)              /* First output line */ 
define f(x) {
    return(x)
}
f(3)              /* Second output line */

define f(x) {       
    return(x - 1)
}
f(3)              /* Third output line */
```


```txt
Runtime error (func=(main), adr=3): Function f not defined.
3
2
```



## C

Demonstrating function scope as well as goto in a C program invariably leads to code like the one below. The [http://en.wikipedia.org/wiki/Scope_(computer_science)#C Wikipedia article] is a good starting point.


```C

#include <stdio.h>

#define sqr(x) ((x) * (x))
#define greet printf("Hello There!\n")

int twice(int x)
{
	return 2 * x;
}

int main(void)
{
	int x;

	printf("This will demonstrate function and label scopes.\n");
	printf("All output is happening through printf(), a function declared in the header stdio.h, which is external to this program.\n");
	printf("Enter a number: ");
	if (scanf("%d", &x) != 1)
		return 0;
	
	switch (x % 2) {
	default:
		printf("Case labels in switch statements have scope local to the switch block.\n");
	case 0:
		printf("You entered an even number.\n");
		printf("Its square is %d, which was computed by a macro. It has global scope within the translation unit.\n", sqr(x));
		break;
	case 1:
		printf("You entered an odd number.\n");
		goto sayhello;
	jumpin:
		printf("2 times %d is %d, which was computed by a function defined in this file. It has global scope within the translation unit.\n", x, twice(x));
		printf("Since you jumped in, you will now be greeted, again!\n"); 
	sayhello:
		greet;
		if (x == -1)
			goto scram;   
		break;
	}

	printf("We now come to goto, it's extremely powerful but it's also prone to misuse. Its use is discouraged and it wasn't even adopted by Java and later languages.\n");

	if (x != -1) {
		x = -1;   /* To break goto infinite loop. */
	 	goto jumpin;  
	}

scram:
	printf("If you are trying to figure out what happened, you now understand goto.\n");
	return 0;
}
```


{{out}} Example run:

```txt

This will demonstrate function and label scopes.
All output is happening throung printf(), a function declared in the header stdio.h, which is external to this program.
Enter a number: 5

You entered an odd number.
Hello There!
We now come to goto, it's extremely powerful but it's also prone to misuse. Its use is discouraged and it wasn't even adopted by Java and later languages.
2 times -1 is -2, which was computed by a function defined in this file. It has global scope within the translation unit.
Since you jumped in, you will now be greeted, again!
Hello There!
If you are trying to figure out what happened, you now understand goto.

```



## Eiffel


Functions (routines which return a result), procedures (routines which have no result) and variables are considered features of a class and follow the same visibility rules. Eiffel allows for information hiding, where features can be selectively hidden from particular classes.

All features are assigned (at compile-time) to a particular scope defined by the most-recently preceding feature clause. Various feature clauses are given below, from least restrictive (most visible) to most restrictive (most hidden).

```Eiffel
--assume A, B and C to be valid classes
class X
feature -- alias for "feature {ANY}"
-- ANY is the class at the top of the class hierarchy and all classes inherit from it
-- features following this clause are given "global" scope: these features are visible to every class

feature {A, B, C, X}
-- features following this clause are only visible to the specified classes (and their descendants)
-- classes not in this set do not even know of the existence of these features

feature {A, B, C}
-- similar to above, except other instances of X cannot access these features

feature {X}
-- features following this clause are only visible to instances of X (and its descendants)

feature {NONE}
-- NONE is the class at the bottom of the class hierarchy and inherits from every class
-- features following this clause are only visible to this particular instance of X

end
```




## Erlang

The scope of an Erlang function is limited to the module where it is declared.
It is possible to modify function scope by exporting a function from a module.
There are no labels.


```Erlang

-module( a_module ).

-export( [exported_function/0] ).

exported_function() -> 1 + local_function().

local_function() -> 2.

```



## Go

Go is block scoped and has both functions and labels.

Functions can only be declared at the "top level" in a source file, that is, not nested or declared inside of anything else.  This gives them "package scope," making them visible within a package (which can consist of multiple source files.)  A They are not visible outside their package unless "exported."  They are exported if the function name begins with an upper case letter, specifically Unicode class "Lu."

A "function literal" is different than a declaration.  It is an expression that returns a function value, which can be assigned or passed around like any other value.

A function definition, either a top-level declaration or a function literal, represents a function block.


```go
package main

import (
    "fmt"
    "runtime"

    "ex"
)

func main() {
    // func nested() { ... not allowed here

    // this is okay, variable f declared and assigned a function literal.
    f := func() {
        // this mess prints the name of the function to show that it's an
        // anonymous function defined in package main
        pc, _, _, _ := runtime.Caller(0)
        fmt.Println(runtime.FuncForPC(pc).Name(), "here!")
    }

    ex.X(f) // function value passed to exported function

    // ex.x() non-exported function not visible here
}
```


```go
package ex

import (
    "fmt"
    "runtime"
)

// X is exported.
func X(x func()) {
    pc, _, _, _ := runtime.Caller(0)
    fmt.Println(runtime.FuncForPC(pc).Name(), "calling argument x...")
    x()
}

func x() { // not exported, x not upper case.
    panic("top level x")
}
```

```txt

ex.X calling argument x...
main.func·001 here!

```


Labels can only be declared in function blocks.  The scope is the function block where the label is declared excluding any nested function blocks.

```go
package main

import "fmt"

func main() {
    // labels loop and y both in scope of main
loop:
    for false {
        continue loop
    }
    goto y
y:
    y := 0 // variable namespace is separate from label namespace

    func() {
        // goto loop ...loop not visible from this literal

        // label y in outer scope not visible so it's okay to define a label y
        // here too.
    y:
        for {
            break y
        }
        y++ // regular lexical scoping applies to variables.
    }()
    fmt.Println(y)
}

// end: // labels not allowed outside function blocks
```

```txt

1

```



## haskell

Functions are considered global in haskell and can be referenced in the sections of code appearing before the function definition. The following code illustrates the same. add2 was declared after add3 and used in add3. The variables x,y and z are local to the function. 


```haskell

add3 :: Int -> Int-> Int-> Int
add3 x y z = add2 x y + z

add2 :: Int -> Int -> Int
add2 x y = x + y

main :: putStrLn(show (add3 5 6 5))

```

```txt

ghci>main
16

```


In the function below the functions g and h are local to the function getSquaredSum. They cannot be called from anywhere outside the function.


```haskell

getSquaredSum :: Int-> Int-> Int
getSquaredSum x y = g x + h y
	where
	g a = a*a
	h b = b*b

```

```txt
 
ghci> getSquaredSum 3 4
25
ghci>h 4
<interactive>: 115:1:error: 
Variable not in scope: h :: Integer -> t

```

=={{header|Icon}} and {{header|Unicon}}==

In both languages, function names (including <i>procedure</i> names - <i>functions</i> are language builtins
while <i>procedures</i> are written in the language) have global scope.  In Unicon, class <i>methods</i> are
locally visible within the class but must be referenced through the class instance externally.

There are no labels in either language.


## J


'''NAMES'''

J is scoped lexically but with non-nested block scope. Nesting can be emulated, where that is required. But deep nesting can be difficult to understand and debug so the coder is penalized (with a bit of extra work) for implementing deep nesting.

Specifically, J provides two scopes for user defined names:

'''Local''' scope, names defined using =. will have local scope if a block scope exists.


```j>   a=. 1</lang


'''Locale''' scope, names defined using =: will have locale scope (and the '''base''' locale is used by default).


```j>   b=: 2</lang


Names may include a locale qualifier. A locative is a qualified name. Locale qualifiers contain two '''_''' characters, are a suffix on what would be the unqualified name. Locale qualifiers come in two forms: absolute and relative. Relative locale qualifiers use another name in the current locale to identify the target locale. Absolute locatives place the locale name between the two '''_''' characters, while relative locatives name the locale reference following the pair of '''_''' characters. 


```j
   c_thingy_=: 3
   d=: <'test'
   e__d=: 4
   b + e_test_
6
```


If a local definition exists for a name, it is an error to use use =: to assign that name (use a locative instead of the unqualified name if you really need to do this).


```j
verb define ''
  f=. 6
  g=: 7
  g=. 8
  g=: 9
)
|domain error
|   g    =:9
```


Meanwhile, there is a global current locale (default: '''base''') but each verb's definition is evaluated with the current locale being the locale where that verb was defined.

Name resolution first checks for local definitions for a name, and if none are found the current locale is checked, and if nothing is found there the current locale's search path is used to check other locales. By default the '''z''' locale is included as the last element of this path. (And, since the z locale contains the system defined words, removing it from a locale's path will break most things in that locale.)

The current locale during the execution of a verb is the locale where that verb was defined. But the current locale can be changed when the current ''named'' verb finishes using <code>18!:4</code>.

If you want to emulate static nested scope (or dynamic scope) you need to arrange for the desired behavior using the locale path mechanism.

'''LABELS'''

Labels are available in explicit definitions, and are names beginning with '''label_''' and ending with a '''.'''. You may use them with a goto which is a name beginning with '''goto_''' and ending with a '''.'''. Use of labels is restricted to the scope where they are defined, and they cannot be used to enter control structures (except in the sense of the normal entry point).

<label j>example=:3 :0
  if. y do.
    echo 0
    goto_a.
    echo 1
  else.
    echo 2
    goto_a.
    echo 3
  end.
  echo 4
  label_a.
  echo 5
)
   example 1
0
5
   example 0
2
5
```



## jq

jq is scoped lexically.  

A function can only be called within its definition or following it, it being understood that functions can in effect be passed by name as parameters to other functions.

A further restriction is that inner functions are invisible outside their enclosing function, an inner function being one which is defined within the body of another.  

A function that is not defined within an inner function
will be called a top-level function.  The lowest-level function in
which an inner function is defined will be called its enclosing
function.

If more than one outer function has the same name, then the last
definition effectively overwrites the first, at least as far as subsequent invocations are concerned.  

A similar rule applies to two inner functions defined within the same enclosing function. Otherwise, inner functions of the same
name can co-exist.  In particular, a function named NAME may define an inner function of the same name.  For example:

```jq
def NAME:
  def NAME: 2;
  1, NAME;  # this calls the inner function, not the outer function

NAME # => 1, 2
```


'''Mutually Defined Functions'''
The "declare-before-use" rule means that two top-level functions cannot be defined in terms of each other. Consider the following example:

```jq
def F(x): if x == 0 then M(x) else 1 end;  # NOT POSSIBLE
def M(x): if x == 1 then F(x) else 2 end;
```

There are several possible workarounds using inner functions. For example, if both F and M must be top-level functions, we could define them as follows:

```jq
def F(x):
  def M(x): if x == 1 then F(x) else 2 end;
  if x == 0 then M(x) else 1 end;

def M(x): if x == 1 then F(x) else 2 end;
```

If F and M are not required to be top-level functions, then both F and M could be defined as inner functions of the same enclosing function.


## Julia

In Julia, there is one type of global scope, defined as the scope of the individual module (module names can be exported or referenced by module name, but there is no global scope above module scope). User code not defined within a specific module has global scope within the predefined default module main. There are two types of local or nonglobal scope in Julia: hard local scope, which is the scope of variables defined within functions, and the "soft" local scope of variables within code structures, as quoted below from Julia's documentation:

```txt

Type of scope | block/construct introducing this kind of scope
----------------------------------------------------------------
Global Scope  |	module, baremodule, at interactive prompt (REPL)
Local Scope   |  Soft Local Scope: for, while, comprehensions, try-catch-finally, let
Local Scope   |  Hard Local Scope: functions (either syntax, anonymous & do-blocks), struct, macro

```

Hard local scope means that a variable defined within a function defaults to being local to
the function unless this is overridden with the <code>global</code> keyword which allows
such a variable to reference a module-level global variable. Soft local scope means that variables
within such a code block reference variables of the same name in the parent scope unless the
variable is explicitly declared to be local with the <code>local</code> keyword.
<br />
Julia uses lexical scoping, so the scope of a variable as defined above can be determined just by looking at the code where the variable was defined. Modules are nest-able within other modules and functions are nest-able within functions.


## Kotlin

Functions in Kotlin can be declared at top level, within a class/object/interface or can be nested within another function.

Top level functions (i.e. functions declared outside any class) support three levels of visibility:

1. public - visible everywhere (the default if no modifier is used).

2. internal - visible anywhere within the same module.

3. private - visible only within the current file.

Functions declared within a class support four levels of visibility:

1. public - visible everywhere its class is visible (the default if no modifier is used).

2. internal - visible anywhere within the same module that its class is visible.

3. protected - visible only inside its class or any sub-classes thereof. 

4. private - visible only inside its class.

Functions declared within an object (i.e. a singleton class) support the same levels of visibility as a normal class except for 'protected'. 

Functions declared within an interface are usually public but can be private if they have a body.

Functions declared within another function do not have any visibility modifiers but are in scope from their point of declaration to the end of the enclosing function.


In Kotlin any expression can be marked with a label (an identifier followed by an @ sign). Currently, they are used with the 'break', 'continue' or 'return' keywords - to break out of the labelled loop, to continue with the next iteration of the labelled loop or to return from a labelled lambda expression, respectively.

Such labels are in scope from their point of declaration to the end of the corresponding block. 

Labels can also be used with the 'this' keyword to distinguish between different outer scopes (classes or function receivers) where there is more than one to choose from.

The following program illustrates some of these usages.

```scala
// version 1.1.2

// top level function visible anywhere within the current module
internal fun a() = println("calling a")

object B {
    // object level function visible everywhere, by default
    fun f() = println("calling f")
}

open class C {
    // class level function visible everywhere, by default
    fun g() = println("calling g") 

    // class level function only visible within C
    private fun h() = println("calling h")

    // class level function only visible within C and its subclasses
    protected fun i() {
        println("calling i")
        println("calling h")  // OK as h within same class
        // nested function in scope until end of i
        fun j() = println("calling j")
        j()
    }
}

class D : C(), E {
    // class level function visible anywhere within the same module
    fun k() {
        println("calling k")
        i()  // OK as C.i is protected
        m()  // OK as E.m is public and has a body
    }
}

interface E {
    fun m() {
        println("calling m")
    }
}
     
fun main(args: Array<String>) {
    a()    // OK as a is internal
    B.f()  // OK as f is public
    val c = C()
    c.g()  // OK as g is public but can't call h or i via c
    val d = D()
    d.k()  // OK as k is public
    // labelled lambda expression assigned to variable 'l'
    val l = lambda@ { ->
        outer@ for (i in 1..3) {
            for (j in 1..3) {
                if (i == 3) break@outer    // jumps out of outer loop
                if (j == 2) continue@outer // continues with next iteration of outer loop
                println ("i = $i, j = $j") 
            }
            if (i > 1) println ("i = $i")  // never executed
        }
        val n = 1
        if (n == 1) return@lambda  // returns from lambda
        println("n = $n")  // never executed
    }
    l()  // invokes lambda
    println("Good-bye!")   // will be executed 
}
```


```txt

calling a
calling f
calling g
calling k
calling i
calling h
calling j
calling m
i = 1, j = 1
i = 2, j = 1
Good-bye!

```



## Oforth

Functions are global and must be defined before use.
Methods are global and must be declared before use. They can be used before a method implementation.


## Perl

Perl allows various ways to futz with scope, but keeping it simple: Routines are package-scoped, and in each package the final definition of the routine is the one that is used throughout. Labels are generally also package-scoped, except when it comes to <code>goto</code>; let's don't even go there.

```perl
no warnings 'redefine';

sub logger { print shift . ": Dicitur clamantis in deserto." };   # discarded

logger('A');                                                      # can use before defined
HighLander::logger('B');                                          # ditto, but referring to another package

package HighLander {
logger('C');
sub logger { print shift . ": I have something to say.\n" };       # discarded
sub down_one_level {
    sub logger { print shift . ": I am a man, not a fish.\n" };    # discarded
    sub down_two_levels {
        sub logger { print shift . ": There can be only one!\n" }; # routine for 'Highlander' package
    }
}
logger('D');
}

logger('E');
sub logger { 
   print shift . ": This thought intentionally left blank.\n"      # routine for 'main' package
};
```

```txt
A: This thought intentionally left blank.
B: There can be only one!
C: There can be only one!
D: There can be only one!
E: This thought intentionally left blank.
```



## Perl 6

First a little hand-wavey exposition. The lines are rather blurry in Perl 6 between subroutines, methods, operators and functions. Methods are associated with an object and are inheritable. Subroutines and operators are not. Other than that though there is a lot of overlap. "A function" doesn't really have a specific definition, but is more of a generic term used when talking about code reference type of things.

Methods don't have a separate scope from the object they are attached to. If the object is in scope, the method will be.

A subroutine is really just another type of object. It has a code reference and has ROUTINE semantics attached to it. The same holds for operators. Operators are really just subroutines with a funny calling convention. That being the case, scoping for subroutines very closely follows scoping rules for any other Perl 6 variable type. 

In general, subroutines are "my" variables by default (if you don't specify, the "my" is implicit), meaning scoping is lexical to the enclosing block and flows inward. A subroutine defined within a block will be visible to everything inside that block, even other blocks within that block. However, any inner block can define its own subroutine with the same name and that will be used in preference to the routine from an outer block. That implies you can easily override / redefine core functions from the Perl 6 setting. The setting is the collection of built in functions supplied by Perl 6, typically and somewhat incongruously referred to as "CORE" even though technically it is the outermost scope. ( SKIN? BARK? CRUST? ... oooo! EXOSKELETON! :-) ) 

Alternately, subroutines may be declared as an "our" variable making it a package global, visible anywhere in the packages' namespace. That is somewhat discouraged though as it pollutes the namespace and reduces the granularity of control.

There are several ways to modify the relative scope of a subroutine (any item in the symbol table really) by adding modifiers to the name.  

```txt

CALLER    # Contextual symbols in the immediate caller's lexical scope
OUTER     # Symbols in the next outer lexical scope
UNIT      # Symbols in the outermost lexical scope of compilation unit
SETTING   # Lexical symbols in the unit's DSL (usually CORE)
PARENT    # Symbols in this package's parent package (or lexical scope)

```



```perl6
# call a routine before it has been defined
say log();              # prints: outer

# define a subroutine that overrides a CORE function
sub log { 'outer' }; 
{
    # redefine the subroutine in this block
    sub log { 'inner' };
    {
        # redefine the subroutine yet again
        sub log { 'way down inside' };
        
        # call it within this block
        say log();                 # prints: way down inside
        
        # call it from the block one level out
        say &OUTER::log();         # prints: inner
        
        # call it from the block two levels out
        say &OUTER::OUTER::log();  # prints: outer
        
        # call it from the outermost block
        say &UNIT::log();          # prints: outer
        
        # call a subroutine that is post declared in outermost scope
        outersub()
    }

    {
        # subroutine in an inner block that doesn't redefine it
        # uses definition from nearest enclosing block
        say log();      # prints: inner
    }
    # call it within this block
    say log();          # prints: inner
    
    # call it from the block one level out
    say &OUTER::log();  # prints: outer
}

sub outersub{ 
    # call subroutine within this block - gets outer sub
    say log();          # prints: outer
    
    # call subroutine from the scope of the callers block
    say &CALLER::log(); # prints: way down inside
    
    # call subroutine from the outer scope of the callers block
    say &CALLER::OUTER::log(); # prints: inner
    
    # call the original overridden CORE routine
    say &CORE::log(e);  # prints: 1 ( natural log of e )
}
```


Labels are less interesting and are typically useful only for control flow in looping constructs. They generally follow the same scoping rules as "my" variables. There is nearly always easier ways to do control flow though, so they aren't heavily used.


## Phix

Functions are private (restricted to a single file) by default, or can be made global by prefixing the definition with the global keyword to make it visible everywhere.


## PL/I


```PL/I

Functions are normally internal to a program. If they are at the nesting level
immediately within the program, they are accessible from anywhere in the program.

Functions can also be encapsuled in a package, and the function name exported.

Functions can be compiled separately, and then linked with a program
in which case they are globally accessible.

```



## PowerShell

A function exists in the scope in which it was created. 

If a function is part of a script, the function is available to statements within that script. By default, a function in a script is not       available at the command prompt. 

You can specify the scope of a function. For example, the function is added to the global scope in the following example: 

```PowerShell

function global:Get-DependentService
{
    Get-Service | Where-Object {$_.DependentServices}
}

```

When a function is in the global scope, you can use the function in scripts, in functions, and at the command line.

Functions normally create a scope. The items created in a function, such as variables, exist only in the function scope.

For more information about scope in Windows PowerShell, see about_Scopes

```PowerShell

Get-Help about_Scopes

```



## Python

:In Python; our chief creator of new scopes is a function definition ... functions and classes... classes and functions... Our ''two'' creators are functions and classes... and files... Our ''three'' creators are ... I'll come in again.

Some (rather dry) rules are:
# All names, (of functions, classes, as well as variables), are scoped in the same way.
# A names scope is its closest enclosing file, function, or class.
# Names belong to the scope where they are assigned-to (or bound)

;Cf.:
* Ka-Ping Yee has a tutorial on the above [http://www-inst.eecs.berkeley.edu/~selfpace/cs9honline/Q2/scope.html here].
* This Python Enhancement Proposal: [http://www.python.org/dev/peps/pep-3104/ PEP 3104] introduces the non-local keyword of Python 3.
* And of course, [http://www.youtube.com/watch?v=vt0Y39eMvpI this]!


## Racket

Racket inherits the strict lexical-scopedness of Scheme, so function bindings (like any other bindings) are visible only within their scope.  For example

```racket

(define (foo x)
  (define (bar y) (+ x y))
  (bar 2))
(foo 1) ; => 3
(bar 1) ; => error

```

but that applies only to the *bindings* -- the actual function values (like other values) can be passed around freely:

```racket

(define (foo x)
  (define (bar y) (+ x y))
  bar)
(foo 1)     ; => #<procedure:bar>
((foo 1) 2) ; => 3

```


But it should be noted that Racket is flexible enough to make it possible to implement other kinds of scope.


## REXX

In REXX, labels (which are also the name of in-stream procedures (or subroutines or functions) are identified by:
::::: (optional blanks)
::: a REXX symbol
::::: (optional blanks)
::: a colon (''':''')
::::: (optional blanks)
::::: (optional REXX statement (''';''')
::::: (optional semicolon (''';''')
::::: (optional blanks)

(all of the above are normally contained on one line (record), but may (syntactically) be continued by the normal REXX rules for continuation.


Any label can be referenced from anywhere in the REXX program   (global scope).


Multiple labels (with the same name) are not considered an error in the REXX language;   the first label found (topmost) is used.

REXX comments may be added anywhere blanks can be used.

Multiple labels may be specified on the same line with: 
::::: (optional blanks)
::: a REXX symbol
::::: (optional blanks)
::: a colon (''':''')
::::: (optional blanks)
::::::: ────(a repeat of the above as many times as is possible on a line of code)────

```rexx
/*REXX program  demonstrates  the  use of    labels    and  also a   CALL   statement.  */
blarney = -0                                     /*just a blarney & balderdash statement*/
signal do_add                                    /*transfer program control to a  label.*/
ttt = sinD(30)                                   /*this REXX statement is never executed*/
                                                 /* [↓]   Note the case doesn't matter. */
DO_Add:                                          /*coming here from the SIGNAL statement*/

say 'calling the sub:  add.2.args'
call add.2.args 1, 7                             /*pass two arguments:   1   and a   7  */
say 'sum =' result                               /*display the result from the function.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add.2.args: procedure;  parse arg x,y;   return x+y       /*first come, first served ···*/
add.2.args: say 'Whoa Nelly!! Has the universe run amok?' /*didactic, but never executed*/
add.2.args: return  arg(1) + arg(2)                       /*concise,   "    "       "   */
```

'''output'''

```txt

calling the sub:  add.2.args
sum = 8

```



## Ring


```ring

# Project : Scope/Function names and labels

see "What is your name?" + nl
give name 
welcome(name)

func welcome(name)
        see "hello " + name + nl

```

Output:

```txt

What is your name?
CalmoSoft
hello CalmoSoft

```



## Ruby

'def' starts the definition of a method, and 'end' ends it - no cute little curly braces.

```ruby

def welcome(name)
   puts "hello #{name}"
end
puts "What is your name?"
$name = STDIN.gets
welcome($name)
return 
```

'''output'''

```txt

What is your name?
xyz
hello xyz

```


## Scala


```Scala
object ScopeFunction extends App {
  val c = new C()
  val d = new D()
  val n = 1

  def a() = println("calling a")

  trait E {
    def m() = println("calling m")
  }

  a() // OK as a is internal
  B.f() // OK as f is public

  class C {
    // class level function visible everywhere, by default
    def g() = println("calling g")

    // class level function only visible within C and its subclasses
    protected def i() {
      println("calling i")
      println("calling h") // OK as h within same class
      // nested function in scope until end of i
      def j() = println("calling j")

      j()
    }

    // class level function only visible within C
    private def h() = println("calling h")
  }

  c.g() // OK as g is public but can't call h or i via c

  class D extends C with E {
    // class level function visible anywhere within the same module
    def k() {
      println("calling k")
      i() // OK as C.i is protected
      m() // OK as E.m is public and has a body
    }
  }

  d.k() // OK as k is public

  object B {
    // object level function visible everywhere, by default
    def f() = println("calling f")
  }

  val l = (i:Int, j: Int) => println(i,j)

  println("Good-bye!") // will be executed

}
```



## Sidef

In Sidef, the same rule which is applied to variable scoping, is applied to functions and classes as well, which means that a function defined inside another function is not visible outside the current scope.

```ruby
# Nested functions
func outer {
    func inner {};   # not visible outside
}

# Nested classes
class Outer {
    class Inner {};  # not visisble outside
}
```



## Tcl

There are a number of different symbol types in Tcl, all of which are handled independently. Each namespace contains a mapping from (simple) command names to command implementations; when a command is looked up, the search is done by looking in the current namespace, then in the namespaces on that namespace's path (which is usually empty), and finally in the global namespace. There are ''no'' local commands (unlike with variables, though a lambda expression in a variable can act very similarly to a command). Commands only have a mapping after they have been created; the <code>proc</code> “declaration” is just a command that creates a procedure at the point where it is called.

```tcl
doFoo 1 2 3;    # Will produce an error

proc doFoo {a b c} {
    puts [expr {$a + $b*$c}]
}
doFoo 1 2 3;    # Will now print 7 (and will continue to do so until doFoo is renamed or deleted
```

Tcl does not support labels, either outside or inside procedures. (Other mechanisms are used for jumps and state machines.)


## UNIX Shell


There is no lookahead in the shell, so functions cannot be called until their definition has been run:


```sh
#!/bin/sh
multiply 3 4    # This will not work
echo $?    # A bogus value was returned because multiply definition has not yet been run.

multiply() {
  return `expr $1 \* $2`    # The backslash is required to suppress interpolation
}

multiply 3 4    # Ok. It works now.
echo $?         # This gives 12
```


The shell does not support the use of arbitrary line labels.


## zkl

zkl has no labels.

Functions have two modifiers, public (the default) and private. Private is the same as public inside the compilation unit, outside that unit/file, a private function can only be accessed via reflection.

```zkl
class C{ fcn [private] f{} }
```



## ZX Spectrum Basic

Functions are visible as soon as they are entered, even if the program is not run. Line numbers are used instead of labels. These are also immediately accessible as soon as the line is entered, even if the program is not run:

```zxbasic
9000 REM The function is immediately visible and usable
9010 DEF FN s(x)=x*x

PRINT FN s(5): REM This will work immediately
GO TO 50: REM This will work immediately
```


