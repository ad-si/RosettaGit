+++
title = "Runtime evaluation/In an environment"
description = ""
date = 2019-07-20T12:32:54Z
aliases = []
[extra]
id = 3556
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "applescript",
  "autohotkey",
  "bbc_basic",
  "bracmat",
  "clojure",
  "common_lisp",
  "e",
  "echolisp",
  "elena",
  "erlang",
  "factor",
  "forth",
  "genyris",
  "go",
  "groovy",
  "j",
  "java",
  "javascript",
  "jsish",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "mathematica",
  "metafont",
  "oorexx",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "php",
  "picolisp",
  "pike",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "sidef",
  "simula",
  "snobol4",
  "tcl",
  "txr",
  "unix_shell",
  "zkl",
]
+++

## Task

{{task}} Given a program in the language (as a string or AST) with a free variable named <var>x</var> (or another name if that is not valid syntax), evaluate it with <var>x</var> bound to a provided value, then evaluate it again with <var>x</var> bound to another provided value, then subtract the result of the first from the second and return or print it.

Do so in a way which:
* does not involve string manipulation of the input source code
* is plausibly extensible to a runtime-chosen set of bindings rather than just <var>x</var>
* does not make <var>x</var> a ''global'' variable
or note that these are impossible.


### See also

* For more general examples and language-specific details, see [[Eval]].
* [[Dynamic variable names]] is a similar task.


## ALGOL 68

<!-- {{does not work with|ALGOL 68|Standard - variable names are not visible at run time with classic compilers.}} -->
<!-- {{does not work with|ELLA ALGOL 68|Any This implementation is a compiler}} -->
Variable names are generally not visible at run time with classic compilers.  However '''ALGOL 68G''' is an interpretor and it retains this ability.  Note that ''evaluate'' returns a '''string'''.

```algol68
PROC eval_with_x = (STRING code, INT a, b)STRING:
	(INT x=a; evaluate(code) ) + (INT x=b; evaluate(code));
print((eval_with_x("2 ** x", 3, 5), new line))
```

Output:

```txt

         +8        +32

```



## AppleScript

<!-- In progress... trying to understand how that works... -->
AppleScript's '''run script''' command allows to interpret and execute the string passed to it as an arbitrarily complex (or simple) script; such a string may thus be viewed as the "program" considered in the task description.<br/>
Each invocation of the '''run script''' command dynamically happens in a separate execution context, so there are no side-effects; on the other hand, this means that such an invocation is quite costly.<br/>
Arguments may be passed as a list of arbitrary values; this however requires the program to be written with an explicit '''run''' handler.<br/>
The result is the value (if any) returned by the program; any valid AppleScript value may be returned.<br/>


Given the above, the task may easily be implemented along these lines:

```AppleScript

on task_with_x(pgrm, x1, x2)
	local rslt1, rslt2
	set rslt1 to run script pgrm with parameters {x1}
	set rslt2 to run script pgrm with parameters {x2}
	rslt2 - rslt1
end task_with_x

```

Example usage (for legibility purposes, the program is stored into an intermediate variable):

```AppleScript

set pgrm_with_x to "
on run {x}
	2^x
end"

task_with_x(pgrm_with_x, 3, 5)

```

The result is 24.0 (a real number).


## AutoHotkey

AutoHotkey does not provide an API to the local symbol table.  Local variables are also not supported within scopes outside functions.  However, a local environment can be simulated by wrapping code in a temporary function.  

```AutoHotkey
msgbox % first := evalWithX("x + 4", 5)
msgbox % second := evalWithX("x + 4", 6)
msgbox % second - first
return

evalWithX(expression, xvalue)
{
global script
script = 
(
    expression(){
    x = %xvalue%  ; := would need quotes
    return %expression%
  }
)
renameFunction("expression", "")  ; remove any previous expressions
gosub load ; cannot use addScript inside a function yet
exp := "expression"
return %exp%()
}

load:
DllCall(A_AhkPath "\addScript","Str",script,"Uchar",0,"Cdecl UInt")
return

renameFunction(funcName, newname){
static         
x%newname% := newname   ; store newname in a static variable so its memory is not freed
strput(newname, &x%newname%, strlen(newname) + 1)
if fnp := FindFunc(funcName)
  numput(&x%newname%, fnp+0, 0, "uint")
}
```



## BBC BASIC


```bbcbasic
      expression$ = "x^2 - 7"
      one = FN_eval_with_x(expression$, 1.2)
      two = FN_eval_with_x(expression$, 3.4)
      PRINT two - one
      END
      
      DEF FN_eval_with_x(expr$, x)
      = EVAL(expr$)
```



## Bracmat

<lang>( ( eval-with-x
  =   code a b argument
    .   !arg:((=?code),?a,?b,?argument)
      &   (!b:?x&!code$!argument)
        + -1*(!a:?x&!code$!argument)
  )
& out$(eval-with-x$((='(.$x^!arg)),3,5,2))
& out$(eval-with-x$((='(.$x^!arg)),12,13,2))
);
```

Output:

```txt
16
25
```



## Clojure


We must define x as global, but we use dynamic bindings.  Only functions within the binding will see the newly bound value of x, before it re-establishes the bindings that existed before.

```clojure
(def ^:dynamic x nil)

(defn eval-with-x [program a b]
  (- (binding [x b] (eval program))
     (binding [x a] (eval program))))
```



```clojure
(eval-with-x '(* x x) 4 9)
=> 65
```




## Common Lisp



```lisp
(defun eval-with-x (program a b)
  (let ((at-a (eval `(let ((x ',a)) ,program)))
        (at-b (eval `(let ((x ',b)) ,program))))
    (- at-b at-a)))
```



```lisp
(eval-with-x '(exp x) 0 1)
=> 1.7182817
```


This version ensures that the program is compiled, once, for more efficient execution:


```lisp
(defun eval-with-x (program a b)
  (let* ((f (compile nil `(lambda (x) ,program)))
         (at-a (funcall f a))
         (at-b (funcall f b)))
    (- at-b at-a)))
```


=={{header|Déjà Vu}}==

```dejavu
local fib n:
	if <= n 1:
		n
	else:
		+ fib - n 1 fib - n 2

local :code !compile-string dup "-- fib x" #one less than the xth fibonacci number

!run-blob-in { :fib @fib :x 4 } code
!run-blob-in { :fib @fib :x 6 } code
!. -
```

```txt
5
```



## E


```e
# Constructing an environment has to be done by way of evaluation
#for historical reasons which will hopefully be entirely eliminated soon.
def bindX(value) {
  def [resolver, env] := e`    # bind x and capture its resolver and the
    def x                      # resulting environment
  `.evalToPair(safeScope)
  resolver.resolve(value)      # set the value
  return env
}

def evalWithX(program, a, b) {
  def atA := program.eval(bindX(a))
  def atB := program.eval(bindX(b))
  return atB - atA
}
```



```e
? evalWithX(e`(x :float64).exp()`, 0, 1)
# value: 1.7182818284590455
```



## EchoLisp

We evaluate prog in a new environment which is an association list ((x x-value)), and could be ((x x-value) (y y-value)....)

```lisp

(define (eval-with-x prog x)
    (eval prog (environment-new (list (list 'x x)))))

(define prog '( + 1 (* x x)))

(eval-with-x prog 10) → 101
(eval-with-x prog 1000) → 1000001
(- (eval-with-x prog 1000) (eval-with-x prog 10)) → 999900

;; check x is unbound (no global)
x
😖️ error: #|user| : unbound variable : x

```


## Elena

Using ELENA Script engine (3.3.0):

```elena
import extensions.
import extensions'scripting.

program =
[
    var program := escript eval("^{ eval(x)[ ^" + 'program'arguments[1] + " ] }").
    
    console printLine('program'arguments[1],",",'program'arguments[2]," = ", program eval('program'arguments[2] toReal)).
].
```

```txt

eval.exe "extensions'math'mathControl power(x,2)" 2
ELENA VM 3.3.1 (C)2005-2017 by Alex Rakov
Initializing...
Debug mode...
Done...
extensions'math'mathControl power(x,2),2 = 4.0

```



## Erlang

Functions below are used by [[Dynamic_variable_names#Erlang| dynamic variable names]]. Any changes here needs to be backwards compatible, or [[Dynamic_variable_names#Erlang| dynamic variable names]] must also be changed.

```Erlang

-module( runtime_evaluation ).

-export( [evaluate_form/2, form_from_string/1, task/0] ).

evaluate_form( Form, {Variable_name, Value} ) ->
	Bindings = erl_eval:add_binding( Variable_name, Value, erl_eval:new_bindings() ),
	{value, Evaluation, _} = erl_eval:expr( Form, Bindings ),
	Evaluation.

form_from_string( String ) ->
	{ok, Tokens, _} = erl_scan:string( String ),
	{ok, [Form]} = erl_parse:parse_exprs( Tokens ),
	Form.

task() ->
	Form = form_from_string( "X." ),
	Variable1 = evaluate_form( Form, {'X', 1} ),
	io:fwrite( "~p~n", [Variable1] ),
	Variable2 = evaluate_form( Form, {'X', 2} ),
	io:fwrite( "~p~n", [Variable2] ),
	io:fwrite( "~p~n", [Variable2 - Variable1] ).

```


```txt

14> runtime_evaluation:task().
1
2
1

```



## Factor

Being a stack-based language, there is usually no need to bind data stack objects to a variable name. This is the idiomatic way to do it, with <code>eval</code> referencing what it needs from the data stack:

```factor
USE: eval
: eval-bi@- ( a b program -- n )
    tuck [ ( y -- z ) eval ] 2bi@ - ;
```


```factor
IN: scratchpad 9 4 "dup *" eval-bi@- .
65
```

Also note that, since programs are first-class denizens in Factor, the use cases for <code>eval</code> are few. Normally, you would pass in a program as a quotation:

```factor
: bi@- ( a b quot -- n ) bi@ - ; inline
```


```factor
IN: scratchpad 9 4 [ dup * ] bi@- .
65
```

However, we can adhere to the letter of the task. Although we are using a dynamic variable for x, it exists in a temporary, non-global namespace. As far as I can tell, <code>eval</code> is unaware of surrounding lexical scope.

```factor
SYMBOL: x
: eval-with-x ( a b program -- n )
    tuck
    [ [ x ] dip [ ( -- y ) eval ] curry with-variable ] 2bi@ - ;
```


```factor
IN: scratchpad 9 4 "x get dup *" eval-with-x .
65
IN: scratchpad x get .
f
```



## Forth

EVALUATE invokes the Forth interpreter on the given string.

```forth
: f-" ( a b snippet" -- )
  [char] " parse   ( code len )
  2dup 2>r evaluate
  swap 2r> evaluate
  - . ;

2 3 f-" dup *"   \ 5  (3*3 - 2*2)
```

This can be used to treat a data stream as code, or to provide a lightweight macro facility when used in an IMMEDIATE word.

```forth
: :macro ( "name <char> ccc<char>" -- )
  : [CHAR] ; PARSE  POSTPONE SLITERAL  POSTPONE EVALUATE
  POSTPONE ; IMMEDIATE
;

:macro times   0 do ;

: test  8 times ." spam " loop ;

see test
: test  
  8 0 
  DO     .\" spam " 
  LOOP
  ; ok
```



## Genyris

One way is to use a macro. In genyris, macros are lazy functions which execute twice, the return value is also evaluated in the caller's environment:

```genyris
defmacro add100() (+ x 100)

var x 23
var firstresult (add100)
x = 1000
print
    + firstresult (add100)
```


This prints 1223.

Another way is to use dynamically scoped variables. In Genyris, symbols prefixed with a period are looked up in the caller's environment, not the lexical environment of the closure.  When a dictionary is the first element of the expression, an environment is created and the &rest is evaluated. 


```genyris
def add100() (+ .x 100)

(dict)                 # create an environment capable of holding dynamic bindings
   var .x 23           # create a binding in the dictionary
   var firstresult (add100)
   .x = 1000
   print 
       + firstresult (add100)
```

Dictionaries can hold bindings to dynamic symbols. To minimize the danger of dynamic scope there is no recursive ascent in the binding lookup.

```genyris
(dict)                 
   var .x 23           
   (dict)
       print .x # fails
```


## Go


```go
package main

import (
    "bitbucket.org/binet/go-eval/pkg/eval"
    "fmt"
    "go/parser"
    "go/token"
)

func main() {
    // an expression on x
    squareExpr := "x*x"

    // parse to abstract syntax tree
    fset := token.NewFileSet()
    squareAst, err := parser.ParseExpr(squareExpr)
    if err != nil {
        fmt.Println(err)
        return
    }
    // create an environment or "world"
    w := eval.NewWorld()

    // allocate a variable
    wVar := new(intV)

    // bind the variable to the name x
    err = w.DefineVar("x", eval.IntType, wVar)
    if err != nil {
        fmt.Println(err)
        return
    }
    // bind the expression AST to the world
    squareCode, err := w.CompileExpr(fset, squareAst)
    if err != nil {
        fmt.Println(err)
        return
    }
    // directly manipulate value of variable within world
    *wVar = 5
    // evaluate
    r0, err := squareCode.Run()
    if err != nil {
        fmt.Println(err)
        return
    }
    // change value
    *wVar--
    // revaluate
    r1, err := squareCode.Run()
    if err != nil {
        fmt.Println(err)
        return
    }
    // print difference
    fmt.Println(r0.(eval.IntValue).Get(nil) - r1.(eval.IntValue).Get(nil))
}

// int value implementation.
type intV int64

func (v *intV) String() string              { return fmt.Sprint(*v) }
func (v *intV) Get(*eval.Thread) int64      { return int64(*v) }
func (v *intV) Set(_ *eval.Thread, x int64) { *v = intV(x) }
func (v *intV) Assign(t *eval.Thread, o eval.Value) {
    *v = intV(o.(eval.IntValue).Get(t))
}
```

Output:

```txt

9

```



## Groovy


The solution:

```groovy
def cruncher = { x1, x2, program ->
   Eval.x(x1, program) - Eval.x(x2, program)
}
```


Test Program:

```groovy
def fibonacciProgram = '''
x < 1 ? 0 : x == 1 ? 1 : (2..x).inject([0,1]){i, j -> [i[1], i[0]+i[1]]}[1]
'''

println "F(${10}) - F(${5}) = ${Eval.x(10, fibonacciProgram)} - ${Eval.x(5, fibonacciProgram)} = " + cruncher(10, 5, fibonacciProgram)
```


Output:

```txt
F(10) - F(5) = 55 - 5 = 50
```



## J


### Explicit

The following satisfies the requirements:

```j
   EvalWithX=. monad : 0
    'CODE V0 V1'=. y
    (". CODE [ x=. V1) - (". CODE [ x=. V0)
   )
   
   EvalWithX '^x';0;1
1.71828183
```


### Tacit

However, it is easier via point-free coding:

```j
   (0&({::) -~&>/@:(128!:2&.>) 1 2&{) '^';0;1
1.71828183
```



### Explicit again


Or, using y as the free variable, instead of x:

```J
EvalDiffWithY=: dyad define
   -~/verb def x"_1 y
)
```


Example use:


```J
   '^y' EvalDiffWithY 0 1
1.71828
```


This can be extended to support a user declared argument name:


```J
EvalDiffWithName=: adverb define
:
   -~/m adverb def ('(m)=.y';x)"_1 y
)
```


This works by preceding the user provided expression with a statement which assigns the argument value to a local variable whose name was provided by the user.  [Note that this implementation skirts the requirement that the implementation does not manipulate strings -- instead we manipulate a structure containing strings.]

Example use:


```J
  '^George' 'George' EvalDiffWithName 0 1
1.71828
   'Z + 2^Z' 'Z' EvalDiffWithName 2 3
5
```


Of course this could be re-defined such that the free variable declaration appears to the left of the expression (<code>'Z' 'Z + 2^Z' Example 2 3</code>).  However, J's [[currying]] and precedence rules might make that less convenient to use, if this were ever used in a real program.


## Java

Although Java is a compiled static language and expression evaluation is not intrinsic part of Java, we can still generate a class at run time and so emulate evaluation of string expressions. Java 1.6 provides some APIs for this sort of thing.

Issues:
* this is not thread-safe because it writes a generated class to the file system, then loads it
* the supplied code to evaluate is assumed to be an expression rather than a series of statements
* the supplied expression should evaluate to a number
* the same class is generated twice - never mind
* it's painfully verbose, but we're bending the language quite a bit
* the exception handling is minimal, but if something goes wrong you should get a stack dump and the exception ''might'' be helpful...


```java5
import java.io.File;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Arrays;
import javax.tools.JavaCompiler;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class Eval {
    private static final String CLASS_NAME = "TempPleaseDeleteMe";

    private static class StringCompiler
            extends SimpleJavaFileObject {
        final String m_sourceCode;

        private StringCompiler( final String sourceCode ) {
            super( URI.create( "string:///" + CLASS_NAME + Kind.SOURCE.extension ), Kind.SOURCE );
            m_sourceCode = sourceCode;
        }

        @Override
        public CharSequence getCharContent( final boolean ignoreEncodingErrors ) {
            return m_sourceCode;
        }

        private boolean compile() {
            final JavaCompiler javac = ToolProvider.getSystemJavaCompiler();

            return javac.getTask( null, javac.getStandardFileManager( null, null, null ),
                null, null, null, Arrays.asList( this )
            ).call();
        }

        private double callEval( final double x )
                throws Exception {
            final Class<?> clarse = Class.forName( CLASS_NAME );
            final Method   eval   = clarse.getMethod( "eval", double.class );

            return ( Double ) eval.invoke( null, x );
        }
    }

    public static double evalWithX( final String code, final double x )
            throws Exception {
        final StringCompiler sc = new StringCompiler(
            "class "
                + CLASS_NAME
                + "{public static double eval(double x){return ("
                + code
                + ");}}"
            );

        if ( ! sc.compile() ) throw new RuntimeException( "Compiler error" );
        return sc.callEval( x );
    }

    public static void main( final String [] args ) 
            throws Exception /* lazy programmer */ {
        final String expression = args [ 0 ];
        final double x1         = Double.parseDouble( args [ 1 ] );
        final double x2         = Double.parseDouble( args [ 2 ] );

        System.out.println(
            evalWithX( expression, x1 )
            - evalWithX( expression, x2 )
        );
    }
}
```


Example usage - calculating the difference of two squares (i.e. 9 - 2 = 7):

```txt
java Eval "Math.pow(x,2)" 3 1.414213562373095
```


Output: 

```txt
7.0
```



## JavaScript


eval uses the environment from the calling function.


```javascript
function evalWithX(expr, a, b) {
    var x = a;
    var atA = eval(expr);
    x = b;
    var atB = eval(expr);
    return atB - atA;
}
```



```javascript
evalWithX('Math.exp(x)', 0, 1) // returns 1.718281828459045
```



## Jsish

From Javascript entry.

```javascript
/* Runtime evaluation in an environment, in Jsish */
function evalWithX(expr, a, b) {
    var x = a;
    var atA = eval(expr);
    x = b;
    var atB = eval(expr);
    return atB - atA;
}

;evalWithX('Math.exp(x)', 0, 1);
;evalWithX('Math.exp(x)', 1, 0);

/*
=!EXPECTSTART!=
evalWithX('Math.exp(x)', 0, 1) ==> 1.71828182845905
evalWithX('Math.exp(x)', 1, 0) ==> -1.71828182845905
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u runtimeEnvironmentEvaluation.jsi
[PASS] runtimeEnvironmentEvaluation.jsi
```



## Julia

```julia
macro evalwithx(expr, a, b)
    return quote
        x = $a
        tmp = $expr
        x = $b
        return $expr - tmp
    end
end

@evalwithx(2 ^ x, 3, 5)     # raw expression (AST)
```


One can even perform the task without using macros:


```julia
function evalwithx(expr::Expr, a, b)
    a = eval(quote let x = $a; return $expr end end)
    b = eval(quote let x = $b; return $expr end end)
    return b - a
end
evalwithx(expr::AbstractString, a, b) = evalwithx(parse(expr), a, b)

evalwithx(:(2 ^ x), 3, 5)
evalwithx("2 ^ x", 3, 5)
```



## Kotlin

When you try to compile the following program, it will appear to the compiler that the local variable 'x' is assigned but never used and warnings will be issued accordingly. You can get rid of these warnings by compiling using the -nowarn flag.

```scala
// Kotlin JS version 1.1.4-3

fun evalWithX(expr: String, a: Double, b: Double) {
    var x = a
    val atA = eval(expr)
    x = b
    val atB = eval(expr)
    return atB - atA
}

fun main(args: Array<String>) {
    println(evalWithX("Math.exp(x)", 0.0, 1.0))
}
```


```txt

1.718281828459045 

```



## Liberty BASIC


```lb

 
 expression$ = "x^2 - 7"
Print (EvaluateWithX(expression$, 5) - EvaluateWithX(expression$, 3))
End

Function EvaluateWithX(expression$, x)
    EvaluateWithX = Eval(expression$)
End Function 
```



## Lua


```lua

code = loadstring"return x^2" --this doesn't really need to be input, does it?
val1 = setfenv(code, {x = io.read() + 0})()
val2 = setfenv(code, {x = io.read() + 0})()
print(val2 - val1)

```


In Lua 5.2 one can use the new <code>load</code> function to evaluate a string as Lua code and specify its environment:

```lua
env = {}
f = load("return x", nil, nil, env)
env.x = tonumber(io.read()) -- user enters 2
a = f()
env.x = tonumber(io.read()) -- user enters 3
b = f()
print(a + b) --> outputs 5
```



## Mathematica


```Mathematica
Input source code is "10 x" , X is locally bound to 3 & 2 and the resulting expressions evaluated.
(10 x /. x -> 3 ) - (10 x /. x -> 2 )
-> 10
```


=={{header|MATLAB}} / {{header|Octave}}==

In Octave, undeclared variables are local.


```octave
function r = calcit(f, val1, val2)
  x = val1;
  a = eval(f);
  x = val2;
  b = eval(f);
  r = b-a;
end
```

Usage:

```txt
p = 'x .* 2';
disp(calcit(p, [1:3], [4:6]));
```


Output:

```txt
6   6   6
```



## Metafont



```metafont
vardef evalit(expr s, va, vb) =
save x,a,b; x := va; a := scantokens s;
x := vb; b := scantokens s; a-b
enddef;

show(evalit("2x+1", 5, 3));
end
```



## ooRexx

The ooRexx interpret instruction executes dynamically created ooRexx code in the current variable context.

```ooRexx

say evalWithX("x**2", 2)
say evalWithX("x**2", 3.1415926)

::routine evalWithX
  use arg expression, x

  -- X now has the value of the second argument
  interpret "return" expression

```

Output:

```txt

4
9.86960406

```



## Oz


```oz
declare
   fun {EvalWithX Program A B}
      {Compiler.evalExpression Program env('X':B) _}
      -
      {Compiler.evalExpression Program env('X':A) _}   
   end
in
   {Show {EvalWithX "{Exp X}" 0.0 1.0}}
```



## PARI/GP

There are many ways of doing this depending on the particular interpretation of the requirements.  This code assumes that <var>f</var> is a string representing a GP closure.

```parigp
test(f,a,b)=f=eval(f);f(a)-f(b);
test("x->print(x);x^2-sin(x)",1,3)
```



## Perl



```perl
sub eval_with_x
   {my $code = shift;
    my $x = shift;
    my $first = eval $code;
    $x = shift;
    return eval($code) - $first;}
 
print eval_with_x('3 * $x', 5, 10), "\n"; # Prints "15".
```



## Perl 6

For security, you must explicitly allow use of 'EVAL'.

```perl6
use MONKEY-SEE-NO-EVAL;
sub eval_with_x($code, *@x) { [R-] @x.map: -> \x { EVAL $code } }

say eval_with_x('3 * x', 5, 10);      # Says "15".
say eval_with_x('3 * x', 5, 10, 50);  # Says "105".
```



## PHP



```php
<?php
function eval_with_x($code, $a, $b) {
    $x = $a;
    $first = eval($code);
    $x = $b;
    $second = eval($code);
    return $second - $first;
}
 
echo eval_with_x('return 3 * $x;', 5, 10), "\n"; # Prints "15".
?>
```



## PicoLisp


```PicoLisp
(let Expression '(+ X (* X X))            # Local expression
   (println
      (+
         (let X 3
            (eval Expression) )
         (let X 4
            (eval Expression) ) ) )
   (let Function (list '(X) Expression)   # Build a local function
      (println
         (+
            (Function 3)
            (Function 4) ) ) ) )
```

Output:

```txt
32
32
```



## Pike

Pike can only compile complete classes. therefore binding a value to a variable is only possible by string manipulation. even Pikes own interactive mode which seemingly evaluates expressions wraps them into a class and replaces variable references before compiling:

```Pike>
 int x=10;
Result: 10
> x * 5;      
Result: 50
> dump wrapper
Last compiled wrapper:
001: mapping(string:mixed) ___hilfe = ___Hilfe->variables;
002: # 1
003: mixed ___HilfeWrapper() { return (([mapping(string:int)](mixed)___hilfe)->x) * 5; ; } 
004: 
>
```

___Hilfe is an object which stores all created variables;

to solve the problem in the task i would create a function that can take arguments:

```Pike
string payload = "x * 5";

program demo = compile_string("string eval(mixed x){ " + payload + "; }");
 
demo()->eval(10);
Result: 50
demo()->eval(20);
Result: 100
```



## Python



```python>>>
 def eval_with_x(code, a, b):
	return eval(code, {'x':b}) - eval(code, {'x':a})

>>> eval_with_x('2 ** x', 3, 5)
24
```


A slight change allows the evaluation to take multiple names:

```python>>>
 def eval_with_args(code, **kwordargs):
	return eval(code, kwordargs)

>>> code = '2 ** x'
>>> eval_with_args(code, x=5) - eval_with_args(code, x=3)
24
>>> code = '3 * x + y'
>>> eval_with_args(code, x=5, y=2) - eval_with_args(code, x=3, y=1)
7
```



## R

We can set up thing so that the "unbound" variable can be any accepted symbol for variables.

```R
evalWithAB <- function(expr, var, a, b) {
  env <- new.env()           # provide a separate env, so that the choosen
  assign(var, a, envir=env)  # var name do not collide with symbols inside
                             # this function (e.g. it could be even "env")
  atA <- eval(parse(text=expr), env)
                             # and then evaluate the expression inside this
                             # ad hoc env-ironment
  assign(var, b, envir=env)
  atB <- eval(parse(text=expr), env)
  return(atB - atA)
}

print(evalWithAB("2*x+1", "x", 5, 3))
print(evalWithAB("2*y+1", "y", 5, 3))
print(evalWithAB("2*y+1", "x", 5, 3)) # error: object "y" not found
```



## Racket


Same hack as the on in the CL/Scheme entries:

```Racket

#lang racket
(define ns (make-base-namespace))
(define (eval-with-x code a b)
  (define (with v) (eval `(let ([x ',v]) ,code) ns))
  (- (with b) (with a)))

```


Better: a more direct use of eval with just the code (for example, this
won't break if we use a namespace with a different meaning for
<tt>let</tt>, which is very possible in Racket):

```Racket

#lang racket
(define ns (make-base-namespace))
(define (eval-with-x code a b)
  (define (with v)
    (namespace-set-variable-value! 'x v #f ns)
    (eval code ns))
  (- (with b) (with a)))

```



## REBOL


```rebol
prog: [x * 2]
fn: func [x] [do bind prog 'x]
a: fn 2
b: fn 4
subtract b a
```


Result:

```txt

4

```



## REXX


```rexx
/*REXX program to demonstrate some run-time evaulations.                */

a=fact(3)
b=fact(4)
say b-a
exit                                   /*stick a fork in it, we're done.*/

/*───────────────────────────────────FACT subroutine────────────────────*/
fact: procedure; parse arg n; !=1;   do j=2  to n;  !=!*j;  end;  return !
```

'''output'''

```txt

18

```



## Ring


```ring

expression = "return pow(x,2) - 7"
one = evalwithx(expression, 1.2)
two = evalwithx(expression, 3.4)
see "one = " + one + nl + "two = " + two + nl
 
func evalwithx expr, x
     return eval(expr)

```

Output:

```txt

one = -5.56
two = 4.56

```



## Ruby


```ruby
def bind_x_to_value(x)
  binding
end

def eval_with_x(code, a, b)
  eval(code, bind_x_to_value(b)) - eval(code, bind_x_to_value(a))
end

puts eval_with_x('2 ** x', 3, 5) # Prints "24"
```


The magic here is how the <code>binding</code> method works with the <code>bind_x_to_value(x)</code> method.
When <code>bind_x_to_value</code> is called, it sets its local variable <code>x</code> to the value passed.
The <code>binding</code> method then returns a reference to the current context (or stack frame) to the caller.
<code>eval</code> can then use the local variable <code>x</code> in this context.


## Scala


```Scala
object Eval extends App {

  def evalWithX(expr: String, a: Double, b: Double)=
    {val x = b; eval(expr)} - {val x = a; eval(expr)}

  println(evalWithX("Math.exp(x)", 0.0, 1.0))

}
```


## Scheme

Almost identical to the [[Common Lisp]] version above.

```scheme
(define (eval-with-x prog a b)
  (let ((at-a (eval `(let ((x ',a)) ,prog)))
        (at-b (eval `(let ((x ',b)) ,prog))))
    (- at-b at-a)))
```



## Sidef


```ruby
func eval_with_x(code, x, y) {
    var f = eval(code);
    x = y;
    eval(code) - f;
}

say eval_with_x(x: 3, y: 5, code: '2 ** x');   # => 24
```


## Simula


```simula
BEGIN

    CLASS ENV;
    BEGIN

        CLASS ITEM(N, X); TEXT N; REAL X;
        BEGIN
            REF(ITEM) NEXT; NEXT :- HEAD; HEAD :- THIS ITEM;
        END ITEM;

        REF(ITEM) HEAD;

        REF(ITEM) PROCEDURE LOOKUP(V); TEXT V;
        BEGIN
            REF(ITEM) I; BOOLEAN FOUND; I :- HEAD;
            WHILE NOT FOUND DO
                IF I == NONE OR ELSE I.N = V
                THEN FOUND := TRUE
                ELSE I :- I.NEXT;
            LOOKUP :- I;
        END LOOKUP;

        REF(ENV) PROCEDURE SET(V, X); TEXT V; REAL X;
        BEGIN
            REF(ITEM) I; I :- LOOKUP(V);
            IF I == NONE THEN I :- NEW ITEM(V, X) ELSE I.X := X;
            SET :- THIS ENV;
        END SET;

        REAL PROCEDURE GET(V); TEXT V;
            GET := LOOKUP(V).X;

    END ENV;

    CLASS EXPR(EV); REF(ENV) EV;
    BEGIN


        REAL PROCEDURE POP;
        BEGIN
            IF STACKPOS > 0 THEN
            BEGIN STACKPOS := STACKPOS - 1; POP := STACK(STACKPOS); END;
        END POP;


        PROCEDURE PUSH(NEWTOP); REAL NEWTOP;
        BEGIN
            STACK(STACKPOS) := NEWTOP;
            STACKPOS := STACKPOS + 1;
        END PUSH;


        REAL PROCEDURE CALC(OPERATOR, ERR); CHARACTER OPERATOR; LABEL ERR;
        BEGIN
            REAL X, Y; X := POP; Y := POP;
            IF      OPERATOR = '+' THEN PUSH(Y + X)
            ELSE IF OPERATOR = '-' THEN PUSH(Y - X)
            ELSE IF OPERATOR = '*' THEN PUSH(Y * X)
            ELSE IF OPERATOR = '/' THEN BEGIN
                                            IF X = 0 THEN
                                            BEGIN
                                                EVALUATEDERR :- "DIV BY ZERO";
                                                GOTO ERR;
                                            END;
                                            PUSH(Y / X);
                                        END
            ELSE IF OPERATOR = '^' THEN PUSH(Y ** X)
            ELSE
            BEGIN
                EVALUATEDERR :- "UNKNOWN OPERATOR";
                GOTO ERR;
            END
        END CALC;


        PROCEDURE READCHAR(CH); NAME CH; CHARACTER CH;
        BEGIN
            IF T.MORE THEN CH := T.GETCHAR ELSE CH := EOT;
        END READCHAR;


        PROCEDURE SKIPWHITESPACE(CH); NAME CH; CHARACTER CH;
        BEGIN
            WHILE (CH = SPACE) OR (CH = TAB) OR (CH = CR) OR (CH = LF) DO
                READCHAR(CH);
        END SKIPWHITESPACE;


        PROCEDURE BUSYBOX(OP, ERR); INTEGER OP; LABEL ERR;
        BEGIN
            CHARACTER OPERATOR;
            REAL NUMBR;
            BOOLEAN NEGATIVE;

            SKIPWHITESPACE(CH);

            IF OP = EXPRESSION THEN
            BEGIN

                NEGATIVE := FALSE;
                WHILE (CH = '+') OR (CH = '-') DO
                BEGIN
                    IF CH = '-' THEN NEGATIVE :=  NOT NEGATIVE;
                    READCHAR(CH);
                END;

                BUSYBOX(TERM, ERR);

                IF NEGATIVE THEN
                BEGIN
                    NUMBR := POP; PUSH(0 - NUMBR);
                END;

                WHILE (CH = '+') OR (CH = '-') DO
                BEGIN
                    OPERATOR := CH; READCHAR(CH);
                    BUSYBOX(TERM, ERR); CALC(OPERATOR, ERR);
                END;

            END
            ELSE IF OP = TERM THEN
            BEGIN

                BUSYBOX(FACTOR, ERR);
                WHILE (CH = '*') OR (CH = '/') DO
                BEGIN
                    OPERATOR := CH; READCHAR(CH);
                    BUSYBOX(FACTOR, ERR); CALC(OPERATOR, ERR)
                END

            END
            ELSE IF OP = FACTOR THEN
            BEGIN

                BUSYBOX(POWER, ERR);
                WHILE CH = '^' DO
                BEGIN
                    OPERATOR := CH; READCHAR(CH);
                    BUSYBOX(POWER, ERR); CALC(OPERATOR, ERR)
                END

            END
            ELSE IF OP = POWER THEN
            BEGIN

                IF (CH = '+') OR (CH = '-') THEN
                    BUSYBOX(EXPRESSION, ERR)
                ELSE IF (CH >= '0') AND (CH <= '9') THEN
                    BUSYBOX(NUMBER, ERR)
                ELSE IF (CH >= 'A') AND (CH <= 'Z') THEN
                    BUSYBOX(VARIABLE, ERR)
                ELSE IF CH = '(' THEN
                BEGIN
                    READCHAR(CH);
                    BUSYBOX(EXPRESSION, ERR);
                    IF CH = ')' THEN READCHAR(CH) ELSE GOTO ERR;
                END
                ELSE GOTO ERR;

            END
            ELSE IF OP = VARIABLE THEN
            BEGIN

                TEXT VARNAM;
                VARNAM :- BLANKS(32);
                WHILE (CH >= 'A') AND (CH <= 'Z')
                   OR (CH >= '0') AND (CH <= '9') DO
                BEGIN
                    VARNAM.PUTCHAR(CH);
                    READCHAR(CH);
                END;
                PUSH(EV.GET(VARNAM.STRIP));

            END
            ELSE IF OP = NUMBER THEN
            BEGIN

                NUMBR := 0;
                WHILE (CH >= '0') AND (CH <= '9') DO
                BEGIN
                    NUMBR := 10 * NUMBR + RANK(CH) - RANK('0'); READCHAR(CH);
                END;
                IF CH = '.' THEN
                BEGIN
                    REAL FAKTOR;
                    READCHAR(CH);
                    FAKTOR := 10;
                    WHILE (CH >= '0') AND (CH <= '9') DO
                    BEGIN
                        NUMBR := NUMBR + (RANK(CH) - RANK('0')) / FAKTOR;
                        FAKTOR := 10 * FAKTOR;
                        READCHAR(CH);
                    END;
                END;
                PUSH(NUMBR);

            END;

            SKIPWHITESPACE(CH);

        END BUSYBOX;


        BOOLEAN PROCEDURE EVAL(INP); TEXT INP;
        BEGIN
            EVALUATEDERR :- NOTEXT;
            STACKPOS := 0;
            T :- COPY(INP.STRIP);
            READCHAR(CH);
            BUSYBOX(EXPRESSION, ERRORLABEL);
            IF NOT T.MORE AND STACKPOS = 1 AND CH = EOT THEN
            BEGIN
                EVALUATED := POP;
                EVAL := TRUE;
                GOTO NOERRORLABEL;
            END;
    ERRORLABEL:
            EVAL := FALSE;
            IF EVALUATEDERR = NOTEXT THEN
                EVALUATEDERR :- "INVALID EXPRESSION: " & INP;
    NOERRORLABEL:
        END EVAL;

        
        REAL PROCEDURE RESULT;
            RESULT := EVALUATED;

        TEXT PROCEDURE ERR;
            ERR :- EVALUATEDERR;

        INTEGER EXPRESSION, TERM, FACTOR, POWER, NUMBER, VARIABLE;
        CHARACTER TAB, LF, CR, SPACE, EOT, CH;
        REAL ARRAY STACK(0:31);
        INTEGER STACKPOS;
        REAL EVALUATED;
        TEXT EVALUATEDERR, T;

        EXPRESSION := 1;
        TERM := 2;
        FACTOR := 3;
        POWER := 4;
        NUMBER := 5;
        VARIABLE := 6;

        TAB := CHAR(9);
        LF := CHAR(10);
        CR := CHAR(13);
        SPACE := CHAR(32);
        EOT := CHAR(0);

    END EXPR;

    REF(EXPR) EXA, EXB;
    EXA :- NEW EXPR(NEW ENV.SET("X", 3));
    EXB :- NEW EXPR(NEW ENV.SET("X", 5));
    IF EXA.EVAL("2 ^ X") THEN
    BEGIN
        IF EXB.EVAL("2 ^ X")
        THEN OUTFIX(EXB.RESULT - EXA.RESULT, 3, 10)
        ELSE OUTTEXT(EXB.ERR)
    END ELSE OUTTEXT(EXA.ERR);
    OUTIMAGE;

END.

```

```txt

    24.000

```



## SNOBOL4

This program defines (at runtime) a function triple(), compiles it, and then executes it twice, with values x = 1 and then x = 3.  The program subtracts the returned value from the first call from the value returned from the first call, and prints the result.  In this example, the value x is passed as a parameter to the function triple().


```SNOBOL4
     compiled = code(' define("triple(x)") :(a);triple triple = 3 * x :(return)')  :<compiled>
a    x = 1
     first = triple(x)
     x = 3
     output = triple(x) - first  
end
```


Output:

```txt
6
```


If you specifically wanted to not pass x as a parameter but instead use it as a value from the environment, that's easy too:


```SNOBOL4
     compiled = code(' define("triple()") :(a);triple triple = 3 * x :(return)')  :<compiled>
a    x = 1
     first = triple(x)
     x = 3
     output = triple(x) - first  
end
```


The output is the same.


## Tcl


```tcl
proc eval_twice {func a b} {
    set x $a
    set 1st [expr $func]
    set x $b
    set 2nd [expr $func]
    expr {$2nd - $1st}
}

puts [eval_twice {2 ** $x} 3 5] ;# ==> 24
```


Here's another take, similar to other answers.  It passes a code block to be <code>eval</code>ed, not just an expression for  <code>expr</code>

```tcl
proc eval_with_x {code val1 val2} {
    expr {[set x $val2; eval $code] - [set x $val1; eval $code]}
}
eval_with_x {expr {2**$x}} 3 5 ;# ==> 24
```


In 8.5, <tt>apply</tt> makes environments like this "first class":

```tcl
package require Tcl 8.5
proc eval_with {body a b} {
    set lambda [list x $body]
    expr {[apply $lambda $b] - [apply $lambda $a]}
}

eval_with {expr {2**$x}} 3 5  ;# ==> 24
```



## TXR


In TXR's embedded Lisp dialect, we can implement the same solution as Lisp or Scheme: transform the code fragment by wrapping a <code>let</code> around it which binds a variable, and then evaluating the whole thing:


```txrlisp
(defun eval-subtract-for-two-values-of-x (code-fragment x1 x0)
  (- (eval ^(let ((x ,x1)) ,code-fragment))
     (eval ^(let ((x ,x0)) ,code-fragment))))

(eval-subtract-for-two-values-of-x 1 2) ;; yields -4.67077427047161
```


Cutting edge TXR code provides access to the environment manipulation functions, making this possible:


```txrlisp
(defun eval-subtract-for-two-values-of-x (code-fragment x1 x0)
  (let ((e1 (make-env (list (cons 'x x1))))   ;; create two environments stuffed with binding for x
        (e0 (make-env (list (cons 'x x0)))))
    (- (eval code-fragment e1)                ;; pass these environment to eval
       (eval code-fragment e0))))
 
(eval-subtract-for-two-values-of-x '(exp x) 1 2)
```


Alternatively, empty environments can be made and extended with bindings:


```txrlisp
(defun eval-subtract-for-two-values-of-x (code-fragment x1 x0)
  (let ((e1 (make-env))
        (e0 (make-env)))
    (env-vbind e1 'x x1)
    (env-vbind e0 'x x0)
    (- (eval code-fragment e1)
       (eval code-fragment e0))))
 
(eval-subtract-for-two-values-of-x '(exp x) 1 2)
```


Explicit environment manipulation has the disadvantage of being hostile against compiling. (See notes about compilation in the Common Lisp example.)

there is an <code>eval</code> function which takes an environment parameter. However, currently there isn't any access to the manipulation of environment objects. It's probably a bad idea because run time tricks with lexical environments lead to programs that are not compilable.

Lastly, we can also solve this problem using dynamically scoped (a.k.a "special") variables. The problem description specifically says that the solution is not to use global variables. Though we must define the variables as global, we do not use the global bindings; we use dynamic bindings.

There is a hidden global variable, namely the dynamic environment itself. That's how <code>eval</code> is able to resolve the free-variable <code>x</code> occurring in <code>code-fragment</code> without receiving any environment parameter.

However, our two <code>let</code> constructs carefully save and restore the dynamic environment (and therefore any prior value of <code>x</code>), even in the face of exceptions, and 


```txrlisp
(defvar x)

(defun eval-subtract-for-two-values-of-x (code-fragment x1 x0)
  (- (let ((x x1)) (eval code-fragment))
     (let ((x x0)) (eval code-fragment))))

(eval-subtract-for-two-values-of-x '(exp x) 1 2)
```


=={{header|TI-89 BASIC}}==


```ti89b
evalx(prog, a, b)
Func
  Local x,eresult1,eresult2
  a→x
  expr(prog)→eresult1
  b→x
  expr(prog)→eresult2
  Return eresult2-eresult1
EndFunc

■ evalx("ℯ^x", 0., 1)
                      1.71828
```


There are no facilities for control over the environment; expr() evaluates in the same environment as the caller, including local variables. [Someone please verify this statement.] [[Category:TI-89 BASIC examples needing attention]]


## UNIX Shell

The backquotes <tt>` ... `</tt> capture the standard output of a subshell. Changes to parameter ''x'' in the subshell will not affect its parent shell.


```bash
eval_with_x() {
	set -- "`x=$2; eval "$1"`" "`x=$3; eval "$1"`"
	expr "$2" - "$1"
}

eval_with_x '
	# compute 2 ** $x
	p=1
	while test $x -gt 0; do
		p=`expr $p \* 2`
		x=`expr $x - 1`
	done
	echo $p
' 3 5
# Prints '24'
```



## zkl


```zkl
fcn evalWithX(text,x) {
   f:=Compiler.Compiler.compileText(text);
   f.x = x; // set free var in compiled blob
   f.__constructor(); // run blob
   vm.regX   // compiler sets the VMs X register for cases like this
}
const TEXT="var x; x*2"; // variables need to be declared
evalWithX(TEXT,5) - evalWithX(TEXT,3) #--> 4
```

This is not a complete solution but close.

Another way to do this is to create a class on the fly that contains the code to be run and reusing that class. The class just acts like as a container for x and a function:

```zkl
var klass=Compiler.Compiler.compileText("var x; returnClass(x*2)");
(klass.__constructor(klass.x=5) - klass.__constructor(klass.x=3)).println();
```

returnClass(x) is required in a constructor if you want to return something other than self. klass.x=y pokes y into the instance variable x. Running the constructor runs x*2.
```txt
4
```


{{omit from|C}} <!-- static compiled -->
{{omit from|Haskell}} <!-- no "eval" in language spec -->
