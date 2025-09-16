+++
title = "Runtime evaluation"
description = ""
date = 2019-06-21T16:55:43Z
aliases = []
[extra]
id = 3557
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "autohotkey",
  "basic",
  "bbc_basic",
  "burlesque",
  "common_lisp",
  "e",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "frink",
  "go",
  "groovy",
  "harbour",
  "hicest",
  "j",
  "java",
  "javascript",
  "jsish",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "matlab",
  "maxima",
  "oforth",
  "oorexx",
  "oxygenbasic",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "php",
  "picolisp",
  "pike",
  "powershell",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "scheme",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "tcl",
  "unix_shell",
  "ursa",
  "zkl",
]
+++

## Task

Demonstrate a language's ability for programs to execute code written in the language provided at runtime. 

Show what kind of program fragments are permitted (e.g. expressions vs. statements), and how to get values in and out (e.g. environments, arguments, return values), if applicable what lexical/static environment the program is evaluated in, and what facilities for restricting (e.g. sandboxes, resource limits) or customizing (e.g. debugging facilities) the execution.

You may not invoke a separate evaluator program, or invoke a compiler and then its output, unless the interface of that program, and the syntax and means of executing it, are considered part of your language/library/platform.

For a more constrained task giving a specific program fragment to evaluate, see [[Eval in environment]].





## ALGOL 68


<!-- {{does not work with|ALGOL 68|Standard - variable names are not visible at run time with classic compilers.}} -->
<!-- {{does not work with|ELLA ALGOL 68|Any This implementation is a compiler}} -->
Variable names are generally not visible at run time with classic compilers.  However '''ALGOL 68G''' is an interpretor and it retains this ability.

```algol68
print(evaluate("4.0*arctan(1.0)"))
```

```txt

+3.14159265358979e  +0

```


This example demonstrates the use of variables and that the Algol 68G evaluate uses the normal Algol 68 scoping rules:

```algol68
# procedure to call the Algol 68G evaluate procedure                    #
# the environment of the evaluation will be the caller's environment    #
# with "code", "x" and "y" defined as the procedure parameters          #
PROC ev = ( STRING code, INT x, INT y )STRING: evaluate( code );

BEGIN

    INT  i := 1;
    INT  j := 2;
    REAL x := 4.2;
    REAL y := 0.7164;

    # evaluates "i + j" in the current environment                       #
    print( ( evaluate( "i + j" ), newline ) );

    # evaluates "x + y" in the environment of the procedure body of ev   #
    print( ( ev( "x + y", i, j ), newline ) );

    # evaluates "x + y" in the current environment, so shows a different #
    # result to the previous call                                        #
    print( ( evaluate( "x + y" ), newline ) );

    # prints "code" because code is defined in the environment of the    #
    # call to evaluate (in ev) although it is not defined in this        #
    # environment                                                        #
    print( ( ev( "code", 1, 2 ), newline ) );

    # prints "code + codecode + code" - see above                        #
    print( ( ev( "code + code", 1, 2 ), newline ) )

END

# if this next call was executed, a runtime error would occur as x and y #
# do not exist anymore                                                   #
# ;print( ( evaluate( "x + y" ), newline ) ) #
```


```txt

         +3
         +3
+4.91640000000000e  +0
code
code + codecode + code

```



## AutoHotkey

function [http://www.autohotkey.net/~HotKeyIt/AutoHotkey/addScript.htm addScript] can be used to dynamically add lines of code to a running script. 

```AutoHotkey
; requires AutoHotkey_H or AutoHotkey.dll
msgbox % eval("3 + 4")
msgbox % eval("4 + 4")
return


eval(expression)
{
global script
script = 
(
    expression(){
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



## BASIC

###  Evaluating expressions 

VAL() function converts string into numeric value.
On many Basic implementations, VAL only accepts simple numeric values.
However, Sinclair Basic and its derivates such as Beta Basic and SAM Basic accept any expression that evaluates to numeric value.

The following example shows a functon that plots graph of any function f(x). The function is passed in string parameter f$.

```txt

100 DEF PROC graph f$
110   LOCAL x,y
120   PLOT 0,90
130   FOR x = -2 TO 2 STEP 0.02
140     LET y = VAL(f$)
150     DRAW TO x*50+100, y*50+90
160   NEXT x
170 END PROC

```

Usage example:

```txt

500 graph "SIN(x) + SIN(x*3)/3"

```



###  Executing code 

The KEYIN statement available on Beta Basic and SAM Basic executes a string as if it had been entered from keyboard in command mode.
It can execute commands directly, or add (or replace) lines in the program while the program is executing. This allows creating self-modifying programs.

The function do_with_x in the following example loops variable x from 1 to 10 and within the loop executes any code passed to function in parameter p$.

```txt

100 DEF PROC do_with_x p$
110   LOCAL x
130   FOR x = 1 TO 10
140     KEYIN p$
160   NEXT x
170 END PROC

```


The usage example below creates a multiplication table by executing inner loop for y:

```txt

500 LET y$ = "FOR y=1 TO 10: PRINT AT y, x*3; x*y: NEXT y"
510 do_with_x y$

```

VAL and KEYIN execute code in the environment they are called from. In the above examples, VAL and KEYIN both see the local variable x.
There is no sandbox functionality in Bata BASIC or SAM BASIC.

==={{works with|ZX Spectrum Basic}} ===

In ZX Spectrum Basic, loading a new program will replace the existing program. The new program will sutomatically run, if it was saved to do so by using SAVE together with LINE:


```zxbasic

10 REM load the next program
20 LOAD "PROG2"

```


You can also include code in a text string as follows:

```zxbasic
10 LET f$=CHR$ 187+"(x)+"+CHR$ 178+"(x*3)/2": REM LET f$="SQR (x)+SIN (x*3)/2"
20 FOR x=0 TO 2 STEP 0.2
30 LET y=VAL f$
40 PRINT y
50 NEXT x

```


CHR$ 178 is the token of function SQR, and CHR$ 178 is the token of function SIN.

In 48 k mode, you can also write this:

```zxbasic
10 LET f= SQR (x)+SIN (x*3)/2
```

Then the type of the variable is changed and the formula is enclosed in quotation marks:

```zxbasic
10 LET f$=" SQR (x)+SIN (x*3)/2"
```



## BBC BASIC

### Expressions

Expressions can be evaluated using the EVAL function:

```bbcbasic
      expr$ = "PI^2 + 1"
      PRINT EVAL(expr$)
```

```txt

10.8696044

```



### Statements

Statements can be executed by being tokenised and then written to a temporary file:

```bbcbasic
      exec$ = "PRINT ""Hello world!"""
      bbc$ = FNtokenise(exec$)
      
      tmpfile$ = @tmp$+"temp.bbc"
      tmpfile% = OPENOUT(tmpfile$)
      BPUT#tmpfile%, bbc$+CHR$0
      CLOSE #tmpfile%
      
      CALL tmpfile$
      END
      
      DEF FNtokenise(A$)
      LOCAL A%
      A% = EVAL("0:"+A$)
      A$ = $(!332+2)
      = CHR$(LENA$+4) + CHR$0 + CHR$0 + A$ + CHR$13

```

```txt

Hello world!

```



## Burlesque


In Burlesque "Code" is actually just a list of identifiers. It is therefore possible to create and manipulate code at runtime.
Evaluating a block:


```burlesque

blsq ) {5 5 .+}e!
10

```


Creating code at runtime (changing map (+5) to map (+6) at runtime):


```burlesque

blsq ) 1 10r@{5.+}m[
{6 7 8 9 10 11 12 13 14 15}
blsq ) 1 10r@{5.+}6 0sam[
{7 8 9 10 11 12 13 14 15 16}

```


Code from string at runtime:


```burlesque

blsq ) 1 10r@"5.+"psm[
{6 7 8 9 10 11 12 13 14 15}

```


Evaluating strings at runtime (reverse is just for demonstration):


```burlesque

blsq ) "[m}+.5{@r01 1"<-pe
{6 7 8 9 10 11 12 13 14 15}

```


Injecting other functions into code:


```burlesque

blsq ) {3 2}(?*)[+e!
6

```


Identifiers not contained in a block require to be quoted to push them to the stack. Note the difference:


```burlesque

blsq ) ?+
ERROR: Burlesque: (.+) Invalid arguments!
blsq ) ?+to
"Error"
blsq ) (?+)
?+
blsq ) (?+)to
"Ident"

```


(also note the fallback to ''.+'' from ''?+'').

=={{header|Caché ObjectScript}}==

The 'XECUTE' command performs substantially the same operation as the '$XECUTE' function, except the latter must specify a return value.
```txt

USER>Set cmd="Write ""Hello, World!"""
USER>Xecute cmd
Hello, World!

USER>Set fnc="(num1, num2) Set res=num1+num2 Quit res"
USER>Write $Xecute(fnc, 2, 3)
5

```



## Common Lisp



### Brief <code>eval</code> tutorial

The <tt>eval</tt> function evaluates Lisp code at runtime.


```lisp
(eval '(+ 4 5)) ; returns 9
```


In Common Lisp, programs are represented as trees (s-expressions). Therefore, it is easily possible to construct a program which includes externally specified values, particularly using backquote template syntax:


```lisp
(defun add-four-complicated (a-number)
  (eval `(+ 4 ',a-number)))
```


Or you can construct a function and then call it. (If the function is used more than once, it would be good to use <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_cmp.htm compile]</code> instead of <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm eval]</code>, which compiles the code before returning the function. <code>eval</code> is permitted to compile as well, but <code>compile</code> requires it.)


```lisp
(defun add-four-by-function (a-number)
  (funcall (eval '(lambda (n) (+ 4 n)))) a-number)
```


If your program came from a file or user input, then you have it as a string, and [http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm <code>read</code>] or <code>read-from-string</code> will convert it to s-expression form:


```lisp
(eval (read-from-string "(+ 4 5)"))
```


Common Lisp has lexical scope, but <code>eval</code> always evaluates “in the null lexical environment”. In particular, <code>eval</code> does not inherit the lexical variables from the enclosing code. (Note that <code>eval</code> is an ordinary function and as such does not have access to that environment anyway.)


```lisp
(let ((x 11) (y 22))
  ;; This is an error!  Inside the eval, x and y are unbound!
  (format t "~%x + y = ~a" (eval '(+ x y))))
```


One way to fix the error is to <tt>(declare (special x y))</tt> for dynamic variables; but the easier and shorter way is to insert the values of x and y with the backquote template syntax.


```lisp
(let ((x 11) (y 22))
  (format t "~%x + y = ~a" (eval `(+ ,x ,y))))
```



### Sandboxing Discussion


Sandboxing in Common Lisp can be approached in a variety of ways, none of which are standardized.

One approach to define a sublanguage and validate expressions before passing them to <code>compile</code> or <code>eval</code>. Of course, a whole different language entirely can be defined, and translated to Lisp. This is essentially the classic "trusted compiler generating safe code in an untrusted target language" approach.

One way to simplify the validator is to use the package system to create a sandbox. This is done by defining a package arbitrarily called <code>sandbox</code>. The validator then simply has to make sure that all symbols used in the expression are restricted to those which are visible inside the <code>sandbox</code> package. Inside <code>sandbox</code>, we include only those functions, operators, variables and other symbols from system packages that are safe: materials which don't allow sandboxed code to do anything harmful from within the sandbox, or to escape from the sandbox. For instance, suppose that some package <code>system</code> has a function called <code>run-shell-command</code>. We do not import <code>run-shell-command</code> into the <code>sandbox</code> package, and our validator will reject code which has references such as 
```lisp
(system:run-shell-command ...)
```
. Therefore, the sandboxed code has no direct way to run that function. To gain access to it, it must exploit some flaw in the sandbox. One flaw in the sandbox would be the inclusion of certain package-related functions like <code>find-symbol</code>. The expression 
```lisp
(find-symbol "FOO" "BAR")
```
 will retrieve symbol <code>foo::bar</code> if it exists. The validator will not find this code because it has no embedded symbolic references to package <code>foo</code>; they are disguised as character string. A cautious approach to the sandbox should be taken: include less rather than more, and consider each expansion of the sandbox with meticulous care.


### Debugging Notes

There are no standardized debugging facilities specific to the <code>eval</code> operation itself, but code evaluted may be affected by the current [http://www.lispworks.com/documentation/HyperSpec/Body/03_c.htm global declarations], particularly the [http://www.lispworks.com/documentation/HyperSpec/Body/d_optimi.htm <code>optimize</code> declaration]'s <code>debug</code> and <code>safety</code> qualities.

=={{header|Déjà Vu}}==

The compiler, module system and interactive interpreter are all implemented in Déjà Vu itself, and the first two are part of the standard library.

Each compiled fragment is considered to be a single "file", and cannot access any local variables from outside of itself.


```dejavu
!run-blob !compile-string "(fake filename)" "!print \qHello world\q"
```

```txt
Hello world
```



## E


In E, eval is a method of expression ASTs (EExprs). (Other types of program fragment ASTs such as methods and patterns may not be directly evaluated, and must be inserted into an expression.)

The lexical environment is provided as a parameter and cannot be omitted. The evaluated program has no access to anything but the provided environment.


```e
? e`1 + 1`.eval(safeScope)
# value: 2
```


<code>eval</code> returns the value of the expression. <code>evalToPair</code> also returns the modified environment for use with further evaluation, e.g. for implementing a [[REPL]].


```e
? def [value, env] := e`def x := 1 + 1`.evalToPair(safeScope)
# value: [2, ...]

? e`x`.eval(env)
# value: 2
```


Eval from a string may be done by invoking the parser.


```e
? def prog := <elang:syntax.makeEParser>
.run("1 + 1")
# value: e`1.add(1)`

? prog.eval(safeScope)
# value: 2
```



## EchoLisp

'''eval''' : The evaluation of the eval argument must give a symbolic expression, which is in turn evaluated. Alternatively, '''read-from-string''' produces a s-expression - any kind of program - from a string.

```scheme

(eval (list * 6 7))
   → 42
(eval '(* 6 7)) ;; quoted argument
   → 42
(eval (read-from-string "(* 6 7)"))
   → 42

```


## Elena

Using ELENA Script engine and VM client mode (3.3.0):

```elena
import extensions'scripting.

program =
[
    escript eval("system'console writeLine(""Hello World"").").
].
```

```txt

Hello World

```



## Elixir


```elixir
iex(1)> Code.eval_string("x + 4 * Enum.sum([1,2,3,4])", [x: 17])
{57, [x: 17]}
iex(2)> Code.eval_string("c = a + b", [a: 1, b: 2])
{3, [a: 1, b: 2, c: 3]} 
iex(3)> Code.eval_string("a = a + b", [a: 1, b: 2])
{3, [a: 3, b: 2]}
```



## Erlang


Erlang eval is a bit complex/verbose and requires the interaction of 3 modules: <tt>erl_scan</tt> (tokenizes), <tt>erl_parse</tt> (returns an abstract form) and <tt>erl_eval</tt> (variable binding, evaluate abstract form, etc).

```erlang
1>
 {ok, Tokens, _} = erl_scan:string("X + 4 * lists:sum([1,2,3,4]).").
...
2> {ok, [Form]} = erl_parse:parse_exprs(Tokens).
...
3> Bindings = erl_eval:add_binding('X', 17, erl_eval:new_bindings()).
[{'X',17}]
4> {value, Value, _} = erl_eval:expr(Form, Bindings).                 
{value,57,[{'X',17}]}
5> Value.
57

```



## Factor


Arbitrary strings can be <tt>eval</tt>'d, but you must provide their stack effect.


```txt

IN: scratchpad "\"Hello, World!\" print" ( -- ) eval
Hello, World!
IN: scratchpad  4 5 "+" ( a b -- c ) eval
9

```


You can use the <tt>infer</tt> word to infer a quotation's stack effect. You can combine <tt>infer</tt> with <tt>parse-string</tt> to <tt>eval</tt> an arbitrary string without writing the stack effect yourself.


```txt

( scratchpad ) "USE: math 8 9 +" dup parse-string
"USE: math 8 9 +"
[ 8 9 + ] 
( scratchpad ) infer
"USE: math 8 9 +"
( x x -- x )
( scratchpad ) eval
17

```



## Forth

EVALUATE invokes the interpreter on a string of Forth code, using and modifying the current dictionary and stack state.

```forth
s" variable foo   1e fatan 4e f*" evaluate

f.      \ 3.14159...
1 foo !
```


Sandboxing can be achieved in general by using MARKER, which defines a checkpoint for the dictionary state which can later be restored.


```forth
unused .    \ show how much dictionary space is available
marker restore

create foo 30 allot
: my-def 30 0 do cr i . ." test" loop ;

unused .    \ lower than before

restore
unused .    \ same as first unused;  restore, foo, and my-def no longer defined
```



## Frink

The <CODE>eval[]</CODE> function can be used to evaluate aribitrary Frink code in the current environment, or in a new context.

```frink

eval["length = 1234 feet + 2 inches"]

```


There is also a two-argument version, <CODE>eval[expression, rethrows]</CODE> where the <CODE>rethrows</CODE> argument is a boolean flag indicating if we want evaluation errors to be thrown or just suppressed and <CODE>undef</CODE> returned.  If it is true, errors will be rethrown as Java exceptions, otherwise an error returns undef.

There is also a three-argument version, <CODE>eval[expression, rethrows, hidesLocals]</CODE> where the <CODE>hidesLocal</CODE> argument is a boolean flag indicating if we want to hide local variables (that is, create a new context) before evaluation. 

Frink has an extensive security manager which allows the eval statement to prevent unsecure operations such as reading or writing a file or URL, creating new functions or classes, altering systemwide flags, evaluate arbitrary Java code, and so on.  If code needs to evaluate unsecure statments, you can use the intentionally frighteningly-named <CODE>unsafeEval[str]</CODE> (which may itself be disallowed in secure contexts.)


## Go

As a compiled, strongly typed language, <code>eval()</code> is not the strong suit of Go.  Nevertheless, an <code>eval</code> package exists that does that.  Just don't expect it to be as easy or efficient as in interpreted languages.  The eval package was originally part of the Go standard library but is now hosted and maintained externally.

```go
package main
import (
	"fmt"
	"bitbucket.org/binet/go-eval/pkg/eval"
	"go/token"
)

func main() {
	w := eval.NewWorld();
	fset := token.NewFileSet();

	code, err := w.Compile(fset, "1 + 2")
	if err != nil {
		fmt.Println("Compile error");
		return
	}

	val, err := code.Run();
	if err != nil {
		fmt.Println("Run time error");
		return;
	}
	fmt.Println("Return value:", val) //prints, well, 3

}
```



## Groovy

Each of these solutions evaluates a Groovy script based on some variation of the solution to the "[[Yuletide_Holiday#Groovy|Yuletide Holiday]]" task. 

Each variation has been verified to give the same output:

```txt
[2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118]
```



### Simple evaluation

The '''GroovyShell''' class allows the evaluation of a string or of the text contents of a '''File''' or '''InputStream''' as a ''Groovy script''. 
A script is a either a set of statements to be executed in order, or a Groovy class with a '''main()''' method, or a Groovy '''Thread''' subclass or '''Runnable''' implementation. 
The return value is the value of the last statement executed, or the value of an explicit '''return''' statement (if any).

```groovy
def years1 = new GroovyShell().evaluate('''
(2008..2121).findAll {
    Date.parse("yyyy-MM-dd", "${it}-12-25").format("EEE") == "Sun"
}
''')

println years1
```


The last expression evaluated in the script, a list of years found, is the return value of the '''evaluate()''' method.


### Evaluation with variables

There are several approaches to evaluating a script with variables:
*'''GString''' embedded values
*'''Binding''' variables
*'''Eval''' shortcut

'''GString''' embedded values

Setting up the script as a '''GString''' with embedded value parsing is a "natural" ''ad hoc'' solution for Groovy programmers, but there are possible pitfalls if the script itself contains '''GString'''s.

```groovy
def startYear = 2008
def endYear = 2121
def years2 = new GroovyShell().evaluate("""
(${startYear}..${endYear}).findAll {
    Date.parse("yyyy-MM-dd", "\${it}-12-25").format("EEE") == "Sun"
}
""")

println years2
```

The variables "startYear" and "endYear" are dynamically pulled into the script '''GString''' as embedded values before the script itself ever executes.

Notice that in the script the embedded value "${it}" must be ''quoted'' with backslash (\) to prevent parsing as a part of the script '''GString'''. However, it is still correctly parsed within the internal '''GString''' when the script is run.

'''Binding''' variables

'''GroovyShell''' uses a '''Binding''' object to pass variable values to a script. This is the only way to pass variables if the script comes from a '''File''' or '''InputStream''', but even if the script is a string '''Binding''' avoids the nested quoting issue caused by the ''ad hoc'' use of '''GString'''.

```groovy
def context = new Binding()
context.startYear = 2008
context.endYear = 2121
def years3 = new GroovyShell(context).evaluate('''
(startYear..endYear).findAll {
    Date.parse("yyyy-MM-dd", "${it}-12-25").format("EEE") == "Sun"
}
''')
```


We may instantiate '''Binding''' with the variables as named parameters, allowing a more terse syntax:

```groovy
def years4 = new GroovyShell( new Binding(startYear: 2008, endYear: 2121) ).evaluate('''
(startYear..endYear).findAll {
    Date.parse("yyyy-MM-dd", "${it}-12-25").format("EEE") == "Sun"
}
''')

println years4
```


We may also access the '''Binding''' object ''after'' script evaluation to extract values of any global variables set during the evaluation:

```groovy
def binding = new Binding(startYear: 2008, endYear: 2121)
new GroovyShell( binding ).evaluate('''
yearList = (startYear..endYear).findAll {
    Date.parse("yyyy-MM-dd", "${it}-12-25").format("EEE") == "Sun"
}
''')

println binding.yearList
```


'''Eval''' shortcut

For simple evaluation of string-based scripts with only a few variables (like this one), the '''Eval''' class has static shortcut methods that do the '''Binding''' setup and '''GroovyShell''' evaluation under the surface. '''Eval.me(script)''' evaluates a script with no variables. '''Eval.x(x,script)''', '''Eval.xy(x,y,script)''', or '''Eval.xyz(x,y,z,script)''' each evaluates a script with 1, 2, or 3 variables, respectively. Here is an example with start and end years as script variables ''x'' and ''y''.

```groovy
def years5 = Eval.xy(2008, 2121, '''
(x..y).findAll {
    Date.parse("yyyy-MM-dd", "${it}-12-25").format("EEE") == "Sun"
}
''')

println years5
```


=={{header|GW-BASIC}}==

```qbasic
10 LINE INPUT "Type an expression: ",A$
20 OPEN "CHAIN.TMP" FOR OUTPUT AS #1
30 PRINT #1, "70 LET Y=("+A$+")"
40 CLOSE #1
50 CHAIN MERGE "CHAIN.TMP",60,ALL
60 FOR X=0 TO 5
70 REM
80 PRINT X,Y
90 NEXT X
100 GOTO 10
```



## Harbour


```harbour

Procedure Main()
   local bAdd := {|Label,n1,n2| Qout( Label ), QQout( n1 + n2 )}
   Eval( bAdd, "5+5 = ", 5, 5 )
   Eval( bAdd, "5-5 = ", 5, -5 )
   return

Upon execution you see: 
5+5 = 10
5-5 =  0

```



## HicEst

XEQ invokes the interpreter on a string of HicEst code, but keeps the current dictionary and stack state. Blocks of expressions are not possible.

```HicEst
value = XEQ( " temp = 1 + 2 + 3 ") ! value is assigned 6
! temp is undefined outside XEQ, if it was not defined before.

XEQ(" WRITE(Messagebox) 'Hello World !' ")

OPEN(FIle="my_file.txt")
READ(FIle="my_file.txt", Row=6) string
XEQ( string ) ! executes row 6 of my_file.txt
```



## J

Use monadic [http://www.jsoftware.com/help/dictionary/d601.htm <code>".</code>] (''Do'') to execute a string.


```j
". 'a =: +/ 1 2 3' NB. execute a string to sum 1, 2 and 3 and assign to noun a
```


Only J expressions are allowed in strings used as as arguments for <code>".</code> (control words and blocks of expressions are not allowed).

Alterntively, you can use the conjunction [http://www.jsoftware.com/help/dictionary/d310n.htm <code>:</code>] (''Explicit Definition'') to create various kinds of functions and evaluate them.  Arguments have names, such as "y", which are specified by the language definition.  For example:


```j
monad :'+/y' 1 2 3
```


Rules of scope for such functions match those described on the [[Scope modifiers]] page.  Also, control words (like if. or for. or while.) and blocks of expressions are allowed in strings which are evaluated in this fashion.

The context for these evaluations will always be the current [http://wiki.jsoftware.com/help/primer/locale.htm locale] (which might typically be the current object [or class]).  If only expressions are allowed, then local variables will be local to the current explicit definition.  Otherwise a new local context will be created for the evaluation (and this will be discarded when evaluation has completed).  Local contexts are lexical while locales may also be manipulated programatically.

Debugging facilities [currently] require that the operation be given a name.

J relies on the OS for sandboxing and does not offer any additional resource constraints.



## Java

You can kind-of do this in Java. The compiler has the relevant APIs, so it is "considered part of your language/library/platform". You have to get a compiler (which may fail), make a pseudo-file-system, compile your class, and make a class loader that will load it. Then you can use regular Java reflection to make an instance and call methods on it.

Longest "Hello world" program ever?
```Java
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;

public class Evaluator{
    public static void main(String[] args){
        new Evaluator().eval(
            "SayHello",
            "public class SayHello{public void speak(){System.out.println(\"Hello world\");}}",
            "speak"
        );
    }

    void eval(String className, String classCode, String methodName){
        Map<String, ByteArrayOutputStream> classCache = new HashMap<>();
        JavaCompiler                       compiler   = ToolProvider.getSystemJavaCompiler();

        if ( null == compiler )
            throw new RuntimeException("Could not get a compiler.");

        StandardJavaFileManager                            sfm  = compiler.getStandardFileManager(null, null, null);
        ForwardingJavaFileManager<StandardJavaFileManager> fjfm = new ForwardingJavaFileManager<StandardJavaFileManager>(sfm){
            @Override
            public JavaFileObject getJavaFileForOutput(Location location, String className, JavaFileObject.Kind kind, FileObject sibling)
                    throws IOException{
                if (StandardLocation.CLASS_OUTPUT == location && JavaFileObject.Kind.CLASS == kind)
                    return new SimpleJavaFileObject(URI.create("mem:///" + className + ".class"), JavaFileObject.Kind.CLASS){
                        @Override
                        public OutputStream openOutputStream()
                                throws IOException{
                            ByteArrayOutputStream baos = new ByteArrayOutputStream();
                            classCache.put(className, baos);
                            return baos;
                        }
                    };
                else
                    throw new IllegalArgumentException("Unexpected output file requested: " + location + ", " + className + ", " + kind);
            }
        };
        List<JavaFileObject> files = new LinkedList<JavaFileObject>(){{
            add(
                new SimpleJavaFileObject(URI.create("string:///" + className + ".java"), JavaFileObject.Kind.SOURCE){
                    @Override
                    public CharSequence getCharContent(boolean ignoreEncodingErrors){
                        return classCode;
                    }
                }
            );
        }};

        // Now we can compile!
        compiler.getTask(null, fjfm, null, null, null, files).call();

        try{
            Class<?> clarse = new ClassLoader(){
                @Override
                public Class<?> findClass(String name){
                    if (! name.startsWith(className))
                        throw new IllegalArgumentException("This class loader is for " + className + " - could not handle \"" + name + '"');
                    byte[] bytes = classCache.get(name).toByteArray();
                    return defineClass(name, bytes, 0, bytes.length);
                }
            }.loadClass(className);

            // Invoke a method on the thing we compiled
            clarse.getMethod(methodName).invoke(clarse.newInstance());

        }catch(ClassNotFoundException | InstantiationException | IllegalAccessException | NoSuchMethodException | InvocationTargetException x){
            throw new RuntimeException("Run failed: " + x, x);
        }
    }
}
```


```txt
Hello world
```

If you have a JRE and not a JDK, there is no compiler, so this doesn't work.
```txt
Exception in thread "main" java.lang.RuntimeException: Could not get a compiler.
        at Evaluator.eval(Evaluator.java:33)
        at Evaluator.main(Evaluator.java:21)
```



## JavaScript

The eval method handles statements and expressions well:

```javascript

var foo = eval('{value: 42}');
eval('var bar = "Hello, world!";');

typeof foo; // 'object'
typeof bar; // 'string'

```



## Jsish

From Javascript entry.

```javascript
/* Runtime evaluation, in Jsish */
var foo = eval('{value: 42}');
eval('var bar = "Hello, world!";');

;typeof foo;
;foo.value;
;typeof bar;
;bar;

/*
=!EXPECTSTART!=
typeof foo ==> object
foo.value ==> 42
typeof bar ==> string
bar ==> Hello, world!
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish --U runtimeEvaluation.jsi
typeof foo ==> object
foo.value ==> 42
typeof bar ==> string
bar ==> Hello, world!
```



## Julia

To run an entire script in the current env:


```julia
include("myfile.jl")
```


To run a string in the current env:


```julia
include_string("""
    x = sum([1, 2, 3])
    @show x
    """)

@show typeof(x) # Int64
```



## Kotlin


Kotlin has a REPL which is started from the command line by running the compiler without any parameters.

Any valid Kotlin code can then be entered and immediately evaluated.

Below is a sample session.


```txt

$ kotlinc
Welcome to Kotlin version 1.2.31 (JRE 1.8.0_162-8u162-b12-0ubuntu0.16.04.2-b12)
Type :help for help, :quit for quit
>>> 20 + 22
42
>>> 5 * Math.sqrt(81.0)
45.0
>>> fun triple(x: Int) = x * 3
>>> triple(16)
48
>>> :quit

```



## Lasso

"Sourcefile" when executed has access to all variables and other data that would be available in scope to an included file.

This means thread vars ($) and types/methods already defined will be accessible.

Types, methods, traits and thread vars created or modified will maintain state subsequently - 
i.e.
	if a type is defined in code executed in a sourcefile context then that type will be available after execution. 
	if a thread var is modified in the sourcefile executed code then the var will maintain that value after execution.

Local variables (#) maintain scope behaviour as normal.

Output is governed by the "autocollect" boolean, 
the third parameter in the sourcefile invocation.


```Lasso
//code, fragment name, autocollect, inplaintext
local(mycode = "'Hello world, it is '+date")
sourcefile('['+#mycode+']','arbritraty_name', true, true)->invoke

'\r'


var(x = 100)
local(mycode = "Outside Lasso\r['Hello world, var x is '+var(x)]")
// autocollect (3rd param): return any output generated
// inplaintext (4th param): if true, assumes this is mixed Lasso and plain text, 
//		requires Lasso code to be in square brackets or other supported code block demarkation. 
sourcefile(#mycode,'arbritraty_name', true, true)->invoke

'\r'

var(y = 2)
local(mycode = "'Hello world, is there output?\r'
var(x) *= var(y)")
// autocollect (3rd param): as false, no output returned
// inplaintext (4th param): as false, assumes this is Lasso code, no mixed-mode Lasso and text.
sourcefile(#mycode,'arbritraty_name', false, false)->invoke
'var x is now: '+$x

'\r'

var(z = 3)
local(mycode = "var(x) *= var(z)")
sourcefile(#mycode,'arbritraty_name', false, false)->invoke
'var x is now: '+$x
```


```txt
Hello world, it is 2013-11-10 15:54:19
Outside Lasso
Hello world, var x is 100
var x is now: 200
var x is now: 600
```



## Liberty BASIC

Liberty BASIC has the ability to evaluate arrays using a string for the array name and a variable for the element.

```lb

'Dimension a numerical and string array
Dim myArray(5)
Dim myStringArray$(5)

'Fill both arrays with the appropriate data
For i = 0 To 5
    myArray(i) = i
    myStringArray$(i) = "String - " + str$(i)
Next i

'Set two variables with the names of each array
numArrayName$ = "myArray"
strArrayName$ = "myStringArray"

'Retrieve the array data by evaluating a string
'that correlates to the array
For i = 0 To 5
    Print Eval$(numArrayName$ + "(" + str$(i) + ")")
    Print Eval$(strArrayName$ + "$(" + str$(i) + ")")
Next i 
```


An example using a struct and a pointer.

```lb

Struct myStruct, value  As long, _
                 string As ptr
myStruct.value.struct = 10
myStruct.string.struct = "Hello World!"

structName$ = "myStruct"
numElement$ = "value"
strElement$ = "string"

Print Eval$(structName$ + "." + numElement$ + "." + "struct")

'Pay close attention that this is EVAL() because we are
'retrieving the PTR to the string which is essentially a ulong
Print Winstring(Eval(structName$ + "." + strElement$ + "." + "struct")) 
```



## Lua


```lua
f = loadstring(s) -- load a string as a function. Returns a function.

one = loadstring"return 1" -- one() returns 1

two = loadstring"return ..." -- two() returns the arguments passed to it
```


In Lua 5.2 the <code>loadstring</code> function is superseded by the more general <code>load</code> function, which can be used in a compatible way. Nevertheless, <code>loadstring</code> is still available.

```lua
f = load("return 42")
f() --> returns 42
```



## M2000 Interpreter


```M2000 Interpreter

Module checkit {
      Module dummy {
            i++
            Print Number
      }
      \\ using Stack New { } we open a new stack for values, and old one connected back at the end
      \\ using block For This {} we erase any new definition, so we erase i (which Local make a new one)
      a$={
            Stack New {
                  For this {
                        Local i
                        for i=1 to 10 : print i : next i
                  }
            }
            If valid(k) then print k
      }
      i=500
      k=600
      Push 1000
      inline a$
      Print i=500
      Print Number=1000
      \\ eval an expression
      Print Eval("i+k")
      \\ eval a function
      Print Function("{read x : = x**2}", 2)=4
      Dim k(10)=123
      \\ eval array only
      Print array("k()", 2)=123
      Push 10, 10
      \\ call a module by make it inline first
      inline code dummy, dummy
      Print i=502
}
CheckIt

```




## Mathematica

Mathematica's <code>ToExpression</code> evaluates an expression string as if it were placed directly in the code. Statements are just <code>CompoundExpression</code>s, so they also work. Any evaluation can be limited with <code>TimeConstrained</code> and <code>MemoryConstrained</code>.

```Mathematica
Print[ToExpression["1 + 1"]];
Print[ToExpression["Print[\"Hello, world!\"]; 10!"]];
x = 5;
Print[ToExpression["x!"]];
Print[ToExpression["Module[{x = 8}, x!]"]];
Print[MemoryConstrained[ToExpression["Range[5]"], 10000, {}]];
Print[MemoryConstrained[ToExpression["Range[10^5]"], 10000, {}]];
Print[TimeConstrained[ToExpression["Pause[1]; True"], 2, False]];
Print[TimeConstrained[ToExpression["Pause[60]; True"], 2, False]];
```

```txt
2
Hello, world!
3628800
120
40320
{1, 2, 3, 4, 5}
{}
True
False
```



## MATLAB

The eval and evalin functions handles any kind of code. It can handle multi-line code, although it needs the lines to be separated by the newline character. It can even allow you to program at runtime, as illustrated in the last example in the code and output below. 
Errors can occur when mixing eval statements with regular code, especially "compile-time" errors if the code appears to be missing key elements (ending brackets or end statements, etc). Some of these are also demonstrated.

```MATLAB
function testEval
    fprintf('Expressions:\n')
    x = eval('5+10^2')
    eval('y = (x-100).*[1 2 3]')
    eval('z = strcat(''my'', '' string'')')
    try
        w eval(' = 45')
    catch
        fprintf('Runtime error: interpretation of w is a function\n\n')
    end
    % eval('v') = 5
    % Invalid at compile-time as MATLAB interprets as using eval as a variable
    
    fprintf('Other Statements:\n')
    nl = sprintf('\n');
    eval(['for k = 1:20' nl ...
              'fprintf(''%.3f\n'', k)' nl ...
              'if k == 3' nl ...
                  'break' nl ...
              'end' nl ...
          'end'])
    true == eval('1')
    try
        true eval(' == 1')
    catch
        fprintf('Runtime error: interpretation of == 1 is of input to true\n\n')
    end
    
    fprintf('Programming on the fly:\n')
    userIn = true;
    codeBlock = '';
    while userIn
        userIn = input('Enter next line of code: ', 's');
        codeBlock = [codeBlock nl userIn];
    end
    eval(codeBlock)
end
```

```txt
Expressions:

x =

   105


y =

     5    10    15


z =

my string

Runtime error: interpretation of w is a function

Other Statements:
1.000
2.000
3.000

ans =

     1

Runtime error: interpretation of == 1 is of input to true

Programming on the fly:
Enter next line of code: fprintf('Goodbye, World!\n')
Enter next line of code: str = 'Ice and Fire';
Enter next line of code: words = textscan(str, '%s');
Enter next line of code: fprintf('%s ', words{1}{end:-1:1})
Enter next line of code: 
Goodbye, World!
Fire and Ice
```



## Maxima



```maxima
/* Here is how to create a function and return a value at runtime. In the first example,
the function is made global, i.e. it still exists after the statement is run. In the second example, the function
is declared local. The evaluated string may read or write any variable defined before eval_string is run. */

kill(f)$

eval_string("block(f(x) := x^2 + 1, f(2))");
5

fundef(f);
/* f(x) := x^2 + 1 */

eval_string("block([f], local(f), f(x) := x^3 + 1, f(2))");
9

fundef(f);
/* f(x) := x^2 + 1 */
```



## Oforth


Oforth can evaluate strings at runtime.

In order to restrict evaluation, perform is used on strings. With perform, only objects can be evaluated. If a function or a method is included into the string an exception is raised and the function is not evaluated.


```Oforth
"[ [ $a, 12], [$b, 1.2], [ $c, [ $aaa, $bbb, $ccc ] ], [ $torun, #first ] ]" perform .s
[1] (List) [[a, 12], [b, 1.2], [c, [aaa, bbb, ccc]], [torun, #first]]

"12 13 +" perform
[1:interpreter] ExCompiler : Can't evaluate <+>
```



In order to evaluate any Oforth code, eval can be used. This method should not be used on unsafe strings.


```Oforth
"12 13 + println" eval
25
": newFunction(a)  a + ; 12 10 newFunction println" eval
22
```



## ooRexx

The ooRexx INTERPRET instruction allows execution of dynamically constructed code.  Almost any well-formed code can be executed dynamically, including multiple instructions at a time.  The instructions are executed in the local context where the interpret instruction executes, so full access to the current variable context is available. For example:

```ooRexx

   a = .array~of(1, 2, 3)
   ins = "loop num over a; say num; end"
   interpret ins

```

Executes the LOOP instruction, displaying the contents of the array pointed to by variable A.


## OxygenBasic

Runtime (secondary) compiling is possible, with some restrictions. 
For instance, static variables may not be created by the compiled code, but parental variables are visible to it. 
This demo produces tables of Y values, given a formula, and a range of X values to step through.


```oxygenbasic


  function ExecSeries(string s,double b,e,i) as string
  '
### =============================================

  '
  sys a,p
  string v,u,tab,cr,er
  '
  'PREPARE OUTPUT BUFFER
  '
  p=1
  cr=chr(13) chr(10)
  tab=chr(9)
  v=nuls 4096
  mid v,p,s+cr+cr
  p+=4+len s
  '
  double x,y,z 'shared variables
  '
  'COMPILE
  '
  a=compile s
  er=error
  if er then
    print "runtime error: "  er : exit function
  end if
  '
  'EXECUTE
  '
  for x=b to e step i
    if p+128>=len v then
      v+=nuls len(v) 'extend buffer
    end if
    call a
    u=str(x) tab str(y) cr
    mid v,p,u : p+=len u
  next
  '
  freememory a 'release compiled code
  '
  return left v,p-1 'results
  '
  end function

  '=====
  'TESTS
  '=====

  'Expression, StartVal, EndVal stepVal, Increment

  print ExecSeries "y=x*x*x", 1, 10, 1
  print ExecSeries "y=sqrt x",1, 9 , 1


```



## Oz


```oz
declare
  %% simplest case: just evaluate expressions without bindings
  R1 = {Compiler.virtualStringToValue "{Abs ~42}"}
  {Show R1}

  %% eval expressions with additional bindings and
  %% the possibility to kill the evaluation by calling KillProc
  KillProc
  R2 = {Compiler.evalExpression "{Abs A}" unit('A':~42) ?KillProc}
  {Show R2}

  %% full control: add and remove bindings, eval expressions or
  %% statements, set compiler switches etc.
  Engine = {New Compiler.engine init}
  {Engine enqueue(setSwitch(expression false))} %% statements instead of expr.
  {Engine enqueue(mergeEnv(env('A':42 'System':System)))}
  {Engine enqueue(feedVirtualString("{System.show A}"))}
```


By restricting the environment it is possible to restrict what kind of programs can be run.


## PARI/GP

Since GP is usually run from the [[wp:Read–eval–print loop|REPL]] gp, it is trivial to evaluate programs at runtime (most are run this way). Slightly less trivial is passing code around as a first-class object:


```parigp
runme(f)={
  f()
};

runme( ()->print("Hello world!") )
```


One facility designed for restricting such embedded programs is <code>default(secure,1)</code> which denies scripts the ability to run <code>system</code> and <code>extern</code>. This cannot be turned off except interactively.


## Perl

The <code>eval</code> function accepts a block or a string as its argument. The difference is that a block is parsed at compile-time, whereas a string is parsed at runtime. The block or string may represent any valid Perl program, including a single expression. The subprogram executes in the same lexical and dynamic scope as the surrounding code. The return value of a call to <code>eval</code> depends on how the subprogram terminates:
* If control reaches the end of the subprogram, <code>eval</code> returns the value of the last expression evaluated.
* If the subprogram uses an explicit <code>return</code>, <code>eval</code> returns the given value.
* If the subprogram throws an exception, <code>eval</code> returns <code>undef</code>. The text of the exception is assigned to <code>$@</code>. (When the subprogram terminates without an exception, <code>$@</code> is set to the null string instead.)


```perl
my ($a, $b) = (-5, 7);
$ans = eval 'abs($a * $b)';  # => 35
```



## Perl 6

Any syntactically valid sequence of statements may be run, and the snippet to be run can see its outer lexical scope at the point of the <tt>eval</tt>:

```perl6
use MONKEY-SEE-NO-EVAL;

my ($a, $b) = (-5, 7);
my $ans = EVAL 'abs($a * $b)';  # => 35
```

Unlike in Perl 5, <tt>eval</tt> in Perl 6 only compiles and executes the string, but does not trap exceptions.  You must say <tt>try eval</tt> to get that behavior (or supply a <tt>CATCH</tt> block within the text to be evaluated).


## PHP

The [http://www.php.net/eval eval construct] allow string evaluation as PHP code. Opening and closing tags are not required. Return statements immediatly terminates evaluation . Eval returns NULL, unless return is called in evalued code.

```php

<?php
  $code = 'echo "hello world"';
  eval($code);
  $code = 'return "hello world"';
  print eval($code);

```



## PicoLisp

In PicoLisp there is a formal equivalence of code and data. Almost any piece of
data is potentially executable. PicoLisp has three internal data types: Numbers,
symbols and lists. Though in certain contexts (e.g. GUI objects) also atomic
data (numbers and symbols) are evaluated as code entities, a typical executable
item is a list.

The PicoLisp reference distinguishes between two terms: An 'exe' (expression) is
an executable list, with a function as the first element, followed by arguments.
A 'prg' (program) is a list of 'exe's, to be executed sequentially.

'exe's and 'prg's are implicit in the whole runtime system. For example, the
body of a function is a 'prg', the "true" branch of an 'if' call is an 'exe',
while the "false" branch again is a 'prg'.

For explicit execution, an 'exe' can be evaluated by passing it to the function
'[http://software-lab.de/doc/refE.html#eval eval]', while a 'prg' can be handled
by '[http://software-lab.de/doc/refR.html#run run]'.

As PicoLisp uses exclusively dynamic binding, any 'exe' or 'prg' can be executed
in arbitrary contexts. The environmet can be controlled in any conceivable way,
through implicit function parameter bindings, or explicitly with the aid of
functions like '[http://software-lab.de/doc/refB.html#bind bind]',
'[http://software-lab.de/doc/refL.html#let let]' or
'[http://software-lab.de/doc/refJ.html#job job]'.


## Pike

Pike provides [http://pike.ida.liu.se/doc/compile_string() <code>compile_string()</code>] and [http://pike.ida.liu.se/doc/compile_file() <code>compile_file()</code>] which can compile code into a class that can be instantiated:

```Pike
program demo = compile_string(#"
    string name=\"demo\";
    string hello()
    { 
       return(\"hello, i am \"+name); 
    }");

demo()->hello();
Result: "hello, i am demo"
```


an actual application of this is shown in [[Simple database]].


## PowerShell

Evaluate an expression:

```PowerShell

$test2plus2 = '2 + 2 -eq 4'
Invoke-Expression $test2plus2

```

```txt

True

```

Evaluate a <code>[scriptblock]</code> (a statement or group of statements) with code surrounded by curly braces using the '''&''' ('''call''') operator:

```PowerShell

$say = {"Hello, world!"}
& $say

```

```txt

Hello, world!

```

Scriptblocks behave just as functions so they may have parameters:

```PowerShell

$say = {param ([string]$Subject) "Hello, $Subject!"}
& $say -Subject "my friend"

```

```txt

Hello, my friend!

```

A slightly more complex example:

```PowerShell

$say = {param ([string]$Exclamation, [string]$Subject) "$Exclamation, $Subject!"}
& $say -Exclamation "Goodbye" -Subject "cruel world"

```

```txt

Goodbye, cruel world!

```

To reverse the normal behaviour of a <code>[scriptblock]</code> use the '''GetNewClosure''' method.  This makes the scriptblock self-contained or closed; ie, the variable will only be read when the scriptblock is initialised:

```PowerShell

$title = "Dong Work For Yuda"
$scriptblock = {$title}
$closedScriptblock = $scriptblock.GetNewClosure()

& $scriptblock
& $closedScriptblock

```

```txt

Dong Work For Yuda
Dong Work For Yuda

```

Change the variable and execute the scriptblock, the closed version will not reflect the change:

```PowerShell

$title = "I'm Too Sexy"
& $scriptblock
& $closedScriptblock

```

```txt

I'm Too Sexy
Dong Work For Yuda

```

Since the [scriptblock] type is an anonymous function, the Begin {}, Process {} and End {} blocks may be added to a scriptblock, just like any function.


## Python

The [http://docs.python.org/reference/simple_stmts.html#exec exec statement] allows the optional passing in of global and local names via mappings (See the link for full syntax). The example below shows exec being used to parse and execute a string containing two statements:


```python
>>>
 exec '''
x = sum([1,2,3,4])
print x
'''
10
```


Note that in Python 3.x [http://docs.python.org/py3k/library/functions.html#exec exec] is a function:


```python
>>>
 exec('''
x = sum([1,2,3,4])
print(x)
''')
10
```



## R


In R, expressions may be manipulated directly as abstract syntax trees, and evaluated within environments.

<tt>quote()</tt> captures the abstract syntax tree of an expression.
<tt>parse()</tt> does the same starting from a string.
<tt>call()</tt> constructs an evaluable parse tree.
Thus all these three are equivalent.


```r
expr1 <- quote(a+b*c)
expr2 <- parse(text="a+b*c")[[1]]
expr3 <- call("+", quote(`a`), call("*", quote(`b`), quote(`c`)))
```


<tt>eval()</tt> evaluates a quoted expression. <tt>evalq()</tt> is a version of <tt>eval()</tt> which quotes its first argument.


```r>
 a <- 1; b <- 2; c <- 3
> eval(expr1)
[1] 7
```


<tt>eval()</tt> has an optional second environment which is the lexical environment to evaluate in.


```r>
 env <- as.environment(list(a=1, b=3, c=2))
> evalq(a, env)
[1] 1
> eval(expr1, env) #this fails; env has only emptyenv() as a parent so can't find "+"
Error in eval(expr, envir, enclos) : could not find function "+"
> parent.env(env) <- sys.frame()
> eval(expr1, env) # eval in env, enclosed in the current context
[1] 7
> assign("b", 5, env) # assign() can assign into environments
> eval(expr1, env)
[1] 11
```



## Racket


Racket has the usual <tt>eval</tt> that is demonstrated [[Runtime_evaluation/In_an_environment#Racket|here]], and in addition, it has a sandbox environment that provides a safe evaluator that is restricted from accessing files, network, etc.


```Racket

#lang racket
(require racket/sandbox)
(define e (make-evaluator 'racket))
(e '(define + *))
(e '(+ 10 20))
(+ 10 20)
;; (e '(delete-file "/etc/passwd"))
;; --> delete-file: `delete' access denied for /etc/passwd

```


And, of course, both of these methods can use Racket's multilingual capabilities and evaluate the code in a language with different semantics.


## REBOL

The [http://www.rebol.com/r3/docs/functions/do.html do] function evaluates a script file or a series of expressions and returns a result.

It performs the fundamental interpretive action of the Rebol language and is used internally within many other functions such as <tt>if, case, while, loop, repeat, foreach</tt>, and others.

```rebol
a: -5
b: 7
answer: do [abs a * b]     ; => 35
```



## REXX

This REXX program does a:
:::*     run-time evaluation of an internal expression,   and
:::*     run-time evaluation of a user-prompted expression.

```rexx
/*REXX program illustrates the ability to  execute code  entered  at runtime (from C.L.)*/
numeric digits 10000000                          /*ten million digits should do it.     */
bee=51
stuff= 'bee=min(-2,44);  say 13*2 "[from inside the box.]";  abc=abs(bee)'
interpret stuff
say 'bee='  bee
say 'abc='  abc
say
                                                 /* [↓]  now, we hear from the user.    */
say 'enter an expression:'
pull expression
say
say 'expression entered is:'  expression
say

interpret '?='expression

say 'length of result='length(?)
say ' left 50 bytes of result='left(?,50)"···"
say 'right 50 bytes of result=···'right(?, 50)   /*stick a fork in it,  we're all done. */
```

'''output'''   when using the input: <tt> 2**44497 - 1 </tt> 

which happens to be the 27th Mersenne prime.

```txt

26 [from inside the box.]
bee= -2
abc= 2

enter an expression:
2**44497 - 1

expression entered is: 2**44497 - 1

length of result=13395
 left 50 bytes of result=85450982430363380319330070531840303650990159130402···
right 50 bytes of result=···22977396345497637789562340536844867686961011228671

```



## Ring


```ring

Eval("nOutput = 5+2*5 " )
See "5+2*5 = " + nOutput + nl
Eval("for x = 1 to 10 see x + nl next")
Eval("func test see 'message from test!' ")
test()

```

Output :

```ring

5+2*5 = 15
1
2
3
4
5
6
7
8
9
10
message from test!

```


We can create simple interactive programming environment using the next program

```ring

while true
        see nl + "code:> "
        give cCode
        try
                eval(cCode)
        catch
                see cCatchError
        done
end

```


Output

```ring

code:> see "hello world"
hello world
code:> for x = 1 to 10 see x + nl next
1
2
3
4
5
6
7
8
9
10

code:> func test see "Hello from test" + nl

code:> test()
Hello from test

code:> bye

```



## Ruby


The <tt>eval</tt> method evaluates a string as code and returns the resulting object.  With one argument, it evaluates in the current context:

```ruby
a, b = 5, -7
ans = eval "(a * b).abs"  # => 35
```


With two arguments, <tt>eval</tt> runs in the given <tt>Binding</tt> or <tt>Proc</tt> context:

```ruby
def first(main_var, main_binding)
  foo = 42
  second [[main_var, main_binding], ["foo", binding]]
end

def second(args)
  sqr = lambda {|x| x**2}
  deref(args << ["sqr", binding])
end

def deref(stuff)
  stuff.each do |varname, context|
    puts "value of #{varname} is #{eval varname, context}"
  end
end

hello = "world"
first "hello", binding
```
 

```txt
value of hello is world
value of foo is 42
value of sqr is #<Proc:0x1002ef6c@eval.rb:7>
```



## Scheme

In Scheme, the expression passed to eval is evaluated in the current interaction environment, unless otherwise specified. The result is read back as a Scheme value.


```scheme>
 (define x 37)
> (eval '(+ x 5))
42
> (eval '(+ x 5) (interaction-environment))
42
> (eval '(+ x 5) (scheme-report-environment 5)) ;; provides R5RS definitions

Error: identifier not visible x.
Type (debug) to enter the debugger.
> (display (eval (read)))
(+ 4 5) ;; this is input from the user.
9
```



## Sidef

The eval method evaluates a string as code and returns the resulting object.


```ruby
var (a, b) = (-5, 7);
say eval '(a * b).abs';  # => 35
say (a * b -> abs);      # => 35
```



## Slate


In Slate, programs are represented as Syntax Node trees, with methods defined on the various syntactic types. The backtick syntax provides a convenient quoting mechanism, and as objects, they have convenient methods defined for evaluation or evaluation within a specific environment:

```slate
`(4 + 5) evaluate.
`(4 + 5) evaluateIn: prototypes.
```


You can also explicitly invoke the Parser on a String, to convert it into syntactic objects:

```slate
(Syntax Parser newOn: '4 + 5') upToEnd do: [| :each | print: each evaluate]
```


You can construct a program using externally-specified values using <tt>`unquote</tt> within a quoted expression:

```slate
define: #x -> 4.
`(x `unquote + 5) evaluate.
```


Or you can obviously construct a string:

```slate
define: #x -> 4.
(Syntax Parser newOn: x printString ; ' + 5')
```


The <tt>evaluate</tt> method also takes into consideration the current lexical scope, unless another environment is specified. The following returns 10, no matter what binding <tt>x</tt> has in the local namespace:

```slate
define: #x -> 4.
[| x | x: 5. `(x `unquote + 5) evaluate] do.
```


Slate can sandbox via constructing a fresh namespace and evaluating within it, but this mechanism is not strongly secure yet.


## Smalltalk


```smalltalk
[ 4 + 5 ] value.
```

Evaluating an expression without bindings:

```smalltalk
e := ' 123 degreesToRadians sin '.
Transcript show: (Compiler evaluate: e) .
```

To get local bindings (x, y),
evaluate an expression which yields a block given as a string, then call the resulting block:

```smalltalk
e := '[ :x :y | (x*x + (y*y)) sqrt ]'.
Transcript show: ((Compiler evaluate: e) value: 3 value: 4).
```

this could be wrapped into a utility, which expects the names to bind as argument, if required.


## SNOBOL4


The built in function eval() evaluates SNOBOL4 expressions and returns the value.  The expression is evaluated in the current environment and has access to then-current variables.


```snobol4
     expression = "' page ' (i + 1)"
     i = 7
     output = eval(expression)
end
```


```txt
 page 8
```


The built in function code() compiles complete SNOBOL4 source statements, or even complete programs.  
The compiled program is returned (as a value of type CODE), and when executed the program is executed in the then-current environment and has access to the then-current variables.  
Labels in the compiled program are added to the current program.  
Programs of type CODE are executed by a variant of the goto clause:


```SNOBOL4
     compiled = code(' output = "Hello, world."')  :s<compiled>
end
```


When passing programs to code(), semicolons are used to separate lines.  

The calling (already-compiled) program can call, for example, functions that are defined in the code compiled at runtime, and can include gotos to labels only defined in the code compiled at runtime.  
Likewise, the code compiled at runtime has access to not just variables, but also files, functions, etc., that are in the already-compiled program.

=={{Header|Sparkling}}==

In Sparkling, the standard library provides functions to compile expressions and statements into functions. Each such function is considered a different top-level program, running in the execution context of it's "parent" program (i. e. the piece of code from within which it was created). Consequently, functions compiled at runtime share their environment (e. g. all globals) with their parent program.

Compiled expressions and statements can take arbitrary arguments and return values to the caller. As with any function, the expression or statement being compiled can refer to its arguments using the <tt>#</tt> prefix operator.

An expression always "returns" a value (i. e. evaluates to one) to the caller. Basically, compiling an expression is semantically (and syntactically) equivalent with creating a function with no declared arguments of which the body consists of a single <tt>return</tt> statement, returning the expression.


### Evaluating expressions


### =Simple=


```sparkling
let fn = exprtofn("13 + 37");
fn() // -> 50
```



### =With arguments=


```sparkling
let fn = exprtofn("#0 * #1");
fn(3, 4) // -> 12
```



### Evaluating statements


```sparkling
let fn = compile("for (var i = 0; i < 10; i++) { print(i); }");
fn(); // result: 0 1 2 3 4 5 6 7 8 9
```



## Tcl


### Simple Evaluation

Evaluation in the current interpreter:

```tcl
set four 4
set result1 [eval "expr {$four + 5}"]           ;# string input

set result2 [eval [list expr [list $four + 5]]] ;# list input
```



### Evaluation in a restricted context

Tcl handles sandboxing by creating new interpreters. Each interpreter is strongly isolated from all other interpreters except in that the interpreter that creates a sub-interpreter retains management control over that “slave” interpreter. The exact capabilities exposed in the slave are controlled by what commands exist in it; commands in the slave may be aliases for other commands in the master interpreter, which allows for trapping into a more highly authorized context (which can be considered analogous to a system call to an OS kernel).

```tcl
# Create an interpreter with a default set of restrictions
interp create -safe restrictedContext

# Our secret variable
set v "secret"

# Allow some guarded access to the secret from the restricted context.
interp alias restrictedContext doubleSecret {} example
proc example {} {
    global v
    lappend v $v
    return [llength $v]
}

# Evaluate a script in the restricted context
puts [restrictedContext eval {
    append v " has been leaked"
    catch {file delete yourCriticalFile.txt} ;# Will be denied!
    return "there are [doubleSecret] words in the secret: the magic number is [expr {4 + 5}]"
}];       # --> there are 2 words in the secret: the magic number is 9
puts $v;  # --> secret secret
```

As can be seen, the result of the overall evaluation is the same as the result of the evaluation in the slave.

Note that with providing values ''to'' the restricted context, it is normal to do this by providing an alias/trap command in the restricted context to allow the script to pick up the value when it wants it. Although the value could also have been provided by setting a variable in the restricted context, this is fairly unusual in practice. The example above shows how this might be done with the result of the <code>doubleSecret</code> command.


### Evaluation within limits

Even stronger protection of the master interpreter is available from Tcl 8.5 onwards through the setting of resource limits on the slaves. 
These allow the master to prevent the evaluated script from going berserk:

```tcl
set i [interp create]
interp limit $i commands -value [expr [$i eval info cmdcount]+20] -granularity 1
interp eval $i {
    set x 0
    while {1} { # Infinite loop! Bwahahahaha!
        puts "Counting up... [incr x]"
    }
}
```

{{out}} (the last line is an error message):

```txt
Counting up... 1
Counting up... 2
Counting up... 3
Counting up... 4
Counting up... 5
Counting up... 6
Counting up... 7
Counting up... 8
Counting up... 9
Counting up... 10
command count limit exceeded
```


=={{header|TI-89 BASIC}}==

The function <code>expr(<var>string</var>)</code> evaluates the string as an expression. It is evaluated in the environment of the calling program (it can see local variables).

The <code>Exec <var>string</var>, <var>args...</var></code> statement executes arbitrary [[68k]] machine code (and is thus entirely unsafe).

TODO: Is there a way to execute statements as well as evaluate expressions? [[Category:TI-89 BASIC examples needing attention]]


## UNIX Shell

<tt>eval</tt> is the command to use:

```bash
$ a=42
$ b=a
$ eval "echo \$$b"
42
```



## Ursa

The eval statement in Ursa takes a string and evaluates it as a command, redirecting the console to the specified I/O device.

```ursa
# writes hello world to the console
eval "out \"hello world\" endl console" console

```



## zkl

In zkl, the compiler is part of the language and compiling a chunk of code returns an executable (which how the REPL works), so 
```zkl
Compiler.Compiler.compileText(
     "fcn f(text){text.len()}").f("foobar")
//-->6
```

All language constructs are allowed, the only sand boxing is the new code can only touch global resources or items explicitly passed in ("foobar" in the example).

