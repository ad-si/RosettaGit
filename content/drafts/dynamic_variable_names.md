+++
title = "Dynamic variable names"
description = ""
date = 2019-10-17T19:29:04Z
aliases = []
[extra]
id = 4297
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}
{{omit From|ACL2}}
{{omit From|Ada}}
{{omit from|AmigaE}}
{{omit from|AWK}}
{{omit from|ALGOL 68}}
{{omit from|C}}
{{omit from|C++}}
{{omit from|D}}
{{omit from|Delphi}}
{{omit from|Fortran}}
{{omit from|Free Pascal}}
{{omit from|GUISS}}
{{omit from|Java}}
{{omit from|Lily}}
{{omit from|Locomotive Basic}}
{{omit from|Metafont}}
{{omit from|NetRexx}}
{{omit from|Octave}}
{{omit from|Pascal}}
{{omit from|PureBasic}}
{{omit from|Rust}}
{{omit from|Swift}}
{{omit from|zkl}}
{{omit from|ZX Spectrum Basic}}

;Task:
Create a variable with a user-defined name.

The variable name should ''not'' be written in the program text, but should be taken from the user dynamically.


;See also
*   [[Eval in environment]] is a similar task.





## APL


```APL

      is←{ t←⍵ ⋄ ⎕this⍎⍺,'←t' } ⍝⍝ the 'Slick Willie' function ;)
      'test' is ⍳2 3
      test
 1 1  1 2  1 3
 2 1  2 2  2 3

```



## AutoHotkey


```AutoHotkey
InputBox, Dynamic, Variable Name
%Dynamic% = hello
ListVars
MsgBox % %dynamic%  ; says hello
```



## BASIC

{{works with|Beta BASIC|3.0}},
{{works with|SAM BASIC}}

```basic
10 INPUT "Enter a variable name", v$
20 KEYIN "LET "+v$+"=42"
```



## Batch File


```DOS
@echo off
setlocal enableDelayedExpansion

set /p "name=Enter a variable name: "
set /p "value=Enter a value: "

::Create the variable and set its value
set "%name%=%value%"

::Display the value without delayed expansion
call echo %name%=%%%name%%%

::Display the value using delayed expansion
echo %name%=!%name%!
```



## BBC BASIC


```bbcbasic
      INPUT "Enter a variable name: " name$
      INPUT "Enter a numeric value: " numeric$
      dummy% = EVAL("FNassign("+name$+","+numeric$+")")
      PRINT "Variable " name$ " now has the value "; EVAL(name$)
      END

      DEF FNassign(RETURN n, v) : n = v : = 0
```



## Bracmat


```bracmat
( put$"Enter a variable name: "
& get$:?name
&   whl
  ' ( put$"Enter a numeric value: "
    & get$:?numeric:~#
    )
& !numeric:?!name
& put$(str$("Variable " !name " now has the value " !!name \n))
);
```



## C#

{{works with|C sharp|5}}
Not exactly a variable, but ExpandoObject allows adding properties at runtime.

```c#
using System;
using System.Dynamic;
using System.Collections.Generic;

public class Program
{
    public static void Main()
    {
        string varname = Console.ReadLine();
        //Let's pretend the user has entered "foo"
        dynamic expando = new ExpandoObject();
        var map = expando as IDictionary<string, object>;
        map.Add(varname, "Hello world!");

        Console.WriteLine(expando.foo);
    }
}
```

{{out}}

```txt

Hello world!

```



## Clojure


```clojure
(eval `(def ~(symbol (read)) 42))
```



## Common Lisp

The short answer is this:

```lisp

(setq var-name (read))  ; reads a name into var-name
(set var-name 1)        ; assigns the value 1 to a variable named as entered by the user

```



The academic answer is this:

In Common Lisp, symbol objects name variables; symbols are produced from strings by way of <code>read</code> (general syntax) or <code>intern</code> (specificially retrieving or making a symbol).

Symbols are grouped into ''packages'' — roughly namespaces — and any time symbols are created at runtime it is usually good to explicitly specify what package they are created in, outside of user/developer tools for working from the REPL (interactive mode) where the ''current package'' <code>*package*</code> is appropriate.

Within the standard, every variable is either ''lexical'' or ''special'' (dynamic scope). There is no global lexical environment, so in order to "create a variable", we must either create our own mechanism to remember it for lexical binding in a later evaluation, or create a special variable. It is unspecified what happens when a symbol not lexically bound or declared ''special'' is used as a variable.

Every symbol has a value slot — a field which, roughly, contains its current value ''considered as a special variable''.

Therefore, there are two parts to dynamically creating a variable: we must declare it special, and give it a value. The first part is accomplished by the <code>proclaim</code> function for making declarations at run-time. The second part is simply assigning to the value slot.

```lisp
(defun rc-create-variable (name initial-value)
  "Create a global variable whose name is NAME in the current package and which is bound to INITIAL-VALUE."
  (let ((symbol (intern name)))
    (proclaim `(special ,symbol))
    (setf (symbol-value symbol) initial-value)
    symbol))
```


```lisp
CL-USER> (rc-create-variable "GREETING" "hello")
GREETING

CL-USER> (print greeting)
"hello"
```

Things to note:

* Once a symbol has been declared special, it cannot be used as a lexical variable. Because of this potentially-surprising behavior, it is conventional to give all symbols naming special variables distinguished names, typically by asterisks as in <code>*greeting*</code>, so that lexical variables will not accidentally be given those names.

* Some implementations <!-- XXX citation --> do, to some extent, support global non-special variables; in these, because of the preceding problem, it is better to simply set the value slot and not proclaim it special. However, this may provoke undefined-variable warnings since the compiler or interpreter has no information with which to know the symbol is intended to be a variable.

* Common Lisp, by default, is case-insensitive; however it accomplishes this by canonicalizing read input to uppercase; there is syntax to denote a lower or mixed-case symbol name, <code>|Foo|</code> or <code>F\o\o</code>. <code>intern</code> does not go through the input path (''reader''), so we must provide the name in uppercase to make an "ordinary" variable name.

=={{header|Déjà Vu}}==
In Déjà Vu, variable names are idents, which are completely separate from strings, and cannot easily be created from them. The way around that is to invoke the compiler:

```dejavu
local :var-name !run-blob !compile-string dup concat( ":" !prompt "Enter a variable name: " )
local var-name 42

#Assuming the user types THISISWEIRD, otherwise this'll error
!. THISISWEIRD
```

{{out}}

```txt
Enter a variable name: THISISWEIRD
42
```



## E

In E, there are no global variables, and there is no modification of the local (lexical) environment. However, it is possible to construct a program which binds any given variable name.

```e>def makeNounExpr := <elang:evm.makeNounExpr


def dynVarName(name) {
    def variable := makeNounExpr(null, name, null)

    return e`{

        def a := 1
        def b := 2
        def c := 3

        {
            def $variable := "BOO!"
            [a, b, c]
        }

    }`.eval(safeScope)
}

? dynVarName("foo")
# value: [1, 2, 3]

? dynVarName("b")
# value: [1, "BOO!", 3]

? dynVarName("c")
# value: [1, 2, "BOO!"]
```

It is also possible to capture the environment object resulting from the evaluation of the constructed program and use it later; this is done by <code>bindX</code> in [[Eval in environment#E]] (except for the program being constant, which is independent).


## Elena

Dynamic variables are not supported by the language. But it is possible to set a dynamic property.

ELENA 4.1 :

```elena
import system'dynamic;
import extensions;

class TestClass
{
    object theVariables;

    constructor()
    {
        theVariables := new DynamicStruct()
    }

    closure()
    {
        auto prop := new MessageName(console.write:"Enter the variable name:".readLine());
        (prop.setPropertyMessage())(theVariables,42);

        console.printLine(prop.Printable,"=",(prop.getPropertyMessage())(theVariables)).readChar()
    }
}

public program = new TestClass();
```

{{out}}

```txt

Enter the variable name:a
a=42

```



## Emacs Lisp

A variable is a symbol.  A name can be read from the user as a string and interned to a symbol.


```Lisp
(set (intern (read-string "Enter variable name: ")) 123)
```


This example deliberately doesn't use any temporary variables so their names won't clash with what the user might enter.  A <code>set</code> like this hits any <code>let</code> dynamic binding or buffer-local setting in the usual way.


## Erlang

This task uses functions from [[Eval_in_environment#Erlang| Runtime evaluation]].

```Erlang

-module( dynamic_variable_names ).

-export( [task/0] ).

task() ->
    {ok,[Variable_name]} = io:fread( "Variable name? ",	"~a" ),
    Form = runtime_evaluation:form_from_string( erlang:atom_to_list(Variable_name) ++ "." ),
    io:fwrite( "~p has value ~p~n", [Variable_name, runtime_evaluation:evaluate_form(Form, {Variable_name, 42})] ).

```

{{out}}

```txt

12> dynamic_variable_names:task().
Variable name? Asd
'Asd' has value 42

```



## Factor

By convention, variable names are usually symbols, but because dynamic variables are implemented via implicit association lists, any object can be used as the key for a value. In this case, we use the string the user enters.

```factor

42 readln set

```



## Forth


```forth
s" VARIABLE " pad swap move
." Variable name: " pad 9 + 80 accept
pad swap 9 + evaluate
```

Of course, it is easier for the user to simply type VARIABLE ''name'' at the Forth console.


## FreeBASIC

FreeBASIC is a statically typed, compiled language and so it is not possible to create new variables, dynamically, at run time. However, you can make it look to the user like you are doing so with code such as the following. Ideally, a 'map' should be used for an exercise such as this but, as there isn't one built into FB, I've used a dynamic array instead which is searched linearly for the variable name.


```freebasic
' FB 1.05.0 Win64

Type DynamicVariable
  As String name
  As String value
End Type

Function FindVariableIndex(a() as DynamicVariable, v as String, nElements As Integer) As Integer
  v = LCase(Trim(v))
  For i As Integer = 1 To nElements
    If a(i).name = v Then Return i
  Next
  Return 0
End Function

Dim As Integer n, index
Dim As String v
Cls

Do
 Input "How many variables do you want to create (max 5) "; n
Loop Until n > 0 AndAlso n < 6

Dim a(1 To n) As DynamicVariable
Print
Print "OK, enter the variable names and their values, below"

For i As Integer = 1 to n
  Print
  Print "  Variable"; i
  Input "    Name  : ", a(i).name
  a(i).name = LCase(Trim(a(i).name)) ' variable names are not case sensitive in FB
  If i > 0 Then
     index = FindVariableIndex(a(), a(i).name, i - 1)
     If index > 0 Then
       Print "  Sorry, you've already created a variable of that name, try again"
       i -= 1
       Continue For
     End If
  End If
  Input "    Value : ", a(i).value
  a(i).value = LCase(Trim(a(i).value))
Next

Print
Print "Press q to quit"
Do
  Print
  Input "Which variable do you want to inspect "; v
  If v = "q" OrElse v = "Q" Then Exit Do
  index = FindVariableIndex(a(), v, n)
  If index = 0 Then
    Print "Sorry there's no variable of that name, try again"
  Else
    Print "It's value is "; a(index).value
  End If
Loop
End
```


Sample input/output :
{{out}}

```txt

How many variables do you want to create (max 5) ? 3

OK, enter the variable names and their values, below

  Variable 1
    Name  : a
    Value : 1

  Variable 2
    Name  : b
    Value : 2

  Variable 3
    Name  : b
  Sorry, you've already created a variable of that name, try again

  Variable 3
    Name  : c
    Value : 4

Press q to quit

Which variable do you want to inspect ? b
It's value is 2

Which variable do you want to inspect ? c
It's value is 4

Which variable do you want to inspect ? a
It's value is 1

Which variable do you want to inspect ? q

```



## GAP


```gap
# As is, will not work if val is a String
Assign := function(var, val)
	Read(InputTextString(Concatenation(var, " := ", String(val), ";")));
end;
```



## Genyris

The intern function creates a symbol from an arbitrary string. Defvar creates a binding. Weird symbols are quoted with pipe characters.

```genyris
defvar (intern 'This is not a pipe.') 42
define |<weird>| 2009
```



## Go

{{trans|FreeBASIC}}
Go is in the same boat as other statically typed, compiled languages here in that variables cannot be created dynamically at runtime. However, we can use the built-in map type to associate names input at runtime with values which, in practice, is just as good.

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strconv"
    "strings"
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    n := 0
    for n < 1 || n > 5 {
        fmt.Print("How many integer variables do you want to create (max 5) : ")
        scanner.Scan()
        n, _ = strconv.Atoi(scanner.Text())
        check(scanner.Err())
    }
    vars := make(map[string]int)
    fmt.Println("OK, enter the variable names and their values, below")
    for i := 1; i <= n; {
        fmt.Println("\n  Variable", i)
        fmt.Print("    Name  : ")
        scanner.Scan()
        name := scanner.Text()
        check(scanner.Err())
        if _, ok := vars[name]; ok {
            fmt.Println("  Sorry, you've already created a variable of that name, try again")
            continue
        }
        var value int
        var err error
        for {
            fmt.Print("    Value : ")
            scanner.Scan()
            value, err = strconv.Atoi(scanner.Text())
            check(scanner.Err())
            if err != nil {
                fmt.Println("  Not a valid integer, try again")
            } else {
                break
            }
        }
        vars[name] = value
        i++
    }
    fmt.Println("\nEnter q to quit")
    for {
        fmt.Print("\nWhich variable do you want to inspect : ")
        scanner.Scan()
        name := scanner.Text()
        check(scanner.Err())
        if s := strings.ToLower(name); s == "q" {
            return
        }
        v, ok := vars[name]
        if !ok {
            fmt.Println("Sorry there's no variable of that name, try again")
        } else {
            fmt.Println("It's value is", v)
        }
    }
}
```


{{out}}
Sample input/output:

```txt

How many integer variables do you want to create (max 5) : 3
OK, enter the variable names and their values, below

  Variable 1
    Name  : pip
    Value : 1

  Variable 2
    Name  : squeak
    Value : 2

  Variable 3
    Name  : pip
  Sorry, you've already created a variable of that name, try again

  Variable 3
    Name  : wilfred
    Value : 3

Enter q to quit

Which variable do you want to inspect : squeak
It's value is 2

Which variable do you want to inspect : auntie
Sorry there's no variable of that name, try again

Which variable do you want to inspect : wilfred
It's value is 3

Which variable do you want to inspect : q

```



## Groovy

Solution:

```groovy
def varname = 'foo'
def value = 42

new GroovyShell(this.binding).evaluate("${varname} = ${value}")

assert foo == 42
```



## Haskell


```haskell
data Var a = Var String a deriving Show
main = do
    putStrLn "please enter you variable name"
    vName <- getLine
    let var = Var vName 42
    putStrLn $ "this is your variable: " ++ show var
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
if *arglist = 0 then stop("Provide the names of variables in the argument list")
&dump := 1                       # dump program state information and variables after run
every variable(!arglist) := 1    # set each user specified variable name in arglist to 1
end
```

Note: that Unicon extends variable to allow access to variables in other co-expressions and in calling procedures


## J


This code was written for J6.02. In J8.04 you will need to replace require'misc' with require'general/misc/prompt'


```j
require 'misc'
(prompt 'Enter variable name: ')=: 0
```


For example:
```j
   require 'misc'
   (prompt 'Enter variable name: ')=: 0
Enter variable name: FOO
   FOO
0
```

Or, if the name is defined in the variable 'userDefined'

```j
   userDefined=: 'BAR'
   (userDefined)=: 1
   BAR
1
```



## JavaScript


```javascript
var varname = 'foo';  // pretend a user input that
var value = 42;
eval('var ' + varname + '=' + value);
```

Alternatively, without using eval:

```javascript
var varname = prompt('Variable name:');
var value = 42;
this[varname] = value;
```



## jq

jq does not have variables in the usual sense, but in practice the key/value pairs of JSON objects can be used as variable/value bindings.  Using this approach, the given task can be accomplished using the following program:


```jq
"Enter a variable name:",
(input as $var
 | ("Enter a value:" ,
    (input as $value | { ($var) : $value })))
```


''Transcript''

```txt
$ jq -nrR -f program.jq
Enter a variable name:
abracadabra
Enter a value:
magic
{
  "abracadabra": "magic"
}
```



## Julia

{{works with|Julia|0.6}}
Julia has powerful macros:


```julia
print("Insert the variable name: ")

variable   = Symbol(readline(STDIN))
expression = quote
    $variable = 42
    println("Inside quote:")
    @show $variable
end

eval(expression)

println("Outside quote:")
@show variable
println("If I named the variable x:")
@show x
```


{{out}}

```txt
Insert the variable name:
x

Inside quote:
x = 42
Outside quote:
variable = :x
x = 42
```



## Kotlin

Kotlin is a statically typed, compiled language and so it is not possible to create new variables, dynamically, at run time. However, you can make it look to the user like you are doing so with code such as the following which uses a map:
{{trans|FreeBASIC}}

```scala
// version 1.1.4

fun main(args: Array<String>) {
    var n: Int
    do {
        print("How many integer variables do you want to create (max 5) : ")
        n = readLine()!!.toInt()
    }
    while (n < 1 || n > 5)

    val map = mutableMapOf<String, Int>()
    var name: String
    var value: Int
    var i = 1
    println("OK, enter the variable names and their values, below")
    do {
        println("\n  Variable $i")
        print("    Name  : ")
        name = readLine()!!
        if (map.containsKey(name)) {
            println("  Sorry, you've already created a variable of that name, try again")
            continue
        }
        print("    Value : ")
        value = readLine()!!.toInt()
        map.put(name, value)
        i++
    }
    while (i <= n)

    println("\nEnter q to quit")
    var v: Int?
    while (true) {
        print("\nWhich variable do you want to inspect : ")
        name = readLine()!!
        if (name.toLowerCase() == "q") return
        v = map[name]
        if (v == null) println("Sorry there's no variable of that name, try again")
        else println("It's value is $v")
    }
}
```

Sample input/output:
{{out}}

```txt

How many integer variables do you want to create (max 5) : 3
OK, enter the variable names and their values, below

  Variable 1
    Name  : faith
    Value : 1

  Variable 2
    Name  : hope
    Value : 2

  Variable 3
    Name  : hope
  Sorry, you've already created a variable of that name, try again

  Variable 3
    Name  : charity
    Value : 3

Enter q to quit

Which variable do you want to inspect : chastity
Sorry there's no variable of that name, try again

Which variable do you want to inspect : charity
It's value is 3

Which variable do you want to inspect : q

```



## Lasso

Thread vars in Lasso 9 can have dynamic names, but local variables cannot.

The example below outputs a random decimal that was assigned to the variable name entered as part of the GET params.

```Lasso
local(thename = web_request->param('thename')->asString)
if(#thename->size) => {^
	var(#thename = math_random)
	var(#thename)
else
	'<a href="?thename=xyz">Please give the variable a name!</a>'
^}
```



## Lingo


```lingo
-- varName might contain a string that was entered by a user at runtime

-- A new global variable with a user-defined name can be created at runtime like this:
(the globals)[varName] = 23 -- or (the globals).setProp(varName, 23)

-- An new instance variable (object property) with a user-defined name can be created at runtime like this:
obj[varName] = 23 -- or obj.setProp(varName, 23)
```



## Logtalk

Logtalk objects can be create or compiled such that new predicates can be added at runtime. A simple example:

```logtalk

| ?- create_object(Id, [], [set_logtalk_flag(dynamic_declarations,allow)], []),
     write('Variable name:  '), read(Name),
     write('Variable value: '), read(Value),
     Fact =.. [Name, Value],
     Id::assertz(Fact).

Variable name:  foo.
Variable value: 42.
Id = o1,
Name = foo,
Value =  42,
Fact = foo(42).

?- o1::current_predicate(foo/1).
true.

| ?- o1::foo(X).
X = 42.

```



## Lua


```lua
_G[io.read()] = 5 --puts 5 in a global variable named by the user
```



## Logo


```logo
? make readword readword
julie
12
? show :julie
12
```



## M2000 Interpreter


```M2000 Interpreter

Module DynamicVariable {
      input "Variable Name:", a$
      a$=filter$(a$," ,+-*/^~'\({=<>})|!$&"+chr$(9)+chr$(127))
      While a$ ~ "..*"  {a$=mid$(a$, 2)}
      If len(a$)=0 then Error "No name found"
      If chrcode(a$)<65 then Error "Not a Valid name"
      Inline a$+"=1000"
      Print eval(a$)=1000
      \\ use of a$ as pointer to variable
      a$.+=100
      Print eval(a$)=1100
      \\ list of variables
      List
}
Keyboard "George"+chr$(13)
DynamicVariable

```



## M4


```M4
Enter foo, please.
define(`inp',esyscmd(`echoinp'))
define(`trim',substr(inp,0,decr(len(inp))))
define(trim,42)
foo
```


DOS batch file echoinp.bat:

```txt

@echo off
set /p Input=
echo %Input%

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
varname = InputString["Enter a variable name"];
varvalue = InputString["Enter a value"];
ReleaseHold[ Hold[Set["nameholder", "value"]] /. {"nameholder" -> Symbol[varname], "value" -> varvalue}];
Print[varname, " is now set to ", Symbol[varname]]
```

{{out|Example output}}

```txt
-> testvar is now set to 86
```



## Maxima


```maxima
/* Use :: for indirect assignment */
block([name: read("name?"), x: read("value?")], name :: x);
```



## min

{{works with|min|0.19.3}}

```min
42 "Enter a variable name" ask define
```



## MUMPS

This is done in immediate mode so you can see the variable is created, although you will have to reference it through the indirection operator, "@".

```mumps>USER
KILL ;Clean up workspace

USER>WRITE ;show all variables and definitions

USER>READ "Enter a variable name: ",A
Enter a variable name: GIBBERISH
USER>SET @A=3.14159

USER>WRITE

A="GIBBERISH"
GIBBERISH=3.14159
```



## Nim

Nim is a compiled language, with powerful Templating and Macros, which are compile-time rather than run-time.

This solution emulates dynamic variables by mapping a string to a pointer to a variable (using a table).

```Nim
import tables

var
  theVar: int = 5
  varMap = initTable[string, pointer]()

proc ptrToInt(p: pointer): int =
  result = cast[ptr int](p)[]

proc main() =
  write(stdout, "Enter a var name: ")
  let sVar = readLine(stdin)
  varMap.add($svar, theVar.addr)
  echo "Variable ", sVar, " is ", ptrToInt(varMap[$sVar])

when isMainModule:
  main()
```

{{out}}

```txt
Enter a var name: varZ
Variable varZ is 5
```



## Octave


```octave
varname = input ("Enter variable name: ", "s");
value = input ("Enter value: ", "s");
eval([varname,"=",value]);
```



## Oforth


```oforth
: createVar(varname)
   "tvar: " varname + eval ;

"myvar" createVar

12 myvar put
myvar at .
```



## PARI/GP


```parigp
eval(Str(input(), "=34"))
```



## Perl


```perl
print "Enter a variable name: ";
$varname = <STDIN>; # type in "foo" on standard input
chomp($varname);
$$varname = 42; # when you try to dereference a string, it will be
                # treated as a "symbolic reference", where they
                # take the string as the name of the variable
print "$foo\n"; # prints "42"
```

If you are operating in a strict environment, this isn't possible.  You need to use 'eval' in this case

```perl
use strict;

print "Enter a variable name: ";
my $foo;
my $varname = <STDIN>; # type in "foo" on standard input
chomp($varname);
my $varref = eval('\$' . $varname);
$$varref = 42;
print "$foo\n"; # prints "42"
```



## Perl 6

{{works with|Rakudo|2018.03}}
You can [https://docs.perl6.org/language/packages#Interpolating_into_names interpolate strings as variable names]:


```perl6
our $our-var = 'The our var';
my  $my-var  = 'The my var';

my $name  = prompt 'Variable name: ';
my $value = $::('name'); # use the right sigil, etc

put qq/Var ($name) starts with value ｢$value｣/;

$::('name') = 137;

put qq/Var ($name) ends with value ｢{$::('name')}｣/;

```



## Phix

Not possible, but it is fairly easy to fake:

```Phix
constant globals = new_dict()

while 1 do
    string name = prompt_string("Enter name or press Enter to quit:")
    if length(name)=0 then exit end if
    integer k = getd_index(name,globals)
    if k=0 then
        string data = prompt_string("No such name, enter a value:")
        setd(name,data,globals)
    else
        string data = prompt_string(sprintf("Already exists, new value[%s]:",{getd(name,globals)}))
        if length(data) then
            setd(name,data,globals)
        end if
    end if
end while
```

{{Out}}

```txt

Enter name or press Enter to quit:fred
No such name, enter a value:35
Enter name or press Enter to quit:fred
Already exists, new value[35]:
Enter name or press Enter to quit:james
No such name, enter a value:1
Enter name or press Enter to quit:fred
Already exists, new value[35]:
Enter name or press Enter to quit:james
Already exists, new value[1]:
Enter name or press Enter to quit:

```



## PHP


```php
<?php
$varname = rtrim(fgets(STDIN)); # type in "foo" on standard input
$$varname = 42;
echo "$foo\n"; # prints "42"
?>
```



## PicoLisp


```PicoLisp
(de userVariable ()
   (prin "Enter a variable name: ")
   (let Var (line T)                                  # Read transient symbol
      (prin "Enter a value: ")
      (set Var (read))                                # Set symbol's value
      (println 'Variable Var 'Value (val Var)) ) )    # Print them
```

{{out}}

```txt
Enter a variable name: Tom
Enter a value: 42
Variable "Tom" Value 42
-> 42
```



## PowerShell


```powershell
$variableName = Read-Host
New-Variable $variableName 'Foo'
Get-Variable $variableName
```



## ProDOS

<lang>editvar /newvar /value=a /userinput=1 /title=Enter a variable name:
editvar /newvar /value=b /userinput=1 /title=Enter a variable title:
editvar /newvar /value=-a- /title=-b-
```



## Python

{{works with|Python|2.x}}

```python>>>
 name = raw_input("Enter a variable name: ")
Enter a variable name: X
>>> globals()[name] = 42
>>> X
42
```

{{works with|Python|3.x}}

```python>>>
 name = input("Enter a variable name: ")
Enter a variable name: X
>>> globals()[name] = 42
>>> X
42
```

Note: most of the time when people ask how to do this on newsgroups and other forums, on investigation, it is found that a neater solution is to '''map name to value in a dictionary'''.


## R


```R
# Read the name in from a command prompt
varname <- readline("Please name your variable >")
# Make sure the name is valid for a variable
varname <- make.names(varname)
message(paste("The variable being assigned is '", varname, "'"))
# Assign the variable (with value 42) into the user workspace (global environment)
assign(varname, 42)
#Check that the value has been assigned ok
ls(pattern=varname)
get(varname)
```



## Racket

This works on the Racket REPL:


```Racket

-> (begin (printf "Enter some name: ")
          (namespace-set-variable-value! (read) 123))
Enter some name: bleh
-> bleh
123

```



## REBOL


```REBOL
REBOL [
	Title: "Dynamic Variable Name"
	URL: http://rosettacode.org/wiki/Dynamic_variable_names
]

; Here, I ask the user for a name, then convert it to a word and
; assign the value "Hello!" to it. To read this phrase, realize that
; REBOL collects terms from right to left, so "Hello!" is stored for
; future use, then the prompt string "Variable name? " is used as the
; argument to ask (prompts user for input). The result of ask is
; converted to a word so it can be an identifier, then the 'set' word
; accepts the new word and the string ("Hello!") to be assigned.

set  to-word  ask "Variable name? "  "Hello!"
```

{{out|Session output}}

```txt
Variable name? glister
== "Hello!"
>> glister
== "Hello!"
```



## Retro


```Retro
: newVariable: ( "- )
  getToken header 0 , ;

newVariable: foo
```

Or:

```Retro
: newVariable: ( "- )
  create 0 , ;

newVariable: foo
```



## REXX

Checks could've been made to:
:::*   check for the minimum number of arguments
:::*   check for a legitimate REXX variable name

```rexx
/*REXX program demonstrates the use of dynamic variable names & setting a val.*/
parse arg newVar newValue
say 'Arguments as they were entered via the command line: '   newVar    newValue
say
call value newVar, newValue
say 'The newly assigned value (as per the VALUE bif)------' newVar value(newVar)
                                       /*stick a fork in it,  we're all done. */
```

'''output'''   for the input:   <tt> abc   456 </tt>

```txt

Arguments as they were entered via the command line = abc 45678.1

The newly assigned value (as per the VALUE bif)------ abc 45678.1

```



## RLaB

In RLaB all the objects are located in a global list $$. To create a variable dynamically, one writes a new entry into the global list. Consider the following example:

```RLaB>>
 s = "myusername"
myusername
>> $$.[s] = 10;
>> myusername
  10
```



## Ring


```ring

See "Enter the variable name: " give cName eval(cName+"=10")
See "The variable name = " + cName + " and the variable value = " + eval("return "+cName) + nl

```

Output

```ring

Enter the variable name: test
The variable name = test and the variable value = 10

```



## Ruby


```ruby
p "Enter a variable name"
x = "@" + gets.chomp!
instance_variable_set x, 42
p "The value of #{x} is #{instance_variable_get x}"

```

{{out|Example output}}
 "Enter a variable name"
 hello
 "The value of @hello is 42"


## Scheme


```scheme>=
 (define (create-variable name initial-val)
     (eval `(define ,name ,initial-val) (interaction-environment)))

=> (create-variable (read) 50)
<hello

=> hello
50
```



## Sidef

It is not possible to create a new lexical variable at run-time, but there are other various ways to do something similar.


```ruby
var name = read("Enter a variable name: ", String);     # type in 'foo'

class DynamicVar(name, value) {
    method init {
        DynamicVar.def_method(name, ->(_) { value })
    }
}

var v = DynamicVar(name, 42);       # creates a dynamic variable
say v.foo;                          # retrieves the value
```



## Slate

Slate symbols are objects that name methods and slots. "Variable definition" is like defining a method which holds the value of a slot, and "variable access" is just method-call to get that value back.

```slate
define: #name -> (query: 'Enter a variable name: ') intern. "X"
define: name -> 42.
X print.
```



## Smalltalk

{{works with|Pharo}}
Define a block-temporary variable with name specified by user input.
Set that variable to 42.
Print that variable's name and value.

```smalltalk
| varName |
varName := FillInTheBlankMorph
	request: 'Enter a variable name'.
Compiler
	evaluate: '| ', varName, ' | ', varName, ' := 42.
	Transcript
		show: ''value of ', varName, ''';
		show: '' is '';
		show: ', varName.
```



## SNOBOL4

Indirect string reference of variables is a basic feature of Snobol, using the $ operator. trim( ) is needed for Snobol4+.

```SNOBOL4
*       # Get var name from user
        output = 'Enter variable name:'
        invar = trim(input)

*       # Get value from user, assign
        output = 'Enter value:'
        $invar = trim(input)

*       Display
        output = invar ' == ' $invar
end
```

{{out}}

```txt
Enter variable name:
pi
Enter value:
3.14159
pi == 3.14159
```



## Stata

Here a scalar variable is created, but one could create a dataset variable, a matrix... Notice the name of the variable is not "s", but the name stored in the global macro "s".


```stata
display "Name?" _request(s)
scalar $s=10
display $s
```



## Tcl


```Tcl
puts "Enter a variable name:"
gets stdin varname
set $varname 42
puts "I have set variable $varname to [set $varname]"
```

Note that it is more normal to use the user's name to index into a Tcl associative array, as the syntax gets easier to work with in that case:

```tcl
puts -nonewline "Enter an element name: "; flush stdout
gets stdin elemname
set ary($elemname) [expr int(rand()*100)]
puts "I have set element $elemname to $ary($elemname)"
```

Another common method for working with dynamic variables is to make an alias to the variable with a fixed name:

```tcl
puts -nonewline "Enter a variable name: "; flush stdout
gets stdin varname
upvar 0 $varname v; # The ‘0’ for “current scope”
set v [expr int(rand()*100)]
puts "I have set variable $varname to $v (see for yourself: [set $varname])"
```


=={{header|TI-89 BASIC}}==

```ti89b
Local varName,value
InputStr "Variable name", varName
Prompt value
value → #varName
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
ASK "Enter variablename": name=""
ASK "Enter value": value=""
TRACE +@name
@name=$value
PRINT @name

```

Output:

```txt

Enter variablename >test
Enter value >Hello World!
TRACING     Scratch-Datei -*TUSTEP.EDT
   5    00  TRACE +@name
test         = Hello World!
Hello World!

```



## UNIX Shell


```bash
read name
declare $name=42
echo "${name}=${!name}"
```



## zkl

zkl doesn't support adding vars to an existing class but can create a new class with new vars:

```zkl
vname:="foo";  // or vname:=ask("var name = ");
klass:=Compiler.Compiler.compileText("var %s=123".fmt(vname))(); // compile & run the constructor
klass.vars.println();
klass.foo.println();
klass.setVar(vname).println();  // setVar(name,val) sets the var
```

{{out}}

```txt

L(L("foo",123))
123
123

```



## Zsh


```zsh
read name
typeset $name=42
```

