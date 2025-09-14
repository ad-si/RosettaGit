+++
title = "Undefined values"
description = ""
date = 2019-10-14T10:20:31Z
aliases = []
[extra]
id = 4997
[taxonomies]
categories = ["task", "Programming language concepts"]
tags = []
languages = [
  "actionscript",
  "ada",
  "algol_68",
  "arturo",
  "basic",
  "bbc_basic",
  "c",
  "common_lisp",
  "csharp",
  "d",
  "delphi",
  "e",
  "erlang",
  "erre",
  "fortran",
  "freebasic",
  "gap",
  "go",
  "haskell",
  "icon",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "logo",
  "lolcode",
  "lua",
  "mathematica",
  "mumps",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "smalltalk",
  "tcl",
  "unicon",
  "unix_shell",
  "zkl",
]
+++

## Task

{{task|Programming language concepts}}For languages which have an explicit notion of an undefined value, identify and exercise those language's mechanisms for identifying and manipulating a variable's value's status as being undefined.
<br/><br/>


## ActionScript

ActionScript has a special '''undefined''' value which applies to untyped variables and properties of dynamic classes which have not been initialized.

```actionscript
var foo; // untyped
var bar:*; // explicitly untyped

trace(foo + ", " + bar); // outputs "undefined, undefined"

if (foo == undefined)
    trace("foo is undefined"); // outputs "foo is undefined"
```


ActionScript also has a '''null''' value: see [[Null object#ActionScript]].


## Ada

Ada language provides attribute 'Valid used to check if a scalar value is valid. An invalid value may appear as a result of unchecked type conversion, input, access through a dangling pointer etc.
The language also provides the configuration pragma Normalize_Scalars which instructs the compiler to initialize uninitialized scalars with values, which when possible, would have the attribute 'Valid false.
This pragma is required to be applied to the whole partition, which would require recompilation of the run-time library. For this reason, the presented example uses another pragma Initialize_Scalars.
This one has the effect similar to Normalize_Scalars, but is [[GNAT]]-specific:

```Ada

pragma Initialize_Scalars;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Invalid_Value is
   type Color is (Red, Green, Blue);
   X : Float;
   Y : Color;
begin
   if not X'Valid then
      Put_Line ("X is not valid");
   end if;
   X := 1.0;
   if X'Valid then
      Put_Line ("X is" & Float'Image (X));
   end if;
   if not Y'Valid then
      Put_Line ("Y is not valid");
   end if;
   Y := Green;
   if Y'Valid then
      Put_Line ("Y is " & Color'Image (Y));
   end if;
end Invalid_Value;

```

Sample output:

```txt

X is not valid
X is 1.00000E+00
Y is not valid
Y is GREEN

```

Note that some types are always initialized valid. E.g. pointers, which are formally non-scalar, are initialized null. Another example are scalar types of which representation does not leave free bit patterns for invalid value. For instance a 32-bit integer will likely valid under any circumstances.


## ALGOL 68

Note: Some implementations (eg [[ALGOL 68C]]) also have a procedure named ''undefined'' that is called to indicated that the behaviour at a particular point in a program is unexpected, undefined, or non-standard.

```algol68
MODE R = REF BOOL;
R r := NIL;

MODE U = UNION(BOOL, VOID);
U u := EMPTY;

IF r IS R(NIL) THEN
  print(("r IS NIL", new line))
ELSE
  print(("r ISNT NIL", new line))
FI;

CASE u IN
  (VOID):print(("u is EMPTY", new line))
  OUT    print(("u isnt EMPTY", new line))
ESAC
```

Output:

```txt

r IS NIL
u is EMPTY

```



## Arturo



```arturo
undef null

print undef
```


```txt
null
```



## BASIC

Classic BASIC does have the concept of un-initialised or undefined variables.


## BBC BASIC

A scalar variable (numeric or string) cannot have an 'undefined' value; if an attempt is made to read a variable which has never been defined a 'No such variable' error results.  By trapping errors this condition can be detected:

```bbcbasic
      ok% = TRUE
      ON ERROR LOCAL IF ERR<>26 REPORT : END ELSE ok% = FALSE
      IF ok% THEN
        PRINT variable$
      ELSE
        PRINT "Not defined"
      ENDIF
      RESTORE ERROR
```


Arrays and structures however '''can''' have an undefined state; for example after having been declared as LOCAL or PRIVATE but before being defined using DIM.  This condition can be detected and manipulated:

```bbcbasic
      PROCtest
      END

      DEF PROCtest
      LOCAL array()
      IF !^array() < 2 PRINT "Array is undefined"
      DIM array(1,2)
      IF !^array() > 1 PRINT "Array is defined"

      !^array() = 0 : REM Set array to undefined state
      ENDPROC
```



## C

C has no specific undefined value.

A function that wants to return an undefined value might indicate failure. Sometimes -1 is failure, sometimes 0 is failure, sometimes 0 is success; one has to look up the documentation to know exactly which. For a pointer, the undefined value is often [[Null object#C|pointer 0, the NULL pointer]].

C programs can also read garbage values from uninitialized memory. There is no way to test for these garbage values, because they might equal anything.


```c
#include <stdio.h>
#include <stdlib.h>

int main()
{
	int junk, *junkp;

	/* Print an unitialized variable! */
	printf("junk: %d\n", junk);

	/* Follow a pointer to unitialized memory! */
	junkp = malloc(sizeof *junkp);
	if (junkp)
		printf("*junkp: %d\n", *junkp);
	return 0;
}
```


## C#
In C# it's important to see the difference between reference and value types. For reference types (class instances) there is <code>null</code> as a general undefined reference.

```csharp>string foo = null;</lang

Dereferencing a null reference will throw a <code>NullReferenceException</code>.

This can't be used normally for value types (<code>int</code>, <code>double</code>, <code>DateTime</code>, etc.) since they are no ''references,'' so the following is a compiler error:

```csharp>int i = null;</lang

With .NET 2.0 there is an additional <code>Nullable<T></code> structure which enables those semantics for value types as well:

```c#
int? answer = null;
if (answer == null) {
    answer = 42;
}
```

There is a bit syntactic sugar involved here. The <code>?</code> after the type name signals the compiler that it's a ''nullable'' type. This only works for value types since reference types are nullable due to their very nature.

But since value types still can't actually ''have'' a <code>null</code> value this gets converted into the following code by the compiler:

```csharp>Nullable<int> answer = new Nullable<int
();
if (!answer.HasValue) {
    answer = new Nullable<int>(42);
}
```

So it's a little compiler magic but in the end works just as one would expect.


## Common Lisp


Common Lisp programs can introspect over the definedness of dynamic variables, but not of lexical variables. In the compilation of a lexical scope, lexical variables can be optimized, and the symbols disappear, replaced by machine code. Variables can be optimized away entirely, making questions of definedness moot. Dynamic variables (also known as special variables), however, carry a run-time association with a symbol.

In Lisp, there are three possibilities with regard to a dynamic variable. It may exist, and have a value. It may exist, but not have a value. Or it may not exist at all. These situations can be probed with certain functions that take a symbol as an argument:


```lisp

  ;; assumption: none of these variables initially exist

  (defvar *x*)    ;; variable exists now, but has no value
  (defvar *y* 42) ;; variable exists now, and has a value

  (special-variable-p '*x*) -> T  ;; Symbol *x* names a special variable
  (boundp '*x*) -> NIL            ;; *x* has no binding
  (boundp '*y*) -> T

  (special-variable-p '*z*) -> NIL ;; *z* does not name a special variable
```


Furthermore, a variable which is bound can be made unbound:


```lisp

  (makunbound '*y*) ;; *y* no longer has a value; it is erroneous to evaluate *y*

  (setf *y* 43)     ;; *y* is bound again.
```


By contrast, lexical variables never lack a binding. Without an initializer, they are initialized to nil.
The same goes for local re-binding of special variables:


```lisp

  (defvar *dyn*)  ;; special, no binding

  (let (*dyn*     ;; locally scoped override, value is nil
        lex)      ;; lexical, value is nil
    (list (boundp '*dyn*) *dyn* (boundp 'lex) lex))         -> (T NIL NIL NIL)

  (boundp '*global*) -> NIL
```


Here we can see that inside the scope of the let, the special variable has a binding (to the value <code>NIL</code>) and so <code>(boundp '*dyn*)</code> yields <code>T</code>. But <code>boundp</code> does not "see" the lexical variable <code>lex</code>; it reports that <code>lex</code> is unbound.  Local binding constructs never leave a variable without a value, be it dynamic or lexical: both <code>*dyn*</code> and <code>lex</code> evaluate to NIL, but using very different mechanisms.


## D

In D variables are initialized either with an explicit Initializer or are set to the default value for the type of the variable. If the Initializer is void, however, the variable is not initialized. If its value is used before it is set, undefined program behavior will result. "void" initializers can be used to avoid the overhead of default initialization in performance critical code.

```d
void main() {
    // Initialized:
    int a = 5;
    double b = 5.0;
    char c = 'f';
    int[] d = [1, 2, 3];

    // Default initialized:
    int aa; // set to 0
    double bb; // set to double.init, that is a NaN
    char cc; // set to 0xFF
    int[] dd; // set to null
    int[3] ee; // set to [0, 0, 0]

    // Undefined (contain garbage):
    int aaa = void;
    double[] bbb = void;
    int[3] eee = void;
}
```



## Delphi

Delphi and its dialects don't have an undefined notion for all variables, but implement the notion of the keyword nil that is untyped, yet compatible with all pointer types and object references as well as interfaces. No compatibility is given for non-referenced data types like integers, enums and records - as well as string types (Some exceptions exist due to some compiler magic for those types).

For Referenced data types like pointers, classes and interfaces a reference can be explicitely set to undefined by assigning the NIL value for it. No memory management  like garbage collection (except for interfaces) is done.

```delphi
var
    P: PInteger;
begin
    New(P);  //Allocate some memory
    try
        If Assigned(P) Then //...
        begin
            P^ := 42;
        end;
    finally
        Dispose(P); //Release memory allocated by New
    end;
end;
```


If P was a Class only the Assigned function would be available; in addition Dispose would have to be replaced by FreeAndNil or calling the .Free method of the instance. For Interfaces no such last call would be necessary as simple removal of the reference would be sufficient to trigger the Garbage Collector.
=={{header|Déjà Vu}}==
There is no undefined value in Déjà Vu. Instead, trying to access an undefined variable raises an exception.

```dejavu
try:
	bogus
catch name-error:
	!print "There is *no* :bogus in the current context"
	return
!print "You won't see this."
```

If you need to declare a local variable, but don't have a value for it yet, there is the standard function <code>undef</code>, which raises an exception when called but is an actual value that can be passed around and assigned to names.


## E


First, there is the <var>null</var> object, which is simply a predefined object which has no methods (other than those every object has). <var>null</var> is generally used as a default return value and as a “no value” marker. <var>null</var> is not included in all object types as in Java; you must explicitly add it (e.g. <code>var foo :nullOk[List]</code> may be a List or null).

There are also <em>broken references</em>. Broken references are generated as the results of failed eventual sends (asynchronous invocations), and also may be constructed using <em>Ref.broken(...)</em>. It is an error to call or send to a broken reference, but it may be passed around like any other value. Broken references may be thought of as “Due to unexpected circumstances, this part of the state of the program is missing; do not proceed.”

Both of the above are values, which may be passed around and stored in variables. On the other hand, there are also ways to have an “undefined” <em>variable.</em> (However, it is not possible for a program to refer to an <em>nonexistent</em> variable; that is a static error which will be reported at the beginning of evaluation.)

Each variable's behavior is defined by a <em>slot</em> object with <code>get</code> and <code>put</code> methods. If the slot throws when invoked, then a program may contain a reference to that variable, but not actually access it. A notable way for the slot to throw is for the slot to itself be a broken reference. This may occur when slots are being passed around as part of metaprogramming or export/import; it is also used in certain control structures. For example:


```e
if (foo == bar || (def baz := lookup(foo)) != null) {
     ...
}
```


The slot for <var>baz</var> is broken if the left side of the <code>||</code> was true and the right side was therefore not evaluated.

Ordinarily, the programmer need not think about broken slots, and only works with null or broken values.


## Erlang

In Erlang a variable is created by assigning to it, so all variables have a value. To get undefined values you have to use a record. The default value of a record member is undefined.

```Erlang

-module( undefined_values ).

-export( [task/0] ).

-record( a_record, {member_1, member_2} ).

task() ->
    Record = #a_record{member_1=a_value},
    io:fwrite( "Record member_1 ~p, member_2 ~p~n", [Record#a_record.member_1, Record#a_record.member_2] ),
    io:fwrite( "Member_2 is undefined ~p~n", [Record#a_record.member_2 =:= undefined] ).

```

```txt

2> undefined_values:task().
Record member_1 a_value, member_2 undefined
Member_2 is undefined true

```



## ERRE


ERRE hasn't the concept of un-initialised or undefined variable: every scalar variable is allocated at runtime with value zero if numeric or value "" if string. Array type variables must be declared but follow the same initialisation rules of scalars.


## Fortran

Older style Fortran had no inbuilt facilities, other than a programmer adding code to recognise certain special values as "no value" or similar, so a set of valid values might be 1:20, and zero was reserved to signify "bad", or for a three-digit data field the special value 999 might be recognised, etc. Mistakes and confusions were routine, since a six-digit field might require 999999 except that value is too big for sixteen-bit integers and as a floating-point value may not be exactly expressed in binary. Averaging such values without checking for 999 (or 999999, etc.) might lead to overflow in output data fields or bad results that might be detected before publishing a report. In ''Numerical Methods That Work - Usually'', F.S. Acton remarks "The person who encoded YES, NO, DON'T CARE as one, two, three, ''respectively'' got the sort of correlations she deserved."

A more flexible approach involves associating a state variable with a monitored variable, thus X to hold the value, and XGOOD the indicator - as before, tedious added code.

More modern Fortrans recognise the NaN state of floating-point variables on computers whose floating-point arithmetic follows the IEEE standard, via the logical function

```Fortran
IsNaN(x)
```

This is the only safe way to detect them as tests to detect the special behaviour of NaN states such as ''if x = x then...else aha!;'' might be optimised away by the compiler, and other tests may behave oddly. For instance x ¬= 0 might be compiled as ¬(x = 0) and the special NaN behaviour will not be as expected. Such NaN values can come via READ statements, because "NaN" is a recognised numerical input option, and could be used instead of "999" in a F3.0 data field, etc. Or, your system's input processing might recognise "?" or other special indicators and so on.


## FreeBASIC

In FreeBASIC all variables are given a default value (zero for numbers, false for boolean and empty for strings) when they are declared unless they are assigned a different value at that time or are specifically left uninitialized (using the 'Any' keyword). If the latter are used before they have been initialized, then they will contain a 'garbage' value i.e. whatever value happens to be in the associated memory:

```freebasic
' FB 1.05.0 Win64

Dim i As Integer        '' initialized to 0 by default
Dim j As Integer = 3    '' initialized to 3
Dim k As Integer = Any  '' left uninitialized (compiler warning but can be ignored)

Print i, j, k
Sleep
```


Sample output (fortuitously, k contained its default value of 0 on this run):

```txt

 0             3             0

```



## GAP


```gap
IsBound(a);
# true

Unbind(a);

IsBound(a);
# false
```



## Go

Go has six types for which nil is defined.  Values of these six types can be tested against the predefined identifier nil [[Undefined values/Check if a variable is defined#Go|as shown]] by task [[Undefined values/Check if a variable is defined|Undefined values/Check if a variable is defined]].

For this task, I demonstrate,
# How certain attempts to use nil objects cause panics.
# How to initialize objects of these types.
# Successful (non panicking) use of initialized objects.
# One more quirky little feature involving a type switch on a nil interface.

```go
package main

import "fmt"

var (
    s []int
    p *int
    f func()
    i interface{}
    m map[int]int
    c chan int
)

func main() {
    fmt.Println("Exercise nil objects:")
    status()

    // initialize objects
    s = make([]int, 1)
    p = &s[0] // yes, reference element of slice just created
    f = func() { fmt.Println("function call") }
    i = user(0) // see user defined type just below
    m = make(map[int]int)
    c = make(chan int, 1)

    fmt.Println("\nExercise objects after initialization:")
    status()
}

type user int

func (user) m() {
    fmt.Println("method call")
}

func status() {
    trySlice()
    tryPointer()
    tryFunction()
    tryInterface()
    tryMap()
    tryChannel()
}

func reportPanic() {
    if x := recover(); x != nil {
        fmt.Println("panic:", x)
    }
}

func trySlice() {
    defer reportPanic()
    fmt.Println("s[0] =", s[0])
}

func tryPointer() {
    defer reportPanic()
    fmt.Println("*p =", *p)
}

func tryFunction() {
    defer reportPanic()
    f()
}

func tryInterface() {
    defer reportPanic()

    // normally the nil identifier accesses a nil value for one of
    // six predefined types.  In a type switch however, nil can be used
    // as a type.  In this case, it matches the nil interface.
    switch i.(type) {
    case nil:
        fmt.Println("i is nil interface")
    case interface {
        m()
    }:
        fmt.Println("i has method m")
    }

    // assert type with method and then call method
    i.(interface {
        m()
    }).m()
}

func tryMap() {
    defer reportPanic()
    m[0] = 0
    fmt.Println("m[0] =", m[0])
}

func tryChannel() {
    defer reportPanic()
    close(c)
    fmt.Println("channel closed")
}
```

Output:

```txt

Exercise nil objects:
panic: runtime error: index out of range
panic: runtime error: invalid memory address or nil pointer dereference
panic: runtime error: invalid memory address or nil pointer dereference
i is nil interface
panic: interface conversion: interface is nil, not interface { main.m() }
panic: runtime error: assignment to entry in nil map
panic: runtime error: close of nil channel

Exercise objects after initialization:
s[0] = 0
*p = 0
function call
i has method m
method call
m[0] = 0
channel closed

```



## Haskell

In Haskell, there is a semantic concept called [http://www.haskell.org/haskellwiki/Bottom "bottom"], which is a computation that never terminates or runs into an error. So <code>undefined</code> is not a proper value at all; it is a bottom that causes an exception when evaluated. For example,


```haskell
main = print $ "Incoming error--" ++ undefined
-- When run in GHC:
-- "Incoming error--*** Exception: Prelude.undefined
```


This isn't quite as dangerous as it sounds because of Haskell's laziness. For example, this program:


```haskell
main = print $ length [undefined, undefined, 1 `div` 0]
```


prints <code>3</code>, since <code>length</code> doesn't need to evaluate any of the elements of its input.

In practice, one uses <code>undefined</code> less often than <code>error</code>, which behaves exactly the same except that it lets you choose the error message. So if you say


```haskell
resurrect 0 = error "I'm out of orange smoke!"
```


then if you make the mistake of writing your program such that it at some point requires the value of <code>resurrect 0</code>, you'll get the error message "I'm out of orange smoke!". <code>undefined</code> may be defined in the same way:


```haskell
undefined :: a
undefined = error "Prelude.undefined"
```


Since <code>undefined</code> causes an exception, the usual exception handling mechanism can be used to catch it:


```haskell
import Control.Exception (catch, evaluate, ErrorCall)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (catch)
import Control.DeepSeq (NFData, deepseq)

scoopError :: (NFData a) => a -> Either String a
scoopError x = unsafePerformIO $ catch right left
  where right = deepseq x $ return $ Right x
        left e = return $ Left $ show (e :: ErrorCall)

safeHead :: (NFData a) => [a] -> Either String a
safeHead = scoopError . head

main = do
  print $ safeHead ([] :: String)
  print $ safeHead ["str"]
```


== Icon and Unicon ==
Icon/Unicon don't really have a notion of an undefined variable.  There is a [[Undefined_values/Check_if_a_variable_is_defined|null value/data type that can be tested]].  However, it is possible in Unicon to interrogate the environment and obtain the string names of variables in the current (or calling procedures) and determine if a variable is defined.
=
## Icon
=
The functions localnames, paramnames, staticnames, and globalnames don't exist in Icon.

=
## Unicon
=

```Unicon
global G1

procedure main(arglist)
local ML1
static MS1
undeftest()
end

procedure undeftest(P1)
static S1
local L1,L2
every                                                                   #write all local, parameter, static, and global variable names
   write((localnames|paramnames|staticnames|globalnames)(&current,0))   # ... visible in the current co-expression at this calling level (0)
return
end
```


The output looks like:

```txt
L1
L2
P1
S1
main
undeftest
write
localnames
paramnames
staticnames
globalnames
```

Note that ML1,arglist, and MS1 are not listed.  Also note, that procedures names are just global variables of type procedure.


## J

J does not have a concept of an "undefined value" as such, but J does allow treatment of undefined names.  The verb <code>nc</code> finds the (syntactic) class of a name.  This result is negative one for names which have not been defined.


```J

  foo=: 3
  nc;:'foo bar'
0 _1
```


From this we can infer that <code>foo</code> has a definition (and its definition is a noun, since 0 is the syntactic [http://www.jsoftware.com/help/dictionary/dx004.htm name class] for nouns), and we can also infer that <code>bar</code> does not have a definition.

This task also asked that we ''identify and exercise .. mechanisms for ... manipulating a variable's value's status as being undefined''. So: a name can be made to be undefined using the verb <code>erase</code>.  The undefined status can be removed by assigning a value to the name.


```J

   erase;:'foo bar'
1 1
   nc;:'foo bar'
_1 _1
   bar=:99
   nc;:'foo bar'
_1 0
```



## Java

In Java there are two kinds of types: primitive types and reference types. The former are predefined in the language (eg. <code>boolean</code>, <code>int</code>, <code>long</code>, <code>double</code>, &c), while the latter are pointers to objects (class instances or arrays).

Java has a special null type, the type of the expression <code>null</code>, that can be cast to any reference type; in practice the null type can be ignored and <code>null</code> can be treated as a special literal that can be of any reference type. When a reference variable has the special value <code>null</code> it refers to no object, meaning that it is undefined.

```java
String string = null;        // the variable string is undefined
System.out.println(string);           //prints "null" to std out
System.out.println(string.length());  // dereferencing null throws java.lang.NullPointerException
```


Variables of primitive types cannot be assigned the special value <code>null</code>, but there are wrapper classes corresponding to the primitive types (eg. <code>Boolean</code>, <code>Integer</code>, <code>Long</code>, <code>Double</code>, &c), that can be used instead of the corresponding primitive; since they can have the special value <code>null</code> it can be used to identify the variable as undefined.

```java
int i = null;      // compilation error: incompatible types, required: int, found: <nulltype>
if (i == null) {   // compilation error: incomparable types: int and <nulltype>
    i = 1;
}
```

But this piece of code can be made valid by replacing <code>int</code> with <code>Integer</code>, and thanks to the automatic conversion between primitive types and their wrapper classes (called autoboxing) the only change required is in the declaration of the variable:

```java
Integer i = null;  // variable i is undefined
if (i == null) {
    i = 1;
}
```



## JavaScript

In Javascript undefined is a property of the global object, i.e. it is a variable in global scope. The initial value of undefined is the primitive value undefined. The problem with using undefined is that undefined is mutable. Instead we can use typeof to check if a value is undefined.

```javascript
var a;

typeof(a) === "undefined";
typeof(b) === "undefined";

var obj = {}; // Empty object.
typeof(obj.c) === "undefined";

obj.c = 42;

obj.c === 42;
delete obj.c;
typeof(obj.c) === "undefined";
```


We can also use the prefix keyword void, it always returns undefined. But this will throw a error if the variable has not been defined.

```javascript
var a;
a === void 0; // true
b === void 0; // throws a ReferenceError
```



## jq

Given a JSON object, o, and a key, k, that is not present in that object, then o[k] evaluates to null, e.g.

```jq
{}["key"] #=> null
```


In an important sense, therefore, null in jq represents an undefined value.  However, it should be noted that in jq, 1/0 does not yield null:

```jq>1/0 == null #=>false</lang


It should also be noted that in jq, null can combine with other values to form non-null values. Specifically, for any JSON entity, e,
both null + e and e + null evaluate to e.  This is often convenient as it avoids having to handle edge cases specially.

For example, suppose it is agreed that the "sum" of the elements of an empty array should be null. Then one can simply write:

```jq
def sum: reduce .[] as $x (null; . + $x);
```




## Julia

Julia has two different notions of undefined values:

1. During compilation, a variable name that is not recognized by the compiler causes an undefined variable error. For example, if the variable x has not been defined previously and is used on the right-hand side of an expression, this produces in the REPL command line:
<code>
 julia> x + 1
 ERROR: UndefVarError: x not defined
 Stacktrace:
  [1] top-level scope at none:0
</code>
2. For variables that are defined for the Julia program but have undefined values, there are two types of undefined value in Julia (version > 0.7): <code> nothing </code> and <code> missing </code>. "nothing" and "missing" are typed constants used by convention to refer to either (with <code> nothing </code>) an absent result, such as a search with nothing found, or in the case of <code> missing, </code> a data location containing a missing value, such as a data table with missing values. <code>nothing</code> generally produces an error if any calculations incorporate it, but <code>missing</code> can be propagated along a calculation:

```julia

julia> arr = [1, 2, nothing, 3]
4-element Array{Union{Nothing, Int64},1}:
 1
 2
  nothing
 3

julia> x = arr .+ 5
ERROR: MethodError: no method matching +(::Nothing, ::Int64)
Closest candidates are:
  +(::Any, ::Any, ::Any, ::Any...) at operators.jl:502
  +(::Complex{Bool}, ::Real) at complex.jl:292
  +(::Missing, ::Number) at missing.jl:93
  ...

julia> arr = [1, 2, missing, 3]
4-element Array{Union{Missing, Int64},1}:
 1
 2
  missing
 3

julia> x = arr .+ 5
4-element Array{Union{Missing, Int64},1}:
 6
 7
  missing
 8

```



## Kotlin

Kotlin distinguishes between nullable and non-nullable types but, as this has already been covered in the Null Object task (http://rosettacode.org/wiki/Null_object#Kotlin), there is no point in repeating it here. It is any case debatable whether 'null' is an undefined value or not since, in Kotlin, it is technically the only value of the nullable Nothing? type.

However, the non-nullable Nothing type which has no instances and is a sub-type of all other types can be said to represent an undefined value as expressions of this type (such as a 'throw' expression) clearly have no defined value. 'Nothing' can be used in Kotlin to represent the return type of a function which never returns, because it always throws an exception or error. This can be useful when developing an application where the implementation of a function is being left until later.

Generally speaking, the compiler ensures that all variables and properties have a value before they are used. However, there is one exception to this - mutable properties of non-null, non-primitive types, marked with the 'lateinit' modifier don't need to be initialized in place or in the constructor where it would be inconvenient to do so. If such a property is used before it has been initialized, a special error is thrown. During the period prior to being initialized, a 'lateinit' property can therefore be said to be undefined.

Here are some simple examples illustrating these points:

```scala
// version 1.1.2

class SomeClass

class SomeOtherClass {
    lateinit var sc: SomeClass

    fun initialize() {
        sc = SomeClass()  // not initialized in place or in constructor
    }

    fun printSomething() {
        println(sc)  // 'sc' may not have been initialized at this point
    }

    fun someFunc(): String {
        // for now calls a library function which throws an error and returns Nothing
        TODO("someFunc not yet implemented")
    }
}

fun main(args: Array<String>) {
    val soc = SomeOtherClass()

    try {
        soc.printSomething()
    }
    catch (ex: Exception) {
        println(ex)
    }

    try {
        soc.someFunc()
    }
    catch (e: Error) {
        println(e)
    }
}
```


```txt

kotlin.UninitializedPropertyAccessException: lateinit property sc has not been initialized
kotlin.NotImplementedError: An operation is not implemented: someFunc not yet implemented

```



## Lingo

In Lingo an undefined variable has the value <Void>. If a variable is <Void> (i.e. undefined) can be checked by comparing it with the constant VOID, or by using the function voidP():

```lingo
put var
-- <Void>
put var=VOID
-- 1
put voidP(var)
-- 1
var = 23
put voidP(var)
-- 0
```



## Logo

UCB Logo has separate namespaces for procedures and variables ("names"). There is no distinction between a proc/name with no value and an undefined proc/name.


```logo
; procedures
to square :x
  output :x * :x
end

show defined? "x   ; true
show procedure? "x  ; true (also works for built-in primitives)
erase "x
show defined? "x   ; false
show square 3      ; I don't know how  to square

; names

make "n 23

show name? "n   ; true
ern "n
show name? "n   ; false
show :n     ; n has no value
```



## LOLCODE

LOLCODE's nil value is called <tt>NOOB</tt>, to which all uninitialized variables evaluate, and which is distinct from <tt>FAIL</tt>, the false value.

```LOLCODE
HAI 1.3

I HAS A foo BTW, INISHULIZD TO NOOB
DIFFRINT foo AN FAIL, O RLY?
    YA RLY, VISIBLE "FAIL != NOOB"
OIC

I HAS A bar ITZ 42
bar, O RLY?
    YA RLY, VISIBLE "bar IZ DEFIND"
OIC

bar R NOOB BTW, UNDEF bar
bar, O RLY?
    YA RLY, VISIBLE "SHUD NEVAR C DIS"
OIC

KTHXBYE
```



## Lua


```lua
print( a )

local b
print( b )

if b == nil then
    b = 5
end
print( b )
```

Output:

```txt
nil
nil
5
```



## Mathematica

Mathematica is a symbolic mathematical software. Variables without given values are treated as symbols.

```Mathematica
a
-> a

a + a
-> 2 a

ValueQ[a]
-> False

a = 5
-> 5

ValueQ[a]
-> True
```

Mathematica also has a build-in symbol "Undefined", representing a quantity with no defined value.

```Mathematica
ConditionalExpression[a, False]
->Undefined
```

Mathematical expressions containing Undefined evaluate to Undefined:

```Mathematica
Sin[Undefined]
-> Undefined
```

Of course you can assign Undefined to be the value of a variable. Here "Undefined" is itself a value.

```Mathematica
a = Undefined
-> Undefined

a
-> Undefined

ValueQ[a]
-> True
```


=={{header|MATLAB}} / {{header|Octave}}==

If a variable is generated without defing a value, e.g. with

```Matlab>  global var; </lang

the variable is empty, and can be tested with

```Matlab
  isempty(var)
```


For numerical values (e.g. vectors or arrays) with a predefined size, often not-a-numbers (NaN's) user used to indicate missing values,

```Matlab
  var = [1, 2, NaN, 0/0, inf-inf, 5]
```

These can be tested with:

```Matlab
  isnan(var)
```



```txt

ans =

   0   0   1   1   1   0

```



## MUMPS

<p>MUMPS does have variables with undefined values, but referencing them usually causes an error. To test for whether a value is undefined use the $Data function. If you are trying to read a value that may be undefined, use $Get as a wrapper. Note that an alternate form of $Get can return a specified value, which must be defined.</p>

```MUMPS
 IF $DATA(SOMEVAR)=0 DO UNDEF ; A result of 0 means the value is undefined
 SET LOCAL=$GET(^PATIENT(RECORDNUM,0)) ;If there isn't a defined item at that location, a null string is returned
```



## OCaml



```ocaml
(* There is no undefined value in OCaml,
   but if you really need this you can use the built-in "option" type.
   It is defined like this: type 'a option = None | Some of 'a *)

let inc = function
  Some n -> Some (n+1)
| None -> failwith "Undefined argument";;

inc (Some 0);;
(* - : value = Some 1 *)

inc None;;
(* Exception: Failure "Undefined argument". *)
```



## Oforth


In Oforth, there is the null object, which is an instance of Null Class.

null is used as the default value for local variables and attributes.


## Oz

A program that uses an undefined variable does not compile.
However, variables can be "unbound" or "free". If a program tries to read such a variable, the current thread will be suspended until the variable's value becomes determined.


```oz
declare X in

thread
   if {IsFree X} then {System.showInfo "X is unbound."} end
   {Wait X}
   {System.showInfo "Now X is determined."}
end

{System.showInfo "Sleeping..."}
{Delay 1000}
{System.showInfo "Setting X."}
X = 42
```


Explicitly checking the status of a variable with <code>IsFree</code> is discouraged because it can introduce race conditions.


## PARI/GP

In GP, undefined variables test equal to the monomial in the variable with the name of that variable.  So to test if <var>v</var> is undefined, do

```parigp
v == 'v
```


In PARI, you can do the same but the function <code>is_entry()</code> is more appropriate:

```C
is_entry("v") == NULL;
```



## Pascal

See [[Undefined_values#Delphi | Delphi]]


## Perl


```perl
#!/usr/bin/perl -w
use strict;

# Declare the variable. It is initialized to the value "undef"
our $var;

# Check to see whether it is defined
print "var contains an undefined value at first check\n" unless defined $var;

# Give it a value
$var = "Chocolate";

# Check to see whether it is defined after we gave it the
# value "Chocolate"
print "var contains an undefined value at second check\n" unless defined $var;

# Give the variable the value "undef".
$var = undef;
# or, equivalently:
undef($var);

# Check to see whether it is defined after we've explicitly
# given it an undefined value.
print "var contains an undefined value at third check\n" unless defined $var;

# Give the variable a value of 42
$var = 42;

# Check to see whether the it is defined after we've given it
# the value 42.
print "var contains an undefined value at fourth check\n" unless defined $var;

# Because most of the output is conditional, this serves as
# a clear indicator that the program has run to completion.
print "Done\n";
```


Results in:


```txt
var contains an undefined value at first check
var contains an undefined value at third check
Done

```



## Perl 6

Perl 6 has "interesting" values of undef, but unlike Perl 5, doesn't actually have a value named <tt>undef</tt>.  Instead, several very different meanings of undefinedness are distinguished.  First, <tt>Nil</tt> represents the absence of a value.  The absence of a value cannot be stored.  Instead, an attempt to assign <tt>Nil</tt> to a storage location causes that location to revert to its uninitialized state, however that is defined.


```perl6
my $x; $x = 42; $x = Nil; say $x.WHAT; # prints Any()
```


This <tt>Any</tt> is an example of another kind of undefined type, which is a typed undef.  All reference types have an undefined value representing the type.  You can think of it as a sort of "type gluon" that carries a type charge without being a "real" particle.  Hence there are undefined values whose names represent types, such as <tt>Int</tt>, <tt>Num</tt>, <tt>Str</tt>, and all the other object types in Perl 6.  As generic objects, such undefined values carry the same metaobject pointer that a real object of the type would have, without being instantiated as a real object.  As such, these types are in the type hierarchy.  For example, <tt>Int</tt> derives from <tt>Cool</tt>, <tt>Cool</tt> derives from <tt>Any</tt>, and <tt>Any</tt> derives from <tt>Mu</tt>, the most general undefined object (akin to Object in other languages).  Since they are real objects of the type, even if undefined, they can be used in reasoning about the type.  You don't have to instantiate a <tt>Method</tt> object in order to ask if <tt>Method</tt> is derived from <tt>Routine</tt>, for instance.


```perl6
say Method ~~ Routine;  # Bool::True
```


Variables default to <tt>Any</tt>, unless declared to be of another type:

```perl6
my     $x; say $x.WHAT; # Any()
my Int $y; say $y.WHAT; # Int()
my Str $z; say $z.WHAT; # Str()
```


The user-interface for definedness are [http://design.perl6.org/S12.html#Abstract_vs_Concrete_types type smilies] and the <tt>with</tt>-statement.


```perl6
my Int:D $i = 1; # if $i has to be defined you must provide a default value
multi sub foo(Int:D $i where * != 0){ (0..100).roll / $i } # we will never divide by 0
multi sub foo(Int:U $i){ die 'WELP! $i is undefined' } # because undefinedness is deadly

with $i { say 'defined' } # as "if" is looking for Bool::True, "with" is looking for *.defined
with 0 { say '0 may not divide but it is defined' }

```


There are further some [http://design.perl6.org/S03.html operators] for your convenience.


```perl6
my $is-defined = 1;
my $ain't-defined = Any;
my $doesn't-matter;
my Any:D $will-be-defined = $ain't-defined // $is-defined // $doesn't-matter;

my @a-mixed-list = Any, 1, Any, 'a';
$will-be-defined = [//] @a-mixed-list; # [//] will return the first defined value

my @a = Any,Any,1,1;
my @b = 2,Any,Any,2;
my @may-contain-any = @a >>//<< @b; # contains: [2, Any, 1, 1]

sub f1(){Failure.new('WELP!')};
sub f2(){ $_ ~~ Failure }; # orelse will kindly set the topic for us
my $s = (f1() orelse f2()); # Please note the parentheses, which are needed because orelse is
                            # much looser then infix:<=> .
dd $s; # this be Bool::False
```


Finally, another major group of undefined values represents failures.  Perl 6 is designed with the notion that throwing exceptions is bad for parallel processing, so exceptions are thrown lazily; that is, they tend to be returned in-band instead as data, and are only thrown for real if not properly handled as exception data.  (In which case, the exception is required to report the original difficulty accurately.)  In order not to disrupt control flow and the synchronicity of event ordering, such failures are returned in-band as a normal values.  And in order not to subvert the type system when it comes to return types, failure may be mixed into any reference type which produces an undefined, typed value which is also indicates the nature of the failure.

Native storage types work under different rules, since most native types cannot represent failure, or even undefinedness.  Any attempt to assign a value to a storage location that cannot represent a failure will cause an exception to be thrown.


## Phix

Phix has a special unassigned value, that can be tested for using object(). (There is no offical way to "un-assign" a variable, though you could fairly easily do so with a bit of inline assembly.)

```Phix
object x

procedure test()
    if object(x) then
        puts(1,"x is an object\n")
    else
        puts(1,"x is unassigned\n")
    end if
end procedure

test()
x = 1
test()
```

```txt

x is unassigned
x is an object

```



## PHP


```php
<?php
// Check to see whether it is defined
if (!isset($var))
    echo "var is undefined at first check\n";

// Give it a value
$var = "Chocolate";

// Check to see whether it is defined after we gave it the
// value "Chocolate"
if (!isset($var))
    echo "var is undefined at second check\n";

// Give the variable an undefined value.
unset($var);

// Check to see whether it is defined after we've explicitly
// given it an undefined value.
if (!isset($var))
    echo "var is undefined at third check\n";

// Give the variable a value of 42
$var = 42;

// Check to see whether the it is defined after we've given it
// the value 42.
if (!isset($var))
    echo "var is undefined at fourth check\n";

// Because most of the output is conditional, this serves as
// a clear indicator that the program has run to completion.
echo "Done\n";
?>
```


Results in:


```txt
var is undefined at first check
var is undefined at third check
Done

```



## PicoLisp

An internal symbol is initialized to NIL. Depending on the context, this is
interpreted as "undefined". When called as a function, an error is issued:

```PicoLisp
: (myfoo 3 4)
!? (myfoo 3 4)
myfoo -- Undefined
?
```

The function 'default' can be used to initialize a variable if and only
if its current value is NIL:

```PicoLisp
: MyVar
-> NIL

: (default MyVar 7)
-> 7

: MyVar
-> 7

: (default MyVar 8)
-> 7

: MyVar
-> 7
```


## Pike

In Pike variables are always defined. <code>UNDEFINED</code> is only used to indicate the nonexistence of a key or object member. <code>UNDEFINED</code> is not a value and can not be assigned. in such cases it is converted to <math>0</math>. <code>zero_type()</code> is used to test if a key exists or not:

```Pike

> zero_type(UNDEFINED);
Result: 1
> mapping bar = ([ "foo":"hello" ]);
> zero_type(bar->foo);
Result: 0
> zero_type(bar->baz);
Result: 1
> bar->baz=UNDEFINED;
Result: 0
> zero_type(bar->baz);
Result: 0

```



## PowerShell

The <code>Get-Variable</code> cmdlet gets the Windows PowerShell variables in the current console. Variables can be filtered by using the <code>-Name</code> parameter.
If a variable doesn't exist an error is returned.  Using the <code>-ErrorAction SilentlyContinue</code> parameter suppresses the error message and returns <code>$false</code>.

```PowerShell

if (Get-Variable -Name noSuchVariable -ErrorAction SilentlyContinue)
{
    $true
}
else
{
    $false
}

```

```txt

False

```

PowerShell represents things like the file system, registry, functions, variables, etc. with '''providers''' known as PS drives.
One of those PS drives is Variable. <code>Variable:</code> contains all of the variables that are currently stored in memory.

```PowerShell

Get-PSProvider

```

```txt

Name                 Capabilities                                             Drives
----                 ------------                                             ------
Registry             ShouldProcess, Transactions                              {HKLM, HKCU}
Alias                ShouldProcess                                            {Alias}
Environment          ShouldProcess                                            {Env}
FileSystem           Filter, ShouldProcess, Credentials                       {C, D, E, Q}
Function             ShouldProcess                                            {Function}
Variable             ShouldProcess                                            {Variable}
Certificate          ShouldProcess                                            {Cert}
WSMan                Credentials                                              {WSMan}

```

To access the Variable PS drive you'd use the same syntax as you would with the file system by specifying <code>Variable:\</code>.

```PowerShell

Test-Path Variable:\noSuchVariable

```

```txt

False

```

If a variable doesn't exist, it technically has a value of <code>$null</code>. <code>$null</code> is an automatic variable that represents "does not exist."

```PowerShell

$noSuchVariable -eq $null

```

```txt

True

```



## Prolog

Prolog has two predicates to know if a variable is instancied or not : '''var/1''' and '''nonvar/1'''


```txt
?- var(Y).
true.

?- X = 4, var(X).
false.

?- nonvar(Y).
false.

?- X = 4, nonvar(X).
X = 4.

```



## PureBasic

Variables are defined through a formal declaration or simply upon first use.  Each variable is initialized to a value.  PureBasic does not allow changing of a variable's status at runtime.  It can be tested at compile-time and acted upon, however, it cannot be undefined once it is defined.

```PureBasic
If OpenConsole()

  CompilerIf Defined(var, #PB_Variable)
    PrintN("var is defined at first check")
  CompilerElse
    PrintN("var is undefined at first check")
    Define var
  CompilerEndIf

  CompilerIf Defined(var, #PB_Variable)
    PrintN("var is defined at second check")
  CompilerElse
    PrintN("var is undefined at second check")
    Define var
  CompilerEndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()

EndIf
```

Sample output:

```txt
var is undefined at first check
var is defined at second check
```



## Python

In Python names, (variables), can be dynamically created and deleted at run time.


```python
# Check to see whether a name is defined
try: name
except NameError: print "name is undefined at first check"

# Create a name, giving it a string value
name = "Chocolate"

# Check to see whether the name is defined now.
try: name
except NameError: print "name is undefined at second check"

# Remove the definition of the name.
del name

# Check to see whether it is defined after the explicit removal.
try: name
except NameError: print "name is undefined at third check"

# Recreate the name, giving it a value of 42
name = 42

# Check to see whether the name is defined now.
try: name
except NameError: print "name is undefined at fourth check"

# Because most of the output is conditional, this serves as
# a clear indicator that the program has run to completion.
print "Done"
```


Results in:


```txt
name is undefined at first check
name is undefined at third check
Done

```



## R

There are four cases to consider.  To test whether a varaible has previously been defined, use <code>exists</code>.

```r

exists("x")

```


If you want to declare a variable with undefined contents, use <code>NULL</code>.

```r

x <- NULL

```


If you want to declare a variable with missing values, use <code>NA</code>.

```r

y <- c(1, 4, 9, NA, 25)
z <- c("foo", NA, "baz")

```

(Note that there are different types of <code>NA</code>, namely <code>NA_integer_</code>, <code>NA_real_</code>, <code>NA_character_</code>, <code>NA_complex_</code> and plain (logical) <code>NA</code>.  In practice, you should hardly ever need to explicitly set which type of NA you are using, as it will be done automatically.)

Finally, you test for arguments that haven't been passed into a function with <code>missing</code>.

```r

print_is_missing <- function(x)
{
  print(missing(x))
}

print_is_missing()                # TRUE
print_is_missing(123)             # FALSE

```



## Racket


Racket does have an undefined value, which is used to initialize
recursive definitions.  It can be grabbed explicitly with:


```Racket

-> (letrec ([x x]) x)
#<undefined>

```


However, it is not used as an implicit value for all (non-existent)
bindings.  (Racket uses other meta-tools for that.)


## REXX


```rexx
/*REXX program test if a (REXX)  variable is  defined  or  not defined.       */
tlaloc = "rain god of the Aztecs."     /*assign a value to the Aztec rain god.*/
                                       /*check if the  rain god  is defined.  */
y= 'tlaloc'
if symbol(y)=="VAR"  then say y  ' is   defined.'
                     else say y  "isn't defined."

                                       /*check if the  fire god  is defined.  */

y= 'xiuhtecuhtli'                      /*assign a value to the Aztec file god.*/
if symbol(y)=="VAR"  then say y  ' is   defined.'
                     else say y  "isn't defined."


drop tlaloc                            /*un─define the  TLALOC  REXX variable.*/
                                       /*check if the  rain god  is defined.  */
y= 'tlaloc'
if symbol(y)=="VAR"  then say y  ' is  defined.'
                     else say y  "isn't defined."
                                       /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

tlaloc  is   defined.
xiuhtecuhtli isn't defined.
tlaloc isn't defined.

```



## Ring


```ring

# Project : Undefined values

test()
func test
       x=10 y=20
       see islocal("x") + nl +
       islocal("y") + nl +
       islocal("z") + nl

```

Output:

```txt

1
1
0

```



## Ruby


```ruby
# Check to see whether it is defined
puts "var is undefined at first check" unless defined? var

# Give it a value
var = "Chocolate"

# Check to see whether it is defined after we gave it the
# value "Chocolate"
puts "var is undefined at second check" unless defined? var

# I don't know any way of undefining a variable in Ruby

# Because most of the output is conditional, this serves as
# a clear indicator that the program has run to completion.
puts "Done"
```


Results in:


```txt
var is undefined at first check
Done

```



## Scala

Learned from [http://catless.ncl.ac.uk/Risks/25.51.html#subj9.1 Tony Hoare's "Null References: The Billion Dollar Mistake"], Scala discourages the use of ''null'' values. For Java compatibility the value ''null'' could be used, but since it implies also a mutable ''var''  this is a bad practice.

Instead type wrapper ''Option'' is used which could result in ''Some(value)'' or ''None''. A Option[Boolean] e.g. reflects a tri-state (nullable) database value.

Notices that ''Option(value).isEmpty'' methods (on e.g. String) usually test for nullable.


## Seed7

Seed7 variables are initialized, when they are defined. This way a variable can never have an undefined value. There is also no general NULL value. When there is a need for a NULL value, an interface type can define its own NULL value. E.g.: The interface type [http://seed7.sourceforge.net/libraries/file.htm file] defines [http://seed7.sourceforge.net/libraries/null_file.htm#STD_NULL STD_NULL], which is used to initialize file variables and as result of [http://seed7.sourceforge.net/libraries/external_file.htm#open%28in_string,in_string%29 open], when a file cannot be opened.


## Sidef

Sidef variables are initialized with a default ''nil'' value, representing the absence of a value.

```ruby
var x;      # declared, but not defined
x == nil   && say "nil value";
defined(x) || say "undefined";

# Give "x" some value
x = 42;

defined(x) && say "defined";

# Change "x" back to `nil`
x = nil;

defined(x) || say "undefined";
```

```txt

nil value
undefined
defined
undefined

```



## Smalltalk

In Smalltalk, all variables are automatically initialized to nil. This includes instance variables (an object's private slots), class variables and locals. Also global and namespace bindings can only be created by passing an existing value (typically also: nil) to the creation message (at:put:). This is part of the language's specification, not implementation specific. Thus a variable cannot ever be undefined.

Sending a message (aka performing a virtual function call) to the nil object results in an Exception, which can be caught if desired.
In other words, there is no segmentation violation or core dump, but well defined behavior in this situation.

However, we can check for the existence of a global binding with:

```smalltalk
Smalltalk includesKey: #FooBar
myNamespace includesKey: #Baz
```



## Tcl

Tcl does not have undefined ''values'', but ''variables'' may be undefined by being not set.

```tcl
# Variables are undefined by default and do not need explicit declaration

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at first check"}

# Give it a value
set var "Screwy Squirrel"

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at second check"}

# Remove its value
unset var

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at third check"}

# Give it a value again
set var 12345

# Check to see whether it is defined
if {![info exists var]} {puts "var is undefind at fourth check"}

puts "Done"
```

Yields this output:

```txt

var is undefind at first check
var is undefind at third check
Done

```



## UNIX Shell


```bash
VAR1="VAR1"
echo ${VAR1:-"Not set."}
echo ${VAR2:-"Not set."}
```


```txt
VAR1
Not set.
```



## zkl

Nothing is undefined in zkl (in theory). Numeric values like inf and NaN are "discouraged". "Void" is an object that is used as a "sentinel" for "I have no idea what the proper value should be" but it is just object that can be used like any other object.

```zkl
println(Void);
1+Void
if(Void){} else { 23 }
```

```txt

Void
Exception thrown: NotImplementedError(Conversion not possible for Void: toInt)
23

```


