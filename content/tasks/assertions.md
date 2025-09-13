+++
title = "Assertions"
description = ""
date = 2019-10-06T04:48:49Z
aliases = []
[extra]
id = 3353
[taxonomies]
categories = ["Basic language learning", "task"]
tags = []
+++

## Task

Assertions are a way of breaking out of code when there is an error or an unexpected input.

Some languages throw [[exceptions]] and some treat it as a break point.


;Task:
Show an assertion in your language by asserting that an integer variable is equal to '''42'''.





## Ada

Using pragma Assert:

```ada
pragma Assert (A = 42, "Oops!");
```

The behavior of pragma is controlled by pragma Assertion_Policy. Another way is to use the predefined package Ada.Assertions:

```ada
with Ada.Assertions;  use Ada.Assertions;
...
Assert (A = 42, "Oops!");
```

The procedure Assert propagates Assertion_Error when condition is false.


## Aime


```aime
integer x;

x = 41;
if (x != 42) {
    error("x is not 42");
}
```

Executing the program will produce on standard error:

```txt
aime: assert: 5: x is not 42
```



## ALGOL 68

The "Revised Report on the Algorithmic Language - ALGOL 68" suggest that
ASSERT may be made available by a particular implementation, quote: "Pragmats may
... convey to the implementation some piece of information affecting some aspect
of the meaning of the program which is not defined by this Report,..."

Example given[http://www.xs4all.nl/~jmvdveer/report_4.html#92]:
 INT a, b; read((a, b)) PR ASSERT a >= 0 & b > 0 PR;

This works with neither [[ELLA ALGOL 68]] nor [[ALGOL 68G]].

The standard alternative would be to implement the assertions
as an exception as per the '''[[Exceptions#ALGOL_68|Exceptions]]''' sample code.

In [[ELLA ALGOL 68]] the ASSERT is implemented as an operator in the ''environment'' prelude:

```algol68
OP      ASSERT = (VECTOR [] CHAR assertion,BOOL valid) VOID:
IF      NOT valid
THEN    type line on terminal(assertion);
        terminal error( 661 {invalid assertion } )
FI;
```

And can be "USEd" as follows:

```algol68
PROGRAM assertions CONTEXT VOID
USE standard,environment
BEGIN
  INT a := 43;
  "Oops!" ASSERT ( a = 42 )
END
FINISH
```



## ALGOL W

Assertions were added to the 1972 version of Algol W. If the tested condition is false, the program terminates. In the following, the write does not get executed.


```algolw
begin
    integer a;
    a := 43;
    assert a = 42;
    write( "this won't appear" )
end.
```



## Apex

Asserts that the specified condition is true. If it is not, a fatal error is returned that causes code execution to halt.

```apex

String myStr = 'test;
System.assert(myStr == 'something else', 'Assertion Failed Message');

```


Asserts that the first two arguments are the same. If they are not, a fatal error is returned that causes code execution to halt.

```apex

Integer i = 5;
System.assertEquals(6, i, 'Expected 6, received ' + i);

```


Asserts that the first two arguments are different. If they are the same, a fatal error is returned that causes code execution to halt.

```apex

Integer i = 5;
System.assertNotEquals(5, i, 'Expected different value than ' + i);

```


'''You can’t catch an assertion failure using a try/catch block even though it is logged as an exception.'''


## AWK


AWK doesn't have a built-in assert statement. It could be simulated using a user-defined assert() function defined as below. The BEGIN section shows some examples of successful and failed "assertions".


```awk

BEGIN {
	meaning = 6 * 7
	assert(meaning == 42, "Integer mathematics failed")
	assert(meaning == 42)
	meaning = strtonum("42 also known as forty-two")
	assert(meaning == 42, "Built-in function failed")
	meaning = "42"
	assert(meaning == 42, "Dynamic type conversion failed")
	meaning = 6 * 9
	assert(meaning == 42, "Ford Prefect's experiment failed")
	print "That's all folks"
	exit
}

# Errormsg is optional, displayed if assertion fails
function assert(cond, errormsg){
	if (!cond) {
		if (errormsg != "") print errormsg
		exit 1
	}
}

```


The above example produces the output below, and sets the program's exit code to 1 (the default is 0)

```txt

Ford Prefect's experiment failed

```



## AutoHotkey


###  Exceptions

{{works with|AutoHotkey_L}}

```AHK
a := 42
Assert(a > 10)
Assert(a < 42) ; throws exception

Assert(bool){
    If !bool
        throw Exception("Expression false", -1)
}
```


###  Legacy versions


```AutoHotkey
if (a != 42)
{
OutputDebug, "a != 42" ; sends output to a debugger if connected
ListVars ; lists values of local and global variables
Pause ; pauses the script, use ExitApp to exit instead
}
```



## Axe


```axe
A=42??Returnʳ
```



## BaCon


```qbasic
' Assertions
answer = assertion(42)
PRINT "The ultimate answer is indeed ", answer

PRINT "Now, expect a failure, unless NDEBUG defined at compile time"
answer = assertion(41)
PRINT answer
END

' Ensure the given number is the ultimate answer
FUNCTION assertion(NUMBER i)

    ' BaCon can easily be intimately integrated with C
    USEH
        #include <assert.h>
    END USEH

    ' If the given expression is not true, abort the program
    USEC
        assert(i == 42);
    END USEC

    RETURN i
END FUNCTION
```


{{out}}

```txt
prompt$ bacon -q assertion.bac && ./assertion
Converting 'assertion.bac'... done, 24 lines were processed in 0.006 seconds.
Compiling 'assertion.bac'... cc  -c assertion.bac.c
cc -o assertion assertion.bac.o -lbacon -lm
Done, program 'assertion' ready.
The ultimate answer is indeed 42
Now, expect a failure, unless NDEBUG defined at compile time
assertion: assertion.assertion.h:16: assertion: Assertion `i == 42' failed.
ERROR: signal ABORT received - internal error. Try to compile the program with TRAP LOCAL to find the cause.

prompt$ bacon -q -o '-DNDEBUG' assertion.bac && ./assertion
Converting 'assertion.bac'... done, 24 lines were processed in 0.003 seconds.
Compiling 'assertion.bac'... cc  -DNDEBUG -c assertion.bac.c
cc -o assertion assertion.bac.o -lbacon -lm
Done, program 'assertion' ready.
The ultimate answer is indeed 42
Now, expect a failure, unless NDEBUG defined at compile time
41
```



## BBC BASIC


```bbcbasic
      PROCassert(a% = 42)
      END

      DEF PROCassert(bool%)
      IF NOT bool% THEN ERROR 100, "Assertion failed"
      ENDPROC
```



## Brat


```brat
squish import :assert :assertions

assert_equal 42 42
assert_equal 13 42  #Raises an exception
```



## C


```c
#include <assert.h>

int main(){
   int a;
   /* ...input or change a here */
   assert(a == 42); /* aborts program when a is not 42, unless the NDEBUG macro was defined */

   return 0;
}
```

To turn off assertions, simply define the <tt>NDEBUG</tt> macro before where <tt><assert.h></tt> is included.

There is no mechanism to add a custom "message" with your assertion, like in other languages. However, there is a "trick" to do this, by simply logical-AND-ing your condition with a string constant message, like in the following. Since a string constant is guaranteed to be non-NULL (and hence evaluated as True), and since AND-ing with True is an identity operation for a boolean, it will not alter the behavior of the assertion, but it will get captured in the debug message that is printed:

```c
assert(a == 42 && "Error message");
```

This trick only works with messages written directly in the source code (i.e. cannot be a variable or be computed), however, since the assertion message is captured by the macro at compile-time.

<!--Putting the languages together for once to avoid duplicating the passage.-->
=={{header|C sharp}} and {{header|Visual Basic .NET}}==

.NET provides the Debug.Assert and Trace.Assert methods, which notify TraceListener instances subscribed to the program's trace output if the specified condition is false. Both methods also have overloads that allow a specified string to be added to the default message of the assertion, which consists of "Assertion Failed" and a stack trace for the location of the assertion.

The behavior of a failed assertion is controlled by the listeners in the TraceListeners collection shared by the Debug and Trace classes. By default, the collection contains an instance of the DefaultTraceListener class, which uses functions in the Windows API that notify attached debuggers, if any. Additional behavior depends on the framework version that the application is running in:

* In .NET Core applications, if no debuggers are attached, failed Debug.Assert assertions for non-UI applications terminate the program and write the assertion message to the console, while failed Trace.Assert assertions do not affect execution. In this respect, a failed Debug assertion behaves similarly to an exception.
* In .NET Framework applications, for both types of assertions, a special instance of the Abort-Retry-Ignore message box containing the assertion message is displayed (even with a debugger attached). "Abort" terminates the program; "Retry" switches to the location of the assertion in source code if the application is running in a debugger, or, if none are attached, prompts to launch a just-in-time debugger; and "Ignore" continues execution past the assertion.

Calls to methods of the Debug class are only compiled when the DEBUG compiler constant is defined, and so are intended for asserting invariants in internal code that could only be broken because of logic errors. Calls to methods of the Trace class similarly require the TRACE constant, which, however, is defined by default for both debug and release builds in Visual Studio projects—trace assertions can thus be used for various logging purposes in production code.


```c#
using System.Diagnostics; // Debug and Trace are in this namespace.

static class Program
{
    static void Main()
    {
        int a = 0;

        Console.WriteLine("Before");

        // Always hit.
        Trace.Assert(a == 42, "Trace assertion failed");

        Console.WriteLine("After Trace.Assert");

        // Only hit in debug builds.
        Debug.Assert(a == 42, "Debug assertion failed");

        Console.WriteLine("After Debug.Assert");
    }
}
```



```vbnet
Imports System.Diagnostics
' Note: VB Visual Studio projects have System.Diagnostics imported by default,
' along with several other namespaces.

Module Program
    Sub Main()
        Dim a As Integer = 0

        Console.WriteLine("Before")

        ' Always hit.
        Trace.Assert(a = 42, "Trace assertion failed: The Answer was incorrect")

        Console.WriteLine("After Trace.Assert")

        ' Only hit in debug builds.
        Debug.Assert(a = 42, "Debug assertion failed: The Answer was incorrect")

        Console.WriteLine("After Debug.Assert")
    End Sub
End Module
```


{{out|note=for .NET Core debug builds when outside of a debugger}}

```txt
Before
After Trace.Assert
Assertion Failed
Debug assertion failed

   at Program.Main() in FILENAME:line 21
```


{{out}}
In .NET Core applications, this is the output
* when a debugger is attached and is used to continue past both assertions when they fail, or
* in release builds of the program, where the call to Debug.Assert is removed and the Trace.Assert assertion is hit but has no visible effects.

In .NET Framework applications, assertions never show up in the console and so the output is this when a debugger or the "Ignore" option used to continue past the assertions.


```txt
Before
After Trace.Assert
After Debug.Assert
```


'''Displaying Trace assertions in console:'''

To "see" the Trace.Assert assertion, additional TraceListener instances must be subscribed by the program. In the .NET Framework, there are several built-in subclasses of TraceListener, including ConsoleTraceListener, which writes trace messages to the console. In .NET Core, these classes are available starting from .NET Core 3.0.

Subscribing an instance involves adding the following line to the beginning of Main() (with a semicolon in C#, of course ;)

```vbnet
Trace.Listeners.Add(new ConsoleTraceListener())
```



## C++

{{trans|C}}

```cpp
#include <cassert> // assert.h also works

int main()
{
  int a;
  // ... input or change a here

  assert(a == 42); // Aborts program if a is not 42, unless the NDEBUG macro was defined
                    // when including <cassert>, in which case it has no effect
}
```

Note that assert does ''not'' get a <code>std::</code> prefix because it's a macro.


## Clojure


```Clojure

(let [i 42]
     (assert (= i 42)))

```



## Common Lisp


```lisp
(let ((x 42))
  (assert (and (integerp x) (= 42 x)) (x)))
```



## Component Pascal

Works with BlackBox Component Builder

```oberon2

MODULE Assertions;
VAR
	x: INTEGER;
PROCEDURE DoIt*;
BEGIN
	x := 41;
	ASSERT(x = 42);
END DoIt;
END Assertions.

Assertions.DoIt

```

Output:

```txt

TRAP 0

 Assertions.DoIt   [0000001DH]
 Kernel.Call   [00001A7CH]
	.adr	INTEGER	1685454913
	.kind	INTEGER	0
	.n	INTEGER	0
	.p	INTEGER	0
	.par	ARRAY 256 OF INTEGER	elements
	.r	REAL	8.70603013185328E+175
	.sig	POINTER	[64760018H]
	.size	INTEGER	2287288
	.sp	INTEGER	256
	.typ	POINTER	NIL
 Meta.Item.ParamCallVal   [00002B5EH]
	.adr	INTEGER	1685454913
	.data	ARRAY 256 OF INTEGER	elements

```



## D


```d
import std.exception: enforce;

int foo(in bool condition) pure nothrow
in {
    // Assertions are used in contract programming.
    assert(condition);
} out(result) {
    assert(result > 0);
} body {
    if (condition)
        return 42;

    // assert(false) is never stripped from the code, it generates an
    // error in debug builds, and it becomes a HALT instruction in
    // -release mode.
    //
    // It's used as a mark by the D type system. If you remove this
    // line the compiles gives an error:
    //
    // Error: function assertions.foo no return exp;
    //   or assert(0); at end of function
    assert(false, "This can't happen.");
}

void main() pure {
    int x = foo(true);

    // A regular assertion, it throws an error.
    // Use -release to disable it.
    // It can be used in nothrow functions.
    assert(x == 42, "x is not 42");

    // This throws an exception and it can't be disabled.
    // There are some different versions of this lazy function.
    enforce(x == 42, "x is not 42");
}
```



## Dart

Dart supplies a class Expect that works similar to the Assert methods of Junit

```d
main() {
  int i=42;
  int j=41;

  Expect.equals(42,i);
  Expect.equals(42,j);
}
```



## Delphi



```Delphi
Assert(a = 42);
```


If an assertion fails, EAssertionFailed exception is raised.

The generation of assertion code can be disabled by compiler directive


```Delphi
{$ASSERTIONS OFF}
```


Here is a simple console demo app which raises and handles assertion exception:


```Delphi
program TestAssert;

{$APPTYPE CONSOLE}

{.$ASSERTIONS OFF}   // remove '.' to disable assertions

uses
  SysUtils;

var
  a: Integer;

begin
  try
    Assert(a = 42);
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  Readln;
end.
```



## DWScript

Simple assertion, with a custom (optional) message

```Delphi
Assert(a = 42, 'Not 42!');
```

Other specialized assertions can be used in contracts, for instance this function check that the parameter (passed by reference ofr the purpose of illustration) is 42 when entering the function and when leaving the function

```Delphi
procedure UniversalAnswer(var a : Integer);
require
   a = 42;
begin
   // code here
ensure
   a = 42;
end;
```



## Dyalect

Dyalect has a built-in "assert" function:


```dyalect
var x = 42
assert(42, x)
```


This function throws an exception if assertion fails.


## E


E does not have the specific feature of assertions which may be disabled by a global option. But it does have a utility to throw an exception if a condition is false:


```e
require(a == 42)          # default message, "Required condition failed"

require(a == 42, "The Answer is Wrong.")   # supplied message

require(a == 42, fn { `Off by ${a - 42}.` })   # computed only on failure
```



## EchoLisp


```lisp

(assert (integer? 42)) → #t ;; success returns true

;; error and return to top level if not true;
(assert (integer? 'quarante-deux))
⛔ error: assert : assertion failed : (#integer? 'quarante-deux)

;; assertion with message (optional)
(assert (integer? 'quarante-deux) "☝️ expression must evaluate to the integer 42")
💥 error: ☝️ expression must evaluate to the integer 42 : assertion failed : (#integer? 'quarante-deux)

```



## ECL


<lang>
ASSERT(a = 42,'A is not 42!',FAIL);
```



## Eiffel

{{works with|SmartEiffel}} version 2.4

There are many assertion types in Eiffel, one is the following:

File called main.e:

```eiffel
class MAIN
    creation main
    feature main is
        local
            test: TEST;
        do
            create test;

            io.read_integer;
            test.assert(io.last_integer);
        end
end
```

Another file called test.e:

```eiffel
class TEST
    feature assert(val: INTEGER) is
        require
            val = 42;
        do
            print("Thanks for the 42!%N");
        end
end
```



## Elixir


```elixir
ExUnit.start

defmodule AssertionTest do
  use ExUnit.Case

  def return_5, do: 5

  test "not equal" do
    assert 42 == return_5
  end
end
```


{{out}}

```txt

  1) test not equal (AssertionTest)
     test.exs:8
     Assertion with == failed
     code: 42 == return_5
     lhs:  42
     rhs:  5
     stacktrace:
       test.exs:9: (test)



Finished in 0.1 seconds (0.1s on load, 0.01s on tests)
1 test, 1 failure

Randomized with seed 869000

```



## Emacs Lisp

Assertion can be loaded from cl.el:

```lisp
(require 'cl)
(let ((x 41))
  (assert (= x 42) t "the answer is not right"))
```



## Erlang

Erlang doesn't have an assert statement. However, it is single assignment, and its assignment operator won't complain if you reassign the exact same value to an existing variable but will throw an exception otherwise.

```erlang>1
 N = 42.
42
2> N = 43.
** exception error: no match of right hand side value 43
3> N = 42.
42
4> 44 = N.
** exception error: no match of right hand side value 42
5> 42 = N.
42
```


As such, the behavior of Erlang's assignment operator is extremely similar to a regular <tt>assert</tt> in other languages.


## Euphoria


```euphoria
type fourty_two(integer i)
    return i = 42
end type

fourty_two i

i = 41 -- type-check failure
```



## Factor

Throw an exception if the value on the top of the stack is not equal to 42:


```factor
USING: kernel ;
42 assert=
```



## FBSL

One needs to DECLARE the asserter variable at the top of script.

This implementation evaluates the expression given to the function and displays a message if it evaluates to false.

```qbasic
#APPTYPE CONSOLE

DECLARE asserter

FUNCTION Assert(expression)
    DIM cmd AS STRING = "DIM asserter AS INTEGER = (" & expression & ")"
    EXECLINE(cmd, 1)
    IF asserter = 0 THEN PRINT "Assertion: ", expression, " failed"
END FUNCTION

Assert("1<2")
Assert("1>2")

PAUSE
```

Output

```txt
Assertion: 1>2 failed

Press any key to continue...
```



## Forth


```fsharp
variable a
: assert   a @ 42 <> throw ;

41 a ! assert
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64
' requires compilation with -g switch

Dim a As Integer = 5
Assert(a = 6)
'The rest of the code will not be executed
Print a
Sleep
```


{{out}}

```txt

assert.bas(5): assertion failed at __FB_MAINPROC__: a =6

```


=={{header|F_Sharp|F#}}==
F# provides an ''assert'' function that is only enabled when the program is compiled with ''DEBUG'' defined. When an assertion fails, a dialog box is shown with the option to enter the debugger.

```fsharp
let test x =
  assert (x = 42)

test 43
```


For additional information about assertions in .NET, see [[#C# and Visual Basic .NET]]


## GAP


```gap
# See section 7.5 of reference manual

# GAP has assertions levels. An assertion is tested if its level
# is less then the global level.

# Set global level
SetAssertionLevel(10);

a := 1;
Assert(20, a > 1, "a should be greater than one");
# nothing happens

a := 1;
Assert(4, a > 1, "a should be greater than one");
# error

# Show current global level
AssertionLevel();
# 10
```



## Go

Assertions are a feature [http://golang.org/doc/go_faq.html#assertions consciously omitted] from Go.  For cases where you want feedback during development, the following code should provide a similar purpose.  While it is simply an if statement and a panic, the technique does have some properties typical of assertions.  For one, the predicate of an if statement in Go is required to be of boolean type.  Specifically, ints are not tacitly tested for zero, pointers are not tested for nil:  the expression must be boolean, as the WP article mentions is typical of assertions.  Also, it provides a good amount of information should the predicate evaluate to true.  First, a value of any type can be passed to the panic, and by default is displayed, followed by a stack trace which includes the location of the panic in the source code&mdash;function name, file name, and line number.

```go
package main

func main() {
    x := 43
    if x != 42 {
        panic(42)
    }
}
```

Output:

```txt

panic: 42

panic PC=0x2b772d1a1048
runtime.panic+0xa7 /pool/go/src/pkg/runtime/proc.c:1032
        runtime.panic(0x40e820, 0x2a)
main.main+0x48 /pool/test.go:8
        main.main()
runtime.mainstart+0xf /pool/go/src/pkg/runtime/amd64/asm.s:77
        runtime.mainstart()
runtime.goexit /pool/go/src/pkg/runtime/proc.c:148
        runtime.goexit()

```



## Groovy


```groovy
def checkTheAnswer = {
   assert it == 42 : "This: " + it + " is not the answer!"
}
```


Test program:

```groovy
println "before 42..."
checkTheAnswer(42)
println "before 'Hello Universe'..."
checkTheAnswer("Hello Universe")
```


Output:

```txt
before 42...
before 'Hello Universe'...
java.lang.AssertionError: This: Hello Universe is not the answer!. Expression: (it == 42). Values: it = Hello Universe
	at ConsoleScript80$_run_closure1.doCall(ConsoleScript80:2)
	at ConsoleScript80.run(ConsoleScript80:8)
```



## Haskell


```haskell
import Control.Exception

main = let a = someValue in
         assert (a == 42) -- throws AssertionFailed when a is not 42
                somethingElse -- what to return when a is 42
```


=={{header|Icon}} and {{header|Unicon}}==


```Icon
...
runerr(n,( expression ,"Assertion/error - message."))  # Throw (and possibly trap) an error number n if expression succeeds.
...
stop(( expression ,"Assertion/stop - message."))       # Terminate program if expression succeeds.
...
```


There are no 'assertions', which can be turned on/off by the compiler.  We can emulate them by prefixing a stop statement with a check on a global variable:


```Icon

$define DEBUG 1 # this allows the assertions to go through

procedure check (a)
  if DEBUG then stop (42 = a, " is invalid value for 'a'")
  write (a)
end

procedure main ()
  check (10)
  check (42)
  check (12)
end

```


This produces the output:

```txt

10
42 is invalid value for 'a'

```


Changing the define to: <code>$define DEBUG &fail</code> turns off the assertion checking.


## J


```j>   assert n = 42</lang



## Java


```java5
public class Assertions {

    public static void main(String[] args) {
        int a = 13;

        // ... some real code here ...

        assert a == 42;
        // Throws an AssertionError when a is not 42.

        assert a == 42 : "Error message";
        // Throws an AssertionError when a is not 42,
        // with "Error message" for the message.
        // The error message can be any non-void expression.
    }
}
```


Note: assertion checking is disabled by default when you run your program with the <tt>java</tt> command. You must provide the <tt>-ea</tt> (short for <tt>-enableassertions</tt>) flag in order to enable them.


## Julia


```julia
const x = 5

# @assert macro checks the supplied conditional expression, with the expression
# returned in the failed-assertion message
@assert x == 42
# ERROR: LoadError: AssertionError: x == 42

# Julia also has type assertions of the form, x::Type which can be appended to
# variable for type-checking at any point
x::String
# ERROR: LoadError: TypeError: in typeassert, expected String, got Int64

```



## Kotlin

Assertions need to be enabled using java's -ea option for an AssertionError to be thrown when the condition is false.

```scala
// version 1.0.6 (assert.kt)

fun main(args: Array<String>) {
   val a = 42
   assert(a == 43)
}
```


{{out}}

```txt

Exception in thread "main" java.lang.AssertionError: Assertion failed
        at AssertKt.main(assert.kt:5)

```



## Lasso


```lasso
local(a) = 8
fail_if(
    #a != 42,
    error_code_runtimeAssertion,
    error_msg_runtimeAssertion + ": #a is not 42"
)
```

{{out}}

```txt
-9945 Runtime assertion: #a is not 42
```



## Liberty BASIC

Liberty BASIC has no exceptions or user-defined error messages,
but we could break program if condition is not met.
We can even make it spell "AssertionFailed". In a way.

```lb

a=42
call assert a=42
print "passed"

a=41
call assert a=42
print "failed (we never get here)"
end

sub assert cond
    if cond=0 then 'simulate error, mentioning "AssertionFailed"
        AssertionFailed(-1)=0
    end if
end sub

```


{{out}}

```txt

passed

```


Stops with error message
{{out}}

```txt

RuntimeError: Subscript out of range: -1, AssertionFailed()

```



## Lingo

Lingo has no assert statement, but the abort command (that exits the full call stack) allows to implement something like it as global function:

```lingo
-- in a movie script
on assert (ok, message)
  if not ok then
    if not voidP(message) then _player.alert(message)
    abort -- exits from current call stack, i.e. also from the caller function
  end if
end

-- anywhere in the code
on test
  x = 42
  assert(x=42, "Assertion 'x=42' failed")
  put "this shows up"
  x = 23
  assert(x=42, "Assertion 'x=42' failed")
  put "this will never show up"
end
```



## Lisaac


```Lisaac
? { n = 42 };
```



## Lua


```lua
a = 5
assert (a == 42)
assert (a == 42,'\''..a..'\' is not the answer to life, the universe, and everything')
```



## M2000 Interpreter

M2000 use Error to produce error, Try variable {} and Try {} to capture errors, and return a number from a function when function called as module (zero means no error).

Trapping error may leave current stack of values with values so if we have above try {} a block of Stack New {} then we get old stack after exit of Stack New {} (this statement hold current stack, attach a new stack of value, and at the exit restore old stack). Another way is to use Flush which clear stack.  Statement Flush Error clear all level of error information.



```M2000 Interpreter

Module Assert {
      \\ This is a global object named Rec
      Global Group Rec {
      Private:
            document doc$="Error List at "+date$(today)+" "+time$(now)+{
            }
      Public:
            lastfilename$="noname.err"
            Module Error(a$) {
                  if a$="" then exit
                  .doc$<="     "+a$+{
                  }
                  flush error
            }
            Module Reset {
                  Clear .doc$
            }
            Module Display {
                  Report  .doc$
            }
            Module SaveIt {
                  .lastfilename$<=replace$("/", "-","Err"+date$(today)+time$(now)+".err")
                  Save.Doc .doc$,.lastfilename$
            }
      }
      Module Checkit {
            Function Error1 (x) {
                  if x<10 then  Print "Normal" : exit
                  =130   ' error code
            }
            Call Error1(5)
            Try ok {
                  Call Error1(100)
            }
            If not Ok then Rec.Error Error$ : Flush Error

            Test "breakpoint A"   ' open Control form, show code as executed, press next or close it

            Try {
                  Report "Run this"
                  Error "Hello"
                  Report "Not run this"
            }
            Rec.Error Error$

            Module Error1 (x) {
                  if x<10 then  Print "Normal" : exit
                  Error "Big Error"
            }
            Try ok {
                   Error1 100
            }
            If Error then Rec.Error Error$
      }
      Checkit
      Rec.Display
      Rec.SaveIt
      win "notepad.exe", dir$+Rec.lastfilename$
}
Assert

```




## Maple

(Taken from Lua, above.)

```Maple
a := 5:
ASSERT( a = 42 );
ASSERT( a = 42, "a is not the answer to life, the universe, and everything" );
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==


```Mathematica
Assert[var===42]
```


=={{header|MATLAB}} / {{header|Octave}} ==


```MATLAB
assert(x == 42,'x = %d, not 42.',x);
```


Sample Output:

```MATLAB
x = 3;
assert(x == 42,'Assertion Failed: x = %d, not 42.',x);
??? Assertion Failed: x = 3, not 42.

```



## Metafont


Metafont has no really an assert built in, but it can easily created:


```metafont
def assert(expr t) = if not (t): errmessage("assertion failed") fi enddef;
```


This <code>assert</code> macro uses the <code>errmessage</code> built in to show the "error". The
<code>errmessage</code> gives the error message and asks the user what to do.

Usage example:


```metafont
n := 41;
assert(n=42);
message "ok";
```


Output (failed assertion):


```txt
This is METAFONT, Version 2.71828 (Web2C 7.5.5)
(./assert.mf
! assertion failed.
<to be read again>
                   ;
l.4 assert(n=42);

?
```


=={{header|Modula-3}}==
<code>ASSERT</code> is a pragma, that creates a run-time error if it returns <code>FALSE</code>.

```modula3
<*ASSERT a = 42*>
```


Assertions can be ignored in the compiler by using the <code>-a</code> switch.


## Nemerle

A basic assertion uses the <tt>assert</tt> keyword:

```Nemerle
assert (foo == 42, $"foo == $foo, not 42.")
```

Assertion violations throw an <tt>AssertionException</tt> with the line number where the assertion failed and the message provided as the second parameter to assert.

Nemerle also provides macros in the <tt>Nemerle.Assertions</tt> namespace to support preconditions, postconditions and class invariants:

```Nemerle
using Nemerle.Assertions;

class SampleClass
{
	public SomeMethod (input : list[int]) : int
	  requires input.Length > 0                 // requires keyword indicates precondition,
                                                    // there can be more than one condition per method
	{ ... }

	public AnotherMethod (input : string) : list[char]
	  ensures value.Length > 0                  // ensures keyword indicates postcondition
	{ ... }                                     // value is a special symbol that indicates the method's return value
}
```

The design by contract macros throw <tt>Nemerle.AssertionException</tt>'s unless another Exception is specified using the <tt>otherwise</tt> keyword after the <tt>requires/ensures</tt> statement.
For further details on design by contract macros, see [http://nemerle.org/wiki/index.php?title=Design_by_contract_macros here].


## Nim


```Nim
var a = 42
assert(a == 42, "Not 42!")
```

Assertions may be disabled by compiling with --assertions:off.

=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE Assertions;
VAR
	a: INTEGER;
BEGIN
	a := 40;
	ASSERT(a = 42);
END Assertions.

```

Output:

```txt

Runtime error: assertion failed (0) on line 6 in module Assertions
In procedure Assertions.%main
   called from MAIN

```



## Objeck

If variable is not equal to 42 a stack trace is generated and the program is halts.

```objeck
class Test {
  function : Main(args : String[]) ~ Nil {
    if(args->Size() = 1) {
      a := args[0]->ToInt();
      Runtime->Assert(a = 42);
    };
  }
}

```


=={{header|Objective-C}}==
For use within an Objective-C method:

```objc
NSAssert(a == 42, @"Error message");
```


If you want to use formatting arguments, you need to use the assertion macro corresponding to your number of formatting arguments:

```objc
NSAssert1(a == 42, @"a is not 42, a is actually %d", a); # has 1 formatting arg, so use NSAssert"1"
```


Within a regular C function you should use <code>NSCAssert</code> or <code>NSCAssert''N''</code> instead.

To turn off assertions, define the <tt>NS_BLOCK_ASSERTIONS</tt> macro.


## OCaml


```ocaml
let a = get_some_value () in
  assert (a = 42); (* throws Assert_failure when a is not 42 *)
  (* evaluate stuff to return here when a is 42 *)
```


It is possible to compile with the parameter <code>-noassert</code> then the compiler won't compile the assertion checks.


## Oforth


In Oforth, assertions are handled as tests.

Assertions are checked only if oforth is launched using --a command line. Default value is to not check assertions.

If an assertion is ko (and if oforth is launched using --a), an exception is raised.


```Oforth
: testInteger(n, m)
   assert: [ n isInteger ]
   assert: [ n 42 == ]

   System.Out "Assertions are ok, parameters are : " << n << ", " << m << cr ;
```


{{out}}

```txt

testInteger(41, 43)
[1:interpreter] ExRuntime : Assertion failed into <#testInteger>

testInteger(42, 43)
Assertions are ok, parameters are : 42, 43

```



## Oz

Oz does not have an assert statement. But if different values are assigned to the same dataflow variable, an exception will be thrown (similar to Erlang).


```oz
declare
  proc {PrintNumber N}
     N=42  %% assert
     {Show N}
  end
in
  {PrintNumber 42} %% ok
  {PrintNumber 11} %% throws
```


Output:

```txt
%***************************** failure **************************
%**
%** Tell: 11 = 42
%**
%** Call Stack:
%** procedure 'PrintNumber' in file "Oz<8>", line 3, column 0, PC = 18600220
%**--------------------------------------------------------------

```



## PARI/GP

PARI can use any of the usual C methods for making assertions.  GP has no built-in assertions.
{{trans|C}}

```C>#include <assert.h

#include <pari/pari.h>

void
test()
{
  GEN a;
  // ... input or change a here

  assert(equalis(a, 42)); /* Aborts program if a is not 42, unless the NDEBUG macro was defined */
}
```


More common is the use of <code>pari_err_BUG</code> in such cases:

```C
if (!equalis(a, 42)) pari_err_BUG("this_function_name (expected a = 42)");
```



## Pascal

See [[Assertions#Delphi | Delphi]]


## Perl

While not exactly an assertion, a common Perl idiom is to use <code>or die</code> to throw an exception when a certain statement is false.


```perl
print "Give me a number: ";
chomp(my $a = <>);

$a == 42 or die "Error message\n";

# Alternatives
die "Error message\n" unless $a == 42;
die "Error message\n" if not $a == 42;
die "Error message\n" if $a != 42;
```


This idiom is typically used during file operations:

```perl
open my $fh, '<', 'file'
    or die "Cannot open file: $!\n"; # $! contains the error message from the last error
```

It is not needed whith the "autodie" pragma:

```perl
use autodie;
open my $fh, '<', 'file'; # automatically throws an exception on failure
```


Some third-party modules provide other ways of using assertions in Perl:

```perl
use Carp::Assert;
assert($a == 42);
```


There is also a number of ways to test assertions in test suites, for example:

```perl
is $a, 42;
ok $a == 42;
cmp_ok $a, '==', 42, 'The answer should be 42';
# etc.
```



## Perl 6


```perl6
my $a = (1..100).pick;
$a == 42 or die '$a ain\'t 42';
```


{{works with|pugs}}
''Note: This example uses an experimental feature, and does not work in the primary Perl 6 compiler, Rakudo.''

```perl6
# with a (non-hygienic) macro
macro assert ($x) { "$x or die 'assertion failed: $x'" }
assert('$a == 42');
```



## Phix

User defined types allow the value to be automatically tested whenever it changes, and
can be disabled using the "without type_check" directive:

```Phix
type int42(object i)
    return i=42
end type

int42 i

i = 41 -- type-check failure
```

When a type check occurs, program execution halts and if the program was run from the
editor, it automatically jumps to the offending source file and line.

Note that, under "without type_check", the run-time reserves the right to continue to
perform limited type checking, for example were the type declared as int42(integer i)
then ensuring that i is an integer may allow subsequent optimisations to be applied,
and therefore, despite the compiler directive, integer() could still be enforced even
though "=42" would not.

You can also use constants to reduce code output on release versions:

```Phix
global constant DEBUG = 0  -- (or any other identifier name can be used)
global procedure assert(integer flag, string msg)
    if DEBUG then
        if not flag then
            {} = message_box(msg,"failed assertion",MB_OK)  -- or
            puts(1,msg)                                     -- , and/or
            crash(msg)      -- crash/ex.err report          -- or
            trace(1)        -- start debugging
        end if
    end if
end function

assert(i=42,"i is not 42!!")
```

Note that while the body of assert() and the call to it are suppressed, the calculation
of the expression (i=42) may still generate code; sometimes further improvements to the
compiler may be possible, sometimes the asserts may need "if DEBUG" around them. Also
note that, as things stand, the constants 42 and "i is not 42!!" will be created in the
executable file whatever DEBUG is set to, though again there is nothing to prevent the
compiler from being enhanced to avoid emitting such unnecessary values, one day.

Lastly, I find the following trivial idioms to be spectacularly effective in Phix, the
first line terminates with a divide by zero, whereas the second produces a slightly more
user-friendly, and therefore potentially less developer-friendly message:

```Phix
if i!=42 then ?9/0 end if
if i!=42 then crash("i is not 42!!") end if
```

Again, if the application was run from Edita, on error it automatically jumps to the
offending file and line.


## PHP


```php
<?php
$a = 5
#...input or change $a here
assert($a == 42) # when $a is not 42, take appropriate actions,
                 # which is set by assert_options()
?>
```



## PicoLisp

The '[http://software-lab.de/doc/refA.html#assert assert]' function, in
combination with the tilde read macro, generates code only in debug mode:

```PicoLisp
...
~(assert (= N 42))  # Exists only in debug mode
...
```

Other possibilities are either to break into an error handler:

```PicoLisp
(let N 41
   (unless (= N 42) (quit "Incorrect N" N)) )  # 'quit' throws an error
41 -- Incorrect N
?
```

or to stop at a debug break point, allowing to continue with the program:

```PicoLisp
(let N 41
   (unless (= N 42) (! setq N 42)) )   # '!' is a breakpoint
(setq N 42)                            # Manually fix the value
!                                      # Hit ENTER to leave the breakpoint
-> 42
```



## PL/I

<lang>
/* PL/I does not have an assert function as such, */
/* but it is something that can be implemented in */
/* any of several ways.  A straight-forward way   */
/* raises a user-defined interrupt. */

on condition (assert_failure) snap
   put skip list ('Assert failure');
....
if a ^= b then signal condition(assert_failure);

/* Another way is to use the preprocessor, thus: */
%assert: procedure (a, b) returns (character);
   return ('if ' || a || '^=' || b ||
      ' then signal condition(assert_failure);');
%end assert;
%activate assert;

assert(a, 42);

```



## Prolog

{{works with|SWI Prolog}}


```prolog

test(A):-
    assertion(A==42).

```



## PureBasic

PureBasic does not have a native function for assertion, but allows for the definition of one.

The Macro below will only be included in the code if is compiled in debug mode, if so it will test the condition and if it fails it will inform with the message defined by the programmer, the line where it happened and in which source code file.


```PureBasic
Macro Assert(TEST,MSG="Assert: ")
  CompilerIf #PB_Compiler_Debugger
    If Not (TEST)
      Debug MSG+" Line="+Str(#PB_Compiler_Line)+" in "+#PB_Compiler_File
      CallDebugger
    EndIf
  CompilerEndIf
EndMacro
```


A implementation as defined above could be;

```PureBasic
A=42
Assert(A=42,"Assert that A=42")
A=42-1
Assert(A=42)
```

Where the second test would fail resulting in a message to the programmer with cause (if given by programmer), code line & file.


## Python


```python
a = 5
#...input or change a here
assert a == 42 # throws an AssertionError when a is not 42
assert a == 42, "Error message" # throws an AssertionError
       # when a is not 42 with "Error message" for the message
       # the error message can be any expression
```

It is possible to turn off assertions by running Python with the <tt>-O</tt> (optimizations) flag.


## R


```R
stopifnot(a==42)
```



## Racket


Racket has higher-order assertions known as ''contracts'' that can protect any values including functions and objects. Contracts are typically applied on the imports or exports of a module.


```Racket
#lang racket

(define/contract x
  (=/c 42) ; make sure x = 42
  42)

(define/contract f
  (-> number? (or/c 'yes 'no)) ; function contract
  (lambda (x)
    (if (= 42 x) 'yes 'no)))

(f 42)    ; succeeds
(f "foo") ; contract error!

```


If typical assertion checking (i.e. error unless some boolean condition holds) is needed, that is also possible:


```Racket
#lang racket

(define x 80)
(unless (= x 42)
  (error "a is not 42")) ; will error

```



## REXX


```REXX
/* REXX ***************************************************************
* There's no assert feature in Rexx. That's how I'd implement it
* 10.08.2012 Walter Pachl
**********************************************************************/
x.=42
x.2=11
Do i=1 By 1
  Call assert x.i,42
  End
Exit
assert:
  Parse Arg assert_have,assert_should_have
  If assert_have\==assert_should_have Then Do
    Say 'Assertion fails in line' sigl
    Say 'expected:' assert_should_have
    Say '   found:' assert_have
    Say sourceline(sigl)
    Say 'Look around'
    Trace ?R
    Nop
    Signal Syntax
    End
  Return
Syntax: Say 'program terminated'
```

Output:

```txt

Assertion fails in line 8
expected: 42
   found: 11
  Call assert x.i,42
Look around
    Here I enter Say i
2
    and then I press just enter
program terminated

```



## RLaB

RLaB does not have a special function to deal with assertions. The following workaround will do the trick:


```RLaB

// test if 'a' is 42, and if not stop the execution of the code and print
// some error message
if (a != 42)
{
  stop("a is not 42 as expected, therefore I stop until this issue is resolved!");
}

```



## Ring


```ring

x = 42
assert( x = 42 )
assert( x = 100 )

```



## Ruby

This uses test/unit from the standard library.


```ruby
require "test/unit/assertions"
include Test::Unit::Assertions

n = 5
begin
  assert_equal(42, n)
rescue Exception => e
  # Ruby 1.8: e is a Test::Unit::AssertionFailedError
  # Ruby 1.9: e is a MiniTest::Assertion
  puts e
end
```


Output:
```txt
<42> expected but was
<5>.
```



## Rust


```rust

let x = 42;
assert!(x == 42);
assert_eq!(x, 42);

```



## Sather


```sather
class MAIN is
  main is
    i ::= 41;
    assert i = 42; -- fatal
    -- ...
  end;
end;
```


(The current GNU Sather compiler v1.2.3 I am using to test the code seems to ignore the assertion and no fatal error is raised, despite Sather should, see e.g. [http://www.gnu.org/software/sather/docs-1.2/tutorial/safety2208.html here]).


## Scala

These two are the same thing, and are tagged <code>@elidable(ASSERTION)</code>:

```scala
assert(a == 42)
assert(a == 42, "a isn't equal to 42")
assume(a == 42)
assume(a == 42, "a isn't equal to 42")
```


The next one does the same thing as above, but it is not tagged. Often used as a pre-condition
checker on class constructors.

```scala
require(a == 42)
require(a == 42, "a isn't equal to 42")
```


This one checks a value and returns it for further use (here shown being printed). It
uses <code>assert</code>, which, as explained, gets tagged.

```scala
println(a.ensuring(a == 42))
println(a.ensuring(a == 42, "a isn't equal to 42"))
println(a.ensuring(_ == 42))
println(a.ensuring(_ == 42, "a isn't equal to 42"))
```



## Scheme

{{Works with|Scheme|R<math>^6</math>RS}}

{{trans|Common Lisp}}

```scheme
(let ((x 42))
  (assert (and (integer? x) (= x 42))))
```



## SETL


```ada
assert( n = 42 );
```



## Sidef


```ruby
var num = pick(0..100);
assert_eq(num, 42);         # dies when "num" is not 42
```

{{out}}

```txt

assert_eq: 26 == 42 is false at assertions.sf line 2.

```



## Slate


```slate
load: 'src/lib/assert.slate'.
define: #n -> 7.
assert: n = 42 &description: 'That is not the Answer.'.
```

raises an <tt>AssertionFailed</tt> condition (an <tt>Error</tt>).


## Smalltalk


```smalltalk
foo := 41.
...
self assert: (foo == 42).
```


In TestCase and subclasses, a number of check methods are inherited; among them:

```smalltalk
self assert: (... somethingMustEvaluateToTrue.. )
self should:[ some code ] raise: someException "ensures that an exception is raised
```


{{works with|Smalltalk/X}}
Object also implements assert:; these are evaluated dynamically, but can be disabled via a flag setting. Also the compiler can be instructed to ignore them for production code (which is not normally done; disabled instead by default):

```smalltalk
self assert: (... somethingMustEvaluateToTrue.. ) "implemented in Object"
```

the implementation in Object raises an AssertionFailedError exception, which usually opens a debugger when in the IDE, but can be caught in deployed apps.


## SPARK

Works with SPARK GPL 2010

Assertions are analysed statically, before compilation or execution.  They can appear in various places:
:inline in the code, either

```ada
-# check X = 42;
```

::or

```ada
-# assert X = 42;
```

:as a precondition on an operation:

```ada
procedure P (X : in out Integer);
--# derives X from *;
--# pre  X = 42;
```

:or as a postcondition on an operation:

```ada
procedure P (X : in out Integer);
--# derives X from *;
--# post X = 42;
```

Example:

```ada
X := 7;
--# check X = 42;
```

produces the following output:

```txt
H1:    true .
       ->
C1:    false .
```

which is an unprovable theorem that tells you that there is a guaranteed failure.


## Stata

Assertions in Stata are limited to checking a property on the observations of a dataset. See '''[http://www.stata.com/help.cgi?assert assert]''' in Stata help.

For instance, if a dataset contains two variables x, y, z, one can check if x<y for all data lines for which z>0, with:


```stata>assert x<y if z>0</lang


There is another command, '''[http://www.stata.com/help.cgi?confirm confirm]''', that can be used to check existence and type of program arguments or files. For instance, to check that the file titanium.dta exists:


```stata>confirm file titanium.dta</lang


If the file does not exist, an error is thrown with return code 601.

It's also possible to use '''[http://www.stata.com/help.cgi?error error]''' to throw an error if some condition is satisfied. However, this command can only print predefined error messages: it takes the error number as an argument. For instance:


```stata
if (`n'==42) error 3
* Will print "no dataset in use"
```


To print a more sensible message, one would do instead:


```stata
if (`n'==42) {
	display as error "The correct answer is not 42."
	exit 54
}
```


Then, if '''[http://www.stata.com/help.cgi?capture capture]''' is used to trap the error, the return code (here 54) can be retrieved in '''[http://www.stata.com/help.cgi?_variables _rc]'''.


## Swift


```swift
var a = 5
//...input or change a here
assert(a == 42) // aborts program when a is not 42
assert(a == 42, "Error message") // aborts program
       // when a is not 42 with "Error message" for the message
       // the error message must be a static string
```

In release mode assertion checks are turned off.


## Tcl

{{tcllib|control}}

```tcl
package require control

set x 5
control::assert {$x == 42}
```

Produces the output:

```txt
assertion failed: $x == 42
```


{{omit from|gnuplot}}
{{omit from|NSIS}}


## UNIX Shell

{{works with|bash}}
Assertions are not builtin commands, but we can add a function easily.

```bash
assert() {
    if test ! $1; then
        [[ $2 ]] && echo "$2" >&2
        exit 1
    fi
}
x=42
assert "$x -eq 42" "that's not the answer"
((x--))
assert "$x -eq 42" "that's not the answer"
echo "won't get here"
```



## Vala


```vala
int a = 42;
int b = 33;
assert (a == 42);
assert (b == 42); // will break the program with "assertion failed" error
```


## VBA


```vb
Sub test()
    Dim a As Integer
    a = 41
    Debug.Assert a = 42
End Sub
```

{{out}}
When run in the development area executing halts and highlights with yellow background the debug.assert line.


## VBScript


### =Definition=


```vb
sub Assert( boolExpr, strOnFail )
	if not boolExpr then
		Err.Raise vbObjectError + 99999, , strOnFail
	end if
end sub

```


### =Invocation=


```vb
dim i
i = 43
Assert i=42, "There's got to be more to life than this!"
```


### =Output=


```VBScript>
cscript "C:\foo\assert.vbs"
C:\foo\assert.vbs(3, 3) (null): There's got to be more to life than this!
```



## Visual Basic

VB's <code>Assert</code> only fires when run from within the IDE. When compiled, all <code>Debug</code> lines are ignored.

```vb>Debug.Assert i = 42</lang



## Visual Basic .NET

See [[#C# and Visual Basic .NET]].


## XPL0

XPL0 does not have an assert command. The equivalent is usually
synthesized something like this.


```XPL0
proc Fatal(Str);        \Display error message and terminate program
char Str;
[\return;                uncomment this if "assertions" are to be disabled
SetVid(3);              \set normal text display if program uses graphics
Text(0, Str);           \display error message
ChOut(0, 7);            \sound the bell
exit 1;                 \terminate the program; pass optional error code to DOS
];

if X#42 then Fatal("X#42");
```



## Yabasic


```Yabasic
sub assert(a)
	if not a then
		error "Assertion failed"
	end if
end sub

assert(myVar = 42)
```



## Zig


```zig
const assert = @import("std").debug.assert;

pub fn main() void {
    assert(1 == 0); // On failure, an `unreachable` is reached
}
```


Zig's assert gives a stack trace for debugging on failure.


## zkl


```zkl
n:=42; (n==42) or throw(Exception.AssertionError);
n=41;  (n==42) or throw(Exception.AssertionError("I wanted 42!"));
```

{{out}}

```txt

Stack trace for VM#1 ():
   Cmd.__constructor addr:38  args(0) reg(1)
   startup.__constructor addr:2242  args(0) reg(1) ER
   startup.__constructor addr:2178  args(0) reg(22)
Exception thrown: AssertionError(I wanted 42!)

```



## zonnon


```zonnon

module Assertions;
var
	a: integer;
begin
	a := 40;
	assert(a = 42,100)
end Assertions.

```

