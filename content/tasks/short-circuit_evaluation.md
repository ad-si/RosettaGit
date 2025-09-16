+++
title = "Short-circuit evaluation"
description = ""
date = 2019-10-18T19:22:42Z
aliases = []
[extra]
id = 7806
[taxonomies]
categories = ["task", "Programming language concepts"]
tags = []
languages = [
  "ada",
  "algol_68",
  "algol_w",
  "applescript",
  "autohotkey",
  "awk",
  "axe",
  "bacon",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "c_shell",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dyalect",
  "e",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "livecode",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "mumps",
  "nemerle",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pike",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sather",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "simula",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "txr",
  "unix_shell",
  "vba",
  "visual_basic_dotnet",
  "visual_foxpro",
  "zkl",
]
+++

Assume functions   <code>a</code>   and   <code>b</code>   return boolean values,   and further, the execution of function   <code>b</code>   takes considerable resources without side effects, <!--treating the printing as being for illustrative purposes only--> and is to be minimized.

If we needed to compute the conjunction   (<code>and</code>):
:::: <code> x = a() and b() </code>

Then it would be best to not compute the value of   <code>b()</code>   if the value of   <code>a()</code>    is computed as   <code>false</code>,   as the value of   <code>x</code>   can then only ever be   <code> false</code>.

Similarly, if we needed to compute the disjunction (<code>or</code>):
:::: <code> y = a() or b() </code>

Then it would be best to not compute the value of   <code>b()</code>   if the value of   <code>a()</code>   is computed as   <code>true</code>,   as the value of   <code>y</code>   can then only ever be   <code>true</code>.

Some languages will stop further computation of boolean equations as soon as the result is known, so-called   [[wp:Short-circuit evaluation|short-circuit evaluation]]   of boolean expressions


## Task

Create two functions named   <code>a</code>   and   <code>b</code>,   that take and return the same boolean value.

The functions should also print their name whenever they are called.

Calculate and assign the values of the following equations to a variable in such a way that function   <code>b</code>   is only called when necessary:
:::: <code> x = a(i) and b(j) </code>
:::: <code> y = a(i) or  b(j) </code>


If the language does not have short-circuit evaluation, this might be achieved with nested     '''if'''     statements.





## Ada

Ada has built-in short-circuit operations '''and then''' and '''or else''':

```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Short_Circuit is
   function A (Value : Boolean) return Boolean is
   begin
      Put (" A=" & Boolean'Image (Value));
      return Value;
   end A;
   function B (Value : Boolean) return Boolean is
   begin
      Put (" B=" & Boolean'Image (Value));
      return Value;
   end B;
begin
   for I in Boolean'Range loop
      for J in Boolean'Range loop
         Put (" (A and then B)=" & Boolean'Image (A (I) and then B (J)));
         New_Line;
      end loop;
   end loop;
   for I in Boolean'Range loop
      for J in Boolean'Range loop
         Put (" (A or else B)=" & Boolean'Image (A (I) or else B (J)));
         New_Line;
      end loop;
   end loop;
end Test_Short_Circuit;
```

```txt

 A=FALSE (A and then B)=FALSE
 A=FALSE (A and then B)=FALSE
 A=TRUE B=FALSE (A and then B)=FALSE
 A=TRUE B=TRUE (A and then B)=TRUE
 A=FALSE B=FALSE (A or else B)=FALSE
 A=FALSE B=TRUE (A or else B)=TRUE
 A=TRUE (A or else B)=TRUE
 A=TRUE (A or else B)=TRUE

```



## ALGOL 68


### With Standard

Note: The "brief" ''conditional clause'' ( ~ | ~ |  ~ ) is a the standard's ''shorthand'' for enforcing ''short-circuit evaluation''.  Moreover, the coder is able to define their own '''proc'''[edures] and '''op'''[erators] that implement ''short-circuit evaluation'' by using Algol68's ''proceduring''.

```algol68
PRIO ORELSE = 2, ANDTHEN = 3; # user defined operators #
OP ORELSE =  (BOOL a, PROC BOOL b)BOOL: ( a | a | b ),
   ANDTHEN = (BOOL a, PROC BOOL b)BOOL: ( a | b | a );

# user defined Short-circuit_evaluation procedures #
PROC or else =  (BOOL a, PROC BOOL b)BOOL: ( a | a | b ),
     and then = (BOOL a, PROC BOOL b)BOOL: ( a | b | a );

test:(

  PROC a = (BOOL a)BOOL: ( print(("a=",a,", ")); a),
       b = (BOOL b)BOOL: ( print(("b=",b,", ")); b);

CO
# Valid for Algol 68 Rev0: using "user defined" operators #
# Note: here BOOL is being automatically "procedured" to PROC BOOL #
  print(("T ORELSE F = ", a(TRUE) ORELSE b(FALSE), new line));
  print(("F ANDTHEN T = ", a(FALSE) ANDTHEN b(TRUE), new line));

  print(("or else(T, F) = ", or else(a(TRUE), b(FALSE)), new line));
  print(("and then(F, T) = ", and then(a(FALSE), b(TRUE)), new line));
END CO

# Valid for Algol68 Rev1: using "user defined" operators #
# Note: BOOL must be manually "procedured" to PROC BOOL #
  print(("T ORELSE F = ",  a(TRUE) ORELSE  (BOOL:b(FALSE)), new line));
  print(("T ORELSE T = ",  a(TRUE) ORELSE  (BOOL:b(TRUE)), new line));

  print(("F ANDTHEN F = ", a(FALSE) ANDTHEN (BOOL:b(FALSE)), new line));
  print(("F ANDTHEN T = ", a(FALSE) ANDTHEN (BOOL:b(TRUE)), new line));

  print(("F ORELSE F = ",  a(FALSE) ORELSE  (BOOL:b(FALSE)), new line));
  print(("F ORELSE T = ",  a(FALSE) ORELSE  (BOOL:b(TRUE)), new line));

  print(("T ANDTHEN F = ", a(TRUE) ANDTHEN (BOOL:b(FALSE)), new line));
  print(("T ANDTHEN T = ", a(TRUE) ANDTHEN (BOOL:b(TRUE)), new line))

)
```

```txt

a=T, T ORELSE F = T
a=T, T ORELSE T = T
a=F, F ANDTHEN F = F
a=F, F ANDTHEN T = F
a=F, b=F, F ORELSE F = F
a=F, b=T, F ORELSE T = T
a=T, b=F, T ANDTHEN F = F
a=T, b=T, T ANDTHEN T = T

```



### With Extensions

```algol68
test:(

  PROC a = (BOOL a)BOOL: ( print(("a=",a,", ")); a),
       b = (BOOL b)BOOL: ( print(("b=",b,", ")); b);

# Valid for Algol 68G and 68RS using non standard operators #
  print(("T OREL F = ",  a(TRUE) OREL  b(FALSE), new line));
  print(("T OREL T = ",  a(TRUE) OREL  b(TRUE), new line));

  print(("F ANDTH F = ", a(FALSE) ANDTH b(FALSE), new line));
  print(("F ANDTH T = ", a(FALSE) ANDTH b(TRUE), new line));

  print(("F OREL F = ",  a(FALSE) OREL  b(FALSE), new line));
  print(("F OREL T = ",  a(FALSE) OREL  b(TRUE), new line));

  print(("T ANDTH F = ", a(TRUE) ANDTH b(FALSE), new line));
  print(("T ANDTH T = ", a(TRUE) ANDTH b(TRUE), new line))

CO;
# Valid for Algol 68G and 68C using non standard operators #
  print(("T ORF F = ", a(TRUE) ORF b(FALSE), new line));
  print(("F ANDF T = ", a(FALSE) ANDF b(TRUE), new line))
END CO

)
```

```txt

a=T, T OREL F = T
a=T, T OREL T = T
a=F, F ANDTH F = F
a=F, F ANDTH T = F
a=F, b=F, F OREL F = F
a=F, b=T, F OREL T = T
a=T, b=F, T ANDTH F = F
a=T, b=T, T ANDTH T = T

```



## ALGOL W

In Algol W the boolean "and" and "or" operators are short circuit operators.

```algolw
begin

    logical procedure a( logical value v ) ; begin write( "a: ", v ); v end ;
    logical procedure b( logical value v ) ; begin write( "b: ", v ); v end ;

    write( "and: ", a( true  ) and b( true ) );
    write( "---" );
    write( "or:  ", a( true  ) or  b( true ) );
    write( "---" );
    write( "and: ", a( false ) and b( true ) );
    write( "---" );
    write( "or:  ", a( false ) or  b( true ) );
    write( "---" );

end.
```

```txt

and:
a:   true
b:   true    true
---
or:
a:   true    true
---
and:
a:  false   false
---
or:
a:  false
b:   true    true
---

```




## AppleScript


AppleScript's boolean operators are short-circuiting (as can be seen from the log below).

What AppleScript lacks, however, is a short-circuiting ternary operator like the '''e ? e2 : e3''' of C, or a short-circuiting three-argument function like '''cond''' in Lisp.  To get a similar effect in AppleScript, we have to delay evaluation on both sides, using a '''cond''' which returns a reference to one of two unapplied handlers, and composing the result with a separate '''apply''' function, to apply the selected handler function to its argument.

(As a statement, rather than an expression, the ''if ... then ... else'' structure does not compose – unlike ''cond'' or ''? :'', it can not be nested inside expressions)


```AppleScript
on run

    map(test, {|and|, |or|})

end run

-- test :: ((Bool, Bool) -> Bool) -> (Bool, Bool, Bool, Bool)
on test(f)
    map(f, {{true, true}, {true, false}, {false, true}, {false, false}})
end test



-- |and| :: (Bool, Bool) -> Bool
on |and|(tuple)
    set {x, y} to tuple

    a(x) and b(y)
end |and|

-- |or| :: (Bool, Bool) -> Bool
on |or|(tuple)
    set {x, y} to tuple

    a(x) or b(y)
end |or|

-- a :: Bool -> Bool
on a(bool)
    log "a"
    return bool
end a

-- b :: Bool -> Bool
on b(bool)
    log "b"
    return bool
end b


-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    script mf
        property lambda : f
    end script
    set lng to length of xs
    set lst to {}
    repeat with i from 1 to lng
        set end of lst to mf's lambda(item i of xs, i, xs)
    end repeat
    return lst
end map


```



```txt
Messages:
(*a*)
(*b*)
(*a*)
(*b*)
(*a*)
(*a*)
(*a*)
(*a*)
(*a*)
(*b*)
(*a*)
(*b*)
Result:
```



## AutoHotkey

In AutoHotkey, the boolean operators, '''and''', '''or''', and ternaries, short-circuit:

```AutoHotkey
i = 1
j = 1
x := a(i) and b(j)
y := a(i) or b(j)

a(p)
{
 MsgBox, a() was called with the parameter "%p%".
 Return, p
}

b(p)
{
 MsgBox, b() was called with the parameter "%p%".
 Return, p
}
```



## AWK

Short-circuit evalation is done in logical AND (&&) and logical OR (||) operators:

```AWK
#!/usr/bin/awk -f
BEGIN {
	print (a(1) && b(1))
	print (a(1) || b(1))
	print (a(0) && b(1))
	print (a(0) || b(1))
}


function a(x) {
	print "  x:"x
	return x
}
function b(y) {
	print "  y:"y
	return y
}
```

```txt

  x:1
  y:1
1
  x:1
1
  x:0
0
  x:0
  y:1
1

```



## Axe


```axe
TEST(0,0)
TEST(0,1)
TEST(1,0)
TEST(1,1)
Return

Lbl TEST
r₁→X
r₂→Y
Disp X▶Hex+3," and ",Y▶Hex+3," = ",(A(X)?B(Y))▶Hex+3,i
Disp X▶Hex+3," or  ",Y▶Hex+3," = ",(A(X)??B(Y))▶Hex+3,i
.Wait for keypress
getKeyʳ
Return

Lbl A
r₁
Return

Lbl B
r₁
Return
```



## BaCon

BaCon supports short-circuit evaluation.


```freebasic
' Short-circuit evaluation
FUNCTION a(f)
    PRINT "FUNCTION a"
    RETURN f
END FUNCTION

FUNCTION b(f)
    PRINT "FUNCTION b"
    RETURN f
END FUNCTION

PRINT "FALSE and TRUE"
x = a(FALSE) AND b(TRUE)
PRINT x

PRINT "TRUE and TRUE"
x = a(TRUE) AND b(TRUE)
PRINT x

PRINT "FALSE or FALSE"
y = a(FALSE) OR b(FALSE)
PRINT y

PRINT "TRUE or FALSE"
y = a(TRUE) OR b(FALSE)
PRINT y
```


```txt
prompt$ ./short-circuit
FALSE and TRUE
FUNCTION a
0
TRUE and TRUE
FUNCTION a
FUNCTION b
1
FALSE or FALSE
FUNCTION a
FUNCTION b
0
TRUE or FALSE
FUNCTION a
1
```



## Batch File

```dos
%
###  Batch Files have no booleans.
%
%
###  I will instead use 1 as true and 0 as false.
%

@echo off
setlocal enabledelayedexpansion
echo AND
for /l %%i in (0,1,1) do (
for /l %%j in (0,1,1) do (
		echo.a^(%%i^) AND b^(%%j^)
		call :a %%i
		set res=!bool_a!
		if not !res!==0 (
			call :b %%j
			set res=!bool_b!
		)
		echo.=^>	!res!
)
)

echo ---------------------------------
echo OR
for /l %%i in (0,1,1) do (
	for /l %%j in (0,1,1) do (
		echo a^(%%i^) OR b^(%%j^)
		call :a %%i
		set res=!bool_a!
		if !res!==0 (
			call :b %%j
			set res=!bool_b!
		)
		echo.=^>	!res!
	)
)
pause>nul
exit /b 0


::----------------------------------------
:a
echo.	calls func a
set bool_a=%1
goto :EOF

:b
echo.	calls func b
set bool_b=%1
goto :EOF
```

```txt
AND
a(0) AND b(0)
        calls func a
=>      0
a(0) AND b(1)
        calls func a
=>      0
a(1) AND b(0)
        calls func a
        calls func b
=>      0
a(1) AND b(1)
        calls func a
        calls func b
=>      1
---------------------------------
OR
a(0) OR b(0)
        calls func a
        calls func b
=>      0
a(0) OR b(1)
        calls func a
        calls func b
=>      1
a(1) OR b(0)
        calls func a
=>      1
a(1) OR b(1)
        calls func a
=>      1

```



## BBC BASIC

Short-circuit operators aren't implemented directly, but short-circuit AND can be simulated using cascaded IFs.  Short-circuit OR can be converted into a short-circuit AND using De Morgan's laws.

```bbcbasic
      REM TRUE is represented as -1, FALSE as 0
      FOR i% = TRUE TO FALSE
        FOR j% = TRUE TO FALSE
          PRINT "For x=a(";FNboolstring(i%);") AND b(";FNboolstring(j%);")"
          x% = FALSE
          REM Short-circuit AND can be simulated by cascaded IFs:
          IF FNa(i%) IF FNb(j%) THEN x%=TRUE
          PRINT "x is ";FNboolstring(x%)
          PRINT
          PRINT "For y=a(";FNboolstring(i%);") OR b(";FNboolstring(j%);")"
          y% = FALSE
          REM Short-circuit OR can be simulated by De Morgan's laws:
          IF NOTFNa(i%) IF NOTFNb(j%) ELSE y%=TRUE : REM Note ELSE without THEN
          PRINT "y is ";FNboolstring(y%)
          PRINT
        NEXT:NEXT
      END

      DEFFNa(bool%)
      PRINT "Function A used; ";
      =bool%

      DEFFNb(bool%)
      PRINT "Function B used; ";
      =bool%

      DEFFNboolstring(bool%)
      IF bool%=0 THEN ="FALSE" ELSE="TRUE"
```

This gives the results shown below:

```txt
For x=a(TRUE) AND b(TRUE)
Function A used; Function B used; x is TRUE

For y=a(TRUE) OR b(TRUE)
Function A used; y is TRUE

For x=a(TRUE) AND b(FALSE)
Function A used; Function B used; x is FALSE

For y=a(TRUE) OR b(FALSE)
Function A used; y is TRUE

For x=a(FALSE) AND b(TRUE)
Function A used; x is FALSE

For y=a(FALSE) OR b(TRUE)
Function A used; Function B used; y is TRUE

For x=a(FALSE) AND b(FALSE)
Function A used; x is FALSE

For y=a(FALSE) OR b(FALSE)
Function A used; Function B used; y is FALSE

```



## Bracmat

Bracmat has no booleans. The closest thing is the success or failure of an expression. A function is not called if the argument fails, so we have to use a trick to pass 'failure' to a function. Here it is accomplished by an extra level of indirection: two == in the definition of 'false' (and 'true', for symmetry) and two !! when evaluating the argument in the functions a and b. The backtick is another hack. This prefix tells Bracmat to look the other way if the backticked expression fails and to continue as if the expression succeeded. A neater way is to introduce an extra OR operator. That solution would have obscured the core of the current task.
Short-circuit evaluation is heavily used in Bracmat code. Although not required, it is a good habit to exclusively use AND (&) and OR (|) operators to separate expressions, as the code below exemplifies.

```bracmat
( (a=.out$"I'm a"&!!arg)
& (b=.out$"I'm b"&!!arg)
& (false==~)
& (true==)
& !false !true:?outer
&   whl
  ' ( !outer:%?x ?outer
    & !false !true:?inner
    &   whl
      ' ( !inner:%?y ?inner
        &   out
          $ ( Testing
              (!!x&true|false)
              AND
              (!!y&true|false)
            )
        & `(a$!x&b$!y)
        &   out
          $ ( Testing
              (!!x&true|false)
              OR
              (!!y&true|false)
            )
        & `(a$!x|b$!y)
        )
    )
& done
);

```

Output:

```txt
Testing false AND false
I'm a
Testing false OR false
I'm a
I'm b
Testing false AND true
I'm a
Testing false OR true
I'm a
I'm b
Testing true AND false
I'm a
I'm b
Testing true OR false
I'm a
Testing true AND true
I'm a
I'm b
Testing true OR true
I'm a
```



## C

Boolean operators <nowiki>&&</nowiki> and || are shortcircuit operators.

```c
#include <stdio.h>
#include <stdbool.h>

bool a(bool in)
{
  printf("I am a\n");
  return in;
}

bool b(bool in)
{
  printf("I am b\n");
  return in;
}

#define TEST(X,Y,O)						\
  do {								\
    x = a(X) O b(Y);						\
    printf(#X " " #O " " #Y " = %s\n\n", x ? "true" : "false");	\
  } while(false);

int main()
{
  bool x;

  TEST(false, true, &&); // b is not evaluated
  TEST(true, false, ||); // b is not evaluated
  TEST(true, false, &&); // b is evaluated
  TEST(false, false, ||); // b is evaluated

  return 0;
}
```



## C++

Just like C, boolean operators <nowiki>&&</nowiki> and || are shortcircuit operators.

```cpp
#include <iostream>

bool a(bool in)
{
    std::cout << "a" << std::endl;
    return in;
}

bool b(bool in)
{
    std::cout << "b" << std::endl;
    return in;
}

void test(bool i, bool j) {
    std::cout << std::boolalpha << i << " and " << j << " = " << (a(i) && b(j)) << std::endl;
    std::cout << std::boolalpha << i << " or " << j << " = " << (a(i) || b(j)) << std::endl;
}

int main()
{
    test(false, false);
    test(false, true);
    test(true, false);
    test(true, true);
    return 0;
}
```

```txt
a
false and false = false
a
b
false or false = false
a
false and true = false
a
b
false or true = true
a
b
true and false = false
a
true or false = true
a
b
true and true = true
a
true or true = true
```


## C#

```c#
using System;

class Program
{
    static bool a(bool value)
    {
        Console.WriteLine("a");
        return value;
    }

    static bool b(bool value)
    {
        Console.WriteLine("b");
        return value;
    }

    static void Main()
    {
        foreach (var i in new[] { false, true })
        {
            foreach (var j in new[] { false, true })
            {
                Console.WriteLine("{0} and {1} = {2}", i, j, a(i) && b(j));
                Console.WriteLine();
                Console.WriteLine("{0} or {1} = {2}", i, j, a(i) || b(j));
                Console.WriteLine();
            }
        }
    }
}
```

<lang>a
False and False = False

a
b
False or False = False

a
False and True = False

a
b
False or True = True

a
b
True and False = False

a
True or False = True

a
b
True and True = True

a
True or True = True
```



## Clojure

The print/println stuff in the doseq is kinda gross, but if you include them all in a single print, then the function traces are printed before the rest (since it has to evaluate them before calling print).

```Clojure
(letfn [(a [bool] (print "(a)") bool)
        (b [bool] (print "(b)") bool)]
  (doseq [i [true false] j [true false]]
    (print i "OR" j "= ")
    (println (or (a i) (b j)))
    (print i "AND" j " = ")
    (println (and (a i) (b j)))))
```

```txt
true OR true = (a)true
true AND true  = (a)(b)true
true OR false = (a)true
true AND false  = (a)(b)false
false OR true = (a)(b)true
false AND true  = (a)false
false OR false = (a)(b)false
false AND false  = (a)false
```



## Common Lisp


```lisp
(defun a (F)
     (print 'a)
     F )

(defun b (F)
     (print 'b)
     F )

(dolist (x '((nil nil) (nil T) (T T) (T nil)))
        (format t "~%(and ~S)" x)
        (and (a (car x)) (b (car(cdr x))))
        (format t "~%(or ~S)" x)
        (or (a (car x)) (b (car(cdr x)))))
```

 (and (NIL NIL))
 A
 (or (NIL NIL))
 A
 B
 (and (NIL T))
 A
 (or (NIL T))
 A
 B
 (and (T T))
 A
 B
 (or (T T))
 A
 (and (T NIL))
 A
 B
 (or (T NIL))
 A


## D

```d
import std.stdio, std.algorithm;

T a(T)(T answer) {
    writefln("  # Called function a(%s) -> %s", answer, answer);
    return answer;
}

T b(T)(T answer) {
    writefln("  # Called function b(%s) -> %s", answer, answer);
    return answer;
}

void main() {
    foreach (immutable x, immutable y;
             [false, true].cartesianProduct([false, true])) {
        writeln("\nCalculating: r1 = a(x) && b(y)");
        immutable r1 = a(x) && b(y);
        writeln("Calculating: r2 = a(x) || b(y)");
        immutable r2 = a(x) || b(y);
    }
}
```

```txt

Calculating: r1 = a(x) && b(y)
  # Called function a(false) -> false
Calculating: r2 = a(x) || b(y)
  # Called function a(false) -> false
  # Called function b(false) -> false

Calculating: r1 = a(x) && b(y)
  # Called function a(true) -> true
  # Called function b(false) -> false
Calculating: r2 = a(x) || b(y)
  # Called function a(true) -> true

Calculating: r1 = a(x) && b(y)
  # Called function a(false) -> false
Calculating: r2 = a(x) || b(y)
  # Called function a(false) -> false
  # Called function b(true) -> true

Calculating: r1 = a(x) && b(y)
  # Called function a(true) -> true
  # Called function b(true) -> true
Calculating: r2 = a(x) || b(y)
  # Called function a(true) -> true
```



## Delphi

Delphi supports short circuit evaluation by default.  It can be turned off using the {$BOOLEVAL OFF} compiler directive.

```Delphi
program ShortCircuitEvaluation;

{$APPTYPE CONSOLE}

uses SysUtils;

function A(aValue: Boolean): Boolean;
begin
  Writeln('a');
  Result := aValue;
end;

function B(aValue: Boolean): Boolean;
begin
  Writeln('b');
  Result := aValue;
end;

var
  i, j: Boolean;
begin
  for i in [False, True] do
  begin
    for j in [False, True] do
    begin
      Writeln(Format('%s and %s = %s', [BoolToStr(i, True), BoolToStr(j, True), BoolToStr(A(i) and B(j), True)]));
      Writeln;
      Writeln(Format('%s or %s = %s', [BoolToStr(i, True), BoolToStr(j, True), BoolToStr(A(i) or B(j), True)]));
      Writeln;
    end;
  end;
end.
```



## Dyalect


```dyalect
func a(v) {
  print(nameof(a), terminator: "")
  return v
}

func b(v) {
  print(nameof(b), terminator: "")
  return v
}

func test(i, j) {
  print("Testing a(\(i)) && b(\(j))")
  print("Trace: ", terminator: "")
  print("\nResult: \(a(i) && b(j))")

  print("Testing a(\(i)) || b(\(j))")
  print("Trace: ", terminator: "")
  print("\nResult: \(a(i) || b(j))")

  print()
}

test(false, false)
test(false, true)
test(true, false)
test(true, true)
```


```txt
Testing a(false) && b(false)
Trace: a
Result: false
Testing a(false) || b(false)
Trace: ab
Result: false

Testing a(false) && b(true)
Trace: a
Result: false
Testing a(false) || b(true)
Trace: ab
Result: true

Testing a(true) && b(false)
Trace: ab
Result: false
Testing a(true) || b(false)
Trace: a
Result: true

Testing a(true) && b(true)
Trace: ab
Result: true
Testing a(true) || b(true)
Trace: a
Result: true
```



## E

E defines <code>&amp;&amp;</code> and <code>||</code> in the usual short-circuiting fashion.

```e
def a(v) { println("a"); return v }
def b(v) { println("b"); return v }

def x := a(i) && b(j)
def y := b(i) || b(j)
```

Unusually, E is an expression-oriented language, and variable bindings (which are expressions) are in scope until the end of the nearest enclosing <code>{ ... }</code> block. The combination of these features means that some semantics must be given to a binding occurring inside of a short-circuited alternative.

```e
def x := a(i) && (def funky := b(j))
```

The choice we make is that <code>funky</code> is ordinary if the right-side expression was evaluated, and otherwise is <em>ruined</em>; attempts to access the variable give an error.


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

Func<bool, bool> a = (bool x){ console.writeLine:"a"; ^ x };

Func<bool, bool> b = (bool x){ console.writeLine:"b"; ^ x };

const bool[] boolValues = new bool[]::( false, true );

public program()
{
    boolValues.forEach:(bool i)
    {
        boolValues.forEach:(bool j)
        {
            console.printLine(i," and ",j," = ",a(i) && b(j));

            console.writeLine();
            console.printLine(i," or ",j," = ",a(i) || b(j));
            console.writeLine()
        }
    }
}
```

```txt

a
false and false = false

a
b
false or false = false

a
false and true = false

a
b
false or true = true

a
b
true and false = false

a
true or false = true

a
b
true and true = true

a
true or true = true

```



## Elixir


```elixir
defmodule Short_circuit do
  defp a(bool) do
    IO.puts "a( #{bool} ) called"
    bool
  end

  defp b(bool) do
    IO.puts "b( #{bool} ) called"
    bool
  end

  def task do
    Enum.each([true, false], fn i ->
      Enum.each([true, false], fn j ->
        IO.puts "a( #{i} ) and b( #{j} ) is #{a(i) and b(j)}.\n"
        IO.puts "a( #{i} ) or b( #{j} ) is #{a(i)  or b(j)}.\n"
      end)
    end)
  end
end

Short_circuit.task
```


```txt

a( true ) called
b( true ) called
a( true ) and b( true ) is true.

a( true ) called
a( true ) or b( true ) is true.

a( true ) called
b( false ) called
a( true ) and b( false ) is false.

a( true ) called
a( true ) or b( false ) is true.

a( false ) called
a( false ) and b( true ) is false.

a( false ) called
b( true ) called
a( false ) or b( true ) is true.

a( false ) called
a( false ) and b( false ) is false.

a( false ) called
b( false ) called
a( false ) or b( false ) is false.

```



## Erlang


```Erlang

-module( short_circuit_evaluation ).

-export( [task/0] ).

task() ->
	[task_helper(X, Y) || X <- [true, false], Y <- [true, false]].



a( Boolean ) ->
	io:fwrite( " a ~p~n", [Boolean] ),
	Boolean.

b( Boolean ) ->
	io:fwrite( " b ~p~n", [Boolean] ),
	Boolean.

task_helper( Boolean1, Boolean2 ) ->
	io:fwrite( "~p andalso ~p~n", [Boolean1, Boolean2] ),
	io:fwrite( "=> ~p~n", [a(Boolean1) andalso b(Boolean2)] ),
	io:fwrite( "~p orelse ~p~n", [Boolean1, Boolean2] ),
	io:fwrite( "=> ~p~n", [a(Boolean1) orelse b(Boolean2)] ).

```

```txt

15> short_circuit_evaluation:task().
true andalso true
 a true
 b true
=> true
true orelse true
 a true
=> true
true andalso false
 a true
 b false
=> false
true orelse false
 a true
=> true
false andalso true
 a false
=> false
false orelse true
 a false
 b true
=> true
false andalso false
 a false
=> false
false orelse false
 a false
 b false
=> false

```


=={{header|F_Sharp|F#}}==

```fsharp
let a (x : bool) = printf "(a)"; x
let b (x : bool) = printf "(b)"; x

[for x in [true; false] do for y in [true; false] do yield (x, y)]
|> List.iter (fun (x, y) ->
    printfn "%b AND %b = %b" x y ((a x) && (b y))
    printfn "%b OR %b = %b" x y ((a x) || (b y)))
```

Output

```txt
(a)(b)true AND true = true
(a)true OR true = true
(a)(b)true AND false = false
(a)true OR false = true
(a)false AND true = false
(a)(b)false OR true = true
(a)false AND false = false
(a)(b)false OR false = false
```



## Factor

<code>&&</code> and <code>||</code> perform short-circuit evaluation, while <code>and</code> and <code>or</code> do not. <code>&&</code> and <code>||</code> both expect a sequence of quotations to evaluate in a short-circuit manner. They are smart combinators; that is, they infer the number of arguments taken by the quotations. If you opt not to use the smart combinators, you can also use words like <code>0&&</code> and <code>2||</code> where the arity of the quotations is dictated.

```factor
USING: combinators.short-circuit.smart io prettyprint ;
IN: rosetta-code.short-circuit

: a ( ? -- ? ) "(a)" write ;
: b ( ? -- ? ) "(b)" write ;

"f && f = " write { [ f a ] [ f b ] } && .
"f || f = " write { [ f a ] [ f b ] } || .
"f && t = " write { [ f a ] [ t b ] } && .
"f || t = " write { [ f a ] [ t b ] } || .
"t && f = " write { [ t a ] [ f b ] } && .
"t || f = " write { [ t a ] [ f b ] } || .
"t && t = " write { [ t a ] [ t b ] } && .
"t || t = " write { [ t a ] [ t b ] } || .
```

```txt

f && f = (a)f
f || f = (a)(b)f
f && t = (a)f
f || t = (a)(b)t
t && f = (a)(b)f
t || f = (a)t
t && t = (a)(b)t
t || t = (a)t

```



## Fantom


```fantom
class Main
{
  static Bool a (Bool value)
  {
    echo ("in a")
    return value
  }

  static Bool b (Bool value)
  {
    echo ("in b")
    return value
  }

  public static Void main ()
  {
    [false,true].each |i|
    {
      [false,true].each |j|
      {
        Bool result := a(i) && b(j)
        echo ("a($i) && b($j): " + result)
        result = a(i) || b(j)
        echo ("a($i) || b($j): " + result)
      }
    }
  }
}
```

```txt

in a
a(false) && b(false): false
in a
in b
a(false) || b(false): false
in a
a(false) && b(true): false
in a
in b
a(false) || b(true): true
in a
in b
a(true) && b(false): false
in a
a(true) || b(false): true
in a
in b
a(true) && b(true): true
in a
a(true) || b(true): true

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Short-circuit_evaluation this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
\ Short-circuit evaluation definitions from Wil Baden, with minor name changes
:  ENDIF  postpone THEN ; immediate

:   COND  0 ; immediate
: ENDIFS  BEGIN DUP WHILE postpone ENDIF REPEAT DROP ; immediate
: ORELSE  s" ?DUP 0= IF"  evaluate ; immediate
:  ANDIF  s" DUP IF DROP" evaluate ; immediate

: .bool IF ." true  " ELSE ." false " THEN ;
: A  ." A=" DUP .bool ;
: B  ." B=" DUP .bool ;

: test
  CR
  1 -1 DO 1 -1 DO
    COND I A ANDIF  J B ENDIFS ." ANDIF="  .bool CR
    COND I A ORELSE J B ENDIFS ." ORELSE=" .bool CR
  LOOP LOOP ;

\ An alternative based on explicitly short-circuiting conditionals, Dave Keenan
: END-PRIOR-IF  1 CS-ROLL postpone ENDIF ; immediate

: test
  CR
  1 -1 DO 1 -1 DO
    I A    IF J B IF 1 ELSE END-PRIOR-IF 0 ENDIF ." ANDIF="  .bool CR
    I A 0= IF J B IF END-PRIOR-IF 1 ELSE 0 ENDIF ." ORELSE=" .bool CR
  LOOP LOOP ;
```

```txt
A=true  B=true  ANDIF=true
A=true  ORELSE=true
A=false ANDIF=false
A=false B=true  ORELSE=true
A=true  B=false ANDIF=false
A=true  ORELSE=true
A=false ANDIF=false
A=false B=false ORELSE=false
```



## Fortran

Using an <code>IF .. THEN .. ELSE</code> construct

```fortran
program Short_Circuit_Eval
  implicit none

  logical :: x, y
  logical, dimension(2) :: l = (/ .false., .true. /)
  integer :: i, j

  do i = 1, 2
    do j = 1, 2
      write(*, "(a,l1,a,l1,a)") "Calculating x = a(", l(i), ") and b(", l(j), ")"
      ! a AND b
      x = a(l(i))
      if(x) then
        x = b(l(j))
        write(*, "(a,l1)") "x = ", x
      else
        write(*, "(a,l1)") "x = ", x
      end if

      write(*,*)
      write(*, "(a,l1,a,l1,a)") "Calculating y = a(", l(i), ") or b(", l(j), ")"
      ! a OR b
      y = a(l(i))
      if(y) then
        write(*, "(a,l1)") "y = ", y
      else
        y = b(l(j))
        write(*, "(a,l1)") "y = ", y
      end if
      write(*,*)
    end do
  end do

contains

function a(value)
  logical :: a
  logical, intent(in) :: value

  a = value
  write(*, "(a,l1,a)") "Called function a(", value, ")"
end function

function b(value)
  logical :: b
  logical, intent(in) :: value

  b = value
  write(*, "(a,l1,a)") "Called function b(", value, ")"
end function
end program
```

```txt
Calculating x = a(F) and b(F)
Called function a(F)
x = F

Calculating y = a(F) or b(F)
Called function a(F)
Called function b(F)
y = F

Calculating x = a(F) and b(T)
Called function a(F)
x = F

Calculating y = a(F) or b(T)
Called function a(F)
Called function b(T)
y = T

Calculating x = a(T) and b(F)
Called function a(T)
Called function b(F)
x = F

Calculating y = a(T) or b(F)
Called function a(T)
y = T

Calculating x = a(T) and b(T)
Called function a(T)
Called function b(T)
x = T

Calculating y = a(T) or b(T)
Called function a(T)
y = T
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function a(p As Boolean) As Boolean
  Print "a() called"
  Return p
End Function

Function b(p As Boolean) As Boolean
  Print "b() called"
  Return p
End Function

Dim As Boolean i, j, x, y
i = False
j = True
Print "Without short-circuit evaluation :"
Print
x = a(i) And b(j)
y = a(i) Or b(j)
Print "x = "; x; " y = "; y
Print
Print "With short-circuit evaluation :"
Print
x = a(i) AndAlso b(j) '' b(j) not called as a(i) = false and so x must be false
y = a(i) OrElse b(j)  '' b(j) still called as can't determine y unless it is
Print "x = "; x; " y = "; y
Print
Print "Press any key to quit"
Sleep
```


```txt

Without short-circuit evaluation :

a() called
b() called
a() called
b() called
x = false y = true

With short-circuit evaluation :

a() called
a() called
b() called
x = false y = true

```



## Go

Short circuit operators are <nowiki>&&</nowiki> and ||.

```go
package main

import "fmt"

func a(v bool) bool {
    fmt.Print("a")
    return v
}

func b(v bool) bool {
    fmt.Print("b")
    return v
}

func test(i, j bool) {
    fmt.Printf("Testing a(%t) && b(%t)\n", i, j)
    fmt.Print("Trace:  ")
    fmt.Println("\nResult:", a(i) && b(j))

    fmt.Printf("Testing a(%t) || b(%t)\n", i, j)
    fmt.Print("Trace:  ")
    fmt.Println("\nResult:", a(i) || b(j))

    fmt.Println("")
}

func main() {
    test(false, false)
    test(false, true)
    test(true, false)
    test(true, true)
}
```

```txt
Testing a(false) && b(false)
Trace:  a
Result: false
Testing a(false) || b(false)
Trace:  ab
Result: false

Testing a(false) && b(true)
Trace:  a
Result: false
Testing a(false) || b(true)
Trace:  ab
Result: true

Testing a(true) && b(false)
Trace:  ab
Result: false
Testing a(true) || b(false)
Trace:  a
Result: true

Testing a(true) && b(true)
Trace:  ab
Result: true
Testing a(true) || b(true)
Trace:  a
Result: true
```



## Groovy

Like all C-based languages (of which I am aware), Groovy short-circuits the logical and (<nowiki>&&</nowiki>) and logical or (||) operations, but not the bitwise and (<nowiki>&</nowiki>) and bitwise or (|) operations.

```groovy
def f = { println '  AHA!'; it instanceof String }
def g = { printf ('%5d ', it); it > 50 }

println 'bitwise'
assert g(100) & f('sss')
assert g(2) | f('sss')
assert ! (g(1) & f('sss'))
assert g(200) | f('sss')

println '''
logical'''
assert g(100) && f('sss')
assert g(2) || f('sss')
assert ! (g(1) && f('sss'))
assert g(200) || f('sss')
```

```txt
bitwise
  100   AHA!
    2   AHA!
    1   AHA!
  200   AHA!

logical
  100   AHA!
    2   AHA!
    1   200
```



## Haskell

[[Lazy evaluation]] makes it possible for user-defined functions to be short-circuited. An expression will not be evaluated as long as it is not [[pattern matching|pattern matched]]:

```haskell
module ShortCircuit where

import Prelude hiding ((&&), (||))
import Debug.Trace

False && _     = False
True  && False = False
_     && _     = True

True  || _     = True
False || True  = True
_     || _     = False

a p = trace ("<a " ++ show p ++ ">") p
b p = trace ("<b " ++ show p ++ ">") p

main = mapM_ print (    [ a p || b q | p <- [False, True], q <- [False, True] ]
                     ++ [ a p && b q | p <- [False, True], q <- [False, True] ])
```

```txt

<a False>
<b False>
False
<a False>
<b True>
True
<a True>
True
<a True>
True
<a False>
False
<a False>
False
<a True>
<b False>
False
<a True>
<b True>
True

```

One can force the right-hand arguemnt to be evaluated first be using the alternate definitions:

```haskell
_     && False = False
False && True  = False
_     && _     = True

_     || True  = True
True  || False = True
_     || _     = False
```

```txt

<b False>
<a False>
False
<b True>
True
<b False>
<a True>
True
<b True>
True
<b False>
False
<b True>
<a False>
False
<b False>
False
<b True>
<a True>
True

```

The order of evaluation (in this case the original order again) can be seen in a more explicit form by [[syntactic sugar|desugaring]] the pattern matching:

```haskell
p && q = case p of
           False -> False
           _     -> case q of
                      False -> False
                      _     -> True

p || q = case p of
           True -> True
           _    -> case q of
                      True -> True
                      _    -> False
```


=={{header|Icon}} and {{header|Unicon}}==
The entire concept of using 'boolean' values for logic control runs counter to the philosophy of Icon. Instead Icon has success (something that returns a result) and failure which is really a signal.  The concept is similar to that used in [[:Category:SNOBOL4|SNOBOL4]] and [[:Category:Lisp|Lisp]] and far more potent than passing around and testing booleans.  There is no way to pass around a 'false' value in that sense. Icon does have facilities for dealing with bits inside integers but these would not normally be used for control purposes.  Because failure is a signal control is always evaluated in a short-circuit manner. One consequence of this is that an expression "i < j" doesn't return a boolean value, instead it returns the value of j.  While this may seem odd at first it allows for elegant expressions like "i < j < k". Another benefit is that there is no need for programmers to devote effort to staying inside the bounds of any data type. For instance, if you loop and iterate beyond bounds the expression simply fails and the loop ends.

While this task could be written literally, it would be more beneficial to show how an Icon programmer would approach the same problem.  Icon extends the idea short circuit evaluation with the ability for expressions to generate alternate results only if needed.  For more information see [[Icon%2BUnicon/Intro#Program_Flow_and_Control|Failure is an option, Everything Returns a Value Except when it Doesn't, and Goal-Directed Evaluation and Generators]].  Consequently some small liberties will be taken with this task:
* Since any result means an expression succeeded and is hence true, we can use any value. In this example our choice will be determined by how we deal with 'false'.
* The inability to pass a 'false' value is a challenge.  At first glance we might try &null, similar to Lisp, but there is no canonical true. Also &null produces a result, so strictly speaking it could be 'true' as well. A good example of this is that an expression like " not expr " returns null if 'expr' fails.
* For this example we will define two procedures 'true' and 'false'.  Because Icon treats procedures as a data type we can assign them and invoke them indirectly via the variable name they are assigned to.  We can write " i := true " and later invoke 'true' via " i() ".
* Rather than have the tasks print their own name, we will just utilize built-in tracing which will be more informative.
This use of procedures as values is somewhat contrived but serves us well for demonstration purposes. In practice this approach would be strained since failure results aren't re-captured as values (and can't easily be).

```Icon
procedure main()
&trace := -1 # ensures functions print their names

every (i := false | true ) & ( j := false | true) do {
  write("i,j := ",image(i),", ",image(j))
  write("i & j:")
  x := i() & j()   # invoke true/false
  write("i | j:")
  y := i() | j()   # invoke true/false
  }
end

procedure true()   #: succeeds always (returning null)
return
end

procedure false()  #: fails always
fail    # for clarity but not needed as running into end has the same effect
end
```

Sample output for a single case:
```txt
i,j := procedure true, procedure false
i & j:
Shortcircuit.icn:    8  | true()
Shortcircuit.icn:   16  | true returned &null
Shortcircuit.icn:    8  | false()
Shortcircuit.icn:   20  | false failed
i | j:
Shortcircuit.icn:   10  | true()
Shortcircuit.icn:   16  | true returned &null
i,j := procedure true, procedure true
```



## Io

```Io
a := method(bool,
    writeln("a(#{bool}) called." interpolate)
    bool
)
b := method(bool,
    writeln("b(#{bool}) called." interpolate)
    bool
)

list(true,false) foreach(avalue,
    list(true,false) foreach(bvalue,
        x := a(avalue) and b(bvalue)
        writeln("x = a(#{avalue}) and b(#{bvalue}) is #{x}" interpolate)
        writeln
        y := a(avalue) or b(bvalue)
        writeln("y = a(#{avalue}) or b(#{bvalue}) is #{y}" interpolate)
        writeln
    )
)
```

```txt
a(true) called.
b(true) called.
x = a(true) and b(true) is true

a(true) called.
y = a(true) or b(true) is true

a(true) called.
b(false) called.
x = a(true) and b(false) is false

a(true) called.
y = a(true) or b(false) is true

a(false) called.
x = a(false) and b(true) is false

a(false) called.
b(true) called.
y = a(false) or b(true) is true

a(false) called.
x = a(false) and b(false) is false

a(false) called.
b(false) called.
y = a(false) or b(false) is false
```



## J

See the J wiki entry on [[j:Essays/Short Circuit Boolean|short circuit booleans]].

```j
labeled=:1 :'[ smoutput@,&":~&m'
A=: 'A ' labeled
B=: 'B ' labeled
and=: ^:
or=: 2 :'u^:(-.@v)'
```

```j
   (A and B) 1
B 1
A 1
1
   (A and B) 0
B 0
0
   (A or B) 1
B 1
1
   (A or B) 0
B 0
A 0
0
```

Note that J evaluates right-to-left.

Note also that both functions take the same argument (which might make this less than ideal for some purposes, but trying micromanage flow of control is usually counter-productive in J in much the way that global values can be counter-productive in an object oriented environment.  When you are processing a large set of array data, flow of control can only make sense when it is relevant to all of the data being processed -- if you want to manage flow of control which is not relevant to the entire set of data being processed you might artificially reduce the amount of data being processed, along the lines of an SQL cursor).


## Java

In Java the boolean operators <code>&&</code> and <code>||</code> are short circuit operators. The eager operator counterparts are <code>&</code> and <code>|</code>.

```java
public class ShortCirc {
    public static void main(String[] args){
        System.out.println("F and F = " + (a(false) && b(false)) + "\n");
        System.out.println("F or F = " + (a(false) || b(false)) + "\n");

        System.out.println("F and T = " + (a(false) && b(true)) + "\n");
        System.out.println("F or T = " + (a(false) || b(true)) + "\n");

        System.out.println("T and F = " + (a(true) && b(false)) + "\n");
        System.out.println("T or F = " + (a(true) || b(false)) + "\n");

        System.out.println("T and T = " + (a(true) && b(true)) + "\n");
        System.out.println("T or T = " + (a(true) || b(true)) + "\n");
    }

    public static boolean a(boolean a){
        System.out.println("a");
        return a;
    }

    public static boolean b(boolean b){
        System.out.println("b");
        return b;
    }
}
```

```txt
a
F and F = false

a
b
F or F = false

a
F and T = false

a
b
F or T = true

a
b
T and F = false

a
T or F = true

a
b
T and T = true

a
T or T = true
```



## JavaScript


Short-circuiting evaluation of boolean expressions has been the default since the first versions of JavaScript.


```JavaScript
(function () {
    'use strict';

    function a(bool) {
        console.log('a -->', bool);

        return bool;
    }

    function b(bool) {
        console.log('b -->', bool);

        return bool;
    }


    var x = a(false) && b(true),
        y = a(true) || b(false),
        z = true ? a(true) : b(false);

  return [x, y, z];
})();
```


The console log shows that in each case (the binding of all three values), only the left-hand part of the expression (the application of ''a(expr)'') was evaluated – ''b(expr)'' was skipped by logical short-circuiting.

Console:

```txt
/* a --> false */
/* a --> true */
/* a --> true */
```


Return value:

```txt
[false, true, true]
```



## jq

jq's 'and' and 'or' are short-circuit operators.  The following demonstration, which follows the "awk" example above, requires a version of jq with the built-in filter 'stderr'.

```jq
def a(x): "  a(\(x))" | stderr | x;

def b(y): "  b(\(y))" | stderr | y;

"and:", (a(true) and b(true)),
"or:",  (a(true) or b(true)),
"and:", (a(false) and b(true)),
"or:",  (a(false) or b(true))
```

```sh
$ jq -r -n -f Short-circuit-evaluation.jq
and:
"  a(true)"
"  b(true)"
true
or:
"  a(true)"
true
and:
"  a(false)"
false
or:
"  a(false)"
"  b(true)"
true
```



## Julia

Julia does have short-circuit evaluation, which works just as you expect it to:


```julia
a(x) = (println("\t# Called a($x)"); return x)
b(x) = (println("\t# Called b($x)"); return x)

for i in [true,false], j in [true, false]
    println("\nCalculating: x = a($i) && b($j)"); x = a(i) && b(j)
    println("\tResult: x = $x")
    println("\nCalculating: y = a($i) || b($j)"); y = a(i) || b(j)
    println("\tResult: y = $y")
end
```

```txt
Calculating: x = a(true) && b(true)
	# Called a(true)
	# Called b(true)
	Result: x = true

Calculating: y = a(true) || b(true)
	# Called a(true)
	Result: y = true

Calculating: x = a(true) && b(false)
	# Called a(true)
	# Called b(false)
	Result: x = false

Calculating: y = a(true) || b(false)
	# Called a(true)
	Result: y = true

Calculating: x = a(false) && b(true)
	# Called a(false)
	Result: x = false

Calculating: y = a(false) || b(true)
	# Called a(false)
	# Called b(true)
	Result: y = true

Calculating: x = a(false) && b(false)
	# Called a(false)
	Result: x = false

Calculating: y = a(false) || b(false)
	# Called a(false)
	# Called b(false)
	Result: y = false
```



## Kotlin


```scala
// version 1.1.2

fun a(v: Boolean): Boolean {
    println("'a' called")
    return v
}

fun b(v: Boolean): Boolean {
    println("'b' called")
    return v
}

fun main(args: Array<String>){
    val pairs = arrayOf(Pair(true, true), Pair(true, false), Pair(false, true), Pair(false, false))
    for (pair in pairs) {
        val x = a(pair.first) && b(pair.second)
        println("${pair.first} && ${pair.second} = $x")
        val y = a(pair.first) || b(pair.second)
        println("${pair.first} || ${pair.second} = $y")
        println()
    }
}
```


```txt

'a' called
'b' called
true && true = true
'a' called
true || true = true

'a' called
'b' called
true && false = false
'a' called
true || false = true

'a' called
false && true = false
'a' called
'b' called
false || true = true

'a' called
false && false = false
'a' called
'b' called
false || false = false

```



## Liberty BASIC

LB does not have short-circuit evaluation. Implemented with IFs.

```lb
print "AND"
for i = 0 to 1
    for j = 0 to 1
        print "a("; i; ") AND b( "; j; ")"
        res =a( i)    'call always
        if res <>0 then  'short circuit if 0
            res = b( j)
        end if
        print "=>",res
    next
next

print "---------------------------------"
print "OR"
for i = 0 to 1
    for j = 0 to 1
        print "a("; i; ") OR b("; j; ")"
        res =a( i)    'call always
        if res = 0 then  'short circuit if <>0
            res = b( j)
        end if
        print "=>", res
    next
next

'----------------------------------------
function a( t)
    print ,"calls func a"
    a = t
end function

function b( t)
    print ,"calls func b"
    b = t
end function
```

```txt
AND
a(0) AND b( 0)
              calls func a
=>            0
a(0) AND b( 1)
              calls func a
=>            0
a(1) AND b( 0)
              calls func a
              calls func b
=>            0
a(1) AND b( 1)
              calls func a
              calls func b
=>            1
---------------------------------
OR
a(0) OR b(0)
              calls func a
              calls func b
=>            0
a(0) OR b(1)
              calls func a
              calls func b
=>            1
a(1) OR b(0)
              calls func a
=>            1
a(1) OR b(1)
              calls func a
=>            1
```



## LiveCode

Livecode uses short-circuit evaluation.

```LiveCode
global outcome
function a bool
    put "a called with" && bool & cr after outcome
    return bool
end a
function b bool
    put "b called with" && bool & cr after outcome
    return bool
end b

on mouseUp
    local tExp
    put empty into outcome
    repeat for each item op in "and,or"
        repeat for each item x in "true,false"
            put merge("a([[x]]) [[op]] b([[x]])") into tExp
            put merge(tExp && "is [[" & tExp & "]]") & cr after outcome
            put merge("a([[x]]) [[op]] b([[not x]])") into tExp
            put merge(tExp && "is [[" & tExp & "]]") & cr after outcome
        end repeat
        put cr after outcome
    end repeat
    put outcome
end mouseUp
```



## Logo

The <code>AND</code> and <code>OR</code> predicates may take either expressions which are all evaluated beforehand, or lists which are short-circuit evaluated from left to right only until the overall value of the expression can be determined.

```logo
and [notequal? :x 0] [1/:x > 3]
(or [:x < 0] [:y < 0] [sqrt :x + sqrt :y <  3])
```



## Lua


```lua
function a(i)
    print "Function a(i) called."
    return i
end

function b(i)
    print "Function b(i) called."
    return i
end

i = true
x = a(i) and b(i);  print ""
y = a(i) or  b(i);  print ""

i = false
x = a(i) and b(i);  print ""
y = a(i) or  b(i)
```



## Maple

Built-in short circuit evaluation

```Maple
a := proc(bool)
	printf("a is called->%s\n", bool):
	return bool:
end proc:
b := proc(bool)
	printf("b is called->%s\n", bool):
	return bool:
end proc:
for i in [true, false] do
	for j in [true, false] do
		printf("calculating	x := a(i) and b(j)\n"):
		x := a(i) and b(j):
		printf("calculating	x := a(i) or b(j)\n"):
		y := a(i) or  b(j):
	od:
od:
```

```txt
calculating	x := a(i) and b(j)
a is called->true
b is called->true
calculating	x := a(i) or b(j)
a is called->true
calculating	x := a(i) and b(j)
a is called->true
b is called->false
calculating	x := a(i) or b(j)
a is called->true
calculating	x := a(i) and b(j)
a is called->false
calculating	x := a(i) or b(j)
a is called->false
b is called->true
calculating	x := a(i) and b(j)
a is called->false
calculating	x := a(i) or b(j)
a is called->false
b is called->false
```



## Mathematica

Mathematica has built-in short-circuit evaluation of logical expressions.

```Mathematica
a[in_] := (Print["a"]; in)
b[in_] := (Print["b"]; in)

a[False] && b[True]
a[True] || b[False]
```

Evaluation of the preceding code gives:

```txt

a
False

a
True

```

Whereas evaluating this:

```Mathematica
a[True] && b[False]
```

Gives:

```txt

a
b
False

```


=={{header|MATLAB}} / {{header|Octave}}==
Short-circuit evalation is done in logical AND (&&) and logical OR (||) operators:

```matlab
  function x=a(x)
    printf('a: %i\n',x);
  end;
  function x=b(x)
    printf('b: %i\n',x);
  end;

  a(1) && b(1)
  a(0) && b(1)
  a(1) || b(1)
  a(0) || b(1)
```

```matlab>
 a(1) && b(1);
  a: 1
  b: 1
  > a(0) && b(1);
  a: 0
  > a(1) || b(1);
  a: 1
  > a(0) || b(1);
  a: 0
  b: 1
```


=={{header|Modula-2}}==

```modula2
MODULE ShortCircuit;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE a(v : BOOLEAN) : BOOLEAN;
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    FormatString("    # Called function a(%b)\n", buf, v);
    WriteString(buf);
    RETURN v
END a;

PROCEDURE b(v : BOOLEAN) : BOOLEAN;
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    FormatString("    # Called function b(%b)\n", buf, v);
    WriteString(buf);
    RETURN v
END b;

PROCEDURE Print(x,y : BOOLEAN);
VAR buf : ARRAY[0..63] OF CHAR;
VAR temp : BOOLEAN;
BEGIN
    FormatString("a(%b) AND b(%b)\n", buf, x, y);
    WriteString(buf);
    temp := a(x) AND b(y);

    FormatString("a(%b) OR b(%b)\n", buf, x, y);
    WriteString(buf);
    temp := a(x) OR b(y);

    WriteLn;
END Print;

BEGIN
    Print(FALSE,FALSE);
    Print(FALSE,TRUE);
    Print(TRUE,TRUE);
    Print(TRUE,FALSE);
    ReadChar
END ShortCircuit.
```



## MUMPS

MUMPS evaluates every expression it encounters, so we have to use conditional statements to do a short circuiting of the expensive second task.

```MUMPS
SSEVAL1(IN)
 WRITE !,?10,$STACK($STACK,"PLACE")
 QUIT IN
SSEVAL2(IN)
 WRITE !,?10,$STACK($STACK,"PLACE")
 QUIT IN
SSEVAL3
 NEW Z
 WRITE "1 AND 1"
 SET Z=$$SSEVAL1(1) SET:Z Z=Z&$$SSEVAL2(1)
 WRITE !,$SELECT(Z:"TRUE",1:"FALSE")
 WRITE !!,"0 AND 1"
 SET Z=$$SSEVAL1(0) SET:Z Z=Z&$$SSEVAL2(1)
 WRITE !,$SELECT(Z:"TRUE",1:"FALSE")
 WRITE !!,"1 OR 1"
 SET Z=$$SSEVAL1(1) SET:'Z Z=Z!$$SSEVAL2(1)
 WRITE !,$SELECT(Z:"TRUE",1:"FALSE")
 WRITE !!,"0 OR 1"
 SET Z=$$SSEVAL1(0) SET:'Z Z=Z!$$SSEVAL2(1)
 WRITE !,$SELECT(Z:"TRUE",1:"FALSE")
 KILL Z
 QUIT
```

```txt
USER>D SSEVAL3^ROSETTA
1 AND 1
          SSEVAL1+1^ROSETTA +3
          SSEVAL2+1^ROSETTA +3
TRUE

0 AND 1
          SSEVAL1+1^ROSETTA +3
FALSE

1 OR 1
          SSEVAL1+1^ROSETTA +3
TRUE

0 OR 1
          SSEVAL1+1^ROSETTA +3
          SSEVAL2+1^ROSETTA +3
TRUE
```



## Nemerle


```Nemerle
using System.Console;

class ShortCircuit
{
    public static a(x : bool) : bool
    {
        WriteLine("a");
        x
    }

    public static b(x : bool) : bool
    {
        WriteLine("b");
        x
    }

    public static Main() : void
    {
        def t = true;
        def f = false;

        WriteLine("True  && True : {0}", a(t) && b(t));
        WriteLine("True  && False: {0}", a(t) && b(f));
        WriteLine("False && True : {0}", a(f) && b(t));
        WriteLine("False && False: {0}", a(f) && b(f));
        WriteLine("True  || True : {0}", a(t) || b(t));
        WriteLine("True  || False: {0}", a(t) || b(f));
        WriteLine("False || True : {0}", a(f) || b(t));
        WriteLine("False || False: {0}", a(f) || b(f));
    }
}
```

<lang>a
b
True  && True : True
a
b
True  && False: False
a
False && True : False
a
False && False: False
a
True  || True : True
a
True  || False: True
a
b
False || True : True
a
b
False || False: False
```



## NetRexx

Like [[OoRexx]], [[NetRexx]] allows a list of expressions in the condition part of <tt>If</tt> and <tt>When</tt>. Evaluation ends with the first of these expressions resulting in <tt>boolean true</tt>.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

Parse Version v
Say 'Version='v

If a()  | b() Then Say 'a and b are true'
If \a() | b() Then Say 'Surprise'
Else               Say 'ok'

If a(),  b() Then Say 'a is true'
If \a(), b() Then Say 'Surprise'
Else              Say 'ok: \\a() is false'

Select
  When \a(), b() Then Say 'Surprise'
  Otherwise           Say 'ok: \\a() is false (Select)'
  End
Return

method a private static binary returns boolean
  state = Boolean.TRUE.booleanValue()
  Say '--a returns' state
  Return state

method b private static binary returns boolean
  state = Boolean.TRUE.booleanValue()
  Say '--b returns' state
  Return state

```

```txt

Version=NetRexx 3.03 11 Jun 2014
--a returns 1
--b returns 1
a and b are true
--a returns 1
--b returns 1
Surprise
--a returns 1
a is true
--a returns 1
--b returns 1
Surprise
--a returns 1
--b returns 1
Surprise


```



## Nim


```nim
proc a(x): bool =
  echo "a called"
  result = x
proc b(x): bool =
  echo "b called"
  result = x

let x = a(false) and b(true) # echoes "a called"

let y = a(true) or b(true) # echoes "a called"
```



## Objeck

In Objeck the Boolean operators <code>&</code> and <code>|</code> short circuit.

```objeck
class ShortCircuit {
  function : a(a : Bool) ~ Bool {
    "a"->PrintLine();
    return a;
  }

  function : b(b : Bool) ~ Bool {
    "b"->PrintLine();
    return b;
  }

  function : Main(args : String[]) ~ Nil {
    result := a(false) & b(false);
    "F and F = {$result}"->PrintLine();
    result := a(false) | b(false);
    "F or F = {$result}"->PrintLine();

    result := a(false) & b(true);
    "F and T = {$result}"->PrintLine();
    result := a(false) | b(true);
    "F or T = {$result}"->PrintLine();

    result := a(true) & b(false);
    "T and F = {$result}"->PrintLine();
    result := a(true) | b(false);
    "T or F = {$result}"->PrintLine();

    result := a(true) & b(true);
    "T and T = {$result}"->PrintLine();
    result := a(true) | b(true);
    "T or T = {$result}"->PrintLine();
  }
}
```



## OCaml


```ocaml
let a r = print_endline " > function a called"; r
let b r = print_endline " > function b called"; r

let test_and b1 b2 =
  Printf.printf "# testing (%b && %b)\n" b1 b2;
  ignore (a b1 && b b2)

let test_or b1 b2 =
  Printf.printf "# testing (%b || %b)\n" b1 b2;
  ignore (a b1 || b b2)

let test_this test =
  test true true;
  test true false;
  test false true;
  test false false;
;;

let () =
  print_endline "
### = Testing and =
";
  test_this test_and;
  print_endline "
### = Testing or =
";
  test_this test_or;
;;
```

### = Testing and =

 # testing (true && true)
  > function a called
  > function b called
 # testing (true && false)
  > function a called
  > function b called
 # testing (false && true)
  > function a called
 # testing (false && false)
  > function a called

### = Testing or =

 # testing (true || true)
  > function a called
 # testing (true || false)
  > function a called
 # testing (false || true)
  > function a called
  > function b called
 # testing (false || false)
  > function a called
  > function b called


## ooRexx

ooRexx allows a list of expressions in the condition part of If and When.
Evaluation ends with the first of these expressions resulting in .false (or 0).

```oorexx
Parse Version v
Say 'Version='v
If a() | b() Then Say 'a and b are true'
If \a() | b() Then Say  'Surprise'
              Else Say 'ok'
If a(), b() Then Say 'a is true'
If \a(), b() Then Say  'Surprise'
             Else Say 'ok: \a() is false'
Select
  When \a(), b() Then Say 'Surprise'
  Otherwise           Say 'ok: \a() is false (Select)'
  End
Exit
a: Say 'a returns .true'; Return .true
b: Say 'b returns 1'; Return 1

```

```txt
Version=REXX-ooRexx_4.2.0(MT)_32-bit 6.04 22 Feb 2014
a returns .true
b returns 1
a and b are true
a returns .true
b returns 1
Surprise
a returns .true
b returns 1
a is true
a returns .true
ok: \a() is false
a returns .true
ok: \a() is false (Select)

```



## Oz

Oz' <code>andthen</code> and <code>orelse</code> operators are short-circuiting, as indicated by their name. The library functions <code>Bool.and</code> and <code>Bool.or</code> are not short-circuiting, on the other hand.

```oz
declare
  fun {A Answer}
     AnswerS = {Value.toVirtualString Answer 1 1}
  in
     {System.showInfo "  % Called function {A "#AnswerS#"} -> "#AnswerS}
     Answer
  end

  fun {B Answer}
     AnswerS = {Value.toVirtualString Answer 1 1}
  in
     {System.showInfo "  % Called function {B "#AnswerS#"} -> "#AnswerS}
     Answer
  end
in
  for I in [false true] do
     for J in [false true] do
        X Y
     in
        {System.showInfo "\nCalculating: X = {A I} andthen {B J}"}
        X = {A I} andthen {B J}
        {System.showInfo "Calculating: Y = {A I} orelse {B J}"}
        Y = {A I} orelse {B J}
     end
  end
```

```oz
Calculating: X = {A I} andthen {B J}
  % Called function {A false} -> false
Calculating: Y = {A I} orelse {B J}
  % Called function {A false} -> false
  % Called function {B false} -> false

Calculating: X = {A I} andthen {B J}
  % Called function {A false} -> false
Calculating: Y = {A I} orelse {B J}
  % Called function {A false} -> false
  % Called function {B true} -> true

Calculating: X = {A I} andthen {B J}
  % Called function {A true} -> true
  % Called function {B false} -> false
Calculating: Y = {A I} orelse {B J}
  % Called function {A true} -> true

Calculating: X = {A I} andthen {B J}
  % Called function {A true} -> true
  % Called function {B true} -> true
Calculating: Y = {A I} orelse {B J}
  % Called function {A true} -> true
```



## PARI/GP

Note that <code>|</code> and <code>&</code> are deprecated versions of the GP short-circuit operators.

```parigp
a(n)={
  print(a"("n")");
  a
};
b(n)={
  print("b("n")");
  n
};
or(A,B)={
  a(A) || b(B)
};
and(A,B)={
  a(A) && b(B)
};
```



## Pascal


### Standard Pascal

Standard Pascal doesn't have native short-circuit evaluation.

```pascal
program shortcircuit(output);

function a(value: boolean): boolean;
 begin
  writeln('a(', value, ')');
  a := value
 end;

function b(value:boolean): boolean;
 begin
  writeln('b(', value, ')');
  b := value
 end;

procedure scandor(value1, value2: boolean);
 var
  result: boolean;
 begin
  {and}
  if a(value1)
   then
    result := b(value2)
   else
    result := false;
  writeln(value1, ' and ', value2, ' = ', result);

  {or}
  if a(value1)
   then
    result := true
   else
    result := b(value2)
  writeln(value1, ' or ', value2, ' = ', result);
 end;

begin
 scandor(false, false);
 scandor(false, true);
 scandor(true, false);
 scandor(true, true);
end.
```



### Turbo Pascal

Turbo Pascal allows short circuit evaluation with a compiler switch:

```pascal
program shortcircuit;

function a(value: boolean): boolean;
 begin
  writeln('a(', value, ')');
  a := value;
 end;

function b(value:boolean): boolean;
 begin
  writeln('b(', value, ')');
  b := value;
 end;

{$B-} {enable short circuit evaluation}
procedure scandor(value1, value2: boolean);
 var
  result: boolean;
 begin
  result :=  a(value1) and b(value);
  writeln(value1, ' and ', value2, ' = ', result);

  result := a(value1) or b(value2);
  writeln(value1, ' or ', value2, ' = ', result);
 end;

begin
 scandor(false, false);
 scandor(false, true);
 scandor(true, false);
 scandor(true, true);
end.
```


### Extended Pascal

The extended Pascal standard introduces the operators <code>and_then</code> and <code>or_else</code> for short-circuit evaluation.

```pascal
program shortcircuit(output);

function a(value: boolean): boolean;
 begin
  writeln('a(', value, ')');
  a := value
 end;

function b(value:boolean): boolean;
 begin
  writeln('b(', value, ')');
  b := value
 end;

procedure scandor(value1, value2: boolean);
 var
  result: integer;
 begin
  result :=  a(value1) and_then b(value)
  writeln(value1, ' and ', value2, ' = ', result);

  result := a(value1) or_else b(value2);
  writeln(value1, ' or ', value2, ' = ', result)
 end;

begin
 scandor(false, false);
 scandor(false, true);
 scandor(true, false);
 scandor(true, true);
end.
```

Note: GNU Pascal allows <code>and then</code> and <code>or else</code> as alternatives to <code>and_then</code> and <code>or_else</code>.


## Perl

Perl uses short-circuit boolean evaluation.

```Perl
sub a { print 'A'; return $_[0] }
sub b { print 'B'; return $_[0] }

# Test-driver
sub test {
    for my $op ('&&','||') {
        for (qw(1,1 1,0 0,1 0,0)) {
           my ($x,$y) = /(.),(.)/;
           print my $str = "a($x) $op b($y)", ': ';
           eval $str; print "\n"; } }
}

# Test and display
test();
```

```txt
a(1) && b(1): AB
a(1) && b(0): AB
a(0) && b(1): A
a(0) && b(0): A
a(1) || b(1): A
a(1) || b(0): A
a(0) || b(1): AB
a(0) || b(0): AB
```



## Perl 6

```perl6
use MONKEY-SEE-NO-EVAL;

sub a ($p) { print 'a'; $p }
sub b ($p) { print 'b'; $p }

for 1, 0 X 1, 0 -> ($p, $q) {
    for '&&', '||' -> $op {
        my $s = "a($p) $op b($q)";
        print "$s: ";
        EVAL $s;
        print "\n";
    }
}
```

```txt
a(1) && b(1): ab
a(1) || b(1): a
a(1) && b(0): ab
a(1) || b(0): a
a(0) && b(1): a
a(0) || b(1): ab
a(0) && b(0): a
a(0) || b(0): ab
```



## Phix

In Phix all expressions are short circuited
```Phix
function a(integer i)
    printf(1,"a ")
    return i
end function

function b(integer i)
    printf(1,"b ")
    return i
end function

for z=0 to 1 do
    for i=0 to 1 do
        for j=0 to 1 do
            if z then
                printf(1,"a(%d) and b(%d) ",{i,j})
                printf(1," => %d\n",a(i) and b(j))
            else
                printf(1,"a(%d) or b(%d) ",{i,j})
                printf(1," => %d\n",a(i) or b(j))
            end if
        end for
    end for
end for
```

```txt

a(0) or b(0) a b  => 0
a(0) or b(1) a b  => 1
a(1) or b(0) a  => 1
a(1) or b(1) a  => 1
a(0) and b(0) a  => 0
a(0) and b(1) a  => 0
a(1) and b(0) a b  => 0
a(1) and b(1) a b  => 1

```



## PicoLisp


```PicoLisp
(de a (F)
   (msg 'a)
   F )

(de b (F)
   (msg 'b)
   F )

(mapc
   '((I J)
      (for Op '(and or)
         (println I Op J '-> (Op (a I) (b J))) ) )
   '(NIL NIL T T)
   '(NIL T NIL T) )
```

```txt
a
NIL and NIL -> NIL
a
b
NIL or NIL -> NIL
a
NIL and T -> NIL
a
b
NIL or T -> T
a
b
T and NIL -> NIL
a
T or NIL -> T
a
b
T and T -> T
a
T or T -> T
```



## Pike


```Pike
int(0..1) a(int(0..1) i)
{
    write(" a\n");
    return i;
}

int(0..1) b(int(0..1) i)
{
    write(" b\n");
    return i;
}

foreach(({ ({ false, false }),  ({ false, true }), ({ true, true }), ({ true, false }) });; array(int) args)
{
    write(" %d && %d\n", @args);
    a(args[0]) && b(args[1]);

    write(" %d || %d\n", @args);
    a(args[0]) || b(args[1]);
}
```

```txt

 0 && 0
 a
 0 || 0
 a
 b
 0 && 1
 a
 0 || 1
 a
 b
 1 && 1
 a
 b
 1 || 1
 a
 1 && 0
 a
 b
 1 || 0
 a

```



## PL/I


```pli
short_circuit_evaluation:
   procedure options (main);
   declare (true initial ('1'b), false initial ('0'b) ) bit (1);
   declare (i, j, x, y) bit (1);

a: procedure (bv) returns (bit(1));
   declare bv bit(1);
   put ('Procedure ' || procedurename() || ' called.');
   return (bv);
end a;
b: procedure (bv) returns (bit(1));
   declare bv bit(1);
   put ('Procedure ' || procedurename() || ' called.');
   return (bv);
end b;

   do i = true, false;
      do j = true, false;
         put skip(2) list ('Evaluating x with <a> with ' || i || ' and <b> with ' || j);
         put skip;
         if a(i) then
            x = b(j);
         else
             x = false;
         put skip data (x);
         put skip(2) list ('Evaluating y with <a> with ' || i || ' and <b> with ' || j);
         put skip;
         if a(i) then
            y = true;
         else
            y = b(j);
         put skip data (y);
      end;
   end;
end short_circuit_evaluation;
```

```txt

Evaluating x with <a> with 1 and <b> with 1
Procedure A called.     Procedure B called.
X='1'B;

Evaluating y with <a> with 1 and <b> with 1
Procedure A called.
Y='1'B;

Evaluating x with <a> with 1 and <b> with 0
Procedure A called.     Procedure B called.
X='0'B;

Evaluating y with <a> with 1 and <b> with 0
Procedure A called.
Y='1'B;

Evaluating x with <a> with 0 and <b> with 1
Procedure A called.
X='0'B;

Evaluating y with <a> with 0 and <b> with 1
Procedure A called.     Procedure B called.
Y='1'B;

Evaluating x with <a> with 0 and <b> with 0
Procedure A called.
X='0'B;

Evaluating y with <a> with 0 and <b> with 0
Procedure A called.     Procedure B called.
Y='0'B;

```



## PowerShell

PowerShell handles this natively.

```powershell
#  Simulated fast function
function a ( [boolean]$J ) { return $J }

#  Simulated slow function
function b ( [boolean]$J ) { Sleep -Seconds 2; return $J }

#  These all short-circuit and do not evaluate the right hand function
( a $True  ) -or  ( b $False )
( a $True  ) -or  ( b $True  )
( a $False ) -and ( b $False )
( a $False ) -and ( b $True  )

#  Measure of execution time
Measure-Command {
( a $True  ) -or  ( b $False )
( a $True  ) -or  ( b $True  )
( a $False ) -and ( b $False )
( a $False ) -and ( b $True  )
} | Select TotalMilliseconds

#  These all appropriately do evaluate the right hand function
( a $False ) -or  ( b $False )
( a $False ) -or  ( b $True  )
( a $True  ) -and ( b $False )
( a $True  ) -and ( b $True  )

#  Measure of execution time
Measure-Command {
( a $False ) -or  ( b $False )
( a $False ) -or  ( b $True  )
( a $True  ) -and ( b $False )
( a $True  ) -and ( b $True  )
} | Select TotalMilliseconds
```

```txt
True
True
False
False

TotalMilliseconds
-----------------
           15.653
False
True
False
True
        8012.9405
```



## Prolog

Prolog has not functions but predicats succeed of fail.
Tested with SWI-Prolog. Should work with other dialects.

```Prolog
short_circuit :-
	(   a_or_b(true, true) -> writeln('==> true'); writeln('==> false')) , nl,
	(   a_or_b(true, false)-> writeln('==>  true'); writeln('==> false')) , nl,
	(   a_or_b(false, true)-> writeln('==> true'); writeln('==> false')) , nl,
	(   a_or_b(false, false)-> writeln('==> true'); writeln('==> false')) , nl,
	(   a_and_b(true, true)-> writeln('==> true'); writeln('==> false')) , nl,
	(   a_and_b(true, false)-> writeln('==>  true'); writeln('==> false')) , nl,
	(   a_and_b(false, true)-> writeln('==>  true'); writeln('==> false')) , nl,
	(   a_and_b(false, false)-> writeln('==>  true'); writeln('==> false')) .


a_and_b(X, Y) :-
	format('a(~w) and b(~w)~n', [X, Y]),
	(   a(X), b(Y)).

a_or_b(X, Y) :-
	format('a(~w) or b(~w)~n', [X, Y]),
	(   a(X); b(Y)).

a(X) :-
	format('a(~w)~n', [X]),
	X.

b(X) :-
	format('b(~w)~n', [X]),
	X.
```

```Prolog
?- short_circuit.
a(true) or b(true)
a(true)
==> true

a(true) or b(false)
a(true)
==> true

a(false) or b(true)
a(false)
b(true)
==> true

a(false) or b(false)
a(false)
b(false)
==> false

a(true) and b(true)
a(true)
b(true)
==> true

a(true) and b(false)
a(true)
b(false)
==> false

a(false) and b(true)
a(false)
==> false

a(false) and b(false)
a(false)
==> false

true.
```



## PureBasic

Logical '''And''' &amp; '''Or''' operators will not evaluate their right-hand expression if the outcome can be determined from the value of the left-hand expression.

```PureBasic
Procedure a(arg)
  PrintN("  # Called function a("+Str(arg)+")")
  ProcedureReturn arg
EndProcedure

Procedure b(arg)
  PrintN("  # Called function b("+Str(arg)+")")
  ProcedureReturn arg
EndProcedure

OpenConsole()
For a=#False To #True
  For b=#False To #True
    PrintN(#CRLF$+"Calculating: x = a("+Str(a)+") And b("+Str(b)+")")
    x= a(a) And b(b)
    PrintN("Calculating: x = a("+Str(a)+") Or b("+Str(b)+")")
    y= a(a) Or b(b)
  Next
Next
Input()
```

```txt
Calculating: x = a(0) And b(0)
  # Called function a(0)
Calculating: x = a(0) Or b(0)
  # Called function a(0)
  # Called function b(0)

Calculating: x = a(0) And b(1)
  # Called function a(0)
Calculating: x = a(0) Or b(1)
  # Called function a(0)
  # Called function b(1)

Calculating: x = a(1) And b(0)
  # Called function a(1)
  # Called function b(0)
Calculating: x = a(1) Or b(0)
  # Called function a(1)

Calculating: x = a(1) And b(1)
  # Called function a(1)
  # Called function b(1)
Calculating: x = a(1) Or b(1)
  # Called function a(1)
```



## Python

Pythons '''and''' and '''or''' binary, infix, boolean operators will not evaluate their right-hand expression if the outcome can be determined from the value of the left-hand expression.

```python
>>>
 def a(answer):
	print("  # Called function a(%r) -> %r" % (answer, answer))
	return answer

>>> def b(answer):
	print("  # Called function b(%r) -> %r" % (answer, answer))
	return answer

>>> for i in (False, True):
	for j in (False, True):
		print ("\nCalculating: x = a(i) and b(j)")
		x = a(i) and b(j)
		print ("Calculating: y = a(i) or  b(j)")
		y = a(i) or  b(j)



Calculating: x = a(i) and b(j)
  # Called function a(False) -> False
Calculating: y = a(i) or  b(j)
  # Called function a(False) -> False
  # Called function b(False) -> False

Calculating: x = a(i) and b(j)
  # Called function a(False) -> False
Calculating: y = a(i) or  b(j)
  # Called function a(False) -> False
  # Called function b(True) -> True

Calculating: x = a(i) and b(j)
  # Called function a(True) -> True
  # Called function b(False) -> False
Calculating: y = a(i) or  b(j)
  # Called function a(True) -> True

Calculating: x = a(i) and b(j)
  # Called function a(True) -> True
  # Called function b(True) -> True
Calculating: y = a(i) or  b(j)
  # Called function a(True) -> True
```

Pythons if ''expression'' can also be used to the same ends (but probably should not):

```python
>>>
 for i in (False, True):
	for j in (False, True):
		print ("\nCalculating: x = a(i) and b(j) using x = b(j) if a(i) else False")
		x = b(j) if a(i) else False
		print ("Calculating: y = a(i) or  b(j) using y = b(j) if not a(i) else True")
		y = b(j) if not a(i) else True



Calculating: x = a(i) and b(j) using x = b(j) if a(i) else False
  # Called function a(False) -> False
Calculating: y = a(i) or  b(j) using y = b(j) if not a(i) else True
  # Called function a(False) -> False
  # Called function b(False) -> False

Calculating: x = a(i) and b(j) using x = b(j) if a(i) else False
  # Called function a(False) -> False
Calculating: y = a(i) or  b(j) using y = b(j) if not a(i) else True
  # Called function a(False) -> False
  # Called function b(True) -> True

Calculating: x = a(i) and b(j) using x = b(j) if a(i) else False
  # Called function a(True) -> True
  # Called function b(False) -> False
Calculating: y = a(i) or  b(j) using y = b(j) if not a(i) else True
  # Called function a(True) -> True

Calculating: x = a(i) and b(j) using x = b(j) if a(i) else False
  # Called function a(True) -> True
  # Called function b(True) -> True
Calculating: y = a(i) or  b(j) using y = b(j) if not a(i) else True
  # Called function a(True) -> True
```



## R

The builtins <tt><nowiki>&&</nowiki></tt> and <tt>||</tt> will short circuit:
```r
a <- function(x) {cat("a called\n"); x}
b <- function(x) {cat("b called\n"); x}

tests <- expand.grid(op=list(quote(`||`), quote(`&&`)), x=c(1,0), y=c(1,0))

invisible(apply(tests, 1, function(row) {
  call <- substitute(op(a(x),b(y)), row)
  cat(deparse(call), "->", eval(call), "\n\n")
}))
```

```r
a called
a(1) || b(1) -> TRUE

a called
b called
a(1) && b(1) -> TRUE

a called
b called
a(0) || b(1) -> TRUE

a called
a(0) && b(1) -> FALSE

a called
a(1) || b(0) -> TRUE

a called
b called
a(1) && b(0) -> FALSE

a called
b called
a(0) || b(0) -> FALSE

a called
a(0) && b(0) -> FALSE
```

Because R waits until function arguments are needed before evaluating them, user-defined functions can also short circuit.

```r
switchop <- function(s, x, y) {
  if(s < 0) x || y
  else if (s > 0) x && y
  else xor(x, y)
}
```

```r>
 switchop(-1, a(1), b(1))
a called
[1] TRUE
> switchop(1, a(1), b(1))
a called
b called
[1] TRUE
> switchop(1, a(0), b(1))
a called
[1] FALSE
> switchop(0, a(0), b(1))
a called
b called
[1] TRUE
```



## Racket


```racket
#lang racket
(define (a x)
  (display (~a "a:" x " "))
  x)

(define (b x)
  (display (~a "b:" x " "))
  x)

(for* ([x '(#t #f)]
       [y '(#t #f)])
  (displayln `(and (a ,x) (b ,y)))
  (and (a x) (b y))
  (newline)

  (displayln `(or (a ,x) (b ,y)))
  (or (a x) (b y))
  (newline))
```

```txt

(and (a #t) (b #t))
a:#t b:#t
(or (a #t) (b #t))
a:#t
(and (a #t) (b #f))
a:#t b:#f
(or (a #t) (b #f))
a:#t
(and (a #f) (b #t))
a:#f
(or (a #f) (b #t))
a:#f b:#t
(and (a #f) (b #f))
a:#f
(or (a #f) (b #f))
a:#f b:#f

```



## REXX

The REXX language doesn't have native short circuits (it's specifically mentioned in the
language specifications that short-circuiting is '''not''' supported).

```rexx
/*REXX programs demonstrates short-circuit evaluation testing  (in an   IF   statement).*/
parse arg LO HI .                                /*obtain optional arguments from he CL.*/
if LO=='' | LO==","  then LO= -2                 /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI=  2                 /* "      "         "   "   "     "    */

         do j=LO  to HI                          /*process from the  low  to  the  high.*/
         x=a(j)  &  b(j)                         /*compute  function A  and  function B */
         y=a(j)  |  b(j)                         /*   "         "    "   or      "    " */
         if \y  then y=b(j)                      /*   "         "    B   (for negation).*/
         say  copies('═', 30)        '  x=' || x            '  y='y                '  j='j
         say
         end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
a: say '      A  entered with:'  arg(1);    return abs( arg(1) // 2)   /*1=odd, 0=even  */
b: say '      B  entered with:'  arg(1);    return arg(1) < 0          /*1=neg, 0=if not*/
```

'''output'''   when using the default inputs:

```txt

      B  entered with: -2
      A  entered with: -2
      B  entered with: -2
      A  entered with: -2
══════════════════════════════   x=0   y=1   j=-2

      B  entered with: -1
      A  entered with: -1
      B  entered with: -1
      A  entered with: -1
══════════════════════════════   x=1   y=1   j=-1

      B  entered with: 0
      A  entered with: 0
      B  entered with: 0
      A  entered with: 0
      B  entered with: 0
══════════════════════════════   x=0   y=0   j=0

      B  entered with: 1
      A  entered with: 1
      B  entered with: 1
      A  entered with: 1
══════════════════════════════   x=0   y=1   j=1

      B  entered with: 2
      A  entered with: 2
      B  entered with: 2
      A  entered with: 2
      B  entered with: 2
══════════════════════════════   x=0   y=0   j=2

```



## Ring


```ring

# Project : Short-circuit evaluation

for k = 1 to 2
      word = ["AND","OR"]
      see "
### ===
 " + word[k] + "
### ========
" + nl
      for i = 0 to 1
           for j = 0 to 1
                see "a(" + i + ") " + word[k] +" b(" + j + ")" + nl
                res =a(i)
                if word[k] = "AND" and res != 0
                   res = b(j)
                ok
                if word[k] = "OR"  and res = 0
                   res = b(j)
                ok
           next
      next
next

func a(t)
        see char(9) + "calls func a" + nl
        a = t
        return a

func b(t)
        see char(9) + "calls func b" + nl
        b = t
        return b

```

Output:

```txt


### ====== AND ===========

a(0) AND b(0)
	calls func a
a(0) AND b(1)
	calls func a
a(1) AND b(0)
	calls func a
	calls func b
a(1) AND b(1)
	calls func a
	calls func b

### ====== OR ===========

a(0) OR b(0)
	calls func a
	calls func b
a(0) OR b(1)
	calls func a
	calls func b
a(1) OR b(0)
	calls func a
a(1) OR b(1)
	calls func a

```



## Ruby

Binary operators are short-circuiting. Demonstration code:

```ruby
def a( bool )
  puts "a( #{bool} ) called"
  bool
end

def b( bool )
  puts "b( #{bool} ) called"
  bool
end

 [true, false].each do |a_val|
   [true, false].each do |b_val|
     puts "a( #{a_val} ) and b( #{b_val} ) is #{a( a_val ) and b( b_val )}."
     puts
     puts "a( #{a_val} ) or b( #{b_val} ) is #{a( a_val)  or b( b_val )}."
     puts
   end
 end
```

```txt

a( true ) called
b( true ) called
a( true ) and b( true ) is true.

a( true ) called
a( true ) or b( true ) is true.

a( true ) called
b( false ) called
a( true ) and b( false ) is false.

a( true ) called
a( true ) or b( false ) is true.

a( false ) called
a( false ) and b( true ) is false.

a( false ) called
b( true ) called
a( false ) or b( true ) is true.

a( false ) called
a( false ) and b( false ) is false.

a( false ) called
b( false ) called
a( false ) or b( false ) is false.

```



## Run BASIC


```runbasic
for k = 1 to 2
ao$ = word$("AND,OR",k,",")
print "
### ===
 ";ao$;"
### ========
"
for i = 0 to 1
    for j = 0 to 1
        print "a("; i; ") ";ao$;" b("; j; ")"
        res =a(i)    'call always
'print res;"<===="
if ao$ = "AND" and res <> 0 then res = b(j)
if ao$ = "OR"  and res =  0 then res = b(j)
    next
next
next k
end

function a( t)
    print chr$(9);"calls func a"
    a = t
end function

function b( t)
    print chr$(9);"calls func b"
    b = t
end function
```


```txt

### ====== AND ===========

a(0) AND b(0)
	calls func a
a(0) AND b(1)
	calls func a
a(1) AND b(0)
	calls func a
	calls func b
a(1) AND b(1)
	calls func a
	calls func b

### ====== OR ===========

a(0) OR b(0)
	calls func a
	calls func b
a(0) OR b(1)
	calls func a
	calls func b
a(1) OR b(0)
	calls func a
a(1) OR b(1)
	calls func a
```



## Rust



```rust
fn a(foo: bool) -> bool {
    println!("a");
    foo
}

fn b(foo: bool) -> bool {
    println!("b");
    foo
}

fn main() {
    for i in vec![true, false] {
        for j in vec![true, false] {
            println!("{} and {} == {}", i, j, a(i) && b(j));
            println!("{} or {} == {}", i, j, a(i) || b(j));
            println!();
        }
    }
}
```

```txt

a
b
true and true == true
a
true or true == true

a
b
true and false == false
a
true or false == true

a
false and true == false
a
b
false or true == true

a
false and false == false
a
b
false or false == false

```



## Sather


```sather
class MAIN is
  a(v:BOOL):BOOL is
    #OUT + "executing a\n";
    return v;
  end;
  b(v:BOOL):BOOL is
    #OUT + "executing b\n";
    return v;
  end;

  main is
    x:BOOL;

    x := a(false) and b(true);
    #OUT + "F and T = " + x + "\n\n";

    x := a(true) or b(true);
    #OUT + "T or T = " + x + "\n\n";

    x := a(true) and b(false);
    #OUT + "T and T = " + x + "\n\n";

    x := a(false) or b(true);
    #OUT + "F or T = " + x + "\n\n";
  end;
end;
```



## Scala


```scala
object ShortCircuit {
   def a(b:Boolean)={print("Called A=%5b".format(b));b}
   def b(b:Boolean)={print(" -> B=%5b".format(b));b}

   def main(args: Array[String]): Unit = {
      val boolVals=List(false,true)
      for(aa<-boolVals; bb<-boolVals){
         print("\nTesting A=%5b AND B=%5b   -> ".format(aa, bb))
         a(aa) && b(bb)
      }
      for(aa<-boolVals; bb<-boolVals){
         print("\nTesting A=%5b  OR B=%5b   -> ".format(aa, bb))
         a(aa) || b(bb)
      }
      println
   }
}
```

```txt
Testing A=false AND B=false   -> Called A=false
Testing A=false AND B= true   -> Called A=false
Testing A= true AND B=false   -> Called A= true -> B=false
Testing A= true AND B= true   -> Called A= true -> B= true
Testing A=false  OR B=false   -> Called A=false -> B=false
Testing A=false  OR B= true   -> Called A=false -> B= true
Testing A= true  OR B=false   -> Called A= true
Testing A= true  OR B= true   -> Called A= true
```



## Scheme


```scheme>
(define (a x)
   (display "a\n")
   x)
>(define (b x)
   (display "b\n")
   x)
>(for-each (lambda (i)
   (for-each (lambda (j)
     (display i) (display " and ") (display j) (newline)
     (and (a i) (b j))
     (display i) (display " or ") (display j) (newline)
     (or (a i) (b j))
    ) '(#t #f))
  ) '(#t #f))
#t and #t
a
b
#t or #t
a
#t and #f
a
b
#t or #f
a
#f and #t
a
#f or #t
a
b
#f and #f
a
#f or #f
a
b

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: a (in boolean: aBool) is func
  result
    var boolean: result is FALSE;
  begin
    writeln("a");
    result := aBool;
  end func;

const func boolean: b (in boolean: aBool) is func
  result
    var boolean: result is FALSE;
  begin
    writeln("b");
    result := aBool;
  end func;

const proc: test (in boolean: param1, in boolean: param2) is func
  begin
    writeln(param1 <& " and " <& param2 <& " = " <& a(param1) and b(param2));
    writeln(param1 <& " or " <& param2 <& " = " <& a(param1) or b(param2));
  end func;

const proc: main is func
  begin
    test(FALSE, FALSE);
    test(FALSE, TRUE);
    test(TRUE, FALSE);
    test(TRUE, TRUE);
  end func;
```

```txt

a
FALSE and FALSE = FALSE
a
b
FALSE or FALSE = FALSE
a
FALSE and TRUE = FALSE
a
b
FALSE or TRUE = TRUE
a
b
TRUE and FALSE = FALSE
a
TRUE or FALSE = TRUE
a
b
TRUE and TRUE = TRUE
a
TRUE or TRUE = TRUE

```



## Sidef


```ruby
func a(bool) { print 'A'; return bool }
func b(bool) { print 'B'; return bool }
 
# Test-driver
func test() {
    for op in ['&&', '||'] {
        for x,y in [[1,1],[1,0],[0,1],[0,0]] {
            "a(%s) %s b(%s): ".printf(x, op, y)
            eval "a(Bool(x)) #{op} b(Bool(y))"
            print "\n"
        }
    }
}
 
# Test and display
test()
```

```txt
a(1) && b(1): AB
a(1) && b(0): AB
a(0) && b(1): A
a(0) && b(0): A
a(1) || b(1): A
a(1) || b(0): A
a(0) || b(1): AB
a(0) || b(0): AB
```


## Simula


```simula
BEGIN

    BOOLEAN PROCEDURE A(BOOL); BOOLEAN BOOL;
    BEGIN OUTCHAR('A'); A := BOOL;
    END A;

    BOOLEAN PROCEDURE B(BOOL); BOOLEAN BOOL;
    BEGIN OUTCHAR('B'); B := BOOL;
    END B;

    PROCEDURE OUTBOOL(BOOL); BOOLEAN BOOL;
        OUTCHAR(IF BOOL THEN 'T' ELSE 'F');

    PROCEDURE TEST;
    BEGIN
        PROCEDURE ANDTEST;
        BEGIN
            BOOLEAN X, Y, Z;
            FOR X := TRUE, FALSE DO
                FOR Y := TRUE, FALSE DO
                BEGIN
                    OUTTEXT("A("); OUTBOOL(X);
                    OUTTEXT(") AND ");
                    OUTTEXT("B("); OUTBOOL(Y);
                    OUTTEXT("): ");
                    Z := A(X) AND THEN B(Y);
                    OUTIMAGE;
                END;
        END ANDTEST;

        PROCEDURE ORTEST;
        BEGIN
            BOOLEAN X, Y, Z;
            FOR X := TRUE, FALSE DO
                FOR Y := TRUE, FALSE DO
                BEGIN
                    OUTTEXT("A("); OUTBOOL(X);
                    OUTTEXT(") OR ");
                    OUTTEXT("B("); OUTBOOL(Y);
                    OUTTEXT("): ");
                    Z := A(X) OR ELSE B(Y);
                    OUTIMAGE;
                END;
        END ORTEST;

        ANDTEST;
        ORTEST;

    END TEST;

    TEST;
END.

```

```txt

A(T) AND B(T): AB
A(T) AND B(F): AB
A(F) AND B(T): A
A(F) AND B(F): A
A(T) OR B(T): A
A(T) OR B(F): A
A(F) OR B(T): AB
A(F) OR B(F): AB

```



## Smalltalk

The <code>and:</code> <code>or:</code> selectors are shortcircuit selectors but in order to avoid evaluation of the second operand, it must be a block: <code>a and: [ code ]</code> will evaluate the code only if a is true. On the other hand, <code>a and: b</code>, where b is an expression (not a block), behaves like the non-shortcircuit and (&amp;). (Same speech for or |)

```smalltalk
Smalltalk at: #a put: nil.
Smalltalk at: #b put: nil.

a := [:x| 'executing a' displayNl. x].
b := [:x| 'executing b' displayNl. x].

('false and false = %1' %
  { (a value: false) and: [ b value: false ] })
    displayNl.

('true or false = %1' %
  { (a value: true) or: [ b value: false ] })
    displayNl.

('false or true = %1' %
  { (a value: false) or: [ b value: true ] })
    displayNl.

('true and false = %1' %
  { (a value: true) and: [ b value: false ] })
    displayNl.
```



## SNOBOL4

Because of its unique success/failure model of flow control, Snobol does not use standard boolean operators or assignment. However, in &amp;fullscan mode Snobol exhibits short-circuit boolean behavior in pattern matches, with concatenation " " functioning as logical AND, and alternation " | " as logical OR.

The test statements below use a pattern constructed from the functions a( ) and b( ) and match it to the null string with deferred evaluation. This idiom allows the functions to self-report the expected short-circuit patterns.

```SNOBOL4
        define('a(val)') :(a_end)
a       out = 'A '
        eq(val,1) :s(return)f(freturn)
a_end

        define('b(val)') :(b_end)
b       out = 'B '
        eq(val,1) :s(return)f(freturn)
b_end

*       # Test and display
        &fullscan = 1
        output(.out,1,'-[-r1]') ;* Macro Spitbol
*       output(.out,1,'B','-')  ;* CSnobol
        define('nl()'):(nlx);nl output = :(return);nlx

        out = 'T and T: '; null ? *a(1) *b(1); nl()
        out = 'T and F: '; null ? *a(1) *b(0); nl()
        out = 'F and T: '; null ? *a(0) *b(1); nl()
        out = 'F and F: '; null ? *a(0) *b(0); nl()
        output =
        out = 'T or T: '; null ? *a(1) | *b(1); nl()
        out = 'T or F: '; null ? *a(1) | *b(0); nl()
        out = 'F or T: '; null ? *a(0) | *b(1); nl()
        out = 'F or F: '; null ? *a(0) | *b(0); nl()
end
```

```txt
T and T: A B
T and F: A B
F and T: A
F and F: A

T or T: A
T or F: A
F or T: A B
F or F: A B
```



## Standard ML

```sml
fun a r = ( print " > function a called\n"; r )
fun b r = ( print " > function b called\n"; r )

fun test_and b1 b2 = (
  print ("# testing (" ^ Bool.toString b1 ^ " andalso " ^ Bool.toString b2 ^ ")\n");
  ignore (a b1 andalso b b2) )

fun test_or b1 b2 = (
  print ("# testing (" ^ Bool.toString b1 ^ " orelse " ^ Bool.toString b2 ^ ")\n");
  ignore (a b1 orelse b b2) )

fun test_this test = (
  test true true;
  test true false;
  test false true;
  test false false )
;

print "
### = Testing and =
\n";
test_this test_and;
print "
### = Testing or =
\n";
test_this test_or;
```

### = Testing and =

 # testing (true andalso true)
  > function a called
  > function b called
 # testing (true andalso false)
  > function a called
  > function b called
 # testing (false andalso true)
  > function a called
 # testing (false andalso false)
  > function a called

### = Testing or =

 # testing (true orelse true)
  > function a called
 # testing (true orelse false)
  > function a called
 # testing (false orelse true)
  > function a called
  > function b called
 # testing (false orelse false)
  > function a called
  > function b called


## Stata


Stata always evaluates both arguments of operators & and |. Here is a solution with '''if''' statements.


```stata
function a(x) {
	printf(" a")
	return(x)
}

function b(x) {
	printf(" b")
	return(x)
}

function call(i, j) {
	printf("and:")
	x = a(i)
	if (x) {
		x = b(j)
	}
	printf("\nor:")
	y = a(i)
	if (!y) {
		y = b(j)
	}
	printf("\n")
	return((x,y))
}
```


'''Example'''


```stata
: call(0,1)
and: a
or: a b
       1   2
    +---------+
  1 |  0   1  |
    +---------+

: call(1,1)
and: a b
or: a
       1   2
    +---------+
  1 |  1   1  |
    +---------+
```



## Swift

Short circuit operators are <nowiki>&&</nowiki> and ||.

```swift
func a(v: Bool) -> Bool {
  print("a")
  return v
}

func b(v: Bool) -> Bool {
  print("b")
  return v
}

func test(i: Bool, j: Bool) {
  println("Testing a(\(i)) && b(\(j))")
  print("Trace:  ")
  println("\nResult: \(a(i) && b(j))")

  println("Testing a(\(i)) || b(\(j))")
  print("Trace:  ")
  println("\nResult: \(a(i) || b(j))")

  println()
}

test(false, false)
test(false, true)
test(true, false)
test(true, true)
```

```txt

Testing a(false) && b(false)
Trace:  a
Result: false
Testing a(false) || b(false)
Trace:  ab
Result: false

Testing a(false) && b(true)
Trace:  a
Result: false
Testing a(false) || b(true)
Trace:  ab
Result: true

Testing a(true) && b(false)
Trace:  ab
Result: false
Testing a(true) || b(false)
Trace:  a
Result: true

Testing a(true) && b(true)
Trace:  ab
Result: true
Testing a(true) || b(true)
Trace:  a
Result: true

```



## Tcl

The <code>&&</code> and <code>||</code> in the <code>expr</code> command support short-circuit evaluation. It is recommended that you always put expressions in braces so that and command or variable substitutions are applied at the right time rather than before the expression is evaluated at all. (Indeed, it is recommended that you do that anyway as unbraced expressions cannot be efficiently compiled.)

```tcl
package require Tcl 8.5
proc tcl::mathfunc::a boolean {
    puts "a($boolean) called"
    return $boolean
}
proc tcl::mathfunc::b boolean {
    puts "b($boolean) called"
    return $boolean
}

foreach i {false true} {
    foreach j {false true} {
        set x [expr {a($i) && b($j)}]
        puts "x = a($i) && b($j) = $x"
        set y [expr {a($i) || b($j)}]
        puts "y = a($i) || b($j) = $y"
        puts ""; # Blank line for clarity
    }
}
```

{{out}}Note that booleans may be written out words or numeric:

```txt

a(false) called
x = a(false) && b(false) = 0
a(false) called
b(false) called
y = a(false) || b(false) = 0

a(false) called
x = a(false) && b(true) = 0
a(false) called
b(true) called
y = a(false) || b(true) = 1

a(true) called
b(false) called
x = a(true) && b(false) = 0
a(true) called
y = a(true) || b(false) = 1

a(true) called
b(true) called
x = a(true) && b(true) = 1
a(true) called
y = a(true) || b(true) = 1


```



## TXR


```txr
@(define a (x out))
@  (output)
  a (@x) called
@  (end)
@  (bind out x)
@(end)
@(define b (x out))
@  (output)
  b (@x) called
@  (end)
@  (bind out x)
@(end)
@(define short_circuit_demo (i j))
@  (output)
a(@i) and b(@j):
@  (end)
@  (maybe)
@    (a i "1")
@    (b j "1")
@  (end)
@  (output)
a(@i) or b(@j):
@  (end)
@  (cases)
@    (a i "1")
@  (or)
@    (b j "1")
@  (or)
@    (accept)
@  (end)
@(end)
@(short_circuit_demo "0" "0")
@(short_circuit_demo "0" "1")
@(short_circuit_demo "1" "0")
@(short_circuit_demo "1" "1")
```

```txt
$ txr short-circuit-bool.txr
a(0) and b(0):
  a (0) called
a(0) or b(0):
  a (0) called
  b (0) called
a(0) and b(1):
  a (0) called
a(0) or b(1):
  a (0) called
  b (1) called
a(1) and b(0):
  a (1) called
  b (0) called
a(1) or b(0):
  a (1) called
a(1) and b(1):
  a (1) called
  b (1) called
a(1) or b(1):
  a (1) called
```

The <code>a</code> and <code>b</code> functions are defined such that the second parameter is intended to be an unbound variable. When the function binds  <code>out</code>, that value propagates back to the unbound variable at the call site. But the way calls works in this language allows us to specify a value instead such as <code>"1"</code>. So now the directive <code>@(bind out x)</code> performs unification instead: if <code>x</code> doesn't match <code>"1"</code>, the function fails, otherwise it succeeds.

So simply by placing two calls consecutively, we get a short circuting conjunction. The second will not execute if the first one fails.

Short-circuiting disjunction is provided by <code>@(cases)</code>.

The <code>@(maybe)</code> construct stops failure from propagating from the enclosed subquery. The <code>@(accept)</code> directive will bail out of the closest enclosing anonymous block (the function body) with a success. It prevents the <code>@(cases)</code> from failing the function if neither case is successful.


## UNIX Shell

The ''&&'' and ''||'' operators use the exit status of each command. The ''true'' and ''false'' commands convert a string to an exit status; our code ''&& x=true || x=false'' converts an exit status to a string.
```bash
a() {
	echo "Called a $1"
	"$1"
}

b() {
	echo "Called b $1"
	"$1"
}

for i in false true; do
	for j in false true; do
		a $i && b $j && x=true || x=false
		echo "  $i && $j is $x"

		a $i || b $j && y=true || y=false
		echo "  $i || $j is $y"
	done
done
```

The output reveals that <nowiki>&&</nowiki> and || have short-circuit evaluation.

```txt
Called a false
  false && false is false
Called a false
Called b false
  false || false is false
Called a false
  false && true is false
Called a false
Called b true
  false || true is true
Called a true
Called b false
  true && false is false
Called a true
  true || false is true
Called a true
Called b true
  true && true is true
Called a true
  true || true is true
```


=
## C Shell
=
Between commands, ''&&'' and ''||'' have short-circuit evaluation. (The aliases for ''a'' and ''b'' must expand to a single command; these aliases expand to an ''eval'' command.)

```csh
alias a eval \''echo "Called a \!:1"; "\!:1"'\'
alias b eval \''echo "Called b \!:1"; "\!:1"'\'

foreach i (false true)
	foreach j (false true)
		a $i && b $j && set x=true || set x=false
		echo "  $i && $j is $x"

		a $i || b $j && set x=true || set x=false
		echo "  $i || $j is $x"
	end
end
```

Inside expressions, ''&&'' and ''||'' can short circuit some commands, but cannot prevent substitutions.

```csh
# Succeeds, only prints "ok".
if ( 1 || { echo This command never runs. } ) echo ok

# Fails, aborts shell with "bad: Undefined variable".
if ( 1 || $bad ) echo ok

# Prints "error", then "ok".
if ( 1 || `echo error >/dev/stderr` ) echo ok
```



## VBA


```vb
Private Function a(i As Variant) As Boolean
    Debug.Print "a:  "; i = 1,
    a = i
End Function
Private Function b(j As Variant) As Boolean
    Debug.Print "b: "; j = 1;
    b = j
End Function
Public Sub short_circuit()
    Dim x As Boolean, y As Boolean
    'Dim p As Boolean, q As Boolean
    Debug.Print "
### ==AND==
" & vbCrLf
    For p = 0 To 1
        For q = 0 To 1
            If a(p) Then
                x = b(q)
            End If
            Debug.Print " = x"
        Next q
        Debug.Print
    Next p
    Debug.Print "
### ===OR==
" & vbCrLf
    For p = 0 To 1
        For q = 0 To 1
            If Not a(p) Then
                x = b(q)
            End If
            Debug.Print " = x"
        Next q
        Debug.Print
    Next p
    Debug.Print
End Sub

```
```txt

### ==AND==


a:  Onwaar     = x
a:  Onwaar     = x

a:  Waar      b: Onwaar = x
a:  Waar      b: Waar = x


### ===OR==


a:  Onwaar    b: Onwaar = x
a:  Onwaar    b: Waar = x

a:  Waar       = x
a:  Waar       = x
```


## Visual Basic .NET

```vbnet
Module Module1

    Function A(v As Boolean) As Boolean
        Console.WriteLine("a")
        Return v
    End Function

    Function B(v As Boolean) As Boolean
        Console.WriteLine("b")
        Return v
    End Function

    Sub Test(i As Boolean, j As Boolean)
        Console.WriteLine("{0} and {1} = {2} (eager evaluation)", i, j, A(i) And B(j))
        Console.WriteLine("{0} or {1} = {2} (eager evaluation)", i, j, A(i) Or B(j))
        Console.WriteLine("{0} and {1} = {2} (lazy evaluation)", i, j, A(i) AndAlso B(j))
        Console.WriteLine("{0} or {1} = {2} (lazy evaluation)", i, j, A(i) OrElse B(j))

        Console.WriteLine()
    End Sub

    Sub Main()
        Test(False, False)
        Test(False, True)
        Test(True, False)
        Test(True, True)
    End Sub

End Module
```

```txt
a
b
False and False = False (eager evaluation)
a
b
False or False = False (eager evaluation)
a
False and False = False (lazy evaluation)
a
b
False or False = False (lazy evaluation)

a
b
False and True = False (eager evaluation)
a
b
False or True = True (eager evaluation)
a
False and True = False (lazy evaluation)
a
b
False or True = True (lazy evaluation)

a
b
True and False = False (eager evaluation)
a
b
True or False = True (eager evaluation)
a
b
True and False = False (lazy evaluation)
a
True or False = True (lazy evaluation)

a
b
True and True = True (eager evaluation)
a
b
True or True = True (eager evaluation)
a
b
True and True = True (lazy evaluation)
a
True or True = True (lazy evaluation)
```



## Visual FoxPro


```vfp

*!* Visual FoxPro natively supports short circuit evaluation
CLEAR
CREATE CURSOR funceval(arg1 L, arg2 L, operation V(3), result L, calls V(10))
*!* Conjunction
INSERT INTO funceval (arg1, arg2, operation) VALUES (.F., .F., "AND")
REPLACE result WITH (a(arg1) AND b(arg2))
INSERT INTO funceval (arg1, arg2, operation) VALUES (.F., .T., "AND")
REPLACE result WITH (a(arg1) AND b(arg2))
INSERT INTO funceval (arg1, arg2, operation) VALUES (.T., .F., "AND")
REPLACE result WITH (a(arg1) AND b(arg2))
INSERT INTO funceval (arg1, arg2, operation) VALUES (.T., .T., "AND")
REPLACE result WITH (a(arg1) AND b(arg2))
*!* Disjunction
INSERT INTO funceval (arg1, arg2, operation) VALUES (.F., .F., "OR")
REPLACE result WITH (a(arg1) OR b(arg2))
INSERT INTO funceval (arg1, arg2, operation) VALUES (.F., .T., "OR")
REPLACE result WITH (a(arg1) OR b(arg2))
INSERT INTO funceval (arg1, arg2, operation) VALUES (.T., .F., "OR")
REPLACE result WITH (a(arg1) OR b(arg2))
INSERT INTO funceval (arg1, arg2, operation) VALUES (.T., .T., "OR")
REPLACE result WITH (a(arg1) OR b(arg2))
GO TOP

_VFP.DataToClip("funceval", 8, 3)

FUNCTION a(v As Boolean) As Boolean
REPLACE calls WITH "a()"
RETURN v
ENDFUNC

FUNCTION b(v As Boolean) As Boolean
REPLACE calls WITH calls + ", b()"
RETURN v
ENDFUNC

```

```txt

Arg1	Arg2	Operation	Result	Calls
F   	F   	AND      	F     	a()
F   	T   	AND      	F     	a()
T   	F   	AND      	F     	a(), b()
T   	T   	AND      	T     	a(), b()
F   	F   	OR       	F     	a(), b()
F   	T   	OR       	T     	a(), b()
T   	F   	OR       	T     	a()
T   	T   	OR       	T     	a()

```



## zkl


```zkl
fcn a(b){self.fcn.println(b); b}
fcn b(b){self.fcn.println(b); b}
```

```txt

a(True)  or b(True)  //-->Fcn(a)True, True
a(False) or b(True)  //-->Fcn(a)False, Fcn(b)True, True
a(False) or b(False) //-->Fcn(a)False, Fcn(b)False, False

a(True)  and b(True)  //-->Fcn(a)True, Fcn(b)True, True
a(True)  and b(False) //-->Fcn(a)True, Fcn(b)False, False
a(False) and b(True)  //-->Fcn(a)False, False

```



