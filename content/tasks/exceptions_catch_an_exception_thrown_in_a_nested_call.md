+++
title = "Exceptions/Catch an exception thrown in a nested call"
description = ""
date = 2019-10-18T10:01:05Z
aliases = []
[extra]
id = 4001
[taxonomies]
categories = ["task", "Control Structures"]
tags = []
languages = [
  "11l",
  "ada",
  "aime",
  "algol_68",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dwscript",
  "dyalect",
  "echolisp",
  "egl",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
  "factor",
  "fantom",
  "freebasic",
  "go",
  "haskell",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "langur",
  "lasso",
  "lua",
  "maple",
  "nemerle",
  "nim",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "smalltalk",
  "swift",
  "tcl",
  "txr",
  "ursala",
  "visual_basic_.net",
  "zkl",
]
+++

## Task

Show how to create a user-defined exception   and   show how to catch an exception raised from several nested calls away.

:#   Create two user-defined exceptions,   '''U0'''   and   '''U1'''.
:#   Have function   '''foo'''   call function   '''bar'''   twice.
:#   Have function   '''bar'''   call function   '''baz'''.
:#   Arrange for function   '''baz'''   to raise, or throw exception   '''U0'''   on its first call, then exception   '''U1'''   on its second.
:#   Function   '''foo'''   should catch only exception   '''U0''',   not   '''U1'''.



Show/describe what happens when the program is run.





## 11l

```11l
T U0 {}
T U1 {}

F baz(i)
   I i == 0
      X U0()
   E
      X U1()

F bar(i)
   baz(i)

F foo()
   L(i) 0..1
      X.try
         bar(i)
      X.catch U0
         print(‘Function foo caught exception U0’)

foo()
```


```txt

Function foo caught exception U0

```

The exact [http://rosettacode.org/wiki/Talk:Exceptions/Catch_an_exception_thrown_in_a_nested_call#11l_swallows_U1.3F behavior] for an uncaught exception is implementation-defined [as in C++].


## Ada


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Exceptions_From_Nested_Calls is
   U0 : exception;
   U1 : exception;
   Baz_Count : Natural := 0;
   procedure Baz is
   begin
      Baz_Count := Baz_Count + 1;
      if Baz_Count = 1 then
         raise U0;
      else
         raise U1;
      end if;
   end Baz;
   procedure Bar is
   begin
      Baz;
   end Bar;
   procedure Foo is
   begin
      Bar;
   exception
      when U0 =>
         Put_Line("Procedure Foo caught exception U0");
   end Foo;
begin
   for I in 1..2 loop
      Foo;
   end loop;
end Exceptions_From_Nested_Calls;
```

```txt

Procedure Foo caught exception U0

raised EXCEPTIONS_FROM_NESTED_CALLS.U1 : exceptions_from_nested_calls.adb:13

```

An unhandled exception leads to termination of the corresponding [[task]].
When the task is the main task of the program as in the example, the whole program is terminated. In the example the exception back tracing message is compiler-specific
(in this case it is [[GNAT]] and further depends on the compiler options.


## Aime


```aime
void
baz(integer i)
{
    error(cat("U", itoa(i)));
}

void
bar(integer i)
{
    baz(i);
}

void
foo(void)
{
    integer i;

    i = 0;
    while (i < 2) {
        text e;

        if (trap_d(e, bar, i)) {
            o_form("Exception `~' thrown\n", e);
            if (e != "U0") {
		o_text("will not catch exception\n");
		error(e);
	    }
	}
	i += 1;
    }

    o_text("Never reached.\n");
}

integer
main(void)
{
    foo();

    return 0;
}
```

```txt
Exception `U0' thrown
Exception `U1' thrown
will not catch exception
aime: nec: 26: U1
```

Exception U0 is caught, exception U1 is caught and re-thrown.  Program execution is terminated as the U1 exception is not caught when thrown the second time.


## ALGOL 68

The following example follows the method used by ALGOL 68 for handling events
in the language's ''transput'' prelude.   Note that in the ''transput'', then exception
is effectively bound to the '''file''' handle, hence different '''file''' events can be caught
by event handler associated to that particular '''file'''.  Similarly the following example
has bound two unique exceptions - ''u0'' & ''u1'' - to each unique instance of '''object'''.

c.f. [[Exceptions#ALGOL_68|ALGOL 68 Exceptions]] for more details.

```algol68
MODE OBJ = STRUCT(
  INT value,
  STRUCT(
    STRING message,
    FLEX[0]STRING args,
    PROC(REF OBJ)BOOL u0, u1
  ) exception
);

PROC on u0 = (REF OBJ self, PROC (REF OBJ) BOOL mended)VOID:
  u0 OF exception OF self := mended;

PROC on u1 = (REF OBJ self, PROC (REF OBJ) BOOL mended)VOID:
  u1 OF exception OF self := mended;

PRIO INIT = 1, RAISE = 1;

OP INIT = (REF OBJ self, INT value)REF OBJ: (
  value OF self := value;
  u0 OF exception OF self := u1 OF exception OF self := (REF OBJ skip)BOOL: FALSE;
  args OF exception OF self := message OF exception OF self := "OBJ Exception";
  self
);

OP RAISE = (REF OBJ self, PROC (REF OBJ) BOOL mended)VOID:
  IF NOT mended(self) THEN
    put(stand error, (message OF exception OF self+" not caught - stop", new line));
    stop
  FI;

PROC (REF OBJ)VOID bar, baz; # early declaration is required by the ALGOL 68RS subset language #

PROC foo := VOID:(
    FOR value FROM 0 TO 1 DO
        REF OBJ i = LOC OBJ INIT value;
        on u0(i, (REF OBJ skip)BOOL: (GO TO except u0; SKIP ));
            bar(i);
        GO TO end on u0;
        except u0:
            print(("Function foo caught exception u0", new line));
        end on u0: SKIP
    OD
);

# PROC # bar := (REF OBJ i)VOID:(
    baz(i) # Nest those calls #
);

# PROC # baz := (REF OBJ i)VOID:
    IF value OF i = 0 THEN
        i RAISE u0 OF exception OF i
    ELSE
        i RAISE u1 OF exception OF i
    FI;

foo
```

```txt

Function foo caught exception u0
OBJ Exception not caught - stop

```

Note: when an event occurs there are three possible responses.
# return '''false''' - in which case the default action takes place.
# ''mend'' the '''object''' and return '''true''' - date is ''mended'' and the program can continue from the point the event was raised.
# jump to an appropriately named ''label'' - effectively abandoning the offending section of code.

In the case of parallel processing, if the ''label'' is outside of the
'''par''' clause, then all parallel the threads are terminated and the
program continues in the parent thread. <!-- example needed -->

## AutoHotkey


###  True exceptions

In [[AutoHotkey_L]], [http://l.autohotkey.net/docs/commands/Try.htm Try], [http://l.autohotkey.net/docs/commands/Catch.htm Catch], and [http://l.autohotkey.net/docs/commands/Throw.htm Throw] are available to handle exceptions.<br/>
When this program is run, the first exception (U0) is raised, and caught by the try-catch section. This causes a Message Box containing the text "An exception was raised: First Exception" to be displayed by the script. The second exception is not caught, generating a runtime error.

```AHK
global U0 := Exception("First Exception")
global U1 := Exception("Second Exception")

foo()

foo(){
	try
		bar()
	catch e
		MsgBox % "An exception was raised: " e.Message
	bar()
}

bar(){
	baz()
}

baz(){
	static calls := 0
	if ( ++calls = 1 )
		throw U0
	else if ( calls = 2 )
		throw U1
}
```

The runtime error:

```txt
Error:  Second Exception

	Line#
	019: calls := 0
	001: U0 := Exception("First Exception")
--->	002: U1 := Exception("Second Exception")
	004: foo()
	006: {
	007: Try
	008: bar()
	009: Catch,e
	010: MsgBox,"An exception was raised: " e.Message
	011: bar()

The thread has exited.
```


=== ErrorLevel-based exceptions ===
[[AutoHotkey_Basic]] has very simple support for error tracking.
The global ErrorLevel keeps track of the last error.
Here is one way to keep track of nested errors:

```AutoHotkey
foo()
Return

foo()
{
  bar(0)
  If InStr(ErrorLevel, "U0")
    MsgBox caught error: U0
  bar(1)
  If InStr(ErrorLevel, "U0")
    MsgBox caught error: U0
}

bar(i)
{
  StringReplace, ErrorLevel, ErrorLevel, baz_error, , All   ; clear baz_error(s)
  If !baz(i)
    ErrorLevel .= "baz_error"  ; add baz_error to errorstack
}

baz(i)
{
  StringReplace, ErrorLevel, ErrorLevel, U1, , All   ; clear U1 errors
  StringReplace, ErrorLevel, ErrorLevel, U0, , All   ; clear U0 errors
  If i
    ErrorLevel .= "U1"  ; add U1 errors to errorstack
  Else
    ErrorLevel .= "U0"
  Return 1
}
```



## BBC BASIC

```bbcbasic
      REM Allocate error numbers:
      U0& = 123
      U1& = 124

      PROCfoo
      END

      DEF PROCfoo
      ON ERROR LOCAL IF ERR = U0& THEN PRINT "Exception U0 caught in foo" ELSE \
      \              RESTORE ERROR : ERROR ERR, REPORT$
      PROCbar
      PROCbar
      ENDPROC

      DEF PROCbar
      PROCbaz
      ENDPROC

      DEF PROCbaz
      PRIVATE called%
      called% += 1
      CASE called% OF
        WHEN 1: ERROR U0&, "Exception U0 thrown"
        WHEN 2: ERROR U1&, "Exception U1 thrown"
      ENDCASE
      ENDPROC

```

{{out}} (the second message is output by the default error handler):

```txt
Exception U0 caught in foo

Exception U1 thrown
```



## C


C doesn't have an exception handling mechanism, so we have to
decide what we want from an exception.

1. Return from a function with an error added to exception context.
2. Detect that a exception was thrown by checking the context after a function call.
3. Recover an error type and message.
4. Return from a function after throwing/catching an exception in a way which allows cleanup code to be called (vs. jumping outside the function).

In conclusion, try/throw/catch keywords are not available in C, nor is
their functionality, so while the following code tries to fulfill the task's
requirements, no attempt is made to mimic them. The goal has been to provide
some modicum level of usefulness for someone actually looking at this for
ideas for their own code.
U0 and U1 are boring for debugging purposes. Added something to help with that.


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct exception {
        int extype;
        char what[128];
} exception;

typedef struct exception_ctx {
        exception * exs;
        int size;
        int pos;
} exception_ctx;

exception_ctx * Create_Ex_Ctx(int length) {
        const int safety = 8; // alignment precaution.
        char * tmp = (char*) malloc(safety+sizeof(exception_ctx)+sizeof(exception)*length);
        if (! tmp) return NULL;
        exception_ctx * ctx = (exception_ctx*)tmp;
        ctx->size = length;
        ctx->pos = -1;
        ctx->exs = (exception*) (tmp + sizeof(exception_ctx));
        return ctx;
}

void Free_Ex_Ctx(exception_ctx * ctx) {
        free(ctx);
}

int Has_Ex(exception_ctx * ctx) {
        return (ctx->pos >= 0) ? 1 : 0;
}

int Is_Ex_Type(exception_ctx * exctx, int extype) {
        return (exctx->pos >= 0 && exctx->exs[exctx->pos].extype == extype) ? 1 : 0;
}

void Pop_Ex(exception_ctx * ctx) {
        if (ctx->pos >= 0) --ctx->pos;
}

const char * Get_What(exception_ctx * ctx) {
        if (ctx->pos >= 0) return ctx->exs[ctx->pos].what;
        return NULL;
}

int Push_Ex(exception_ctx * exctx, int extype, const char * msg) {
        if (++exctx->pos == exctx->size) {
                // Use last slot and report error.
                --exctx->pos;
                fprintf(stderr, "*** Error: Overflow in exception context.\n");
        }
        snprintf(exctx->exs[exctx->pos].what, sizeof(exctx->exs[0].what), "%s", msg);
        exctx->exs[exctx->pos].extype = extype;
        return -1;
}

//////////////////////////////////////////////////////////////////////

exception_ctx * GLOBALEX = NULL;
enum { U0_DRINK_ERROR = 10, U1_ANGRYBARTENDER_ERROR };

void baz(int n) {
        if (! n) {
                Push_Ex(GLOBALEX, U0_DRINK_ERROR , "U0 Drink Error. Insufficient drinks in bar Baz.");
                return;
        }
        else {
                Push_Ex(GLOBALEX, U1_ANGRYBARTENDER_ERROR , "U1 Bartender Error. Bartender kicked customer out of bar Baz.");
                return;
        }
}

void bar(int n) {
        fprintf(stdout, "Bar door is open.\n");
        baz(n);
        if (Has_Ex(GLOBALEX)) goto bar_cleanup;
        fprintf(stdout, "Baz has been called without errors.\n");
bar_cleanup:
        fprintf(stdout, "Bar door is closed.\n");
}

void foo() {
        fprintf(stdout, "Foo entering bar.\n");
        bar(0);
        while (Is_Ex_Type(GLOBALEX, U0_DRINK_ERROR)) {
                fprintf(stderr, "I am foo() and I deaall wrth U0 DriNk Errors with my own bottle... GOT oNE! [%s]\n", Get_What(GLOBALEX));
                Pop_Ex(GLOBALEX);
        }
        if (Has_Ex(GLOBALEX)) return;
        fprintf(stdout, "Foo left the bar.\n");
        fprintf(stdout, "Foo entering bar again.\n");
        bar(1);
        while (Is_Ex_Type(GLOBALEX, U0_DRINK_ERROR)) {
                fprintf(stderr, "I am foo() and I deaall wrth U0 DriNk Errors with my own bottle... GOT oNE! [%s]\n", Get_What(GLOBALEX));
                Pop_Ex(GLOBALEX);
        }
        if (Has_Ex(GLOBALEX)) return;
        fprintf(stdout, "Foo left the bar.\n");
}


int main(int argc, char ** argv) {
        exception_ctx * ctx = Create_Ex_Ctx(5);
        GLOBALEX = ctx;

        foo();
        if (Has_Ex(ctx)) goto main_ex;

        fprintf(stdout, "No errors encountered.\n");

main_ex:
        while (Has_Ex(ctx)) {
                fprintf(stderr, "*** Error: %s\n", Get_What(ctx));
                Pop_Ex(ctx);
        }
        Free_Ex_Ctx(ctx);
        return 0;
}



```

```txt
Foo entering bar.
Bar door is open.
Bar door is closed.
I am foo() and I deaall wrth U0 DriNk Errors with my own bottle... GOT oNE! [U0 Drink Error. Insufficient drinks in bar Baz.]
Foo left the bar.
Foo entering bar again.
Bar door is open.
Bar door is closed.
*** Error: U1 Bartender Error. Bartender kicked customer out of bar Baz.

```



## C++

First exception will be caught and message will be displayed,
second will be caught by the default exception handler,
which as required by the C++ Standard, will call terminate(),
aborting the task, typically with an error message.


```cpp
#include <iostream>
class U0 {};
class U1 {};

void baz(int i)
{
    if (!i) throw U0();
    else throw U1();
}
void bar(int i) { baz(i); }

void foo()
{
    for (int i = 0; i < 2; i++)
    {
        try {
            bar(i);
        } catch(U0 e) {
		std::cout<< "Exception U0 caught\n";
        }
    }
}

int main() {
    foo();
    std::cout<< "Should never get here!\n";
    return 0;
}
```


Result:

```txt

Exception U0 caught
This application has requested the Runtime to terminate it in an unusual way.

```

The exact behavior for an uncaught exception is implementation-defined.

## C#
This example will first catch U0 and print "U0 Caught" to the console when it does. The uncaught U1 exception will then cause the program to terminate and print the type of the exception, location of the error, and the stack.


```c#
using System; //Used for Exception and Console classes
class Exceptions
{
  class U0 : Exception { }
  class U1 : Exception { }
  static int i;
  static void foo()
  {
    for (i = 0; i < 2; i++)
      try
      {
        bar();
      }
      catch (U0) {
        Console.WriteLine("U0 Caught");
      }
  }
  static void bar()
  {
    baz();
  }
  static void baz(){
    if (i == 0)
      throw new U0();
    throw new U1();
  }

  public static void Main()
  {
    foo();
  }
}
```


```txt

U0 Caught
Unhandled Exception: Exceptions+U1: Exception of type 'Exceptions+U1' was thrown.
   at Exceptions.baz() in Program.cs:line 27
   at Exceptions.bar() in Program.cs:line 22
   at Exceptions.foo() in Program.cs:line 14
   at Exceptions.Main() in Program.cs:line 32

```




## Clojure


```clojure
(def U0 (ex-info "U0" {}))
(def U1 (ex-info "U1" {}))

(defn baz [x] (if (= x 0) (throw U0) (throw U1)))
(defn bar [x] (baz x))

(defn foo []
  (dotimes [x 2]
    (try
      (bar x)
      (catch clojure.lang.ExceptionInfo e
        (if (= e U0)
          (println "foo caught U0")
          (throw e))))))

(defn -main [& args]
  (foo))
```


```txt

foo caught U0

Exception in thread "main" clojure.lang.ExceptionInfo: U1 {}
	at clojure.core$ex_info.invoke(core.clj:4403)
	at X.core__init.load(Unknown Source)
	...

```


The first line of the output is generated from catching the U0 exception in function foo on the first call to bar.

On the second call to bar, U1 is caught and re-thrown, which gives a stack trace of the uncaught exception, U1.

This example uses clojure.lang.ExceptionInfo, but Java Exceptions can be used instead.


## Common Lisp



```lisp
(define-condition user-condition-1 (error) ())
(define-condition user-condition-2 (error) ())

(defun foo ()
  (dolist (type '(user-condition-1 user-condition-2))
    (handler-case
        (bar type)
      (user-condition-1 (c)
        (format t "~&foo: Caught: ~A~%" c)))))

(defun bar (type)
  (baz type))

(defun baz (type)
  (error type))    ; shortcut for (error (make-condition type))

(trace foo bar baz)
(foo)
```


{{out}} (the numbered lines are output from <code>trace</code>):

```lisp
  0: (FOO)
    1: (BAR USER-CONDITION-1)
      2: (BAZ USER-CONDITION-1)
foo: Caught: Condition USER-CONDITION-1 was signalled.
    1: (BAR USER-CONDITION-2)
      2: (BAZ USER-CONDITION-2)
```


At this point, the debugger (if any) is invoked
with the unhandled condition of type USER-CONDITION-2.


## D

First exception will be caught and message will be displayed,
second will be caught by default exception handler.

```d
class U0 : Exception {
    this() @safe pure nothrow { super("U0 error message"); }
}

class U1 : Exception {
    this() @safe pure nothrow { super("U1 error message"); }
}

void foo() {
    import std.stdio;

    foreach (immutable i; 0 .. 2) {
        try {
            i.bar;
        } catch (U0) {
            "Function foo caught exception U0".writeln;
        }
    }
}

void bar(in int i) @safe pure {
    i.baz;
}

void baz(in int i) @safe pure {
    throw i ? new U1 : new U0;
}

void main() {
    foo;
}
```

```txt
test.U1(at)test.d(8): U1 error message
----------------
\test.d(20): pure void test.bar(int)
\test.d(25): void test.baz()
\test.d(33): _Dmain
----------------
Exception U0 caught
```



## Delphi

```delphi
program ExceptionsInNestedCall;

{$APPTYPE CONSOLE}

uses SysUtils;

type
  U0 = class(Exception)
  end;
  U1 = class(Exception)
  end;

procedure Baz(i: Integer);
begin
  if i = 0 then
    raise U0.Create('U0 Error message')
  else
    raise U1.Create('U1 Error message');
end;

procedure Bar(i: Integer);
begin
  Baz(i);
end;

procedure Foo;
var
  i: Integer;
begin
  for i := 0 to 1 do
  begin
    try
      Bar(i);
    except
      on E: U0 do
        Writeln('Exception ' + E.ClassName + ' caught');
    end;
  end;
end;

begin
  Foo;
end.
```


```txt

Exception U0 caught

```


The uncaught exception shows a Windows Error Report dialog.



## DWScript

First exception will be caught and message will be displayed, second will be caught by default exception handler.

```delphi
type Exception1 = class (Exception) end;
type Exception2 = class (Exception) end;

procedure Baz(i : Integer);
begin
   if i=0 then
      raise new Exception1('Error message 1')
   else raise new Exception2('Error message 2');
end;

procedure Bar(i : Integer);
begin
   Baz(i);
end;

procedure Foo;
var
   i : Integer;
begin
   for i:=0 to 2 do begin
      try
         Bar(i);
      except
         on E : Exception1 do
            PrintLn(E.ClassName+' caught');
      end;
   end;
end;

Foo;
```

Result:

```txt
Exception1 caught
User defined exception: Error message 2
```



## Dyalect



```dyalect
type U0()
type U1()
var bazCallCount = 0

func baz() {
    bazCallCount += 1
    if bazCallCount == 1 {
        throw U0()
    } else if bazCallCount == 2 {
        throw U1()
    }
}

func bar() {
    baz()
}

func foo() {
    var calls = 2
    while calls > 0 {
        try {
            bar()
        } catch {
            U0() => print("U0 caught.")
        }
        calls -= 1
    }
}
```


```txt
U0 caught.
Runtime exception Dy601: U1
Stack trace: ...
```



## EchoLisp


```lisp

(define (foo)
  (for ((i 2))
    (try
    (bar i)
    (catch (id message)
      (if (= id 'U0)
         (writeln message 'catched)
         (error id "not catched"))))))

(define (bar i)
    (baz i))

(define (baz i)
    (if (= i 0)
        (throw 'U0 "U0 raised")
        (throw 'U1 "U1 raised")))


(foo) →
    "U0 raised"     catched
    👓 error: U1 not catched

```



## Eiffel

{{works with|SmartEiffel}} version 2.4

A file called main.e:

```eiffel
class MAIN
    inherit EXCEPTIONS

    creation foo

feature {ANY}
    baz_calls: INTEGER

    feature foo is
        do
            Current.bar
        rescue
            if is_developer_exception_of_name("U0") then
                baz_calls := 1
                print("Caught U0 exception.%N")
                retry
            end
            if is_developer_exception then
                print("Won't catch ")
                print(developer_exception_name)
                print(" exception...%N")
            end
        end

    feature bar is
        do
            Current.baz
        end

    feature baz is
        do
            if baz_calls = 0 then
                raise("U0")
            else
                raise("U1")
            end
        end
end
```


```txt
Caught U0 exception.
Won't catch U1 exception...
Exception number 3 not handled.
Developer exception:
3 frames in current stack.
=====  Bottom of run-time stack  =====
<system root>
Current = MAIN#0x8068038
        [ baz_calls = 1
        ]
line 9 column 13 file ./main.e

### ================================

foo MAIN
Current = MAIN#0x8068038
        [ baz_calls = 1
        ]
line 21 column 17 file ./main.e

### =   Rescue stack  ==============

bar MAIN
Current = MAIN#0x8068038
        [ baz_calls = 1
        ]
line 27 column 21 file ./main.e
=====   Top of run-time stack    =====
Exception number 3 not handled.
Developer exception:
```



## EGL

```EGL
record U0 type Exception
end

record U1 type Exception
end

program Exceptions

    function main()
        foo();
    end

    function foo()
        try
            bar();
        onException(ex U0)
            SysLib.writeStdout("Caught a U0 with message: '" :: ex.message :: "'");
        end
        bar();
    end

    function bar()
        baz();
    end

    firstBazCall boolean = true;
    function baz()
        if(firstBazCall)
            firstBazCall = false;
            throw new U0{message = "This is the U0 exception"};
        else
            throw new U1{message = "This is the U1 exception"};
        end
    end
end
```


```txt

Caught a U0 with message: 'This is the U0 exception'
This is the U1 exception

```


## Elena

ELENA 4.1 :

```elena
import extensions;

class U0 : Exception;

class U1 : Exception;

singleton Exceptions
{
    static int i;

    bar()
        <= baz();

    baz()
    {
        if (i == 0)
        {
            U0.new().raise()
        }
        else
        {
            U1.new().raise()
        }
    }

    foo()
    {
        for(i := 0, i < 2, i += 1)
        {
            try
            {
                self.bar()
            }
            catch:(U0 e)
            {
                console.printLine("U0 Caught")
            }
        }
    }
}

public program()
{
    Exceptions.foo()
}
```

```txt

U0 Caught
mytest'U1#class
Call stack:
system'Exception#class.new$system'LiteralValue[1]:exceptions.l(124)
system'Exception#class.new[0]:exceptions.l(128)
mytest'Exceptions.baz[0]:test.l(21)
mytest'Exceptions.bar[0]:test.l(12)
mytest'Exceptions.foo[0]:test.l(30)
mytest'program.eval[0]:test.l(45)
system'#inline1AB.start[1]:win32_app.l(35)
system'startUp(1)

```



## Elixir


```elixir
defmodule U0, do: defexception [:message]
defmodule U1, do: defexception [:message]

defmodule ExceptionsTest do
  def foo do
    Enum.each([0,1], fn i ->
      try do
        bar(i)
      rescue
        U0 -> IO.puts "U0 rescued"
      end
    end)
  end

  def bar(i), do: baz(i)

  def baz(0), do: raise U0
  def baz(1), do: raise U1
end

ExceptionsTest.foo
```


```txt

U0 rescued
** (U1) got nil while retrieving Exception.message/1 for %U1{message: nil} (expected a string)
    ExceptionsTest.exs:18: ExceptionsTest.baz/1
    ExceptionsTest.exs:8: anonymous fn/1 in ExceptionsTest.foo/0
    (elixir) lib/enum.ex:645: Enum."-each/2-lists^foreach/1-0-"/2
    (elixir) lib/enum.ex:645: Enum.each/2
    (elixir) lib/code.ex:370: Code.require_file/2

```

displayed message in version 1.4


## Erlang


```Erlang

-module( exceptions_catch ).

-export( [task/0] ).

task() -> [foo(X) || X<- lists:seq(1, 2)].



baz( 1 ) -> erlang:throw( u0 );
baz( 2 ) -> erlang:throw( u1 ).

foo( N ) ->
	try
	baz( N )

	catch
	_:u0 -> io:fwrite( "Catched ~p~n", [u0] )

	end.

```

```txt

76> exceptions_catch:task().
Catched u0
** exception throw: u1
     in function  exceptions_catch:baz/1 (src/exceptions_catch.erl, line 10)
     in call from exceptions_catch:foo/1 (src/exceptions_catch.erl, line 14)
     in call from exceptions_catch:'-task/0-lc$^0/1-0-'/1 (src/exceptions_catch.erl, line 5)
     in call from exceptions_catch:'-task/0-lc$^0/1-0-'/1 (src/exceptions_catch.erl, line 5)

```



## Factor


```factor
USING: combinators.extras continuations eval formatting kernel ;
IN: rosetta-code.nested-exceptions

ERROR: U0 ;
ERROR: U1 ;

: baz ( -- )
    "IN: rosetta-code.nested-exceptions : baz ( -- ) U1 ;"
    ( -- ) eval U0 ;

: bar ( -- ) baz ;

: foo ( -- )
    [
        [ bar ] [
            dup T{ U0 } =
            [ "%u recovered\n" printf ] [ rethrow ] if
        ] recover
    ] twice ;

foo
```

```txt

T{ U0 } recovered
U1

(U) Quotation: [ c-to-factor => ]
    Word: c-to-factor
(U) Quotation: [ [ (get-catchstack) push ] dip call => (get-catchstack) pop* ]
(O) Word: command-line-startup
(O) Word: run-script
(O) Word: foo
(O) Word: baz
(O) Word: U1
(O) Method: M\ object throw
(U) Quotation: [
        OBJ-CURRENT-THREAD special-object error-thread set-global
        current-continuation => error-continuation set-global
        [ original-error set-global ] [ rethrow ] bi
    ]

```



## Fantom



```fantom

const class U0 : Err
{
  new make () : super ("U0") {}
}

const class U1 : Err
{
  new make () : super ("U1") {}
}

class Main
{
  Int bazCalls := 0

  Void baz ()
  {
    bazCalls += 1
    if (bazCalls == 1)
      throw U0()
    else
      throw U1()
  }

  Void bar ()
  {
    baz ()
  }

  Void foo ()
  {
    2.times
    {
      try
      {
        bar ()
      }
      catch (U0 e)
      {
        echo ("Caught U0")
      }
    }
  }

  public static Void main ()
  {
    Main().foo
  }
}

```


```txt

Caught U0
nestedexceptions_0::U1: U1
  nestedexceptions_0::U1.<init> (nested-exceptions.fan)
  nestedexceptions_0::U1.make (nested-exceptions.fan:9)
  nestedexceptions_0::Main.baz (nested-exceptions.fan:22)
  nestedexceptions_0::Main.bar (nested-exceptions.fan:27)
  nestedexceptions_0::Main.foo (nested-exceptions.fan:36)
  fan.sys.FanInt.times (FanInt.java:492)
  nestedexceptions_0::Main.foo (nested-exceptions.fan:33)
  nestedexceptions_0::Main.main (nested-exceptions.fan:47)
  java.lang.reflect.Method.invoke (Method.java:597)
  fan.sys.Method.invoke (Method.java:552)
  fan.sys.Method$MethodFunc.callList (Method.java:198)
  fan.sys.Method.callList (Method.java:138)
  fanx.tools.Fan.callMain (Fan.java:135)
  fanx.tools.Fan.executeFile (Fan.java:88)
  fanx.tools.Fan.execute (Fan.java:34)
  fanx.tools.Fan.run (Fan.java:250)
  fanx.tools.Fan.main (Fan.java:288)

```


The output shows the first exception is caught and handled.
The second exception is not handled, and results in the program finishing
and printing a stack trace.


## FreeBASIC

FreeBASIC does not support exceptions or the Try/Catch/Finally statement, as such. However, you can use the Err() function, together with an If (or Switch) statement, to provide somewhat similar functionality:

```freebasic
' FB 1.05.0 Win64

Enum ErrorTypes
  U0 = 1000
  U1
End Enum

Function errorName(ex As ErrorTypes) As String
  Select Case As Const ex
    Case U0
      Return "U0"
    Case U1
      Return "U1"
  End Select
End Function

Sub catchError(ex As ErrorTypes)
   Dim e As Integer = Err '' cache the error number
   If e = ex Then
     Print "Error "; errorName(ex); ", number"; ex; " caught"
   End If
End Sub

Sub baz()
  Static As Integer timesCalled = 0 '' persisted between procedure calls
  timesCalled += 1
  If timesCalled = 1 Then
    err = U0
  Else
    err = U1
  End if
End Sub

Sub bar()
  baz
End Sub

Sub foo()
  bar
  catchError(U0) '' not interested in U1, assumed non-fatal
  bar
  catchError(U0)
End Sub

Foo
Print
Print "Press any key to quit"
Sleep
```


```txt

Error U0, number 1000 caught

```



## Go

Not strictly conforming to task description as foo does not directly call bar.

The panic/recover mechanism of Go is missing (by design)
some elements of exception handling needed for this task.
Specifically, a function that recovers a panic cannot resume
execution of the remainder of the function.
If foo recovers a panic in the first call to bar, there is no way for it
to make the second call to bar.
The solution here is to define a wrapper, or proxy function, called try.
Function foo calls bar indirectly through try.

```go
// Outline for a try/catch-like exception mechanism in Go
//
// As all Go programmers should know, the Go authors are sharply critical of
// the try/catch idiom and consider it bad practice in general.
// See http://golang.org/doc/go_faq.html#exceptions

package main

import (
    "fmt"
    "runtime"
    "strings"
)

// trace is for pretty output for the Rosetta Code task.
// It would have no place in a practical program.
func trace(s string) {
    nc := runtime.Callers(2, cs)
    f := runtime.FuncForPC(cs[0])
    fmt.Print(strings.Repeat("  ", nc-3), f.Name()[5:], ": ", s, "\n")
}

var cs = make([]uintptr, 10)

type exception struct {
    name    string
    handler func()
}

// try implents the try/catch-like exception mechanism.  It takes a function
// to be called, and a list of exceptions to catch during the function call.
// Note that for this simple example, f has no parameters.  In a practical
// program it might, of course.  In this case, the signature of try would
// have to be modified to take these parameters and then supply them to f
// when it calls f.
func try(f func(), exs []exception) {
    trace("start")
    defer func() {
        if pv := recover(); pv != nil {
            trace("Panic mode!")
            if px, ok := pv.(exception); ok {
                for _, ex := range exs {
                    if ex.name == px.name {
                        trace("handling exception")
                        px.handler()
                        trace("panic over")
                        return
                    }
                }
            }
            trace("can't recover this one!")
            panic(pv)
        }
    }()
    f()
    trace("complete")
}

func main() {
    trace("start")
    foo()
    trace("complete")
}

// u0, u1 declared at package level so they can be accessed by any function.
var u0, u1 exception

// foo.  Note that function literals u0, u1 here in the lexical scope
// of foo serve the purpose of catch blocks of other languages.
// Passing u0 to try serves the purpose of the catch condition.
// While try(bar... reads much like the try statement of other languages,
// this try is an ordinary function.  foo is passing bar into try,
// not calling it directly.
func foo() {
    trace("start")
    u0 = exception{"U0", func() { trace("U0 handled") }}
    u1 = exception{"U1", func() { trace("U1 handled") }}
    try(bar, []exception{u0})
    try(bar, []exception{u0})
    trace("complete")
}

func bar() {
    trace("start")
    baz()
    trace("complete")
}

var bazCall int

func baz() {
    trace("start")
    bazCall++
    switch bazCall {
    case 1:
        trace("panicking with execption U0")
        panic(u0)
    case 2:
        trace("panicking with execption U1")
        panic(u1)
    }
    trace("complete")
}
```

```txt

main: start
  foo: start
    try: start
      bar: start
        baz: start
        baz: panicking with execption U0
            _func_001: Panic mode!
            _func_001: handling exception
              _func_002: U0 handled
            _func_001: panic over
    try: start
      bar: start
        baz: start
        baz: panicking with execption U1
            _func_001: Panic mode!
            _func_001: can't recover this one!
panic: (main.exception) (0x468040,0xf8400273c0) [recovered]
        panic: (main.exception) (0x468040,0xf8400273c0)

goroutine 1 [running]:
main._func_001(0x2af727232f20, 0x2af727232100, 0x2af727232fb8, 0x2af727232e70)
        t.go:52 +0x1d9
----- stack segment boundary -----
main.baz()
        t.go:100 +0xd1
main.bar()
        t.go:85 +0x31
main.try(0x40105b, 0x2af727232f68, 0x100000001, 0x478dec)
        t.go:55 +0x4f
main.foo()
        t.go:79 +0x16c
main.main()
        t.go:61 +0x31

```

A simpler example, closer to the task description:

```go
package main

import "fmt"

type U0 struct {
	error
	s string
}
type U1 int

func foo2() {
	defer func() {
		// We can't just "catch" U0 and ignore U1 directly but ...
		if e := recover(); e != nil {
			// e can be of any type, check for type U0
			if x, ok := e.(*U0); ok {
				// we can only execute code here,
				// not return to the body of foo2
				fmt.Println("Recovered U0:", x.s)
				// We could cheat and call bar the second time
				// from here, if it paniced again (even with U0)
				// it wouldn't get recovered.
				// Instead we've split foo into two calls to foo2.
			} else {
				// ... if we don't want to handle it we can
				// pass it along.
				fmt.Println("passing on:", e)
				panic(e) // like a "re-throw"
			}
		}
	}()
	bar()
}

func foo() {
	// Call bar twice via foo2
	foo2()
	foo2()
	fmt.Println("not reached")
}

func bar() int {
	return baz()
}

var done bool

func baz() int {
	if !done {
		done = true
		panic(&U0{nil, "a message"})
	}
	panic(U1(42))
}

func main() {
	foo()
	fmt.Println("No panic")
}
```

[http://play.golang.org/p/X2pa8zE1Ce Run in Go Playground].
```txt
Recovered U0: a message
passing on: 42
panic: (main.U1) (0xfc140,0x2a) [recovered]
	panic: (main.U1) (0xfc140,0x2a)
[... go-routine and stack trace omitted ...]

```



## Haskell



```haskell
import Control.Monad.Error
import Control.Monad.Trans (lift)

-- Our "user-defined exception" tpe
data MyError = U0 | U1 | Other deriving (Eq, Read, Show)

-- Required for any error type
instance Error MyError where
  noMsg    = Other
  strMsg _ = Other

-- Throwing and catching exceptions implies that we are working in a monad. In
-- this case, we use ErrorT to support our user-defined exceptions, wrapping
-- IO to be able to report the happenings. ('lift' converts ErrorT e IO a
-- actions into IO a actions.)

foo = do lift (putStrLn "foo")
         mapM_ (\toThrow -> bar toThrow                      -- the protected call
                              `catchError` \caught ->        -- the catch operation
                                                             -- ↓ what to do with it
                                 case caught of U0 -> lift (putStrLn "foo caught U0")
                                                _  -> throwError caught)
               [U0, U1]                                      -- the two exceptions to throw


bar toThrow = do lift (putStrLn " bar")
                 baz toThrow

baz toThrow = do lift (putStrLn "  baz")
                 throwError toThrow

-- We cannot use exceptions without at some outer level choosing what to do
-- if an exception propagates all the way up. Here we just print the exception
-- if there was one.
main = do result <- runErrorT foo
          case result of
            Left e  -> putStrLn ("Caught error at top level: " ++ show e)
            Right v -> putStrLn ("Return value: " ++ show v)
```


```txt

 foo
  bar
   baz
 foo caught U0
  bar
   baz
 Caught error at top level: U1

```


==Icon and {{header|Unicon}}==

The following Unicon example makes use of support for exceptions found
in the [http://tapestry.tucson.az.us/unilib/ The Unicon Code Library].
<i>Since exception support is not built into Unicon,
but rather implemented as Unicon code, there are limitations
not found in languages that natively support exceptions.</i>


```Unicon
import Exceptions

class U0 : Exception()
    method getMessage()
        return "U0: " || (\message | "unknown")
    end
end

class U1 : Exception()
    method getMessage()
        return "U1: " || (\message | "unknown")
    end
end

procedure main()
    # (Because Exceptions are not built into Unicon, uncaught
    #   exceptions are ignored.  This clause will catch any
    #   exceptions not caught farther down in the code.)
    case Try().call{ foo() } of {
        Try().catch(): {
            ex := Try().getException()
            write(ex.getMessage(), ":\n", ex.getLocation())
            }
        }
end

procedure foo()
    every 1|2 do {
        case Try().call{ bar() } of {
            Try().catch("U0"): {
                ex := Try().getException()
                write(ex.getMessage(), ":\n", ex.getLocation())
                }
            }
        }
end

procedure bar()
    return baz()
end

procedure baz()
    initial U0().throw("First exception")
    U1().throw("Second exception")
end
```


```txt

U0: First exception:
    procedure baz [Etest5.icn:43]
    procedure bar [Etest5.icn:39]
    procedure foo [Etest5.icn:29]

U1: Second exception:
    procedure baz [Etest5.icn:44]
    procedure bar [Etest5.icn:39]
    procedure foo [Etest5.icn:29]

```


Note: it may be possible to implement exceptions in Icon; however,
it would require a major rework and would likely be inelegant.


## Io


```Io
U0 := Exception clone
U1 := Exception clone

foo := method(
    for(i,1,2,
        try(
            bar(i)
        )catch( U0,
            "foo caught U0" print
        )pass
    )
)
bar := method(n,
    baz(n)
)
baz := method(n,
    if(n == 1,U0,U1) raise("baz with n = #{n}" interpolate)
)

foo
```

```txt
foo caught U0
  U1: baz with n = 2
  ---------
  U1 raise                             exceptions_catch_nested.io 34
  Object baz                           exceptions_catch_nested.io 31
  Object bar                           exceptions_catch_nested.io 24
```

The first line comes from when U0 was caught and the second from when U1 was raised and not caught.  This is followed by a traceback with the most recent call first.


## J


'''Solution:'''

J leaves most of the implementation of exceptions to the programmer, so:


```J
main=: monad define
  smoutput 'main'
  try. foo ''
  catcht. smoutput 'main caught ',type_jthrow_
  end.
)

foo=: monad define
  smoutput '  foo'
  for_i. 0 1 do.
    try. bar i
    catcht. if. type_jthrow_-:'U0' do. smoutput '  foo caught ',type_jthrow_ else. throw. end.
    end.
  end.
)

bar=: baz [ smoutput bind '    bar'

baz=: monad define
  smoutput '      baz'
  type_jthrow_=: 'U',":y throw.
)
```


'''Example use:'''

```j
   main ''
main
  foo
    bar
      baz
  foo caught U0
    bar
      baz
main caught U1
```



## Java

Methods that may throw an exception (or that call a method
that may throw an exception that it does not catch)
must explicitly declare that they can throw such an exception
(or a superclass thereof), unless they are unchecked exceptions
(subclasses of <code>RuntimeException</code> or <code>Error</code>):

```java
class U0 extends Exception { }
class U1 extends Exception { }

public class ExceptionsTest {
    public static void foo() throws U1 {
        for (int i = 0; i <= 1; i++) {
            try {
                bar(i);
            } catch (U0 e) {
                System.out.println("Function foo caught exception U0");
            }
        }
    }

    public static void bar(int i) throws U0, U1 {
        baz(i); // Nest those calls
    }

    public static void baz(int i) throws U0, U1 {
        if (i == 0)
            throw new U0();
        else
            throw new U1();
    }

    public static void main(String[] args) throws U1 {
        foo();
    }
}
```

```txt

Function foo caught exception U0
Exception in thread "main" U1
	at ExceptionsTest.baz(ExceptionsTest.java:23)
	at ExceptionsTest.bar(ExceptionsTest.java:16)
	at ExceptionsTest.foo(ExceptionsTest.java:8)
	at ExceptionsTest.main(ExceptionsTest.java:27)

```

The first line of the output is generated from catching the U0 exception
in function foo.

Uncaught exceptions give information showing where the exception
originated through the nested function calls together with the name
of the uncaught exception, (U1) to stderr,
then quit the running program.


## JavaScript

{{works with|Firefox}} except for the print() function

The <code>callee.name</code> property, and the <code>catch(e if ...)</code> statement are Mozilla JavaScript extensions.


```javascript
function U() {}
U.prototype.toString = function(){return this.className;}

function U0() {
    this.className = arguments.callee.name;
}
U0.prototype = new U();

function U1() {
    this.className = arguments.callee.name;
}
U1.prototype = new U();

function foo() {
    for (var i = 1; i <= 2; i++) {
        try {
            bar();
        }
        catch(e if e instanceof U0) {
            print("caught exception " + e);
        }
    }
}

function bar() {
    baz();
}

function baz() {
    // during the first call, redefine the function for subsequent calls
    baz = function() {throw(new U1());}
    throw(new U0());
}

foo();
```

{{out}} from [[Rhino]]:

```txt
caught exception U0
js: "nested_calls.js", line 31: exception from uncaught JavaScript throw: U1
```

{{out}} from [[SpiderMonkey]]:

```txt
caught exception U0
uncaught exception: U1
```


## jq

```jq
# n is assumed to be the number of times baz has been previously called:
def baz(n):
  if n==0 then error("U0")
  elif n==1 then error("U1")
  else "Goodbye"
  end;

def bar(n): baz(n);

def foo:
  (try bar(0) catch if . == "U0" then "We caught U0" else error(.) end),
  (try bar(1) catch if . == "U0" then "We caught U0" else error(.) end);

foo
```

 $ jq -n -f Catch_an_exception_thrown_in_a_nested_call.jq
 "We caught U0"
 jq: error: U1


## Julia

```julia
struct U0 <: Exception end
struct U1 <: Exception end

function foo()
    for i in 1:2
        try
            bar()
        catch err
            if isa(err, U0) println("catched U0")
            else rethrow(err) end
        end
    end
end

function bar()
    baz()
end

function baz()
    if isdefined(:_called) && _called
        throw(U1())
    else
        global _called = true
        throw(U0())
    end
end

foo()
```


```txt
catched U0
LoadError: U1()
while loading /home/giovanni/documents/workspace/julia/Rosetta-Julia/src/Catch_an_exception_thrown_in_a_nested_call.jl, in expression starting on line 31
in foo at Rosetta-Julia/src/Catch_an_exception_thrown_in_a_nested_call.jl:10
in baz at Rosetta-Julia/src/Catch_an_exception_thrown_in_a_nested_call.jl:24
```



## Kotlin


```scala
// version 1.0.6

class U0 : Throwable("U0 occurred")
class U1 : Throwable("U1 occurred")

fun foo() {
    for (i in 1..2) {
        try {
            bar(i)
        } catch(e: U0) {
            println(e.message)
        }
    }
}

fun bar(i: Int) {
    baz(i)
}

fun baz(i: Int) {
    when (i) {
        1 -> throw U0()
        2 -> throw U1()
    }
}

fun main(args: Array<String>) {
    foo()
}
```


```txt

U0 occurred
Exception in thread "main" U1: U1 occurred
        at ExceptionsKt.baz(exceptions.kt:23)
        at ExceptionsKt.bar(exceptions.kt:17)
        at ExceptionsKt.foo(exceptions.kt:9)
        at ExceptionsKt.main(exceptions.kt:28)

```



## Langur

Exceptions in langur are hashes that are guaranteed to always contain certain fields.

There is no explicit try block.


```Langur
val .U0 = h{"msg": "U0"}
val .U1 = h{"msg": "U1"}

val .baz = f(.i) throw if(.i==0: .U0; .U1)
val .bar = f(.i) .baz(.i)

val .foo = f() {
    for .i in [0, 1] {
        .bar(.i)
        catch if .err["msg"] == .U0["msg"] {
            writeln "caught .U0 in .foo()"
        } else {
            throw
        }
    }
}

.foo()
```


```txt
caught .U0 in .foo()
VM Errors
general: U1 (.baz)
```



## Lasso

Lasso currently does not currently have a try mechanic —
but we can easily add one like so.


```Lasso
define try(exception) => {
    local(
        gb = givenblock,
        error
    )
    handle => {
        // Only relay error if it's not the specified exception
        if(#error) => {
            if(#error->get(2) == #exception) => {
                stdoutnl('Handled exception: '+#error->get(2))
            else
                stdoutnl('Throwing exception: '+#error->get(2))
                fail(:#error)
            }
        }
    }
    protect => {
        handle_error => {
            #error = (:error_code,error_msg,error_stack)
        }
        #gb()
    }
}

define foo => {
    stdoutnl('foo')
    try('U0') => { bar }
    try('U0') => { bar }
}

define bar => {
    stdoutnl('- bar')
    baz()
}

define baz => {
    stdoutnl('  - baz')
    var(bazzed) ? fail('U1') | $bazzed = true
    fail('U0')
}
```


```txt
foo
- bar
- baz
Handled exception: U0
- bar
- baz
Throwing exception: U1
```


;Error Stack:


```txt
U1
13:2 error.lasso
38:19 Debugger
33:5 Debugger
28:20 Debugger
21:9 Debugger
18:9 Debugger
6:5 Debugger
```



## Lua


```Lua
local baz_counter=1
function baz()
  if baz_counter==1 then
    baz_counter=baz_counter+1
    error("U0",3)--3 sends it down the call stack.
  elseif baz_counter==2 then
    error("U1",3)--3 sends it down the call stack.
  end
end

function bar()
  baz()
end

function foo()
  function callbar()
    local no_err,result = pcall(bar)
    --pcall is a protected call which catches errors.
    if not no_err then
      --If there are no errors, pcall returns true.
      if not result:match("U0") then
        --If the error is not a U0 error, rethrow it.
        error(result,2)
        --2 is the distance down the call stack to send
        --the error. We want it to go back to the callbar() call.
      end
    end
  end
  callbar()
  callbar()
end

foo()

```

output:

```txt
lua: errorexample.lua:31: U1
stack traceback:
        [C]: in function 'error'
        errorexample.lua:24: in function 'callbar'
        errorexample.lua:31: in function 'foo'
        errorexample.lua:34: in main chunk
        [C]: ?
```



## Maple

```Maple
num_times:=0:
baz := proc()
    global num_times := num_times + 1;
    error `if`(num_times <= 1, "U0", "U1");
end proc:

bar:=proc()
    baz();
end proc;

foo:=proc()
    local i;
    for i from 0 to 1 do
        bar();
    end do;
end proc;
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
foo[] := Catch[ bar[1]; bar[2]; ]

bar[i_] := baz[i];

baz[i_] := Switch[i,
  1, Throw["Exception U0 in baz"];,
  2, Throw["Exception U1 in baz"];]
```

Output:

```txt
 foo[]
-> Exception U0 in baz
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function exceptionsCatchNestedCall()
    function foo()

        try
            bar(1);
            bar(2);
        catch
            disp(lasterror);
            rethrow(lasterror);
        end

    end

    function bar(i)
        baz(i);
    end

    function baz(i)
        switch i
            case 1
                error('BAZ:U0','HAHAHAH');
            case 2
                error('BAZ:U1','AWWWW');
            otherwise
                disp 'I can''t do that Dave.';
        end
    end

    foo();

end
```

```MATLAB>>
 exceptionsCatchNestedCall()
       message: [1x177 char]
    identifier: 'BAZ:U0'
         stack: [4x1 struct]

??? Error using ==> exceptionsCatchNestedCall>baz at 21
HAHAHAH

Error in ==> exceptionsCatchNestedCall at 29
    foo();
```



## Nemerle


```Nemerle
using System;
using System.Console;

namespace NestedExceptions
{
    public class U0 : Exception
    {
        public this() {base()}
    }

    public class U1 : Exception
    {
        public this() {base()}
    }

    module NestedExceptions
    {
        Foo () : void
        {
            mutable call = 0;

            repeat(2) {
                try {
                    Bar(call);
                }
                catch {
                    |e is U0 => WriteLine("Exception U0 caught.")
                }
                finally {
                    call++;
                }
            }
        }

        Bar (call : int) : void
        {
            Baz(call)
        }

        Baz (call : int) : void // throw U0() on first call, U1() on second
        {
            unless (call > 0) throw U0();
            when (call > 0) throw U1();
        }

        Main () : void
        {
            Foo()
        }
    }
}
```

```txt
Exception U0 caught.

Unhandled Exception: NestedExceptions.U1: Exception of type 'NestedExceptions.U1' was thrown.
   at NestedExceptions.NestedExceptions.Baz(Int32 call)
   at NestedExceptions.NestedExceptions.Foo()
   at NestedExceptions.NestedExceptions.Main()
```



## Nim


```nim
type U0 = object of Exception
type U1 = object of Exception

proc baz(i) =
  if i > 0: raise newException(U1, "Some error")
  else: raise newException(U0, "Another error")

proc bar(i) =
  baz(i)

proc foo() =
  for i in 0..1:
    try:
      bar(i)
    except U0:
      echo "Function foo caught exception U0"

foo()
```

```txt
Function foo caught exception U0
Traceback (most recent call last)
exceptionnested.nim(18)  exceptionnested
exceptionnested.nim(14)  foo
exceptionnested.nim(9)   bar
exceptionnested.nim(5)   baz
Error: unhandled exception: Some error [U1]
Error: execution of an external program failed
```


=={{header|Objective-C}}==

```objc
@interface U0 : NSObject { }
@end
@interface U1 : NSObject { }
@end
@implementation U0
@end
@implementation U1
@end

void foo();
void bar(int i);
void baz(int i);

void foo() {
  for (int i = 0; i <= 1; i++) {
    @try {
      bar(i);
    } @catch (U0 *e) {
      NSLog(@"Function foo caught exception U0");
    }
  }
}

void bar(int i) {
  baz(i); // Nest those calls
}

void baz(int i) {
  if (i == 0)
    @throw [U0 new];
  else
    @throw [U1 new];
}


int main (int argc, const char * argv[]) {
  @autoreleasepool {

    foo();

  }
  return 0;
}
```

```txt

2011-06-03 23:11:53.871 Untitled[9968:903] Function foo caught exception U0
2011-06-03 23:11:53.878 Untitled[9968:903] *** Terminating app due to uncaught exception of class 'U1'

```



## OCaml

Exceptions are used everywhere in OCaml, they are easy to write, and they are cheap.

```ocaml
exception U0
exception U1

let baz i =
  raise (if i = 0 then U0 else U1)

let bar i = baz i (* Nest those calls *)

let foo () =
  for i = 0 to 1 do
    try
      bar i
    with U0 ->
      print_endline "Function foo caught exception U0"
  done

let () = foo ()
```

```txt

Function foo caught exception U0
Exception: U1.

```



## Oforth



```Oforth
Exception Class new: U0
Exception Class new: U1

: baz  ifZero: [ "First call" U0 throw ] else: [ "Second call" U1 throw ] ;
: bar  baz ;

: foo
| e |
   try: e [ 0 bar ] when: [ e isKindOf(U0) ifTrue: [ "Catched" .cr ] else: [ e throw ] ]
   try: e [ 1 bar ] when: [ e isKindOf(U0) ifTrue: [ "Catched" .cr ] else: [ e throw ] ]
   "Done" . ;
```


```txt

Catched
[stdin:1:3] U1 : Second call
ok

```



## Oz

Any value can be raised as an exception. In this example, we simply use atoms.

Exceptions are caught by pattern matching.

```oz
declare
  proc {Foo}
     for I in 1..2 do
        try
           {Bar I}
        catch u0 then {System.showInfo "Procedure Foo caught exception u0"}
        end
     end
  end

  proc {Bar I} {Baz I} end

  proc {Baz I}
     if I == 1 then
        raise u0 end
     else
        raise u1 end
     end
  end
in
  {Foo}
```


```txt

Procedure Foo caught exception u0

%****************************************************************
%**
%** Error: unhandled exception
%**
%** u1
%**--------------------------------------------------------------

```



## PARI/GP


```parigp
call = 0;

U0() = error("x = ", 1, " should not happen!");
U1() = error("x = ", 2, " should not happen!");
baz(x) = if(x==1, U0(), x==2, U1());x;
bar() = baz(call++);
foo() = if(!call, iferr(bar(), E, printf("Caught exception, call=%d",call)), bar())
```


Output 1. call to foo():
```txt
Caught exception, call=1
```

Output 2. call to foo():
```txt
  ***   at top-level: foo()
  ***                 ^-----
  ***   in function foo: ...ception, call=%d",call)),bar())
  ***                                                ^------
  ***   in function bar: baz(call++)
  ***                    ^-----------
  ***   in function baz: if(x==1,U0(),x==2,U1());x
  ***                                      ^-------
  ***   in function U1: error("x = ",2," sho
  ***                   ^--------------------
  ***   user error: x = 2 should not happen!

```

Output 3. call to foo():
```txt
3
```




## Pascal

See [[Exceptions/Catch_an_exception_thrown_in_a_nested_call#Delphi | Delphi]]


## Perl

Note: Both exceptions are caught and one is re-raised rather than only one being caught.

```perl
sub foo {
    foreach (0..1) {
        eval { bar($_) };
        if ($@ =~ /U0/) { print "Function foo caught exception U0\n"; }
        else { die; } # propagate the exception
    }
}

sub bar {
    baz(@_); # Nest those calls
}

sub baz {
    my $i = shift;
    die ($i ? "U1" : "U0");
}

foo();
```

```txt

Function foo caught exception U0
U1 at exceptionsnested.pl line 15.
	...propagated at exceptionsnested.pl line 5.

```



## Perl 6

```perl6
sub foo() {
    for 0..1 -> $i {
        bar $i;
        CATCH {
            when /U0/ { say "Function foo caught exception U0" }
        }
    }
}

sub bar($i) { baz $i }

sub baz($i) { die "U$i" }

foo;
```

```txt
Function foo caught exception U0
U1
  in sub baz at catch:12
  in sub bar at catch:10
  in sub foo at catch:4
  in block  at catch:14
```



## Phix

Phix does not have "exception classes" as such, instead you can just throw any string (on it's own) or any integer, optionally
with any (deeply nested) user_data that you like. All exceptions are always caught, however rethrowing is trivial.

As per the discussion for Go, I should say that "bar(); bar();" cannot work - if you catch an exception from the first call,
control resumes within the catch handler, with no way to invoke that second bar(). But a simple loop does the trick.

```Phix
constant U0 = 0,
         U1 = 1

integer count = 0

procedure baz()
    count += 1
    if count=1 then
        throw(U0,{{"any",{{"thing"},"you"}},"like"})
    else
        throw(U1)
    end if
end procedure

procedure bar()
    baz()
end procedure

procedure foo()
    for i=1 to 2 do
        try
            bar()
        catch e
            if e[E_CODE]=U0 then
                ?e[E_USER]
            else
                throw(e)    -- (terminates)
            end if
        end try
        puts(1,"still running...\n")
    end for
    puts(1,"not still running...\n")
end procedure

foo()
```

```txt

{{"any",{{"thing"},"you"}},"like"}
still running...

C:\Program Files (x86)\Phix\test.exw:27 in procedure foo()
unhandled exception
    i = 2
    e = {1,7533630,11,847,"baz","test.exw","C:\\Program Files (x86)\\Phix\\"}
... called from C:\Program Files (x86)\Phix\test.exw:35

Global & Local Variables

--> see C:\Program Files (x86)\Phix\ex.err
Press Enter...

```

Note that, unlike Python, the call stack from foo() to baz() has gone, for good, however e[E_LINE] is 11, indicating that unhandled exception originated from line 11 (ie "throw(U1)"), and if you need any more help than that, you'll have to arrange for it to end up in e[E_USER] manually.


## PicoLisp


```PicoLisp
(de foo ()
   (for Tag '(U0 U1)
      (catch 'U0
         (bar Tag) ) ) )

(de bar (Tag)
   (baz Tag) )

(de baz (Tag)
   (throw Tag) )

(mapc trace '(foo bar baz))
(foo)
```

```txt
 foo :
  bar : U0
   baz : U0
  bar : U1
   baz : U1
[x:13] !? (throw Tag)
U1 -- Tag not found
?                          # Debug prompt
```



## PL/I


```PL/I

/* Exceptions: Catch an exception thrown in a nested call */
test: proc options (main);
                                                   /* 8/1/2011 */
   declare (m, n) fixed initial (2);
   declare (U0, U1) condition;

foo: procedure () returns (fixed);
   on condition(U0) snap begin;
      put list ('Raised condition U0 in function <bar>.'); put skip;
   end;
   m = bar();
   m = bar();
   return (m);
end foo;

bar: procedure () returns (fixed);
   n = n + 1;
   return (baz());
   return (n);
end bar;
baz: procedure () returns (fixed);
   declare first bit(1) static initial ('1'b);
   n = n + 1;
   if first then do; first = '0'b; signal condition(U0); end;
   else signal condition(U1);
   return (n);
end baz;

   m = foo();
end test;

```


DESCRIPTION OF EXECUTION:

```txt

Function FOO is invoked.
FOO invokes BAR.  BAR invoked BAZ.
In BAZ, exception UO is raised, and is handled in FOO,
which outputs a message and a traceback is produced.
Upon return to BAZ, BAZ terminates, and control returns to FOO.
In FOO, BAR is invoked a second time, which in turn invokes BAZ.
This (second) time that BAZ is invoked, the exception U1 is raised.
As this exception is defined in the outer procedure TEST,
a diagnostic and traceback are produced, and execution resumes
in BAZ, returns to BAR, and then to FOO.
Finally, a return is made to TEST and the program terminates.


OUTPUT:

  CONDITION condition was raised
   At offset +000000E0 in procedure with entry FOO
  From offset +0000007C in procedure with entry TEST

Raised condition U0 in function <bar>.
IBM0400I  ONCODE=0500  The CONDITION condition was raised
          by a SIGNAL statement and the condition U1 was signaled.
   At offset +0000010D in procedure with entry FOO

```



## Python

There is no extra syntax to add to functions and/or methods such as ''bar'',
to say what exceptions they may raise or pass through them:

```python
class U0(Exception): pass
class U1(Exception): pass

def foo():
    for i in range(2):
        try:
            bar(i)
        except U0:
            print("Function foo caught exception U0")

def bar(i):
    baz(i) # Nest those calls

def baz(i):
    raise U1 if i else U0

foo()
```

```txt

Function foo caught exception U0

Traceback (most recent call last):
  File "C:/Paddy3118/Exceptions_Through_Nested_Calls.py", line 17, in <module>
    foo()
  File "C:/Paddy3118/Exceptions_Through_Nested_Calls.py", line 7, in foo
    bar(i)
  File "C:/Paddy3118/Exceptions_Through_Nested_Calls.py", line 12, in bar
    baz(i) # Nest those calls
  File "C:/Paddy3118/Exceptions_Through_Nested_Calls.py", line 15, in baz
    raise U1 if i else U0
U1

```

The first line of the output is generated from catching the U0 exception
in function foo.

Uncaught exceptions give information showing where the exception originated
through the nested function calls together with the name of the
uncaught exception, (U1) to stderr, then quit the running program.


## R

The counter for the number of calls to baz is kept in
the global environment for simplicity, but you could hide it
in your own environment.
See ?new.env and ?get.

```r

number_of_calls_to_baz <- 0

foo <- function()
{
   for(i in 1:2) tryCatch(bar())
}

bar <- function() baz()

baz <- function()
{
   e <- simpleError(ifelse(number_of_calls_to_baz > 0, "U1", "U0"))
   assign("number_of_calls_to_baz", number_of_calls_to_baz + 1, envir=globalenv())
   stop(e)
}

```

Example Usage:

```r

foo()   # Error: U0
traceback()

```

```txt

6: stop(e) at file.r#11
5: baz()
4: bar()
3: tryCatchList(expr, classes, parentenv, handlers)
2: tryCatch(bar()) at file.r#4
1: foo()

```



## Racket


```racket

#lang racket

(define-struct (exn:U0 exn) ())
(define-struct (exn:U1 exn) ())

(define (foo)
  (for ([i 2])
    (with-handlers ([exn:U0? (λ(_) (displayln "Function foo caught exception U0"))])
      (bar i))))

(define (bar i)
  (baz i))

(define (baz i)
  (if (= i 0)
      (raise (make-exn:U0 "failed 0" (current-continuation-marks)))
      (raise (make-exn:U1 "failed 1" (current-continuation-marks)))))

(foo)

```

```racket

Function foo caught exception U0
. . failed 1

```



## REXX

While the REXX language doesn't have a ''throw'' capability ''pe se'', it does have the ability to catch exceptions (by label).

This type of exception handling (in REXX) has its limitation
(the label is known global to the program, but not to external subroutines).

```rexx
/*REXX program  creates  two exceptions and demonstrates how to  handle  (catch)  them. */
call foo                                         /*invoke the  FOO  function  (below).  */
say 'The REXX mainline program has completed.'   /*indicate that Elroy was here.        */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
foo:   call bar;   call bar                      /*invoke  BAR  function  twice.        */
       return 0                                  /*return a zero to the invoker.        */
                                                 /*the 1st  U0  in REXX program is used.*/
U0:    say 'exception U0 caught in FOO'          /*handle the  U0  exception.           */
       return -2                                 /*return to the invoker.               */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bar:   call baz                                  /*have BAR function invoke BAZ function*/
       return 0                                  /*return a zero to the invoker.        */
/*──────────────────────────────────────────────────────────────────────────────────────*/
baz:   if symbol('BAZ#')=='LIT'  then baz#=0     /*initialize the first BAZ invocation #*/
       baz# = baz#+1                             /*bump the BAZ invocation number by 1. */
       if baz#==1  then signal U0                /*if first  invocation, then raise  U0 */
       if baz#==2  then signal U1                /* " second      "        "    "    U1 */
       return 0                                  /*return a   0  (zero)  to the invoker.*/
                                                 /* [↓]  this  U0 subroutine is ignored.*/
U0:    return -1                                 /*handle exception if not caught.      */
U1:    return -1                                 /*   "       "      "  "     "         */
```

'''output'''

```txt

exception U0 caught in FOO
The REXX mainline program has completed.

```



## Ruby


```ruby
def foo
  2.times do |i|
    begin
      bar(i)
    rescue U0
      $stderr.puts "captured exception U0"
    end
  end
end

def bar(i)
  baz(i)
end

def baz(i)
  raise i == 0 ? U0 : U1
end

class U0 < StandardError; end

class U1 < StandardError; end

foo
```

The first call to foo causes the U0 exception. It gets rescued.
The second call results in a U1 exception which is not rescued,
so the program dumps a stacktrace and exits.
```txt
C:>ruby exception.rb
captured exception U0
exception.rb:16:in `baz': U1 (U1)
        from exception.rb:12:in `bar'
        from exception.rb:4:in `block in foo'
        from exception.rb:2:in `times'
        from exception.rb:2:in `foo'
        from exception.rb:23:in `<main>'
```



## Rust

Rust has panics, which are similar to exceptions in that they default to unwinding the stack and the unwinding can be caught. However, panics can be configured to simply abort the program and thus cannot be guaranteed to be catchable. Panics should only be used for situations which are truly unexpected. It is prefered to return an Option or Result when a function can fail. <code>Result<T, U></code> is an enum (or sum type) with variants <code>Ok(T)</code> and <code>Err(U)</code>, representing a success value or failure value. <code>main</code> can return a Result, in which case the debug representation of the error will be shown.

```Rust
#[derive(Debug)]
enum U {
    U0(i32),
    U1(String),
}

fn baz(i: u8) -> Result<(), U> {
    match i {
        0 => Err(U::U0(42)),
        1 => Err(U::U1("This will be returned from main".into())),
        _ => Ok(()),
    }
}

fn bar(i: u8) -> Result<(), U> {
    baz(i)
}

fn foo() -> Result<(), U> {
    for i in 0..2 {
        match bar(i) {
            Ok(()) => {},
            Err(U::U0(n)) => eprintln!("Caught U0 in foo: {}", n),
            Err(e) => return Err(e),
        }
    }
    Ok(())
}

fn main() -> Result<(), U> {
    foo()
}
```

```txt
Caught U0 in foo: 42
Error: U1("This will be returned from main")
```



## Scala

```Scala
object ExceptionsTest extends App {
  class U0 extends Exception
  class U1 extends Exception

  def foo {
    for (i <- 0 to 1)
      try {
        bar(i)
      } catch { case e: U0 => println("Function foo caught exception U0") }
  }

  def bar(i: Int) {
    def baz(i: Int) = { if (i == 0) throw new U0 else throw new U1 }

    baz(i) // Nest those calls
  }

  foo
}

```

Exception U0 is caught, exception U1 is caught and re-thrown.
Program execution is terminated as the U1 exception is not caught
when thrown the second time.


## Seed7

When an [http://seed7.sourceforge.net/manual/errors.htm#Exceptions exception]
is not [http://seed7.sourceforge.net/manual/errors.htm#Handlers handled]
the program is terminated and a [http://seed7.sourceforge.net/manual/errors.htm#Stack_trace stack trace] is written.

```seed7
$ include "seed7_05.s7i";

const EXCEPTION: U0 is enumlit;
const EXCEPTION: U1 is enumlit;

const proc: baz (in integer: num) is func
  begin
    if num = 1 then
      raise U0;
    else
      raise U1;
    end if;
  end func;

const proc: bar (in integer: num) is func
  begin
    baz(num);
  end func;

const proc: foo is func
  local
    var integer: num is 0;
  begin
    for num range 1 to 2 do
      block
        bar(num);
      exception
        catch U0: writeln("U0 catched");
      end block;
    end for;
  end func;

const proc: main is func
  begin
    foo;
  end func;
```


```txt

U0 catched

*** Uncaught EXCEPTION U1 raised with
{raise U1 }

Stack:
in raise (ref EXCEPTION: anException) at /media/disk2_460GiB/home/tm/seed7_5/prg/seed7_05.s7i(322)
in baz (val integer: num) at rosetta/catchAnExceptionThrownInANestedCall.sd7(11)
in bar (val integer: num) at rosetta/catchAnExceptionThrownInANestedCall.sd7(17)
in foo at rosetta/catchAnExceptionThrownInANestedCall.sd7(26)
in main at rosetta/catchAnExceptionThrownInANestedCall.sd7(35)

```



## Sidef


```ruby
func baz(i) { die "U#{i}" };
func bar(i) { baz(i)      };

func foo {
    [0, 1].each { |i|
        try   { bar(i) }
        catch { |_, msg|
            msg ~~ /^U0/ ? say "Function foo() caught exception U0"
                         : die msg;       # re-raise the exception
        };
    }
}

foo();
```

```txt

Function foo() caught exception U0
U1 at test.sf line 1. at test.sf line 9.

```



## Smalltalk

```smalltalk

Exception subclass: #U0.
Exception subclass: #U1.

Object subclass: Foo [

    bazCount := 0.

    foo
        [2 timesRepeat:
            [ "==>" [self bar] "<=="
                on: U0
                do:
                    [:sig |
                    'Call to bar was aborted by exception U0' printNl.
                    sig return]]]

    bar
        [self baz]

    baz
        [bazCount := bazCount + 1.
        bazCount = 1 ifTrue: [U0 new signal].
        bazCount = 2 ifTrue: [U1 new signal].
        "Thirds time's a charm..."]
]

```


Running the code:

```Smalltalk

st> Foo new foo
'Call to bar was aborted by exception U0'
Object: Foo new "<-0x4c9a7960>" error: An exception has occurred
U1(Exception)>>signal (ExcHandling.st:254)
Foo>>baz (catch_exception.st:32)
Foo>>bar (catch_exception.st:27)
optimized [] in Foo>>foo (catch_exception.st:19)
BlockClosure>>on:do: (BlkClosure.st:193)
Foo>>foo (catch_exception.st:20)
UndefinedObject>>executeStatements (a String:1)
nil

```


Explanation:<br/>
Inside the foo method, inside the 2 timesRepeat: block, there is a small
block <code>[self bar]</code> which simply calls bar. This block is sent
the <code>#on:do:</code> message, which will evaluate the block and catch
any mentioned exception. First time this block is evaluated, it results in
a U0 exception, which we catch and handle by printing a message and
returning <code>nil</code> in place of whatever the block would have
returned. The second time the block is evaluated, it results in a U1
exception, which we do ''not'' catch, so it passes to the default handler
which prints a trace and exits. The second line of the trace
<code>U1(Exception)>>signal</code> shows that this was a U1 exception.

Exception handling in Smalltalk is exceptional, and the exception handler
(the following do: block) can do quite some cool stuff, like retrying the
block, retrying with a different block, and even resuming evaluation at the
point where the exception was raised (baz in this example) having <code>U0
new signal</code> return some value.


## Swift

```swift
enum MyException : ErrorType {
  case U0
  case U1
}

func foo() throws {
  for i in 0 ... 1 {
    do {
      try bar(i)
    } catch MyException.U0 {
      print("Function foo caught exception U0")
    }
  }
}

func bar(i: Int) throws {
  try baz(i) // Nest those calls
}

func baz(i: Int) throws {
  if i == 0 {
    throw MyException.U0
  } else {
    throw MyException.U1
  }
}

try foo()
```

```txt

Function foo caught exception U0
fatal error: Error raised at top level: MyApp.MyException.U1: file /Library/Caches/com.apple.xbs/Sources/swiftlang/swiftlang-700.0.45/src/swift/stdlib/public/core/ErrorType.swift, line 47

```



## Tcl

Note: Both exceptions are caught and one is re-raised rather than only one being caught.

```tcl
package require Tcl 8.5

proc foo {} {
    set code [catch {bar} ex options]
    if {$code == 1} {
        switch -exact -- $ex {
            U0      {puts "caught exception U0"}
            default {return -options $options $ex ;# re-raise exception}
        }
    }
}

proc bar {} {baz}

# create an alias to pass the initial exception U0 to the baz proc
interp alias {} baz {} _baz U0

proc _baz {exception} {
    # re-set the alias so subsequent invocations will use exception U1
    interp alias {} baz {} _baz U1
    # throw
    return -code error $exception
}

foo
foo
```

```txt
$ tclsh85 exceptions.tcl
caught exception U0
U1
    while executing
"baz"
    (procedure "bar" line 1)
    invoked from within
"bar"
    (procedure "foo" line 2)
    invoked from within
"foo"
    (file "exceptions.tcl" line 26)
```



## TXR



```txr
@(defex u0)
@(defex u1)
@(define baz (x))
@  (cases)
@    (bind x "0")
@    (throw u0 "text0")
@  (or)
@    (bind x "1")
@    (throw u1 "text1")
@  (end)
@(end)
@(define bar (x))
@  (baz x)
@(end)
@(define foo ())
@  (next :list @'("0" "1"))
@  (collect)
@num
@    (try)
@      (bar num)
@    (catch u0 (arg))
@      (output)
caught u0: @arg
@      (end)
@    (end)
@  (end)
@(end)
@(foo)
```


```txt
$ txr except.txr
caught u0: text0
txr: unhandled exception of type u1:
txr: text1
txr: during evaluation at exceptions.txr:9 of form (throw u1 "text1")
$ echo $?
1

```



## Ursala

Foo calls bar, and bar calls baz. Normal termination of bar is bypassed
if baz raises an exception.
The exception is caught or not by foo.

```Ursala
#import std

baz =

~&?(
   ~&h?(
      :/'baz succeeded with this input:',
      <'baz threw a user-defined empty string exception','U1'>!%),
   <'baz threw a user-defined empty file exception','U0'>!%)

bar = :/'bar received this result from normal termination of baz:'+ baz

#executable&

foo =

guard(
   :/'foo received this result from normal termination of bar:'+ bar,
   'U0'?=z/~& :/'foo caught an exception with this error message:')
```

Note that the definition of bar includes no conditional (?) or exception
handling operators, and is written without regard for any exceptions.
Here is an example bash session:

```txt

$ echo "valid input" | foo
foo received this result from normal termination of bar:
bar received this result from normal termination of baz:
baz succeeded with this input:
valid input
$ foo < /dev/null
baz threw a user-defined empty file exception
U0
$ echo "" | foo
foo caught an exception with this error message:
baz threw a user-defined empty string exception
U1
```



## Visual Basic .NET



```vbnet
Class U0
    Inherits Exception
End Class

Class U1
    Inherits Exception
End Class

Module Program
    Sub Main()
        Foo()
    End Sub

    Sub Foo()
        Try
            Bar()
            Bar()
        Catch ex As U0
            Console.WriteLine(ex.GetType.Name & " caught.")
        End Try
    End Sub

    Sub Bar()
        Baz()
    End Sub

    Sub Baz()
        ' Static local variable is persisted between calls of the method and is initialized only once.
        Static firstCall As Boolean = True
        If firstCall Then
            firstCall = False
            Throw New U0()
        Else
            Throw New U1()
        End If
    End Sub
End Module
```


Control passes to the Catch block after U0 is thrown, and so the second call to Bar() is not made.

```txt
U0 caught.
```


To prevent this, a loop can be used to run the entire Try statement twice:


```vbnet
    Sub Foo()
        For i = 1 To 2
            Try
                Bar()
            Catch ex As U0
                Console.WriteLine(ex.GetType().Name & " caught.")
            End Try
        Next
    End Sub
```


```txt
U0 caught.

Unhandled Exception: U1: Exception of type 'U1' was thrown.
   at Program.Baz() in Program.vb:line 34
   at Program.Bar() in Program.vb:line 25
   at Program.Foo() in Program.vb:line 17
   at Program.Main() in Program.vb:line 11
```



## zkl


```zkl
class U0(Exception.Exception){fcn init{Exception.init("U0")}}
class U1(Exception.Exception){fcn init{Exception.init("U1")}}

fcn foo{try{bar(U0)}catch(U0){} bar(U1)}
fcn bar(e){baz(e)}
fcn baz(e){throw(e)}
foo()
```

```txt

Stack trace for VM#1 ():
   Cmd.baz addr:2  args(1) reg(0)
   Cmd.bar addr:6  args(1) reg(0) R
   Cmd.foo addr:34  args(0) reg(0) R
   Cmd.__constructor@foo addr:5  args(0) reg(0) R
   startup.__constructor addr:2242  args(0) reg(1) ER
   startup.__constructor addr:2178  args(0) reg(22)
Exception thrown: U1(An Exception)

```

foo catches exception U0 and ignores it.
It calls bar with the exception to throw. bar in turn calls baz
with that exception, which it throws.
A stack trace is printed when an uncaught exception bubbles up to the VM
(which handles all catchable exceptions).
