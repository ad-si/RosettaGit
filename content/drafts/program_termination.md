+++
title = "Program termination"
description = ""
date = 2019-04-12T13:20:47Z
aliases = []
[extra]
id = 2833
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Simple]]


;Task:
Show the syntax for a complete stoppage of a program inside a   [[Conditional Structures|conditional]].

This includes all [[threads]]/[[processes]] which are part of your program.

Explain the cleanup (or lack thereof) caused by the termination (allocated memory, database connections, open files, object finalizers/destructors, run-on-exit hooks, etc.).

Unless otherwise described, no special cleanup outside that provided by the operating system is provided.





## Ada

Ada programs execute in one or more tasks. All tasks created during the execution of a program depend in a hierarchical manner on the task that create them, except for the environment task which executes the "main" procedure for the program.
Each task will abort (terminate abnormally) if the task upon which it depends is aborted.
This approach to task termination is not recommended because it does not allow tasks to terminate in a known state.

However, this Rosetta Code task requires a simple stoppage of the program including all tasks. The simple way to achieve this is to abort the environment task.

```ada
with Ada.Task_Identification;  use Ada.Task_Identification;

procedure Main is
   -- Create as many task objects as your program needs
begin
   -- whatever logic is required in your Main procedure
   if some_condition then
      Abort_Task (Current_Task);
   end if;
end Main;
```

Aborting a task with Abort_Task is equivalent to ''abort'' statement,
which is not used here because the environment task object is anonymous.
The semantics of ''abort'' is as follows:

* Abort is deferred until certain unbreakable actions are accomplished. These are protected actions on shared objects, initialization, assignment, and finalization of controlled objects, waiting for dependent tasks to be aborted;
* Local objects of the task are finalized;
* The tasks dependent on the aborted task are aborted.
* The state of external files will depend on the [[OS]]

The above is a preemptive way to abort tasks, which is not recommended to use, unless you firmly know what you are doing. A standard approach to such termination is either (or a combination of):

* to provide an entry in each task created by the environment task which, when called by the task upon which it depends, causes the called task to terminate in a known state;
* to provide "terminate" alternative open in each of such tasks.

In both cases the task objects are made local or otherwise destroyed upon completion of the main task. Note that destruction of a task always waits for its termination. If the task refuses to terminate it deadlocks.

With the first approach:

```ada
procedure Main is
   -- Create as many task objects as your program needs
begin
   -- whatever logic is required in your Main procedure
   if some_condition then
      -- for each task created by the Main procedure
      The_task.Stop;
      -- end the Main procedure
      return;  -- actually, this is not needed
   end if;
end Main;
```

A task might look like:

```ada
task body Some_Task is
begin
   loop
      select
         -- Some alternatives
         ...
      or accept Stop do
            -- Some cleanup while holding the caller is here
         end Stop;
            -- A cleanup asynchronous to the caller is here
         exit; -- We are through
      end select
   end loop;
end Some_Task;
```

With the second approach one simply returns from Main and all tasks are terminated by selecting the terminate alternative. Such tasks might look like:

```ada
task body Some_Task is
begin
   loop
      select
         -- Some alternatives
         ...
      or terminate; -- We are through
      end select
   end loop;
end Some_Task;
```



## Aime


```aime
void
f1(integer a)
{
    if (a) {
	exit(1);
    }
}

integer
main(void)
{
    f1(3);

    return 0;
}
```



## ALGOL 68

The label "stop" appears at the start of the <i>standard-postlude</i> and can be invoked to terminate any program.

```algol68
IF problem = 1 THEN
   stop
FI
```

The <i>standard-postlude</i> closes any opens files and basically wraps up execution.


## ALGOL W

The program can be stopped by a false assertion.

```algolw
if anErrorOccured then assert( false );
```



## AppleScript

AppleScript doesn't include a built-in command for immediate script termination. The <code>return</code> command can be used to exit the main run handler, but will not force termination of the entire script from another handler/function.

```AppleScript
on run
	If problem then return
end run
```

It's possible to simulate an immediate program termination from within any handler by throwing a user error, but this will force a modal dialog (AppleScript Error) to appear announcing the error. Such a dialog cannot be bypassed, and the script immediately quits upon user acknowledgement.

```AppleScript
on run
	f()
	display dialog "This message will never be displayed."
end run

on f()
	error
end f
```

Memory is automatically managed and reclaimed by AppleScript.

## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program ending.s   */

/* Constantes               */
.equ EXIT,   1                          @ Linux syscall

/* Initialized data */
.data

/*  code section */
.text
.global main
main:                                   @ entry of program
    push {fp,lr}                        @ saves registers

OK:
    @ end program OK
    mov r0, #0                          @ return code
    b 100f
NONOK:
    @ if error detected end program no ok
    mov r0, #1                          @ return code
100:                                    @ standard end of the program
    pop {fp,lr}                         @restaur  registers
    mov r7, #EXIT                       @ request to exit program
    swi 0                               @ perform the system call Linux


```



## AutoHotkey


```AutoHotkey
If (problem)
  ExitApp
```



## AutoIt

Then Endif is entirely unnecessary, but it is good form.

```AutoIt
If problem Then
  Exit
Endif
```



## AWK

An "exit"-statement aborts the current script,
optionally returning a status-code:

```awk
if(problem)exit 1
```


Before exiting, the END-block(s) are processed.

An "exit" in an END-block causes an immediate exit:

```awk
# usage:  awk -f exittest.awk input.txt
BEGIN  { print "# Exit-Test" }

#/s.*t/ { print "!", NR, $0; next }           #1: List all matches
 /s.*t/ { print "!", NR, $0; problem=1; exit} #2: Abort after first match
        { print " ", NR, $0}

END     { if(problem) {print "!! Problem !!"; exit 2} }
END     { print "# Lines read:", NR }

```

To compare, un/comment one of the lines #1 or #2:
{{in}}

```txt

This is file input.txt
you can use it
to provide your
program with some data
to process.

```


{{out}} with line #1 "next" active:

```txt

# Exit-Test
! 1 This is file input.txt
! 2 you can use it
  3 to provide your
! 4 program with some data
  5 to process.
# Lines read: 5

```


{{out}} with line #2 "exit" active:

```txt

# Exit-Test
! 1 This is file input.txt
!! Problem !!
```



## Axe

The following example will force exit from any number of nested calls and loops:

```axe
Returnʳ
```



## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
if problem = 1 then
   end
end if
```


=== {{header|Applesoft BASIC}} ===

```Applesoft BASIC>10  IF 1 THEN  STOP</lang


=
## BaCon
=
No special cleanup routines are compiled in by default, but any functions registered via POSIX ''atexit'' and/or ''on_exit'' will be honoured. END can include a status code to be passed back to the invoking process.

```freebasic>IF TRUE THEN END 42</lang


=
## BASIC256
=
if not more then end

==={{header|IS-BASIC}}===
<lang IS-BASIC> 100 IF CONDITION THEN STOP
```


=
## Locomotive Basic
=

```locobasic>10 IF 1 THEN END</lang


=== {{header|ZX Spectrum Basic}} ===
The ZX Spectrum has a STOP command, rather than an END command:

```zxbasic
10 LET a = 1: LET b = 1
20 IF a = b THEN GO TO 9995
9995 STOP
```



## Batch File


```dos>if condition exit</lang

In Windows batch files this doesn't need to exit the program but instead can also just exit a subroutine. <code>exit /b</code> can also be used alternatively if a return value if desired.


## BBC BASIC


```bbcbasic
      IF condition% THEN QUIT
```

Only QUIT fully terminates the program.  END and STOP stop execution and return control to the immediate-mode prompt; END closes all open files, but STOP does not.


## Befunge


```bbcbasic
_@
```

The @ instruction ends the program. Some interpreters revert changes to the code (made by p) while others do not.


## Bracmat

From infinite loops (such as the read-eval-print loop that Bracmat enters when run in interactive mode) the program can only exit cleanly by evaluating a closing parenthesis followed by an affirmative <i>y</i>, <i>Y</i>, <i>j</i> or <i>J</i>. Thus, at the Bracmat prompt you type the closing parenthesis, a <i>y</i> and then press enter. From within Bracmat code, you force an exit by evaluating <code>get'(")y",MEM)</code>. If Bracmat is linked to another program (e.g. as a Java Native Interface library), it is better to turn off the possibility to exit Bracmat, as that normally would cause the main program to crash. There is a C preprocessor macro that disables exiting.

If there are no infinite loops in Bracmat code, the Bracmat program will ultimately terminate in a natural way.


## C


```cpp
#include <iostream>
/* More "natural" way of ending the program: finish all work and return
   from main() */
int main(int argc, char **argv)
{
  /* work work work */
     ...
  return 0;  /* the return value is the exit code. see below */
}

if(problem){
  exit(exit_code);
  /* On unix, exit code 0 indicates success, but other OSes may follow
     different conventions.  It may be more portable to use symbols
     EXIT_SUCCESS and EXIT_FAILURE; it all depends on what meaning
     of codes are agreed upon.
  */
}
```

The <code>atexit()</code> function (also in <tt>stdlib.h</tt>) can be used to register functions to be run when the program exits. Registered functions will be called in the reverse order in which they were registered.

```cpp
#include <iostream>

if(problem){
  abort();
}
```

Unlike <tt>exit()</tt>, <tt>abort()</tt> will not do any cleanup other than the normal OS one. Also, it may cause other actions like producing a core dump or starting a debugger.

To end not just the current process, but all processes in the same group, do
```C
exit_group();
```



## C++

There are several ways to terminate a program. The following is mostly the same as in C:

```cpp
#include <cstdlib>

void problem_occured()
{
  std::exit(EXIT_FAILURE);
}
```

The argument is the return value passed to the operating system. Returning 0 or the EXIT_SUCCESS signals successful termination to the calling process, EXIT_FAILURE signals failure. The meaning of any other value is implementation defined.

On calling <tt>std::exit</tt>, all functions registered with std::atexit are called, and the destructors of all objects at namespace scope, as well as of all static objects already constructed, are called. However the destructors of automatic objects (i.e. local variables) are ''not'' called (and of course, objects allocated with new will not be destructed as well, except if one of the called destructors destroys them). Due to this inconsistency calling <tt>std::exit</tt> is often not a good idea.

```cpp
#include <cstdlib>

void problem_occured()
{
  std::abort();
}
```

Unlike <tt>std::exit</tt>, <tt>std::abort</tt> will not do any cleanup other than the normal OS one. Also, it may cause other actions like producing a core dump or starting a debugger.

```cpp
#include <exception>

void problem_occured()
{
  std::terminate();
}
```

The function <tt>std::terminate</tt> is what is automatically called when certain exception related failures happen. However it also can be called directly. By default it just calls abort, but unlike abort, its behaviour can be overridden with <tt>std::set_terminate</tt> (but it still must terminate the program in one way or anouther). Thererfore the amount of cleanup it does depends on whether it was overridden, and what the overridden function does.

Note that returning a value from main is mostly equivalent to calling <tt>std::exit</tt> with the returned value, except that automatic variables are correctly destructed. If one wants to return from an inner function, while still doing complete cleanup, a solution is to throw an exception caught in main (this will call the destructors of non-main local variables during stack unwinding), and to then return normally from main (which will destruct all automatic objects in main, and then do the cleanup like <tt>std::exit</tt>.

## C#

```c#
if (problem)
{
   Environment.Exit(1);
}
```



## Clojure

{{trans|Java}}
The call <tt>System.exit</tt> does not finalize any objects by default. This default is to keep the program thread-safe. From the javadocs for the method to change this default: "may result in finalizers being called on live objects while other threads are concurrently manipulating those objects, resulting in erratic behavior or deadlock."

```clojure
(if problem
   (. System exit integerErrorCode))
   ;conventionally, error code 0 is the code for "OK",
   ; while anything else is an actual problem
   ;optionally: (-> Runtime (. getRuntime) (. exit integerErrorCode))
}
```

You can use <code>(-> Runtime (. getRuntime) (. addShutdownHook myThread))</code> to add threads which represent actions to be run when the program exits.

This one does not perform cleanup:

```clojure
(if problem
   (-> Runtime (. getRuntime) (. halt integerErrorCode)))
   ; conventionally, error code 0 is the code for "OK",
   ; while anything else is an actual problem

```



## COBOL

Terminating the program will cause all open files to be closed and control to be returned to the operating system. There are 2 ways to do this: <code>STOP RUN</code> and <code>GOBACK</code>.<br/>

```cobol
IF problem
    STOP RUN
END-IF
```


<code>GOBACK</code> was added in COBOL 2002, and will terminate the program if it is reached in the '''main''' program.

```cobol
IF problem
    GOBACK
END-IF
```


The ability to return a return code to the operating system is available for both of these statements as an extension in some compilers.


## Common Lisp

Many Common Lisp implementations provide a function named <code>quit</code> or sometimes <code>exit</code> which will exit the Lisp system; its parameters and the package it is in vary, but here are some implementations' versions, with a Unix-style exit status argument, and a fallback:

```lisp
(defun terminate (status)
  #+sbcl     (           sb-ext:quit      :unix-status status)    ; SBCL
  #+ccl      (              ccl:quit      status)                 ; Clozure CL
  #+clisp    (              ext:quit      status)                 ; GNU CLISP
  #+cmu      (             unix:unix-exit status)                 ; CMUCL
  #+ecl      (              ext:quit      status)                 ; ECL
  #+abcl     (              ext:quit      :status status)         ; Armed Bear CL
  #+allegro  (             excl:exit      status :quiet t)        ; Allegro CL
  #+gcl      (common-lisp-user::bye       status)                 ; GCL
  #+ecl      (              ext:quit      status)                 ; ECL
  (cl-user::quit))           ; Many implementations put QUIT in the sandbox CL-USER package.
```

There is no standard form because the Common Lisp standard does not assume the presence of an operating system outside of the Lisp environment to exit to.

What cleanup will be performed varies. Some implementations have at-exit hooks. SBCL will unwind the stack and execute any <code>unwind-protect</code>s (like <code>finally</code> in other languages) it encounters, unless <code>:recklessly-p t</code> is specified.


## Computer/zero Assembly

A <tt>STP</tt> instruction causes program execution to halt. The program counter is left pointing to the word <i>after</i> the <tt>STP</tt>, so this instruction can be used to stop the computer while the user enters some data before resuming execution.


## D

The usual C functions are available, plus assert Errors and user defined Errors, and Exceptions.

```d
import core.stdc.stdio, core.stdc.stdlib;

extern(C) void foo() nothrow {
    "foo at exit".puts;
}

extern(C) void bar() nothrow {
    "bar at exit".puts;
}

extern(C) void spam() nothrow {
    "spam at exit".puts;
}

int baz(in int x) pure nothrow
in {
    assert(x != 0);
} body {
    if (x < 0)
        return 10;
    if (x > 0)
        return 20;

    // x can't be 0.

    // In release mode this becomes a halt, and it's sometimes
    // necessary. If you remove this the compiler gives:
    // Error: function test.notInfinite no return exp;
    //    or assert(0); at end of function
    assert(false);
}

// This generates an error, that is not meant to be caught.
// Objects are not guaranteed to be finalized.
int empty() pure nothrow {
    throw new Error(null);
}

static ~this() {
    // This module destructor is never called if
    // the program calls the exit function.
    import std.stdio;
    "Never called".writeln;
}

void main() {
    atexit(&foo);
    atexit(&bar);
    atexit(&spam);

    //abort(); // Also this is allowed. Will not call foo, bar, spam.
    exit(0);
}
```

{{out}}

```txt
spam at exit
bar at exit
foo at exit
```





###  Simple exit with D Runtime cleanup


```d
import core.runtime, std.c.stdlib;

static ~this() {
    // This module destructor is called if
    // the program calls the dexit function.
    import std.stdio;
    "Called on dexit".writeln;
}

void dexit(int rc) {
    // Calling dexit() should have the same effect with regard to cleanup as as reaching the end of the main program.
    Runtime.terminate();
    exit(rc);
}

int main() {
    if(true) {
        dexit(0);
    }
    return 0;
}
```

{{out}}

```txt
Called on dexit

```


=={{header|Delphi}}/{{header|Pascal}}==

```Delphi>System.Halt;</lang

or

```Delphi
System.Halt(1); // Optional exit code
```



## E

Exit indicating successful completion:

```e
if (true) {
    interp.exitAtTop()
}
```

Exit indicating some problem:

```e
if (true) {
    interp.exitAtTop("because the task said so")
}
```

Both of these have the same effect with regard to cleanup as as reaching the end of the main program. [To do: Find out what effect that is.]


## EDSAC order code

The 'stop' order is <code>Z</code>, so <code>ZF</code> will halt the machine completely. EDSAC programmers used also to use an infinite loop as a 'dynamic stop': since <code>F</code> is the order for an unconditional jump, <code>Fn@</code> where <math>n</math> = the current address - <i>θ</i> is equivalent to <code>here: goto here</code>.


## Elixir


```elixir
if rcode != :ok, do: System.halt(1)
```


```elixir
exit(:normal)
# or
exit(:shutdown)
```



## Emacs Lisp


```Lisp
(if something
    (kill-emacs))
```


Functions in <code>kill-emacs-hook</code> are called.  (Except prior to Emacs 24 that hook was not run when in <code>-batch</code> mode.)  The underlying C library <code>atexit()</code> handlers are called.


## Erlang

;Polite:

```erlang
% Implemented by Arjun Sunel
if problem ->
	exit(1).
```


;As soon as possible:

```erlang
% Implemented by Arjun Sunel
if problem ->
	halt().
```



## Forth


```forth
debug @
if   QUIT  \ quit back to the interpreter
else BYE   \ exit forth environment completely (e.g. end of a Forth shell script)
then
```



## Fortran

In Fortran <tt>STOP</tt> stops the execution of the ''main'' process and its "children" (tested with OpenMP; if using POSIX threads, I think the <tt>stop</tt> behaves almost like C <tt>exit</tt>). Allocated memory or any other resource except opened file (which are closed) is not cleaned up.

```fortran
IF (condition) STOP [message]
! message is optional and is a character string.
! If present, the message is output to the standard output device.
```



## FreeBASIC

In FreeBASIC, the 'End' statement exits the program immediately and optionally returns a value to the operating system.
Typically, a return value of 0 (the default) indicates there was no error and some other value (such as 1) indicates termination was due to an error. Any open files will be closed automatically.

However, the use of this statement does not necessarily produce a clean exit. As FreeBASIC does not unwind the stack, local variables will not have their destructors (if any) called automatically - though global variables will.

Because of this, it is generally better if some way can be found of returning to the module-level code and then jumping to the end thereof to ensure that all variables have their destructors called as they fall out of scope.

Here is a very simple example of the use of the 'End' statement:

```freebasic
'FB 1.05.0 Win64 'endprog.bas'

Dim isError As Boolean = True
If isError Then
  End 1
End If

' The following code won't be executed
Print
Print "Press any key to quit"
Sleep
```


After running the program, the Windows console will look something like this:
{{out}}

```txt

c:\FreeBasic>endprog

c:\FreeBasic>echo exit code is %errorlevel%
exit code is 1

```


=={{header|F Sharp|F#}}==

```fsharp
open System

if condition then
    Environment.Exit 1
```



## Gambas

In a GUI environment

```gambas
Public Sub Form_Open()
Dim siCount As Short

Do
  If siCount > 1000 Then Break
  Inc siCount
Loop

Me.Close

End
```


In a CLI environment

```gambas
Public Sub Main()
Dim siCount As Short

Do
  If siCount > 1000 Then Break
  Inc siCount
Loop

Quit

End
```



## Gema

Terminate with an error message and a non-zero status code if "Star Trek" is found in the input stream.

```gema
Star Trek=@err{found a Star Trek reference\n}@abort
```



## Gnuplot


```Gnuplot
problem=1
if (problem) {
  exit gnuplot
}
```


The Gnuplot manual under "exit" notes that "any open output files may not be completed cleanly".  (Does that mean output buffers not flushed?)


## Go

Operating system resources such as memory and file handles are generally released on exit automatically, but this is not specified in the language definition.
Proceses started with os.StartProcess or exec.Run are not automatically terminated by any of the techniques below and will continue to run after the main program terminates.

### Return statement

Basically, a return statement executed from anywhere in main() terminates the program.

```go
func main() {
    if problem {
        return
    }
}
```

Deferred functions are run when the enclosing function returns, so in the example below, function <tt>paperwork</tt> is run.
This is the idiomatic mechanism for doing any kind of necessary cleanup.

Other goroutines are terminated unceremoniously when <tt>main</tt> returns.
Below, <tt>main</tt> returns without waiting for <tt>pcj</tt> to complete.

The tantalizingly named <tt>SetFinalizer</tt> mechanism is also not invoked on program termination.  It is designed for resource reclamation in long-running processes, not for program termination.

Returns from functions other than main do not cause program termination.  In particular, return from a goroutine simply terminates that one goroutine, and not the entire program.

```go
package main

import (
    "fmt"
    "runtime"
    "time"
)

const problem = true

func main() {
    fmt.Println("main program start")

    // this will get run on exit
    defer paperwork()

    // this will not run to completion
    go pcj()

    // this will not get run on exit
    rec := &requiresExternalCleanup{"external object"}
    runtime.SetFinalizer(rec, cleanup)

    if problem {
        fmt.Println("main program returning")
        return
    }
}

func paperwork() {
    fmt.Println("i's dotted, t's crossed")
}

func pcj() {
    fmt.Println("there's uncle Joe")
    time.Sleep(1e10)
    fmt.Println("movin kinda slow")
}

type requiresExternalCleanup struct {
    id string
}

func cleanup(rec *requiresExternalCleanup) {
    fmt.Println(rec.id, "cleanup")
}
```

{{out}}

```txt

main program start
main program returning
there's uncle Joe
i's dotted, t's crossed

```



### Runtime.Goexit

<code>Runtime.Goexit</code> terminates the goroutine that calls it.
No other goroutine is affected.
<code>Goexit</code> runs all deferred calls before terminating the goroutine.

Calling <code>Goexit</code> from the main goroutine terminates that goroutine without func main returning.
Since func main has not returned, the program continues execution of other goroutines.
If all other goroutines exit, the program crashes.


### Os.Exit

Os.Exit causes its argument to be returned to the operating system as a program exit code.  Unlike the return statement and runtime.Goexit, os.Exit exits promptly and does not run deferred functions.

```go
func main() {
    fmt.Println("main program start")

    // this will not get run on os.Exit
    defer func() {
        fmt.Println("deferred function")
    }()

    if problem {
        fmt.Println("main program exiting")
        os.Exit(-1)
    }
}
```

{{out}}

```txt

main program start
main program exiting

```



### Panic

Panics have some similarities to exceptions in other languages, including that there is a recovery mechanism allowing program termination to be averted.
When the program terminates from panic however, it prints the panic value and then a stack trace for all goroutines.

Like the return statement, panic runs deferred functions.
It run functions deferred from the current function, but then proceeds to unwind the call stack of the goroutine, calling deferred functions at each level.
It does this only in the goroutine where panic was called.
Deferred functions in other goroutines are not run and if panicking goes unrecovered and the program terminates, all other goroutines are terminated abruptly.

```go
func pcj() {
    fmt.Println("at the junction")
    defer func() {
        fmt.Println("deferred from pcj")
    }()
    panic(10)
}

func main() {
    fmt.Println("main program start")
    defer func() {
        fmt.Println("deferred from main")
    }()
    go pcj()
    time.Sleep(1e9)
    fmt.Println("main program done")
}
```

{{out}}

```txt

main program start
at the junction
deferred from pcj
panic: 10
(and the stack trace follows)

```


=={{header|GW-BASIC}}==

```qbasic>10 IF 1 THEN STOP</lang



## Groovy

See [[#Java|Java]] for a more complete explanation.

Solution #1:

```groovy
if (problem) System.exit(intExitCode)
```


Solution #1:

```groovy
if (problem) Runtime.runtime.halt(intExitCode)
```



## Haskell


```haskell
import Control.Monad
import System.Exit

when problem do
    exitWith ExitSuccess                    -- success
    exitWith (ExitFailure integerErrorCode) -- some failure with code
    exitSuccess                             -- success; in GHC 6.10+
    exitFailure                             -- generic failure
```

The above shows how to exit a thread. When the main thread exits, all other threads exit, and the return code in the exit call is the return code of the program. When any thread other than the main thread exits, only it is stopped, and if the exit code is not ExitSuccess, it is printed.


## HicEst


```HicEst
ALARM( 999 )
```

This closes windows, dialogs, files, DLLs, and frees allocated memory. Script editing is resumed on next start.

=={{header|Icon}} and {{header|Unicon}}==

```Icon
exit(i)          # terminates the program setting an exit code of i
stop(x1,x2,..)   # terminates the program writing out x1,..; if any xi is a file writing switches to that file
runerr(i,x)      # terminates the program with run time error 'i' for value 'x'
```



## J

Given <tt>condition</tt>, an integer which is zero if everything's OK (and we should NOT exit), or a non-zero exit code if there's a problem (and we should exit), then:

Tacit version:

```j
2!:55^:] condition
```

Explicit version:

```j
3 : 'if. 0~: condition do. 2!:55 condition end.'
```



## Java

The call <tt>System.exit</tt> does not finalize any objects by default.
This default is to keep the program thread-safe. From the javadocs for the method to change this default: "may result in finalizers being called on live objects while other threads are concurrently manipulating those objects, resulting in erratic behavior or deadlock."

```java
if(problem){
   System.exit(integerErrorCode);
   //conventionally, error code 0 is the code for "OK",
   // while anything else is an actual problem
   //optionally: Runtime.getRuntime().exit(integerErrorCode);
}
```

You can use <code>Runtime.getRuntime().addShutdownHook(myThread);</code> to add threads which represent actions to be run when the program exits.

This one does not perform cleanup:

```java
if(problem){
   Runtime.getRuntime().halt(integerErrorCode);
   //conventionally, error code 0 is the code for "OK",
   // while anything else is an actual problem
}
```



## JavaScript

{{works with|SpiderMonkey}}
The <code>quit()</code> function exits the shell.

```javascript
if (some_condition)
    quit();
```




## jq

The jq process will stop when it encounters a call to the function "error" (error/0) unless it occurs within the scope of a "try" command.

'''Example:'''

```sh
$ jq -n '"Hello", if 1 then error else 2 end'
"Hello"
```


Note that error/0 expects its input to be null (as above), in which case no error message is printed, or a string, in which case the string is printed as an error message, as illustrated below:

```sh
$ jq -n '"Hello" | if 1 then error else 2 end'
jq: error: Hello
```



## Jsish

Jsi supports ''assert'', normally disabled, but will abort (regardless of control flow depth) with a message after cleanup.

Returning a code to operating system, regardless of control flow depth, is via ''exit(code)'' where numeric ''code'' is passed to the parent process (after internal cleanup).

Signal handlers can also be installed to trap external events and deal with the issue programmatically, via Signal.callback, Signal.handle, Signal.ignore along with other control methods.  These ''interrupt'' handlers will fire regardless of control flow depth.

In ''jsish'' Ctrl-C will halt any running code and return to the shell.  If no code is running, the shell exits on Ctrl-C.


```javascript
assert(0 == 1);

if (problem) exit(1);
```



## Julia


```julia

quit() # terminates program normally, with its child processes. See also exit(0).

```



## Kotlin

Premature termination of a program in Kotlin would normally be achieved by calling Java's System.exit(status) method which calls Runtime.getRuntime().exit(status) under the hood and passes the value of 'status' to the operating system. By convention a non-zero code indicates that termination was triggered due to some problem. According to the Java documentation, prior to JVM termination:

1. All shutdown hooks, registered by the Runtime.addShutdownHook(hook) method, would be started in an unspecified order and allowed to run concurrently until they finish.

2. Uninvoked finalizers would then be invoked if this behavior had been enabled using the Runtime.runFinalizersOnExit(true) method. However, this method is currently deprecated because it may result in finalizers being called on live objects while other threads are concurrently manipulating those objects, leading to erratic behavior or even deadlock.


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val problem = true
    if (problem) System.exit(1) // non-zero code passed to OS to indicate a problem
    println("Program terminating normally")  // this line will not be executed
}
```

After the program has terminated, the exit status can be queried from the command line (Windows 10) as follows:
{{out}}

```txt

c:\kotlin-compiler-1.0.6>echo %errorlevel%
1

```



## Lasso

Lasso will stop processing when it encounters an "abort". By providing a "handle" block, possible cleanups can be executed before finishing execution.

```Lasso
#!/usr/bin/lasso9
//[

handle => {
	stdoutnl('The end is here')
}

stdoutnl('Starting execution')

abort

stdoutnl('Ending execution')
```


{{out}}

```txt
Starting execution
The end is here

```


It is also possible to provide a "handle" block that will only execute if there's an error.

```Lasso
#!/usr/bin/lasso9

handle_error => {
	stdoutnl('There was an error ' + error_msg)
	abort
}

stdoutnl('Starting execution')

0/0

stdoutnl('Ending execution')
```


{{out}}

```txt
Starting execution
There was an error Divide by zero

```



## Liberty BASIC

If any files or devices are still open when execution is terminated, then Liberty BASIC will close them and present a dialog expressing this fact.

The STOP statement is functionally identical to END and is interchangable. Also, make sure that when a program is finished running that it terminates properly with an END statement. Otherwise the program's windows may all be closed, giving the illusion that it has stopped running, but it will still be resident in memory and may still consume processor resources.

The following is functional. Better practice is to instead jump to commands or subs to close known open files, windows etc, avoiding error messages as above.

```lb>if 2 =2 then end</lang



## Logo

{{works with|UCB Logo}}

```logo
bye   ; exits to shell

throw "toplevel  ; exits to interactive prompt

pause      ; escapes to interactive prompt for debugging
continue   ; resumes after a PAUSE
```



## Lua


```lua
if some_condition then
    os.exit( number )
end
```



## M2000 Interpreter

Set End send End to command line interpreter which end the program
Threads also erased when the module where belong exit.

M2000 has two interpreters the CLI which used in M2000 console (it is in global namespace), and the program interpreter when we execute a module or a function (always they have a namespace by the name of module/function, and the position of the call in the flow).


Statement End in program interpreter is same as Exit

IF we didn't use Escape Off we can end the program using Esc key. We can use Break key to restart it. We can change Set End with Test to open dialog for step by step execution, slow and stop. From this dialog we can execute statements and we can see the code in color syntax as executed (including threads, excluding events from Gui)




```M2000 Interpreter

Module Checkit {
      For i=1 to 200
      Thread {
            k++
            Print "Thread:"; num, "k=";k
      } as M
      Thread M Execute  {
            static num=M, k=1000*i
      }
      Thread M interval 100+900*rnd
      next i

      Task.Main 20 {
            if random(10)=1 then Set End
      }
}
Checkit

```



## M4


```M4
beginning
define(`problem',1)
ifelse(problem,1,`m4exit(1)')
ending
```

{{out}}

```txt

beginning

```



## Mathematica


```Mathematica
If[problem, Abort[]];
```

Kernels stop all computation after "Abort[]" command. But the kernels are still operational, and all definitions are still available. Note that an Abort[] can be caught by a calling function using <code>CheckAbort</code>, in which case the computation will continue at that place.

```txt

Quit[]

```

This will completely quit the kernel. All definitions will be lost. Since this terminates the actual kernel process, this will also free all resources used by that kernel (especially memory). Note however that if the kernel is interactively used through a notebook, the notebook still remains operable.


## MATLAB


```matlab
if condition
    return
end
```

There is no special way to stop a program. You can terminate it by calling <code>return</code>.

```matlab
if condition
    quit
end
```

The <code>quit</code> function runs the MATLAB script <code>finish.m</code>, if it exists, and terminates MATLAB completely.


## Maxima


```maxima
/* Basically, it's simply quit() */

block([ans], loop, if (ans: read("Really quit ? (y, n)")) = 'y
                   then quit()
                   elseif ans = 'n then (print("Nice choice!"), 'done)
                   else (print("I dont' understand..."), go(loop)));
```


=={{header|МК-61/52}}==
<lang>ИП0	x=0	04	С/П	...
```


Condition of termination is ''Р0 = 0''.


## Neko

Neko installs abnormal run down handlers, or can call the C ABI exit routine, which returns a status code to the operating system.


```ActionScript
/*
 Program termination, in Neko
*/

var sys_exit = $loader.loadprim("std@sys_exit", 1)

var return_code = 42
if true sys_exit(return_code)

$print("Control flow does not make it this far")
```


{{out}}

```txt
prompt$ nekoc program-termination.neko
prompt$ neko program-termination.n
prompt$ echo $?
42
```



## Nemerle


```Nemerle
using System.Environment
...
    when (problem) Exit(1)
...
```



## NetRexx

NetRexx's <tt>exit</tt> statement invokes [[Java|Java's]] <tt>System.exit()</tt> so job termination is handled in the same way as any other Java program. (See [[#Java|Java]] above.)

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

extremePrejudice = (1 == 1)
if extremePrejudice then do
  exit extremePrejudice
  end

return

```



## Nim


```Nim
if problem:
  quit(QuitFailure)
```


=={{header|Oberon-2}}==

```oberon2

  IF problem THEN
    HALT(1)
  END

```



## Objeck

The code below, will terminate a program without any cleanup.

```objeck
if(problem) {
  Runtime->Exit(1);
};
```



## OCaml


```ocaml
if problem then
  exit integerErrorCode;
  (* conventionally, error code 0 is the code for "OK",
     while anything else is an actual problem *)
```

The <code>at_exit</code> function can be used to register functions to be run when the program exits. Registered functions will be called in the reverse order in which they were registered.


## Oforth


OS.exit returns to OS with return value as parameter.


```Oforth
import: os

some_condition ifTrue: [ 0 OS.exit ]
```



## Ol


```scheme

(shutdown 0) ; it can be any exit code instead of provided 0

```



## Oz


```oz
if Problem then {Application.exit 0} end
```

All threads exit. All processes (local and remote) exit unless they were created with <code>detach:true</code>. Finalizers are not executed (unless enforced with <code>{System.gcDo})</code>.


## PARI/GP


```parigp
if(stuff, quit)
```



## Perl


```perl
if ($problem) {
    exit integerErrorCode;
    # conventionally, error code 0 is the code for "OK"
    #  (you can also omit the argument in this case)
    # while anything else is an actual problem
}
```


The <code>DESTROY()</code> methods of all objects are called, in an unspecified order (see "Global Destruction" in <code>perlobj.pod</code>).  This includes objects in global variables or with circular references which otherwise keep them alive during normal running.

<code>END { }</code> blocks are executed in reverse order of their creation (see <code>perlmod.pod</code>).

The underlying C library <code>exit()</code> runs its C level <code>atexit()</code> callbacks.

An exit without these cleanups can be done with <code>POSIX::_exit()</code> (as noted in <code>perlfunc.pod</code> under the normal <code>exit()</code>).  This is the <code>_exit()</code> system call (which the C library generally provides in equivalent form on non-Unix/non-POSIX systems too).


## Perl 6


```perl6
if $problem { exit $error-code }
```

An <tt>exit</tt> runs all appropriate scope-leaving blocks such as <tt>LEAVE</tt>, <tt>KEEP</tt>, or <tt>UNDO</tt>,
followed by all <tt>END</tt> blocks, followed by all destructors that do more than just reclaim memory, and so cannot be skipped because they may have side effects visible outside the process.  If run from an embedded interpreter, all
memory must also be reclaimed.  (Perl 6 does not yet have a thread-termination policy, but will need to before we're done.)


## Phix

To terminate the entire application:

```Phix
if error_code!=NO_ERROR then
    abort(0)
end if
```

To terminate just the current thread:

```Phix
if error_code!=NO_ERROR then
    exit_thread(0)
end if
```

Files will be closed automatically and memory will be freed, however any other cleanup should be invoked manually.

Most of my code has Abort() routines acting as wrappers for final-housekeeping-then-abort().


## PHP


```php
if (problem)
    exit(1);
```

The <code>register_shutdown_function()</code> function can be used to register functions to be run when the program exits.


## PicoLisp

Calling 'bye', optionally with a numeric code, terminates the program.

This will execute all pending 'finally' expressions, close all open files and/or pipes, flush standard output, and execute all expressions in the global variable '*Bye' before exiting.

```PicoLisp
(push '*Bye '(prinl "Goodbye world!"))
(bye)
```

{{out}}

```txt
Goodbye world!
$
```



## PL/I


```pli
STOP; /* terminates the entire program */
      /* PL/I does any required cleanup, such as closing files. */
```


```pli
STOP THREAD (tiger); /* terminates only thread "tiger". */
```


```pli
SIGNAL FINISH; /* terminates the entire program.   */
               /* PL/I does any required cleanup,  */
               /* such as closing files.           */
```



## Pop11


```pop11
if condition then
    sysexit();
endif;
```



## PostScript

There are two ways which differ slightly:

```postscript
condition {stop} if
```

will terminate a so-called <code>stopped</code> context which is a way of executing a block of code and catching errors that occur within. Any user program will always run in such a context and therefore be terminated upon calling <code>stop</code>

Neither the operand stack nor the dictionary stack are touched or cleaned up when calling <code>stop</code>. Anything pushed onto either stack will remain there afterwards.

```postscript
condition {quit} if
```

will terminate the PostScript interpreter. This is definitely a way to stop the current program but since an interpreter can run multiple programs at the same time, this should rarely, if ever, be used.


## PowerShell


```powershell
if (somecondition) {
    exit
}
```

This ends the scope for any non-global variables defined in the script. No special cleanup is performed.


## Prolog

Terminate Prolog execution. Open files are closed. Exits the Interpreter.

```prolog>halt.</lang

Terminate Prolog execution but don't exit the Interpreter.

```prolog>abort.</lang



## PureBasic

This will free any allocated memory, close files and free other resources (i.e. windows, gadgets, threads, space for variable, etc.) that were set aside during execution of any PureBasic commands in the program.

```PureBasic
If problem = 1
   End
EndIf
```

It is possible to also access outside resources (i.e. via an OS API or linked library), and those items may or may not be cleaned up properly.


## Python

;Polite:

```python
import sys
if problem:
    sys.exit(1)
```

The [http://docs.python.org/library/atexit.html atexit] module allows you to register functions to be run when the program exits.
;As soon as possible:
(Signals the underlying OS to abort the program. No cleanup is performed)

```python
import os
if problem:
    os.abort()
```



## R


```r
if(problem) q(status=10)
```



## Racket


Racket has an "exit" function that can be used to exit the Racket
process, possibly returning a status code.


```Racket

#lang racket
(run-stuff)
(when (something-bad-happened) (exit 1))

```


In addition, Racket has "custodians", which are objects that are used to
manage a bunch of dynamically allocated resources (ie, open files,
running threads).  This makes it easy to run some code and conveniently
kill it with all related resources when needed, without exiting from the
whole process.  It is common to use this facility in servers, where each
handler invocation is wrapped in a new custodian that is shut down when
its client interaction is done.  For example:


```Racket

#lang racket
(parameterize ([current-custodian (make-custodian)])
  (define (loop) (printf "looping\n") (sleep 1) (loop))
  (thread loop) ; start a thread under the new custodian
  (sleep 5)
  ;; kill it: this will kill the thread, and any other opened resources
  ;; like file ports, network connections, etc
  (custodian-shutdown-all (current-custodian)))

```



## REBOL

The '''quit''' word stops all evaluation, releases operating system resources and exits the interpreter.

```REBOL
if error? try [6 / 0] [quit]
```

A return value can be provided to the operating system:

```REBOL
if error? try [dangerous-operation] [quit/return -12]
```

Because of REBOL's tightly integrated REPL, you can also use '''q''' to do the same thing.

```REBOL
if error? try [something-silly] [q/return -12]
```

Since GUI programs are often developed from the REPL, a special '''halt''' word is provided to kill the GUI and return to the REPL. No cleanup is done and the GUI is still displayed (although halted). You can restart it with the '''do-events''' word.

```REBOL
view layout [button "stopme" [halt]]
```



## Retro


```Retro
problem? [ bye ] ifTrue
```



## REXX

In REXX, the REXX interpreter takes care of the closing of any open files (or any I/O streams), as well as any memory management (cleanup).

```rexx
/*REXX program showing five ways to perform a REXX program termination. */

  /*─────1st way────────────────────────────────────────────────────────*/
exit


  /*─────2nd way────────────────────────────────────────────────────────*/
exit  (expression)     /*Note: the "expression" doesn't need parentheses*/
                       /*"expression"  is any REXX expression.          */


  /*─────3rd way────────────────────────────────────────────────────────*/
return                 /*which returns to this program's invoker.  If   */
                       /*this is the main body  (and not a subroutine), */
                       /*the REXX interpreter terminates the program.   */


  /*─────4th way────────────────────────────────────────────────────────*/
return (expression)    /* [See the note above concerning parentheses.]  */


  /*─────5th way────────────────────────────────────────────────────────*/
    /*control*/
    /*   │   */        /*if there is no EXIT and program control "falls */
    /*   │   */        /*through" to the "bottom" (end) of the program, */
    /*   │   */        /*an   EXIT   is simulated and the program is    */
    /*   │   */        /*terminated.                                    */
    /*   ↓   */
    /* e-o-f */        /* e-o-f   =   end-of-file.                      */
```


Regina actually implies a RETURN when the end of the program is found at the end of a subroutine:

```rexx
Parse Version v
Say v
Call sub
Say 'Back from sub'
Exit
sub:
```

{{out}}

```txt
REXX-Regina_3.9.1(MT) 5.00 5 Apr 2015
Back from sub
```



## Ring


```ring

for n = 1 to 10
    see n + nl
    if n = 5 exit ok
next

```



## Ruby


```ruby
if problem
  exit(1)
end

# or
if problem
  abort   # equivalent to exit(1)
end
```


You can use <code>at_exit { ... }</code> to register a block of code which will be run when the program exits. Registered handlers will be called in the reverse order in which they were registered.


```ruby
if problem
  exit!   # default value 1
end
```

Exits the process immediately. No exit handlers are run.
<code>exit!</code> is different from <code>exit</code> and it doesn't do an exception handling.


## Run BASIC


```runbasic>if whatever then end</lang



## Rust


### Return statement

A return statement executed in the main() function will exit the program.

```rust
fn main() {
    println!("The program is running");
    return;
    println!("This line won't be printed");
}
```



### Exit function

You can run <code>std::process::exit</code> from anywhere in the program in order to exit. This will work from the main function as well as any other function or file.


```rust
fn main() {
    if problem {
        std::process::exit(1); // 1 is the exit code
    }
}
```



### Panics

A panic in Rust will terminate the current thread. If the panic is in the main thread, the program will exit. If the panic is from another thread; that thread will terminate and the program, along with the other threads, will keep running.


```rust
fn main() {
    println!("The program is running");
    panic!("A runtime panic occured");
    println!("This line won't be printed");
}
```

Because of the panic, the last line will not run. If the panic happened in another thread, the program could keep running.

'''Panic Inside a Thread'''

```rust
use std::thread;

fn main() {
    println!("The program is running");

    thread::spawn(move|| {
        println!("This is the second thread");
        panic!("A runtime panic occured");
    }).join();

    println!("This line should be printed");
}
```

Now the panic will be contained inside the background thread and will not affect the rest of the program.


## Scala

{{libheader|Scala}}

```Scala
if (problem) {
  // sys.exit returns type "Nothing"
  sys.exit(0)
  // conventionally, error code 0 is the code for "OK",
  // while anything else is an actual problem
}

```



## Slate


```slate
problem ifTrue: [exit: 1].
```



## Scheme

{{works with|Scheme|R<sup>6</sup>RS}}

```scheme
(if problem
  (exit)) ; exit successfully
```

or

```scheme
(if problem
  (exit #f)) ; exit unsuccessfully
```

or

```scheme
(if problem
  (exit some-value)) ; converts "some-value" into an appropriate exit code for your system
```



## Seed7

When a program is stopped with exit(PROGRAM) allocated memory is freed and open files are closed,

```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    # whatever logic is required in your main procedure
    if some_condition then
      exit(PROGRAM);
    end if;
  end func;
```



## Sidef


```ruby
if (problem) {
    Sys.exit(code);
}
```


## Simula


```Simula
IF terminallyIll THEN terminate_program;
```
»The procedure "terminate_program" terminates program execution. It closes SYSIN and SYSOUT.
  It is implementation-dependent with respect to whether or not other open files are also closed.« [[http://simula67.at.ifi.uio.no/Standard-86/chap_10.htm| Simula Standard 86]]


## SNOBOL4

Conditional transfer to the required END label causes immediate termination
and normal cleanup.
In this example, if ''condition'' succeeds (is true),
the value of ''errlevel'' is assigned to the &code keyword
as the exit status of the program,
and the :s( ) goto transfers control to END.

```SNOBOL4
        &code = condition errlevel :s(end)
```



## SSEM

The machine can be halted at any point using a <tt>111 Stop</tt> instruction. Since the only conditional operation we have is <tt>011 Test</tt>, which has the effect of skipping one word if the number in the accumulator is negative, we had better illustrate it using that.

```ssem
00000000000000110000000000000000   Test
00000000000001110000000000000000   Stop
```

This code fragment stops the computer if the accumulator is positive or zero; if it is negative, the <tt>Stop</tt> instruction is skipped and execution continues from the next instruction.


## Standard ML

No cleanup is performed.

```sml
if problem then
  OS.Process.exit OS.Process.failure
  (* valid status codes include OS.Process.success and OS.Process.failure *)
else
  ()
```

The <code>OS.Process.atExit</code> function can be used
to register functions to be run when the program exits.
Registered functions will be called in the reverse order
in which they were registered.


## Tcl

The language runtime (in [[C]]) includes a mechanism for cleaning up open resources when the application quits, but access to this is not exposed at script level; extension packages just register with it automatically when required.
At the script level, all that is needed to make the program terminate
is the <tt>exit</tt> command:

```tcl
if {$problem} {
    # Print a “friendly” message...
    puts stderr "some problem occurred"
    # Indicate to the caller of the program that there was a problem
    exit 1
}
```

Alternatively, in a top-level script but ''not'' an event handler:

```tcl
if {$problem} {
    error "some problem occurred"
}
```


=={{header|TI-83 BASIC}}==
 If 1
 Stop
<tt>Return</tt> works as well, but if run from inside a subprogram (TI-83 BASIC's version of a function) it will return - hence the function's name - to the main program instead of stopping everything.

When a program is executed, the OS copies its data to another RAM area (0x9D95 on the monochrome calculators, and 0xA60B on the C Silver Edition) and pushes some information about it onto the OS stack. When the program terminates via a Return/Stop statement or error, both of those are removed.

=={{header|TI-89 BASIC}}==

```ti89b
Prgm
  ...
  Stop
  ...
EndPrgm
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
IF (condition==1) STOP
-> execution stops and message:
IF (condition==2) ERROR/STOP "condition ",condition, " Execution STOP "
```



## Unlambda


```unlambda
`ei
```

Note: the argument to the <code>e</code> function is the return value of the program; however many implementation simply ignore it.

There are no objects to be cleaned up.


## UNIX Shell

{{works with|Bourne Shell}}

```bash
#!/bin/sh

a='1'
b='1'
if [ "$a" -eq "$b" ]; then
  exit 239    # Unexpected error
fi
exit 0    # Program terminated normally
```



## Ursa

Standard Ursa supports the stop function, which immediately halts program execution.

```ursa>stop</lang



## VBA


```vb
'In case of problem this will terminate the program (without cleanup):
If problem then End
'As VBA is run within an application, such as Excel, a more rigorous way would be:
If problem then Application.Quit
'This will stop the application, but will prompt you to save work.
```


## Vedit macro language


```vedit
if (#99 == 1) { Return }        // Exit current macro. Return to calling macro.
if (#99 == 2) { Break_Out() }   // Stop all macro execution and return to command mode.
if (#99 == 3) { Exit }          // Exit Vedit. Prompt for saving any changed files.
if (#99 == 4) { Exit(4) }       // As above, but return specified value (instead of 0) to OS
if (#99 == 5) { Xall }          // Exit Vedit. Save changed files without prompting.
if (#99 == 6) { Qall }          // Exit Vedit. Do not save any files.
```

Return or Break_Out() do not perform any cleanup. If needed, cleanup has to be done in the macro before exit.
Special ''locked-in macro'' can be used to perform cleanup in case user presses Break key.

When exit from Vedit is done, all the cleanup is performed automatically.
Note, however, that if ''Edit Restore'' is enabled or a ''project'' is open, the session state is saved.
In this case, if your macro does not do cleanup, you may eventually run out of free text registers, and you have to do manual cleanup.


## VBScript

No matter how deep you're in, <code>wscript.quit</code> will get you out.

```vb
dim i, j
j = 0
do
    for i = 1 to 100
        while j < i
            if i = 3 then
                wscript.quit
            end if
        wend
    next
loop
```



## Visual Basic

While the example listed under [[#BASIC|BASIC]] will work unaltered, it is a terrible idea to use the <code>End</code> keyword in VB. Doing so will cause the ''immediate'' termination of the program without ''any'' clean up -- forms and other things are left loaded in memory.

When the app needs to end, for whatever reason, problem or not, it's always a good idea to unload the forms ''first''.

```vb
Sub Main()
    '...
    If problem Then
        For n& = Forms.Count To 0 Step -1
            Unload Forms(n&)
        Next
        Exit Sub
    End If
    '...
End Sub
```



## XPL0

XPL0 cleans up after itself. Its DOS Protected Mode Interface (DPMI)
releases extended memory (since DOS can't do it). It restores any system
vectors that were altered, such as the divide-by-zero exception vector.
It also restores changes made to the 8254 system timer chip (or its
equivalent).

The value following 'exit' is optional. It's passed to DOS and can be
used by a controlling batch file in an IF ERRORLEVEL statement, and thus
change the commands the batch file executes.


```XPL0
if Problem then exit 1;

```



## zkl


```zkl
if (die) System.exit();
if (die) System.exit(1);
if (die) System.exit("dumping core");
```

The parameter to exit (string or number) determines how hard zkl exits. 0 runs the garbage collector to close any orphaned open files, etc. Any other number doesn't. Text will cause a core dump (if $zklDumpCore is set). The OS gets to clean up any mess.
