+++
title = "Stack traces"
description = ""
date = 2019-03-20T13:02:12Z
aliases = []
[extra]
id = 4197
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}
Many programming languages allow for introspection of the current call stack environment. This can be for a variety of purposes such as enforcing security checks, debugging, or for getting access to the stack frame of callers.


;Task:
Print out (in a manner considered suitable for the platform) the current call stack. 

The amount of information printed for each frame on the call stack is not constrained, but should include at least the name of the function or method at that level of the stack frame. 

You may explicitly add a call to produce the stack trace to the (example) code being instrumented for examination.

The task should allow the program to continue after generating the stack trace. 

The task report here must include the trace from a sample program.





## Ada

{{works with|GNAT}}

The provided solution is specific to the [[GNAT]] Ada compiler. Further it is restricted to some platforms. See the description of the package GNAT.Traceback supplied with [[GNAT]]. The switch -g must be used in order to include debug information into the executable.

```Ada
with Ada.Text_IO;  use Ada.Text_IO;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

procedure Test_Stack_Trace is
   procedure Call_Stack is
      Trace  : GNAT.Traceback.Tracebacks_Array (1..1_000);
      Length : Natural;
   begin
      GNAT.Traceback.Call_Chain (Trace, Length);
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1..Length)));
   end Call_Stack; 

   procedure Inner (K : Integer) is
   begin
      Call_Stack;
   end Inner;
 
   procedure Middle (X, Y : Integer) is
   begin
      Inner (X * Y);
   end Middle;
 
   procedure Outer (A, B, C : Integer) is
   begin
     Middle (A + B, B + C);
   end Outer;
   
begin
  Outer (2,3,5);
end Test_Stack_Trace;
```

Sample output:

```txt

00417D7F in ?? at cygming-crtend.c:0
00401A61 in test_stack_trace.call_stack at test_stack_trace.adb:10
00401A25 in test_stack_trace.inner at test_stack_trace.adb:16
00401A0C in test_stack_trace.middle at test_stack_trace.adb:21
0040193E in test_stack_trace.outer at test_stack_trace.adb:26
004018A2 in _ada_test_stack_trace at test_stack_trace.adb:30
004016BE in main at b~test_stack_trace.adb:183
00401235 in ?? at cygming-crtend.c:0
00401286 in ?? at cygming-crtend.c:0
7C817075 in ?? at ??:0

```



## AutoHotkey

The local, static, and global bindings are viewable using 'Listvars'

 recently executed lines are available through ListLines


ListLines can be turned on or off... with:

 ListLines, On|Off

```autohotkey
f()
return
 
f()
{
return g()
}

g()
{
ListLines
msgbox, lines recently executed
x = local to g
ListVars
msgbox, variable bindings
}
#Persistent
```

Output:

```autohotkey
001: f()  
006: Return,g()
011: ListLines (0.05)
012: MsgBox,lines recently executed (3.81)
013: x = local to g
014: ListVars
015: MsgBox,variable bindings (3.94)
016: }
002: Return (181.66)

Global Variables (alphabetical)
--------------------------------------------------
0[1 of 3]: 0
ErrorLevel[1 of 3]: 0

```



## BASIC

{{works with|Beta BASIC|3.0}}

{{works with|SAM BASIC}}

On Beta BASIC and SAM BASIC, the call stack is used for procedure calls, GOSUBs and DO loops.
The stack contains return addresses as line and statement numbers. (The parameters for procedures are stored in another stack.)
POP statement can be used to pop the return address from the stack.

The following procedure pops the return addrsses from the stack and lists the corresponding lines where the call occurred.
Since it is not possible to push the addresses back into the stack, it is not possible to continue the normal flow of execution after displaying the stack.
However, it is possible to continue the program otherwise.
If the program execution stops on error, it is possible to display the call stack by typing <tt>callstack</tt> in command mode.


```txt

 100 DEF PROC callstack
 110   ON ERROR GOTO 1000
 120   FOR i=1 TO 100
 130      POP lnum
 140      LIST lnum TO lnum
 150   NEXT i
 190 END PROC

1000 PRINT "End of stack"
1010 STOP

```


Example usage. An error is generated on line 320, which causes branch to error handler on line 1100.
The error handler displays error number followed by call stack.


```txt

 200 DEF PROC foo count
 210   PRINT "foo ";
 220   IF count > 1
 230     foo count-1
 240   ELSE
 250     bar
 260   END IF
 290 END PROC

 300 DEF PROC bar
 310   PRINT "bar"
 320   x = 1/0
 390 END PROC
 
 500 ON ERROR GOTO 1100
 510 foo 3
 520 STOP

1100 PRINT "Error "; error; " on line "; lino
1110 PRINT "Callstack:"
1120 callstack
1130 STOP

```


Output:


```txt

foo foo foo bar
Error 28 on line 320
Call stack:
 1120 callstack
  320 x = 1/0
  250 bar
  230 foo count-1
  230 foo count-1
  510 foo 3
End of stack

```



## C


###  Using GNU extensions 

{{works with|POSIX}}

{{works with|GNU}}

The <tt>backtrace*</tt> functions are a GNU extension to the standard C library.

In order to be able to see the symbols, we need to link with an option telling to export symbols names in the dynamic section (for ELF and targets supporting it); for gcc, it means using the option <tt>-rdynamic</tt> (or <tt>-export-dynamic</tt> in the GNU linker). Otherwise we see only addresses. Static functions will always have their names "hidden".


```c>#include <stdio.h

#include <stdlib.h>
#include <unistd.h>

#include <execinfo.h>

#define MAX_BT 200

void print_stack_trace()
{
  void *buffer[MAX_BT];
  int n;

  n = backtrace(buffer, MAX_BT);
  fprintf(stderr, "--- (depth %d) ---\n", n);
  backtrace_symbols_fd(buffer, n, STDERR_FILENO);
}


void inner(int k)
{
  print_stack_trace();
}

void middle(int x, int y)
{
  inner(x*y);
}

void outer(int a, int b, int c)
{
  middle(a+b, b+c);
}

int main()
{
  outer(2,3,5);
  return EXIT_SUCCESS;
}
```


Sample output on my system:


```txt
--- (depth 7) ---
./pst(print_stack_trace+0x1f)[0x8048683]
./pst(inner+0xb)[0x80486cd]
./pst(middle+0x15)[0x80486e4]
./pst(outer+0x23)[0x8048709]
./pst(main+0x2d)[0x8048738]
/lib/i686/libc.so.6(__libc_start_main+0xe5)[0xb7e045c5]
./pst[0x80485d1]
```




###  Using no extensions 

Sometimes microcomputers do not come with any kind of debug or stack tracing routines.  Typically a program would "just hang" somewhere, or crash the gadget.  This then requires manually "wolf fencing" of the bug with ''printf'' statements to identify the C source file, then the procedure, and then the line at the point of system crash.

The following macros and procedures provide an alternative way of doing this trouble shooting.

The steps are:
* #include "stack_trace.h" in the suspect C source code.
* Change the initial and last { and } of each procedure in the code to BEGIN(proc_name) and END.  
* #define STACK_TRACE_ON and assign stack_trace.on to TRUE
* Recompile and run....

The resulting output can be used to locate offending procedure and - hopefully - give a hint to the location of the actual bug.

The key point is that the following can be done on systems that are equipped with only and editor and compiler, and no debugger or library extension.

'''==> stack_trace.h <=='''

```C

/* stack_trace.c - macros for hinting/tracing where a program crashed
                   on a system _without_ any form of debugger.

Simple goodbye_cruel_world.c example:

#include <stdio.h>
#include <stdlib.h>

#define STACK_TRACE_ON // compile in these "stack_trace" routines
#include "stack_trace.h"

void goodbye_cruel_world()
BEGIN(goodbye_cruel_world)
  print_stack_trace();
  for(;;){}
END

int main()
BEGIN(main)
  stack_trace.on = TRUE; // turn on runtime tracing
  goodbye_cruel_world();
  stack_trace.on = FALSE;
  RETURN(EXIT_SUCCESS);
END

Output:
goodbye_cruel_world.c:8: BEGIN goodbye_cruel_world[0x80486a8], stack(depth:1, size:60)
goodbye_cruel_world.c:8:        goodbye_cruel_world[0x80486a8]  --- stack(depth:2, size:60) ---
goodbye_cruel_world.c:14:       main[0x80486f4] --- stack(depth:1, size:0) ---
goodbye_cruel_world.c:8:   --- (depth 2) ---

*/
#ifndef _LINUX_STDDEF_H
#include <stddef.h>
#endif

typedef struct stack_trace_frame_s { 
  const char *file_name;
  int file_line;
  const char *proc_name;
  void *proc_addr;
  struct stack_trace_frame_s *down, *up;
} stack_trace_frame_t;

#define SKIP
typedef enum {TRUE=1, FALSE=0} bool_t;

typedef struct {
  bool_t on;
  struct { const char *_begin, *_print, *_return, *_exit, *_end; } fmt;
  struct { int depth; stack_trace_frame_t *lwb, *upb; } stack;
  struct { int lwb, by, upb; const char *prefix; } indent;
} stack_trace_t;

extern stack_trace_t stack_trace;

void stack_trace_begin(char *SKIP, stack_trace_frame_t *SKIP);
void stack_trace_end(char *SKIP, int SKIP);
void print_stack_trace();


#ifdef STACK_TRACE_ON

/* Many ThanX to Steve R Bourne for inspiring the following macros ;-) */
#define BEGIN(x) { auto stack_trace_frame_t this = {__FILE__, __LINE__, #x, &x, NULL, NULL}; \
                  stack_trace_begin(stack_trace.fmt._begin, &this); {
#define RETURN(x) { stack_trace_end(stack_trace.fmt._return, __LINE__); return(x); }
#define EXIT(x)   { stack_trace_end(stack_trace.fmt._exit, __LINE__); exit(x); }
#define END }       stack_trace_end(stack_trace.fmt._end, __LINE__); }

#else

/* Apologies to Landon Curt Noll and Larry Bassel for the following macros :-) */
#define BEGIN(x) {
#define RETURN(x) return(x)
#define EXIT(x) exit(x)
#define END }

#endif
```

'''==> stack_trace.c <=='''

```C>#include <stdio.h

#include <stddef.h>

#define STACK_TRACE_ON
#include "stack_trace.h"

#define indent_fmt "%s"
#define std_cc_diag_fmt "%s:%d: "
#define stack_trace_diag_fmt " %s[0x%x], stack(depth:%d, size:%u)\n"
#define stack_trace_fmt "%s:%d:\t%s[0x%x]\t--- stack(depth:%d, size:%u) ---\n"

stack_trace_t stack_trace  = { 
    FALSE, /* default: stack_trace.on == FALSE */
    { std_cc_diag_fmt""indent_fmt"BEGIN"stack_trace_diag_fmt, 
      stack_trace_fmt, 
      std_cc_diag_fmt""indent_fmt"RETURN"stack_trace_diag_fmt, 
      std_cc_diag_fmt""indent_fmt"EXIT"stack_trace_diag_fmt, 
      std_cc_diag_fmt""indent_fmt"END"stack_trace_diag_fmt },
    { 0, (stack_trace_frame_t*)NULL, (stack_trace_frame_t*)NULL }, /* stack */
    { 19, 2, 20, "                   " } /* indent wrap */
  };

void stack_trace_begin(const char *fmt, stack_trace_frame_t *this){
  if(stack_trace.on){
    fprintf(stderr, fmt,
      this->file_name, this->file_line,  /* file details */
      stack_trace.indent.prefix+stack_trace.indent.lwb,  
      this->proc_name, this->proc_addr,  /* procedure details */
      stack_trace.stack.depth, (unsigned)stack_trace.stack.lwb-(unsigned)this);
    stack_trace.indent.lwb =
        ( stack_trace.indent.lwb - stack_trace.indent.by ) % stack_trace.indent.upb;
  }

  if(!stack_trace.stack.upb){ /* this IS the stack !! */
    stack_trace.stack.lwb = stack_trace.stack.upb = this;
  } else {
    this -> down = stack_trace.stack.upb;
    stack_trace.stack.upb -> up = this;
    stack_trace.stack.upb = this;
  }
  stack_trace.stack.depth++;
}

void stack_trace_end(const char *fmt, int line){
  stack_trace.stack.depth--;
  if(stack_trace.on){
    stack_trace.indent.lwb =
        ( stack_trace.indent.lwb + stack_trace.indent.by ) % stack_trace.indent.upb;
    stack_trace_frame_t *this = stack_trace.stack.upb;
    fprintf(stderr, fmt, 
      this->file_name, this->file_line,  /* file details */
      stack_trace.indent.prefix+stack_trace.indent.lwb,  
      this->proc_name, this->proc_addr,  /* procedure details */
      stack_trace.stack.depth, (unsigned)stack_trace.stack.lwb-(unsigned)this);
  }
  stack_trace.stack.upb = stack_trace.stack.upb -> down;
}

void print_indent(){
  if(!stack_trace.stack.upb){
    /* fprintf(stderr, "STACK_TRACE_ON not #defined during compilation\n"); */
  } else {
    stack_trace_frame_t *this = stack_trace.stack.upb;
    fprintf(stderr, std_cc_diag_fmt""indent_fmt, 
      this->file_name, this->file_line,  /* file details */
      stack_trace.indent.prefix+stack_trace.indent.lwb
    );
  }
}

void print_stack_trace() {
  if(!stack_trace.stack.upb){
    /* fprintf(stderr, "STACK_TRACE_ON not #defined during compilation\n"); */
  } else {
    int depth = stack_trace.stack.depth;
    stack_trace_frame_t *this = stack_trace.stack.upb;
    for(this = stack_trace.stack.upb; this; this = this->down, depth--){
      fprintf(stderr, stack_trace.fmt._print,
        this->file_name, this->file_line,  /* file details */
        this->proc_name, this->proc_addr,  /* procedure details */
        depth, (unsigned)stack_trace.stack.lwb-(unsigned)this);
    }
    print_indent(); fprintf(stderr, "--- (depth %d) ---\n", stack_trace.stack.depth);
  }
}
```


'''==> stack_trace_test.c <=='''

The following code demonstrates the usage of the macros.  Note that the initial and last curly brackets have been changed to BEGIN(procedure_name) and END.  This is sometimes called '''macro magic''' and is unfashionable.


```C>#include <stdio.h

#include <stdlib.h>

#define STACK_TRACE_ON /* compile in these "stack_trace" routines */
#include "stack_trace.h"

void inner(int k)
BEGIN(inner)
   print_indent(); printf("*** Now dump the stack ***\n");
   print_stack_trace();
END
 
void middle(int x, int y)
BEGIN(middle)
  inner(x*y);
END
 
void outer(int a, int b, int c)
BEGIN(outer)
  middle(a+b, b+c);
END
 
int main()
BEGIN(main)
  stack_trace.on = TRUE; /* turn on runtime tracing */
  outer(2,3,5);
  stack_trace.on = FALSE;
  RETURN(EXIT_SUCCESS);
END
```


{{out}}

```txt

stack_trace_test.c:19: BEGIN outer[0x80487b4], stack(depth:1, size:60)
stack_trace_test.c:14:   BEGIN middle[0x8048749], stack(depth:2, size:108)
stack_trace_test.c:8:     BEGIN inner[0x80486d8], stack(depth:3, size:156)
stack_trace_test.c:8:       *** Now dump the stack ***
stack_trace_test.c:8:   inner[0x80486d8]        --- stack(depth:4, size:156) ---
stack_trace_test.c:14:  middle[0x8048749]       --- stack(depth:3, size:108) ---
stack_trace_test.c:19:  outer[0x80487b4]        --- stack(depth:2, size:60) ---
stack_trace_test.c:24:  main[0x804882a] --- stack(depth:1, size:0) ---
stack_trace_test.c:8:       --- (depth 4) ---
stack_trace_test.c:8:     END inner[0x80486d8], stack(depth:3, size:156)
stack_trace_test.c:14:   END middle[0x8048749], stack(depth:2, size:108)
stack_trace_test.c:19: END outer[0x80487b4], stack(depth:1, size:60)

```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Diagnostics;

class Program
{
    static void Inner()
    {
        Console.WriteLine(new StackTrace());
    }

    static void Middle()
    {
        Inner();
    }

    static void Outer()
    {
        Middle();
    }

    static void Main()
    {
        Outer();
    }
}
```

Sample output:
<lang>at Program.Inner()
at Program.Middle()
at Program.Outer()
at Program.Main()
```



## Clojure

{{works with|Java|1.6+}}

[http://docs.oracle.com/javase/8/docs/api/java/lang/management/ThreadMXBean.html ThreadMXBean] can be used to show you the stack of all live threads. 


```clojure

(doall
  (map println (.dumpAllThreads (java.lang.management.ManagementFactory/getThreadMXBean) false false)))

```

{{out}}

```txt

#<ThreadInfo "nREPL-worker-26" Id=64 RUNNABLE
        at sun.management.ThreadImpl.dumpThreads0(Native Method)
        at sun.management.ThreadImpl.dumpAllThreads(ThreadImpl.java:446)
        at user$eval1285.invoke(form-init6675235431801252432.clj:1)
        at clojure.lang.Compiler.eval(Compiler.java:6619)
        at clojure.lang.Compiler.eval(Compiler.java:6582)
        at clojure.core$eval.invoke(core.clj:2852)
        at clojure.main$repl$read_eval_print__6588$fn__6591.invoke(main.clj:259)
        at clojure.main$repl$read_eval_print__6588.invoke(main.clj:259)
        ...

```



## Common Lisp


Stack trace facilities are not specified by the Common Lisp standard. Implementations vary widely in the amount of information provided, how it can be retrieved, and the amount of internal implementation detail included.

Here we use SWANK, which a component of SLIME, a Common Lisp IDE (it is the library loaded into the target Lisp system to enable interaction and remote debugging), to make use of its portable debugging facilities:


```lisp
(swank-backend:call-with-debugging-environment
  (lambda ()
    (swank:backtrace 0 nil)))
```


Here are a few lines of the result when running under [[SBCL]], the rest is omitted since it's long and boring:

```lisp
((0 "((LAMBDA (SWANK-BACKEND::DEBUGGER-LOOP-FN)) #<FUNCTION (LAMBDA #) {100459BBC9}>)")
 (1 "(SB-INT:SIMPLE-EVAL-IN-LEXENV (SWANK-BACKEND:CALL-WITH-DEBUGGING-ENVIRONMENT (LAMBDA () (SWANK:BACKTRACE 0 NIL))) #<NULL-LEXENV>)")
 (2 "(SWANK::EVAL-REGION \"(swank-backend:call-with-debugging-environment\\n            (lambda ()\\n              (swank:backtrace 0 nil)))\\n\\n\")")
 (3 "((LAMBDA ()))") ...)
```


Note that this is a data structure containing the backtrace, not a format intended for presentation. In [[SBCL]], executing <code>(sb-debug:backtrace 7)</code> produces output like this (run from the SLIME-REPL, which is why it still contains mentions of SWANK):


```lisp
CL-USER> (sb-debug:backtrace 7)
0: (SB-DEBUG::MAP-BACKTRACE
    #<CLOSURE (LAMBDA (SB-DEBUG::FRAME)) {1193EFCD}>)[:EXTERNAL]
1: (BACKTRACE
    7
    #<TWO-WAY-STREAM
      :INPUT-STREAM #<SWANK-BACKEND::SLIME-INPUT-STREAM {120F6519}>
      :OUTPUT-STREAM #<SWANK-BACKEND::SLIME-OUTPUT-STREAM {1208F3E1}>>)
2: (SB-INT:SIMPLE-EVAL-IN-LEXENV (BACKTRACE 7) #<NULL-LEXENV>)
3: (SWANK::EVAL-REGION
    "(sb-debug:backtrace 7)
")
4: ((LAMBDA ()))
5: (SWANK::TRACK-PACKAGE #<CLOSURE (LAMBDA ()) {1193ECBD}>)
6: (SWANK::CALL-WITH-RETRY-RESTART
    "Retry SLIME REPL evaluation request."
    #<CLOSURE (LAMBDA ()) {1193EC4D}>)
```


SBCL's backtraces consist entirely of lists of the form <code>(<var>function-name</var> <var>args...</var>)</code>.


## D

Compiled with the dmd compiler using the -g switch.
{{trans|Java}}

```d
import std.stdio, core.runtime;

void inner() { defaultTraceHandler.writeln; }
void middle() { inner; }
void outer() { middle; }

void main() {
    outer;
    "After the stack trace.".writeln;
}
```

{{out}}

```txt
0x00404FBE in core.sys.windows.stacktrace.StackTrace core.sys.windows.stacktrace.StackTrace.__ctor(uint, core.sys.windows.windows.CONTEXT*) at E:\dmd2\src\druntime\import\core\sys\windows\stacktrace.d(69)
0x00404ACB in object.Throwable.TraceInfo core.runtime.defaultTraceHandler(void*) at E:\dmd2\src\druntime\import\core\runtime.d(646)
0x0040201A in void test.inner() at E:\test.d(3)
0x0040202C in void test.middle() at E:\test.d(4)
0x00402038 in void test.outer() at E:\test.d(5)
0x00402044 in _Dmain at E:\test.d(9)
0x00409AAC in void rt.dmain2._d_run_main(int, char**, extern (C) int function(char[][])*).runAll().void __lambda1()
0x00409A7F in void rt.dmain2._d_run_main(int, char**, extern (C) int function(char[][])*).runAll()
0x00409997 in _d_run_main
0x004048D8 in main
0x0041FB2D in mainCRTStartup
0x76EED2E9 in BaseThreadInitThunk
0x77821603 in RtlInitializeExceptionChain
0x778215D6 in RtlInitializeExceptionChain

After the stack trace.
```



## DWScript

Stack traces can be obtained from exception objects

```delphi
procedure Inner;
begin
   try
      raise Exception.Create('');
   except
      on E: Exception do
         PrintLn(E.StackTrace);
   end;
end;

procedure Middle;
begin
   Inner;
end;

procedure Outer;
begin
   Middle;
end;

Outer;
```

Output:
```txt
Inner [line: 4, column: 23]
Middle [line: 13, column: 4]
Outer [line: 18, column: 4]
 [line: 21, column: 1]
```


## Elena

{{trans|C#}}
ELENA 4.0 :

```elena
import extensions;
 
public singleton program
{
    inner()
    {
        console.printLine(new CallStack())
    }
 
    middle()
    {
        self.inner()
    }
 
    outer()
    {
        self.middle()
    }
 
    // program entry point
    closure()
    {
        program.outer()
    }
}
```

{{out}}

```txt

mytest'program.inner[0]:test.l(7)
mytest'program.middle[0]:test.l(12)
mytest'program.outer[0]:test.l(17)
mytest'program.#invoke[0]:test.l(23)
system'$private'entry.#invoke[0]:win32_app.l(37)
system'#startUp:win32_app.l(55)

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Stack_traces do
  def main do
    {:ok, a} = outer
    IO.inspect a
  end
  
  defp outer do 
    {:ok, a} = middle
    {:ok, a}
  end 
  
  defp middle do
    {:ok, a} = inner
    {:ok, a}
  end
  
  defp inner do
    try do
      throw(42)
    catch 42 -> {:ok, :erlang.get_stacktrace}
    end
  end
end

Stack_traces.main
```


{{out}}

```txt

[{Stack_traces, :inner, 0, [file: 'stack_trace.exs', line: 19]},
 {Stack_traces, :middle, 0, [file: 'stack_trace.exs', line: 13]},
 {Stack_traces, :outer, 0, [file: 'stack_trace.exs', line: 8]},
 {Stack_traces, :main, 0, [file: 'stack_trace.exs', line: 3]},
 {:elixir_compiler, :dispatch_loaded, 6,
  [file: 'src/elixir_compiler.erl', line: 125]},
 {:elixir_lexical, :run, 3, [file: 'src/elixir_lexical.erl', line: 16]},
 {:elixir_compiler, :quoted, 3, [file: 'src/elixir_compiler.erl', line: 30]},
 {Code, :require_file, 2, [file: 'lib/code.ex', line: 363]}]

```



## Erlang

{{trans|Java}}
Stack traces only can be obtained inside a catch block. Additionally, it doesn't work for tail calls.

```erlang
-module(stack_traces).

-export([main/0]).

main() ->
	{ok,A} = outer(),
	io:format("~p\n", [A]).

outer() -> 
	{ok,A} = middle(),
	{ok,A}.

middle() -> 
	{ok,A} = inner(),
	{ok,A}.

inner() -> 
	try throw(42) catch 42 -> {ok,erlang:get_stacktrace()} end.
```

{{out}}

```txt
[{stack_traces,inner,0,[{file,"stack_traces.erl"},{line,18}]},
 {stack_traces,middle,0,[{file,"stack_traces.erl"},{line,14}]},
 {stack_traces,outer,0,[{file,"stack_traces.erl"},{line,10}]},
 {stack_traces,main,0,[{file,"stack_traces.erl"},{line,6}]},
 {init,start_it,1,[]},
 {init,start_em,1,[]}]
```


=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System.Diagnostics

type myClass() =
    member this.inner() = printfn "%A" (new StackTrace())
    member this.middle() = this.inner()
    member this.outer() = this.middle()

[<EntryPoint>]
let main args =
    let that = new myClass()
    that.outer()
    0
```

Output

```txt
   at Rosetta.myClass.inner()
   at Rosetta.myClass.middle()
   at Rosetta.myClass.outer()
   at Rosetta.main(String[] args)
```



## Factor

This stack trace shows the listener — Factor's Read-Eval-Print-Loop. The current execution point in each call frame is indicated by <code>=></code>. Press ctrl+w in the listener to walk through the call stack one step at a time and watch how it affects the data stack. You can even step backward in most cases. This is handy for debugging.

```factor
USE: prettyprint
get-callstack callstack.
```

{{out}}

```txt

(U)
Quotation: [ set-namestack init-catchstack self quot>> call => stop ]
(O)
Word: listener-thread
(O)
Word: listener
(O)
Word: (listener)
(O)
Word: listener-step
(U)
Quotation: [
    [ ~quotation~ dip swap ~quotation~ dip ] dip swap
    [ call get-datastack ] dip => swap [ set-datastack ] dip
]

```



## Forth

In Forth, calling sequence information is kept on the Return Stack. Some Forth compilers have the word "R.S" that dumps the contents of the Return Stack - just like ".S", which dumps the contents of the Data Stack. Note this may also include stack frames, local variables and temporary values. Forth has no way of knowing which is which, because that is usually left to the programmer.

{{works with|4tH|3.60.0}}

```forth
[UNDEFINED] R.S [IF]
\ Return stack counterpart of DEPTH
\ Note the STACK-CELLS correction is there to hide RDEPTH itself
                                       ( -- n)
: RDEPTH STACK-CELLS -2 [+] CELLS RP@ - ;

\ Return stack counterpart of .S
\ Note the : R.S R> .. >R ; sequence is there to hide R.S itself
                                       ( --)
: R.S R> CR RDEPTH DUP 0> IF DUP
  BEGIN DUP WHILE R> -ROT 1- REPEAT DROP DUP
  BEGIN DUP WHILE ROT DUP . >R 1- REPEAT DROP
  THEN ." (TORS) " DROP >R ;
[THEN]
```



## Fortran

Fortran provides nothing of the sort as a language feature. A given compiler may (possibly via an option) record information in the code file that when a fatal error such as divide-by-zero is caught, allows the error message to name the routine in which the error occurred, possibly identifying the source line and perhaps even the sequence of routines that were invoked to get there. Otherwise, a routine has no information whereby it might identify its caller - or for that matter, itself. Some compilers might make available special routines, but, there is no such requirement in the standard. Outside of Fortran's lack of standardised access, some systems such as TaskInfo on Windows can show a task's current call stack (updated each time TaskInfo re-samples, say every four seconds), possibly including the names of the called routines. Watching this being updated as the task runs a lengthy computation gives hints that otherwise could be provided via execution profiling and a lot more effort.

Otherwise, it is up to the programmer. For instance, on entry to ''every'' routine, <code>CALL SUBIN("''name''")</code> and on exit, ''without fail'', <code>CALL SUBOUT("''name''")</code> which routines maintain a ... stack of names, and further can count invocations, though perhaps not for heavily-used routines - trials suggest that around ten million SUBIN calls consume about a second of cpu time. Add to this the judicious use of a routine <code>CALL STATE("''activity description''")</code> similarly using a stack (the stack pointer being maintained by SUBIN/SUBOUT) not only provides interesting documentation in the source but also, should there be a disaster, <code>CALL CROAK("''dismayed message''")</code> can be used to end a run, routine CROAK of course displaying the current stack with at each level the declared activity message before the final STOP. A similar routine could be invoked to display the current stack state without a STOP as some sort of status report. With this protocol in place, the routine to present trace output can name its caller (and, the caller of that) to provide some context for the bewildering output.

Here for example is a run of such a programme, called Gnash (for New Zealand's national collection of half-hourly electricity data) that accepts certain commands and in particular a command "croak". A log of input and output is maintained, echoing what appears on the screen. Thus, the programme requests the next input after prompting "Gnash:" and that input is the command "croak" followed by some text...

```txt

Gnash: croak Life is troubled
Goodbye, cruel world!
Routine XeqACard croaks: Life is troubled
...from XeqACard Confronting croak Life is troubled
...from Attack   some input
...from Gnash    Gnash gnashing
Omitted exit from level 3:XeqACard
Omitted exit from level 2:Attack

```

So, Gnash has invoked ATTACK to deal with input commands, as if cards are being read. ATTACK states that it is dealing with some input (possibly from a script file but in this case the keyboard), and invokes XeqACard to do so. It states that it is confronting some text, then invokes the appropriate command handler - which invokes CROAK, which unwinds the stack. Its final act is <code>STOP "I STOP now. Farewell..."	!Whatever pit I was in, I'm gone.</code> which of course is not written to the log file and unless you're using a DOS-style session, the window will be closed before you can read it.

Should a fatal error be declared by the run-time system, none of this will happen, but if while running, some unrecoverable or a should-never-happen but I'll check anyway type of problem be noted, then a possibly informative output will follow.


## Go


```go
package main

import (
    "fmt"
    "runtime"
)

func main() {
    stackTrace := make([]byte, 1024)
    n := runtime.Stack(stackTrace, true)
    stackTrace = stackTrace[:n]
    fmt.Printf("%s\n", stackTrace)
    fmt.Printf("(%d bytes)\n", len(stackTrace))
}
```

outputs:

```txt

goroutine 16 [running]:
main.main()
	/tmpfs/gosandbox-efa5a722_5cfcad46_bebc14b4_0486dec5_bd9e34fc/prog.go:10 +0xa0

goroutine 19 [runnable]:
runfinq()
	/tmp/sandbox/go/src/pkg/runtime/mgc0.c:2606
runtime.goexit()
	/tmp/sandbox/go/src/pkg/runtime/proc.c:1445

(259 bytes)

```



## Groovy

Solution:

```groovy
def rawTrace = { Thread.currentThread().stackTrace }
```


Test: (demonstrates, among other things, continued execution after generating stack trace)

```groovy
def trace = rawTrace().collect {
    def props = it.properties
    def keys = (it.properties.keySet() - (new Object().properties.keySet()))
    props.findAll{ k, v -> k in keys }
}

def propNames = trace[0].keySet().sort()
def propWidths = propNames.collect { name -> [name, trace.collect{ it[name].toString() }].flatten()*.size().max() }

propNames.eachWithIndex{ name, i -> printf("%-${propWidths[i]}s  ", name) }; println ''
propWidths.each{ width -> print('-' * width + '  ') }; println ''
trace.each {
    propNames.eachWithIndex{ name, i -> printf("%-${propWidths[i]}s  ", it[name].toString()) }; println ''
}
```


Output:
<pre style="height:45ex;overflow:scroll;">className                                                          fileName                           lineNumber  methodName                       nativeMethod  
-----------------------------------------------------------------  ---------------------------------  ----------  -------------------------------  ------------  
java.lang.Thread                                                   Thread.java                        1479        getStackTrace                    false         
sun.reflect.NativeMethodAccessorImpl                               NativeMethodAccessorImpl.java      -2          invoke0                          true          
sun.reflect.NativeMethodAccessorImpl                               NativeMethodAccessorImpl.java      39          invoke                           false         
sun.reflect.DelegatingMethodAccessorImpl                           DelegatingMethodAccessorImpl.java  25          invoke                           false         
java.lang.reflect.Method                                           Method.java                        597         invoke                           false         
org.codehaus.groovy.reflection.CachedMethod                        CachedMethod.java                  90          invoke                           false         
groovy.lang.MetaMethod                                             MetaMethod.java                    233         doMethodInvoke                   false         
groovy.lang.MetaClassImpl$GetBeanMethodMetaProperty                MetaClassImpl.java                 3465        getProperty                      false         
org.codehaus.groovy.runtime.callsite.GetEffectivePojoPropertySite  GetEffectivePojoPropertySite.java  61          getProperty                      false         
org.codehaus.groovy.runtime.callsite.AbstractCallSite              AbstractCallSite.java              227         callGetProperty                  false         
ConsoleScript38$_run_closure1                                      ConsoleScript38                    1           doCall                           false         
sun.reflect.NativeMethodAccessorImpl                               NativeMethodAccessorImpl.java      -2          invoke0                          true          
sun.reflect.NativeMethodAccessorImpl                               NativeMethodAccessorImpl.java      39          invoke                           false         
sun.reflect.DelegatingMethodAccessorImpl                           DelegatingMethodAccessorImpl.java  25          invoke                           false         
java.lang.reflect.Method                                           Method.java                        597         invoke                           false         
org.codehaus.groovy.reflection.CachedMethod                        CachedMethod.java                  90          invoke                           false         
groovy.lang.MetaMethod                                             MetaMethod.java                    233         doMethodInvoke                   false         
org.codehaus.groovy.runtime.metaclass.ClosureMetaClass             ClosureMetaClass.java              272         invokeMethod                     false         
groovy.lang.MetaClassImpl                                          MetaClassImpl.java                 885         invokeMethod                     false         
org.codehaus.groovy.runtime.callsite.PogoMetaClassSite             PogoMetaClassSite.java             66          callCurrent                      false         
org.codehaus.groovy.runtime.callsite.CallSiteArray                 CallSiteArray.java                 46          defaultCallCurrent               false         
org.codehaus.groovy.runtime.callsite.AbstractCallSite              AbstractCallSite.java              133         callCurrent                      false         
org.codehaus.groovy.runtime.callsite.AbstractCallSite              AbstractCallSite.java              141         callCurrent                      false         
ConsoleScript38$_run_closure1                                      ConsoleScript38                    -1          doCall                           false         
sun.reflect.NativeMethodAccessorImpl                               NativeMethodAccessorImpl.java      -2          invoke0                          true          
sun.reflect.NativeMethodAccessorImpl                               NativeMethodAccessorImpl.java      39          invoke                           false         
sun.reflect.DelegatingMethodAccessorImpl                           DelegatingMethodAccessorImpl.java  25          invoke                           false         
java.lang.reflect.Method                                           Method.java                        597         invoke                           false         
org.codehaus.groovy.reflection.CachedMethod                        CachedMethod.java                  90          invoke                           false         
groovy.lang.MetaMethod                                             MetaMethod.java                    233         doMethodInvoke                   false         
org.codehaus.groovy.runtime.metaclass.ClosureMetaClass             ClosureMetaClass.java              272         invokeMethod                     false         
groovy.lang.MetaClassImpl                                          MetaClassImpl.java                 885         invokeMethod                     false         
org.codehaus.groovy.runtime.callsite.PogoMetaClassSite             PogoMetaClassSite.java             39          call                             false         
org.codehaus.groovy.runtime.callsite.CallSiteArray                 CallSiteArray.java                 42          defaultCall                      false         
org.codehaus.groovy.runtime.callsite.AbstractCallSite              AbstractCallSite.java              108         call                             false         
org.codehaus.groovy.runtime.callsite.AbstractCallSite              AbstractCallSite.java              112         call                             false         
ConsoleScript38                                                    ConsoleScript38                    3           run                              false         
groovy.lang.GroovyShell                                            GroovyShell.java                   266         runScriptOrMainOrTestOrRunnable  false         
groovy.lang.GroovyShell                                            GroovyShell.java                   517         run                              false         
groovy.lang.GroovyShell                                            GroovyShell.java                   172         run                              false         
groovy.lang.GroovyShell$run                                        null                               -1          call                             false         
groovy.ui.Console$_runScriptImpl_closure16                         Console.groovy                     910         doCall                           false         
sun.reflect.GeneratedMethodAccessor232                             null                               -1          invoke                           false         
sun.reflect.DelegatingMethodAccessorImpl                           DelegatingMethodAccessorImpl.java  25          invoke                           false         
java.lang.reflect.Method                                           Method.java                        597         invoke                           false         
org.codehaus.groovy.reflection.CachedMethod                        CachedMethod.java                  90          invoke                           false         
groovy.lang.MetaMethod                                             MetaMethod.java                    233         doMethodInvoke                   false         
org.codehaus.groovy.runtime.metaclass.ClosureMetaClass             ClosureMetaClass.java              272         invokeMethod                     false         
groovy.lang.MetaClassImpl                                          MetaClassImpl.java                 885         invokeMethod                     false         
org.codehaus.groovy.runtime.callsite.PogoMetaClassSite             PogoMetaClassSite.java             66          callCurrent                      false         
org.codehaus.groovy.runtime.callsite.AbstractCallSite              AbstractCallSite.java              141         callCurrent                      false         
groovy.ui.Console$_runScriptImpl_closure16                         Console.groovy                     -1          doCall                           false         
sun.reflect.GeneratedMethodAccessor231                             null                               -1          invoke                           false         
sun.reflect.DelegatingMethodAccessorImpl                           DelegatingMethodAccessorImpl.java  25          invoke                           false         
java.lang.reflect.Method                                           Method.java                        597         invoke                           false         
org.codehaus.groovy.reflection.CachedMethod                        CachedMethod.java                  90          invoke                           false         
groovy.lang.MetaMethod                                             MetaMethod.java                    233         doMethodInvoke                   false         
org.codehaus.groovy.runtime.metaclass.ClosureMetaClass             ClosureMetaClass.java              272         invokeMethod                     false         
groovy.lang.MetaClassImpl                                          MetaClassImpl.java                 885         invokeMethod                     false         
groovy.lang.Closure                                                Closure.java                       405         call                             false         
groovy.lang.Closure                                                Closure.java                       399         call                             false         
groovy.lang.Closure                                                Closure.java                       483         run                              false         
java.lang.Thread                                                   Thread.java                        662         run                              false
```


==Icon and {{header|Unicon}}==
This Icon solution uses Unicon extensions. An Icon only version has not been provided.

{{libheader|Unicon Code Library}}
[http://tapestry.tucson.az.us/unilib/pack_Utils.html the following code for buildStackTrace in Utils is taken verbatim and shown below the main program]

```Unicon

import Utils  # for buildStackTrace

procedure main()
   g()
   write()
   f()
end

procedure f()
   g()
end

procedure g()
   # Using 1 as argument omits the trace of buildStackTrace itself
   every write("\t",!buildStackTrace(1))
end
```



```Unicon>#<p

#  Compute the current stack trace.  Starting at level <i>n</i> above
#  the current procedure.  Here, <i>n</i> defaults to 0, which will
#  include this procedure in the stack trace.
#  <i>ce</i> defaults to &current.
#  <i>This only works with newer versions of Unicon!</i>
#  <[generates the stacktrace from current call back to first
#   in the co-expression]>
#</p>
procedure buildStackTrace(n:0,  # starting distance from this call
                          ce    # co-expr to trace stack in [&current]
                         )
    local L
    /ce := &current
    L := []; n -:= 1
    while pName := image(proc(ce, n+:=1)) do {
        fName := keyword("&file",ce,n) | "no file name"
        fLine := keyword("&line",ce,n) | "no line number"
        put(L, pName||" ["||fName||":"||fLine||"]" )
        }
    return L
end
```


The output of this example is:

```txt

        procedure g [Stacktrace.icn:13]
        procedure main [Stacktrace.icn:2]

        procedure g [Stacktrace.icn:13]
        procedure f [Stacktrace.icn:8]
        procedure main [Stacktrace.icn:4]

```



## J


J's stack can be accessed only when suspension has been enabled.  When suspension has not been enabled, break points will not work, errors will bubble out to the top level, and the stack data structure will not be available.

To enable suspension and record subsequent stack frames:

```j
   13!:0]1
```


To retrieve a current stack trace:

```j
   13!:13''
```


See also: http://www.jsoftware.com/help/dictionary/dx013.htm

Example:


```j
   f=:g
   g=:13!:13 bind ''
   f 7    NB. empty stack trace because debugging has not been enabled
   13!:0]1
   f 7
┌─┬─┬─┬─┬─────────────┬┬───┬──┬─┐
│g│0│0│3│13!:13@(''"_)││┌─┐│  │ │
│ │ │ │ │             │││7││  │ │
│ │ │ │ │             ││└─┘│  │ │
├─┼─┼─┼─┼─────────────┼┼───┼──┼─┤
│f│0│0│3│g            ││┌─┐│  │ │
│ │ │ │ │             │││7││  │ │
│ │ │ │ │             ││└─┘│  │ │
└─┴─┴─┴─┴─────────────┴┴───┴──┴─┘
```


Technical note: the stack trace is not displayed, here, until after the stack has been discarded.  This is because we have returned the stack trace as a result and relied on J's implicit display of the result of an expression to display the stack trace.


## Java

{{works with|Java|1.5+}}

```java5
public class StackTracer {
    public static void printStackTrace() {
	StackTraceElement[] elems = Thread.currentThread().getStackTrace();

	System.out.println("Stack trace:");
	for (int i = elems.length-1, j = 2 ; i >= 3 ; i--, j+=2) {
	    System.out.printf("%" + j + "s%s.%s%n", "",
		    elems[i].getClassName(), elems[i].getMethodName());
	}
    }
}
```

Demonstration code:

```java5
public class StackTraceDemo {
    static void inner() {
	StackTracer.printStackTrace();
    }
    static void middle() {
	inner();
    }
    static void outer() {
	middle();
    }
    public static void main(String[] args) {
	outer();
    }
}
```

Output:

```txt
Stack trace:
  StackTraceDemo.main
    StackTraceDemo.outer
      StackTraceDemo.middle
        StackTraceDemo.inner
```



## JavaScript

There is no standard way to do this, but some implementations provide it.<br />
{{works with|Firefox}}

```javascript
try {
  throw new Error;
} catch(e) {
  alert(e.stack);
}
```


The following version works in many browsers but it infinitely loops when there is recursion:

```javascript
function foo () {
  var stack = "Stack trace:";
  for (var f = arguments.callee // current function
       ; f; f = f.caller) {
    stack += "\n" + f.name;
  }
  alert(stack);
}
foo();
```



## Julia

{{works with|Julia|0.6}}


```julia
f() = g()
g() = println.(stacktrace())

f()
```


{{out}}

```txt
g() at Stack_traces.jl:5
f() at Stack_traces.jl:4
include_string(::String, ::String) at loading.jl:515
include_string(::Module, ::String, ::String) at Compat.jl:464
(::Atom.##57#60{String,String})() at eval.jl:74
withpath(::Atom.##57#60{String,String}, ::String) at utils.jl:30
withpath(::Function, ::String) at eval.jl:38
macro expansion at eval.jl:72 [inlined]
(::Atom.##56#59{Dict{String,Any}})() at task.jl:80
```



## Kotlin


```scala
// version 1.1.2 (stacktrace.kt which compiles to StacktraceKt.class)

fun myFunc() {
    println(Throwable().stackTrace.joinToString("\n"))
}

fun main(args:Array<String>) {
    myFunc()
    println("\nContinuing ... ")
}
```


{{out}}

```txt

StacktraceKt.myFunc(stacktrace.kt:4)
StacktraceKt.main(stacktrace.kt:8)

Continuing ...

```



## Lasso

By default Lasso tracks the file path, line and column numbers. You can create a trace method to track type and method names illustrated below or use one of the public libraries like L-Debug [https://github.com/zeroloop/l-debug].


```Lasso
// Define our own trace method
define trace => {
    local(gb) = givenblock
    
    // Set a depth counter
    var(::_tracedepth)->isnota(::integer) ? $_tracedepth = 0
    handle => {$_tracedepth--}
    
    // Only output when supplied a capture
    #gb ? stdoutnl(
        // Indent
        ('\t' * $_tracedepth++) +                     
        
        // Type + Method
        #gb->self->type + '.' + #gb->calledname +     
        
        // Call site file
        ': ' + #gb->home->callsite_file +             
        
        // Line number and column number
        ' (line '+#gb->home->callsite_line + ', col ' + #gb->home->callsite_col +')'
    )
    return #gb()
}
```


;Use Trace:


```Lasso>define stackexample =
 type {
    public oncreate => trace => { return self }
    public inner => trace => { }
    public middle => trace => { .inner }
    public outer => trace => { .middle }
}

stackexample->outer
```


{{out}}


```txt
stackexample.oncreate: adminapp_lasso_runner_thread (line 2, col 24)
stackexample.outer: adminapp_lasso_runner_thread (line 5, col 21)
	stackexample.middle: adminapp_lasso_runner_thread (line 4, col 22)
		stackexample.inner: adminapp_lasso_runner_thread (line 3, col 21)
```



## Lua


```lua
function Inner( k )
    print( debug.traceback() )
    print "Program continues..."
end

function Middle( x, y )
    Inner( x+y )
end

function Outer( a, b, c )
    Middle( a*b, c )
end

Outer( 2, 3, 5 )
```


```txt
stack traceback:
	./prog:4: in function 'Inner'
	./prog:9: in function 'Middle'
	./prog:13: in function 'Outer'
	./prog:16: in main chunk
	[C]: ?
Program continues...
```



## Mathematica

Built-in function Stack does the task, example I:

```Mathematica
f[g[1, Print[Stack[]]; 2]]
```

prints, gives back:

```Mathematica
{f,g,CompoundExpression,Print}
f[g[1, 2]]
```

Example II:

```Mathematica
f[g[1, Print[Stack[_]]; 2]]
```

prints, gives back:

```Mathematica
{f[g[1,Print[Stack[_]];2]],g[1,Print[Stack[_]];2],Print[Stack[_]];2,Print[Stack[_]]}
f[g[1, 2]]
```

Related and similar functions are: Trace, TracePrint, TraceScan,TraceDialog, Monitor, StackInhibit, StackBegin, StackComplete. In the manual look for 'guide/SymbolicExecutionHistory'.


## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

class RStackTraces
  method inner() static
    StackTracer.printStackTrace()
  method middle() static
    inner()
  method outer() static
    middle()
  method main(args = String[]) public static
    outer()

class RStackTraces.StackTracer
  method printStackTrace() public static
    elems = Thread.currentThread().getStackTrace()
    say 'Stack trace:'
    j_ = 2
    loop i_ = elems.length - 1 to 2 by -1
      say ''.left(j_) || elems[i_].getClassName()'.'elems[i_].getMethodName()
      j_ = j_ + 2
      end i_

```

'''Output:'''

```txt

Stack trace:
  RStackTraces.main
    RStackTraces.outer
      RStackTraces.middle
        RStackTraces.inner

```


=={{header|Objective-C}}==

```objc>#include <execinfo.h


void *frames[128];
int len = backtrace(frames, 128);
char **symbols = backtrace_symbols(frames, len);
for (int i = 0; i < len; ++i) {
    NSLog(@"%s", symbols[i]);
}
free(symbols);
```


Or in Mac OS X 10.6+:

```objc
NSArray *symbols = [NSThread callStackSymbols];
for (NSString *symbol in symbols) {
    NSLog(@"%@", symbol);
}
```



## OCaml

{{works with|OCaml|3.11+}}

```ocaml
let div a b = a / b

let () =
  try let _ = div 3 0 in ()
  with e ->
    prerr_endline(Printexc.to_string e);
    Printexc.print_backtrace stderr;
;;
```


outputs:
 Division_by_zero
 Raised by primitive operation at file "test.ml", line 4, characters 14-21


###  By Environment Variable 

Another way is to set the environment variable '''OCAMLRUNPARAM''' to '''b''', for example you can add in your ~/.bashrc file this line:
 export OCAMLRUNPARAM='b'

Then the code doesn't need additional statements:

```ocaml
let div a b = a / b

let () =
  let _ = div 3 0 in ()
;;
```

outputs:
 Fatal error: exception Division_by_zero
 Raised at file "test.ml", line 4, characters 10-17


## Oforth


Stack trace is only available : 

1) When an exception is raised

2) And when oforth debug mode is set, using --D command line option.

Otherwise no stack trace is available nor printed.


```Oforth
: f1   Exception throw("An exception") ;
Integer method: f2  self f1 ;
: f3   f2 ;
: f4   f3 ;

10 f4
```


{{out}}

```txt

[1:interpreter] Exception : An exception
Into Method   #throw      self = (Exception) An exception
Into Method   #throw      self = (Class) Exception
Into Function #f1
Into Method   #f2         self = (Integer) 10
Into Function #f3

```



## OxygenBasic


```oxygenbasic

'32bit x86

static string Report


macro ReportStack(n)
'
### =============


  '
  scope
  '
  static sys stack[0X100],stackptr,e
  '
  'CAPTURE IMAGE OF UP TO 256 ENTRIES
  '
  '
  mov eax,n
  cmp eax,0x100
  (
    jle exit
    mov eax,0x100 'UPPER LIMIT
  )
  mov e,eax
  mov stackptr,esp
  lea edx,stack
  mov ecx,e
  mov esi,esp
  (
    mov eax,[esi]
    mov [edx],eax
    add esi,4
    add edx,4
    dec ecx
    jg repeat
  )
  sys i
  string cr=chr(13)+chr(10), tab=chr(9)
  '
  for i=1 to e
    report+=hex(stackptr+(i-1)*4,8) tab hex(i-1,2) tab hex(stack[i],8) cr
  next
  '
  end scope
  '
end macro

'====
'TEST
'====

  function foo()
  '
### =======


  push 0x44556677
  push 0x33445566
  push 0x22334455
  push 0x11223344
  ReportStack(8)

  end function

Report+="Trace inside foo
"
foo()
print report
'putfile "s.txt",Report

/*
RESULT:

Trace inside foo
0017FE00	00	11223344
0017FE04	01	22334455
0017FE08	02	33445566
0017FE0C	03	44556677
0017FE10	04	005EAB1C
0017FE14	05	0017FE40
0017FE18	06	10002D5F
0017FE1C	07	00000000
*/

```



## Oz

System exceptions contain the current stack at the nested feature <code>debug.stack</code>. For example:

```oz
declare
  proc {Test}
     _ = 1 div 0
  end
in
  try
     {Test}
  catch E then
     {Inspect E}
  end
```

Output:
[[File:Oz_stacktrace1.png|center|Stack trace of a system exception in Oz.]]

To have such a stack trace in custom exceptions, either indicate this by throwing a record value with a <code>debug:unit</code> feature or use the [http://www.mozart-oz.org/documentation/base/exception.html Exception] module to create exceptions.

To access the stack trace directly, you can use the undocumented internal Debug module. Its <code>getTaskStack</code> function takes a thread, a depth value and a boolean "verbose" flag. It returns a list of stack frames. Example:

```oz
%% make sure that simple function calls are not optimized away
\switch +controlflowinfo

declare
  [Debug] = {Link ['x-oz://boot/Debug']}

  proc {F} {G} end

  proc {G} {H} end

  proc {H}
     {Inspect {Debug.getTaskStack {Thread.this} 100 true}}
  end
in
   {F}
```


Output:
[[File:Oz_stacktrace2.png|center|Stack trace created by the Debug module.]]


## Nim

In (normal) debug builds stacktraces are enabled, while in release builds stacktraces are disabled by default, but can be enabled like this: <code>nim c -d:release --stacktrace:on --linetrace:on file.nim</code>

```nim
proc g() =
  writeStackTrace()
proc f() =
  g()

f()
```



## Perl


```perl
use Carp 'cluck';

sub g {cluck 'Hello from &g';}
sub f {g;}

f;
```


This prints:


```txt
Hello from &g at Print a Stack Trace line 3
	main::g() called at Print a Stack Trace line 4
	main::f() called at Print a Stack Trace line 6
```

  

## Perl 6

{{works with|Rakudo|2013-03-03}}

```perl6
sub g { say Backtrace.new.concise }
sub f { g }
sub MAIN { f }
```

{{out}}

```txt
  in sub g at bt:1
  in sub f at bt:2
  in sub MAIN at bt:3
```



## Phix

There no standard method of obtaining a stack trace mid-run (as yet, non-fatally that is), but we can quickly cobble something together:

```Phix
constant W = machine_word(),
         {RTN,PREVEBP} = iff(W=4?{8,20}:{16,40})

procedure show_stack()
sequence symtab, symtabN
integer rtn
atom prev_ebp

    #ilASM{
        [32]
            lea edi,[symtab]
            call :%opGetST      -- [edi]=symtab (ie our local:=the real symtab)
            mov edi,[ebp+20]    -- prev_ebp
            mov eax,[edi+8]     -- calling routine no
            mov [rtn],eax
            mov eax,edi
            lea edi,[prev_ebp]
            call :%pStoreMint
        [64]
            lea rdi,[symtab]
            call :%opGetST      -- [rdi]=symtab (ie our local:=the real symtab)
            mov rdi,[rbp+40]    -- prev_ebp
            mov rax,[rdi+16]    -- calling routine no
            mov [rtn],rax
            mov rax,rdi
            lea rdi,[prev_ebp]
            call :%pStoreMint
        []
          }
    while rtn!=21 do -- (T_maintls, main top level routine, always present)
        symtabN = symtab[rtn]
        ?symtabN[1]
        prev_ebp = peekNS(prev_ebp+PREVEBP,W,0)
        rtn = peekNS(prev_ebp+RTN,W,0)
    end while
end procedure

procedure three(bool die)
    if die then
        ?9/0
    else
        show_stack()
    end if
end procedure

procedure two(bool die)
    three(die)
end procedure

procedure one(bool die)
    two(die)
end procedure

one(0)
?routine_id("dummy")    -- see note below
one(1)
```

During compilation, the symbol table entries hold an integer ternary tree index rather than a string name, and normally 
things are left like that during interpretation (the proper string names are always written out when an executable is
created) unless and until a fatal error occurs, or the compiler has evidence that they might be needed, such as that 
unresolved routine_id("dummy"). The -1 in the output below is that call failing to find any such routine.
{{Out}}

```txt

"three"
"two"
"one"
-1

C:\Program Files (x86)\Phix\e01.exw:59 in procedure three()
attempt to divide by 0
    die = 1
... called from C:\Program Files (x86)\Phix\e01.exw:66 in procedure two()
    die = 1
... called from C:\Program Files (x86)\Phix\e01.exw:70 in procedure one()
    die = 1
... called from C:\Program Files (x86)\Phix\e01.exw:75

Global & Local Variables

--> see C:\Program Files (x86)\Phix\ex.err
Press Enter...

```

The first half (up to the -1) is the output from show_stack_trace(), the rest is standard fatal error diagnostics.

For more details of how the latter is actually produced, refer to builtins\VM\pDiagN.e - in particular the conversion of
raw addresses into source code line numbers is not trivial (and not worth trying to replicate here). In fact I didn't just
type all that #ilASM gunk above in, but instead copied it from other source files in builtins\VM.

Alternatively, but only when interpreting (whereas the above works as shown both when interpreting and pre-compiled), the debugger is
started with the much saner

```Phix
trace(1)
```

though as yet it offers no means of examining the stack trace (resume-ably), the above suggests it should be relative easy to add.


## PHP


```php
<?php
class StackTraceDemo {
    static function inner() {
        debug_print_backtrace();
    }
    static function middle() {
        self::inner();
    }
    static function outer() {
        self::middle();
    }
}

StackTraceDemo::outer();
?>
```



```txt
#0  StackTraceDemo::inner() called at [/home/cweiske/Dev/cvs/test/php-stacktrace.php:7]
#1  StackTraceDemo::middle() called at [/home/cweiske/Dev/cvs/test/php-stacktrace.php:10]
#2  StackTraceDemo::outer() called at [/home/cweiske/Dev/cvs/test/php-stacktrace.php:14]
```



## PL/I


```PL/I

/* The SNAP option in the ON statement is sufficient to obtain */
/* a traceback.  The SYSTEM option specifies that standard     */
/* system action is to occur, which resume execution after the */
/* SIGNAL statement.                                           */
on condition(traceback) snap system;
...
signal condition(traceback);

```



## PicoLisp

PicoLisp doesn't keep full backtrace information at runtime. This is for performance reasons. However, existing variable bindings (environments) can be inspected with the '[http://software-lab.de/doc/refE.html#env env]' function, so this can be used to build your own stack frames.

The following is analog to (though simpler than) the built-in '[http://software-lab.de/doc/refT.html#trace trace]' mechanism. The function '$$' (corresponds to '[http://software-lab.de/doc/ref_.html#$ $]' for tracing) is inserted by 'stackAll' into every function and method definition (corresponds to '[http://software-lab.de/doc/refT.html#traceAll traceAll]'). Then, when stopping at a '[http://software-lab.de/doc/refD.html#debug debug]' breakpoint or an error handler, 'dumpStack' can be used to inspect the stack contents.

As this mechanism uses 'let' to hold the stack frames, it is robust also across catch/throw, coroutines and error handling.

```PicoLisp
(off "Stack")

(de $$ "Prg"
   (let "Stack" (cons (cons (car "Prg") (env)) "Stack")  # Build stack frame
      (set "Stack"
         (delq (asoq '"Stack" (car "Stack"))  # Remove self-created entries
            (delq (asoq '"Prg" (car "Stack"))
               (car "Stack") ) ) )
      (run (cdr "Prg")) ) )  # Run body

(de stackAll (Excl)
   (let *Dbg NIL
      (for "X" (all)
         (or
            (memq "X" Excl)
            (memq "X" '($$ @ @@ @@@))
            (= `(char "*") (char "X"))
            (cond
               ((= `(char "+") (char "X"))
                  (for "Y" (pair (val "X"))
                     (and
                        (pair "Y")
                        (fun? (cdr "Y"))
                        (unless (== '$$ (caaddr "Y"))
                           (con (cdr "Y")
                              (list
                                 (cons '$$ (cons (car "Y" "X") (cddr "Y"))) ) ) ) ) ) )
               ((pair (getd "X"))
                  (let "Y" @
                     (unless (== '$$ (caadr "Y"))
                        (con "Y"
                           (list (cons '$$ "X" (cdr "Y"))) ) ) ) ) ) ) ) ) )

(de dumpStack ()
   (more (reverse (cdr "Stack")))
   T )
```

Test:

```PicoLisp
(de foo (A B)
   (let C 3
      (bar (inc 'A) (inc 'B) (inc 'C)) ) )

(de bar (A D E)
   (let (A 7  B 8  C 9)
      (! println A B C) ) )  # Set a breakpoint before (println A B C)

(stackAll)
```


```txt
: (foo 1 2)  # Call 'foo'
(println A B C)  # Stopped at breakpoint in 'bar'
! (dumpStack)  # Dump stack history
(foo (A . 1) (B . 2) (@ . T))  # Hit <enter> on each line to continue
(bar (B . 3) (C . 4) (A . 2) (D . 3) (E . 4) (@ . T))
-> T
!  # Hit <enter> to continue execution
7 8 9  # Output of (println A B C)
-> 9
:
```



## PureBasic

The [http://www.purebasic.com/documentation/debugger/showcallstack.html ShowCallstack()]command opens a interactive display allowing viewing of the procedures in the calling path an all their local variables.

```PureBasic
Procedure Three()
  a=7
  ShowCallstack()
  CallDebugger
EndProcedure

Procedure Two()
  a=4
  Three()
EndProcedure

Procedure One()
  a=2
  Two()
EndProcedure

One()
```



## Python


See the [http://docs.python.org/library/traceback.html traceback] module

```python
import traceback

def f(): return g()
def g(): traceback.print_stack()

f()
```


Sample output from a session in the Idle IDE:

```txt
  File "<string>", line 1, in <module>
  File "C:\Python26\lib\idlelib\run.py", line 93, in main
    ret = method(*args, **kwargs)
  File "C:\Python26\lib\idlelib\run.py", line 293, in runcode
    exec code in self.locals
  File "C:/Documents and Settings/All Users/Documents/Paddys/traceback.py", line 6, in <module>
    f()
  File "C:/Documents and Settings/All Users/Documents/Paddys/traceback.py", line 3, in f
    def f(): return g()
  File "C:/Documents and Settings/All Users/Documents/Paddys/traceback.py", line 4, in g
    def g(): traceback.print_stack()

```



## R


```R
foo <- function()
{   
   bar <- function()
   {
     sys.calls()
   }  
   bar()
}

foo()
```

 <nowiki>[[1]]</nowiki>
 foo()
 <nowiki>[[2]]</nowiki>
 bar()
traceback() returns the callstack of the last unhandled (i.e. not in try/catch) error.

You can also see the stack trace when a function is called (or as it exits) using the trace function.

```R
trace("foo", recover)
foo()
```


```txt

Tracing foo() on entry 

Enter a frame number, or 0 to exit   

1: foo()

Selection: 

```



## REXX

{{works with|Regina}}

```rexx
/* call stack */
say 'Call A'
call A '123'
say result
exit 0

A:
say 'Call B'
call B '456'
say result
return ARG(1)

B:
say 'Call C'
call C '789'
say result
return ARG(1)

C:
call callstack
return ARG(1)

callstack: procedure
getcallstack(cs.)
say 'Dump call stack with' cs.0 'items'
do i = 1 to cs.0
    parse var cs.i line func
    say format(line, 3) ':' left(func, 9) ': source "' || sourceline(line) || '"'
end
return cs.0
```


{{out}}

```txt
prompt$ regina callstack.rexx
Call A
Call B
Call C
Dump call stack with 4 items
 20 : CALLSTACK : source "call callstack"
 15 : C         : source "call C '789'"
  9 : B         : source "call B '456'"
  3 : A         : source "call A '123'"
789
456
123
```


Regina will also dump a call stack during certain error conditions.  For instance, if the code listing above raised an unhandled signal (simulated here with "signal noname" in routine C).  This is not a recoverable scenario though, and the program will not continue.


```rexx
C:
signal noname
call callstack
return ARG(1)
```


{{out}}

```txt
prompt$ regina callstack.rexx
Call A
Call B
Call C
    20 +++          signal noname
    15 +++       call C '789'
     9 +++    call B '456'
     3 +++ call A '123'
Error 16 running "/demos/callstack.rexx", line 20: Label not found
Error 16.1: Label "NONAME" not found
```



## Racket


```Racket

#lang racket

;; To see these calls we do two things: mutate the binding to prevent
;; Racket from inlining the value; use a (void) call at the end so the
;; calls are not tail calls (which will otherwise not show on the
;; stack).
(define foo #f)
(set! foo (λ() (bar) (void)))
(define bar #f)
(set! bar (λ() (show-stacktrace) (void)))

(define (show-stacktrace)
  (for ([s (continuation-mark-set->context (current-continuation-marks))]
        [i (in-naturals)])
    ;; show just the names, not the full source information
    (when (car s) (printf "~s: ~s\n" i (car s)))))
(foo)

```


Output:

```txt

0: show-stacktrace
1: bar
2: foo
3: |[running body]|

```



## Raven


```Raven
 [1 2 3 4] 42 { 'a' 1 'b' 2 'c' 3 } 34.1234 ( -1 -2 -3 )  "The quick brown fox" FILE dump
```

{{out}}

```txt
hash (4 items)
  usage => 1
  index => 65836
  flags => 256
 buffer => " 98252f8  73 74 64 69 6e 00                                stdin."
The quick brown fox
28.1234
hash (3 items)
 a => 1
 b => 2
 c => 3
42
list (4 items)
 0 => 1
 1 => 2
 2 => 3
 3 => 4
```



## Ruby


```ruby
def outer(a,b,c)
  middle a+b, b+c
end

def middle(d,e)
  inner d+e
end

def inner(f)
  puts caller(0)
  puts "continuing... my arg is #{f}"
end

outer 2,3,5
```


```txt
$ ruby stacktrace.rb
stacktrace.rb:10:in `inner'
stacktrace.rb:6:in `middle'
stacktrace.rb:2:in `outer'
stacktrace.rb:14
continuing... my arg is 13
```


Exceptions caught in a rescue clause contain the trace information:

```ruby
def outer(a,b,c)
  middle a+b, b+c
end

def middle(d,e)
  inner d+e
end

def inner(f)
  raise
  puts "this will not be printed"
end

begin
  outer 2,3,5
rescue Exception => e
  puts e.backtrace
end
puts "continuing after the rescue..."
```


```txt
stacktrace.rb:10:in `inner'
stacktrace.rb:6:in `middle'
stacktrace.rb:2:in `outer'
stacktrace.rb:15
continuing after the rescue...
```

Thread has a backtrace method:

```ruby>p Thread.current.backtrace</lang



## Scala

While the code on the Java example works with Scala too, the code below is an alternative. Which, by
the way, could be used from Java as well, with minor modifications.


```scala
def callStack = try { error("exception") } catch { case ex => ex.getStackTrace drop 2 }

def printStackTrace = callStack drop 1 /* don't print ourselves! */ foreach println
```


Usage example:


```txt

scala> def f1 = printStackTrace
f1: Unit

scala> def f2 = f1
f2: Unit

scala> def f3 = f2
f3: Unit

scala> f3
line40$object$$iw$$iw$.f1(<console>:6)
line41$object$$iw$$iw$.f2(<console>:7)
line42$object$$iw$$iw$.f3(<console>:8)
line43$object$$iw$$iw$.<init>(<console>:10)
line43$object$$iw$$iw$.<clinit>(<console>)
RequestResult$line43$object$.<init>(<console>:4)
RequestResult$line43$object$.<clinit>(<console>)
RequestResult$line43$object.result(<console>)
sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
java.lang.reflect.Method.invoke(Method.java:597)
scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1$$anonfun$apply$13.apply(Interpreter.scala:788)
scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1$$anonfun$apply$13.apply(Interpreter.scala:788)
scala.util.control.Exception$Catch.apply(Exception.scala:79)
scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1.apply(Interpreter.scala:787)
scala.tools.nsc.Interpreter$Request$$anonfun$loadAndRun$1.apply(Interpreter.scala:787)
scala.util.control.Exception$Catch.apply(Exception.scala:79)
scala.tools.nsc.Interpreter$Request.loadAndRun(Interpreter.scala:786)
scala.tools.nsc.Interpreter.interpret(Interpreter.scala:435)
scala.tools.nsc.Interpreter.interpret(Interpreter.scala:425)
scala.tools.nsc.InterpreterLoop.interpretStartingWith(InterpreterLoop.scala:331)
scala.tools.nsc.InterpreterLoop.command(InterpreterLoop.scala:308)
scala.tools.nsc.InterpreterLoop.processLine$1(InterpreterLoop.scala:205)
scala.tools.nsc.InterpreterLoop.repl(InterpreterLoop.scala:223)
scala.tools.nsc.InterpreterLoop.main(InterpreterLoop.scala:379)
scala.tools.nsc.MainGenericRunner$.createLoop$1(MainGenericRunner.scala:119)
scala.tools.nsc.MainGenericRunner$.main(MainGenericRunner.scala:144)
scala.tools.nsc.MainGenericRunner.main(MainGenericRunner.scala)

```


Note that the stack is an array of StackTraceElement, on which it is possible to get
the class and method name as well as the file name and line number of its definition.


## Slate


The following #printCurrentStack is already defined in the base slate image but it is replicated here.


```slate
slate[1]> d@(Debugger traits) printCurrentStack &limit: limit &stream: out &showLocation: showLocation
[
  d clone `>> [baseFramePointer: (d interpreter framePointerOf: #printCurrentStack).
               buildFrames. 
               printBacktrace &limit: limit &stream: out &showLocation: showLocation ]
].
Defining function 'printCurrentStack' on: 'Debugger traits'
[printCurrentStack &limit: &stream: &showLocation:]
```


The output from calling the function:


```slate
slate[2]> Debugger printCurrentStack.
Backtrace (method @ source): 
frame: 0        [printCurrentStack &limit: &stream: &showLocation:] @ stdin:0
frame: 1        [evaluateIn: &optionals:] @ src/mobius/syntax.slate:180
frame: 2        [(arity: 0)] @ src/lib/repl.slate:155
frame: 3        [on:do:] @ src/core/condition.slate:43
frame: 4        [(arity: 0)] @ src/lib/repl.slate:147
frame: 5        [handlingCases:] @ src/core/condition.slate:64
frame: 6        [interpretHook:] @ src/lib/repl.slate:42
frame: 7        [(arity: 0)] @ src/lib/repl.slate:139
frame: 8        [enter] @ src/lib/repl.slate:135
frame: 9        [start &resource:] @ src/lib/repl.slate:185
frame: 10       [start] @ src/mobius/prelude.slate:38
Nil
```



## Smalltalk

{{works with|GNU Smalltalk}}

A backtrace is normally sent when some error occurs; however, it can be "forced":


```smalltalk
Object subclass: Container [
   Container class >> outer: a and: b and: c [
     self middle: (a+b) and: (b+c)
   ]
   Container class >> middle: x and: y [
     self inner: (x*y)
   ]
   Container class >> inner: k [
     Smalltalk backtrace
   ]
].

Container outer: 2 and: 3 and: 5.

'Anyway, we continue with it' displayNl.
```


Output:


```txt
Container class>>inner:
Container class>>middle:and:
Container class>>outer:and:and:
UndefinedObject>>executeStatements
Anyway, we continue with it
```



## Tcl


```tcl
proc printStackTrace {} {
    puts "Stack trace:"
    for {set i 1} {$i < [info level]} {incr i} {
        puts [string repeat "  " $i][info level $i]
    }
}
```

Demonstration code:

```tcl
proc outer {a b c} {
    middle [expr {$a+$b}] [expr {$b+$c}]
}
proc middle {x y} {
    inner [expr {$x*$y}]
}
proc inner k {
    printStackTrace
} 
outer 2 3 5
```

Produces this output:

```txt
Stack trace:
  outer 2 3 5
    middle 5 8
      inner 40
```



## VBA

In VBE the VBA Editor hitting Control+L while stepping through your code in debug mode will pop up a window which displays the call stack.

## zkl


```zkl
fcn f{println("F");vm.stackTrace().println()} fcn g{println("G")}
f();g();
```

{{out}}
stackTrace just returns a string. You don't get to futz with the stack.

```txt

F
Stack trace for VM#1 ():
   Cmd.f@stackTrace addr:4  args(0) reg(0) 
   Cmd.__constructor addr:3  args(0) reg(0) R
   startup.__constructor addr:2242  args(0) reg(1) ER
   startup.__constructor addr:2178  args(0) reg(22)
G

```



{{omit from|AWK|No introspection}}
{{omit from|GUISS}}
{{omit from|Locomotive Basic|No introspection}}
{{omit from|Lotus 123 Macro Scripting|No introspection}}
{{omit from|PARI/GP}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}}
{{omit from|M4}}
{{omit from|ZX Spectrum Basic|No introspection}}
