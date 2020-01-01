+++
title = "Fork"
description = ""
date = 2019-08-20T08:09:15Z
aliases = []
[extra]
id = 1924
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}

;Task:

Spawn a new [[process]] which can run simultaneously with, and independently of, the original parent process.





## Ada

{{libheader|POSIX}}

```ada
with Ada.Text_IO,
     POSIX.Process_Identification,
     POSIX.Unsafe_Process_Primitives;

procedure Fork is
   use Ada.Text_IO,
       POSIX.Process_Identification,
       POSIX.Unsafe_Process_Primitives;
begin
   if Fork = Null_Process_ID then
      Put_Line ("This is the new process.");
   else
      Put_Line ("This is the original process.");
   end if;
exception
   when others =>
      Put_Line ("Something went wrong.");
end Fork;
```



## Aikido


```aikido

    var pid = fork()
    switch (pid) {
    case <0:
        println ("fork error")
        break
    case 0:
        println ("child")
        break
    default:
        println ("parent")
        break
    }

```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9 - "fork" is not part of the standard's prelude.}}

```algol68
main:
(
  INT pid;
  IF (pid:=fork)=0 THEN
    print("This is new process")
  ELIF pid>0 THEN
    print("This is the original process")
  ELSE
    print("ERROR: Something went wrong")
  FI
)
```

Output:

```txt

This is new process
This is the original process

```


## AutoHotkey


```autohotkey
MsgBox, 4, Fork, Start another process?
IfMsgBox, Yes
    Run, %A_AhkPath% "%A_ScriptFullPath%"
MsgBox, 0, Fork, Stop this process.
```



## BaCon


```freebasic
' Fork
pid = FORK
IF pid = 0 THEN
    PRINT "I am the child, my PID is:", MYPID
    ENDFORK
ELIF pid > 0 THEN
    PRINT "I am the parent, pid of child:", pid
    REPEAT
        PRINT "Waiting for child to exit"
        SLEEP 50
    UNTIL REAP(pid)
ELSE
    PRINT "Error in fork"
ENDIF
```


{{out}}

```txt
prompt$ bacon fork.bac
Converting 'fork.bac'... done, 14 lines were processed in 0.004 seconds.
Compiling 'fork.bac'... cc  -c fork.bac.c
cc -o fork fork.bac.o -lbacon -lm
Done, program 'fork' ready.
prompt$ ./fork
I am the parent, pid of child:12733
Waiting for child to exit
I am the child, my PID is:12733
```



## Batch File

While you cannot fork into asynchronous subroutines conventionally, there are workarounds involving the <code>start</code> command.


```dos

@echo off

if "%1" neq "" goto %1 || echo Not a valid subroutine

echo Starting mySubroutine1
start "" "%~n0" mySubroutine1
echo.

echo Starting mySubroutine2 6 3
start "" "%~n0" mySubroutine2 6 3
echo.

echo Starting mySubroutine3
start "" "%~n0" mySubroutine3
echo.

:: We wait here for the subroutines to run, but they are running asynchronously
timeout /t 1

for /l %%i in (1,1,3) do (
	for /f "tokens=*" %%j in (output%%i.txt) do (
		set output%%i=%%j
		del output%%i.txt
	)
)
echo.
echo.
echo Return values
echo ----------------------------
echo mySubroutine1: %output1%
echo mySubroutine2: %output2%
echo mySubroutine3: %output3%

pause>nul
exit

:mySubroutine1
echo This is the result of subroutine1 > output1.txt
exit

:mySubroutine2
set /a result=%2+%3
echo %result% > output2.txt
exit

:mySubroutine3
echo mySubroutine1 hasn't been run > output3.txt
if exist output1.txt echo mySubroutine1 has been run > output3.txt
exit

```


Output:


```txt

Starting mySubroutine1

Starting mySubroutine2 6 3

Starting mySubroutine3


Waiting for 0 seconds, press a key to continue ...


Return values
----------------------------
mySubroutine1: This is the result of subroutine1
mySubroutine2: 9
mySubroutine3: mySubroutine1 has been run

```


## C

{{libheader|POSIX}}
```c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <err.h>

int main()
{
	pid_t pid;

	if (!(pid = fork())) {
		usleep(10000);
		printf("\tchild process: done\n");
	} else if (pid < 0) {
		err(1, "fork error");
	} else {
		printf("waiting for child %d...\n", (int)pid);
		printf("child %d finished\n", (int)wait(0));
	}

	return 0;
}
```
output<lang>waiting for child 3604...
        child process: done
child 3604 finished
```



## C++

{{trans|C}}

{{libheader|POSIX}}

```cpp
#include <iostream>
#include <unistd.h>

int main()
{
  pid_t pid = fork();

  if (pid == 0)
  {
    std::cout << "This is the new process\n";
  }
  else if (pid > 0)
  {
    std::cout << "This is the original process\n";
  }
  else
  {
    std::cerr << "ERROR: Something went wrong\n";
  }

  return 0;
}
```


=={{header|C#|C sharp}}==

```csharp
using System;
using System.Threading;

namespace Fork {
    class Program {
        static void Fork() {
            Console.WriteLine("Spawned Thread");
        }

        static void Main(string[] args) {
            Thread t = new Thread(new ThreadStart(Fork));
            t.Start();

            Console.WriteLine("Main Thread");
            t.Join();

            Console.ReadLine();
        }
    }
}
```



## Clojure

Through its Java interop capabilities, Clojure has full access to the JRE's process creation and control facilities. The ''clojure.java.shell'' API (in Clojure 1.2; there's an equivalent in 1.1 ''clojure.contrib.shell'') uses these facilities to provide a convenient way of running a shell command in a separate process, providing its arguments, input, environment, and working dir as necessary, and capturing the process's return code and its stdout and stderr output.

```clojure
(require '[clojure.java.shell :as shell])
(shell/sh "echo" "foo") ; evaluates to {:exit 0, :out "foo\n", :err ""}
```

Though this starts a separate process, the code in ''shell/sh'' blocks until the process completes. We can get other stuff done in the meantime by running the function in a separate thread with the core function ''future''. Suppose we want to find files named "needle.*" in a large directory tree ''haystack'', and do other stuff while the search proceeds. Using the Unix-like command ''find'' the code would look something like

```clojure
(let [search (future (shell/sh "find" "." "-name" "needle.*" :dir haystack))]
  (while (and (other-stuff-to-do?) (not (future-done? search)))
    (do-other-stuff))
  (let [{:keys [exit out err]} @search]
    (if (zero? exit)
      (do-something-with out)
      (report-errors-in err))))
```



## COBOL

Using libc fork

{{works with|GnuCOBOL}}


```cobol
       identification division.
       program-id. forking.

       data division.
       working-storage section.
       01 pid usage binary-long.

       procedure division.
       display "attempting fork"

       call "fork" returning pid
           on exception
               display "error: no fork linkage" upon syserr
       end-call

       evaluate pid
          when = 0
              display "    child sleeps"
              call "C$SLEEP" using 3
              display "    child task complete"
          when < 0
              display "error: fork result not ok" upon syserr
          when > 0
              display "parent waits for child..."
              call "wait" using by value 0
              display "parental responsibilities fulfilled"
       end-evaluate

       goback.
       end program forking.
```


{{out}}

```txt
prompt$ cobc -xj forking.cob
attempting fork
parent waits for child...
    child sleeps
    child task complete
parental responsibilities fulfilled
```



## Common Lisp


There's not a standard way to fork, but some implementations have built-in bindings for POSIX fork.

{{trans|C}}

{{works with|SBCL}}


```lisp
(let ((pid (sb-posix:fork)))
  (cond
   ((zerop pid) (write-line "This is the new process."))
   ((plusp pid) (write-line "This is the original process."))
   (t           (error "Something went wrong while forking."))))
```



## D


```D
import core.thread;
import std.stdio;

void main() {
    new Thread({
        writeln("Spawned thread.");
    }).start;
    writeln("Main thread.");
}
```


{{out}}

```txt
Main thread.
Spawned thread.
```



## DCL

In OpenVMS DCL, spawning a subprocess creates a partially independent process.  The parent and child processes share certain pooled quotas, certain shared resources, and if the parent process is deleted then the child process is too automatically.


```DCL
$! looper.com procedure
$ i = 10
$ loop:
$  show time
$  wait 'p1
$  i = i - 1
$  if i .gt. 0 then $  goto loop
```

{{out}}

```txt
$ spawn /nowait /notify @looper 0::2  ! up to 8 parameters are allowed
%DCL-S-SPAWNED, process DAVID_51258 spawned  ! random number suffix assigned
$
   4-JUN-2015 13:13:50
show default5 13:13:52  ! display anomaly due to parent and child overwriting output
   4-JUN-2015 13:13:54
  USER_ROOT:[DAVID]
$
   4-JUN-2015 13:13:57
   4-JUN-2015 13:13:59
 Interrupt  ! ctrl-c is the interrupt character; all child processes are deleted immediately

$
Subprocess DAVID_51258 has completed
$
```

To create a more independent process requires a privilege, e.g. detach.  There isn't a mechanism for passing parameters to the detached process, so we embed them in a jacket procedure (possibly created dynamically).

```DCL
$! fork.com procedure
$ set noverify  ! detached processes have verify on by default which clutters up the output log file
$ @looper 0::2
```

{{out}}

```txt
$ run /detach sys$system:loginout /input = fork /output = fork
%RUN-S-PROC_ID, identification of created process is 23A4195C
$ stop/id=23A4195C  ! rather than just waiting the 10 loop iterations
$ type fork.log
$! fork.com procedure
$ set noverify
   4-JUN-2015 13:35:47
   4-JUN-2015 13:35:49
   4-JUN-2015 13:35:51
   4-JUN-2015 13:35:53
   4-JUN-2015 13:35:55
   4-JUN-2015 13:35:57
```



## Elixir


```elixir
defmodule Fork do
  def start do
    spawn(fn -> child end)
    IO.puts "This is the original process"
  end

  def child, do: IO.puts "This is the new process"
end

Fork.start
```


{{out}}

```txt

This is the original process
This is the new process

```



## Erlang


```erlang
-module(fork).
-export([start/0]).

start() ->
    erlang:spawn( fun() -> child() end ),
    io:format("This is the original process~n").

child() ->
    io:format("This is the new process~n").
```


Then you can compile your code and execute it:


```erlang
c(fork).
fork:start().
```



## Factor

This works only in the terminal, if used from the UI the child process won't print.


```factor
USING: unix unix.process ;

[ "Hello form child" print flush 0 _exit ] [ drop "Hi from parent" print flush ] with-fork
```



## Fexl

There are many levels at which I can address this task.  I'll start from the lowest possible level:

```fexl
fork \pid
print "pid = ";print pid;nl;

```

{{out}}

```txt

pid = 10077
pid = 0

```

The child process prints the 0, and the parent process prints the pid of that child, which in this case happened to be 10077.

At the next level up, we can define a "spawn" function which makes it easy to fork a child process and interact with its stdin, stdout, and stderr:

```fexl

# (spawn child_fn next)
# Fork the child function as a process and return its pid, stdin, stdout, and
# stderr.
\spawn =
    (
    ### Use error-checking versions of system routines
    \pipe =
        (\next
        pipe \status\read\write
        long_lt status 0 (die "pipe failed");
        next read write
        )

    \dup2 =
        (\oldfd\newfd\next
        dup2 oldfd newfd \status
        long_lt status 0 (die "dup2 failed");
        next
        )

    \fdopen =
        (\fd\mode\next
        fdopen fd mode next;
        die "fdopen failed"
        )

    \fork =
        (\next
        fork \pid
        long_lt pid 0 (die "fork failed");
        next pid
        )

    # Now here's the spawn function itself.
    \child_fn\next

    # First flush the parent's stdout and stderr to avoid any pending output
    # accidentally getting pushed into the child's input.  I've noticed this
    # can happen when your script output is sent to a file or pipe instead of
    # a console.
    get_stdout \fh fflush fh \_
    get_stderr \fh fflush fh \_

    # Now create a series of pipes, each with a read and write side.
    pipe \r_in\w_in
    pipe \r_out\w_out
    pipe \r_err\w_err

    fork \pid
    long_eq pid 0
        (
        # Child process.

        # Duplicate one side of each pipe into stdin, stdout, and stderr
        # as appropriate.
        dup2 r_in 0;
        dup2 w_out 1;
        dup2 w_err 2;

        # Close unused file handles.  They're all unused because we duped the
        # ones we need.  Also, we must close w_in or the child hangs waiting
        # for stdin to close.
        close r_in; close w_in;
        close r_out; close w_out;
        close r_err; close w_err;

        # Now run the child function, which can use stdin, stdout, and stderr
        # normally.
        child_fn
        )
        (
        # Parent process.  Open the opposite side of each pipe into three new
        # file handles.
        fdopen w_in "w" \child_in
        fdopen r_out "r" \child_out
        fdopen r_err "r" \child_err

        # Close unused file handles.  We don't close the ones we fdopened
        # because they are still in play (i.e. fdopen does not dup).
        close r_in;
        close w_out;
        close w_err;

        # Return the child's pid, stdin, stdout, and stderr.
        next pid child_in child_out child_err
        )
    )

```


Next, we define a test_pipe function to test the whole apparatus:


```fexl

\test_pipe =
(\next
print "== test_pipe";nl;

### Handy

# Echo entire contents of stream fh to stdout.
\file_print ==
    (\fh\next
    fgetc fh \ch
    long_lt ch 0 next;
    putchar ch;
    file_print fh next
    )

# Show a stream with a descriptive label.
\show_stream =
    (\label\fh\next
    print "[ ";print label;print ":";nl;
    file_print fh;
    print "]";nl;
    next
    )

### Here is a child function to try with spawn.

\child_fn =
    (\next
    print "Hello from child.";nl;
    get_stdin \stdin
    show_stream "input from parent" stdin;
    print "Good bye from child.";nl;
    die "Oops the child had an error!";
    next
    )

# Spawn the child.
spawn child_fn \pid\child_in\child_out\child_err

# Now we can communicate with the child through its three file handles.
print "Hello from parent, child pid = ";print pid;print ".";nl;

# Say something to the child.
(
# Override print routines for convenience.
\print = (fwrite child_in)
\nl = (print NL)

# Start talking.
print "Hello child, I am your parent!";nl;
print "OK, nice talking with you.";nl;
);

print "The parent is now done talking to the child.";nl;

# Now show the child's stdout and stderr streams.
show_stream "output from child" child_out;
show_stream "error from child" child_err;

# Wait for child to finish.
wait \pid\status
# LATER shift and logical bit operators
# LATER WEXITSTATUS and other wait macros
\status = (long_div status 256)

print "Child ";print pid;print " exited with status ";
    print status;print ".";nl;
print "Good bye from parent.";nl;

print "test_pipe completed successfully.";nl;
next
)

```


Finally we call the test function:

```fexl
test_pipe;
```

{{out}}

```txt

== test_pipe
Hello from parent, child pid = 10391.
The parent is now done talking to the child.
[ output from child:
Hello from child.
[ input from parent:
Hello child, I am your parent!
OK, nice talking with you.
]
Good bye from child.
]
[ error from child:
Oops the child had an error!
]
Child 10391 exited with status 1.
Good bye from parent.
test_pipe completed successfully.

```



## Go

This program prints its own pid, then runs a copy of itself if given any argument on the command line. When it does so, it prints the pid of the child process it started. Output should show this pid matching the child's self reported pid.
Note that on Unix like systems <code>os.StartProcess</code> is a wrapper around <code>syscal.ForkExec</code> (which as the name implies, safely calls <code>fork</code> and <code>exec</code> system calls).
The [https://golang.org/pkg/os/exec <code>os/exec</code>] package offers a higher level interface and may be simpler in some situations.
For the purpose of this task though, there is little difference.

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Printf("PID: %v\n", os.Getpid())
    if len(os.Args) < 2 {
        fmt.Println("Done.")
        return
    }
    cp, err := os.StartProcess(os.Args[0], nil,
        &os.ProcAttr{Files: []*os.File{nil, os.Stdout}},
    )
    if err != nil {
        fmt.Println(err)
    }
    // Child process running independently at this point.
    // We have its PID and can print it.
    fmt.Printf("Child's PID: %v\n", cp.Pid)
    if _, err = cp.Wait(); err != nil {
        fmt.Println(err)
    }
}
```

{{out}}

```txt
PID: 28044
Child's PID: 28045
PID: 28045
Done.
```



## Groovy

Like Java, Groovy controls the standard I/O streams of its child processes. Unlike Java, Groovy provides convenience methods on the Process class to make this burden somewhat easier to manage. This sample code launches the child process and then ties that process's regular and error output streams into the Groovy program's own such streams. This allows us to verify simply that the parent and child processes are running independently.

For the subprocess this example uses Cygwin's bash shell and commands running under MS Windows.

```groovy
println "BEFORE PROCESS"
Process p = Runtime.runtime.exec('''
C:/cygwin/bin/sh -c "
/usr/bin/date +'BEFORE LOOP: %T';
for i in 1 2 3 4 ; do
    /usr/bin/sleep 1;
    /usr/bin/echo \$i;
done;
/usr/bin/date +'AFTER LOOP: %T'"
''')
p.consumeProcessOutput(System.out, System.err)
(0..<8).each {
    Thread.sleep(500)
    print '.'
}
p.waitFor()
println "AFTER PROCESS"
```


Output:

```txt
BEFORE PROCESS
BEFORE LOOP: 12:36:07
..1
..2
..3
..4
AFTER LOOP: 12:36:11
AFTER PROCESS
```



## Haskell


```haskell
import System.Posix.Process

main = do
  forkProcess (putStrLn "This is the new process")
  putStrLn "This is the original process"
```



## HicEst


```hicest
SYSTEM( RUN )

WRITE(Messagebox='?Y', IOStat=ios) "Another Fork?"
IF(ios == 2) ALARM(999) ! quit immediately

! assume this script is stored as 'Fork.hic'
SYSTEM(SHell='Fork.hic')

BEEP("c e g 'c")
WRITE(Messagebox="!") "Waiting ..."
ALARM(999)              ! quit immediately
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
   if (fork()|runerr(500)) = 0 then
      write("child")
   else {
      delay(1000)
      write("parent")
   }
end
```

Notes:
* Fork should not fail.  If an error 500 is generated there is a problem.
* Fork is not supported under windows.  Multitasking should be used instead.


## J

This example works by calling fork in a shared object library of Ubuntu 14.04.1 LTS .  The verb given to adverb Fork evaluates in the child process.

```J

load'dll'
Fork =: (('Error'"_)`('Parent'"_)`)(@.([: >: [: * '/lib/x86_64-linux-gnu/libc-2.19.so __fork > x' cd [: i. 0&[))
```

The child process explicitly exits remaining as a zombie until the parent terminates.

```txt

   NB. interactive session demonstrating Fork
   Time =: 6!:
   SLEEP =: 3
   sleep =: SLEEP Time
   ([:exit 0:[:smoutput'child'[sleep)Fork 50  NB. start the child
parent
   i._2 3 4 NB. interactive computations continue in the parent process
12 13 14 15
16 17 18 19
20 21 22 23

 0  1  2  3
 4  5  6  7
 8  9 10 11
   NB. zzzzz  50 seconds elapse, the child finishes.
   child

```



## Java

{{trans|NetRexx}}

```Java

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class RFork {

  public static void main(String[] args) {
    ProcessBuilder pb;
    Process pp;
    List<String> command;
    Map<String, String> env;
    BufferedReader ir;
    String currentuser;
    String line;
    try {
      command = Arrays.asList("");
      pb = new ProcessBuilder(command);
      env = pb.environment();
      currentuser = env.get("USER");
      command = Arrays.asList("ps", "-f", "-U", currentuser);
      pb.command(command);
      pp = pb.start();
      ir = new BufferedReader(new InputStreamReader(pp.getInputStream()));
      line = "Output of running " + command.toString() + " is:";
      do {
        System.out.println(line);
      } while ((line = ir.readLine()) != null);
    }
    catch (IOException iox) {
      iox.printStackTrace();
    }

    return;
  }
}

```

{{out}}

```txt

Output of running [ps, -f, -U, developer] is:
  UID   PID  PPID   C STIME   TTY           TIME CMD
  502 74079     1   0  8:13PM ??         0:00.02 /sbin/launchd
  ...
  502 74047 74045   0  8:13PM ttys000    0:00.05 bash
  502 74198 74047   0  8:18PM ttys000    0:00.21 /usr/bin/java -cp .:.. RFork
  502 74199 74047   0  8:18PM ttys000    0:00.00 tee data/RForkJ.log
    0 74200 74198   0  8:18PM ttys000    0:00.00 ps -f -U developer
  ...

```



## Julia


```julia
println("Parent running.")
@async(begin sleep(1); println("This is the child process."); sleep(2); println("Child again.") end)
sleep(2)
println("This is the parent process again.")
sleep(2)
println("Parent again.")

```
{{output}}
```txt

 Parent running.
 This is the child process.
 This is the parent process again.
 Child again.
 Parent again.

```




## Kotlin

{{trans|NetRexx}}

```scala
// version 1.1.51

import java.io.InputStreamReader
import java.io.BufferedReader
import java.io.IOException

fun main(args: Array<String>) {
    try {
        val pb = ProcessBuilder()
        val currentUser = pb.environment().get("USER")
        val command = listOf("ps", "-f", "U", currentUser)
        pb.command(command)
        val proc = pb.start()
        val isr = InputStreamReader(proc.inputStream)
        val br = BufferedReader(isr)
        var line: String? = "Output of running $command is:"
        while(true) {
            println(line)
            line = br.readLine()
            if (line == null) break
        }
    }
    catch (iox: IOException) {
        iox.printStackTrace()
    }
}
```


Sample output (Ubuntu 14.04):

```txt

Output of running [ps, -f, U, user1] is:
UID        PID  PPID  C STIME TTY      STAT   TIME CMD
user1     1401  1387  0 00:13 ?        Ss     0:00 init --user
.....
user1     2687  2425  0 00:21 pts/8    Sl+    0:00 java -jar fork.jar
user1     2699  2687  0 00:21 pts/8    R+     0:00 ps -f U user1

```



## Lasso

Lasso is multithreaded by design.
You can fork of an independent thread at anytime using split_thread. The second thread will inherit all local variables declared before it is split.

```Lasso
local(mydata = 'I am data one')

split_thread => {
	loop(2)	=> {
		sleep(2000)
		stdoutnl(#mydata)
		#mydata = 'Oh, looks like I am in a new thread'
	}
}

loop(2)	=> {
	sleep(3000)
	stdoutnl(#mydata)
	#mydata = 'Aha, I am still in the original thread'
}
```

Output:

```txt
I am data one
I am data one
Oh, looks like I am in a new thread
Aha, I am still in the original thread

```



## LFE


{{trans|Erlang}}

You can run this in the REPL as-is:


```lisp

(defun start ()
  (spawn (lambda () (child))))

(defun child ()
  (lfe_io:format "This is the new process~n" '()))

```



## Lua

{{libheader|POSIX}}

```Lua
local posix = require 'posix'

local pid = posix.fork()
if pid == 0 then
    print("child process")
elseif pid > 0 then
    print("parent process")
else
    error("unable to fork")
end
```



## Mathematica

This code will run a standalone Mathematica kernel, putting the result of a command in a temporary file:

```Mathematica
commandstring =  First[$CommandLine] <> " -noprompt -run \"Put[Factorial[20],ToFileName[$TemporaryDirectory,ToString[temp1]]];Quit[]\""
->"MathKernel -noprompt -run \"Put[Factorial[20],ToFileName[$TemporaryDirectory,ToString[temp1]]];Quit[]\""

Run[commandstring]
->0
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static

  do
    pb = ProcessBuilder([String ''])
    env = pb.environment()
    currentuser = String env.get('USER')
    command = Arrays.asList([String 'ps', '-f', '-U', currentuser])
    pb.command(command)
    pp = pb.start()
    ir = BufferedReader(InputStreamReader(pp.getInputStream()))
    line = String 'Output of running' command.toString() 'is:'
    loop label w_ until line = null
      say line
      line = ir.readLine()
      end w_
  catch iox = IOException
    iox.printStackTrace()
  end

  return

```

{{out}}

```txt

Output of running [ps, -f, -U, nrxuser] is:
  UID   PID  PPID   C STIME   TTY           TIME CMD
  501   277     1   0 21Aug13 ??         0:32.05 /sbin/launchd
  ...
    0   366   291   0 21Aug13 ttys001    0:00.02 login -pfl nrxuser /bin/bash -c exec -la bash /bin/bash
  501   368   366   0 21Aug13 ttys001    0:00.16 -bash
  501 72276   368   0  6:28PM ttys001    0:00.23 /usr/bin/java -cp .:.. RFork
  501 72277   368   0  6:28PM ttys001    0:00.00 tee data/RFork.log
    0 72278 72276   0  6:28PM ttys001    0:00.00 ps -f -U nrxuser
    0   380   291   0 21Aug13 ttys002    0:00.02 login -pfl nrxuser /bin/bash -c exec -la bash /bin/bash
  ...


```



## NewLISP


```NewLISP
(let (pid (fork (println "Hello from child")))
  (cond
   ((nil? pid) (throw-error "Unable to fork"))
   ('t (wait-pid pid))))
```



## Nim


```nim
import posix

var pid = fork()
if pid < 0:
    # error forking a child
elif pid > 0:
    # parent, and pid is process id of child
else:
    # child
    quit()
# further Parent stuff here
```



## OCaml



```ocaml
#load "unix.cma";;
let pid = Unix.fork ();;
if pid > 0 then
  print_endline "This is the original process"
else
  print_endline "This is the new process";;
```



## ooRexx


### version 1 using REPLY


```oorexx
sub=.fork~new
sub~sub
Call syssleep 1
Do 3
  Say 'program   ' time()
  Call syssleep 1
  End

::class fork
:: method sub
Reply
Do 6
  Say 'subroutine' time()
  Call syssleep 1
  End
```

{{out}}

```txt
subroutine 10:53:27
program    10:53:28
subroutine 10:53:29
program    10:53:35
subroutine 10:53:38
program    10:53:40
subroutine 10:53:40
subroutine 10:53:41
subroutine 10:53:42
```



### version 2 using START


```oorexx
sub=.fork~new
sub~start('start_working')

Call syssleep 1
Do 3
  Say 'program   ' time()
  Call syssleep 1
  End

::class fork
:: method start_working
Do 6
  Say 'subroutine' time()
  Call syssleep 1
  End
```

{{out}}

```txt
subroutine 14:55:10
program    14:55:11
subroutine 14:55:11
subroutine 14:55:12
program    14:55:12
program    14:55:13
subroutine 14:55:13
subroutine 14:55:14
subroutine 14:55:15
```



## Oz

Mozart's support for distributed programming is quite unique. We can send code accross the network and share data by lexical scoping. It doesn't matter whether we create the process on the local machine (as in this example) or on some remote computer as long as we have ssh access (or some similar method) and Mozart is installed.


```oz
declare
  ParentVar1 = "parent data"
  ParentVar2

  functor RemoteCode
  export
     result:Result
  import QTk at 'x-oz://system/wp/QTk.ozf'
  define
     Result
     %% Show a simple window. When it is closed by the user, set Result.
     Window =
     {QTk.build
      td(action:proc {$} Result = 42 end %% on close
         label(text:"In child process: "#ParentVar1))} %% read parent process variable
     {Window show}
     !ParentVar2 = childData %% write to parent process variable
     {Wait Result}
  end

  %% create a new process on the same machine
  RM = {New Remote.manager init(host:localhost)}
  %% execute the code encapsulated in the given functor
  RemoteModule = {RM apply(RemoteCode $)}
in
  %% retrieve data from child process
  {Show RemoteModule.result} %% prints 42
  %% exit child process
  {RM close}
  {Show ParentVar2} %% print "childData"
```



## PARI/GP

This is a PARI implementation which uses <code>fork()</code> via PARI's <code>pari_daemon</code>.  Of course <code>fork()</code> could be used directly.


```C
void
foo()
{
  if (pari_daemon())
    pari_printf("Original\n");
  else
    pari_printf("Fork\n");
}
```



## Perl

{{works with|Perl|5.x}}
In the child code, you may have to re-open database handles and such.


```perl
FORK:
if ($pid = fork()) {
    # parent code
} elsif (defined($pid)) {
    setsid; # tells apache to let go of this process and let it run solo
    # disconnect ourselves from input, output, and errors
    close(STDOUT);
    close(STDIN);
    close(STDERR);
    # re-open to /dev/null to prevent irrelevant warn messages.
    open(STDOUT, '>/dev/null');
    open(STDIN, '>/dev/null');
    open(STDERR, '>>/home/virtual/logs/err.log');

    # child code

    exit; # important to exit
} elsif($! =~ /emporar/){
    warn '[' . localtime() . "] Failed to Fork - Will try again in 10 seconds.\n";
    sleep(10);
    goto FORK;
} else {
    warn '[' . localtime() . "] Unable to fork - $!";
    exit(0);
}
```


Obviously you could do a Fork in a lot less lines, but this code covers all the bases.

Another example using [http://search.cpan.org/perldoc?Proc::Fork Proc::Fork] module:


```perl
use Proc::Fork;
run_fork {
    child {
        # child code ...
    }
    parent {
        # parent code ...
    }
};
```


Or:

```perl
use Proc::Fork;
# parent code ...
run_fork {
    child {
        # child code ...
    }
};
# parent code continues ...
```


More complex example with retries and error handling:

```perl
use Proc::Fork;
run_fork {
    child {
        # child code ...
    }
    parent {
        # parent code ...
    }
    retry {
        # retry code ...
    }
    error {
        # error handling ...
    }
};
```



## Perl 6

{{Works with|rakudo|2016.06}}

```perl6
use NativeCall;
sub fork() returns int32 is native { ... }

if fork() -> $pid {
    print "I am the proud parent of $pid.\n";
}
else {
    print "I am a child.  Have you seen my mommy?\n";
}
```

{{out}}

```txt
I am the proud parent of 17691.
I am a child.  Have you seen my mommy?
```



## Phix

Phix has create_thread which creates a separate thread, with its own call stack, but sharing common data (like most fork examples here).

To run something completely independently, use system() or system_exec(), depending on whether you want a shell and/or to wait for a result.


```Phix
procedure mythread()
    ?"mythread"
    exit_thread(0)
end procedure

atom hThread = create_thread(routine_id("mythread"),{})
?"main carries on"
wait_thread(hThread)
```

or

```Phix
system("calc")
```



## PHP

{{trans|C}}

```php
<?php
$pid = pcntl_fork();
if ($pid == 0)
  echo "This is the new process\n";
else if ($pid > 0)
  echo "This is the original process\n";
else
  echo "ERROR: Something went wrong\n";
?>
```



## PicoLisp


```PicoLisp
(unless (fork)                         # In child process
   (println *Pid)                      # Print the child's PID
   (bye) )                             # and terminate
```



## PL/I


```PL/I

ATTACH SOLVE (X) THREAD (T5);

```



## Pop11


```pop11
lvars ress;
if sys_fork(false) ->> ress then
   ;;; parent
   printf(ress, 'Child pid = %p\n');
else
   printf('In child\n');
endif;
```

{{omit from|PureBasic}}


## Python

{{works with|Python|2.5}}

```python
import os

pid = os.fork()
if pid > 0:
 # parent code
else:
 # child code
```



## Racket


Looks like there are two popular things that people do for this task, so here
are both.  First, run some subprocess independently of Racket:


```Racket

#lang racket
(define-values [P _out _in _err]
  (subprocess (current-output-port) (current-input-port) (current-error-port)
              (find-executable-path "du") "-hs" "/usr/share"))
;; wait for process to end, print messages as long as it runs
(let loop () (unless (sync/timeout 10 P) (printf "Still running...\n") (loop)))

```


Output:

```txt

Still running...
Still running...
Still running...
...snip...
15G	/usr/share

```


Second, using fork() in its raw form, which is doable in racket, but as unsafe as you'd expect it to be:


```Racket

#lang racket
(require ffi/unsafe)
(define fork (get-ffi-obj 'fork #f (_fun -> _int)))
(printf ">>> fork() => ~s\n" (fork))

```


Output:

```txt

>>> fork() => 23834
>>> fork() => 0

```



## REXX

This function   '''only'''   works with Regina REXX.

```rexx
child = fork()
```



## Ruby


```ruby
pid = fork
if pid
 # parent code
else
 # child code
end
```

or

```ruby
fork do
  # child code
end
# parent code
```



## Run BASIC

You can run a program until that program executes a wait statement.
Once the program waits,you can use it's functions.


```runbasic
run "someProgram.bas",#handle
render #handle     ' this runs the program until it waits
                   ' both the parent and child are running
' --------------------------------------------------------
' You can also call a function in the someProgram.bas program.
' For example if it had a DisplayBanner Funciton.
#handle DisplayBanner("Welcome!")
```



## Rust

This uses the nix(0.15) crate. The code has been tested on Linux, OS X.

```rust
use nix::unistd::{fork, ForkResult};
use std::process::id;

fn main() {
    match fork() {
        Ok(ForkResult::Parent { child, .. }) => {
            println!(
                "This is the original process(pid: {}). New child has pid: {}",
                id(),
                child
            );
        }
        Ok(ForkResult::Child) => println!("This is the new process(pid: {}).", id()),
        Err(_) => println!("Something went wrong."),
    }
}

```
output<lang>This is the original process(pid: 88637). New child has pid: 88651
This is the new process(pid: 88651).

```



## Scala

{{libheader|Scala}}

### A Linux version


```scala
import java.io.IOException

object Fork extends App {
  val builder: ProcessBuilder = new ProcessBuilder()
  val currentUser: String = builder.environment.get("USER")
  val command: java.util.List[String] = java.util.Arrays.asList("ps", "-f", "-U", currentUser)
  builder.command(command)
  try {
    val lines = scala.io.Source.fromInputStream(builder.start.getInputStream).getLines()
    println(s"Output of running $command is:")
    while (lines.hasNext) println(lines.next())
  }
  catch {
    case iox: IOException => iox.printStackTrace()
  }
}
```


### A Windows version


```scala
import java.io.IOException

object Fork extends App {
  val command: java.util.List[String] = java.util.Arrays.asList("cmd.exe", "/C", "ECHO.| TIME")
  val builder: ProcessBuilder = new ProcessBuilder(command)
  try {
    val lines = scala.io.Source.fromInputStream(builder.start.getInputStream).getLines()
    println(s"Output of running $command is:")
    while (lines.hasNext) println(lines.next())
  }
  catch {
    case iox: IOException => iox.printStackTrace()
  }
}
```



## Sidef


```ruby
var x = 42;
{ x += 1; say x }.fork.wait;     # x is 43 here
say x;                           # but here is still 42
```



## Slate

The following built-in method uses the cloneSystem primitive (which calls fork()) to fork code. The parent and the child both get a socket from a socketpair which they can use to communicate. The cloneSystem is currently unimplemented on windows (since there isn't a fork() system call).
```slate
p@(Process traits) forkAndDo: b
[| ret |
  (ret := lobby cloneSystem)
    first ifTrue: [p pipes addLast: ret second. ret second]
           ifFalse: [[p pipes clear. p pipes addLast: ret second. b applyWith: ret second] ensure: [lobby quit]]
].
```



## Smalltalk



```smalltalk
'Here I am' displayNl.
|a|
a := [
  (Delay forSeconds: 2) wait .
  1 to: 100 do: [ :i | i displayNl ]
] fork.
'Child will start after 2 seconds' displayNl.
"wait to avoid terminating first the parent;
 a better way should use semaphores"
(Delay forSeconds: 10) wait.
```



## Standard ML



```sml
case Posix.Process.fork () of
   SOME pid => print "This is the original process\n"
 | NONE     => print "This is the new process\n";
```



## Tcl

(from the [http://wiki.tcl.tk/1967 Tcl Wiki])

Fork is one of the primitives used for process creation in Unixy systems. It creates a copy of the process that calls it, and the only difference in internal state between the original and the copy is in the return value from the fork call (0 in the copy, but the pid of the copy in the parent).

The [[SMW::off]][[:Category:Expect|Expect]][[Category:Expect]][[SMW::on]]{{#set:Uses library=Expect}}<!--{{libheader|Expect}}--> package includes a fork. So does the [[SMW::off]][[:Category:TclX|TclX]][[Category:TclX]][[SMW::on]]{{#set:Uses library=TclX}}<!--{{libheader|TclX}}--> package.

Example:


```tcl
package require Expect
# or
package require Tclx

for {set i 0} {$i < 100} {incr i} {
    set pid [fork]
    switch $pid {
        -1 {
            puts "Fork attempt #$i failed."
        }
        0 {
            puts "I am child process #$i."
            exit
        }
        default {
            puts "The parent just spawned child process #$i."
        }
    }
}
```


In most cases though, one is not interested in spawning a copy of the process one already has, but rather wants a different process. When using POSIX APIs, this has to be done by first forking and then having the child use the exec system call to replace itself with a different program. The Tcl <code>[http://www.tcl.tk/man/tcl8.5/TclCmd/exec.htm exec]</code> command does this fork&exec combination — in part because non-Unix OSs typicallly don't have "make a copy of parent process" as an intermediate step when spawning new processes.

Note that <code>fork</code> is only supported at all on unthreaded builds of Tcl. This is because the POSIX threads library does not sit well with the fork() system call.


## Toka


```toka
needs shell
getpid is-data PID
[ fork getpid PID = [ ." Child PID: " . cr ] [ ." In child\n" ] ifTrueFalse ] invoke
```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
i=0
(while test $i -lt 10; do
  sleep 1
  echo "Child process"
  i=`expr $i + 1`
done) &
while test $i -lt 5; do
  sleep 2
  echo "Parent process"
  i=`expr $i + 1`
done
```


This uses the operator <tt>&</tt> to run the child process and the parent process at the same time. The output for the next 10 seconds is "Child process" every 1 second, and "Parent process" every 2 seconds. Both processes inherit <tt>i=0</tt>, but each process has its own <tt>i</tt> variable because processes are independent.

The original version of this code used a bash for-loop.

{{works with|bash}}


```bash
(for ((i=0;i<10;i++)); do sleep 1; echo "Child process"; done) &
for ((i=0;i<5;i++)); do
  sleep 2
  echo "Parent process"
done
```



## UnixPipes

Demonstrating a subshell getting forked, and running concurrently with the original process


```bash
(echo "Process 1" >&2 ;sleep 5; echo "1 done" ) | (echo "Process 2";cat;echo "2 done")
```



## Wart


```wart
do (fork sleep.1
         prn.1)
   prn.2
```



## X86 Assembly

I've written a subroutine that prints out any positive value. It lives on my desktop and you can't find it on rosetta code.
I've also written a sleep subroutine and you can find that in the Sleep task on this site.


```x86asm

; x86_64 linux nasm

%include "/home/james/Desktop/ASM_LIB/Print.asm"
%include "/home/james/Desktop/ASM_LIB/Sleep.asm"

section .data

parent: db "Parent: "
child: db "Child: "
newLine: db 10

section .text

global _start

_start:
  mov rax, 57 ; fork syscall
  syscall
  cmp rax, 0 ; if the return value is 0, we're in the child process
  je printChild

  printParent: ; else it's the child's PID, we're in the parent

    mov rax, 1
    mov rdi, 1
    mov rsi, parent
    mov rdx, 8
    syscall

    mov rax, 39 ; sys_getpid
    syscall
    mov rdi, rax
    call Print_Unsigned

    mov rax, 1
    mov rdi, 1
    mov rsi, newLine
    mov rdx, 1
    syscall

    mov rdi, 1 ; sleep so the child process can print befor the parent exits
    call Sleep ; you might not see the child output if you don't do this

    jmp exit

  printChild:

    mov rdi, 1
    call Sleep ; sleep and wait for parent to print to screen first

    mov rax, 1
    mov rdi, 1
    mov rsi, child
    mov rdx, 7
    syscall

    mov rax, 39 ; sys_getpid
    syscall
    mov rdi, rax
    call Print_Unsigned

    mov rax, 1
    mov rdi, 1
    mov rsi, newLine
    mov rdx, 1
    syscall

  exit:
    mov rax, 60
    mov rdi, 0
    syscall

```



## zkl

{{works with|Unix}}
This just tells the Unix shell to run the process in the background

```zkl
zkl: System.cmd("ls &")
```

{{out}}

```txt

0  // return code from the shell, ls has been forked
zkl: 1_2_all_freq.txt      ff.zkl	     lua.zkl		      rot13.b
2hkprimes.txt	      fg.zkl	     lucas-lehmer.zkl	      rot13.zkl
...

```



{{omit from|JavaScript}}
{{omit from|Maxima}}
{{omit from|TI-83 BASIC|Does not have concurrency or background processes.}}
{{omit from|TI-89 BASIC|Does not have concurrency or background processes.}}
{{omit from|Unlambda|Does not have concurrency or background processes.}}
{{omit from|Retro|No concurrency in the standard VM}}
{{omit from|SAS}}
{{omit from|Stata}}
{{omit from|VBA}}
{{omit from|ZX Spectrum Basic|Does not support multiple processes.}}
