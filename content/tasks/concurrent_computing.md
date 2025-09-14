+++
title = "Concurrent computing"
description = ""
date = 2019-09-16T17:17:34Z
aliases = []
[extra]
id = 1915
[taxonomies]
categories = ["task", "Concurrency"]
tags = []
languages = [
  "ada",
  "algol_68",
  "astro",
  "bacon",
  "bbc_basic",
  "c",
  "cind",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dodo0",
  "e",
  "echolisp",
  "egel",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lfe",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "mercury",
  "neko",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pike",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "raven",
  "rhope",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "swift",
  "tcl",
  "unixpipes",
  "vba",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

Using either native language concurrency syntax or freely available libraries, write a program to display the strings "Enjoy" "Rosetta" "Code", one string per line, in random order.

Concurrency syntax must use [[thread|threads]], tasks, co-routines, or whatever concurrency is called in your language.





## Ada


```ada
with Ada.Text_IO, Ada.Numerics.Float_Random;

procedure Concurrent_Hello is
   type Messages is (Enjoy, Rosetta, Code);
   task type Writer (Message : Messages);
   task body Writer is
      Seed : Ada.Numerics.Float_Random.Generator;
   begin
      Ada.Numerics.Float_Random.Reset (Seed); -- time-dependent, see ARM A.5.2
      delay Duration (Ada.Numerics.Float_Random.Random (Seed));
      Ada.Text_IO.Put_Line (Messages'Image(Message));
   end Writer;
   Taks: array(Messages) of access Writer -- 3 Writer tasks will immediately run
     := (new Writer(Enjoy), new Writer(Rosetta), new Writer(Code));
begin
   null; -- the "environment task" doesn't need to do anything
end Concurrent_Hello;
```


Note that random generator object is local to each task. It cannot be accessed concurrently without mutual exclusion. In order to get different initial states of local generators Reset is called (see [http://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-5-2.html ARM A.5.2]).


## ALGOL 68


```algol68
main:(
  PROC echo = (STRING string)VOID:
      printf(($gl$,string));
  PAR(
    echo("Enjoy"),
    echo("Rosetta"),
    echo("Code")
  )
)
```



## Astro


```python
let words = ["Enjoy", "Rosetta", "Code"]

for word in words:
    (word) |> async (w) =>
        sleep(random())
        print(w)
```



## BaCon

BaCon is a BASIC-to-C compiler. Assuming GCC compiler in this demonstration. Based on the C OpenMP source.


```freebasic
' Concurrent computing using the OpenMP extension in GCC. Requires BaCon 3.6 or higher.

' Specify compiler flag
PRAGMA OPTIONS -fopenmp

' Sepcify linker flag
PRAGMA LDFLAGS -lgomp

' Declare array with text
DECLARE str$[] = { "Enjoy", "Rosetta", "Code" }

' Indicate MP optimization for FOR loop
PRAGMA omp parallel for num_threads(3)

' The actual FOR loop
FOR i = 0 TO 2
    PRINT str$[i]
NEXT

```


```txt
prompt$ bacon concurrent-computing
Converting 'concurrent-computing.bac'... done, 11 lines were processed in 0.002 seconds.
Compiling 'concurrent-computing.bac'... cc  -fopenmp -c concurrent-computing.bac.c
cc -o concurrent-computing concurrent-computing.bac.o -lbacon -lm  -lgomp
Done, program 'concurrent-computing' ready.
prompt$ ./concurrent-computing
Code
Enjoy
Rosetta
```



## BBC BASIC

The BBC BASIC interpreter is single-threaded so the only way of achieving 'concurrency' (short of using assembler code) is to use timer events:

```bbcbasic
      INSTALL @lib$+"TIMERLIB"

      tID1% = FN_ontimer(100, PROCtask1, 1)
      tID2% = FN_ontimer(100, PROCtask2, 1)
      tID3% = FN_ontimer(100, PROCtask3, 1)

      ON ERROR PRINT REPORT$ : PROCcleanup : END
      ON CLOSE PROCcleanup : QUIT

      REPEAT
        WAIT 0
      UNTIL FALSE
      END

      DEF PROCtask1
      PRINT "Enjoy"
      ENDPROC

      DEF PROCtask2
      PRINT "Rosetta"
      ENDPROC

      DEF PROCtask3
      PRINT "Code"
      ENDPROC

      DEF PROCcleanup
      PROC_killtimer(tID1%)
      PROC_killtimer(tID2%)
      PROC_killtimer(tID3%)
      ENDPROC
```



## C


```c
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

pthread_mutex_t condm = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
int bang = 0;

#define WAITBANG() do { \
   pthread_mutex_lock(&condm); \
   while( bang == 0 ) \
   { \
      pthread_cond_wait(&cond, &condm); \
   } \
   pthread_mutex_unlock(&condm); } while(0);\

void *t_enjoy(void *p)
{
  WAITBANG();
  printf("Enjoy\n");
  pthread_exit(0);
}

void *t_rosetta(void *p)
{
  WAITBANG();
  printf("Rosetta\n");
  pthread_exit(0);
}

void *t_code(void *p)
{
  WAITBANG();
  printf("Code\n");
  pthread_exit(0);
}

typedef void *(*threadfunc)(void *);
int main()
{
   int i;
   pthread_t a[3];
   threadfunc p[3] = {t_enjoy, t_rosetta, t_code};

   for(i=0;i<3;i++)
   {
     pthread_create(&a[i], NULL, p[i], NULL);
   }
   sleep(1);
   bang = 1;
   pthread_cond_broadcast(&cond);
   for(i=0;i<3;i++)
   {
     pthread_join(a[i], NULL);
   }
}
```


'''Note''': since threads are created one after another, it is likely that the execution of their code follows the order of creation. To make this less evident, I've added the ''bang'' idea using condition: the thread really executes their code once the gun bang is heard. Nonetheless, I still obtain the same order of creation (Enjoy, Rosetta, Code), and maybe it is because of the order locks are acquired. The only way to obtain randomness seems to be to add random wait in each thread (or wait for special cpu load condition)


### OpenMP

Compile with <code>gcc -std=c99 -fopenmp</code>:

```c
#include <stdio.h>
#include <omp.h>

int main()
{
	const char *str[] = { "Enjoy", "Rosetta", "Code" };
	#pragma omp parallel for num_threads(3)
	for (int i = 0; i < 3; i++)
		printf("%s\n", str[i]);
	return 0;
}
```



## C++

The following example compiles with GCC 4.7.

<code>g++ -std=c++11 -D_GLIBCXX_USE_NANOSLEEP -o concomp concomp.cpp</code>


```cpp
#include <thread>
#include <iostream>
#include <vector>
#include <random>
#include <chrono>

int main()
{
  std::random_device rd;
  std::mt19937 eng(rd()); // mt19937 generator with a hardware random seed.
  std::uniform_int_distribution<> dist(1,1000);
  std::vector<std::thread> threads;

  for(const auto& str: {"Enjoy\n", "Rosetta\n", "Code\n"}) {
    // between 1 and 1000ms per our distribution
    std::chrono::milliseconds duration(dist(eng));

    threads.emplace_back([str, duration](){
      std::this_thread::sleep_for(duration);
      std::cout << str;
    });
  }

  for(auto& t: threads) t.join();

  return 0;
}
```


Output:

```txt
Enjoy
Code
Rosetta
```


```cpp
#include <iostream>
#include <ppl.h> // MSVC++

void a(void) { std::cout << "Eat\n";   }
void b(void) { std::cout << "At\n";    }
void c(void) { std::cout << "Joe's\n"; }

int main()
{
    // function pointers
    Concurrency::parallel_invoke(&a, &b, &c);

    // C++11 lambda functions
    Concurrency::parallel_invoke(
        []{ std::cout << "Enjoy\n";   },
        []{ std::cout << "Rosetta\n"; },
        []{ std::cout << "Code\n";    }
    );
    return 0;
}
```

Output:

```txt

Joe's
Eat
At
Enjoy
Code
Rosetta

```


## C#

### With Threads


```c#

static Random tRand = new Random();

static void Main(string[] args)
{
	Thread t = new Thread(new ParameterizedThreadStart(WriteText));
	t.Start("Enjoy");

	t = new Thread(new ParameterizedThreadStart(WriteText));
	t.Start("Rosetta");

	t = new Thread(new ParameterizedThreadStart(WriteText));
	t.Start("Code");

	Console.ReadLine();
}

private static void WriteText(object p)
{
	Thread.Sleep(tRand.Next(1000, 4000));
	Console.WriteLine(p);
}

```


An example result:

```txt

Enjoy
Code
Rosetta

```


### With Tasks

```c#
using System;
using System.Threading.Tasks;

public class Program
{
    static async Task Main() {
        Task t1 = Task.Run(() => Console.WriteLine("Enjoy"));
        Task t2 = Task.Run(() => Console.WriteLine("Rosetta"));
        Task t3 = Task.Run(() => Console.WriteLine("Code"));

        await Task.WhenAll(t1, t2, t3);
    }
}
```


### With a parallel loop


```c#
using System;
using System.Threading.Tasks;

public class Program
{
    static void Main() => Parallel.ForEach(new[] {"Enjoy", "Rosetta", "Code"}, s => Console.WriteLine(s));
}
```



## Cind



```cind

execute() {
    {# host.println("Enjoy");
     # host.println("Rosetta");
     # host.println("Code"); }
}

```




## Clojure


A simple way to obtain concurrency is using the ''future'' function, which evaluates its body on a separate thread.

```clojure
(doseq [text ["Enjoy" "Rosetta" "Code"]]
  (future (println text)))
```

Using the new (2013) ''core.async'' library, "go blocks" can execute asynchronously,
sharing threads from a pool. This works even in ClojureScript (the JavaScript target of Clojure)
on a single thread. The ''timeout'' call is there just to shuffle things up: note this delay doesn't block a thread.

```clojure
(require '[clojure.core.async :refer [go <! timeout]])
(doseq [text ["Enjoy" "Rosetta" "Code"]]
  (go
    (<! (timeout (rand-int 1000))) ; wait a random fraction of a second,
    (println text)))
```



## CoffeeScript


===Using Bash (or an equivalent shell)===
JavaScript, which CoffeeScript compiles to, is single-threaded. This approach launches multiple process to achieve concurrency on [http://nodejs.org Node.js]:


```coffeescript
{ exec } = require 'child_process'

for word in [ 'Enjoy', 'Rosetta', 'Code' ]
    exec "echo #{word}", (err, stdout) ->
        console.log stdout
```



### Using Node.js

As stated above, CoffeeScript is single-threaded. This approach launches multiple [http://nodejs.org Node.js] processes to achieve concurrency.


```coffeescript
# The "master" file.

{ fork } = require 'child_process'
path = require 'path'
child_name = path.join __dirname, 'child.coffee'
words = [ 'Enjoy', 'Rosetta', 'Code' ]

fork child_name, [ word ] for word in words
```



```coffeescript
# child.coffee

console.log process.argv[ 2 ]
```



## Common Lisp


Concurrency and threads are not part of the Common Lisp standard.  However, most implementations provide some interface for concurrency.  [http://common-lisp.net/project/bordeaux-threads/ Bordeaux Threads], used here, provides a compatibility layer for many implementations.  (Binding <var>out</var> to <code>*standard-output*</code> before threads are created is needed as each thread gets its own binding for <code>*standard-output*</code>.)


```lisp
(defun concurrency-example (&optional (out *standard-output*))
  (let ((lock (bordeaux-threads:make-lock)))
    (flet ((writer (string)
             #'(lambda ()
                 (bordeaux-threads:acquire-lock lock t)
                 (write-line string out)
                 (bordeaux-threads:release-lock lock))))
      (bordeaux-threads:make-thread (writer "Enjoy"))
      (bordeaux-threads:make-thread (writer "Rosetta"))
      (bordeaux-threads:make-thread (writer "Code")))))
```



## D


```d
import std.stdio, std.random, std.parallelism, core.thread, core.time;

void main() {
    foreach (s; ["Enjoy", "Rosetta", "Code"].parallel(1)) {
        Thread.sleep(uniform(0, 1000).dur!"msecs");
        s.writeln;
    }
}
```



### Alternative version

```d
import tango.core.Thread;
import tango.io.Console;
import tango.math.Random;

void main() {
    (new Thread( { Thread.sleep(Random.shared.next(1000) / 1000.0); Cout("Enjoy").newline; } )).start;
    (new Thread( { Thread.sleep(Random.shared.next(1000) / 1000.0); Cout("Rosetta").newline; } )).start;
    (new Thread( { Thread.sleep(Random.shared.next(1000) / 1000.0); Cout("Code").newline; } )).start;
}
```



## Delphi


```Delphi
program ConcurrentComputing;

{$APPTYPE CONSOLE}

uses SysUtils, Classes, Windows;

type
  TRandomThread = class(TThread)
  private
    FString: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aString: string); overload;
  end;

constructor TRandomThread.Create(const aString: string);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FString := aString;
end;

procedure TRandomThread.Execute;
begin
  Sleep(Random(5) * 100);
  Writeln(FString);
end;

var
  lThreadArray: Array[0..2] of THandle;
begin
  Randomize;
  lThreadArray[0] := TRandomThread.Create('Enjoy').Handle;
  lThreadArray[1] := TRandomThread.Create('Rosetta').Handle;
  lThreadArray[2] := TRandomThread.Create('Stone').Handle;

  WaitForMultipleObjects(Length(lThreadArray), @lThreadArray, True, INFINITE);
end.
```



## dodo0


```dodo0
fun parprint -> text, return
(
   fork() -> return, throw
      println(text, return)
   | x
   return()
)
| parprint

parprint("Enjoy") ->
parprint("Rosetta") ->
parprint("Code") ->

exit()
```



## E


```e
def base := timer.now()
for string in ["Enjoy", "Rosetta", "Code"] {
    timer <- whenPast(base + entropy.nextInt(1000), fn { println(string) })
}
```


Nondeterminism from preemptive concurrency rather than a random number generator:


```e>def seedVat := <import:org.erights.e.elang.interp.seedVatAuthor
(<unsafe>)
for string in ["Enjoy", "Rosetta", "Code"] {
   seedVat <- (`
       fn string {
           println(string)
           currentVat <- orderlyShutdown("done")
       }
   `) <- get(0) <- (string)
}
```



## EchoLisp


```scheme

(lib 'tasks) ;; use the tasks library

(define (tprint line ) ;; task definition
		(writeln _TASK  line)
		#f )

(for-each  task-run ;; run three // tasks
      (map (curry make-task tprint) '(Enjoy Rosetta code )))

   →
   #task:id:66:running     Rosetta
   #task:id:67:running     code
   #task:id:65:running     Enjoy

```



## Egel


```Egel

import "prelude.eg"
import "io.ego"

using System
using IO

def main =
    let _ = par (par [_ -> print "enjoy\n"]
                    [_ -> print "rosetta\n"])
                [_ -> print "code\n"] in nop

```



## Elixir


```Elixir
defmodule Concurrent do
  def computing(xs) do
    Enum.each(xs, fn x ->
      spawn(fn ->
        Process.sleep(:rand.uniform(1000))
        IO.puts x
      end)
    end)
    Process.sleep(1000)
  end
end

Concurrent.computing ["Enjoy", "Rosetta", "Code"]
```


```txt

Rosetta
Code
Enjoy

```



## Erlang

hw.erl

```erlang
-module(hw).
-export([start/0]).

start() ->
   [ spawn(fun() ->  say(self(), X) end) || X <- ['Enjoy', 'Rosetta', 'Code'] ],
   wait(2),
   ok.

say(Pid,Str) ->
   io:fwrite("~s~n",[Str]),
   Pid ! done.

wait(N) ->
   receive
       done -> case N of
           0 -> 0;
           _N -> wait(N-1)
       end
   end.
```


running it

```erlang
|erlc hw.erl
|erl -run hw start -run init stop -noshell
```



## Euphoria


```euphoria
procedure echo(sequence s)
    puts(1,s)
    puts(1,'\n')
end procedure

atom task1,task2,task3

task1 = task_create(routine_id("echo"),{"Enjoy"})
task_schedule(task1,1)

task2 = task_create(routine_id("echo"),{"Rosetta"})
task_schedule(task2,1)

task3 = task_create(routine_id("echo"),{"Code"})
task_schedule(task3,1)

task_yield()
```


Output:
 Code
 Rosetta
 Enjoy

=={{header|F_Sharp|F#}}==
We define a parallel version of <code>Seq.iter</code> by using asynchronous workflows:

```fsharp
module Seq =
    let piter f xs =
        seq { for x in xs -> async { f x } }
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

let main() =  Seq.piter
                (System.Console.WriteLine:string->unit)
                ["Enjoy"; "Rosetta"; "Code";]

main()
```


With version 4 of the .NET framework and F# PowerPack 2.0 installed, it is possible to use the predefined <code>PSeq.iter</code> instead.


## Factor


```factor
USE: concurrency.combinators

{ "Enjoy" "Rosetta" "Code" } [ print ] parallel-each
```



## Forth

Many Forth implementations come with a simple cooperative task scheduler. Typically each task blocks on I/O or explicit use of the '''pause''' word. There is also a class of variables called "user" variables which contain task-specific data, such as the current base and stack pointers.


```forth
require tasker.fs
require random.fs

: task ( str len -- )
  64 NewTask 2 swap pass
  ( str len -- )
  10 0 do
    100 random ms
    pause 2dup cr type
  loop 2drop ;

: main
  s" Enjoy"   task
  s" Rosetta" task
  s" Code"    task
  begin pause single-tasking? until ;
main
```



## Fortran

Fortran doesn't have threads but there are several compilers that support OpenMP, e.g. gfortran and Intel. The following code has been tested with thw Intel 11.1 compiler on WinXP.


```Fortran
program concurrency
  implicit none
  character(len=*), parameter :: str1 = 'Enjoy'
  character(len=*), parameter :: str2 = 'Rosetta'
  character(len=*), parameter :: str3 = 'Code'
  integer                     :: i
  real                        :: h
  real, parameter             :: one_third = 1.0e0/3
  real, parameter             :: two_thirds = 2.0e0/3

  interface
     integer function omp_get_thread_num
     end function omp_get_thread_num
  end interface
  interface
     integer function omp_get_num_threads
     end function omp_get_num_threads
  end interface

  ! Use OpenMP to create a team of threads
  !$omp parallel do private(i,h)
  do i=1,20
     ! First time through the master thread output the number of threads
     ! in the team
     if (omp_get_thread_num() == 0 .and. i == 1) then
        write(*,'(a,i0,a)') 'Using ',omp_get_num_threads(),' threads'
     end if

     ! Randomize the order
     call random_number(h)

     !$omp critical
     if (h < one_third) then
        write(*,'(a)') str1
     else if (h < two_thirds) then
        write(*,'(a)') str2
     else
        write(*,'(a)') str3
     end if
     !$omp end critical
  end do
  !$omp end parallel do

end program concurrency
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64
' Compiled with -mt switch (to use threadsafe runtiume)
' The 'ThreadCall' functionality in FB is based internally on LibFFi (see [https://github.com/libffi/libffi/blob/master/LICENSE] for license)

Sub thread1()
  Print "Enjoy"
End Sub

Sub thread2()
  Print "Rosetta"
End Sub

Sub thread3()
  Print "Code"
End Sub

Print "Press any key to print next batch of 3 strings or ESC to quit"
Print

Do
  Dim t1 As Any Ptr = ThreadCall thread1
  Dim t2 As Any Ptr = ThreadCall thread2
  Dim t3 As Any Ptr = ThreadCall thread3
  ThreadWait t1
  ThreadWait t2
  ThreadWait t3
  Print
  Sleep
Loop While Inkey <> Chr(27)
```


Sample output

```txt

Press any key to print next batch of 3 strings or ESC to quit

Enjoy
Code
Rosetta

Enjoy
Rosetta
Code

```



## Go


### Channel

Simple and direct solution:  Start three goroutines, give each one a word.  Each sleeps, then returns the word on a channel.  The main goroutine prints words as they return.  The print loop represents a [[Checkpoint_synchronization|checkpoint]] -- main doesn't exit until all words have been returned and printed.

This solution also shows a good practice for generating random numbers in concurrent goroutines.  While certainly not needed for this RC task, in the more general case where you have a number of goroutines concurrently needing random numbers, the goroutines can suffer congestion if they compete heavily for the sole default library source.  This can be relieved by having each goroutine create its own non-sharable source.  Also particularly in cases where there might be a large number of concurrent goroutines, the source provided in subrepository rand package (exp/rand) can be a better choice than the standard library generator.  The subrepo generator requires much less memory for "state" and is much faster to seed.


```go
package main

import (
    "fmt"
    "golang.org/x/exp/rand"
    "time"
)

func main() {
    words := []string{"Enjoy", "Rosetta", "Code"}
    seed := uint64(time.Now().UnixNano())
    q := make(chan string)
    for i, w := range words {
        go func(w string, seed uint64) {
            r := rand.New(rand.NewSource(seed))
            time.Sleep(time.Duration(r.Int63n(1e9)))
            q <- w
        }(w, seed+uint64(i))
    }
    for i := 0; i < len(words); i++ {
        fmt.Println(<-q)
    }
}
```



### Afterfunc

time.Afterfunc combines the sleep and the goroutine start.  log.Println serializes output in the case goroutines attempt to print concurrently.  sync.WaitGroup is used directly as a checkpoint.

```go
package main

import (
    "log"
    "math/rand"
    "os"
    "sync"
    "time"
)

func main() {
    words := []string{"Enjoy", "Rosetta", "Code"}
    rand.Seed(time.Now().UnixNano())
    l := log.New(os.Stdout, "", 0)
    var q sync.WaitGroup
    q.Add(len(words))
    for _, w := range words {
        w := w
        time.AfterFunc(time.Duration(rand.Int63n(1e9)), func() {
            l.Println(w)
            q.Done()
        })
    }
    q.Wait()
}
```



### Select

This solution might stretch the intent of the task a bit.  It is concurrent but not parallel.  Also it doesn't sleep and doesn't call the random number generator explicity.  It works because the select statement is specified to make a "pseudo-random fair choice" among
multiple channel operations.

```go
package main

import "fmt"

func main() {
    w1 := make(chan bool, 1)
    w2 := make(chan bool, 1)
    w3 := make(chan bool, 1)
    for i := 0; i < 3; i++ {
        w1 <- true
        w2 <- true
        w3 <- true
        fmt.Println()
        for i := 0; i < 3; i++ {
            select {
            case <-w1:
                fmt.Println("Enjoy")
            case <-w2:
                fmt.Println("Rosetta")
            case <-w3:
                fmt.Println("Code")
            }
        }
    }
}
```

Output:

```txt

Code
Rosetta
Enjoy

Enjoy
Rosetta
Code

Rosetta
Enjoy
Code

```



## Groovy


```groovy
'Enjoy Rosetta Code'.tokenize().collect { w ->
    Thread.start {
        Thread.sleep(1000 * Math.random() as int)
        println w
    }
}.each { it.join() }
```



## Haskell


Note how the map treats the list of processes just like any other data.


```haskell
import Control.Concurrent

main = mapM_ forkIO [process1, process2, process3] where
  process1 = putStrLn "Enjoy"
  process2 = putStrLn "Rosetta"
  process3 = putStrLn "Code"
```


A more elaborated example using MVars and a random running time per thread.


```haskell
import Control.Concurrent
import System.Random

concurrent :: IO ()
concurrent = do
    var <- newMVar [] -- use an MVar to collect the results of each thread
    mapM_ (forkIO . task var) ["Enjoy", "Rosetta", "Code"] -- run 3 threads
    putStrLn "Press Return to show the results." -- while we wait for the user,
    -- the threads run
    _ <- getLine
    takeMVar var >>= mapM_ putStrLn -- read the results and show them on screen
    where
        -- "task" is a thread
        task v s = do
            randomRIO (1,10) >>= \r -> threadDelay (r * 100000) -- wait a while
            val <- takeMVar v -- read the MVar and block other threads from reading it
            -- until we write another value to it
            putMVar v (s : val) -- append a text string to the MVar and block other
            -- threads from writing to it unless it is read first
```


==Icon and {{header|Unicon}}==
The following code uses features exclusive to Unicon

```unicon
procedure main()
   L:=[ thread write("Enjoy"), thread write("Rosetta"), thread write("Code") ]
   every wait(!L)
end
```



## J


Example:


```j
   smoutput&>({~?~@#);:'Enjoy Rosetta Code'
Rosetta
Code
Enjoy
```


NOTES AND CAUTIONS:

1) While J's syntax and semantics is highly parallel, it is a deterministic sort of parallelism (analogous to the design of modern GPUs) and not the stochastic parallelism which is implied in this task specification (and which is usually obtained by timeslicing threads of control).  The randomness implemented here is orthogonal to the parallelism in the display (and you could remove <code>smoutput&</code> without altering the appearence, in this trivial example).

2) The current release of J (and the past implementations) do not implement hardware based concurrency.  This is partially an economic issue (since all of the current and past language implementations have been distributed for free, with terms which allow free distribution), and partially a hardware maturity issue (historically, most CPU multi-core development has been optimized for stochastic parallelism with minimal cheap support for large scale deterministic parallelism and GPUs have not been capable of supporting the kind of generality needed by J).

This state of affairs is likely to change, eventually (most likely this will be after greater than factor of 2 speedups from hardware parallelism are available for the J users in cases which are common and important enough to support the implementation).  But, for now, J's parallelism is entirely conceptual.


## Java

Uses CyclicBarrier to force all threads to wait until they're at the same point before executing the println, increasing the odds they'll print in a different order (otherwise, while the they may be executing in parallel, the threads are started sequentially and with such a short run-time, will usually output sequentially as well).


```java5
import java.util.concurrent.CyclicBarrier;

public class Threads
{
  public static class DelayedMessagePrinter implements Runnable
  {
    private CyclicBarrier barrier;
    private String msg;

    public DelayedMessagePrinter(CyclicBarrier barrier, String msg)
    {
      this.barrier = barrier;
      this.msg = msg;
    }

    public void run()
    {
      try
      {  barrier.await();  }
      catch (Exception e)
      {  }
      System.out.println(msg);
    }
  }

  public static void main(String[] args)
  {
    CyclicBarrier barrier = new CyclicBarrier(3);
    new Thread(new DelayedMessagePrinter(barrier, "Enjoy")).start();
    new Thread(new DelayedMessagePrinter(barrier, "Rosetta")).start();
    new Thread(new DelayedMessagePrinter(barrier, "Code")).start();
  }
}
```



## JavaScript


JavaScript now enjoys access to a concurrency library thanks to [http://en.wikipedia.org/wiki/Web_worker Web Workers]. The Web Workers specification defines an API for spawning background scripts. This first code is the background script and should be in the concurrent_worker.js file.

```javascript
self.addEventListener('message', function (event) {
  self.postMessage(event.data);
  self.close();
}, false);
```

This second block creates the workers, sends them a message and creates an event listener to handle the response.

```javascript
var words = ["Enjoy", "Rosetta", "Code"];
var workers = [];

for (var i = 0; i < words.length; i++) {
  workers[i] = new Worker("concurrent_worker.js");
  workers[i].addEventListener('message', function (event) {
    console.log(event.data);
  }, false);
  workers[i].postMessage(words[i]);
}
```



## Julia

```julia
words = ["Enjoy", "Rosetta", "Code"]

function sleepprint(s)
    sleep(rand())
    println(s)
end

@sync for word in words
    @async sleepprint(word)
end
```



## Kotlin

```scala
// version 1.1.2

import java.util.concurrent.CyclicBarrier

class DelayedMessagePrinter(val barrier: CyclicBarrier, val msg: String) : Runnable {
    override fun run() {
        barrier.await()
        println(msg)
    }
}

fun main(args: Array<String>) {
    val msgs = listOf("Enjoy", "Rosetta", "Code")
    val barrier = CyclicBarrier(msgs.size)
    for (msg in msgs) Thread(DelayedMessagePrinter(barrier, msg)).start()
}
```


Sample output:

```txt

Code
Rosetta
Enjoy

```



## LFE


```lisp

;;;
;;;   This is a straight port of the Erlang version.
;;;
;;;   You can run this under the LFE REPL as follows:
;;;
;;;   (slurp "concurrent-computing.lfe")
;;;   (start)
;;;
(defmodule concurrent-computing
  (export (start 0)))

(defun start ()
  (lc ((<- word '("Enjoy" "Rosetta" "Code")))
    (spawn (lambda () (say (self) word))))
  (wait 2)
  'ok)

(defun say (pid word)
  (lfe_io:format "~p~n" (list word))
  (! pid 'done))

(defun wait (n)
  (receive
    ('done (case n
             (0 0)
             (_n (wait (- n 1)))))))

```



## Logtalk

Works when using SWI-Prolog, XSB, or YAP as the backend compiler.

```logtalk
:- object(concurrency).

    :- initialization(output).

    output :-
        threaded((
            write('Enjoy'),
            write('Rosetta'),
            write('Code')
        )).

:- end_object.
```



## Lua


```lua
co = {}
co[1] = coroutine.create( function() print "Enjoy" end )
co[2] = coroutine.create( function() print "Rosetta" end )
co[3] = coroutine.create( function() print "Code" end )

math.randomseed( os.time() )
h = {}
i = 0
repeat
    j = math.random(3)
    if h[j] == nil then
       coroutine.resume( co[j] )
       h[j] = true
       i = i + 1
    end
until i == 3
```



## M2000 Interpreter

Each thread executed in same scope where created. We can use static variables, which are at thread level, so in this example we have three A$,one for each thread. Each thread can use Thread This to send command, or using a known handler (number which return to k) to execute commands to other threads. Commands are HOLD,RESTART, ERASE, INTERVAL, EXECUTE.

Each thread has own stack of values, which deleted when erased. We can '''Push''' values to top of stack, or use '''Data''' to push to end of stack. A '''Read''' statement read from top of stack. In the example we Flush module's stack (modules use parent stack, functions get own stack), we place with Data strings and we read it using Read M$. We place the M$ to Static A$ (this can be done one time, because if A$ as static exist then interpreter skip expression without execute it).

All threads of a module erased when a module exit. Any block of code inside { } in Thread run without concurrency, when we use Thread.Plan Concurrent (when a thread runs we can't change Plan). Other plan is the sequential. Interval can be set to milliseconds.

Threads actually runs in Wait loop. We can use Main.Task as a loop which is thread also. Threads can be run when we wait for input in m2000 console, or for events from M2000 GUI forms, also. Events always run in sequential form.


```M2000 Interpreter

Thread.Plan Concurrent
Module CheckIt {
      Flush  \\ empty stack of values
      Data "Enjoy", "Rosetta", "Code"
      For i=1 to 3 {
            Thread {
                  Print A$
                  Thread This Erase
            } As K
            Read M$
            Thread K Execute Static A$=M$
            Thread K Interval Random(500,1000)
            Threads
      }
      Rem : Wait 3000   ' we can use just a wait loop, or the main.task loop
      \\ main.task exit if all threads erased
      Main.Task 30 {
      }
\\ when module exit all threads from this module get a signal to stop.
\\ we can use Threads Erase to erase all threads.
\\ Also if we press Esc we do the same
}
CheckIt

\\ we can define again the module, and now we get three time each name, but not every time three same names.
\\ if we change to Threads.Plan Sequential we get always the three same names
\\ Also in concurrent plan we can use a block to ensure that statements run without other thread executed in parallel.

Module CheckIt {
      Flush  \\ empty stack of values
      Data "Enjoy", "Rosetta", "Code"
      For i=1 to 3 {
            Thread {
                  Print A$
                  Print A$
                  Print A$
                  Thread This Erase
            } As K
            Read M$
            Thread K Execute Static A$=M$
            Thread K Interval Random(500,530)
            Threads
      }
      Rem : Wait 3000   ' we can use just a wait loop, or the main.task loop
      \\ main.task exit if all threads erased
      Main.Task 30 {
      }
\\ when module exit all threads from this module get a signal to stop.
\\ we can use Threads Erase to erase all threads.
\\ Also if we press Esc we do the same
}
CheckIt


```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Parallelization requires Mathematica 7 or later

```Mathematica
ParallelDo[
    Pause[RandomReal[]];
    Print[s],
    {s, {"Enjoy", "Rosetta", "Code"}}
]
```



## Mercury

<lang>:- module concurrent_computing.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module thread.

main(!IO) :-
   spawn(io.print_cc("Enjoy\n"), !IO),
   spawn(io.print_cc("Rosetta\n"), !IO),
   spawn(io.print_cc("Code\n"), !IO).
```



## Neko


```ActionScript
/**
 Concurrent computing, in Neko
*/

var thread_create = $loader.loadprim("std@thread_create", 2);

var subtask = function(message) {
  $print(message, "\n");
}

/* The thread functions happen so fast as to look sequential */
thread_create(subtask, "Enjoy");
thread_create(subtask, "Rosetta");
thread_create(subtask, "Code");

/* slow things down */
var sys_sleep = $loader.loadprim("std@sys_sleep", 1);
var random_new = $loader.loadprim("std@random_new", 0);
var random_int = $loader.loadprim("std@random_int", 2);

var randomsleep = function(message) {
  var r = random_new();
  var sleep = random_int(r, 3);
  sys_sleep(sleep);
  $print(message, "\n");
}

$print("\nWith random delays\n");
thread_create(randomsleep, "Enjoy");
thread_create(randomsleep, "Rosetta");
thread_create(randomsleep, "Code");

/* Let the threads complete */
sys_sleep(4);
```


```txt
prompt$ nekoc threading.neko
prompt$ neko threading
Enjoy
Rosetta
Code

With random delays
Rosetta
Enjoy
Code
```



## Nim

Compile with <code>nim --threads:on c concurrent</code>:

```nim
const str = ["Enjoy", "Rosetta", "Code"]

var thr: array[3, Thread[int32]]

proc f(i:int32) {.thread.} =
  echo str[i]

for i in 0..thr.high:
  createThread(thr[i], f, int32(i))
joinThreads(thr)
```



### OpenMP

Compile with <code>nim --passC:"-fopenmp" --passL:"-fopenmp" c concurrent</code>:

```nim
const str = ["Enjoy", "Rosetta", "Code"]

for i in 0||2:
  echo str[i]
```



### Thread Pools

Compile with <code>nim --threads:on c concurrent</code>:

```nim
import threadpool
const str = ["Enjoy", "Rosetta", "Code"]

proc f(i) {.thread.} =
  echo str[i]

for i in 0..str.high:
  spawn f(i)
sync()
```



## Objeck


```objeck

bundle Default {
  class MyThread from Thread {
    New(name : String) {
      Parent(name);
    }

    method : public : Run(param : Base) ~ Nil {
      string := param->As(String);
      string->PrintLine();
    }
  }

  class Concurrent {
    New() {
    }

    function : Main(args : System.String[]) ~ Nil {
      t0 := MyThread->New("t0");
      t1 := MyThread->New("t1");
      t2 := MyThread->New("t2");

      t0->Execute("Enjoy"->As(Base));
      t1->Execute("Rosetta"->As(Base));
      t2->Execute("Code"->As(Base));
    }
  }
}

```



## OCaml



```ocaml
#directory "+threads"
#load "unix.cma"
#load "threads.cma"

let sleepy_print msg =
  Unix.sleep (Random.int 4);
  print_endline msg

let threads =
  List.map (Thread.create sleepy_print) ["Enjoy"; "Rosetta"; "Code"]

let () =
  Random.self_init ();
  List.iter (Thread.join) threads
```



## Oforth


Oforth uses tasks to implement concurrent computing. A task is scheduled using #& on a function, method, block, ...


```Oforth
#[ "Enjoy" println ] &
#[ "Rosetta" println ] &
#[ "Code" println ] &
```

mapParallel method can be used to map a runnable on each element of a collection and returns a collection of results. Here, we println the string and return string size.


```Oforth
[ "Enjoy", "Rosetta", "Code" ] mapParallel(#[ dup . size ])
```



## ooRexx


```ooRexx

-- this will launch 3 threads, with each thread given a message to print out.
-- I've added a stoplight to make each thread wait until given a go signal,
-- plus some sleeps to give the threads a chance to randomize the execution
-- order a little.
launcher = .launcher~new
launcher~launch

::class launcher
-- the launcher method.  Guarded is the default, but let's make this
-- explicit here
::method launch guarded

  runner1 = .runner~new(self, "Enjoy")
  runner2 = .runner~new(self, "Rosetta")
  runner3 = .runner~new(self, "Code")

  -- let's give the threads a chance to settle in to the
  -- starting line
  call syssleep 1

  guard off   -- release the launcher lock.  This is the starter's gun

-- this is a guarded method that the runners will call.  They
-- will block until the launch method releases the object guard
::method block guarded

::class runner
::method init
  use arg launcher, text
  reply  -- this creates the new thread

  call syssleep .5  -- try to mix things up by sleeping
  launcher~block    -- wait for the go signal
  call syssleep .5  -- add another sleep here
  say text

```



## Oz

The randomness comes from the unpredictability of thread scheduling (this is how I understand this exercise).


```oz
for Msg in ["Enjoy" "Rosetta" "Code"] do
   thread
      {System.showInfo Msg}
   end
end

```



## PARI/GP

Here is a GP implementation using the [http://pari.math.u-bordeaux.fr/cgi-bin/gitweb.cgi?p=pari.git;a=tree;h=refs/heads/bill-mt;hb=refs/heads/bill-mt bill-mt] branch:

```parigp
inline(func);
func(n)=print(["Enjoy","Rosetta","Code"][n]);
parapply(func,[1..3]);
```


This is a PARI implementation which uses <code>fork()</code> internally.  Note that the [[#C|C]] solutions can be used instead if desired; this program demonstrates the native PARI capabilities instead.

For serious concurrency, see Appendix B of the User's Guide to the PARI Library which discusses a solution using [[wp:Thread-local storage|tls]] on [[wp:POSIX Threads|pthreads]].  (There are nontrivial issues with using PARI in this environment, do not attempt to blindly implement a [[#C|C]] solution.)

```C
void
foo()
{
  if (pari_daemon()) {
    // Original
    if (pari_daemon()) {
      // Original
      pari_printf("Enjoy\n");
    } else {
      // Daemon #2
      pari_printf("Code\n");
    }
  } else {
    // Daemon #1
    pari_printf("Rosetta\n");
  }
}
```


See also [http://pari.math.u-bordeaux1.fr/Events/PARI2012/talks/pareval.pdf Bill Allombert's slides on parallel programming in GP].


## Perl

```perl
use threads;
use Time::HiRes qw(sleep);

$_->join for map {
    threads->create(sub {
        sleep rand;
        print shift, "\n";
    }, $_)
} qw(Enjoy Rosetta Code);
```


Or using coroutines provided by {{libheader|Coro}}

```perl
use feature qw( say );
use Coro;
use Coro::Timer qw( sleep );

$_->join for map {
    async {
        sleep rand;
        say @_;
    } $_
} qw( Enjoy Rosetta Code );

```



## Perl 6

```perl6>my @words = <Enjoy Rosetta Code
;
@words.race(:batch(1)).map: { sleep rand; say $_ };
```

```txt
Code
Rosetta
Enjoy
```



## Phix

Without the sleep it is almost always Enjoy Rosetta Code, because create_thread() is more costly than echo(), as the former has to create a new call stack etc.

The lock prevents the displays from mangling each other.

```Phix
procedure echo(string s)
    sleep(rand(100)/100)
    enter_cs()
    puts(1,s)
    puts(1,'\n')
    leave_cs()
end procedure

constant threads = {create_thread(routine_id("echo"),{"Enjoy"}),
                    create_thread(routine_id("echo"),{"Rosetta"}),
                    create_thread(routine_id("echo"),{"Code"})}

wait_thread(threads)
puts(1,"done")
{} = wait_key()
```



## PicoLisp


### Using background tasks


```PicoLisp
(for (N . Str) '("Enjoy" "Rosetta" "Code")
   (task (- N) (rand 1000 4000)              # Random start time 1 .. 4 sec
      Str Str                                # Closure with string value
      (println Str)                          # Task body: Print the string
      (task @) ) )                           # and stop the task
```


### Using child processes


```PicoLisp
(for Str '("Enjoy" "Rosetta" "Code")
   (let N (rand 1000 4000)                   # Randomize
      (unless (fork)                         # Create child process
         (wait N)                            # Wait 1 .. 4 sec
         (println Str)                       # Print string
         (bye) ) ) )                         # Terminate child process
```



## Pike

Using POSIX threads:

```Pike
int main() {
	// Start threads and wait for them to finish
	({
		Thread.Thread(write, "Enjoy\n"),
		Thread.Thread(write, "Rosetta\n"),
		Thread.Thread(write, "Code\n")
	})->wait();

	// Exit program
	exit(0);
}
```

Output:
 Enjoy
 Rosetta
 Code

Using Pike's backend:

```Pike
int main(int argc, array argv)
{
    call_out(write, random(1.0), "Enjoy\n");
    call_out(write, random(1.0), "Rosetta\n");
    call_out(write, random(1.0), "Code\n");
    call_out(exit, 1, 0);
    return -1; // return -1 starts the backend which makes Pike run until exit() is called.
}
```

Output:
 Rosetta
 Code
 Enjoy


## PowerShell

Using Background Jobs:

```Powershell
$Strings = "Enjoy","Rosetta","Code"

$SB = {param($String)Write-Output $String}

foreach($String in $Strings) {
    Start-Job -ScriptBlock $SB -ArgumentList $String | Out-Null
    }

Get-Job | Wait-Job | Receive-Job
Get-Job | Remove-Job
```


Using .NET Runspaces:

```Powershell
$Strings = "Enjoy","Rosetta","Code"

$SB = {param($String)Write-Output $String}

$Pool = [RunspaceFactory]::CreateRunspacePool(1, 3)
$Pool.ApartmentState = "STA"
$Pool.Open()
foreach ($String in $Strings) {
    $Pipeline  = [System.Management.Automation.PowerShell]::create()
    $Pipeline.RunspacePool = $Pool
    [void]$Pipeline.AddScript($SB).AddArgument($String)
    $AsyncHandle = $Pipeline.BeginInvoke()
    $Pipeline.EndInvoke($AsyncHandle)
    $Pipeline.Dispose()
    }
$Pool.Close()
```



## Prolog


This example works in SWI-Prolog. It may work in other Prolog implementations too.

Create a separate thread for each word.  Join the threads to make sure they complete before the program exits.


```prolog
main :-
    thread_create(say("Enjoy"),A,[]),
    thread_create(say("Rosetta"),B,[]),
    thread_create(say("Code"),C,[]),
    thread_join(A,_),
    thread_join(B,_),
    thread_join(C,_).

say(Message) :-
    Delay is random_float,
    sleep(Delay),
    writeln(Message).
```



## PureBasic


```PureBasic
Global mutex = CreateMutex()

Procedure Printer(*str)
	LockMutex(mutex)
	PrintN( PeekS(*str) )
	UnlockMutex(mutex)
EndProcedure

If OpenConsole()
	LockMutex(mutex)
	thread1 = CreateThread(@Printer(), @"Enjoy")
	thread2 = CreateThread(@Printer(), @"Rosetta")
	thread3 = CreateThread(@Printer(), @"Code")
	UnlockMutex(mutex)

	WaitThread(thread1)
	WaitThread(thread2)
	WaitThread(thread3)

	Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
	Input()

	CloseConsole()
EndIf

FreeMutex(mutex)
```



## Python

Using asyncio module (I know almost nothing about it, so feel free to improve it :-)):

```python
import asyncio


async def print_(string: str) -> None:
    print(string)


async def main():
    strings = ['Enjoy', 'Rosetta', 'Code']
    coroutines = map(print_, strings)
    await asyncio.gather(*coroutines)


if __name__ == '__main__':
    asyncio.run(main())
```


Using the new to Python 3.2 [http://docs.python.org/release/3.2/library/concurrent.futures.html concurrent.futures library] and choosing to use processes over threads; the example will use up to as many processes as your machine has cores. This doesn't however guarantee an order of sub-process results.

```python
Python 3.2 (r32:88445, Feb 20 2011, 21:30:00) [MSC v.1500 64 bit (AMD64)] on win 32
Type "help", "copyright", "credits" or "license" for more information.
>>> from concurrent import futures
>>> with futures.ProcessPoolExecutor() as executor:
...   _ = list(executor.map(print, 'Enjoy Rosetta Code'.split()))
...
Enjoy
Rosetta
Code
>>>
```


```python
import threading
import random

def echo(text):
    print(text)

threading.Timer(random.random(), echo, ("Enjoy",)).start()
threading.Timer(random.random(), echo, ("Rosetta",)).start()
threading.Timer(random.random(), echo, ("Code",)).start()
```


Or, by using a for loop to start one thread per list entry, where our list is our set of source strings:


```python
import threading
import random

def echo(text):
    print(text)

for text in ["Enjoy", "Rosetta", "Code"]:
    threading.Timer(random.random(), echo, (text,)).start()
```



###  threading.Thread


```python
import random, sys, time
import threading

lock = threading.Lock()

def echo(s):
    time.sleep(1e-2*random.random())
    # use `.write()` with lock due to `print` prints empty lines occasionally
    with lock:
        sys.stdout.write(s)
        sys.stdout.write('\n')

for line in 'Enjoy Rosetta Code'.split():
    threading.Thread(target=echo, args=(line,)).start()
```



###  multiprocessing


```python
from __future__ import print_function
from multiprocessing import Pool

def main():
    p = Pool()
    p.map(print, 'Enjoy Rosetta Code'.split())

if __name__=="__main__":
    main()
```



###  twisted


```python
import random
from twisted.internet    import reactor, task, defer
from twisted.python.util import println

delay = lambda: 1e-4*random.random()
d = defer.DeferredList([task.deferLater(reactor, delay(), println, line)
                        for line in 'Enjoy Rosetta Code'.split()])
d.addBoth(lambda _: reactor.stop())
reactor.run()
```



###  gevent


```python
from __future__ import print_function
import random
import gevent

delay = lambda: 1e-4*random.random()
gevent.joinall([gevent.spawn_later(delay(), print, line)
               for line in 'Enjoy Rosetta Code'.split()])
```



## Racket


Threads provide a simple API for concurrent programming.

```racket

#lang racket
(for ([str '("Enjoy" "Rosetta" "Code")])
  (thread (λ () (displayln str))))

```


In addition to "thread" which is implemented as green threads (useful for IO etc), Racket has "futures" and "places" which are similar tools for using multiple OS cores.


## Raven


```raven
[ 'Enjoy' 'Rosetta' 'Code' ] as $words

thread talker
    $words pop "%s\n"
    repeat dup print
        500 choose ms

talker as a
talker as b
talker as c
```



## Rhope

```rhope
Main(0,0)
|:
    Print["Enjoy"]
    Print["Rosetta"]
    Print["Code"]
:|
```

In Rhope, expressions with no shared dependencies run in parallel by default.


## Ruby


```ruby
%w{Enjoy Rosetta Code}.map do |x|
    Thread.new do
        sleep rand
        puts x
    end
end.each do |t|
  t.join
end
```



## Rust

```rust
extern crate rand;
use std::thread;
use rand::thread_rng;
use rand::distributions::{Range, IndependentSample};

fn main() {
    let mut rng = thread_rng();
    let rng_range = Range::new(0u32, 100);
    for word in "Enjoy Rosetta Code".split_whitespace() {
        let snooze_time = rng_range.ind_sample(&mut rng);
        let local_word = word.to_owned();
        std::thread::spawn(move || {
            thread::sleep_ms(snooze_time);
            println!("{}", local_word);
        });
    }
    thread::sleep_ms(1000);
}
```



## Scala


```scala
import scala.actors.Futures
List("Enjoy", "Rosetta", "Code").map { x =>
    Futures.future {
      Thread.sleep((Math.random * 1000).toInt)
       println(x)
    }
}.foreach(_())
```



## Scheme


```scheme
(parallel-execute (lambda () (print "Enjoy"))
                  (lambda () (print "Rosetta"))
                  (lambda () (print "Code")))
```



## Sidef

A very basic threading support is provided by the '''Block.fork()''' method:

```ruby>var a = <Enjoy Rosetta Code


a.map{|str|
    {   Sys.sleep(1.rand)
        say str
    }.fork
}.map{|thr| thr.wait }
```


```txt

Enjoy
Code
Rosetta

```



## Swift

Using Grand Central Dispatch with concurrent queues.

```Swift
import Foundation

let myList = ["Enjoy", "Rosetta", "Code"]

for word in myList {
    dispatch_async(dispatch_get_global_queue(0, 0)) {
        NSLog(word)
    }
}

dispatch_main()
```

```txt

2015-02-05 10:15:01.831 rosettaconcurrency[1917:37905] Code
2015-02-05 10:15:01.831 rosettaconcurrency[1917:37902] Enjoy
2015-02-05 10:15:01.831 rosettaconcurrency[1917:37904] Rosetta

```


## VBA

Three tasks scheduled for the same time with OnTime. The last scheduled task gets executed first.

```vb
Private Sub Enjoy()
    Debug.Print "Enjoy"
End Sub
Private Sub Rosetta()
    Debug.Print "Rosetta"
End Sub
Private Sub Code()
    Debug.Print "Code"
End Sub
Public Sub concurrent()
    when = Now + TimeValue("00:00:01")
    Application.OnTime when, "Enjoy"
    Application.OnTime when, "Rosetta"
    Application.OnTime when, "Code"
End Sub
```



## Tcl

Assuming that "random" means that we really want the words to appear in random (rather then "undefined" or "arbitrary") order:


```tcl
after [expr int(1000*rand())] {puts "Enjoy"}
after [expr int(1000*rand())] {puts "Rosetta"}
after [expr int(1000*rand())] {puts "Code"}
```


will execute each line after a randomly chosen number (0...1000) of milliseconds.

A step towards "undefined" would be to use <tt>after idle</tt>, which is Tcl for "do this whenever you get around to it". Thus:


```tcl
after idle {puts "Enjoy"}
after idle {puts "Rosetta"}
after idle {puts "Code"}
```


(While no particular order is guaranteed by the Tcl spec, the current implementations will all execute these in the order in which they were added to the idle queue).

It's also possible to use threads for this. Here we do this with the built-in thread-pool support:

```tcl
package require Thread
set pool [tpool::create -initcmd {
    proc delayPrint msg {
        after [expr int(1000*rand())]
        puts $msg
    }
}]
tpool::post -detached $pool [list delayPrint "Enjoy"]
tpool::post -detached $pool [list delayPrint "Rosetta"]
tpool::post -detached $pool [list delayPrint "Code"]
tpool::release $pool
after 1200 ;# Give threads time to do their work
exit
```



## UnixPipes


```bash
(echo "Enjoy" & echo "Rosetta"& echo "Code"&)
```



## Visual Basic .NET



```vbnet
Imports System.Threading

Module Module1
   Public rnd As New Random

   Sub Main()
       Dim t1 As New Thread(AddressOf Foo)
       Dim t2 As New Thread(AddressOf Foo)
       Dim t3 As New Thread(AddressOf Foo)

       t1.Start("Enjoy")
       t2.Start("Rosetta")
       t3.Start("Code")

       t1.Join()
       t2.Join()
       t3.Join()

   End Sub

   Sub Foo(ByVal state As Object)
       Thread.Sleep(rnd.Next(1000))
       Console.WriteLine(state)
   End Sub

End Module
```


### Alternative version

[https://tio.run/##TY9PC8IwDMXv@xRhpw60oODFm@gEQUWs4Llbg6t0zWjrn3362bmBvssjCfnl5VlMS3LYdbu6IRc8iNYHrPmlciiVtrfkQOphEAabJRC10TU4q2Dl4YgvOEurqGbZdyYeBRyktmPZ6ySdNAYN35LLZVmxNLd3auFMHkOQsCaFKReN0YGlkGaTHsL8D9BrCMSFQWxYPM6P@A5svsgyWEaC9WSQX50OuNcW/7fzmDQCh8ZYJL0PL3XdBw Try It Online!]

```vbnet
Imports System.Threading
Module Module1
    Dim rnd As New Random()
    Sub Main()
        Parallel.ForEach("Enjoy Rosetta Code".Split(" "), Sub(s)
            Thread.Sleep(rnd.Next(25)) : Console.WriteLine(s)
        End Sub)
    End Sub
End Module
```

```txt
Rosetta
Enjoy
Code
```



## zkl


```zkl
fcn{println("Enjoy")}.launch();  // thread
fcn{println("Rosetta")}.strand();  // co-op thread
fcn{println("Code")}.future();  // another thread type
```

```txt

Rosetta
Code
Enjoy

```


