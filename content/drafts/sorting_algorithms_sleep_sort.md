+++
title = "Sorting algorithms/Sleep sort"
description = ""
date = 2019-09-17T17:44:04Z
aliases = []
[extra]
id = 9934
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}{{Sorting Algorithm}}
In general, sleep sort works by starting a separate task for each item to be sorted, where each task sleeps for an interval corresponding to the item's sort key, then emits the item.  Items are then collected sequentially in time.

Task:  Write a program that implements sleep sort.  Have it accept non-negative integers on the command line and print the integers in sorted order.  If this is not idomatic in your language or environment, input and output may be done differently.  Enhancements for optimization, generalization, practicality, robustness, and so on are not required.

Sleep sort was [https://archive.fo/xhGo presented] anonymously on 4chan and has been [http://news.ycombinator.com/item?id=2657277 discussed] on Hacker News.


## Ada


```Ada
with Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure SleepSort is
   task type PrintTask (num : Integer);
   task body PrintTask is begin
      delay Duration (num) / 100.0;
      Ada.Text_IO.Put(num'Img);
   end PrintTask;
   type TaskAcc is access PrintTask;
   TaskList : array (1 .. Argument_Count) of TaskAcc;
begin
   for i in TaskList'Range loop
      TaskList(i) := new PrintTask(Integer'Value(Argument(i)));
   end loop;
end SleepSort;
```

{{out}}

```txt
./sleepsort 35 21 11 1 2 27 32 7 42 20 50 42 25 41 43 14 46 20 30 8
 1 2 7 8 11 14 20 20 21 25 27 30 32 35 41 42 42 43 46 50
```



## APL


```APL

sleepsort←{{r}⎕TSYNC{r,←⊃⍵,⎕DL ⍵}&¨⍵,r←⍬}

```



## Bash


```bash

function sleep_and_echo {
  sleep "$1"
  echo "$1"
}

for val in "$@"; do
  sleep_and_echo "$val" &
done

wait

```


{{out}}


```txt

$ ./sleep_sort.sh 35 21 11 1 2 27 32 7 42 20 50 42 25 41 43 14 46 20 30 8
1
2
7
8
11
14
20
20
21
25
27
30
32
35
41
42
42
43
46
50

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
This does not explicitly 'sleep', but uses timers to implement the different delays.

```bbcbasic
      INSTALL @lib$+"TIMERLIB"

      DIM test%(9)
      test%() = 4, 65, 2, 31, 0, 99, 2, 83, 782, 1

      FOR i% = 0 TO DIM(test%(),1)
        p% = EVAL("!^PROCtask" + STR$(i%))
        tid% = FN_ontimer(100 + test%(i%), p%, 0)
      NEXT

      REPEAT
        WAIT 0
      UNTIL FALSE

      DEF PROCtask0 : PRINT test%(0) : ENDPROC
      DEF PROCtask1 : PRINT test%(1) : ENDPROC
      DEF PROCtask2 : PRINT test%(2) : ENDPROC
      DEF PROCtask3 : PRINT test%(3) : ENDPROC
      DEF PROCtask4 : PRINT test%(4) : ENDPROC
      DEF PROCtask5 : PRINT test%(5) : ENDPROC
      DEF PROCtask6 : PRINT test%(6) : ENDPROC
      DEF PROCtask7 : PRINT test%(7) : ENDPROC
      DEF PROCtask8 : PRINT test%(8) : ENDPROC
      DEF PROCtask9 : PRINT test%(9) : ENDPROC
```

'''Output:'''

```txt

         0
         1
         2
         2
         4
        31
        65
        83
        99
       782

```


=={{header|Brainfuck}}==

```C

>>>>>,----------[++++++++
++[->+>+<<]>+>[-<<+>>]+++
+++++[-<------>]>>+>,----
------<<+[->>>>>+<<<<<]>>
]>>>[<<<<[<<<[->>+<<[->+>
[-]<<]]>[-<+>]>[-<<<.>>>>
->>>>>[>>>>>]<-<<<<[<<<<<
]+<]<<<<]>>>>>[>>>>>]<]

```

Not exactly 'sleep' sort but it is similar, it inputs an array and in each iteration reduces elements by 1 and prints the number if result is 0.

Input: 1539\n

Output: 1359


## C


```cpp
#include <iostream>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main(int c, char **v)
{
        while (--c > 1 && !fork());
        sleep(c = atoi(v[c]));
        printf("%d\n", c);
        wait(0);
        return 0;
}
```

Running it:<lang>% ./a.out 5 1 3 2 11 6 4
1
2
3
4
5
6
11
```

If you worry about time efficiency of this sorting algorithm (ha!), you can make it a 100 times faster by replacing the <code>sleep(...</code> with <code>usleep(10000 * (c = atoi(v[c])))</code>.  The smaller the coefficient, the faster it is, but make sure it's not comparable to your kernel clock ticks or the wake up sequence will be wrong.


## C++


```cpp

#include <chrono>
#include <iostream>
#include <thread>
#include <vector>

int main(int argc, char* argv[]) {
  std::vector<std::thread> threads;

  for (int i = 1; i < argc; ++i) {
    threads.emplace_back([i, &argv]() {
      int arg = std::stoi(argv[i]);
      std::this_thread::sleep_for(std::chrono::seconds(arg));
      std::cout << argv[i] << std::endl;
    });
  }

  for (auto& thread : threads) {
    thread.join();
  }
}

```

{{out}}

```txt

./a.out 8 15 14 9 17 20 16 24 6 24 21 23 19 23 19
6
8
9
14
15
16
17
19
19
20
21
23
23
24
24

```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

class Program
{
    static void ThreadStart(object item)
    {
        Thread.Sleep(1000 * (int)item);
        Console.WriteLine(item);
    }

    static void SleepSort(IEnumerable<int> items)
    {
        foreach (var item in items)
        {
            new Thread(ThreadStart).Start(item);
        }
    }

    static void Main(string[] arguments)
    {
        SleepSort(arguments.Select(int.Parse));
    }
}
```



### Using Tasks



```csharp
var input = new[] { 1, 9, 2, 1, 3 };

foreach (var n in input)
	Task.Run(() =>
	{
		Thread.Sleep(n * 1000);
		Console.WriteLine(n);
	});

```


Output, i.e. in LINQPad:


```txt
1
1
2
3
9
```



## Clojure

Using core.async

```clojure
(ns sleepsort.core
  (require [clojure.core.async :as async :refer [chan go <! <!! >! timeout]]))

(defn sleep-sort [l]
  (let [c (chan (count l))]
    (doseq [i l]
      (go (<! (timeout (* 1000 i)))
          (>! c i)))
    (<!! (async/into [] (async/take (count l) c)))))
```


```clojure
(sleep-sort [4 5 3 1 2 7 6])
;=> [1 2 3 4 5 6 7]
```



## CoffeeScript

{{works_with|node.js}}

```coffeescript

after = (s, f) -> setTimeout f, s*1000

# Setting Computer Science back at least a century, maybe more,
# this algorithm sorts integers using a highly parallelized algorithm.
sleep_sort = (arr) ->
  for n in arr
    do (n) -> after n, -> console.log n

do ->
  input = (parseInt(arg) for arg in process.argv[2...])
  sleep_sort input

```

output
<lang>
> time coffee sleep_sort.coffee 5, 1, 3, 4, 2
1
2
3
4
5

real	0m5.184s
user	0m0.147s
sys	0m0.024s

```



## Common Lisp

{{works_with|SBCL}}

```lisp
(defun sleeprint(n)
    (sleep (/ n 10))
    (format t "~a~%" n))

(loop for arg in (cdr sb-ext:*posix-argv*) doing
      (sb-thread:make-thread (lambda() (sleeprint (parse-integer arg)))))

(loop while (not (null (cdr (sb-thread:list-all-threads)))))
```

{{Out}}

```txt
$ sbcl --script ss.cl 3 1 4 1 5
1
1
3
4
5

```



## D


```d

import core.thread, std.concurrency, std.datetime,
    std.stdio, std.algorithm, std.conv;

void main(string[] args)
{
    if (!args.length)
        return;

    foreach (number; args[1 .. $].map!(to!uint))
        spawn((uint num) {
            Thread.sleep(dur!"msecs"(10 * num));
            writef("%d ", num);
        }, number);

    thread_joinAll();
}


```

{{out}}

```txt
sorting_algorithms_sleep_sort 1 6 2 5 3 4
1 2 3 4 5 6
```



## Dart


```dart

void main() async {
  Future<void> sleepsort(Iterable<int> input) => Future.wait(input
      .map((i) => Future.delayed(Duration(milliseconds: i), () => print(i))));

  await sleepsort([3, 10, 2, 120, 122, 121, 54]);
}

```

{{out}}

```txt

2
3
10
54
120
121
122

```



## Delphi


```Delphi
program SleepSortDemo;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes;

type
  TSleepThread = class(TThread)
  private
    FValue: Integer;
    FLock: PRTLCriticalSection;
  protected
    constructor Create(AValue: Integer; ALock: PRTLCriticalSection);
    procedure Execute; override;
  end;

constructor TSleepThread.Create(AValue: Integer; ALock: PRTLCriticalSection);
begin
  FValue:= AValue;
  FLock:= ALock;
  inherited Create(False);
end;

procedure TSleepThread.Execute;
begin
  Sleep(1000 * FValue);
  EnterCriticalSection(FLock^);
  Write(FValue:3);
  LeaveCriticalSection(FLock^);
end;

var
  A: array[0..15] of Integer;
  Handles: array[0..15] of THandle;
  Threads: array[0..15] of TThread;
  Lock: TRTLCriticalSection;
  I: Integer;

begin
  for I:= Low(A) to High(A) do
    A[I]:= Random(15);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;

  InitializeCriticalSection(Lock);
  for I:= Low(A) to High(A) do begin
    Threads[I]:= TSleepThread.Create(A[I], @Lock);
    Handles[I]:= Threads[I].Handle;
  end;
  WaitForMultipleObjects(Length(A), @Handles, True, INFINITE);
  for I:= Low(A) to High(A) do
    Threads[I].Free;
  DeleteCriticalSection(Lock);

  Writeln;
  ReadLn;
end.
```

Output:

```txt

  0  0 12  3  4 10  4  2  5  6  1  7  1 12  0  4
  0  0  0  1  1  2  3  4  4  4  5  6  7 10 12 12

```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'routines;
import extensions'threading;
import system'threading;

static sync = new object();

extension op
{
    sleepSort()
    {
        self.forEach:(n)
        {
            threadControl.start(()
            {
                threadControl.sleep(1000 * n);

                lock(sync)
                {
                    console.printLine(n)
                }
            })
        }
    }
}

public program()
{
    program_arguments.skipping:1.selectBy(mssgconst toInt<convertorOp>[0]).toArray().sleepSort();

    console.readChar()
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Sort do
  def sleep_sort(args) do
    Enum.each(args, fn(arg) -> Process.send_after(self, arg, 5 * arg) end)
    loop(length(args))
  end

  defp loop(0), do: :ok
  defp loop(n) do
    receive do
        num -> IO.puts num
               loop(n - 1)
    end
  end
end

Sort.sleep_sort [2, 4, 8, 12, 35, 2, 12, 1]
```


{{out}}

```txt

1
2
2
4
8
12
12
35

```



## Erlang


```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname sleepsort

main(Args) ->
    lists:foreach(fun(Arg) ->
                          timer:send_after(5 * list_to_integer(Arg), self(), Arg)
                  end, Args),
    loop(length(Args)).

loop(0) ->
    ok;
loop(N) ->
    receive
        Num ->
            io:format("~s~n", [Num]),
            loop(N - 1)
    end.
```

{{out}}

```txt
./sleepsort 2 4 8 12 35 2 12 1
1
2
2
4
8
12
12
35
```



## Euphoria


```euphoria
include get.e

integer count

procedure sleeper(integer key)
    ? key
    count -= 1
end procedure

sequence s, val
atom task

s = command_line()
s = s[3..$]
if length(s)=0 then
    puts(1,"Nothing to sort.\n")
else
    count = 0
    for i = 1 to length(s) do
        val = value(s[i])
        if val[1] = GET_SUCCESS then
            task = task_create(routine_id("sleeper"),{val[2]})
            task_schedule(task,{val[2],val[2]}/10)
            count += 1
        end if
    end for

    while count do
        task_yield()
    end while
end if
```



## Factor



```Factor

USING: threads calendar concurrency.combinators ;

: sleep-sort ( seq -- ) [ dup seconds sleep . ] parallel-each ;

```


Usage:


```Factor

{ 1 9 2 6 3 4 5 8 7 0 } sleep-sort

```



## FreeBASIC

Can't use FreeBASIC '''sleep''' since it halts the program.
Instead it uses a second array filled with times based on the value of number, this array is check against the timer. If the timer is past the stored time the value is printed.

```freebasic
' version 21-10-2016
' compile with: fbc -s console
' compile with: fbc -s console -exx (for bondry check on the array's)
' not very well suited for large numbers and large array's
' positive numbers only

Sub sandman(sleepy() As ULong)
    Dim As Long lb = LBound(sleepy)
    Dim As Long ub = UBound(sleepy)
    Dim As Long i, count = ub
    Dim As Double wakeup(lb To ub)
    Dim As Double t = Timer

    For i = lb To ub
        wakeup(i) = sleepy(i) +1 + t
    Next

    Do
        t = Timer
        For i = lb To ub
            If wakeup(i) <= t Then
                Print Using "####";sleepy(i);
                wakeup(i) = 1e9 ' mark it as used
                count = count -1
            End If
        Next
        Sleep (1 - (Timer - t)) * 300, 1 ' reduce CPU load
    Loop Until count < lb

End Sub

' ------=< MAIN >=------

Dim As ULong i, arr(10)
Dim As ULong lb = LBound(arr)
Dim As ULong ub = UBound(arr)

Randomize Timer
For i = lb To ub -1 ' leave last one zero
    arr(i) = Int(Rnd * 10) +1
Next

Print "unsorted  ";
For i = lb To ub
    Print Using "####";arr(i);
Next
Print : Print

Print "  sorted  ";
sandman(arr())

Print : Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
unsorted     5   2   5   6   4   6   9   5   1   2   0

  sorted     0   1   2   2   4   5   5   5   6   6   9
```



## Fortran


```Fortran

program sleepSort
    use omp_lib
    implicit none
    integer::nArgs,myid,i,stat
    integer,allocatable::intArg(:)
    character(len=5)::arg

    !$omp master
    nArgs=command_argument_count()
    if(nArgs==0)stop ' : No argument is given !'
    allocate(intArg(nArgs))
    do i=1,nArgs
        call get_command_argument(i, arg)
	read(arg,'(I5)',iostat=stat)intArg(i)
	if(intArg(i)<0 .or. stat/=0) stop&
        &' :Only 0 or positive integer allowed !'
    end do
    call omp_set_num_threads(nArgs)
    !$omp end master


    !$omp parallel private(myid)
    myid =omp_get_thread_num()
    call sleepNprint(intArg(myid+1))
    !$omp end parallel

  contains
	subroutine sleepNprint(nSeconds)
	    integer::nSeconds
            call sleep(nSeconds)
	    print*,nSeconds
	end subroutine sleepNprint
end program sleepSort

```

Compile and Output:

```txt

gfortran -fopenmp sleepSort.f90 -o sleepSort
./sleepSort 0 3 1 4 1 5 9
0
1
1
3
4
5
9

```



## Go


```go
package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"time"
)

func main() {
	out := make(chan uint64)
	for _, a := range os.Args[1:] {
		i, err := strconv.ParseUint(a, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		go func(n uint64) {
			time.Sleep(time.Duration(n) * time.Millisecond)
			out <- n
		}(i)
	}
	for _ = range os.Args[1:] {
		fmt.Println(<-out)
	}
}
```

Usage and output:

```txt
./sleepsort 3 1 4 1 5 9
1
1
3
4
5
9

```



## Groovy


```groovy

@Grab(group = 'org.codehaus.gpars', module = 'gpars', version = '1.2.1')
import groovyx.gpars.GParsPool

GParsPool.withPool args.size(), {
    args.eachParallel {
        sleep(it.toInteger() * 10)
        println it
    }
}

```


Sample Run:

```txt
> groovy sleepsort.groovy 42 23 16 15 8 4
4
8
15
16
23
42
```



## Haskell



```haskell
import System.Environment
import Control.Concurrent
import Control.Monad

sleepSort :: [Int] -> IO ()
sleepSort values = do
        chan <- newChan
        forM_ values (\time -> forkIO (threadDelay (50000 * time) >> writeChan chan time))
        forM_ values (\_ -> readChan chan >>= print)

main :: IO ()
main = getArgs >>= sleepSort . map read
```



### Using mapConcurrently


```haskell
import System.Environment
import Control.Concurrent
import Control.Concurrent.Async

sleepSort :: [Int] -> IO ()
sleepSort = (() <$) . mapConcurrently (\x -> threadDelay (x*10^4) >> print x)

main :: IO ()
main = getArgs >>= sleepSort . map read
```


This is problematic for inputs with multiple duplicates like <code>[1,2,3,1,4,1,5,1]</code> because simultaneous <code>print</code>s are done concurrently and the 1s and newlines get output in jumbled up order. The channels-based version above doesn't have this problem.

=={{header|Icon}} and {{header|Unicon}}==

The following solution only works in Unicon.


```unicon
procedure main(A)
    every insert(t:=set(),mkThread(t,!A))
    every spawn(!t)    # start threads as closely grouped as possible
    while (*t > 0) do write(<<@)
end

procedure mkThread(t,n)    # 10ms delay scale factor
    return create (delay(n*10),delete(t,&current),n@>&main)
end
```


Sample run:


```txt

->ss 3 1 4 1 5 9 2 6
1
1
2
3
4
5
6
9
->

```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.concurrent.CountDownLatch;

public class SleepSort {
	public static void sleepSortAndPrint(int[] nums) {
		final CountDownLatch doneSignal = new CountDownLatch(nums.length);
		for (final int num : nums) {
			new Thread(new Runnable() {
				public void run() {
					doneSignal.countDown();
					try {
						doneSignal.await();

						//using straight milliseconds produces unpredictable
						//results with small numbers
						//using 1000 here gives a nifty demonstration
						Thread.sleep(num * 1000);
						System.out.println(num);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			}).start();
		}
	}
	public static void main(String[] args) {
		int[] nums = new int[args.length];
		for (int i = 0; i < args.length; i++)
			nums[i] = Integer.parseInt(args[i]);
		sleepSortAndPrint(nums);
	}
}
```

Output (using "3 1 4 5 2 3 1 6 1 3 2 5 4 6" as arguments):

```txt
1
1
1
2
2
3
3
3
4
4
5
5
6
6
```



## JavaScript


```javascript
Array.prototype.timeoutSort = function (f) {
	this.forEach(function (n) {
		setTimeout(function () { f(n) }, 5 * n)
	});
}

```

Usage and output:

```javascript
[1, 9, 8, 7, 6, 5, 3, 4, 5, 2, 0].timeoutSort(function(n) { document.write(n + '
'); })
```


```txt

0
1
2
3
4
5
6
7
8
9

```



```javascript
Array.prototype.sleepSort = function(callback) {
  const res = [];
  for (let n of this)
    setTimeout(() => {
      res.push(n);
      if (this.length === res.length)
        callback(res);
    }, n + 1);
  return res;
};

[1, 9, 8, 7, 6, 5, 3, 4, 5, 2, 0].sleepSort(console.log);
// [ 1, 0, 2, 3, 4, 5, 5, 6, 7, 8, 9 ]

```



## jq

{{trans|Brainfuck}}

Doesn't actually sleep. Instead, iterates reducing the values by one until each is zero.


```jq
echo '[5, 1, 3, 2, 11, 6, 4]' | jq '
def f:
  if .unsorted == [] then
    .sorted
  else
    { unsorted: [.unsorted[] | .t = .t - 1 | select(.t != 0)]
    , sorted: (.sorted + [.unsorted[] | .t = .t - 1 | select(.t == 0) | .v]) }
    | f
  end;
{unsorted: [.[] | {v: ., t: .}], sorted: []} | f | .[]
'
```

{{out}}

```txt
1
2
3
4
5
6
11
```



## Julia

{{works with|Julia|0.6}}


```julia
function sleepsort(arr::Vector{<:Real})
    out = Vector{eltype(arr)}(0)
    sizehint!(out, length(arr))
    @sync for x in arr
        @async begin
            sleep(x)
            push!(out, x)
        end
    end
    return out
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", sleepsort(v))
```


{{out}}

```txt
# unordered: [9, 5, 3, 8, 8, 2, 5, 2, 5, 5]
 -> ordered: [2, 2, 3, 5, 5, 5, 5, 8, 8, 9]
```



## Kotlin


```scala
// version 1.1.51

import kotlin.concurrent.thread

fun sleepSort(list: List<Int>, interval: Long) {
    print("Sorted  : ")
    for (i in list) {
        thread {
            Thread.sleep(i * interval)
            print("$i ")
        }
    }
    thread { // print a new line after displaying sorted list
        Thread.sleep ((1 + list.max()!!) * interval)
        println()
    }
}

fun main(args: Array<String>) {
   val list = args.map { it.toInt() }.filter { it >= 0 } // ignore negative integers
   println("Unsorted: ${list.joinToString(" ")}")
   sleepSort(list, 50)
}
```


Sample output:

```txt

$ java -jar sleepsort.jar 5 7 -1 2 4 1 8 0 3 9 6
Unsorted: 5 7 2 4 1 8 0 3 9 6
Sorted  : 0 1 2 3 4 5 6 7 8 9

```



## Lua

Here's a slow implementation using only stock C Lua:


```lua
function sleeprint(n)
  local t0 = os.time()
  while os.time() - t0 <= n do
    coroutine.yield(false)
  end
  print(n)
  return true
end

coroutines = {}
for i=1, #arg do
  wrapped = coroutine.wrap(sleeprint)
  table.insert(coroutines, wrapped)
  wrapped(tonumber(arg[i]))
end

done = false
while not done do
  done = true
  for i=#coroutines,1,-1 do
    if coroutines[i]() then
      table.remove(coroutines, i)
    else
      done = false
    end
  end
end
```


By installing LuaSocket, you can get better than 1-second precision on the clock, and therefore faster output:


```lua
socket = require 'socket'

function sleeprint(n)
  local t0 = socket.gettime()
  while (socket.gettime() - t0)*100 <= n do
    coroutine.yield(false)
  end
  print(n)
  return true
end

coroutines = {}
for i=1, #arg do
  wrapped = coroutine.wrap(sleeprint)
  table.insert(coroutines, wrapped)
  wrapped(tonumber(arg[i]))
end

done = false
while not done do
  done = true
  for i=#coroutines,1,-1 do
    if coroutines[i]() then
      table.remove(coroutines, i)
    else
      done = false
    end
  end
end
```


Either way, the output is the same:

{{Out}}

```txt

$ lua sleep_sort.lua 3 1 4 1 5 9 2 6 5 3 5
1
1
2
3
3
4
5
5
5
6
9

```



## Mathematica


```mathematica
SleepSort = RunScheduledTask[Print@#, {#, 1}] & /@ # &;
SleepSort@{1, 9, 8, 7, 6, 5, 3, 4, 5, 2, 0};
```

{{Out}}

```txt

0
1
2
3
4
5
6
7
8
9

```



## NetRexx

As implemented this sample goes beyond the scope of the task as defined; it will handle negative numbers.


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary
import java.util.concurrent.CountDownLatch

--
### =======================================================================

class RSortingSleepsort
  properties constant private
    dflt = '-6 3 1 4 5 2 3 -7 1 6 001 3 -9 2 5 -009 -8 4 6 1 9 8 7 6 5 -7 3 4 5 2 0 -2 -1 -5 -4 -3 -0 000 0'
  properties indirect
    startLatch = CountDownLatch
    doneLatch  = CountDownLatch
    floor      = 0
    sorted     = ''
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    arg = Rexx(args)
    if arg = '' then arg = dflt
    say ' unsorted:' arg
    say '   sorted:' (RSortingSleepsort()).sleepSort(arg)
    return
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method sleepSort(iArg) public
    setStartLatch(CountDownLatch(1))           -- used to put all threads on hold until we're ready to run
    setDoneLatch(CountDownLatch(iArg.words())) -- used to indicate all work is done
    loop mn = 1 to iArg.words()
      setFloor(getFloor().min(iArg.word(mn)))   -- save smallest -ve number so we can use it as a scale for sleep
      Thread(SortThread(iArg.word(mn))).start() -- loop through input and create a thread for each element
      end mn
    getStartLatch().countDown() -- cry 'Havoc', and let slip the dogs of war.
    do
      getDoneLatch().await() -- wait for worker threads to complete
    catch ix = InterruptedException
      ix.printStackTrace()
    end
    return getSorted()

--
### =======================================================================

class RSortingSleepsort.SortThread dependent implements Runnable
  properties indirect
    num
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method SortThread(nm)
    setNum(nm)
    return
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method run() public
    do
      parent.getStartLatch().await()                 -- wait until all threads are constructed
      sleepTime = getNum() + parent.getFloor().abs() -- shifted by value of smallest number (permits numbers < 0)
      sleepTime = sleepTime * 250                    -- scale up; milliseconds are not granular enough
      Thread.sleep(sleepTime)                        -- wait for this number's turn to run
    catch ie = InterruptedException
      ie.printStackTrace()
    end
    do protect parent -- lock the parent to prevent collisions
      parent.setSorted((parent.getSorted() num).strip()) -- stow the number in the results List
    end
    parent.getDoneLatch().countDown() -- this one's done; decrement the latch
    return

```

'''Output:'''

```txt

 unsorted: -6 3 1 4 5 2 3 -7 1 6 001 3 -9 2 5 -009 -8 4 6 1 9 8 7 6 5 -7 3 4 5 2 0 -2 -1 -5 -4 -3 -0 000 0
   sorted: -9 -009 -8 -7 -7 -6 -5 -4 -3 -2 -1 000 0 0 -0 1 1 001 1 2 2 2 3 3 3 3 4 4 4 5 5 5 5 6 6 6 7 8 9

```



## Nim

Compile with <code>nim --threads:on c sleepsort</code>:

```nim
import os, strutils

proc single(n: int) =
  sleep n
  echo n

proc main =
  var thr = newSeq[TThread[int]](paramCount())
  for i,c in commandLineParams():
    thr[i].createThread(single, c.parseInt)
  thr.joinThreads

main()
```

Usage:

```txt
$ ./sleepsort 5 1 3 2 11 6 4
1
2
3
4
5
6
11
```



## Objeck


```objeck

use System.Concurrency;
use Collection;

bundle Default {
  class Item from Thread {
    @value : Int;

    New(value : Int) {
      Parent();
      @value := value;
    }

    method : public : Run(param : System.Base) ~ Nil {
      Sleep(1000 * @value);
      @value->PrintLine();
    }
  }

  class SleepSort {
    function : Main(args : String[]) ~ Nil {
      items := Vector->New();
      each(i : args) {
        items->AddBack(Item->New(args[i]->ToInt()));
      };

      each(i : items) {
        items->Get(i)->As(Item)->Execute(Nil);
      };
    }
  }
}

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


int main(int argc, char **argv)
{
    NSOperationQueue *queue = [[NSOperationQueue alloc] init];
    while (--argc) {
        int i = atoi(argv[argc]);
        [queue addOperationWithBlock: ^{
            sleep(i);
            NSLog(@"%d\n", i);
        }];
    }
    [queue waitUntilAllOperationsAreFinished];
}
```

Rather than having multiple operations that sleep, we could also dispatch the tasks after a delay:

```objc>#import <Foundation/Foundation.h


int main(int argc, char **argv)
{
    while (--argc) {
        int i = atoi(argv[argc]);
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, i * NSEC_PER_SEC),
                       dispatch_get_main_queue(),
                       ^{ NSLog(@"%d\n", i); });
    }
}
```



## Oforth


Instead of printing numbers, each task sends its integer into a channel (after sleeping 20 * n milliseconds). This allows the main task to create a new sorted list with those integers.

20 milliseconds is used to (try to) handle scheduler tick on Windows systems (around 15 ms). On Linux systems (after kernel 2.6.8), this value can be smaller.


```Oforth
import: parallel

: sleepSort(l)
| ch n |
   Channel new ->ch
   l forEach: n [ #[ n dup 20 * sleep ch send drop ] & ]
   ListBuffer newSize(l size) #[ ch receive over add ] times(l size) ;
```


{{out}}

```txt

100 seq 100 seq + sleepSort println
[1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14,
 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25, 2
5, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 33, 33, 34, 34, 35, 35, 36, 36,
 37, 37, 38, 38, 39, 39, 40, 40, 41, 41, 42, 42, 43, 43, 44, 44, 45, 45, 46, 46, 47, 47, 4
8, 48, 49, 49, 50, 50, 51, 51, 52, 52, 53, 53, 54, 54, 55, 55, 56, 56, 57, 57, 58, 58, 59,
 59, 60, 60, 61, 61, 62, 62, 63, 63, 64, 64, 65, 65, 66, 66, 67, 67, 68, 68, 69, 69, 70, 7
0, 71, 71, 72, 72, 73, 73, 74, 74, 75, 75, 76, 76, 77, 77, 78, 78, 79, 79, 80, 80, 81, 81,
 82, 82, 83, 83, 84, 84, 85, 85, 86, 86, 87, 87, 88, 88, 89, 89, 90, 90, 91, 91, 92, 92, 9
3, 93, 94, 94, 95, 95, 96, 96, 97, 97, 98, 98, 99, 99, 100, 100]

```



## Perl

Basically the C code.

```Perl
1 while ($_ = shift and @ARGV and !fork);
sleep $_;
print "$_\n";
wait;
```



A more optimal solution makes use of Coro, a cooperative threading library. It has the added effect of being much faster, fully deterministic (sleep is not exact), and it allows you to easily collect the return value:

```Perl
use Coro;
$ret = Coro::Channel->new;
@nums = qw(1 32 2 59 2 39 43 15 8 9 12 9 11);

for my $n (@nums){
	async {
		Coro::cede for 1..$n;
		$ret->put($n);
	}
}

print $ret->get,"\n" for 1..@nums;
```



## Perl 6



```perl6
await map -> $delay { start { sleep $delay ; say $delay } },
    <6 8 1 12 2 14 5 2 1 0>;
```


{{out}}

```txt
0
1
1
2
2
5
6
8
12
14
```



## Phix

Copy of [[Sorting_algorithms/Sleep_sort#Euphoria|Euphoria]]

```Phix
integer count

procedure sleeper(integer key)
    ? key
    count -= 1
end procedure

sequence s, val
atom task

s = command_line()
s = s[3..$]
if length(s)=0 then
    puts(1,"Nothing to sort.\n")
else
    count = 0
    for i = 1 to length(s) do
        val = value(s[i])
        if val[1] = GET_SUCCESS then
            task = task_create(routine_id("sleeper"),{val[2]})
            task_schedule(task,{val[2],val[2]}/10)
            count += 1
        end if
    end for

    while count do
        task_yield()
    end while
end if
```



## PicoLisp


### Sleeping in main process


```PicoLisp
(de sleepSort (Lst)
   (make
      (for (I . N) Lst
         (task (- I) (* N 100)  N N  I I
            (link N)
            (pop 'Lst)
            (task (- I)) ) )
      (wait NIL (not Lst)) ) )
```


### Sleeping in child processes


```PicoLisp
(de sleepSort (Lst)
   (make
      (for N Lst
         (task (pipe (wait (* N 100))) N N
            (link N)
            (pop 'Lst)
            (task (close @)) ) )
      (wait NIL (not Lst)) ) )
```

Output in both cases:

```txt
: (sleepSort (3 1 4 1 5 9 2 6 5))
-> (1 1 2 3 4 5 5 6 9)
```

===Just printing (no sorted result list)===
Basically the C code.

```PicoLisp
(for N (3 1 4 1 5 9 2 6 5)
   (unless (fork)
      (call 'sleep N)
      (msg N)
      (bye) ) )
```

Output:

```txt
1
1
2
3
4
5
5
6
9
```



## Pike



```Pike

#!/usr/bin/env pike

int main(int argc, array(string) argv)
{
        foreach(argv[1..], string value)
        {
                int v = (int)value;
                if(v<0)
                        continue;
                call_out(print, v, value);
        }
        return -1;
}

void print(string value)
{
        write("%s\n", value);
        if(find_call_out(print)==-1)
                exit(0);
        return;
}

```

Output :

```txt

$ ./sleep-sort.pike 4 5 -3 1 2 7 6
1
2
4
5
6
7

```



## Prolog

Works with SWI-Prolog.

```Prolog
sleep_sort(L) :-
	thread_pool_create(rosetta, 1024, []) ,
	maplist(initsort, L, LID),
	maplist(thread_join, LID, _LStatus),
	thread_pool_destroy(rosetta).

initsort(V, Id) :-
	thread_create_in_pool(rosetta, (sleep(V), writeln(V)), Id, []).


```

Output :

```txt
 sleep_sort([5, 1, 3, 2, 11, 6, 3, 4]).
1
2
3
3
4
5
6
11
true.


```



## PureBasic


```PureBasic
NewMap threads()

Procedure Foo(n)
  Delay(n)
  PrintN(Str(n))
EndProcedure

If OpenConsole()
  For i=1 To CountProgramParameters()
    threads(Str(i)) = CreateThread(@Foo(), Val(ProgramParameter()))
  Next

  ForEach threads()
    WaitThread(threads())
  Next
  Print("Press ENTER to exit"): Input()
EndIf
```


```txt
Sleep_sort.exe 3 1 4 1 5 9
1
1
3
4
5
9
Press ENTER to exit
```



## Python


### Python: Using threading.Timer



```python
from time import sleep
from threading import Timer

def sleepsort(values):
    sleepsort.result = []
    def add1(x):
        sleepsort.result.append(x)
    mx = values[0]
    for v in values:
        if mx < v: mx = v
        Timer(v, add1, [v]).start()
    sleep(mx+1)
    return sleepsort.result

if __name__ == '__main__':
    x = [3,2,4,7,3,6,9,1]
    if sleepsort(x) == sorted(x):
        print('sleep sort worked for:',x)
    else:
        print('sleep sort FAILED for:',x)
```


;Sample output:

```txt
sleep sort worked for: [3, 2, 4, 7, 3, 6, 9, 1]
```



### Python v3.5+: Using asyncio


Since the introduction of async/await syntax, the implementation
could be a sole translation from the original version in Bash:
{{Works with|Python 3.5+}}

```python
#!/usr/bin/env python3
from asyncio import run, sleep, wait
from sys import argv

async def f(n):
    await sleep(n)
    print(n)

if __name__ == '__main__':
    run(wait(list(map(f, map(int, argv[1:])))))
```

Example usage:

```txt

$ ./sleepsort.py 5 3 6 3 6 3 1 4 7
1
3
3
3
4
5
6
6
7

```



## Racket



```racket

#lang racket

;; accepts a list to sort
(define (sleep-sort lst)
  (define done (make-channel))
  (for ([elem lst])
    (thread
     (λ ()
       (sleep elem)
       (channel-put done elem))))
  (for/list ([_ (length lst)])
    (channel-get done)))

;; outputs '(2 5 5 7 8 9 10)
(sleep-sort '(5 8 2 7 9 10 5))

```



## REXX

This sort will accept any manner of numbers, or for that matter, any character string as well.

REXX isn't particular what is being sorted.

This REXX version '''only''' works with Regina REXX   (as the program uses the   '''fork'''
function.

```rexx
/*REXX program implements a sleep sort (with numbers entered from C.L.).*/
numeric digits 300                     /*over the top, but what the hey!*/
                                       /*  (above)  ··· from vaudeville.*/
#.=                                    /*placeholder for the array of #s*/
stuff= 1e9 50 5 40 4 1 100 30 3 12 2 8 9 7 6 6 10 20 0  /*alphabetically*/
parse arg numbers                      /*let the user specify on the CL.*/
if numbers=''  then numbers=stuff      /*Not specified? Then use default*/
N=words(numbers)                       /*N  is the  number  of numbers. */
w=length(N)                            /*width of  N  (for nice output).*/
say N 'numbers to be sorted:' numbers  /*informative informational info.*/

    do j=1  for N                      /*let's start to boogie-woogie.  */
    #.j=word(numbers,j)                /*plug in one number at a time.  */
    if datatype(#.j,'N')  then #.j=#.j/1     /*normalize it if a number.*/
    call fork                          /*only REGINA REXX supports FORK.*/
    call sortItem j                    /*start a sort for array number. */
    end   /*j*/

      do forever  while \inOrder(N)    /*wait for the sorts to complete.*/
      call sleep 1                     /*1 sec is minimum effective time*/
      end    /*forever while*/         /*well, other than zero seconds. */

m=max(length(#.1),length(#.N))         /*width of smallest | largest num*/
say;  say  'after sort:'               /*display blank line and a title.*/

      do k=1  for N                    /*list (sorted) array's elements.*/
      say left('',20)  'array element'  right(k,w)   '───►'   right(#.k,m)
      end   /*k*/
exit                                   /*stick a fork in it, we're done.*/
/*───────────────────────────────────SortItem subroutine────────────────*/
sortItem: procedure expose #.;   parse arg ?        /*sorts single item.*/
              do Asort=1  until \switched           /*cook until cooked.*/
              switched=0                            /*hunky-dorey so far*/
                            do i=1   while   #.i\==''  &  \switched
                            if #.? >= #.i then iterate     /*this one ok*/
                            parse value   #.?  #.i     with     #.i  #.?
                            switched=1              /* [↑]  swapped one.*/
                            end   /*i*/
              if Asort//?==0  then call sleep switched   /*sleep if last*/
              end   /*Asort*/
return    /*Sleeping Beauty awakes.    Not to worry:   (c) = circa 1697.*/
/*───────────────────────────────────InOrder subroutine─────────────────*/
inOrder: procedure expose #.;  parse arg howMany   /*is array in order? */
          do m=1  for howMany-1;   next=m+1;  if #.m>#.next  then return 0
          end   /*m*/                 /*keep looking for fountain of yut*/
return 1                              /*yes, indicate with an indicator.*/

```

'''output''' when using the default input

```txt

19 numbers to be sorted: 1e9 50 5 40 4 1 100 30 3 12 2 8 9 7 6 6 10 20 0

after sort:
                     array element  1 ───>          0
                     array element  2 ───>          1
                     array element  3 ───>          2
                     array element  4 ───>          3
                     array element  5 ───>          4
                     array element  6 ───>          5
                     array element  7 ───>          6
                     array element  8 ───>          6
                     array element  9 ───>          7
                     array element 10 ───>          8
                     array element 11 ───>          9
                     array element 12 ───>         10
                     array element 13 ───>         12
                     array element 14 ───>         20
                     array element 15 ───>         30
                     array element 16 ───>         40
                     array element 17 ───>         50
                     array element 18 ───>        100
                     array element 19 ───> 1000000000

```



## Ruby


```ruby
require 'thread'

nums = ARGV.collect(&:to_i)
sorted = []
mutex = Mutex.new

threads = nums.collect do |n|
  Thread.new do
    sleep 0.01 * n
    mutex.synchronize {sorted << n}
  end
end
threads.each {|t| t.join}

p sorted
```


Example

```txt
$ ruby sleepsort.rb 3 1 4 5 2 3 1 6 1 3 2 5 4 6
[1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6]
```



## Rust


```rust
use std::thread;

fn sleepsort<I: Iterator<Item=u32>>(nums: I) {
    let threads: Vec<_> = nums.map(|n|
        thread::spawn(move || {
            thread::sleep_ms(n);
            println!("{}", n); })).collect();
    for t in threads { t.join(); }
}

fn main() {
    sleepsort(std::env::args().skip(1).map(|s| s.parse().unwrap()));
}
```

Output:

```txt
$ ./sleepsort 50 34 43 3 2
2
3
34
43
50

```



## Scala


```scala
object SleepSort {

  def main(args: Array[String]): Unit = {
    val nums = args.map(_.toInt)
    sort(nums)
    Thread.sleep(nums.max * 21) // Keep the JVM alive for the example
  }

  def sort(nums: Seq[Int]): Unit =
    nums.foreach(i => new Thread {
      override def run() {
        Thread.sleep(i * 20) // just `i` is unpredictable with small numbers
        print(s"$i ")
      }
    }.start())

}
```

{{out}}

```bash
$ scala SleepSort 1 3 6 0 9 7 4 2 5 8
0 1 2 3 4 5 5 6 7 8 9
```



## Swift


```Swift
import Foundation

for i in [5, 2, 4, 6, 1, 7, 20, 14] {
    let time = dispatch_time(DISPATCH_TIME_NOW,
        Int64(i * Int(NSEC_PER_SEC)))

    dispatch_after(time, dispatch_get_main_queue()) {
        print(i)
    }
}

CFRunLoopRun()
```

{{out}}

```txt

1
2
4
5
6
7
14
20

```



## Sidef


```ruby
ARGV.map{.to_i}.map{ |i|
    {Sys.sleep(i); say i}.fork;
}.each{.wait};
```

{{out}}

```txt
% sidef test.sf 5 1 3 2 11 6 4
1
2
3
4
5
6
11
```


## Simula


```simula
SIMULATION
BEGIN

    PROCESS CLASS SORTITEM(N); INTEGER N;
    BEGIN
        HOLD(N);
        OUTINT(N, 3);
    END;

    INTEGER I;
    FOR I := 3, 2, 4, 7, 3, 6, 9, 1 DO
    BEGIN
        REF(SORTITEM) SI;
        SI :- NEW SORTITEM(I);
        ACTIVATE SI;
    END;
    HOLD(100000);
    OUTIMAGE;

END;
```

{{out}}

```txt
  1  2  3  3  4  6  7  9

```



## SNUSP

Bloated SNUSP is ideally suited to this task, since this the variant adds multithreading and an additional dimension of data space.  Sleep time is simulated by the loop delay required to copy each cell value, thereby ensuring that smaller values are printed earlier than larger values.  This program requires a Bloated SNUSP interpreter which returns zero on input end-of-file.

```SNUSP

      /$>\  input until eof
  #/?<\?,/  foreach: fork
   \ &/:+   copy and\
    /:\?-;    delay /
    \.#     print and exit thread

```


Legend:
* '''&''' - SPLIT creates a new thread.  Like '''@''' ENTER, it skips one cell of code space to start its continuation.
* ''': ;''' - UP and DOWN are equivalent to '''< >''' LEFT and RIGHT, but moves the data pointer in the second dimension.
* '''#''' - in Bloated SNUSP, LEAVE only terminates the current thread. The overall program only exits when all threads have quit.


## Tcl


### Tcl 8.5


```tcl
#!/bin/env tclsh
set count 0
proc process val {
    puts $val
    incr ::count
}
# Schedule the output of the values
foreach val $argv {
    after [expr {$val * 10}] [list process $val]
}
# Run event loop until all values output...
while {$count < $argc} {
    vwait count
}
```

'''Demo:'''

```txt

bash$ sleepsort.tcl 3 1 4 5 2 3 1 6 1 3 2 5 4 6
1
1
1
2
2
3
3
3
4
4
5
5
6
6

```


### Tcl 8.6: coroutine


```tcl
#! /usr/bin/env tclsh

package require Tcl 8.6

# By aspect (https://wiki.tcl-lang.org/page/aspect).  Modified slightly.
# 1. Schedule N delayed calls to our own coroutine.
# 2. Yield N times to grab the scheduled values.  Print each.
# 3. Store the sorted list in $varName.
proc sleep-sort {ls varName} {
    foreach x $ls {
        after $x [info coroutine] $x
    }

    set $varName [lmap x $ls {
        set newX [yield]
        puts $newX
        lindex $newX
    }]
}

# Ensure the list is suitable for use with [sleep-sort].
proc validate ls {
    if {[llength $ls] == 0} {
        error {list is empty}
    }

    foreach x $ls {
        if {![string is integer -strict $x] || $x < 0} {
            error [list invalid value: $x]
        }
    }

    return $ls
}

coroutine c sleep-sort [validate $argv] ::sorted
vwait sorted
```

'''Demo:'''

```txt

$ ./sleepsort.tcl 1 2 100 40 76 0 0 0 200 199
0
0
0
1
2
40
76
100
199
200

```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
f() {
    sleep "$1"
    echo "$1"
}
while [ -n "$1" ]
do
    f "$1" &
    shift
done
wait
```

Usage and output:

```txt

sh sleepsort.sh 3 1 4 1 5 9
1
1
3
4
5
9

```



## zkl


```zkl
vm.arglist.apply(fcn(n){ Atomic.sleep(n); print(n) }.launch);
Atomic.waitFor(fcn{ vm.numThreads == 1 }); Atomic.sleep(2); println();
```

{{out}}

```txt

$ zkl bbb 7 6 9 2 4 8 1 3 5
123456789
$

```


{{omit from|GUISS}}
{{omit from|Axe}}
