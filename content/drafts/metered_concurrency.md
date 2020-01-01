+++
title = "Metered concurrency"
description = ""
date = 2018-10-15T17:47:40Z
aliases = []
[extra]
id = 1931
[taxonomies]
categories = []
tags = []
+++

{{task|Concurrency}}{{requires|Concurrency}}
The goal of this task is to create a [[wp:Counting semaphore|counting semaphore]] used to control the execution of a set of concurrent units. This task intends to demonstrate coordination of active concurrent units through the use of a passive concurrent unit. The operations for a counting semaphore are ''acquire'', ''release'', and ''count''. Each active concurrent unit should attempt to acquire the counting semaphore before executing its assigned duties. In this case the active concurrent unit should report that it has acquired the semaphore. It should sleep for 2 seconds and then release the semaphore.


## Ada

{{works with|GNAT|GPL 2006}}

The interface for the counting semaphore is defined in an Ada package specification:

```ada
package Semaphores is
   protected type Counting_Semaphore(Max : Positive) is
      entry Acquire;
      procedure Release;
      function Count return Natural;
   private
      Lock_Count : Natural := 0;
   end Counting_Semaphore;
end Semaphores;
```

The ''Acquire'' entry has a condition associated with it. A task can only execute the ''Acquire'' entry when ''Lock_Count'' is less than ''Max''. This is the key to making this structure behave as a counting semaphore. This condition, and all the other aspects of ''Counting_Semaphore'' are contained in the package body.

```ada
package body Semaphores is

   ------------------------
   -- Counting_Semaphore --
   ------------------------

   protected body Counting_Semaphore is

      -------------
      -- Acquire --
      -------------

      entry Acquire when Lock_Count < Max is
      begin
         Lock_Count := Lock_Count + 1;
      end Acquire;

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Lock_Count;
      end Count;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         if Lock_Count > 0 then
            Lock_Count := Lock_Count - 1;
         end if;
      end Release;

   end Counting_Semaphore;

end Semaphores;
```

We now need a set of tasks to properly call an instance of ''Counting_Semaphore''.

```ada
with Semaphores;
with Ada.Text_Io; use Ada.Text_Io;

procedure Semaphores_Main is
   -- Create an instance of a Counting_Semaphore with Max set to 3
   Lock : Semaphores.Counting_Semaphore(3);

   -- Define a task type to interact with the Lock object declared above
   task type Worker is
      entry Start (Sleep : in Duration; Id : in Positive);
   end Worker;

   task body Worker is
      Sleep_Time : Duration;
      My_Id : Positive;
   begin
      accept Start(Sleep : in Duration; Id : in Positive) do
         My_Id := Id;
         Sleep_Time := Sleep;
      end Start;
      --Acquire the lock. The task will suspend until the Acquire call completes
      Lock.Acquire;
      Put_Line("Task #" & Positive'Image(My_Id) & " acquired the lock.");
      -- Suspend the task for Sleep_Time seconds
      delay Sleep_Time;
      -- Release the lock. Release is unconditional and happens without suspension
      Lock.Release;
   end Worker;

   -- Create an array of 5 Workers
   type Staff is array(Positive range 1..5) of Worker;
   Crew : Staff;
begin
   for I in Crew'range loop
      Crew(I).Start(2.0, I);
   end loop;
end Semaphores_Main;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to PAR and SEMA being unimplemented}}

```algol68
SEMA sem = LEVEL 1;

PROC job = (INT n)VOID: (
   printf(($" Job "d" acquired Semaphore ..."$,n));
   TO 10000000 DO SKIP OD;
   printf(($" Job "d" releasing Semaphore"l$,n))
);

PAR (
  ( DOWN sem ; job(1) ; UP sem ) ,
  ( DOWN sem ; job(2) ; UP sem ) ,
  ( DOWN sem ; job(3) ; UP sem )
)
```

Output:

```txt

 Job 3 acquired Semaphore ... Job 3 releasing Semaphore
 Job 1 acquired Semaphore ... Job 1 releasing Semaphore
 Job 2 acquired Semaphore ... Job 2 releasing Semaphore

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
In BBC BASIC concurrency can only be achieved by timer events (short of running multiple processes).

```bbcbasic
      INSTALL @lib$+"TIMERLIB"
      DIM tID%(6)

      REM Two workers may be concurrent
      DIM Semaphore%(2)

      tID%(6) = FN_ontimer(11, PROCtimer6, 1)
      tID%(5) = FN_ontimer(10, PROCtimer5, 1)
      tID%(4) = FN_ontimer(11, PROCtimer4, 1)
      tID%(3) = FN_ontimer(10, PROCtimer3, 1)
      tID%(2) = FN_ontimer(11, PROCtimer2, 1)
      tID%(1) = FN_ontimer(10, PROCtimer1, 1)

      ON CLOSE PROCcleanup : QUIT
      ON ERROR PRINT REPORT$ : PROCcleanup : END

      sc% = 0
      REPEAT
        oldsc% = sc%
        sc% = -SUM(Semaphore%())
        IF sc%<>oldsc% PRINT "Semaphore count now ";sc%
        WAIT 0
      UNTIL FALSE

      DEF PROCtimer1 : PROCtask(1) : ENDPROC
      DEF PROCtimer2 : PROCtask(2) : ENDPROC
      DEF PROCtimer3 : PROCtask(3) : ENDPROC
      DEF PROCtimer4 : PROCtask(4) : ENDPROC
      DEF PROCtimer5 : PROCtask(5) : ENDPROC
      DEF PROCtimer6 : PROCtask(6) : ENDPROC

      DEF PROCtask(n%)
      LOCAL i%, temp%
      PRIVATE delay%(), sem%()
      DIM delay%(6), sem%(6)
      IF delay%(n%) THEN
        delay%(n%) -= 1
        IF delay%(n%) = 0 THEN
          SWAP Semaphore%(sem%(n%)),temp%
          delay%(n%) = -1
          PRINT "Task " ; n% " released semaphore"
        ENDIF
        ENDPROC
      ENDIF
      FOR i% = 1 TO DIM(Semaphore%(),1)
        temp% = TRUE
        SWAP Semaphore%(i%),temp%
        IF NOT temp% EXIT FOR
      NEXT
      IF temp% THEN ENDPROC : REM Waiting to acquire semaphore
      sem%(n%) = i%
      delay%(n%) = 200
      PRINT "Task "; n% " acquired semaphore"
      ENDPROC

      DEF PROCcleanup
      LOCAL i%
      FOR i% = 1 TO 6
        PROC_killtimer(tID%(i%))
      NEXT
      ENDPROC
```

'''Output:'''

```txt

Task 1 acquired semaphore
Task 2 acquired semaphore
Semaphore count now 2
Task 1 released semaphore
Task 3 acquired semaphore
Task 2 released semaphore
Task 4 acquired semaphore
Task 3 released semaphore
Task 5 acquired semaphore
Task 4 released semaphore
Task 6 acquired semaphore
Task 5 released semaphore
Semaphore count now 1
Task 6 released semaphore
Semaphore count now 0

```



## C

{{works with|POSIX}}

```c
#include <semaphore.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

sem_t sem;
int count = 3;

/* the whole point of a semaphore is that you don't count it:
 * p/v are atomic.  Unless it's locked while you are doing
 * something with the count, the value is only informative */
#define getcount() count
void acquire()
{
	sem_wait(&sem);
	count--;
}

void release()
{
	count++;
	sem_post(&sem);
}

void* work(void * id)
{
	int i = 10;
	while (i--) {
		acquire();
		printf("#%d acquired sema at %d\n", *(int*)id, getcount());
		usleep(rand() % 4000000); /* sleep 2 sec on average */
		release();
		usleep(0);  /* effectively yield */
	}
	return 0;
}

int main()
{
	pthread_t th[4];
	int i, ids[] = {1, 2, 3, 4};

	sem_init(&sem, 0, count);

	for (i = 4; i--;) pthread_create(th + i, 0, work, ids + i);
	for (i = 4; i--;) pthread_join(th[i], 0);
	printf("all workers done\n");

	return sem_destroy(&sem);
}
```



## C sharp

C# has built in semaphore system where acquire is called via Wait(), release with Release() and count with semaphore.CurrentCount.

```csharp
using System;
using System.Threading;
using System.Threading.Tasks;

namespace RosettaCode
{
  internal sealed class Program
  {
    private static void Worker(object arg, int id)
    {
      var sem = arg as SemaphoreSlim;
      sem.Wait();
      Console.WriteLine("Thread {0} has a semaphore & is now working.", id);
      Thread.Sleep(2*1000);
      Console.WriteLine("#{0} done.", id);
      sem.Release();
    }

    private static void Main()
    {
      var semaphore = new SemaphoreSlim(Environment.ProcessorCount*2, int.MaxValue);

      Console.WriteLine("You have {0} processors availiabe", Environment.ProcessorCount);
      Console.WriteLine("This program will use {0} semaphores.\n", semaphore.CurrentCount);

      Parallel.For(0, Environment.ProcessorCount*3, y => Worker(semaphore, y));
    }
  }
}
```



## D


```d
module meteredconcurrency ;
import std.stdio ;
import std.thread ;
import std.c.time ;

class Semaphore {
  private int lockCnt, maxCnt ;
  this(int count) { maxCnt = lockCnt = count ;}
  void acquire() {
    if(lockCnt < 0 || maxCnt <= 0)
      throw new Exception("Negative Lock or Zero init. Lock") ;
    while(lockCnt == 0)
      Thread.getThis.yield ; // let other threads release lock
    synchronized lockCnt-- ;
  }
  void release() {
    synchronized
      if (lockCnt < maxCnt)
        lockCnt++ ;
      else
        throw new Exception("Release lock before acquire") ;
  }
  int getCnt() { synchronized return lockCnt ; }
}

class Worker : Thread {
  private static int Id = 0 ;
  private Semaphore lock ;
  private int myId ;
  this (Semaphore l) { super() ; lock = l ; myId = Id++ ; }
  override int run() {
    lock.acquire ;
    writefln("Worker %d got a lock(%d left).", myId, lock.getCnt) ;
    msleep(2000) ;  // wait 2.0 sec
    lock.release ;
    writefln("Worker %d released a lock(%d left).", myId, lock.getCnt) ;
    return 0 ;
  }
}

void main() {
  Worker[10] crew ;
  Semaphore lock = new Semaphore(4) ;

  foreach(inout c ; crew)
    (c = new Worker(lock)).start ;
  foreach(inout c ; crew)
    c.wait ;
}
```


### Phobos with tools

Using the scrapple.tools extension library for Phobos ..

```d
module metered;

import tools.threads, tools.log, tools.time, tools.threadpool;

void main() {
  log_threads = false;
  auto done = new Semaphore, lock = new Semaphore(4);
  auto tp = new Threadpool(10);
  for (int i = 0; i < 10; ++i) {
    tp.addTask(i /apply/ (int i) {
      scope(exit) done.release;
      lock.acquire;
      logln(i, ": lock acquired");
      sleep(2.0);
      lock.release;
      logln(i, ": lock released");
    });
  }
  for (int i = 0; i < 10; ++i)
    done.acquire;
}
```


## E

This semaphore slightly differs from the task description; the release operation is not on the semaphore itself but given out with each acquisition, and cannot be invoked too many times.


```e
def makeSemaphore(maximum :(int > 0)) {
    var current := 0
    def waiters := <elib:vat.makeQueue>()
    def notify() {
        while (current < maximum && waiters.hasMoreElements()) {
            current += 1
            waiters.optDequeue().resolve(def released)
            when (released) -> {
                current -= 1
                notify()
            }
        }
    }
    def semaphore {
        to acquire() {
            waiters.enqueue(def response)
            notify()
            return response
        }
        to count() { return current }
    }
    return semaphore
}

def work(label, interval, semaphore, timer, println) {
    when (def releaser := semaphore <- acquire()) -> {
        println(`$label: I have acquired the lock.`)
        releaser.resolve(
            timer.whenPast(timer.now() + interval, fn {
                println(`$label: I will have released the lock.`)
            })
        )
    }
}

def semaphore := makeSemaphore(3)
for i in 1..5 {
    work(i, 2000, semaphore, timer, println)
}
```



## EchoLisp


```scheme

(require 'tasks) ;; tasks library

(define (task id)
	(wait S) ;; acquire, p-op
	(printf "task %d acquires semaphore @ %a" id (date->time-string (current-date)))
	(sleep 2000)
	(signal S) ;; release, v-op
	id)

(define S (make-semaphore 4)) ;; semaphore with init count 4

;; run 10 // tasks
(for ([i 10]) (task-run (make-task task i ) (random 500)))

```

{{out}}

```txt

task 1 acquires semaphore @ 19:23:03
task 6 acquires semaphore @ 19:23:03
task 4 acquires semaphore @ 19:23:03
task 7 acquires semaphore @ 19:23:03
task 8 acquires semaphore @ 19:23:05
task 9 acquires semaphore @ 19:23:05
task 0 acquires semaphore @ 19:23:05
task 3 acquires semaphore @ 19:23:05
task 2 acquires semaphore @ 19:23:07
task 1 acquires semaphore @ 19:23:07
task 6 acquires semaphore @ 19:23:07
task 5 acquires semaphore @ 19:23:08
task 7 acquires semaphore @ 19:23:09
task 4 acquires semaphore @ 19:23:09
task 9 acquires semaphore @ 19:23:10
task 8 acquires semaphore @ 19:23:10
task 0 acquires semaphore @ 19:23:11
;; etc.

```



## Erlang

In this implementation the semaphore is handled as its own process. Taking advantage of erlang's receive queues, which act as a FIFO queue for 'acquire' requests. As workers come online and request the semaphore they will receive it in order. 'receive' has the effect of pausing the process until a message is matched, so there's no idle looping.

```erlang

-module(metered).
-compile(export_all).

create_semaphore(N) ->
    spawn(?MODULE, sem_loop, [N,N]).

sem_loop(0,Max) ->
    io:format("Resources exhausted~n"),
    receive
        {release, PID} ->
            PID ! released,
            sem_loop(1,Max);
        {stop, _PID} ->
            ok
    end;
sem_loop(N,N) ->
    receive
        {acquire, PID} ->
            PID ! acquired,
            sem_loop(N-1,N);
        {stop, _PID} ->
            ok
    end;
sem_loop(N,Max) ->
    receive
        {release, PID} ->
            PID ! released,
            sem_loop(N+1,Max);
        {acquire, PID} ->
            PID ! acquired,
            sem_loop(N-1,Max);
        {stop, _PID} ->
            ok
    end.

release(Sem) ->
    Sem ! {release, self()},
    receive
        released ->
            ok
    end.
acquire(Sem) ->
    Sem ! {acquire, self()},
    receive
        acquired ->
            ok
    end.

start() -> create_semaphore(10).

stop(Sem) -> Sem ! {stop, self()}.

worker(P,N,Sem) ->
    acquire(Sem),
    io:format("Worker ~b has the acquired semaphore~n",[N]),
    timer:sleep(500 * random:uniform(4)),
    release(Sem),
    io:format("Worker ~b has released the semaphore~n",[N]),
    P ! {done, self()}.

test() ->
    Sem = start(),
    Pids = lists:map(fun (N) ->
                             spawn(?MODULE, worker, [self(),N,Sem])
                     end, lists:seq(1,20)),
    lists:foreach(fun (P) -> receive {done, P} -> ok end end, Pids),
    stop(Sem).

```



## Euphoria


```euphoria
sequence sems
sems = {}
constant COUNTER = 1, QUEUE = 2

function semaphore(integer n)
    if n > 0 then
        sems = append(sems,{n,{}})
        return length(sems)
    else
        return 0
    end if
end function

procedure acquire(integer id)
    if sems[id][COUNTER] = 0 then
        task_suspend(task_self())
        sems[id][QUEUE] &= task_self()
        task_yield()
    end if
    sems[id][COUNTER] -= 1
end procedure

procedure release(integer id)
    sems[id][COUNTER] += 1
    if length(sems[id][QUEUE])>0 then
        task_schedule(sems[id][QUEUE][1],1)
        sems[id][QUEUE] = sems[id][QUEUE][2..$]
    end if
end procedure

function count(integer id)
    return sems[id][COUNTER]
end function

procedure delay(atom delaytime)
    atom t
    t = time()
    while time() - t < delaytime do
        task_yield()
    end while
end procedure

integer sem

procedure worker()
    acquire(sem)
        printf(1,"- Task %d acquired semaphore.\n",task_self())
        delay(2)
    release(sem)
    printf(1,"+ Task %d released semaphore.\n",task_self())
end procedure

integer task

sem = semaphore(4)

for i = 1 to 10 do
    task = task_create(routine_id("worker"),{})
    task_schedule(task,1)
    task_yield()
end for

while length(task_list())>1 do
    task_yield()
end while
```


Output:

```txt
- Task 1 acquired semaphore.
- Task 2 acquired semaphore.
- Task 3 acquired semaphore.
- Task 4 acquired semaphore.
+ Task 1 released semaphore.
- Task 5 acquired semaphore.
+ Task 4 released semaphore.
- Task 6 acquired semaphore.
+ Task 3 released semaphore.
- Task 7 acquired semaphore.
+ Task 2 released semaphore.
- Task 8 acquired semaphore.
+ Task 7 released semaphore.
- Task 9 acquired semaphore.
+ Task 6 released semaphore.
- Task 10 acquired semaphore.
+ Task 5 released semaphore.
+ Task 8 released semaphore.
+ Task 10 released semaphore.
+ Task 9 released semaphore.
```



## Go


### Buffered channel

Recommended solution for simplicity.  Acquire operation is channel send, release is channel receive, and count is provided with cap and len.

To demonstrate, this example implements the [https://en.wikipedia.org/wiki/Semaphore_(programming)#Library_analogy Library analogy] from Wikipedia with 10 study rooms and 20 students.

The channel type shown here is <code>struct{}</code>.  <code>struct{}</code> is nice because it has zero size and zero content, although the syntax is slightly akward.  Other popular choices for no-content tokens are ints and bools.  They read a little nicer but waste a few bytes and could potentially mislead someone to think the values had some meaning.

A couple of other concurrency related details used in the example are the log package for serializing output and sync.WaitGroup used as a completion checkpoint.  Functions of the fmt package are not synchronized and can produce interleaved output with concurrent writers.  The log package does nice synchronization to avoid this.

```go
package main

import (
    "log"
    "os"
    "sync"
    "time"
)

// counting semaphore implemented with a buffered channel
type sem chan struct{}

func (s sem) acquire()   { s <- struct{}{} }
func (s sem) release()   { <-s }
func (s sem) count() int { return cap(s) - len(s) }

// log package serializes output
var fmt = log.New(os.Stdout, "", 0)

// library analogy per WP article
const nRooms = 10
const nStudents = 20

func main() {
    rooms := make(sem, nRooms)
    // WaitGroup used to wait for all students to have studied
    // before terminating program
    var studied sync.WaitGroup
    studied.Add(nStudents)
    // nStudents run concurrently
    for i := 0; i < nStudents; i++ {
        go student(rooms, &studied)
    }
    studied.Wait()
}

func student(rooms sem, studied *sync.WaitGroup) {
    rooms.acquire()
    // report per task descrption.  also exercise count operation
    fmt.Printf("Room entered.  Count is %d.  Studying...\n",
        rooms.count())
    time.Sleep(2 * time.Second) // sleep per task description
    rooms.release()
    studied.Done() // signal that student is done
}
```

Output for this and the other Go programs here shows 10 students studying immediately, about a 2 second pause, 10 more students studying, then another pause of about 2 seconds before returning to the command prompt.  In this example the count values may look jumbled.  This is a result of the student goroutines running concurrently.


### Sync.Cond

A more traditional approach implementing a counting semaphore object with sync.Cond.  It has a constructor and methods for the three operations requested by the task.

```go
package main

import (
    "log"
    "os"
    "sync"
    "time"
)

var fmt = log.New(os.Stdout, "", 0)

type countSem struct {
    int
    sync.Cond
}

func newCount(n int) *countSem {
    return &countSem{n, sync.Cond{L: &sync.Mutex{}}}
}

func (cs *countSem) count() int {
    cs.L.Lock()
    c := cs.int
    cs.L.Unlock()
    return c
}

func (cs *countSem) acquire() {
    cs.L.Lock()
    cs.int--
    for cs.int < 0 {
        cs.Wait()
    }
    cs.L.Unlock()
}

func (cs *countSem) release() {
    cs.L.Lock()
    cs.int++
    cs.L.Unlock()
    cs.Broadcast()
}

func main() {
    librarian := newCount(10)
    nStudents := 20
    var studied sync.WaitGroup
    studied.Add(nStudents)
    for i := 0; i < nStudents; i++ {
        go student(librarian, &studied)
    }
    studied.Wait()
}

func student(studyRoom *countSem, studied *sync.WaitGroup) {
    studyRoom.acquire()
    fmt.Printf("Room entered.  Count is %d.  Studying...\n", studyRoom.count())
    time.Sleep(2 * time.Second)
    studyRoom.release()
    studied.Done()
}
```



## Groovy

Solution:

```groovy
class CountingSemaphore {
    private int count = 0
    private final int max

    CountingSemaphore(int max) { this.max = max }

    synchronized int acquire() {
        while (count >= max) { wait() }
        ++count
    }

    synchronized int release() {
        if (count) { count--; notifyAll() }
        count
    }

    synchronized int getCount() { count }
}
```


Test:

```groovy
def cs = new CountingSemaphore(4)
(1..12).each { threadID ->
    Thread.start {
        def id = "Thread #${(threadID as String).padLeft(2,'0')}"
        try {
            def sCount = cs.acquire()
            println("${id} has acquired Semaphore at count = ${sCount}")
            sleep(2000)
        } finally {
            println("${id} is releasing Semaphore at count = ${cs.count}")
            cs.release()
        }
    }
}
```


Output:
<pre  style="height:30ex;overflow:scroll;">Thread #03 has acquired Semaphore at count = 4
Thread #07 has acquired Semaphore at count = 2
Thread #02 has acquired Semaphore at count = 1
Thread #09 has acquired Semaphore at count = 3
Thread #03 is releasing Semaphore at count = 4
Thread #02 is releasing Semaphore at count = 4
Thread #09 is releasing Semaphore at count = 4
Thread #07 is releasing Semaphore at count = 4
Thread #12 has acquired Semaphore at count = 4
Thread #05 has acquired Semaphore at count = 3
Thread #06 has acquired Semaphore at count = 4
Thread #08 has acquired Semaphore at count = 2
Thread #12 is releasing Semaphore at count = 4
Thread #06 is releasing Semaphore at count = 4
Thread #05 is releasing Semaphore at count = 4
Thread #10 has acquired Semaphore at count = 4
Thread #11 has acquired Semaphore at count = 4
Thread #08 is releasing Semaphore at count = 3
Thread #01 has acquired Semaphore at count = 4
Thread #04 has acquired Semaphore at count = 4
Thread #11 is releasing Semaphore at count = 4
Thread #10 is releasing Semaphore at count = 4
Thread #04 is releasing Semaphore at count = 2
Thread #01 is releasing Semaphore at count = 2
```



## Haskell

The QSem (quantity semaphore) waitQSem and signalQSem functions are the Haskell acquire and release equivalents, and the MVar (synchronizing variable) functions are used to put the workers statuses on the main thread for printing.  Note that this code is likely only compatible with GHC due to the use of "threadDelay" from Control.Concurrent.


```Haskell
import Control.Concurrent
import Control.Monad

worker :: QSem -> MVar String -> Int -> IO ()
worker q m n = do
    waitQSem q
    putMVar m $ "Worker " ++ show n ++ " has acquired the lock."
    threadDelay 2000000 -- microseconds!
    signalQSem q
    putMVar m $ "Worker " ++ show n ++ " has released the lock."

main :: IO ()
main = do
    q <- newQSem 3
    m <- newEmptyMVar
    let workers = 5
        prints  = 2 * workers
    mapM_ (forkIO . worker q m) [1..workers]
    replicateM_ prints $ takeMVar m >>= print
```


==Icon and {{header|Unicon}}==

Icon doesn't support concurrency.  A Unicon solution is:

```unicon
procedure main(A)
    n := integer(A[1] | 3)    # Max. number of active tasks
    m := integer(A[2] | 2)    # Number of visits by each task
    k := integer(A[3] | 5)    # Number of tasks
    sem := [: |mutex([])\n :]
    every put(threads := [], (i := 1 to k, thread
              every 1 to m do {
                 write("unit ",i," ready")
                 until flag := trylock(!sem)
                 write("unit ",i," running")
                 delay(2000)
                 write("unit ",i," done")
                 unlock(flag)
                 }))

    every wait(!threads)
end
```


Sample run:

```txt

->mc
unit 2 ready
unit 2 running
unit 1 ready
unit 1 running
unit 3 ready
unit 3 running
unit 4 ready
unit 5 ready
unit 2 done
unit 2 ready
unit 5 running
unit 1 done
unit 2 running
unit 1 ready
unit 3 done
unit 3 ready
unit 4 running
unit 5 done
unit 5 ready
unit 1 running
unit 2 done
unit 5 running
unit 4 done
unit 3 running
unit 4 ready
unit 1 done
unit 4 running
unit 5 done
unit 3 done
unit 4 done
->

```




## Java


```java
public class CountingSemaphore{
   private int lockCount = 0;
   private int maxCount;

   CountingSemaphore(int Max){
      maxCount = Max;
   }

   public synchronized void acquire() throws InterruptedException{
      while( lockCount >= maxCount){
         wait();
      }
      lockCount++;
   }
   public synchronized void release(){
      if (lockCount > 0)
      {
         lockCount--;
         notifyAll();
      }
   }
   public synchronized int getCount(){
      return lockCount;
   }
}

public class Worker extends Thread{
   private CountingSemaphore lock;
   private int id;

   Worker(CountingSemaphore coordinator, int num){
      lock = coordinator;
      id = num;
   }
   Worker(){
   }
   public void run(){
      try{
         lock.acquire();
         System.out.println("Worker " + id + " has acquired the lock.");
         sleep(2000);
      }
      catch (InterruptedException e){
      }
      finally{
         lock.release();
      }
   }
   public static void main(String[] args){
      CountingSemaphore lock = new CountingSemaphore(3);
      Worker crew[];
      crew = new Worker[5];
      for (int i = 0; i < 5; i++){
         crew[i] = new Worker(lock, i);
         crew[i].start();
      }

   }
}
```



## Julia


```julia

function acquire(num, sem)
    sleep(rand())
    println("Task $num waiting for semaphore")
    lock(sem)
    println("Task $num has acquired semaphore")
    sleep(rand())
    unlock(sem)
end


function runsem(numtasks)
    println("Sleeping and running $numtasks tasks.")
    sem = Base.Threads.RecursiveSpinLock()
    @sync(
    for i in 1:numtasks
        @async acquire(i, sem)
    end)
    println("Done.")
end

runsem(4)

```
{{output}}
```txt

Sleeping and running 4 tasks.
Task 4 waiting for semaphore
Task 4 has acquired semaphore
Task 1 waiting for semaphore
Task 1 has acquired semaphore
Task 2 waiting for semaphore
Task 2 has acquired semaphore
Task 3 waiting for semaphore
Task 3 has acquired semaphore
Done.

```




## Kotlin


```scala
// version 1.1.51

import java.util.concurrent.Semaphore
import kotlin.concurrent.thread

fun main(args: Array<String>) {
    val numPermits = 4
    val numThreads = 9
    val semaphore = Semaphore(numPermits)
    for (i in 1..numThreads) {
        thread {
            val name = "Unit #$i"
            semaphore.acquire()
            println("$name has acquired the semaphore")
            Thread.sleep(2000)
            semaphore.release()
            println("$name has released the semaphore")
        }
    }
}
```


Sample output:

```txt

Unit #1 has acquired the semaphore
Unit #2 has acquired the semaphore
Unit #3 has acquired the semaphore
Unit #4 has acquired the semaphore
Unit #1 has released the semaphore
Unit #5 has acquired the semaphore
Unit #2 has released the semaphore
Unit #6 has acquired the semaphore
Unit #4 has released the semaphore
Unit #8 has acquired the semaphore
Unit #3 has released the semaphore
Unit #7 has acquired the semaphore
Unit #5 has released the semaphore
Unit #6 has released the semaphore
Unit #9 has acquired the semaphore
Unit #8 has released the semaphore
Unit #7 has released the semaphore
Unit #9 has released the semaphore

```



## Logtalk

Using Logtalk's multi-threading notifications, which use a per-object FIFO message queue, thus avoiding the need of idle-loops. Works when using SWI-Prolog, XSB, or YAP as the backend compiler.

```logtalk

:- object(metered_concurrency).

    :- threaded.

    :- public(run/2).
    run(Workers, Max) :-
        % start the semaphore and the workers
        threaded_ignore(semaphore(Max, Max)),
        forall(
            integer::between(1, Workers, Worker),
            threaded_call(worker(Worker))
        ),
        % wait for the workers to finish
        forall(
            integer::between(1, Workers, Worker),
            threaded_exit(worker(Worker))
        ),
        % tell the semaphore thread to stop
        threaded_notify(worker(stop, _)).

    :- public(run/0).
    run :-
        % default values: 7 workers, 2 concurrent workers
        run(7, 2).

    semaphore(N, Max) :-
        threaded_wait(worker(Action, Worker)),
        (   Action == acquire, N > 0 ->
            M is N - 1,
            threaded_notify(semaphore(acquired, Worker)),
            semaphore(M, Max)
        ;   Action == release ->
            M is N + 1,
            threaded_notify(semaphore(released, Worker)),
            semaphore(M, Max)
        ;   Action == stop ->
            true
        ;   % Action == acquire, N =:= 0,
            threaded_wait(worker(release, OtherWorker)),
            threaded_notify(semaphore(released, OtherWorker)),
            threaded_notify(semaphore(acquired, Worker)),
            semaphore(N, Max)
        ).

    worker(Worker) :-
        % use a random setup time for the worker
        random::random(0.0, 2.0, Setup),
        thread_sleep(Setup),
        threaded_notify(worker(acquire, Worker)),
        threaded_wait(semaphore(acquired, Worker)),
        write('Worker '), write(Worker), write(' acquired semaphore\n'),
        thread_sleep(2),
        threaded_notify(worker(release, Worker)),
        write('Worker '), write(Worker), write(' releasing semaphore\n'),
        threaded_wait(semaphore(released, Worker)).

:- end_object.

```

Output:

```text

| ?- metered_concurrency::run.
Worker 1 acquired semaphore
Worker 6 acquired semaphore
Worker 1 releasing semaphore
Worker 2 acquired semaphore
Worker 6 releasing semaphore
Worker 5 acquired semaphore
Worker 2 releasing semaphore
Worker 7 acquired semaphore
Worker 5 releasing semaphore
Worker 3 acquired semaphore
Worker 7 releasing semaphore
Worker 4 acquired semaphore
Worker 3 releasing semaphore
Worker 4 releasing semaphore
yes

```



## Oforth


A semaphore can be emulated with a channel starting with n objects.
Acquiring the semaphore is receiving an object from the channel
Releasing the semaphore is sending by the object into the channel.

If the channel is empty a task will wait until it is no more empty.


```oforth
import: parallel

Object Class new: Semaphore(ch)

Semaphore method: initialize(n)
   Channel newSize(n) dup := ch
   #[ 1 over send drop ] times(n) drop ;

Semaphore method: acquire  @ch receive drop ;
Semaphore method: release  1 @ch send drop ;
```


Usage :


```oforth
: mytask(s)
   while( true ) [
      s acquire "Semaphore acquired" .cr
      2000 sleep
      s release "Semaphore released" .cr
      ] ;

: test(n)
| s i |
   Semaphore new(n) ->s
   10 loop: i [ #[ s mytask ] & ] ;
```



## Oz

Counting semaphores can be implemented in terms of mutexes (called "locks" in Oz) and dataflow variables (used as condition variables here). The mutex protects both the counter and the mutable reference to the dataflow variable.

```oz
declare
  fun {NewSemaphore N}
     sem(max:N count:{NewCell 0} 'lock':{NewLock} sync:{NewCell _})
  end

  proc {Acquire Sem=sem(max:N count:C 'lock':L sync:S)}
     Sync
     Acquired
  in
     lock L then
        if @C < N then
         C := @C + 1
         Acquired = true
        else
         Sync = @S
         Acquired = false
        end
     end
     if {Not Acquired} then
        {Wait Sync}
        {Acquire Sem}
     end
  end

  proc {Release sem(count:C 'lock':L sync:S ...)}
     lock L then
        C := @C - 1
        @S = unit %% wake up waiting threads
        S := _ %% prepare for new waiters
     end
  end

  proc {WithSemaphore Sem Proc}
     {Acquire Sem}
     try
        {Proc}
     finally
        {Release Sem}
     end
  end

  S = {NewSemaphore 4}

  proc {StartWorker Name}
     thread
	for do
	   {WithSemaphore S
	    proc {$}
	       {System.showInfo Name#" acquired semaphore"}
	       {Delay 2000}
	    end
	   }
	   {Delay 100}
	end
     end
  end
in
  for I in 1..10 do
     {StartWorker I}
  end
```



## Perl

See [http://search.cpan.org/dist/Coro/Coro/Semaphore.pm Coro::Semaphore].

## Perl 6

Uses a buffered channel to hand out a limited number of tickets.

```perl6
class Semaphore {
    has $.tickets = Channel.new;
    method new ($max) {
        my $s = self.bless;
        $s.tickets.send(True) xx $max;
        $s;
    }
    method acquire { $.tickets.receive }
    method release { $.tickets.send(True) }
}

sub MAIN ($units = 5, $max = 2) {
    my $sem = Semaphore.new($max);

    my @units = do for ^$units -> $u {
        start {
            $sem.acquire; say "unit $u acquired";
            sleep 2;
            $sem.release; say "unit $u released";
        }
    }
    await @units;
}
```

{{out}}

```txt
unit 0 acquired
unit 1 acquired
unit 0 released
unit 1 released
unit 3 acquired
unit 2 acquired
unit 3 released
unit 2 released
unit 4 acquired
unit 4 released
```



## Phix

{{trans|Euphoria}}
Requires 0.7.6 or later

```Phix
sequence sems = {}
constant COUNTER = 1, QUEUE = 2

function semaphore(integer n)
    if n>0 then
        sems = append(sems,{n,{}})
        return length(sems)
    else
        return 0
    end if
end function

procedure acquire(integer id)
    if sems[id][COUNTER]=0 then
        task_suspend(task_self())
        sems[id][QUEUE] &= task_self()
        task_yield()
    end if
    sems[id][COUNTER] -= 1
end procedure

procedure release(integer id)
    sems[id][COUNTER] += 1
    if length(sems[id][QUEUE])>0 then
        task_schedule(sems[id][QUEUE][1],1)
        sems[id][QUEUE] = sems[id][QUEUE][2..$]
    end if
end procedure

function count(integer id)
    return sems[id][COUNTER]
end function

procedure delay(atom delaytime)
atom t = time()
    while time()-t<delaytime do
        task_yield()
    end while
end procedure

integer sem = semaphore(4)

procedure worker()
    acquire(sem)
    printf(1,"- Task %d acquired semaphore.\n",task_self())
    delay(2)
    release(sem)
    printf(1,"+ Task %d released semaphore.\n",task_self())
end procedure

for i=1 to 10 do
    integer task = task_create(routine_id("worker"),{})
    task_schedule(task,1)
    task_yield()
end for

integer sc = 0
atom t0 = time()+1
while length(task_list())>1 do
    task_yield()
    integer scnew = count(sem)
    if scnew!=sc
    or time()>t0 then
        sc = scnew
        printf(1,"Semaphore count now %d\n",{sc})
        t0 = time()+2
    end if
end while
?"done"
```

{{out}}

```txt

- Task 2 acquired semaphore.
- Task 3 acquired semaphore.
- Task 4 acquired semaphore.
- Task 5 acquired semaphore.
Semaphore count now 0
+ Task 4 released semaphore.
- Task 6 acquired semaphore.
+ Task 3 released semaphore.
- Task 7 acquired semaphore.
+ Task 2 released semaphore.
- Task 8 acquired semaphore.
+ Task 5 released semaphore.
- Task 9 acquired semaphore.
Semaphore count now 0
+ Task 9 released semaphore.
- Task 10 acquired semaphore.
+ Task 8 released semaphore.
- Task 11 acquired semaphore.
+ Task 7 released semaphore.
+ Task 6 released semaphore.
Semaphore count now 2
+ Task 11 released semaphore.
+ Task 10 released semaphore.
Semaphore count now 4
"done"

```



## PicoLisp


```PicoLisp
(let Sem (tmp "sem")
   (for U 4  # Create 4 concurrent units
      (unless (fork)
         (ctl Sem
            (prinl "Unit " U " aquired the semaphore")
            (wait 2000)
            (prinl "Unit " U " releasing the semaphore") )
         (bye) ) ) )
```



## PureBasic

This launches a few threads in parallel, but restricted by the counter.
After a thread has completed it releases the Semaphore and a new thread will
be able to start.

```PureBasic
#Threads=10
#Parallels=3
Global Semaphore=CreateSemaphore(#Parallels)

Procedure Worker(*arg.i)
  WaitSemaphore(Semaphore)
  Debug "Thread #"+Str(*arg)+" active."
  Delay(Random(2000))
  SignalSemaphore(Semaphore)
EndProcedure

; Start a multi-thread based work
Dim thread(#Threads)
For i=0 To #Threads
  thread(i)=CreateThread(@Worker(),i)
Next
Debug "Launcher done."

; Wait for all threads to finish before closing down
For i=0 To #Threads
  If IsThread(i)
    WaitThread(i)
  EndIf
Next
```

Sample output

```txt
Thread #0 active.
Thread #2 active.
Thread #4 active.
Launcher done.
Thread #1 active.
Thread #3 active.
Thread #5 active.
Thread #7 active.
Thread #9 active.
Thread #6 active.
Thread #8 active.
Thread #10 active.
```



## Python

Python threading module includes a semaphore implementation. This code show how to use it.


```python
import time
import threading

# Only 4 workers can run in the same time
sem = threading.Semaphore(4)

workers = []
running = 1


def worker():
    me = threading.currentThread()
    while 1:
        sem.acquire()
        try:
            if not running:
                break
            print '%s acquired semaphore' % me.getName()
            time.sleep(2.0)
        finally:
            sem.release()
        time.sleep(0.01) # Let others acquire

# Start 10 workers
for i in range(10):
    t = threading.Thread(name=str(i), target=worker)
    workers.append(t)
    t.start()

# Main loop
try:
    while 1:
        time.sleep(0.1)
except KeyboardInterrupt:
    running = 0
    for t in workers:
        t.join()
```



## Racket



```racket

#lang racket

(define sema (make-semaphore 4)) ; allow 4 concurrent jobs

;; start 20 jobs and wait for all of them to end
(for-each
 thread-wait
 (for/list ([i 20])
   (thread (λ() (semaphore-wait sema)
                (printf "Job #~a acquired semaphore\n" i)
                (sleep 2)
                (printf "Job #~a done\n" i)
                (semaphore-post sema)))))

```



## Raven

Counting semaphores are built in:


```raven
# four workers may be concurrent
4 semaphore as sem

thread worker
    5 each as i
        sem acquire
        # tid is thread id
        tid "%d acquired semaphore\n" print
        2000 ms
        sem release
        # let others acquire
        100 ms

# start 10 threads
group
    10 each drop worker
list as workers
```


Thread joining is automatic by default.


## Ruby


This one uses SizedQueue class from the standard library since it blocks when the size limit is reached. An alternative approach would be having a mutex and a counter and blocking explicitly.

```ruby

require 'thread'

# Simple Semaphore implementation
class Semaphore
  def initialize(size = 1)
    @queue = SizedQueue.new(size)
    size.times { acquire }
  end

  def acquire
    tap { @queue.push(nil) }
  end

  def release
    tap { @queue.pop }
  end

  # @return [Integer]
  def count
    @queue.length
  end

  def synchronize
    release
    yield
  ensure
    acquire
  end
end

def foo(id, sem)
  sem.synchronize do
    puts "Thread #{id} Acquired lock"
    sleep(2)
  end
end

threads = []
n = 5
s = Semaphore.new(3)
n.times do |i|
  threads << Thread.new { foo i, s }
end
threads.each(&:join)


```



## Scala


```Scala
class CountingSemaphore(var maxCount: Int) {
  private var lockCount = 0

  def acquire(): Unit = {
    while ( {
      lockCount >= maxCount
    }) wait()
    lockCount += 1
  }

  def release(): Unit = {
    if (lockCount > 0) {
      lockCount -= 1
      notifyAll()
    }
  }

  def getCount: Int = lockCount
}

object Worker {
  def main(args: Array[String]): Unit = {
    val (lock, crew) = (new CountingSemaphore(3), new Array[Worker](5))

    for { i <- 0 until 5} {
      crew(i) = new Worker(lock, i)
      crew(i).start()
    }
  }
}
```


## Tcl

{{works with|Tcl|8.6}}
Uses the Thread package, which is expected to form part of the overall Tcl 8.6 release.

```tcl
package require Tcl 8.6
package require Thread

# Create the global shared state of the semaphore
set handle semaphore0
tsv::set $handle mutex [thread::mutex create]
tsv::set $handle cv [thread::cond create]
tsv::set $handle count 0
tsv::set $handle max 3

# Make five worker tasks
for {set i 0} {$i<5} {incr i} {
    lappend threads [thread::create -preserved {
	# Not bothering to wrap this in an object for demonstration
	proc init {handle} {
	    global mutex cv count max
	    set mutex [tsv::object $handle mutex]
	    set cv [tsv::object $handle cv]
	    set count [tsv::object $handle count]
	    set max [tsv::get $handle max]
	}
	proc acquire {} {
	    global mutex cv count max
	    thread::mutex lock [$mutex get]
	    while {[$count get] >= $max} {
		thread::cond wait [$cv get] [$mutex get]
	    }
	    $count incr
	    thread::mutex unlock [$mutex get]
	}
	proc release {} {
	    global mutex cv count max
	    thread::mutex lock [$mutex get]
	    if {[$count get] > 0} {
		$count incr -1
		thread::cond notify [$cv get]
	    }
	    thread::mutex unlock [$mutex get]
	}

        # The core task of the worker
	proc run {handle id} {
	    init $handle
	    acquire
	    puts "worker $id has acquired the lock"
	    after 2000
	    release
	    puts "worker $id is done"
	}

        # Wait for further instructions from the main thread
	thread::wait
    }]
}

# Start the workers doing useful work, giving each a unique id for pretty printing
set i 0
foreach t $threads {
    puts "starting thread [incr i]"
    thread::send -async $t [list run $handle $i]
}

# Wait for all the workers to finish
foreach t $threads {
    thread::release -wait $t
}
```



## UnixPipes

The number of concurrent jobs can be set by issuing that many echo '1''s at the begining to sem.


```bash
rm -f sem ; mkfifo sem

acquire() {
   x='';while test -z "$x"; do read x; done;
}

release() {
   echo '1'
}

job() {
   n=$1; echo "Job $n acquired Semaphore">&2 ; sleep 2; echo "Job $n released Semaphore">&2 ;
}

( acquire < sem ; job 1 ; release > sem ) &
( acquire < sem ; job 2 ; release > sem ) &
( acquire < sem ; job 3 ; release > sem ) &

echo 'Initialize Jobs' >&2 ; echo '1' > sem
```



## Visual Basic .NET


This code shows using a local semaphore. Semaphores can also be named, in which case they will be shared system wide.


```vbnet
Dim sem As New Semaphore(5, 5) 'Indicates that up to 5 resources can be aquired
sem.WaitOne() 'Blocks until a resouce can be aquired
Dim oldCount = sem.Release() 'Returns a resource to the pool
'oldCount has the Semaphore's count before Release was called
```



## zkl

Semaphores are built in.

```zkl
fcn job(name,sem){
   name.println(" wait"); sem.acquire();
   name.println(" go"); Atomic.sleep(2);
   sem.release(); name.println(" done")
}
   // start 3 threads using the same semphore
s:=Thread.Semaphore(1);
job.launch("1",s); job.launch("2",s); job.launch("3",s);
```

{{out}}

```txt

2 wait
2 go
1 wait
3 wait
2 done
1 go
1 done
3 go
3 done

```



{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have concurrency or background processes. -->
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|PARI/GP}}
{{omit from|Retro|No concurrency}}
{{omit from|ZX Spectrum Basic|Does not support concurrency.}}
