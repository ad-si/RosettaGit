+++
title = "Mutex"
description = ""
date = 2018-07-05T18:04:49Z
aliases = []
[extra]
id = 3034
[taxonomies]
categories = []
tags = []
+++

{{requires|Concurrency}}
[[Category:Encyclopedia]]

A '''mutex''' (''abbreviated'' '''Mut'''ually '''Ex'''clusive access) is a synchronization object, a variant of [[semaphore]] with ''k''=1.
A mutex is said to be seized by a [[task]] decreasing ''k''.
It is released when the task restores ''k''. Mutexes are typically used to protect a shared resource from concurrent access.
A [[task]] seizes (or acquires) the mutex, then accesses the resource, and after that releases the mutex.

A mutex is a low-level synchronization primitive exposed to deadlocking. A deadlock can occur with just two tasks and two mutexes (if each task attempts to acquire both mutexes, but in the opposite order).
Entering the deadlock is usually aggravated by a [[race condition]] state, which leads to sporadic hangups, which are very difficult to track down.

=Variants of mutexes=

==Global and local mutexes==
Usually the [[OS]] provides various implementations of mutexes corresponding to the variants of [[task]]s available in the OS. For example, system-wide mutexes can be used by [[process]]es. Local mutexes can be used only by [[threads]] etc. This distinction is maintained because, depending on the hardware, seizing a global mutex might be a thousand times slower than seizing a local one.

==Reentrant mutex==
A reentrant mutex can be seized by the same [[task]] multiple times. Each seizing of the mutex is matched by releasing it, in order to allow another task to seize it.

==Read write mutex==
A read write mutex can be seized at two levels for ''read'' and for ''write''. The mutex can be seized for ''read'' by any number of tasks. Only one task may seize it for '''write''. Read write mutexes are usually used to protect resources which can be accessed in mutable and immutable ways. Immutable (read) access is granted concurrently for many tasks because they do not change the resource state. Read write mutexes can be reentrant, global or local. Further, promotion operations may be provided. That's when a [[task]] that has seized the mutex for ''write'' releases it while keeping seized for ''read''. Note that the reverse operation is potentially deadlocking and requires some additional access policy control.

=Deadlock prevention=
There exists a simple technique of deadlock prevention when mutexes are seized in some fixed order. This is discussed in depth in the [[Dining philosophers]] problem.

=Sample implementations / APIs=


## Ada

[[Ada]] provides higher-level concurrency primitives, which are complete in the sense that they also allow implementations of the lower-level ones, like mutexes. Here is an implementation of a plain non-reentrant mutex based on protected objects.

The mutex interface:

```ada
protected type Mutex is
   entry Seize;
   procedure Release;
private
   Owned : Boolean := False;
end Mutex;
```

The implementation of:

```ada
protected body Mutex is
   entry Seize when not Owned is
   begin
      Owned := True;
   end Seize;
   procedure Release is
   begin
      Owned := False;
   end Release;
end Mutex;
```

Here the entry Seize has a queue of the [[task]]s waiting for the mutex. The entry's barrier is closed when Owned is true. So any task calling to the entry will be queued. When the barrier is open the first task from the queue executes the entry and Owned becomes true closing the barrier again. The procedure Release simply sets Owned to false. Both Seize and Release are protected actions whose execution causes reevaluation of all barriers, in this case one of Seize.

Use:

```ada
declare
   M : Mutex;
begin
   M.Seize;    -- Wait infinitely for the mutex to be free
   ...         -- Critical code
   M.Release;  -- Release the mutex
   ...
   select
      M.Seize; -- Wait no longer than 0.5s
   or delay 0.5;
      raise Timed_Out;
   end select;
   ...         -- Critical code
   M.Release;  -- Release the mutex
end;
```

It is also possible to implement mutex as a monitor task.

## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      REM Create mutex:
      SYS "CreateMutex", 0, 0, 0 TO hMutex%

      REM Wait to acquire mutex:
      REPEAT
        SYS "WaitForSingleObject", hMutex%, 1 TO res%
      UNTIL res% = 0

      REM Release mutex:
      SYS "ReleaseMutex", hMutex%

      REM Free mutex:
      SYS "CloseHandle", hMutex%
```



## C



### Win32

{{works with|Win32}}
To create a mutex operating system "object":

```c
HANDLE hMutex = CreateMutex(NULL, FALSE, NULL);
```

To lock the mutex:

```c
WaitForSingleObject(hMutex, INFINITE);
```

To unlock the mutex

```c
ReleaseMutex(hMutex);
```

When the program is finished with the mutex:

```c
CloseHandle(hMutex);
```



### POSIX

{{works with|POSIX}}

Creating a mutex:


```c
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
```


Or:


```c
pthread_mutex_t mutex;
pthread_mutex_init(&mutex, NULL);
```


Locking:


```c
int error = pthread_mutex_lock(&mutex);
```


Unlocking:


```c
int error = pthread_mutex_unlock(&mutex);
```


Trying to lock (but do not wait if it can't)


```c
int error = pthread_mutex_trylock(&mutex);
```



## C++


### Win32

{{works with|Win32}}
:[[Mutex#Win32|See C example]]

### POSIX

{{works with|POSIX}}
:[[Mutex#POSIX|See C example]]

### C++11

[http://www.cplusplus.com/reference/mutex/mutex/ C++11 reference for mutexe related functionality in the standard library]


## D


```d

class Synced
{
public:
    synchronized int func (int input)
    {
        num += input;
        return num;
    }
private:
    static num = 0;
}

```


Keep in mind that '''synchronized''' used as above works on a per-class-instance basis.
This is described in [[http://digitalmars.com/d/1.0/class.html##synchronized-functions|documentation]].

The following example tries to illustrate the problem:

```D
import tango.core.Thread, tango.io.Stdout, tango.util.log.Trace;

class Synced {
    public synchronized int func (int input) {
        Trace.formatln("in {} at func enter: {}", input, foo);
        // stupid loop to consume some time
        int arg;
        for (int i = 0; i < 1000*input; ++i) {
            for (int j = 0; j < 10_000; ++j) arg += j;
        }
        foo += input;
        Trace.formatln("in {} at func exit: {}", input, foo);
        return arg;
    }
    private static int foo;
}

void main(char[][] args) {
    SimpleThread[] ht;
    Stdout.print( "Starting application..." ).newline;

    for (int i=0; i < 3; i++) {
        Stdout.print( "Starting thread for: " )(i).newline;
        ht ~= new SimpleThread(i+1);
        ht[i].start();
    }

    // wait for all threads
    foreach( s; ht )
        s.join();
}

class SimpleThread : Thread
{
    private int d_id;
    this (int id) {
        super (&run);
        d_id = id;
    }

    void run() {
        auto tested = new Synced;
        Trace.formatln ("in run() {}", d_id);
        tested.func(d_id);
    }
}
```


Every created thread creates its own '''Synced''' object, and because the monitor created by synchronized statement is created for every object,
each thread can enter the ''func()'' method.

To resolve that either ''func()'' could be done static (static member functions are synchronized per-class basis) or '''synchronized block''' should be used
like here:


```D

class Synced {
    public int func (int input) {
        synchronized(Synced.classinfo) {
            // ...
            foo += input;
            // ...
        }
        return arg;
    }
    private static int foo;
}

```



## E


E's approach to concurrency is to ''never'' block, in favor of message passing/event queues/callbacks. Therefore, it is unidiomatic to use a mutex at all, and incorrect, or rather ''unsafe'', to use a mutex which blocks the calling thread. That said, here is a mutex written in E.


```e
def makeMutex() {

    # The mutex is available (released) if available is resolved, otherwise it
    # has been seized/locked. The specific value of available is irrelevant.
    var available := null

    # The interface to the mutex is a function, taking a function (action)
    # to be executed.
    def mutex(action) {
        # By assigning available to our promise here, the mutex remains
        # unavailable to the /next/ caller until /this/ action has gotten
        # its turn /and/ resolved its returned value.
        available := Ref.whenResolved(available, fn _ { action <- () })
    }
    return mutex
}
```


This implementation of a mutex is designed to have a very short implementation as well as usage in E. The mutex object is a function which takes a function ''action'' to be executed once the mutex is available. The mutex is unavailable until the return value of ''action'' resolves. This interface has been chosen over lock and unlock operations to reduce the hazard of unbalanced lock/unlock pairs, and because it naturally fits into E code.

Usage example:


```e
Creating the mutex:

? def mutex := makeMutex()
# value: <mutex>

Creating the shared resource:

? var value := 0
# value: 0

Manipulating the shared resource non-atomically so as to show a problem:

? for _ in 0..1 {
>     when (def v := (&value) <- get()) -> {
>         (&value) <- put(v + 1)
>     }
> }

? value
# value: 1

The value has been incremented twice, but non-atomically, and so is 1 rather
than the intended 2.

? value := 0
# value: 0

This time, we use the mutex to protect the action.

? for _ in 0..1 {
>     mutex(fn {
>         when (def v := (&value) <- get()) -> {
>             (&value) <- put(v + 1)
>         }
>     })
> }

? value
# value: 2
```


<code>when</code> blocks and <code>Ref.whenResolved</code> return a ''promise'' for the result of the deferred action, so the mutex here waits for the gratuitously complicated increment to complete before becoming available for the next action.


## Erlang

Erlang has no mutexes so this is a super simple one, hand built to allow 3 slowly printing processes to print until done before the next one starts.

```Erlang

-module( mutex  ).

-export( [task/0] ).

task() ->
	Mutex = erlang:spawn( fun() -> loop() end ),
	[erlang:spawn(fun() -> random:seed( X, 0, 0 ), print(Mutex, X, 3) end) || X <- lists:seq(1, 3)].



loop() ->
	receive
	{acquire, Pid} ->
		Pid ! {access, erlang:self()},
		receive
		{release, Pid} -> loop()
		end
	end.

mutex_acquire( Pid ) ->
	Pid ! {acquire, erlang:self()},
	receive
	{access, Pid} -> ok
	end.

mutex_release( Pid ) -> Pid ! {release, erlang:self()}.

print( _Mutex, _N, 0 ) -> ok;
print( Mutex, N, M ) ->
	timer:sleep( random:uniform(100) ),
	mutex_acquire( Mutex ),
	io:fwrite( "Print ~p: ", [N] ),
	[print_slow(X) || X <- lists:seq(1, 3)],
	io:nl(),
	mutex_release( Mutex ),
	print( Mutex, N, M - 1 ).

print_slow( X ) ->
	io:fwrite( " ~p", [X] ),
	timer:sleep( 100 ).

```

{{out}}

```txt

27> mutex:task().
Print 2:  1 2 3
Print 1:  1 2 3
Print 3:  1 2 3
Print 2:  1 2 3
Print 1:  1 2 3
Print 3:  1 2 3
Print 2:  1 2 3
Print 1:  1 2 3
Print 3:  1 2 3

```



## Go


### sync.Mutex

{{trans|E}}
Go has mutexes, and here is an example use of a mutex, somewhat following the example of E.  This code defines a slow incrementer, that reads a variable, then a significant amount of time later, writes an incremented value back to the variable.  Two incrementers are started concurrently.  Without the mutex, one would overwrite the other and the result would be 1.  Using a mutex, as shown here, one waits for the other and the result is 2.

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

var value int
var m sync.Mutex
var wg sync.WaitGroup

func slowInc() {
    m.Lock()
    v := value
    time.Sleep(1e8)
    value = v+1
    m.Unlock()
    wg.Done()
}

func main() {
    wg.Add(2)
    go slowInc()
    go slowInc()
    wg.Wait()
    fmt.Println(value)
}
```

{{out}}

```txt

2

```

Read-write mutex is provided by the <tt>sync.RWMutex</tt> type.
For a code example using a RWMutex, see [[Atomic updates#RWMutex]].


### Channels

If a mutex is exactly what you need, sync.Mutex is there.
As soon as things start getting complicated though, Go channels offer a much clearer alternative.
As a gateway from mutexes to channels, here is the above program implemented with channels:

```go
package main

import (
    "fmt"
    "time"
)

var value int

func slowInc(ch, done chan bool) {
    // channel receive, used here to implement mutex lock.
    // it will block until a value is available on the channel
    <-ch

    // same as above
    v := value
    time.Sleep(1e8)
    value = v + 1

    // channel send, equivalent to mutex unlock.
    // makes a value available on channel
    ch <- true

    // channels can be used to signal completion too
    done <- true
}

func main() {
    ch := make(chan bool, 1) // ch used as a mutex
    done := make(chan bool)  // another channel used to signal completion
    go slowInc(ch, done)
    go slowInc(ch, done)
    // a freshly created sync.Mutex starts out unlocked, but a freshly created
    // channel is empty, which for us represents "locked."  sending a value on
    // the channel puts the value up for grabs, thus representing "unlocked."
    ch <- true
    <-done
    <-done
    fmt.Println(value)
}
```

The value passed on the channel is not accessed here, just as the internal state of a mutex is not accessed.  Rather, it is only the effect of the value being available that is important.  (Of course if you wanted to send something meaningful on the channel, a reference to the shared resource would be a good start...)


## Haskell

Haskell has a slight variation on the mutex, namely the MVar. MVars, unlike mutexes, are containers. However, they are similar enough that MVar () is essentially a mutex. A MVar can be in two states: empty or full, only storing a value when full. There are 4 main ways to deal with MVars:


```haskell
takeMVar :: MVar a -> IO a
putMVar :: MVar a -> a -> IO ()
tryTakeMVar :: MVar a -> IO (Maybe a)
tryPutMVar :: MVar a -> a -> IO Bool

```


takeMVar will attempt to fetch a value from the MVar, and will block while the MVar is empty. After using this, the MVar will be left empty.
putMVar will attempt to put a value in a MVar, and will block while there already is a value in the MVar. This will leave the MVar full.
The last two functions are non-blocking versions of takeMVar and putMVar, returning Nothing and False, respectively, if their blocking counterpart would have blocked.

For more information see the [http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html documentation].

==Icon and {{header|Unicon}}==
The following code uses features exclusive to Unicon.


```Unicon


   x: = mutex()  # create and return a mutex handle for sharing between threads needing to synchronize with each other

   lock(x)       # lock mutex x

   trylock(x))   # non-blocking lock, succeeds only if there are no other thread already in the critical region

   unlock(x)     # unlock mutex x


```


## Java

{{works with|Java|1.5+}}

Java 5 added a <code>Semaphore</code> class which can act as a mutex (as stated above, a mutex is "a variant of semaphore with ''k''=1").

```java5
import java.util.concurrent.Semaphore;

public class VolatileClass{
   public Semaphore mutex = new Semaphore(1); //also a "fair" boolean may be passed which,
                                              //when true, queues requests for the lock
   public void needsToBeSynched(){
      //...
   }
   //delegate methods could be added for acquiring and releasing the mutex
}
```

Using the mutex:

```java5
public class TestVolitileClass throws Exception{
   public static void main(String[] args){
      VolatileClass vc = new VolatileClass();
      vc.mutex.acquire(); //will wait automatically if another class has the mutex
                          //can be interrupted similarly to a Thread
                          //use acquireUninterruptibly() to avoid that
      vc.needsToBeSynched();
      vc.mutex.release();
   }
}
```


Java also has the synchronized keyword, which allows almost any object to be used to enforce mutual exclusion.


```java
public class Main {
    static Object mutex = new Object();
    static int i = 0;

    public void addAndPrint()
    {
        System.out.print("" + i + " + 1 = ");
        i++;
        System.out.println("" + i);
    }

    public void subAndPrint()
    {
        System.out.print("" + i + " - 1 = ");
        i--;
        System.out.println("" + i);
    }


    public static void main(String[] args){
        final Main m = new Main();
        new Thread() {
            public void run()
            {
                while (true) { synchronized(m.mutex) { m.addAndPrint(); } }
            }
        }.start();
        new Thread() {
            public void run()
            {
                while (true) { synchronized(m.mutex) { m.subAndPrint(); } }
            }
        }.start();
    }
}
```


The "synchronized" keyword actually is a form of [[monitor]], which was a later-proposed solution to the same problems that mutexes and semaphores were designed to solve.  More about synchronization may be found on Sun's website - http://java.sun.com/docs/books/tutorial/essential/concurrency/sync.html , and more about monitors may be found in any decent operating systems textbook.


## Logtalk

Logtalk provides a synchronized/0 directive for synchronizing all object (or category) predicates using the same implicit mutex and a synchronized/1 directive for synchronizing a set of predicates using the same implicit mutex. Follow an usage example of the synchronized/1 directive (inspired by the Erlang example). Works when using SWI-Prolog, XSB, or YAP as the backend compiler.

```logtalk

:- object(slow_print).

    :- threaded.

    :- public(start/0).

    :- private([slow_print_abc/0, slow_print_123/0]).
    :- synchronized([slow_print_abc/0, slow_print_123/0]).

    start :-
        % launch two threads, running never ending goals
        threaded((
            repeat_abc,
            repeat_123
        )).

    repeat_abc :-
        repeat, slow_print_abc, fail.

    repeat_123 :-
        repeat, slow_print_123, fail.

    slow_print_abc :-
        write(a), thread_sleep(0.2),
        write(b), thread_sleep(0.2),
        write(c), nl.

    slow_print_123 :-
        write(1), thread_sleep(0.2),
        write(2), thread_sleep(0.2),
        write(3), nl.

:- end_object.

```

{{out}}

```txt

?- slow_print::start.
abc
123
abc
123
abc
123
abc
123
abc
...

```



## M2000 Interpreter

We can simulate mutex. Try with Thread.Plan Sequential
Using concurrent (in interpreter level), after the execution of one statement, thread change. Using Sequential each thread block run all statements, until end, or leave some if a continue take place. In concurrent also a call to a module, or executing a block of code happen without in one thread.


```M2000 Interpreter

Form 80, 50
Module CheckIt {
      Thread.Plan Concurrent
      Class mutex {
            mylock as boolean=True
            Function Lock {
                  if not .mylock then exit
                   .mylock<=False
                   =True
            }
            Module Unlock {
                  .mylock<=True
            }
      }
      Group PhoneBooth {
            NowUser$
            module UseIt (a$, x){
                  .NowUser$<=a$
                  Print a$+" phone home ",Int(x*100);"%"
            }
            module leave {
                  .NowUser$<=""
            }
      }
      m=mutex()
      Flush
      Data "Bob", "John","Tom"
      For i=1 to 3 {
            Thread {
                  \\ we use N$, C and Max as stack variables for each thread
                  \\ all other variables are shared for module
                  If C=0 Then if not m.lock() then Print N$+" waiting...................................": Continue
                  C++
                  if c=1 then thread this interval 20
                  PhoneBooth.UseIt N$,C/Max
                  iF C<Max  Then Continue
                  PhoneBooth.leave
                  m.Unlock
                  Thread This Erase
            } as K
            Read M$
            Thread K Execute Static N$=M$,  C=0, Max=RANDOM(5,8)
            Thread K interval Random(300, 2000)
      }
      \\ Start we lock Phone Booth for service
      Service=m.lock()
      Main.Task 50 {
            \\ a block in a thread run alone
            {
                  If Service Then if Keypress(32) then m.unlock: Service=false: Continue
                  If not Service then if Keypress(32)  Then if m.lock() then Service=true : Continue
                  if PhoneBooth.NowUser$<>"" Then  {
                        Print "Phone:";PhoneBooth.NowUser$
                  } Else.if Service then Print "Service Time"
            }
      }
}
CheckIt

```



## Nim

For mutexes (called locks in Nim) threads support is required,
so compile using <code>nim --threads:on c mutex</code>

Creating a mutex:

```nim
import locks

var mutex: TLock
initLock mutex
```

Locking:

```nim
acquire mutex
```

Unlocking:

```nim
release mutex
```

# Trying to lock (but do not wait if it can't)

```nim
let success = tryAcquire mutex
```


=={{header|Objective-C}}==


```objc
NSLock *m = [[NSLock alloc] init];

[m lock]; // locks in blocking mode

if ([m tryLock]) { // acquire a lock -- does not block if not acquired
  // lock acquired
} else {
  // already locked, does not block
}

[m unlock];
```


Reentrant mutex is provided by the <tt>NSRecursiveLock</tt> class.

Objective-C also has [http://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ObjectiveC/Articles/ocThreading.html#//apple_ref/doc/uid/TP30001163-CH19-BCIIGGHG @synchronized() blocks], like Java.


## Objeck

Objeck provides a simple way to lock a section of code.  Please refer to the [[Objeck|programer's guide]] for addition information.


```objeck

m := ThreadMutex->New("lock a");
# section locked
critical(m) {
   ...
}
# section unlocked

```



## OCaml


OCaml provides a built-in [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Mutex.html Mutex module]. <BR>
It is very simple, there are four functions:


```ocaml
let m = Mutex.create() in
Mutex.lock m;  (* locks in blocking mode *)

if (Mutex.try_lock m)
then ...  (* did the lock *)
else ...  (* already locked, do not block *)

Mutex.unlock m;
```



## Oforth


Oforth has no mutex.
A mutex can be simulated using a channel initialized with one object.
A task can receive the object from the channel (get the mutex) and send it to the channel when the job is done.
If the channel is empty, a task will wait until an object is available into the channel.


```Oforth
import: parallel

: job(mut)
   mut receive drop
   "I get the mutex !" .
   2000 sleep
   "Now I release the mutex" println
   1 mut send drop ;

: mymutex
| mut |
   Channel new dup send(1) drop ->mut
   10 #[ #[ mut job ] & ] times ;
```



## Oz

Oz has "locks" which are local, reentrant mutexes.

Creating a mutex:

```oz
declare L = {Lock.new}
```


The only way to acquire a mutex is to use the <code>lock</code> syntax. This ensures that releasing a lock can never be forgotten. Even if an exception occurs, the lock will be released.

```oz
lock L then
   {System.show exclusive}
end
```


To make it easier to work with objects, classes can be marked with the property <code>locking</code>. Instances of such classes have their own internal lock and can use a variant of the <code>lock</code> syntax:

```oz
class Test
   prop locking

   meth test
      lock
	 {Show exclusive}
      end
   end
end
```



## Perl

Code demonstrating shared resources and simple locking. Resource1 and Resource2 represent some limited resources that must be exclusively used and released by each thread.  Each thread reports how many of each is available; if it goes below zero, something is wrong.  Try comment out either of the "lock $lock*" line to see what happens without locking.

```Perl
use Thread qw'async';
use threads::shared;

my ($lock1, $lock2, $resource1, $resource2) :shared = (0) x 4;

sub use_resource {
        {       # curly provides lexical scope, exiting which causes lock to release
                lock $lock1;
                $resource1 --;          # acquire resource
                sleep(int rand 3);      # artifical delay to pretend real work
                $resource1 ++;          # release resource
                print "In thread ", threads->tid(), ": ";
                print "Resource1 is $resource1\n";
        }
        {
                lock $lock2;
                $resource2 --;
                sleep(int rand 3);
                $resource2 ++;
                print "In thread ", threads->tid(), ": ";
                print "Resource2 is $resource2\n";
        }
}

# create 9 threads and clean up each after they are done.
for ( map async{ use_resource }, 1 .. 9) {
        $_->join
}
```



## Perl 6


```perl6
my $lock = Lock.new;

$lock.protect: { your-ad-here() }
```

Locks are reentrant.  You may explicitly lock and unlock them, but the syntax above guarantees the lock will be unlocked on scope exit, even if by thrown exception or other exotic control flow. That being said, direct use of locks is discouraged in Perl 6 in favor of promises, channels, and supplies, which offer better composable semantics.

## PicoLisp

PicoLisp uses several mechanisms of interprocess communication, mainly within
the same process family (children of the same parent process) for database
synchronization (e.g.
'[http://software-lab.de/doc/refL.html#lock lock]',
'[http://software-lab.de/doc/refS.html#sync sync]' or
'[http://software-lab.de/doc/refT.html#tell tell]'.

For a simple synchronization of unrelated PicoLisp processes the
'[http://software-lab.de/doc/refA.html#acquire acquire]' /
'[http://software-lab.de/doc/refR.html#release release]' function pair
can be used.


## PureBasic


'''PureBasic has the following Mutex functions;'''

```PureBasic
MyMutex=CreateMutex()
Result = TryLockMutex(MyMutex)
LockMutex(MyMutex)
UnlockMutex(MyMutex)
FreeMutex(MyMutex)
```


'''Example'''

```PureBasic
Declare ThreadedTask(*MyArgument)
Define Mutex

If OpenConsole()
  Define thread1, thread2, thread3

  Mutex = CreateMutex()
  thread1 = CreateThread(@ThreadedTask(), 1):  Delay(5)
  thread2 = CreateThread(@ThreadedTask(), 2):  Delay(5)
  thread3 = CreateThread(@ThreadedTask(), 3)
  WaitThread(thread1)
  WaitThread(thread2)
  WaitThread(thread3)

  PrintN(#CRLF$+"Press ENTER to exit"): Input()
  FreeMutex(Mutex)
  CloseConsole()
EndIf

Procedure ThreadedTask(*MyArgument)
  Shared Mutex
  Protected a, b
  For a = 1 To 3
    LockMutex(Mutex)
    ; Without Lock-/UnLockMutex() here the output from the parallel threads would be all mixed.
    ; Reading/Writing to shared memory resources are a common use for Mutextes i PureBasic
    PrintN("Thread "+Str(*MyArgument)+": Print 3 numbers in a row:")
    For b = 1 To 3
      Delay(75)
      PrintN("Thread "+Str(*MyArgument)+" : "+Str(b))
    Next
    UnlockMutex(Mutex)
  Next
EndProcedure
```



## Python

Demonstrating semaphores.
Note that semaphores can be considered as a multiple version of mutex;
while a mutex allows a singular exclusive access to code or resources,
a semaphore grants access to a number of threads up to certain value.


```Python
import threading
from time import sleep

# res: max number of resources. If changed to 1, it functions
# identically to a mutex/lock object
res = 2
sema = threading.Semaphore(res)

class res_thread(threading.Thread):
	def run(self):
		global res
		n = self.getName()
		for i in range(1, 4):
			# acquire a resource if available and work hard
			# for 2 seconds.  if all res are occupied, block
			# and wait
			sema.acquire()
			res = res - 1
			print n, "+  res count", res
			sleep(2)

                        # after done with resource, return it to pool and flag so
			res = res + 1
			print n, "-  res count", res
			sema.release()

# create 4 threads, each acquire resorce and work
for i in range(1, 5):
	t = res_thread()
	t.start()
```



## Racket


Racket has semaphores which can be used as mutexes in the usual way.
With other language features this can be used to implement new features -- for example, here is how we would implement a protected-by-a-mutex function:

```racket

(define foo
  (let ([sema (make-semaphore 1)])
    (lambda (x)
      (dynamic-wind (λ() (semaphore-wait sema))
                    (λ() (... do something ...))
                    (λ() (semaphore-post sema))))))
```

and it is now easy to turn this into a macro for definitions of such functions:

```racket

(define-syntax-rule (define/atomic (name arg ...) E ...)
  (define name
    (let ([sema (make-semaphore 1)])
      (lambda (arg ...)
        (dynamic-wind (λ() (semaphore-wait sema))
                      (λ() E ...)
                      (λ() (semaphore-post sema)))))))
;; this does the same as the above now:
(define/atomic (foo x)
  (... do something ...))
```


But more than just linguistic features, Racket has many additional synchronization tools in its VM.
Some notable examples:
OS semaphore for use with OS threads, green threads, lightweight OS threads, and heavyweight OS threads, synchronization channels, thread mailboxes, CML-style event handling, generic synchronizeable event objects, non-blocking IO, etc, etc.


## Ruby

Ruby's standard library includes a <tt>mutex_m</tt> module that can be mixed-in
to a class.

```ruby
require 'mutex_m'

class SomethingWithMutex
  include Mutex_m
  ...
end
```


Individual objects can be extended with the module too

```ruby
an_object = Object.new
an_object.extend(Mutex_m)
```


An object with mutex powers can then:

```ruby
# acquire a lock -- block execution until it becomes free
an_object.mu_lock

# acquire a lock -- return immediately even if not acquired
got_lock = an_object.mu_try_lock

# have a lock?
if an_object.mu_locked? then ...

# release the lock
an_object.mu_unlock

# wrap a lock around a block of code -- block execution until it becomes free
an_object.my_synchronize do
  do critical stuff
end
```



## Tcl

Tcl's [http://tcl.cvs.sourceforge.net/*checkout*/tcl/thread/doc/html/thread.html#19 mutexes] have four functions.

```tcl
package require Thread

# How to create a mutex
set m [thread::mutex create]

# This will block if the lock is already held unless the mutex is made recursive
thread::mutex lock $m
# Now locked...
thread::mutex unlock $m
# Unlocked again

# Dispose of the mutex
thread::mutex destroy $m
```

There are also read-write mutexes available.

```tcl
set rw [thread::rwmutex create]

# Get and drop a reader lock
thread::rwmutex rlock $rw
thread::rwmutex unlock $rw

# Get and drop a writer lock
thread::rwmutex wlock $rw
thread::rwmutex unlock $rw

thread::rwmutex destroy $rw
```



## zkl

zkl has two mutex objects, Lock (mutex) and WriteLock a mutex that allows multiple readers but only one writer.
The critical keyword fences code to ensure the lock is released when the code is done.

```zkl
var lock=Atomic.Lock(); lock.acquire(); doSomething(); lock.release();
critical(lock){ doSomething(); }
```


```zkl
var lock=Atomic.WriteLock();
lock.acquireForReading(); doSomeReading(); lock.readerRelease();
critical(lock,acquireForReading,readerRelease){ ... }
lock.acquireForWriting(); write(); lock.writerRelease();
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures or objects. -->
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|ML/I}}
{{omit from|PARI/GP}}
{{omit from|Factor|Factor only supports cooperative threads for now}}
{{omit from|Unlambda|Does not support multithreading.}}
{{omit from|ZX Spectrum Basic|Does not support multithreading}}
