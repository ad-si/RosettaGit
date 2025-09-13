+++
title = "Checkpoint synchronization"
description = ""
date = 2019-07-27T21:15:46Z
aliases = []
[extra]
id = 7914
[taxonomies]
categories = ["task", "Concurrency"]
tags = []
+++

## Task

The checkpoint synchronization is a problem of synchronizing multiple [[task]]s. Consider a workshop where several workers ([[task]]s) assembly details of some mechanism. When each of them completes his work they put the details together. There is no store, so a worker who finished its part first must wait for others before starting another one. Putting details together is the ''checkpoint'' at which [[task]]s synchronize themselves before going their paths apart.

'''The task'''

Implement checkpoint synchronization in your language.

Make sure that the solution is [[Race condition|race condition]]-free. Note that a straightforward solution based on [[event]]s is exposed to [[Race condition|race condition]]. Let two [[task]]s A and B need to be synchronized at a checkpoint. Each signals its event (''EA'' and ''EB'' correspondingly), then waits for the AND-combination of the events (''EA''&''EB'') and resets its event. Consider the following scenario: A signals ''EA'' first and gets blocked waiting for ''EA''&''EB''. Then B signals ''EB'' and loses the processor. Then A is released (both events are signaled) and resets ''EA''. Now if B returns and enters waiting for ''EA''&''EB'', it gets lost.

When a worker is ready it shall not continue before others finish. A typical implementation bug is when a worker is counted twice within one working cycle causing its premature completion. This happens when the quickest worker serves its cycle two times while the laziest one is lagging behind.

If you can, implement workers joining and leaving.


## Ada


```Ada
with Ada.Calendar;               use Ada.Calendar;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Test_Checkpoint is

   package FR renames Ada.Numerics.Float_Random;
   No_Of_Cubicles: constant Positive := 3;
     -- That many workers can work in parallel
   No_Of_Workers: constant Positive := 6;
     -- That many workers are potentially available
     -- some will join the team when others quit the job

   type Activity_Array is array(Character) of Boolean;
     -- we want to know who is currently working

   protected Checkpoint is
      entry Deliver;
      entry Join (Label : out Character; Tolerance: out Float);
      entry Leave(Label : in Character);
   private
      Signaling     : Boolean   := False;
      Ready_Count   : Natural   := 0;
      Worker_Count  : Natural   := 0;
      Unused_Label  : Character := 'A';
      Likelyhood_To_Quit: Float := 1.0;
      Active        : Activity_Array := (others => false);
      entry Lodge;
   end Checkpoint;

   protected body Checkpoint is
      entry Join (Label : out Character; Tolerance: out Float)
      when not Signaling and Worker_Count < No_Of_Cubicles is
      begin
         Label        := Unused_Label;
         Active(Label):= True;
         Unused_Label := Character'Succ (Unused_Label);
         Worker_Count := Worker_Count + 1;
         Likelyhood_To_Quit := Likelyhood_To_Quit / 2.0;
         Tolerance    := Likelyhood_To_Quit;
      end Join;

      entry Leave(Label: in Character) when not Signaling is
      begin
         Worker_Count  := Worker_Count - 1;
         Active(Label) := False;
      end Leave;

      entry Deliver when not Signaling is
      begin
         Ready_Count := Ready_Count + 1;
         requeue Lodge;
      end Deliver;

      entry Lodge when Ready_Count = Worker_Count or Signaling is
      begin
         if Ready_Count = Worker_Count then
            Put("---Sync Point [");
            for C in Character loop
               if Active(C) then
                  Put(C);
               end if;
            end loop;
            Put_Line("]---");
         end if;
         Ready_Count := Ready_Count - 1;
         Signaling   := Ready_Count /= 0;
      end Lodge;
   end Checkpoint;

   task type Worker;

   task body Worker is
      Dice      : FR.Generator;
      Label     : Character;
      Tolerance : Float;
      Shift_End : Time := Clock + 2.0;
        -- Trade unions are hard!
   begin
      FR.Reset (Dice);
      Checkpoint.Join (Label, Tolerance);
      Put_Line(Label & " joins the team");
      loop
         Put_Line (Label & " is working");
         delay Duration (FR.Random (Dice) * 0.500);
         Put_Line (Label & " is ready");
         Checkpoint.Deliver;
         if FR.Random(Dice) < Tolerance then
            Put_Line(Label & " leaves the team");
            exit;
         elsif Clock >= Shift_End then
            Put_Line(Label & " ends shift");
            exit;
         end if;
      end loop;
      Checkpoint.Leave(Label);
   end Worker;
   Set : array (1..No_Of_Workers) of Worker;
begin
   null; -- Nothing to do here
end Test_Checkpoint;


```

Sample output:
<pre style="height: 200px;overflow:scroll">
A joins the team
A is working
B joins the team
B is working
C joins the team
C is working
B is ready
C is ready
A is ready
---Sync Point [ABC]---
A is working
C is working
B is working
C is ready
B is ready
A is ready
---Sync Point [ABC]---
A is working
B is working
C is working
B is ready
C is ready
A is ready
---Sync Point [ABC]---
A leaves the team
D joins the team
D is working
C is working
B is working
C is ready
B is ready
D is ready
---Sync Point [BCD]---
D is working
C is working
B is working
C is ready
B is ready
D is ready
---Sync Point [BCD]---
D is working
C is working
B is working
D is ready
B is ready
C is ready
---Sync Point [BCD]---
C leaves the team
E joins the team
E is working
D is working
B leaves the team
F joins the team
F is working
D is ready
E is ready
F is ready
---Sync Point [DEF]---
D is working
F is working
E is working
D is ready
F is ready
E is ready
---Sync Point [DEF]---
E ends shift
F ends shift
D ends shift

```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"TIMERLIB"
      nWorkers% = 3
      DIM tID%(nWorkers%)

      tID%(1) = FN_ontimer(10, PROCworker1, 1)
      tID%(2) = FN_ontimer(11, PROCworker2, 1)
      tID%(3) = FN_ontimer(12, PROCworker3, 1)

      DEF PROCworker1 : PROCtask(1) : ENDPROC
      DEF PROCworker2 : PROCtask(2) : ENDPROC
      DEF PROCworker3 : PROCtask(3) : ENDPROC

      ON ERROR PROCcleanup : REPORT : PRINT : END
      ON CLOSE PROCcleanup : QUIT

      REPEAT
        WAIT 0
      UNTIL FALSE
      END

      DEF PROCtask(worker%)
      PRIVATE cnt%()
      DIM cnt%(nWorkers%)
      CASE cnt%(worker%) OF
        WHEN 0:
          cnt%(worker%) = RND(30)
          PRINT "Worker "; worker% " starting (" ;cnt%(worker%) " ticks)"
        WHEN -1:
        OTHERWISE:
          cnt%(worker%) -= 1
          IF cnt%(worker%) = 0 THEN
            PRINT "Worker "; worker% " ready and waiting"
            cnt%(worker%) = -1
            PROCcheckpoint
            cnt%(worker%) = 0
          ENDIF
      ENDCASE
      ENDPROC

      DEF PROCcheckpoint
      PRIVATE checked%, sync%
      IF checked% = 0 sync% = FALSE
      checked% += 1
      WHILE NOT sync%
        WAIT 0
        IF checked% = nWorkers% THEN
          sync% = TRUE
          PRINT "--Sync Point--"
        ENDIF
      ENDWHILE
      checked% -= 1
      ENDPROC

      DEF PROCcleanup
      LOCAL I%
      FOR I% = 1 TO nWorkers%
        PROC_killtimer(tID%(I%))
      NEXT
      ENDPROC
```

'''Output:'''

```txt

Worker 1 starting (23 ticks)
Worker 3 starting (26 ticks)
Worker 2 starting (13 ticks)
Worker 2 ready and waiting
Worker 1 ready and waiting
Worker 3 ready and waiting
--Sync Point--
Worker 3 starting (2 ticks)
Worker 1 starting (23 ticks)
Worker 2 starting (2 ticks)
Worker 3 ready and waiting
Worker 2 ready and waiting
Worker 1 ready and waiting
--Sync Point--
Worker 3 starting (8 ticks)
Worker 1 starting (28 ticks)
Worker 2 starting (5 ticks)

```



## C

Using OpenMP.  Compiled with <code>gcc -Wall -fopenmp</code>.

```c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <omp.h>

int main()
{
        int jobs = 41, tid;
        omp_set_num_threads(5);

        #pragma omp parallel shared(jobs) private(tid)
        {
                tid = omp_get_thread_num();
                while (jobs > 0) {
                        /* this is the checkpoint */
                        #pragma omp barrier
                        if (!jobs) break;

                        printf("%d: taking job %d\n", tid, jobs--);
                        usleep(100000 + rand() / (double) RAND_MAX * 3000000);
                        printf("%d: done job\n", tid);
                }

                printf("[%d] leaving\n", tid);

                /* this stops jobless thread from exiting early and killing workers */
                #pragma omp barrier
        }

        return 0;
}
```




## C++

```cpp
#include <iostream>
#include <chrono>
#include <atomic>
#include <mutex>
#include <random>
#include <thread>

std::mutex cout_lock;

class Latch
{
    std::atomic<int> semafor;
  public:
    Latch(int limit) : semafor(limit) {}

    void wait()
    {
        semafor.fetch_sub(1);
        while(semafor.load() > 0)
            std::this_thread::yield();
    }
};

struct Worker
{
    static void do_work(int how_long, Latch& barrier, std::string name)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(how_long));
        {   std::lock_guard<std::mutex> lock(cout_lock);
            std::cout << "Worker " << name << " finished work\n";   }
        barrier.wait();
        {   std::lock_guard<std::mutex> lock(cout_lock);
            std::cout << "Worker " << name << " finished assembly\n";   }
    }
};

int main()
{
    Latch latch(5);
    std::mt19937 rng(std::random_device{}());
    std::uniform_int_distribution<> dist(300, 3000);
    std::thread threads[] {
        std::thread(&Worker::do_work, dist(rng), std::ref(latch), "John"),
        std::thread{&Worker::do_work, dist(rng), std::ref(latch), "Henry"},
        std::thread{&Worker::do_work, dist(rng), std::ref(latch), "Smith"},
        std::thread{&Worker::do_work, dist(rng), std::ref(latch), "Jane"},
        std::thread{&Worker::do_work, dist(rng), std::ref(latch), "Mary"},
    };
    for(auto& t: threads) t.join();
    std::cout << "Assembly is finished";
}
```

```txt

Worker Mary finished work
Worker Smith finished work
Worker John finished work
Worker Henry finished work
Worker Jane finished work
Worker Jane finished assembly
Worker Smith finished assembly
Worker Mary finished assembly
Worker Henry finished assembly
Worker John finished assembly
Assembly is finished

```



## Clojure

With a fixed number of workers, this would be very straightforward in Clojure by using a ''CyclicBarrier'' from ''java.util.concurrent''.
So to make it interesting, this version supports workers dynamically joining and parting, and uses the new (2013) ''core.async'' library to use Go-like channels.
Also, each worker passes a value to the checkpoint, so that some ''combine'' function could consume them once they're all received.

```clojure
(ns checkpoint.core
  (:gen-class)
  (:require [clojure.core.async :as async :refer [go <! >! <!! >!! alts! close!]]
            [clojure.string :as string]))

(defn coordinate [ctl-ch resp-ch combine]
  (go
    (<! (async/timeout 2000)) ;delay a bit to allow worker setup
    (loop [members {}, received {}] ;maps by in-channel of out-channels & received data resp.
      (let [rcvd-count (count received)
            release   #(doseq [outch (vals members)] (go (>! outch %)))
            received  (if (and (pos? rcvd-count) (= rcvd-count (count members)))
                        (do (-> received vals combine release) {})
                        received)
            [v ch] (alts! (cons ctl-ch (keys members)))]
              ;receive a message on ctrl-ch or any member input channel
        (if (= ch ctl-ch)
          (let [[op inch outch] v] ;only a Checkpoint (see below) sends on ctl-ch
            (condp = op
              :join (do (>! resp-ch :ok)
                        (recur (assoc members inch outch) received))
              :part (do (>! resp-ch :ok)
                        (close! inch) (close! outch)
                        (recur (dissoc members inch) (dissoc received inch)))
              :exit :exit))
          (if (nil? v) ;is the channel closed?
            (do
              (close! (get members ch))
              (recur (dissoc members ch) (dissoc received ch)))
            (recur members (assoc received ch v))))))))

(defprotocol ICheckpoint
  (join [this])
  (part [this inch outch]))

(deftype Checkpoint [ctl-ch resp-ch sync]
  ICheckpoint
  (join [this]
    (let [inch (async/chan), outch (async/chan 1)]
      (go
        (>! ctl-ch [:join inch outch])
        (<! resp-ch)
        [inch outch])))
  (part [this inch outch]
    (go
      (>! ctl-ch [:part inch outch]))))

(defn checkpoint [combine]
  (let [ctl-ch (async/chan), resp-ch (async/chan 1)]
    (->Checkpoint ctl-ch resp-ch (coordinate ctl-ch resp-ch combine))))

(defn worker
  ([ckpt repeats] (worker ckpt repeats (fn [& args] nil)))
  ([ckpt repeats mon]
    (go
      (let [[send recv] (<! (join ckpt))]
        (doseq [n (range repeats)]
          (<! (async/timeout (rand-int 5000)))
          (>! send n) (mon "sent" n)
          (<! recv)  (mon "recvd"))
        (part ckpt send recv)))))


(defn -main
  [& args]
  (let [ckpt (checkpoint identity)
        monitor (fn [id]
                  (fn [& args] (println (apply str "worker" id ":" (string/join " " args)))))]
    (worker ckpt 10 (monitor 1))
    (worker ckpt 10 (monitor 2))))


```



## D


```d
import std.stdio;
import std.parallelism: taskPool, defaultPoolThreads, totalCPUs;

void buildMechanism(uint nparts) {
    auto details = new uint[nparts];
    foreach (i, ref detail; taskPool.parallel(details)) {
        writeln("Build detail ", i);
        detail = i;
    }

    // This could be written more concisely via std.parallelism.reduce,
    // but we want to see the checkpoint explicitly.
    writeln("Checkpoint reached. Assemble details ...");
    uint sum = 0;
    foreach (immutable detail; details)
        sum += detail;
    writeln("Mechanism with ", nparts, " parts finished: ", sum);
}

void main() {
    defaultPoolThreads = totalCPUs + 1; // totalCPUs - 1 on default.
    buildMechanism(42);
    buildMechanism(11);
}
```

```txt
Build detail 0
Build detail 2
Build detail 6
Build detail 3
Build detail 8
Build detail 10
Build detail 4
Build detail 5
Build detail 7
Build detail 9
Build detail 1
Checkpoint reached. Assemble details ...
Mechanism with 11 parts finished: 55
```



## E


The problem as stated is somewhat unnatural in E. We would prefer to define the control flow in association with the data flow; for example, such that the workers return values that are combined at the checkpoint; the availability of that result value naturally defines when the workers should proceed with the next round.

That said, here is an implementation of the task as stated. We start by defining a 'flag set' data structure (which is hopefully also useful for other problems), which allows us to express the checkpoint algorithm straightforwardly while being protected against the possibility of a task calling <code>deliver</code> or <code>leave</code> too many times. Note also that each task gets its own reference denoting its membership in the checkpoint group; thus it can only speak for itself and not break any global invariants.


```e
/** A flagSet solves this problem: There are N things, each in a true or false
  * state, and we want to know whether they are all true (or all false), and be
  * able to bulk-change all of them, and all this without allowing double-
  * counting -- setting a flag twice is idempotent.
  */
def makeFlagSet() {
  # Each flag object is either in the true set or the false set.
  def trues := [].asSet().diverge()
  def falses := [].asSet().diverge()
  return def flagSet {
    /** Add a flag to the set. */
    to join() {
      def flag {
        /** Get the value of this flag. */
        to get() :boolean {

        }
        /** Set the value of this flag. */
        to put(v :boolean) {
          def [del,add] := if (v) { [falses,trues] } else { [trues,falses] }
          if (del.contains(flag)) {
            del.remove(flag)
            add.addElement(flag)
          }
        }
        /** Remove this flag from the set. */
        to leave() :void {
          trues.remove(flag)
          falses.remove(flag)
        }
      }
      falses.addElement(flag)
      return flag
    }
    /** Are all the flags true (none false)? */
    to allTrue() { return falses.size().isZero() }
    /** Are all the flags false (none true)? */
    to allFalse() { return trues.size().isZero() }
    /** Set all the flags to the same value. */
    to setAll(v :boolean) {
      def [del,add] := if (v) { [falses,trues] } else { [trues,falses] }
      add.addAll(del)
      del.removeAll(del)
    }
  }
}

def makeCheckpoint() {
  def [var continueSignal, var continueRes] := Ref.promise()
  def readies := makeFlagSet()

  /** Check whether all tasks have reached the checkpoint, and if so send the
    * signal and go to the next round. */
  def check() {
    if (readies.allTrue()) {
      readies.setAll(false)

      continueRes.resolve(null)    # send the continue signal

      def [p, r] := Ref.promise()  # prepare a new continue signal
      continueSignal := p
      continueRes := r
    }
  }

  return def checkpoint {
    to join() {
      def &flag := readies.join()
      return def membership {
        to leave() {
          (&flag).leave()
          check <- ()
        }
        to deliver() {
          flag := true
          check <- ()
          return continueSignal
        }
      }
    }
  }
}

def makeWorker(piece, checkpoint) {
  def stops := timer.now() + 3000 + entropy.nextInt(2000)
  var count := 0
  def checkpointMember := checkpoint <- join()
  def stopped
  def run() {
    # Pretend to do something lengthy; up to 1000 ms.
    timer.whenPast(timer.now() + entropy.nextInt(1000), fn {
      if (timer.now() >= stops) {
        checkpointMember <- leave()
        bind stopped := true
      } else {
        count += 1
        println(`Delivering $piece#$count`)
        when (checkpointMember <- deliver()) -> {
          println(`Delivered $piece#$count`)
          run()
        }
      }
    })
  }
  run()
  return stopped
}

def checkpoint := makeCheckpoint()
var waits := []
for piece in 1..5 {
  waits with= makeWorker(piece, checkpoint)
}
interp.waitAtTop(promiseAllFulfilled(waits))
```



## Erlang

A team of 5 workers assemble 3 items. The time it takes to assemble 1 item is 0 - 100 milliseconds.

```Erlang

-module( checkpoint_synchronization ).

-export( [task/0] ).

task() ->
      Pid = erlang:spawn( fun() -> checkpoint_loop([], []) end ),
      [erlang:spawn(fun() -> random:seed(X, 1, 0), worker_loop(X, 3, Pid) end) || X <- lists:seq(1, 5)],
      erlang:exit( Pid, normal ).



checkpoint_loop( Assemblings, Completes ) ->
        receive
        {starting, Worker} -> checkpoint_loop( [Worker | Assemblings], Completes );
        {done, Worker} ->
               New_assemblings = lists:delete( Worker, Assemblings ),
               New_completes = checkpoint_loop_release( New_assemblings, [Worker | Completes] ),
               checkpoint_loop( New_assemblings, New_completes )
        end.

checkpoint_loop_release( [], Completes ) ->
        [X ! all_complete || X <- Completes],
        [];
checkpoint_loop_release( _Assemblings, Completes ) -> Completes.

worker_loop( _Worker, 0, _Checkpoint ) -> ok;
worker_loop( Worker, N, Checkpoint ) ->
        Checkpoint ! {starting, erlang:self()},
        io:fwrite( "Worker ~p ~p~n", [Worker, N] ),
        timer:sleep( random:uniform(100) ),
        Checkpoint ! {done, erlang:self()},
        receive
        all_complete -> ok
        end,
        worker_loop( Worker, N - 1, Checkpoint ).

```

```txt

36> checkpoint_synchronization:task().
Worker 1 item 3
Worker 2 item 3
Worker 3 item 3
Worker 4 item 3
Worker 5 item 3
Worker 5 item 2
Worker 4 item 2
Worker 2 item 2
Worker 3 item 2
Worker 1 item 2
Worker 1 item 1
Worker 2 item 1
Worker 3 item 1
Worker 4 item 1
Worker 5 item 1

```



## Go

'''Solution 1, WaitGroup'''

The type sync.WaitGroup in the standard library implements a sort of checkpoint synchronization.  It allows one goroutine to wait for a number of other goroutines to indicate something, such as completing some work.

This first solution is a simple interpretation of the task, starting a goroutine (worker) for each part, letting the workers run concurrently, and waiting for them to all indicate completion.  This is efficient and idiomatic in Go.


```go
package main

import (
    "log"
    "math/rand"
    "sync"
    "time"
)

func worker(part string) {
    log.Println(part, "worker begins part")
    time.Sleep(time.Duration(rand.Int63n(1e6)))
    log.Println(part, "worker completes part")
    wg.Done()
}

var (
    partList    = []string{"A", "B", "C", "D"}
    nAssemblies = 3
    wg          sync.WaitGroup
)

func main() {
    rand.Seed(time.Now().UnixNano())
    for c := 1; c <= nAssemblies; c++ {
        log.Println("begin assembly cycle", c)
        wg.Add(len(partList))
        for _, part := range partList {
            go worker(part)
        }
        wg.Wait()
        log.Println("assemble.  cycle", c, "complete")
    }
}
```

Sample run, with race detector option to show no race conditions detected.

```txt

$ go run -race r1.go
2018/06/04 15:44:11 begin assembly cycle 1
2018/06/04 15:44:11 A worker begins part
2018/06/04 15:44:11 B worker begins part
2018/06/04 15:44:11 B worker completes part
2018/06/04 15:44:11 D worker begins part
2018/06/04 15:44:11 A worker completes part
2018/06/04 15:44:11 C worker begins part
2018/06/04 15:44:11 D worker completes part
2018/06/04 15:44:11 C worker completes part
2018/06/04 15:44:11 assemble.  cycle 1 complete
2018/06/04 15:44:11 begin assembly cycle 2
2018/06/04 15:44:11 A worker begins part
2018/06/04 15:44:11 B worker begins part
2018/06/04 15:44:11 A worker completes part
2018/06/04 15:44:11 C worker begins part
2018/06/04 15:44:11 D worker begins part
2018/06/04 15:44:11 C worker completes part
2018/06/04 15:44:11 B worker completes part
2018/06/04 15:44:11 D worker completes part
2018/06/04 15:44:11 assemble.  cycle 2 complete
2018/06/04 15:44:11 begin assembly cycle 3
2018/06/04 15:44:11 A worker begins part
2018/06/04 15:44:11 B worker begins part
2018/06/04 15:44:11 A worker completes part
2018/06/04 15:44:11 C worker begins part
2018/06/04 15:44:11 D worker begins part
2018/06/04 15:44:11 B worker completes part
2018/06/04 15:44:11 C worker completes part
2018/06/04 15:44:11 D worker completes part
2018/06/04 15:44:11 assemble.  cycle 3 complete
$

```


'''Solution 2, channels'''

Channels also synchronize, and in addition can send data.  The solution shown here is very similar to the WaitGroup solution above but sends data on a channel to simulate a completed part.  The channel operations provide synchronization and a WaitGroup is not needed.


```go
package main

import (
    "log"
    "math/rand"
    "strings"
    "time"
)

func worker(part string, completed chan string) {
    log.Println(part, "worker begins part")
    time.Sleep(time.Duration(rand.Int63n(1e6)))
    p := strings.ToLower(part)
    log.Println(part, "worker completed", p)
    completed <- p
}

var (
    partList    = []string{"A", "B", "C", "D"}
    nAssemblies = 3
)

func main() {
    rand.Seed(time.Now().UnixNano())
    completed := make([]chan string, len(partList))
    for i := range completed {
        completed[i] = make(chan string)
    }
    for c := 1; c <= nAssemblies; c++ {
        log.Println("begin assembly cycle", c)
        for i, part := range partList {
            go worker(part, completed[i])
        }
        a := ""
        for _, c := range completed {
            a += <-c
        }
        log.Println(a, "assembled.  cycle", c, "complete")
    }
}
```

```txt

$ go run -race r2.go
2018/06/04 15:56:33 begin assembly cycle 1
2018/06/04 15:56:33 A worker begins part
2018/06/04 15:56:33 B worker begins part
2018/06/04 15:56:33 A worker completed a
2018/06/04 15:56:33 D worker begins part
2018/06/04 15:56:33 C worker begins part
2018/06/04 15:56:33 B worker completed b
2018/06/04 15:56:33 C worker completed c
2018/06/04 15:56:33 D worker completed d
2018/06/04 15:56:33 abcd assembled.  cycle 1 complete
2018/06/04 15:56:33 begin assembly cycle 2
2018/06/04 15:56:33 A worker begins part
2018/06/04 15:56:33 B worker begins part
2018/06/04 15:56:33 C worker begins part
2018/06/04 15:56:33 D worker begins part
2018/06/04 15:56:33 A worker completed a
2018/06/04 15:56:33 B worker completed b
2018/06/04 15:56:33 D worker completed d
2018/06/04 15:56:33 C worker completed c
2018/06/04 15:56:33 abcd assembled.  cycle 2 complete
2018/06/04 15:56:33 begin assembly cycle 3
2018/06/04 15:56:33 A worker begins part
2018/06/04 15:56:33 B worker begins part
2018/06/04 15:56:33 C worker begins part
2018/06/04 15:56:33 D worker begins part
2018/06/04 15:56:33 B worker completed b
2018/06/04 15:56:33 A worker completed a
2018/06/04 15:56:33 D worker completed d
2018/06/04 15:56:33 C worker completed c
2018/06/04 15:56:33 abcd assembled.  cycle 3 complete
$

```


'''Solution 3, two-phase barrier'''

For those that might object to the way the two solutions above start new goroutines in each cycle, here is a technique sometimes called a two-phase barrier, where goroutines loop until being shutdown.  In each loop there are two phases, one of making the part, and one of waiting for the completed parts to be assembled.  This more literally satisfies the task but in fact is not idiomatic Go.  Goroutines are cheap to start up and shut down in Go and the extra complexity of this two-phase barrier technique is
not justified.


```go
package main

import (
    "log"
    "math/rand"
    "strings"
    "sync"
    "time"
)

func worker(part string, completed chan string) {
    log.Println(part, "worker running")
    for {
        select {
        case <-start:
            log.Println(part, "worker begins part")
            time.Sleep(time.Duration(rand.Int63n(1e6)))
            p := strings.ToLower(part)
            log.Println(part, "worker completed", p)
            completed <- p
            <-reset
            wg.Done()
        case <-done:
            log.Println(part, "worker stopped")
            wg.Done()
            return
        }
    }
}

var (
    partList    = []string{"A", "B", "C", "D"}
    nAssemblies = 3
    start       = make(chan int)
    done        = make(chan int)
    reset       chan int
    wg          sync.WaitGroup
)

func main() {
    rand.Seed(time.Now().UnixNano())
    completed := make([]chan string, len(partList))
    for i, part := range partList {
        completed[i] = make(chan string)
        go worker(part, completed[i])
    }
    for c := 1; c <= nAssemblies; c++ {
        log.Println("begin assembly cycle", c)
        reset = make(chan int)
        close(start)
        a := ""
        for _, c := range completed {
            a += <-c
        }
        log.Println(a, "assembled.  cycle", c, "complete")
        wg.Add(len(partList))
        start = make(chan int)
        close(reset)
        wg.Wait()
    }
    wg.Add(len(partList))
    close(done)
    wg.Wait()
}
```

```txt

$ go run -race r3.go
2018/06/04 16:11:54 A worker running
2018/06/04 16:11:54 B worker running
2018/06/04 16:11:54 C worker running
2018/06/04 16:11:54 begin assembly cycle 1
2018/06/04 16:11:54 A worker begins part
2018/06/04 16:11:54 D worker running
2018/06/04 16:11:54 C worker begins part
2018/06/04 16:11:54 B worker begins part
2018/06/04 16:11:54 D worker begins part
2018/06/04 16:11:54 A worker completed a
2018/06/04 16:11:54 C worker completed c
2018/06/04 16:11:54 D worker completed d
2018/06/04 16:11:54 B worker completed b
2018/06/04 16:11:54 abcd assembled.  cycle 1 complete
2018/06/04 16:11:54 begin assembly cycle 2
2018/06/04 16:11:54 C worker begins part
2018/06/04 16:11:54 D worker begins part
2018/06/04 16:11:54 B worker begins part
2018/06/04 16:11:54 A worker begins part
2018/06/04 16:11:54 D worker completed d
2018/06/04 16:11:54 A worker completed a
2018/06/04 16:11:54 B worker completed b
2018/06/04 16:11:54 C worker completed c
2018/06/04 16:11:54 abcd assembled.  cycle 2 complete
2018/06/04 16:11:54 begin assembly cycle 3
2018/06/04 16:11:54 A worker begins part
2018/06/04 16:11:54 D worker begins part
2018/06/04 16:11:54 C worker begins part
2018/06/04 16:11:54 B worker begins part
2018/06/04 16:11:54 D worker completed d
2018/06/04 16:11:54 A worker completed a
2018/06/04 16:11:54 B worker completed b
2018/06/04 16:11:54 C worker completed c
2018/06/04 16:11:54 abcd assembled.  cycle 3 complete
2018/06/04 16:11:54 D worker stopped
2018/06/04 16:11:54 B worker stopped
2018/06/04 16:11:54 C worker stopped
2018/06/04 16:11:54 A worker stopped

```


'''Solution 4, workers joining and leaving'''

This solution shows workers joining and leaving, although it is a rather different interpretation of the task.

```go
package main

import (
    "log"
    "math/rand"
    "os"
    "sync"
    "time"
)

const nMech = 5
const detailsPerMech = 4

var l = log.New(os.Stdout, "", 0)

func main() {
    assemble := make(chan int)
    var complete sync.WaitGroup

    go solicit(assemble, &complete, nMech*detailsPerMech)

    for i := 1; i <= nMech; i++ {
        complete.Add(detailsPerMech)
        for j := 0; j < detailsPerMech; j++ {
            assemble <- 0
        }
        // Go checkpoint feature
        complete.Wait()
        // checkpoint reached
        l.Println("mechanism", i, "completed")
    }
}

func solicit(a chan int, c *sync.WaitGroup, nDetails int) {
    rand.Seed(time.Now().UnixNano())
    var id int // worker id, for output
    for nDetails > 0 {
        // some random time to find a worker
        time.Sleep(time.Duration(5e8 + rand.Int63n(5e8)))
        id++
        // contract to assemble a certain number of details
        contract := rand.Intn(5) + 1
        if contract > nDetails {
            contract = nDetails
        }
        dword := "details"
        if contract == 1 {
            dword = "detail"
        }
        l.Println("worker", id, "contracted to assemble", contract, dword)
        go worker(a, c, contract, id)
        nDetails -= contract
    }
}

func worker(a chan int, c *sync.WaitGroup, contract, id int) {
    // some random time it takes for this worker to assemble a detail
    assemblyTime := time.Duration(5e8 + rand.Int63n(5e8))
    l.Println("worker", id, "enters shop")
    for i := 0; i < contract; i++ {
        <-a
        l.Println("worker", id, "assembling")
        time.Sleep(assemblyTime)
        l.Println("worker", id, "completed detail")
        c.Done()
    }
    l.Println("worker", id, "leaves shop")
}
```

Output:

```txt
worker 1 contracted to assemble 2 details
worker 1 enters shop
worker 1 assembling
worker 2 contracted to assemble 5 details
worker 2 enters shop
worker 2 assembling
worker 1 completed detail
worker 1 assembling
worker 2 completed detail
worker 2 assembling
worker 3 contracted to assemble 1 detail
worker 3 enters shop
worker 1 completed detail
worker 1 leaves shop
worker 2 completed detail
mechanism 1 completed
worker 3 assembling
worker 2 assembling

...

worker 5 completed detail
worker 7 completed detail
worker 7 leaves shop
mechanism 4 completed
worker 6 assembling
worker 5 assembling
worker 6 completed detail
worker 6 assembling
worker 5 completed detail
worker 5 leaves shop
worker 6 completed detail
worker 6 assembling
worker 6 completed detail
worker 6 leaves shop
mechanism 5 completed
```



## Haskell

<p>Although not being sure if the approach might be right, this example shows several workers performing a series of tasks simultaneously and synchronizing themselves before starting the next task.</p>
<p>Each worker has several tasks in order to complete his work. As an example, they have to calculate big sums. A worker can be idle during one of the tasks. The tasks are arranged so that we get a list of the first task of all workers, then a list of the second task of all workers, and so on. Idle workers are skipped. The tasks are taken out of the Task data type and returned as plain values. The notion of a worker vanishes here. The definition of the worker's tasks allows us to keep track of what each worker actually performs.</p>
<p>The function "runTasks" gets one of those groups of tasks and executes them in parallel and gathers the result of each task. But this function doesn't return until all tasks are finished. This function doesn't know what each worker is doing.</p>
<p>Once the first group of tasks is finished, a function is applied to the results (in the example, the results are simply added together).</p>
<p>Then the second group of tasks is passed to "runTasks", and so on.</p>
<p>The workers can have any number of tasks, and they can contain idle phases. That way, a worker doesn't need to join at the first task, and may skip tasks. The function "groupTasks" takes care of idle states.</p>
<p>Caveats:</p>
<ul>
<li>A group of tasks must return values of the same type.</li>
<li>If each group of tasks should return values a different type, you have to define groups of workers for each different task instead of defining workers with several tasks each.</li>
<li>More flexibility can be achieved with the use of custom data types.</li>
<li>Due to the use of parallel computation, only pure functions (without side effects) can be executed. Moreover, only a few Haskell compilers (GHC among them) support parallel computation. The program must be compiled with the -threaded and -rtsopts options enabled and run with the +RTS -N commandline option for computations to be actually performed in parallel.</li>
<li>For effectful computations, you should use concurrent threads (forkIO and MVar from the module Control.Concurrent), software transactional memory (STM) or alternatives provided by other modules.</li>
</ul>

```Haskell
import Control.Parallel

data Task a = Idle | Make a
type TaskList a = [a]
type Results a = [a]
type TaskGroups a = [TaskList a]
type WorkerList a = [Worker a]
type Worker a = [Task a]

-- run tasks in parallel and collect their results
-- the function doesn't return until all tasks are done, therefore
-- finished threads wait for the others to finish.
runTasks :: TaskList a -> Results a
runTasks [] = []
runTasks (x:[]) = x : []
runTasks (x:y:[]) = y `par` x : y : []
runTasks (x:y:ys) = y `par` x : y : runTasks ys

-- take a list of workers with different numbers of tasks and group
-- them: first the first task of each worker, then the second one etc.
groupTasks :: WorkerList a -> TaskGroups a
groupTasks [] = []
groupTasks xs
    | allWorkersIdle xs = []
    | otherwise =
        concatMap extractTask xs : groupTasks (map removeTask xs)

-- return a task as a plain value
extractTask :: Worker a -> [a]
extractTask [] = []
extractTask (Idle:_) = []
extractTask (Make a:_) = [a]

-- remove the foremost task of each worker
removeTask :: Worker a -> Worker a
removeTask = drop 1

-- checks whether all workers are idle in this task
allWorkersIdle :: WorkerList a -> Bool
allWorkersIdle = all null . map extractTask

-- the workers must calculate big sums. the first sum of each worker
-- belongs to the first task, and so on.
-- because of laziness, nothing is computed yet.

-- worker1 has 5 tasks to do
worker1 :: Worker Integer
worker1 = map Make [ sum [1..n*1000000] | n <- [1..5] ]

-- worker2 has 4 tasks to do
worker2 :: Worker Integer
worker2 = map Make [ sum [1..n*100000] | n <- [1..4] ]

-- worker3 has 3 tasks to do
worker3 :: Worker Integer
worker3 = map Make [ sum [1..n*1000000] | n <- [1..3] ]

-- worker4 has 5 tasks to do
worker4 :: Worker Integer
worker4 = map Make [ sum [1..n*300000] | n <- [1..5] ]

-- worker5 has 4 tasks to do, but starts at the second task.
worker5 :: Worker Integer
worker5 = [Idle] ++ map Make [ sum [1..n*400000] | n <- [1..4] ]

-- group the workers' tasks
tasks :: TaskGroups Integer
tasks = groupTasks [worker1, worker2, worker3, worker4, worker5]

-- a workshop: take a function to operate the results and a group of tasks,
-- execute the tasks showing the process and process the results
workshop :: (Show a, Num a, Show b, Num b) => ([a] -> b) -> [[a]] -> IO ()
workshop func a = mapM_ doWork $ zip [1..length a] a
    where
        doWork (x, y) = do
            putStrLn $ "Doing task " ++ show x ++ "."
            putStrLn $ "There are " ++ show (length y) ++ " workers for this task."
            putStrLn "Waiting for all workers..."
            print $ func $ runTasks y
            putStrLn $ "Task " ++ show x ++ " done."

main = workshop sum tasks

```

<p>The following version works with the concurrency model provided by the module Control.Concurrent</p>
<p>A workshop is an MVar that holds three values: the number of workers doing something, the number of workers ready for the next task and the total number of workers at the moment.</p>
<p>A worker takes a list of actions. Before executing the actions, he joins the workshop and the total number of workers is increased. Then, he reports that he has started an action and the number of active workers is increased. Next, the worker carries out an action. After that, he reports that he is ready for the next action. The number of active and ready workers is updated. Then he enters the check point loop, where he stays until all other workers have reported being ready. Then he goes into active state again and executes the next action, and so on. After the last action, he leaves the workshop and the total number of workers is decreased.</p>
<p>The checkPoint function keeps reading the values of the MVar and looping until there are 0 or less active workers and the number of ready workers is equal to the total number of workers. At this point, the MVar is reset. In order to avoid race conditions, if there are zero active and ready workers, the function returns immediately as to allow the worker to start an action.</p>
<p>The example code prints some useful messages to the screen, such as when a check point is reached and what each worker is currently doing (it also shows his thread ID).</p>
<p>The "shop" function forks the worker threads and returns their ID's, so the threads can be killed easily from the "main" function when the user hits a key.</p>
<p>The "main" function launches three workers first, and after 5 seconds it launches two workers more. It then waits for a key press and kills all threads, if they're still active.</p>
<p>Other than the parallel version above, this code runs in the IO Monad and makes it possible to perform IO actions such as accessing the hardware. However, all actions must have the return type IO (). If the workers must return some useful values, the MVar should be extended with the necessary fields and the workers should use those fields to store the results they produce.</p>
<p>Note: This code has been tested on GHC 7.6.1 and will most probably not run under other Haskell implementations due to the use of some functions from the module Control.Concurrent. It won't work if compiled with the -O2 compiler switch. Compile with the -threaded compiler switch if you want to run the threads in parallel.</p>

```Haskell
import Control.Concurrent
import Control.Monad        -- needed for "forM", "forM_"

-- (workers working, workers done, workers total)
type Workshop = MVar (Int, Int, Int)
-- list of IO actions to be performed by one worker
type Actions = [IO ()]

newWorkshop :: IO Workshop
newWorkshop = newMVar (0, 0, 0)

-- check point: workers wait here for the other workers to
-- finish, before resuming execution/restarting
checkPoint :: Workshop -> IO ()
checkPoint w = do
    (working, done, count) <- takeMVar w
    -- all workers are done: reset counters and return (threads
    -- resume execution or restart)
    if working <= 0 && done == count
    then do
            putStrLn "---- Check Point"
            putMVar w (0, 0, count)
    -- mvar was just initialized: do nothing, just return.
    -- otherwise, a race condition may arise
    else if working == 0 && done == 0
            then putMVar w (working, done, count)
    -- workers are still working: wait for them (loop)
    else do
            putMVar w (working, done, count)
            checkPoint w

-- join the workshop
addWorker :: Workshop -> ThreadId -> IO ()
addWorker w i = do
    (working, done, count) <- takeMVar w
    putStrLn $ "Worker " ++ show i ++ " has joined the group."
    putMVar w (working, done, count + 1)

-- leave the workshop
removeWorker :: Workshop -> ThreadId -> IO ()
removeWorker w i = do
    (working, done, count) <- takeMVar w
    putStrLn $ "Worker " ++ show i ++ " has left the group."
    putMVar w (working, done, count - 1)

-- increase the number of workers doing something.
-- optionally, print a message using the thread's ID
startWork :: Workshop -> ThreadId -> IO ()
startWork w i = do
    (working, done, count) <- takeMVar w
    putStrLn $ "Worker " ++ show i ++ " has started."
    putMVar w (working + 1, done, count)

-- decrease the number of workers doing something and increase the
-- number of workers done. optionally, print a message using
-- the thread's ID
finishWork :: Workshop -> ThreadId -> IO ()
finishWork w i = do
    (working, done, count) <- takeMVar w
    putStrLn $ "Worker " ++ show i ++ " is ready."
    putMVar w (working - 1, done + 1, count)

-- put a worker to do his tasks. the steps are:
-- 1. join the workshop "w"
-- 2. report that the worker has started an action
-- 3. perform one action
-- 4. report that the worker is ready for the next action
-- 5. wait for the other workers to finish
-- 6. repeat from 2 until the worker has nothing more to do
-- 7. leave the workshop
worker :: Workshop -> Actions -> IO ()
worker w actions = do
    i <- myThreadId
    addWorker w i
    forM_ actions $ \action -> do
        startWork w i
        action
        finishWork w i
        checkPoint w
    removeWorker w i

-- launch several worker threads. their thread ID's are returned
shop :: Workshop -> [Actions] -> IO [ThreadId]
shop w actions = do
    forM actions $ \x -> forkIO (worker w x)

main = do
    -- make a workshop
    w <- newWorkshop

    -- the workers won't be doing anything special, just wait for n
    -- regular intervals. pids gathers the ID's of the threads

    -- this are the first workers joining the workshop
    pids1 <- shop w
        [replicate 5 $ threadDelay 1300000
        ,replicate 10 $ threadDelay 759191
        ,replicate 7 $ threadDelay 965300]

    -- wait for 5 secs before the next workers join
    threadDelay 5000000

    -- these are other workers that join the workshop later
    pids2 <- shop w
        [replicate 6 $ threadDelay 380000
        ,replicate 4 $ threadDelay 250000]

    -- wait for a key press
    getChar

    -- kill all worker threads before exit, if they're still running
    forM_ (pids1 ++ pids2) killThread
```

'''Output:'''
<pre style="height: 200px;overflow:scroll">
main +RTS -N2

Worker ThreadId 30 has joined the group.
Worker ThreadId 31 has joined the group.
Worker ThreadId 32 has joined the group.
Worker ThreadId 30 has started.
Worker ThreadId 31 has started.
Worker ThreadId 32 has started.
Worker ThreadId 31 is ready.
Worker ThreadId 32 is ready.
Worker ThreadId 30 is ready.
---- Check Point
Worker ThreadId 32 has started.
Worker ThreadId 31 has started.
Worker ThreadId 30 has started.
Worker ThreadId 31 is ready.
Worker ThreadId 32 is ready.
Worker ThreadId 30 is ready.
---- Check Point
Worker ThreadId 32 has started.
Worker ThreadId 31 has started.
Worker ThreadId 30 has started.
Worker ThreadId 31 is ready.
Worker ThreadId 32 is ready.
Worker ThreadId 30 is ready.
---- Check Point
Worker ThreadId 32 has started.
Worker ThreadId 31 has started.
Worker ThreadId 30 has started.
Worker ThreadId 31 is ready.
Worker ThreadId 32 is ready.
Worker ThreadId 33 has joined the group.
Worker ThreadId 34 has joined the group.
Worker ThreadId 33 has started.
Worker ThreadId 34 has started.
Worker ThreadId 30 is ready.
Worker ThreadId 34 is ready.
Worker ThreadId 33 is ready.
---- Check Point
Worker ThreadId 32 has started.
Worker ThreadId 34 has started.
Worker ThreadId 31 has started.
Worker ThreadId 30 has started.
Worker ThreadId 33 has started.
Worker ThreadId 34 is ready.
Worker ThreadId 33 is ready.
Worker ThreadId 31 is ready.
Worker ThreadId 32 is ready.
Worker ThreadId 30 is ready.
---- Check Point
Worker ThreadId 31 has started.
Worker ThreadId 32 has started.
Worker ThreadId 34 has started.
Worker ThreadId 33 has started.
Worker ThreadId 30 has left the group.
Worker ThreadId 34 is ready.
Worker ThreadId 33 is ready.
Worker ThreadId 31 is ready.
Worker ThreadId 32 is ready.
---- Check Point
Worker ThreadId 34 has started.
Worker ThreadId 33 has started.
Worker ThreadId 31 has started.
Worker ThreadId 32 has started.
Worker ThreadId 34 is ready.
Worker ThreadId 33 is ready.
Worker ThreadId 31 is ready.
Worker ThreadId 32 is ready.
---- Check Point
Worker ThreadId 31 has started.
Worker ThreadId 33 has started.
Worker ThreadId 34 has left the group.
Worker ThreadId 32 has left the group.
Worker ThreadId 33 is ready.
Worker ThreadId 31 is ready.
---- Check Point
Worker ThreadId 33 has started.
Worker ThreadId 31 has started.
Worker ThreadId 33 is ready.
Worker ThreadId 31 is ready.
---- Check Point
Worker ThreadId 33 has left the group.
Worker ThreadId 31 has started.
Worker ThreadId 31 is ready.
---- Check Point
Worker ThreadId 31 has left the group.

```


==Icon and {{header|Unicon}}==

The following only works in Unicon:


```unicon
global nWorkers, workers, cv

procedure main(A)
    nWorkers := integer(A[1]) | 3
    cv  := condvar()
    every put(workers := [], worker(!nWorkers))
    every wait(!workers)
end

procedure worker(n)
    return thread every !3 do {       # Union limits each worker to 3 pieces
        write(n," is working")
        delay(?3 * 1000)
        write(n," is done")
        countdown()
        }
end

procedure countdown()
    critical cv: {
        if (nWorkers -:= 1) <= 0 then {
            write("\t\tAll done")
            nWorkers := *workers
            return (unlock(cv),signal(cv, 0))
            }
        wait(cv)
        }
end
```


Sample run:

```txt

->cps
1 is working
2 is working
3 is working
3 is done
1 is done
2 is done
		All done
2 is working
3 is working
1 is working
1 is done
3 is done
2 is done
		All done
2 is working
1 is working
3 is working
2 is done
1 is done
3 is done
		All done
->

```



## J


The current implementations of J are all single threaded.  However, the language definition offers a lot of parallelism which I imagine will eventually be supported, after performance gains significantly better than a factor of 2 on common problems become economically viable.

For example in 1 2 3 + 4 5 6, we have three addition operations which are specified to be carried out in parallel, and this kind of parallelism pervades the language definition.


## Java


```Java
import java.util.Scanner;
import java.util.Random;

public class CheckpointSync{
	public static void main(String[] args){
		System.out.print("Enter number of workers to use: ");
		Scanner in = new Scanner(System.in);
		Worker.nWorkers = in.nextInt();
		System.out.print("Enter number of tasks to complete:");
		runTasks(in.nextInt());
	}

	/*
	 * Informs that workers started working on the task and
	 * starts running threads. Prior to proceeding with next
	 * task syncs using static Worker.checkpoint() method.
	 */
	private static void runTasks(int nTasks){
		for(int i = 0; i < nTasks; i++){
			System.out.println("Starting task number " + (i+1) + ".");
			runThreads();
			Worker.checkpoint();
		}
	}

	/*
	 * Creates a thread for each worker and runs it.
	 */
	private static void runThreads(){
		for(int i = 0; i < Worker.nWorkers; i ++){
			new Thread(new Worker(i+1)).start();
		}
	}

	/*
	 * Worker inner static class.
	 */
	public static class Worker implements Runnable{
		public Worker(int threadID){
			this.threadID = threadID;
		}
		public void run(){
			work();
		}

		/*
		 *  Notifies that thread started running for 100 to 1000 msec.
		 *  Once finished increments static counter 'nFinished'
		 *  that counts number of workers finished their work.
		 */
		private synchronized void work(){
			try {
				int workTime = rgen.nextInt(900) + 100;
				System.out.println("Worker " + threadID + " will work for " + workTime + " msec.");
				Thread.sleep(workTime); //work for 'workTime'
				nFinished++; //increases work finished counter
				System.out.println("Worker " + threadID + " is ready");
			} catch (InterruptedException e) {
				System.err.println("Error: thread execution interrupted");
				e.printStackTrace();
			}
		}

		/*
		 * Used to synchronize Worker threads using 'nFinished' static integer.
		 * Waits (with step of 10 msec) until 'nFinished' equals to 'nWorkers'.
		 * Once they are equal resets 'nFinished' counter.
		 */
		public static synchronized void checkpoint(){
			while(nFinished != nWorkers){
				try {
					Thread.sleep(10);
				} catch (InterruptedException e) {
					System.err.println("Error: thread execution interrupted");
					e.printStackTrace();
				}
			}
			nFinished = 0;
		}

		/* inner class instance variables */
		private int threadID;

		/* static variables */
		private static Random rgen = new Random();
		private static int nFinished = 0;
		public static int nWorkers = 0;
	}
}
```

Output:
<pre style="height: 200px;overflow:scroll">
Enter number of workers to use: 5
Enter number of tasks to complete:3
Starting task number 1.
Worker 1 will work for 882 msec.
Worker 2 will work for 330 msec.
Worker 3 will work for 618 msec.
Worker 4 will work for 949 msec.
Worker 5 will work for 805 msec.
Worker 2 is ready
Worker 3 is ready
Worker 5 is ready
Worker 1 is ready
Worker 4 is ready
Starting task number 2.
Worker 1 will work for 942 msec.
Worker 2 will work for 247 msec.
Worker 3 will work for 545 msec.
Worker 4 will work for 850 msec.
Worker 5 will work for 888 msec.
Worker 2 is ready
Worker 3 is ready
Worker 4 is ready
Worker 5 is ready
Worker 1 is ready
Starting task number 3.
Worker 2 will work for 976 msec.
Worker 1 will work for 194 msec.
Worker 4 will work for 532 msec.
Worker 3 will work for 515 msec.
Worker 5 will work for 326 msec.
Worker 1 is ready
Worker 5 is ready
Worker 3 is ready
Worker 4 is ready
Worker 2 is ready

```

```java5
import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class Sync {
	static class Worker implements Runnable {
		private final CountDownLatch doneSignal;
		private int threadID;

		public Worker(int id, CountDownLatch doneSignal) {
			this.doneSignal = doneSignal;
			threadID = id;
		}

		public void run() {
			doWork();
			doneSignal.countDown();
		}

		void doWork() {
			try {
				int workTime = new Random().nextInt(900) + 100;
				System.out.println("Worker " + threadID + " will work for " + workTime + " msec.");
				Thread.sleep(workTime); //work for 'workTime'
				System.out.println("Worker " + threadID + " is ready");
			} catch (InterruptedException e) {
				System.err.println("Error: thread execution interrupted");
				e.printStackTrace();
			}
		}
	}

	public static void main(String[] args) {
		int n = 3;//6 workers and 3 tasks
		for(int task = 1; task <= n; task++) {
			CountDownLatch latch = new CountDownLatch(n * 2);
			System.out.println("Starting task " + task);
			for(int worker = 0; worker < n * 2; worker++) {
				new Thread(new Worker(worker, latch)).start();
			}
			try {
				latch.await();//wait for n*2 threads to signal the latch
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			System.out.println("Task " + task + " complete");
		}
	}
}
```

Output:
<pre style="height: 200px;overflow:scroll">Starting task 1
Worker 0 will work for 959 msec.
Worker 1 will work for 905 msec.
Worker 3 will work for 622 msec.
Worker 2 will work for 969 msec.
Worker 4 will work for 577 msec.
Worker 5 will work for 727 msec.
Worker 4 is ready
Worker 3 is ready
Worker 5 is ready
Worker 1 is ready
Worker 0 is ready
Worker 2 is ready
Task 1 complete
Starting task 2
Worker 0 will work for 305 msec.
Worker 2 will work for 541 msec.
Worker 4 will work for 663 msec.
Worker 1 will work for 883 msec.
Worker 3 will work for 324 msec.
Worker 5 will work for 459 msec.
Worker 0 is ready
Worker 3 is ready
Worker 5 is ready
Worker 2 is ready
Worker 4 is ready
Worker 1 is ready
Task 2 complete
Starting task 3
Worker 0 will work for 554 msec.
Worker 2 will work for 727 msec.
Worker 1 will work for 203 msec.
Worker 4 will work for 249 msec.
Worker 3 will work for 612 msec.
Worker 5 will work for 723 msec.
Worker 1 is ready
Worker 4 is ready
Worker 0 is ready
Worker 3 is ready
Worker 5 is ready
Worker 2 is ready
Task 3 complete
```



## Julia

Julia has specific macros for checkpoint type synchronization. @async starts an asynchronous task, and multiple @async tasks can be synchronized by wrapping them within the @sync macro statement, which creates a checkpoint for all @async tasks.

```julia

function runsim(numworkers, runs)
    for count in 1:runs
        @sync begin
            for worker in 1:numworkers
                @async begin
                    tasktime = rand()
                    sleep(tasktime)
                    println("Worker $worker finished after $tasktime seconds")
                end
            end
        end
        println("Checkpoint reached for run $count.")
    end
    println("Finished all runs.\n")
end

const trials = [[3, 2], [4, 1], [2, 5], [7, 6]]
for trial in trials
    runsim(trial[1], trial[2])
end
```

```txt

Worker 1 finished after 0.2496063425219046 seconds
Worker 3 finished after 0.6437560525692665 seconds
Worker 2 finished after 0.7622150880806831 seconds
Checkpoint reached for run 1.
Worker 2 finished after 0.0745173155757679 seconds
Worker 3 finished after 0.39089824936640993 seconds
Worker 1 finished after 0.5397505221156416 seconds
Checkpoint reached for run 2.
Finished all runs.

Worker 4 finished after 0.26840044205839897 seconds
Worker 3 finished after 0.5589553147289623 seconds
Worker 2 finished after 0.8546852411700241 seconds
Worker 1 finished after 0.9300832572304523 seconds
Checkpoint reached for run 1.
Finished all runs.

Worker 1 finished after 0.5289138841087624 seconds
Worker 2 finished after 0.7356027970934949 seconds
Checkpoint reached for run 1.
Worker 1 finished after 0.20674100912304416 seconds
Worker 2 finished after 0.6998567540438869 seconds
Checkpoint reached for run 2.
Worker 1 finished after 0.11392579333661912 seconds
Worker 2 finished after 0.4949249386371388 seconds
Checkpoint reached for run 3.
Worker 1 finished after 0.6032150410794788 seconds
Worker 2 finished after 0.8986919181800306 seconds
Checkpoint reached for run 4.
Worker 1 finished after 0.4237385941703915 seconds
Worker 2 finished after 0.5574922259408035 seconds
Checkpoint reached for run 5.
Finished all runs.

Worker 7 finished after 0.0396918164082527 seconds
Worker 3 finished after 0.31472648034105966 seconds
Worker 6 finished after 0.32606467253051474 seconds
Worker 5 finished after 0.3690388125862416 seconds
Worker 1 finished after 0.4290499974502766 seconds
Worker 2 finished after 0.48606373107736744 seconds
Worker 4 finished after 0.8723256915201081 seconds
Checkpoint reached for run 1.
Worker 2 finished after 0.10418765463492563 seconds
Worker 3 finished after 0.14023815791725713 seconds
Worker 7 finished after 0.7850239937628409 seconds
Worker 4 finished after 0.8145187186029617 seconds
Worker 6 finished after 0.8446820477646959 seconds
Worker 1 finished after 0.9195642711183825 seconds
Worker 5 finished after 0.9517129615316944 seconds
Checkpoint reached for run 2.
Worker 7 finished after 0.28490757307993486 seconds
Worker 3 finished after 0.4199539978001552 seconds
Worker 2 finished after 0.5509998796559186 seconds
Worker 5 finished after 0.7840588445793306 seconds
Worker 1 finished after 0.8049513381813924 seconds
Worker 6 finished after 0.8848651563027041 seconds
Worker 4 finished after 0.9074862779348334 seconds
Checkpoint reached for run 3.
Worker 5 finished after 0.21855944993484533 seconds
Worker 2 finished after 0.27709606350565275 seconds
Worker 7 finished after 0.28450943951411123 seconds
Worker 4 finished after 0.40871929967426857 seconds
Worker 1 finished after 0.5506243033572837 seconds
Worker 3 finished after 0.9287035426710006 seconds
Worker 6 finished after 0.9624436931735709 seconds
Checkpoint reached for run 4.
Worker 5 finished after 0.04032963358782826 seconds
Worker 6 finished after 0.17464708712852195 seconds
Worker 4 finished after 0.19558842246553398 seconds
Worker 3 finished after 0.2113199231977796 seconds
Worker 7 finished after 0.423009958033447 seconds
Worker 1 finished after 0.7584848109224733 seconds
Worker 2 finished after 0.8116269421151843 seconds
Checkpoint reached for run 5.
Worker 6 finished after 0.12563630313371443 seconds
Worker 4 finished after 0.33588040252159823 seconds
Worker 1 finished after 0.44873857982831256 seconds
Worker 5 finished after 0.536029356963061 seconds
Worker 3 finished after 0.5687590862891123 seconds
Worker 2 finished after 0.6655311849010326 seconds
Worker 7 finished after 0.8454083062748163 seconds
Checkpoint reached for run 6.
Finished all runs.

```




## Kotlin

```scala
// Version 1.2.41

import java.util.Random

val rgen = Random()
var nWorkers = 0
var nTasks = 0

class Worker(private val threadID: Int) : Runnable {

    @Synchronized
    override fun run() {
        try {
            val workTime = rgen.nextInt(900) + 100L  // 100..999 msec.
            println("Worker $threadID will work for $workTime msec.")
            Thread.sleep(workTime)
            nFinished++
            println("Worker $threadID is ready")
        }
        catch (e: InterruptedException) {
            println("Error: thread execution interrupted")
            e.printStackTrace()
        }
    }

    companion object {
        private var nFinished = 0

        @Synchronized
        fun checkPoint() {
            while (nFinished != nWorkers) {
                try {
                    Thread.sleep(10)
                }
                catch (e: InterruptedException) {
                    println("Error: thread execution interrupted")
                    e.printStackTrace()
                }
            }
            nFinished = 0  // reset
        }
    }
}

fun runTasks() {
    for (i in 1..nTasks) {
        println("\nStarting task number $i.")
        // Create a thread for each worker and run it.
        for (j in 1..nWorkers) Thread(Worker(j)).start()
        Worker.checkPoint()  // wait for all workers to finish the task
    }
}

fun main(args: Array<String>) {
    print("Enter number of workers to use: ")
    nWorkers = readLine()!!.toInt()
    print("Enter number of tasks to complete: ")
    nTasks = readLine()!!.toInt()
    runTasks()
}
```


Sample session:
<pre style="height: 200px;overflow:scroll">
Enter number of workers to use: 5
Enter number of tasks to complete: 3

Starting task number 1.
Worker 1 will work for 894 msec.
Worker 3 will work for 777 msec.
Worker 2 will work for 243 msec.
Worker 4 will work for 938 msec.
Worker 5 will work for 551 msec.
Worker 2 is ready
Worker 5 is ready
Worker 3 is ready
Worker 1 is ready
Worker 4 is ready

Starting task number 2.
Worker 2 will work for 952 msec.
Worker 3 will work for 253 msec.
Worker 1 will work for 165 msec.
Worker 4 will work for 995 msec.
Worker 5 will work for 499 msec.
Worker 1 is ready
Worker 3 is ready
Worker 5 is ready
Worker 2 is ready
Worker 4 is ready

Starting task number 3.
Worker 1 will work for 622 msec.
Worker 2 will work for 642 msec.
Worker 4 will work for 344 msec.
Worker 3 will work for 191 msec.
Worker 5 will work for 703 msec.
Worker 3 is ready
Worker 4 is ready
Worker 1 is ready
Worker 2 is ready
Worker 5 is ready

```



## Logtalk

The following example can be found in the Logtalk distribution and is used here with permission. It's based on the Erlang solution for this task. Works when using SWI-Prolog, XSB, or YAP as the backend compiler.

```logtalk

:- object(checkpoint).

    :- threaded.

    :- public(run/3).
    :- mode(run(+integer,+integer,+float), one).
    :- info(run/3, [
        comment is 'Assemble items using a team of workers with a maximum time per item assembly.',
        arguments is ['Workers'-'Number of workers', 'Items'-'Number of items to assemble', 'Time'-'Maximum time in seconds to assemble one item']
    ]).

    :- public(run/0).
    :- mode(run, one).
    :- info(run/0, [
        comment is 'Assemble three items using a team of five workers with a maximum of 0.1 seconds per item assembly.'
    ]).

    :- uses(integer, [between/3]).
    :- uses(random,  [random/3]).

    run(Workers, Items, Time) :-
        % start the workers
        forall(
            between(1, Workers, Worker),
            threaded_ignore(worker(Worker, Items, Time))
        ),
        % assemble the items
        checkpoint_loop(Workers, Items).

    run :-
        % default values
        run(5, 3, 0.100).

    checkpoint_loop(_, 0) :-
        !,
        write('All assemblies done.'), nl.
    checkpoint_loop(Workers, Item) :-
        % wait for all threads to reach the checkpoint
        forall(
            between(1, Workers, Worker),
            threaded_wait(done(Worker, Item))
        ),
        write('Assembly of item '), write(Item), write(' done.'), nl,
        % signal the workers to procede to the next assembly
        NextItem is Item - 1,
        forall(
            between(1, Workers, Worker),
            threaded_notify(next(Worker, NextItem))
        ),
        checkpoint_loop(Workers, NextItem).

    worker(_, 0, _) :-
        !.
    worker(Worker, Item, Time) :-
        % the time necessary to assemble one item varies between 0.0 and Time seconds
        random(0.0, Time, AssemblyTime), thread_sleep(AssemblyTime),
        write('Worker '), write(Worker), write(' item '), write(Item), nl,
        % notify checkpoint that the worker have done his/her part of this item
        threaded_notify(done(Worker, Item)),
        % wait for green light to move to the next item
        NextItem is Item - 1,
        threaded_wait(next(Worker, NextItem)),
        worker(Worker, NextItem, Time).

:- end_object.

```

Output:

```text

| ?- checkpoint::run.
Worker 1 item 3
Worker 3 item 3
Worker 5 item 3
Worker 2 item 3
Worker 4 item 3
Assembly of item 3 done.
Worker 4 item 2
Worker 1 item 2
Worker 5 item 2
Worker 3 item 2
Worker 2 item 2
Assembly of item 2 done.
Worker 4 item 1
Worker 1 item 1
Worker 2 item 1
Worker 3 item 1
Worker 5 item 1
Assembly of item 1 done.
All assemblies done.
yes

```



## Oforth

Checkpoint is implemented as a task. It :

- Receives n "jobDone" events from n tasks into a "jobs" channel.

- Then sends $allDone event to all tasks so they can work again.

Each task :

- Sleeps randomly between 1 and 1000 milliseconds, simulating its job.

- Then sends "jobDone" to the checkpoint using "jobs" channel.

- And waits for $allDone checkpoint return on its personal channel.


```Oforth
: task(n, jobs, myChannel)
   while(true) [
      System.Out "TASK " << n << " : Beginning my work..." << cr
      System sleep(1000 rand)
      System.Out "TASK " << n << " : Finish, sendind done and waiting for others..." << cr
      jobs send($jobDone) drop
      myChannel receive drop
      ] ;

: checkPoint(n, jobs, channels)
   while(true) [
      #[ jobs receive drop ] times(n)
      "CHECKPOINT : All jobs done, sending done to all tasks" println
      channels apply(#[ send($allDone) drop ])
      ] ;

: testCheckPoint(n)
| jobs channels i |
   ListBuffer init(n, #[ Channel new ]) dup freeze ->channels
   Channel new ->jobs

   #[ checkPoint(n, jobs, channels) ] &
   n loop: i [ #[ task(i, jobs, channels at(i)) ] & ] ;
```



## Perl


The perlipc man page details several approaches to interprocess communication.  Here's one of my favourites: socketpair and fork.  I've omitted some error-checking for brevity.


```perl
#!/usr/bin/perl
use warnings;
use strict;
use v5.10;

use Socket;

my $nr_items = 3;

sub short_sleep($) {
    (my $seconds) = @_;
    select undef, undef, undef, $seconds;
}

# This is run in a worker thread.  It repeatedly waits for a character from
# the main thread, and sends a value back to the main thread.  A short
# sleep introduces random timing, just to keep us honest.

sub be_worker($$) {
    my ($socket, $value) = @_;
    for (1 .. $nr_items) {
        sysread $socket, my $dummy, 1;
        short_sleep rand 0.5;
        syswrite $socket, $value;
        ++$value;
    }

    exit;
}

# This function forks a worker and sends it a socket on which to talk to
# the main thread, as well as an initial value to work with.  It returns
# (to the main thread) a socket on which to talk to the worker.

sub fork_worker($) {
    (my $value) = @_;
    socketpair my $kidsock, my $dadsock, AF_UNIX, SOCK_STREAM, PF_UNSPEC
        or die "socketpair: $!";

    if (fork // die "fork: $!") {
        # We're the parent
        close $dadsock;
        return $kidsock;
    }
    else {
        # We're the child
        close $kidsock;
        be_worker $dadsock, $value;
        # Never returns
    }
}

# Fork two workers, send them start signals, retrieve the values they send
# back, and print them

my $alpha_sock = fork_worker 'A';
my $digit_sock = fork_worker 1;

for (1 .. $nr_items) {
    syswrite $_, 'x'   for $alpha_sock, $digit_sock;
    sysread $alpha_sock, my $alpha, 1;
    sysread $digit_sock, my $digit, 1;
    say $alpha, $digit;
}

# If the main thread were planning to run for a long time after the
# workers had terminate, it would need to reap them to avoid zombies:

wait; wait;
```


A sample run:

```txt

msl@64Lucid:~/perl$ ./checkpoint
A1
B2
C3
msl@64Lucid:~/perl$

```



## Perl 6


```perl6
#!/usr/bin/env perl6

use v6;

my $TotalWorkers = 3;
my $BatchToRun = 3;
my @TimeTaken = (5..15); # in seconds

my $batch_progress = 0;
my @batch_lock = map { Semaphore.new(1) } , ^$TotalWorkers;
my $lock = Lock.new;

sub assembly_line ($ID) {
   my $wait;
   for ^$BatchToRun -> $j {
      $wait = @TimeTaken.roll;
      say "Worker ",$ID," at batch $j will work for ",$wait," seconds ..";
      sleep($wait);
      $lock.protect: {
         my $k = ++$batch_progress;
         print "Worker ",$ID," is done and update batch $j complete counter ";
         say "to $k of $TotalWorkers";
         if ($batch_progress == $TotalWorkers) {
            say ">>>>> batch $j completed.";
            $batch_progress = 0; # reset for next batch
            for @batch_lock { .release }; # and ready for next batch
         };
       };

       @batch_lock[$ID].acquire; # for next batch
   }
}

for ^$TotalWorkers -> $i {
   Thread.start(
      sub {
         @batch_lock[$i].acquire;
         assembly_line($i);
      }
   );
}
```

```txt
Worker 1 at batch 0 will work for 6 seconds ..
Worker 2 at batch 0 will work for 32 seconds ..
Worker 0 at batch 0 will work for 13 seconds ..
Worker 1 is done and update batch 0 complete counter to 1 of 3
Worker 0 is done and update batch 0 complete counter to 2 of 3
Worker 2 is done and update batch 0 complete counter to 3 of 3
>>>>> batch 0 completed.
Worker 2 at batch 1 will work for 27 seconds ..
Worker 0 at batch 1 will work for 18 seconds ..
Worker 1 at batch 1 will work for 13 seconds ..
Worker 1 is done and update batch 1 complete counter to 1 of 3
Worker 0 is done and update batch 1 complete counter to 2 of 3
Worker 2 is done and update batch 1 complete counter to 3 of 3
>>>>> batch 1 completed.
Worker 2 at batch 2 will work for 5 seconds ..
Worker 0 at batch 2 will work for 28 seconds ..
Worker 1 at batch 2 will work for 33 seconds ..
Worker 2 is done and update batch 2 complete counter to 1 of 3
Worker 0 is done and update batch 2 complete counter to 2 of 3
Worker 1 is done and update batch 2 complete counter to 3 of 3
>>>>> batch 2 completed.

```



## Phix

Simple multitasking solution: no locking required, no race condition possible, supports workers leaving and joining.

```Phix
-- demo\rosetta\checkpoint_synchronisation.exw
constant NPARTS = 3
integer workers = 0
sequence waiters = {}
bool terminate = false

procedure checkpoint(integer task_id)
    if length(waiters)+1=NPARTS or terminate then
        printf(1,"checkpoint\n")
        for i=1 to length(waiters) do
            task_schedule(waiters[i],1)
        end for
        waiters = {}
    else
        waiters &= task_id
        task_suspend(task_id)
        task_yield()
    end if
end procedure

procedure worker(string name)
    printf(1,"worker %s running\n",{name})
    while not terminate do
        printf(1,"worker %s begins part\n",{name})
        task_delay(rnd())
        printf(1,"worker %s completes part\n",{name})
        checkpoint(task_self())
        if rnd()>0.95 then exit end if
        task_delay(rnd())
    end while
    printf(1,"worker %s leaves\n",{name})
    workers -= 1
end procedure

string name = "A"

while get_key()!=#1B do -- (key escape to shut down)
    if workers<NPARTS then
        integer task_id = task_create(routine_id("worker"),{name})
        task_schedule(task_id,1)
        name[1] += 1
        workers += 1
    end if
    task_yield()
end while
printf(1,"escape keyed\n")
terminate = true
while workers>0 do
    task_yield()
end while
```

<pre style="height: 200px;overflow:scroll">
worker A running
worker A begins part
worker B running
worker B begins part
worker C running
worker C begins part
worker B completes part
worker C completes part
worker A completes part
checkpoint
worker B begins part
worker C begins part
worker A begins part
worker B completes part
worker A completes part
worker C completes part
checkpoint
worker B begins part
worker C begins part
worker A begins part
worker B completes part
worker C completes part
worker A completes part
checkpoint
worker B begins part
worker B completes part
worker C begins part
worker C completes part
worker A begins part
worker A completes part
checkpoint
worker A leaves
worker C begins part
worker B begins part
worker D running
worker D begins part
worker D completes part
worker C completes part
worker B completes part
checkpoint
worker B begins part
worker D begins part
worker B completes part
worker D completes part
worker C begins part
worker C completes part
checkpoint
worker B begins part
worker D begins part
worker C begins part
worker B completes part
worker C completes part
worker D completes part
checkpoint
worker B begins part
worker D begins part
worker C begins part
worker D completes part
worker B completes part
worker C completes part
checkpoint
worker C begins part
worker D begins part
worker B begins part
worker C completes part
worker B completes part
worker D completes part
checkpoint
escape keyed
worker C leaves
worker D leaves
worker B leaves

```



## PicoLisp

The following solution implements each worker as a coroutine. Therefore, it
works only in the 64-bit version.

'checkpoints' takes a number of projects to do, and a number of workers. Each
worker is started with a random number of steps to do (between 2 and 5), and is
kept in a list of 'Staff' members. Whenever a worker finishes, he is removed
from that list, until it is empty and the project is done.

'worker' takes a number of steps to perform. It "works" by printing each step,
and returning NIL when done.

```PicoLisp
(de checkpoints (Projects Workers)
   (for P Projects
      (prinl "Starting project number " P ":")
      (for
         (Staff
            (mapcar
               '((I) (worker (format I) (rand 2 5)))  # Create staff of workers
               (range 1 Workers) )
            Staff                                     # While still busy
            (filter worker Staff) ) )                 # Remove finished workers
      (prinl "Project number " P " is done.") ) )

(de worker (ID Steps)
   (co ID
      (prinl "Worker " ID " has " Steps " steps to do")
      (for N Steps
         (yield ID)
         (prinl "Worker " ID " step " N) )
      NIL ) )
```

Output:

```txt
: (checkpoints 2 3)  # Start two projects with 3 workers
Starting project number 1:
Worker 1 has 2 steps to do
Worker 2 has 3 steps to do
Worker 3 has 5 steps to do
Worker 1 step 1
Worker 2 step 1
Worker 3 step 1
Worker 1 step 2
Worker 2 step 2
Worker 3 step 2
Worker 2 step 3
Worker 3 step 3
Worker 3 step 4
Worker 3 step 5
Project number 1 is done.
Starting project number 2:
Worker 1 has 4 steps to do
Worker 2 has 3 steps to do
Worker 3 has 2 steps to do
Worker 1 step 1
Worker 2 step 1
Worker 3 step 1
Worker 1 step 2
Worker 2 step 2
Worker 3 step 2
Worker 1 step 3
Worker 2 step 3
Worker 1 step 4
Project number 2 is done.
```



## PureBasic


PureBasic normally uses Semaphores and Mutex’s to synchronize parallel systems. This system only relies on semaphores between each thread and the controller (CheckPoint-procedure). For exchanging data a Mutex based message stack could easily be added, both synchronized according to this specific task or non-blocking if each worker could be allowed that freedom.

```PureBasic
#MaxWorktime=8000 ; "Workday" in msec

; Structure that each thread uses
Structure MyIO
  ThreadID.i
  Semaphore_Joining.i
  Semaphore_Release.i
  Semaphore_Deliver.i
  Semaphore_Leaving.i
EndStructure

; Array of used threads
Global Dim Comm.MyIO(0)

; Master loop synchronizing the threads via semaphores
Procedure CheckPoint()
  Protected i, j, maxthreads=ArraySize(Comm())
  Protected Worker_count, Deliver_count
  Repeat
    For i=1 To maxthreads
      With Comm(i)
        If TrySemaphore(\Semaphore_Leaving)
          Worker_count-1
        ElseIf TrySemaphore(\Semaphore_Deliver)
          Deliver_count+1
          If Deliver_count=Worker_count
            PrintN("All Workers reported in, starting next task.")
            Deliver_count=0
            For j=1 To maxthreads
              SignalSemaphore(Comm(j)\Semaphore_Release)
            Next j
          EndIf
        ElseIf TrySemaphore(\Semaphore_Joining)
          PrintN("A new Worker joined the force.")
          Worker_count+1: SignalSemaphore(\Semaphore_Release)
        ElseIf Worker_count=0
          ProcedureReturn
        EndIf
      Next i
    EndWith
  ForEver
  StartAll=0
EndProcedure

; A worker thread, all orchestrated by the Checkpoint() routine
Procedure Worker(ID)
  Protected EndTime=ElapsedMilliseconds()+#MaxWorktime, n
  With Comm(ID)
    SignalSemaphore(\Semaphore_Joining)
    Repeat
      Repeat ; Use a non-blocking semaphore check to avoid dead-locking at shutdown.
        If ElapsedMilliseconds()>EndTime
          SignalSemaphore(\Semaphore_Leaving)
          PrintN("Thread #"+Str(ID)+" is done.")
          ProcedureReturn
        EndIf
        Delay(1)
      Until TrySemaphore(\Semaphore_Release)
      n=Random(1000)
      PrintN("Thread #"+Str(ID)+" will work for "+Str(n)+" msec.")
      Delay(n): PrintN("Thread #"+Str(ID)+" delivering")
      SignalSemaphore(\Semaphore_Deliver)
    ForEver
  EndWith
EndProcedure

; User IO & init
If OpenConsole()
  Define i, j
  Repeat
    Print("Enter number of workers to use [2-2000]: ")
    j=Val(Input())
  Until j>=2 And j<=2000
  ReDim Comm(j)
  For i=1 To j
    With Comm(i)
      \Semaphore_Release =CreateSemaphore()
      \Semaphore_Joining =CreateSemaphore()
      \Semaphore_Deliver =CreateSemaphore()
      \Semaphore_Leaving =CreateSemaphore()
      \ThreadID = CreateThread(@Worker(),i)
    EndWith
  Next
  PrintN("Work started, "+Str(j)+" workers has been called.")
  CheckPoint()
  Print("Press ENTER to exit"): Input()
EndIf
```

<pre style="height: 200px;overflow:scroll">Enter number of workers to use [2-2000]: 5
Work started, 5 workers has been called.
A new Worker joined the force.
A new Worker joined the force.
A new Worker joined the force.
A new Worker joined the force.
A new Worker joined the force.
Thread #5 will work for 908 msec.
Thread #3 will work for 405 msec.
Thread #1 will work for 536 msec.
Thread #2 will work for 632 msec.
Thread #4 will work for 202 msec.
Thread #4 delivering
Thread #3 delivering
Thread #1 delivering
Thread #2 delivering
Thread #5 delivering
All Workers reported in, starting next task.
Thread #2 will work for 484 msec.
Thread #4 will work for 836 msec.
Thread #5 will work for 464 msec.
Thread #3 will work for 251 msec.
Thread #1 will work for 734 msec.
Thread #3 delivering
Thread #5 delivering
Thread #2 delivering
Thread #1 delivering
Thread #4 delivering
All Workers reported in, starting next task.
Thread #3 will work for 864 msec.
Thread #1 will work for 526 msec.
Thread #5 will work for 145 msec.
Thread #2 will work for 762 msec.
Thread #4 will work for 283 msec.
Thread #5 delivering
Thread #4 delivering
Thread #1 delivering
Thread #2 delivering
Thread #3 delivering
All Workers reported in, starting next task.
Thread #2 will work for 329 msec.
Thread #4 will work for 452 msec.
Thread #1 will work for 176 msec.
Thread #5 will work for 702 msec.
Thread #3 will work for 500 msec.
Thread #1 delivering
Thread #2 delivering
Thread #4 delivering
Thread #3 delivering
Thread #5 delivering
All Workers reported in, starting next task.
Thread #5 will work for 681 msec.
Thread #3 will work for 71 msec.
Thread #2 will work for 267 msec.
Thread #1 will work for 151 msec.
Thread #4 will work for 252 msec.
Thread #3 delivering
Thread #1 delivering
Thread #4 delivering
Thread #2 delivering
Thread #5 delivering
All Workers reported in, starting next task.
Thread #5 will work for 963 msec.
Thread #3 will work for 378 msec.
Thread #1 will work for 209 msec.
Thread #4 will work for 897 msec.
Thread #2 will work for 736 msec.
Thread #1 delivering
Thread #3 delivering
Thread #2 delivering
Thread #5 delivering
Thread #4 delivering
All Workers reported in, starting next task.
Thread #2 will work for 44 msec.
Thread #4 will work for 973 msec.
Thread #1 will work for 700 msec.
Thread #3 will work for 505 msec.
Thread #5 will work for 256 msec.
Thread #2 delivering
Thread #5 delivering
Thread #3 delivering
Thread #1 delivering
Thread #4 delivering
All Workers reported in, starting next task.
Thread #2 will work for 703 msec.
Thread #4 will work for 296 msec.
Thread #1 will work for 702 msec.
Thread #3 will work for 99 msec.
Thread #5 will work for 114 msec.
Thread #3 delivering
Thread #5 delivering
Thread #4 delivering
Thread #1 delivering
Thread #2 delivering
All Workers reported in, starting next task.
Thread #3 will work for 97 msec.
Thread #5 will work for 192 msec.
Thread #2 will work for 762 msec.
Thread #1 will work for 232 msec.
Thread #4 will work for 484 msec.
Thread #3 delivering
Thread #5 delivering
Thread #1 delivering
Thread #4 delivering
Thread #2 delivering
All Workers reported in, starting next task.
Thread #1 will work for 790 msec.
Thread #5 will work for 602 msec.
Thread #3 will work for 105 msec.
Thread #2 will work for 449 msec.
Thread #4 will work for 180 msec.
Thread #3 delivering
Thread #4 delivering
Thread #2 delivering
Thread #2 is done.
Thread #4 is done.
Thread #3 is done.
Thread #5 delivering
Thread #5 is done.
Thread #1 delivering
Thread #1 is done.
Press ENTER to exit
```



## Python


```Python

"""

Based on https://pymotw.com/3/threading/

"""

import threading
import time
import random


def worker(workernum, barrier):
    # task 1
    sleeptime = random.random()
    print('Starting worker '+str(workernum)+" task 1, sleeptime="+str(sleeptime))
    time.sleep(sleeptime)
    print('Exiting worker'+str(workernum))
    barrier.wait()
    # task 2
    sleeptime = random.random()
    print('Starting worker '+str(workernum)+" task 2, sleeptime="+str(sleeptime))
    time.sleep(sleeptime)
    print('Exiting worker'+str(workernum))

barrier = threading.Barrier(3)

w1 = threading.Thread(target=worker, args=((1,barrier)))
w2 = threading.Thread(target=worker, args=((2,barrier)))
w3 = threading.Thread(target=worker, args=((3,barrier)))

w1.start()
w2.start()
w3.start()

```

Output:

```txt

Starting worker 1 task 1, sleeptime=0.26685336937182835
Starting worker 2 task 1, sleeptime=0.947511912308323
Starting worker 3 task 1, sleeptime=0.6495569605252262
Exiting worker1
Exiting worker3
Exiting worker2
Starting worker 2 task 2, sleeptime=0.5585479798026259
Starting worker 3 task 2, sleeptime=0.4104925281220747
Starting worker 1 task 2, sleeptime=0.15963562165203105
Exiting worker1
Exiting worker3
Exiting worker2

```



## Racket

This solution uses a double barrier to synchronize the five threads.
The method can be found on page 41 of the delightful book
[http://greenteapress.com/semaphores/downey08semaphores.pdf "The Little Book of Semaphores"] by Allen B. Downey.

```racket

#lang racket
(define t 5)     ; total number of threads
(define count 0) ; number of threads arrived at rendezvous
(define mutex      (make-semaphore 1)) ; exclusive access to count
(define turnstile  (make-semaphore 0))
(define turnstile2 (make-semaphore 1))
(define ch (make-channel))

(define (make-producer name start)
  (λ ()
    (let loop ([n start])
      (sleep (* 0.01 (random 10))) ; "compute" something
      ;; rendezvous
      (semaphore-wait mutex)
      (set! count (+ count 1)) ; we have arrived
      (when (= count t) ; are we the last to arrive?
        (semaphore-wait turnstile2)
        (semaphore-post turnstile))
      (semaphore-post mutex)
      ; avoid deadlock problem:
      (semaphore-wait turnstile)
      (semaphore-post turnstile)
      ; critical point
      (channel-put ch n) ; send result to controller
      ; leave properly
      (semaphore-wait mutex)
      (set! count (- count 1))
      (when (= count 0) ; are we the last to leave?
        (semaphore-wait turnstile)
        (semaphore-post turnstile2))
      (semaphore-post mutex)

      (semaphore-wait turnstile2)
      (semaphore-post turnstile2)

      (loop (+ n t)))))

; start t workers:
(map (λ(start) (thread (make-producer start start)))
     (range 0 t))

(let loop ()
  (displayln (for/list ([_ t]) (channel-get ch)))
  (loop))

```

Output:

```racket

(1 4 2 0 3)
(6 9 7 8 5)
(11 10 14 12 13)
(16 15 18 19 17)
(24 21 20 23 22)
(29 25 28 27 26)
(30 33 34 32 31)
(37 38 39 35 36)
(44 43 41 40 42)
(46 45 48 49 47)
(50 53 51 54 52)
(56 57 58 55 59)
(60 63 62 61 64)
(66 69 65 68 67)
(73 70 74 71 72)
(78 77 76 79 75)
(82 80 81 84 83)
(87 89 88 86 85)
(92 93 90 91 94)
(97 98 99 95 96)
...

```



## Ruby

```ruby
require 'socket'

# A Workshop runs all of its workers, then collects their results. Use
# Workshop#add to add workers and Workshop#work to run them.
#
# This implementation forks some processes to run the workers in
# parallel. Ruby must provide Kernel#fork and 'socket' library must
# provide UNIXSocket.
#
# Why processes and not threads? C Ruby still has a Global VM Lock,
# where only one thread can hold the lock. One platform, OpenBSD, still
# has userspace threads, with all threads on one cpu core. Multiple
# processes will not compete for a single Global VM Lock and can run
# on multiple cpu cores.
class Workshop
  # Creates a Workshop.
  def initialize
    @sockets = {}
  end

  # Adds a worker to this Workshop. Returns a worker id _wid_ for this
  # worker. The worker is a block that takes some _args_ and returns
  # some value. Workshop#work will run the block.
  #
  # This implementation forks a process for the worker. This process
  # will use Marshal with UNIXSocket to receive the _args_ and to send
  # the return value. The _wid_ is a process id. The worker also
  # inherits _IO_ objects, which might be a problem if the worker holds
  # open a pipe or socket, and the other end never reads EOF.
  def add
    child, parent = UNIXSocket.pair

    wid = fork do
      # I am the child.
      child.close
      @sockets.each_value { |sibling| sibling.close }

      # Prevent that all the children print their backtraces (to a mess
      # of mixed lines) when user presses Control-C.
      Signal.trap("INT") { exit! }

      loop do
        # Wait for a command.
        begin
          command, args = Marshal.load(parent)
        rescue EOFError
          # Parent probably died.
          break
        end

        case command
        when :work
          # Do work. Send result to parent.
          result = yield *args
          Marshal.dump(result, parent)
        when :remove
          break
        else
          fail "bad command from workshop"
        end
      end
    end

    # I am the parent.
    parent.close
    @sockets[wid] = child
    wid
  end

  # Runs all of the workers, and collects the results in a Hash. Passes
  # the same _args_ to each of the workers. Returns a Hash that pairs
  # _wid_ => _result_, where _wid_ is the worker id and _result_ is the
  # return value from the worker.
  #
  # This implementation runs the workers in parallel, and waits until
  # _all_ of the workers finish their results. Workshop provides no way
  # to start the work without waiting for the work to finish. If a
  # worker dies (for example, by raising an Exception), then
  # Workshop#work raises a RuntimeError.
  def work(*args)
    message = [:work, args]
    @sockets.each_pair do |wid, child|
      Marshal.dump(message, child)
    end

    # Checkpoint! Wait for all workers to finish.
    result = {}
    @sockets.each_pair do |wid, child|
      begin
        # This waits until the child finishes a result.
        result[wid] = Marshal.load(child)
      rescue EOFError
        fail "Worker #{wid} died"
      end
    end
    result
  end

  # Removes a worker from the Workshop, who has a worker id _wid_.
  # If there is no such worker, raises ArgumentError.
  #
  # This implementation kills and reaps the process for the worker.
  def remove(wid)
    unless child = @sockets.delete(wid)
      raise ArgumentError, "No worker #{wid}"
    else
      Marshal.dump([:remove, nil], child)
      child.close
      Process.wait(wid)
    end
  end
end



# First create a Workshop.
require 'pp'
shop = Workshop.new
wids = []

# Our workers must not use the same random numbers after the fork.
@fixed_rand = false
def fix_rand
  unless @fixed_rand; srand; @fixed_rand = true; end
end

# Start with some workers.
6.times do
  wids << shop.add do |i|
    # This worker slowly calculates a Fibonacci number.
    fix_rand
    f = proc { |n| if n < 2 then n else f[n - 1] + f[n - 2] end }
    [i, f[25 + rand(10)]]
  end
end

6.times do |i|
  # Do one cycle of work, and print the result.
  pp shop.work(i)

  # Remove a worker.
  victim = rand(wids.length)
  shop.remove wids[victim]
  wids.slice! victim

  # Add another worker.
  wids << shop.add do |j|
    # This worker slowly calculates a number from
    # the sequence 0, 1, 2, 3, 6, 11, 20, 37, 68, 125, ...
    fix_rand
    f = proc { |n| if n < 3 then n else f[n - 1] + f[n - 2] + f[n - 3] end }
    [j, i, f[20 + rand(10)]]
  end
end

# Remove all workers.
wids.each { |wid| shop.remove wid }
pp shop.work(6)
```


Example of output:
```txt
{23187=>[0, 1346269],
 17293=>[0, 1346269],
 9974=>[0, 317811],
 31730=>[0, 196418],
 30156=>[0, 2178309],
 25663=>[0, 832040]}
...
{23187=>[5, 5702887],
 17293=>[5, 832040],
 31730=>[5, 514229],
 17459=>[5, 2, 24548655],
 18683=>[5, 3, 187427],
 4494=>[5, 4, 1166220]}
{}
```



## Scala


```Scala
import java.util.{Random, Scanner}

object CheckpointSync extends App {
  val in = new Scanner(System.in)

  /*
   * Informs that workers started working on the task and
   * starts running threads. Prior to proceeding with next
   * task syncs using static Worker.checkpoint() method.
   */
  private def runTasks(nTasks: Int): Unit = {

    for (i <- 0 until nTasks) {
      println("Starting task number " + (i + 1) + ".")
      runThreads()
      Worker.checkpoint()
    }
  }

  /*
   * Creates a thread for each worker and runs it.
   */
  private def runThreads(): Unit =
    for (i <- 0 until Worker.nWorkers) new Thread(new Worker(i + 1)).start()

  class Worker(/* inner class instance variables */ var threadID: Int)
    extends Runnable {
    override def run(): Unit = {
      work()
    }

    /*
     *  Notifies that thread started running for 100 to 1000 msec.
     *  Once finished increments static counter 'nFinished'
     *  that counts number of workers finished their work.
     */
    private def work(): Unit = {
      try {
        val workTime = Worker.rgen.nextInt(900) + 100
        println("Worker " + threadID + " will work for " + workTime + " msec.")
        Thread.sleep(workTime) //work for 'workTime'

        Worker.nFinished += 1 //increases work finished counter

        println("Worker " + threadID + " is ready")
      } catch {
        case e: InterruptedException =>
          System.err.println("Error: thread execution interrupted")
          e.printStackTrace()
      }
    }
  }

  /*
   * Worker inner static class.
   */
  object Worker {
    private val rgen = new Random
    var nWorkers = 0
    private var nFinished = 0

    /*
     * Used to synchronize Worker threads using 'nFinished' static integer.
     * Waits (with step of 10 msec) until 'nFinished' equals to 'nWorkers'.
     * Once they are equal resets 'nFinished' counter.
     */
    def checkpoint(): Unit = {
      while (nFinished != nWorkers)
        try Thread.sleep(10)
        catch {
          case e: InterruptedException =>
            System.err.println("Error: thread execution interrupted")
            e.printStackTrace()
        }
      nFinished = 0
    }
  }

  print("Enter number of workers to use: ")
  Worker.nWorkers = in.nextInt
  print("Enter number of tasks to complete:")
  runTasks(in.nextInt)

}
```



## Tcl

This implementation works by having a separate thread handle the synchronization (inter-thread message delivery already being serialized). The alternative, using a read-write mutex, is more complex and more likely to run into trouble with multi-core machines.

```tcl
package require Tcl 8.5
package require Thread

namespace eval checkpoint {
    namespace export {[a-z]*}
    namespace ensemble create
    variable members {}
    variable waiting {}
    variable event
    # Back-end of join operation
    proc Join {id} {
	variable members
	variable counter
	if {$id ni $members} {
	    lappend members $id
	}
	return $id
    }
    # Back-end of leave operation
    proc Leave {id} {
	variable members
	set idx [lsearch -exact $members $id]
	if {$idx > -1} {
	    set members [lreplace $members $idx $idx]
	    variable event
	    if {![info exists event]} {
		set event [after idle ::checkpoint::Release]
	    }
	}
	return
    }
    # Back-end of deliver operation
    proc Deliver {id} {
	variable waiting
	lappend waiting $id

	variable event
	if {![info exists event]} {
	    set event [after idle ::checkpoint::Release]
	}
	return
    }
    # Releasing is done as an "idle" action to prevent deadlocks
    proc Release {} {
	variable members
	variable waiting
	variable event
	unset event
	if {[llength $members] != [llength $waiting]} return
	set w $waiting
	set waiting {}
	foreach id $w {
	    thread::send -async $id {incr ::checkpoint::Delivered}
	}
    }

    # Make a thread and attach it to the public API of the checkpoint
    proc makeThread {{script ""}} {
	set id [thread::create thread::wait]
	thread::send $id {
	    namespace eval checkpoint {
		namespace export {[a-z]*}
		namespace ensemble create

		# Call to actually join the checkpoint group
		proc join {} {
		    variable checkpoint
		    thread::send $checkpoint [list \
			    ::checkpoint::Join [thread::id]]
		}
		# Call to actually leave the checkpoint group
		proc leave {} {
		    variable checkpoint
		    thread::send $checkpoint [list \
			    ::checkpoint::Leave [thread::id]]
		}
		# Call to wait for checkpoint synchronization
		proc deliver {} {
		    variable checkpoint
		    # Do this from within the [vwait] to ensure that we're already waiting
		    after 0 [list thread::send $checkpoint [list \
			    ::checkpoint::Deliver [thread::id]]]
		    vwait ::checkpoint::Delivered
		}
	    }
	}
	thread::send $id [list set ::checkpoint::checkpoint [thread::id]]
	thread::send $id $script
	return $id
    }

    # Utility to help determine whether the checkpoint is in use
    proc anyJoined {} {
	variable members
	expr {[llength $members] > 0}
    }
}
```

Demonstration of how this works.
```tcl
# Build the workers
foreach worker {A B C D} {
    dict set ids $worker [checkpoint makeThread {
	proc task {name} {
	    checkpoint join
	    set deadline [expr {[clock seconds] + 2}]
	    while {[clock seconds] <= $deadline} {
		puts "$name is working"
		after [expr {int(500 * rand())}]
		puts "$name is ready"
		checkpoint deliver
	    }
	    checkpoint leave
	    thread::release; # Ask the thread to finish
	}
    }]
}

# Set them all processing in the background
dict for {name id} $ids {
    thread::send -async $id "task $name"
}

# Wait until all tasks are done (i.e., they have unregistered)
while 1 {
    after 100 set s 1; vwait s; # Process events for 100ms
    if {![checkpoint anyJoined]} {
	break
    }
}
```

Output:

```txt

A is working
C is working
B is working
D is working
B is ready
A is ready
D is ready
C is ready
B is working
A is working
D is working
C is working
D is ready
A is ready
C is ready
B is ready
B is working
D is working
A is working
C is working
D is ready
C is ready
B is ready
A is ready
D is working
C is working
B is working
A is working
D is ready
A is ready
C is ready
B is ready
D is working
C is working
A is working
B is working
C is ready
A is ready
B is ready
D is ready
```



## zkl

Simulate a pool of workers, each making one part, waiting for the part to be requested and then putting the part on a conveyor belt to be sent to the station that assembles all parts into a product. After shipping the part, it turns off the request flag.
The consumer requests a part it doesn't have, waits for a part and puts the received part (which might not be the requested one (if buggy code)) in a bin and assembles the parts into a product.
Repeat until all requested products are made.

```zkl
const NUM_PARTS=5;  // number of parts used to make the product
var requested=Atomic.Int(-1); // the id of the part the consumer needs
var pipe=Thread.Pipe();       // "conveyor belt" of parts to consumer

fcn producer(id,pipe){
   while(True){ // make part forever
      requested.waitFor(id);  // wait for consumer to ask for my part
      requested.set(-1);      // I'm making the part
      pipe.write(id);         // ship my part
   }
   println(id," stopped");
}

foreach id in (NUM_PARTS){ producer.launch(id,pipe) } // start workers/threads

product:=NUM_PARTS.pump(List(),0);  // parts I have on hand
do(10){	  // make 10 products
   while(False!=(id:=product.filter1n('==(0)))){ // gather parts to make product
      requested.set(id);
      part:=pipe.read();  // get requested part
      product[part]+=1; // assemble part into product
   }
   println("product made: ",product);
   foreach n in (NUM_PARTS){ product[n]-=1 } // remove parts from bin
}
println("Done");	// but workers are still waiting
```

An AtomicInt is an integer that does its operations in an atomic fashion. It is used to serialize the producers and consumer.

The filter1n list method returns the index of the first list element that meets the filter test else False.
```txt

product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
product made: L(1,1,1,1,1)
Done

```


