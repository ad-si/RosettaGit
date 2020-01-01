+++
title = "Atomic updates"
description = ""
date = 2019-06-25T10:57:30Z
aliases = []
[extra]
id = 4178
[taxonomies]
categories = []
tags = []
+++

{{task|Concurrency}}
{{requires|Concurrency}}

;Task:
Define a data type consisting of a fixed number of 'buckets', each containing a nonnegative integer value, which supports operations to:
# get the current value of any bucket
# remove a specified amount from one specified bucket and add it to another, preserving the total of all bucket values, and [[wp:Clamping (graphics)|clamping]] the transferred amount to ensure the values remain non-negative

----

In order to exercise this data type, create one set of buckets, and start three concurrent tasks:
# As often as possible, pick two buckets and make their values closer to equal.
# As often as possible, pick two buckets and arbitrarily redistribute their values.
# At whatever rate is convenient, display (by any means) the total value and, optionally, the individual values of each bucket.



The display task need not be explicit; use of e.g. a debugger or trace tool is acceptable provided it is simple to set up to provide the display.

----

This task is intended as an exercise in ''atomic'' operations.   The sum of the bucket values must be preserved even if the two tasks attempt to perform transfers simultaneously, and a straightforward solution is to ensure that at any time, only one transfer is actually occurring — that the transfer operation is ''atomic''.





## 8th


```Forth
var bucket
var bucket-size

\ The 'bucket' will be a simple array of some values:
: genbucket \ n --
  a:new swap
  (
    \ make a random int up to 1000
    rand-pcg n:abs 1000 n:mod
    a:push
  ) swap times
  bucket ! ;

\ display bucket and its total:
: .bucket
  bucket lock @
    dup . space
    ' n:+ 0 a:reduce . cr
  bucket unlock drop ;

\ Get current value of bucket #x
: bucket@ \ n -- bucket[n]
  bucket @
  swap a:@ nip ;

\ Transfer x from bucket n to bucket m
: bucket-xfer \ m n x --
  >r bucket @
  \ m n bucket
  over a:@ r@ n:-
  rot swap a:!
  \ m bucket
  over a:@ r> n:+
  rot swap a:!
  drop ;

\ Get two random indices to check (ensure they're not the same):
: pick2
  rand-pcg n:abs bucket-size @ n:mod dup >r
  repeat
    drop
    rand-pcg n:abs bucket-size @ n:mod
    r@ over n:=
  while!
  r> ;

\ Pick two buckets and make them more equal (by a quarter of their difference):
: make-equal
  repeat
    pick2
    bucket lock @
    third a:@ >r
    over a:@ r> n:-
    \ if they are equal, do nothing
    dup not if
      \ equal, so do nothing
      drop -rot 2drop
    else
      4 n:/ n:int
      >r -rot r>
      bucket-xfer
    then
    drop
    bucket unlock drop
  again ;

\ Moves a quarter of the smaller value from one (random) bucket to another:
: make-redist
  repeat
    pick2 bucket lock @
      \ n m bucket
      over a:@ >r \ n m b b[m]
      third a:@ r>  \ n m b b[n]
      n:min 4 n:/ n:int
      nip bucket-xfer

    bucket unlock drop
  again ;

: app:main
  \ create 10 buckets with random positive integer values:
  10 genbucket bucket @ a:len bucket-size ! drop

  \ print the bucket
  .bucket

  \ the problem's tasks:
  ' make-equal t:task
  ' make-redist t:task

  \ the print-the-bucket task. We'll do it just 10 times and then quit:
  ( 1 sleep .bucket ) 10 times
  bye ;
```

{{out}}
```txt
[941,654,311,605,332,822,62,658,9,348] 4742
[289,98,710,698,183,490,675,688,793,118] 4742
[269,51,141,11,3,1284,1371,436,344,832] 4742
[1097,229,1097,307,421,25,85,676,188,617] 4742
[503,475,459,467,458,477,451,488,460,504] 4742
[480,498,484,460,481,464,467,488,481,439] 4742
[442,491,511,446,540,487,424,489,524,388] 4742
[3,306,114,88,185,366,2331,202,1138,9] 4742
[312,187,212,616,698,790,551,572,568,236] 4742
[473,474,475,474,474,473,476,475,473,475] 4742
[466,457,448,468,454,501,479,490,469,510] 4742

```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Updates is

   type Bucket_Index is range 1..13;
   package Random_Index is new Ada.Numerics.Discrete_Random (Bucket_Index);
   use Random_Index;
   type Buckets is array (Bucket_Index) of Natural;

   protected type Safe_Buckets is
      procedure Initialize (Value : Buckets);
      function Get (I : Bucket_Index) return Natural;
      procedure Transfer (I, J : Bucket_Index; Amount : Integer);
      function Snapshot return Buckets;
   private
      Data : Buckets := (others => 0);
   end Safe_Buckets;

   protected body Safe_Buckets is
      procedure Initialize (Value : Buckets) is
      begin
         Data := Value;
      end Initialize;

      function Get (I : Bucket_Index) return Natural is
      begin
         return Data (I);
      end Get;

      procedure Transfer (I, J : Bucket_Index; Amount : Integer) is
         Increment : constant Integer :=
            Integer'Max (-Data (J), Integer'Min (Data (I), Amount));
      begin
         Data (I) := Data (I) - Increment;
         Data (J) := Data (J) + Increment;
      end Transfer;

      function Snapshot return Buckets is
      begin
         return Data;
      end Snapshot;
   end Safe_Buckets;

   Data : Safe_Buckets;

   task Equalize;
   task Mess_Up;

   task body Equalize is
      Dice : Generator;
      I, J : Bucket_Index;
   begin
      loop
         I := Random (Dice);
         J := Random (Dice);
         Data.Transfer (I, J, (Data.Get (I) - Data.Get (J)) / 2);
      end loop;
   end Equalize;

   task body Mess_Up is
      Dice : Generator;
   begin
      loop
         Data.Transfer (Random (Dice), Random (Dice), 100);
      end loop;
   end Mess_Up;

begin
   Data.Initialize ((1,2,3,4,5,6,7,8,9,10,11,12,13));
   loop
      delay 1.0;
      declare
         State : Buckets := Data.Snapshot;
         Sum   : Natural := 0;
      begin
         for Index in State'Range loop
            Sum := Sum + State (Index);
            Put (Integer'Image (State (Index)));
         end loop;
         Put (" =" & Integer'Image (Sum));
         New_Line;
      end;
   end loop;
end Test_Updates;
```

The array of buckets is a protected object which controls access to its state. The task Equalize averages pairs of buckets. The task Mess_Up moves content of one bucket to another. The main task performs monitoring of the buckets state. Sample output:

```txt

 18 0 0 0 36 16 0 0 0 2 0 19 0 = 91
 0 0 0 6 0 0 37 0 6 23 19 0 0 = 91
 1 0 7 66 4 0 0 4 0 0 0 0 9 = 91
 0 1 0 2 28 0 17 0 0 22 1 0 20 = 91
 2 0 0 11 0 37 17 0 0 0 8 0 16 = 91
 0 10 0 59 0 2 0 13 0 2 0 5 0 = 91
 0 1 0 10 0 0 0 0 0 0 80 0 0 = 91
 16 0 0 0 13 0 9 8 14 16 0 15 0 = 91
 0 1 2 0 1 0 42 1 0 42 2 0 0 = 91
 0 16 0 0 0 19 28 0 0 0 0 0 28 = 91
...

```



## AutoHotkey


```AutoHotkey
Bucket := [],	Buckets := 10,	Originaltotal = 0
loop, %Buckets% {
	Random, rnd, 0,50
	Bucket[A_Index] := rnd,		Originaltotal += rnd
}

loop 100
{
	total := 0
	Randomize(B1, B2, Buckets)
	temp := (Bucket[B1] + Bucket[B2]) /2
	Bucket[B1] := floor(temp),	Bucket[B2] := Ceil(temp)	; values closer to equal

	Randomize(B1, B2, Buckets)
	temp := Bucket[B1] + Bucket[B2]
	Random, value, 0, %temp%
	Bucket[B1] := value,	Bucket[B2] := temp-value		; redistribute values arbitrarily

	VisualTip := "Original Total = " Originaltotal "`n"
	loop, %Buckets%
		VisualTip .= SubStr("0" Bucket[A_Index], -1) " : " x(Bucket[A_Index]) "`n" , total += Bucket[A_Index]

	ToolTip % VisualTip "Current Total = " total
	if (total <> Originaltotal)
		MsgBox "Error"
	Sleep, 100
}
return

Randomize(ByRef B1, ByRef B2, Buckets){
	Random, B1, 1, %Buckets%
	Loop
		Random, B2, 1, %Buckets%
	until (B1<>B2)
}

x(n){
	loop, % n
		Res.= ">"
	return Res
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
The BBC BASIC interpreter is single-threaded so the 'concurrent' tasks are implemented by timer events.  In this context an 'atomic' update means one which takes place within a single BASIC statement, so it cannot be 'interrupted'.  Two (or more) buckets can be updated atomically by making them RETURN parameters of a procedure.

```bbcbasic
      INSTALL @lib$+"TIMERLIB"

      DIM Buckets%(100)
      FOR i% = 1 TO 100 : Buckets%(i%) = RND(10) : NEXT

      tid0% = FN_ontimer(10, PROCdisplay, 1)
      tid1% = FN_ontimer(11, PROCflatten, 1)
      tid2% = FN_ontimer(12, PROCroughen, 1)

      ON ERROR PROCcleanup : REPORT : PRINT : END
      ON CLOSE PROCcleanup : QUIT

      REPEAT
        WAIT 0
      UNTIL FALSE
      END

      DEF PROCdisplay
      PRINT SUM(Buckets%()) " ", MOD(Buckets%())
      ENDPROC

      DEF PROCflatten
      LOCAL d%, i%, j%
      REPEAT
        i% = RND(100)
        j% = RND(100)
      UNTIL i%<>j%
      d% = Buckets%(i%) - Buckets%(j%)
      PROCatomicupdate(Buckets%(i%), Buckets%(j%), d% DIV 4)
      ENDPROC

      DEF PROCroughen
      LOCAL i%, j%
      REPEAT
        i% = RND(100)
        j% = RND(100)
      UNTIL i%<>j%
      PROCatomicupdate(Buckets%(i%), Buckets%(j%), RND(10))
      ENDPROC

      DEF PROCatomicupdate(RETURN src%, RETURN dst%, amt%)
      IF amt% > src% amt% = src%
      IF amt% < -dst% amt% = -dst%
      src% -= amt%
      dst% += amt%
      ENDPROC

      DEF PROCcleanup
      PROC_killtimer(tid0%)
      PROC_killtimer(tid1%)
      PROC_killtimer(tid2%)
      ENDPROC
```



## C

{{trans|C#}}

{{works with|POSIX|.1-2001}}

{{libheader|pthread}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>

#define N_BUCKETS 15

pthread_mutex_t bucket_mutex[N_BUCKETS];
int buckets[N_BUCKETS];

pthread_t equalizer;
pthread_t randomizer;

void transfer_value(int from, int to, int howmuch)
{
  bool swapped = false;

  if ( (from == to) || ( howmuch < 0 ) ||
       (from < 0 ) || (to < 0) || (from >= N_BUCKETS) || (to >= N_BUCKETS) ) return;

  if ( from > to ) {
    int temp1 = from;
    from = to;
    to = temp1;
    swapped = true;
    howmuch = -howmuch;
  }

  pthread_mutex_lock(&bucket_mutex[from]);
  pthread_mutex_lock(&bucket_mutex[to]);

  if ( howmuch > buckets[from] && !swapped )
    howmuch = buckets[from];
  if ( -howmuch > buckets[to] && swapped )
    howmuch = -buckets[to];

  buckets[from] -= howmuch;
  buckets[to] += howmuch;

  pthread_mutex_unlock(&bucket_mutex[from]);
  pthread_mutex_unlock(&bucket_mutex[to]);
}

void print_buckets()
{
  int i;
  int sum=0;

  for(i=0; i < N_BUCKETS; i++) pthread_mutex_lock(&bucket_mutex[i]);
  for(i=0; i < N_BUCKETS; i++) {
    printf("%3d ", buckets[i]);
    sum += buckets[i];
  }
  printf("= %d\n", sum);
  for(i=0; i < N_BUCKETS; i++) pthread_mutex_unlock(&bucket_mutex[i]);
}

void *equalizer_start(void *t)
{
  for(;;) {
    int b1 = rand()%N_BUCKETS;
    int b2 = rand()%N_BUCKETS;
    int diff = buckets[b1] - buckets[b2];
    if ( diff < 0 )
      transfer_value(b2, b1, -diff/2);
    else
      transfer_value(b1, b2, diff/2);
  }
  return NULL;
}

void *randomizer_start(void *t)
{
  for(;;) {
    int b1 = rand()%N_BUCKETS;
    int b2 = rand()%N_BUCKETS;
    int diff = rand()%(buckets[b1]+1);
    transfer_value(b1, b2, diff);
  }
  return NULL;
}

int main()
{
  int i, total=0;

  for(i=0; i < N_BUCKETS; i++) pthread_mutex_init(&bucket_mutex[i], NULL);

  for(i=0; i < N_BUCKETS; i++) {
    buckets[i] = rand() % 100;
    total += buckets[i];
    printf("%3d ", buckets[i]);
  }
  printf("= %d\n", total);

  // we should check if these succeeded
  pthread_create(&equalizer, NULL, equalizer_start, NULL);
  pthread_create(&randomizer, NULL, randomizer_start, NULL);

  for(;;) {
    sleep(1);
    print_buckets();
  }

  // we do not provide a "good" way to stop this run, so the following
  // is never reached indeed...
  for(i=0; i < N_BUCKETS; i++) pthread_mutex_destroy(bucket_mutex+i);
  return EXIT_SUCCESS;
}
```



### With OpenMP

Compiled with <code>gcc -std=c99 -fopenmp</code>.  The <code>#pragma omp critical</code> ensures the following block is entered by one thread at a time.

```c
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define irand(n) (n * (double)rand()/(RAND_MAX + 1.0))

int bucket[10];
int main()
{
	int i;
	for (i = 0; i < 10; i++) bucket[i] = 1000;
	omp_set_num_threads(3);

	#pragma omp parallel private(i)
	for (i = 0; i < 10000; i++) {
		int from, to, mode, diff = 0, sum;

		from = irand(10);
		do { to = irand(10); } while (from == to);
		mode = irand(10);

		switch (mode) {
		case 0:
		case 1:
		case 2:	/* equalize */
			diff = (bucket[from] - bucket[to]) / 2;
			break;

		case 3: /* report */
			sum = 0;
			for (int j = 0; j < 10; j++) {
				printf("%d ", bucket[j]);
				sum += bucket[j];
			}
			printf(" Sum: %d\n", sum);
			continue;

		default: /* random transfer */
			diff = irand(bucket[from]);
			break;
		}

		#pragma omp critical
		{
			bucket[from] -= diff;
			bucket[to]   += diff;
		}
	}

	return 0;
}
```
Output:<lang>1000 1000 1000 1798 1000 1000 1000 1000 202 1000  Sum: 10000
595 800 2508 2750 470 1209 283 314 601 470  Sum: 10000
5 521 3339 1656 351 1038 1656 54 508 872  Sum: 10000
.
.
.
752 490 385 2118 1503 508 384 509 1110 2241  Sum: 10000
752 823 385 2118 1544 508 10 509 1110 2241  Sum: 10000
```



## C++

{{trans|C}}

{{works with|C++11}}

```cpp
#include <algorithm>
#include <array>
#include <chrono>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <random>
#include <thread>

using namespace std;

constexpr int bucket_count = 15;

void equalizer(array<int, bucket_count>& buckets,
               array<mutex, bucket_count>& bucket_mutex) {
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dist_bucket(0, bucket_count - 1);

    while (true) {
        int from = dist_bucket(gen);
        int to = dist_bucket(gen);
        if (from != to) {
            lock_guard<mutex> lock_first(bucket_mutex[min(from, to)]);
            lock_guard<mutex> lock_second(bucket_mutex[max(from, to)]);
            int diff = buckets[from] - buckets[to];
            int amount = abs(diff / 2);
            if (diff < 0) {
                swap(from, to);
            }
            buckets[from] -= amount;
            buckets[to] += amount;
        }
    }
}

void randomizer(array<int, bucket_count>& buckets,
                array<mutex, bucket_count>& bucket_mutex) {
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dist_bucket(0, bucket_count - 1);

    while (true) {
        int from = dist_bucket(gen);
        int to = dist_bucket(gen);
        if (from != to) {
            lock_guard<mutex> lock_first(bucket_mutex[min(from, to)]);
            lock_guard<mutex> lock_second(bucket_mutex[max(from, to)]);
            uniform_int_distribution<> dist_amount(0, buckets[from]);
            int amount = dist_amount(gen);
            buckets[from] -= amount;
            buckets[to] += amount;
        }
    }
}

void print_buckets(const array<int, bucket_count>& buckets) {
    int total = 0;
    for (const int& bucket : buckets) {
        total += bucket;
        cout << setw(3) << bucket << ' ';
    }
    cout << "= " << setw(3) << total << endl;
}

int main() {
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dist(0, 99);

    array<int, bucket_count> buckets;
    array<mutex, bucket_count> bucket_mutex;
    for (int& bucket : buckets) {
        bucket = dist(gen);
    }
    print_buckets(buckets);

    thread t_eq(equalizer, ref(buckets), ref(bucket_mutex));
    thread t_rd(randomizer, ref(buckets), ref(bucket_mutex));

    while (true) {
        this_thread::sleep_for(chrono::seconds(1));
        for (mutex& mutex : bucket_mutex) {
            mutex.lock();
        }
        print_buckets(buckets);
        for (mutex& mutex : bucket_mutex) {
            mutex.unlock();
        }
    }
    return 0;
}
```



## C#

This C# implementation uses a class to hold the buckets and data associated with them. The ThreadSafeBuckets class implements thread-stability, and ensures that two threads cannot operate on the same data at the same time. Additionally, the class uses a seperate mutex for each bucket, allowing multiple operations to occur at once if they do not alter the same buckets.

I updated the original class for a few things:
 - Changed to using object locks and Montor.Enter rather than Mutexes.  This allows use of the cleaner "lock" statement, and also has lower runtime overhead for in process locks
 - The previous implementation tracked a "swapped" state - which seems a harder way to tackle the problem.   You need to acquire the locks in the correct order, not swap i and j


```c#

using System; //Rand class
using System.Threading; //Thread, Mutex classes
public class ThreadSafeBuckets
{
    //This class is thread safe, and ensures that all operations on it are atomic.
    //Calling threads do not need to ensure safety.
    Random rand = new Random();
    int[] Buckets;
    object[] locks; //Mutexes for each bucket so they can lock individually
    public int BucketCount { get; private set; }
    public ThreadSafeBuckets(int bucketcount)
    {
        //Create buckets+mutexes and fill them with a random amount
        BucketCount = bucketcount;
        Buckets = new int[bucketcount];
        locks = new object[bucketcount];
        int startingtotal = 0;
        for (int i = 0; i < BucketCount; i++)
        {
            locks[i] = new object();
            Buckets[i] = rand.Next(30);
            startingtotal += Buckets[i];
        }
        //Print the starting total
        Console.WriteLine("Starting total: " + startingtotal);
    }
    public int GetBucketValue(int i)
    {
        return Buckets[i];
    }
    public void Transfer(int i, int j, int amount)
    {
        //Transfer amount from bucket i to bucket j
        if (i > BucketCount || j > BucketCount || i < 0 || j < 0 ||
            i == j || amount < 0)
            return;

        //To prevent deadlock, always lock the lower bucket first
        lock (locks[Math.Min(i, j)])
            lock (locks[Math.Max(i, j)])
            {
                //Make sure don't transfer out more than is in the bucket
                amount = Math.Min(amount, Buckets[i]);

                //Do the transfer
                Buckets[i] -= amount;
                Buckets[j] += amount;
            }
    }

    public void PrintBuckets()
    {
        int counter = 0;
        //Lock all the buckets in sequential order and print their contents
        for (int i = 0; i < BucketCount; i++)
        {
            Monitor.Enter(locks[i]);
            Console.Write(Buckets[i] + " ");
            counter += Buckets[i];
        }
        //Print the bucket total, then unlock all the mutexes
        Console.Write("= " + counter);
        Console.WriteLine();

        foreach (var l in locks)
            Monitor.Exit(l);
    }
}

class Program
{
    static ThreadSafeBuckets TSBs;

    public static void Main(){
        //Create the thread-safe bucket list
        TSBs = new ThreadSafeBuckets(10);
        TSBs.PrintBuckets();
        //Create and start the Equalizing Thread
        new Thread(new ThreadStart(EqualizerThread)).Start();
        Thread.Sleep(1);
        //Create and start the Randamizing Thread
        new Thread(new ThreadStart(RandomizerThread)).Start();
        //Use this thread to do the printing
        PrinterThread();
    }
    //EqualizerThread runs on it's own thread and randomly averages two buckets
    static void EqualizerThread()
    {
        Random rand = new Random();
        while (true)
        {
            //Pick two buckets
            int b1 = rand.Next(TSBs.BucketCount);
            int b2 = rand.Next(TSBs.BucketCount);
            //Get the difference
            int diff = TSBs.GetBucketValue(b1) - TSBs.GetBucketValue(b2);
            //Transfer to equalize
            if (diff < 0)
                TSBs.Transfer(b2, b1, -diff / 2);
            else
                TSBs.Transfer(b1, b2, diff/2);
        }
    }
    //RandomizerThread redistributes the values between two buckets
    static void RandomizerThread()
    {
        Random rand = new Random();
        while (true)
        {
            int b1 = rand.Next(TSBs.BucketCount);
            int b2 = rand.Next(TSBs.BucketCount);
            int diff = rand.Next(TSBs.GetBucketValue(b1));
            TSBs.Transfer(b1, b2, diff);
        }
    }
    //PrinterThread prints the current bucket contents
    static void PrinterThread()
    {
        while (true)
        {
            Thread.Sleep(50); //Only print every few milliseconds to let the other threads work
            TSBs.PrintBuckets();
        }
    }
}
```


Sample Output:

```txt

Starting total: 156
15 15 12 27 6 21 19 18 16 7 = 156
17 13 15 15 18 14 18 15 14 17 = 156
12 9 22 15 9 8 23 10 16 32 = 156
0 6 28 4 21 10 28 11 34 14 = 156
35 14 30 11 32 1 26 4 3 0 = 156
11 17 19 1 18 1 12 35 26 16 = 156

```



## Clojure

Function returning a new map containing altered values:

```lisp
(defn xfer [m from to amt]
  (let [{f-bal from t-bal to} m
        f-bal (- f-bal amt)
        t-bal (+ t-bal amt)]
    (if (or (neg? f-bal) (neg? t-bal))
      (throw (IllegalArgumentException. "Call results in negative balance."))
      (assoc m from f-bal to t-bal))))
```

Since clojure data structures are immutable, atomic mutability occurs via a reference, in this case an atom:

```lisp
(def *data* (atom {:a 100 :b 100})) ;; *data* is an atom holding a map
(swap! *data* xfer :a :b 50) ;; atomically results in *data* holding {:a 50 :b 150}
```

Now for the test:

```lisp
(defn equalize [m a b]
  (let [{a-val a b-val b} m
        diff (- a-val b-val)
        amt (/ diff 2)]
    (xfer m a b amt)))

(defn randomize [m a b]
  (let [{a-val a b-val b} m
        min-val (min a-val b-val)
        amt (rand-int (- min-val) min-val)]
    (xfer m a b amt)))

(defn test-conc [f data a b n name]
  (dotimes [i n]
    (swap! data f a b)
    (println (str "total is " (reduce + (vals @data)) " after " name " iteration " i))))

(def thread-eq (Thread. #(test-conc equalize *data* :a :b 1000 "equalize")))
(def thread-rand (Thread. #(test-conc randomize *data* :a :b 1000 "randomize")))

(.start thread-eq)
(.start thread-rand)
```



## Common Lisp

Depends on libraries in Quicklisp. STMX is a library that provides Software Transactional Memory.

```lisp
(ql:quickload '(:alexandria :stmx :bordeaux-threads))

(defpackage :atomic-updates
  (:use :cl))

(in-package :atomic-updates)

(defvar *buckets* nil)
(defvar *running* nil)

(defun distribute (ratio a b)
  "Atomically redistribute the values of buckets A and B by RATIO."
  (stmx:atomic
   (let* ((sum (+ (stmx:$ a) (stmx:$ b)))
          (a2 (truncate (* ratio sum))))
     (setf (stmx:$ a) a2)
     (setf (stmx:$ b) (- sum a2)))))

(defun runner (ratio-func)
  "Continously distribute to two different elements in *BUCKETS* with the
value returned from RATIO-FUNC."
  (loop while *running*
     do (let ((a (alexandria:random-elt *buckets*))
              (b (alexandria:random-elt *buckets*)))
          (unless (eq a b)
            (distribute (funcall ratio-func) a b)))))

(defun print-buckets ()
  "Atomically get the bucket values and print out their metrics."
  (let ((buckets (stmx:atomic (map 'vector 'stmx:$ *buckets*))))
    (format t "Buckets: ~a~%Sum: ~a~%" buckets (reduce '+ buckets))))

(defun scenario ()
  (setf *buckets* (coerce (loop repeat 20 collect (stmx:tvar 10)) 'vector))
  (setf *running* t)
  (bt:make-thread (lambda () (runner (constantly 0.5))))
  (bt:make-thread (lambda () (runner (lambda () (random 1.0))))))
```

{{out}}

```lisp
ATOMIC-UPDATES> (scenario)
#<SB-THREAD:THREAD "Anonymous thread" RUNNING {10058441D3}>
ATOMIC-UPDATES> (loop repeat 3 do (print-buckets) (sleep 1))
Buckets: #(8 4 12 17 12 10 5 10 9 10 4 11 4 15 16 20 11 8 4 10)
Sum: 200
Buckets: #(2 12 24 7 8 3 13 6 8 31 0 9 7 11 12 8 8 12 15 4)
Sum: 200
Buckets: #(1 2 3 3 2 8 33 23 0 8 4 11 24 2 3 5 32 8 2 26)
Sum: 200
NIL
```



## D

This implements a more scalable version than most of the other languages, by using a lock per bucket instead of a single lock for the whole array.


```d
import std.stdio: writeln;
import std.conv: text;
import std.random: uniform, Xorshift;
import std.algorithm: min, max;
import std.parallelism: task;
import core.thread: Thread;
import core.sync.mutex: Mutex;
import core.time: seconds;

__gshared uint transfersCount;

final class Buckets(size_t nBuckets) if (nBuckets > 0) {
    alias TBucketValue = uint;

    // The trailing padding avoids cache line contention
    // when run with two or more cores.
    align(128) private static struct Bucket {
        TBucketValue value;
        Mutex mtx;
        alias value this;
    }

    private Bucket[nBuckets] buckets;
    private bool running;

    public this() {
        this.running = true;
        foreach (ref b; buckets)
            b = Bucket(uniform(0, 100), new Mutex);
    }

    public TBucketValue opIndex(in size_t index) const pure nothrow {
        return buckets[index];
    }

    public void transfer(in size_t from, in size_t to,
                         in TBucketValue amount) {
        immutable low  = min(from, to);
        immutable high = max(from, to);
        buckets[low].mtx.lock();
        buckets[high].mtx.lock();

        scope(exit) {
            buckets[low].mtx.unlock();
            buckets[high].mtx.unlock();
        }

        immutable realAmount = min(buckets[from].value, amount);
        buckets[from] -= realAmount;
        buckets[to  ] += realAmount;
        transfersCount++;
    }

    @property size_t length() const pure nothrow {
        return this.buckets.length;
    }

    void toString(in void delegate(const(char)[]) sink) {
        TBucketValue total = 0;
        foreach (ref b; buckets) {
            b.mtx.lock();
            total += b;
        }

        scope(exit)
            foreach (ref b; buckets)
                b.mtx.unlock();

        sink(text(buckets));
        sink(" ");
        sink(text(total));
    }
}

void randomize(size_t N)(Buckets!N data) {
    auto rng = Xorshift(1);

    while (data.running) {
        immutable i = uniform(0, data.length, rng);
        immutable j = uniform(0, data.length, rng);
        immutable amount = uniform!"[]"(0, 20, rng);
        data.transfer(i, j, amount);
    }
}

void equalize(size_t N)(Buckets!N data) {
    auto rng = Xorshift(1);

    while (data.running) {
        immutable i = uniform(0, data.length, rng);
        immutable j = uniform(0, data.length, rng);
        immutable a = data[i];
        immutable b = data[j];
        if (a > b)
            data.transfer(i, j, (a - b) / 2);
        else
            data.transfer(j, i, (b - a) / 2);
    }
}

void display(size_t N)(Buckets!N data) {
    foreach (immutable _; 0 .. 10) {
        writeln(transfersCount, " ", data);
        transfersCount = 0;
        Thread.sleep(1.seconds);
    }
    data.running = false;
}

void main() {
    writeln("N. transfers, buckets, buckets sum:");
    auto data = new Buckets!20();
    task!randomize(data).executeInNewThread();
    task!equalize(data).executeInNewThread();
    task!display(data).executeInNewThread();
}
```

{{out}}

```txt
N. transfers, buckets, buckets sum:
445977 [0, 175, 33, 18, 26, 61, 34, 13, 181, 8, 28, 12, 28, 47, 4, 12, 3, 76, 46, 59] 864
4863591 [32, 18, 45, 12, 69, 29, 98, 64, 108, 28, 54, 16, 15, 93, 56, 0, 4, 16, 48, 59] 864
4872790 [46, 162, 6, 2, 42, 70, 77, 34, 78, 99, 19, 0, 10, 59, 61, 13, 0, 27, 0, 59] 864
5102493 [1, 10, 120, 159, 108, 0, 51, 0, 35, 74, 0, 7, 14, 5, 6, 23, 53, 99, 40, 59] 864
5139426 [42, 43, 42, 42, 42, 42, 43, 42, 42, 42, 42, 43, 43, 42, 42, 43, 43, 43, 42, 59] 864
4853088 [12, 108, 18, 53, 25, 62, 37, 86, 141, 0, 45, 18, 0, 30, 0, 129, 11, 0, 30, 59] 864
4739723 [84, 12, 105, 80, 140, 0, 6, 53, 17, 86, 55, 0, 0, 41, 14, 51, 25, 11, 25, 59] 864
5295588 [43, 43, 42, 42, 57, 53, 43, 34, 42, 66, 61, 49, 10, 39, 29, 24, 48, 50, 30, 59] 864
5137883 [42, 43, 42, 42, 43, 43, 43, 42, 42, 42, 43, 42, 42, 43, 42, 43, 49, 42, 35, 59] 864
5143735 [42, 42, 43, 43, 43, 42, 43, 42, 42, 43, 42, 43, 42, 42, 42, 42, 42, 43, 42, 59] 864
```



## E


In E, any computation occurs in a particular ''vat''. Over its lifetime, a vat executes many individual computations, ''turns'', which are taken from a queue of pending events. The ''eventual send'' operator <code>&lt;-</code> puts message-sends on the queue.

Since a vat executes only one turn at a time, each turn is atomic; since the below implementation of the transfer operation does not invoke any other code, the transfer operation is itself automatically atomic and will always preserve the total value provided that it does not have any bugs.

In this example, the tasks are in the same vat as the buckets, but it would be straightforward to write them to live in separate vats.

{{works with|E-on-Java}}

This example uses a Java AWT window to display the current state of the buckets.


```e
#!/usr/bin/env rune
pragma.syntax("0.9")

def pi := (-1.0).acos()
def makeEPainter := <unsafe:com.zooko.tray.makeEPainter>
def colors := <awt:makeColor>

# --------------------------------------------------------------
# --- Definitions

/** Execute 'task' repeatedly as long 'indicator' is unresolved. */
def doWhileUnresolved(indicator, task) {
  def loop() {
    if (!Ref.isResolved(indicator)) {
      task()
      loop <- ()
    }
  }
  loop <- ()
}

/** The data structure specified for the task. */
def makeBuckets(size) {
    def values := ([100] * size).diverge() # storage
    def buckets {
        to size() :int { return size }
        /** get current quantity in bucket 'i' */
        to get(i :int) { return values[i] }
        /** transfer 'amount' units, as much as possible, from bucket 'i' to bucket 'j'
            or vice versa if 'amount' is negative */
        to transfer(i :int, j :int, amount :int) {
            def amountLim := amount.min(values[i]).max(-(values[j]))
            values[i] -= amountLim
            values[j] += amountLim
        }
    }
    return buckets
}

/** A view of the current state of the buckets. */
def makeDisplayComponent(buckets) {
  def c := makeEPainter(def paintCallback {
    to paintComponent(g) {
      def pixelsW := c.getWidth()
      def pixelsH := c.getHeight()
      def bucketsW := buckets.size()

      g.setColor(colors.getWhite())
      g.fillRect(0, 0, pixelsW, pixelsH)

      g.setColor(colors.getDarkGray())
      var sum := 0
      for i in 0..!bucketsW {
        sum += def value := buckets[i]
        def x0 := (i       * pixelsW / bucketsW).floor()
        def x1 := ((i + 1) * pixelsW / bucketsW).floor()
        g.fillRect(x0 + 1, pixelsH - value,
                   x1 - x0 - 1, value)
      }

      g.setColor(colors.getBlack())
      g."drawString(String, int, int)"(`Total: $sum`, 2, 20)
    }
  })
  c.setPreferredSize(<awt:makeDimension>(500, 300))
  return c
}

# --------------------------------------------------------------
# --- Application setup

def buckets := makeBuckets(100)
def done # Promise indicating when the window is closed

# Create the window
def frame := <unsafe:javax.swing.makeJFrame>("Atomic transfers")
frame.setContentPane(def display := makeDisplayComponent(buckets))
frame.addWindowListener(def mainWindowListener {
  to windowClosing(event) :void {
    bind done := null
  }
  match _ {}
})
frame.setLocation(50, 50)
frame.pack()

# --------------------------------------------------------------
# --- Tasks

# Neatens up buckets
var ni := 0
doWhileUnresolved(done, fn {
  def i := ni
  def j := (ni + 1) %% buckets.size()
  buckets.transfer(i, j, (buckets[i] - buckets[j]) // 4)
  ni := j
})

# Messes up buckets
var mi := 0
doWhileUnresolved(done, fn {
    def i := (mi + entropy.nextInt(3)) %% buckets.size()
    def j := (i + entropy.nextInt(3)) %% buckets.size() #entropy.nextInt(buckets.size())
    buckets.transfer(i, j, (buckets[i] / pi).floor())
    mi := j
})

# Updates display at fixed 10 Hz
# (Note: tries to catch up; on slow systems slow this down or it will starve the other tasks)
def clock := timer.every(100, def _(_) {
  if (Ref.isResolved(done)) {
    clock.stop()
  } else {
    display.repaint()
  }
})
clock.start()

# --------------------------------------------------------------
# --- All ready, go visible and wait

frame.show()
interp.waitAtTop(done)
```



## Erlang

Erlang has a built in database (Mnesia) with atomic operations. This is another way.
Instead of deleting the Buckets process manually I use spawn_link(). That way Buckets goes away with the user. Output is:

```txt

[1,2,3,4,5,6,7,8,9,10] = 55
[1,3,4,7,2,5,13,8,4,8] = 55
[6,13,6,0,8,3,1,0,8,10] = 55
[8,0,9,0,5,9,8,8,8,0] = 55
[8,11,9,3,1,12,8,0,0,3] = 55
[13,4,3,8,1,5,10,4,5,2] = 55
[6,6,9,5,6,5,6,1,5,6] = 55
[20,7,5,0,5,0,0,10,8,0] = 55
[2,10,0,10,0,4,8,3,15,3] = 55
[0,11,7,0,4,16,7,0,10,0] = 55

```


```Erlang

-module( atomic_updates ).
-export( [buckets/1, buckets_get/2, buckets_get_all/1, buckets_move_contents/4, task/0] ).

buckets( N ) ->
	Buckets = erlang:list_to_tuple( lists:seq(1, N) ),
	erlang:spawn_link( fun() -> buckets_loop(Buckets) end ).

buckets_get( N, Buckets_pid ) ->
	{is_buckets_alive, true} = {is_buckets_alive, erlang:is_process_alive( Buckets_pid )},
	Buckets_pid ! {get, N, erlang:self()},
	receive
	{value, Buckets_pid, Value} -> Value
	end.

buckets_get_all( Buckets_pid ) ->
	{is_buckets_alive, true} = {is_buckets_alive, erlang:is_process_alive( Buckets_pid )},
	Buckets_pid ! {get_all, erlang:self()},
	receive
	{values, Buckets_pid, Values} -> Values
	end.

buckets_move_contents( Amount, From, To, Buckets_pid ) ->
	{is_buckets_alive, true} = {is_buckets_alive, erlang:is_process_alive( Buckets_pid )},
	Buckets_pid ! {move_contents, Amount, From, To, erlang:self()},
	receive
	{move_contents_done, Buckets_pid} -> ok
	end.

task() ->
	erlang:spawn( fun() ->
		N = 10,
		Buckets = buckets( N ),
		erlang:spawn_link( fun() -> closer_loop(N, Buckets) end ),
		erlang:spawn_link( fun() -> redistribute_loop(N, Buckets) end ),
		display_loop( 0, N, Buckets ),
		erlang:exit( stop )
	end ).



closer_loop( N, Buckets ) ->
	One = random:uniform( N ),
	Two = random:uniform( N ),
	Difference = buckets_get( One, Buckets ) - buckets_get( Two, Buckets ),
	{Amount, From, To} = closer_loop_how_to_move( Difference, One, Two ),
	buckets_move_contents( Amount, From, To, Buckets ),
	closer_loop( N, Buckets ).

closer_loop_how_to_move( Difference, One, Two ) when Difference < 0 ->
	{-1* Difference div 2, Two, One};
closer_loop_how_to_move( Difference, One, Two ) ->
	{Difference div 2, One, Two}.

buckets_loop( Buckets ) ->
	receive
	{get, N, Pid} ->
		Pid ! {value, erlang:self(), erlang:element( N, Buckets )},
		buckets_loop( Buckets );
	{get_all, Pid} ->
		Pid ! {values, erlang:self(), erlang:tuple_to_list( Buckets )},
		buckets_loop( Buckets );
	{move_contents, Amount, From, To, Pid} ->
		Pid ! {move_contents_done, erlang:self()},
		buckets_loop( buckets_loop_move_contents(Amount, From, To, Buckets) )
	end.

buckets_loop_move_contents( _Amount, Same, Same, Buckets ) ->
	Buckets;
buckets_loop_move_contents( Amount, From, To, Buckets ) ->
	Amount_from = erlang:element( From, Buckets ),
	Clamped_amount = erlang:min( Amount, Amount_from ),
	Removed = erlang:setelement( From, Buckets, Amount_from - Clamped_amount ),
	Amount_to = erlang:element( To, Buckets ) + Clamped_amount,
	erlang:setelement( To, Removed, Amount_to ).

display_loop( N, N, _Buckets ) -> ok;
display_loop( Counter, N, Buckets ) ->
	Contents = buckets_get_all( Buckets ),
	io:fwrite( "~p = ~p~n", [Contents, lists:sum(Contents)] ),
	timer:sleep( 100 ),
	display_loop( Counter + 1, N, Buckets ).

redistribute_loop( N, Buckets ) ->
	Amount = random:uniform( N ),
	From = random:uniform( N ),
	To = random:uniform( N ),
	buckets_move_contents( Amount, From, To, Buckets ),
	redistribute_loop( N, Buckets ).

```



## Euphoria


```euphoria
function move(sequence s, integer amount, integer src, integer dest)
    if src < 1 or src > length(s) or dest < 1 or dest > length(s) or amount < 0 then
        return -1
    else
        if src != dest and amount then
            if amount > s[src] then
                amount = s[src]
            end if
            s[src] -= amount
            s[dest] += amount
        end if
        return s
    end if
end function

sequence buckets
buckets = repeat(100,10)

procedure equalize()
    integer i, j, diff
    while 1 do
        i = rand(length(buckets))
        j = rand(length(buckets))
        diff = buckets[i] - buckets[j]
        if  diff >= 2 then
            buckets = move(buckets, floor(diff / 2), i, j)
        elsif diff <= -2 then
            buckets = move(buckets, -floor(diff / 2), j, i)
        end if
        task_yield()
    end while
end procedure

procedure redistribute()
    integer i, j
    while 1 do
        i = rand(length(buckets))
        j = rand(length(buckets))
        if buckets[i] then
            buckets = move(buckets, rand(buckets[i]), i, j)
        end if
        task_yield()
    end while
end procedure

function sum(sequence s)
    integer sum
    sum = 0
    for i = 1 to length(s) do
        sum += s[i]
    end for
    return sum
end function

atom task

task = task_create(routine_id("equalize"), {})
task_schedule(task, 1)

task = task_create(routine_id("redistribute"), {})
task_schedule(task, 1)

task_schedule(0, {0.5, 0.5})

for i = 1 to 24 do
    print(1,buckets)
    printf(1," sum: %d\n", {sum(buckets)})
    task_yield()
end for
```


Output:

```txt
{100,100,100,100,100,100,100,100,100,100} sum: 1000
{150,77,68,150,113,126,14,192,68,42} sum: 1000
{46,64,58,117,139,59,143,114,130,130} sum: 1000
{82,99,13,99,58,117,10,191,194,137} sum: 1000
{72,65,68,193,67,65,112,106,128,124} sum: 1000
{43,43,42,31,234,104,105,234,30,134} sum: 1000
{83,106,31,82,174,62,254,71,106,31} sum: 1000
{145,102,247,86,159,30,87,35,102,7} sum: 1000
{93,102,114,40,126,48,243,101,10,123} sum: 1000
{160,38,9,89,182,240,116,15,61,90} sum: 1000
{31,45,123,31,308,189,71,0,79,123} sum: 1000
{9,86,198,87,72,194,168,148,38,0} sum: 1000
{122,99,42,99,140,128,106,68,155,41} sum: 1000
{223,45,0,220,220,50,153,6,82,1} sum: 1000
{171,68,192,100,78,31,100,0,31,229} sum: 1000
{47,70,108,253,66,113,70,92,157,24} sum: 1000
{113,85,147,84,97,21,93,180,99,81} sum: 1000
{82,35,8,75,166,342,48,79,99,66} sum: 1000
{65,53,71,36,72,108,127,146,116,206} sum: 1000
{154,15,107,47,50,204,82,177,107,57} sum: 1000
{63,127,62,126,261,57,127,95,70,12} sum: 1000
{25,50,0,39,55,105,586,54,47,39} sum: 1000
{31,86,137,66,117,116,157,121,110,59} sum: 1000
{129,65,27,38,135,54,175,129,135,113} sum: 1000

```


=={{header|F Sharp|F#}}==

The Buckets class is thread safe and its private higher-order Lock function ensures that locks are taken out in order (to avoid deadlocks):


```fsharp

open System.Threading

type Buckets(n) =
  let rand = System.Random()
  let mutex = Array.init n (fun _ -> new Mutex())
  let bucket = Array.init n (fun _ -> 100)

  member this.Count = n

  member this.Item n = bucket.[n]

  member private this.Lock is k =
    let is = Seq.sort is
    for i in is do
      mutex.[i].WaitOne() |> ignore
    try k() finally
    for i in is do
      mutex.[i].ReleaseMutex()

  member this.Transfer i j d =
    if i <> j && d <> 0 then
      let i, j, d = if d > 0 then i, j, d else j, i, -d
      this.Lock [i; j] (fun () ->
        let d = min d bucket.[i]
        bucket.[i] <- bucket.[i] - d
        bucket.[j] <- bucket.[j] + d)

  member this.Read =
    this.Lock [0..n-1] (fun () -> Array.copy bucket)

  member this.Print() =
    let xs = this.Read
    printf "%A = %d\n" xs (Seq.sum xs)

  interface System.IDisposable with
    member this.Dispose() =
      for m in mutex do
        (m :> System.IDisposable).Dispose()

let transfers = ref 0
let max_transfers = 1000000

let rand_pair (rand: System.Random) n =
  let i, j = rand.Next n, rand.Next(n-1)
  i, if j<i then j else j+1

let equalizer (bucket: Buckets) () =
  let rand = System.Random()
  while System.Threading.Interlocked.Increment transfers < max_transfers do
    let i, j = rand_pair rand bucket.Count
    let d = (bucket.[i] - bucket.[j]) / 2
    if d > 0 then
      bucket.Transfer i j d
    else
      bucket.Transfer j i -d

let randomizer (bucket: Buckets) () =
  let rand = System.Random()
  while System.Threading.Interlocked.Increment transfers < max_transfers do
    let i, j = rand_pair rand bucket.Count
    let d = 1 + rand.Next bucket.[i]
    bucket.Transfer i j d

do
  use bucket = new Buckets(10)
  let equalizer = Thread(equalizer bucket)
  let randomizer = Thread(randomizer bucket)
  bucket.Print()
  equalizer.Start()
  randomizer.Start()
  while !transfers < max_transfers do
    Thread.Sleep 100
    bucket.Print()

```


This program performs a million concurrent transfers. Typical output is:


```fsharp

[|100; 100; 100; 100; 100; 100; 100; 100; 100; 100|] = 1000
[|119; 61; 138; 115; 157; 54; 82; 58; 157; 59|] = 1000
[|109; 90; 78; 268; 55; 104; 91; 46; 105; 54|] = 1000
[|101; 75; 38; 114; 161; 160; 2; 234; 14; 101|] = 1000
[|104; 30; 114; 37; 32; 117; 50; 236; 127; 153|] = 1000
[|102; 32; 6; 55; 367; 69; 157; 80; 77; 55|] = 1000
[|211; 12; 319; 18; 11; 25; 73; 154; 154; 23|] = 1000
[|23; 373; 110; 108; 64; 33; 109; 8; 63; 109|] = 1000
[|72; 106; 174; 99; 115; 141; 98; 63; 123; 9|] = 1000
[|188; 67; 271; 30; 76; 134; 1; 74; 91; 68|] = 1000
[|2; 46; 240; 198; 63; 63; 113; 57; 136; 82|] = 1000
[|5; 151; 11; 191; 88; 236; 14; 0; 152; 152|] = 1000
[|162; 97; 102; 97; 122; 123; 0; 86; 84; 127|] = 1000
[|9; 11; 204; 50; 169; 206; 137; 26; 137; 51|] = 1000
[|175; 55; 157; 150; 116; 54; 10; 168; 114; 1|] = 1000
[|73; 85; 124; 3; 63; 62; 189; 115; 172; 114|] = 1000
[|112; 102; 253; 124; 39; 67; 197; 77; 20; 9|] = 1000
[|139; 172; 102; 1; 101; 64; 127; 55; 92; 147|] = 1000
[|54; 72; 130; 31; 99; 99; 130; 38; 186; 161|] = 1000
[|90; 0; 43; 46; 84; 335; 77; 79; 90; 156|] = 1000
[|20; 7; 128; 115; 24; 26; 128; 105; 240; 207|] = 1000
[|42; 79; 45; 60; 312; 37; 26; 61; 47; 291|] = 1000
[|176; 25; 10; 44; 126; 268; 78; 94; 46; 133|] = 1000
[|117; 153; 74; 63; 214; 44; 43; 93; 96; 103|] = 1000
[|56; 11; 106; 54; 1; 135; 174; 140; 174; 149|] = 1000
[|84; 153; 108; 77; 118; 140; 96; 102; 103; 19|] = 1000
[|59; 64; 85; 118; 215; 127; 42; 42; 120; 128|] = 1000
[|147; 95; 175; 116; 117; 0; 74; 116; 117; 43|] = 1000
[|131; 24; 128; 140; 45; 139; 155; 23; 68; 147|] = 1000
[|63; 184; 70; 24; 64; 84; 254; 14; 184; 59|] = 1000
[|119; 0; 234; 0; 98; 130; 94; 53; 99; 173|] = 1000
[|101; 0; 114; 129; 162; 176; 86; 84; 64; 84|] = 1000
[|95; 49; 57; 38; 73; 153; 276; 10; 147; 102|] = 1000
[|109; 182; 3; 147; 81; 107; 2; 142; 147; 80|] = 1000
[|45; 2; 103; 43; 103; 79; 65; 314; 57; 189|] = 1000
[|86; 86; 202; 47; 69; 11; 31; 246; 157; 65|] = 1000
[|82; 27; 107; 86; 106; 182; 64; 120; 82; 144|] = 1000
[|32; 158; 248; 50; 83; 109; 85; 16; 134; 85|] = 1000
[|49; 15; 246; 68; 69; 13; 219; 123; 130; 68|] = 1000
[|125; 133; 70; 23; 266; 30; 30; 44; 44; 235|] = 1000
[|18; 40; 174; 145; 146; 131; 62; 46; 138; 100|] = 1000
[|24; 128; 64; 104; 65; 109; 231; 101; 87; 87|] = 1000
[|107; 82; 40; 8; 133; 110; 180; 82; 102; 156|] = 1000
[|129; 122; 122; 52; 22; 143; 45; 49; 217; 99|] = 1000
[|15; 13; 71; 55; 55; 120; 115; 192; 192; 172|] = 1000
[|3; 95; 136; 76; 74; 37; 309; 44; 137; 89|] = 1000
[|14; 185; 47; 47; 97; 164; 180; 74; 98; 94|] = 1000
[|152; 145; 148; 83; 27; 35; 35; 77; 289; 9|] = 1000
[|78; 133; 147; 148; 83; 84; 142; 21; 141; 23|] = 1000
[|101; 63; 94; 168; 63; 90; 55; 94; 209; 63|] = 1000
[|73; 131; 182; 172; 130; 43; 102; 102; 5; 60|] = 1000
[|84; 61; 102; 9; 164; 175; 56; 4; 266; 79|] = 1000
[|89; 95; 29; 78; 200; 82; 152; 87; 101; 87|] = 1000
[|32; 33; 100; 7; 132; 75; 134; 234; 85; 168|] = 1000
[|197; 53; 81; 27; 1; 264; 100; 130; 34; 113|] = 1000
[|120; 198; 102; 51; 102; 64; 178; 45; 64; 76|] = 1000
[|208; 147; 18; 25; 178; 159; 23; 170; 36; 36|] = 1000
Press any key to continue . . .

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "sync"
    "time"
)

const nBuckets = 10

type bucketList struct {
    b [nBuckets]int // bucket data specified by task

    // transfer counts for each updater, not strictly required by task but
    // useful to show that the two updaters get fair chances to run.
    tc [2]int

    sync.Mutex // synchronization
}

// Updater ids, to track number of transfers by updater.
// these can index bucketlist.tc for example.
const (
    idOrder = iota
    idChaos
)

const initialSum = 1000 // sum of all bucket values

// Constructor.
func newBucketList() *bucketList {
    var bl bucketList
    // Distribute initialSum across buckets.
    for i, dist := nBuckets, initialSum; i > 0; {
        v := dist / i
        i--
        bl.b[i] = v
        dist -= v
    }
    return &bl
}

// method 1 required by task, get current value of a bucket
func (bl *bucketList) bucketValue(b int) int {
    bl.Lock() // lock before accessing data
    r := bl.b[b]
    bl.Unlock()
    return r
}

// method 2 required by task
func (bl *bucketList) transfer(b1, b2, a int, ux int) {
    // Get access.
    bl.Lock()
    // Clamping maintains invariant that bucket values remain nonnegative.
    if a > bl.b[b1] {
        a = bl.b[b1]
    }
    // Transfer.
    bl.b[b1] -= a
    bl.b[b2] += a
    bl.tc[ux]++ // increment transfer count
    bl.Unlock()
}

// additional useful method
func (bl *bucketList) snapshot(s *[nBuckets]int, tc *[2]int) {
    bl.Lock()
    *s = bl.b
    *tc = bl.tc
    bl.tc = [2]int{} // clear transfer counts
    bl.Unlock()
}

var bl = newBucketList()

func main() {
    // Three concurrent tasks.
    go order() // make values closer to equal
    go chaos() // arbitrarily redistribute values
    buddha()   // display total value and individual values of each bucket
}

// The concurrent tasks exercise the data operations by calling bucketList
// methods.  The bucketList methods are "threadsafe", by which we really mean
// goroutine-safe.  The conconcurrent tasks then do no explicit synchronization
// and are not responsible for maintaining invariants.

// Exercise 1 required by task: make values more equal.
func order() {
    r := rand.New(rand.NewSource(time.Now().UnixNano()))
    for {
        b1 := r.Intn(nBuckets)
        b2 := r.Intn(nBuckets - 1)
        if b2 >= b1 {
            b2++
        }
        v1 := bl.bucketValue(b1)
        v2 := bl.bucketValue(b2)
        if v1 > v2 {
            bl.transfer(b1, b2, (v1-v2)/2, idOrder)
        } else {
            bl.transfer(b2, b1, (v2-v1)/2, idOrder)
        }
    }
}

// Exercise 2 required by task: redistribute values.
func chaos() {
    r := rand.New(rand.NewSource(time.Now().Unix()))
    for {
        b1 := r.Intn(nBuckets)
        b2 := r.Intn(nBuckets - 1)
        if b2 >= b1 {
            b2++
        }
        bl.transfer(b1, b2, r.Intn(bl.bucketValue(b1)+1), idChaos)
    }
}

// Exercise 3 requred by task: display total.
func buddha() {
    var s [nBuckets]int
    var tc [2]int
    var total, nTicks int

    fmt.Println("sum  ---updates---    mean  buckets")
    tr := time.Tick(time.Second / 10)
    for {
        <-tr
        bl.snapshot(&s, &tc)
        var sum int
        for _, l := range s {
            if l < 0 {
                panic("sob") // invariant not preserved
            }
            sum += l
        }
        // Output number of updates per tick and cummulative mean
        // updates per tick to demonstrate "as often as possible"
        // of task exercises 1 and 2.
        total += tc[0] + tc[1]
        nTicks++
        fmt.Printf("%d %6d %6d %7d  %3d\n", sum, tc[0], tc[1], total/nTicks, s)
        if sum != initialSum {
            panic("weep") // invariant not preserved
        }
    }
}
```

{{out}}

```txt

sum  ---updates---    mean  buckets
1000 317832 137235  455067  [100 100 100 100 100 100 100 100 100 100]
1000 391239 339389  592847  [ 85 266  81  85 131  37  62  80 111  62]
1000 509436 497362  730831  [ 70 194 194  62  16 193  10  16 126 119]
1000 512065 499038  800899  [100 100 100 100 100 100 100 100 100 100]
1000 250590 121947  715226  [ 47 271  78  61  34 199  73  58 100  79]
...

```



## Groovy

Solution:

```groovy
class Buckets {

    def cells = []
    final n

    Buckets(n, limit=1000, random=new Random()) {
        this.n = n
        (0..<n).each {
            cells << random.nextInt(limit)
        }
    }

    synchronized getAt(i) {
        cells[i]
    }

    synchronized transfer(from, to, amount) {
        assert from in (0..<n) && to in (0..<n)
        def cappedAmt = [cells[from], amount].min()
        cells[from] -= cappedAmt
        cells[to] += cappedAmt
    }

    synchronized String toString() { cells.toString() }
}

def random = new Random()

def buckets = new Buckets(5)

def makeCloser = { i, j ->
    synchronized(buckets) {
        def targetDiff = (buckets[i]-buckets[j]).intdiv(2)
        if (targetDiff < 0) {
            buckets.transfer(j, i, -targetDiff)
        } else {
            buckets.transfer(i, j, targetDiff)
        }
    }
}

def randomize = { i, j ->
    synchronized(buckets) {
        def targetLimit = buckets[i] + buckets[j]
        def targetI = random.nextInt(targetLimit + 1)
        if (targetI < buckets[i]) {
            buckets.transfer(i, j, buckets[i] - targetI)
        } else {
            buckets.transfer(j, i, targetI - buckets[i])
        }
    }
}

Thread.start {
    def start = System.currentTimeMillis()
    while (start + 10000 > System.currentTimeMillis()) {
        def i = random.nextInt(buckets.n)
        def j = random.nextInt(buckets.n)
        makeCloser(i, j)
    }
}

Thread.start {
    def start = System.currentTimeMillis()
    while (start + 10000 > System.currentTimeMillis()) {
        def i = random.nextInt(buckets.n)
        def j = random.nextInt(buckets.n)
        randomize(i, j)
    }
}

def start = System.currentTimeMillis()
while (start + 10000 > System.currentTimeMillis()) {
    synchronized(buckets) {
        def sum = buckets.cells.sum()
        println "${new Date()}: checksum: ${sum} buckets: ${buckets}"
    }
    Thread.sleep(500)
}
```


Output:

```txt
Sat Jan 07 02:24:45 CST 2012: checksum: 2161 buckets: [227, 700, 635, 299, 300]
Sat Jan 07 02:24:46 CST 2012: checksum: 2161 buckets: [477, 365, 364, 478, 477]
Sat Jan 07 02:24:46 CST 2012: checksum: 2161 buckets: [432, 434, 429, 434, 432]
Sat Jan 07 02:24:47 CST 2012: checksum: 2161 buckets: [432, 428, 434, 432, 435]
Sat Jan 07 02:24:48 CST 2012: checksum: 2161 buckets: [432, 433, 432, 432, 432]
Sat Jan 07 02:24:48 CST 2012: checksum: 2161 buckets: [433, 432, 432, 432, 432]
Sat Jan 07 02:24:49 CST 2012: checksum: 2161 buckets: [359, 425, 254, 868, 255]
Sat Jan 07 02:24:49 CST 2012: checksum: 2161 buckets: [433, 432, 432, 432, 432]
Sat Jan 07 02:24:50 CST 2012: checksum: 2161 buckets: [432, 431, 430, 430, 438]
Sat Jan 07 02:24:50 CST 2012: checksum: 2161 buckets: [466, 404, 388, 466, 437]
Sat Jan 07 02:24:51 CST 2012: checksum: 2161 buckets: [476, 569, 365, 386, 365]
Sat Jan 07 02:24:51 CST 2012: checksum: 2161 buckets: [35, 111, 1038, 387, 590]
Sat Jan 07 02:24:52 CST 2012: checksum: 2161 buckets: [423, 341, 341, 423, 633]
Sat Jan 07 02:24:52 CST 2012: checksum: 2161 buckets: [141, 1295, 102, 370, 253]
Sat Jan 07 02:24:53 CST 2012: checksum: 2161 buckets: [683, 188, 345, 638, 307]
Sat Jan 07 02:24:53 CST 2012: checksum: 2161 buckets: [379, 275, 354, 240, 913]
Sat Jan 07 02:24:54 CST 2012: checksum: 2161 buckets: [894, 515, 455, 234, 63]
Sat Jan 07 02:24:54 CST 2012: checksum: 2161 buckets: [306, 507, 793, 507, 48]
Sat Jan 07 02:24:55 CST 2012: checksum: 2161 buckets: [463, 462, 240, 632, 364]
Sat Jan 07 02:24:55 CST 2012: checksum: 2161 buckets: [204, 162, 223, 996, 576]
```



## Haskell


{{works with|GHC}}

This uses MVar as its concurrency protection. An MVar is a container that may have a value or not; trying to ''take'' the value when it is absent blocks until a value is provided, at which point it is atomically taken again. modifyMVar_ is a shortcut to take the value, then put a modified value; readMVar takes the value and puts back the same value while returning it.

So, at any given time, the current value map is either in the MVar or being examined or replaced by one thread, but not both. The IntMap held by the MVar is a pure immutable data structure (<code>adjust</code> returns a modified version), so there is no problem from that the ''display'' task puts the value back before it is done printing.


```haskell
module AtomicUpdates (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forever, forM_)
import Data.IntMap (IntMap, (!), toAscList, fromList, adjust)
import System.Random (randomRIO)
import Text.Printf (printf)

-------------------------------------------------------------------------------

type Index = Int
type Value = Integer
data Buckets = Buckets Index (MVar (IntMap Value))

makeBuckets   :: Int -> IO Buckets
size          :: Buckets -> Index
currentValue  :: Buckets -> Index -> IO Value
currentValues :: Buckets          -> IO (IntMap Value)
transfer      :: Buckets -> Index -> Index -> Value -> IO ()

-------------------------------------------------------------------------------

makeBuckets n = do v <- newMVar (fromList [(i, 100) | i <- [1..n]])
                   return (Buckets n v)

size (Buckets n _) = n

currentValue (Buckets _ v) i = fmap (! i) (readMVar v)
currentValues (Buckets _ v) = readMVar v

transfer b@(Buckets n v) i j amt | amt < 0        = transfer b j i (-amt)
                                 | otherwise      = do
  modifyMVar_ v $ \map -> let amt' = min amt (map ! i)
                            in return $ adjust (subtract amt') i
                                      $ adjust (+        amt') j
                                      $ map

-------------------------------------------------------------------------------

roughen, smooth, display :: Buckets -> IO ()

pick buckets = randomRIO (1, size buckets)

roughen buckets = forever loop where
  loop = do i <- pick buckets
            j <- pick buckets
            iv <- currentValue buckets i
            transfer buckets i j (iv `div` 3)

smooth buckets = forever loop where
  loop = do i <- pick buckets
            j <- pick buckets
            iv <- currentValue buckets i
            jv <- currentValue buckets j
            transfer buckets i j ((iv - jv) `div` 4)

display buckets = forever loop where
  loop = do threadDelay 1000000
            bmap <- currentValues buckets
            putStrLn (report $ map snd $ toAscList bmap)
  report list = "\nTotal: " ++ show (sum list) ++ "\n" ++ bars
    where bars = concatMap row $ map (*40) $ reverse [1..5]
          row lim = printf "%3d " lim ++ [if x >= lim then '*' else ' ' | x <- list] ++ "\n"

main = do buckets <- makeBuckets 100
          forkIO (roughen buckets)
          forkIO (smooth buckets)
          display buckets
```


Sample output:

 Total: 10000
 200       *           *                                   *
 160       *           *           *            *          *                *  *   *          *        *
 120 **   ** *  ***   ****   **    *   *    *   ** *    * **              * *  *   * *        *   *    **
  80 ***  ** ** ***** **** ******  ****** ***   ** **  ***** * * *****    * * **   * ***     **   *    **
  40 ********** ******************************* ***** ****** *******************  ******* ***************

 Total: 10000
 200                                   *
 160                *        *         *                         *     *                      *         *
 120     *  **  *** *  *     **    *  **    *    ** * *    *  ** *   * *  * *    *   * **     *      * **
  80  ***** **  ********     ***   * *** ** **  *** * * ***** ****   ***  *** * ** *** ***  * *** *  * **
  40  ******** ******************  ************************************************************** *******

==Icon and {{header|Unicon}}==

The following only works in Unicon:


```unicon
global mtx

procedure main(A)
    nBuckets := integer(A[1]) | 10
    nShows := integer(A[2]) | 4
    showBuckets := A[3]
    mtx := mutex()
    every !(buckets := list(nBuckets)) := ?100

    thread repeat {
        every (b1|b2) := ?nBuckets   # OK if same!
        critical mtx: xfer((buckets[b1] - buckets[b2])/2, b1, b2)
        }
    thread repeat {
        every (b1|b2) := ?nBuckets   # OK if same!
        critical mtx: xfer(integer(?buckets[b1]), b1, b2)
        }
    wait(thread repeat {
        delay(500)
        critical mtx: {
            every (sum := 0) +:= !buckets
            writes("Sum: ",sum)
            if \showBuckets then every writes(" -> "|right(!buckets, 4))
            }
        write()
        if (nShows -:= 1) <= 0 then break
        })
end

procedure xfer(x,b1,b2)
    buckets[b1] -:= x
    buckets[b2] +:= x
end
```


Sample run:


```txt

->au 20 10 yes
Sum: 973 ->   48  49  48  49  49  49  48  48  49  49  49  49  48  49  48  49  48  49  49  49
Sum: 973 ->   49  49  48  49  49  48  49  49  49  48  48  49  49  48  49  49  48  48  49  49
Sum: 973 ->   49  49  49  48  49  48  48  49  49  49  49  49  49  48  49  48  48  48  49  49
Sum: 973 ->   49  49  49  48  48  48  48  48  49  49  49  49  49  49  49  49  49  48  49  48
Sum: 973 ->   48  49  48  49  49  49  49  48  49  49  48  48  48  49  48  49  49  49  49  49
Sum: 973 ->   70  51  49  31  87  51  53  51  48  50  88  12  43  39  50  46  50   0  53  51
Sum: 973 ->   11  15  83  95   3  53 145   0   8 120   9   9  10   5  45 122  38  70   2 130
Sum: 973 ->   12 260  17   3  45  13   9   4  46  71  18  41  15  68 104  53  18 104  44  28
Sum: 973 ->   49  48  49  49  49  48  49  48  49  49  49  49  49  48  49  48  49  48  48  49
Sum: 973 ->  140  47  32  47  32  60 227   0  48  32  78  15  36 135   8  16   0   8  11   1
->

```



## Java

{{works with|Java|8+}}

```java
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

public class AtomicUpdates {

    private static final int NUM_BUCKETS = 10;

    public static class Buckets {
        private final int[] data;

        public Buckets(int[] data) {
            this.data = data.clone();
        }

        public int getBucket(int index) {
            synchronized (data) {
                return data[index];
            }
        }

        public int transfer(int srcIndex, int dstIndex, int amount) {
            if (amount < 0)
                throw new IllegalArgumentException("negative amount: " + amount);
            if (amount == 0)
                return 0;

            synchronized (data) {
                if (data[srcIndex] - amount < 0)
                    amount = data[srcIndex];
                if (data[dstIndex] + amount < 0)
                    amount = Integer.MAX_VALUE - data[dstIndex];
                if (amount < 0)
                    throw new IllegalStateException();
                data[srcIndex] -= amount;
                data[dstIndex] += amount;
                return amount;
            }
        }

        public int[] getBuckets() {
            synchronized (data) {
                return data.clone();
            }
        }
    }

    private static long getTotal(int[] values) {
        long total = 0;
        for (int value : values) {
            total += value;
        }
        return total;
    }

    public static void main(String[] args) {
        ThreadLocalRandom rnd = ThreadLocalRandom.current();

        int[] values = new int[NUM_BUCKETS];
        for (int i = 0; i < values.length; i++)
            values[i] = rnd.nextInt() & Integer.MAX_VALUE;
        System.out.println("Initial Array: " + getTotal(values) + " " + Arrays.toString(values));

        Buckets buckets = new Buckets(values);
        new Thread(() -> equalize(buckets), "equalizer").start();
        new Thread(() -> transferRandomAmount(buckets), "transferrer").start();
        new Thread(() -> print(buckets), "printer").start();
    }

    private static void transferRandomAmount(Buckets buckets) {
        ThreadLocalRandom rnd = ThreadLocalRandom.current();
        while (true) {
            int srcIndex = rnd.nextInt(NUM_BUCKETS);
            int dstIndex = rnd.nextInt(NUM_BUCKETS);
            int amount = rnd.nextInt() & Integer.MAX_VALUE;
            buckets.transfer(srcIndex, dstIndex, amount);
        }
    }

    private static void equalize(Buckets buckets) {
        ThreadLocalRandom rnd = ThreadLocalRandom.current();
        while (true) {
            int srcIndex = rnd.nextInt(NUM_BUCKETS);
            int dstIndex = rnd.nextInt(NUM_BUCKETS);
            int amount = (buckets.getBucket(srcIndex) - buckets.getBucket(dstIndex)) / 2;
            if (amount >= 0)
                buckets.transfer(srcIndex, dstIndex, amount);
        }
    }

    private static void print(Buckets buckets) {
        while (true) {
            long nextPrintTime = System.currentTimeMillis() + 3000;
            long now;
            while ((now = System.currentTimeMillis()) < nextPrintTime) {
                try {
                    Thread.sleep(nextPrintTime - now);
                } catch (InterruptedException e) {
                    return;
                }
            }

            int[] bucketValues = buckets.getBuckets();
            System.out.println("Current values: " + getTotal(bucketValues) + " " + Arrays.toString(bucketValues));
        }
    }
}
```


{{out}}

```txt
Initial Array: 8345792262 [143255168, 196076270, 933397723, 1556699232, 1050802212, 538674858, 1196357020, 738586704, 726124301, 1265818774]
Current values: 8345792262 [0, 1874588555, 1422104978, 1646554792, 272895092, 0, 1100055274, 562892928, 0, 1466700643]
Current values: 8345792262 [0, 938536756, 1022153269, 802097042, 834165196, 893056852, 1022153268, 985683168, 985683168, 862263543]
Current values: 8345792262 [828663081, 828663080, 800738961, 653833491, 926105549, 856587200, 1235929058, 653833491, 780719176, 780719175]
Current values: 8345792262 [834986940, 835010170, 833752099, 835010170, 834668841, 834620567, 833370581, 835083486, 834620567, 834668841]
Current values: 8345792262 [0, 249877205, 1201027166, 2147483647, 0, 966988101, 725353114, 107211829, 2147483647, 800367553]
Current values: 8345792262 [789241957, 389741912, 898370461, 1824723292, 389741912, 898370462, 1824723293, 434230374, 896648599, 0]
Current values: 8345792262 [290197046, 76068608, 2147483647, 185783029, 646610948, 187523099, 1387188383, 0, 2147483647, 1277453855]
Current values: 8345792262 [0, 0, 1594297983, 1972188797, 0, 2147483647, 0, 2147483647, 92403769, 391934419]
Current values: 8345792262 [330331828, 330331828, 2147483647, 515452290, 2010486407, 0, 515452290, 0, 348770325, 2147483647]
...
```


Commenting out either of the threads mutating the buckets shows that they work properly.


## Julia


```julia
using StatsBase

function runall()
    nbuckets = 16
    unfinish = true
    spinner = ReentrantLock()
    buckets = rand(1:99, nbuckets)
    totaltrans = 0

    bucketsum() = sum(buckets)
    smallpause() = sleep(rand() / 2000)
    picktwo() = (samplepair(nbuckets)...)
    function equalizer()
        while unfinish
            smallpause()
            if trylock(spinner)
                i, j = picktwo()
                sm = buckets[i] + buckets[j]
                m = fld(sm + 1, 2)
                buckets[i], buckets[j] = m, sm - m
                totaltrans += 1
                unlock(spinner)
            end
        end
    end
    function redistributor()
        while unfinish
            smallpause()
            if trylock(spinner)
                i, j = picktwo()
                sm = buckets[i] + buckets[j]
                buckets[i] = rand(0:sm)
                buckets[j] = sm - buckets[i]
                totaltrans += 1
                unlock(spinner)
            end
        end
    end
    function accountant()
        count = 0
        while count < 16
            smallpause()
            if trylock(spinner)
                println("Current state of buckets: $buckets. Total in buckets: $(bucketsum())")
                unlock(spinner)
                count += 1
                sleep(1)
            end
        end
        unfinish = false
    end
    t = time()
    @async equalizer()
    @async redistributor()
    @async accountant()
    while unfinish sleep(0.25) end
    println("Total transactions: $totaltrans ($(round(Int, totaltrans / (time() - t))) unlocks per second).")
end

runall()
```


{{out}}

```txt
Current state of buckets: [56, 26, 34, 57, 26, 25, 39, 91, 53, 46, 96, 67, 86, 49, 2, 85]. Total in buckets: 838
Current state of buckets: [62, 32, 90, 50, 9, 43, 16, 71, 67, 99, 22, 44, 63, 85, 78, 7]. Total in buckets: 838
Current state of buckets: [58, 25, 41, 30, 9, 79, 42, 43, 32, 66, 110, 123, 90, 35, 13, 42]. Total in buckets: 838
Current state of buckets: [86, 63, 70, 21, 41, 69, 30, 29, 38, 40, 12, 28, 85, 13, 127, 86]. Total in buckets: 838
Current state of buckets: [45, 32, 26, 30, 45, 9, 86, 200, 31, 45, 9, 23, 60, 64, 79, 54]. Total in buckets: 838
Current state of buckets: [68, 16, 89, 104, 15, 35, 15, 23, 91, 92, 29, 27, 33, 21, 136, 44]. Total in buckets: 838
Current state of buckets: [13, 72, 8, 25, 27, 62, 134, 33, 78, 79, 7, 22, 132, 73, 12, 61]. Total in buckets: 838
Current state of buckets: [97, 78, 16, 90, 90, 69, 0, 22, 26, 84, 23, 22, 78, 69, 32, 42]. Total in buckets: 838
Current state of buckets: [3, 105, 99, 69, 70, 8, 50, 32, 17, 69, 53, 1, 68, 66, 64, 64]. Total in buckets: 838
Current state of buckets: [27, 181, 9, 5, 66, 16, 60, 56, 66, 140, 43, 29, 51, 59, 1, 29]. Total in buckets: 838
Current state of buckets: [45, 108, 45, 28, 58, 108, 86, 41, 45, 29, 57, 11, 54, 23, 52, 48]. Total in buckets: 838
Current state of buckets: [76, 45, 47, 75, 62, 34, 73, 27, 102, 64, 32, 51, 55, 32, 43, 20]. Total in buckets: 838
Current state of buckets: [35, 69, 41, 34, 29, 79, 82, 72, 71, 65, 34, 67, 68, 14, 33, 45]. Total in buckets: 838
Current state of buckets: [85, 53, 53, 26, 45, 53, 84, 99, 48, 50, 27, 52, 60, 79, 13, 11]. Total in buckets: 838
Current state of buckets: [49, 63, 24, 38, 64, 79, 75, 70, 69, 68, 50, 74, 12, 60, 6, 37]. Total in buckets: 838
Current state of buckets: [32, 20, 82, 70, 54, 41, 87, 15, 15, 44, 82, 55, 17, 33, 87, 104]. Total in buckets: 838
Total transactions: 26751 (1639 unlocks per second).
```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

import java.util.concurrent.ThreadLocalRandom
import kotlin.concurrent.thread

const val NUM_BUCKETS = 10

class Buckets(data: IntArray) {
    private val data = data.copyOf()

    operator fun get(index: Int) = synchronized(data) { data[index] }

    fun transfer(srcIndex: Int, dstIndex: Int, amount: Int): Int {
        if (amount < 0) {
            throw IllegalArgumentException("Negative amount: $amount")
        }
        if (amount == 0) return 0
        synchronized(data) {
            var a = amount
            if (data[srcIndex] - a < 0) a = data[srcIndex]
            if (data[dstIndex] + a < 0) a = Int.MAX_VALUE - data[dstIndex]
            if (a < 0) throw IllegalStateException()
            data[srcIndex] -= a
            data[dstIndex] += a
            return a
        }
    }

    val buckets get() = synchronized(data) { data.copyOf() }

    fun transferRandomAmount() {
        val rnd = ThreadLocalRandom.current()
        while (true) {
            val srcIndex = rnd.nextInt(NUM_BUCKETS)
            val dstIndex = rnd.nextInt(NUM_BUCKETS)
            val amount = rnd.nextInt() and Int.MAX_VALUE
            transfer(srcIndex, dstIndex, amount)
        }
    }

    fun equalize() {
        val rnd = ThreadLocalRandom.current()
        while (true) {
            val srcIndex = rnd.nextInt(NUM_BUCKETS)
            val dstIndex = rnd.nextInt(NUM_BUCKETS)
            val amount = (this[srcIndex] - this[dstIndex]) / 2
            if (amount >= 0) transfer(srcIndex, dstIndex, amount)
        }
    }

    fun print() {
        while (true) {
            val nextPrintTime = System.currentTimeMillis() + 3000
            while (true) {
                val now = System.currentTimeMillis()
                if (now >= nextPrintTime) break
                try {
                    Thread.sleep(nextPrintTime - now)
                }
                catch (e: InterruptedException) {
                    return
                }
            }
            val bucketValues = buckets
            println("Current values: ${bucketValues.total} ${bucketValues.asList()}")
        }
    }
}

val IntArray.total: Long get() {
    var sum = 0L
    for (d in this) sum += d
    return sum
}

fun main(args: Array<String>) {
    val rnd = ThreadLocalRandom.current()
    val values = IntArray(NUM_BUCKETS) { rnd.nextInt() and Int.MAX_VALUE }
    println("Initial array:  ${values.total} ${values.asList()}")
    val buckets = Buckets(values)
    thread(name = "equalizer")   { buckets.equalize() }
    thread(name = "transferrer") { buckets.transferRandomAmount() }
    thread(name = "printer")     { buckets.print() }
}
```


Sample output:

```txt

Initial array:  9412276676 [1252597313, 1908616225, 824662669, 972947315, 2126883821, 405179067, 693458796, 481375538, 396750085, 349805847]
Current values: 9412276676 [2147483647, 844064584, 983174119, 580879514, 1073741823, 666808378, 2147483647, 0, 484320482, 484320482]
Current values: 9412276676 [941221423, 941207347, 941304553, 941221422, 941235585, 941235585, 941225242, 941242321, 941157955, 941225243]
Current values: 9412276676 [941656114, 941197476, 941190372, 941203044, 941187119, 941177701, 941208610, 941038975, 941226893, 941190372]
Current values: 9412276676 [0, 202110459, 2147483647, 1901203310, 2147483647, 1489083519, 0, 234363721, 1290548373, 0]
Current values: 9412276676 [695300460, 2147483647, 1452183187, 2147483647, 0, 0, 0, 570277505, 252064583, 2147483647]
Current values: 9412276676 [941219147, 941226725, 941226725, 941238796, 941219147, 941247715, 941238795, 941234946, 941189734, 941234946]
Current values: 9412276676 [941306524, 941145153, 941241743, 940668167, 942314400, 941491117, 940668168, 941145153, 941306524, 940989727]
Current values: 9412276676 [945548859, 939149475, 935477311, 941294057, 944294715, 940031668, 940860151, 940662863, 940662862, 944294715]
Current values: 9412276676 [941254898, 941342907, 941188859, 941250824, 941250825, 940973864, 941078878, 941373381, 941373381, 941188859]
Current values: 9412276676 [941147294, 941232689, 941132597, 941330728, 941281708, 941236213, 941147294, 941265970, 941236214, 941265969]
......

```



## Lasso

Lasso thread objects are thread-safe by design.

```lasso>define atomic =
 thread {
    data
        private buckets = staticarray_join(10, void),
        private lock = 0

    public onCreate => {
        loop(.buckets->size) => {
            .`buckets`->get(loop_count) = math_random(0, 1000)
        }
    }

    public buckets => .`buckets`

    public bucket(index::integer) => .`buckets`->get(#index)

    public transfer(source::integer, dest::integer, amount::integer) => {
        #source == #dest
            ? return

        #amount = math_min(#amount, .`buckets`->get(#source))
        .`buckets`->get(#source) -= #amount
        .`buckets`->get(#dest)   += #amount
    }

    public numBuckets => .`buckets`->size

    public lock => {
        .`lock` == 1
            ? return false

        .`lock` = 1
        return true
    }
    public unlock => {
        .`lock` = 0
    }
}

local(initial_total) = (with b in atomic->buckets sum #b)
local(total) = #initial_total

// Make 2 buckets close to equal
local(_) = split_thread => {
    local(bucket1) = math_random(1, atomic->numBuckets)
    local(bucket2) = math_random(1, atomic->numBuckets)
    local(value1)  = atomic->bucket(#bucket1)
    local(value2)  = atomic->bucket(#bucket2)

    if(#value1 >= #value2) => {
        atomic->transfer(#bucket1, #bucket2, (#value1 - #value2) / 2)
    else
        atomic->transfer(#bucket2, #bucket1, (#value2 - #value1) / 2)
    }

    currentCapture->restart
}

// Randomly distribute 2 buckets
local(_) = split_thread => {
    local(bucket1) = math_random(1, atomic->numBuckets)
    local(bucket2) = math_random(1, atomic->numBuckets)
    local(value1)  = atomic->bucket(#bucket1)

    atomic->transfer(#bucket1, #bucket2, math_random(1, #value1))

    currentCapture->restart
}

local(buckets)
while(#initial_total == #total) => {
    sleep(2000)
    #buckets = atomic->buckets
    #total   = with b in #buckets sum #b
    stdoutnl(#buckets->asString + " -- total: " + #total)
}
stdoutnl(`ERROR: totals no longer match: ` + #initial_total + ', ' + #total)
```


{{out}}

```txt
staticarray(130, 399, 339, 0, 444, 444, 618, 872, 390, 23) -- total: 3659
staticarray(233, 538, 461, 117, 389, 110, 232, 517, 633, 429) -- total: 3659
staticarray(129, 648, 494, 809, 823, 132, 425, 131, 58, 10) -- total: 3659
staticarray(255, 484, 53, 261, 484, 264, 336, 521, 211, 790) -- total: 3659
staticarray(464, 16, 463, 1043, 470, 177, 369, 486, 41, 130) -- total: 3659
staticarray(281, 717, 341, 716, 50, 17, 129, 247, 964, 197) -- total: 3659
staticarray(423, 509, 51, 458, 265, 423, 292, 458, 661, 119) -- total: 3659
```



## Logtalk

The following example can be found in the Logtalk distribution and is used here with permission. Works when using SWI-Prolog, XSB, or YAP as the backend compiler.

```logtalk

:- object(buckets).

    :- threaded.

    :- public([start/0, start/4]).

    % bucket representation
    :- private(bucket_/2).
    :- dynamic(bucket_/2).

    % use the same mutex for all the predicates that access the buckets
    :- private([bucket/2, buckets/1, transfer/3]).
    :- synchronized([bucket/2, buckets/1, transfer/3]).

    start :-
        % by default, create ten buckets with initial random integer values
        % in the interval [0, 10[ and print their contents ten times
        start(10, 0, 10, 10).

    start(N, Min, Max, Samples) :-
        % create the buckets with random values in the
        % interval [Min, Max[ and return their sum
        create_buckets(N, Min, Max, Sum),
        write('Sum of all bucket values: '), write(Sum), nl, nl,
        % use competitive or-parallelism for the three loops such that
        % the computations terminate when the display loop terminates
        threaded((
                display_loop(Samples)
            ;   match_loop(N)
            ;   redistribute_loop(N)
        )).

    create_buckets(N, Min, Max, Sum) :-
        % remove all exisiting buckets
        retractall(bucket_(_,_)),
        % create the new buckets
        create_buckets(N, Min, Max, 0, Sum).

    create_buckets(0, _, _, Sum, Sum) :-
        !.
    create_buckets(N, Min, Max, Sum0, Sum) :-
        random::random(Min, Max, Value),
        asserta(bucket_(N,Value)),
        M is N - 1,
        Sum1 is Sum0 + Value,
        create_buckets(M, Min, Max, Sum1, Sum).

    bucket(Bucket, Value) :-
        bucket_(Bucket, Value).

    buckets(Values) :-
        findall(Value, bucket_(_, Value), Values).

    transfer(Origin, _, Origin) :-
        !.
    transfer(Origin, Delta, Destin) :-
        retract(bucket_(Origin, OriginValue)),
        retract(bucket_(Destin, DestinValue)),
        % the buckets may have changed between the access to its
        % values and the calling of this transfer predicate; thus,
        % we must ensure that we're transfering a legal amount
        Amount is min(Delta, OriginValue),
        NewOriginValue is OriginValue - Amount,
        NewDestinValue is DestinValue + Amount,
        assertz(bucket_(Origin, NewOriginValue)),
        assertz(bucket_(Destin, NewDestinValue)).

    match_loop(N) :-
        % randomly select two buckets
        M is N + 1,
        random::random(1, M, Bucket1),
        random::random(1, M, Bucket2),
        % access their contents
        bucket(Bucket1, Value1),
        bucket(Bucket2, Value2),
        % make their new values approximately equal
        Delta is truncate(abs(Value1 - Value2)/2),
        (   Value1 > Value2 ->
            transfer(Bucket1, Delta, Bucket2)
        ;   Value1 < Value2 ->
            transfer(Bucket2, Delta, Bucket1)
        ;   true
        ),
        match_loop(N).

    redistribute_loop(N) :-
        % randomly select two buckets
        M is N + 1,
        random::random(1, M, FromBucket),
        random::random(1, M, ToBucket),
        % access bucket from where we transfer
        bucket(FromBucket, Current),
        Limit is Current + 1,
        random::random(0, Limit, Delta),
        transfer(FromBucket, Delta, ToBucket),
        redistribute_loop(N).

    display_loop(0) :-
        !.
    display_loop(N) :-
        buckets(Values),
        write(Values), nl,
        thread_sleep(2),
        M is N - 1,
        display_loop(M).

:- end_object.

```


Sample output:


```logtalk

?- buckets::start.
Sum of all bucket values: 52

[4,6,9,5,3,5,9,7,4,0]
[5,3,6,3,9,5,5,6,2,8]
[2,2,3,13,5,5,2,8,6,6]
[7,4,7,1,1,1,5,11,8,7]
[8,5,8,4,4,3,4,1,3,12]
[2,4,8,6,11,6,6,7,1,1]
[2,12,3,2,6,5,0,9,7,6]
[2,6,3,3,16,3,2,3,7,7]
[6,0,4,0,23,1,1,4,2,11]
[11,6,10,4,0,4,5,5,4,3]
true.

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
transfer[bucks_, src_, dest_, n_] :=
  ReplacePart[
   bucks, {src -> Max[bucks[[src]] - n, 0],
    dest -> bucks[[dest]] + Min[bucks[[src]], n]}];
DistributeDefinitions[transfer];
SetSharedVariable[bucks, comp];
bucks = RandomInteger[10, 20];
comp = True;
Print["Original sum: " <> IntegerString[Plus @@ bucks]];
Print[Dynamic["Current sum: " <> IntegerString[Plus @@ bucks]]];
WaitAll[{ParallelSubmit[
    While[True, While[! comp, Null]; comp = False;
     Module[{a = RandomInteger[{1, 20}], b = RandomInteger[{1, 20}]},
      bucks = transfer[bucks, Max[a, b], Min[a, b],
        Floor[Abs[bucks[[a]] - bucks[[b]]]/2]]]; comp = True]],
   ParallelSubmit[
    While[True, While[! comp, Null]; comp = False;
     Module[{src = RandomInteger[{1, 20}],
       dest = RandomInteger[{1, 20}]},
      bucks = transfer[bucks, src, dest,
        RandomInteger[{1, bucks[[src]]}]]]; comp = True]]}];
```

{{out}}

```txt
Original sum: &lt;number&gt;
Current sum: &lt;same number, stays fixed&gt;
```

This simply uses a variable named <tt>comp</tt> to determine whether or not it is currently computing something.


## Oz

Uses a lock for every bucket. Enforces a locking order to avoid deadlocks.


```oz
declare
  %%
  %% INIT
  %%
  NBuckets = 100
  StartVal = 50
  ExpectedSum = NBuckets * StartVal

  %% Makes a tuple and calls Fun for every field
  fun {Make Label N Fun}
     R = {Tuple.make Label N}
  in
     for I in 1..N do R.I = {Fun} end
     R
  end

  Buckets = {Make buckets NBuckets fun {$} {Cell.new StartVal} end}
  Locks = {Make locks NBuckets Lock.new}
  LockList = {Record.toList Locks}

  %%
  %% DISPLAY
  %%
  proc {Display}
     Snapshot = {WithLocks LockList
                 fun {$}
                    {Record.map Buckets Cell.access}
                 end
                }
     Sum = {Record.foldL Snapshot Number.'+' 0}
  in
     {Print Snapshot}
     {System.showInfo "  sum: "#Sum}
     Sum = ExpectedSum %% assert
  end

  %% Calls Fun with multiple locks locked and returns the result of Fun.
  fun {WithLocks Ls Fun}
     case Ls of L|Lr then
        lock L then
           {WithLocks Lr Fun}
        end
     [] nil then {Fun}
     end
  end

  %%
  %% MANIPULATE
  %%
  proc {Smooth I J}
     Diff = @(Buckets.I) - @(Buckets.J) %% reading without lock: by design
     Amount = Diff div 4
  in
     {Transfer I J Amount}
  end

  proc {Roughen I J}
     Amount = @(Buckets.I) div 3 %% reading without lock: by design
  in
     {Transfer I J Amount}
  end

  %% Atomically transfer an amount from From to To.
  %% Negative amounts are allowed;
  %% will never make a bucket negative.
  proc {Transfer From To Amount}
     if From \= To then
        %% lock in order (to avoid deadlocks)
        Smaller = {Min From To}
        Bigger = {Max From To}
     in
        lock Locks.Smaller then
           lock Locks.Bigger then
              FromBucket = Buckets.From
              ToBucket = Buckets.To
              NewFromValue = @FromBucket - Amount
              NewToValue = @ToBucket + Amount
           in
              if NewFromValue >= 0 andthen NewToValue >= 0 then
                 FromBucket := NewFromValue
                 ToBucket := NewToValue
              end
           end
        end
     end
   end

  %% Returns a random bucket index.
  fun {Pick}
     {OS.rand} mod NBuckets + 1
  end
in
  %%
  %% START
  %%
  thread for do {Smooth {Pick} {Pick}} end end
  thread for do {Roughen {Pick} {Pick}} end end
  for do {Display} {Time.delay 50} end
```


Sample output:

```oz
buckets(50 50 50 50 50 50 50 50 50 50 ,,,)  sum: 5000
buckets(24 68 58 43 78 85 43 66 14 48 ,,,)  sum: 5000
buckets(36 33 59 38 39 23 55 51 43 45 ,,,)  sum: 5000
buckets(64 32 62 26 50 82 38 70 16 43 ,,,)  sum: 5000
buckets(51 51 49 50 51 51 51 49 49 49 ,,,)  sum: 5000
buckets(43 28 27 60 77 41 36 48 72 70 ,,,)  sum: 5000
...
```



## PARI/GP

GP is not able to do atomic updates.  PARI does atomic updates just like [[#C|C]].


## Perl


```perl
use strict;
use 5.10.0;

use threads 'yield';
use threads::shared;

my @a :shared = (100) x 10;
my $stop :shared = 0;

sub pick2 {
	my $i = int(rand(10));
	my $j;
	$j = int(rand(10)) until $j != $i;
	($i, $j)
}

sub even {
	lock @a;
	my ($i, $j) = pick2;
	my $sum = $a[$i] + $a[$j];
	$a[$i] = int($sum / 2);
	$a[$j] = $sum - $a[$i];
}

sub rand_move {
	lock @a;
	my ($i, $j) = pick2;

	my $x = int(rand $a[$i]);
	$a[$i] -= $x;
	$a[$j] += $x;
}

sub show {
	lock @a;
	my $sum = 0;
	$sum += $_ for (@a);
	printf "%4d", $_ for @a;
	print " total $sum\n";
}

my $t1 = async { even		until $stop }
my $t2 = async { rand_move	until $stop }
my $t3 = async {
	for (1 .. 10) {
		show;
		sleep(1);
	}
	$stop = 1;
};

$t1->join; $t2->join; $t3->join;
```



## Perl 6


{{trans|Ruby}}
{{works with|Rakudo|2016.07}}

```perl6
#| A collection of non-negative integers, with atomic operations.
class BucketStore {

  has $.elems is required;
  has @!buckets = ^1024 .pick xx $!elems;
  has $lock     = Lock.new;

  #| Returns an array with the contents of all buckets.
  method buckets {
    $lock.protect: { [@!buckets] }
  }

  #| Transfers $amount from bucket at index $from, to bucket at index $to.
  method transfer ($amount, :$from!, :$to!) {
    return if $from == $to;

    $lock.protect: {
      my $clamped = $amount min @!buckets[$from];

      @!buckets[$from] -= $clamped;
      @!buckets[$to]   += $clamped;
    }
  }
}

# Create bucket store
my $bucket-store = BucketStore.new: elems => 8;
my $initial-sum = $bucket-store.buckets.sum;

# Start a thread to equalize buckets
Thread.start: {
  loop {
    my @buckets = $bucket-store.buckets;

    # Pick 2 buckets, so that $to has not more than $from
    my ($to, $from) = @buckets.keys.pick(2).sort({ @buckets[$_] });

    # Transfer half of the difference, rounded down
    $bucket-store.transfer: ([-] @buckets[$from, $to]) div 2, :$from, :$to;
  }
}

# Start a thread to distribute values among buckets
Thread.start: {
  loop {
    my @buckets = $bucket-store.buckets;

    # Pick 2 buckets
    my ($to, $from) = @buckets.keys.pick(2);

    # Transfer a random portion
    $bucket-store.transfer: ^@buckets[$from] .pick, :$from, :$to;
  }
}

# Loop to display buckets
loop {
  sleep 1;

  my @buckets = $bucket-store.buckets;
  my $sum = @buckets.sum;

  say "{@buckets.fmt: '%4d'}, total $sum";

  if $sum != $initial-sum {
    note "ERROR: Total changed from $initial-sum to $sum";
    exit 1;
  }
}
```


{{out}}

```txt

  23   52  831  195 1407  809  813   20, total 4150
1172   83  336  306  751  468  615  419, total 4150
 734  103 1086   88  313  136 1252  438, total 4150
 512  323  544  165  200    3 2155  248, total 4150
...

```



## Phix

Requires Phix 0.6.7 or later (due 1st Sept 15)

```Phix
constant nBuckets = 20
sequence buckets = tagset(nBuckets)     -- {1,2,3,..,20}
constant bucket_cs = init_cs()          -- critical section
atom equals = 0, rands = 0              -- operation counts
integer terminate = 0                   -- control flag

procedure mythreads(integer eq)
-- if eq then equalise else randomise
integer b1,b2,amt
    while not terminate do
        b1 = rand(nBuckets)
        b2 = rand(nBuckets)
        if b1!=b2 then                  -- (test not actually needed)
            enter_cs(bucket_cs)
            if eq then
                amt = floor((buckets[b1]-buckets[b2])/2)
                equals += 1
            else
                amt = rand(buckets[b1]+1)-1
                rands += 1
            end if
            buckets[b1] -= amt
            buckets[b2] += amt
            leave_cs(bucket_cs)
        end if
    end while
    exit_thread(0)
end procedure

procedure display()
    enter_cs(bucket_cs)
    ?{sum(buckets),equals,rands,buckets}
    leave_cs(bucket_cs)
end procedure

display()

constant threads = {create_thread(routine_id("mythreads"),{1}), -- equalise
                    create_thread(routine_id("mythreads"),{0})} -- randomise

constant ESC = #1B
while not find(get_key(),{ESC,'q','Q'}) do
    sleep(1)
    display()
end while
terminate = 1
wait_thread(threads)
delete_cs(bucket_cs)
```

{{out}}

```txt

{210,0,0,{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}}
{210,1326977,1619458,{14,17,10,7,9,6,8,5,9,7,6,10,14,12,12,14,13,15,12,10}}
{210,2637987,3137483,{10,7,4,31,1,11,5,6,16,11,9,15,9,13,15,5,5,10,6,21}}
{210,3973762,4619906,{22,38,9,17,9,10,12,0,3,0,13,11,2,39,4,11,9,0,0,1}}
{210,5327923,6082436,{1,0,0,9,23,1,33,7,1,43,8,17,1,6,30,0,24,2,3,1}}
{210,6671482,7561288,{12,11,2,9,11,4,11,9,13,9,9,20,19,10,10,8,11,11,12,9}}
{210,7950733,9131581,{7,9,10,8,11,8,13,12,11,5,6,8,11,16,15,14,15,11,11,9}}
{210,9272164,10625022,{4,8,28,2,13,13,6,32,12,5,10,4,28,1,12,9,4,9,4,6}}
{210,10615451,12117282,{10,17,18,2,7,13,10,2,12,4,19,10,18,12,9,5,12,11,8,11}}
{210,11912322,13610386,{10,7,15,11,12,8,12,10,15,14,10,7,9,10,8,11,8,10,13,10}}
{210,13243566,15099214,{8,12,11,7,12,13,13,8,9,9,16,10,10,8,10,10,8,10,13,13}}

```



## PicoLisp

We use database objects (persistent symbols) for the buckets, and
child processes to handle the tasks, as this is the standard way
for general PicoLisp applications.

```PicoLisp
(seed (in "/dev/urandom" (rd 8)))

(de *Buckets . 15)   # Number of buckets

# E/R model
(class +Bucket +Entity)
(rel key (+Key +Number))   # Key  1 .. *Buckets
(rel val (+Number))        # Value 1 .. 999

# Create new DB file
(pool (tmp "buckets.db"))

# Create *Buckets buckets with values between 1 and 999
(for K *Buckets
   (new T '(+Bucket)  'key K  'val (rand 1 999)) )
(commit)

# Pick a random bucket
(de pickBucket ()
   (db 'key '+Bucket (rand 1 *Buckets)) )

# First process
(unless (fork)
   (seed *Pid)                                  # Ensure local random sequence
   (loop
      (let (B1 (pickBucket)  B2 (pickBucket))   # Pick two buckets 'B1' and 'B2'
         (dbSync)                               # Atomic DB operation
         (let (V1 (; B1 val)  V2 (; B2 val))    # Get current values
            (cond
               ((> V1 V2)
                  (dec> B1 'val)                # Make them closer to equal
                  (inc> B2 'val) )
               ((> V2 V1)
                  (dec> B2 'val)
                  (inc> B1 'val) ) ) )
         (commit 'upd)                          # Close transaction
         (wait 1) ) ) )

# Second process
(unless (fork)
   (seed *Pid)                                  # Ensure local random sequence
   (loop
      (let (B1 (pickBucket)  B2 (pickBucket))   # Pick two buckets 'B1' and 'B2'
         (unless (== B1 B2)                     # Found two different ones?
            (dbSync)                            # Atomic DB operation
            (let (V1 (; B1 val)  V2 (; B2 val)) # Get current values
               (cond
                  ((> V1 V2 0)
                     (inc> B1 'val)             # Redistribute them
                     (dec> B2 'val) )
                  ((> V2 V1 0)
                     (inc> B2 'val)
                     (dec> B1 'val) ) ) )
            (commit 'upd)                       # Close transaction
            (wait 1) ) ) ) )

# Third process
(unless (fork)
   (loop
      (let Lst (collect 'key '+Bucket)          # Get all buckets
         (for This Lst                          # Print current values
            (printsp (: val)) )
         (prinl                                 # and total sum
            "-- Total: "
            (sum '((This) (: val)) Lst) ) )
      (wait 2000) ) )                           # Sleep two seconds

(wait)
```

Output:

```txt
70 236 582 30 395 215 525 653 502 825 129 769 722 440 708 -- Total: 6801
0 156 566 352 198 263 0 743 0 1316 58 1180 897 0 1072 -- Total: 6801
0 0 424 101 0 0 0 682 0 1809 0 1549 961 0 1275 -- Total: 6801
0 0 0 0 0 0 0 452 0 2226 0 1838 884 0 1401 -- Total: 6801
54 55 56 55 54 55 54 102 54 2363 54 1816 666 55 1308 -- Total: 6801
198 198 197 196 198 198 197 197 196 1903 197 1438 345 197 946 -- Total: 6801
342 344 343 344 344 342 344 343 343 1278 343 992 343 343 413 -- Total: 6801
^C
```



## PureBasic


```PureBasic
#Buckets=9
#TotalAmount=200
Global Dim Buckets(#Buckets)
Global BMutex=CreateMutex()
Global Quit=#False

Procedure max(x,y)
  If x>=y:  ProcedureReturn x
  Else:     ProcedureReturn y
  EndIf
EndProcedure

Procedure Move(WantedAmount, From, Dest)
  Protected RealAmount
  If from<>Dest
    LockMutex(BMutex)
    RealAmount=max(0, Buckets(from)-WantedAmount)
    Buckets(From)-RealAmount
    Buckets(Dest)+RealAmount
    UnlockMutex(BMutex)
  EndIf
  ProcedureReturn RealAmount
EndProcedure

Procedure Level(A,B)
  Protected i, j, t
  If A<>B
    LockMutex(BMutex)
    t=Buckets(A)+Buckets(B)
    i=t/2: j=t-i
    Buckets(A)=i
    Buckets(B)=j
    UnlockMutex(BMutex)
  EndIf
EndProcedure

Procedure DoInvent(Array A(1))
  Protected i, sum
  LockMutex(BMutex)
  For i=0 To ArraySize(Buckets())
    A(i)=Buckets(i)
    sum+A(i)
  Next i
  UnlockMutex(BMutex)
  ProcedureReturn sum
EndProcedure

Procedure MixingThread(arg)
  Repeat
    Move(Random(#TotalAmount),Random(#Buckets),Random(#Buckets))
  Until Quit
EndProcedure

Procedure LevelingThread(arg)
  Repeat
    Level(Random(#Buckets),Random(#Buckets))
  Until Quit
EndProcedure

If OpenWindow(0,0,0,100,150,"Atomic updates",#PB_Window_SystemMenu)
  Define Thread1=CreateThread(@MixingThread(),0)
  Define Thread2=CreateThread(@MixingThread(),0)
  Define i, Event
  Dim Inventory(#Buckets)
  ; Set up a small GUI
  For i=0 To 9
    TextGadget(i, 0,i*15,50, 15,"Bucket #"+Str(i))
  Next i
  TextGadget(10,55,135,40,15,"=")
  AddWindowTimer(0,0,500)
  Buckets(0)=#TotalAmount
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Timer
      i=DoInvent(Inventory())
      SetGadgetText(10,"="+Str(i))
      For i=0 To #Buckets
        SetGadgetText(i, Str(Inventory(i)))
      Next i
    EndIf
  Until Event=#PB_Event_CloseWindow
  Quit=#True  ; Tell threads to shut down
  WaitThread(Thread1): WaitThread(Thread2)
EndIf
```



## Python

{{works with|Python|2.5 and above}}

This code uses a ''threading.Lock'' to serialize access to the bucket set.


```python
from __future__ import with_statement # required for Python 2.5
import threading
import random
import time

terminate = threading.Event()

class Buckets:
    def __init__(self, nbuckets):
        self.nbuckets = nbuckets
        self.values = [random.randrange(10) for i in range(nbuckets)]
        self.lock = threading.Lock()

    def __getitem__(self, i):
        return self.values[i]

    def transfer(self, src, dst, amount):
        with self.lock:
            amount = min(amount, self.values[src])
            self.values[src] -= amount
            self.values[dst] += amount

    def snapshot(self):
        # copy of the current state (synchronized)
        with self.lock:
            return self.values[:]

def randomize(buckets):
    nbuckets = buckets.nbuckets
    while not terminate.isSet():
        src = random.randrange(nbuckets)
        dst = random.randrange(nbuckets)
        if dst!=src:
            amount = random.randrange(20)
            buckets.transfer(src, dst, amount)

def equalize(buckets):
    nbuckets = buckets.nbuckets
    while not terminate.isSet():
        src = random.randrange(nbuckets)
        dst = random.randrange(nbuckets)
        if dst!=src:
            amount = (buckets[src] - buckets[dst]) // 2
            if amount>=0: buckets.transfer(src, dst, amount)
            else: buckets.transfer(dst, src, -amount)

def print_state(buckets):
    snapshot = buckets.snapshot()
    for value in snapshot:
        print '%2d' % value,
    print '=', sum(snapshot)

# create 15 buckets
buckets = Buckets(15)

# the randomize thread
t1 = threading.Thread(target=randomize, args=[buckets])
t1.start()

# the equalize thread
t2 = threading.Thread(target=equalize, args=[buckets])
t2.start()

# main thread, display
try:
    while True:
        print_state(buckets)
        time.sleep(1)
except KeyboardInterrupt: # ^C to finish
    terminate.set()

# wait until all worker threads finish
t1.join()
t2.join()
```


Sample Output:


```txt

 5  5 11  5  5  5  5  5  5  0  6  5  5  6  5 = 78
 9  0  0  0 20  5  0 21 10  0  0  8  5  0  0 = 78
 4  0  4 12  4  4  9  2 14  0 11  2  0 12  0 = 78
 5  5  6  5  5  5  6  5  6  5  5  5  5  5  5 = 78
 2  0  3  0  0  0  0  4 13  4  9  0  1  9 33 = 78
 0  0  0 22 11  0 13 12  0  0  0 20  0  0  0 = 78

```



## Racket


```racket
#lang racket

(struct bucket (value [lock #:auto])
  #:auto-value #f
  #:mutable
  #:transparent)

(define *buckets* (build-vector 10 (λ (i) (bucket 100))))

(define (show-buckets)
  (let* ([values (for/list ([b *buckets*]) (bucket-value b))]
         [total (apply + values)])
    (append values (list '- total))))

(define *equalizations* 0)
(define *randomizations* 0)
(define *blocks* 0)

(define (show-stats)
  (let ([n (length *log*)]
        [log (reverse *log*)])
    (printf "Equalizations ~a, Randomizations ~a, Transfers: ~a, Blocks ~a\n"
              *equalizations* *randomizations* n *blocks*)
    (for ([i (in-range 10)])
      (define j (min (floor (* i (/ n 9))) (sub1 n)))
      (printf "~a (~a). " (add1 i) (add1 j))
      (displayln (list-ref log j)))))

(define *log* (list (show-buckets)))

(define-syntax-rule (inc! x) (set! x (add1 x)))

(define (get-bucket i) (vector-ref *buckets* i))

(define (get-value i) (bucket-value (get-bucket i)))
(define (set-value! i v) (set-bucket-value! (get-bucket i) v))

(define (locked? i) (bucket-lock (vector-ref *buckets* i)))
(define (lock! i v) (set-bucket-lock! (get-bucket i) v))
(define (unlock! i) (lock! i #f))

(define *clamp-lock* #f)

(define (clamp i j)
  (cond [*clamp-lock* (inc! *blocks*)
                      #f]
        [else (set! *clamp-lock* #t)
              (let ([result #f]
                    [g (gensym)])
                (unless (locked? i)
                  (lock! i g)
                  (cond [(locked? j) (unlock! i)]
                        [else (lock! j g)
                              (set! result #t)]))
                (unless result (inc! *blocks*))
                (set! *clamp-lock* #f)
                result)]))

(define (unclamp i j)
  (unlock! i)
  (unlock! j))

(define (transfer i j amount)
  (let* ([lock1 (locked? i)]
         [lock2 (locked? j)]
         [a (get-value i)]
         [b (get-value j)]
         [c (- a amount)]
         [d (+ b amount)])
    (cond [(< c 0) (error 'transfer "Removing too much.")]
          [(< d 0) (error 'transfer "Stealing too much.")]
          [(and lock1 (equal? lock1 lock2)) (set-value! i c)
                                            (set-value! j d)
                                            (set! *log*
                                                  (cons (show-buckets) *log*))]
          [else (error 'transfer "Lock problem")])))

(define (equalize i j)
  (when (clamp i j)
    (let ([a (get-value i)]
          [b (get-value j)])
      (unless (= a b)
        (transfer i j (if (> a b)
                          (floor (/ (- a b) 2))
                          (- (floor (/ (- b a) 2)))))
        (inc! *equalizations*)))
        (unclamp i j)))

(define (randomize i j)
  (when (clamp i j)
    (let* ([a (get-value i)]
           [b (get-value j)]
           [t (+ a b)]
           [r (if (= t 0) 0 (random t))])
      (unless (= r 0)
        (transfer i j (- a r))
        (inc! *randomizations*)))
    (unclamp i j)))

(thread (λ () (for ([_ (in-range 500000)]) (equalize (random 10) (random 10)))))
(thread (λ () (for ([_ (in-range 500000)]) (randomize (random 10) (random 10)))))
```


Sample output:


```txt

> (show-stats)

Equalizations 33616, Randomizations 159240, Transfers: 192857, Blocks 579035
1 (1). (100 100 100 100 100 100 100 100 100 100 - 1000)
2 (21429). (100 238 23 36 153 111 86 100 38 115 - 1000)
3 (42858). (162 26 127 39 459 5 40 23 90 29 - 1000)
4 (64286). (16 80 41 307 117 38 251 36 29 85 - 1000)
5 (85715). (96 62 142 7 102 48 150 80 57 256 - 1000)
6 (107143). (69 70 69 69 69 69 298 69 69 149 - 1000)
7 (128572). (56 66 99 23 328 99 116 117 78 18 - 1000)
8 (150000). (23 128 108 110 56 232 69 25 33 216 - 1000)
9 (171429). (27 169 298 9 26 184 134 27 110 16 - 1000)
10 (192857). (54 80 38 52 29 14 42 173 246 272 - 1000)

```



## Ring


```ring

# Project : Atomic updates

bucket = list(10)
f2 = 0
for i = 1 to 10
     bucket[i] = floor(random(9)*10)
next

a = display("display:")
see nl
a = flatten(a)
see "" + a + nl
a = display("flatten:")
see nl
a = transfer(3,5)
see a + nl
see "19 from 3 to 5: "
a = display(a)
see nl

func display(a)
        display = 0
        see "" + a + " " + char(9)
        for i = 1 to 10
             display = display + bucket[i]
             see "" + bucket[i] + " "
        next
       see " total:" + display
       return display

func flatten(f)
        f1 = floor((f / 10) + 0.5)
        for i = 1 to 10
             bucket[i] = f1
             f2	 = f2 + f1
        next
        bucket[10] = bucket[10] + f - f2

func transfer(a1,a2)
        transfer = floor(random(9)/10 * bucket[a1])
        bucket[a1] = bucket[a1] - transfer
        bucket[a2] = bucket[a2] + transfer

```

Output:

```txt

display: 60 10 70 60 40 80 90 20 90 20  total:540
flatten: 54 54 54 54 54 54 54 54 54 54  total:540
19 from 3 to 5: 54 54 33 54 75 54 54 54 54 54  total:540

```



## Ruby


```Ruby
require 'thread'

# A collection of buckets, filled with random non-negative integers.
# There are atomic operations to look at the bucket contents, and
# to move amounts between buckets.
class BucketStore

  # Creates a BucketStore with +nbuckets+ buckets. Fills each bucket
  # with a random non-negative integer.
  def initialize nbuckets
    # Create an array for the buckets
    @buckets = (0...nbuckets).map { rand(1024) }

    # Mutex used to make operations atomic
    @mutex = Mutex.new
  end

  # Returns an array with the contents of all buckets.
  def buckets
    @mutex.synchronize { Array.new(@buckets) }
  end

  # Transfers _amount_ to bucket at array index _destination_,
  # from bucket at array index _source_.
  def transfer destination, source, amount
    # Do nothing if both buckets are same
    return nil if destination == source

    @mutex.synchronize do
      # Clamp amount to prevent negative value in bucket
      amount = [amount, @buckets[source]].min

      @buckets[source] -= amount
      @buckets[destination] += amount
    end
    nil
  end
end

# Create bucket store
bucket_store = BucketStore.new 8

# Get total amount in the store
TOTAL = bucket_store.buckets.inject { |a, b| a += b }

# Start a thread to equalize buckets
Thread.new do
  loop do
    # Pick 2 buckets
    buckets = bucket_store.buckets
    first = rand buckets.length
    second = rand buckets.length

    # Swap buckets so that _first_ has not more than _second_
    first, second = second, first if buckets[first] > buckets[second]

    # Transfer half of the difference, rounded down
    bucket_store.transfer first, second, (buckets[second] - buckets[first]) / 2
  end
end

# Start a thread to distribute values among buckets
Thread.new do
  loop do
    # Pick 2 buckets
    buckets = bucket_store.buckets
    first = rand buckets.length
    second = rand buckets.length

    # Transfer random amount to _first_ from _second_
    bucket_store.transfer first, second, rand(buckets[second])
  end
end

# Loop to display buckets
loop do
  sleep 1

  buckets = bucket_store.buckets

  # Compute the total value in all buckets.
  # We calculate this outside BucketStore so BucketStore can't cheat by
  # always reporting the same value.
  n = buckets.inject { |a, b| a += b }

  # Display buckets and total
  printf "%s, total %d\n", (buckets.map { |v| sprintf "%4d", v }.join " "), n

  if n != TOTAL
    # This should never happen
    $stderr.puts "ERROR: Total changed from #{TOTAL} to #{n}"
    exit 1
  end
end
```


Sample Output:

```txt
 221  521  331 1186  654  185  521   19, total 3638
 455  455  455  455  454  454  455  455, total 3638
 455  455  455  455  454  454  455  455, total 3638
 755    3  115   10  598 1326  515  316, total 3638
```


## Run BASIC


```runbasic
DIM bucket(10)
FOR i = 1 TO 10 : bucket(i) = int(RND(0)*100) : NEXT

a = display("       Display:")	' show original array
a = flatten(a)			' flatten the array
a = display("       Flatten:")	' show flattened array
a = transfer(3,5)		' transfer some amount from 3 to 5
a = display(a;" from 3 to 5:")	' Show transfer array
end

FUNCTION display(a$)
  print a$;" ";chr$(9);
    for i = 1 to 10
      display = display + bucket(i)
      print bucket(i);chr$(9);
    next i
  print " Total:";display
END FUNCTION

FUNCTION flatten(f)
   f1 = int((f / 10) + .5)
   for i = 1 to 10
    bucket(i)	= f1
    f2		= f2 + f1
   next i
   bucket(10)	= bucket(10) + f - f2
END FUNCTION


FUNCTION transfer(a1,a2)
transfer	= int(rnd(0) * bucket(a1))
bucket(a1)	= bucket(a1) - transfer
bucket(a2)	= bucket(a2) + transfer
END FUNCTION
```


```txt
       Display: 	24	50	50	85	63	49	50	91	10	2	 Total:474
       Flatten: 	47	47	47	47	47	47	47	47	47	51	 Total:474
19 from 3 to 5: 	47	47	28	47	66	47	47	47	47	51	 Total:474
```



## Rust

{{libheader|rand}}

```rust
extern crate rand;

use std::sync::{Arc, Mutex};
use std::thread;
use std::cmp;
use std::time::Duration;

use rand::Rng;
use rand::distributions::{IndependentSample, Range};

trait Buckets {
    fn equalize<R:Rng>(&mut self, rng: &mut R);
    fn randomize<R:Rng>(&mut self, rng: &mut R);
    fn print_state(&self);
}

impl Buckets for [i32] {
    fn equalize<R:Rng>(&mut self, rng: &mut R) {
        let range = Range::new(0,self.len()-1);
        let src = range.ind_sample(rng);
        let dst = range.ind_sample(rng);
        if dst != src {
            let amount = cmp::min(((dst + src) / 2) as i32, self[src]);
            let multiplier = if amount >= 0 { -1 } else { 1 };
            self[src] += amount * multiplier;
            self[dst] -= amount * multiplier;
        }
    }
    fn randomize<R:Rng>(&mut self, rng: &mut R) {
        let ind_range = Range::new(0,self.len()-1);
        let src = ind_range.ind_sample(rng);
        let dst = ind_range.ind_sample(rng);
        if dst != src {
            let amount = cmp::min(Range::new(0,20).ind_sample(rng), self[src]);
            self[src] -= amount;
            self[dst] += amount;

        }
    }
    fn print_state(&self) {
        println!("{:?} = {}", self, self.iter().sum::<i32>());
    }
}

fn main() {
    let e_buckets = Arc::new(Mutex::new([10; 10]));
    let r_buckets = e_buckets.clone();
    let p_buckets = e_buckets.clone();

    thread::spawn(move || {
        let mut rng = rand::thread_rng();
        loop {
            let mut buckets = e_buckets.lock().unwrap();
            buckets.equalize(&mut rng);
        }
    });
    thread::spawn(move || {
        let mut rng = rand::thread_rng();
        loop {
            let mut buckets = r_buckets.lock().unwrap();
            buckets.randomize(&mut rng);
        }
    });

    let sleep_time = Duration::new(1,0);
    loop {
        {
            let buckets = p_buckets.lock().unwrap();
            buckets.print_state();
        }
        thread::sleep(sleep_time);
    }
}
```



## Scala


```Scala

object AtomicUpdates {

  class Buckets(ns: Int*) {

    import scala.actors.Actor._

    val buckets = ns.toArray

    case class Get(index: Int)
    case class Transfer(fromIndex: Int, toIndex: Int, amount: Int)
    case object GetAll

    val handler = actor {
      loop {
        react {
          case Get(index) => reply(buckets(index))
          case Transfer(fromIndex, toIndex, amount) =>
            assert(amount >= 0)
            val actualAmount = Math.min(amount, buckets(fromIndex))
            buckets(fromIndex) -= actualAmount
            buckets(toIndex) += actualAmount
          case GetAll => reply(buckets.toList)
        }
      }
    }

    def get(index: Int): Int = (handler !? Get(index)).asInstanceOf[Int]
    def transfer(fromIndex: Int, toIndex: Int, amount: Int) = handler ! Transfer(fromIndex, toIndex, amount)
    def getAll: List[Int] = (handler !? GetAll).asInstanceOf[List[Int]]
  }

  def randomPair(n: Int): (Int, Int) = {
    import scala.util.Random._
    val pair = (nextInt(n), nextInt(n))
    if (pair._1 == pair._2) randomPair(n) else pair
  }

  def main(args: Array[String]) {
    import scala.actors.Scheduler._
    val buckets = new Buckets(List.range(1, 11): _*)
    val stop = new java.util.concurrent.atomic.AtomicBoolean(false)
    val latch = new java.util.concurrent.CountDownLatch(3)
    execute {
      while (!stop.get) {
        val (i1, i2) = randomPair(10)
        val (n1, n2) = (buckets.get(i1), buckets.get(i2))
        val m = (n1 + n2) / 2
        if (n1 < n2)
          buckets.transfer(i2, i1, n2 - m)
        else
          buckets.transfer(i1, i2, n1 - m)
      }
      latch.countDown
    }
    execute {
      while (!stop.get) {
        val (i1, i2) = randomPair(10)
        val n = buckets.get(i1)
        buckets.transfer(i1, i2, if (n == 0) 0 else scala.util.Random.nextInt(n))
      }
      latch.countDown
    }
    execute {
      for (i <- 1 to 20) {
        val all = buckets.getAll
        println(all.sum + ":" + all)
        Thread.sleep(500)
      }
      stop.set(true)
      latch.countDown
    }
    latch.await
    shutdown
  }
}

```



## Swift



```swift
import Foundation

final class AtomicBuckets: CustomStringConvertible {
  var count: Int {
    return buckets.count
  }

  var description: String {
    return withBucketsLocked { "\(buckets)" }
  }

  var total: Int {
    return withBucketsLocked { buckets.reduce(0, +) }
  }

  private let lock = DispatchSemaphore(value: 1)

  private var buckets: [Int]

  subscript(n: Int) -> Int {
    return withBucketsLocked { buckets[n] }
  }

  init(with buckets: [Int]) {
    self.buckets = buckets
  }

  func transfer(amount: Int, from: Int, to: Int) {
    withBucketsLocked {
      let transferAmount = buckets[from] >= amount ? amount : buckets[from]

      buckets[from] -= transferAmount
      buckets[to] += transferAmount
    }
  }

  private func withBucketsLocked<T>(do: () -> T) -> T {
    let ret: T

    lock.wait()
    ret = `do`()
    lock.signal()

    return ret
  }
}

let bucks = AtomicBuckets(with: [21, 39, 40, 20])
let order = DispatchSource.makeTimerSource()
let chaos = DispatchSource.makeTimerSource()
let printer = DispatchSource.makeTimerSource()

printer.setEventHandler {
  print("\(bucks) = \(bucks.total)")
}

printer.schedule(deadline: .now(), repeating: .seconds(1))
printer.activate()

order.setEventHandler {
  let (b1, b2) = (Int.random(in: 0..<bucks.count), Int.random(in: 0..<bucks.count))
  let (v1, v2) = (bucks[b1], bucks[b2])

  guard v1 != v2 else {
    return
  }

  if v1 > v2 {
    bucks.transfer(amount: (v1 - v2) / 2, from: b1, to: b2)
  } else {
    bucks.transfer(amount: (v2 - v1) / 2, from: b2, to: b1)
  }
}

order.schedule(deadline: .now(), repeating: .milliseconds(5))
order.activate()

chaos.setEventHandler {
  let (b1, b2) = (Int.random(in: 0..<bucks.count), Int.random(in: 0..<bucks.count))

  bucks.transfer(amount: Int.random(in: 0..<(bucks[b1] + 1)), from: b1, to: b2)
}

chaos.schedule(deadline: .now(), repeating: .milliseconds(5))
chaos.activate()

dispatchMain()
```


{{out}}


```txt
[21, 39, 40, 20] = 120
[14, 28, 46, 32] = 120
[25, 17, 38, 40] = 120
[5, 46, 69, 0] = 120
[22, 52, 24, 22] = 120
[11, 70, 20, 19] = 120
[18, 19, 46, 37] = 120
```



## Tcl

In Tcl, you need to explicitly hold a mutex if you want to reliably access multiple shared variables; single shared variable accesses use a built-in lock.


{{works with|Tcl|8.5}}

```tcl
package require Thread
package require Tk

# Make the shared state
canvas .c		;# So we can allocate the display lines in one loop
set m [thread::mutex create]
for {set i 0} {$i<100} {incr i} {
    set bucket b$i	;# A handle for every bucket...
    tsv::set buckets $bucket 50
    lappend buckets $bucket
    lappend lines [.c create line 0 0 0 0]
}
tsv::set still going 1

# Make the "make more equal" task
lappend tasks [thread::create {
    # Perform an atomic update of two cells
    proc transfer {b1 b2 val} {
	variable m
	thread::mutex lock $m
	set v [tsv::get buckets $b1]
	if {$val > $v} {
	    set val $v
	}
	tsv::incr buckets $b1 [expr {-$val}]
	tsv::incr buckets $b2 $val
	thread::mutex unlock $m
    }

    # The task itself; we loop this round frequently
    proc task {mutex buckets} {
	variable m $mutex b $buckets i 0
	while {[tsv::get still going]} {
	    set b1 [lindex $b $i]
	    if {[incr i] == [llength $b]} {set i 0}
	    set b2 [lindex $b $i]

	    if {[tsv::get buckets $b1] > [tsv::get buckets $b2]} {
		transfer $b1 $b2 1
	    } else {
		transfer $b1 $b2 -1
	    }
	}
    }
    thread::wait
}]

# Make the "mess things up" task
lappend tasks [thread::create {
    # Utility to pick a random item from a list
    proc pick list {
	lindex $list [expr {int(rand() * [llength $list])}]
    }
    proc transfer {b1 b2 val} {
	variable m
	thread::mutex lock $m
	set v [tsv::get buckets $b1]
	if {$val > $v} {
	    set val $v
	}
	tsv::incr buckets $b1 [expr {-$val}]
	tsv::incr buckets $b2 $val
	thread::mutex unlock $m
    }

    # The task to move a large amount between two random buckets
    proc task {mutex buckets} {
	variable m $mutex b $buckets
	while {[tsv::get still going]} {
	    set b1 [pick $b]
	    set b2 [pick $b]
	    transfer $b1 $b2 [expr {[tsv::get buckets $b1] / 3}]
	}
    }
    thread::wait
}]

# The "main" task; we keep GUI operations in the main thread
proc redisplay {} {
    global m buckets lines
    thread::mutex lock $m
    set i 1
    foreach b $buckets l $lines {
	.c coords $l $i 0 $i [tsv::get buckets $b]
	incr i 2
    }
    thread::mutex unlock $m
    after 100 redisplay
}

# Start tasks and display
.c configure -width 201 -height 120
pack .c
redisplay
foreach t $tasks {
    thread::send -async $t [list task $m $buckets]
}

# Wait for user to close window, then tidy up
tkwait window .
tsv::set still going 0
thread::broadcast thread::exit
```



## zkl

Threads and thread safe objects (locks, lists, ints, etc) are built in.

```zkl
class B{
   const N=10;
   var [const]
      buckets=(1).pump(N,List).copy(),  //(1,2,3...)
      lock=Atomic.Lock(), cnt=Atomic.Int();
   fcn init{ "Initial sum: ".println(values().sum()); }
   fcn transferArb{  // transfer arbitary amount from 1 bucket to another
      b1:=(0).random(N); b2:=(0).random(N);
      critical(lock){
	 t:=(0).random(buckets[b1]);
	 buckets[b1]=buckets[b1]-t; buckets[b2]=buckets[b2]+t;
      }
      cnt.inc();
   }
   fcn transferEq{  // try to make two buckets equal
      b1:=(0).random(N); b2:=(0).random(N);
      critical(lock){
         v1:=buckets[b1]; v2:=buckets[b2];
	 t:=(v1-v2).abs()/2;
	 if (v1<v2) t = -t;
	 buckets[b1]=v1-t; buckets[b2]=v2+t;
      }
      cnt.inc();
   }
   fcn values{ critical(lock){buckets.copy()} }
}

fcn threadA(b){ while(1) { b.transferArb(); } }
fcn threadE(b){ while(1) { b.transferEq();  } }
```


```zkl
b:=B();
do(10){ threadA.launch(b); } do(10){ threadE.launch(b); }

while(1){
   v:=b.values();
   v.println("-->",v.sum(),"  ", b.cnt.value," transfers ",
	     vm.numThreads," threads");
   Atomic.sleep(2.5);
}
```

{{out}}

```txt

Initial sum: 55
L(8,8,7,4,2,2,6,4,4,10)-->55  24 transfers 20 threads
L(1,3,5,6,8,8,1,8,10,5)-->55  33755 transfers 20 threads
L(6,5,4,2,7,6,11,1,7,6)-->55  67616 transfers 20 threads
L(5,8,5,5,9,4,4,4,5,6)-->55  101434 transfers 20 threads
L(4,1,6,9,10,4,5,5,4,7)-->55  135013 transfers 20 threads
L(7,6,5,4,5,4,4,7,7,6)-->55  168516 transfers 20 threads
L(2,4,5,3,4,14,1,5,11,6)-->55  202241 transfers 20 threads
L(7,5,2,3,14,8,6,6,1,3)-->55  235660 transfers 20 threads
L(8,7,9,8,7,6,1,1,6,2)-->55  269039 transfers 20 threads
L(7,4,8,17,3,2,1,5,5,3)-->55  302837 transfers 20 threads
L(4,5,4,5,10,5,5,5,3,9)-->55  336642 transfers 20 threads

```

Another solution, using a Pipe as a "holding tank". Pipes are thread safe queues. This code just moves the values to and from the pipe to synchronize changes. The use of this class is the same as above, just change b:=B() to b:=C();

```zkl
class C{
   const N=10;
   var [const]
      buckets=(1).pump(N,List).copy(),  //(1,2,3...)
      pipe = Thread.Pipe(), cnt=Atomic.Int();
   fcn init{
      pipe.write(buckets);
      "Initial sum: ".println(values().sum());
   }
   fcn transferArb{  // transfer arbitary amount from 1 bucket to another
      b1:=(0).random(N); b2:=(0).random(N);
      v:=pipe.read();
         t:=(0).random(v[b1]); v[b1]=v[b1]-t; v[b2]=v[b2]+t;
      pipe.write(v);
      cnt.inc();
   }
   fcn transferEq{  // try to make two buckets equal
      b1:=(0).random(N); b2:=(0).random(N);
      v:=pipe.read();
         v1:=v[b1]; v2:=v[b2]; t:=(v1-v2).abs()/2;
	 if (v1<v2) t = -t;
	 v[b1]=v1-t; v[b2]=v2+t;
      pipe.write(v);
      cnt.inc();
   }
   fcn values{
      v:=pipe.read(); v2:=v.copy(); pipe.write(v);
      v2;
   }
}
```



{{omit from|AWK}}
{{omit from|Befunge}}
{{omit from|Brainfuck}}
{{omit from|gnuplot}}
{{omit from|JavaScript}}
{{omit from|LaTeX}}
{{omit from|M4}}
{{omit from|ML/I}}
{{omit from|Make}}
{{omit from|PlainTeX}}
{{omit from|TI-89 BASIC}} <!-- Does not have concurrency or background processes. -->
{{omit from|XSLT}}
